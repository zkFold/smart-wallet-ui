module ZkFold.Cardano.SmartWallet.Api.Send (
  sendFundsWithCreation,
  sendFundsWithCreation',
  findMintedAuthToken,
  findMintedAuthTokens,
  sendFunds,
  sendFunds',
) where

import Data.Foldable (Foldable (foldl'), find)
import Data.Set qualified as Set
import GHC.IsList (toList)
import GeniusYield.Imports (mapMaybe, (&))
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Api.Create
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Cardano.UPLC.Wallet.Types

-- | See 'sendFundsWithCreation''.
sendFundsWithCreation :: (ZKWalletQueryMonad m, Foldable f) => ZKCreateWalletInfo -> f BuildOut -> m (GYTxSkeleton 'PlutusV3)
sendFundsWithCreation zkcwi@ZKCreateWalletInfo{..} outs = do
  (zkiws, walletAddress) <- addressFromEmail zkcwiEmail
  sendFundsWithCreation' zkiws walletAddress zkcwi outs

-- | Send funds from a zk-wallet along with registering the stake validator so that future spends can utilize stake validator instead of minting policy. Note that we cannot withdraw from a credential before it is registered.
sendFundsWithCreation' :: (GYTxQueryMonad m, Foldable f) => ZKInitializedWalletScripts -> GYAddress -> ZKCreateWalletInfo -> f BuildOut -> m (GYTxSkeleton 'PlutusV3)
sendFundsWithCreation' zkiws walletAddress zkcwi outs = do
  createWalletSkel <- createWallet' zkcwi zkiws walletAddress
  pure $
    createWalletSkel
      <> foldMap
        ( \BuildOut{..} ->
            mustHaveOutput $
              GYTxOut
                { gyTxOutValue = boValue
                , gyTxOutRefS = Nothing
                , gyTxOutDatum =
                    fmap
                      ( \(dat, toInline) ->
                          ( dat
                          , if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum
                          )
                      )
                      boDatum
                , gyTxOutAddress = boAddress
                }
        )
        outs

-- | Find a UTxO at wallet's address that has a proof validity token. Require that token to be in output.
findMintedAuthToken :: (GYTxQueryMonad m) => ZKInitializedWalletScripts -> GYAddress -> Email -> GYPaymentKeyHash -> m (GYUTxO, GYAssetClass)
findMintedAuthToken ZKInitializedWalletScripts{..} walletAddress email pkh = do
  walletOuts <- utxosAtAddress walletAddress Nothing
  let tn = tokenNameFromKeyHash pkh
      ac = GYToken (mintingPolicyId zkiwsWeb2Auth) tn
  case find (\out -> valueAssetPresent (utxoValue out) ac) (utxosToList walletOuts) of
    Nothing -> throwAppError (ZKWENoAuthToken email walletAddress tn)
    Just out -> pure (out, ac)

-- | Find all minted auth tokens at the given wallet address.
findMintedAuthTokens :: (GYTxQueryMonad m) => ZKInitializedWalletScripts -> GYAddress -> m (GYMintingPolicyId, [GYTokenName])
findMintedAuthTokens ZKInitializedWalletScripts{..} walletAddress = do
  walletOuts <- utxosAtAddress walletAddress Nothing
  let walletOutsL = utxosToList walletOuts
      web2AuthMP = mintingPolicyId zkiwsWeb2Auth
      web2AuthTNs =
        foldl'
          ( \acc out ->
              let assets = valueAssets (utxoValue out) & toList
                  mps =
                    assets
                      & mapMaybe
                        ( \case
                            GYLovelace -> Nothing
                            GYToken mp tn -> if mp == web2AuthMP then Just tn else Nothing
                        )
               in acc <> Set.fromList mps
          )
          mempty
          walletOutsL
  pure (web2AuthMP, Set.toList web2AuthTNs)

-- | Send funds from a zk-wallet.
sendFunds :: (ZKWalletQueryMonad m, Foldable f) => ZKSpendWalletInfo -> f BuildOut -> m (GYTxSkeleton 'PlutusV3)
sendFunds zkswi@ZKSpendWalletInfo{..} outs = do
  (zkiws, walletAddress) <- addressFromEmail zkswiEmail
  sendFunds' zkiws walletAddress zkswi outs

-- | Send funds from a zk-wallet.
sendFunds' ::
  (GYTxQueryMonad m, Foldable f) =>
  ZKInitializedWalletScripts ->
  -- | Address of the zk wallet.
  GYAddress ->
  ZKSpendWalletInfo ->
  f BuildOut ->
  m (GYTxSkeleton 'PlutusV3)
sendFunds' zkiws@ZKInitializedWalletScripts{..} walletAddress ZKSpendWalletInfo{..} outs = do
  (authOut, ac) <- findMintedAuthToken zkiws walletAddress zkswiEmail zkswiPaymentKeyHash
  nid <- networkId
  let
    stakeCred = GYCredentialByScript $ scriptHash zkiwsCheckSig
    stakeAddr = stakeAddressFromCredential nid stakeCred
  si <-
    stakeAddressInfo stakeAddr >>= \case
      Just si -> pure si
      Nothing -> throwAppError $ ZKWEStakeAddressInfoNotFound stakeAddr
  pure $
    -- Marking the first output with token for easy redeemer computation of withdrawal script.
    mustHaveOutput (mkGYTxOutNoDatum (utxoAddress authOut) (valueSingleton ac 1))
      <> foldMap
        ( \BuildOut{..} ->
            mustHaveOutput $
              GYTxOut
                { gyTxOutValue = boValue
                , gyTxOutRefS = Nothing
                , gyTxOutDatum =
                    fmap
                      ( \(dat, toInline) ->
                          ( dat
                          , if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum
                          )
                      )
                      boDatum
                , gyTxOutAddress = boAddress
                }
        )
        outs
      <> mustBeSignedBy zkswiPaymentKeyHash
      <> mustHaveWithdrawal
        ( GYTxWdrl
            { gyTxWdrlStakeAddress = stakeAddr
            , gyTxWdrlAmount = gyStakeAddressInfoAvailableRewards si
            , gyTxWdrlWitness =
                GYTxBuildWitnessPlutusScript
                  (GYBuildPlutusScriptInlined zkiwsCheckSig)
                  (redeemerFromPlutusData $ Signature 0 0)
            }
        )
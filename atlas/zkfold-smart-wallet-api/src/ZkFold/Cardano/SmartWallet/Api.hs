module ZkFold.Cardano.SmartWallet.Api (
  addressFromEmail,
  createWallet,
  createWallet',
  sendFunds,
  sendFunds',
  extraBuildConfiguration,
  batchTxs,
) where

import Data.Default (Default (..))
import Data.Foldable (find)
import GeniusYield.Transaction.Common
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Api.Batch
import ZkFold.Cardano.SmartWallet.Api.Create
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Cardano.UPLC.Wallet.Types

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
sendFunds' ZKInitializedWalletScripts{..} walletAddress ZKSpendWalletInfo{..} outs = do
  -- Find a UTxO at wallet's address that has a proof validity token. Require that token to be in output.
  walletOuts <- utxosAtAddress walletAddress Nothing
  let tn = tokenNameFromKeyHash zkswiPaymentKeyHash
      ac = GYToken (mintingPolicyId zkiwsWeb2Auth) tn
  authOut <- case find (\out -> valueAssetPresent (utxoValue out) ac) (utxosToList walletOuts) of
    Nothing -> throwAppError (ZKWENoAuthToken zkswiEmail walletAddress tn)
    Just out -> pure out
  nid <- networkId
  let stakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash zkiwsCheckSig)
  si <-
    stakeAddressInfo stakeAddr >>= \case
      Just si -> pure si
      Nothing -> throwAppError $ ZKWEStakeAddressInfoNotFound stakeAddr
  pure $
    mustHaveInput (GYTxIn{gyTxInTxOutRef = utxoRef authOut, gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptInlined zkiwsWallet) Nothing unitRedeemer})
      <>
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
            , gyTxWdrlWitness = GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptInlined zkiwsCheckSig) (redeemerFromPlutusData $ Signature 0 0)
            }
        )

-- | Extra build configuration to use when building transactions for zk wallet.
extraBuildConfiguration :: ZKInitializedWalletScripts -> GYTxExtraConfiguration 'PlutusV3
extraBuildConfiguration zkiws =
  def
    { gytxecUtxoInputMapper = \GYUTxO{..} ->
        GYTxInDetailed
          { gyTxInDet = GYTxIn utxoRef (GYTxInWitnessScript (GYBuildPlutusScriptInlined $ zkiwsWallet zkiws) Nothing unitRedeemer)
          , gyTxInDetAddress = utxoAddress
          , gyTxInDetValue = utxoValue
          , gyTxInDetDatum = utxoOutDatum
          , gyTxInDetScriptRef = utxoRefScript
          }
    }

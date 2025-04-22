module ZkFold.Cardano.SmartWallet.Api (
  addressFromEmail,
  createWallet,
  createWallet',
  registerAndDelegateWithdrawalScript,
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

registerAndDelegateWithdrawalScript ::
  (GYTxQueryMonad m) =>
  ZKInitializedWalletScripts ->
  -- | Address of the zk wallet.
  GYAddress ->
  ZKRegisterAndDelegateWithdrawalScriptInfo ->
  m (GYTxSkeleton 'PlutusV3)
registerAndDelegateWithdrawalScript zkiws@ZKInitializedWalletScripts{..} walletAddress ZKRegisterAndDelegateWithdrawalScriptInfo{..} = do
  (authOut, _ac) <- findMintedAuthToken zkiws walletAddress zkradiEmail zkradiPaymentKeyHash
  let stakeCred = GYCredentialByScript $ scriptHash zkiwsCheckSig
  pure $
    mustHaveRefInput (utxoRef authOut)
      <> mustBeSignedBy zkradiPaymentKeyHash
      <> mustHaveCertificate (mkStakeAddressCombinedRegistrationAndDelegationCertificate stakeCred (GYDelegStakeVote zkradiStakePool zkradiDRep) (GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptInlined zkiwsCheckSig) (redeemerFromPlutusData $ Signature 0 0)))

-- | Send funds from a zk-wallet.
sendFunds :: (ZKWalletQueryMonad m, Foldable f) => ZKSpendWalletInfo -> f BuildOut -> m (GYTxSkeleton 'PlutusV3)
sendFunds zkswi@ZKSpendWalletInfo{..} outs = do
  (zkiws, walletAddress) <- addressFromEmail zkswiEmail
  sendFunds' zkiws walletAddress zkswi outs

-- | Find a UTxO at wallet's address that has a proof validity token. Require that token to be in output.
findMintedAuthToken :: (GYTxQueryMonad m) => ZKInitializedWalletScripts -> GYAddress -> Email -> GYPaymentKeyHash -> m (GYUTxO, GYAssetClass)
findMintedAuthToken ZKInitializedWalletScripts{..} walletAddress email pkh = do
  walletOuts <- utxosAtAddress walletAddress Nothing
  let tn = tokenNameFromKeyHash pkh
      ac = GYToken (mintingPolicyId zkiwsWeb2Auth) tn
  case find (\out -> valueAssetPresent (utxoValue out) ac) (utxosToList walletOuts) of
    Nothing -> throwAppError (ZKWENoAuthToken email walletAddress tn)
    Just out -> pure (out, ac)

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
  (authOut, _ac) <- findMintedAuthToken zkiws walletAddress zkswiEmail zkswiPaymentKeyHash
  nid <- networkId
  let stakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash zkiwsCheckSig)
  si <-
    stakeAddressInfo stakeAddr >>= \case
      Just si -> pure si
      Nothing -> throwAppError $ ZKWEStakeAddressInfoNotFound stakeAddr
  pure $
    mustHaveRefInput (utxoRef authOut)
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

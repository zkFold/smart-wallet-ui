module ZkFold.Cardano.SmartWallet.Api (
  addressFromEmail,
  createWallet,
  createWallet',
  sendFunds,
  sendFunds',
) where

import Control.Monad.Reader (MonadReader (..))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import GeniusYield.Imports (Text, (&), (>>>))
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Cardano.UPLC.Wallet.Types

-- | Converts 'Text' to 'BuiltinByteString' assuming hex encoding for the associated bytestring.
textToBuiltinByteString :: Text -> PlutusTx.BuiltinByteString
textToBuiltinByteString = PlutusTx.toBuiltin >>> PlutusTx.encodeUtf8

web2CredsFromEmail :: Email -> Web2Creds
web2CredsFromEmail (emailToText -> email) =
  Web2Creds
    { w2cEmail = textToBuiltinByteString email
    }

jwtPartsFromJWT :: (GYTxQueryMonad m) => Email -> JWT -> m JWTParts
jwtPartsFromJWT email jwt = do
  let emailT = emailToText email
      jwtT = jwtToText jwt
  (prefix, suffixWithMail) <-
    if Text.null emailT
      then throwAppError $ ZKWEEmailError ZKEmailEmpty
      else pure $ Text.breakOn emailT jwtT
  if Text.null suffixWithMail
    then throwAppError $ ZKWEJWTError (ZKJWTEmailNotPresent jwt email)
    else
      pure $
        JWTParts
          { jwtPrefix = textToBuiltinByteString prefix
          , jwtSuffix = textToBuiltinByteString $ Text.drop (Text.length emailT) suffixWithMail
          }

-- | Obtain fully applied plutus scripts associated with this email.
initializeWalletScripts :: (ZKWalletQueryMonad m) => Email -> m ZKInitializedWalletScripts
initializeWalletScripts email = do
  ZKWalletBuildInfo{..} <- ask
  let web2AuthMP = zkwbiWeb2AuthMintingPolicy (web2CredsFromEmail email)
      checkSigScript = mintingPolicyId web2AuthMP & zkwbiCheckSigRewardValidator
      walletScript = checkSigScript & scriptHash & zkwbiWalletValidator
  pure $
    ZKInitializedWalletScripts
      { zkiwsCheckSig = checkSigScript
      , zkiwsWallet = walletScript
      , zkiwsWeb2Auth = web2AuthMP
      }

addressFromEmail :: (ZKWalletQueryMonad m) => Email -> m (ZKInitializedWalletScripts, GYAddress)
addressFromEmail email = do
  zkiws@ZKInitializedWalletScripts{zkiwsWallet} <- initializeWalletScripts email
  walletAddr <- addressFromScriptM zkiwsWallet
  pure $ (zkiws, walletAddr)

addressFromScriptM :: (GYTxQueryMonad m) => GYScript v -> m GYAddress
addressFromScriptM script = do
  nid <- networkId
  pure $ addressFromValidatorHash nid (scriptHash script)

tokenNameFromKeyHash :: GYPaymentKeyHash -> GYTokenName
tokenNameFromKeyHash = keyHashToRawBytes >>> tokenNameFromBS >>> fromJust -- 'fromJust' is safe as key hashes are <= 32 bytes (28 bytes actually).

-- | Initialize a zk-wallet.
createWallet :: (ZKWalletQueryMonad m) => ZKCreateWalletInfo -> m (GYTxSkeleton 'PlutusV3)
createWallet zkcwi@ZKCreateWalletInfo{..} = do
  zkiws <- initializeWalletScripts zkcwiEmail
  createWallet' zkcwi zkiws

-- | Initialize a zk-wallet.
createWallet' :: (GYTxQueryMonad m) => ZKCreateWalletInfo -> ZKInitializedWalletScripts -> m (GYTxSkeleton 'PlutusV3)
createWallet' ZKCreateWalletInfo{..} ZKInitializedWalletScripts{..} = do
  jwtParts <- jwtPartsFromJWT zkcwiEmail zkcwiJWT
  zkWalletAddr <- addressFromScriptM zkiwsWallet
  let
    tn = tokenNameFromKeyHash zkcwiPaymentKeyHash
    red = Web2Auth jwtParts (proofToPlutus zkcwiProofBytes) (tokenNameToPlutus tn)
  pure $
    mustMint
      (GYBuildPlutusScript $ GYBuildPlutusScriptInlined zkiwsWeb2Auth)
      (redeemerFromPlutusData red)
      tn
      1
      -- Not strictly required, but we prefer for token to be at zk wallet's address.
      <> mustHaveOutput (mkGYTxOutNoDatum zkWalletAddr (valueSingleton (GYToken (mintingPolicyId zkiwsWeb2Auth) tn) 1))

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
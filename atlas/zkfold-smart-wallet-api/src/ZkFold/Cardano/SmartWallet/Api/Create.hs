module ZkFold.Cardano.SmartWallet.Api.Create (
  initializeWalletScripts,
  addressFromEmail,
  tokenNameFromKeyHash,
  createWallet,
  createWallet',
) where

import Control.Monad.Reader (MonadReader (..))
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import GeniusYield.Imports (Text, (>>>))
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
      web2AuthMPId = mintingPolicyId web2AuthMP
      checkSigScript = zkwbiCheckSigRewardValidator web2AuthMPId
      walletScript = zkwbiWalletValidator web2AuthMPId $ scriptHash checkSigScript
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
  pure (zkiws, walletAddr)

addressFromScriptM :: (GYTxQueryMonad m) => GYScript v -> m GYAddress
addressFromScriptM script = do
  nid <- networkId
  pure $ addressFromValidatorHash nid (scriptHash script)

tokenNameFromKeyHash :: GYPaymentKeyHash -> GYTokenName
tokenNameFromKeyHash = keyHashToRawBytes >>> tokenNameFromBS >>> fromJust -- 'fromJust' is safe as key hashes are <= 32 bytes (28 bytes actually).

-- | Initialize a zk-wallet.
createWallet :: (ZKWalletQueryMonad m) => ZKCreateWalletInfo -> m (GYTxSkeleton 'PlutusV3)
createWallet zkcwi@ZKCreateWalletInfo{..} = do
  (zkiws, zkWalletAddr) <- addressFromEmail zkcwiEmail
  createWallet' zkcwi zkiws zkWalletAddr

-- | Initialize a zk-wallet.
createWallet' :: (GYTxQueryMonad m) => ZKCreateWalletInfo -> ZKInitializedWalletScripts -> GYAddress -> m (GYTxSkeleton 'PlutusV3)
createWallet' ZKCreateWalletInfo{..} ZKInitializedWalletScripts{..} zkWalletAddr = do
  jwtParts <- jwtPartsFromJWT zkcwiEmail zkcwiJWT
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
      <> mustBeSignedBy zkcwiPaymentKeyHash
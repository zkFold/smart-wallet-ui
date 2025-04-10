module ZkFold.Cardano.SmartWallet.Api (
  addressFromEmail,
  createWallet,
  sendFunds,
) where

import Control.Monad ((>=>))
import Control.Monad.Reader (MonadReader (..))
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import GeniusYield.Imports (Text, (&), (<&>), (>>>))
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Cardano.UPLC.Wallet.Types

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
  nid <- networkId
  zkiws@ZKInitializedWalletScripts{zkiwsWallet} <- initializeWalletScripts email
  pure $ (zkiws, addressFromValidatorHash nid (scriptHash zkiwsWallet))

createWallet :: (ZKWalletQueryMonad m) => ZKCreateWalletInfo -> m (GYTxSkeleton 'PlutusV3)
createWallet zkcwi@ZKCreateWalletInfo{..} = do
  zkiws <- initializeWalletScripts zkcwiEmail
  createWallet' zkcwi zkiws

createWallet' :: (GYTxQueryMonad m) => ZKCreateWalletInfo -> ZKInitializedWalletScripts -> m (GYTxSkeleton 'PlutusV3)
createWallet' ZKCreateWalletInfo{..} ZKInitializedWalletScripts{..} = do
  jwtParts <- jwtPartsFromJWT zkcwiEmail zkcwiJWT
  let
    -- 'fromJust' is safe as key hashes are <= 32 bytes (28 bytes actually).
    tn = zkcwiPaymentKeyHash & keyHashToRawBytes & tokenNameFromBS & fromJust
    red = Web2Auth jwtParts zkcwiProofBytes (tokenNameToPlutus tn)
  pure $
    mustMint
      (GYBuildPlutusScript $ GYBuildPlutusScriptInlined zkiwsWeb2Auth)
      (redeemerFromPlutusData red)
      tn
      1

-- | Send funds from a zk-wallet to a given address.
sendFunds :: (ZKWalletQueryMonad m) => Email -> GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
sendFunds email sendAddr sendVal = undefined -- validatorSetupFromEmail email >>= \validatorSetup -> sendFunds' validatorSetup sendAddr sendVal

-- sendFunds' :: (ZKWalletQueryMonad m) => ValidatorSetup -> GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
-- sendFunds' (sb, ws) sendAddr sendVal = do
--   nid <- networkId
--   zkwbi <- ask
--   let stakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash $ zkwbiSmartWalletValidator zkwbi sb ws)
--   let mockStakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash $ zkwbiMockStakeValidator zkwbi)
--   -- TODO: Perhaps stake address information is not required if we are sure that withdrawal amount would always be zero.
--   si <-
--     stakeAddressInfo stakeAddr >>= \case
--       Just si -> pure si
--       Nothing -> throwAppError $ ZKWEStakeAddressInfoNotFound stakeAddr
--   pure $
--     mustHaveOutput (mkGYTxOutNoDatum sendAddr sendVal)
--       -- TODO: To make use of reference scripts?
--       <> mustHaveWithdrawal
--         ( GYTxWdrl
--             { gyTxWdrlStakeAddress = mockStakeAddr
--             , gyTxWdrlAmount = gyStakeAddressInfoAvailableRewards si
--             , gyTxWdrlWitness = GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptInlined (zkwbiMockStakeValidator zkwbi)) dummyRedeemer
--             }
--         )
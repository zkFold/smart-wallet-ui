module ZkFold.Cardano.SmartWallet.Constants (
  ezkWalletBuildInfo,
  zkWalletBuildInfo,
)
where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Short qualified as SBS
import Data.Foldable (find)
import Data.Text (unpack)
import GeniusYield.Imports (Text, encodeUtf8, (&), (<&>))
import GeniusYield.Types
import ZkFold.Algebra.Field (toZp)
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes)
import ZkFold.Cardano.SmartWallet.Types (ZKWalletBuildInfo (..), expModSetupMock, mkSetup, setupToPlutus)
import ZkFold.Cardano.UPLC.Wallet.CompiledScript

-- FIXME: replace zero with an appropriate secret

-- | Common 'SetupBytes' used across multiple zk-wallets.
walletSetupBytes :: SetupBytes
walletSetupBytes = setupToPlutus . mkSetup $ expModSetupMock @ByteString (toZp 0)

-- | Select a particular validator from blueprints file.
selectValScript ::
  (SingPlutusVersionI v, Foldable f, Show (f ValidatorBlueprint)) =>
  -- | Validators in a blueprint file.
  f ValidatorBlueprint ->
  -- | Validator title to match against.
  Text ->
  Either String (GYScript v)
selectValScript vals valTitle = do
  val <- case find (\val -> validatorTitle val == valTitle) vals of
    Nothing -> Left $ "Couldn't find validator \"" <> unpack valTitle <> "\" in validator blueprints: " <> show vals
    Just val -> Right val
  valCC <- case validatorCompiled val of
    Nothing -> Left $ "no compiled code of validator, \"" <> unpack valTitle <> "\" found"
    Just cc -> Right cc
  encodeUtf8 (compiledValidatorCode valCC) & BS16.decode <&> scriptFromSerialisedScript . SBS.toShort

ezkWalletBuildInfo :: Either String ZKWalletBuildInfo
ezkWalletBuildInfo = do
  bp <- Aeson.eitherDecodeStrict smartWalletBPFile
  let vals = contractValidators bp
  web2Auth <- selectValScript @PlutusV3 vals "web2Auth"
  wallet <- selectValScript @PlutusV3 vals "wallet"
  checkSig <- selectValScript @PlutusV3 vals "checkSig"
  pure $
    ZKWalletBuildInfo
      { zkwbiWeb2AuthMintingPolicy = applyParam (applyParam web2Auth walletSetupBytes)
      , zkwbiWalletValidator = \cs sh -> applyParam (applyParam wallet (mintingPolicyIdToCurrencySymbol cs)) (scriptHashToPlutus sh)
      , zkwbiCheckSigRewardValidator = applyParam checkSig . mintingPolicyIdToCurrencySymbol
      }

zkWalletBuildInfo :: ZKWalletBuildInfo
zkWalletBuildInfo = either error id ezkWalletBuildInfo

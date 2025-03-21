module ZkFold.Cardano.SmartWallet.Server.Config (
  ServerConfig (..),
  serverConfigOptionalFPIO,
  coreConfigFromServerConfig,
  signingKeyFromUserWallet,
) where

import Data.Aeson (
  eitherDecodeFileStrict,
  eitherDecodeStrict,
 )
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Word (Word32)
import Data.Yaml qualified as Yaml
import Deriving.Aeson
import GeniusYield.GYConfig (Confidential, GYCoreConfig (..), GYCoreProviderInfo)
import GeniusYield.Imports (Text, throwIO, (&))
import GeniusYield.Types hiding (Port)
import Network.Wai.Handler.Warp (Port)
import System.Envy
import System.FilePath (takeExtension)

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import           Data.Proxy
-}

-- >>> Aeson.encode (MnemonicWallet (MnemonicWalletDetails ["hello"] (Just 1) (Just 2)))
-- "{\"tag\":\"mnemonicWallet\",\"contents\":{\"mnemonic\":[\"hello\"],\"acc_ix\":1,\"addr_ix\":2}}"
data UserWallet = MnemonicWallet !MnemonicWalletDetails | KeyPathWallet !FilePath
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] UserWallet

-- | Details of a wallet to be created from a mnemonic.
data MnemonicWalletDetails = MnemonicWalletDetails
  { mnemonic :: !Mnemonic
  -- ^ Mnemonic (seed phrase).
  , accIx :: !(Maybe Word32)
  -- ^ Account index.
  , addrIx :: !(Maybe Word32)
  -- ^ Payment address index.
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Configuration for the server.
data ServerConfig = ServerConfig
  { scCoreProvider :: !GYCoreProviderInfo
  , scNetworkId :: !GYNetworkId
  , scLogging :: ![GYLogScribeConfig]
  , scPort :: !Port
  , scServerApiKey :: !(Confidential Text)
  , scCollateral :: !GYTxOutRef
  , scCollateralWallet :: !UserWallet
  }
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "sc", LowerFirst]] ServerConfig

instance FromEnv ServerConfig where
  fromEnv _ = forceFromJsonOrYaml <$> env "SERVER_CONFIG"
   where
    forceFromJsonOrYaml :: (FromJSON a) => String -> a
    forceFromJsonOrYaml s =
      let bs = fromString s
          parseResults = eitherDecodeStrict bs :| [first show $ Yaml.decodeEither' bs]
       in go parseResults
     where
      go (x :| []) = case x of
        Left e -> error e
        Right a -> a
      go (x :| y : ys) = case x of
        Left _ -> go (y :| ys)
        Right a -> a

eitherDecodeFileStrictJsonOrYaml :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrictJsonOrYaml fp =
  case takeExtension fp of
    ".json" -> eitherDecodeFileStrict fp
    ".yaml" -> first show <$> Yaml.decodeFileEither fp
    _ -> throwIO $ userError "Only .json or .yaml extensions are supported for configuration."

serverConfigOptionalFPIO :: Maybe FilePath -> IO ServerConfig
serverConfigOptionalFPIO mfp = do
  e <- maybe decodeEnv eitherDecodeFileStrictJsonOrYaml mfp
  either (throwIO . userError) return e

coreConfigFromServerConfig :: ServerConfig -> GYCoreConfig
coreConfigFromServerConfig ServerConfig{..} =
  GYCoreConfig
    { cfgCoreProvider = scCoreProvider
    , cfgNetworkId = scNetworkId
    , cfgLogging = scLogging
    , cfgLogTiming = Nothing
    }

signingKeyFromUserWallet :: GYNetworkId -> UserWallet -> IO (Maybe (GYSomePaymentSigningKey, GYAddress))
signingKeyFromUserWallet nid (MnemonicWallet MnemonicWalletDetails{..}) = do
  let wk' = walletKeysFromMnemonicIndexed mnemonic (fromMaybe 0 accIx) (fromMaybe 0 addrIx)
   in pure $ case wk' of
        Left _ -> Nothing
        Right wk -> Just (AGYExtendedPaymentSigningKey (walletKeysToExtendedPaymentSigningKey wk), walletKeysToAddress wk nid)
signingKeyFromUserWallet nid (KeyPathWallet fp) = do
  skey <- readSomePaymentSigningKey fp
  pure $ Just (skey, addressFromSomePaymentSigningKey skey)
 where
  addressFromSomePaymentSigningKey :: GYSomePaymentSigningKey -> GYAddress
  addressFromSomePaymentSigningKey skey =
    let pkh =
          case skey of
            AGYPaymentSigningKey skey' -> paymentKeyHash . paymentVerificationKey $ skey'
            AGYExtendedPaymentSigningKey skey' -> getExtendedVerificationKey skey' & extendedVerificationKeyHash
     in addressFromPaymentKeyHash nid pkh
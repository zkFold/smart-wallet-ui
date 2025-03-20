module ZkFold.Cardano.SmartWallet.Server.Api.Settings (
  SettingsAPI,
  handleSettings,
) where

import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Swagger qualified as Swagger
import Data.Version (showVersion)
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&), (>>>))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import PackageInfo_zkfold_smart_wallet_server_lib qualified as PackageInfo
import Servant
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Orphans ()
import ZkFold.Cardano.SmartWallet.Server.Utils

type SettingsPrefix :: Symbol
type SettingsPrefix = "settings"

data Settings = Settings
  { settingsNetwork :: !String
  , settingsVersion :: !String
  , settingsCollateral :: !GYTxOutRef
  , settingsCollateralAddress :: !GYAddressBech32
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SettingsPrefix, CamelToSnake]] Settings

instance Swagger.ToSchema Settings where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SettingsPrefix}
      & addSwaggerDescription "zkFold Smart Wallet Server settings."

type SettingsAPI = Summary "Server settings" :> Description "Get server settings such as network and version. Optionally if a collateral UTxO reference and signing key are individually configured in the server (provided server is spun up locally), then it's details are also returned." :> Get '[JSON] Settings

handleSettings :: Ctx -> IO Settings
handleSettings ctx@Ctx{..} = do
  logInfo ctx "Settings requested."
  pure $
    Settings
      { settingsNetwork = ctxNetworkId & customShowNetworkId
      , settingsVersion = showVersion PackageInfo.version
      , settingsCollateral = ctxCollateral
      , settingsCollateralAddress = addressToBech32 . snd $ ctxCollateralKey
      }

-- >>> customShowNetworkId GYMainnet
-- "mainnet"
-- >>> customShowNetworkId GYTestnetLegacy
-- "legacy"
-- >>> customShowNetworkId GYPrivnet
-- "privnet"
customShowNetworkId :: GYNetworkId -> String
customShowNetworkId = show >>> removePrefix "GY" >>> removePrefix "Testnet" >>> lowerFirstChar
 where
  removePrefix :: String -> String -> String
  removePrefix pref str
    | pref `isPrefixOf` str = drop (length pref) str
    | otherwise = str
  lowerFirstChar :: String -> String
  lowerFirstChar "" = ""
  lowerFirstChar (x : xs) = toLower x : xs

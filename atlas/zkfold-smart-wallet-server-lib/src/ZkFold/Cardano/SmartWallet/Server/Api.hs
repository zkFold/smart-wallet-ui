module ZkFold.Cardano.SmartWallet.Server.Api (
  ZkFoldSmartWalletAPI,
  zkFoldSmartWalletAPI,
  zkFoldSmartWalletServer,
  MainAPI,
  mainAPI,
  mainServer,
  zkFoldSmartWalletAPIOpenApi,
) where

import Control.Lens ((.~), (?~))
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as K
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (isPrefixOf, sortBy)
import Data.Map.Strict qualified as Map
import Data.OpenApi
import Data.OpenApi qualified as OpenApi
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as T
import Data.Version (showVersion)
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&), (>>>))
import GeniusYield.TxBuilder (GYTxQueryMonad (utxosAtAddress))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import PackageInfo_zkfold_smart_wallet_server_lib qualified as PackageInfo
import Servant
import Servant.OpenApi
import ZkFold.Cardano.SmartWallet.Server.Auth (APIKeyAuthProtect, V0)
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Orphans ()
import ZkFold.Cardano.SmartWallet.Server.Utils
import ZkFold.Cardano.SmartWallet.Types (ZKWalletBuildInfo)

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import           Data.Proxy
>>> import qualified Data.Swagger               as Swagger
>>> import GeniusYield.Types
-}

-------------------------------------------------------------------------------
-- Settings.
-------------------------------------------------------------------------------

type SettingsPrefix :: Symbol
type SettingsPrefix = "settings"

data Settings = Settings
  { settingsNetwork :: !String
  , settingsVersion :: !String
  , settingsAddress :: !(Maybe GYAddressBech32)
  , settingsStakeAddress :: !(Maybe GYStakeAddressBech32)
  , settingsCollateral :: !(Maybe GYTxOutRef)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SettingsPrefix, CamelToSnake]] Settings

instance Swagger.ToSchema Settings where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SettingsPrefix}
      & addSwaggerDescription "Genius Yield Server settings."

-------------------------------------------------------------------------------
-- Server's API.
-------------------------------------------------------------------------------

type SettingsAPI = Summary "Server settings" :> Description "Get server settings such as network and version. Optionally if a collateral UTxO reference and signing key are individually configured in the server (provided server is spun up locally), then it's details are also returned." :> Get '[JSON] Settings

type V0API =
  "settings" :> SettingsAPI

type ZkFoldSmartWalletAPI = APIKeyAuthProtect :> V0 :> V0API

zkFoldSmartWalletAPI :: Proxy ZkFoldSmartWalletAPI
zkFoldSmartWalletAPI = Proxy

infixr 4 +>

type family (+>) (api1 :: k) (api2 :: Type) where
  (+>) api1 api2 = APIKeyAuthProtect :> V0 :> api1 :> api2

zkFoldSmartWalletAPIOpenApi :: OpenApi
zkFoldSmartWalletAPIOpenApi =
  toOpenApi zkFoldSmartWalletAPI
    & info
    . OpenApi.title
    .~ "zkFold Smart Wallet Server API"
      & info
      . version
    .~ "0.0.1"
      & info
      . license
    ?~ ("Apache-2.0" & url ?~ URL "https://opensource.org/licenses/apache-2-0")
      & info
      . contact
    ?~ ( mempty
          & url
          ?~ URL "https://zkfold.io/"
            & email
          ?~ "info@zkfold.io"
            & name
          ?~ "zkFold Technical Support"
       )
      & info
      . OpenApi.description
    ?~ "API to interact with zkFold Smart Wallet."
      & applyTagsFor (subOperations (Proxy :: Proxy ("settings" +> SettingsAPI)) (Proxy :: Proxy ZkFoldSmartWalletAPI)) ["Settings" & OpenApi.description ?~ "Endpoint to get server settings such as network and version"]

zkFoldSmartWalletServer :: Ctx -> ServerT ZkFoldSmartWalletAPI IO
zkFoldSmartWalletServer ctx =
  ignoredAuthResult $
    handleSettings ctx
 where
  ignoredAuthResult f _authResult = f

type MainAPI =
  ZkFoldSmartWalletAPI

mainAPI :: Proxy MainAPI
mainAPI = Proxy

mainServer :: Ctx -> ServerT MainAPI IO
mainServer = zkFoldSmartWalletServer

handleSettings :: Ctx -> IO Settings
handleSettings ctx@Ctx{..} = do
  logInfo ctx "Settings requested."
  pure $
    Settings
      { settingsNetwork = ctxNetworkId & customShowNetworkId
      , settingsVersion = showVersion PackageInfo.version
      , settingsAddress = fmap (addressToBech32 . snd) ctxSigningKey
      , settingsStakeAddress = ctxStakeAddress
      , settingsCollateral = ctxCollateral
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

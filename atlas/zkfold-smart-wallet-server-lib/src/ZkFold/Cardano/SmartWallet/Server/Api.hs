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
import Data.Kind (Type)
import Data.OpenApi
import Data.OpenApi qualified as OpenApi
import GeniusYield.Imports ((&))
import GeniusYield.Types.OpenApi ()
import Servant
import Servant.OpenApi
import ZkFold.Cardano.SmartWallet.Server.Api.Settings
import ZkFold.Cardano.SmartWallet.Server.Api.Tx (TxAPI, handleTxApi)
import ZkFold.Cardano.SmartWallet.Server.Api.Wallet (WalletAPI, handleWalletApi)
import ZkFold.Cardano.SmartWallet.Server.Auth (APIKeyAuthProtect, V0)
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Orphans ()

-------------------------------------------------------------------------------
-- Server's API.
-------------------------------------------------------------------------------

type V0API =
  "settings" :> SettingsAPI
    :<|> "wallet" :> WalletAPI
    :<|> "tx" :> TxAPI

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
      & applyTagsFor (subOperations (Proxy :: Proxy ("wallet" +> WalletAPI)) (Proxy :: Proxy ZkFoldSmartWalletAPI)) ["Wallet" & OpenApi.description ?~ "Endpoint to interact with zk-wallet"]
      & applyTagsFor (subOperations (Proxy :: Proxy ("tx" +> TxAPI)) (Proxy :: Proxy ZkFoldSmartWalletAPI)) ["Tx" & OpenApi.description ?~ "Endpoint to interact with built transactions"]

zkFoldSmartWalletServer :: Ctx -> ServerT ZkFoldSmartWalletAPI IO
zkFoldSmartWalletServer ctx =
  ignoredAuthResult $
    handleSettings ctx
      :<|> handleWalletApi ctx
      :<|> handleTxApi ctx
 where
  ignoredAuthResult f _authResult = f

type MainAPI =
  ZkFoldSmartWalletAPI

mainAPI :: Proxy MainAPI
mainAPI = Proxy

mainServer :: Ctx -> ServerT MainAPI IO
mainServer = zkFoldSmartWalletServer
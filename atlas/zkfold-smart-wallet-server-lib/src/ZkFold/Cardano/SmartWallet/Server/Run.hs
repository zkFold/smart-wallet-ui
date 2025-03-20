module ZkFold.Cardano.SmartWallet.Server.Run (
  runServer,
) where

import Control.Exception (Exception (..), SomeException, try)
import Control.Monad.Except (ExceptT (..))
import Data.ByteString qualified as B
import Data.Text.Lazy qualified as LT
import Data.Version (showVersion)
import Data.Yaml.Pretty qualified as Yaml
import Fmt
import GeniusYield.GYConfig
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import PackageInfo_zkfold_smart_wallet_server_lib qualified as PackageInfo
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Internal.ServerError (responseServerError)
import System.TimeManager (TimeoutThread (..))
import ZkFold.Cardano.SmartWallet.Constants
import ZkFold.Cardano.SmartWallet.Server.Api
import ZkFold.Cardano.SmartWallet.Server.Auth
import ZkFold.Cardano.SmartWallet.Server.Config (ServerConfig (..), coreConfigFromServerConfig, serverConfigOptionalFPIO, signingKeyFromUserWallet)
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.ErrorMiddleware
import ZkFold.Cardano.SmartWallet.Server.RequestLoggerMiddleware (gcpReqLogger)
import ZkFold.Cardano.SmartWallet.Server.Utils
import ZkFold.Cardano.SmartWallet.Types (ZKWalletBuildInfo (..))

runServer :: Maybe FilePath -> IO ()
runServer mfp = do
  serverConfig <- serverConfigOptionalFPIO mfp
  collateralKey <-
    signingKeyFromUserWallet (scNetworkId serverConfig) (scCollateralWallet serverConfig)
      >>= \case
        Nothing -> throwIO $ userError "Collateral signing key not found."
        Just k -> pure k
  let nid = scNetworkId serverConfig
      coreCfg = coreConfigFromServerConfig serverConfig
  -- writePythonForAPI (Proxy @MainAPI) requests "web/swagger/api.py"
  withCfgProviders coreCfg "server" $ \providers -> do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
    logInfoS $ "zkFold smart wallet server version: " +| showVersion PackageInfo.version |+ "\ncollateral configuration: " +|| scCollateral serverConfig ||+ "\nAddress of collateral wallet: " +|| snd collateralKey ||+ ""
    B.writeFile "web/openapi/api.yaml" (Yaml.encodePretty Yaml.defConfig zkFoldSmartWalletAPI)
    reqLoggerMiddleware <- gcpReqLogger
    let
      -- These are only meant to catch fatal exceptions, application thrown exceptions should be caught beforehand.
      onException :: req -> SomeException -> IO ()
      onException _req exc =
        displayException exc
          & if isMatchedException exceptionsToIgnore exc
            then logInfoS
            else logErrorS
       where
        -- TimeoutThread and Warp.ConnectionClosedByPeer do not indicate that anything is wrong and
        -- should not be logged as errors. See
        -- https://magnus.therning.org/2021-07-03-the-timeout-manager-exception.html
        -- https://www.rfc-editor.org/rfc/rfc5246#page-29
        exceptionsToIgnore = Proxy @TimeoutThread :>> Proxy @Warp.InvalidRequest :>> ENil
      onExceptionResponse :: SomeException -> Wai.Response
      onExceptionResponse _ = responseServerError . apiErrorToServerError $ someBackendError "Internal Server Error"
      settings =
        Warp.defaultSettings
          & Warp.setPort (scPort serverConfig)
          & Warp.setOnException onException
          & Warp.setOnExceptionResponse onExceptionResponse
      errLoggerMiddleware = errorLoggerMiddleware $ logErrorS . LT.unpack
      ctx =
        Ctx
          { ctxProviders = providers
          , ctxNetworkId = nid
          , ctxSmartWalletBuildInfo = ZKWalletBuildInfo smartWalletValidator mockSmartWalletStakeValidator
          , ctxCollateral = scCollateral serverConfig
          , ctxCollateralKey = collateralKey
          }

    logInfoS $
      "Starting zkFold-smart-wallet server on port "
        +| scPort serverConfig
        |+ "\nCore config:\n"
        +| indentF 4 (fromString $ show coreCfg)
        |+ ""
    Warp.runSettings settings
      . reqLoggerMiddleware
      . errLoggerMiddleware
      . errorJsonWrapMiddleware
      $ let context = apiKeyAuthHandler (case scServerApiKey serverConfig of Confidential t -> apiKeyFromText t) :. EmptyContext
         in serveWithContext mainAPI context
              $ hoistServerWithContext
                mainAPI
                (Proxy :: Proxy '[AuthHandler Wai.Request ()])
                (\ioAct -> Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try ioAct)
              $ mainServer ctx

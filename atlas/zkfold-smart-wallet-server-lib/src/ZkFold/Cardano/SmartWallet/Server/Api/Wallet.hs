module ZkFold.Cardano.SmartWallet.Server.Api.Wallet (
  ZKWalletBuildException,
  WalletAPI,
  handleWalletApi,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Maybe (fromMaybe, isNothing)
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.HTTP.Errors
import GeniusYield.Imports ((&))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import Network.HTTP.Types (status400)
import Servant
import ZkFold.Cardano.SmartWallet.Api
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Orphans ()
import ZkFold.Cardano.SmartWallet.Server.Tx (handleTxSignCollateral)
import ZkFold.Cardano.SmartWallet.Server.Utils
import ZkFold.Cardano.SmartWallet.Types

type ObtainAddressPrefix :: Symbol
type ObtainAddressPrefix = "oap"

newtype ObtainAddressParameters = ObtainAddressParameters
  { oapEmail :: Email
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix ObtainAddressPrefix, CamelToSnake]] ObtainAddressParameters

instance Swagger.ToSchema ObtainAddressParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @ObtainAddressPrefix}
      & addSwaggerDescription "Obtain user's wallet address."

type ObtainAddressResponsePrefix :: Symbol
type ObtainAddressResponsePrefix = "oar"

newtype ObtainAddressResponse = ObtainAddressResponse
  { oarAddress :: GYAddressBech32
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix ObtainAddressResponsePrefix, CamelToSnake]] ObtainAddressResponse

instance Swagger.ToSchema ObtainAddressResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @ObtainAddressResponsePrefix}
      & addSwaggerDescription "User's address."

type CreateWalletPrefix :: Symbol
type CreateWalletPrefix = "cwp"

data CreateWalletParameters = CreateWalletParameters
  { cwpEmail :: !Email
  -- ^ User's email.
  , cwpJWT :: !JWT
  -- ^ JSON web token.
  , cwpPaymentKeyHash :: !GYPaymentKeyHash
  -- ^ Associated payment key hash.
  , cwpProofBytes :: !ZKProofBytes
  -- ^ Computed proof bytes.
  , cwpFundAddress :: !(Maybe GYAddressBech32)
  -- ^ Address which will fund this transaction. If not provided, we assume address of user's zk wallet.
  , cwpCollateral :: !(Maybe GYTxOutRef)
  -- ^ Optional collateral to use. If not provided, we try picking suitable UTxO for collateral from fund address.
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CreateWalletPrefix, CamelToSnake]] CreateWalletParameters

instance Swagger.ToSchema CreateWalletParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CreateWalletPrefix}
      & addSwaggerDescription "Initialize zk smart wallet. If \"fund_address\" is not provided, we use address of the wallet (which is to be initialized here). If \"collateral\" is not provided, we use suitable collateral from \"fund_address\"."

type CreateWalletResponsePrefix :: Symbol
type CreateWalletResponsePrefix = "cwr"

data CreateWalletResponse = CreateWalletResponse
  { cwrAddress :: !GYAddressBech32
  , cwrTransaction :: !GYTx
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CreateWalletResponsePrefix, CamelToSnake]] CreateWalletResponse

instance Swagger.ToSchema CreateWalletResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CreateWalletResponsePrefix}
      & addSwaggerDescription "Transaction which would initialize user's wallet."

type SendFundsPrefix :: Symbol
type SendFundsPrefix = "sfp"

data SendFundsParameters = SendFundsParameters
  { sfpValue :: !GYValue
  , sfpEmail :: !Email
  , sfpPaymentKeyHash :: !GYPaymentKeyHash
  , sfpOuts :: ![BuildOut]
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SendFundsPrefix, CamelToSnake]] SendFundsParameters

instance Swagger.ToSchema SendFundsParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SendFundsPrefix}
      & addSwaggerDescription "Send funds parameters."

type SendFundsResponsePrefix :: Symbol
type SendFundsResponsePrefix = "sfr"

data SendFundsResponse = SendFundsResponse
  { sfrTransaction :: !GYTx
  , sfrTransactionId :: !GYTxId
  , sfrTransactionFee :: !GYNatural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SendFundsResponsePrefix, CamelToSnake]] SendFundsResponse

instance Swagger.ToSchema SendFundsResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SendFundsResponsePrefix}
      & addSwaggerDescription "Send funds response."

data ZKWalletBuildException
  = -- | We can't take UTxO as collateral from zk smart wallet as collateral UTxO can't themselves be locked by plutus script.
    ZKWBENoCollateralAvailable
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError ZKWalletBuildException where
  toApiError ZKWBENoCollateralAvailable =
    GYApiError
      { gaeErrorCode = "COLLATERAL_CANT_BE_CHOSEN"
      , gaeHttpStatus = status400
      , gaeMsg = "No collateral output was provided. Note that we can't take UTxO from zk smart wallet as collateral since collateral UTxOs can't themselves be locked by plutus script."
      }

type WalletAPI =
  Summary "Obtain address."
    :> Description "Obtain address of the wallet initialized with the given mail."
    :> "address"
    :> ReqBody '[JSON] ObtainAddressParameters
    :> Post '[JSON] ObtainAddressResponse
    :<|> Summary "Create wallet."
      :> Description "Create zk smart wallet."
      :> "create"
      :> ReqBody '[JSON] CreateWalletParameters
      :> Post '[JSON] CreateWalletResponse
    :<|> Summary "Send funds."
      :> Description "Send funds from a zk based wallet to a given address."
      :> "send-funds"
      :> ReqBody '[JSON] SendFundsParameters
      :> Post '[JSON] SendFundsResponse

handleWalletApi :: Ctx -> ServerT WalletAPI IO
handleWalletApi ctx =
  handleObtainAddress ctx
    :<|> handleCreateWallet ctx
    :<|> handleSendFunds ctx

handleObtainAddress :: Ctx -> ObtainAddressParameters -> IO ObtainAddressResponse
handleObtainAddress ctx oap@ObtainAddressParameters{..} = do
  logInfo ctx $ "Obtaining user's address. Parameters: " +|| oap ||+ ""
  (_, addressToBech32 -> walletAddress) <- runQuery ctx (addressFromEmail oapEmail)
  logInfo ctx $ "Wallet address computed: " +|| walletAddress ||+ ""
  pure $
    ObtainAddressResponse
      { oarAddress = walletAddress
      }

handleCreateWallet :: Ctx -> CreateWalletParameters -> IO CreateWalletResponse
handleCreateWallet ctx cwp@CreateWalletParameters{..} = do
  logInfo ctx $ "Creating user's wallet. Parameters: " +|| cwp ||+ ""
  (zkiws, addressToBech32 -> walletAddress) <- runQuery ctx $ addressFromEmail cwpEmail
  let fundWallet = fromMaybe walletAddress cwpFundAddress
      fundWallet' = addressFromBech32 fundWallet
  logInfo ctx $ "Fund wallet address: " +|| fundWallet ||+ ""
  when (isNothing cwpFundAddress && isNothing cwpCollateral) $ throwIO ZKWBENoCollateralAvailable
  body <-
    runSkeletonI ctx [fundWallet'] fundWallet' cwpCollateral $
      createWallet'
        ( ZKCreateWalletInfo
            { zkcwiProofBytes = cwpProofBytes
            , zkcwiPaymentKeyHash = cwpPaymentKeyHash
            , zkcwiJWT = cwpJWT
            , zkcwiEmail = cwpEmail
            }
        )
        zkiws
  logInfo ctx $ "Tranasction body: " +|| body ||+ ""
  pure $
    CreateWalletResponse
      { cwrAddress = walletAddress
      , cwrTransaction = unsignedTx body
      }

handleSendFunds :: Ctx -> SendFundsParameters -> IO SendFundsResponse
handleSendFunds ctx@Ctx{..} sfp@SendFundsParameters{..} = do
  logInfo ctx $ "Send funds requested. Parameters: " +|| sfp ||+ ""
  (zkiws, walletAddress) <- runQuery ctx $ addressFromEmail sfpEmail
  logInfo ctx $ "Wallet address: " +|| addressToBech32 walletAddress ||+ ""
  let ec = extraBuildConfiguration zkiws
  txBody <- runSkeletonWithExtraConfigurationI ec ctx [walletAddress] walletAddress (Just ctxCollateral) $ do
    sendFunds' zkiws walletAddress (ZKSpendWalletInfo{zkswiPaymentKeyHash = sfpPaymentKeyHash, zkswiEmail = sfpEmail}) sfpOuts
  signedTx <- handleTxSignCollateral ctx $ unsignedTx txBody
  pure $
    SendFundsResponse
      { sfrTransaction = signedTx
      , sfrTransactionId = txBodyTxId txBody
      , sfrTransactionFee = fromIntegral $ txBodyFee txBody
      }
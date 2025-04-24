module ZkFold.Cardano.SmartWallet.Server.Api.Wallet (
  WalletAPI,
  handleWalletApi,
) where

import Data.Swagger qualified as Swagger
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import Servant
import ZkFold.Cardano.SmartWallet.Api
import ZkFold.Cardano.SmartWallet.Constants (extraBuildConfiguration)
import ZkFold.Cardano.SmartWallet.Server.Api.Tx (handleTxSignCollateral)
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Orphans ()
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
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix CreateWalletPrefix, CamelToSnake]] CreateWalletParameters

instance Swagger.ToSchema CreateWalletParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CreateWalletPrefix}
      & addSwaggerDescription "Initialize zk smart wallet. If \"fund_address\" is not provided, we use address of the wallet (which is to be initialized here). Collateral UTxO configured inside server is used for this transaction."

type CreateWalletResponsePrefix :: Symbol
type CreateWalletResponsePrefix = "cwr"

data CreateWalletResponse = CreateWalletResponse
  { cwrAddress :: !GYAddressBech32
  -- ^ Address of the zk-wallet.
  , cwrTransaction :: !GYTx
  , cwrTransactionId :: !GYTxId
  , cwrTransactionFee :: !GYNatural
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
  { sfpEmail :: !Email
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

type CreateAndSendFundsPrefix :: Symbol
type CreateAndSendFundsPrefix = "casfp"

data CreateAndSendFundsParameters = CreateAndSendFundsParameters
  { casfpEmail :: !Email
  , casfpJWT :: !JWT
  , casfpPaymentKeyHash :: !GYPaymentKeyHash
  , casfpProofBytes :: !ZKProofBytes
  , casfpOuts :: ![BuildOut]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix CreateAndSendFundsPrefix, CamelToSnake]] CreateAndSendFundsParameters

instance Swagger.ToSchema CreateAndSendFundsParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CreateAndSendFundsPrefix}
      & addSwaggerDescription "Create and send funds parameters."

type CreateAndSendFundsResponsePrefix :: Symbol
type CreateAndSendFundsResponsePrefix = "casfr"

data CreateAndSendFundsResponse = CreateAndSendFundsResponse
  { casfrAddress :: !GYAddressBech32
  , casfrTransaction :: !GYTx
  , casfrTransactionId :: !GYTxId
  , casfrTransactionFee :: !GYNatural
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix CreateAndSendFundsResponsePrefix, CamelToSnake]] CreateAndSendFundsResponse

instance Swagger.ToSchema CreateAndSendFundsResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @CreateAndSendFundsResponsePrefix}
      & addSwaggerDescription "Create and send funds response."

type IsInitializedPrefix :: Symbol
type IsInitializedPrefix = "iip"

newtype IsInitializedParameters = IsInitializedParameters
  { iipEmail :: Email
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix IsInitializedPrefix, CamelToSnake]] IsInitializedParameters

instance Swagger.ToSchema IsInitializedParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @IsInitializedPrefix}
      & addSwaggerDescription "Is initialized parameters."

type IsInitializedResponsePrefix :: Symbol
type IsInitializedResponsePrefix = "iir"

newtype IsInitializedResponse = IsInitializedResponse
  { iirIsInitialized :: Maybe (GYMintingPolicyId, [GYTokenName])
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix IsInitializedResponsePrefix, CamelToSnake]] IsInitializedResponse

instance Swagger.ToSchema IsInitializedResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @IsInitializedResponsePrefix}
      & addSwaggerDescription "Is initialized response."

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
      :> Description "Send funds from a zk based wallet."
      :> "send-funds"
      :> ReqBody '[JSON] SendFundsParameters
      :> Post '[JSON] SendFundsResponse
    :<|> Summary "Create and send funds."
      :> Description "Create (initialize) zk-wallet and send funds from it."
      :> "create-and-send-funds"
      :> ReqBody '[JSON] CreateAndSendFundsParameters
      :> Post '[JSON] CreateAndSendFundsResponse
    :<|> Summary "Know if wallet is initialized."
      :> "is-initialized"
      :> ReqBody '[JSON] IsInitializedParameters
      :> Post '[JSON] IsInitializedResponse

handleWalletApi :: Ctx -> ServerT WalletAPI IO
handleWalletApi ctx =
  handleObtainAddress ctx
    :<|> handleCreateWallet ctx
    :<|> handleSendFunds ctx
    :<|> handleCreateAndSendFunds ctx
    :<|> handleIsInitialized ctx

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
  (zkiws, walletAddress) <- runQuery ctx $ addressFromEmail cwpEmail
  body <-
    ( case cwpFundAddress of
        Just (addressFromBech32 -> cwpFundAddress') ->
          runSkeletonI ctx [cwpFundAddress'] cwpFundAddress' (Just $ ctxCollateral ctx)
        Nothing ->
          runSkeletonWithExtraConfigurationI (extraBuildConfiguration zkiws True) ctx [walletAddress] walletAddress (Just $ ctxCollateral ctx)
      )
      $ createWallet'
        ( ZKCreateWalletInfo
            { zkcwiProofBytes = cwpProofBytes
            , zkcwiPaymentKeyHash = cwpPaymentKeyHash
            , zkcwiJWT = cwpJWT
            , zkcwiEmail = cwpEmail
            }
        )
        zkiws
        walletAddress
  logInfo ctx $ "Tranasction body: " +|| body ||+ ""
  collSignedTx <- handleTxSignCollateral ctx (unsignedTx body)
  pure $
    CreateWalletResponse
      { cwrAddress = addressToBech32 walletAddress
      , cwrTransaction = collSignedTx
      , cwrTransactionId = txBodyTxId body
      , cwrTransactionFee = txBodyFee body & fromIntegral
      }

handleSendFunds :: Ctx -> SendFundsParameters -> IO SendFundsResponse
handleSendFunds ctx@Ctx{..} sfp@SendFundsParameters{..} = do
  logInfo ctx $ "Send funds requested. Parameters: " +|| sfp ||+ ""
  (zkiws, walletAddress) <- runQuery ctx $ addressFromEmail sfpEmail
  logInfo ctx $ "Wallet address: " +|| addressToBech32 walletAddress ||+ ""
  let ec = extraBuildConfiguration zkiws False
  txBody <- runSkeletonWithExtraConfigurationI ec ctx [walletAddress] walletAddress (Just ctxCollateral) $ do
    sendFunds' zkiws walletAddress (ZKSpendWalletInfo{zkswiPaymentKeyHash = sfpPaymentKeyHash, zkswiEmail = sfpEmail}) sfpOuts
  signedTx <- handleTxSignCollateral ctx $ unsignedTx txBody
  pure $
    SendFundsResponse
      { sfrTransaction = signedTx
      , sfrTransactionId = txBodyTxId txBody
      , sfrTransactionFee = fromIntegral $ txBodyFee txBody
      }

handleCreateAndSendFunds :: Ctx -> CreateAndSendFundsParameters -> IO CreateAndSendFundsResponse
handleCreateAndSendFunds ctx@Ctx{..} casfp@CreateAndSendFundsParameters{..} = do
  logInfo ctx $ "Create and send funds requested. Parameters: " +|| casfp ||+ ""
  (zkiws, walletAddress) <- runQuery ctx $ addressFromEmail casfpEmail
  logInfo ctx $ "Wallet address: " +|| addressToBech32 walletAddress ||+ ""
  let ec = extraBuildConfiguration zkiws True
  txBody <- runSkeletonWithExtraConfigurationI ec ctx [walletAddress] walletAddress (Just ctxCollateral) $ do
    sendFundsWithCreation'
      zkiws
      walletAddress
      ( ZKCreateWalletInfo
          { zkcwiPaymentKeyHash = casfpPaymentKeyHash
          , zkcwiProofBytes = casfpProofBytes
          , zkcwiEmail = casfpEmail
          , zkcwiJWT = casfpJWT
          }
      )
      casfpOuts
  signedTx <- handleTxSignCollateral ctx $ unsignedTx txBody
  pure $
    CreateAndSendFundsResponse
      { casfrAddress = addressToBech32 walletAddress
      , casfrTransaction = signedTx
      , casfrTransactionId = txBodyTxId txBody
      , casfrTransactionFee = fromIntegral $ txBodyFee txBody
      }

handleIsInitialized :: Ctx -> IsInitializedParameters -> IO IsInitializedResponse
handleIsInitialized ctx iip@IsInitializedParameters{..} = do
  logInfo ctx $ "Is initialized requested. Parameters: " +|| iip ||+ ""
  (zkiws, walletAddress) <- runQuery ctx $ addressFromEmail iipEmail
  logInfo ctx $ "Wallet address: " +|| addressToBech32 walletAddress ||+ ""
  (mp, authTNs) <- runQuery ctx $ findMintedAuthTokens zkiws walletAddress
  pure $
    IsInitializedResponse
      { iirIsInitialized =
          case authTNs of
            [] -> Nothing
            tns -> Just (mp, tns)
      }

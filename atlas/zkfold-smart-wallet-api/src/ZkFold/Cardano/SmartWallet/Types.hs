module ZkFold.Cardano.SmartWallet.Types (
  ZKWalletBuildInfo (..),
  ZKWalletQueryMonad,
  JWT,
  jwtFromText,
  jwtToText,
  ZKCreateWalletInfo (..),
  ZKInitializedWalletScripts (..),
  ZKEmailError (..),
  ZKJWTError (..),
  ZKWalletException (..),
  Email,
  emailFromText,
  emailToText,
  ByteStringFromHex (..),
  ZKF (..),
  ZKProofBytes (..),
  proofToPlutus,
  BuildOut (..),
  ZKSpendWalletInfo (..),
  ZKBatchWalletInfo (..),
) where

import Control.Exception (Exception)
import Control.Lens ((?~))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (withText)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports (FromJSON (..), ToJSON (..), coerce, (&))
import GeniusYield.Swagger.Utils
import GeniusYield.TxBuilder (GYTxSpecialQueryMonad)
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import Network.HTTP.Types (status400, status500)
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.OnChain.BLS12_381.F (F (..))
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes (..))
import ZkFold.Cardano.SmartWallet.Orphans ()
import ZkFold.Cardano.UPLC.Wallet.Types

-- | Information required to build transactions for a zk-wallet.
data ZKWalletBuildInfo = ZKWalletBuildInfo
  { zkwbiWeb2AuthMintingPolicy :: !(Web2Creds -> GYScript 'PlutusV3)
  -- ^ Web2 Auth minting policy.
  , zkwbiWalletValidator :: !(GYScriptHash -> GYScript 'PlutusV3)
  -- ^ Wallet spending validator.
  , zkwbiCheckSigRewardValidator :: !(GYMintingPolicyId -> GYScript 'PlutusV3)
  -- ^ Reward validator for wallet's spending validator parameterized by web 2 auth minting policy id.
  }

type ZKWalletQueryMonad m = (GYTxSpecialQueryMonad m, MonadReader ZKWalletBuildInfo m)

-- | JSON web token, to be given in textual format.
newtype JWT = JWT Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

instance Swagger.ToSchema JWT where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "JSON web token"

jwtFromText :: Text -> JWT
jwtFromText = coerce

jwtToText :: JWT -> Text
jwtToText = coerce

-- | Information required to initialize user's wallet.
data ZKCreateWalletInfo = ZKCreateWalletInfo
  { zkcwiEmail :: !Email
  , zkcwiJWT :: !JWT
  , zkcwiPaymentKeyHash :: !GYPaymentKeyHash
  , zkcwiProofBytes :: !ZKProofBytes
  }

-- | Fully applied plutus scripts associated with an email.
data ZKInitializedWalletScripts = ZKInitializedWalletScripts
  { zkiwsWeb2Auth :: !(GYScript 'PlutusV3)
  , zkiwsWallet :: !(GYScript 'PlutusV3)
  , zkiwsCheckSig :: !(GYScript 'PlutusV3)
  }

-- | Errors related to invalid email.
data ZKEmailError
  = -- | Provided email is empty.
    ZKEmailEmpty
  deriving stock (Show)

-- | Errors related to invalid JWT.
data ZKJWTError
  = -- | Email was not present as a substring inside JWT.
    ZKJWTEmailNotPresent !JWT !Email
  deriving stock (Show)

data ZKWalletException
  = -- | Could not find stake address information for the given stake address.
    ZKWEStakeAddressInfoNotFound !GYStakeAddress
  | -- | Errors related to invalid email.
    ZKWEEmailError !ZKEmailError
  | -- | Errors related to invalid JWT.
    ZKWEJWTError !ZKJWTError
  | -- | Wallet does not have authentication token.
    ZKWENoAuthToken !Email !GYAddress !GYTokenName
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError ZKWalletException where
  toApiError (ZKWEStakeAddressInfoNotFound sa) =
    GYApiError
      { gaeErrorCode = "STAKE_ADDRESS_INFO_NOT_FOUND"
      , gaeHttpStatus = status500
      , gaeMsg = Text.pack $ "Could not find stake address information for the given stake address: " <> show sa <> "."
      }
  toApiError (ZKWEEmailError emailError) =
    case emailError of
      ZKEmailEmpty ->
        GYApiError
          { gaeErrorCode = "EMAIL_EMPTY"
          , gaeHttpStatus = status400
          , gaeMsg = "Provided email is empty."
          }
  toApiError (ZKWEJWTError jwtError) =
    case jwtError of
      ZKJWTEmailNotPresent jwt email ->
        GYApiError
          { gaeErrorCode = "JWT_EMAIL_NOT_PRESENT"
          , gaeHttpStatus = status400
          , gaeMsg = Text.pack $ "Provided JWT, " <> show jwt <> ", does not contain email, " <> show email <> "."
          }
  toApiError (ZKWENoAuthToken email walletAddr tokenName) =
    GYApiError
      { gaeErrorCode = "AUTH_TOKEN_NOT_FOUND"
      , gaeHttpStatus = status400
      , gaeMsg = Text.pack $ "Could not find authentication token, " <> show tokenName <> " for user's wallet, " <> show walletAddr <> " associated with email, " <> show email <> "."
      }

-- | Email address.
newtype Email = Email Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON)

instance FromJSON Email where
  parseJSON =
    withText "Email" $
      either (fail . show) return . emailFromText

instance Swagger.ToSchema Email where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "User's email address."
      & addSwaggerExample "zkfold@gmail.com"

-- TODO: To validate for emails? Or specifically gmails?

-- | Validates and returns for email.
emailFromText :: Text -> Either String Email
emailFromText = Right . coerce

emailToText :: Email -> Text
emailToText = coerce

-- | Field element.
newtype ZKF = ZKF Integer
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

instance Swagger.ToSchema ZKF where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Field element."

-- | 'ByteString' whose on wire representation is given in hexadecimal encoding.
newtype ByteStringFromHex = ByteStringFromHex ByteString
  deriving stock (Generic)
  deriving newtype (Eq, Ord)

byteStringFromHexToHex :: ByteStringFromHex -> Text
byteStringFromHexToHex = decodeUtf8 . BS16.encode . coerce

instance Show ByteStringFromHex where
  showsPrec d bs =
    showParen (d > 10) $
      showString "ByteStringFromHex "
        . showsPrec 11 (byteStringFromHexToHex bs)

instance FromJSON ByteStringFromHex where
  parseJSON = withText "ByteStringFromHex" $ \t ->
    either (fail . show) (pure . ByteStringFromHex) $ BS16.decode (encodeUtf8 t)

instance ToJSON ByteStringFromHex where
  toJSON = Aeson.String . byteStringFromHexToHex

instance Swagger.ToSchema ByteStringFromHex where
  declareNamedSchema _ =
    pure $
      Swagger.named "ByteStringFromHex" $
        mempty & Swagger.type_
          ?~ Swagger.SwaggerString & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ "Bytes encoded in hex."

-- | ZK proof bytes, assuming hex encoding for relevant bytes.
data ZKProofBytes = ZKProofBytes
  { cmA_bytes :: !ByteStringFromHex
  , cmB_bytes :: !ByteStringFromHex
  , cmC_bytes :: !ByteStringFromHex
  , cmF_bytes :: !ByteStringFromHex
  , cmH1_bytes :: !ByteStringFromHex
  , cmH2_bytes :: !ByteStringFromHex
  , cmZ1_bytes :: !ByteStringFromHex
  , cmZ2_bytes :: !ByteStringFromHex
  , cmQlow_bytes :: !ByteStringFromHex
  , cmQmid_bytes :: !ByteStringFromHex
  , cmQhigh_bytes :: !ByteStringFromHex
  , proof1_bytes :: !ByteStringFromHex
  , proof2_bytes :: !ByteStringFromHex
  , a_xi_int :: !Integer
  , b_xi_int :: !Integer
  , c_xi_int :: !Integer
  , s1_xi_int :: !Integer
  , s2_xi_int :: !Integer
  , f_xi_int :: !Integer
  , t_xi_int :: !Integer
  , t_xi'_int :: !Integer
  , z1_xi'_int :: !Integer
  , z2_xi'_int :: !Integer
  , h1_xi'_int :: !Integer
  , h2_xi_int :: !Integer
  , l1_xi :: !ZKF
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Swagger.ToSchema ZKProofBytes where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Proof bytes where bytes are represented in hexadecimal encoding."

proofToPlutus :: ZKProofBytes -> ProofBytes
proofToPlutus ZKProofBytes{..} =
  ProofBytes
    { cmA_bytes = bsFromHexToPlutus cmA_bytes
    , cmB_bytes = bsFromHexToPlutus cmB_bytes
    , cmC_bytes = bsFromHexToPlutus cmC_bytes
    , cmF_bytes = bsFromHexToPlutus cmF_bytes
    , cmH1_bytes = bsFromHexToPlutus cmH1_bytes
    , cmH2_bytes = bsFromHexToPlutus cmH2_bytes
    , cmZ1_bytes = bsFromHexToPlutus cmZ1_bytes
    , cmZ2_bytes = bsFromHexToPlutus cmZ2_bytes
    , cmQlow_bytes = bsFromHexToPlutus cmQlow_bytes
    , cmQmid_bytes = bsFromHexToPlutus cmQmid_bytes
    , cmQhigh_bytes = bsFromHexToPlutus cmQhigh_bytes
    , proof1_bytes = bsFromHexToPlutus proof1_bytes
    , proof2_bytes = bsFromHexToPlutus proof2_bytes
    , a_xi_int = a_xi_int
    , b_xi_int = b_xi_int
    , c_xi_int = c_xi_int
    , s1_xi_int = s1_xi_int
    , s2_xi_int = s2_xi_int
    , f_xi_int = f_xi_int
    , t_xi_int = t_xi_int
    , t_xi'_int = t_xi'_int
    , z1_xi'_int = z1_xi'_int
    , z2_xi'_int = z2_xi'_int
    , h1_xi'_int = h1_xi'_int
    , h2_xi_int = h2_xi_int
    , l_xi = [coerce l1_xi]
    }
 where
  bsFromHexToPlutus :: ByteStringFromHex -> PlutusTx.BuiltinByteString
  bsFromHexToPlutus (ByteStringFromHex bs) = PlutusTx.toBuiltin bs

type BuildOutPrefix :: Symbol
type BuildOutPrefix = "bo"

-- | An output to be created. Note that our balancer may add additional lovelace to satisfy minimum lovelace requirement for this output.
data BuildOut = BuildOut
  { boAddress :: GYAddress
  , boValue :: GYValue
  , boDatum :: Maybe (GYDatum, Bool)
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BuildOutPrefix, CamelToSnake]] BuildOut

instance Swagger.ToSchema BuildOut where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @BuildOutPrefix}
      & addSwaggerDescription "Output to be created. Note that our balancer may add additional lovelace to satisfy minimum lovelace requirement for this output."

-- | Information required to spend funds from user's wallet.
data ZKSpendWalletInfo = ZKSpendWalletInfo
  { zkswiEmail :: !Email
  , zkswiPaymentKeyHash :: !GYPaymentKeyHash
  }

type ZKBatchWalletInfoPrefix :: Symbol
type ZKBatchWalletInfoPrefix = "zkbwi"

-- | Information required to batch a particular transaction.
data ZKBatchWalletInfo = ZKBatchWalletInfo
  { zkbwiEmail :: !Email
  , zkbwiTx :: !GYTx
  , zkbwiPaymentKeyHash :: !GYPaymentKeyHash
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix ZKBatchWalletInfoPrefix, CamelToSnake]] ZKBatchWalletInfo

instance Swagger.ToSchema ZKBatchWalletInfo where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @ZKBatchWalletInfoPrefix}
      & addSwaggerDescription "Information required to batch a particular transaction."
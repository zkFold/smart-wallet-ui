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
  ZKSetupBytes (..),
  ZKProofBytes (..),
  setupToPlutus,
  proofToPlutus,
  mkSetup,
  mkProof,
  ExpModProofInput (..),
  expModSetupMock,
  expModProofMock,
  BuildOut (..),
  ZKSpendWalletInfo (..),
  ZKBatchWalletInfo (..),
  ZKRegisterAndDelegateWithdrawalScriptInfo (..),
) where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (withText)
import Data.ByteString.Base16 qualified as BS16
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Text qualified as Text
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports (FromJSON (..), coerce, (&))
import GeniusYield.Swagger.Utils
import GeniusYield.TxBuilder (GYTxSpecialQueryMonad)
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import Network.HTTP.Types (status400, status500)
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.OnChain.BLS12_381.F (F (..))
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes (..), SetupBytes (..))
import ZkFold.Cardano.SmartWallet.Orphans ()
import ZkFold.Cardano.UPLC.Wallet.Types
import ZkFold.Symbolic.Cardano.Contracts.SmartWallet (
  ByteStringFromHex (..),
  ExpModProofInput (..),
  ZKF (..),
  ZKProofBytes (..),
  ZKSetupBytes (..),
  expModProofMock,
  expModSetupMock,
  mkProof,
  mkSetup,
 )

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
  deriving stock (Show)

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
  bsFromHexToPlutus (ByteStringFromHex bs) =
    case BS16.decode bs of
      Right b -> PlutusTx.toBuiltin b
      Left e -> error e

setupToPlutus :: ZKSetupBytes -> SetupBytes
setupToPlutus ZKSetupBytes{..} =
  SetupBytes
    { n = n
    , pow = pow
    , omega = coerce omega_int
    , k1 = coerce k1_int
    , k2 = coerce k2_int
    , h1_bytes = PlutusTx.toBuiltin h1_bytes
    , cmQm_bytes = PlutusTx.toBuiltin cmQm_bytes
    , cmQl_bytes = PlutusTx.toBuiltin cmQl_bytes
    , cmQr_bytes = PlutusTx.toBuiltin cmQr_bytes
    , cmQo_bytes = PlutusTx.toBuiltin cmQo_bytes
    , cmQc_bytes = PlutusTx.toBuiltin cmQc_bytes
    , cmQk_bytes = PlutusTx.toBuiltin cmQk_bytes
    , cmS1_bytes = PlutusTx.toBuiltin cmS1_bytes
    , cmS2_bytes = PlutusTx.toBuiltin cmS2_bytes
    , cmS3_bytes = PlutusTx.toBuiltin cmS3_bytes
    , cmT1_bytes = PlutusTx.toBuiltin cmT1_bytes
    }

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

-- | Information required to register and delegate the withdrawal script.
data ZKRegisterAndDelegateWithdrawalScriptInfo = ZKRegisterAndDelegateWithdrawalScriptInfo
  { zkradiStakePool :: !GYStakePoolId
  , zkradiDRep :: !GYDRep
  , zkradiEmail :: !Email
  , zkradiPaymentKeyHash :: !GYPaymentKeyHash
  }
  deriving stock (Show, Generic)

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

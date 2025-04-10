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
  ZKF (..),
  ZKProofBytes (..),
  zkProofToPlutus,
) where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (withText)
import Data.ByteString.Base16 qualified as BS16
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports (FromJSON (..), ToJSON, coerce, encodeUtf8, (&), (<&>))
import GeniusYield.Swagger.Utils
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.Types (GYMintingPolicyId, GYPaymentKeyHash, GYPubKeyHash, GYScript, GYScriptHash, GYStakeAddress, PlutusVersion (..))
import GeniusYield.Types.OpenApi ()
import Network.HTTP.Types (status400, status500)
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.OnChain.BLS12_381.F (F (..))
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes (..))
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

type ZKWalletQueryMonad m = (GYTxQueryMonad m, MonadReader ZKWalletBuildInfo m)

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

-- | ZK proof bytes, assuming hex encoding for relevant bytes.
data ZKProofBytes = ZKProofBytes
  { cmA_bytes_hex :: !Text
  , cmB_bytes_hex :: !Text
  , cmC_bytes_hex :: !Text
  , cmF_bytes_hex :: !Text
  , cmH1_bytes_hex :: !Text
  , cmH2_bytes_hex :: !Text
  , cmZ1_bytes_hex :: !Text
  , cmZ2_bytes_hex :: !Text
  , cmQlow_bytes_hex :: !Text
  , cmQmid_bytes_hex :: !Text
  , cmQhigh_bytes_hex :: !Text
  , proof1_bytes_hex :: !Text
  , proof2_bytes_hex :: !Text
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

zkProofToPlutus :: ZKProofBytes -> Either String ProofBytes
zkProofToPlutus ZKProofBytes{..} = do
  cmA <- hexToBytes cmA_bytes_hex
  cmB <- hexToBytes cmB_bytes_hex
  cmC <- hexToBytes cmC_bytes_hex
  cmF <- hexToBytes cmF_bytes_hex
  cmH1 <- hexToBytes cmH1_bytes_hex
  cmH2 <- hexToBytes cmH2_bytes_hex
  cmZ1 <- hexToBytes cmZ1_bytes_hex
  cmZ2 <- hexToBytes cmZ2_bytes_hex
  cmQlow <- hexToBytes cmQlow_bytes_hex
  cmQmid <- hexToBytes cmQmid_bytes_hex
  cmQhigh <- hexToBytes cmQhigh_bytes_hex
  proof1 <- hexToBytes proof1_bytes_hex
  proof2 <- hexToBytes proof2_bytes_hex

  pure $
    ProofBytes
      { cmA_bytes = cmA
      , cmB_bytes = cmB
      , cmC_bytes = cmC
      , cmF_bytes = cmF
      , cmH1_bytes = cmH1
      , cmH2_bytes = cmH2
      , cmZ1_bytes = cmZ1
      , cmZ2_bytes = cmZ2
      , cmQlow_bytes = cmQlow
      , cmQmid_bytes = cmQmid
      , cmQhigh_bytes = cmQhigh
      , proof1_bytes = proof1
      , proof2_bytes = proof2
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
      , l1_xi = coerce l1_xi
      }
 where
  hexToBytes :: Text -> Either String PlutusTx.BuiltinByteString
  hexToBytes t = BS16.decode (encodeUtf8 t) <&> PlutusTx.toBuiltin
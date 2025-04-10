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
) where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Imports (FromJSON, ToJSON, coerce)
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.Types (GYMintingPolicyId, GYPaymentKeyHash, GYPubKeyHash, GYScript, GYScriptHash, GYStakeAddress, PlutusVersion (..))
import Network.HTTP.Types (status400, status500)
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)
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

jwtFromText :: Text -> JWT
jwtFromText = coerce

jwtToText :: JWT -> Text
jwtToText = coerce

-- | Information required to initialize user's wallet.
data ZKCreateWalletInfo = ZKCreateWalletInfo
  { zkcwiEmail :: !Email
  , zkcwiJWT :: !JWT
  , zkcwiPaymentKeyHash :: !GYPaymentKeyHash
  , zkcwiProofBytes :: !ProofBytes
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
  deriving newtype (FromJSON, ToJSON)

-- TODO: To validate for emails? Or specifically gmails?
emailFromText :: Text -> Email
emailFromText = coerce

emailToText :: Email -> Text
emailToText = coerce
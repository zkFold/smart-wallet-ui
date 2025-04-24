module ZkFold.Cardano.SmartWallet.Types.Errors (
  ZKEmailError (..),
  ZKJWTError (..),
  ZKWalletException (..),
) where

import Control.Exception (Exception)
import Data.Text qualified as Text
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Types
import Network.HTTP.Types (status400, status500)
import ZkFold.Cardano.SmartWallet.Types.Common (Email, JWT)

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
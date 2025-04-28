module ZkFold.Cardano.SmartWallet.Types.Common (
  JWT,
  jwtFromText,
  jwtToText,
  Email,
  emailFromText,
  emailToText,
) where

import Data.Aeson (withText)
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Deriving.Aeson
import GeniusYield.Imports (FromJSON (..), coerce, (&))
import GeniusYield.Swagger.Utils

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
module ZkFold.Cardano.SmartWallet.Types.Send (
  BuildOut (..),
  ZKSpendWalletInfo (..),
) where

import Data.Swagger qualified as Swagger
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&))
import GeniusYield.Swagger.Utils
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Orphans ()
import ZkFold.Cardano.SmartWallet.Types.Common

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
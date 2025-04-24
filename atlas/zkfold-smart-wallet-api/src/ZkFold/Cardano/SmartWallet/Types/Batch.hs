module ZkFold.Cardano.SmartWallet.Types.Batch (
  ZKBatchWalletInfo (..),
) where

import Data.Swagger qualified as Swagger
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&))
import GeniusYield.Swagger.Utils
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Orphans ()
import ZkFold.Cardano.SmartWallet.Types.Common

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

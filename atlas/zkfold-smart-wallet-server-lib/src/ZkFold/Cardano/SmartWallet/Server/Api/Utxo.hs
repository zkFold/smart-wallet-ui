module ZkFold.Cardano.SmartWallet.Server.Api.Utxo (
  UtxoAPI,
  handleUtxoApi,
) where

import Data.Function ((&))
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Servant
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Utils

type UtxoResponsePrefix :: Symbol
type UtxoResponsePrefix = "ur"

data UtxoResponse = UtxoResponse
  { urRef :: !GYTxOutRef
  , urAddress :: !GYAddressBech32
  , urValue :: !GYValue
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix UtxoResponsePrefix, CamelToSnake]] UtxoResponse

instance Swagger.ToSchema UtxoResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @UtxoResponsePrefix}
      & addSwaggerDescription "UTXO information."

type UtxoAPI =
  "addresses"
    :> Summary "Get utxos belonging to a list of addresses"
    :> ReqBody '[JSON] [GYAddressBech32]
    :> Post '[JSON] [UtxoResponse]

handleUtxoApi :: Ctx -> ServerT UtxoAPI IO
handleUtxoApi = handleUtxoAddresses

handleUtxoAddresses :: Ctx -> [GYAddressBech32] -> IO [UtxoResponse]
handleUtxoAddresses ctx addresses = do
  logInfo ctx $ "Getting utxos for addresses: " +|| addresses ||+ ""
  utxos <- runQuery ctx $ utxosAtAddresses $ map addressFromBech32 addresses
  pure
    $ map
      ( \GYUTxO{..} ->
          UtxoResponse
            { urRef = utxoRef
            , urAddress = addressToBech32 utxoAddress
            , urValue = utxoValue
            }
      )
    $ utxosToList utxos

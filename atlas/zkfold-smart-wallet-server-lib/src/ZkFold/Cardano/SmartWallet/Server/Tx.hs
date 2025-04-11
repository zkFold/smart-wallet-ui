module ZkFold.Cardano.SmartWallet.Server.Tx (
  TxAPI,
  handleTxApi,
  handleTxSignCollateral,
  handleTxSignCollateralAndSubmit,
  handleTxSubmit,
) where

import Data.Function ((&))
import Data.Swagger qualified as Swagger
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.Types
import Servant
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Utils

type AddWitAndSubmitPrefix :: Symbol
type AddWitAndSubmitPrefix = "awasp"

data AddWitAndSubmitParameters = AddWitAndSubmitParameters
  { awaspOriginalUnsignedTx :: !GYTx
  , awaspWalletWitness :: !GYTxWitness
  }
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix AddWitAndSubmitPrefix, CamelToSnake]] AddWitAndSubmitParameters

instance Swagger.ToSchema AddWitAndSubmitParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @AddWitAndSubmitPrefix}
      & addSwaggerDescription "Add witness to a transaction, and submit it."

type TxAPI =
  "submit"
    :> Summary "Submit a transaction"
    :> Description "Submits the given transaction to the network."
    :> ReqBody '[JSON] GYTx
    :> Post '[JSON] GYTxId
    :<|> "add-wit-and-submit"
      :> Summary "Add a witness to the transaction and submit it."
      :> ReqBody '[JSON] AddWitAndSubmitParameters
      :> Post '[JSON] GYTxId

handleTxApi :: Ctx -> ServerT TxAPI IO
handleTxApi ctx =
  handleTxSubmit ctx
    :<|> handleTxAddWitAndSubmit ctx

handleTxSignCollateral :: Ctx -> GYTx -> IO GYTx
handleTxSignCollateral ctx@Ctx{..} tx = do
  logInfo ctx $ "Signing transaction for collateral: " +| txToHex tx |+ ""
  pure $ signGYTx' tx [somePaymentSigningKeyToSomeSigningKey $ fst ctxCollateralKey]

handleTxSignCollateralAndSubmit :: Ctx -> GYTx -> IO GYTxId
handleTxSignCollateralAndSubmit ctx tx = do
  logInfo ctx $ "Signing and submitting transaction: " +| txToHex tx |+ ""
  signedTx <- handleTxSignCollateral ctx tx
  handleTxSubmit ctx signedTx

handleTxSubmit :: Ctx -> GYTx -> IO GYTxId
handleTxSubmit ctx@Ctx{..} tx = do
  logInfo ctx $ "Submitting transaction: " +| txToHex tx |+ ""
  gySubmitTx ctxProviders tx

handleTxAddWitAndSubmit :: Ctx -> AddWitAndSubmitParameters -> IO GYTxId
handleTxAddWitAndSubmit ctx AddWitAndSubmitParameters{..} = do
  logInfo ctx $ "AddWitAndSubmitParameters, tx: " +| txToHex awaspOriginalUnsignedTx |+ ", witness: " +|| awaspWalletWitness ||+ ""
  handleTxSubmit ctx $ appendWitnessGYTx awaspWalletWitness awaspOriginalUnsignedTx
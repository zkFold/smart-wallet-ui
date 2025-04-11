module ZkFold.Cardano.SmartWallet.Server.Tx (
  TxAPI,
  handleTxApi,
  handleTxSignCollateral,
  handleTxSignCollateralAndSubmit,
  handleTxSubmit,
) where

import Fmt
import GeniusYield.Types
import Servant
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Utils

type TxAPI =
  "submit"
    :> Summary "Submit a transaction"
    :> Description "Submits the given transaction to the network."
    :> ReqBody '[JSON] GYTx
    :> Post '[JSON] GYTxId

handleTxApi :: Ctx -> ServerT TxAPI IO
handleTxApi = handleTxSubmit

handleTxSignCollateral :: Ctx -> GYTx -> IO GYTx
handleTxSignCollateral ctx@Ctx{..} tx = do
  logInfo ctx $ "Signing transaction: " +| txToHex tx |+ ""
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

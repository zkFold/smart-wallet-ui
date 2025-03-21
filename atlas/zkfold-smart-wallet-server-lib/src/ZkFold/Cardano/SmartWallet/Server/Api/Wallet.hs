module ZkFold.Cardano.SmartWallet.Server.Api.Wallet (
  WalletAPI,
  handleWalletApi,
) where

import Cardano.Api qualified as Api
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Deriving.Aeson
import Fmt
import GHC.TypeLits (Symbol)
import GeniusYield.Imports ((&))
import GeniusYield.Transaction.Common (GYTxExtraConfiguration (..), GYTxInDetailed (..))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import Servant
import ZkFold.Cardano.SmartWallet.Api
import ZkFold.Cardano.SmartWallet.Server.Ctx
import ZkFold.Cardano.SmartWallet.Server.Orphans ()
import ZkFold.Cardano.SmartWallet.Server.Tx (handleTxSignCollateral, handleTxSubmit)
import ZkFold.Cardano.SmartWallet.Server.Utils

type SendFundsPrefix :: Symbol
type SendFundsPrefix = "sfp"

data SendFundsParameters = SendFundsParameters
  { sfpValue :: !GYValue
  , sfpEmail :: !Text
  , sfpSendAddress :: !GYAddressBech32
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SendFundsPrefix, CamelToSnake]] SendFundsParameters

instance Swagger.ToSchema SendFundsParameters where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SendFundsPrefix}
      & addSwaggerDescription "Send funds parameters."

type SendFundsResponsePrefix :: Symbol
type SendFundsResponsePrefix = "sfr"

data SendFundsResponse = SendFundsResponse
  { sfrTransaction :: !GYTx
  , sfrTransactionId :: !GYTxId
  , sfrTransactionFee :: !GYNatural
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SendFundsResponsePrefix, CamelToSnake]] SendFundsResponse

instance Swagger.ToSchema SendFundsResponse where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions{Swagger.fieldLabelModifier = dropSymbolAndCamelToSnake @SendFundsResponsePrefix}
      & addSwaggerDescription "Send funds response."

type WalletAPI =
  Summary "Send funds."
    :> Description "Send funds from a zk based wallet to a given address."
    :> "send-funds"
    :> ReqBody '[JSON] SendFundsParameters
    :> Post '[JSON] SendFundsResponse

handleWalletApi :: Ctx -> ServerT WalletAPI IO
handleWalletApi ctx = handleSendFunds ctx

handleSendFunds :: Ctx -> SendFundsParameters -> IO SendFundsResponse
handleSendFunds ctx@Ctx{..} sfp@SendFundsParameters{..} = do
  logInfo ctx $ "Send funds requested. Parameters: " +|| sfp ||+ ""
  validatorSetup <- runQuery ctx $ validatorSetupFromEmail sfpEmail
  senderWalletAddress <- runQuery ctx $ addressFromValidatorSetup validatorSetup
  logInfo ctx $ "Sender wallet address: " +|| senderWalletAddress ||+ ""
  let ec =
        GYTxExtraConfiguration
          { gytxecUtxoInputMapper = \GYUTxO{..} ->
              GYTxInDetailed
                { gyTxInDet = GYTxIn utxoRef undefined -- FIXME: Give script witness.
                , gyTxInDetAddress = undefined -- FIXME: Change address to fake script that allows forged proofs.
                , gyTxInDetValue = utxoValue
                , gyTxInDetDatum = utxoOutDatum
                , gyTxInDetScriptRef = utxoRefScript
                }
          , -- FIXME: Provide pre & post content mappers.
            gytxecPreBodyContentMapper = \body ->
              -- When balancing, @makeTransactionBodyAutoBalance@ function of @cardano-api@ that is used internally inside Atlas, adds following output before computing execution units, thus we need to do same here to make sure that script execution doesn't fail.
              let bodyWithExtraOut =
                    body
                      { Api.txOuts = Api.txOuts body <> [txOutToApi (GYTxOut senderWalletAddress (valueFromLovelace $ 2 ^ (64 :: Integer) - 1) Nothing Nothing)]
                      }
               in bodyWithExtraOut
                    { Api.txWithdrawals =
                        Api.TxWithdrawals Api.ShelleyBasedEraConway $
                          [ txWdrlToApi $
                              GYTxWdrl
                                { gyTxWdrlStakeAddress = undefined
                                , gyTxWdrlAmount = undefined
                                , gyTxWdrlWitness = GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptInlined @'PlutusV3 @'PlutusV3 undefined) undefined
                                }
                          ]
                    }
          , gytxecPostBodyContentMapper = \body ->
              -- Correct address of inputs.
              -- Correct withdrawal address.
              undefined
          }
  txBody <- runSkeletonWithExtraConfigurationI ec ctx [senderWalletAddress] senderWalletAddress (Just ctxCollateral) $ do
    sendFunds' validatorSetup (addressFromBech32 sfpSendAddress) sfpValue
  signedTx <- handleTxSignCollateral ctx $ unsignedTx txBody
  tid <- handleTxSubmit ctx signedTx
  pure $
    SendFundsResponse
      { sfrTransaction = signedTx
      , sfrTransactionId = tid
      , sfrTransactionFee = fromIntegral $ txBodyFee txBody
      }
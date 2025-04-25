module ZkFold.Cardano.SmartWallet.Test.Batch (smartWalletBatchTests) where

import Control.Monad (forM)
import Data.List.NonEmpty qualified as NE
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Cardano.SmartWallet.Api
import ZkFold.Cardano.SmartWallet.Test.Single (smartWalletSingleRun)
import ZkFold.Cardano.SmartWallet.Test.Utils
import ZkFold.Cardano.SmartWallet.Types

smartWalletBatchTests :: Setup -> TestTree
smartWalletBatchTests setup =
  testGroup
    "smart-wallet-batch-tests"
    [ testCaseSteps "able to batch funds expenditure transaction from multiple wallets" $ \info -> withSetup info setup $ \ctx -> do
        let emails = ["zkfold1@gmail.com", "zkfold2@gmail.com"]
            fundUser = ctxUserF ctx
        batchWalletInfos <- forM emails $ \emailT -> do
          (email, _fundUser, spendWalletBody, (newKey, newKeyHash)) <- smartWalletSingleRun ctx info emailT
          pure
            ( ZKBatchWalletInfo
                { zkbwiTx = unsignedTx spendWalletBody
                , zkbwiPaymentKeyHash = newKeyHash
                , zkbwiEmail = email
                }
            , newKey
            )
        (getTxBody -> batchTxBody) <- zkctxRunQuery ctx $ batchTxs $ NE.fromList $ map fst batchWalletInfos
        info $ "Batch tx body: " <> show batchTxBody
        tidBatchTx <- ctxRun ctx fundUser $ submitTxBodyConfirmed batchTxBody $ GYSomeSigningKey (fundUser & userPaymentSKey) : map (GYSomeSigningKey . snd) batchWalletInfos
        info $ "Submitted batch tx: " <> show tidBatchTx
    ]

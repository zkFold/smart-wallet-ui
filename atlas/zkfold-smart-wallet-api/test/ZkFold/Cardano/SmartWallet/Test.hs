module ZkFold.Cardano.SmartWallet.Test (smartWalletTests) where

import GeniusYield.Test.Privnet.Setup
import Test.Tasty (TestTree, testGroup)
import ZkFold.Cardano.SmartWallet.Test.Batch (smartWalletBatchTests)
import ZkFold.Cardano.SmartWallet.Test.Single (smartWalletSingleTests)

smartWalletTests :: Setup -> TestTree
smartWalletTests setup =
  testGroup
    "privnet-smart-wallet-tests"
    [ smartWalletSingleTests setup
    , smartWalletBatchTests setup
    ]

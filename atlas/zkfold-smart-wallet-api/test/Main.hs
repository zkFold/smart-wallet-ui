module Main (main) where

import GeniusYield.Test.Privnet.Setup
import Test.Tasty (defaultMain, testGroup)
import ZkFold.Cardano.SmartWallet.Test (smartWalletTests)

main :: IO ()
main = do
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
    defaultMain $
      testGroup
        "zkfold-wallet-tests"
        [ smartWalletTests setup
        ]

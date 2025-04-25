module ZkFold.Cardano.SmartWallet.Test.Single (
  smartWalletSingleRun,
  smartWalletSingleTests,
) where

import Data.Text (Text)
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Cardano.SmartWallet.Api (addressFromEmail, createWallet', sendFunds')
import ZkFold.Cardano.SmartWallet.Constants (extraBuildConfiguration)
import ZkFold.Cardano.SmartWallet.Test.Utils
import ZkFold.Cardano.SmartWallet.Types

smartWalletSingleRun :: Ctx -> (String -> IO ()) -> Text -> IO (Email, User, GYTxBody, (GYSigningKey GYKeyRolePayment, GYKeyHash GYKeyRolePayment))
smartWalletSingleRun ctx info emailT = do
  -- Obtain address of wallet.
  let email = emailFromText emailT & either error id
      fundUser = ctxUserF ctx
      fundUserAddr = fundUser & userChangeAddress
  (zkiws, walletAddress) <- zkctxRunQuery ctx $ addressFromEmail email
  info $ "Wallet's address: " <> show walletAddress
  info $ "Initialized wallet scripts: " <> show zkiws
  -- Fund this address.
  ctxRun ctx fundUser $ do
    txBodyFund <- buildTxBody $ mustHaveOutput $ mkGYTxOutNoDatum walletAddress (valueFromLovelace 100_000_000)
    signAndSubmitConfirmed_ txBodyFund
  info "Funded wallet"

  -- Initialize this wallet.
  -- Generate signing key.
  newKey :: GYSigningKey 'GYKeyRolePayment <- generateSigningKey
  let newKeyHash = newKey & getVerificationKey & verificationKeyHash
  info $ "Generated key: " <> show newKeyHash
  let jwt = "{testHeader:\"testData\"},{testPayload:\"payloadData\",email:\"" <> emailToText email <> "\"}"
      proofBytes = proofBytesFromJwt jwt newKeyHash
      cwi =
        ZKCreateWalletInfo
          { zkcwiProofBytes = proofBytes
          , zkcwiPaymentKeyHash = newKeyHash
          , zkcwiJWT = jwtFromText jwt
          , zkcwiEmail = email
          }
  info $ "proof bytes: " <> show proofBytes
  -- Find suitable UTxO as collateral.
  utxos <- ctxRunQuery ctx $ utxosAtAddress fundUserAddr Nothing
  let collUtxo = utxosToList utxos & head
  initWalletBody <- zkctxRunBuilder ctx walletAddress (utxoRef collUtxo) $ do
    createWallet' cwi zkiws walletAddress >>= buildTxBodyWithExtraConfiguration (extraBuildConfiguration zkiws True)
  info $ "Wallet initialization tx body: " <> show initWalletBody
  -- We require signature from 'fundUser' since we used it's collateral.
  tidInit <- ctxRun ctx fundUser $ submitTxBodyConfirmed initWalletBody [GYSomeSigningKey newKey, GYSomeSigningKey (fundUser & userPaymentSKey)]
  info $ "Submitted tx: " <> show tidInit

  -- Spending funds from the zk based smart wallet by exercising stake script.
  walletUtxos <- ctxRunQuery ctx $ utxosAtAddress walletAddress Nothing
  info $ "Wallet UTxOs: " <> show walletUtxos
  let outs =
        [ BuildOut
            { boValue = valueFromLovelace 10_000_000
            , boDatum = Nothing
            , boAddress = ctxUserF ctx & userChangeAddress & addressToBech32
            }
        , BuildOut
            { boValue = valueFromLovelace 20_000_000
            , boDatum = Nothing
            , boAddress = ctxUserF ctx & userChangeAddress & addressToBech32
            }
        ]
  spendWalletBody <- zkctxRunBuilder ctx walletAddress (utxoRef collUtxo) $ do
    sendFunds' zkiws walletAddress (ZKSpendWalletInfo{zkswiPaymentKeyHash = newKeyHash, zkswiEmail = email}) outs >>= buildTxBodyWithExtraConfiguration (extraBuildConfiguration zkiws False)
  info $ "send funds tx body: " <> show spendWalletBody
  pure (email, fundUser, spendWalletBody, (newKey, newKeyHash))

smartWalletSingleTests :: Setup -> TestTree
smartWalletSingleTests setup =
  testGroup
    "smart-wallet-single-tests"
    [ testCaseSteps "able to initialize and send funds from a zk based smart wallet" $ \info -> withSetup info setup $ \ctx -> do
        (_email, fundUser, spendWalletBody, (newKey, _newKeyHash)) <- smartWalletSingleRun ctx info "zkfold@gmail.com"
        tidSpend <- ctxRun ctx fundUser $ submitTxBodyConfirmed spendWalletBody [GYSomeSigningKey newKey, GYSomeSigningKey (fundUser & userPaymentSKey)]
        info $ "Submitted spend tx: " <> show tidSpend
    ]

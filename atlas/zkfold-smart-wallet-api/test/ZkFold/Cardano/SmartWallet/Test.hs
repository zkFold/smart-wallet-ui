module ZkFold.Cardano.SmartWallet.Test (smartWalletTests) where

import Codec.Crypto.RSA (generateKeyPair)
import Codec.Crypto.RSA qualified as R
import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString, unpack)
import Data.Text.Encoding (encodeUtf8)
import GeniusYield.Imports (Text, (&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import System.Random (mkStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Algebra.Class (zero)
import ZkFold.Cardano.SmartWallet.Api (addressFromEmail, createWallet', extraBuildConfiguration, sendFunds')
import ZkFold.Cardano.SmartWallet.Test.Utils
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))

proofBytesFromJwt :: Text -> ByteString -> ZKProofBytes
proofBytesFromJwt jwt keyHash = mkProof $ expModProofMock @ByteString zero (PlonkupProverSecret $ pure zero) (ExpModProofInput e n msg keyNat)
 where
  (R.PublicKey{..}, R.PrivateKey{..}, _) = generateKeyPair (mkStdGen 42) 2048

  h = hash (encodeUtf8 jwt)

  bsToNat :: ByteString -> Natural
  bsToNat = foldr (\w a -> fromIntegral w + 2 * a) 0 . unpack

  hNat :: Natural
  hNat = bsToNat h

  prD, e, n :: Natural
  prD = fromIntegral private_d
  e = fromIntegral public_e
  n = fromIntegral public_n

  msg = (hNat ^ prD) `mod` n

  keyNat :: Natural
  keyNat = bsToNat keyHash

smartWalletTests :: Setup -> TestTree
smartWalletTests setup =
  testGroup
    "privnet-smart-wallet-tests"
    [ testCaseSteps "able to initialize and send funds from a zk based smart wallet" $ \info -> withSetup info setup $ \ctx -> do
        -- Obtain address of wallet.
        let email = emailFromText "zkfold@gmail.com" & either error id
        (zkiws, walletAddress) <- zkctxRunQuery ctx $ addressFromEmail email
        info $ "Wallet's address: " <> show walletAddress
        -- Fund this address.
        ctxRun ctx (ctxUserF ctx) $ do
          txBodyFund <- buildTxBody $ mustHaveOutput $ mkGYTxOutNoDatum walletAddress (valueFromLovelace 100_000_000)
          signAndSubmitConfirmed_ txBodyFund
        -- Initialize this wallet.
        -- Generate signing key.
        newKey :: GYSigningKey 'GYKeyRolePayment <- generateSigningKey
        let newKeyHash = newKey & getVerificationKey & verificationKeyHash
        info $ "Generated key: " <> show newKeyHash
        let jwt = "{testHeader:\"testData\"},{testPayload:\"payloadData\",email:\"" <> emailToText email <> "\"}"
            proofBytes = proofBytesFromJwt jwt $ keyHashToRawBytes newKeyHash
            cwi =
              ZKCreateWalletInfo
                { zkcwiProofBytes = proofBytes
                , zkcwiPaymentKeyHash = newKeyHash
                , zkcwiJWT = jwtFromText jwt
                , zkcwiEmail = email
                }
        -- Find suitable UTxO as collateral.
        utxos <- ctxRunQuery ctx $ utxosAtAddress (ctxUserF ctx & userChangeAddress) Nothing
        let collUtxo = utxosToList utxos & head
        initWalletBody <- zkctxRunBuilder ctx walletAddress (utxoRef collUtxo) $ do
          createWallet' cwi zkiws >>= buildTxBodyWithExtraConfiguration (extraBuildConfiguration zkiws)
        info $ "Wallet initialization tx body: " <> show initWalletBody
        -- We require signature from 'ctxUserF' since we used it's collateral.
        tidInit <- ctxRun ctx (ctxUserF ctx) $ signAndSubmitConfirmed initWalletBody
        info $ "Submitted tx: " <> show tidInit
        walletUtxos <- ctxRunQuery ctx $ utxosAtAddress walletAddress Nothing
        info $ "Wallet UTxOs: " <> show walletUtxos
        let outs =
              [ BuildOut
                  { boValue = valueFromLovelace 10_000_000
                  , boDatum = Nothing
                  , boAddress = ctxUserF ctx & userChangeAddress
                  }
              , BuildOut
                  { boValue = valueFromLovelace 20_000_000
                  , boDatum = Nothing
                  , boAddress = ctxUserF ctx & userChangeAddress
                  }
              ]
        spendWalletBody <- zkctxRunBuilder ctx walletAddress (utxoRef collUtxo) $ do
          sendFunds' zkiws walletAddress (ZKSpendWalletInfo{zkswiPaymentKeyHash = newKeyHash, zkswiEmail = email}) outs >>= buildTxBodyWithExtraConfiguration (extraBuildConfiguration zkiws)
        info $ "send funds tx body: " <> show spendWalletBody
        tidSpend <- ctxRun ctx (ctxUserF ctx) $ submitTxBodyConfirmed spendWalletBody [GYSomeSigningKey newKey, GYSomeSigningKey (ctxUserF ctx & userPaymentSKey)]
        info $ "Submitted tx: " <> show tidSpend
    ]

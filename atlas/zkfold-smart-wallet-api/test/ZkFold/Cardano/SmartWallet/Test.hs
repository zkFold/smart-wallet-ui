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
import ZkFold.Cardano.SmartWallet.Api (addressFromEmail, sendFunds', sendFundsWithCreation')
import ZkFold.Cardano.SmartWallet.Constants (extraBuildConfiguration)
import ZkFold.Cardano.SmartWallet.Test.Utils
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))

proofBytesFromJwt :: Text -> ByteString -> ZKProofBytes
proofBytesFromJwt jwt keyHash = zkpBytes
 where
  -- Generate an RSA key pair. Private key will be used to sign the JWT (in real life that will be done by Google),
  -- and the public key will be sent to the wallet script alongside the signature.
  --
  (R.PublicKey{..}, R.PrivateKey{..}, _) = generateKeyPair (mkStdGen 42) 2048

  h = hash (encodeUtf8 jwt)

  bsToNat :: ByteString -> Natural
  bsToNat = foldl (\a w -> fromIntegral w + 256 * a) 0 . unpack

  hNat :: Natural
  hNat = bsToNat h

  prD, e, n :: Natural
  prD = fromIntegral private_d
  e = fromIntegral public_e
  n = fromIntegral public_n

  expM :: Natural -> Natural -> Natural -> Natural
  expM _ 0 _ = 1
  expM b ex m =
    case ex `mod` 2 of
      1 -> (b * expM b (ex - 1) m) `mod` m
      _ ->
        let e2 = expM b (ex `div` 2) m
         in (e2 * e2) `mod` m

  msg = expM hNat prD n

  keyNat :: Natural
  keyNat = bsToNat keyHash

  zkpBytes :: ZKProofBytes
  zkpBytes = mkProof $ expModProofMock @ByteString zero (PlonkupProverSecret $ pure zero) (ExpModProofInput e n msg keyNat)

smartWalletTests :: Setup -> TestTree
smartWalletTests setup =
  testGroup
    "privnet-smart-wallet-tests"
    [ testCaseSteps "able to initialize and send funds from a zk based smart wallet" $ \info -> withSetup info setup $ \ctx -> do
        -- Obtain address of wallet.
        let email = emailFromText "zkfold@gmail.com" & either error id
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
            proofBytes = proofBytesFromJwt jwt $ keyHashToRawBytes newKeyHash
            cwi =
              ZKCreateWalletInfo
                { zkcwiProofBytes = proofBytes
                , zkcwiPaymentKeyHash = newKeyHash
                , zkcwiJWT = jwtFromText jwt
                , zkcwiEmail = email
                }
        -- Find suitable UTxO as collateral.
        utxos <- ctxRunQuery ctx $ utxosAtAddress fundUserAddr Nothing
        let collUtxo = utxosToList utxos & head
        initWalletBody <- zkctxRunBuilder ctx walletAddress (utxoRef collUtxo) $ do
          sendFundsWithCreation' zkiws walletAddress cwi [] >>= buildTxBodyWithExtraConfiguration (extraBuildConfiguration zkiws True)
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
                  , boAddress = ctxUserF ctx & userChangeAddress
                  }
              , BuildOut
                  { boValue = valueFromLovelace 20_000_000
                  , boDatum = Nothing
                  , boAddress = ctxUserF ctx & userChangeAddress
                  }
              ]
        spendWalletBody <- zkctxRunBuilder ctx walletAddress (utxoRef collUtxo) $ do
          sendFunds' zkiws walletAddress (ZKSpendWalletInfo{zkswiPaymentKeyHash = newKeyHash, zkswiEmail = email}) outs >>= buildTxBodyWithExtraConfiguration (extraBuildConfiguration zkiws False)
        info $ "send funds tx body: " <> show spendWalletBody
        tidSpend <- ctxRun ctx fundUser $ submitTxBodyConfirmed spendWalletBody [GYSomeSigningKey newKey, GYSomeSigningKey (fundUser & userPaymentSKey)]
        info $ "Submitted spend tx: " <> show tidSpend
    ]

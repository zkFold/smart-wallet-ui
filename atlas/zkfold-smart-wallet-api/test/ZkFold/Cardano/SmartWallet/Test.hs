module ZkFold.Cardano.SmartWallet.Test (smartWalletTests) where

import Codec.Crypto.RSA (generateKeyPair)
import Codec.Crypto.RSA qualified as R
import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString, unpack)
import Data.Set qualified as Set
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
      1 -> (b * (expM b (ex - 1) m)) `mod` m
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
        (zkiws, walletAddress) <- zkctxRunQuery ctx $ addressFromEmail email
        info $ "Wallet's address: " <> show walletAddress
        info $ "Initialized wallet scripts: " <> show zkiws
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
        initWalletBody <- zkctxRunBuilder ctx (ctxUserF ctx & userChangeAddress) (utxoRef collUtxo) $ do
          createWallet' cwi zkiws >>= buildTxBody
        info $ "Wallet initialization tx body: " <> show initWalletBody
        -- We require signature from 'ctxUserF' since we used it's collateral.
        tidInit <- ctxRun ctx (ctxUserF ctx) $ signAndSubmitConfirmed initWalletBody
        info $ "Submitted tx: " <> show tidInit

        -- Register & delegate the withdrawal script.
        nid <- ctxRunQuery ctx networkId
        let stakeCred = GYCredentialByScript $ scriptHash $ zkiwsCheckSig zkiws
            stakeAddr = stakeAddressFromCredential nid stakeCred
        info $ "Stake address (withdrawal script): " <> show stakeAddr
        regTxId <- ctxRun ctx (ctxUserF ctx) $ do
          sps <- stakePools
          let sp = sps & Set.findMin & stakePoolIdFromApi
          let skel = mustHaveCertificate @'PlutusV3 (mkStakeAddressCombinedRegistrationAndDelegationCertificate stakeCred (GYDelegStakeVote sp GYDRepAlwaysAbstain) (GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptInlined $ zkiwsCheckSig zkiws) unitRedeemer))
          body <- buildTxBody skel
          signAndSubmitConfirmed body
        info $ "Submitted withdrawal script's registration & delegation tx: " <> show regTxId
        -- Spending funds from the zk based smart wallet.
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

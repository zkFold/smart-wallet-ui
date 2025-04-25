module ZkFold.Cardano.SmartWallet.Test.Utils (
  zkctxRunQuery,
  zkctxRunBuilder,
  proofBytesFromJwt,
) where

import Codec.Crypto.RSA (generateKeyPair)
import Codec.Crypto.RSA qualified as R
import Control.Monad.Reader (ReaderT (..))
import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString, unpack)
import Data.Text.Encoding (encodeUtf8)
import GeniusYield.Imports (Text)
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.TxBuilder
import GeniusYield.Types
import System.Random (mkStdGen)
import ZkFold.Algebra.Class (zero)
import ZkFold.Cardano.SmartWallet.Constants (zkWalletBuildInfo)
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Protocol.Plonkup.Prover.Secret (PlonkupProverSecret (..))

zkctxRunQuery :: Ctx -> ReaderT ZKWalletBuildInfo GYTxQueryMonadIO a -> IO a
zkctxRunQuery ctx q = runGYTxQueryMonadIO (ctxNetworkId ctx) (ctxProviders ctx) $ runReaderT q zkWalletBuildInfo

zkctxRunBuilder :: Ctx -> GYAddress -> GYTxOutRef -> ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO a -> IO a
zkctxRunBuilder ctx walletAddr coll b = runGYTxBuilderMonadIO (ctxNetworkId ctx) (ctxProviders ctx) [walletAddr] walletAddr (Just (coll, False)) $ runReaderT b zkWalletBuildInfo

proofBytesFromJwt :: forall kr. Text -> GYKeyHash kr -> ZKProofBytes
proofBytesFromJwt jwt (keyHashToRawBytes -> keyHash) = zkpBytes
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
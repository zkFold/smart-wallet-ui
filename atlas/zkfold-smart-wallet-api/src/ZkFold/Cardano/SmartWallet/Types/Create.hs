module ZkFold.Cardano.SmartWallet.Types.Create (
  ZKCreateWalletInfo (..),
  ByteStringFromHex (..),
  ZKF (..),
  ZKProofBytes (..),
  ZKSetupBytes (..),
  mkSetup,
  mkProof,
  ExpModProofInput (..),
  expModSetupMock,
  expModProofMock,
  proofToPlutus,
  setupToPlutus,
) where

import Data.ByteString.Base16 qualified as BS16
import GeniusYield.Imports (coerce)
import GeniusYield.Types
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.OnChain.BLS12_381.F (F (..))
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes (..), SetupBytes (..))
import ZkFold.Cardano.SmartWallet.Types.Common
import ZkFold.Symbolic.Cardano.Contracts.SmartWallet (
  ByteStringFromHex (..),
  ExpModProofInput (..),
  ZKF (..),
  ZKProofBytes (..),
  ZKSetupBytes (..),
  expModProofMock,
  expModSetupMock,
  mkProof,
  mkSetup,
 )

-- | Information required to initialize user's wallet.
data ZKCreateWalletInfo = ZKCreateWalletInfo
  { zkcwiEmail :: !Email
  , zkcwiJWT :: !JWT
  , zkcwiPaymentKeyHash :: !GYPaymentKeyHash
  , zkcwiProofBytes :: !ZKProofBytes
  }

proofToPlutus :: ZKProofBytes -> ProofBytes
proofToPlutus ZKProofBytes{..} =
  ProofBytes
    { cmA_bytes = bsFromHexToPlutus cmA_bytes
    , cmB_bytes = bsFromHexToPlutus cmB_bytes
    , cmC_bytes = bsFromHexToPlutus cmC_bytes
    , cmF_bytes = bsFromHexToPlutus cmF_bytes
    , cmH1_bytes = bsFromHexToPlutus cmH1_bytes
    , cmH2_bytes = bsFromHexToPlutus cmH2_bytes
    , cmZ1_bytes = bsFromHexToPlutus cmZ1_bytes
    , cmZ2_bytes = bsFromHexToPlutus cmZ2_bytes
    , cmQlow_bytes = bsFromHexToPlutus cmQlow_bytes
    , cmQmid_bytes = bsFromHexToPlutus cmQmid_bytes
    , cmQhigh_bytes = bsFromHexToPlutus cmQhigh_bytes
    , proof1_bytes = bsFromHexToPlutus proof1_bytes
    , proof2_bytes = bsFromHexToPlutus proof2_bytes
    , a_xi_int = a_xi_int
    , b_xi_int = b_xi_int
    , c_xi_int = c_xi_int
    , s1_xi_int = s1_xi_int
    , s2_xi_int = s2_xi_int
    , f_xi_int = f_xi_int
    , t_xi_int = t_xi_int
    , t_xi'_int = t_xi'_int
    , z1_xi'_int = z1_xi'_int
    , z2_xi'_int = z2_xi'_int
    , h1_xi'_int = h1_xi'_int
    , h2_xi_int = h2_xi_int
    , l_xi = [coerce l1_xi]
    }
 where
  bsFromHexToPlutus :: ByteStringFromHex -> PlutusTx.BuiltinByteString
  bsFromHexToPlutus (ByteStringFromHex bs) =
    case BS16.decode bs of
      Right b -> PlutusTx.toBuiltin b
      Left e -> error e

setupToPlutus :: ZKSetupBytes -> SetupBytes
setupToPlutus ZKSetupBytes{..} =
  SetupBytes
    { n = n
    , pow = pow
    , omega = coerce omega_int
    , k1 = coerce k1_int
    , k2 = coerce k2_int
    , h1_bytes = PlutusTx.toBuiltin h1_bytes
    , cmQm_bytes = PlutusTx.toBuiltin cmQm_bytes
    , cmQl_bytes = PlutusTx.toBuiltin cmQl_bytes
    , cmQr_bytes = PlutusTx.toBuiltin cmQr_bytes
    , cmQo_bytes = PlutusTx.toBuiltin cmQo_bytes
    , cmQc_bytes = PlutusTx.toBuiltin cmQc_bytes
    , cmQk_bytes = PlutusTx.toBuiltin cmQk_bytes
    , cmS1_bytes = PlutusTx.toBuiltin cmS1_bytes
    , cmS2_bytes = PlutusTx.toBuiltin cmS2_bytes
    , cmS3_bytes = PlutusTx.toBuiltin cmS3_bytes
    , cmT1_bytes = PlutusTx.toBuiltin cmT1_bytes
    }
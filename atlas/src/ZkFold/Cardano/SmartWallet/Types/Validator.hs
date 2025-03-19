{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.SmartWallet.Types.Validator (
  WalletSetup (..),
  SetupBytes (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (BuiltinByteString)
import PlutusTx (makeIsDataIndexed)

-- TODO: Have these definitions in one-place. Currently it's also defined in zkfold-cardano but that comes with it's own bloat that we want to avoid here.
--
-- Or avoid definition altogether by using blueprints?

data WalletSetup = WalletSetup
  { wsPubKeyHash :: !BuiltinByteString
  , wsWeb2UserId :: !BuiltinByteString
  }
  deriving stock (Show, Generic)

makeIsDataIndexed ''WalletSetup [('WalletSetup, 0)]

newtype F = F Integer
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)
makeIsDataIndexed ''F [('F, 0)]

-- TODO: Make fields strict for types defined here?

data SetupBytes = SetupBytes
  { n :: Integer
  , pow :: Integer
  , omega :: F
  , k1 :: F
  , k2 :: F
  , h1_bytes :: BuiltinByteString
  , cmQm_bytes :: BuiltinByteString
  , cmQl_bytes :: BuiltinByteString
  , cmQr_bytes :: BuiltinByteString
  , cmQo_bytes :: BuiltinByteString
  , cmQc_bytes :: BuiltinByteString
  , cmQk_bytes :: BuiltinByteString
  , cmS1_bytes :: BuiltinByteString
  , cmS2_bytes :: BuiltinByteString
  , cmS3_bytes :: BuiltinByteString
  , cmT1_bytes :: BuiltinByteString
  }
  deriving stock (Show, Generic)

makeIsDataIndexed ''SetupBytes [('SetupBytes, 0)]

data ProofBytes = ProofBytes
  { cmA_bytes :: BuiltinByteString
  , cmB_bytes :: BuiltinByteString
  , cmC_bytes :: BuiltinByteString
  , cmF_bytes :: BuiltinByteString
  , cmH1_bytes :: BuiltinByteString
  , cmH2_bytes :: BuiltinByteString
  , cmZ1_bytes :: BuiltinByteString
  , cmZ2_bytes :: BuiltinByteString
  , cmQlow_bytes :: BuiltinByteString
  , cmQmid_bytes :: BuiltinByteString
  , cmQhigh_bytes :: BuiltinByteString
  , proof1_bytes :: BuiltinByteString
  , proof2_bytes :: BuiltinByteString
  , a_xi_int :: Integer
  , b_xi_int :: Integer
  , c_xi_int :: Integer
  , s1_xi_int :: Integer
  , s2_xi_int :: Integer
  , f_xi_int :: Integer
  , t_xi_int :: Integer
  , t_xi'_int :: Integer
  , z1_xi'_int :: Integer
  , z2_xi'_int :: Integer
  , h1_xi'_int :: Integer
  , h2_xi_int :: Integer
  , l1_xi :: F
  }
  deriving stock (Show, Generic)

makeIsDataIndexed ''ProofBytes [('ProofBytes, 0)]

data Web2Creds = Web2Creds
  { wUserId :: BuiltinByteString
  , wTokenHash :: BuiltinByteString
  , wAmount :: Integer
  }
  deriving stock (Show, Generic)

makeIsDataIndexed ''Web2Creds [('Web2Creds, 0)]

data SpendingCreds = SpendWithSignature BuiltinByteString | SpendWithWeb2Token Web2Creds
  deriving stock (Show, Generic)

makeIsDataIndexed ''SpendingCreds [('SpendWithSignature, 0), ('SpendWithWeb2Token, 1)]

data WalletRedeemer = WalletRedeemer
  { wrTxDate :: !BuiltinByteString
  , wrTxRecipient :: !BuiltinByteString
  , wrZkp :: !ProofBytes
  , wrCreds :: !SpendingCreds
  }
  deriving stock (Show, Generic)

makeIsDataIndexed ''WalletRedeemer [('WalletRedeemer, 0)]
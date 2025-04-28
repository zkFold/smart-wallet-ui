module ZkFold.Cardano.SmartWallet.Types.Script (
  ZKWalletBuildInfo (..),
  ZKInitializedWalletScripts (..),
) where

import GeniusYield.Types
import ZkFold.Cardano.UPLC.Wallet.Types

-- | Information required to build transactions for a zk-wallet.
data ZKWalletBuildInfo = ZKWalletBuildInfo
  { zkwbiWeb2AuthMintingPolicy :: !(Web2Creds -> GYScript 'PlutusV3)
  -- ^ Web2 Auth minting policy.
  , zkwbiWalletValidator :: !(GYMintingPolicyId -> GYScriptHash -> GYScript 'PlutusV3)
  -- ^ Wallet spending validator.
  , zkwbiCheckSigRewardValidator :: !(GYMintingPolicyId -> GYScript 'PlutusV3)
  -- ^ Reward validator for wallet's spending validator parameterized by web 2 auth minting policy id.
  }

-- | Fully applied plutus scripts associated with an email.
data ZKInitializedWalletScripts = ZKInitializedWalletScripts
  { zkiwsWeb2Auth :: !(GYScript 'PlutusV3)
  , zkiwsWallet :: !(GYScript 'PlutusV3)
  , zkiwsCheckSig :: !(GYScript 'PlutusV3)
  }
  deriving stock (Show)
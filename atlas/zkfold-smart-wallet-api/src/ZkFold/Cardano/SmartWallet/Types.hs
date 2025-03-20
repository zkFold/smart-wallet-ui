module ZkFold.Cardano.SmartWallet.Types (
  ZKWalletBuildInfo (..),
  ZKWalletException (..),
  ZKWalletQueryMonad,
  module ZkFold.Cardano.SmartWallet.Types.Validator,
) where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader)
import Data.Text qualified as Text
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.Types (GYScript, GYStakeAddress, PlutusVersion (..))
import Network.HTTP.Types (status500)
import ZkFold.Cardano.SmartWallet.Types.Validator

-- | Information required to build transactions for a zk-wallet.
data ZKWalletBuildInfo = ZKWalletBuildInfo
  { zkwbiSmartWalletValidator :: SetupBytes -> WalletSetup -> GYScript 'PlutusV3
  -- ^ Smart wallet validator script.
  , zkwbiMockStakeValidator :: GYScript 'PlutusV3
  -- ^ Mock stake validator used to compute execution units. Since redeemer depends upon script context, and script context depends upon redeemer (as it influences fees and thus also influences change output(s)) we find ourselves in a chicken-and-egg problem. To solve this, we use a mock stake validator whose proofs server can easily compute as script has setup parameters that allow forging.
  }

type ZKWalletQueryMonad m = (GYTxQueryMonad m, MonadReader ZKWalletBuildInfo m)

data ZKWalletException
  = -- | Could not find stake address information for the given stake address.
    ZKWEStakeAddressInfoNotFound !GYStakeAddress
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError ZKWalletException where
  toApiError (ZKWEStakeAddressInfoNotFound sa) =
    GYApiError
      { gaeErrorCode = "STAKE_ADDRESS_INFO_NOT_FOUND"
      , gaeHttpStatus = status500
      , gaeMsg = Text.pack $ "Could not find stake address information for the given stake address: " <> show sa
      }

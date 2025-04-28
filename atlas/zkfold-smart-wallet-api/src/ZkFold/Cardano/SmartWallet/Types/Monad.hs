module ZkFold.Cardano.SmartWallet.Types.Monad (
  ZKWalletQueryMonad,
) where

import Control.Monad.Reader (MonadReader)
import GeniusYield.TxBuilder (GYTxSpecialQueryMonad)
import ZkFold.Cardano.SmartWallet.Types.Script (ZKWalletBuildInfo)

type ZKWalletQueryMonad m = (GYTxSpecialQueryMonad m, MonadReader ZKWalletBuildInfo m)

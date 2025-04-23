module ZkFold.Cardano.SmartWallet.Test.Utils (
  zkctxRunQuery,
  zkctxRunBuilder,
) where

import Control.Monad.Reader (ReaderT (..))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Constants (zkWalletBuildInfo)
import ZkFold.Cardano.SmartWallet.Types

zkctxRunQuery :: Ctx -> ReaderT ZKWalletBuildInfo GYTxQueryMonadIO a -> IO a
zkctxRunQuery ctx q = runGYTxQueryMonadIO (ctxNetworkId ctx) (ctxProviders ctx) $ runReaderT q zkWalletBuildInfo

zkctxRunBuilder :: Ctx -> GYAddress -> GYTxOutRef -> ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO a -> IO a
zkctxRunBuilder ctx walletAddr coll b = runGYTxBuilderMonadIO (ctxNetworkId ctx) (ctxProviders ctx) [walletAddr] walletAddr (Just (coll, False)) $ runReaderT b zkWalletBuildInfo
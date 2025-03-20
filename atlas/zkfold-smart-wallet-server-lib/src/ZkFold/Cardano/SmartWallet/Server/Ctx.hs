module ZkFold.Cardano.SmartWallet.Server.Ctx (
  Ctx (..),
  runSkeletonI,
  runSkeletonWithStrategyI,
  runSkeletonF,
  runSkeletonWithStrategyF,
  runQuery,
  runQueryWithReader,
) where

import Control.Monad.Reader (ReaderT (..))
import GeniusYield.Imports
import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Types (ZKWalletBuildInfo)

-- | Server context: configuration & shared state.
data Ctx = Ctx
  { ctxNetworkId :: !GYNetworkId
  , ctxProviders :: !GYProviders
  , ctxSmartWalletBuildInfo :: !ZKWalletBuildInfo
  , ctxSigningKey :: !(Maybe (GYSomePaymentSigningKey, GYAddress))
  , ctxCollateral :: !(Maybe GYTxOutRef)
  , ctxStakeAddress :: !(Maybe GYStakeAddressBech32)
  }

-- | Create 'TxBody' from a 'GYTxSkeleton'.
runSkeletonI ::
  Ctx ->
  -- | User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | User's collateral.
  Maybe GYTxOutRef ->
  ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO (GYTxSkeleton v) ->
  IO GYTxBody
runSkeletonI = coerce (runSkeletonF @Identity)

-- | Create 'TxBody' from a 'GYTxSkeleton', with the specified coin selection strategy.
runSkeletonWithStrategyI ::
  GYCoinSelectionStrategy ->
  Ctx ->
  -- | User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | User's collateral.
  Maybe GYTxOutRef ->
  ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO (GYTxSkeleton v) ->
  IO GYTxBody
runSkeletonWithStrategyI cstrat = coerce (runSkeletonWithStrategyF @Identity cstrat)

runSkeletonF ::
  (Traversable t) =>
  Ctx ->
  -- | User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | User's collateral.
  Maybe GYTxOutRef ->
  ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO (t (GYTxSkeleton v)) ->
  IO (t GYTxBody)
runSkeletonF = runSkeletonWithStrategyF GYRandomImproveMultiAsset

runSkeletonWithStrategyF ::
  (Traversable t) =>
  GYCoinSelectionStrategy ->
  Ctx ->
  -- | User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | User's collateral.
  Maybe GYTxOutRef ->
  ReaderT ZKWalletBuildInfo GYTxBuilderMonadIO (t (GYTxSkeleton v)) ->
  IO (t GYTxBody)
runSkeletonWithStrategyF cstrat ctx addrs addr mcollateral skeleton = do
  let nid = ctxNetworkId ctx
      providers = ctxProviders ctx
      wi = ctxSmartWalletBuildInfo ctx
      mcollateral' = do
        collateral <- mcollateral
        pure (collateral, False)

  runGYTxMonadNodeF cstrat nid providers (addr : addrs) addr mcollateral' $ runReaderT skeleton wi

runQuery :: Ctx -> ReaderT ZKWalletBuildInfo GYTxQueryMonadIO a -> IO a
runQuery ctx = runQueryWithReader ctx (ctxSmartWalletBuildInfo ctx)

runQueryWithReader :: Ctx -> a -> ReaderT a GYTxQueryMonadIO b -> IO b
runQueryWithReader ctx a q = do
  let nid = ctxNetworkId ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadIO nid providers $ runReaderT q a

runGYTxMonadNodeF :: forall t v. (Traversable t) => GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (t (GYTxSkeleton v)) -> IO (t GYTxBody)
runGYTxMonadNodeF strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= traverse (buildTxBodyWithStrategy strat)

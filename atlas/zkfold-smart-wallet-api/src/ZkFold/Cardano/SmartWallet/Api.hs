module ZkFold.Cardano.SmartWallet.Api (
  sendFunds,
) where

import Control.Monad.Reader (MonadReader (..))
import GeniusYield.TxBuilder (GYTxQueryMonad (..), GYTxSkeleton, mustHaveOutput, mustHaveWithdrawal, throwAppError)
import GeniusYield.Types (GYAddress, GYBuildPlutusScript (..), GYCredential (GYCredentialByScript), GYRedeemer, GYStakeAddressInfo (..), GYTxBuildWitness (..), GYTxWdrl (..), GYValue, PlutusVersion (..), mkGYTxOutNoDatum, scriptHash, stakeAddressFromCredential, unitRedeemer)
import ZkFold.Cardano.SmartWallet.Constants (smartWalletValidator)
import ZkFold.Cardano.SmartWallet.Types

-- | A dummy redeemer. We would update this with actual redeemer later on.
dummyRedeemer :: GYRedeemer
dummyRedeemer = unitRedeemer

-- TODO: To not require @SetupBytes@ and @WalletSetup@, but rather have this function receive `GYScript` directly?

-- | Send funds from a zk-wallet to a given address.
sendFunds :: (ZKWalletQueryMonad m) => SetupBytes -> WalletSetup -> GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
sendFunds sb ws sendAddr sendVal = do
  nid <- networkId
  zkwbi <- ask
  let stakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash $ smartWalletValidator sb ws)
  let mockStakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash $ zkwbiMockStakeValidator zkwbi)
  -- TODO: Perhaps stake address information is not required if we are sure that withdrawal amount would always be zero.
  si <-
    stakeAddressInfo stakeAddr >>= \case
      Just si -> pure si
      Nothing -> throwAppError $ ZKWEStakeAddressInfoNotFound stakeAddr
  pure $
    mustHaveOutput (mkGYTxOutNoDatum sendAddr sendVal)
      -- TODO: To make use of reference scripts?
      <> mustHaveWithdrawal (GYTxWdrl{gyTxWdrlStakeAddress = mockStakeAddr, gyTxWdrlAmount = gyStakeAddressInfoAvailableRewards si, gyTxWdrlWitness = GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptInlined (zkwbiMockStakeValidator zkwbi)) dummyRedeemer})
module ZkFold.Cardano.SmartWallet.Api (
  validatorSetupFromEmail,
  addressFromEmail,
  addressFromValidatorSetup,
  sendFunds,
  sendFunds',
) where

import Control.Monad ((>=>))
import Control.Monad.Reader (MonadReader (..))
import GeniusYield.TxBuilder (GYTxQueryMonad (..), GYTxSkeleton, mustHaveOutput, mustHaveWithdrawal, throwAppError)
import GeniusYield.Types (GYAddress, GYBuildPlutusScript (..), GYCredential (GYCredentialByScript), GYRedeemer, GYStakeAddressInfo (..), GYTxBuildWitness (..), GYTxWdrl (..), GYValue, PlutusVersion (..), mkGYTxOutNoDatum, scriptHash, stakeAddressFromCredential, unitRedeemer)
import ZkFold.Cardano.SmartWallet.Types

-- | A dummy redeemer. We would update this with actual redeemer later on.
dummyRedeemer :: GYRedeemer
dummyRedeemer = unitRedeemer

-- TODO: To not require @SetupBytes@ and @WalletSetup@, but rather have this function receive `GYScript` directly?

validatorSetupFromEmail :: (ZKWalletQueryMonad m) => Email -> m ValidatorSetup
validatorSetupFromEmail email = undefined -- FIXME:

addressFromEmail :: (ZKWalletQueryMonad m) => Email -> m GYAddress
addressFromEmail = validatorSetupFromEmail >=> addressFromValidatorSetup

addressFromValidatorSetup :: (ZKWalletQueryMonad m) => ValidatorSetup -> m GYAddress
addressFromValidatorSetup (sb, ws) = undefined -- FIXME:

-- | Send funds from a zk-wallet to a given address.
sendFunds :: (ZKWalletQueryMonad m) => Email -> GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
sendFunds email sendAddr sendVal = validatorSetupFromEmail email >>= \validatorSetup -> sendFunds' validatorSetup sendAddr sendVal

sendFunds' :: (ZKWalletQueryMonad m) => ValidatorSetup -> GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
sendFunds' (sb, ws) sendAddr sendVal = do
  nid <- networkId
  zkwbi <- ask
  let stakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash $ zkwbiSmartWalletValidator zkwbi sb ws)
  let mockStakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash $ zkwbiMockStakeValidator zkwbi)
  -- TODO: Perhaps stake address information is not required if we are sure that withdrawal amount would always be zero.
  si <-
    stakeAddressInfo stakeAddr >>= \case
      Just si -> pure si
      Nothing -> throwAppError $ ZKWEStakeAddressInfoNotFound stakeAddr
  pure $
    mustHaveOutput (mkGYTxOutNoDatum sendAddr sendVal)
      -- TODO: To make use of reference scripts?
      <> mustHaveWithdrawal
        ( GYTxWdrl
            { gyTxWdrlStakeAddress = mockStakeAddr
            , gyTxWdrlAmount = gyStakeAddressInfoAvailableRewards si
            , gyTxWdrlWitness = GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptInlined (zkwbiMockStakeValidator zkwbi)) dummyRedeemer
            }
        )
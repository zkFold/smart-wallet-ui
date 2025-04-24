module ZkFold.Cardano.SmartWallet.Types.Register (
  ZKRegisterWithdrawalScriptInfo (..),
) where

import Deriving.Aeson
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Types.Common

-- | Information required to register and delegate the withdrawal script.
data ZKRegisterWithdrawalScriptInfo = ZKRegisterWithdrawalScriptInfo
  { zkradiEmail :: !Email
  , zkradiPaymentKeyHash :: !GYPaymentKeyHash
  }
  deriving stock (Show, Generic)
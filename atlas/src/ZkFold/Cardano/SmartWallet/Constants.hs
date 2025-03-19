{-# LANGUAGE TemplateHaskell #-}

module ZkFold.Cardano.SmartWallet.Constants (
  smartWalletValidator,
)
where

import Data.FileEmbed
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Types (SetupBytes, WalletSetup)

smartWalletValidator :: SetupBytes -> WalletSetup -> GYScript 'PlutusV3
smartWalletValidator =
  let fileBS = $(makeRelativeToProject "./data/compiled-scripts/smart-wallet.plutus" >>= embedFile)
   in -- TODO: Check if script was correctly parsed. It might have been the case that we need to unwrap one CBOR layer of the script.
      -- TODO: Load the parameterised script, perhaps via blueprint feature of Atlas?
      case readScript' fileBS of
        Left e -> error $ "Failed to read smart-wallet.plutus: " <> show e
        Right script -> applyParam . applyParam script
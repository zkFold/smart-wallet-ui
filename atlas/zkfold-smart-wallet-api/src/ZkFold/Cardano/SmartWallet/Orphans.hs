{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.SmartWallet.Orphans () where

import Control.Lens ((?~))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import GeniusYield.Imports ((&))
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()

-- TODO: Move it to Atlas.
instance Swagger.ToSchema GYDatum where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYDatum" $
        mempty & Swagger.description
          ?~ "JSON representation of datum"
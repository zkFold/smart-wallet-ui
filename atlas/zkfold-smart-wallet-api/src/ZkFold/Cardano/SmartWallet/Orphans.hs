{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.SmartWallet.Orphans () where

import Control.Lens ((?~))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import GeniusYield.Imports ((&))
import GeniusYield.Swagger.Utils
import GeniusYield.Types
import GeniusYield.Types.OpenApi ()
import ZkFold.Symbolic.Cardano.Contracts.SmartWallet (ZKProofBytes (..), ZKF (..), ByteStringFromHex (..))

-- TODO: Move it to Atlas.
instance Swagger.ToSchema GYDatum where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYDatum" $
        mempty & Swagger.description
          ?~ "JSON representation of datum"

instance Swagger.ToSchema ZKF where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Field element."

instance Swagger.ToSchema ByteStringFromHex where
  declareNamedSchema _ =
    pure $
      Swagger.named "ByteStringFromHex" $
        mempty & Swagger.type_
          ?~ Swagger.SwaggerString & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ "Bytes encoded in hex."

instance Swagger.ToSchema ZKProofBytes where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Proof bytes where bytes are represented in hexadecimal encoding."


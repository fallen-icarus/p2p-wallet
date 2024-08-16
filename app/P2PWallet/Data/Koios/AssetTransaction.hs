{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/asset_txs API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.AssetTransaction where

import Data.Aeson

import P2PWallet.Prelude

data AssetTransaction = AssetTransaction
  { txHash :: Text 
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AssetTransaction

instance FromJSON AssetTransaction where
  parseJSON = withObject "AssetTransaction" $ \o ->
      AssetTransaction
        <$> o .: "tx_hash"

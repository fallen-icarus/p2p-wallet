{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.Network where

import Data.Aeson qualified as Aeson

import P2PWallet.Prelude

-- | Which cardano network to use. 
data Network 
  = Mainnet
  | Testnet -- ^ Preproduction testnet.
  deriving (Show,Eq)

readNetwork :: Text -> Maybe Network
readNetwork "mainnet" = Just Mainnet
readNetwork "testnet" = Just Testnet
readNetwork _ = Nothing

instance ToText Network where
  toText Mainnet = "mainnet"
  toText Testnet = "testnet"

instance ToString Network where
  toString Mainnet = "mainnet"
  toString Testnet = "testnet"

instance Aeson.ToJSON Network where
  toJSON = Aeson.toJSON . toText

instance Aeson.FromJSON Network where
  parseJSON = Aeson.withText "Network" (maybe mzero return . readNetwork)

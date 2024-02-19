{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.App.Config where

import Data.Aeson

import P2PWallet.Data.Core.Network
import P2PWallet.Prelude

data Config = Config
  { _network :: Network -- ^ Which network to use.
  } deriving (Show,Eq)

instance Default Config where
  def = Config
    { _network = Testnet
    }

instance ToJSON Config where
  toJSON Config{..} =
    object 
      [ "network" .= _network
      ]

instance FromJSON Config where
  parseJSON =
    withObject "Config" $ \o ->
      Config
        <$> o .: "network"

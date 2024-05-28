{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Network where

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple (SQLData(SQLText))

import P2PWallet.Prelude

-- | Which cardano network to use. 
data Network 
  = Mainnet
  | Testnet -- ^ Preproduction testnet.
  deriving (Show,Eq,Ord)

makePrisms ''Network

readNetwork :: Text -> Maybe Network
readNetwork "mainnet" = Just Mainnet
readNetwork "testnet" = Just Testnet
readNetwork _ = Nothing

displayNetwork :: Network -> Text
displayNetwork Mainnet = "Mainnet"
displayNetwork Testnet = "Testnet"

instance ToText Network where
  toText Mainnet = "mainnet"
  toText Testnet = "testnet"

instance ToString Network where
  toString Mainnet = "mainnet"
  toString Testnet = "testnet"

instance ToField Network where
  toField = toField . toText

instance FromField Network where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "failed to parse Network") Ok $ readNetwork t
  fromField f = returnError ConversionFailed f "need a text"

instance Default Network where
  def = Testnet

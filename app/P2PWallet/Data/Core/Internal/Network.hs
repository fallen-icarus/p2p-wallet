{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.Network where

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

parseNetwork :: Text -> Maybe Network
parseNetwork "mainnet" = Just Mainnet
parseNetwork "testnet" = Just Testnet
parseNetwork _ = Nothing

instance Display Network where
  display Mainnet = "Mainnet"
  display Testnet = "Testnet"

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
    maybe (returnError ConversionFailed f "failed to parse Network") Ok $ parseNetwork t
  fromField f = returnError ConversionFailed f "need a text"

instance Default Network where
  def = Testnet

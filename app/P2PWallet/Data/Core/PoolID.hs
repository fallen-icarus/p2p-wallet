{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.PoolID where

import Data.Aeson 
import Prettyprinter (Pretty(..))

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))

import P2PWallet.Prelude

newtype PoolID = PoolID { unPoolId :: Text }
  deriving (Show)
  deriving newtype (Eq,Ord,ToField,FromField,FromJSON,ToJSON)

makeFieldLabelsNoPrefix ''PoolID

instance Pretty PoolID where
  pretty (PoolID addr) = pretty addr

instance IsString PoolID where
  fromString = PoolID . toText

instance ToText PoolID where
  toText (PoolID addr) = addr

instance ToString PoolID where
  toString (PoolID addr) = toString addr

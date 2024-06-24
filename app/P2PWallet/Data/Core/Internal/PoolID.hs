{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.PoolID where

import Data.Aeson 

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))

import P2PWallet.Prelude

newtype PoolID = PoolID { unPoolId :: Text }
  deriving (Show)
  deriving newtype (Eq,Ord,ToField,FromField,FromJSON,ToJSON,IsString,ToText,ToString)

makeFieldLabelsNoPrefix ''PoolID

instance Display PoolID where
  display = unPoolId

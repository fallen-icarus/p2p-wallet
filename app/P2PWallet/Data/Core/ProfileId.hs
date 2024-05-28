{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.ProfileId where

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))

import P2PWallet.Prelude

-- | The row id for the profiles sqlite table.
newtype ProfileId = ProfileId { unProfileId :: Int }
  deriving (Show)
  deriving newtype (Eq,Ord,Num,ToField,FromField)

makeFieldLabelsNoPrefix ''ProfileId

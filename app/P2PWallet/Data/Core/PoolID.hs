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

import P2PWallet.Prelude

newtype PoolID = PoolID { unPoolId :: Text }
  deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''PoolID

instance ToJSON PoolID where
  toJSON = toJSON . unPoolId

instance FromJSON PoolID where
  parseJSON = withText "PoolID" (return . PoolID)

instance Pretty PoolID where
  pretty (PoolID addr) = pretty addr

instance IsString PoolID where
  fromString = PoolID . toText

instance ToText PoolID where
  toText (PoolID addr) = addr

instance ToString PoolID where
  toString (PoolID addr) = toString addr

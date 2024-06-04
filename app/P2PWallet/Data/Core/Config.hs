{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

Type to represent the current user configurations for the app. 

-}
module P2PWallet.Data.Core.Config where

import Data.Time (utc)

import P2PWallet.Data.Core.Network
import P2PWallet.Prelude

data Config = Config
  { network :: Network -- ^ Which network to use.
  , timeZone :: TimeZone -- ^ The user's current time zone.
  , currentDay :: Day -- ^ The current date.
  } deriving (Show,Eq)

instance Default Config where
  def = Config
    { network = def
    , timeZone = utc
    , currentDay = def
    }

makeFieldLabelsNoPrefix ''Config

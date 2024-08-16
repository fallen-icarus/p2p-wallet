{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

Type to represent the current user configurations for the app. 

-}
module P2PWallet.Data.Core.Internal.Config where

import Data.Time (utc)

import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Prelude

data Config = Config
  -- | Which network to use.
  { network :: Network
  -- | The user's current time zone.
  , timeZone :: TimeZone
  -- | The current date.
  , currentDay :: Day
  -- | The current time.
  , currentTime :: POSIXTime
  } deriving (Show,Eq)

instance Default Config where
  def = Config
    { network = def
    , timeZone = utc
    , currentDay = def
    , currentTime = 0
    }

makeFieldLabelsNoPrefix ''Config

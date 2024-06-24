{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TickerRegistryModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Prelude

-------------------------------------------------
-- Ticker Registry Events
-------------------------------------------------
-- | The possible UI events on the Ticker Registry page.
data TickerRegistryEvent
  -- | Add a new ticker to the ticker registry.
  = AddNewTickerInfo (AddEvent Text TickerInfo)
  -- | Edit the selected ticker info.
  | ChangeTickerInfo (AddEvent NewTickerInfo TickerInfo)
  -- | Delete the selected ticker info.
  | DeleteTickerInfo (DeleteWithConfirmationEvent NewTickerInfo)

-------------------------------------------------
-- Ticker Registry Model
-------------------------------------------------
data TickerRegistryModel = TickerRegistryModel
  -- | The targets to search for.
  { search :: Text
  -- | The new ticker info.
  , newTickerInfo :: NewTickerInfo
  -- | Whether the add new ticker widget should be open.
  , addingTicker :: Bool
  -- | Whether the edit ticker widget should be open.
  , editingTicker :: Bool
  -- | Whether the delete ticker widget should be open.
  , deletingTicker :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''TickerRegistryModel

instance Default TickerRegistryModel where
  def = TickerRegistryModel 
    { search = ""
    , newTickerInfo = def
    , addingTicker = False
    , editingTicker = False
    , deletingTicker = False
    }

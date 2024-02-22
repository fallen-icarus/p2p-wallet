{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.App.Common where

import P2PWallet.Prelude

-------------------------------------------------
-- Add a new `a`.
-------------------------------------------------
-- | Add UI steps for something of type `a`.
data AddEvent a
  -- | Open the widget for getting the new information.
  = StartAdding
  -- | Close the widget and reset the new information in the form.
  | CancelAdding
  -- | Keep the widget open, validate the information, and (if pairing) try to get the required 
  -- information from the hardware wallet. If anything fails, display an error message to the 
  -- user. The widget is kept open to allow the user to quickly try again.
  | ConfirmAdding
  -- | Process the new information and add it to the list of tracked items.
  | AddResult a
  deriving (Show,Eq)

-------------------------------------------------
-- Filters
-------------------------------------------------
-- | Filter UI steps.
data FilterEvent a
  -- | Open the widget for editing the currently set filters.
  = StartFiltering 
  -- | Close the widget and keep the previously set filters.
  | CancelFiltering 
  -- | Close the widget and reset the previously set filters.
  | ResetFiltering 
  -- | Keep the widget open and validate the user input. It is kept open in case validation fails
  -- so that users can quickly try again.
  | VerifyFilters 
  -- | Close the widget and replace the previously set filters with the new ones.
  | ConfirmFilters a 
  deriving (Show,Eq)

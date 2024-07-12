{-# LANGUAGE StrictData #-}

module P2PWallet.Data.AppModel.Common where

import P2PWallet.Prelude

-------------------------------------------------
-- Add/Edit an `a`
-------------------------------------------------
-- | Add UI steps for something of type `a`. This is also used for editing. The returned type
-- can be different than the initializing type in `StartAdding`. This flexibility is useful for
-- adding something to the builder while initializing the new data with some other data type.
data AddEvent a b
  -- | Open the widget for getting the new information. Optionally take the info for setting aside
  -- until after changes are made. This is used when a model does not normally use a `selected` 
  -- field.
  = StartAdding (Maybe a)
  -- | Close the widget and reset the new information in the form.
  | CancelAdding
  -- | Keep the widget open, validate the information, and try to get any information from the real
  -- world. If anything fails, display an error message to the user. The widget is kept open to
  -- allow the user to quickly try again. This is meant to be the IO step.
  | ConfirmAdding
  -- | Process the new information and update the database.
  | AddResult b
  deriving (Show,Eq)

-------------------------------------------------
-- Delete an `a`
-------------------------------------------------
-- | Delete UI steps for something of type `a`.
data DeleteWithConfirmationEvent a
  -- | Open the widget for confirming the deletion. Optionally take the info for setting aside
  -- until after changes are made. This is used when a model does not normally use a `selected` 
  -- field.
  = GetDeleteConfirmation (Maybe a)
  -- | Close the widget without deleting the item.
  | CancelDeletion
  -- | Delete the item. This is meant to be the IO step.
  | ConfirmDeletion
  -- | Update UI to post deletion.
  | PostDeletionAction
  deriving (Show,Eq)

-------------------------------------------------
-- Process Event
-------------------------------------------------
-- | Most interactions with the real world require two steps: get/edit/delete the data, and process the result.
-- This tries to separate the pure part from the IO part.
data ProcessEvent a
  = StartProcess -- IO step.
  | ProcessResults a -- Pure step.
  deriving (Show,Eq)

-------------------------------------------------
-- Sorting Directions
-------------------------------------------------
-- | Sorting Direction.
data SortDirection
  = SortAscending
  | SortDescending
  deriving (Show,Eq,Enum)

instance Display SortDirection where
  display SortAscending = "Ascending"
  display SortDescending = "Descending"

-- | A list of possible sorting directions. This is useful for dropdown menus.
sortingDirections :: [SortDirection]
sortingDirections = enumFrom SortAscending

-------------------------------------------------
-- Filtering Scenes
-------------------------------------------------
-- | The filter widget subscene.
data FilterScene
  = FilterScene
  | SortScene
  | SearchScene
  deriving (Show,Eq)

instance Default FilterScene where
  def = FilterScene


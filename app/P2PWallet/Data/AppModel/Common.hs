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
  -- | Keep the widget open, validate the information, and (if pairing) try to get the required 
  -- information from the hardware wallet. If anything fails, display an error message to the 
  -- user. The widget is kept open to allow the user to quickly try again.
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
  -- | Delete the item.
  | ConfirmDeletion
  -- | Update UI to post deletion.
  | PostDeletionAction
  deriving (Show,Eq)

-------------------------------------------------
-- Syncing information of type `a`
-------------------------------------------------
-- | The UI steps for syncing. These steps are automated.
data SyncEvent a
  = StartSync
  | SyncResults a
  deriving (Show,Eq)

-------------------------------------------------
-- Sorting Directions
-------------------------------------------------
-- | Sorting Direction.
data SortDirection
  = SortAscending
  | SortDescending
  deriving (Show,Eq,Enum)

displaySortDirection :: SortDirection -> Text
displaySortDirection SortAscending = "Ascending"
displaySortDirection SortDescending = "Descending"

-- | A list of possible sorting directions. This is useful for dropdown menus.
sortingDirections :: [SortDirection]
sortingDirections = enumFrom SortAscending

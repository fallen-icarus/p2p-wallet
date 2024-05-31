{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Home scene is dedicated to `PaymentWallet`s.

-}
module P2PWallet.Data.AppModel.Home where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Wallets.PaymentWallet
import P2PWallet.Data.Transaction
import P2PWallet.Prelude

-------------------------------------------------
-- Home Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Home page.
data HomeScene
  -- | Information about the payment wallet as well as access to adding new payment wallets.
  = HomeAbout 
  -- | Transaction history for the address.
  | HomeTransactions 
  -- | UTxOs at the address.
  | HomeUTxOs 
  -- | Assets at the address.
  | HomeAssets 
  deriving (Eq,Show)

-- | The filter widget subscene
data FilterScene
  = FilterScene
  | SortScene
  | SearchScene
  deriving (Show,Eq)

instance Default FilterScene where
  def = FilterScene

data SortDirection
  = SortAscending
  | SortDescending
  deriving (Show,Eq,Enum)

displaySortDirection :: SortDirection -> Text
displaySortDirection SortAscending = "Ascending"
displaySortDirection SortDescending = "Descending"

sortingDirections :: [SortDirection]
sortingDirections = enumFrom SortAscending

-------------------------------------------------
-- Home Page Events
-------------------------------------------------
-- | The possible UI events on the Home page.
data HomeEvent
  -- | Change the Home subscene to the specified subscene.
  = ChangeHomeScene HomeScene
  -- | Pair a new `PaymentWallet`. It can only be done from the `HomeAbout` subscene.
  | PairPaymentWallet (AddEvent PaymentWallet)
  -- | Watch a new `PaymentWallet`. It can only be done from the `HomeAbout` subscene.
  | WatchPaymentWallet (AddEvent PaymentWallet)
  -- | Change a payment wallet name.
  | ChangePaymentWalletName (AddEvent Text)
  -- | Delete a payment wallet.
  | DeletePaymentWallet (DeleteWithConfirmationEvent PaymentWallet)
  -- | Open the more popup widget
  | ShowMorePopup
  -- | Reset UTxO Filters.
  | ResetUTxOFilters
  -- | Reset Asset Filters.
  | ResetAssetFilters
  -- | Show all UTxO detials.
  | ShowAllUTxODetails
  -- | Hide all UTxO detials.
  | HideAllUTxODetails
  -- | Inspect the target transaction.
  | InspectHomeTransaction Transaction
  -- | Stop inspecting transaction.
  | CloseInspectedHomeTransaction

-------------------------------------------------
-- UTxO Filter Model
-------------------------------------------------
-- | Possible sortings.
data UTxOSortMethod
  = UTxOLexicographical
  | UTxOAdaBalance
  | UTxOTime
  | UTxOSearchTokenBalance
  deriving (Show,Eq,Enum)

displayUTxOSortMethod :: UTxOSortMethod -> Text
displayUTxOSortMethod UTxOLexicographical = "Lexicographically"
displayUTxOSortMethod UTxOAdaBalance = "Ada Quantity"
displayUTxOSortMethod UTxOTime = "Chronologically"
displayUTxOSortMethod UTxOSearchTokenBalance = "Search Token Quantity"

utxoSortingMethods :: [UTxOSortMethod]
utxoSortingMethods = enumFrom UTxOLexicographical

data UTxOFilterModel = UTxOFilterModel
  { hasReferenceScript :: Maybe Bool
  , hasDatum :: Maybe Bool
  , hasNativeAssets :: Maybe Bool
  , sortingMethod :: UTxOSortMethod
  , sortingDirection :: SortDirection
  , search :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''UTxOFilterModel

instance Default UTxOFilterModel where
  def = UTxOFilterModel
    { hasReferenceScript = Nothing
    , hasDatum = Nothing
    , hasNativeAssets = Nothing
    , sortingMethod = UTxOLexicographical
    , sortingDirection = SortAscending
    , search = ""
    }

-------------------------------------------------
-- Asset Filter Model
-------------------------------------------------
data AssetFilterModel = AssetFilterModel
  { search :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AssetFilterModel

instance Default AssetFilterModel where
  def = AssetFilterModel
    { search = ""
    }

-------------------------------------------------
-- Home State
-------------------------------------------------
data HomeModel = HomeModel
  -- | The current subscene.
  { scene :: HomeScene 
  -- | The currently focused `PaymentWallet` from the list of tracked `PaymentWallet`s.
  , selectedWallet :: PaymentWallet
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the edit payment wallet widget should be open.
  , editingWallet :: Bool
  -- | Whether the delete payment wallet widget should be open.
  , deletingWallet :: Bool
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | The information for the new `PaymentWallet` being paired.
  , newPaymentWallet :: NewPaymentWallet
  -- | Whether to show the filter widget for UTxOs.
  , showUTxOFilter :: Bool
  -- | The active scene for the utxo filter widget.
  , utxoFilterScene :: FilterScene
  -- | The current filter settings for the Home UTxOs.
  , utxoFilterModel :: UTxOFilterModel
  -- | The current filter settings for the Home Assets.
  , assetFilterModel :: AssetFilterModel
  -- | Whether to show the filter widget for assets.
  , showAssetFilter :: Bool
  -- | Focused transaction details.
  , inspectedTransaction :: Maybe Transaction
  } deriving (Eq,Show)

instance Default HomeModel where
  def = HomeModel 
    { scene = HomeAbout
    , selectedWallet = def
    , addingWallet = False
    , editingWallet = False
    , deletingWallet = False
    , showMorePopup = False
    , newPaymentWallet = def
    , showUTxOFilter = False
    , utxoFilterScene = def
    , utxoFilterModel = def
    , assetFilterModel = def
    , showAssetFilter = False
    , inspectedTransaction = Nothing
    }

makeFieldLabelsNoPrefix ''HomeModel

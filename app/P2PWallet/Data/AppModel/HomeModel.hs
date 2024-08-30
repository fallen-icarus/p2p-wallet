{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Home scene is dedicated to `PaymentWallet`s.

-}
module P2PWallet.Data.AppModel.HomeModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.LoanBuilderModel.LenderAddressUpdate
import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

-------------------------------------------------
-- Home Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Home page.
data HomeScene
  -- | Information about the payment wallet.
  = HomeAbout 
  -- | Transaction history for the address.
  | HomeTransactions 
  -- | UTxOs at the address.
  | HomeUTxOs 
  -- | Assets at the address.
  | HomeAssets 
  deriving (Eq,Show)

-------------------------------------------------
-- Home Page Events
-------------------------------------------------
-- | The possible UI events on the Home page.
data HomeEvent
  -- | Change the Home subscene to the specified subscene.
  = ChangeHomeScene HomeScene
  -- | Pair a new `PaymentWallet`. It can only be done from the `HomeAbout` subscene.
  | PairPaymentWallet (AddEvent PaymentWallet PaymentWallet)
  -- | Watch a new `PaymentWallet`. It can only be done from the `HomeAbout` subscene.
  | WatchPaymentWallet (AddEvent PaymentWallet PaymentWallet)
  -- | Add the corresponding stake wallet to the database.
  | AddCorrespondingStakeWallet (ProcessEvent () StakeWallet)
  -- | Change a payment wallet name.
  | ChangePaymentWalletName (AddEvent Text PaymentWallet)
  -- | Delete a payment wallet.
  | DeletePaymentWallet (DeleteWithConfirmationEvent PaymentWallet)
  -- | Open the more popup widget
  | ShowHomeMorePopup
  -- | Reset UTxO Filters.
  | ResetUTxOFilters
  -- | Reset Asset Filters.
  | ResetAssetFilters
  -- | Reset Tx Filters.
  | ResetHomeTxFilters
  -- | Show all UTxO detials.
  | ShowAllUTxODetails
  -- | Hide all UTxO detials.
  | HideAllUTxODetails
  -- | Inspect the target transaction.
  | InspectHomeTransaction Transaction
  -- | Stop inspecting transaction.
  | CloseInspectedHomeTransaction
  -- | Add the selected user input to the tx builder.
  | AddSelectedUserInput PersonalUTxO
  -- | Add the selected collateral input to the tx builder.
  | AddSelectedCollateralInput PersonalUTxO
  -- | Add the selected change address to the tx builder.
  | AddSelectedChangeAddress PaymentAddress
  -- | Inspect loan for corresponding key nft.
  | InspectCorrespondingLoan Loans.LoanId
  -- | Stop inspecting the loan's history.
  | CloseInspectedCorrespondingLoan
  -- | Claim collateral from an expired loan.
  | ClaimExpiredCollateral LoanUTxO
  -- | Burn leftover Key NFT.
  | BurnLoanKeyNFT Loans.LoanId
  -- | Update lender payment address.
  | UpdateLenderPaymentAddress (AddEvent LoanUTxO LenderAddressUpdate)

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

instance Display UTxOSortMethod where
  display UTxOLexicographical = "Lexicographically"
  display UTxOAdaBalance = "Ada Quantity"
  display UTxOTime = "Chronologically"
  display UTxOSearchTokenBalance = "Search Token Quantity"

-- | The list of possible sorting methods. This is useful for dropdown menus.
utxoSortingMethods :: [UTxOSortMethod]
utxoSortingMethods = enumFrom UTxOLexicographical

data UTxOFilterModel = UTxOFilterModel
  -- | Whether the UTxO should have a reference script. When `Nothing`, it will match either way.
  { hasReferenceScript :: Maybe Bool
  -- | Whether the UTxO should have a datum. When `Nothing`, it will match either way.
  , hasDatum :: Maybe Bool
  -- | Whether the UTxO should have native assets. When `Nothing`, it will match either way.
  , hasNativeAssets :: Maybe Bool
  -- | The current sorting method for the UTxOs.
  , sortingMethod :: UTxOSortMethod
  -- | The current sorting direction for the UTxOs.
  , sortingDirection :: SortDirection
  -- | The targets to search for.
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
-- | The types of Key NFTs.
data KeyNftType
  = LoanKey
  | OptionsKey
  deriving (Show,Eq)

data AssetFilterModel = AssetFilterModel
  -- | The targets to search for.
  { search :: Text
  , keyNftType :: Maybe KeyNftType
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AssetFilterModel

instance Default AssetFilterModel where
  def = AssetFilterModel
    { search = ""
    , keyNftType = Nothing
    }

-------------------------------------------------
-- Transaction Filter Model
-------------------------------------------------
data HomeTxFilterModel = HomeTxFilterModel
  -- | The targets to search for.
  { search :: Text
  -- | The date range for displaying transactions.
  , dateRange :: (Maybe Day, Maybe Day)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''HomeTxFilterModel

instance Default HomeTxFilterModel where
  def = HomeTxFilterModel
    { search = ""
    , dateRange = (Nothing,Nothing)
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
  -- | The information for the new `PaymentWallet` being added.
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
  -- | The current filter settings for the Home Transactions.
  , txFilterModel :: HomeTxFilterModel
  -- | Whether to show the widget for filtering transactions.
  , showTransactionFilter :: Bool
  -- | The active scene for the tx filter widget.
  , txFilterScene :: FilterScene
  -- | The text field where new aliases are entered.
  , newAliasField :: Text
  -- | Focused loan history.
  , inspectedLoan :: Maybe Loans.LoanId
  -- | The new loan address update.
  , newLenderAddressUpdate :: Maybe NewLenderAddressUpdate
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
    , txFilterModel = def
    , showTransactionFilter = False
    , txFilterScene = FilterScene
    , newAliasField = ""
    , inspectedLoan = Nothing
    , newLenderAddressUpdate = Nothing
    }

makeFieldLabelsNoPrefix ''HomeModel

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Dex scene is dedicated to Cardano-Swaps `DexWallet`.

-}
module P2PWallet.Data.AppModel.DexModel where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.SwapBuilderModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Prelude

-------------------------------------------------
-- Cached Order Books
-------------------------------------------------
-- | A type alias for a map from trading pair to known swaps.
type CachedOrderBooks = Map.Map (OfferAsset,AskAsset) [SwapUTxO]

-------------------------------------------------
-- Dex Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Dex page.
data DexScene
  -- | Information about all open positions.
  = DexPositions
  -- | Trade history for the address.
  | DexTradeHistory
  -- | The order-book market.
  | DexMarket
  deriving (Eq,Show)

-------------------------------------------------
-- Dex Page Events
-------------------------------------------------
-- | The possible UI events on the Dex page.
data DexEvent
  -- | Change the Dex subscene to the specified subscene.
  = ChangeDexScene DexScene
  -- | Add a new dex wallet using one of the known staking credentials.
  | AddNewDexWallet (AddEvent DexWallet DexWallet)
  -- | Delete a swap wallet.
  | DeleteDexWallet (DeleteWithConfirmationEvent DexWallet)
  -- | Open the more popup widget
  | ShowDexMorePopup
  -- | Clear the fields for a new limit order whenever the swap changes direction.
  -- This is useful for nudging users to update the offer quantity since the units 
  -- depend on the swap direction.
  | ClearLimitOrderFields
  -- | Clear all fields whenever the swap type changes (ie, limit order -> liquidity swap).
  | ClearNewSwapForm
  -- | Invert the order book: the bids become the asks, and vice versa. The units are also changed
  -- to be the new ask asset.
  | InvertOrderBook
  -- | Set the new trading pair as teh selected trading pair.
  | SetNewTradingPair (AddEvent (Text,Text) (OfferAsset, AskAsset))
  -- | Add the new limit order to the transaction builder.
  | AddNewLimitOrderCreation (ProcessEvent SwapCreation)
  -- | Add the new liquidity swap to the transaction builder.
  | AddNewLiquiditySwapCreation (ProcessEvent SwapCreation)
  -- | Sync the order-book for the selected trading pair.
  | SyncOrderBook (ProcessEvent CachedOrderBooks)
  -- | Add the new swap close to the transaction builder.
  | AddSelectedSwapClose SwapUTxO
  -- | Add the new swap update to the transaction builder.
  | AddSelectedSwapUpdate (AddEvent SwapUTxO SwapUpdate)
  -- | Reset the positions filter model.
  | ResetPositionsFilters

-------------------------------------------------
-- Positions Filter Model
-------------------------------------------------
-- | Possible sortings.
data PositionsSortMethod
  -- | By utxo output reference.
  = PositionsLexicographical
  -- | By the quantity of the offer asset present in the swap.
  | PositionsOfferQuantity
  -- | By the quantity of the ask asset present in the swap.
  | PositionsAskQuantity
  -- | By the time the swap was last "touched".
  | PositionsTime
  -- | The limit price for the specified trading pair.
  | PositionsPrice
  deriving (Show,Eq,Enum)

instance Display PositionsSortMethod where
  display PositionsLexicographical = "Lexicographically"
  display PositionsOfferQuantity = "Offer Asset Quantity"
  display PositionsAskQuantity = "Ask Asset Quantity"
  display PositionsTime = "Chronologically"
  display PositionsPrice = "Limit Price"

data PositionsFilterModel = PositionsFilterModel
  -- | The swap must offer the specified asset. Leave it blank for any asset.
  { offerAsset :: Text
  -- | The swap must ask for the specified asset. Leave it blank for any asset.
  , askAsset :: Text
  -- | The current sorting method for the positions.
  , sortingMethod :: PositionsSortMethod
  -- | The current sorting direction for the positions.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PositionsFilterModel

instance Default PositionsFilterModel where
  def = PositionsFilterModel
    { offerAsset = ""
    , askAsset = ""
    , sortingMethod = PositionsTime
    , sortingDirection = SortDescending
    }

-------------------------------------------------
-- Dex State
-------------------------------------------------
data DexModel = DexModel
  -- | The current subscene.
  { scene :: DexScene
  -- | The currently focused `DexWallet` from the list of tracked `DexWallet`s.
  , selectedWallet :: DexWallet
  -- | The stake wallet to possibly use for the new dex wallet. This enables previewing
  -- the stake wallet's info before confirming.
  , newSwapCredential :: Maybe StakeWallet
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the delete wallet widget should be open.
  , deletingWallet :: Bool
  -- | Whether the trading pair widget should be open.
  , choosingTradingPair :: Bool
  -- | Whether the creating limit order widget should be open, or the creating liquidity swap 
  -- widget should be open.
  , creatingLimitOrder :: Bool
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | The selected trading pair.
  , selectedTradingPair :: Maybe (OfferAsset, AskAsset)
  -- | The new trading pair information.
  , newTradingPair :: (Text,Text)
  -- | The new swap creation. This is used for both limit orders and liquidity swaps.
  , newSwapCreation :: NewSwapCreation
  -- | The new swap update. This is used for both limit order updates and liquidity swap updates.
  , newSwapUpdate :: Maybe (SwapUTxO,NewSwapCreation)
  -- | Cached order-books.
  , cachedOrderBooks :: CachedOrderBooks
  -- | The current ask page for the order book.
  , currentAskPage :: Int
  -- | The current bid page for the order book.
  , currentBidPage :: Int
  -- | How many results to display per page in the order book.
  , orderBookSampleSize :: Int
  -- | Whether to show the filter widget for positions.
  , showPositionsFilter :: Bool
  -- | The positions filter model.
  , positionsFilterModel :: PositionsFilterModel
  -- | Teh positions filter scene.
  , positionsFilterScene :: FilterScene
  } deriving (Eq,Show)

makeFieldLabelsNoPrefix ''DexModel

instance Default DexModel where
  def = DexModel
    { scene = DexPositions
    , selectedWallet = def
    , newSwapCredential = Nothing
    , addingWallet = False
    , deletingWallet = False
    , choosingTradingPair = False
    , creatingLimitOrder = True
    , showMorePopup= False
    , selectedTradingPair = Nothing
    , newTradingPair = ("","")
    , newSwapCreation = def
    , newSwapUpdate = Nothing
    , cachedOrderBooks = mempty
    , orderBookSampleSize = 8
    , currentAskPage = 0
    , currentBidPage = 0
    , showPositionsFilter = False
    , positionsFilterModel = def
    , positionsFilterScene = def
    }
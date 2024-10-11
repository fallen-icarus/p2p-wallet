{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Resell scene is dedicated to Cardano-Aftermarket `MarketWallet`.

-}
module P2PWallet.Data.AppModel.AftermarketModel
  ( AftermarketScene(..)
  , AftermarketEvent(..)
  , AftermarketModel(..)

  , module P2PWallet.Data.AppModel.AftermarketModel.BidderModel
  , module P2PWallet.Data.AppModel.AftermarketModel.SellerModel
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.AftermarketModel.BidderModel
import P2PWallet.Data.AppModel.AftermarketModel.SellerModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Prelude

-------------------------------------------------
-- Aftermarket Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Resell page.
data AftermarketScene
  -- | Information for the staking credential used as a seller.
  = AftermarketSellerScene
  -- | Information for the staking credential used as a bidder.
  | AftermarketBidderScene
  deriving (Eq,Show)

-------------------------------------------------
-- Aftermarket Page Events
-------------------------------------------------
-- | The possible UI events on the Resell page.
data AftermarketEvent
  -- | Change the Resell scene to the specified scene.
  = ChangeAftermarketScene AftermarketScene
  -- | Add a new market wallet using one of the known staking credentials.
  | AddNewAftermarketWallet (AddEvent MarketWallet MarketWallet)
  -- | Delete a market wallet.
  | DeleteAftermarketWallet (DeleteWithConfirmationEvent MarketWallet)
  -- | Open the more popup widget
  | ShowAftermarketMorePopup
  -- | Sell Event.
  | AftermarketSellerEvent AftermarketSellerEvent
  -- | Lookup the info for the Keys in a specific sale. The bool is whether to force resyncing the
  -- info.
  | LookupKeyInfo (Bool,AftermarketUTxO)

-------------------------------------------------
-- Aftermarket State
-------------------------------------------------
data AftermarketModel = AftermarketModel
  -- | The current subscene.
  { scene :: AftermarketScene
  -- | The currently focused `MarketWallet` from the list of tracked `MarketWallet`s.
  , selectedWallet :: MarketWallet
  -- | The stake wallet to possibly use for the new market wallet. This enables previewing
  -- the stake wallet's info before confirming.
  , targetStakeCredential :: Maybe StakeWallet
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the delete wallet widget should be open.
  , deletingWallet :: Bool
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | The seller model.
  , sellerModel :: AftermarketSellerModel
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AftermarketModel

instance Default AftermarketModel where
  def = AftermarketModel
    { scene = AftermarketSellerScene
    , selectedWallet = def
    , targetStakeCredential = Nothing
    , addingWallet = False
    , deletingWallet = False
    , showMorePopup = False
    , sellerModel = def
    }

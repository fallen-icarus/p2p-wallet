{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Delegation scene is dedicated to `StakeWallet`s.

-}
module P2PWallet.Data.AppModel.Delegation where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Wallets.StakeWallet
import P2PWallet.Prelude

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Delegation page.
data DelegationScene
  -- | Information about the stake wallet as well as access to adding new stake wallets.
  = DelegationSummary
  -- | Registered stake pools available to delegate to.
  | DelegationPools
  deriving (Eq,Show)

-------------------------------------------------
-- Delegation Page Events
-------------------------------------------------
-- | The possible UI events on the Home page.
data DelegationEvent
  -- | Change the Delegation subscene to the specified subscene.
  = ChangeDelegationScene DelegationScene
  -- | Pair a new `StakeWallet`. It can only be done from the `HomeAbout` subscene.
  | PairStakeWallet (AddEvent StakeWallet)
  -- | Watch a new `StakeWallet`. It can only be done from the `HomeAbout` subscene.
  | WatchStakeWallet (AddEvent StakeWallet)
  -- | Change a payment wallet name.
  | ChangeStakeWalletName (AddEvent Text)
  -- | Delete a payment wallet.
  | DeleteStakeWallet (DeleteWithConfirmationEvent StakeWallet)
  -- | Open the more popup widget
  | ShowDelegationMorePopup

-------------------------------------------------
-- Delegation State
-------------------------------------------------
data DelegationModel = DelegationModel
  -- | The current subscene.
  { scene :: DelegationScene 
  -- | The currently focused `StakeWallet` from the list of tracked `StakeWallet`s.
  , selectedWallet :: StakeWallet
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the edit wallet widget should be open.
  , editingWallet :: Bool
  -- | Whether the delete wallet widget should be open.
  , deletingWallet :: Bool
  -- | The information for the new `StakeWallet` being added.
  , newStakeWallet :: NewStakeWallet
  } deriving (Eq,Show)

makeFieldLabelsNoPrefix ''DelegationModel

instance Default DelegationModel where
  def = DelegationModel 
    { scene = DelegationSummary
    , selectedWallet = def 
    , showMorePopup = False
    , addingWallet = False
    , editingWallet = False
    , deletingWallet = False
    , newStakeWallet = def
    }

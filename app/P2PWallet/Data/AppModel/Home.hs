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
    }

makeFieldLabelsNoPrefix ''HomeModel

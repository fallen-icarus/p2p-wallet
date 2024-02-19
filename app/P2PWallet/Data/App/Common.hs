{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.App.Common where

import P2PWallet.Prelude

-------------------------------------------------
-- Add a new `PaymentWallet`
-------------------------------------------------
-- | Add wallet UI steps for a wallet of type `a`.
data AddWalletEvent a
  -- | Open the widget for getting the new wallet information.
  = StartAdding
  -- | Close the widget and reset the new wallet information in the form.
  | CancelAdding
  -- | Keep the widget open, validate the information, and (if pairing) try to get the required 
  -- information from the hardware wallet. If anything fails, display an error message to the 
  -- user. The widget is kept open to allow the user to quickly try again.
  | ConfirmAdding
  -- | Process the new wallet information and add it to the tracked wallets.
  | AddResult a
  deriving (Show,Eq)


{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler.HomeEvent
  ( 
    handleHomeEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Prelude

handleHomeEvent :: AppModel -> HomeEvent -> [AppEventResponse AppModel AppEvent]
handleHomeEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeHomeScene newScene -> 
    [ Model $ model & #homeModel % #scene .~ newScene ]

  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowMorePopup -> 
    [ Model $ model & #homeModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Pairing Wallets
  -----------------------------------------------
  PairPaymentWallet modal -> case modal of
    -- A paired payment wallet is an address using a hardware wallet payment key and 
    -- possibly a hardware wallet stake key. Scripts are not part of payment wallets.
    StartAdding -> 
      -- Set `pairing` to `True` to display the widget for getting the new payment wallet info.
      -- Also reset the `newPaymentWallet` field so that the last information is cleared.
      [ Model $ model & #homeModel % #addingWallet .~ True -- Show widget.
                      & #homeModel % #newPaymentWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new payment wallet info.
      [ Model $ model & #homeModel % #addingWallet .~ False ]
    ConfirmAdding -> 
      -- Set `waitingOnDevice` to `True` so that users know to check their hardware wallet.
      -- Get the information from the hardware wallet and generate the addresses.
      [ Model $ model & #waitingOnDevice .~ True
      , Task $ runActionOrAlert (HomeEvent . PairPaymentWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def $ model ^. #selectedProfile
              newWallet = model ^. #homeModel % #newPaymentWallet

          -- Get the new payment id for the new entry into the payment_wallet table.
          paymentId <- getNextPaymentId databaseFile >>= fromRightOrAppError
          
          -- Validate the new payment wallet info, and export the required keys.
          verifiedPaymentWallet <- pairPaymentWallet network profile paymentId newWallet

          -- Add the new payment wallet to the database.
          addNewPaymentWallet databaseFile verifiedPaymentWallet >>= fromRightOrAppError

          return verifiedPaymentWallet
      ]
    AddResult verifiedPaymentWallet -> 
       [ Model $
          model & #knownWallets % #paymentWallets %~ flip snoc verifiedPaymentWallet
                & #waitingOnDevice .~ False
                & #homeModel % #addingWallet .~ False
                & #homeModel % #selectedWallet .~ verifiedPaymentWallet
                & #scene .~ HomeScene
       , Task $ return $ SyncWallets StartSync
       ]

  -----------------------------------------------
  -- Watching Wallets
  -----------------------------------------------
  WatchPaymentWallet modal -> case modal of
    -- A watched payment wallet can be any kind of payment wallet. However, it cannot be used
    -- to sign since only signing with hardware wallets is supported.
    StartAdding -> 
      -- Set `addingWallet` to `True` to display the widget for getting the new payment wallet info.
      -- Also reset the `newPaymentWallet` field so that the last information is cleared.
      [ Model $ model & #homeModel % #addingWallet .~ True -- Show widget.
                      & #homeModel % #newPaymentWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new payment wallet info.
      [ Model $ model & #homeModel % #addingWallet .~ False ]
    ConfirmAdding -> 
      -- Validate the information and generate the stake address, if any.
      [ Task $ runActionOrAlert (HomeEvent . WatchPaymentWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def $ model ^. #selectedProfile
              newWallet = model ^. #homeModel % #newPaymentWallet

          -- Get the new payment id for the new entry into the payment_wallet table.
          paymentId <- getNextPaymentId databaseFile >>= fromRightOrAppError
          
          -- Validate the new payment wallet info, and extract the stake address, if any.
          verifiedPaymentWallet <- watchPaymentWallet network profile paymentId newWallet

          -- Add the new payment wallet to the database.
          addNewPaymentWallet databaseFile verifiedPaymentWallet >>= fromRightOrAppError

          return verifiedPaymentWallet
      ]
    AddResult verifiedPaymentWallet -> 
       [ Model $
          model & #knownWallets % #paymentWallets %~ flip snoc verifiedPaymentWallet
                & #homeModel % #addingWallet .~ False
                & #homeModel % #selectedWallet .~ verifiedPaymentWallet
                & #scene .~ HomeScene
       , Task $ return $ SyncWallets StartSync
       ]

  -----------------------------------------------
  -- Change Payment Wallet Name
  -----------------------------------------------
  ChangePaymentWalletName modal -> case modal of
    -- Show the edit widget and set the extraTextField to the current alias.
    StartAdding -> 
      [ Model $ model & #homeModel % #editingWallet .~ True
                      & #homeModel % #showMorePopup .~ False
                      & #extraTextField .~ (homeModel ^. #selectedWallet % #alias)
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info and reset the text field.
      [ Model $ model & #homeModel % #editingWallet .~ False 
                      & #extraTextField .~ ""
      ]
    ConfirmAdding ->
      -- The state is deliberately not updated in case there is an error with any of these steps.
      -- The state will be updated after everything has successfully executed.
      [ Task $ runActionOrAlert (HomeEvent . ChangePaymentWalletName . AddResult) $ do
          let currentWallet = model ^. #homeModel % #selectedWallet
              -- Get the row id for the payment wallet being updated.
              currentId = currentWallet ^. #paymentId
              -- Filter out the selected profile from the list of known payment wallets.
              otherWallets = filter (\p -> currentId /= p ^. #paymentId) $
                knownWallets ^. #paymentWallets
              newAlias = model ^. #extraTextField
              newWallet = currentWallet & #alias .~ newAlias

          -- Check if the alias name is already being used.
          when (newAlias == "") $ 
            throwIO $ AppError "New name is empty."
          when (any (\w -> w ^. #alias == newAlias) otherWallets) $ 
            throwIO $ AppError "Name is already being used."

          -- Overwrite the current payment wallet name.
          addNewPaymentWallet databaseFile newWallet >>= fromRightOrAppError

          return newAlias
      ] 
    AddResult newAlias ->
      let currentWallet = model ^. #homeModel % #selectedWallet
          -- Get the row id for the payment wallet being updated.
          currentId = currentWallet ^. #paymentId
          -- Filter out the selected profile from the list of known payment wallets.
          otherWallets = filter (\p -> currentId /= p ^. #paymentId) $
            knownWallets ^. #paymentWallets
          newWallet = currentWallet & #alias .~ newAlias
          newWallets = sortOn (view #paymentId) $ newWallet : otherWallets
      -- Toggle the editingWallet flag.
      in [ Model $ 
             model & #knownWallets % #paymentWallets .~ newWallets
                   & #homeModel % #editingWallet .~ False
                   & #homeModel % #selectedWallet .~ newWallet
                   & #extraTextField .~ ""
         ]

  -----------------------------------------------
  -- Delete Payment Wallet
  -----------------------------------------------
  DeletePaymentWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation -> 
      [ Model $ model & #homeModel % #deletingWallet .~ True
                      & #homeModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #homeModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      [ Task $ runActionOrAlert (const $ HomeEvent $ DeletePaymentWallet PostDeletionAction) $ do
          -- Get the payment id for the payment wallet to delete.
          let currentId = model ^. #homeModel % #selectedWallet % #paymentId

          -- Delete the payment wallet.
          deletePaymentWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Get the payment id for the payment wallet to delete.
      let currentId = model ^. #homeModel % #selectedWallet % #paymentId
          newWallets = filter (\w -> w ^. #paymentId /= currentId) $
            model ^. #knownWallets % #paymentWallets
      in [ Model $ model & #homeModel % #deletingWallet .~ False
                         & #knownWallets % #paymentWallets .~ newWallets
                         & #homeModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
         ]

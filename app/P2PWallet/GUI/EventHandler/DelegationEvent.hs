{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler.DelegationEvent
  ( 
    handleDelegationEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.Database
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Prelude

handleDelegationEvent :: AppModel -> DelegationEvent -> [AppEventResponse AppModel AppEvent]
handleDelegationEvent model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Open the More Popup
  -----------------------------------------------
  ShowDelegationMorePopup -> 
    [ Model $ model & #delegationModel % #showMorePopup .~ True ]

  -----------------------------------------------
  -- Change the pool sort method
  -----------------------------------------------
  ChangePoolPickerSortMethod method -> 
    [ Model $ model & #delegationModel % #poolFilterModel % #sortMethod .~ method ]

  -----------------------------------------------
  -- Open the Pool Picker Widget
  -----------------------------------------------
  OpenPoolPicker ->
    [ Model $ model & #delegationModel % #showPoolPicker .~ True
    , Task $ 
        if model ^. #delegationModel % #registeredPools == [] then
          return $ SyncRegisteredPools StartSync
        else 
          return AppInit
    ]

  -----------------------------------------------
  -- Pairing Wallets
  -----------------------------------------------
  PairStakeWallet modal -> case modal of
    -- A paired stake wallet is an address using a hardware wallet stake key and 
    -- possibly a hardware wallet stake key. Scripts are not part of paired stake wallets.
    StartAdding _ -> 
      -- Set `pairing` to `True` to display the widget for getting the new stake wallet info.
      -- Also reset the `newStakeWallet` field so that the last information is cleared.
      [ Model $ model & #delegationModel % #addingWallet .~ True -- Show widget.
                      & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new stake wallet info.
      [ Model $ model & #delegationModel % #addingWallet .~ False ]
    ConfirmAdding -> 
      -- Set `waitingOnDevice` to `True` so that users know to check their hardware wallet.
      -- Get the information from the hardware wallet and generate the addresses.
      [ Model $ model & #waitingOnDevice .~ True
      , Task $ runActionOrAlert (DelegationEvent . PairStakeWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def $ model ^. #selectedProfile
              newWallet = model ^. #delegationModel % #newStakeWallet

          -- Get the new stake id for the new entry into the stake_wallet table.
          stakeId <- getNextStakeId databaseFile >>= fromRightOrAppError
          
          -- Validate the new stake wallet info, and export the required key.
          verifiedStakeWallet <- pairStakeWallet network profile stakeId newWallet

          -- Add the new payment wallet to the database.
          addNewStakeWallet databaseFile verifiedStakeWallet >>= fromRightOrAppError

          return verifiedStakeWallet
      ]
    AddResult verifiedStakeWallet -> 
       [ Model $
          model & #knownWallets % #stakeWallets %~ flip snoc verifiedStakeWallet
                & #waitingOnDevice .~ False
                & #delegationModel % #addingWallet .~ False
                & #delegationModel % #selectedWallet .~ verifiedStakeWallet
                & #scene .~ DelegationScene
       , Task $ return $ SyncWallets StartSync
       ]

  -----------------------------------------------
  -- Watching Wallets
  -----------------------------------------------
  WatchStakeWallet modal -> case modal of
    -- A watched stake wallet can be any kind of stake wallet. However, it cannot be used
    -- to sign since only signing with hardware wallets is supported.
    StartAdding _ -> 
      -- Set `addingWallet` to `True` to display the widget for getting the new wallet info.
      -- Also reset the `newStakeWallet` field so that the last information is cleared.
      [ Model $ model & #delegationModel % #addingWallet .~ True -- Show widget.
                      & #delegationModel % #newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new wallet info.
      [ Model $ model & #delegationModel % #addingWallet .~ False ]
    ConfirmAdding -> 
      -- Validate the information.
      [ Task $ runActionOrAlert (DelegationEvent . WatchStakeWallet . AddResult) $ do
          let network = config ^. #network
              profile = fromMaybe def $ model ^. #selectedProfile
              newWallet = model ^. #delegationModel % #newStakeWallet

          -- Get the new stake id for the new entry into the stake_wallet table.
          stakeId <- getNextStakeId databaseFile >>= fromRightOrAppError
          
          -- Validate the new wallet info.
          verifiedStakeWallet <- watchStakeWallet network profile stakeId newWallet

          -- Add the new wallet to the database.
          addNewStakeWallet databaseFile verifiedStakeWallet >>= fromRightOrAppError

          return verifiedStakeWallet
      ]
    AddResult verifiedStakeWallet -> 
       [ Model $
          model & #knownWallets % #stakeWallets %~ flip snoc verifiedStakeWallet
                & #delegationModel % #addingWallet .~ False
                & #delegationModel % #selectedWallet .~ verifiedStakeWallet
                & #scene .~ DelegationScene
       , Task $ return $ SyncWallets StartSync
       ]

  -----------------------------------------------
  -- Change Stake Wallet Name
  -----------------------------------------------
  ChangeStakeWalletName modal -> case modal of
    -- Show the edit widget and set the extraTextField to the current alias.
    StartAdding _ -> 
      [ Model $ model & #delegationModel % #editingWallet .~ True
                      & #delegationModel % #showMorePopup .~ False
                      & #extraTextField .~ (delegationModel ^. #selectedWallet % #alias)
      ]
    CancelAdding -> 
      -- Close the widget for getting the new info and reset the text field.
      [ Model $ model & #delegationModel % #editingWallet .~ False 
                      & #extraTextField .~ ""
      ]
    ConfirmAdding ->
      -- The state is deliberately not updated in case there is an error with any of these steps.
      -- The state will be updated after everything has successfully executed.
      [ Task $ runActionOrAlert (DelegationEvent . ChangeStakeWalletName . AddResult) $ do
          let currentWallet = model ^. #delegationModel % #selectedWallet
              -- Get the row id for the stake wallet being updated.
              currentId = currentWallet ^. #stakeId
              -- Filter out the selected wallet from the list of known stake wallets.
              otherWallets = filter (\p -> currentId /= p ^. #stakeId) $
                knownWallets ^. #stakeWallets
              newAlias = model ^. #extraTextField
              newWallet = currentWallet & #alias .~ newAlias

          -- Check if the alias name is already being used.
          when (newAlias == "") $ 
            throwIO $ AppError "New name is empty."
          when (any (\w -> w ^. #alias == newAlias) otherWallets) $ 
            throwIO $ AppError "Name is already being used."

          -- Overwrite the current stake wallet name.
          addNewStakeWallet databaseFile newWallet >>= fromRightOrAppError

          return newAlias
      ] 
    AddResult newAlias ->
      let currentWallet = model ^. #delegationModel % #selectedWallet
          -- Get the row id for the wallet being updated.
          currentId = currentWallet ^. #stakeId
          -- Filter out the selected wallet from the list of known wallets.
          otherWallets = filter (\p -> currentId /= p ^. #stakeId) $
            knownWallets ^. #stakeWallets
          newWallet = currentWallet & #alias .~ newAlias
          newWallets = sortOn (view #stakeId) $ newWallet : otherWallets
      in [ Model $ 
             model & #knownWallets % #stakeWallets .~ newWallets
                   & #delegationModel % #editingWallet .~ False
                   & #delegationModel % #selectedWallet .~ newWallet
                   & #extraTextField .~ ""
         ]

  -----------------------------------------------
  -- Delete Stake Wallet
  -----------------------------------------------
  DeleteStakeWallet modal -> case modal of
    -- Show the confirmation widget.
    GetDeleteConfirmation _ -> 
      [ Model $ model & #delegationModel % #deletingWallet .~ True
                      & #delegationModel % #showMorePopup .~ False
      ]
    CancelDeletion -> 
      -- Close the widget for confirming deletion.
      [ Model $ model & #delegationModel % #deletingWallet .~ False ]
    ConfirmDeletion ->
      [ Task $ runActionOrAlert (const $ DelegationEvent $ DeleteStakeWallet PostDeletionAction) $ do
          -- Get the stake id for the stake wallet to delete.
          let currentId = model ^. #delegationModel % #selectedWallet % #stakeId

          -- Delete the stake wallet.
          deleteStakeWallet databaseFile currentId >>= fromRightOrAppError
      ]
    PostDeletionAction ->
      -- Get the stake id for the stake wallet to delete.
      let currentId = model ^. #delegationModel % #selectedWallet % #stakeId
          newWallets = filter (\w -> w ^. #stakeId /= currentId) $
            model ^. #knownWallets % #stakeWallets
      in [ Model $ model & #delegationModel % #deletingWallet .~ False
                         & #knownWallets % #stakeWallets .~ newWallets
                         & #delegationModel % #selectedWallet .~ fromMaybe def (maybeHead newWallets)
         ]

  -----------------------------------------------
  -- Reset Filters
  -----------------------------------------------
  ResetPoolFilters -> 
    [ Model $ model & #delegationModel % #poolFilterModel .~ def
                    & #forceRedraw %~ not -- this is needed to force redrawing upon resets 
    ]

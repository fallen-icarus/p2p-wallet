{-# LANGUAGE DuplicateRecordFields #-}

module P2PWallet.GUI.EventHandler.HomeEvent
  ( 
    handleHomeEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.BackupFiles
import P2PWallet.Actions.Utils
import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Data.Plutus
import P2PWallet.Prelude

handleHomeEvent
  :: AppModel
  -> HomeEvent
  -> [AppEventResponse AppModel AppEvent]
handleHomeEvent model evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeHomeScene newScene -> 
    [ Model $ model & homeModel . scene .~ newScene ]

  -----------------------------------------------
  -- Home Details
  -----------------------------------------------
  ShowHomeDetails specific -> 
    -- Display the information for the selected item.
    [ Model $ model & homeModel . details .~ Just specific ]

  CloseHomeDetails -> 
    -- Close the information for the selected item and reset all `expandedFields`.
    [ Model $ model & homeModel . details .~ Nothing 
                    & homeModel . expandedFields .~ def
    ]

  -----------------------------------------------
  -- Filtering Home Information
  -----------------------------------------------
  FilterHomeAssets modal -> case modal of
    StartFiltering -> 
      -- Enable `filteringAssets` to show the asset filter widget. Set the `newFilters` to the
      -- `setFilters` to allow editing the current filters.
      [ Model $ model & homeModel . filteringAssets .~ True 
                      & homeModel . newFilters . assetFilters .~ 
                          (model ^. homeModel . setFilters . assetFilters)
      ]
    CancelFiltering -> 
      -- Disable `filteringAssets` to close the asset filter widget. Reset the `newFilters` to
      -- be the `setFilters` for next editing.
      [ Model $ model & homeModel . filteringAssets .~ False 
                      & homeModel . newFilters . assetFilters .~ 
                          (model ^. homeModel . setFilters . assetFilters)
      ]
    ResetFiltering -> 
      -- Disable `filteringAssets` and reset both the `setFilters` and the `newFilters`.
      [ Model $ model & homeModel . filteringAssets .~ False
                      & homeModel . setFilters . assetFilters .~ def
                      & homeModel . newFilters . assetFilters .~ def
      ]
    VerifyFilters ->
      -- Before confirming the filters, check if they are valid. Throw an appropriate error
      -- message if they are not.
      [ Task $
          either
            (return . Alert)
            (return . HomeEvent . FilterHomeAssets . ConfirmFilters)
            (toVerifiedFilters $ model ^. homeModel . newFilters)
      ]
    ConfirmFilters verifiedFilters -> 
      -- Disable `filteringAssets` and assign the `newFilters` to the `setFilters`.
      [ Model $ model & homeModel . filteringAssets .~ False
                      & homeModel . setFilters . assetFilters .~ (verifiedFilters ^. assetFilters)
      ]

  FilterHomeTransactions modal -> case modal of
    StartFiltering -> 
      -- Enable `filteringTxs` to show the tx filter widget. Set the `newFilters` to the
      -- `setFilters` to allow editing the current filters.
      [ Model $ model & homeModel . filteringTxs .~ True 
                      & homeModel . newFilters . txFilters .~ 
                          (model ^. homeModel . setFilters . txFilters)
      ]
    CancelFiltering -> 
      -- Disable `filteringTxs` to close the tx filter widget. Reset the `newFilters` to
      -- be the `setFilters` for next editing.
      [ Model $ model & homeModel . filteringTxs .~ False 
                      & homeModel . newFilters . txFilters .~ 
                          (model ^. homeModel . setFilters . txFilters)
      ]
    ResetFiltering -> 
      -- Disable `filteringTxs` and reset both the `setFilters` and the `newFilters`.
      [ Model $ model & homeModel . filteringTxs .~ False
                      & homeModel . setFilters . txFilters .~ def
                      & homeModel . newFilters . txFilters .~ def
      ]
    VerifyFilters ->
      -- Before confirming the filters, check if they are valid. Throw an appropriate error
      -- message if they are not.
      [ Task $
          either
            (return . Alert)
            (return . HomeEvent . FilterHomeTransactions . ConfirmFilters)
            (toVerifiedFilters $ model ^. homeModel . newFilters)
      ]
    ConfirmFilters verifiedFilters -> 
      -- Disable `filteringTxs` and assign the `newFilters` to the `setFilters`.
      [ Model $ model & homeModel . filteringTxs .~ False
                      & homeModel . setFilters . txFilters .~ (verifiedFilters ^. txFilters)
      ]

  FilterHomeUTxOs modal -> case modal of
    StartFiltering -> 
      -- Enable `filteringUtxos` to show the utxo filter widget. Set the `newFilters` to the
      -- `setFilters` to allow editing the current filters.
      [ Model $ model & homeModel . filteringUtxos .~ True
                      & homeModel . newFilters . utxoFilters .~ 
                          (model ^. homeModel . setFilters . utxoFilters)
      ]
    CancelFiltering -> 
      -- Disable `filteringUtxos` to close the utxo filter widget. Reset the `newFilters` to
      -- be the `setFilters` for next editing.
      [ Model $ model & homeModel . filteringUtxos .~ False 
                      & homeModel . newFilters . utxoFilters .~ 
                          (model ^. homeModel . setFilters . utxoFilters)
      ]
    ResetFiltering -> 
      -- Disable `filteringUtxos` and reset both the `setFilters` and the `newFilters`.
      [ Model $ model & homeModel . filteringUtxos .~ False
                      & homeModel . setFilters . utxoFilters .~ def
                      & homeModel . newFilters . utxoFilters .~ def
      ]
    VerifyFilters ->
      -- Before confirming the filters, check if they are valid. Throw an appropriate error
      -- message if they are not.
      [ Task $
          either
            (return . Alert)
            (return . HomeEvent . FilterHomeUTxOs . ConfirmFilters)
            (toVerifiedFilters $ model ^. homeModel . newFilters)
      ]
    ConfirmFilters verifiedFilters -> 
      -- Disable `filteringUtxos` and assign the `newFilters` to the `setFilters`.
      [ Model $ model & homeModel . filteringUtxos .~ False
                      & homeModel . setFilters . utxoFilters .~ (verifiedFilters ^. utxoFilters)
      ]

  -----------------------------------------------
  -- Pairing Wallets
  -----------------------------------------------
  PairPaymentWallet modal -> case modal of
    -- A paired payment wallet is an address using a hardware wallet payment key and 
    -- possibly a hardware wallet stake key. Scripts are not part of payment wallets.
    StartAdding -> 
      -- Set `pairing` to `True` to display the widget for getting the new payment wallet info.
      -- Also reset the `newPaymentWallet` field so that the last information is cleared.
      [ Model $ model & homeModel . pairing .~ True -- Show widget.
                      & homeModel . newPaymentWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new payment wallet info.
      [ Model $ model & homeModel . pairing .~ False ]
    ConfirmAdding -> 
      -- Set `waitingOnDevice` to `True` so that users know to check their hardware wallet.
      -- Get the information from the hardware wallet and generate the addresses.
      [ Model $ model & waitingOnDevice .~ True
      , Task $
          let network' = model ^. config . network
              newWallet = model ^. homeModel . newPaymentWallet
          in runActionOrAlert 
               (HomeEvent . PairPaymentWallet . AddResult)
               (pairPaymentWallet network' newWallet)
      ]
    AddResult w -> 
      -- Disable `pairing` and `waitingOnDevice`. Update the list of known wallets and change
      -- the scene to display the new wallet on the home page. Also backup and sync
      -- the new list of wallets so that the pairing is saved for the next application start.
      case processPaymentWallet w (model ^. wallets) of
        Left err -> [ Task $ return $ Alert err ]
        Right updatedWallets ->
          let network' = model ^. config . network
          in [ Model $ 
                 model & homeModel . pairing .~ False 
                       & waitingOnDevice .~ False
                       & homeModel . selectedWallet .~ w
                       & scene .~ HomeScene
                       & wallets .~ updatedWallets
             , Task $ do
                 backupWallets network' updatedWallets
                 return $ SyncWallets StartSync
             ]

  -----------------------------------------------
  -- Watching Wallets
  -----------------------------------------------
  WatchPaymentWallet modal -> case modal of
    -- A watched payment wallet can be any kind of payment wallet. However, it cannot be used
    -- to sign since only signing with hardware wallets is supported.
    StartAdding -> 
      -- Set `watching` to `True` to display the widget for getting the new payment wallet info.
      -- Also reset the `newPaymentWallet` field so that the last information is cleared.
      [ Model $ model & homeModel . watching .~ True -- Show widget.
                      & homeModel . newPaymentWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new payment wallet info.
      [ Model $ model & homeModel . watching .~ False ]
    ConfirmAdding -> 
      -- Validate the information and generate the stake address, if any.
      [ Task $
          let network' = model ^. config . network
              newWallet = model ^. homeModel . newPaymentWallet
          in runActionOrAlert 
               (HomeEvent . WatchPaymentWallet . AddResult)
               (watchPaymentWallet network' newWallet)
      ]
    AddResult w -> 
      -- Disable `watching`. Update the list of known wallets and change
      -- the scene to display the new wallet on the home page. Also backup and sync
      -- the new list of wallets so that the new wallet is saved for the next application start.
      case processPaymentWallet w (model ^. wallets) of
        Left err -> [ Task $ return $ Alert err ]
        Right updatedWallets ->
          let network' = model ^. config . network
          in [ Model $ 
                 model & homeModel . watching .~ False 
                       & homeModel . selectedWallet .~ w
                       & scene .~ HomeScene
                       & wallets .~ updatedWallets
             , Task $ do
                 backupWallets network' updatedWallets
                 return $ SyncWallets StartSync
             ]

  -----------------------------------------------
  -- Quick Actions
  -----------------------------------------------
  -- Called from the details widget.
  QuickSpend utxoRef' ->
    -- Add an input to the TxBuilderModel for the specified utxo. 
    -- This will also close the details page and move the user to the transaction summary page.
    let newIn = UserInput
          { _utxoRef = showTxOutRef utxoRef'
          }
    in [ Model $ model & homeModel . details .~ Nothing
                       & txBuilderModel . newInput .~ (0,newIn)
                       & scene .~ TxBuilderScene
       , Task $ return $ TxBuilderEvent InsertNewInput
       ]

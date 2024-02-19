module P2PWallet.GUI.EventHandler.DelegationEvent
  ( 
    handleDelegationEvent
  ) where

import Monomer

import P2PWallet.Actions.AddWallet
import P2PWallet.Actions.BackupFiles
import P2PWallet.Actions.Utils
import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude

handleDelegationEvent
  :: AppModel
  -> DelegationEvent
  -> [AppEventResponse AppModel AppEvent]
handleDelegationEvent model evt = case evt of
  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeDelegationScene newScene -> 
    -- If the new scene is `DelegatedPools` and there are currently no known registered pools,
    -- sync the pools after changing the scene. This sync should only happen automatically
    -- the first time.
    [ Model $ model & delegationModel . scene .~ newScene 
    , Task $ do
        if [] == (model ^. registeredPools) && newScene == DelegationPools
          then return $ SyncRegisteredPools StartSync
          else return AppInit
    ]

  -----------------------------------------------
  -- Delegation Details
  -----------------------------------------------
  ShowDelegationDetails specific -> 
    -- Display the information for the selected item.
    [ Model $ model & delegationModel . details .~ Just specific ]

  CloseDelegationDetails -> 
    -- Close the information for the selected item.
    [ Model $ model & delegationModel . details .~ Nothing ]

  -----------------------------------------------
  -- Pairing Wallets
  -----------------------------------------------
  PairStakeWallet modal -> case modal of
    -- A paired stake wallet is an address using a hardware wallet stake key.
    StartAdding -> 
      -- Set `pairing` to `True` to display the widget for getting the new stake wallet info.
      -- Also reset the `newPaymentWallet` field so that the last information is cleared.
      [ Model $ model & delegationModel . pairing .~ True -- Show widget.
                      & delegationModel . newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new stake wallet info.
      [ Model $ model & delegationModel . pairing .~ False ]
    ConfirmAdding -> 
      -- Set `waitingOnDevice` to `True` so that users know to check their hardware wallet.
      -- Get the information from the hardware wallet and generate the addresses.
      [ Model $ model & waitingOnDevice .~ True
      , Task $
          let network' = model ^. config . network
              newWallet = model ^. delegationModel . newStakeWallet
          in runActionOrAlert 
               (DelegationEvent . PairStakeWallet . AddResult)
               (pairStakeWallet network' newWallet)
      ]
    AddResult w -> 
      -- Disable `pairing` and `waitingOnDevice`. Update the list of known wallets and change
      -- the scene to display the new wallet on the summary page. Also backup and sync
      -- the new list of wallets so that the pairing is saved for the next application start.
      case processStakeWallet w (model ^. wallets) of
        Left err -> [ Task $ return $ Alert err ]
        Right updatedWallets ->
          let network' = model ^. config . network
          in [ Model $ 
                 model & delegationModel . pairing .~ False 
                       & waitingOnDevice .~ False
                       & delegationModel . selectedWallet .~ w
                       & scene .~ DelegationScene
                       & wallets .~ updatedWallets
             , Task $ do
                 backupWallets network' updatedWallets
                 return $ SyncWallets StartSync
             ]

  -----------------------------------------------
  -- Watching Wallets
  -----------------------------------------------
  WatchStakeWallet modal -> case modal of
    -- A watched stake wallet can be any kind of stake wallet. However, it cannot be used
    -- to sign since only signing with hardware wallets is supported.
    StartAdding -> 
      -- Set `watching` to `True` to display the widget for getting the new stake wallet info.
      -- Also reset the `newStakeWallet` field so that the last information is cleared.
      [ Model $ model & delegationModel . watching .~ True -- Show widget.
                      & delegationModel . newStakeWallet .~ def -- Clear information.
      ]
    CancelAdding -> 
      -- Close the widget for getting the new stake wallet info.
      [ Model $ model & delegationModel . watching .~ False ]
    ConfirmAdding -> 
      -- Validate the information and generate the stake address, if any.
      [ Task $
          let network' = model ^. config . network
              newWallet = model ^. delegationModel . newStakeWallet
          in runActionOrAlert 
               (DelegationEvent . WatchStakeWallet . AddResult)
               (watchStakeWallet network' newWallet)
      ]
    AddResult w -> 
      -- Disable `watching`. Update the list of known wallets and change
      -- the scene to display the new wallet on the delegation page. Also backup and sync
      -- the new list of wallets so that the new wallet is saved for the next application start.
      case processStakeWallet w (model ^. wallets) of
        Left err -> [ Task $ return $ Alert err ]
        Right updatedWallets ->
          let network' = model ^. config . network
          in [ Model $ 
                 model & delegationModel . watching .~ False 
                       & delegationModel . selectedWallet .~ w
                       & scene .~ DelegationScene
                       & wallets .~ updatedWallets
             , Task $ do
                 backupWallets network' updatedWallets
                 return $ SyncWallets StartSync
             ]

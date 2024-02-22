{-# LANGUAGE DuplicateRecordFields #-}

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
        if [] == (model ^. delegationModel . registeredPools) && newScene == DelegationPools
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
              profile' = fromMaybe def $ model ^. selectedProfile
              newWallet = model ^. delegationModel . newStakeWallet
          in runActionOrAlert 
               (DelegationEvent . PairStakeWallet . AddResult)
               (pairStakeWallet network' (profile' ^. accountIndex) newWallet)
      ]
    AddResult w -> 
      -- Disable `pairing` and `waitingOnDevice`. Update the list of known wallets and change
      -- the scene to display the new wallet on the summary page. Also backup and sync
      -- the new list of wallets so that the pairing is saved for the next application start.
      case processStakeWallet w (model ^. wallets) of
        Left err -> [ Task $ return $ Alert err ]
        Right updatedWallets ->
          let network' = model ^. config . network
              profile' = fromMaybe def $ model ^. selectedProfile
          in [ Model $ 
                 model & delegationModel . pairing .~ False 
                       & waitingOnDevice .~ False
                       & delegationModel . selectedWallet .~ w
                       & scene .~ DelegationScene
                       & wallets .~ updatedWallets
             , Task $ do
                 backupWallets network' profile' updatedWallets
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
              profile' = fromMaybe def $ model ^. selectedProfile
          in [ Model $ 
                 model & delegationModel . watching .~ False 
                       & delegationModel . selectedWallet .~ w
                       & scene .~ DelegationScene
                       & wallets .~ updatedWallets
             , Task $ do
                 backupWallets network' profile' updatedWallets
                 return $ SyncWallets StartSync
             ]

  -----------------------------------------------
  -- Filtering Registered Pools
  -----------------------------------------------
  FilterRegisteredPools modal -> case modal of
    StartFiltering -> 
      -- Enable `filteringRegisteredPools` to show the pool filter widget. If the currently
      -- set filters is [], then show a default implementation of the filter language. Otherwise, 
      -- show the current filters.
      let vf@(VerifiedPoolFilters setFilters') = model ^. delegationModel . setPoolFilters
          newFilters' = if setFilters' == [] then def else fromVerifiedPoolFilters vf
      in
        [ Model $ model & delegationModel . filteringRegisteredPools .~ True 
                        & delegationModel . newPoolFilters .~ newFilters'
        ]
    CancelFiltering -> 
      -- Disable `filteringRegisteredPools` to close the pool filter widget. 
      [ Model $ model & delegationModel . filteringRegisteredPools .~ False ]
    ResetFiltering -> 
      -- Disable `filteringRegisteredPools` and reset the `setPoolFilters`.
      [ Model $ model & delegationModel . filteringRegisteredPools .~ False
                      & delegationModel . setPoolFilters .~ def
      ]
    VerifyFilters ->
      -- Before confirming the filters, check if they are valid. Throw an appropriate error
      -- message if they are not.
      [ Task $
          either
            (return . Alert)
            (return . DelegationEvent . FilterRegisteredPools . ConfirmFilters)
            (toVerifiedPoolFilters $ model ^. delegationModel . newPoolFilters)
      ]
    ConfirmFilters verifiedFilters -> 
      -- Disable `filteringRegisteredPools` and assign the `newFilters` to the `setFilters`.
      [ Model $ model & delegationModel . filteringRegisteredPools .~ False
                      & delegationModel . setPoolFilters .~ verifiedFilters
      ]

  -----------------------------------------------
  -- Quick Actions
  -----------------------------------------------
  -- Called from the details widget.
  QuickDelegate poolId' ->
    -- Add a delegation certificate to the TxBuilderModel for the specified pool and the
    -- currently selected stake wallet. This will also close the details page and move
    -- the user to the transaction summary page.
    let newCert = UserCertificate
          { _internalWallet = Just $ model ^. delegationModel . selectedWallet
          , _stakeAddress = ""
          , _certificateAction = Delegation poolId'
          }
    in [ Model $ model & delegationModel . details .~ Nothing
                       & txBuilderModel . newCertificate .~ (0,newCert)
                       & scene .~ TxBuilderScene
       , Task $ return $ TxBuilderEvent InsertNewCertificate
       ]

  -- Called from the summary widget.
  QuickWithdraw ->
    -- Add the currently selected wallet to the newWithdrawal in the txBuilderModel and
    -- take the user to the widget for getting the withdrawal amount.
    let newWtdr = UserWithdrawal
          { _internalWallet = Just $ model ^. delegationModel . selectedWallet
          , _stakeAddress = ""
          , _lovelaces = 0
          }
    in [ Model $ model & txBuilderModel . newWithdrawal .~ (0,newWtdr)
                       & scene .~ TxBuilderScene
                       & txBuilderModel . scene .~ BuilderAddNewWithdrawal
       ]

  -- Called from the summary widget.
  QuickRegister ->
    -- Add a registration certificate to the TxBuilderModel for the currently selected stake 
    -- wallet. This will also move the user to the transaction summary page.
    let newCert = UserCertificate
          { _internalWallet = Just $ model ^. delegationModel . selectedWallet
          , _stakeAddress = ""
          , _certificateAction = Registration
          }
    in [ Model $ model & delegationModel . details .~ Nothing
                       & txBuilderModel . newCertificate .~ (0,newCert)
                       & scene .~ TxBuilderScene
       , Task $ return $ TxBuilderEvent InsertNewCertificate
       ]

module P2PWallet.GUI.EventHandler
  ( 
    handleEvent
  ) where

import Monomer

import P2PWallet.Actions.BackupFiles
import P2PWallet.Actions.LookupPools
import P2PWallet.Actions.SignTx
import P2PWallet.Actions.SubmitTx
import P2PWallet.Actions.SyncWallets
import P2PWallet.Actions.Utils
import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.GUI.EventHandler.DelegationEvent
import P2PWallet.GUI.EventHandler.HomeEvent
import P2PWallet.GUI.EventHandler.TxBuilderEvent
import P2PWallet.Prelude

handleEvent
  :: AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt = case evt of
  -----------------------------------------------
  -- Application Start and "Do nothing" event.
  -----------------------------------------------
  -- Aside from starting the application, this is sometimes useful for when a widget requires 
  -- an event but you don't actually want to trigger an event. An example would be a read-only
  -- implementation of `Monomer.textFieldV_`.
  AppInit -> []

  -----------------------------------------------
  -- Alert Messages
  -----------------------------------------------
  -- Alerts are used for both error messages and status updates.
  --
  -- Note on errors: Even though Monomer allows disabling widgets if certain criteria is not
  -- satisfied, when several criteria must be met, it is not always clear what is not satisfied.
  -- To improve the user experience, the "submit" widgets will always be enabled and an alert
  -- message will shown when the input is invalid. The alert message will explain to the user
  -- what is wrong. IMO this is a much better UX.
  Alert msg -> 
    -- Disable all overlays and display the message. The scene is not changed so users can 
    -- quickly try again if desired. 
    [ Model $ model & alertMessage .~ Just msg
                    & waitingOnDevice .~ False
                    & syncingWallets .~ False
                    & syncingPools .~ False
                    & building .~ False
                    & submitting .~ False
    ]
  CloseAlertMessage -> 
    -- Close the alert widget and reset the alert message.
    [ Model $ model & alertMessage .~ Nothing ]

  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  ChangeMainScene newScene -> 
    [ Model $ model & scene .~ newScene ]

  -----------------------------------------------
  -- Changing the current profile
  -----------------------------------------------
  -- Change the currently selected profile and load the new wallets. If there profile has
  -- existing wallets, sync them.
  ChangeProfile modal -> case modal of
    -- Remove the current profile and open the profile picker widget.
    LogoutCurrentProfile ->
      [ Model $ model & selectedProfile .~ Nothing ]
    -- Set the new profile as the selectedProfile. Then try to load the wallets for that profile.
    LoadNewProfile newProfile' ->
      [ Model $ model & selectedProfile .~ Just newProfile'
      , Task $
          runActionOrAlert
            (ChangeProfile . LoadWalletsResult)
            (loadWallets (model ^. config . network) newProfile' >>= fromRightOrAppError)
      ]
    -- Initialize the model with the wallet information for the profile. Take the user
    -- to the home page.
    LoadWalletsResult wallets' ->
      [ Model $ model & wallets .~ wallets'
                      & homeModel . selectedWallet .~
                          fromMaybe def (maybeHead $ wallets' ^. paymentWallets)
                      & delegationModel . selectedWallet .~
                          fromMaybe def (maybeHead $ wallets' ^. stakeWallets)
                      & scene .~ HomeScene
      , Task $
          if wallets' == def
          then return AppInit
          else return $ SyncWallets StartSync
      ]

  -----------------------------------------------
  -- Adding new profiles
  -----------------------------------------------
  AddNewProfile modal -> case modal of
    -- Show the addNewProfile widget, clear the old information, and initialize the accountIndex
    -- with the next index.
    StartAdding -> 
      let newIdx = length $ model ^. knownProfiles
      in [ Model $ model & newProfile . accountIndex .~ newIdx
                         & newProfile . alias .~ ""
                         & scene .~ NewProfileScene
                         & addingProfile .~ True
         ]
    CancelAdding -> 
      -- Close the widget for getting the new info.
      [ Model $ model & addingProfile .~ False 
                      & scene .~ ProfilePickerScene
      ]
    ConfirmAdding -> 
      -- Check if the profile name or accountIndex is already in use.
      case processNewProfile (model ^. newProfile) (model ^. knownProfiles) of
        Left err -> [ Task $ return $ Alert err ]
        Right newProfile' -> [ Task $ return $ AddNewProfile $ AddResult newProfile' ]
    AddResult newProfile' ->
      -- Backup the new profile and take the user to the home page. Also toggle the 
      -- addingProfile flag. Initialize the new profile's wallet file.
      let network' = model ^. config . network
          newProfiles = sortOn (view accountIndex) $ newProfile' : model ^. knownProfiles
      in [ Model $ 
             model & selectedProfile .~ Just newProfile'
                   & knownProfiles .~ newProfiles
                   & addingProfile .~ False
                   & scene .~ HomeScene
         , Task $ do
             backupProfiles network' newProfiles
             backupWallets network' newProfile' def
             return AppInit
         ]

  -----------------------------------------------
  -- Syncing Wallets
  -----------------------------------------------
  SyncWallets modal -> case modal of
    StartSync -> 
      -- Set `syncing` to True to let users know syncing is happening.
      [ Model $ model & syncingWallets .~ True 
      , Task $ 
          let network' = model ^. config . network
              wallets' = model ^. wallets
          in runActionOrAlert
              (SyncWallets . SyncResults)
              (syncWallets network' wallets')
      ]
    SyncResults resp -> 
      -- Disable `syncing` and update the list of wallets. Also update the information for
      -- the `selectedWallet`.
      let paymentTarget = model ^. homeModel . selectedWallet . alias
          updatedPaymentTarget = 
            fromMaybe def $ find (\w -> w ^. alias == paymentTarget) $ resp ^. paymentWallets
          stakeTarget = model ^. delegationModel . selectedWallet . alias
          updatedStakeTarget = 
            fromMaybe def $ find (\w -> w ^. alias == stakeTarget) $ resp ^. stakeWallets
      in
        [ Model $ 
            model & syncingWallets .~ False
                  & wallets .~ resp
                  & homeModel . selectedWallet .~ updatedPaymentTarget
                  & delegationModel . selectedWallet .~ updatedStakeTarget
        ]

  -----------------------------------------------
  -- Syncing Registered Pools
  -----------------------------------------------
  SyncRegisteredPools modal -> case modal of
    StartSync -> 
      -- Set `syncing` to True to let users know syncing is happening.
      [ Model $ model & syncingPools .~ True 
      , Task $ 
          let network' = model ^. config . network
          in runActionOrAlert
              (SyncRegisteredPools . SyncResults)
              (lookupRegisteredPools network')
      ]
    SyncResults resp -> 
      -- Disable `syncing` and update the list of pools. 
      [ Model $ 
          model & syncingPools .~ False
                & delegationModel . registeredPools .~ resp
      ]

  -----------------------------------------------
  -- Home Events
  -----------------------------------------------
  HomeEvent modal -> handleHomeEvent model modal

  -----------------------------------------------
  -- Delegation Events
  -----------------------------------------------
  DelegationEvent modal -> handleDelegationEvent model modal

  -----------------------------------------------
  -- TxBuilder Events
  -----------------------------------------------
  TxBuilderEvent modal -> handleTxBuilderEvent model modal

  -----------------------------------------------
  -- Sign Transaction
  -----------------------------------------------
  SignTx ->
    -- Signing the transaction can only be done if `isBuilt` is set to True since that means
    -- the tx.body file in the tmp directory is up-to-date. If there is an error, it will be shown
    -- in the `alertMessage`. Signing assumes the same hardware wallet seed phrase manages all
    -- relevant keys for the transaction.
    if not $ model ^. txBuilderModel . isBuilt
    then [ Task $ return $ Alert "You must first build the transaction."]
    else
      [ Model $ model & waitingOnDevice .~ True
      , Task $
          runActionOrAlert SubmitTx $ 
            signTx (model ^. config . network) (model ^. txBuilderModel)
      ]

  -----------------------------------------------
  -- Submit Transaction
  -----------------------------------------------
  SubmitTx signedFile ->
    -- Submitting the transaction can only be done if `isBuilt` is set to True since that means
    -- the tx.signed file in the tmp directory is up-to-date. If there is an error, it will be shown
    -- in the `alertMessage`.
    [ Model $ model & submitting .~ True
                    & waitingOnDevice .~ False -- Disable this since it can be called from `SignTx`.
    , Task $
        if not $ model ^. txBuilderModel . isBuilt
        then return $ Alert "You must first build the transaction."
        else runActionOrAlert Alert $ submitTx (model ^. config . network) signedFile
    ]

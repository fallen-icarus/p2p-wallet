{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.EventHandler
  ( 
    handleEvent
  ) where

import Monomer

import P2PWallet.Actions.Database
import P2PWallet.Actions.SyncWallets
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.GUI.EventHandler.HomeEvent
import P2PWallet.GUI.EventHandler.ProfileEvent
import P2PWallet.Prelude

handleEvent
  :: AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _ _ model@AppModel{..} evt = case evt of
  -----------------------------------------------
  -- Application Start and "Do nothing" event.
  -----------------------------------------------
  -- Aside from starting the application, this is sometimes useful for when a widget requires 
  -- an event, but you don't actually want to trigger an event. An example would be a read-only
  -- implementation of `Monomer.textFieldV_`.
  AppInit -> []

  -----------------------------------------------
  -- Copying Text
  -----------------------------------------------
  CopyText text -> 
    [ setClipboardData $ ClipboardText text
    , Task $ return $ Alert "Successfully copied to clipboard!"
    ]

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
    [ Model $ model & #alertMessage .~ Just msg 
                    & #waitingOnDevice .~ False
                    & #syncingWallets .~ False
                    & #loadingWallets .~ False
    ]
  CloseAlertMessage -> 
    -- Close the alert widget and reset the alert message.
    [ Model $ model & #alertMessage .~ Nothing ]

  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  -- Change the main scene while trying to leave the `AppModel` alone. This way, users can switch
  -- between scenes without losing their place when building transactions.
  ChangeMainScene newScene -> 
    [ Model $ model & #scene .~ newScene ]

  -----------------------------------------------
  -- Set Network
  -----------------------------------------------
  -- Set the app config to that network and load the profiles associated with that network.
  SetNetwork network' -> 
    [ Model $ model & #config % #network .~ network'
    , Task $ runActionOrAlert (ProfileEvent . LoadKnownProfiles) $
        -- Try to load the profiles from the sqlite database. Show the user any error that appears.
        -- The resulting profiles are passed to `LoadKnownProfiles`.
        loadProfiles databaseFile network' >>= fromRightOrAppError
    ]

  -----------------------------------------------
  -- Profile Events
  -----------------------------------------------
  ProfileEvent modal -> handleProfileEvent model modal

  -----------------------------------------------
  -- Home Events
  -----------------------------------------------
  HomeEvent modal -> handleHomeEvent model modal

  -----------------------------------------------
  -- Syncing Wallets
  -----------------------------------------------
  SyncWallets modal -> case modal of
    StartSync -> 
      -- Set `syncing` to True to let users know syncing is happening.
      [ Model $ model & #syncingWallets .~ True 
      , Task $ do
          let network = config ^. #network
              wallets = model ^. #knownWallets
          runActionOrAlert (SyncWallets . SyncResults) $ 
            syncWallets (model ^. #databaseFile) network wallets
      ]
    SyncResults resp ->
      -- Disable `syncing` and update the list of wallets. Also update the information for
      -- the `selectedWallet`.
      let paymentTarget = model ^. #homeModel % #selectedWallet % #alias
          updatedPaymentTarget = 
            fromMaybe def $ find (\w -> w ^. #alias == paymentTarget) $ resp ^. #paymentWallets
      in
        [ Model $ 
            model & #syncingWallets .~ False
                  & #knownWallets .~ resp
                  & #homeModel % #selectedWallet .~ updatedPaymentTarget
        , Task $
            runActionOrAlert UpdateCurrentDate $ getCurrentDay (config ^. #timeZone)
        ]

  -----------------------------------------------
  -- Updating the current date
  -----------------------------------------------
  UpdateCurrentDate day -> 
    [ Model $ model & #config % #currentDay .~ day ]


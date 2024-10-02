module P2PWallet.GUI.EventHandler
  ( 
    handleEvent
  ) where

import Monomer
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as Time

import P2PWallet.Actions.SubmitTx
import P2PWallet.Actions.SyncWallets
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Wallets
import P2PWallet.GUI.EventHandler.AddressBookEvent
import P2PWallet.GUI.EventHandler.DelegationEvent
import P2PWallet.GUI.EventHandler.DexEvent
import P2PWallet.GUI.EventHandler.HomeEvent
import P2PWallet.GUI.EventHandler.LendingEvent
import P2PWallet.GUI.EventHandler.OptionsEvent
import P2PWallet.GUI.EventHandler.ProfileEvent
import P2PWallet.GUI.EventHandler.TickerRegistryEvent
import P2PWallet.GUI.EventHandler.TxBuilderEvent
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
  -- To improve the user experience, the "submit" widgets will usually be enabled and an alert
  -- message will shown when the input is invalid. The alert message will explain to the user
  -- what is wrong. IMO this is a much better UX.
  Alert msg -> 
    -- Disable all overlays and display the message. The scene is not changed so users can 
    -- quickly try again if desired. 
    [ Model $ model 
        & #alertMessage ?~ msg 
        & #waitingStatus .~ def
        & #txBuilderModel %~
            -- Clear the tx builder, but only if the submission was successfull.
            if "Submission successfull!" `Text.isPrefixOf` msg then const def else id
    ]
  CloseAlertMessage -> 
    -- Close the alert widget and reset the alert message.
    [ Model $ model & #alertMessage .~ Nothing ]

  -----------------------------------------------
  -- Changing Scenes
  -----------------------------------------------
  -- Change the main scene while leaving the `AppModel` alone. This way, users can switch
  -- between scenes without losing their place when building transactions.
  ChangeMainScene newScene -> 
    [ Model $ model & #scene .~ newScene ]

  -----------------------------------------------
  -- Set Network
  -----------------------------------------------
  -- Set the app config to that network and load the profiles associated with that network.
  SetNetwork targetNetwork -> 
    [ Model $ model & #config % #network .~ targetNetwork
    , Task $ return $ ProfileEvent $ LoadKnownProfiles $ StartProcess Nothing
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
  -- Delegation Events
  -----------------------------------------------
  DelegationEvent modal -> handleDelegationEvent model modal

  -----------------------------------------------
  -- Ticker Registry Events
  -----------------------------------------------
  TickerRegistryEvent modal -> handleTickerRegistryEvent model modal

  -----------------------------------------------
  -- Address Book Events
  -----------------------------------------------
  AddressBookEvent modal -> handleAddressBookEvent model modal

  -----------------------------------------------
  -- Dex Events
  -----------------------------------------------
  DexEvent modal -> handleDexEvent model modal

  -----------------------------------------------
  -- Loans Events
  -----------------------------------------------
  LendingEvent modal -> handleLendingEvent model modal

  -----------------------------------------------
  -- Options Events
  -----------------------------------------------
  OptionsEvent modal -> handleOptionsEvent model modal

  -----------------------------------------------
  -- TxBuilder Events
  -----------------------------------------------
  TxBuilderEvent modal -> handleTxBuilderEvent model modal

  -----------------------------------------------
  -- Updating the current date
  -----------------------------------------------
  -- This is always called after syncing so it will handle any new notifications.
  UpdateCurrentDate modal -> case modal of
    StartProcess _ -> 
      [ Task $ runActionOrAlert (UpdateCurrentDate . ProcessResults) $ 
          (,) <$> getCurrentDay (config ^. #timeZone)
              <*> Time.getPOSIXTime
      ]
    ProcessResults (day,time) ->
      [ Model $ model 
          & #config % #currentDay .~ day
          & #config % #currentTime .~ time
      , Task $ 
          if notifications /= [] then do
            return $ Alert $ unlines
              [ "You have new notifications!"
              , "Go to the News page to check them out."
              ]
          else return AppInit
      ]

  -----------------------------------------------
  -- Syncing Wallets
  -----------------------------------------------
  SyncWallets modal -> case modal of
    StartProcess _ -> 
      -- Set `syncing` to True to let users know syncing is happening.
      [ Model $ model & #waitingStatus % #syncingWallets .~ True 
      , Task $ do
          runActionOrAlert (SyncWallets . ProcessResults) $ 
            syncWallets databaseFile (config ^. #network) knownWallets
      ]
    ProcessResults (resp@Wallets{..}, networkParams, newNotifications) ->
      -- Disable `syncing` and update the list of wallets. Also update the information for
      -- the `selectedWallet`.
      let paymentTarget = model ^. #homeModel % #selectedWallet % #paymentWalletId
          updatedPaymentTarget = 
            fromMaybe def $ find (\w -> w ^. #paymentWalletId == paymentTarget) paymentWallets
          stakeTarget = model ^. #delegationModel % #selectedWallet % #stakeWalletId
          updatedStakeTarget = 
            fromMaybe def $ find (\w -> w ^. #stakeWalletId == stakeTarget) stakeWallets
          dexTarget = model ^. #dexModel % #selectedWallet % #dexWalletId
          updatedDexTarget = 
            fromMaybe def $ find (\w -> w ^. #dexWalletId == dexTarget) dexWallets
          loanTarget = model ^. #lendingModel % #selectedWallet % #loanWalletId
          updatedLoanTarget = 
            fromMaybe def $ find (\w -> w ^. #loanWalletId == loanTarget) loanWallets
          optionsTarget = model ^. #optionsModel % #selectedWallet % #optionsWalletId
          updatedOptionsTarget = 
            fromMaybe def $ find (\w -> w ^. #optionsWalletId == optionsTarget) optionsWallets
      in  [ Model $ model 
              & #waitingStatus % #syncingWallets .~ False
              & #knownWallets .~ resp
              & #homeModel % #selectedWallet .~ updatedPaymentTarget
              & #delegationModel % #selectedWallet .~ updatedStakeTarget
              & #dexModel % #selectedWallet .~ updatedDexTarget
              & #lendingModel % #selectedWallet .~ updatedLoanTarget
              & #optionsModel % #selectedWallet .~ updatedOptionsTarget
              & #txBuilderModel % #parameters ?~ networkParams
              & #fingerprintMap .~ 
                  toFingerprintMap (concatMap (view #nativeAssets) paymentWallets)
              & #notifications .~ zip [0..] newNotifications
          -- `UpdateCurrentDate` will handle any new notifications alerts.
          , Task $ return $ UpdateCurrentDate $ StartProcess Nothing
          ]

  -----------------------------------------------
  -- Submit Transaction
  -----------------------------------------------
  SubmitTx signedFile ->
    [ Model $ model & #waitingStatus % #submitting .~ True
    , Task $ runActionOrAlert Alert $ submitTx (config ^. #network) signedFile
    ]

  -----------------------------------------------
  -- Toggle notification status
  -----------------------------------------------
  ToggleNotificationReadStatus idx ->
    [ Model $ model & #notifications % ix idx % _2 % #markedAsRead %~ not ]

  -----------------------------------------------
  -- Mark all notifications as read
  -----------------------------------------------
  MarkAllNotificationsAsRead ->
    [ Model $ model & #notifications %~ map (set (_2 % #markedAsRead) True) ]

module P2PWallet.GUI.UIBuilder
  (
    buildUI
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.AddressBook
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Delegation
import P2PWallet.GUI.Widgets.Dex
import P2PWallet.GUI.Widgets.Home
import P2PWallet.GUI.Widgets.Lending
import P2PWallet.GUI.Widgets.MainMenu
import P2PWallet.GUI.Widgets.Networks
import P2PWallet.GUI.Widgets.Notifications
import P2PWallet.GUI.Widgets.Options
import P2PWallet.GUI.Widgets.Profiles
import P2PWallet.GUI.Widgets.Settings
import P2PWallet.GUI.Widgets.TickerRegistry
import P2PWallet.GUI.Widgets.TxBuilder
import P2PWallet.Prelude

buildUI :: AppWenv -> AppModel -> AppNode
buildUI _ model@AppModel{..} = do
  let alertOverlay = customAlertMsg (fromMaybe "" alertMessage) CloseAlertMessage
      waitingOverlay caption =
        box (label caption `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor $ darkGray & #a .~ 0.8]

  zstack 
    [ networksWidget model `nodeVisible` (NetworksScene == scene)
    , profilesWidget model `nodeVisible` (ProfilesScene == scene)
    , hstack
        [ mainMenuWidget model
        , vstack
            [ settingsWidget model `nodeVisible` (SettingsScene == scene)
            , homeWidget model `nodeVisible` (HomeScene == scene)
            , delegationWidget model `nodeVisible` (DelegationScene == scene)
            , txBuilderWidget model `nodeVisible` (TxBuilderScene == scene)
            , addressBookWidget model `nodeVisible` (AddressBookScene == scene)
            , tickerRegistryWidget model `nodeVisible` (TickerRegistryScene == scene)
            , dexWidget model `nodeVisible` (DexScene == scene)
            , lendingWidget model `nodeVisible` (LendingScene == scene)
            , optionsWidget model `nodeVisible` (OptionsScene == scene)
            , notificationsWidget model `nodeVisible` (NotificationsScene == scene)
            ]
        ] `nodeVisible` isJust selectedProfile
    , alertOverlay `nodeVisible` isJust alertMessage
    , waitingOverlay "Waiting on Device..." `nodeVisible` waitingStatus ^. #waitingOnDevice
    , waitingOverlay "Syncing Wallets..." `nodeVisible` waitingStatus ^. #syncingWallets
    , waitingOverlay "Loading Profile..." `nodeVisible` waitingStatus ^. #loadingProfile
    , waitingOverlay "Syncing Pools..." `nodeVisible` waitingStatus ^. #syncingPools
    , waitingOverlay "Building..." `nodeVisible` waitingStatus ^. #building
    , waitingOverlay "Submitting Transaction..." `nodeVisible` waitingStatus ^. #submitting
    , waitingOverlay "Adding to Builder..." `nodeVisible` waitingStatus ^. #addingToBuilder
    , waitingOverlay "Syncing Order Book..." `nodeVisible` waitingStatus ^. #syncingOrderBook
    , waitingOverlay "Syncing Loan Requests..." `nodeVisible` waitingStatus ^. #syncingLoanAsks
    , waitingOverlay "Syncing Loan Offers..." `nodeVisible` waitingStatus ^. #syncingLoanOffers
    , waitingOverlay "Syncing Active Loans..." `nodeVisible` waitingStatus ^. #syncingActiveLoans
    , waitingOverlay "Syncing Loan History..." `nodeVisible` waitingStatus ^. #syncingLoanHistory
    , waitingOverlay "Syncing Borrower Info..." `nodeVisible` waitingStatus ^. #syncingBorrowerInfo
    ] `styleBasic` [bgColor customGray4]

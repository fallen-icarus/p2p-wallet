module P2PWallet.GUI.UIBuilder
  (
    buildUI
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.AddressBook
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Delegation
import P2PWallet.GUI.Widgets.Home
import P2PWallet.GUI.Widgets.MainMenu
import P2PWallet.GUI.Widgets.Networks
import P2PWallet.GUI.Widgets.Profiles
import P2PWallet.GUI.Widgets.Settings
import P2PWallet.GUI.Widgets.TickerRegistry
import P2PWallet.GUI.Widgets.TxBuilder
import P2PWallet.MonomerOptics()
import P2PWallet.Prelude

buildUI :: AppWenv -> AppModel -> AppNode
buildUI _ model = do
  let alertOverlay = customAlertMsg (fromMaybe "" $ model ^. #alertMessage) CloseAlertMessage
      waitingOverlay caption =
        box (label caption `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor $ darkGray & #a .~ 0.8]

  zstack 
    [ networksWidget model `nodeVisible` (NetworksScene == model ^. #scene)
    , profilesWidget model `nodeVisible` (ProfilesScene == model ^. #scene)
    , hstack
        [ mainMenuWidget model
        , vstack
            [ settingsWidget model `nodeVisible` (SettingsScene == model ^. #scene)
            , homeWidget model `nodeVisible` (HomeScene == model ^. #scene)
            , delegationWidget model `nodeVisible` (DelegationScene == model ^. #scene)
            , txBuilderWidget model `nodeVisible` (TxBuilderScene == model ^. #scene)
            , addressBookWidget model `nodeVisible` (AddressBookScene == model ^. #scene)
            , tickerRegistryWidget model `nodeVisible` (TickerRegistryScene == model ^. #scene)
            ]
        ] `nodeVisible` (isJust $ model ^. #selectedProfile)
    , alertOverlay `nodeVisible` (isJust $ model ^. #alertMessage)
    , waitingOverlay "Waiting on Device..." `nodeVisible` (model ^. #waitingOnDevice)
    , waitingOverlay "Syncing Wallets..." `nodeVisible` (model ^. #syncingWallets)
    , waitingOverlay "Loading Profile..." `nodeVisible` (model ^. #loadingProfile)
    , waitingOverlay "Syncing Pools..." `nodeVisible` (model ^. #syncingPools)
    , waitingOverlay "Building..." `nodeVisible` (model ^. #building)
    , waitingOverlay "Submitting Transaction..." `nodeVisible` (model ^. #submitting)
    ] `styleBasic` [bgColor customGray4]

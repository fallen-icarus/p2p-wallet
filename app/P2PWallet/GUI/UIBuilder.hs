module P2PWallet.GUI.UIBuilder
  (
    buildUI
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Networks
import P2PWallet.GUI.Widgets.Profiles
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Home
import P2PWallet.GUI.Widgets.MainMenu
import P2PWallet.GUI.Widgets.Settings
import P2PWallet.MonomerOptics()
import P2PWallet.Prelude

buildUI :: AppWenv -> AppModel -> AppNode
buildUI _ model = do
  let alertOverlay = customAlertMsg (fromMaybe "" $ model ^. #alertMessage) CloseAlertMessage
      waitingOnDeviceOverlay =
        box (label "Waiting on Device..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor $ darkGray & #a .~ 0.9]
      syncingWalletsOverlay = 
        box (label "Syncing Wallets..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor (darkGray & #a .~ 0.8)]

  zstack 
    [ networksWidget model `nodeVisible` (NetworksScene == model ^. #scene)
    , profilesWidget model `nodeVisible` (ProfilesScene == model ^. #scene)
    , hstack
        [ mainMenuWidget model
        , vstack
            [ settingsWidget model `nodeVisible` (SettingsScene == model ^. #scene)
            , homeWidget model `nodeVisible` (HomeScene == model ^. #scene)
            ]
        ] `nodeVisible` (isJust $ model ^. #selectedProfile)
    , alertOverlay `nodeVisible` (isJust $ model ^. #alertMessage)
    , waitingOnDeviceOverlay `nodeVisible` (model ^. #waitingOnDevice)
    , syncingWalletsOverlay `nodeVisible` (model ^. #syncingWallets)
    ] `styleBasic` [bgColor customGray4]

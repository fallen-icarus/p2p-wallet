module P2PWallet.GUI.UIBuilder
  (
    buildUI
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Delegation
import P2PWallet.GUI.Widgets.Home
import P2PWallet.GUI.Widgets.MainMenu
import P2PWallet.GUI.Widgets.Networks
import P2PWallet.GUI.Widgets.Profiles
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
      loadingWalletsOverlay = 
        box (label "Loading Wallets..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor (darkGray & #a .~ 0.8)]
      syncingPoolsOverlay = 
        box (label "Syncing Pools..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor (darkGray & #a .~ 0.8)]

  zstack 
    [ networksWidget model `nodeVisible` (NetworksScene == model ^. #scene)
    , profilesWidget model `nodeVisible` (ProfilesScene == model ^. #scene)
    , hstack
        [ mainMenuWidget model
        , vstack
            [ settingsWidget model `nodeVisible` (SettingsScene == model ^. #scene)
            , homeWidget model `nodeVisible` (HomeScene == model ^. #scene)
            , delegationWidget model `nodeVisible` (DelegationScene == model ^. #scene)
            ]
        ] `nodeVisible` (isJust $ model ^. #selectedProfile)
    , alertOverlay `nodeVisible` (isJust $ model ^. #alertMessage)
    , waitingOnDeviceOverlay `nodeVisible` (model ^. #waitingOnDevice)
    , syncingWalletsOverlay `nodeVisible` (model ^. #syncingWallets)
    , syncingPoolsOverlay `nodeVisible` (model ^. #syncingPools)
    , loadingWalletsOverlay `nodeVisible` (model ^. #loadingWallets)
    ] `styleBasic` [bgColor customGray4]

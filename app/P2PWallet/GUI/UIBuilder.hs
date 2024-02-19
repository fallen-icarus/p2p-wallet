module P2PWallet.GUI.UIBuilder
  (
    buildUI
  ) where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.GUI.Widgets.Delegation
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.MainMenu
import P2PWallet.GUI.Widgets.Home
import P2PWallet.GUI.Widgets.TxBuilder
import P2PWallet.Prelude

buildUI :: AppWenv -> AppModel -> AppNode
buildUI wenv model = do
  let waitingOnDeviceOverlay =
        box (label "Waiting on Device..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor $ darkGray & L.a .~ 0.9]
      syncingWalletsOverlay = 
        box (label "Syncing Wallets..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor (darkGray & L.a .~ 0.8)] where
      syncingPoolsOverlay = 
        box (label "Syncing Pools..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor (darkGray & L.a .~ 0.8)] where
      buildingOverlay =
        box (label "Building Transaction..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor $ darkGray & L.a .~ 0.9]
      submittingOverlay =
        box (label "Submitting Transaction..." `styleBasic` [textSize 20, textColor black])
          `styleBasic` [bgColor $ darkGray & L.a .~ 0.9]
      alertOverlay = customAlertMsg (fromMaybe "" $ model ^. alertMessage) CloseAlertMessage

  zstack 
    [ hstack 
        [ 
          mainMenuWidget wenv
        , delegationWidget wenv model `nodeVisible` (DelegationScene == model ^. scene)
        , homeWidget wenv model `nodeVisible` (HomeScene == model ^. scene)
        , txBuilderWidget wenv model `nodeVisible` (TxBuilderScene == model ^. scene)
        ]
    , alertOverlay `nodeVisible` isJust (model ^. alertMessage)
    , waitingOnDeviceOverlay `nodeVisible` (model ^. waitingOnDevice)
    , syncingWalletsOverlay `nodeVisible` (model ^. syncingWallets)
    , syncingPoolsOverlay `nodeVisible` (model ^. syncingPools)
    , submittingOverlay `nodeVisible` (model ^. submitting)
    , buildingOverlay `nodeVisible` (model ^. building)
    ]

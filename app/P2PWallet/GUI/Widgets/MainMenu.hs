module P2PWallet.GUI.Widgets.MainMenu where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

-- | A side bar for switching between the main app scenes.
mainMenuWidget :: AppWenv -> AppNode
mainMenuWidget wenv = do
  let sectionBgColor = wenv ^. L.theme . L.sectionColor

  vstack 
    [ spacer
    , customButtonWithName wenv "Home" remixHome4Line $ ChangeMainScene HomeScene
    , customButtonWithName wenv "Staking" remixMedalLine $ ChangeMainScene DelegationScene
    , customButtonWithName wenv "DEX" remixArrowLeftRightFill $ ChangeMainScene LimitOrders
    , customButtonWithName wenv "Liquidity" remixWaterFlashLine $ ChangeMainScene MarketMakers
    , customButtonWithName wenv "Tx Builder" remixToolsLine $ ChangeMainScene TxBuilderScene
    , filler
    , customButtonWithName wenv "Settings" remixSettings3Line $ ChangeMainScene SettingsScene
    , spacer
    ] `styleBasic` [bgColor sectionBgColor,radiusTR 5, radiusBR 5, width 60] 

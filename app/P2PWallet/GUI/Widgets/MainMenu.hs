module P2PWallet.GUI.Widgets.MainMenu 
  ( 
    mainMenuWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

-- | A side bar for switching between the main app scenes.
mainMenuWidget :: AppModel -> AppNode
mainMenuWidget model = do
    vstack 
      [ changeSceneButton "Home" homeIcon HomeScene
      , changeSceneButton "Staking" delegationCenterIcon DelegationScene
      -- , changeSceneButton "DEX" remixArrowLeftRightFill LimitOrders
      -- , changeSceneButton "Liquidity" remixWaterFlashLine MarketMakers
      -- , changeSceneButton "Builder" remixToolsLine TxBuilderScene
      , changeSceneButton "Settings" settingsIcon SettingsScene
      , filler
      , logOutButton
      , spacer
      ] `styleBasic` [bgColor customGray3, width 40] 

  where
    -- | A button with an icon, a label, and a transparent background.
    changeSceneButton :: Text -> Text -> MainScene -> AppNode
    changeSceneButton name remixIcon newScene = do
      let dormantColor
            | model ^. #scene == newScene = customBlue
            | otherwise = white
          btn = 
            vstack 
              [ centerWidgetH $ label remixIcon
                  `styleBasic`
                      [ textFont "Remix"
                      , paddingT 8
                      , paddingL 10
                      , paddingR 10
                      , paddingB 0
                      , textMiddle
                      , textSize 12
                      , textColor dormantColor
                      , border 0 transparent
                      ]
              , centerWidgetH $ label name
                  `styleBasic` 
                      [ paddingT 0
                      , paddingL 0
                      , paddingR 0
                      , paddingB 2
                      , textSize 8
                      , textColor dormantColor
                      ]
              ]
      box_ [onClick $ ChangeMainScene newScene] btn
        `styleBasic` [bgColor transparent , paddingB 5 , radius 5]
        `styleHover` [bgColor customGray2, cursorIcon CursorHand]

    -- | A button with an icon, a label, and a transparent background.
    logOutButton :: AppNode
    logOutButton = do
      let btn = 
            vstack 
              [ centerWidgetH $ label logoutIcon
                  `styleBasic`
                      [ textFont "Remix"
                      , paddingT 8
                      , paddingL 10
                      , paddingR 10
                      , paddingB 0
                      , textMiddle
                      , textSize 12
                      , border 0 transparent
                      ]
              , centerWidgetH $ label "Logout"
                  `styleBasic` 
                      [ paddingT 0
                      , paddingL 0
                      , paddingR 0
                      , paddingB 2
                      , textSize 8
                      ]
              ]
      box_ [onClick $ ProfileEvent LogoutProfile] btn
        `styleBasic` [paddingB 5, radius 5]
        `styleHover` [bgColor customGray2, cursorIcon CursorHand]

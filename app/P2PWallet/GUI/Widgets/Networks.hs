module P2PWallet.GUI.Widgets.Networks 
  ( 
    networksWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

networksWidget :: AppModel -> AppNode
networksWidget _ =
    centerWidget $
      vstack
        [ centerWidgetH $ label "Select a network:" 
            `styleBasic` [textFont "Medium", textSize 30]
        , spacer_ [width 100]
        , centerWidgetH $ flip styleBasic [border 1 black, padding 10, radius 5] $ 
            hstack_ [childSpacing] 
              [ networkBox Testnet
              , networkBox Mainnet 
              ]
        ]
  where
    networkBox :: Network -> AppNode
    networkBox network = do
      let content =
            vstack
              [ centerWidgetH $ label (display network)
                  `styleBasic` [textFont "Regular", textSize 24]
              ] `styleBasic` [width 200, padding 20, radius 5]
                `styleHover` [bgColor customBlue, cursorIcon CursorHand]
      box_ [expandContent, onClick $ SetNetwork network] content 
        `styleBasic` [bgColor customGray1, radius 5]

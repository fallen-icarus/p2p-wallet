{-# LANGUAGE DataKinds #-}

module P2PWallet.GUI.Widgets.TxBuilder.Internal where

import Monomer
import Data.Text qualified as Text

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.Prelude

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Trim the stake address or pool id so that they fit better on the same line.
trimBech32 :: (ToText a) => a -> Text
trimBech32 info = Text.take 25 text <> "..." <> Text.drop 35 text
  where
    text = toText info

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Text -> Color -> Double -> WidgetNode s AppEvent
copyableLabelSelf caption color fontSize = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize fontSize
      , border 0 transparent
      , textColor color
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

-- | A label button that will copy other data. The font size is configurable.
copyableLabelFor :: Double -> Color -> Text -> Text -> WidgetNode s AppEvent
copyableLabelFor fontSize color caption info = 
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText info)
        `styleBasic`
          [ padding 0
          , radius 5
          , textMiddle
          , border 0 transparent
          , textColor customBlue
          , bgColor transparent
          , textSize fontSize
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label_ info [ellipsis] `styleBasic` [textColor color, textSize fontSize]
    ]

copyableLabelSelfWith :: (ToText a) => Double -> (a -> Text) -> a -> Color -> WidgetNode s AppEvent
copyableLabelSelfWith fontSize modifier fullInfo color = do
  let formattedInfo = modifier fullInfo
  tooltip_ "Copy" [tooltipDelay 0] $ button formattedInfo (CopyText $ toText fullInfo)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , border 0 transparent
      , textColor color
      , bgColor transparent
      , textSize fontSize
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]
    
-------------------------------------------------
-- Helper Lens
-------------------------------------------------
toggleDetails 
  :: forall a. (LabelOptic "showDetails" A_Lens a a Bool Bool) 
  => Int -> Lens' [(Int,a)] Bool
toggleDetails idx = lens getToggleDetails setToggleDetails
  where
    getToggleDetails :: [(Int,a)] -> Bool
    getToggleDetails [] = False
    getToggleDetails ((i,u):us) =
      if i == idx
      then u ^. #showDetails
      else getToggleDetails us

    setToggleDetails :: [(Int,a)] -> Bool -> [(Int,a)]
    setToggleDetails [] _ = []
    setToggleDetails ((i,u):us) b =
      if i == idx
      then (i,u & #showDetails .~ b) : us
      else (i,u) : setToggleDetails us b

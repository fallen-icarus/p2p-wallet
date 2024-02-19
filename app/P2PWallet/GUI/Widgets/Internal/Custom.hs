module P2PWallet.GUI.Widgets.Internal.Custom 
  (
    copyableTextField
  , copyableTextArea
  , customButtonWithName
  , customButton
  , centerWidget
  , centerWidgetH
  , centerWidgetV
  , customAlertMsg
  ) where

import Monomer
import Monomer.Lens qualified as L
import Prettyprinter (vsep)
import Prettyprinter.Util (reflow)

import P2PWallet.Data.App
import P2PWallet.Prelude

-- | A read only text field to allow the user to copy the underlying text.
copyableTextField :: Text -> WidgetNode s AppEvent
copyableTextField text = 
  textFieldV_ text (const AppInit) [readOnly]
    `styleBasic` [paddingT 0, paddingB 0, bgColor transparent, border 0 transparent]

-- | A read only text are to allow the user to copy the underlying text.
-- If the text extends off the screen, a horizontal scroll bar will appear at the bottom of
-- the text area. Unfortunately, the scroll bar will cover any text at the bottom. To account
-- for this, there is extra space given to the text area as well as padding on top of the text
-- area to make it look symmetrical.
copyableTextArea :: Text -> WidgetNode s AppEvent
copyableTextArea text = do
  let h = fromIntegral $ length $ lines text
  textAreaV_ text (const AppInit) [readOnly]
    `styleBasic` 
      [ paddingT 8
      , paddingB 0
      , height $ h * 18.5 + 10
      , bgColor transparent
      , border 0 transparent
      ]

-- | A button with an icon, a transparent background, and a tooltip.
customButtonWithName
  :: AppWenv
  -> Text 
  -> Text 
  -> AppEvent 
  -> AppNode
customButtonWithName wenv name remixIcon evt = do
  let rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
      rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
      btn = 
        vstack 
          [ centerWidgetH $ label remixIcon
              `styleBasic` [
                  textFont "Remix", 
                  paddingT 10,
                  paddingL 10,
                  paddingR 10,
                  paddingB 0,
                  textMiddle, 
                  textColor rowButtonColor, 
                  bgColor transparent, 
                  border 0 transparent]
          , centerWidgetH $ label name `styleBasic` [padding 0, textSize 10]
          ]
  box_ [onClick evt] btn
    `styleBasic` [paddingB 10, radius 5]
    `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

-- | A button with an icon, a transparent background, and a tooltip.
customButton 
  :: AppWenv
  -> Text 
  -> Text 
  -> AppEvent 
  -> AppNode
customButton wenv tip remixIcon evt = do
  let rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
      rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
  tooltip_ tip [tooltipDelay 1000] $
    button remixIcon evt
      `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
      `styleBasic` [
          textFont "Remix", 
          padding 10,
          textMiddle, 
          textColor rowButtonColor, 
          bgColor transparent, 
          border 0 transparent]

-- | Center a widget both vertically and horizontally.
centerWidget :: WidgetNode s AppEvent -> WidgetNode s AppEvent
centerWidget w = centerWidgetV $ centerWidgetH w

-- | Center a widget vertically.
centerWidgetV :: WidgetNode s AppEvent -> WidgetNode s AppEvent
centerWidgetV w =
  vstack
    [ filler
    , w
    , filler
    ]

-- | Center a widget vertically.
centerWidgetH :: WidgetNode s AppEvent -> WidgetNode s AppEvent
centerWidgetH w =
  hstack
    [ filler
    , w
    , filler
    ]

-- | A custom alert box that allows the user to copy the alert message.
-- It will try to reflow the text so that it is not one long line.
customAlertMsg :: Text -> AppEvent -> WidgetNode AppModel AppEvent
customAlertMsg msg closeEvt = 
  alert closeEvt $ centerWidget $
    copyableTextArea $ show $ vsep $ map reflow $ lines msg

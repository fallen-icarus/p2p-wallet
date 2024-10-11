module P2PWallet.GUI.Widgets.Internal.Custom 
  (
    copyableTextField
  , copyableTextArea
  , centerWidget
  , centerWidgetH
  , centerWidgetV
  , cushionWidget
  , cushionWidgetH
  , cushionWidgetV
  , customAlertMsg
  ) where

import Monomer
import Prettyprinter (vsep)
import Prettyprinter.Util (reflow)

import P2PWallet.Data.AppModel
import P2PWallet.Prelude

-- | A read only text field to allow the user to copy the underlying text.
copyableTextField :: Text -> WidgetNode s AppEvent
copyableTextField text = 
  textFieldV_ text (const AppInit) [readOnly]
    `styleBasic` [paddingT 0, paddingB 0, bgColor transparent, border 0 transparent]

-- | A read only text area to allow the user to copy the underlying text.
copyableTextArea :: Text -> WidgetNode s AppEvent
copyableTextArea text = do
  -- let h = fromIntegral $ length $ lines text
  textAreaV_ text (const AppInit) [readOnly]
    `styleBasic` 
      [ padding 0
      , bgColor transparent
      , border 0 transparent
      ]

-- | Center a widget both vertically and horizontally.
centerWidget :: WidgetNode s AppEvent -> WidgetNode s AppEvent
centerWidget = centerWidgetV . centerWidgetH

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

-- | Add a single space on all sides of a widget.
cushionWidget :: WidgetNode s AppEvent -> WidgetNode s AppEvent
cushionWidget = cushionWidgetV . cushionWidgetH

-- | Add a single space on the top and the bottom of a widget.
cushionWidgetV :: WidgetNode s AppEvent -> WidgetNode s AppEvent
cushionWidgetV w =
  vstack
    [ spacer
    , w
    , spacer
    ]

-- | Add a single space to the left and the right of a widget.
cushionWidgetH :: WidgetNode s AppEvent -> WidgetNode s AppEvent
cushionWidgetH w =
  hstack
    [ spacer
    , w
    , spacer
    ]

-- | A custom alert box that allows the user to copy the alert message.
-- It will try to reflow the text so that it is not one long line.
customAlertMsg :: Text -> AppEvent -> WidgetNode AppModel AppEvent
customAlertMsg msg closeEvt = do
  let text = show $ vsep $ map reflow $ lines msg
  alert closeEvt $ centerWidget $ 
    textAreaV_ text (const AppInit) [readOnly]
      `styleBasic` 
        [ paddingT 8
        , paddingB 0
        , textSize 12
        , bgColor transparent
        , border 0 transparent
        ]

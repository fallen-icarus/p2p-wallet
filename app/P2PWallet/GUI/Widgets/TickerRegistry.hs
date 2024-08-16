module P2PWallet.GUI.Widgets.TickerRegistry
  (
    tickerRegistryWidget
  ) where

import Monomer hiding (decimals)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

tickerRegistryWidget :: AppModel -> AppNode
tickerRegistryWidget model
  | isListView = tickerRegistryListWidget model
  | otherwise = tickerRegistrySearchWidget model
  where
    isListView = model ^. #tickerRegistryModel % #listView

tickerRegistrySearchWidget :: AppModel -> AppNode
tickerRegistrySearchWidget AppModel{..} = do
    centerWidget $ vstack
      [ centerWidgetH $ hstack
          [ label "Ticker Registry"
              `styleBasic`
                [ padding 10
                , textSize 20
                , textFont "Italics"
                ]
          , tooltip_ "Switch to List View" [tooltipDelay 0] $
              button remixListCheck (TickerRegistryEvent SwitchTickerRegistryView)
                `styleBasic`
                  [ textFont "Remix"
                  , padding 0
                  , radius 5
                  , textMiddle
                  , border 0 transparent
                  , textColor $ if isChanging then darkGray else customBlue
                  , bgColor transparent
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                `nodeEnabled` not isChanging
          ]
      , spacer
      , centerWidgetH $ hstack
          [ label "Ticker:"
          , spacer_ [width 5]
          , textField_ (toLensVL $ #tickerRegistryModel % #search) 
                [ placeholder "DJED" ] 
              `styleBasic`
                [ textSize 12
                , width 200
                , bgColor customGray1
                , sndColor darkGray
                ]
              `styleDisabled` [textColor white]
              `styleFocus` [border 1 customBlue]
              `nodeEnabled` not isChanging
          ]
      , zstack
          [ widgetMaybe result resultWidget
              `nodeVisible` not isChanging
          , adaProhibitedWidget `nodeVisible` searchIsADA
          , unknownTickerWidget searchTarget
              `nodeVisible` and
                [ isNothing result
                , searchTarget /= ""
                , not isChanging
                , not searchIsADA
                ]
          , addingTickerWidget `nodeVisible` isAdding
          , editingTickerWidget `nodeVisible` isEditing
          , confirmDeleteWidget (tickerRegistryModel ^. #newTickerInfo % #ticker) 
              `nodeVisible` isDeleting
          ] `styleBasic`
              [ padding 10
              ]
      ] `styleBasic` 
          [ bgColor customGray3
          , padding 20
          , radius 20
          ]
  where
    isChanging :: Bool
    isChanging = or
      [ isAdding
      , isEditing
      , isDeleting
      ]

    isAdding :: Bool
    isAdding = tickerRegistryModel ^. #addingTicker

    isEditing :: Bool
    isEditing = tickerRegistryModel ^. #editingTicker

    isDeleting :: Bool
    isDeleting = tickerRegistryModel ^. #deletingTicker

    searchTarget :: Text
    searchTarget = tickerRegistryModel ^. #search

    searchIsADA :: Bool
    searchIsADA = searchTarget == "ADA"

    result :: Maybe (CurrencySymbol,TokenName,Word8)
    result = Map.lookup (Ticker searchTarget) tickerMap

    resultWidget :: (CurrencySymbol,TokenName,Word8) -> AppNode
    resultWidget (policyId,assetName,decimal) = do
      let fingerprint = mkAssetFingerprint policyId assetName
          newInfo = NewTickerInfo searchTarget (display policyId) (display assetName) decimal
      vstack
        [ centerWidgetH $ hstack
            [ tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon (TickerRegistryEvent $ ChangeTickerInfo $ StartAdding $ Just newInfo)
                  `styleBasic`
                    [ textFont "Remix"
                    , padding 0
                    , radius 5
                    , textMiddle
                    , border 0 transparent
                    , textColor customBlue
                    , bgColor transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer
            , separatorLine `styleBasic` [fgColor customGray1]
            , spacer
            , tooltip_ "Delete" [tooltipDelay 0] $
                button deleteIcon 
                  (TickerRegistryEvent $ DeleteTickerInfo $ GetDeleteConfirmation $ Just newInfo)
                  `styleBasic`
                    [ textFont "Remix"
                    , padding 0
                    , radius 5
                    , textMiddle
                    , border 0 transparent
                    , textColor customRed
                    , bgColor transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ] `styleBasic`
                [ bgColor customGray2
                , radius 20
                , padding 5
                ]
        , spacer
        , centerWidgetH $ vstack
            [ copyableLabelFor "Policy Id:" $ display policyId
            , spacer
            , copyableLabelFor "Asset Name:" $ display assetName
            , spacer
            , copyableLabelFor "Fingerprint:" $ display fingerprint
            , spacer
            , copyableLabelFor "Decimals:" $ show decimal
            ]
        ]

    addingTickerWidget :: AppNode
    addingTickerWidget = do
      vstack
        [ hstack 
            [ label "Policy Id:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #policyId) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Asset Name:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #assetName) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Decimals:"
            , spacer
            , numericField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #decimals)
                `styleBasic` [textSize 12, width 100, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ TickerRegistryEvent $ AddNewTickerInfo CancelAdding
            , spacer
            , mainButton "Save" $ TickerRegistryEvent $ AddNewTickerInfo ConfirmAdding
            ]
        ]

    editingTickerWidget :: AppNode
    editingTickerWidget = do
      vstack
        [ hstack 
            [ label "Policy Id:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #policyId) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Asset Name:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #assetName) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Decimals:"
            , spacer
            , numericField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #decimals)
                `styleBasic` [textSize 12, width 100, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ TickerRegistryEvent $ ChangeTickerInfo CancelAdding
            , spacer
            , mainButton "Save" $ TickerRegistryEvent $ ChangeTickerInfo ConfirmAdding
            ]
        ]

    confirmDeleteWidget :: Text -> AppNode
    confirmDeleteWidget tckr = do
      vstack
        [ centerWidgetH $ flip styleBasic [textColor lightGray] $ label $ mconcat
            [ "Are you sure you would like to delete the entry for '"
            , tckr
            , "'?"
            ]
        , spacer_ [width 20]
        , hstack 
            [ filler
            , button "Cancel" $ TickerRegistryEvent $ DeleteTickerInfo CancelDeletion
            , spacer
            , mainButton "Confirm" $ TickerRegistryEvent $ DeleteTickerInfo ConfirmDeletion
            ]
        ] `styleBasic` [bgColor customGray3, padding 20]

tickerRegistryListWidget :: AppModel -> AppNode
tickerRegistryListWidget AppModel{..} = do
    zstack
      [ centerWidget $ vstack
          [ centerWidgetH $ hstack
              [ label "Ticker Registry"
                  `styleBasic`
                    [ padding 10
                    , textSize 20
                    , textFont "Italics"
                    ]
              , tooltip_ "Switch to Search View" [tooltipDelay 0] $
                  button remixSearchLine (TickerRegistryEvent SwitchTickerRegistryView)
                    `styleBasic`
                      [ textFont "Remix"
                      , padding 0
                      , radius 5
                      , textMiddle
                      , border 0 transparent
                      , textColor customBlue
                      , bgColor transparent
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , spacer
          , centerWidgetH $ hstack
              [ label "Ticker:"
              , spacer_ [width 5]
              , textField_ (toLensVL $ #tickerRegistryModel % #search) [ placeholder "DJED" ] 
                  `styleBasic`
                    [ textSize 12
                    , width 200
                    , bgColor customGray1
                    , sndColor darkGray
                    ]
              , spacer_ [width 5]

              , box_ [onClick $ TickerRegistryEvent $ AddNewTickerInfo $ StartAdding Nothing] $
                  label largeAddIcon
                    `styleBasic` 
                      [ textSize 14
                      , textColor customBlue
                      , radius 10
                      , textMiddle
                      , paddingT 3
                      , paddingB 3
                      , paddingR 7
                      , paddingL 7
                      , bgColor customGray2
                      , border 0 transparent
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , widgetIf (sample /= []) $ cushionWidget $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map tickerRow sample)
                `styleBasic` 
                  [ padding 10
                  , paddingT 0
                  ]
          , centerWidget adaProhibitedWidget `nodeVisible` searchIsADA
          , widgetIf (null sample && allTickers /= [] && not searchIsADA) $ 
              centerWidget $ unknownTickerWidget searchTarget
          , widgetIf (null allTickers) $
              centerWidget $
                label "The ticker registry is empty"
                 `styleBasic` [textFont "Italics"]
          ] `styleBasic` 
              [ bgColor customGray4
              , padding 20
              , radius 20
              ]
            `nodeVisible` not isChanging
      , widgetIf isAdding $ centerWidget addingTickerWidget 
      , widgetIf isEditing $ centerWidget editingTickerWidget 
      , widgetIf isDeleting $ centerWidget $
          confirmDeleteWidget (tickerRegistryModel ^. #newTickerInfo % #ticker)
      ] `styleBasic` [padding 10]
  where
    isChanging :: Bool
    isChanging = or
      [ isAdding
      , isEditing
      , isDeleting
      ]

    isAdding :: Bool
    isAdding = tickerRegistryModel ^. #addingTicker

    isEditing :: Bool
    isEditing = tickerRegistryModel ^. #editingTicker

    isDeleting :: Bool
    isDeleting = tickerRegistryModel ^. #deletingTicker

    searchTarget :: Text
    searchTarget = tickerRegistryModel ^. #search

    searchIsADA :: Bool
    searchIsADA = searchTarget == "ADA"

    searchFilter :: [TickerInfo] -> [TickerInfo]
    searchFilter xs
      | searchTarget == "" = xs
      | otherwise = flip filter xs $ \TickerInfo{..} -> 
          searchTarget `Text.isInfixOf` display ticker

    allTickers :: [TickerInfo]
    allTickers = fromTickerMap tickerMap

    sample :: [TickerInfo]
    sample = searchFilter allTickers

    tickerRow :: TickerInfo-> AppNode
    tickerRow TickerInfo{..} = do
      let newInfo = NewTickerInfo (display ticker) (display policyId) (display assetName) decimals
      vstack
        [ hstack 
            [ label (display ticker)
                `styleBasic` [textSize 10, textColor customBlue]
            , filler
            , label ("Decimal Places: " <> show decimals)
                `styleBasic` [textSize 10]
            ]
        , spacer_ [width 5]
        , hstack 
            [ copyableLabelSelf (display policyId <> "." <> display assetName) 8 lightGray customBlue
            , filler
            , tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon 
                    (TickerRegistryEvent $ ChangeTickerInfo $ StartAdding $ Just newInfo)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 0
                    , bgColor transparent
                    , border 0 transparent
                    ]
            , spacer_ [width 3]
            , tooltip_ "Delete" [tooltipDelay 0] $
                button deleteIcon 
                    (TickerRegistryEvent $ DeleteTickerInfo $ GetDeleteConfirmation $ Just newInfo)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customRed
                    , textFont "Remix"
                    , textMiddle
                    , padding 0
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        ] `styleBasic`
            [ padding 10
            , radius 10
            , border 1 black
            , bgColor customGray2
            ]

    editingTickerWidget :: AppNode
    editingTickerWidget = do
      vstack
        [ centerWidgetH $ label (tickerRegistryModel ^. #newTickerInfo % #ticker)
        , spacer
        , hstack 
            [ label "Policy Id:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #policyId) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Asset Name:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #assetName) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Decimals:"
            , spacer
            , numericField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #decimals)
                `styleBasic` [textSize 12, width 100, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ TickerRegistryEvent $ ChangeTickerInfo CancelAdding
            , spacer
            , mainButton "Save" $ TickerRegistryEvent $ ChangeTickerInfo ConfirmAdding
            ]
        ] `styleBasic` 
            [ bgColor customGray3
            , padding 20
            , radius 5
            ]

    addingTickerWidget :: AppNode
    addingTickerWidget = do
      vstack
        [ centerWidgetH $ label "New Ticker Info"
        , spacer
        , hstack 
            [ label "Ticker:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #ticker) 
                `styleBasic` [width 200, textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Policy Id:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #policyId) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Asset Name:"
            , spacer
            , textField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #assetName) 
                `styleBasic` [textSize 12, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ label "Decimals:"
            , spacer
            , numericField (toLensVL $ #tickerRegistryModel % #newTickerInfo % #decimals)
                `styleBasic` [textSize 12, width 100, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ TickerRegistryEvent $ AddNewTickerInfo CancelAdding
            , spacer
            , mainButton "Save" $ TickerRegistryEvent $ AddNewTickerInfo ConfirmAdding
            ]
        ] `styleBasic` 
            [ bgColor customGray3
            , padding 20
            , radius 5
            ]

    confirmDeleteWidget :: Text -> AppNode
    confirmDeleteWidget tckr = do
      vstack
        [ spacer
        , centerWidgetH $ flip styleBasic [textColor lightGray] $ label $ mconcat
            [ "Are you sure you would like to delete the entry for '"
            , tckr
            , "'?"
            ]
        , spacer_ [width 20]
        , hstack 
            [ filler
            , button "Cancel" $ TickerRegistryEvent $ DeleteTickerInfo CancelDeletion
            , spacer
            , mainButton "Confirm" $ TickerRegistryEvent $ DeleteTickerInfo ConfirmDeletion
            ]
        ] `styleBasic` [bgColor customGray3, padding 10, radius 5]

adaProhibitedWidget :: AppNode
adaProhibitedWidget = 
  centerWidgetH $ 
    label "'ADA' cannot be used as a ticker." `styleBasic` [textColor lightGray]

unknownTickerWidget :: Text -> AppNode
unknownTickerWidget searchTarget = do
  vstack
    [ centerWidgetH $ label "No tickers match that search"
        `styleBasic` [textColor lightGray]
    , spacer_ [width 5]
    , centerWidgetH $ 
        box_ [onClick $ TickerRegistryEvent $ AddNewTickerInfo $ StartAdding $ Just searchTarget] $
          label largeAddIcon
            `styleBasic` 
              [ textSize 14
              , textColor customBlue
              , radius 10
              , textMiddle
              , paddingT 3
              , paddingB 3
              , paddingR 7
              , paddingL 7
              , bgColor customGray2
              , border 0 transparent
              , textFont "Remix"
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
    ]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy other data.
copyableLabelFor :: Text -> Text -> WidgetNode s AppEvent
copyableLabelFor caption info = 
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText info)
        `styleBasic`
          [ padding 0
          , radius 5
          , textMiddle
          , border 0 transparent
          , textColor customBlue
          , bgColor transparent
          , textSize 14
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label info `styleBasic` [textSize 12, textColor lightGray]
    ]

-- | A label button that will copy itself.
copyableLabelSelf :: Text -> Double -> Color -> Color -> WidgetNode s AppEvent
copyableLabelSelf caption fontSize mainColor hoverColor = 
  tooltip_ "Copy" [tooltipDelay 0] $ button_ caption (CopyText caption) [resizeFactor 2]
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textLeft
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor hoverColor, cursorIcon CursorHand]

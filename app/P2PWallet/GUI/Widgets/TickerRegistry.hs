module P2PWallet.GUI.Widgets.TickerRegistry
  (
    tickerRegistryWidget
  ) where

import Monomer
import Data.Map qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

tickerRegistryWidget :: AppModel -> AppNode
tickerRegistryWidget AppModel{..} = do
    centerWidget $ vstack
      [ centerWidgetH $ label "Ticker Registry"
          `styleBasic`
            [ padding 10
            , textSize 20
            , textFont "Italics"
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
          , spacer_ [width 5]
          , toggleButton_ "Search" (toLensVL #forceRedraw)
              [toggleButtonOffStyle searchOffStyle]
              `styleBasic`
                [ bgColor customGray4
                , textColor customBlue
                , textSize 12
                , border 1 customBlue
                ]
              `styleHover` [ bgColor customGray1 ]
              `styleDisabled`
                [ textColor darkGray
                , border 1 darkGray
                ]
              `nodeEnabled` not isChanging
          ]
      , box_ [mergeRequired reqUpdate] $ zstack
          [ widgetMaybe result resultWidget
              `nodeVisible` not isChanging
          , unknownTickerWidget
              `nodeVisible` and
                [ isNothing result
                , searchTarget /= ""
                , not isChanging
                ]
          , addingTickerWidget `nodeVisible` isAdding
          , editingTickerWidget `nodeVisible` isEditing
          , confirmDeleteWidget `nodeVisible` isDeleting
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

    reqUpdate :: AppWenv -> AppModel -> AppModel -> Bool
    reqUpdate _ old new 
      | old ^. #tickerRegistryModel % #search /= new ^. #tickerRegistryModel % #search = False
      | otherwise = True

    searchOffStyle :: Style
    searchOffStyle = 
      def `styleBasic` 
            [ bgColor customGray4
            , textColor customBlue
            , textSize 12
            , border 1 customBlue
            ]
          `styleHover`
            [ bgColor customGray1 ]

    searchTarget :: Text
    searchTarget = tickerRegistryModel ^. #search

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

    unknownTickerWidget :: AppNode
    unknownTickerWidget = do
      vstack
        [ centerWidgetH $ label "This ticker is not in the registry"
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

    confirmDeleteWidget :: AppNode
    confirmDeleteWidget = do
      vstack
        [ centerWidgetH $ flip styleBasic [textColor lightGray] $ label $ mconcat
            [ "Are you sure you would like to delete the entry for '"
            , tickerRegistryModel ^. #newTickerInfo % #ticker
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


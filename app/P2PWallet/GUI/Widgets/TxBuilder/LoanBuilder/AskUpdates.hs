module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.AskUpdates
  ( 
    askUpdatesList
  , editAskUpdateWidget
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

askUpdatesList :: ReverseTickerMap -> [(Int,AskUpdate)] -> [AppNode]
askUpdatesList reverseTickerMap = map utxoRow
  where
    collateralAssetWidget :: NativeAsset -> AppNode
    collateralAssetWidget asset = do
      hstack
        [ spacer_ [width 2]
        , copyableLabelSelf (showAssetNameOnly reverseTickerMap asset) lightGray 8
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

    utxoRow :: (Int,AskUpdate) -> AppNode
    utxoRow s@(idx,AskUpdate{oldAsk,newAsk=AskCreation{..}}) = do
      let askAmount = showAssetBalance True reverseTickerMap loanAmount
      hstack
        [ vstack
            [ hstack
                [ label "Update Loan Ask"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display (oldAsk ^. #utxoRef) in
                  flip styleBasic [textSize 10] $ tooltip_ prettyRef [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText prettyRef] $
                      label targetUTxOIcon
                        `styleBasic` 
                          [ bgColor black
                          , textMiddle
                          , textFont "Remix"
                          , textSize 8
                          , textColor customBlue
                          , paddingT 1
                          , paddingB 1
                          , paddingL 3
                          , paddingR 3
                          , radius 5
                          ]
                        `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ tooltip_ (oldAsk ^. #walletAlias) [tooltipDelay 0] $
                    label userIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        ]
                , filler
                , label (askAmount <> " for " <> show loanTerm <> " Day(s)")
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ box_ [alignTop] $ label "Offered Collateral:"
                    `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , vstack_ [childSpacing_ 3] $ for (groupInto 4 collateral) $ 
                    \asset -> hstack_ [childSpacing_ 3] $ map collateralAssetWidget asset
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (loanBuilderEvent $ EditSelectedAskUpdate $ StartAdding $ Just s)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (loanBuilderEvent $ RemoveSelectedAskUpdate idx)
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , padding 3
                , radius 3
                , bgColor transparent
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        ]

editAskUpdateWidget :: AppModel -> AppNode
editAskUpdateWidget _ = do
  let maybeLens' = maybeLens (0,def) $ #txBuilderModel % #loanBuilderModel % #targetAskUpdate
  vstack
    [ centerWidget $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Edit Ask Update"
              `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
        , spacer
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert askLoanAmountMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Loan Amount:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #loanAmount) 
                  [placeholder "10 ADA"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 50]
            , box_ [alignMiddle, onClick $ Alert askDurationMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Duration (Days):"
                `styleBasic` [textSize 10]
            , spacer
            , numericField_ (toLensVL $ maybeLens' % _2 % #loanTerm) [M.decimals 0]
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , separatorLine `styleBasic` [fgColor darkGray]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert askCollateralMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Collateral Assets (separated with newlines):"
                `styleBasic` [textSize 10]
            ]
        , spacer
        , textArea (toLensVL $ maybeLens' % _2 % #collateral)
            `styleBasic` [height 180, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer
        , box_ [alignRight] $ 
            hstack
              [ button "Cancel" (loanBuilderEvent $ EditSelectedAskUpdate CancelAdding)
                  `styleBasic` [textSize 10]
              , spacer
              , mainButton "Confirm" (loanBuilderEvent $ EditSelectedAskUpdate ConfirmAdding)
                  `styleBasic` [textSize 10]
              ]
        ] `styleBasic`
            [ bgColor customGray3
            , padding 20
            , radius 20
            ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
        , radius 10
        ]

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

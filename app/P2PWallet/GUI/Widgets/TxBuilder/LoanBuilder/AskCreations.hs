module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.AskCreations
  ( 
    askCreationsList
  , editAskCreationWidget
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

askCreationsList :: ReverseTickerMap -> [(Int,AskCreation)] -> [AppNode]
askCreationsList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,AskCreation) -> AppNode
    utxoRow s@(idx,AskCreation{..}) = do
      let askAmount = showAssetBalance True reverseTickerMap loanAmount
          numberOfCollateral = length collateral
      hstack
        [ vstack
            [ hstack
                [ label ("Ask for " <> askAmount <> " Loan")
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label ("Duration: " <> show loanTerm <> " Days")
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label ("Borrower ID: " <> alias <> " (" <> display borrowerCredential <> ")")
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label (show numberOfCollateral <> " Collateral Asset(s)")
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , hstack
            [ vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                    button editIcon 
                        (loanBuilderEvent $ EditSelectedAskCreation $ StartAdding $ Just s)
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
                ]
            , spacer_ [width 2]
            , countWidget idx count
            , spacer_ [width 2]
            , vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                    button closeCircleIcon (loanBuilderEvent $ RemoveSelectedAskCreation idx)
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
            ]
        ]

    countWidget :: Int -> Int -> AppNode
    countWidget idx count = do
      let upperCount = count + 1
          lowerCount = count - 1
      vstack
        [ box_ [onClick $ loanBuilderEvent $ ChangeAskCreationCount idx upperCount] $
            label ascendingSortIcon
              `styleBasic`
                [ textSize 14
                , textColor lightGray
                , padding 0
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover`
                [ textColor customBlue
                , cursorIcon CursorHand
                ]
        , flip styleBasic [padding 3, bgColor customGray2] $ 
            box $ label (show count) `styleBasic` [textSize 12, padding 0, textColor customBlue]
        , flip nodeEnabled (lowerCount > 0) $
            box_ [onClick $ loanBuilderEvent $ ChangeAskCreationCount idx lowerCount] $
              label descendingSortIcon
                `styleBasic`
                  [ textSize 14
                  , textColor $ if lowerCount > 0 then lightGray else customGray1
                  , padding 0
                  , textMiddle
                  , textFont "Remix"
                  ]
                `styleHover`
                  [ textColor customBlue
                  , cursorIcon CursorHand
                  ]
        ]

editAskCreationWidget :: AppModel -> AppNode
editAskCreationWidget _ = do
  let maybeLens' = maybeLens (0,def) $ #txBuilderModel % #loanBuilderModel % #targetAskCreation
  centerWidget $ vstack
    [ spacer
    , box_ [alignMiddle] $
        label "Edit Ask"
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
        , label "Collateral Assets (separated with newlines)"
            `styleBasic` [textSize 10]
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #collateral)
        `styleBasic` [height 180, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (loanBuilderEvent $ EditSelectedAskCreation CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Confirm" (loanBuilderEvent $ EditSelectedAskCreation ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic`
        [ bgColor customGray3
        , padding 20
        , radius 20
        ]

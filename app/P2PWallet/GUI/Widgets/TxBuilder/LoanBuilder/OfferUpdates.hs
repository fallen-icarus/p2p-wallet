module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferUpdates
  ( 
    offerUpdatesList
  , editOfferUpdateWidget
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

offerUpdatesList :: ReverseTickerMap -> [(Int,OfferUpdate)] -> [AppNode]
offerUpdatesList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,OfferUpdate) -> AppNode
    utxoRow s@(idx,OfferUpdate{oldOffer,newOffer=OfferCreation{..}}) = do
      let prettyLoanAmount = showAssetBalance True reverseTickerMap loanAmount
          numberOfCollateral = length collateralization
      hstack
        [ vstack
            [ hstack
                [ label "Update Offer"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , let prettyRef = display (oldOffer ^. #utxoRef) in
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
                , filler
                , label ("New Offer: " <> prettyLoanAmount <> " for " <> show loanTerm <> " Days")
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label ("Lender: " <> alias <> " (" <> display lenderCredential <> ")")
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
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (loanBuilderEvent $ EditSelectedOfferUpdate $ StartAdding $ Just s)
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
                button closeCircleIcon (loanBuilderEvent $ RemoveSelectedOfferUpdate idx)
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

editOfferUpdateWidget :: AppModel -> AppNode
editOfferUpdateWidget AppModel{txBuilderModel, knownWallets, reverseTickerMap, tickerMap} = do
  let maybeLens' = maybeLens (0,def) $ #txBuilderModel % #loanBuilderModel % #targetOfferUpdate
      NewOfferCreation{..} = maybe def snd $ 
        txBuilderModel ^. #loanBuilderModel % #targetOfferUpdate
      suppliedMinPayment = readMaybe @Decimal $ toString minPayment
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
      parsedLoanAmount = parseNativeAssets tickerMap mempty loanAmount
      helpButton msg = box_ [alignMiddle, onClick $ Alert msg] $
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
  centerWidget $ vscroll_ [wheelRate 50] $ vstack
    [ spacer
    , box_ [alignMiddle] $
        label "Edit Offer Update"
          `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
    , spacer
    , hstack
        [ helpButton offerBorrowerIdMsg
        , spacer_ [width 3]
        , label "Borrower ID:"
            `styleBasic` [textSize 10]
        , spacer
        , copyableLabelSelf (display borrowerCredential) lightGray 10
        ]
    , spacer
    , hstack
        [ helpButton offerLoanAmountMsg
        , spacer_ [width 3]
        , label "Loan Amount:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #loanAmount) 
              [placeholder "5"] 
            `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ helpButton offerLoanTermMsg
        , spacer_ [width 3]
        , label "Duration:"
            `styleBasic` [textSize 10]
        , spacer
        , numericField_ (toLensVL $ maybeLens' % _2 % #loanTerm) [M.decimals 0]
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , label "Day(s)"
            `styleBasic` [textColor lightGray, textMiddle, textSize 10]
        ]
    , spacer
    , hstack
        [ helpButton offerPaymentAddressMsg
        , spacer_ [width 3]
        , label "Payment Address:"
            `styleBasic` [textSize 10]
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % _2 % #paymentWallet) 
              (knownWallets ^. #paymentWallets) 
              (view #alias) -- The dropdown displays the wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray2
              , width 150
              , border 1 black
              , textSize 10
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        ]
    , spacer
    , hstack
        [ helpButton offerInterestMsg
        , spacer_ [width 3]
        , label "Interest:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #interest) 
              [placeholder "10"] 
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , label "%"
            `styleBasic` [textColor lightGray, textMiddle, textFont "Bold", textSize 14]
        ]
    , spacer
    , hstack
        [ helpButton offerCompoundFrequencyMsg
        , spacer_ [width 3]
        , label "Compound Frequency:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #compoundFrequency) 
              [placeholder "5"] 
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , label "Day(s)"
            `styleBasic` [textColor lightGray, textMiddle, textSize 10]
        ]
    , widgetIf (compoundFrequency /= "") $ vstack
        [ spacer
        , hstack
            [ helpButton offerMinimumPaymentMsg
            , spacer_ [width 3]
            , label "Minimum payment:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #minPayment) 
                  [placeholder "10.0"] 
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , label (either (const "") (showAssetNameOnly reverseTickerMap) parsedLoanAmount)
                `styleBasic` [textColor lightGray, textMiddle, textSize 10]
            ]
        ]
    , widgetIf (suppliedMinPayment > Just 0) $ vstack
        [ spacer
        , hstack
            [ helpButton offerPenaltyMsg
            , spacer_ [width 3]
            , label "Penalty:"
                `styleBasic` [textSize 10]
            ]
        , spacer_ [width 5]
        , hstack
            [ spacer
            , textDropdown_ (toLensVL $ maybeLens' % _2 % #penalty % _1) (enumFrom NoNewPenalty) display
                [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` [textSize 10, width 120, bgColor customGray1]
                `styleFocus` [border 1 customBlue]
            , widgetIf (penalty ^. _1 == NewFixedPenalty) $ hstack
                [ spacer
                , textField_ (toLensVL $ maybeLens' % _2 % #penalty % _2) 
                      [placeholder "10.0"] 
                    `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
                , spacer_ [width 3]
                , label (either (const "") (showAssetNameOnly reverseTickerMap) parsedLoanAmount)
                    `styleBasic` [textColor lightGray, textMiddle, textSize 10]
                ]
            , widgetIf (penalty ^. _1 == NewPercentPenalty) $ hstack
                [ spacer
                , textField_ (toLensVL $ maybeLens' % _2 % #penalty % _2) 
                      [placeholder "2"] 
                    `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
                , spacer_ [width 3]
                , label "%"
                    `styleBasic` [textColor lightGray, textMiddle, textFont "Bold", textSize 14]
                ]
            ]
        ]
    , spacer
    , hstack
        [ helpButton offerCollateralIsSwappableMsg
        , spacer_ [width 3]
        , label "Collateral Can Be Swapped Out:"
            `styleBasic` [textSize 10]
        , spacer
        , checkbox_ (toLensVL $ maybeLens' % _2 % #collateralIsSwappable) [checkboxSquare]
            `styleBasic` [fgColor customGray1, hlColor customBlue]
        ]
    , spacer
    , hstack
        [ helpButton offerClaimPeriodMsg
        , spacer_ [width 3]
        , label "Claim Period:"
            `styleBasic` [textSize 10]
        , spacer
        , numericField_ (toLensVL $ maybeLens' % _2 % #claimPeriod) [M.decimals 0]
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , label "Day(s)"
            `styleBasic` [textColor lightGray, textMiddle, textSize 10]
        ]
    , spacer
    , hstack
        [ helpButton offerExpirationMsg
        , spacer_ [width 3]
        , label "Offer Expiration:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #offerExpiration)
              [placeholder "5"] 
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , label "Day(s)"
            `styleBasic` [textColor lightGray, textMiddle, textSize 10]
        ]
    , spacer
    , hstack
        [ helpButton offerCollateralMsg
        , spacer_ [width 3]
        , label "Collateral Values (separated with newlines)"
            `styleBasic` [textSize 10]
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #collateralization)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (loanBuilderEvent $ EditSelectedOfferUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Confirm" (loanBuilderEvent $ EditSelectedOfferUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic`
        [ bgColor customGray3
        , padding 20
        , radius 20
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

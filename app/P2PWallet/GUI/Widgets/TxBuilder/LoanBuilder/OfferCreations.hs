module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferCreations
  ( 
    offerCreationsList
  , editOfferCreationWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

offerCreationsList :: ReverseTickerMap -> TimeZone -> [(Int,OfferCreation)] -> [AppNode]
offerCreationsList reverseTickerMap timeZone = map utxoRow
  where
    genLenderTerms :: OfferCreation -> Loans.LenderTerms
    genLenderTerms OfferCreation{..} = Loans.LenderTerms
      { lenderCredential = lenderCredential
      , lenderAddress = paymentWallet ^. #paymentAddress
      , loanTerm = toPlutusTime $ convertDaysToPosixPeriod loanTerm
      , loanAmount = loanAmount
      , interest = interest
      , compoundingInterest = compoundingInterest
      , epochDuration = toPlutusTime . convertDaysToPosixPeriod <$> epochDuration
      , minPayment = minPayment
      , penalty = penalty
      , claimPeriod = toPlutusTime $ convertDaysToPosixPeriod claimPeriod
      , offerExpiration = (+ currentTime) . toPlutusTime . convertDaysToPosixPeriod <$> offerExpiration
      , collateralization = sortOn fst collateralization
      , offerDeposit = unLovelace deposit
      , collateralIsSwappable = collateralIsSwappable
      }

    collateralAssetWidget :: NativeAsset -> (NativeAsset,Rational) -> AppNode
    collateralAssetWidget loanAsset (collateralAsset, price) = do
      let formattedPrice = showPriceFormatted reverseTickerMap collateralAsset loanAsset price
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap collateralAsset
            , " / "
            , showAssetNameOnly reverseTickerMap loanAsset
            ]
      hstack
        [ spacer_ [width 2]
        , label prettyPrice
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

    utxoRow :: (Int,OfferCreation) -> AppNode
    utxoRow s@(idx,oc@OfferCreation{alias,paymentWallet}) = do
      let Loans.OfferDatum{..} = Loans.createOfferDatum $ genLenderTerms oc
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          prettyLoanAmount = showAssetBalance True reverseTickerMap loanAmount
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                           $ collateralization ^. #unCollateralization
          prettyInterest 
            | loanInterest == 0 = "Interest-Free"
            | otherwise = unwords
                [ if compoundingInterest then "Compounding" else "Non-Compounding"
                , "Interest:"
                , displayPercentage (toRational loanInterest) <> "%"
                ]
          prettyEpochDuration = flip (maybe "No Loan Epochs") epochDuration $ \freq ->
            unwords
              [ "Loan Epoch:"
              , show (calcDaysInPosixPeriod $ fromPlutusTime freq)
              , "Day(s)"
              ]
          prettyMinPayment = unwords
            [ "Minimum Payment:"
            , showAssetBalance True reverseTickerMap $ loanAmount & #quantity .~ minPayment
            ]
          prettyPenalty = case penalty of
            Loans.NoPenalty -> "No Penalty"
            Loans.FixedFee fee -> unwords
              [ "Fee Penalty:"
              , showAssetBalance True reverseTickerMap $ loanAmount & #quantity .~ fee
              ]
            Loans.PercentFee percent -> unwords
              [ "Percent Penalty:"
              , displayPercentage (toRational percent) <> "%"
              ]
          prettyExpirationTime = maybe "Offer does not expire." $ \exprTime ->
            unwords
              [ "Offer Expires:"
              , showLocalDate timeZone $ fromPlutusTime exprTime
              , showLocalTime timeZone $ fromPlutusTime exprTime
              ]
          prettyClaimPeriod = unwords
            [ "Claim Period:"
            , show $ oc ^. #claimPeriod
            , "Day(s)"
            ]
          prettyOfferTime = prettyExpirationTime offerExpiration <> "\n" <> prettyClaimPeriod
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          payToAddress = paymentWallet ^. #paymentAddress
          addressTip = unwords
            [ "Payments to"
            , paymentWallet ^. #alias <> ":"
            , display payToAddress
            ]
      hstack
        [ vstack
            [ hstack
                [ label "Create Loan Offer"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ tooltip_ alias [tooltipDelay 0] $
                    label userIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        ]
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ prettyOfferTime [tooltipDelay 0] $
                      label expirationIcon
                        `styleBasic` 
                          [ textMiddle
                          , textFont "Remix"
                          , textSize 10
                          , textColor customRed
                          ]
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ tooltip_ addressTip [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText $ display payToAddress] $
                      label targetAddressIcon
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
                , label (prettyLoanAmount <> " for " <> show duration <> " Day(s)")
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 3]
            , hstack
                [ label prettyInterest
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label prettyEpochDuration
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            , widgetIf (isJust epochDuration) $ vstack
                [ spacer_ [width 3]
                , hstack
                    [ label prettyMinPayment
                        `styleBasic` [textSize 8, textColor lightGray]
                    , filler
                    , label prettyPenalty
                        `styleBasic` [textSize 8, textColor lightGray]
                    ]
                ]
            , spacer_ [width 2]
            , hstack
                [ widgetIf collateralIsSwappable $ box_ [alignTop] $ hstack
                    [ flip styleBasic [textSize 10] $ tooltip_ swapCollateralMsg [tooltipDelay 0] $
                        label swappableCollateralIcon
                          `styleBasic` 
                            [ textMiddle
                            , textFont "Remix"
                            , textSize 10
                            , textColor customBlue
                            , paddingT 1
                            ]
                    , spacer_ [width 2]
                    ]
                , box_ [alignTop] $ label "Collateralization:"
                    `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
                    \col -> hstack_ [childSpacing_ 3] $ map (collateralAssetWidget loanAmount) col
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
                    (loanBuilderEvent $ EditSelectedOfferCreation $ StartAdding $ Just s)
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
                button closeCircleIcon (loanBuilderEvent $ RemoveSelectedOfferCreation idx)
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

editOfferCreationWidget :: AppModel -> AppNode
editOfferCreationWidget AppModel{txBuilderModel, knownWallets, reverseTickerMap, tickerMap} = do
  let maybeLens' = maybeLens (0,def) $ #txBuilderModel % #loanBuilderModel % #targetOfferCreation
      NewOfferCreation{..} = maybe def snd $ 
        txBuilderModel ^. #loanBuilderModel % #targetOfferCreation
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
        label "Edit Offer"
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
        [ helpButton offerEpochDurationMsg
        , spacer_ [width 3]
        , label "Epoch Duration:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #epochDuration) 
              [placeholder "5"] 
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , label "Day(s)"
            `styleBasic` [textColor lightGray, textMiddle, textSize 10]
        ]
    , widgetIf (epochDuration /= "") $ vstack
        [ spacer
        , hstack
            [ helpButton offerCompoundingInterestMsg
            , spacer_ [width 3]
            , label "Compounding:"
                `styleBasic` [textSize 10]
            , spacer
            , checkbox_ (toLensVL $ maybeLens' % _2 % #compoundingInterest) [checkboxSquare]
                `styleBasic` [fgColor customGray1, hlColor customBlue]
            ]
        , spacer
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
        , label "Collateral Values (separated with newlines):"
            `styleBasic` [textSize 10]
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #collateralization)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (loanBuilderEvent $ EditSelectedOfferCreation CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Confirm" (loanBuilderEvent $ EditSelectedOfferCreation ConfirmAdding)
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

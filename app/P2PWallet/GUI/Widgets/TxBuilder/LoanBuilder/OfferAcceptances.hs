module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferAcceptances
  ( 
    offerAcceptancesList
  , editOfferAcceptanceWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

offerAcceptancesList :: ReverseTickerMap -> [(Int,OfferAcceptance)] -> [AppNode]
offerAcceptancesList reverseTickerMap = map utxoRow
  where
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

    utxoRow :: (Int,OfferAcceptance) -> AppNode
    utxoRow s@(idx,OfferAcceptance{..}) = do
      let Loans.OfferDatum{..} = fromMaybe def $ loanUTxOOfferDatum offerUTxO
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
          prettyClaimPeriod = unwords
            [ "Claim Period:"
            , show $ calcDaysInPosixPeriod $ fromPlutusTime claimPeriod
            , "Day(s)"
            ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          payToAddress = either (const "error") fst $ plutusToBech32 network lenderAddress
          addressTip = unwords
            [ "Payments to:"
            , display payToAddress
            ]
      hstack
        [ vstack
            [ hstack
                [ label "Accept Loan Offer"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display $ offerUTxO ^. #utxoRef in
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
                    tooltip_ prettyClaimPeriod [tooltipDelay 0] $
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
                    (loanBuilderEvent $ EditSelectedOfferAcceptance $ StartAdding $ Just s)
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
                button closeCircleIcon (loanBuilderEvent $ RemoveSelectedOfferAcceptance idx)
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

editOfferAcceptanceWidget :: AppModel -> AppNode
editOfferAcceptanceWidget AppModel{..} = do
    let maybeLens' = maybeLens (def,def) 
                   $ #txBuilderModel % #loanBuilderModel % #targetOfferAcceptance
        NewOfferAcceptance{offerUTxO} = maybe def snd $ 
          txBuilderModel ^. #loanBuilderModel % #targetOfferAcceptance
        Loans.OfferDatum{loanAsset,collateralization,loanPrincipal} = fromMaybe def $
          loanUTxOOfferDatum offerUTxO
        loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
        collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                         $ collateralization ^. #unCollateralization
    centerWidget $ vstack
      [ centerWidgetH $
          label "Edit your specified collateral"
            `styleBasic` [textFont "Italics", textColor customBlue]
      , label ("Loan Amount: " <> showAssetBalance True reverseTickerMap loanAmount)
          `styleBasic` [textSize 12]
      , spacer
      , hstack
          [ box_ [alignMiddle, onClick $ Alert collateralRatesMsg] $
              label helpIcon
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , textFont "Remix"
                  , textSize 10
                  , padding 2
                  ]
                `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , spacer_ [width 3]
          , label "Collateral Rates:"
              `styleBasic` [textSize 12]
          ]
      , spacer_ [width 3]
      , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
          \col -> hstack_ [childSpacing_ 3] $ [spacer] <> map (collateralAssetWidget loanAmount) col
      , spacer
      , hstack
          [ box_ [alignMiddle, onClick $ Alert collateralAmountsMsg] $
              label helpIcon
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , textFont "Remix"
                  , textSize 10
                  , padding 2
                  ]
                `styleHover` [bgColor customGray2, cursorIcon CursorHand]
          , spacer_ [width 3]
          , label "Collateral Assets (separated with newlines):"
              `styleBasic` [textSize 12]
          ]
      , spacer_ [width 3]
      , textArea (toLensVL $ maybeLens' % _2 % #collateralAmounts)
          `styleBasic` [height 180, textSize 10, bgColor customGray1]
          `styleFocus` [border 1 customBlue]
      , spacer
      , box_ [alignRight] $ 
          hstack
            [ button "Cancel" (loanBuilderEvent $ EditSelectedOfferAcceptance CancelAdding)
                `styleBasic` [textSize 10]
            , spacer
            , mainButton "Confirm" (loanBuilderEvent $ EditSelectedOfferAcceptance ConfirmAdding)
                `styleBasic` [textSize 10]
            ]
      , spacer
      ] `styleBasic`
          [ bgColor customGray3
          , radius 10
          , border 1 black
          , padding 30
          , paddingB 0
          ]
  where
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
            `styleBasic` [textSize 12, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

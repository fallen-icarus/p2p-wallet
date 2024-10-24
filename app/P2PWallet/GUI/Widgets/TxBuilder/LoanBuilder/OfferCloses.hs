module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferCloses
  ( 
    offerClosesList
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Plutus
import P2PWallet.Prelude

offerClosesList :: ReverseTickerMap -> TimeZone -> [(Int,OfferClose)] -> [AppNode]
offerClosesList reverseTickerMap timeZone = map utxoRow
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

    utxoRow :: (Int, OfferClose) -> AppNode
    utxoRow (idx,OfferClose{..}) = do
      let Loans.OfferDatum{..} = fromMaybe def offerDatum
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
            , show $ calcDaysInPosixPeriod $ fromPlutusTime claimPeriod
            , "Day(s)"
            ]
          prettyOfferTime = prettyExpirationTime offerExpiration <> "\n" <> prettyClaimPeriod
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          payToAddress = either (const "error") fst $ plutusToBech32 network lenderAddress
          addressTip = unwords
            [ "Payments to:"
            , display payToAddress
            ]
      hstack
        [ vstack
            [ hstack
                [ label "Close Loan Offer"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display utxoRef in
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
                , flip styleBasic [textSize 10] $ tooltip_ walletAlias [tooltipDelay 0] $
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
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (loanBuilderEvent $ RemoveSelectedOfferClose idx)
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

module P2PWallet.GUI.Widgets.Lending.Borrow.CreditHistory
  ( creditHistoryWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

creditHistoryWidget :: AppModel -> AppNode
creditHistoryWidget AppModel{lendingModel=LendingModel{..},reverseTickerMap} =
    zstack
      [ mainWidget
      ]
  where
    LoanWallet{..} = selectedWallet

    allSuccesses :: [LoanResult]
    allSuccesses = filter (not . view #isDefault) creditHistory

    creditScore :: Text
    creditScore 
      | null creditHistory = "undefined"
      | otherwise = displayPercentage 
                  $ toRational (length allSuccesses) / toRational (length creditHistory)

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ centerWidgetH $ hstack 
            [ label ("Credit Score: " <> creditScore)
                `styleBasic` [textFont "Italics", textSize 14]
            , box_ [onClick $ Alert creditScoreMsg] $
                label helpIcon
                  `styleBasic`
                    [ padding 2
                    , textSize 8
                    , border 0 transparent
                    , radius 20
                    , textMiddle
                    , bgColor transparent
                    , textColor customBlue
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
            vstack_ [childSpacing] (map resultRow creditHistory)
              `styleBasic` [padding 10]
        , filler
        ] 

    lockedCollateralWidget :: NativeAsset -> AppNode
    lockedCollateralWidget collateralAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap collateralAsset)
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

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
            , radius 3
            , border 1 customGray1
            ]

    resultRow :: LoanResult -> AppNode
    resultRow LoanResult{..} = do
      let Loans.ActiveDatum{..} = terms
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          unpaidBalance
            | isDefault = loanAmount & #quantity .~ roundUp (toRational loanOutstanding)
            | otherwise = loanAmount & #quantity .~ 0
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                           $ collateralization ^. #unCollateralization
          allAssets = 
            (lovelaceAsNativeAsset & #quantity .~ unLovelace remainingLovelace) : remainingNativeAssets
          lockedCollateral = 
            filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets
          prettyUnpaidBalance = unwords
            [ "Unpaid Balance:"
            , showAssetBalance True reverseTickerMap unpaidBalance
            ]
          prettyInterest = unwords
            [ "Interest:"
            , displayPercentage (toRational loanInterest) <> "%"
            ]
          prettyCompounding = flip (maybe "Non-Compounding") compoundFrequency $ \freq ->
            unwords
              [ "Compounding Every"
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
          defaultMsg
            | isDefault = "Default"
            | otherwise = "Success"
          swapCollateralMsg = "Collateral could be swapped out for other approved collateral"
          loanHistoryEvt = LendingEvent $ BorrowEvent $ InspectActiveLoanHistory loanId
      vstack
        [ hstack
            [ label defaultMsg
                `styleBasic` 
                  [ textSize 10
                  , textColor $ if isDefault then customRed else customBlue
                  ]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Loan ID: " <> display loanId) [tooltipDelay 0] $
                  box_ [alignMiddle , onClick loanHistoryEvt] $
                    label idCardIcon
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
            , label prettyUnpaidBalance
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 3]
        , hstack
            [ label ("Loan Principal: " <> showAssetBalance True reverseTickerMap loanAmount)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Duration: " <> show duration <> " Day(s)")
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , spacer_ [width 3]
        , hstack
            [ label prettyInterest
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyCompounding
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , widgetIf (isJust compoundFrequency) $ vstack
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
            [ widgetIf collateralIsSwappable $ hstack
                [ flip styleBasic [textSize 10] $ tooltip_ swapCollateralMsg [tooltipDelay 0] $
                    label swappableCollateralIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        ]
                , spacer_ [width 2]
                ]
            , label "Collateralization:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 3]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
                \col -> hstack_ [childSpacing_ 3] $ map (collateralAssetWidget loanAmount) col
            ]
        , widgetIf isDefault $ vstack
            [ spacer_ [width 1]
            , hstack
                [ label "Confiscated Collateral:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , vstack_ [childSpacing_ 3] $ for (groupInto 3 lockedCollateral) $ 
                    \col -> hstack_ [childSpacing_ 3] $ map lockedCollateralWidget col
                ]
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

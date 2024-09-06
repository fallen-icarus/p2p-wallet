module P2PWallet.GUI.Widgets.Lending.Borrow.ActiveLoans
  ( activeLoansWidget
  , inspectLoanWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map

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

activeLoansWidget :: AppModel -> AppNode
activeLoansWidget model@AppModel{lendingModel=LendingModel{..},reverseTickerMap,config} =
    zstack
      [ mainWidget
      , makePaymentWidget model `nodeVisible` isJust (borrowModel ^. #newLoanPayment)
      , activeLoansFilterWidget model `nodeVisible` (borrowModel ^. #showActiveLoansFilter)
      ]
  where
    Config{currentTime} = config

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    LoanWallet{..} = selectedWallet

    allActives :: [LoanUTxO]
    allActives = filter ((==Just True) . fmap (is _ActiveDatum) . view #loanDatum) utxos

    fractionShown :: Text
    fractionShown = 
      show (length sample) <> "/" <> show (length allActives)

    sample :: [LoanUTxO]
    sample = orderer (borrowModel ^. #activeLoansFilterModel % #sortingDirection) 
           . sorter (borrowModel ^. #activeLoansFilterModel % #sortingMethod) 
           . filterer currentTime reverseTickerMap (borrowModel ^. #activeLoansFilterModel) 
           $ allActives

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ centerWidgetH $ hstack 
            [ label ("Loans (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #lendingModel % #borrowModel % #showActiveLoansFilter)
                  [toggleButtonOffStyle toggleOffStyle]
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , paddingT 0
                    , paddingB 0
                    , paddingL 5
                    , paddingR 5
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ]
        , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
            vstack_ [childSpacing] (map activeRow sample)
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
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

    activeRow :: LoanUTxO -> AppNode
    activeRow u@LoanUTxO{utxoRef,lovelace=utxoLovelace,nativeAssets=utxoNativeAssets} = do
      let Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum u
          loanBalance = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
          expiration = fromPlutusTime loanExpiration
          claimDeadline = fromPlutusTime claimExpiration
          mNextCompounding = (+lastCompounding) <$> compoundFrequency
          nextPaymentDueDate = case mNextCompounding of
            Nothing -> expiration
            Just nextCompounding -> min (fromPlutusTime nextCompounding) expiration
          amountDue = minPayment - totalEpochPayments
          nextPaymentSize
            | minPayment == 0 = loanBalance
            | amountDue < 0 = loanBalance & #quantity .~ 0
            | otherwise = loanBalance & #quantity .~ amountDue
          prettyExpirationTime = unwords
            [ "Expires:"
            , showLocalDate (config ^. #timeZone) expiration
            , showLocalTime (config ^. #timeZone) expiration
            ]
          prettyClaimExpiration = unwords
            [ showLocalDate (config ^. #timeZone) claimDeadline
            , showLocalTime (config ^. #timeZone) claimDeadline
            ]
          prettyNextPaymentDueDate = unwords
            [ "Next Deadline:"
            , ""
            , showLocalDate (config ^. #timeZone) nextPaymentDueDate
            , showLocalTime (config ^. #timeZone) nextPaymentDueDate
            ]
          prettyInterest = unwords
            [ "Interest:"
            , displayPercentage (toRational loanInterest) <> "%"
            ]
          prettyNextPayment = unwords
            [ "Amount Required by Deadline:"
            , ""
            , showAssetBalance True reverseTickerMap nextPaymentSize
            ]
          prettyCompounding = flip (maybe "Non-Compounding") compoundFrequency $ \freq ->
            unwords
              [ "Compounding Every"
              , show (calcDaysInPosixPeriod $ fromPlutusTime freq)
              , "Day(s)"
              ]
          prettyPenalty = case penalty of
            Loans.NoPenalty -> "No Penalty"
            Loans.FixedFee fee -> unwords
              [ "Fee Penalty:"
              , showAssetBalance True reverseTickerMap $ loanBalance & #quantity .~ fee
              ]
            Loans.PercentFee percent -> unwords
              [ "Percent Penalty:"
              , displayPercentage (toRational percent) <> "%"
              ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          allAssets = 
            (lovelaceAsNativeAsset & #quantity .~ unLovelace utxoLovelace) : utxoNativeAssets
          lockedCollateral = 
            filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets
          loanHistoryEvt = LendingEvent $ BorrowEvent $ InspectActiveLoanHistory loanId
          (buttonEvt,buttonTip,buttonIcon,buttonEnabled)
            | currentTime >= nextPaymentDueDate && currentTime < expiration =
                -- A rollover is required to apply the interest and penalties.
                ( LendingEvent $ BorrowEvent $ RolloverLoan $ StartProcess $ Just u
                , "Rollover loan into next payment period"
                , rolloverLoanIcon
                , True
                )
            | currentTime < nextPaymentDueDate && currentTime < expiration =
                -- The borrower can still make payments.
                ( LendingEvent $ BorrowEvent $ MakeLoanPayment $ StartAdding $ Just u
                , "Make a loan payment"
                , paymentIcon
                , True
                )
            | currentTime >= expiration && currentTime < claimDeadline =
                -- The borrower cannot do anything.
                ( LendingEvent $ BorrowEvent $ ClaimLostCollateral u
                , "The lender has until " <> prettyClaimExpiration <> " to claim the collateral"
                , claimCollateralIcon
                , False
                )
            | otherwise =
                -- The borrower can claim the collateral.
                ( LendingEvent $ BorrowEvent $ ClaimLostCollateral u
                , "Claim the lost collateral"
                , claimCollateralIcon
                , True
                )
      hstack
        [ vstack
            [ hstack
                [ label ("Balance: " <> showAssetBalance True reverseTickerMap loanBalance)
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , let prettyRef = display utxoRef in
                  flip styleBasic [textSize 10] $ tooltip_ prettyRef [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText $ display utxoRef] $
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
                , flip styleBasic [textSize 10] $ 
                    tooltip_ prettyExpirationTime [tooltipDelay 0] $
                      label expirationIcon
                        `styleBasic` 
                          [ textMiddle
                          , textFont "Remix"
                          , textSize 10
                          , textColor customRed
                              ]
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ ("Loan ID: " <> display loanId) [tooltipDelay 0] $
                      box_ [alignMiddle , onClick loanHistoryEvt] $
                        label historyIcon
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
                , label prettyNextPaymentDueDate
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 3]
            , hstack
                [ label prettyInterest
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label prettyNextPayment
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            , widgetIf (isJust compoundFrequency) $ vstack
                [ spacer_ [width 3]
                , hstack
                    [ label prettyCompounding
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
                , label "Locked Collateral:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , vstack_ [childSpacing_ 3] $ for (groupInto 3 lockedCollateral) $ 
                    \col -> hstack_ [childSpacing_ 3] $ map lockedCollateralWidget col
                ]
            ] `styleBasic` 
                  [ padding 10
                  , bgColor customGray2
                  , radius 5
                  , border 1 black
                  ]
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ buttonTip [tooltipDelay 0] $
                button buttonIcon buttonEvt
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
                  `nodeEnabled` buttonEnabled
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

makePaymentWidget :: AppModel -> AppNode
makePaymentWidget AppModel{..} = do
    let maybeLens' = maybeLens def $ #lendingModel % #borrowModel % #newLoanPayment
        NewLoanPayment{activeUTxO,paymentAmount} = fromMaybe def $ 
          lendingModel ^. #borrowModel % #newLoanPayment
        Loans.ActiveDatum{loanAsset,collateralization,loanOutstanding} = fromMaybe def $
          loanUTxOActiveDatum activeUTxO
        loanBalance = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
        collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                         $ collateralization ^. #unCollateralization
        mPaymentAmount = toRational . view #quantity <$> 
          rightToMaybe (parseNativeAssets tickerMap mempty paymentAmount)
        startingBalance = toRational $ roundUp $ toRational loanOutstanding
        mPaymentRatio = (/ startingBalance) <$> mPaymentAmount
        prettyRatio = (<> "%") $ show @_ @Decimal $ (*100) $ realFracToDecimal 10
                    $ (fromMaybe 0 mPaymentRatio)
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
    vstack
      [ centerWidget $ vstack
          [ centerWidgetH $
              label "New Loan Payment"
                `styleBasic` [textFont "Italics", textColor customBlue]
          , spacer
          , label ("Current Balance: " <> showAssetBalance True reverseTickerMap loanBalance)
              `styleBasic` [textSize 12]
          , spacer
          , hstack
              [ label "Payment Amount:"
                  `styleBasic` [textSize 12]
              , spacer
              , textField_ (toLensVL $ maybeLens' % #paymentAmount) 
                    [placeholder $ showAssetBalance True reverseTickerMap loanBalance]
                  `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                  `styleFocus` [border 1 customBlue]
              , spacer_ [width 3]
              , helpButton offerLoanAmountMsg
              , filler
              , label ("Collateral Unlocked: " <> prettyRatio)
                  `styleBasic` [textSize 12]
              ]
          , spacer
          , label "Collateral Rates:"
              `styleBasic` [textSize 12]
          , spacer
          , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
              \col -> hstack_ [childSpacing_ 3] $ [spacer] <> map (collateralAssetWidget loanBalance) col
          , spacer
          , hstack
              [ label "Collateral Assets (separated with newlines)"
                  `styleBasic` [textSize 12]
              , helpButton paymentCollateralAmountsMsg
              ]
          , spacer
          , textArea (toLensVL $ maybeLens' % #collateralBalances)
              `styleBasic` [height 180, textSize 10, bgColor customGray1]
              `styleFocus` [border 1 customBlue]
          , spacer
          , box_ [alignRight] $ 
              hstack
                [ button "Cancel" (LendingEvent $ BorrowEvent $ MakeLoanPayment CancelAdding)
                    `styleBasic` [textSize 10]
                , spacer
                , mainButton "Confirm" (LendingEvent $ BorrowEvent $ MakeLoanPayment ConfirmAdding)
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
      ] `styleBasic` 
          [ bgColor $ black & #a .~ 0.4
          , padding 20
          , radius 10
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
            `styleBasic` [textSize 10, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

inspectLoanWidget :: AppModel -> AppNode
inspectLoanWidget AppModel{lendingModel=LendingModel{..},scene=_,..} = do
    vstack
      [ vstack
          [ centerWidgetH $
              label "Event History For Loan ID"
                `styleBasic` [textFont "Italics", textColor customBlue]
          , spacer
          , centerWidgetH $ hstack
              [ copyableLabelSelf (display targetId) lightGray 12
              , spacer_ [width 3]
              , tooltip_ "Resync History" [tooltipDelay 0] $
                  box_ [alignMiddle, onClick $ LendingEvent $ LookupLoanHistory $ StartProcess $ Just targetId] $
                    label refreshIcon
                      `styleBasic` 
                        [ border 0 transparent
                        , radius 20
                        , bgColor transparent
                        , textColor customBlue
                        , textMiddle
                        , textFont "Remix"
                        , padding 3
                        , textSize 12
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map eventRow history)
          , filler
          , hstack
              [ filler
              , button "Close" $ LendingEvent $ BorrowEvent CloseInspectedActiveLoanHistory
              ]
          ] `styleBasic`
              [ bgColor customGray3
              , padding 30
              , radius 10
              ]
      ] `styleBasic` 
          [ bgColor $ black & #a .~ 0.4
          , padding 30
          , radius 10
          ]
  where
    targetId :: Loans.LoanId
    targetId = fromMaybe "" $ borrowModel ^. #inspectedLoan

    history :: [LoanEvent]
    history = maybe [] fst $ Map.lookup targetId cachedLoanHistories

    explainEvent :: LoanEvent -> Text
    explainEvent LoanEvent{state=eventState, event, timeStamp} =
      let Loans.ActiveDatum{loanAsset, claimExpiration} = fromMaybe def eventState 
          loanNativeAsset = toNativeAsset loanAsset
       in case event of
            Left (Loans.CreateActive _) -> "Loan started."
            Right (Loans.MakePayment amount) -> mconcat
              [ "Made payment of "
              , showAssetBalance True reverseTickerMap $ loanNativeAsset & #quantity .~ amount
              , "."
              ]
            Right (Loans.ApplyInterest deposit times) -> mconcat
              [ "Applied interest "
              , show times
              , " time(s) with deposit increase of "
              , display $ Lovelace deposit
              , "."
              ]
            Right Loans.SpendWithKeyNFT -> "Defaulted collateral claimed by lender."
            Right (Loans.UpdateLenderAddress newAddress deposit) -> mconcat
              [ "Lender changed the required payment address to "
              , display $ fromRight "" $ 
                  fmap fst $ plutusToBech32 (config ^. #network) newAddress
              , " with deposit increase of "
              , display $ Lovelace deposit
              , "."
              ]
            Right Loans.Unlock -> 
              if claimExpiration < toPlutusTime timeStamp 
              then "Lost collateral claimed by borrower."
              else "Invalid Active UTxO closed by borrower."
            _ -> error "Other loan redeemer used."

    eventRow :: LoanEvent -> AppNode
    eventRow e@LoanEvent{timeStamp} = do
      vstack
        [ hstack
            [ label (explainEvent e)
                `styleBasic` [textSize 10]
            , filler
            ]
        , spacer_ [width 2]
        , hstack
            [ label calendarIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalDate (config ^. #timeZone) timeStamp)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , spacer
            , label clockIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalTime (config ^. #timeZone) timeStamp)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , filler
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

activeLoansFilterWidget :: AppModel -> AppNode
activeLoansFilterWidget AppModel{lendingModel=LendingModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #lendingModel % #borrowModel
      filterScene = borrowModel ^. #activeLoansFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #activeLoansFilterModel % #scene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radiusTL 10
                      , radiusBL 0
                      , radiusTR 0
                      , radiusBR 0
                      , border 1 black
                      ]
                , optionButton_ "Sort" SortScene 
                    (toLensVL $ rootLens % #activeLoansFilterModel % #scene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radius 0
                      , border 1 black
                      ]
                ]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (filterScene == FilterScene) filterWidget
                    , widgetIf (filterScene == SortScene) sortWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ LendingEvent $ BorrowEvent ResetActiveLoansFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showActiveLoansFilter)
                        [onClick $ LendingEvent $ BorrowEvent CheckActiveLoansFilterModel]
                    ] `styleBasic` [padding 10]
                ] `styleBasic`
                    [ bgColor customGray3
                    , radiusTL 0
                    , radiusTR 10
                    , radiusBR 10
                    , radiusBL 10
                    , border 1 black
                    ]
            , filler
            ]
        ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
        , radius 10
        ]
  where
    filterWidget :: AppNode
    filterWidget = do
      let rootLens = #lendingModel % #borrowModel % #activeLoansFilterModel
          offStyle = def 
            `styleBasic` [ bgColor customGray1 , textColor white ]
            `styleHover` [ bgColor customBlue ]
          choiceButton caption field targetLens =
            centerWidgetV $ optionButton_ caption field targetLens
              [optionButtonOffStyle offStyle]
              `styleBasic` 
                [ bgColor customBlue
                , textColor white
                , radius 10
                , border 1 black
                , paddingT 2
                , paddingB 2
                , paddingL 7
                , paddingR 7
                , textSize 12
                ]
      centerWidget $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert askCfgLoanAmountMsg] $
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
            , label "Loan Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #loanAsset) 
                  [placeholder "ADA"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert askCfgMinDurationMsg] $
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
            , label "Minimum Duration (Days):"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #minDuration) 
                  [placeholder "10"] 
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , filler
            , box_ [alignMiddle, onClick $ Alert askCfgMaxDurationMsg] $
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
            , label "Maximum Duration (Days):"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #maxDuration) 
                  [placeholder "100"] 
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert "Whether the payment phase of the loan has expired."] $
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
            , label "Loan is Expired:"
                `styleBasic` [textSize 10]
            , spacer_ [width 2]
            , choiceButton "Yes" (Just True) (toLensVL $ rootLens % #loanExpired)
            , choiceButton "No" (Just False) (toLensVL $ rootLens % #loanExpired)
            , choiceButton "Either" Nothing (toLensVL $ rootLens % #loanExpired)
            , filler
            , box_ [alignMiddle, onClick $ Alert "Whether the lender's claim period has expired."] $
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
            , label "Claim is Expired:"
                `styleBasic` [textSize 10]
            , spacer_ [width 2]
            , choiceButton "Yes" (Just True) (toLensVL $ rootLens % #claimExpired)
            , choiceButton "No" (Just False) (toLensVL $ rootLens % #claimExpired)
            , choiceButton "Either" Nothing (toLensVL $ rootLens % #claimExpired)
            ]
        , spacer
        , separatorLine `styleBasic` [fgColor darkGray]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert askCfgCollateralMsg] $
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
        , textArea (toLensVL $ rootLens % #collateral)
            `styleBasic` [textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        ]

    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 12, bgColor customGray2, border 1 black]
                `styleHover` [textSize 12, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 12, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 12, bgColor customGray1, border 1 customBlue]
          possibleSortingMethods = enumFrom ActiveLoansLexicographically
          rootLens = #lendingModel % #borrowModel % #activeLoansFilterModel
      vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Sort Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , hstack
            [ spacer_ [width 40]
            , label "Method:" `styleBasic` [textSize 14]
            , spacer
            , textDropdown_
                  (toLensVL $ rootLens % #sortingMethod) 
                  possibleSortingMethods
                  display 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 200
                  , border 1 black
                  , textSize 12
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 3]
            , box_ [onClick $ Alert activeLoansSortMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , padding 5
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ]
        , spacer
        , hstack
            [ spacer_ [width 40]
            , label "Order:" `styleBasic` [textSize 14]
            , spacer
            , textDropdown_
                  (toLensVL $ rootLens % #sortingDirection) 
                  sortingDirections
                  display 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 150
                  , border 1 black
                  , textSize 12
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
orderer :: SortDirection -> [LoanUTxO] -> [LoanUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: ActiveLoansSortMethod -> [LoanUTxO] -> [LoanUTxO]
sorter sortingMethod = 
  case sortingMethod of
    ActiveLoansLexicographically -> sortOn (view #utxoRef)
    ActiveLoansTime -> sortOn (view #blockTime)
    ActiveLoansBalance -> sortOn loanUTxOLoanBalance
    ActiveLoansInterest -> sortOn loanUTxOLoanInterest
    ActiveLoansDeadline -> sortOn loanUTxONextDeadline
    ActiveLoansRequiredPayment -> sortOn loanUTxORequiredPayment

filterer :: POSIXTime -> ReverseTickerMap -> ActiveLoansFilterModel -> [LoanUTxO] -> [LoanUTxO]
filterer currentTime reverseTickerMap ActiveLoansFilterModel{..} us = do
    u <- us
    let activeDatum@Loans.ActiveDatum{loanTerm,loanExpiration,claimExpiration} = 
          fromMaybe def $ loanUTxOActiveDatum u
        utxoCollateral = map (toNativeAsset . fst)
                       $ activeDatum ^. #collateralization % #unCollateralization
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) >= d) $ 
      readMaybe $ toString minDuration
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) <= d) $ 
      readMaybe $ toString maxDuration
    guard $ matchesAsset [toNativeAsset $ activeDatum ^. #loanAsset] loanAsset
    guard $ all (matchesAsset utxoCollateral) $ lines collateral
    guard $ maybe True ((toPlutusTime currentTime > loanExpiration) ==) loanExpired
    guard $ maybe True ((toPlutusTime currentTime > claimExpiration) ==) claimExpired
    return u
  where
    matchesAsset :: [NativeAsset] -> Text -> Bool
    matchesAsset xs searchTarget
      | searchTarget == "" = True
      | otherwise = flip any xs $ \NativeAsset{..} -> or
          [ display policyId <> "." <> display tokenName == searchTarget
          , Just searchTarget ==
              fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap) 
          , policyId == "" && searchTarget == "ADA"
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

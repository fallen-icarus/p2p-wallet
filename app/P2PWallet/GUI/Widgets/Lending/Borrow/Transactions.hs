module P2PWallet.GUI.Widgets.Lending.Borrow.Transactions 
  ( 
    transactionsWidget
  , borrowerTxInspectionWidget
  ) where

import Monomer hiding (duration)
import Data.Time.Format qualified as Time

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

transactionsWidget :: AppModel -> AppNode
transactionsWidget model@AppModel{lendingModel} = do
    zstack 
      [ noTransactions `nodeVisible` null transactions
      , mainWidget model `nodeVisible` (transactions /= [])
      ]
  where
    LoanWallet{transactions} = lendingModel ^. #selectedWallet 

    noTransactions :: AppNode
    noTransactions = centerWidget $
      label "This wallet does not have any transactions yet"
        `styleBasic` [textFont "Italics"]

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{lendingModel=LendingModel{..},config} =
    zstack
      [ cushionWidgetH $ vstack
          [ centerWidgetH $ hstack
              [ label shownDateRange
                  `styleBasic` 
                    [ padding 0
                    , textMiddle
                    , textFont "Italics"
                    ]
              , spacer_ [width 5]
              , tooltip_ "Filter/Search" [tooltipDelay 0] $
                  toggleButton_ menuSearchIcon
                    (toLensVL $ #lendingModel % #borrowModel % #showTransactionFilter)
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
          , widgetIf (sample /= []) $ flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map txRow sample)
                `styleBasic` [padding 10]
          , widgetIf (null sample) $ 
              centerWidget $
                label "No transactions found"
                 `styleBasic` [textFont "Italics"]
          ]
      , txFilterWidget model `nodeVisible` (borrowModel ^. #showTransactionFilter)
      ]
  where
    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    timeZone :: TimeZone
    timeZone = config ^. #timeZone

    today :: Day
    today = config ^. #currentDay

    (mLowerDay,mUpperDay) = borrowModel ^. #txFilterModel % #dateRange

    startTime :: POSIXTime
    startTime = maybe 0 (localTimeToPosixTime timeZone . beginningOfDay) mLowerDay

    endTime :: POSIXTime
    endTime = localTimeToPosixTime timeZone 
            $ endOfDay
            $ fromMaybe today mUpperDay

    shownDateRange :: Text
    shownDateRange = unwords
      [ maybe "Beginning" (const $ showLocalDate timeZone startTime) mLowerDay 
      , "-"
      , maybe "Present" (const $ showLocalDate timeZone endTime) mUpperDay 
      ]

    withinDateRange :: Transaction -> Bool
    withinDateRange Transaction{blockTime} =
      blockTime >= startTime && blockTime <= endTime

    LoanWallet{transactions} = selectedWallet

    sample :: [Transaction]
    sample = --applySearchFilter selectedWallet reverseTickerMap txFilterModel 
      filter withinDateRange transactions

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      vstack
        [ hstack 
            [ copyableLabelSelf txHash 10 customBlue lightGray
            , spacer_ [width 2]
            , tooltip_ "Inspect Transaction" [tooltipDelay 0] $
                button inspectIcon (LendingEvent $ BorrowEvent $ InspectBorrowerTransaction tx)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 0
                    , paddingL 2
                    , paddingR 2
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , label calendarIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalDate (config ^. #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor white
                  ]
            , spacer_ [width 5]
            , label clockIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalTime (config ^. #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor white
                  ]
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

txFilterWidget :: AppModel -> AppNode
txFilterWidget model = do
  let currentScene = model ^. #lendingModel % #borrowModel % #txFilterScene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene (toLensVL $ #lendingModel % #borrowModel % #txFilterScene) 
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
                ] `styleBasic` [height 50]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (currentScene == FilterScene) filterWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ LendingEvent $ BorrowEvent ResetBorrowTxFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ #lendingModel % #borrowModel % #showTransactionFilter)
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
      let rootLens = #lendingModel % #borrowModel % #txFilterModel
      vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , vstack
            [ hstack 
                [ spacer_ [width 20]
                , label "Start Date:"
                , spacer
                , textField_ (toLensVL $ rootLens % lowerBoundText) [placeholder "MM-DD-YYYY"]
                    `styleBasic` [width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
                , mainButton helpIcon (Alert txStartDateMsg)
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ]
            , spacer
            , hstack 
                [ spacer_ [width 20]
                , label "End Date:"
                , spacer
                , textField_ (toLensVL $ rootLens % upperBoundText) [placeholder "MM-DD-YYYY"]
                    `styleBasic` [width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
                , mainButton helpIcon (Alert txEndDateMsg)
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ]
            ]
        ]

borrowerTxInspectionWidget :: AppModel -> AppNode
borrowerTxInspectionWidget model = do
    flip styleBasic [bgColor $ black & #a .~ 0.4, padding 30, radius 10] $ box $
      vstack
        [ centerWidgetH $ label "Loan Transaction"
            `styleBasic` [ textFont "Italics" ]
        , spacer
        , vscroll_ [wheelRate 50] $ vstack
            [ loanInputs model
            , spacer
            , loanOutputs model
            ]
        , filler
        , hstack
            [ filler
            , button "Close" $ LendingEvent $ BorrowEvent CloseInspectedBorrowerTransaction
            ]
        ] `styleBasic` [bgColor customGray3, padding 30, radius 10]

loanInputs :: AppModel -> AppNode
loanInputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Inputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #lendingModel % #borrowModel % #inspectedBorrowerTransaction % toggleShow #showInputs)
              [toggleButtonOffStyle moreOffStyle]
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , radius 20
                , paddingT 2
                , paddingB 2
                , paddingR 5
                , paddingL 5
                , bgColor black
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              `nodeVisible` (allInputs /= [])
          , label "none" 
              `styleBasic` [textColor white, textSize 12]
              `nodeVisible` null allInputs
          ]
      , widgetIf showInputs $
          vstack_ [childSpacing] (map inputRow allInputs)
            `styleBasic` [padding 10]
      ]
  where
    Config{timeZone,network} = config

    LoanWallet{loanAddress} = lendingModel ^. #selectedWallet

    Transaction{showInputs, plutusContracts, inputs} = 
      fromMaybe def $ lendingModel ^. #borrowModel % #inspectedBorrowerTransaction

    allInputs = filter ((== Just loanAddress) . view #paymentAddress) plutusContracts

    inputRow :: TransactionPlutusContract -> AppNode
    inputRow TransactionPlutusContract{spendsInput,datum,redeemer} =
      case parseInlineLoanDatum <$> datum of
        Just (ActiveDatum activeDatum) -> 
          activeRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            activeDatum
            (fromMaybe Loans.Unlock $ decodeData @Loans.LoanRedeemer redeemer)
        Just (AskDatum askDatum) -> 
          askRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            askDatum
            (fromMaybe Loans.CloseOrUpdateAsk $ decodeData @Loans.LoanRedeemer redeemer)
        Just (OfferDatum offerDatum) -> 
          offerRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            offerDatum
            (fromMaybe Loans.CloseOrUpdateOffer $ decodeData @Loans.LoanRedeemer redeemer)
        _ -> spacer `nodeVisible` False

    collateralAssetWidget :: NativeAsset -> AppNode
    collateralAssetWidget asset = do
      hstack
        [ spacer_ [width 2]
        , copyableLabelSelf (showAssetNameOnly reverseTickerMap asset) 8 lightGray customBlue
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

    askRow :: TxOutRef -> Loans.AskDatum -> Loans.LoanRedeemer -> AppNode
    askRow utxoRef Loans.AskDatum{..} redeemer = do
      let loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          offeredCollateral = map toNativeAsset $ collateral ^. #unCollateral
      vstack
        [ hstack
            [ label ("Ask For " <> showAssetBalance True reverseTickerMap loanAmount)
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
            , filler
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
            , filler
            , label "Duration:"
                `styleBasic` [textSize 10, textColor white]
            , spacer_ [width 3]
            , label (show duration <> " Days")
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ label "Offered Collateral:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 3]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 offeredCollateral) $ 
                \asset -> hstack_ [childSpacing_ 3] $ map collateralAssetWidget asset
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

    collateralizationWidget :: NativeAsset -> (NativeAsset,Rational) -> AppNode
    collateralizationWidget loanAsset (collateralAsset, price) = do
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

    offerRow :: TxOutRef -> Loans.OfferDatum -> Loans.LoanRedeemer -> AppNode
    offerRow utxoRef Loans.OfferDatum{..} redeemer = do
      let loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
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
          prettyExpirationTime exprTime = unwords
            [ "Expires:"
            , showLocalDate timeZone exprTime
            , showLocalTime timeZone exprTime
            ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          payToAddress = either (const "error") fst $ plutusToBech32 network lenderAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
      vstack
        [ hstack
            [ label ("Offer For " <> showAssetBalance True reverseTickerMap loanAmount)
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
            , widgetMaybe offerExpiration $ \exprTime -> hstack
                [ spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ (prettyExpirationTime $ fromPlutusTime exprTime) [tooltipDelay 0] $
                      label expirationIcon
                        `styleBasic` 
                          [ textMiddle
                          , textFont "Remix"
                          , textSize 10
                          , textColor customRed
                          ]
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
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
            , filler
            , label "Duration:"
                `styleBasic` [textSize 10, textColor white]
            , spacer_ [width 3]
            , label (show duration <> " Days")
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
                \col -> hstack_ [childSpacing_ 3] $ map (collateralizationWidget loanAmount) col
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
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

    activeRow :: TxOutRef -> Loans.ActiveDatum -> Loans.LoanRedeemer -> AppNode
    activeRow utxoRef Loans.ActiveDatum{..} redeemer = do
      let TransactionUTxO{lovelace,nativeAssets} = fromMaybe def 
                                                 $ find ((==utxoRef) . view #utxoRef) inputs
          loanBalance = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
          expiration = fromPlutusTime loanExpiration
          claimDeadline = fromPlutusTime claimExpiration
          mNextEpochBoundary = (+lastEpochBoundary) <$> epochDuration
          nextPaymentDueDate = case mNextEpochBoundary of
            Nothing -> expiration
            Just nextEpochBoundary -> min (fromPlutusTime nextEpochBoundary) expiration
          amountDue = minPayment - totalEpochPayments
          nextPaymentSize
            | minPayment == 0 = loanBalance
            | amountDue < 0 = loanBalance & #quantity .~ 0
            | otherwise = loanBalance & #quantity .~ amountDue
          prettyExpirationTime = unlines
            [ unwords
                [ "Expires:"
                , showLocalDate (config ^. #timeZone) expiration
                , showLocalTime (config ^. #timeZone) expiration
                ]
            , unwords
                [ "Claim Expiration:"
                , showLocalDate (config ^. #timeZone) claimDeadline
                , showLocalTime (config ^. #timeZone) claimDeadline
                ]
            ]
          prettyNextPaymentDueDate = unwords
            [ "Next Deadline:"
            , ""
            , showLocalDate (config ^. #timeZone) nextPaymentDueDate
            , showLocalTime (config ^. #timeZone) nextPaymentDueDate
            ]
          prettyInterest 
            | loanInterest == 0 = "Interest-Free"
            | otherwise = unwords
                [ if compoundingInterest then "Compounding" else "Non-Compounding"
                , "Interest:"
                , displayPercentage (toRational loanInterest) <> "%"
                ]
          prettyNextPayment = unwords
            [ "Amount Required by Deadline:"
            , ""
            , showAssetBalance True reverseTickerMap nextPaymentSize
            ]
          prettyEpochDuration = flip (maybe "No Loan Epochs") epochDuration $ \freq ->
            unwords
              [ "Loan Epoch:"
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
            (lovelaceAsNativeAsset & #quantity .~ unLovelace lovelace) : nativeAssets
          lockedCollateral = 
            filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets
      vstack
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
                  box_ [alignMiddle , onClick $ CopyText $ display loanId] $
                    label keyNftIcon
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
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
            , filler
            , label prettyNextPaymentDueDate
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
                [ label prettyNextPayment
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

loanOutputs :: AppModel -> AppNode
loanOutputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Outputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #lendingModel % #borrowModel % #inspectedBorrowerTransaction % toggleShow #showOutputs)
              [toggleButtonOffStyle moreOffStyle]
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , radius 20
                , paddingT 2
                , paddingB 2
                , paddingR 5
                , paddingL 5
                , bgColor black
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              `nodeVisible` (allOutputs /= [])
          , label "none" 
              `styleBasic` [textColor white, textSize 12]
              `nodeVisible` null allOutputs
          ]
      , widgetIf showOutputs $
          vstack_ [childSpacing] (map outputRow allOutputs)
            `styleBasic` [padding 10]
      ]
  where
    Config{timeZone,network} = config

    LoanWallet{loanAddress} = lendingModel ^. #selectedWallet

    Transaction{showOutputs, outputs} = 
      fromMaybe def $ lendingModel ^. #borrowModel % #inspectedBorrowerTransaction

    allOutputs = filter ((== loanAddress) . view #paymentAddress) outputs

    outputRow :: TransactionUTxO -> AppNode
    outputRow output@TransactionUTxO{paymentAddress,inlineDatum}
      | paymentAddress /= loanAddress = spacer `nodeVisible` False
      | otherwise = case parseInlineLoanDatum <$> inlineDatum of
          Just (ActiveDatum activeDatum) -> activeRow output activeDatum
          Just (OfferDatum offerDatum) -> offerRow output offerDatum
          Just (AskDatum askDatum) -> askRow output askDatum
          _ -> spacer `nodeVisible` False

    collateralAssetWidget :: NativeAsset -> AppNode
    collateralAssetWidget asset = do
      hstack
        [ spacer_ [width 2]
        , copyableLabelSelf (showAssetNameOnly reverseTickerMap asset) 8 lightGray customBlue
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

    askRow :: TransactionUTxO -> Loans.AskDatum -> AppNode
    askRow TransactionUTxO{utxoRef} Loans.AskDatum{..} = do
      let loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          offeredCollateral = map toNativeAsset $ collateral ^. #unCollateral
      vstack
        [ hstack
            [ label ("Ask For " <> showAssetBalance True reverseTickerMap loanAmount)
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
            , filler
            , label "Duration:"
                `styleBasic` [textSize 10, textColor white]
            , spacer_ [width 3]
            , label (show duration <> " Days")
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ label "Offered Collateral:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 3]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 offeredCollateral) $ 
                \asset -> hstack_ [childSpacing_ 3] $ map collateralAssetWidget asset
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

    collateralizationWidget :: NativeAsset -> (NativeAsset,Rational) -> AppNode
    collateralizationWidget loanAsset (collateralAsset, price) = do
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
            , paddingB 1
            , radius 3
            , border 1 customGray1
            ]

    offerRow :: TransactionUTxO -> Loans.OfferDatum -> AppNode
    offerRow TransactionUTxO{utxoRef} Loans.OfferDatum{..} = do
      let loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
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
          prettyExpirationTime exprTime = unwords
            [ "Expires:"
            , showLocalDate timeZone exprTime
            , showLocalTime timeZone exprTime
            ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          payToAddress = either (const "error") fst $ plutusToBech32 network lenderAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
      vstack
        [ hstack
            [ label ("Offer For " <> showAssetBalance True reverseTickerMap loanAmount)
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
            , widgetMaybe offerExpiration $ \exprTime -> hstack
                [ spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ (prettyExpirationTime $ fromPlutusTime exprTime) [tooltipDelay 0] $
                      label expirationIcon
                        `styleBasic` 
                          [ textMiddle
                          , textFont "Remix"
                          , textSize 10
                          , textColor customRed
                          ]
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
            , label "Duration:"
                `styleBasic` [textSize 10, textColor white]
            , spacer_ [width 3]
            , label (show duration <> " Days")
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
                \col -> hstack_ [childSpacing_ 3] $ map (collateralizationWidget loanAmount) col
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
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
            , paddingB 1
            , radius 3
            , border 1 customGray1
            ]

    activeRow :: TransactionUTxO -> Loans.ActiveDatum -> AppNode
    activeRow TransactionUTxO{utxoRef,lovelace,nativeAssets} Loans.ActiveDatum{..} = do
      let loanBalance = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
          expiration = fromPlutusTime loanExpiration
          claimDeadline = fromPlutusTime claimExpiration
          mNextEpochBoundary = (+lastEpochBoundary) <$> epochDuration
          nextPaymentDueDate = case mNextEpochBoundary of
            Nothing -> expiration
            Just nextEpochBoundary -> min (fromPlutusTime nextEpochBoundary) expiration
          amountDue = minPayment - totalEpochPayments
          nextPaymentSize
            | minPayment == 0 = loanBalance
            | amountDue < 0 = loanBalance & #quantity .~ 0
            | otherwise = loanBalance & #quantity .~ amountDue
          prettyExpirationTime = unlines
            [ unwords
                [ "Expires:"
                , showLocalDate (config ^. #timeZone) expiration
                , showLocalTime (config ^. #timeZone) expiration
                ]
            , unwords
                [ "Claim Expiration:"
                , showLocalDate (config ^. #timeZone) claimDeadline
                , showLocalTime (config ^. #timeZone) claimDeadline
                ]
            ]
          prettyNextPaymentDueDate = unwords
            [ "Next Deadline:"
            , ""
            , showLocalDate (config ^. #timeZone) nextPaymentDueDate
            , showLocalTime (config ^. #timeZone) nextPaymentDueDate
            ]
          prettyInterest 
            | loanInterest == 0 = "Interest-Free"
            | otherwise = unwords
                [ if compoundingInterest then "Compounding" else "Non-Compounding"
                , "Interest:"
                , displayPercentage (toRational loanInterest) <> "%"
                ]
          prettyNextPayment = unwords
            [ "Amount Required by Deadline:"
            , ""
            , showAssetBalance True reverseTickerMap nextPaymentSize
            ]
          prettyEpochDuration = flip (maybe "No Loan Epochs") epochDuration $ \freq ->
            unwords
              [ "Loan Epoch:"
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
            (lovelaceAsNativeAsset & #quantity .~ unLovelace lovelace) : nativeAssets
          lockedCollateral = 
            filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets
      vstack
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
                  box_ [alignMiddle , onClick $ CopyText $ display loanId] $
                    label keyNftIcon
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
            , label prettyEpochDuration
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , widgetIf (isJust epochDuration) $ vstack
            [ spacer_ [width 3]
            , hstack
                [ label prettyNextPayment
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

moreOffStyle :: Style
moreOffStyle = 
  def `styleBasic` 
        [ bgColor black
        , textColor customBlue
        , radius 20
        , paddingT 2
        , paddingB 2
        , paddingR 5
        , paddingL 5
        ]
      `styleHover`
        [ bgColor customGray1]

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
lowerBoundText :: Lens' BorrowTxFilterModel Text
lowerBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: BorrowTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _1

    setLowerBoundText :: BorrowTxFilterModel -> Text -> BorrowTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _1 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

upperBoundText :: Lens' BorrowTxFilterModel Text
upperBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: BorrowTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _2

    setLowerBoundText :: BorrowTxFilterModel -> Text -> BorrowTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _2 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

-- | A lens to toggle the `show` field of the `Transaction`.
toggleShow :: Lens' Transaction Bool -> Lens' (Maybe Transaction) Bool
toggleShow finalLens = lens getToggleShow setToggleShow
  where
    getToggleShow :: Maybe Transaction -> Bool
    getToggleShow = maybe False (view finalLens)

    setToggleShow :: Maybe Transaction -> Bool -> Maybe Transaction
    setToggleShow maybeInfo b = fmap (set finalLens b) maybeInfo

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
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

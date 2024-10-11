module P2PWallet.GUI.Widgets.Options.Writer.Transactions 
  ( 
    transactionsWidget
  , inspectionWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map
import Data.Time.Format qualified as Time

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

transactionsWidget :: AppModel -> AppNode
transactionsWidget model@AppModel{optionsModel} = do
    zstack 
      [ noTransactions `nodeVisible` null transactions
      , mainWidget model `nodeVisible` (transactions /= [])
      ]
  where
    OptionsWallet{transactions} = optionsModel ^. #selectedWallet 

    noTransactions :: AppNode
    noTransactions = centerWidget $
      label "This wallet does not have any transactions yet"
        `styleBasic` [textFont "Italics"]

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{optionsModel=OptionsModel{..},config,reverseTickerMap} =
    zstack
      [ cushionWidgetH $ vstack
          [ centerWidgetH $ hstack
              [ label shownDateRange
                  `styleBasic` 
                    [ padding 0
                    , textMiddle
                    , textFont "Italics"
                    ]
              , spacer_ [width 2]
              , tooltip_ "Filter/Search" [tooltipDelay 0] $
                  toggleButton_ menuSearchIcon
                    (toLensVL $ #optionsModel % #writerModel % #showTransactionFilter)
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
      , txFilterWidget model `nodeVisible` (writerModel ^. #showTransactionFilter)
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

    (mLowerDay,mUpperDay) = writerModel ^. #txFilterModel % #dateRange

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

    OptionsWallet{optionsAddress,transactions} = selectedWallet

    sample :: [Transaction]
    sample = applySearchFilter selectedWallet reverseTickerMap (writerModel ^. #txFilterModel)
           $ filter withinDateRange transactions

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      let (closedCount, addressUpdateCount, executedCount, purchasedCount, createdCount) =
            optionsActionCount optionsAddress tx
      vstack
        [ hstack 
            [ copyableLabelSelf txHash 10 customBlue lightGray
            , spacer_ [width 2]
            , tooltip_ "Inspect Options Transaction" [tooltipDelay 0] $
                button inspectIcon (OptionsEvent $ OptionsWriterEvent $ InspectOptionsTransaction tx)
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
            , label (showLocalDate timeZone blockTime)
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
            , label (showLocalTime timeZone blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor white
                  ]
            ]
        , hstack
            [ label (show closedCount <> " Closed")
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label "/"
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label (show createdCount <> " Created")
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label "/"
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label (show addressUpdateCount <> " Address Update(s)")
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , filler
            , label (show executedCount <> " Executed")
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label "/"
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label (show purchasedCount <> " Purchased")
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
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
  let currentScene = model ^. #optionsModel % #writerModel % #txFilterModel % #scene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ #optionsModel % #writerModel % #txFilterModel % #scene) 
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
                , optionButton_ "Search" SearchScene 
                    (toLensVL $ #optionsModel % #writerModel % #txFilterModel % #scene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radiusBL 10
                      , radiusTL 0
                      , radiusTR 0
                      , radiusBR 0
                      , border 1 black
                      ]
                ] `styleBasic` [height 100]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (currentScene == FilterScene) filterWidget
                    , widgetIf (currentScene == SearchScene) searchWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ OptionsEvent $ OptionsWriterEvent ResetOptionsTxFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ #optionsModel % #writerModel % #showTransactionFilter)
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
      let rootLens = #optionsModel % #writerModel % #txFilterModel
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

    searchWidget :: AppNode
    searchWidget = do
      vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Search Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert optionsTxFilterOfferAssetMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , textField_ (toLensVL $ #optionsModel % #writerModel % #txFilterModel % #offerAsset)
                  [placeholder "Offer Asset"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer
            , label remixArrowRightLine 
                `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
            , spacer
            , textField_ (toLensVL $ #optionsModel % #writerModel % #txFilterModel % #askAsset)
                  [placeholder "Ask Asset"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , box_ [alignMiddle, onClick $ Alert optionsTxFilterAskAssetMsg] $
                label helpIcon
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
        , spacer_ [width 10]
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert optionsTxFilterPremiumAssetMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , textField_ (toLensVL $ #optionsModel % #writerModel % #txFilterModel % #premiumAsset)
                  [placeholder "Premium Asset"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        ]

inspectionWidget :: AppModel -> AppNode
inspectionWidget model = do
    flip styleBasic [bgColor $ black & #a .~ 0.4, padding 30, radius 10] $ box $
      vstack
        [ centerWidgetH $ label "Options Transaction"
            `styleBasic` [ textFont "Italics" ]
        , spacer
        , vscroll_ [wheelRate 50] $ vstack
            [ optionsInputs model
            , spacer
            , optionsOutputs model
            ]
        , filler
        , hstack
            [ filler
            , button "Close" $ OptionsEvent $ OptionsWriterEvent CloseInspectedOptionsTransaction
            ]
        ] `styleBasic` [bgColor customGray3, padding 30, radius 10]

optionsInputs :: AppModel -> AppNode
optionsInputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Inputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #optionsModel % #writerModel % #inspectedTransaction % toggleShow #showInputs)
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

    OptionsWallet{optionsAddress} = optionsModel ^. #selectedWallet

    Transaction{showInputs, plutusContracts} = 
      fromMaybe def $ optionsModel ^. #writerModel % #inspectedTransaction

    allInputs = filter ((== Just optionsAddress) . view #paymentAddress) plutusContracts

    inputRow :: TransactionPlutusContract -> AppNode
    inputRow TransactionPlutusContract{spendsInput,datum,redeemer} =
      case parseInlineOptionsDatum <$> datum of
        Just (OptionsActiveDatum activeDatum) -> 
          activeRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            activeDatum
            (fromMaybe Options.CloseOrUpdateProposal $ decodeData @Options.OptionsRedeemer redeemer)
        Just (OptionsProposalDatum proposalDatum) -> 
          proposalRow 
            (fromMaybe (TxOutRef "" 0) spendsInput) 
            proposalDatum
            (fromMaybe Options.CloseOrUpdateProposal $ decodeData @Options.OptionsRedeemer redeemer)
        _ -> spacer `nodeVisible` False

    termsRow 
      :: NativeAsset 
      -> NativeAsset 
      -> NativeAsset 
      -> Options.OptionsRedeemer
      -> Integer
      -> Options.Terms
      -> AppNode
    termsRow offerAsset askAsset premiumAsset redeemer targetIdx terms = do
      let purchasedIdx = case redeemer of
            Options.PurchaseContract i -> Just i
            _ -> Nothing
          Options.Terms{premium,expiration,strikePrice} = terms
          formattedPrice = 
            showPriceFormatted reverseTickerMap askAsset offerAsset $ toRational strikePrice
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAsset
            ]
          prettyPremium = 
            showAssetBalance True reverseTickerMap $ premiumAsset & #quantity .~ premium
      hstack
        [ spacer_ [width 2]
        , label ("Premium: " <> prettyPremium)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Price: " <> prettyPrice)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Expiration: " <> showLocalDate timeZone (fromPlutusTime $ expiration - 1))
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 $ if purchasedIdx == Just targetIdx then customBlue else customGray1
            ]

    proposalRow :: TxOutRef -> Options.ProposalDatum -> Options.OptionsRedeemer -> AppNode
    proposalRow utxoRef Options.ProposalDatum{..} redeemer = do
      let offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          premiumNativeAsset = toNativeAsset premiumAsset
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = 
            find ((==payToAddress) . view #paymentAddress) $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
      vstack
        [ hstack
            [ label ("Offer: " <> showAssetBalance True reverseTickerMap offerAmount)
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
            , label ("Ask Asset: " <> showAssetNameOnly reverseTickerMap askNativeAsset)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , label "Possible Terms:"
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        , hstack
            [ spacer
            , vstack $ 
                zipWith 
                  (termsRow offerAmount askNativeAsset premiumNativeAsset redeemer)
                  [0..] 
                  possibleTerms
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

    activeRow :: TxOutRef -> Options.ActiveDatum -> Options.OptionsRedeemer -> AppNode
    activeRow utxoRef Options.ActiveDatum{..} redeemer = do
      let offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askQuantity = roundUp $ toRational strikePrice * toRational offerQuantity
          askNativeAsset = toNativeAsset askAsset & #quantity .~ askQuantity
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = 
            find ((==payToAddress) . view #paymentAddress) $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
          formattedPrice = showPriceFormatted reverseTickerMap askNativeAsset offerAmount 
                         $ toRational strikePrice
          prettyPrice = mconcat
            [ "Strike Price: "
            , formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askNativeAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAmount
            ]
          prettyExpirationTime = unwords
            [ "Expiration:"
            , showLocalDate timeZone $ fromPlutusTime expiration
            , showLocalTime timeZone $ fromPlutusTime expiration
            ]
      vstack
        [ hstack
            [ label ("Offer: " <> showAssetBalance True reverseTickerMap offerAmount)
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
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Contract ID: " <> display contractId) [tooltipDelay 0] $
                  box_ [alignMiddle , onClick $ CopyText $ display contractId] $
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
            , label ("Ask: " <> showAssetBalance True reverseTickerMap askNativeAsset)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ label prettyPrice
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyExpirationTime
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

optionsOutputs :: AppModel -> AppNode
optionsOutputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Outputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #optionsModel % #writerModel % #inspectedTransaction % toggleShow #showOutputs)
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

    OptionsWallet{optionsAddress} = optionsModel ^. #selectedWallet

    Transaction{showOutputs, outputs} = 
      fromMaybe def $ optionsModel ^. #writerModel % #inspectedTransaction

    allOutputs = filter ((== optionsAddress) . view #paymentAddress) outputs

    outputRow :: TransactionUTxO -> AppNode
    outputRow TransactionUTxO{paymentAddress,utxoRef,inlineDatum}
      | paymentAddress /= optionsAddress = spacer `nodeVisible` False
      | otherwise = case parseInlineOptionsDatum <$> inlineDatum of
          Just (OptionsActiveDatum activeDatum) -> activeRow utxoRef activeDatum
          Just (OptionsProposalDatum proposalDatum) -> proposalRow utxoRef proposalDatum
          _ -> spacer `nodeVisible` False

    termsRow 
      :: NativeAsset 
      -> NativeAsset 
      -> NativeAsset 
      -> Options.Terms
      -> AppNode
    termsRow offerAsset askAsset premiumAsset terms = do
      let Options.Terms{premium,expiration,strikePrice} = terms
          formattedPrice = 
            showPriceFormatted reverseTickerMap askAsset offerAsset $ toRational strikePrice
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAsset
            ]
          prettyPremium = 
            showAssetBalance True reverseTickerMap $ premiumAsset & #quantity .~ premium
      hstack
        [ spacer_ [width 2]
        , label ("Premium: " <> prettyPremium)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Price: " <> prettyPrice)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Expiration: " <> showLocalDate timeZone (fromPlutusTime $ expiration - 1))
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

    proposalRow :: TxOutRef -> Options.ProposalDatum -> AppNode
    proposalRow utxoRef Options.ProposalDatum{..} = do
      let offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          premiumNativeAsset = toNativeAsset premiumAsset
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = 
            find ((==payToAddress) . view #paymentAddress) $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
      vstack
        [ hstack
            [ label ("Offer: " <> showAssetBalance True reverseTickerMap offerAmount)
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
            , label "Proposal Contract"
                `styleBasic` [textSize 12, textColor customBlue, textFont "Italics"]
            , filler
            , label ("Ask Asset: " <> showAssetNameOnly reverseTickerMap askNativeAsset)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , label "Possible Terms:"
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        , hstack
            [ spacer
            , vstack $ map (termsRow offerAmount askNativeAsset premiumNativeAsset) possibleTerms
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

    activeRow :: TxOutRef -> Options.ActiveDatum -> AppNode
    activeRow utxoRef Options.ActiveDatum{..} = do
      let offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askQuantity = roundUp $ toRational strikePrice * toRational offerQuantity
          askNativeAsset = toNativeAsset askAsset & #quantity .~ askQuantity
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = 
            find ((==payToAddress) . view #paymentAddress) $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
          formattedPrice = showPriceFormatted reverseTickerMap askNativeAsset offerAmount 
                         $ toRational strikePrice
          prettyPrice = mconcat
            [ "Strike Price: "
            , formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askNativeAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAmount
            ]
          prettyExpirationTime = unwords
            [ "Expiration:"
            , showLocalDate timeZone $ fromPlutusTime expiration
            , showLocalTime timeZone $ fromPlutusTime expiration
            ]
      vstack
        [ hstack
            [ label ("Offer: " <> showAssetBalance True reverseTickerMap offerAmount)
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
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Contract ID: " <> display contractId) [tooltipDelay 0] $
                  box_ [alignMiddle , onClick $ CopyText $ display contractId] $
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
            , label "Active Contract"
                `styleBasic` [textSize 12, textColor customBlue, textFont "Italics"]
            , filler
            , label ("Ask: " <> showAssetBalance True reverseTickerMap askNativeAsset)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ label prettyPrice
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyExpirationTime
                `styleBasic` [textSize 8, textColor lightGray]
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
lowerBoundText :: Lens' OptionsTxFilterModel Text
lowerBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: OptionsTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _1

    setLowerBoundText :: OptionsTxFilterModel -> Text -> OptionsTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _1 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

upperBoundText :: Lens' OptionsTxFilterModel Text
upperBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: OptionsTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _2

    setLowerBoundText :: OptionsTxFilterModel -> Text -> OptionsTxFilterModel
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
-- Helper Functions
-------------------------------------------------
optionsActionCount :: PaymentAddress -> Transaction -> (Int,Int,Int,Int,Int)
optionsActionCount optionsAddress Transaction{plutusContracts,outputs} = 
    (closedCount, addressUpdateCount, executedCount, purchasedCount, createdCount)
  where
    checkSpend :: (Int,Int,Int,Int) -> TransactionPlutusContract -> (Int,Int,Int,Int)
    checkSpend acc@(closed,addressUpdate,executed,purchased) TransactionPlutusContract{..} =
      if Just optionsAddress /= paymentAddress then acc else
        case decodeData @Options.OptionsRedeemer redeemer of
          Nothing -> acc
          Just r -> case r of
            Options.CloseOrUpdateProposal -> (closed + 1, addressUpdate, executed, purchased)
            Options.CloseExpiredContract -> (closed + 1, addressUpdate, executed, purchased)
            Options.UpdatePaymentAddress _ _ -> (closed, addressUpdate + 1, executed, purchased)
            Options.ExecuteContract -> (closed, addressUpdate, executed + 1, purchased)
            Options.PurchaseContract _ -> (closed, addressUpdate, executed, purchased + 1)

    (closedCount, addressUpdateCount, executedCount, purchasedCount) = 
      foldl' checkSpend (0,0,0,0) plutusContracts

    checkCreation :: Int -> TransactionUTxO -> Int
    checkCreation acc TransactionUTxO{paymentAddress,inlineDatum} =
      if optionsAddress /= paymentAddress then acc else
        case decodeData @Options.ProposalDatum =<< inlineDatum of
          Nothing -> acc
          Just Options.ProposalDatum{} -> acc + 1

    createdCount = foldl' checkCreation 0 outputs

matchesUTxO 
  :: OptionsWallet
  -> ReverseTickerMap 
  -> OptionsTxFilterModel
  -> TransactionUTxO 
  -> Bool
matchesUTxO OptionsWallet{optionsAddress} reverseTickerMap OptionsTxFilterModel{..} TransactionUTxO{..} = 
    and
      [ optionsAddress == paymentAddress
      , matchesAsset offerSample offerAsset
      , matchesAsset askSample askAsset
      , matchesAsset premiumSample premiumAsset
      ]
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

    offerSample = catMaybes
      [ inlineDatum >>= decodeData @Options.ActiveDatum >>= \activeDatum ->
          return $ toNativeAsset $ activeDatum ^. #offerAsset
      , inlineDatum >>= decodeData @Options.ProposalDatum >>= \proposalDatum ->
          return $ toNativeAsset $ proposalDatum ^. #offerAsset
      ]

    askSample = catMaybes
      [ inlineDatum >>= decodeData @Options.ActiveDatum >>= \activeDatum ->
          return $ toNativeAsset $ activeDatum ^. #askAsset
      , inlineDatum >>= decodeData @Options.ProposalDatum >>= \proposalDatum ->
          return $ toNativeAsset $ proposalDatum ^. #askAsset
      ]

    premiumSample = catMaybes
      [ inlineDatum >>= decodeData @Options.ProposalDatum >>= \proposalDatum ->
          return $ toNativeAsset $ proposalDatum ^. #premiumAsset
      ]

applySearchFilter 
  :: OptionsWallet 
  -> ReverseTickerMap 
  -> OptionsTxFilterModel 
  -> [Transaction] 
  -> [Transaction]
applySearchFilter selectedWallet reverseTickerMap filterModel xs
  | noFilters = xs
  | otherwise = flip filter xs $ \Transaction{..} -> or
      [ any (matchesUTxO selectedWallet reverseTickerMap filterModel) inputs
      , any (matchesUTxO selectedWallet reverseTickerMap filterModel) outputs
      ]
  where
    noFilters = and
      [ filterModel ^. #offerAsset == ""
      , filterModel ^. #askAsset == ""
      , filterModel ^. #premiumAsset == ""
      ]

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

module P2PWallet.GUI.Widgets.Dex.Transactions 
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
import P2PWallet.Data.Core.Wallets.DexWallet
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

transactionsWidget :: AppModel -> AppNode
transactionsWidget model@AppModel{dexModel} = do
    zstack 
      [ noTransactions `nodeVisible` null transactions
      , mainWidget model `nodeVisible` (transactions /= [])
      ]
  where
    DexWallet{transactions} = dexModel ^. #selectedWallet 

    noTransactions :: AppNode
    noTransactions = centerWidget $
      label "This wallet does not have any transactions yet"
        `styleBasic` [textFont "Italics"]

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{dexModel=DexModel{..},config,reverseTickerMap} =
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
                    (toLensVL $ #dexModel % #showTransactionFilter)
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
      , txFilterWidget model `nodeVisible` (model ^. #dexModel % #showTransactionFilter)
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

    (mLowerDay,mUpperDay) = txFilterModel ^. #dateRange

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

    DexWallet{oneWaySwapAddress,twoWaySwapAddress,transactions} = selectedWallet

    sample :: [Transaction]
    sample = applySearchFilter selectedWallet reverseTickerMap txFilterModel 
           $ filter withinDateRange transactions

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      let (closedCount,createdCount,executedCount) = 
            swapActionCount oneWaySwapAddress twoWaySwapAddress tx
      vstack
        [ hstack 
            [ copyableLabelSelf txHash 10 customBlue lightGray
            , spacer_ [width 2]
            , tooltip_ "Inspect DEX Transaction" [tooltipDelay 0] $
                button inspectIcon (DexEvent $ InspectDexTransaction tx)
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
        , hstack
            [ label (show closedCount <> " Swap(s) Closed")
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
            , label (show createdCount <> " Swap(s) Created")
                `styleBasic` 
                  [ textSize 8
                  , textColor lightGray
                  ]
            , filler
            , label (show executedCount <> " Swap(s) Executed")
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
  let currentScene = model ^. #dexModel % #txFilterScene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene (toLensVL $ #dexModel % #txFilterScene) 
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
                , optionButton_ "Search" SearchScene (toLensVL $ #dexModel % #txFilterScene) 
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
                    , button "Reset" $ DexEvent ResetDexTxFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ #dexModel % #showTransactionFilter)
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
      let rootLens = #dexModel % #txFilterModel
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
        , box_ [alignMiddle] $ hstack
            [ box_ [alignMiddle, onClick $ Alert offerAssetTxFilterMsg] $
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
            , textField_ (toLensVL $ #dexModel % #txFilterModel % #offerAsset)
                  [placeholder "Offer Asset"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer
            , label remixArrowRightLine 
                `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
            , spacer
            , textField_ (toLensVL $ #dexModel % #txFilterModel % #askAsset)
                  [placeholder "Ask Asset"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , box_ [alignMiddle, onClick $ Alert askAssetTxFilterMsg] $
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
        ]

inspectionWidget :: AppModel -> AppNode
inspectionWidget model = do
    flip styleBasic [bgColor $ black & #a .~ 0.4, padding 30, radius 10] $ box $
      vstack
        [ centerWidgetH $ label "DEX Transaction"
            `styleBasic` [ textFont "Italics" ]
        , spacer
        , vscroll_ [wheelRate 50] $ vstack
            [ dexInputs model
            , spacer
            , dexOutputs model
            ]
        , filler
        , hstack
            [ filler
            , button "Close" $ DexEvent CloseInspectedDexTransaction
            ]
        ] `styleBasic` [bgColor customGray3, padding 30, radius 10]

dexInputs :: AppModel -> AppNode
dexInputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Inputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #dexModel % #inspectedTransaction % toggleShow #showInputs)
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
    DexWallet{oneWaySwapAddress,twoWaySwapAddress} = dexModel ^. #selectedWallet

    Transaction{showInputs, plutusContracts, inputs} = 
      fromMaybe def $ dexModel ^. #inspectedTransaction

    allInputs = flip filter plutusContracts $ \TransactionPlutusContract{paymentAddress} ->
      paymentAddress == Just oneWaySwapAddress || paymentAddress == Just twoWaySwapAddress

    inputRow :: TransactionPlutusContract -> AppNode
    inputRow TransactionPlutusContract{spendsInput,datum,redeemer} =
      case parseInlineSwapDatum <$> datum of
        Just (OneWayDatum oneWayDatum) -> 
          limitOrderRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            oneWayDatum
            (fromMaybe OneWay.SpendWithMint $ decodeData @OneWay.SwapRedeemer redeemer)
        Just (TwoWayDatum twoWayDatum) -> 
          liquiditySwapRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            twoWayDatum
            (fromMaybe TwoWay.SpendWithMint $ decodeData @TwoWay.SwapRedeemer redeemer)
        _ -> spacer `nodeVisible` False

    limitOrderRow :: TxOutRef -> OneWay.SwapDatum -> OneWay.SwapRedeemer -> AppNode
    limitOrderRow utxoRef OneWay.SwapDatum{..} redeemer = do
      let u = fromMaybe def $ find ((==utxoRef) . view #utxoRef) inputs
          offerAsset = updateQuantity u $ mkNativeAsset offerId offerName
          askAsset = updateQuantity u $ mkNativeAsset askId askName
          offerAssetName = showAssetNameOnly reverseTickerMap offerAsset
          askAssetName = showAssetNameOnly reverseTickerMap askAsset
          buyPrice = showPriceFormatted reverseTickerMap askAsset offerAsset 
                   $ toGHC swapPrice
          sellPrice = showPriceFormatted reverseTickerMap offerAsset askAsset 
                    $ 1 / toGHC swapPrice
          buyPriceCaption = unwords
            [ "Buy"
            , askAssetName
            , "@"
            , sellPrice
            , offerAssetName
            , "/"
            , askAssetName
            ]
          sellPriceCaption = unwords
            [ "Sell"
            , offerAssetName
            , "@"
            , buyPrice
            , askAssetName
            , "/"
            , offerAssetName
            ]
      vstack
        [ hstack 
            [ label "Limit Order:"
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 5]
            , label offerAssetName
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 3]
            , label remixArrowRightLine 
                `styleBasic` [textSize 12, textMiddle, textFont "Remix", textColor customBlue]
            , spacer_ [width 3]
            , label askAssetName
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
            , label (showAssetBalance True reverseTickerMap offerAsset)
                `styleBasic` [textSize 10, textColor customBlue]
            ]
        , spacer_ [width 2]
        , hstack
            [ label sellPriceCaption
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 10]
            , separatorLine
            , spacer_ [width 10]
            , label buyPriceCaption
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Converted: " <> showAssetBalance True reverseTickerMap askAsset)
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

    liquiditySwapRow :: TxOutRef -> TwoWay.SwapDatum -> TwoWay.SwapRedeemer -> AppNode
    liquiditySwapRow utxoRef TwoWay.SwapDatum{..} redeemer = do
      let u = fromMaybe def $ find ((==utxoRef) . view #utxoRef) inputs
          asset1 = updateQuantity u $ mkNativeAsset asset1Id asset1Name
          asset2 = updateQuantity u $ mkNativeAsset asset2Id asset2Name
          asset1AssetName = showAssetNameOnly reverseTickerMap asset1
          asset2AssetName = showAssetNameOnly reverseTickerMap asset2
          sellAsset1Price = 
            showPriceFormatted reverseTickerMap asset1 asset2 $ toGHC asset2Price
          sellAsset2Price = 
            showPriceFormatted reverseTickerMap asset2 asset1 $ toGHC asset1Price
          buyAsset1Price = 
            showPriceFormatted reverseTickerMap asset2 asset1 $ 1 / toGHC asset2Price
          buyAsset2Price = 
            showPriceFormatted reverseTickerMap asset1 asset2 $ 1 / toGHC asset1Price
          buyPriceCaption w x y z = 
            fromString $ printf "Buy %s @ %s %s / %s" w x y z
          sellPriceCaption w x y z = 
            fromString $ printf "Sell %s @ %s %s / %s" w x y z
      vstack
        [ hstack 
            [ label "Liquidity Swap:"
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 5]
            , label asset1AssetName
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 3]
            , label twoWayIcon
                `styleBasic` [textSize 12, textMiddle, textFont "Remix", textColor customBlue]
            , spacer_ [width 3]
            , label asset2AssetName
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
            , label (showAssetBalance True reverseTickerMap asset1)
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 3]
            , label twoWayIcon
                `styleBasic` [textSize 12, textMiddle, textFont "Remix", textColor customBlue]
            , spacer_ [width 3]
            , label (showAssetBalance True reverseTickerMap asset2)
                `styleBasic` [textSize 10, textColor customBlue]
            ]
        , spacer_ [width 2]
        , hstack
            [ label 
                (sellPriceCaption asset1AssetName sellAsset2Price asset2AssetName asset1AssetName)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label 
                (sellPriceCaption asset2AssetName sellAsset1Price asset1AssetName asset2AssetName)
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , spacer_ [width 2]
        , hstack
            [ label 
                (buyPriceCaption asset1AssetName buyAsset1Price asset2AssetName asset1AssetName)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label 
                (buyPriceCaption asset2AssetName buyAsset2Price asset1AssetName asset2AssetName)
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

dexOutputs :: AppModel -> AppNode
dexOutputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Outputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #dexModel % #inspectedTransaction % toggleShow #showOutputs)
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
    DexWallet{oneWaySwapAddress,twoWaySwapAddress} = dexModel ^. #selectedWallet

    Transaction{showOutputs, outputs} = 
      fromMaybe def $ dexModel ^. #inspectedTransaction

    allOutputs = flip filter outputs $ \TransactionUTxO{paymentAddress} ->
      paymentAddress == oneWaySwapAddress || paymentAddress == twoWaySwapAddress

    outputRow :: TransactionUTxO -> AppNode
    outputRow output@TransactionUTxO{paymentAddress,utxoRef,inlineDatum}
      | paymentAddress /= oneWaySwapAddress && paymentAddress /= twoWaySwapAddress 
      = spacer `nodeVisible` False
      | otherwise =
          case parseInlineSwapDatum <$> inlineDatum of
            Just (OneWayDatum oneWayDatum) -> limitOrderRow utxoRef oneWayDatum output
            Just (TwoWayDatum twoWayDatum) -> liquiditySwapRow utxoRef twoWayDatum output
            _ -> spacer `nodeVisible` False

    limitOrderRow :: TxOutRef -> OneWay.SwapDatum -> TransactionUTxO -> AppNode
    limitOrderRow utxoRef OneWay.SwapDatum{..} output = do
      let offerAsset = updateQuantity output $ mkNativeAsset offerId offerName
          askAsset = updateQuantity output $ mkNativeAsset askId askName
          offerAssetName = showAssetNameOnly reverseTickerMap offerAsset
          askAssetName = showAssetNameOnly reverseTickerMap askAsset
          buyPrice = showPriceFormatted reverseTickerMap askAsset offerAsset 
                   $ toGHC swapPrice
          sellPrice = showPriceFormatted reverseTickerMap offerAsset askAsset 
                    $ 1 / toGHC swapPrice
          buyPriceCaption = unwords
            [ "Buy"
            , askAssetName
            , "@"
            , sellPrice
            , offerAssetName
            , "/"
            , askAssetName
            ]
          sellPriceCaption = unwords
            [ "Sell"
            , offerAssetName
            , "@"
            , buyPrice
            , askAssetName
            , "/"
            , offerAssetName
            ]
      vstack
        [ hstack 
            [ label "Limit Order:"
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 5]
            , label offerAssetName
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 3]
            , label remixArrowRightLine 
                `styleBasic` [textSize 12, textMiddle, textFont "Remix", textColor customBlue]
            , spacer_ [width 3]
            , label askAssetName
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
            , label (showAssetBalance True reverseTickerMap offerAsset)
                `styleBasic` [textSize 10, textColor customBlue]
            ]
        , spacer_ [width 2]
        , hstack
            [ label sellPriceCaption
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 10]
            , separatorLine
            , spacer_ [width 10]
            , label buyPriceCaption
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Converted: " <> showAssetBalance True reverseTickerMap askAsset)
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

    liquiditySwapRow :: TxOutRef -> TwoWay.SwapDatum -> TransactionUTxO -> AppNode
    liquiditySwapRow utxoRef TwoWay.SwapDatum{..} output = do
      let asset1 = updateQuantity output $ mkNativeAsset asset1Id asset1Name
          asset2 = updateQuantity output $ mkNativeAsset asset2Id asset2Name
          asset1AssetName = showAssetNameOnly reverseTickerMap asset1
          asset2AssetName = showAssetNameOnly reverseTickerMap asset2
          sellAsset1Price = 
            showPriceFormatted reverseTickerMap asset1 asset2 $ toGHC asset2Price
          sellAsset2Price = 
            showPriceFormatted reverseTickerMap asset2 asset1 $ toGHC asset1Price
          buyAsset1Price = 
            showPriceFormatted reverseTickerMap asset2 asset1 $ 1 / toGHC asset2Price
          buyAsset2Price = 
            showPriceFormatted reverseTickerMap asset1 asset2 $ 1 / toGHC asset1Price
          buyPriceCaption w x y z = 
            fromString $ printf "Buy %s @ %s %s / %s" w x y z
          sellPriceCaption w x y z = 
            fromString $ printf "Sell %s @ %s %s / %s" w x y z
      vstack
        [ hstack 
            [ label "Liquidity Swap:"
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 5]
            , label asset1AssetName
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 3]
            , label twoWayIcon
                `styleBasic` [textSize 12, textMiddle, textFont "Remix", textColor customBlue]
            , spacer_ [width 3]
            , label asset2AssetName
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
            , label (showAssetBalance True reverseTickerMap asset1)
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 3]
            , label twoWayIcon
                `styleBasic` [textSize 12, textMiddle, textFont "Remix", textColor customBlue]
            , spacer_ [width 3]
            , label (showAssetBalance True reverseTickerMap asset2)
                `styleBasic` [textSize 10, textColor customBlue]
            ]
        , spacer_ [width 2]
        , hstack
            [ label 
                (sellPriceCaption asset1AssetName sellAsset2Price asset2AssetName asset1AssetName)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label 
                (sellPriceCaption asset2AssetName sellAsset1Price asset1AssetName asset2AssetName)
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , spacer_ [width 2]
        , hstack
            [ label 
                (buyPriceCaption asset1AssetName buyAsset1Price asset2AssetName asset1AssetName)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label 
                (buyPriceCaption asset2AssetName buyAsset2Price asset1AssetName asset2AssetName)
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
lowerBoundText :: Lens' DexTxFilterModel Text
lowerBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: DexTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _1

    setLowerBoundText :: DexTxFilterModel -> Text -> DexTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _1 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

upperBoundText :: Lens' DexTxFilterModel Text
upperBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: DexTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _2

    setLowerBoundText :: DexTxFilterModel -> Text -> DexTxFilterModel
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
-- | (# Swap Closed, # Swaps Created, # Swaps Executed). The count is combined for the two addresses
-- since the fact that there actually are two addresses is abstracted away from users.
swapActionCount :: PaymentAddress -> PaymentAddress -> Transaction -> (Int,Int,Int)
swapActionCount oneWayAddress twoWayAddress Transaction{plutusContracts,outputs} =
    (closedCount, createdCount, executedCount)
  where
    checkSpend :: (Int,Int) -> TransactionPlutusContract -> (Int,Int)
    checkSpend acc@(closed,executed) TransactionPlutusContract{paymentAddress,redeemer}
      | paymentAddress == Just oneWayAddress = case decodeData @OneWay.SwapRedeemer redeemer of
          Nothing -> acc
          Just r -> case r of
            OneWay.SpendWithStake -> (closed + 1, executed)
            OneWay.SpendWithMint -> (closed + 1, executed)
            OneWay.Swap -> (closed, executed + 1)
      | paymentAddress == Just twoWayAddress = case decodeData @TwoWay.SwapRedeemer redeemer of
          Nothing -> acc
          Just r -> case r of
            TwoWay.SpendWithStake -> (closed + 1, executed)
            TwoWay.SpendWithMint -> (closed + 1, executed)
            TwoWay.TakeAsset1 -> (closed, executed + 1)
            TwoWay.TakeAsset2 -> (closed, executed + 1)
      | otherwise = acc

    (closedCount, executedCount) = foldl' checkSpend (0,0) plutusContracts

    hasOneWayBeacons :: [NativeAsset] -> Bool
    hasOneWayBeacons = any ((== OneWay.beaconCurrencySymbol) . view #policyId)

    hasTwoWayBeacons :: [NativeAsset] -> Bool
    hasTwoWayBeacons = any ((== TwoWay.beaconCurrencySymbol) . view #policyId)

    checkCreation :: Int -> TransactionUTxO -> Int
    checkCreation acc TransactionUTxO{paymentAddress,nativeAssets,inlineDatum}
      | paymentAddress == oneWayAddress && hasOneWayBeacons nativeAssets =
          case (decodeData @OneWay.SwapDatum) =<< inlineDatum of
            Nothing -> acc
            Just OneWay.SwapDatum{prevInput} -> 
              case prevInput of
                Nothing -> acc + 1
                justRef ->
                  if justRef `notElem` map (view #spendsInput) plutusContracts
                  then acc + 1
                  else acc
      | paymentAddress == twoWayAddress && hasTwoWayBeacons nativeAssets =
          case (decodeData @TwoWay.SwapDatum) =<< inlineDatum of
            Nothing -> acc
            Just TwoWay.SwapDatum{prevInput} -> 
              case prevInput of
                Nothing -> acc + 1
                justRef ->
                  if justRef `notElem` map (view #spendsInput) plutusContracts
                  then acc + 1
                  else acc
      | otherwise = acc

    createdCount = foldl' checkCreation 0 outputs

matchesUTxO 
  :: DexWallet
  -> ReverseTickerMap 
  -> DexTxFilterModel
  -> TransactionUTxO 
  -> Bool
matchesUTxO DexWallet{..} reverseTickerMap DexTxFilterModel{..} TransactionUTxO{paymentAddress,inlineDatum} = 
    (paymentAddress == oneWaySwapAddress || paymentAddress == twoWaySwapAddress) && and
      [ matchesAsset offerSample offerAsset
      , matchesAsset askSample askAsset
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
      [ inlineDatum >>= decodeData @OneWay.SwapDatum >>= \OneWay.SwapDatum{..} ->
          return $ mkNativeAsset offerId offerName 
      , inlineDatum >>= decodeData @TwoWay.SwapDatum >>= \TwoWay.SwapDatum{..} ->
          return $ mkNativeAsset asset1Id asset1Name
      , inlineDatum >>= decodeData @TwoWay.SwapDatum >>= \TwoWay.SwapDatum{..} ->
          return $ mkNativeAsset asset2Id asset2Name
      ]
    askSample = catMaybes
      [ inlineDatum >>= decodeData @OneWay.SwapDatum >>= \OneWay.SwapDatum{..} ->
          return $ mkNativeAsset askId askName
      , inlineDatum >>= decodeData @TwoWay.SwapDatum >>= \TwoWay.SwapDatum{..} ->
          return $ mkNativeAsset asset1Id asset1Name
      , inlineDatum >>= decodeData @TwoWay.SwapDatum >>= \TwoWay.SwapDatum{..} ->
          return $ mkNativeAsset asset2Id asset2Name
      ]

applySearchFilter 
  :: DexWallet 
  -> ReverseTickerMap 
  -> DexTxFilterModel 
  -> [Transaction] 
  -> [Transaction]
applySearchFilter selectedWallet reverseTickerMap filterModel xs
  | filterModel ^. #offerAsset == "" && filterModel ^. #askAsset == "" = xs
  | otherwise = flip filter xs $ \Transaction{..} -> or
      [ any (matchesUTxO selectedWallet reverseTickerMap filterModel) inputs
      , any (matchesUTxO selectedWallet reverseTickerMap filterModel) outputs
      ]

-- | Update the native asset quantity to reflect the actual values present in the UTxO.
updateQuantity :: TransactionUTxO -> NativeAsset -> NativeAsset
updateQuantity TransactionUTxO{nativeAssets,lovelace} asset@NativeAsset{policyId,fingerprint}
  | policyId == "" = asset & #quantity .~ unLovelace lovelace
  | otherwise = flip (set #quantity) asset $ maybe 0 (view #quantity) $ flip find nativeAssets $
      \a -> a ^. #fingerprint == fingerprint

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


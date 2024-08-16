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
            , widgetIf (executedCount > 0) $ tooltip_ "Inspect Swap Executions" [tooltipDelay 0] $
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
                  [ textSize 10
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label "/"
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , spacer_ [width 5]
            , label (show createdCount <> " Swap(s) Created")
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , filler
            , label (show executedCount <> " Swap(s) Executed")
                `styleBasic` 
                  [ textSize 10
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

inspectionWidget :: ReverseTickerMap -> PaymentAddress -> PaymentAddress -> Transaction -> AppNode
inspectionWidget reverseTickerMap oneWayAddress twoWayAddress Transaction{..} = do
    flip styleBasic [bgColor $ black & #a .~ 0.4, padding 30, radius 10] $ box $
      vstack
        [ centerWidgetH $ label "Swap(s) Executed"
            `styleBasic` 
              [ textFont "Italics"
              , paddingB 14
              ]
        , vstack_ [childSpacing_ 5] $ map executionRow plutusContracts
        , filler
        , hstack
            [ filler
            , button "Close" $ DexEvent CloseInspectedDexTransaction
            ]
        ] `styleBasic` [bgColor customGray3, padding 30, radius 10]
  where
    executionRow :: TransactionPlutusContract -> AppNode
    executionRow TransactionPlutusContract{spendsInput,paymentAddress,redeemer} = do
      let parsedRedeemer
            | paymentAddress == Just oneWayAddress = 
                OneWayRedeemer <$> decodeData @OneWay.SwapRedeemer redeemer
            | paymentAddress == Just twoWayAddress =
                TwoWayRedeemer <$> decodeData @TwoWay.SwapRedeemer redeemer
            | otherwise = Nothing
          isExecutionRedeemer = or
            [ parsedRedeemer == Just (OneWayRedeemer OneWay.Swap)
            , parsedRedeemer == Just (TwoWayRedeemer TwoWay.TakeAsset1)
            , parsedRedeemer == Just (TwoWayRedeemer TwoWay.TakeAsset2)
            ]
      widgetIf isExecutionRedeemer $ do
        let input = find ((== spendsInput) . Just . view #utxoRef) inputs
            output = flip find outputs $ \o -> 
              (view #inlineDatum o >>= prevSwapInput . parseInlineSwapDatum) == spendsInput
            diffAssets = sumNativeAssets $ mconcat
              [ map (over #quantity negate) $ maybe [] (view #nativeAssets) input
              , maybe [] (view #nativeAssets) output
              ]
            diffLoves = maybe 0 (view #lovelace) output - maybe 0 (view #lovelace) input
        flip styleBasic [padding 5, bgColor customGray2, radius 5, border 1 black] $ 
          box_ [alignMiddle] $ vstack
            [ copyableLabelSelf (maybe "" display spendsInput) 10 customBlue lightGray
            , spacer_ [width 2]
            , hstack
                [ label "Taken:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 5]
                , widgetIf (diffLoves < 0) $
                    label (display $ abs diffLoves)
                      `styleBasic` [textSize 8, textColor lightGray]
                , widgetIf (diffLoves < 0 && any ((<0) . view #quantity) diffAssets) $
                    hstack
                      [ spacer_ [width 5]
                      , label "+"
                          `styleBasic` [textSize 8, textColor lightGray]
                      , spacer_ [width 5]
                      ]
                , widgetMaybe (find ((<0) . view #quantity) diffAssets) $ \asset ->
                    label (showAssetBalance True reverseTickerMap $ asset & #quantity %~ negate)
                      `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label "Deposited:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 5]
                , widgetIf (diffLoves > 0) $
                    label (display diffLoves)
                      `styleBasic` [textSize 8, textColor lightGray]
                , widgetIf (diffLoves > 0 && any ((>0) . view #quantity) diffAssets) $
                    hstack
                      [ spacer_ [width 5]
                      , label "+"
                          `styleBasic` [textSize 8, textColor lightGray]
                      , spacer_ [width 5]
                      ]
                , widgetMaybe (find ((>0) . view #quantity) diffAssets) $ \asset ->
                    label (showAssetBalance True reverseTickerMap asset)
                      `styleBasic` [textSize 8, textColor lightGray]
                ]
            ]

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


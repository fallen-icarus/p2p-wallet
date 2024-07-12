module P2PWallet.GUI.Widgets.Dex.Trade 
  ( 
    tradeWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

tradeWidget :: AppModel -> AppNode
tradeWidget model@AppModel{dexModel=DexModel{..}} = do
    zstack 
      [ getTradingPairWidget 
          `nodeVisible` (isNothing selectedTradingPair || choosingTradingPair)
      , mainWidget model
          `nodeVisible` (isJust selectedTradingPair && not choosingTradingPair)
      ]

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{dexModel=DexModel{..},reverseTickerMap} = do
    hstack
      [ orderBookWidget model
      , filler
      , entryWidget
      ] `styleBasic` [padding 20]
  where
    (offerAsset,askAsset) = fromMaybe (def, def) selectedTradingPair
    offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
    askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset

    entryWidget :: AppNode
    entryWidget = do
      let offStyle = def 
            `styleBasic` [ bgColor customGray4 , textColor lightGray ]
            `styleHover` [ textColor customBlue ]
      centerWidget $ vstack
        [ tooltip_ "Change Trading Pair" [tooltipDelay 0] $
            box_ [onClick $ DexEvent $ SetNewTradingPair $ StartAdding Nothing] $
              hstack
                [ label offerAssetName
                    `styleBasic` [textSize 12, textFont "Italics", textColor customBlue]
                , spacer
                , label remixArrowRightLine 
                    `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
                , spacer
                , label askAssetName
                    `styleBasic` [textSize 12, textFont "Italics", textColor customBlue]
                ] `styleBasic` [padding 5 , radius 5]
                  `styleHover` [bgColor customGray1]
        , separatorLine `styleBasic` [paddingL 75, paddingR 75, fgColor gray]
        , spacer
        , hgrid
            [ optionButton_ "Limit Order" True (toLensVL $ #dexModel % #creatingLimitOrder)
                [optionButtonOffStyle offStyle, onClick $ DexEvent ClearNewSwapForm]
                `styleBasic` 
                  [ bgColor customGray3
                  , textColor customBlue
                  , textSize 12
                  , radius 0
                  , border 0 transparent
                  ]
            , optionButton_ "Liquidity Swap" False (toLensVL $ #dexModel % #creatingLimitOrder)
                [optionButtonOffStyle offStyle, onClick $ DexEvent ClearNewSwapForm]
                `styleBasic` 
                  [ bgColor customGray3
                  , textColor customBlue
                  , textSize 12
                  , radius 0
                  , border 0 transparent
                  ]
            ]
        , zstack 
            [ widgetIf creatingLimitOrder $ createLimitOrderWidget model
            , widgetIf (not creatingLimitOrder) $ createLiquiditySwapWidget model
            ] `styleBasic` 
                [ bgColor customGray3
                , paddingT 0
                , paddingL 20
                , paddingR 20
                , paddingB 20
                , radius 0
                ]
        ] `styleBasic`
            [ bgColor customGray2
            , padding 10
            , radius 10
            , width 300
            ]

orderBookWidget :: AppModel -> AppNode
orderBookWidget AppModel{dexModel=DexModel{..}, reverseTickerMap} = do
    centerWidget $ vstack
      [ vstack
          [ sellSide `nodeVisible` (tmpSellSwaps /= [])
          , emptySellSide `nodeVisible` null tmpSellSwaps
          ] `styleBasic`
              [ bgColor customGray2
              , padding 10
              , radius 10
              ]
      , spacer_ [width 10]
      , spreadWidget
          `styleBasic`
            [ padding 10
            , radius 10
            , bgColor customGray3
            ]
      , spacer_ [width 10]
      , vstack
          [ buySide `nodeVisible` (tmpBuySwaps /= [])
          , emptyBuySide `nodeVisible` null tmpBuySwaps
          ] `styleBasic`
              [ bgColor customGray2
              , padding 10
              , radius 10
              ]
      ] 
  where
    (OfferAsset offerAsset,AskAsset askAsset) = fromMaybe (def, def) selectedTradingPair
    -- Saving the intermediate lists so that the spread can use the first element.
    tmpBuySwaps = fromMaybe [] 
                $ Map.lookup (OfferAsset offerAsset, AskAsset askAsset) cachedOrderBooks
    tmpSellSwaps = fromMaybe [] 
                 $ Map.lookup (OfferAsset askAsset, AskAsset offerAsset) cachedOrderBooks
    buySwaps = take orderBookSampleSize 
              $ drop (currentBidPage * orderBookSampleSize) tmpBuySwaps
    nextBuySwaps = take orderBookSampleSize 
                 $ drop ((1 + currentBidPage) * orderBookSampleSize) tmpBuySwaps
    sellSwaps = reverse 
              $ take orderBookSampleSize 
              $ drop (currentAskPage * orderBookSampleSize) tmpSellSwaps
    nextSellSwaps = reverse 
              $ take orderBookSampleSize 
              $ drop ((1 + currentAskPage) * orderBookSampleSize) tmpSellSwaps

    emptySellSide :: AppNode
    emptySellSide = 
      centerWidget $ flip styleBasic [radius 5, padding 5, bgColor customGray4] $ box $ 
        label "No open asks. Be the first!"
          `styleBasic` [textSize 12, textFont "Italics"]
      
    buySide :: AppNode
    buySide = do
      let previousPage = currentBidPage - 1
          nextPage = currentBidPage + 1
      vstack
        [ box_ [alignRight] $ hstack
            [ let shouldBeEnabled = previousPage /= -1 in 
                pageButton "Previous" previousPage (not shouldBeEnabled) #currentBidPage
                  `nodeEnabled` shouldBeEnabled
            , spacer_ [width 3]
            , let shouldBeEnabled = length buySwaps == orderBookSampleSize 
                                 && nextBuySwaps /= []
              in  pageButton "Next" nextPage (not shouldBeEnabled) #currentBidPage
                    `nodeEnabled` shouldBeEnabled
            ]
        , spacer_ [width 5]
        , vstack_ [childSpacing_ 5] (map buyRow buySwaps)
        ]

    sellSide :: AppNode
    sellSide = do
      let previousPage = currentAskPage - 1
          nextPage = currentAskPage + 1
      vstack
        [ box_ [alignRight] $ hstack
            [ let shouldBeEnabled = previousPage /= -1 in 
                pageButton "Previous" previousPage (not shouldBeEnabled) #currentAskPage
                  `nodeEnabled` shouldBeEnabled
            , spacer_ [width 3]
            , let shouldBeEnabled = length sellSwaps == orderBookSampleSize 
                                 && nextSellSwaps /= []
              in  pageButton "Next" nextPage (not shouldBeEnabled) #currentAskPage
                    `nodeEnabled` shouldBeEnabled
            ]
        , spacer_ [width 5]
        , vstack_ [childSpacing_ 5] (map sellRow sellSwaps) 
        ]

    emptyBuySide :: AppNode
    emptyBuySide = 
      centerWidget $ flip styleBasic [radius 5, padding 5, bgColor customGray4] $ box $ 
        label "No open bids. Be the first!"
          `styleBasic` [textSize 12, textFont "Italics"]
      
    buyRow :: SwapUTxO -> AppNode
    buyRow u@SwapUTxO{..} = do
      let offerAssetName = showAssetNameOnly reverseTickerMap offerAsset
          askAssetName = showAssetNameOnly reverseTickerMap askAsset
          mAskPerOffer = swapUTxOPrice (OfferAsset offerAsset) (AskAsset askAsset) u
      case mAskPerOffer of
        -- If `Nothing` is ever returned, there is a bug in cardano-swaps; the beacons were stored
        -- with an invalid datum.
        Nothing ->
          box_ [onClick $ Alert $ cardanoSwapsBugMsg u] $ 
            label "ERROR - click for more info" 
              `styleBasic` [ textColor customRed , textSize 8 ]
              `styleHover` [ cursorIcon CursorHand , textColor lightGray ]
        Just askPerOfferPrice -> do
          let prettyPrice = 
                -- Price needs to be in units of the ask asset, but it is in units of the
                -- offer asset so it needs to be inverted.
                showPriceFormatted reverseTickerMap offerAsset askAsset (1 / askPerOfferPrice)
              positionSize
                | offerAsset ^. #policyId == "" = unLovelace lovelace
                | otherwise = maybe 0 (view #quantity) $ flip find nativeAssets $
                    \NativeAsset{policyId} -> policyId == offerAsset ^. #policyId
              positionCaption = showAssetBalance True reverseTickerMap $ 
                askAsset & #quantity .~ round (fromInteger positionSize * askPerOfferPrice)
              priceCaption x = x <> " " <> offerAssetName <> " / " <> askAssetName
          hstack
            [ label positionCaption
                `styleBasic` [textSize 8, textColor customBlue]
            , filler
            , label (priceCaption prettyPrice)
                `styleBasic` [textSize 8, textColor customBlue]
            ] `styleBasic`
                [ padding 5
                , radius 5
                , bgColor customGray4
                ]
      
    sellRow :: SwapUTxO -> AppNode
    sellRow u@SwapUTxO{..} = do
      let offerAssetName = showAssetNameOnly reverseTickerMap offerAsset
          askAssetName = showAssetNameOnly reverseTickerMap askAsset
          mAskPerOffer = swapUTxOPrice (OfferAsset askAsset) (AskAsset offerAsset) u
      case mAskPerOffer of
        -- If `Nothing` is ever returned, there is a bug in cardano-swaps; the beacons were stored
        -- with an invalid datum.
        Nothing ->
          box_ [onClick $ Alert $ cardanoSwapsBugMsg u] $ 
            label "ERROR - click for more info" 
              `styleBasic` [ textColor customRed , textSize 8 ]
              `styleHover` [ cursorIcon CursorHand , textColor lightGray ]
        Just askPerOfferPrice -> do
          let prettyPrice = 
                -- The price is already in terms of the ask asset.
                showPriceFormatted reverseTickerMap offerAsset askAsset askPerOfferPrice
              positionSize
                | askAsset ^. #policyId == "" = unLovelace lovelace
                | otherwise = maybe 0 (view #quantity) $ flip find nativeAssets $
                    \NativeAsset{policyId} -> policyId == askAsset ^. #policyId
              positionCaption = showAssetBalance True reverseTickerMap $ 
                askAsset & #quantity .~ positionSize
              priceCaption x = x <> " " <> offerAssetName <> " / " <> askAssetName
          hstack
            [ label positionCaption
                `styleBasic` [textSize 8, textColor customRed]
            , filler
            , label (priceCaption prettyPrice)
                `styleBasic` [textSize 8, textColor customRed]
            ] `styleBasic`
                [ padding 5
                , radius 5
                , bgColor customGray4
                ]
      
    spreadWidget :: AppNode
    spreadWidget = do
      let mHighestBid = 
            maybeHead tmpBuySwaps >>= swapUTxOPrice (OfferAsset offerAsset) (AskAsset askAsset)
          mLowestAsk = 
            maybeHead tmpSellSwaps >>= swapUTxOPrice (OfferAsset askAsset) (AskAsset offerAsset)
          spreadLabel = case (mHighestBid,mLowestAsk) of
            (Nothing,_) -> 
              label "n/a" `styleBasic` [textColor lightGray, textSize 9, textFont "Italics"]
            (_,Nothing) -> 
              label "n/a" `styleBasic` [textColor lightGray, textSize 9, textFont "Italics"]
            (Just bidPrice, Just askPrice) -> do
              let spread = showPriceFormatted reverseTickerMap offerAsset askAsset $ 
                    askPrice - (1 / bidPrice)
                  offerAssetName = showAssetNameOnly reverseTickerMap offerAsset
                  askAssetName = showAssetNameOnly reverseTickerMap askAsset
              label (spread <> " " <> offerAssetName <> " / " <> askAssetName)
                `styleBasic` [textSize 9, textFont "Italics", textColor lightGray]
      hstack
        [ label "Spread:"
            `styleBasic` [textColor lightGray, textSize 9, textFont "Italics"]
        , spacer_ [width 10]
        , spreadLabel
        , filler
        , tooltip_ "Invert Order Book" [tooltipDelay 0] $
            box_ [alignMiddle, onClick $ DexEvent InvertOrderBook] $
              label remixArrowUpDownLine
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
        , spacer_ [width 5]
        , tooltip_ "Resync Order Book" [tooltipDelay 0] $
            box_ [alignMiddle, onClick $ DexEvent $ SyncOrderBook StartProcess] $
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

-- Scrolling doesn't work as required with the order book so pagination is used instead.
pageButton :: Text -> Int -> Bool -> Lens' DexModel Int -> AppNode
pageButton caption pageNum isDisabled pageLens = do
  let targetColor
        | isDisabled = darkGray
        | otherwise = customBlue
      offStyle = def 
        `styleBasic`
          [ bgColor customGray4
          , border 1 targetColor
          , textColor targetColor
          , padding 3
          , textSize 10
          ]
        `styleHover` [ bgColor customGray1 ]
  optionButton_ caption pageNum (toLensVL $ #dexModel % pageLens)
      [optionButtonOffStyle offStyle]
    `styleBasic`
      [ bgColor customGray4
      , border 1 targetColor
      , textColor targetColor
      , padding 3
      , textSize 10
      ]
    `styleHover` [bgColor customGray1]
      
createLimitOrderWidget :: AppModel -> AppNode
createLimitOrderWidget AppModel{dexModel=DexModel{..},reverseTickerMap} = do
  let NewSwapCreation{offerAsset,askAsset,tradingPairInverted} = newSwapCreation
      offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
      askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
      -- The price is always in units of the ask asset because that is what the rest of
      -- the order book is in units of.
      pricePlaceholder = offerAssetName <> " / " <> askAssetName
      quantityPlaceholder
        | tradingPairInverted = "# " <> askAssetName
        | otherwise = "# " <> offerAssetName
      buyOffStyle = def 
        `styleBasic` [ bgColor customGray4 , textColor lightGray ]
        `styleHover` [ textColor customBlue ]
      sellOffStyle = def 
        `styleBasic` [ bgColor customGray4 , textColor lightGray ]
        `styleHover` [ textColor customRed ]
  vstack
    [ spacer
    , centerWidgetH $ hstack
        [ label "What is a p2p limit order?"
            `styleBasic` [textSize 10, textFont "Italics"]
        , spacer_ [width 3]
        , box_ [alignMiddle, onClick $ Alert p2pLimitOrderMsg] $
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
        ]
    , spacer
    , centerWidgetH $ vstack
        [ centerWidgetH $ hstack
            [ optionButton_ ("Buy " <> askAssetName) False 
                  (toLensVL $ #dexModel % #newSwapCreation % #tradingPairInverted)
                  [onClick $ DexEvent ClearLimitOrderFields, optionButtonOffStyle buyOffStyle]
                `styleBasic` 
                  [ bgColor customBlue
                  , textColor white
                  , textSize 10
                  , radius 0
                  , border 0 transparent
                  ]
            , optionButton_ ("Sell " <> askAssetName) True 
                  (toLensVL $ #dexModel % #newSwapCreation % #tradingPairInverted)
                  [onClick $ DexEvent ClearLimitOrderFields, optionButtonOffStyle sellOffStyle]
                `styleBasic` 
                  [ bgColor customRed
                  , textColor white
                  , textSize 10
                  , radius 0
                  , border 0 transparent
                  ]
            ]
        , spacer
        , hstack 
            [ box_ [alignMiddle, onClick $ Alert limitPositionSizeMsg] $
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
            , label "Position Size:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ #dexModel % #newSwapCreation % #offerQuantity) 
                  [placeholder quantityPlaceholder] 
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ box_ [alignMiddle, onClick $ Alert limitPriceMsg] $
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
            , label "Limit Price:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ #dexModel % #newSwapCreation % #askPerOfferPrice) 
                  [placeholder pricePlaceholder]
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ box_ [alignMiddle, onClick $ Alert swapArbitrageFeeMsg] $
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
            , label "Arbitrage Fee:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ #dexModel % #newSwapCreation % #arbitrageFee) 
                  [placeholder "0.0"]
                `styleBasic` 
                  [ textSize 10
                  , width 50
                  , bgColor customGray1
                  , sndColor darkGray
                  , textRight
                  ]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , label "%"
                `styleBasic` [textColor lightGray, textMiddle, textFont "Bold", textSize 14]
            ]
        ]
    , spacer
    , box_ [alignRight] $ 
        mainButton "Create Limit Order" (DexEvent $ AddNewLimitOrderCreation StartProcess)
          `styleBasic`
            [textSize 10]
    ]
          
createLiquiditySwapWidget :: AppModel -> AppNode
createLiquiditySwapWidget AppModel{dexModel=DexModel{..},reverseTickerMap} = do
  let NewSwapCreation{..} = newSwapCreation
      offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
      askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
      pricePlaceholder = offerAssetName <> " / " <> askAssetName
      quantityPlaceholder x = "# " <> x
      buyPriceLabel = "Buy " <> askAssetName <> " Price:"
      sellPriceLabel = "Sell " <> askAssetName <> " Price:"
      depositLabel = fromString . printf "%s Deposit:"
      askQuantityLens = maybeLens "" (#dexModel % #newSwapCreation % #askQuantity)
      offerPerAskLens = maybeLens "" (#dexModel % #newSwapCreation % #offerPerAskPrice)
  vstack
    [ spacer
    , centerWidgetH $ hstack
        [ label "What is a p2p liquidity swap?"
            `styleBasic` [textSize 10, textFont "Italics"]
        , spacer_ [width 3]
        , box_ [alignMiddle, onClick $ Alert p2pLiquiditySwapMsg] $
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
        ]
    , spacer
    , centerWidgetH $ vstack
        [ hstack 
            [ box_ [alignMiddle, onClick $ Alert liquidityPositionSizeMsg] $
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
            , label (depositLabel offerAssetName)
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ #dexModel % #newSwapCreation % #offerQuantity) 
                  [placeholder $ quantityPlaceholder offerAssetName] 
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ box_ [alignMiddle, onClick $ Alert liquidityPositionSizeMsg] $
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
            , label (depositLabel askAssetName)
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL askQuantityLens)
                  [placeholder $ quantityPlaceholder askAssetName] 
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ box_ [alignMiddle, onClick $ Alert liquidityPriceMsg] $
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
            , label sellPriceLabel
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL offerPerAskLens) 
                  [placeholder pricePlaceholder]
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ box_ [alignMiddle, onClick $ Alert liquidityPriceMsg] $
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
            , label buyPriceLabel
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ #dexModel % #newSwapCreation % #askPerOfferPrice) 
                  [placeholder pricePlaceholder]
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        ]
    , spacer
    , box_ [alignRight] $ 
        mainButton "Create Liquidity Swap" (DexEvent $ AddNewLiquiditySwapCreation StartProcess)
          `styleBasic`
            [textSize 10]
    ]

getTradingPairWidget :: AppNode
getTradingPairWidget = do
  centerWidget $ vstack
    [ centerWidgetH $ label "Which trading pair would you like to lookup?"
    , spacer_ [width 20]
    , centerWidgetH $ hstack
        [ box_ [alignMiddle, onClick $ Alert offerAssetMsg] $
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
        , textField_ (toLensVL $ #dexModel % #newTradingPair % _1)
              [placeholder "Offer Asset"]
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer
        , label remixArrowRightLine 
            `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
        , spacer
        , textField_ (toLensVL $ #dexModel % #newTradingPair % _2)
              [placeholder "Ask Asset"]
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , box_ [alignMiddle, onClick $ Alert askAssetMsg] $
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
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ DexEvent $ SetNewTradingPair CancelAdding
        , spacer
        , mainButton "Confirm" $ DexEvent $ SetNewTradingPair ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, radius 10]

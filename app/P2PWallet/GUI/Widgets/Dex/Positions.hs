module P2PWallet.GUI.Widgets.Dex.Positions 
  ( 
    positionsWidget
  ) where

import Monomer
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

positionsWidget :: AppModel -> AppNode
positionsWidget model@AppModel{dexModel} = do
    zstack 
      [ noCurrentPositions `nodeVisible` null utxos
      , mainWidget model `nodeVisible` (utxos /= [])
      ]
  where
    DexWallet{utxos} = dexModel ^. #selectedWallet 

    noCurrentPositions :: AppNode
    noCurrentPositions = centerWidget $
      label "No open positions"
        `styleBasic` [textFont "Italics"]

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{dexModel=DexModel{..},reverseTickerMap,tickerMap,config} = do
    zstack
      [ cushionWidget $ vstack
          [ -- A header widget saying how many positions match that search out of the total as well
            -- as a filter button for opening the filter menu. 
            hstack 
              [ label ("Positions (" <> fractionShown <> ")")
                  `styleBasic` [textFont "Italics", textSize 14]
              , spacer_ [width 3]
              , tooltip_ "Sort/Filter/Search" [tooltipDelay 0] $
                  toggleButton_ menuSearchIcon
                    (toLensVL $ #dexModel % #showPositionsFilter)
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
              , spacer_ [width 50]
              , let rootLens = #dexModel % #positionsFilterModel in
                hstack
                  [ label "Status:" `styleBasic` [textSize 14]
                  , spacer
                  , hgrid_ [childSpacing_ 2]
                      [ choiceButton "Converted" (Just True) (toLensVL $ rootLens % #mustBeFullyConverted)
                      , choiceButton "Open" (Just False) (toLensVL $ rootLens % #mustBeFullyConverted)
                      , choiceButton "Either" Nothing (toLensVL $ rootLens % #mustBeFullyConverted)
                      ]
                  ]
              , filler
              ]
            -- These are the UTxOs that match those filter/search settings.
          , box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing_ 5] (map utxoRow sample)
                `styleBasic` [padding 10]
          ]
      , positionsFilterWidget model `nodeVisible` showPositionsFilter
      , updateSwapWidget model `nodeVisible` isJust newSwapUpdate
      ]
  where
    choiceOffStyle = def 
      `styleBasic` [ bgColor customGray1 , textColor white ]
      `styleHover` [ bgColor customBlue ]

    choiceButton caption field targetLens =
      optionButton_ caption field targetLens
        [optionButtonOffStyle choiceOffStyle]
        `styleBasic` 
          [ bgColor customBlue
          , border 0 transparent
          , textColor white
          , radius 0
          , textSize 8
          ]

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    sample :: [SwapUTxO]
    sample = orderer (positionsFilterModel ^. #sortingDirection)
           . sorter reverseTickerMap tickerMap positionsFilterModel
           . filterer reverseTickerMap positionsFilterModel
           $ selectedWallet ^. #utxos

    fractionShown :: Text
    fractionShown = 
      show (length sample) <> "/" <> show (length $ selectedWallet ^. #utxos)

    utxoRow :: SwapUTxO -> AppNode
    utxoRow u@SwapUTxO{swapDatum} = case swapDatum of
      Just (OneWayDatum datum) -> limitOrderRow u datum
      Just (TwoWayDatum datum) -> liquiditySwapRow u datum
      -- This path should never come about from a UTxO created by this wallet.
      -- TODO: Figure out what to do for invalid datums. They are just hidden for now.
      _ -> spacer `nodeVisible` False

    limitOrderRow :: SwapUTxO -> OneWay.SwapDatum -> AppNode
    limitOrderRow u@SwapUTxO{..} OneWay.SwapDatum{..} = do
      let offerAsset = updateQuantity u $ mkNativeAsset offerId offerName
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
          prettyLocalTime = unwords
            [ showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
      hstack
        [ vstack
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
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                    label clockIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        ]
                , widgetIf (swapIsFullyConverted u) $ 
                    hstack
                      [ filler
                      , label "Fully Converted"
                          `styleBasic` [textSize 10, textColor customRed, textFont "Italics"]
                      , filler
                      ]
                , filler
                , label (showAssetBalance True reverseTickerMap offerAsset)
                    `styleBasic` [textSize 10, textColor customBlue]
                ]
            , spacer_ [width 2]
            , hstack
                [ label sellPriceCaption
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 5]
                , label "=="
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 5]
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
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon (DexEvent $ AddSelectedSwapUpdate $ StartAdding $ Just u)
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
            , spacer_ [width 2]
            , separatorLine `styleBasic` [fgColor darkGray, paddingL 5, paddingR 5]
            , spacer_ [width 2]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Close" [tooltipDelay 0] $
                button closeCircleIcon (DexEvent $ AddSelectedSwapClose u)
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
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

    liquiditySwapRow :: SwapUTxO -> TwoWay.SwapDatum -> AppNode
    liquiditySwapRow u@SwapUTxO{..} TwoWay.SwapDatum{..} = do
      let asset1 = updateQuantity u $ mkNativeAsset asset1Id asset1Name
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
          prettyLocalTime = unwords
            [ showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
      hstack
        [ vstack
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
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                    label clockIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        ]
                , widgetIf (swapIsFullyConverted u) $ 
                    hstack
                      [ filler
                      , label "Fully Converted"
                          `styleBasic` [textSize 10, textColor customRed, textFont "Italics"]
                      , filler
                      ]
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
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon (DexEvent $ AddSelectedSwapUpdate $ StartAdding $ Just u)
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
            , spacer_ [width 2]
            , separatorLine `styleBasic` [fgColor darkGray, paddingL 5, paddingR 5]
            , spacer_ [width 2]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Close" [tooltipDelay 0] $
                button closeCircleIcon (DexEvent $ AddSelectedSwapClose u)
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
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

positionsFilterWidget:: AppModel -> AppNode
positionsFilterWidget AppModel{dexModel=DexModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene (toLensVL $ #dexModel % #positionsFilterScene) 
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
                , optionButton_ "Sort" SortScene (toLensVL $ #dexModel % #positionsFilterScene) 
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
                    [ widgetIf (positionsFilterScene == FilterScene) filterWidget
                    , widgetIf (positionsFilterScene == SortScene) sortWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ DexEvent ResetPositionsFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ #dexModel % #showPositionsFilter)
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
      let rootLens = #dexModel % #positionsFilterModel
          offStyle = def 
            `styleBasic` [ bgColor customGray1 , textColor white ]
            `styleHover` [ bgColor customBlue ]
          choiceButton caption field targetLens =
            optionButton_ caption field targetLens
              [optionButtonOffStyle offStyle]
              `styleBasic` 
                [ bgColor customBlue
                , border 0 transparent
                , textColor white
                , radius 5
                , textSize 12
                ]
      vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , box_ [alignMiddle] $ hstack
            [ box_ [alignMiddle, onClick $ Alert offerAssetPositionFilterMsg] $
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
            , textField_ (toLensVL $ #dexModel % #positionsFilterModel % #offerAsset)
                  [placeholder "Offer Asset"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer
            , label remixArrowRightLine 
                `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
            , spacer
            , textField_ (toLensVL $ #dexModel % #positionsFilterModel % #askAsset)
                  [placeholder "Ask Asset"]
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , box_ [alignMiddle, onClick $ Alert askAssetPositionFilterMsg] $
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
        , centerWidgetH $ hstack_ [childSpacing]
            [ label "Fully Converted:" `styleBasic` [textSize 12]
            , hgrid_ [childSpacing_ 3]
                [ choiceButton "Yes" (Just True) (toLensVL $ rootLens % #mustBeFullyConverted)
                , choiceButton "No" (Just False) (toLensVL $ rootLens % #mustBeFullyConverted)
                , choiceButton "Either" Nothing (toLensVL $ rootLens % #mustBeFullyConverted)
                ]
            , mainButton helpIcon (Alert fullyConvertedSwapMsg)
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , textFont "Remix"
                  , padding 0
                  ]
                `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ] `styleBasic` [height 30]
        ]

    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 12, bgColor customGray2, border 1 black]
                `styleHover` [textSize 12, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 12, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 12, bgColor customGray1, border 1 customBlue]
          offerAsset = positionsFilterModel ^. #offerAsset
          askAsset = positionsFilterModel ^. #askAsset
          possibleSortingMethods = mconcat
            [ [ PositionsTime, PositionsLexicographical ]
            , [ PositionsOfferQuantity | offerAsset /= "" ]
            , [ PositionsAskQuantity | askAsset /= "" ]
            , [ PositionsPrice | offerAsset /= "" && askAsset /= "" ]
            ]
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
                  (toLensVL $ #dexModel % #positionsFilterModel % #sortingMethod) 
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
            , box_ [onClick $ Alert positionsSortMsg] $
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
                  (toLensVL $ #dexModel % #positionsFilterModel % #sortingDirection) 
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

updateSwapWidget :: AppModel -> AppNode
updateSwapWidget model@AppModel{dexModel=DexModel{newSwapUpdate}} = 
  vstack
    [ centerWidget $ vstack
        [ case maybe LimitOrder (view #swapType . snd) newSwapUpdate of
            LimitOrder -> updateLimitOrderWidget model
            LiquiditySwap -> updateLiquiditySwapWidget model
        ] `styleBasic`
            [ bgColor customGray3
            , padding 20
            , radius 20
            ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
        , radius 10
        ]

updateLimitOrderWidget :: AppModel -> AppNode
updateLimitOrderWidget AppModel{dexModel=DexModel{..},reverseTickerMap} = do
  let NewSwapCreation{offerAsset,askAsset,tradingPairInverted} = maybe def snd newSwapUpdate
      maybeLens' = maybeLens (def,def) $ #dexModel % #newSwapUpdate
      offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
      askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
      -- The price is always in units of the ask asset because that is what the rest of
      -- the order book is in units of.
      pricePlaceholder = offerAssetName <> " / " <> askAssetName
      quantityPlaceholder
        | tradingPairInverted = "# " <> askAssetName
        | otherwise = "# " <> offerAssetName
  vstack
    [ spacer
    , box_ [alignMiddle] $
        label "Edit Limit Order"
          `styleBasic` [textSize 12, textFont "Italics"]
    , spacer
    , box_ [alignMiddle] $ vstack
        [ hstack 
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
            , textField_ (toLensVL $ maybeLens' % _2 % #offerQuantity) 
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
            , textField_ (toLensVL $ maybeLens' % _2 % #askPerOfferPrice) 
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
            , textField_ (toLensVL $ maybeLens' % _2 % #arbitrageFee) 
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
        hstack
          [ button "Cancel" (DexEvent $ AddSelectedSwapUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Update Limit Order" (DexEvent $ AddSelectedSwapUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ]
          
updateLiquiditySwapWidget :: AppModel -> AppNode
updateLiquiditySwapWidget AppModel{dexModel=DexModel{..},reverseTickerMap} = do
  let NewSwapCreation{offerAsset,askAsset} = maybe def snd newSwapUpdate
      maybeLens' = maybeLens (def,def) $ #dexModel % #newSwapUpdate
      offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
      askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
      pricePlaceholder = offerAssetName <> " / " <> askAssetName
      quantityPlaceholder x = "# " <> x
      buyPriceLabel = "Buy " <> askAssetName <> " Price:"
      sellPriceLabel = "Sell " <> askAssetName <> " Price:"
      depositLabel = fromString . printf "%s Deposit:"
      askQuantityLens = maybeLens "" (maybeLens' % _2 % #askQuantity)
      offerPerAskLens = maybeLens "" (maybeLens' % _2 % #offerPerAskPrice)
  vstack
    [ spacer
    , box_ [alignMiddle] $ 
        label "Edit Liquidity Swap"
          `styleBasic` [textSize 12, textFont "Italics"]
    , spacer
    , box_ [alignMiddle] $ vstack
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
            , textField_ (toLensVL $ maybeLens' % _2 % #offerQuantity) 
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
            , textField_ (toLensVL $ maybeLens' % _2 % #askPerOfferPrice) 
                  [placeholder pricePlaceholder]
                `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        ]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (DexEvent $ AddSelectedSwapUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Update Limit Order" (DexEvent $ AddSelectedSwapUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the native asset quantity to reflect the actual values present in the UTxO.
updateQuantity :: SwapUTxO -> NativeAsset -> NativeAsset
updateQuantity SwapUTxO{nativeAssets,lovelace} asset@NativeAsset{policyId,fingerprint}
  | policyId == "" = asset & #quantity .~ unLovelace lovelace
  | otherwise = flip (set #quantity) asset $ maybe 0 (view #quantity) $ flip find nativeAssets $
      \a -> a ^. #fingerprint == fingerprint

targetQuantity :: ReverseTickerMap -> Text -> SwapUTxO -> Maybe Integer
targetQuantity reverseTickerMap target p =
  fmap (view #quantity) $
    flip find (p ^. #nativeAssets) $ \NativeAsset{..} -> or
      [ display policyId <> "." <> display tokenName == target
      , Just target == fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap)
      ]

sorter :: ReverseTickerMap -> TickerMap -> PositionsFilterModel -> [SwapUTxO] -> [SwapUTxO]
sorter reverseTickerMap tickerMap PositionsFilterModel{offerAsset,askAsset,sortingMethod} = 
  case sortingMethod of
    PositionsLexicographical -> sortOn (view #utxoRef)
    PositionsTime -> sortOn (view #blockTime)
    PositionsOfferQuantity -> sortOn (targetQuantity reverseTickerMap offerAsset)
    PositionsAskQuantity -> sortOn (targetQuantity reverseTickerMap askAsset)
    PositionsPrice -> 
      let offerAsset' = OfferAsset $ fromMaybe def $ 
            rightToMaybe $ parseNativeAssetName tickerMap offerAsset 
          askAsset' = AskAsset $ fromMaybe def $ 
            rightToMaybe $ parseNativeAssetName tickerMap askAsset
      in sortOn (swapUTxOPrice offerAsset' askAsset')

orderer :: SortDirection -> [SwapUTxO] -> [SwapUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

filterer :: ReverseTickerMap -> PositionsFilterModel -> [SwapUTxO] -> [SwapUTxO]
filterer reverseTickerMap PositionsFilterModel{..} us = do
    u <- us
    let offerSample = catMaybes
          [ swapUTxOOfferAsset u
          , swapUTxOAsset1 u
          , swapUTxOAsset2 u
          ]
        askSample = catMaybes
          [ swapUTxOAskAsset u
          , swapUTxOAsset1 u
          , swapUTxOAsset2 u
          ]
    guard $ matchesAsset offerSample offerAsset
    guard $ matchesAsset askSample askAsset
    guard $ maybe True (swapIsFullyConverted u ==) mustBeFullyConverted
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

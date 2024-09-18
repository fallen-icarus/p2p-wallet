module P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapCreations
  ( 
    swapCreationsList
  , editSwapCreationWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

swapCreationsList :: ReverseTickerMap -> [(Int,SwapCreation)] -> [AppNode]
swapCreationsList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,SwapCreation) -> AppNode
    utxoRow s@(_,SwapCreation{swapType}) = case swapType of
      LimitOrder -> limitOrderRow s
      LiquiditySwap -> liquiditySwapRow s

    limitOrderRow :: (Int,SwapCreation) -> AppNode
    limitOrderRow s@(idx,SwapCreation{..}) = do
      let offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
          askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
          positionSize
            | tradingPairInverted = showAssetBalance True reverseTickerMap $ unAskAsset askAsset
            | otherwise = showAssetBalance True reverseTickerMap $ unOfferAsset offerAsset
          price
            | tradingPairInverted =
                showPriceFormatted
                  reverseTickerMap 
                  (unOfferAsset offerAsset) 
                  (unAskAsset askAsset) 
                  askPerOfferPrice
            | otherwise =
                showPriceFormatted
                  reverseTickerMap 
                  (unOfferAsset offerAsset) 
                  (unAskAsset askAsset) 
                  (1 / askPerOfferPrice)
          priceCaption
            | tradingPairInverted = fromString $
                printf "Sell %s Price: %s %s / %s" askAssetName price offerAssetName askAssetName 
            | otherwise = fromString $ 
                printf "Buy %s Price: %s %s / %s" askAssetName price offerAssetName askAssetName 
          arbitrageFeeAsPercent =
            fromString $ printf "Arbitrage Fee: %s%%" $ displayPercentage arbitrageFee
      hstack
        [ vstack
            [ hstack
                [ label "Create New Limit Order"
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label positionSize
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label priceCaption
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label arbitrageFeeAsPercent
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , hstack
            [ vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                    button editIcon 
                        (swapBuilderEvent $ EditSelectedSwapCreation $ StartAdding $ Just s)
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
                ]
            , spacer_ [width 2]
            , countWidget idx count
            , spacer_ [width 2]
            , vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                    button closeCircleIcon (swapBuilderEvent $ RemoveSelectedSwapCreation idx)
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
        ]

    liquiditySwapRow :: (Int,SwapCreation) -> AppNode
    liquiditySwapRow s@(idx,SwapCreation{..}) = do
      let offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
          askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
          offerPositionSize = showAssetBalance True reverseTickerMap $ unOfferAsset offerAsset
          askPositionSize = showAssetBalance True reverseTickerMap $ unAskAsset askAsset
          askPerOfferDecimal = 
            showPriceFormatted
              reverseTickerMap
              (unOfferAsset offerAsset)
              (unAskAsset askAsset)
              (1 / askPerOfferPrice)
          offerPerAskDecimal = 
            showPriceFormatted
              reverseTickerMap
              (unOfferAsset offerAsset)
              (unAskAsset askAsset)
              (fromMaybe 0 offerPerAskPrice)
          askPerOfferCaption =
            fromString $
              printf "Buy %s Price: %s %s / %s" askAssetName askPerOfferDecimal offerAssetName askAssetName
          offerPerAskCaption =
            fromString $
              printf "Sell %s Price: %s %s / %s" askAssetName offerPerAskDecimal offerAssetName askAssetName
      hstack
        [ vstack
            [ hstack
                [ label "Create New Liquidity Swap"
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label offerAssetName
                    `styleBasic` [textSize 10, textColor white]
                , spacer_ [width 5]
                , label twoWayIcon
                    `styleBasic` [textMiddle, textSize 10, textColor white, textFont "Remix"]
                , spacer_ [width 5]
                , label askAssetName
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label offerPerAskCaption
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label askPositionSize
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            , spacer_ [width 2]
            , hstack
                [ label askPerOfferCaption
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label offerPositionSize
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignMiddle] $ hstack
            [ vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                    button editIcon 
                        (swapBuilderEvent $ EditSelectedSwapCreation $ StartAdding $ Just s)
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
                ]
            , spacer_ [width 2]
            , countWidget idx count
            , spacer_ [width 2]
            , vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                    button closeCircleIcon (swapBuilderEvent $ RemoveSelectedSwapCreation idx)
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
        ]

    countWidget :: Int -> Int -> AppNode
    countWidget idx count = do
      let upperCount = count + 1
          lowerCount = count - 1
      vstack
        [ box_ [onClick $ swapBuilderEvent $ ChangeSwapCreationCount idx upperCount] $
            label ascendingSortIcon
              `styleBasic`
                [ textSize 14
                , textColor lightGray
                , padding 0
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover`
                [ textColor customBlue
                , cursorIcon CursorHand
                ]
        , flip styleBasic [padding 3, bgColor customGray2] $ 
            box $ label (show count) `styleBasic` [textSize 12, padding 0, textColor customBlue]
        , flip nodeEnabled (lowerCount > 0) $
            box_ [onClick $ swapBuilderEvent $ ChangeSwapCreationCount idx lowerCount] $
              label descendingSortIcon
                `styleBasic`
                  [ textSize 14
                  , textColor $ if lowerCount > 0 then lightGray else customGray1
                  , padding 0
                  , textMiddle
                  , textFont "Remix"
                  ]
                `styleHover`
                  [ textColor customBlue
                  , cursorIcon CursorHand
                  ]
        ]

editSwapCreationWidget :: AppModel -> AppNode
editSwapCreationWidget model@AppModel{txBuilderModel=TxBuilderModel{..}} = do
  let NewSwapCreation{swapType} = maybe def snd $ swapBuilderModel ^. #targetSwapCreation
  centerWidget $ vstack
    [ -- This is deliberately written as a `case` to have the compiler prompt this location when
      -- new swap types are added.
      case swapType of
        LimitOrder -> editLimitOrderWidget model
        LiquiditySwap -> editLiquiditySwapWidget model
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

editLimitOrderWidget :: AppModel -> AppNode
editLimitOrderWidget AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #swapBuilderModel % #targetSwapCreation)
      NewSwapCreation{..} = maybe def snd $ swapBuilderModel ^. #targetSwapCreation
      offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
      askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
      actionLabel
        | tradingPairInverted =
            label ("Sell " <> askAssetName) 
              `styleBasic` [textSize 12, textColor customRed]
        | otherwise =
            label ("Buy " <> askAssetName) 
              `styleBasic` [textSize 12, textColor customBlue]
      pricePlaceholder = offerAssetName <> " / " <> askAssetName
      quantityPlaceholder
        | tradingPairInverted = "# " <> askAssetName
        | otherwise = "# " <> offerAssetName
  vstack
    [ centerWidgetH $ label "Edit Limit Order"
    , spacer_ [width 20]
    , centerWidgetH $ vstack
        [ box_ [alignMiddle] $ hstack
            [ label "Action:"
                `styleBasic` [textSize 12]
            , spacer
            , actionLabel
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
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Position Size:"
                `styleBasic` [textSize 12]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #offerQuantity) 
                  [placeholder quantityPlaceholder] 
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
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
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Limit Price:"
                `styleBasic` [textSize 12]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #askPerOfferPrice) 
                  [placeholder pricePlaceholder]
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
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
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Arbitrage Fee:"
                `styleBasic` [textSize 12]
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
    , hstack 
        [ filler
        , button "Cancel" $ swapBuilderEvent $ EditSelectedSwapCreation CancelAdding
        , spacer
        , mainButton "Confirm" $ swapBuilderEvent $ EditSelectedSwapCreation ConfirmAdding
        ]
    ]

editLiquiditySwapWidget :: AppModel -> AppNode
editLiquiditySwapWidget AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #swapBuilderModel % #targetSwapCreation)
      NewSwapCreation{..} = maybe def snd $ swapBuilderModel ^. #targetSwapCreation
      offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
      askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
      pricePlaceholder = offerAssetName <> " / " <> askAssetName
      quantityPlaceholder x = "# " <> x
      buyPriceLabel = "Buy " <> askAssetName <> " Price:"
      sellPriceLabel = "Sell " <> askAssetName <> " Price:"
      depositLabel = fromString . printf "%s Deposit:"
      askQuantityLens = maybeLens "" (maybeLens' % _2 % #askQuantity)
      offerPerAskPriceLens = maybeLens "" (maybeLens' % _2 % #offerPerAskPrice)
  vstack
    [ centerWidgetH $ label "Edit Liquidity Swap"
    , spacer_ [width 20]
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
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label (depositLabel offerAssetName)
                `styleBasic` [textSize 12]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #offerQuantity) 
                  [placeholder $ quantityPlaceholder offerAssetName] 
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
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
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label (depositLabel askAssetName)
                `styleBasic` [textSize 12]
            , spacer
            , textField_ (toLensVL askQuantityLens)
                  [placeholder $ quantityPlaceholder askAssetName] 
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
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
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label sellPriceLabel
                `styleBasic` [textSize 12]
            , spacer
            , textField_ (toLensVL offerPerAskPriceLens) 
                  [placeholder pricePlaceholder]
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
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
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label buyPriceLabel
                `styleBasic` [textSize 12]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #askPerOfferPrice) 
                  [placeholder pricePlaceholder]
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ swapBuilderEvent $ EditSelectedSwapCreation CancelAdding
        , spacer
        , mainButton "Confirm" $ swapBuilderEvent $ EditSelectedSwapCreation ConfirmAdding
        ]
    ]

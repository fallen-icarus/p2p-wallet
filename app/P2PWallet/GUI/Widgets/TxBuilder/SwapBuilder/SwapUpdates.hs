module P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapUpdates
  ( 
    swapUpdatesList
  , editSwapUpdateWidget
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

swapUpdatesList :: ReverseTickerMap -> [(Int,SwapUpdate)] -> [AppNode]
swapUpdatesList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,SwapUpdate) -> AppNode
    utxoRow s@(_,SwapUpdate{newSwap=SwapCreation{swapType}}) = case swapType of
      LimitOrder -> limitOrderRow s
      LiquiditySwap -> liquiditySwapRow s

    limitOrderRow :: (Int,SwapUpdate) -> AppNode
    limitOrderRow s@(idx,SwapUpdate{oldSwap,newSwap=SwapCreation{..}}) = do
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
                [ label "Update Limit Order"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display (oldSwap ^. #utxoRef) in
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
                , flip styleBasic [textSize 10] $ tooltip_ (oldSwap ^. #walletAlias) [tooltipDelay 0] $
                    label userIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        ]
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
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (swapBuilderEvent $ EditSelectedSwapUpdate $ StartAdding $ Just s)
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
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (swapBuilderEvent $ RemoveSelectedSwapUpdate idx)
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

    liquiditySwapRow :: (Int,SwapUpdate) -> AppNode
    liquiditySwapRow s@(idx,SwapUpdate{oldSwap,newSwap=SwapCreation{..}}) = do
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
                [ label "Update Liquidity Swap"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display (oldSwap ^. #utxoRef) in
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
                , flip styleBasic [textSize 10] $ tooltip_ (oldSwap ^. #walletAlias) [tooltipDelay 0] $
                    label userIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        ]
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
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (swapBuilderEvent $ EditSelectedSwapUpdate $ StartAdding $ Just s)
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
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (swapBuilderEvent $ RemoveSelectedSwapUpdate idx)
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

editSwapUpdateWidget :: AppModel -> AppNode
editSwapUpdateWidget model@AppModel{txBuilderModel=TxBuilderModel{..}} = do
  let NewSwapCreation{swapType} = maybe def snd $ swapBuilderModel ^. #targetSwapUpdate
  centerWidget $ vstack
    [ -- This is deliberately written as a `case` to have the compiler prompt this location when
      -- new swap types are added.
      case swapType of
        LimitOrder -> editLimitOrderWidget model
        LiquiditySwap -> editLiquiditySwapWidget model
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

editLimitOrderWidget :: AppModel -> AppNode
editLimitOrderWidget AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #swapBuilderModel % #targetSwapUpdate)
      NewSwapCreation{..} = maybe def snd $ swapBuilderModel ^. #targetSwapUpdate
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
        , button "Cancel" $ swapBuilderEvent $ EditSelectedSwapUpdate CancelAdding
        , spacer
        , mainButton "Confirm" $ swapBuilderEvent $ EditSelectedSwapUpdate ConfirmAdding
        ]
    ]

editLiquiditySwapWidget :: AppModel -> AppNode
editLiquiditySwapWidget AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #swapBuilderModel % #targetSwapUpdate)
      NewSwapCreation{..} = maybe def snd $ swapBuilderModel ^. #targetSwapUpdate
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
        , button "Cancel" $ swapBuilderEvent $ EditSelectedSwapUpdate CancelAdding
        , spacer
        , mainButton "Confirm" $ swapBuilderEvent $ EditSelectedSwapUpdate ConfirmAdding
        ]
    ]

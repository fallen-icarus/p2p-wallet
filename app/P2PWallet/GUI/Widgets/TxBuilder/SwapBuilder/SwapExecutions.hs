module P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapExecutions
  ( 
    swapExecutionsList
  , editSwapExecutionWidget
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

swapExecutionsList :: ReverseTickerMap -> [(Int,SwapExecution)] -> [AppNode]
swapExecutionsList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,SwapExecution) -> AppNode
    utxoRow s@(idx,SwapExecution{..}) = do
      let offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
          askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
          purchaseAmount = showAssetBalance True reverseTickerMap $ 
            unOfferAsset offerAsset & #quantity %~ (offerAvailable -)
          purchaseCost = showAssetBalance True reverseTickerMap $ 
            unAskAsset askAsset & #quantity %~ subtract startingAskQuantity
          prettyPrice =
            showPriceFormatted
              reverseTickerMap 
              (unAskAsset askAsset) 
              (unOfferAsset offerAsset) 
              askPerOfferPrice
          priceCaption =
            fromString $ printf "Price: %s %s / %s" prettyPrice askAssetName offerAssetName
      hstack
        [ vstack
            [ hstack
                [ label "Execute Swap"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , let prettyRef = display utxoRef in
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
                , filler
                , label ("Buying: " <> purchaseAmount)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label priceCaption
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label ("Cost: " <> purchaseCost)
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
                    (swapBuilderEvent $ EditSelectedSwapExecution $ StartAdding $ Just s)
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
            button closeCircleIcon (swapBuilderEvent $ RemoveSelectedSwapExecution idx)
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

editSwapExecutionWidget :: AppModel -> AppNode
editSwapExecutionWidget AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap}= do
  let (_,NewSwapExecution{..}) = fromMaybe (0,def) $ swapBuilderModel ^. #targetSwapExecution
      maybeLens' = maybeLens def $ #txBuilderModel % #swapBuilderModel % #targetSwapExecution
      offerAssetName = showAssetNameOnly reverseTickerMap $ unOfferAsset offerAsset
      askAssetName = showAssetNameOnly reverseTickerMap $ unAskAsset askAsset
      mQuantity = rightToMaybe $ view #quantity <$>
        parseFormattedAssetQuantity reverseTickerMap (unOfferAsset offerAsset) offerQuantity
      availableCaption = showAssetBalance True reverseTickerMap $
        unOfferAsset offerAsset & #quantity .~ offerAvailable
      askAssetCost = unAskAsset askAsset & #quantity .~ 
        roundUp (fromIntegral (fromMaybe 0 mQuantity) * askPerOfferPrice)
      priceCaption = unwords
        [ showPriceFormatted 
            reverseTickerMap 
            (unAskAsset askAsset) 
            (unOfferAsset offerAsset) 
            askPerOfferPrice
        , " "
        , askAssetName <> " / " <> offerAssetName
        ]
  centerWidget $ vstack
    [ spacer
    , box_ [alignMiddle] $
        label "Edit Swap Execution"
          `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
    , spacer
    , hstack
        [ box_ [alignMiddle, onClick $ Alert $ purchaseAmountMsg offerAssetName] $
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
        , label "Purchase Amount:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #offerQuantity) 
              [placeholder $ "# " <> offerAssetName] 
            `styleBasic` [textSize 10, width 100, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer_ [width 10]
    , separatorLine
    , spacer_ [width 10]
    , hstack
        [ label "Available: "
            `styleBasic` [textSize 10]
        , spacer
        , label availableCaption
            `styleBasic` [textSize 10]
        ]
    , spacer
    , hstack
        [ label "Price:"
            `styleBasic` [textSize 10]
        , spacer
        , label priceCaption
            `styleBasic` [textSize 10]
        ]
    , spacer
    , hstack
        [ label "Approximate Cost:"
            `styleBasic` [textSize 10]
        , spacer
        , label (showAssetBalance True reverseTickerMap askAssetCost)
            `styleBasic` [textSize 10]
        ]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (swapBuilderEvent $ EditSelectedSwapExecution CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Confirm" (swapBuilderEvent $ EditSelectedSwapExecution ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic`
        [ bgColor customGray3
        , padding 20
        , radius 20
        ]

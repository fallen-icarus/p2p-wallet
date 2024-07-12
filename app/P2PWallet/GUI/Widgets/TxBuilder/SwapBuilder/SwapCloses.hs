module P2PWallet.GUI.Widgets.TxBuilder.SwapBuilder.SwapCloses
  ( 
    swapClosesList
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.Prelude

swapClosesList :: ReverseTickerMap -> [(Int,SwapClose)] -> [AppNode]
swapClosesList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int,SwapClose) -> AppNode
    utxoRow s@(_,SwapClose{swapDatum}) = case swapDatum of
      Just (OneWay datum) -> limitOrderRow s datum
      Just (TwoWay datum) -> liquiditySwapRow s datum

      -- It shouldn't be possible to create a `SwapClose` for a UTxO with an invalid datum.
      Just (SwapDatumError _) -> spacer `nodeVisible` False
      Nothing -> spacer `nodeVisible` False
      
    limitOrderRow :: (Int,SwapClose) -> OneWay.SwapDatum -> AppNode
    limitOrderRow (idx,s@SwapClose{utxoRef,walletAlias}) OneWay.SwapDatum{..} = do
      let offerAsset = updateQuantity s $ NativeAsset
            { policyId = offerId
            , tokenName = offerName
            , fingerprint = mkAssetFingerprint offerId offerName
            , quantity = 0
            }
          askAsset = updateQuantity s $ NativeAsset
            { policyId = askId
            , tokenName = askName
            , fingerprint = mkAssetFingerprint askId askName
            , quantity = 0
            }
          positionSize = showAssetBalance True reverseTickerMap offerAsset
      hstack
        [ vstack
            [ hstack
                [ label ("Close Limit Order For " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label positionSize
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label (display utxoRef)
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
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (swapBuilderEvent $ RemoveSelectedSwapClose idx)
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

    liquiditySwapRow :: (Int,SwapClose) -> TwoWay.SwapDatum -> AppNode
    liquiditySwapRow (idx,s@SwapClose{utxoRef,walletAlias}) TwoWay.SwapDatum{..} = do
      let asset1 = updateQuantity s $ NativeAsset
            { policyId = asset1Id
            , tokenName = asset1Name
            , fingerprint = mkAssetFingerprint asset1Id asset1Name
            , quantity = 0
            }
          asset2 = updateQuantity s $ NativeAsset
            { policyId = asset2Id
            , tokenName = asset2Name
            , fingerprint = mkAssetFingerprint asset2Id asset2Name
            , quantity = 0
            }
      hstack
        [ vstack
            [ hstack
                [ label ("Close Liquidity Swap For " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label (showAssetBalance True reverseTickerMap asset1)
                    `styleBasic` [textSize 10, textColor white]
                , spacer_ [width 3]
                , label twoWayIcon
                    `styleBasic` [textSize 12, textMiddle, textFont "Remix", textColor white]
                , spacer_ [width 3]
                , label (showAssetBalance True reverseTickerMap asset2)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , label (display utxoRef)
                `styleBasic` [textSize 8, textColor lightGray]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (swapBuilderEvent $ RemoveSelectedSwapClose idx)
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

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the native asset quantity to reflect the actual values present in the UTxO.
-- This also accounts for ada being located separate to the native assets in the `SwapClose` UTxO.
updateQuantity :: SwapClose -> NativeAsset -> NativeAsset
updateQuantity SwapClose{nativeAssets,lovelace} asset@NativeAsset{policyId,fingerprint}
  | policyId == "" = asset & #quantity .~ unLovelace lovelace
  | otherwise = flip (set #quantity) asset $ maybe 0 (view #quantity) $ flip find nativeAssets $
      \a -> a ^. #fingerprint == fingerprint

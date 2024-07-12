module P2PWallet.GUI.Widgets.Dex.Positions 
  ( 
    positionsWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
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
      label "No currrent positions."
        `styleBasic` [textFont "Italics"]

mainWidget :: AppModel -> AppNode
mainWidget AppModel{dexModel=DexModel{..},reverseTickerMap} = do
    cushionWidget $ vstack
      [ -- A header widget saying how many positions match that search out of the total as well
        -- as a filter button for opening the filter menu. 
        hstack 
          [ label ("Positions (" <> fractionShown <> ")")
              `styleBasic` [textFont "Italics", textSize 14]
          , spacer_ [width 5]
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
          , filler
          ]
        -- These are the UTxOs that match those filter/search settings.
      , box $ vscroll_ [wheelRate 50] $ 
          vstack_ [childSpacing_ 5] (map utxoRow sample)
            `styleBasic` [padding 10]
      ]
  where
    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    sample :: [SwapUTxO]
    sample = selectedWallet ^. #utxos

    fractionShown :: Text
    fractionShown = 
      show (length sample) <> "/" <> show (length $ selectedWallet ^. #utxos)

    utxoRow :: SwapUTxO -> AppNode
    utxoRow u@SwapUTxO{swapDatum} = case swapDatum of
      Just (OneWay datum) -> limitOrderRow u datum
      Just (TwoWay datum) -> liquiditySwapRow u datum

    limitOrderRow :: SwapUTxO -> OneWay.SwapDatum -> AppNode
    limitOrderRow u@SwapUTxO{..} OneWay.SwapDatum{..} = do
      let offerAsset = updateQuantity u $ NativeAsset
            { policyId = offerId
            , tokenName = offerName
            , fingerprint = mkAssetFingerprint offerId offerName
            , quantity = 0
            }
          askAsset = updateQuantity u $ NativeAsset
            { policyId = askId
            , tokenName = askName
            , fingerprint = mkAssetFingerprint askId askName
            , quantity = 0
            }
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
                  tooltip_ prettyRef [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText $ display utxoRef] $
                      label idCardIcon
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
                [ label buyPriceCaption
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 10]
                , separatorLine
                , spacer_ [width 10]
                , label sellPriceCaption
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
                button editIcon AppInit
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
      let asset1 = updateQuantity u $ NativeAsset
            { policyId = asset1Id
            , tokenName = asset1Name
            , fingerprint = mkAssetFingerprint asset1Id asset1Name
            , quantity = 0
            }
          asset2 = updateQuantity u $ NativeAsset
            { policyId = asset2Id
            , tokenName = asset2Name
            , fingerprint = mkAssetFingerprint asset2Id asset2Name
            , quantity = 0
            }
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
                  tooltip_ prettyRef [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText $ display utxoRef] $
                      label idCardIcon
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
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon AppInit
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

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Double -> Color -> Color -> Text -> WidgetNode s AppEvent
copyableLabelSelf fontSize mainColor hoverColor caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor hoverColor, cursorIcon CursorHand]

-- | A label button that will copy other data.
copyableLabelFor :: Double -> Text -> Text -> WidgetNode s AppEvent
copyableLabelFor fontSize caption info = 
  hstack
    [ tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText info)
        `styleBasic`
          [ padding 0
          , radius 5
          , textMiddle
          , border 0 transparent
          , textColor customBlue
          , bgColor transparent
          , textSize fontSize
          ]
        `styleHover` [textColor lightGray, cursorIcon CursorHand]
    , spacer
    , label_ info [ellipsis] `styleBasic` [textColor lightGray, textSize fontSize]
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

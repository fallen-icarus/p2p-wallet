module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ExpiredCloses
  ( 
    expiredOptionsClosesList
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Plutus
import P2PWallet.Prelude

expiredOptionsClosesList :: ReverseTickerMap -> TimeZone -> [(Int,ExpiredOptionsClose)] -> [AppNode]
expiredOptionsClosesList reverseTickerMap timeZone = map utxoRow
  where
    utxoRow :: (Int, ExpiredOptionsClose) -> AppNode
    utxoRow (idx,ExpiredOptionsClose{..}) = do
      let Options.ActiveDatum{..} = fromMaybe def activeDatum
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
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
      hstack
        [ vstack
            [ hstack
                [ label ("Close Expired Options For " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label (showAssetBalance True reverseTickerMap offerAmount)
                    `styleBasic` [textSize 10, textColor white]
                , spacer_ [width 2]
                , label remixArrowRightFill
                    `styleBasic` [textMiddle, textFont "Remix", textSize 10, textColor white]
                , spacer_ [width 2]
                , label (showAssetNameOnly reverseTickerMap askNativeAsset)
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
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (optionsBuilderEvent $ RemoveSelectedExpiredOptionsClose idx)
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


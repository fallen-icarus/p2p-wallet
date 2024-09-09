module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalCloses
  ( 
    proposalClosesList
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

proposalClosesList :: ReverseTickerMap -> TimeZone -> [(Int,ProposalClose)] -> [AppNode]
proposalClosesList reverseTickerMap timeZone = map utxoRow
  where
    utxoRow :: (Int, ProposalClose) -> AppNode
    utxoRow (idx,ProposalClose{..}) = do
      let Options.ProposalDatum{..} = fromMaybe def proposalDatum
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          premiumNativeAsset = toNativeAsset premiumAsset
      hstack
        [ vstack
            [ hstack
                [ label ("Close Options Proposal For " <> walletAlias)
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
            , label "Possible Terms:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , hstack
                [ spacer
                , vstack (map (termsRow offerAmount askNativeAsset premiumNativeAsset) possibleTerms)
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (optionsBuilderEvent $ RemoveSelectedProposalClose idx)
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

    termsRow 
      :: NativeAsset 
      -> NativeAsset 
      -> NativeAsset 
      -> Options.Terms
      -> AppNode
    termsRow offerAsset askAsset premiumAsset Options.Terms{premium, strikePrice, expiration} = do
      let formattedPrice = 
            showPriceFormatted reverseTickerMap askAsset offerAsset $ toRational strikePrice
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAsset
            ]
          prettyPremium = 
            showAssetBalance True reverseTickerMap $ premiumAsset & #quantity .~ premium
      hstack
        [ spacer_ [width 2]
        , label ("Premium: " <> prettyPremium)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Price: " <> prettyPrice)
            `styleBasic` [textSize 8, textColor lightGray]
        , filler
        , label ("Expiration: " <> showLocalDate timeZone (fromPlutusTime $ expiration - 1))
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]


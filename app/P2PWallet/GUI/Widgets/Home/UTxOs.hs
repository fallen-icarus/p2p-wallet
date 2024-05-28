{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.UTxOs where

import Monomer

import Prettyprinter (align, pretty, vsep)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Wallets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

utxosWidget :: AppModel -> AppNode
utxosWidget model =
    cushionWidgetH $ vstack
      [ hstack 
          [ label ("UTxOs (" <> show (length sample) <> ")")
              `styleBasic` [textFont "Italics", textSize 14]
          , filler
          ]
      , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
          vstack_ [childSpacing] (map utxoRow sample)
            `styleBasic` [padding 10]
      , spacer
      ] 
  where
    sample :: [PersonalUTxO]
    sample = -- applyAddressUTxOFilters (model ^. homeModel . setFilters . utxoFilters) $
      sortOn (view #utxoRef) $ model ^. #homeModel % #selectedWallet % #utxos

    datumIcon :: Text
    datumIcon = toGlyph 0XF2F5

    scriptIcon :: Text
    scriptIcon = toGlyph 0XF433

    moreIcon :: Bool -> Text
    moreIcon detailsOpen
      | detailsOpen = remixCloseCircleLine
      | otherwise = remixMoreLine

    moreTip :: Bool -> Text
    moreTip detailsOpen
      | detailsOpen = "Close Details"
      | otherwise = "Show Details"

    moreOffStyle :: Style
    moreOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1]

    utxoRow :: PersonalUTxO -> AppNode
    utxoRow u@PersonalUTxO{..} =
      vstack
        [ vstack
            [ hstack 
                [ copyableLabelSelf (showTxOutRef utxoRef)
                    `styleBasic` [textSize 12]
                , filler
                , label (fromString $ printf "%D ADA" $ toAda lovelace) 
                    `styleBasic` [textSize 12]
                ]
            , hstack
                [ label remixCalendarLine
                    `styleBasic` 
                      [ textSize 10
                      , textColor customBlue
                      , textFont "Remix"
                      , paddingT 5
                      ]
                , spacer_ [width 3]
                , label (showLocalDate (model ^. #config % #timeZone) blockTime)
                    `styleBasic` 
                      [ textSize 10
                      , textColor lightGray
                      ]
                , spacer
                , label remixTimeLine
                    `styleBasic` 
                      [ textSize 10
                      , textColor customBlue
                      , textFont "Remix"
                      , paddingT 5
                      ]
                , spacer_ [width 3]
                , label (showLocalTime (model ^. #config % #timeZone) blockTime)
                    `styleBasic` 
                      [ textSize 10
                      , textColor lightGray
                      ]
                , spacer
                , widgetIf (not $ null nativeAssets) $ 
                    tooltip_ "Native Assets" [tooltipDelay 500] $ label remixCoinsLine
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , widgetIf (not $ null nativeAssets) $ spacer_ [width 3]
                , widgetIf (isJust datumHash) $ 
                    tooltip_ "Datum" [tooltipDelay 500] $ label datumIcon
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , widgetIf (isJust datumHash) $ spacer_ [width 3]
                , widgetIf (isJust referenceScriptHash) $ 
                    tooltip_ "Reference Script" [tooltipDelay 500] $ label scriptIcon
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , filler
                , tooltip_ (moreTip showDetails) [tooltipDelay 1000] $
                    toggleButton_ (moreIcon showDetails)
                      (toLensVL $ #homeModel % #selectedWallet % #utxos % toggleDetails utxoRef)
                      [toggleButtonOffStyle moreOffStyle]
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        , padding 0
                        , bgColor transparent
                        , border 0 transparent
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
               ] 
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , utxoDetails u `nodeVisible` showDetails
        ]

    utxoDetails :: PersonalUTxO -> AppNode
    utxoDetails PersonalUTxO{..} = 
      hstack
        [ filler
        , vstack
            [ copyableLabelFor_ "Block Height:" (show @Text blockHeight) 10
                  `styleBasic` [padding 2]
            , widgetMaybe referenceScriptHash $ \hash ->
                copyableLabelFor_ "Reference Script Hash:" hash 10
                  `styleBasic` [padding 2]
            , widgetMaybe datumHash $ \hash ->
                copyableLabelFor_ "Datum Hash:" hash 10
                  `styleBasic` [padding 2]
            , widgetMaybe inlineDatum $ \x ->
                copyableLabelFor_ "Inline Datum:" (showValue x) 10
                  `styleBasic` [padding 2]
            , widgetIf (not $ null nativeAssets) $
                vstack
                  [ label "Native Assets:" `styleBasic` [textSize 10, textColor customBlue]
                  , hstack
                      [ spacer_ [width 10]
                      , copyableTextArea (show $ align $ vsep $ map pretty nativeAssets)
                          `styleBasic` [textSize 10, textColor lightGray, maxWidth 700]
                      ]
                  ] `styleBasic` [padding 2]
            ] `styleBasic`
                [ bgColor black
                , padding 10
                , border 1 black
                ]
        ]

module P2PWallet.GUI.Widgets.TxBuilder.UserInputs
  ( 
    userInputsList
  ) where

import Monomer
import Prettyprinter (pretty, align, vsep, tupled)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.TxBuilder.Internal
import P2PWallet.Prelude

userInputsList :: AppModel -> AppNode
userInputsList AppModel{txBuilderModel=TxBuilderModel{userInputs},reverseTickerMap} = do
  vstack
    [ label ("Personal UTxOs " <> show (tupled [pretty $ length userInputs]))
        `styleBasic` [textSize 12]
    , vstack_ [childSpacing] (map utxoRow userInputs)
        `styleBasic` [padding 10]
    ] `styleBasic` [padding 5]
  where
    moreTip :: Bool -> Text
    moreTip detailsOpen
      | detailsOpen = "Close Details"
      | otherwise = "Show Details"

    specificUtxoMoreOffStyle :: Style
    specificUtxoMoreOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1]

    utxoRow :: (Int,UserInput) -> AppNode
    utxoRow (idx,u@UserInput{..}) =
      vstack
        [ vstack
            [ hstack 
                [ copyableLabelSelf (display utxoRef) white 10
                , filler
                , label (display lovelace) 
                    `styleBasic` [textSize 10, textColor white]
                ]
            , hstack
                [ label ("From: " <> walletAlias)
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , widgetIf (not $ null nativeAssets) $ 
                    tooltip_ "Native Assets" [tooltipDelay 0] $ label coinsIcon
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , filler
                , tooltip_ "Remove UTxO" [tooltipDelay 0] $
                    button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserInput idx)
                      `styleBasic` 
                        [ textSize 10
                        , textColor customRed
                        , textFont "Remix"
                        , textMiddle
                        , padding 0
                        , bgColor transparent
                        , border 0 transparent
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                , spacer_ [width 5]
                , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                    toggleButton_ horizontalMoreIcon
                      (toLensVL $ #txBuilderModel 
                                % #userInputs 
                                % toggleDetails idx)
                      [toggleButtonOffStyle specificUtxoMoreOffStyle]
                      `styleBasic` 
                        [ textSize 10
                        , textColor customRed
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
        , widgetIf showDetails $ utxoDetails u
        ]

    utxoDetails :: UserInput -> AppNode
    utxoDetails UserInput{..} = do
      let prettyAssets = map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
      hstack
        [ filler
        , vstack
            [ copyableLabelFor 8 lightGray "Payment Address:" (toText paymentAddress)
                `styleBasic` [padding 2]
            , widgetIf (not $ null nativeAssets) $
                vstack
                  [ label "Native Assets:" `styleBasic` [textSize 8, textColor customBlue]
                  , hstack
                      [ spacer_ [width 10]
                      , vstack
                          [ spacer_ [width 10]
                          , copyableTextArea (show $ align $ vsep prettyAssets)
                              `styleBasic` 
                                [ height $ 20 + 12 * (fromIntegral (length nativeAssets) - 1)
                                , textSize 8
                                , textColor lightGray
                                , maxWidth 300
                                ]
                          ]
                      ]
                  ] `styleBasic` [padding 2]
            ] `styleBasic`
                [ bgColor black
                , padding 10
                , border 1 black
                ]
        ]

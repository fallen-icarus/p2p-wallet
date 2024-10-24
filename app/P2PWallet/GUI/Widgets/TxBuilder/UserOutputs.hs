module P2PWallet.GUI.Widgets.TxBuilder.UserOutputs
  ( 
    userOutputsList
  , editUserOutputWidget
  ) where

import Monomer
import Prettyprinter (pretty, align, vsep)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.TxBuilder.Internal
import P2PWallet.Prelude

userOutputsList :: ReverseTickerMap -> [(Int,UserOutput)] -> [AppNode]
userOutputsList reverseTickerMap = map utxoRow
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

    utxoRow :: (Int,UserOutput) -> AppNode
    utxoRow o@(idx,u@UserOutput{..}) =
      hstack
        [ vstack
            [ vstack
                [ hstack 
                    [ label ("Pay " <> if alias == "" then "external address" else alias)
                        `styleBasic` [textSize 10, textColor customBlue]
                    , spacer_ [width 5]
                    , widgetIf (not $ null nativeAssets) $ 
                        tooltip_ "Native Assets" [tooltipDelay 0] $ label coinsIcon
                          `styleBasic` 
                            [ textSize 10
                            , textColor customBlue
                            , textFont "Remix"
                            , textMiddle
                            ]
                    , filler
                    , label (display lovelace) 
                        `styleBasic` [textSize 10, textColor white]
                    ]
                , spacer_ [width 2]
                , hstack
                    [ copyableLabelSelf (toText paymentAddress) lightGray 8
                    , filler
                    , widgetIf (nativeAssets /= []) $ hstack
                        [ spacer_ [width 5]
                        , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                            toggleButton_ horizontalMoreIcon
                              (toLensVL $ #txBuilderModel 
                                        % #userOutputs 
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
                   ] 
                ] `styleBasic` 
                    [ padding 10
                    , bgColor customGray2
                    , radius 5
                    , border 1 black
                    ]
            , widgetIf showDetails $ utxoDetails u
            ]
        , spacer_ [width 3]
        , box_ [alignTop] $ hstack
            [ vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                    button editIcon 
                        (TxBuilderEvent $ EditSelectedUserOutput $ StartAdding $ Just o)
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
                    button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserOutput idx)
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
        [ box_ [onClick $ TxBuilderEvent $ ChangeUserOutputCount idx upperCount] $
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
            box_ [onClick $ TxBuilderEvent $ ChangeUserOutputCount idx lowerCount] $
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

    utxoDetails :: UserOutput -> AppNode
    utxoDetails UserOutput{nativeAssets} = do
      let prettyAssets = map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
      hstack
        [ filler
        , vstack
            [ widgetIf (not $ null nativeAssets) $
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

editUserOutputWidget :: Text -> AppNode
editUserOutputWidget recipient = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #targetUserOutput)
  centerWidget $ vstack
    [ centerWidgetH $ label ("How much would you like to send to " <> recipient <> "?")
    , spacer_ [width 20]
    , hstack
        [ label "ADA:"
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #ada)
              [placeholder "1.234567"]
            `styleBasic` [width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack
        [ label "Native Assets (separated with newlines):"
        , mainButton helpIcon (Alert nativeAssetAreaEntryMsg)
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
    , textArea (toLensVL $ maybeLens' % _2 % #nativeAssets)
        `styleBasic` [height 180, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ EditSelectedUserOutput CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ EditSelectedUserOutput ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]


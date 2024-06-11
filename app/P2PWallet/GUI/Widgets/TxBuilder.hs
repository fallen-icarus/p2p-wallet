{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.TxBuilder
  ( 
    txBuilderWidget
  ) where

import Monomer hiding 
  ( popupAnchor
  , alignTop
  , popupAlignToOuterV
  )
import Prettyprinter (pretty, align, vsep, tupled)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.GUI.Widgets.Internal.Popup
import P2PWallet.Plutus
import P2PWallet.Prelude

txBuilderWidget :: AppModel -> AppNode
txBuilderWidget model@AppModel{..} = do
    zstack
      [ vstack
          [ zstack
              [ widgetIf (not isEmpty) mainWidget
              , widgetIf isEmpty $ vstack
                  [ centerWidget $
                      flip styleBasic [bgColor transparent, padding 20, radius 5] $ box $ 
                        label "Add something to the builder to begin!"
                         `styleBasic` [textFont "Italics"]
                  , filler
                  ]
              ]
          , filler
          , hstack
              [ filler
              , addPopup
              ]
          ] `nodeVisible` and
              [ isNothing targetUserOutput
              ]
      , editUserOutputWidget (maybe "" (view (_2 % #alias)) targetUserOutput)
          `nodeVisible` isJust targetUserOutput
      ] `styleBasic`
          [ padding 20
          ]
  where
    isEmpty :: Bool
    isEmpty = isEmptyBuilder txBuilderModel

    targetUserOutput :: Maybe (Int,NewUserOutput)
    targetUserOutput = txBuilderModel ^. #targetUserOutput

    mainWidget :: AppNode
    mainWidget = do
      vstack
        [ centerWidgetH $ label "Tx Builder"
            `styleBasic` [paddingT 10, paddingB 10, textFont "Italics", textColor white, textSize 18]
        , actionsList model
        , widgetIf (txBuilderModel ^. #userInputs /= []) $ userInputsList model
        ]

    addPopup :: AppNode
    addPopup = do
      let anchor = 
            button addIcon (TxBuilderEvent ShowTxAddPopup)
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , paddingT 2
                , paddingB 2
                , bgColor black
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
      vstack
        [ customPopup_ (toLensVL $ #txBuilderModel % #showAddPopup) 
            [popupAnchor anchor, alignTop, popupAlignToOuterV] $
            vstack
              [ button "Input" AppInit
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              , separatorLine `styleBasic` [fgColor black]
              , button "Output" AppInit
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ] `styleBasic`
                  [ bgColor customGray3
                  , border 1 black
                  , padding 5
                  ]
        ] `styleBasic` [padding 0]

userInputsList :: AppModel -> AppNode
userInputsList model@AppModel{txBuilderModel=TxBuilderModel{userInputs}} = do
  vstack
    [ label ("Personal UTxOs " <> show (tupled [pretty $ length userInputs]))
    , flip styleBasic [padding 5] $
        vstack_ [childSpacing] (map utxoRow userInputs)
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
                [ copyableLabelSelf (showTxOutRef utxoRef) white 12
                , filler
                , label (fromString $ printf "%D ADA" $ toAda lovelace) 
                    `styleBasic` [textSize 12, textColor white]
                ]
            , hstack
                [ label ("From: " <> walletAlias)
                    `styleBasic` [textSize 10, textColor lightGray]
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
                                % toggleUserInputDetails idx)
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
    utxoDetails UserInput{..} = 
      hstack
        [ filler
        , vstack
            [ copyableLabelFor 10 "Payment Address:" (toText paymentAddress)
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

actionsList :: AppModel -> AppNode
actionsList model@AppModel{txBuilderModel=TxBuilderModel{..}} = do
  let numActions = length userOutputs
  vstack
    [ label ("Actions " <> show (tupled [pretty numActions]))
    , userOutputsList userOutputs
    ] `styleBasic` [padding 5]

userOutputsList :: [(Int,UserOutput)] -> AppNode
userOutputsList userOutputs = do
    flip styleBasic [padding 5] $
      vstack_ [childSpacing] (map utxoRow userOutputs)
        `styleBasic` [padding 10]
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
                        `styleBasic` [textSize 12, textColor white]
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
                    , label (fromString $ printf "%D ADA" $ toAda lovelace) 
                        `styleBasic` [textSize 12, textColor white]
                    ]
                , hstack
                    [ copyableLabelSelf (toText paymentAddress) lightGray 10
                    , filler
                    , tooltip_ "Remove Output" [tooltipDelay 0] $
                        button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserOutput idx)
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
                    , spacer_ [width 3]
                    , tooltip_ "Edit Output" [tooltipDelay 0] $
                        button editIcon 
                            (TxBuilderEvent $ EditSelectedUserOutput $ StartAdding $ Just o)
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
                    , widgetIf (nativeAssets /= []) $ hstack
                        [ spacer_ [width 5]
                        , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                            toggleButton_ horizontalMoreIcon
                              (toLensVL $ #txBuilderModel 
                                        % #userOutputs 
                                        % toggleUserOutputDetails idx)
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
        , spacer
        , countWidget idx count
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
    utxoDetails UserOutput{nativeAssets} = 
      hstack
        [ filler
        , vstack
            [ widgetIf (not $ null nativeAssets) $
                vstack
                  [ label "Native Assets:" `styleBasic` [textSize 10, textColor customBlue]
                  , hstack
                      [ spacer_ [width 10]
                      , copyableTextArea 
                            (show $ align $ vsep $ 
                               map (pretty . view fullNameAndQuantity) nativeAssets)
                          `styleBasic` [textSize 10, textColor lightGray, maxWidth 700]
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
    [ centerWidgetH $ label ("How much would you like to pay " <> recipient <> "?")
    , spacer_ [width 20]
    , hstack
        [ label "ADA:"
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #ada)
              [placeholder "1.234567"]
            `styleBasic` [width 200]
        ]
    , spacer
    , hstack
        [ label "Native Assets (separated with newlines)"
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
        `styleBasic` [height 180, textSize 10]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ EditSelectedUserOutput CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ EditSelectedUserOutput ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20, width 700]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Text -> Color -> Double -> WidgetNode s AppEvent
copyableLabelSelf caption color fontSize = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize fontSize
      , border 0 transparent
      , textColor color
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

-- | A label button that will copy other data. The font size is configurable.
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
    , label_ info [ellipsis] `styleBasic` [textColor lightGray, textSize 10]
    ]

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
toggleUserInputDetails :: Int -> Lens' [(Int,UserInput)] Bool
toggleUserInputDetails idx = lens getToggleDetails setToggleDetails
  where
    getToggleDetails :: [(Int,UserInput)] -> Bool
    getToggleDetails [] = False
    getToggleDetails ((i,u):us) =
      if i == idx
      then u ^. #showDetails
      else getToggleDetails us

    setToggleDetails :: [(Int,UserInput)] -> Bool -> [(Int,UserInput)]
    setToggleDetails [] _ = []
    setToggleDetails ((i,u):us) b =
      if i == idx
      then (i,(u & #showDetails .~ b)) : us
      else (i,u) : setToggleDetails us b

toggleUserOutputDetails :: Int -> Lens' [(Int,UserOutput)] Bool
toggleUserOutputDetails idx = lens getToggleDetails setToggleDetails
  where
    getToggleDetails :: [(Int,UserOutput)] -> Bool
    getToggleDetails [] = False
    getToggleDetails ((i,u):us) =
      if i == idx
      then u ^. #showDetails
      else getToggleDetails us

    setToggleDetails :: [(Int,UserOutput)] -> Bool -> [(Int,UserOutput)]
    setToggleDetails [] _ = []
    setToggleDetails ((i,u):us) b =
      if i == idx
      then (i,(u & #showDetails .~ b)) : us
      else (i,u) : setToggleDetails us b

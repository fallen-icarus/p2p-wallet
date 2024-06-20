{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.TxBuilder
  ( 
    txBuilderWidget
  ) where

import Monomer hiding 
  ( popupAnchor
  , alignTop
  , popupAlignToOuterV
  , popupAlignToOuterH
  )
import Prettyprinter (pretty, align, vsep, tupled)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.TickerMap
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
              , box_ [alignMiddle] $ hstack
                  [ spacer_ [width 50]
                  , mainButton "Build" (TxBuilderEvent BuildTx)
                      `nodeVisible` canBeBuilt
                  , button "Build" AppInit
                      `nodeEnabled` canBeBuilt
                      `nodeVisible` not canBeBuilt
                  , spacer_ [width 3]
                  , mainButton "Sign & Submit" (TxBuilderEvent SignAndSubmitTx)
                      `nodeVisible` txBuilderModel ^. #isBuilt
                  , button "Sign & Submit" AppInit
                      `nodeEnabled` txBuilderModel ^. #isBuilt
                      `nodeVisible` (not $ txBuilderModel ^. #isBuilt)
                  ] `nodeVisible` not isEmpty
              , filler
              , box_ [alignBottom,alignRight] addPopup
              ]
          ] `nodeVisible` and
              [ isNothing targetUserOutput
              , not isAddingChangeOutput
              ]
      , editUserOutputWidget (maybe "" (view (_2 % #alias)) targetUserOutput)
          `nodeVisible` isJust targetUserOutput
      , addChangeOutputWidget model
          `nodeVisible` isAddingChangeOutput
      ] `styleBasic`
          [ padding 20
          ]
  where
    isEmpty :: Bool
    isEmpty = isEmptyBuilder txBuilderModel

    canBeBuilt :: Bool
    canBeBuilt = and
      [ isJust $ txBuilderModel ^. #changeOutput
      , Just "" /= txBuilderModel ^? #changeOutput % _Just % #paymentAddress
      , txBuilderModel ^. #isBalanced
      , if txBuilderModel ^. #requiresCollateral then 
          isJust $ txBuilderModel ^. #collateralInput
        else
          True
      ]

    isAddingChangeOutput :: Bool
    isAddingChangeOutput = txBuilderModel ^. #addingChangeOutput

    targetUserOutput :: Maybe (Int,NewUserOutput)
    targetUserOutput = txBuilderModel ^. #targetUserOutput

    mainWidget :: AppNode
    mainWidget = do
      vstack
        [ centerWidgetH $ label "Tx Builder"
            `styleBasic` [paddingT 10, paddingB 10, textFont "Italics", textColor white, textSize 18]
        , box_ [alignMiddle] $ hstack
            [ changeInfoPopup model
            , spacer_ [width 2]
            , statusBar model
            ]
        , actionsList model
        , widgetIf (txBuilderModel ^. #userInputs /= []) $ userInputsList model
        ]

    addPopup :: AppNode
    addPopup = do
      let anchor = 
            button remixCommandLine (TxBuilderEvent ShowTxAddPopup)
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
            [popupAnchor anchor, alignTop, alignLeft, popupAlignToOuterV] $
            vstack
              [ button "Change Output" (TxBuilderEvent $ AddNewChangeOutput $ StartAdding Nothing)
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              , separatorLine `styleBasic` [fgColor black, padding 5]
              , button "Reset" (TxBuilderEvent ResetBuilder)
                  `styleBasic`
                    [ border 0 transparent
                    , textSize 12
                    , bgColor transparent
                    , textColor customRed
                    , textMiddle
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ] `styleBasic`
                  [ bgColor customGray3
                  , border 1 black
                  , padding 5
                  ]
        ] `styleBasic` [padding 0]

statusBar :: AppModel -> AppNode
statusBar AppModel{txBuilderModel=TxBuilderModel{..}} = do
  let checkboxIcon b
        | b == Nothing = indeterminateCheckboxIcon
        | b == Just True = checkedBoxIcon
        | otherwise = uncheckedBoxIcon
      checkboxColor b
        | b == Nothing = gray
        | b == Just True = customBlue
        | otherwise = customRed
      collateralState
        | requiresCollateral = Just $ isJust collateralInput
        | otherwise = Nothing
      changeAddressSet = and
        [ isJust changeOutput
        , Just "" /= changeOutput ^? _Just % #paymentAddress
        ]
  hstack_ [childSpacing]
    [ hstack
        [ label "Change Address"
            `styleBasic`
              [ textSize 12 ]
        , spacer_ [width 3]
        , label (checkboxIcon $ Just changeAddressSet)
            `styleBasic`
              [ textFont "Remix"
              , textColor $ checkboxColor $ Just changeAddressSet
              , textMiddle
              , textSize 12
              ]
        ]
    , separatorLine
    , hstack
        [ label "Balanced"
            `styleBasic`
              [ textSize 12 ]
        , spacer_ [width 3]
        , label (checkboxIcon $ Just isBalanced)
            `styleBasic`
              [ textFont "Remix"
              , textColor $ checkboxColor $ Just isBalanced
              , textMiddle
              , textSize 12
              ]
        ]
    , separatorLine
    , hstack
        [ label "Collateral"
            `styleBasic`
              [ textSize 12 ]
        , spacer_ [width 3]
        , label (checkboxIcon collateralState)
            `styleBasic`
              [ textFont "Remix"
              , textColor $ checkboxColor collateralState
              , textMiddle
              , textSize 12
              ]
        ]
    ] `styleBasic`
        [ bgColor customGray2
        , border 1 black
        , padding 10
        , radius 15
        ]

changeInfoPopup :: AppModel -> AppNode
changeInfoPopup AppModel{txBuilderModel,reverseTickerMap} = do
  let ChangeOutput{..} = fromMaybe def $ txBuilderModel ^. #changeOutput
      anchor = 
        box_ [alignMiddle] $ tooltip_ "Change Info" [tooltipDelay 0] $
          button remixExchangeDollarLine (TxBuilderEvent ShowTxChangePopup)
            `styleBasic`
              [ border 0 transparent
              , radius 20
              , padding 2
              , bgColor transparent
              , textColor customBlue
              , textMiddle
              , textFont "Remix"
              ]
            `styleHover` [bgColor customGray2, cursorIcon CursorHand]
  customPopup_ (toLensVL $ #txBuilderModel % #showChangePopup) 
    [popupAnchor anchor, alignBottom, popupAlignToOuterV] $
    vstack
      [ hstack 
          [ label "Change Address:"
              `styleBasic` [textSize 8]
          , spacer
          , if paymentAddress == "" then 
              label "not set"
               `styleBasic` [textColor customRed, textSize 8]
            else
              label (toText paymentAddress)
               `styleBasic` [textSize 8]
          ]
      , spacer_ [width 5]
      , hstack 
          [ label "Value:"
              `styleBasic` [textSize 8]
          , spacer
          , label (fromString $ printf "%D ADA" $ toAda lovelace)
              `styleBasic` [textSize 8]
          ]
      , widgetIf (not $ null nativeAssets) $
          vstack
            [ spacer_ [width 5]
            , label "Native Assets:" `styleBasic` [textSize 8]
            , hstack
                [ spacer_ [width 10]
                , copyableTextArea 
                    (show $ align $ vsep $ map (pretty . showAssetInList reverseTickerMap) nativeAssets)
                    `styleBasic` [textSize 8, maxWidth 300]
                ]
            ]
      ] `styleBasic`
          [ bgColor customGray3
          , border 1 black
          , padding 10
          , maxWidth 600
          ]

userInputsList :: AppModel -> AppNode
userInputsList AppModel{txBuilderModel=TxBuilderModel{userInputs},reverseTickerMap} = do
  vstack
    [ label ("Personal UTxOs " <> show (tupled [pretty $ length userInputs]))
        `styleBasic` [textSize 12]
    , flip styleBasic [padding 5] $
        vscroll_ [wheelRate 50] $ vstack_ [childSpacing] (map utxoRow userInputs)
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
                      , copyableTextArea 
                          (show $ align $ vsep $ map (pretty . showAssetInList reverseTickerMap) nativeAssets)
                          `styleBasic` [textSize 10, textColor lightGray, maxWidth 300]
                      ]
                  ] `styleBasic` [padding 2]
            ] `styleBasic`
                [ bgColor black
                , padding 10
                , border 1 black
                ]
        ]

actionsList :: AppModel -> AppNode
actionsList AppModel{txBuilderModel=TxBuilderModel{..},reverseTickerMap} = do
  let numActions = length userOutputs
  vstack
    [ label ("Actions " <> show (tupled [pretty numActions]))
        `styleBasic` [textSize 12]
    , vscroll_ [wheelRate 50] $ userOutputsList reverseTickerMap userOutputs
    ] `styleBasic` [padding 5]

userOutputsList :: ReverseTickerMap -> [(Int,UserOutput)] -> AppNode
userOutputsList reverseTickerMap userOutputs = do
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
        [ hstack
            [ vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Edit Output" [tooltipDelay 0] $
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
                , box_ [alignCenter,alignTop] $ tooltip_ "Remove Output" [tooltipDelay 0] $
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
        , spacer_ [width 3]
        , vstack
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
                          (show $ align $ vsep $ map (pretty . showAssetInList reverseTickerMap) nativeAssets)
                          `styleBasic` [textSize 10, textColor lightGray, maxWidth 300]
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
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

addChangeOutputWidget :: AppModel -> AppNode
addChangeOutputWidget _ = do
  centerWidget $ vstack 
    [ hstack 
        [ label "Change Address:"
        , spacer
        , textField (toLensVL $ #txBuilderModel % #newChangeOutput % #paymentAddress)
            `styleBasic`
              [textSize 10]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ TxBuilderEvent $ AddNewChangeOutput CancelAdding
        , spacer
        , mainButton "Confirm" $ TxBuilderEvent $ AddNewChangeOutput ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20]

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

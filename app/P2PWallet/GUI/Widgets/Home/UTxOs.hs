{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.UTxOs where

import Monomer

import Prettyprinter (align, pretty, vsep)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Wallets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.MonomerOptics()
import P2PWallet.Plutus
import P2PWallet.Prelude

utxosWidget :: AppModel -> AppNode
utxosWidget model =
    zstack
      [ cushionWidgetH $ vstack
          [ hstack 
              [ label ("UTxOs (" <> show (length sample) <> ")")
                  `styleBasic` [textFont "Italics", textSize 14]
              , spacer_ [width 5]
              , tooltip_ "Sort/Filter/Search" [tooltipDelay 1000] $
                  toggleButton_ menuSearchIcon
                    (toLensVL $ #homeModel % #showUTxOFilter)
                    [toggleButtonOffStyle menuOffStyle]
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
          , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map utxoRow sample)
                `styleBasic` [padding 10]
          , filler
          ] 
      , utxoFilterWidget model `nodeVisible` (model ^. #homeModel % #showUTxOFilter)
      ]
  where
    filterModel :: UTxOFilterModel
    filterModel = model ^. #homeModel % #utxoFilterModel

    sortMethodSetting :: UTxOSortMethod
    sortMethodSetting = filterModel ^. #sortingMethod

    sortOrderSetting :: SortDirection
    sortOrderSetting = filterModel ^. #sortingDirection

    searchTarget :: Text
    searchTarget = filterModel ^. #search

    searchFilter :: [PersonalUTxO] -> [PersonalUTxO]
    searchFilter
      | searchTarget == "" = filter (const True)
      | otherwise = filter $ \p -> or
          [ p ^. #referenceScriptHash == Just searchTarget
          , p ^. #datumHash == Just searchTarget
          , flip any (p ^. #nativeAssets) $ \NativeAsset{..} -> or
              [ policyId == searchTarget
              , tokenName == searchTarget
              , policyId <> "." <> tokenName == searchTarget
              , fingerprint == searchTarget
              ]
          ]

    sorter
      | sortMethodSetting == UTxOLexicographical = sortOn (view #utxoRef)
      | sortMethodSetting == UTxOBalance = sortOn (view #lovelace)
      | otherwise = sortOn (view #blockTime)

    orderer
      | sortOrderSetting == SortAscending = id
      | otherwise = reverse

    filterer :: [PersonalUTxO] -> [PersonalUTxO]
    filterer us = do
      u@PersonalUTxO{..} <- us
      guard $ maybe True (isJust referenceScriptHash ==) (filterModel ^. #hasReferenceScript)
      guard $ maybe True (isJust datumHash ==) (filterModel ^. #hasDatum)
      guard $ maybe True (not (null nativeAssets) ==) (filterModel ^. #hasNativeAssets)
      return u

    sample :: [PersonalUTxO]
    sample = searchFilter $ filterer $
      orderer $ sorter $ model ^. #homeModel % #selectedWallet % #utxos

    menuSearchIcon :: Text
    menuSearchIcon = toGlyph 0XF3D1

    menuOffStyle :: Style
    menuOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            ]
          `styleHover`
            [ bgColor customGray1]

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

utxoFilterWidget:: AppModel -> AppNode
utxoFilterWidget model = do
  let currentScene = model ^. #homeModel % #utxoFilterScene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidgetV $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene (toLensVL $ #homeModel % #utxoFilterScene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radiusTL 10
                      , radiusBL 0
                      , radiusTR 0
                      , radiusBR 0
                      , border 1 black
                      ]
                , optionButton_ "Sort" SortScene (toLensVL $ #homeModel % #utxoFilterScene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radius 0
                      , border 1 black
                      ]
                , optionButton_ "Search" SearchScene (toLensVL $ #homeModel % #utxoFilterScene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radiusBL 10
                      , radiusTL 0
                      , radiusTR 0
                      , radiusBR 0
                      , border 1 black
                      ]
                ] `styleBasic` [height 100]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (currentScene == FilterScene) filterWidget
                    , widgetIf (currentScene == SortScene) sortWidget
                    , widgetIf (currentScene == SearchScene) searchWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ HomeEvent ResetUTxOFilters
                    , spacer
                    , toggleButton "Close" (toLensVL $ #homeModel % #showUTxOFilter)
                    ] `styleBasic` [padding 10]
                ] `styleBasic`
                    [ bgColor customGray3
                    , radiusTL 0
                    , radiusTR 10
                    , radiusBR 10
                    , radiusBL 10
                    , border 1 black
                    ]
            , filler
            ]
        ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]
  where
    filterWidget :: AppNode
    filterWidget = do
      let rooLens = #homeModel % #utxoFilterModel
          offStyle = def 
            `styleBasic` [ bgColor customGray1 , textColor white ]
            `styleHover` [ bgColor customBlue ]
          choiceButton caption field targetLens =
            centerWidgetV $ optionButton_ caption field targetLens
              [optionButtonOffStyle offStyle]
              `styleBasic` 
                [ bgColor customBlue
                , textColor white
                , radius 10
                , border 1 black
                , paddingT 2
                , paddingB 2
                , paddingL 7
                , paddingR 7
                , textSize 12
                ]
      vstack_ [childSpacing]
        [ spacer
        , vstack
            [ cushionWidgetH $ hstack_ [childSpacing]
                [ label "Reference Script:"
                , choiceButton "Yes" (Just True) (toLensVL $ rooLens % #hasReferenceScript)
                , choiceButton "No" (Just False) (toLensVL $ rooLens % #hasReferenceScript)
                , choiceButton "Either" Nothing (toLensVL $ rooLens % #hasReferenceScript)
                ]
            , cushionWidgetH $ hstack_ [childSpacing]
                [ label "Datum:"
                , choiceButton "Yes" (Just True) (toLensVL $ rooLens % #hasDatum)
                , choiceButton "No" (Just False) (toLensVL $ rooLens % #hasDatum)
                , choiceButton "Either" Nothing (toLensVL $ rooLens % #hasDatum)
                ]
            , cushionWidgetH $ hstack_ [childSpacing]
                [ label "Native Assets:"
                , choiceButton "Yes" (Just True) (toLensVL $ rooLens % #hasNativeAssets)
                , choiceButton "No" (Just False) (toLensVL $ rooLens % #hasNativeAssets)
                , choiceButton "Either" Nothing (toLensVL $ rooLens % #hasNativeAssets)
                ]
            ]
        ] `styleBasic` [height 100]

    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [bgColor customGray2, border 1 black]
                `styleHover` [bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [bgColor customGray1, border 1 customBlue]
      vstack_ [childSpacing]
        [ spacer
        , hstack
            [ spacer
            , label "Method:"
            , spacer
            , textDropdown_
                  (toLensVL $ #homeModel % #utxoFilterModel % #sortingMethod) 
                  utxoSortingMethods
                  displayUTxOSortMethod 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 200
                  , border 1 black
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        , hstack
            [ spacer
            , label "Order:"
            , spacer
            , textDropdown_
                  (toLensVL $ #homeModel % #utxoFilterModel % #sortingDirection) 
                  sortingDirections
                  displaySortDirection 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 150
                  , border 1 black
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        ] `styleBasic` [height 100]

    searchWidget :: AppNode
    searchWidget = do
      vstack_ [childSpacing]
        [ spacer
        , hstack 
            [ spacer
            , label "Find:"
            , spacer
            , textField_ 
                (toLensVL $ #homeModel % #utxoFilterModel % #search) 
                [placeholder "native token, reference script hash, datum hash"] 
            , spacer
            ]
        ] `styleBasic` [height 100]


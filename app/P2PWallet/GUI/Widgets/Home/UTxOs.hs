{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.UTxOs where

import Monomer

import Prettyprinter (align, pretty, vsep)
import Data.Text qualified as Text

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Wallets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Information
import P2PWallet.MonomerOptics()
import P2PWallet.Plutus
import P2PWallet.Prelude

utxosWidget :: AppModel -> AppNode
utxosWidget model =
    zstack
      [ cushionWidgetH $ vstack
          [ hstack 
              [ label ("UTxOs (" <> fractionShown <> ")")
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
              , tooltip_ "Expand All Details" [tooltipDelay 1000] $
                  mainButton expandAllIcon (HomeEvent ShowAllUTxODetails)
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      , padding 0
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              , tooltip_ "Collapse All Details" [tooltipDelay 1000] $
                  mainButton collapseAllIcon (HomeEvent HideAllUTxODetails)
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      , padding 0
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ]
          , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map utxoRow sample)
                `styleBasic` [padding 10]
          , filler
          ] 
      , utxoFilterWidget model `nodeVisible` (model ^. #homeModel % #showUTxOFilter)
      ]
  where
    fractionShown :: Text
    fractionShown = show (length sample) 
                 <> "/" 
                 <> show (length $ model ^. #homeModel % #selectedWallet % #utxos)

    filterModel :: UTxOFilterModel
    filterModel = model ^. #homeModel % #utxoFilterModel

    sortMethodSetting :: UTxOSortMethod
    sortMethodSetting = filterModel ^. #sortingMethod

    sortOrderSetting :: SortDirection
    sortOrderSetting = filterModel ^. #sortingDirection

    searchTargets :: [Text]
    searchTargets = words $ replace "," " " $ filterModel ^. #search

    targetQuantity :: PersonalUTxO -> Maybe Integer
    targetQuantity p =
      flip (maybe Nothing) (maybeHead searchTargets) $ \target ->
        fmap (view #quantity) $
          flip find (p ^. #nativeAssets) $ \NativeAsset{..} -> or
            [ policyId <> "." <> tokenName == target
            , fingerprint == target
            ]

    applySearchFilter :: [Text] -> [PersonalUTxO] -> [PersonalUTxO]
    applySearchFilter [] xs = xs
    applySearchFilter (target:ts) xs = applySearchFilter ts $ searchFilter target xs

    searchFilter :: Text -> [PersonalUTxO] -> [PersonalUTxO]
    searchFilter searchTarget
      | searchTarget == "" = filter (const True)
      | otherwise = filter $ \p -> or
          [ p ^. #referenceScriptHash == Just searchTarget
          , p ^. #datumHash == Just searchTarget
          , Text.isPrefixOf searchTarget $ showTxOutRef $ p ^. #utxoRef
          , flip any (p ^. #nativeAssets) $ \NativeAsset{..} -> or
              [ policyId == searchTarget
              , tokenName == searchTarget
              , policyId <> "." <> tokenName == searchTarget
              , fingerprint == searchTarget
              ]
          ]

    sorter = case sortMethodSetting of
      UTxOLexicographical -> sortOn (view #utxoRef)
      UTxOAdaBalance -> sortOn (view #lovelace)
      UTxOTime -> sortOn (view #blockTime)
      UTxOSearchTokenBalance -> sortOn targetQuantity

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
    sample = orderer 
           $ sorter 
           $ applySearchFilter searchTargets
           $ filterer 
           $ model ^. #homeModel % #selectedWallet % #utxos

    menuSearchIcon :: Text
    menuSearchIcon = toGlyph 0XF3D1

    expandAllIcon :: Text
    expandAllIcon = toGlyph 0XF326

    collapseAllIcon :: Text
    collapseAllIcon = toGlyph 0XF302

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
    [ centerWidget $ hstack
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
                    , toggleButton "Confirm" (toLensVL $ #homeModel % #showUTxOFilter)
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
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
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
      vstack
        [ spacer
        , vstack
            [ cushionWidgetH $ hstack_ [childSpacing]
                [ label "Reference Script:"
                , choiceButton "Yes" (Just True) (toLensVL $ rooLens % #hasReferenceScript)
                , choiceButton "No" (Just False) (toLensVL $ rooLens % #hasReferenceScript)
                , choiceButton "Either" Nothing (toLensVL $ rooLens % #hasReferenceScript)
                , mainButton remixInformationLine (Alert "Does the UTxO have a reference script?")
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      , padding 0
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ] `styleBasic` [height 30]
            , cushionWidgetH $ hstack_ [childSpacing]
                [ label "Datum:"
                , choiceButton "Yes" (Just True) (toLensVL $ rooLens % #hasDatum)
                , choiceButton "No" (Just False) (toLensVL $ rooLens % #hasDatum)
                , choiceButton "Either" Nothing (toLensVL $ rooLens % #hasDatum)
                , mainButton remixInformationLine (Alert "Does the UTxO have a datum?")
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      , padding 0
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ] `styleBasic` [height 30]
            , cushionWidgetH $ hstack_ [childSpacing]
                [ label "Native Assets:"
                , choiceButton "Yes" (Just True) (toLensVL $ rooLens % #hasNativeAssets)
                , choiceButton "No" (Just False) (toLensVL $ rooLens % #hasNativeAssets)
                , choiceButton "Either" Nothing (toLensVL $ rooLens % #hasNativeAssets)
                , mainButton remixInformationLine (Alert "Does the UTxO have native assets?")
                    `styleBasic`
                      [ border 0 transparent
                      , radius 20
                      , bgColor transparent
                      , textColor customBlue
                      , textMiddle
                      , textFont "Remix"
                      , padding 0
                      ]
                    `styleHover` [bgColor customGray2, cursorIcon CursorHand]
                ] `styleBasic` [height 30]
            ]
        ]

    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [bgColor customGray2, border 1 black]
                `styleHover` [bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [bgColor customGray1, border 1 customBlue]
          possibleSortingMethods
            | model ^. #homeModel % #utxoFilterModel % #search == "" = take 3 utxoSortingMethods
            | otherwise = utxoSortingMethods
      vstack_ [childSpacing]
        [ spacer
        , hstack
            [ spacer
            , label "Method:"
            , spacer
            , textDropdown_
                  (toLensVL $ #homeModel % #utxoFilterModel % #sortingMethod) 
                  possibleSortingMethods
                  displayUTxOSortMethod 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 200
                  , border 1 black
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , mainButton remixInformationLine (Alert utxoSortMsg)
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
        ]

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
                [placeholder "many of: native token, reference script hash, datum hash, tx hash"] 
            , mainButton remixInformationLine (Alert utxoSearchMsg)
                `styleBasic`
                  [ border 0 transparent
                  , radius 20
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , textFont "Remix"
                  ]
                `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer
            ]
        ]


module P2PWallet.GUI.Widgets.Home.UTxOs 
  ( utxosWidget
  ) where

import Monomer

import Prettyprinter (align, pretty, vsep)
import Data.Text qualified as Text
import Data.Map qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

utxosWidget :: AppModel -> AppNode
utxosWidget model@AppModel{homeModel=HomeModel{..},reverseTickerMap,config} =
    zstack
      [ cushionWidgetH $ vstack
          [ -- A header widget saying how many UTxOs match that search out of the total as well
            -- as a filter button for opening the filter menu. On the far right, there are expand
            -- and collapse all buttons for showing/hiding the details for all UTxOs.
            hstack 
              [ label ("UTxOs (" <> fractionShown <> ")")
                  `styleBasic` [textFont "Italics", textSize 14]
              , spacer_ [width 5]
              , tooltip_ "Sort/Filter/Search" [tooltipDelay 0] $
                  toggleButton_ menuSearchIcon
                    (toLensVL $ #homeModel % #showUTxOFilter)
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
              , tooltip_ "Expand All Details" [tooltipDelay 0] $
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
              , tooltip_ "Collapse All Details" [tooltipDelay 0] $
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
            -- These are the UTxOs that match those filter/search settings.
          , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map utxoRow sample)
                `styleBasic` [padding 10]
          , filler
          ] 
      , utxoFilterWidget model `nodeVisible` showUTxOFilter
      ]
  where
    sortMethodSetting :: UTxOSortMethod
    sortMethodSetting = utxoFilterModel ^. #sortingMethod

    sortOrderSetting :: SortDirection
    sortOrderSetting = utxoFilterModel ^. #sortingDirection

    searchTargets :: [Text]
    searchTargets = words $ replace "," " " $ utxoFilterModel ^. #search

    sample :: [PersonalUTxO]
    sample = orderer sortOrderSetting
           . sorter reverseTickerMap (maybeHead searchTargets) sortMethodSetting 
           . applySearchFilter reverseTickerMap searchTargets 
           . filterer utxoFilterModel
           $ selectedWallet ^. #utxos

    fractionShown :: Text
    fractionShown = 
      show (length sample) <> "/" <> show (length $ selectedWallet ^. #utxos)

    -- This is used for all toggleButton off styles.
    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    -- This is the button icon for showing/hiding UTxO details.
    moreIcon :: Bool -> Text
    moreIcon detailsOpen
      | detailsOpen = closeCircleIcon
      | otherwise = horizontalMoreIcon

    moreTip :: Bool -> Text
    moreTip detailsOpen
      | detailsOpen = "Close Details"
      | otherwise = "Show Details"

    utxoRow :: PersonalUTxO -> AppNode
    utxoRow u@PersonalUTxO{..} =
      vstack
        [ vstack
            [ hstack 
                [ copyableLabelSelf 10 (display utxoRef)
                , filler
                , label (display lovelace) 
                    `styleBasic` [textSize 10]
                ]
            , hstack
                [ label calendarIcon
                    `styleBasic` 
                      [ textSize 10
                      , textColor customBlue
                      , textFont "Remix"
                      , paddingT 5
                      ]
                , spacer_ [width 3]
                , label (showLocalDate (config ^. #timeZone) blockTime)
                    `styleBasic` 
                      [ textSize 10
                      , textColor lightGray
                      ]
                , spacer
                , label clockIcon
                    `styleBasic` 
                      [ textSize 10
                      , textColor customBlue
                      , textFont "Remix"
                      , paddingT 5
                      ]
                , spacer_ [width 3]
                , label (showLocalTime (config ^. #timeZone) blockTime)
                    `styleBasic` 
                      [ textSize 10
                      , textColor lightGray
                      ]
                , spacer
                , widgetIf (not $ null nativeAssets) $ 
                    hstack
                      [ tooltip_ "Native Assets" [tooltipDelay 0] $ label coinsIcon
                          `styleBasic` 
                            [ textSize 10
                            , textColor customBlue
                            , textFont "Remix"
                            , textMiddle
                            ]
                      , spacer_ [width 3]
                      ]
                , widgetIf (isJust datumHash) $ 
                    hstack
                      [ tooltip_ "Datum" [tooltipDelay 0] $ label datumIcon
                          `styleBasic` 
                            [ textSize 10
                            , textColor customBlue
                            , textFont "Remix"
                            , textMiddle
                            ]
                      , spacer_ [width 3]
                      ]
                , widgetIf (isJust referenceScriptHash) $ 
                    tooltip_ "Reference Script" [tooltipDelay 0] $ label scriptIcon
                      `styleBasic` 
                        [ textSize 10
                        , textColor customBlue
                        , textFont "Remix"
                        , textMiddle
                        ]
                , filler
                , hstack
                    [ tooltip_ "Use as collateral" [tooltipDelay 0] $
                        button collateralIcon (HomeEvent $ AddSelectedCollateralInput u)
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
                    ] `nodeVisible` and
                        [ null nativeAssets
                        , lovelace >= 5_000_000
                        ]
                , tooltip_ "Spend UTxO" [tooltipDelay 0] $
                    button spendUTxOIcon (HomeEvent $ AddSelectedUserInput u)
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
                , spacer_ [width 5]
                , tooltip_ (moreTip showDetails) [tooltipDelay 0] $
                    toggleButton_ (moreIcon showDetails)
                      (toLensVL $ #homeModel % #selectedWallet % #utxos % toggleDetails utxoRef)
                      [toggleButtonOffStyle toggleOffStyle]
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
    utxoDetails PersonalUTxO{..} = do
      let prettyAssets = map (pretty . showAssetBalance True reverseTickerMap) nativeAssets
      hstack
        [ filler
        , vstack
            [ copyableLabelFor 8 "Block Height:" (show @Text blockHeight)
                  `styleBasic` [padding 2]
            , widgetMaybe referenceScriptHash $ \hash ->
                copyableLabelFor 8 "Reference Script Hash:" hash
                  `styleBasic` [padding 2]
            , widgetMaybe datumHash $ \hash ->
                copyableLabelFor 8 "Datum Hash:" hash
                  `styleBasic` [padding 2]
            , widgetMaybe inlineDatum $ \x ->
                copyableLabelFor 8 "Inline Datum:" (showValue x)
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
                -- , maxHeight 100
                ]
        ]

utxoFilterWidget:: AppModel -> AppNode
utxoFilterWidget AppModel{homeModel=HomeModel{..}} = do
  let offStyle = def 
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
                    [ widgetIf (utxoFilterScene == FilterScene) filterWidget
                    , widgetIf (utxoFilterScene == SortScene) sortWidget
                    , widgetIf (utxoFilterScene == SearchScene) searchWidget
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
      let rootLens = #homeModel % #utxoFilterModel
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
                , choiceButton "Yes" (Just True) (toLensVL $ rootLens % #hasReferenceScript)
                , choiceButton "No" (Just False) (toLensVL $ rootLens % #hasReferenceScript)
                , choiceButton "Either" Nothing (toLensVL $ rootLens % #hasReferenceScript)
                , mainButton helpIcon (Alert "Does the UTxO have a reference script?")
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
                , choiceButton "Yes" (Just True) (toLensVL $ rootLens % #hasDatum)
                , choiceButton "No" (Just False) (toLensVL $ rootLens % #hasDatum)
                , choiceButton "Either" Nothing (toLensVL $ rootLens % #hasDatum)
                , mainButton helpIcon (Alert "Does the UTxO have a datum?")
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
                , choiceButton "Yes" (Just True) (toLensVL $ rootLens % #hasNativeAssets)
                , choiceButton "No" (Just False) (toLensVL $ rootLens % #hasNativeAssets)
                , choiceButton "Either" Nothing (toLensVL $ rootLens % #hasNativeAssets)
                , mainButton helpIcon (Alert "Does the UTxO have native assets?")
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
            | utxoFilterModel ^. #search == "" = take 3 utxoSortingMethods
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
                  display 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 200
                  , border 1 black
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , mainButton helpIcon (Alert utxoSortMsg)
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
                  display 
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
                `styleBasic` [bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , mainButton helpIcon (Alert utxoSearchMsg)
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

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Double -> Text -> WidgetNode s AppEvent
copyableLabelSelf fontSize caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textSize fontSize
      , border 0 transparent
      , textColor white
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

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
-- Helper Lens
-------------------------------------------------
-- | A lens to toggle the `showDetails` field of the `PersonalUTxO`.
toggleDetails :: TxOutRef -> Lens' [PersonalUTxO] Bool
toggleDetails ref = lens (getToggleDetails ref) (setToggleDetails ref)
  where
    getToggleDetails :: TxOutRef -> [PersonalUTxO] -> Bool
    getToggleDetails _ [] = False
    getToggleDetails targetRef (u:us) =
      if u ^. #utxoRef == targetRef 
      then u ^. #showDetails
      else getToggleDetails targetRef us

    setToggleDetails :: TxOutRef -> [PersonalUTxO] -> Bool -> [PersonalUTxO]
    setToggleDetails _ [] _ = []
    setToggleDetails targetRef (u:us) b =
      if u ^. #utxoRef == targetRef 
      then (u & #showDetails .~ b) : us
      else u : setToggleDetails targetRef us b

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- Sort the list based of the first search criteria. The first criteria is expected to 
-- be a native asset.
targetQuantity :: ReverseTickerMap -> Maybe Text -> PersonalUTxO -> Maybe Integer
targetQuantity reverseTickerMap mTarget p =
  flip (maybe Nothing) mTarget $ \target ->
    fmap (view #quantity) $
      flip find (p ^. #nativeAssets) $ \NativeAsset{..} -> or
        [ display policyId <> "." <> display tokenName == target
        , display fingerprint == target
        , Just target == fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap)
        ]

sorter :: ReverseTickerMap -> Maybe Text -> UTxOSortMethod -> [PersonalUTxO] -> [PersonalUTxO]
sorter reverseTickerMap mTarget sortMethod = case sortMethod of
  UTxOLexicographical -> sortOn (view #utxoRef)
  UTxOAdaBalance -> sortOn (view #lovelace)
  UTxOTime -> sortOn (view #blockTime)
  UTxOSearchTokenBalance -> sortOn (targetQuantity reverseTickerMap mTarget) -- Only used with search

orderer :: SortDirection -> [PersonalUTxO] -> [PersonalUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

filterer :: UTxOFilterModel -> [PersonalUTxO] -> [PersonalUTxO]
filterer UTxOFilterModel{..} us = do
  u@PersonalUTxO{..} <- us
  guard $ maybe True (isJust referenceScriptHash ==) hasReferenceScript
  guard $ maybe True (isJust datumHash ==) hasDatum
  guard $ maybe True (not (null nativeAssets) ==) hasNativeAssets
  return u

-- Apply the search filter recursively since users can search for multiple criteria
-- and the UTxOs must match all set criteria.
applySearchFilter :: ReverseTickerMap -> [Text] -> [PersonalUTxO] -> [PersonalUTxO]
applySearchFilter _ [] !xs = xs
applySearchFilter reverseTickerMap (target:ts) !xs = 
  applySearchFilter reverseTickerMap ts $ searchFilter reverseTickerMap target xs

searchFilter :: ReverseTickerMap -> Text -> [PersonalUTxO] -> [PersonalUTxO]
searchFilter reverseTickerMap searchTarget !xs
  | searchTarget == "" = xs
  | otherwise = flip filter xs $ \p -> or
      [ p ^. #referenceScriptHash == Just searchTarget
      , p ^. #datumHash == Just searchTarget
      , Text.isPrefixOf searchTarget $ display $ p ^. #utxoRef
      , flip any (p ^. #nativeAssets) $ \NativeAsset{..} -> or
          [ display policyId == searchTarget
          , display tokenName == searchTarget
          , display policyId <> "." <> display tokenName == searchTarget
          , display fingerprint == searchTarget
          , Just searchTarget ==
              fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap) 
          ]
      ]

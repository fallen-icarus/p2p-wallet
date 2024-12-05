module P2PWallet.GUI.Widgets.Delegation.PoolPicker 
  (
    poolPickerWidget
  , poolFilterWidget
  ) where

import Monomer hiding (icon)
import Data.Text qualified as Text
import Data.Ord qualified as Ord

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Koios.Pool
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

poolPickerWidget :: AppModel -> AppNode
poolPickerWidget AppModel{delegationModel=DelegationModel{poolFilterModel,..}} = do
  flip styleBasic [bgColor $ black & #a .~ 0.4, padding 30] $ box $
    centerWidget $ vstack
      [ hstack 
          [ filler
          , label "Pool Picker"
          , filler
          , tooltip_ "Close" [tooltipDelay 0] $ toggleButton closeCircleIcon
              (toLensVL $ #delegationModel % #showPoolPicker)
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , padding 2
                , bgColor transparent
                , textColor lightGray
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
          ]
      , spacer
      , hstack
          [ tooltip_ "Filter" [tooltipDelay 0] $
              toggleButton_ menuSearchIcon
                (toLensVL $ #delegationModel % #showPoolFilter)
                [toggleButtonOffStyle menuOffStyle]
                `styleBasic`
                  [ border 0 transparent
                  , textSize 12
                  , bgColor transparent
                  , textColor customBlue
                  , textMiddle
                  , textFont "Remix"
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
          , textField_ 
                (toLensVL $ #delegationModel % #poolFilterModel % #search) 
                [placeholder "ticker, pool id"] 
              `styleBasic` [bgColor customGray1, sndColor darkGray]
              `styleFocus` [border 1 customBlue]
          ]
      , spacer
      , hgrid_ [childSpacing]
          [ box_ [alignMiddle] $ label "Pool Description"
              `styleBasic` [textMiddle, textSize 8]
          , box_ [alignMiddle] $ vstack
              [ sortButton ascendingSortIcon (PoolActiveSaturation SortAscending)
              , headerField "Active Saturation" activeSaturationMsg
              , sortButton descendingSortIcon (PoolActiveSaturation SortDescending)
              ]
          , box_ [alignMiddle] $ vstack
              [ sortButton ascendingSortIcon (PoolActivePledge SortAscending)
              , headerField "Pledge" pledgeMsg
              , sortButton descendingSortIcon (PoolActivePledge SortDescending)
              ]
          , box_ [alignMiddle] $ vstack
              [ sortButton ascendingSortIcon (PoolCost SortAscending)
              , headerField "Cost" fixedCostMsg
              , sortButton descendingSortIcon (PoolCost SortDescending)
              ]
          , box_ [alignMiddle] $ vstack
              [ sortButton ascendingSortIcon (PoolMargin SortAscending)
              , headerField "Margin" marginMsg
              , sortButton descendingSortIcon (PoolMargin SortDescending)
              ]
          , box_ [alignMiddle, onClick $ DelegationEvent $ SyncRegisteredPools $ StartProcess Nothing] $ 
              tooltip_ "Refresh" [tooltipDelay 0] $ 
                label refreshIcon
                  `styleBasic`
                    [ textSize 10
                    , bgColor transparent
                    , padding 3
                    , textMiddle
                    , radius 10
                    , textColor customBlue
                    , textFont "Remix"
                    ]
                  `styleHover`
                    [ bgColor customGray1 
                    , cursorIcon CursorHand
                    ]
          ] `styleBasic` [padding 5, bgColor customGray4, radiusTL 5, radiusTR 5]
      , spacer_ [width 1]
      , vscroll_ [scrollOverlay, wheelRate 50, barWidth 3, thumbWidth 3] $ 
          vstack_ [childSpacing_ 1] $
            for sample $ \PoolList{..} -> do
              let newDelegationEvent = DelegationEvent $ 
                    AddSelectedUserCertificate (ticker, StakeDelegation poolId)
                  prettyActiveSaturation = 
                    displayPercentage (maybe 0 toRational activeSaturation) <> "%"
              hgrid_ [childSpacing]
                [ vstack 
                    [ widgetMaybe ticker $ \t -> 
                        label t
                          `styleBasic` [textLeft, textSize 8, textColor white]
                    , copyableTruncatedPoolId 8 lightGray poolId
                    , widgetMaybe url $ copyableLabelSelf 8 lightGray
                    ]
                , box_ [alignMiddle] $ 
                    label prettyActiveSaturation
                      `styleBasic` [textMiddle, textSize 8]
                , box_ [alignMiddle] $
                    label (display $ fromMaybe 0 pledge)
                        `styleBasic` [textCenter, textSize 8]
                , box_ [alignMiddle] $ 
                    label (display $ fromMaybe 0 fixedCost)
                      `styleBasic` [textMiddle, textSize 8]
                , box_ [alignMiddle] $ 
                    label (fromString $ printf "%D%%" $ (*100) $ fromMaybe 0 margin)
                      `styleBasic` [textMiddle, textSize 8]
                , box_ [ignoreEmptyArea, alignMiddle, onClick newDelegationEvent] $
                    label "Delegate" 
                      `styleBasic`
                        [ textSize 10
                        , bgColor customBlue
                        , padding 3
                        , textMiddle
                        , radius 10
                        , textColor lightGray
                        ]
                      `styleHover`
                        [ bgColor customGray1 
                        , cursorIcon CursorHand
                        ]
                ] `styleBasic` [padding 5, bgColor customGray4]
      , spacer
      , box_ [alignRight] $ hstack
          [ let shouldBeEnabled = previousPage /= -1 in 
              pageButton "Previous" previousPage (not shouldBeEnabled)
                `nodeEnabled` shouldBeEnabled
          , spacer_ [width 3]
          , let shouldBeEnabled = actualSampleSize == sampleSize in
              pageButton "Next" nextPage (not shouldBeEnabled)
                `nodeEnabled` shouldBeEnabled
          ]
      ] `styleBasic`
          [ bgColor customGray2 
          , radius 20
          , padding 20
          ]
  where
    PoolFilterModel{..} = poolFilterModel

    previousPage :: Int
    previousPage = currentPage - 1

    nextPage :: Int
    nextPage = currentPage + 1

    searchTarget :: Text
    searchTarget = search

    menuOffStyle :: Style
    menuOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            , textSize 12
            ]
          `styleHover`
            [ bgColor customGray1]

    sample :: [PoolList]
    sample = take sampleSize
           . drop (currentPage * sampleSize)
           . sorter sortMethod
           . filterer poolFilterModel
           . searchFilter searchTarget 
           $ registeredPools

    actualSampleSize :: Int
    actualSampleSize = length sample

    sortButton :: Text -> PoolSortMethod -> AppNode
    sortButton icon method = do
      let activeColor
            | sortMethod == method = customBlue
            | otherwise = lightGray
      box_ [onClick $ DelegationEvent $ ChangePoolPickerSortMethod method] $
        label icon 
          `styleBasic`
            [ textSize 12
            , textColor activeColor
            , padding 0
            , textMiddle
            , textFont "Remix"
            ]
          `styleHover`
            [ textColor customBlue
            , cursorIcon CursorHand
            ]

poolFilterWidget :: AppNode
poolFilterWidget = do
  vstack
    [ centerWidget $ vstack
        [ hstack
            [ label "Results per page:"
                `styleBasic` [textSize 12, textColor lightGray]
            , spacer_ [width 5]
            , numericField_ (toLensVL $ #delegationModel % #poolFilterModel % #sampleSize)
                  [minValue 0, decimals 0]
                `styleBasic` [bgColor customGray1, width 100, height 30, textSize 10]
                `styleFocus` [border 1 customBlue]
            ] 
        , spacer
        , vstack
            [ label "Margin:"
                `styleBasic` [textSize 12, textColor lightGray]
            , spacer
            , hstack
                [ spacer_ [width 30]
                , label "min"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , numericField_ (toLensVL $ #delegationModel % #poolFilterModel % #marginRange % _1)
                      [minValue 0, maxValue 100, decimals 3]
                    `styleBasic` [bgColor customGray1, width 100, textSize 10, height 30]
                    `styleFocus` [border 1 customBlue]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , numericField_ (toLensVL $ #delegationModel % #poolFilterModel % #marginRange % _2)
                      [minValue 0, maxValue 100, decimals 3]
                    `styleBasic` [bgColor customGray1, width 100, textSize 10, height 30]
                    `styleFocus` [border 1 customBlue]
                ]
            ] 
        , spacer
        , vstack
            [ label "Active Saturation:"
                `styleBasic` [textSize 12, textColor lightGray]
            , spacer
            , hstack
                [ spacer_ [width 30]
                , label "min"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , numericField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % #activeSaturationRange % _1)
                      [minValue 0, maxValue 100, decimals 3]
                    `styleBasic` [bgColor customGray1, width 100, textSize 10, height 30]
                    `styleFocus` [border 1 customBlue]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , numericField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % #activeSaturationRange % _2)
                      [minValue 0, maxValue 100, decimals 3]
                    `styleBasic` [bgColor customGray1, width 100, textSize 10, height 30]
                    `styleFocus` [border 1 customBlue]
                ]
            ] 
        , spacer
        , vstack
            [ label "Fixed Cost:"
                `styleBasic` [textSize 12, textColor lightGray]
            , spacer
            , hstack
                [ spacer_ [width 30]
                , label "min"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , textField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % fixedCostMinimum)
                      [placeholder "170"]
                    `styleBasic` 
                      [ width 100
                      , textSize 10
                      , height 30
                      , bgColor customGray1
                      , sndColor darkGray
                      ]
                    `styleFocus` [border 1 customBlue]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , textField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % fixedCostMaximum)
                      [placeholder "340"]
                    `styleBasic` 
                      [ width 100
                      , textSize 10
                      , height 30
                      , bgColor customGray1
                      , sndColor darkGray
                      ]
                    `styleFocus` [border 1 customBlue]
                ]
            ] 
        , spacer
        , vstack
            [ label "Pledge:"
                `styleBasic` [textSize 12, textColor lightGray]
            , spacer
            , hstack
                [ spacer_ [width 30]
                , label "min"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , textField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % pledgeMinimum)
                      [placeholder "1"]
                    `styleBasic` 
                      [ width 100
                      , textSize 10
                      , height 30
                      , bgColor customGray1
                      , sndColor darkGray
                      ]
                    `styleFocus` [border 1 customBlue]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , textField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % pledgeMaximum)
                      [placeholder "100000"]
                    `styleBasic` 
                      [ width 100
                      , textSize 10
                      , height 30
                      , bgColor customGray1
                      , sndColor darkGray
                      ]
                    `styleFocus` [border 1 customBlue]
                ]
            ] 
        , spacer
        , box_ [alignRight] $ hstack
            [ button "Reset" $ DelegationEvent ResetPoolFilters
            , spacer
            , toggleButton "Confirm" (toLensVL $ #delegationModel % #showPoolFilter)
            ] `styleBasic` [padding 10]
        ] `styleBasic`
            [ bgColor customGray3
            , radius 10
            , border 1 black
            , padding 20
            ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
        , radius 10
        ]

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
headerField :: Text -> Text -> AppNode
headerField caption helpMsg =
  hstack
    [ label caption
        `styleBasic`
          [ textSize 8
          , textMiddle
          ]
    , spacer_ [width 2]
    , box_ [onClick $ Alert helpMsg] $ 
        label helpIcon
          `styleBasic`
            [ textColor customBlue 
            , textSize 10
            , textFont "Remix"
            , bgColor transparent
            , textMiddle
            , radius 10
            ]
          `styleHover`
            [ bgColor customGray2 
            , cursorIcon CursorHand
            ]
    ]

pageButton :: Text -> Int -> Bool -> AppNode
pageButton caption pageNum isDisabled = do
  let targetColor
        | isDisabled = darkGray
        | otherwise = customBlue
      offStyle = def 
        `styleBasic`
          [ bgColor customGray4
          , border 1 targetColor
          , textColor targetColor
          , padding 3
          , textSize 10
          ]
        `styleHover` [ bgColor customGray1 ]
  optionButton_ caption pageNum (toLensVL $ #delegationModel % #poolFilterModel % #currentPage)
      [optionButtonOffStyle offStyle]
    `styleBasic`
      [ bgColor customGray4
      , border 1 targetColor
      , textColor targetColor
      , padding 3
      , textSize 10
      ]
    `styleHover` [bgColor customGray1]
      
-- | A label button that will copy itself but display a truncated version.
copyableTruncatedPoolId :: Double -> Color -> PoolID -> WidgetNode s AppEvent
copyableTruncatedPoolId fontSize mainColor (PoolID text) = 
  tooltip_ "Copy" [tooltipDelay 0] $ button (Text.take 10 text <> "...") (CopyText text)
    `styleBasic`
      [ padding 0
      , textLeft
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

-- | A label button that will copy itself.
copyableLabelSelf :: Double -> Color -> Text -> WidgetNode s AppEvent
copyableLabelSelf fontSize mainColor caption = 
  tooltip_ "Copy" [tooltipDelay 0] $ button caption (CopyText caption)
    `styleBasic`
      [ padding 0
      , textLeft
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor customBlue, cursorIcon CursorHand]

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
fixedCostMinimum :: Lens' PoolFilterModel Text
fixedCostMinimum = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: PoolFilterModel -> Text
    getLowerBoundText model = maybe "" show $ model ^. #fixedCostRange % _1

    setLowerBoundText :: PoolFilterModel -> Text -> PoolFilterModel
    setLowerBoundText model decimal = model & #fixedCostRange % _1 .~ readMaybe (toString decimal)

fixedCostMaximum :: Lens' PoolFilterModel Text
fixedCostMaximum = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: PoolFilterModel -> Text
    getLowerBoundText model = maybe "" show $ model ^. #fixedCostRange % _2

    setLowerBoundText :: PoolFilterModel -> Text -> PoolFilterModel
    setLowerBoundText model decimal = model & #fixedCostRange % _2 .~ readMaybe (toString decimal)

pledgeMinimum :: Lens' PoolFilterModel Text
pledgeMinimum = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: PoolFilterModel -> Text
    getLowerBoundText model = maybe "" show $ model ^. #pledgeRange % _1

    setLowerBoundText :: PoolFilterModel -> Text -> PoolFilterModel
    setLowerBoundText model decimal = model & #pledgeRange % _1 .~ readMaybe (toString decimal)

pledgeMaximum :: Lens' PoolFilterModel Text
pledgeMaximum = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: PoolFilterModel -> Text
    getLowerBoundText model = maybe "" show $ model ^. #pledgeRange % _2

    setLowerBoundText :: PoolFilterModel -> Text -> PoolFilterModel
    setLowerBoundText model decimal = model & #pledgeRange % _2 .~ readMaybe (toString decimal)

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
searchFilter :: Text -> [PoolList] -> [PoolList]
searchFilter searchTarget xs
  | searchTarget == "" = xs
  | otherwise = flip filter xs $ \PoolList{..} -> or
      [ display poolId == searchTarget
      , maybe False (Text.isPrefixOf searchTarget) ticker
      ]

sorter :: PoolSortMethod -> [PoolList] -> [PoolList]
sorter sortMethod = case sortMethod of
  PoolActiveSaturation SortAscending -> sortOn (view #activeSaturation)
  PoolActiveSaturation SortDescending -> sortOn (Ord.Down . view #activeSaturation)
  PoolActivePledge SortAscending -> sortOn (view #pledge)
  PoolActivePledge SortDescending -> sortOn (Ord.Down . view #pledge)
  PoolCost SortAscending -> sortOn (view #fixedCost)
  PoolCost SortDescending -> sortOn (Ord.Down . view #fixedCost)
  PoolMargin SortAscending -> sortOn (view #margin)
  PoolMargin SortDescending -> sortOn (Ord.Down . view #margin)

filterer :: PoolFilterModel -> [PoolList] -> [PoolList]
filterer poolFilterModel = filter $ \PoolList{..} -> and
  [ fmap (*100) margin >= Just (poolFilterModel ^. #marginRange % _1)
  , fmap (*100) margin <= Just (poolFilterModel ^. #marginRange % _2)
  , activeSaturation >= Just ((/100) $ poolFilterModel ^. #activeSaturationRange % _1)
  , activeSaturation <= Just ((/100) $ poolFilterModel ^. #activeSaturationRange % _2)
  , flip (maybe True) (poolFilterModel ^. #fixedCostRange % _1) $ \lowerBound ->
      fmap (unAda . toAda) fixedCost >= Just lowerBound
  , flip (maybe True) (poolFilterModel ^. #fixedCostRange % _2) $ \upperBound ->
      fmap (unAda . toAda) fixedCost <= Just upperBound
  , flip (maybe True) (poolFilterModel ^. #pledgeRange % _1) $ \lowerBound ->
      fmap (unAda . toAda) pledge >= Just lowerBound
  , flip (maybe True) (poolFilterModel ^. #pledgeRange % _2) $ \upperBound ->
      fmap (unAda . toAda) pledge <= Just upperBound
  ]

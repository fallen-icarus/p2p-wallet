{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Delegation.PoolPicker 
  (
    poolPickerWidget
  , poolFilterWidget
  ) where

import Monomer hiding (icon)
import Data.Text qualified as Text
import Prettyprinter ((<+>), pretty)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.Pool
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.MonomerOptics()
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
              [placeholder "ticker, name, pool id"] 
          ]
      , spacer
      , hgrid_ [childSpacing]
          [ centerWidgetH $ label "Pool Description"
              `styleBasic` [textMiddle, textSize 8]
          , centerWidgetH $ vstack
              [ sortButton ascendingSortIcon (PoolLiveSaturation SortAscending)
              , headerField "Live Saturation" liveSaturationMsg
              , sortButton descendingSortIcon (PoolLiveSaturation SortDescending)
              ]
          , centerWidgetH $ vstack
              [ sortButton ascendingSortIcon (PoolActivePledge SortAscending)
              , headerField "Pledge" pledgeMsg
              , sortButton descendingSortIcon (PoolActivePledge SortDescending)
              ]
          , centerWidgetH $ vstack
              [ sortButton ascendingSortIcon (PoolCost SortAscending)
              , headerField "Cost" fixedCostMsg
              , sortButton descendingSortIcon (PoolCost SortDescending)
              ]
          , centerWidgetH $ vstack
              [ sortButton ascendingSortIcon (PoolMargin SortAscending)
              , headerField "Margin" marginMsg
              , sortButton descendingSortIcon (PoolMargin SortDescending)
              ]
          , box_ [onClick $ SyncRegisteredPools StartSync] $ tooltip_ "Refresh" [tooltipDelay 0] $ 
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
            flip map sample $ \Pool{..} -> do
              let PoolInfo{..} = fromMaybe def info
                  displayName = if Text.length name <= 10 then name else Text.take 10 name <> "..."
                  nameAndTicker = show $ 
                    pretty ticker <+> "-" <+> pretty displayName
                  (pledgeIcon,pledgeColor)
                    | pledge > livePledge = (closeCircleIcon, customRed)
                    | otherwise = (circleCheckboxFillIcon, customBlue)
                  actualPledge = fromString $ printf "%D ADA" $ toAda $ fromMaybe 0 livePledge
              hgrid_ [childSpacing]
                [ vstack 
                    [ tooltip_ name [tooltipDelay 0] $ label nameAndTicker
                        `styleBasic` [textLeft, textSize 8, textColor white]
                    , copyableTruncatedPoolId 8 lightGray poolId
                    , copyableLabelSelf 8 lightGray homepage
                    ]
                , centerWidgetH $ 
                    label (fromString $ printf "%D%%" $ fromMaybe 0 liveSaturation)
                      `styleBasic` [textMiddle, textSize 8]
                , centerWidgetH $ box_ [alignMiddle] $ vstack
                    [ label (fromString $ printf "%D ADA" $ toAda $ fromMaybe 0 pledge)
                        `styleBasic` [textCenter, textSize 8]
                    , spacer_ [width 3]
                    , tooltip_ ("Actual: " <> actualPledge) [tooltipDelay 0] $ 
                        label pledgeIcon
                          `styleBasic` 
                            [ textMiddle
                            , textCenter
                            , textSize 8
                            , textFont "Remix"
                            , textColor pledgeColor
                            ]
                    ]
                , centerWidgetH $ 
                    label (fromString $ printf "%D ADA" $ toAda $ fromMaybe 0 fixedCost)
                      `styleBasic` [textMiddle, textSize 8]
                , centerWidgetH $ 
                    label (fromString $ printf "%D%%" $ (*100) $ fromMaybe 0 margin)
                      `styleBasic` [textMiddle, textSize 8]
                , box_ [onClick AppInit] $
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
      , hstack
          [ filler
          , let shouldBeEnabled = previousPage /= -1 in 
              pageButton "Previous" previousPage (not shouldBeEnabled)
                `nodeEnabled` shouldBeEnabled
          , spacer_ [width 3]
          , let shouldBeEnabled = actualSampleSize == poolFilterModel ^. #sampleSize in
              pageButton "Next" nextPage (not shouldBeEnabled)
                `nodeEnabled` shouldBeEnabled
          ]
      ] `styleBasic`
          [ bgColor customGray2 
          , radius 20
          , padding 20
          ]

  where
    sortMethod :: PoolSortMethod
    sortMethod = poolFilterModel ^. #sortMethod

    previousPage :: Int
    previousPage = (poolFilterModel ^. #currentPage) - 1

    nextPage :: Int
    nextPage = (poolFilterModel ^. #currentPage) + 1

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
      
    searchTarget :: Text
    searchTarget = poolFilterModel ^. #search

    searchFilter :: [Pool] -> [Pool]
    searchFilter
      | searchTarget == "" = filter (const True)
      | otherwise = filter $ \Pool{..} -> or
          [ toText poolId == searchTarget
          , searchTarget `Text.isPrefixOf` fromMaybe "" (info ^? _Just % #ticker)
          , searchTarget `Text.isPrefixOf` fromMaybe "" (info ^? _Just % #name)
          ]

    menuOffStyle :: Style
    menuOffStyle = 
      def `styleBasic` 
            [ bgColor transparent
            , textColor customBlue
            , textSize 12
            ]
          `styleHover`
            [ bgColor customGray1]

    sorter = case sortMethod of
      PoolLiveSaturation SortAscending -> sortOn (view #liveSaturation)
      PoolLiveSaturation SortDescending -> reverse . sortOn (view #liveSaturation)
      PoolActivePledge SortAscending -> sortOn (view #livePledge)
      PoolActivePledge SortDescending -> reverse . sortOn (view #livePledge)
      PoolCost SortAscending -> sortOn (view #fixedCost)
      PoolCost SortDescending -> reverse . sortOn (view #fixedCost)
      PoolMargin SortAscending -> sortOn (view #margin)
      PoolMargin SortDescending -> reverse . sortOn (view #margin)

    filterer :: [Pool] -> [Pool]
    filterer = filter $ \Pool{..} -> and
      [ fmap (*100) margin >= Just (poolFilterModel ^. #marginRange % _1)
      , fmap (*100) margin <= Just (poolFilterModel ^. #marginRange % _2)
      , liveSaturation >= Just (poolFilterModel ^. #liveSaturationRange % _1)
      , liveSaturation <= Just (poolFilterModel ^. #liveSaturationRange % _2)
      , flip (maybe True) (poolFilterModel ^. #fixedCostRange % _1) $ \lowerBound ->
          fmap (unAda . toAda) fixedCost >= Just lowerBound
      , flip (maybe True) (poolFilterModel ^. #fixedCostRange % _2) $ \upperBound ->
          fmap (unAda . toAda) fixedCost <= Just upperBound
      , flip (maybe True) (poolFilterModel ^. #pledgeRange % _1) $ \lowerBound ->
          fmap (unAda . toAda) pledge >= Just lowerBound
      , flip (maybe True) (poolFilterModel ^. #pledgeRange % _2) $ \upperBound ->
          fmap (unAda . toAda) pledge <= Just upperBound
      ]
      
    sample :: [Pool]
    sample = take (poolFilterModel ^. #sampleSize) 
           $ drop (poolFilterModel ^. #currentPage * poolFilterModel ^. #sampleSize)
           $ sorter 
           $ filterer 
           $ searchFilter registeredPools

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

poolFilterWidget :: AppModel -> AppNode
poolFilterWidget _ = do
  vstack
    [ centerWidget $ vstack
        [ hstack
            [ label "Results per page:"
                `styleBasic` [textSize 12, textColor lightGray]
            , spacer_ [width 5]
            , numericField_ (toLensVL $ #delegationModel % #poolFilterModel % #sampleSize)
                  [minValue 0, decimals 0]
                `styleBasic` [width 100, height 30, textSize 10]
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
                    `styleBasic` [width 100, textSize 10, height 30]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , numericField_ (toLensVL $ #delegationModel % #poolFilterModel % #marginRange % _2)
                      [minValue 0, maxValue 100, decimals 3]
                    `styleBasic` [width 100, textSize 10, height 30]
                ]
            ] 
        , spacer
        , vstack
            [ label "Saturation:"
                `styleBasic` [textSize 12, textColor lightGray]
            , spacer
            , hstack
                [ spacer_ [width 30]
                , label "min"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , numericField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % #liveSaturationRange % _1)
                      [minValue 0, maxValue 100, decimals 3]
                    `styleBasic` [width 100, textSize 10, height 30]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , numericField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % #liveSaturationRange % _2)
                      [minValue 0, maxValue 100, decimals 3]
                    `styleBasic` [width 100, textSize 10, height 30]
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
                    `styleBasic` [width 100, textSize 10, height 30]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , textField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % fixedCostMaximum)
                      [placeholder "340"]
                    `styleBasic` [width 100, textSize 10, height 30]
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
                    `styleBasic` [width 100, textSize 10, height 30]
                , spacer_ [width 10]
                , label "max"
                    `styleBasic` [textSize 10, textColor lightGray]
                , spacer_ [width 5]
                , textField_ 
                      (toLensVL $ #delegationModel % #poolFilterModel % pledgeMaximum)
                      [placeholder "100000"]
                    `styleBasic` [width 100, textSize 10, height 30]
                ]
            ] 
        , hstack
            [ filler
            , button "Reset" $ DelegationEvent ResetPoolFilters
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


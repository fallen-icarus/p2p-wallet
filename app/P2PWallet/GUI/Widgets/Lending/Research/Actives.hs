module P2PWallet.GUI.Widgets.Lending.Research.Actives
  ( 
    researchActivesWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

researchActivesWidget :: AppModel -> AppNode
researchActivesWidget model@AppModel{lendingModel=LendingModel{..}} = do
  let LoanResearchModel{selectedActiveLoanConfiguration,newActiveLoanConfiguration} = researchModel
  zstack 
    [ allActivesWidget model
        `nodeVisible` (isJust selectedActiveLoanConfiguration && isNothing newActiveLoanConfiguration)
    , getActiveLoanConfiguration model
        `nodeVisible` (isNothing selectedActiveLoanConfiguration || isJust newActiveLoanConfiguration)
    , activesFilterWidget model `nodeVisible` (researchModel ^. #showActivesFilter)
    ]

getActiveLoanConfiguration :: AppModel -> AppNode
getActiveLoanConfiguration model = do
  let maybeLens' = maybeLens def $ #lendingModel % #researchModel % #newActiveLoanConfiguration
  centerWidget $ vstack
    [ spacer
    , box_ [alignMiddle] $
        label "What loan terms are you interested in?"
          `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
    , spacer
    , centerWidgetH $ hstack
        [ box_ [alignMiddle, onClick $ Alert askCfgLoanAmountMsg] $
            label helpIcon
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                , textSize 10
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer_ [width 3]
        , label "Loan Asset:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % #loanAsset) 
              [placeholder "ADA"] 
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , centerWidgetH $ hstack
        [ box_ [alignMiddle, onClick $ Alert askCfgMinDurationMsg] $
            label helpIcon
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                , textSize 10
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer_ [width 3]
        , label "Minimum Duration (Days):"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % #minDuration) 
              [placeholder "10"] 
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , filler
        , box_ [alignMiddle, onClick $ Alert askCfgMaxDurationMsg] $
            label helpIcon
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                , textSize 10
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer_ [width 3]
        , label "Maximum Duration (Days):"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % #maxDuration) 
              [placeholder "100"] 
            `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , separatorLine `styleBasic` [fgColor darkGray]
    , spacer
    , hstack
        [ box_ [alignMiddle, onClick $ Alert askCfgCollateralMsg] $
            label helpIcon
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                , textSize 10
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer_ [width 3]
        , label "Collateral Assets (separated with newlines):"
            `styleBasic` [textSize 10]
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % #collateral)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (LendingEvent $ LoanResearchEvent $ InitializeActiveLoanConfiguration CancelAdding)
              `styleBasic` [textSize 10]
              `nodeVisible` isJust (model ^. #lendingModel % #researchModel % #selectedActiveLoanConfiguration)
          , spacer
          , mainButton "Confirm" (LendingEvent $ LoanResearchEvent $ InitializeActiveLoanConfiguration ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic`
        [ bgColor customGray3
        , padding 20
        , radius 20
        ]

allActivesWidget :: AppModel -> AppNode
allActivesWidget AppModel{lendingModel=LendingModel{..},reverseTickerMap} =
    cushionWidgetH $ vstack
      [ centerWidgetH $ hstack 
          [ label "Active Loans"
              `styleBasic` [textFont "Italics", textSize 14]
          , tooltip_ "Sort/Filter" [tooltipDelay 0] $
              toggleButton_ menuSearchIcon
                (toLensVL $ #lendingModel % #researchModel % #showActivesFilter)
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
          ]
      , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
          vstack_ [childSpacing] (map activeRow sample)
            `styleBasic` [padding 10]
      , filler
      ] 
  where
    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    LoanResearchModel{scene=_,..} = researchModel

    sample :: [LoanUTxO]
    sample = orderer (activesFilterModel ^. #sortingDirection) 
           $ sorter (activesFilterModel ^. #sortingMethod) 
           $ filterer reverseTickerMap activesFilterModel
           $ filter ((==Just True) . fmap (is _ActiveDatum) . view #loanDatum)
           $ fromMaybe [] $ selectedActiveLoanConfiguration >>= flip lookupCachedActiveLoans cachedActiveLoans

    collateralAssetWidget :: NativeAsset -> (NativeAsset,Rational) -> AppNode
    collateralAssetWidget loanAsset (collateralAsset, price) = do
      let formattedPrice = showPriceFormatted reverseTickerMap collateralAsset loanAsset price
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap collateralAsset
            , " / "
            , showAssetNameOnly reverseTickerMap loanAsset
            ]
      hstack
        [ spacer_ [width 2]
        , label prettyPrice
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

    activeRow :: LoanUTxO -> AppNode
    activeRow u@LoanUTxO{loanAddress,utxoRef} = do
      let Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum u
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                           $ collateralization ^. #unCollateralization
          prettyInterest 
            | loanInterest == 0 = "Interest-Free"
            | otherwise = unwords
                [ if compoundingInterest then "Compounding" else "Non-Compounding"
                , "Interest:"
                , displayPercentage (toRational loanInterest) <> "%"
                ]
          prettyEpochDuration = flip (maybe "No Loan Epochs") epochDuration $ \freq ->
            unwords
              [ "Loan Epoch:"
              , show (calcDaysInPosixPeriod $ fromPlutusTime freq)
              , "Day(s)"
              ]
          prettyMinPayment = unwords
            [ "Minimum Payment:"
            , showAssetBalance True reverseTickerMap $ loanAmount & #quantity .~ minPayment
            ]
          prettyPenalty = case penalty of
            Loans.NoPenalty -> "No Penalty"
            Loans.FixedFee fee -> unwords
              [ "Fee Penalty:"
              , showAssetBalance True reverseTickerMap $ loanAmount & #quantity .~ fee
              ]
            Loans.PercentFee percent -> unwords
              [ "Percent Penalty:"
              , displayPercentage (toRational percent) <> "%"
              ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          loanHistoryEvt = LendingEvent $ LoanResearchEvent $ InspectResearchLoanHistory loanId
          lookupBorrowerEvt = LendingEvent 
                            $ LoanResearchEvent 
                            $ InspectResearchBorrowerInformation (borrowerId, loanAddress)
      vstack
        [ hstack
            [ label ("Loan For: " <> showAssetBalance True reverseTickerMap loanAmount)
                `styleBasic` [textSize 10, textColor customBlue]
            , spacer_ [width 5]
            , let prettyRef = display utxoRef in
              flip styleBasic [textSize 10] $ tooltip_ prettyRef [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ CopyText $ display utxoRef] $
                  label targetUTxOIcon
                    `styleBasic` 
                      [ bgColor black
                      , textMiddle
                      , textFont "Remix"
                      , textSize 8
                      , textColor customBlue
                      , paddingT 1
                      , paddingB 1
                      , paddingL 3
                      , paddingR 3
                      , radius 5
                      ]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Loan ID: " <> display loanId <> " (history)") [tooltipDelay 0] $
                  box_ [alignMiddle , onClick loanHistoryEvt] $
                    label keyNftIcon
                      `styleBasic` 
                        [ bgColor black
                        , textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        , paddingT 1
                        , paddingB 1
                        , paddingL 3
                        , paddingR 3
                        , radius 5
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ "Lookup Borrower Information" [tooltipDelay 0] $
                  box_ [alignMiddle , onClick lookupBorrowerEvt] $
                    label idCardIcon
                      `styleBasic` 
                        [ bgColor black
                        , textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        , paddingT 1
                        , paddingB 1
                        , paddingL 3
                        , paddingR 3
                        , radius 5
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , label "Duration:"
                `styleBasic` [textSize 10, textColor white]
            , spacer_ [width 3]
            , label (show duration <> " Days")
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 3]
        , hstack
            [ label prettyInterest
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyEpochDuration
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , widgetIf (isJust epochDuration) $ vstack
            [ spacer_ [width 3]
            , hstack
                [ label prettyMinPayment
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label prettyPenalty
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            ]
        , spacer_ [width 2]
        , hstack
            [ widgetIf collateralIsSwappable $ box_ [alignTop] $ hstack
                [ flip styleBasic [textSize 10] $ tooltip_ swapCollateralMsg [tooltipDelay 0] $
                    label swappableCollateralIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        , paddingT 1
                        ]
                , spacer_ [width 2]
                ]
            , box_ [alignTop] $ label "Collateralization:"
                `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
            , spacer_ [width 3]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
                \col -> hstack_ [childSpacing_ 3] $ map (collateralAssetWidget loanAmount) col
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

activesFilterWidget :: AppModel -> AppNode
activesFilterWidget AppModel{lendingModel=LendingModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #lendingModel % #researchModel
      filterScene = researchModel ^. #activesFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #activesFilterModel % #scene) 
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
                , optionButton_ "Sort" SortScene 
                    (toLensVL $ rootLens % #activesFilterModel % #scene) 
                    [optionButtonOffStyle offStyle]
                    `styleBasic` 
                      [ bgColor customGray3
                      , textColor customBlue
                      , radius 0
                      , border 1 black
                      ]
                ]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (filterScene == FilterScene) filterWidget
                    , widgetIf (filterScene == SortScene) sortWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ LendingEvent $ LoanResearchEvent ResetResearchActivesFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showActivesFilter)
                        [onClick $ LendingEvent $ LoanResearchEvent $ UpdateActiveLoanConfiguration $ StartProcess Nothing]
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
      let rootLens = #lendingModel % #researchModel % #activesFilterModel
      centerWidget $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert askCfgLoanAmountMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Loan Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #newActiveLoanConfiguration % #loanAsset) 
                  [placeholder "ADA"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , centerWidgetH $ hstack
            [ box_ [alignMiddle, onClick $ Alert askCfgMinDurationMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Minimum Duration (Days):"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #newActiveLoanConfiguration % #minDuration) 
                  [placeholder "10"] 
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , filler
            , box_ [alignMiddle, onClick $ Alert askCfgMaxDurationMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Maximum Duration (Days):"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #newActiveLoanConfiguration % #maxDuration) 
                  [placeholder "100"] 
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , separatorLine `styleBasic` [fgColor darkGray]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert askCfgCollateralMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , textSize 10
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            , spacer_ [width 3]
            , label "Collateral Assets (separated with newlines):"
                `styleBasic` [textSize 10]
            ]
        , spacer
        , textArea (toLensVL $ rootLens % #newActiveLoanConfiguration % #collateral)
            `styleBasic` [textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        ]

    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 12, bgColor customGray2, border 1 black]
                `styleHover` [textSize 12, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 12, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 12, bgColor customGray1, border 1 customBlue]
          possibleSortingMethods = enumFrom ActiveResearchLexicographically
          rootLens = #lendingModel % #researchModel % #activesFilterModel
      vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Sort Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , hstack
            [ spacer_ [width 40]
            , label "Method:" `styleBasic` [textSize 14]
            , spacer
            , textDropdown_
                  (toLensVL $ rootLens % #sortingMethod) 
                  possibleSortingMethods
                  display 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 200
                  , border 1 black
                  , textSize 12
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , spacer_ [width 3]
            , box_ [onClick $ Alert activeLoansResearchSortMsg] $
                label helpIcon
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , padding 5
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
            ]
        , spacer
        , hstack
            [ spacer_ [width 40]
            , label "Order:" `styleBasic` [textSize 14]
            , spacer
            , textDropdown_
                  (toLensVL $ rootLens % #sortingDirection) 
                  sortingDirections
                  display 
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 150
                  , border 1 black
                  , textSize 12
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
orderer :: SortDirection -> [LoanUTxO] -> [LoanUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: ActiveResearchSortMethod -> [LoanUTxO] -> [LoanUTxO]
sorter sortingMethod = 
  case sortingMethod of
    ActiveResearchLexicographically -> sortOn (view #utxoRef)
    ActiveResearchTime -> sortOn (view #blockTime)
    ActiveResearchLoanAmount -> sortOn loanUTxOLoanAmount
    ActiveResearchDuration -> sortOn loanUTxOLoanDuration
    ActiveResearchInterest -> sortOn loanUTxOLoanInterest

filterer :: ReverseTickerMap -> ActiveResearchFilterModel -> [LoanUTxO] -> [LoanUTxO]
filterer reverseTickerMap ActiveResearchFilterModel{newActiveLoanConfiguration=NewActiveLoanConfiguration{..}} us = do
    u <- us
    let activeDatum@Loans.ActiveDatum{loanTerm} = fromMaybe def $ loanUTxOActiveDatum u
        utxoCollateral = map (toNativeAsset . fst)
                       $ activeDatum ^. #collateralization % #unCollateralization
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) >= d) $ 
      readMaybe $ toString minDuration
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) <= d) $ 
      readMaybe $ toString maxDuration
    guard $ matchesAsset [toNativeAsset $ activeDatum ^. #loanAsset] loanAsset
    guard $ all (matchesAsset utxoCollateral) $ lines collateral
    return u
  where
    matchesAsset :: [NativeAsset] -> Text -> Bool
    matchesAsset xs searchTarget
      | searchTarget == "" = True
      | otherwise = flip any xs $ \NativeAsset{..} -> or
          [ display policyId <> "." <> display tokenName == searchTarget
          , Just searchTarget ==
              fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap) 
          , policyId == "" && searchTarget == "ADA"
          ]

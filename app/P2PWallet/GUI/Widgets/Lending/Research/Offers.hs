module P2PWallet.GUI.Widgets.Lending.Research.Offers
  ( 
    researchOffersWidget
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

researchOffersWidget :: AppModel -> AppNode
researchOffersWidget model@AppModel{lendingModel=LendingModel{..}} = do
  let LoanResearchModel{selectedLoanOfferConfiguration,newLoanOfferConfiguration} = researchModel
  zstack 
    [ allOffersWidget model
        `nodeVisible` (isJust selectedLoanOfferConfiguration && isNothing newLoanOfferConfiguration)
    , getLoanOfferConfiguration model
        `nodeVisible` (isNothing selectedLoanOfferConfiguration || isJust newLoanOfferConfiguration)
    , offersFilterWidget model `nodeVisible` (researchModel ^. #showOffersFilter)
    ]

getLoanOfferConfiguration :: AppModel -> AppNode
getLoanOfferConfiguration model = do
  let maybeLens' = maybeLens def $ #lendingModel % #researchModel % #newLoanOfferConfiguration
  centerWidget $ vstack
    [ spacer
    , box_ [alignMiddle] $
        label "What offer terms are you interested in?"
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
        , label "Collateral Assets (separated with newlines)"
            `styleBasic` [textSize 10]
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % #collateral)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (LendingEvent $ LoanResearchEvent $ InitializeLoanOfferConfiguration CancelAdding)
              `styleBasic` [textSize 10]
              `nodeVisible` isJust (model ^. #lendingModel % #researchModel % #selectedLoanOfferConfiguration)
          , spacer
          , mainButton "Confirm" (LendingEvent $ LoanResearchEvent $ InitializeLoanOfferConfiguration ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic`
        [ bgColor customGray3
        , padding 20
        , radius 20
        ]

allOffersWidget :: AppModel -> AppNode
allOffersWidget AppModel{knownWallets,lendingModel=LendingModel{..},reverseTickerMap,config} =
  cushionWidgetH $ vstack
    [ hstack 
        [ filler
        , label "Offers"
            `styleBasic` [textFont "Italics", textSize 14]
        , tooltip_ "Sort/Filter" [tooltipDelay 0] $
            toggleButton_ menuSearchIcon
              (toLensVL $ #lendingModel % #researchModel % #showOffersFilter)
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
        vstack_ [childSpacing] (map offerRow sample)
          `styleBasic` [padding 10]
    , filler
    ] 
  where
    Config{network} = config

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    LoanResearchModel{scene=_,..} = researchModel

    sample :: [LoanUTxO]
    sample = orderer (offersFilterModel ^. #sortingDirection) 
           $ sorter (offersFilterModel ^. #sortingMethod) 
           $ filterer reverseTickerMap offersFilterModel
           $ filter ((==Just True) . fmap (is _OfferDatum) . view #loanDatum)
           $ fromMaybe [] $ selectedLoanOfferConfiguration >>= flip lookupCachedOffers cachedLoanOffers

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

    offerRow :: LoanUTxO -> AppNode
    offerRow u@LoanUTxO{utxoRef,blockTime,loanAddress} = do
      let Loans.OfferDatum{..} = fromMaybe def $ loanUTxOOfferDatum u
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          borrowerId = Loans.genBorrowerId 
                     $ fromRight (PubKeyCredential "") 
                     $ paymentAddressStakeCredential loanAddress
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
          prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          prettyExpirationTime exprTime = unwords
            [ "Expires:"
            , showLocalDate (config ^. #timeZone) exprTime
            , showLocalTime (config ^. #timeZone) exprTime
            ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          payToAddress = either (const "error") fst $ plutusToBech32 network lenderAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
          lookupEvt = LendingEvent 
                    $ LoanResearchEvent
                    $ InspectResearchBorrowerInformation (borrowerId, loanAddress)
      vstack
        [ hstack
            [ label ("Offer For " <> showAssetBalance True reverseTickerMap loanAmount)
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
            , flip styleBasic [textSize 10] $ tooltip_ prettyLocalTime [tooltipDelay 0] $
                label clockIcon
                  `styleBasic` 
                    [ textMiddle
                    , textFont "Remix"
                    , textSize 10
                    , textColor customBlue
                    ]
            , widgetMaybe offerExpiration $ \exprTime -> hstack
                [ spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ (prettyExpirationTime $ fromPlutusTime exprTime) [tooltipDelay 0] $
                      label expirationIcon
                        `styleBasic` 
                          [ textMiddle
                          , textFont "Remix"
                          , textSize 10
                          , textColor customRed
                          ]
                ]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ tooltip_ addressTip [tooltipDelay 0] $
                box_ [alignMiddle, onClick $ CopyText $ display payToAddress] $
                  label targetAddressIcon
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
                  box_ [alignMiddle , onClick lookupEvt] $
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
            [ widgetIf collateralIsSwappable $ hstack
                [ flip styleBasic [textSize 10] $ tooltip_ swapCollateralMsg [tooltipDelay 0] $
                    label swappableCollateralIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 10
                        , textColor customBlue
                        ]
                , spacer_ [width 2]
                ]
            , label "Collateralization:"
                `styleBasic` [textSize 8, textColor lightGray]
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

offersFilterWidget :: AppModel -> AppNode
offersFilterWidget AppModel{lendingModel=LendingModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #lendingModel % #researchModel
      filterScene = researchModel ^. #offersFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #offersFilterModel % #scene) 
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
                    (toLensVL $ rootLens % #offersFilterModel % #scene) 
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
                    , button "Reset" $ LendingEvent $ LoanResearchEvent ResetResearchOffersFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showOffersFilter)
                        [onClick $ LendingEvent $ LoanResearchEvent $ UpdateLoanOfferConfiguration $ StartProcess Nothing]
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
      let rootLens = #lendingModel % #researchModel % #offersFilterModel
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
            , textField_ (toLensVL $ rootLens % #newLoanOfferConfiguration % #loanAsset) 
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
            , textField_ (toLensVL $ rootLens % #newLoanOfferConfiguration % #minDuration) 
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
            , textField_ (toLensVL $ rootLens % #newLoanOfferConfiguration % #maxDuration) 
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
            , label "Collateral Assets (separated with newlines)"
                `styleBasic` [textSize 10]
            ]
        , spacer
        , textArea (toLensVL $ rootLens % #newLoanOfferConfiguration % #collateral)
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
          possibleSortingMethods = enumFrom OfferResearchLexicographically
          rootLens = #lendingModel % #researchModel % #offersFilterModel
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
            , box_ [onClick $ Alert lenderOffersSortMsg] $
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

sorter :: OfferResearchSortMethod -> [LoanUTxO] -> [LoanUTxO]
sorter sortingMethod = 
  case sortingMethod of
    OfferResearchLexicographically -> sortOn (view #utxoRef)
    OfferResearchTime -> sortOn (view #blockTime)
    OfferResearchLoanAmount -> sortOn loanUTxOLoanAmount
    OfferResearchDuration -> sortOn loanUTxOLoanDuration
    OfferResearchInterest -> sortOn loanUTxOLoanInterest

filterer :: ReverseTickerMap -> OfferResearchFilterModel -> [LoanUTxO] -> [LoanUTxO]
filterer reverseTickerMap OfferResearchFilterModel{newLoanOfferConfiguration=NewLoanOfferConfiguration{..}} us = do
    u <- us
    let offerDatum@Loans.OfferDatum{loanTerm} = fromMaybe def $ loanUTxOOfferDatum u
        utxoCollateral = map (toNativeAsset . fst)
                       $ offerDatum ^. #collateralization % #unCollateralization
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) >= d) $ 
      readMaybe $ toString minDuration
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) <= d) $ 
      readMaybe $ toString maxDuration
    guard $ matchesAsset [toNativeAsset $ offerDatum ^. #loanAsset] loanAsset
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

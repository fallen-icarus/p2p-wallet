module P2PWallet.GUI.Widgets.Lending.Lend.ViewLoanRequests
  ( 
    viewLoanRequestsWidget
  , inspectBorrowerWidget
  , inspectLoanWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.BorrowerInformation
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

viewLoanRequestsWidget :: AppModel -> AppNode
viewLoanRequestsWidget model@AppModel{lendingModel=LendingModel{..}} = do
  let LendModel{selectedLoanAskConfiguration,newLoanAskConfiguration} = lendModel
  zstack 
    [ allAsksWidget model
        `nodeVisible` (isJust selectedLoanAskConfiguration && isNothing newLoanAskConfiguration)
    , getLoanAskConfiguration model
        `nodeVisible` (isNothing selectedLoanAskConfiguration || isJust newLoanAskConfiguration)
    , requestsFilterWidget model `nodeVisible` (lendModel ^. #showViewRequestsFilter)
    , createNewOfferWidget model `nodeVisible` isJust (lendModel ^. #newOfferCreation)
    ]

getLoanAskConfiguration :: AppModel -> AppNode
getLoanAskConfiguration model = do
  let maybeLens' = maybeLens def $ #lendingModel % #lendModel % #newLoanAskConfiguration
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
          [ button "Cancel" (LendingEvent $ LendEvent $ InitializeLoanAskConfiguration CancelAdding)
              `styleBasic` [textSize 10]
              `nodeVisible` isJust (model ^. #lendingModel % #lendModel % #selectedLoanAskConfiguration)
          , spacer
          , mainButton "Confirm" (LendingEvent $ LendEvent $ InitializeLoanAskConfiguration ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic`
        [ bgColor customGray3
        , padding 20
        , radius 20
        ]

allAsksWidget :: AppModel -> AppNode
allAsksWidget AppModel{lendingModel=LendingModel{lendModel},reverseTickerMap} = do
    cushionWidgetH $ vstack
      [ centerWidgetH $ hstack 
          [ tooltip_ "Resync Requests" [tooltipDelay 0] $
              box_ [alignMiddle, onClick $ LendingEvent $ LendEvent $ SyncLoanAsks $ StartProcess Nothing] $
                label refreshIcon
                  `styleBasic` 
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    , padding 3
                    , textSize 12
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
          , spacer_ [width 3]
          , label "Requests For Loans"
              `styleBasic` [textFont "Italics", textSize 14]
          , tooltip_ "Filter/Sort" [tooltipDelay 0] $
              toggleButton_ menuSearchIcon
                (toLensVL $ #lendingModel % #lendModel % #showViewRequestsFilter)
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
          vstack_ [childSpacing] (map askRow sample)
            `styleBasic` [padding 10]
      , filler
      ] 
  where
    LendModel{..} = lendModel

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    sample :: [LoanUTxO]
    sample = orderer (requestsFilterModel ^. #sortingDirection) 
           $ sorter (requestsFilterModel ^. #sortingMethod) 
           $ fromMaybe [] $ selectedLoanAskConfiguration >>= flip Map.lookup cachedLoanAsks

    collateralAssetWidget :: NativeAsset -> AppNode
    collateralAssetWidget asset = do
      hstack
        [ spacer_ [width 2]
        , copyableLabelSelf (showAssetNameOnly reverseTickerMap asset) lightGray 8
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

    askRow :: LoanUTxO -> AppNode
    askRow u@LoanUTxO{utxoRef,loanAddress} = do
      let Loans.AskDatum{..} = fromMaybe def $ loanUTxOAskDatum u
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          offeredCollateral = map toNativeAsset $ collateral ^. #unCollateral
          addOfferEvt = LendingEvent $ LendEvent $ CreateNewOffer $ StartAdding $ Just u
          lookupEvt = LendingEvent 
                    $ LendEvent 
                    $ InspectProspectiveBorrowerInformation (borrowerId, loanAddress)
      hstack
        [ vstack
            [ hstack
                [ label (showAssetBalance True reverseTickerMap loanAmount)
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
            , spacer_ [width 2]
            , hstack
                [ label "Offered Collateral:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , vstack_ [childSpacing_ 3] $ for (groupInto 3 offeredCollateral) $ 
                    \asset -> hstack_ [childSpacing_ 3] $ map collateralAssetWidget asset
                ]
            ] `styleBasic` 
                  [ padding 10
                  , bgColor customGray2
                  , radius 5
                  , border 1 black
                  ]
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ tooltip_ "Make Offer" [tooltipDelay 0] $
                button remixDraftLine addOfferEvt
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
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

requestsFilterWidget :: AppModel -> AppNode
requestsFilterWidget AppModel{lendingModel=LendingModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #lendingModel % #lendModel
      filterScene = lendModel ^. #requestsFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #requestsFilterModel % #scene) 
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
                    (toLensVL $ rootLens % #requestsFilterModel % #scene) 
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
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showViewRequestsFilter)
                        [onClick $ LendingEvent $ LendEvent $ UpdateLoanAskConfiguration $ StartProcess Nothing]
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
      let rootLens = #lendingModel % #lendModel % #requestsFilterModel % #newLoanAskConfiguration
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
            , textField_ (toLensVL $ rootLens % #loanAsset) 
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
            , textField_ (toLensVL $ rootLens % #minDuration) 
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
            , textField_ (toLensVL $ rootLens % #maxDuration) 
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
        , textArea (toLensVL $ rootLens % #collateral)
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
          possibleSortingMethods = enumFrom RequestsLexicographically
          rootLens = #lendingModel % #lendModel % #requestsFilterModel
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
            , box_ [onClick $ Alert openAsksSortMsg] $
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

createNewOfferWidget :: AppModel -> AppNode
createNewOfferWidget AppModel{lendingModel, knownWallets, reverseTickerMap, tickerMap} = do
  let maybeLens' = maybeLens def $ #lendingModel % #lendModel % #newOfferCreation
      NewOfferCreation{..} = fromMaybe def $ lendingModel ^. #lendModel % #newOfferCreation
      suppliedMinPayment = readMaybe @Decimal $ toString minPayment
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
      parsedLoanAmount = parseNativeAssets tickerMap mempty loanAmount
      helpButton msg = box_ [alignMiddle, onClick $ Alert msg] $
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
  vstack
    [ centerWidget $ vscroll_ [wheelRate 50] $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "New Offer"
              `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
        , spacer
        , hstack
            [ helpButton offerBorrowerIdMsg
            , spacer_ [width 3]
            , label "Borrower ID:"
                `styleBasic` [textSize 10]
            , spacer
            , copyableLabelSelf (display borrowerCredential) lightGray 10
            ]
        , spacer
        , hstack
            [ helpButton offerLoanAmountMsg
            , spacer_ [width 3]
            , label "Loan Amount:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % #loanAmount) 
                  [placeholder "5 ADA"] 
                `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ helpButton offerLoanTermMsg
            , spacer_ [width 3]
            , label "Duration:"
                `styleBasic` [textSize 10]
            , spacer
            , numericField_ (toLensVL $ maybeLens' % #loanTerm) [M.decimals 0]
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , label "Day(s)"
                `styleBasic` [textColor lightGray, textMiddle, textSize 10]
            ]
        , spacer
        , hstack
            [ helpButton offerPaymentAddressMsg
            , spacer_ [width 3]
            , label "Payment Address:"
                `styleBasic` [textSize 10]
            , spacer
            , textDropdown_ 
                  (toLensVL $ maybeLens' % #paymentWallet) 
                  (knownWallets ^. #paymentWallets) 
                  (view #alias) -- The dropdown displays the wallet's alias in the menu.
                  [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                `styleBasic` 
                  [ bgColor customGray2
                  , width 150
                  , border 1 black
                  , textSize 10
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            ]
        , spacer
        , hstack
            [ helpButton offerInterestMsg
            , spacer_ [width 3]
            , label "Interest:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % #interest) 
                  [placeholder "10"] 
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , label "%"
                `styleBasic` [textColor lightGray, textMiddle, textFont "Bold", textSize 14]
            ]
        , spacer
        , hstack
            [ helpButton offerCompoundFrequencyMsg
            , spacer_ [width 3]
            , label "Compound Frequency:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % #compoundFrequency) 
                  [placeholder "5"] 
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , label "Day(s)"
                `styleBasic` [textColor lightGray, textMiddle, textSize 10]
            ]
        , widgetIf (compoundFrequency /= "") $ vstack
            [ spacer
            , hstack
                [ helpButton offerMinimumPaymentMsg
                , spacer_ [width 3]
                , label "Minimum payment:"
                    `styleBasic` [textSize 10]
                , spacer
                , textField_ (toLensVL $ maybeLens' % #minPayment) 
                      [placeholder "10.0"] 
                    `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
                , spacer_ [width 3]
                , label (either (const "") (showAssetNameOnly reverseTickerMap) parsedLoanAmount)
                    `styleBasic` [textColor lightGray, textMiddle, textSize 10]
                ]
            ]
        , widgetIf (suppliedMinPayment > Just 0) $ vstack
            [ spacer
            , hstack
                [ helpButton offerPenaltyMsg
                , spacer_ [width 3]
                , label "Penalty:"
                    `styleBasic` [textSize 10]
                ]
            , spacer_ [width 5]
            , hstack
                [ spacer
                , textDropdown_ (toLensVL $ maybeLens' % #penalty % _1) (enumFrom NoNewPenalty) display
                    [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                    `styleBasic` [textSize 10, width 120, bgColor customGray1]
                    `styleFocus` [border 1 customBlue]
                , widgetIf (penalty ^. _1 == NewFixedPenalty) $ hstack
                    [ spacer
                    , textField_ (toLensVL $ maybeLens' % #penalty % _2) 
                          [placeholder "10.0"] 
                        `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                        `styleFocus` [border 1 customBlue]
                    , spacer_ [width 3]
                    , label (either (const "") (showAssetNameOnly reverseTickerMap) parsedLoanAmount)
                        `styleBasic` [textColor lightGray, textMiddle, textSize 10]
                    ]
                , widgetIf (penalty ^. _1 == NewPercentPenalty) $ hstack
                    [ spacer
                    , textField_ (toLensVL $ maybeLens' % #penalty % _2) 
                          [placeholder "2"] 
                        `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                        `styleFocus` [border 1 customBlue]
                    , spacer_ [width 3]
                    , label "%"
                        `styleBasic` [textColor lightGray, textMiddle, textFont "Bold", textSize 14]
                    ]
                ]
            ]
        , spacer
        , hstack
            [ helpButton offerCollateralIsSwappableMsg
            , spacer_ [width 3]
            , label "Collateral Can Be Swapped Out:"
                `styleBasic` [textSize 10]
            , spacer
            , checkbox_ (toLensVL $ maybeLens' % #collateralIsSwappable) [checkboxSquare]
                `styleBasic` [fgColor customGray1, hlColor customBlue]
            ]
        , spacer
        , hstack
            [ helpButton offerClaimPeriodMsg
            , spacer_ [width 3]
            , label "Claim Period:"
                `styleBasic` [textSize 10]
            , spacer
            , numericField_ (toLensVL $ maybeLens' % #claimPeriod) [M.decimals 0]
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , label "Day(s)"
                `styleBasic` [textColor lightGray, textMiddle, textSize 10]
            ]
        , spacer
        , hstack
            [ helpButton offerExpirationMsg
            , spacer_ [width 3]
            , label "Offer Expiration:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % #offerExpiration)
                  [placeholder "5"] 
                `styleBasic` [textSize 10, width 50, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , spacer_ [width 3]
            , label "Day(s)"
                `styleBasic` [textColor lightGray, textMiddle, textSize 10]
            ]
        , spacer
        , hstack
            [ helpButton offerCollateralMsg
            , spacer_ [width 3]
            , label "Collateral Values (separated with newlines)"
                `styleBasic` [textSize 10]
            ]
        , spacer
        , textArea (toLensVL $ maybeLens' % #collateralization)
            `styleBasic` [height 100, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer
        , box_ [alignRight] $ 
            hstack
              [ button "Cancel" (LendingEvent $ LendEvent $ CreateNewOffer CancelAdding)
                  `styleBasic` [textSize 10]
              , spacer
              , mainButton "Confirm" (LendingEvent $ LendEvent $ CreateNewOffer ConfirmAdding)
                  `styleBasic` [textSize 10]
              ]
        ] `styleBasic`
            [ bgColor customGray3
            , padding 20
            , radius 20
            ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , paddingT 50
        , paddingB 50
        , paddingL 30
        , paddingR 30
        , radius 10
        ]

inspectBorrowerWidget :: AppModel -> AppNode
inspectBorrowerWidget model@AppModel{lendingModel=LendingModel{..}} = do
    vstack
      [ vstack
          [ centerWidgetH $
              label "Information For Borrower ID"
                `styleBasic` [textFont "Italics", textColor customBlue]
          , spacer
          , centerWidgetH $ hstack
              [ copyableLabelSelf (display target) lightGray 12
              , spacer_ [width 3]
              , tooltip_ "Resync Information" [tooltipDelay 0] $
                  box_ [alignMiddle, onClick $ LendingEvent $ LookupBorrowerInformation $ StartProcess $ Just info] $
                    label refreshIcon
                      `styleBasic` 
                        [ border 0 transparent
                        , radius 20
                        , bgColor transparent
                        , textColor customBlue
                        , textMiddle
                        , textFont "Remix"
                        , padding 3
                        , textSize 12
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
          , spacer_ [width 3]
              ]
          , spacer
          , vscroll_ [wheelRate 50] $ vstack
              [ creditHistoryField model
              , spacer
              , openAsksField model
              , spacer
              , currentOffersField model
              , spacer
              , activeLoansField model
              ]
          , filler
          , hstack
              [ filler
              , button "Close" $ LendingEvent $ 
                  LendEvent CloseInspectedProspectiveBorrowerInformation
              ]
          ] `styleBasic`
              [ bgColor customGray3
              , padding 30
              , radius 10
              ]
      ] `styleBasic` 
          [ bgColor $ black & #a .~ 0.4
          , padding 30
          , radius 10
          ]
  where
    info@(target,_) = fromMaybe ("","") $ lendModel ^. #inspectedBorrower

moreOffStyle :: Style
moreOffStyle = 
  def `styleBasic` 
        [ bgColor black
        , textColor customBlue
        , radius 20
        , paddingT 2
        , paddingB 2
        , paddingR 5
        , paddingL 5
        ]
      `styleHover`
        [ bgColor customGray1]

creditHistoryField :: AppModel -> AppNode
creditHistoryField AppModel{..} = do
    vstack
      [ hstack
          [ label ("Credit Score: " <> creditScore)
              `styleBasic` [textSize 12]
          , spacer_ [width 2]
          , box_ [onClick $ Alert creditScoreMsg] $
              label helpIcon
                `styleBasic`
                  [ padding 2
                  , textSize 8
                  , border 0 transparent
                  , radius 20
                  , textMiddle
                  , bgColor transparent
                  , textColor customBlue
                  , textFont "Remix"
                  ]
                `styleHover` [bgColor customGray1, cursorIcon CursorHand]
          , spacer_ [width 5]
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #lendingModel % #cachedBorrowerInfo % at target % toggleShow #showCreditHistory)
              [toggleButtonOffStyle moreOffStyle]
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , radius 20
                , paddingT 2
                , paddingB 2
                , paddingR 5
                , paddingL 5
                , bgColor black
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              `nodeVisible` (creditHistory /= [])
          ]
      , widgetIf showCreditHistory $
          vstack_ [childSpacing] (map resultRow creditHistory)
            `styleBasic` [padding 10]
      ]
  where
    target :: Loans.BorrowerId
    target = maybe "" fst $ lendingModel ^. #lendModel % #inspectedBorrower

    BorrowerInformation{..} = fromMaybe def 
                            $ Map.lookup target (lendingModel ^. #cachedBorrowerInfo)

    allSuccesses :: [LoanResult]
    allSuccesses = filter (not . view #isDefault) creditHistory

    creditScore :: Text
    creditScore 
      | null creditHistory = "undefined"
      | otherwise = displayPercentage 
                  $ toRational (length allSuccesses) / toRational (length creditHistory)

    lockedCollateralWidget :: NativeAsset -> AppNode
    lockedCollateralWidget collateralAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap collateralAsset)
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

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
            , radius 3
            , border 1 customGray1
            ]

    resultRow :: LoanResult -> AppNode
    resultRow LoanResult{..} = do
      let Loans.ActiveDatum{borrowerId=_,..} = terms
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          unpaidBalance
            | isDefault = loanAmount & #quantity .~ roundUp (toRational loanOutstanding)
            | otherwise = loanAmount & #quantity .~ 0
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                           $ collateralization ^. #unCollateralization
          allAssets = 
            (lovelaceAsNativeAsset & #quantity .~ unLovelace remainingLovelace) : remainingNativeAssets
          lockedCollateral = 
            filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets
          prettyUnpaidBalance = unwords
            [ "Unpaid Balance:"
            , showAssetBalance True reverseTickerMap unpaidBalance
            ]
          prettyInterest = unwords
            [ "Interest:"
            , displayPercentage (toRational loanInterest) <> "%"
            ]
          prettyCompounding = flip (maybe "Non-Compounding") compoundFrequency $ \freq ->
            unwords
              [ "Compounding Every"
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
          defaultMsg
            | isDefault = "Default"
            | otherwise = "Success"
          swapCollateralMsg = "Collateral could be swapped out for other approved collateral"
          loanHistoryEvt = LendingEvent $ LendEvent $ InspectTargetLoanHistory loanId
      vstack
        [ hstack
            [ label defaultMsg
                `styleBasic` 
                  [ textSize 10
                  , textColor $ if isDefault then customRed else customBlue
                  ]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Loan ID: " <> display loanId) [tooltipDelay 0] $
                  box_ [alignMiddle , onClick loanHistoryEvt] $
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
            , label prettyUnpaidBalance
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 3]
        , hstack
            [ label ("Loan Principal: " <> showAssetBalance True reverseTickerMap loanAmount)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Duration: " <> show duration <> " Day(s)")
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , spacer_ [width 3]
        , hstack
            [ label prettyInterest
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyCompounding
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , widgetIf (isJust compoundFrequency) $ vstack
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
        , widgetIf isDefault $ vstack
            [ spacer_ [width 1]
            , hstack
                [ label "Confiscated Collateral:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 3]
                , vstack_ [childSpacing_ 3] $ for (groupInto 3 lockedCollateral) $ 
                    \col -> hstack_ [childSpacing_ 3] $ map lockedCollateralWidget col
                ]
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

openAsksField :: AppModel -> AppNode
openAsksField AppModel{..} = do
    vstack
      [ hstack
          [ label "Current Loan Requests"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #lendingModel % #cachedBorrowerInfo % at target % toggleShow #showOpenAsks)
              [toggleButtonOffStyle moreOffStyle]
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , radius 20
                , paddingT 2
                , paddingB 2
                , paddingR 5
                , paddingL 5
                , bgColor black
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              `nodeVisible` (creditHistory /= [])
          ]
      , widgetIf showOpenAsks $
          vstack_ [childSpacing] (map askRow openAsks)
            `styleBasic` [padding 10]
      ]
  where
    target :: Loans.BorrowerId
    target = maybe "" fst $ lendingModel ^. #lendModel % #inspectedBorrower

    BorrowerInformation{..} = fromMaybe def 
                            $ Map.lookup target (lendingModel ^. #cachedBorrowerInfo)

    askRow :: LoanUTxO -> AppNode
    askRow u@LoanUTxO{utxoRef,blockTime} = do
      let Loans.AskDatum{borrowerId=_,..} = fromMaybe def $ loanUTxOAskDatum u
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          offeredCollateral = map toNativeAsset $ collateral ^. #unCollateral
          prettyLocalTime = unwords
            [ showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
      vstack
        [ hstack
            [ label ("Borrow " <> showAssetBalance True reverseTickerMap loanAmount)
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
            , filler
            , label "Duration:"
                `styleBasic` [textSize 10, textColor white]
            , spacer_ [width 3]
            , label (show duration <> " Days")
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , hstack
            [ label "Offered Collateral:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 3]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 offeredCollateral) $ 
                \asset -> hstack_ [childSpacing_ 3] $ map collateralAssetWidget asset
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

    collateralAssetWidget :: NativeAsset -> AppNode
    collateralAssetWidget asset = do
      hstack
        [ spacer_ [width 2]
        , copyableLabelSelf (showAssetNameOnly reverseTickerMap asset) lightGray 8
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

currentOffersField :: AppModel -> AppNode
currentOffersField AppModel{..} = do
    vstack
      [ hstack
          [ label "Current Offers"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #lendingModel % #cachedBorrowerInfo % at target % toggleShow #showCurrentOffers)
              [toggleButtonOffStyle moreOffStyle]
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , radius 20
                , paddingT 2
                , paddingB 2
                , paddingR 5
                , paddingL 5
                , bgColor black
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              `nodeVisible` (creditHistory /= [])
          ]
      , widgetIf showCurrentOffers $
          vstack_ [childSpacing] (map offerRow currentOffers)
            `styleBasic` [padding 10]
      ]
  where
    target :: Loans.BorrowerId
    target = maybe "" fst $ lendingModel ^. #lendModel % #inspectedBorrower

    BorrowerInformation{..} = fromMaybe def 
                            $ Map.lookup target (lendingModel ^. #cachedBorrowerInfo)

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
    offerRow u@LoanUTxO{utxoRef,blockTime} = do
      let Loans.OfferDatum{..} = fromMaybe def $ loanUTxOOfferDatum u
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                           $ collateralization ^. #unCollateralization
          prettyInterest = unwords
            [ "Interest:"
            , displayPercentage (toRational loanInterest) <> "%"
            ]
          prettyCompounding = flip (maybe "Non-Compounding") compoundFrequency $ \freq ->
            unwords
              [ "Compounding Every"
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
            , label prettyCompounding
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , widgetIf (isJust compoundFrequency) $ vstack
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

activeLoansField :: AppModel -> AppNode
activeLoansField AppModel{..} = do
    vstack
      [ hstack
          [ label "Active Loans"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #lendingModel % #cachedBorrowerInfo % at target % toggleShow #showActiveLoans)
              [toggleButtonOffStyle moreOffStyle]
              `styleBasic` 
                [ textSize 10
                , textColor customRed
                , textFont "Remix"
                , textMiddle
                , radius 20
                , paddingT 2
                , paddingB 2
                , paddingR 5
                , paddingL 5
                , bgColor black
                , border 0 transparent
                ]
              `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              `nodeVisible` (creditHistory /= [])
          ]
      , widgetIf showActiveLoans $
          vstack_ [childSpacing] (map activeRow activeLoans)
            `styleBasic` [padding 10]
      ]
  where
    target :: Loans.BorrowerId
    target = maybe "" fst $ lendingModel ^. #lendModel % #inspectedBorrower

    BorrowerInformation{..} = fromMaybe def 
                            $ Map.lookup target (lendingModel ^. #cachedBorrowerInfo)

    lockedCollateralWidget :: NativeAsset -> AppNode
    lockedCollateralWidget collateralAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap collateralAsset)
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
    activeRow u@LoanUTxO{utxoRef,lovelace=utxoLovelace,nativeAssets=utxoNativeAssets} = do
      let Loans.ActiveDatum{borrowerId=_,..} = fromMaybe def $ loanUTxOActiveDatum u
          loanBalance = toNativeAsset loanAsset & #quantity .~ roundUp (toRational loanOutstanding)
          expiration = fromPlutusTime loanExpiration
          mNextCompounding = (+lastCompounding) <$> compoundFrequency
          nextPaymentDueDate = case mNextCompounding of
            Nothing -> expiration
            Just nextCompounding -> min (fromPlutusTime nextCompounding) expiration
          amountDue = minPayment - totalEpochPayments
          nextPaymentSize
            | minPayment == 0 = loanBalance
            | amountDue < 0 = loanBalance & #quantity .~ 0
            | otherwise = loanBalance & #quantity .~ amountDue
          prettyExpirationTime = unwords
            [ "Expires:"
            , showLocalDate (config ^. #timeZone) expiration
            , showLocalTime (config ^. #timeZone) expiration
            ]
          prettyNextPaymentDueDate = unwords
            [ "Next Deadline:"
            , ""
            , showLocalDate (config ^. #timeZone) nextPaymentDueDate
            , showLocalTime (config ^. #timeZone) nextPaymentDueDate
            ]
          prettyInterest = unwords
            [ "Interest:"
            , displayPercentage (toRational loanInterest) <> "%"
            ]
          prettyNextPayment = unwords
            [ "Amount Required by Deadline:"
            , ""
            , showAssetBalance True reverseTickerMap nextPaymentSize
            ]
          prettyCompounding = flip (maybe "Non-Compounding") compoundFrequency $ \freq ->
            unwords
              [ "Compounding Every"
              , show (calcDaysInPosixPeriod $ fromPlutusTime freq)
              , "Day(s)"
              ]
          prettyPenalty = case penalty of
            Loans.NoPenalty -> "No Penalty"
            Loans.FixedFee fee -> unwords
              [ "Fee Penalty:"
              , showAssetBalance True reverseTickerMap $ loanBalance & #quantity .~ fee
              ]
            Loans.PercentFee percent -> unwords
              [ "Percent Penalty:"
              , displayPercentage (toRational percent) <> "%"
              ]
          swapCollateralMsg = "Collateral can be swapped out for other approved collateral"
          allAssets = 
            (lovelaceAsNativeAsset & #quantity .~ unLovelace utxoLovelace) : utxoNativeAssets
          lockedCollateral = 
            filter ((/= Loans.activeBeaconCurrencySymbol) . view #policyId) allAssets
          loanHistoryEvt = LendingEvent $ LendEvent $ InspectTargetLoanHistory loanId
      vstack
        [ hstack
            [ label ("Balance: " <> showAssetBalance True reverseTickerMap loanBalance)
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
                tooltip_ prettyExpirationTime [tooltipDelay 0] $
                  label expirationIcon
                    `styleBasic` 
                      [ textMiddle
                      , textFont "Remix"
                      , textSize 10
                      , textColor customRed
                          ]
            , spacer_ [width 5]
            , flip styleBasic [textSize 10] $ 
                tooltip_ ("Loan ID: " <> display loanId) [tooltipDelay 0] $
                  box_ [alignMiddle , onClick loanHistoryEvt] $
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
            , label prettyNextPaymentDueDate
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 3]
        , hstack
            [ label prettyInterest
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label prettyNextPayment
                `styleBasic` [textSize 8, textColor lightGray]
            ]
        , widgetIf (isJust compoundFrequency) $ vstack
            [ spacer_ [width 3]
            , hstack
                [ label prettyCompounding
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
            , label "Locked Collateral:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 3]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 lockedCollateral) $ 
                \col -> hstack_ [childSpacing_ 3] $ map lockedCollateralWidget col
            ]
        ] `styleBasic` 
              [ padding 10
              , bgColor customGray2
              , radius 5
              , border 1 black
              ]

inspectLoanWidget :: AppModel -> AppNode
inspectLoanWidget AppModel{lendingModel=LendingModel{..},scene=_,..} = do
    vstack
      [ vstack
          [ centerWidgetH $
              label "Event History For Loan ID"
                `styleBasic` [textFont "Italics", textColor customBlue]
          , spacer
          , centerWidgetH $ hstack
              [ copyableLabelSelf (display targetId) lightGray 12
              , spacer_ [width 3]
              , tooltip_ "Resync History" [tooltipDelay 0] $
                  box_ [alignMiddle, onClick $ LendingEvent $ LookupLoanHistory $ StartProcess $ Just targetId] $
                    label refreshIcon
                      `styleBasic` 
                        [ border 0 transparent
                        , radius 20
                        , bgColor transparent
                        , textColor customBlue
                        , textMiddle
                        , textFont "Remix"
                        , padding 3
                        , textSize 12
                        ]
                      `styleHover` [bgColor customGray1, cursorIcon CursorHand]
              ]
          , spacer
          , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map eventRow history)
          , filler
          , hstack
              [ filler
              , button "Close" $ LendingEvent $ LendEvent CloseInspectedTargetLoanHistory
              ]
          ] `styleBasic`
              [ bgColor customGray3
              , padding 30
              , radius 10
              ]
      ] `styleBasic` 
          [ bgColor $ black & #a .~ 0.4
          , padding 30
          , radius 10
          ]
  where
    targetId :: Loans.LoanId
    targetId = fromMaybe "" $ lendModel ^. #inspectedLoan

    history :: [LoanEvent]
    history = fromMaybe [] $ Map.lookup targetId cachedLoanHistories

    explainEvent :: LoanEvent -> Text
    explainEvent LoanEvent{state=eventState, event, timeStamp} =
      let Loans.ActiveDatum{loanAsset, claimExpiration} = fromMaybe def eventState 
          loanNativeAsset = toNativeAsset loanAsset
       in case event of
            Left (Loans.CreateActive _) -> "Loan started."
            Right (Loans.MakePayment amount) -> mconcat
              [ "Made payment of "
              , showAssetBalance True reverseTickerMap $ loanNativeAsset & #quantity .~ amount
              , "."
              ]
            Right (Loans.ApplyInterest deposit times) -> mconcat
              [ "Applied interest "
              , show times
              , " time(s) with deposit increase of "
              , display $ Lovelace deposit
              , "."
              ]
            Right Loans.SpendWithKeyNFT -> "Defaulted collateral claimed by lender."
            Right (Loans.UpdateLenderAddress newAddress deposit) -> mconcat
              [ "Lender changed the required payment address to "
              , display $ fromRight "" $ 
                  fmap fst $ plutusToBech32 (config ^. #network) newAddress
              , " with deposit increase of "
              , display $ Lovelace deposit
              , "."
              ]
            Right Loans.Unlock -> 
              if claimExpiration < toPlutusTime timeStamp 
              then "Lost collateral claimed by borrower."
              else "Invalid Active UTxO closed by borrower."
            _ -> error "Other loan redeemer used."

    eventRow :: LoanEvent -> AppNode
    eventRow e@LoanEvent{timeStamp} = do
      vstack
        [ hstack
            [ label (explainEvent e)
                `styleBasic` [textSize 10]
            , filler
            ]
        , spacer_ [width 2]
        , hstack
            [ label calendarIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalDate (config ^. #timeZone) timeStamp)
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
            , label (showLocalTime (config ^. #timeZone) timeStamp)
                `styleBasic` 
                  [ textSize 10
                  , textColor lightGray
                  ]
            , filler
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
orderer :: SortDirection -> [LoanUTxO] -> [LoanUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: RequestsSortMethod -> [LoanUTxO] -> [LoanUTxO]
sorter sortingMethod = 
  case sortingMethod of
    RequestsLexicographically -> sortOn (view #utxoRef)
    RequestsTime -> sortOn (view #blockTime)
    RequestsLoanAmount -> sortOn loanUTxOLoanAmount
    RequestsDuration -> sortOn loanUTxOLoanDuration

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

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
-- | A lens to toggle the `show` field of the `Transaction`.
toggleShow :: Lens' BorrowerInformation Bool -> Lens' (Maybe BorrowerInformation) Bool
toggleShow finalLens = lens getToggleShow setToggleShow
  where
    getToggleShow :: Maybe BorrowerInformation -> Bool
    getToggleShow = maybe False (view finalLens)

    setToggleShow :: Maybe BorrowerInformation -> Bool -> Maybe BorrowerInformation
    setToggleShow maybeInfo b = fmap (set finalLens b) maybeInfo

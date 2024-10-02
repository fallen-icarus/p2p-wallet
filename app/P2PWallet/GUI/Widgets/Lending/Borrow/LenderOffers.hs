module P2PWallet.GUI.Widgets.Lending.Borrow.LenderOffers
  ( lenderOffersWidget
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

lenderOffersWidget :: AppModel -> AppNode
lenderOffersWidget model@AppModel{lendingModel=LendingModel{..},reverseTickerMap,config} =
    zstack
      [ mainWidget
      , offersFilterWidget model `nodeVisible` (borrowModel ^. #showLenderOffersFilter)
      , chooseAskWidget model `nodeVisible` and
          [ isJust $ borrowModel ^. #newOfferAcceptance
          , borrowModel ^. #offerAcceptanceScene == ChooseAskScene
          ]
      , specifyCollateralWidget model `nodeVisible` and
          [ isJust $ borrowModel ^. #newOfferAcceptance
          , borrowModel ^. #offerAcceptanceScene == SpecifyCollateralScene
          ]
      ]
  where
    Config{currentTime} = config

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    LoanWallet{..} = selectedWallet

    allOffers :: [LoanUTxO]
    allOffers = filterOutExpired currentTime
              $ filter ((==Just True) . fmap (is _OfferDatum) . view #loanDatum) utxos

    fractionShown :: Text
    fractionShown = 
      show (length sample) <> "/" <> show (length allOffers)

    sample :: [LoanUTxO]
    sample = orderer (borrowModel ^. #lenderOffersFilterModel % #sortingDirection) 
           . sorter (borrowModel ^. #lenderOffersFilterModel % #sortingMethod) 
           . filterer reverseTickerMap (borrowModel ^. #lenderOffersFilterModel) 
           $ allOffers

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ hstack 
            [ filler
            , label ("Offers (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #lendingModel % #borrowModel % #showLenderOffersFilter)
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
          acceptEvt = LendingEvent $ BorrowEvent $ AcceptLoanOffer $ ChooseOfferToAccept u
      hstack
        [ vstack
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
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Accept Offer" [tooltipDelay 0] $
                button acceptIcon acceptEvt
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

chooseAskWidget :: AppModel -> AppNode
chooseAskWidget AppModel{..} = do
    vstack
      [ centerWidget $ vstack
          [ centerWidgetH $
              label "Choose an Ask to Close"
                `styleBasic` [textFont "Italics", textColor customBlue]
          , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map askRow allAsks)
                `styleBasic` [padding 10]
          , filler
          , box_ [alignRight] $ 
              button "Cancel" (LendingEvent $ BorrowEvent $ AcceptLoanOffer CancelAcceptance)
                `styleBasic` [textSize 10]
          ] `styleBasic`
              [ bgColor customGray3
              , radius 10
              , border 1 black
              , padding 30
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
    LoanWallet{..} = lendingModel ^. #selectedWallet

    collateralAssetWidget :: NativeAsset -> AppNode
    collateralAssetWidget asset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetNameOnly reverseTickerMap asset) 
            `styleBasic` [textColor lightGray, textSize 8]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , radius 3
            , border 1 customGray1
            ]

    usedAsks = map (view $ _2 % #askUTxO % #utxoRef) 
             $ txBuilderModel ^. #loanBuilderModel % #offerAcceptances

    allAsks :: [LoanUTxO]
    allAsks = utxos
      & filter (\x -> isJust (loanUTxOAskDatum x) && (x ^. #utxoRef) `notElem` usedAsks)
      & sortOn (view #blockTime) 

    askRow :: LoanUTxO -> AppNode
    askRow u = do
      let Loans.AskDatum{..} = fromMaybe def $ loanUTxOAskDatum u
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
          offeredCollateral = map toNativeAsset $ collateral ^. #unCollateral
          chooseEvt = LendingEvent $ BorrowEvent $ AcceptLoanOffer $ ChooseAskToClose u
      box_ [onClick chooseEvt] $ vstack
        [ hstack
            [ label ("Borrow " <> showAssetBalance True reverseTickerMap loanAmount)
                `styleBasic` [textSize 10, textColor customBlue]
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
          `styleHover`
            [ border 1 customBlue
            , cursorIcon CursorHand
            ]

specifyCollateralWidget :: AppModel -> AppNode
specifyCollateralWidget AppModel{..} = do
    let maybeLens' = maybeLens def $ #lendingModel % #borrowModel % #newOfferAcceptance
        NewOfferAcceptance{offerUTxO} = fromMaybe def $ 
          lendingModel ^. #borrowModel % #newOfferAcceptance
        Loans.OfferDatum{loanAsset,collateralization,loanPrincipal} = fromMaybe def $
          loanUTxOOfferDatum offerUTxO
        loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
        collateralPrices = map (over _1 toNativeAsset . over _2 toRational) 
                         $ collateralization ^. #unCollateralization
    vstack
      [ centerWidget $ vstack
          [ centerWidgetH $
              label "Specify your collateral"
                `styleBasic` [textFont "Italics", textColor customBlue]
          , label ("Loan Amount: " <> showAssetBalance True reverseTickerMap loanAmount)
              `styleBasic` [textSize 12]
          , spacer
          , label "Collateral Rates:"
              `styleBasic` [textSize 12]
          , spacer_ [width 3]
          , vstack_ [childSpacing_ 3] $ for (groupInto 3 collateralPrices) $ 
              \col -> hstack_ [childSpacing_ 3] $ map (collateralAssetWidget loanAmount) col
          , hstack
              [ label "Collateral Assets (separated with newlines)"
                  `styleBasic` [textSize 12]
              , mainButton helpIcon (Alert collateralAmountsMsg)
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
          , textArea (toLensVL $ maybeLens' % #collateralAmounts)
              `styleBasic` [height 180, textSize 10, bgColor customGray1]
              `styleFocus` [border 1 customBlue]
          , spacer
          , box_ [alignRight] $ 
              hstack
                [ button "Cancel" (LendingEvent $ BorrowEvent $ AcceptLoanOffer CancelAcceptance)
                    `styleBasic` [textSize 10]
                , spacer
                , mainButton "Confirm" (LendingEvent $ BorrowEvent $ AcceptLoanOffer ProcessAcceptance)
                    `styleBasic` [textSize 10]
                ]
          , spacer
          ] `styleBasic`
              [ bgColor customGray3
              , radius 10
              , border 1 black
              , padding 30
              , paddingB 0
              ]
      ] `styleBasic` 
          [ bgColor $ black & #a .~ 0.4
          , padding 20
          , radius 10
          ]
  where
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
            `styleBasic` [textSize 12, textColor lightGray]
        , spacer_ [width 2]
        ] `styleBasic` 
            [ bgColor customGray4
            , padding 2
            , paddingT 1
            , paddingT 1
            , radius 3
            , border 1 customGray1
            ]

offersFilterWidget :: AppModel -> AppNode
offersFilterWidget AppModel{lendingModel=LendingModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #lendingModel % #borrowModel
      filterScene = borrowModel ^. #lenderOffersFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #lenderOffersFilterModel % #scene) 
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
                    (toLensVL $ rootLens % #lenderOffersFilterModel % #scene) 
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
                    , button "Reset" $ LendingEvent $ BorrowEvent ResetLenderOffersFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showLenderOffersFilter)
                        [onClick $ LendingEvent $ BorrowEvent CheckLenderOffersFilterModel]
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
      let rootLens = #lendingModel % #borrowModel % #lenderOffersFilterModel
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
          possibleSortingMethods = enumFrom LenderOffersLexicographically
          rootLens = #lendingModel % #borrowModel % #lenderOffersFilterModel
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

sorter :: LenderOffersSortMethod -> [LoanUTxO] -> [LoanUTxO]
sorter sortingMethod = 
  case sortingMethod of
    LenderOffersLexicographically -> sortOn (view #utxoRef)
    LenderOffersTime -> sortOn (view #blockTime)
    LenderOffersLoanAmount -> sortOn loanUTxOLoanAmount
    LenderOffersDuration -> sortOn loanUTxOLoanDuration
    LenderOffersInterest -> sortOn loanUTxOLoanInterest

filterer :: ReverseTickerMap -> LenderOffersFilterModel -> [LoanUTxO] -> [LoanUTxO]
filterer reverseTickerMap LenderOffersFilterModel{..} us = do
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

filterOutExpired :: POSIXTime -> [LoanUTxO] -> [LoanUTxO]
filterOutExpired currentTime us = do
    u <- us
    let Loans.OfferDatum{offerExpiration} = fromMaybe def $ loanUTxOOfferDatum u
    guard $ case offerExpiration of
      Nothing -> True
      Just expir -> toPlutusTime currentTime < expir
    return u

module P2PWallet.GUI.Widgets.Lending.Lend.OpenOffers
  ( openOffersWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map qualified as Map

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

openOffersWidget :: AppModel -> AppNode
openOffersWidget model@AppModel{knownWallets,lendingModel=LendingModel{..},reverseTickerMap,config} =
    zstack
      [ mainWidget
      , updateOfferWidget model `nodeVisible` isJust (lendModel ^. #newOfferUpdate)
      , offersFilterWidget model `nodeVisible` (lendModel ^. #showOpenOffersFilter)
      ]
  where
    Config{currentTime} = config

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    LoanWallet{..} = selectedWallet

    allOffers :: [LoanUTxO]
    allOffers = filter ((==Just True) . fmap (is _OfferDatum) . view #loanDatum) offerUTxOs

    fractionShown :: Text
    fractionShown = 
      show (length sample) <> "/" <> show (length allOffers)

    sample :: [LoanUTxO]
    sample = orderer (lendModel ^. #openOffersFilterModel % #sortingDirection) 
           $ sorter (lendModel ^. #openOffersFilterModel % #sortingMethod) 
           $ filterer currentTime reverseTickerMap (lendModel ^. #openOffersFilterModel) allOffers

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ hstack 
            [ filler
            , label ("Offers (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #lendingModel % #lendModel % #showOpenOffersFilter)
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
          payToAddress = either (const "error") fst $ plutusToBech32 network lenderAddress
          mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                        $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
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
                      box_ [alignMiddle , onClick AppInit] $
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
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon (LendingEvent $ LendEvent $ AddSelectedOfferUpdate $ StartAdding $ Just u)
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
            , spacer_ [width 2]
            , separatorLine `styleBasic` [fgColor darkGray, paddingL 5, paddingR 5]
            , spacer_ [width 2]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Close" [tooltipDelay 0] $
                button closeCircleIcon (LendingEvent $ LendEvent $ AddSelectedOfferClose u)
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
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

offersFilterWidget :: AppModel -> AppNode
offersFilterWidget AppModel{lendingModel=LendingModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #lendingModel % #lendModel
      filterScene = lendModel ^. #openOffersFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #openOffersFilterModel % #scene) 
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
                    (toLensVL $ rootLens % #openOffersFilterModel % #scene) 
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
                    , button "Reset" $ LendingEvent $ LendEvent ResetOpenOffersFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showOpenOffersFilter)
                        [onClick $ LendingEvent $ LendEvent CheckOpenOffersFilterModel]
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
      let rootLens = #lendingModel % #lendModel % #openOffersFilterModel
          offStyle = def 
            `styleBasic` [ bgColor customGray1 , textColor white ]
            `styleHover` [ bgColor customBlue ]
          choiceButton caption field targetLens =
            box_ [alignMiddle] $ optionButton_ caption field targetLens
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
                , textSize 10
                ]
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
            , filler
            , label "Expired Offer:"
                `styleBasic` [textSize 10]
            , spacer_ [width 2]
            , choiceButton "Yes" (Just True) (toLensVL $ rootLens % #shouldBeExpired)
            , choiceButton "No" (Just False) (toLensVL $ rootLens % #shouldBeExpired)
            , choiceButton "Either" Nothing (toLensVL $ rootLens % #shouldBeExpired)
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
          possibleSortingMethods = enumFrom OpenOffersLexicographically
          rootLens = #lendingModel % #lendModel % #openOffersFilterModel
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

updateOfferWidget :: AppModel -> AppNode
updateOfferWidget AppModel{lendingModel, knownWallets, reverseTickerMap, tickerMap} = do
  let maybeLens' = maybeLens (def,def) $ #lendingModel % #lendModel % #newOfferUpdate
      NewOfferCreation{..} = maybe def snd $ lendingModel ^. #lendModel % #newOfferUpdate
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
            label "Update Offer"
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
            , textField_ (toLensVL $ maybeLens' % _2 % #loanAmount) 
                  [placeholder "5"] 
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
            , numericField_ (toLensVL $ maybeLens' % _2 % #loanTerm) [M.decimals 0]
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
                  (toLensVL $ maybeLens' % _2 % #paymentWallet) 
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
            , textField_ (toLensVL $ maybeLens' % _2 % #interest) 
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
            , textField_ (toLensVL $ maybeLens' % _2 % #compoundFrequency) 
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
                , textField_ (toLensVL $ maybeLens' % _2 % #minPayment) 
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
                , textDropdown_ (toLensVL $ maybeLens' % _2 % #penalty % _1) (enumFrom NoNewPenalty) display
                    [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
                    `styleBasic` [textSize 10, width 120, bgColor customGray1]
                    `styleFocus` [border 1 customBlue]
                , widgetIf (penalty ^. _1 == NewFixedPenalty) $ hstack
                    [ spacer
                    , textField_ (toLensVL $ maybeLens' % _2 % #penalty % _2) 
                          [placeholder "10.0"] 
                        `styleBasic` [textSize 10, width 150, bgColor customGray1, sndColor darkGray]
                        `styleFocus` [border 1 customBlue]
                    , spacer_ [width 3]
                    , label (either (const "") (showAssetNameOnly reverseTickerMap) parsedLoanAmount)
                        `styleBasic` [textColor lightGray, textMiddle, textSize 10]
                    ]
                , widgetIf (penalty ^. _1 == NewPercentPenalty) $ hstack
                    [ spacer
                    , textField_ (toLensVL $ maybeLens' % _2 % #penalty % _2) 
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
            , checkbox_ (toLensVL $ maybeLens' % _2 % #collateralIsSwappable) [checkboxSquare]
                `styleBasic` [fgColor customGray1, hlColor customBlue]
            ]
        , spacer
        , hstack
            [ helpButton offerClaimPeriodMsg
            , spacer_ [width 3]
            , label "Claim Period:"
                `styleBasic` [textSize 10]
            , spacer
            , numericField_ (toLensVL $ maybeLens' % _2 % #claimPeriod) [M.decimals 0]
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
            , textField_ (toLensVL $ maybeLens' % _2 % #offerExpiration)
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
        , textArea (toLensVL $ maybeLens' % _2 % #collateralization)
            `styleBasic` [height 100, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer
        , box_ [alignRight] $ 
            hstack
              [ button "Cancel" (LendingEvent $ LendEvent $ AddSelectedOfferUpdate CancelAdding)
                  `styleBasic` [textSize 10]
              , spacer
              , mainButton "Confirm" (LendingEvent $ LendEvent $ AddSelectedOfferUpdate ConfirmAdding)
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

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
orderer :: SortDirection -> [LoanUTxO] -> [LoanUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: OpenOffersSortMethod -> [LoanUTxO] -> [LoanUTxO]
sorter sortingMethod = 
  case sortingMethod of
    OpenOffersLexicographically -> sortOn (view #utxoRef)
    OpenOffersTime -> sortOn (view #blockTime)
    OpenOffersLoanAmount -> sortOn loanUTxOLoanAmount
    OpenOffersDuration -> sortOn loanUTxOLoanDuration
    OpenOffersInterest -> sortOn loanUTxOLoanInterest
    OpenOffersExpiration -> sortOn (preview $ #loanDatum % _Just % _OfferDatum % #offerExpiration)

filterer :: POSIXTime -> ReverseTickerMap -> OpenOffersFilterModel -> [LoanUTxO] -> [LoanUTxO]
filterer currentTime reverseTickerMap OpenOffersFilterModel{..} us = do
    u <- us
    let offerDatum@Loans.OfferDatum{loanTerm,offerExpiration} = fromMaybe def $ loanUTxOOfferDatum u
        utxoCollateral = map (toNativeAsset . fst)
                       $ offerDatum ^. #collateralization % #unCollateralization
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) >= d) $ 
      readMaybe $ toString minDuration
    guard $ maybe True (\d -> calcDaysInPosixPeriod (fromPlutusTime loanTerm) <= d) $ 
      readMaybe $ toString maxDuration
    guard $ matchesAsset [toNativeAsset $ offerDatum ^. #loanAsset] loanAsset
    guard $ all (matchesAsset utxoCollateral) $ lines collateral
    guard $ expirationCheck shouldBeExpired offerExpiration
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

    expirationCheck :: Maybe Bool -> Maybe PlutusTime -> Bool
    expirationCheck mSetting mExprTime = case mSetting of
      Nothing -> True
      Just mustBeExpired -> 
        let check = (currentTime >=) . fromPlutusTime <$> mExprTime in
          if mustBeExpired then
            Just True == check
          else
            Just True /= check

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

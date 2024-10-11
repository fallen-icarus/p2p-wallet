module P2PWallet.GUI.Widgets.Options.Writer.ActiveContracts
  ( activeContractsWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

activeContractsWidget :: AppModel -> AppNode
activeContractsWidget model@AppModel{knownWallets,optionsModel=OptionsModel{..},reverseTickerMap,config} =
    zstack
      [ mainWidget
      , updateAddressWidget `nodeVisible` isJust (writerModel ^. #newWriterAddressUpdate)
      , activeFilterWidget model `nodeVisible` (writerModel ^. #showActivesFilter)
      ]
  where
    Config{timeZone,currentTime} = config

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]
  
    OptionsWallet{..} = selectedWallet

    allActives :: [OptionsUTxO]
    allActives = filter ((==Just True) . fmap (is _OptionsActiveDatum) . view #optionsDatum) utxos

    fractionShown :: Text
    fractionShown = show (length sample) <> "/" <> show (length allActives)

    sample :: [OptionsUTxO]
    sample = orderer (writerModel ^. #activesFilterModel % #sortingDirection) 
           $ sorter reverseTickerMap (writerModel ^. #activesFilterModel) 
           $ filterer currentTime reverseTickerMap (writerModel ^. #activesFilterModel) allActives
    
    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ centerWidgetH $ hstack 
            [ box_ [alignMiddle, onClick $ Alert activeOptionsMsg] $
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
            , spacer_ [width 5]
            , label ("Active Contracts (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , spacer_ [width 2]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #optionsModel % #writerModel % #showActivesFilter)
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

    activeRow :: OptionsUTxO -> AppNode
    activeRow u@OptionsUTxO{utxoRef,blockTime} = do
      let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum u
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askQuantity = roundUp $ toRational strikePrice * toRational offerQuantity
          askNativeAsset = toNativeAsset askAsset & #quantity .~ askQuantity
          prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate timeZone blockTime
            , showLocalTime timeZone blockTime
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = 
            find ((==payToAddress) . view #paymentAddress) $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
          formattedPrice = showPriceFormatted reverseTickerMap askNativeAsset offerAmount 
                         $ toRational strikePrice
          prettyPrice = mconcat
            [ "Strike Price: "
            , formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askNativeAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAmount
            ]
          prettyExpirationTime = unwords
            [ "Expiration:"
            , showLocalDate timeZone $ fromPlutusTime expiration
            , showLocalTime timeZone $ fromPlutusTime expiration
            ]
          (actionEvt, actionTip, actionIcon)
            | toPlutusTime (config ^. #currentTime) >= expiration =
                ( OptionsEvent $ OptionsWriterEvent $ AddSelectedExpiredOptionsClose u
                , "Close expired options contract"
                , closeCircleIcon
                )
            | otherwise =
                ( OptionsEvent $ OptionsWriterEvent $ 
                    UpdateWriterPaymentAddress $ StartAdding $ Just u
                , "Update payment address"
                , updatePaymentAddressIcon
                )
      hstack
        [ vstack
            [ hstack
                [ label ("Offer: " <> showAssetBalance True reverseTickerMap offerAmount)
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
                    tooltip_ ("Contract ID: " <> display contractId) [tooltipDelay 0] $
                      box_ [alignMiddle , onClick $ CopyText $ display contractId] $
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
                , widgetIf (expiration <= toPlutusTime currentTime) $
                    hstack
                      [ filler
                      , label "Expired"
                          `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                      , filler
                      ]
                , filler
                , label ("Ask: " <> showAssetBalance True reverseTickerMap askNativeAsset)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label prettyPrice
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label prettyExpirationTime
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ actionTip [tooltipDelay 0] $
                button actionIcon actionEvt 
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

updateAddressWidget :: AppNode
updateAddressWidget = do
  let maybeLens' = maybeLens def (#optionsModel % #writerModel % #newWriterAddressUpdate)
  vstack
    [ centerWidget $ vstack
        [ centerWidgetH $ label "Where would you like the payment to go?"
        , spacer_ [width 20]
        , hstack
            [ label "Address:"
            , spacer
            , textField (toLensVL $ maybeLens' % #newPaymentAddress)
                `styleBasic` [textSize 10, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ OptionsEvent $ OptionsWriterEvent $ 
                UpdateWriterPaymentAddress CancelAdding
            , spacer
            , mainButton "Confirm" $ OptionsEvent $ OptionsWriterEvent $ 
                UpdateWriterPaymentAddress ConfirmAdding
            ]
        ] `styleBasic` [radius 20, bgColor customGray3, padding 20]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

activeFilterWidget :: AppModel -> AppNode
activeFilterWidget AppModel{optionsModel=OptionsModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #optionsModel % #writerModel
      filterScene = writerModel ^. #activesFilterModel % #scene
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
                    , button "Reset" $ OptionsEvent $ OptionsWriterEvent ResetActiveContractsFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showActivesFilter)
                        [onClick $ OptionsEvent $ OptionsWriterEvent CheckActiveContractsFilterModel]
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
      let rootLens = #optionsModel % #writerModel % #activesFilterModel
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
      cushionWidgetH $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert proposalFilterOfferAssetMsg] $
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
            , label "Offer Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #offerAsset) 
                  [placeholder "ADA"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            , filler
            , label "Expired Contract:"
                `styleBasic` [textSize 10]
            , spacer_ [width 2]
            , choiceButton "Yes" (Just True) (toLensVL $ rootLens % #shouldBeExpired)
            , choiceButton "No" (Just False) (toLensVL $ rootLens % #shouldBeExpired)
            , choiceButton "Either" Nothing (toLensVL $ rootLens % #shouldBeExpired)
            ]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert proposalFilterAskAssetMsg] $
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
            , label "Ask Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #askAsset) 
                  [placeholder "ADA"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        ]

    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 12, bgColor customGray2, border 1 black]
                `styleHover` [textSize 12, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 12, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 12, bgColor customGray1, border 1 customBlue]
          ActiveContractsFilterModel{offerAsset,askAsset} = 
            writerModel ^. #activesFilterModel
          possibleSortingMethods = mconcat
            [ [ ActiveContractsLexicographically, ActiveContractsTime, ActiveContractsExpiration ]
            , [ ActiveContractsOfferAmount | offerAsset /= "" ]
            , [ ActiveContractsStrikePrice | offerAsset /= "" && askAsset /= "" ]
            , [ ActiveContractsAskAmount | askAsset /= "" ]
            ]
          rootLens = #optionsModel % #writerModel % #activesFilterModel
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
            , box_ [onClick $ Alert activeFilterSortMsg] $
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
targetQuantity :: ReverseTickerMap -> Text -> OptionsUTxO -> Maybe Integer
targetQuantity reverseTickerMap target p =
  fmap (view #quantity) $
    flip find (p ^. #nativeAssets) $ \NativeAsset{..} -> or
      [ display policyId <> "." <> display tokenName == target
      , Just target == fmap (display . fst) (Map.lookup (policyId,tokenName) reverseTickerMap)
      ]

orderer :: SortDirection -> [OptionsUTxO] -> [OptionsUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: ReverseTickerMap -> ActiveContractsFilterModel -> [OptionsUTxO] -> [OptionsUTxO]
sorter reverseTickerMap ActiveContractsFilterModel{..} = 
  case sortingMethod of
    ActiveContractsLexicographically -> sortOn (view #utxoRef)
    ActiveContractsTime -> sortOn (view #blockTime)
    ActiveContractsOfferAmount -> sortOn (targetQuantity reverseTickerMap offerAsset)
    ActiveContractsAskAmount -> 
      let focusDatum = fromMaybe def . preview (#optionsDatum % _Just % _OptionsActiveDatum)
          calcAskAmount Options.ActiveDatum{strikePrice,offerQuantity} = 
            toRational strikePrice * toRational offerQuantity
       in sortOn (calcAskAmount . focusDatum)
    ActiveContractsStrikePrice ->
      let focusPrice :: OptionsUTxO -> Maybe Options.Fraction
          focusPrice = preview (#optionsDatum % _Just % _OptionsActiveDatum % #strikePrice)
       in sortOn focusPrice
    ActiveContractsExpiration ->
      let focusExpiration :: OptionsUTxO -> Maybe PlutusTime
          focusExpiration = preview (#optionsDatum % _Just % _OptionsActiveDatum % #expiration)
       in sortOn focusExpiration

filterer 
  :: POSIXTime 
  -> ReverseTickerMap 
  -> ActiveContractsFilterModel 
  -> [OptionsUTxO] 
  -> [OptionsUTxO]
filterer currentTime reverseTickerMap ActiveContractsFilterModel{..} us = do
    u <- us
    let activeDatum = fromMaybe def $ optionsUTxOActiveDatum u
    guard $ matchesAsset [toNativeAsset $ activeDatum ^. #offerAsset] offerAsset
    guard $ matchesAsset [toNativeAsset $ activeDatum ^. #askAsset] askAsset
    guard $ expirationCheck shouldBeExpired $ activeDatum ^. #expiration
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

    expirationCheck :: Maybe Bool -> PlutusTime -> Bool
    expirationCheck mSetting expiration = case mSetting of
      Nothing -> True
      Just mustBeExpired -> 
        if mustBeExpired then
          toPlutusTime currentTime >= expiration
        else
          toPlutusTime currentTime < expiration

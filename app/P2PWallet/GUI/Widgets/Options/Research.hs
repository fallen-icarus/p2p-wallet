module P2PWallet.GUI.Widgets.Options.Research
  ( researchWidget
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

researchWidget :: AppModel -> AppNode
researchWidget model@AppModel{optionsModel=OptionsModel{..}} = do
  let OptionsResearchModel{selectedActiveContractAssets,choosingActiveContractAssets} = researchModel
  zstack 
    [ allActivesWidget model
        `nodeVisible` and
          [ isJust selectedActiveContractAssets
          , not choosingActiveContractAssets
          , not $ model ^. #waitingStatus % #syncingActiveOptionsContracts
          ]
    , getActiveContractAssetsWidget model
        `nodeVisible` (isNothing selectedActiveContractAssets || choosingActiveContractAssets)
    , activeFilterWidget model `nodeVisible` (researchModel ^. #showActiveFilter)
    ]

getActiveContractAssetsWidget :: AppModel -> AppNode
getActiveContractAssetsWidget model = do
  centerWidget $ vstack
    [ centerWidgetH $ label "Active Options Contract Trading Pair"
        `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
    , spacer_ [width 20]
    , centerWidgetH $ hstack
        [ box_ [alignMiddle, onClick $ Alert writerOfferAssetMsg] $
            label helpIcon
              `styleBasic`
                [ border 0 transparent
                , radius 20
                , bgColor transparent
                , textColor customBlue
                , textMiddle
                , textFont "Remix"
                ]
              `styleHover` [bgColor customGray2, cursorIcon CursorHand]
        , spacer_ [width 3]
        , textField_ (toLensVL $ #optionsModel % #researchModel % #newActiveContractAssets % _1)
              [placeholder "Offer Asset"]
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer
        , label remixArrowRightLine 
            `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
        , spacer
        , textField_ (toLensVL $ #optionsModel % #researchModel % #newActiveContractAssets % _2)
              [placeholder "Ask Asset"]
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , box_ [alignMiddle, onClick $ Alert writerAskAssetMsg] $
            label helpIcon
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
    , spacer
    , hstack 
        [ filler
        , button "Cancel" (OptionsEvent $ OptionsResearchEvent $ SetResearchActiveContractAssets CancelAdding)
            `nodeVisible` isJust (model ^. #optionsModel % #researchModel % #selectedActiveContractAssets)
        , spacer
        , mainButton "Confirm" $ OptionsEvent $ OptionsResearchEvent $ 
            SetResearchActiveContractAssets ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, radius 10]

activeFilterWidget :: AppModel -> AppNode
activeFilterWidget AppModel{optionsModel=OptionsModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #optionsModel % #researchModel
      filterScene = researchModel ^. #activesFilterModel % #scene
  vstack
    [ centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Sort" SortScene 
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
                    [ widgetIf (filterScene == SortScene) sortWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ OptionsEvent $ OptionsResearchEvent ResetResearchAllActivesFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ rootLens % #showActiveFilter)
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
    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 12, bgColor customGray2, border 1 black]
                `styleHover` [textSize 12, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 12, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 12, bgColor customGray1, border 1 customBlue]
          possibleSortingMethods = enumFrom AllActiveOptionsLexicographically
          rootLens = #optionsModel % #researchModel % #activesFilterModel
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
            , box_ [onClick $ Alert allActivesFilterSortMsg] $
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

allActivesWidget :: AppModel -> AppNode
allActivesWidget AppModel{optionsModel=OptionsModel{scene=_,..},..} =
    cushionWidgetH $ vstack
      [ spacer
      , centerWidgetH $ hstack 
          [ tooltip_ "Resync Contracts" [tooltipDelay 0] $
              box_ [alignMiddle, onClick resyncEvt] $
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
          , tooltip_ "Change Trading Pair" [tooltipDelay 0] $
              box_ [onClick changePairEvt] $
                hstack
                  [ label "Active Options For"
                      `styleBasic` [textFont "Italics", textSize 12]
                  , spacer_ [width 2]
                  , label (showAssetNameOnly reverseTickerMap $ toNativeAsset targetOffer)
                      `styleBasic` [textSize 12, textFont "Italics"]
                  , spacer_ [width 2]
                  , label remixArrowRightLine 
                      `styleBasic` [textMiddle, textFont "Remix", radius 5]
                  , spacer_ [width 2]
                  , label (showAssetNameOnly reverseTickerMap $ toNativeAsset targetAsk)
                      `styleBasic` [textSize 12, textFont "Italics"]
                  ] `styleBasic` [padding 5 , radius 5, border 1 customBlue]
                    `styleHover` [bgColor customGray1, cursorIcon CursorHand]
          , spacer_ [width 2]
          , tooltip_ "Sort" [tooltipDelay 0] $
              toggleButton_ menuSearchIcon
                (toLensVL $ #optionsModel % #researchModel % #showActiveFilter)
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
    Config{network,timeZone,currentTime} = config

    OptionsResearchModel{..} = researchModel

    targets@(targetOffer,targetAsk) = fromMaybe (def,def) selectedActiveContractAssets

    resyncEvt = OptionsEvent 
              $ SyncActiveOptionsContracts
              $ StartProcess 
              $ Just targets

    changePairEvt = OptionsEvent 
                  $ OptionsResearchEvent 
                  $ SetResearchActiveContractAssets 
                  $ StartAdding Nothing

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    sample :: [OptionsUTxO]
    sample = orderer (activesFilterModel ^. #sortingDirection) 
           $ sorter (toNativeAsset targetOffer) activesFilterModel
           $ maybe [] (filterOutFullyExpired currentTime)
           $ selectedActiveContractAssets >>= flip Map.lookup cachedActiveContracts

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
      vstack
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

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
targetQuantity :: NativeAsset -> OptionsUTxO -> Maybe Integer
targetQuantity target p =
  fmap (view #quantity) $
    flip find (p ^. #nativeAssets) $ \NativeAsset{fingerprint} ->
      fingerprint == target ^. #fingerprint

orderer :: SortDirection -> [OptionsUTxO] -> [OptionsUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: NativeAsset -> AllActiveOptionsFilterModel -> [OptionsUTxO] -> [OptionsUTxO]
sorter offerAsset AllActiveOptionsFilterModel{..} = 
  case sortingMethod of
    AllActiveOptionsLexicographically -> sortOn (view #utxoRef)
    AllActiveOptionsTime -> sortOn (view #blockTime)
    AllActiveOptionsOfferAmount -> sortOn (targetQuantity offerAsset)
    AllActiveOptionsAskAmount -> 
      let focusDatum = fromMaybe def . preview (#optionsDatum % _Just % _OptionsActiveDatum)
          calcAskAmount Options.ActiveDatum{strikePrice,offerQuantity} = 
            toRational strikePrice * toRational offerQuantity
       in sortOn (calcAskAmount . focusDatum)
    AllActiveOptionsStrikePrice ->
      let focusPrice :: OptionsUTxO -> Maybe Options.Fraction
          focusPrice = preview (#optionsDatum % _Just % _OptionsActiveDatum % #strikePrice)
       in sortOn focusPrice
    AllActiveOptionsExpiration ->
      let focusExpiration :: OptionsUTxO -> Maybe PlutusTime
          focusExpiration = preview (#optionsDatum % _Just % _OptionsActiveDatum % #expiration)
       in sortOn focusExpiration

filterOutFullyExpired :: POSIXTime -> [OptionsUTxO] -> [OptionsUTxO]
filterOutFullyExpired currentTime us = do
  u <- us
  let Options.ActiveDatum{expiration} = fromMaybe def $ optionsUTxOActiveDatum u
  guard $ toPlutusTime currentTime < expiration
  return u

module P2PWallet.GUI.Widgets.Options.Buyer
  ( buyerWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map
import Data.List (minimum, maximum)

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

buyerWidget :: AppModel -> AppNode
buyerWidget model@AppModel{optionsModel=OptionsModel{..}} =
    zstack
      [ getContractAssetsWidget model
          `nodeVisible` (isNothing selectedContractAssets || choosingContractAssets)
      , allProposalsWidget model
          `nodeVisible` and
            [ isJust selectedContractAssets
            , not choosingContractAssets
            , not $ model ^. #waitingStatus % #syncingOptionsProposals
            ]
      , proposalFilterWidget model `nodeVisible` (buyerModel ^. #showProposalFilter)
      ]
  where
    OptionsBuyerModel{selectedContractAssets, choosingContractAssets} = buyerModel

getContractAssetsWidget :: AppModel -> AppNode
getContractAssetsWidget model = do
  centerWidget $ vstack
    [ centerWidgetH $ label "Which trading pair would you like to lookup?"
    , spacer_ [width 20]
    , centerWidgetH $ hstack
        [ box_ [alignMiddle, onClick $ Alert offerAssetMsg] $
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
        , textField_ (toLensVL $ #optionsModel % #buyerModel % #newContractAssets % _1)
              [placeholder "Offer Asset"]
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer
        , label remixArrowRightLine 
            `styleBasic` [textMiddle, textFont "Remix", textColor customBlue, radius 5]
        , spacer
        , textField_ (toLensVL $ #optionsModel % #buyerModel % #newContractAssets % _2)
              [placeholder "Ask Asset"]
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , box_ [alignMiddle, onClick $ Alert askAssetMsg] $
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
    , spacer_ [width 10]
    , centerWidgetH $ hstack
        [ box_ [alignMiddle, onClick $ Alert premiumAssetMsg] $
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
        , textField_ (toLensVL $ #optionsModel % #buyerModel % #newContractAssets % _3)
              [placeholder "Premium Asset"]
            `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" (OptionsEvent $ OptionsBuyerEvent $ SetNewContractAssets CancelAdding)
            `nodeVisible` isJust (model ^. #optionsModel % #buyerModel % #selectedContractAssets)
        , spacer
        , mainButton "Confirm" $ OptionsEvent $ OptionsBuyerEvent $ SetNewContractAssets ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, radius 10]

allProposalsWidget :: AppModel -> AppNode
allProposalsWidget AppModel{optionsModel=OptionsModel{..},reverseTickerMap,config} = do
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
              box_ [onClick $ OptionsEvent $ OptionsBuyerEvent $ SetNewContractAssets $ StartAdding Nothing] $
                hstack
                  [ label "Proposed Options For"
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
          , tooltip_ "Sort/Filter" [tooltipDelay 0] $
              toggleButton_ menuSearchIcon
                (toLensVL $ #optionsModel % #buyerModel % #showProposalFilter)
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
            vstack_ [childSpacing] (map proposalRow sample)
              `styleBasic` [padding 10]
        , filler
        ] 
  where
    Config{network,timeZone,currentTime} = config

    targets@(targetOffer,targetAsk,_) = 
      fromMaybe (def,def,Nothing) $ buyerModel ^. #selectedContractAssets

    resyncEvt = OptionsEvent 
              $ SyncOptionsProposals 
              $ StartProcess 
              $ Just targets

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    allProposals :: [OptionsUTxO]
    allProposals = filterOutFullyExpired currentTime
                 $ filter ((==Just True) . fmap (is _OptionsProposalDatum) . view #optionsDatum)
                 $ fromMaybe []
                 $ Map.lookup targets cachedProposals

    sample :: [OptionsUTxO]
    sample = orderer (buyerModel ^. #proposalsFilterModel % #sortingDirection) 
           $ sorter targetOffer (buyerModel ^. #proposalsFilterModel) allProposals

    termsRow 
      :: OptionsUTxO
      -> NativeAsset 
      -> NativeAsset 
      -> NativeAsset 
      -> Integer
      -> Options.Terms
      -> AppNode
    termsRow u offerAsset askAsset premiumAsset targetIdx terms = do
      let Options.Terms{premium, strikePrice, expiration} = terms
          formattedPrice = 
            showPriceFormatted reverseTickerMap askAsset offerAsset $ toRational strikePrice
          prettyPrice = mconcat
            [ formattedPrice
            , " "
            , showAssetNameOnly reverseTickerMap askAsset
            , " / "
            , showAssetNameOnly reverseTickerMap offerAsset
            ]
          prettyPremium = 
            showAssetBalance True reverseTickerMap $ premiumAsset & #quantity .~ premium
          purchaseEvt = OptionsEvent 
                      $ OptionsBuyerEvent 
                      $ PurchaseOptionsProposal 
                      $ StartProcess 
                      $ Just (targetIdx,u)
          termsWidget = hstack
            [ spacer_ [width 2]
            , label ("Premium: " <> prettyPremium)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Price: " <> prettyPrice)
                `styleBasic` [textSize 8, textColor lightGray]
            , filler
            , label ("Expiration: " <> showLocalDate timeZone (fromPlutusTime $ expiration - 1))
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            ] `styleBasic` 
                [ bgColor customGray4
                , padding 2
                , radius 3
                , border 1 customGray1
                ]
              `styleHover` [border 1 customBlue, cursorIcon CursorHand]
      tooltip_ "Purchase Contract" [tooltipDelay 0] $ 
        box_ [onClick purchaseEvt] termsWidget

    proposalRow :: OptionsUTxO -> AppNode
    proposalRow u@OptionsUTxO{utxoRef} = do
      let Options.ProposalDatum{..} = fromMaybe def $ optionsUTxOProposalDatum u
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          premiumNativeAsset = toNativeAsset premiumAsset
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords
            [ "Payments to:"
            , display payToAddress
            ]
      vstack
        [ hstack
            [ label ("Offer Asset: " <> showAssetBalance True reverseTickerMap offerAmount)
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
            , filler
            , label ("Ask Asset: " <> showAssetNameOnly reverseTickerMap askNativeAsset)
                `styleBasic` [textSize 10, textColor white]
            ]
        , spacer_ [width 2]
        , label "Possible Terms:"
            `styleBasic` [textSize 8, textColor lightGray]
        , spacer_ [width 2]
        , hstack
            [ spacer
            , vstack $ 
                zipWith 
                  (termsRow u offerAmount askNativeAsset premiumNativeAsset)
                  [0..] 
                  possibleTerms
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

proposalFilterWidget :: AppModel -> AppNode
proposalFilterWidget AppModel{optionsModel=OptionsModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #optionsModel % #buyerModel
      filterScene = buyerModel ^. #proposalsFilterModel % #scene
  vstack
    [ centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Sort" SortScene 
                    (toLensVL $ rootLens % #proposalsFilterModel % #scene) 
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
                    , button "Reset" $ OptionsEvent $ OptionsBuyerEvent ResetAllProposalsFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ rootLens % #showProposalFilter)
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
          (_,_,mTargetPremium) = fromMaybe (def,def,Nothing) $ buyerModel ^. #selectedContractAssets
          possibleSortingMethods = mconcat
            [ [ AllProposalsExpiration, AllProposalsOfferAmount, AllProposalsStrikePrice ]
            , [ AllProposalsPremium | isJust mTargetPremium ]
            ]
          rootLens = #optionsModel % #buyerModel % #proposalsFilterModel
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
            , box_ [onClick $ Alert allProposalFilterSortMsg] $
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
targetQuantity :: NativeAsset -> OptionsUTxO -> Maybe Integer
targetQuantity target p =
  fmap (view #quantity) $
    flip find (p ^. #nativeAssets) $ \NativeAsset{fingerprint} ->
      fingerprint == target ^. #fingerprint

orderer :: SortDirection -> [OptionsUTxO] -> [OptionsUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: Options.OfferAsset -> AllProposalsFilterModel -> [OptionsUTxO] -> [OptionsUTxO]
sorter offerAsset AllProposalsFilterModel{sortingMethod,sortingDirection} = 
  case sortingMethod of
    AllProposalsOfferAmount -> sortOn (targetQuantity $ toNativeAsset offerAsset)
    AllProposalsPremium ->
      let focusPremiums :: OptionsUTxO -> Maybe [Integer]
          focusPremiums = fmap (map $ view #premium) 
                        . preview (#optionsDatum % _Just % _OptionsProposalDatum % #possibleTerms)
       in case sortingDirection of
            -- Sort on the smallest premium.
            SortAscending -> sortOn (fmap minimum . focusPremiums)
            -- Sort on the largest premium.
            SortDescending -> sortOn (fmap maximum . focusPremiums)
    AllProposalsStrikePrice ->
      let focusPrice :: OptionsUTxO -> Maybe [Options.Fraction]
          focusPrice = fmap (map $ view #strikePrice) 
                     . preview (#optionsDatum % _Just % _OptionsProposalDatum % #possibleTerms)
       in case sortingDirection of
            -- Sort on the lowest price.
            SortAscending -> sortOn (fmap minimum . focusPrice)
            -- Sort on the highest price.
            SortDescending -> sortOn (fmap maximum . focusPrice)
    AllProposalsExpiration ->
      let focusExpirations :: OptionsUTxO -> Maybe [PlutusTime]
          focusExpirations = fmap (map $ view #expiration) 
                           . preview (#optionsDatum % _Just % _OptionsProposalDatum % #possibleTerms)
       in case sortingDirection of
            -- Sort on the earliest expiration.
            SortAscending -> sortOn (fmap minimum . focusExpirations)
            -- Sort on the latest expiration.
            SortDescending -> sortOn (fmap maximum . focusExpirations)

filterOutFullyExpired :: POSIXTime -> [OptionsUTxO] -> [OptionsUTxO]
filterOutFullyExpired currentTime us = do
  u <- us
  let Options.ProposalDatum{possibleTerms} = fromMaybe def $ optionsUTxOProposalDatum u
  -- Only one terms needs to still be valid.
  guard $ any ((toPlutusTime currentTime <) . view #expiration) possibleTerms
  return u

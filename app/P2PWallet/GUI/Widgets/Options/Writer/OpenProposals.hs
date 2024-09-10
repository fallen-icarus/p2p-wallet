module P2PWallet.GUI.Widgets.Options.Writer.OpenProposals
  ( openProposalsWidget
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

openProposalsWidget :: AppModel -> AppNode
openProposalsWidget model@AppModel{knownWallets,optionsModel=OptionsModel{..},reverseTickerMap,config} =
    zstack
      [ mainWidget
      , createNewProposalWidget model `nodeVisible` isJust (writerModel ^. #newProposalCreation)
      , updateProposalWidget model `nodeVisible` isJust (writerModel ^. #newProposalUpdate)
      , proposalFilterWidget model `nodeVisible` (writerModel ^. #showProposalFilter)
      ]
  where
    Config{timeZone,currentTime} = config

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]
  
    OptionsWallet{..} = selectedWallet

    allProposals :: [OptionsUTxO]
    allProposals = filter ((==Just True) . fmap (is _OptionsProposalDatum) . view #optionsDatum) utxos

    fractionShown :: Text
    fractionShown = show (length sample) <> "/" <> show (length allProposals)

    sample :: [OptionsUTxO]
    sample = orderer (writerModel ^. #proposalsFilterModel % #sortingDirection) 
           $ sorter reverseTickerMap (writerModel ^. #proposalsFilterModel) 
           $ filterer currentTime reverseTickerMap (writerModel ^. #proposalsFilterModel) allProposals

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ hstack 
            [ box_ [alignMiddle, onClick $ Alert optionsProposalsMsg] $
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
            , label ("Proposals (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , spacer_ [width 5]
            , tooltip_ "Create New Proposal" [tooltipDelay 0] $
                mainButton "New" 
                  (OptionsEvent $ OptionsWriterEvent $ CreateNewOptionsProposal $ StartAdding Nothing)
                  `styleBasic`
                    [ radius 5
                    , textMiddle
                    , textFont "Italics"
                    , padding 3
                    , textSize 10
                    ]
                  `styleHover` [cursorIcon CursorHand]
            , spacer_ [width 2]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #optionsModel % #writerModel % #showProposalFilter)
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
            , filler
            ]
        , flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
            vstack_ [childSpacing] (map proposalRow sample)
              `styleBasic` [padding 10]
        , filler
        ] 

    termsRow 
      :: NativeAsset 
      -> NativeAsset 
      -> NativeAsset 
      -> Options.Terms
      -> AppNode
    termsRow offerAsset askAsset premiumAsset Options.Terms{premium, strikePrice, expiration} = do
      let formattedPrice = 
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
      hstack
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

    proposalRow :: OptionsUTxO -> AppNode
    proposalRow u@OptionsUTxO{utxoRef,blockTime} = do
      let Options.ProposalDatum{..} = fromMaybe def $ optionsUTxOProposalDatum u
          offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
          askNativeAsset = toNativeAsset askAsset
          premiumNativeAsset = toNativeAsset premiumAsset
          prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          mTargetWallet = 
            find ((==payToAddress) . view #paymentAddress) $ knownWallets ^. #paymentWallets
          addressTip = unwords $ filter (/= "")
            [ "Payments to"
            , maybe ":" ((<> ":") . view #alias) mTargetWallet
            , display payToAddress
            ]
      hstack
        [ vstack
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
                , vstack (map (termsRow offerAmount askNativeAsset premiumNativeAsset) possibleTerms)
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
                button editIcon 
                  (OptionsEvent $ OptionsWriterEvent $ AddSelectedProposalUpdate $ StartAdding $ Just u)
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
                button closeCircleIcon (OptionsEvent $ OptionsWriterEvent $ AddSelectedProposalClose u)
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

createNewProposalWidget :: AppModel -> AppNode
createNewProposalWidget AppModel{knownWallets} = do
  let maybeLens' = maybeLens def $ #optionsModel % #writerModel % #newProposalCreation
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
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
    [ centerWidget $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "New Options Proposal"
              `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
        , spacer
        , hstack
            [ helpButton proposalOfferAmountMsg
            , spacer_ [width 3]
            , label "Offer Amount:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % #offerAsset) 
                  [placeholder "10 ADA"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ helpButton proposalAskAssetMsg
            , spacer_ [width 3]
            , label "Ask Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % #askAsset) 
                  [placeholder "DJED"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ helpButton proposalPremiumAssetMsg
            , spacer_ [width 3]
            , label "Premium Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % #premiumAsset) 
                  [placeholder "DJED"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ helpButton proposalPaymentAddressMsg
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
        , separatorLine `styleBasic` [fgColor darkGray]
        , spacer
        , hstack
            [ helpButton proposalTermsMsg
            , spacer_ [width 3]
            , label "Possible Terms (separated with newlines):"
                `styleBasic` [textSize 10]
            ]
        , spacer
        , textArea (toLensVL $ maybeLens' % #possibleTerms)
            `styleBasic` [height 180, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer
        , box_ [alignRight] $ 
            hstack
              [ button "Cancel" (OptionsEvent $ OptionsWriterEvent $ CreateNewOptionsProposal CancelAdding)
                  `styleBasic` [textSize 10]
              , spacer
              , mainButton "Confirm" (OptionsEvent $ OptionsWriterEvent $ CreateNewOptionsProposal ConfirmAdding)
                  `styleBasic` [textSize 10]
              ]
        ] `styleBasic`
            [ bgColor customGray3
            , padding 20
            , radius 20
            ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 20
        , radius 10
        ]

updateProposalWidget :: AppModel -> AppNode
updateProposalWidget AppModel{knownWallets} = do
  let maybeLens' = maybeLens (def,def) $ #optionsModel % #writerModel % #newProposalUpdate
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
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
    [ centerWidget $ vstack
        [ spacer
        , box_ [alignMiddle] $
            label "New Options Proposal"
              `styleBasic` [textSize 16, textFont "Italics", textColor customBlue]
        , spacer
        , hstack
            [ helpButton proposalOfferAmountMsg
            , spacer_ [width 3]
            , label "Offer Amount:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #offerAsset) 
                  [placeholder "10 ADA"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ helpButton proposalAskAssetMsg
            , spacer_ [width 3]
            , label "Ask Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #askAsset) 
                  [placeholder "DJED"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ helpButton proposalPremiumAssetMsg
            , spacer_ [width 3]
            , label "Premium Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ maybeLens' % _2 % #premiumAsset) 
                  [placeholder "DJED"] 
                `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                `styleFocus` [border 1 customBlue]
            ]
        , spacer
        , hstack
            [ helpButton proposalPaymentAddressMsg
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
        , separatorLine `styleBasic` [fgColor darkGray]
        , spacer
        , hstack
            [ helpButton proposalTermsMsg
            , spacer_ [width 3]
            , label "Possible Terms (separated with newlines):"
                `styleBasic` [textSize 10]
            ]
        , spacer
        , textArea (toLensVL $ maybeLens' % _2 % #possibleTerms)
            `styleBasic` [height 180, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer
        , box_ [alignRight] $ 
            hstack
              [ button "Cancel" (OptionsEvent $ OptionsWriterEvent $ AddSelectedProposalUpdate CancelAdding)
                  `styleBasic` [textSize 10]
              , spacer
              , mainButton "Confirm" (OptionsEvent $ OptionsWriterEvent $ AddSelectedProposalUpdate ConfirmAdding)
                  `styleBasic` [textSize 10]
              ]
        ] `styleBasic`
            [ bgColor customGray3
            , padding 20
            , radius 20
            ]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 20
        , radius 10
        ]

proposalFilterWidget :: AppModel -> AppNode
proposalFilterWidget AppModel{optionsModel=OptionsModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #optionsModel % #writerModel
      filterScene = writerModel ^. #proposalsFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #proposalsFilterModel % #scene) 
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
                    [ widgetIf (filterScene == FilterScene) filterWidget
                    , widgetIf (filterScene == SortScene) sortWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ OptionsEvent $ OptionsWriterEvent ResetOpenProposalsFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showProposalFilter)
                        [onClick $ OptionsEvent $ OptionsWriterEvent CheckOpenProposalsFilterModel]
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
      let rootLens = #optionsModel % #writerModel % #proposalsFilterModel
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
            , filler
            , box_ [alignMiddle, onClick $ Alert proposalFilterPremiumAssetMsg] $
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
            , label "Premium Asset:"
                `styleBasic` [textSize 10]
            , spacer
            , textField_ (toLensVL $ rootLens % #premiumAsset) 
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
          OpenProposalsFilterModel{offerAsset,askAsset,premiumAsset} = 
            writerModel ^. #proposalsFilterModel
          possibleSortingMethods = mconcat
            [ [ OpenProposalsLexicographically, OpenProposalsTime, OpenProposalsExpiration ]
            , [ OpenProposalsOfferAmount | offerAsset /= "" ]
            , [ OpenProposalsStrikePrice | offerAsset /= "" && askAsset /= "" ]
            , [ OpenProposalsPremium | premiumAsset /= "" ]
            ]
          rootLens = #optionsModel % #writerModel % #proposalsFilterModel
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
            , box_ [onClick $ Alert proposalFilterSortMsg] $
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

sorter :: ReverseTickerMap -> OpenProposalsFilterModel -> [OptionsUTxO] -> [OptionsUTxO]
sorter reverseTickerMap OpenProposalsFilterModel{..} = 
  case sortingMethod of
    OpenProposalsLexicographically -> sortOn (view #utxoRef)
    OpenProposalsTime -> sortOn (view #blockTime)
    OpenProposalsOfferAmount -> sortOn (targetQuantity reverseTickerMap offerAsset)
    OpenProposalsPremium ->
      let focusPremiums :: OptionsUTxO -> Maybe [Integer]
          focusPremiums = fmap (map $ view #premium) 
                        . preview (#optionsDatum % _Just % _OptionsProposalDatum % #possibleTerms)
       in case sortingDirection of
            -- Sort on the smallest premium.
            SortAscending -> sortOn (fmap minimum . focusPremiums)
            -- Sort on the largest premium.
            SortDescending -> sortOn (fmap maximum . focusPremiums)
    OpenProposalsStrikePrice ->
      let focusPrice :: OptionsUTxO -> Maybe [Options.Fraction]
          focusPrice = fmap (map $ view #strikePrice) 
                     . preview (#optionsDatum % _Just % _OptionsProposalDatum % #possibleTerms)
       in case sortingDirection of
            -- Sort on the lowest price.
            SortAscending -> sortOn (fmap minimum . focusPrice)
            -- Sort on the highest price.
            SortDescending -> sortOn (fmap maximum . focusPrice)
    OpenProposalsExpiration ->
      let focusExpirations :: OptionsUTxO -> Maybe [PlutusTime]
          focusExpirations = fmap (map $ view #expiration) 
                           . preview (#optionsDatum % _Just % _OptionsProposalDatum % #possibleTerms)
       in case sortingDirection of
            -- Sort on the earliest expiration.
            SortAscending -> sortOn (fmap minimum . focusExpirations)
            -- Sort on the latest expiration.
            SortDescending -> sortOn (fmap maximum . focusExpirations)

filterer 
  :: POSIXTime 
  -> ReverseTickerMap 
  -> OpenProposalsFilterModel 
  -> [OptionsUTxO] 
  -> [OptionsUTxO]
filterer currentTime reverseTickerMap OpenProposalsFilterModel{..} us = do
    u <- us
    let proposalDatum = fromMaybe def $ optionsUTxOProposalDatum u
    guard $ matchesAsset [toNativeAsset $ proposalDatum ^. #offerAsset] offerAsset
    guard $ matchesAsset [toNativeAsset $ proposalDatum ^. #askAsset] askAsset
    guard $ matchesAsset [toNativeAsset $ proposalDatum ^. #premiumAsset] premiumAsset
    guard $ expirationCheck shouldBeExpired $ proposalDatum ^. #possibleTerms
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

    expirationCheck :: Maybe Bool -> [Options.Terms] -> Bool
    expirationCheck mSetting terms = case mSetting of
      Nothing -> True
      Just mustBeExpired -> 
        let expirations = map (view #expiration) terms in
          if mustBeExpired then
            all (toPlutusTime currentTime >=) expirations
          else
            not $ all (toPlutusTime currentTime >=) expirations

module P2PWallet.GUI.Widgets.Aftermarket.Seller.CurrentBids
  ( currentBidsWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Aftermarket.Common
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

currentBidsWidget :: AppModel -> AppNode
currentBidsWidget model@AppModel{knownWallets,aftermarketModel,reverseTickerMap,config} =
    zstack
      [ mainWidget
      , claimBidAcceptanceWidget model `nodeVisible` and
          [ isJust (sellerModel ^. #newClaimBidAcceptance)
          -- Hide until after syncing is complete.
          , not $ model ^. #waitingStatus % #syncingLoanHistories
          , not $ model ^. #waitingStatus % #syncingOptionsContracts
          ]
      , bidsFilterWidget model `nodeVisible` (sellerModel ^. #showBidFilter)
      , widgetMaybe (sellerModel ^. #inspectedBid) $ \bidUTxO ->
          inspectBatchWidget model 
            InspectBatchConfig
              { batchUTxO = bidUTxO
              , closeEvent = 
                  AftermarketEvent $ AftermarketSellerEvent CloseInspectedAftermarketSellerBid
              , inspectLoanHistoryEvent =
                  AftermarketEvent . AftermarketSellerEvent . InspectSellerLoanHistory
              , lookupBorrowerEvent =
                  AftermarketEvent . AftermarketSellerEvent . InspectSellerBorrowerInformation
              , mAddToHomeEvent = Nothing
              }
          `nodeVisible` and
            -- Hide until after syncing is complete.
            [ not $ model ^. #waitingStatus % #syncingLoanHistories
            , not $ model ^. #waitingStatus % #syncingOptionsContracts
            ]
      ]
  where
    AftermarketModel{..} = aftermarketModel

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]
  
    MarketWallet{utxos} = selectedWallet

    Config{currentTime,network} = config

    allBids :: [AftermarketUTxO]
    allBids = filter (isNotExpiredBid currentTime)
            $ filter ((==Just True) . fmap isBidDatum . view #marketDatum) utxos

    fractionShown :: Text
    fractionShown = show (length sample) <> "/" <> show (length allBids)

    sample :: [AftermarketUTxO]
    sample = orderer (sellerModel ^. #bidsFilterModel % #sortingDirection) 
           $ sorter (sellerModel ^. #bidsFilterModel) 
           $ filterer (sellerModel ^. #bidsFilterModel) allBids

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ centerWidgetH $ hstack 
            [ label ("Current Bids (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , spacer_ [width 2]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #aftermarketModel % #sellerModel % #showBidFilter)
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
            vstack_ [childSpacing] (map bidRow sample)
              `styleBasic` [padding 10]
        , filler
        ] 

    bidRow :: AftermarketUTxO -> AppNode
    bidRow u@AftermarketUTxO{marketDatum} = case marketDatum of
      (Just (SpotBidDatum spotBidDatum)) -> spotBidRow u spotBidDatum
      (Just (ClaimBidDatum claimBidDatum)) -> claimBidRow u claimBidDatum
      (Just (AcceptedBidDatum acceptedBidDatum)) -> acceptedBidRow u acceptedBidDatum
      _ -> spacer `nodeVisible` False

    priceWidget :: NativeAsset -> AppNode
    priceWidget priceAsset = do
      hstack
        [ spacer_ [width 2]
        , label (showAssetBalance True reverseTickerMap priceAsset)
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

    spotBidRow :: AftermarketUTxO -> Aftermarket.SpotBidDatum -> AppNode
    spotBidRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.SpotBidDatum{..} = do
      let prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
          addressTip = unwords $ filter (/= "")
            [ "Payments to:"
            , display payToAddress
            ]
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ bid ^. #unPrices
          inspectEvt = AftermarketEvent $ AftermarketSellerEvent $ InspectAftermarketSellerBid u
          acceptEvt = AftermarketEvent 
                    $ AftermarketSellerEvent 
                    $ AddSelectedSpotBidAcceptance u
      hstack
        [ vstack
            [ hstack
                [ label "Spot Bid"
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
                , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                    box_ [alignMiddle, onClick inspectEvt] $
                      label inspectIcon
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
                , label (show numNfts <> " " <> keyTypeLabel)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ box_ [alignTop] $ label "Bid:"
                    `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
                , spacer_ [width 2]
                , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                    \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Accept Bid" [tooltipDelay 0] $
                button swapIcon acceptEvt
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

    claimBidRow :: AftermarketUTxO -> Aftermarket.ClaimBidDatum -> AppNode
    claimBidRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.ClaimBidDatum{..} = do
      let prettyLocalTime = unwords
            [ "Created:"
            , showLocalDate (config ^. #timeZone) blockTime
            , showLocalTime (config ^. #timeZone) blockTime
            ]
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ bid ^. #unPrices
          prettyExpirationTime = unlines
            [ unwords
                [ "Bid Expires:"
                , flip (maybe "Never") bidExpiration $ \expr -> unwords
                    [ showLocalDate (config ^. #timeZone) $ fromPlutusTime expr
                    , showLocalTime (config ^. #timeZone) $ fromPlutusTime expr
                    ]
                ]
            , unwords
                [ "Claim Deadline:"
                , showLocalDate (config ^. #timeZone) $ fromPlutusTime claimExpiration
                , showLocalTime (config ^. #timeZone) $ fromPlutusTime claimExpiration
                ]
            ]
          inspectEvt = AftermarketEvent $ AftermarketSellerEvent $ InspectAftermarketSellerBid u
          acceptEvt = AftermarketEvent 
                    $ AftermarketSellerEvent 
                    $ AddSelectedClaimBidAcceptance
                    $ StartAdding 
                    $ Just u
          acceptTip
            | null $ knownWallets ^. #paymentWallets =
                "Add a payment wallet under the 'Home' page to enable accepting claim bids"
            | otherwise = "Accept Bid"
      hstack
        [ vstack
            [ hstack
                [ label "Claim Bid"
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
                , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                    box_ [alignMiddle, onClick inspectEvt] $
                      label inspectIcon
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
                , widgetIf (maybe False (<= toPlutusTime currentTime) bidExpiration) $
                    hstack
                      [ filler
                      , label "Expired"
                          `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                      , filler
                      ]
                , filler
                , label (show numNfts <> " " <> keyTypeLabel)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ box_ [alignTop] $ label "Bid:"
                    `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
                , spacer_ [width 2]
                , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                    \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ acceptTip [tooltipDelay 0] $
                button swapIcon acceptEvt
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
                  `nodeEnabled` (knownWallets ^. #paymentWallets /= [])
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

    acceptedBidRow :: AftermarketUTxO -> Aftermarket.AcceptedBidDatum -> AppNode
    acceptedBidRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.AcceptedBidDatum{..} = do
      let prettyLocalTime = unwords
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
          numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ bid ^. #unPrices
          prettyExpirationTime = unlines
            [ unwords
                [ "Claim Deadline:"
                , showLocalDate (config ^. #timeZone) $ fromPlutusTime claimExpiration
                , showLocalTime (config ^. #timeZone) $ fromPlutusTime claimExpiration
                ]
            ]
          inspectEvt = AftermarketEvent $ AftermarketSellerEvent $ InspectAftermarketSellerBid u
          claimEvt = AftermarketEvent $ AftermarketSellerEvent $ AddSelectedBidUnlock u
          canClaim = toPlutusTime currentTime > claimExpiration
          claimTip
            | not canClaim = "The buyer still has time to claim the NFTs"
            | otherwise = "Re-claim NFTs"
      hstack
        [ vstack
            [ hstack
                [ label "Accepted Claim Bid"
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
                , flip styleBasic [textSize 10] $ tooltip_ "Inspect Batch" [tooltipDelay 0] $
                    box_ [alignMiddle, onClick inspectEvt] $
                      label inspectIcon
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
                , widgetIf (claimExpiration <= toPlutusTime currentTime) $
                    hstack
                      [ filler
                      , label "Expired"
                          `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
                      , filler
                      ]
                , filler
                , label (show numNfts <> " " <> keyTypeLabel)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ box_ [alignTop] $ label "Bid:"
                    `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
                , spacer_ [width 2]
                , vstack_ [childSpacing_ 3] $ for (groupInto 4 prices) $ 
                    \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , flip styleBasic [padding 3] $ box_ [alignCenter,alignMiddle] $ vstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ claimTip [tooltipDelay 0] $
                button claimCollateralIcon claimEvt
                  `styleBasic` 
                    [ textSize 10
                    , textColor $ if canClaim then customBlue else customRed
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                  `nodeEnabled` canClaim
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

claimBidAcceptanceWidget :: AppModel -> AppNode
claimBidAcceptanceWidget AppModel{knownWallets} = do
  let maybeLens' = maybeLens def (#aftermarketModel % #sellerModel % #newClaimBidAcceptance)
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
        [ centerWidgetH $ label "Where would you like the bid payment to go?"
        , spacer_ [width 20]
        , centerWidgetH $ hstack
            [ label "Payment Address:"
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
            , spacer_ [width 3]
            , helpButton claimBidPaymentAddressMsg
            ]
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ AftermarketEvent $ AftermarketSellerEvent $ AddSelectedClaimBidAcceptance CancelAdding
            , spacer
            , mainButton "Confirm" $ AftermarketEvent $ AftermarketSellerEvent $ AddSelectedClaimBidAcceptance ConfirmAdding
            ]
        ] `styleBasic` [radius 20, bgColor customGray3, padding 20]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

bidsFilterWidget :: AppModel -> AppNode
bidsFilterWidget model@AppModel{aftermarketModel=AftermarketModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #aftermarketModel % #sellerModel
      filterScene = sellerModel ^. #bidsFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #bidsFilterModel % #scene) 
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
                    (toLensVL $ rootLens % #bidsFilterModel % #scene) 
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
                    , button "Reset" $ AftermarketEvent $ AftermarketSellerEvent ResetCurrentBidsFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showBidFilter)
                        [onClick $ AftermarketEvent $ AftermarketSellerEvent CheckCurrentBidsFilters]
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
      let rootLens = #aftermarketModel % #sellerModel % #bidsFilterModel
          offStyle = def 
            `styleBasic` [ bgColor customGray4 , textColor white, border 0 transparent]
            `styleHover` [ bgColor customBlue ]
          choiceButton caption field targetLens =
            optionButton_ caption field targetLens
              [optionButtonOffStyle offStyle]
              `styleBasic` 
                [ bgColor customBlue
                , border 0 transparent
                , textColor white
                , radius 0
                , textSize 12
                ]
      cushionWidgetH $ vstack
        [ spacer
        , centerWidgetH $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , hstack
            [ spacer
            , spacer_ [width 3]
            , label "Bid Type:"
                `styleBasic` [textSize 12]
            , spacer_ [width 10]
            , hgrid_ [childSpacing_ 3]
                [ choiceButton "SpotBid" (Just SpotBid) (toLensVL $ rootLens % #bidType)
                , choiceButton "ClaimBid" (Just ClaimBid) (toLensVL $ rootLens % #bidType)
                , choiceButton "AcceptedBid" (Just AcceptedBid) (toLensVL $ rootLens % #bidType)
                , choiceButton "Any" Nothing (toLensVL $ rootLens % #bidType)
                ]
            ]
        , spacer
        , hstack
            [ box_ [alignMiddle, onClick $ Alert nftTypeFilterMsg] $
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
            , label "NFT Type:"
                `styleBasic` [textSize 10]
            , spacer_ [width 10]
            , hgrid_ [childSpacing_ 3]
                [ choiceButton "Loan Keys" (Just LoanKey)
                    (toLensVL $ rootLens % #nftType)
                , choiceButton "Options Keys" (Just OptionsKey)
                    (toLensVL $ rootLens % #nftType)
                , choiceButton "Other NFTs" (Just OtherNft) (toLensVL $ rootLens % #nftType)
                , choiceButton "Any" Nothing (toLensVL $ rootLens % #nftType)
                ]
            ]
          , spacer
          , hstack
              [ spacer_ [width 50]
              , label "Policy Id:"
                  `styleBasic` [textSize 10]
              , spacer
              , textField (toLensVL $ rootLens % #policyId) 
                  `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                  `styleFocus` [border 1 customBlue]
              ] `nodeVisible` (model ^. rootLens % #nftType == Just OtherNft)
        ]

    sortWidget :: AppNode
    sortWidget = do
      let innerDormantStyle = 
            def `styleBasic` [textSize 12, bgColor customGray2, border 1 black]
                `styleHover` [textSize 12, bgColor customGray1, border 1 black]
          innerFocusedStyle = 
            def `styleFocus` [textSize 12, bgColor customGray2, border 1 customBlue]
                `styleFocusHover` [textSize 12, bgColor customGray1, border 1 customBlue]
          possibleSortingMethods = enumFrom CurrentBidsLexicographically
          rootLens = #aftermarketModel % #sellerModel % #bidsFilterModel
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
            , box_ [onClick $ Alert bidFilterSortMsg] $
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
-- | The bid is not expired. This only checks for ClaimBidDatum's bidExpiration.
isNotExpiredBid :: POSIXTime -> AftermarketUTxO -> Bool
isNotExpiredBid currentTime utxo = 
  maybe True (> toPlutusTime currentTime) $ aftermarketUTxOBidExpiration utxo

orderer :: SortDirection -> [AftermarketUTxO] -> [AftermarketUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: CurrentBidsFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
sorter CurrentBidsFilterModel{..} = 
  case sortingMethod of
    CurrentBidsLexicographically -> sortOn (view #utxoRef)
    CurrentBidsTime -> sortOn (view #blockTime)
    CurrentBidsNftCount -> sortOn (aftermarketUTxONfts >=> return . length . snd)
    CurrentBidsExpiration -> 
      case sortingDirection of
        -- Nothing should always appear last.
        SortAscending -> sortOn (NothingLast . aftermarketUTxOExpiration)
        SortDescending -> sortOn aftermarketUTxOExpiration

filterer :: CurrentBidsFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
filterer CurrentBidsFilterModel{policyId,bidType,nftType} us = do
    u <- us
    let (actualPolicyId,_) = fromMaybe ("",def) $ aftermarketUTxONfts u
    guard $ matchesPolicyId actualPolicyId
    guard $ maybe True matchesBidType $ u ^. #marketDatum
    return u
  where
    matchesPolicyId :: CurrencySymbol -> Bool
    matchesPolicyId actualPolicyId
      | nftType == Just LoanKey = actualPolicyId == Loans.activeBeaconCurrencySymbol
      | nftType == Just OptionsKey = actualPolicyId == Options.activeBeaconCurrencySymbol
      | nftType == Just OtherNft = display actualPolicyId == policyId
      | otherwise = True

    matchesBidType :: AftermarketDatum -> Bool
    matchesBidType marketDatum = case bidType of
      Nothing -> True
      Just SpotBid -> isJust $ marketDatum ^? _SpotBidDatum
      Just ClaimBid -> isJust $ marketDatum ^? _ClaimBidDatum
      Just AcceptedBid -> isJust $ marketDatum ^? _AcceptedBidDatum

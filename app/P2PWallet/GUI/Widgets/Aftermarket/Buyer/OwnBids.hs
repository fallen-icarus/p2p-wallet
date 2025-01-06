module P2PWallet.GUI.Widgets.Aftermarket.Buyer.OwnBids
  ( ownBidsWidget
  ) where

import Monomer as M hiding (duration)
import Data.Map.Strict qualified as Map

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

ownBidsWidget :: AppModel -> AppNode
ownBidsWidget model@AppModel{scene=_,..} =
    zstack
      [ mainWidget
      , updateBidWidget model `nodeVisible` and
          [ isJust (buyerModel ^. #newBidUpdate)
          -- Hide until after syncing is complete.
          , not $ model ^. #waitingStatus % #syncingLoanHistories
          , not $ model ^. #waitingStatus % #syncingOptionsContracts
          ]
      , bidsFilterWidget model `nodeVisible` (buyerModel ^. #showBidFilter)
      , loanBidClaimAddressUpdateWidget model
          `nodeVisible` and
            [ isJust $ buyerModel ^. #newLoanKeyBidClaim
            , buyerModel ^. #showBidClaimLenderAddressWidget
            ]
      , optionsAcceptedClaimExecutionWidget model
          `nodeVisible` isJust (aftermarketModel ^. #buyerModel % #newOptionsKeyAcceptedBidClaim)
      ]
  where
    AftermarketModel{..} = aftermarketModel

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]
  
    MarketWallet{..} = selectedWallet

    Config{currentTime} = config

    LendingModel{cachedLoanHistories} = lendingModel

    OptionsModel{cachedKeyContracts} = optionsModel

    fractionShown :: Text
    fractionShown = show (length sample) <> "/" <> show (length bidUTxOs)

    sample :: [AftermarketUTxO]
    sample = orderer (buyerModel ^. #bidsFilterModel % #sortingDirection) 
           $ sorter (buyerModel ^. #bidsFilterModel) 
           $ filterer (buyerModel ^. #bidsFilterModel) bidUTxOs

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ hstack 
            [ filler
            , label ("Bids (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , spacer_ [width 2]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #aftermarketModel % #buyerModel % #showBidFilter)
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
          inspectEvt = AftermarketEvent $ AftermarketBuyerEvent $ InspectAftermarketBid u
          updateEvt = AftermarketEvent 
                    $ AftermarketBuyerEvent 
                    $ AddSelectedBidUpdate 
                    $ StartAdding 
                    $ Just u
          lookupEvt = AftermarketEvent 
                    $ AftermarketBuyerEvent
                    $ InspectSellerInformation
                    $ u ^. #marketAddress
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
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ "Lookup Seller Information" [tooltipDelay 0] $
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
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon updateEvt
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
                button closeCircleIcon (AftermarketEvent $ AftermarketBuyerEvent $ AddSelectedBidClose u)
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
          inspectEvt = AftermarketEvent $ AftermarketBuyerEvent $ InspectAftermarketBid u
          updateEvt = AftermarketEvent 
                    $ AftermarketBuyerEvent 
                    $ AddSelectedBidUpdate 
                    $ StartAdding 
                    $ Just u
          lookupEvt = AftermarketEvent 
                    $ AftermarketBuyerEvent
                    $ InspectSellerInformation
                    $ u ^. #marketAddress
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
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ 
                    tooltip_ "Lookup Seller Information" [tooltipDelay 0] $
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
                , filler
                , label "Deposit:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 2]
                , label (display $ Lovelace bidDeposit)
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
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit" [tooltipDelay 0] $
                button editIcon updateEvt
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
                button closeCircleIcon (AftermarketEvent $ AftermarketBuyerEvent $ AddSelectedBidClose u)
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

    acceptedBidRow :: AftermarketUTxO -> Aftermarket.AcceptedBidDatum -> AppNode
    acceptedBidRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.AcceptedBidDatum{..} = do
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
          nftType
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = LoanKey
            | nftPolicyId == Options.activeBeaconCurrencySymbol = OptionsKey
            | otherwise = OtherNft
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
          mLoanUTxOs = 
            sequence $ for nftNames (maybe Nothing snd . flip Map.lookup cachedLoanHistories . Loans.LoanId)
          mOptionsUTxOs = 
            sequence $ for nftNames (fromMaybe Nothing . flip Map.lookup cachedKeyContracts . Options.ContractId)
          mPaymentWallet = maybeHead $ knownWallets ^. #paymentWallets
          inspectEvt = AftermarketEvent $ AftermarketBuyerEvent $ InspectAftermarketBid u
          claimEvt
            | nftType == LoanKey = AftermarketEvent 
                                 $ AftermarketBuyerEvent 
                                 $ ClaimAcceptedLoanKeyBid 
                                 $ StartAdding 
                                 $ Just (u, fromMaybe [] mLoanUTxOs)
            | nftType == OptionsKey = AftermarketEvent 
                                    $ AftermarketBuyerEvent 
                                    $ ClaimAcceptedOptionsKeyBid 
                                    $ StartAdding 
                                    $ Just (u, fromMaybe [] mOptionsUTxOs)
            | otherwise = AppInit
          loanClaimTip
            | isNothing mLoanUTxOs = "Inspect the batch before claiming"
            | isNothing mPaymentWallet = "Add a payment wallet under the 'Home' page to enable claiming"
            | otherwise = "Claim Loan Keys"
          optionsClaimTip
            | isNothing mOptionsUTxOs = "Inspect the batch before claiming"
            | otherwise = "Claim Options Keys"
          claimTip
            | nftType == LoanKey = loanClaimTip
            | nftType == OptionsKey = optionsClaimTip
            | otherwise = "soon"
          canClaimLoans = isJust mPaymentWallet && isJust mLoanUTxOs
          canClaimOptions = isJust mOptionsUTxOs
          canClaim
            | nftType == LoanKey = canClaimLoans
            | nftType == OptionsKey = canClaimOptions
            | otherwise = False
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
                , filler
                , label "Deposit:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 2]
                , label (display $ Lovelace bidDeposit)
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
            [ box_ [alignCenter,alignMiddle] $ tooltip_ claimTip [tooltipDelay 0] $
                button swapIcon claimEvt
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

updateBidWidget :: AppModel -> AppNode
updateBidWidget model = do
  let maybeLens' = maybeLens (def,def) (#aftermarketModel % #buyerModel % #newBidUpdate)
      NewBidCreation{isSpotBid,nfts} = maybe def snd $ model ^? maybeLens'
      keyType = maybe "" (view #policyId) $ maybeHead nfts
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  vstack
    [ centerWidget $ vscroll_ [wheelRate 50] $ vstack
        [ hgrid
            [ optionButton_ "Spot Bid" True (toLensVL $ maybeLens' % _2 % #isSpotBid) 
                [optionButtonOffStyle offStyle]
                `styleBasic` 
                  [ bgColor customGray3
                  , textColor customBlue
                  , radiusTL 10
                  , radiusTR 0
                  , radiusBR 0
                  , radiusBL 0
                  , border 0 transparent
                  ]
                `nodeVisible` (keyType /= Loans.activeBeaconCurrencySymbol)
            , optionButton_ "Claim Bid" False (toLensVL $ maybeLens' % _2 % #isSpotBid)
                [optionButtonOffStyle offStyle]
                `styleBasic` 
                  [ bgColor customGray3
                  , textColor customBlue
                  , radiusTL 0
                  , radiusTR 10
                  , radiusBR 0
                  , radiusBL 0
                  , border 0 transparent
                  , styleIf (keyType == Loans.activeBeaconCurrencySymbol) $ radiusTL 10
                  ]
            ]
        , zstack 
            [ widgetIf isSpotBid $ centerWidgetH $ updateSpotBidWidget model
            , widgetIf (not isSpotBid) $ centerWidgetH $ updateClaimBidWidget model
            ] `styleBasic` 
                [ bgColor customGray3
                , paddingT 0
                , paddingL 20
                , paddingR 20
                , paddingB 20
                , radiusTL 0
                , radiusTR 0
                , radiusBL 10
                , radiusBR 10
                ]
        ] 
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

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

updateSpotBidWidget :: AppModel -> AppNode
updateSpotBidWidget model@AppModel{..} = do
  let maybeLens' = maybeLens (def,def) (#aftermarketModel % #buyerModel % #newBidUpdate)
      NewBidCreation{nfts,showNfts} = maybe def snd $ model ^? maybeLens'
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
      removeEvt = AftermarketEvent . AftermarketBuyerEvent . RemoveBuyerBidUpdateNft
  vstack
    [ centerWidgetH $ hstack
        [ label "What is a spot bid?"
            `styleBasic` [textFont "Italics"]
        , spacer_ [width 3]
        , helpButton whatIsSpotBidMsg
        ]
    , spacer
    , hstack
        [ label "NFTs: "
            `styleBasic` [textSize 12]
        , spacer_ [width 5]
        , toggleButton_ horizontalMoreIcon 
            (toLensVL $ maybeLens' % _2 % #showNfts)
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
        ]
    , widgetIf showNfts $
        vstack_ [childSpacing] (map (nftsRow (length nfts > 1) removeEvt model) nfts)
          `styleBasic` [padding 10]
    , spacer
    , hstack
        [ label "Bidder Credential:"
            `styleBasic` [textSize 10]
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % _2 % #marketWallet) 
              (knownWallets ^. #marketWallets) 
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
        , helpButton bidderCredentialMsg
        ]
    , spacer
    , hstack
        [ label "Payment Address:"
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
        , spacer_ [width 3]
        , helpButton bidderPaymentAddressMsg
        ]
    , spacer
    , hstack
        [ label "Bid Price (assets separated with newlines):"
            `styleBasic` [textSize 10]
        , spacer_ [width 3]
        , helpButton bidPriceMsg
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #bid)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (AftermarketEvent $ AftermarketBuyerEvent $ AddSelectedBidUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Update" 
              (AftermarketEvent $ AftermarketBuyerEvent $ AddSelectedBidUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

updateClaimBidWidget :: AppModel -> AppNode
updateClaimBidWidget model@AppModel{..} = do
  let maybeLens' = maybeLens (def,def) (#aftermarketModel % #buyerModel % #newBidUpdate)
      NewBidCreation{nfts,showNfts} = maybe def snd $ model ^? maybeLens'
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
      removeEvt = AftermarketEvent . AftermarketBuyerEvent . RemoveBuyerBidUpdateNft
  vstack
    [ centerWidgetH $ hstack
        [ label "What is a claim bid?"
            `styleBasic` [textFont "Italics"]
        , spacer_ [width 3]
        , helpButton whatIsClaimBidMsg
        ]
    , spacer
    , hstack
        [ label "NFTs: "
            `styleBasic` [textSize 12]
        , spacer_ [width 5]
        , toggleButton_ horizontalMoreIcon 
            (toLensVL $ maybeLens' % _2 % #showNfts)
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
        ]
    , widgetIf showNfts $
        vstack_ [childSpacing] (map (nftsRow (length nfts > 1) removeEvt model) nfts)
          `styleBasic` [padding 10]
    , spacer
    , hstack
        [ label "Bidder Credential:"
            `styleBasic` [textSize 10]
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % _2 % #marketWallet) 
              (knownWallets ^. #marketWallets) 
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
        , helpButton bidderCredentialMsg
        ]
    , spacer
    , hstack
        [ label "Bid Expiration:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #bidExpiration)
            [placeholder "MM/DD/YY"]
            `styleBasic` [width 200, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , helpButton bidExpirationMsg
        ]
    , spacer
    , hstack
        [ label "Claim Expiration:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #claimExpiration)
            [placeholder "MM/DD/YY"]
            `styleBasic` [width 200, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , helpButton claimExpirationMsg
        ]
    , spacer
    , hstack
        [ label "Bid Deposit:"
            `styleBasic` [textSize 10]
        , spacer
        , textField_ (toLensVL $ maybeLens' % _2 % #deposit)
            [placeholder "10.0"]
            `styleBasic` [width 200, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , helpButton bidDepositMsg
        ]
    , spacer
    , hstack
        [ label "Bid Price (assets separated with newlines):"
            `styleBasic` [textSize 10]
        , spacer_ [width 3]
        , helpButton bidPriceMsg
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #bid)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (AftermarketEvent $ AftermarketBuyerEvent $ AddSelectedBidUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Update" 
              (AftermarketEvent $ AftermarketBuyerEvent $ AddSelectedBidUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

loanBidClaimAddressUpdateWidget :: AppModel -> AppNode
loanBidClaimAddressUpdateWidget AppModel{knownWallets} = do
  let maybeLens' = maybeLens def (#aftermarketModel % #buyerModel % #newLoanKeyBidClaim)
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
  vstack
    [ centerWidget $ vstack
        [ centerWidgetH $ label "Where would you like future loan payments to go for these loans?"
        , spacer_ [width 20]
        , centerWidgetH $ hstack
            [ label "Address:"
            , spacer
            , textDropdown_ 
                  (toLensVL $ maybeLens' % #newPaymentWallet) 
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
            [ filler
            , button "Cancel" $ AftermarketEvent $ AftermarketBuyerEvent $ ClaimAcceptedLoanKeyBid CancelAdding
            , spacer
            , mainButton "Confirm" $ AftermarketEvent $ AftermarketBuyerEvent $ ClaimAcceptedLoanKeyBid ConfirmAdding
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
      rootLens = #aftermarketModel % #buyerModel
      filterScene = buyerModel ^. #bidsFilterModel % #scene
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
                    , button "Reset" $ AftermarketEvent $ AftermarketBuyerEvent ResetOwnBidsFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showBidFilter)
                        [onClick $ AftermarketEvent $ AftermarketBuyerEvent CheckOwnBidsFilters]
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
      let rootLens = #aftermarketModel % #buyerModel % #bidsFilterModel
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
          possibleSortingMethods = enumFrom OwnBidsLexicographically
          rootLens = #aftermarketModel % #buyerModel % #bidsFilterModel
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

optionsAcceptedClaimExecutionWidget :: AppModel -> AppNode
optionsAcceptedClaimExecutionWidget model = do
  let maybeLens' = maybeLens def (#aftermarketModel % #buyerModel % #newOptionsKeyAcceptedBidClaim)
      contracts = model ^. maybeLens' % #contracts
  vstack
    [ centerWidget $ vstack
        [ centerWidgetH $ label "Which contracts would you like to immediately execute?"
        , spacer_ [width 20]
        , vscroll_ [wheelRate 50] $
            vstack_ [childSpacing] $ 
              zipWith (curry $ executeOptionsWidget model maybeLens') [0..] contracts
        , spacer
        , hstack 
            [ filler
            , button "Cancel" $ AftermarketEvent $ AftermarketBuyerEvent $ ClaimAcceptedOptionsKeyBid CancelAdding
            , spacer
            , mainButton "Confirm" $ AftermarketEvent $ AftermarketBuyerEvent $ ClaimAcceptedOptionsKeyBid ConfirmAdding
            ]
        ] `styleBasic` [radius 20, bgColor customGray3, padding 20]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

executeOptionsWidget 
  :: AppModel
  -> Lens' AppModel NewOptionsKeyAcceptedBidClaim
  -> (Int,(Bool,OptionsUTxO)) 
  -> AppNode
executeOptionsWidget AppModel{..} maybeLens' (idx,(_,u@OptionsUTxO{..})) = do
  let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum u
      offerAmount = toNativeAsset offerAsset & #quantity .~ offerQuantity
      askNativeAsset = toNativeAsset askAsset
      payToAddress = either (const "error") fst $ plutusToBech32 (config ^. #network) paymentAddress
      mTargetWallet = find ((==payToAddress) . view #paymentAddress) 
                    $ knownWallets ^. #paymentWallets
      addressTip = case mTargetWallet of
        Nothing -> "Payments to: " <> display payToAddress
        Just w -> unwords
          [ "Payments to"
          , w ^. #alias <> ":"
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
        , showLocalDate (config ^. #timeZone) $ fromPlutusTime expiration
        , showLocalTime (config ^. #timeZone) $ fromPlutusTime expiration
        ]

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
                tooltip_ ("Options Contract ID: " <> display contractId) [tooltipDelay 0] $
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
            , filler
            , label ("Ask Asset: " <> showAssetNameOnly reverseTickerMap askNativeAsset)
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
    , spacer
    , checkbox_ (toLensVL $ maybeLens' % #contracts % toggleExecution idx) [checkboxSquare]
        `styleBasic` [fgColor customGray1, hlColor customBlue]
    ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
orderer :: SortDirection -> [AftermarketUTxO] -> [AftermarketUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: OwnBidsFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
sorter OwnBidsFilterModel{..} = 
  case sortingMethod of
    OwnBidsLexicographically -> sortOn (view #utxoRef)
    OwnBidsTime -> sortOn (view #blockTime)
    OwnBidsNftCount -> sortOn (aftermarketUTxONfts >=> return . length . snd)
    OwnBidsExpiration -> 
      case sortingDirection of
        -- Nothing should always appear last.
        SortAscending -> sortOn (NothingLast . aftermarketUTxOExpiration)
        SortDescending -> sortOn aftermarketUTxOExpiration

filterer :: OwnBidsFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
filterer OwnBidsFilterModel{policyId,bidType,nftType} us = do
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

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
-- | A lens to toggle the `Bool` field of the NewOptionsKeyAcceptedBidClaim contracts list.
toggleExecution :: Int -> Lens' [(Bool,OptionsUTxO)] Bool
toggleExecution idx = lens getToggleExecution setToggleExecution
  where
    getToggleExecution :: [(Bool,OptionsUTxO)] -> Bool
    getToggleExecution us = fromMaybe False $ us ^? ix idx % _1

    setToggleExecution :: [(Bool,OptionsUTxO)] -> Bool -> [(Bool,OptionsUTxO)]
    setToggleExecution us b = us & ix idx % _1 .~ b

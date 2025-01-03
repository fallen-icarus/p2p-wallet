module P2PWallet.GUI.Widgets.Aftermarket.Buyer.Aftermarket
  ( aftermarketWidget
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

aftermarketWidget :: AppModel -> AppNode
aftermarketWidget model@AppModel{aftermarketModel} =
    zstack
      [ vstack
          [ getPolicyId model
          , filler
          , filler
          ] `nodeVisible` (isNothing selectedPolicyId || choosingPolicyId)
      , allSalesWidget model
          `nodeVisible` and
            [ isJust selectedPolicyId
            , not choosingPolicyId
            , not $ model ^. #waitingStatus % #syncingAftermarketSales
            ]
      , salesFilterWidget model `nodeVisible` showSaleFilter
      , createBidWidget model `nodeVisible` isJust newBidCreation
      , loanSpotPurchaseAddressUpdateWidget model
          `nodeVisible` and
            [ isJust newLoanKeySpotPurchase
            , showSpotPurchaseLenderAddressWidget
            ]
      , optionsSpotPurchaseExecutionWidget model
          `nodeVisible` isJust newOptionsKeySpotPurchase
      ]
  where
    AftermarketBuyerModel{..} = aftermarketModel ^. #buyerModel

getPolicyId :: AppModel -> AppNode
getPolicyId model = do
  let rootLens = #aftermarketModel % #buyerModel
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
  centerWidget $ vstack
    [ centerWidgetH $ label "Which aftermarket would you like to lookup?"
    , spacer_ [width 20]
    , centerWidgetH $ hgrid_ [childSpacing_ 3]
        [ choiceButton "Loans" LoanKey (toLensVL $ rootLens % #nftType)
        , choiceButton "Options" OptionsKey (toLensVL $ rootLens % #nftType)
        , choiceButton "Other NFT" OtherNft (toLensVL $ rootLens % #nftType)
        ]
    , spacer
    , widgetIf (model ^. rootLens % #nftType == OtherNft) $ 
        centerWidgetH $ hstack
          [ label "Policy Id:"
              `styleBasic` [textSize 10]
          , spacer
          , textField (toLensVL $ rootLens % #newPolicyId) 
              `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
              `styleFocus` [border 1 customBlue]
          ] 
    , spacer
    , hstack 
        [ filler
        , button "Cancel" (AftermarketEvent $ AftermarketBuyerEvent $ SetNewSalePolicyId CancelAdding)
            `nodeVisible` isJust (model ^. #aftermarketModel % #buyerModel % #selectedPolicyId)
        , spacer
        , mainButton "Confirm" $ AftermarketEvent $ AftermarketBuyerEvent $ SetNewSalePolicyId ConfirmAdding
        ]
    ] `styleBasic` [bgColor customGray3, padding 20, radius 10]

allSalesWidget :: AppModel -> AppNode
allSalesWidget AppModel{aftermarketModel=AftermarketModel{..},scene=_,..} =
    cushionWidgetH $ vstack
      [ centerWidgetH $ hstack 
          [ tooltip_ "Resync Aftermarket" [tooltipDelay 0] $
              box_ [alignMiddle , onClick resyncEvt] $
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
          , spacer_ [width 2]
          , tooltip_ "Change Aftermarket" [tooltipDelay 0] $
              box_ [onClick changeMarketEvt] $ hstack
                [ label (header <> " (" <> fractionShown <> ")")
                    `styleBasic` [textFont "Italics", textSize 14]
                ] `styleBasic` [padding 5 , radius 5, border 1 customBlue]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
          , spacer_ [width 2]
          , tooltip_ "Sort/Filter" [tooltipDelay 0] $
              toggleButton_ menuSearchIcon
                (toLensVL $ #aftermarketModel % #buyerModel % #showSaleFilter)
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
          vstack_ [childSpacing] (map saleRow sample)
            `styleBasic` [padding 10]
      , filler
      ] 
  where
    changeMarketEvt = AftermarketEvent 
                    $ AftermarketBuyerEvent 
                    $ SetNewSalePolicyId 
                    $ StartAdding Nothing

    resyncEvt = AftermarketEvent $ SyncAftermarketSales $ StartProcess selectedPolicyId

    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    Config{network} = config
  
    AftermarketBuyerModel{scene=_,..} = buyerModel

    LendingModel{cachedLoanHistories} = lendingModel

    OptionsModel{cachedKeyContracts} = optionsModel

    allSales :: [AftermarketUTxO]
    allSales = fromMaybe [] $ selectedPolicyId >>= flip Map.lookup cachedSales

    fractionShown :: Text
    fractionShown = show (length sample) <> "/" <> show (length allSales)

    sample :: [AftermarketUTxO]
    sample = orderer (buyerModel ^. #salesFilterModel % #sortingDirection) 
           $ sorter (buyerModel ^. #salesFilterModel) 
           $ filterer (buyerModel ^. #salesFilterModel) allSales
    
    header
      | nftType == LoanKey = "Loan Keys For Sale"
      | nftType == OptionsKey = "Options Keys For Sale"
      | otherwise = maybe "" display selectedPolicyId <> " For Sale"

    saleRow :: AftermarketUTxO -> AppNode
    saleRow u@AftermarketUTxO{marketDatum} = case marketDatum of
      (Just (SpotDatum spotDatum)) -> spotRow u spotDatum
      (Just (AuctionDatum auctionDatum)) -> auctionRow u auctionDatum
      _ -> spacer

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

    spotRow :: AftermarketUTxO -> Aftermarket.SpotDatum -> AppNode
    spotRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.SpotDatum{..} = do
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
          prices = map toNativeAsset $ salePrice ^. #unPrices
          inspectEvt = AftermarketEvent $ AftermarketBuyerEvent $ InspectAftermarketBuyerSale u
          lookupEvt = AftermarketEvent 
                    $ AftermarketBuyerEvent
                    $ InspectSellerInformation
                    $ u ^. #marketAddress
          mLoanUTxOs = 
            sequence $ for nftNames (maybe Nothing snd . flip Map.lookup cachedLoanHistories . Loans.LoanId)
          mOptionsUTxOs = 
            sequence $ for nftNames (fromMaybe Nothing . flip Map.lookup cachedKeyContracts . Options.ContractId)
          mPaymentWallet = maybeHead $ knownWallets ^. #paymentWallets
          buyEvt
            | nftType == LoanKey = AftermarketEvent 
                                 $ AftermarketBuyerEvent 
                                 $ PurchaseLoanKeySpot 
                                 $ StartAdding 
                                 $ Just (u, fromMaybe [] mLoanUTxOs)
            | nftType == OptionsKey = AftermarketEvent 
                                    $ AftermarketBuyerEvent 
                                    $ PurchaseOptionsKeySpot 
                                    $ StartAdding 
                                    $ Just (u, fromMaybe [] mOptionsUTxOs)
            | otherwise = AppInit
          counterOfferEvt = AftermarketEvent
                          $ AftermarketBuyerEvent
                          $ CreateBid
                          $ StartAdding 
                          $ Just u
          loanBuyTip
            | isNothing mLoanUTxOs = "Inspect the batch before purchasing"
            | isNothing mPaymentWallet = "Add a payment wallet under the 'Home' page to enable purchases"
            | otherwise = "Purchase Loan Keys"
          optionsBuyTip
            | isNothing mOptionsUTxOs = "Inspect the batch before purchasing"
            | otherwise = "Purchase Options Keys"
          buyTip
            | nftType == LoanKey = loanBuyTip
            | nftType == OptionsKey = optionsBuyTip
            | otherwise = "soon"
          canBuyLoans = isJust mPaymentWallet && isJust mLoanUTxOs
          canBuyOptions = isJust mOptionsUTxOs
          canBuy
            | nftType == LoanKey = canBuyLoans
            | nftType == OptionsKey = canBuyOptions
            | otherwise = False
          loanCounterTip
            | isNothing mLoanUTxOs = "Inspect the batch before creating counter offer"
            | isNothing mPaymentWallet = "Add a payment wallet under the 'Home' page to enable counter offers"
            | otherwise = "Create counter offer"
          optionsCounterTip
            | isNothing mOptionsUTxOs = "Inspect the batch before creating counter offer"
            | isNothing mPaymentWallet = "Add a payment wallet under the 'Home' page to enable counter offers"
            | otherwise = "Create counter offer"
          counterTip
            | nftType == LoanKey = loanCounterTip
            | nftType == OptionsKey = optionsCounterTip
            | otherwise = "soon"
          canCounterLoans = isJust mPaymentWallet && isJust mLoanUTxOs
          canCounterOptions = isJust mPaymentWallet && isJust mOptionsUTxOs
          canCounter
            | nftType == LoanKey = canCounterLoans
            | nftType == OptionsKey = canCounterOptions
            | otherwise = False
      hstack
        [ vstack
            [ hstack
                [ label "Spot"
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
                [ box_ [alignTop] $ label "Sale Price:"
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
            [ box_ [alignCenter,alignMiddle] $ tooltip_ buyTip [tooltipDelay 0] $
                button swapIcon buyEvt
                  `styleBasic` 
                    [ textSize 10
                    , textColor $ if canBuy then customBlue else customRed
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                  `nodeEnabled` canBuy
            , spacer_ [width 2]
            , separatorLine `styleBasic` [fgColor darkGray, paddingL 5, paddingR 5]
            , spacer_ [width 2]
            , box_ [alignCenter,alignMiddle] $ tooltip_ counterTip [tooltipDelay 0] $
                button counterOfferIcon counterOfferEvt
                  `styleBasic` 
                    [ textSize 10
                    , textColor $ if canCounter then customBlue else customRed
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                  `nodeEnabled` canCounter
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]
        
    auctionRow :: AftermarketUTxO -> Aftermarket.AuctionDatum -> AppNode
    auctionRow u@AftermarketUTxO{utxoRef,blockTime} Aftermarket.AuctionDatum{..} = do
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
          prices = map toNativeAsset $ startingPrice ^. #unPrices
          inspectEvt = AftermarketEvent $ AftermarketBuyerEvent $ InspectAftermarketBuyerSale u
          lookupEvt = AftermarketEvent 
                    $ AftermarketBuyerEvent
                    $ InspectSellerInformation
                    $ u ^. #marketAddress
          mPaymentWallet = maybeHead $ knownWallets ^. #paymentWallets
          mLoanUTxOs = 
            sequence $ for nftNames (maybe Nothing snd . flip Map.lookup cachedLoanHistories . Loans.LoanId)
          mOptionsUTxOs = 
            sequence $ for nftNames (fromMaybe Nothing . flip Map.lookup cachedKeyContracts . Options.ContractId)
          loanBidTip
            | isNothing mLoanUTxOs = "Inspect the batch before creating a bid"
            | isNothing mPaymentWallet = "Add a payment wallet under the 'Home' page to enable bids"
            | otherwise = "Create bid"
          optionsBidTip
            | isNothing mOptionsUTxOs = "Inspect the batch before creating a bid"
            | isNothing mPaymentWallet = "Add a payment wallet under the 'Home' page to enable bids"
            | otherwise = "Create bid"
          bidTip
            | nftType == LoanKey = loanBidTip
            | nftType == OptionsKey = optionsBidTip
            | otherwise = "soon"
          canBidLoans = isJust mPaymentWallet && isJust mLoanUTxOs
          canBidOptions = isJust mPaymentWallet && isJust mOptionsUTxOs
          canBid
            | nftType == LoanKey = canBidLoans
            | nftType == OptionsKey = canBidOptions
            | otherwise = False
          createBidEvt = AftermarketEvent
                       $ AftermarketBuyerEvent
                       $ CreateBid
                       $ StartAdding 
                       $ Just u
      hstack
        [ vstack
            [ hstack
                [ label "Auction"
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
                [ box_ [alignTop] $ label "Starting Price:"
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
            [ box_ [alignCenter,alignMiddle] $ tooltip_ bidTip [tooltipDelay 0] $
                button makeOfferIcon createBidEvt
                  `styleBasic` 
                    [ textSize 10
                    , textColor $ if canBid then customBlue else customRed
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
                  `nodeEnabled` canBid
            ] `styleBasic`
                [ padding 3
                , radius 3
                , bgColor customGray2
                ]
        ]

salesFilterWidget :: AppModel -> AppNode
salesFilterWidget AppModel{aftermarketModel=AftermarketModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #aftermarketModel % #buyerModel
      filterScene = buyerModel ^. #salesFilterModel % #scene
  vstack
    [  centerWidget $ vstack
        [ hstack
            [ hgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ rootLens % #salesFilterModel % #scene) 
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
                    (toLensVL $ rootLens % #salesFilterModel % #scene) 
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
                    , button "Reset" $ AftermarketEvent $ AftermarketBuyerEvent ResetAllSalesFilters
                    , spacer
                    , toggleButton "Confirm" (toLensVL $ rootLens % #showSaleFilter)
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
      let rootLens = #aftermarketModel % #buyerModel % #salesFilterModel
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
        , centerWidgetH $ hstack
            [ label "Sale Type:"
                `styleBasic` [textSize 12]
            , spacer_ [width 10]
            , hgrid_ [childSpacing_ 3]
                [ choiceButton "Spot" (Just False) (toLensVL $ rootLens % #shouldBeAuction)
                , choiceButton "Auction" (Just True) (toLensVL $ rootLens % #shouldBeAuction)
                , choiceButton "Either" Nothing (toLensVL $ rootLens % #shouldBeAuction)
                ]
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
          possibleSortingMethods = enumFrom AllSalesLexicographically
          rootLens = #aftermarketModel % #buyerModel % #salesFilterModel
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
            , box_ [onClick $ Alert saleFilterSortMsg] $
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

loanSpotPurchaseAddressUpdateWidget :: AppModel -> AppNode
loanSpotPurchaseAddressUpdateWidget AppModel{knownWallets} = do
  let maybeLens' = maybeLens def (#aftermarketModel % #buyerModel % #newLoanKeySpotPurchase)
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
            , button "Cancel" $ AftermarketEvent $ AftermarketBuyerEvent $ PurchaseLoanKeySpot CancelAdding
            , spacer
            , mainButton "Confirm" $ AftermarketEvent $ AftermarketBuyerEvent $ PurchaseLoanKeySpot ConfirmAdding
            ]
        ] `styleBasic` [radius 20, bgColor customGray3, padding 20]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

createBidWidget :: AppModel -> AppNode
createBidWidget model = do
  let maybeLens' = maybeLens def (#aftermarketModel % #buyerModel % #newBidCreation)
      NewBidCreation{isSpotBid,nfts} = fromMaybe def $ model ^? maybeLens'
      keyType = maybe "" (view #policyId) $ maybeHead nfts
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  vstack
    [ centerWidget $ vscroll_ [wheelRate 50] $ vstack
        [ hgrid
            [ optionButton_ "Spot Bid" True (toLensVL $ maybeLens' % #isSpotBid) 
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
            , optionButton_ "Claim Bid" False (toLensVL $ maybeLens' % #isSpotBid)
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
            [ widgetIf isSpotBid $ centerWidgetH $ createSpotBidWidget model
            , widgetIf (not isSpotBid) $ centerWidgetH $ createClaimBidWidget model
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

createSpotBidWidget :: AppModel -> AppNode
createSpotBidWidget model@AppModel{..} = do
  let maybeLens' = maybeLens def (#aftermarketModel % #buyerModel % #newBidCreation)
      NewBidCreation{nfts,showNfts} = fromMaybe def $ model ^? maybeLens'
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
      removeEvt = AftermarketEvent . AftermarketBuyerEvent . RemoveBuyerBidCreationNft
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
            (toLensVL $ maybeLens' % #showNfts)
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
              (toLensVL $ maybeLens' % #marketWallet) 
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
    , textArea (toLensVL $ maybeLens' % #bid)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (AftermarketEvent $ AftermarketBuyerEvent $ CreateBid CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Create" 
              (AftermarketEvent $ AftermarketBuyerEvent $ CreateBid ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

createClaimBidWidget :: AppModel -> AppNode
createClaimBidWidget model@AppModel{..} = do
  let maybeLens' = maybeLens def (#aftermarketModel % #buyerModel % #newBidCreation)
      NewBidCreation{nfts,showNfts} = fromMaybe def $ model ^? maybeLens'
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
      removeEvt = AftermarketEvent . AftermarketBuyerEvent . RemoveBuyerBidCreationNft
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
            (toLensVL $ maybeLens' % #showNfts)
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
              (toLensVL $ maybeLens' % #marketWallet) 
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
        , textField_ (toLensVL $ maybeLens' % #bidExpiration)
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
        , textField_ (toLensVL $ maybeLens' % #claimExpiration)
            [placeholder "MM/DD/YY"]
            `styleBasic` [width 200, textSize 10, bgColor customGray1]
            `styleFocus` [border 1 customBlue]
        , spacer_ [width 3]
        , helpButton claimExpirationMsg
        ]
    , spacer
    , hstack
        [ label "Bid Price (assets separated with newlines):"
            `styleBasic` [textSize 10]
        , spacer_ [width 3]
        , helpButton bidPriceMsg
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % #bid)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (AftermarketEvent $ AftermarketBuyerEvent $ CreateBid CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Create" 
              (AftermarketEvent $ AftermarketBuyerEvent $ CreateBid ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

optionsSpotPurchaseExecutionWidget :: AppModel -> AppNode
optionsSpotPurchaseExecutionWidget model = do
  let maybeLens' = maybeLens def (#aftermarketModel % #buyerModel % #newOptionsKeySpotPurchase)
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
            , button "Cancel" $ AftermarketEvent $ AftermarketBuyerEvent $ PurchaseOptionsKeySpot CancelAdding
            , spacer
            , mainButton "Confirm" $ AftermarketEvent $ AftermarketBuyerEvent $ PurchaseOptionsKeySpot ConfirmAdding
            ]
        ] `styleBasic` [radius 20, bgColor customGray3, padding 20]
    ] `styleBasic` 
        [ bgColor $ black & #a .~ 0.4
        , padding 30
        , radius 10
        ]

executeOptionsWidget 
  :: AppModel
  -> Lens' AppModel NewOptionsKeySpotPurchase
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

sorter :: AllSalesFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
sorter AllSalesFilterModel{..} = 
  case sortingMethod of
    AllSalesLexicographically -> sortOn (view #utxoRef)
    AllSalesTime -> sortOn (view #blockTime)
    AllSalesNftCount -> sortOn (aftermarketUTxONfts >=> return . length . snd)

filterer :: AllSalesFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
filterer AllSalesFilterModel{shouldBeAuction} us = do
    u <- us
    guard $ maybe True matchesSaleType $ u ^. #marketDatum
    return u
  where
    matchesSaleType :: AftermarketDatum -> Bool
    matchesSaleType marketDatum
      | isNothing shouldBeAuction = True
      | otherwise = isJust (marketDatum ^? _AuctionDatum) == fromMaybe False shouldBeAuction

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
-- | A lens to toggle the `Bool` field of the NewOptionsKeySpotPurchase contracts list.
toggleExecution :: Int -> Lens' [(Bool,OptionsUTxO)] Bool
toggleExecution idx = lens getToggleExecution setToggleExecution
  where
    getToggleExecution :: [(Bool,OptionsUTxO)] -> Bool
    getToggleExecution us = fromMaybe False $ us ^? ix idx % _1

    setToggleExecution :: [(Bool,OptionsUTxO)] -> Bool -> [(Bool,OptionsUTxO)]
    setToggleExecution us b = us & ix idx % _1 .~ b

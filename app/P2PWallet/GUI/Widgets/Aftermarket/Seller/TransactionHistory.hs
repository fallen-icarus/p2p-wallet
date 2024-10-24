module P2PWallet.GUI.Widgets.Aftermarket.Seller.TransactionHistory 
  ( 
    transactionsWidget
  , txInspectionWidget
  ) where

import Monomer hiding (duration)
import Data.Time.Format qualified as Time

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Plutus
import P2PWallet.Prelude

transactionsWidget :: AppModel -> AppNode
transactionsWidget model@AppModel{aftermarketModel} = do
    zstack 
      [ noTransactions `nodeVisible` null transactions
      , mainWidget model `nodeVisible` (transactions /= [])
      ]
  where
    MarketWallet{transactions} = aftermarketModel ^. #selectedWallet 

    noTransactions :: AppNode
    noTransactions = centerWidget $
      label "This wallet does not have any transactions yet"
        `styleBasic` [textFont "Italics"]

mainWidget :: AppModel -> AppNode
mainWidget model@AppModel{aftermarketModel=AftermarketModel{..},config} =
    zstack
      [ cushionWidgetH $ vstack
          [ centerWidgetH $ hstack
              [ label shownDateRange
                  `styleBasic` 
                    [ padding 0
                    , textMiddle
                    , textFont "Italics"
                    ]
              , spacer_ [width 5]
              , tooltip_ "Filter/Search" [tooltipDelay 0] $
                  toggleButton_ menuSearchIcon
                    (toLensVL $ #aftermarketModel % #sellerModel % #showTransactionFilter)
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
          , widgetIf (sample /= []) $ flip styleBasic [padding 5] $ box $ vscroll_ [wheelRate 50] $ 
              vstack_ [childSpacing] (map txRow sample)
                `styleBasic` [padding 10]
          , widgetIf (null sample) $ 
              centerWidget $
                label "No transactions found"
                 `styleBasic` [textFont "Italics"]
          ]
      , txFilterWidget model `nodeVisible` (sellerModel ^. #showTransactionFilter)
      ]
  where
    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]

    timeZone :: TimeZone
    timeZone = config ^. #timeZone

    today :: Day
    today = config ^. #currentDay

    (mLowerDay,mUpperDay) = sellerModel ^. #txFilterModel % #dateRange

    startTime :: POSIXTime
    startTime = maybe 0 (localTimeToPosixTime timeZone . beginningOfDay) mLowerDay

    endTime :: POSIXTime
    endTime = localTimeToPosixTime timeZone 
            $ endOfDay
            $ fromMaybe today mUpperDay

    shownDateRange :: Text
    shownDateRange = unwords
      [ maybe "Beginning" (const $ showLocalDate timeZone startTime) mLowerDay 
      , "-"
      , maybe "Present" (const $ showLocalDate timeZone endTime) mUpperDay 
      ]

    withinDateRange :: Transaction -> Bool
    withinDateRange Transaction{blockTime} =
      blockTime >= startTime && blockTime <= endTime

    MarketWallet{marketAddress,transactions} = selectedWallet

    sample :: [Transaction]
    sample = applySearchFilter marketAddress (sellerModel ^. #txFilterModel)
           $ filter withinDateRange transactions

    txRow :: Transaction -> AppNode
    txRow tx@Transaction{..} = do
      vstack
        [ hstack 
            [ copyableLabelSelf txHash 10 customBlue lightGray
            , spacer_ [width 2]
            , tooltip_ "Inspect Aftermarket Transaction" [tooltipDelay 0] $
                button inspectIcon (AftermarketEvent $ AftermarketSellerEvent $ InspectSellerTransaction tx)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 0
                    , paddingL 2
                    , paddingR 2
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , filler
            , label calendarIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalDate (config ^. #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor white
                  ]
            , spacer_ [width 5]
            , label clockIcon
                `styleBasic` 
                  [ textSize 10
                  , textColor customBlue
                  , textFont "Remix"
                  , paddingT 5
                  ]
            , spacer_ [width 3]
            , label (showLocalTime (config ^. #timeZone) blockTime)
                `styleBasic` 
                  [ textSize 10
                  , textColor white
                  ]
            ]
        ] `styleBasic` 
            [ padding 10
            , bgColor customGray2
            , radius 5
            , border 1 black
            ]

txFilterWidget :: AppModel -> AppNode
txFilterWidget model = do
  let currentScene = model ^. #aftermarketModel % #sellerModel % #txFilterScene
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
  vstack
    [ centerWidget $ hstack
        [ vstack
            [ vgrid
                [ optionButton_ "Filter" FilterScene 
                    (toLensVL $ #aftermarketModel % #sellerModel % #txFilterScene) 
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
                ] `styleBasic` [height 100]
            , filler
            ]
        , vstack
            [ vstack 
                [ zstack
                    [ widgetIf (currentScene == FilterScene) filterWidget
                    ]
                , spacer
                , hstack 
                    [ filler
                    , button "Reset" $ AftermarketEvent $ AftermarketSellerEvent ResetSellerTxFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ #aftermarketModel % #sellerModel % #showTransactionFilter)
                        [onClick $ AftermarketEvent $ AftermarketSellerEvent CheckSellerTxFilters]
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
      let rootLens = #aftermarketModel % #sellerModel % #txFilterModel
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
      vstack
        [ spacer
        , box_ [alignMiddle] $
            label "Filter Settings"
              `styleBasic` [textSize 14, textFont "Italics"]
        , spacer
        , vstack
            [ hstack 
                [ spacer_ [width 20]
                , label "Start Date:"
                , spacer
                , textField_ (toLensVL $ rootLens % lowerBoundText) [placeholder "MM-DD-YYYY"]
                    `styleBasic` [width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
                , mainButton helpIcon (Alert txStartDateMsg)
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
                [ spacer_ [width 20]
                , label "End Date:"
                , spacer
                , textField_ (toLensVL $ rootLens % upperBoundText) [placeholder "MM-DD-YYYY"]
                    `styleBasic` [width 150, bgColor customGray1, sndColor darkGray]
                    `styleFocus` [border 1 customBlue]
                , mainButton helpIcon (Alert txEndDateMsg)
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
                [ spacer_ [width 20]
                , label "NFT Type:"
                , spacer
                , hgrid_ [childSpacing_ 3]
                    [ choiceButton "Loans" (Just LoanKey) (toLensVL $ rootLens % #nftType)
                    , choiceButton "Options" (Just OptionsKey) (toLensVL $ rootLens % #nftType)
                    , choiceButton "Other NFT" (Just OtherNft) (toLensVL $ rootLens % #nftType)
                    , choiceButton "Any" Nothing (toLensVL $ rootLens % #nftType)
                    ]
                ]
            , spacer
            , widgetIf (model ^. rootLens % #nftType == Just OtherNft) $ 
                hstack
                  [ spacer_ [width 50]
                  , label "Policy Id:"
                      `styleBasic` [textSize 14]
                  , spacer
                  , textField (toLensVL $ rootLens % #policyId) 
                      `styleBasic` [textSize 10, width 200, bgColor customGray1, sndColor darkGray]
                      `styleFocus` [border 1 customBlue]
                  ] 
            ]
        ]

txInspectionWidget :: AppModel -> AppNode
txInspectionWidget model = do
    flip styleBasic [bgColor $ black & #a .~ 0.4, padding 30, radius 10] $ box $
      vstack
        [ centerWidgetH $ label "Aftermarket Transaction"
            `styleBasic` [ textFont "Italics" ]
        , spacer
        , vscroll_ [wheelRate 50] $ vstack
            [ aftermarketInputs model
            , spacer
            , aftermarketOutputs model
            ]
        , filler
        , hstack
            [ filler
            , button "Close" $ AftermarketEvent $ AftermarketSellerEvent CloseInspectedSellerTransaction
            ]
        ] `styleBasic` [bgColor customGray3, padding 30, radius 10]

aftermarketInputs :: AppModel -> AppNode
aftermarketInputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Inputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #aftermarketModel % #sellerModel % #inspectedTransaction % toggleShow #showInputs)
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
              `nodeVisible` (allInputs /= [])
          , label "none" 
              `styleBasic` [textColor white, textSize 12]
              `nodeVisible` null allInputs
          ]
      , widgetIf showInputs $
          vstack_ [childSpacing] (map inputRow allInputs)
            `styleBasic` [padding 10]
      ]
  where
    Config{network} = config

    MarketWallet{marketAddress} = aftermarketModel ^. #selectedWallet

    Transaction{showInputs, plutusContracts} = 
      fromMaybe def $ aftermarketModel ^. #sellerModel % #inspectedTransaction

    allInputs = filter ((== Just marketAddress) . view #paymentAddress) plutusContracts

    inputRow :: TransactionPlutusContract -> AppNode
    inputRow TransactionPlutusContract{spendsInput,datum,redeemer} =
      case parseInlineAftermarketDatum <$> datum of
        Just (SpotBidDatum spotBidDatum) -> 
          spotBidRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            spotBidDatum
            (fromMaybe Aftermarket.CloseOrUpdateBidderUTxO $ decodeData @Aftermarket.MarketRedeemer redeemer)
        Just (ClaimBidDatum claimBidDatum) ->
          claimBidRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            claimBidDatum
            (fromMaybe Aftermarket.CloseOrUpdateBidderUTxO $ decodeData @Aftermarket.MarketRedeemer redeemer)
        Just (AcceptedBidDatum acceptedBidDatum) ->
          acceptedBidRow
            (fromMaybe (TxOutRef "" 0) spendsInput)
            acceptedBidDatum
            (fromMaybe Aftermarket.CloseOrUpdateBidderUTxO $ decodeData @Aftermarket.MarketRedeemer redeemer)
        Just (SpotDatum spotDatum) -> 
          spotRow 
            (fromMaybe (TxOutRef "" 0) spendsInput)
            spotDatum
            (fromMaybe Aftermarket.CloseOrUpdateBidderUTxO $ decodeData @Aftermarket.MarketRedeemer redeemer)
        Just (AuctionDatum auctionDatum) -> 
          auctionRow 
            (fromMaybe (TxOutRef "" 0) spendsInput)
            auctionDatum
            (fromMaybe Aftermarket.CloseOrUpdateBidderUTxO $ decodeData @Aftermarket.MarketRedeemer redeemer)
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

    spotBidRow :: TxOutRef -> Aftermarket.SpotBidDatum -> Aftermarket.MarketRedeemer -> AppNode
    spotBidRow utxoRef Aftermarket.SpotBidDatum{..} redeemer = do
      let payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
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
      vstack
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
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
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

    claimBidRow :: TxOutRef -> Aftermarket.ClaimBidDatum -> Aftermarket.MarketRedeemer -> AppNode
    claimBidRow utxoRef Aftermarket.ClaimBidDatum{..} redeemer = do
      let numNfts = length nftNames
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
      vstack
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
            , flip styleBasic [textSize 10] $ 
                tooltip_ prettyExpirationTime [tooltipDelay 0] $
                  label expirationIcon
                    `styleBasic` 
                      [ textMiddle
                      , textFont "Remix"
                      , textSize 10
                      , textColor customRed
                      ]
            , filler
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
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

    acceptedBidRow :: TxOutRef -> Aftermarket.AcceptedBidDatum -> Aftermarket.MarketRedeemer -> AppNode
    acceptedBidRow utxoRef Aftermarket.AcceptedBidDatum{..} redeemer = do
      let payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
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
          prettyExpirationTime = unlines
            [ unwords
                [ "Claim Deadline:"
                , showLocalDate (config ^. #timeZone) $ fromPlutusTime claimExpiration
                , showLocalTime (config ^. #timeZone) $ fromPlutusTime claimExpiration
                ]
            ]
      vstack
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
            , filler
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
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

    spotRow :: TxOutRef -> Aftermarket.SpotDatum -> Aftermarket.MarketRedeemer -> AppNode
    spotRow utxoRef Aftermarket.SpotDatum{..} redeemer = do
      let payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
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
      vstack
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
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
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
    
    auctionRow :: TxOutRef -> Aftermarket.AuctionDatum -> Aftermarket.MarketRedeemer -> AppNode
    auctionRow utxoRef Aftermarket.AuctionDatum{..} redeemer = do
      let numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ startingPrice ^. #unPrices
      vstack
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
            , filler
            , label (display redeemer)
                `styleBasic` [textSize 12, textColor customRed, textFont "Italics"]
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

aftermarketOutputs :: AppModel -> AppNode
aftermarketOutputs AppModel{..} = do
    vstack
      [ hstack
          [ label "Outputs:"
              `styleBasic` [textSize 12]
          , spacer
          , toggleButton_ horizontalMoreIcon 
              (toLensVL $ #aftermarketModel % #sellerModel % #inspectedTransaction % toggleShow #showOutputs)
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
              `nodeVisible` (allOutputs /= [])
          , label "none" 
              `styleBasic` [textColor white, textSize 12]
              `nodeVisible` null allOutputs
          ]
      , widgetIf showOutputs $
          vstack_ [childSpacing] (map outputRow allOutputs)
            `styleBasic` [padding 10]
      ]
  where
    Config{network} = config

    MarketWallet{marketAddress} = aftermarketModel ^. #selectedWallet

    Transaction{showOutputs, outputs} = 
      fromMaybe def $ aftermarketModel ^. #sellerModel % #inspectedTransaction

    allOutputs = filter ((== marketAddress) . view #paymentAddress) outputs

    outputRow :: TransactionUTxO -> AppNode
    outputRow TransactionUTxO{utxoRef,inlineDatum} =
      case parseInlineAftermarketDatum <$> inlineDatum of
          Just (SpotBidDatum spotBidDatum) -> spotBidRow utxoRef spotBidDatum
          Just (ClaimBidDatum claimBidDatum) -> claimBidRow utxoRef claimBidDatum
          Just (AcceptedBidDatum acceptedBidDatum) -> acceptedBidRow utxoRef acceptedBidDatum
          Just (SpotDatum spotDatum) -> spotRow utxoRef spotDatum
          Just (AuctionDatum auctionDatum) -> auctionRow utxoRef auctionDatum
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

    spotBidRow :: TxOutRef -> Aftermarket.SpotBidDatum -> AppNode
    spotBidRow utxoRef Aftermarket.SpotBidDatum{..} = do
      let payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
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
      vstack
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

    claimBidRow :: TxOutRef -> Aftermarket.ClaimBidDatum -> AppNode
    claimBidRow utxoRef Aftermarket.ClaimBidDatum{..} = do
      let numNfts = length nftNames
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
      vstack
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
            , flip styleBasic [textSize 10] $ 
                tooltip_ prettyExpirationTime [tooltipDelay 0] $
                  label expirationIcon
                    `styleBasic` 
                      [ textMiddle
                      , textFont "Remix"
                      , textSize 10
                      , textColor customRed
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

    acceptedBidRow :: TxOutRef -> Aftermarket.AcceptedBidDatum -> AppNode
    acceptedBidRow utxoRef Aftermarket.AcceptedBidDatum{..} = do
      let payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
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
          prettyExpirationTime = unlines
            [ unwords
                [ "Claim Deadline:"
                , showLocalDate (config ^. #timeZone) $ fromPlutusTime claimExpiration
                , showLocalTime (config ^. #timeZone) $ fromPlutusTime claimExpiration
                ]
            ]
      vstack
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

    spotRow :: TxOutRef -> Aftermarket.SpotDatum -> AppNode
    spotRow utxoRef Aftermarket.SpotDatum{..} = do
      let payToAddress = either (const "error") fst $ plutusToBech32 network paymentAddress
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
      vstack
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
    
    auctionRow :: TxOutRef -> Aftermarket.AuctionDatum -> AppNode
    auctionRow utxoRef Aftermarket.AuctionDatum{..} = do
      let numNfts = length nftNames
          keyTypeLabel
            | nftPolicyId == Loans.activeBeaconCurrencySymbol = "Loan Key NFT(s)"
            | nftPolicyId == Options.activeBeaconCurrencySymbol = "Options Key NFT(s)"
            | otherwise = "Other NFT(s)"
          prices = map toNativeAsset $ startingPrice ^. #unPrices
      vstack
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

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
matchesUTxO 
  :: PaymentAddress
  -> SellerTxFilterModel
  -> TransactionUTxO 
  -> Bool
matchesUTxO marketAddress SellerTxFilterModel{..} TransactionUTxO{..} = 
    and
      [ paymentAddress == marketAddress
      , maybe True (matchesAsset . policyBeaconName) nftType
      ]
  where
    policyBeaconName keyType = view #unPolicyBeacon $ case keyType of
      LoanKey -> Aftermarket.genPolicyBeacon Loans.activeBeaconCurrencySymbol
      OptionsKey -> Aftermarket.genPolicyBeacon Options.activeBeaconCurrencySymbol
      OtherNft -> maybe "" (Aftermarket.genPolicyBeacon . CurrencySymbol) $ parseHex policyId

    matchesAsset :: TokenName -> Bool
    matchesAsset target =
      flip any nativeAssets $ \NativeAsset{policyId=actualPolicyId,..} ->
        actualPolicyId == Aftermarket.beaconCurrencySymbol && tokenName == target
        
applySearchFilter 
  :: PaymentAddress
  -> SellerTxFilterModel 
  -> [Transaction] 
  -> [Transaction]
applySearchFilter marketAddress filterModel xs
  | isNothing $ filterModel ^. #nftType = xs
  | otherwise = flip filter xs $ \Transaction{..} -> or
      [ any (matchesUTxO marketAddress filterModel) inputs
      , any (matchesUTxO marketAddress filterModel) outputs
      ]

-------------------------------------------------
-- Helper Lens
-------------------------------------------------
lowerBoundText :: Lens' SellerTxFilterModel Text
lowerBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: SellerTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _1

    setLowerBoundText :: SellerTxFilterModel -> Text -> SellerTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _1 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

upperBoundText :: Lens' SellerTxFilterModel Text
upperBoundText = lens getLowerBoundText setLowerBoundText
  where
    getLowerBoundText :: SellerTxFilterModel -> Text
    getLowerBoundText tx = 
      maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%m-%d-%Y") $ tx ^. #dateRange % _2

    setLowerBoundText :: SellerTxFilterModel -> Text -> SellerTxFilterModel
    setLowerBoundText tx date = 
      tx & #dateRange % _2 .~ Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (toString date)

-- | A lens to toggle the `show` field of the `Transaction`.
toggleShow :: Lens' Transaction Bool -> Lens' (Maybe Transaction) Bool
toggleShow finalLens = lens getToggleShow setToggleShow
  where
    getToggleShow :: Maybe Transaction -> Bool
    getToggleShow = maybe False (view finalLens)

    setToggleShow :: Maybe Transaction -> Bool -> Maybe Transaction
    setToggleShow maybeInfo b = fmap (set finalLens b) maybeInfo

-------------------------------------------------
-- Helper Widgets
-------------------------------------------------
-- | A label button that will copy itself.
copyableLabelSelf :: Text -> Double -> Color -> Color -> WidgetNode s AppEvent
copyableLabelSelf caption fontSize mainColor hoverColor = 
  tooltip_ "Copy" [tooltipDelay 0] $ button_ caption (CopyText caption) [resizeFactor 2]
    `styleBasic`
      [ padding 0
      , radius 5
      , textMiddle
      , textLeft
      , textSize fontSize
      , border 0 transparent
      , textColor mainColor
      , bgColor transparent
      ]
    `styleHover` [textColor hoverColor, cursorIcon CursorHand]

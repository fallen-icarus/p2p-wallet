module P2PWallet.GUI.Widgets.Aftermarket.Seller.OpenSales
  ( openSalesWidget
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

openSalesWidget :: AppModel -> AppNode
openSalesWidget model@AppModel{knownWallets,aftermarketModel=AftermarketModel{..},reverseTickerMap,config} =
    zstack
      [ mainWidget
      , updateSaleWidget model `nodeVisible` and
          [ isJust (sellerModel ^. #newSaleUpdate)
          -- Hide until after syncing is complete.
          , not $ model ^. #waitingStatus % #syncingLoanHistories
          , not $ model ^. #waitingStatus % #syncingOptionsContracts
          ]
      , salesFilterWidget model `nodeVisible` (sellerModel ^. #showSaleFilter)
      , widgetMaybe (sellerModel ^. #inspectedSale) $ \saleUTxO ->
          inspectBatchWidget model 
            InspectBatchConfig
              { batchUTxO = saleUTxO
              , closeEvent = 
                  AftermarketEvent $ AftermarketSellerEvent CloseInspectedAftermarketSale
              , inspectLoanHistoryEvent =
                  AftermarketEvent . AftermarketSellerEvent . InspectSellerLoanHistory
              , lookupBorrowerEvent =
                  AftermarketEvent . AftermarketSellerEvent . InspectSellerBorrowerInformation
              , mAddToHomeEvent = Just $ AftermarketEvent . AftermarketSellerEvent . AddNftToHomeBatch
              }
          `nodeVisible` and
            -- Hide until after syncing is complete.
            [ not $ model ^. #waitingStatus % #syncingLoanHistories
            , not $ model ^. #waitingStatus % #syncingOptionsContracts
            ]
      ]
  where
    toggleOffStyle :: Style
    toggleOffStyle = 
      def `styleBasic` [ bgColor transparent , textColor customBlue ]
          `styleHover` [ bgColor customGray1 ]
  
    MarketWallet{..} = selectedWallet

    allSales :: [AftermarketUTxO]
    allSales = filter ((==Just True) . fmap isSellerDatum . view #marketDatum) utxos

    fractionShown :: Text
    fractionShown = show (length sample) <> "/" <> show (length allSales)

    sample :: [AftermarketUTxO]
    sample = orderer (sellerModel ^. #salesFilterModel % #sortingDirection) 
           $ sorter (sellerModel ^. #salesFilterModel) 
           $ filterer (sellerModel ^. #salesFilterModel) allSales

    mainWidget :: AppNode
    mainWidget =
      cushionWidgetH $ vstack
        [ hstack 
            [ label ("Sales (" <> fractionShown <> ")")
                `styleBasic` [textFont "Italics", textSize 14]
            , spacer_ [width 2]
            , tooltip_ "Sort/Filter" [tooltipDelay 0] $
                toggleButton_ menuSearchIcon
                  (toLensVL $ #aftermarketModel % #sellerModel % #showSaleFilter)
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
            vstack_ [childSpacing] (map saleRow sample)
              `styleBasic` [padding 10]
        , filler
        ] 

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
          prices = map toNativeAsset $ salePrice ^. #unPrices
          inspectEvt = AftermarketEvent $ AftermarketSellerEvent $ InspectAftermarketSale u
          updateEvt = AftermarketEvent 
                    $ AftermarketSellerEvent 
                    $ AddSelectedSaleUpdate 
                    $ StartAdding 
                    $ Just u
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
                , filler
                , label (show numNfts <> " " <> keyTypeLabel)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , label "Sale Price:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ spacer : map priceWidget p
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
                button closeCircleIcon (AftermarketEvent $ AftermarketSellerEvent $ AddSelectedSaleClose u)
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
          inspectEvt = AftermarketEvent $ AftermarketSellerEvent $ InspectAftermarketSale u
          updateEvt = AftermarketEvent 
                    $ AftermarketSellerEvent 
                    $ AddSelectedSaleUpdate 
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
                , filler
                , label (show numNfts <> " " <> keyTypeLabel)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , label "Starting Price:"
                `styleBasic` [textSize 8, textColor lightGray]
            , spacer_ [width 2]
            , vstack_ [childSpacing_ 3] $ for (groupInto 3 prices) $ 
                \p -> hstack_ [childSpacing_ 3] $ spacer : map priceWidget p
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
                button closeCircleIcon (AftermarketEvent $ AftermarketSellerEvent $ AddSelectedSaleClose u)
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

updateSaleWidget :: AppModel -> AppNode
updateSaleWidget model = do
  let maybeLens' = maybeLens (def,def) (#aftermarketModel % #sellerModel % #newSaleUpdate)
      NewSaleCreation{isAuction} = fromMaybe def $ model ^? maybeLens' % _2
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  vstack
    [ centerWidget $ vstack
        [ hgrid
            [ optionButton_ "Spot" False (toLensVL $ maybeLens' % _2 % #isAuction) 
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
            , optionButton_ "Auction" True (toLensVL $ maybeLens' % _2 % #isAuction)
                [optionButtonOffStyle offStyle]
                `styleBasic` 
                  [ bgColor customGray3
                  , textColor customBlue
                  , radiusTL 0
                  , radiusTR 10
                  , radiusBR 0
                  , radiusBL 0
                  , border 0 transparent
                  ]
            ]
        , zstack 
            [ widgetIf (not isAuction) $ centerWidgetH $ spotUpdateWidget model
            , widgetIf isAuction $ centerWidgetH $ auctionUpdateWidget model
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

spotUpdateWidget :: AppModel -> AppNode
spotUpdateWidget model@AppModel{..} = do
  let maybeLens' = maybeLens (def,def) (#aftermarketModel % #sellerModel % #newSaleUpdate)
      NewSaleCreation{nfts,showNfts} = fromMaybe def $ model ^? maybeLens' % _2
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
      removeEvt = AftermarketEvent . AftermarketSellerEvent . RemoveSellerSaleUpdateNft
  vstack
    [ centerWidgetH $ hstack
        [ label "What is a spot sale?"
            `styleBasic` [textFont "Italics"]
        , spacer_ [width 3]
        , helpButton whatIsSpotSaleMsg
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
        [ label "Seller Credential:"
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
        , helpButton sellerCredentialMsg
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
        , helpButton salePaymentAddressMsg
        ]
    , spacer
    , hstack
        [ label "Sale Price (assets separated with newlines)"
            `styleBasic` [textSize 10]
        , spacer_ [width 3]
        , helpButton salePriceMsg
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #salePrice)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" (AftermarketEvent $ AftermarketSellerEvent $ AddSelectedSaleUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Update" 
              (AftermarketEvent $ AftermarketSellerEvent $ AddSelectedSaleUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

auctionUpdateWidget :: AppModel -> AppNode
auctionUpdateWidget model@AppModel{..} = do
  let maybeLens' = maybeLens (def,def) (#aftermarketModel % #sellerModel % #newSaleUpdate)
      NewSaleCreation{nfts,showNfts} = fromMaybe def $ model ^? maybeLens' % _2
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
      removeEvt = AftermarketEvent . AftermarketSellerEvent . RemoveSellerSaleUpdateNft
  vstack
    [ centerWidgetH $ hstack
        [ label "What is an auction sale?"
            `styleBasic` [textFont "Italics"]
        , spacer_ [width 3]
        , helpButton whatIsAuctionSaleMsg
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
        [ label "Seller Credential:"
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
        , helpButton sellerCredentialMsg
        ]
    , spacer
    , hstack
        [ label "Starting Price (assets separated with newlines)"
            `styleBasic` [textSize 10]
        , spacer_ [width 3]
        , helpButton startingPriceMsg
        ]
    , spacer
    , textArea (toLensVL $ maybeLens' % _2 % #salePrice)
        `styleBasic` [height 100, textSize 10, bgColor customGray1]
        `styleFocus` [border 1 customBlue]
    , spacer
    , box_ [alignRight] $ 
        hstack
          [ button "Cancel" 
              (AftermarketEvent $ AftermarketSellerEvent $ AddSelectedSaleUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Update" 
              (AftermarketEvent $ AftermarketSellerEvent $ AddSelectedSaleUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

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

salesFilterWidget :: AppModel -> AppNode
salesFilterWidget model@AppModel{aftermarketModel=AftermarketModel{..}} = do
  let offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray, border 1 customBlue ]
      rootLens = #aftermarketModel % #sellerModel
      filterScene = sellerModel ^. #salesFilterModel % #scene
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
                    , button "Reset" $ AftermarketEvent $ AftermarketSellerEvent ResetOpenSalesFilters
                    , spacer
                    , toggleButton_ "Confirm" (toLensVL $ rootLens % #showSaleFilter)
                        [onClick $ AftermarketEvent $ AftermarketSellerEvent CheckOpenSalesFilterModel]
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
      let rootLens = #aftermarketModel % #sellerModel % #salesFilterModel
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
            , label "Sale Type:"
                `styleBasic` [textSize 12]
            , spacer_ [width 10]
            , hgrid_ [childSpacing_ 3]
                [ choiceButton "Spot" (Just False) (toLensVL $ rootLens % #shouldBeAuction)
                , choiceButton "Auction" (Just True) (toLensVL $ rootLens % #shouldBeAuction)
                , choiceButton "Either" Nothing (toLensVL $ rootLens % #shouldBeAuction)
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
          possibleSortingMethods = enumFrom OpenSalesLexicographically
          rootLens = #aftermarketModel % #sellerModel % #salesFilterModel
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

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
orderer :: SortDirection -> [AftermarketUTxO] -> [AftermarketUTxO]
orderer = \case
  SortAscending -> id
  SortDescending -> reverse

sorter :: OpenSalesFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
sorter OpenSalesFilterModel{..} = 
  case sortingMethod of
    OpenSalesLexicographically -> sortOn (view #utxoRef)
    OpenSalesTime -> sortOn (view #blockTime)
    OpenSalesNftCount -> sortOn (aftermarketUTxONfts >=> return . length . snd)

filterer :: OpenSalesFilterModel -> [AftermarketUTxO] -> [AftermarketUTxO]
filterer OpenSalesFilterModel{policyId,shouldBeAuction,nftType} us = do
    u <- us
    let (actualPolicyId,_) = fromMaybe ("",def) $ aftermarketUTxONfts u
    guard $ matchesPolicyId actualPolicyId
    guard $ maybe True matchesSaleType $ u ^. #marketDatum
    return u
  where
    matchesPolicyId :: CurrencySymbol -> Bool
    matchesPolicyId actualPolicyId
      | nftType == Just LoanKey = actualPolicyId == Loans.activeBeaconCurrencySymbol
      | nftType == Just OptionsKey = actualPolicyId == Options.activeBeaconCurrencySymbol
      | nftType == Just OtherNft = display actualPolicyId == policyId
      | otherwise = True

    matchesSaleType :: AftermarketDatum -> Bool
    matchesSaleType marketDatum
      | isNothing shouldBeAuction = True
      | otherwise = isJust (marketDatum ^? _AuctionDatum) == fromMaybe False shouldBeAuction

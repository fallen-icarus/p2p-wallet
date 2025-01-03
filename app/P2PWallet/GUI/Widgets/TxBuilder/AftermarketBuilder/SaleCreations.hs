module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleCreations
  ( 
    saleCreationsList
  , editSaleCreationWidget
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Aftermarket.Common
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

saleCreationsList :: ReverseTickerMap -> [(Int,SaleCreation)] -> [AppNode]
saleCreationsList reverseTickerMap = map utxoRow
  where
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

    utxoRow :: (Int,SaleCreation) -> AppNode
    utxoRow s@(idx,SaleCreation{..}) = do
      let policyId = maybe "" (view #policyId) $ maybeHead nfts
          numberSold = length nfts
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol = 
                show numberSold <> " Loan Key(s)"
            | policyId == Options.activeBeaconCurrencySymbol =
                show numberSold <> " Options Key(s)"
            | otherwise =
                show numberSold <> " Other NFT(s)"
          (saleType, priceHeader)
            | isAuction = ("Auction", "Starting Price:")
            | otherwise = ("Spot", "Sale Price:")
      hstack
        [ vstack
            [ hstack
                [ label ("Create Aftermarket " <> saleType <> " Sale")
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ tooltip_ (marketWallet ^. #alias) [tooltipDelay 0] $
                    label userIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        ]
                , filler
                , label keyType
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ box_ [alignTop] $ label priceHeader
                    `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
                , spacer_ [width 2]
                , vstack_ [childSpacing_ 3] $ for (groupInto 4 salePrice) $ 
                    \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , hstack
            [ vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                    button editIcon 
                        (aftermarketBuilderEvent $ EditSelectedSaleCreation $ StartAdding $ Just s)
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
                ]
            , vstack
                [ spacer_ [width 15]
                , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                    button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedSaleCreation idx)
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
                ]
            ]
        ]

editSaleCreationWidget :: AppModel -> AppNode
editSaleCreationWidget model = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #aftermarketBuilderModel % #targetSaleCreation)
      NewSaleCreation{isAuction} = fromMaybe def $ model ^? maybeLens' % _2
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  centerWidget $ vscroll_ [wheelRate 50] $ vstack
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
        [ widgetIf (not isAuction) $ centerWidgetH $ spotEditWidget model
        , widgetIf isAuction $ centerWidgetH $ auctionEditWidget model
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

spotEditWidget :: AppModel -> AppNode
spotEditWidget model@AppModel{..} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #aftermarketBuilderModel % #targetSaleCreation)
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
      removeEvt = aftermarketBuilderEvent . RemoveSaleCreationNft
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
        [ label "Sale Price (assets separated with newlines):"
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
          [ button "Cancel" (aftermarketBuilderEvent $ EditSelectedSaleCreation CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Create Spot" (aftermarketBuilderEvent $ EditSelectedSaleCreation ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

auctionEditWidget :: AppModel -> AppNode
auctionEditWidget model@AppModel{..} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #aftermarketBuilderModel % #targetSaleCreation)
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
      removeEvt = aftermarketBuilderEvent . RemoveSaleCreationNft
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
        [ label "Starting Price (assets separated with newlines):"
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
          [ button "Cancel" (aftermarketBuilderEvent $ EditSelectedSaleCreation CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Create Auction" (aftermarketBuilderEvent $ EditSelectedSaleCreation ConfirmAdding)
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

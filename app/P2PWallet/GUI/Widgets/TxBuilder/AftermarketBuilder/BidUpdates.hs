module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.BidUpdates
  ( 
    bidUpdatesList
  , editBidUpdateWidget
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Aftermarket.Common
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

bidUpdatesList :: ReverseTickerMap -> [(Int,BidUpdate)] -> [AppNode]
bidUpdatesList reverseTickerMap = map utxoRow
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

    utxoRow :: (Int,BidUpdate) -> AppNode
    utxoRow s@(idx,BidUpdate{oldBid,newBid=BidCreation{..}}) = do
      let policyId = maybe "" (view #policyId) $ maybeHead nfts
          numberSold = length nfts
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol =
                show numberSold <> " Loan Key(s)"
            | policyId == Options.activeBeaconCurrencySymbol =
                show numberSold <> " Options Key(s)"
            | otherwise =
                show numberSold <> " Other NFT(s)"
          newBidType
            | isSpotBid = "Spot"
            | otherwise = "Claim"
          oldBidType
            | isJust $ oldBid ^? #marketDatum % _Just % _SpotBidDatum = "Spot"
            | otherwise = "Claim"
      hstack
        [ vstack
            [ hstack
                [ label ("Update Aftermarket Bid (" <> oldBidType <> " -> " <> newBidType <> ")")
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display (oldBid ^. #utxoRef) in
                  flip styleBasic [textSize 10] $ tooltip_ prettyRef [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText prettyRef] $
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
                , flip styleBasic [textSize 10] $ tooltip_ alias [tooltipDelay 0] $
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
                [ box_ [alignTop] $ label "Bid:"
                    `styleBasic` [paddingT 3, textSize 8, textColor lightGray]
                , spacer_ [width 2]
                , vstack_ [childSpacing_ 3] $ for (groupInto 4 bid) $ 
                    \p -> hstack_ [childSpacing_ 3] $ map priceWidget p
                , widgetIf (not isSpotBid) $ hstack
                    [ filler
                    , label "Deposit:"
                        `styleBasic` [textSize 8, textColor lightGray]
                    , spacer_ [width 2]
                    , label (display deposit)
                        `styleBasic` [textSize 8, textColor lightGray]
                    ]
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignMiddle] $ hstack
            [ box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (aftermarketBuilderEvent $ EditSelectedBidUpdate $ StartAdding $ Just s)
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
            , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedBidUpdate idx)
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

editBidUpdateWidget :: AppModel -> AppNode
editBidUpdateWidget model = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #aftermarketBuilderModel % #targetBidUpdate)
      NewBidCreation{isSpotBid,nfts} = fromMaybe def $ model ^? maybeLens' % _2
      keyType = maybe "" (view #policyId) $ maybeHead nfts
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  centerWidget $ vscroll_ [wheelRate 50] $ vstack
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
        [ widgetIf isSpotBid $ centerWidgetH $ editSpotBidWidget model
        , widgetIf (not isSpotBid) $ centerWidgetH $ editClaimBidWidget model
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

editSpotBidWidget :: AppModel -> AppNode
editSpotBidWidget model@AppModel{..} = do
  let maybeLens' = maybeLens def (#txBuilderModel % #aftermarketBuilderModel % #targetBidUpdate)
      NewBidCreation{nfts,showNfts} = fromMaybe def $ model ^? maybeLens' % _2
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
      removeEvt = aftermarketBuilderEvent . RemoveBidUpdateNft
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
          [ button "Cancel" (aftermarketBuilderEvent $ EditSelectedBidUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Confirm" 
              (aftermarketBuilderEvent $ EditSelectedBidUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

editClaimBidWidget :: AppModel -> AppNode
editClaimBidWidget model@AppModel{..} = do
  let maybeLens' = maybeLens def (#txBuilderModel % #aftermarketBuilderModel % #targetBidUpdate)
      NewBidCreation{nfts,showNfts} = fromMaybe def $ model ^? maybeLens' % _2
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
      removeEvt = aftermarketBuilderEvent . RemoveBidUpdateNft
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
          [ button "Cancel" (aftermarketBuilderEvent $ EditSelectedBidUpdate CancelAdding)
              `styleBasic` [textSize 10]
          , spacer
          , mainButton "Confirm" 
              (aftermarketBuilderEvent $ EditSelectedBidUpdate ConfirmAdding)
              `styleBasic` [textSize 10]
          ]
    ] `styleBasic` [padding 20]

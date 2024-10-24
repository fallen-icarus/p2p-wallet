module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.ClaimBidAcceptances
  ( 
    claimBidAcceptancesList
  , editClaimBidAcceptance
  ) where

import Monomer as M

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
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

claimBidAcceptancesList :: ReverseTickerMap -> [(Int,ClaimBidAcceptance)] -> [AppNode]
claimBidAcceptancesList reverseTickerMap = map utxoRow
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

    utxoRow :: (Int,ClaimBidAcceptance) -> AppNode
    utxoRow s@(idx,ClaimBidAcceptance{..}) = do
      let prices = maybe [] (map toNativeAsset . Aftermarket.unPrices)
                 $ aftermarketUTxOBuyerPrice bidUTxO
          (policyId, nfts) = fromMaybe ("",[]) $ aftermarketUTxONfts bidUTxO
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol = "Loan Key(s)"
            | policyId == Options.activeBeaconCurrencySymbol = "Options Key(s)"
            | otherwise = "Other NFT(s)"
          numberSold = length nfts
      hstack
        [ vstack
            [ hstack
                [ label "Accept Claim Bid"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display $ bidUTxO ^. #utxoRef in
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
                , flip styleBasic [textSize 10] $ tooltip_ (marketWallet ^. #alias) [tooltipDelay 0] $
                    label userIcon
                      `styleBasic` 
                        [ textMiddle
                        , textFont "Remix"
                        , textSize 8
                        , textColor customBlue
                        ]
                , filler
                , label (show numberSold <> " " <> keyType)
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
        , box_ [alignMiddle] $ hstack
            [ box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (aftermarketBuilderEvent $ EditSelectedClaimBidAcceptance $ StartAdding $ Just s)
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
                button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedClaimBidAcceptance idx)
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

editClaimBidAcceptance :: AppModel -> AppNode
editClaimBidAcceptance AppModel{..} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #aftermarketBuilderModel % #targetClaimBidAcceptance)
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
  centerWidget $ vstack
    [ centerWidgetH $ label "Where would you like the bid payment to go?"
    , spacer_ [width 20]
    , centerWidgetH $ hstack
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
        , helpButton claimBidPaymentAddressMsg
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ aftermarketBuilderEvent $ EditSelectedClaimBidAcceptance CancelAdding
        , spacer
        , mainButton "Confirm" $ aftermarketBuilderEvent $ EditSelectedClaimBidAcceptance ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

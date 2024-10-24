module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.LoanKeySpotPurchases
  ( 
    loanKeySpotPurchasesList
  , editLoanKeySpotPurchase
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

loanKeySpotPurchasesList :: ReverseTickerMap -> [(Int,LoanKeySpotPurchase)] -> [AppNode]
loanKeySpotPurchasesList reverseTickerMap = map utxoRow
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

    utxoRow :: (Int,LoanKeySpotPurchase) -> AppNode
    utxoRow s@(idx,LoanKeySpotPurchase{..}) = do
      let prices = maybe [] (map toNativeAsset . Aftermarket.unPrices)
                 $ aftermarketUTxOSellerPrice 
                 $ spotPurchase ^. #saleUTxO
          (_, nfts) = fromMaybe ("",[]) $ aftermarketUTxONfts $ spotPurchase ^. #saleUTxO
          numberSold = length nfts
      hstack
        [ vstack
            [ hstack
                [ label "Purchase Spot Sale"
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label (show numberSold <> " Loan Key(s)")
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
        , box_ [alignMiddle] $ hstack
            [ widgetIf (lenderAddressUpdates /= []) $
                box_ [alignCenter,alignTop] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                  button editIcon 
                      (aftermarketBuilderEvent $ EditSelectedLoanKeySpotPurchase $ StartAdding $ Just s)
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
                button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedLoanKeySpotPurchase idx)
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

editLoanKeySpotPurchase :: AppModel -> AppNode
editLoanKeySpotPurchase AppModel{knownWallets} = do
  let maybeLens' = maybeLens (0,def) (#txBuilderModel % #aftermarketBuilderModel % #targetLoanKeySpotPurchase)
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
  centerWidget $ vstack
    [ centerWidgetH $ label "Where would you like future loan payments to go for these loans?"
    , spacer_ [width 20]
    , centerWidgetH $ hstack
        [ label "Address:"
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % _2 % #newPaymentWallet) 
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
        , button "Cancel" $ aftermarketBuilderEvent $ EditSelectedLoanKeySpotPurchase CancelAdding
        , spacer
        , mainButton "Confirm" $ aftermarketBuilderEvent $ EditSelectedLoanKeySpotPurchase ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

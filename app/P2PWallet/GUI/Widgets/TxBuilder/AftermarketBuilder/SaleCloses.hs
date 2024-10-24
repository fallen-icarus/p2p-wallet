module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleCloses
  ( 
    saleClosesList
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Prelude

saleClosesList :: ReverseTickerMap -> [(Int,SaleClose)] -> [AppNode]
saleClosesList reverseTickerMap = map utxoRow
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

    utxoRow :: (Int,SaleClose) -> AppNode
    utxoRow (idx,SaleClose{..}) = do
      let (policyId,names) = fromMaybe ("",[]) $ marketDatum >>= aftermarketDatumNfts
          numberSold = length names
          salePrice = maybe [] (map toNativeAsset . view #unPrices) $ marketDatum >>= aftermarketDatumPrice
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol =
                show numberSold <> " Loan Key(s)"
            | policyId == Options.activeBeaconCurrencySymbol =
                show numberSold <> " Options Key(s)"
            | otherwise =
                show numberSold <> " Other NFT(s)"
          (saleType, priceHeader)
            | isJust $ marketDatum ^? _Just % _AuctionDatum = ("Auction", "Starting Price:")
            | otherwise = ("Spot", "Sale Price:")
      hstack
        [ vstack
            [ hstack
                [ label ("Close Aftermarket " <> saleType <> " Sale")
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display utxoRef in
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
                , flip styleBasic [textSize 10] $ tooltip_ walletAlias [tooltipDelay 0] $
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
                , box_ [alignCenter,alignTop] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                    button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedSaleClose idx)
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

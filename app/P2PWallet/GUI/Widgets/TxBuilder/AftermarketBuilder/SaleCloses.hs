module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleCloses
  ( 
    saleClosesList
  ) where

import Monomer as M

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Prelude

saleClosesList :: [(Int,SaleClose)] -> [AppNode]
saleClosesList = map utxoRow
  where
    utxoRow :: (Int,SaleClose) -> AppNode
    utxoRow (idx,SaleClose{..}) = do
      let (policyId,names) = fromMaybe ("",[]) $ marketDatum >>= aftermarketDatumNfts
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol = "Loan Keys"
            | policyId == Options.activeBeaconCurrencySymbol = "Options Keys"
            | otherwise = "Other NFTs"
          numberSold = length names
          saleType
            | isJust $ marketDatum ^? _Just % _AuctionDatum = "Auction"
            | otherwise = "Spot"
      hstack
        [ vstack
            [ hstack
                [ label ("Close Aftermarket Sale for " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label keyType
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label ("Type: " <> saleType)
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label (show numberSold <> " NFT(s)")
                    `styleBasic` [textSize 8, textColor lightGray]
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

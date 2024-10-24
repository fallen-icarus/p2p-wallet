module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.BidUnlocks
  ( 
    bidUnlocksList
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

bidUnlocksList :: [(Int,BidUnlock)] -> [AppNode]
bidUnlocksList = map utxoRow
  where
    utxoRow :: (Int,BidUnlock) -> AppNode
    utxoRow (idx,BidUnlock{..}) = do
      let (policyId,names) = fromMaybe ("",[]) 
                           $ bidUTxO ^. #marketDatum >>= aftermarketDatumNfts
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol = "Loan Keys"
            | policyId == Options.activeBeaconCurrencySymbol = "Options Keys"
            | otherwise = "Other NFTs"
          numberSold = length names
      hstack
        [ vstack
            [ hstack
                [ label ("Unlock Unclaimed Aftermarket Bid for " <> sellerWallet ^. #alias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label keyType
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label (show numberSold <> " NFT(s)")
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
                    button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedBidUnlock idx)
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

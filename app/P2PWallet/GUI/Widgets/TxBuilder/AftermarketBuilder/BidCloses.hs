module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.BidCloses
  ( 
    bidClosesList
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

bidClosesList :: [(Int,BidClose)] -> [AppNode]
bidClosesList = map utxoRow
  where
    utxoRow :: (Int,BidClose) -> AppNode
    utxoRow (idx,BidClose{..}) = do
      let (policyId,names) = fromMaybe ("",[]) $ marketDatum >>= aftermarketDatumNfts
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol = "Loan Keys"
            | policyId == Options.activeBeaconCurrencySymbol = "Options Keys"
            | otherwise = "Other NFTs"
          numberSold = length names
          bidType
            | isJust $ marketDatum ^? _Just % _SpotBidDatum = "Spot Bid"
            | otherwise = "Claim Bid"
      hstack
        [ vstack
            [ hstack
                [ label ("Close Aftermarket Bid for " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label keyType
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label ("Type: " <> bidType)
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
                    button closeCircleIcon (aftermarketBuilderEvent $ RemoveSelectedBidClose idx)
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

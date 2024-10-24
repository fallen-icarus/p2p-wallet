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
          numberSold = length names
          keyType
            | policyId == Loans.activeBeaconCurrencySymbol =
                show numberSold <> " Loan Key(s)"
            | policyId == Options.activeBeaconCurrencySymbol =
                show numberSold <> " Options Key(s)"
            | otherwise =
                show numberSold <> " Other NFT(s)"
      hstack
        [ vstack
            [ hstack
                [ label "Unlock Unclaimed Aftermarket Bid"
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
                , flip styleBasic [textSize 10] $ tooltip_ (sellerWallet ^. #alias) [tooltipDelay 0] $
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

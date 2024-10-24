module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.ExpiredClaims
  ( 
    expiredClaimsList
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Prelude

expiredClaimsList :: [(Int,ExpiredClaim)] -> [AppNode]
expiredClaimsList = map utxoRow
  where
    utxoRow :: (Int, ExpiredClaim) -> AppNode
    utxoRow (idx,ExpiredClaim{..}) = do
      let Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum loanUTxO
          titleMsg
            | isJust borrowerCredential = 
                "Claim Lost Loan Collateral"
            | otherwise = 
                "Claim Defaulted Loan Collateral"
      hstack
        [ vstack
            [ hstack
                [ label titleMsg
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display $ loanUTxO ^. #utxoRef in
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
                ]
            , spacer_ [width 2]
            , hstack
                [ label "Loan ID:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 5]
                , label (display loanId)
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (loanBuilderEvent $ RemoveSelectedExpiredClaim idx)
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

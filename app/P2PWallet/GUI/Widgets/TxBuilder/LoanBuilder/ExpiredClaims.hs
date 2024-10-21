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
                "Claim Lost Collateral For " <> walletAlias
            | otherwise = 
                "Claim Defaulted Collateral For " <> walletAlias
      hstack
        [ vstack
            [ hstack
                [ label titleMsg
                    `styleBasic` [textSize 10, textColor customBlue]
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

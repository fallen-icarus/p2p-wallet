module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.KeyBurns
  ( 
    loanKeyBurnsList
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Prelude

loanKeyBurnsList :: [(Int,LoanKeyBurn)] -> [AppNode]
loanKeyBurnsList = map utxoRow
  where
    utxoRow :: (Int, LoanKeyBurn) -> AppNode
    utxoRow (idx,LoanKeyBurn{..}) = do
      hstack
        [ vstack
            [ hstack
                [ label ("Burn Excess Loan Key For " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                ]
            , spacer_ [width 2]
            , hstack
                [ label "Loan ID:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 5]
                , label (display $ loanIdAsset ^. #tokenName)
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
            button closeCircleIcon (loanBuilderEvent $ RemoveSelectedLoanKeyBurn idx)
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

module P2PWallet.GUI.Widgets.TxBuilder.UserWithdrawals
  ( 
    userWithdrawalsList
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.TxBuilder.Internal
import P2PWallet.Prelude

userWithdrawalsList :: [(Int,UserWithdrawal)] -> [AppNode]
userWithdrawalsList = map withdrawalRow
  where
    withdrawalRow :: (Int,UserWithdrawal) -> AppNode
    withdrawalRow (idx,UserWithdrawal{..}) = do
      let mainLabelCaption = fromString $
            printf "Withdraw Rewards from %s" walletAlias
      hstack
        [ vstack
            [ hstack
                [ label mainLabelCaption
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label (display lovelace)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ copyableLabelSelf (toText stakeAddress) lightGray 8
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (TxBuilderEvent $ RemoveSelectedUserWithdrawal idx)
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


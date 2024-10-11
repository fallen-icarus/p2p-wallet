module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.InterestApplications
  ( 
    interestApplicationsList
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Prelude

interestApplicationsList :: [(Int,InterestApplication)] -> [AppNode]
interestApplicationsList = map utxoRow
  where
    utxoRow :: (Int, InterestApplication) -> AppNode
    utxoRow (idx,InterestApplication{..}) = do
      hstack
        [ vstack
            [ hstack
                [ label ("Apply Interest/Penalties For " <> alias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label (show requiredApplicationCount <> " time(s)")
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label ("Loan ID: " <> display (activeDatum ^. #loanId))
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
            button closeCircleIcon (loanBuilderEvent $ RemoveSelectedInterestApplication idx)
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

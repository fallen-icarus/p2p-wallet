module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.AskCloses
  ( 
    askClosesList
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.Plutus
import P2PWallet.Prelude

askClosesList :: ReverseTickerMap -> [(Int,AskClose)] -> [AppNode]
askClosesList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int, AskClose) -> AppNode
    utxoRow (idx,AskClose{..}) = do
      let Loans.AskDatum{..} = fromMaybe def askDatum
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
      hstack
        [ vstack
            [ hstack
                [ label ("Close Loan Ask For " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                , label (showAssetBalance True reverseTickerMap loanAmount)
                    `styleBasic` [textSize 10, textColor white]
                ]
            , spacer_ [width 2]
            , hstack
                [ label (display utxoRef)
                    `styleBasic` [textSize 8, textColor lightGray]
                , filler
                , label ("Duration: " <> show duration <> " Day(s)")
                    `styleBasic` [textSize 8, textColor lightGray]
                ]
            ] `styleBasic` 
                [ padding 10
                , bgColor customGray2
                , radius 5
                , border 1 black
                ]
        , spacer_ [width 3]
        , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
            button closeCircleIcon (loanBuilderEvent $ RemoveSelectedAskClose idx)
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

module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferCloses
  ( 
    offerClosesList
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

offerClosesList :: ReverseTickerMap -> [(Int,OfferClose)] -> [AppNode]
offerClosesList reverseTickerMap = map utxoRow
  where
    utxoRow :: (Int, OfferClose) -> AppNode
    utxoRow (idx,OfferClose{..}) = do
      let Loans.OfferDatum{..} = fromMaybe def offerDatum
          loanAmount = toNativeAsset loanAsset & #quantity .~ loanPrincipal
          duration = calcDaysInPosixPeriod $ fromPlutusTime loanTerm
      hstack
        [ vstack
            [ hstack
                [ label ("Close Loan Offer From " <> walletAlias)
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
            button closeCircleIcon (loanBuilderEvent $ RemoveSelectedOfferClose idx)
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

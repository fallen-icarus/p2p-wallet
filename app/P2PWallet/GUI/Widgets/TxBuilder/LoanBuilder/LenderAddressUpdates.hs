module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.LenderAddressUpdates
  ( 
    lenderAddressUpdatesList
  , editLenderAddressUpdateWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Wallets.LoanWallet
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

lenderAddressUpdatesList :: [(Int,LenderAddressUpdate)] -> [AppNode]
lenderAddressUpdatesList = map utxoRow
  where
    utxoRow :: (Int, LenderAddressUpdate) -> AppNode
    utxoRow s@(idx,LenderAddressUpdate{..}) = do
      let Loans.ActiveDatum{..} = fromMaybe def $ loanUTxOActiveDatum loanUTxO
      hstack
        [ vstack
            [ hstack
                [ label "Update Loan Payment Address"
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
        , hstack
            [ box_ [alignCenter,alignMiddle] $ tooltip_ "Edit Action" [tooltipDelay 0] $
                button editIcon 
                    (loanBuilderEvent $ EditSelectedLenderAddressUpdate $ StartAdding $ Just s)
                  `styleBasic` 
                    [ textSize 10
                    , textColor customBlue
                    , textFont "Remix"
                    , textMiddle
                    , padding 3
                    , radius 3
                    , bgColor transparent
                    , border 0 transparent
                    ]
                  `styleHover` [bgColor customGray1, cursorIcon CursorHand]
            , box_ [alignCenter,alignMiddle] $ tooltip_ "Remove Action" [tooltipDelay 0] $
                button closeCircleIcon (loanBuilderEvent $ RemoveSelectedLenderAddressUpdate idx)
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

editLenderAddressUpdateWidget :: AppNode
editLenderAddressUpdateWidget = do
  let maybeLens' = maybeLens (def,def) $ #txBuilderModel % #loanBuilderModel % #targetAddressUpdate
  centerWidget $ vstack
    [ centerWidgetH $ label "Where would you like future loan payments to go?"
    , spacer_ [width 20]
    , hstack
        [ label "Address:"
        , spacer
        , textField (toLensVL $ maybeLens' % _2 % #newPaymentAddress)
            `styleBasic` [textSize 10, bgColor customGray1, sndColor darkGray]
            `styleFocus` [border 1 customBlue]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ loanBuilderEvent $ EditSelectedLenderAddressUpdate CancelAdding
        , spacer
        , mainButton "Confirm" $ loanBuilderEvent $ EditSelectedLenderAddressUpdate ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

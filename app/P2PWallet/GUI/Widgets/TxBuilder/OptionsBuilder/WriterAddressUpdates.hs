module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.WriterAddressUpdates
  ( 
    writerAddressUpdatesList
  , editWriterAddressUpdateWidget
  ) where

import Monomer as M hiding (duration)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Data.DeFi.CardanoOptions qualified as Options
import P2PWallet.GUI.Colors
import P2PWallet.GUI.Icons
import P2PWallet.GUI.MonomerOptics()
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

writerAddressUpdatesList :: [(Int,WriterAddressUpdate)] -> [AppNode]
writerAddressUpdatesList = map utxoRow
  where
    utxoRow :: (Int, WriterAddressUpdate) -> AppNode
    utxoRow s@(idx,WriterAddressUpdate{..}) = do
      let Options.ActiveDatum{..} = fromMaybe def $ optionsUTxOActiveDatum optionsUTxO
      hstack
        [ vstack
            [ hstack
                [ label ("Update Options Payment Address For " <> walletAlias)
                    `styleBasic` [textSize 10, textColor customBlue]
                , filler
                ]
            , spacer_ [width 2]
            , hstack
                [ label "Contract ID:"
                    `styleBasic` [textSize 8, textColor lightGray]
                , spacer_ [width 5]
                , label (display contractId)
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
                    (optionsBuilderEvent $ EditSelectedWriterAddressUpdate $ StartAdding $ Just s)
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
                button closeCircleIcon (optionsBuilderEvent $ RemoveSelectedWriterAddressUpdate idx)
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

editWriterAddressUpdateWidget :: AppNode
editWriterAddressUpdateWidget = do
  let maybeLens' = maybeLens (def,def) $ #txBuilderModel % #optionsBuilderModel % #targetAddressUpdate
  centerWidget $ vstack
    [ centerWidgetH $ label "Where would you like the payment to go?"
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
        , button "Cancel" $ optionsBuilderEvent $ EditSelectedWriterAddressUpdate CancelAdding
        , spacer
        , mainButton "Confirm" $ optionsBuilderEvent $ EditSelectedWriterAddressUpdate ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

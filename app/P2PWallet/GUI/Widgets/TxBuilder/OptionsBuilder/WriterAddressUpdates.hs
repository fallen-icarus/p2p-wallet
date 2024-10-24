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
          addressTip = unwords
            [ "Payments to"
            , newPaymentWallet ^. #alias <> ":"
            , display $ newPaymentWallet ^. #paymentAddress
            ]
      hstack
        [ vstack
            [ hstack
                [ label "Update Options Payment Address"
                    `styleBasic` [textSize 10, textColor customBlue]
                , spacer_ [width 5]
                , separatorLine `styleBasic` [fgColor darkGray, paddingT 1, paddingB 1]
                , spacer_ [width 5]
                , let prettyRef = display $ optionsUTxO ^. #utxoRef in
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
                , spacer_ [width 5]
                , flip styleBasic [textSize 10] $ tooltip_ addressTip [tooltipDelay 0] $
                    box_ [alignMiddle, onClick $ CopyText $ display $ newPaymentWallet ^. #paymentAddress] $
                      label targetAddressIcon
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

editWriterAddressUpdateWidget :: AppModel -> AppNode
editWriterAddressUpdateWidget AppModel{knownWallets} = do
  let maybeLens' = maybeLens (def,def) $ #txBuilderModel % #optionsBuilderModel % #targetAddressUpdate
      innerDormantStyle = 
        def `styleBasic` [textSize 10, bgColor customGray3, border 1 black]
            `styleHover` [textSize 10, bgColor customGray2, border 1 black]
      innerFocusedStyle = 
        def `styleFocus` [textSize 10, bgColor customGray3, border 1 customBlue]
            `styleFocusHover` [textSize 10, bgColor customGray2, border 1 customBlue]
  centerWidget $ vstack
    [ centerWidgetH $ label "Where would you like the payment to go?"
    , spacer_ [width 20]
    , centerWidgetH $ hstack
        [ label "Address:"
        , spacer
        , textDropdown_ 
              (toLensVL $ maybeLens' % _2 % #newPaymentWallet) 
              (knownWallets ^. #paymentWallets) 
              (view #alias) -- The dropdown displays the wallet's alias in the menu.
              [itemBasicStyle innerDormantStyle, itemSelectedStyle innerFocusedStyle]
            `styleBasic` 
              [ bgColor customGray2
              , width 150
              , border 1 black
              , textSize 10
              ]
            `styleHover` [bgColor customGray1, cursorIcon CursorHand]
        ]
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ optionsBuilderEvent $ EditSelectedWriterAddressUpdate CancelAdding
        , spacer
        , mainButton "Confirm" $ optionsBuilderEvent $ EditSelectedWriterAddressUpdate ConfirmAdding
        ]
    ] `styleBasic` [radius 20, bgColor customGray3, padding 20]

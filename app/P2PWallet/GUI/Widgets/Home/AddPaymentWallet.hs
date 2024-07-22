module P2PWallet.GUI.Widgets.Home.AddPaymentWallet 
  (
    addPaymentWalletWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

-- A widget for adding either a paired wallet or watched wallet.
addPaymentWalletWidget :: AppModel -> AppNode
addPaymentWalletWidget model@AppModel{homeModel} = do
  let isPairing = homeModel ^. #newPaymentWallet % #pairing
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  centerWidget $ vstack
    [ hgrid
        [ optionButton_ "Pair" True (toLensVL $ #homeModel % #newPaymentWallet % #pairing) 
            [optionButtonOffStyle offStyle]
            `styleBasic` 
              [ bgColor customGray3
              , textColor customBlue
              , radius 0
              , border 0 transparent
              ]
        , optionButton_ "Watch" False (toLensVL $ #homeModel % #newPaymentWallet % #pairing)
            [optionButtonOffStyle offStyle]
            `styleBasic` 
              [ bgColor customGray3
              , textColor customBlue
              , radius 0
              , border 0 transparent
              ]
        ]
    , zstack 
        [ widgetIf isPairing $ centerWidgetH $ pairPaymentWidget model
        , widgetIf (not isPairing) $ centerWidgetH $ watchPaymentWidget model
        ] `styleBasic` 
            [ bgColor customGray3
            , paddingT 0
            , paddingL 20
            , paddingR 20
            , paddingB 20
            , radius 0
            ]
    ]

pairPaymentWidget :: AppModel -> AppNode
pairPaymentWidget model = do
  let boolLens' = boolLens 0 (#homeModel % #newPaymentWallet % #stakeAddressIndex)
      numLens' = maybeLens 0 (#homeModel % #newPaymentWallet % #stakeAddressIndex)

      editFields = 
        vstack_ [childSpacing]
          [ hstack
              [ label "What is a paired wallet?"
                  `styleBasic` [textFont "Italics"]
              , mainButton helpIcon (Alert whatIsPairedWalletMsg)
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ]
          , hstack 
              [ label "Payment Wallet Name:"
              , spacer
              , textField_ (toLensVL $ #homeModel % #newPaymentWallet % #alias) [placeholder "Personal"] 
                  `styleBasic` [width 300, bgColor customGray1, sndColor darkGray]
                  `styleFocus` [border 1 customBlue]
              ]
          , hstack 
              [ label "Payment Key Address Index:"
              , spacer
              , numericField (toLensVL $ #homeModel % #newPaymentWallet % #paymentAddressIndex)
                  `styleBasic` [width 100, bgColor customGray1]
                  `styleFocus` [border 1 customBlue]
              , mainButton helpIcon (Alert paymentAddressIndexMsg)
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ]
          , hstack 
              [ label "Enable Staking"
              , spacer
              , checkbox_ (toLensVL boolLens') [checkboxSquare]
                  `styleBasic` [fgColor customGray1, hlColor customBlue]
              ]
          , hstack
              [ spacer_ [width 20]
              , label cornerDownRightArrowIcon 
                  `styleBasic` [paddingT 8, paddingR 5, textFont "Remix"]
              , label "Staking Key Address Index:"
              , spacer
              , numericField (toLensVL numLens')
                  `styleBasic` [width 100, bgColor customGray1]
                  `styleFocus` [border 1 customBlue]
              , mainButton helpIcon (Alert stakeAddressIndexMsg)
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ] `nodeVisible` (model ^. boolLens')
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ HomeEvent $ PairPaymentWallet CancelAdding
        , spacer
        , mainButton "Pair" $ HomeEvent $ PairPaymentWallet ConfirmAdding
        ]
    ] `styleBasic` [bgColor transparent, padding 20]

watchPaymentWidget :: AppModel -> AppNode
watchPaymentWidget _ = do
  let editFields = 
        vstack_ [childSpacing]
          [ hstack
              [ label "What is a watched wallet?"
                  `styleBasic` [textFont "Italics"]
              , mainButton helpIcon (Alert whatIsWatchedWalletMsg)
                  `styleBasic`
                    [ border 0 transparent
                    , radius 20
                    , bgColor transparent
                    , textColor customBlue
                    , textMiddle
                    , textFont "Remix"
                    ]
                  `styleHover` [bgColor customGray2, cursorIcon CursorHand]
              ]
          , hstack 
              [ label "Payment Wallet Name:"
              , spacer
              , textField_ 
                    (toLensVL $ #homeModel % #newPaymentWallet % #alias) 
                    [placeholder "Personal Watched"] 
                  `styleBasic` [width 300, bgColor customGray1, sndColor darkGray]
                  `styleFocus` [border 1 customBlue]
              ]
          , hstack 
              [ label "Payment Address:"
              , spacer
              , textField (toLensVL $ #homeModel % #newPaymentWallet % #paymentAddress)
                  `styleBasic` [textSize 12, bgColor customGray1]
                  `styleFocus` [border 1 customBlue]
              ]
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ HomeEvent $ WatchPaymentWallet CancelAdding
        , spacer
        , mainButton "Watch" $ HomeEvent $ WatchPaymentWallet ConfirmAdding
        ]
    ] `styleBasic` [bgColor transparent, padding 20]

module P2PWallet.GUI.Widgets.Delegation.AddStakeWallet 
  (
    addStakeWalletWidget
  ) where

import Monomer

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Colors
import P2PWallet.GUI.HelpMessages
import P2PWallet.GUI.Icons
import P2PWallet.GUI.Widgets.Internal.Custom
import P2PWallet.Prelude

addStakeWalletWidget :: AppModel -> AppNode
addStakeWalletWidget AppModel{..} = do
  let isPairing = delegationModel ^. #newStakeWallet % #pairing
      offStyle = def 
        `styleBasic` [ bgColor customGray1 , textColor white ]
        `styleHover` [ textColor lightGray ]
  centerWidget $ vstack
    [ hgrid
        [ optionButton_ "Pair" True (toLensVL $ #delegationModel % #newStakeWallet % #pairing) 
            [optionButtonOffStyle offStyle]
            `styleBasic` 
              [ bgColor customGray3
              , textColor customBlue
              , radius 0
              , border 0 transparent
              ]
        , optionButton_ "Watch" False (toLensVL $ #delegationModel % #newStakeWallet % #pairing)
            [optionButtonOffStyle offStyle]
            `styleBasic` 
              [ bgColor customGray3
              , textColor customBlue
              , radius 0
              , border 0 transparent
              ]
        ]
    , zstack 
        [ widgetIf isPairing $ centerWidgetH pairStakeWidget
        , widgetIf (not isPairing) $ centerWidgetH watchStakeWidget
        ] `styleBasic` 
            [ bgColor customGray3
            , paddingT 0
            , paddingL 20
            , paddingR 20
            , paddingB 20
            , radius 0
            ]
    ]

pairStakeWidget :: AppNode
pairStakeWidget = do
  let editFields = 
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
              [ label "Stake Wallet Name:"
              , spacer
              , textField_ (toLensVL $ #delegationModel % #newStakeWallet % #alias) 
                  [placeholder "Personal Stake"]
                  `styleBasic` [width 300]
              ]
          , hstack 
              [ label "Stake Key Address Index:"
              , spacer
              , numericField (toLensVL $ #delegationModel % #newStakeWallet % #stakeAddressIndex)
                  `styleBasic` [width 100]
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
              ]
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ DelegationEvent $ PairStakeWallet CancelAdding
        , spacer
        , mainButton "Pair" $ DelegationEvent $ PairStakeWallet ConfirmAdding
        ]
    ] `styleBasic` [bgColor transparent, padding 20]

watchStakeWidget :: AppNode
watchStakeWidget = do
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
              [ label "Stake Wallet Name:"
              , spacer
              , textField_ 
                  (toLensVL $ #delegationModel % #newStakeWallet % #alias) 
                  [placeholder "Personal Stake Watched"] 
                  `styleBasic` [width 300]
              ]
          , hstack 
              [ label "Stake Address:"
              , spacer
              , textField (toLensVL $ #delegationModel % #newStakeWallet % #stakeAddress)
              ]
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , button "Cancel" $ DelegationEvent $ WatchStakeWallet CancelAdding
        , spacer
        , mainButton "Watch" $ DelegationEvent $ WatchStakeWallet ConfirmAdding
        ]
    ] `styleBasic` [bgColor transparent, padding 20]

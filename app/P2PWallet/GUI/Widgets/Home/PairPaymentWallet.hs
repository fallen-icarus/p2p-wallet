{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.PairPaymentWallet where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude

pairPaymentWidget :: AppWenv -> AppModel -> AppNode
pairPaymentWidget wenv model = do
  let boolLens' = boolLens "1852H/1815H/0H/2/0" (homeModel . newPaymentWallet . stakeKeyPath)
      textLens' = maybeLens "" (homeModel . newPaymentWallet . stakeKeyPath)
      sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "Payment Wallet Name:"
              , spacer
              , textField_ (homeModel . newPaymentWallet . alias) [placeholder "Personal"] 
              ]
          , hstack 
              [ label "Payment Derivation Path:"
              , spacer
              , textField_ (homeModel . newPaymentWallet . paymentKeyPath) [maxLength 19]
                  `styleBasic` [width 200]
              ]
          , hstack 
              [ label "Enable Staking"
              , spacer
              , checkbox_ boolLens' [checkboxSquare]
              ]
          , hstack
              [ spacer
              , spacer
              , spacer
              , spacer
              , label "Staking Derivation Path:"
              , spacer
              , textField_ textLens' [maxLength 19]
                  `styleBasic` [width 200]
              ] `nodeVisible` (model ^# boolLens')
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , mainButton "Pair" $ HomeEvent $ PairPaymentWallet ConfirmAdding
        , spacer
        , button "Cancel" $ HomeEvent $ PairPaymentWallet CancelAdding
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

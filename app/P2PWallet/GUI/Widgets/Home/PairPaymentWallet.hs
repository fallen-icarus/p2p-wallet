{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.PairPaymentWallet where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude

pairPaymentWidget :: AppWenv -> AppModel -> AppNode
pairPaymentWidget wenv _ = do
  let boolLens' = boolLens 0 (homeModel . newPaymentWallet . stakeAddressIndex)
      -- numLens' = maybeLens 0 (homeModel . newPaymentWallet . stakeAddressIndex)
      sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "Payment Wallet Name:"
              , spacer
              , textField_ (homeModel . newPaymentWallet . alias) [placeholder "Personal"] 
                  `styleBasic` [width 400]
              ]
          , hstack 
              [ label "Payment Address Index:"
              , spacer
              , numericField (homeModel . newPaymentWallet . paymentAddressIndex)
                  `styleBasic` [width 100]
              ]
          , hstack 
              [ label "Enable Staking"
              , spacer
              , checkbox_ boolLens' [checkboxSquare]
              ]
          -- , hstack
          --     [ spacer
          --     , spacer
          --     , spacer
          --     , spacer
          --     , label "Staking Address Index:"
          --     , spacer
          --     , numericField numLens' 
          --         `styleBasic` [width 100]
          --     ] `nodeVisible` (model ^# boolLens')
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

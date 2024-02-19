{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Home.WatchPaymentWallet where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude

watchPaymentWidget :: AppWenv -> AppModel -> AppNode
watchPaymentWidget wenv _ = do
  let sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "Payment Wallet Name:"
              , spacer
              , textField_ (homeModel . newPaymentWallet . alias) [placeholder "Personal Watched"]
              ]
          , hstack 
              [ label "Payment Address:"
              , spacer
              , textField (homeModel . newPaymentWallet . paymentAddress)
              ]
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , mainButton "Watch" $ HomeEvent $ WatchPaymentWallet ConfirmAdding
        , spacer
        , button "Cancel" $ HomeEvent $ WatchPaymentWallet CancelAdding
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

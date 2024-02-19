{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Delegation.WatchStakeWallet where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude

watchStakeWidget :: AppWenv -> AppModel -> AppNode
watchStakeWidget wenv _ = do
  let sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "Stake Wallet Name:"
              , spacer
              , textField_ 
                  (delegationModel . newStakeWallet . alias) 
                  [placeholder "Personal Stake Watched"]
              ]
          , hstack 
              [ label "Stake Address:"
              , spacer
              , textField (delegationModel . newStakeWallet . stakeAddress)
              ]
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , mainButton "Watch" $ DelegationEvent $ WatchStakeWallet ConfirmAdding
        , spacer
        , button "Cancel" $ DelegationEvent $ WatchStakeWallet CancelAdding
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

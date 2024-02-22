{-# LANGUAGE RecordWildCards #-}

module P2PWallet.GUI.Widgets.Delegation.PairStakeWallet where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude

pairStakeWidget :: AppWenv -> AppModel -> AppNode
pairStakeWidget wenv _ = do
  let sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = 
        vstack_ [childSpacing]
          [ hstack 
              [ label "Stake Wallet Name:"
              , spacer
              , textField_ 
                  (delegationModel . newStakeWallet . alias) 
                  [placeholder "Personal Stake"]
              ]
          -- , hstack 
          --     [ label "Stake Derivation Path:"
          --     , spacer
          --     , textField_ (delegationModel . newStakeWallet . stakeKeyPath) [maxLength 19]
          --         `styleBasic` [width 200]
          --     ]
          ]

  vstack 
    [ editFields
    , spacer
    , hstack 
        [ filler
        , mainButton "Pair" $ DelegationEvent $ PairStakeWallet ConfirmAdding
        , spacer
        , button "Cancel" $ DelegationEvent $ PairStakeWallet CancelAdding
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

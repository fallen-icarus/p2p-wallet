module P2PWallet.GUI.Widgets.TxBuilder.AddInput where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.Internal.Custom

addInputWidget :: AppWenv -> AppModel -> AppNode
addInputWidget wenv _ = 
  let sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = vstack_ [childSpacing]
        [ label "UTxO:"
        , spacer
        , textField (txBuilderModel . newInput . _2 . utxoRef)
        ]

  in centerWidget $ vstack 
       [ editFields
       , spacer
       , hstack 
           [ filler
           , mainButton "Confirm" $ TxBuilderEvent InsertNewInput 
           , spacer
           , button "Cancel" $ TxBuilderEvent $ ChangeBuilderScene BuilderSummary
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]

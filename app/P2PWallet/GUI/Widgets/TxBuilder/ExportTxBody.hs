module P2PWallet.GUI.Widgets.TxBuilder.ExportTxBody where

import Monomer
import Monomer.Lens qualified as L

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.Internal.Custom

exportTxBodyWidget :: AppWenv -> AppModel -> AppNode
exportTxBodyWidget wenv model = 
  let sectionBg = wenv ^. L.theme . L.sectionColor

      editFields = vstack_ [childSpacing]
        [ label $ unwords 
            [ "Enter the absolute filepath to the destination below."
            , "Environment variables and shortcuts like ~/ are supported."
            ]
        , spacer
        , textField_ extraTextField [placeholder "$HOME/Documents/tx.body"]
        ]

  in centerWidget $ vstack 
       [ editFields
       , spacer
       , hstack 
           [ filler
           , mainButton "Confirm" (TxBuilderEvent ExportTxBody) 
               `nodeEnabled` (model ^. extraTextField /= "")
           , spacer
           , button "Cancel" $ TxBuilderEvent $ ChangeBuilderScene BuilderSummary
           ]
       ] `styleBasic` [bgColor sectionBg, padding 20]

module P2PWallet.GUI.Widgets.TxBuilder where

import Monomer

import P2PWallet.Data.App
import P2PWallet.Data.Lens
import P2PWallet.Prelude
import P2PWallet.GUI.Widgets.TxBuilder.AddCertificate
import P2PWallet.GUI.Widgets.TxBuilder.AddChangeOutput
import P2PWallet.GUI.Widgets.TxBuilder.AddInput
import P2PWallet.GUI.Widgets.TxBuilder.AddOutput
import P2PWallet.GUI.Widgets.TxBuilder.ExportTxBody
import P2PWallet.GUI.Widgets.TxBuilder.Summary

txBuilderWidget :: AppWenv -> AppModel -> AppNode
txBuilderWidget wenv model = do
    zstack 
      [ summaryWidget wenv model
          `nodeVisible` (model ^. txBuilderModel . scene == BuilderSummary)
      , addOutputWidget wenv model
          `nodeVisible` (model ^. txBuilderModel . scene == BuilderAddNewOutput)
      , addInputWidget wenv model
          `nodeVisible` (model ^. txBuilderModel . scene == BuilderAddNewInput)
      , addCertificateWidget wenv model
          `nodeVisible` (model ^. txBuilderModel . scene == BuilderAddNewCertificate)
      , addChangeOutputWidget wenv model
          `nodeVisible` (model ^. txBuilderModel . scene == BuilderAddChangeOutput)
      , exportTxBodyWidget wenv model
          `nodeVisible` (model ^. txBuilderModel . scene == BuilderGetExportDestination)
      ]


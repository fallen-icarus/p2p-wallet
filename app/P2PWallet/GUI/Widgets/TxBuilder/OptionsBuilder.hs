module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder
  ( 
    optionsActionCount
  , proposalCreationsList
  , editProposalCreationWidget
  , proposalClosesList
  , proposalUpdatesList
  , editProposalUpdateWidget
  , proposalPurchasesList
  ) where

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalCloses
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalCreations
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalPurchases
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalUpdates
import P2PWallet.Prelude

optionsActionCount :: OptionsBuilderModel -> Int
optionsActionCount OptionsBuilderModel{..} = sum
  [ length proposalCreations
  , length proposalCloses
  , length proposalUpdates
  , length proposalPurchases
  ]

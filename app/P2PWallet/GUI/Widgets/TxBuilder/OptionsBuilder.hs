module P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder
  ( 
    optionsActionCount
  , proposalCreationsList
  , editProposalCreationWidget
  , proposalClosesList
  , proposalUpdatesList
  , editProposalUpdateWidget
  , proposalPurchasesList
  , expiredOptionsClosesList
  , writerAddressUpdatesList
  , editWriterAddressUpdateWidget
  , optionsKeyBurnsList
  , optionsContractExecutionsList
  ) where

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ContractExecutions
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ExpiredCloses
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.KeyBurns
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalCloses
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalCreations
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalPurchases
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.ProposalUpdates
import P2PWallet.GUI.Widgets.TxBuilder.OptionsBuilder.WriterAddressUpdates
import P2PWallet.Prelude

optionsActionCount :: OptionsBuilderModel -> Int
optionsActionCount OptionsBuilderModel{..} = sum
  [ length proposalCreations
  , length proposalCloses
  , length proposalUpdates
  , length proposalPurchases
  , length expiredCloses
  , length addressUpdates
  , length keyBurns
  , length contractExecutions
  ]

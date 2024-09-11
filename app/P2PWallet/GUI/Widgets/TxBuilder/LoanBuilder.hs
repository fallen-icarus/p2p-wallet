module P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder
  ( 
    loanActionCount
  , askCreationsList
  , editAskCreationWidget
  , askClosesList
  , askUpdatesList
  , editAskUpdateWidget
  , offerCreationsList
  , editOfferCreationWidget
  , offerClosesList
  , offerUpdatesList
  , editOfferUpdateWidget
  , offerAcceptancesList
  , editOfferAcceptanceWidget
  , loanPaymentsList
  , editLoanPaymentWidget
  , interestApplicationsList
  , expiredClaimsList
  , loanKeyBurnsList
  , lenderAddressUpdatesList
  , editLenderAddressUpdateWidget
  ) where

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.AskCloses
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.AskCreations
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.AskUpdates
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.ExpiredClaims
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.InterestApplications
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.KeyBurns
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.LenderAddressUpdates
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.LoanPayments
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferAcceptances
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferCloses
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferCreations
import P2PWallet.GUI.Widgets.TxBuilder.LoanBuilder.OfferUpdates
import P2PWallet.Prelude

loanActionCount :: LoanBuilderModel -> Int
loanActionCount LoanBuilderModel{..} = sum
  [ length askCreations
  , length askCloses
  , length askUpdates
  , length offerCreations
  , length offerCloses
  , length offerUpdates
  , length offerAcceptances
  , length loanPayments
  , length interestApplications
  , length expiredClaims
  , length keyBurns
  , length addressUpdates
  ]

module P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder
  ( 
    aftermarketActionCount
  , saleCreationsList
  , editSaleCreationWidget
  , saleClosesList
  , saleUpdatesList
  , editSaleUpdateWidget
  , loanKeySpotPurchasesList
  , editLoanKeySpotPurchase
  , bidCreationsList
  , editBidCreationWidget
  , bidClosesList
  , bidUpdatesList
  , editBidUpdateWidget
  , claimBidAcceptancesList
  , editClaimBidAcceptance
  , loanKeyBidClaimsList
  , editLoanKeyBidClaim
  , optionsKeySpotPurchasesList
  , editOptionsKeySpotPurchase
  , spotBidAcceptancesList
  , optionsKeyBidClaimsList
  , editOptionsKeyBidClaim
  , bidUnlocksList
  ) where

import P2PWallet.Data.AppModel
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.BidCloses
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.BidCreations
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.BidUnlocks
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.BidUpdates
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.ClaimBidAcceptances
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.LoanKeyBidClaims
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.LoanKeySpotPurchases
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.OptionsKeyBidClaims
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.OptionsKeySpotPurchases
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleCloses
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleCreations
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SaleUpdates
import P2PWallet.GUI.Widgets.TxBuilder.AftermarketBuilder.SpotBidAcceptances
import P2PWallet.Prelude

aftermarketActionCount :: AftermarketBuilderModel -> Int
aftermarketActionCount AftermarketBuilderModel{..} = sum
  [ length saleCreations
  , length saleCloses
  , length saleUpdates
  , length loanKeySpotPurchases
  , length bidCreations
  , length bidCloses
  , length bidUpdates
  , length claimBidAcceptances
  , length loanKeyBidClaims
  , length optionsKeySpotPurchases
  , length spotBidAcceptances
  , length optionsKeyBidClaims
  , length bidUnlocks
  ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Sell scene is dedicated to the `MarketWallet` being used to sell Key NFTs.

-}
module P2PWallet.Data.AppModel.AftermarketModel.SellerModel
  ( AftermarketSellerScene(..)
  , AftermarketSellerEvent(..)
  , AftermarketSellerModel(..)

  , module P2PWallet.Data.AppModel.AftermarketModel.SellerModel.OpenSalesFilterModel
  )where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel
import P2PWallet.Data.AppModel.AftermarketModel.SellerModel.OpenSalesFilterModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Prelude

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
data AftermarketSellerScene
  -- | All current sales that belong to this seller.
  = OpenAftermarketSales
  -- | All bids to this seller.
  | CurrentAftermarketBids
  -- | The transaction history for the market address.
  | AftermarketTransactions
  deriving (Eq,Show)

-------------------------------------------------
-- Sell Scene Events
-------------------------------------------------
-- | The possible UI events on the Sell Scene
data AftermarketSellerEvent
  -- | Change the Sell subscene to the specified subscene.
  = ChangeAftermarketSellerScene AftermarketSellerScene
  -- | Inspect an aftermarket sale.
  | InspectAftermarketSale AftermarketUTxO
  -- | Stop inspecting the sale.
  | CloseInspectedAftermarketSale
  -- | Add the new sale close to the transaction builder.
  | AddSelectedSaleClose AftermarketUTxO
  -- | Add the new sale update to the transaction builder.
  | AddSelectedSaleUpdate (AddEvent AftermarketUTxO SaleUpdate)
  -- | Remove the selected NFT from the SaleUpdate from the seller page.
  | RemoveSellerSaleUpdateNft NativeAsset
  -- | Add the selected NFT to the Home page's queue for creating a new sale with the NFT.
  | AddNftToHomeBatch NativeAsset
  -- | Verify the open sales filter model has valid information.
  | CheckOpenSalesFilterModel
  -- | Reset the open sales fitler model.
  | ResetOpenSalesFilters
  -- | Inspect loan history for a key nft being sold.
  | InspectSellerLoanHistory Loans.LoanId
  -- | Stop inspecting the loan's history.
  | CloseInspectedSellerLoanHistory
  -- | Lookup borrower information corresponding to a loan in the batch being sold.
  | InspectSellerBorrowerInformation (Loans.BorrowerId, PaymentAddress)
  -- | Stop inspecting the borrower's information.
  | CloseInspectedSellerBorrowerInformation

-------------------------------------------------
-- Sell State
-------------------------------------------------
-- | The state for the Sell scene.
data AftermarketSellerModel = AftermarketSellerModel
  -- | The current sell subscene.
  { scene :: AftermarketSellerScene
  , inspectedSale :: Maybe AftermarketUTxO
  -- | The new sale update.
  , newSaleUpdate :: Maybe (AftermarketUTxO,NewSaleCreation)
  , showSaleFilter :: Bool
  -- | The open sales filter model.
  , salesFilterModel :: OpenSalesFilterModel
  -- | Focused loan history.
  , inspectedLoan :: Maybe Loans.LoanId
  -- | Focused borrower.
  , inspectedBorrower :: Maybe (Loans.BorrowerId,PaymentAddress)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AftermarketSellerModel

instance Default AftermarketSellerModel where
  def = AftermarketSellerModel
    { scene = OpenAftermarketSales
    , inspectedSale = Nothing
    , newSaleUpdate = Nothing
    , showSaleFilter = False
    , salesFilterModel = def
    , inspectedLoan = Nothing
    , inspectedBorrower = Nothing
    }

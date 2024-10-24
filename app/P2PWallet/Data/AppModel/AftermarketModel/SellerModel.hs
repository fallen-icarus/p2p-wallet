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

  , SellerTxFilterModel(..)

  , module P2PWallet.Data.AppModel.AftermarketModel.SellerModel.CurrentBidsFilterModel
  , module P2PWallet.Data.AppModel.AftermarketModel.SellerModel.OpenSalesFilterModel
  )where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.AftermarketModel.SellerModel.CurrentBidsFilterModel
import P2PWallet.Data.AppModel.AftermarketModel.SellerModel.OpenSalesFilterModel
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel
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
  -- | Inspect an aftermarket bid to the seller.
  | InspectAftermarketSellerBid AftermarketUTxO
  -- | Stop inspecting the bid to the seller.
  | CloseInspectedAftermarketSellerBid
  -- | Add the new claim bid acceptance to the transaction builder.
  | AddSelectedClaimBidAcceptance (AddEvent AftermarketUTxO ClaimBidAcceptance)
  -- | Add the new spot bid acceptance to the transaction builder.
  | AddSelectedSpotBidAcceptance AftermarketUTxO
  -- | Inspect a Seller Transaction.
  | InspectSellerTransaction Transaction
  -- | Stop inspecting the transaction.
  | CloseInspectedSellerTransaction
  -- | Verify the bid tx filter model has valid information.
  | CheckSellerTxFilters
  -- | Reset the bid tx fitler model.
  | ResetSellerTxFilters
  -- | Verify the bids filter model has valid information.
  | CheckCurrentBidsFilters
  -- | Reset the bids fitler model.
  | ResetCurrentBidsFilters
  -- | Add the new bid unlock to the transaction builder.
  | AddSelectedBidUnlock AftermarketUTxO

-------------------------------------------------
-- Transaction Filter Model
-------------------------------------------------
data SellerTxFilterModel = SellerTxFilterModel
  -- | The date range for displaying transactions.
  { dateRange :: (Maybe Day, Maybe Day)
  -- | The nft type. Nothing for any type.
  , nftType :: Maybe KeyNftType
  -- | The sale must be for the specified policy id. This is only used in conjunction with OtherNft
  -- type.
  , policyId :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SellerTxFilterModel

instance Default SellerTxFilterModel where
  def = SellerTxFilterModel
    { dateRange = (Nothing,Nothing)
    , nftType = def
    , policyId = ""
    }

-------------------------------------------------
-- Sell State
-------------------------------------------------
-- | The state for the Sell scene.
data AftermarketSellerModel = AftermarketSellerModel
  -- | The current sell subscene.
  { scene :: AftermarketSellerScene
  -- | Inspect the NFTs being sold in the batch.
  , inspectedSale :: Maybe AftermarketUTxO
  -- | The new sale update.
  , newSaleUpdate :: Maybe (AftermarketUTxO,NewSaleCreation)
  -- | Whether the sales filter model should be open.
  , showSaleFilter :: Bool
  -- | The open sales filter model.
  , salesFilterModel :: OpenSalesFilterModel
  -- | Focused loan history.
  , inspectedLoan :: Maybe Loans.LoanId
  -- | Focused borrower.
  , inspectedBorrower :: Maybe (Loans.BorrowerId,PaymentAddress)
  -- | Whether the bids filter model should be open.
  , showBidFilter :: Bool
  -- | The bids filter model.
  , bidsFilterModel :: CurrentBidsFilterModel
  -- | Inspect the NFTs being asked for in the bid's batch.
  , inspectedBid :: Maybe AftermarketUTxO
  -- | The new claim bid acceptance.
  , newClaimBidAcceptance :: Maybe ClaimBidAcceptance
  -- | Focused seller transaction details. This shows all transactions for the market address,
  -- even those initiated by bidders.
  , inspectedTransaction :: Maybe Transaction
  -- | Whether to show the filter widget for transactions.
  , showTransactionFilter :: Bool
  -- | The transaction filter model.
  , txFilterModel :: SellerTxFilterModel
  -- | The transaction filter model scene.
  , txFilterScene :: FilterScene
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
    , showBidFilter = False
    , bidsFilterModel = def
    , inspectedBid = Nothing
    , newClaimBidAcceptance = Nothing
    , inspectedTransaction = Nothing
    , showTransactionFilter = False
    , txFilterModel = def
    , txFilterScene = FilterScene
    }

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Buy scene is dedicated to the `MarketWallet` being used to buy Key NFTs.

-}
module P2PWallet.Data.AppModel.AftermarketModel.BuyerModel
  ( AftermarketBuyerScene(..)
  , AftermarketBuyerEvent(..)
  , AftermarketBuyerModel(..)

  , BidTxFilterModel(..)

  , module P2PWallet.Data.AppModel.AftermarketModel.BuyerModel.AllSalesFilterModel
  , module P2PWallet.Data.AppModel.AftermarketModel.BuyerModel.OwnBidsFilterModel
  )where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.AftermarketModel.BuyerModel.AllSalesFilterModel
import P2PWallet.Data.AppModel.AftermarketModel.BuyerModel.OwnBidsFilterModel
import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
data AftermarketBuyerScene
  -- | All current sales for a specific policy id.
  = PolicyAftermarketSales
  -- | All bids belonging to the bidder credential.
  | OwnAftermarketBids
  -- | The transaction history for the bidder credential.
  | AftermarketBidTransactions
  deriving (Eq,Show)

-------------------------------------------------
-- Buy Scene Events
-------------------------------------------------
-- | The possible UI events on the Buy Scene
data AftermarketBuyerEvent
  -- | Change the Buy subscene to the specified subscene.
  = ChangeAftermarketBuyerScene AftermarketBuyerScene
  -- | Set the new policy id as the selected policy id.
  | SetNewSalePolicyId (AddEvent Text CurrencySymbol)
  -- | Inspect an aftermarket sale.
  | InspectAftermarketBuyerSale AftermarketUTxO
  -- | Stop inspecting the sale.
  | CloseInspectedAftermarketBuyerSale
  -- | Inspect loan history for a key nft being sold.
  | InspectBuyerLoanHistory Loans.LoanId
  -- | Stop inspecting the loan's history.
  | CloseInspectedBuyerLoanHistory
  -- | Lookup borrower information corresponding to a loan in the batch being sold.
  | InspectBuyerBorrowerInformation (Loans.BorrowerId, PaymentAddress)
  -- | Stop inspecting the borrower's information.
  | CloseInspectedBuyerBorrowerInformation
  -- | Reset the all sales fitler model.
  | ResetAllSalesFilters
  -- | Purchase a spot sale for cardano-loans loan keys.
  | PurchaseLoanKeySpot (AddEvent (AftermarketUTxO,[LoanUTxO]) LoanKeySpotPurchase)
  -- | Create a bid. Specify the key type since some key types require certain bid types.
  | CreateBid (AddEvent AftermarketUTxO BidCreation)
  -- | Remove the selected NFT from the BidCreation from the buyer page.
  | RemoveBuyerBidCreationNft NativeAsset
  -- | Inspect an aftermarket bid.
  | InspectAftermarketBid AftermarketUTxO
  -- | Stop inspecting the bid.
  | CloseInspectedAftermarketBid
  -- | Add the new bid close to the transaction builder.
  | AddSelectedBidClose AftermarketUTxO
  -- | Add the new bid update to the transaction builder.
  | AddSelectedBidUpdate (AddEvent AftermarketUTxO BidUpdate)
  -- | Remove the selected NFT from the BidUpdate from the buyer page.
  | RemoveBuyerBidUpdateNft NativeAsset
  -- | Claim an accepted bid for cardano-loans loan keys.
  | ClaimAcceptedLoanKeyBid (AddEvent (AftermarketUTxO,[LoanUTxO]) LoanKeyAcceptedBidClaim)
  -- | Inspect a Bid Transaction.
  | InspectBidTransaction Transaction
  -- | Stop inspecting the transaction.
  | CloseInspectedBidTransaction
  -- | Verify the bid tx filter model has valid information.
  | CheckBidTxFilters
  -- | Reset the bid tx fitler model.
  | ResetBidTxFilters
  -- | Verify the own bids filter model has valid information.
  | CheckOwnBidsFilters
  -- | Reset the own bids fitler model.
  | ResetOwnBidsFilters
  -- | Purchase a spot sale for cardano-options options keys.
  | PurchaseOptionsKeySpot (AddEvent (AftermarketUTxO,[OptionsUTxO]) OptionsKeySpotPurchase)
  -- | Claim an accepted bid for cardano-options options keys.
  | ClaimAcceptedOptionsKeyBid (AddEvent (AftermarketUTxO,[OptionsUTxO]) OptionsKeyAcceptedBidClaim)

-------------------------------------------------
-- Transaction Filter Model
-------------------------------------------------
data BidTxFilterModel = BidTxFilterModel
  -- | The date range for displaying transactions.
  { dateRange :: (Maybe Day, Maybe Day)
  -- | The nft type. Nothing for any type.
  , nftType :: Maybe KeyNftType
  -- | The sale must be for the specified policy id. This is only used in conjunction with OtherNft
  -- type.
  , policyId :: Text
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''BidTxFilterModel

instance Default BidTxFilterModel where
  def = BidTxFilterModel
    { dateRange = (Nothing,Nothing)
    , nftType = def
    , policyId = ""
    }

-------------------------------------------------
-- Buy State
-------------------------------------------------
-- | The state for the Buy scene.
data AftermarketBuyerModel = AftermarketBuyerModel
  -- | The current buy subscene.
  { scene :: AftermarketBuyerScene
  -- | Inspect the NFTs being sold in the batch.
  , inspectedSale :: Maybe AftermarketUTxO
  -- | Whether the sales filter model should be open.
  , showSaleFilter :: Bool
  -- | The all sales filter model.
  , salesFilterModel :: AllSalesFilterModel
  -- | Focused loan history.
  , inspectedLoan :: Maybe Loans.LoanId
  -- | Focused borrower.
  , inspectedBorrower :: Maybe (Loans.BorrowerId,PaymentAddress)
  -- | A user friendly method for choosing key nfts or "other" tokens.
  , nftType :: KeyNftType
  -- | The sales must be for the specified policy id. This is only used in conjunction with OtherNft
  -- type.
  , selectedPolicyId :: Maybe CurrencySymbol
  -- | The new policy id to lookup sales for.
  , newPolicyId :: Text
  -- | Whether the policy id widget should be open.
  , choosingPolicyId :: Bool
  -- | The new loan key spot purchase.
  , newLoanKeySpotPurchase :: Maybe NewLoanKeySpotPurchase
  -- | The new bid creation.
  , newBidCreation :: Maybe NewBidCreation
  -- | Inspect the NFTs being bid in the batch.
  , inspectedBid :: Maybe AftermarketUTxO
  -- | Whether the bid filter model should be open.
  , showBidFilter :: Bool
  -- | The own bids filter model.
  , bidsFilterModel :: OwnBidsFilterModel
  -- | The new bid update.
  , newBidUpdate :: Maybe (AftermarketUTxO,NewBidCreation)
  -- | The new loan key accepted bid claim.
  , newLoanKeyBidClaim :: Maybe NewLoanKeyAcceptedBidClaim
  -- | Whether the lender address widget is needed.
  , showBidClaimLenderAddressWidget :: Bool
  -- | Whether the lender address widget is needed.
  , showSpotPurchaseLenderAddressWidget :: Bool
  -- | Focused bid transaction details.
  , inspectedBidTransaction :: Maybe Transaction
  -- | Whether to show the filter widget for transactions.
  , showTransactionFilter :: Bool
  -- | The transaction filter model.
  , txFilterModel :: BidTxFilterModel
  -- | The transaction filter model scene.
  , txFilterScene :: FilterScene
  -- | The new options key spot purchase.
  , newOptionsKeySpotPurchase :: Maybe NewOptionsKeySpotPurchase
  -- | The new options key accepted bid claim.
  , newOptionsKeyAcceptedBidClaim :: Maybe NewOptionsKeyAcceptedBidClaim
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AftermarketBuyerModel

instance Default AftermarketBuyerModel where
  def = AftermarketBuyerModel
    { scene = OwnAftermarketBids
    , inspectedSale = Nothing
    , showSaleFilter = False
    , salesFilterModel = def
    , inspectedLoan = Nothing
    , inspectedBorrower = Nothing
    , nftType = LoanKey
    , selectedPolicyId = Nothing
    , newPolicyId = ""
    , choosingPolicyId = False
    , newLoanKeySpotPurchase = Nothing
    , newBidCreation = Nothing
    , inspectedBid = Nothing
    , showBidFilter = False
    , bidsFilterModel = def
    , newBidUpdate = Nothing
    , newLoanKeyBidClaim = Nothing
    , showBidClaimLenderAddressWidget = False
    , showSpotPurchaseLenderAddressWidget = False
    , inspectedBidTransaction = Nothing
    , showTransactionFilter = False
    , txFilterModel = def
    , txFilterScene = FilterScene
    , newOptionsKeySpotPurchase = Nothing
    , newOptionsKeyAcceptedBidClaim = Nothing
    }

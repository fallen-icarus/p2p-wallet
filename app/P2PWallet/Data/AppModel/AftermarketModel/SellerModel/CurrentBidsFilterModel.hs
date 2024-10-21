{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.AftermarketModel.SellerModel.CurrentBidsFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.Wallets.MarketWallet
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Current Bids Filter Model
-------------------------------------------------
-- | Possible sortings.
data CurrentBidsSortMethod
  -- | By utxo output reference.
  = CurrentBidsLexicographically
  -- | By the number nfts in the batch.
  | CurrentBidsNftCount
  -- | By the time the bid was last "touched".
  | CurrentBidsTime
  -- | By the time the bid will expired: bid expiration for claim bids and claim expiration for
  -- accepted bids.
  | CurrentBidsExpiration
  deriving (Show,Eq,Enum)

instance Display CurrentBidsSortMethod where
  display CurrentBidsLexicographically = "Lexicographically"
  display CurrentBidsNftCount = "Number of NFTs"
  display CurrentBidsTime = "Chronologically"
  display CurrentBidsExpiration = "Expiration"

data CurrentBidsFilterModel = CurrentBidsFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | Whether the bid should be an SpotBid, ClaimBid, or AcceptedBid.
  , bidType :: Maybe BidType
  -- | The nft type. Nothing for any type.
  , nftType :: Maybe KeyNftType
  -- | The bid must be for the specified policy id. This is only used in conjunction with OtherNft
  -- type.
  , policyId :: Text
  -- | The current sorting method for the bids.
  , sortingMethod :: CurrentBidsSortMethod
  -- | The current sorting direction for the bids.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''CurrentBidsFilterModel

instance Default CurrentBidsFilterModel where
  def = CurrentBidsFilterModel
    { scene = FilterScene
    , bidType = Nothing
    , nftType = Nothing
    , policyId = ""
    , sortingMethod = CurrentBidsTime
    , sortingDirection = SortDescending
    }

-- | Verify the information is valid.
checkCurrentBidsFilterModel :: CurrentBidsFilterModel -> Either Text ()
checkCurrentBidsFilterModel CurrentBidsFilterModel{..} = do
  unless (isNothing nftType) $ 
    unless (nftType /= Just OtherNft) $
      if policyId == "" 
      then Left "Policy id field left blank."
      else void $ maybeToRight ("Could not parse policy id: " <> policyId) $ parseHex policyId

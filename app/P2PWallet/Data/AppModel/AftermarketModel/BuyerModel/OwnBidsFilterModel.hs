{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.AftermarketModel.BuyerModel.OwnBidsFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.Wallets.MarketWallet
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Own Bids Filter Model
-------------------------------------------------
-- | Possible sortings.
data OwnBidsSortMethod
  -- | By utxo output reference.
  = OwnBidsLexicographically
  -- | By the number nfts in the batch.
  | OwnBidsNftCount
  -- | By the time the bid was last "touched".
  | OwnBidsTime
  -- | By the time the bid will expired: bid expiration for claim bids and claim expiration for
  -- accepted bids.
  | OwnBidsExpiration
  deriving (Show,Eq,Enum)

instance Display OwnBidsSortMethod where
  display OwnBidsLexicographically = "Lexicographically"
  display OwnBidsNftCount = "Number of NFTs"
  display OwnBidsTime = "Chronologically"
  display OwnBidsExpiration = "Expiration"

data OwnBidsFilterModel = OwnBidsFilterModel
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
  , sortingMethod :: OwnBidsSortMethod
  -- | The current sorting direction for the bids.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OwnBidsFilterModel

instance Default OwnBidsFilterModel where
  def = OwnBidsFilterModel
    { scene = FilterScene
    , bidType = Nothing
    , nftType = Nothing
    , policyId = ""
    , sortingMethod = OwnBidsTime
    , sortingDirection = SortDescending
    }

-- | Verify the information is valid.
checkOwnBidsFilterModel :: OwnBidsFilterModel -> Either Text ()
checkOwnBidsFilterModel OwnBidsFilterModel{..} = do
  unless (isNothing nftType) $ 
    unless (nftType /= Just OtherNft) $
      if policyId == "" 
      then Left "Policy id field left blank."
      else void $ maybeToRight ("Could not parse policy id: " <> policyId) $ parseHex policyId

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.AftermarketModel.SellerModel.OpenSalesFilterModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.Wallets
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Open Sales Filter Model
-------------------------------------------------
-- | Possible sortings.
data OpenSalesSortMethod
  -- | By utxo output reference.
  = OpenSalesLexicographically
  -- | By the number nfts in the batch.
  | OpenSalesNftCount
  -- | By the time the sale was last "touched".
  | OpenSalesTime
  deriving (Show,Eq,Enum)

instance Display OpenSalesSortMethod where
  display OpenSalesLexicographically = "Lexicographically"
  display OpenSalesNftCount = "Number of NFTs"
  display OpenSalesTime = "Chronologically"

data OpenSalesFilterModel = OpenSalesFilterModel
  -- | The filter scene.
  { scene :: FilterScene
  -- | The nft type. Nothing for any type.
  , nftType :: Maybe KeyNftType
  -- | The sale must be for the specified policy id. This is only used in conjunction with OtherNft
  -- type.
  , policyId :: Text
  -- | Whether the sale should be an auction. False for spot.
  , shouldBeAuction :: Maybe Bool
  -- | The current sorting method for the open Sales.
  , sortingMethod :: OpenSalesSortMethod
  -- | The current sorting direction for the open Sales.
  , sortingDirection :: SortDirection
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OpenSalesFilterModel

instance Default OpenSalesFilterModel where
  def = OpenSalesFilterModel
    { scene = FilterScene
    , nftType = Nothing
    , policyId = ""
    , shouldBeAuction = Nothing
    , sortingMethod = OpenSalesTime
    , sortingDirection = SortDescending
    }

-- | Verify the information is valid.
checkOpenSalesFilterModel :: OpenSalesFilterModel -> Either Text ()
checkOpenSalesFilterModel OpenSalesFilterModel{..} = do
  unless (isNothing nftType) $ 
    unless (nftType /= Just OtherNft) $
      if policyId == "" 
      then Left "Policy id field left blank."
      else void $ maybeToRight ("Could not parse policy id: " <> policyId) $ parseHex policyId

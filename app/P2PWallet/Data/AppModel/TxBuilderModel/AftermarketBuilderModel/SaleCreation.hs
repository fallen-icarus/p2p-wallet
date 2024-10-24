{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SaleCreation where

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Sale Creation
-------------------------------------------------
-- | Information for a new sale sale. This is used for both spot sales and auctions.
data SaleCreation = SaleCreation
  -- | The aftermarket wallet to use for the sale.
  { marketWallet :: MarketWallet
  -- | Whether the sale is a spot sale or auction.
  , isAuction :: Bool
  -- | The address the payment must go to.
  , paymentWallet :: PaymentWallet
  -- | The nfts for sale in this batch.
  , nfts :: [NativeAsset]
  -- | The required deposit for the sale.
  , deposit :: Lovelace
  -- | The sale price. This can include ada. If this is an auction, this is used as the starting
  -- price.
  , salePrice :: [NativeAsset]
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SaleCreation

-- This instance is used for the change calculation.
instance AssetBalancesForChange (a,SaleCreation) where
  assetBalancesForChange xs =
    ( sum $ map (negate . view #deposit . snd) xs
    , flip concatMap xs $ \(_,SaleCreation{nfts}) -> map (over #quantity negate) nfts
    )

-------------------------------------------------
-- New Sale Creation
-------------------------------------------------
-- | Information for a new sale sale.
data NewSaleCreation = NewSaleCreation
  -- | The aftermarket wallet to use for the sale.
  { marketWallet :: MarketWallet
  -- | Whether the sale is a spot sale or auction.
  , isAuction :: Bool
  -- | The address the payment must go to.
  , paymentWallet :: PaymentWallet
  -- | The nfts for sale in this batch.
  , nfts :: [NativeAsset]
  -- | The sale price. This can include ada. The assets are assumed to be separated by newlines.
  , salePrice :: Text
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | Show the NFTs list to enable removing NFTs.
  , showNfts :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewSaleCreation

instance Default NewSaleCreation where
  def = NewSaleCreation
    { marketWallet = def
    , isAuction = False
    , paymentWallet = def
    , nfts = []
    , salePrice = ""
    , network = def
    , showNfts = False
    }

-- | Create a fresh `NewSaleCreation`.
createNewSaleCreation 
  :: Network 
  -> MarketWallet 
  -> PaymentWallet -- ^ The wallet where the payment must go upon purchase.
  -> [NativeAsset] -- ^ The NFTs for sale.
  -> NewSaleCreation
createNewSaleCreation network marketWallet paymentWallet nfts =
  def & #network .~ network
      & #paymentWallet .~ paymentWallet
      & #marketWallet .~ marketWallet
      & #nfts .~ nfts

-------------------------------------------------
-- NewSaleCreation <--> SaleCreation
-------------------------------------------------
-- | Verify the user info for the new sale sale.
verifyNewSaleCreation 
  :: TickerMap 
  -> NewSaleCreation 
  -> Either Text SaleCreation
verifyNewSaleCreation tickerMap NewSaleCreation{..} = do
    -- Verify the sale price. No fingerprints allowed.
    verifiedPrice <- forM (lines salePrice) $ \line -> 
      first (const $ parseErrorMsg line) $ parseNativeAssets tickerMap mempty line

    return $ SaleCreation
      { network = network
      , isAuction = isAuction
      , paymentWallet = paymentWallet
      , marketWallet = marketWallet
      , nfts = nfts
      , salePrice = sumNativeAssets verifiedPrice -- combine native asset quantities
      , deposit = 
          -- This will be overridden later. It is needed to represent the proper amount of bytes 
          -- for the fee field since it will impact the final fee calculated. This is necessary
          -- since the deposit amount must also go in the sale datum.
          9_999_999 
      }

  where
    -- A custom error message is used since fingerprints are not allowed.
    parseErrorMsg :: Text -> Text
    parseErrorMsg x = unlines
      [ "Invalid native asset entry. Entries must be separated by newlines, and be one of:"
      , "'# policy_id.asset_name'"
      , "'# ticker'"
      , "'# ADA'"
      , ""
      , "Could not parse: '" <> x <> "'"
      , ""
      , "If using a ticker, make sure it is in the Ticker Registry."
      ]

toNewSaleCreation :: ReverseTickerMap -> SaleCreation -> NewSaleCreation
toNewSaleCreation reverseTickerMap SaleCreation{..} = NewSaleCreation
  { network = network
  , isAuction = isAuction
  , paymentWallet = paymentWallet
  , marketWallet = marketWallet
  , nfts = nfts
  , salePrice = 
      mconcat $ intersperse "\n" $ map (showAssetBalance True reverseTickerMap) salePrice
  , showNfts = False
  }

module P2PWallet.Actions.SyncOptions
  ( syncOptionsProposals
  , syncOptionsContract
  , syncActiveOptionsContracts
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel.OptionsModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Prelude

-- | This function gets all current proposed options contract for the specified assets.
syncOptionsProposals 
  :: Network 
  -> (OfferAsset, AskAsset, Maybe PremiumAsset)
  -> CachedOptionsProposals
  -> IO CachedOptionsProposals
syncOptionsProposals network key@(offerAsset, askAsset, mPremiumAsset) currentCache = do
    allProposals <- runQueryOptionsProposals network offerAsset askAsset mPremiumAsset >>= 
      -- Throw an error if syncing failed.
      fromRightOrAppError

    -- Update the cached.
    return $ currentCache
      & Map.insert key allProposals

syncOptionsContract 
  :: Network 
  -> ContractId 
  -> CachedKeyOptionsContracts 
  -> IO CachedKeyOptionsContracts
syncOptionsContract network contractId currentCache = do
  mUTxO <- runQuerySpecificOptionsContract network contractId >>= 
    -- Throw an error if syncing failed.
    fromRightOrAppError

  return $ currentCache
    & Map.insert contractId mUTxO

-- | This function gets all active options contracts for the specified assets.
syncActiveOptionsContracts
  :: Network 
  -> (OfferAsset, AskAsset)
  -> POSIXTime
  -> CachedActiveOptionsContracts
  -> IO CachedActiveOptionsContracts
syncActiveOptionsContracts network key@(offerAsset, askAsset) currentTime currentCache = do
    allActives <- runQueryActiveOptionsContracts network offerAsset askAsset currentTime >>= 
      -- Throw an error if syncing failed.
      fromRightOrAppError

    -- Update the cached.
    return $ currentCache
      & Map.insert key allActives

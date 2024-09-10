module P2PWallet.Actions.SyncOptions
  ( syncOptionsProposals
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel.OptionsModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.DeFi.CardanoOptions as Options
import P2PWallet.Prelude

-- | This function gets all current options contracts for the specified assets.
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

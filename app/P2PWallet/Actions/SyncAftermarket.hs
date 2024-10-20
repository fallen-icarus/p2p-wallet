module P2PWallet.Actions.SyncAftermarket
  ( syncAftermarketSales
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel.AftermarketModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Plutus
import P2PWallet.Prelude

-- | This function gets all current sales for the specified nft policy id.
syncAftermarketSales
  :: Network 
  -> CurrencySymbol
  -> CachedAftermarketSales
  -> IO CachedAftermarketSales
syncAftermarketSales network nftPolicyId currentCache = do
    allSales <- runQueryAftermarketSales network nftPolicyId >>= 
      -- Throw an error if syncing failed.
      fromRightOrAppError

    -- Update the cached.
    return $ currentCache
      & Map.insert nftPolicyId allSales

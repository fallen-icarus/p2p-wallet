module P2PWallet.Actions.SyncOrderBooks
  (
    syncOrderBooks
  ) where

import UnliftIO.Async (concurrently)
import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel.DexModel
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Prelude

-- | This function gets the current order book for a specific trading pair. It gets the swaps
-- for each direction, so that the results can be displayed as an order-book.
syncOrderBooks :: Network -> OfferAsset -> AskAsset -> CachedOrderBooks -> IO CachedOrderBooks
syncOrderBooks network (OfferAsset offerAsset) (AskAsset askAsset) currentCache = do
    (forwardOrderBook,reverseOrderBook) <- concurrently fetchForwardDirection fetchReverseDirection

    -- Update the cached order-books for each direction.
    return $ currentCache
      & Map.insert (OfferAsset askAsset, AskAsset offerAsset) reverseOrderBook
      & Map.insert (OfferAsset offerAsset, AskAsset askAsset) forwardOrderBook

  where
    fetchForwardDirection :: IO [SwapUTxO]
    fetchForwardDirection =
      runQuerySwaps network (OfferAsset offerAsset) (AskAsset askAsset) >>= 
        -- Throw an error if syncing failed.
        fromRightOrAppError

    fetchReverseDirection :: IO [SwapUTxO]
    fetchReverseDirection =
      runQuerySwaps network (OfferAsset askAsset) (AskAsset offerAsset) >>= 
        -- Throw an error if syncing failed.
        fromRightOrAppError

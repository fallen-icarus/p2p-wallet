module P2PWallet.Actions.SyncLoans
  (
    syncLoanAsks
  ) where

import Data.Map.Strict qualified as Map

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.AppModel.LendingModel
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Prelude

-- | This function gets all current loan asks that match the specified configuration.
syncLoanAsks :: Network -> LoanAskConfiguration -> CachedLoanAsks -> IO CachedLoanAsks
syncLoanAsks network askCfg currentCache = do
    allAsks <- runQueryLoanAsks network askCfg >>= 
      -- Throw an error if syncing failed.
      fromRightOrAppError

    -- Update the cached.
    return $ currentCache
      & Map.insert askCfg allAsks

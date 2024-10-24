module P2PWallet.Actions.LookupPools
  (
    lookupRegisteredPools
  ) where

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.Koios.Pool
import P2PWallet.Prelude

lookupRegisteredPools :: Network -> IO [Pool]
lookupRegisteredPools network = do
  runQueryAllRegisteredPools network >>= fromRightOrAppError

module P2PWallet.Data.Core.Internal.AppError where

import P2PWallet.Prelude

-------------------------------------------------
-- App Errors
-------------------------------------------------
-- | The type for an app error that can be caught in `IO`.
newtype AppError = AppError Text deriving (Show)

instance Exception AppError

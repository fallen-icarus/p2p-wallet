{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Database.Pattern
  ( Pattern(..)
  , patternToSql
  ) where

import Database.SQLite.Simple qualified as Sqlite

import P2PWallet.Data.Core.Network
import P2PWallet.Data.Core.PaymentId
import P2PWallet.Data.Core.ProfileId
import P2PWallet.Prelude

data Pattern
  = MatchNetwork Network
  | MatchProfileId ProfileId
  | MatchPaymentId PaymentId

patternToSql :: [Pattern] -> Sqlite.Query
patternToSql [] = ""
patternToSql ps =
    Sqlite.Query $ toText $ " WHERE " <> intercalate " AND " (map patternToSql' ps)
  where
    -- Text in the database is wrapped in double quotes. Using `show` on a `String` will wrap it in
    -- double quotes.
    patternToSql' :: Pattern -> String
    patternToSql' = \case
      MatchNetwork network -> "network = " <> show (toString network)
      MatchProfileId (ProfileId profileId) -> "profile_id = " <> show profileId
      MatchPaymentId (PaymentId paymentId) -> "payment_id = " <> show paymentId

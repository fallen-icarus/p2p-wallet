{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.Internal.RegistrationStatus where

import Data.Aeson

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple (SQLData(SQLText))

import P2PWallet.Prelude

-- | Whether it is registered or not.
data RegistrationStatus
  = Registered
  | NotRegistered
  deriving (Show,Eq)

instance ToJSON RegistrationStatus where
  toJSON = toJSON . showRegistrationStatus

instance FromJSON RegistrationStatus where
  parseJSON = withText "RegistrationStatus" (maybe mzero return . parseRegistrationStatus)

instance ToField RegistrationStatus where
  toField = toField . showRegistrationStatus

instance FromField RegistrationStatus where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "not a valid registration status") Ok  $ 
      parseRegistrationStatus t
  fromField f = returnError ConversionFailed f "need a text"

parseRegistrationStatus :: Text -> Maybe RegistrationStatus
parseRegistrationStatus "not registered" = Just NotRegistered
parseRegistrationStatus "registered" = Just Registered
parseRegistrationStatus _ = Nothing

-- Koios uses lower case letters.
showRegistrationStatus :: RegistrationStatus -> Text
showRegistrationStatus NotRegistered = "not registered"
showRegistrationStatus Registered = "registered"

instance Display RegistrationStatus where
  display NotRegistered = "Not Registered"
  display Registered = "Registered"

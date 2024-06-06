{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.RegistrationStatus where

import Data.Aeson

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple (SQLData(SQLText))

import P2PWallet.Prelude

data RegistrationStatus
  = Registered
  | NotRegistered
  deriving (Show,Eq)

instance ToJSON RegistrationStatus where
  toJSON = toJSON . showRegistrationStatus

instance FromJSON RegistrationStatus where
  parseJSON = withText "RegistrationStatus" (maybe mzero return . readRegistrationStatus)

instance ToField RegistrationStatus where
  toField = toField . showRegistrationStatus

instance FromField RegistrationStatus where
  fromField f@(Field (SQLText t) _) = 
    maybe (returnError ConversionFailed f "not a valid registration status") Ok  $ 
      readRegistrationStatus t
  fromField f = returnError ConversionFailed f "need a text"

readRegistrationStatus :: Text -> Maybe RegistrationStatus
readRegistrationStatus "not registered" = Just NotRegistered
readRegistrationStatus "registered" = Just Registered
readRegistrationStatus _ = Nothing

showRegistrationStatus :: RegistrationStatus -> Text
showRegistrationStatus NotRegistered = "not registered"
showRegistrationStatus Registered = "registered"

displayRegistrationStatus :: RegistrationStatus -> Text
displayRegistrationStatus NotRegistered = "Not Registered"
displayRegistrationStatus Registered = "Registered"

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Core.RegistrationStatus where

import Data.Aeson

import P2PWallet.Prelude

data RegistrationStatus
  = Registered
  | NotRegistered
  deriving (Show,Eq)

instance ToJSON RegistrationStatus where
  toJSON = toJSON . showRegistrationStatus

instance FromJSON RegistrationStatus where
  parseJSON = withText "RegistrationStatus" (maybe mzero return . readRegistrationStatus)

readRegistrationStatus :: Text -> Maybe RegistrationStatus
readRegistrationStatus "not registered" = Just NotRegistered
readRegistrationStatus "registered" = Just Registered
readRegistrationStatus _ = Nothing

showRegistrationStatus :: RegistrationStatus -> Text
showRegistrationStatus NotRegistered = "not registered"
showRegistrationStatus Registered = "registered"

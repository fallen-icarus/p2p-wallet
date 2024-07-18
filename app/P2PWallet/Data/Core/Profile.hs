{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

A `Profile` is related to the derivation path for hardware keys in that it is the account index 
combined with an alias. The derivation path is outlined as:

@
    m / purpose' / coin_type' / account' / chain / address_index
@

When signing transactions with hardware wallets, all keys must be for the same account index and the
same device. Therefore, the profile name can be thought of as the key family name. Every `Profile`
will get their own backup file and wallets. The app will only have one profile loaded at a given
time but users can easily switch between profiles from within the app.

There can be multiple profiles dedicated to a given account index. 

-}
module P2PWallet.Data.Core.Profile where

import Database.SQLite.Simple (ToRow(..),FromRow(..))

import P2PWallet.Data.Core.Internal
import P2PWallet.Database
import P2PWallet.Prelude

-------------------------------------------------
-- Profiles
-------------------------------------------------
-- | A type represnting a user profile.
data Profile = Profile
  { network :: Network -- ^ The network this profile is dedicated to.
  , profileId :: ProfileId -- ^ The unique id for this profile.
  , accountIndex :: AccountIndex -- ^ The key family associated with this profile.
  , alias :: Text -- ^ The user friendly name for this profile.
  , device :: HardwareDevice -- ^ Which hardware wallet derivation the profile is dedicated to.
  , derivationType :: Maybe DerivationType -- ^ The derivation type to use if Trezor.
  } deriving (Show,Eq,Ord,Generic,ToRow,FromRow)

makeFieldLabelsNoPrefix ''Profile

instance Default Profile where
  def = Profile
    { network = def
    , profileId = 0
    , accountIndex = 0
    , alias = ""
    , device = Ledger
    , derivationType = Nothing
    }

instance TableName Profile where
  tableName = "profiles"

instance Creatable Profile where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @Profile
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER PRIMARY KEY"
        , "account_index INTEGER NOT NULL"
        , "alias TEXT NOT NULL"
        , "device TEXT NOT NULL"
        , "derivationt_type TEXT"
        , "UNIQUE(network,profile_id,alias)"
        ]
    , ");"
    ]

instance Insertable Profile where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @Profile
    , "("
    , unwords $ intersperse ","
        [ "network"
        , "profile_id"
        , "account_index"
        , "alias"
        , "device"
        , "derivationt_type"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?);"
    ]

-------------------------------------------------
-- NewProfile
-------------------------------------------------
-- | The type representing information the user must supply in order to add a new `Profile`. 
data NewProfile = NewProfile
  -- | A user-friendly name for the `Profile`.
  { alias :: Text 
  -- | The account index to be used for all hardware keys under this profile.
  , accountIndex :: Int 
  -- | Which hardware wallet device this profile is for.
  , device :: HardwareDevice
  -- | Which derivation type to use if the device is Trezor.
  , derivationType :: Maybe DerivationType
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewProfile

instance Default NewProfile where
  def = NewProfile 
    { alias = ""
    , accountIndex = 0
    , device = Ledger
    , derivationType = Nothing
    }

-- | Process the user's info for the profile. Check that the profile name is not already being
-- used. `AccountIndex` must be >= 0.
verifyNewProfile :: Network -> ProfileId -> NewProfile -> [Profile] -> Either Text Profile
verifyNewProfile network newProfileId NewProfile{..} otherProfiles
  | alias == "" = Left "Profile name is empty."
  | any (\p -> p ^. #alias == alias) otherProfiles = Left "Another profile exists with that name."
  | accountIndex < 0 = Left "Account indices must be >= 0."
  | otherwise = Right $ Profile
      { network = network
      , profileId = newProfileId
      , alias = alias
      , accountIndex = AccountIndex accountIndex
      , device = device
      , derivationType = if device == Ledger then Nothing else derivationType
      }

toNewProfile :: Profile -> NewProfile
toNewProfile Profile{..} = NewProfile
  { alias = alias
  , accountIndex = unAccountIndex accountIndex
  , device = device
  , derivationType = derivationType
  }

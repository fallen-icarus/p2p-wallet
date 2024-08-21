{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.ProfileModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.AddressBook
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Profile Events
-------------------------------------------------
data ProfileEvent
  -- | Load the profiles for that network, and then prompt the user to pick one.
  = LoadKnownProfiles (ProcessEvent () [Profile])
  -- | Load selected profile.
  | LoadSelectedProfile Profile
  -- | Load known information for this profile.
  | LoadProfileInfo (ProcessEvent () (Wallets, [AddressEntry], [TickerInfo]))
  -- | Log out of current profile.
  | LogoutProfile
  -- | Add a new profile.
  | AddNewProfile (AddEvent Profile Profile)
  -- | Change a profile name.
  | ChangeProfileName (AddEvent Profile Profile)
  -- | Delete a profile.
  | DeleteProfile (DeleteWithConfirmationEvent Profile)

-------------------------------------------------
-- Profile State
-------------------------------------------------
data ProfileModel = ProfileModel
  -- | A list of all known profiles for this network.
  { knownProfiles :: [Profile]
  -- | The new `Profile` information.
  , newProfile :: NewProfile
  -- | Whether the new profile widget should be open.
  , addingProfile :: Bool
  -- | Whether the delete profile widget should be open.
  , deletingProfile :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ProfileModel

instance Default ProfileModel where
  def = ProfileModel
    { knownProfiles = []
    , newProfile = def
    , addingProfile = False
    , deletingProfile = False
    }

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel
  ( -- * Monomer Synonyms
    AppWenv
  , AppNode

    -- * App Errors
  , AppError(..)

    -- * Main App Types
  , MainScene(..)
  , AppEvent(..)
  , ProfileEvent(..)
  , AppModel(..)

    -- * Re-exports
  , module P2PWallet.Data.AppModel.Common
  , module P2PWallet.Data.AppModel.Home
  ) where

import Monomer qualified as Monomer

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.Home
import P2PWallet.Data.Core
import P2PWallet.Data.Profile
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Main Scenes
-------------------------------------------------
-- | The current main scene of the app.
data MainScene
  = NetworksScene
  | ProfilesScene
  | HomeScene
  | SettingsScene
  deriving (Show,Eq)

-------------------------------------------------
-- App Events
-------------------------------------------------
-- | The main UI events for the app.
data AppEvent
  -- | Initialize the app. This is also useful as a "Do Nothing" event for Monomer.
  = AppInit 
  -- | Copy the text to the user's clipboard.
  | CopyText Text
  -- | Open the alert widget and show the user the specified `Text`.
  | Alert Text 
  -- | Close alert widget and reset the alert message.
  | CloseAlertMessage 
  -- | Change the scene to the specified `MainScene`.
  | ChangeMainScene MainScene 
  -- | Set the config to the specified network and load the known profiles for that network.
  | SetNetwork Network
  -- | An event updating the current profile.
  | ProfileEvent ProfileEvent
  -- | An event for the Home page.
  | HomeEvent HomeEvent 
  -- | Sync the currently tracked wallets.
  | SyncWallets (SyncEvent Wallets)


data ProfileEvent
  -- | Load the profiles for that network, and then prompt the user to pick one.
  = LoadKnownProfiles [Profile]
  -- | Load selected profile.
  | LoadSelectedProfile Profile
  -- | Load known wallets for this profile.
  | LoadKnownWallets Wallets
  -- | Log out of current profile.
  | LogoutProfile
  -- | Add a new profile.
  | AddNewProfile (AddEvent Profile)
  -- | Change a profile name.
  | ChangeProfileName (AddEvent Text)
  -- | Delete a profile.
  | DeleteProfile (DeleteWithConfirmationEvent Profile)

-------------------------------------------------
-- Main App State
-------------------------------------------------
-- | The main state for the app.
data AppModel = AppModel
  -- | The absolute path to the sqlite database.
  { databaseFile :: FilePath
  -- | The configuration for the app.
  , config :: Config
  -- | Which main scene is currently active.
  , scene :: MainScene
  -- | Used for both error messages and notices.
  , alertMessage :: Maybe Text
  -- | A list of all known profiles for this network.
  , knownProfiles :: [Profile]
  -- | The currently loaded profile. `Nothing` if one has not been loaded yet.
  , selectedProfile :: Maybe Profile
  -- | The new `Profile` information.
  , newProfile :: NewProfile
  -- | Whether the new profile widget should be open.
  , addingProfile :: Bool
  -- | Whether the delete profile widget should be open.
  , deletingProfile :: Bool
  -- | The model for the home scene.
  , homeModel :: HomeModel
  -- | The known wallets for the selected profile.
  , knownWallets :: Wallets
  -- | The app is waiting for the hardware wallet.
  , waitingOnDevice :: Bool
  -- | The app is syncing the wallets.
  , syncingWallets :: Bool
  -- | Useful when the user must specify a one-time use input such as a filepath or new alias.
  , extraTextField :: Text
  -- | This is useful for forcing the redraw of the UI when a text field is changed.
  , forceRedraw :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AppModel

instance Default AppModel where
  def = AppModel
    { databaseFile = ""
    , config = def
    , scene = NetworksScene
    , alertMessage = Nothing
    , knownProfiles = []
    , selectedProfile = Nothing
    , newProfile = def
    , addingProfile = False
    , deletingProfile = False
    , homeModel = def
    , knownWallets = def
    , waitingOnDevice = False
    , syncingWallets = False
    , extraTextField = ""
    , forceRedraw = False
    }

-------------------------------------------------
-- Monomer synonyms
-------------------------------------------------
type AppWenv = Monomer.WidgetEnv AppModel AppEvent
type AppNode = Monomer.WidgetNode AppModel AppEvent

-------------------------------------------------
-- App Errors
-------------------------------------------------
-- | The type for an app error that can be caught in `IO`.
newtype AppError = AppError Text deriving (Show)

instance Exception AppError

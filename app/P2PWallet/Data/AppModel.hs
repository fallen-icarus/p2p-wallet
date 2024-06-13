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
  , module P2PWallet.Data.AppModel.AddressBook
  , module P2PWallet.Data.AppModel.Common
  , module P2PWallet.Data.AppModel.Delegation
  , module P2PWallet.Data.AppModel.Home
  , module P2PWallet.Data.AppModel.TickerRegistry
  , module P2PWallet.Data.AppModel.TxBuilder
  ) where

import Monomer qualified as Monomer

import P2PWallet.Data.AppModel.AddressBook
import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.Delegation
import P2PWallet.Data.AppModel.Home
import P2PWallet.Data.AppModel.TickerRegistry
import P2PWallet.Data.AppModel.TxBuilder
import P2PWallet.Data.AddressBook
import P2PWallet.Data.Core
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Profile
import P2PWallet.Data.TickerMap
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
  | DelegationScene
  | TxBuilderScene
  | AddressBookScene
  | TickerRegistryScene
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
  -- | An event for the Delegation page.
  | DelegationEvent DelegationEvent
  -- | An event for the AddressBook page.
  | AddressBookEvent AddressBookEvent 
  -- | An event for the Token Registry page.
  | TickerRegistryEvent TickerRegistryEvent 
  -- | An event for the Tx Builder page.
  | TxBuilderEvent TxBuilderEvent 
  -- | Sync the currently tracked wallets.
  | SyncWallets (SyncEvent Wallets)
  -- | Sync all registered pools.
  | SyncRegisteredPools (SyncEvent [Pool])
  -- | Update the current date.
  | UpdateCurrentDate Day


data ProfileEvent
  -- | Load the profiles for that network, and then prompt the user to pick one.
  = LoadKnownProfiles [Profile]
  -- | Load selected profile.
  | LoadSelectedProfile Profile
  -- | Load known information for this profile.
  | LoadProfileInfo (Wallets, [AddressEntry], [TickerInfo])
  -- | Log out of current profile.
  | LogoutProfile
  -- | Add a new profile.
  | AddNewProfile (AddEvent Profile Profile)
  -- | Change a profile name.
  | ChangeProfileName (AddEvent Text Text)
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
  -- | The model for the delegation scene.
  , delegationModel :: DelegationModel
  -- | The model for the tx builder scene.
  , txBuilderModel :: TxBuilderModel
  -- | The known wallets for the selected profile.
  , knownWallets :: Wallets
  -- | The app is waiting for the hardware wallet.
  , waitingOnDevice :: Bool
  -- | The app is syncing the wallets.
  , syncingWallets :: Bool
  -- | The app is syncing the pools.
  , syncingPools :: Bool
  -- | The app is loading the profile.
  , loadingProfile :: Bool
  -- | The address book for this profile.
  , addressBook :: [AddressEntry]
  -- | The address book model
  , addressBookModel :: AddressBookModel
  -- | Useful when the user must specify a one-time use input such as a filepath or new alias.
  , extraTextField :: Text
  -- | This is useful for forcing the redraw of the UI when a text field is changed.
  , forceRedraw :: Bool
  -- | A mapping from tickers to their on-chain name `policy_id.token_name` and their decimal
  -- places.
  , tickerMap :: TickerMap
  -- | A mapping from on-chain names (`policy_id.token_name`) to tickers and their decimal
  -- places.
  , reverseTickerMap :: ReverseTickerMap
  -- | The ticker registry model
  , tickerRegistryModel :: TickerRegistryModel
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
    , delegationModel = def
    , addressBookModel = def
    , txBuilderModel = def
    , knownWallets = def
    , waitingOnDevice = False
    , syncingWallets = False
    , syncingPools = False
    , loadingProfile = False
    , extraTextField = ""
    , forceRedraw = False
    , addressBook = []
    , tickerMap = mempty
    , reverseTickerMap = mempty
    , tickerRegistryModel = def
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

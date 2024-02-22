{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module P2PWallet.Data.App
  ( -- * Monomer Synonyms
    AppWenv
  , AppNode

    -- * App Errors
  , AppError(..)

    -- * Main App Types
  , MainScene(..)
  , SyncEvent(..)
  , ChangeProfileEvent(..)
  , AppEvent(..)
  , AppModel(..)
  , NewProfile(..)

    -- * Re-exports
  , module P2PWallet.Data.App.Common
  , module P2PWallet.Data.App.Config
  , module P2PWallet.Data.App.Delegation
  , module P2PWallet.Data.App.Home
  , module P2PWallet.Data.App.TxBuilder
  ) where

import Monomer qualified as Monomer

import P2PWallet.Data.App.Common
import P2PWallet.Data.App.Config
import P2PWallet.Data.App.Delegation
import P2PWallet.Data.App.Home
import P2PWallet.Data.App.TxBuilder
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Files
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

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

-------------------------------------------------
-- Main Scenes
-------------------------------------------------
-- | The current main scene of the app.
data MainScene
  = ProfilePickerScene -- ^ Choose between profiles.
  | NewProfileScene -- ^ Add a new profile.
  | HomeScene -- ^ Payment wallets.
  | DelegationScene -- ^ Stake wallets.
  | LimitOrders -- ^ One-Way swaps.
  | MarketMakers -- ^ Two-Way swaps.
  | TxBuilderScene
  | SettingsScene
  deriving (Show,Eq)

-------------------------------------------------
-- Add a new `Profile`
-------------------------------------------------
-- | The type representing information the user must supply in order to add a new `Profile`.
data NewProfile = NewProfile
  -- | A user-friendly name for the `Profile`.
  { _alias :: Text 
  -- | The account index to be used for all hardware keys under this profile.
  , _accountIndex :: Int 
  -- | Which hardware wallet device this profile is for.
  , _device :: HwDevice
  } deriving (Show,Eq)

instance Default NewProfile where
  def = NewProfile 
    { _alias = ""
    , _accountIndex = 0
    , _device = Ledger
    }

-------------------------------------------------
-- Main App Events
-------------------------------------------------
-- | The UI steps for syncing. These steps are automated.
data SyncEvent a
  = StartSync
  | SyncResults a
  deriving (Show,Eq)

-- | The UI steps for switching pofiles. These steps are automated.
data ChangeProfileEvent
  = LogoutCurrentProfile
  | LoadNewProfile Profile
  | LoadWalletsResult Wallets
  deriving (Show,Eq)

-- | The main UI events for the app.
data AppEvent
  -- | Initialize the app. This is also useful as a "Do Nothing" event for Monomer.
  = AppInit 
  -- | Open the alert widget and show the user the specified `Text`.
  | Alert Text 
  -- | Close alert widget and reset the alert message.
  | CloseAlertMessage 
  -- | Change the scene to the specified `MainScene`.
  | ChangeMainScene MainScene 
  -- | An event for the Home page.
  | HomeEvent HomeEvent 
  -- | An event for the Delegation page.
  | DelegationEvent DelegationEvent 
  -- | An event for the TxBuilder page.
  | TxBuilderEvent TxBuilderEvent
  -- -- | Sync the currently tracked wallets.
  | SyncWallets (SyncEvent Wallets)
  -- | Sync all registered pools.
  | SyncRegisteredPools (SyncEvent [Pool])
  -- | Sign a transaction that was built using the TxBuilder.
  | SignTx 
  -- | Submit a signed transaction. The transaction can be loaded from an external file.
  | SubmitTx SignedTxFile 
  -- | Change the current profile.
  | ChangeProfile ChangeProfileEvent
  -- | Add a new profile.
  | AddNewProfile (AddEvent Profile)

-------------------------------------------------
-- Main App State
-------------------------------------------------
-- | The main state for the app.
data AppModel = AppModel
  { {- Config -}
    _config :: Config -- ^ The configuration for the app.

  
    {- Profiles -}
  , _knownProfiles :: [Profile] -- ^ A list of all known profiles.
  , _newProfile :: NewProfile -- ^ The new `Profile` information.
  , _addingProfile :: Bool -- ^ Whether the new profile widget should be open.
  , _selectedProfile :: Maybe Profile -- ^ The currently loaded profile. `Nothing` if one has not
                                      -- been loaded yet.

    {- Wallets -}
  , _wallets :: Wallets -- ^ The currently tracked wallets.

    {- Scenes -}
  , _scene :: MainScene -- ^ Which main scene is currently active.

    {- Overlays for telling the user to wait for something -}
  , _waitingOnDevice :: Bool -- ^ The app is waiting for the hardware wallet.
  , _syncingWallets :: Bool -- ^ The app is syncing wallets.
  , _syncingPools :: Bool -- ^ The app is syncing registered pools.
  , _building :: Bool -- ^ The app is building the specified transaction.
  , _submitting :: Bool -- ^ The app is submitting the transaction.

    {- Models for the main scenes -}
  , _homeModel :: HomeModel
  , _delegationModel :: DelegationModel
  , _txBuilderModel :: TxBuilderModel

    {- Misc -}
  , _extraTextField :: Text -- ^ Useful when the user must specify a one-time use input such
                            -- as a filepath for saving/loading.

    {- Alerts -}
  , _alertMessage :: Maybe Text -- ^ Used for both error messages and notices.
  } deriving (Show,Eq)

instance Default AppModel where
  def = AppModel
    { _config = def
    , _knownProfiles = []
    , _newProfile = def
    , _addingProfile = False
    , _selectedProfile = Nothing -- Always start the app with the profile picker.
    , _wallets = def
    , _scene = ProfilePickerScene
    , _waitingOnDevice = False 
    , _syncingWallets = False 
    , _syncingPools = False 
    , _building = False
    , _submitting = False
    , _homeModel = def 
    , _delegationModel = def 
    , _txBuilderModel = def 
    , _extraTextField = ""
    , _alertMessage = Nothing
    }

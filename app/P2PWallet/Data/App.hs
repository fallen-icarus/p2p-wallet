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
  , AppEvent(..)
  , AppModel(..)

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
  = HomeScene -- ^ Payment wallets.
  | DelegationScene -- ^ Stake wallets.
  | LimitOrders -- ^ One-Way swaps.
  | MarketMakers -- ^ Two-Way swaps.
  | TxBuilderScene
  | SettingsScene
  deriving (Show,Eq)

-------------------------------------------------
-- Main App Events
-------------------------------------------------
-- | The UI steps for syncing. These steps are automated.
data SyncEvent a
  = StartSync
  | SyncResults a
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
  -- | Sync the currently tracked wallets.
  | SyncWallets (SyncEvent Wallets)
  -- | Sync all registered pools.
  | SyncRegisteredPools (SyncEvent [Pool])
  -- | Sign a transaction that was built using the TxBuilder.
  | SignTx 
  -- | Submit a signed transaction. The transaction can be loaded from an external file.
  | SubmitTx SignedTxFile 

-------------------------------------------------
-- Main App State
-------------------------------------------------
-- | The main state for the app.
data AppModel = AppModel
  { {- Config -}
    _config :: Config -- ^ The configuration for the app.

    {- Wallets -}
  , _wallets :: Wallets -- ^ The currently tracked wallets.

    {- Pools -}
  , _registeredPools :: [Pool] -- ^ A list of all known registered pools.

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
    , _wallets = def
    , _registeredPools = []
    , _scene = HomeScene 
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

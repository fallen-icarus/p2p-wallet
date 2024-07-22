{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel
  ( -- * Monomer Synonyms
    AppWenv
  , AppNode

    -- * Main App Types
  , MainScene(..)
  , AppEvent(..)
  , AppModel(..)

    -- * Helper Functions
  , configureSelectedDeFiWallets
  , swapBuilderEvent

    -- * Re-exports
  , module P2PWallet.Data.AppModel.AddressBookModel
  , module P2PWallet.Data.AppModel.Common
  , module P2PWallet.Data.AppModel.DelegationModel
  , module P2PWallet.Data.AppModel.DexModel
  , module P2PWallet.Data.AppModel.HomeModel
  , module P2PWallet.Data.AppModel.ProfileModel
  , module P2PWallet.Data.AppModel.TickerRegistryModel
  , module P2PWallet.Data.AppModel.TxBuilderModel
  ) where

import Monomer qualified

import P2PWallet.Data.AppModel.AddressBookModel
import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.AppModel.DelegationModel
import P2PWallet.Data.AppModel.DexModel
import P2PWallet.Data.AppModel.HomeModel
import P2PWallet.Data.AppModel.ProfileModel
import P2PWallet.Data.AppModel.TickerRegistryModel
import P2PWallet.Data.AppModel.TxBuilderModel
import P2PWallet.Data.Core.AddressBook
import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal.Config
import P2PWallet.Data.Core.Internal.Files
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.Core.Internal.Notification
import P2PWallet.Data.Core.Profile
import P2PWallet.Data.Core.Wallets
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
  | DexScene
  | NotificationsScene
  deriving (Show,Eq)

-------------------------------------------------
-- Waiting Status
-------------------------------------------------
-- | Whether the app should block user actions and why. This is useful when the app is waiting for
-- something external.
data WaitingStatus = WaitingStatus
  -- | The app is waiting for the hardware wallet.
  { waitingOnDevice :: Bool
  -- | The app is syncing the wallets.
  , syncingWallets :: Bool
  -- | The app is syncing the pools.
  , syncingPools :: Bool
  -- | The app is syncing the order-book for the selected trading pair.
  , syncingOrderBook :: Bool
  -- | The app is building the transaction.
  , building :: Bool
  -- | The app is loading the profile.
  , loadingProfile :: Bool
  -- | The app is submitting a transaction.
  , submitting :: Bool
  -- | The app is validating the user supplied information for a new builder action. This is only 
  -- needed if there is a noticable waiting period.
  , addingToBuilder :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''WaitingStatus

instance Default WaitingStatus where
  def = WaitingStatus
    { waitingOnDevice = False
    , syncingWallets = False
    , syncingPools = False
    , syncingOrderBook = False
    , building = False
    , loadingProfile = False
    , submitting = False
    , addingToBuilder = False
    }

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
  -- | Sync the currently tracked wallets. This can be called from most scenes which is why it
  -- is a main event. This also gets the current network parameters so they are available for
  -- building transactions.
  | SyncWallets (ProcessEvent (Wallets, (ByteString,Decimal) ,[Notification]))
  -- | Update the current date.
  | UpdateCurrentDate (ProcessEvent Day)
  -- | An event for the Home page.
  | HomeEvent HomeEvent 
  -- | An event for the Delegation page.
  | DelegationEvent DelegationEvent
  -- | An event for the AddressBook page.
  | AddressBookEvent AddressBookEvent 
  -- | An event for the Token Registry page.
  | TickerRegistryEvent TickerRegistryEvent 
  -- | An event for the Dex page.
  | DexEvent DexEvent 
  -- | An event for the Tx Builder page.
  | TxBuilderEvent TxBuilderEvent 
  -- | Submit a signed transaction.
  | SubmitTx SignedTxFile 
  -- | Toggle notification status.
  | ToggleNotificationReadStatus Int
  -- | Mark all notifications as read.
  | MarkAllNotificationsAsRead

-------------------------------------------------
-- App State
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
  -- | The profile model.
  , profileModel :: ProfileModel
  -- | The currently loaded profile. `Nothing` if one has not been loaded yet.
  , selectedProfile :: Maybe Profile
  -- | The known wallets for the selected profile.
  , knownWallets :: Wallets
  -- | Notifications from the latest sync.
  , notifications :: [(Int,Notification)]
  -- | The waiting status for the app.
  , waitingStatus :: WaitingStatus
  -- | The home model.
  , homeModel :: HomeModel
  -- | The delegation model.
  , delegationModel :: DelegationModel
  -- | The address book model.
  , addressBookModel :: AddressBookModel
  -- | The ticker registry model.
  , tickerRegistryModel :: TickerRegistryModel
  -- | The dex model.
  , dexModel :: DexModel
  -- | The model for the tx builder scene.
  , txBuilderModel :: TxBuilderModel
  -- | The address book for this profile.
  , addressBook :: [AddressEntry]
  -- | A mapping from tickers to their on-chain name `policy_id.token_name` and their decimal
  -- places.
  , tickerMap :: TickerMap
  -- | A mapping from on-chain names (`policy_id.token_name`) to tickers and their decimal
  -- places.
  , reverseTickerMap :: ReverseTickerMap
  -- | A mapping from fingerprints to their associated on-chain name.
  , fingerprintMap :: FingerprintMap
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
    , profileModel = def
    , selectedProfile = Nothing
    , knownWallets = def
    , notifications = []
    , waitingStatus = def
    , homeModel = def
    , delegationModel = def
    , addressBookModel = def
    , tickerRegistryModel = def
    , dexModel = def
    , txBuilderModel = def
    , addressBook = []
    , tickerMap = mempty
    , reverseTickerMap = mempty
    , fingerprintMap = mempty
    , forceRedraw = False
    }

-------------------------------------------------
-- Monomer synonyms
-------------------------------------------------
type AppWenv = Monomer.WidgetEnv AppModel AppEvent
type AppNode = Monomer.WidgetNode AppModel AppEvent

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Configure all selected wallets to use the first one in each wallet list. This does not set the
-- payment wallet for the home model.
configureSelectedDeFiWallets :: AppModel -> AppModel
configureSelectedDeFiWallets model@AppModel{..} = 
  model 
    & #delegationModel % #selectedWallet .~ fromMaybe def (maybeHead $ knownWallets ^. #stakeWallets)
    & #dexModel % #selectedWallet .~ fromMaybe def (maybeHead $ knownWallets ^. #dexWallets)

-- | This is a useful alias for conciseness.
swapBuilderEvent :: SwapBuilderEvent -> AppEvent
swapBuilderEvent = TxBuilderEvent . SwapBuilderEvent

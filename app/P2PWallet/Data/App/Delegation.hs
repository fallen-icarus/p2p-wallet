{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{-

The Delegation scene is dedicated to `StakeWallet`s.

-}
module P2PWallet.Data.App.Delegation where

import Data.Aeson (eitherDecodeStrictText)
import Data.Aeson.Encode.Pretty (encodePretty)

import P2PWallet.Data.App.Common
import P2PWallet.Data.Core.PoolID
import P2PWallet.Data.FilterLang
import P2PWallet.Data.Koios.Pool
import P2PWallet.Data.Wallets.StakeWallet
import P2PWallet.Prelude

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Delegation page.
data DelegationScene
  -- | Information about the stake wallet as well as access to adding new stake wallets.
  = DelegationSummary
  -- | Reward history for the stake address.
  | DelegationHistory
  -- | Registered stake pools available to delegate to.
  | DelegationPools
  deriving (Eq,Show)

-- | The detail overlays that can be shown on the delegation page. The overlay will cover the 
-- entire page and prevent interacting with the it until the overlay is closed.
data DelegationDetails
  -- | View more detailed information about a particular stake pool.
  = DelegationPool Pool
  deriving (Show,Eq)

-------------------------------------------------
-- Add a new `StakeWallet`
-------------------------------------------------
-- | The type representing information the user must supply in order to track a new `StakeWallet`.
data NewStakeWallet = NewStakeWallet
  -- | A user-friendly name for the address. This is used regardless of pairing/watching.
  { _alias :: Text 
  -- | What derivation path to use for the stake key. This is only used when pairing a stake
  -- wallet.
  , _stakeKeyPath :: Text 
  -- | The new stake address to watch. This is only used when adding a watched stake wallet.
  , _stakeAddress :: Text 
  } deriving (Show,Eq)

instance Default NewStakeWallet where
  def = NewStakeWallet
    { _alias = ""
    , _stakeKeyPath = "1852H/1815H/0H/2/0" 
    , _stakeAddress = ""
    }

-------------------------------------------------
-- Filters
-------------------------------------------------
-- | The type for a user supplied registered pool filter.
newtype UserPoolFilters = UserPoolFilters { _rawFilters :: Text }
  deriving (Show,Eq)

instance Default UserPoolFilters where
  def = UserPoolFilters $ decodeUtf8 $ encodePretty 
    [ MatchAll
        [ MatchPredicate (MatchLiveSaturation $ IsLT 90)
        , MatchAny 
            [ MatchPredicate (MatchMargin $ IsLT 5.0)
            , MatchPredicate (MatchFixedCost $ IsLT 340)
            ]
        ]
    ]

unUserPoolFilters :: UserPoolFilters -> Text
unUserPoolFilters (UserPoolFilters fs) = fs

newtype VerifiedPoolFilters = VerifiedPoolFilters [FilterLang PoolFilterLang]
  deriving (Show,Eq)

instance Default VerifiedPoolFilters where
  def = VerifiedPoolFilters []

unVerifiedPoolFilters :: VerifiedPoolFilters -> [FilterLang PoolFilterLang]
unVerifiedPoolFilters (VerifiedPoolFilters fs) = fs

toVerifiedPoolFilters :: UserPoolFilters -> Either Text VerifiedPoolFilters
toVerifiedPoolFilters (UserPoolFilters fs) =
  fmap VerifiedPoolFilters $ first toText $ eitherDecodeStrictText fs

fromVerifiedPoolFilters :: VerifiedPoolFilters -> UserPoolFilters
fromVerifiedPoolFilters (VerifiedPoolFilters fs) =
  UserPoolFilters $ decodeUtf8 $ encodePretty fs

-------------------------------------------------
-- Delegation Page Events
-------------------------------------------------
-- | The possible UI events on the Home page.
data DelegationEvent
  -- | Change the Delegation subscene to the specified subscene.
  = ChangeDelegationScene DelegationScene
  -- | Pair a new `StakeWallet`. It can only be done from the `DelegationSummary` subscene.
  | PairStakeWallet (AddWalletEvent StakeWallet)
  -- | Watch a new `StakeWallet`. It can only be done from the `DelegationSummary` subscene.
  | WatchStakeWallet (AddWalletEvent StakeWallet)
  -- | Show the details overlay for the specified item.
  | ShowDelegationDetails DelegationDetails
  -- | Close the details overlay and return to the previous screen.
  | CloseDelegationDetails
  -- | Filter the registered pools.
  | FilterRegisteredPools (FilterEvent VerifiedPoolFilters)
  -- | Quickly add a delegation certificate to the transaction builder for the pool
  -- currently being view with the details widget. The currently selected wallet
  -- is the one the certificate will be for.
  | QuickDelegate PoolID
  -- | Quickly set up the newWithdrawal field with the current stake wallet and open the widget
  -- for specifing how much ADA to withdraw.
  | QuickWithdraw
  -- | Quickly add a registration certificate. The currently selected wallet
  -- is the one the certificate will be for.
  | QuickRegister

-------------------------------------------------
-- Delegation State
-------------------------------------------------
data DelegationModel = DelegationModel
  -- | The current subscene.
  { _scene :: DelegationScene 
  -- | The currently focused `StakeWallet` from the list of tracked `StakeWallet`s.
  , _selectedWallet :: StakeWallet
  -- | Whether the pairing widget should be open.
  , _pairing :: Bool
  -- | Whether the watching widget should be open.
  , _watching :: Bool
  -- | Whether the filterRegisteredPools widget should be open.
  , _filteringRegisteredPools :: Bool
  -- | The information for the new `StakeWallet` being paired.
  , _newStakeWallet :: NewStakeWallet
  -- | The target details to show.
  , _details :: Maybe DelegationDetails
  -- | A list of all known registered pools.
  , _registeredPools :: [Pool]
  -- | The set filters for registered pools.
  , _setPoolFilters :: VerifiedPoolFilters
  -- | The new filters for registered pools.
  , _newPoolFilters :: UserPoolFilters
  } deriving (Eq,Show)

instance Default DelegationModel where
  def = DelegationModel 
    { _scene = DelegationSummary
    , _selectedWallet = def 
    , _pairing = False
    , _watching = False
    , _filteringRegisteredPools = False
    , _newStakeWallet = def
    , _details = Nothing
    , _registeredPools = []
    , _setPoolFilters = def
    , _newPoolFilters = def
    }

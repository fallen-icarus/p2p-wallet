{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

The Delegation scene is dedicated to `StakeWallet`s.

-}
module P2PWallet.Data.AppModel.DelegationModel where

import P2PWallet.Data.AppModel.Common
import P2PWallet.Data.Core.TxBody
import P2PWallet.Data.Core.Wallets.StakeWallet
import P2PWallet.Data.Koios.Pool
import P2PWallet.Prelude

-------------------------------------------------
-- Delegation Sub-Scene
-------------------------------------------------
data DelegationScene
  -- | The scene for viewing which stake pool is being delegated to.
  = StakeDelegationScene
  -- | The scene for viewing which drep is being delegated to.
  | GovernanceDelegationScene
  deriving (Show,Eq)

-------------------------------------------------
-- Delegation Page Events
-------------------------------------------------
-- | The possible UI events on the Home page.
data DelegationEvent
  -- | Change the delegation subscene.
  = ChangeDelegationScene DelegationScene
  -- | Pair a new `StakeWallet`. It can only be done from the `HomeAbout` subscene.
  | PairStakeWallet (AddEvent StakeWallet StakeWallet)
  -- | Watch a new `StakeWallet`. It can only be done from the `HomeAbout` subscene.
  | WatchStakeWallet (AddEvent StakeWallet StakeWallet)
  -- | Change a payment wallet name.
  | ChangeStakeWalletName (AddEvent Text StakeWallet)
  -- | Delete a payment wallet.
  | DeleteStakeWallet (DeleteWithConfirmationEvent StakeWallet)
  -- | Open the more popup widget
  | ShowDelegationMorePopup
  -- | Open the pool picker widget and sync the pools if necessary.
  | OpenPoolPicker
  -- | Change the pool picker's sorting method.
  | ChangePoolPickerSortMethod PoolSortMethod
  -- | Reset Pool Filters.
  | ResetPoolFilters
  -- | Sync all registered pools.
  | SyncRegisteredPools (ProcessEvent () [PoolList])
  -- | Add the selected user certificate to the tx builder. The `Text` is the target
  -- pool name so that the GUI can show it on the Builder scene.
  | AddSelectedUserCertificate (Maybe Text,CertificateAction)
  -- | Add the selected user withdrawal to the tx builder. 
  | AddSelectedUserWithdrawal StakeWallet
  -- | Get the information for a new drep delegation.
  | AddDrepDelegation (AddEvent () VoteDelegation)

-------------------------------------------------
-- Pool Filter Model
-------------------------------------------------
-- | Possible sortings.
data PoolSortMethod
  = PoolActiveSaturation SortDirection
  | PoolActivePledge SortDirection
  | PoolCost SortDirection
  | PoolMargin SortDirection
  deriving (Show,Eq)

data PoolFilterModel = PoolFilterModel
  -- | The targets to search for.
  { search :: Text
  -- | The number of stake pools to have loaded per page.
  , sampleSize :: Int
  -- | The current page.
  , currentPage :: Int
  -- | The sorting method.
  , sortMethod :: PoolSortMethod
  -- | The required range for the margin value.
  , marginRange :: (Decimal,Decimal)
  -- | The required range for the active saturation value.
  , activeSaturationRange :: (Decimal,Decimal)
  -- | The required range for the fixed cost value.
  , fixedCostRange :: (Maybe Decimal,Maybe Decimal)
  -- | The required range for the pledge value.
  , pledgeRange :: (Maybe Decimal,Maybe Decimal)
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PoolFilterModel

instance Default PoolFilterModel where
  def = PoolFilterModel
    { search = ""
    , sampleSize = 1000
    , currentPage = 0
    , sortMethod = PoolActiveSaturation SortDescending
    , marginRange = (0,100)
    , activeSaturationRange = (0,100)
    , fixedCostRange = (Nothing,Nothing)
    , pledgeRange = (Nothing,Nothing)
    }

-------------------------------------------------
-- Delegation State
-------------------------------------------------
data DelegationModel = DelegationModel
  -- | The currently focused `StakeWallet` from the list of tracked `StakeWallet`s.
  { selectedWallet :: StakeWallet
  -- | The current delegation scene.
  , scene :: DelegationScene
  -- | Whether to show the more popup.
  , showMorePopup :: Bool
  -- | Whether the add new wallet widget should be open.
  , addingWallet :: Bool
  -- | Whether the edit wallet widget should be open.
  , editingWallet :: Bool
  -- | Whether the delete wallet widget should be open.
  , deletingWallet :: Bool
  -- | The information for the new `StakeWallet` being added.
  , newStakeWallet :: NewStakeWallet
  -- | Whether to show the pool picker.
  , showPoolPicker :: Bool
  -- | The list of registered pools.
  , registeredPools :: [PoolList]
  -- | The current filter settings for the stake pools.
  , poolFilterModel :: PoolFilterModel
  -- | Whether to show the pool filter widget.
  , showPoolFilter :: Bool
  -- | The text field where new aliases are entered.
  , newAliasField :: Text
  -- | The new drep delegation.
  , newDrepDelegation :: Maybe VoteDelegation
  -- | The new drep id. This is only used when `VoteDelegation` is `DRepDelegation`.
  , newDrepId :: Text
  } deriving (Eq,Show)

makeFieldLabelsNoPrefix ''DelegationModel

instance Default DelegationModel where
  def = DelegationModel 
    { selectedWallet = def 
    , scene = StakeDelegationScene
    , showMorePopup = False
    , addingWallet = False
    , editingWallet = False
    , deletingWallet = False
    , newStakeWallet = def
    , showPoolPicker = False
    , registeredPools = []
    , poolFilterModel = def
    , showPoolFilter = False
    , newAliasField = ""
    , newDrepDelegation = Nothing
    , newDrepId = ""
    }

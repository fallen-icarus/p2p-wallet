{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{-

The Home scene is dedicated to `PaymentWallet`s.

-}
module P2PWallet.Data.App.Home where

import P2PWallet.Data.App.Common
import P2PWallet.Data.Core
import P2PWallet.Data.Plutus
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Koios.Transaction
import P2PWallet.Data.Wallets.PaymentWallet
import P2PWallet.Prelude

-------------------------------------------------
-- Scenes and Overlays
-------------------------------------------------
-- | The subscenes for the Home page.
data HomeScene
  -- | Information about the payment wallet as well as access to adding new payment wallets.
  = HomeAbout 
  -- | Transaction history for the address.
  | HomeTransactions 
  -- | UTxOs at the address.
  | HomeUTxOs 
  -- | Assets at the address.
  | HomeAssets 
  deriving (Eq,Show)

-- | The detail overlays that can be shown on the home page. The overlay will cover the entire
-- home page and prevent interacting with the home page until the overlay is closed.
data HomeDetails
  -- | View more detailed information about a particular UTxO.
  = HomeUTxO AddressUTxO
  -- | View more detailed information about a particular transaction.
  | HomeTransaction Transaction
  -- | View more detailed information about a particular native asset.
  | HomeAsset NativeAsset
  deriving (Show,Eq)

-------------------------------------------------
-- Filters
-------------------------------------------------
data AssetFilters = AssetFilters
  { _byPolicyId :: Maybe Text -- ^ Filter by policy id.
  , _byTokenName :: Maybe Text -- ^ Filter by asset name.
  , _byFingerprint :: Maybe Text -- ^ Filter by asset fingerprint.
  } deriving (Eq,Show)

instance Default AssetFilters where
  def = AssetFilters
    { _byPolicyId = Nothing
    , _byTokenName = Nothing
    , _byFingerprint = Nothing
    }

data UTxOFilters = UTxOFilters
  -- | Filter by policy id.
  { _byPolicyId :: Maybe Text 
  -- | Filter by asset name.
  , _byTokenName :: Maybe Text 
  -- | Filter by asset fingerprint.
  , _byFingerprint :: Maybe Text 
  -- | Filter by reference script hash. When it is `Just ""`, it will match any UTxO that has
  -- a reference script.
  , _byReferenceScriptHash :: Maybe Text 
  -- | Filter by datum hash. When it is `Just ""`, it will match any UTxO that has
  -- a datum. Inline datums still get returned with hashes from Koios.
  , _byDatumHash :: Maybe Text
  -- | Filter by specific transaction hash.
  , _byTxHash :: Maybe Text 
  } deriving (Eq,Show)

instance Default UTxOFilters where
  def = UTxOFilters
    { _byPolicyId = Nothing
    , _byTokenName = Nothing
    , _byFingerprint = Nothing
    , _byReferenceScriptHash = Nothing
    , _byDatumHash = Nothing
    , _byTxHash = Nothing
    }

data TransactionFilters = TransactionFilters
  { _byPolicyId :: Maybe Text -- ^ Filter by policy id.
  , _byTokenName :: Maybe Text -- ^ Filter by asset name.
  , _byFingerprint :: Maybe Text -- ^ Filter by asset fingerprint.
  } deriving (Eq,Show)

instance Default TransactionFilters where
  def = TransactionFilters
    { _byPolicyId = Nothing
    , _byTokenName = Nothing
    , _byFingerprint = Nothing
    }

-- | A type representing filters that have already been verified.
data VerifiedFilters = VerifiedFilters
  { _assetFilters :: AssetFilters
  , _utxoFilters :: UTxOFilters
  , _txFilters :: TransactionFilters
  } deriving (Show,Eq)

instance Default VerifiedFilters where
  def = VerifiedFilters
    { _assetFilters = def
    , _utxoFilters = def
    , _txFilters = def
    }

-- | A type representing user input that has yet to be checked for validity.
data UserFilters = UserFilters
  { _assetFilters :: AssetFilters
  , _utxoFilters :: UTxOFilters
  , _txFilters :: TransactionFilters
  } deriving (Show,Eq)

instance Default UserFilters where
  def = UserFilters
    { _assetFilters = def
    , _utxoFilters = def
    , _txFilters = def
    }

fromVerifiedFilters :: VerifiedFilters -> UserFilters
fromVerifiedFilters VerifiedFilters{..} =
  UserFilters
    { _assetFilters = _assetFilters
    , _utxoFilters = _utxoFilters
    , _txFilters = _txFilters
    }

-- | Try to convert `UserFilters` to `VerifiedFilters`. Fail with an error message if something
-- is invalid.
toVerifiedFilters :: UserFilters -> Either Text VerifiedFilters
toVerifiedFilters UserFilters{..} = do
    -- Verify the filters are valid.
    verifiedAssetFilters _assetFilters
    verifiedUtxoFilters _utxoFilters
    verifiedTxFilters _txFilters
    
    return $
      VerifiedFilters
        { _assetFilters = _assetFilters
        , _utxoFilters = _utxoFilters
        , _txFilters = _txFilters
        }
  where
    verifiedAssetFilters :: AssetFilters -> Either Text ()
    verifiedAssetFilters AssetFilters{..} = do
      flip unless (Left "Invalid policy id. It cannot be empty.") $ 
        maybe True (\pol -> pol /= "" && isJust (readHex pol)) _byPolicyId
      flip unless (Left "Invalid token name. It cannot be empty.") $ 
        maybe True (\name -> name /= "" && isJust (readHex name)) _byTokenName
      flip unless (Left "Invalid fingerprint. It cannot be empty.") $ 
        maybe True (/= "") _byFingerprint

    verifiedTxFilters :: TransactionFilters -> Either Text ()
    verifiedTxFilters TransactionFilters{..} = do
      flip unless (Left "Invalid policy id. It cannot be empty.") $ 
        maybe True (\pol -> pol /= "" && isJust (readHex pol)) _byPolicyId
      flip unless (Left "Invalid token name. It cannot be empty.") $ 
        maybe True (\name -> name /= "" && isJust (readHex name)) _byTokenName
      flip unless (Left "Invalid fingerprint. It cannot be empty.") $ 
        maybe True (/= "") _byFingerprint

    verifiedUtxoFilters :: UTxOFilters -> Either Text ()
    verifiedUtxoFilters UTxOFilters{..} = do
      flip unless (Left "Invalid policy id. It cannot be empty.") $ 
        maybe True (\pol -> pol /= "" && isJust (readHex pol)) _byPolicyId
      flip unless (Left "Invalid token name. It cannot be empty.") $ 
        maybe True (\name -> name /= "" && isJust (readHex name)) _byTokenName
      flip unless (Left "Invalid fingerprint. It cannot be empty.") $ 
        maybe True (/= "") _byFingerprint
      flip unless (Left "Invalid reference script hash.") $ 
        maybe True (isJust . readHex) _byReferenceScriptHash
      flip unless (Left "Invalid datum hash.") $ 
        maybe True (isJust . readHex) _byDatumHash
      flip unless (Left "Invalid transaction hash. It cannot be empty") $ 
        maybe True (\hash -> hash /= "" && isJust (readHex hash)) _byTxHash

-------------------------------------------------
-- Expand Tabs
-------------------------------------------------
-- | Whether the information is folded or unfolded. These settings are togglable.
-- This is mainly used for the transaction details overlay but it is (ideally) designed to be 
-- extensible if other widgets may use them in the future.
data ExpandedFields = ExpandedFields
  { _referenceInputs :: Bool
  , _collateralInputs :: Bool
  , _collateralOutput :: Bool
  , _inputs :: Bool
  , _outputs :: Bool
  , _certificates :: Bool
  , _withdrawals :: Bool
  } deriving (Eq,Show)

instance Default ExpandedFields where
  def = ExpandedFields 
    { _referenceInputs = False
    , _collateralInputs = False
    , _collateralOutput = False
    , _inputs = False
    , _outputs = False
    , _certificates = False
    , _withdrawals = False
    }

-------------------------------------------------
-- Add a new `PaymentWallet`
-------------------------------------------------
-- | The type representing information the user must supply in order to track a new `PaymentWallet`.
-- There is no need to ask for a staking address when adding a watched payment wallet since it
-- can be derived from the payment address.
data NewPaymentWallet = NewPaymentWallet
  -- | A user-friendly name for the address. This is used regardless of pairing/watching.
  { _alias :: Text 
  -- | What derivation path to use for the payment key. This is only used when pairing a payment
  -- wallet.
  , _paymentKeyPath :: Text 
  -- | What derivation path to use for the stake key. This is only used when pairing a payment
  -- wallet.
  , _stakeKeyPath :: Maybe Text 
  -- | The new payment address to watch. This is only used when adding a watched payment wallet.
  , _paymentAddress :: Text 
  } deriving (Show,Eq)

instance Default NewPaymentWallet where
  def = NewPaymentWallet 
    { _alias = ""
    , _paymentKeyPath = "1852H/1815H/0H/0/0" 
    , _stakeKeyPath = Nothing
    , _paymentAddress = ""
    }

-------------------------------------------------
-- Home Page Events
-------------------------------------------------
-- | The possible UI events on the Home page.
data HomeEvent
  -- | Change the Home subscene to the specified subscene.
  = ChangeHomeScene HomeScene
  -- | Show the details overlay for the specified item.
  | ShowHomeDetails HomeDetails
  -- | Close the details overlay and return to the previous screen.
  | CloseHomeDetails
  -- | Filter the assets in the `HomeAssets` subscene.
  | FilterHomeAssets (FilterEvent VerifiedFilters)
  -- | Filter the utxos in the `HomeUTxO` subscene.
  | FilterHomeUTxOs (FilterEvent VerifiedFilters)
  -- | Filter the transactions in the `HomeTransactions` subscene.
  | FilterHomeTransactions (FilterEvent VerifiedFilters)
  -- | Pair a new `PaymentWallet`. It can only be done from the `HomeAbout` subscene.
  | PairPaymentWallet (AddWalletEvent PaymentWallet)
  -- | Watch a new `PaymentWallet`. It can only be done from the `HomeAbout` subscene.
  | WatchPaymentWallet (AddWalletEvent PaymentWallet)

-------------------------------------------------
-- Home State
-------------------------------------------------
data HomeModel = HomeModel
  -- | The current subscene.
  { _scene :: HomeScene 
  -- | The target details to show. `Nothing` if no details need to be shown.
  , _details :: Maybe HomeDetails 
  -- | Which fields need to be currently expanded. This is mostly for the details overlay.
  , _expandedFields :: ExpandedFields
  -- | The currently focused `PaymentWallet` from the list of tracked `PaymentWallet`s.
  , _selectedWallet :: PaymentWallet
  -- | The currently set filters.
  , _setFilters :: VerifiedFilters
  -- | The new filter information.
  , _newFilters :: UserFilters
  -- | Whether the asset filtering widget should be open.
  , _filteringAssets :: Bool
  -- | Whether the utxo filtering widget should be open.
  , _filteringUtxos :: Bool
  -- | Whether the tx filtering widget should be open.
  , _filteringTxs :: Bool
  -- | Whether the pairing widget should be open.
  , _pairing :: Bool
  -- | Whether the watching widget should be open.
  , _watching :: Bool
  -- | The information for the new `PaymentWallet` being paired.
  , _newPaymentWallet :: NewPaymentWallet
  } deriving (Eq,Show)

instance Default HomeModel where
  def = HomeModel 
    { _scene = HomeAbout
    , _details = Nothing 
    , _expandedFields = def 
    , _selectedWallet = def 
    , _setFilters = def
    , _newFilters = def
    , _filteringAssets = False
    , _filteringUtxos = False
    , _filteringTxs = False
    , _pairing = False
    , _watching = False
    , _newPaymentWallet = def
    }

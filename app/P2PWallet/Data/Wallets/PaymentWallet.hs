{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Wallets.PaymentWallet where

import Data.Aeson

import Database.SQLite.Simple (field,Query(..),ToRow(..),FromRow(..))
import Database.SQLite.Simple.ToField (toField)

import P2PWallet.Data.Core
import P2PWallet.Data.Database
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Transaction
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Personal UTxO
-------------------------------------------------
-- | The type representing the information of a UTxO for a normal payment address.
data PersonalUTxO = PersonalUTxO
  { utxoRef :: TxOutRef
  , lovelace :: Lovelace
  , datumHash :: Maybe Text
  , inlineDatum :: Maybe Value
  , referenceScriptHash :: Maybe Text
  , nativeAssets :: [NativeAsset]
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  , showDetails :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PersonalUTxO

instance ToJSON PersonalUTxO where
  toJSON PersonalUTxO{..} =
    object [ "utxo_ref" .= utxoRef 
           , "lovelace" .= lovelace
           , "datum_hash" .= datumHash
           , "inline_datum" .= inlineDatum
           , "reference_script_hash" .= referenceScriptHash
           , "native_assets" .= nativeAssets
           , "block_time" .= blockTime
           , "block_height" .= blockHeight
           ]

instance FromJSON PersonalUTxO where
  parseJSON = withObject "PersonalUTxO" $ \o ->
    PersonalUTxO
      <$> o .: "utxo_ref"
      <*> o .: "lovelace"
      <*> o .: "datum_hash"
      <*> o .: "inline_datum"
      <*> o .: "reference_script_hash"
      <*> o .: "native_assets"
      <*> o .: "block_time"
      <*> o .: "block_height"
      <*> return False

toPersonalUTxO :: AddressUTxO -> PersonalUTxO
toPersonalUTxO AddressUTxO{..} = PersonalUTxO
  { utxoRef = utxoRef
  , lovelace = lovelace
  , datumHash = datumHash
  , inlineDatum = inlineDatum
  , referenceScriptHash = referenceScriptHash
  , nativeAssets = nativeAssets
  , blockTime = blockTime
  , blockHeight = blockHeight
  , showDetails = False
  }

-- | A lens to toggle the `showDetails` field of the `PersonalUTxO`.
toggleDetails :: TxOutRef -> Lens' [PersonalUTxO] Bool
toggleDetails ref = lens (getToggleDetails ref) (setToggleDetails ref)
  where
    getToggleDetails :: TxOutRef -> [PersonalUTxO] -> Bool
    getToggleDetails _ [] = False
    getToggleDetails targetRef (u:us) =
      if u ^. #utxoRef == targetRef 
      then u ^. #showDetails
      else getToggleDetails targetRef us

    setToggleDetails :: TxOutRef -> [PersonalUTxO] -> Bool -> [PersonalUTxO]
    setToggleDetails _ [] _ = []
    setToggleDetails targetRef (u:us) b =
      if u ^. #utxoRef == targetRef 
      then (u & #showDetails .~ b) : us
      else u : setToggleDetails targetRef us b

-------------------------------------------------
-- Payment Wallet
-------------------------------------------------
-- | A payment wallet is a payment address. If the payment address is a paired hardware
-- wallet, then `_paymentKeyPath` will be `Just derivationPath`. Only payment wallets
-- with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other
-- addresses, like cold wallets.
data PaymentWallet = PaymentWallet
  { network :: Network
  , profileId :: ProfileId
  , paymentId :: PaymentId
  , alias :: Text
  , paymentAddress :: PaymentAddress
  , paymentKeyPath :: Maybe DerivationPath
  , stakeAddress :: Maybe StakeAddress
  , stakeKeyPath :: Maybe DerivationPath
  , utxos :: [PersonalUTxO]
  , lovelace :: Lovelace
  , nativeAssets :: [NativeAsset]
  , transactions :: [Transaction] -- These are stored separately.
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''PaymentWallet

instance Ord PaymentWallet where
  p1 <= p2 = p1 ^. #paymentId <= p2 ^. #paymentId

instance Default PaymentWallet where
  def = PaymentWallet 
    { network = def
    , profileId = 0
    , paymentId = 0
    , alias = "dummy" 
    , paymentAddress = PaymentAddress "" 
    , stakeAddress = Nothing 
    , paymentKeyPath = Just $ PaymentKeyPath 0 0
    , stakeKeyPath = Nothing 
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    }

instance FromRow PaymentWallet where
  fromRow = do
    network <- field
    profileId <- field
    paymentId <- field
    alias <- field
    paymentAddress <- field
    stakeAddress <- field
    paymentKeyPath <- field
    stakeKeyPath <- field
    utxos <- fromMaybe mzero . decode <$> field
    lovelace <- field
    nativeAssets <- fromMaybe mzero . decode <$> field
    return $ PaymentWallet
      { network = network
      , profileId = profileId
      , paymentId = paymentId
      , alias = alias
      , paymentAddress = paymentAddress
      , stakeAddress = stakeAddress
      , paymentKeyPath = paymentKeyPath
      , stakeKeyPath = stakeKeyPath
      , utxos = utxos
      , lovelace = lovelace
      , nativeAssets = nativeAssets
      , transactions = []
      }

instance ToRow PaymentWallet where
  toRow PaymentWallet{..} =
    [ toField network
    , toField profileId
    , toField paymentId
    , toField alias
    , toField paymentAddress
    , toField stakeAddress
    , toField paymentKeyPath
    , toField stakeKeyPath
    , toField $ encode utxos
    , toField lovelace
    , toField $ encode nativeAssets
    ]

instance TableName PaymentWallet where
  tableName = "payment_wallets"

instance Creatable PaymentWallet where
  createStmt = Query $ unlines
    [ "CREATE TABLE " <> tableName @PaymentWallet
    , "( network TEXT NOT NULL,"
    ,  " profile_id INTEGER REFERENCES profiles (profile_id),"
    ,  " payment_id INTEGER PRIMARY KEY,"
    ,  " alias TEXT NOT NULL,"
    ,  " payment_address TEXT NOT NULL,"
    ,  " payment_key_path TEXT,"
    ,  " stake_address TEXT,"
    ,  " stake_key_path TEXT,"
    ,  " utxos BLOB,"
    ,  " lovelace INTEGER NOT NULL,"
    ,  " native_assets BLOB,"
    ,  " UNIQUE(network,profile_id,alias)"
    , ");"
    ]

instance Insertable PaymentWallet where
  insertStmt = Query $ unlines
    [ "INSERT OR REPLACE INTO " <> tableName @PaymentWallet
    , "( network,"
    ,  " profile_id,"
    ,  " payment_id,"
    ,  " alias,"
    ,  " payment_address,"
    ,  " payment_key_path,"
    ,  " stake_address,"
    ,  " stake_key_path,"
    ,  " utxos,"
    ,  " lovelace,"
    ,  " native_assets"
    , ") VALUES (?,?,?,?,?,?,?,?,?,?,?);"
    ]

instance Queryable PaymentWallet where
  queryStmt = Query $ "SELECT * FROM " <> tableName @PaymentWallet

instance Deletable PaymentWallet where
  deleteStmt = Query $ "DELETE FROM " <> tableName @PaymentWallet

-------------------------------------------------
-- New Payment Wallet
-------------------------------------------------
-- | The type representing information the user must supply in order to track a new `PaymentWallet`.
-- There is no need to ask for a staking address when adding a watched payment wallet since it
-- can be derived from the payment address.
data NewPaymentWallet = NewPaymentWallet
  -- | A user-friendly name for the address. This is used regardless of pairing/watching.
  { alias :: Text 
  -- | What address index for the payment key's derivation path. The account index is set by the
  -- profile. This is only used when pairing a payment wallet.
  , paymentAddressIndex :: Int 
  -- | What address index to use for the stake key's derivation path. The account index is set by 
  -- the profile. This is only used when pairing a payment wallet.
  , stakeAddressIndex :: Maybe Int 
  -- | The new payment address to watch. This is only used when adding a watched payment wallet.
  , paymentAddress :: Text 
  -- | Whether the new wallet is being paired.
  , pairing :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewPaymentWallet

instance Default NewPaymentWallet where
  def = NewPaymentWallet 
    { alias = ""
    , paymentAddressIndex = 0
    , stakeAddressIndex = Nothing
    , paymentAddress = ""
    , pairing = True
    }

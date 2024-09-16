{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets.PaymentWallet where

import Data.Aeson

import Database.SQLite.Simple (ToRow,FromRow)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Core.Transaction
import P2PWallet.Database
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
  , referenceScript :: Maybe ReferenceScript
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
           , "reference_script" .= referenceScript
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
      <*> o .: "reference_script"
      <*> o .: "native_assets"
      <*> o .: "block_time"
      <*> o .: "block_height"
      <*> return False

instance Default PersonalUTxO where
  def = PersonalUTxO
    { utxoRef = TxOutRef "" 0
    , lovelace = 0
    , datumHash = Nothing
    , inlineDatum = Nothing
    , referenceScript = Nothing
    , nativeAssets = []
    , blockTime = 0
    , blockHeight = 0
    , showDetails = False
    }

instance FromAddressUTxO PersonalUTxO where
  fromAddressUTxO AddressUTxO{..} = PersonalUTxO
    { utxoRef = utxoRef
    , lovelace = lovelace
    , datumHash = datumHash
    , inlineDatum = inlineDatum
    , referenceScript = referenceScript
    , nativeAssets = nativeAssets
    , blockTime = blockTime
    , blockHeight = blockHeight
    , showDetails = False
    }

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
  , paymentWalletId :: PaymentWalletId
  , alias :: Text
  , paymentAddress :: PaymentAddress
  , paymentKeyDerivation :: Maybe DerivationInfo
  , stakeAddress :: Maybe StakeAddress
  , stakeKeyDerivation :: Maybe DerivationInfo
  , utxos :: [PersonalUTxO]
  , lovelace :: Lovelace
  , nativeAssets :: [NativeAsset]
  , transactions :: [Transaction]
  } deriving (Show,Eq,Generic,ToRow,FromRow)

makeFieldLabelsNoPrefix ''PaymentWallet

instance Ord PaymentWallet where
  p1 <= p2 = p1 ^. #paymentWalletId <= p2 ^. #paymentWalletId

instance Default PaymentWallet where
  def = PaymentWallet 
    { network = def
    , profileId = 0
    , paymentWalletId = 0
    , alias = "dummy" 
    , paymentAddress = PaymentAddress "" 
    , stakeAddress = Nothing 
    , paymentKeyDerivation = Just (Nothing, PaymentKeyPath 0 0)
    , stakeKeyDerivation = Nothing 
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    }

instance TableName PaymentWallet where
  tableName = "payment_wallets"

instance Creatable PaymentWallet where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @PaymentWallet
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "payment_wallet_id INTEGER PRIMARY KEY"
        , "alias TEXT NOT NULL"
        , "payment_address TEXT NOT NULL"
        , "payment_key_derivation TEXT"
        , "stake_address TEXT"
        , "stake_key_derivation TEXT"
        , "utxos BLOB"
        , "lovelace INTEGER NOT NULL"
        , "native_assets BLOB"
        , "transactions BLOB"
        , "UNIQUE(network,profile_id,payment_wallet_id,alias)"
        ]
    , ");"
    ]

instance Insertable PaymentWallet where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @PaymentWallet
    , "("
    , unwords $ intersperse ","
        [ "network"
        , "profile_id"
        , "payment_wallet_id"
        , "alias"
        , "payment_address"
        , "payment_key_derivation"
        , "stake_address"
        , "stake_key_derivation"
        , "utxos"
        , "lovelace"
        , "native_assets"
        , "transactions"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

instance Notify PaymentWallet where
  notify oldState newState
    | null msg = Nothing
    | otherwise = Just $ Notification
        { notificationType = PaymentNotification
        , alias = oldState ^. #alias
        , message = mconcat $ intersperse "\n" msg
        , markedAsRead = False
        }
    where
      lovelaceDiff = newState ^. #lovelace - oldState ^. #lovelace
      nativeAssetDiff = sumNativeAssets $ mconcat
        [ newState ^. #nativeAssets
        , map (over #quantity negate) $ oldState ^. #nativeAssets
        ]

      lovelaceMsg
        | lovelaceDiff > 0 = display lovelaceDiff <> " was deposited."
        | lovelaceDiff < 0 = display (abs lovelaceDiff) <> " was spent."
        | otherwise = ""

      nativeAssetMsg
        | null nativeAssetDiff = ""
        | otherwise = "Native asset balances have changed."

      msg = filter (/= "")
        [ lovelaceMsg
        , nativeAssetMsg
        ]

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

-- NOTE: Validation of new payment wallet information occurs in `P2PWallet.Actions.AddWallet`.

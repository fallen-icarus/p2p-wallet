{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets.DexWallet where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)

import Database.SQLite.Simple (field,ToRow(..),FromRow(..))
import Database.SQLite.Simple.ToField (toField)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoSwaps.Common
import P2PWallet.Data.DeFi.CardanoSwaps.OneWaySwaps qualified as OneWay
import P2PWallet.Data.DeFi.CardanoSwaps.TwoWaySwaps qualified as TwoWay
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Database
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Swap Datum
-------------------------------------------------
-- | The swap datum used for the UTxO.
data SwapDatum
  = OneWay OneWay.SwapDatum
  | TwoWay TwoWay.SwapDatum
  -- | This is included in case a bug is found in the protocols.
  | SwapDatumError Value
  deriving (Eq,Show)

makePrisms ''SwapDatum

instance FromJSON SwapDatum where
  parseJSON value = pure $ fromMaybe (SwapDatumError value) $ asum
    [ OneWay <$> parseMaybe (parseJSON @OneWay.SwapDatum) value
    , TwoWay <$> parseMaybe (parseJSON @TwoWay.SwapDatum) value
    ]

instance ToJSON SwapDatum where
  toJSON (OneWay datum) = toJSON datum
  toJSON (TwoWay datum) = toJSON datum
  toJSON (SwapDatumError value) = toJSON value

-------------------------------------------------
-- Swap UTxO
-------------------------------------------------
-- | The type representing the information of a UTxO for a swap address. 
data SwapUTxO = SwapUTxO
  { utxoRef :: TxOutRef
  , swapAddress :: PaymentAddress
  , swapType :: SwapType
  , lovelace :: Lovelace
  , swapDatum :: Maybe SwapDatum
  , nativeAssets :: [NativeAsset]
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''SwapUTxO

instance ToJSON SwapUTxO where
  toJSON SwapUTxO{..} =
    object [ "utxo_ref" .= utxoRef 
           , "swap_address" .= swapAddress
           , "swap_type" .= swapType
           , "lovelace" .= lovelace
           , "swap_datum" .= swapDatum
           , "native_assets" .= nativeAssets
           , "block_time" .= blockTime
           , "block_height" .= blockHeight
           ]

instance FromJSON SwapUTxO where
  parseJSON = withObject "SwapUTxO" $ \o ->
    SwapUTxO
      <$> o .: "utxo_ref"
      <*> o .: "swap_address"
      <*> o .: "swap_type"
      <*> o .: "lovelace"
      <*> o .: "swap_datum"
      <*> o .: "native_assets"
      <*> o .: "block_time"
      <*> o .: "block_height"

toSwapUTxO :: AddressUTxO -> SwapUTxO
toSwapUTxO AddressUTxO{..} = SwapUTxO
    { utxoRef = utxoRef
    , swapAddress = paymentAddress
    , swapType = if OneWay.isSwapAddress paymentAddress then LimitOrder else LiquiditySwap
    , lovelace = lovelace
    , swapDatum = parseInlineDatum <$> inlineDatum
    , nativeAssets = nativeAssets
    , blockTime = blockTime
    , blockHeight = blockHeight
    }
  where
    parseInlineDatum :: Value -> SwapDatum
    parseInlineDatum value =
      fromMaybe (SwapDatumError value) $ asum
        [ OneWay <$> decodeDatum @OneWay.SwapDatum value
        , TwoWay <$> decodeDatum @TwoWay.SwapDatum value
        ]

-- Get the prices from a SwapUTxO based on the swap direction. 
swapUTxOPrice :: OfferAsset -> AskAsset -> SwapUTxO -> Maybe Rational
swapUTxOPrice (OfferAsset offerAsset) (AskAsset askAsset) SwapUTxO{swapDatum} = case swapDatum of
  Just (OneWay OneWay.SwapDatum{swapPrice=price}) -> Just $ toGHC price
  Just (TwoWay TwoWay.SwapDatum{..}) ->
    Just $ toGHC $ if offerAsset < askAsset then asset1Price else asset2Price
  _ -> Nothing

-------------------------------------------------
-- Dex Wallet
-------------------------------------------------
-- | A dex wallet is all CardanoSwaps addresses using that staking credential. If the staking
-- credential is a paired hardware wallet, then `_stakeKeyPath` will be `Just derivationPath`. Only
-- dex wallets with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other dex wallets,
-- like cold dex wallets.
data DexWallet = DexWallet
  { network :: Network
  , profileId :: ProfileId
  -- | The payment id used for the dex wallet. The payment id is used as the row id for multiple
  -- tables.
  , paymentId :: PaymentId
  -- | The stake id for the stake credential used.
  , stakeId :: StakeId
  -- | Alias for the stake credential. This will also be used as the alias for this swap wallet.
  , alias :: Text
  , oneWaySwapAddress :: PaymentAddress
  , twoWaySwapAddress :: PaymentAddress
  , stakeAddress :: StakeAddress
  , stakeKeyPath :: Maybe DerivationPath
  , utxos :: [SwapUTxO]
  , lovelace :: Lovelace
  , nativeAssets :: [NativeAsset]
  , transactions :: [Transaction] -- These are stored separately.
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''DexWallet

instance Ord DexWallet where
  p1 <= p2 = p1 ^. #paymentId <= p2 ^. #paymentId

instance Default DexWallet where
  def = DexWallet
    { network = def
    , profileId = 0
    , paymentId = 0
    , stakeId = 0
    , alias = "dummy" 
    , oneWaySwapAddress = "" 
    , twoWaySwapAddress = "" 
    , stakeAddress = "" 
    , stakeKeyPath = Nothing 
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    }

instance FromRow DexWallet where
  fromRow = do
    network <- field
    profileId <- field
    paymentId <- field
    stakeId <- field
    alias <- field
    oneWaySwapAddress <- field
    twoWaySwapAddress <- field
    stakeAddress <- field
    stakeKeyPath <- field
    utxos <- fromMaybe mzero . decode <$> field
    lovelace <- field
    nativeAssets <- fromMaybe mzero . decode <$> field
    return $ DexWallet
      { network = network
      , profileId = profileId
      , paymentId = paymentId
      , stakeId = stakeId
      , alias = alias
      , oneWaySwapAddress = oneWaySwapAddress
      , twoWaySwapAddress = twoWaySwapAddress
      , stakeAddress = stakeAddress
      , stakeKeyPath = stakeKeyPath
      , utxos = utxos
      , lovelace = lovelace
      , nativeAssets = nativeAssets
      , transactions = []
      }

instance ToRow DexWallet where
  toRow DexWallet{..} =
    [ toField network
    , toField profileId
    , toField paymentId
    , toField stakeId
    , toField alias
    , toField oneWaySwapAddress
    , toField twoWaySwapAddress
    , toField stakeAddress
    , toField stakeKeyPath
    , toField $ encode utxos
    , toField lovelace
    , toField $ encode nativeAssets
    ]

instance TableName DexWallet where
  tableName = "dex_wallets"

instance Creatable DexWallet where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @DexWallet
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "payment_id INTEGER PRIMARY KEY"
        , "stake_id INTEGER REFERENCES stake_wallets (stake_id)"
        , "alias TEXT NOT NULL"
        , "one_way_swap_address TEXT NOT NULL"
        , "two_way_swap_address TEXT NOT NULL"
        , "stake_address TEXT NOT NULL"
        , "stake_key_path TEXT"
        , "utxos BLOB"
        , "lovelace INTEGER NOT NULL"
        , "native_assets BLOB"
        , "UNIQUE(network,profile_id,payment_id,alias)"
        ]
    , ");"
    ]

instance Insertable DexWallet where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @DexWallet
    , "("
    , unwords $ intersperse ","
        [ "network"
        , "profile_id"
        , "payment_id"
        , "stake_id"
        , "alias"
        , "one_way_swap_address"
        , "two_way_swap_address"
        , "stake_address"
        , "stake_key_path"
        , "utxos"
        , "lovelace"
        , "native_assets"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

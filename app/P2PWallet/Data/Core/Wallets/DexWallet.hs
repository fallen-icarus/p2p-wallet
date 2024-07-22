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
  = OneWayDatum OneWay.SwapDatum
  | TwoWayDatum TwoWay.SwapDatum
  -- | This is included in case a bug is found in the protocols.
  | SwapDatumError Value
  deriving (Eq,Show)

makePrisms ''SwapDatum

instance FromJSON SwapDatum where
  parseJSON value = pure $ fromMaybe (SwapDatumError value) $ asum
    [ OneWayDatum <$> parseMaybe (parseJSON @OneWay.SwapDatum) value
    , TwoWayDatum <$> parseMaybe (parseJSON @TwoWay.SwapDatum) value
    ]

instance ToJSON SwapDatum where
  toJSON (OneWayDatum datum) = toJSON datum
  toJSON (TwoWayDatum datum) = toJSON datum
  toJSON (SwapDatumError value) = toJSON value

parseInlineSwapDatum :: Value -> SwapDatum
parseInlineSwapDatum value =
  fromMaybe (SwapDatumError value) $ asum
    [ OneWayDatum <$> decodeData @OneWay.SwapDatum value
    , TwoWayDatum <$> decodeData @TwoWay.SwapDatum value
    ]

-- | Get the swap input that was executed to produce this datum.
prevSwapInput :: SwapDatum -> Maybe TxOutRef
prevSwapInput swapDatum = case swapDatum of
  OneWayDatum OneWay.SwapDatum{prevInput} -> prevInput
  TwoWayDatum TwoWay.SwapDatum{prevInput} -> prevInput
  _ -> Nothing

-------------------------------------------------
-- Swap Redeemer
-------------------------------------------------
-- | The swap redeemer used for the UTxO.
data SwapRedeemer
  = OneWayRedeemer OneWay.SwapRedeemer
  | TwoWayRedeemer TwoWay.SwapRedeemer
  -- | This is included in case a bug is found in the protocols.
  | SwapRedeemerError Value
  deriving (Eq,Show)

makePrisms ''SwapRedeemer

parseSwapRedeemer :: Value -> SwapRedeemer
parseSwapRedeemer value =
  fromMaybe (SwapRedeemerError value) $ asum
    [ OneWayRedeemer <$> decodeData @OneWay.SwapRedeemer value
    , TwoWayRedeemer <$> decodeData @TwoWay.SwapRedeemer value
    ]

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

instance Default SwapUTxO where
  def = SwapUTxO
    { utxoRef = TxOutRef "" 0
    , swapAddress = ""
    , swapType = LimitOrder
    , lovelace = 0
    , swapDatum = Nothing
    , nativeAssets = []
    , blockTime = 0
    , blockHeight = 0
    }

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

instance FromAddressUTxO SwapUTxO where
  fromAddressUTxO AddressUTxO{..} = SwapUTxO
      { utxoRef = utxoRef
      , swapAddress = paymentAddress
      , swapType = if OneWay.isSwapAddress paymentAddress then LimitOrder else LiquiditySwap
      , lovelace = lovelace
      , swapDatum = parseInlineSwapDatum <$> inlineDatum
      , nativeAssets = nativeAssets
      , blockTime = blockTime
      , blockHeight = blockHeight
      }

-- | Get the prices from a SwapUTxO based on the swap direction. 
swapUTxOPrice :: OfferAsset -> AskAsset -> SwapUTxO -> Maybe Rational
swapUTxOPrice (OfferAsset offerAsset) (AskAsset askAsset) SwapUTxO{swapDatum} = case swapDatum of
  Just (OneWayDatum OneWay.SwapDatum{swapPrice=price}) -> Just $ toGHC price
  Just (TwoWayDatum TwoWay.SwapDatum{..}) ->
    Just $ toGHC $ if offerAsset < askAsset then asset1Price else asset2Price
  _ -> Nothing

-- | Get the offer asset if it is a one-way swap.
swapUTxOOfferAsset :: SwapUTxO -> Maybe NativeAsset
swapUTxOOfferAsset SwapUTxO{swapDatum} = case swapDatum of
  Just (OneWayDatum OneWay.SwapDatum{offerId,offerName}) -> Just NativeAsset
    { policyId = offerId
    , tokenName = offerName
    , fingerprint = mkAssetFingerprint offerId offerName
    , quantity = 0
    }
  _ -> Nothing
  
-- | Get the ask asset if it is a one-way swap.
swapUTxOAskAsset :: SwapUTxO -> Maybe NativeAsset
swapUTxOAskAsset SwapUTxO{swapDatum} = case swapDatum of
  Just (OneWayDatum OneWay.SwapDatum{askId,askName}) -> Just NativeAsset
    { policyId = askId
    , tokenName = askName
    , fingerprint = mkAssetFingerprint askId askName
    , quantity = 0
    }
  _ -> Nothing
  
-- | Get the asset1 if it is a two-way swap.
swapUTxOAsset1 :: SwapUTxO -> Maybe NativeAsset
swapUTxOAsset1 SwapUTxO{swapDatum} = case swapDatum of
  Just (TwoWayDatum TwoWay.SwapDatum{asset1Id,asset1Name}) -> Just NativeAsset
    { policyId = asset1Id
    , tokenName = asset1Name
    , fingerprint = mkAssetFingerprint asset1Id asset1Name
    , quantity = 0
    }
  _ -> Nothing
  
-- | Get the asset2 if it is a two-way swap.
swapUTxOAsset2 :: SwapUTxO -> Maybe NativeAsset
swapUTxOAsset2 SwapUTxO{swapDatum} = case swapDatum of
  Just (TwoWayDatum TwoWay.SwapDatum{asset2Id,asset2Name}) -> Just NativeAsset
    { policyId = asset2Id
    , tokenName = asset2Name
    , fingerprint = mkAssetFingerprint asset2Id asset2Name
    , quantity = 0
    }
  _ -> Nothing

swapIsFullyConverted :: SwapUTxO -> Bool
swapIsFullyConverted u = flip any offerSample $ \x@NativeAsset{policyId} ->
    if policyId == "" 
    then quantityOf x u < 5_000_000 
    else quantityOf x u == 0
  where
    offerSample = catMaybes
      [ swapUTxOOfferAsset u
      , swapUTxOAsset1 u
      , swapUTxOAsset2 u
      ]

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
  , stakeKeyDerivation :: Maybe DerivationInfo
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
    , stakeKeyDerivation = Nothing 
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
    stakeKeyDerivation <- field
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
      , stakeKeyDerivation = stakeKeyDerivation
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
    , toField stakeKeyDerivation
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
        , "stake_key_derivation TEXT"
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
        , "stake_key_derivation"
        , "utxos"
        , "lovelace"
        , "native_assets"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?);"
    ]

instance Notify DexWallet where
  notify oldState newState
    | oldState ^. #lovelace == newState ^. #lovelace && 
      oldState ^. #nativeAssets == newState ^. #nativeAssets = Nothing
    | otherwise = Just $ Notification
        { notificationType = DexNotification
        , alias = oldState ^. #alias
        , message = msg
        , markedAsRead = False
        }
    where
      msg :: Text
      msg = unlines $ filter (/= "")
        [ "Swap statuses have changed."
        , if any swapIsFullyConverted $ newState ^. #utxos
          then "Some swaps have been fully converted!"
          else ""
        ]

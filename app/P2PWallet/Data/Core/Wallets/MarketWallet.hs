{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Wallets.MarketWallet where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)

import Database.SQLite.Simple (ToRow,FromRow)

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Transaction
import P2PWallet.Data.DeFi.CardanoAftermarket qualified as Aftermarket
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Database
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Aftermarket Datum
-------------------------------------------------
-- | The aftermarket datum used for the aftermarket address UTxO.
data AftermarketDatum
  = SpotDatum Aftermarket.SpotDatum
  | AuctionDatum Aftermarket.AuctionDatum
  | SpotBidDatum Aftermarket.SpotBidDatum
  | ClaimBidDatum Aftermarket.ClaimBidDatum
  | AcceptedBidDatum Aftermarket.AcceptedBidDatum
  -- | This is included in case a bug is found in the protocols.
  | AftermarketDatumError Value
  deriving (Eq,Show)

makePrisms ''AftermarketDatum

instance FromJSON AftermarketDatum where
  parseJSON value = pure $ fromMaybe (AftermarketDatumError value) $ asum
    [ SpotDatum <$> parseMaybe (parseJSON @Aftermarket.SpotDatum) value
    , AuctionDatum <$> parseMaybe (parseJSON @Aftermarket.AuctionDatum) value
    , SpotBidDatum <$> parseMaybe (parseJSON @Aftermarket.SpotBidDatum) value
    , ClaimBidDatum <$> parseMaybe (parseJSON @Aftermarket.ClaimBidDatum) value
    , AcceptedBidDatum <$> parseMaybe (parseJSON @Aftermarket.AcceptedBidDatum) value
    ]

instance ToJSON AftermarketDatum where
  toJSON (SpotDatum datum) = toJSON datum
  toJSON (AuctionDatum datum) = toJSON datum
  toJSON (SpotBidDatum datum) = toJSON datum
  toJSON (ClaimBidDatum value) = toJSON value
  toJSON (AcceptedBidDatum value) = toJSON value
  toJSON (AftermarketDatumError value) = toJSON value

parseInlineAftermarketDatum :: Value -> AftermarketDatum
parseInlineAftermarketDatum value =
  fromMaybe (AftermarketDatumError value) $ asum
    [ SpotDatum <$> decodeData @Aftermarket.SpotDatum value
    , AuctionDatum <$> decodeData @Aftermarket.AuctionDatum value
    , SpotBidDatum <$> decodeData @Aftermarket.SpotBidDatum value
    , ClaimBidDatum <$> decodeData @Aftermarket.ClaimBidDatum value
    , AcceptedBidDatum <$> decodeData @Aftermarket.AcceptedBidDatum value
    ]

-- | Get the NFTs from an aftermarket datum.
aftermarketDatumNfts :: AftermarketDatum -> Maybe (CurrencySymbol,[TokenName])
aftermarketDatumNfts marketDatum = case marketDatum of
  SpotDatum Aftermarket.SpotDatum{nftPolicyId,nftNames} -> Just (nftPolicyId, nftNames)
  AuctionDatum Aftermarket.AuctionDatum{nftPolicyId,nftNames} -> Just (nftPolicyId, nftNames)
  SpotBidDatum Aftermarket.SpotBidDatum{nftPolicyId,nftNames} -> Just (nftPolicyId, nftNames)
  ClaimBidDatum Aftermarket.ClaimBidDatum{nftPolicyId,nftNames} -> Just (nftPolicyId, nftNames)
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{nftPolicyId,nftNames} -> Just (nftPolicyId, nftNames)
  _ -> Nothing

-- | Get the seller's price from an aftermarket datum.
aftermarketDatumSellerPrice :: AftermarketDatum -> Maybe Aftermarket.Prices
aftermarketDatumSellerPrice marketDatum = case marketDatum of
  SpotDatum Aftermarket.SpotDatum{salePrice} -> Just salePrice
  AuctionDatum Aftermarket.AuctionDatum{startingPrice} -> Just startingPrice
  _ -> Nothing

-- | Get the seller's payment address from an aftermarket datum.
aftermarketDatumSellerAddress :: AftermarketDatum -> Maybe Address
aftermarketDatumSellerAddress marketDatum = case marketDatum of
  SpotDatum Aftermarket.SpotDatum{paymentAddress} -> Just paymentAddress
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{paymentAddress} -> Just paymentAddress
  _ -> Nothing

-------------------------------------------------
-- Aftermarket UTxO
-------------------------------------------------
-- | The type representing the information of a UTxO for an aftermarket address. 
data AftermarketUTxO = AftermarketUTxO
  { utxoRef :: TxOutRef
  , marketAddress :: PaymentAddress
  , lovelace :: Lovelace
  , marketDatum :: Maybe AftermarketDatum
  , nativeAssets :: [NativeAsset]
  , blockTime :: POSIXTime
  , blockHeight :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''AftermarketUTxO

instance Default AftermarketUTxO where
  def = AftermarketUTxO
    { utxoRef = TxOutRef "" 0
    , marketAddress = ""
    , lovelace = 0
    , marketDatum = Nothing
    , nativeAssets = []
    , blockTime = 0
    , blockHeight = 0
    }

instance ToJSON AftermarketUTxO where
  toJSON AftermarketUTxO{..} =
    object [ "utxo_ref" .= utxoRef 
           , "market_address" .= marketAddress
           , "lovelace" .= lovelace
           , "market_datum" .= marketDatum
           , "native_assets" .= nativeAssets
           , "block_time" .= blockTime
           , "block_height" .= blockHeight
           ]

instance FromJSON AftermarketUTxO where
  parseJSON = withObject "AftermarketUTxO" $ \o ->
    AftermarketUTxO
      <$> o .: "utxo_ref"
      <*> o .: "market_address"
      <*> o .: "lovelace"
      <*> o .: "market_datum"
      <*> o .: "native_assets"
      <*> o .: "block_time"
      <*> o .: "block_height"

instance FromAddressUTxO AftermarketUTxO where
  fromAddressUTxO AddressUTxO{..} = AftermarketUTxO
      { utxoRef = utxoRef
      , marketAddress = paymentAddress
      , lovelace = lovelace
      , marketDatum = parseInlineAftermarketDatum <$> inlineDatum
      , nativeAssets = nativeAssets
      , blockTime = blockTime
      , blockHeight = blockHeight
      }

-- | Get the SpotDatum from an AftermarketUTxO.
aftermarketUTxOSpotDatum :: AftermarketUTxO -> Maybe Aftermarket.SpotDatum
aftermarketUTxOSpotDatum AftermarketUTxO{marketDatum} = case marketDatum of
  Just (SpotDatum spotDatum) -> Just spotDatum
  _ -> Nothing

-- | Get the AuctionDatum from an AftermarketUTxO.
aftermarketUTxOAuctionDatum :: AftermarketUTxO -> Maybe Aftermarket.AuctionDatum
aftermarketUTxOAuctionDatum AftermarketUTxO{marketDatum} = case marketDatum of
  Just (AuctionDatum auctionDatum) -> Just auctionDatum
  _ -> Nothing

-- | Get the NFTs from an AftermarketUTxO.
aftermarketUTxONfts :: AftermarketUTxO -> Maybe (CurrencySymbol,[TokenName])
aftermarketUTxONfts AftermarketUTxO{marketDatum} = marketDatum >>= aftermarketDatumNfts

-- | Get the seller's price from an AftermarketUTxO.
aftermarketUTxOSellerPrice :: AftermarketUTxO -> Maybe Aftermarket.Prices
aftermarketUTxOSellerPrice AftermarketUTxO{marketDatum} = 
  marketDatum >>= aftermarketDatumSellerPrice

-- | Get the seller's payment address from an AftermarketUTxO.
aftermarketUTxOSellerAddress :: AftermarketUTxO -> Maybe Address
aftermarketUTxOSellerAddress AftermarketUTxO{marketDatum} = 
  marketDatum >>= aftermarketDatumSellerAddress

-------------------------------------------------
-- Aftermarket Wallet
-------------------------------------------------
-- | A market wallet is the CardanoAftermarket address using that staking credential. If the staking
-- credential is a paired hardware wallet, then `_stakeKeyPath` will be `Just derivationPath`. Only
-- market wallets with known derivation paths can sign transactions using the app. By allowing the
-- derivation paths to be optional, it makes it possible for users to "watch" other aftermarket wallets,
-- like cold aftermarket wallets.
--
-- This wallet tracks all activity for the staking credential, regardless of it is being used to
-- sell or buy. Sales take place in the `marketAddress` while bids take place in other market
-- addresses but are queryable by the BidderId beacon that uses the `stakeCredential` of this wallet.
data MarketWallet = MarketWallet
  { network :: Network
  , profileId :: ProfileId
  -- | The wallet id used for the aftermarket wallet. 
  , marketWalletId :: MarketWalletId
  -- | The stake wallet id for the stake credential used.
  , stakeWalletId :: StakeWalletId
  -- | Alias for the stake credential. This will also be used as the alias for this market wallet.
  , alias :: Text
  , marketAddress :: PaymentAddress
  , stakeAddress :: StakeAddress
  , stakeKeyDerivation :: Maybe DerivationInfo
  , stakeCredential :: Credential
  -- | All UTxOs currently locked at the `marketAddress`.
  , utxos :: [AftermarketUTxO]
  , lovelace :: Lovelace
  , nativeAssets :: [NativeAsset]
  -- | The transaction history for this market address. This includes transactions initiated
  -- by buyers making/closing bids.
  , transactions :: [Transaction]
  -- | Bid UTxOs for this staking credential used as a Bidder id. Only Bid UTxOs are tracked. These
  -- UTxOs are _not_ located at the `marketAddress`.
  , bidUTxOs :: [AftermarketUTxO]
  -- | Transaction history of the Bidder Id associated with this staking credential.
  , bidTransactions :: [Transaction]
  } deriving (Show,Eq,Generic,FromRow,ToRow)

makeFieldLabelsNoPrefix ''MarketWallet

instance Ord MarketWallet where
  p1 <= p2 = p1 ^. #marketWalletId <= p2 ^. #marketWalletId

instance Default MarketWallet where
  def = MarketWallet
    { network = def
    , profileId = 0
    , marketWalletId = 0
    , stakeWalletId = 0
    , alias = "dummy" 
    , marketAddress = "" 
    , stakeAddress = "" 
    , stakeKeyDerivation = Nothing 
    , stakeCredential = PubKeyCredential ""
    , utxos = []
    , lovelace = 0
    , nativeAssets = []
    , transactions = []
    , bidUTxOs = []
    , bidTransactions = []
    }

instance TableName MarketWallet where
  tableName = "market_wallets"

instance Creatable MarketWallet where
  createStmt = Query $ unwords
    [ "CREATE TABLE IF NOT EXISTS " <> tableName @MarketWallet
    , "("
    , unwords $ intersperse ","
        [ "network TEXT NOT NULL"
        , "profile_id INTEGER REFERENCES profiles (profile_id)"
        , "market_wallet_id INTEGER PRIMARY KEY"
        , "stake_wallet_id INTEGER REFERENCES stake_wallets (stake_wallet_id)"
        , "alias TEXT NOT NULL"
        , "market_address TEXT NOT NULL"
        , "stake_address TEXT NOT NULL"
        , "stake_key_derivation TEXT"
        , "stake_credential TEXT NOT NULL"
        , "utxos BLOB"
        , "lovelace INTEGER NOT NULL"
        , "native_assets BLOB"
        , "transactions BLOB"
        , "bid_utxos BLOB"
        , "bid_transactions BLOB"
        , "UNIQUE(network,profile_id,market_wallet_id,alias)"
        ]
    , ");"
    ]

instance Insertable MarketWallet where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @MarketWallet
    , "("
    , unwords $ intersperse ","
        [ "network"
        , "profile_id"
        , "market_wallet_id"
        , "stake_wallet_id"
        , "alias"
        , "market_address"
        , "stake_address"
        , "stake_key_derivation"
        , "stake_credential"
        , "utxos"
        , "lovelace"
        , "native_assets"
        , "transactions"
        , "bid_utxos"
        , "bid_transactions"
        ]
    , ")"
    , "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);"
    ]


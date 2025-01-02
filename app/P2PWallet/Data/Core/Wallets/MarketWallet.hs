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
-- Key Types
-------------------------------------------------
-- | The types of Key NFTs.
data KeyNftType
  = LoanKey
  | OptionsKey
  | OtherNft
  deriving (Show,Eq)

instance Default KeyNftType where
  def = OtherNft

-------------------------------------------------
-- Bid Types
-------------------------------------------------
-- | The types of possible bid UTxOs.
data BidType
  = SpotBid
  | ClaimBid
  | AcceptedBid
  deriving (Show,Eq)

instance Default BidType where
  def = SpotBid

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
    -- This must be tried before SpotBidDatum because SpotBidDatum is a subset of AcceptedBidDatum
    -- and decoding will stop despite more fields remaining.
    , AcceptedBidDatum <$> parseMaybe (parseJSON @Aftermarket.AcceptedBidDatum) value
    , SpotBidDatum <$> parseMaybe (parseJSON @Aftermarket.SpotBidDatum) value
    , ClaimBidDatum <$> parseMaybe (parseJSON @Aftermarket.ClaimBidDatum) value
    ]

instance ToJSON AftermarketDatum where
  toJSON (SpotDatum datum) = toJSON datum
  toJSON (AuctionDatum datum) = toJSON datum
  toJSON (SpotBidDatum datum) = toJSON datum
  toJSON (ClaimBidDatum datum) = toJSON datum
  toJSON (AcceptedBidDatum datum) = toJSON datum
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

-- | Get the bidderId from an aftermarket datum.
aftermarketDatumBidderId :: AftermarketDatum -> Maybe Aftermarket.BidderId
aftermarketDatumBidderId marketDatum = case marketDatum of
  SpotBidDatum Aftermarket.SpotBidDatum{bidderCredential} -> 
    Just $ Aftermarket.genBidderId bidderCredential
  ClaimBidDatum Aftermarket.ClaimBidDatum{bidderCredential} ->
    Just $ Aftermarket.genBidderId bidderCredential
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{bidderCredential} ->
    Just $ Aftermarket.genBidderId bidderCredential
  _ -> Nothing

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

-- | Get the buyer's price from an aftermarket datum.
aftermarketDatumBuyerPrice :: AftermarketDatum -> Maybe Aftermarket.Prices
aftermarketDatumBuyerPrice marketDatum = case marketDatum of
  SpotBidDatum Aftermarket.SpotBidDatum{bid} -> Just bid
  ClaimBidDatum Aftermarket.ClaimBidDatum{bid} -> Just bid
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{bid} -> Just bid
  _ -> Nothing

-- | Get the price from an aftermarket datum. It does not matter if it is the sale price or bid.
aftermarketDatumPrice :: AftermarketDatum -> Maybe Aftermarket.Prices
aftermarketDatumPrice marketDatum = case marketDatum of
  SpotDatum Aftermarket.SpotDatum{salePrice} -> Just salePrice
  AuctionDatum Aftermarket.AuctionDatum{startingPrice} -> Just startingPrice
  ClaimBidDatum Aftermarket.ClaimBidDatum{bid} -> Just bid
  SpotBidDatum Aftermarket.SpotBidDatum{bid} -> Just bid
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{bid} -> Just bid
  _ -> Nothing

-- | Get the seller's payment address from an aftermarket datum.
aftermarketDatumSellerAddress :: AftermarketDatum -> Maybe Address
aftermarketDatumSellerAddress marketDatum = case marketDatum of
  SpotDatum Aftermarket.SpotDatum{paymentAddress} -> Just paymentAddress
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{paymentAddress} -> Just paymentAddress
  _ -> Nothing

-- | Get the seller's deposit from an aftermarket datum.
aftermarketDatumSellerDeposit :: AftermarketDatum -> Maybe Integer
aftermarketDatumSellerDeposit marketDatum = case marketDatum of
  SpotDatum Aftermarket.SpotDatum{saleDeposit} -> Just saleDeposit
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{sellerDeposit} -> Just sellerDeposit
  _ -> Nothing

-- | Get the seller's deposit from an aftermarket datum.
aftermarketDatumBuyerDeposit :: AftermarketDatum -> Maybe Integer
aftermarketDatumBuyerDeposit marketDatum = case marketDatum of
  SpotBidDatum Aftermarket.SpotBidDatum{bidDeposit} -> Just bidDeposit
  AcceptedBidDatum Aftermarket.AcceptedBidDatum{bidDeposit} -> Just bidDeposit
  ClaimBidDatum Aftermarket.ClaimBidDatum{bidDeposit} -> Just bidDeposit
  _ -> Nothing

isBidDatum :: AftermarketDatum -> Bool
isBidDatum (SpotBidDatum _) = True
isBidDatum (ClaimBidDatum _) = True
isBidDatum (AcceptedBidDatum _) = True
isBidDatum _ = False

isSellerDatum :: AftermarketDatum -> Bool
isSellerDatum (SpotDatum _) = True
isSellerDatum (AuctionDatum _) = True
isSellerDatum _ = False

bidDatumBidExpiration :: AftermarketDatum -> Maybe PlutusTime
bidDatumBidExpiration (ClaimBidDatum Aftermarket.ClaimBidDatum{bidExpiration}) = bidExpiration
bidDatumBidExpiration _ = Nothing

bidDatumExpiration :: AftermarketDatum -> Maybe PlutusTime
bidDatumExpiration (ClaimBidDatum Aftermarket.ClaimBidDatum{bidExpiration}) = bidExpiration
bidDatumExpiration (AcceptedBidDatum Aftermarket.AcceptedBidDatum{claimExpiration}) = Just claimExpiration
bidDatumExpiration _ = Nothing

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

-- | Get the buyer's price from an AftermarketUTxO.
aftermarketUTxOBuyerPrice :: AftermarketUTxO -> Maybe Aftermarket.Prices
aftermarketUTxOBuyerPrice AftermarketUTxO{marketDatum} = 
  marketDatum >>= aftermarketDatumBuyerPrice

-- | Get the price from an AftermarketUTxO. It does not matter whether it is the sale price or bid.
aftermarketUTxOPrice :: AftermarketUTxO -> Maybe Aftermarket.Prices
aftermarketUTxOPrice AftermarketUTxO{marketDatum} = 
  marketDatum >>= aftermarketDatumPrice

-- | Get the seller's payment address from an AftermarketUTxO.
aftermarketUTxOSellerAddress :: AftermarketUTxO -> Maybe Address
aftermarketUTxOSellerAddress AftermarketUTxO{marketDatum} = 
  marketDatum >>= aftermarketDatumSellerAddress

-- | Get the seller's deposit from an AftermarketUTxO.
aftermarketUTxOSellerDeposit :: AftermarketUTxO -> Maybe Integer
aftermarketUTxOSellerDeposit AftermarketUTxO{marketDatum} = 
  marketDatum >>= aftermarketDatumSellerDeposit

-- | Get the bid's expiration from an AftermarketUTxO. 
aftermarketUTxOBidExpiration :: AftermarketUTxO -> Maybe PlutusTime
aftermarketUTxOBidExpiration AftermarketUTxO{marketDatum} = marketDatum >>= bidDatumBidExpiration

-- | Get the bid's expiration from an AftermarketUTxO. This is the bidExpiration for ClaimBid UTxOs
-- and claimExpiration for AcceptedBid UTxOs.
aftermarketUTxOExpiration :: AftermarketUTxO -> Maybe PlutusTime
aftermarketUTxOExpiration AftermarketUTxO{marketDatum} = marketDatum >>= bidDatumExpiration

-- | Get the buyer's deposit from an AftermarketUTxO.
aftermarketUTxOBuyerDeposit :: AftermarketUTxO -> Maybe Integer
aftermarketUTxOBuyerDeposit AftermarketUTxO{marketDatum} = 
  marketDatum >>= aftermarketDatumBuyerDeposit

-- | Whether the UTxO is a seller UTxOs.
isSellerUTxO :: AftermarketUTxO -> Bool
isSellerUTxO AftermarketUTxO{marketDatum} = maybe False isSellerDatum marketDatum

-- | Whether the UTxO is a bidder UTxOs.
isBidderUTxO :: AftermarketUTxO -> Bool
isBidderUTxO AftermarketUTxO{marketDatum} = maybe False isBidDatum marketDatum

claimBidHasExpired :: POSIXTime -> AftermarketUTxO -> Bool
claimBidHasExpired currentTime AftermarketUTxO{marketDatum} = case marketDatum of
  Just (ClaimBidDatum Aftermarket.ClaimBidDatum{bidExpiration}) ->
    maybe False (<= toPlutusTime currentTime) bidExpiration
  _ -> False

claimPeriodHasExpired :: POSIXTime -> AftermarketUTxO -> Bool
claimPeriodHasExpired currentTime AftermarketUTxO{marketDatum} = case marketDatum of
  Just (AcceptedBidDatum Aftermarket.AcceptedBidDatum{claimExpiration}) ->
    claimExpiration <= toPlutusTime currentTime
  _ -> False

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

instance Notify MarketWallet where
  notify currentTime oldState newState
    | msg /= [] =
        Just $ Notification
          { notificationType = AftermarketNotification
          , alias = oldState ^. #alias
          , message = unlines msg
          , markedAsRead = False
          }
    | otherwise = Nothing
    where
      msg :: [Text]
      msg = filter (/= "")
        [ spotMsg
        , auctionMsg
        , sellerBidMsg
        , buyerBidMsg
        , sellerAcceptedBidMsg
        , buyerAcceptedBidMsg
        ]

      spotOnly :: [AftermarketUTxO] -> [TxOutRef]
      spotOnly = sort
              . map (view #utxoRef) 
              . filter (isJust . preview (#marketDatum % _Just % _SpotDatum))

      auctionOnly :: [AftermarketUTxO] -> [TxOutRef]
      auctionOnly = sort
                  . map (view #utxoRef) 
                  . filter (isJust . preview (#marketDatum % _Just % _AuctionDatum))

      spotBidOnly :: [AftermarketUTxO] -> [TxOutRef]
      spotBidOnly = sort
                  . map (view #utxoRef) 
                  . filter (isJust . preview (#marketDatum % _Just % _SpotBidDatum))

      claimBidOnly :: [AftermarketUTxO] -> [TxOutRef]
      claimBidOnly = sort
                   . map (view #utxoRef) 
                   . filter (isJust . preview (#marketDatum % _Just % _ClaimBidDatum))

      acceptedBidOnly :: [AftermarketUTxO] -> [TxOutRef]
      acceptedBidOnly = sort
                      . map (view #utxoRef) 
                      . filter (isJust . preview (#marketDatum % _Just % _AcceptedBidDatum))

      spotMsg :: Text
      spotMsg
        | spotOnly (oldState ^. #utxos) /= spotOnly (newState ^. #utxos) = 
            "Spot sale statuses have changed."
        | otherwise = ""

      auctionMsg :: Text
      auctionMsg
        | auctionOnly (oldState ^. #utxos) /= auctionOnly (newState ^. #utxos) = 
            "Auction sale statuses have changed."
        | otherwise = ""

      sellerBidMsg :: Text
      sellerBidMsg
        | spotBidOnly (oldState ^. #utxos) /= spotBidOnly (newState ^. #utxos) = 
            "Bids from potential buyers have changed."
        | claimBidOnly (oldState ^. #utxos) /= claimBidOnly (newState ^. #utxos) = 
            "Bids from potential buyers have changed."
        | otherwise = ""

      buyerBidMsg :: Text
      buyerBidMsg
        | spotBidOnly (oldState ^. #bidUTxOs) /= spotBidOnly (newState ^. #bidUTxOs) = 
            "Bids to to sellers have changed."
        | claimBidOnly (oldState ^. #bidUTxOs) /= claimBidOnly (newState ^. #bidUTxOs) = 
            mconcat $ intersperse "\n" $ filter (/="")
              [ "Bids to to sellers have changed."
              , if any (claimBidHasExpired currentTime) $ newState ^. #bidUTxOs
                then "Some Claim Bids have expired."
                else ""
              ]
        | otherwise =
            if any (claimBidHasExpired currentTime) $ newState ^. #bidUTxOs
            then "Some Claim Bids have expired."
            else ""

      sellerAcceptedBidMsg :: Text
      sellerAcceptedBidMsg
        | acceptedBidOnly (oldState ^. #utxos) /= acceptedBidOnly (newState ^. #utxos) = 
            mconcat $ intersperse "\n" $ filter (/="")
              [ "Accepted Bid statuses have changed."
              , if any (claimPeriodHasExpired currentTime) $ newState ^. #utxos
                then "Some Accepted Bid NFTs are reclaimable!"
                else ""
              ]
        | otherwise =
            if any (claimPeriodHasExpired currentTime) $ newState ^. #utxos
            then "Some Accepted Bid NFTs are reclaimable!"
            else ""

      buyerAcceptedBidMsg :: Text
      buyerAcceptedBidMsg
        | acceptedBidOnly (newState ^. #bidUTxOs) /= [] = "Accepted Bids available to claim."
        | otherwise = ""

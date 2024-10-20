{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.BidCreation where

import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time

import P2PWallet.Data.Core.AssetMaps
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Data.DeFi.CardanoLoans qualified as Loans
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Bid Creation
-------------------------------------------------
-- | Information for a new claim bid.
data BidCreation = BidCreation
  -- | The aftermarket wallet to use for the bid.
  { marketWallet :: MarketWallet
  -- | Whether the bid is a spot bid or a claim bid.
  , isSpotBid :: Bool
  -- | The seller's market address.
  , sellerAddress :: PaymentAddress
  -- | The address the payment must go to.
  , paymentWallet :: PaymentWallet
  -- | The nfts for sale in this batch.
  , nfts :: [NativeAsset]
  -- | The required deposit for the bid UTxO.
  , deposit :: Lovelace
  -- | The bid. This can include ada. 
  , bid :: [NativeAsset]
  -- | The time when the bid will expire. 
  , bidExpiration :: Maybe POSIXTime
  -- | The time when you will claim the NFTs by. 
  , claimExpiration :: POSIXTime
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the seller credential.
  , alias :: Text
  -- | The current time.
  , currentTime :: PlutusTime
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''BidCreation

-- This instance is used for the change calculation.
instance AssetBalancesForChange (a,BidCreation) where
  assetBalancesForChange xs =
    ( negate $ sum $ for xs $ \(_,BidCreation{..}) -> sum
        [ deposit
        , if isSpotBid
          then maybe 0 (Lovelace . view #quantity) $ find ((=="") . view #policyId) bid
          else 0
        ]
    , flip concatMap xs $ \(_,BidCreation{bid,isSpotBid}) -> 
        if isSpotBid 
        then map (over #quantity negate) $ filter ((/="") . view #policyId) bid 
        else []
    )

-------------------------------------------------
-- New Bid Creation
-------------------------------------------------
-- | Information for a new claim bid.
data NewBidCreation = NewBidCreation
  -- | The aftermarket wallet to use for the bid.
  { marketWallet :: MarketWallet
  -- | Whether the bid is a spot bid or a claim bid.
  , isSpotBid :: Bool
  -- | The seller's market address.
  , sellerAddress :: PaymentAddress
  -- | The address the payment must go to.
  , paymentWallet :: PaymentWallet
  -- | The nfts for sale in this batch.
  , nfts :: [NativeAsset]
  -- | The bid. This can include ada. The assets are assumed to be separated by newlines.
  , bid :: Text
  -- | The time when the bid will expire. 
  , bidExpiration :: Text
  -- | The time when you will claim the NFTs by. 
  , claimExpiration :: Text
  -- | Which network the asks are for. This is used internally to figure out which reference
  -- scripts to use.
  , network :: Network
  -- | The alias for the seller credential.
  , alias :: Text
  -- | Show the NFTs list to enable removing NFTs.
  , showNfts :: Bool
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewBidCreation

instance Default NewBidCreation where
  def = NewBidCreation
    { marketWallet = def
    , isSpotBid = False
    , sellerAddress = ""
    , paymentWallet = def
    , nfts = []
    , bid = ""
    , bidExpiration = ""
    , claimExpiration = ""
    , network = def
    , alias = ""
    , showNfts = False
    }

-- | Create a fresh `NewBidCreation`.
aftermarketUTxOToNewBidCreation 
  :: POSIXTime
  -> TimeZone
  -> ReverseTickerMap
  -> Network 
  -> MarketWallet 
  -> PaymentWallet 
  -> AftermarketUTxO
  -> NewBidCreation
aftermarketUTxOToNewBidCreation currentTime timeZone reverseTickerMap network marketWallet paymentWallet utxo =
  def & #network .~ network
      & #marketWallet .~ marketWallet
      & #alias .~ marketWallet ^. #alias
      & #paymentWallet .~ paymentWallet
      & #nfts .~ nfts
      & #sellerAddress .~ utxo ^. #marketAddress
      & #bidExpiration .~ 
          -- The default is 5 days from now.
          showDate timeZone (currentTime + convertDaysToPosixPeriod 5)
      & #claimExpiration .~ 
          -- The default is 10 days from now.
          showDate timeZone (currentTime + convertDaysToPosixPeriod 10)
      -- Loan keys cannot use spot bids.
      & #isSpotBid .~ (policyId /= Loans.activeBeaconCurrencySymbol)
      & #bid .~ mconcat (intersperse "\n" $ map (showAssetBalance True reverseTickerMap) prices)
  where
    (policyId,names) = fromMaybe ("",[]) $ aftermarketUTxONfts utxo
    nfts = map (\name -> mkNativeAsset policyId name & #quantity .~ 1) names
    prices = maybe [] (map toNativeAsset . view #unPrices) $ aftermarketUTxOPrice utxo

-------------------------------------------------
-- NewBidCreation <--> BidCreation
-------------------------------------------------
-- | Verify the user info for the new sale sale.
verifyNewBidCreation 
  :: POSIXTime
  -> TimeZone
  -> TickerMap 
  -> NewBidCreation 
  -> Either Text BidCreation
verifyNewBidCreation currentTime timeZone tickerMap NewBidCreation{..} = do
    -- Verify the bid. No fingerprints allowed.
    verifiedPrice <- forM (lines bid) $ \line -> 
      first (const $ parseErrorMsg line) $ parseNativeAssets tickerMap mempty line

    verifiedBidExpiration <- 
      if bidExpiration == "" then return Nothing else Just <$> parseDate timeZone bidExpiration

    verifiedClaimExpiration <- parseDate timeZone claimExpiration

    whenJust verifiedBidExpiration $ \bidExpr ->
      when (bidExpr <= currentTime) $
        Left "The bid expiration must be in the future."

    when (verifiedClaimExpiration <= currentTime) $
      Left "The claim expiration must be in the future."

    return $ BidCreation
      { network = network
      , alias = alias
      , marketWallet = marketWallet
      , paymentWallet = paymentWallet
      , isSpotBid = isSpotBid
      , currentTime = toPlutusTime currentTime
      , nfts = nfts
      , sellerAddress = sellerAddress
      , bid = sumNativeAssets verifiedPrice -- combine native asset quantities
      , bidExpiration = verifiedBidExpiration
      , claimExpiration = verifiedClaimExpiration
      , deposit = 
          -- This will be overridden later. It is needed to represent the proper amount of bytes 
          -- for the fee field since it will impact the final fee calculated. This is necessary
          -- since the deposit amount must also go in the sale datum.
          9_999_999 
      }

  where
    -- A custom error message is used since fingerprints are not allowed.
    parseErrorMsg :: Text -> Text
    parseErrorMsg x = unlines
      [ "Invalid native asset entry. Entries must be separated by newlines, and be one of:"
      , "'# policy_id.asset_name'"
      , "'# ticker'"
      , "'# ADA'"
      , ""
      , "Could not parse: '" <> x <> "'"
      , ""
      , "If using a ticker, make sure it is in the Ticker Registry."
      ]

toNewBidCreation :: TimeZone -> ReverseTickerMap -> BidCreation -> NewBidCreation
toNewBidCreation timeZone reverseTickerMap BidCreation{..} = NewBidCreation
  { network = network
  , alias = alias
  , marketWallet = marketWallet
  , paymentWallet = paymentWallet
  , isSpotBid = isSpotBid
  , sellerAddress = sellerAddress
  , nfts = nfts
  , bid = 
      mconcat $ intersperse "\n" $ map (showAssetBalance True reverseTickerMap) bid
  , bidExpiration = maybe "" (showDate timeZone) bidExpiration
  , claimExpiration = showDate timeZone claimExpiration
  , showNfts = False
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
showDate :: TimeZone -> POSIXTime -> Text
showDate timeZone t = toText $ Time.formatTime locale formatter localTime
  where
    locale = Time.defaultTimeLocale & #knownTimeZones %~ flip snoc timeZone
    -- Midnight is technically the next day so must subtract 1 second.
    utcTime = Time.posixSecondsToUTCTime $ t - 1
    localTime = Time.utcToLocalTime timeZone utcTime
    formatter = "%m/%d/%y"

-- The expiration is the end of the specified day (midnight).
parseDate :: TimeZone -> Text -> Either Text POSIXTime
parseDate timeZone t = 
  let locale = Time.defaultTimeLocale & #knownTimeZones %~ flip snoc timeZone
      dayToPosixTime = localTimeToPosixTime timeZone . endOfDay
   in maybeToRight ("Could not parse date: " <> t) $ 
        dayToPosixTime <$> (Time.parseTimeM True locale "%m/%d/%y" $ toString t :: Maybe Day)

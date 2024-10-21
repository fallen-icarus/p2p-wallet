{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.DeFi.CardanoAftermarket
  ( -- * Datums
    SpotDatum(..)
  , AuctionDatum(..)
  , SpotBidDatum(..)
  , ClaimBidDatum(..)
  , AcceptedBidDatum(..)
  , PaymentDatum(..)

    -- * Redeemers
  , MarketRedeemer(..)
  , MarketObserverRedeemer(..)
  , BeaconsRedeemer(..)
  , _CreateCloseOrUpdateMarketUTxOs

    -- * Contracts
  , proxyScript
  , proxyScriptHash
  , proxyScriptSize
  , aftermarketScript
  , aftermarketScriptHash
  , aftermarketScriptSize
  , aftermarketObserverScript
  , aftermarketObserverScriptHash
  , aftermarketObserverScriptSize
  , beaconScript
  , beaconScriptHash
  , beaconCurrencySymbol
  , beaconScriptSize

    -- * Beacon Names
  , genPolicyBeacon
  , genBidderId
  , spotBeaconName
  , auctionBeaconName
  , bidBeaconName

    -- * Creating Datums
  , createSpotDatum
  , createAuctionDatum
  , createSpotBidDatum
  , createClaimBidDatum
  , createAcceptedBidDatum

    -- * Protocol Addresses
  , genProxyAddress
  , isProxyAddress
  , isValidPaymentAddress
  , genSellerAddress
  , isSellerAddress
  , beaconStakeAddress
  , observerStakeAddress

    -- * Reference Scripts Output References
  , getScriptRef

    -- * Re-exports
  , module P2PWallet.Data.DeFi.CardanoAftermarket.Internal
  ) where

import qualified Data.Map.Strict as Map
import Data.Aeson

import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusLedgerApi.V2 as PV2

import CardanoAftermarket.Blueprints

import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Data.Core.Internal.Bech32Address
import P2PWallet.Data.Core.Internal.Network
import P2PWallet.Data.DeFi.CardanoAftermarket.Internal
import P2PWallet.Data.DeFi.CardanoLoans 
  ( proxyScript
  , proxyScriptHash
  , proxyScriptSize
  , genProxyAddress
  , isProxyAddress
  , proxyScriptTestnetRef
  )
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Spot Datum
-------------------------------------------------
-- | The datum for a Spot UTxO sale for NFTs.
data SpotDatum = SpotDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
  -- | The policy id for the NFTs being offered.
  , nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The address where the proceeds must go upon purchase of the Spot UTxO.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , saleDeposit :: Integer
  -- | The price for this batch, denominated in the specified assets. This is the price for the
  -- entire batch of NFTs. The payment output must contain *all* of the required assets, and their
  -- specified amounts.
  , salePrice :: Prices
  } deriving (Show,Eq)

PlutusTx.makeIsDataIndexed ''SpotDatum [('SpotDatum,0)]
makeFieldLabelsNoPrefix ''SpotDatum

instance Default SpotDatum where
  def = SpotDatum
    { beaconId = ""
    , aftermarketObserverHash = ""
    , nftPolicyId = ""
    , nftNames = []
    , saleDeposit = 0
    , salePrice = Prices []
    , paymentAddress = Address (PubKeyCredential "") Nothing
    }

instance ToJSON SpotDatum where
  toJSON SpotDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "nft_names" .= nftNames
           , "payment_address" .= paymentAddress
           , "sale_deposit" .= saleDeposit
           , "sale_price" .= salePrice
           ]

instance FromJSON SpotDatum where
  parseJSON = withObject "SpotDatum" $ \o ->
    SpotDatum
      <$> o .: "beacon_id"
      <*> o .: "aftermarket_observer_hash"
      <*> o .: "nft_policy_id"
      <*> o .: "nft_names"
      <*> o .: "payment_address"
      <*> o .: "sale_deposit"
      <*> o .: "sale_price"

-------------------------------------------------
-- Auction Datum
-------------------------------------------------
-- | The datum for an Auction UTxO for NFTs.
data AuctionDatum = AuctionDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
  -- | The policy id for the NFTs being auctioned.
  , nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The desired starting price. This is only used to broadcast the auctioner's desired value.
  -- Bidders are able to create "counter-bids" with different assets and/or different nft names.
  -- For example, the bidder can make a bid for just one of the NFTs in the batch.
  , startingPrice :: Prices
  } deriving (Show,Eq)

PlutusTx.makeIsDataIndexed ''AuctionDatum [('AuctionDatum,1)]
makeFieldLabelsNoPrefix ''AuctionDatum

instance Default AuctionDatum where
  def = AuctionDatum
    { beaconId = ""
    , aftermarketObserverHash = ""
    , nftPolicyId = ""
    , nftNames = []
    , startingPrice = Prices []
    }

instance ToJSON AuctionDatum where
  toJSON AuctionDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "nft_names" .= nftNames
           , "starting_price" .= startingPrice
           ]

instance FromJSON AuctionDatum where
  parseJSON = withObject "AuctionDatum" $ \o ->
    AuctionDatum
      <$> o .: "beacon_id"
      <*> o .: "aftermarket_observer_hash"
      <*> o .: "nft_policy_id"
      <*> o .: "nft_names"
      <*> o .: "starting_price"

-------------------------------------------------
-- SpotBid Datum
-------------------------------------------------
-- | The datum for a SpotBid UTxO for NFTs. These can be immediately claimed by the seller as long
-- as they send the NFTs to the required address.
data SpotBidDatum = SpotBidDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
  -- | The policy id for the NFTs being bid on.
  , nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. 
  , bidderCredential :: Credential
  -- | The token names of the NFTs being bid on.
  , nftNames :: [TokenName]
  -- | The address where the NFTs must go upon accepting the bid.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , bidDeposit :: Integer
  -- | The actual bid.
  , bid :: Prices
  } deriving (Show,Eq)

PlutusTx.makeIsDataIndexed ''SpotBidDatum [('SpotBidDatum,2)]
makeFieldLabelsNoPrefix ''SpotBidDatum

instance Default SpotBidDatum where
  def = SpotBidDatum
    { beaconId = ""
    , aftermarketObserverHash = ""
    , nftPolicyId = ""
    , nftNames = []
    , bidderCredential = PubKeyCredential ""
    , paymentAddress = Address (PubKeyCredential "") Nothing
    , bidDeposit = 0
    , bid = Prices []
    }

instance ToJSON SpotBidDatum where
  toJSON SpotBidDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "bidder_credential" .= bidderCredential
           , "nft_names" .= nftNames
           , "payment_address" .= paymentAddress
           , "bid_deposit" .= bidDeposit
           , "bid" .= bid
           ]

instance FromJSON SpotBidDatum where
  parseJSON = withObject "SpotBidDatum" $ \o ->
    SpotBidDatum
      <$> o .: "beacon_id"
      <*> o .: "aftermarket_observer_hash"
      <*> o .: "nft_policy_id"
      <*> o .: "bidder_credential"
      <*> o .: "nft_names"
      <*> o .: "payment_address"
      <*> o .: "bid_deposit"
      <*> o .: "bid"

-------------------------------------------------
-- ClaimBid Datum
-------------------------------------------------
-- | The datum for a ClaimBid UTxO for NFTs. These must be evolved into an AcceptedBid UTxO so that
-- the bidder can come claim the NFTs. This step is required for certain compositions where the
-- buyer needs to update UTxOs for the primary market in the same transaction.
data ClaimBidDatum = ClaimBidDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
  -- | The policy id for the NFTs being bid on.
  , nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. 
  , bidderCredential :: Credential
  -- | The token names of the NFTs being bid on.
  , nftNames :: [TokenName]
  -- | The amount of ada used for the minUTxOValue. If the ClaimBid is accepted and the bidder walks
  -- away, this deposit will be taken by the seller. The bidder can increase the deposit to make
  -- their ClaimBid more enticing for sellers.
  , bidDeposit :: Integer
  -- | The actual bid. This field tells the seller how much you are promising to pay for the NFTs.
  , bid :: Prices
  -- | The time this bid expires.
  , bidExpiration :: Maybe PlutusTime
  -- | The time, after which, the seller can reclaim the NFTs + the bidder's deposit.
  , claimExpiration :: PlutusTime
  } deriving (Show,Eq)

PlutusTx.makeIsDataIndexed ''ClaimBidDatum [('ClaimBidDatum,3)]
makeFieldLabelsNoPrefix ''ClaimBidDatum

instance Default ClaimBidDatum where
  def = ClaimBidDatum
    { beaconId = ""
    , aftermarketObserverHash = ""
    , nftPolicyId = ""
    , nftNames = []
    , bidderCredential = PubKeyCredential ""
    , bidDeposit = 0
    , bid = Prices []
    , bidExpiration = Nothing
    , claimExpiration = 0
    }

instance ToJSON ClaimBidDatum where
  toJSON ClaimBidDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "bidder_credential" .= bidderCredential
           , "nft_names" .= nftNames
           , "bid_deposit" .= bidDeposit
           , "bid" .= bid
           , "bid_expiration" .= bidExpiration
           , "claim_expiration" .= claimExpiration
           ]

instance FromJSON ClaimBidDatum where
  parseJSON = withObject "ClaimBidDatum" $ \o ->
    ClaimBidDatum
      <$> o .: "beacon_id"
      <*> o .: "aftermarket_observer_hash"
      <*> o .: "nft_policy_id"
      <*> o .: "bidder_credential"
      <*> o .: "nft_names"
      <*> o .: "bid_deposit"
      <*> o .: "bid"
      <*> o .: "bid_expiration"
      <*> o .: "claim_expiration"

-------------------------------------------------
-- AcceptedBid Datum
-------------------------------------------------
-- | The datum for an AcceptedBid UTxO for NFTs. It contains the required NFTs and is waiting to be
-- claimed by the bidder. The bidder must pay the seller the required bid amount + the seller's
-- deposit to actually claim the NFTs. If the bidder does not claim them, the the seller can
-- re-claim the NFTs after the claim expiration has passed. The seller will also claim the bidder's
-- deposit in this context as compensation.
data AcceptedBidDatum = AcceptedBidDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
  -- | The policy id for the NFTs being bid on.
  , nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. Only this credential can claim the
  -- NFTs.
  , bidderCredential :: Credential
  -- | The token names of the NFTs being bid on.
  , nftNames :: [TokenName]
  -- | The amount of ada used for the minUTxOValue. If the bidder walks away, this deposit will 
  -- be taken by the seller. 
  , bidDeposit :: Integer
  -- | The amount the seller paid for the minUTxOValue, over what the bidder paid. This will be
  -- returned to the seller.
  , sellerDeposit :: Integer
  -- | The actual bid. This field tells the seller how much you are promising to pay for the NFTs.
  , bid :: Prices
  -- | The address where the bid payment must go upon claiming the NFTs.
  , paymentAddress :: Address
  -- | The time, after which, the seller can reclaim the NFTs + the bidder's deposit.
  , claimExpiration :: PlutusTime
  } deriving (Show,Eq)

PlutusTx.makeIsDataIndexed ''AcceptedBidDatum [('AcceptedBidDatum,4)]
makeFieldLabelsNoPrefix ''AcceptedBidDatum

instance Default AcceptedBidDatum where
  def = AcceptedBidDatum
    { beaconId = ""
    , aftermarketObserverHash = ""
    , nftPolicyId = ""
    , nftNames = []
    , bidderCredential = PubKeyCredential ""
    , bidDeposit = 0
    , sellerDeposit = 0
    , bid = Prices []
    , paymentAddress = Address (PubKeyCredential "") Nothing
    , claimExpiration = 0
    }

instance ToJSON AcceptedBidDatum where
  toJSON AcceptedBidDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "bidder_credential" .= bidderCredential
           , "nft_names" .= nftNames
           , "bid_deposit" .= bidDeposit
           , "seller_deposit" .= sellerDeposit
           , "bid" .= bid
           , "payment_address" .= paymentAddress
           , "claim_expiration" .= claimExpiration
           ]

instance FromJSON AcceptedBidDatum where
  parseJSON = withObject "AcceptedBidDatum" $ \o ->
    AcceptedBidDatum
      <$> o .: "beacon_id"
      <*> o .: "aftermarket_observer_hash"
      <*> o .: "nft_policy_id"
      <*> o .: "bidder_credential"
      <*> o .: "nft_names"
      <*> o .: "bid_deposit"
      <*> o .: "seller_deposit"
      <*> o .: "bid"
      <*> o .: "payment_address"
      <*> o .: "claim_expiration"

-------------------------------------------------
-- Payment Datum
-------------------------------------------------
newtype PaymentDatum = PaymentDatum (CurrencySymbol,TxOutRef)
  deriving (Show,Eq)

instance ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,ref)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData ref]

instance FromData PaymentDatum where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,ref])) =
    fmap PaymentDatum . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData ref
  fromBuiltinData _ = Nothing

-------------------------------------------------
-- On-Chain Redeemers
-------------------------------------------------
data MarketRedeemer
  -- | Close or update either a Spot UTxO or an Auction UTxO.
  = CloseOrUpdateSellerUTxO
  -- | Close or update a Bid UTxO.
  | CloseOrUpdateBidderUTxO
  -- | Purchase a Spot UTxO.
  | PurchaseSpot
  -- | Accept a SpotBid UTxO, and close the associated Auction UTxO.
  | AcceptSpotBid
  -- | Accept a ClaimBid UTxO, and close the associated Auction UTxO. This will create an
  -- AcceptedBid UTxO with the NFTs for the bidder to come claim. The payment address is the address
  -- the seller would like the payment sent to.
  | AcceptClaimBid { sellerDeposit :: Integer, paymentAddress :: Address }
  -- | Claim an AcceptedBid UTxO that belongs to you (ie, you have the bidder credential).
  | ClaimAcceptedBid
  -- | Reclaim the NFTs from an AcceptedBid UTxO after the bidder has failed to claim them. Take the
  -- bidder's deposit as compensation.
  | UnlockUnclaimedAcceptedBid
  deriving (Generic,Show)

makeFieldLabelsNoPrefix ''MarketRedeemer

instance Display MarketRedeemer where
  display CloseOrUpdateSellerUTxO = "Closed/Updated Seller UTxO"
  display CloseOrUpdateBidderUTxO = "Closed/Updated Buyer UTxO"
  display PurchaseSpot = "Spot Purchased"
  display AcceptSpotBid = "SpotBid Accepted"
  display (AcceptClaimBid _ _) = "ClaimBid Accepted"
  display ClaimAcceptedBid = "AcceptedBid Claimed"
  display UnlockUnclaimedAcceptedBid = "Unclaimed AcceptedBid Confiscated"

data MarketObserverRedeemer
  -- | Observe a market payment/acceptance transaction. 
  = ObserveAftermarket { beaconId :: BeaconId }
  -- | Register the script.
  | RegisterAftermarketObserverScript
  deriving (Generic,Show)

data BeaconsRedeemer
  -- | Create, close, or update some market UTxOs (1 or more). 
  = CreateCloseOrUpdateMarketUTxOs
  -- | Burn any beacons. 
  | BurnBeacons
  -- | Register the script.
  | RegisterBeaconsScript
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''MarketRedeemer
PlutusTx.unstableMakeIsData ''MarketObserverRedeemer
PlutusTx.unstableMakeIsData ''BeaconsRedeemer
makePrisms ''BeaconsRedeemer

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Convert an NFT's policy id to a PolicyBeacon.
genPolicyBeacon :: PV2.CurrencySymbol -> PolicyBeacon
genPolicyBeacon (PV2.CurrencySymbol currSym) = 
  PolicyBeacon $ PV2.TokenName $ PlutusTx.sha2_256 $ unsafeToBuiltinByteString "00" <> currSym

-- | Convert a credential to a BidderId.
genBidderId :: PV2.Credential -> BidderId
genBidderId (PV2.PubKeyCredential (PV2.PubKeyHash pkh)) = 
  BidderId $ TokenName $ unsafeToBuiltinByteString "01" <> pkh
genBidderId (PV2.ScriptCredential (PV2.ScriptHash sh)) = 
  BidderId $ TokenName $ unsafeToBuiltinByteString "01" <> sh

spotBeaconName :: TokenName
spotBeaconName = "Spot"

auctionBeaconName :: TokenName
auctionBeaconName = "Auction"

bidBeaconName :: TokenName
bidBeaconName = "Bid"

-------------------------------------------------
-- Contracts
-------------------------------------------------
aftermarketScript :: SerialisedScript
aftermarketScript = parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.aftermarket_script"

aftermarketScriptHash :: PV2.ScriptHash
aftermarketScriptHash = hashScript aftermarketScript

aftermarketScriptSize :: Integer
aftermarketScriptSize = getScriptSize aftermarketScript

aftermarketObserverScript :: SerialisedScript
aftermarketObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.aftermarket_observer_script")
    [ PV2.toData proxyScriptHash
    , PV2.toData aftermarketScriptHash
    ]

aftermarketObserverScriptHash :: PV2.ScriptHash
aftermarketObserverScriptHash = hashScript aftermarketObserverScript

aftermarketObserverScriptSize :: Integer
aftermarketObserverScriptSize = getScriptSize aftermarketObserverScript

beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.beacon_script")
    [ PV2.toData proxyScriptHash
    , PV2.toData aftermarketScriptHash
    , PV2.toData aftermarketObserverScriptHash
    ]

beaconScriptHash :: PV2.ScriptHash
beaconScriptHash = hashScript beaconScript

beaconCurrencySymbol :: PV2.CurrencySymbol
beaconCurrencySymbol = scriptHashToPolicyId beaconScriptHash

beaconScriptSize :: Integer
beaconScriptSize = getScriptSize beaconScript

-------------------------------------------------
-- Creating Datums
-------------------------------------------------
-- | Create a new SpotDatum.
createSpotDatum 
  :: [NativeAsset] -- ^ NFTs
  -> PaymentAddress -- ^ Address to be used for payment.
  -> Lovelace -- ^ Deposit.
  -> [NativeAsset] -- ^ Sale price.
  -> SpotDatum
createSpotDatum nfts paymentAddr deposit price = SpotDatum
    { beaconId = BeaconId beaconCurrencySymbol
    , aftermarketObserverHash = aftermarketObserverScriptHash
    , nftPolicyId = nftPolicyId
    , nftNames = nftNames
    , saleDeposit = unLovelace deposit
    , salePrice = Prices $ map fromNativeAsset price
    , paymentAddress = address
    }
  where
    address = fromRight (error "createSpotDatum paymentAddr") -- This should never return left.
            $ paymentAddressToPlutusAddress paymentAddr
    nftPolicyId = maybe (error "createSpotDatum nfts") (view #policyId) $ maybeHead nfts
    nftNames = map (view #tokenName) nfts

-- | Create a new AuctionDatum.
createAuctionDatum
  :: [NativeAsset] -- ^ NFTs
  -> [NativeAsset] -- ^ Sale price.
  -> AuctionDatum
createAuctionDatum nfts price = AuctionDatum
    { beaconId = BeaconId beaconCurrencySymbol
    , aftermarketObserverHash = aftermarketObserverScriptHash
    , nftPolicyId = nftPolicyId
    , nftNames = nftNames
    , startingPrice = Prices $ map fromNativeAsset price
    }
  where
    nftPolicyId = maybe (error "createAuctionDatum nfts") (view #policyId) $ maybeHead nfts
    nftNames = map (view #tokenName) nfts

-- | Create a new SpotBidDatum.
createSpotBidDatum 
  :: [NativeAsset] -- ^ NFTs
  -> Lovelace -- ^ Deposit.
  -> PaymentAddress -- ^ The address where the NFTs must go.
  -> [NativeAsset] -- ^ Bid.
  -> Credential -- ^ Bidder's credential.
  -> SpotBidDatum
createSpotBidDatum nfts deposit paymentAddress price bidderCred = SpotBidDatum
    { beaconId = BeaconId beaconCurrencySymbol
    , aftermarketObserverHash = aftermarketObserverScriptHash
    , nftPolicyId = nftPolicyId
    , nftNames = nftNames
    , bidderCredential = bidderCred
    , bidDeposit = unLovelace deposit
    , bid = Prices $ map fromNativeAsset price
    , paymentAddress = address
    }
  where
    nftPolicyId = maybe (error "createSpotBidDatum nfts") (view #policyId) $ maybeHead nfts
    nftNames = map (view #tokenName) nfts
    address = fromRight (error "createSpotBidDatum paymentAddress") -- This should never return left.
            $ paymentAddressToPlutusAddress paymentAddress

-- | Create a new ClaimBidDatum.
createClaimBidDatum 
  :: [NativeAsset] -- ^ NFTs
  -> Lovelace -- ^ Deposit.
  -> [NativeAsset] -- ^ Bid.
  -> Maybe PlutusTime -- ^ Bid expiration.
  -> PlutusTime -- Claim expiration.
  -> Credential -- Bidder's credential.
  -> ClaimBidDatum
createClaimBidDatum nfts deposit price bidExpiration claimExpiration bidderCred = ClaimBidDatum
    { beaconId = BeaconId beaconCurrencySymbol
    , aftermarketObserverHash = aftermarketObserverScriptHash
    , nftPolicyId = nftPolicyId
    , nftNames = nftNames
    , bidderCredential = bidderCred
    , bidDeposit = unLovelace deposit
    , bid = Prices $ map fromNativeAsset price
    , bidExpiration = bidExpiration
    , claimExpiration = claimExpiration
    }
  where
    nftPolicyId = maybe (error "createClaimBidDatum nfts") (view #policyId) $ maybeHead nfts
    nftNames = map (view #tokenName) nfts

-- | Create a new AcceptedBidDatum from a ClaimBidDatum.
createAcceptedBidDatum
  :: ClaimBidDatum
  -> Lovelace -- ^ Deposit.
  -> PaymentAddress
  -> AcceptedBidDatum
createAcceptedBidDatum ClaimBidDatum{..} sellerDeposit sellerPayAddr = AcceptedBidDatum
    { beaconId = BeaconId beaconCurrencySymbol
    , aftermarketObserverHash = aftermarketObserverScriptHash
    , nftPolicyId = nftPolicyId
    , nftNames = nftNames
    , bidderCredential = bidderCredential
    , bidDeposit = bidDeposit
    , sellerDeposit = unLovelace sellerDeposit
    , bid = bid
    , claimExpiration = claimExpiration
    , paymentAddress = address
    }
  where
    address = fromRight (error "createAcceptedBidDatum sellerPayAddr") -- This should never return left.
            $ paymentAddressToPlutusAddress sellerPayAddr

-------------------------------------------------
-- Protocol Addresses
-------------------------------------------------
-- | Check whether the address is a valid payment address.
isValidPaymentAddress :: Address -> Bool
isValidPaymentAddress (Address (PubKeyCredential _) _) = True
isValidPaymentAddress (Address (ScriptCredential cred) mStakeCred) =
  cred == proxyScriptHash && case mStakeCred of
    Just (StakingHash _) -> True
    _ -> False

-- | Create the seller address for the stake credential.
genSellerAddress :: Network -> Maybe Credential -> Either Text PaymentAddress
genSellerAddress network mStakeCred = fmap fst $ plutusToBech32 network $ Address
  { addressCredential = ScriptCredential aftermarketScriptHash
  , addressStakingCredential = StakingHash <$> mStakeCred
  }

-- | Whether this address is a seller address.
isSellerAddress :: PaymentAddress -> Bool
isSellerAddress = either (const False) ((== ScriptCredential aftermarketScriptHash) . addressCredential)
                . paymentAddressToPlutusAddress

-- | The address that must be used for the beacon staking execution.
beaconStakeAddress :: Network -> StakeAddress
beaconStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential beaconScriptHash
      , addressStakingCredential = Just $ StakingHash $ ScriptCredential beaconScriptHash
      }

-- | The address that must be used for the observer staking execution.
observerStakeAddress :: Network -> StakeAddress
observerStakeAddress network = 
  fromMaybe "" $ either (const Nothing) snd $ plutusToBech32 network $ 
    Address
      { addressCredential = ScriptCredential aftermarketObserverScriptHash
      , addressStakingCredential = 
          Just $ StakingHash $ ScriptCredential aftermarketObserverScriptHash
      }

-------------------------------------------------
-- Reference Script UTxOs
-------------------------------------------------
-- The reference scripts are locked at the options address without any staking credential.
-- For testnet, that address is: 
-- addr_test1wrs8a6yhjamxjt8rgaascr2nknr9pmmvett4cfvkmg3gglqrqjydc
--
-- The scripts are deliberately stored with an invalid datum so that they are locked forever.
getScriptRef :: Network -> ScriptHash -> (TxOutRef,Integer)
getScriptRef network scriptHash = (referenceScriptMap Map.! network) Map.! scriptHash

referenceScriptMap :: Map.Map Network (Map.Map ScriptHash (TxOutRef,Integer))
referenceScriptMap = Map.fromList
  [ (Testnet, Map.fromList
        [ (proxyScriptHash, (proxyScriptTestnetRef, proxyScriptSize))
        , (beaconScriptHash, (beaconScriptTestnetRef, beaconScriptSize))
        , (aftermarketScriptHash, (aftermarketScriptTestnetRef, aftermarketScriptSize))
        , (aftermarketObserverScriptHash, (aftermarketObserverScriptTestnetRef, aftermarketObserverScriptSize))
        ])
  ]

beaconScriptTestnetRef :: TxOutRef
beaconScriptTestnetRef = 
  TxOutRef "6c402050892c8cb0e3e54f803d7ae292d6f5f90745b7f76722f7c303c7085d50" 0

aftermarketScriptTestnetRef :: TxOutRef
aftermarketScriptTestnetRef = 
  TxOutRef "e95a73a1e03afdf74b86d10e504b64285f7afdfab7f7021a41054ae4b377ca9f" 0

aftermarketObserverScriptTestnetRef :: TxOutRef
aftermarketObserverScriptTestnetRef = 
  TxOutRef "b6b5bd23fa762b2630dc9dedc10d0bac61d6ffa3617f451df8a8ee31a83c441f" 0

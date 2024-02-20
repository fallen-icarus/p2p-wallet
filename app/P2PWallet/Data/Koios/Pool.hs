{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/pool_info API 
endpoint (the types are the same for mainnet).

-}
module P2PWallet.Data.Koios.Pool where

import Data.Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types (parseFail,Parser,parseEither)
import Data.Maybe (fromJust)

import P2PWallet.Prelude
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.PoolID
import P2PWallet.Data.FilterLang

data PoolStatus
  = RegisteredPool
  | RetiringPool
  | RetiredPool
  deriving (Show,Eq)

instance ToJSON PoolStatus where
  toJSON = toJSON . showPoolStatus

instance FromJSON PoolStatus where
  parseJSON = withText "PoolStatus" (maybe mzero return . readPoolStatus)

readPoolStatus :: Text -> Maybe PoolStatus
readPoolStatus "retiring" = Just RetiringPool
readPoolStatus "registered" = Just RegisteredPool
readPoolStatus "retired" = Just RetiredPool
readPoolStatus _ = Nothing

showPoolStatus :: PoolStatus -> Text
showPoolStatus RetiringPool = "retiring"
showPoolStatus RegisteredPool = "registered"
showPoolStatus RetiredPool = "retired"

data PoolInfo = PoolInfo
  { _name :: Text
  , _ticker :: Text
  , _homepage :: Text
  , _description :: Text
  } deriving (Show,Eq)

instance FromJSON PoolInfo where
  parseJSON = withObject "PoolInfo" $ \o ->
    PoolInfo
      <$> o .: "name"
      <*> o .: "ticker"
      <*> o .: "homepage"
      <*> o .: "description"
      
-- A default instance is usefull for `Maybe PoolInfo`.
instance Default PoolInfo where
  def = PoolInfo
    { _name = "none"
    , _ticker = "none"
    , _homepage = "none"
    , _description = "none"
    }

data Pool = Pool
  { _poolId :: PoolID
  , _margin :: Maybe Decimal
  , _fixedCost :: Maybe Lovelace
  , _pledge :: Maybe Lovelace
  , _info :: Maybe PoolInfo
  , _status :: PoolStatus
  , _retiringEpoch :: Maybe Integer
  , _activeStake :: Maybe Lovelace
  , _sigma :: Maybe Decimal
  , _blockCount :: Maybe Integer
  , _livePledge :: Maybe Lovelace
  , _liveStake :: Maybe Lovelace
  , _liveDelegators :: Maybe Integer
  , _liveSaturation :: Maybe Decimal
  } deriving (Show,Eq)

instance FromJSON Pool where
  parseJSON = withObject "Pool" $ \o ->
    Pool
      <$> o .: "pool_id_bech32"
      <*> o .: "margin"
      <*> o .: "fixed_cost"
      <*> o .: "pledge"
      <*> o .: "meta_json"
      <*> o .: "pool_status"
      <*> o .: "retiring_epoch"
      <*> o .: "active_stake"
      <*> o .: "sigma"
      <*> o .: "block_count"
      <*> o .: "live_pledge"
      <*> o .: "live_stake"
      <*> o .: "live_delegators"
      <*> o .: "live_saturation"

-------------------------------------------------
-- Filtering Pools
-------------------------------------------------
-- | The possible predicates for filtering the list of registered pools. Users supply ADA
-- values so `Lovelace` values must be converted before comparing.
data PoolFilterLang
  = MatchPoolID PoolID
  | MatchMargin (EqualityPredicate Decimal)
  | MatchFixedCost (EqualityPredicate Decimal)
  | MatchPledge (EqualityPredicate Decimal)
  | MatchActiveStake (EqualityPredicate Decimal)
  | MatchSigma (EqualityPredicate Decimal)
  | MatchBlockCount (EqualityPredicate Integer)
  | MatchLivePledge (EqualityPredicate Decimal)
  | MatchLiveStake (EqualityPredicate Decimal)
  | MatchLiveDelegators (EqualityPredicate Integer)
  | MatchLiveSaturation (EqualityPredicate Decimal)
  deriving (Show,Eq)

instance ToJSON PoolFilterLang where
  toJSON (MatchPoolID poolId) = object [ "pool_id" .= toText poolId ]
  toJSON (MatchMargin x) = object [ "margin" .= x ]
  toJSON (MatchFixedCost x) = object [ "fixed_cost" .= x ]
  toJSON (MatchPledge x) = object [ "pledge" .= x ]
  toJSON (MatchActiveStake x) = object [ "active_stake" .= x ]
  toJSON (MatchSigma x) = object [ "sigma" .= x ]
  toJSON (MatchBlockCount x) = object [ "block_count" .= x ]
  toJSON (MatchLivePledge x) = object [ "live_pledge" .= x ]
  toJSON (MatchLiveStake x) = object [ "live_stake" .= x ]
  toJSON (MatchLiveDelegators x) = object [ "live_delegators" .= x ]
  toJSON (MatchLiveSaturation x) = object [ "live_saturation" .= x ]

-- A stepwise parser so that errors can be more user friendly.
instance FromJSON PoolFilterLang where
  parseJSON value = flip (withObject "PoolFilterLang") value $ \o -> do
      let poolIdVal = Aeson.lookup "pool_id" o
          marginVal = Aeson.lookup "margin" o
          fixedCostVal = Aeson.lookup "fixed_cost" o
          pledgeVal = Aeson.lookup "pledge" o
          activeStakeVal = Aeson.lookup "active_stake" o
          sigmaVal = Aeson.lookup "sigma" o
          blockCountVal = Aeson.lookup "block_count" o
          livePledgeVal = Aeson.lookup "live_pledge" o
          liveStakeVal = Aeson.lookup "live_stake" o
          liveDelegatorsVal = Aeson.lookup "live_delegators" o
          liveSaturationVal = Aeson.lookup "live_saturation" o
          unknownKey = maybeHead $ Aeson.keys o
      if isJust poolIdVal then
        either parseFail pure $ parseEither poolIdParser value
      else if isJust marginVal then
        either parseFail pure $ parseEither marginParser value
      else if isJust fixedCostVal then
        either parseFail pure $ parseEither fixedCostParser value
      else if isJust pledgeVal then
        either parseFail pure $ parseEither pledgeParser value
      else if isJust activeStakeVal then
        either parseFail pure $ parseEither activeStakeParser value
      else if isJust sigmaVal then
        either parseFail pure $ parseEither sigmaParser value
      else if isJust blockCountVal then
        either parseFail pure $ parseEither blockCountParser value
      else if isJust livePledgeVal then
        either parseFail pure $ parseEither livePledgeParser value
      else if isJust liveStakeVal then
        either parseFail pure $ parseEither liveStakeParser value
      else if isJust liveDelegatorsVal then
        either parseFail pure $ parseEither liveDelegatorsParser value
      else if isJust liveSaturationVal then
        either parseFail pure $ parseEither liveSaturationParser value
      else if isJust unknownKey then
        parseFail $ "Unknown key: " <> show (fromJust unknownKey)
      else
        -- A catch all that should never occur.
        parseFail $ "Could not parse: " <> toString (showValue value)

    where
      poolIdParser :: Value -> Parser PoolFilterLang
      poolIdParser = withObject "PoolID" $ \o ->
        MatchPoolID <$> o .: "pool_id"

      marginParser :: Value -> Parser PoolFilterLang
      marginParser = withObject "Margin" $ \o ->
        MatchMargin <$> o .: "margin"

      fixedCostParser :: Value -> Parser PoolFilterLang
      fixedCostParser = withObject "FixedCost" $ \o ->
        MatchFixedCost <$> o .: "fixed_cost"

      pledgeParser :: Value -> Parser PoolFilterLang
      pledgeParser = withObject "Pledge" $ \o ->
        MatchPledge <$> o .: "pledge"

      activeStakeParser :: Value -> Parser PoolFilterLang
      activeStakeParser = withObject "ActiveStake" $ \o ->
        MatchActiveStake <$> o .: "active_stake"

      sigmaParser :: Value -> Parser PoolFilterLang
      sigmaParser = withObject "Sigma" $ \o ->
        MatchSigma <$> o .: "sigma"

      blockCountParser :: Value -> Parser PoolFilterLang
      blockCountParser = withObject "BlockCount" $ \o ->
        MatchBlockCount <$> o .: "block_count"

      livePledgeParser :: Value -> Parser PoolFilterLang
      livePledgeParser = withObject "LivePledge" $ \o ->
        MatchLivePledge <$> o .: "live_pledge"

      liveStakeParser :: Value -> Parser PoolFilterLang
      liveStakeParser = withObject "LiveStake" $ \o ->
        MatchLiveStake <$> o .: "live_stake"

      liveDelegatorsParser :: Value -> Parser PoolFilterLang
      liveDelegatorsParser = withObject "LiveDelegators" $ \o ->
        MatchLiveDelegators <$> o .: "live_delegators"

      liveSaturationParser :: Value -> Parser PoolFilterLang
      liveSaturationParser = withObject "live_saturation" $ \o ->
        MatchLiveSaturation <$> o .: "live_saturation"

-- | Check whether a `Pool` satisfies the predicates. Defaults to True for empty filters.
poolCheck :: [FilterLang PoolFilterLang] -> Pool -> Bool
poolCheck ps Pool{..} = all (runFilter check) ps
  where
    -- Improperly formatted pools should just be dropped.
    check :: PoolFilterLang -> Bool
    check p = case p of
      MatchPoolID poolId -> 
        poolId == _poolId
      MatchMargin eqPred -> 
        maybe False (runEqualityPredicate eqPred) _margin
      MatchFixedCost eqPred -> 
        maybe False (runEqualityPredicate eqPred) $ fmap (unADA . toADA) _fixedCost
      MatchPledge eqPred -> 
        maybe False (runEqualityPredicate eqPred) $ fmap (unADA . toADA) _pledge
      MatchActiveStake eqPred -> 
        maybe False (runEqualityPredicate eqPred) $ fmap (unADA . toADA) _activeStake
      MatchSigma eqPred -> 
        maybe False (runEqualityPredicate eqPred) _sigma
      MatchBlockCount eqPred -> 
        maybe False (runEqualityPredicate eqPred) _blockCount
      MatchLivePledge eqPred -> 
        maybe False (runEqualityPredicate eqPred) $ fmap (unADA . toADA) _livePledge
      MatchLiveStake eqPred -> 
        maybe False (runEqualityPredicate eqPred) $ fmap (unADA . toADA) _liveStake
      MatchLiveDelegators eqPred -> 
        maybe False (runEqualityPredicate eqPred) _liveDelegators
      MatchLiveSaturation eqPred -> 
        maybe False (runEqualityPredicate eqPred) _liveSaturation

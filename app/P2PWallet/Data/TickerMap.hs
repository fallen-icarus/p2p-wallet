{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.TickerMap where

import Database.SQLite.Simple (ToRow(..),FromRow(..))
import Data.Map (foldrWithKey')
import Data.Text qualified as Text
import Data.Map qualified as Map

import P2PWallet.Data.Core.Asset hiding (fullName)
import P2PWallet.Data.Database
import P2PWallet.Prelude

type TickerMap = Map Text (Text,Text,Word8)
type FingerprintMap = Map Text (Text,Text)
type ReverseTickerMap = Map Text (Text,Word8)

-------------------------------------------------
-- Ticker Info
-------------------------------------------------
-- | The type for a verified ticker entry.
data TickerInfo = TickerInfo
  { ticker :: Text
  , policyId :: Text
  , assetName :: Text
  , decimals :: Word8 
  } deriving (Show,Eq,Generic,ToRow,FromRow)

makeFieldLabelsNoPrefix ''TickerInfo

instance TableName TickerInfo where
  tableName = "tickers"

instance Creatable TickerInfo where
  createStmt = Query $ unwords
    [ "CREATE TABLE " <> tableName @TickerInfo
    , "("
    , unwords $ intersperse ","
        [ "ticker TEXT PRIMARY KEY"
        , "policy_id TEXT NOT NULL"
        , "asset_name TEXT NOT NULL"
        , "decimals INTEGER NOT NULL"
        , "UNIQUE(policy_id,asset_name)"
        ]
    , ");"
    ]

instance Insertable TickerInfo where
  insertStmt = Query $ unwords
    [ "INSERT OR REPLACE INTO " <> tableName @TickerInfo
    , "("
    , unwords $ intersperse ","
        [ "ticker"
        , "policy_id"
        , "asset_name"
        , "decimals"
        ]
    , ")"
    , "VALUES (?,?,?,?);"
    ]

toTickerMap :: [TickerInfo] -> Map Text (Text,Text,Word8)
toTickerMap = fromList . map (\TickerInfo{..} -> (ticker,(policyId,assetName,decimals)))

fromTickerMap :: Map Text (Text,Text,Word8) -> [TickerInfo]
fromTickerMap = 
  foldrWithKey' 
    (\ticker (policyId,assetName,decimal) acc -> TickerInfo ticker policyId assetName decimal : acc) 
    []

toReverseTickerMap :: [TickerInfo] -> Map Text (Text,Word8)
toReverseTickerMap = fromList . map (\TickerInfo{..} -> (policyId <> "." <> assetName,(ticker,decimals)))

fromReverseTickerMap :: Map Text (Text,Word8) -> [TickerInfo]
fromReverseTickerMap = 
  foldrWithKey' 
    (\fullName (ticker,decimal) acc -> 
        let (policyId,assetName) = fmap (Text.drop 1) $ Text.breakOn "." fullName
        in TickerInfo ticker policyId assetName decimal : acc) 
    []

-------------------------------------------------
-- New Ticker Info
-------------------------------------------------
-- | The type for an unverified ticker entry.
data NewTickerInfo = NewTickerInfo
  { ticker :: Text
  , policyId :: Text
  , assetName :: Text
  , decimals :: Word8 
  } deriving (Show,Eq,Generic,ToRow,FromRow)

makeFieldLabelsNoPrefix ''NewTickerInfo

instance Default NewTickerInfo where
  def = NewTickerInfo 
    { ticker = ""
    , policyId = ""
    , assetName = ""
    , decimals = 0
    }

-------------------------------------------------
-- Showing native assets
-------------------------------------------------
showAssetBalance :: ReverseTickerMap -> NativeAsset -> Text
showAssetBalance reverseMap NativeAsset{..} =
  let mInfo = Map.lookup (policyId <> "." <> tokenName) reverseMap in 
  case mInfo of
    Nothing -> show quantity
    Just (ticker,decimal) -> 
      (<> " " <> ticker) $ show $ formatQuantity decimal quantity

showAssetInList :: ReverseTickerMap -> NativeAsset -> Text
showAssetInList reverseMap NativeAsset{..} =
  let mInfo = Map.lookup (policyId <> "." <> tokenName) reverseMap in 
  case mInfo of
    Nothing -> show quantity <> " " <> fingerprint
    Just (ticker,decimal) -> 
      (<> " " <> ticker) $ show $ formatQuantity decimal quantity

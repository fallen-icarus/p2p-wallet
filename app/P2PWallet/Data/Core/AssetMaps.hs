{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.AssetMaps where

import Database.SQLite.Simple (ToRow(..),FromRow(..))
import Data.Map (lookup)

import P2PWallet.Data.Core.Internal
import P2PWallet.Database
import P2PWallet.Plutus
import P2PWallet.Prelude

-------------------------------------------------
-- Map Aliases
-------------------------------------------------
-- | A type alias for the ticker map. It is used for converting a ticker alias to its on-chain
-- name when building transactions.
type TickerMap = Map Ticker (CurrencySymbol,TokenName,Word8)

-- | A type alias for the fingerprint map. It is used for converting a fingerprint to its on-chain
-- name when building transactions.
type FingerprintMap = Map Fingerprint (CurrencySymbol,TokenName)

-- | A type alias for the reverse ticker map. It is used both for checking if that ticker is 
-- already being used and for displaying asset balances using the tickers.
type ReverseTickerMap = Map (CurrencySymbol,TokenName) (Ticker,Word8)

-------------------------------------------------
-- Ticker Info
-------------------------------------------------
-- | The type for a verified ticker entry.
data TickerInfo = TickerInfo
  { ticker :: Ticker
  , policyId :: CurrencySymbol
  , assetName :: TokenName
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

-------------------------------------------------
-- List --> Map
-------------------------------------------------
toTickerMap :: [TickerInfo] -> TickerMap
toTickerMap = fromList . map (\TickerInfo{..} -> (ticker,(policyId,assetName,decimals)))

toReverseTickerMap :: [TickerInfo] -> ReverseTickerMap
toReverseTickerMap = fromList . map (\TickerInfo{..} -> ((policyId,assetName), (ticker,decimals)))

toFingerprintMap :: [NativeAsset] -> FingerprintMap
toFingerprintMap = fromList . map (\NativeAsset{..} -> (fingerprint,(policyId,tokenName)))

-------------------------------------------------
-- New Ticker Info
-------------------------------------------------
-- | The type for an unverified ticker entry.
data NewTickerInfo = NewTickerInfo
  { ticker :: Text
  , policyId :: Text
  , assetName :: Text
  , decimals :: Word8 
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewTickerInfo

instance Default NewTickerInfo where
  def = NewTickerInfo 
    { ticker = ""
    , policyId = ""
    , assetName = ""
    , decimals = 0
    }

-- | Process the new ticker and check that the asset is not already linked to another ticker.
processNewTickerInfo :: NewTickerInfo -> ReverseTickerMap -> Either Text TickerInfo
processNewTickerInfo NewTickerInfo{..} reverseTickerMap = do
  -- Check the policy id is valid.
  policy <- maybeToRight "Not a valid policy id" $ CurrencySymbol <$> parseHex policyId

  -- Check the asset name is valid.
  tokenName <- maybeToRight "Not a valid asset name" $ TokenName <$> parseHex assetName

  -- Check the on-chain name is not already linked to another ticker.
  whenJust (lookup (policy,tokenName) reverseTickerMap) $ \(tckr,_) ->
    -- If they match, that means this ticker info is about to be replaced while keeping the
    -- asset/ticker pair the same; this is okay.
    when (tckr /= Ticker ticker) $ 
      Left $ "This asset is already linked to another ticker: '" <> display tckr <> "'"

  -- Check the decimal places is valid.
  when (decimals < 0) $ Left "Decimal places must be >= 0"

  return $ TickerInfo
    { ticker = Ticker ticker
    , policyId = policy
    , assetName = tokenName
    , decimals = decimals
    }

-------------------------------------------------
-- Parsing using the maps
-------------------------------------------------
-- | Parse native assets separated by newlines. Native assets can be one of:
-- '# policy_id.asset_name'
-- '# fingerprint'
-- '# ticker'
-- All quantities must be greater than or equal to 0.
parseNativeAssets :: TickerMap -> FingerprintMap -> Text -> Either Text NativeAsset
parseNativeAssets tickerMap fingerprintMap assetLine =
    case words assetLine of
      [num,name] -> do
        asset <- maybeToRight parseErrorMsg $ asum
          [ parseTickerEntry num name
          , parseOnChainEntry assetLine
          , parseFingerprintEntry num name
          ]
        if asset ^. #quantity < 0 then Left $ "Quantities must be >= 0: " <> assetLine
        else return asset
      _ -> Left parseErrorMsg
  where
    parseErrorMsg :: Text
    parseErrorMsg = unlines
      [ "Invalid native asset entry. Entries must be separated by newlines, and be one of:"
      , "'# policy_id.asset_name'"
      , "'# fingerprint'"
      , "'# ticker'"
      , ""
      , "Could not parse: '" <> assetLine <> "'"
      , ""
      , "If using a ticker, make sure it is in the Ticker Registry."
      ]

    parseOnChainEntry :: Text -> Maybe NativeAsset
    parseOnChainEntry = parseNativeAsset

    parseTickerEntry :: Text -> Text -> Maybe NativeAsset
    parseTickerEntry num name =
      case lookup (Ticker name) tickerMap of
        Nothing -> Nothing
        Just (policy,assetName,decimal) -> do
          rawQuantity <- readMaybe @Decimal $ toString num
          parseNativeAsset $ unwords
            [ show (unFormatQuantity decimal rawQuantity)
            , display policy <> "." <> display assetName
            ]

    parseFingerprintEntry :: Text -> Text -> Maybe NativeAsset
    parseFingerprintEntry num name =
      case lookup (Fingerprint name) fingerprintMap of
        Nothing -> Nothing
        Just (policy,assetName) -> do
          parseNativeAsset $ unwords
            [ num
            , display policy <> "." <> display assetName
            ]

-------------------------------------------------
-- Showing Native Asset Balances
-------------------------------------------------
-- | Show the asset balance using the ticker if possible. If a ticker is not set,
-- using a fingerprint instead can be toggled with True/False. It is not always
-- desirable to use the fingerprint when a ticker is not set.
showAssetBalance :: Bool -> ReverseTickerMap -> NativeAsset -> Text
showAssetBalance withFingerprint reverseMap NativeAsset{..} =
  case lookup (policyId,tokenName) reverseMap of
    Nothing ->
      if withFingerprint
      then show quantity <> " " <> display fingerprint
      else show quantity
    Just (ticker,decimal) -> unwords
      [ show $ formatQuantity decimal quantity
      , display ticker
      ]

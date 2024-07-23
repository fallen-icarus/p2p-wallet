{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.AssetMaps where

import Database.SQLite.Simple (ToRow(..),FromRow(..))
import Data.Decimal (decimalPlaces)
import Data.Map.Strict qualified as Map

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
-- List <--> Map
-------------------------------------------------
toTickerMap :: [TickerInfo] -> TickerMap
toTickerMap = fromList . map (\TickerInfo{..} -> (ticker,(policyId,assetName,decimals)))

fromTickerMap :: TickerMap -> [TickerInfo]
fromTickerMap = map toTickerInfo . Map.toList
  where
    toTickerInfo :: (Ticker, (CurrencySymbol,TokenName,Word8)) -> TickerInfo
    toTickerInfo (ticker,(policyId,assetName,decimals)) = TickerInfo
      { ticker = ticker
      , policyId = policyId
      , assetName = assetName
      , decimals = decimals
      }

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
verifyNewTickerInfo :: NewTickerInfo -> ReverseTickerMap -> Either Text TickerInfo
verifyNewTickerInfo NewTickerInfo{..} reverseTickerMap = do
  -- Check the policy id is valid.
  policy <- maybeToRight "Not a valid policy id" $ CurrencySymbol <$> parseHex policyId

  -- Check the asset name is valid.
  tokenName <- maybeToRight "Not a valid asset name" $ TokenName <$> parseHex assetName

  -- 'ADA' cannot be used as a ticker.
  when (ticker == "ADA") $ Left "'ADA' cannot be used as a ticker."

  -- Check the on-chain name is not already linked to another ticker.
  whenJust (Map.lookup (policy,tokenName) reverseTickerMap) $ \(tckr,_) ->
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
      case Map.lookup (Ticker name) tickerMap of
        Nothing -> Nothing
        Just (policy,assetName,decimal) -> do
          rawQuantity <- readMaybe @Decimal $ toString num
          parseNativeAsset $ unwords
            [ show (unFormatQuantity decimal rawQuantity)
            , display policy <> "." <> display assetName
            ]

    parseFingerprintEntry :: Text -> Text -> Maybe NativeAsset
    parseFingerprintEntry num name =
      case Map.lookup (Fingerprint name) fingerprintMap of
        Nothing -> Nothing
        Just (policy,assetName) -> do
          parseNativeAsset $ unwords
            [ num
            , display policy <> "." <> display assetName
            ]

-- | Parse native asset names separated by newlines. Native assets names can be one of:
-- 'policy_id.asset_name'
-- 'ticker'
--
-- This can also be used to parse "ADA" into a `NativeAsset`.
parseNativeAssetName :: TickerMap -> Text -> Either Text NativeAsset
parseNativeAssetName tickerMap text =
    maybeToRight parseErrorMsg $ asum
      [ parseAdaEntry text
      , parseTickerEntry text
      , parseOnChainEntry text
      ]
  where
    parseErrorMsg :: Text
    parseErrorMsg = unlines
      [ "Invalid native asset name. Names must be one of:"
      , "'policy_id.asset_name'"
      , "'ticker'"
      , ""
      , "Could not parse: '" <> text <> "'"
      , ""
      , mconcat $ intersperse " " 
          [ "If using a ticker, make sure it is in the Ticker Registry."
          , "The ticker for the ada token is 'ADA'"
          ]
      , ""
      , mconcat $ intersperse " "
          [ "Fingerprints are not supported because there is no way for the p2p-wallet to know"
          , "which asset the fingerprint corresponds to unless it knows the asset in advance."
          ]
      ]

    parseAdaEntry :: Text -> Maybe NativeAsset
    parseAdaEntry t
      | t == "ADA" = Just lovelaceAsNativeAsset
      | otherwise = Nothing

    parseOnChainEntry :: Text -> Maybe NativeAsset
    parseOnChainEntry = parseNativeAsset

    parseTickerEntry :: Text -> Maybe NativeAsset
    parseTickerEntry name =
      case Map.lookup (Ticker name) tickerMap of
        Nothing -> Nothing
        Just (policy,assetName,_) -> do
          parseNativeAsset $ unwords
            [ "0"
            , display policy <> "." <> display assetName
            ]

-- | Parse the quantity of an asset, accounting for any decimal places.
parseFormattedAssetQuantity :: ReverseTickerMap -> NativeAsset -> Text -> Either Text NativeAsset
parseFormattedAssetQuantity reverseTickerMap asset@NativeAsset{policyId,tokenName} text =
  case Map.lookup (policyId,tokenName) reverseTickerMap of
    Nothing ->
      -- Ada is not in the ticker map, but will be provided as a decimal.
      if policyId == "" then do
        let errMsg = unlines
              [ "Could not parse: " <> text
              , "Expecting a decimal with 6 decimal places for: 'ADA'"
              ]
        amountAsDecimal <- maybeToRight errMsg $ readMaybe @Decimal (toString text)
        if decimalPlaces amountAsDecimal > 6 then
          Left errMsg
        else
          return $ asset & #quantity .~ unFormatQuantity 6 amountAsDecimal
      else do
        let errMsg = unlines
              [ "Could not parse: " <> text
              , "Expecting a whole number for: " <> display policyId <> "." <> display tokenName
              ]
        amount <- maybeToRight errMsg $ readMaybe @Integer $ toString text
        return $ asset & #quantity .~ amount
    Just (tckr,decimal) -> do
      let errMsg = unlines
            [ "Could not parse: " <> text
            , fromString $ printf 
                "Expecting a decimal with %d decimal place(s) for: '%s'" 
                decimal 
                (display tckr)
            ]
      amountAsDecimal <- maybeToRight errMsg $ readMaybe @Decimal (toString text)
      if decimalPlaces amountAsDecimal > decimal then
        Left errMsg
      else
        return $ asset & #quantity .~ unFormatQuantity decimal amountAsDecimal

-------------------------------------------------
-- Showing Native Asset Balances
-------------------------------------------------
-- | Show the asset balance using the ticker if possible. If a ticker is not set,
-- using a fingerprint instead can be toggled with True/False. It is not always
-- desirable to use the fingerprint when a ticker is not set.
showAssetBalance :: Bool -> ReverseTickerMap -> NativeAsset -> Text
showAssetBalance withFingerprint reverseMap NativeAsset{..}
  | policyId == "" = display $ Lovelace quantity 
  | otherwise = case Map.lookup (policyId,tokenName) reverseMap of
      Nothing ->
        if withFingerprint
        then show quantity <> " " <> display fingerprint
        else show quantity
      Just (ticker,decimal) -> unwords
        [ show $ formatQuantity decimal quantity
        , display ticker
        ]

-- | Show the asset quantity without the name/fingerprint/ticker.
showAssetQuantityOnly :: ReverseTickerMap -> NativeAsset -> Text
showAssetQuantityOnly reverseMap NativeAsset{..}
  | policyId == "" = show $ unAda $ toAda $ Lovelace quantity
  | otherwise = case Map.lookup (policyId,tokenName) reverseMap of
      Nothing -> show quantity
      Just (_,decimal) -> show $ formatQuantity decimal quantity

-- | Show the asset name using the ticker if possible. If a ticker is not set,
-- use the asset fingerprint.
showAssetNameOnly :: ReverseTickerMap -> NativeAsset -> Text
showAssetNameOnly reverseMap NativeAsset{..}
  | policyId == "" = "ADA"
  | otherwise = case Map.lookup (policyId,tokenName) reverseMap of
      Nothing -> display fingerprint
      Just (ticker,_) -> display ticker

-------------------------------------------------
-- Price Parsing
-------------------------------------------------
-- | Format a `Rational` (representing the on-chain price) to the required number of decimals,
-- and convert it to `Text`.
showPriceFormatted
  :: ReverseTickerMap 
  -> NativeAsset -- ^ Numerator asset.
  -> NativeAsset -- ^ Denominator asset.
  -> Rational
  -> Text
showPriceFormatted reverseTickerMap numAsset denAsset price =
    show @Text @Decimal $ 
      realFracToDecimal numDecimals $ -- Use the number of decimals for the numerator.
        price * mkScaleFactor (decimals denAsset) / mkScaleFactor (decimals numAsset)
  where
    numDecimals :: Word8
    numDecimals = decimals numAsset

    decimals :: NativeAsset -> Word8
    decimals NativeAsset{policyId,tokenName}
      | policyId == "" = 6
      | otherwise = case Map.lookup (policyId,tokenName) reverseTickerMap of
          Nothing -> 0
          Just (_,decimal) -> decimal

-- | Parse a formatted decimal to the on-chain price.
parseFormattedPrice 
  :: ReverseTickerMap 
  -> NativeAsset -- ^ Numerator asset.
  -> NativeAsset -- ^ Denominator asset.
  -> Text
  -> Either Text Rational
parseFormattedPrice reverseTickerMap numAsset denAsset text = do
    price <- maybeToRight ("'" <> text <> "' is not a decimal") $ 
      readMaybe @Decimal $ toString text

    -- The price cannot have more decimal places than the numerator asset supports.
    when (decimalPlaces price > numeratorDecimals) $ Left errMsg

    return $ toRational price * mkScaleFactor numeratorDecimals / mkScaleFactor (decimals denAsset)
  where
    errMsg = unlines
      [ "Could not parse: " <> text
      , fromString $ printf "Expecting a decimal with %d decimal place(s)." numeratorDecimals
      ]

    numeratorDecimals :: Word8
    numeratorDecimals = decimals numAsset

    decimals :: NativeAsset -> Word8
    decimals NativeAsset{policyId,tokenName}
      | policyId == "" = 6
      | otherwise = case Map.lookup (policyId,tokenName) reverseTickerMap of
          Nothing -> 0
          Just (_,decimal) -> decimal

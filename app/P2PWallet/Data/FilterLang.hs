{-# LANGUAGE StrictData #-}

{-

A very basic DSL for expressing filtering algorithms. It is based on JSON so that a custom
parser is not needed.

-}
module P2PWallet.Data.FilterLang
  ( EqualityPredicate(..)
  , readEqualityPredicate
  , runEqualityPredicate
  , FilterLang(..)
  , runFilter
  ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types (parseEither,Parser,parseFail)

import P2PWallet.Prelude

-- | A predicate for types with an `Ord` instance.
data EqualityPredicate a
  -- | Greater than a. Written as "> a".
  = IsGT a
  -- | Greater than or equal to a. Written as ">= a".
  | IsGTE a
  -- | Less than a. Written as "< a".
  | IsLT a
  -- | Less than or equal to a. Written as "<= a".
  | IsLTE a
  -- | Equal to a. Written as "== a".
  | IsE a
  deriving (Show,Eq)

readEqualityPredicate :: (Ord a, Read a) => Text -> Maybe (EqualityPredicate a)
readEqualityPredicate t = case words t of
  [">",n] -> IsGT <$> readMaybe (toString n)
  [">=",n] -> IsGTE <$> readMaybe (toString n)
  ["<",n] -> IsLT <$> readMaybe (toString n)
  ["<=",n] -> IsLTE <$> readMaybe (toString n)
  ["==",n] -> IsE <$> readMaybe (toString n)
  _ -> Nothing

runEqualityPredicate :: Ord a => EqualityPredicate a -> a -> Bool
runEqualityPredicate (IsGT x) y = y > x
runEqualityPredicate (IsGTE x) y = y >= x
runEqualityPredicate (IsLT x) y = y < x
runEqualityPredicate (IsLTE x) y = y <= x
runEqualityPredicate (IsE x) y = y == x

instance (Show a) => ToJSON (EqualityPredicate a) where
  toJSON (IsGT x) = toJSON $ unwords [">", show x]
  toJSON (IsGTE x) = toJSON $ unwords [">=", show x]
  toJSON (IsLT x) = toJSON $ unwords ["<", show x]
  toJSON (IsLTE x) = toJSON $ unwords ["<=", show x]
  toJSON (IsE x) = toJSON $ unwords ["==", show x]

instance (Ord a, Read a) => FromJSON (EqualityPredicate a) where
  parseJSON value = withText "EqualityPredicate" 
      (either parseFail pure . maybeToRight errMsg . readEqualityPredicate) value
    where
      errMsg :: String
      errMsg = toString $ unlines
        [ "Could not parse: " <> showValue value
        , ""
        , "Make sure equality checks contain a space: '<= x'."
        ]

data FilterLang a
  = MatchAny [FilterLang a]
  | MatchAll [FilterLang a]
  | MatchNone [FilterLang a]
  | MatchPredicate a
  deriving (Show,Eq)

runFilter :: (a -> Bool) -> FilterLang a -> Bool
runFilter check (MatchPredicate p) = check p
runFilter check (MatchAny ps') = any (runFilter check) ps'
runFilter check (MatchAll ps') = all (runFilter check) ps'
runFilter check (MatchNone ps') = not $ runFilter check $ MatchAny ps'

instance ToJSON a => ToJSON (FilterLang a) where
  toJSON (MatchAny xs) = object [ "any" .= toJSON xs ]
  toJSON (MatchAll xs) = object [ "all" .= toJSON xs ]
  toJSON (MatchNone xs) = object [ "none" .= toJSON xs ]
  toJSON (MatchPredicate x) = toJSON x

instance FromJSON a => FromJSON (FilterLang a) where
  parseJSON value = flip (withObject "FilterLang") value $ \o -> do
      let anyVal = Aeson.lookup "any" o
          allVal = Aeson.lookup "all" o
          noneVal = Aeson.lookup "none" o
      if isJust anyVal then
        either parseFail pure $ parseEither anyParser value
      else if isJust allVal then
        either parseFail pure $ parseEither allParser value
      else if isJust noneVal then
        either parseFail pure $ parseEither noneParser value
      else
        either parseFail pure $ parseEither predicateParser value

    where
      anyParser :: Value -> Parser (FilterLang a)
      anyParser = withObject "Any" $ \o ->
        MatchAny <$> o .: "any"

      allParser :: Value -> Parser (FilterLang a)
      allParser = withObject "All" $ \o ->
        MatchAll <$> o .: "all"

      noneParser :: Value -> Parser (FilterLang a)
      noneParser = withObject "None" $ \o ->
        MatchNone <$> o .: "none"

      predicateParser :: Value -> Parser (FilterLang a)
      predicateParser = fmap MatchPredicate . parseJSON

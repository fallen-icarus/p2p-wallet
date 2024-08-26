{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Core.Internal.ReferenceScript where

import Data.Aeson

import P2PWallet.Prelude

-- | The type representing the information for a reference script attached to a UTxO.
data ReferenceScript = ReferenceScript
  { scriptHash :: Text
  , size :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ReferenceScript

instance FromJSON ReferenceScript where
  parseJSON =
      withObject "ReferenceScript" $ \o ->
        ReferenceScript
          <$> o .: "hash"
          <*> o .: "size"

instance ToJSON ReferenceScript where
  toJSON ReferenceScript{..} =
    object [ "hash" .= scriptHash
           , "size" .= size
           ]

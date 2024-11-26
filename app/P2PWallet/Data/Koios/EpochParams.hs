{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#get-/epoch_params API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.EpochParams where

import Data.Aeson

import P2PWallet.Prelude

data EpochParams = EpochParams 
  { optimalPoolCount :: Decimal -- K parameter
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''EpochParams

instance FromJSON EpochParams where
  parseJSON = withArray "EpochParams" $ \case
    [obj] -> flip (withObject "EpochParams") obj $ \o -> 
               EpochParams <$> o .: "optimal_pool_count"
    _ -> mzero

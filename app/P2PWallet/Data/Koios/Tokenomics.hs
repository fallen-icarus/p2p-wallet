{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#get-/totals API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.Tokenomics where

import Data.Aeson

import P2PWallet.Data.Core.Internal.Assets
import P2PWallet.Prelude

data Tokenomics = Tokenomics 
  { totalSupply :: Lovelace 
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''Tokenomics

instance FromJSON Tokenomics where
  parseJSON = withArray "Tokenomics" $ \case
    [obj] -> flip (withObject "Tokenomics") obj $ \o -> 
               Tokenomics <$> o .: "supply"
    _ -> mzero

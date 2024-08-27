{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

This module has the return types for the https://preprod.koios.rest/#post-/asset_history API 
endpoint (the types are the same for mainnet). 

-}
module P2PWallet.Data.Koios.MintTransaction where

import Data.Aeson

import P2PWallet.Prelude

data MintTransaction = MintTransaction 
  { txHash :: Text 
  , quantity :: Integer
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''MintTransaction

instance FromJSON MintTransaction where
  parseJSON = withObject "MintTransaction" $ \o ->
      MintTransaction
        <$> o .: "tx_hash"
        <*> (o .: "quantity" >>= maybe mzero return . readMaybe)

-- | This type is used to unwrap the koios response.
newtype MintTransactions = MintTransactions { unMintTransactions :: [MintTransaction] }

instance FromJSON MintTransactions where
  parseJSON = withObject "MintTransactions" $ \o ->
      MintTransactions
        <$> o .: "minting_txs"

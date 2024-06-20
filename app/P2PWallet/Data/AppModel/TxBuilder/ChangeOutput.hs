{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilder.ChangeOutput where

import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.Network
import P2PWallet.Prelude

-------------------------------------------------
-- Change Output
-------------------------------------------------
-- | Information for the change output. The fee will be deducted from this output.
data ChangeOutput = ChangeOutput
  -- | The address where the change will go.
  { paymentAddress :: PaymentAddress
  -- | The amount of ada to be returned as change.
  , lovelace :: Lovelace
  -- | The native assets quantities to be returned as change.
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''ChangeOutput

instance Default ChangeOutput where
  def = ChangeOutput
    { paymentAddress = ""
    , lovelace = 0
    , nativeAssets = []
    }

-------------------------------------------------
-- New Change Output
-------------------------------------------------
-- | Information for the change output. The fee will be deducted from this output.
data NewChangeOutput = NewChangeOutput
  -- | The address where the change will go.
  { paymentAddress :: Text
  , lovelace :: Lovelace
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewChangeOutput

instance Default NewChangeOutput where
  def = NewChangeOutput
    { paymentAddress = ""
    , lovelace = 0
    , nativeAssets = []
    }

-------------------------------------------------
-- NewChangeOutput <--> ChangeOutput
-------------------------------------------------
processNewChangeOutput :: Network -> NewChangeOutput -> Either Text ChangeOutput
processNewChangeOutput network NewChangeOutput{..} = do
  addr <- readPaymentAddress network paymentAddress

  return $ ChangeOutput
    { paymentAddress = addr
    , lovelace = 0
    , nativeAssets = []
    }

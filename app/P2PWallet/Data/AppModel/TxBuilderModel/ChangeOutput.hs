{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.ChangeOutput where

import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Plutus
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

instance AddToTxBody ChangeOutput where
  addToTxBody txBody ChangeOutput{..} = 
      txBody 
        -- Add the output while preserving ordering.
        & #outputs %~ flip snoc newOutput
    where 
      newOutput :: TxBodyOutput
      newOutput = TxBodyOutput
        { paymentAddress = paymentAddress
        , lovelace = lovelace
        , nativeAssets = nativeAssets
        , datum = NoOutputDatum
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
verifyNewChangeOutput :: Network -> NewChangeOutput -> Either Text ChangeOutput
verifyNewChangeOutput network NewChangeOutput{..} = do
  addr <- parsePaymentAddress network paymentAddress

  return $ ChangeOutput
    { paymentAddress = addr
    , lovelace = 0
    , nativeAssets = []
    }

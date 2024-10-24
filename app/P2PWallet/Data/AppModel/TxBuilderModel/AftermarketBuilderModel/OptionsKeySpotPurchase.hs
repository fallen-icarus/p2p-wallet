{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.OptionsKeySpotPurchase where

import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.SpotPurchase
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ContractExecution
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets
import P2PWallet.Prelude

-------------------------------------------------
-- Options Key Spot Purchase
-------------------------------------------------
-- | Information for purchasing a spot sale.
data OptionsKeySpotPurchase = OptionsKeySpotPurchase
  -- | The sale being purchased.
  { spotPurchase :: SpotPurchase
  -- | The contracts and whether they should be executed.
  , contracts :: [(Bool,OptionsUTxO)]
  -- | The contracts to be immediately executed.
  , executions :: [OptionsContractExecution]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsKeySpotPurchase

instance AssetBalancesForChange (a,OptionsKeySpotPurchase) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #spotPurchase) xs
    , assetBalancesForChange $
        concatMap (map (0::Int,) . view (_2 % #executions)) xs
    ]

-------------------------------------------------
-- NewOptionsKeySpotPurchase
-------------------------------------------------
data NewOptionsKeySpotPurchase = NewOptionsKeySpotPurchase
  { spotPurchase :: SpotPurchase
  , contracts :: [(Bool,OptionsUTxO)]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewOptionsKeySpotPurchase

instance Default NewOptionsKeySpotPurchase where
  def = NewOptionsKeySpotPurchase
    { spotPurchase = def
    , contracts = []
    }

-------------------------------------------------
-- NewOptionsKeySpotPurchase <--> OptionsKeySpotPurchase
-------------------------------------------------
-- | Verify the user info for the address update.
verifyNewOptionsKeySpotPurchase 
  :: Network
  -> Text
  -> POSIXTime 
  -> NewOptionsKeySpotPurchase 
  -> Either Text OptionsKeySpotPurchase
verifyNewOptionsKeySpotPurchase network alias currentTime NewOptionsKeySpotPurchase{..} = do
  let verifiedExecutions = 
        map (optionsUTxOToOptionsContractExecution network alias currentTime . snd) $
          filter fst contracts

  return $ OptionsKeySpotPurchase
    { spotPurchase = spotPurchase
    , contracts = contracts
    , executions = verifiedExecutions
    }

toNewOptionsKeySpotPurchase :: OptionsKeySpotPurchase -> NewOptionsKeySpotPurchase
toNewOptionsKeySpotPurchase OptionsKeySpotPurchase{..} = NewOptionsKeySpotPurchase
  { spotPurchase = spotPurchase
  , contracts = contracts
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateOptionsKeySpotPurchaseDeposits 
  :: OptionsKeySpotPurchase 
  -> [Lovelace] 
  -> Either Text OptionsKeySpotPurchase
updateOptionsKeySpotPurchaseDeposits i calculatedDeposits = do
  spotDeposit <- maybeToRight "calculatedDeposits is empty" $ maybeLast calculatedDeposits

  return $ i
    & #spotPurchase %~ flip updateSpotPurchaseDeposit spotDeposit

-- | Generate the deposit message.
createOptionsKeySpotPurchaseDepositMsg :: OptionsKeySpotPurchase -> Text
createOptionsKeySpotPurchaseDepositMsg OptionsKeySpotPurchase{spotPurchase} =
  unlines $ intersperse "" $ filter (/= "")
    [ createSpotPurchaseDepositMsg spotPurchase ]

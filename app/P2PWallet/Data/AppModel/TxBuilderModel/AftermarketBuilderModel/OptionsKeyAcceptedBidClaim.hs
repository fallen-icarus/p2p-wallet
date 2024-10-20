{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.OptionsKeyAcceptedBidClaim where

import P2PWallet.Data.AppModel.TxBuilderModel.AftermarketBuilderModel.AcceptedBidClaim
import P2PWallet.Data.AppModel.TxBuilderModel.OptionsBuilderModel.ContractExecution
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.Wallets.OptionsWallet
import P2PWallet.Prelude

-------------------------------------------------
-- Options Key Accepted Bid Claim
-------------------------------------------------
-- | Information for claiming an accepted bid for options keys.
data OptionsKeyAcceptedBidClaim = OptionsKeyAcceptedBidClaim
  -- | The accepted bid being claim.
  { bidClaim :: AcceptedBidClaim
  -- | The contracts and whether they should be executed.
  , contracts :: [(Bool,OptionsUTxO)]
  -- | The contracts to be immediately executed.
  , executions :: [OptionsContractExecution]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''OptionsKeyAcceptedBidClaim

instance AssetBalancesForChange (a,OptionsKeyAcceptedBidClaim) where
  assetBalancesForChange xs = sumAssetBalances
    [ assetBalancesForChange $ map (over _2 $ view #bidClaim) xs
    , assetBalancesForChange $
        concatMap (map (0::Int,) . view (_2 % #executions)) xs
    ]

-------------------------------------------------
-- NewOptionsKeyAcceptedBidClaim
-------------------------------------------------
data NewOptionsKeyAcceptedBidClaim = NewOptionsKeyAcceptedBidClaim
  { bidClaim :: AcceptedBidClaim
  , contracts :: [(Bool,OptionsUTxO)]
  } deriving (Show,Eq)

makeFieldLabelsNoPrefix ''NewOptionsKeyAcceptedBidClaim

instance Default NewOptionsKeyAcceptedBidClaim where
  def = NewOptionsKeyAcceptedBidClaim
    { bidClaim = def
    , contracts = []
    }

-------------------------------------------------
-- NewOptionsKeyAcceptedBidClaim <--> OptionsKeyAcceptedBidClaim
-------------------------------------------------
-- | Verify the user info for the address update.
verifyNewOptionsKeyAcceptedBidClaim 
  :: POSIXTime 
  -> Text
  -> Network
  -> NewOptionsKeyAcceptedBidClaim 
  -> Either Text OptionsKeyAcceptedBidClaim
verifyNewOptionsKeyAcceptedBidClaim currentTime alias network NewOptionsKeyAcceptedBidClaim{..} = do
  let verifiedExecutions = 
        map (optionsUTxOToOptionsContractExecution network alias currentTime . snd) $
          filter fst contracts

  return $ OptionsKeyAcceptedBidClaim
    { bidClaim = bidClaim
    , contracts = contracts
    , executions = verifiedExecutions
    }

toNewOptionsKeyAcceptedBidClaim :: OptionsKeyAcceptedBidClaim -> NewOptionsKeyAcceptedBidClaim
toNewOptionsKeyAcceptedBidClaim OptionsKeyAcceptedBidClaim{..} = NewOptionsKeyAcceptedBidClaim
  { bidClaim = bidClaim
  , contracts = contracts
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Update the required deposit increase if necessary.
updateOptionsKeyAcceptedBidClaimDeposits 
  :: OptionsKeyAcceptedBidClaim 
  -> [Lovelace] 
  -> Either Text OptionsKeyAcceptedBidClaim
updateOptionsKeyAcceptedBidClaimDeposits i calculatedDeposits = do
  spotDeposit <- maybeToRight "calculatedDeposits is empty" $ maybeLast calculatedDeposits

  return $ i
    & #bidClaim %~ flip updateAcceptedBidClaimDeposit spotDeposit

-- | Generate the deposit message.
createOptionsKeyAcceptedBidClaimDepositMsg :: OptionsKeyAcceptedBidClaim -> Text
createOptionsKeyAcceptedBidClaimDepositMsg OptionsKeyAcceptedBidClaim{bidClaim} =
  unlines $ intersperse "" $ filter (/= "") $
    [ createAcceptedBidClaimDepositMsg bidClaim ]

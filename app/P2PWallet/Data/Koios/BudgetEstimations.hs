{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module P2PWallet.Data.Koios.BudgetEstimations where

import Data.Aeson

import P2PWallet.Data.Core.TxBody
import P2PWallet.Prelude

-- | The type of validator returned by Koios during budget estimations.
data ValidatorIndex
  = Spending Int
  | Minting Int
  | Withdrawing Int
  deriving (Show,Eq,Ord)

instance ToJSON ValidatorIndex where
  toJSON (Spending idx) = toJSON $ "spend:" <> show @Text idx
  toJSON (Minting idx) = toJSON $ "mint:" <> show @Text idx
  toJSON (Withdrawing idx) = toJSON $ "withdraw:" <> show @Text idx

instance FromJSON ValidatorIndex where
  parseJSON = withObject "ValidatorIndex" $ \o -> do
    purpose <- o .: "purpose"
    idx <- o .: "index"

    case (purpose :: Text) of
      "mint" -> return $ Minting idx
      "spend" -> return $ Spending idx
      "withdraw" -> return $ Withdrawing idx
      _ -> mzero

-- | The estimated execution budget paired with the corresponding validator index.
data BudgetEstimation = BudgetEstimation
  { validatorIndex :: ValidatorIndex
  , executionBudget :: ExecutionBudget
  } deriving (Show,Eq,Ord)

makeFieldLabelsNoPrefix ''BudgetEstimation

instance ToJSON BudgetEstimation where
  toJSON BudgetEstimation{..} = object
    [ "validator" .= validatorIndex
    , "budget" .= executionBudget
    ]

instance FromJSON BudgetEstimation where
  parseJSON = withObject "BudgetEstimation" $ \o ->
    BudgetEstimation
      <$> o .: "validator"
      <*> o .: "budget"

-- | The successfull estimation result.
newtype EstimationResult = EstimationResult { unEstimationResult :: [BudgetEstimation] }
  deriving (Show,Eq)

makeFieldLabelsNoPrefix ''EstimationResult

instance FromJSON EstimationResult where
  parseJSON = withObject "EstimationResult" $ \o ->
    EstimationResult
      <$> o .: "result"

-- | The estimation error message.
newtype EstimationError = EstimationError { unEstimationError :: Value }
  deriving (Show,Eq)

makeFieldLabelsNoPrefix ''EstimationError

instance FromJSON EstimationError where
  parseJSON = withObject "EstimationError" $ \o ->
    EstimationError
      <$> o .: "error"

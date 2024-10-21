{-# LANGUAGE StrictData #-}

module P2PWallet.Data.Koios.TxSubmissionResponse
  ( 
    NewTxHash(..)
  , SubmissionErrorMessage(..)
  ) where

import Data.Aeson

import P2PWallet.Prelude

-- | A type representing the new transaction hash for a successfully submitted transaction. The hash
-- is prefixed with a success message.
newtype NewTxHash = NewTxHash { unNewTxHash :: Text }

instance FromJSON NewTxHash where
  parseJSON x = do
    result <- withObject "submission_response" (.: "result") x
    tx <- withObject "submission_result_field" (.: "transaction") result
    hash <- withObject "submission_tx_field" (.: "id") tx
    return $ NewTxHash $ "Submission successfull!\n\nNew transaction hash: " <> hash

-- | A type representing the node error message when the node rejects a transaction.
newtype SubmissionErrorMessage = SubmissionErrorMessage { unSubmissionErrorMessage :: Text }

instance FromJSON SubmissionErrorMessage where
  parseJSON x = do
    err <- withObject "submission_response" (.: "error") x
    errMsg <- withObject "submission_error" (.: "data") err
    return $ SubmissionErrorMessage $ "Submission failed.\n\n" <> showValue errMsg

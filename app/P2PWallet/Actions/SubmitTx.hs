module P2PWallet.Actions.SubmitTx
  ( 
    submitTx
  ) where

import Data.Aeson (parseJSON)
import Data.Aeson.Types (parseMaybe)

import P2PWallet.Actions.Query.Koios
import P2PWallet.Data.App
import P2PWallet.Data.Core.Network
import P2PWallet.Data.Files
import P2PWallet.Data.Koios.TxSubmissionResponse
import P2PWallet.Prelude

submitTx :: Network -> SignedTxFile -> IO Text
submitTx network' (SignedTxFile signedFile) = do
  runSubmitTx network' signedFile >>= \case
    Right r -> do 
      -- It was returned as `Value` so the result must still be decoded.
      let succMsg = unNewTxHash <$> parseMaybe parseJSON r
          errMsg = unSubmissionErrorMessage <$> parseMaybe parseJSON r
      case succMsg <|> errMsg of
        Nothing -> throwIO $ AppError $ "Could not parse response:\n\n" <> showValue r
        Just msg -> return msg
    Left err -> throwIO $ AppError err

module P2PWallet.Actions.CalculateMinUTxOValue
  (
    calculateMinUTxOValue
  , minUTxOErrorMessage
  , changeHasTooLittleAdaMsg
  ) where

import System.FilePath ((</>), (<.>))

import P2PWallet.Actions.Query.Koios
import P2PWallet.Actions.Utils
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
import P2PWallet.Prelude

-- | The error message to throw when the min UTxO value is not satisfied.
minUTxOErrorMessage :: Lovelace -> Text
minUTxOErrorMessage reqLovelace = unlines
  [ "There is not enough ADA in this output."
  , fromString $ printf "The minimum required ADA for this output is: %s" $ display reqLovelace
  ]

-- | The error message to throw when the change output has too little ADA. This is checked/thrown
-- during the build step.
changeHasTooLittleAdaMsg :: Lovelace -> Text
changeHasTooLittleAdaMsg reqLovelace = unlines
  [ "There is not enough ADA in the change output."
  , fromString $ 
      printf "The minimum required ADA for the change output is: %s" $ display reqLovelace
  , ""
  , "Add another input to increase the amount of ADA as change."
  , "NOTE: Adding more native tokens may increase the amount of required ADA."
  ]

-- | Calculate the required minUTxOValue for the new output. The order of the returned
-- list matches the order of the outputs generated in the `AddToTxBody` class.
calculateMinUTxOValue :: (AddToTxBody a) => Network -> Maybe ByteString -> a -> IO [Lovelace]
calculateMinUTxOValue network mParameters newOutputAction = do
  -- Get the file names since cardano-cli works with files.
  tmpDir <- TmpDirectory <$> getTemporaryDirectory
  let paramsFile = ParamsFile $ toString tmpDir </> "params" <.> "json"

  -- Get the current parameters if necessary, and write them to a file for cardano-cli to use.
  -- If wallets have already been synced since starting the app, the parameters should already be
  -- saved in the `TxBuilderModel`.
  parameters <- maybe (runGetParams network >>= fromRightOrAppError) return mParameters
  writeFileBS (toString paramsFile) parameters

  -- Extract out the new outputs for the this action..
  let TxBody{outputs} = addToTxBody mempty newOutputAction

  -- Any plutus scripts that are used locally must be exported.  The redeemers and datums used
  -- must also be exported. All files will be located in the tmp directory and will have their
  -- hashes as the file names.
  mapM_ exportContractFiles outputs

  -- Calculate the min required UTxO value for each of the new outputs.
  forM outputs $ \output ->
    fromJustOrAppError "Could not parse min required value." . fmap Lovelace . readMaybe =<<
      runCmd (calculateCmd tmpDir paramsFile output)

-- | The actual command used to calculate the required minUTxO.
calculateCmd :: TmpDirectory -> ParamsFile -> TxBodyOutput -> String
calculateCmd tmpDir paramsFile output = toString $ unwords
  [ "(cardano-cli transaction calculate-min-required-utxo"
  , "--protocol-params-file " <> toText paramsFile
  , toBuildCmdField tmpDir output <> ") | cut -d' ' -f2"
  ]

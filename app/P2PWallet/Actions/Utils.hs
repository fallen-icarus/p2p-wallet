{-# LANGUAGE RecordWildCards #-}

module P2PWallet.Actions.Utils where

import System.Exit (ExitCode(ExitSuccess))
import System.Process (readCreateProcessWithExitCode, shell)
import Data.Map qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core
import P2PWallet.Plutus
import P2PWallet.Prelude

-- | Run an action. If successfull, do something with the results. If unsuccessfull, do something
-- with the error message. This is usefull for letting the frontend decide what to do based on
-- the results.
runAction :: (Text -> b) -> (c -> b) -> IO c -> IO b
runAction onAppErr onSuccess action = 
  handle (\(AppError err) -> return $ onAppErr err) $ fmap onSuccess action

-- | Run an action. If successfull, do something with the results. If unsuccessfull, pass
-- the error message to `Alert`. 
runActionOrAlert :: (c -> AppEvent) -> IO c -> IO AppEvent
runActionOrAlert onSuccess action = runAction Alert onSuccess action

-- | Gets the `Just` value or throws an `AppError` with the supplied error message.
fromJustOrAppError :: Text -> Maybe a -> IO a
fromJustOrAppError errMsg = maybe (throwIO $ AppError errMsg) return

-- | Gets the `Right` value or throws an `AppError` with the error message in the `Left`.
fromRightOrAppError :: Either Text a -> IO a
fromRightOrAppError = either (throwIO . AppError) return

-- | Run the command and return either the results or the error message. The trailing newline is
-- dropped.
runCmd :: String -> IO String
runCmd cmd =
    readCreateProcessWithExitCode (shell cmd) [] >>= \case
      (ExitSuccess,result,_) -> return $ dropTrailingNewline result
      (_,_,errMsg) -> throwIO $ AppError $ toText $ dropTrailingNewline errMsg
  where
    dropTrailingNewline s = 
      case reverse s of
        ('\n':xs) -> reverse xs
        _ -> s

-- | Re-index the lists while preserving ordering.
reIndex :: [(Int,a)] -> [(Int,a)]
reIndex xs = go xs 0
  where
    go [] _ = []
    go ((_,y):ys) i = (i,y) : go ys (i+1)

-- | Remove the item from the list and re-index after.
removeAction :: Int -> [(Int,a)] -> [(Int,a)]
removeAction idx xs = reIndex $ filter ((/=idx) . fst) xs

-- | Balance the inputs with the outputs by updating the changeOutput and subtracting off the
-- fee.
balanceTx :: TxBuilderModel -> TxBuilderModel
balanceTx tx@TxBuilderModel{..} =
    tx & #changeOutput .~ (if newChange == def then Nothing else Just newChange)
       & #isBalanced .~ balanced
       & #isBuilt .~ False
  where
    newChange :: ChangeOutput
    newChange = ChangeOutput
      { paymentAddress = fromMaybe "" $ changeOutput ^? _Just % #paymentAddress
      , lovelace = loves - fee
      , nativeAssets = assets
      }

    -- The amount of ADA and native assets available as change.
    (loves :: Lovelace, assets :: [NativeAsset]) =
      let (inLoves,inAssets) = 
            ( sum $ map (view #lovelace . snd) userInputs
            , concatMap (view #nativeAssets . snd) userInputs
            )
          (outLoves,outAssets) = 
            ( sum $ map (view #lovelace . snd) userOutputs
            , concatMap (view #nativeAssets . snd) userOutputs
            )
          inMap = Map.fromList $ map (\a -> (a ^. fullName, a ^. #quantity)) inAssets
          outMap = Map.fromList $ map (\a -> (a ^. fullName, negate $ a ^. #quantity)) outAssets
          bal = map (fromMaybe def . readNativeAsset . \(name',q) -> show q <> " " <> name') 
              $ filter (\(_,q) -> q /= 0)
              $ Map.toList 
              $ Map.unionWith (+) inMap outMap
      in (inLoves - outLoves,bal)

    -- Whether all assets are balanced.
    balanced = all ((>= 0) . view #quantity) assets && loves - fee >= 0

-- | A list of unique pub keys that must sign the transaction as well as their `DerivationPath`
-- if known. 
getRequiredWitnesses :: TxBuilderModel -> Either Text [Witness]
getRequiredWitnesses TxBuilderModel{..} = do
    -- | Get the required pubkeys among the inputs. Returns the first error, if any.
    inputWitnesses <- catMaybes <$> sequence (map checkInput userInputs)

    -- | Remove repeat keys before returning. You cannot have the same `PubKeyHash` with different
    -- `DerivationPath`s so it is enough to just check the key hashes. Filter out registration
    -- certificate keys if they are also normal witnesses.
    return $ ordNubOn (fst . view #witness) inputWitnesses
  where
    -- Get the pubkeyhash from the plutus address. Returns nothing if the input is a script input.
    checkInput :: (Int,UserInput) -> Either Text (Maybe Witness)
    checkInput (index,i) = do
      plutusAddr <- 
        first (const $ "Input " <> show index <> " has an invalid bech32 shelley address.") $
          paymentAddressToPlutusAddress $ i ^. #paymentAddress
      return $ case toPubKeyHash plutusAddr of
        Nothing -> Nothing
        Just pkHash -> Just $ Witness (pkHash, i ^. #paymentKeyPath)

-- | Convert `Network` to the required flag for cardano-cli.
toNetworkFlag :: Network -> Text
toNetworkFlag Mainnet = "--mainnet"
toNetworkFlag Testnet = "--testnet-magic 1"

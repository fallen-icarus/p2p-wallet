module P2PWallet.Actions.Utils where

import System.Exit (ExitCode(ExitSuccess))
import System.Process (readCreateProcessWithExitCode, shell)
import Data.Map qualified as Map

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
import P2PWallet.Data.Core.TxBody
-- import P2PWallet.Plutus
import P2PWallet.Prelude

-- | Run an action. If successfull, do something with the results. If unsuccessfull, do something
-- with the error message. This is usefull for letting the frontend decide what to do based on
-- the results.
runAction :: (Text -> b) -> (c -> b) -> IO c -> IO b
runAction onAppErr onSuccess action = 
  handle (\(AppError err) -> return $ onAppErr err) $ fmap onSuccess action

-- | Run an action. If successfull, do something with the results. If unsuccessfull, pass
-- the error message to `Alert`. This is usefull for immediately displaying the error to the
-- user through the frontend.
runActionOrAlert :: (c -> AppEvent) -> IO c -> IO AppEvent
runActionOrAlert = runAction Alert

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

-- | Run the command without returning anything except an error message if an error occurs. The trailing newline is
-- dropped for the error message.
runCmd_ :: String -> IO ()
runCmd_ cmd =
    readCreateProcessWithExitCode (shell cmd) [] >>= \case
      (ExitSuccess,_,_) -> return ()
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
    go ((_,y):ys) !i = (i,y) : go ys (i+1)

-- | Remove the item from the list, and then re-index.
removeAction :: Int -> [(Int,a)] -> [(Int,a)]
removeAction idx xs = reIndex $ filter ((/=idx) . fst) xs

-- | Convert `Network` to the required flag for cardano-cli.
toNetworkFlag :: Network -> Text
toNetworkFlag Mainnet = "--mainnet"
toNetworkFlag Testnet = "--testnet-magic 1"

-- | Balance the inputs with the outputs by updating the changeOutput and subtracting off the
-- fee.
balanceTx :: TxBuilderModel -> TxBuilderModel
balanceTx tx@TxBuilderModel{..} =
    tx & #changeOutput .~ 
          -- Deleting elements can result in `newChange` being `def` which should signal an empty
          -- builder.
          (if newChange == def then Nothing else Just newChange)
       & #isBalanced .~ balanced
       & #isBuilt .~ False
  where
    -- The total deposit required from certificates.
    requiredDeposits :: Lovelace
    requiredDeposits = (flip . flip foldl') 0 userCertificates $ \acc (_,cert) ->
      case cert ^. #certificateAction of
        Registration -> acc + 2_000_000 -- 2 ADA must be paid.
        Deregistration -> acc - 2_000_000 -- 2 ADA must be returned.
        Delegation _ -> acc

    -- The amount of ADA and native assets available as change.
    (loves :: Lovelace, assets :: [NativeAsset]) =
      let (inLoves,inAssets) = 
            ( sum $ map (view #lovelace . snd) userInputs
            , concatMap (view #nativeAssets . snd) userInputs
            )
          (outLoves,outAssets) = 
            -- Increase the quantity of lovelace for each output by the count.
            ( sum $ map (\(_,UserOutput{count,lovelace}) -> fromIntegral count * lovelace) userOutputs
            -- Increase the quantity of each native asset by the count.
            , flip concatMap userOutputs $ \(_,UserOutput{count,nativeAssets}) -> 
                for nativeAssets $ \asset -> asset & #quantity %~ (fromIntegral count *)
            )
          inMap = Map.fromList $ map (\a -> (a ^. onChainName, a ^. #quantity)) inAssets
          outMap = Map.fromList $ map (\a -> (a ^. onChainName, negate $ a ^. #quantity)) outAssets
          bal = map (fromMaybe def . parseNativeAsset . \(name',q) -> show q <> " " <> name') 
              $ filter (\(_,q) -> q /= 0)
              $ Map.toList 
              $ Map.unionWith (+) inMap outMap
      in (inLoves - outLoves,bal)

    lovelaceChange :: Lovelace
    lovelaceChange = loves - fee - requiredDeposits

    newChange :: ChangeOutput
    newChange = ChangeOutput
      { paymentAddress = fromMaybe "" $ changeOutput ^? _Just % #paymentAddress
      , lovelace = lovelaceChange
      , nativeAssets = assets
      }

    -- Whether all assets are balanced.
    balanced :: Bool
    balanced = all ((>= 0) . view #quantity) assets && lovelaceChange >= 0

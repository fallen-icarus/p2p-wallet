module P2PWallet.Actions.Utils where

import System.Exit (ExitCode(ExitSuccess))
import System.Process (readCreateProcessWithExitCode, shell)

import P2PWallet.Data.AppModel
import P2PWallet.Data.Core.Internal
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

-- | Run the command without returning anything except an error message if an error occurs. The
-- trailing newline is dropped for the error message.
runCmd_ :: String -> IO ()
runCmd_ = void . runCmd

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

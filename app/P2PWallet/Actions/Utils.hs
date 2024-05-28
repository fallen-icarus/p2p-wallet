module P2PWallet.Actions.Utils where

import System.Exit (ExitCode(ExitSuccess))
import System.Process (readCreateProcessWithExitCode, shell)

import P2PWallet.Data.AppModel
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

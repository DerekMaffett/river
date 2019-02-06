module Logger where

import           Config
import           System.Exit
import           System.Log.Logger
import           System.IO
import qualified Control.Monad.Reader          as Reader


logDebug :: String -> Program ()
logDebug msg = do
    Config { logger } <- ask
    Reader.liftIO $ debugM logger ("DEBUG: " <> msg)

logNotice :: String -> Program ()
logNotice msg = do
    Config { logger } <- ask
    Reader.liftIO $ noticeM logger msg

logError :: String -> Program ()
logError msg = do
    Config { logger } <- ask
    Reader.liftIO $ criticalM logger msg
    Reader.liftIO $ exitFailure

-- Haskell prints only when a line is complete or the buffer is full.
-- Getting same-line input requires flushing the buffer early.
query :: String -> Program String
query question = Reader.liftIO $ putStr question >> hFlush stdout >> getLine

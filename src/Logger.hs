module Logger where

import           Config
import           Utils                          ( trim )
import           System.Exit
import           System.Log.Logger
import           System.IO
import           System.Console.Haskeline
import           Data.List                      ( isPrefixOf )
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

-----------------------------------

query :: String -> Program String
query question = Reader.liftIO $ runInputT defaultSettings $ do
    maybeInput <- getInputLine question
    case maybeInput of
        Nothing    -> return ""
        Just input -> return input

queryWithSuggestions :: String -> [String] -> Program String
queryWithSuggestions question wordList =
    Reader.liftIO $ runInputT (setComplete completionFn defaultSettings) $ do
        maybeInput <- getInputLine question
        case maybeInput of
            Nothing    -> return ""
            Just input -> return . trim $ input
  where
    searchFn str = simpleCompletion <$> filter (str `isPrefixOf`) wordList
    completionFn = completeWord Nothing "\t" $ return . searchFn

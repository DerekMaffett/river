module Logger where

import           Config
import           Utils                          ( trim )
import           System.Exit
import           System.Log.Logger
import           System.IO
import           System.Console.Haskeline
import           Data.List                      ( isPrefixOf )
import qualified Control.Monad.Reader          as Reader

logDebug :: (ContainsLogger a) => String -> Reader.ReaderT a IO ()
logDebug msg = do
    logger <- getLoggerFromContext <$> Reader.ask
    Reader.liftIO $ debugM logger ("DEBUG: " <> msg)

logNotice :: (ContainsLogger a) => String -> Reader.ReaderT a IO ()
logNotice msg = do
    logger <- getLoggerFromContext <$> Reader.ask
    Reader.liftIO $ noticeM logger msg

logError :: (ContainsLogger a) => String -> Reader.ReaderT a IO ()
logError msg = do
    logger <- getLoggerFromContext <$> Reader.ask
    Reader.liftIO $ criticalM logger msg
    Reader.liftIO $ exitFailure

initializeLogger :: Bool -> IO String
initializeLogger useDebugLogger = do
    updateGlobalLogger "logger" $ setLevel loggerLevel
    return "logger"
    where loggerLevel = if useDebugLogger then DEBUG else NOTICE

-----------------------------------

queryMasked :: String -> Reader.ReaderT a IO String
queryMasked question = runInputT defaultSettings $ do
    maybeInput <- getPassword (Just '*') question
    case maybeInput of
        Nothing    -> return ""
        Just input -> return input


query :: String -> Reader.ReaderT a IO String
query question = do
    runInputT defaultSettings $ do
        maybeInput <- getInputLine question
        case maybeInput of
            Nothing    -> return ""
            Just input -> return input

queryYesNo :: String -> Reader.ReaderT a IO Bool
queryYesNo question = Reader.liftIO go
  where
    go = runInputT defaultSettings $ do
        maybeInput <- getInputLine (question <> " (y/n) ")
        case maybeInput of
            Nothing    -> return False
            Just input -> case input of
                "y" -> return True
                "Y" -> return True
                "n" -> return False
                "N" -> return False
                _   -> return False

queryWithSuggestions :: String -> [String] -> Reader.ReaderT a IO String
queryWithSuggestions question wordList = do
    runInputT (setComplete completionFn defaultSettings) $ do
        maybeInput <- getInputLine question
        case maybeInput of
            Nothing    -> return ""
            Just input -> return . trim $ input
  where
    searchFn str = simpleCompletion <$> filter (str `isPrefixOf`) wordList
    completionFn = completeWord Nothing "\t" $ return . searchFn


queryWithLimitedSuggestions :: String -> [String] -> Reader.ReaderT a IO String
queryWithLimitedSuggestions question wordList = do
    answer <- queryWithSuggestions question wordList
    if answer `elem` wordList
        then return answer
        else do
            Reader.liftIO
                (  putStrLn
                $  answer
                <> " is not a valid choice. Valid choices: "
                <> show wordList
                )
            queryWithLimitedSuggestions question wordList

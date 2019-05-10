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

-----------------------------------

query :: String -> Program String
query = Reader.liftIO <$> query'

queryMasked' :: String -> IO String
queryMasked' question = runInputT defaultSettings $ do
    maybeInput <- getPassword (Just '*') question
    case maybeInput of
        Nothing    -> return ""
        Just input -> return input


query' :: String -> IO String
query' question = runInputT defaultSettings $ do
    maybeInput <- getInputLine question
    case maybeInput of
        Nothing    -> return ""
        Just input -> return input

queryWithSuggestions :: String -> [String] -> Program String
queryWithSuggestions question =
    Reader.liftIO <$> queryWithSuggestions' question

queryWithSuggestions' :: String -> [String] -> IO String
queryWithSuggestions' question wordList =
    runInputT (setComplete completionFn defaultSettings) $ do
        maybeInput <- getInputLine question
        case maybeInput of
            Nothing    -> return ""
            Just input -> return . trim $ input
  where
    searchFn str = simpleCompletion <$> filter (str `isPrefixOf`) wordList
    completionFn = completeWord Nothing "\t" $ return . searchFn

queryWithLimitedSuggestions' :: String -> [String] -> IO String
queryWithLimitedSuggestions' question wordList = do
    answer <- queryWithSuggestions' question wordList
    if answer `elem` wordList
        then return answer
        else do
            putStrLn
                $  answer
                <> " is not a valid choice. Valid choices: "
                <> show wordList
            queryWithLimitedSuggestions' question wordList

module IntegrationSpecs where

import           Test.Hspec
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe                     ( fromJust )
import           System.Process.Typed
import           System.IO
import           GHC.Generics
import           Control.Monad
import           Control.Exception.Safe
import           Data.Aeson                    as A
import           Data.Text                     as T


getContentsUntil expectationFn maxWait h = do
    runWithAccum ""
  where
    runWithAccum accum = do
        contents <- getCurrentContentsWithAccum accum
        if (expectationFn $ T.pack contents)
            then return contents
            else do
                inputAvailable <-
                    handleAny (const $ return False) $ hWaitForInput h maxWait
                if inputAvailable
                    then do
                        runWithAccum contents
                    else do
                        return contents
    getCurrentContentsWithAccum accum = do
        hasMoreContent <- hReady h
        if hasMoreContent
            then do
                char <- hGetChar h
                getCurrentContentsWithAccum $ accum <> [char]
            else do
                return accum




hHasPrompt process expectedText = do
    maybeExitCode <- getExitCode process
    case maybeExitCode of
        Nothing -> do
            content <- getContentsUntil expectedTextIsPresent
                                        2000
                                        (getStdout process)
            putStr content
            shouldSatisfy (T.pack content) expectedTextIsPresent
        Just exitCode -> do
            expectationFailure "failed"
    where expectedTextIsPresent = T.isInfixOf expectedText

hInput inHandle text = do
    hPutStrLn inHandle text
    putStrLn text
    hFlush inHandle

data BitbucketJiraAuth = BitbucketJiraAuth
  { bitbucketUsername :: String
  , bitbucketPassword :: String
  , jiraUsername :: String
  , jiraPassword :: String
  } deriving (Generic, A.FromJSON)

spec = describe "Integration Specs" $ do
    describe "Init" $ do
        it "should initialize the configuration files" $ do
            authInfo <-
                fromJust <$> (A.decodeFileStrict' ".integration-test-auth.json") :: IO
                    (Maybe BitbucketJiraAuth)
            let config = setStdin createPipe $ setStdout createPipe $ setStderr
                    createPipe
                    "river init"
            p <- startProcess config
            let inHandle  = getStdin p
            let hasPrompt = hHasPrompt p
            let input     = hInput inHandle
            hasPrompt "Select your repo manager (tab for suggestions): "
            input "invalid-manager"
            hasPrompt "invalid-manager is not a valid choice"
            input "bitbucket"
            hasPrompt "Repo name"
            input "river"
            hasPrompt "Repo org"
            input "DerekMaffett"
            hasPrompt "Bitbucket username"
            input "der"
            hasPrompt "Bitbucket password"
            input "jkjk"
            hasPrompt "Would you like to add yourself as a default reviewer?"
            stopProcess p
            checkExitCode p

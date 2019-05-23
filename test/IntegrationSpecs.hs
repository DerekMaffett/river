module IntegrationSpecs where

import           Test.Hspec
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe                     ( fromJust )
import           System.Process.Typed
import           System.IO
import qualified System.Directory              as Dir
import           GHC.Generics
import           Control.Monad
import           Control.Exception.Safe
import           Data.Aeson                    as A
import           Data.Text                     as T


getContentsUntil expectationFn maxWait h eh = do
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
    content <- getContentsUntil expectedTextIsPresent
                                2000
                                (getStdout process)
                                (getStderr process)
    putStr content
    shouldSatisfy (T.pack content) expectedTextIsPresent
    where expectedTextIsPresent = T.isInfixOf expectedText

hInput process text = do
    hPutStrLn inHandle text
    putStrLn text
    hFlush inHandle
    where inHandle = getStdin process

data BitbucketJiraAuth = BitbucketJiraAuth
  { bitbucketUsername :: String
  , bitbucketPassword :: String
  , jiraUsername :: String
  , jiraPassword :: String
  } deriving (Generic, A.FromJSON)

bitbucketJiraFlow authInfo =
    [ ("Select your repo manager"             , "invalid-manager")
    , ("invalid-manager is not a valid choice", "bitbucket")
    , ("Repo name"                            , "river-bitbucket")
    , ("Repo org"                             , "DerekMaffett")
    , ("Bitbucket username"                   , bitbucketUsername authInfo)
    , ("Bitbucket password"                   , bitbucketPassword authInfo)
    , ("Would you like to add yourself as a default reviewer?", "y")
    , ("Select your project manager"          , "invalid-manager")
    , ("invalid-manager is not a valid choice", "jira")
    , ("Jira project key"                     , "TEST")
    , ("Jira domain name"                     , "bitbucket-river-test")
    , ("Jira username"                        , jiraUsername authInfo)
    , ("Jira password"                        , jiraPassword authInfo)
    , ("example task prior to starting"       , "1")
    , ("Select correct transition for starting a task", "In Progress")
    , ("example task prior to starting a PR"  , "2")
    , ("Select correct transition for starting a PR", "Code Review")
    , ("example task prior to merging a PR"   , "3")
    , ("Select correct transition for merging a PR", "Done")
    , ("Main git branch"                      , "master")
    , ("Remote origin name"                   , "origin")
    , ("Bug categories"                       , "type-error gremlins")
    ]

createInteraction p (prompt, response) = do
    hHasPrompt p prompt
    hInput p response

spec = describe "Integration Specs" $ do
    describe "Init" $ do
        it "should initialize the configuration files" $ do
            Dir.removePathForcibly ".river.env.json"
            Dir.removePathForcibly ".river.json"
            authInfo <-
                fromJust
                    <$> (A.decodeFileStrict' ".integration-test-auth.json" :: IO
                              (Maybe BitbucketJiraAuth)
                        )
            p <- runCommand "river init"
            mapM_ (createInteraction p) (bitbucketJiraFlow authInfo)
            checkExitCode p

            p <- runCommand "river begin -q \"TEST ISSUE\""
            hHasPrompt p "Input branch name: feature/TEST-"
            hInput p "test-branch"
            hHasPrompt p "jkjk"
            checkExitCode p
  where
    runCommand command = do
        let
            config = setStdin createPipe $ setStdout createPipe $ setStderr
                inherit
                command
        startProcess config

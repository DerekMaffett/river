module IntegrationSpecs where

import           Test.Hspec
import qualified Data.ByteString.Lazy.Char8    as B
import           System.Process.Typed
import           System.IO
import           Control.Monad
import           Control.Exception.Safe
import           Data.Aeson
import           Data.Text                     as T

getCurrentContents handle = getCurrentContentsWithAccum ""
  where
    getCurrentContentsWithAccum accum = do
        hasMoreContent <- hReady handle
        if hasMoreContent
            then do
                char <- hGetChar handle
                getCurrentContentsWithAccum $ accum <> [char]
            else do
                return accum

hHasPrompt process expectedText = do
    maybeExitCode <- getExitCode process
    case maybeExitCode of
        Just exitCode -> do
            hWaitForInput (getStdout process) 1000
            content <- getCurrentContents (getStdout process)
            putStr content
            shouldSatisfy (T.pack content) (T.isInfixOf expectedText)
        Nothing -> expectationFailure "failed"

hInput inHandle text = do
    hPutStrLn inHandle text
    putStrLn text
    hFlush inHandle


spec = describe "Integration Specs" $ do
    describe "Init" $ do
        it "should initialize the configuration files" $ do
            let config = setStdin createPipe $ setStdout createPipe $ setStderr
                    closed
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
            input "derekmaffett-orm"
            hasPrompt "Bitbucket password"
            input ""
            hasPrompt "Would you like to add yourself as a default reviewer?"
            handle
                    (\(ExitCodeException _ _ _ stderr) -> do
                        throwString (B.unpack stderr)
                    )
                $ checkExitCode p




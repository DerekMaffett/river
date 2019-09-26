module Api.Jira.Get
    ( getIssue
    )
where

import           Api.Jira.Base
import qualified Config
import           Control.Exception.Safe
import qualified Control.Monad.Reader   as Reader
import           Data.Aeson
import qualified Data.ByteString.Char8  as B
import           Data.Maybe
import           Data.Text
import           GHC.Generics
import qualified Logger
import           Network.HTTP.Req
import           Types                  (Issue (..))

getIssue
    :: (Config.ContainsLogger a)
    => Config.JiraConfig
    -> String
    -> Reader.ReaderT a IO (Maybe Issue)
getIssue settings issueKey = do
    issueOrErr <- tryAny $ do
        runReq defaultHttpConfig $ do
            response <- req GET
                            url
                            NoReqBody
                            jsonResponse
                            (authOptions <> urlOptions)
            return (responseBody response :: Issue)
    case issueOrErr of
        Left err -> do
            Logger.logDebug $ "Failed to fetch issue \"" <> issueKey <> "\""
            Logger.logDebug $ displayException err
            return Nothing
        Right issue -> return $ Just issue
  where
    authOptions = generateAuthOptions settings
    url         = (getBaseUrl settings) /: "issue" /: (pack issueKey)
    urlOptions  = "expand" =: ("transitions" :: Text)

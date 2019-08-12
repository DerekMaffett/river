module Api.Jira.Get
    ( getIssue
    )
where

import           Api.Jira.Base
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Char8         as B
import           Data.Default.Class
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Req
import qualified Logger
import qualified Config
import           Types                          ( Issue(..) )
import qualified Control.Monad.Reader          as Reader
import           Control.Exception.Safe

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

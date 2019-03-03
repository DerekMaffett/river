module Api.Jira.Assign
    ( assignIssue
    )
where

import           Api.Jira.Base
import           Data.Aeson
import qualified Data.ByteString.Char8         as B
import           Data.Default.Class
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Req
import           Config                         ( Program
                                                , JiraConfig(..)
                                                )
import qualified Types

assignIssue :: JiraConfig -> String -> Program ()
assignIssue settings issueKey = do
    url         <- getUrl
    authOptions <- generateAuthOptions settings
    runReq def $ do
        req PUT url (ReqBodyJson $ requestBody) ignoreResponse authOptions
        return ()
  where
    getUrl = do
        baseUrl <- getBaseUrl settings
        return $ baseUrl /: "issue" /: (pack issueKey)
    requestBody = object [("fields" .= object [("assignee" .= user settings)])]

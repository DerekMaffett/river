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
import           Config
import qualified Types

assignIssue :: String -> Program ()
assignIssue issueKey = do
    Config { jiraUser } <- ask
    authOptions         <- generateAuthOptions
    runReq def $ do
        req PUT
            url
            (ReqBodyJson $ requestBody jiraUser)
            ignoreResponse
            authOptions
        return ()
  where
    url = baseUrl /: "issue" /: (pack issueKey)
    requestBody jiraUser =
        object [("fields" .= object [("assignee" .= jiraUser)])]

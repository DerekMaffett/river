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
    runReq defaultHttpConfig $ do
        user <-
            responseBody
                <$> req GET myselfUrl NoReqBody jsonResponse authOptions
        req PUT url (ReqBodyJson $ requestBody user) ignoreResponse authOptions
        return ()
  where
    baseUrl     = getBaseUrl settings
    url         = baseUrl /: "issue" /: (pack issueKey)
    myselfUrl   = baseUrl /: "myself"
    authOptions = generateAuthOptions settings
    requestBody user =
        object [("fields" .= object [("assignee" .= (user :: Types.JiraUser))])]

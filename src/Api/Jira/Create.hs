module Api.Jira.Create
    ( createIssue
    )
where

import           Api.Jira.Base
import           Data.Aeson
import           Data.Maybe
import           Data.Default.Class
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Req
import           Config
import qualified Types                          ( IssueType(..)
                                                , Issue(..)
                                                )


data Response = Response
  { key :: String
  } deriving (Show, Generic, FromJSON)

createIssue :: String -> Types.IssueType -> Program String
createIssue summary issueType = do
    Config { projectKey } <- ask
    url                   <- getUrl
    authOptions           <- generateAuthOptions
    runReq def $ do
        response <- req POST
                        url
                        (ReqBodyJson $ request projectKey)
                        jsonResponse
                        authOptions
        return $ key . responseBody $ response
  where
    getUrl = do
        baseUrl <- getBaseUrl
        return $ baseUrl /: "issue"
    request projectKey = object
        [ "fields"
              .= (object
                     [ "summary" .= summary
                     , "project" .= (object ["key" .= projectKey])
                     , "issuetype"
                         .= (object ["name" .= (jiraIssueType :: String)])
                     ]
                 )
        ]
    jiraIssueType = case issueType of
        Types.Bug  -> "Bug"
        Types.Task -> "Task"

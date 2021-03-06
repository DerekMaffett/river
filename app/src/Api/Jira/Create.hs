module Api.Jira.Create
    ( createIssue
    )
where

import           Api.Jira.Base
import           Data.Aeson
import           Data.Default.Class
import           GHC.Generics
import           Network.HTTP.Req
import           Config
import qualified Types                          ( IssueType(..) )


data Response = Response
  { key :: String
  } deriving (Show, Generic, FromJSON)

createIssue :: JiraConfig -> String -> Types.IssueType -> Program String
createIssue settings summary issueType = do
    runReq def $ do
        response <- req POST
                        url
                        (ReqBodyJson $ request)
                        jsonResponse
                        authOptions
        return $ key . responseBody $ response
  where
    projectKey  = Config.projectKey settings
    authOptions = generateAuthOptions settings
    url         = (getBaseUrl settings) /: "issue"
    request     = object
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

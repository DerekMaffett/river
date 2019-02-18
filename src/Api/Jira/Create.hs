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
import qualified Types                          ( IssueTypeInternal(..)
                                                , Issue(..)
                                                )


data Response = Response
  { key :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

createIssue :: String -> Types.IssueTypeInternal -> Program String
createIssue summary issueType = do
    Config { projectKey } <- ask
    url                   <- getUrl
    authOptions           <- generateAuthOptions
    runReq def $ do
        response <- req POST
                        url
                        (ReqBodyJson $ request projectKey summary issueType)
                        jsonResponse
                        authOptions
        return $ (key :: Response -> String) . responseBody $ response
  where
    getUrl = do
        baseUrl <- getBaseUrl
        return $ baseUrl /: "issue"
    request projectKey summary issueType = object
        [ "fields"
              .= (object
                     [ "summary" .= summary
                     , "project" .= (object ["key" .= projectKey])
                     , "issuetype" .= (object ["name" .= show issueType])
                     ]
                 )
        ]

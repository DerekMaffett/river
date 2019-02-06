module Api.Jira.Myself
    ( getMyself
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
import           Config
import qualified Types

getMyself jiraEmail jiraToken = runReq def $ do
    response <- req GET
                    url
                    NoReqBody
                    jsonResponse
                    (_generateAuthOptions jiraEmail jiraToken)
    return (responseBody response :: Maybe Types.JiraUser)
    where url = baseUrl /: "myself"

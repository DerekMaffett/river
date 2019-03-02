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

getMyself jiraDomain (BasicAuthCredentials username password) = runReq def $ do
    response <- req GET
                    (_getBaseUrl jiraDomain /: "myself")
                    NoReqBody
                    jsonResponse
                    (_generateAuthOptions username password)
    return (responseBody response :: Maybe Types.JiraUser)

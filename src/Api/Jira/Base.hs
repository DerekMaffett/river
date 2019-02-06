module Api.Jira.Base
    ( host
    , baseUrl
    , generateAuthOptions
    , _generateAuthOptions
    )
where

import qualified Data.ByteString.Char8         as B
import           Network.HTTP.Req
import           Config

-- TODO: Add host name to config since this can't be assumed anymore
host = undefined
baseUrl = host /: "rest" /: "api" /: "latest"

generateAuthOptions = do
    Config { jiraEmail, jiraToken } <- ask
    return $ _generateAuthOptions jiraEmail jiraToken

_generateAuthOptions jiraEmail jiraToken =
    basicAuth (B.pack jiraEmail) (B.pack jiraToken)

module Api.Jira.Base
    ( getBaseUrl
    , _getBaseUrl
    , generateAuthOptions
    , _generateAuthOptions
    )
where

import           Data.Text                     as T
import qualified Data.ByteString.Char8         as B
import           Network.HTTP.Req
import           Config

getBaseUrl = do
    Config { jiraDomain } <- ask
    return $ _getBaseUrl jiraDomain

_getBaseUrl jiraDomain =
    https (T.pack jiraDomain <> ".atlassian.net") /: "rest" /: "api" /: "latest"

generateAuthOptions = do
    Config { jiraEmail, jiraToken } <- ask
    return $ _generateAuthOptions jiraEmail jiraToken

_generateAuthOptions jiraEmail jiraToken =
    basicAuth (B.pack jiraEmail) (B.pack jiraToken)

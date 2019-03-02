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

getBaseUrl settings = do
    return $ _getBaseUrl (domainName settings)

_getBaseUrl jiraDomain =
    https (T.pack jiraDomain <> ".atlassian.net") /: "rest" /: "api" /: "latest"

generateAuthOptions (JiraConfig { auth }) = do
    let (BasicAuthCredentials username password) = auth
    return $ _generateAuthOptions username password

_generateAuthOptions username password =
    basicAuth (B.pack username) (B.pack password)

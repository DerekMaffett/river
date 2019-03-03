module Api.Jira.Base
    ( getBaseUrl
    , generateAuthOptions
    )
where

import           Data.Text                     as T
import qualified Data.ByteString.Char8         as B
import           Network.HTTP.Req
import           Config

getBaseUrl settings = do
    return
        $  https (T.pack (domainName settings) <> ".atlassian.net")
        /: "rest"
        /: "api"
        /: "latest"

generateAuthOptions (JiraConfig { auth }) = do
    let (BasicAuthCredentials username password) = auth
    return $ basicAuth (B.pack username) (B.pack password)

module Api.Jira.Get
    ( getIssue
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
import           Types                          ( Issue(..) )

getIssue issueKey = do
    url         <- getUrl
    authOptions <- generateAuthOptions
    runReq def $ do
        response <- req GET
                        url
                        NoReqBody
                        jsonResponse
                        (authOptions <> urlOptions)
        return (responseBody response :: Maybe Issue)
  where
    getUrl = do
        baseUrl <- getBaseUrl
        return $ baseUrl /: "issue" /: (pack issueKey)
    urlOptions = "expand" =: ("transitions" :: Text)

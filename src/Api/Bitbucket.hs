module Api.Bitbucket
    ( createPullRequest
    , getMyself
    )
where

import qualified Types                         as Types
import           Config
import           Data.Default.Class
import           Data.Aeson
import qualified Data.Text                     as T
import           GHC.Generics
import           Debug.Trace
import           Network.HTTP.Req
import qualified Data.ByteString.Char8         as B

host = https "api.bitbucket.org"

baseUrl = host /: "2.0"

-- generateAuthOptions = do
--     Config { bitbucketUsername, bitbucketPassword } <- ask
--     return $ _generateAuthOptions bitbucketUsername bitbucketPassword


_generateAuthOptions username password =
    basicAuth (B.pack username) (B.pack password)


createPullRequest :: BitbucketConfig -> Types.Issue -> String -> Program String
createPullRequest _ _ _ = return "hi"
-- createPullRequest issue branchName = do
--     Config { repoName, repoOrg } <- ask
--     authOptions                  <- generateAuthOptions
--     body                         <- getBody
--     runReq def $ do
--         response <- req
--             POST
--             (  baseUrl
--             /: "repositories"
--             /: (T.pack repoOrg)
--             /: (T.pack repoName)
--             /: "pullrequests"
--             )
--             (ReqBodyJson body)
--             jsonResponse
--             authOptions
--         return $ case (responseBody response :: Response) of
--             Response link -> link
--   where
--     getBody = do
--         Config { defaultReviewers, bitbucketUser, workingBranch } <- ask
--         let reviewers = filter (/= bitbucketUser) defaultReviewers
--         return $ object
--             [ ("title" .= branchName)
--             , ("description" .= description)
--             , ("reviewers" .= reviewers)
--             , ("close_source_branch" .= True)
--             , (  "source"
--               .= object [("branch" .= object [("name" .= branchName)])]
--               )
--             , (  "destination"
--               .= object [("branch" .= object [("name" .= workingBranch)])]
--               )
--             ]
--
--     description = case Types.description issue of
--         Nothing   -> Types.summary issue
--         Just desc -> desc
--
--
-- data Response = Response String
--
-- instance FromJSON Response where
--   parseJSON = withObject "object" $ \o -> Response <$> (o .: "links" >>= (.: "html") >>= (.: "href"))
--

getMyself :: BasicAuthCredentials -> IO (Maybe Types.BitbucketUser)
getMyself (BasicAuthCredentials username password) = do
    runReq def $ do
        response <- req GET
                        (baseUrl /: "user")
                        NoReqBody
                        jsonResponse
                        authOptions
        return (responseBody response :: Maybe Types.BitbucketUser)
    where authOptions = _generateAuthOptions username password

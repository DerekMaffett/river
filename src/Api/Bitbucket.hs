module Api.Bitbucket
    ( createPullRequest
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

generateAuthOptions (BasicAuthCredentials username password) =
    basicAuth (B.pack username) (B.pack password)

createPullRequest :: BitbucketConfig -> Types.Issue -> String -> Program String
createPullRequest (BitbucketConfig { defaultReviewers, repoName, repoOrg, auth }) issue branchName
    = do
        Config { workingBranch } <- ask
        runReq def $ do
            user <-
                responseBody
                    <$> req GET
                            (baseUrl /: "user")
                            NoReqBody
                            jsonResponse
                            authOptions
            response <- req
                POST
                (  baseUrl
                /: "repositories"
                /: (T.pack repoOrg)
                /: (T.pack repoName)
                /: "pullrequests"
                )
                (ReqBodyJson $ getBody user workingBranch)
                jsonResponse
                authOptions
            return $ case (responseBody response :: Response) of
                Response link -> link
  where
    authOptions = generateAuthOptions auth
    reviewers user = filter (/= (user :: Types.BitbucketUser)) defaultReviewers
    getBody user workingBranch = object
        [ ("title" .= branchName)
        , ("description" .= description)
        , ("reviewers" .= reviewers user)
        , ("close_source_branch" .= True)
        , ("source" .= object [("branch" .= object [("name" .= branchName)])])
        , (  "destination"
          .= object [("branch" .= object [("name" .= workingBranch)])]
          )
        ]

    description = case Types.description issue of
        Nothing   -> Types.summary issue
        Just desc -> desc


data Response = Response String

instance FromJSON Response where
  parseJSON = withObject "object" $ \o -> Response <$> (o .: "links" >>= (.: "html") >>= (.: "href"))

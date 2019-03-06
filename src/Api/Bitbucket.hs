module Api.Bitbucket
    ( createPullRequest
    , mergePullRequest
    )
where

import qualified Types                         as Types
import           Config
import           Data.Default.Class
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           GHC.Generics
import           Debug.Trace
import           Network.HTTP.Req
import qualified Data.ByteString.Char8         as B

host = https "api.bitbucket.org"

baseUrl = host /: "2.0"

getPullRequestsUrl repoOrg repoName =
    (  baseUrl
    /: "repositories"
    /: (T.pack repoOrg)
    /: (T.pack repoName)
    /: "pullrequests"
    )

generateAuthOptions (BasicAuthCredentials username password) =
    basicAuth (B.pack username) (B.pack password)

data Link = Link String

instance FromJSON Link where
  parseJSON = withObject "object" $ \o -> Link <$> (o .: "links" >>= (.: "html") >>= (.: "href"))

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
            response <- req POST
                            (getPullRequestsUrl repoOrg repoName)
                            (ReqBodyJson $ getBody user workingBranch)
                            jsonResponse
                            authOptions
            return $ case (responseBody response :: Link) of
                Link link -> link
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

data PrId = PrId Integer

instance FromJSON PrId where
  parseJSON = withObject "object" $ \o -> do
    entriesArray <- o .: "values"
    matchingPR <- withArray "paginated entries" parseEntries entriesArray
    id <- matchingPR .: "id"
    return $ PrId id
    where
      parseEntries entries = case V.length entries of
        1 -> withObject "pull request object" return (V.head entries)
        n -> fail $ (show n) <> " open pull requests found under the current branch. Should be one."


mergePullRequest :: BitbucketConfig -> String -> Program Integer
mergePullRequest (BitbucketConfig { defaultReviewers, repoName, repoOrg, auth }) branchName
    = do
        Config { workingBranch } <- ask
        prId                     <- runReq def $ do
            responseBody <$> req GET
                                 (getPullRequestsUrl repoOrg repoName)
                                 NoReqBody
                                 jsonResponse
                                 (authOptions <> urlOptions workingBranch)
            -- req POST (pullRequestsUrl /: "merge")
        case prId of
            PrId id -> return id
  where
    authOptions = generateAuthOptions auth
    urlOptions workingBranch =
        "q"
            =: (  "source.branch.name="
               <> bitbucketQueryArg branchName
               <> " AND destination.branch.name="
               <> bitbucketQueryArg workingBranch
               <> " AND state="
               <> bitbucketQueryArg "OPEN"
               )
    bitbucketQueryArg arg = "\"" <> T.pack arg <> "\""

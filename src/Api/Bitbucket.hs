module Api.Bitbucket
    ( createPullRequest
    , mergePullRequest
    , getSelf
    )
where

import qualified Types                         as Types
import           Config
import           Data.Default.Class
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Control.Monad
import qualified Control.Monad.Reader          as Reader
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
    parseJSON = withObject "object"
        $ \o -> Link <$> (o .: "links" >>= (.: "html") >>= (.: "href"))

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
            response <-
                responseBody
                    <$> req POST
                            (getPullRequestsUrl repoOrg repoName)
                            (ReqBodyJson $ getBody user workingBranch)
                            jsonResponse
                            authOptions
            return $ case response of
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
        matchingPR   <- withArray "paginated entries" parseEntries entriesArray
        id           <- matchingPR .: "id"
        return $ PrId id
      where
        parseEntries entries = case V.length entries of
            1 -> withObject "pull request object" return (V.head entries)
            n ->
                fail
                    $ (show n)
                    <> " open pull requests found from the current branch to the working branch. There should be 1."


mergePullRequest :: BitbucketConfig -> String -> Program ()
mergePullRequest (BitbucketConfig { defaultReviewers, repoName, repoOrg, auth }) branchName
    = do
        Config { workingBranch } <- ask
        runReq def $ do
            prId <- responseBody <$> req
                GET
                pullRequestsUrl
                NoReqBody
                jsonResponse
                (authOptions <> urlOptions workingBranch)
            let (PrId id) = prId
            void $ req POST
                       (pullRequestsUrl /: (T.pack . show $ id) /: "merge")
                       (ReqBodyJson $ mergePullRequestBody)
                       ignoreResponse
                       authOptions
  where
    pullRequestsUrl = getPullRequestsUrl repoOrg repoName
    authOptions     = generateAuthOptions auth
    urlOptions workingBranch =
        "q"
            =: (  "source.branch.name="
               <> bitbucketQueryArg branchName
               <> " AND destination.branch.name="
               <> bitbucketQueryArg workingBranch
               <> " AND state="
               <> bitbucketQueryArg "OPEN"
               )
    mergePullRequestBody = object
        [ ("close_source_branch" .= True)
        , ("merge_strategy" .= ("merge_commit" :: T.Text))
        ]


bitbucketQueryArg arg = "\"" <> T.pack arg <> "\""

getSelf auth = do
    res <- runReq def $ do
        responseBody
            <$> req GET (baseUrl /: "user") NoReqBody jsonResponse authOptions
    return res
    where authOptions = generateAuthOptions auth

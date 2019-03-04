module Api.Github
    ( createPullRequest
    )
where

import qualified Types                         as Types
import           Config
import           Data.GraphQL.AST
import           Data.GraphQL.Encoder
import           Data.Default.Class
import           Data.Aeson                    as A
import qualified Data.Text                     as T
import           GHC.Generics
import           Debug.Trace
import           Network.HTTP.Req
import qualified Data.ByteString.Char8         as B

graphqlUrl = https "api.github.com" /: "graphql"

generateAuthOptions (BasicAuthCredentials username password) =
    basicAuth (B.pack username) (B.pack password)

data RepoId = RepoId T.Text deriving (Show)

instance FromJSON RepoId where
    parseJSON = withObject "object" $ \o -> do
        repoId <- o .: "data" >>= (.: "repository") >>= (.: "id")
        return $ RepoId repoId


createPullRequest (GithubConfig { repoOrg, repoName, auth }) issue branchName =
    do
        let authOptions = generateAuthOptions auth
        let
            getRepoIdQuery =
                (operationDefinition $ Query $ Node
                    ""
                    []
                    []
                    [ SelectionField $ Field
                          ""
                          "repository"
                          [ Argument
                              "owner"
                              (ValueString . StringValue . T.pack $ repoOrg)
                          , Argument
                              "name"
                              (ValueString . StringValue . T.pack $ repoName)
                          ]
                          []
                          [SelectionField $ Field "" "id" [] [] []]
                    ]
                )
        let body graphqlQuery = object [("query" .= graphqlQuery)]
        let headers = header (B.pack "User-Agent") (B.pack repoOrg)
        runReq def $ do
            repoId <- responseBody <$> req
                POST
                graphqlUrl
                (ReqBodyJson $ body getRepoIdQuery)
                jsonResponse
                (authOptions <> headers)
            return (repoId :: RepoId)

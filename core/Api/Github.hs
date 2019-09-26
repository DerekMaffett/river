module Api.Github
    ( createPullRequest
    , mergePullRequest
    )
where

import qualified Types                         as Types
import           Config
import           Language.GraphQL.AST
import           Language.GraphQL.Encoder
import           Data.Aeson                    as A
import qualified Data.Text                     as T
import           Data.Maybe
import qualified Data.Vector                   as V
import           Control.Monad
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

data Permalink = Permalink String deriving (Show)

instance FromJSON Permalink where
    parseJSON = withObject "object" $ \o -> do
        errors <- o .:? "errors"
        case (errors) of
            Nothing -> do
                permalink <-
                    o
                    .:  "data"
                    >>= (.: "createPullRequest")
                    >>= (.: "pullRequest")
                    >>= (.: "permalink")
                return $ Permalink permalink
            Just errors -> withArray "array of errors" parseErrorsArray errors
      where
        parseError = withObject "object" (.: "message")
        parseErrorsArray errorsArray = do
            errorsArray <- mapM parseError (V.toList errorsArray)
            fail $ unlines errorsArray


createPullRequest (GithubConfig { repoOrg, repoName, auth }) issue branchName =
    do
        Config { workingBranch } <- ask
        let authOptions = generateAuthOptions auth
        -- let 
            -- getRepoIdQuery =
            --     (OperationDefinition $ Query $ Node
            --         Nothing
            --         []
            --         []
            --         [ SelectionField $ Field
            --               ""
            --               "repository"
            --               [ Argument
            --                   "owner"
            --                   (ValueString . StringValue . T.pack $ repoOrg)
            --               , Argument
            --                   "name"
            --                   (ValueString . StringValue . T.pack $ repoName)
            --               ]
            --               []
            --               [SelectionField $ Field "" "id" [] [] []]
            --         ]
            --     )
        -- let
            -- createPullRequestQuery (RepoId repoId) =
            --     (OperationDefinition Mutation Nothing Node
            --         Nothing
            --         []
            --         []
            --         [ SelectionField $ Field
            --               ""
            --               "createPullRequest"
            --               [ Argument
            --                     "input"
            --                     ( ValueObject
            --                     . ObjectValue
            --                     $ [ ObjectField "baseRefName"
            --                       $ ValueString
            --                       . StringValue
            --                       . T.pack
            --                       $ workingBranch
            --                       , ObjectField "headRefName"
            --                       $ ValueString
            --                       . StringValue
            --                       . T.pack
            --                       $ branchName
            --                       , ObjectField "title"
            --                       $ ValueString
            --                       . StringValue
            --                       . T.pack
            --                       $ branchName
            --                       , ObjectField "repositoryId"
            --                       $ ValueString
            --                       . StringValue
            --                       $ repoId
            --                       , ObjectField "body"
            --                       $ ValueString
            --                       . StringValue
            --                       . T.pack
            --                       $ fromMaybe (Types.summary issue)
            --                                   (Types.description issue)
            --                       ]
            --                     )
            --               ]
            --               []
            --               [ SelectionField $ Field
            --                     ""
            --                     "pullRequest"
            --                     []
            --                     []
            --                     [SelectionField $ Field "" "permalink" [] [] []]
            --               ]
            --         ]
            --     )
        let body graphqlQuery = object [("query" .= graphqlQuery)]
        let headers = header (B.pack "User-Agent") (B.pack repoOrg)
        runReq defaultHttpConfig $ do
            (repoId :: RepoId) <- responseBody <$> req
                POST
                graphqlUrl
                (ReqBodyJson $ body ("hi" :: T.Text))
                jsonResponse
                (authOptions <> headers)
            response <- req POST
                            graphqlUrl
                            (ReqBodyJson $ body ("hi" :: T.Text))
                            jsonResponse
                            (authOptions <> headers)
            let (Permalink link) = (responseBody response :: Permalink)
            return link

data PrId = PrId T.Text

instance FromJSON PrId where
    parseJSON = withObject "object" $ \o -> do
        nodes <-
            o
            .:  "data"
            >>= (.: "repository")
            >>= (.: "pullRequests")
            >>= (.: "nodes")
        pullRequest <- withArray "array of pull requests" parseNodes nodes
        id          <- pullRequest .: "id"
        return $ PrId id
      where
        parseNodes nodesArray = case V.length nodesArray of
            1 -> withObject "pull request object" return (V.head nodesArray)
            n ->
                fail
                    $ (show n)
                    <> " open pull requests found from the current branch to the working branch. There should be 1."



mergePullRequest :: GithubConfig -> String -> Program ()
mergePullRequest (GithubConfig { repoOrg, repoName, auth }) branchName = do
    Config { workingBranch } <- ask
    let body graphqlQuery = object [("query" .= graphqlQuery)]
    let headers     = header (B.pack "User-Agent") (B.pack repoOrg)
    let authOptions = generateAuthOptions auth
    -- let
    --     getRepoWithPrsQuery =
    --         (OperationDefinition $ Query $ Node
    --             ""
    --             []
    --             []
    --             [ SelectionField $ Field
    --                   ""
    --                   "repository"
    --                   [ Argument
    --                       "owner"
    --                       (ValueString . StringValue . T.pack $ repoOrg)
    --                   , Argument
    --                       "name"
    --                       (ValueString . StringValue . T.pack $ repoName)
    --                   ]
    --                   []
    --                   [ SelectionField $ Field "" "id" [] [] []
    --                   , SelectionField $ Field
    --                       ""
    --                       "pullRequests"
    --                       [ Argument
    --                           "baseRefName"
    --                           ( ValueString
    --                           . StringValue
    --                           . T.pack
    --                           $ workingBranch
    --                           )
    --                       , Argument
    --                           "headRefName"
    --                           (ValueString . StringValue . T.pack $ branchName)
    --                       , Argument "first"  (ValueInt 10)
    --                       , Argument "states" (ValueEnum . T.pack $ "OPEN")
    --                       ]
    --                       []
    --                       [ SelectionField $ Field
    --                             ""
    --                             "nodes"
    --                             []
    --                             []
    --                             [SelectionField $ Field "" "id" [] [] []]
    --                       ]
    --                   ]
    --             ]
    --         )
    -- let mergePullRequestQuery (PrId prId) =
    --         (OperationDefinition $ Mutation $ Node
    --             ""
    --             []
    --             []
    --             [ SelectionField $ Field
    --                   ""
    --                   "mergePullRequest"
    --                   [ Argument
    --                         "input"
    --                         ( ValueObject
    --                         . ObjectValue
    --                         $ [ ObjectField "pullRequestId"
    --                             $ ValueString
    --                             . StringValue
    --                             $ prId
    --                           ]
    --                         )
    --                   ]
    --                   []
    --                   [ SelectionField $ Field
    --                         ""
    --                         "pullRequest"
    --                         []
    --                         []
    --                         [SelectionField $ Field "" "permalink" [] [] []]
    --                   ]
    --             ]
    --         )
    runReq defaultHttpConfig $ do
        (prId :: PrId) <- responseBody <$> req
            POST
            graphqlUrl
            (ReqBodyJson $ body ("hi" :: T.Text))
            jsonResponse
            (authOptions <> headers)
        void $ req POST
                   graphqlUrl
                   (ReqBodyJson $ body ("hi" :: T.Text))
                   ignoreResponse
                   (authOptions <> headers)

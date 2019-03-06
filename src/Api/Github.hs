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
import           Data.Maybe
import qualified Data.Vector                   as V
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
            permalink <- o .: "data" >>= (.: "createPullRequest") >>= (.: "pullRequest") >>= (.: "permalink")
            return $ Permalink permalink
          Just errors -> withArray "array of errors" parseErrorsArray errors
      where
          parseError = withObject "object" (.: "message")
          parseErrorsArray errorsArray = do
            errorsArray <- mapM parseError (V.toList errorsArray)
            fail $ unlines errorsArray


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
        let
            createPullRequestQuery (RepoId repoId)
                = (operationDefinition $ Mutation $ Node
                      ""
                      []
                      []
                      [ SelectionField $ Field
                            ""
                            "createPullRequest"
                            [ Argument
                                  "input"
                                  ( ValueObject
                                  . ObjectValue
                                  $ [ ObjectField "baseRefName"
                                    $ ValueString
                                    . StringValue
                                    $ "master"
                                    , ObjectField "headRefName"
                                    $ ValueString
                                    . StringValue
                                    . T.pack
                                    $ branchName
                                    , ObjectField "title"
                                    $ ValueString
                                    . StringValue
                                    . T.pack
                                    $ branchName
                                    , ObjectField "repositoryId"
                                    $ ValueString
                                    . StringValue
                                    $ repoId
                                    , ObjectField "body"
                                    $ ValueString
                                    . StringValue
                                    . T.pack
                                    $ fromMaybe (Types.summary issue)
                                                (Types.description issue)
                                    ]
                                  )
                            ]
                            []
                            [ SelectionField $ Field
                                  ""
                                  "pullRequest"
                                  []
                                  []
                                  [ SelectionField
                                        $ Field "" "permalink" [] [] []
                                  ]
                            ]
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
            response <- req
                POST
                graphqlUrl
                (ReqBodyJson $ body (createPullRequestQuery repoId))
                jsonResponse
                (authOptions <> headers)
            let (Permalink link) = (responseBody response :: Permalink)
            return link

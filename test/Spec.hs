import           Test.Hspec
import qualified Git
import           Data.GraphQL.AST
import           Data.GraphQL.Encoder
import           Data.Either
import           Data.Text                     as T

main = hspec spec

spec :: Spec
spec = do
    describe "Git" $ do
        describe "getIssueKeyFromBranch" $ do
            it "should extract an issue key from the branch name" $ do
                shouldBe
                    (Git.getIssueKeyFromBranch "feature/PROJECT-34whatever")
                    (Right "PROJECT-34")
                shouldBe
                    (Git.getIssueKeyFromBranch "bug/PROJECT-50234-whatever")
                    (Right "PROJECT-50234")

            it "should fail for malformed branch names" $ do
                shouldSatisfy (Git.getIssueKeyFromBranch "bugPROJECT-34") isLeft
                shouldSatisfy
                    (Git.getIssueKeyFromBranch "feature/PROJECT34")
                    isLeft
                shouldSatisfy
                    (Git.getIssueKeyFromBranch "DSP-PROJECT-stuff")
                    isLeft
            it "testing assumptions" $ do
                shouldBe
                    (operationDefinition $ Mutation $ Node
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
                                      $ "test-branch"
                                      , ObjectField "title"
                                      $ ValueString
                                      . StringValue
                                      $ "test-title"
                                      , ObjectField "repositoryId"
                                      $ ValueString
                                      . StringValue
                                      $ "test-repoId"
                                      , ObjectField "body"
                                      $ ValueString
                                      . StringValue
                                      $ "test-body"
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
                    ""
  --                   mutation CreatePR {
  -- createPullRequest(input:{baseRefName:"master",headRefName:"test",title:"test title",repositoryId:"MDEwOlJlcG9zaXRvcnkxNzEzNjk4OTY="}) {
  --   pullRequest {
  --     permalink
  --   }
  -- }

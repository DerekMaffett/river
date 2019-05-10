import           Test.Hspec
import qualified Git
import           Data.GraphQL.AST
import           Data.GraphQL.Encoder
import           Data.Either
import           Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           Data.Aeson
import           Utils

main = hspec spec

shouldBeObject object (Object expectedObject) = shouldBe object expectedObject
testSetAtPath pathSegments (Object object) content =
    setAtPath pathSegments object content

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
                shouldBe "" ""
    describe "Utils" $ do
        describe "setAtPath" $ do
            it "should set a single field on an empty object" $ do
                shouldBeObject
                    (testSetAtPath ["field"]
                                   (object [])
                                   (MkWriteable (1 :: Int))
                    )
                    (object ["field" .= (1 :: Int)])
            it "should set additional fields on a populated object" $ do
                shouldBeObject
                    (testSetAtPath ["field2"]
                                   (object ["field1" .= (1 :: Int)])
                                   (MkWriteable (2 :: Int))
                    )
                    (object ["field1" .= (1 :: Int), "field2" .= (2 :: Int)])
            it "should set nested fields on an empty object" $ do
                shouldBeObject
                    (testSetAtPath ["settings", "id"]
                                   (object [])
                                   (MkWriteable (2 :: Int))
                    )
                    (object ["settings" .= object ["id" .= (2 :: Int)]])
            it "should set nested fields on a populated object" $ do
                shouldBeObject
                    (testSetAtPath
                        ["settings", "id"]
                        (object
                            ["settings" .= object ["name" .= ("Tom" :: String)]]
                        )
                        (MkWriteable (2 :: Int))
                    )
                    (object
                        [ "settings" .= object
                              ["id" .= (2 :: Int), "name" .= ("Tom" :: String)]
                        ]
                    )
            it "should create extra nesting as necessary without other changes"
                $ do
                      shouldBeObject
                          (testSetAtPath
                              ["settings", "country", "name"]
                              (object
                                  [ "settings"
                                        .= object ["name" .= ("Tom" :: String)]
                                  ]
                              )
                              (MkWriteable ("Sverige" :: String))
                          )
                          (object
                              [ "settings" .= object
                                    [ "country" .= object
                                        ["name" .= ("Sverige" :: String)]
                                    , "name" .= ("Tom" :: String)
                                    ]
                              ]
                          )

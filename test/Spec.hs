import           Test.Hspec
import           Git
import           Data.Either

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

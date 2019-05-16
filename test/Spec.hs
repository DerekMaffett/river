import           Test.Hspec
import           Data.GraphQL.AST
import           Data.GraphQL.Encoder
import           Data.Maybe
import           Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           Utils
import qualified IntegrationSpecs              as Integration
import qualified GitSpecs                      as Git
import qualified UtilsSpecs                    as Utils

main = hspec $ sequence_ [Utils.spec, Git.spec, Integration.spec]

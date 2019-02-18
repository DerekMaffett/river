module Config
    ( Config(..)
    , Program(..)
    , ask
    )
where

import qualified Types
import qualified Control.Monad.Reader          as Reader
import           GHC.Generics
import qualified Data.Aeson                    as A

type Program = Reader.ReaderT Config IO

ask :: Program Config
ask = Reader.ask

data Config = Config
  { logger :: String
  , repoName :: String
  , repoOrg :: String
  , projectKey :: String
  , defaultReviewers :: [Types.BitbucketUser]
  , workingBranch :: String
  , jiraDomain :: String
  , bitbucketUser :: Types.BitbucketUser
  , jiraUser :: Types.JiraUser
  , bitbucketUsername :: String
  , bitbucketPassword :: String
  , jiraEmail :: String
  , jiraToken :: String
  } deriving (Show)

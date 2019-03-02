module Config
    ( Config(..)
    , Program(..)
    , RepoManager(..)
    , ProjectManager(..)
    , JiraConfig(..)
    , BitbucketConfig(..)
    , BasicAuthCredentials(..)
    , ask
    )
where

import qualified Types
import qualified Control.Monad.Reader          as Reader
import           GHC.Generics
import           Data.Aeson

type Program = Reader.ReaderT Config IO

ask :: Program Config
ask = Reader.ask

data BasicAuthCredentials = BasicAuthCredentials String String

instance FromJSON BasicAuthCredentials where
  parseJSON = withObject "object" $ \o -> do
    username <- o .: "username"
    password <- o .: "password"
    return $ BasicAuthCredentials username password

data ProjectManager = Jira JiraConfig

data JiraConfig = JiraConfig
  { projectKey :: String
  , domainName :: String
  , auth :: BasicAuthCredentials
  , user :: Types.JiraUser
  }

data RepoManager = Bitbucket BitbucketConfig

data BitbucketConfig = BitbucketConfig
  { repoName :: String
  , repoOrg :: String
  , defaultReviewers :: [Types.BitbucketUser]
  , auth :: BasicAuthCredentials
  , user :: Types.BitbucketUser
  }

data Config = Config
  { logger :: String
  , projectManager :: ProjectManager
  , repoManager :: RepoManager
  , workingBranch :: String
  , bugCategories :: [String]
  }

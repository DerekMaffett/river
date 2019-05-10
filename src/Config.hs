module Config
    ( Config(..)
    , Program(..)
    , RepoManager(..)
    , ProjectManager(..)
    , JiraConfig(..)
    , BitbucketConfig(..)
    , GithubConfig(..)
    , BasicAuthCredentials(..)
    , LoggerContext(..)
    , ContainsLogger
    , ask
    , liftIO
    , getLoggerFromContext
    )
where

import qualified Types
import qualified Control.Monad.Reader          as Reader
import           GHC.Generics
import           Data.Aeson

class ContainsLogger a where
  getLoggerFromContext :: a -> String

data LoggerContext = LoggerContext String
instance ContainsLogger LoggerContext where
  getLoggerFromContext (LoggerContext logger) = logger

type Program = Reader.ReaderT Config IO

liftIO :: IO a -> Program a
liftIO = Reader.liftIO

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
  , onStart :: String
  , onPRCreation :: Maybe String
  , onMerge :: String
  , auth :: BasicAuthCredentials
  }

data RepoManager = Bitbucket BitbucketConfig | Github GithubConfig

data GithubConfig = GithubConfig
  { repoName :: String
  , repoOrg :: String
  , auth :: BasicAuthCredentials
  }

data BitbucketConfig = BitbucketConfig
  { repoName :: String
  , repoOrg :: String
  , defaultReviewers :: [Types.BitbucketUser]
  , auth :: BasicAuthCredentials
  }

data Config = Config
  { logger :: String
  , projectManager :: ProjectManager
  , repoManager :: RepoManager
  , workingBranch :: String
  , bugCategories :: [String]
  }
instance ContainsLogger Config where
  getLoggerFromContext (Config { logger }) = logger

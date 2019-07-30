module Config where

import qualified Types
import qualified Control.Monad.Reader          as Reader
import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import qualified Data.HashMap.Strict           as HM
import qualified Utils
import qualified Data.Text                     as T
import           Data.Aeson                    as A
import qualified Data.Aeson.Types              as AT
import qualified Data.Aeson.Encode.Pretty      as Pretty
import qualified System.Directory              as Dir

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

data BasicAuthCredentials = BasicAuthCredentials String String deriving (Show)

instance A.FromJSON BasicAuthCredentials where
    parseJSON = withObject "object" $ \o -> do
        username <- o .: "username"
        password <- o .: "password"
        return $ BasicAuthCredentials username password

data ProjectManager = Jira JiraConfig deriving (Show)

data JiraConfig = JiraConfig
  { projectKey :: String
  , domainName :: String
  , onStart :: String
  , onPRCreation :: Maybe String
  , onMerge :: String
  , auth :: BasicAuthCredentials
  } deriving (Show)

data RepoManager = Bitbucket BitbucketConfig | Github GithubConfig deriving (Show)

data GithubConfig = GithubConfig
  { repoName :: String
  , repoOrg :: String
  , auth :: BasicAuthCredentials
  } deriving (Show)

data BitbucketConfig = BitbucketConfig
  { repoName :: String
  , repoOrg :: String
  , defaultReviewers :: [Types.BitbucketUser]
  , auth :: BasicAuthCredentials
  } deriving (Show)

data Config = Config
  { logger :: String
  , projectManager :: ProjectManager
  , repoManager :: RepoManager
  , workingBranch :: String
  , remoteOrigin :: String
  , bugCategories :: [String]
  } deriving (Show)

instance ContainsLogger Config where
    getLoggerFromContext (Config { logger }) = logger

-- Structure of config files

data ConfigFile = River | RiverEnv
data DataPathSuggestion = DataPathSuggestion
  { file :: ConfigFile
  , path :: [T.Text]
  }

data RepoManagerType = BitbucketManager | GithubManager deriving (Show, Eq)
instance FromJSON RepoManagerType where
    parseJSON = withText "string" $ \s -> case s of
        "bitbucket" -> return BitbucketManager
        "github"    -> return GithubManager
        otherKey    -> fail $ show otherKey <> " is not a permitted key"

data ProjectManagerType = JiraManager deriving (Show, Eq)
instance FromJSON ProjectManagerType where
    parseJSON = withText "string" $ \s -> case s of
        "jira"   -> return JiraManager
        otherKey -> fail $ show otherKey <> " is not a permitted key"

dataSuggestion :: ConfigFile -> T.Text -> DataPathSuggestion
dataSuggestion configFile path =
    DataPathSuggestion configFile $ T.splitOn "." path

repoManagerTypeF :: DataPathSuggestion
repoManagerTypeF = dataSuggestion River "repoManager.name"

githubRepoNameF :: DataPathSuggestion
githubRepoNameF = dataSuggestion River "repoManager.settings.repoName"
githubRepoOrgF :: DataPathSuggestion
githubRepoOrgF = dataSuggestion River "repoManager.settings.repoOrg"
githubUsernameF :: DataPathSuggestion
githubUsernameF = dataSuggestion RiverEnv "github.username"
githubPasswordF :: DataPathSuggestion
githubPasswordF = dataSuggestion RiverEnv "github.password"

bitbucketRepoNameF :: DataPathSuggestion
bitbucketRepoNameF = dataSuggestion River "repoManager.settings.repoName"
bitbucketRepoOrgF :: DataPathSuggestion
bitbucketRepoOrgF = dataSuggestion River "repoManager.settings.repoOrg"
bitbucketDefaultReviewersF :: DataPathSuggestion
bitbucketDefaultReviewersF =
    dataSuggestion River "repoManager.settings.defaultReviewers"
bitbucketUsernameF :: DataPathSuggestion
bitbucketUsernameF = dataSuggestion RiverEnv "bitbucket.username"
bitbucketPasswordF :: DataPathSuggestion
bitbucketPasswordF = dataSuggestion RiverEnv "bitbucket.password"

projectManagerTypeF :: DataPathSuggestion
projectManagerTypeF = dataSuggestion River "projectManager.name"

jiraProjectKeyF :: DataPathSuggestion
jiraProjectKeyF = dataSuggestion River "projectManager.settings.projectKey"
jiraDomainNameF :: DataPathSuggestion
jiraDomainNameF = dataSuggestion River "projectManager.settings.domainName"
jiraOnStartF :: DataPathSuggestion
jiraOnStartF = dataSuggestion River "projectManager.settings.onStart"
jiraOnPRCreationF :: DataPathSuggestion
jiraOnPRCreationF = dataSuggestion River "projectManager.settings.onPRCreation"
jiraOnMergeF :: DataPathSuggestion
jiraOnMergeF = dataSuggestion River "projectManager.settings.onMerge"
jiraUsernameF :: DataPathSuggestion
jiraUsernameF = dataSuggestion RiverEnv "jira.username"
jiraPasswordF :: DataPathSuggestion
jiraPasswordF = dataSuggestion RiverEnv "jira.password"

workingBranchF :: DataPathSuggestion
workingBranchF = dataSuggestion River "workingBranch"
remoteOriginNameF :: DataPathSuggestion
remoteOriginNameF = dataSuggestion RiverEnv "remoteOrigin"
bugCategoriesF :: DataPathSuggestion
bugCategoriesF = dataSuggestion River "bugCategories"

-- Getting and parsing config files

parseConfig
    :: (A.FromJSON a)
    => (Maybe A.Object, Maybe A.Object)
    -> DataPathSuggestion
    -> Either String a
parseConfig (configFile, envFile) dataPathSuggestion =
    case file dataPathSuggestion of
        River    -> parseAtPath (path dataPathSuggestion) configFile
        RiverEnv -> parseAtPath (path dataPathSuggestion) envFile

parseAtPath :: (A.FromJSON a) => [T.Text] -> Maybe A.Object -> Either String a
parseAtPath pathSegments fileObject = do
    case fileObject of
        Nothing -> Left "no parseable file found"
        Just o  -> AT.parseEither parse o
          where
            parse objectToParse = do
                result <- foldM (.:) objectToParse (init pathSegments)
                result .: (last pathSegments)

getConfigFiles :: IO (Maybe A.Object, Maybe A.Object)
getConfigFiles = do
    configFile <- getRawFileContent ".river.json"
    envFile    <- getRawFileContent ".river.env.json"
    return (configFile, envFile)

getRawFileContent :: String -> IO (Maybe A.Object)
getRawFileContent fileName = do
    fileExists <- Dir.doesFileExist fileName
    if fileExists then A.decodeFileStrict' fileName else return Nothing

-- Parsing normally configured files

readConfig :: String -> IO (Either String Config.Config)
readConfig logger = do
    files <- getConfigFiles
    let getField dataPathSuggestion = parseConfig files dataPathSuggestion
    let getAuth usernameField passwordField = do
            username <- getField usernameField
            password <- getField passwordField
            return $ BasicAuthCredentials username password

    return $ do
        repoManagerType <- getField repoManagerTypeF
        repoManager     <- case repoManagerType of
            BitbucketManager -> do
                repoName         <- getField bitbucketRepoNameF
                repoOrg          <- getField bitbucketRepoOrgF
                defaultReviewers <- getField bitbucketDefaultReviewersF
                auth <- getAuth bitbucketUsernameF bitbucketPasswordF
                return $ Bitbucket $ BitbucketConfig { .. }
            GithubManager -> do
                repoName <- getField githubRepoNameF
                repoOrg  <- getField githubRepoOrgF
                auth     <- getAuth githubUsernameF githubPasswordF
                return $ Github $ GithubConfig { .. }
        projectManagerType <- getField projectManagerTypeF
        projectManager     <- case projectManagerType of
            JiraManager -> do
                projectKey   <- getField jiraProjectKeyF
                domainName   <- getField jiraDomainNameF
                onStart      <- getField jiraOnStartF
                onPRCreation <- getField jiraOnPRCreationF
                onMerge      <- getField jiraOnMergeF
                auth         <- getAuth jiraUsernameF jiraPasswordF
                return $ Jira $ JiraConfig { .. }
        workingBranch <- getField workingBranchF
        remoteOrigin  <- getField remoteOriginNameF
        bugCategories <- getField bugCategoriesF
        return $ Config { .. }

field
    :: (A.ToJSON a)
    => Config.DataPathSuggestion
    -> a
    -> (Config.DataPathSuggestion, Utils.Writeable)
field path content = (path, Utils.MkWriteable content)

writeToConfigFiles :: Config -> IO ()
writeToConfigFiles config = do
    B.writeFile ".river.json"
        $ Pretty.encodePretty' aesonPrettyConfig
        $ A.Object mainConfigFile
    B.writeFile ".river.env.json"
        $ Pretty.encodePretty' aesonPrettyConfig
        $ A.Object envFile
  where
    (mainConfigFile, envFile) =
        foldl setFieldWithData (HM.empty, HM.empty) fields
    setFieldWithData (mainConfig, envConfig) (dataPathSuggestion, content) =
        case file dataPathSuggestion of
            River ->
                ( Utils.setAtPath (path dataPathSuggestion) mainConfig content
                , envConfig
                )
            RiverEnv ->
                ( mainConfig
                , Utils.setAtPath (path dataPathSuggestion) envConfig content
                )
    fields :: [(DataPathSuggestion, Utils.Writeable)]
    fields            = repoManagerFields <> projectManagerFields <> baseFields
    repoManagerFields = case repoManager config of
        Bitbucket bitbucketConfig ->
            [field repoManagerTypeF ("bitbucket" :: T.Text)]
                <> bitbucketFields bitbucketConfig
        Github githubConfig -> [field repoManagerTypeF ("github" :: T.Text)]
            <> githubFields githubConfig
    projectManagerFields = case projectManager config of
        Jira jiraConfig -> [field projectManagerTypeF ("jira" :: T.Text)]
            <> jiraFields jiraConfig
    githubFields (GithubConfig {..}) =
        [ field githubRepoNameF repoName
        , field githubRepoOrgF  repoOrg
        , field githubUsernameF $ getUsername auth
        , field githubPasswordF $ getPassword auth
        ]
    bitbucketFields (BitbucketConfig {..}) =
        [ field bitbucketRepoNameF         repoName
        , field bitbucketRepoOrgF          repoOrg
        , field bitbucketDefaultReviewersF defaultReviewers
        , field bitbucketUsernameF $ getUsername auth
        , field bitbucketPasswordF $ getPassword auth
        ]
    jiraFields (JiraConfig {..}) =
        [ field jiraProjectKeyF   projectKey
        , field jiraDomainNameF   domainName
        , field jiraOnStartF      onStart
        , field jiraOnPRCreationF onPRCreation
        , field jiraOnMergeF      onMerge
        , field jiraUsernameF $ getUsername auth
        , field jiraPasswordF $ getPassword auth
        ]
    baseFields =
        [ field workingBranchF $ workingBranch config
        , field remoteOriginNameF $ remoteOrigin config
        , field bugCategoriesF $ bugCategories config
        ]
    getUsername (BasicAuthCredentials username _) = username
    getPassword (BasicAuthCredentials _ password) = password

aesonPrettyConfig :: Pretty.Config
aesonPrettyConfig = Pretty.Config
    { confIndent          = Pretty.Spaces 4
    , confCompare         = Pretty.keyOrder
                                [ "workingBranch"
                                , "bugCategories"
                                , "repoManager"
                                , "projectManager"
                                , "name"
                                , "settings"
                                , "repoName"
                                , "repoOrg"
                                , "defaultReviewers"
                                , "projectKey"
                                , "domainName"
                                , "username"
                                , "password"
                                ]
    , confNumFormat       = Pretty.Generic
    , confTrailingNewline = False
    }

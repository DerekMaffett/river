module Main where

import qualified Begin
import qualified Review
import qualified Merge
import qualified Config
import qualified Types
import qualified Git
import qualified Logger
import qualified Utils
import qualified Api.Jira                      as Jira
import           Control.Monad
import qualified Control.Monad.Reader          as Reader
import           Options.Applicative
import           System.Exit
import qualified Data.HashMap.Strict           as HM
import           Data.Aeson                    as A
                                         hiding ( Options )
import qualified Data.Aeson.Types              as AT
import qualified Data.Text                     as Text
import qualified Data.Aeson.Encode.Pretty      as Pretty
import qualified Data.ByteString.Lazy          as B
import qualified System.Directory              as Dir
import           System.Log.Logger

data BeginOptions = BeginOptions
    { useDebugLogger :: Bool
    , issueSource :: Begin.IssueSource
    } deriving (Show)

data ReviewOptions = ReviewOptions
  { useDebugLogger :: Bool
  } deriving (Show)

data MergeOptions = MergeOptions
  { useDebugLogger :: Bool
  } deriving (Show)

data InitOptions = InitOptions
  { useDebugLogger :: Bool
  , forceRebuild :: Bool
  } deriving (Show)

data Options
  = Begin BeginOptions
  | Review ReviewOptions
  | Merge MergeOptions
  | Init InitOptions deriving (Show)

debugModeFlag :: Parser Bool
debugModeFlag = switch $ long "debug" <> short 'd' <> help "debug mode"

issueKeyOpt :: Parser Begin.IssueSource
issueKeyOpt =
    Begin.Key
        <$> (strOption $ long "key" <> short 'k' <> metavar "ISSUE_KEY" <> help
                "issue key"
            )

quickFixOpt :: Parser Begin.IssueSource
quickFixOpt = liftA2 Begin.QuickFix summaryOpt issueTypeOpt
  where
    summaryOpt =
        strOption
            $  long "quick-fix"
            <> short 'q'
            <> metavar "ISSUE_SUMMARY"
            <> help "creates a jira ticket"
    issueTypeOpt =
        (\isBug -> if isBug then Types.Bug else Types.Task)
            <$> (switch $ long "bug" <> short 'b' <> help
                    "marks quick-fix as a bug, defaults to task"
                )


initOpts :: ParserInfo Options
initOpts = info optsParser desc
  where
    optsParser =
        Init <$> liftA2 InitOptions debugModeFlag forceRebuildFlag <**> helper
    forceRebuildFlag = switch $ long "force-rebuild" <> short 'f' <> help
        "force rebuild from scratch"
    desc =
        fullDesc <> progDesc "Initializes your account as a reviewer" <> header
            "Initializes your account as a reviewer"


beginOpts :: ParserInfo Options
beginOpts = info optsParser desc
  where
    optsParser =
        Begin
            <$>  liftA2 BeginOptions debugModeFlag (issueKeyOpt <|> quickFixOpt)
            <**> helper
    desc = fullDesc <> progDesc "Begins a Jira issue" <> header
        "Begins a Jira issue"

reviewOpts :: ParserInfo Options
reviewOpts = info optsParser desc
  where
    optsParser = Review <$> ReviewOptions <$> debugModeFlag <**> helper
    desc       = fullDesc <> progDesc "Sets an issue to code review" <> header
        "Sets an issue to code review"

mergeOpts :: ParserInfo Options
mergeOpts = info optsParser desc
  where
    optsParser = Merge <$> MergeOptions <$> debugModeFlag <**> helper
    desc =
        fullDesc <> progDesc "Completes an issue" <> header "Completes an issue"

opts :: ParserInfo Options
opts = info optsParser desc
  where
    optsParser =
        subparser
                (  (command "init" initOpts)
                <> (command "begin" beginOpts)
                <> (command "pr" reviewOpts)
                <> (command "merge" mergeOpts)
                )
            <**> helper
    desc = fullDesc <> progDesc "Automates workflow for projects" <> header
        "Workflow automation"


main :: IO ()
main = do
    execParser opts >>= runProgram

runProgram :: Options -> IO ()
runProgram options = do
    case options of
        Begin (BeginOptions { useDebugLogger, issueSource }) ->
            runWithConfigContext useDebugLogger $ Begin.begin issueSource
        Review (ReviewOptions { useDebugLogger }) ->
            runWithConfigContext useDebugLogger $ Review.review
        Merge (MergeOptions { useDebugLogger }) ->
            runWithConfigContext useDebugLogger $ Merge.merge
        Init (InitOptions { forceRebuild, useDebugLogger }) ->
            runWithLoggerContext useDebugLogger
                $ initializeApplication forceRebuild
  where
    runWithConfigContext useDebugLogger program = do
        configResult <- readConfig useDebugLogger
        case configResult of
            Left errorMsg -> die errorMsg
            Right config -> Reader.runReaderT (Git.setOrigin >> program) config
    runWithLoggerContext useDebugLogger program = do
        logger <- initializeLogger useDebugLogger
        Reader.runReaderT program (Config.LoggerContext logger)

dataSuggestion :: ConfigFile -> Text.Text -> DataPathSuggestion
dataSuggestion configFile path =
    DataPathSuggestion configFile $ Text.splitOn "." path

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
bugCategoriesF :: DataPathSuggestion
bugCategoriesF = dataSuggestion River "bugCategories"

data RepoManagerType = Bitbucket | Github deriving (Show)
instance FromJSON RepoManagerType where
  parseJSON = withText "string" $ \s -> case s of
                                          "bitbucket" -> return Bitbucket
                                          "github" -> return Github
                                          otherKey -> fail $ show otherKey <> " is not a permitted key"

data ProjectManagerType = Jira deriving (Show)
instance FromJSON ProjectManagerType where
  parseJSON = withText "string" $ \s -> case s of
                                          "jira" -> return Jira
                                          otherKey -> fail $ show otherKey <> " is not a permitted key"


type ConfigFiles = (Maybe Object, Maybe Object)
data ConfigFile = River | RiverEnv
data DataPathSuggestion = DataPathSuggestion
  { file :: ConfigFile
  , path :: [Text.Text]
  }

getFieldFromConfig
    :: (FromJSON a) => ConfigFiles -> DataPathSuggestion -> Either String a
getFieldFromConfig files dataPathSuggestion = do
    parseConfig files dataPathSuggestion

getFieldFromConfigOrPrompt
    :: (FromJSON a, Show a)
    => ConfigFiles
    -> DataPathSuggestion
    -> IO a
    -> Reader.ReaderT Config.LoggerContext IO a
getFieldFromConfigOrPrompt files dataPathSuggestion promptForField = do
    case parseConfig files dataPathSuggestion of
        Left errorMsg -> do
            Logger.logDebug errorMsg
            Reader.liftIO promptForField
        Right fieldContent -> do
            Logger.logDebug
                $  "Data found at "
                <> show (Text.intercalate "." (path dataPathSuggestion))
                <> ": "
                <> show fieldContent
            return fieldContent

parseConfig
    :: (FromJSON a) => ConfigFiles -> DataPathSuggestion -> Either String a
parseConfig (configFile, envFile) dataPathSuggestion =
    case file dataPathSuggestion of
        River    -> parseAtPath (path dataPathSuggestion) configFile
        RiverEnv -> parseAtPath (path dataPathSuggestion) envFile


parseAtPath :: (FromJSON a) => [Text.Text] -> Maybe Object -> Either String a
parseAtPath pathSegments fileObject = do
    case fileObject of
        Nothing -> Left "no parseable file found"
        Just o  -> AT.parseEither parse o
          where
            parse objectToParse = do
                result <- foldM (.:) objectToParse (init pathSegments)
                result .: (last pathSegments)

getConfigFiles :: IO ConfigFiles
getConfigFiles = do
    configFile <- getRawFileContent ".river.json"
    envFile    <- getRawFileContent ".river.env.json"
    return (configFile, envFile)

getRawFileContent :: String -> IO (Maybe Object)
getRawFileContent fileName = do
    fileExists <- Dir.doesFileExist fileName
    if fileExists then A.decodeFileStrict' fileName else return Nothing


readConfig :: Bool -> IO (Either String Config.Config)
readConfig useDebugLogger = do
    logger <- initializeLogger useDebugLogger
    files  <- getConfigFiles
    let getField dataPathSuggestion =
            getFieldFromConfig files dataPathSuggestion
    let getAuth usernameField passwordField = do
            username <- getField usernameField
            password <- getField passwordField
            return $ Config.BasicAuthCredentials username password

    return $ do
        repoManagerType <- getField repoManagerTypeF
        repoManager     <- case repoManagerType of
            Bitbucket -> do
                repoName         <- getField bitbucketRepoNameF
                repoOrg          <- getField bitbucketRepoOrgF
                defaultReviewers <- getField bitbucketDefaultReviewersF
                auth <- getAuth bitbucketUsernameF bitbucketPasswordF
                return $ Config.Bitbucket $ Config.BitbucketConfig {..}
            Github -> do
                repoName <- getField githubRepoNameF
                repoOrg  <- getField githubRepoOrgF
                auth     <- getAuth githubUsernameF githubPasswordF
                return $ Config.Github $ Config.GithubConfig {..}
        projectManagerType <- getField projectManagerTypeF
        projectManager     <- case projectManagerType of
            Jira -> do
                projectKey   <- getField jiraProjectKeyF
                domainName   <- getField jiraDomainNameF
                onStart      <- getField jiraOnStartF
                onPRCreation <- getField jiraOnPRCreationF
                onMerge      <- getField jiraOnMergeF
                auth         <- getAuth jiraUsernameF jiraPasswordF
                return $ Config.Jira $ Config.JiraConfig {..}
        workingBranch <- getField workingBranchF
        bugCategories <- getField bugCategoriesF
        return $ Config.Config {..}

initializeApplication :: Bool -> Reader.ReaderT Config.LoggerContext IO ()
initializeApplication forceRebuild = do
    config <- getConfigFromPrompt forceRebuild
    Reader.liftIO $ writeToConfigFiles config
    Logger.logNotice
        "Config files written. Please add .river.env.json to your gitignore - it contains your passwords."

getConfigFromPrompt
    :: Bool -> Reader.ReaderT Config.LoggerContext IO Config.Config
getConfigFromPrompt forceRebuild = do
    logger <- Config.getLoggerFromContext <$> Reader.ask
    files  <- Reader.liftIO getConfigFiles
    let getField dataPathSuggestion promptForField = if forceRebuild
            then Reader.liftIO promptForField
            else getFieldFromConfigOrPrompt files
                                            dataPathSuggestion
                                            promptForField
    let getAuth usernameField passwordField label = do
            username <- getField usernameField
                                 (Logger.query' $ label <> " username: ")
            password <- getField passwordField
                                 (Logger.query' $ label <> " password: ")
            return $ Config.BasicAuthCredentials username password
    repoManager <- do
        repoManagerType <-
            getField repoManagerTypeF
            $   (\answer -> case answer of
                    "bitbucket" -> Bitbucket
                    "github"    -> Github
                    _           -> Bitbucket
                )
            <$> Logger.queryWithLimitedSuggestions'
                    "Select your repo manager (tab for suggestions): "
                    ["bitbucket", "github"]
        case repoManagerType of
            Bitbucket -> do
                repoName <- getField bitbucketRepoNameF
                    $ Logger.query' "Repo name: "
                repoOrg <- getField bitbucketRepoOrgF
                    $ Logger.query' "Repo org: "
                auth <- getAuth bitbucketUsernameF
                                bitbucketPasswordF
                                "Bitbucket"
                return $ Config.Bitbucket $ Config.BitbucketConfig
                    { defaultReviewers = []
                    , ..
                    }
            Github -> do
                repoName <- getField githubRepoNameF
                    $ Logger.query' "Repo name: "
                repoOrg <- getField githubRepoOrgF $ Logger.query' "Repo org: "
                auth    <- getAuth githubUsernameF githubPasswordF "Github"
                return $ Config.Github $ Config.GithubConfig {..}
    projectManager <- do
        projectManagerType <-
            getField projectManagerTypeF
            $  Jira
            <$ Logger.queryWithLimitedSuggestions'
                   "Select your project manager (tab for suggestions): "
                   ["jira"]
        case projectManagerType of
            Jira -> do
                projectKey <- getField jiraProjectKeyF
                    $ Logger.query' "Jira project key: "
                domainName <- getField jiraDomainNameF
                    $ Logger.query' "Jira domain name: "
                auth <- getAuth jiraUsernameF jiraPasswordF "Jira"
                let partialJiraConfig = Config.JiraConfig {..}
                onStart <- getField jiraOnStartF
                    $ getTransitionName partialJiraConfig "starting a task"
                onPRCreation <-
                    getField jiraOnPRCreationF $ getOptionalTransitionName
                        partialJiraConfig
                        "starting a PR"
                onMerge <- getField jiraOnMergeF
                    $ getTransitionName partialJiraConfig "merging a PR"
                return $ Config.Jira $ Config.JiraConfig {..}
    workingBranch <- getField workingBranchF $ Logger.queryWithSuggestions'
        "Main git branch (tab for suggestions): "
        ["master", "develop"]
    bugCategories <-
        getField bugCategoriesF
            $ (words <$> Logger.query' "Bug categories (separate by spaces): ")
    return Config.Config {..}

getTransitionName :: Config.JiraConfig -> String -> IO String
getTransitionName jiraConfig reasonForTransition = getTransitionName'
    jiraConfig
    Logger.queryWithLimitedSuggestions'
    reasonForTransition
    (  "Select correct transition for "
    <> reasonForTransition
    <> " (tab for suggestions): "
    )

getOptionalTransitionName :: Config.JiraConfig -> String -> IO (Maybe String)
getOptionalTransitionName jiraConfig reasonForTransition =
    (\label -> if label == "" then Nothing else Just label)
        <$> getTransitionName'
                jiraConfig
                Logger.queryWithSuggestions'
                reasonForTransition
                (  "Select correct transition for "
                <> reasonForTransition
                <> ". Leave empty for none. (tab for suggestions): "
                )

getTransitionName'
    :: Config.JiraConfig
    -> (String -> [String] -> IO String)
    -> String
    -> String
    -> IO String
getTransitionName' jiraConfig queryFn reasonForTransition prompt = do
    issueKey <-
        (\issueNumber -> (Config.projectKey jiraConfig) <> "-" <> issueNumber)
            <$> Logger.query'
                    (  "Please provide an example task prior to "
                    <> reasonForTransition
                    <> ": "
                    <> (Config.projectKey jiraConfig)
                    <> "-"
                    )
    maybeIssue <- Jira.getIssue jiraConfig issueKey
    case maybeIssue of
        Nothing -> do
            putStrLn $ "Issue " <> issueKey <> " not found"
            getTransitionName' jiraConfig queryFn reasonForTransition prompt
        Just issue -> do
            let transitionNames =
                    (map (Types.name :: Types.Transition -> String))
                        . Types.transitions
                        $ issue
            queryFn prompt transitionNames


field
    :: (ToJSON a)
    => DataPathSuggestion
    -> a
    -> (DataPathSuggestion, Utils.Writeable)
field path content = (path, Utils.MkWriteable content)

writeToConfigFiles :: Config.Config -> IO ()
writeToConfigFiles config = do
    B.writeFile ".river.json" $ Pretty.encodePretty' aesonPrettyConfig $ Object
        mainConfigFile
    B.writeFile ".river.env.json"
        $ Pretty.encodePretty' aesonPrettyConfig
        $ Object envFile
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
    repoManagerFields = case Config.repoManager config of
        Config.Bitbucket bitbucketConfig ->
            [field repoManagerTypeF ("bitbucket" :: Text.Text)]
                <> bitbucketFields bitbucketConfig
        Config.Github githubConfig ->
            [field repoManagerTypeF ("github" :: Text.Text)]
                <> githubFields githubConfig
    projectManagerFields = case Config.projectManager config of
        Config.Jira jiraConfig ->
            [field projectManagerTypeF ("jira" :: Text.Text)]
                <> jiraFields jiraConfig
    githubFields (Config.GithubConfig {..}) =
        [ field githubRepoNameF repoName
        , field githubRepoOrgF  repoOrg
        , field githubUsernameF $ getUsername auth
        , field githubPasswordF $ getPassword auth
        ]
    bitbucketFields (Config.BitbucketConfig {..}) =
        [ field bitbucketRepoNameF         repoName
        , field bitbucketRepoOrgF          repoOrg
        , field bitbucketDefaultReviewersF defaultReviewers
        , field bitbucketUsernameF $ getUsername auth
        , field bitbucketPasswordF $ getPassword auth
        ]
    jiraFields (Config.JiraConfig {..}) =
        [ field jiraProjectKeyF   projectKey
        , field jiraDomainNameF   domainName
        , field jiraOnStartF      onStart
        , field jiraOnPRCreationF onPRCreation
        , field jiraOnMergeF      onMerge
        , field jiraUsernameF $ getUsername auth
        , field jiraPasswordF $ getPassword auth
        ]
    baseFields =
        [ field workingBranchF $ Config.workingBranch config
        , field bugCategoriesF $ Config.bugCategories config
        ]
    getUsername (Config.BasicAuthCredentials username _) = username
    getPassword (Config.BasicAuthCredentials _ password) = password

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

initializeLogger :: Bool -> IO String
initializeLogger useDebugLogger = do
    updateGlobalLogger "logger" $ setLevel loggerLevel
    return "logger"
    where loggerLevel = if useDebugLogger then DEBUG else NOTICE

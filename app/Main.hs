module Main where

import qualified Begin
import qualified Review
import qualified Merge
import qualified Config
import qualified Types
import qualified Git
import qualified Logger
import qualified Api.Jira                      as Jira
import           Control.Monad
import qualified Control.Monad.Reader          as Reader
import           Options.Applicative
import           System.Exit
import qualified Data.HashMap.Strict           as HM
import           Data.Aeson                    as A
                                         hiding ( Options )
import qualified Data.Aeson.Types              as AT
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
  { forceRebuild :: Bool
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
    optsParser       = Init <$> InitOptions <$> forceRebuildFlag <**> helper
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
    options <- execParser opts
    case options of
        Init forceRebuild -> initializeApplication forceRebuild
        _                 -> return ()
    config <- configFromOptions options
    Reader.runReaderT (runProgram options) config

runProgram :: Options -> Config.Program ()
runProgram options = do
    Git.setOrigin
    case options of
        Begin (BeginOptions { issueSource }) -> Begin.begin issueSource
        Review _ -> Review.review
        Merge _ -> Merge.merge
        Init -> return ()


configFromOptions :: Options -> IO Config.Config
configFromOptions options = do
    logger       <- initializeLogger options
    configExists <- Dir.doesFileExist ".river.json"
    envExists    <- Dir.doesFileExist ".river.env.json"
    unless configExists $ die ".river.json file does not exist!"
    unless envExists $ die ".river.env.json file does not exist!"
    combinedConfigResult <- getCombinedConfigsResult logger
    case combinedConfigResult of
        Left  errorMsg       -> die errorMsg
        Right combinedConfig -> return combinedConfig
  where
    getCombinedConfigsResult logger = do
        riverConfigValueResult <-
            (A.eitherDecodeFileStrict' ".river.json" :: IO
                  (Either String A.Object)
            )
        envConfigValueResult <-
            (A.eitherDecodeFileStrict' ".river.env.json" :: IO
                  (Either String A.Object)
            )
        return $ mergeConfigResults logger
                                    riverConfigValueResult
                                    envConfigValueResult

    mergeConfigResults logger configAResult configBResult = do
        configA <- configAResult
        configB <- configBResult
        let mergedValues = Object $ HM.union configA configB
        AT.parseEither (configParseJSON logger) mergedValues

    configParseJSON logger = withObject "object" $ \o -> do
        repoManager         <- o .: "repoManager"
        repoManagerType     <- repoManager .: "name"
        repoManagerSettings <- repoManager .: "settings"
        parsedRepoManager   <- case repoManagerType of
            "bitbucket" -> do
                repoName         <- repoManagerSettings .: "repoName"
                repoOrg          <- repoManagerSettings .: "repoOrg"
                defaultReviewers <- repoManagerSettings .: "defaultReviewers"
                auth             <- o .: "bitbucket"
                return $ Config.Bitbucket $ Config.BitbucketConfig
                    { repoName         = repoName
                    , repoOrg          = repoOrg
                    , defaultReviewers = defaultReviewers
                    , auth             = auth
                    }
            "github" -> do
                repoName <- repoManagerSettings .: "repoName"
                repoOrg  <- repoManagerSettings .: "repoOrg"
                auth     <- o .: "github"
                return $ Config.Github $ Config.GithubConfig
                    { repoName = repoName
                    , repoOrg  = repoOrg
                    , auth     = auth
                    }
            otherKey -> fail $ otherKey <> " is not an allowed repo manager"
        projectManager         <- o .: "projectManager"
        projectManagerType     <- projectManager .: "name"
        projectManagerSettings <- projectManager .: "settings"
        parsedProjectManager   <- case projectManagerType of
            "jira" -> do
                projectKey   <- projectManagerSettings .: "projectKey"
                domainName   <- projectManagerSettings .: "domainName"
                onStart      <- projectManagerSettings .: "onStart"
                onPRCreation <- projectManagerSettings .: "onPRCreation"
                onMerge      <- projectManagerSettings .: "onMerge"
                auth         <- o .: "jira"
                return $ Config.Jira $ Config.JiraConfig
                    { projectKey   = projectKey
                    , domainName   = domainName
                    , onStart      = onStart
                    , onPRCreation = onPRCreation
                    , onMerge      = onMerge
                    , auth         = auth
                    }
            otherKey -> fail $ otherKey <> " is not an allowed project manager"
        workingBranch <- o .: "workingBranch"
        bugCategories <- o .: "bugCategories"
        return $ Config.Config
            { logger         = logger
            , projectManager = parsedProjectManager
            , repoManager    = parsedRepoManager
            , workingBranch  = workingBranch
            , bugCategories  = bugCategories
            }

initializeApplication :: Bool -> IO ()
initializeApplication forceRebuild = do
    config <- getConfigFromPrompt forceRebuild
    writePrivateInfo config
    writePublicInfo config
    putStrLn
        "Config files written. Please add .river.env.json to your gitignore - it contains your passwords."

data DataPathSuggestion = River String | RiverEnv String

getConfigFromPrompt :: Bool -> IO Config.Config
getConfigFromPrompt forceRebuild = do
    repoManager    <- getRepoManager
    projectManager <- getProjectManager
    workingBranch  <- getWorkingBranch
    bugCategories  <- getBugCategories
    return Config.Config
        { logger         = "whatever"
        , projectManager = projectManager
        , repoManager    = repoManager
        , workingBranch  = workingBranch
        , bugCategories  = bugCategories
        }
  where
    suggestedDataLocation suggestedPath queryAction = do
        let fileName = case suggestedPath of
                River    _ -> "river.json"
                RiverEnv _ -> "river.env.json"

        object <-
            (A.eitherDecodeFileStrict' fileName :: IO (Either String A.Object))
        getObject


        AT.parseEither

    getRepoManager = do
        repoManager <- Logger.queryWithLimitedSuggestions'
            "Select your repo manager (tab for suggestions): "
            ["bitbucket", "github"]
        case repoManager of
            "bitbucket" -> getBitbucketRepoManager
            "github"    -> getGithubRepoManager
            _           -> die "Invalid project manager somehow selected"
    getBitbucketRepoManager = do
        repoName <- Logger.query' "Repo name: "
        repoOrg  <- Logger.query' "Repo org: "
        auth     <- getAuthCredentials "Bitbucket"
        return $ Config.Bitbucket $ Config.BitbucketConfig
            { repoName         = repoName
            , repoOrg          = repoOrg
            , defaultReviewers = []
            , auth             = auth
            }
    getGithubRepoManager = do
        repoName <- Logger.query' "Repo name: "
        repoOrg  <- Logger.query' "Repo org: "
        auth     <- getAuthCredentials "Github"
        return $ Config.Github $ Config.GithubConfig
            { repoName = repoName
            , repoOrg  = repoOrg
            , auth     = auth
            }
    getProjectManager = do
        projectKey <- Logger.query' "Jira project key: "
        domainName <- Logger.query' "Jira domain name: "
        auth       <- getAuthCredentials "Jira"
        let partialJiraConfig = Config.JiraConfig
                { projectKey = projectKey
                , domainName = domainName
                , auth       = auth
                }
        onStart      <- getTransitionName partialJiraConfig "starting a task"
        onPRCreation <- getOptionalTransitionName partialJiraConfig
                                                  "starting a PR"
        onMerge <- getTransitionName partialJiraConfig "merging a PR"
        return $ Config.Jira $ Config.JiraConfig
            { projectKey   = projectKey
            , domainName   = domainName
            , onStart      = onStart
            , onPRCreation = onPRCreation
            , onMerge      = onMerge
            , auth         = auth
            }
    getTransitionName jiraConfig reasonForTransition = getTransitionName'
        jiraConfig
        Logger.queryWithLimitedSuggestions'
        reasonForTransition
        (  "Select correct transition for "
        <> reasonForTransition
        <> " (tab for suggestions): "
        )
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
            (\issueNumber ->
                    (Config.projectKey jiraConfig) <> "-" <> issueNumber
                )
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




    getAuthCredentials serviceName = do
        when (serviceName == "Jira") $ putStrLn "Jira username is your email"
        username <- Logger.query' $ serviceName <> " username: "
        password <- Logger.queryMasked' $ serviceName <> " password: "
        return $ Config.BasicAuthCredentials username password
    getWorkingBranch = Logger.queryWithSuggestions'
        "Main git branch (tab for suggestions): "
        ["master", "develop"]
    getBugCategories =
        words <$> Logger.query' "Bug categories (separate by spaces): "


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


writePrivateInfo :: Config.Config -> IO ()
writePrivateInfo config =
    B.writeFile ".river.env.json"
        $ Pretty.encodePretty' aesonPrettyConfig
        $ A.Object
        . HM.unions
        . map (\(A.Object x) -> x)
        $ [repoManagerObject, projectManagerObject]
  where
    repoManagerObject = case Config.repoManager config of
        Config.Bitbucket (Config.BitbucketConfig { auth }) ->
            A.object ["bitbucket" .= (authObject auth)]
        Config.Github (Config.GithubConfig { auth }) ->
            A.object ["github" .= (authObject auth)]
    projectManagerObject = case Config.projectManager config of
        Config.Jira (Config.JiraConfig { auth }) ->
            A.object ["jira" .= (authObject auth)]
    authObject (Config.BasicAuthCredentials username password) =
        A.object ["username" .= username, "password" .= password]

writePublicInfo :: Config.Config -> IO ()
writePublicInfo config = B.writeFile ".river.json"
    $ Pretty.encodePretty' aesonPrettyConfig publicConfigObject
  where
    publicConfigObject = A.object
        [ "repoManager" .= repoManagerObject
        , "projectManager" .= projectManagerObject
        , "workingBranch" .= workingBranch
        , "bugCategories" .= bugCategories
        ]

    repoManagerObject = case Config.repoManager config of
        Config.Bitbucket (Config.BitbucketConfig { repoName, repoOrg, defaultReviewers })
            -> A.object
                [ "name" .= ("bitbucket" :: String)
                , "settings" .= A.object
                    [ "repoName" .= repoName
                    , "repoOrg" .= repoOrg
                    , "defaultReviewers" .= defaultReviewers
                    ]
                ]
        Config.Github (Config.GithubConfig { repoName, repoOrg }) -> A.object
            [ "name" .= ("github" :: String)
            , "settings"
                .= A.object ["repoName" .= repoName, "repoOrg" .= repoOrg]
            ]
    projectManagerObject = case Config.projectManager config of
        Config.Jira (Config.JiraConfig { projectKey, domainName, onStart, onPRCreation, onMerge })
            -> A.object
                [ "name" .= ("jira" :: String)
                , "settings" .= A.object
                    [ "projectKey" .= projectKey
                    , "domainName" .= domainName
                    , "onStart" .= onStart
                    , "onPRCreation" .= onPRCreation
                    , "onMerge" .= onMerge
                    ]
                ]

    workingBranch = Config.workingBranch config
    bugCategories = Config.bugCategories config

initializeLogger :: Options -> IO String
initializeLogger options = do
    updateGlobalLogger logger $ setLevel level
    return logger
  where
    logger = case options of
        Begin (BeginOptions { useDebugLogger }) ->
            if useDebugLogger then "DebugLogger" else "BasicLogger"
        Review (ReviewOptions { useDebugLogger }) ->
            if useDebugLogger then "DebugLogger" else "BasicLogger"
        Merge (MergeOptions { useDebugLogger }) ->
            if useDebugLogger then "DebugLogger" else "BasicLogger"
        Init -> "BasicLogger"
    level = case logger of
        "BasicLogger" -> NOTICE
        "DebugLogger" -> DEBUG
        _             -> NOTICE

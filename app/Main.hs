module Main where

import qualified Begin
import qualified Review
import qualified Config
import qualified Types
import qualified Git
import           Control.Monad
import qualified Control.Monad.Reader          as Reader
import           Options.Applicative
import           GHC.Generics
import           System.Exit
import           Api.Jira                      as Jira
import           Api.Bitbucket                 as Bitbucket
import           Data.Aeson                    as A
                                         hiding ( Options )
import qualified System.Directory              as Dir
import           System.Log.Logger

data BeginOptions = BeginOptions
    { useDebugLogger :: Bool
    , issueSource :: Begin.IssueSource
    } deriving (Show)

data ReviewOptions = ReviewOptions
  { useDebugLogger :: Bool
  } deriving (Show)

data Options
  = Begin BeginOptions
  | Review ReviewOptions
  | Init deriving (Show)

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
    optsParser = pure Init <**> helper
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

opts :: ParserInfo Options
opts = info optsParser desc
  where
    optsParser =
        subparser
                (  (command "init" initOpts)
                <> (command "begin" beginOpts)
                <> (command "pr" reviewOpts)
                )
            <**> helper
    desc = fullDesc <> progDesc "Automates workflow for projects" <> header
        "Workflow automation"


main :: IO ()
main = do
    options <- execParser opts
    config  <- configFromOptions options
    Reader.runReaderT (runProgram options) config

runProgram :: Options -> Config.Program ()
runProgram options = do
    Git.setOrigin
    case options of
        Begin (BeginOptions { issueSource }) -> Begin.begin issueSource
        Review _ -> Review.review
        Init -> do
            Config.Config { repoManager } <- Reader.ask
            case repoManager of
                Config.Bitbucket (Config.BitbucketConfig { user }) ->
                    Reader.liftIO $ initializeReviewer user

initializeReviewer :: Types.BitbucketUser -> IO ()
initializeReviewer bitbucketUser = do
    return ()
  --   result <- A.eitherDecodeFileStrict' ".river.json"
  --   case result of
  --       Left  errorMsg    -> die $ show errorMsg
  --       Right riverConfig -> A.encodeFile ".river.json"
  --           $ addSelfToConfig riverConfig bitbucketUser
  -- where
  --   addSelfToConfig riverConfig self = riverConfig
  --       { defaultReviewers = (filter (/= self) . defaultReviewers $ riverConfig)
  --                                <> [self]
  --       }

data RiverConfig = RiverConfig
  { repoManager :: RepoManager
  , projectManager :: ProjectManager
  , workingBranch :: String
  , bugCategories :: [String]
  } deriving (Generic, A.FromJSON)

data ProjectManager = Jira JiraConfig

data JiraConfig = JiraConfig
  { projectKey :: String
  , domainName :: String
  }

instance A.FromJSON ProjectManager where
  parseJSON = withObject "object" $ \o -> do
    managerType <- o .: "name"
    settings <- o .: "settings"
    case managerType of
      "jira" -> do
        projectKey <- settings .: "projectKey"
        domainName <- settings .: "domainName"
        return $ Jira $ JiraConfig { projectKey = projectKey, domainName = domainName }
      otherKey -> fail $ otherKey <> " is not an allowed project manager"

data RepoManager = Bitbucket BitbucketConfig

data BitbucketConfig = BitbucketConfig
  { repoName :: String
  , repoOrg :: String
  , defaultReviewers :: [Types.BitbucketUser]
  }

instance A.FromJSON RepoManager where
  parseJSON = withObject "object" $ \o -> do
    managerType <- o .: "name"
    settings <- o .: "settings"
    case managerType of
      "bitbucket" -> do
        repoName <- settings .: "repoName"
        repoOrg <- settings .: "repoOrg"
        defaultReviewers <- settings .: "defaultReviewers"
        return $ Bitbucket $ BitbucketConfig
          { repoName = repoName, repoOrg = repoOrg, defaultReviewers = defaultReviewers }
      otherKey -> fail $ otherKey <> " is not an allowed repo manager"

data JiraAuthCredentials = JiraAuthCredentials
  { jira :: Config.BasicAuthCredentials } deriving (Generic, A.FromJSON)

data BitbucketAuthCredentials = BitbucketAuthCredentials
  { bitbucket :: Config.BasicAuthCredentials } deriving (Generic, A.FromJSON)

configFromOptions :: Options -> IO Config.Config
configFromOptions options = do
    configExists <- Dir.doesFileExist ".river.json"
    envExists    <- Dir.doesFileExist ".river.env.json"
    unless configExists $ die ".river.json file does not exist!"
    unless envExists $ die ".river.env.json file does not exist!"
    riverConfigResult <- A.eitherDecodeFileStrict' ".river.json"
    logger            <- initializeLogger options
    case riverConfigResult of
        Left  riverError  -> die $ "river.json parsing error:\n" <> riverError
        Right riverConfig -> do
            projectManagerResult <- parseProjectManager riverConfig
            case projectManagerResult of
                Left  errorAction    -> errorAction
                Right projectManager -> do
                    repoManagerResult <- parseRepoManager riverConfig
                    case repoManagerResult of
                        Left  errorAction -> errorAction
                        Right repoManager -> do
                            return Config.Config
                                { logger         = logger
                                , projectManager = projectManager
                                , repoManager    = repoManager
                                , bugCategories  = bugCategories riverConfig
                                , workingBranch  = workingBranch riverConfig
                                }
  where
    parseProjectManager riverConfig = do
        case projectManager riverConfig of
            Jira (JiraConfig { projectKey, domainName }) -> do
                envResult <- A.eitherDecodeFileStrict' ".river.env.json"
                case envResult of
                    Left envError ->
                        return
                            $  Left
                            $  die
                            $  "river.env.json parsing error:\n"
                            <> envError
                    Right jiraAuth -> do
                        maybeUser <- Jira.getMyself domainName (jira jiraAuth)
                        case maybeUser of
                            Nothing ->
                                return
                                    $ Left
                                    $ die
                                          "Jira user not found. Are your credentials correct?"
                            Just user ->
                                return $ Right $ Config.Jira $ Config.JiraConfig
                                    { projectKey = projectKey
                                    , domainName = domainName
                                    , auth       = (jira jiraAuth)
                                    , user       = user
                                    }

    parseRepoManager riverConfig = do
        case repoManager riverConfig of
            Bitbucket (BitbucketConfig { repoName, repoOrg, defaultReviewers })
                -> do
                    envResult <- A.eitherDecodeFileStrict' ".river.env.json"
                    case envResult of
                        Left envError ->
                            return
                                $  Left
                                $  die
                                $  "river.env.json parsing error:\n"
                                <> envError
                        Right bitbucketAuth -> do
                            maybeUser <- Bitbucket.getMyself
                                (bitbucket bitbucketAuth)
                            case maybeUser of
                                Nothing ->
                                    return
                                        $ Left
                                        $ die
                                              "Bitbucket user not found. Are your credentials correct?"
                                Just user ->
                                    return
                                        $ Right
                                        $ Config.Bitbucket
                                        $ Config.BitbucketConfig
                                              { repoName = repoName
                                              , repoOrg = repoOrg
                                              , defaultReviewers = defaultReviewers
                                              , auth = bitbucket bitbucketAuth
                                              , user = user
                                              }

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
        Init -> "BasicLogger"
    level = case logger of
        "BasicLogger" -> NOTICE
        "DebugLogger" -> DEBUG
        _             -> NOTICE

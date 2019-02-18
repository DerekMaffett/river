module Main where

import qualified Begin
import qualified Review
import qualified Config
import qualified Types
import           Control.Monad
import qualified Control.Monad.Reader          as Reader
import           Options.Applicative
import           GHC.Generics
import           System.Exit
import           Api.Jira                      as Jira
import           Api.Bitbucket                 as Bitbucket
import qualified Data.Aeson                    as A
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
    case options of
        Begin (BeginOptions { issueSource }) -> Begin.begin issueSource
        Review _ -> Review.review
        Init -> do
            Config.Config { bitbucketUser } <- Reader.ask
            Reader.liftIO $ initializeReviewer bitbucketUser

initializeReviewer :: Types.BitbucketUser -> IO ()
initializeReviewer bitbucketUser = do
    result <- A.eitherDecodeFileStrict' ".river.json"
    case result of
        Left  errorMsg    -> die $ show errorMsg
        Right riverConfig -> A.encodeFile ".river.json"
            $ addSelfToConfig riverConfig bitbucketUser
  where
    addSelfToConfig riverConfig self = riverConfig
        { defaultReviewers = (filter (/= self) . defaultReviewers $ riverConfig)
                                 <> [self]
        }



data EnvConfig = EnvConfig
  { jiraEmail :: String
  , jiraToken :: String
  , bitbucketUsername :: String
  , bitbucketPassword :: String
  } deriving (Generic, A.ToJSON, A.FromJSON)

data RiverConfig = RiverConfig
  { workingBranch :: String
  , repoName :: String
  , defaultReviewers :: [Types.BitbucketUser]
  , projectKey :: String
  , jiraDomain :: String
  } deriving (Generic, A.ToJSON, A.FromJSON)


configFromOptions :: Options -> IO Config.Config
configFromOptions options = do
    configExists <- Dir.doesFileExist ".river.json"
    envExists    <- Dir.doesFileExist ".river.env.json"
    unless configExists $ die ".river.json file does not exist!"
    unless envExists $ die ".river.env.json file does not exist!"
    riverConfigResult <- A.eitherDecodeFileStrict' ".river.json"
    envResult         <- A.eitherDecodeFileStrict' ".river.env.json"
    logger            <- initializeLogger options
    case envResult of
        Left  envError  -> die $ "river.env.json parsing error:\n" <> envError
        Right envConfig -> case riverConfigResult of
            Left riverError ->
                die $ "river.json parsing error:\n" <> riverError
            Right riverConfig -> do
                maybeJiraUser <- Jira.getMyself (jiraDomain riverConfig)
                                                (jiraEmail envConfig)
                                                (jiraToken envConfig)
                maybeBitbucketUser <- Bitbucket.getMyself
                    (bitbucketUsername envConfig)
                    (bitbucketPassword envConfig)
                case maybeJiraUser of
                    Nothing ->
                        die "Jira user not found. Are your credentials correct?"
                    Just jiraUser -> case maybeBitbucketUser of
                        Nothing ->
                            die
                                "Jira user not found. Are your credentials correct?"
                        Just bitbucketUser -> return Config.Config
                            { logger            = logger
                            , repoName          = repoName riverConfig
                            , projectKey        = projectKey riverConfig
                            , defaultReviewers  = defaultReviewers riverConfig
                            , jiraDomain        = jiraDomain riverConfig
                            , bitbucketUser     = bitbucketUser
                            , bitbucketUsername = bitbucketUsername envConfig
                            , bitbucketPassword = bitbucketPassword envConfig
                            , jiraUser          = jiraUser
                            , jiraEmail         = jiraEmail envConfig
                            , jiraToken         = jiraToken envConfig
                            , workingBranch     = workingBranch riverConfig
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

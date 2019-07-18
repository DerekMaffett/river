module Main where

import qualified Begin
import qualified Review
import qualified Merge
import qualified Init
import qualified Config
import qualified Types
import qualified Logger
import qualified Control.Monad.Reader          as Reader
import           Control.Exception.Safe
import           Options.Applicative
import           System.Exit

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
  , useGui :: Bool
  } deriving (Show)

data Options
  = Begin BeginOptions
  | Review ReviewOptions
  | Merge MergeOptions
  | Init InitOptions deriving (Show)

debugModeFlag :: Parser Bool
debugModeFlag = switch $ long "debug" <> short 'd' <> help "debug mode"

useGuiFlag :: Parser Bool
useGuiFlag =
    switch $ long "graphical" <> short 'g' <> help "use graphical interface"

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
        Init
            <$>  liftA3 InitOptions debugModeFlag forceRebuildFlag useGuiFlag
            <**> helper
    forceRebuildFlag = switch $ long "force-rebuild" <> short 'f' <> help
        "force rebuild from scratch"
    desc =
        fullDesc
            <> progDesc "Initializes or fills in configuration as necessary"
            <> header "Initializes or fills in configuration as necessary"

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
    catchAny (execParser opts >>= runProgram) (putStrLn <$> displayException)

runProgram :: Options -> IO ()
runProgram options = do
    case options of
        Begin (BeginOptions { useDebugLogger, issueSource }) ->
            runWithConfigContext useDebugLogger $ Begin.begin issueSource
        Review (ReviewOptions { useDebugLogger }) ->
            runWithConfigContext useDebugLogger $ Review.review
        Merge (MergeOptions { useDebugLogger }) ->
            runWithConfigContext useDebugLogger $ Merge.merge
        Init (InitOptions { forceRebuild, useDebugLogger, useGui }) ->
            runWithLoggerContext useDebugLogger
                $ Init.initializeApplication True forceRebuild
  where
    runWithConfigContext useDebugLogger program = do
        logger       <- Logger.initializeLogger useDebugLogger
        configResult <- Config.readConfig logger
        case configResult of
            Left  errorMsg -> die errorMsg
            Right config   -> Reader.runReaderT program config
    runWithLoggerContext useDebugLogger program = do
        logger <- Logger.initializeLogger useDebugLogger
        Reader.runReaderT program (Config.LoggerContext logger)

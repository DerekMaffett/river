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
import qualified Data.HashMap.Strict           as HM
import           Data.Aeson                    as A
                                         hiding ( Options )
import qualified Data.Aeson.Types              as AT
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
            return ()
            -- Config.Config { repoManager } <- Reader.ask
            -- case repoManager of
            --     Config.Bitbucket (Config.BitbucketConfig { user }) ->
            --         Reader.liftIO $ initializeReviewer user

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
                projectKey <- projectManagerSettings .: "projectKey"
                domainName <- projectManagerSettings .: "domainName"
                auth       <- o .: "jira"
                return $ Config.Jira $ Config.JiraConfig
                    { projectKey = projectKey
                    , domainName = domainName
                    , auth       = auth
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

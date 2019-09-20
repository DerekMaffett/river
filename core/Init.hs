module Init
    ( initializeApplication
    )
where

import qualified Logger
import qualified Api.Jira                      as Jira
import qualified Api.Bitbucket                 as Bitbucket
import qualified Types
import qualified Data.Text                     as T
import qualified Config
import qualified Data.Aeson                    as A
import qualified System.IO                     as System
import qualified System.Directory              as Dir
import           Control.Monad
import qualified Control.Monad.Reader          as Reader

initializeApplication :: Bool -> Reader.ReaderT Config.LoggerContext IO ()
initializeApplication forceRebuild = do
    config <- getConfigFromPrompt forceRebuild
    Reader.liftIO $ Config.writeToConfigFiles config
    addEnvFileToGitignore
    Logger.logNotice "Config files written"


addEnvFileToGitignore :: Reader.ReaderT Config.LoggerContext IO ()
addEnvFileToGitignore = do
    gitignoreExists <- Reader.liftIO $ Dir.doesFileExist gitignoreFilePath
    if gitignoreExists
        then Reader.liftIO $ do
            ignoredGlobs <- lines <$> System.readFile gitignoreFilePath
            if riverEnvFilePath `notElem` ignoredGlobs
                then System.appendFile gitignoreFilePath
                                       ("\n" <> riverEnvFilePath)
                else return ()
        else do
            Logger.logNotice
                ".gitignore file not found. Please add .river.env.json to your gitignore - it contains your passwords."
  where
    riverEnvFilePath  = ".river.env.json"
    gitignoreFilePath = ".gitignore"


getFieldFromConfigOrPrompt
    :: (A.FromJSON a, Show a)
    => (Maybe A.Object, Maybe A.Object)
    -> Config.DataPathSuggestion
    -> Reader.ReaderT Config.LoggerContext IO a
    -> Reader.ReaderT Config.LoggerContext IO a
getFieldFromConfigOrPrompt files dataPathSuggestion promptForField = do
    case Config.parseConfig files dataPathSuggestion of
        Left errorMsg -> do
            Logger.logDebug errorMsg
            promptForField
        Right fieldContent -> do
            Logger.logDebug
                $  "Data found at "
                <> show (T.intercalate "." (Config.path dataPathSuggestion))
                <> ": "
                <> show fieldContent
            return fieldContent

getConfigFromPrompt
    :: Bool -> Reader.ReaderT Config.LoggerContext IO Config.Config
getConfigFromPrompt forceRebuild = do
    logger <- Config.getLoggerFromContext <$> Reader.ask
    files  <- Reader.liftIO Config.getConfigFiles
    let getField dataPathSuggestion promptForField = if forceRebuild
            then promptForField
            else getFieldFromConfigOrPrompt files
                                            dataPathSuggestion
                                            promptForField
    let getAuth getUsernameField getPasswordField = do
            username <- getUsernameField
            password <- getPasswordField
            return $ Config.BasicAuthCredentials username password
    repoManager <- do
        repoManagerType <-
            getField Config.repoManagerTypeF
            $   (\answer -> case answer of
                    "bitbucket" -> Config.BitbucketManager
                    "github"    -> Config.GithubManager
                    _           -> Config.BitbucketManager
                )
            <$> (Logger.queryWithLimitedSuggestions
                    "Select your repo manager (tab for suggestions): "
                    ["bitbucket", "github"]
                )
        case repoManagerType of
            Config.BitbucketManager -> do
                repoName <- getField Config.bitbucketRepoNameF
                    $ Logger.query "Repo name: "
                repoOrg <- getField Config.bitbucketRepoOrgF
                    $ Logger.query "Repo org: "
                auth <- getAuth
                    ( getField Config.bitbucketUsernameF
                    $ Logger.query "Bitbucket username: "
                    )
                    ( getField Config.bitbucketPasswordF
                    $ Logger.queryMasked "Bitbucket App Password: "
                    )
                defaultReviewers <- getDefaultReviewers files auth
                return $ Config.Bitbucket $ Config.BitbucketConfig { .. }
            Config.GithubManager -> do
                repoName <- getField Config.githubRepoNameF
                    $ Logger.query "Repo name: "
                repoOrg <- getField Config.githubRepoOrgF
                    $ Logger.query "Repo org: "
                auth <- getAuth
                    ( getField Config.githubUsernameF
                    $ Logger.query "Github username: "
                    )
                    ( getField Config.githubPasswordF
                    $ Logger.queryMasked "Github API Token: "
                    )
                return $ Config.Github $ Config.GithubConfig { .. }
    projectManager <- do
        projectManagerType <-
            getField Config.projectManagerTypeF
            $  Config.JiraManager
            <$ (Logger.queryWithLimitedSuggestions
                   "Select your project manager (tab for suggestions): "
                   ["jira"]
               )
        case projectManagerType of
            Config.JiraManager -> do
                projectKey <- getField Config.jiraProjectKeyF
                    $ Logger.query "Jira project key: "
                domainName <- getField Config.jiraDomainNameF
                    $ Logger.query "Jira domain name: "
                auth <- getAuth
                    ( getField Config.jiraUsernameF
                    $ Logger.query "Jira account email: "
                    )
                    ( getField Config.jiraPasswordF
                    $ Logger.queryMasked "Jira API Token: "
                    )
                let partialJiraConfig = Config.JiraConfig { .. }
                onStart <- getField Config.jiraOnStartF
                    $ getTransitionName partialJiraConfig "starting a task"
                onPRCreation <-
                    getField Config.jiraOnPRCreationF
                        $ getOptionalTransitionName partialJiraConfig
                                                    "starting a PR"
                onMerge <- getField Config.jiraOnMergeF
                    $ getTransitionName partialJiraConfig "merging a PR"
                return $ Config.Jira $ Config.JiraConfig { .. }
    workingBranch <-
        getField Config.workingBranchF $ Logger.queryWithSuggestions
            "Main git branch (tab for suggestions): "
            ["master", "develop"]
    remoteOrigin <-
        getField Config.remoteOriginNameF $ Logger.queryWithSuggestions
            "Remote origin name (suggestion: \"origin\"): "
            ["origin"]
    bugCategories <- getField Config.bugCategoriesF $ words <$> Logger.query
        "Bug categories (separate by spaces): "
    return Config.Config { .. }

getDefaultReviewers
    :: (Maybe A.Object, Maybe A.Object)
    -> Config.BasicAuthCredentials
    -> Reader.ReaderT Config.LoggerContext IO [Types.BitbucketUser]
getDefaultReviewers files bitbucketAuth = do
    currentReviewers <-
        case Config.parseConfig files Config.bitbucketDefaultReviewersF of
            Left errorMsg -> do
                Logger.logDebug errorMsg
                return []
            Right reviewers -> do
                Logger.logDebug $ "Reviewers found: " <> show reviewers
                return reviewers
    currentUser <- Logger.catchWithLog
        "Failed to get your Bitbucket account"
        (Bitbucket.getSelf bitbucketAuth)
    if (currentUser `notElem` currentReviewers)
        then do
            shouldAddAsReviewer <- Logger.queryYesNo
                "Would you like to add yourself as a default reviewer?"
            if shouldAddAsReviewer
                then return (currentUser : currentReviewers)
                else return currentReviewers
        else do
            return currentReviewers


getTransitionName
    :: Config.JiraConfig
    -> String
    -> Reader.ReaderT Config.LoggerContext IO String
getTransitionName jiraConfig reasonForTransition = getTransitionName'
    jiraConfig
    Logger.queryWithLimitedSuggestions
    reasonForTransition
    (  "Select correct transition for "
    <> reasonForTransition
    <> " (tab for suggestions): "
    )

getOptionalTransitionName
    :: Config.JiraConfig
    -> String
    -> Reader.ReaderT Config.LoggerContext IO (Maybe String)
getOptionalTransitionName jiraConfig reasonForTransition =
    (\label -> if label == "" then Nothing else Just label)
        <$> getTransitionName'
                jiraConfig
                Logger.queryWithSuggestions
                reasonForTransition
                (  "Select correct transition for "
                <> reasonForTransition
                <> ". Leave empty for none. (tab for suggestions): "
                )

getTransitionName'
    :: Config.JiraConfig
    -> (String -> [String] -> Reader.ReaderT Config.LoggerContext IO String)
    -> String
    -> String
    -> Reader.ReaderT Config.LoggerContext IO String
getTransitionName' jiraConfig queryFn reasonForTransition prompt = do
    issueKey <-
        (\issueNumber -> (Config.projectKey jiraConfig) <> "-" <> issueNumber)
            <$> Logger.query
                    (  "Please provide an example task prior to "
                    <> reasonForTransition
                    <> ": "
                    <> (Config.projectKey jiraConfig)
                    <> "-"
                    )
    maybeIssue <- Jira.getIssue jiraConfig issueKey
    case maybeIssue of
        Nothing -> do
            Logger.logNotice $ "Issue " <> issueKey <> " not found"
            getTransitionName' jiraConfig queryFn reasonForTransition prompt
        Just issue -> do
            let transitionNames =
                    (map (Types.name :: Types.Transition -> String))
                        . Types.transitions
                        $ issue
            queryFn prompt transitionNames

module Init
    ( initializeApplication
    )
where

import qualified Logger
import qualified Api.Jira                      as Jira
import qualified Api.Bitbucket                 as Bitbucket
import qualified Types
import qualified Utils
import qualified Data.Text                     as T
import qualified Config
import qualified Data.Aeson.Encode.Pretty      as Pretty
import qualified Data.HashMap.Strict           as HM
import qualified Data.ByteString.Lazy          as B
import qualified Data.Aeson                    as A
import           Control.Monad
import qualified Control.Monad.Reader          as Reader


initializeApplication :: Bool -> Reader.ReaderT Config.LoggerContext IO ()
initializeApplication forceRebuild = do
    config <- getConfigFromPrompt forceRebuild
    Reader.liftIO $ writeToConfigFiles config
    Logger.logNotice
        "Config files written. Please add .river.env.json to your gitignore - it contains your passwords."

getFieldFromConfigOrPrompt
    :: (A.FromJSON a, Show a)
    => (Maybe A.Object, Maybe A.Object)
    -> Config.DataPathSuggestion
    -> IO a
    -> Reader.ReaderT Config.LoggerContext IO a
getFieldFromConfigOrPrompt files dataPathSuggestion promptForField = do
    case Config.parseConfig files dataPathSuggestion of
        Left errorMsg -> do
            Logger.logDebug errorMsg
            Reader.liftIO promptForField
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
            getField Config.repoManagerTypeF
            $   (\answer -> case answer of
                    "bitbucket" -> Config.BitbucketManager
                    "github"    -> Config.GithubManager
                    _           -> Config.BitbucketManager
                )
            <$> Logger.queryWithLimitedSuggestions'
                    "Select your repo manager (tab for suggestions): "
                    ["bitbucket", "github"]
        case repoManagerType of
            Config.BitbucketManager -> do
                repoName <- getField Config.bitbucketRepoNameF
                    $ Logger.query' "Repo name: "
                repoOrg <- getField Config.bitbucketRepoOrgF
                    $ Logger.query' "Repo org: "
                auth <- getAuth Config.bitbucketUsernameF
                                Config.bitbucketPasswordF
                                "Bitbucket"
                defaultReviewers <- getDefaultReviewers files auth
                return $ Config.Bitbucket $ Config.BitbucketConfig {..}
            Config.GithubManager -> do
                repoName <- getField Config.githubRepoNameF
                    $ Logger.query' "Repo name: "
                repoOrg <- getField Config.githubRepoOrgF
                    $ Logger.query' "Repo org: "
                auth <- getAuth Config.githubUsernameF
                                Config.githubPasswordF
                                "Github"
                return $ Config.Github $ Config.GithubConfig {..}
    projectManager <- do
        projectManagerType <-
            getField Config.projectManagerTypeF
            $  Config.JiraManager
            <$ Logger.queryWithLimitedSuggestions'
                   "Select your project manager (tab for suggestions): "
                   ["jira"]
        case projectManagerType of
            Config.JiraManager -> do
                projectKey <- getField Config.jiraProjectKeyF
                    $ Logger.query' "Jira project key: "
                domainName <- getField Config.jiraDomainNameF
                    $ Logger.query' "Jira domain name: "
                auth <- getAuth Config.jiraUsernameF Config.jiraPasswordF "Jira"
                let partialJiraConfig = Config.JiraConfig {..}
                onStart <- getField Config.jiraOnStartF
                    $ getTransitionName partialJiraConfig "starting a task"
                onPRCreation <-
                    getField Config.jiraOnPRCreationF
                        $ getOptionalTransitionName partialJiraConfig
                                                    "starting a PR"
                onMerge <- getField Config.jiraOnMergeF
                    $ getTransitionName partialJiraConfig "merging a PR"
                return $ Config.Jira $ Config.JiraConfig {..}
    workingBranch <-
        getField Config.workingBranchF $ Logger.queryWithSuggestions'
            "Main git branch (tab for suggestions): "
            ["master", "develop"]
    bugCategories <-
        getField Config.bugCategoriesF
            $ (words <$> Logger.query' "Bug categories (separate by spaces): ")
    return Config.Config {..}

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
            Right reviewers -> return reviewers
    currentUser <- Bitbucket.getSelf bitbucketAuth
    if (currentUser `notElem` currentReviewers)
        then do
            shouldAddAsReviewer <- Logger.queryYesNo
                "Would you like to add yourself as a default reviewer?"
            if shouldAddAsReviewer
                then return (currentUser : currentReviewers)
                else return currentReviewers
        else do
            return currentReviewers


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
    :: (A.ToJSON a)
    => Config.DataPathSuggestion
    -> a
    -> (Config.DataPathSuggestion, Utils.Writeable)
field path content = (path, Utils.MkWriteable content)

writeToConfigFiles :: Config.Config -> IO ()
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
        case Config.file dataPathSuggestion of
            Config.River ->
                ( Utils.setAtPath (Config.path dataPathSuggestion)
                                  mainConfig
                                  content
                , envConfig
                )
            Config.RiverEnv ->
                ( mainConfig
                , Utils.setAtPath (Config.path dataPathSuggestion)
                                  envConfig
                                  content
                )
    fields :: [(Config.DataPathSuggestion, Utils.Writeable)]
    fields            = repoManagerFields <> projectManagerFields <> baseFields
    repoManagerFields = case Config.repoManager config of
        Config.Bitbucket bitbucketConfig ->
            [field Config.repoManagerTypeF ("bitbucket" :: T.Text)]
                <> bitbucketFields bitbucketConfig
        Config.Github githubConfig ->
            [field Config.repoManagerTypeF ("github" :: T.Text)]
                <> githubFields githubConfig
    projectManagerFields = case Config.projectManager config of
        Config.Jira jiraConfig ->
            [field Config.projectManagerTypeF ("jira" :: T.Text)]
                <> jiraFields jiraConfig
    githubFields (Config.GithubConfig {..}) =
        [ field Config.githubRepoNameF repoName
        , field Config.githubRepoOrgF  repoOrg
        , field Config.githubUsernameF $ getUsername auth
        , field Config.githubPasswordF $ getPassword auth
        ]
    bitbucketFields (Config.BitbucketConfig {..}) =
        [ field Config.bitbucketRepoNameF         repoName
        , field Config.bitbucketRepoOrgF          repoOrg
        , field Config.bitbucketDefaultReviewersF defaultReviewers
        , field Config.bitbucketUsernameF $ getUsername auth
        , field Config.bitbucketPasswordF $ getPassword auth
        ]
    jiraFields (Config.JiraConfig {..}) =
        [ field Config.jiraProjectKeyF   projectKey
        , field Config.jiraDomainNameF   domainName
        , field Config.jiraOnStartF      onStart
        , field Config.jiraOnPRCreationF onPRCreation
        , field Config.jiraOnMergeF      onMerge
        , field Config.jiraUsernameF $ getUsername auth
        , field Config.jiraPasswordF $ getPassword auth
        ]
    baseFields =
        [ field Config.workingBranchF $ Config.workingBranch config
        , field Config.bugCategoriesF $ Config.bugCategories config
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

module GUI where

import           Control.Monad                  ( join )
import qualified Control.Monad.Reader          as Reader
import           Reflex.Dom
import           Data.FileEmbed
import           Data.Maybe
import           Data.Either
import           Data.List
import           Data.Functor
import qualified Data.Text                     as T
import           Control.Monad.IO.Class
import           Types                          ( Issue(..)
                                                , Transition(..)
                                                )
import qualified Config
import qualified Logger
import qualified Data.Map                      as Map
import qualified Data.Aeson                    as A
import qualified Api.Jira.Get                  as Jira

main = do
    files <- Config.getConfigFiles
    mainWidgetWithCss css $ bodyElement files
    return ()


css = $(embedFile "src/init.css")

createDropdownOptions = zipWith
    (\index (val, displayVal) -> (index, val, displayVal))
    ([0 ..] :: [Integer])

getReflexDropdownOptions :: [(Integer, a, T.Text)] -> Map.Map Integer T.Text
getReflexDropdownOptions =
    Map.fromList . fmap (\(index, val, displayVal) -> (index, displayVal))

getValueFromIndex options index =
    fromJust
        . fmap (\(_, val, _) -> val)
        . find (\(i, _, _) -> i == index)
        $ options
getIndexFromValue options value =
    fromJust
        . fmap (\(i, _, _) -> i)
        . find (\(_, val, _) -> val == value)
        $ options

dropdownValue options ddropdown =
    getValueFromIndex options <$> _dropdown_value ddropdown

bodyElement :: MonadWidget t m => (Maybe A.Object, Maybe A.Object) -> m ()
bodyElement configFiles = do
    let repoManagerOptions = createDropdownOptions
            [ (Config.GithubManager   , "Github")
            , (Config.BitbucketManager, "Bitbucket")
            ]
        repoManagerType = getFieldFromConfigFiles configFiles
                                                  Config.GithubManager
                                                  Config.repoManagerTypeF
        projectManagerOptions =
            createDropdownOptions [(Config.JiraManager, "Jira")]
        projectManagerType = getFieldFromConfigFiles
            configFiles
            Config.JiraManager
            Config.projectManagerTypeF

    elClass "section" "section" $ do
        elClass "div" "container columns is-8" $ do
            elClass "div" "column is-full" $ do
                elClass "h1" "title" $ text "River project settings"
                let workingBranch = getFieldFromConfigFiles
                        configFiles
                        "master"
                        Config.workingBranchF
                    remoteOrigin = getFieldFromConfigFiles
                        configFiles
                        "origin"
                        Config.remoteOriginNameF
                dRepoManagerType <-
                    dropdownValue repoManagerOptions
                        <$> selectField "Repo Manager"
                                        repoManagerType
                                        repoManagerOptions
                dRepoManager :: Dynamic t Config.RepoManager <- widgetFold
                    (repoManagerSettingsWidget configFiles)
                    repoManagerType
                    (updated dRepoManagerType)

                dProjectManagerType <-
                    dropdownValue projectManagerOptions
                        <$> selectField "Project Manager"
                                        projectManagerType
                                        projectManagerOptions
                dProjectManager :: Dynamic t Config.ProjectManager <- widgetFold
                    (projectManagerSettingsWidget configFiles)
                    projectManagerType
                    (updated dProjectManagerType)

                iWorkingBranch <- textField "Working Branch" workingBranch
                iRemoteOrigin  <- textField "Git Remote Name" remoteOrigin
                let dBugCategories = constDyn []

                (submitEl, _) <- elClass' "button" "button is-primary" $ do
                    text "Generate Config"


                let eSubmitClick = domEvent Click submitEl
                    dFormState =
                        Config.Config "logger not needed"
                            <$> dProjectManager
                            <*> dRepoManager
                            <*> (T.unpack <$> value iWorkingBranch)
                            <*> (T.unpack <$> value iRemoteOrigin)
                            <*> dBugCategories
                    eFormSubmit = tagPromptlyDyn dFormState eSubmitClick
                performEvent_
                    ( ffor eFormSubmit
                    $ \val -> liftIO . Config.writeToConfigFiles $ val
                    )
    return ()

widgetFold foldFn init event = do
    nestedD <- widgetHold (foldFn init) (foldFn <$> event)
    return $ join nestedD

repoManagerSettingsWidget
    :: MonadWidget t m
    => (Maybe A.Object, Maybe A.Object)
    -> Config.RepoManagerType
    -> m (Dynamic t Config.RepoManager)
repoManagerSettingsWidget configFiles = \case
    Config.GithubManager -> do
        repoNameInput <- textField
            "Repo Name"
            (getFieldFromConfigFiles configFiles "" Config.githubRepoNameF)
        repoOrgInput <- textField
            "Repo Organization"
            (getFieldFromConfigFiles configFiles "" Config.githubRepoOrgF)
        usernameInput <- textField
            "Github Username"
            (getFieldFromConfigFiles configFiles "" Config.githubUsernameF)
        passwordInput <- textField
            "Github API Token"
            (getFieldFromConfigFiles configFiles "" Config.githubPasswordF)
        let dAuth =
                Config.BasicAuthCredentials
                    <$> (T.unpack <$> value usernameInput)
                    <*> (T.unpack <$> value passwordInput)
            dGithubConfig =
                Config.GithubConfig
                    <$> (T.unpack <$> value repoNameInput)
                    <*> (T.unpack <$> value repoOrgInput)
                    <*> dAuth
            dRepoManager = Config.Github <$> dGithubConfig
        return $ dRepoManager
    Config.BitbucketManager -> do
        repoNameInput <- textField
            "Repo Name"
            (getFieldFromConfigFiles configFiles "" Config.bitbucketRepoNameF)
        repoOrgInput <- textField
            "Repo Organization"
            (getFieldFromConfigFiles configFiles "" Config.bitbucketRepoOrgF)
        usernameInput <- textField
            "Bitbucket Username"
            (getFieldFromConfigFiles configFiles "" Config.bitbucketUsernameF)
        passwordInput <- textField
            "Bitbucket App Password"
            (getFieldFromConfigFiles configFiles "" Config.bitbucketPasswordF)
        let dAuth =
                Config.BasicAuthCredentials
                    <$> (T.unpack <$> value usernameInput)
                    <*> (T.unpack <$> value passwordInput)
            dBitbucketConfig =
                Config.BitbucketConfig
                    <$> (T.unpack <$> value repoNameInput)
                    <*> (T.unpack <$> value repoOrgInput)
                    <*> constDyn []
                    <*> dAuth
            dRepoManager = Config.Bitbucket <$> dBitbucketConfig
        return $ dRepoManager

projectManagerSettingsWidget
    :: MonadWidget t m
    => (Maybe A.Object, Maybe A.Object)
    -> Config.ProjectManagerType
    -> m (Dynamic t Config.ProjectManager)
projectManagerSettingsWidget configFiles = \case
    Config.JiraManager -> mdo
        jiraProjectKey <- textField
            "Project Key"
            (getFieldFromConfigFiles configFiles "" Config.jiraProjectKeyF)
        jiraDomainName <- textField
            "Jira Domain Name"
            (getFieldFromConfigFiles configFiles "" Config.jiraDomainNameF)
        usernameInput <- textField
            "Jira Account Email"
            (getFieldFromConfigFiles configFiles "" Config.jiraUsernameF)
        passwordInput <- textField
            "Jira API Token"
            (getFieldFromConfigFiles configFiles "" Config.jiraPasswordF)

        dOnStart <- exampleTaskWidget
            ("Example task ready to be started", "On Start Transition")
            dJiraConfig
        dOnPRCreation <- exampleTaskWidget
            ( "Example task ready for PR (optional)"
            , "On PR Creation Transition"
            )
            dJiraConfig
        dOnMerge <- exampleTaskWidget
            ("Example task ready for merging", "On Merge Transition")
            dJiraConfig

        let dAuth =
                Config.BasicAuthCredentials
                    <$> (T.unpack <$> value usernameInput)
                    <*> (T.unpack <$> value passwordInput)
            dJiraConfig =
                Config.JiraConfig
                    <$> (T.unpack <$> value jiraProjectKey)
                    <*> (T.unpack <$> value jiraDomainName)
                    <*> (fromMaybe "" <$> dOnStart)
                    <*> (dOnPRCreation)
                    <*> (fromMaybe "" <$> dOnMerge)
                    <*> dAuth
            dRepoManager = Config.Jira <$> dJiraConfig
        return $ dRepoManager

exampleTaskWidget
    :: MonadWidget t m
    => (T.Text, T.Text)
    -> Dynamic t Config.JiraConfig
    -> m (Dynamic t (Maybe String))
exampleTaskWidget (exampleLabel, transitionLabel) dJiraConfig = do
    eExampleTaskSearch <- elClass "label" "label" $ do
        text exampleLabel
        elClass "div" "field has-addons" $ do
            exampleTaskInput <- elClass "div" "control is-expanded" $ do
                textInput
                    $  def
                    &  textInputConfig_initialValue
                    .~ ""
                    &  attributes
                    .~ constDyn
                           (  ("class" =: "input")
                           <> ("autocapitalize" =: "none")
                           <> ("autocorrect" =: "off")
                           <> ("autocomplete" =: "off")
                           )
            (el, _) <- elClass "div" "control" $ do
                elClass' "button" "button is-info" $ do
                    text "search"
            let eClick = domEvent Click el
            return $ tagPromptlyDyn (T.unpack <$> value exampleTaskInput) eClick

    let eExampleTaskWithJiraConfig =
            attachPromptlyDyn dJiraConfig eExampleTaskSearch

    eMaybeIssue <- performEvent
        (ffor eExampleTaskWithJiraConfig $ \(jiraConfig, exampleTicketName) ->
            liftIO $ do
                logger <- Logger.initializeLogger False
                Reader.runReaderT
                    (Jira.getIssue jiraConfig exampleTicketName)
                    (Config.LoggerContext logger)
        )

    dMaybeOnStartValue :: Dynamic t (Maybe String) <- widgetFold
        (\case
            Nothing    -> return $ constDyn Nothing
            Just issue -> do
                let options     = mapToDropdownOptions issue
                let firstOption = getValueFromIndex options 0
                dOnStartValue <-
                    dropdownValue options
                        <$> selectField transitionLabel firstOption options
                return $ Just <$> dOnStartValue
        )
        Nothing
        eMaybeIssue

    return dMaybeOnStartValue

mapToDropdownOptions issue =
    createDropdownOptions
        $   (\transition -> (name transition, T.pack . name $ transition))
        <$> (transitions issue)

getFieldFromConfigFiles
    :: (A.FromJSON a)
    => (Maybe A.Object, Maybe A.Object)
    -> a
    -> Config.DataPathSuggestion
    -> a
getFieldFromConfigFiles files defaultValue dataPathSuggestion =
    fromRight defaultValue $ Config.parseConfig files dataPathSuggestion

selectField
    :: (Eq a, MonadWidget t m)
    => T.Text
    -> a
    -> [(Integer, a, T.Text)]
    -> m (Dropdown t Integer)
selectField label initialValue options = elClass "div" "field" $ do
    elClass "label" "label" $ do
        text label
        elClass "div" "control is-expanded" $ do
            elClass "div" "select is-fullwidth" $ do
                dropdown (getIndexFromValue options initialValue)
                         (constDyn . getReflexDropdownOptions $ options)
                    $ def

textField :: MonadWidget t m => T.Text -> T.Text -> m (TextInput t)
textField label initialValue = elClass "div" "field" $ do
    elClass "label" "label" $ do
        text label
        elClass "div" "control" $ do
            textInput
                $  def
                &  textInputConfig_initialValue
                .~ initialValue
                &  attributes
                .~ constDyn
                       (  ("class" =: "input")
                       <> ("autocapitalize" =: "none")
                       <> ("autocorrect" =: "off")
                       <> ("autocomplete" =: "off")
                       )

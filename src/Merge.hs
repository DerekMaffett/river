module Merge
    ( merge
    )
where

import qualified Api.Jira                      as Jira
import qualified Api.Bitbucket                 as Bitbucket
import qualified Api.Github                    as Github
import qualified Git
import           Config
import qualified Logger                        as L


merge :: Program ()
merge = do
    branchName <- Git.getCurrentBranch
    issueKey   <- getIssueKey branchName
    maybeIssue <- getIssue issueKey
    case maybeIssue of
        Nothing    -> L.logError "No such Jira issue could be found"
        Just issue -> do
            closeIssue issue
            mergePullRequest branchName


getIssue issueKey = do
    Config { projectManager } <- ask
    case projectManager of
        Jira settings -> Jira.getIssue settings issueKey


closeIssue issue = do
    Config { projectManager } <- ask
    case projectManager of
        Jira settings -> do
            L.logNotice "Setting JIRA issue to Done..."
            Jira.toDone settings issue


mergePullRequest branchName = do
    Config { repoManager } <- ask
    L.logNotice "Merging pull request..."
    case repoManager of
        Bitbucket settings -> do
            id <- Bitbucket.mergePullRequest settings branchName
            liftIO $ putStrLn (show id)
        Github settings -> Github.mergePullRequest settings branchName


getIssueKey branchName = case Git.getIssueKeyFromBranch branchName of
    Left _ -> do
        L.logError "Could not infer issue key from branch name"
        return "" -- Type matching... failing the program imperatively is wonky
    Right issueKey -> return issueKey

module Review
    ( review
    )
where

import qualified Api.Jira                      as Jira
import qualified Api.Bitbucket                 as Bitbucket
import qualified Api.Github                    as Github
import qualified Git
import qualified BugTracker
import           Control.Monad
import           Config
import qualified Logger                        as L
import qualified Types


review :: Program ()
review = do
    branchName <- Git.getCurrentBranch
    issueKey   <- getIssueKey branchName
    maybeIssue <- getIssue issueKey
    case maybeIssue of
        Nothing    -> L.logError "No such Jira issue could be found"
        Just issue -> do
            when (isBug issue) $ BugTracker.trackResolution issue
            setIssueToCodeReview issue
            openPullRequest issue branchName

getIssue issueKey = do
    Config { projectManager } <- ask
    case projectManager of
        Jira settings -> Jira.getIssue settings issueKey


setIssueToCodeReview issue = do
    Config { projectManager } <- ask
    case projectManager of
        Jira settings -> do
            L.logNotice "Setting JIRA issue to Code Review..."
            Jira.toCodeReview settings issue


openPullRequest issue branchName = do
    Config { repoManager } <- ask
    L.logNotice "Creating pull request..."
    case repoManager of
        Bitbucket settings -> do
            link <- Bitbucket.createPullRequest settings issue branchName
            L.logNotice $ "\nBitbucket link:\n" <> link
        Github settings -> do
            link <- Github.createPullRequest settings issue branchName
            L.logNotice $ "\nGithub link:\n" <> link

getIssueKey branchName = case Git.getIssueKeyFromBranch branchName of
    Left _ -> do
        L.logError "Could not infer issue key from branch name"
        return "" -- Type matching... failing the program imperatively is wonky
    Right issueKey -> return issueKey

isBug issue = case Types.issueType issue of
    Types.Bug  -> True
    Types.Task -> False

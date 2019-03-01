module Review
    ( review
    )
where

import qualified Api.Jira                      as Jira
import qualified Api.Bitbucket                 as Bitbucket
import qualified Git
import qualified BugTracker
import           Control.Monad
import           Config
import qualified Logger                        as L
import qualified Types


review :: Program ()
review = do
    branchName <- Git.getCurrentBranch
    case Git.getIssueKeyFromBranch branchName of
        Left _ -> L.logError "Could not infer issue key from branch name"
        Right issueKey -> do
            maybeIssue <- Jira.getIssue issueKey
            case maybeIssue of
                Nothing    -> L.logError "No such Jira issue could be found"
                Just issue -> do
                    when (isBug issue) $ BugTracker.trackResolution issue
                    L.logNotice "Creating pull request..."
                    link <- Bitbucket.createPullRequest issue branchName
                    L.logNotice "Setting JIRA issue to Code Review..."
                    Jira.toCodeReview issue
                    L.logNotice $ "\nBitbucket link:\n" <> link

isBug issue = case Types.issueType issue of
    Types.Bug  -> True
    Types.Task -> False

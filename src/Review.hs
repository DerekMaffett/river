module Review
    ( review
    )
where

import qualified Api.Jira                      as Jira
import qualified Api.Bitbucket                 as Bitbucket
import qualified Git
import           Config
import qualified Logger                        as L


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
                    -- when
                    L.logNotice "Creating pull request..."
                    link <- Bitbucket.createPullRequest issue branchName
                    L.logNotice "Setting JIRA issue to Code Review..."
                    Jira.toCodeReview issue
                    L.logNotice $ "\nBitbucket link:\n" <> link

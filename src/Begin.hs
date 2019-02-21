module Begin
    ( begin
    , IssueSource(..)
    )
where

import qualified Api.Jira                      as Jira
import qualified Git
import           Config
import qualified Types
import qualified Logger                        as L

data IssueSource
  = Key String
  | QuickFix String Types.IssueType deriving (Show)


begin :: IssueSource -> Program ()
begin issueSource = case issueSource of
    Key issueKey -> do
        L.logNotice "Searching JIRA for issue..."
        _begin issueKey
    QuickFix summary issueType -> do
        L.logNotice "Creating issue..."
        issueKey <- Jira.createIssue summary issueType
        L.logNotice $ "Created issue: " <> issueKey
        _begin issueKey


_begin :: String -> Program ()
_begin issueKey = do
    maybeIssue <- Jira.getIssue issueKey
    case maybeIssue of
        Nothing    -> L.logError $ "Issue " <> issueKey <> " not found"
        Just issue -> do
            L.logNotice "Issue found...\n"
            L.logNotice $ getIssueSummary issue
            Git.openBranch =<< getBranchName issue
            L.logNotice "Setting JIRA issue to In Progress..."
            Jira.toInProgress issue
            L.logNotice "Assigning JIRA issue to you..."
            Jira.assignIssue $ Types.key issue


getBranchName :: Types.Issue -> Program String
getBranchName issue = do
    input <- L.query $ "Input branch name: " <> baseBranchName
    return $ baseBranchName <> input
  where
    baseBranchName =
        (show . Types.issueType) issue <> "/" <> Types.key issue <> "-"

getIssueSummary :: Types.Issue -> String
getIssueSummary issue = Types.key issue <> " - " <> Types.summary issue

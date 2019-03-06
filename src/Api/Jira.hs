module Api.Jira
    ( module Api.Jira.Assign
    , module Api.Jira.Get
    , module Api.Jira.Create
    , toInProgress
    , toCodeReview
    , toDone
    )
where

import           Api.Jira.Get
import           Api.Jira.Create
import           Api.Jira.Transitions
import           Api.Jira.Assign

toInProgress = transitionIssue ToInProgress
toCodeReview = transitionIssue ToCodeReview
toDone = transitionIssue ToDone

module Api.Jira.Transitions
    ( transitionIssue
    , TransitionState(..)
    )
where

import           Api.Jira.Base
import           Data.Aeson
import           Data.Default.Class
import           Data.List
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics
import           Network.HTTP.Req
import           Config
import           Logger
import qualified Types                          ( Transition(..)
                                                , Issue(..)
                                                )

newtype Body = Body
  { transition :: Types.Transition
  } deriving (Show, Generic, FromJSON, ToJSON)

data TransitionState
    = InProgress
    | CodeReview
    | Todo
    | Done
    | InTest
    | Unknown deriving (Eq)

instance Show TransitionState where
  show Todo = "To Do"
  show InProgress = "In Progress"
  show CodeReview = "Code review"
  show InTest = "Awaiting approval"
  show Done = "Done"
  show _ = "Unknown Transition State"

transitionIssue :: TransitionState -> Types.Issue -> Program ()
transitionIssue transitionState issue = do
    url         <- getUrl
    authOptions <- generateAuthOptions
    case maybeTransition of
        Nothing ->
            logError $ "Unable to transition to " <> show transitionState
        Just transition -> runReq def $ do
            req POST
                url
                (ReqBodyJson (Body transition))
                ignoreResponse
                (authOptions <> urlOptions)
            return ()
  where
    getUrl = do
        baseUrl <- getBaseUrl
        return
            $  baseUrl
            /: "issue"
            /: (pack . Types.key $ issue)
            /: "transitions"

    urlOptions      = "expand" =: ("transitions" :: Text)
    maybeTransition = find
        ((== transitionState) . convertToTransitionState . Types.name)
        (Types.transitions issue)
    convertToTransitionState transitionName = case transitionName of
        "To Do"             -> Todo
        "In Progress"       -> InProgress
        "Code review"       -> CodeReview
        "Awaiting approval" -> InTest
        "Done"              -> Done
        _                   -> Unknown

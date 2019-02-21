module Api.Jira.Transitions
    ( transitionIssue
    , Transition(..)
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

data Transition
    = ToInProgress
    | ToCodeReview
    | Unknown deriving (Eq)

instance Show Transition where
  show ToInProgress = "In Progress"
  show ToCodeReview = "Code Review"
  show _ = "Unknown Transition State"

transitionIssue :: Transition -> Types.Issue -> Program ()
transitionIssue transition issue = do
    url         <- getUrl
    authOptions <- generateAuthOptions
    logDebug $ "Issue: " <> show issue
    case maybeTransition of
        Nothing -> logError $ "Unable to transition to " <> show transition
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
        ((== transition) . convertToTransitionState . Types.name)
        (Types.transitions issue)
    convertToTransitionState transitionName = case transitionName of
        "River-To-In-Progress" -> ToInProgress
        "River-To-Code-Review" -> ToCodeReview
        _                      -> Unknown

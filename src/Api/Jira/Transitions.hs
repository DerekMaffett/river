module Api.Jira.Transitions
    ( transitionIssue
    , Transition(..)
    )
where

import           Api.Jira.Base
import           Data.Aeson
import           Data.Default.Class
import           Data.List
import           Data.Tuple
import           Data.Maybe
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

data Transition
    = ToInProgress
    | ToCodeReview
    | ToDone
    | Unknown deriving (Eq)

transitionLookup =
    [ (ToInProgress, "river-to-in-progress")
    , (ToCodeReview, "river-to-code-review")
    , (ToDone      , "river-to-done")
    ]

inverseTransitionLookup = swap <$> transitionLookup

convertToTransition transitionName =
    (fromMaybe Unknown . lookup transitionName) inverseTransitionLookup
convertToTransitionName transition =
    (fromMaybe "unknown" . lookup transition) transitionLookup

instance Show Transition where
  show = convertToTransitionName

newtype Body = Body
  { transition :: Types.Transition
  } deriving (Show, Generic, FromJSON, ToJSON)

transitionIssue :: Transition -> JiraConfig -> Types.Issue -> Program ()
transitionIssue transition settings issue = do
    logDebug $ "Issue: " <> show issue
    case maybeTransition of
        Nothing ->
            logError
                $  "Unable to find transition "
                <> show transition
                <> "\n\nAvailable transitions: \n"
                <> (unlines . fmap Types.name . Types.transitions) issue
        Just transition -> runReq def $ do
            req POST
                url
                (ReqBodyJson (Body transition))
                ignoreResponse
                (authOptions <> urlOptions)
            return ()
  where
    authOptions = generateAuthOptions settings
    url =
        (getBaseUrl settings)
            /: "issue"
            /: (pack . Types.key $ issue)
            /: "transitions"

    urlOptions      = "expand" =: ("transitions" :: Text)
    maybeTransition = find
        ((== transition) . convertToTransition . Types.name)
        (Types.transitions issue)

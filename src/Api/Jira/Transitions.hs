module Api.Jira.Transitions
    ( transitionIssue
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

newtype Body = Body
  { transition :: Types.Transition
  } deriving (Show, Generic, FromJSON, ToJSON)

transitionIssue :: String -> JiraConfig -> Types.Issue -> Program ()
transitionIssue transitionLabel settings issue = do
    logDebug $ "Issue: " <> show issue
    case maybeTransition of
        Nothing ->
            logError
                $  "Unable to find transition "
                <> transitionLabel
                <> "\n\nAvailable transitions: \n"
                <> (unlines . fmap Types.name . Types.transitions) issue
        Just transition -> runReq defaultHttpConfig $ do
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

    urlOptions = "expand" =: ("transitions" :: Text)
    maybeTransition =
        find ((== transitionLabel) . Types.name) (Types.transitions issue)

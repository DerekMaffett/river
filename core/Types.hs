module Types
    ( Issue(..)
    , IssueType(..)
    , Transition(..)
    , JiraUser(..)
    , BitbucketUser(..)
    )
where

import           Data.Aeson
import           Data.List.Split
import           GHC.Generics

data IssueType
  = Bug
  | Task

instance Show IssueType where
  show Bug = "bug"
  show Task = "feature"

data Issue = Issue
  { key :: String
  , transitions :: [Transition]
  , summary :: String
  , issueType :: IssueType
  , description :: Maybe String
  } deriving (Show)

instance FromJSON Issue where
  parseJSON = withObject "object" $ \o -> do
    key <- o .: "key"
    transitions <- o .: "transitions"
    fields <- o .: "fields"
    summary <- fields .: "summary"
    issueType <- convertIssueType <$> (fields .: "issuetype" >>= (.: "name"))
    description <- fields .: "description"
    return $ Issue key transitions summary issueType description
    where
      convertIssueType :: String -> IssueType
      convertIssueType issueTypeString = if issueTypeString `elem` ["Bug", "Bug Sub-task"] then Bug else Task


data Transition = Transition
  { id :: String
  , name :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data JiraUser = JiraUser
  { name :: String
  , accountId :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

data BitbucketUser = BitbucketUser
  { uuid :: String
  , account_id :: String
  , display_name :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

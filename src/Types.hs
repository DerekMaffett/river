module Types
    ( Issue(..)
    , Fields(..)
    , IssueType(..)
    , IssueTypeInternal(..)
    , Transition(..)
    , JiraUser(..)
    , BitbucketUser(..)
    )
where

import           Data.Aeson
import           Data.List.Split
import           GHC.Generics

data Issue = Issue
  { key :: String
  , transitions :: [Transition]
  , fields :: Fields
  } deriving (Show, Generic, FromJSON)

-- Fields is a recurring data type, but fields are optional. Not sure
-- how to best deal with this yet.
data Fields = Fields
  { summary :: String
  , issuetype :: IssueType
  , description :: Maybe String
  } deriving (Show, Generic, FromJSON)

data IssueType = IssueType
  { issueTypeName :: String
  } deriving (Show)

instance FromJSON IssueType where
  parseJSON (Object x) = IssueType <$> x .: "name"
  parseJSON _ = fail "Expected an Object"

data IssueTypeInternal
  = Bug
  | Task deriving (Show)

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

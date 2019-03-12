module Types where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Aeson
import           Protolude hiding (Text)

newtype UserId = UserId Int deriving (Eq, Generic, Show)
newtype Email = Email Text deriving (Eq, Generic, Show)

data Safe = SafeState deriving (Eq, Generic, Show)
data AtRisk = AtRiskState deriving (Eq, Generic, Show)
data Unknown = UnknownState deriving (Eq, Generic, Show)
data UserState = Safe | AtRisk | Unknown deriving (Eq, Generic, Show)

data User a state =
  User
  { userId        :: a UserId
  , userFirstName :: a Text
  , userLastName  :: a Text
  , userEmail     :: a Email
  , userState     :: a state
  }
  deriving (Generic)

data ErrorResponse =
  ErrorResponse
  { errors :: [Text]
  }
  deriving (Eq, Generic, Show)

type SafeUser = User Identity Safe
deriving instance Eq SafeUser
deriving instance Show SafeUser

type AtRiskUser = User Identity AtRisk
deriving instance Eq AtRiskUser
deriving instance Show AtRiskUser

type UnknownUser = User Identity Unknown
deriving instance Eq UnknownUser
deriving instance Show UnknownUser

type AnyUser = User Identity UserState
deriving instance Eq AnyUser
deriving instance Show AnyUser

type PartialUser = User Maybe UserState
deriving instance Eq PartialUser
deriving instance Generic PartialUser
deriving instance Show PartialUser

instance ToJSON UserId
instance FromJSON UserId
instance ToJSON Email
instance FromJSON Email
instance ToJSON Safe
instance FromJSON Safe
instance ToJSON AtRisk
instance FromJSON AtRisk
instance ToJSON Unknown
instance FromJSON Unknown
instance ToJSON UserState
instance FromJSON UserState
instance ToJSON AnyUser
instance FromJSON AnyUser
instance ToJSON ErrorResponse

instance FromJSON PartialUser where
  parseJSON (Object v) = do
    firstName <- v .: "firstName"
    lastName <- v .: "lastName"
    email <- v .: "email"
    return $
      User
      Nothing
      firstName
      lastName
      email
      (Just Unknown)

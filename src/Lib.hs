module Lib where

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status
import Protolude hiding (get)
import Web.Scotty

import Data.Monoid (mconcat)

newtype UserId = UserId Int deriving (Eq, Generic, Show)
newtype Email = Email Text deriving (Eq, Generic, Show)

data User =
  User
  { userId        :: UserId
  , userFirstName :: Text
  , userLastName  :: Text
  , userEmail     :: Email
  }
  deriving (Eq, Generic, Show)

instance ToJSON UserId
instance ToJSON Email
instance ToJSON User

bob = User (UserId 1) "Bob" "Belcher" (Email "bob@burgers.com")

linda = User (UserId 2) "Linda" "Belcher" (Email "linda@burgers.com")

users :: [User]
users = [bob, linda]

run = scotty 3000 $ do
  get "/users" $ do
    json $ users
  get "/users/:id" $ do
    uid <- param "id"
    case (find (\user -> userId user == UserId uid) users) of
      Nothing -> status status404
      Just user -> json user

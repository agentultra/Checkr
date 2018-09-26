{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson hiding (json)
import Data.Default.Class
import Data.Text.Lazy hiding (find)
import Network.HTTP.Types.Status
import Protolude hiding (get, Text)
import Web.Scotty.Trans

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

data CreateUser =
  CreateUser
  { createUserFirstName :: Text
  , createUserLastName  :: Text
  , createUserEmail     :: Email
  }
  deriving (Eq, Generic, Show)

instance ToJSON UserId
instance FromJSON UserId
instance ToJSON Email
instance FromJSON Email
instance ToJSON User
instance FromJSON User
instance FromJSON CreateUser

data AppState = AppState { users :: [User], serial :: Int }

instance Default AppState where
  def = AppState [] 0

newtype WebAction a = WebAction { runWebAction :: ReaderT (TVar AppState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webAction :: MonadTrans t => WebAction a -> t WebAction a
webAction = lift

getState :: (AppState -> b) -> WebAction b
getState f = ask >>= liftIO . readTVarIO >>= return . f

modifyState :: (AppState -> AppState) -> WebAction ()
modifyState f = ask >>= liftIO . atomically . flip modifyTVar' f

addUser :: Text -> Text -> Email -> AppState -> AppState
addUser firstName lastName email state =
  let nextId = (serial state) + 1
      user = User (UserId nextId) firstName lastName email
  in
    state { users = (users state) ++ [user], serial = nextId }

run :: IO ()
run = do
  sync <- newTVarIO def
  let runActionToIO m = runReaderT (runWebAction m) sync

  scottyT 3000 runActionToIO app

app :: ScottyT Text WebAction ()
app = do
  get "/users" $ do
    users' <- webAction $ getState users
    json $ users'
  get "/users/:id" $ do
    uid <- param "id"
    users' <- webAction $ getState users
    case (find (\user -> userId user == UserId uid) users') of
      Nothing   -> status status404
      Just user -> json user
  post "/users" $ do
    b <- body
    let decodedUser :: Maybe CreateUser = decode b
    case decodedUser of
      Nothing -> status status406
      Just user -> do
        webAction $ modifyState $
          addUser
          (createUserFirstName user)
          (createUserLastName user)
          (createUserEmail user)
        redirect "/users"

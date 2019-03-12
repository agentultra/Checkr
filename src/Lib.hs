module Lib where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Aeson hiding (json, Success)
import           Data.Default.Class
import           Data.Monoid (mconcat)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Validation
import           Network.HTTP.Types.Status
import           Protolude hiding (get, Text)
import           Web.Scotty.Trans

import Types
import Validation

data AppState = AppState { users :: [AnyUser], serial :: Int }

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

type Key = Int

class MonadDB m v | m -> v where
  getEntity :: Key -> m (Maybe v)
  getEntities :: m [v]
  insertEntity :: v -> m Key

instance MonadDB WebAction AnyUser where
  getEntity id            = do
    users' <- getState users
    return $ find (\user -> userId user == Identity (UserId id)) users'
  getEntities             = getState users
  insertEntity user = do
    state <- getState identity
    let nextId = (serial state) + 1
    let user' = user { userId = Identity (UserId nextId) }
    modifyState $ \_ -> state { users = (users state) ++ [user'], serial = nextId }
    return nextId

run :: IO ()
run = do
  sync <- newTVarIO def
  let runActionToIO m = runReaderT (runWebAction m) sync

  scottyT 3000 runActionToIO app

app :: ScottyT Text WebAction ()
app = do
  get "/users" $ do
    users' <- webAction $ getEntities
    json $ users'
  get "/users/:id" $ do
    uid <- param "id"
    user' <- webAction $ getEntity uid
    case user' of
      Nothing   -> status status404
      Just user -> json user
  post "/users" $ do
    b <- body
    let decodedUser :: Maybe PartialUser = decode b
    case decodedUser of
      Nothing -> status status406
      Just user -> do
        case isValidUser user of
          Success [firstName, lastName, email] -> do
            webAction $ insertEntity $
              User
              (Identity (UserId (-1)))
              (Identity firstName)
              (Identity lastName)
              (Identity (Email email))
              (Identity Unknown)
            redirect "/users"
          Failure errs -> do
            json $ ErrorResponse errs
            status status406

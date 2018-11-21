module Lib where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson hiding (json, Success)
import Data.Default.Class
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Validation
import Network.HTTP.Types.Status
import Protolude hiding (get, Text)
import Web.Scotty.Trans

import Data.Monoid (mconcat)

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
      User (Just (UserId (-1)))
      firstName
      lastName
      email
      (Just Unknown)

isNameLongEnough :: Text -> Validation [Text] Text
isNameLongEnough name = if Text.length name >= 1
                        then Success name
                        else Failure ["Name must contain 1 or more characters"]

isNameShortEnough :: Text -> Validation [Text] Text
isNameShortEnough name = if Text.length name <= 80
                         then Success name
                         else Failure ["Name must be less than 80 characters"]

isValidName :: Text -> Validation [Text] Text
isValidName name = pure name <* isNameLongEnough name <* isNameShortEnough name

isEmailLongEnough :: Email -> Validation [Text] Email
isEmailLongEnough e@(Email email) =
  if Text.length email >= 3
  then Success e
  else Failure ["Email must be at least 3 characters"]

doesEmailHaveAtInMiddle :: Email -> Validation [Text] Email
doesEmailHaveAtInMiddle e@(Email email) =
  if not (Text.isPrefixOf "@" email) &&
     Text.isInfixOf "@" email &&
     not (Text.isSuffixOf "@" email)
  then Success e
  else Failure ["Email must contain an '@' character in the middle"]

isValidEmail :: Email -> Validation [Text] Text
isValidEmail e@(Email email) =
  pure email
  <* isEmailLongEnough e
  <* doesEmailHaveAtInMiddle e

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
        let firstName = maybe "" identity (userFirstName user)
        let lastName = maybe "" identity (userLastName user)
        let email = maybe (Email "") identity (userEmail user)
        let validations = sequenceA [ isValidName firstName
                                    , isValidName lastName
                                    , isValidEmail email
                                    ]
        case validations of
          Success _ -> do
            webAction $ insertEntity $
              User
              (Identity (UserId (-1)))
              (Identity firstName)
              (Identity lastName)
              (Identity email)
              (Identity Unknown)
            redirect "/users"
          Failure errs -> do
            json $ ErrorResponse errs
            status status406

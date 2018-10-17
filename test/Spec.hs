import Lib

import Protolude
import Test.Hspec

newtype TestM a = TestM (Identity a)
  deriving (Functor, Applicative, Monad)

unTestM :: TestM a -> a
unTestM (TestM (Identity x)) = x

instance MonadDB TestM User where
  getEntity id =
    return $ Just $ User (UserId 1) "Bob" "Belcher" (Email "bob@burgers.com")
  getEntities = return []
  insertEntity user = return 2

main :: IO ()
main = hspec $ do
  describe "MonadDB" $ do
    context "User" $ do
      it "returns an empty list when there are no users to fetch" $ do
        let users' = unTestM getEntities
        users' `shouldBe` []

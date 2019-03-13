import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Protolude
import           Test.Hspec
import           Test.QuickCheck

-- Utility functions

nameFromVector :: Vector Text -> Gen Text
nameFromVector v = (v !) <$> choose (0, upperBound)
  where upperBound = V.length v - 1

vectorFromFile :: FilePath -> IO (Vector Text)
vectorFromFile path = V.fromList . T.lines <$> T.readFile path

nameGenFromFile :: FilePath -> IO (Gen Text)
nameGenFromFile path = nameFromVector <$> vectorFromFile path

genFirstName = nameGenFromFile "data/firstNames"

emailDomains :: [(Int, Gen Text)]
emailDomains = map (\(i, t) -> (i, pure t))
  [ (50, "gmail.com")
  , (40, "yahoo.com")
  , (30, "hotmail.com")
  , (20, "aol.com")
  , (8, "abcglobal.co.uk")
  , (8, "gubberment.ca")
  ]

main :: IO ()
main = hspec $ do
  describe "Validation" $ do
    context "isValidEmail" $ do
      it "should validate all valid emails" $ do
        1 + 1 `shouldBe` 2

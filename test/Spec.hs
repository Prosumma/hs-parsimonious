import Parsimonious
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "something" $ do
    it "works" $ do
      (2 :: Int) `shouldBe` 2

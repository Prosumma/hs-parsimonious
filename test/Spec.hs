import Parsimonious
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parsimonious.Parser" $ do
    it "is a functor" $ do
      let p = pure 3 :: Parser () () Int
      let r = parse () $ fmap (+ 1) p
      r `shouldBe` Right (4, ())
    it "is applicative" $ do
      let p = pure 3 :: Parser () () Int
      let f = pure (+ 1) :: Parser () () (Int -> Int)
      let r = parse () (f <*> p)
      r `shouldBe` Right (4, ())
    it "is a monad" $ do
      let p = pure 3 :: Parser () () Int
      let plus1 o = pure $ o + 1
      let r = parse () (p >>= plus1)
      r `shouldBe` Right (4, ())

{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Parsimonious
import Parsimonious.Text
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
  describe "string eq" $ do
    it "matches a string case-sensitively" $ do
      let p = string eq "abc" :: Parser Text () Text
      parse "abc" p `shouldBe` Right ("abc", "")
  describe "string eqi" $ do
    it "matches a string case-insensitively" $ do
      let p = string eqi "AbC" :: Parser Text () Text
      parse "abc" p `shouldBe` Right ("abc", "")

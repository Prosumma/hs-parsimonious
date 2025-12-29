module Parsimonious.Text
  ( TextParser,
    eq,
    eqi,
    satisfy,
    string,
  )
where

import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Text
import Parsimonious

type TextParser = Parser Text

satisfy :: (Char -> Bool) -> TextParser e Char
satisfy test = Parser $ \i -> case uncons i of
  Nothing -> throwReason EOF
  Just (c, i) ->
    if test c
      then pure (c, i)
      else throwReason NoMatch

eq :: Char -> TextParser e Char
eq c = satisfy (== c)

eqi :: Char -> TextParser e Char
eqi c = satisfy $ \c' -> Char.toLower c == Char.toLower c'

string :: (Char -> TextParser e Char) -> String -> TextParser e Text
string test s = pack . toList <$> mapM test s

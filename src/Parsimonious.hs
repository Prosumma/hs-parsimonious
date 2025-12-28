{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Parsimonious
  ( Reason (..),
    Result,
    ParseError (..),
    choose,
    parse,
    throwReason,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.Foldable
import Data.Tuple.Extra

data Reason = EOF | NoMatch | Empty deriving (Eq, Show)

data ParseError e = ParseError Reason Bool (Maybe e) deriving (Eq, Show)

type Result i e o = Either (ParseError e) (o, i)

newtype Parser i e o = Parser {unParse :: i -> Result i e o}

parse :: i -> Parser i e o -> Result i e o
parse = flip unParse

instance Functor (Parser i e) where
  fmap f (Parser parse) = Parser $ fmap (first f) . parse

instance Applicative (Parser i e) where
  pure o = Parser $ pure . (o,)
  (Parser f) <*> (Parser o) = Parser $ \i -> do
    (f, i) <- f i
    (o, i) <- o i
    pure (f o, i)

instance Monad (Parser i e) where
  return = pure
  (Parser p) >>= f = Parser $ \i -> do
    (o, i) <- p i
    parse i (f o)

instance MonadError (ParseError e) (Parser i e) where
  throwError = Parser . const . Left
  catchError (Parser p) handler = Parser $
    \i -> catchError (p i) $ parse i . handler

throwReason :: (MonadError (ParseError e) m) => Reason -> m o
throwReason r = throwError $ ParseError r False Nothing

instance Alternative (Parser i e) where
  empty = throwReason Empty
  (Parser lhs) <|> (Parser rhs) = Parser $ \i -> case lhs i of
    irr@(Left (ParseError _ True _)) -> irr
    Left _ -> rhs i
    ok -> ok

choose :: [Parser i e o] -> Parser i e o
choose = foldr' (<|>) $ throwReason Empty

instance (Semigroup o) => Semigroup (Parser e i o) where
  lhs <> rhs = liftA2 (<>) lhs rhs

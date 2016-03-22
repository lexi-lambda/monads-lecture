module Lecture.Either where

import Prelude hiding (Either(..))

data Either e a = Left e | Right a
  deriving (Show, Eq)

instance Functor (Either e) where
  fmap _ (Left l) = Left l
  fmap f (Right r) = Right (f r)

instance Applicative (Either e) where
  pure = Right
  (<*>) (Right f) (Right x) = Right (f x)
  (<*>) (Left e) _ = Left e
  (<*>) _ (Left e) = Left e

instance Monad (Either e) where
  (>>=) (Left l)  _ = Left l
  (>>=) (Right r) f = f r

safeHead :: [a] -> Either String a
safeHead []    = Left "safeHead: called with the empty list"
safeHead (x:_) = Right x

safeTail :: [a] -> Either String [a]
safeTail []     = Left "safeTail: called with the empty list"
safeTail (_:xs) = Right xs

safeDiv :: (Eq a, Fractional a) => a -> a -> Either String a
safeDiv _ 0 = Left "safeDiv: attempted to divide by zero"
safeDiv a b = Right (a / b)

andThen :: Either e a -> (a -> Either e b) -> Either e b
andThen (Left l)  _ = Left l
andThen (Right r) f = f r

divideNumbers :: [Double] -> Either String Double
divideNumbers ns =
  safeHead ns >>= \a ->
  safeTail ns >>= \rest ->
  safeHead rest >>= \b ->
  safeDiv a b


divideNumbers' :: [Double] -> Either String Double
divideNumbers' ns = do
  a <- safeHead ns
  rest <- safeTail ns
  b <- safeHead rest
  safeDiv a b

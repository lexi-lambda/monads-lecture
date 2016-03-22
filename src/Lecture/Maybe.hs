module Lecture.Maybe where

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv a b = Just (a / b)

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing  _ = Nothing
andThen (Just x) f = f x

divideNumbers :: [Double] -> Maybe Double
divideNumbers ns =
  safeHead ns `andThen` \a ->
  safeTail ns `andThen` \rest ->
  safeHead rest `andThen` \b ->
  safeDiv a b

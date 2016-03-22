module Lecture.World (
  World,
  runWorld,
  getWorld
) where

newtype World v a = World { runWorld :: v -> a }

instance Functor (World v) where
  fmap f (World a) = World (fmap f a)

instance Applicative (World v) where
  pure = World . const
  (World f) <*> (World a) = World (\v -> f v (a v))

instance Monad (World v) where
  (World a) >>= f = World $ \v -> runWorld (f (a v)) v

getWorld :: World v v
getWorld = World id

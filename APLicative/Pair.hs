{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Pair (Pair(..)) where

import Naperian
import Dimension

import Data.Kind

type Pair :: * -> *
data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair x y = Pair (f x) (g y)

instance Naperian Pair where
  type Log Pair = Bool
  lookup (Pair a _) False = a
  lookup (Pair _ a) True = a
  positions = Pair False True

instance Foldable Pair where
  foldr f e (Pair x y) = (f x . f y) e

instance Traversable Pair where
  traverse f (Pair x y) = Pair <$> f x <*> f y

instance Dimension Pair where size _ = 2

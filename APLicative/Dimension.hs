{-# LANGUAGE StandaloneKindSignatures #-}

module Dimension (Dimension(..), transpose) where

import Prelude hiding (lookup)

import Naperian

import Data.Foldable
import Data.Kind

type Dimension :: (* -> *) -> Constraint
class (Applicative f, Traversable f, Naperian f) => Dimension f where
  size :: f a -> Int
  size = length . toList

transpose :: (Dimension f, Dimension g) => f (g a) -> g (f a)
transpose = tabulate . fmap tabulate . flip . fmap lookup . lookup

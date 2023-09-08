{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Vector (Vector(..)) where

import Prelude hiding (lookup)

import Naperian
import Dimension

import Data.Kind
import qualified Data.Nat as N
import qualified Data.Fin as F

type Vector :: N.Nat -> * -> *
data Vector n a where
  VNil :: Vector N.Z a
  VCons :: a -> Vector n a -> Vector (N.S n) a

instance Functor (Vector n) where
  fmap _ VNil = VNil
  fmap f (VCons a as) = VCons (f a) (fmap f as)

type Count :: N.Nat -> Constraint
class Count n where
  vreplicate :: a -> Vector n a
  vlength :: Vector n a -> Int

instance Count N.Z where
  vreplicate _ = VNil
  vlength _ = 0

instance Count n => Count (N.S n) where
  vreplicate a = VCons a (vreplicate a)
  vlength (VCons a as) = 1 + vlength as

vzipWith :: Vector n (a -> b) -> Vector n a -> Vector n b
vzipWith VNil VNil = VNil
vzipWith (VCons f fs) (VCons x xs) = VCons (f x) (vzipWith fs xs)

instance Count n => Applicative (Vector n) where
  pure = vreplicate
  (<*>) = vzipWith

vlookup :: Vector n a -> F.Fin n -> a
vlookup (VCons a _) F.FZ = a
vlookup (VCons _ x) (F.FS n) = vlookup x n

instance Count n => Naperian (Vector n) where
  type Log (Vector n) = F.Fin n
  lookup = vlookup
  positions = viota (vreplicate ()) where
    viota :: Vector m () -> Vector m (F.Fin m)
    viota VNil = VNil
    viota (VCons _ x) = VCons F.FZ (fmap F.FS (viota x))

instance Foldable (Vector n) where
  foldr _ e VNil = e
  foldr f e (VCons x xs) = f x (foldr f e xs)

instance Traversable (Vector n) where
  traverse _ VNil = pure VNil
  traverse f (VCons x xs) = VCons <$> f x  <*> traverse f xs

instance Count n => Dimension (Vector n) where size = vlength -- optional optimisation

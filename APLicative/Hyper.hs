{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Hyper (Hyper(..), Shapely(..), unary, binary, forceRepl, hzipWith, reduceBy, transposeHyper) where

import Dimension

import Data.Foldable
import Data.Kind

import GHC.TypeLits

type Shapely :: [* -> *] -> Constraint
class Shapely fs where
  hreplicate :: a -> Hyper fs a
  hsize :: Hyper fs a -> Int

instance Shapely '[] where
  hreplicate = Scalar
  hsize _ = 1

first :: Shapely fs => Hyper fs a -> a
first (Scalar a) = a
first (Prism x) = (head . toList . first) x
first (Repl x) = first x

instance (Dimension f, Shapely fs) => Shapely (f ': fs) where
  hreplicate a = Prism ((hreplicate . pure) a)
  hsize (Prism x) = size (first x) * hsize x
  hsize (Repl x) = hsize x

type Hyper :: [* -> *] -> * -> *
data Hyper dims a where
  Scalar :: a -> Hyper '[] a
  Prism :: (Dimension f, Shapely fs) => Hyper fs (f a) -> Hyper (f ': fs) a
  Repl :: (Dimension f, Shapely fs) => Hyper fs a -> Hyper (f ': fs) a
  -- Trans :: (Dimension f, Dimension g, Shapely fs) => Hyper (f ': g ': fs) a -> Hyper (g ': f ': fs) a

instance Show a => Show (Hyper '[] a) where
  show (Scalar a) = show a

instance (Show a, Dimension f, Show (Hyper fs (f a)), Show (Hyper fs a)) => Show (Hyper (f ': fs) a) where
  show (Prism x) = "<" ++ show x ++ ">"
  show (Repl x) = "R<" ++ show x ++ ">R"

forceRepl :: Shapely fs => Hyper fs a -> Hyper fs a
forceRepl (Repl x) = Prism (fmap pure x)
forceRepl (Prism x) = Prism (forceRepl x)
forceRepl (Scalar x) = Scalar x

-- | smart constructor for @Trans@ such that @Trans . Trans == Id@
-- trans :: (Dimension f, Dimension g, Shapely fs) => Hyper (f ': g ': fs) a -> Hyper (g ': f ': fs) a
-- trans (Trans x) = x
-- trans x = Trans x

instance Functor (Hyper fs) where
  fmap f (Scalar a) = Scalar (f a)
  fmap f (Prism x) = Prism ((fmap . fmap) f x)
  fmap f (Repl x) = Repl (fmap f x)

azipWith :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
azipWith f a b = f <$> a <*> b

hzipWith :: (a -> b -> c) -> Hyper fs a -> Hyper fs b -> Hyper fs c
hzipWith f (Scalar a) (Scalar b) = Scalar (f a b)
hzipWith f (Prism x) (Prism y) = Prism ((hzipWith . azipWith) f x y)
hzipWith f (Repl x) (Repl y) = Repl (f <$> x <*> y)
hzipWith f (Prism x) (Repl y) = Prism (((\xs y' -> fmap (\x' -> f x' y') xs)) <$> x <*> y)
hzipWith f (Repl x) (Prism y) = Prism (((\x' ys -> fmap (\y' -> f x' y') ys)) <$> x <*> y)

instance Shapely fs => Applicative (Hyper fs) where
  pure = hreplicate
  (<*>) = hzipWith (\f x -> f x)

-- | fold along the innermost axis
reduceBy :: (a -> a -> a) -> a -> Hyper (f ': fs) a -> Hyper fs a
reduceBy f e (Prism x) = fmap (foldr f e) x
reduceBy f e x@(Repl _) = reduceBy f e (forceRepl x)

transposeHyper :: Hyper (f ': g ': fs) a -> Hyper (g ': f ': fs) a
transposeHyper (Prism (Prism x)) = Prism (Prism (fmap transpose x))
transposeHyper (Repl (Repl x)) = Repl (Repl x)
transposeHyper (Prism (Repl x)) = Repl (Prism x)
transposeHyper (Repl (Prism x)) = Prism (Repl x)

unary :: Shapely fs => (a -> b) -> Hyper fs a -> Hyper fs b
unary = fmap

-- | Two shapes are alignable when one is the prefix of the other (recall: head is innermost).
-- | The smaller one is lifted to the shape of the greater one.
type Alignable :: [* -> *] -> [* -> *] -> Constraint
class (Shapely fs, Shapely gs) => Alignable fs gs where
  align :: Hyper fs a -> Hyper gs a

instance Alignable '[] '[] where
  align = id

instance (Dimension f, Alignable fs gs) => Alignable (f ': fs) (f ': gs) where
  align (Prism x) = Prism (align x)
  align (Repl x) = Repl (align x)

instance (Dimension f, Shapely fs) => Alignable '[] (f ': fs) where
  align (Scalar a) = hreplicate a

-- NOTE why can't this be written as just the last two instances?

type Max :: [* -> *] -> [* -> *] -> [* -> *]
type family Max fs gs where
  Max '[] '[] = '[]
  Max '[] (f ': gs) = f ': gs
  Max (f ': fs) '[] = f ': fs
  Max (f ': fs) (f ': gs) = f ': Max fs gs

data IsDefined e = Defined | Undefined e

-- | utility function for friendlier error messages
type IsCompatible :: [* -> *] -> [* -> *] -> IsDefined Symbol
type family IsCompatible fs gs where
  IsCompatible '[] '[] = Defined
  IsCompatible '[] (f ': _) = Defined
  IsCompatible (f ': _) '[] = Defined
  IsCompatible (f ': fs) (f ': gs) = IsCompatible fs gs
  IsCompatible (f ': _) (g ': _) = Undefined "Mismatching dimensions"

binary :: (IsCompatible fs gs ~ Defined, Max fs gs ~ hs, Alignable fs hs, Alignable gs hs)
  => (a -> b -> c) -> Hyper fs a -> Hyper gs b -> Hyper hs c
binary f x y = f <$> align x <*> align y

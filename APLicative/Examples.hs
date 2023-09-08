{-# LANGUAGE DataKinds #-}

module APLicative.Examples where

import Vector
import Pair
import Hyper
import Naperian

import Data.Foldable
import qualified Data.Nat as N
import qualified Data.Fin as F

type Zero = N.Z
type One = N.S Zero
type Two = N.S One
type Three = N.S Two
type Four = N.S Three

v123 = VCons 1 (VCons 2 (VCons 3 VNil)) :: Vector Three Integer
v456 = VCons 4 (VCons 5 (VCons 6 VNil)) :: Vector Three Integer

vv123456 = Pair v123 v456 :: Pair (Vector Three Integer)

h123456 = Prism (Prism (Scalar vv123456)) :: Hyper '[Vector Three, Pair] Integer
-- >>> h123456
-- <<((1,2,3,),(4,5,6,))>>

-- three different representations for the same vector
-- one define plainly
-- one defined with hreplicate (undesirable)
-- one defined with Repl (desirable, but potentially confusing)
h123123 = Prism (Prism (Scalar (Pair v123 v123))) :: Hyper '[Vector Three, Pair] Integer
h123123' = Prism (hreplicate v123) :: Hyper '[Vector Three, Pair] Integer
h123123'' = Prism (Repl (Scalar v123)) :: Hyper '[Vector Three, Pair] Integer
-- >>> forceRepl h123123''
-- <<((1,2,3,),(1,2,3,))>>

instance Show a => Show (Pair a) where
  show (Pair a b) = "(" ++ show a ++ "," ++ show b ++ ")"

instance Show a => Show (Vector n a) where
  show v = "(" ++ concatMap (\x -> show x ++ ",") (toList v) ++ ")"

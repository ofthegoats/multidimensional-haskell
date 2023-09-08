{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Naperian where

import Data.Kind

-- | Class for "representable" functors, i.e. functors of statically known shape and size.
-- | NB @lookup == Inverse (tabulate)@
type Naperian :: (* -> *) -> Constraint
class Functor f => Naperian f where
  type Log f -- base type @p@ of positions, st. f a ≃ p -> a

  -- | Extract a value given a position @Log f@ i.e. @p@
  lookup :: f a -> Log f -> a

  -- | Build an f-struct with each position @p@ valued @h p@
  tabulate :: (Log f -> a) -> f a
  tabulate h = fmap h positions

  -- | Build an f-struct with each position @p@ valued @p@
  positions :: f (Log f)
  positions = tabulate id

  {-# MINIMAL lookup, (tabulate | positions) #-}

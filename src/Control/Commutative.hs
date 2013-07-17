module Control.Commutative
  ( Commutative ()
  , runCommutative
  )
where

import Control.Applicative
import Data.Profunctor


data Commutative r a = Commutative {
  runCommutative :: r -> r -> a
}

instance Profunctor Commutative where
  dimap f g c = Commutative scope where
    scope r1' r2' = x' where
      r1 = f r1'
      r2 = f r2'
      x' = g x
      x = runCommutative c r1 r2

instance Functor (Commutative r) where
  fmap = rmap

instance Applicative (Commutative r) where
  pure = Commutative . const . const
  cf <*> cx = Commutative scope where
    scope r1 r2 = f x where
      f = runCommutative cf r1 r2
      x = runCommutative cx r1 r2

instance Monad (Commutative r) where
  return = Commutative . const . const
  cx >>= f = Commutative scope where
    scope r1 r2 = y where
      x = runCommutative cx r1 r2
      y = runCommutative cy r1 r2
      cy = f x

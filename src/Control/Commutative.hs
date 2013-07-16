module Control.Commutative
  ( Commutative ()
  , runCommutative
  )
where

import Control.Applicative
import Data.Profunctor


data Commutative a b = Commutative {
  runCommutative :: a -> a -> b
}

instance Profunctor Commutative where
  dimap f g c = Commutative scope where
    scope a1' a2' = b' where
      a1 = f a1'
      a2 = f a2'
      b' = g b
      b = runCommutative c a1 a2

instance Functor (Commutative a) where
  fmap = rmap

instance Applicative (Commutative a) where
  pure = Commutative . const . const
  cf <*> cx = Commutative scope where
    scope a1 a2 = f x where
      f = runCommutative cf a1 a2
      x = runCommutative cx a1 a2

instance Monad (Commutative a) where
  return = Commutative . const . const
  cx >>= f = Commutative scope where
    scope a1 a2 = y where
      x = runCommutative cx a1 a2
      y = runCommutative cy a1 a2
      cy = f x

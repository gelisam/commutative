module Control.Commutative
  ( Commutative ()
  , runCommutative
  , on
  , DepEq (..)
  , depEq
  , eq_and_more
  , observe_eq
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

on :: Profunctor f => f r' a -> (r -> r') -> f r a
on = flip lmap

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


-- the type of the second value(s)
-- depends on whether the first are equal.
data DepEq r a b = Same (r, a) | Different (r, b) (r, b)
                   deriving (Eq, Ord, Show)

-- not commutative because the values of type b can give away
-- whether the arguments were swapped or not.
depEq :: Ord r => a -> (r, b) -> (r, b) -> DepEq r a b
depEq x (r, y) (r', y') = case compare r r' of
                            LT -> Different (r, y) (r', y')
                            EQ -> Same (r, x)
                            GT -> Different (r', y') (r, y)

-- commutative because the values of type b cannot give away
-- whether the arguments were swapped or not.
-- 
-- More formally, let (x , y ) = r
--                    (x', y') = r'
--                    f = runCommutative eq_and_more
-- we want to show that f r r' = f r' r.
-- 
-- wlog, let x <= x'. Then, either
--   f r r' = Same (x, ()) = f r' r
-- or
--   f r r' = Different (x, y) (x', y') = f r' r
-- qed.
eq_and_more :: Ord a => Commutative (a, b) (DepEq a () b)
eq_and_more = Commutative $ depEq ()

-- simpler variant
observe_eq :: Ord a => Commutative a (Either a (a, a))
observe_eq = dimap wrap unwrap eq_and_more where
  wrap r = (r, ())
  unwrap (Same (x, ())) = Left x
  unwrap (Different (x, ()) (x', ())) = Right (x, x')

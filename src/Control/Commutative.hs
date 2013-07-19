module Control.Commutative
  ( Commutative ()
  , runCommutative
  , DepEq (..)
  , depEq
  , observe_eq
  , observe_eq_and
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
-- More formally, let (x , y ) = observe r
--                    (x', y') = observe r'
--                    f = observe_eq_and observe
-- we want to show that f r r' = f r' r.
-- 
-- wlog, let x <= x'. Then, either
--   f r r' = Same (x, ()) = f r' r
-- or
--   f r r' = Different (x, y) (x', y') = f r' r
-- qed.
observe_eq_and :: Ord a => (r -> (a, b)) -> Commutative r (DepEq a () b)
observe_eq_and observe = Commutative scope where
  scope r1 r2 = depEq () (observe r1) (observe r2)

-- simpler variant
observe_eq :: Ord a => (r -> a) -> Commutative r (Either a (a, a))
observe_eq observe = simplify <$> observe_eq_and observe' where
  observe' r = (observe r, ())
  simplify (Same (x, ())) = Left x
  simplify (Different (x, ()) (x', ())) = Right (x, x')

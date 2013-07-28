module Control.Commutative
  ( Commutative ()
  , runCommutative
  , on
  , Unordered_Either (..)
  , distinguish
  , distinguishBy
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


-- a pair of Either a b values, in which the observation is tainted
-- if the constructors are both Left or both Right.
data Unordered_Either a a' b b' = LL a' | RR b' | LR a b

-- The one function whose commutativity ensures that all other computations
-- of type Commutative are commutative. Better write a proof!
-- 
-- let (&) = distinguish
-- we want to show that r1 & r2 = r2 & r1.
-- there are four cases:
--   Left  x1 & Left  x2 = LL ()    = Left  x2 & Left  x1
--   Right y1 & Right y2 = RR ()    = Right y2 & Right y1
--   Left  x1 & Right y2 = LR x1 y2 = Right y2 & Left  x1
--   Right y1 & Left  x2 = LR x2 y1 = Left  x2 & Right y1
-- qed.
distinguish :: Commutative (Either a b) (Unordered_Either a ()
                                                          b ())
distinguish = Commutative $ merge where
  merge (Left  _) (Left  _) = LL ()
  merge (Right _) (Right _) = RR ()
  merge (Left  x) (Right y) = LR x y
  merge (Right y) (Left  x) = LR x y

-- simpler version with a predicate instead of Either
distinguishBy :: (r -> Bool) -> Commutative r (Either Bool (r, r))
distinguishBy p = do bb <- distinguish `on` isP
                     case bb of
                       LL ()    -> return $ Left False
                       RR ()    -> return $ Left True
                       LR r1 r2 -> return $ Right (r1, r2)
                  where
  isP x = (if p x then Right else Left) x

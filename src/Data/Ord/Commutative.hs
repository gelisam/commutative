module Data.Ord.Commutative where

import Control.Applicative

import Control.Commutative


-- Minimal complete definition:
-- either commutative_sort2, or both commutative_min and commutative_max
class Commutative_Ord a where
  commutative_sort2 :: Commutative a (a, a)
  commutative_sort2 = (,) <$> commutative_min <*> commutative_max
  
  commutative_min, commutative_max :: Commutative a a
  commutative_min = fst <$> commutative_sort2
  commutative_max = snd <$> commutative_sort2

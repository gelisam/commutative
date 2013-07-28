module Data.Eq.Commutative where

import Control.Applicative
import Control.Commutative


-- Minimal complete definition:
-- either commutative_eq or commutative_neq
class Commutative_Eq a where
  commutative_eq, commutative_neq :: Commutative a Bool
  commutative_eq = not <$> commutative_neq
  commutative_neq = not <$> commutative_eq

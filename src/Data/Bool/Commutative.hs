module Data.Bool.Commutative where

import Control.Applicative

import Control.Commutative
import Data.Unordered


-- |
-- prop> (x || y) == runCommutative commutative_or x y
commutative_or :: Commutative Bool Bool
commutative_or = or <$> unorder where
  or FF = False
  or _  = True

-- |
-- prop> (x && y) == runCommutative commutative_and x y
commutative_and :: Commutative Bool Bool
commutative_and = and <$> unorder where
  and TT = True
  and _  = False

-- |
-- prop> (x /= y) == runCommutative commutative_xor x y
commutative_xor :: Commutative Bool Bool
commutative_xor = xor <$> unorder where
  xor TF = True
  xor _  = False

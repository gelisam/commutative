{-# OPTIONS -XNPlusKPatterns #-}
module Data.Nat where

import Control.Applicative
import Control.Commutative
import Data.Unordered


data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Show)

-- |
-- >>> fromInteger 3 :: Nat
-- Succ (Succ (Succ Zero))
-- 
-- prop> if x >= 0 && y >= 0           then fromInteger (x + y)    == (fromInteger x + fromInteger y :: Nat) else True
-- prop> if x >= 0 && y >= 0           then fromInteger (x * y)    == (fromInteger x * fromInteger y :: Nat) else True
-- prop> if x >= 0 && y >= 0 && x >= y then fromInteger (x - y)    == (fromInteger x - fromInteger y :: Nat) else True
-- prop> if x >= 0 && y >= 0           then fromInteger (signum x) == (signum (fromInteger x)        :: Nat) else True
instance Num Nat where
  Zero   + x' = x'
  Succ x + x' = Succ (x + x')
  
  Zero   * x' = Zero
  Succ x * x' = x' + x * x'
  
  x      - Zero    = x
  Zero   - x'      = negate x'
  Succ x - Succ x' = x - x'
  
  negate = error "Nat cannot be negative"
  abs = id
  
  signum Zero     = Zero
  signum (Succ _) = Succ Zero
  
  fromInteger 0     = Zero
  fromInteger (x+1) = Succ (fromInteger x)

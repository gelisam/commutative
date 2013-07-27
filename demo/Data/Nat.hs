{-# OPTIONS -XNPlusKPatterns -XTypeFamilies #-}
module Data.Nat where

import Control.Applicative
import Control.Commutative
import Data.List
import Data.Unordered
import Test.QuickCheck


data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Show)

instance Arbitrary Nat where
  arbitrary = fmap fromList arbitrary where
    fromList []      = Zero
    fromList (():xs) = Succ (fromList xs)
  shrink = unfoldr shrinkNat where
    shrinkNat Zero     = Nothing
    shrinkNat (Succ x) = Just (x, x)

-- | Non-commutative versions of (+) and (*).
-- 
-- >>> fromInteger 3 :: Nat
-- Succ (Succ (Succ Zero))
-- 
-- prop>                fromEnum x + fromEnum y == fromEnum (x + y :: Nat)
-- prop>                fromEnum x * fromEnum y == fromEnum (x * y :: Nat)
-- prop> if x >= y then fromEnum x - fromEnum y == fromEnum (x - y :: Nat) else True
-- prop>                signum (fromEnum x)     == fromEnum (signum x :: Nat)
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

-- |
-- >>> toEnum 3 :: Nat
-- Succ (Succ (Succ Zero))
-- 
-- >>> fromEnum (Succ (Succ (Succ Zero)))
-- 3
instance Enum Nat where
  succ = Succ
  pred (Succ x) = x
  
  toEnum 0     = Zero
  toEnum (x+1) = Succ (toEnum x)
  
  fromEnum Zero     = 0
  fromEnum (Succ x) = succ (fromEnum x)


data Unordered_Nat = ZZ | ZS Nat | SS Unordered_Nat

instance Unorderable Nat where
  type Unordered Nat = Unordered_Nat
  unorder = do nn <- distinguish `on` isZero
               case nn of
                 LL ()   -> return $ ZZ
                 LR () x -> return $ ZS x
                 RR ()   -> SS <$> unorder `on` pred
            where
    isZero :: Nat -> Either () Nat
    isZero Zero     = Left ()
    isZero (Succ x) = Right x

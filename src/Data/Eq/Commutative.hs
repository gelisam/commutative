module Data.Eq.Commutative where

import Control.Applicative
import Data.Maybe

import Control.Commutative
import Data.Either.Extra
import Data.List.Extra


-- Minimal complete definition:
-- either commutative_eq or commutative_neq
class Commutative_Eq a where
  commutative_eq, commutative_neq :: Commutative a Bool
  commutative_eq = not <$> commutative_neq
  commutative_neq = not <$> commutative_eq


-- |
-- prop> (x == y) == runCommutative commutative_eq x (y :: ())
-- prop> (x /= y) == runCommutative commutative_neq x (y :: ())
instance Commutative_Eq () where
  commutative_eq = return True

-- |
-- prop> (x == y) == runCommutative commutative_eq x (y :: Bool)
-- prop> (x /= y) == runCommutative commutative_neq x (y :: Bool)
instance Commutative_Eq Bool where
  commutative_eq = do diff <- distinguishBy id
                      case diff of
                        Left  _ -> return True
                        Right _ -> return False

-- |
-- prop> (x == y) == runCommutative commutative_eq x (y :: Maybe Bool)
-- prop> (x /= y) == runCommutative commutative_neq x (y :: Maybe Bool)
instance Commutative_Eq a => Commutative_Eq (Maybe a) where
  commutative_eq = do diff <- distinguishBy isJust
                      case diff of
                        Left False -> return True
                        Left True  -> commutative_eq `on` fromJust
                        Right _    -> return False

-- |
-- prop> (x == y) == runCommutative commutative_eq x (y :: Either Bool Bool)
-- prop> (x /= y) == runCommutative commutative_neq x (y :: Either Bool Bool)
instance (Commutative_Eq a, Commutative_Eq b) => Commutative_Eq (Either a b) where
  commutative_eq = do diff <- distinguishBy isRight
                      case diff of
                        Left False -> commutative_eq `on` fromLeft
                        Left True  -> commutative_eq `on` fromRight
                        Right _    -> return False

-- |
-- prop> (x == y) == runCommutative commutative_eq x (y :: (Bool, Bool))
-- prop> (x /= y) == runCommutative commutative_neq x (y :: (Bool, Bool))
instance (Commutative_Eq a, Commutative_Eq b) => Commutative_Eq (a, b) where
  commutative_eq = do eq1 <- commutative_eq `on` fst
                      if eq1
                        then commutative_eq `on` snd
                        else return False

-- |
-- prop> (x == y) == runCommutative commutative_eq x (y :: [Bool])
-- prop> (x /= y) == runCommutative commutative_neq x (y :: [Bool])
instance Commutative_Eq a => Commutative_Eq [a] where
  commutative_eq = do diff <- distinguishBy isCons
                      case diff of
                        Left False -> return True
                        Left True  -> commutative_eq `on` fromCons
                        Right _    -> return False

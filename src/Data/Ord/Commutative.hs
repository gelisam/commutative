module Data.Ord.Commutative where

import Control.Applicative
import Data.List
import Data.Maybe

import Control.Commutative
import Data.Either.Extra
import Data.List.Extra

-- $setup
-- >>> let  list2 (x, y) = [x, y]


-- Minimal complete definition:
-- either commutative_sort2, or both commutative_min and commutative_max
class Commutative_Ord a where
  commutative_sort2 :: Commutative a (a, a)
  commutative_sort2 = (,) <$> commutative_min <*> commutative_max
  
  commutative_min, commutative_max :: Commutative a a
  commutative_min = fst <$> commutative_sort2
  commutative_max = snd <$> commutative_sort2


-- |
-- prop> sort [x, y] == list2 (runCommutative commutative_sort2 x (y :: ()))
-- prop> min x y == runCommutative commutative_min x (y :: ())
-- prop> max x y == runCommutative commutative_max x (y :: ())
instance Commutative_Ord () where
  commutative_sort2 = return ((), ())

-- |
-- prop> sort [x, y] == list2 (runCommutative commutative_sort2 x (y :: Bool))
-- prop> min x y == runCommutative commutative_min x (y :: Bool)
-- prop> max x y == runCommutative commutative_max x (y :: Bool)
instance Commutative_Ord Bool where
  commutative_sort2 = do diff <- distinguishBy id
                         case diff of
                           Left True  -> return (True, True)
                           Left False -> return (False, False)
                           Right bb -> return bb

-- |
-- prop> sort [x, y] == list2 (runCommutative commutative_sort2 x (y :: Maybe Bool))
-- prop> min x y == runCommutative commutative_min x (y :: Maybe Bool)
-- prop> max x y == runCommutative commutative_max x (y :: Maybe Bool)
instance Commutative_Ord a => Commutative_Ord (Maybe a) where
  commutative_sort2 = do
      diff <- distinguishBy isJust
      case diff of
        Left False -> return (Nothing, Nothing)
        Left True  -> just2 <$> commutative_sort2 `on` fromJust
        Right mm   -> return mm
    where
        just2 (x, y) = (Just x, Just y)

-- |
-- prop> sort [x, y] == list2 (runCommutative commutative_sort2 x (y :: Either Bool Bool))
-- prop> min x y == runCommutative commutative_min x (y :: Either Bool Bool)
-- prop> max x y == runCommutative commutative_max x (y :: Either Bool Bool)
instance (Commutative_Ord a, Commutative_Ord b) => Commutative_Ord (Either a b) where
  commutative_sort2 = do
      diff <- distinguishBy isRight
      case diff of
        Left False -> left2 <$> commutative_sort2 `on` fromLeft
        Left True  -> right2 <$> commutative_sort2 `on` fromRight
        Right ee   -> return ee
    where
      left2  (x, y) = (Left x, Left y)
      right2 (x, y) = (Right x, Right y)

-- |
-- prop> sort [x, y] == list2 (runCommutative commutative_sort2 x (y :: (Bool, Bool)))
-- prop> min x y == runCommutative commutative_min x (y :: (Bool, Bool))
-- prop> max x y == runCommutative commutative_max x (y :: (Bool, Bool))
instance (Eq a, Commutative_Ord a, Commutative_Ord b) => Commutative_Ord (a, b) where
  commutative_sort2 = do
      (min1, max1) <- commutative_sort2 `on` fst
      diff <- distinguishBy (fst_is max1)
      case diff of
        Left False -> error "commutative_sort2 is inconsistent with (==)"
        Left True  -> cons2 max1 <$> commutative_sort2 `on` snd
        Right pp   -> return pp
    where
      fst_is x p = fst p == x
      cons2 x (y1, y2) = ((x, y1), (x, y2))

-- |
-- prop> sort [x, y] == list2 (runCommutative commutative_sort2 x (y :: [Bool]))
-- prop> min x y == runCommutative commutative_min x (y :: [Bool])
-- prop> max x y == runCommutative commutative_max x (y :: [Bool])
instance (Eq a, Commutative_Ord a) => Commutative_Ord [a] where
  commutative_sort2 = do
      diff <- distinguishBy isCons
      case diff of
        Right ll   -> return ll
        Left False -> return ([], [])
        Left True  -> cons2 <$> commutative_sort2 `on` fromCons
    where
      cons2 ((x,xs), (y,ys)) = (x:xs, y:ys)

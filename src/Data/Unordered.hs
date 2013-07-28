{-# OPTIONS -XTypeFamilies #-}
module Data.Unordered where

import Control.Applicative
import Data.Maybe

import Control.Commutative
import Data.Either.Extra
import Data.Ord.Commutative


class Unorderable a where
  type Unordered a :: *
  unorder :: Commutative a (Unordered a)

observe_both :: Unorderable a => (r -> a) -> Commutative r (Unordered a)
observe_both = on unorder
  


instance Unorderable () where
  type Unordered () = ()
  unorder = return ()


data Unordered_Bool = TT | TF | FF
                      deriving (Eq, Ord, Show)

instance Unorderable Bool where
  type Unordered Bool = Unordered_Bool
  unorder = do bb <- distinguish `on` constructor
               case bb of
                 LL ()    -> return FF
                 LR () () -> return TF
                 RR ()    -> return TT
            where
    constructor :: Bool -> Either () ()
    constructor False = Left  ()
    constructor True  = Right ()


data Unordered_Maybe a aa = NN | NJ a | JJ aa
                            deriving (Eq, Ord, Show)

instance Unorderable a => Unorderable (Maybe a) where
  type Unordered (Maybe a) = Unordered_Maybe a (Unordered a)
  unorder = do jj <- distinguish `on` constructor
               case jj of
                 LL ()   -> return $ NN
                 LR () x -> return $ NJ x
                 RR ()   -> JJ <$> unorder `on` fromJust
            where
    constructor :: Maybe a -> Either () a
    constructor Nothing  = Left ()
    constructor (Just x) = Right x


-- Unordered_Either is defined in Control.Commutative

instance (Unorderable a, Unorderable b) => Unorderable (Either a b) where
  type Unordered (Either a b) = Unordered_Either a (Unordered a)
                                                 b (Unordered b)
  unorder = do ee <- distinguish
               case ee of
                 LL ()  -> LL <$> unorder `on` fromLeft
                 LR x y -> return $ LR x y
                 RR ()  -> RR <$> unorder `on` fromRight


-- if the As are different, the pairs are given in a sorted order
data Unordered_Pair a b bb = Same a bb | Diff (a, b) (a, b)
                             deriving (Eq, Ord, Show)

instance (Eq a, Commutative_Ord a, Unorderable b) => Unorderable (a, b) where
  type Unordered (a, b) = Unordered_Pair a b (Unordered b)
  unorder = do
      (min1, max1) <- commutative_sort2 `on` fst
      diff <- distinguishBy (fst_is max1)
      case diff of
        Left False    -> error "commutative_sort2 is inconsistent with (==)"
        Left True     -> Same max1 <$> unorder `on` snd
        Right (p, p') -> return $ Diff p p'
    where
      fst_is x p = fst p == x

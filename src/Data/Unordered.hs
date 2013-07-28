{-# OPTIONS -XTypeFamilies #-}
module Data.Unordered where

import Control.Applicative
import Data.Maybe

import Control.Commutative


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
            where
    fromLeft :: Either a b -> a
    fromLeft (Left x) = x
    
    fromRight :: Either a b -> b
    fromRight (Right y) = y

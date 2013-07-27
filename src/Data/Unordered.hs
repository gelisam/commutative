{-# OPTIONS -XTypeFamilies -XNPlusKPatterns #-}
module Data.Unordered where

import Control.Applicative
import Control.Commutative
import Data.Maybe
import Data.Profunctor


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


-- either?


data Unordered_Nat = ZZ | ZS Int | SS Unordered_Nat
                     deriving (Eq, Ord, Show)

instance Unorderable Int where
  type Unordered Int = Unordered_Nat
  unorder = do nn <- distinguish `on` isZero
               case nn of
                 LL ()   -> return $ ZZ
                 LR () x -> return $ ZS x
                 RR ()   -> SS <$> unorder `on` pred
            where
    isZero :: Int -> Either () Int
    isZero 0     = Left ()
    isZero (x+1) = Right x

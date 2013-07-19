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
  unorder = do bb <- observe_eq
               case bb of
                 Left True  -> return TT
                 Right _    -> return TF
                 Left False -> return FF


data Unordered_Maybe a aa = NN | NJ a | JJ aa
instance Unorderable a => Unorderable (Maybe a) where
  type Unordered (Maybe a) = Unordered_Maybe a (Unordered a)
  unorder = do jj <- eq_and_more `on` just_and_more
               case jj of
                 Same (False, _)     -> return $ NN
                 Different (False, _)
                           (True, x) -> return $ NJ x
                 Same (True, _)      -> JJ <$> unorder `on` fromJust
            where
    just_and_more Nothing  = (False, undefined)
    just_and_more (Just x) = (True, x)

data Unordered_Nat = ZZ | ZS Int | SS Unordered_Nat
instance Unorderable Int where
  type Unordered Int = Unordered_Nat
  unorder = do ss <- eq_and_more `on` succ_and_more
               case ss of
                 Same (False, _)     -> return $ ZZ
                 Different (False, _)
                           (True, x) -> return $ ZS x
                 Same (True, _)      -> SS <$> unorder `on` pred
            where
    succ_and_more 0     = (False, undefined)
    succ_and_more (x+1) = (True, x)
                 

instance (Ord a, Unorderable b) => Unorderable (a, b) where
  type Unordered (a, b) = DepEq a (Unordered b) b
  unorder = do pp <- eq_and_more
               case pp of
                 Same (a, ()) -> do
                   bb <- unorder `on` snd
                   return $ Same (a, bb)
                 Different (a, b) (a', b') ->
                   return $ Different (a, b) (a', b')

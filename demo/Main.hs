import Control.Commutative
import Data.Nat
import Data.Maybe
import Data.Unordered


-- $setup
-- >>> let env = [(1,'a'), (5,'b'), (10,'c'), (15,'d'), (20, 'e')] :: [(Int, Char)]


-- |
-- >>> runCommutative eq_int 10 10
-- True
-- >>> runCommutative eq_int 5 10
-- False
eq_int :: Commutative Int Bool
eq_int = do
  zz <- observe_both (== (0 :: Int))
  case zz of
    TT -> return True        -- (0 == 0)
    TF -> return False       -- (0 != succ y), and also (succ x != 0)
    FF -> eq_int `on` pred   -- (succ x == succ y) iff (x == y)


-- |
-- >>> lookupBy eq_int 10 env
-- Just 'c'
lookupBy :: Commutative a Bool -> a -> [(a, b)] -> Maybe b
lookupBy eq x [] = Nothing
lookupBy eq x ((k,v):env) | runCommutative eq k x = Just v
lookupBy eq x ((k,v):env) | otherwise             = lookupBy eq x env

-- |
-- >>> lookupBy' (==) 10 env
-- Just 'c'
-- >>> lookupBy' (>) 10 env
-- Just 'd'
lookupBy' :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy' eq x [] = Nothing
lookupBy' eq x ((k,v):env) | k `eq` x  = Just v
lookupBy' eq x ((k,v):env) | otherwise = lookupBy' eq x env

-- |
-- >>> lookupBy'' (==) 10 env
-- Just 'c'
-- >>> lookupBy'' (>) 10 env
-- Just 'a'
lookupBy'' :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy'' eq x = fmap snd . listToMaybe . dropWhile not_x where
  not_x = not . eq x . fst


main = putStrLn "typechecks."

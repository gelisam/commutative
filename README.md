the commutativity monad
=======================

Using the type system to guarantee commutativity.


usage
-----

Make the type of your higher-order functions more precise by declaring that you expect a function parameter to be commutative.

        lookupBy :: Commutative a Bool -> a -> [(a, b)] -> Maybe b
        lookupBy eq x [] = Nothing
        lookupBy eq x ((k,v):env) | runCommutative eq k x = Just v
        lookupBy eq x ((k,v):env) | otherwise             = lookupBy eq x env

The commutativity of `runCommutative` _c_ is guaranteed, because _c_ represents a computation which cannot observe the order of its two arguments. In other words, `Commutative` is a monadic combinator library which preserves commutativity. The trick is that `Commutative`'s primitive operations observe both arguments simultaneously, in a way which does not reveal which result was observed from which argument.

        eq_int :: Commutative Int Bool
        eq_int = do
          zz <- observe_both (== 0)
          case zz of
            TT -> return True        -- (0 == 0)
            TF -> return False       -- (0 != succ y), and also (succ x != 0)
            FF -> eq_int `on` pred   -- (succ x == succ y) iff (x == y)
        
        >>> let env = [(1,'a'), (5,'b'), (10,'c'), (15,'d'), (20, 'e')]
        >>> lookupBy eq_int 10 env
        Just 'c'

The `TT`, `TF` and `FF` above represent each possible unordered pair of booleans. Since the pairs are unordered, the pairs `(True, False)` and `(False, True)` are both represented by `TF`.


motivation
----------

Without `Commutative`, the type of your higher-order function might be too loose. Your function will accept non-commutative arguments; that in itself isn't too bad, and your function might even produce a useful result...

        lookupBy' :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
        lookupBy' eq x [] = Nothing
        lookupBy' eq x ((k,v):env) | k `eq` x  = Just v
        lookupBy' eq x ((k,v):env) | otherwise = lookupBy' eq x env
        
        >>> lookupBy' (==) 10 env
        Just 'c'
        >>> lookupBy' (>) 10 env
        Just 'd'

But if you wrote `lookupBy'` assuming its argument was commutative, you would probably accept a pull request suggesting the following reimplementation. The new code has less moving parts, and it has the same behaviour as the previous implementation. Or has it?

        lookupBy' :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
        lookupBy' eq x = fmap snd . listToMaybe . dropWhile not_x where
          not_x = not . eq x . fst
        
        >>> lookupBy' (==) 10 env
        Just 'c'
        >>> lookupBy' (>) 10 env
        Just 'a'

Since the name `eq` implies commutativity, you might not have noticed that the new implementation gives `eq` its arguments in the opposite order. Your change has broken code which relied on your library!

If such a situation ever happens, you could rightfully blame the user for misusing your library function and relying on undocumented behaviour. Or, you could rely on the type system to prevent such misuses in the first place.

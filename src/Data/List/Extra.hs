module Data.List.Extra where


isCons :: [a] -> Bool
isCons = not . null

fromCons :: [a] -> (a, [a])
fromCons (x:xs) = (x, xs)

module Data.Either.Extra where


isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True


fromLeft :: Either a b -> a
fromLeft (Left x) = x

fromRight :: Either a b -> b
fromRight (Right y) = y

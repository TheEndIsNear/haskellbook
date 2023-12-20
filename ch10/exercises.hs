myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||). f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (== x)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:) ) []

myMap :: (a->b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x:y else y) []

squish :: Foldable t => t [a] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x acc -> if f x acc == GT then x else acc) (last xs) xs

myMaximumBy' f (x:_xs) = foldl (\x acc -> if f x acc == GT then x else acc) (x)


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x acc -> if f x acc == LT then x else acc) (last xs) xs

myMinimumBy' f (x:_xs) = foldl (\x acc -> if f x acc == LT then x else acc) (x)

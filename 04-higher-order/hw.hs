-- Exercise 1: Wholemeal Programming
-- Write each function in a more pipeline way
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x -2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then (x `div` 2) else (3 * x + 1))

-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = Leaf

height :: Tree a -> Integer
height Leaf               = 0
height (Node Leaf _ Leaf) = 0
height (Node l _ r)       = 1 + max (height l) (height r)

height' :: Tree a -> Integer
height' Leaf         = 0
height' (Node l _ r) = 1 + max (height' l) (height' r)

isBalance :: Tree a -> Bool
isBalance Leaf         = True
isBalance (Node l _ r) = diff <= 1 && isBalance l && isBalance r
    where diff = abs (height' l - height' r)

right :: Tree a -> Tree a
right Leaf         = Leaf
right (Node _ _ r) = r

left :: Tree a -> Tree a
left Leaf         = Leaf
left (Node l _ _) = l

-- Exercise 3: More folds!
xor :: [Bool] -> Bool
xor = odd . length . filter (==True)
xor' :: [Bool] -> Bool
xor' = odd .foldr (\x acc -> acc + 1) 0 . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

-- (Optional) implement foldl using foldr
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr f base xs
--
-- Exercise 04: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
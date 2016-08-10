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

--------------------------------------------------------------------------------------
-- ***********************************************************************************
--------------------------------------------------------------------------------------
-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: (Eq a) => [a] -> Tree a
foldTree = foldr insert Leaf
    where insert :: a -> Tree a -> Tree a
          insert n Leaf = Node 0 Leaf n Leaf
          insert n (Node _ left val right)
              | height left <= height right =
                let new_left = insert n left
                in Node (height new_left + 1) (insert n left) val right
              | otherwise =
                let new_right = insert n right
                in Node (height new_right + 1) left val (insert n right)

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

isBalance :: Tree a -> Bool
isBalance Leaf         = True
isBalance (Node _ l _ r) = diff <= 1 && isBalance l && isBalance r
    where diff = abs (height l - height r)

right :: Tree a -> Tree a
right Leaf         = Leaf
right (Node _ _ _ r) = r

left :: Tree a -> Tree a
left Leaf         = Leaf
left (Node _ l _ _) = l

--------------------------------------------------------------------------------------
-- ***********************************************************************************
--------------------------------------------------------------------------------------
-- Exercise 3: More folds!
xor :: [Bool] -> Bool
xor = odd . length . filter (==True)
xor' :: [Bool] -> Bool
xor' = odd .foldr (\x acc -> acc + 1) 0 . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

-- (Optional) implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> f y x) base $ reverse xs

--------------------------------------------------------------------------------------
-- ***********************************************************************************
--------------------------------------------------------------------------------------
-- Exercise 04: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ filter (\x -> notElem x sieveNumbers) [1..n]
    where sieveNumbers = map (\(i, j) -> i+j+2*i*j) $ filter (\(i,j) -> i < j) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
import Data.List
-- Exercise 01 Hopscotch
skips :: [a] -> [[a]]
skips [] = []
skips xs = skips' 1 (length xs) xs
   where skips' i e xs
            | i > e     = []
            | otherwise = every i xs : skips' (i + 1) e xs

every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
               (y:ys) -> y : every n ys
               [] -> []

-- Exercies 02 Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | x < y && z < y = y : localMaxima (z:zs)
    | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

-- Exercise 03 Histogram
histogram :: [Integer] -> String
histogram xs = concat $ intersperse "\n" $ histograms' xs


histograms' :: [Integer] -> [String]
histograms' [] = []
histograms' xs = upperPart ++ [divider] ++ [numbers ++ "\n"]
    where divider   = concat $ replicate 10 "="
          numbers   = concat $ map show [0..9]
          upperPart = hist $ transpose $ group $ sort xs

hist :: [[Integer]] -> [String]
hist [] = []
hist (x:xs) = hist xs ++ [printLine x]

printLine :: [Integer] -> String
printLine [] = ""
printLine xs = print' 0 xs
    where print' :: Integer -> [Integer] -> String
          print' _ [] = []
          print' n (x:xs)
              | n /= x    = " " ++ print' (n+1) (x:xs)
              | otherwise = "*" ++ print' (n+1) xs 
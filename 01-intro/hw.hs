-- Exercise 01
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- Exercise 02
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = reverse $ doubleEveryOther' $ reverse xs
    where doubleEveryOther' xs = case xs of
                                   x:y:zs -> x : y*2 : doubleEveryOther' zs
                                   [x]    -> [x]
                                   _      -> []
                                   
-- Exercise 03
sumDigits :: [Integer] -> Integer
sumDigits xs = case xs of
                 x:xs -> sum (toDigits x) +  sumDigits xs
                 []   -> 0
                 
-- Exercise 04
validate :: Integer -> Bool
validate x = sum `mod` 10 == 0
    where sum = sumDigits $ doubleEveryOther $ toDigits x
    
-- Exercise 05
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src goal tmp
   | n <= 0    = []
   | n == 1    = [(src, goal)]
   | otherwise = hanoi (n-1) src tmp goal
              ++ hanoi 1 src goal tmp
              ++ hanoi (n-1) tmp goal src
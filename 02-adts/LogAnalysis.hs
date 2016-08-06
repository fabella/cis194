{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 01
parseMessage :: String -> LogMessage
parseMessage message = case message of
                          'I':xs -> LogMessage Info (parseNthInt 0 xs) (dropWords 1 xs)
                          'W':xs -> LogMessage Warning (parseNthInt 0 xs) (dropWords 1 xs)
                          'E':xs -> LogMessage (Error (parseNthInt 0 xs)) (parseNthInt 1 xs) (dropWords 2 xs)
                          _      -> Unknown message
                       where parseNthInt n xs = read ((words xs) !! n) :: Int
                             dropWords n xs = unwords $ drop n $ words xs

-- Exercise 02
parse :: String -> [LogMessage]
parse xs = map parseMessage $ lines xs

-- Exercise 03
insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m1@(LogMessage _ ts1 _) (Node lt m2@(LogMessage _ ts2 _) rt)
   | ts1 <= ts2 = Node (insert m1 lt) m2 rt
   | otherwise  = Node lt m2 (insert m1 rt)
insert _ tree = tree -- do not insert Unknown messages

-- Exercise 04
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 05
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt n rt) = inOrder lt ++ [n] ++ inOrder rt

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = map getLogMessage $ inOrder $ build $ filter (filterByError 50) xs

-- filters log messages for error types only of the level provided or above
filterByError :: Int -> LogMessage -> Bool
filterByError level (LogMessage (Error n) _ _) = n >= level
filterByError _  _= False 

-- gets log message
getLogMessage :: LogMessage -> String
getLogMessage (LogMessage (Error _) _ s) = s
getLogMessage (LogMessage _ _ s) = s
getLogMessage (Unknown s) = s
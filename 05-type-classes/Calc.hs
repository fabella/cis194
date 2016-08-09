-- CIS 194 Homework 5
module Calc where

import ExprT
import Parser (parseExp)

-- Exercise 01
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Exercise 02
evalStr :: String -> Maybe Integer
evalStr xs = maybe Nothing (\x -> Just $ eval x) (parseExp Lit Add Mul xs)

-- Exercise 03
class Expr a where
   mul :: a -> a -> a
   lit :: Integer -> a
   add :: a -> a -> a

instance Expr ExprT where
   lit x   = Lit x
   add x y = Add x y
   mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

-- Exercise 04
instance Expr Integer where
   lit x   = x
   add x y = x + y
   mul x y = x * y

instance Expr Bool where
  lit x
     | x < 0     = False
     | otherwise = True
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
   lit x                     = MinMax x
   add (MinMax x) (MinMax y) = MinMax (max x y)
   mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
   lit x = Mod7 (x `mod` 7)
   add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
   mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMinMax  = testExp :: Maybe MinMax
testMod7    = testExp :: Maybe Mod7

-- Exercise 05

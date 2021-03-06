{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = foldr (\(x,y) acc -> if x == y then acc + 1 else acc) 0 $ zip a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors codes = [foldr (\y acc -> if y == x then acc + 1 else acc) 0 codes | x <- colors] 

getMax :: Int -> Int -> Int
getMax a b = if a >= b then b else a

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches secret guess = foldr (\(x,y) acc -> if x > 0 then acc + (getMax x y) else acc) 0 $ zip countA countB
  where countA = countColors secret
        countB = countColors guess

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove a b = Move b exact ((matches a b) - exact)
  where exact = exactMatches a b

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c1 e1 ne1) c2 = exactMatches c1 c2 == e1 && nonExactMatches c1 c2 == ne1 
  where nonExactMatches :: Code -> Code -> Int
        nonExactMatches a b = (matches a b) - (exactMatches a b)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m l = filter (isConsistent m) l

-- Exercise 6 -----------------------------------------

-- Calculate the cartesian product of two lists
prod :: [[a]] -> [[a]] -> [[a]]
prod as bs = [a ++ b | a <- as, b <- bs]

-- Calculate n-amount of permutations of colors
allCodes :: Int -> [Code]
allCodes n = foldr1 prod $ replicate n $ group colors

-- Exercise 7 ----------------------------------------

takeUntilPerfect :: Int -> [Move] -> [Move]
takeUntilPerfect _ [] = []
takeUntilPerfect i (x@(Move _ y _):xs) = x : if i /= y then takeUntilPerfect i xs else []

solve :: Code -> [Move]
solve secret = takeUntilPerfect sLength $ scanr (\attempt _ -> getMove secret attempt) first (tail attempts)
  where attempts = allCodes sLength
        first    = getMove secret (head attempts)
        sLength  = length secret

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List (sort, intersect)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

factors :: (Integral a, Enum a, Num a) => a -> [a]
factors n = [i | i <- [1..n], (mod n i) == 0]

factorized :: (Integral a, Ord a, Num a) => [a] -> [a]
factorized a = map (`div` highestFactor) a
  where highestFactor = last $ sort $ foldl (\fs n -> intersect fs (factors n)) (factors $ head a) (tail a)

instance (Num a, Eq a, Ord a, Integral a) => Eq (Poly a) where
  (==) (P a) (P b) = factorized a == factorized b
 
-- Exercise 3 -----------------------------------------

displayC :: (Num a, Ord a, Eq a, Show a) => a -> a -> String
displayC c d
  | c == 0 && d >= 1 = ""
  | c == 1 && d > 0  = ""
  | c == -1 && d > 0 = "-"
  | otherwise        = show c

display :: (Num a, Eq a, Ord a, Show a) => (a, a) -> String
display (c,d) = (displayC c d) ++ case d of 0 -> ""
                                            1 -> "x"
                                            n -> "x^" ++ show n

instance (Num a, Eq a, Show a, Enum a, Ord a) => Show (Poly a) where
    show (P a) = foldr (\(c,b) st -> st ++ if c /= 0 then (prefix st) ++ display (c,b) else "") "" (zip a [0..])
      where prefix st = if st /= "" then " + " else ""

-- Exercise 4 -----------------------------------------

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (z:xs) (y:ys) = (z,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

plus :: (Num a) => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P [m + n | (m,n) <- zipWithPadding 0 0 a b]

-- Exercise 5 -----------------------------------------

pad :: Int -> a -> [a] -> [a]
pad 0 _ as = as
pad n a as = a:(pad (n-1) a as)

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = foldr (+) (P[0]) padded
  where list   = scanl (\_ val -> map (*val) b) [0] a
        padded = [P (pad n 0 m) | (m,n) <- zip (tail list) [0..]]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = times (P[-1])
    fromInteger = \y -> P [(fromInteger y)]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: (Enum a, Floating a, Num a) => Poly a -> a -> a
applyP (P list) val = foldr (\(v,i) prev -> prev + (v * (val ** i)) ) 0 $ zip list [0..]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv a b = foldl (\acc _ -> deriv acc) b [1..a]

-- Exercise 9 -----------------------------------------

instance (Enum a, Num a) => Differentiable (Poly a) where
    deriv (P a) = P $ tail $ map (\(val,pos) -> val * pos) $ zip a [0..]


{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
--import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib i  
  | i <= 2    = 1
  | otherwise = fib (i-1) + fib (i-2)

fibs1 :: [Integer]
fibs1 = map fib [1..] 

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2) 

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a str) = a : streamToList str  

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a str) = Cons (f a) (fmap f str)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons (f a) (sIterate f (f a))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a aStr) bStr = Cons a (sInterleave bStr aStr)

sTake :: Int -> Stream a -> [a]
sTake n s = take n $ streamToList s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0 

ruler :: Stream Integer
ruler = r 0
  where r n = sInterleave (sRepeat n) $ r (n+1)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand s = sIterate (\x -> ((1103515245 * x) + 12345) `mod` 2147483648) s

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 271 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 197 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax []     = Nothing
minMax (x:xs) = Just $ foldl' compute (x,x) xs
  where compute (cMin, cMax) next
          | next < cMin = (next, cMax)
          | next > cMax = (cMin, next)
          | otherwise   = (cMin, cMax)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Vector = Vector Integer Integer
data Matrix = Matrix Vector Vector

instance Num Matrix where
  (*) (Matrix (Vector a1 a2) (Vector a3 a4)) (Matrix (Vector b1 b2) (Vector b3 b4)) 
      = Matrix (Vector (a1*b1 + a2*b3) (a1*b2 + a2*b4)) (Vector (a3*b1 + a4*b3) (a3*b2 + a4*b4))

instance Show Matrix where
  show (Matrix (Vector a1 a2) (Vector a3 a4)) = "[[" ++ show a1 ++ "," ++ show a2 ++ "], [" ++ show a3 ++ "," ++ show a4 ++ "]]"

toFloat :: Integer -> Float
toFloat = fromIntegral

m0 :: Matrix
m0 = Matrix (Vector 1 1) (Vector 1 0)

ex :: Matrix -> Int -> Matrix
ex _ 0 = m0
ex x 1 = x
ex x n
  | n `mod` 2 == 0 = (ex x (n `div` 2)) * (ex x (n `div` 2))
  | otherwise      = x * (ex x (n-1))

fastFib :: Int -> Integer
fastFib n = extract $ ex m0 n
  where extract (Matrix (Vector _ x) _) = x

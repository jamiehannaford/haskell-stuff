{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs) 
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)

instance Functor Parser where
  fmap f (Parser a) = Parser (fmap (first f) . a)

instance Applicative Parser where
  pure a  = Parser (\s -> Just (a, s))
  (Parser fa) <*> pb = Parser $ \s -> 
    case fa s of
      Nothing     -> Nothing
      Just (f,xs) -> runParser (f <$> pb) xs

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

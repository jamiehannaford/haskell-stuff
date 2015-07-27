{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)
import Data.List (intersect, sortBy, (\\))

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret orig enc = do
  origCont <- BS.readFile orig
  encCont <- BS.readFile enc
  return $ BS.pack $ filter (/= 0) $ zipWith xor (BS.unpack origCont) (BS.unpack encCont)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  encrypted <- BS.readFile (path ++ ".enc")
  let unencrypted = BS.pack $ zipWith xor (BS.unpack encrypted) (BS.unpack (BS.cycle key))
  BS.writeFile path unencrypted

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  content <- BS.readFile path
  return $ decode content

-- Exercise 4 -----------------------------------------

combine :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
combine (Just a) (Just b) = if length filtered == 0 then Nothing else Just filtered
  where filtered = filter (\tr -> elem (tid tr) a) b
combine _ _ = Nothing

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vp tp = do
  vs <- parseFile vp :: IO (Maybe [TId])
  ts <- parseFile tp :: IO (Maybe [Transaction])
  return $ combine vs ts 

-- Exercise 5 -----------------------------------------

applyFn :: String -> (Integer -> Integer) -> Map String Integer -> Map String Integer
applyFn k fn mp = if Map.notMember k mp then applyFn k fn (Map.insert k 0 mp) else Map.adjust fn k mp

add k n mp = applyFn k (\x -> x + n) mp
deduct k n mp = applyFn k (\x -> x - n) mp

parseTrns :: [Transaction] -> Map String Integer -> Map String Integer
parseTrns ts inMp = foldr (\tr mp -> deduct (from tr) (amount tr) $ add (to tr) (amount tr) mp) inMp ts

getFlow :: [Transaction] -> Map String Integer
getFlow ts = parseTrns ts Map.empty 

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal mp = fst $ Map.foldrWithKey (\p c1 acc@(_, c2) -> if c1 > c2 then (p, c1) else acc) ("", 0) mp 

-- Exercise 7 -----------------------------------------

adjust :: (String, Integer) -> (String, Integer) -> String -> Transaction
adjust (p1N, p1A) (p2N, p2A) id = Transaction{from = p1N, to = p2N, amount = abs amount, tid = id}
  where amount = abs $ if (abs p1A) > (abs p2A) then p2A else p1A

trim :: Map String Integer -> Map String Integer
trim mp = Map.filter ((/=) 0) mp

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs input tids
  | length payers == 0 || length payees == 0 = []
  | otherwise                                = trans ++ undoTs (trim (parseTrns trans input)) (tids \\ [tid tr | tr <- trans])
  where trans  = zipWith3 adjust payers payees tids
        payers = sortBy (\(_,s1) (_,s2) -> s2 `compare` s1) $ Map.assocs $ Map.filter (\x -> x > 0) input
        payees = sortBy (\(_,s1) (_,s2) -> s1 `compare` s2) $ Map.assocs $ Map.filter (\x -> x <= 0) input

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON p d = BS.writeFile p $ encode d

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim


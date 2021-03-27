{-# LANGUAGE BangPatterns #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List (foldl')
-- import Debug.Trace
import Text.Printf

import Data.Maybe (fromJust)
import System.IO
import Text.Read

getNum :: IO Int
getNum = fst . fromJust . B.readInt <$> B.getLine

getNums :: IO [Int]
getNums = map ((fst . fromJust) . B.readInt) . B.split ' ' <$> B.getLine

showNums :: [Int] -> IO ()
showNums xs = putStrLn $ unwords $ map show xs

main :: IO ()
main = do
  t <- getNum
  forM_ [1 .. t] handleCase

handleCase :: Int -> IO ()
handleCase ti = do
  [n, c] <- getNums
  putStrLn $ printf "Case #%d: %s" ti (solve n c)

solve :: Int -> Int -> String
solve n c
  | c < (n - 1) || (c >= n * (n + 1) `div` 2) = "IMPOSSIBLE"
  | otherwise = construct [1..n] n c (n - 2)

construct :: [Int] -> Int -> Int -> Int -> String
construct as _ c (-1)
  | c > 0 = "IMPOSSIBLE"
  | otherwise = unwords $ map show as

construct as n c i  = construct as' n (c - stepCost) (i - 1)
  where
    stepCost = min (n - i) (c - i)
    j = i + stepCost - 1
    (p1, p1') = splitAt i as
    (p2, p3) = splitAt stepCost p1'
    as' = p1 ++ reverse p2 ++ p3

{-# LANGUAGE BangPatterns #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List (foldl', sort)
-- import Debug.Trace
import Text.Printf
import Data.Maybe (fromJust)
import System.IO
import Text.Read

getNum :: IO Int
getNum = fst . fromJust . B.readInt <$> B.getLine

getNums :: IO [Int]
getNums = map ((fst . fromJust) . B.readInt) . B.split ' ' <$> B.getLine

main :: IO ()
main = do
  t <- getNum
  forM_ [1 .. t] handleCase

handleCase :: Int -> IO ()
handleCase ti = do
  n <- getNum
  ls <- getNums
  putStrLn $ printf "Case #%d: %d" ti (solve ls)

solve :: [Int] -> Int
solve [l] = 0
solve ls = idx + solve (tail ls')
  where
    idx = snd $ minimum $ zip ls [1..]
    (s1, s2) = splitAt idx ls
    ls' = reverse s1 ++ s2

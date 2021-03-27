{-# LANGUAGE BangPatterns #-}

import Control.Arrow
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List (foldl', group)
-- import Debug.Trace
import Text.Printf
import Data.Maybe (fromJust)
import System.IO
import Text.Read

getNum :: IO Int
getNum = fst . fromJust . B.readInt <$> B.getLine

readNums :: [B.ByteString] -> [Int]
readNums = map $ (fst . fromJust) . B.readInt

main :: IO ()
main = do
  t <- getNum
  forM_ [1 .. t] handleCase

handleCase :: Int -> IO ()
handleCase ti = do
  [x', y', s'] <- B.split ' ' <$> B.getLine
  let [x, y] = readNums [x', y']
  let s = B.unpack s'
  putStrLn $ printf "Case #%d: %d" ti (solve x y s)

data Token = End | C | J | Q { len :: Int } deriving (Eq, Show)

getToken :: String -> Token
getToken ('C':_) = C
getToken ('J':_) = J
getToken i = Q $ length i

asymCost :: Int -> Int -> Int -> Bool -> Int
asymCost x y i moreCJ = minimum [0, cjs * x + jcs * y, singleCost]
  where
    cjs = if moreCJ then i - (i `div` 2) else i `div` 2
    jcs = if moreCJ then i `div` 2 else i - (i `div` 2)
    singleCost = if moreCJ then x else y

countQ :: Int -> Int -> (Token, Token, Token) -> Int
countQ x y (End, Q i, C) = asymCost x y i False
countQ x y (End, Q i, J) = asymCost x y i True
countQ x y (J, Q i, End) = asymCost x y i False
countQ x y (C, Q i, End) = asymCost x y i True
countQ x y (End, Q i, End) = min (asymCost x y (i - 1) True) (asymCost x y (i - 1) False)
countQ x y (t1, Q i, t2) = min 0 $ flips * x + flips * y
  where
    flips = (if t1 == t2 then i + 1 else i) `div` 2
countQ _ _ _ = 0

countCJ :: Int -> Int -> (Token, Token, Token) -> Int
countCJ x y (C, J, _) = x
countCJ x y (J, C, _) = y
countCJ _ _ _ = 0

zipWithEnds xs = zip3 (End:xs) xs (tail xs ++ [End])

solve :: Int -> Int -> String -> Int
solve x y s = baseCost + addlCost
  where
    sizes = map getToken $ group s
    baseCost = sum $ map (countCJ x y) $ zipWithEnds $ filter (uncurry (||) . ((== C) &&& (== J))) sizes
    addlCost = sum $ map (countQ x y) $ zipWithEnds sizes

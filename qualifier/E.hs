{-# LANGUAGE BangPatterns #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List (foldl', sort)
import Data.Map ((!))
import qualified Data.Map.Strict as M

import Data.Maybe (fromJust)
import System.IO
import Text.Printf
import Text.Read

getNum :: IO Int
getNum = fst . fromJust . B.readInt <$> B.getLine

n = 100 :: Int
m = 10000 :: Int
pn = 1 / fromIntegral n :: Double
pm = 1 / fromIntegral m :: Double

main :: IO ()
main = do
  t <- getNum
  p <- getNum
  forM_ [1 .. t] handleCase

handleCase :: Int -> IO ()
handleCase ti = do
  answers <- replicateM n B.getLine
  putStrLn $ printf "Case #%d: %d" ti (solve answers)

type Answers = [B.ByteString]
type QMap = M.Map Int Double

e = exp 1

evToQ :: Double -> Double
evToQ x = log $ (e - exp x) / (exp x - 1)

evToS :: Double -> Double
evToS x = log $ (-e) * (exp x - 1) / (exp x - e)

getQs :: Answers -> QMap
getQs as = M.map evToQ evs
  where
    foldStepInternal :: (QMap, Int) -> Char -> (QMap, Int)
    foldStepInternal (!m, i) '1' = (M.adjust (+ pn) i m, i + 1)
    foldStepInternal (!m, i) '0' = (m, i + 1)

    foldStep :: QMap -> B.ByteString -> QMap
    foldStep !m !bs = fst $ B.foldl' foldStepInternal (m, 1) bs

    initM = M.fromList $ zip [1..m] $ repeat 0
    evs = foldl' foldStep initM as

cToI :: Char -> Int
cToI '0' = 0
cToI '1' = 1

sigmoid x = 1 / (1 + exp(-x))

logPCheaterForQ :: Double -> Double -> Int -> Double
logPCheaterForQ s q 1 = log $ 0.5 * (1 + (sigmoid $ s - q))
logPCheaterForQ s q 0 = log $ 0.5 * (1 - (sigmoid $ s - q))

logPNonCheaterForQ :: Double -> Double -> Int -> Double
logPNonCheaterForQ s q 1 = log $ sigmoid $ s - q
logPNonCheaterForQ s q 0 = log $ 1 - sigmoid (s - q)

cheatLikelihood :: B.ByteString -> [Double] -> Double
cheatLikelihood bs qs =
  logPCheater - logPNonCheater
  where
    bs' = map cToI $ B.unpack bs
    ev = fromIntegral (sum bs') / fromIntegral m
    s = evToS ev
    sCheater = evToS $ max 0 $ 2 * ev - 1

    zipped = zip qs bs'

    logPCheater = (log 0.5) + (sum $ map (\(q, i) -> logPCheaterForQ sCheater q i) zipped)
    logPNonCheater = (log 0.5) + (sum $ map (\(q, i) -> logPNonCheaterForQ s q i) zipped)

solve :: Answers -> Int
solve as = snd $ head sorted
  where
    qMap = getQs as
    zipped = zip [1..] as
    likelihoods = map (\(i, bs) -> (cheatLikelihood bs $ M.elems qMap, i)) zipped
    sorted = reverse $ sort likelihoods

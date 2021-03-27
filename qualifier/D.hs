{-# LANGUAGE MultiWayIf #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import System.IO
import Text.Read

getNum :: IO Int
getNum = fst . fromJust . B.readInt <$> B.getLine

getNums :: IO [Int]
getNums = map ((fst . fromJust) . B.readInt) . B.split ' ' <$> B.getLine

showNums :: [Int] -> IO ()
showNums xs = putStrLn $ unwords $ map show xs

data Node
  = Unsorted {unsortedNums :: [Int], leftBound :: Maybe Int, rightBound :: Maybe Int}
  | Sorted {sortedNums :: [Int]}
  deriving (Show)

getMedian :: Int -> Int -> Int -> IO Int
getMedian a b c = showNums [a, b, c] >> hFlush stdout >> getNum

sortTwo :: Int -> Int -> Maybe Int -> Maybe Int -> IO [Node]
-- only happens at the root. This is where the two possible correct answers come from.
sortTwo a b Nothing Nothing = return [Sorted [a, b]]
sortTwo a b (Just l) _ = do
  m <- getMedian l a b
  return $ if m == a then [Sorted [a, b]] else [Sorted [b, a]]
sortTwo a b _ (Just r) = do
  m <- getMedian r a b
  return $ if m == b then [Sorted [a, b]] else [Sorted [b, a]]

data PivotOut = Lft | Rght | Mid deriving (Eq, Ord, Show)

pivot l r i = do
  m <- getMedian l r i
  return $
    if
        | m == l -> Lft
        | m == r -> Rght
        | otherwise -> Mid

divideLeaf :: Node -> IO [Node]
divideLeaf (Unsorted ns l r) =
  if
      | length ns <= 1 -> return [Sorted ns]
      | length ns == 2 -> sortTwo (ns !! 0) (ns !! 1) l r
      | otherwise -> do
        let p1 : p2 : ns' = ns
        [Sorted [pl, pr]] <- sortTwo p1 p2 l r
        buckets <- mapM (pivot pl pr) ns'
        let zipped = zip buckets ns'
        let leftNs = map snd $ filter ((== Lft) . fst) zipped
        let rightNs = map snd $ filter ((== Rght) . fst) zipped
        let midNs = map snd $ filter ((== Mid) . fst) zipped
        return
          [ Unsorted leftNs l (Just pl),
            Sorted [pl],
            Unsorted midNs (Just pl) (Just pr),
            Sorted [pr],
            Unsorted rightNs (Just pr) r
          ]
divideLeaf x = return [x]

divideRecursive :: Node -> IO [Node]
divideRecursive node = case node of
  Sorted {} -> return [node]
  Unsorted {} -> do
    results <- divideLeaf node
    concat <$> mapM divideRecursive results

getNodeNums :: Node -> [Int]
getNodeNums (Unsorted ns l r) = ns
getNodeNums (Sorted ns) = ns

main :: IO ()
main = do
  [t, n, q] <- getNums
  replicateM_ t (handleCase n)

handleCase :: Int -> IO ()
handleCase n = do
  d <- divideRecursive $ Unsorted [1 .. n] Nothing Nothing
  showNums $ concatMap getNodeNums d
  hFlush stdout
  void getNum

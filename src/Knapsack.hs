{-# LANGUAGE RankNTypes #-}
module Knapsack where
import Data.Map (Map, empty, insert, (!))
import qualified Data.Map as Map

data Item = I { weight :: Int, value :: Int }

bestValue :: [Item] -> Int -> Int
bestValue i c = fst $ bestValue' empty i c

bestValue' :: Map Int Int -> [Item] -> Int -> (Int, Map Int Int)
bestValue' memo items capacity =

  let findMax :: (Int, Map Int Int) -> Item -> (Int, Map Int Int)
      findMax (currentBest, currentMemo) item
        | weight item > capacity = (currentBest, currentMemo)
        | otherwise =
          (max currentBest $ bestLessCapacity + value item, postRecMemo)
          where (bestLessCapacity, postRecMemo) =
                  bestValue' currentMemo items $ capacity - weight item

  in case Map.lookup capacity memo of
      Just x  -> (x, memo)
      Nothing -> foldl findMax (0, memo) items

bestValuE :: [Item] -> Int -> Int
bestValuE items cap = snd $ Map.findMax $ memoize empty 0

  where memoize :: Map Int Int -> Int -> Map Int Int
        memoize memo capacity
         | capacity > cap = memo
         | otherwise = memoize memo' $ capacity + 1
           where memo' = insert capacity maxValForCapacity memo
                 maxValForCapacity = foldl getMax 0 items
                 getMax currentVal item
                  | weight item > capacity = currentVal
                  | otherwise =
                    max currentVal $ value item + memo!(capacity - weight item)

tuplesToItems :: [(Int, Int)] -> [Item]
tuplesToItems tuples = map (\(w, v) -> I { weight = w, value = v }) tuples

items1 :: [Item]
items1 = tuplesToItems [(1,1)]

items2 :: [Item]
items2 = tuplesToItems [(3,5),(2,3),(2,3)]
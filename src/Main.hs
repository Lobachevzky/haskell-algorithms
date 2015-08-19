-- | Main entry point to the application.
module Main where
import TestTrees

import Knapsack


-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    --print tree
    --print (isolatedSet linkedListOfTree)
    --print $ "test: " ++ show test
    --print $ getLinkedList (treeToAdjlist $ dfs 1 tree) 1
    --print $ bfs 1 tree
      --where tree = tree1
    print $ and $ map (\num -> bestValuE items num == bestValue items num) [1..10]
     where items = items1


linkedListOfTree :: Vertex Int
linkedListOfTree = getLinkedList tree2 1

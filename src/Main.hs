-- | Main entry point to the application.
module Main where

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"


undominated' :: (Ord a) => [(a,a)] -> [(a,a)]
undominated' [] = []
undominated' [p] = [p]
undominated' ps = merge (undominated' left) (undominated' right) 
    where (left, right) = split ps
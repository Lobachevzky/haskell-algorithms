-- | Main entry point to the application.
module Main where
import Data.List
-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"

compare_x :: (Ord a) => (a,a) -> (a,a) -> Ordering
compare_x (x1, _) (x2, _)
    | x1 > x2   = GT
    | x1 < x2   = LT
    | otherwise = EQ

median ps = ps !! (length ps) / 2

undominated ps = undominated' sorted where sorted = sortBy compare_x ps

undominated'     :: (Ord a) => [(a,a)] -> [(a,a)]
undominated' []  = []
undominated' [p] = [p]
undominated' ps  = merge (undominated' left) (undominated' right) 
    where (left, right) = partition (< median ps) ps
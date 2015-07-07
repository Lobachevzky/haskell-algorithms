-- | Main entry point to the application.
module Main where
import Data.List
-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"

data Dim = X | Y

compareBy :: (Ord a) => Dim -> (a,a) -> (a,a) -> Ordering
compareBy X (x1, _) (x2, _) = compare x1 x2
compareBy Y (_, y1) (_, y2) = compare y1 y2

merge :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
merge left right = [p | p <- left, p `above` top right] ++ right
    where
        above p1 p2 = compareBy Y = GT
        top = maximumBy (ccompareBy Y)

median :: [a] -> a
median ps = ps !! (length ps `quot` 2)

undominated    :: (Ord a) => [(a,a)] -> [(a,a)]
undominated ps = undominated' (sortBy (compareBy X) ps)

undominated'     :: (Ord a) => [(a,a)] -> [(a,a)]
undominated' [p] = [p]
undominated' ps  = merge (undominated' left) (undominated' right) 
    where (left, right) = partition (< median ps) ps
module FindUndominatedPoints where
import           Data.List
import           Data.List.Split

data Dim = X | Y

compareBy :: (Ord a) => Dim -> (a,a) -> (a,a) -> Ordering
compareBy X (x1, _) (x2, _) = compare x1 x2
compareBy Y (_, y1) (_, y2) = compare y1 y2

merge :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
merge left right = [p | p <- left, p `above` top right] ++ right
    where
        above (_,y1) (_,y2) = y1 > y2
        top = maximumBy (compareBy Y)

undominated    :: (Ord a) => [(a,a)] -> [(a,a)]
undominated ps = 
    let undominated' [q] = [q]
        undominated' qs  = merge (undominated' left) (undominated' right)
            where median rs     = length rs `div` 2
                  [left, right] = splitPlaces [median qs] qs
    in undominated' (sortBy (compareBy X) ps)

{-# LANGUAGE GADTs #-}
module AdjacencyList where
import Data.List (delete)

data AdjList v = Eq v => AL [(v, [v])]

keys :: AdjList v -> [v]
keys (AL list) = map fst list

(!) :: Eq v => AdjList v -> v -> [v]
(!) (AL []) _ = [] 
(!) (AL ((u, us):rest)) v
    | u == v = us
    | otherwise = AL rest !v

(\\) :: Eq v => AdjList v -> v -> AdjList v
(\\) (AL list) v = foldr vRemove (AL []) list
                    where vRemove (u, adj) (AL accum)
                            | v == u    = AL accum
                            | otherwise = AL ((u, adj):accum)

(+++) :: AdjList v -> AdjList v -> AdjList v
(+++) (AL list1) (AL list2) = AL (list1 ++ list2)

getVert :: AdjList v -> Maybe v
getVert (AL []) = Nothing
getVert (AL ((v, _):_)) = Just v

pop :: v -> AdjList v -> (Maybe (v, [v]), AdjList v)
pop v (AL list) = foldr f (Nothing, AL []) list
                    where f vert (Just match, AL accum) = (Just match, AL (vert:accum))
                          f vert (Nothing, AL accum)
                            | u == v    = (Just vert, AL accum)
                            | otherwise = (Nothing, AL (vert:accum))
                                where (u, _) = vert

removeVert :: Eq v => v -> AdjList v -> v -> AdjList v
removeVert v adjList u = 
    case pop u adjList of (Nothing, _) -> error "v not in adjList"
                          (Just (w, adj), rest) -> rest +++ AL [(w, delete v adj)]

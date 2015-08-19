{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module DfsBfs where
import Data.Map (Map, (!), adjust, union, singleton, empty)
import qualified Data.Map as Map

import TestTrees

data Tree v = Node v [Tree v] | Leaf

data Attrs v = Attrs
  { adj      :: [v]
  , color    :: Color
  , distance :: Int }

data Color = White | Gray | Black deriving (Eq)

--All-Purpose Functions
initialize :: forall k v. Map k [v] -> Map k (Attrs v)
initialize = Map.map (\adj' -> Attrs { adj = adj', color = White, distance = -1 })

--Breadth First Search
bfs :: Ord a => a -> Map a [a] -> Map a Int
bfs s adjlist = Map.map distance $ snd $ bfsTraverse [s] 0 $ initialize adjlist

bfsTraverse :: Ord a => [a] -> Int -> Map a (Attrs a) -> ([t], Map a (Attrs a))
bfsTraverse [] _ adjlist = ([], adjlist)
bfsTraverse currentLayer dist adjlist = bfsTraverse nextLayer (dist + 1) adjlist'
  {-nextLayer includes children of all White members of currentLayer
    adjlist' sets all distances and colors in currentLayer to dist and Gray. -}
  where (nextLayer, adjlist') = layerAfter currentLayer dist adjlist 

{-if parent is White, adds children to accum, and
  makes color parent = White and distance parent = dist -}
layerAfter :: Ord a => [a] -> Int -> Map a (Attrs v) -> ([v], Map a (Attrs v))
layerAfter currentLayer dist adjlist =
  foldl (bfsVisit dist) ([], adjlist) currentLayer

{-if s is white, returns children and adjlist with gray s and distance s = dist.
  Otherwise, returns empty list and unchanged adjlist -}
bfsVisit :: Ord k => Int -> ([v], Map k (Attrs v)) -> k -> ([v], Map k (Attrs v))
bfsVisit dist (accum, adjlist) parent =
  case adjlist!parent of
    Attrs { adj = children, color = White }
      -> let modColDist n = n { color = Gray, distance = dist }
         in (children ++ accum, adjust modColDist parent adjlist)
    _ -> (accum, adjlist)

test :: [Int]
test = fst $ layerAfter [] 0 $ initialize tree3

--Depth First Search
dfs :: forall t. Ord t => t -> Map t [t] -> Tree t
dfs s adjlist = fst $ dfsVisit s $ initialize adjlist

dfsVisit :: Ord v => v -> Map v (Attrs v) -> (Tree v, Map v (Attrs v))
dfsVisit s adjlist =
  case adjlist!s of
    Attrs { adj = children, color = White }
      -- For each child, inserts subtree into Node s [] and updates adjlist
      -> foldl absorb (Node s [], adjust recolor s adjlist) children
        where recolor node = node { color = Gray }
    _ -> (Leaf, adjlist) --result when s is Gray

absorb :: Ord v => (Tree v, Map v (Attrs v)) -> v -> (Tree v, Map v (Attrs v))
absorb (Leaf, _) _ = error "absorb doesn't take Leaf as an argument"
absorb (Node v vs, adjlist) child = (treeWithChild, adjlist')
  where (childTree, adjlist') = dfsVisit child adjlist
        treeWithChild = case childTree of Leaf -> Node v vs
                                          _    -> Node v (childTree:vs)
                                     --dfsVisit returns Leaf when child is Gray

--stuff
treeToAdjlist :: forall k. Ord k => Tree k -> Map k [k]
treeToAdjlist Leaf = empty
treeToAdjlist (Node v children) =
  foldl union (singleton v (getValues children)) childMaps
    where getValues []              = []
          getValues (Leaf:rest)     = getValues rest
          getValues (Node u _:rest) = u:getValues rest
          childMaps = map treeToAdjlist children
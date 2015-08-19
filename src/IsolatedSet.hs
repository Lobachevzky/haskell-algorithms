{-# LANGUAGE ExistentialQuantification #-}
module IsolatedSet where
import TestTrees

isolatedSet :: Show a => Vertex a -> ([a], Bool)
isolatedSet (Vertex root []) = ([root], True)
isolatedSet (Vertex root children)
    | excludeRoot = (childSets, False)
    | otherwise   = (root:childSets, True)
        where resultForChildren = map isolatedSet children
              childSets         = concatMap fst resultForChildren 
              --union of isolated sets below the root
              excludeRoot       = any snd resultForChildren
              --any of the childSets include the children

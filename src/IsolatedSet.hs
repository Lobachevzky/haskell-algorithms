{-# LANGUAGE ExistentialQuantification #-}
module IsolatedSet where
import           Data.Map (Map, (!), adjust, fromList)
import           Data.List (delete)

data Vertex a = Show a => Vertex a [Vertex a]
instance Show (Vertex a) where
    show (Vertex root children) = 
        show root ++ displayList (map (show . indent) children)
            where indent (Vertex r cs) = Vertex ("-" ++ show r) (map indent cs)
                  displayList list = foldl (\x xs -> (x\\"\"\\") ++ "\n" ++ xs) "" list

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) xs [] = xs
(\\) (x:xs) (y:ys)
    | x==y      = xs \\ (y:ys)
    | otherwise = (x:(xs \\ [y])) \\ ys

isolatedSet :: Show a => Vertex a -> ([a], Bool)
isolatedSet (Vertex root []) = ([root], True)
isolatedSet (Vertex root children)
    | excludeRoot = (childSets, False)
    | otherwise   = (root:childSets, True)
        where resultForChildren = map isolatedSet children
              childSets         = concat $ map fst resultForChildren
              excludeRoot       = any snd resultForChildren


getLinkedList :: Show a => Ord a => Map a [a] -> a -> Vertex a
getLinkedList adjList root = Vertex root (childVertices children adjList')
    where children = adjList!root
          deleteRoot child anAdjList = adjust (delete root) child anAdjList  
          adjList' = foldr deleteRoot adjList children

childVertices :: Show a => Ord a => [a] -> Map a [a] -> [Vertex a]
childVertices children adjList = Prelude.map (getLinkedList adjList) children

tree :: Map Int [Int]
tree = fromList [(1, [2,5]),
                 (2, [1, 3, 4]),
                 (3, [2]),
                 (4, [2]),
                 (5, [1])]

linkedListOfTree :: Vertex Int
linkedListOfTree = getLinkedList tree 1
{-
adjListToRoot :: Ord a => Map a [a] -> a -> Vertex a
adjListToRoot adjList root = processVerts (Vertex root []) (adjList!root) [] adjList

returnToParent :: Ord a => Vertex a -> [Vertex a] -> Map a [a] -> Vertex a
returnToParent root [] _ = root
returnToParent (Vertex root _) (parent:restParents) adjList =
    let Vertex parentName siblings = parent
        adjList' = adjust (delete root) parentName adjList
        parent' = Vertex parentName (root:siblings)
        children' = adjList'!parentName
    in processVerts parent' children' restParents adjList'

processVerts :: Ord a => Vertex a -> [a] -> [Vertex a] -> Map a [a] -> Vertex a
processVerts root children parents adjList =
    case children of []      -> returnToParent root parents adjList
                     child:_ -> processVerts child (adjList!child) (root:parents) adjList'
                         where (Vertex rootName _) = root
                               adjList' = adjust (delete rootName) child adjList
                               -}




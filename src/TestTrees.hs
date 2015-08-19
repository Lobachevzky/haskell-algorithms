{-# LANGUAGE GADTs #-}
module TestTrees where
import           Data.Map (Map, (!), adjust, fromList)
import           Data.List (delete)

data Vertex a = Show a => Vertex a [Vertex a]

stringify :: Vertex a -> Vertex String
stringify (Vertex root children) = Vertex (show root) (map stringify children) 

indent :: Vertex String -> Vertex String
indent (Vertex root children) = Vertex ("-" ++ root) (map indent children)

deepIndent :: Vertex String -> Vertex String
deepIndent (Vertex root children) = Vertex root (map (deepIndent . indent) children)

vertexToString :: Vertex String -> String
vertexToString (Vertex root children) = 
    root ++ "\n" ++ concatMap vertexToString children

instance Show (Vertex a) where
    show vertex = vertexToString $ deepIndent $ stringify vertex

getLinkedList :: Show a => Ord a => Map a [a] -> a -> Vertex a
getLinkedList adjList root = Vertex root (childVertices children adjList')
    where children = adjList!root
          adjList' = foldr (adjust (delete root)) adjList children

childVertices :: Show a => Ord a => [a] -> Map a [a] -> [Vertex a]
childVertices children adjList = Prelude.map (getLinkedList adjList) children

tree1 :: Map Int [Int]
tree1 = fromList [(1, [2, 3, 4, 5]),
                 (2,  [1, 6]),
                 (3,  [1]),
                 (4,  [1]),
                 (5,  [1, 7]),
                 (6,  [2, 8, 9]),
                 (7,  [5, 10]),
                 (8,  [6]),
                 (9,  [6]),
                 (10, [7, 11, 12, 13]),
                 (11, [10]),
                 (12, [10]),
                 (13, [10])]

tree2 :: Map Int [Int]
tree2 = fromList [(1, [1, 2, 5, 4]),
                  (2, [1, 3, 4]),
                  (3, [2]),
                  (4, [2, 1, 5]),
                  (5, [1, 4])]

tree3 :: Map Int [Int]
tree3 = fromList [(1, [2]),
                  (2, [])]
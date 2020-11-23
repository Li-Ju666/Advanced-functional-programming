module Kcolor where

import Test.QuickCheck
import Data.List(nub, sort)


type Vert = Int
type Edge = (Vert, Vert)
type Graph = [(Vert, [Vert])]

newGraph :: Graph
newGraph = []

insertEdge :: Graph -> Edge -> Graph
insertEdge g (v1, v2) = 
    case v1 == v2 of 
        True -> insertVert g v1
        False -> insertDirEdge (insertDirEdge g (v1, v2)) (v2, v1)

insertDirEdge :: Graph -> Edge -> Graph
insertDirEdge g (v1, v2) =
    case g of 
        [] -> [(v1, [v2])]
        (v, adj):vs -> 
            if v == v1 then (v, nub (v2:adj)):vs
                       else (v, adj):(insertDirEdge vs (v1,v2))

insertVert :: Graph -> Vert -> Graph
insertVert g v = 
    case g of 
        [] -> [(v,[])]
        (v1,adj):vs -> 
            if v == v1 then g else (v1,adj):(insertVert vs v)

getAdj :: Graph -> Vert -> [Vert]
getAdj g v = 
    case g of 
        [] -> []
        (v1,adj):vs ->
            if v == v1 then adj else getAdj vs v

getVerts :: Graph -> [Vert]
getVerts g = 
    case g of 
        [] -> []
        (v,_):vs -> v:(getVerts vs)

getEdges :: Graph -> [Edge]
getEdges g = getEdgesAcc g []

getEdgesAcc [] acc = acc
getEdgesAcc ((v,adj):vs) acc = getEdgesAcc vs [(v,x)|x<-adj]++acc

-- Property-based test for graph data structure
-- 1. pairwise adjacent vertex testing

prop_pairwiseAdj l = 
    forAll (randomGraph (length l) l newGraph) $ pairwiseTest

randomGraph :: Int -> [Int] -> Graph -> Gen Graph
randomGraph n l g = do
        v1 <- elements l
        v2 <- elements l
        if n > 2 then randomGraph (n-1) l (insertEdge g (v1,v2))
                 else return g

pairwiseTest g = 
    case getVerts g of
        [] -> True
        (v1:_) -> case getAdj g v1 of
                    [] -> True
                    (v2:_) -> elem v1 (getAdj g v2)

-- Property-based test for graph data structure
-- 2. to check the set of vertices is the subset of 
-- set of all vertices
prop_edgeVert l = 
    forAll (randomGraph (length l) l newGraph) $ edgeVertTest

edgeVertTest [] = True
edgeVertTest g = 
    foldr (\x acc -> if elem x verts then acc else False) True edgeVerts
    where edges = getEdges g
          verts = nub $ getVerts g
          edgeVerts = nub $ foldr (\(v1,v2) acc -> v1:v2:acc) [] edges

-- Kcolor problem: implementation of brute-force algorithm
type Color = (Vert, Char)
kcolor :: Graph -> Int -> Maybe [Color]
kcolor g num = solve g (getColors num)

solve g colors = do
    let resultList = take 1 [x | x<- (getCombs g colors), isValid (getEdges g) x]
    case resultList of
        [] -> Nothing
        x:xs -> Just x

getCombs :: Graph -> [Char] -> [[Color]]
getCombs [] _ = []
getCombs _ [] = []
getCombs g colors = 
    foldl (\acc a -> [x:y|x<-a, y<-acc]) [[]] vertColor
    where 
        vertColor = foldl (\acc c -> [(c,x)|x<-colors]:acc) [] vs
        vs = getVerts g

getColors :: Int -> [Char]
getColors n = take n ['a'..'z']

isValid :: [Edge] -> [Color] -> Bool
isValid [] _ = True
isValid _ [] = False
isValid ((v1,v2):vs) colors = 
    if vertColor v1 colors == vertColor v2 colors
        then False else isValid vs colors
    where
        vertColor v1 ((v2,c):vs) = if v1 == v2 then c else vertColor v1 vs

-- property-based testing for kcolor implementation
-- 1. test to check if the length of returned colors equals
-- the number of all vertices
prop_colorLength l = 
    forAll (randomGraphWithNum (length l) l newGraph) $ colorLenTest 

randomGraphWithNum len l g = do
    graph <- randomGraph (2*len) l g
    num <- elements [0..(div len 2)]
    return (graph, num)

-- colorLenTest _ = True
colorLenTest ([], _) = True
colorLenTest (g, n) = 
    case kcolor g n of
        Just colored -> (length colored) == (length g)
        _ -> True

-- property-based testing for kcolor implementation
-- 2. to check if adjacent vertices are of different color 
prop_adjColor l = 
    forAll (randomGraphWithNum (length l) l newGraph) $ adjTest

adjTest ([],_) = True
adjTest (g, n) =
    case kcolor g n of
        Just colored -> foldl 
            (\acc (v1,v2) -> 
                if acc == False then acc
                else (vertColor v1 colored) /= (vertColor v2 colored))
                True (getEdges g) 
        _ -> True
    where vertColor v1 ((v2,c):vs) = if v1 == v2 then c else vertColor v1 vs

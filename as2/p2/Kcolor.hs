module Kcolor where

import Test.QuickCheck
import Data.List(nub)

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
getEdges g = getEdgesD g []

getEdgesD [] acc = acc
getEdgesD ((v,adj):vs) acc = getEdgesD vs [(v,x)|x<-adj]

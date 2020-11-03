-module(kcolor).
-export([insertEdge/2, insertVert/2, findAdj/2]).

-type vert() :: {integer(), [integer()]}.
-type graph() :: [vert()].

-spec insertEdge(graph(), vert()) -> graph().
insertEdge(G, {V1, V2}) -> insertEdge(insertEdge(G,V1,V2),V2,V1).
insertEdge(G, V1, V2) -> 
    case G of 
        [] -> [{V1,[V2]}];
        [{V,Adj}|Ns] when V =:= V1 -> [{V,lists:usort([V2|Adj])}|Ns];
        [N1|Ns] -> [N1]++insertEdge(Ns,V1,V2)
    end.

-spec insertVert(graph(), integer()) -> graph().
insertVert(G, V1) ->
    case G of
        [] -> [{V1,[]}];
        [{V,_}|_] when V=:=V1 -> G;
        [N1|Ns] -> [N1] ++ insertVert(Ns, V1)
    end.

-spec findAdj(graph(), integer()) -> [integer()].
findAdj(G, V1) -> 
    case G of
        [] -> [];
        [{V,Adj}|_] when V=:=V1 -> Adj;
        [_|Ns] -> findAdj(Ns, V1)
    end.

-module(kcolor).
-export([random_insert/2, getVerts/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type vert() :: {term(), [term()]}.
-type graph() :: [vert()].

%% to insert an edge specified by {V1, V2} to graph G
-spec insertEdge(graph(), vert()) -> graph().
insertEdge(G, {V1, V2}) -> insertEdge(insertEdge(G,V1,V2),V2,V1).
insertEdge(G, V1, V2) -> 
    case G of 
        [] -> [{V1,[V2]}];
        [{V,Adj}|Ns] when V =:= V1 -> [{V,lists:usort([V2|Adj])}|Ns];
        [N1|Ns] -> [N1]++insertEdge(Ns,V1,V2)
    end.

%% to insert a vertex V1 to graph G
-spec insertVert(graph(), term()) -> graph().
insertVert(G, V1) ->
    case G of
        [] -> [{V1,[]}];
        [{V,_}|_] when V=:=V1 -> G;
        [N1|Ns] -> [N1] ++ insertVert(Ns, V1)
    end.

%% to find all adjacent vertices of the vertex V1 in graph G and return
%% them in a list
-spec findAdj(graph(), term()) -> [term()].
findAdj(G, V1) -> 
    case G of
        [] -> [];
        [{V,Adj}|_] when V=:=V1 -> Adj;
        [_|Ns] -> findAdj(Ns, V1)
    end.

-spec getVerts(graph()) -> [term()].
getVerts(G) -> 
    case G of
        [] -> [];
        [{V,_} | Vs] -> [V]++getVerts(Vs)
    end.

%% ====================================================================
%% Property-based testing for graph data structure
%% ====================================================================
% prop_adj_in_pair() -> 
%     ?LET(L, random_graph(integer()), in_pair_test(L)).

random_graph(T) -> 
    ?LET(L, list(T), random_insert([],L)).

random_insert(G, List) -> 
    case List of
        [] -> G;
        [V] -> insertVert(G,V);
        [V1,V2] -> insertEdge(G,{V1,V2});
        [V1,V2|Vs] -> case lists:nth(rand:uniform(2),[true,false]) of
                         true -> random_insert(insertEdge(G, {V1,V2}), Vs);
                         false -> random_insert(insertVert(G,V1), [V2|Vs])
                      end
    end.

% in_pair_test(G) -> 
%     case G of 
%         [] -> true;
%         []

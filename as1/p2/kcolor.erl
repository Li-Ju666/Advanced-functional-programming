-module(kcolor).
-export([random_insert/2, pairwise_test/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type vert() :: {term(), [term()]}.
-type graph() :: [vert()].

%% to insert an edge specified by {V1, V2} to graph G
-spec insertEdge(graph(), vert()) -> graph().
insertEdge(G, {V1, V2}) ->
        case V1 =:= V2 of 
            true -> insertVert(G,V1);
            false -> insertEdge(insertEdge(G,V1,V2),V2,V1)
    end.

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
-spec getAdj(graph(), term()) -> [term()].
getAdj(G, V1) -> 
    case G of
        [] -> [];
        [{V,Adj}|_] when V=:=V1 -> Adj;
        [_|Ns] -> getAdj(Ns, V1)
    end.

%% to get all vertices in the graph G
-spec getVerts(graph()) -> [term()].
getVerts(G) -> 
    case G of
        [] -> [];
        [{V,_} | Vs] -> [V]++getVerts(Vs)
    end.

%% ====================================================================
%% Property-based testing for graph data structure
%% ====================================================================
prop_pairwise_adj() -> 
    ?FORALL(L, random_graph(integer()), pairwise_test(L)).

%% generator to generate random graph with a random list
random_graph(T) -> 
    ?LET(L, list(T), random_insert([],L)).

random_insert(G, List) -> 
    case List of
        [] -> G;
        [V] -> insertVert(G,V);
        [V1,V2|Vs] -> case lists:nth(rand:uniform(2),[true,false]) of
                         true -> random_insert(insertEdge(G, {V1,V2}), Vs);
                         false -> random_insert(insertVert(G,V1), [V2|Vs])
                      end
    end.

%% randomly pick a vertex and check if it is adjcent to a random neighbor
%% of itself
pairwise_test(G) -> 
    case getVerts(G) of
        [] -> true;
        [V1|_] -> case getAdj(G,V1) of 
                      [] -> true;
                      [V2|_] -> lists:member(V1,getAdj(G,V2))
                  end
    end.

%% ======================================================================
%% kcolor algorithm implementation
% kcolor([], _) -> []
% kcolor(G, Num) -> kcolor(G, Num, []). 

% kcolor(G, Num, Colored) -> 
    

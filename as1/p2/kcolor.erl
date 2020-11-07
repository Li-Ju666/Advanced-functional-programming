-module(kcolor).
-export([newGraph/0, insertEdge/2, insertVert/2, kcolor/2]).
% -compile(export_all).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type vert() :: {term(), [term()]}.
-type graph() :: [vert()].

%% to generate an empty graph
newGraph() -> [].

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
        % [{V,_} | Vs] -> [V]++getVerts(Vs)
        [{V, _}|Vs] -> [V|getVerts(Vs)]
    end.

%% to get all edges in the graph G 
%% Notice: a single edge will be returned in both directions
%%         edge 1 to 2 will be return as both {1,2} and {2,1}
-spec getEdges(graph()) -> [{term(), term()}].
getEdges(G) -> getEdges(G, []).
getEdges([], Edges) -> Edges;
getEdges([{V, Es}|Vs], Edges) ->
    getEdges(Vs, Edges++[{V,X}||X<-Es]).


%% ====================================================================
%% Property-based testing for graph data structure - 1
%% ====================================================================
%% to check if adajcent vertices are in pair: adajcent vertices of vertex 1
%% include vertex 2, at the same time, adajcent vertices of vertex 2 must
%% include vertex 1.
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

%% randomly pick a vertex and check if it is adjacent to a random neighbor
%% of itself
pairwise_test(G) -> 
    case getVerts(G) of
        [] -> true;
        [V1|_] -> case getAdj(G,V1) of 
                      [] -> true;
                      [V2|_] -> lists:member(V1,getAdj(G,V2))
                  end
    end.

%% ====================================================================
%% Property-based testing for graph data structure - 2
%% ====================================================================
%% to check if vertices in all edges are a subset of the set of all 
%% vertices

prop_edge_verts() -> 
    ?FORALL(L, random_graph(integer()), edge_vert_test(L)).

edge_vert_test([]) -> true;
edge_vert_test(G) -> 
    Edges = getEdges(G),
    Verts = getVerts(G),
    EVerts = lists:foldr(
              fun({V1,V2},Acc) -> [V1,V2|Acc] end, [], Edges),
    length(lists:usort(Verts) -- lists:usort(EVerts)) >= 0.

%% ======================================================================
%% kcolor algorithm implementation

kcolor([], _) -> [];
kcolor(G, Num) -> 
    Colored = lists:sort(colorGraph(G, [])),
    NumOfColor = length(lists:usort(
                   lists:foldr(fun({_, C}, Acc) -> [C|Acc] end, [], Colored))),
    case NumOfColor =< Num of
        true -> Colored;
        false -> false
    end.

%% Color the graph with least number of colors
colorGraph([], Colored) -> Colored;
colorGraph([{V1,Edges}|Vs], Colored) -> 
    Adjs = getAdj([{V1,Edges}|Vs],V1),
    AdjColors = getAdjColors(Colored, Adjs),
    AllColors = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
    colorGraph(Vs,[{V1,lists:min(AllColors--AdjColors)}|Colored]).

%% get colors of a list of vertices
getAdjColors([], _) -> [];
getAdjColors(_, []) -> [];
getAdjColors(Colored, [V|Vs]) -> 
    getVertColor(Colored, V) ++ getAdjColors(Colored, Vs).

%% get color of a vertex
getVertColor([], _) -> [];
getVertColor([{V1,Color}|Vs], V) ->
    case V1 =:= V of
          true -> [Color];
          false -> getVertColor(Vs, V)
    end.

%% ====================================================================
%% Property-based testing for kcolor implementation 
%% ====================================================================
%% test to check if two vertices connected by each edge are of different
%% colors
prop_adj_color() -> 
    ?FORALL(L, random_graph(integer()), adj_color_test(L)).

adj_color_test([]) -> true;
adj_color_test(G) ->
    Colored = kcolor(G, rand:uniform(26)),
    case Colored of 
       false -> true;
       Colors -> 
            Edges = getEdges(G),
            CheckList = [checkColor(V1,V2,Colors)||{V1,V2}<-Edges],
            lists:foldr(
              fun(X,Acc) -> case X of true -> 1+Acc; _ -> Acc end end, 
              0, CheckList) =:= length(CheckList)
     end.

%% Check two vertices are of different colors
checkColor(V1, V2, Colors) -> 
    getColor(V1, Colors) =/= getColor(V2, Colors).

%% to get color of a vertex
getColor(_, []) -> false;
getColor(V, Colors) -> 
    case Colors of
        [{V1, Color}|_] when V1 =:= V -> Color;
        [_|Cs] -> getColor(V, Cs)
    end.

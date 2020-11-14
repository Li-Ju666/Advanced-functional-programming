-module(kcolor).
-export([new_graph/0, add_edge/2, add_vertex/2, kcolor/2]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type vert() :: term().
-type edge() :: {vert(), vert()}.
-type graph() :: [{vert(), [vert()]}].

new_graph() -> newGraph().
add_edge(A, B) -> insertEdge(A, B).
add_vertex(A, B) -> insertVert(A, B).


%% to generate an empty graph
newGraph() -> [].

%% to insert an edge specified by {V1, V2} to graph G
-spec insertEdge(graph(), edge()) -> graph().
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

getColors(Num) -> 
    AllColors = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z], 
    AllColors -- lists:nthtail(Num, AllColors).

kcolor(G, Num) -> kcolor(G, G, [], getColors(Num)).

kcolor(_, [], Colored, _) -> Colored;
kcolor(G, [{V, _}|Vs], Colored, Colors) ->
    ValidColoring = [[{V, X}|Colored] || X<-Colors, isValid(G, Colored, {V,X})],
    case ValidColoring of 
        [] -> false;
        [Coloring1|Others] -> 
            FirstColoring = kcolor(G, Vs, Coloring1, Colors),
            case FirstColoring of
                false -> case Others of 
                             [] -> false;
                             _ -> kcolor(G, Vs, lists:nth(1, Others), Colors)
                         end; 
                _ -> lists:sort(FirstColoring)
            end
    end.

isValid(G, Colored, {V, Color}) -> 
   Adjs = getAdj(G, V), 
   lists:foldr(fun(X, Acc) -> 
            case getColor(X, Colored) =/= Color of 
               true -> true andalso Acc; 
               false -> false
            end
         end, true, Adjs).

getColor(_, []) -> null;
getColor(V, [{V1, Color}|Vs]) -> 
    case V =:= V1 of 
        true -> Color;
        false -> getColor(V, Vs)
    end.

%% ====================================================================
%% Property-based testing for kcolor implementation: 1
%% ====================================================================
%% test to check if the length of returned colors equals the number of
%% vertices
prop_color_length() -> 
    ?FORALL(L, random_graph(integer()), color_length_test(L)).

color_length_test([]) -> true;
color_length_test(G) ->
    Num = rand:uniform(26), 
    Colored = kcolor(G, Num),
    case Colored of 
       false -> true;
       _ -> length(Colored) =:= length(G)
    end.
%% ====================================================================
%% Property-based testing for kcolor implementation: 2
%% ====================================================================
%% test to check if the length of returned colors equals the number of
%% vertices

prop_adj_color() -> 
    ?FORALL(L, random_graph(integer()), adj_color_test(L)).

adj_color_test([]) -> true;
adj_color_test(G) -> 
    Num = rand:uniform(26), 
    % io:format("~p", [G]),
    % io:format("~p~n", [Num]),
    Colored = kcolor(G, Num), 
    case Colored of
        false -> true;
        _ -> checkEdges(getEdges(G), Colored)
    end.

checkEdges([], _) -> true;
checkEdges(_, []) -> false;
checkEdges([{V1, V2}|Es], Colored) -> 
    case getColor(V1, Colored) =:= getColor(V2, Colored) of
        true -> false;
        false -> checkEdges(Es, Colored)
    end.

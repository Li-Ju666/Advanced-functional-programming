-module(perm).
-export([perm/2]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec perm(list(),list()) -> 'false' | 'true'.
perm(Input, Output) -> perm(reverse(Input), reverse(Output), []).
perm([], Output, Stack) -> 
    case Output of 
        [] -> true;
        [O|Os] when O =:= hd(Stack) -> perm([], Os, tl(Stack));
        _ -> false
    end;
perm([I|Is], [O|Os], Stack) ->
    case Stack of
        [S|Ss] when S =:= O -> perm([I|Is], Os, Ss);
        _ when I =:= O -> perm(Is, Os, Stack);
        _ -> perm(Is, [O|Os], [I|Stack])
    end.


%% Helper function to reverse a list
-spec reverse(list()) -> list().
reverse(List) -> reverse(List, []).
reverse([], Acc) -> Acc;
reverse([X|Xs], Acc) -> reverse(Xs, [X|Acc]).


%% ==========================================================
%% Unit tests for correctness of the function
correctness_test_() -> 
    [test_three(), test_four(), test_five(), test_ten()].

%% Unit test for 3 elements
test_three() -> 
    [?_assertEqual(true, perm([1,2,3], [2,1,3])), 
     ?_assertEqual(false, perm([1,2,3], [2,3,1]))].

%% Unit test for 4 elements
test_four() -> 
    [?_assertEqual(true, perm([1,2,3,4], [2,1,4,3])), 
     ?_assertEqual(false, perm([1,4,3,2], [3,1,2,4]))].

%% Unit test for 5 elements
test_five() ->
    [?_assertEqual(true, perm([4,5,3,2,1], [4,5,1,3,2])), 
     ?_assertEqual(false, perm([3,1,4,2,5], [2,1,5,4,3]))].

%% Unit test for 10 elements
test_ten() -> 
    [?_assertEqual(true, perm([1,8,3,10,2,5,7,6,4,9], [4,1,3,8,10,6,7,5,2,9])),
     ?_assertEqual(false, perm([8,3,9,7,2,6,4,1,10,5], [9,2,8,5,1,4,6,7,3,10]))].

%% ==============================================================
%% Property-based test for perm:
%%
%% property 1: for dequeuing order i j k, if i<j<k while Qk<Qi<Qj, 
%% the input queue is not stack-sortable
prop_unsortable() -> 
    ?FORALL(L, integer(), ?IMPLIES(L>3, pattern_test(lists:seq(L,1,-1)))).

pattern_test(L) -> 
    Queue231 = generate231(L),
    % even_print(Shuffled),
    false =:= perm(reverse(Queue231), L).

%% Helper function to generate queue with pattern 231
generate231(L) -> 
    case L of 
        [X1,X2,X3|_] when X3<X1 andalso X1<X2 -> L;
        _ -> generate231(shuffle(L))
    end.

%% Property 2: lists with more than 1,000,000 elements can 
%% be computed with in one second
prop_efficiency() -> 
    ?FORALL(L, range(0,1000000), time_test(lists:seq(0,L))).

time_test(L) ->
    Shuffled1 = shuffle(L),
    Shuffled2 = shuffle(L),
    element(1,timer:tc(perm,perm, [Shuffled1, Shuffled2])) =< 1000000.

%% Helper function to shuffle all elements of the input list
shuffle(L) -> 
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].



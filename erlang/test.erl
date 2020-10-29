-module(test).
-export([sort/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("proper/include/proper.hrl").


sort([]) -> [];
sort([P|Xs]) -> 
    sort([X || X<-Xs, X<P]) ++ [P] ++ sort([X || X<-Xs, X>=P]).

% sort_test_() -> 
%     [test_zero(), test_two(), test_four()].

% test_zero() -> 
%     [?_assertEqual([], sort([]))].

% test_two() -> 
%     [?_assertEqual([17,42], sort([17,42])), 
%      ?_assertEqual([17,42], sort([42,17]))].

% test_four() -> 
%     [?_assertEqual([1,2,3,4], sort([2,1,3,4]))].

% another_test_() -> 
%     [test_five()].

% test_five() -> 
%     [?_assertEqual([1,2,3,4,5], sort([2,1,5,4,3])), 
%      ?_assertEqual([1,2,3,4,5], sort([1,2,3,4,5]))].

prop_ordered() -> 
    ?FORALL(L, list(integer()), ordered(sort(L))).

ordered([]) -> true;
ordered([_]) -> true; 
ordered([A,B|T]) -> A =< B andalso ordered([B|T]).

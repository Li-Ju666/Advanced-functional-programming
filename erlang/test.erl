-module(test).
-export([sort/1, sort_test/0]).

sort([]) -> [];
sort([P|Xs]) -> 
    sort([X || X<-Xs, X<P]) ++ [P] ++ sort([X || X<-Xs, X>=P]).

sort_test()->
    [] = sort([]), 
    [17,42] = sort([42,17]), 
    [17,42] = sort([17,42]), 
    [1,3,2,4] = sort([3,1,2,4]), 
    ok. 

-module(tut).
-export([fac/1, double/1, mult/2, convert/1, list_length/1]).

double(X) -> 2*X.

fac(1) -> 1;
fac(X) -> X*fac(X-1).

mult(X,Y) -> X*Y.

convert({cm, X}) -> {inch, X/2.54};
convert({inch, X}) -> {cm, X*2.54}.

list_length(Xs)->list_length(Xs,0).

list_length([], Acc) -> Acc;
list_length([_|Tail], Acc)->list_length(Tail, Acc+1).

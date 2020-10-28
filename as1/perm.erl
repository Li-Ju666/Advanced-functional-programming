-module(perm).
-export([perm/2]).

reverse([], Acc) -> Acc;
reverse([X|Xs], Acc) -> reverse(Xs, [X|Acc]).

perm(Input, Output) -> perm(reverse(Input,[]), reverse(Output,[]), []).

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




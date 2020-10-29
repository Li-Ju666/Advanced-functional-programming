-module(msg).
-export([start/0]).

start() -> 
    spawn(fun() -> loop(0) end).

loop(Count) -> 
    NC = receive
             {report, Pid} -> Pid ! Count;
             _ -> Count + 1
         end, 
    loop(NC).

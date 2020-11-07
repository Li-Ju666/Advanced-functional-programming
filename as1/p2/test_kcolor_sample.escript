#!/usr/bin/env escript
%% -*- erlang-indent-level: 2 -*-
%%! +S1 -pa ..

main(_Args) ->
  task("Graph k-coloring", kcolor, 2),
  ok.

grade(kcolor) -> grade_kcolor().

%%==============================================================================

task(Title, Module, Points) ->
  io:format("=== ~s ===~n", [Title]),
  Score =
    try Module:module_info() of
      _ -> grade(Module)
    catch
      _:_ ->
        io:format("Missing...~n"),
        0.0
    end,
  io:format("--------------------~n"),
  io:format("~s Grade: ~.1f/~.1f~n~n",[Title, Score*Points, Points*1.0]),
  io:format("NOTE: THIS IS A SAMPLE PROGRAM, NOT YOUR ACTUAL GRADE~n"),
  Score.

%%==============================================================================

grade_kcolor() ->
  {Correct, Total} = run_tests(inputs(), 0, 0),
  Correct * 1.0 / Total.

run_tests([], Correct, Total) -> {Correct, Total};
run_tests([T|Tests], Correct, Total) ->
  io:format("Running test ~2B... ", [Total+1]),
  S = self(),
  C = spawn(fun() -> run_test(S, T) end),
  receive
    {C, true} ->
      io:format("[SUCCESS]~n"),
      run_tests(Tests, Correct+1, Total+1);
    {C, false} ->
      io:format("[FAILED]~n"),
      run_tests(Tests, Correct, Total+1)
  after
    10000 ->
      exit(C, kill),
      io:format("[TIMEOUT]~n"),
      run_tests(Tests, Correct, Total+1)
  end.

run_test(S, {Graph, NumColors, Possible}) ->
  Result = kcolor:kcolor(Graph, NumColors),
  S ! {self(), check_solution({Graph, NumColors, Possible}, Result)},
  ok.

check_solution({Graph, Colors, Possible}, Solution) ->
  io:format("~nWARNING: THIS SAMPLE TEST PROGRAM DOES NOT ACTUALLY TEST YOUR CODE CORRECTLY!!!~n"),
  io:format("THIS IS ONLY TO MAKE SURE YOUR CODE FOLLOWS THE CORRECT INTERFACE.~n"),
  true.

%
% Inputs 
%

inputs() ->
  [
   % Small tests
   {[{1, [2, 3]}, {2, [1, 3]}, {3, [1, 2]}], 3, true},
   {[{1, [2, 3]}, {2, [1, 3]}, {3, [1, 2]}], 1, false}
  ].

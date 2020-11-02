#!/usr/bin/env escript
%% -*- erlang-indent-level: 2 -*-
%%! +S1 -pa ..

main(_Args) ->
  task("Stack permutations", perm, 3),
  ok.

grade(perm) -> grade_perm().

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

grade_perm() ->
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
    2000 ->
      exit(C, kill),
      io:format("[TIMEOUT]~n"),
      run_tests(Tests, Correct, Total+1)
  end.

run_test(S, {Input, Output, Expected}) ->
  Result = perm:perm(Input, Output),
  S ! {self(), Result == Expected},
  ok.

%
% Inputs 
%

inputs() ->
  [
   {[1, 2, 3], [3, 1, 2], true},
   {[1, 2, 3], [2, 3, 1], false}
  ].

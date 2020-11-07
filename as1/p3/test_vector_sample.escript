#!/usr/bin/env escript
%% -*- erlang-indent-level: 2 -*-
%%! +S1 -pa ..

main(_Args) ->
  task("Vector Server", vector_server, 5),
  ok.

grade(vector_server) -> grade_vector().

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
  io:format("~13s Grade: ~.1f/~.1f~n~n",[Title, Score, Points*1.0]),
  io:format("NOTE: THIS IS A SAMPLE PROGRAM, NOT YOUR ACTUAL GRADE~n"),
  Score.

%%==============================================================================

wrong(Format) ->
  wrong(Format, []).
wrong(Format, Args) ->
  reply("  *** WRONG! ", Format, Args, none).

correct(Format) ->
  correct(Format, []).
correct(Format, Args) ->
  reply("  *** CORRECT! ", Format, Args, whole).

half(Format) ->
  half(Format, []).
half(Format, Args) ->
  reply("  *** ALMOST! ", Format, Args, half).

reply(Comment, Format, Args, Value) ->
  io:format(Comment ++ Format ++ "~n", Args),
  Value.

%%==============================================================================

grade_vector() ->
  P = self(),
  Ref = make_ref(),
  {C, MR} = spawn_monitor(fun() -> grade_vector({P, Ref}) end),
  collect_points(C, MR, Ref, 0).

send_points({P, Ref}, Points) ->
  P ! {Ref, Points}.

collect_points(Pid, Monitor, Ref, Score) ->
  receive
    {Ref, V} -> collect_points(Pid, Monitor, Ref, Score + V);
    {'DOWN', Monitor, process, Pid, Reason} ->
      case Reason =:= normal of
        true -> ok;
        false ->
          wrong("Server crashed (~p). Skipping rest of tests.", [Reason])
      end,
      Total=
        1 + % Initialization
        vector_score(eval_set_1()) +
        vector_score(eval_set_2()) +
        vector_score(eval_set_3())
        %+ 1 Extra point for stopping correctly
        ,
      io:format("~nTotal: ~w/~w points~n", [Score,Total]),
      min(5.0,Score/(Total)*5)
  end.

vector_score(List) ->
  0 +
    1 +            % Connection
    2 +            % Protocol
    length(List) + % Cases
    1.             % Correct Disconnection

grade_vector(ParentRef) ->
  try
    io:format("Starting the server:~n"),
    {ok, _Server} = vector_server:start_link(),
    correct("1/1 points"),
    send_points(ParentRef, 1),
    inner(ParentRef)
  catch
    _:_ ->
      wrong("vector_server:start_link() failed. Skipping rest of tests.")
  end.

inner(ParentRef) ->
  connect_server(ParentRef, 1, eval_set_1()),
  connect_server(ParentRef, 2, eval_set_2()),
  connect_server(ParentRef, 3, eval_set_3()),
  Terminating = stop_server(),
  send_points(ParentRef, Terminating).

connect_server(ParentRef, Attempt, Inputs) ->
  try
    io:format("Connecting to server (attempt ~p):~n", [Attempt]),
    {ok, Socket} = gen_tcp:connect("localhost", 1055, [binary, {packet, 0}]),
    correct("1/1 points"),
    send_points(ParentRef, 1),
    io:format("Inspecting compilance with specification:~n"),
    Flags = testeval(Socket),
    case Flags of
      [out_res] ->
        correct(" 2/2 points"),
        send_points(ParentRef, 2);
      [in_dots|_] ->
        wrong("Specification did not use dots. 0/2 points");
      [] ->
        wrong("Specification required a 'Res:' before result. 0/2 points");
      [none] ->
        wrong("Reply does not follow specification. 0/2 points")
    end,
    Eval1 = evaluate(ParentRef, Socket, format(Flags, Inputs)),
    io:format("Disconnecting from server (attempt ~p):~n", [Attempt]),
    ok = gen_tcp:close(Socket),
    receive after 500 -> ok end,
    correct("1/1 points"),
    send_points(ParentRef, 1),
    ok
  catch
    _:_ ->
      wrong("Could not connect to port 1055. Skipping rest of tests.")
  end.

% [begin [C] = proper:counterexample(), E = vectors:vector_4(C), A = try
% (vectors:vector(I))(C) catch _:_ -> crash end, {I, C, E, A} end || I <-
% lists:seq(1,50), not proper:quickcheck(prop:prop(vectors:vector(I)), [quiet,
% 2000])].

% [io:format("{\"~w\",\"~w\"},~n",[I,O]) || {_, I, O, _} <- v(18)].

format([], Inputs) -> Inputs;
format([none], Inputs) -> Inputs;
format([in_dots|Rest], Inputs) ->
  NewInputs = [{I++".",O} || {I,O} <- Inputs],
  format(Rest, NewInputs);
format([out_res|Rest], Inputs) ->
  NewInputs = [{I,"Res: " ++ O} || {I,O} <- Inputs],
  format(Rest, NewInputs).

eval_set_1() ->
  [
   {"[0]","[0]"}
  ].

eval_set_2() ->
  [
   {"{sub,[2],[0]}","[2]"}
  ].

eval_set_3() ->
  [
   {"[]","error"}
  ].

testeval(Socket) ->
  case one_case(Socket, "[0]") of
    "Res: [0]" -> [out_res];
    "[0]" -> [];
    _Other1 ->
      case one_case(Socket, "[0].") of
        "Res: [0]" -> [in_dots, out_res];
        "[0]" -> [in_dots];
        _Other2 -> [none]
      end
  end.

evaluate(_ParentRef, _Socket, []) -> ok;
evaluate(ParentRef, Socket, [{In, Exp}|Inputs]) ->
  S = do_evaluate(Socket, In, Exp),
  send_points(ParentRef, S),
  evaluate(ParentRef, Socket, Inputs).  

do_evaluate(Socket, Input, Expected) ->
  io:format("Evaluating ~s:~n", [Input]), 
  case one_case(Socket, Input) of
    timeout ->
      wrong("Server took more than 1s to reply. 0/1 points"),
      0;
    Expected ->
      correct("(reply was ~s). 1/1 points", [Expected]),
      1;
    String ->
      wrong("Expected ~s but reply was ~s. 0/1 points", [Expected, String]),
      0
  end.
      
      
one_case(Socket, Input) ->
  ok = gen_tcp:send(Socket, Input),
  receive
    {tcp,Socket,Binary} ->
      [$\n|Reverse] = lists:reverse(binary_to_list(Binary)),
      lists:reverse(Reverse)
  after
    1000 ->
      receive
        _ -> ok
      after
        500 -> ok
      end,
      timeout
  end.

stop_server() ->
  io:format("Stopping the server:~n"),
  try vector_server:stop() of
    ok ->
      correct("1/1 EXTRA points"),
      1
  catch
    _:_ ->
      wrong("did not return ok. 0/1 EXTRA points"),
      0
  end.

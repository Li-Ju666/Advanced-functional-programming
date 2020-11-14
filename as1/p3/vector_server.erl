%%-------------------------------------------------------------------
%% @author Li Ju
%%-------------------------------------------------------------------

-module(vector_server).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, get_count/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% -include_lib("proper/include/proper.hrl").
% -include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
    % receive normalExit -> start_link(Port) end.

%% @spec start_link() -> {ok, Pid}
%% @doc Calls `start_link(Port)' using the default port.
start_link() ->
    start_link(?DEFAULT_PORT).

%% --------------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    % io:format("Initializing"),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};

handle_info({tcp_closed, _}, State) ->
    handle_info(timeout,State#state{request_count = 0}); 

handle_info(timeout, #state{lsock = LSock} = State) ->
    io:format("Waiting to get connected...~n"),
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Terminate function called"), 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_rpc(Socket, RawData) ->
        % io:format(RawData),
    case evaluate(RawData) of
        error -> gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [error]));
        Result -> gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [Result]))
    end.

evaluate(RawData) ->
    {ok, Ts, _} = erl_scan:string(RawData ++ "."),
    case erl_parse:parse_term(Ts) of
            {ok, Term} -> try evaluate(Term, 0) of 
                              Result -> Result
                          catch
                              _:_ -> error
                          end;
            _ -> error
    end.

%% evaluate single vector
evaluate([], _) -> throw(empty_list);
evaluate([X|Xs], _) -> 
    case length([X|Xs])>100 of 
        true -> throw(too_long_list);
        false -> [X|Xs]
    end;

%% evaluate norm operation
evaluate({Operation, E}, Depth) -> 
    case Depth>100 of
        true -> throw(too_deep_expression);
        false -> 
            Arg = evaluate(E, Depth+1),
            case Operation of
                norm_one -> norm_one(Arg);
                norm_inf -> norm_inf(Arg)
            end
             % end
    end;

%% evaluate vector and scalar operation
evaluate({Operation, E1, E2}, Depth) -> 
    case Depth>100 of
        true -> throw(too_deep_expression);
        false -> 
            Arg1 = evaluate(E1, Depth+1),
            Arg2 = evaluate(E2, Depth+1),
            case Operation of
                add -> add(Arg1,Arg2);
                sub -> sub(Arg1,Arg2);
                dot -> dot(Arg1,Arg2);
                mul -> mul(Arg1,Arg2);
                'div' -> divide(Arg1,Arg2)
            end
     end;

%% evaluate scalar 
evaluate(Val, _) -> 
    case is_integer(Val) of
        true -> Val;
        false -> throw(badarg)
    end.

% vector operations ==============================
add(V1, V2) -> 
    case length(V1) =:= length(V2) of
        true -> [X1+X2||{X1,X2}<-lists:zip(V1,V2)];
        false -> throw(different_length) 
    end.

sub(V1, V2) -> 
    case length(V1) =:= length(V2) of
        true -> [X1-X2||{X1,X2}<-lists:zip(V1,V2)];
        false -> throw(different_length) 
    end.

dot(V1, V2) -> 
    case length(V1) =:= length(V2) of
        true -> [X1*X2||{X1,X2}<-lists:zip(V1,V2)];
        false -> throw(different_length) 
    end.

%% scalar operations =============================
mul(A, V) -> [X*A||X<-V].

divide(0, _) -> throw(badarg);
divide(A, V) -> [X div A||X<-V].

%% norm operations ==============================
norm_one(V) -> lists:sum([abs(X)||X<-V]).
norm_inf(V) -> lists:max([abs(X)||X<-V]).

% %% =============== test functions =============================
% %% property-based testing for evaluate function  ==============
% %% ============================================================
% generate_n_exp(0) -> {add, [0], [0]};
% generate_n_exp(N) -> {add, [1], generate_n_exp(N-1)}.

% prop_depth_test() -> 
%     ?FORALL(L, non_neg_integer(), evaluate_test(
%                                     lists:flatten(io_lib:format("~p", [generate_n_exp(L)])), L)).

% evaluate_test(Exp, Depth) -> 
%     case Depth > 100 of
%         true -> evaluate(Exp) =:= error;
%         false -> evaluate(Exp) =:= [Depth]
%     end.

%%-------------------------------------------------------------------
%% @author Li Ju
%%-------------------------------------------------------------------

-module(vector_server).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         start_link/0,
         get_count/0,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
    {ok, Term} = erl_parse:parse_term(Ts),
    evaluate(Term,0).

evaluate([], _) -> error;
evaluate([X|Xs], _) -> 
    case length([X|Xs])>100 of 
        true -> error;
        false -> [X|Xs]
    end;
evaluate({Operation, E}, Depth) -> 
    case Depth>100 of
        true -> error;
        false -> 
            case Operation of 
                norm_one -> norm_one(evaluate(E, Depth+1));
                norm_inf -> norm_inf(evaluate(E, Depth+1))
            end
    end;
evaluate({Operation, E1, E2}, Depth) -> 
    case Depth>100 of
        true -> error;
        false -> 
            % A1 -> evaluate(E1, Depth+1)
            case Operation of
                add -> add(evaluate(E1, Depth+1), evaluate(E2, Depth+1));
                sub -> sub(evaluate(E1, Depth+1), evaluate(E2, Depth+1));
                dot -> dot(evaluate(E1, Depth+1), evaluate(E2, Depth+1));
                mul -> mul(evaluate(E1, Depth+1), evaluate(E2, Depth+1));
                'div' -> divide(evaluate(E1, Depth+1), evaluate(E2, Depth+1))
             end
     end;
%% integer evaluation to be re-writed
evaluate(Value, _) -> Value.

add(V1, V2) -> 
    case length(V1) =:= length(V2) of
        true -> [X1+X2||{X1,X2}<-lists:zip(V1,V2)];
        false -> error
    end.
sub(V1, V2) -> 
    case length(V1) =:= length(V2) of
        true -> [X1-X2||{X1,X2}<-lists:zip(V1,V2)];
        false -> error
    end.
dot(V1, V2) -> 
    case length(V1) =:= length(V2) of
        true -> [X1*X2||{X1,X2}<-lists:zip(V1,V2)];
        false -> error
    end.

mul(A, V) -> [X*A||X<-V].

divide(0, _) -> error;
divide(A, V) -> [X div A||X<-V].

norm_one(V) -> lists:sum([abs(X)||X<-V]).
norm_inf(V) -> lists:max([abs(X)||X<-V]).

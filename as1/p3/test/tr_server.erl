-module(tr_server).
-behaviour(gen_server).

-export([]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count=0}).

%% ==================================================
%% API
%% ==================================================

start_link(Port) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
start_link() ->
    start_link(?DEFAULT_PORT).

get_count() -> 
    gen_server:call(?SERVER, get_count).

stop() -> 
    gen_server:cast(?SERVER, stop).

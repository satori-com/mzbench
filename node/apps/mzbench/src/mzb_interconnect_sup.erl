-module(mzb_interconnect_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_client/3]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Port, Handler) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [main, Port, Handler]).

start_client(Host, Port, Role) ->
    supervisor:start_child(mzb_interconnect_clients, [Host, Port, Role]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([main, Port, Handler]) ->
    {ok, {{one_for_all, 100, 100}, [
            {main_server, {mzb_interconnect, start_link, [Handler]}, permanent, 5000, worker, [mzb_interconnect]},
            ranch:child_spec(interconnect_server, 10, ranch_tcp, [{port, Port}], mzb_interconnect_server, []),
            {interconnect_clients, {supervisor, start_link, [{local, mzb_interconnect_clients}, ?MODULE, [clients]]}, permanent, infinity, supervisor, []}
        ]}};

init([clients]) ->
    {ok, {{simple_one_for_one, 100, 100}, [
            {interconnect_client, {mzb_interconnect_client, start_link, []}, permanent, 5000, worker, [mzb_interconnect_client]}
        ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


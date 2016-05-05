-module(mzb_dummycloud_plugin).

-export([
    start/2,
    create_cluster/3,
    destroy_cluster/1
]).

%%%===================================================================
%%% API
%%%===================================================================

start(Name, Opts) -> {Name, Opts}.

create_cluster({_Name, _Opts}, _N, _Config) ->
    {ok, _Ref = erlang:make_ref(), _User = undefined, ["127.0.0.1"]}.

destroy_cluster(_Ref) ->
    ok.



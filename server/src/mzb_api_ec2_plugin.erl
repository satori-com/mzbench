-module(mzb_api_ec2_plugin).

-export([start/2, create_cluster/3, destroy_cluster/1]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-define(POLL_INTERVAL, 2000).
-define(MAX_POLL_COUNT, 150).

% ===========================================================
% Public API
% ===========================================================

start(_Name, Opts) ->
    Opts.

-spec create_cluster(#{}, NumNodes :: pos_integer(), Config :: #{}) -> {ok, term(), string(), [string()]}.
create_cluster(Opts = #{instance_user:= UserName}, NumNodes, Config) when is_integer(NumNodes), NumNodes > 0 ->
    {ok, Data} = erlcloud_ec2:run_instances(instance_spec(NumNodes, Opts), get_config(Opts)),
    Instances = proplists:get_value(instances_set, Data),
    Ids = [proplists:get_value(instance_id, X) || X <- Instances],
    lager:info("AWS ids: ~p", [Ids]),
    try
        erlcloud_ec2:create_tags(Ids, [{"Name", maps:get(purpose, Config, "")}], get_config(Opts)),
        wait_nodes_start(Ids, Opts, ?MAX_POLL_COUNT),
        {ok, [NewData]} = get_description(Ids, Opts, ?MAX_POLL_COUNT),
        lager:info("~p", [NewData]),
        {Kind, Hosts} = get_hosts(Ids, NewData),
        wait_nodes_ssh(Hosts, ?MAX_POLL_COUNT),
        case Kind of
            dns_name -> ok; % when dns names are used for hosts there is no need to set them
            _ -> update_hostfiles(UserName, Hosts, Opts)
        end,
        {ok, {Opts, Ids}, UserName, Hosts}
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            destroy_cluster({Opts, Ids}),
            erlang:raise(C,E,ST)
    end.

get_description(_, _, C) when C < 0 -> {ec2_error, cluster_getinfo_timeout};
get_description(Ids, Opts, C) ->
    case erlcloud_ec2:describe_instances(Ids, get_config(Opts)) of
        {ok, _} = Data -> Data;
        _ -> timer:sleep(?POLL_INTERVAL),
             get_description(Ids, Opts, C - 1)
    end.

% try to extract dns names or ip addresses for allocated hosts
-spec get_hosts([string()], [any(), ...]) -> {dns_name | ip_address | private_ip_address, [string(), ...]}.
get_hosts(Ids, Data) ->
    get_hosts(Ids, Data, [dns_name, ip_address, private_ip_address]).

-spec get_hosts([string()], [any()], [atom()]) -> {atom(), [string()]}.
get_hosts(_, _, []) -> erlang:error({ec2_error, couldnt_obtain_hosts});
get_hosts(Ids, Data, [H | T]) ->
    Instances = proplists:get_value(instances_set, Data),
    Hosts = [proplists:get_value(H, X) || X <- Instances],
    case Hosts of
        [R | _] when R =/= undefined, R =/= "" -> {H, Hosts};
        _ -> get_hosts(Ids, Data, T)
    end.

-spec destroy_cluster({#{}, [term()]}) -> ok.
destroy_cluster({Opts, Ids}) ->
    R = erlcloud_ec2:terminate_instances(Ids, get_config(Opts)),
    lager:info("Deallocating ids: ~p, result: ~p", [Ids, R]),
    {ok, _} = R,
    ok.

update_hostfiles(UserName, Hosts, #{host_prefix:= HPrefix}) ->
    Logger = mzb_api_app:default_logger(),
    _ = lists:map(
        fun ({N, H}) ->
            HostName = mzb_string:format("~s~b", [HPrefix, N]),
            Cmd1 = io_lib:format("sudo hostname ~s", [HostName]),
            _ = mzb_subprocess:remote_cmd(UserName, [H], Cmd1, [], Logger),
            Cmd2 = mzb_string:format("sudo sh -c 'echo \"~s     ~s\" >> /etc/hosts'", [H, HostName]),
            mzb_subprocess:remote_cmd(UserName, Hosts, Cmd2, [], Logger)
        end, mzb_lists:enumerate(Hosts)),
    ok;
update_hostfiles(UserName, Hosts, _) ->
    Logger = mzb_api_app:default_logger(),
    _ = lists:map(fun (H) -> mzb_subprocess:remote_cmd(UserName, Hosts,
        mzb_string:format("sudo sh -c 'echo \"~s     ip-~s\" >> /etc/hosts'", [H, string:join(string:tokens(H, "."), "-")]), [], Logger) end, Hosts),
    ok.

wait_nodes_ssh(_, C) when C < 0 -> erlang:error({ec2_error, cluster_ssh_start_timed_out});
wait_nodes_ssh([], _) -> ok;
wait_nodes_ssh([H | T], C) ->
  R = gen_tcp:connect(H, 22, [], ?POLL_INTERVAL),
  case R of
        {ok, Socket} -> gen_tcp:close(Socket), wait_nodes_ssh(T, C);
        _ -> timer:sleep(?POLL_INTERVAL), wait_nodes_ssh([H | T], C - 1)
  end.

wait_nodes_start(_, _, C) when C < 0 -> erlang:error({ec2_error, cluster_start_timed_out});
wait_nodes_start([], _, _) -> ok;
wait_nodes_start([H | T], Opts, C) ->
    {ok, Res} = erlcloud_ec2:describe_instance_status([{"InstanceId", H}], [], get_config(Opts)),
    lager:info("Waiting nodes result: ~p", [Res]),
    Status = case Res of
        [P | _] -> proplists:get_value(instance_state_name, P);
             _  -> undefined
    end,
    case Status of
        "running"  -> wait_nodes_start(T, Opts, C - 1);
        _ -> timer:sleep(?POLL_INTERVAL),
             wait_nodes_start([H | T], Opts, C - 1)
    end.

instance_spec(NumNodes, #{instance_spec:= Ec2AppConfig}) ->
    lists:foldr(fun({Name, Value}, A) -> set_record_element(A, Name, Value) end,
        #ec2_instance_spec{
            min_count = NumNodes,
            max_count = NumNodes,
            iam_instance_profile_name = "undefined"},
        Ec2AppConfig).

get_config(#{config:= AppConfig}) ->
    lists:foldr(fun({Name, Value}, A) -> set_record_element(A, Name, Value) end,
        #aws_config{}, AppConfig).

% Following three functions no more than just a record field setters
set_record_element(Record, Field, Value) ->
    RecordName = erlang:element(1, Record),
    setelement(record_field_num(RecordName, Field), Record, Value).

record_field_num(Record, Field) ->
    Fields = record_fields(Record),
    case length(lists:takewhile(fun (F) -> F /= Field end, Fields)) of
        Length when Length =:= length(Fields) ->
            erlang:error({bad_record_field, Record, Field});
        Length -> Length + 2
    end.

record_fields(ec2_instance_spec) -> record_info(fields, ec2_instance_spec);
record_fields(aws_config) -> record_info(fields, aws_config).

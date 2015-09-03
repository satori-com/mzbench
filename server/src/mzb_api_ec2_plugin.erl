-module(mzb_api_ec2_plugin).

-export([create_cluster/3, destroy_cluster/1]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-define(POLL_INTERVAL, 2000).
-define(MAX_POLL_COUNT, 150).

% ===========================================================
% Public API
% ===========================================================

-spec create_cluster(Name :: string(), NumNodes :: pos_integer(), Config :: #{}) -> {ok, term(), string(), [string()]}.
create_cluster(Name, NumNodes, _Config) when is_list(Name), is_integer(NumNodes), NumNodes > 0 ->

    {ok, Data} = erlcloud_ec2:run_instances(instance_spec(NumNodes), get_config()),
    Instances = proplists:get_value(instances_set, Data),
    Ids = [proplists:get_value(instance_id, X) || X <- Instances],
    Hosts = [proplists:get_value(private_ip_address, X) || X <- Instances], %private_dns_name
    lager:info("AWS ids: ~p, hosts: ~p", [Ids, Hosts]),
    wait_nodes_start(Ids, ?MAX_POLL_COUNT),
    wait_nodes_ssh(Hosts, ?MAX_POLL_COUNT),
    {ok, UserName} = application:get_env(mzbench_api, ec2_instance_user),
    update_hostfiles(UserName, Hosts),
    {ok, Ids, UserName, Hosts}.

-spec destroy_cluster([term()]) -> ok.
destroy_cluster(Ids) ->
    R = erlcloud_ec2:terminate_instances(Ids, get_config()),
    lager:info("Deallocating ids: ~p, result: ~p", [Ids, R]),
    {ok, _} = R,
    ok.

update_hostfiles(UserName, Hosts) ->
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

wait_nodes_start(_, C) when C < 0 -> erlang:error({ec2_error, cluster_start_timed_out});
wait_nodes_start([], _) -> ok;
wait_nodes_start([H | T], C) ->
    {ok, Res} = erlcloud_ec2:describe_instance_status([{"InstanceId", H}], [], get_config()),
    lager:info("Waiting nodes result: ~p", [Res]),
    Status = case Res of
        [P | _] -> proplists:get_value(instance_state_name, P);
             _  -> undefined
    end,
    case Status of
        "running"  -> wait_nodes_start(T, C - 1);
        _ -> timer:sleep(?POLL_INTERVAL),
              wait_nodes_start([H | T], C - 1)
    end.

instance_spec(NumNodes) ->
    {ok, Ec2AppConfig} = application:get_env(mzbench_api, ec2_instance_spec),
    lists:foldr(fun({Name, Value}, A) -> set_record_element(A, Name, Value) end,
        #ec2_instance_spec{
            min_count = NumNodes,
            max_count = NumNodes,
            iam_instance_profile_name = "undefined"},
        Ec2AppConfig).

get_config() ->
    {ok, AppConfig} = application:get_env(mzbench_api, aws_config),
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

-module(mzb_hooks_validator).

-export([validate/1]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

validate(#operation{name = Name, args = [Args]}) when Name == pre_hook; Name == post_hook ->
    lists:flatmap(fun validate/1, Args);
validate(#operation{name=exec, args=[Target, Cmd]}) ->
    TargetErr = case Target of
        T when T == all; T == director -> [];
        _ -> [mzb_string:format("Invalid hook target: '~s' (valid values are 'director' or 'all')", [Target])]
    end,

    CmdErr = case Cmd of
        C when is_list(C) -> [];
        _ -> [mzb_string:format("Invalid exec command target: '~s' (should be string)", [Cmd])]
    end,

    TargetErr ++ CmdErr;
validate(#operation{name=worker_call, args=[_Method | WorkerType]}) ->
    {Provider, Worker} = mzbl_script:resolve_worker_provider(WorkerType),
    Provider:validate(Worker);
validate(Command) ->
    [mzb_string:format("Invalid hook command ~p", [Command])].


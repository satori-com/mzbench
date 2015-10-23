-type abstract_expr() :: term().
-type meta() :: [{Key :: atom(), Value :: any()}].
-type script_value() :: term().
-type script_expr() :: tuple() | script_value().
-type script_loopspec() :: [tuple()].
-type script_validation_result() :: ok | {invalid_script, [string()]}.
-type pool() :: {[script_expr()], [tuple()]}.
-type named_pool() :: {atom(), [script_expr()], [tuple()]}.
-type worker_provider() :: mzb_erl_worker | mzb_lua_worker| mzb_python_worker.
-type worker_name() :: atom().
-type worker_state() :: term().
-type worker_env() :: [{Key :: any(), Value :: any()}].
-type type() :: number | integer | float
    | atom | string | binary
    | list | tuple
    | any | nil
    | time | rate | timeunit | rateunit.
-type typecheck_result()
    :: true
    | {false, Reason :: term(), Location :: string() | undefined}.

-record(operation, {
        is_std = true :: true | false,
        name = undefined :: atom(),
        args = [] :: abstract_expr(),
        meta = [] :: meta()
        }).
-type operation() :: #operation{}.

-record(constant, {
        value = undefined :: term(),
        units = undefined :: atom(),
        meta = [] :: meta()
        }).
-type constant() :: #constant{}.

-record(git_install_spec, {
        repo :: string(),
        branch :: string(),
        dir :: string()
        }).
-type git_install_spec() :: #git_install_spec{}.

-record(rsync_install_spec, {
        remote :: string(),
        dir :: string(),
        excludes :: [string()]
        }).
-type rsync_install_spec() :: #rsync_install_spec{}.

-type install_spec() :: git_install_spec() | rsync_install_spec().

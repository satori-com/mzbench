-module(mzbl_script).

-export([parse/1,
         hostname/1,
         add_indents/1,
         get_real_script_name/1,
         read/1,
         read_from_string/1,
         get_benchname/1,
         meta_to_location_string/1,
         normalize_env/1,
         extract_info/2,
         extract_install_specs/2,
         enumerate_pools/1,
         extract_worker/1,
         resolve_worker_provider/1,
         make_git_install_spec/4,
         make_rsync_install_spec/3,
         get_loop_assert_metrics/1,
         eval_opts/2]).

-include("mzbl_types.hrl").

-spec eval_opts([Operation], Env) -> [NewOperation]
    when Operation :: script_expr(),
         NewOperation :: script_expr(),
         Env :: [proplists:property()].
eval_opts(Opts, Env) ->
    lists:map(
        fun (#operation{args = Args} = Op) ->
            Op#operation{args = mzbl_interpreter:eval_std(Args, Env)}
        end, Opts).

-spec get_real_script_name([proplists:property()]) -> string().
get_real_script_name(Env) ->
    case lists:keyfind("mzb_script_name", 1, Env) of
        {_K, V} -> V;
        false -> erlang:error(no_real_script_name)
    end.

-spec meta_to_location_string(meta()) -> string().
meta_to_location_string(Meta) ->
    case proplists:get_value(line, Meta) of
        undefined -> "unknown line";
        LineNumber -> "line " ++ integer_to_list(LineNumber)
    end ++
    case proplists:get_value(column, Meta) of
        undefined -> "";
        ColumnNumber -> ", column " ++ integer_to_list(ColumnNumber)
    end ++ ": ".

-spec read_from_string(string()) -> abstract_expr().
read_from_string(String) ->
    try
        mzbl_literals:convert(parse(String))
    catch
        C:{parse_error, {_, Module, ErrorInfo}} = E ->
            ST = erlang:get_stacktrace(),
            lager:error("Parsing script file failed: ~s", [Module:format_error(ErrorInfo)]),
            erlang:raise(C,E,ST);
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error(
                "Failed to read script '~p' 'cause of ~p~nStacktrace: ~s",
                [String, E, pretty_errors:stacktrace(ST)]),
            erlang:raise(C,E,ST)
    end.

-spec read(string()) -> abstract_expr().
read(Path) ->
    try
        read_from_string(read_file(Path))
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error(
                "Failed to read script: ~p 'cause of ~p~nStacktrace: ~s",
                [Path, E, pretty_errors:stacktrace(ST)]),
            erlang:raise(C,E,ST)
    end.

-spec parse(string()) -> [script_expr()].
parse(Body) ->
    case bdl_script(Body) of
        true -> new_parser(Body);
        _ ->case erl_scan:string(Body) of
                {ok, [], _} -> [];
                {ok, Ts, _} ->
                    case erl_parse:parse_exprs(Ts) of
                        {ok, [AST]} ->
                            mzbl_ast:transform(AST);
                        {error, Error} ->
                            erlang:error({parse_error, Error})
                    end;
                {error, Error, _} ->
                    erlang:error({parse_error, Error})
            end
    end.

-spec new_parser(string()) -> term().
new_parser(Text) ->
    case mzbl_syntax:parse(add_indents(Text)) of
        {fail, Msg} -> erlang:error({parse_error, Msg});
        {scenario, List} -> mzbl_ast:new_records(List)
    end.

-spec bdl_script(string()) -> boolean().
bdl_script([$#, $!, $b, $e, $n, $c, $h, $D, $L, $\n | _]) -> true;
bdl_script(_) -> false.

-spec add_indents(string()) -> string().
add_indents(Text) ->
    Lines = string:tokens(Text, "\n"),
    {LineNumber, NewText, Indents, Br} = lists:foldl(fun process_line/2, {1, [], [0], []}, Lines),
    if Br =/= [] -> erlang:error({parse_error, "Wrong bracket count, unpaired: " ++ Br});
        true -> ok end,
    {IndentToken, _} = token_and_indents(LineNumber, Indents, 0),
    string:join(lists:reverse(NewText) ++ [IndentToken], "\n").

-spec process_line(string(), {integer(), [string()], [integer()], [char()]}) -> {integer(), [string()], [integer()], [char()]}.
process_line(Line, {LineNumber, SoFar, Indents, []}) ->
    NewIndent = get_indent(0, Line),
    {IndentToken, NewIndents} = token_and_indents(LineNumber, Indents, NewIndent),
    {LineNumber + 1, [IndentToken ++ Line | SoFar], NewIndents, eat_brackets([], Line, none)};
process_line(Line, {LineNumber, SoFar, Indent, Brackets}) ->
    {LineNumber + 1, [Line | SoFar], Indent, eat_brackets(Brackets, Line, none)}.

-spec token_and_indents(integer(), [integer()], integer()) -> {string(), [integer()]}.
token_and_indents(_, [H | T], N) when (H == N) or (N == -1) -> {[], [H | T]};
token_and_indents(_, [H | T], N) when H < N -> {"_INDENT_", [N, H | T]};
token_and_indents(LineNumber, L, N) ->
    case lists:member(N, L) of
        true -> {A, B} = lists:splitwith(fun (E) -> E > N end, L),
                {lists:flatten(["_DEDENT_ " || _X <- A]), B};
        false -> erlang:error({parse_error,
            lists:flatten(io_lib:format("Wrong indentation on Line ~p", [LineNumber]))})
    end.

-spec get_indent(integer(), [char()]) -> integer().
get_indent(N, [$   | Tail]) -> get_indent(N + 1, Tail);
get_indent(N, [$\t | Tail]) -> get_indent(N + 1, Tail);
get_indent(_, [$# | _]) -> -1;
get_indent(_, []) -> -1;
get_indent(N, _) -> N.

-spec eat_brackets([char()], string(), char() | none) -> [char()].
eat_brackets(Brackets, [], _) -> Brackets;
eat_brackets(Brackets, [$" | Rest], none) -> eat_brackets(Brackets, Rest, $");
eat_brackets(Brackets, [$" | Rest], $") -> eat_brackets(Brackets, Rest, none);
eat_brackets(Brackets, [$' | Rest], none) -> eat_brackets(Brackets, Rest, $');
eat_brackets(Brackets, [$' | Rest], $') -> eat_brackets(Brackets, Rest, none);
eat_brackets(Brackets, [$\\, $\\ | Rest], Mode) -> eat_brackets(Brackets, Rest, Mode);
eat_brackets(Brackets, [$\\, $" | Rest], Mode) -> eat_brackets(Brackets, Rest, Mode);
eat_brackets(Brackets, [$\\, $' | Rest], Mode) -> eat_brackets(Brackets, Rest, Mode);
eat_brackets(Brackets, [$# | _], none) -> Brackets;
eat_brackets(Brackets, [Br | Rest], none) when (Br == $() or (Br == $[) -> eat_brackets([Br | Brackets], Rest, none);
eat_brackets([$( | Brackets], [ $) | Rest], none) -> eat_brackets(Brackets, Rest, none);
eat_brackets([$[ | Brackets], [ $] | Rest], none) -> eat_brackets(Brackets, Rest, none);
eat_brackets(_, [Br | _], none) when (Br == $)) or (Br == $]) -> erlang:error({parse_error, "Wrong bracketing"});
eat_brackets(Brackets, [_ | Rest], Mode) -> eat_brackets(Brackets, Rest, Mode).

-spec get_loop_assert_metrics([script_expr()]) -> [string()].
get_loop_assert_metrics(Script) ->
    {_, Metrics} = mzbl_ast:mapfold(
        fun (#operation{name = while, args = [#operation{args = A}]} = Op, Acc) ->
                {Op, lists:filter(fun is_list/1, A) ++ Acc};
            (Op, Acc) -> {Op, Acc}
        end, [], Script),
    Metrics.

-spec extract_info([script_expr()], [{Key::term(), Value::term()}]) ->
    {[#operation{}], [proplists:property()], [proplists:property()]}.
extract_info(Script, Env) ->
    {Env2, Asserts} = lists:foldl(
            fun (#operation{name = include_resource, args = [NameExpr, PathExpr]}, {Acc, Ass}) ->
                    Name = mzbl_interpreter:eval_std(NameExpr, Env),
                    Path = mzbl_interpreter:eval_std(PathExpr, Env),
                    {[{{resource, Name}, import_resource(Env, Path, erlang)} | Acc], Ass};
                (#operation{name = include_resource, args = [NameExpr, PathExpr, Type]}, {Acc, Ass}) ->
                    Name = mzbl_interpreter:eval_std(NameExpr, Env),
                    Path = mzbl_interpreter:eval_std(PathExpr, Env),
                    {[{{resource, Name}, import_resource(Env, Path, Type)} | Acc], Ass};
                (#operation{name = defaults, args = [DefaultsList]}, {Acc, Ass}) ->
                    {interpret_defaults(DefaultsList, Env) ++ Acc, Ass};
                (#operation{name = assert, args = [TimeExpr, Expr]}, {Acc, Ass}) ->
                    Time = mzbl_literals:convert(mzbl_interpreter:eval_std(TimeExpr, Env)),
                    {Acc, [{Time, Expr} | Ass]};
                (_, Acc) -> Acc
            end, {Env, []}, Script),

    Script2 = lists:filter(fun (#operation{name = pool}) -> true; (_) -> false end, enumerate_pools(Script)),
    Script3 = mzbl_ast:map_meta(fun (Meta, Op) -> [{function, Op}|Meta] end, Script2),
    {Script3, Env2, Asserts}.

import_resource(Env, File, Type) ->
    {ok, Content} = case re:run(File, "^https?://", [{capture, first}, caseless]) of
        {match, _} ->
            {ok, Result} = httpc:request(File),
            {_, _, Body} = Result,
            {ok, Body};
        nomatch ->
            Root = proplists:get_value("bench_script_dir", Env),
            WorkerDirs = proplists:get_value("bench_workers_dir", Env),
            try
                file:read_file(filename:join(Root, File))
            catch
                error:{read_file_error, _, enoent} = E ->
                    Masks = [filename:join([D, "*", "resources", File]) || D <- WorkerDirs],
                    case lists:append([mzb_file:wildcard(M) || M <- Masks])  of
                        [] -> erlang:error(E);
                        [Path|_] -> file:read_file(Path)
                    end
            end
    end,
    convert(Content, Type).

-spec interpret_defaults([{string(), script_expr()}], [{term(), term()}]) -> [{term(), term()}].
interpret_defaults(DefaultsList, Env) ->
    lists:foldl(
        fun ({Name, Value}, Acc) ->
            case proplists:is_defined(Name, Env) of
                false ->
                    NewValue = mzbl_interpreter:eval_std(Value, Env),
                    [{Name, NewValue} | Acc];
                true -> Acc
            end
        end, [], DefaultsList).

-spec convert(string() | binary(), erlang) -> term();
             (string() | binary(), binary) -> binary();
             (string() | binary(), text) -> string();
             (string() | binary(), json) -> list() | map();
             (string() | binary(), tsv) -> [string()].
convert(X, binary) when is_binary(X) -> X;
convert(X, binary) -> list_to_binary(X);
convert(X, text) when is_binary(X) -> binary_to_list(X);
convert(X, text) -> X;
convert(X, erlang) ->
    S = case is_binary(X) of
        true -> binary_to_list(X);
        false -> X
    end,
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term;
convert(X, json) -> jiffy:decode(X, [return_maps]);
convert(X, tsv) ->
    S = case is_binary(X) of
        true -> binary_to_list(X);
        false -> X
    end,
    lists:map(fun(L) -> string:tokens(L, "\t") end, string:tokens(S, "\n"));
convert(X, T) -> erlang:error({invalid_conversion, T, X}).

-spec enumerate_pools([script_expr()]) -> [script_expr()].
enumerate_pools(Pools) ->
    {Pools2, _} = lists:mapfoldl(
        fun (#operation{name = pool} = Op, Number) ->
                {mzbl_ast:add_meta(Op, [{pool_name, "pool" ++ integer_to_list(Number)}, {pool_id, Number}]), Number + 1};
            (Op, Number) ->
                {Op, Number}
        end, 1, Pools),
    Pools2.

read_file(File) ->
    case file:read_file(File) of
        {ok, Content} -> erlang:binary_to_list(Content);
        {error, E} -> erlang:error({read_file_error, File, E})
    end.

-spec get_benchname(string()) -> string().
get_benchname(ScriptName) ->
    Name = filename:basename(ScriptName, ".erl"),
    re:replace(Name, "[^a-zA-Z0-9]", "_", [{return, list}, global]).

-spec extract_worker([operation()]) -> {worker_provider(), worker_name()}.
extract_worker(PoolOpts) ->
    WorkerType = mzbl_ast:find_operation_and_extract_args(worker_type, PoolOpts, [undefined]),
    resolve_worker_provider(WorkerType).

-spec resolve_worker_provider([atom()]) -> {worker_provider(), worker_name()}.
resolve_worker_provider(Worker) ->
    case Worker of
        [[WorkerName, Kind]] when is_atom(Kind) -> resolve_worker_provider([WorkerName, Kind]);
        [WorkerName] -> {mzb_erl_worker, WorkerName};
        [WorkerName, erlang] -> {mzb_erl_worker, WorkerName};
        [WorkerName, lua] -> {mzb_lua_worker, WorkerName};
        [WorkerName, python] -> {mzb_python_worker, WorkerName}
    end.

-spec hostname(atom()) -> string().
hostname(Node) ->
    [_, H] = string:tokens(erlang:atom_to_list(Node), "@"),
    H.

-spec extract_install_specs(abstract_expr(), [term()]) -> [install_spec()].
extract_install_specs(AST, Env) ->
    [Defaults] = mzbl_ast:find_operation_and_extract_args(defaults, AST, [[]]),
    Env2 = Env ++ interpret_defaults(Defaults, Env),
    Convert =
        fun(#operation{args = [Expr]}) ->
            Args = eval_opts(Expr, Env2),
            case mzbl_ast:find_operation_and_extract_args(git, Args, undefined) of
                undefined ->
                    case mzbl_ast:find_operation_and_extract_args(rsync, Args, undefined) of
                        undefined -> erlang:error({install_spec_error, missed_mandatory_option, git});
                        [Remote] ->
                            [Excludes] = mzbl_ast:find_operation_and_extract_args(excludes, Args, [[]]),
                            [Subdir] = mzbl_ast:find_operation_and_extract_args(dir, Args, [""]),
                            make_rsync_install_spec(Remote, Subdir, Excludes)
                    end;
                [Repo] ->
                    [Branch] = mzbl_ast:find_operation_and_extract_args(branch, Args, [""]),
                    [Subdir] = mzbl_ast:find_operation_and_extract_args(dir, Args, ["."]),
                    [Build] = mzbl_ast:find_operation_and_extract_args(build, Args, [""]),
                    make_git_install_spec(Repo, Branch, Subdir, Build)
            end
        end,
    [Convert(InstallOperation) || (#operation{name = make_install} = InstallOperation) <- AST].

-spec make_git_install_spec(string(), string(), string(), string()) -> git_install_spec().
make_git_install_spec(Repo, Branch, Dir, Build) ->
    #git_install_spec{
        repo = to_string(Repo),
        branch = to_string(Branch),
        dir = to_string(Dir),
        build = to_string(Build)}.

-spec make_rsync_install_spec(binary() | string(), binary() | string(), [binary() | string()]) -> rsync_install_spec().
make_rsync_install_spec(Remote, Subdir, Excludes) ->
    #rsync_install_spec{
        remote = to_string(Remote),
        dir = to_string(Subdir),
        excludes = [to_string(E) || E <- Excludes]}.

-spec to_string(string() | binary()) -> string().
to_string(X) when is_binary(X) -> binary_to_list(X);
to_string(X) when is_list(X) -> X;
to_string(Y) -> erlang:error({not_a_stringy_thing, Y}).

-spec normalize_env([term()]) -> [term()].
normalize_env(Env) ->
    lists:map(
        fun ({{resource, _} = K, V}) -> {K, V};
            ({asserts = K, V}) -> {K, V};
            ({K, V}) -> {normalize_env_(K), normalize_env_(V)}
        end, Env).

normalize_env_(V) when is_binary(V) -> erlang:binary_to_list(V);
normalize_env_(V) when is_list(V) -> V;
normalize_env_(V) when is_number(V) -> V;
normalize_env_(U) ->
    Msg = mzb_string:format("Env value of unknown type: ~p", [U]),
    erlang:error({error, {validation, [Msg]}}).

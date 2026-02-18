-module(purl_type_registry).

-feature(maybe_expr, enable).

-behaviour(gen_server).

-moduledoc false.

-include_lib("kernel/include/file.hrl").

-record(state, {
    ets_ref :: ets:table(),
    dets_ref :: dets:tab_name()
}).
-record(row, {type, filename = undefined, sync_time, specification}).

-export_type([start_opt/0, start_opts/0]).

-type start_opt() :: {name, module()} | {type_dir, file:filename()}.
-type start_opts() :: [start_opt()].

-type state() :: #state{}.
-type row() :: #row{
    type :: purl:type(),
    filename :: file:filename() | undefined,
    sync_time :: pos_integer(),
    specification :: purl:type_specification()
}.

%% API
-export([
    start_link/0,
    start_link/1,
    add/1,
    add/2,
    delete/1,
    delete/2,
    lookup/1,
    lookup/2,
    list_types/0,
    list_types/1,
    child_spec/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_NAME, ?MODULE).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link([]).

-spec start_link(Opts) -> gen_server:start_ret() when Opts :: start_opts().
start_link(Opts) ->
    Name = proplists:get_value(name, Opts, ?DEFAULT_NAME),
    InitOpts = #{
        name => Name,
        type_dir => proplists:get_value(
            type_dir, Opts, filename:join([code:priv_dir(purl), "spec", "types"])
        )
    },
    gen_server:start_link({local, Name}, ?MODULE, InitOpts, []).

-spec add(Specification :: purl:type_specification()) -> ok.
add(Specification) ->
    add(?DEFAULT_NAME, Specification).

-spec add(Name :: module(), Specification :: purl:type_specification()) -> ok.
add(Name, Specification) ->
    gen_server:call(Name, {add, Specification}).

-spec delete(Type :: purl:type()) -> ok.
delete(Type) ->
    delete(?DEFAULT_NAME, Type).

-spec delete(Name :: module(), Type :: purl:type()) -> ok.
delete(Name, Type) ->
    gen_server:call(Name, {delete, Type}).

-spec lookup(Type :: purl:type()) -> purl:type_specification() | undefined.
lookup(Type) ->
    lookup(?DEFAULT_NAME, Type).

-spec lookup(Name :: module(), Type :: purl:type()) -> purl:type_specification().
lookup(Name, Type) ->
    case ets:lookup(Name, Type) of
        [#row{type = Type, specification = Specification}] -> Specification;
        [] -> default_type_specification(Type)
    end.

-spec list_types() -> [purl:type()].
list_types() ->
    list_types(?DEFAULT_NAME).

-spec list_types(Name :: module()) -> [purl:type()].
list_types(Name) ->
    ets:select(Name, [{#row{type = '$1', _ = '_'}, [], ['$1']}]).

-spec child_spec(Opts :: start_opts()) -> supervisor:child_spec().
child_spec(Opts) ->
    Name = proplists:get_value(name, Opts, ?DEFAULT_NAME),
    #{
        id => Name,
        start => {?MODULE, start_link, [Opts]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    }.

init(#{name := Name, type_dir := TypeDir} = _Opts) ->
    DetsFile = get_dets_filename(Name),

    ok = ensure_dets_data_dir(),

    Table = ets:new(Name, [named_table, protected, set, {read_concurrency, true}, {keypos, 2}]),
    {ok, DetsRef} = dets:open_file(Name, [{type, set}, {file, DetsFile}, {keypos, 2}]),

    dets:to_ets(Name, Table),

    State = #state{ets_ref = Table, dets_ref = DetsRef},

    maybe_reload_from_disk(TypeDir, State),

    {ok, State}.

handle_call({add, Specification}, _From, State) ->
    DefaultedSpec = type_specification_set_defaults(Specification),
    Type = maps:get(type, DefaultedSpec),
    Timestamp = erlang:system_time(second),
    Row = #row{type = Type, specification = DefaultedSpec, sync_time = Timestamp},
    true = ets:insert(State#state.ets_ref, Row),
    ok = dets:insert(State#state.dets_ref, Row),
    ok = dets:sync(State#state.dets_ref),
    {reply, ok, State};
handle_call({delete, Type}, _From, State) ->
    true = ets:delete(State#state.ets_ref, Type),
    ok = dets:delete(State#state.dets_ref, Type),
    ok = dets:sync(State#state.dets_ref),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    true = ets:delete(State#state.ets_ref),
    ok = dets:close(State#state.dets_ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec get_dets_data_dir() -> file:filename().
get_dets_data_dir() ->
    filename:join([code:priv_dir(purl), "data"]).

-spec get_dets_filename(Name :: module()) -> file:filename().
get_dets_filename(Name) ->
    filename:join([get_dets_data_dir(), atom_to_list(Name) ++ ".dets"]).

-spec ensure_dets_data_dir() -> ok | {error, term()}.
ensure_dets_data_dir() ->
    DataDir = get_dets_data_dir(),
    case filelib:is_dir(DataDir) of
        true ->
            ok;
        false ->
            case file:make_dir(DataDir) of
                ok -> ok;
                {error, _Reason} -> {error, unable_to_create_data_dir}
            end
    end.

-spec maybe_reload_from_disk(TypeDir :: file:filename(), State :: state()) -> ok.
maybe_reload_from_disk(TypeDir, State) ->
    PathWildcard = filename:join([TypeDir, "*.json"]),
    Files = filelib:wildcard(PathWildcard),

    CachedFiles = ets:foldl(
        fun(#row{filename = Filename, sync_time = SyncTime}, Acc) ->
            maps:put(Filename, SyncTime, Acc)
        end,
        #{},
        State#state.ets_ref
    ),

    ChangedFiles = lists:filter(
        fun(File) ->
            case maps:get(File, CachedFiles, undefined) of
                undefined ->
                    true;
                CacheTime ->
                    {ok, #file_info{mtime = MTime}} = file:read_file_info(File, [{time, posix}]),
                    CacheTime < MTime
            end
        end,
        Files
    ),

    ok = reload_specification_from_disk(ChangedFiles, State).

-spec reload_specification_from_disk(Files :: [file:filename()], State :: state()) ->
    ok | {error, term()}.
reload_specification_from_disk([], State) ->
    ok = dets:sync(State#state.dets_ref),
    ok;
reload_specification_from_disk([File | Rest], State) ->
    case load_specification_row(File) of
        {ok, Row} ->
            ets:insert(State#state.ets_ref, Row),
            dets:insert(State#state.dets_ref, Row),
            reload_specification_from_disk(Rest, State);
        {error, Reason} ->
            {error, Reason}
    end.

-spec load_specification_row(File :: file:filename()) -> {ok, row()} | {error, term()}.
load_specification_row(File) ->
    maybe
        {ok, Data} ?= file:read_file(File),
        {Parsed, Result, <<>>} = json:decode(Data, ok, #{
            object_push => fun(Key, Value, Acc) -> [{binary_to_atom(Key), Value} | Acc] end
        }),
        case {Result, Parsed} of
            {ok, #{'$schema' := Schema} = Specification} when
                Schema =:=
                    <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>;
                Schema =:= <<"https://packageurl.org/schemas/purl-type.schema-1.0.json">>
            ->
                Type = maps:get(type, Specification),
                SyncTime = erlang:system_time(second),
                Row = #row{
                    type = Type,
                    specification = type_specification_set_defaults(Specification),
                    sync_time = SyncTime,
                    filename = File
                },
                {ok, Row};
            {ok, Parsed} ->
                {error, {invalid_specification, File, Parsed}};
            {{error, Reason}, _Data} ->
                {error, {json_parse, File, Reason}}
        end
    end.

-spec type_specification_set_defaults(Specification :: purl:type_specification()) ->
    purl:type_specification().
type_specification_set_defaults(#{type := Type} = Specification) ->
    Default = default_type_specification(Type),
    deep_merge(Default, Specification).

-spec deep_merge(Left :: map(), Right :: map()) -> map().
deep_merge(Left, Right) ->
    maps:fold(
        fun(Key, LeftVal, Acc) ->
            case maps:get(Key, Right, undefined) of
                undefined ->
                    maps:put(Key, LeftVal, Acc);
                RightVal when is_map(LeftVal), is_map(RightVal) ->
                    maps:put(Key, deep_merge(LeftVal, RightVal), Acc);
                ActualVal ->
                    maps:put(Key, ActualVal, Acc)
            end
        end,
        Right,
        Left
    ).

-spec default_type_specification(Type :: purl:type()) -> purl:type_specification().
default_type_specification(Type) when is_list(Type) ->
    default_type_specification(list_to_binary(Type));
default_type_specification(Type) ->
    DefaultComponentDefinition = #{case_sensitive => true, normalization_rules => []},
    #{
        '$schema' => <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>,
        '$id' => <<"unregistered://", Type/binary, ".json">>,
        type => Type,
        type_name => Type,
        description => <<"Package URL type definition for ", Type/binary>>,
        repository => #{use_repository => false},
        namespace_definition => maps:merge(
            #{requirement => <<"optional">>}, DefaultComponentDefinition
        ),
        name_definition => maps:merge(
            #{requirement => <<"required">>}, DefaultComponentDefinition
        ),
        version_definition => maps:merge(
            #{requirement => <<"optional">>}, DefaultComponentDefinition
        ),
        qualifiers_definition => [],
        subpath_definition => maps:merge(
            #{requirement => <<"optional">>}, DefaultComponentDefinition
        ),
        examples => [],
        reference_urls => []
    }.

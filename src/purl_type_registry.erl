-module(purl_type_registry).

-behaviour(gen_server).

-moduledoc false.

-record(state, {
    ets_ref :: ets:table()
}).
-record(row, {type, specification}).

-export_type([start_opt/0, start_opts/0]).

-type start_opt() :: {name, module()}.
-type start_opts() :: [start_opt()].

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
    InitOpts = #{name => Name},
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

init(#{name := Name} = _Opts) ->
    Table = ets:new(Name, [named_table, protected, set, {read_concurrency, true}, {keypos, 2}]),

    true = ets:insert(Table, [
        #row{
            type = maps:get(type, Specification),
            specification = type_specification_set_defaults(Specification)
        }
     || Specification <- purl_type_data:specifications()
    ]),

    {ok, #state{ets_ref = Table}}.

handle_call({add, Specification}, _From, State) ->
    DefaultedSpec = type_specification_set_defaults(Specification),
    Type = maps:get(type, DefaultedSpec),
    Row = #row{type = Type, specification = DefaultedSpec},
    true = ets:insert(State#state.ets_ref, Row),
    {reply, ok, State};
handle_call({delete, Type}, _From, State) ->
    true = ets:delete(State#state.ets_ref, Type),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    true = ets:delete(State#state.ets_ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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

-module(rebar3_purl_gen).

-moduledoc false.

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, purl_gen).

-define(TARGET, "src/purl_type_data.erl").
-define(SPEC_DIR, "priv/spec").
-define(TYPE_DIR, "priv/spec/types").

-define(SUPPORTED_SCHEMAS, [
    <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>,
    <<"https://packageurl.org/schemas/purl-type.schema-1.0.json">>
]).

-spec init(State :: term()) -> {ok, term()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, []},
        {example, "rebar3 purl_gen"},
        {opts, [
            {check, $c, "check", boolean,
                "Verify the committed module is in sync instead of writing it."}
        ]},
        {short_desc, "Generate src/purl_type_data.erl from the purl-spec submodule."},
        {desc,
            "Generates the committed purl_type_data module from the purl type\n"
            "definitions in priv/spec/types/*.json.\n"
            "\n"
            "Run this after updating the priv/spec submodule and commit the result.\n"
            "Use --check to verify the committed module is up to date."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(State :: term()) -> {ok, term()} | {error, string()}.
do(State) ->
    {Args, _Rest} = rebar_state:command_parsed_args(State),
    Check = proplists:get_value(check, Args, false),

    case generate() of
        {ok, Generated} when Check ->
            check(Generated, State);
        {ok, Generated} ->
            write(Generated, State);
        {error, Reason} ->
            {error, lists:flatten(format_error(Reason))}
    end.

-spec format_error(Reason :: term()) -> iolist().
format_error(no_type_definitions) ->
    io_lib:format(
        "No type definitions found in ~ts.~n"
        "The purl-spec submodule is most likely not checked out. Run:~n"
        "~n"
        "    git submodule update --init",
        [?TYPE_DIR]
    );
format_error(out_of_date) ->
    io_lib:format(
        "~ts is out of date.~n"
        "~n"
        "Regenerate it by running:~n"
        "~n"
        "    rebar3 purl_gen~n"
        "~n"
        "and commit the result.",
        [?TARGET]
    );
format_error({unsupported_schema, File, Schema}) ->
    io_lib:format("Unsupported $schema in ~ts: ~ts", [File, Schema]);
format_error({missing_schema, File}) ->
    io_lib:format("Missing $schema in ~ts", [File]);
format_error({invalid_json, File, Reason}) ->
    io_lib:format("Invalid JSON in ~ts: ~p", [File, Reason]);
format_error({file_error, File, Reason}) ->
    io_lib:format("Unable to access ~ts: ~p", [File, Reason]);
format_error({format_failed, Error}) ->
    io_lib:format("Unable to format generated source: ~p", [Error]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec check(Generated :: binary(), State :: term()) -> {ok, term()} | {error, string()}.
check(Generated, State) ->
    case file:read_file(?TARGET) of
        {ok, Generated} ->
            rebar_api:info("~ts is up to date.", [?TARGET]),
            {ok, State};
        {ok, _Different} ->
            {error, lists:flatten(format_error(out_of_date))};
        {error, Reason} ->
            {error, lists:flatten(format_error({file_error, ?TARGET, Reason}))}
    end.

-spec write(Generated :: binary(), State :: term()) -> {ok, term()} | {error, string()}.
write(Generated, State) ->
    case file:write_file(?TARGET, Generated) of
        ok ->
            rebar_api:info("Wrote ~ts.", [?TARGET]),
            {ok, State};
        {error, Reason} ->
            {error, lists:flatten(format_error({file_error, ?TARGET, Reason}))}
    end.

-spec generate() -> {ok, binary()} | {error, term()}.
generate() ->
    case lists:sort(filelib:wildcard(filename:join([?TYPE_DIR, "*.json"]))) of
        [] ->
            {error, no_type_definitions};
        Files ->
            try
                format(render(spec_revision(), [load_specification(File) || File <- Files]))
            catch
                throw:Reason -> {error, Reason}
            end
    end.

%% Records which purl-spec revision the module was generated from, so the
%% generated file states its own provenance.
-spec spec_revision() -> string().
spec_revision() ->
    case rebar_utils:sh("git rev-parse HEAD", [{cd, ?SPEC_DIR}, return_on_error, {use_stdout, false}]) of
        {ok, Output} -> string:trim(Output);
        {error, _Reason} -> "unknown"
    end.

%% Runs the rendered source through `erlfmt`, which rebar3 has already loaded as
%% a project plugin. This keeps the generated file compliant with
%% `rebar3 fmt --check` and makes `--check` a plain byte comparison.
-spec format(Source :: iodata()) -> {ok, binary()} | {error, term()}.
format(Source) ->
    case erlfmt:format_string(unicode:characters_to_list(Source), []) of
        {ok, Formatted, _Warnings} ->
            {ok, unicode:characters_to_binary(Formatted)};
        {error, Error} ->
            {error, {format_failed, Error}}
    end.

-spec load_specification(File :: file:filename()) -> map().
load_specification(File) ->
    Data =
        case file:read_file(File) of
            {ok, Contents} -> Contents;
            {error, ReadReason} -> throw({file_error, File, ReadReason})
        end,

    {Parsed, Result, <<>>} = json:decode(Data, ok, #{
        object_push => fun(Key, Value, Acc) -> [{binary_to_atom(Key), Value} | Acc] end
    }),

    case {Result, Parsed} of
        {ok, #{'$schema' := Schema} = Specification} ->
            case lists:member(Schema, ?SUPPORTED_SCHEMAS) of
                true -> Specification;
                false -> throw({unsupported_schema, File, Schema})
            end;
        {ok, _Other} ->
            throw({missing_schema, File});
        {{error, Reason}, _Data} ->
            throw({invalid_json, File, Reason})
    end.

-spec render(Revision :: string(), Specifications :: [map()]) -> iodata().
render(Revision, Specifications) ->
    Entries = [["        ", io_lib:format("~kp", [Spec])] || Spec <- Specifications],

    [
        "%% @private\n"
        "%% This file is generated by `rebar3 purl_gen` from the purl-spec type\n"
        "%% definitions in `priv/spec/types/*.json`.\n"
        "%%\n"
        "%% Do not edit it by hand. Run `rebar3 purl_gen` after updating the\n"
        "%% `priv/spec` submodule and commit the result.\n"
        "%%\n",
        io_lib:format("%% purl-spec revision: ~ts~n", [Revision]),
        "-module(purl_type_data).\n"
        "\n"
        "-moduledoc false.\n"
        "\n"
        "-export([specifications/0]).\n"
        "\n"
        "-spec specifications() -> [purl:type_specification()].\n"
        "specifications() ->\n"
        "    [\n",
        lists:join(",\n", Entries),
        "\n"
        "    ].\n"
    ].

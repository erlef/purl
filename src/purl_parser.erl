-module(purl_parser).

-feature(maybe_expr, enable).

-include("purl.hrl").

-moduledoc false.

-export([parse/2]).

-spec parse(Purl, Options) -> {ok, purl:t()} | {error, purl:parse_error()} when
    Purl :: uri_string:uri_string() | uri_string:uri_map() | purl:t(),
    Options :: purl:parse_options().
parse(Purl, Options) when is_binary(Purl) or is_list(Purl) ->
    maybe
        #{} = UriMap ?= uri_string:parse(Purl),
        parse(UriMap, Options)
    end;
parse(#{} = UriMap, _Options) when not (is_map_key(scheme, UriMap)) ->
    {error, {invalid_scheme, undefined}};
parse(#{scheme := Scheme} = UriMap, Options) when Scheme =/= <<"pkg">> ->
    case string:lowercase(Scheme) of
        <<"pkg">> -> parse(maps:put(scheme, <<"pkg">>, UriMap), Options);
        _ -> {error, {invalid_scheme, Scheme}}
    end;
parse(#{scheme := <<"pkg">>, host := Type, path := Path} = UriMap, Options) ->
    parse(
        maps:remove(
            host,
            maps:put(
                path,
                <<Type/binary, "/", Path/binary>>,
                UriMap
            )
        ),
        Options
    );
parse(#{scheme := <<"pkg">>, path := Path} = UriMap, Options) ->
    Strict = maps:get(strict, Options, false),
    maybe
        {ok, {Type, Namespace, Name, Version}} ?= parse_path(Path),
        {ok, Qualifiers} ?= parse_query(maps:get(query, UriMap, undefined), Strict),
        Subpath = parse_fragment(maps:get(fragment, UriMap, undefined)),
        Purl = #purl{
            type = Type,
            namespace = Namespace,
            name = Name,
            version = Version,
            qualifiers = Qualifiers,
            subpath = Subpath
        },
        parse(Purl, Options)
    end;
parse(#purl{} = Purl, Options) ->
    Strict = maps:get(strict, Options, false),
    maybe
        {ok, Type} ?= parse_type(Purl#purl.type),
        {ok, Namespace} ?= parse_namespace(Purl#purl.namespace),
        {ok, Name} ?= parse_name(Purl#purl.name),
        {ok, Version} ?= parse_version(Purl#purl.version),
        {ok, Qualifiers} ?= parse_qualifiers(Purl#purl.qualifiers, Strict),
        {ok, Subpath} ?= parse_subpath(Purl#purl.subpath),
        {ok, #purl{
            type = Type,
            namespace = Namespace,
            name = Name,
            version = Version,
            qualifiers = Qualifiers,
            subpath = Subpath
        }}
    end.

-spec parse_path(Path) -> {ok, {Type, Namespace, Name, Version}} | purl:parse_error() when
    Path :: unicode:chardata(),
    Type :: unicode:chardata(),
    Namespace :: [unicode:chardata()],
    Name :: unicode:chardata(),
    Version :: unicode:chardata() | undefined.
parse_path(Path) ->
    case binary:split(Path, <<"/">>, [trim_all, global]) of
        [_One] ->
            {error, {invalid_field, name, <<"">>}};
        [RawType | [_NameOrNamespace | _Rest] = Rest] ->
            Type = string:lowercase(RawType),

            {RawNamespace, [NameWithVersion]} = lists:split(length(Rest) - 1, Rest),

            Namespace = lists:map(fun uri_string:percent_decode/1, RawNamespace),

            %% The version is separated by the last unencoded "@", so that names
            %% which themselves contain an "@" (such as the brew formula
            %% "node@20") are preserved.
            {Name, Version} =
                case binary:matches(NameWithVersion, <<"@">>) of
                    [] ->
                        {uri_string:percent_decode(NameWithVersion), undefined};
                    Matches ->
                        {Start, Length} = lists:last(Matches),
                        RawName = binary:part(NameWithVersion, 0, Start),
                        RawVersion = binary:part(
                            NameWithVersion,
                            Start + Length,
                            byte_size(NameWithVersion) - Start - Length
                        ),
                        {uri_string:percent_decode(RawName), uri_string:percent_decode(RawVersion)}
                end,

            {ok, {Type, Namespace, Name, Version}}
    end.

-spec parse_query(Query, Strict) ->
    {ok, #{unicode:chardata() => unicode:chardata()}} | purl:parse_error()
when
    Query :: unicode:chardata() | undefined,
    Strict :: boolean().
parse_query(undefined, _Strict) ->
    {ok, #{}};
parse_query(Query, Strict) ->
    Parts = uri_string:dissect_query(Query),
    lists:foldl(
        fun
            ({RawKey, Value}, {ok, Acc}) ->
                Key =
                    case Strict of
                        true -> RawKey;
                        false -> string:lowercase(RawKey)
                    end,
                case Acc of
                    #{Key := _} -> {error, {duplicate_qualifier, Key}};
                    _ -> {ok, maps:put(Key, Value, Acc)}
                end;
            (_, {error, Reason}) ->
                {error, Reason}
        end,
        {ok, #{}},
        Parts
    ).

-spec parse_fragment(Fragment) -> [unicode:chardata()] when
    Fragment :: unicode:chardata() | undefined.
parse_fragment(undefined) ->
    [];
parse_fragment(Fragment) ->
    Decoded = uri_string:percent_decode(Fragment),
    Trimmed = string:trim(Decoded, both, "/"),
    binary:split(Trimmed, <<"/">>, [trim_all, global]).

-spec parse_type(RawType) -> {ok, Type} | purl:parse_error() when
    RawType :: unicode:chardata(),
    Type :: purl:type().
parse_type(Type) ->
    case re:run(Type, "^[a-zA-Z\\+\\.\\-][a-zA-Z0-9\\+\\.\\-]*$") of
        {match, _} ->
            {ok, Type};
        nomatch ->
            {error, {invalid_field, type, Type}}
    end.

-spec parse_namespace(RawNamespace) -> {ok, Namespace} | purl:parse_error() when
    RawNamespace :: [unicode:chardata()],
    Namespace :: purl:namespace().
parse_namespace(RawNamespace) ->
    Result = lists:foldl(
        fun
            (Segment, {ok, Acc}) ->
                case parse_namespace_segment(Segment) of
                    {ok, Segment} -> {ok, [Segment | Acc]};
                    skip -> {ok, Acc};
                    {error, Reason} -> {error, Reason}
                end;
            (_, {error, Reason}) ->
                {error, Reason}
        end,
        {ok, []},
        RawNamespace
    ),
    case Result of
        {ok, Namespace} -> {ok, lists:reverse(Namespace)};
        {error, Reason} -> {error, Reason}
    end.

-spec parse_namespace_segment(RawSegment) ->
    {ok, Segment} | skip | {error, {invalid_field, namespace, Segment}}
when
    RawSegment :: unicode:chardata(),
    Segment :: purl:namespace_segment().
parse_namespace_segment(<<"">>) ->
    skip;
parse_namespace_segment(Segment) ->
    case valid_utf8(Segment) of
        false ->
            {error, {invalid_field, namespace, Segment}};
        true ->
            case binary:match(Segment, <<"/">>) of
                {_, _} -> {error, {invalid_field, namespace, Segment}};
                nomatch -> {ok, Segment}
            end
    end.

-spec parse_name(RawName) -> {ok, Name} | purl:parse_error() when
    RawName :: unicode:chardata(),
    Name :: purl:name().
parse_name(<<"">>) ->
    {error, {invalid_field, name, <<"">>}};
parse_name(Name) ->
    case valid_utf8(Name) of
        false -> {error, {invalid_field, name, Name}};
        true -> {ok, Name}
    end.

-spec parse_version(RawVersion) ->
    {ok, Version} | purl:parse_error()
when
    RawVersion :: unicode:chardata(),
    Version :: purl:version().
parse_version(undefined) ->
    {ok, undefined};
parse_version(Version) ->
    case valid_utf8(Version) of
        false -> {error, {invalid_field, version, Version}};
        true -> {ok, Version}
    end.

-spec parse_qualifiers(RawQualifiers, Strict) ->
    {ok, Qualifiers} | purl:parse_error()
when
    RawQualifiers :: #{unicode:chardata() => unicode:chardata()},
    Strict :: boolean(),
    Qualifiers :: purl:qualifiers().
parse_qualifiers(RawQualifiers, Strict) ->
    Result = maps:fold(
        fun
            (RawQualifierKey, RawQualifierValue, {ok, Acc}) ->
                maybe
                    {ok, QualifierKey} ?= parse_qualifier_key(RawQualifierKey, Strict),
                    {ok, QualifierValue} ?= parse_qualifier_value(RawQualifierValue),
                    {ok, maps:put(QualifierKey, QualifierValue, Acc)}
                else
                    skip -> {ok, Acc};
                    {error, Reason} -> {error, Reason}
                end;
            (_, _, {error, Reason}) ->
                {error, Reason}
        end,
        {ok, #{}},
        RawQualifiers
    ),
    case Result of
        {ok, Qualifiers} -> {ok, Qualifiers};
        {error, Reason} -> {error, Reason}
    end.

-spec parse_qualifier_key(RawQualifierKey, Strict) ->
    {ok, QualifierKey} | {error, {invalid_field, qualifiers, QualifierKey}}
when
    Strict :: boolean(),
    RawQualifierKey :: unicode:chardata(),
    QualifierKey :: purl:qualifier_key().
parse_qualifier_key(<<"">>, _Strict) ->
    {error, {invalid_field, qualifiers, <<"">>}};
parse_qualifier_key(QualifierKey, true) ->
    case re:run(QualifierKey, "^[a-z\\.\\-\\_][a-z0-9\\.\\-\\_]*$") of
        {match, _} ->
            {ok, QualifierKey};
        nomatch ->
            {error, {invalid_field, qualifiers, QualifierKey}}
    end;
parse_qualifier_key(QualifierKey, false) ->
    case re:run(QualifierKey, "^[a-zA-Z\\.\\-\\_][a-zA-Z0-9\\.\\-\\_]*$") of
        {match, _} ->
            {ok, QualifierKey};
        nomatch ->
            {error, {invalid_field, qualifiers, QualifierKey}}
    end.

-spec parse_qualifier_value(RawQualifierValue) ->
    {ok, QualifierValue} | skip | {error, {invalid_field, qualifiers, QualifierValue}}
when
    RawQualifierValue :: unicode:chardata(),
    QualifierValue :: purl:qualifier_value().
parse_qualifier_value(<<"">>) ->
    skip;
parse_qualifier_value(QualifierValue) ->
    case valid_utf8(QualifierValue) of
        false -> {error, {invalid_field, qualifiers, QualifierValue}};
        true -> {ok, QualifierValue}
    end.

-spec parse_subpath(RawSubpath) -> {ok, Subpath} | purl:parse_error() when
    RawSubpath :: [unicode:chardata()],
    Subpath :: purl:subpath().
parse_subpath(RawSubpath) ->
    Result = lists:foldl(
        fun
            (Segment, {ok, Acc}) ->
                case parse_subpath_segment(Segment) of
                    {ok, Segment} -> {ok, [Segment | Acc]};
                    skip -> {ok, Acc};
                    {error, Reason} -> {error, Reason}
                end;
            (_, {error, Reason}) ->
                {error, Reason}
        end,
        {ok, []},
        RawSubpath
    ),
    case Result of
        {ok, Subpath} -> {ok, lists:reverse(Subpath)};
        {error, Reason} -> {error, Reason}
    end.

-spec parse_subpath_segment(RawSegment) ->
    {ok, Segment} | skip | {error, {invalid_field, subpath, Segment}}
when
    RawSegment :: unicode:chardata(),
    Segment :: purl:subpath_segment().
parse_subpath_segment(<<"">>) ->
    skip;
parse_subpath_segment(<<".">>) ->
    skip;
parse_subpath_segment(<<"..">>) ->
    skip;
parse_subpath_segment(Segment) ->
    case valid_utf8(Segment) of
        false -> {error, {invalid_field, subpath, Segment}};
        true -> {ok, Segment}
    end.

-spec valid_utf8(Binary) -> boolean() when
    Binary :: binary().
valid_utf8(<<>>) -> true;
valid_utf8(<<_H/utf8, T/binary>>) -> valid_utf8(T);
valid_utf8(_Binary) -> false.

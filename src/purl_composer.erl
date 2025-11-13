-module(purl_composer).

-feature(maybe_expr, enable).

-include("purl.hrl").

-moduledoc false.

-export([compose_uri/1]).

-spec compose_uri(Purl) -> uri_string:uri_map() when Purl :: purl:t().
compose_uri(#purl{} = Purl) ->
    maps:filter(fun(_Key, Value) -> Value =/= undefined end, #{
        fragment => encode_subpath(Purl#purl.subpath),
        path => encode_uri_path(Purl),
        query => encode_qualifiers(Purl#purl.qualifiers),
        scheme => <<"pkg">>
    }).

-spec encode_uri_path(Purl) -> unicode:chardata() when Purl :: purl:t().
encode_uri_path(#purl{version = undefined, type = Type, name = Name, namespace = Namespace}) ->
    Segments = [Type | encode_namespace(Namespace)] ++ [encode_name(Name)],
    Path = lists:join("/", Segments),
    iolist_to_binary(Path);
encode_uri_path(#purl{version = Version} = Purl) ->
    Path = [
        encode_uri_path(Purl#purl{version = undefined}),
        "@",
        encode_version(Version)
    ],
    iolist_to_binary(Path).

-spec encode_namespace(Namespace) -> [unicode:chardata()] when Namespace :: purl:namespace().
encode_namespace(Namespace) -> lists:map(fun encode/1, Namespace).

-spec encode_name(Name) -> unicode:chardata() when Name :: purl:name().
encode_name(Name) -> encode(Name, "/").

-spec encode_qualifiers(Qualifiers) -> unicode:chardata() | undefined when
    Qualifiers :: purl:qualifiers().
encode_qualifiers(Qualifiers) when Qualifiers =:= #{} ->
    undefined;
encode_qualifiers(Qualifiers) ->
    Encoded = lists:map(
        fun({Key, Value}) ->
            [Key, "=", encode(Value, "&+/,")]
        end,
        maps:to_list(Qualifiers)
    ),
    Joined = lists:join("&", Encoded),
    iolist_to_binary(Joined).

-spec encode_version(Version) -> unicode:chardata() when Version :: unicode:chardata().
encode_version(Version) -> encode(Version, "/").

-spec encode_subpath(Subpath) -> unicode:chardata() | undefined when Subpath :: purl:subpath().
encode_subpath([]) ->
    undefined;
encode_subpath(Subpath) ->
    Encoded = lists:map(fun encode/1, Subpath),
    Joined = lists:join("/", Encoded),
    iolist_to_binary(Joined).

-spec encode(Data) -> unicode:chardata() when Data :: unicode:chardata().
encode(Data) -> encode(Data, []).

-spec encode(Data, AdditionalCodepoints) -> unicode:chardata() when
    Data :: unicode:chardata(), AdditionalCodepoints :: unicode:charlist().
encode(Data, AdditionalCodepoints) ->
    AllowedCharacters = uri_string:allowed_characters(),
    {reserved, Reserved} = lists:keyfind(reserved, 1, AllowedCharacters),
    {unreserved, Unreserved} = lists:keyfind(unreserved, 1, AllowedCharacters),
    PurlUnsafe = "#?@[]+",
    UnescapedCharacters = ((Reserved ++ Unreserved) -- PurlUnsafe) -- AdditionalCodepoints,
    uri_string:quote(Data, UnescapedCharacters).

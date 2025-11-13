-module(purl_type_normalizer).

-feature(maybe_expr, enable).

-moduledoc false.

-include("purl.hrl").

-export([normalize/2]).

-spec normalize(Purl :: purl:t(), Specification :: purl:type_specification()) -> purl:t().
normalize(Purl, Specification) ->
    Purl1 = normalize_namespace(Purl, Specification),
    Purl2 = normalize_name(Purl1, Specification),
    Purl3 = normalize_version(Purl2, Specification),
    Purl4 = normalize_subpath(Purl3, Specification),
    normalize_qualifiers(Purl4, Specification).

-spec normalize_namespace(Purl, Specification :: purl:type_specification()) -> Purl when
    Purl :: purl:t().
normalize_namespace(#purl{namespace = Namespace} = Purl, #{
    namespace_definition := NamespaceDefinition
}) ->
    Purl#purl{
        namespace = lists:map(
            fun(Part) -> normalize_component(Part, NamespaceDefinition) end, Namespace
        )
    }.

-spec normalize_name(Purl, Specification :: purl:type_specification()) -> Purl when
    Purl :: purl:t().
normalize_name(#purl{name = Name} = Purl, #{name_definition := NameDefinition}) ->
    Purl#purl{name = normalize_component(Name, NameDefinition)}.

-spec normalize_version(Purl, Specification :: purl:type_specification()) -> Purl when
    Purl :: purl:t().
normalize_version(#purl{version = Version} = Purl, #{version_definition := VersionDefinition}) ->
    Purl#purl{version = normalize_component(Version, VersionDefinition)}.

-spec normalize_subpath(Purl, Specification :: purl:type_specification()) -> Purl when
    Purl :: purl:t().
normalize_subpath(#purl{subpath = Subpath} = Purl, #{subpath_definition := SubpathDefinition}) ->
    Purl#purl{
        subpath = lists:map(
            fun(Part) -> normalize_component(Part, SubpathDefinition) end, Subpath
        )
    }.

-spec normalize_component(
    Part :: binary(), ComponentDefinition :: purl:type_component_definition()
) -> binary().
normalize_component(Part, ComponentDefinition) ->
    CaseNormalizedPart =
        case maps:get(case_sensitive, ComponentDefinition) of
            true -> Part;
            false -> string:lowercase(Part)
        end,
    lists:foldl(
        fun(Rule, Acc) ->
            apply_normalization_rule(Rule, Acc)
        end,
        CaseNormalizedPart,
        maps:get(normalization_rules, ComponentDefinition)
    ).

-spec apply_normalization_rule(Rule :: binary(), Part :: binary()) -> binary().
apply_normalization_rule(<<"Replace underscore _ with dash -">>, Part) ->
    iolist_to_binary(string:replace(Part, <<"_">>, <<"-">>));
apply_normalization_rule(
    <<"Replace dot . with underscore _ when used in distribution (sdist, wheel) names">>,
    Part
) ->
    %% FIXME: No clear definition yet
    Part;
apply_normalization_rule(
    <<"Replace non-[a-z] letters, non-[0-9] digits with underscore _">>, Part
) ->
    iolist_to_binary(string:replace(Part, <<"[^\$a-zA-Z0-9]">>, <<"_">>));
apply_normalization_rule(<<"Apply kebab-case">>, Part) ->
    iolist_to_binary(string:replace(Part, <<"_">>, <<"-">>));
apply_normalization_rule(<<"It is not case sensitive and must be lowercased.">>, Part) ->
    %% Already handled in normalize_component
    Part;
apply_normalization_rule(_Rule, Part) ->
    %% Skip unknown rules
    Part.

-spec normalize_qualifiers(Purl, Specification :: purl:type_specification()) -> Purl when
    Purl :: purl:t().
normalize_qualifiers(#purl{qualifiers = Qualifiers} = Purl, Specification) ->
    NormalizedQualifiers = maps:filter(
        fun(Key, Value) ->
            not is_default_qualifier_value(Key, Value, Specification)
        end,
        Qualifiers
    ),
    Purl#purl{qualifiers = NormalizedQualifiers}.

-spec is_default_qualifier_value(
    Key :: purl:qualifier_key(),
    Value :: purl:qualifier_value(),
    Specification :: purl:type_specification()
) -> boolean().
is_default_qualifier_value(
    Key, Value, #{qualifiers_definition := QualifiersDefinition} = Specification
) ->
    case lists:search(fun(#{key := DefKey}) -> DefKey =:= Key end, QualifiersDefinition) of
        {value, QualifierDef} ->
            DefaultValue = get_default_value_for_qualifier(Key, QualifierDef, Specification),
            case DefaultValue of
                undefined -> false;
                _ -> values_match_after_normalization(Value, DefaultValue)
            end;
        _ ->
            false
    end.

-spec get_default_value_for_qualifier(
    Key :: purl:qualifier_key(),
    QualifierDef :: map(),
    Specification :: purl:type_specification()
) -> purl:qualifier_value() | undefined.
get_default_value_for_qualifier(<<"repository_url">>, _QualifierDef, #{
    repository := #{default_repository_url := DefaultRepoUrl}
}) ->
    DefaultRepoUrl;
get_default_value_for_qualifier(_Key, #{default_value := DefaultValue}, _Specification) ->
    DefaultValue;
get_default_value_for_qualifier(_Key, _QualifierDef, _Specification) ->
    undefined.

-spec values_match_after_normalization(Value1 :: binary(), Value2 :: binary()) -> boolean().
values_match_after_normalization(Value1, Value2) ->
    normalize_url_for_comparison(Value1) =:= normalize_url_for_comparison(Value2).

-spec normalize_url_for_comparison(Value :: binary()) -> binary().
normalize_url_for_comparison(Value) when byte_size(Value) =:= 0 ->
    Value;
normalize_url_for_comparison(Value) ->
    %% Remove trailing slash for URL comparison only
    case binary:last(Value) of
        $/ -> binary:part(Value, 0, byte_size(Value) - 1);
        _ -> Value
    end.

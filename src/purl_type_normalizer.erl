-module(purl_type_normalizer).

-feature(maybe_expr, enable).

-moduledoc false.

-include("purl.hrl").

-export([normalize/2, default_qualifiers/1]).

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
normalize_component(undefined, _ComponentDefinition) ->
    undefined;
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
    Purl#purl{qualifiers = maps:merge(default_qualifiers(Specification), Qualifiers)}.

-doc false.
-spec default_qualifiers(Specification :: purl:type_specification()) -> purl:qualifiers().
default_qualifiers(#{qualifiers_definition := QualifiersDefinition} = Specification) ->
    lists:foldl(
        fun(QualifierDef, Acc) ->
            Key = maps:get(key, QualifierDef),
            case get_default_value_for_qualifier(Key, QualifierDef, Specification) of
                undefined -> Acc;
                DefaultValue -> maps:put(Key, DefaultValue, Acc)
            end
        end,
        #{},
        QualifiersDefinition
    ).

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

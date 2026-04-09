-module(purl_type_validator).

-feature(maybe_expr, enable).

-moduledoc false.

-include("purl.hrl").

-export([validate/2]).

-spec validate(Purl :: purl:t(), TypeSpecification :: purl:type_specification()) ->
    ok | purl:parse_error().
validate(Purl, TypeSpecification) ->
    maybe
        ok ?= validate_namespace(Purl, TypeSpecification),
        ok ?= validate_name(Purl, TypeSpecification),
        ok ?= validate_version(Purl, TypeSpecification),
        ok ?= validate_qualifiers(Purl, TypeSpecification),
        ok
    end.

-spec validate_namespace(Purl :: purl:t(), TypeSpecification :: purl:type_specification()) ->
    ok | purl:parse_error().
validate_namespace(#purl{namespace = []}, #{
    type := Type, namespace_definition := #{requirement := <<"required">>}
}) ->
    {error, {type_validation, Type, namespace, <<"">>, <<"Namespace is required but missing.">>}};
validate_namespace(#purl{namespace = [_ | _]}, #{
    type := Type, namespace_definition := #{requirement := <<"prohibited">>}
}) ->
    {error, {type_validation, Type, namespace, <<"">>, <<"Namespace is not allowed but found.">>}};
validate_namespace(#purl{namespace = Namespace}, #{
    type := Type, namespace_definition := NamespaceDefinition
}) ->
    validate_components(Namespace, NamespaceDefinition, Type, namespace).

-spec validate_name(Purl :: purl:t(), TypeSpecification :: purl:type_specification()) ->
    ok | purl:parse_error().
validate_name(#purl{name = Name}, #{type := Type, name_definition := NameDefinition}) ->
    validate_component(Name, NameDefinition, Type, name).

-spec validate_version(Purl :: purl:t(), TypeSpecification :: purl:type_specification()) ->
    ok | purl:parse_error().
validate_version(#purl{version = undefined}, _TypeSpecification) ->
    ok;
validate_version(#purl{version = Version}, #{type := Type, version_definition := VersionDefinition}) ->
    validate_component(Version, VersionDefinition, Type, version);
validate_version(_Purl, _TypeSpecification) ->
    ok.

-spec validate_components(
    Parts :: [binary()],
    ComponentDefinition :: purl:type_component_definition(),
    Type :: purl:type(),
    ComponentName :: atom()
) -> ok | purl:parse_error().
validate_components([], _ComponentDefinition, _Type, _ComponentName) ->
    ok;
validate_components([Part | Rest], ComponentDefinition, Type, ComponentName) ->
    maybe
        ok ?= validate_component(Part, ComponentDefinition, Type, ComponentName),
        validate_components(Rest, ComponentDefinition, Type, ComponentName)
    end.

-spec validate_component(
    Part :: binary(),
    ComponentDefinition :: purl:type_component_definition(),
    Type :: purl:type(),
    ComponentName :: atom()
) -> ok | purl:parse_error().
validate_component(Part, ComponentDefinition, Type, ComponentName) ->
    maybe
        ok ?=
            validate_component_permitted_characters(Part, ComponentDefinition, Type, ComponentName),
        ok
    end.

-spec validate_component_permitted_characters(
    Part :: binary(),
    ComponentDefinition :: purl:type_component_definition(),
    Type :: purl:type(),
    ComponentName :: atom()
) -> ok | purl:parse_error().
validate_component_permitted_characters(
    Part, #{permitted_characters := PermittedCharacters}, Type, ComponentName
) ->
    case re:run(Part, PermittedCharacters) of
        {match, _} ->
            ok;
        nomatch ->
            {error,
                {type_validation, Type, ComponentName, Part,
                    <<"Component contains invalid characters.">>}}
    end;
validate_component_permitted_characters(_Part, _ComponentDefinition, _Type, _ComponentName) ->
    ok.

-spec validate_qualifiers(Purl :: purl:t(), TypeSpecification :: purl:type_specification()) ->
    ok | purl:parse_error().
validate_qualifiers(Purl, #{qualifiers_definition := QualifiersDefinition, type := Type}) ->
    validate_required_qualifiers(Purl, QualifiersDefinition, Type);
validate_qualifiers(_Purl, _TypeSpecification) ->
    ok.

-spec validate_required_qualifiers(
    Purl :: purl:t(),
    QualifiersDefinition :: [#{key := purl:qualifier_key(), requirement => binary()}],
    Type :: purl:type()
) -> ok | purl:parse_error().
validate_required_qualifiers(#purl{qualifiers = Qualifiers}, QualifiersDefinition, Type) ->
    RequiredQualifiers = [
        Key
     || #{key := Key, requirement := <<"required">>} <- QualifiersDefinition
    ],
    validate_required_qualifiers_present(RequiredQualifiers, Qualifiers, Type).

-spec validate_required_qualifiers_present(
    RequiredKeys :: [purl:qualifier_key()],
    Qualifiers :: purl:qualifiers(),
    Type :: purl:type()
) -> ok | purl:parse_error().
validate_required_qualifiers_present([], _Qualifiers, _Type) ->
    ok;
validate_required_qualifiers_present([Key | Rest], Qualifiers, Type) ->
    case maps:is_key(Key, Qualifiers) of
        true ->
            validate_required_qualifiers_present(Rest, Qualifiers, Type);
        false ->
            {error,
                {type_validation, Type, {qualifiers, Key}, <<"">>,
                    <<"Required qualifier is missing.">>}}
    end.

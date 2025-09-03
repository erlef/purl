-module(purl_type_validator).

-feature(maybe_expr, enable).

-include("internal/doc.hrl").
?MODULEDOC(false).

-include("purl.hrl").

-export([validate/2]).

-spec validate(Purl :: purl:t(), TypeSpecification :: purl:type_specification()) ->
    ok | purl:parse_error().
validate(Purl, TypeSpecification) ->
    maybe
        ok ?= validate_namespace(Purl, TypeSpecification),
        ok
    end.

-spec validate_namespace(Purl :: purl:t(), TypeSpecification :: purl:type_specification()) ->
    ok | purl:parse_error().
validate_namespace(#purl{namespace = []}, #{
    type := Type, namespace_definition := #{requirement := <<"required">>}
}) ->
    {error, {type_validation, Type, namespace, <<"">>, <<"Namespace is required but missing.">>}};
validate_namespace(#purl{namespace = [_ | _]}, #{
    ype := Type, namespace_definition := #{requirement := <<"prohibited">>}
}) ->
    {error, {type_validation, Type, namespace, <<"">>, <<"Namespace is not allowed but found.">>}};
validate_namespace(#purl{namespace = Namespace}, #{
    type := Type, namespace_definition := NamespaceDefinition
}) ->
    lists:foreach(
        fun(Part) -> validate_component(Part, NamespaceDefinition, Type) end, Namespace
    ),
    ok.

-spec validate_component(
    Part :: binary(),
    ComponentDefinition :: purl:type_component_definition(),
    Type :: purl:type()
) -> ok | purl:parse_error().
validate_component(Part, ComponentDefinition, Type) ->
    maybe
        ok ?= validate_component_permitted_characters(Part, ComponentDefinition, Type),
        ok
    end.

-spec validate_component_permitted_characters(
    Part :: binary(), ComponentDefinition :: purl:type_component_definition(), Type :: purl:type()
) -> ok | purl:parse_error().
validate_component_permitted_characters(Part, #{permitted_characters := PermittedCharacters}, Type) ->
    case re:run(Part, PermittedCharacters) of
        {match, _} ->
            ok;
        nomatch ->
            {error,
                {type_validation, Type, component, Part,
                    <<"Component contains invalid characters.">>}}
    end;
validate_component_permitted_characters(_Part, _ComponentDefinition, _Type) ->
    ok.

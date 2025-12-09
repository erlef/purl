-module(purl_type_registry_test).

-include_lib("eunit/include/eunit.hrl").

-if(?OTP_RELEASE >= 27).
doctest_test() ->
    doctest:module(purl_type_registry, #{}).
-endif.

serve_known_value_test() ->
    file:delete(filename:join([code:priv_dir(purl), "data", "serve_known_value_test.dets"])),

    {ok, _} = purl_type_registry:start_link([{name, serve_known_value_test}]),
    ?assertMatch(
        #{
            type := <<"hex">>,
            '$schema' := <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>,
            '$id' := <<"https://packageurl.org/types/hex-definition.json">>,
            type_name := <<"Hex">>,
            repository :=
                #{
                    use_repository := true,
                    default_repository_url := <<"https://repo.hex.pm">>
                }
        },
        purl_type_registry:lookup(serve_known_value_test, <<"hex">>)
    ),

    ok.

add_lookup_delete_test() ->
    file:delete(filename:join([code:priv_dir(purl), "data", "add_lookup_delete_test.dets"])),

    {ok, _} = purl_type_registry:start_link([{name, add_lookup_delete_test}]),
    Type = <<"hex">>,
    DefaultComponentDefinition = #{case_sensitive => true, normalization_rules => []},
    Specification = #{
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
    },

    ?assertEqual(ok, purl_type_registry:add(add_lookup_delete_test, Specification)),
    ?assertEqual(Specification, purl_type_registry:lookup(add_lookup_delete_test, Type)),

    ?assertEqual(ok, purl_type_registry:delete(add_lookup_delete_test, Type)),
    ?assertEqual(
        #{
            type => <<"hex">>,
            description => <<"Package URL type definition for hex">>,
            '$schema' => <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>,
            '$id' => <<"unregistered://hex.json">>,
            type_name => <<"hex">>,
            repository => #{use_repository => false},
            namespace_definition =>
                #{
                    requirement => <<"optional">>,
                    case_sensitive => true,
                    normalization_rules => []
                },
            name_definition => #{
                requirement => <<"required">>,
                case_sensitive => true,
                normalization_rules => []
            },
            version_definition =>
                #{
                    requirement => <<"optional">>,
                    case_sensitive => true,
                    normalization_rules => []
                },
            qualifiers_definition => [],
            subpath_definition =>
                #{
                    requirement => <<"optional">>,
                    case_sensitive => true,
                    normalization_rules => []
                },
            examples => [],
            reference_urls => []
        },
        purl_type_registry:lookup(add_lookup_delete_test, Type)
    ),

    ok.

list_types_test() ->
    file:delete(filename:join([code:priv_dir(purl), "data", "list_types_test.dets"])),

    {ok, _} = purl_type_registry:start_link([{name, list_types_test}]),

    % Initially should have pre-loaded types from JSON files
    InitialTypes = purl_type_registry:list_types(list_types_test),
    ?assert(is_list(InitialTypes)),
    ?assert(lists:member(<<"hex">>, InitialTypes)),

    % Add a custom type
    CustomType = <<"mytype">>,
    CustomSpec = #{
        type => CustomType,
        type_name => <<"My Type">>,
        description => <<"Custom type for testing">>
    },

    ?assertEqual(ok, purl_type_registry:add(list_types_test, CustomSpec)),

    % Verify the custom type is in the list
    UpdatedTypes = purl_type_registry:list_types(list_types_test),
    ?assert(lists:member(CustomType, UpdatedTypes)),

    % Test default registry function
    ?assertEqual(
        purl_type_registry:list_types(), purl_type_registry:list_types(purl_type_registry)
    ),

    ok.

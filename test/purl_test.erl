-module(purl_test).

-include_lib("eunit/include/eunit.hrl").
-include("purl.hrl").

init_test() ->
    {ok, _Apps} = application:ensure_all_started(purl),
    ok.

-if(?OTP_RELEASE >= 27).
doctest_test() ->
    {ok, _} = application:ensure_all_started(purl),
    doctest:module(purl, #{
        records => [{purl, record_info(fields, purl)}]
    }).
-endif.

to_uri_test() ->
    URI = purl:to_uri(#purl{type = "hex", name = "purl"}),
    ?assertEqual(<<"pkg:hex/purl">>, uri_string:recompose(URI)).

custom_types_test() ->
    Type = <<"acme-package">>,
    Specification = #{
        '$schema' => <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>,
        '$id' => <<"https://acme.com/type.json">>,
        type => Type,
        type_name => <<"Acme Package">>,
        description => <<"Acme Package Type">>,
        repository => #{use_repository => false},
        namespace_definition => #{requirement => <<"optional">>},
        examples => [<<"pkg:acme/acme-package@1.0.0">>]
    },
    ?assertEqual(ok, purl:register_type(Specification)),
    ?assertEqual(
        #{
            type => <<"acme-package">>,
            description => <<"Acme Package Type">>,
            '$schema' => <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>,
            '$id' => <<"https://acme.com/type.json">>,
            type_name => <<"Acme Package">>,
            repository => #{use_repository => false},
            namespace_definition =>
                #{
                    requirement => <<"optional">>,
                    case_sensitive => true,
                    normalization_rules => []
                },
            name_definition => #{
                requirement => <<"required">>, case_sensitive => true, normalization_rules => []
            },
            version_definition => #{
                requirement => <<"optional">>, case_sensitive => true, normalization_rules => []
            },
            qualifiers_definition => [],
            subpath_definition => #{
                requirement => <<"optional">>, case_sensitive => true, normalization_rules => []
            },
            examples => [<<"pkg:acme/acme-package@1.0.0">>],
            reference_urls => []
        },
        purl:lookup_type(Type)
    ),
    ?assertEqual(ok, purl:unregister_type(Type)),
    ?assertEqual(
        #{
            type => <<"acme-package">>,
            description => <<"Package URL type definition for acme-package">>,
            '$schema' => <<"https://packageurl.org/schemas/purl-type-definition.schema-1.0.json">>,
            '$id' => <<"unregistered://acme-package.json">>,
            type_name => <<"acme-package">>,
            repository => #{use_repository => false},
            namespace_definition =>
                #{
                    requirement => <<"optional">>,
                    case_sensitive => true,
                    normalization_rules => []
                },
            examples => [],
            name_definition => #{
                requirement => <<"required">>, case_sensitive => true, normalization_rules => []
            },
            version_definition => #{
                requirement => <<"optional">>, case_sensitive => true, normalization_rules => []
            },
            qualifiers_definition => [],
            subpath_definition => #{
                requirement => <<"optional">>, case_sensitive => true, normalization_rules => []
            },
            reference_urls => []
        },
        purl:lookup_type(Type)
    ),
    ok.

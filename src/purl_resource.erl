-module(purl_resource).

-feature(maybe_expr, enable).

-include("purl.hrl").

-moduledoc false.

-export([from_uri/2]).

-spec from_uri(Uri, FallbackVersion) -> {ok, purl:t()} | error when
    Uri :: uri_string:uri_map() | uri_string:uri_string(),
    FallbackVersion :: undefined | binary().
from_uri(#{scheme := <<"https">>, path := Path, host := <<"github.com">>}, FallbackVersion) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository, <<"tree">>, Version | _Rest] ->
            {ok, github_purl(User, Repository, Version)};
        [User, Repository, <<"blob">>, Version | _Rest] ->
            {ok, github_purl(User, Repository, Version)};
        [User, Repository, <<"releases">>, <<"tag">>, Version | _Rest] ->
            {ok, github_purl(User, Repository, Version)};
        [User, Repository | _Rest] ->
            case FallbackVersion of
                undefined -> {ok, github_purl(User, Repository, <<"HEAD">>)};
                Version -> {ok, github_purl(User, Repository, Version)}
            end;
        _Other ->
            error
    end;
from_uri(
    #{scheme := <<"git+ssh">>, path := Path, userinfo := <<"git">>, host := <<"github.com">>},
    FallbackVersion
) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository] ->
            case FallbackVersion of
                undefined -> {ok, github_purl(User, Repository, <<"HEAD">>)};
                Version -> {ok, github_purl(User, Repository, Version)}
            end;
        _Other ->
            error
    end;
from_uri(#{scheme := <<"https">>, path := Path, host := <<"bitbucket.org">>}, FallbackVersion) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository, <<"src">>, Version | _Rest] ->
            {ok, bitbucket_purl(User, Repository, Version)};
        [User, Repository, <<"raw">>, Version | _Rest] ->
            {ok, bitbucket_purl(User, Repository, Version)};
        [User, Repository, <<"branch">>, Version | _Rest] ->
            {ok, bitbucket_purl(User, Repository, Version)};
        [User, Repository, <<"commits">>, <<"branch">>, Version | _Rest] ->
            {ok, bitbucket_purl(User, Repository, Version)};
        [User, Repository, <<"commits">>, <<"tag">>, Version | _Rest] ->
            {ok, bitbucket_purl(User, Repository, Version)};
        [User, Repository | _Rest] ->
            case FallbackVersion of
                undefined -> {ok, bitbucket_purl(User, Repository, <<"HEAD">>)};
                Version -> {ok, bitbucket_purl(User, Repository, Version)}
            end;
        _Other ->
            error
    end;
from_uri(
    #{scheme := <<"git+ssh">>, path := Path, userinfo := <<"git">>, host := <<"bitbucket.org">>},
    FallbackVersion
) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository] ->
            case FallbackVersion of
                undefined -> {ok, bitbucket_purl(User, Repository, <<"HEAD">>)};
                Version -> {ok, bitbucket_purl(User, Repository, Version)}
            end;
        _Other ->
            error
    end;
from_uri(
    #{scheme := <<"https">>, path := Path, host := <<"hex.pm">>},
    FallbackVersion
) ->
    PathSegments = split_path(Path),

    case PathSegments of
        [<<"packages">>, Package, Version | _Rest] when Version =/= <<"versions">> ->
            {ok, hex_purl(Package, Version)};
        [<<"packages">>, Package | _Rest] ->
            {ok, hex_purl(Package, FallbackVersion)};
        _Other ->
            error
    end;
from_uri(#{}, _FallbackVersion) ->
    error;
from_uri(Uri, FallbackVersion) ->
    case uri_string:parse(Uri) of
        #{} = UriMap ->
            from_uri(UriMap, FallbackVersion);
        {error, _Type, _Term} ->
            case uri_string:parse(convert_git_to_normal_uri(Uri)) of
                #{} = UriMap -> from_uri(UriMap, FallbackVersion);
                {error, _GitType, _GitTerm} -> error
            end
    end.

-spec remove_git_suffix(Path) -> Path when Path :: binary().
remove_git_suffix(Path) -> re:replace(Path, "\\.git$", "", [{return, binary}, unicode]).

-spec split_path(Path) -> [binary()] when Path :: binary().
split_path(Path) -> binary:split(Path, <<"/">>, [trim_all, global]).

-spec convert_git_to_normal_uri(Uri) -> binary() when Uri :: binary().
convert_git_to_normal_uri(Uri) ->
    Replaced = re:replace(Uri, ":", "/", [{return, binary}]),
    <<"git+ssh://", Replaced/binary>>.

-spec github_purl(User, Repository, Version) -> purl:t() when
    User :: binary(),
    Repository :: binary(),
    Version :: binary().
github_purl(User, Repository, Version) ->
    {ok, Purl} = purl:new(#purl{
        type = <<"github">>,
        namespace = [User],
        name = Repository,
        version = Version,
        qualifiers = #{
            <<"vcs_url">> =>
                <<"git+https://github.com/", User/binary, "/", Repository/binary, ".git">>,
            <<"download_url">> =>
                <<"https://github.com/", User/binary, "/", Repository/binary, "/archive/",
                    Version/binary, ".tar.gz">>
        }
    }),
    Purl.

-spec bitbucket_purl(User, Repository, Version) -> purl:t() when
    User :: binary(),
    Repository :: binary(),
    Version :: binary().
bitbucket_purl(User, Repository, Version) ->
    {ok, Purl} = purl:new(#purl{
        type = <<"bitbucket">>,
        namespace = [User],
        name = Repository,
        version = Version,
        qualifiers = #{
            <<"vcs_url">> =>
                <<"git+https://bitbucket.org/", User/binary, "/", Repository/binary, ".git">>,
            <<"download_url">> =>
                <<"https://bitbucket.org/", User/binary, "/", Repository/binary, "/get/",
                    Version/binary, ".tar.gz">>
        }
    }),
    Purl.

-spec hex_purl(Packagey, Version) -> purl:t() when
    Packagey :: binary(),
    Version :: binary() | undefined.
hex_purl(Package, undefined) ->
    {ok, Purl} = purl:new(#purl{type = <<"hex">>, namespace = [], name = Package}),
    Purl;
hex_purl(Package, Version) ->
    Purl = hex_purl(Package, undefined),
    Purl#purl{
        version = Version,
        qualifiers = #{
            <<"download_url">> =>
                <<"https://repo.hex.pm/tarballs/", Package/binary, "/", Version/binary, ".tar">>
        }
    }.

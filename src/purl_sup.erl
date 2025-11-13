-module(purl_sup).
-behaviour(supervisor).

-moduledoc false.

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        purl_type_registry:child_spec([])
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

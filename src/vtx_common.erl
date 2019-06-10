-module(vtx_common).
-export([vsn/0]).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    vtx_common_sup:start_link().


stop(_State) ->
    ok.

vsn() ->
    application:get_key(vtx_common, vsn).


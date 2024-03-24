-module(tree_weaver_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tree_weaver_sup:start_link().

stop(_State) ->
    ok.

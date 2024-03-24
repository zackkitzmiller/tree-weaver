-module(tempfile).
-export([make/0]).

make() ->
    TempDir = os:getenv("TMPDIR", "/tmp"),
    Filename = filename:join(TempDir, "tree_weaver_test_" ++
        integer_to_list(erlang:system_time(microsecond)) ++
        integer_to_list(rand:uniform(1000000))),
    {ok, Filename}.

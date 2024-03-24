-module(tree_weaver_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test the main CLI module
cli_test() ->
    % We'll need to create a temp file for the CLI to process
    {ok, TempFile} = tempfile:make(),

    % Write a simple tree structure to the temp file
    TreeContent =
        "project/\n" ++
        "|-- src/\n" ++
        "|   |-- main.erl\n" ++
        "|-- include/\n" ++
        "|   |-- header.hrl\n" ++
        "|-- README.md\n",

    ok = file:write_file(TempFile, TreeContent),

    % Run the CLI module directly with dry-run
    Result = (catch tree_weaver_cli:main([TempFile, "--dry-run"])),

    % Clean up
    file:delete(TempFile),

    % Verify we didn't crash - using a named variable pattern
    ?assertMatch(X when X =/= {'EXIT', _}, Result).

%% Test CLI with dest option
dest_option_test() ->
    % Create temp file
    {ok, TempFile} = tempfile:make(),

    % Write a simple tree to the file
    ok = file:write_file(TempFile,
        "project/\n" ++
        "|-- src/\n"),

    % Run CLI with dest option
    Result = (catch tree_weaver_cli:main([TempFile, "--dest", "/tmp"])),

    % Clean up
    file:delete(TempFile),

    % Verify we didn't crash
    ?assertMatch(X when X =/= {'EXIT', _}, Result).

%% Test skipping root directory
skip_root_test() ->
    % Create temp file
    {ok, TempFile} = tempfile:make(),

    % Write a simple tree to the file
    ok = file:write_file(TempFile,
        "project/\n" ++
        "|-- src/\n"),

    % Run CLI with dest=. option
    Result = (catch tree_weaver_cli:main([TempFile, "--dest", "."])),

    % Clean up
    file:delete(TempFile),

    % Verify we didn't crash
    ?assertMatch(X when X =/= {'EXIT', _}, Result).

%% Test handling comments in the tree
comments_test() ->
    % Create temp file
    {ok, TempFile} = tempfile:make(),

    % Write a tree with comments
    ok = file:write_file(TempFile,
        "project/\n" ++
        "|-- src/ # Source directory\n" ++
        "|   |-- main.erl # Main module\n" ++
        "|-- # Just a comment line\n" ++
        "|-- README.md\n"),

    % Run CLI
    Result = (catch tree_weaver_cli:main([TempFile, "--dry-run"])),

    % Clean up
    file:delete(TempFile),

    % Verify we didn't crash
    ?assertMatch(X when X =/= {'EXIT', _}, Result).

%% Test handling special characters
special_chars_test() ->
    % Create temp file
    {ok, TempFile} = tempfile:make(),

    % Write a tree with special characters
    ok = file:write_file(TempFile,
        "project/\n" ++
        "|-- name with spaces.txt\n" ++
        "|-- file-with-dashes.md\n" ++
        "|-- .hidden_file\n"),

    % Run CLI
    Result = (catch tree_weaver_cli:main([TempFile, "--dry-run"])),

    % Clean up
    file:delete(TempFile),

    % Verify we didn't crash
    ?assertMatch(X when X =/= {'EXIT', _}, Result).

-module(tree_weaver_cli).
-export([main/1]).

-import(z, [x/1]).

main(["katy"]) ->
    x(["katy"]);
main(Args) ->
    % Parse arguments
    parse_args(Args, #{dry_run => false, skip_root => false, dest => "."}).

parse_args([], _Options) ->
    usage();
parse_args(["--dry-run" | Rest], Options) ->
    parse_args(Rest, Options#{dry_run => true});
parse_args(["--dest", "." | Rest], Options) ->
    % When dest is ".", skip root node
    parse_args(Rest, Options#{skip_root => true, dest => "."});
parse_args(["--dest", Dest | Rest], Options) ->
    parse_args(Rest, Options#{dest => Dest});
parse_args([Filename, "--dry-run" | _], Options) ->
    process_file(Filename, Options#{dry_run => true});
parse_args([Filename, "--dest", "." | _], Options) ->
    process_file(Filename, Options#{skip_root => true, dest => "."});
parse_args([Filename, "--dest", Dest | _], Options) ->
    process_file(Filename, Options#{dest => Dest});
parse_args([Filename | _], Options) ->
    process_file(Filename, Options).

process_file(Filename, Options) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            DryRun = maps:get(dry_run, Options),
            if DryRun -> io:format("DRY RUN MODE: No files or directories will be created~n~n"); true -> ok end,
            process_content(Filename, Content, Options);
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [Filename, Reason])
    end.

process_content(Filename, Content, Options) ->
    % Convert to list and split lines
    Lines = string:split(binary_to_list(Content), "\n", all),

    % Debug output
    io:format("Processing file with ~p lines~n", [length(Lines)]),

    % Parse the first line as base directory
    case Lines of
        [] ->
            io:format("Error: Empty file ~s~n", [Filename]);
        [BaseDir | StructureLines] ->
            % Trim base directory name
            TrimmedBaseDir = string:trim(BaseDir),

            % Get destination directory
            DestDir = maps:get(dest, Options),

            % Check if we should skip root
            SkipRoot = maps:get(skip_root, Options),

            % Final base path
            BasePath = if
                SkipRoot ->
                    io:format("Using destination directory directly: ~s~n", [DestDir]),
                    DestDir;
                true ->
                    io:format("Base directory: ~s~n", [TrimmedBaseDir]),
                    filename:join(DestDir, TrimmedBaseDir)
            end,

            DryRun = maps:get(dry_run, Options),
            % Create base directory (in non-dry-run mode)
            if
                DryRun ->
                    if SkipRoot ->
                        io:format("Would use existing directory: ~s~n", [BasePath]);
                      true ->
                        io:format("Would create base directory: ~s~n", [BasePath])
                    end;
                true ->
                    case filelib:ensure_dir(filename:join(BasePath, "dummy")) of
                        ok ->
                            % Create the base directory explicitly if it doesn't exist
                            case SkipRoot of
                                false ->
                                    case file:make_dir(BasePath) of
                                        ok -> ok;
                                        {error, eexist} -> ok; % Directory already exists
                                        {error, Reason} ->
                                            io:format("Error creating base directory: ~p~n", [Reason])
                                    end;
                                true -> ok
                            end;
                        {error, Reason} ->
                            io:format("Error ensuring base directory path: ~p~n", [Reason])
                    end
            end,

            % Process structure directly using the simple approach
            process_structure_lines(BasePath, StructureLines, DryRun)
    end.

process_structure_lines(BasePath, Lines, DryRun) ->
    % Parse the tree structure
    Paths = parse_tree(Lines),

    % Create each path
    lists:foreach(
        fun(Path) ->
            FullPath = filename:join(BasePath, Path),
            create_node(FullPath, Path, DryRun)
        end,
        Paths
    ).
% Parse a tree structure into a list of paths
parse_tree(Lines) ->
    % Keep track of the current path at each indentation level
    parse_lines(Lines, #{}, 0, []).

parse_lines([], _Paths, _CurrentIndent, Acc) ->
    lists:reverse(Acc);
parse_lines([Line | Rest], Paths, _PrevIndent, Acc) ->
    % Skip empty lines
    TrimmedLine = string:trim(Line),
    if TrimmedLine =:= "" ->
        parse_lines(Rest, Paths, 0, Acc);
    true ->
        % Count indent level (any non-content characters)
        {Indent, Name} = extract_name(Line),

        % Clean up any comments
        CleanName = remove_comments(Name),

        % Skip if just a comment line
        if CleanName =:= "" ->
            parse_lines(Rest, Paths, Indent, Acc);
        true ->
            % Update paths map for this indent level
            NewPaths = update_paths(Paths, Indent, CleanName),

            % Build full path for this item
            CurrentPath = build_path(NewPaths, Indent),

            % Add to results and continue
            parse_lines(Rest, NewPaths, Indent, [CurrentPath | Acc])
        end
    end.

% Extract name from a line, returning indent level and name
extract_name(Line) ->
    % Find the actual content (skipping tree chars, whitespace)
    Chars = unicode:characters_to_list(Line),
    extract_name(Chars, 0, []).

extract_name([], Indent, Acc) ->
    {Indent, lists:reverse(Acc)};
extract_name([C | Rest], Indent, Acc) ->
    % Only printable ASCII characters (excluding tree chars)
    if
        % Treat these as content characters (ASCII printable, excluding space)
        (C >= 33 andalso C =< 126) ->
            % This is actual content, add to accumulator
            extract_name(Rest, Indent, [C | Acc]);
        true ->
            % Skip this character (tree char, whitespace, etc)
            extract_name(Rest, Indent + 1, Acc)
    end.


% Remove comments from a name
remove_comments(Name) ->
    % Look for " #" which indicates a comment
    case string:find(Name, " #") of
        nomatch -> Name;
        Match ->
            BeforeComment = string:slice(Name, 0, string:length(Name) - string:length(Match)),
            string:trim(BeforeComment)
    end.

% Update the paths map for this indentation level
update_paths(Paths, Indent, Name) ->
    % Add this path to the map at this indent level
    % Remove any higher indent levels (they're children)
    FilteredPaths = maps:filter(
        fun(Level, _) -> Level < Indent end,
        Paths
    ),

    % Add our path at this level
    FilteredPaths#{Indent => Name}.

build_path(Paths, MaxIndent) ->
    % Get all paths up to this indent level, in order
    Levels = lists:sort(maps:keys(Paths)),
    ValidLevels = [L || L <- Levels, L =< MaxIndent],

    % Build path from components
    Components = [maps:get(Level, Paths) || Level <- ValidLevels],
    filename:join(Components).


% Create a directory or file based on path
create_node(FullPath, Path, DryRun) ->
    IsDir = is_directory(Path),
    if
        IsDir ->
            if
                DryRun ->
                    io:format("Would create directory: ~s~n", [FullPath]);
                true ->
                    io:format("Creating directory: ~s~n", [FullPath]),
                    filelib:ensure_dir(filename:join(FullPath, "dummy")),
                    file:make_dir(FullPath)
            end;
        true ->
            if
                DryRun ->
                    io:format("Would create file: ~s~n", [FullPath]);
                true ->
                    io:format("Creating file: ~s~n", [FullPath]),
                    filelib:ensure_dir(FullPath),
                    file:write_file(FullPath, <<>>)
            end
    end.

% Determine if path is a directory
is_directory(Path) ->
    case string:length(Path) of
        0 -> false;
        Len ->
            % Check if path ends with /
            case string:slice(Path, Len-1, 1) of
                "/" -> true;
                _ ->
                    % If it has no extension, assume it's a directory
                    case filename:extension(Path) of
                        "" -> true;
                        _ -> false
                    end
            end
    end.


usage() ->
    io:format("Usage: tree-weaver [--dry-run] [--dest PATH] FILENAME~n"),
    io:format("  FILENAME: A text file describing the project structure~n"),
    io:format("  --dry-run: Show what would be created without making changes~n"),
    io:format("  --dest PATH: Specify the destination path (default: current directory)~n"),
    io:format("  --dest .: Skip creating the root directory~n"),
    io:format("  Format: First line is base directory, subsequent lines are paths~n").

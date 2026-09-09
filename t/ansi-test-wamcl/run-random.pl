% Run with: swipl -s t/ansi-test-wamcl/run-random.pl -g run_random_tests,halt -t "halt(1)"
:- ensure_loaded(run).

wamcl_random_sources(
    [ ansi('compile-and-load.lsp'),
      ansi('rt-package.lsp'),
      ansi('rt.lsp'),
      ansi('cl-test-package.lsp'),
      ansi('auxiliary/ansi-aux-macros.lsp'),
      ansi('universe.lsp'),
      ansi('auxiliary/random-aux.lsp'),
      adapter('random-bootstrap.lsp')
    ]).

wamcl_marked_input(_, [], "", []).
wamcl_marked_input(Root, [Source|Rest], Input, [Marker|Markers]) :-
    wamcl_source_path(Root, Source, Path),
    read_file_to_string(Path, Text, [encoding(utf8)]),
    format(string(Marker), '@@WAMCL-SOURCE:~w@@', [Source]),
    format(string(EndForm),
           '~n(cl::prolog-inline "writeln(user_error,\'~s\')")~n',
           [Marker]),
    wamcl_marked_input(Root, Rest, Tail, Markers),
    atomics_to_string([Text, EndForm, Tail], Input).

run_random_tests :-
    wamcl_repo_root(Root),
    wamcl_random_sources(Sources),
    wamcl_marked_input(Root, Sources, Input, Markers),
    wamcl_source_path(Root, adapter('child.pl'), ChildFile),
    directory_file_path(Root, 'prolog/wamcl.pl', WamclFile),
    directory_file_path(Root, prolog, PrologDir),
    format(atom(LibraryArg), 'library=~w', [PrologDir]),
    Goal = 'assertz((user:wamcl_test_pass:-writeln(user_error,\'@@WAMCL-RANDOM-PASS@@\'))),assertz((user:wamcl_test_fail:-writeln(user_error,\'@@WAMCL-RANDOM-FAIL@@\'))),lisp,halt',
    wamcl_spawn_child(LibraryArg, WamclFile, ChildFile, Goal, 600,
                      Input, Status, Output),
    wamcl_source_path(Root, adapter('results'), ResultsDir),
    make_directory_path(ResultsDir),
    directory_file_path(ResultsDir, 'random-latest.log', LogPath),
    setup_call_cleanup(open(LogPath, write, Log, [encoding(utf8)]),
                       format(Log, '~s', [Output]), close(Log)),
    count_substring(Output, '%~ failed(', LoadFailures),
    TestCount = 9,
    length(Markers, SourceCount),
    split_string(Output, "\n", "\r", Lines),
    include(==("@@WAMCL-RANDOM-PASS@@"), Lines, Passes),
    include(==("@@WAMCL-RANDOM-FAIL@@"), Lines, Failures),
    length(Passes, PassCount),
    length(Failures, FailCount),
    include(wamcl_marker_present(Lines), Markers, FoundMarkers),
    length(FoundMarkers, MarkerCount),
    format(user_error,
           '=== Random bootstrap: ~d/~d source markers, ~d failed forms, ~d/~d passed, ~d failed; child ~w ===~n',
           [MarkerCount, SourceCount, LoadFailures, PassCount, TestCount,
            FailCount, Status]),
    Status == exit(0),
    MarkerCount =:= SourceCount,
    LoadFailures =:= 0,
    PassCount =:= TestCount,
    FailCount =:= 0.

wamcl_marker_present(Lines, Marker) :-
    memberchk(Marker, Lines).

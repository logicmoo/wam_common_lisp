% Bootstrap and execute the first WAM-CL ANSI test slice.
%
% Run from the repository root:
%   swipl -s t/ansi-test-wamcl/run.pl -g run_tests,halt -t "halt(1)"

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).

wamcl_ansi_sources(
    [ ansi('compile-and-load.lsp'),
      ansi('rt-package.lsp'),
      ansi('rt.lsp'),
      ansi('cl-test-package.lsp'),
      ansi('auxiliary/ansi-aux-macros.lsp'),
      adapter('loop-prelude.lsp'),
      ansi('iteration/loop.lsp')
    ]).

wamcl_ansi_test_names(
    [ 'SLOOP.1',
      'SLOOP.2',
      'SLOOP.3',
      'SLOOP.4',
      'SLOOP.5',
      'SLOOP.6',
      'SLOOP.7'
    ]).

wamcl_repo_root(Root) :-
    (   exists_file('prolog/wam_cl/repl.pl')
    ->  absolute_file_name('.', Root)
    ;   exists_file('../../prolog/wam_cl/repl.pl')
    ->  absolute_file_name('../..', Root)
    ;   throw(error(existence_error(directory, repository_root), _))
    ).

wamcl_source_path(Root, ansi(Relative), Path) :-
    directory_file_path(Root, t, TestDir),
    directory_file_path(TestDir, 'ansi-test', AnsiDir),
    directory_file_path(AnsiDir, Relative, Path).
wamcl_source_path(Root, adapter(Relative), Path) :-
    directory_file_path(Root, t, TestDir),
    directory_file_path(TestDir, 'ansi-test-wamcl', AdapterDir),
    directory_file_path(AdapterDir, Relative, Path).

wamcl_build_input(_, [], "").
wamcl_build_input(Root, [Source|Rest], Input) :-
    wamcl_source_path(Root, Source, Path),
    read_file_to_string(Path, Text, [encoding(utf8)]),
    wamcl_build_input(Root, Rest, RestInput),
    atomics_to_string([Text, "\n", RestInput], Input).

run_tests :-
    wamcl_repo_root(Root),
    wamcl_ansi_sources(Sources),
    wamcl_ansi_test_names(TestNames),
    wamcl_build_input(Root, Sources, Input),
    length(Sources, SourceCount),
    length(TestNames, TestCount),
    string_length(Input, InputBytes),
    format(user_error,
           '~N=== WAM-CL ANSI LOOP slice: ~d bytes, ~d source files, ~d tests ===~n',
           [InputBytes, SourceCount, TestCount]),
    wamcl_run_child(Root, TestNames, Input, Status, Output),
    wamcl_write_reports(Root, Status, TestCount, Output, ReportPath),
    wamcl_print_result_lines(Output),
    count_substring(Output, '@@WAMCL-ANSI-RESULT@@', ResultCount),
    count_substring(Output, '%~ failed(', LoadFailureCount),
    format(string(ExpectedSummary),
           '@@WAMCL-ANSI-SUMMARY@@ passed=~d failed=0 errors=0',
           [TestCount]),
    (   Status == exit(0),
        ResultCount =:= TestCount,
        LoadFailureCount =:= 0,
        sub_string(Output, _, _, _, ExpectedSummary)
    ->  format(user_error,
               '=== WAM-CL ANSI LOOP slice complete: ~d passed, 0 failed ===~n',
               [TestCount])
    ;   format(user_error,
               '*** ANSI LOOP slice failed: child ~w, ~d result markers, ~d load failures; see ~w ***~n',
               [Status, ResultCount, LoadFailureCount, ReportPath]),
        fail
    ).

wamcl_run_child(Root, TestNames, Input, Status, Output) :-
    directory_file_path(Root, prolog, PrologDir),
    directory_file_path(PrologDir, 'wamcl.pl', WamclFile),
    directory_file_path(Root, t, TestDir),
    directory_file_path(TestDir, 'ansi-test-wamcl', AdapterDir),
    directory_file_path(AdapterDir, 'child.pl', ChildFile),
    format(atom(LibraryArg), 'library=~w', [PrologDir]),
    term_string(TestNames, TestNamesText, [quoted(true)]),
    format(atom(Goal),
           'catch(set_prolog_flag(lisp_verbose,0),_,true),catch(cl:set_wam_cl_option(call_statistics,false),_,true),catch(lisp,_,true),(wamcl_ansi_run(~w)->halt;halt(1))',
           [TestNamesText]),
    tmp_file_stream(utf8, OutFile, Out0), close(Out0),
    tmp_file_stream(utf8, ErrFile, Err0), close(Err0),
    open(OutFile, write, OutStream, [encoding(utf8)]),
    open(ErrFile, write, ErrStream, [encoding(utf8)]),
    setup_call_cleanup(
        process_create(path(swipl),
                       ['-p', LibraryArg, '-g', Goal, '-t', 'halt(1)',
                       WamclFile, ChildFile],
                       [ stdin(pipe(InPipe)),
                         stdout(stream(OutStream)),
                         stderr(stream(ErrStream)),
                         process(PID)
                       ]),
        ( thread_create(wamcl_write_child_input(InPipe,Input),Writer,[]),
          process_wait(PID, WaitStatus, [timeout(180)]),
          (   WaitStatus == timeout
          ->  process_kill(PID, term),
              process_wait(PID, _),
              Status = timeout
          ;   Status = WaitStatus
          ),
          thread_join(Writer,_)
        ),
        ( catch(close(InPipe),_,true),
          close(OutStream),
          close(ErrStream)
        )),
    read_file_to_string(OutFile, Stdout, [encoding(utf8)]),
    read_file_to_string(ErrFile, Stderr, [encoding(utf8)]),
    delete_file(OutFile),
    delete_file(ErrFile),
    atomics_to_string([Stdout, "\n", Stderr], Output).

wamcl_write_child_input(Stream,Input):-
    catch(( set_stream(Stream,encoding(utf8)),
            write(Stream,Input),
            flush_output(Stream)
          ),
          _,
          true),
    catch(close(Stream),_,true).

wamcl_print_result_lines(Output) :-
    split_string(Output, "\n", "\r", Lines),
    forall(( member(Line, Lines),
             sub_string(Line, 0, _, _, '@@WAMCL-ANSI-')
           ),
           format(user_error, '~s~n', [Line])).

wamcl_write_reports(Root, Status, Selected, Output, MarkdownPath) :-
    directory_file_path(Root, t, TestDir),
    directory_file_path(TestDir, 'ansi-test-wamcl', AdapterDir),
    directory_file_path(AdapterDir, results, ResultsDir),
    make_directory_path(ResultsDir),
    directory_file_path(ResultsDir, 'latest.md', MarkdownPath),
    setup_call_cleanup(
        open(MarkdownPath, write, Markdown, [encoding(utf8)]),
        format(Markdown,
               '# WAM-CL ANSI LOOP transcript~n~nChild status: `~w`~n~n````text~n~s~n````~n',
               [Status, Output]),
        close(Markdown)),
    count_result_lines(Output, 'PASS', Passed),
    count_result_lines(Output, 'FAIL', Failed),
    count_result_lines(Output, 'ERROR', Errors),
    directory_file_path(ResultsDir, 'latest.json', JsonPath),
    setup_call_cleanup(
        open(JsonPath, write, Json, [encoding(utf8)]),
        format(Json,
               '{~n  "suite": "iteration/loop.lsp",~n  "selected": ~d,~n  "passed": ~d,~n  "failed": ~d,~n  "errors": ~d,~n  "childStatus": "~w"~n}~n',
               [Selected, Passed, Failed, Errors, Status]),
        close(Json)).

count_result_lines(Output, Status, Count) :-
    split_string(Output, "\n", "\r", Lines),
    aggregate_all(
        count,
        ( member(Line, Lines),
          sub_string(Line, 0, _, _, '@@WAMCL-ANSI-RESULT@@'),
          sub_string(Line, _, _, 0, Status)
        ),
        Count).

count_substring(String, Substring, Count) :-
    aggregate_all(count, sub_string(String, _, _, _, Substring), Count).

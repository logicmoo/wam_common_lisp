% t/travis.pl - `make test` entry point for WAM-CL.
%
% Runs the Lisp sanity test suite by feeding the concatenated test files to a
% fresh WAM-CL REPL subprocess on stdin (the same path a human uses when piping
% a file into the REPL). The REPL catches per-form errors and keeps going, so
% one broken form never aborts the whole run.
%
% The `is` test macro (prolog/wam_cl/utests.pl) calls (prolog-inline
% "wamcl_test_pass") / (prolog-inline "wamcl_test_fail") on each assertion. The
% child process defines those predicates so they print a unique marker; this
% harness counts the markers in the child's output and exits non-zero if any
% assertion failed (or if no assertion ran at all).
%
% Run standalone:  swipl -s t/travis.pl -g run_tests,halt -t 'halt(1)'

:- use_module(library(process)).
:- use_module(library(readutil)).

% Sanity test files (without the ".lisp" suffix), loaded in order.
wamcl_sanity_tests(
  [ "sanity-util",
    "sanity-test-0",
    "sanity-test-1",
    "sanity-test-2",
    "sanity-test-4",
    "sanity-test-5" ]).

% Locate the repository root (the directory that holds prolog/ and t/).
wamcl_repo_root(Root) :-
    ( exists_file('prolog/wam_cl/repl.pl')    -> absolute_file_name('.',  Root)
    ; exists_file('../prolog/wam_cl/repl.pl') -> absolute_file_name('..', Root)
    ; absolute_file_name('.', Root) ).

% Concatenate the source of each "<name>.lisp" test file from <Root>/t/.
wamcl_build_input(_Root, [], "").
wamcl_build_input(Root, [Name|Rest], Input) :-
    format(atom(File), "~w/t/~w.lisp", [Root, Name]),
    ( catch(read_file_to_string(File, Src, []), _, fail) -> true ; Src = "" ),
    wamcl_build_input(Root, Rest, RestInput),
    atomics_to_string([Src, "\n", RestInput], Input).

atomics_to_string(List, String) :-
    atomic_list_concat(List, Atom),
    atom_string(Atom, String).

% Goal run inside the child REPL process: define the assertion markers, quiet
% the compiler noise, then start the REPL reading test source from stdin.
wamcl_child_goal(
  "assertz((user:wamcl_test_pass:-write(user_error,'@@WAMCL_PASS@@'),nl(user_error))), \c
   assertz((user:wamcl_test_fail:-write(user_error,'@@WAMCL_FAIL@@'),nl(user_error))), \c
   catch(set_prolog_flag(lisp_verbose,0),_,true), \c
   catch(cl:set_wam_cl_option(call_statistics,false),_,true), \c
   catch(lisp,_,true), halt").

run_tests :-
    wamcl_repo_root(Root),
    wamcl_sanity_tests(Tests),
    wamcl_build_input(Root, Tests, Input),
    string_length(Input, InLen),
    length(Tests, NTests),
    format(user_error, "~N=== WAM-CL sanity tests: ~w bytes of input from ~w file(s) ===~n",
           [InLen, NTests]),
    ( InLen =:= 0
    -> format(user_error, "~N*** No test input found under ~w/t/ ***~n", [Root]), fail
    ;  true ),
    format(atom(WamclPl), "~w/prolog/wamcl.pl", [Root]),
    format(atom(LibArg),  "library=~w/prolog", [Root]),
    % Capture the child's stdout/stderr through temp files (unbounded, so they
    % never fill a pipe buffer) and feed the test source on a stdin pipe that we
    % write in full and then close.
    tmp_file_stream(utf8, OutFile, OutS0), close(OutS0),
    tmp_file_stream(utf8, ErrFile, ErrS0), close(ErrS0),
    wamcl_child_goal(ChildGoal),
    open(OutFile, write, OutStream, [encoding(utf8)]),
    open(ErrFile, write, ErrStream, [encoding(utf8)]),
    setup_call_cleanup(
        process_create(path(swipl),
                       ['-p', LibArg, '-g', ChildGoal, '-t', 'halt(1)', WamclPl],
                       [ stdin(pipe(InPipe)),
                         stdout(stream(OutStream)),
                         stderr(stream(ErrStream)),
                         process(PID) ]),
        ( set_stream(InPipe, encoding(utf8)),
          write(InPipe, Input), flush_output(InPipe), close(InPipe),
          process_wait(PID, Status) ),
        ( close(OutStream), close(ErrStream) )),
    read_file_to_string(ErrFile, ErrStr, [encoding(utf8)]),
    read_file_to_string(OutFile, _OutStr, [encoding(utf8)]),
    count_substring(ErrStr, "@@WAMCL_PASS@@", NPass),
    count_substring(ErrStr, "@@WAMCL_FAIL@@", NFail),
    Total is NPass + NFail,
    catch(delete_file(OutFile), _, true),
    catch(delete_file(ErrFile), _, true),
    format(user_error,
           "~N=== WAM-CL sanity tests complete: ~w passed, ~w failed (~w assertions), child ~w ===~n",
           [NPass, NFail, Total, Status]),
    ( Total =:= 0
    -> format(user_error, "~N*** No assertions ran -- test harness did not execute the suite ***~n", []),
       fail
    ; NFail =:= 0
    -> true
    ; format(user_error, "~N*** ~w assertion(s) FAILED ***~n", [NFail]),
       fail ).

% Count non-overlapping occurrences of Sub in Str.
count_substring(Str, Sub, Count) :-
    ( sub_string(Str, _, _, _, Sub)
    -> aggregate_all(count, sub_string(Str, _, _, _, Sub), Count)
    ;  Count = 0 ).

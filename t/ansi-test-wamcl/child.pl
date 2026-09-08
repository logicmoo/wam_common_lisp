% Executes registered ANSI tests after the WAM-CL REPL has consumed stdin.
% Running dynamic EVAL while the REPL is still reading corrupts its input
% stream, so the parent harness deliberately invokes this only after EOF.

wamcl_ansi_run(TestNames) :-
    get_opv(regression_test_xx_entries_xx, symbol_value, [_|Entries]),
    maplist(wamcl_ansi_run_named(Entries), TestNames, Outcomes),
    include(==(pass), Outcomes, Passes),
    include(==(fail), Outcomes, Failures),
    include(==(error), Outcomes, Errors),
    length(Passes, PassCount),
    length(Failures, FailCount),
    length(Errors, ErrorCount),
    format(user_error,
           '@@WAMCL-ANSI-SUMMARY@@ passed=~d failed=~d errors=~d~n',
           [PassCount, FailCount, ErrorCount]),
    FailCount =:= 0,
    ErrorCount =:= 0.

wamcl_ansi_run_named(Entries, TestName, Outcome) :-
    (   member(Entry, Entries),
        f_regression_test_name(Entry, [], Symbol),
        pl_symbol_name(Symbol, NameString),
        string_upper(NameString, UpperName),
        atom_string(TestName, UpperName)
    ->  wamcl_ansi_run_entry(TestName, Entry, Outcome)
    ;   Outcome = error,
        format(user_error, '@@WAMCL-ANSI-RESULT@@ ~w ERROR missing-test~n',
               [TestName])
    ).

wamcl_ansi_run_entry(TestName, Entry, Outcome) :-
    f_regression_test_form(Entry, [], Form),
    f_regression_test_vals(Entry, [], Expected),
    global_env(Env),
    nb_linkval('$mv_return', [Result]),
    catch(( f_sys_env_eval(Env, Form, Result),
            nb_current('$mv_return', Actual),
            f_regression_test_equalp_with_case(Actual, Expected, Equal),
            (   Equal == t
            ->  Outcome = pass,
                format(user_error, '@@WAMCL-ANSI-RESULT@@ ~w PASS~n',
                       [TestName])
            ;   Outcome = fail,
                format(user_error, '@@WAMCL-ANSI-RESULT@@ ~w FAIL~n',
                       [TestName]),
                wamcl_ansi_write_values(actual, Actual),
                wamcl_ansi_write_values(expected, Expected)
            )
          ),
          Error,
          ( Outcome = error,
            format(user_error, '@@WAMCL-ANSI-RESULT@@ ~w ERROR~n',
                   [TestName]),
            write_term(user_error, Error,
                       [quoted(true), max_depth(20), cycles(true)]),
            nl(user_error) )).

wamcl_ansi_write_values(Label, Values) :-
    format(user_error, '@@WAMCL-ANSI-~w@@ ', [Label]),
    write_term(user_error, Values,
               [quoted(true), max_depth(20), cycles(true)]),
    nl(user_error).

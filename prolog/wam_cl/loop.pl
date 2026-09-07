/*******************************************************************
 *
 * Practical ANSI Common Lisp LOOP macro expansion.
 *
 * Expands LOOP forms into the WAM-CL primitives LET*, BLOCK, TAGBODY,
 * GO, SETQ and ordinary function calls.  Keeping the implementation at
 * macro-expansion time avoids adding another evaluator/compiler path.
 *
 * Implemented here: simple LOOP; NAMED; WITH; REPEAT; numeric, IN, ON,
 * ACROSS and =/THEN drivers; destructuring; FOR/AS ... AND; INITIALLY and
 * FINALLY; DO; RETURN; list/numeric/extreme accumulation; WHILE/UNTIL;
 * ALWAYS/NEVER/THEREIS; conditional AND/ELSE/END and IT; LOOP-FINISH.
 *
 * Hash-table and package iteration paths remain unavailable because their
 * underlying WAM-CL runtime iterators are incomplete.
 *
 *******************************************************************/

:- module(loop, []).
:- set_module(class(library)).
:- include('./header').

:- discontiguous(loop_parse_clause/4).
:- discontiguous(loop_parse_action/5).

% The image metadata already names mf_loop as CL:LOOP's macro function, but
% older images did not provide the predicate itself.
mf_loop(Whole, Expansion) :-
    mf_loop(Whole, [], Expansion).

mf_loop([loop|Clauses], _Env, Expansion) :-
    !,
    once(loop_expand(Clauses, Expansion)).
mf_loop(Form, _Env, _Expansion) :-
    loop_syntax_error(expected_loop_form, Form).

% Frozen phase-A fallback.  Keep the original operator name out of the old
% expander so it emits a normal CL:LOOP expansion without sharing this parser.
mf_sys_practical_loop(Whole, Expansion) :-
    mf_sys_practical_loop(Whole, [], Expansion).

mf_sys_practical_loop([sys_practical_loop|Clauses], Env, Expansion) :-
    !,
    loop_version_a:expand_practical_loop([loop|Clauses], Env, Expansion).
mf_sys_practical_loop(Form, _Env, _Expansion) :-
    loop_syntax_error(expected_sys_practical_loop_form, Form).

% ---------------------------------------------------------------------------
% Top-level expansion
% ---------------------------------------------------------------------------

loop_expand(Clauses, Expansion) :-
    ( loop_extended_start(Clauses)
    -> loop_expand_extended(Clauses, Expansion)
    ;  loop_expand_simple(Clauses, Expansion)
    ).

loop_extended_start([Keyword|_]) :-
    atom(Keyword),
    loop_clause_keyword(Keyword).

loop_expand_simple(Forms0, Expansion) :-
    maplist(loop_validate_simple_form, Forms0),
    gensym(loop_start_, Start),
    gensym(loop_finish_, Finish),
    maplist(loop_rewrite_form(Finish), Forms0, Forms),
    append([[label, Start]|Forms],
           [[go, Start], [label, Finish]],
           TagBody),
    Expansion = [block, [], [tagbody|TagBody], []].

loop_validate_simple_form([_|_]) :-
    !.
loop_validate_simple_form(Form) :-
    loop_syntax_error(simple_loop_requires_compound_form, Form).

loop_expand_extended(Clauses0, Expansion) :-
    loop_parse_name(Clauses0, Name, Clauses),
    loop_initial_state(Name, State0),
    loop_parse_clauses(Clauses, State0, State),
    loop_build_expansion(State, Expansion0),
    get_dict(finish, State, Finish),
    loop_rewrite_form(Finish, Expansion0, Expansion).

loop_parse_name([Keyword, Name|Rest], Name, Rest) :-
    loop_token_is(Keyword, named),
    !,
    loop_validate_variable(Name).
loop_parse_name(Clauses, [], Clauses).

loop_initial_state(Name,
                   loop_state{
                       name:Name,
                       start:Start,
                       finish:Finish,
                       bindings:[],
                       variables:[],
                       initial:[],
                       checks:[],
                       prefix:[],
                       body:[],
                       steps:[],
                       final:[],
                       result:[],
                       result_set:false,
                       default_acc:none,
                       accumulators:[],
                       source_vars:[],
                       prefix_items:[]
                   }) :-
    gensym(loop_start_, Start),
    gensym(loop_finish_, Finish).

loop_build_expansion(State, Expansion) :-
    get_dict(name, State, Name),
    get_dict(start, State, Start),
    get_dict(finish, State, Finish),
    get_dict(bindings, State, Bindings),
    get_dict(initial, State, Initial),
    get_dict(checks, State, Conditions),
    get_dict(prefix, State, Prefix),
    get_dict(body, State, Body),
    get_dict(steps, State, Steps),
    get_dict(final, State, Final0),
    get_dict(result, State, Result0),
    loop_final_result(Name, Final0, Result0, Final, Result),
    maplist(loop_exit_check(Finish), Conditions, Checks),
    append(Initial, [[label, Start]|Checks], TagStart),
    append(TagStart, Prefix, Tag0),
    append(Tag0, Body, Tag1),
    append(Tag1, Steps, Tag2),
    append(Tag2, [[go, Start], [label, Finish]], TagBody),
    append([[tagbody|TagBody]|Final], [Result], LetBody),
    Expansion = [block, Name, [let_xx, Bindings|LetBody]].

loop_exit_check(Finish, Condition, [when, Condition, [go, Finish]]).

% WAM-CL's block compiler eagerly unifies the value of the last form even when
% an earlier RETURN makes it unreachable.  Hoist the conventional final
% (return value) into the LOOP result position to preserve CL semantics.
loop_final_result(_Name, Final0, _Default, Final, Result) :-
    append(Final, [[return, Result]], Final0),
    !.
loop_final_result(Name, Final0, _Default, Final, Result) :-
    append(Final, [[return_from, Name, Result]], Final0),
    !.
loop_final_result(_Name, Final, Result, Final, Result).


% ---------------------------------------------------------------------------
% Clause parser
% ---------------------------------------------------------------------------

loop_parse_clauses([], State, State) :-
    !.
loop_parse_clauses(Tokens, State0, State) :-
    loop_parse_clause(Tokens, Rest, State0, State1),
    !,
    loop_parse_clauses(Rest, State1, State).
loop_parse_clauses(Tokens, _State0, _State) :-
    loop_syntax_error(unsupported_or_malformed_clause, Tokens).

loop_parse_clause([Token|Tokens], Rest, State0, State) :-
    loop_token_is(Token, Keyword),
    !,
    loop_parse_clause_kind(Keyword, Tokens, Rest, State0, State).
loop_parse_clause(Tokens, _Rest, _State0, _State) :-
    loop_syntax_error(unsupported_or_malformed_clause, Tokens).

loop_parse_clause_kind(with, Tokens, Rest, State0, State) :-
    loop_parse_with(Tokens, Rest, State0, State).
loop_parse_clause_kind(repeat, [Count|Rest], Rest, State0, State) :-
    loop_add_repeat(Count, State0, State).
loop_parse_clause_kind(for, Tokens, Rest, State0, State) :-
    loop_parse_for(Tokens, Rest, State0, State).
loop_parse_clause_kind(as, Tokens, Rest, State0, State) :-
    loop_parse_for(Tokens, Rest, State0, State).
loop_parse_clause_kind(initially, Tokens, Rest, State0, State) :-
    loop_take_top_forms(Tokens, Forms, Rest),
    loop_require_forms(initially, Forms),
    loop_add_forms(initial, Forms, State0, State).
loop_parse_clause_kind(finally, Tokens, Rest, State0, State) :-
    loop_take_top_forms(Tokens, Forms, Rest),
    loop_require_forms(finally, Forms),
    loop_add_forms(final, Forms, State0, State).
loop_parse_clause_kind(while, [Test|Rest], Rest, State0, State) :-
    get_dict(finish, State0, Finish),
    loop_add_forms(body, [[unless, Test, [go, Finish]]], State0, State).
loop_parse_clause_kind(until, [Test|Rest], Rest, State0, State) :-
    get_dict(finish, State0, Finish),
    loop_add_forms(body, [[when, Test, [go, Finish]]], State0, State).
loop_parse_clause_kind(do, Tokens, Rest, State0, State) :-
    loop_take_top_forms(Tokens, Forms, Rest),
    loop_require_forms(do, Forms),
    loop_add_forms(body, Forms, State0, State).
loop_parse_clause_kind(doing, Tokens, Rest, State0, State) :-
    loop_take_top_forms(Tokens, Forms, Rest),
    loop_require_forms(doing, Forms),
    loop_add_forms(body, Forms, State0, State).
loop_parse_clause_kind(return, [Value|Rest], Rest, State0, State) :-
    get_dict(name, State0, Name),
    loop_add_forms(body, [[return_from, Name, Value]], State0, State).
loop_parse_clause_kind(Keyword, Tokens, Rest, State0, State) :-
    loop_accumulation_keyword_canonical(Keyword, Kind),
    loop_parse_accumulation(Kind, Tokens, Rest, State0, State1, Action),
    loop_add_forms(body, [Action], State1, State).
loop_parse_clause_kind(always, [Test|Rest], Rest, State0, State) :-
    get_dict(name, State0, Name),
    loop_set_result(t, State0, State1),
    loop_add_forms(body,
                   [[unless, Test, [return_from, Name, []]]],
                   State1,
                   State).
loop_parse_clause_kind(never, [Test|Rest], Rest, State0, State) :-
    get_dict(name, State0, Name),
    loop_set_result(t, State0, State1),
    loop_add_forms(body,
                   [[when, Test, [return_from, Name, []]]],
                   State1,
                   State).
loop_parse_clause_kind(thereis, [Form|Rest], Rest, State0, State) :-
    get_dict(name, State0, Name),
    gensym(loop_thereis_, Value),
    loop_set_result([], State0, State1),
    loop_add_forms(body,
                   [[let, [[Value, Form]],
                          [when, Value, [return_from, Name, Value]]]],
                   State1,
                   State).
loop_parse_clause_kind(Keyword, [Test|Tokens], Rest, State0, State) :-
    loop_conditional_keyword_canonical(Keyword, Sense),
    loop_parse_conditional(Sense,
                           Test,
                           Tokens,
                           Rest,
                           State0,
                           State1,
                           Conditional),
    loop_add_forms(body, [Conditional], State1, State).
loop_parse_clause_kind(Keyword, Tokens, _Rest, _State0, _State) :-
    loop_syntax_error(malformed_clause(Keyword), Tokens).


% ---------------------------------------------------------------------------
% WITH and iteration drivers
% ---------------------------------------------------------------------------

loop_parse_with(Tokens, Rest, State0, State) :-
    get_dict(bindings, State0, BindingsBefore),
    loop_parse_with_one(Tokens, Rest0, State0, State1),
    loop_parse_with_more(Rest0, Rest, State1, State2, 1, BindingCount),
    loop_parallelize_group_bindings(BindingCount,
                                    BindingsBefore,
                                    State2,
                                    State).

loop_parse_with_one([Var|Tokens0], Rest, State0, State) :-
    loop_validate_variable(Var),
    loop_optional_type(Tokens0, Type, Tokens1),
    loop_optional_initial_value(Tokens1, Type, Init, Rest),
    loop_add_new_binding(Var, Init, State0, State).
loop_parse_with_one(Tokens, _Rest, _State0, _State) :-
    loop_syntax_error(malformed_with_clause, Tokens).

loop_parse_with_more([Token|Tokens],
                     Rest,
                     State0,
                     State,
                     Count0,
                     Count) :-
    loop_token_is(Token, and),
    !,
    loop_parse_with_one(Tokens, Rest0, State0, State1),
    Count1 is Count0 + 1,
    loop_parse_with_more(Rest0, Rest, State1, State, Count1, Count).
loop_parse_with_more(Rest, Rest, State, State, Count, Count).

loop_optional_type([Token, Type|Rest], Type, Rest) :-
    loop_token_is(Token, of_type),
    !.
loop_optional_type([Token|Rest], Type, Rest) :-
    loop_bare_type(Token),
    loop_token_is(Token, Type),
    !.
loop_optional_type(Tokens, none, Tokens).

loop_bare_type(Token) :-
    loop_token_is(Token, Type),
    memberchk(Type, [fixnum, float, t, nil]).

loop_optional_initial_value(['=', Init|Rest], _Type, Init, Rest) :-
    !.
loop_optional_initial_value(Rest, Type, Init, Rest) :-
    loop_type_default(Type, Init).

loop_type_default(fixnum, 0) :- !.
loop_type_default(float, 0.0) :- !.
loop_type_default(Type, 0.0) :-
    loop_float_type(Type),!.
loop_type_default(Type, 0) :-
    loop_numeric_type(Type),!.
loop_type_default(_Type, []).

loop_float_type(Type) :-
    atom(Type),
    loop_token_is(Type, Canonical),
    memberchk(Canonical,
              [short_float, single_float, double_float, long_float]).

loop_numeric_type([Head|_]) :-
    !,
    loop_numeric_type(Head).
loop_numeric_type(Type) :-
    atom(Type),
    loop_token_is(Type, Canonical),
    memberchk(Canonical,
              [integer, fixnum, bignum, bit, number, real, rational,
               signed_byte, unsigned_byte, mod]).

loop_add_repeat(Count, State0, State) :-
    gensym(loop_repeat_limit_, Limit),
    gensym(loop_repeat_index_, Index),
    loop_add_hidden_binding(Limit, Count, State0, State1),
    loop_add_hidden_binding(Index, 0, State1, State2),
    loop_add_forms(checks, [[>=, Index, Limit]], State2, State3),
    loop_add_forms(steps, [[setq, Index, ['1+', Index]]], State3, State).

loop_parse_for(Tokens, Rest, State0, State) :-
    get_dict(steps, State0, StepsBefore),
    get_dict(bindings, State0, BindingsBefore),
    get_dict(source_vars, State0, SourceVarsBefore),
    get_dict(prefix, State0, PrefixBefore),
    get_dict(prefix_items, State0, PrefixItemsBefore),
    loop_parse_for_one(sequential, Tokens, Rest0, State0, State1),
    ( Rest0 = [AndToken|_],
      loop_token_is(AndToken, and)
    -> loop_parse_for_one(parallel, Tokens, ParallelRest0, State0, ParallelState1),
       loop_parse_for_more(parallel,
                           ParallelRest0,
                           Rest,
                           ParallelState1,
                           State2,
                           1,
                           DriverCount)
    ;  Rest = Rest0,
       State2 = State1,
       DriverCount = 1
    ),
    loop_parallelize_group_sources(DriverCount,
                                   BindingsBefore,
                                   SourceVarsBefore,
                                   State2,
                                   State3),
    loop_parallelize_group_prefixes(DriverCount,
                                    PrefixBefore,
                                    PrefixItemsBefore,
                                    State3,
                                    State4),
    loop_parallelize_group_steps(DriverCount, StepsBefore, State4, State).

loop_parse_for_one(Mode, [Var|Tokens0], Rest, State0, State) :-
    loop_validate_pattern(Var),
    loop_optional_type(Tokens0, _Type, Tokens),
    loop_parse_for_driver(Mode, Tokens, Rest, Var, State0, State).
loop_parse_for_one(_Mode, Tokens, _Rest, _State0, _State) :-
    loop_syntax_error(malformed_for_clause, Tokens).

loop_parse_for_more(Mode,
                    [Token|Tokens],
                    Rest,
                    State0,
                    State,
                    Count0,
                    Count) :-
    loop_token_is(Token, and),
    !,
    loop_parse_for_one(Mode, Tokens, Rest0, State0, State1),
    Count1 is Count0 + 1,
    loop_parse_for_more(Mode,
                        Rest0,
                        Rest,
                        State1,
                        State,
                        Count1,
                        Count).
loop_parse_for_more(_Mode, Rest, Rest, State, State, Count, Count).

loop_parse_for_driver(Mode, [Token, Sequence|Tokens0], Rest, Var, State0, State) :-
    loop_token_is(Token, in),
    !,
    loop_optional_by(Tokens0, By, Rest),
    gensym(loop_list_, Cursor),
    loop_add_source_binding(Cursor, Sequence, State0, State1),
    loop_add_pattern_driver(Mode, Var, [car, Cursor], State1, State2,
                            Item, Assignments, First),
    loop_add_forms(checks, [[endp, Cursor]], State2, State3),
    loop_sequence_step(By, Cursor, State3, State4, Step),
    loop_sequence_driver_steps(Mode, [endp, Cursor], [car, Cursor],
                               Item, Assignments, First,
                               [setq, Cursor, Step], Steps),
    loop_add_forms(steps, Steps, State4, State).
loop_parse_for_driver(Mode, [Token, Sequence|Tokens0], Rest, Var, State0, State) :-
    loop_token_is(Token, on),
    !,
    loop_optional_by(Tokens0, By, Rest),
    gensym(loop_list_, Cursor),
    loop_add_source_binding(Cursor, Sequence, State0, State1),
    loop_add_pattern_driver(Mode, Var, Cursor, State1, State2,
                            Item, Assignments, First),
    loop_add_forms(checks, [[atom, Cursor]], State2, State3),
    loop_sequence_step(By, Cursor, State3, State4, Step),
    loop_sequence_driver_steps(Mode, [atom, Cursor], Cursor,
                               Item, Assignments, First,
                               [setq, Cursor, Step], Steps),
    loop_add_forms(steps, Steps, State4, State).
loop_parse_for_driver(_Mode, ['=', Init, ThenToken, Step|Rest], Rest, Var, State0, State) :-
    atom(Var),
    loop_token_is(ThenToken, then),
    !,
    loop_add_source_binding(Var, Init, State0, State1),
    loop_add_visible_variable(Var, State1, State2),
    loop_add_forms(steps, [[setq, Var, Step]], State2, State).
loop_parse_for_driver(Mode, [Token, Sequence|Rest], Rest, Var, State0, State) :-
    loop_token_is(Token, across),
    !,
    gensym(loop_vector_, Vector),
    gensym(loop_index_, Index),
    gensym(loop_length_, Length),
    loop_add_source_binding(Vector, Sequence, State0, State1),
    loop_add_hidden_binding(Index, 0, State1, State2),
    loop_add_hidden_binding(Length, [length, Vector], State2, State3),
    loop_add_pattern_driver(Mode, Var, [aref, Vector, Index], State3, State4,
                            Item, Assignments, First),
    loop_add_forms(checks, [[>=, Index, Length]], State4, State5),
    loop_sequence_driver_steps(Mode, [>=, Index, Length],
                               [aref, Vector, Index], Item, Assignments,
                               First, [setq, Index, ['1+', Index]], Steps),
    loop_add_forms(steps, Steps, State5, State).
loop_parse_for_driver(Mode, ['=', Init, ThenToken, Step|Rest], Rest, Var, State0, State) :-
    loop_token_is(ThenToken, then),
    !,
    gensym(loop_current_, Current),
    loop_add_source_binding(Current, Init, State0, State1),
    loop_add_pattern_driver(Mode, Var, Current, State1, State2,
                            Item, Assignments, First),
    loop_pattern_driver_steps(Mode, Current, Step, Item, Assignments,
                              First, Steps),
    loop_add_forms(steps, Steps, State2, State).
loop_parse_for_driver(sequential, ['=', Init|Rest], Rest, Var, State0, State) :-
    !,
    loop_add_pattern_driver(sequential, Var, Init, State0, State1,
                            Item, Assignments, First),
    append([[setq, Item, Init]|Assignments],
           [[setq, First, []]],
           Steps),
    loop_add_forms(steps, Steps, State1, State).
loop_parse_for_driver(parallel, ['=', Init|Rest], Rest, Var, State0, State) :-
    !,
    loop_parse_for_driver(parallel,
                          ['=', Init, then, Init|Rest],
                          Rest,
                          Var,
                          State0,
                          State).
loop_parse_for_driver(_Mode, Tokens0, Rest, Var, State0, State) :-
    loop_validate_variable(Var),
    loop_parse_numeric_driver(Tokens0, Rest, Var, State0, State),
    !.
loop_parse_for_driver(_Mode, Tokens, _Rest, Var, _State0, _State) :-
    loop_syntax_error(malformed_for_driver(Var), Tokens).

loop_optional_by([Token, Function|Rest], Function, Rest) :-
    loop_token_is(Token, by),
    !.
loop_optional_by(Rest, default, Rest).

loop_sequence_step(default, Cursor, State, State, [cdr, Cursor]).
loop_sequence_step(Function, Cursor, State0, State,
                   [funcall, FunctionVar, Cursor]) :-
    gensym(loop_step_function_, FunctionVar),
    loop_add_source_binding(FunctionVar, Function, State0, State).

loop_parallelize_group_steps(Count, _StepsBefore, State, State) :-
    Count =< 1,
    !.
loop_parallelize_group_steps(_Count, StepsBefore, State0, State) :-
    get_dict(steps, State0, AllSteps),
    append(StepsBefore, GroupSteps, AllSteps),
    loop_step_pairs(GroupSteps, Pairs),
    ( Pairs == []
    -> ParallelSteps = []
    ;  ParallelSteps = [[psetq|Pairs]]
    ),
    append(StepsBefore, ParallelSteps, Steps),
    put_dict(steps, State0, Steps, State).

loop_step_pairs([], []).
loop_step_pairs([[setq, Var, Form]|Steps], [Var, Form|Pairs]) :-
    loop_step_pairs(Steps, Pairs).

loop_parallelize_group_sources(Count,
                               _BindingsBefore,
                               _SourceVarsBefore,
                               State,
                               State) :-
    Count =< 1,
    !.
loop_parallelize_group_sources(_Count,
                               BindingsBefore,
                               SourceVarsBefore,
                               State0,
                               State) :-
    get_dict(bindings, State0, AllBindings),
    get_dict(source_vars, State0, AllSourceVars),
    get_dict(variables, State0, Variables),
    append(BindingsBefore, GroupBindings, AllBindings),
    append(SourceVarsBefore, GroupSourceVars, AllSourceVars),
    loop_parallel_source_bindings(GroupBindings,
                                  GroupSourceVars,
                                  Variables,
                                  SourceBindings,
                                  Bridges),
    loop_parallel_dependent_bindings(GroupBindings,
                                     GroupSourceVars,
                                     Bridges,
                                     DependentBindings),
    append(SourceBindings, DependentBindings, ParallelBindings),
    append(BindingsBefore, ParallelBindings, Bindings),
    put_dict(bindings, State0, Bindings, State).

loop_parallelize_group_prefixes(Count,
                                _PrefixBefore,
                                _PrefixItemsBefore,
                                State,
                                State) :-
    Count =< 1,
    !.
loop_parallelize_group_prefixes(_Count,
                                PrefixBefore,
                                PrefixItemsBefore,
                                State0,
                                State) :-
    get_dict(prefix, State0, AllPrefix),
    get_dict(prefix_items, State0, AllPrefixItems),
    append(PrefixBefore, GroupPrefix, AllPrefix),
    append(PrefixItemsBefore, GroupItems, AllPrefixItems),
    loop_partition_prefix_assignments(GroupPrefix,
                                      GroupItems,
                                      SourcePairs,
                                      DependentPrefix),
    ( SourcePairs == []
    -> ParallelSource = []
    ;  ParallelSource = [[psetq|SourcePairs]]
    ),
    append(ParallelSource, DependentPrefix, ParallelPrefix),
    append(PrefixBefore, ParallelPrefix, Prefix),
    put_dict(prefix, State0, Prefix, State).

loop_partition_prefix_assignments([], _Items, [], []).
loop_partition_prefix_assignments([[setq, Var, Form]|Prefix],
                                  Items,
                                  SourcePairs,
                                  DependentPrefix) :-
    memberchk(Var, Items),
    !,
    SourcePairs = [Var, Form|PairRest],
    loop_partition_prefix_assignments(Prefix,
                                      Items,
                                      PairRest,
                                      DependentPrefix).
loop_partition_prefix_assignments([Form|Prefix],
                                  Items,
                                  SourcePairs,
                                  [Form|DependentPrefix]) :-
    loop_partition_prefix_assignments(Prefix,
                                      Items,
                                      SourcePairs,
                                      DependentPrefix).

loop_parallel_source_bindings([], _Sources, _Variables, [], []).
loop_parallel_source_bindings([[Var, Initial]|Bindings],
                              Sources,
                              Variables,
                              SourceBindings,
                              Bridges) :-
    ( memberchk(Var, Sources)
    -> ( memberchk(Var, Variables)
       -> gensym(loop_for_value_, Temporary),
          SourceBindings = [[Temporary, Initial]|SourceRest],
          Bridges = [bridge(Var, Temporary)|BridgeRest]
       ;  SourceBindings = [[Var, Initial]|SourceRest],
          Bridges = BridgeRest
       )
    ;  SourceBindings = SourceRest,
       Bridges = BridgeRest
    ),
    loop_parallel_source_bindings(Bindings,
                                  Sources,
                                  Variables,
                                  SourceRest,
                                  BridgeRest).

loop_parallel_dependent_bindings([], _Sources, _Bridges, []).
loop_parallel_dependent_bindings([[Var, Initial]|Bindings],
                                 Sources,
                                 Bridges,
                                 DependentBindings) :-
    ( memberchk(bridge(Var, Temporary), Bridges)
    -> DependentBindings = [[Var, Temporary]|Rest]
    ; memberchk(Var, Sources)
    -> DependentBindings = Rest
    ;  DependentBindings = [[Var, Initial]|Rest]
    ),
    loop_parallel_dependent_bindings(Bindings,
                                     Sources,
                                     Bridges,
                                     Rest).

loop_parallelize_group_bindings(Count, _BindingsBefore, State, State) :-
    Count =< 1,
    !.
loop_parallelize_group_bindings(_Count, BindingsBefore, State0, State) :-
    get_dict(bindings, State0, AllBindings),
    append(BindingsBefore, GroupBindings, AllBindings),
    maplist(loop_parallel_binding,
            GroupBindings,
            TemporaryBindings,
            VariableBindings),
    append(TemporaryBindings, VariableBindings, ParallelBindings),
    append(BindingsBefore, ParallelBindings, Bindings),
    put_dict(bindings, State0, Bindings, State).

loop_parallel_binding([Var, Initial],
                      [Temporary, Initial],
                      [Var, Temporary]) :-
    gensym(loop_with_value_, Temporary).

loop_add_pattern_driver(Mode, Pattern, ValueForm, State0, State,
                        Item, Assignments, First) :-
    gensym(loop_item_, Item),
    loop_pattern_assignments(Pattern, Item, Variables, Assignments),
    loop_add_pattern_bindings(Variables, State0, State1),
    loop_add_hidden_binding(Item, [], State1, State2),
    loop_add_first_binding(Mode, State2, State3, First),
    get_dict(prefix_items, State2, PrefixItems),
    append(PrefixItems, [Item], UpdatedPrefixItems),
    put_dict(prefix_items, State3, UpdatedPrefixItems, State4),
    loop_initial_pattern_forms(Mode, First, Item, ValueForm,
                               Assignments, PrefixForms),
    loop_add_forms(prefix,
                   PrefixForms,
                   State4,
                   State).

loop_add_first_binding(parallel, State, State, none).
loop_add_first_binding(sequential, State0, State, First) :-
    gensym(loop_first_, First),
    loop_add_hidden_binding(First, t, State0, State).

loop_initial_pattern_forms(parallel, _First, Item, Value,
                           Assignments,
                           [[setq, Item, Value]|Assignments]).
loop_initial_pattern_forms(sequential, First, Item, Value,
                           Assignments,
                           [[when, First,
                             [progn, [setq, Item, Value]|Assignments]]]).

loop_sequence_driver_steps(parallel, _Termination, _Value,
                           _Item, _Assignments, _First, StateStep,
                           [StateStep]).
loop_sequence_driver_steps(sequential, Termination, Value,
                           Item, Assignments, First, StateStep,
                           [StateStep,
                            [unless, Termination,
                             [progn, [setq, Item, Value]|Assignments]],
                            [setq, First, []]]).

loop_pattern_driver_steps(parallel, Current, Step,
                          _Item, _Assignments, _First,
                          [[setq, Current, Step]]).
loop_pattern_driver_steps(sequential, Current, Step,
                          Item, Assignments, First, Steps) :-
    append([[setq, Current, Step], [setq, Item, Current]],
           Assignments,
           Steps0),
    append(Steps0,
           [[setq, First, []]],
           Steps).

loop_add_pattern_bindings([], State, State).
loop_add_pattern_bindings([Var|Vars], State0, State) :-
    loop_add_new_binding(Var, [], State0, State1),
    loop_add_pattern_bindings(Vars, State1, State).

loop_pattern_assignments([], _Value, [], []) :-
    !.
loop_pattern_assignments(Var, Value, [Var], [[setq, Var, Value]]) :-
    atom(Var),
    !.
loop_pattern_assignments([Head|Tail], Value, Variables, Assignments) :-
    !,
    loop_pattern_assignments(Head, [car, Value], HeadVars, HeadAssignments),
    loop_pattern_assignments(Tail, [cdr, Value], TailVars, TailAssignments),
    append(HeadVars, TailVars, Variables),
    append(HeadAssignments, TailAssignments, Assignments).
loop_pattern_assignments(Pattern, _Value, _Variables, _Assignments) :-
    loop_syntax_error(invalid_destructuring_pattern, Pattern).

loop_parse_numeric_driver(Tokens0, Rest, Var, State0, State) :-
    loop_collect_numeric_phrases(Tokens0, Rest, Phrases),
    Phrases = [_|_],
    loop_validate_numeric_phrases(Phrases,
                                  StartSpec,
                                  EndSpec,
                                  StepSpec,
                                  Direction),
    loop_materialize_numeric_phrases(Phrases,
                                     State0,
                                     State1,
                                     Values),
    loop_numeric_start_value(StartSpec, Values, Start),
    loop_numeric_end_value(EndSpec, Values, Direction, End),
    loop_numeric_step_value(StepSpec, Values, Step),
    loop_add_new_binding(Var, Start, State1, State2),
    loop_add_numeric_end(End, Var, State2, State3),
    loop_add_numeric_step(Direction, Step, Var, State3, State).

loop_collect_numeric_phrases([Token, Form|Tokens],
                             Rest,
                             [phrase(Category, Kind, Form)|Phrases]) :-
    loop_numeric_phrase(Token, Category, Kind),
    !,
    loop_collect_numeric_phrases(Tokens, Rest, Phrases).
loop_collect_numeric_phrases([Token], _Rest, _Phrases) :-
    loop_numeric_phrase(Token, _Category, _Kind),
    !,
    loop_syntax_error(numeric_phrase_requires_value, Token).
loop_collect_numeric_phrases(Rest, Rest, []).

loop_numeric_phrase(Token, start, neutral) :-
    loop_token_is(Token, from).
loop_numeric_phrase(Token, start, up) :-
    loop_token_is(Token, upfrom).
loop_numeric_phrase(Token, start, down) :-
    loop_token_is(Token, downfrom).
loop_numeric_phrase(Token, end, to) :-
    loop_token_is(Token, to).
loop_numeric_phrase(Token, end, upto) :-
    loop_token_is(Token, upto).
loop_numeric_phrase(Token, end, below) :-
    loop_token_is(Token, below).
loop_numeric_phrase(Token, end, downto) :-
    loop_token_is(Token, downto).
loop_numeric_phrase(Token, end, above) :-
    loop_token_is(Token, above).
loop_numeric_phrase(Token, step, by) :-
    loop_token_is(Token, by).

loop_validate_numeric_phrases(Phrases,
                              StartSpec,
                              EndSpec,
                              StepSpec,
                              Direction) :-
    loop_unique_numeric_phrase(start, Phrases, StartSpec),
    loop_unique_numeric_phrase(end, Phrases, EndSpec),
    loop_unique_numeric_phrase(step, Phrases, StepSpec),
    loop_numeric_direction(StartSpec, EndSpec, Direction).

loop_unique_numeric_phrase(Category, Phrases, Spec) :-
    findall(Kind,
            member(phrase(Category, Kind, _Form), Phrases),
            Kinds),
    ( Kinds == []
    -> Spec = none
    ; Kinds = [Kind]
    -> Spec = some(Kind)
    ;  loop_syntax_error(duplicate_numeric_phrase(Category), Phrases)
    ).

loop_numeric_direction(StartSpec, some(to), Direction) :-
    !,
    loop_direction_from_start(StartSpec, Direction).
loop_numeric_direction(StartSpec, some(EndKind), Direction) :-
    !,
    loop_end_direction(EndKind, Direction),
    loop_validate_start_direction(StartSpec, Direction).
loop_numeric_direction(some(down), none, down) :-
    !.
loop_numeric_direction(_StartSpec, none, up).

loop_direction_from_start(some(down), down) :- !.
loop_direction_from_start(_StartSpec, up).

loop_end_direction(upto, up).
loop_end_direction(below, up).
loop_end_direction(downto, down).
loop_end_direction(above, down).

loop_validate_start_direction(none, down) :-
    !,
    loop_syntax_error(downward_iteration_requires_start, down).
loop_validate_start_direction(some(up), down) :-
    !,
    loop_syntax_error(conflicting_numeric_directions, up_down).
loop_validate_start_direction(some(down), up) :-
    !,
    loop_syntax_error(conflicting_numeric_directions, down_up).
loop_validate_start_direction(_StartSpec, _Direction).

loop_materialize_numeric_phrases([], State, State, []).
loop_materialize_numeric_phrases([phrase(Category, Kind, Form)|Phrases],
                                 State0,
                                 State,
                                 [value(Category, Kind, Value)|Values]) :-
    gensym(loop_numeric_, Value),
    loop_add_source_binding(Value, Form, State0, State1),
    loop_materialize_numeric_phrases(Phrases, State1, State, Values).

loop_numeric_start_value(none, _Values, 0).
loop_numeric_start_value(some(_Kind), Values, Start) :-
    memberchk(value(start, _StartKind, Start), Values).

loop_numeric_end_value(none, _Values, _Direction, none).
loop_numeric_end_value(some(to), Values, Direction, end(EffectiveKind, Limit)) :-
    !,
    memberchk(value(end, to, Limit), Values),
    ( Direction == down -> EffectiveKind = downto ; EffectiveKind = to ).
loop_numeric_end_value(some(Kind), Values, _Direction, end(Kind, Limit)) :-
    memberchk(value(end, Kind, Limit), Values).

loop_numeric_step_value(none, _Values, 1).
loop_numeric_step_value(some(by), Values, Step) :-
    memberchk(value(step, by, Step), Values).

loop_add_numeric_end(none, _Var, State, State).
loop_add_numeric_end(end(Kind, Limit), Var, State0, State) :-
    loop_numeric_exit_test(Kind, Var, Limit, Test),
    loop_add_forms(checks, [Test], State0, State).

loop_numeric_exit_test(to, Var, Limit, [>, Var, Limit]).
loop_numeric_exit_test(upto, Var, Limit, [>, Var, Limit]).
loop_numeric_exit_test(below, Var, Limit, [>=, Var, Limit]).
loop_numeric_exit_test(downto, Var, Limit, [<, Var, Limit]).
loop_numeric_exit_test(above, Var, Limit, ['<=', Var, Limit]).

loop_add_numeric_step(Direction, Step, Var, State0, State) :-
    ( Direction == down
    -> Next = [-, Var, Step]
    ;  Next = [+, Var, Step]
    ),
    loop_add_forms(steps, [[setq, Var, Next]], State0, State).


% ---------------------------------------------------------------------------
% Accumulation and conditional clauses
% ---------------------------------------------------------------------------

loop_parse_accumulation(Kind,
                        [Expression|Tokens0],
                        Rest,
                        State0,
                        State,
                        Action) :-
    loop_optional_into(Tokens0, Destination, Rest),
    loop_accumulator(Kind, Destination, State0, State, Var, Extra),
    loop_accumulation_action(Kind, Var, Extra, Expression, Action).
loop_parse_accumulation(Kind, Tokens, _Rest, _State0, _State, _Action) :-
    loop_syntax_error(malformed_accumulation(Kind), Tokens).

loop_optional_into([Token, Var|Rest], explicit(Var), Rest) :-
    loop_token_is(Token, into),
    !,
    loop_validate_variable(Var).
loop_optional_into(Rest, implicit, Rest).

loop_accumulator(Kind, implicit, State0, State, Var, Extra) :-
    loop_accumulator_family(Kind, Family),
    get_dict(default_acc, State0, Default),
    ( Default == none
    -> gensym(loop_result_, Var),
       loop_create_accumulator(Kind, Var, State0, State1, Extra),
       put_dict(default_acc, State1, acc(Family, Var), State2),
       loop_set_result(Var, State2, State)
    ; Default = acc(Family, Var)
    -> loop_find_accumulator(Var, State0, Extra),
       State = State0
    ;  loop_syntax_error(incompatible_implicit_accumulators, Kind)
    ).
loop_accumulator(Kind, explicit(Var), State0, State, Var, Extra) :-
    get_dict(accumulators, State0, Accumulators),
    ( memberchk(acc(ExistingKind, Var, Extra0), Accumulators)
    -> ( loop_accumulator_family(ExistingKind, Family),
         loop_accumulator_family(Kind, Family)
       -> Extra = Extra0,
          State = State0
       ;  loop_syntax_error(incompatible_accumulator(Var), Kind)
       )
    ;  loop_create_accumulator(Kind, Var, State0, State, Extra)
    ).

loop_create_accumulator(Kind, Var, State0, State, Extra) :-
    loop_accumulator_initial(Kind, Initial, Extra0),
    loop_ensure_binding(Var, Initial, State0, State1),
    loop_add_accumulator_extra(Extra0, State1, State2, Extra),
    get_dict(accumulators, State2, Accumulators),
    put_dict(accumulators,
             State2,
             [acc(Kind, Var, Extra)|Accumulators],
             State).

loop_accumulator_initial(collect, [], none).
loop_accumulator_initial(append, [], none).
loop_accumulator_initial(nconc, [], none).
loop_accumulator_initial(sum, 0, none).
loop_accumulator_initial(count, 0, none).
loop_accumulator_initial(maximize, [], seen).
loop_accumulator_initial(minimize, [], seen).

loop_accumulator_family(collect, list).
loop_accumulator_family(append, list).
loop_accumulator_family(nconc, list).
loop_accumulator_family(sum, numeric).
loop_accumulator_family(count, numeric).
loop_accumulator_family(maximize, extreme).
loop_accumulator_family(minimize, extreme).

loop_add_accumulator_extra(none, State, State, none).
loop_add_accumulator_extra(seen, State0, State, seen(Seen)) :-
    gensym(loop_seen_, Seen),
    loop_add_hidden_binding(Seen, [], State0, State).

loop_find_accumulator(Var, State, Extra) :-
    get_dict(accumulators, State, Accumulators),
    memberchk(acc(_Kind, Var, Extra), Accumulators).

loop_accumulation_action(collect, Var, _Extra, Expression,
                         [setq, Var, [append, Var, [list, Expression]]]).
loop_accumulation_action(append, Var, _Extra, Expression,
                         [setq, Var, [append, Var, Expression]]).
loop_accumulation_action(nconc, Var, _Extra, Expression,
                         [setq, Var, [nconc, Var, Expression]]).
loop_accumulation_action(sum, Var, _Extra, Expression,
                         [setq, Var, [+, Var, Expression]]).
loop_accumulation_action(count, Var, _Extra, Expression,
                         [when, Expression, [setq, Var, ['1+', Var]]]).
loop_accumulation_action(maximize, Var, seen(Seen), Expression, Action) :-
    loop_extreme_action(>, Var, Seen, Expression, Action).
loop_accumulation_action(minimize, Var, seen(Seen), Expression, Action) :-
    loop_extreme_action(<, Var, Seen, Expression, Action).

loop_extreme_action(Comparison, Var, Seen, Expression,
                    [let, [[Value, Expression]],
                          [if, Seen,
                               [when, [Comparison, Value, Var],
                                      [setq, Var, Value]],
                               [progn,
                                [setq, Var, Value],
                                [setq, Seen, t]]]]) :-
    gensym(loop_value_, Value).

loop_parse_conditional_actions(Tokens,
                               Rest,
                               State0,
                               State,
                               Forms) :-
    loop_parse_action(Tokens, Rest0, State0, State1, FirstForms),
    loop_parse_more_conditional_actions(Rest0,
                                        Rest,
                                        State1,
                                        State,
                                        MoreForms),
    append(FirstForms, MoreForms, Forms).

loop_parse_conditional(Sense,
                       Test,
                       Tokens,
                       Rest,
                       State0,
                       State,
                       Conditional) :-
    loop_parse_conditional_actions(Tokens,
                                   AfterThen,
                                   State0,
                                   State1,
                                   ThenForms),
    loop_parse_else(AfterThen,
                    Rest0,
                    State1,
                    State2,
                    ElseForms),
    loop_optional_end(Rest0, Rest),
    loop_conditional_form(Sense, Test, ThenForms, ElseForms, Conditional),
    State = State2.

loop_parse_more_conditional_actions([Token|Tokens],
                                    Rest,
                                    State0,
                                    State,
                                    Forms) :-
    loop_token_is(Token, and),
    !,
    loop_parse_action(Tokens, Rest0, State0, State1, FirstForms),
    loop_parse_more_conditional_actions(Rest0,
                                        Rest,
                                        State1,
                                        State,
                                        MoreForms),
    append(FirstForms, MoreForms, Forms).
loop_parse_more_conditional_actions(Rest, Rest, State, State, []).

loop_parse_action([Token|Tokens], Rest, State, State, Forms) :-
    loop_token_is(Token, do),
    !,
    loop_take_conditional_forms(Tokens, Forms, Rest),
    loop_require_forms(do, Forms).
loop_parse_action([Token|Tokens], Rest, State, State, Forms) :-
    loop_token_is(Token, doing),
    !,
    loop_take_conditional_forms(Tokens, Forms, Rest),
    loop_require_forms(doing, Forms).
loop_parse_action([Token, Value|Rest], Rest, State, State,
                  [[return_from, Name, Value]]) :-
    loop_token_is(Token, return),
    !,
    get_dict(name, State, Name).
loop_parse_action([Token, Test|Tokens], Rest, State0, State, [Conditional]) :-
    loop_token_is(Token, Keyword),
    loop_conditional_keyword_canonical(Keyword, Sense),
    !,
    loop_parse_conditional(Sense,
                           Test,
                           Tokens,
                           Rest,
                           State0,
                           State,
                           Conditional).
loop_parse_action([Token|Tokens], Rest, State0, State, [Action]) :-
    loop_token_is(Token, Keyword),
    loop_accumulation_keyword_canonical(Keyword, Kind),
    !,
    loop_parse_accumulation(Kind, Tokens, Rest, State0, State, Action).
loop_parse_action(Tokens, _Rest, _State0, _State, _Forms) :-
    loop_syntax_error(expected_conditional_action, Tokens).

loop_parse_else([Token|Tokens], Rest, State0, State, ElseForms) :-
    loop_token_is(Token, else),
    !,
    loop_parse_conditional_actions(Tokens, Rest, State0, State, ElseForms).
loop_parse_else(Rest, Rest, State, State, []).

loop_optional_end([Token|Rest], Rest) :-
    loop_token_is(Token, end),
    !.
loop_optional_end(Rest, Rest).

loop_conditional_form(Sense, Test, ThenForms0, ElseForms0, Form) :-
    ( ( loop_forms_contain_it(ThenForms0)
      ; loop_forms_contain_it(ElseForms0)
      )
    -> loop_conditional_form_with_it(Sense,
                                     Test,
                                     ThenForms0,
                                     ElseForms0,
                                     Form)
    ;  loop_conditional_branches(Sense,
                                 Test,
                                 ThenForms0,
                                 ElseForms0,
                                 Form)
    ).

loop_conditional_form_with_it(Sense, Test, ThenForms0, ElseForms0,
                              [let, [[It, Test]], Conditional]) :-
    gensym(loop_it_, It),
    maplist(loop_replace_it(It), ThenForms0, ThenForms),
    maplist(loop_replace_it(It), ElseForms0, ElseForms),
    loop_conditional_branches(Sense,
                              It,
                              ThenForms,
                              ElseForms,
                              Conditional).

loop_forms_contain_it([Form|_]) :-
    loop_form_contains_it(Form),
    !.
loop_forms_contain_it([_|Forms]) :-
    loop_forms_contain_it(Forms).

loop_form_contains_it([quote|_]) :-
    !,
    fail.
loop_form_contains_it([function|_]) :-
    !,
    fail.
loop_form_contains_it([lambda|_]) :-
    !,
    fail.
loop_form_contains_it([loop|_]) :-
    !,
    fail.
loop_form_contains_it(Symbol) :-
    atom(Symbol),
    loop_token_is(Symbol, it),
    !.
loop_form_contains_it([Head|Tail]) :-
    ( loop_form_contains_it(Head)
    ; member(Form, Tail),
      loop_form_contains_it(Form)
    ).

loop_conditional_branches(positive, It, ThenForms, [],
                          [when, It, [progn|ThenForms]]).
loop_conditional_branches(positive, It, ThenForms, ElseForms,
                          [if, It, [progn|ThenForms], [progn|ElseForms]]).
loop_conditional_branches(negative, It, ThenForms, [],
                          [unless, It, [progn|ThenForms]]).
loop_conditional_branches(negative, It, ThenForms, ElseForms,
                          [if, It, [progn|ElseForms], [progn|ThenForms]]).


% ---------------------------------------------------------------------------
% State helpers
% ---------------------------------------------------------------------------

loop_add_forms(Key, Forms, State0, State) :-
    get_dict(Key, State0, Existing),
    append(Existing, Forms, Updated),
    put_dict(Key, State0, Updated, State).

loop_add_new_binding(Var, Initial, State0, State) :-
    get_dict(variables, State0, Variables),
    ( memberchk(Var, Variables)
    -> loop_syntax_error(duplicate_loop_variable, Var)
    ;  get_dict(bindings, State0, Bindings),
       append(Bindings, [[Var, Initial]], UpdatedBindings),
       append(Variables, [Var], UpdatedVariables),
       put_dict(_{bindings:UpdatedBindings, variables:UpdatedVariables},
                State0,
                State)
    ).

loop_add_hidden_binding(Var, Initial, State0, State) :-
    get_dict(bindings, State0, Bindings),
    append(Bindings, [[Var, Initial]], UpdatedBindings),
    put_dict(bindings, State0, UpdatedBindings, State).

loop_add_source_binding(Var, Initial, State0, State) :-
    loop_add_hidden_binding(Var, Initial, State0, State1),
    get_dict(source_vars, State1, SourceVars),
    append(SourceVars, [Var], UpdatedSourceVars),
    put_dict(source_vars, State1, UpdatedSourceVars, State).

loop_add_visible_variable(Var, State0, State) :-
    get_dict(variables, State0, Variables),
    ( memberchk(Var, Variables)
    -> loop_syntax_error(duplicate_loop_variable, Var)
    ;  append(Variables, [Var], UpdatedVariables),
       put_dict(variables, State0, UpdatedVariables, State)
    ).

loop_ensure_binding(Var, Initial, State0, State) :-
    get_dict(variables, State0, Variables),
    ( memberchk(Var, Variables)
    -> State = State0
    ;  loop_add_new_binding(Var, Initial, State0, State)
    ).

loop_set_result(Result, State0, State) :-
    get_dict(result_set, State0, IsSet),
    ( IsSet == false
    -> put_dict(_{result:Result, result_set:true}, State0, State)
    ;  get_dict(result, State0, Existing),
       ( Existing == Result
       -> State = State0
       ;  loop_syntax_error(conflicting_loop_result, Result)
       )
    ).


% ---------------------------------------------------------------------------
% Token and syntax helpers
% ---------------------------------------------------------------------------

loop_take_top_forms([Token|Tokens], [], [Token|Tokens]) :-
    atom(Token),
    loop_clause_keyword(Token),
    !.
loop_take_top_forms([Form|Tokens], [Form|Forms], Rest) :-
    !,
    loop_take_top_forms(Tokens, Forms, Rest).
loop_take_top_forms([], [], []).

loop_take_conditional_forms([Token|Tokens], [], [Token|Tokens]) :-
    atom(Token),
    loop_conditional_delimiter(Token),
    !.
loop_take_conditional_forms([Form|Tokens], [Form|Forms], Rest) :-
    !,
    loop_take_conditional_forms(Tokens, Forms, Rest).
loop_take_conditional_forms([], [], []).

loop_require_forms(_Clause, [_|_]) :-
    !.
loop_require_forms(Clause, []) :-
    loop_syntax_error(clause_requires_body, Clause).

loop_validate_variable(Var) :-
    atom(Var),
    \+ loop_clause_keyword(Var),
    !.
loop_validate_variable(Var) :-
    loop_syntax_error(expected_variable, Var).

loop_validate_pattern(Var) :-
    atom(Var),
    !,
    loop_validate_variable(Var).
loop_validate_pattern([]) :-
    !.
loop_validate_pattern([Head|Tail]) :-
    !,
    loop_validate_pattern(Head),
    loop_validate_pattern(Tail).
loop_validate_pattern(Pattern) :-
    loop_syntax_error(expected_variable_or_pattern, Pattern).

loop_rewrite_form(_Finish, [quote|Rest], [quote|Rest]) :-
    !.
loop_rewrite_form(_Finish, [function|Rest], [function|Rest]) :-
    !.
loop_rewrite_form(_Finish, [lambda|Rest], [lambda|Rest]) :-
    !.
loop_rewrite_form(_Finish, [loop|Rest], [loop|Rest]) :-
    !.
loop_rewrite_form(Finish, ['#BQ', Template], ['#BQ', Rewritten]) :-
    !,
    loop_rewrite_backquote(Finish, 1, Template, Rewritten).
loop_rewrite_form(Finish, [Operator], [go, Finish]) :-
    loop_finish_symbol(Operator),
    !.
loop_rewrite_form(Finish, [Head|Tail], [NewHead|NewTail]) :-
    !,
    loop_rewrite_form(Finish, Head, NewHead),
    maplist(loop_rewrite_form(Finish), Tail, NewTail).
loop_rewrite_form(_Finish, Form, Form).

loop_replace_it(_Value, [quote|Rest], [quote|Rest]) :-
    !.
loop_replace_it(_Value, [function|Rest], [function|Rest]) :-
    !.
loop_replace_it(_Value, [lambda|Rest], [lambda|Rest]) :-
    !.
loop_replace_it(_Value, [loop|Rest], [loop|Rest]) :-
    !.
loop_replace_it(Value, ['#BQ', Template], ['#BQ', Rewritten]) :-
    !,
    loop_replace_it_backquote(Value, 1, Template, Rewritten).
loop_replace_it(Value, Symbol, Value) :-
    atom(Symbol),
    loop_token_is(Symbol, it),
    !.
loop_replace_it(Value, [Head|Tail], [NewHead|NewTail]) :-
    !,
    loop_replace_it(Value, Head, NewHead),
    maplist(loop_replace_it(Value), Tail, NewTail).
loop_replace_it(_Value, Form, Form).

loop_rewrite_backquote(Finish, Depth, ['#BQ', Template],
                       ['#BQ', Rewritten]) :-
    !,
    NextDepth is Depth + 1,
    loop_rewrite_backquote(Finish, NextDepth, Template, Rewritten).
loop_rewrite_backquote(Finish, 1, [Operator, Form],
                       [Operator, Rewritten]) :-
    loop_comma_operator(Operator),
    !,
    loop_rewrite_form(Finish, Form, Rewritten).
loop_rewrite_backquote(Finish, Depth, [Operator, Form],
                       [Operator, Rewritten]) :-
    loop_comma_operator(Operator),
    !,
    NextDepth is Depth - 1,
    loop_rewrite_backquote(Finish, NextDepth, Form, Rewritten).
loop_rewrite_backquote(Finish, Depth, [Head|Tail],
                       [NewHead|NewTail]) :-
    !,
    loop_rewrite_backquote(Finish, Depth, Head, NewHead),
    maplist(loop_rewrite_backquote(Finish, Depth), Tail, NewTail).
loop_rewrite_backquote(_Finish, _Depth, Form, Form).

loop_replace_it_backquote(Value, Depth, ['#BQ', Template],
                          ['#BQ', Rewritten]) :-
    !,
    NextDepth is Depth + 1,
    loop_replace_it_backquote(Value, NextDepth, Template, Rewritten).
loop_replace_it_backquote(Value, 1, [Operator, Form],
                          [Operator, Rewritten]) :-
    loop_comma_operator(Operator),
    !,
    loop_replace_it(Value, Form, Rewritten).
loop_replace_it_backquote(Value, Depth, [Operator, Form],
                          [Operator, Rewritten]) :-
    loop_comma_operator(Operator),
    !,
    NextDepth is Depth - 1,
    loop_replace_it_backquote(Value, NextDepth, Form, Rewritten).
loop_replace_it_backquote(Value, Depth, [Head|Tail],
                          [NewHead|NewTail]) :-
    !,
    loop_replace_it_backquote(Value, Depth, Head, NewHead),
    maplist(loop_replace_it_backquote(Value, Depth), Tail, NewTail).
loop_replace_it_backquote(_Value, _Depth, Form, Form).

loop_comma_operator('#COMMA').
loop_comma_operator('#BQ-COMMA-ELIPSE').

loop_finish_symbol(loop_finish).
loop_finish_symbol('loop-finish').
loop_finish_symbol(u_loop_finish).
loop_finish_symbol(sys_loop_finish).

% LOOP keywords are compared by print name, not package identity.  The reader
% represents non-CL symbols as u_* (CL-USER) or sys_* (SYSTEM), while some
% words such as DO and RETURN are inherited CL symbols.
loop_token_is(Token, Canonical) :-
    atom(Token),
    atom_concat(u_, Canonical, Token),
    !.
loop_token_is(Token, Canonical) :-
    atom(Token),
    atom_concat(sys_, Canonical, Token),
    !.
loop_token_is(Token, Canonical) :-
    atom(Token),
    atom_concat(kw_, Canonical, Token),
    !.
loop_token_is(Token, Canonical) :-
    atom(Token),
    catch(get_opv(Token, symbol_name, Name), _, fail),
    prologcase_name(Name, Canonical),
    !.
loop_token_is(Token, Token) :-
    atom(Token).

loop_clause_keyword(Token) :-
    loop_token_is(Token, Canonical),
    loop_canonical_clause_keyword(Canonical).

loop_canonical_clause_keyword(named).
loop_canonical_clause_keyword(with).
loop_canonical_clause_keyword(repeat).
loop_canonical_clause_keyword(for).
loop_canonical_clause_keyword(as).
loop_canonical_clause_keyword(initially).
loop_canonical_clause_keyword(finally).
loop_canonical_clause_keyword(while).
loop_canonical_clause_keyword(until).
loop_canonical_clause_keyword(do).
loop_canonical_clause_keyword(doing).
loop_canonical_clause_keyword(return).
loop_canonical_clause_keyword(collect).
loop_canonical_clause_keyword(collecting).
loop_canonical_clause_keyword(append).
loop_canonical_clause_keyword(appending).
loop_canonical_clause_keyword(nconc).
loop_canonical_clause_keyword(nconcing).
loop_canonical_clause_keyword(sum).
loop_canonical_clause_keyword(summing).
loop_canonical_clause_keyword(count).
loop_canonical_clause_keyword(counting).
loop_canonical_clause_keyword(maximize).
loop_canonical_clause_keyword(maximizing).
loop_canonical_clause_keyword(minimize).
loop_canonical_clause_keyword(minimizing).
loop_canonical_clause_keyword(always).
loop_canonical_clause_keyword(never).
loop_canonical_clause_keyword(thereis).
loop_canonical_clause_keyword(when).
loop_canonical_clause_keyword(unless).
loop_canonical_clause_keyword(if).

loop_conditional_delimiter(Token) :-
    loop_token_is(Token, Canonical),
    memberchk(Canonical, [and, else, end]),
    !.
loop_conditional_delimiter(Token) :-
    loop_clause_keyword(Token).

loop_conditional_keyword_canonical(when, positive).
loop_conditional_keyword_canonical(if, positive).
loop_conditional_keyword_canonical(unless, negative).

loop_accumulation_keyword_canonical(collect, collect).
loop_accumulation_keyword_canonical(collecting, collect).
loop_accumulation_keyword_canonical(append, append).
loop_accumulation_keyword_canonical(appending, append).
loop_accumulation_keyword_canonical(nconc, nconc).
loop_accumulation_keyword_canonical(nconcing, nconc).
loop_accumulation_keyword_canonical(sum, sum).
loop_accumulation_keyword_canonical(summing, sum).
loop_accumulation_keyword_canonical(count, count).
loop_accumulation_keyword_canonical(counting, count).
loop_accumulation_keyword_canonical(maximize, maximize).
loop_accumulation_keyword_canonical(maximizing, maximize).
loop_accumulation_keyword_canonical(minimize, minimize).
loop_accumulation_keyword_canonical(minimizing, minimize).

loop_syntax_error(Reason, Form) :-
    throw(error(syntax_error(loop(Reason, Form)),
                context(loop/1, 'Invalid or unsupported LOOP syntax'))).

:- fixup_exports.

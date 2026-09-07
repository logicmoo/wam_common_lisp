/*******************************************************************
 *
 * Frozen phase-A practical Common Lisp LOOP macro expansion.
 * Reconstructed from the 2026-09-07 phase-A edit sequence; the unadapted
 * snapshot SHA-256 is F93CDC6F7DC52F651A10BA273744549C95EE2B483310D3547F7BCD41B89EC4DF.
 *
 * Expands LOOP forms into the WAM-CL primitives LET*, BLOCK, TAGBODY,
 * GO, SETQ and ordinary function calls.  Keeping the implementation at
 * macro-expansion time avoids adding another evaluator/compiler path.
 *
 *******************************************************************/

:- module(loop_version_a, []).
:- set_module(class(library)).
:- include('./header').

:- discontiguous(loop_parse_clause/4).
:- discontiguous(loop_parse_action/5).

% Private snapshot entry point; CL:LOOP remains owned by loop.pl.
expand_practical_loop(Whole, Expansion) :-
    expand_practical_loop(Whole, [], Expansion).

expand_practical_loop([loop|Clauses], _Env, Expansion) :-
    !,
    once(loop_expand(Clauses, Expansion)).
expand_practical_loop(Form, _Env, _Expansion) :-
    loop_syntax_error(expected_loop_form, Form).

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
    gensym(loop_start_, Start),
    gensym(loop_finish_, Finish),
    maplist(loop_rewrite_form(Finish), Forms0, Forms),
    append([[label, Start]|Forms],
           [[go, Start], [label, Finish]],
           TagBody),
    Expansion = [block, [], [tagbody|TagBody], []].

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
                       accumulators:[]
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
    append([[label, Start]|Checks], Prefix, Tag0),
    append(Tag0, Body, Tag1),
    append(Tag1, Steps, Tag2),
    append(Tag2, [[go, Start], [label, Finish]], TagBody),
    append(Initial, [[tagbody|TagBody]|Final], LetBody0),
    append(LetBody0, [Result], LetBody),
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
    loop_parse_with_one(Tokens, Rest0, State0, State1),
    loop_parse_with_more(Rest0, Rest, State1, State).

loop_parse_with_one([Var|Tokens0], Rest, State0, State) :-
    loop_validate_variable(Var),
    loop_optional_type(Tokens0, Tokens1),
    loop_optional_initial_value(Tokens1, Init, Rest),
    loop_add_new_binding(Var, Init, State0, State).
loop_parse_with_one(Tokens, _Rest, _State0, _State) :-
    loop_syntax_error(malformed_with_clause, Tokens).

loop_parse_with_more([Token|Tokens], Rest, State0, State) :-
    loop_token_is(Token, and),
    !,
    loop_parse_with_one(Tokens, Rest0, State0, State1),
    loop_parse_with_more(Rest0, Rest, State1, State).
loop_parse_with_more(Rest, Rest, State, State).

loop_optional_type([Token, _Type|Rest], Rest) :-
    loop_token_is(Token, of_type),
    !.
loop_optional_type(Tokens, Tokens).

loop_optional_initial_value(['=', Init|Rest], Init, Rest) :-
    !.
loop_optional_initial_value(Rest, [], Rest).

loop_add_repeat(Count, State0, State) :-
    gensym(loop_repeat_limit_, Limit),
    gensym(loop_repeat_index_, Index),
    loop_add_hidden_binding(Limit, Count, State0, State1),
    loop_add_hidden_binding(Index, 0, State1, State2),
    loop_add_forms(checks, [[>=, Index, Limit]], State2, State3),
    loop_add_forms(steps, [[setq, Index, ['1+', Index]]], State3, State).

loop_parse_for(Tokens, Rest, State0, State) :-
    loop_parse_for_one(Tokens, Rest0, State0, State1),
    loop_parse_for_more(Rest0, Rest, State1, State).

loop_parse_for_one([Var|Tokens0], Rest, State0, State) :-
    loop_validate_pattern(Var),
    loop_optional_type(Tokens0, Tokens),
    loop_parse_for_driver(Tokens, Rest, Var, State0, State).
loop_parse_for_one(Tokens, _Rest, _State0, _State) :-
    loop_syntax_error(malformed_for_clause, Tokens).

loop_parse_for_more([Token|Tokens], Rest, State0, State) :-
    loop_token_is(Token, and),
    !,
    loop_parse_for_one(Tokens, Rest0, State0, State1),
    loop_parse_for_more(Rest0, Rest, State1, State).
loop_parse_for_more(Rest, Rest, State, State).

loop_parse_for_driver([Token, Sequence|Tokens0], Rest, Var, State0, State) :-
    loop_token_is(Token, in),
    !,
    loop_optional_by(Tokens0, By, Rest),
    gensym(loop_list_, Cursor),
    loop_add_hidden_binding(Cursor, Sequence, State0, State1),
    loop_add_pattern_driver(Var, [car, Cursor], State1, State2),
    loop_add_forms(checks, [[endp, Cursor]], State2, State3),
    loop_sequence_step(By, Cursor, Step),
    loop_add_forms(steps, [[setq, Cursor, Step]], State3, State).
loop_parse_for_driver([Token, Sequence|Tokens0], Rest, Var, State0, State) :-
    loop_token_is(Token, on),
    !,
    loop_optional_by(Tokens0, By, Rest),
    gensym(loop_list_, Cursor),
    loop_add_hidden_binding(Cursor, Sequence, State0, State1),
    loop_add_pattern_driver(Var, Cursor, State1, State2),
    loop_add_forms(checks, [[endp, Cursor]], State2, State3),
    loop_sequence_step(By, Cursor, Step),
    loop_add_forms(steps, [[setq, Cursor, Step]], State3, State).
loop_parse_for_driver([Token, Sequence|Rest], Rest, Var, State0, State) :-
    loop_token_is(Token, across),
    !,
    gensym(loop_vector_, Vector),
    gensym(loop_index_, Index),
    gensym(loop_length_, Length),
    loop_add_hidden_binding(Vector, Sequence, State0, State1),
    loop_add_hidden_binding(Index, 0, State1, State2),
    loop_add_hidden_binding(Length, [length, Vector], State2, State3),
    loop_add_pattern_driver(Var, [aref, Vector, Index], State3, State4),
    loop_add_forms(checks, [[>=, Index, Length]], State4, State5),
    loop_add_forms(steps, [[setq, Index, ['1+', Index]]], State5, State).
loop_parse_for_driver(['=', Init, ThenToken, Step|Rest], Rest, Var, State0, State) :-
    loop_token_is(ThenToken, then),
    !,
    gensym(loop_current_, Current),
    loop_add_hidden_binding(Current, Init, State0, State1),
    loop_add_pattern_driver(Var, Current, State1, State2),
    loop_add_forms(steps, [[setq, Current, Step]], State2, State).
loop_parse_for_driver(['=', Init|Rest], Rest, Var, State0, State) :-
    !,
    loop_add_pattern_driver(Var, Init, State0, State).
loop_parse_for_driver(Tokens0, Rest, Var, State0, State) :-
    loop_validate_variable(Var),
    loop_parse_numeric_driver(Tokens0, Rest, Var, State0, State),
    !.
loop_parse_for_driver(Tokens, _Rest, Var, _State0, _State) :-
    loop_syntax_error(malformed_for_driver(Var), Tokens).

loop_optional_by([Token, Function|Rest], Function, Rest) :-
    loop_token_is(Token, by),
    !.
loop_optional_by(Rest, default, Rest).

loop_sequence_step(default, Cursor, [cdr, Cursor]).
loop_sequence_step(Function, Cursor, [funcall, Function, Cursor]).

loop_add_pattern_driver(Pattern, ValueForm, State0, State) :-
    gensym(loop_item_, Item),
    loop_pattern_assignments(Pattern, Item, Variables, Assignments),
    loop_add_pattern_bindings(Variables, State0, State1),
    loop_add_hidden_binding(Item, [], State1, State2),
    loop_add_forms(prefix,
                   [[setq, Item, ValueForm]|Assignments],
                   State2,
                   State).

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
    loop_numeric_start(Tokens0, Start, Direction0, Tokens1),
    loop_numeric_end(Tokens1, End, Direction0, Direction, Tokens2),
    loop_numeric_by(Tokens2, By, Rest),
    loop_add_new_binding(Var, Start, State0, State1),
    loop_add_numeric_end(End, Var, State1, State2),
    loop_add_numeric_step(Direction, By, Var, State2, State).

loop_numeric_start([Token, Start|Rest], Start, up, Rest) :-
    loop_token_is(Token, from).
loop_numeric_start([Token, Start|Rest], Start, up, Rest) :-
    loop_token_is(Token, upfrom).
loop_numeric_start([Token, Start|Rest], Start, down, Rest) :-
    loop_token_is(Token, downfrom).
loop_numeric_start(Tokens, 0, Direction, Tokens) :-
    Tokens = [Keyword|_],
    loop_numeric_end_keyword(Keyword, Direction).

loop_numeric_end([Token, End|Rest], end(to, End), _Direction0, up, Rest) :-
    loop_token_is(Token, to).
loop_numeric_end([Token, End|Rest], end(to, End), _Direction0, up, Rest) :-
    loop_token_is(Token, upto).
loop_numeric_end([Token, End|Rest], end(below, End), _Direction0, up, Rest) :-
    loop_token_is(Token, below).
loop_numeric_end([Token, End|Rest], end(downto, End), _Direction0, down, Rest) :-
    loop_token_is(Token, downto).
loop_numeric_end([Token, End|Rest], end(above, End), _Direction0, down, Rest) :-
    loop_token_is(Token, above).
loop_numeric_end(Rest, none, Direction, Direction, Rest).

loop_numeric_end_keyword(Token, up) :-
    loop_token_is(Token, to).
loop_numeric_end_keyword(Token, up) :-
    loop_token_is(Token, upto).
loop_numeric_end_keyword(Token, up) :-
    loop_token_is(Token, below).
loop_numeric_end_keyword(Token, down) :-
    loop_token_is(Token, downto).
loop_numeric_end_keyword(Token, down) :-
    loop_token_is(Token, above).

loop_numeric_by([Token, Step|Rest], Step, Rest) :-
    loop_token_is(Token, by),
    !.
loop_numeric_by(Rest, 1, Rest).

loop_add_numeric_end(none, _Var, State, State).
loop_add_numeric_end(end(Kind, Form), Var, State0, State) :-
    gensym(loop_limit_, Limit),
    loop_add_hidden_binding(Limit, Form, State0, State1),
    loop_numeric_exit_test(Kind, Var, Limit, Test),
    loop_add_forms(checks, [Test], State1, State).

loop_numeric_exit_test(to, Var, Limit, [>, Var, Limit]).
loop_numeric_exit_test(below, Var, Limit, [>=, Var, Limit]).
loop_numeric_exit_test(downto, Var, Limit, [<, Var, Limit]).
loop_numeric_exit_test(above, Var, Limit, ['<=', Var, Limit]).

loop_add_numeric_step(Direction, StepForm, Var, State0, State) :-
    ( StepForm == 1
    -> Step = 1,
       State1 = State0
    ;  gensym(loop_step_, Step),
       loop_add_hidden_binding(Step, StepForm, State0, State1)
    ),
    ( Direction == down
    -> Next = [-, Var, Step]
    ;  Next = [+, Var, Step]
    ),
    loop_add_forms(steps, [[setq, Var, Next]], State1, State).


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
    get_dict(default_acc, State0, Default),
    ( Default == none
    -> gensym(loop_result_, Var),
       loop_create_accumulator(Kind, Var, State0, State1, Extra),
       put_dict(default_acc, State1, acc(Kind, Var), State2),
       loop_set_result(Var, State2, State)
    ; Default = acc(Kind, Var)
    -> loop_find_accumulator(Var, State0, Extra),
       State = State0
    ;  loop_syntax_error(incompatible_implicit_accumulators, Kind)
    ).
loop_accumulator(Kind, explicit(Var), State0, State, Var, Extra) :-
    get_dict(accumulators, State0, Accumulators),
    ( memberchk(acc(ExistingKind, Var, Extra0), Accumulators)
    -> ( ExistingKind == Kind
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

loop_conditional_form(positive, Test, ThenForms, [],
                      [when, Test, [progn|ThenForms]]).
loop_conditional_form(positive, Test, ThenForms, ElseForms,
                      [if, Test, [progn|ThenForms], [progn|ElseForms]]).
loop_conditional_form(negative, Test, ThenForms, [],
                      [unless, Test, [progn|ThenForms]]).
loop_conditional_form(negative, Test, ThenForms, ElseForms,
                      [if, Test, [progn|ElseForms], [progn|ThenForms]]).


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
loop_rewrite_form(Finish, [loop_finish], [go, Finish]) :-
    !.
loop_rewrite_form(Finish, [Head|Tail], [NewHead|NewTail]) :-
    !,
    loop_rewrite_form(Finish, Head, NewHead),
    maplist(loop_rewrite_form(Finish), Tail, NewTail).
loop_rewrite_form(_Finish, Form, Form).

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

% Deliberately private.  CL:LOOP uses the current implementation; the public
% wrapper SYS::PRACTICAL-LOOP calls this module explicitly.

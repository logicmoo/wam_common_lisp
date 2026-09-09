/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. 
 *
 *******************************************************************/
:- module(errs, []).



:- include('./header').


:- meta_predicate wamcl_with_handler_frame(+, 0).
:- meta_predicate wamcl_handler_case(0, ?, +, +, -).
:- meta_predicate wamcl_restart_case(0, ?, +, -).

wl:declared_as(f_error,inline(error)).
wl:init_args(0,error).
wl:init_args(1,make_condition).
wl:init_args(1,signal).
wl:init_args(1,warn).
wl:init_args(0,muffle_warning).
wl:init_args(1,find_restart).
wl:init_args(1,invoke_restart).
wl:init_args(0,compute_restarts).

mf_ignore_errors([ignore_errors|Forms],_Env,
                 [sys_ignore_errors|Forms]).
mf_handler_bind([handler_bind,Bindings|Forms],_Env,
                [sys_handler_bind,Bindings|Forms]).
mf_handler_case([handler_case,Form|Clauses],_Env,
                [sys_handler_case,Form|Clauses]).
mf_restart_case([restart_case,Form|Clauses],_Env,
                [sys_restart_case,Form|Clauses]).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [sys_ignore_errors|Forms],_PreviousResult,
    catch((nb_linkval('$mv_return',[Result]),Body),
          Error,wamcl_ignore_error(Error,Result))):-
  must_compile_body(Ctx,Env,Result,[progn|Forms],Body).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [sys_handler_bind,Bindings|Forms],_PreviousResult,
    (BindingCode,wamcl_with_handler_frame(Handlers,BodyCode))):-
  compile_handler_bindings(Ctx,Env,Bindings,Handlers,BindingCode),
  must_compile_body(Ctx,Env,Result,[progn|Forms],BodyCode).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [HandlerBind,Bindings|Forms],_PreviousResult,
    (BindingCode,wamcl_with_handler_frame(Handlers,BodyCode))):-
  same_symbol_names(HandlerBind,handler_bind),
  compile_handler_bindings(Ctx,Env,Bindings,Handlers,BindingCode),
  must_compile_body(Ctx,Env,Result,[progn|Forms],BodyCode).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [sys_handler_case,Protected|Clauses],_PreviousResult,
    (ClauseCode,
     wamcl_handler_case(ProtectedCode,ProtectedResult,
                        Cases,NoError,Result))):-
  compile_handler_case_clauses(Ctx,Env,Clauses,Cases,NoError,ClauseCode),
  must_compile_body(Ctx,Env,ProtectedResult,Protected,ProtectedCode).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [HandlerCase,Protected|Clauses],_PreviousResult,
    (ClauseCode,
     wamcl_handler_case(ProtectedCode,ProtectedResult,
                        Cases,NoError,Result))):-
  same_symbol_names(HandlerCase,handler_case),
  compile_handler_case_clauses(Ctx,Env,Clauses,Cases,NoError,ClauseCode),
  must_compile_body(Ctx,Env,ProtectedResult,Protected,ProtectedCode).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [sys_restart_case,Protected|Clauses],_PreviousResult,
    (ClauseCode,
     wamcl_restart_case(ProtectedCode,ProtectedResult,Restarts,Result))):-
  compile_restart_clauses(Ctx,Env,Clauses,Restarts,ClauseCode),
  must_compile_body(Ctx,Env,ProtectedResult,Protected,ProtectedCode).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,
    [RestartCase,Protected|Clauses],_PreviousResult,
    (ClauseCode,
     wamcl_restart_case(ProtectedCode,ProtectedResult,Restarts,Result))):-
  same_symbol_names(RestartCase,restart_case),
  compile_restart_clauses(Ctx,Env,Clauses,Restarts,ClauseCode),
  must_compile_body(Ctx,Env,ProtectedResult,Protected,ProtectedCode).

compile_handler_bindings(_Ctx,_Env,[],[],true).
compile_handler_bindings(Ctx,Env,[[Type,HandlerForm]|Bindings],
                         [handler(Type,Handler)|Handlers],
                         (HandlerCode,RestCode)):-
  must_compile_body(Ctx,Env,Handler,HandlerForm,HandlerCode),
  compile_handler_bindings(Ctx,Env,Bindings,Handlers,RestCode).

compile_handler_case_clauses(_Ctx,_Env,[],[],none,true).
compile_handler_case_clauses(Ctx,Env,
                             [[Type,Variables|Body]|Clauses],
                             Cases,NoError,RestCode):-
  compile_condition_clause(Ctx,Env,Variables,Body,Clause),
  ( Type==kw_no_error
  -> Cases=RestCases,
     NoError=no_error(Clause)
  ;  Cases=[condition_case(Type,Clause)|RestCases],
     NoError=RestNoError
  ),
  compile_handler_case_clauses(Ctx,Env,Clauses,
                               RestCases,RestNoError,RestCode).

compile_condition_clause(Ctx,Env,Variables,Body,
                         compiled_clause(Values,Code,ClauseResult)):-
  make_condition_clause_env(Variables,Values,Env,ClauseEnv),
  must_compile_body(Ctx,ClauseEnv,ClauseResult,[progn|Body],Code).

make_condition_clause_env([],[],Env,Env).
make_condition_clause_env([Variable|Variables],[Value|Values],Env,
                          [bv(Variable,Value)|ClauseEnv]):-
  make_condition_clause_env(Variables,Values,Env,ClauseEnv).

compile_restart_clauses(_Ctx,_Env,[],[],true).
compile_restart_clauses(Ctx,Env,[[Name,Variables|Body0]|Clauses],
                        [restart_spec(Name,Clause)|Restarts],
                        RestCode):-
  strip_restart_options(Body0,Body),
  compile_condition_clause(Ctx,Env,Variables,Body,Clause),
  compile_restart_clauses(Ctx,Env,Clauses,Restarts,RestCode).

strip_restart_options([Key,_Value|Rest],Body):-
  memberchk(Key,[kw_interactive,kw_report,kw_test]),!,
  strip_restart_options(Rest,Body).
strip_restart_options(Body,Body).

f_make_condition(Type,InitArgs,Condition):-
  wamcl_condition_class(Type,Class),
  Condition='$OBJ'(Class,wamcl_condition(InitArgs)).

wamcl_condition_class(Class,Class):-
  atom(Class),
  atom_concat(claz_,_,Class),
  is_subclass(Class,claz_condition),!.
wamcl_condition_class(Type,Class):-
  find_class(Type,Class),
  is_subclass(Class,claz_condition),!.
wamcl_condition_class(Type,_):-
  throw(error(type_error(condition_type,Type),make_condition)).

is_condition_object(Condition):-
  f_class_of(Condition,Class),
  is_subclass(Class,claz_condition).

wamcl_condition_slot('$OBJ'(_,wamcl_condition(InitArgs)),Key,Default,Value):-
  ( condition_plist_value(InitArgs,Key,Value) -> true ; Value=Default ).

condition_plist_value([Found,Value|_],Key,Value):-
  same_symbol(Found,Key),!.
condition_plist_value([_,_|Rest],Key,Value):-
  condition_plist_value(Rest,Key,Value).

f_simple_condition_format_control(Condition,Value):-
  wamcl_condition_slot(Condition,kw_format_control,[],Value).
f_simple_condition_format_arguments(Condition,Value):-
  wamcl_condition_slot(Condition,kw_format_arguments,[],Value).
f_type_error_datum(Condition,Value):-
  wamcl_condition_slot(Condition,kw_datum,[],Value).
f_type_error_expected_type(Condition,Value):-
  wamcl_condition_slot(Condition,kw_expected_type,[],Value).
f_cell_error_name(Condition,Value):-
  wamcl_condition_slot(Condition,kw_name,[],Value).
f_stream_error_stream(Condition,Value):-
  wamcl_condition_slot(Condition,kw_stream,[],Value).
f_file_error_pathname(Condition,Value):-
  wamcl_condition_slot(Condition,kw_pathname,[],Value).
f_package_error_package(Condition,Value):-
  wamcl_condition_slot(Condition,kw_package,[],Value).
f_print_not_readable_object(Condition,Value):-
  wamcl_condition_slot(Condition,kw_object,[],Value).
f_unbound_slot_instance(Condition,Value):-
  wamcl_condition_slot(Condition,kw_instance,[],Value).
f_arithmetic_error_operation(Condition,Value):-
  wamcl_condition_slot(Condition,kw_operation,[],Value).
f_arithmetic_error_operands(Condition,Value):-
  wamcl_condition_slot(Condition,kw_operands,[],Value).

f_error(Args,_Result):-
  wamcl_error_condition(Args,Condition),
  wamcl_invoke_handlers(Condition),
  throw(wamcl_condition(Condition)).

wamcl_error_condition([],Condition):-
  f_make_condition(program_error,[],Condition).
wamcl_error_condition([Datum|Arguments],Condition):-
  wamcl_designator_condition(simple_error,Datum,Arguments,Condition).

f_signal(Datum,Arguments,[]):-
  wamcl_designator_condition(simple_condition,Datum,Arguments,Condition),
  wamcl_invoke_handlers(Condition).

f_warn(Datum,Arguments,[]):-
  wamcl_designator_condition(simple_warning,Datum,Arguments,Condition),
  wamcl_warn_condition(Condition).

wamcl_designator_condition(_DefaultType,Datum,_Arguments,Datum):-
  is_condition_object(Datum),!.
wamcl_designator_condition(_DefaultType,Datum,Arguments,Condition):-
  is_symbolp(Datum),!,
  f_make_condition(Datum,Arguments,Condition).
wamcl_designator_condition(DefaultType,Datum,Arguments,Condition):-
  ( is_stringp(Datum) ; is_functionp(Datum) ),!,
  f_make_condition(DefaultType,
                   [kw_format_control,Datum,
                    kw_format_arguments,Arguments],
                   Condition).
wamcl_designator_condition(DefaultType,Datum,Arguments,Condition):-
  f_make_condition(DefaultType,
                   [kw_format_control,Datum,
                    kw_format_arguments,Arguments],
                   Condition).

wamcl_current_handlers(Handlers):-
  ( nb_current('$wamcl_handler_frames',Handlers) -> true ; Handlers=[] ).

wamcl_with_handler_frame(Handlers,Goal):-
  wamcl_current_handlers(OldHandlers),
  setup_call_cleanup(
      b_setval('$wamcl_handler_frames',[handler_frame(Handlers)|OldHandlers]),
      once(Goal),
      b_setval('$wamcl_handler_frames',OldHandlers)).

wamcl_invoke_handlers(Condition):-
  wamcl_current_handlers(Frames),
  wamcl_invoke_handler_frames(Frames,Condition).

wamcl_invoke_handler_frames([],_Condition).
wamcl_invoke_handler_frames([handler_frame(Handlers)|Outer],Condition):-
  wamcl_invoke_handler_list(Handlers,Outer,Condition),
  wamcl_invoke_handler_frames(Outer,Condition).

wamcl_invoke_handler_list([],_Outer,_Condition).
wamcl_invoke_handler_list([handler(Type,Function)|Handlers],Outer,Condition):-
  ( wamcl_condition_typep(Condition,Type)
  -> wamcl_call_handler(Outer,Function,Condition)
  ;  true
  ),
  wamcl_invoke_handler_list(Handlers,Outer,Condition).

wamcl_call_handler(Outer,wamcl_handler_case_transfer(Token),Condition):-
  wamcl_current_handlers(Active),
  setup_call_cleanup(
      b_setval('$wamcl_handler_frames',Outer),
      throw(wamcl_handler_case_transfer(Token,Condition)),
      b_setval('$wamcl_handler_frames',Active)).
wamcl_call_handler(Outer,Function,Condition):-
  wamcl_current_handlers(Active),
  nb_current('$mv_return',SavedValues),
  setup_call_cleanup(
      b_setval('$wamcl_handler_frames',Outer),
      ( f_funcall(Function,[Condition],_HandlerResult),
        Returned=t
      ),
      b_setval('$wamcl_handler_frames',Active)),
  Returned==t,
  nb_linkval('$mv_return',SavedValues).

wamcl_condition_typep(Condition,Type):-
  f_class_of(Condition,ConditionClass),
  wamcl_typespec_class(Type,TypeClass),
  is_subclass(ConditionClass,TypeClass).
wamcl_condition_typep(Condition,[not,Type]):-
  \+ wamcl_condition_typep(Condition,Type).
wamcl_condition_typep(Condition,[or|Types]):-
  member(Type,Types),
  wamcl_condition_typep(Condition,Type),!.
wamcl_condition_typep(Condition,[and|Types]):-
  \+ (member(Type,Types),\+ wamcl_condition_typep(Condition,Type)).

wamcl_typespec_class(Class,Class):-
  atom(Class),atom_concat(claz_,_,Class),!.
wamcl_typespec_class(Type,Class):-
  find_class(Type,Class).

wamcl_handler_case(ProtectedGoal,ProtectedResult,Cases,NoError,Result):-
  gensym(wamcl_handler_case_,Token),
  wamcl_handler_case_handlers(Cases,Token,Handlers),
  catch(wamcl_with_handler_frame(
           Handlers,
           ( nb_linkval('$mv_return',[ProtectedResult]),
             once(ProtectedGoal),
             nb_current('$mv_return',RawValues),
             wamcl_result_values(ProtectedResult,RawValues,Values),
             Outcome=normal(ProtectedResult,Values)
           )),
        Error,
        wamcl_handler_case_caught(Error,Token,Outcome)),
  wamcl_handler_case_outcome(Outcome,Cases,NoError,Result).

wamcl_handler_case_handlers([],_,[]).
wamcl_handler_case_handlers([condition_case(Type,_)|Cases],Token,
                          [handler(Type,wamcl_handler_case_transfer(Token))|Handlers]):-
  wamcl_handler_case_handlers(Cases,Token,Handlers).

wamcl_handler_case_caught(wamcl_handler_case_transfer(CaughtToken,Condition),
                         Token,condition(Condition)):-
  CaughtToken==Token,!.
wamcl_handler_case_caught(Error,_Token,_Outcome):-
  Error=wamcl_handler_case_transfer(_,_),!,
  throw(Error).
wamcl_handler_case_caught(Error,_Token,_Outcome):-
  wamcl_control_transfer(Error),!,
  throw(Error).
wamcl_handler_case_caught(Error,_Token,error(Error)).

wamcl_handler_case_outcome(normal(ProtectedResult,Values),_Cases,NoError,Result):-
  wamcl_handler_case_normal(NoError,ProtectedResult,Values,Result).
wamcl_handler_case_outcome(condition(Condition),Cases,_NoError,Result):-
  wamcl_handler_case_error(wamcl_condition(Condition),Cases,Result).
wamcl_handler_case_outcome(error(Error),Cases,_NoError,Result):-
  wamcl_handler_case_error(Error,Cases,Result).

wamcl_handler_case_normal(none,ProtectedResult,Values,ProtectedResult):-
  wamcl_restore_values(Values,ProtectedResult).
wamcl_handler_case_normal(no_error(Clause),_ProtectedResult,Values,Result):-
  wamcl_run_compiled_clause(Clause,Values,Result).

wamcl_handler_case_error(Error,_Cases,_Result):-
  wamcl_control_transfer(Error),!,
  throw(Error).
wamcl_handler_case_error(Error,Cases,Result):-
  wamcl_exception_condition(Error,Condition),
  ( member(condition_case(Type,Clause),Cases),
    wamcl_condition_typep(Condition,Type)
  -> Clause=compiled_clause(Values,_Code,_ClauseResult),
     ( Values==[] -> Arguments=[] ; Arguments=[Condition] ),
     wamcl_run_compiled_clause(Clause,Arguments,Result)
  ;  throw(Error)
  ).

wamcl_run_compiled_clause(compiled_clause(Values,Code,ClauseResult),
                          Arguments,Result):-
  Values=Arguments,
  nb_linkval('$mv_return',[ClauseResult]),
  once(user:Code),
  Result=ClauseResult.

wamcl_exception_condition(wamcl_condition(Condition),Condition):-!.
wamcl_exception_condition(error(type_error(Expected,Datum),_Context),Condition):-!,
  f_make_condition(type_error,
                   [kw_datum,Datum,kw_expected_type,Expected],Condition).
wamcl_exception_condition(error(existence_error(procedure,Name),_Context),
                          Condition):-!,
  f_make_condition(program_error,[kw_name,Name],Condition).
wamcl_exception_condition(lpa_throw(100,Name),Condition):-!,
  f_make_condition(unbound_variable,[kw_name,Name],Condition).
wamcl_exception_condition(Error,Condition):-
  f_make_condition(simple_error,
                   [kw_format_control,Error,kw_format_arguments,[]],
                   Condition).

wamcl_ignore_error(Error,_Result):-
  wamcl_control_transfer(Error),!,
  throw(Error).
wamcl_ignore_error(Error,Result):-
  wamcl_exception_condition(Error,Condition),
  f_values_list([[],Condition],Result).

wamcl_current_restarts(Restarts):-
  ( nb_current('$wamcl_restart_frames',Restarts) -> true ; Restarts=[] ).

wamcl_restart_case(ProtectedGoal,ProtectedResult,RestartSpecs,Result):-
  gensym(wamcl_restart_,Token),
  maplist(wamcl_make_restart(Token),RestartSpecs,Restarts),
  wamcl_current_restarts(OldRestarts),
  setup_call_cleanup(
      b_setval('$wamcl_restart_frames',[restart_frame(Restarts)|OldRestarts]),
      catch(( nb_linkval('$mv_return',[ProtectedResult]),
              once(ProtectedGoal),
              nb_current('$mv_return',RawValues),
              wamcl_result_values(ProtectedResult,RawValues,Values),
              Outcome=normal(ProtectedResult,Values)
            ),
            wamcl_restart(Token,Function,Arguments),
            Outcome=restart(Function,Arguments)),
      b_setval('$wamcl_restart_frames',OldRestarts)),
  wamcl_finish_restart_case(Outcome,Result).

wamcl_make_restart(Token,restart_spec(Name,Clause),
                   '$OBJ'(claz_restart,
                          wamcl_restart(Name,Token,Clause))).

wamcl_finish_restart_case(normal(Result,Values),Result):-
  wamcl_restore_values(Values,Result).
wamcl_finish_restart_case(restart(Clause,Arguments),Result):-
  wamcl_run_compiled_clause(Clause,Arguments,Result).

wamcl_result_values([],[],[]):-!.
wamcl_result_values(Result,[First|Rest],[First|Rest]):- First==Result,!.
wamcl_result_values(Result,_RawValues,[Result]).

f_restart_name('$OBJ'(claz_restart,wamcl_restart(Name,_Token,_Clause)),Name).
f_restartp(Restart,Result):-
  t_or_nil(Restart='$OBJ'(claz_restart,wamcl_restart(_,_,_)),Result).

f_compute_restarts(_Options,Restarts):-
  wamcl_current_restarts(Frames),
  wamcl_restart_frame_objects(Frames,Restarts).

wamcl_restart_frame_objects([],[]).
wamcl_restart_frame_objects([restart_frame(Frame)|Frames],Restarts):-
  wamcl_restart_frame_objects(Frames,OuterRestarts),
  append(Frame,OuterRestarts,Restarts).

f_find_restart(Identifier,_Options,Restart):-
  wamcl_current_restarts(Frames),
  ( member(restart_frame(Frame),Frames),
    member(Candidate,Frame),
    wamcl_restart_matches(Identifier,Candidate)
  -> Restart=Candidate
  ;  Restart=[]
  ).

wamcl_restart_matches(Restart,Candidate):- Restart==Candidate,!.
wamcl_restart_matches('$OBJ'(claz_restart,wamcl_restart(_,Token,_)),
                      '$OBJ'(claz_restart,wamcl_restart(_,CandidateToken,_))):-
  Token==CandidateToken,!.
wamcl_restart_matches(Identifier,
                      '$OBJ'(claz_restart,
                             wamcl_restart(Name,_Token,_Clause))):-
  same_symbol(Identifier,Name).

f_invoke_restart(Identifier,Arguments,_Result):-
  f_find_restart(Identifier,[],Restart),
  Restart='$OBJ'(claz_restart,wamcl_restart(_Name,Token,Clause)),
  throw(wamcl_restart(Token,Clause,Arguments)).

f_muffle_warning(_Arguments,_Result):-
  f_find_restart(muffle_warning,[],Restart),
  f_invoke_restart(Restart,[],_).

wamcl_warn_condition(Condition):-
  gensym(wamcl_restart_,Token),
  Restart='$OBJ'(claz_restart,
                 wamcl_restart(muffle_warning,Token,wamcl_muffle_warning)),
  wamcl_current_restarts(OldRestarts),
  setup_call_cleanup(
      b_setval('$wamcl_restart_frames',
               [restart_frame([Restart])|OldRestarts]),
      catch((wamcl_invoke_handlers(Condition),Outcome=unmuffled),
            wamcl_restart(Token,wamcl_muffle_warning,[]),
            Outcome=muffled),
      b_setval('$wamcl_restart_frames',OldRestarts)),
  ( Outcome==unmuffled
  -> format(user_error,'~NWarning: ~q~n',[Condition])
  ;  true
  ).

wamcl_control_transfer(block_exit(_,_)).
wamcl_control_transfer(goto(_,_)).
wamcl_control_transfer(goto(_,_,_)).
wamcl_control_transfer(goto(_,_,_,_)).
wamcl_control_transfer(lisp_throw(_,_)).
wamcl_control_transfer(wamcl_restart(_,_,_)).
wamcl_control_transfer('$aborted').

% Connection to LPA's built-in error handler

'?ERROR?'(Error, Form):-
	lisp_error_description(_, Error, Description),
	!,
	write('LISP ERROR  '),
	write(Description),
	write(Form),
	nl.
'?ERROR?'(Error, Goal):-
	wl:error_hook(Error, Goal).

lisp_error_description(unbound_atom,        100, 'No value found for atom: ').
lisp_error_description(atom_does_not_exist, 101, 'SetQ: Variable does not exist: ').
lisp_error_description(first_not_cons,      102, 'Form1: This is not a cons cell: ').
lisp_error_description(rest_not_cons,       103, 'Rest: This is not a cons cell: ').



:- fixup_exports.

      
end_of_file.
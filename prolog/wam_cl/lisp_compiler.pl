/*******************************************************************
 *
 * A Lisp compiler, written in Prolog
 *
 * (lisp_compiler.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 * Changes since 2001:
 *
 *  ..............................
 *
 *
 * Neil''s Notes:
 *
 * (c) Neil Smith, 2001
 *
 * This program, and its associated support files, forms a compiler
 * for a subset of the language LISP.  It supports a few simple
 * built-in procedures, listed below.  It also supports both special
 * and lexical variables, and higher-order functions and lexical
 * closures.
 *
 * This compiler was written in LPA Prolog v3.6 under MS Windows.
 * It should run under other Prologs without too much conversion needed,
 * but note the required library modules.
 *
 *
 * Special forms
 *
 * [] and nil are treated as special forms, evaluating to [], and treated as 'false'
 * t is a special form, evaluating to t, and treated as 'true'
 * if, cond
 * progn (and implicit progn in defun and let bodies)
 * quote
 * let
 * setq
 * function
 * lambda
 * defvar, defparameter (both with and without initial values)
 *
 * Built-in procedures (defined in builtin_lisp_functions.pl)
 *
 * cons, first, rest, null
 * eq, equalp
 * plus, minus, times, divide
 * lisp_not, or, and
 * lisp_apply
 *
 * Other procedures are defined in lisp_library.pl
 *
 *******************************************************************/

% :- module(lisp_compiler,[lisp_call/2]).
/*******************************************************************
 *
 * Example definitions:
 * second(l) <<== first(rest(l)).
 * list_3(a, b, c) <<== cons(a, cons(b, cons(c, nil))).
 *
 * Example use:
 * ?| - lisp_call([second,[quote, [a,b,c]]], Result).
 * Result = b
 *
 * ?| - second([a,b,c], Result).
 * Result = b
 *
 * ?| - lisp_call([list_3, tom, dick, harry], Result).
 * Result = [tom, dick, harry]
 *
 * ?| - list_3(tom, dick, harry, Result).
 * Result = [tom, dick, harry]
 *
 *******************************************************************/
:- set_module(class(library)).
:- ensure_loaded(lpa_to_swi).

:- style_check.

:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
read_and_parse(Expr):- current_input(In),parse_sexpr_untyped(In, Expr).
:- endif.


:- ensure_loaded(library(higher_order)).
:- ensure_loaded(library(list_utilities)).
:- require([dmsg/1]).


:- ensure_loaded(library(must_trace)).
:- ensure_loaded(library(logicmoo_util_terms)).
:- ensure_loaded(library(logicmoo_util_common)).


% :- ensure_loaded(builtin_lisp_functions). % Lisp primitives: this directives is at the end of the file
% :- ensure_loaded(lisp_library).	% Functions defined in lisp: this directive is at the end of the file
					% allowing them to be compiled correctly


:- op(1200, xfx, <<== ).	% function definition
:- op(1200,  fx, <<== ).	% functional imperative definition

:- dynamic special_var/2.	% closure environments


% Connection to LPA's built-in error handler

'?ERROR?'(Error, Form):-
	lisp_error_description(_, Error, Description),
	!,
	write('LISP ERROR  '),
	write(Description),
	write(Form),
	nl.
'?ERROR?'(Error, Goal):-
	error_hook(Error, Goal).

bvof(E,L):-member(E,L).
env_memb(E,L):-member(E,L).

lisp_error_description(unbound_atom,        100, 'No value found for atom: ').
lisp_error_description(atom_does_not_exist, 101, 'Setq: Variable does not exist: ').
lisp_error_description(first_not_cons,      102, 'First: This is not a cons cell: ').
lisp_error_description(rest_not_cons,       103, 'Rest: This is not a cons cell: ').

find_incoming_value(_Head:ArgBindings,Atom,InValue,Value):-
      debug_var([Atom,'_In'],InValue),
      debug_var([Atom,'_Thru'],Value),
      ignore((member(bv(Atom0,[Value0|Unused]),ArgBindings),
         Atom0==Atom,Value0=InValue,debug_var("__",Unused))).

% The hook into the compiler

lisp_compiler_term_expansion( (FunctionHeadP <<== FunctionBodyP),
		[(Head         :- !,  BodyOO),
                 (Head         :- fail, <<==(FunctionHead , FunctionBody))] ):-
        must_det_l((expand_pterm_to_sterm(FunctionHeadP,FunctionHead),
        expand_pterm_to_sterm(FunctionBodyP,FunctionBody),
	expand_function_head(FunctionHead, Head, ArgBindings, Result),
	expand_function_body_e(Head:ArgBindings,implicit_progn([FunctionBody]), Result, Body0, Environment),
        debug_var("RET",Result),
        debug_var("Env",Environment),
        Body = (Environment=[ArgBindings],Body0),
   term_attvars(Body,AttVars),
   maplist(del_attr_rev2(freeze),AttVars),
   mize_body(',',Body,BodyOO))).

lisp_compiler_term_expansion( ( <<== FunctionBodyP),
		( :-   BodyOO) ):-
        must_det_l((expand_pterm_to_sterm(FunctionBodyP,FunctionBody),
	expand_function_body_e(_Ctx,implicit_progn([FunctionBody]), _Result, Body, []),
   term_attvars(Body,AttVars),
   maplist(del_attr_rev2(freeze),AttVars),
   mize_body(',',Body,BodyOO))).
 


lisp_eval(FunctionBody,Result):-
  expand_function_body_e(_:[],implicit_progn([FunctionBody]), Result, Body, []),!,
  dmsg(body:-Body),
  call(Body),!.

lisp_eval(FunctionBody):- lisp_eval(FunctionBody,Result),dmsg(result(Result)).

del_attr_rev2(Name,Var):- del_attr(Var,Name).


'safe_univ_2'(Comp,[F|List]):- (compound(Comp)->compound_name_arguments(Comp,F,List);Comp=..[F|List]).


expand_function_head([FunctionName | FormalArgs], Head, ArgBindings, Result):-!,
        freeze(Arg,debug_var([Arg,''],Val)),
	zip_with(FormalArgs, ActualArgs, [Arg, Val, bv(Arg, [Val|_])]^true, ArgBindings),
	append(ActualArgs, [Result], HeadArgs),
	Head =.. [FunctionName | HeadArgs].
expand_function_head(FunctionName , Head, ArgBindings, Result):-
    expand_function_head([FunctionName], Head, ArgBindings, Result).


% expand_function_body(Ctx,Function, Result, Body, Environment).
% Expands a Lisp-like function body into its Prolog equivalent

expand_function_body_e(Ctx,Function, Result, Body, Environment):-
  must(expand_function_body(Ctx,Function, Result, Body, Environment)),!.


expand_pterm_to_sterm(VAR,VAR):- \+ compound(VAR),!.
expand_pterm_to_sterm([X|L],[Y|Ls]):-!,expand_pterm_to_sterm(X,Y),expand_pterm_to_sterm(L,Ls),!.
expand_pterm_to_sterm(X,[F|Y]):- compound_name_arguments(X,F,L),expand_pterm_to_sterm(L,Y),!.
expand_pterm_to_sterm(X,X).


:- dynamic(shared_lisp_compiler:plugin_expand_function_body/5).
:- multifile(shared_lisp_compiler:plugin_expand_function_body/5).
:- discontiguous(shared_lisp_compiler:plugin_expand_function_body/5).

:- discontiguous(expand_function_body/5).

expand_function_body(Ctx,Form, Result, CompileBody, Environment):-
  shared_lisp_compiler:plugin_expand_function_body(Ctx,Form, Result, CompileBody, Environment),!.

expand_function_body(_Ctx,[defun,Name,Args|FunctionBody], Name, CompileBody, Environment):-
    FunctionHead=[Name|Args],
    CompileBody = (asserta(Head  :- fail, <<==(FunctionHead , FunctionBody)),
                  asserta((Head   :- !,  BodyOO))),                  
      expand_function_head(FunctionHead, Head, ArgBindings, Result),
      expand_function_body_e(Head:ArgBindings,implicit_progn(FunctionBody), Result, Body0, Environment),
      debug_var("RET",Result),
      debug_var("Env",Environment),
      Body = (Environment=[ArgBindings],Body0),
      term_attvars(Body,AttVars),
      maplist(del_attr_rev2(freeze),AttVars),
      mize_body(',',Body,BodyOO).

expand_function_body(Ctx,implicit_progn(Forms), Result, Body, Environment):-
	!,
      must((
	(once(Forms = [] ; Forms = [_|_] )
	->  expand_progn(Ctx,Forms, [], Result, Body, Environment)
	;   expand_function_body_e(Ctx,Forms, Result, Body, Environment)))).

expand_function_body(_Ctx,nil, [], true, _Environment):-
	!.
expand_function_body(_Ctx,[], [], true, _Environment):-
	!.
expand_function_body(_Ctx,t,   t,  true, _Environment):-
	!.
expand_function_body(_Ctx,SelfEval,  SelfEval,  true, _Environment):-is_self_evaluationing_object(SelfEval),!.

expand_function_body(Ctx, 's'(Str),  Result,  Body, Environment):-
  parse_sexpr_untyped(string(Str),Expression),!,
  expand_function_body(Ctx, Expression,  Result,  Body, Environment).

/*
expand_function_body(Ctx, List,  Result,  Body, Environment):-  \+ is_list(List),!,
  expand_pterm_to_sterm(List,PTerm),
  expand_function_body(Ctx, PTerm,  Result,  Body, Environment).
*/

expand_function_body(Ctx,[unless,Test, IfFalse], Result, Body, Environment):-expand_function_body(Ctx,[if, Test, [], IfFalse], Result, Body, Environment).
expand_function_body(Ctx,[when,Test, IfTrue], Result, Body, Environment):-expand_function_body(Ctx,[if, Test, IfTrue, []], Result, Body, Environment).
expand_function_body(Ctx,[if, Test, IfTrue], Result, Body, Environment):-expand_function_body(Ctx,[if, Test, IfTrue, []], Result, Body, Environment).
expand_function_body(Ctx,[if, [null,Test], IfTrue, IfFalse], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,Test,    TestResult,  TestBody,  Environment),
	expand_function_body_e(Ctx,IfTrue,  TrueResult,  TrueBody,  Environment),
	expand_function_body_e(Ctx,IfFalse, FalseResult, FalseBody, Environment),
        debug_var("IF",TestResult),
        Body = (	TestBody,
			( TestResult == []
				-> 	TrueBody,
					Result      = TrueResult
				;  	FalseBody,
					Result      = FalseResult	) ).

expand_function_body(Ctx,[if, Test, IfTrue, IfFalse], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,Test,    TestResult,  TestBody,  Environment),
	expand_function_body_e(Ctx,IfTrue,  TrueResult,  TrueBody,  Environment),
	expand_function_body_e(Ctx,IfFalse, FalseResult, FalseBody, Environment),
        debug_var("IF",TestResult),
        Body = (	TestBody,
			( TestResult \= []
				-> 	TrueBody,
					Result      = TrueResult
				;  	FalseBody,
					Result      = FalseResult	) ).

expand_function_body(_Ctx,[cond, []], [], true, _Environment):-
	!.
expand_function_body(Ctx,[cond, [ [Test|ResultForms] |Clauses]], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,Test, TestResult, TestBody, Environment),
	expand_progn(Ctx,ResultForms, TestResult, ResultFormsResult, ResultFormsBody, Environment),
	expand_function_body_e(Ctx,[cond, Clauses], ClausesResult, ClausesBody, Environment),
	Body = (	TestBody,
			( TestResult \= []
				->	ResultFormsBody,
					Result      = ResultFormsResult
				;	ClausesBody,
					Result      = ClausesResult )	).

expand_function_body(Ctx,[progn, Forms], Result, Body, Environment):-
	!,
	expand_progn(Ctx,Forms, [], Result, Body, Environment).

expand_function_body(Ctx,[progn|Forms], Result, Body, Environment):-
	!,
	expand_progn(Ctx,Forms, [], Result, Body, Environment).

expand_function_body_unused_needs_throw(Ctx,[car, IN], Result, Body, Environment):- \+ current_prolog_flag(lisp_inline,false),
	!,
        expand_function_body_e(Ctx,IN, MID, ValueBody, Environment),
        Body = (ValueBody,(MID =[Result|_]->true;Result=MID)).

expand_function_body(Ctx,[cons, IN1,IN2], Result, Body, Environment):- \+ current_prolog_flag(lisp_inline,false),
	!,
        expand_function_body_e(Ctx,IN1, MID1, ValueBody1, Environment),
        expand_function_body_e(Ctx,IN2, MID2, ValueBody2, Environment),
        Body = (ValueBody1,ValueBody2,Result=[MID1|MID2]).

symbol_setq(Atom, Result, Environment):-
      (	env_memb(Bindings, Environment),
                bvof(bv(Atom, Value0),Bindings)
      ->	extract_variable_value(Value0, _, Hole),
                Hole = [Result|_]
      ;	special_var(Atom, Old)
      ->	once(retract(special_var(Atom, Old))),
                assert(special_var(Atom, Result))
      ;         (lisp_error_description(atom_does_not_exist, ErrNo, _),throw(ErrNo, Atom))).

expand_function_body(Ctx,[setq, Atom, ValueForm], Result, Body, Environment):- \+ current_prolog_flag(lisp_inline,true),
	!,	
	expand_function_body_e(Ctx,ValueForm, Result, ValueBody, Environment),
	Body = (ValueBody, symbol_setq(Atom, Result, Environment)).


expand_function_body(Ctx,[setq, Atom, ValueForm], Result, Body, Environment):-
	!,
	lisp_error_description(atom_does_not_exist, ErrNo, _),
	expand_function_body_e(Ctx,ValueForm, Result, ValueBody, Environment),
	Body = (	ValueBody,
			(	env_memb(Bindings, Environment),
				bvof(bv(Atom, Value0),Bindings)
			->	extract_variable_value(Value0, _, Hole),
				Hole = [Result|_]
			;	special_var(Atom, Old)
			->	once(retract(special_var(Atom, Old))),
				assert(special_var(Atom, Result))
			;	throw(ErrNo, Atom)	)	).

expand_function_body(_Ctx,[quote, Item], Item, true, _Environment):-
	!.

expand_function_body(Ctx,[let, NewBindings, BodyForms], Result, Body, Environment):-
        !,        
	zip_with(Variables, ValueForms, [Variable, Form, [bind, Variable, Form]]^true, NewBindings),
	expand_arguments(Ctx,ValueForms, ValueBody, Values, Environment),
	zip_with(Variables, Values, [Var, Val, bv(Var, [Val|Unused])]^true,Bindings),
   must((debug_var("_U",Unused),
   debug_var("LETENV",BindingsEnvironment),
   ignore((member(VarN,[Variable,Var]),atom(VarN),debug_var([VarN,'_Let'],Val))))),       
	expand_function_body_e(Ctx,implicit_progn(BodyForms), Result, BodyFormsBody,
		BindingsEnvironment),
         Body = ( ValueBody,BindingsEnvironment=[Bindings|Environment], BodyFormsBody ).

expand_function_body(Ctx,[let, NewBindings| BodyForms], Result, Body, Environment):-
      expand_function_body(Ctx,[let, NewBindings,[progn| BodyForms]], Result, Body, Environment).


expand_function_body(Ctx,[function, [lambda,LambdaArgs| LambdaBody]], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,implicit_progn(LambdaBody), ClosureResult, ClosureBody, ClosureEnvironment),
        debug_var('LArgs',LambdaArgs),
        debug_var('LResult',ClosureResult),
        debug_var('LEnv',ClosureEnvironment),
                     Result = [closure,LambdaArgs,
			[ClosureEnvironment, ClosureResult]^ClosureBody,
			Environment],
	Body = true.

expand_function_body(Ctx,[lambda,LambdaArgs, LambdaBody], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,implicit_progn([LambdaBody]), ClosureResult, ClosureBody, ClosureEnvironment),
        debug_var('LArgs',LambdaArgs),
        debug_var('LResult',ClosureResult),
        debug_var('LEnv',ClosureEnvironment),
                     Result = [closure,LambdaArgs,
			[ClosureEnvironment, ClosureResult]^ClosureBody,
			Environment],
	Body = true.


expand_function_body(_Ctx,[function, Function], [function,Function], true, _Environment):-
	!.

expand_function_body(Ctx,[defvar, Var], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,[defvar, Var, nil], Result, Body, Environment).
expand_function_body(Ctx,[defvar, Var, Value], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,Value, Result, ValueBody, Environment),
	Body = (	ValueBody,
			(	special_var(Var, _)
			->	true
			;	assert(special_var(Var, Result))	)	).
expand_function_body(Ctx,[defparameter, Var], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,[defparameter, Var, nil], Result, Body, Environment).
expand_function_body(Ctx,[defparameter, Var, Value], Result, Body, Environment):-
	!,
	expand_function_body_e(Ctx,Value, Result, ValueBody, Environment),
	Body = (	ValueBody,
			(	special_var(Var, _)
			->	once(retract(special_var(Var, _)))
			;	true	),
			assert(special_var(Var, Result))	).

expand_function_body(Ctx,Atom, InValue, Body, _Environment):-
	atom(Atom),
        find_incoming_value(Ctx,Atom,InValue,Value),
        (get_attr(Value,initState,t);get_attr(InValue,initState,t)),
	!,
        Body = true.

initState:attr_unify_hook(_,_).

expand_function_body(Ctx,Atom, Value, Body, Environment):-  \+ current_prolog_flag(lisp_inline,true),
	atom(Atom),
        find_incoming_value(Ctx,Atom,InValue,Value),
        put_attr(Value,initState,t),
        put_attr(InValue,initState,t),
	!,
        Body = sym_arg_val_env(Atom,InValue,Value,Environment).

sym_arg_val_env(Atom,_InValue,Value,Environment):-
  (once((	env_memb(Bindings, Environment),
			bvof(bv(Atom, Value0),Bindings),
			extract_variable_value(Value0, Value, _)
		    ;	special_var(Atom, Value)
		    ;	(lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Atom))))).

expand_function_body(_Ctx,Atom, Value, Body, Environment):-
	atom(Atom),
   debug_var([Atom,'_Stack'],Value0),
   debug_var([Atom,'_VAL'],Value),
	!,
	lisp_error_description(unbound_atom, ErrNo, _),
	Body = (once((	env_memb(Bindings, Environment),
			bvof(bv(Atom, Value0),Bindings),
			extract_variable_value(Value0, Value, _)
		    ;	special_var(Atom, Value)
		    ;	throw(ErrNo, Atom)	)	)).	


expand_function_body(Ctx,[+ | FunctionArgs], Result, Body, Environment):-!,
  expand_function_body(Ctx,[plus | FunctionArgs], Result, Body, Environment).
expand_function_body(Ctx,[- | FunctionArgs], Result, Body, Environment):-!,
  expand_function_body(Ctx,[minus | FunctionArgs], Result, Body, Environment).

% Non built-in function expands into an explicit function call
expand_function_body(Ctx,[FunctionName | FunctionArgs], Result, Body, Environment):-
	!,
	expand_arguments(Ctx,FunctionArgs, ArgBody, Args, Environment),
	append(Args, [Result], ArgsResult),
        debug_var([FunctionName,'_Ret'],Result),
	ExpandedFunction =.. [FunctionName | ArgsResult],
	Body = (	ArgBody,
			ExpandedFunction	).

	expand_arguments(_Ctx,[], true, [], _Environment).
	expand_arguments(Ctx,[Arg|Args], Body, [Result|Results], Environment):-
		expand_function_body_e(Ctx,Arg, Result, ArgBody, Environment),
                Body = (ArgBody, ArgsBody),
		expand_arguments(Ctx,Args, ArgsBody, Results, Environment).

p_n_atom(Atom,UP):- string(Atom),name(Atom,[C|S]),to_upper(C,U),name(UP,[U|S]).
p_n_atom(Atom,UP):- atom(Atom),name(Atom,[C|S]),to_upper(C,U),name(UP,[U|S]).
p_n_atom([C|S],UP):- to_upper(C,U),name(UP,[U|S]).

% debug_var(_,_):-!.
debug_var(A,B):- ignore(catch(debug_var0(A,B),E,dmsg(A=E))).
debug_var0([Atom|Rest],Var):-p_n_atom(Atom,UP),atomics_to_string([UP|Rest],NAME),add_var_to_env(NAME,Var),!.
debug_var0(Atom,Var):- p_n_atom(Atom,UP),add_var_to_env(UP,Var),!.

expand_progn(_Ctx,[], Result, Result, true, _Environment).
expand_progn(Ctx,[Form | Forms], _PreviousResult, Result, Body, Environment):-  !,
	expand_function_body_e(Ctx,Form, FormResult, FormBody, Environment),
	Body = (FormBody, FormsBody),
	expand_progn(Ctx, Forms, FormResult, Result, FormsBody, Environment).
expand_progn(Ctx, Form , _PreviousResult, Result, Body, Environment):-
	expand_function_body_e(Ctx,Form, Result, Body, Environment).

conjoin_0(A,B,A):- B==true,!.
conjoin_0(A,B,B):- A==true,!.
conjoin_0((A,AA),B,(A,AAB)):-!, conjoin(AA,B,AAB).
conjoin_0(A,B,(A,B)).

mize_body(_,A,A):- \+ compound(A),!.
mize_body(F,(A,B),AB):-!,mize_body(F,A,AA),mize_body(F,B,BB),conjoin_0(AA,BB,AB).
mize_body(_,(A -> B ; C),(AA -> BB ; CC)):-!,mize_body1(->,A,AA),mize_body1(';',B,BB),mize_body1(';',C,CC).
mize_body(_,(B ; C),( BB ; CC)):-!,mize_body1(';',B,BB),mize_body1(';',C,CC).
mize_body(F,A,C):- mize_body1(F,A,B),mize_body2(F,B,C),!.
mize_body(_F,A,B):- A=..[F|AA], must_maplist(mize_body(F),AA,BB),B=..[F|BB].
mize_body(_,A,A):-!.

mize_body1(_,A,A):- var(A),del_attr(A,initState).
mize_body1(_,A,A):- \+ compound(A),!.
mize_body1(F,(A,B),AB):-!,mize_body1(F,A,AA),mize_body1(F,B,BB),conjoin_0(AA,BB,AB).
mize_body1(F,A,B):- is_list(A),must_maplist(mize_body1(F),A,B).
mize_body1(_,A,L=[R]):- A =@= (L=[R, []]).
mize_body1(_F,A,B):- A=..[F|AA],must_maplist(mize_body1(F),AA,BB),B=..[F|BB].
mize_body1(_,A,A):-!.

mize_body2(_,A,A):- \+ compound(A),!.
mize_body2(_,A=B,pass2(A=B)):- var(A),var(B),A=B,!.
mize_body2(_F,A,B):- A=..[F|AA], must_maplist(mize_body2(F),AA,BB),B=..[F|BB].
mize_body2(_,A,A):-!.


ssip_compiler_term_expansion(Symbol,lambda(Args,Body),[OOUT]):- atom(Symbol),is_list(Args),
  length(Args,A1),
  A is A1+1,
  cfunctor(P,Symbol,A),
 (predicate_property(P,defined)->gensym(Symbol,SymbolR);Symbol=SymbolR),
  Head=..[SymbolR|Args],
  subst(Body,Symbol,SymbolR,BodyM),
  OUT= ((Head <<== BodyM)),
  must(lisp_compiler_term_expansion(OUT,OOUT)),!.

ssip_compiler_term_expansion(Symbol,Symbol2,ssip_define(Symbol,Symbol2)):-!.

:- discontiguous(ssip_define/2).
:- multifile(ssip_define/2).

% The hook into the compiler
term_expansion(Symbol==Function,O) :- I= (Symbol==Function),ssip_compiler_term_expansion(Symbol,Function,O),nl,nl,
  flatten([I,O],L),
  in_cmt(maplist(portray_clause,L)),!.
term_expansion(I,O) :- lisp_compiler_term_expansion(I,O),I\==O,nl,nl,
  flatten([I,O],L),
  maplist(dmsg,L),!.


% Now Prolog can understand them, compile the additional library files


:- ensure_loaded(builtin_lisp_functions).
:- ensure_loaded(lisp_library).
:- ensure_loaded(streams).
:- ensure_loaded(tests).

%% [tim.prolog]siptest.
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:39:27 1986
%% this file contains samples for SIP.





fact == lambda([n], if(=(n,0),1,n*fact(sub1(n)))).

add1 == lambda([n], n+1).

sub1 == lambda([n], n-1).

% higher order functions

mapcar ==
  lambda([f,l],
         if(null(l),
            nil,
            cons(f(car(l)),mapcar(f,cdr(l))))).

% simple list manipulation functions.

length == lambda([l], if(null(l),0,add1(length(cdr(l))))).

append == lambda([l1,l2],if(null(l1),l2,cons(car(l1),append(cdr(l1),l2)))).


% stuff for streams.

filter ==
  lambda([f,s],
         if('emptyStream?'(s),
            s,
            if(f(head(s)),
               consStream(head(s),filter(f,tail(s))),
               filter(f,tail(s))))).

from(n) <<== consStream(n,from(n+1)).
% from == lambda([n],consStream(n,from(n+1))).

nthStream == lambda([s,n],if(n=1,head(s),nthStream(tail(s),n-1))).

integers == from(1).

% environments

makeCounter ==
  lambda([],
         begin(counter == 0,
               lambda([],setq(counter,1+counter)))).

caaaar == lambda([x],car(car(car(car(x))))).

caar == lambda([x],car(car(x))).

reverse ==
  lambda([l],
     if(null(l),
        l,
        append(reverse(cdr(l)),(cons(car(l),nil))))).

lisp_compile(s(Str),Result):-!,
  parse_sexpr_untyped(string(Str),Expression),!,
  lisp_eval(Expression,Result).
lisp_compile(Expression,Result):-!,
  lisp_eval(Expression,Result).

:- set_prolog_flag(double_quotes,string).


c1:- print_eval_string("
 (defun sum_with_map (xs)
  (let (( running_total 0))
    (let ((summer 
            (function 
               (lambda (n)
                (setq running_total (+ running_total n))))))
       (mapcar summer  xs) running_total))) "
).

c2:- lisp_compile(s("
(defun sum_with_map (xs)
  (let (( running_total 0))
    (let ((summer 
            (function 
               (lambda (n)
                (setq running_total (+ running_total n))))))
       (mapcar summer  xs) running_total)))
 "
  ),O),writeln(O).




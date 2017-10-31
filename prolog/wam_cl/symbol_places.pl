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

:- ensure_loaded(library(higher_order)).
:- ensure_loaded(library(list_utilities)).
:- require([colormsg1/1]).


:- ensure_loaded(library(must_trace)).
:- ensure_loaded(library(logicmoo_util_terms)).
:- ensure_loaded(library(logicmoo_util_common)).

:- dynamic(tst:is_local_test/1).
:- multifile(tst:is_local_test/1).
:- discontiguous(tst:is_local_test/1).
:- dynamic(tst:is_local_test/2).
:- multifile(tst:is_local_test/2).
:- discontiguous(tst:is_local_test/2).
:- dynamic(tst:is_local_test/3).
:- multifile(tst:is_local_test/3).
:- discontiguous(tst:is_local_test/3).
:- dynamic(compile_assignents/5).
:- multifile(compile_assignents/5).
:- discontiguous(compile_assignents/5).



compile_assignents(Ctx,Env,Result,[SetQ, Atom, ValueForm, Atom2| Rest], Body):- is_parallel_op(SetQ),!, 
   pairify([Atom, ValueForm, Atom2| Rest],Atoms,Forms),
   maplist(expand_ctx_env_forms(Ctx,Env),Forms,BodyS1,Results),
   maplist(set_with_prolog_var(Ctx,Env,SetQ),Atoms,Results,BodyS2),   
   ((op_return_type(SetQ,RT),RT=name) ->  last(Atoms,Result) ; last(Results,Result)),
   append(BodyS1,BodyS2,BodyS),list_to_conjuncts(BodyS,Body).


compile_assignents(Ctx,Env,Result,[SetQ, Atom, ValueForm, Atom2| Rest], Body):- is_pair_op(SetQ), 
   must_compile_body(Ctx,Env,_ResultU,[SetQ, Atom, ValueForm], Body1),
   must_compile_body(Ctx,Env,Result,[SetQ, Atom2| Rest],  Body2),
   Body = (Body1 , Body2).

compile_assignents(Ctx,Env,Result,[Defvar, Var], Body):- is_def_nil(Defvar),!,
  must_compile_body(Ctx,Env,Result,[Defvar, Var , nil],Body).

compile_assignents(Ctx,Env,Result,[Getf, Atom| ValuesForms], Body):- is_place_op(Getf),     
	must_maplist(expand_ctx_env_forms(Ctx,Env),ValuesForms, ValuesBody,ResultVs),
        list_to_conjuncts(ValuesBody,BodyS),
        Body = (BodyS, place_op(Getf, Atom, ResultVs,Env,Result)).

compile_assignents(Ctx,Env,Result,[SetQ, Atom, ValueForm, String], (assert(type_documentation(variable,Atom,String)),Body)):- 
        string(String),is_def_maybe_docs(SetQ),
	!, compile_assignents(Ctx,Env,Result,[SetQ, Atom, ValueForm], Body).


compile_assignents(Ctx,Env,Result,[SetQ, Atom, ValueForm], Body):- is_symbol_setter(SetQ),
       % (EnvIn\==[]-> true ; break),
	!,	
	must_compile_body(Ctx,Env,ResultV,ValueForm, ValueBody),
        Body = (ValueBody, symbol_setter(SetQ, Atom, ResultV, Env)),
        ((op_return_type(SetQ,RT),RT=name) ->  =(Atom,Result) ; =(ResultV,Result)).

compile_assignents(Ctx,Env,Result,Atom, Body):- Atom==mapcar,!, dbmsg(compile_assignents(Ctx,Env,Result,Atom, Body)), dumpST,break.

compile_assignents(Ctx,Env,Value, Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        (get_attr(Value,initState,t);get_attr(InValue,initState,t)),
	!,
        Body = symbol_value(Atom,Env, Value).

compile_assignents(Ctx,Env,InValue, Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        (get_attr(Value,initState,t);get_attr(InValue,initState,t)),
	!,
        Body = true.

compile_assignents(Ctx,Env,Value,Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        put_attr(Value,initState,t),
        put_attr(InValue,initState,t),
	!,
        Body = sym_arg_val_env(Atom,InValue,Value,Env).

compile_assignents(_Cx,Env,Value, Atom,  Body):- atom(Atom),
   debug_var([Atom,'_Stack'],Value0),
   debug_var([Atom,'_VAL'],Value),
	!,
	lisp_error_description(unbound_atom, ErrNo, _),
	Body = (once((	env_memb(Bindings, Env),
			bvof(bv(Atom, Value0),Bindings),
			extract_variable_value(Value0, Value, _)
		    ;	special_var(Atom, Value)
		    ;	throw(ErrNo, Atom)	)	)).	








initState:attr_unify_hook(_,_).


symbol_value(Var,Env,Value):-
  symbol_value_or(Var,Env,
    last_chance_symbol_value(Var,Env,Value),Value).

last_chance_symbol_value(Var,_Env,Result):- nb_current(Var,Result),!.
last_chance_symbol_value(Var,_Env,_Result):- 
  lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).

symbol_value_or(Var,Env,G,Value):-
 (env_memb(Bindings, Env),bvof(bv(Var, Value0),Bindings))-> extract_variable_value(Value0, Value, _);
   (special_var(Var, Value) -> true;  G).


set_symbol_value(Var,Env,Result):-var(Result),!,symbol_value(Var,Env,Result).
set_symbol_value(Var,Env,Result):- !,
     ((	env_memb(Bindings, Env),
                bvof(bv(Var, Value0),Bindings)
      ->	nb_setarg(1,Value0,Result)
      ;	special_var(Var, Old)
      ->	once(retract(special_var(Var, Old))),
                asserta(special_var(Var, Result))
      ;         last_chance_set_symbol_value(Var,Env,Result))).
set_symbol_value(Var,Env,Result):- 
      (	env_memb(Bindings, Env),
                bvof(bv(Var, Value0),Bindings)
      ->	extract_variable_value(Value0, _, Hole),
                Hole = [Result|_]
      ;	special_var(Var, Old)
      ->	once(retract(special_var(Var, Old))),
                asserta(special_var(Var, Result))
      ;         last_chance_set_symbol_value(Var,Env,Result)).
last_chance_set_symbol_value(Var,_Env,Result):- nb_setval(Var,Result),!.
last_chance_set_symbol_value(Var,_Env,_Result):- 
  lisp_error_description(atom_does_not_exist, ErrNo, _),throw(ErrNo, Var).


sym_arg_val_env(Var,InValue,Value,Env):-
  set_symbol_value(Var,Env,InValue),
  Value=InValue,!.

sym_arg_val_env(Var,InValue,Value,Env):- !,
  symbol_value_or(Var,Env,(nonvar(InValue),InValue=Value),Value)-> true;
    lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).

place_op(incf, Var, [Value], Env, Result):- atom(Var),!,
  symbol_value(Var, Env , Old),
  Result is Old+Value,
  set_symbol_value(Var, Env , Result).
place_op(decf, Var, [Value], Env, Result):- atom(Var),!,
  symbol_value(Var, Env , Old),
  Result is Old-Value,
  set_symbol_value(Var, Env , Result).
place_op(setf, Var, [Result], Env, Result):- atom(Var),!,
  set_symbol_value(Var, Env , Result).

%TODO Make it a constantp
symbol_setter(defconstant, Var, Result, _Environment):-
   ( special_var(Var, _) -> once(retract(special_var(Var, _))); true),
   asserta(special_var(Var, Result)).

symbol_setter(defparameter, Var, Result, _Environment):-
   ( special_var(Var, _) -> once(retract(special_var(Var, _))); true),
   asserta(special_var(Var, Result)).

symbol_setter(defvar, Var, Result, _Environment):-
     special_var(Var, _) -> true ; asserta(special_var(Var, Result)).

symbol_setter(setq, Var, Result, Env):- !, set_symbol_value(Var,Env,Result).

symbol_setter(psetq, Var, Result, Env):- !,
  symbol_setter(setq, Var, Result, Env).


is_symbol_setter(OP):- is_pair_op(OP).
is_symbol_setter(OP):- is_parallel_op(OP).
is_symbol_setter(OP):- is_def_maybe_docs(OP).

op_return_type(Op,name):- is_def_nil(Op).
op_return_type(defconstant,name).

is_def_nil(defparameter).
is_def_nil(defvar).

is_def_maybe_docs(defparameter).
is_def_maybe_docs(defvar).
is_def_maybe_docs(defconstant).

is_pair_op(setq).
is_pair_op(psetq).

is_pair_op(setf).
is_pair_op(psetf).


is_place_op(setf).
is_place_op(psetf).
is_place_op(getf).
is_place_op(incf).
is_place_op(decf).
is_place_op(rotatef).
is_place_op(shiftf).


is_parallel_op(psetf).
is_parallel_op(psetq).

pairify([],[],[]).
pairify([Atom, ValueForm | Rest],[Atom | Atoms],[ValueForm | Forms]):-
   pairify(Rest,Atoms,Forms).

set_with_prolog_var(_Cx,Env,SetQ,Atom,Result,symbol_setter(SetQ, Atom, Result, Env)).

expand_ctx_env_forms(Ctx, Env,Forms,Body, Result):- 
   must_compile_body(Ctx,Env,Result,Forms, Body).



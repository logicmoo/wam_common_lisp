/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(assign, []).
:- set_module(class(library)).
:- include('header.pro').
:- ensure_loaded((utils_for_swi)).



compile_assigns(Ctx,Env,Result,[SetQ, Atom, ValueForm, Atom2| Rest], Body):- is_parallel_op(SetQ),!, 
   pairify([Atom, ValueForm, Atom2| Rest],Atoms,Forms),
   maplist(expand_ctx_env_forms(Ctx,Env),Forms,BodyS1,Results),
   maplist(set_with_prolog_var(Ctx,Env,SetQ),Atoms,Results,BodyS2),   
   ((op_return_type(SetQ,RT),RT=name) ->  last(Atoms,Result) ; last(Results,Result)),
   append(BodyS1,BodyS2,BodyS),list_to_conjuncts(BodyS,Body).


compile_assigns(Ctx,Env,Result,[SetQ, Atom, ValueForm, Atom2| Rest], Body):- is_pair_op(SetQ), 
   must_compile_body(Ctx,Env,_ResultU,[SetQ, Atom, ValueForm], Body1),
   must_compile_body(Ctx,Env,Result,[SetQ, Atom2| Rest],  Body2),
   Body = (Body1 , Body2).

compile_assigns(Ctx,Env,Result,[Defvar, Var], Body):- is_def_nil(Defvar),!,
  must_compile_body(Ctx,Env,Result,[Defvar, Var , nil],Body).

compile_assigns(Ctx,Env,Result,[Getf, Atom| ValuesForms], Body):- is_place_op(Getf),     
	must_maplist(expand_ctx_env_forms(Ctx,Env),ValuesForms, ValuesBody,ResultVs),
        list_to_conjuncts(ValuesBody,BodyS),
        Body = (BodyS, place_op(Getf, Atom, ResultVs,Env,Result)).

compile_assigns(Ctx,Env,Result,[SetQ, Atom, ValueForm, String], (assert(type_documentation(variable,Atom,String)),Body)):- 
        string(String),is_def_maybe_docs(SetQ),
	!, compile_assigns(Ctx,Env,Result,[SetQ, Atom, ValueForm], Body).


compile_assigns(Ctx,Env,Result,[SetQ, Atom, ValueForm], Body):- is_symbol_setter(SetQ),
       % (EnvIn\==[]-> true ; break),
	!,	
	must_compile_body(Ctx,Env,ResultV,ValueForm, ValueBody),
        Body = (ValueBody, symbol_setter(SetQ, Atom, ResultV, Env)),
        ((op_return_type(SetQ,RT),RT=name) ->  =(Atom,Result) ; =(ResultV,Result)).

compile_assigns(Ctx,Env,Result,Atom, Body):- Atom==mapcar,!, dbmsg(compile_assigns(Ctx,Env,Result,Atom, Body)), dumpST,break.

compile_assigns(Ctx,Env,Value, Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        (get_attr(Value,initState,t);get_attr(InValue,initState,t)),
	!,
        Body = symbol_value(Atom,Env, Value).

compile_assigns(Ctx,Env,InValue, Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        (get_attr(Value,initState,t);get_attr(InValue,initState,t)),
	!,
        Body = true.

compile_assigns(Ctx,Env,Value,Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        put_attr(Value,initState,t),
        put_attr(InValue,initState,t),
	!,
        Body = sym_arg_val_env(Atom,InValue,Value,Env).

compile_assigns(_Cx,Env,Value, Atom,  Body):- atom(Atom),
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


extract_variable_value([Val|Vals], FoundVal, Hole):-
		var(Vals)
	->	FoundVal = Val,
		Hole = Vals
	;	extract_variable_value(Vals, FoundVal, Hole).


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

:- fixup_exports.



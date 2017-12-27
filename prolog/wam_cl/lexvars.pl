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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(assign, []).
:- set_module(class(library)).
:- include('header').
:- ensure_loaded((utils_for_swi)).


%TODO Make it a constantp
deflexical(Env,defconstant, Var, Result):- 
   set_var(Env,Var,Result),
   set_opv(Var,declared_as,defconstant).
deflexical(Env,defconst, Var, Result):- 
  deflexical(Env,defconstant, Var, Result).

deflexical(Env,defparameter, Var, Result):- 
   set_opv(Var,declared_as,defparameter),
   set_var(Env,Var,Result).

deflexical(_Env,defvar, Var, Result):-   
   (get_opv(Var, value, _) -> true ; update_opv(Var, value, Result)),
   set_opv(Var,declared_as,defvar).

deflexical(Env,setq, Var, Result):- !, set_var(Env,Var,Result).

deflexical(Env,psetq, Var, Result):- !, set_var(Env,Var,Result).



op_return_type(Op,name):- is_def_nil(Op).
op_return_type(defconstant,name).
op_return_type(defconst,name).

is_def_nil(defparameter).
is_def_nil(defvar).

is_def_maybe_docs(defparameter).
is_def_maybe_docs(defvar).
is_def_maybe_docs(defconstant).
is_def_maybe_docs(defconst).

% catches an internal error in this compiler 
compile_symbol_getter(Ctx,Env,Result,Var, Body):- Var==mapcar,!, 
  dbginfo(compile_symbol_getter(Ctx,Env,Result,Var, Body)), lisp_dump_break.


compile_symbol_getter(Ctx,Env,Value, Var, Body):-  always((atom(Var),!,
        debug_var([Var,'_Get'],Value),
        add_tracked_var(Ctx,Var,Value),
        rw_add(Ctx,Var,r),
        %debug_var('_GEnv',Env),
        Body = get_var(Env, Var, Value))).



extract_variable_value([Val|Vals], FoundVal, Hole):-
		var(Vals)
	->	FoundVal = Val,
		Hole = Vals
	;	extract_variable_value(Vals, FoundVal, Hole).


bind_dynamic_value(Env,Var,Result):- set_var(Env,Var,Result).

get_symbol_value(Env,Obj,Value):- get_var(Env,Obj,Value).

get_var(Var,Value):- current_env(Env), get_var(Env,Var,Value).
get_var(Env,Var,Value):-
  symbol_value_or(Env,Var,
    symbol_value_error(Env,Var,Value),Value).

symbol_value_or(Env,Var,G,Value):-
 ensure_env(Env), 
 (symbol_value0(Env,Var,Value) -> true ; G).

symbol_value0(Env,Var,Value):-  bvof(bv(Var, Value),_,Env).
symbol_value0(_Env,Var,_Value):- notrace((nonvar(Var),is_functionp(Var),dbginfo(is_functionp(Var)))),!,lisp_dump_break.
symbol_value0(_Env,Var,Result):- get_opv(Var, value, Result),!.
symbol_value0(_Env,Var,Result):- atom(Var),get_opv(Var,value,Result),!.
symbol_value0(Env,[Place,Obj],Result):- trace, set_place(Env,getf,[Place,Obj],[],Result).


symbol_value_error(_Env,Var,_Result):- lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).

reset_mv:- b_getval('$mv_return',[V1,_V2|_])->b_setval('$mv_return',[V1]);true.

push_values([V1|Push],V1):- always(nonvar(Push)),nb_setval('$mv_return',[V1|Push]).

bvof(_,_,T):- notrace(((var(T);T==[tl]))),!,fail.
bvof(E,M,T):-E=T,!,M=T.
bvof(E,M,[L|L2]):- ((nonvar(L),bvof(E,M,L))->true;(nonvar(L2),bvof(E,M,L2))).


set_var(Var,Val):- % ensure_env(Env),!,
       set_var(_Env,Var,Val).

set_var(Env,Var,Result):-var(Result),!,get_var(Env,Var,Result).
set_var(Env,Var,Result):- % ensure_env(Env),!,
     (bvof(bv(Var,_),BV,Env)
      -> nb_setarg(2,BV,Result)
      ;	( 
        (get_opv(Var, value, _Old) 
           -> update_opv(Var, value, Result) 
           ; set_symbol_value_last_chance(Env,Var,Result)))).

%set_symbol_value_last_chance(_Env,Var,Result):- nb_current(Var,_)-> nb_setval(Var,Result),!.
%set_symbol_value_last_chance(Env,Var,Result):- add_to_env(Env,Var,Result),!.
set_symbol_value_last_chance(_Env,Var,Result):- set_opv(Var, value, Result),!.
set_symbol_value_last_chance(_Env,Var,_Result):- 
  lisp_error_description(atom_does_not_exist, ErrNo, _),throw(ErrNo, Var).


cl_defparameter(Var, Result, Result):- 
   set_opv(Var,declared_as,defparameter),
   set_var(_Env,Var,Result).


env_sym_arg_val(Env,Var,InValue,Value):-
  set_var(Env,Var,InValue),
  Value=InValue,!.

env_sym_arg_val(Env,Var,InValue,Value):- !,
  symbol_value_or(Env,Var,(nonvar(InValue),InValue=Value),Value)-> true;
    lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).


set_with_prolog_var(Ctx,Env,SetQ,Var,Result,set_var(Env,SetQ, Var, Result)):-
  rw_add(Ctx,Var,w).

expand_ctx_env_forms(Ctx, Env,Forms,Body, Result):- 
   must_compile_body(Ctx,Env,Result,Forms, Body).


:- fixup_exports.




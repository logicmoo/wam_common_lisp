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


compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm, Atom2| Rest], Body):- is_parallel_op(SetQ),!, 
   pairify([Var, ValueForm, Atom2| Rest],Atoms,Forms),
   maplist(expand_ctx_env_forms(Ctx,Env),Forms,BodyS1,Results),
   maplist(set_with_prolog_var(Ctx,Env,SetQ),Atoms,Results,BodyS2),   
   ((op_return_type(SetQ,RT),RT=name) ->  last(Atoms,Result) ; last(Results,Result)),
   append(BodyS1,BodyS2,BodyS),list_to_conjuncts(BodyS,Body).


compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm, Atom2| Rest], Body):- is_pair_op(SetQ), 
   if_must_compile_body(Ctx,Env,_ResultU,[SetQ, Var, ValueForm], Body1),
   if_must_compile_body(Ctx,Env,Result,[SetQ, Atom2| Rest],  Body2),
   Body = (Body1 , Body2).

compile_assigns(Ctx,Env,Result,[Defvar, Var], Body):- is_def_nil(Defvar),!,
  compile_assigns(Ctx,Env,Result,[Defvar, Var , nil],Body).

compile_assigns(Ctx,Env,Result,[Getf, Var| ValuesForms], Body):- is_place_op(Getf),     
	must_maplist(expand_ctx_env_forms(Ctx,Env),ValuesForms, ValuesBody,ResultVs),
        list_to_conjuncts(ValuesBody,BodyS),
        debug_var([Getf,'_R'],Result),
        debug_var([Getf,'_Env'],Env),
        Body = (BodyS, place_op(Env,Getf, Var, ResultVs,Result)).

compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm, String], (Code,Body)):- 
        string(String),is_def_maybe_docs(SetQ),
        Code = asserta(doc:doc_string(Var,_Package,variable,String)),
	!, compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm], Body).


compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm], Body):- is_symbol_setter(Env,SetQ),
       % (EnvIn\==[]-> true ; break),
	!,	
	if_must_compile_body(Ctx,Env,ResultV,ValueForm, ValueBody),
        ((op_return_type(SetQ,RT),RT=name) ->  =(Var,Result) ; =(ResultV,Result)),
        Body = (ValueBody, symbol_setter(Env,SetQ, Var, ResultV)).
        

ensure_rw(V,Dict):- get_attr(V,rwstate,Dict)-> true ; (Dict = rw{r:0,w:0},put_attr(V,rwstate,Dict)).


rw_add(V,RW):- ensure_rw(V,Dict),arginfo_incr(RW,Dict).
rw_get(V,RW,Value):- ensure_rw(V,Dict),get_dict(RW,Dict,Value).


compile_symbol_getter(Ctx,Env,Result,Var, Body):- Var==mapcar,!, dbmsg(compile_symbol_getter(Ctx,Env,Result,Var, Body)), lisp_dumpST,break.


compile_symbol_getter(_Ctx,Env,Value, Var, Body):-  atom(Var),!,
        debug_var([Var,'_Get'],Value),
        Body = symbol_value(Env, Var, Value).   
        
compile_symbol_getter(_Cx,Env,Value, Var,  Body):- % lisp_dumpST,break,
   debug_var([Var,'_Stack'],Value0),
   debug_var([Var,'_VAL'],Value),
	!,
	lisp_error_description(unbound_atom, ErrNo, _),
	Body = (once((	env_memb(Bindings, Env),
			bvof(bv(Var, Value0),Bindings),
			extract_variable_value(Value0, Value, _)
		    ;	symbol_value(Var,Value)
		    ;	throw(ErrNo, Var)	)	)).	




rwstate:attr_unify_hook(_,_).


extract_variable_value([Val|Vals], FoundVal, Hole):-
		var(Vals)
	->	FoundVal = Val,
		Hole = Vals
	;	extract_variable_value(Vals, FoundVal, Hole).


bind_dynamic_value(Env,Var,Result):- set_symbol_value(Env,Var,Result).

symbol_value(Env,Var,Value):-
  symbol_value_or(Env,Var,
    symbol_value_last_chance(Env,Var,Value),Value).

symbol_value_last_chance(_Env,Var,Result):- nb_current(Var,Result),!.
symbol_value_last_chance(_Env,Var,_Result):- 
  lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).


push_values([V1|Push],V1):- must(nonvar(Push)),nb_setval('$mv_return',[V1|Push]).



bvof(E,L):- nonvar(L),member(E,L).
env_memb(E,L):- nonvar(L),member(E,L).
env_memb(E,E).

symbol_value_or(Env,Var,G,Value):-
 (env_memb(Bindings, Env),bvof(bv(Var, Value0),Bindings)) 
    -> extract_variable_value(Value0, Value, _)
      ; (symbol_value(Var,Value) -> true;  G).

symbol_value(Var,Value):- get_opv(Var,value,Value).


set_symbol_value(Env,Var,Result):-var(Result),!,symbol_value(Env,Var,Result).
set_symbol_value(Env,Var,Result):- !,
     ((	env_memb(Bindings, Env),bvof(bv(Var, Value0),Bindings))
      -> nb_setarg(1,Value0,Result)
      ;	( 
        (get_opv(Var, value, _Old) 
           -> update_opv(Var, value, Result) 
           ; set_symbol_value_last_chance(Env,Var,Result)))).

set_symbol_value_last_chance(_Env,Var,Result):- nb_setval(Var,Result),!.
set_symbol_value_last_chance(_Env,Var,_Result):- 
  lisp_error_description(atom_does_not_exist, ErrNo, _),throw(ErrNo, Var).


env_sym_arg_val(Env,Var,InValue,Value):-
  set_symbol_value(Env,Var,InValue),
  Value=InValue,!.

env_sym_arg_val(Env,Var,InValue,Value):- !,
  symbol_value_or(Env,Var,(nonvar(InValue),InValue=Value),Value)-> true;
    lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).

place_op(Env,incf, Var, [Value],  Result):- atom(Var),!,
  symbol_value(Env,Var, Old),
  Result is Old+Value,
  set_symbol_value(Env,Var, Result).
place_op(Env,decf, Var, [Value],  Result):- atom(Var),!,
  symbol_value(Env,Var, Old),
  Result is Old-Value,
  set_symbol_value(Env,Var, Result).
place_op(Env,setf, Var, [Result],  Result):- atom(Var),!,
  set_symbol_value(Env,Var, Result).

%TODO Make it a constantp
symbol_setter(Env,defconstant, Var, Result):- 
   set_symbol_value(Env,Var,Result),
   add_opv(Var,kw_deftype,defconstant).
symbol_setter(Env,defconst, Var, Result):- 
  symbol_setter(Env,defconstant, Var, Result).

symbol_setter(Env,defparameter, Var, Result):- 
   add_opv(Var,kw_deftype,defparameter),
   set_symbol_value(Env,Var,Result).

symbol_setter(_Env,defvar, Var, Result):-   
   (get_opv(Var, value, _) -> true ; update_opv(Var, value, Result)),
   add_opv(Var,kw_deftype,defvar).

symbol_setter(Env,setq, Var, Result):- !, set_symbol_value(Env,Var,Result).

symbol_setter(Env,psetq, Var, Result):- !, symbol_setter(Env,setq, Var, Result).


is_symbol_setter(_Env,OP):- is_pair_op(OP).
is_symbol_setter(_Env,OP):- is_parallel_op(OP).
is_symbol_setter(_Env,OP):- is_def_maybe_docs(OP).

op_return_type(Op,name):- is_def_nil(Op).
op_return_type(defconstant,name).
op_return_type(defconst,name).

is_def_nil(defparameter).
is_def_nil(defvar).

is_def_maybe_docs(defparameter).
is_def_maybe_docs(defvar).
is_def_maybe_docs(defconstant).
is_def_maybe_docs(defconst).

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
pairify([Var, ValueForm | Rest],[Var | Atoms],[ValueForm | Forms]):-
   pairify(Rest,Atoms,Forms).

set_with_prolog_var(_Cx,Env,SetQ,Var,Result,symbol_setter(Env,SetQ, Var, Result)).

expand_ctx_env_forms(Ctx, Env,Forms,Body, Result):- 
   must_compile_body(Ctx,Env,Result,Forms, Body).

:- fixup_exports.



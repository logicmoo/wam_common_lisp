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
:- include('header.pro').
:- ensure_loaded((utils_for_swi)).

% local symbol?
rw_add(Ctx,Var,RW):- atom(Var),!,  get_var_tracker(Ctx,Var,Dict),arginfo_incr(RW,Dict).
rw_add(_Ctx,_Var,_RW).

% actual var
add_tracked_var(Ctx,Atom,Var):-
   get_var_tracker(Ctx,Atom,Dict),
   Vars=Dict.vars,
   sort([Var|Vars],NewVars),
   b_set_dict(vars,Dict,NewVars).
  
get_var_tracker(Ctx0,Atom,Dict):- get_attr(Ctx0,tracker,Ctx), must(sanity(atom(Atom))),oo_get_attr(Ctx,var_tracker(Atom),Dict),(is_dict(Dict)->true;(trace,oo_get_attr(Ctx,var_tracker(Atom),_SDict))).
get_var_tracker(Ctx0,Atom,Dict):-  get_attr(Ctx0,tracker,Ctx),Dict=rw{name:Atom,r:0,w:0,vars:[]},oo_put_attr(Ctx,var_tracker(Atom),Dict),!.


locally_let([N=V|More],G):- castify(V,Value),!,locally_let([N=Value|More],G).
locally_let([N=V|More],G):- castify(N,Symbol),!,locally_let([Symbol=V|More],G).
locally_let([N=V|More],G):- 
 symbol_value(N,Was),
  setup_call_cleanup(
     set_symbol_value(N,V),
     once(locally($(N)=V,locally_let(More,G))),
     set_symbol_value(N,Was)).
   
locally_let([],G):- call(G).
locally_let(N=V,G):-!,locally_let([N=V],G).

castify(O,O):- \+compound(O),!,fail.
castify(str(O),S):-!, castify1(O,M),cl_string(M,S).
castify(sym(O),S):-!, castify1(O,M),reader_intern_symbols(M,S).
castify(P,S):- P=..[F,M],!, castify1(M,MM),must(get_opv(MM,F,S)).

castify1(O,O):- \+compound(O),!.
castify1(O,O):- is_list(O),!.
castify1(I,O):- castify(I,O).

extract_var_atom([_,RVar|_],RVar):-atomic(RVar).
extract_var_atom(Var,Var).

compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm, Atom2| Rest], Body):- is_parallel_op(SetQ),!, 
   pairify([Var, ValueForm, Atom2| Rest],Atoms,Forms),
   maplist(expand_ctx_env_forms(Ctx,Env),Forms,BodyS1,Results),
   maplist(set_with_prolog_var(Ctx,Env,SetQ),Atoms,Results,BodyS2),   
   ((op_return_type(SetQ,RT),RT=name) ->  last(Atoms,Result) ; last(Results,Result)),
   append(BodyS1,BodyS2,BodyS),list_to_conjuncts(BodyS,Body).


compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm, Atom2| Rest], Body):- is_pair_op(SetQ), 
   must_compile_body(Ctx,Env,_ResultU,[SetQ, Var, ValueForm], Body1),
   must_compile_body(Ctx,Env,Result,[SetQ, Atom2| Rest],  Body2),
   Body = (Body1 , Body2).

compile_assigns(Ctx,Env,Result,[Defvar, Var], Body):- is_def_nil(Defvar),!,
  compile_assigns(Ctx,Env,Result,[Defvar, Var , nil],Body).

compile_assigns(Ctx,Env,Result,[Getf, Var| ValuesForms], Body):- is_place_op(Getf),     
	must_maplist(expand_ctx_env_forms(Ctx,Env),ValuesForms, ValuesBody,ResultVs),
        list_to_conjuncts([true|ValuesBody],BodyS),!,
        debug_var([Getf,'_R'],Result),
        debug_var([Getf,'_Env'],Env),
        extract_var_atom(Var,RVar),
        compile_place(Ctx,Env,UsedVar,Var,Code),
        (Var\==RVar -> rw_add(Ctx,RVar,r) ; (is_only_read_op(Getf)->rw_add(Ctx,RVar,r);rw_add(Ctx,RVar,w))),
        Body = (BodyS,Code,place_op(Env,Getf, UsedVar, ResultVs,Result)).

compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm, String], (Code,Body)):- 
        string(String),is_def_maybe_docs(SetQ),
        Code = asserta(doc:doc_string(Var,_Package,variable,String)),
	!, compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm], Body).


compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm], Body):- is_symbol_setter(Env,SetQ),
        rw_add(Ctx,Var,w),
        debug_var('Env',Env),
        !,	
	must_compile_body(Ctx,Env,ResultV,ValueForm, ValueBody),
        ((op_return_type(SetQ,RT),RT=name) ->  =(Var,Result) ; =(ResultV,Result)),
        Body = (ValueBody, symbol_setter(Env,SetQ, Var, ResultV)).


% catches an internal error in this compiler 
compile_symbol_getter(Ctx,Env,Result,Var, Body):- Var==mapcar,!, 
  dbmsg(compile_symbol_getter(Ctx,Env,Result,Var, Body)), lisp_dump_break.


compile_symbol_getter(Ctx,Env,Value, Var, Body):-  must(atom(Var)),!,
        debug_var([Var,'_Get'],Value),
        add_tracked_var(Ctx,Var,Value),
        rw_add(Ctx,Var,r),
        debug_var('Env',Env),
        Body = symbol_value(Env, Var, Value).   

% compile_place(Ctx,Env,Result,Var,Code).
compile_place(_Ctx,_Env,[value,Var],Var,true):- \+ is_list(Var),!.
%compile_place(_Ctx,_Env,[Place,Var],[Place,Var],true):- atom(Var),!.
compile_place(Ctx,Env,[Place|VarResult],[Place|VarEval],Code):- compile_each(Ctx,Env,VarResult,VarEval,Code).
%compile_place(Ctx,Env,[Place,Var,Result],[Place,Var|Eval],Code):- compile_forms(Ctx,Env,Result,Eval,Code).
%compile_place(_Ctx,_Env,Var,Var,true).

compile_each(_Ctx,_Env,[],[],true).
compile_each(Ctx,Env,[VarR|Result],[Var|Eval],Code):-
  compile_body(Ctx,Env,VarR,Var,Code0),
  compile_each(Ctx,Env,Result,Eval,Code1),
  conjoin_0(Code0,Code1,Code).

rwstate:attr_unify_hook(_,_):-fail.


extract_variable_value([Val|Vals], FoundVal, Hole):-
		var(Vals)
	->	FoundVal = Val,
		Hole = Vals
	;	extract_variable_value(Vals, FoundVal, Hole).


bind_dynamic_value(Env,Var,Result):- set_symbol_value(Env,Var,Result).


symbol_value(Var,Value):- env_current(Env), symbol_value(Env,Var,Value).
symbol_value(Env,Var,Value):-
  symbol_value_or(Env,Var,
    symbol_value_error(Env,Var,Value),Value).

symbol_value_or(Env,Var,G,Value):-
 ensure_env(Env), 
 (symbol_value0(Env,Var,Value) -> true ; G).

symbol_value0(Env,Var,Value):-  bvof(bv(Var, Value),_,Env).
symbol_value0(_Env,Var,_Value):- notrace((nonvar(Var),is_functionp(Var),wdmsg(is_functionp(Var)))),!,lisp_dump_break.
symbol_value0(_Env,Var,Result):- get_opv(Var, value, Result),!.
symbol_value0(_Env,Var,Result):- atom(Var),nb_current(Var,Result),!.
symbol_value0(Env,[Place,Obj],Result):- place_op(Env,getf,[Place,Obj],[],Result).


symbol_value_error(_Env,Var,_Result):- lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).


push_values([V1|Push],V1):- must(nonvar(Push)),nb_setval('$mv_return',[V1|Push]).

bvof(_,_,T):- notrace(((var(T);T==[tl]))),!,fail.
bvof(E,M,T):-E=T,!,M=T.
bvof(E,M,[L|L2]):- ((nonvar(L),bvof(E,M,L))->true;(nonvar(L2),bvof(E,M,L2))).



set_symbol_value(Env,Var,Result):-var(Result),!,symbol_value(Env,Var,Result).

set_symbol_value(Env,Var,Result):- ensure_env(Env),!,
     (bvof(bv(Var,_),BV,Env)
      -> nb_setarg(2,BV,Result)
      ;	( 
        (get_opv(Var, value, _Old) 
           -> update_opv(Var, value, Result) 
           ; set_symbol_value_last_chance(Env,Var,Result)))).

set_symbol_value_last_chance(_Env,Var,Result):- nb_current(Var,_)-> nb_setval(Var,Result),!.
%set_symbol_value_last_chance(Env,Var,Result):- add_to_env(Env,Var,Result),!.
set_symbol_value_last_chance(_Env,Var,Result):- set_opv(Var, value, Result),!.
set_symbol_value_last_chance(_Env,Var,_Result):- 
  lisp_error_description(atom_does_not_exist, ErrNo, _),throw(ErrNo, Var).




env_sym_arg_val(Env,Var,InValue,Value):-
  set_symbol_value(Env,Var,InValue),
  Value=InValue,!.

env_sym_arg_val(Env,Var,InValue,Value):- !,
  symbol_value_or(Env,Var,(nonvar(InValue),InValue=Value),Value)-> true;
    lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).

%TODO Make it a constantp
symbol_setter(Env,defconstant, Var, Result):- 
   set_symbol_value(Env,Var,Result),
   set_opv(Var,declared_as,defconstant).
symbol_setter(Env,defconst, Var, Result):- 
  symbol_setter(Env,defconstant, Var, Result).

symbol_setter(Env,defparameter, Var, Result):- 
   set_opv(Var,declared_as,defparameter),
   set_symbol_value(Env,Var,Result).

symbol_setter(_Env,defvar, Var, Result):-   
   (get_opv(Var, value, _) -> true ; update_opv(Var, value, Result)),
   set_opv(Var,declared_as,defvar).

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

is_only_read_op(getf).

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

set_with_prolog_var(Ctx,Env,SetQ,Var,Result,symbol_setter(Env,SetQ, Var, Result)):-
  rw_add(Ctx,Var,w).

expand_ctx_env_forms(Ctx, Env,Forms,Body, Result):- 
   must_compile_body(Ctx,Env,Result,Forms, Body).
:- fixup_exports.




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

% local symbol?
rw_add(Ctx,Var,RW):- atom(Var),!,  get_var_tracker(Ctx,Var,Dict),arginfo_incr(RW,Dict).
rw_add(_Ctx,_Var,_RW).

% actual var
add_tracked_var(Ctx,Atom,Var):-
   get_var_tracker(Ctx,Atom,Dict),
   Vars=Dict.vars,
   sort([Var|Vars],NewVars),
   b_set_dict(vars,Dict,NewVars).

%get_var_tracker(_,Atom,rw{name:Atom,r:0,w:0,p:0,ret:0,u:0,vars:[]}):-!. % mockup
get_var_tracker(Ctx0,Atom,Dict):- get_tracker(Ctx0,Ctx), always(sanity(atom(Atom))),get_env_attribute(Ctx,var_tracker(Atom),Dict),(is_dict(Dict)->true;(trace,oo_get_attr(Ctx,var_tracker(Atom),_SDict))).
get_var_tracker(Ctx0,Atom,Dict):- get_tracker(Ctx0,Ctx),Dict=rw{name:Atom,r:0,w:0,p:0,ret:0,u:0,vars:[]},set_env_attribute(Ctx,var_tracker(Atom),Dict),!.


extract_var_atom([_,RVar|_],RVar):-atomic(RVar).
extract_var_atom(Var,Var).

:- discontiguous compile_assigns/5.

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

compile_assigns(Ctx,Env,Result,[Getf|ValuePlace], Body):- fail, is_place_op_verbatum(Getf),     
        debug_var([Getf,'_R'],Result),
        debug_var([Getf,'_Env'],Env),
        place_extract(ValuePlace,Value,Place),
        extract_var_atom(Place,RVar),
        (is_only_read_op(Getf)->rw_add(Ctx,RVar,r);rw_add(Ctx,RVar,w)),
        Body = (set_place(Env,Getf, Place, Value, Result)).


portray(List):- notrace((nonvar(List),List=[_,_],sub_term(E,List),ground(E),E = ((environ=W)),write(environment(W)))).


compile_assigns(Ctx,Env,Result,[Setf, Place|ValuesForms], (Part0,Body)):- is_place_write(Setf),
     get_setf_expander_get_set(Ctx,Env,Place,GET,SET,Part0),
     make_place_op(Ctx,Env,Result,Setf,GET,ValuesForms,SET,Body).

compile_assigns(Ctx,Env,Result,[setf, Place, ValuesForms], (Part0,Part1,Part4)):- \+ atom(Place),
     get_setf_expander_get_set(Ctx,Env,Place,_,SET,Part0),     
     must_compile_body(Ctx,Env,New,ValuesForms,Part1),
     append(SET,[New],LispOp),
     must_compile_body(Ctx,Env,Result,LispOp,Part4).

compile_assigns(Ctx,Env,Result,[getf, Place], (Part0,Part4)):- 
     get_setf_expander_get_set(Ctx,Env,Place,GET,_SET,Part0),     
     must_compile_body(Ctx,Env,Result,GET,Part4).


% get_setf_expander_get_set(_Ctx,_Env,[car,Var],[car,Var],[set_car,Var],  true):- atom(Var),!.
get_setf_expander_get_set(Ctx,Env,[OP,LVar|EXTRA],[OP,GET|EXTRA],[INVERSE,GET|EXTRA],  Body):- setf_inverse_op(OP,INVERSE),
   must_compile_body(Ctx,Env,GET,LVar, Body), (var(GET)->put_attr(GET,preserved_var,t); true).

get_setf_expander_get_set(Ctx,Env,LVar,GET,[sys_set_symbol_value,GET], true):- atom(LVar),lookup_symbol_macro(Ctx,Env,LVar,GET),!.
get_setf_expander_get_set(_,_,LVar,GET,[sys_set_symbol_value,GET], true):- \+ atom(LVar),atom(LVar),LVar=GET.

lookup_symbol_macro(Ctx,Env,LVar,GET):- get_ctx_env_attribute(Ctx,Env,symbol_macro(LVar),GET).

setf_inverse_op(car,rplaca).
setf_inverse_op(cdr,rplacd).
setf_inverse_op(Sym,Inverse):- 
   symbol_prefix_and_atom(Sym,FunPkg,Name),
   member(SETPRefix,['setf','set','pf_set']),
   atomic_list_concat([FunPkg,SETPRefix,Name],'_',Inverse),
   find_lisp_function(Inverse,_Arity,_Fn).


%  (defun p () (incf (car p)))
% (defmacro incf (place &optional (delta 1))`(setf ,place (+ ,place ,delta)))
make_place_op(Ctx,Env,Result,incf,GET,LV,SET,Body) :- 
 always((
   value_or(LV,Value,1),!,
   must_compile_body(Ctx,Env,ValueR,Value,Part1),
   must_compile_body(Ctx,Env,Old,GET,Part2),
   Part3 = (New is Old+ ValueR),
   append(SET,[New],LispOp),
   must_compile_body(Ctx,Env,Result,LispOp,Part4),
   Body = (Part1,Part2,Part3,Part4))).


compile_assigns(Ctx,Env,Result,[Getf, Var| ValuesForms], Body):- is_place_op(Getf),     
	must_maplist(expand_ctx_env_forms(Ctx,Env),ValuesForms, ValuesBody,ResultVs),
        list_to_conjuncts([true|ValuesBody],BodyS),!,
        debug_var([Getf,'_R'],Result),
        debug_var([Getf,'_Env'],Env),
        extract_var_atom(Var,RVar),
        compile_place(Ctx,Env,UsedVar,Var,Code),
        (Var\==RVar -> rw_add(Ctx,RVar,r) ; (is_only_read_op(Getf)->rw_add(Ctx,RVar,r);rw_add(Ctx,RVar,w))),
        Body = (BodyS,Code,set_place(Env,Getf, UsedVar, ResultVs,Result)).

compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm, StringL], (Code,Body)):- 
        is_stringp(StringL),to_prolog_string(StringL,String),is_def_maybe_docs(SetQ),
        (atom(Var)->cl_symbol_package(Var,Package);reading_package(Package)),
        Code = assert_lsp(Var,doc:doc_string(Var,Package,variable,String)),
	!, compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm], Body).


compile_assigns(Ctx,Env,Result,[SetQ, Var, ValueForm], Body):- is_symbol_setter(Env,SetQ),
        rw_add(Ctx,Var,w),
        debug_var('AEnv',Env),
        !,	
	must_compile_body(Ctx,Env,ResultV,ValueForm, ValueBody),
        ((op_return_type(SetQ,RT),RT=name) ->  =(Var,Result) ; =(ResultV,Result)),
        Body = (ValueBody, set_var(Env,SetQ, Var, ResultV)).


% catches an internal error in this compiler 
compile_symbol_getter(Ctx,Env,Result,Var, Body):- Var==mapcar,!, 
  dbginfo(compile_symbol_getter(Ctx,Env,Result,Var, Body)), lisp_dump_break.


compile_symbol_getter(Ctx,Env,Value, Var, Body):-  always((atom(Var),!,
        debug_var([Var,'_Get'],Value),
        add_tracked_var(Ctx,Var,Value),
        rw_add(Ctx,Var,r),
        %debug_var('_GEnv',Env),
        Body = get_var(Env, Var, Value))).

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
  conjoin_0(Ctx,Code0,Code1,Code).
                  
%rwstate:attr_unify_hook(_,_):-!,fail.
%rwstate:attr_unify_hook(_,V):-always(var(V)),fail.
nonplainvar(V):- nonvar(V);attvar_non_vn(V).
attvar_non_vn(V):- attvar(V),get_attr(V,searchvar,_),!.
attvar_non_vn(V):- attvar(V),copy_term(V,VV),del_attr(VV,vn),del_attr(VV,rwstate),del_attr(VV,varuse),
  (get_attrs(VV,[]);\+attvar(VV)).



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

%TODO Make it a constantp
set_var(Env,defconstant, Var, Result):- 
   set_var(Env,Var,Result),
   set_opv(Var,declared_as,defconstant).
set_var(Env,defconst, Var, Result):- 
  set_var(Env,defconstant, Var, Result).

set_var(Env,defparameter, Var, Result):- 
   set_opv(Var,declared_as,defparameter),
   set_var(Env,Var,Result).

set_var(_Env,defvar, Var, Result):-   
   (get_opv(Var, value, _) -> true ; update_opv(Var, value, Result)),
   set_opv(Var,declared_as,defvar).

set_var(Env,setq, Var, Result):- !, set_var(Env,Var,Result).

set_var(Env,psetq, Var, Result):- !, set_var(Env,setq, Var, Result).


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

is_place_write(setf).
is_place_write(psetf).
is_place_write(incf).
is_place_write(decf).
is_place_write(rotatef).
is_place_write(shiftf).
is_place_write(V):- is_place_op_verbatum(V).
is_place_write(P):- is_place_op(P), \+ is_only_read_op(P).

is_place_op_verbatum(rotatefsdfsdfsdfsdfsdffs).

is_place_op(setf).
is_place_op(psetf).
is_place_op(getf).
is_place_op(incf).
is_place_op(decf).
is_place_op(rotatef).
is_place_op(shiftf).
is_place_op(push).
is_place_op(pushnew).
is_place_op(pop).

is_any_place_op(P):-is_place_op_verbatum(P).
is_any_place_op(P):-is_parallel_op(P).
is_any_place_op(P):-is_place_op(P).



is_parallel_op(psetf).
is_parallel_op(psetq).

pairify([],[],[]).
pairify([Var, ValueForm | Rest],[Var | Atoms],[ValueForm | Forms]):-
   pairify(Rest,Atoms,Forms).

set_with_prolog_var(Ctx,Env,SetQ,Var,Result,set_var(Env,SetQ, Var, Result)):-
  rw_add(Ctx,Var,w).

expand_ctx_env_forms(Ctx, Env,Forms,Body, Result):- 
   must_compile_body(Ctx,Env,Result,Forms, Body).
:- fixup_exports.




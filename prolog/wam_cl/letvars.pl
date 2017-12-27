/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (lisp_compiler.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 * Changes since 2001:
 *
 *
 *******************************************************************/
:- module(l3ts, []).
:- set_module(class(library)).
:- include('header').


normalize_let1([Variable, Form],[bind, Variable, Form]).
normalize_let1([bind, Variable, Form],[bind, Variable, Form]).
normalize_let1( Variable,[bind, Variable, []]).


%  (LET  () .... )
compile_let(Ctx,Env,Result,[_LET, []| BodyForms], Body):- !, must_compile_progn(Ctx,Env,Result, BodyForms, Body).

%  (LET ((*package* (find-package :keyword))) *package*)
compile_let(Ctx,Env,Result,[let, [[Var,VarInit]]| BodyForms], (VarInitCode,locally_set(Var,Value,BodyCode))):-
 is_special_var(Var),!,
 must_compile_body(Ctx,Env,Value,VarInit,VarInitCode),
  must_compile_progn(Ctx,Env,Result,BodyForms,BodyCode).

%  (LET ((local 1)) local)
compile_let(Ctx,Env,Result,[let, [[Var,VarInit]]| BodyForms], (VarInitCode,locally_bind(Env,Var,Value,BodyCode))):- fail,
 atom(Var),
 must_compile_body(Ctx,Env,Value,VarInit,VarInitCode),
  must_compile_progn(Ctx,Env,Result,BodyForms,BodyCode).


%  (LET  (....) .... )
compile_let(Ctx,Env,Result,[LET, NewBindingsIn| BodyForms], Body):- !,
  always((
      debug_var("BEnv",BindingsEnvironment),
      debug_var('LetResult',Result),
      debug_var("LEnv",CEnv),
                         
        freeze(Var,ignore((var(Value),debug_var('_Init',Var,Value)))),
        freeze(Var,ignore((var(Value),add_tracked_var(Ctx,Var,Value)))),
        
        must_maplist(normalize_let1,NewBindingsIn,NewBindings),
	zip_with(Variables, ValueForms, [Variable, Form, [bind, Variable, Form]]^true, NewBindings),
	expand_arguments(Ctx,Env,'funcall',1,ValueForms, VarInitCode, Values),

        zip_with(Variables, Values, [Var, Value, BV]^make_letvar(LET,Var,Value,BV),Bindings),

        % rescue out special bindings
        partition(is_bv2,Bindings,LocalBindings,SpecialBindings),  

        ignore((member(VarN,[Variable,Var]),atom(VarN),var(Value),debug_var([VarN,'_Let'],Value),fail)),                
        add_alphas(Ctx,Variables),
        let_body(Env,BindingsEnvironment,LocalBindings,SpecialBindings,BodyFormsBody,MaybeSpecialBody),
          make_env_append(Ctx,Env,CEnv,[LocalBindings|Env],EnvAssign),
          must_compile_progn(Ctx,CEnv,BResult,BodyForms, BodyFormsBody),          
         Body = (VarInitCode, EnvAssign, MaybeSpecialBody,G))),
  ensure_assignment(Result=BResult,G).

is_bv2(BV):- \+ \+ BV=bv(_,_).
% No Locals
let_body(Env,BindingsEnvironment,[],SpecialBindings,BodyFormsBody,MaybeSpecialBody):- fail,!,
  Env=BindingsEnvironment, debug_var("LEnv",BindingsEnvironment),
  maybe_specials_in_body(SpecialBindings,BodyFormsBody,MaybeSpecialBody).

% Some Locals
let_body(_Env,_BindingsEnvironment,_LocalBindings,SpecialBindings,BodyFormsBody,(MaybeSpecialBody)):-!,
  
  %nb_set_last_tail(LocalBindings,Env),
  maybe_specials_in_body(SpecialBindings,BodyFormsBody,MaybeSpecialBody).
  

% No Specials
maybe_specials_in_body([],BodyFormsBody,BodyFormsBody).
% Some Specials
maybe_specials_in_body(SpecialBindings,BodyFormsBody,SpecialBody):-
  mize_prolog_code(maplist(save_special,SpecialBindings),PreCode),
  mize_prolog_code(maplist(restore_special,SpecialBindings),PostCode),
   SpecialBody = (PreCode, BodyFormsBody, PostCode). 
    % Some SAFE specials -> SpecialBody = setup_call_cleanup(PreCode, BodyFormsBody, PostCode).

is_special_var(Var):- atom(Var),!,get_opv_i(Var,value,_).

make_letvar(ext_letf,Place,Value,place(Place,Value,_OldValue)):- is_list(Place).
make_letvar(_,Var,Value,sv(Var,Value,value,_OldValue)):- is_special_var(Var),!.
make_letvar(_,Var,Value,bv(Var,Value)).
%make_letvar(_,Var,Value,vv(Var,Value)).


  % maybe_bind_local(Ctx,Env,[Var,ValueForm|_],Body,(VarInitCode,locally_bind(Env,Var,Value,Body))):- 

%save_special(vv(_,_)).
save_special(sv(Var,New,Prop,Old)):- get_opv(Var,Prop,Old),set_opv(Var,Prop,New).
save_special(place(Place,New,Old)):- get_place(Place,Old),set_place(Place,New).

%restore_special(vv(_,_)).
restore_special(sv(Var,_,Prop,Old)):- set_opv(Var,Prop,Old).
restore_special(place(Place,_New,Old)):- set_place(Place,Old).


/*
compile_let_9(Ctx,Env,Result,[let, [Bind1|NewBindingsIn]| BodyForms], Call):- 
  maybe_bind_special(Ctx,Env,Bind1,Body,Call),!,
  compile_let(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body).
compile_let_9(Ctx,Env,Result,[let, [Bind1|NewBindingsIn]| BodyForms], Call):- 
  maybe_bind_local(Ctx,Env,Bind1,Body,Call),!,
  compile_let(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body).
compile_let_9(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body):- 
  compile_let(LET,Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body).

%maybe_bind_special(_Ctx,_Env,Var,Body,locally_let(Var=[],Body)):- is_sboundp(Var),!.
maybe_bind_special(Ctx,Env,Var,Body,Out):- is_sboundp(Var),!,maybe_bind_special(Ctx,Env,[Var,[]],Body,Out).
maybe_bind_special(Ctx,Env,[Var,ValueForm|_],Body,(VarInitCode,locally_set(Var,Value,Body))):- 
  debug_var([Var,'_SLet'],Value),
  is_sboundp(Var),!,must_compile_body(Ctx,Env,Value,ValueForm,VarInitCode).

maybe_bind_local(Ctx,Env,Var,Body,Out):- atom(Var),!,maybe_bind_local(Ctx,Env,[Var,[]],Body,Out).
maybe_bind_local(Ctx,Env,[Var,ValueForm|_],Body,(VarInitCode,locally_bind(Env,Var,Value,Body))):- 
    %debug_var("LEnv",BindingsEnvironment),
    debug_var([Var,'_Let'],Value),
    must_compile_body(Ctx,Env,Value,ValueForm,VarInitCode).
*/



%  current_env(Env),
  %set_var(Env,Var,Value).

%   zip_with(Xs, Ys, Pred, Zs)
%   is true if Pred(X, Y, Z) is true for all X, Y, Z.

zip_with([], [], _, []).
zip_with([X|Xs], [Y|Ys], Pred, [Z|Zs]):-
	lpa_apply(Pred, [X, Y, Z]),
	zip_with(Xs, Ys, Pred, Zs).


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

                  
%rwstate:attr_unify_hook(_,_):-!,fail.
%rwstate:attr_unify_hook(_,V):-always(var(V)),fail.
nonplainvar(V):- nonvar(V);attvar_non_vn(V).
attvar_non_vn(V):- attvar(V),get_attr(V,searchvar,_),!.
attvar_non_vn(V):- attvar(V),copy_term(V,VV),del_attr(VV,vn),del_attr(VV,rwstate),del_attr(VV,varuse),
  (get_attrs(VV,[]);\+attvar(VV)).


:- fixup_exports.


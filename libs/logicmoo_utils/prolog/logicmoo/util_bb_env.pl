/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles  logicmoo@gmail.com ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2021/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
:- module(logicmoo_util_bb_env, [
   abolish_and_make_static/2,
   add_push_prefix_arg/4,
   % bb:'$sourcefile_info_env'/1,
   % baseKB:decl_env_mpred/2, a-box:defaultTBoxMt/1,
   clause_to_hb/3,
   clause_to_hb0/3,
   decl_env_mpred/2,
   decl_env_mpred_dom/2,
   decl_env_mpred_fa/4,
   decl_env_mpred_real/4,
   decl_env_mpred_task/2,
   do_prefix_arg/4,
   env_1_info/2,
   env_assert/1,
   env_asserta/1,
   (env_call)/1,
   env_clear/1,
   env_consult/1,
   env_get/1,
   env_info/1,
   env_info/2,
   env_learn_pred/2,
   env_meta_term/1,
   env_mpred_op/1,
   env_mpred_op/2,
   env_mpred_op/3,
   env_mpred_op_1/3,
   env_predinfo/2,
   env_push_args/4,
   env_push_argsA/4,
   env_recorded/2,
   env_retract/1,
   env_retractall/1,
   env_set/1,
   env_shadow/2,
   env_source_file/1,
   env_term_expansion/2,
   get_env_ctx/1,
   get_env_expected_ctx/1,
   get_env_source_ctx/2,
   get_mp_arity/2,
   get_mpred_stubType/3,
   harvest_preds/2,
   hb_to_clause_env/3,
   hb_to_clause_env0/3,
   in_dyn/2,
   in_dyn_pred/2,
   inside_file_bb/1,
   lg_op2/3,
   ppi/1,
   pred_1_info/4,
   push_prefix_arg/4,
   term_expansion_add_context/5
]).
/** <module> Utility LOGICMOO_UTIL_BB_ENV
This module sets up the blackboard environment.  
- @author Douglas R. Miles
- @license LGPL
*/

:- kb_global(baseKB:mpred_prop/4).

 :- meta_predicate % cmt :-
        env_call(+),
        env_consult(:),
        env_mpred_op(1, :),
        env_mpred_op(?, 1, :),
        env_mpred_op_1(?, 1, :),
        env_shadow(1, ?).

%:- (multifile bb:'$sourcefile_info_env'/1, a-box:defaultTBoxMt/1, env_push_args/4).
:- (module_transparent env_consult/1, env_mpred_op/2, env_mpred_op/3, env_mpred_op_1/3, env_shadow/2).
%:- export((clause_to_hb0/3, env_mpred_op_1/3, hb_to_clause_env0/3, lg_op2/3)).
%:- (dynamic bb:'$sourcefile_info_env'/1, a-box:defaultTBoxMt/1, env_push_args/4, env_source_file/1, in_dyn/2).
%:- kb_shared((bb:'$sourcefile_info_env'/1, abox:defaultTBoxMt/1, env_push_args/4, env_source_file/1, in_dyn/2)).




:- thread_local(t_l:push_env_ctx/0).
:- dynamic(bb:'$sourcefile_info_env'/1).
:- multifile(bb:'$sourcefile_info_env'/1).

:-asserta((t_l:push_env_ctx)).

:- meta_predicate(env_call(+)).

:- dynamic(env_push_args/4).
:- multifile(env_push_args/4).
:- thread_local(t_l:db_spy/0).

:- thread_local((canDoTermExp/0)).
:- retractall(canDoTermExp).


% :- discontiguous((env_call/1,env_assert/1,env_asserta/1,env_retract/1,env_retractall/1)).

% 401,809,449 inferences, 52.592 CPU in 53.088 seconds (99% CPU, 7640071 Lips)

env_call(P):- call(ocl:P),ppi(P).
env_assert(P):- call(assert,ocl:P),!.
env_asserta(P):- call(asserta,ocl:P),!.
env_retract(P):-  call(retract,ocl:P).
env_retractall(F/A):- functor(P,F,A),!,retractall(ocl:P),!.
env_retractall(P):- call(retractall,ocl:P),!.

/*
env_call(P):- env_mpred_op(call,P),ppi(P).
env_assert(P):- must_det(env_mpred_op(assert,P)),!.
env_asserta(P):- must_det(env_mpred_op(asserta,P)),!.
env_retract(P):- env_mpred_op(retract,P).
env_retractall(F/A):- functor(P,F,A),!,env_retractall(P),!.
env_retractall(P):- must(env_mpred_op(retractall,P)),!.
*/

env_clear(baseKB(Dom)):-nonvar(Dom),!,env_clear(Dom).
env_clear(Dom):- forall(baseKB:mpred_prop(M,F,A,Dom),env_mpred_op(retractall(M:F/A))).
env_mpred_op(OP_P):- OP_P=..[OP,P],env_mpred_op(OP,P).

:- module_transparent(env_mpred_op/2).
:- meta_predicate env_mpred_op(1,:).
env_mpred_op(OP,P):- var(OP),!,P.
%TODO env_mpred_op(OP,P):- baseKB:mpred_prop(M,F,A,P),!,forall(baseKB:mpred_prop(M,F,A,P),(nop(dtrace),env_mpred_op(OP,F/A) )).
%TODO env_mpred_op(OP,F):- baseKB:mpred_prop(M,F,_,_),!,forall(baseKB:mpred_prop(M,F,A,_),(nop(dtrace),env_mpred_op(OP,F/A) )).
env_mpred_op(OP,F/A):-integer(A),atom(F),!,functor(P,F,A),!,env_mpred_op(OP,P).
env_mpred_op(OP,P):- t_l:push_env_ctx, do_prefix_arg(P, ZZ, PP, _Type),P\==PP,!,get_env_ctx(ZZ),call(OP,/*ocluser*/ocl:PP).
env_mpred_op(OP,P):- functor_h(P,F,A),must(get_mpred_stubType(F,A,ENV)),!,env_mpred_op(ENV,OP,P).
env_mpred_op(OP,P):- append_term(OP,P,CALL),current_predicate(_,CALL),!,show_call(why,/*ocluser*/ocl:CALL).
env_mpred_op(OP,P):- dtrace,trace_or_throw(unk_env_mpred_op(OP,P)).

env_shadow(OP,P):-baseKB:call(OP,P).

:- dynamic( in_dyn/2).
in_dyn(_DB,Call):- var(Call),!,get_mp_arity(F,A),functor(Call,F,A),( predicate_property(Call,_) -> loop_check(Call)).
in_dyn(_DB,Call):- functor(Call,F,A), get_mp_arity(F,A), predicate_property(Call,_), !, loop_check(Call).
in_dyn_pred(_DB,Call):- var(Call),!,get_mp_arity(F,A),functor(Call,F,A),( predicate_property(Call,_) -> loop_check(Call)).
in_dyn_pred(_DB,Call):- functor(Call,F,A), get_mp_arity(F,A), predicate_property(Call,_), !, loop_check(Call).

:- dynamic(get_mp_arity/2).

get_mp_arity(F,A):- defaultAssertMt(M),if_defined(M:arity(F,A)).
get_mp_arity(F,A):- get_current_default_tbox(M),if_defined(M:arity(F,A)).
get_mp_arity(F,A):- arity_no_bc(F,A).

% baseKB:mpred_prop(M,F,A,Prop):- get_current_default_tbox(M),M:isa(F,Prop),get_mp_arity(F,A).

get_mpred_stubType(_,_,dyn):-!.
get_mpred_stubType(F,A,StubOut):-    
   clause_b(mpred_prop(_M,F,A,stubType(Stub))),!,must(StubIn=Stub),
   % PREVENTS FAILURE
   nop(StubIn==dyn->true;dmsg(get_mpred_stubType(F,A,StubIn))),
   StubOut=dyn.

get_mpred_stubType(F,A,dyn):- clause_b(mpred_prop(_M,F,A,dyn)).
get_mpred_stubType(_,_,dyn).

:- ain(isa_kb:box_prop(l)).
:- ain(isa_kb:box_prop(g)).
:- ain(isa_kb:box_prop(dyn)).
:- thread_initialization(nb_setval(disabled_env_learn_pred,false)).

:- thread_local(t_l:env_ctx/2).

:- export(decl_env_mpred/2).
%:-module_transparent(decl_env_mpred/2).

decl_env_mpred(_,[]):-!.
decl_env_mpred([],_):-!.
decl_env_mpred(Props,[H|T]):-!,decl_env_mpred(Props,H),decl_env_mpred(Props,T).
decl_env_mpred(Props,(H,T)):-!,decl_env_mpred(Props,H),decl_env_mpred(Props,T).


decl_env_mpred([H],Pred):-!,decl_env_mpred(H,Pred).
decl_env_mpred([H|T],Pred):-!,decl_env_mpred(H,Pred),decl_env_mpred(T,Pred).
decl_env_mpred((H,T),Pred):-!,decl_env_mpred(H,Pred),decl_env_mpred(T,Pred).

decl_env_mpred(baseKB(KB),_):- ain(isa_abox(KB)),fail.
decl_env_mpred(stubType(dyn),Pred):-!, decl_env_mpred(dyn,Pred).

decl_env_mpred(CMPD,Pred):-  fail, compound(CMPD),CMPD=..[_|CMPDL],
   decl_env_mpred(CMPDL,Pred),
   get_functor(Pred,F,A),
   decl_env_mpred_fa(CMPD,Pred,F,A),!.

decl_env_mpred(Prop,Pred):- get_functor(Pred,F,A),
   decl_env_mpred_fa(Prop,Pred,F,A).

decl_env_mpred_dom(Props,Preds):-
 must(locally(t_l:env_ctx(dom,_CurrentDomain), 
    decl_env_mpred([dom|Props],Preds))).
decl_env_mpred_task(Props,Preds):-
 must(locally(t_l:env_ctx(task,_CurrentTask), 
    decl_env_mpred([task|Props],Preds))).


% abolish_and_make_static(_F,_A):-!.
abolish_and_make_static(F,A):-
 ignore(baseKB:mpred_prop(M,F,A,_)),
 ignore(baseKB:current_predicate(M:F/A)),

  must_det_l((
   retractall(get_mp_arity(F,A)),
   retractall(arity(F,A)),
   retractall(baseKB:mpred_prop(M,F,A,_)),
  functor(H,F,A),
  abolish(M:F,A),
  asserta((H:-trace_or_throw(H))),
  M:compile_predicates([F/A]),lock_predicate(H))).

decl_env_mpred_fa(Prop,_Pred,F,A):-  t_l:push_env_ctx,    
   t_l:env_ctx(Type,Prefix),A1 is A+1,
  must_det_l((functor(Pred1,F,A1),functor(Pred,F,A),
   must(add_push_prefix_arg(Pred,Type,Prefix,Pred1)),
   abolish_and_make_static(F,A),
   must((decl_env_mpred_real(Prop,Pred1,F,A1),
   if_defined(arity(F,AA)),
   must(arity(F,A1)==arity(F,AA)))))),!.
decl_env_mpred_fa(Prop,Pred,F,A):-
   decl_env_mpred_real(Prop,Pred,F,A).

decl_env_mpred_real(Prop,Pred,F,A):-
  ignore(baseKB:mpred_prop(M,F,A,_)),
  ignore(M=ocl),
  (Prop==task->(thread_local(/*ocluser*/ocl:F/A));true),
  (Prop==dyn->(dynamic(/*ocluser*/ocl:F/A));true),
  (Prop==cache->'$set_pattr'(ocl:Pred, pred, (volatile));true),
  (Prop==dom->(multifile(/*ocluser*/ocl:F/A));true),
  baseKB:export(/*ocluser*/ocl:F/A),
  if_defined(decl_mpred(Pred,Prop),ain(baseKB:box_prop(F,Prop))),
  ain(isa_kb:box_prop(Prop)), 
  ain(get_mp_arity(F,A)),
  ain(arity(F,A)),!,
  % dtrace,
  ain(baseKB:mpred_prop(M,F,A,Prop)).


env_learn_pred(_,_):-nb_getval(disabled_env_learn_pred,true),!.
env_learn_pred(ENV,P):-baseKB:decl_env_mpred(ENV,P).

env_recorded(call,Val) :- recorded(Val,Val).
env_recorded(assert, Val) :- recordz(Val,Val).
env_recorded(asserta, Val) :- recorda(Val,Val).
env_recorded(retract, Val) :- recorded(Val,Val,Ref), erase_safe(recorded(Val,Val,Ref),Ref).
env_recorded(retractall, Val) :- foreach( recorded(Val,Val,Ref), erase_safe(recorded(Val,Val,Ref),Ref) ).

lg_op2(rec_db,OP,env_recorded(OP)).
lg_op2(g,OP,OP).

lg_op2(_,OP,OP).

:- meta_predicate env_mpred_op(?,1,:).
:- meta_predicate env_mpred_op_1(?,1,:).
:- meta_predicate env_shadow(1,?).

env_mpred_op(_,_,[]):-!.
env_mpred_op(ENV,OP,F/A):- % dtrace,
   var(A),!, forall(clause_b(mpred_prop(_M,F,A,ENV)),((functor(P,F,A),env_mpred_op(ENV,OP,P)))).
env_mpred_op(ENV,retractall,F/A):-functor(P,F,A),!,env_mpred_op(ENV,retractall,P).
% env_mpred_op(ENV,OP,Dom):- isa_kb:box_prop(Dom),!,forall(baseKB:mpred_prop(M,F,A,Dom),env_mpred_op(ENV,OP,F/A)).
% env_mpred_op(ENV,OP,F/A):-!, functor(P,F,A), (((get_mpred_stubType(F,A,LG2),LG2\==ENV)  -> env_mpred_op(LG2,OP,P) ; env_mpred_op(ENV,OP,P) )).
% env_mpred_op(_,retractall,P):-functor_h(P,F,A),must(get_mpred_stubType(F,A,_)),fail.
% env_mpred_op(ENV,OP,P):- functor_h(P,F,A),  (((get_mpred_stubType(F,A,LG2),LG2\==ENV)  -> env_mpred_op(LG2,OP,P) ; fail )).
env_mpred_op(ENV,OP,P):- functor_h(P,F,A),  (((get_mpred_stubType(F,A,LG2),LG2\==ENV)  -> env_mpred_op_1(LG2,OP,P) ; env_mpred_op_1(ENV,OP,P) )).


env_mpred_op_1(dyn,OP,P):- !,call(OP,/*ocluser*/ocl:P).
env_mpred_op_1(ENV,OP,(A,B)):-!, env_mpred_op(OP,A), env_mpred_op(ENV,OP,B).
env_mpred_op_1(ENV,OP,[A|B]):-!, env_mpred_op(ENV,OP,A), env_mpred_op(ENV,OP,B).

env_mpred_op_1(in_dyn(DB),OP,P):- !, call(OP,in_dyn(DB,P)).
env_mpred_op_1(in_pred(DB),OP,P):-!, DBPRED=..[DB,P], call(OP,DBPRED).
env_mpred_op_1(with_pred(Pred),OP,P):-!, call(Pred,OP,P).
% env_mpred_op_1(ENV,OP,P):- dmsg(env_mpred_op_1(ENV,OP,P)),fail.
env_mpred_op_1(ENV,OP,P):- lg_op2(ENV,OP,OP2),!,call(OP2,P).
% env_mpred_op_1(ENV,OP,P):- throw(dtrace),simplest(ENV),!,call(OP,P).
env_mpred_op_1(stubType(ENV),OP,P):-!,env_mpred_op(ENV,OP,P).
% !,env_mpred_op_1(in_dyn(DB),OP,P).
env_mpred_op_1(_,OP,P):-!,env_mpred_op_1(in_dyn(db),OP,P).
env_mpred_op_1(_,_,_):-dtrace,fail.
env_mpred_op_1(l,OP,P):-!,call(OP,/*ocluser*/ocl:P).
env_mpred_op_1(g,OP,P):-!,call(OP,/*ocluser*/ocl:P).
env_mpred_op_1(l,OP,P):-!,env_mpred_op_1(dyn,OP,P).
env_mpred_op_1(l,OP,P):-!,env_mpred_op_1(in_dyn(db),OP,P).
env_mpred_op_1(l,OP,P):-!,env_mpred_op_1(rec_db,OP,P).
env_mpred_op_1(g,asserta,P):-retractall(/*ocluser*/ocl:P),asserta(/*ocluser*/ocl:P).
env_mpred_op_1(g,assert,P):-ain(P).
env_mpred_op_1(g,retract,P):-env_mpred_op_1(g,call,P),retract(/*ocluser*/ocl:P).
env_mpred_op_1(g,retractall,P):-foreach(env_mpred_op_1(g,call,P),retractall(P)).
env_mpred_op_1(ENV,OP,P):-env_learn_pred(ENV,P),lg_op2(ENV,OP,OP2),!,call(OP2,P).
env_mpred_op_1(_,OP,P):-call(OP,P).

%ppi(P):-functor(P,tp_node,_),!.
%ppi(P):-predicate_property(P,number_of_clauses(NC)),!,(NC<2000->true;(dmsg((number_of_clauses(NC):-P)))),!.
ppi(_).


env_info(O):- forall(env_info(O,Info),portray_clause(env_info(O):-Info)).

env_info(Type,Infos):- isa_kb:box_prop(Type),atom(Type),!,findall(Info,env_1_info(Type,Info),Infos),!.
env_info(Pred,Infos):- (nonvar(Pred)-> env_predinfo(Pred,Infos) ; (get_mp_arity(F,A),Pred=F/A,env_predinfo(Pred,Infos))),!.


harvest_preds(Type,Functors):-
 findall(functor(P,F,A),((get_mp_arity(F,A),(clause_b(mpred_prop(_M,F,A,Type));Type=F),functor(P,F,A))),Functors).

env_1_info(Type,[predcount(NC)|Infos]):- 
 gensym(env_1_info,Sym),flag(Sym,_,0),
   harvest_preds(Type,PFAs),
    findall(F/A - PredInf,
      (member(functor(P,F,A),PFAs),
        predicate_property(P,number_of_clauses(NC)),
        env_predinfo(P,PredInf),
        flag(Sym,X,X+NC)),
    Infos),flag(Sym,NC,0).

env_predinfo(PIn,Infos):- functor_h(PIn,F,A),get_mp_arity(F,A),functor(P,F,A),findall(Info,pred_1_info(P,F,A,Info),Infos).

pred_1_info(P,_,_,Info):- 
  member(Info:Prop,[count(NC):number_of_clauses(NC),mf:multifile,dyn:dynamic,vol:volitile,local:local]),predicate_property(P,Prop).
pred_1_info(_,F,A,Info):- clause_b(mpred_prop(_M,F,A,Info)).
pred_1_info(_,F,A,F/A).


:- meta_predicate(env_consult(:)).
env_consult(M:File):- \+ exists_file(File),!,forall(filematch(File,FM),env_consult(M:FM)).
env_consult(M:File):- ain(env_source_file(File)), locally(((M:term_expansion(A,B):- t_l:push_env_ctx, env_term_expansion(A,B))),M:consult(File)).


env_set(Call):-Call=..[P,V],!,hooked_gvar_put(P,V).
env_get(Call):-Call=..[P,V],!,hooked_gvar_get(P,V).


env_meta_term(t(env_call,env_assert,env_asserta,env_retract,env_retractall)).

% nt

env_push_argsA(Pred, Type,Prefix,Pred1 ):-t_l:push_env_ctx, env_push_args(Pred, Type,Prefix,Pred1 ).



do_prefix_arg(Pred,Prefix,Pred1 ,Type ):- do_prefix_arg0(Pred,Prefix,Pred1 ,Type ),!.
do_prefix_arg(M:Pred,Prefix,M:Pred1 ,Type ):- do_prefix_arg0(Pred,Prefix,Pred1 ,Type ),!.

do_prefix_arg0(Pred,Prefix,Pred ,Type ) :-env_push_argsA(_,  Type,Prefix,Pred ),!.
do_prefix_arg0(Pred,Prefix,Pred1 ,Type ) :-env_push_argsA(Pred, Type,Prefix,Pred1 ),!.
% do_prefix_arg(M:Pred,Prefix,M:Pred1, Type):- !,do_prefix_arg(Pred,Prefix,Pred1,Type),!.


push_prefix_arg(Pred,Type,Prefix,Pred ):-env_push_argsA(_,Type,Prefix,Pred),!,must(compound(Pred)).
push_prefix_arg(Pred,Type,Prefix,Pred1):-env_push_argsA(Pred,Type,Prefix,Pred1),!.

add_push_prefix_arg(Pred,Type,Prefix,Pred1):- must_det(add_push_prefix_arg0(Pred,Type,Prefix,OUT)),must(Pred1=OUT).
add_push_prefix_arg0((F/A),Type,Prefix,Pred1):- !, sanity((atom(F),integer(A))),functor(Pred,F,A),
   add_push_prefix_arg0(Pred,Type,Prefix,Pred1).
add_push_prefix_arg0(Pred,Type,Prefix,Pred1):- sanity(atom(Type)),Pred=..[F|ARGS],Pred1=..[F,Prefix|ARGS],
   ain(env_push_args(Pred,Type,Prefix,Pred1)),!.


term_expansion_add_context(_NeedIt,_Ctx,_,B,B):- var(B),!.
term_expansion_add_context( NeedIt, Ctx,Outter,B,BB):- Outter \== (/), 
   do_prefix_arg(B,Ctx,BB,_Type),!,ignore(NeedIt=Outter).

term_expansion_add_context(_NeedIt,_Ctx,_,B,B):- \+compound(B),!.
term_expansion_add_context(_NeedIt,_Ctx,_,DECL,DECLN):- 
    DECL  =..[DF,(F/A)],number(A),atom(F),functor(H,F,A),
    term_expansion_add_context(_Dont,_Ctx2, decl ,H,HH),
    DECLN =..[DF,(F/B)],functor(HH,F,B).
term_expansion_add_context( NeedIt, Ctx,_,B,BB):- B=..[F|A], must_maplist(term_expansion_add_context(NeedIt,Ctx,F),A,AA),BB=..[F|AA],!.

:- dynamic(env_source_file/1).

hb_to_clause_env(H,B,HB):- hb_to_clause_env0(H,B,HB0),!,HB=HB0.
hb_to_clause_env0(H,T,H):- T==true.
hb_to_clause_env0(T,B,(:-B)):- T==true.
hb_to_clause_env0(H,B,(H:-B)).


clause_to_hb(HB,H,B):-clause_to_hb0(HB,H0,B0),!,H=H0,B=B0.
clause_to_hb0((C:-T),H,B):- T==true,clause_to_hb0(C,H,B).
clause_to_hb0((H:-B),H,B).
clause_to_hb0((:-B),true,B).
clause_to_hb0((H),H,true).

:- export(env_term_expansion/2).
env_term_expansion(HB,HB):- \+ compound(HB),!.
env_term_expansion(HB,HB):- is_ftVar(HB),!.
env_term_expansion(HB,OUT):- 
 %must_det_l
 ((
   %must_det_l
   ((
   clause_to_hb(HB,H,B),
   term_expansion_add_context(BNeedIt,Ctx,(:-),B,BB),
   term_expansion_add_context(HNeedIt,Ctx,(:-),H,HH),
   ((var(BNeedIt),functor(HH,F,A),\+functor(H,F,A)) -> ((get_env_ctx(Ctx),nonvar(Ctx))) ; true),
   (((nonvar(HNeedIt);nonvar(BNeedIt)),var(Ctx)) -> BBB = (get_env_ctx(Ctx),BB) ; BBB = BB))))),
   (BBB\==B ; H\==HH),
   must_det_l((
     dmsg((old(H):-B)),dmsg((new(HH):-BBB)),
     hb_to_clause_env(HH,BBB,OUT))),!.

env_term_expansion(HB,(H:-B)):-  HB\=(:-_),
    end_of_file\==HB, 
    clause_to_hb(HB,H,B),
   _ALT = bb:'$sourcefile_info_env'(OUT),
   must(ain((H:-B))),
   hb_to_clause_env(H,B,OUT),!.


get_env_expected_ctx(Current):- 
   (prolog_load_context(source,File) -> Current = loading(File) ; (env_source_file(_) -> Current = memory ; Current = bb  )).

get_env_source_ctx(A,Active):-
    clause(domain_name(A),true,Ref),
    (clause_property(Ref,file(From)) -> (env_source_file(From) -> Active = loaded(From) ;  Active = loading(From)) ; Active = memory).

get_env_ctx(A):- hooked_gvar_get(domain_name,A),!.
get_env_ctx(A):- 
    get_env_expected_ctx(Current),
    get_env_source_ctx(A, Active),
    ( Current=Active -> true ; fail).
get_env_ctx(_ChameleonWorld).
% get_env_ctx(chameleonWorld):-!.

:- add_push_prefix_arg(get_tasks/3,dom,_,_).
:- add_push_prefix_arg(domain_name/1,dom,_,_).

inside_file_bb(ocl):- loading_file(File),!,once(file_name_extension(_,ocl,File);env_source_file(File)),!.
   
/*

user:term_expansion(A,B):- nonvar(A), A\==end_of_file, inside_file_bb(ocl),
  env_term_expansion(A,B),
  must(nonvar(B)),A\=@=B.

*/

:- env_term_expansion((
  mpred_undo(Why,nt(Head,Condition,Body)) :-
  % undo a negative trigger.
  !,
  (retract_i(nt(Head,Condition,Body))
    -> mpred_unfwc(nt(Head,Condition,Body))
     ; pfc_trace_msg("for ~p:\nTrigger not found to retract: ~p",[Why,nt(Head,Condition,Body)]))),OOO),
              wdmsg(OOO).

/*

env_mpred_op(ENV,OP_P):- throw(dtrace),simplest(ENV),!,OP_P.
env_mpred_op(ENV,call(P)):-env_mpred_op(ENV,call,P).
env_mpred_op(ENV,assert(P)):-env_mpred_op(ENV,assert,P).
env_mpred_op(ENV,asserta(P)):-env_mpred_op(ENV,asserta,P).
env_mpred_op(ENV,retract(P)):-env_mpred_op(ENV,retract,P).
env_mpred_op(ENV,retractall(F/A)):-functor(P,F,A),!,env_mpred_op(ENV,retractall,P).
env_mpred_op(ENV,retractall(P)):-env_mpred_op(ENV,retractall,P).
env_mpred_op(ENV,OP_P):- OP_P=..[OP,P], env_mpred_op(ENV,OP,P).


env_call(P):- env_mpred_op(call,P),ppi(P).
env_assert(P):- env_mpred_op(assert,P).
env_asserta(P):- env_mpred_op(asserta,P).
env_retract(P):- env_mpred_op(retract,P).
env_retractall(P):-env_mpred_op(retractall,P).


env_call(F):-!,call(F).
env_asserta(F):-!,maybe_show_env_mpred_op(asserta(F)).
env_assert(F):-!,maybe_show_env_mpred_op(assert(F)).
env_retract(F):-!,maybe_show_env_mpred_op(retract(F)).
env_retractall(F):-!,maybe_show_env_mpred_op(retractall(F)).
maybe_show_env_mpred_op(G):- !,G.
maybe_show_env_mpred_op(G):- t_l:db_spy -> show_call(why,G); G.

:- meta_predicate(maybe_show_env_mpred_op(0)).

*/

:- fixup_exports.

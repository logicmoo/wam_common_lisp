/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * 8ball.pl 
 *
 * Douglas'' Notes:
 *
 * 8BALL is used to predict when failure and errors may occur
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module('8ball', []).
:- set_module(class(library)).

:- use_module(library(logicmoo_common)).


di_test:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            'mapcar-visualize',
                            [func, l],

                            [ if,
                              [null, l],
                              [],

                              [ cons,
                                [apply, func, [list, [first, l]]],
                                [mapcar, func, [rest, l]]
                              ]
                            ]
                          ]).



nonplainvar(V):- notrace(nonvar(V);attvar_non_vn(V)),!.
attvar_non_vn(V):- attvar(V),get_attr(V,searchvar,_),!.
attvar_non_vn(V):- attvar(V),copy_term(V,VV),del_attr(VV,vn),del_attr(VV,rwstate),del_attr(VV,varuse),
  (get_attrs(VV,[]);\+attvar(VV)).

bind_breaks(More):- put_attr(More,bind_breaks,break).
:- meta_predicate bind_breaks:attr_unify_hook(0,*).
bind_breaks:attr_unify_hook(G,_):-G.

lisp_dump_break:- both_outputs(dumpST),!,trace,!,throw(lisp_dump_break).
%lisp_dump_break:- trace,throw(lisp_dump_break).
lisp_dump_break:- lisp_dumpST,!,break.
lisp_dumpST:- both_outputs(dumpST).

true_or_die(Goal):-functor(Goal,_,A),arg(A,Goal,Ret),always((Goal,Ret\==[])).

always_skip_always:- true.

% Must offer_rtrace succeed (or else there is a bug in the lisp impl!)
offer_rtrace((A->B;C)):- !, (A-> offer_rtrace(B);offer_rtrace(C)).
offer_rtrace((A,!,B)):-!,offer_rtrace(A),!,offer_rtrace(B).
offer_rtrace((A,B)):-!,offer_rtrace(A),offer_rtrace(B).
offer_rtrace(notrace(G)):- !, quietly_must_or_rtrace(G).
offer_rtrace(always(G)):-!,offer_rtrace(G).
offer_rtrace(rtrace(G)):-!,offer_rtrace(G).
offer_rtrace(call(G)):-!,offer_rtrace(G).
offer_rtrace(G):-slow_trace,trace,maybe_trace(G).

maybe_trace(G):- notrace(tracing)->user:rtrace(G);show_call_trace(user:G).
/*offer_rtrace(G):- notrace(tracing),!,( G -> true; (dbginfo(failed(G)),dumpST,dbginfo(failed(G)),break,G,!,fail)),!.
offer_rtrace(G):- !,( G-> true; (dbginfo(failed(G)),dumpST,dbginfo(failed(G)),trace,G,!,fail)),!.
%offer_rtrace(G):- notrace(tracing),!,(G->true;break). % nonquietly_must_or_rtrace(G).
offer_rtrace(G):- nonquietly_must_or_rtrace(G),!.
*/

length_safe(X,Y):- catch(length(X,Y),E,(dmsg(length(X,Y)=E),break)).

% Must certainly succeed (or else there is a bug in the users code!)
certainly((A,B)):-!,certainly(A),certainly(B).
% certainly(notrace(G)):- !, quietly_must_or_rtrace(G).
certainly(G):- notrace(tracing),!,G. % nonquietly_must_or_rtrace(G).
certainly(G):- nonquietly_must_or_rtrace(G).

always_catch(G):- catch(catch(G,'$aborted',notrace),E,(dbginfo(always_uncaught(E)),notrace,!,fail)).
with_nat_term(G):-
  \+ \+ ((
  (term_attvars(G,Vs),
    maplist(del_attr_rev2(freeze),Vs),
    maplist(del_attr_rev2(tracker),Vs),
   G))).

quietly_must_or_rtrace(G):-  
  (catch((G),E,gripe_problem(uncaught(E),(rtrace(G),!,fail)))
   *-> true ; (gripe_problem(fail_must_or_rtrace_failed,G),!,fail)),!.

nonquietly_must_or_rtrace(MG):- always_skip_always,!,call(MG).
nonquietly_must_or_rtrace(MG):- 
  strip_module(MG,M,G),
   dinterp(w_tr_lvl(_),M, Cut ,  G, 0 ),
   (callable(Cut)->(!,call(Cut));true).

:- '$hide'(lquietly/1).
lquietly(G):- quietly((G)).

slow_trace:- stop_rtrace,nortrace,trace,wdmsg(slow_trace).
on_x_rtrace(G):- catch(G,E,(dbginfo(E),rtrace(G),break)).



% nonquietly_must_or_rtrace
nmot1 :- true,true,fail,true.

nmot2 :- true,true,call_fail,true.

call_fail:- dmsg(fail),fail.

incr_arg(N,Redo):- arg(N,Redo,Val),ValNext is 1 + Val,nb_setarg(N,Redo,ValNext).


show_call_trace(Info,Goal):-
  Redo = sol(0,0),
  dmsg(call:Info),!, 
  ( ((call((Goal,deterministic(YN))),
      nb_setarg(Redo,2,YN),
      (YN==yes -> dmsg(exit_det:Info);dmsg(exit_nd:Info)))) 
   *-> 
      (incr_arg(1,Redo);((arg(1,Redo,Stage),dmsg(Stage:Info),fail)))
   ;
  dmsg(fail(Redo):Info)),

  (Redo == sol(0,0) -> (!,fail) ; (Redo=sol(_,yes) -> ! ; true)).

:- export(always/1).
:- module_transparent(always/1).
% Must always succeed (or else there is a bug in the lisp impl!)
always(Var):- notrace(var(Var)),!,throw(var_always(Var)).
always([]):-!.
always([A|B]):-!,always(A),always(B),!.
%always(MG):- strip_module(MG,M,G),!,rtrace(M:G).

always(MG):- always_skip_always, !, (call(MG) *->true;throw(failed_always(MG))).
/*
always(MG):- copy_term(MG,MGC),call(MG),once(always_borked(MGC)).
always_borked(MG):-
  strip_module(MG,M,G),
   w_dinterp(true,dinterp(w_tr_lvl(_),M, Cut , G, 0 )),
(callable(Cut)->(!,call(Cut));true).
*/

/*

always((A->B;C)):- !, (on_x_rtrace(user:A) -> always(B);always(C)).
always((A*->B;C)):- !, (on_x_rtrace(user:A) *-> always(B);always(C)).
always((A,!,B)):-!,always(A),!,always(B).
always((A,B)):-!,always(A),always(B).
always(always(G)):-!,always(G).
always(call(G)):-!,always(G).
always(notrace(G)):- !, quietly_must_or_rtrace(G),!.
always(G):- nonquietly_must_or_rtrace(G),!.
*/
%always(notrace(G)):- notrace(tracing),!, m(0)(quietly(user:G)),!.
%always(quietly(G)):- notrace(tracing),!, always(user:G).

cross_cut(_Cut,_Cut2).

%always(G):- !,(G-> true; (dbginfo(failed(G)),dumpST,dbginfo(failed(G)),trace,G,!,fail)),!.
%always(G):- notrace(tracing),!,(G->true;break). % nonquietly_must_or_rtrace(G).
:- module_transparent(dinterp/5).
%dinterp(Must,M,Cut,G,L):-L > -1,!,M:call(G).
dinterp(Must,N,Cut,M:G,L):-!,assertion(callable(G)),N:dinterp(Must,M,Cut,G,L).
%dinterp(Must,_,_,compound_name_arity(G,F,A),_Level):-!,compound_name_arity(G,F,A).
%dinterp(Must,_,_,is_functionp(G),_Level):-!,rtrace(is_functionp(G)).
dinterp(_,_,_,true,_):-!.
dinterp(_Must,_M, Cut, (!),_):-!,(nonvar(Cut)->true;Cut=!).

% dinterp(Must,M,Cut, G,L):- notrace(tracing),!,notrace,call_cleanup(dinterp(tracing(Must),M,Cut,G,L),trace).
dinterp(Must,M,Cut,call(G),L):- cross_cut(Cut,Cut2),!,dinterp(Must,M,Cut2,G,L) .
dinterp(_Must,_M,_Cut,dbginfo(G),_L):-!,dbginfo(G),!.
dinterp(_Must,_M,_Cut,compound(G),_L):-!,compound(G),!.

dinterp(Must,M,Cut,(repeat,G),L):- cross_cut(Cut,Cut2),!,repeat,dinterp(Must,M,Cut2,G,L),(callable(Cut2)->(!,call(Cut2));true),(callable(Cut)->(!,call(Cut));true).

%dinterp(_Must,_M, Cut,CutFail,_L):- CutFail==(!,fail),!,ignore(Cut=fail),!.
dinterp(_Must,_M,_Cut,fail,_):- !,fail.


dinterp(Must,M,Cut,once(G),L):-!,cross_cut(Cut,Cut2),dinterp(Must,M,Cut2,G,L),!.

dinterp(Must,M,Cut, ( \+ \+ G),L):- L2 is L +1, cross_cut(Cut,Cut2), !, \+ \+ dinterp(Must,M,Cut2,G,L2).
dinterp(Must,M,Cut, ( \+ G),L):- L2 is L +1, cross_cut(Cut,Cut2), !, \+ dinterp(Must,M,Cut2,G,L2).
dinterp(Must,M,Cut,  not(G),L):- L2 is L +1, cross_cut(Cut,Cut2), !, \+ dinterp(Must,M,Cut2,G,L2).

dinterp(Must,M,Cut,(Cond *-> Then ; Else),L):-!,L2 is L +1,
   (dinterp(Must,M,Cut,Cond,L2) *-> dinterp(Must,M,Cut,Then,L) ; dinterp(Must,M,Cut,Else,L)).
dinterp(Must,M,Cut,(Cond  -> Then ; Else),L):-!,L2 is L +1,
   (dinterp(Must,M,Cut,Cond,L2) -> dinterp(Must,M,Cut,Then,L) ; dinterp(Must,M,Cut,Else,L)).

dinterp(Must,M,Cut,(Cond  -> Then),L):-!, (dinterp(Must,M,Cut,Cond,L) -> dinterp(Must,M,Cut,Then,L)).

dinterp(Must,M,Cut,(Cond *-> Then),L):-!, (dinterp(Must,M,Cut,Cond,L)*-> dinterp(Must,M,Cut,Then,L)).

dinterp(Must,M,Cut,(GoalsL;GoalsR),L):-!,L2 is L +1,
   (dinterp(Must,M,Cut,GoalsL,L2);dinterp(Must,M,Cut,GoalsR,L)).

dinterp(Must,M,Cut,(Goals1,Goals2),L):- !,          
  (dinterp(Must,M,Cut,Goals1,L ),dinterp(Must,M,Cut,Goals2,L)).

dinterp(_Must,M,_Cut, always(G),_):- !, always(M:G). % cross_cut(Cut,Cut2),!,dinterp(Must,M,Cut2,G,0),!.
dinterp(_Must,M,_Cut, must(G),_):- !, always(M:G).
dinterp(_Must,M,_Cut, call_call(G),_):- !, call(M:G).
dinterp(_Must,M,_Cut, call(call,G),_):- !, call(M:G).
% RESOTRE  dinterp(Must,M,Cut,  must(G),_):- cross_cut(Cut,Cut2),!,dinterp(Must,M,Cut2,G,0),!.
dinterp(Must,M,Cut,lquietly(G),L):- cross_cut(Cut,Cut2),!,quietly(dinterp(Must,M,Cut2,G,L)).
% RESOTRE dinterp(Must,M,Cut, quietly(G),L):-!,quietly(dinterp(Must,M,Cut,G,L)).
%dinterp(Must,M,Cut, quietly(G),L):-!,quietly(dinterp(Must,M,Cut,G,L)).
dinterp(Must,M,Cut, quietly(G),L):- cross_cut(Cut,Cut2),!, dinterp(Must,M,Cut2,G,L).
% RESOTRE  dinterp(Must,M,Cut, notrace(G),L):-!,quietly(dinterp(Must,M,Cut,G,L)).
dinterp(Must,M,Cut, notrace(G),L):- !, %wo_trace
   (((cross_cut(Cut,Cut2), dinterp(Must,M,Cut2,G,L)))).
dinterp(Must,M,Cut,findall(Template,G,Bag),L):-cross_cut(Cut,Cut2),!,L2 is L +1,findall(Template,dinterp(Must,M,Cut2,G,L2),Bag).
dinterp(Must,M,Cut,call_cleanup(G,Cleanup),L):-cross_cut(Cut,Cut2),!,call_cleanup(dinterp(Must,M,Cut2,G,L),Cleanup).


%wo_trace
% RESOTRE dinterp(Must,M,Cut,setup_call_cleanup(S,G,Cleanup),L):- cross_cut(Cut,Cut2),cross_cut(Cut,Cut3),!,setup_call_cleanup(dinterp(Must,M,_Cut,S,L),dinterp(Must,M,Cut2,G,L),dinterp(Must,M,Cut3,Cleanup,L)).


dinterp(_Must,M,_Cut,catch(G,E,F),_L):- !,M:catch(G,E,F).

% RESOTRE dinterp(Must,M,Cut,catch(G,E,F),L):-cross_cut(Cut,Cut2),cross_cut(Cut,Cut3),!,catch(dinterp(Must,M,Cut2,G,L),E,dinterp(Must,M,Cut3,F,L)).
%d  i nterp(_,Cut,!,_):-!,(var(Cut);Cut=!).

dinterp(Must,M,Cut,CallN,L):- 
  notrace((fix_callables(CallN,CallNew)->CallN\=@=CallNew)),!,
  dinterp(Must,M,Cut,CallNew,L).

%dinterp(_Must,M,_Cut,Goal,_L):- notrace(tracing),!,M:call(M:Goal).
dinterp(Must,M,Cut,Goal,L):- dinterp_c(Must,M,Cut,Goal,L).


fix_callables(Atom,Atom):- \+ compound(Atom),!.
fix_callables(call(In),Out):- !, fix_callables(In,Out).
fix_callables(\+ (In), \+ Out):- !, fix_callables(In,Out).
fix_callables(apply(F,ARGS),NewCall2):- !, assertion(callable(F)),
   F=..FL,append(FL,ARGS,NewCallL),NewCall=..NewCallL,!,fix_callables(NewCall,NewCall2).
fix_callables(CallN,NewCall2):- CallN=..[call,F|ARGS],!,assertion(callable(F)),
   F=..FL,append(FL,ARGS,NewCallL),NewCall=..NewCallL,!,fix_callables(NewCall,NewCall2).
fix_callables(NewCall,NewCall).

:- meta_predicate(wo_trace(0)).
wo_trace(G):- !, call(G).
%wo_trace(G):- notrace(tracing)->each_call_cleanup(notrace,G,notrace(trace)); call(G).
/*
wo_trace(G):- !, 
  (notrace(tracing)->
   (visible(-all),visible(+exception),leash(-all),leash(+exception),call_cleanup(G,(visible(+full),leash(+full))));
    call(G)).
%wo_trace(G):- notrace(tracing)->(visible(-all),visible(+exception),call_cleanup(G,(visible(+full),leash(+full))));call(G).
%wo_trace(G):- notrace(tracing)->(visible(-all),visible(+exception),call_cleanup(G,(visible(+full),leash(+full))));call(G).
*/
dinterp_c(Must,M,Cut, G,L):-  notrace((fail,tracing)),!,  
  wo_trace(dinterp_c(tracing(Must),M,Cut,G,L)),
(callable(Cut)->(!,call(Cut));true).

dinterp_c(tracing(Must),M,Cut, G,L):- !, 
  show_call_trace((Must->M:G),dinterp_d(Must,M,Cut, G,L)),
(callable(Cut)->(!,call(Cut));true).

dinterp_c(Must,M,Cut,G,Level):-   
  notrace((Must = rtrace(TraceLvl), Level==TraceLvl,
  next_trace_level(Must,_NewTraceLevel))),!,
   rtrace(M:G),
(callable(Cut)->(!,call(Cut));true).


dinterp_c(Must,M,Cut,G,Level):-
  notrace((compound(Must),arg(1,Must,TraceLvl),Level==TraceLvl, !,     
   next_trace_level(Must,NextMust))),
   show_call_trace((Must->M:G),dinterp_d(NextMust,M,Cut,G,Level)),
(callable(Cut)->(!,call(Cut));true).

dinterp_c(Must,M,Cut,G, Level):-
  next_trace_level(Must,NextMust),
  dinterp_d(NextMust,M,Cut,G, Level),
(callable(Cut)->(!,call(Cut));true).

/*
*/

dinterp_d(Must,M,Cut,G,L):- 
  (compound(G)->
    (compound_name_arity(G,F,A),compound_name_arity(GG,F,A)) ;
    GG =G),
 dinterp_e(Must,M,Cut,G,GG,L),
(callable(Cut)->(!,call(Cut));true).
                         
/*
*/

dinterp_e(_Must,M,_Cut,G, GG, _L):- 
  (((nb_current('$w_dinterp',false) ; just_call(M,GG)))),!,
  (call(M:G)).
% (callable(Cut)->(!,call(Cut));true).
%:- '$hide'(rtrace:trace).
%dinterp_e(_Must,M,_UnseenCut,G,_GG,_L):- !, rtrace(M:G).
dinterp_e(Must,M,_UnseenCut,G,GG,L):- 
   notrace((L2 is L -1,predicate_property(M:GG,number_of_clauses(_)))),!,
   (( M:clause(GG,Body), G=GG)),
   dinterp(Must,M,Cut2,Body,L2),
(callable(Cut2)->(!,call(Cut2));true).

dinterp_e(_Must,M,Cut,G,GG,_L):- 
   predicate_property(M:GG,defined),!,
   M:call(M:G),
(callable(Cut)->(!,call(Cut));true).

dinterp_e(Must,_M,Cut,G,GG,L):-  
  notrace(( current_module(MM),    
    predicate_property(MM:GG,number_of_clauses(_)),
    \+ predicate_property(MM:GG,imported_from(_)))),!,
  dmsg("Found Inaccessable predicate!"),
  trace,G=GG,dinterp_e(Must,MM,Cut,G,GG,L).

dinterp_e(_Must,M,Cut,G,_GG,_L):- 
   M:on_x_rtrace(M:G),
(callable(Cut)->(!,call(Cut));true).

next_trace_level(In,Out):- notrace((In=@=w_tr_lvl(_),In=Out)),!.
next_trace_level(In,Out):- compound(In),arg(1,In,Mid),!,
  next_trace_level(Mid,MidOut),ignore(In=Out),!,setarg(1,Out,MidOut).
next_trace_level(In,Out):- number(In),!, Out is In+1.
next_trace_level(In,In).


w_dinterp(V,G):- (nb_current('$w_dinterp',Was);Was=[]),!,
 ((V = Was) -> G ; 
    (b_setval('$w_dinterp',V),G,b_setval('$w_dinterp',Was))).

just_call(_,G):- var(G),!.
just_call(_,=(_,_)):-!.
just_call(_,call_call(_)):-!.
just_call(_,G):- compound(G),functor(G,F,_),just_call_f(F),!.
just_call(M,G):- predicate_property(M:G,nodebug),!.
just_call(M,G):- M:predicate_property(_:G,nodebug),!.
just_call(M,G):- \+ \+ (predicate_property(M:G,meta_predicate(GG)),arg(_,GG,N),integer(N)),!.
just_call(M,G):-  predicate_property(M:G,number_of_clauses(_)),notrace(catch( (M:clause(G,_),fail), _, true)),!.


just_call_f('$sig_atomic').
%%just_call_f(maplist).

%set_prolog_flag(gc,false),
just_call_f(F):- atom_concat(_,ii,F).
just_call_f(F):- atom_concat(atom_,_,F).

just_call_f(F):- atom_concat(get_opv,_,F).
just_call_f(F):- atom_concat(nb_,_,F).
just_call_f(F):- atom_concat(package_,_,F).
just_call_f(F):- atom_concat(is_,_,F).
just_call_f(F):- atom_concat(dinterp,_,F).
just_call_f(F):- atom_concat(filter_var_chars,_,F).

just_call_f(with_mutex).
just_call_f(flag).
just_call_f(is).
just_call_f(=).
just_call_f(call_call).
just_call_f(gensym).

nonquietly_must_or_rtrace_now(G):- 
  (catch((G),E,gripe_problem(uncaught(E),(rtrace(G),!,fail)))
   *-> true ; (gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,G))),!,fail)),!.
                        

gripe_problem(Problem,G):- always_catch(gripe_problem0(Problem,(G))).
gripe_problem0(Problem,G):-
     notrace(( 
     dbginfo((Problem=G)),
     dumpST,
     dbginfo((Problem=G)))),
     nortrace,
     trace,!,
     lisp_dump_break,
     slow_trace,
     ((G)*->(slow_trace,lisp_dump_break);
       (dbginfo(warn(failed_rtrace(G))),notrace,lisp_dump_break,!,fail)).


:- meta_predicate(timel(+,:)).
timel(_,MG):- wam_cl_option(call_statistics,false),!, call(MG).
timel(What,M:X):- notrace(( write('## '),write(What))),prolog_statistics:time(M:X).


% is_assert_op(_,_):-!,fail.
is_assert_op(A,B,C):- notrace(is_assert_op0(A,B,C)),!.
is_assert_op0(A,_,_):- \+ compound(A),!,fail.
is_assert_op0(M:I,W,M:O):- !, is_assert_op0(I,W,O).
is_assert_op0(assert_lsp(W,P),W,P).
is_assert_op0(assert_lsp(P),u,P).
is_assert_op0(assertz(P),u,P).
is_assert_op0(asserta(P),u,P).
is_assert_op0(assert(P),u,P).
is_assert_op0(asserta_if_new(P),u,P).
is_assert_op0(asserta_new(P),u,P).
is_assert_op0(assertz_if_new(P),u,P).
is_assert_op0(assertz_new(P),u,P).
is_assert_op0(assert_if_new(P),u,P).


fmt99(O):- in_md(prolog,always((make_pretty(O,P),fmt999(P)))),!.

fmt999(P):- \+ compound(P),!,fmt9(P).
fmt999((:- M:P)):-
  with_output_to(string(A),fmt9(:-P)),
  trim_off(':-',A,B),
  format('~N:- ~q:~s~n',[M,B]).
fmt999((M:H :- Body)):- P= (M:H :- Body),
  with_output_to(string(A),fmt9(:-P)),
  trim_off(':-',A,B),
  format('~N:- ~q:~s~n',[M,B]).
fmt999(M:P):- functor(P,':-',_),!,fmt9(M:P).
fmt999(M:P):- with_output_to(string(A),fmt9(:-P)),
  trim_off(':-',A,B),
  format('~N~q:~s~n',[M,B]).
fmt999(P):- functor(P,':-',_),!,fmt9(P).
fmt999(P):- with_output_to(string(A),fmt9(:-P)),
  trim_off(':-',A,B),
  format('~N~s~n',[B]).
fmt999(P):- fmt9(P),nl.
% notrace((dbmsg0(Var))).
trim_off(W,A,B):- atomic(A), string_concat(W,B,A),!.
trim_off(_,A,A).

assert_lsp(G):- assert_lsp(u,G).
assert_lsp(S,(G1,G2)):- !,assert_lsp(S,G1),assert_lsp(S,G2).
assert_lsp(_,G):-  wo_trace((copy_term_nat(G,GG),assert_local(GG))).

assert_local(user:G):-!,assert_local(G).
assert_local(user:G:-B):-!,assert_local(G:-B).
assert_local((G,B)):- !,assert_local(G),assert_local(B).
assert_local(G:-B):- B==true,!,assert_local(G).
assert_local(G):- assert_local0(G).
assert_local0(G):- \+ \+ (clause_asserted_local(G,_)),!.
assert_local0(G):- doall((clause_asserted_local(G,E),erase(E),fail)),!,user:asserta(G),!.

clause_asserted_local((H:-_),R):-!,  predicate_property(H,number_of_clauses(_)),clause(H,_,R).
clause_asserted_local(H,R):-  predicate_property(H,number_of_clauses(_)),clause(H,true,R).

:- fixup_exports.

%system:goal_expansion(always(G),G) :- wam_cl_option(speed,S),S>2.
%system:goal_expansion(certainly(G),G) :- wam_cl_option(safety,0).

:- use_module(debugio).
%:- include('./header').

wl:interned_eval("(defparameter sys:*markdown* cl:t)").


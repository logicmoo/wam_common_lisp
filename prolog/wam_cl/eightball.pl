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


slow_trace:- stop_rtrace,nortrace,trace,wdmsg(slow_trace).

on_x_rtrace(G):- catch(G,E,(dbginfo(E),rtrace(G),break)).
atom_concat_or_rtrace(X,Y,Z):- tracing->atom_concat(X,Y,Z);catch(atom_concat(X,Y,Z),_,break).


nonplainvar(V):- nonvar(V);attvar_non_vn(V).
attvar_non_vn(V):- attvar(V),get_attr(V,searchvar,_),!.
attvar_non_vn(V):- attvar(V),copy_term(V,VV),del_attr(VV,vn),del_attr(VV,rwstate),del_attr(VV,varuse),
  (get_attrs(VV,[]);\+attvar(VV)).

bind_breaks(More):- put_attr(More,bind_breaks,break).
:- meta_predicate bind_breaks:attr_unify_hook(0,*).
bind_breaks:attr_unify_hook(G,_):-G.

lisp_dump_break:- both_outputs(dumpST),!,trace,throw(lisp_dump_break).
%lisp_dump_break:- trace,throw(lisp_dump_break).
lisp_dump_break:- lisp_dumpST,break.
lisp_dumpST:- both_outputs(dumpST).

true_or_die(Goal):-functor(Goal,_,A),arg(A,Goal,Ret),always((Goal,Ret\==[])).

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

nonquietly_must_or_rtrace(G):- dinterp(user,_ ,  G, 0 ).

:- '$hide'(lquietly/1).
lquietly(G):- quietly((G)).

% Must always succeed (or else there is a bug in the lisp impl!)
always(Var):- var(Var),!,throw(var_always(Var)).
always([]):-!.
always([A|B]):-!,always(A),always(B).
always((A->B;C)):- !, (on_x_rtrace(user:A) -> always(B);always(C)).
always((A,!,B)):-!,always(A),!,always(B).
always((A,B)):-!,always(A),always(B).
always(always(G)):-!,always(G).
always(call(G)):-!,always(G).
always(notrace(G)):- !, quietly_must_or_rtrace(G),!.
always(G):- nonquietly_must_or_rtrace(G),!.
%always(notrace(G)):- notrace(tracing),!, must(quietly(user:G)),!.
%always(quietly(G)):- notrace(tracing),!, always(user:G).

%always(G):- !,(G-> true; (dbginfo(failed(G)),dumpST,dbginfo(failed(G)),trace,G,!,fail)),!.
%always(G):- notrace(tracing),!,(G->true;break). % nonquietly_must_or_rtrace(G).
:- module_transparent(dinterp/4).
%dinterp(M,_,G,L):-L > -1,!,M:call(G).
dinterp(N,C,M:G,L):-!,assertion(nonvar(G)),N:dinterp(M,C,G,L).
%dinterp(_,_,compound_name_arity(G,F,A),_Level):-!,compound_name_arity(G,F,A).
%dinterp(_,_,is_functionp(G),_Level):-!,rtrace(is_functionp(G)).
dinterp(_,_,true,_).
dinterp(M,_,call(G),L):-!,dinterp(M,_,G,L) .
dinterp(M,_,(\+ G),L):-!,\+ dinterp(M,_,G,L).
dinterp(M,C,(Cond -> Then ; Else),L):-!,( dinterp(M,C,Cond,L)  ->  dinterp(M,C,Then,L) ; dinterp(M,C,Else,L)).
dinterp(M,C,(Cond *-> Then ; Else),L):-!,L2 is L +1,( dinterp(M,C,Cond,L2)  *->  dinterp(M,C,Then,L) ; dinterp(M,C,Else,L)).
dinterp(M,C,(Cond -> Then),L):-!,(dinterp(M,C,Cond,L) -> dinterp(M,C,Then,L)).
dinterp(M,C,(Cond *-> Then),L):-!,L2 is L +1,(dinterp(M,C,Cond,L2) *-> dinterp(M,C,Then,L)).
dinterp(M,C,(GoalsL ; GoalsR),L):-!,L2 is L +1,(dinterp(M,C,GoalsL,L2) ; dinterp(M,C,GoalsR,L2)).
dinterp(M,C,(Goals1,Goals2),L):-!,(dinterp(M,C,Goals1,L),dinterp(M,C,Goals2,L)).
dinterp(M,_,  once(G),L):-!,dinterp(M,_,(G),L),!.
dinterp(M,C,always(G),_):-!,dinterp(M,C,(G),0),!.
dinterp(M,C,  must(G),_):-!,dinterp(M,C,G,0),!.
dinterp(M,C,lquietly(G),L):-!,quietly(dinterp(M,C,G,L)).
dinterp(M,C, quietly(G),L):-!,quietly(dinterp(M,C,G,L)).
dinterp(M,C, notrace(G),L):-!,quietly(dinterp(M,C,G,L)).
dinterp(M,_,findall(Template,G,Bag),L):-!,L2 is L +1,findall(Template,dinterp(M,_,G,L2),Bag).
dinterp(M,_,setup_call_cleanup(T,G,Bag),L):-!,L2 is L +1,setup_call_cleanup(dinterp(M,_,T,L2),dinterp(M,_,G,L2),dinterp(M,_,Bag,L2)).
dinterp(M,_,catch(G,E,F),L):-!,catch(dinterp(M,_,G,L),E,dinterp(M,_,F,L)).
%d  i nterp(_,C,!,_):-!,(var(C);C=!).
dinterp(_,C,!,_):-!,(nonvar(C)->true;C=!).
dinterp(M,_,G,_):- notrace((\+ compound(G))),!,M:G.
dinterp(M,C,G,L):- notrace((G=..[call,F|ARGS],atom(F),Call2=..[F|ARGS])),!,dinterp(M,C,Call2,L).

dinterp(M,_,G, Level):- Level==0,!, (M:call(G)*-> true; (rtrace((M:call(G))),throw(failed_must(G)))).
dinterp(M,_,G, _):- M:call(G).
%dinterp(M,_, G,L):-L > -1,!,nonquietly_must_or_rtrace0(M:G).
/*
dinterp(M,_,G,_):- quietly(just_call(M,G)),!,M:call(G).
dinterp(M,C,G,L):- L2 is L +1,functor(G,F,A),functor(GG,F,A),!,dinterp_c(M,C,G,GG,L2).
*/
dinterp_c(M,_, G,_,L):-L > -1,!,nonquietly_must_or_rtrace0(M:G).
dinterp_c(_,C,G,GG,L):- \+ clause(GG,_), 
  notrace(( current_module(MM),clause(MM:GG,_),\+ clause(MM:GG,imported_from(_)))),!,trace,dinterp_c(MM,C,G,G,L).

dinterp_c(M,C,G,GG,L):- 
   clause(M:GG,Body),G=GG,
   dinterp(M,C,Body,L),(var(C)-> true ; (!,C)).

just_call(_,G):- compound(G),functor(G,F,_),just_call_f(F).
just_call(M,G):- predicate_property(M:G,nodebug).
just_call(M,G):- M:predicate_property(_:G,nodebug).
just_call(M,G):- notrace(catch( (M:clause(G,_),fail), _, true)).
just_call_f('$sig_atomic').
just_call_f(maplist).
just_call_f(dinterp).
just_call_f(with_mutex).
just_call_f(flag).
just_call_f(is).
just_call_f(gensym).

nonquietly_must_or_rtrace0(G):- 
  (catch((G),E,gripe_problem(uncaught(E),(rtrace(G),!,fail)))
   *-> true ; (gripe_problem(fail_must_or_rtrace_failed,rtrace((slow_trace,G))),!,fail)),!.
                        

gripe_problem(Problem,G):- always_catch(gripe_problem0(Problem,(G))).
gripe_problem0(Problem,G):-
     notrace(( 
     dbginfo((Problem=G)),
     dumpST,
     dbginfo((Problem=G)))),
     nortrace,
     trace,
     lisp_dump_break,
     slow_trace,
     ((G)*->(slow_trace,lisp_dump_break);(dbginfo(warn(failed_rtrace(G))),notrace,lisp_dump_break,!,fail)).


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
assert_lsp(_,G):-  notrace((copy_term_nat(G,GG),assert_local(GG))).
assert_local(user:G):-!,assert_local(G).
assert_local(user:G:-B):-!,assert_local(G:-B).
assert_local(G:-B):- B==true,!,assert_local(G).
assert_local(G):- assert_local0(G).
assert_local0(G):- \+ \+ (clause_asserted_local(G,_)),!.
assert_local0(G):- doall((clause_asserted_local(G,E),erase(E),fail)),!,user:asserta(G),!.
clause_asserted_local((H:-_),R):-!, clause(H,_,R).
clause_asserted_local(H,R):- clause(H,true,R).

:- fixup_exports.

%system:goal_expansion(always(G),G) :- wam_cl_option(speed,S),S>2.
%system:goal_expansion(certainly(G),G) :- wam_cl_option(safety,0).

:- use_module(debugio).
:- include('header').

wl:interned_eval("(defparameter ext:*markdown* cl:t)").


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


slow_trace:- notrace(tracing),!,stop_rtrace,nortrace,trace.
slow_trace:- stop_rtrace,nortrace.

on_x_rtrace(G):- catch(G,E,(dbginfo(E),rtrace(G),break)).
atom_concat_or_rtrace(X,Y,Z):- tracing->atom_concat(X,Y,Z);catch(atom_concat(X,Y,Z),_,break).

lisp_dump_break:- both_outputs(dumpST),!,trace,throw(lisp_dump_break).
%lisp_dump_break:- trace,throw(lisp_dump_break).
lisp_dump_break:- lisp_dumpST,break.
lisp_dumpST:- both_outputs(dumpST).

true_or_die(Goal):-functor(Goal,_,A),arg(A,Goal,Ret),always((Goal,Ret\==[])).

:- '$hide'(lquietly/1).
lquietly(G):- notrace((G)).

% Must always succeed (or else there is a bug in the lisp impl!)
always(G):- notrace(tracing),!,G,!.
always((A->B;C)):- !, (on_x_rtrace(user:A) -> always(B);always(C)),!.
always((A,!,B)):-!,always(A),!,always(B).
always((A,B)):-!,always(A),always(B).
always(notrace(G)):- notrace(tracing),!, must(quietly(user:G)),!.
always(notrace(G)):- !, quietly_must_or_rtrace(G).
always(quietly(G)):- notrace(tracing),!, always(user:G).
always(always(G)):-!,always(G).
always(call(G)):-!,always(G).
always(G):- notrace(tracing),!,user:G,!.
always(G):- notrace(tracing),!,( user:G -> true; (dbginfo(failed(G)),dumpST,dbginfo(failed(G)),trace,slow_trace,G,!,fail)),!.
always(G):- !,nonquietly_must_or_rtrace(user:G),!.
%always(G):- !,( G-> true; (dbginfo(failed(G)),dumpST,dbginfo(failed(G)),trace,G,!,fail)),!.
%always(G):- notrace(tracing),!,(G->true;break). % nonquietly_must_or_rtrace(G).

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
nonquietly_must_or_rtrace(G):- 
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



:- dynamic(wam_cl_option/2).
:- dynamic(wam_cl_option_local/2).
wam_cl_option(N,V):- V==true,!,wam_cl_option(N,t).
wam_cl_option(N,V):- nonvar(N), wam_cl_option_local(N,VV),!,V=VV.
wam_cl_option(N,V):- var(N), wam_cl_option_local(N,VV),V=VV.
wam_cl_option(speed,V):- !, (current_prolog_flag(runtime_speed,V)->true;V=1).
wam_cl_option(safety,V):- !, (current_prolog_flag(runtime_safety,V)->true;V=1).
wam_cl_option(debug,V):- !, (current_prolog_flag(runtime_debug,V)->true;V=1).
wam_cl_option(safe(_),t):- !, (wam_cl_option(safety,V),V>0).
wam_cl_option(_,TF):- wam_cl_option(safety,N),(N<1-> TF=t; TF=[]).

system:goal_expansion(always(G),G) :- wam_cl_option(speed,S),S>2.
system:goal_expansion(certainly(G),G) :- wam_cl_option(safety,0).

:- fixup_exports.
:- use_module(debugio).
:- include('header').

wl:interned_eval("(defparameter ext:*markdown* cl:t)").


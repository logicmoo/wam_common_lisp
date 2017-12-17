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

show_call_trace(G):- G *-> wdmsg(G); (wdmsg(warn(failed(show_call_trace(G)))),fail).

slow_trace:- notrace(tracing),!,stop_rtrace,nortrace,trace.
slow_trace:- nortrace.

on_x_rtrace(G):- catch(G,E,(dbmsg(E),rtrace(G),break)).
atom_concat_or_rtrace(X,Y,Z):- tracing->atom_concat(X,Y,Z);catch(atom_concat(X,Y,Z),_,break).

%lisp_dump_break:- both_outputs(dumpST),!,throw(lisp_dump_break).
%lisp_dump_break:- trace,throw(lisp_dump_break).
lisp_dump_break:- lisp_dumpST,break.
lisp_dumpST:- both_outputs(dumpST).

true_or_die(Goal):-functor(Goal,_,A),arg(A,Goal,Ret),always((Goal,Ret\==[])).

lquietly(G):- quietly(G).

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
always(G):- notrace(tracing),!,( user:G -> true; (wdmsg(failed(G)),dumpST,wdmsg(failed(G)),trace,slow_trace,G,!,fail)),!.
always(G):- !,nonquietly_must_or_rtrace(user:G),!.
%always(G):- !,( G-> true; (wdmsg(failed(G)),dumpST,wdmsg(failed(G)),trace,G,!,fail)),!.
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
/*offer_rtrace(G):- notrace(tracing),!,( G -> true; (wdmsg(failed(G)),dumpST,wdmsg(failed(G)),break,G,!,fail)),!.
offer_rtrace(G):- !,( G-> true; (wdmsg(failed(G)),dumpST,wdmsg(failed(G)),trace,G,!,fail)),!.
%offer_rtrace(G):- notrace(tracing),!,(G->true;break). % nonquietly_must_or_rtrace(G).
offer_rtrace(G):- nonquietly_must_or_rtrace(G),!.
*/

% Must certainly succeed (or else there is a bug in the users code!)
certainly((A,B)):-!,certainly(A),certainly(B).
% certainly(notrace(G)):- !, quietly_must_or_rtrace(G).
certainly(G):- notrace(tracing),!,G. % nonquietly_must_or_rtrace(G).
certainly(G):- nonquietly_must_or_rtrace(G).

always_catch(G):- catch(catch(G,'$aborted',notrace),E,(wdmsg(always_uncaught(E)),notrace,!,fail)).
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
     wdmsg((Problem:-G)),
     dumpST,
     dbmsg((Problem:-G)))),
     lisp_dump_break,
     slow_trace,
     ((G)*->(slow_trace,lisp_dump_break);(wdmsg(failed_rtrace(G)),notrace,lisp_dump_break,!,fail)).


:- meta_predicate(timel(+,:)).
timel(What,M:X):- notrace(( write('% '),writeln(What))),prolog_statistics:time(M:X).


is_user_output:- current_output(O),stream_property(CO,alias(user_output)),!,CO==O.

both_outputs(G):-
  (is_user_output -> G ; (with_output_to(user_output,G),G)).

assert_lsp(G):- assert_lsp(u,G).
% assert_lsp(Symbol,G)
assert_lsp(_,G):-  notrace((copy_term_nat(G,GG),assert_local(GG))).
assert_local(user:G):-!,assert_local(G).
assert_local(user:G:-B):-!,assert_local(G:-B).
assert_local(G:-B):- B==true,!,assert_local(G).
assert_local(G):- assert_local0(G).

assert_local0(G):- \+ \+ (clause_asserted_local(G,_)),!.
assert_local0(G):- doall((clause_asserted_local(G,E),erase(E),fail)),!,user:asserta(G),!.

clause_asserted_local((H:-_),R):-!, clause(H,_,R).
clause_asserted_local(H,R):- clause(H,true,R).


colormsg1(Msg,Args):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,format(Msg,Args)).
%colormsg1(Msg):- writeq(Msg),nl,nl,!. %notrace(colormsg11(Msg)).
colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt99(Msg)).

%ansicall_maybe(_Ctrl,Cmd):- !,nl,nl,portray_clause_w_vars(Cmd),nl,nl,(Cmd),break.
ansicall_maybe(_Ctrl,Cmd):- current_output(O), \+ stream_property(O,tty(true)),!,call(Cmd).
ansicall_maybe(Ctrl,Cmd):- always(shrink_lisp_strings(Cmd,Cmd0)),!,call(ansicall(Ctrl,Cmd0)).

dbmsg(X):- dbmsg_cmt(X).
dbmsg_cmt(Var):- shrink_lisp_strings(Var,O), wdmsg(O).
dbmsg_real(X):- notrace(both_outputs(dbmsg0(X))),!.

in_comment(X):- notrace(setup_call_cleanup(write('/* '),(X),writeln(' */'))).

% is_assert_op(_,_):-!,fail.
is_assert_op(A,B,C):- notrace(is_assert_op0(A,B,C)),!.
is_assert_op0(A,_,_):- \+ compound(A),!,fail.
is_assert_op0(M:I,W,M:O):- !, is_assert_op0(I,W,O).
is_assert_op0(assert_lsp(W,P),W,P).
is_assert_op0(assertz(P),u,P).
is_assert_op0(asserta(P),u,P).
is_assert_op0(assert_lsp(P),u,P).
is_assert_op0(asserta_if_new(P),u,P).
is_assert_op0(assert_if_new(P),u,P).
is_assert_op0(asserta_new(P),u,P).
is_assert_op0(assert(P),u,P).

fmt99(O):- make_pretty(O,P),fmt999(P),!.

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
trim_off(W,A,B):- atomic(A), string_concat(W,B,A).
trim_off(_,A,A).

dbmsg0(Var):- var(Var),!,in_comment(colormsg1(dbmsg_var(Var))).
dbmsg0(Str):- string(Str),!,in_comment(colormsg1(Str)).
% dbmsg0(StringL):- to_prolog_string_if_needed(StringL,String),!,dbmsg0(String).
dbmsg0(:-((B,A))):-  is_assert_op(A,Where,AA), !,dbmsg0(:- B),dbmsg_assert(Where, AA).
dbmsg0(:-((A,B))):-  is_assert_op(A,Where,AA), !,dbmsg_assert(Where, AA),dbmsg0(:- B).
dbmsg0(:- A):- is_assert_op(A,Where,AA),!,dbmsg_assert(Where,AA).
dbmsg0(comment(X)):-!, shrink_lisp_strings(X,X0), in_comment(fmt99(X0)).
dbmsg0(N=V):- !, shrink_lisp_strings(N=V,X0),  in_comment(fmt99(X0)).
dbmsg0(A):- is_assert_op(A,Where,AA),!,dbmsg_assert(Where,AA).
dbmsg0(:- X):- X==true,!.
dbmsg0(:- X):- colormsg1(:- X),!.
dbmsg0(X):- colormsg1(:- X),!.

dbmsg_assert(Where,X):- notrace((dbmsg_assert0(Where,X))),!.

dbmsg_assert0(Where,(A,B)):- !,dbmsg_assert0(Where,A),dbmsg_assert0(Where,B).
dbmsg_assert0(Where,user:(HBody)):- !,dbmsg_assert0(Where,(HBody)).
dbmsg_assert0(Where,user:H :- Body):- !,dbmsg_assert0(Where,(H :- Body)),!.
%dbmsg_assert0(Where,M:Body:- (true,[])):-!,colormsg1("\n% asserting fact...\n"),!,colormsg1(M:Body),!.
dbmsg_assert0(Where,(Head:-Body)):- Body==true,!, dbmsg_assert0(Where,(Head)).
dbmsg_assert0(Where,(Head:-Body)):- !, colormsg1("\n% asserting... ~w ",[Where]),!,colormsg1(Head:-Body),!,
   assert_lsp(Head:-Body),
   assert_lsp(pass_clause(Where,Head,Body)),!.
dbmsg_assert0(Where,Head):- !, colormsg1("\n% asserting1... ~w ",[Where]),!,fmt99(Head),!,
   assert_lsp(Head),
   assert_lsp(pass_clause(Where,Head,true)),!.
   
/*
dbmsg_assert(Where,Body):-  
  body_cleanup(_,Body,Cleaned), !,
  (Body==Cleaned-> true;
   (colormsg1("\n% cleanup... ~w ",[Where]),!,colormsg1(Cleaned)),
   (colormsg1("\n% asserting... ~w ",[Where]),!,colormsg1(Body))),!,
    assert_lsp(Body),!.
*/


:- dynamic(lisp_compiler_option/2).
:- dynamic(lisp_compiler_option_local/2).
lisp_compiler_option(N,V):- V==true,!,lisp_compiler_option(N,t).
lisp_compiler_option(N,V):- nonvar(N), lisp_compiler_option_local(N,VV),!,V=VV.
lisp_compiler_option(N,V):- var(N), lisp_compiler_option_local(N,VV),V=VV.
lisp_compiler_option(speed,V):- !, (current_prolog_flag(runtime_speed,V)->true;V=1).
lisp_compiler_option(safety,V):- !, (current_prolog_flag(runtime_safety,V)->true;V=1).
lisp_compiler_option(debug,V):- !, (current_prolog_flag(runtime_debug,V)->true;V=1).
lisp_compiler_option(safe(_),t):- !, (lisp_compiler_option(safety,V),V>0).
lisp_compiler_option(_,TF):- lisp_compiler_option(safety,N),(N<1-> TF=t; TF=[]).

system:goal_expansion(always(G),G) :- lisp_compiler_option(speed,S),S>2.
system:goal_expansion(certainly(G),G) :- lisp_compiler_option(safety,0).

:- fixup_exports.
:- include('header').



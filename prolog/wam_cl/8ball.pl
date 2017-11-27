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


on_x_rtrace(G):- catch(G,E,(dbmsg(E),rtrace(G),break)).
atom_concat_or_rtrace(X,Y,Z):- on_x_rtrace(atom_concat(X,Y,Z)).
lisp_dump_break:- lisp_dumpST,break.
lisp_dumpST:- both_outputs(dumpST).

true_or_die(Goal):-functor(Goal,_,A),arg(A,Goal,Ret),always((Goal,Ret\==[])).

% Must always succeed (or else there is a bug in the lisp impl!)
always((A,B)):-!,always(A),always(B).
always(notrace(G)):- !, quietly_must_or_rtrace(G).
always(G):- notrace(tracing),G. % nonquietly_must_or_rtrace(G).
always(G):- nonquietly_must_or_rtrace(G).

% Must certainly succeed (or else there is a bug in the users code!)
certainly((A,B)):-!,certainly(A),certainly(B).
% certainly(notrace(G)):- !, quietly_must_or_rtrace(G).
certainly(G):- notrace(tracing),G. % nonquietly_must_or_rtrace(G).
certainly(G):- nonquietly_must_or_rtrace(G).

always_catch(G):- G. %  catch(catch(G,'$aborted',notrace),_,notrace).
with_nat_term(G):-
  \+ \+ ((
  (term_attvars(G,Vs),
    maplist(del_attr_rev2(freeze),Vs),
    maplist(del_attr_rev2(tracker),Vs),
   G))).

quietly_must_or_rtrace(G):- 
  (catch(quietly(G),E,gripe_problem(uncaught(E),G)) 
   *-> true ; (gripe_problem(fail_must_or_rtrace_failed,G),!,fail)),!.
nonquietly_must_or_rtrace(G):- 
  (catch((G),E,gripe_problem(uncaught(E),G)) 
   *-> true ; (gripe_problem(fail_must_or_rtrace_failed,G),!,fail)),!.


gripe_problem(Problem,G):- always_catch(gripe_problem0(Problem,G)).
gripe_problem0(Problem,G):-
     notrace(( 
     wdmsg((Problem:-G)),lisp_dumpST,
     dbmsg((Problem:-G)))),
     (rtrace(G)*->(notrace,break);(wdmsg(failed_rtrace(G)),notrace,break,!,fail)).


:- meta_predicate(timel(+,:)).
timel(What,M:X):- notrace(( write('% '),writeln(What))),prolog_statistics:time(M:X).

both_outputs(G):-
  notrace((current_output(O),stream_property(CO,alias(user_output)),
  (CO\==O -> with_output_to(CO,G) ; true),G)).


dbmsg(X):- both_outputs(dbmsg0(X)).

in_comment(X):- notrace((write('/* '),(X),writeln(' */'))).


dbmsg0(Var):- var(Var),!,in_comment(colormsg1(dbmsg_var(Var))).
dbmsg0(Str):- string(Str),!,in_comment(colormsg1(Str,[])).
dbmsg0(:- asserta(A)):- !, colormsg1(A).
dbmsg0(:- assert(A)):- !, colormsg1(A).
dbmsg0(:-((B,asserta(A)))):- !, dbmsg0(:- B), dbmsg0(:-asserta(A)).
dbmsg0(:-((asserta(A),B))):- !, dbmsg0(:-asserta(A)),dbmsg0(:- B).
dbmsg0(comment(S)):- in_comment(fmt9(S)).
dbmsg0(N=V):- in_comment(fmt9(N=V)).
dbmsg0(H :- Body):- !,colormsg1(H :- Body),!.
dbmsg0(:- Body):- !,colormsg1(:- Body),!.
dbmsg0(Body):- in_comment(colormsg1(:- Body)),!.
% dbmsg(:- Body):- !, dmsg(:- Body).

colormsg1(Msg,Args):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl,format(Msg,Args)).
colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl,fmt90(Msg)).

:- dynamic(lisp_compiler_option/2).
lisp_compiler_option(speed,N):- current_prolog_flag(runtime_speed,N).
lisp_compiler_option(safety,N):- current_prolog_flag(runtime_safety,N).
lisp_compiler_option(safe(_),true).
lisp_compiler_option(safe(_),true):- lisp_compiler_option(safety,N),N<2.
lisp_compiler_option(_,2).

system:goal_expansion(always(G),G) :- lisp_compiler_option(speed,0).
system:goal_expansion(certainly(G),G) :- lisp_compiler_option(safety,0).

:- fixup_exports.
:- include('header.pro').



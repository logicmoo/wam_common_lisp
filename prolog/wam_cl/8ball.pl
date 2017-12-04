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


show_call_trace(G):- G *-> wdmsg(G); (wdmsg(warn(failed(show_call_trace(G)))),fail).

on_x_rtrace(G):- catch(G,E,(dbmsg(E),rtrace(G),break)).
atom_concat_or_rtrace(X,Y,Z):- tracing->atom_concat(X,Y,Z);catch(atom_concat(X,Y,Z),_,break).
lisp_dump_break:- lisp_dumpST,break.
lisp_dumpST:- both_outputs(dumpST).

true_or_die(Goal):-functor(Goal,_,A),arg(A,Goal,Ret),always((Goal,Ret\==[])).

% Must always succeed (or else there is a bug in the lisp impl!)
always((A,B)):-!,always(A),always(B).
always(notrace(G)):- !, quietly_must_or_rtrace(G).
always(G):- notrace(tracing),!,(G->true;break). % nonquietly_must_or_rtrace(G).
always(G):- nonquietly_must_or_rtrace(G),!.

% Must certainly succeed (or else there is a bug in the users code!)
certainly((A,B)):-!,certainly(A),certainly(B).
% certainly(notrace(G)):- !, quietly_must_or_rtrace(G).
certainly(G):- notrace(tracing),!,G. % nonquietly_must_or_rtrace(G).
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
     trace,break,
     (rtrace(G)*->(notrace,break);(wdmsg(failed_rtrace(G)),notrace,break,!,fail)).


:- meta_predicate(timel(+,:)).
timel(What,M:X):- notrace(( write('% '),writeln(What))),prolog_statistics:time(M:X).


is_user_output:- current_output(O),stream_property(CO,alias(user_output)),!,CO==O.

both_outputs(G):-
  (is_user_output -> G ; (with_output_to(user_output,G),G)).


dbmsg(X):- both_outputs(dbmsg0(X)).

in_comment(X):- notrace((write('/* '),(X),writeln(' */'))).



dbmsg0(Var):- var(Var),!,in_comment(colormsg1(dbmsg_var(Var))).
dbmsg0(Str):- string(Str),!,in_comment(colormsg1(Str,[])).
% dbmsg0(StringL):- to_prolog_string_if_needed(StringL,String),!,dbmsg0(String).
dbmsg0(:-((B,asserta(A)))):- !, dbmsg0(:- B), dbmsg0(:-asserta(A)).
dbmsg0(:-((asserta(A),B))):- !, dbmsg0(:-asserta(A)),dbmsg0(:- B).

dbmsg0(comment(X)):- shrink_lisp_strings(X,X0), in_comment(fmt9(X0)).
dbmsg0(N=V):- shrink_lisp_strings(N=V,X0),  in_comment(fmt9(X0)).
dbmsg0(:- asserta(A)):- !, colormsg1(A).
dbmsg0(:- assert(A)):- !, colormsg1(A).
dbmsg0(H :- Body):- !,colormsg1(H :- Body),!.
dbmsg0(:- Body):- !,colormsg1(:- Body),!.
dbmsg0(X):- shrink_lisp_strings(X,X0), in_comment(colormsg1(:- X0)),!.
% dbmsg(:- Body):- !, dmsg(:- Body).


colormsg1(Msg,Args):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,format(Msg,Args)).
colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt90(Msg)).

ansicall_maybe(Ctrl,Cmd):- current_output(O), \+ stream_property(O,tty(true)),!,call(ansicall(Ctrl,Cmd)).
ansicall_maybe(Ctrl,Cmd):- shrink_lisp_strings(Cmd,Cmd0),call(ansicall(Ctrl,Cmd0)).

:- dynamic(lisp_compiler_option/2).
:- dynamic(lisp_compiler_option_local/2).
lisp_compiler_option(N,V):- lisp_compiler_option_local(N,VV),!,V=VV.
lisp_compiler_option(speed,V):- !, (current_prolog_flag(runtime_speed,V)->true;V=1).
lisp_compiler_option(safety,V):- !, (current_prolog_flag(runtime_safety,V)->true;V=1).
lisp_compiler_option(debug,V):- !, (current_prolog_flag(runtime_debug,V)->true;V=1).
lisp_compiler_option(safe(_),t):- (lisp_compiler_option(safety,V),V>0).
lisp_compiler_option(_,TF):- lisp_compiler_option(safety,N),(N<1-> TF=t; TF=1).

system:goal_expansion(always(G),G) :- lisp_compiler_option(speed,S),S>2.
system:goal_expansion(certainly(G),G) :- lisp_compiler_option(safety,0).

:- fixup_exports.
:- include('header.pro').



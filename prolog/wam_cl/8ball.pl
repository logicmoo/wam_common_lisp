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

on_x_rtrace(G):- catch(G,E,(dbmsg(E),rtrace(G),break)).
atom_concat_or_rtrace(X,Y,Z):- tracing->atom_concat(X,Y,Z);catch(atom_concat(X,Y,Z),_,break).

lisp_dump_break:- both_outputs(dumpST),!,throw(lisp_dump_break).
lisp_dump_break:- throw(lisp_dump_break).
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
     trace,lisp_dump_break,
     (rtrace(G)*->(notrace,lisp_dump_break);(wdmsg(failed_rtrace(G)),notrace,lisp_dump_break,!,fail)).


:- meta_predicate(timel(+,:)).
timel(What,M:X):- notrace(( write('% '),writeln(What))),prolog_statistics:time(M:X).


is_user_output:- current_output(O),stream_property(CO,alias(user_output)),!,CO==O.

both_outputs(G):-
  (is_user_output -> G ; (with_output_to(user_output,G),G)).


%colormsg1(Msg,Args):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,format(Msg,Args)).
%colormsg1(Msg):- writeq(Msg),nl,nl,!. %notrace(colormsg11(Msg)).
colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt90(Msg)).

%ansicall_maybe(_Ctrl,Cmd):- !,nl,nl,portray_clause_w_vars(Cmd),nl,nl,(Cmd),break.
ansicall_maybe(_Ctrl,Cmd):- current_output(O), \+ stream_property(O,tty(true)),!,call(Cmd).
ansicall_maybe(Ctrl,Cmd):- always(shrink_lisp_strings(Cmd,Cmd0)),!,call(ansicall(Ctrl,Cmd0)).

dbmsg_cmt(Var):- shrink_lisp_strings(Var,O), wdmsg(O).
dbmsg(X):- dbmsg_cmt(X).
dbmsg_real(X):- notrace(both_outputs(dbmsg0(X))),!.

in_comment(X):- notrace(setup_call_cleanup(write('/* '),(X),writeln(' */'))).

% is_assert_op(_,_):-!,fail.
is_assert_op(A,_):- \+ compound(A),!,fail.
is_assert_op(M:I,M:O):- is_assert_op(I,O).
is_assert_op(asserta(P),P).
is_assert_op(assertz(P),P).
is_assert_op(assert_if_new(P),P).
is_assert_op(assert(P),P).

% notrace((dbmsg0(Var))).

dbmsg0(Var):- var(Var),!,in_comment(colormsg1(dbmsg_var(Var))).
dbmsg0(Str):- string(Str),!,in_comment(colormsg1(Str)).
% dbmsg0(StringL):- to_prolog_string_if_needed(StringL,String),!,dbmsg0(String).
dbmsg0(:-((B,A))):-  is_assert_op(A,AA), !,dbmsg0(:- B),dbmsg_assert( AA).
dbmsg0(:-((A,B))):-  is_assert_op(A,AA), !,dbmsg_assert( AA),dbmsg0(:- B).
dbmsg0(:- A):- is_assert_op(A,AA),!,
  dbmsg_assert(AA).

dbmsg0(comment(X)):- shrink_lisp_strings(X,X0), in_comment(fmt9(X0)).
dbmsg0(N=V):- shrink_lisp_strings(N=V,X0),  in_comment(fmt9(X0)).
%dbmsg0(:- Body):- !,colormsg1(:- Body),!.
dbmsg0(X):- colormsg1(X),!.
% dbmsg(:- Body):- !, dmsg(:- Body).

dbmsg_assert(user:(HBody)):- !,dbmsg_assert((HBody)).
dbmsg_assert(user:H :- Body):- !,dbmsg_assert(H :- Body),!.
dbmsg_assert(M:Body:- (true,[])):-!,colormsg1("\n% asserting fact...\n"),!,colormsg1(M:Body),!.
dbmsg_assert(Body):- colormsg1("\n% asserting...\n"),!,colormsg1(Body),!.

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



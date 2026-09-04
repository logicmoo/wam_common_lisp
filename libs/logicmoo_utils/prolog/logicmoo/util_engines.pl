% ===================================================================
% File 'logicmoo_util_engines.pl'
% Purpose: An Implementation in SWI-Prolog of Comparable context frames
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_ctx_frame.pl' 1.1.1
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2035/12/16 01:57:18 $
% ===================================================================
% ===================================================================
%  
%   Run multiple goals (in separate thread/engines)
%
               
:- module(logicmoo_util_engines,
  [ result_check/3,
    on_diff_fail/2,
    on_diff_throw/2,
    call_diff/3,
    intersect_eq0/3,
   % member_eq0/2,
    collecting_list/4,

    start_listening/1,
    call_in_engine/1,
    start_goal_saved1/0,
    wait_until_next_request/0,
    next_solution/0,
    next_solution/1
  ]).

:- meta_predicate
   collecting_list(0,+,+,+),
   result_check(0,+,+),
   call_in_engine(0),
   on_diff_throw(0,0),
   on_diff_fail(0,0),
   call_diff(2,0,0).
   
   

result_check(Call, _ + NVs, _ + OVs):-
   NVs=@=OVs -> true; Call.


/*

  % I'd like these two to run at the same time (Did I really 
  need to start a thread with send_messages)
 
 ?- on_diff_fail(member(X,[1,2,3,4]),member(X,[1,2,x,4])). 
   X = 1 ;
   X = 2 ;
   X = 4 ;
   No.
*/

on_diff_fail(Left,Right):- 
  call_diff(result_check(fail),Left,Right).

on_diff_throw(Left,Right):- 
  call_diff(result_check(trace_or_throw(different(Left,Right))),Left,Right).

call_diff(Check,Left,Right):- 
   shared_vars(Left,Right,Vs),
 % Right must be able to diverge
   copy_term(Right+Vs,CO+CVs),
 % Later on we''ll allow different result orders
   NOL = saved_in([],[]), 
   collecting_list(call(CO),CVs,1,NOL),
   collecting_list(call(Left),Vs,2,NOL),
   call(Check,Left+Vs,Right+CVs).

collecting_list(G,Vs,At,S):- 
   call(G),copy_term(Vs,CVs),
   arg(At,S,Was),nb_setarg(At,S,[CVs|Was]).


:- thread_initialization(nb_setval(query_result,sol(0,1,false,false))).

% sol(number,G,successfull,done)
next_solution:- quietly(next_solution(How)),call(How).

next_solution(throw(no_query_result)) :- \+ nb_current(query_result,_),!.
next_solution(request_next0) :- nb_getval(query_result,sol(_,_,true,false)),!.
next_solution(nop(last(G))) :- nb_getval(query_result,sol(_,G,true,true)),!.
next_solution(request_next0) :- nb_getval(query_result,sol(_,_,_,false)),!.
next_solution(nop(unknown(QR))) :- nb_getval(query_result,QR),!.

request_next0 :- 
  quietly((thread_send_message(ask1,please(next_sol)), !, 
  thread_get_message(answer1,M),wdmsg(rn(M)),nb_setval(query_result,M))),!.

call_in_engine(G):- 
  once((nb_setval(in,v(_,G,_)), 
    start_goal_saved1)),fail.
call_in_engine(G):- next_solution, get_sol(G).


get_sol(G):-
  repeat,
  next_solution,
  nb_getval(query_result,M),
  react_message(M,G,(ReactA,ReactB)),
  wdmsg(M),
  (ReactA == ! -> ! ; ReactA),
   ReactB.

react_message(sol(_,_,unknown,false),_,(true,fail)):-!.
react_message(sol(_,_,false,_),_,(!,fail)):- !.
react_message(sol(_,G,true,false),G,(true,true)):-!.
react_message(sol(_,G,true,true),G,(!,true)):- !.


call_goal_in_thread_saved_nd:- start_goal_saved1, fail.
call_goal_in_thread_saved_nd:- next_solution.

:-  message_queue_property(_Queue, alias(answer1)) -> true;message_queue_create(_,[alias(answer1)]).
:-  message_queue_property(_Queue, alias(ask1)) -> true;message_queue_create(_,[alias(ask1)]).


start_goal_saved1:- 
  quietly((nb_getval(in,v(_,G,_,_)),
  thread_create(start_listening(G),_ID,[detached(true)]))),
  nb_setval(query_result,sol(0,G,unknown,false)),!.

wait_until_next_request:-
   thread_get_message(ask1,please(C),[]),
   (C == more_sols -> true; (C == completed -> (!,fail) ; (true))).

thread_send_answer(Left,G,TF,Done):- wdmsg(next_________sol(Left,G,TF,Done)), thread_send_message(answer1,sol(Left,G,TF,Done)).

:- meta_predicate start_listening(0).
start_listening(G):-
  quietly((flag(sol,_,0),
  ((thread_send_answer(0,G,unknown,false)),
  thread_get_message(ask1,please(next_sol),[]),  
  ignore(((
  ((G,deterministic(Det),flag(sol,I,I+1))
    *-> (flag(sol,X,X),thread_send_answer(X,G,true,Det),wait_until_next_request) ; 
    flag(sol,X,X),thread_send_answer(X,G,false,true))
    ),fail))))).
  

intersect_eq0([], _, []).
intersect_eq0([X|Xs], Ys, L) :-
 	(   member_eq0(X, Ys)
 	->  L = [X|T],
 	    intersect_eq0(Xs, Ys, T)
 	;   intersect_eq0(Xs, Ys, L)
 	).



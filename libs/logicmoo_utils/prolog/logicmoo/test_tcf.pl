
:- module(test_tcf, []).

:- set_module(class(library)).


:- meta_predicate(try_call_finally(0,0,0)).
:- export(try_call_finally/3).
try_call_finally(S, G, C) :-
   call_cleanup_(S, C),
   deterministic_(G, F),
   (F == true, !; call_cleanup_(C, S)).

/*
deterministic_(G, F) :-
   G,
   deterministic(F),
   otherwise. /* prevent tail recursion */
*/
call_cleanup_(G, C) :-
   call_cleanup((G; fail), C). /* prevent early determinism */


/*
:- dynamic(try_c_f_state/1).
try_c_f_state(W):- writeln(W=true).

:- 
    WriteFalse = ( try_c_f_state(W):- writeln(W=false), ! ),
    setup_call_cleanup_each(
         asserta(WriteFalse,Ref),
         ( (X=1;X=2), try_c_f_state(X) ),
         erase(Ref)), 
    try_c_f_state(X),
    fail.
  
  ?- main.
  
  1=false
  1=true
  2=false
  2=true
*/
is_try_call_finally_pred(try_call_finally).
is_try_call_finally_pred(each_call_cleanup).

try_try_call_finally1:- test_try_call_finally(1,try_call_finally).

test_try_call_finally(1,P):-
 is_try_call_finally_pred(P),
 doall((

  assert((try_c_f_state(W):- writeln(W=true))),

  WriteFalse = ( try_c_f_state(W):- writeln(W=false), ! ),
  call(P,
       asserta(WriteFalse,Ref),
       ( member(X,[1,2,3]), try_c_f_state(X) ),
       erase(Ref)), 
  try_c_f_state(X)

 )).

/*
?- try_call_finally(writeln('in'), member(X,[1,2,3]), writeln('out')).
  in
  out
  X = 1 ;
  in
  out
  X = 2 ;
  in
  out
  X = 3.
*/
try_try_call_finally2:-
  try_call_finally(
      writeln('in'), 
      member(X,[1,2,3]), 
      writeln('out')),
  writeln('X'=X).
  fail.
try_try_call_finally2.

/*
:-  try_call_finally(
       gensym(hi_,X),
       member(N,[1,2,3]),
       write(X=N)),
    fail.

hi_0 = 1
hi_1 = 2
hi_2 = 3
No.
*/

try_try_call_finally3:- 
   try_call_finally(
       gensym(hi_,X),
       member(N,[1,2,3]),
       writeln(c(X=N))),
   writeln(o(X=N)),
   fail.
try_try_call_finally3.

% ?- try_call_finally(writeln('in'), member(X,[1,2,3]), writeln('out')).
% in
% out
% X = 1 ;
% in
% out
% X = 2 ;
% in
% out
% X = 3.

redo_call_cleanup_v1(Setup,Call,Cleanup):-
   CallCleanup = call(Cleanup),
   CleanupOnce = (CallCleanup, b_setarg(1,CallCleanup,true)),
   SetupAndClean = (Setup,undo(CleanupOnce)),
   call_cleanup( 
     (SetupAndClean, Call, undo(SetupAndClean)), 
     CleanupOnce).

redo_call_cleanup_v2(Setup,Call,Cleanup):-
   CallCleanup = call(Cleanup),
   CleanupOnce = (CallCleanup, b_setarg(1,CallCleanup,true)),
   call_cleanup(
      (repeat,Setup,undo(CleanupOnce), 
         (Call*-> true 
           /*kill repeat*/ 
           ;(!,fail)), 
         (deterministic(true) -> ! ; CleanupOnce)),
    CleanupOnce).
/*
no_trace(Goal):-  
   notrace(notrace),!,
      setup_call_cleanup(
        notrace,
        (undo(notrace),Goal,trace),
        trace).
*/
:- fixup_exports.


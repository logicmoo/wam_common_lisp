%% SIPC - Scheme In Prolog with Continuation Semantics

%% a simple implementation of Scheme in Prolog (w. continuation semantics)
%% Tim Finin, University of Pennsylvania, Mon Oct  6 10:11:18 1986

%% note: in order to handle side effects in a reasonable way,
%% environments are stored in the database.  The result is that old
%% environments never die.  They don't even fade away!

% operators

:- set_prolog_flag(backquoted_string,false).
:- dynamic(((( ==> ))/2)).
%:- op(1100,xfx, (==)).	

:- op(900,xfy,user:'==>').		% used for macros.
:- op(500,fx,user:'`').		% `x = quote(x).

:- thread_local(t_l:bound/3).

%:- use_module(library(sexpr_reader)).

% sip -  creates a virgin initial environment and enters a 
% read-eval-print loop for SIP.  Exit by typing "prolog(`abort)."

ssip :- create0Env, sipREP2.

%  sipREP -  enters a read-eval-print loop for SIP.
sipREP2 :-
  repeat,
  writeln('SIP> '),
  read(E),
  once(eval3(print(E),0,k(nil))),
  fail.


% create0Env sets up E as the inital Scheme environment.
create0Env :-
  % flush all old environments.
  % abolish(t_l:bound,3),
   retractall(t_l:bound(_,_,_)),
  % (re)set the environment counter.
  abolish(envNum,1),
  assert(envNum(1)),
  % define the initial variables.
  load('sipcore.pl').

% abbreviations for eval/3.
eval(E) :- eval3(E,0,k(nil)),!.
eval(E,V) :- eval3(E,0,k(nil)),!.
eval(E,Env,C) :- eval3(E,Env,C),!.

% like eval3, but "returns" its value to caller.
evalReturn(Exp,Env,Val) :- eval3(Exp,Env,k(return(Val))).


% eval3(expression,environment,continuation)

eval3(X,_,C) :- number(X),applyC(C,X).

eval3(X,E,C) :- atomic(X),!,lookUp(X,V,E),applyC(C,V).

% syntax kludge - call a fn F of no arguments as [F].
% bug - doen't check for special forms!!!
eval3([F|Args],E,C) :- !,eval3(F,Env,k(evalArgs(Args,[]),Env,C)).

% special forms ...

eval3(quote(E),_,C) :- !,applyC(C,E).

eval3(lambda(Vars,Body),E,C) :- !, applyC(C,closure(Vars,Body,E)).

eval3(set(Sym,Val),E,C) :- !, eval3(Val,E,k(set(Sym),E,C)).

eval3(define(Sym,Exp),Env,C) :- !, eval3(Exp,Env,k(define(Sym),Env,C)).

eval3(if(E1,E2,E3),Env,C) :- !, eval3(E1,Env,k(if(E2,E3),Env,C)).

eval3(and(X,Y),Env,C) :- !, eval3(X,Env,k(and(Y),Env,C)).

eval3(or(X,Y),Env,C) :- !, eval3(X,Env,k(or(Y),Env,C)).

eval3(delay(X),Env,C) :- applyC(C,promise(X,Env,_)).

eval3(F1==>F2,_,C) :- !, assert(F1==>F2), applyC(C,F2).

% this should be done via apply.
eval3(eval(X),Env,C) :- !, eval3(X,Env,k(eval,Env,C)).

% check for a "macro" application.
eval3(X,E,C) :- (X==>NewX), !, eval3(NewX,E,C).
  
% non-special form...
eval3(E,Env,C) :-
  !,
  E =.. [F|Args],
  eval3(F,Env,k(evalArgs(Args,[]),Env,C)).

% applyC(continuation,value) ...
% a continuation has one of the forms:
%      k(operation,expression,next continuation)
%      k(nil)
%      k(return(Value))

applyC(k(if(Then,Else),E,C),false) :- !,eval3(Else,E,C).
applyC(k(if(Then,Else),E,C),_) :- eval3(Then,E,C).

applyC(k(set(Sym),E,C),V) :- set(Sym,V,E,OldV),applyC(C,OldV).

applyC(k(define(Sym),E,C),V) :- define(Sym,V,E),applyC(C,Sym).

applyC(k(and(X),E,C),V) :- V=false->applyC(C,false);eval3(X,E,C).

applyC(k(or(X),E,C),V) :- V=false->eval3(X,E,C);applyC(C,V).

applyC(k(eval,E,C),V) :- eval3(V,E,C).

applyC(k(evalArgs([In1|InRest],Out),E,C),V) :-
  eval3(In1,E,k(evalArgs(InRest,[V|Out]),E,C)).

applyC(k(evalArgs([],RCall),E,C),V) :-
  reverse([V|RCall],Call),
  Call = [F|Args],
  apply(F,Args,C).

% we're done
applyC(k(return(V)),V) :- !.
applyC(k(nil),V) :- !.

% apply(Function,Args,COntinuation) - apply a prmitive function.

apply(pf(callCC),[F],C) :- !, apply(F,[C],C).

apply(pf(X),Args,C) :- 
  !, 
  applyPrim(X,Args,V),
  applyC(C,V).

% apply a compound function.
apply(closure(Parameters,Body,Env),Args,Value) :- !,
  makeEnv(Parameters,Args,New/Env),
  eval3(Body,New/Env,Value),
  !.

% this should never happen.
apply(X,Args,_) :- err('I dont know how to apply:', [X|Args]).

% applyPrim(function,arguments,value) 

applyPrim('+',[A1,A2],V) :- V is A1+A2.
applyPrim('-',[A1,A2],V) :- V is A1-A2.
applyPrim('*',[A1,A2],V) :- V is A1*A2.
applyPrim('=',[A1,A2],V) :- A1=A2->V=true;V=false.
applyPrim(cons,[A1,A2],[A1|A2]).
applyPrim(car,[[H|T]],H).
applyPrim(cdr,[[H|T]],T).
applyPrim(begin,[A1,A2],A2).
applyPrim(force,[promise(B,E,V)],V) :-  var(V)->evalReturn(B,E,V);true.
applyPrim(load,[File],File) :- !,load(File).
applyPrim(prolog,[X],V) :- !,call(X) -> V=X;V=false.
applyPrim(print,[X],true) :- !,writeln(X).


applyPrim(F,Args,error) :- 
 Call =.. [F|Args],
 err('bad call to a primitive function',Call).

% evalList(listOfArguments,listOfValues,environment)
% evlas a list of expressions and returns a list of the results.
evalList([],[],_).
evalList([A1|Arest],[V1|Vrest],E) :-
  eval3(A1,E,V1),
  evalList(Arest,Vrest,E).

% makeEnv(+Parameters,+Arguments,-Environment) -  creates a new environment
% in which the variables in the 1st arg are t_l:bound to the values in the
% 2nd.  The new envitronment is returned in the 3rd.
makeEnv(Ps,As,New/Old) :-
  % determine the next environment number to use.
  retract(envNum(N)),
  New is N+1,
  assert(envNum(New)),
  !,
  % add the binding to the new environment.
  addBindings(Ps,As,New/Old).

% addBindings(Variables,Values,Environment) binds variables to 
%  corresponding values in the specified Environment.
addBindings([],[],_) :- !.
addBindings([P|Ps],[V|Vs],Env) :-
  !,
  define(P,V,Env),
  addBindings(Ps,Vs,Env).
addBindings([_|_],[],_) :-  !, err('too few arguments').
addBindings([],[_|_],_) :-  !, err('too many arguments').

% looks up the values associated with a symbol in an environment.  It's
% an error if there is no binding.
lookUp(Symbol,Value,Env) :- value(Symbol,Value,Env,_),!.
lookUp(S,_,Env) :-  err('unbound symbol: ',S/Env).

% value(+symbol,-value,+frameSought,-frameFound) like lookUp but also
% returns the frame in which the variable was t_l:bound.
value(S,V,Env,Env) :- t_l:bound(S,V,Env),!.
value(S,V,SIN,E) :- compound(SIN),SIN = (E1/E2),
  \+ (t_l:bound(S,V,SS),compound(SS),SS = E1/E2 ),
  value(S,V,E2,E).

% change the value associated with symbol S to V, returning the old value.
set(S,V,Env,OldV) :-
  value(S,OldV,Env,BindingEnv),
  !,
  retract(t_l:bound(S,OldV,BindingEnv)),
  assert(t_l:bound(S,V,BindingEnv)).

set(S,_,E,_) :-  err('symbol not t_l:bound in environment:',(S/E)).

% add an initial binding for symbol S to value V in environment Env.
define(S,V,Env) :-
  sip_when(retract(t_l:bound(S,_,Env)), 
       warn('symbol already defined in environment: ',(S,Env))),
  assert(t_l:bound(S,V,Env)).

% load(F) reads and evals all expressions in file F.
/*
load(File):- see(File),!,
  repeat,
  call_cleanup((read(X), ((X = end_if_file ) -> true;((once(loadProcess(X)),fail)))),seen),!.
*/

load(File) :-
  open(File,read,S),
  repeat,read_term(S,X,[module(user),backquoted_string(false)]),  
  (end_of_file == X -> close(S) ; (loadProcess(X),fail)),  
  !.

loadProcess(end_of_file).
loadProcess(X) :- eval(X),fail.

%%% misc. utilities ...

err(Msg) :- warn(Msg),!,fail.
err(Msg1,Msg2) :- warn(Msg1,Msg2),!,fail.

warn(Msg) :- writeln(Msg).
warn(Msg1,Msg2) :-writeln(Msg1),write(' '),write(Msg2).

% once(X) executes X only once.
%once(X) :- X,!.

%writeln(X) :- nl,write(X).

sip_when(Test,Then) :- Test->Then;true.



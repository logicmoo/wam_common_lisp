%% SIP - Scheme In Prolog

%% a simple implementation of Scheme in Prolog (w/o continuation semantics)
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

sip :- create0Env, sipREP1.

%  sipREP -  enters a read-eval-print loop for SIP.
sipREP1 :-
  repeat,
  writeln('SIP> '),
  read_term(E,[backquoted_string(false)]),
  once(eval3(E,0,V)),
  writeln(V),
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
eval(E) :- eval3(E,0,V),!.
eval(E,V) :- eval3(E,0,V),!.
eval(E,Env,V) :- eval3(E,Env,V),!.

% syntax kludge - call a fn F of no arguments as [F].
% bug - doen't check for special forms!!!
eval3([F|Args],E,C) :- !,eval3(F,Env,k(evalArgs(Args,[]),Env,C)).

% eval3(expression,environment,value), mode(+,+,-)

eval3(X,E,X) :- number(X),!.

eval3(X,E,V) :- atomic(X),!,lookUp(X,V,E).

% special forms ...

eval3(quote(E),_,E) :- !.

eval3(lambda(_,_),E,closure(_,_,E)) :- !.

eval3(set(Sym,Val),E,V) :- !,
  eval3(Val,E,V),
  set(Sym,V,Env,OldValue).

eval3(define(Sym,Exp),Env,Sym) :- !, eval3(Exp,Env,V), define(Sym,V,Env).

eval3(if(E1,E2,E3),Env,V) :- !,
  eval3(E1,Env,false)->
     eval3(E3,Env,V);
     eval3(E2,Env,V).

eval3(eval(X),Env,V) :- !, eval3(X,Env,V1), eval3(V1,Enc,V).

eval3(and(X,Y),Env,V) :- !, eval3(X,Env,false) -> V=false;eval3(Y,Env,V).

eval3(or(X,Y),Env,V) :- !, 
  eval3(X,Env,Vx),
  Vx=false -> eval3(Y,Env,V);V=Vx.
  
eval3(delay(X),Env,promise(X,Env,_)) :- !.

eval3(F1==>F2,E,F1==>F2) :- !,assert(F1==>F2).

% call/cc
eval3(callCC(_),_,_) :-!,err('callCC not implemented').

% check for a "macro" application.
eval3(X,E,V) :- (X==>NewX), !, eval3(NewX,E,V).
  
% non-special form...
eval3(E,Env,V) :-
  !,
  E =.. Call,
  evalList(Call,[F|Args],Env),
  apply(F,Args,V).

% apply(F,L,V) applys function F to the list of arguments L yielding value V.

% apply a prmitive function.
apply(pf(X),A,V) :- !, applyPrim(X,A,V),!.

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
applyPrim(force,[promise(B,E,V)],V) :-  var(V)->eval3(B,E,V);true.
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



/*
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">
<html>
 <head>
  <title>Index of /~finin//sip</title>
 </head>
 <body>
<h1>Index of /~finin//sip</h1>
<ul><li><a href="/~finin//"> Parent Directory</a></li>
<li><a href="README.txt"> README.txt</a></li>
<li><a href="sip.msg"> sip.msg</a></li>
<li><a href="sip1"> sip1</a></li>
<li><a href="sip2"> sip2</a></li>
<li><a href="sipcore"> sipcore</a></li>
<li><a href="siptest"> siptest</a></li>
</ul>
</body></html>
SIP, Tim Finin (tim@cis.upenn.edu), University of Pennsylvania, April 1987.

SIP (Scheme in Prolog) is a partial interpreter for a subset of
Scheme.  There are two versions: one with (sip2) and one without
(sip1) continuation semantics.  The relevant files are:

	sip1	- interpreter w/o continuations.
	sip2	- interpreter with continuations
	sipcore	- base file for both, defines primitive variables, etc.
	siptest - examples

Start SIP by loading either "sip1" or "sip2" and issuing the query:

	?- sip.

This should create an initial environment (by loading the file
"sipcore" and then enter a read-eval-print loop.  You can re-start the
read-eval-print loop w/o recreating the initial environment by issuing
the prolog query:

	?- sipREP.

A few notes:

The bad news: the syntax is unLispy, due to the prolog reader. Instead
of (foo a 1) you must say foo(a,1).  Look at the file "siptest" to see
some samples of simple expressions.  Similarly, you must type a "."
after every input.  The usual prolog special characters (e.g.
operators) may cause the reader to complain.  There is no way to call
a function with no arguments!  The good news: infix operators are
allowed, to some degree.  In particular "S == E" means (define S E).

More syntax kludges: Cal a no-argument function FOO like [FOO].  Call
a lambda-form like: [lambda([n],n+2),3].  BEGIN only takes two
arguments - you can embed them, of course.

evaling prolog(P) invokes prolog on query P and returns #!t if it
succeeded and #!F otherwise.

The character ` acts like ' in normal Scheme in that it quotes the
next expression in the input.

evaling prolog(`abort) is the (dirty) way to exit the read-eval-print
loop.  In fact, evaling prolog(`abort) is the only way to exit the
read-eval-print loop.

Just to make my job easier, I added a macro like facility.  Evaling S1
==> S2 defines a rewriting type macro that transforms S1 into S2.  For
example, we could (and do) do:

		tail(X) ==> force(cdr(X)).

*/

:- if(                                                                  ).
	
%% SIP - Scheme In Prolog

%% a simple implementation of Scheme in Prolog (w/o continuation semantics)
%% Tim Finin, University of Pennsylvania, Mon Oct  6 10:11:18 1986

%% note: in order to handle side effects in a reasonable way,
%% environments are stored in the database.  The result is that old
%% environments never die.  They don't even fade away!

% operators

:- op(900,xfy,'==>').		% used for macros.
:- op(500,fx,'`').		% `x = quote(x).

% sip -  creates a virgin initial environment and enters a 
% read-eval-print loop for SIP.  Exit by typing "prolog(`abort)."

sip :- create0Env, sipREP.

%  sipREP -  enters a read-eval-print loop for SIP.
sipREP :-
  repeat,
  writeln('SIP> '),
  read(E),
  once(eval3(E,0,V)),
  writeln(V),
  fail.

% create0Env sets up E as the inital Scheme environment.
create0Env :-
  % flush all old environments.
  abolish(bound,3),
  % (re)set the environment counter.
  abolish(envNum,1),
  assert(envNum(1)),
  % define the initial variables.
  load(sipcore).

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
% in which the variables in the 1st arg are bound to the values in the
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
lookUp(S,_,Env) :-  err('unbound symbol: ',S-Env).

% value(+symbol,-value,+frameSought,-frameFound) like lookUp but also
% returns the frame in which the variable was bound.
value(S,V,Env,Env) :- bound(S,V,Env).
value(S,V,E1/E2,E) :-
  not(bound(S,V,E1/E2)),
  value(S,V,E2,E).

% change the value associated with symbol S to V, returning the old value.
set(S,V,Env,OldV) :-
  value(S,OldV,Env,BindingEnv),
  !,
  retract(bound(S,OldV,BindingEnv)),
  assert(bound(S,V,BindingEnv)).

set(S,_,E,_) :-  err('symbol not bound in environment:',(S/E)).

% add an initial binding for symbol S to value V in environment Env.
define(S,V,Env) :-
  when(retract(bound(S,_,Env)), 
       warn('symbol already defined in environment: ',(S,Env))),
  assert(bound(S,V,Env)).

% load(F) reads and evals all expressions in file F.
load(File) :-
  see(File),
  repeat,
  read(X),
  loadProcess(X),
  seen,
  !.

loadProcess(end_of_file).
loadProcess(X) :- eval(X),fail.

%%% misc. utilities ...

err(Msg) :- warn(Msg),!,fail.
err(Msg1,Msg2) :- warn(Msg1,Msg2),!,fail.

warn(Msg) :- writeln(Msg).
warn(Msg1,Msg2) :-writeln(Msg1),write(' '),write(Msg2).

% once(X) executes X only once.
once(X) :- X,!.

writeln(X) :- nl,write(X).

when(Test,Then) :- Test->Then;true.

:- else.

%% SIPC - Scheme In Prolog with Continuation Semantics

%% a simple implementation of Scheme in Prolog (w. continuation semantics)
%% Tim Finin, University of Pennsylvania, Mon Oct  6 10:11:18 1986

%% note: in order to handle side effects in a reasonable way,
%% environments are stored in the database.  The result is that old
%% environments never die.  They don't even fade away!

% operators

:- op(900,xfy,'==>').		% used for macros.
:- op(500,fx,'`').		% `x = quote(x).

% sip -  creates a virgin initial environment and enters a 
% read-eval-print loop for SIP.  Exit by typing "prolog(`abort)."

sip :- create0Env, sipREP.

%  sipREP -  enters a read-eval-print loop for SIP.
sipREP :-
  repeat,
  writeln('SIP> '),
  read(E),
  once(eval3(print(E),0,k(nil))),
  fail.


% create0Env sets up E as the inital Scheme environment.
create0Env :-
  % flush all old environments.
  abolish(bound,3),
  % (re)set the environment counter.
  abolish(envNum,1),
  assert(envNum(1)),
  % define the initial variables.
  load(sipcore).

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
apply(closure(Parameters,Body,Env),Args,C) :- !,
  makeEnv(Parameters,Args,New/Env),
  eval3(Body,New/Env,C),
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

% makeEnv(+Parameters,+Arguments,-Environment) -  creates a new environment
% in which the variables in the 1st arg are bound to the values in the
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
% returns the frame in which the variable was bound.
value(S,V,Env,Env) :- bound(S,V,Env).
value(S,V,E1/E2,E) :-
  not(bound(S,V,E1/E2)),
  value(S,V,E2,E).

% change the value associated with symbol S to V, returning the old value.
set(S,V,Env,OldV) :-
  value(S,OldV,Env,BindingEnv),
  !,
  retract(bound(S,OldV,BindingEnv)),
  assert(bound(S,V,BindingEnv)).

set(S,_,E,_) :-  err('symbol not bound in environment:',(S/E)).

% add an initial binding for symbol S to value V in environment Env.
define(S,V,Env) :-
  when(retract(bound(S,_,Env)), 
       warn('symbol already defined in environment: ',(S,Env))),
  assert(bound(S,V,Env)).

% load(F) reads and evals all expressions in file F.
load(File) :-
  see(File),
  repeat,
  read(X),
  loadProcess(X),
  seen,
  !.

loadProcess(end_of_file).
loadProcess(X) :- eval(X),fail.

%%% misc. utilities ...

err(Msg) :- warn(Msg),!,fail.
err(Msg1,Msg2) :- warn(Msg1,Msg2),!,fail.

warn(Msg) :- writeln(Msg).
warn(Msg1,Msg2) :-writeln(Msg1),write(' '),write(Msg2).

% once(X) executes X only once.
once(X) :- X,!.

writeln(X) :- nl,write(X).

when(Test,Then) :- Test->Then;true.

:- endif.

/*
append([],X,X).
append([U|L],X,[U|M]) :- append(L,X,M).

reverse([],[]).
reverse([Head|Tail],L) :- 
  reverse(Tail,TailReversed),
  append(TailReversed,[Head],L).
*/

%% [tim.prolog]SIPCORE, 
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:40:00 1986
%% this file specifies the initial environment for SIP.

% PRIMITIVE "MACROS"

(X==Y) ==> define(X,Y).
`(X) ==> quote(X).
consStream(X,Y) ==> cons(X,delay(Y)).
head(X) ==> car(X).
tail(X) ==> force(cdr(X)).
theEmptyStream ==> nil.
'emptyStream?' ==> 'null?'.

%% PRIMITIVE VARIABLES.
true == quote(true).
false == quote(false).
nil == quote([]).

%% PRIMITIVE FUNCTIONS
car == `pf(car).
cdr == `pf(cdr).
cons == `pf(cons).
'eq?' == `pf(=).
'=' == `pf('=').
('+') == `pf('+').
('-') == `pf('-').
'*' == `pf('*').
'/' == `pf('/').
begin == `pf(begin).
force == `pf(force).
load == `pf(load).
'==>' == `pf('==>').
prolog == `pf(prolog).
print == `pf(print).

callCC == `pf(callCC).

null == lambda([x], 'eq?'(x,nil)).

/*
Posted to comp.lang.prolog

Date: Sun, 5 Apr 87 01:42:37 EST
From: Tim Finin <t...@linc.cis.upenn.edu>
Subject: Scheme

A while back there was a discussion on the SCHEME newsgroup concerning
implementations of logic programming languages in Scheme.  David Moon
wondered if anyone had implemented Prolog in Scheme.  

What I found most interesting about the exercise has more to do with
Prolog than with Scheme - It is very difficult to implement an
efficient interpreter for a language which has side-effects in Prolog.

I could not find a way to represent environments which had what I
consider to be the neccessary features:

1 - unreferenced enviroments should be automatically GCed.

2 - looking up the value of a variable should be cheap and,
    in particular, should not depend on the the number of values it
    has received in the past.

3 - variable assignment should be cheap and, in particular should not
            require copying abritrary portions of an environment.

4 - The interpreter should not require an infinite stack nor
    should the host prolog be required to detect and optimize
    for tail recursion.

I basically considered two alternatives for representing the
environment:

  o represent an environment as a term which contains a sequence of
    variable-name/variable-value pairs.  This achieves (1) in most
    prologs
    but must give up on either (2) or (3).

  o represent an environment as a set of assertions in the clausal
    database of the form: bound(Symbol,Value,EnvironmentID).  This
    wins on
    (2) and (3) but loses on (1).

This makes me think that a side-effect predicate like RPLACARG
(discussed in Prolog-Digest about a year ago) is not such a bad idea.
It also reinforces the notion that Lisp is either a (i) more general
or (ii) lower level language than Prolog, depending, of course, on
your point of view.

-- Tim
*/
%% [tim.prolog]siptest.
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:39:27 1986
%% this file contains samples for SIP.

fact == lambda([n], if(=(n,0),1,n*fact(sub1(n)))).

add1 == lambda([n], n+1).

sub1 == lambda([n], n-1).

% higher order functions

mapcar == 
  lambda([f,l],
         if(null(l),
            nil,
            cons(f(car(l)),mapcar(f,cdr(l))))).

% simple list manipulation functions.

length == lambda([l], if(null(l),0,add1(length(cdr(l))))).

append == lambda([l1,l2],if(null(l1),l2,cons(car(l1),append(cdr(l1),l2)))).

reverse == 
  lambda([l], 
     if(null(l),
        l,
        append(reverse(cdr(l)),(cons(car(l),nil))))).

% stuff for streams.

filter == 
  lambda([f,s], 
         if('emptyStream?'(s),
            s,
            if(f(head(s)),
               consStream(head(s),filter(f,tail(s))),
               filter(f,tail(s))))).

from == lambda([n],consStream(n,from(n+1))).

nthStream == lambda([s,n],if(n=1,head(s),nthStream(tail(s),n-1))).

integers == from(1).

% environments

makeCounter ==
  lambda([],
         begin(counter == 0,
               lambda([],set(counter,1+counter)))).

      
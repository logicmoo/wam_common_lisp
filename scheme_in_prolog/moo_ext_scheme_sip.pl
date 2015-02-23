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
          "sipcore") and then enter a read-eval-print loop.  You can re-start the
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

          The character '`'  acts like '`'  in normal Scheme in that it quotes the
          next expression in the input.

          evaling prolog('`' abort) is the (dirty) way to exit the read-eval-print
          loop.  In fact, evaling prolog('`' abort) is the only way to exit the
          read-eval-print loop.

          Just to make my job easier, I added a macro like facility.  Evaling S1
          ==> S2 defines a rewriting type macro that transforms S1 into S2.  For
          example, we could (and do) do:

                          tail(X) ==> force(cdr(X)).

*/
:-swi_module(moo_ext_scheme_sip,[proccessInSip/1,eval/1]).
:-ensure_loaded(moo_ext_lisp_triska).

%% SIPC - Scheme In Prolog with Continuation Semantics

%% a simple implementation of Scheme in Prolog (w. continuation semantics)
%% Tim Finin, University of Pennsylvania, Mon Oct  6 10:11:18 1986

%% note: in order to handle side effects in a reasonable way,
%% environments are stored in the database.  The result is that old
%% environments never die.  They don't even fade away!

% operators

:- set_prolog_flag(backquoted_string,false).
:- dynamic(((( ==> ))/2)).
:- op( 900,xfy, (==>)).		% used for macros.
:- op(1100,xfx, (==)).	
:- op(200,fx,'`').		% '`'(x) = quote(x).

:- thread_local(bound/3).


% sip -  creates a virgin initial environment and enters a 
% read-eval-print loop for SIP.  Exit by typing "prolog('`' abort)."

:- meta_predicate moo_ext_scheme_sip:sip_when(0,0).
% :- meta_predicate moo_ext_scheme_sip:goal_truth(0,*,*,*).
:- meta_predicate moo_ext_scheme_sip:applyPrim(*,*,0).

sip :- create0Env, sipREP.
ssip :- create0Env, ssipREP.

%  sipREP -  enters a read-eval-print loop for SIP.
sipREP :-
  repeat,
  writeln('SIP> '),
  read(E),
  once(eval3(print(E),0,k(nil))),
  fail.

%  sipREP -  enters a read-eval-print loop for SIP.
ssipREP :-
  repeat,
  writeln('SSIP> '),
  readCycL(E),
  once(eval3(print(E),0,k(nil))),
  fail.

% create0Env sets up E as the inital Scheme environment.
create0Env :-
  % flush all old environments.
  retractall(bound(_,_,_)),
  % (re)set the environment counter.
  %abolish(envNum,1),
  %assert(envNum(1)),
  flag(envNum,_,1).
  % define the initial variables.
  % load(sipcore).

% abbreviations for eval/3.
eval(E) :- eval3(E,0,k(nil)),!.
eval(E,_V) :- eval3(E,0,k(nil)),!.
eval(E,Env,C) :- eval3(E,Env,C),!.

sip_when(Test,Then) :- Test->Then;true.

% like eval3, but "returns" its value to caller.
evalReturn(Exp,Env,Val) :- eval3(Exp,Env,k(return(Val))).


% eval3(expression,environment,continuation)

eval3(X,_,C) :- number(X),applyC(C,X).

eval3(X,E,C) :- atomic(X),!,lookUp(X,V,E),applyC(C,V).

% syntax kludge - call a fn F of no arguments as [F].
% bug - doen't check for special forms!!!
eval3([F|Args],_E,C) :- !,eval3(F,Env,k(evalArgs(Args,[]),Env,C)).

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

applyC(k(if(_Then,Else),E,C),false) :- !,eval3(Else,E,C).
applyC(k(if(Then,_Else),E,C),_) :- eval3(Then,E,C).

applyC(k(set(Sym),E,C),V) :- set(Sym,V,E,OldV),applyC(C,OldV).

applyC(k(define(Sym),E,C),V) :- define(Sym,V,E),applyC(C,Sym).

applyC(k(and(X),E,C),V) :- V=false->applyC(C,false);eval3(X,E,C).

applyC(k(or(X),E,C),V) :- V=false->eval3(X,E,C);applyC(C,V).

applyC(k(eval,E,C),V) :- eval3(V,E,C).

applyC(k(evalArgs([In1|InRest],Out),E,C),V) :-
  eval3(In1,E,k(evalArgs(InRest,[V|Out]),E,C)).

applyC(k(evalArgs([],RCall),_E,C),V) :-
  reverse([V|RCall],Call),
  Call = [F|Args],
  apply(F,Args,C).

% we're done
applyC(k(return(V)),V) :- !.
applyC(k(nil),_V) :- !.

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
applyPrim(car,[[H|_]],H).
applyPrim(cdr,[[_|T]],T).
applyPrim(begin,[_A1,A2],A2).
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
  sip_when(retract(bound(S,_,Env)), 
       warn('symbol already defined in environment: ',(S,Env))),
  assert(bound(S,V,Env)).

% load(F) reads and evals all expressions in file F.
load(File):- see(File),
 repeat,
  call_cleanup((read(X), ((X = end_if_file ) -> true;((once(loadProcess(X)),fail)))),seen),!.

:-thread_local(schemePExpansion/0).
loadProcess(end_of_file):- ignore(retract(schemePExpansion)),!.
loadProcess(X) :- eval(X).

%%% misc. utilities ...

err(Msg) :- warn(Msg),!,fail.
err(Msg1,Msg2) :- warn(Msg1,Msg2),!,fail.

warn(Msg) :- writeln(Msg).
warn(Msg1,Msg2) :-writeln(Msg1),write(' '),write(Msg2).

proccessInSip(M:H):-atom(M),!,proccessInSip(H).
proccessInSip(H):-functor(H,==,2).
proccessInSip(H):-functor(H,==>,2).

term_exp_process((H:-B),S):-B==true,!, term_exp_process(H,S).
term_exp_process(H,H):- proccessInSip(H),!,ignore(loadProcess(H)).

%% [tim.prolog]SIPCORE, 
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:40:00 1986
%% this file specifies the initial environment for SIP.

user:term_expansion( T ,evaluated_term(S)):- schemePExpansion,term_exp_process(T,S).
user:goal_expansion( T ,eval(T)):- proccessInSip(T),!.

:-asserta(schemePExpansion).
% PRIMITIVE "MACROS"

(X==Y) ==> define(X,Y).
'`'(X) ==> quote(X).
consStream(X,Y) ==> cons(X,delay(Y)).
head(X) ==> car(X).
tail(X) ==> force(cdr(X)).
theEmptyStream ==> nil.
'emptyStream?' ==> 'null?'.

%% PRIMITIVE VARIABLES.
:-eval((true == quote(true))).
false == quote(false).
nil == quote([]).

:-debug.

%% PRIMITIVE FUNCTIONS

:-eval((car == '`'(pf(car)))).
:-eval((cdr == '`'(pf(cdr)))).

cons == '`' pf(cons).
'eq?' == '`' pf(=).
'=' == '`' pf('=').
('+') == '`' pf('+').
('-') == '`' pf('-').
'*' == '`' pf('*').
'/' == '`' pf('/').
begin == '`' pf(begin).
force == '`' pf(force).
load == '`' pf(load).
==> == '`' pf(==>).

prolog == '`' pf(prolog). 
print == '`' pf(print). 

callCC == '`' pf(callCC).

null == lambda([x], 'eq?'(x,nil)).


end_of_file.  % <-- why mankind will always dominate machines? 

% once(X) executes X only once.
once(X) :- X,!.

writeln(X) :- nl,write(X).

append([],X,X).
append([U|L],X,[U|M]) :- append(L,X,M).

reverse([],[]).
reverse([Head|Tail],L) :- 
  reverse(Tail,TailReversed),
  append(TailReversed,[Head],L).


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


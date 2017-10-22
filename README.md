Common Lisp in Prolog 
=================

This library provides an ad-hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp.

 API to Common Lisp

https://github.com/TeamSPoon/wam_common_lisp

![](t/Common Lisp.png)

## Useful to Me?

Run `?- pack_install(wam_common_lisp)`.


/*
 
## NOTES:
 
* WAM-CL currently produces code 6 times slower than the handwritten code
 
* Handwritten Prolog is 2-3 slower than SBCL
 
* If WAM-CL becomes fast as handwritten code,
** it will be 17 times faster than CLISP
** it will be 6 times faster than ECL
 
 
 
```` 

(declaim (optimize (speed 3) (debug 0) (safety 0)))(defun fib (n) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2))))))
 (time (fib 38))


(declaim (optimize (speed 0) (debug 3) (safety 3)))(defun fib (n)(declare (optimize  (safety 3) (debug 0)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))

(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 3) (debug 0) (safety 0)))(defun fib (n)(declare (optimize speed (safety 0) (debug 0))) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))



(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 2) (debug 2) (safety 2)))(defun fib (n)(declare (optimize speed (safety 2) (debug 2)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))

(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 2) (debug 2) (safety 2)))(defun fib (n)(declare (optimize speed (safety 2) (debug 2)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 40))



````
 
*/


````
% WAM-CL 
fibc(A, K) :- !,
        B=[[bv(n, [A|_])]],
        sym_arg_val_envc(n, A, C, B),
        >(C, 1, D),
        (   D\=[]
        ->  sym_arg_val_envc(n, A, E, B),
            -(E, 1, F),
            fibc(F, I),
            sym_arg_val_envc(n, A, G, B),
            -(G, 2, H),
            fibc(H, J),
            +(I, J, L),
            K=L
        ;   K=1
        ).
fibc(_, _) :- '<<=='(fibc(n),if(n>1, fibc(n-1)+fibc(n-2), 1)).
````

````
% HANDWRITTEN
fibp2(N, F) :-
        N =< 1 
        -> F = 1 
        ;
        N1 is N-1,
        N2 is N-2,
        fibp2(N1, F1),
        fibp2(N2, F2),
        F is F1+F2.
````



````
% SBCL 1.3.1
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)
````

````
% YAP-Prolog (Hand written)
% ?- time(fibp2(38,O)).
% 3.124 CPU in 3.148 seconds ( 99% CPU)
````

````
% YAP-Lisp (WAM-CL)
% ?- time(fibc(38,O)).
% 20.184 CPU in 20.340 seconds ( 99% CPU)
````

````
% SWI-Prolog (Hand written)
% ?- timel(fibp2(38,O)).
% 24.558 CPU in 24.826 seconds (99% CPU, 18027611 Lips)
````

````
% ECL 15.3.7
% > (time (fib 38))
% run time  : 25.516 secs (real time : 26.290 secs)
````

````
% CLISP 2.49
% (time (fib 38))
% 53.0 sec.
````

````
% SWI-Lisp (WAM-CL)
% ?- time(fibc(38,O)).
% 113.043 CPU in 114.324 seconds (99% CPU, 15665558 Lips)
````

## Usage

## Copyright and License

Copyright (c) 2017, [Douglas Miles](https://twitter.com/logicmoo)

This project is licensed under the [MIT License](LICENSE.md).


# Releasing Common Lisp-Prolog

There're no hard rules about when to release wam_common_lisp. Release bug fixes frequently, features not so frequently and breaking API changes rarely.

### Release

Run tests, check that all tests succeed locally.

```prolog
?- run_tests(wam_common_lisp).

```

Increment the version, modify [pack.pl](pack.pl).

*  Increment the third number if the release has bug fixes and/or very minor features, only (eg. change `0.0.1` to `0.0.2`).
*  Increment the second number if the release contains major features or breaking API changes (eg. change `0.0.1` to `0.2.0`).


```
### 0.0.2 (2/10/2017)
```

Remove the line with "Your contribution here.", since there will be no more contributions to this release.

Remove the "Stable Release" section in README that warns users that they are reading the documentation for an unreleased version.

Commit your changes.

```
git add README.md CHANGELOG.md pack.pl
git commit -m "Preparing for release, 0.0.2."
git push origin master
```

Release.

```
$ @TODO
 
Tagged v0.0.2.
Pushed git commits and tags.
Pushed wam_common_lisp 0.0.2 to swi-prolog.org.
```

### Prepare for the Next Version

Add the next release to [CHANGELOG.md](CHANGELOG.md).

```
Next Release
============

* Your contribution here.
```

Increment the third version number in [pack.pl](pack.pl).

Commit your changes.

```
git add CHANGELOG.md pack.pl
git commit -m "Preparing for next development iteration, 0.0.2."
git push origin master
```

# Some TODOs

Document this pack!
Write tests
Untangle the 'pack' install deps
Still in progress (Moving predicates over here from logicmoo_base)


[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and TeamSPoon
All rights reserved.

# Dislike having tons of forks that are several commits behind the main git repo?

(Why feel obligated to maintain a git fork just to contribute ?)

Please ask to be added to TeamSPoon !

````
% =====================================================
% File: sanity_tests.pl
% =====================================================

:- use_module(library(multivar)).
:- use_module(library(pfc)).

isa(i1,c1).
predicate_function_canonical(isa,instanceOf).

% weaken_goals/2 that converts arguments (from legacy code)
% into metaterms which allow logical constraints to be placed upon unification
% in the case of atoms, they are "weakened" to non ground terms
predicate_hold_aliases(Spec),{mpred_functor(Spec,F,A),functor(P,F,A)} 
  ==> (  P, { weaken_goal(P,Q) } ==> {ignore(call(retract,P))},Q ).
       
predicate_hold_aliases(loves/2).

% the predicate is weakened on read (all args)
loves(sue,joe).
loves(joe,fred).

/*
?- loves(X,joe).
X = _{ '$value'= X, iz = sue}.
*/

% so that one may use "typed unification"
tFemale(sue).
~tFemale(joe).

/*
?-  use_module(library(attvar_reader)).  % allows attvars to be read from files and console

?- loves( X{iza=tFemale},joe).
X = _{ '$value'= X, iz = sue, iza=[tFemale]}.
Yes.

?- loves( sue, Y{iza=tFemale}).
Y = _{ '$value'= X, iz = fred}.
Yes.

% this was Joe was asserted to specifically not to be a tFemale.
% However the gender of Fred is still unknown

*/

:- if(false).

%  @TODO  Move this to a different set of exmaples
% this gets hairy to the instances can belong to several intensional types, extensional collections and datatypes.
:- ensure_loaded(library('logicmoo/pfc/user_transitiveViaArg.pfc')).
% both arguments must have at least some type attributes in common
meta_argtypes(loves(X,X)).  % 


:- endif.

% =====================================================
end_of_file.
% =====================================================



?- p(X,X) = p(i1,instanceOf(c1)).

X = _{ '$value'= X, iz = c1, iza=[c1]}.
Yes

?- use_module(clause_attvars).

?- p(X,X) = p(i1,instanceOf(c1)), asserta(x_was(X)).

?- 
````



My task is to create a Prolog  version of a Common Lisp Interpretor.   
Intially starting out with Lisp500.  I haven't checked yet if Lisp 500 will be good enough to run most Lisp programs it's not quite common Lisp obviously but at least it's accepted widely enough that it even CL-BENCH references it.
If there's a better common Lisp out there for us and we get there faster than I'll take you up on the offer to use that Lisp and said this Lisp for example one might find armed they are common Lisp.   
The reason for choosing Lisp 500 is it would require you to only implement 158 initial trampolines from there core500.lisp  is loaded on top of your Prolog system initial Lisp500.pl.  
Presently lisp500.pl has a C functions including comments as you can see it's manipulating memory arrays and a completely idiomatic port of the 158  C  functions would probably not be ideal but I'm allowing this to be possible if it allows you to get the job completed faster.    See,  If I was doing this work myself,  I would probably begin to work on the C idiomatically but then within a short amount of time realize that this approach is creating more work compared to implementing the exact function that the core Lisp file requires CAR/APPLY etc.  But I don't feel comfortable going straight to the core list functionality until I've at least had a crack at idiomatic C.  I leave this up to your discretion.
Regardless how this first part was done you would get to the point where you are loading the core500.lisp.  You now have a toy Lisp interpreter.    You know that the bounty is complete when you can run the either the package or unpackaged version of knowledge machine in your Lisp implementation.      If this target was a little too far off then I would first go for passing of the ANSI Common Lisp Test Suite.  http://common-lisp.net/project/ansi-test/ or as much as possible.   After a reasonable number of ANSI tests can be passed I would then start using smaller list programs which can be added to the test directory the finalists program that I like to feel the run is called the knowledge machine and that's in the test directory right now and in there is a script file that runs the tests for the seat the KM system.




NOTES:
CL ANSI tests I suppose at this very moment I should be testing the C version in seeing how far it gets in the ANSI tests to get his myself a baseline to find out if Lisp 500s actually really worth it.  
I would like to also find out if there is a way of eventually getting the CLOS out of Lisp 500.  Some people suggested using closette for this particular work order getting classes in necessary but it is in the plan and if it seems like this is not an happen starting with with 500 but starting with the different lisp  I would try to figure this as soon as possible.




FUTHER THOUHTS:

Some people say it's absolutely absurd to try to implement common Lisp inside a Prolog because of the final result would be to inefficient run to slow use it too much memory etc . A week ago when the JavaScript version of miniKanren ran faster than the Scheme version because in their particular use case the user types in code and expected it to run as quickly as possible in which it did because it skipped over some compilation step.  All the JavaScript version did was create a list of uncompiled closures.  Our analogy here is that Lisp macros can almost always be compiled down into a set of Lisp functions after they been expanded properly.  However we also know that they can be interpreted and that the Lisp interpreter does not really have to compile these macros and said do term replacement.  While this is prolog chief capability that is to put terms inside of terms during unification you may ask is really Lisp just macro after macro and would we really get a speed of benefit by doing this.  This argument may be flawed but it was just a thought.

My usescase .. A propositional resolution program such as CYC, SNARK or KM (knowledge machine) is an exercise into how quickly it can select properly with correct piece of code to invoke this or that .   I believe does not lend itself that greatly to compilation.   This thread will  probably quickly have very knowledgeable people helping me out by telling me why this is not a great idea.  Truthfully I rather doubt that my hypothesis is true that it would run these programs faster but I do believe they will not run is slow and some programming communities would predict.    That said the task is not to make it run fast for me but just to work at all would be wonderful.  




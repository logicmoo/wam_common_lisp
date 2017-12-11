Common Lisp in Prolog 
=================

[![Build Status](https://travis-ci.org/rla/simple-template.svg)](https://travis-ci.org/rla/simple-template)

This library is designed to *not* be just another an ad-hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp.


https://github.com/TeamSPoon/wam_common_lisp

![](t/Common Lisp.png)


## Useful to Me?

* Translates Lisp source files into Prolog source files.  ( compilation is done by Host prolog on the Translated source (from either disk or memory)) 

* At the REPL, forms are converted from Lisp to Prolog then call/1d 

* Being written as a SWI-Prolog "pack" 

* Picks up freebies .. whatever the host prolog system offers such as 
**Makes Plaform Executables and. DLL/So files 
**Garbage Collection 
**Memoization 
**Embedding (from C/C++/Python/C#/Mono/Java)  

* Gives MOP/CLOS to Prolog programmers 
 
* Goal is to ensure can run in YAP (which Lisp to Prolog benchmarking shows about 4x speedup over SWI)
Very importantly we need to ensure we can run well in
** Sicstus
** PrologCafe
** Yield-Prolog
** Jekejeke
** EcLiPSe Prolog
 
* Most simple functions optimize to how handwritten code might look.. *only* 2-3 slower than compiled SBCL
* comp.lang.lisp thread https://groups.google.com/forum/#!topic/comp.lang.lisp/0G77ebK3DIw
* comp.lang.prolog thread https://groups.google.com/forum/#!topic/comp.lang.prolog/85jyECdWTxc
* other README.MD https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl
* HOWTO Ats bottem

## Goals and TODOs
* Document this pack!
* Write tests
* Untangle the 'pack' install deps
* Still in progress (Moving predicates over here from logicmoo_base)
* Keep later `copy_term/2's` cheap, 
* Experment with way to passes entire term object object references as atoms  (nb_current/2 allows access to the object's property map)
* Ensure passes most all CL-ANSI tests 
** Hardest part is making sure it throws/complains about all the things it needs to
* Using SWICLI as FFI (SWICLI itself still needs work) 
* Ensure works with ASDF-INSTALL
* Quicklisp 


## Usaage output of --help)
````
WAM-CL (https://github.com/TeamSPoon/wam_common_lisp) is an ANSI Common Lisp implementation.

Usage:  $wamcl [prolog-options] [wamcl-options] [lispfile [argument ...]]

Host Prolog options:

-x state         Start from Image state (must be first)
                 (may be used to debug saved lisp EXEs)

-[LGT]size[KMG]  Specify {Local,Global,Trail} limits
[+/-]tty         Allow tty control
-O               Optimised compilation
--nosignals      Do not modify any signal handling
--nodebug        Omit generation of debug info
--version        Print the Prolog version information

WAM-CL Options:

 -?, --help    - print this help and exit

Lisp Startup actions:
 --ansi        - more ANSI CL compliance  (TODO)
 -p package    - start in the package
 -norc         - do not load the user ~/.wamclrc file   (TODO)
 -lp dir       - add dir to *LOAD-PATHS* (can be repeated)    (TODO)
 -i file       - load initfile (can be repeated)

Compiler actions put WAM-CL into a batch mode:
 -x expressions - execute the expressions (mixed into compiler actions)
 -c [-l] lispfile [-o outputfile] - compile or load a lispfile
               [--exe outputfile] - make a platform binary

Which are overridden by:

  --repl                Enter the interactive read-eval-print loop when done
  --load <filename>     File to load
  --eval <form>         Form to eval

Default action is an interactive read-eval-print loop.

  --quit, -norepl       Exit with status code (instead) from prevous option processing.
                        Otherwise, an interactive read-eval-print loop is entered.

   "lispfile"           When "lispfile" is given, it is loaded (via --load)

Remaining arguments are placed in EXT:*ARGS* as strings.


Examples:

$PACKDIR/wam_common_lisp/prolog/wam_cl$

# creating your first image
$ swipl ../wamcl.pl --exe wamcl
# try it
$ ./wamcl
$ ./wamcl -c hello.lisp -o hello.pl --exe hello
$ ./hello world
$ swipl -x hello --repl
$ swipl hello.pl
$ swipl -x wamcl.prc
% swipl -x wamcl.prc hello.lisp world


Extended Info:

handle_program_args('--help', -?) :-
        listing(handle_program_args),
        show_help,
        set_interactive(false).
handle_program_args('--debug', '-debug') :-
        cl_push_new(xx_features_xx, kw_debugger).
handle_program_args('--quit', '-norepl') :-
        set_interactive(false).
handle_program_args('--repl', '-repl') :-
        set_interactive(true).
handle_program_args('--ansi', '-ansi') :-
        cl_push_new(xx_features_xx, kw_ansi).
handle_program_args('--package', '-p', A) :-
        cl_inpackage(A).
handle_program_args('--exe', '-o', A) :-
        qsave_program(A),
        set_interactive(false).
handle_program_args('--compile', '-c', A) :-
        cl_compile_file(A, [], _),
        set_interactive(false).
handle_program_args('--l', '-l', A) :-
        cl_load(A, [], _),
        set_interactive(false).
handle_program_args('--load', '-i', A) :-
        set_interactive(true),
        cl_load(A, [], _).
handle_program_args('--eval', '-x', A) :-
        set_interactive(true),
        lisp_compiled_eval(A, _).


````



## WHY ?!?!?!
==============


### Myth busting


* Is it really super easy to implement _anything_ on Prolog?  Some junior Prolog programmers would be surprised by Prolog doing any OO let alone MOP.  After all, Prolog is very very simple when it comes to its types. 

* If it can be done, in the end, will it look as ugly as trying to implement and maintain a CommonLisp in a programing language like LOGO?  Everyone who graduates with a CS degree was tasked with several disarming hour just trying to do something as simple as adding up a list of numbers in Prolog. In moments of horror they think how simple it would have been it has it been any other language than Prolog.  Most come away with the misunderstanding that Prolog is only capable of certain pure tasks. And too awkward for everything else.  Much like how LOGO is the best language for mornings you've woken desperately needing to draw a box inside a circle.  Not so much for those mornings, you need to implement an HTTP client. 

* Other myths "prolog doesn't scale".. least will be busted that whenever a lisp program (that scales according to whatever "scale" means) is running on a lisp-in-prolog (like WAM-CL) 

* Some people say it's absolutely absurd to try to implement common Lisp inside a Prolog because of the final result would be to inefficient run to slow use it too much memory etc .

### Practical reasons: 

* Several decades of Common Lisp development libraries can, within a matter of hours, be translated to useable Prolog development libraries. 

* Also, DAYDREAMER, Knowledge Machine, SWALE, and CYC might perform differently and be more practical at non-toy domains. 


### State of affairs
* One small code example so far runs about ¼ the speed of SBCL 

````
CL-USER>  (defun fib (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))
% :- lisp_compiled_eval(
%                     [ defun,
%                       u_fib,
%                       [n],
%                       [if, [<=, n, 1], 1, [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]]]
%                     ]).
% COMPILER
/*
alphas=[n].
type=ctx.
var_tracker(n)=rw{name:n, p:1, r:3, ret:0, u:0, vars:[N_Param, N_Get, N_Get29, N_Get36], w:1}.
 */
% inlined(-(N_Param, 1, C45_Ret)) :-
%       C45_Ret is N_Param-1.
% inlined(-(N_Param, 2, C45_Ret38)) :-
%       C45_Ret38 is N_Param-2.
% inlined(+(Fib_Ret, Fib_Ret39, C43_Ret)) :-
%       C43_Ret is Fib_Ret+Fib_Ret39.
% 345,783 inferences, 0.074 CPU in 0.077 seconds (97% CPU, 4661956 Lips)

% asserting... u
wl:arglist_info(f_u_fib, [n], [N_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[n], opt:0, req:1, rest:0}).

% asserting... u
wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [<=, n, 1], 1, [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]]]]).

% asserting... u
f_u_fib(N_Param, _rPrevRes) :-
        (   N_Param=<1
        ->  _rPrevRes=1
        ;   N_Param is N_Param-1,
            f_u_fib(N_Param, Fib_Ret39),
            N_Param is N_Param-2,
            f_u_fib(N_Param, Fib_Ret39),
            Fib_Ret39 is Fib_Ret39+Fib_Ret39,
            _rPrevRes=Fib_Ret39
        ).
:- set_opv(f_u_fib, classof, claz_compiled_function),
   set_opv(u_fib, compile_as, kw_function),
   set_opv(u_fib, function, f_u_fib).
% EXEC
% 409 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 1768029 Lips)
FIB
CL-USER>
````
is very close to
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
% * (defun fib (n) (if (<= n 1) 1 (the fixnum (+ (fib (- n 1)) (fib (- n 2))))))
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)
````

````
% YAP-Prolog 
% ?- time(fib(38,O)).
% 3.124 CPU in 3.148 seconds ( 99% CPU)
````

````
% SWI-Prolog
% ?- timel(fib(38,O)).
% 24.558 CPU in 24.826 seconds (99% CPU, 18027611 Lips)
````

````
% ECL 15.3.7
% > (time (fib 38))
% run time  : 25.516 secs (real time : 26.290 secs)
````


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



[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and TeamSPoon
All rights reserved.

# Dislike having tons of forks that are several commits behind the main git repo?

(Why feel obligated to maintain a git fork just to contribute ?)

Please ask to be added to TeamSPoon !


## FUTHER MISTHOUGHT:

I'll reply inline and correct some of the confusing misstatements I had made.
> > 
> >    I've only spent a week on it ...    I hope to recruit people that seem to know both Lisp and Prolog languages.
> > 
> >        The main purpose is this impl is it to run prolog-in-lisp 1000x  faster than the fastest lisps
prolog-in-lisp(s) are *not* 1000x slower than prolog-in-c but certainly not as fast (I apologize, I should have said 5-10x time slower).  The problem arises for Prolog programs like: English to CommonLogic converters (used in Natural Language Understanding), large-scale ontology checkers, KL-ONE language interpreters, and PDDL planners (Planning Domain Definition Language).  Such programs perform fine when written entirely in Lisp or Prolog (neither better or worse).  The problem is that they more often perform unacceptably poor when written in Prolog and then ran on a prolog-in-lisp interpreter.    

This leads to another class of programs 

> > and be at least in the top 3 impls
> >         for speed    Also the type of lisp programs I like to run (SWALE, DAYDREAMER) are buggy partial impl of Greenspun's rule as applied to Prolog (Instead of Lisp)

I should clarify, SWALE and DAYDREAMER are *not* buggy implementations of Prolog! they are their own things.  But there are certain routines they contain that make extensive use of unification and backtracking.  These routines  (for decades now) are examples where the data representations and processing their capabilities (well mostly domain sizes) have been scaled back due to virtually creating the same penalties of the "prolog-in-lisp" scenario.  This scenario is similar to taking an assembly language program that twiddles bitmasks and using bignum math to emulate the registers of the  Intel-4930k CPU. *You might just see some performance differences? We will be very lucky if 4x-10x was the only speed difference between running that same assembly code program directly on the processor or in our program.





```` 

(declaim (optimize (speed 3) (debug 0) (safety 0)))(defun fib (n) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2))))))
 (time (fib 38))


(declaim (optimize (speed 0) (debug 3) (safety 3)))(defun fib (n)(declare (optimize  (safety 3) (debug 0)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))

(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 3) (debug 0) (safety 0)))(defun fib (n)(declare (optimize speed (safety 0) (debug 0))) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))



(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 2) (debug 2) (safety 2)))(defun fib (n)(declare (optimize speed (safety 2) (debug 2)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))

(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 2) (debug 2) (safety 2)))(defun fib (n)(declare (optimize speed (safety 2) (debug 2)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 40))


````

## HOWTO/QUICKSTART
===================


## Installation 
````
$ swipl
Run `?- pack_install(wam_common_lisp)`.
````

### try it
````
$ swipl

?- use_module(library(wamcl)).
true.

?- lisp.

__        ___    __  __        ____ _
\ \      / / \  |  \/  |      / ___| |
 \ \ /\ / / _ \ | |\/| |_____| |   | |
  \ V  V / ___ \| |  | |_____| |___| |___
   \_/\_/_/   \_\_|  |_|      \____|_____|

Common Lisp, written in Prolog
CL-USER> (sqrt -1)
% :- lisp_compiled_eval([sqrt, -1]).
% COMPILER
% 32,324 inferences, 0.006 CPU in 0.006 seconds (97% CPU, 5643884 Lips)
:- cl_sqrt(-1, Sqrt_Ret).
% EXEC
% 22 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 824897 Lips)
#C(0 1)
````

### navigate disk

````
CL-USER> pwd.
% /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/

CL-USER> :cd t
% /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/

CL-USER> ls.
% baby2015/                           credit_tim_finin.sip.msg            MicroPrologII/                      test_cisp/
% credit_neil_smith.html              daydreamer/                         reference/                          travis.pl
% credit_neil_smith_prolog.html       hello.lisp                          test_1000/
% credit_tim_finin.html               km/                                 test_1500/
% ls.
T
CL-USER>
````

### Translate a file to prolog
Translate a file to prolog

CL-USER> (compile-file "hello.lisp")
<...snip...>
% 97,548 inferences, 0.023 CPU in 0.023 seconds (100% CPU, 4227371 Lips)
#P/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp
CL-USER>

### Open PceEmacs on a translated file
CL-USER> (edit (compile-file "hello.lisp"))
.... Opens PCEMACS ....

### Load a file
CL-USER> (prolog:consult (compile-file "hello.lisp"))

CL-USER> (load "hello.lisp" :compile t)   ;;; same as the above



### .... docvument
cd $PACKDIR/wam_common_lisp/prolog/wam_cl
$ swipl ../wamcl.pl --exe wamcl
$ ./wamcl -c hello.lisp -o hello.pl --exe hello
$ ./hello world
$ swipl -x hello --repl
$ swipl hello.pl
$ swipl -x wamcl.prc
% swipl -x wamcl.prc hello.lisp world

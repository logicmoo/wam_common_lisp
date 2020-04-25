## Common Lisp in Prolog Goals

````The resulting Translation of CommonLisp into Prolog who's non-special forms are treated special (operands are always passed unevaluated!) give us the simplest model: it has no types, no data, and no environments and yet the refection is powerful enough to make all resulting optimizations and program self-repairs trivial/obvious. (This Method works for Curry-lang, Haskell, ALL languages)````

Stop implementing an ad-hoc, informally-specified, bug-ridden, slow implementation of less than half of ISO-Prolog.

* !0th rule:  Any sufficiently complicated Lisp or Scheme program contains an ad-hoc, informally-specified, bug-ridden, slow implementation of half of ISO Prolog.
* Translating Lisp to Prolog gives Prolog
   * Metaobject Protocol
   * Common Lisp Object System    
   * Instant Prolog Ecosystem/Development Libraries (days, not years)
      * Several decades of Common Lisp libraries may be translated to useable Prolog development libraries.       
   * Maintain your code from original Lisp or translated Prolog (though wont translate back)
   * Settings to try to emulate handwritten code ([Examples](https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl/README.md))      
   * Forms (at REPL) are transpiled to Prolog, Compiled to WAM, Call/Executed.
   *  *only* 2-3 slower than SBCL  
   * Gives to prolog more than we can list!
* Simular to how CLISP is indespensable sometimes.
   * _a_ Common Lisp used for sanity testing
   * Makes debugging easy for Prolog and Lisp experts
* Picks up freebies .. whatever the host Prolog system offers such as 
  * Garbage Collection 
  * Memoization/Coinduction
  * Dynamic Extent
  * Exception Handling
  * Unwind-Protect/Cleanup
  * Native Locatives
  * Two-way calling and embedding from C/C++/Python/C#/Mono/Scala/Java/Haskell/LUA/Perl
  * Makes Plaform Executables and. DLL/So files  ([Quick Start](https://github.com/TeamSPoon/wam_common_lisp/blob/master/README.md#makeanexecutableandrunit)) *
  * (too enormous to go into)
* Developed/Installed as a SWI-Prolog pack 
  * [http://www.swi-prolog.org/pack/list?p=wam_common_lisp](http://www.swi-prolog.org/pack/list?p=wam_common_lisp) ``
## Incompleteness 
must fix for release worthiness
* Bugs Running/Translating:
  * Fully working LOOP (must-fix)
  * SWANK (must-fix)
  * PAIP Book code (bug in-progress)
  * [DAYDREAMER](https://github.com/eriktmueller/daydreamer) (in-progress)
  * [KNOWLEDGE MACHINE](http://www.cs.utexas.edu/users/mfkb/RKF/km.html)
  * Quicklisp (bug must-fix)
  * ASDF-INSTALL (bug must-fix)
* Add missing impls
  * delete-package (must-fix)
  * (more to be Listed) (not here)
* Tests ([in-progress](https://github.com/TeamSPoon/wam_common_lisp/tree/master/t))
  * Must pass 70% or above CL-ANSI tests (bug in-progress)
  * Ensure passes _all_ CL-ANSI tests (with --ansi) (feature always in-prgress)
  * Hardest part is making sure it throws/complains about all the things it needs to
  * need more tests!
* FFI (bug in-progress)
  * Use https://github.com/JanWielemaker/ffi ?
  * Using SWICLI as FFI (SWICLI's FFI itself still needs work but works for YAP as well)
  
## TODO _Features_
* Document prolog source-code this pack! (indeed, a feature!)
* Keep later `copy_term/2's` cheap, (feature in-progress)
  * Experment with way to passes entire term object object references as atoms  (nb_current/2 allows access to the object's property map)
* [(FAKE TODO![Build Status](https://travis-ci.org/rla/simple-template.svg)](https://rlaanemets.com/post/show/adding-travis-to-swi-prolog-packs))
* Untangle the `pack` install deps
  * Moving predicates to logicmoo_utils from logicmoo_base (Still in progress)
* DEpackifed version for Portability?
  * YAP-Prolog (in-progress) (which Lisp to Prolog benchmarking shows about 5x speedup)
  * TODO: Sicstus, B-Prolog, Bin-Prolog, EcLiPSe Prolog and Jekejeke
  * Low-Priority: PrologCafe, Yield-Prolog


## Installation 
````
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 7.7.4-36-gc02793b-DIRTY-BIRDY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

`?- pack_install(wam_common_lisp)`.

````

### Try it
````

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
## Start an inferior prolog
````
CL-USER> prolog.
% Break level 1
[1]  ?- writeln("press ctrl-d to return to lisp").
press ctrl-d to return to lisp
true.

[1]  ?- ^D
% Exit break level 1
% prolog.
T
CL-USER>
````
## Exit lisp
````
<press cntrl-d>
CL-USER> ^DEND-OF-FILE
true.
?-
````
 
## Start in REPL
````
$ swipl prolog/wamcl.pl

Common Lisp, written in Prolog
CL-USER>
````

### Compile a system Image and run it
````
$ swipl ../prolog/wamcl.pl --exe wamcl
$ ./wamcl
````

### Reattach to a system Image and debug
````
$ swipl -x wamcl --debug
````

### Run some lisp file
````
$ echo '(print (cons "hello" *ARGS*))' > hello.lisp
$ swipl -x wamcl hello.lisp world
````
outputs:
`(HELLO WORLD)`


### Translate a file to prolog
````
CL-USER> (compile-file "hello.lisp")
<...snip...>
% 97,548 inferences, 0.023 CPU in 0.023 seconds (100% CPU, 4227371 Lips)
#P/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp
CL-USER>
````

### Open PceEmacs on a translated file
````
CL-USER> (edit (compile-file "hello.lisp"))
.... Opens  hello.pl  ....
````

### Load a file
````
CL-USER> (prolog:consult (compile-file "hello.lisp"))

CL-USER> (load "hello.lisp" :compile t)   ;;; same as the above
`````

### Make an executable and run it
````
$ echo '(print (cons "hello" *ARGS*))' > hello.lisp
$ ./wamcl -c hello.lisp -o hello.pl --exe hello
$ ./hello world
```` 
outputs:
````
(HELLO WORLD)
````

### Attach to your image
````
$ swipl -x hello --repl
CL-USER> 
````

### Run your translated lisp
````
$ swipl hello.pl world
````
outputs:
````
(HELLO WORLD)
````


### See the source
````
$ cat hello.pl
````
````
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "hello.lisp" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
% Start time: Mon Dec 11 12:48:38 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user, [print, [cons, '$STRING'("hello"), '*ARGS*']]).
:- get_var(TLEnv, ext_xx_args_xx, Ext_xx_args_xx_Get),
   Print_Param=['$ARRAY'([*], claz_base_character, "hello")|Ext_xx_args_xx_Get],
   cl_print(Print_Param, _Ignored).


% Total time: 0.02 seconds
````

## Navigate disk
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




## Usaage output of --help)
````
'WAM-CL (https://github.com/TeamSPoon/wam_common_lisp) is an ANSI Common Lisp implementation.
Usage:  wamcl [prolog-options] [wamcl-options] [lispfile [argument ...]]

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
 -c [-l] lispfile [--main pkg::symbol] - compile or load a lispfile
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

$PACKDIR/wam_common_lisp/t$

# creating your first image
$ swipl ../prolog/wamcl.pl --exe wamcl
# try it
$ ./wamcl --repl
$ swipl ../prolog/wamcl.pl --compile hello.lisp --main sys::main --exe hello
$ ./hello world
$ swipl -x hello --repl
$ swipl hello.pl
$ swipl -x wamcl.prc
% swipl -x wamcl.prc hello.lisp world

````


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





```` 

(declaim (optimize (speed 3) (debug 0) (safety 0)))(defun fib (n) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2))))))
 (time (fib 38))


(declaim (optimize (speed 0) (debug 3) (safety 3)))(defun fib (n)(declare (optimize  (safety 3) (debug 0)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))

(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 3) (debug 0) (safety 0)))(defun fib (n)(declare (optimize speed (safety 0) (debug 0))) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))



(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 2) (debug 2) (safety 2)))(defun fib (n)(declare (optimize speed (safety 2) (debug 2)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 41))

(declaim (ftype (function (fixnum) fixnum) fib))(declaim (optimize (speed 2) (debug 2) (safety 2)))(defun fib (n)(declare (optimize speed (safety 2) (debug 2)) (fixnum n)) (if (<= (1- n) 0) n (the fixnum (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 40))


````


## Copyright and License

Copyright (c) 2017, [Douglas Miles](https://twitter.com/logicmoo)

This project is licensed under the [MIT License](LICENSE.md).

## WTF?!? (also known as FAQ)
* Was it easy to implement this in Prolog?

Yes! Still junior Prolog programmers would be surprised by Prolog doing any OO let alone MOP.  After all, Prolog is very very simple when it comes to its types. 

--------------------------------------------

* If it can be done, in the end, will it look as ugly as trying to implement and maintain a CommonLisp in a programing language like LOGO?

No! Everyone who graduates with a CS degree was tasked with several disarming hour just trying to do something as simple as adding up a list of numbers in Prolog. In moments of horror they think how simple it would have been it has it been any other language than Prolog.  Most come away with the misunderstanding that "Prolog is only capable of certain pure tasks. And too awkward for everything else."  Much like how LOGO is the best language for mornings you've woken desperately needing to draw a box inside a circle.  Not so much for those mornings, you need to implement an HTTP client. 

--------------------------------------------

* But prolog doesn't scale!

Myth will be busted that whenever a lisp program (that scales according to whatever "scale" means) is running on a lisp-in-prolog (like WAM-CL) 

--------------------------------------------

* It wont be fast, and even then it will never be faster than SBCL.

True when comparing WAM-CL to SBCL. But false about "slow".
Some people say it's absolutely absurd to try to implement common Lisp inside a Prolog because of the final result would be to inefficient run to slow use it too much memory etc .
Also, DAYDREAMER, Knowledge Machine, SWALE, and CYC might perform differently and be more practical at non-toy domains. 

--------------------------------------------

* Some things you said on eitehr of these thread [[comp.lang.lisp](https://groups.google.com/forum/#!topic/comp.lang.lisp/0G77ebK3DIw)] or [[comp.lang.prolog](https://groups.google.com/forum/#!topic/comp.lang.prolog/85jyECdWTxc)] are really misinformed!

Maybe

So I'll reply inline and correct some of the confusing misstatements I had made
> > 
> >    I've only spent a week on it ...    I hope to recruit people that seem to know both Lisp and Prolog languages.
> > 
> >        The main purpose is this impl is it to run prolog-in-lisp 1000x  faster than the fastest lisps
prolog-in-lisp(s) are *not* 1000x slower than prolog-in-c but certainly not as fast (I apologize, I should have said 5-10x time slower).  The problem arises for Prolog programs like: English to CommonLogic converters (used in Natural Language Understanding), large-scale ontology checkers, KL-ONE language interpreters, and PDDL planners (Planning Domain Definition Language).  Such programs perform fine when written entirely in Lisp or Prolog (neither better or worse).  The problem is that they more often perform unacceptably poor when written in Prolog and then ran on a prolog-in-lisp interpreter.    

This leads to another class of programs 

> > and be at least in the top 3 impls
> >         for speed    Also the type of lisp programs I like to run (SWALE, DAYDREAMER) are buggy partial impl of Greenspun's rule as applied to Prolog (Instead of Lisp)

I should clarify, SWALE and DAYDREAMER are *not* buggy implementations of Prolog! they are their own things.  But there are certain routines they contain that make extensive use of unification and backtracking.  The routines these programs use are examples where the domain had to be scaled back.  Only because they are under the penalties of the "prolog-in-lisp" scenario.  This scenario is similar to taking an assembly language program that twiddles bitmasks and using bignum math to emulate the registers of the  Intel-4930k CPU. You might just see some performance differences? Very lucky if it was only a 4x-10x slowdown   

> >  --------------------------------------------
--------------------------------------------






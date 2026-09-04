##  Common predicates used by external Logicmoo Utils
[![Build Status](https://travis-ci.org/logicmoo/logicmoo_utils.svg?branch=master)](https://travis-ci.org/logicmoo/logicmoo_utils) master
[![Build Status](https://travis-ci.org/logicmoo/logicmoo_utils.svg)](https://travis-ci.org/logicmoo/logicmoo_utils) default


## Installation using SWI-Prolog 8.1.x or later (due to duplicate transitive deps failing in earlier versions):

    `?- pack_install('https://github.com/logicmoo/logicmoo_utils.git'). `




## Control local side effects on each REDO 

Example usages: 

```prolog


 % Wrap `quietly/1` over parts of your code you no longer need to stepped thru.
 % This is a *nondeterministic* version of notrace/1 !  (Instead of like once/1)
 quietly(Goal):- 
    tracing ->
      each_call_cleanup(notrace,Goal,trace);
      Goal.
    
            
 % Scope *when* a prolog flag is set
 with_prolog_flag(Flag,Value,Goal):- 
    current_prolog_flag(Flag,Was),
     each_call_cleanup( 
	 set_prolog_flag(Flag,Value), 
	  Goal, 
	   set_prolog_flag(Flag,Was)). 
  

 % Trace non interactively *sometimes* 
 rtrace(Goal):- 
    ( tracing-> Undo=trace ; Undo = notrace ), 
    '$leash'(OldL, OldL), '$visible'(OldV, OldV), 
    each_call_cleanup( 
         (notrace,visible(+all),leash(-all),leash(+exception),trace), 
         Goal,
         (notrace,'$leash'(_, OldL),'$visible'(_, OldV),Undo)).


```


## Trace with your eyeballs instead of your fingers


```prolog

?- use_module(library(logicmoo_utils)).
true.

?- rtrace(member(X,[1,2,3])).
   Call: (9) lists:member(_8730, [1, 2, 3])
   Unify: (9) lists:member(_8730, [1, 2, 3])
   Exit: (9) lists:member(1, [1, 2, 3])
X = 1 ;
   Redo: (9) lists:member(_8730, [1, 2, 3])
   Exit: (9) lists:member(2, [1, 2, 3])
X = 2 ;
   Redo: (9) lists:member(_8730, [1, 2, 3])
   Exit: (9) lists:member(3, [1, 2, 3])
X = 3.

?-  rtrace(member(X,[1,2,3])),member(Y,[4,5]).
   Call: (10) lists:member(_10508, [1, 2, 3])
   Unify: (10) lists:member(_10508, [1, 2, 3])
   Exit: (10) lists:member(1, [1, 2, 3])
X = 1,
Y = 4 ;
X = 1,
Y = 5 ;
   Redo: (10) lists:member(_10508, [1, 2, 3])
   Exit: (10) lists:member(2, [1, 2, 3])
X = 2,
Y = 4 ;
X = 2,
Y = 5 ;
   Redo: (10) lists:member(_10508, [1, 2, 3])
   Exit: (10) lists:member(3, [1, 2, 3])
X = 3,
Y = 4 ;
X = 3,
Y = 5.

?- rtrace((member(X,[1,2,3]),member(Y,[4,5]))).
   Call: (10) lists:member(_11854, [1, 2, 3])
   Unify: (10) lists:member(_11854, [1, 2, 3])
   Exit: (10) lists:member(1, [1, 2, 3])
   Call: (10) lists:member(_11872, [4, 5])
   Unify: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(4, [4, 5])
X = 1,
Y = 4 ;
   Redo: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(5, [4, 5])
X = 1,
Y = 5 ;
   Redo: (10) lists:member(_11854, [1, 2, 3])
   Exit: (10) lists:member(2, [1, 2, 3])
   Call: (10) lists:member(_11872, [4, 5])
   Unify: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(4, [4, 5])
X = 2,
Y = 4 ;
   Redo: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(5, [4, 5])
X = 2,
Y = 5 ;
   Redo: (10) lists:member(_11854, [1, 2, 3])
   Exit: (10) lists:member(3, [1, 2, 3])
   Call: (10) lists:member(_11872, [4, 5])
   Unify: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(4, [4, 5])
X = 3,
Y = 4 ;
   Redo: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(5, [4, 5])
X = 3,
Y = 5.

```


This is a miniture part of a very large debugging library...
Some better parts of that library are not yet added. 

Here are current uses:

?- use_module(library(logicmoo_utils)).


Wrap `must/1` over parts of your code you do not trust yet.
If your code fails.. it will rewind to your entry block (at the scope of this declaration) and invoke rtrace/1 .
If there are 50 steps to your code, it will save you from pushing `creep` 50 times.  
Instead it turns off the leash to allow you to trace with your eyeballs instead of your fingers


```prolog

:- must(member(3,[4,5])).

..... TODO Doc this .....

```


```




Wrap `sanity/1` over parts of your code you want to turn on/off that is only usefull for slow debugging

```prolog

:- sanity(sleep(10)).

..... TODO Doc this .....

```




Wrap `nop/1` over parts of your code you do not want to quickly comment out yet not break syntax. 
 Defined as:

```prolog

:- meta_predicate nop(0).

nop(_).

..... TODO Doc this .....


```



## no_repeats
New ways to avoid duplicate solutions

```prolog

?- use_module(library(logicmoo_utils)).

?- no_repeats( X , member(X-Y,[3-2,1-4,1-5,2-1])).
% X = 3, Y = 2 ;
% X = 1, Y = 4 ;
% X = 2, Y = 1.


 ?- no_repeats(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.


```



## loop_check
New simple loop checking

Allows code to declare special locations that loop prevention will occur

```prolog
?- use_module(library(logicmoo_utils)).
true.

?-  TODO Doc this

```



## sub_clause_expansion

```prolog

:- use_module(library(file_scope)).

:- set_prolog_flag_until_eof(access_level, system).

:- assert_until_eof(( term_expansion(.,.) :- .. )).

```

## hook_hybrid
Hook and/or override assert, retract, call, clause, erase, etc for specific predicates

```prolog
?- use_module(library(hook_database)).

true.

?-  TODO Doc this

```


## clause_attvars
An alternate interface to the clause database to allow attributed variables to be asserted/read

```prolog
?- use_module(library(hook_database)).
true.

?-  TODO Doc this

```


## attvar_reader

```prolog
?- use_module(library(hook_database)).
true.

?-  TODO Doc this

```
 



# with_thread_local
Call a Goal with local assertions

   locally_each( :Effect, :Call) is nondet.
 
   Temporally have :Effect (see locally/2)
 
   But Ensure Non-determism is respected (effect is undone between _each_ Redo)
 
   uses each_call_cleanup/3 instead of setup_call_cleanup/3 (slightly slower?)
 
  for example,
 
   locally_each/2 works (Does not throw)
```prolog 
  ?- current_prolog_flag(xref,Was), 
      locally_each(set_prolog_flag(xref,true),
      assertion(current_prolog_flag(xref,true));assertion(current_prolog_flag(xref,true))),
      assertion(current_prolog_flag(xref,Was)),fail.
```
 
   locally/2 is little less carefull so it should _not_ work (it throws instead)
```prolog
?- current_prolog_flag(xref,Was), 
    locally(set_prolog_flag(xref,true),
    assertion(current_prolog_flag(xref,true));assertion(current_prolog_flag(xref,true))),
    assertion(current_prolog_flag(xref,Was)),fail.
```

   locally( :Effect, :Call) is nondet.
 
  Effect may be of type:
 
   set_prolog_flag -
      Temporarily change prolog flag
 
   op/3 - 
      change op
 
   $gvar=Value -
      set a global variable
 
   Temporally (thread_local) Assert some :Effect 
 
   use locally_each/3 if respecting Non-determism is important 
  (slightly slower?)
 
```prolog
  ?- current_prolog_flag(xref,Was), 
      locally(set_prolog_flag(xref,true),
      assertion(current_prolog_flag(xref,true))),
      assertion(current_prolog_flag(xref,Was)). 
```


:- set_prolog_flag_until_eof(access_level,system).

:- assert_until_eof(( term_expansion(.,.) :- .. )).

```


# Utilities to open various objects for read/write

```prolog
?- use_module(library(logicmoo_utils)).

```




# mass wildcard expansions
```prolog
?- use_module(library(logicmoo_utils)).

```




## Not _obligated_ to maintain a git fork just to contribute

( Please ask to be added to logicmoo and Contribute directly ! )

I really dislike having tons of forks that are several commits behind the main git repo.

Rather just give you commit access (feel free to work from a branch)


Still, we wont stop you from doing it the Fork+PullRequest method


## [BSD 2-Clause License](LICENSE.md)

Copyright (c) 1997 - 2018 
logicmoo and Douglas Miles <logicmoo@gmail.com> 



## Some TODOs

Document this pack!
```text

Write tests

./must_sanity.pl
./hook_database.pl
./file_scope.pl
./logicmoo_startup.pl
./logicmoo_common.pl
./no_repeats/no_repeats.pl
./loop_check/no_loops.pl

./scope_locally/with_no_x.pl
./scope_locally/with_thread_local.pl
./scope_locally/each_call_cleanup.pl

./expand_finer/subclause_expansion.pl

./file_utils/script_files.pl
./xlisting/xlisting_web/logicmoo_run_clio.pl
./xlisting/xlisting_web/mpred_rdf.pl
./xlisting/xlisting_web/logicmoo_run_pldoc.pl
./xlisting/xlisting_web/mpred_pldoc_util.pl
./xlisting/xlisting_web/xlisting_web.pl
./xlisting/xlisting_web/xlisting_web.pfc.pl
./xlisting/xlisting_web/swish_lib/render/html.pl
./xlisting/xlisting_web/xlisting_web_sanity_tests.pl
./xlisting/xlisting_web/logicmoo_run_swish.pl
./xlisting/listing_vars.pl
./xlisting/xlisting.pl
./debuggery/bugger.pl
./debuggery/block3.pl
./debuggery/frames.pl
./debuggery/dumpst.pl
./debuggery/rtrace.pl
./debuggery/unused_rtrace.pl
./debuggery/ucatch.pl
./debuggery/first.pl
./debuggery/util_supp.pl
./debuggery/dmsg.pl
./debuggery/sanity_tests.pl
./hybrid_db/attvar_serializer.pl
./hybrid_db/toplevel_variable_names.pl
./hybrid_db/call_from_module.pl
./hybrid_db/sanity_tests_cav.pl
./hybrid_db/predicate_inheritance.pl
./hybrid_db/retry_undefined.pl
./hybrid_db/virtualize_source.pl
./hybrid_db/lockable_vars.pl
./hybrid_db/clause_attvars.pl
./hybrid_db/sanity_tests.pl
./hybrid_db/attvar_reader.pl

./misc_lm/logicmoo_util_bb_env.pl
./misc_lm/logicmoo_util_bb_gvar.pl
./misc_lm/logicmoo_util_dra.pl
./misc_lm/logicmoo_util_ctx_frame.pl
./misc_lm/logicmoo_util_structs.pl
./misc_lm/logicmoo_util_butterfly.pl
./misc_lm/logicmoo_util_dlist.pl
./misc_lm/logicmoo_util_strings.pl
./misc_lm/logicmoo_util_engines.pl
./misc_lm/logicmoo_util_terms.pl
./file_utils/filestreams.pl
./file_utils/filesystem.pl

```

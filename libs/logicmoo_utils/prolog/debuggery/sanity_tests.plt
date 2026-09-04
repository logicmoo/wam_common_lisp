
:- use_module(library(must_trace)).

:- sanity(current_prolog_flag(runtime_saftey,1)).
:- sanity(current_prolog_flag(runtime_debug,1)).
:- sanity(current_prolog_flag(runtime_speed,1)).
:- sanity(current_prolog_flag(runtime_space,1)).
:- sanity(current_prolog_flag(compilation_speed,1)).

:- use_module(library(bugger)).

:- sanity( \+ current_prolog_flag(runtime_must,keep_going)).
:- sanity( \+ current_prolog_flag(unsafe_speedups,false)).
:- sanity( \+ current_prolog_flag(lm_expanders,false)).


test_must_throw:- must( fail).

test_must_throw:- must_once(fail).

test_must_throw:- must_det(between(0,2,_)).

test_must_throw:- sanity(fail).

% quietly(:GOAL) - if tracing, temporaily quietly/1
test_must_throw:- trace, quietly(must(\+ tracing)).

all_tests:- forall(clause(test_must_throw,Goal),
  catch((Goal,dmsg('NoThrow'(Goal)))*->fail;dmsg('Failed'(Goal)),
	Did,dmsg('Success'(Goal:Did)))).

:- listing.



:- use_module(library(must_sanity)).
:- ensure_loaded(library(ansimesg)).

:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).

:- set_prolog_flag(must_type,keep_going).

test(0):- must(\+ fail).

test(1):- must_once(fail).

all_tests:- forall(test(_),true).

:- listing(test(_)).

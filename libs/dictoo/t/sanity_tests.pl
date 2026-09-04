:- module(dictoo_sanity, []).


:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).

:- consult(library(dictoo_tests)).
:- consult(library(dictoo_example)).



:- module(wamcl,[wamcl/0]).

:- set_prolog_flag(verbose_autoload,false).
:- set_prolog_flag(verbose_load,false).
:- ensure_loaded(library(wam_cl/repl)).

wamcl:- lisp.

:- set_prolog_flag(verbose_autoload,false).
:- set_prolog_flag(verbose_load,false).
:- initialization(lisp,main).


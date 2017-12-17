#!/usr/bin/env swipl

:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
:- set_prolog_flag(gc,false).
:- use_module(library(wamcl)).



#!/usr/bin/env swipl

:- cls.
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
:- set_prolog_flag(gc,false).
:- ensure_loaded(library(wamcl)).



#!/usr/bin/env swipl

:- use_module(library(must_trace)).
:- use_module(library(dmsg)).
:- cls.
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
:- set_prolog_flag(occurs_check,error).

:- ensure_loaded(library(wamcl)).



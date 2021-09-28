#!/usr/bin/env lmoo-clif

% Tests Emulation of assertable attributed variables
% :- include(library(logicmoo_test_header)).

:- ensure_loaded(library(logicmoo_test)).

%:- set_prolog_flag(logicmoo_message_hook, break).
:- set_prolog_flag(gc, false).

:- use_module(library(wamcl)).
% 7.26094900
% 7,991.83946480

:- f_load("sanity-test",_).




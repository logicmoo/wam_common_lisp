#!/usr/bin/env swipl

% Tests Emulation of assertable attributed variables
% :- include(test_header).

:- ensure_loaded(library(pfc_test)).

%:- set_prolog_flag(logicmoo_message_hook, break).
:- set_prolog_flag(gc, false).

:- use_module(library(wamcl)).
% 7.26094900
% 7,991.83946480

:- f_load("sanity-test",_).




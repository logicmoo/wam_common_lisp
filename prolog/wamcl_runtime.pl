:- module(wmclrt,[load_wamcl_runtime/0]).

:- set_prolog_flag(verbose_autoload,false).
:- set_prolog_flag(verbose_load,false).
:- ensure_loaded(wamcl).

load_wamcl_runtime.


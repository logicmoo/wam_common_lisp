:- module(wmclrt,[load_wamcl_runtime/0]).
:- include('./wam_cl/header').
:- ensure_loaded(wamcl).
:- create_prolog_flag(lisp_repl_goal,true,[keep(true),type(term)]).
:- initialization((do_wamcl_inits),now).

load_wamcl_runtime.


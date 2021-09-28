:- module(wmclrt,[load_wamcl_runtime/0]).

:- create_prolog_flag(lisp_repl_goal,true,[keep(true),type(term)]).
:- include('./wam_cl/header').
:- ensure_loaded(wamcl).
:- initialization((do_wamcl_inits),now).
%:- initialization((do_wamcl_inits),restore).

load_wamcl_runtime.


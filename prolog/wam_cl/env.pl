/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(env, []).
:- set_module(class(library)).
:- include('header.pro').

env_toplevel(GlobalBindings):- must(nb_current('$env_toplevel',GlobalBindings)).

:- nb_setval('$env_toplevel',[bv(tl,[])]).

env_current(Env):- nb_current('$env_current',Env),!.
env_current(Env):- env_toplevel(Env),nb_linkval('$env_current',Env),!.

reenter_lisp(Ctx,Env):- env_current(Env),new_compile_ctx(Ctx). 
% GlobalBindings
:- fixup_exports.


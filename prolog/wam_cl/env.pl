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

lisp_global_bindings(GlobalBindings):- must(nb_current('$toplevel_env',GlobalBindings)).

% GlobalBindings

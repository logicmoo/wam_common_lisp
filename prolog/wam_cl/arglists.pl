/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(arglists, []).
:- set_module(class(library)).
:- include('header.pro').

bind_formal_parameters(Formal, Actual, Bindings):-
	env_bind_variables(Formal, Actual, [], Bindings).

bind_formal_parameters([], [], Env, Env).
bind_formal_parameters([FormalParam|FormalParams], [ActualParam|ActualParams],
		Bindings0, Bindings):- 
	bind_formal_parameters(FormalParams, ActualParams, 
		[bv(FormalParam, [ActualParam|_])|Bindings0], Bindings).

:- fixup_exports.


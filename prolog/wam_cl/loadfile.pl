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
:- module(loadfile, []).
:- set_module(class(library)).
:- include('header.pro').



cl:load(File,t):- with_lisp_translation(File,writeExpression).

cl:load_compile(File,t):- with_lisp_translation(File,lisp_compile).
% cl:load_compile(File,t):- with_lisp_translation(File,lisp_compiled_eval).





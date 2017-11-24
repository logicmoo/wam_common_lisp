/*******************************************************************
 *
 * C1 Common Lisp compiler/interpretor, written in Prolog
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
:- module(evil, []).
:- set_module(class(library)).
:- include('header.pro').

cl_declare(_,_).
cl_proclaim(_,_).

cl_sxhash(O,H):- term_hash(O,H).

:- cl_intern("PSXHASH",pkg_sys,_).
f_sys_psxhash(O,H):- f_u_to_pvs(O,HT),term_hash(HT,H).
f_u_psxhash(O,H):-f_sys_psxhash(O,H).

:- fixup_exports.



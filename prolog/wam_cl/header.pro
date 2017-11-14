/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 * This codebase contains ideas from the examinations of  
 *   Neil Smith, Tim Finin, Markus Triska and the Straightforward Implementation of Scheme
 * 
 * Changes since 2001:
 *
 *
 *******************************************************************/

:- set_prolog_flag(verbose_load,full).
:- set_prolog_flag(verbose,normal).
:- set_prolog_flag(verbose_autoload,true).
:- ensure_loaded(library(must_trace)).
:- ensure_loaded(library(logicmoo_util_terms)).
:- ensure_loaded(library(logicmoo_util_common)).
%:- ensure_loaded(utils_list).
:- ensure_loaded(utils_higher_order).
:- ensure_loaded(library(dmsg)).

% :- require([colormsg1/1]).

:- set_prolog_flag(verbose_load,full).
:- set_prolog_flag(verbose,normal).
:- set_prolog_flag(verbose_autoload,true).


:- dynamic(tst:is_local_test/1).
:- multifile(tst:is_local_test/1).
:- discontiguous(tst:is_local_test/1).
:- dynamic(tst:is_local_test/2).
:- multifile(tst:is_local_test/2).
:- discontiguous(tst:is_local_test/2).
:- dynamic(tst:is_local_test/3).
:- multifile(tst:is_local_test/3).
:- discontiguous(tst:is_local_test/3).
:- dynamic(shared_lisp_compiler:plugin_expand_function_body/5).
:- multifile(shared_lisp_compiler:plugin_expand_function_body/5).
:- discontiguous(shared_lisp_compiler:plugin_expand_function_body/5).

:- multifile(user:op_replacement/2).
:- dynamic(user:op_replacement/2).
:- discontiguous(user:op_replacement/2).


%:- dynamic(compile_assigns/4).
%:- multifile(compile_assigns/4).
%:- discontiguous(compile_assigns/4).

%:- dynamic(ssip_define/2).
:- multifile(ssip_define/2).
:- discontiguous(ssip_define/2).



:-ensure_loaded(arglists).
:-ensure_loaded(assign).
:-ensure_loaded(backquote).
:-ensure_loaded(block).
:-ensure_loaded(callp).
:-ensure_loaded(compile).
:-ensure_loaded(compile_funop).
:-ensure_loaded(docs).
:-ensure_loaded(hashtables).
:-ensure_loaded(env).
:-ensure_loaded(interp).
:-ensure_loaded(loadfile).
:-ensure_loaded(mizepro).
:-ensure_loaded(package).
:-ensure_loaded(places).
:-ensure_loaded(pnames).
:-ensure_loaded(prims).
:-ensure_loaded(print).
:-ensure_loaded(readtables).
:-ensure_loaded(repl).
:-ensure_loaded(soops).
:-ensure_loaded(sreader).
:-ensure_loaded(streams).
:-ensure_loaded(string).
:-ensure_loaded(symbol).
:-ensure_loaded(tagbody).
:-ensure_loaded(tests).
/*
:-ensure_loaded(neil_smith).
:-ensure_loaded(utils_for_swi).
:-ensure_loaded(utils_higher_order).
:-ensure_loaded(utils_list).
:-ensure_loaded(utils_oset).
:-ensure_loaded(utils_set).
:-ensure_loaded(utils_shortest_paths).
:-ensure_loaded(utils_writef).
*/

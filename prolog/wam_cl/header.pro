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
:- use_module(library(must_trace)).
:- use_module(library(logicmoo_util_terms)).
:- use_module(library(logicmoo_util_common)).
%:- use_module(utils_list).
:- use_module(utils_higher_order).
:- use_module(library(dmsg)).

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



:- user:use_module(arglists).
:- user:use_module(assign).
:- user:use_module(backquote).
:- user:use_module(block).
:- user:use_module(callp).
:- user:use_module(compile_funop).
:- user:use_module(compile).
:- user:use_module(docs).
:- user:use_module(hashtables).
:- user:use_module(env).
:- user:use_module(interp).
:- user:use_module(loadfile).
:- user:use_module(mizepro).
:- user:use_module(package).
:- user:use_module(places).
:- user:use_module(pathname).
:- user:use_module(funcall).
:- user:use_module(pnames).
:- user:use_module(prims).
:- user:use_module(print).
:- user:use_module(readtables).
:- user:use_module(repl).
:- user:use_module(soops).
:- user:use_module(sreader).
:- user:use_module(streams).
:- user:use_module(string).
:- user:use_module(symbol).
:- user:use_module(tagbody).
:- user:use_module(typeof).
:- user:use_module(tests).
/*
:- user:use_module(neil_smith).
:- user:use_module(utils_for_swi).
:- user:use_module(utils_higher_order).
:- user:use_module(utils_list).
:- user:use_module(utils_oset).
:- user:use_module(utils_set).
:- user:use_module(utils_shortest_paths).
:- user:use_module(utils_writef).
*/

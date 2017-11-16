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



:-use_module(arglists).
:-use_module(assign).
:-use_module(backquote).
:-use_module(block).
:-use_module(callp).
:-use_module(compile_funop).
:-use_module(compile).
:-use_module(docs).
:-use_module(hashtables).
:-use_module(env).
:-use_module(interp).
:-use_module(loadfile).
:-use_module(mizepro).
:-use_module(package).
:-use_module(places).
:-use_module(pathname).
:-use_module(funcall).
:-use_module(pnames).
:-use_module(prims).
:-use_module(print).
:-use_module(readtables).
:-use_module(repl).
:-use_module(soops).
:-use_module(sreader).
:-use_module(streams).
:-use_module(string).
:-use_module(symbol).
:-use_module(tagbody).
:-use_module(typeof).
:-use_module(tests).
/*
:-use_module(neil_smith).
:-use_module(utils_for_swi).
:-use_module(utils_higher_order).
:-use_module(utils_list).
:-use_module(utils_oset).
:-use_module(utils_set).
:-use_module(utils_shortest_paths).
:-use_module(utils_writef).
*/

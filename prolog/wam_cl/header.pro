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

:- dynamic(user:arglist_info/4).
:- multifile(user:arglist_info/4).

:- dynamic(user:function_lambda/4).
:- multifile(user:function_lambda/4).

%:- dynamic(compile_assigns/4).
%:- multifile(compile_assigns/4).
%:- discontiguous(compile_assigns/4).

%:- dynamic(ssip_define/2).
:- multifile(ssip_define/2).
:- discontiguous(ssip_define/2).

:- ensure_loaded('8ball.pl').
:- ensure_loaded('evil_workarounds.pl').
:- ensure_loaded('arglists.pl').
:- ensure_loaded('array.pl').
:- ensure_loaded('assign.pl').
:- ensure_loaded('backquote.pl').
:- ensure_loaded('reader_macros.pl').
:- ensure_loaded('block.pl').
:- ensure_loaded('body.pl').
:- ensure_loaded('callp.pl').
:- ensure_loaded('compile.pl').
:- ensure_loaded('compile_funop.pl').
:- ensure_loaded('conditions.pl').
:- ensure_loaded('decls.pl').
:- ensure_loaded('docs.pl').
:- ensure_loaded('env.pl').
:- ensure_loaded('funcall.pl').
:- ensure_loaded('hashtables.pl').
:- ensure_loaded('interp.pl').
:- ensure_loaded('loadfile.pl').
:- ensure_loaded('mizepro.pl').
:- ensure_loaded('package.pl').
:- ensure_loaded('pathname.pl').
:- ensure_loaded('places.pl').
:- ensure_loaded('pnames.pl').
:- ensure_loaded('prims.pl').
:- ensure_loaded('print.pl').
:- ensure_loaded('readtables.pl').
:- ensure_loaded('repl.pl').
:- ensure_loaded('soops.pl').
:- ensure_loaded('sreader.pl').
:- ensure_loaded('streams.pl').
:- ensure_loaded('string.pl').
:- ensure_loaded('symbol.pl').
:- ensure_loaded('tagbody.pl').
:- ensure_loaded('tests.pl').
:- ensure_loaded('typeof.pl').
:- ensure_loaded('disassemble.pl').


/*
:- ensure_loaded('utils_for_swi.pl').
:- ensure_loaded('utils_higher_order.pl').
:- ensure_loaded('utils_list.pl').
:- ensure_loaded('utils_oset.pl').
:- ensure_loaded('utils_set.pl').
:- ensure_loaded('utils_shortest_paths.pl').
:- ensure_loaded('utils_writef.pl').
*/

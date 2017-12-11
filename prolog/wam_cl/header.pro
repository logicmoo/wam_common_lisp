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

:- set_prolog_flag(generate_debug_info, false).

% :- require([colormsg1/1]).


:- if(current_prolog_flag(debug,false)).

%:- set_prolog_flag(last_call_optimisation,true).
 :- set_prolog_flag(compile_meta_arguments,false).
%:- set_prolog_flag(access_level,system).

:- endif.

:- use_module(library(must_trace)).
:- use_module(library(logicmoo_util_terms)).
:- use_module(library(logicmoo_util_common)).
:- user:use_module(library(globals_api)).
%:- use_module(utils_list).
:- use_module(utils_higher_order).
:- use_module(library(dmsg)).


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

:- multifile(wl:op_replacement/2).
:- dynamic(wl:op_replacement/2).
:- discontiguous(wl:op_replacement/2).

:- multifile(wl:declared/2).
:- dynamic(wl:declared/2).
:- discontiguous(wl:declared/2).

:- multifile(wl:arg_lambda_type/2).
:- dynamic(wl:arg_lambda_type/2).
:- discontiguous(wl:arg_lambda_type/2).

:- multifile(wl:uses_rest_only/1).
:- dynamic(wl:uses_rest_only/1).
:- discontiguous(wl:uses_rest_only/1).

:- multifile(wl:interned_eval/1).
:- dynamic(wl:interned_eval/1).
:- discontiguous(wl:interned_eval/1).

:- multifile(wl:wam_cl_setup/1).
:- dynamic(wl:wam_cl_setup/1).
:- discontiguous(wl:wam_cl_setup/1).


:- multifile(wl:type_checked/1).
:- dynamic(wl:type_checked/1).
:- discontiguous(wl:type_checked/1).


:- multifile(wl:coercion/3).
:- dynamic(wl:coercion/3).
:- discontiguous(wl:coercion/3).

:- discontiguous(wl:lambda_def/5).
:- dynamic(wl:lambda_def/5).
:- multifile(wl:lambda_def/5).
:- export(wl:lambda_def/5).
:- system:import(wl:lambda_def/5).

:- discontiguous(wl:arglist_info/5).
:- dynamic(wl:arglist_info/5).
:- multifile(wl:arglist_info/5).
:- export(wl:arglist_info/5).
:- system:import(wl:arglist_info/5).

:- discontiguous(wl:arglist_info/4).
:- dynamic(wl:arglist_info/4).
:- multifile(wl:arglist_info/4).
:- export(wl:arglist_info/4).
:- system:import(wl:arglist_info/4).

%:- dynamic(compile_assigns/4).
%:- multifile(compile_assigns/4).
%:- discontiguous(compile_assigns/4).

%:- dynamic(ssip_define/2).
:- multifile(ssip_define/2).
:- discontiguous(ssip_define/2).

:- ensure_loaded('8ball.pl').
:- ensure_loaded('disassemble.pl').
:- ensure_loaded('typecheck.pl').
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
:- ensure_loaded('math.pl').



/*
:- ensure_loaded('utils_for_swi.pl').
:- ensure_loaded('utils_higher_order.pl').
:- ensure_loaded('utils_list.pl').
:- ensure_loaded('utils_oset.pl').
:- ensure_loaded('utils_set.pl').
:- ensure_loaded('utils_shortest_paths.pl').
:- ensure_loaded('utils_writef.pl').
*/

#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "compat" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:14:18 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
*******************************************************************************
*/
/*
*/
/*
 GATE
*/
/*
 Version 2.3
*/
/*
*/
/*
 Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
*/
/*
 All Rights Reserved.
*/
/*
*/
/*
 This file contains:
*/
/*
 Compatibility functions for T2.8/T3 (Scheme) code running under Common Lisp
*/
/*
*/
/*
 19990429: begun
*/
/*
 19990503: more work
*/
/*
*/
/*
(setq else t)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:345 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,else,t])
:- set_var(AEnv, setq, u_else, t).
/*
(setq *repl-wont-print* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:360 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*repl-wont-print*',[]])
:- set_var(AEnv, setq, u_xx_repl_wont_print_xx, []).
/*
(defun t-or-nil (a) (if a 't nil))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:389 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'t-or-nil',[a],[if,a,[quote,t],[]]])
wl:lambda_def(defun, u_t_or_nil, f_u_t_or_nil, [u_a], [[if, u_a, [quote, t], []]]).
wl:arglist_info(u_t_or_nil, f_u_t_or_nil, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_t_or_nil).

/*

### Compiled:  `U::T-OR-NIL` 
*/
f_u_t_or_nil(A, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_a, A)|Env],
	get_var(Env12, u_a, IFTEST),
	(   IFTEST\==[]
	->  FnResult=t
	;   FnResult=[]
	).
:- set_opv(f_u_t_or_nil, classof, claz_function),
   set_opv(u_t_or_nil, compile_as, kw_function),
   set_opv(u_t_or_nil, function, f_u_t_or_nil),
   _Ignored4=u_t_or_nil.
/*
:- side_effect(assert_lsp(u_t_or_nil,
			  wl:lambda_def(defun, u_t_or_nil, f_u_t_or_nil, [u_a], [[if, u_a, [quote, t], []]]))).
*/
/*
:- side_effect(assert_lsp(u_t_or_nil,
			  wl:arglist_info(u_t_or_nil, f_u_t_or_nil, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_t_or_nil, wl:init_args(exact_only, f_u_t_or_nil))).
*/
/*
(defmacro string->symbol (a) `(intern ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:424 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string->symbol',[a],['#BQ',[intern,['#COMMA',a]]]])
wl:lambda_def(defmacro, u_string_c62_symbol, f_u_string_c62_symbol, [u_a], [progn, ['#BQ', [intern, ['#COMMA', u_a]]]]).
wl:arglist_info(u_string_c62_symbol, f_u_string_c62_symbol, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_c62_symbol).

/*

### Compiled:  `U::STRING->SYMBOL` 
*/
f_u_string_c62_symbol(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[intern, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_c62_symbol, classof, claz_macro),
   set_opv(u_string_c62_symbol, compile_as, kw_operator),
   set_opv(u_string_c62_symbol, function, f_u_string_c62_symbol),
   _Ignored4=u_string_c62_symbol.
/*
:- side_effect(assert_lsp(u_string_c62_symbol,
			  wl:lambda_def(defmacro, u_string_c62_symbol, f_u_string_c62_symbol, [u_a], [progn, ['#BQ', [intern, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_c62_symbol,
			  wl:arglist_info(u_string_c62_symbol, f_u_string_c62_symbol, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_c62_symbol,
			  wl:init_args(exact_only, f_u_string_c62_symbol))).
*/
/*
(defmacro symbol->string (a) `(symbol-name ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:467 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'symbol->string',[a],['#BQ',['symbol-name',['#COMMA',a]]]])
wl:lambda_def(defmacro, u_symbol_c62_string, f_u_symbol_c62_string, [u_a], [progn, ['#BQ', [symbol_name, ['#COMMA', u_a]]]]).
wl:arglist_info(u_symbol_c62_string, f_u_symbol_c62_string, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_symbol_c62_string).

/*

### Compiled:  `U::SYMBOL->STRING` 
*/
f_u_symbol_c62_string(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[symbol_name, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_symbol_c62_string, classof, claz_macro),
   set_opv(u_symbol_c62_string, compile_as, kw_operator),
   set_opv(u_symbol_c62_string, function, f_u_symbol_c62_string),
   _Ignored4=u_symbol_c62_string.
/*
:- side_effect(assert_lsp(u_symbol_c62_string,
			  wl:lambda_def(defmacro, u_symbol_c62_string, f_u_symbol_c62_string, [u_a], [progn, ['#BQ', [symbol_name, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_symbol_c62_string,
			  wl:arglist_info(u_symbol_c62_string, f_u_symbol_c62_string, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_symbol_c62_string,
			  wl:init_args(exact_only, f_u_symbol_c62_string))).
*/
/*
(defmacro string-length (a) `(length ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:515 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-length',[a],['#BQ',[length,['#COMMA',a]]]])
wl:lambda_def(defmacro, u_string_length, f_u_string_length, [u_a], [progn, ['#BQ', [length, ['#COMMA', u_a]]]]).
wl:arglist_info(u_string_length, f_u_string_length, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_length).

/*

### Compiled:  `U::STRING-LENGTH` 
*/
f_u_string_length(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[length, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_length, classof, claz_macro),
   set_opv(u_string_length, compile_as, kw_operator),
   set_opv(u_string_length, function, f_u_string_length),
   _Ignored4=u_string_length.
/*
:- side_effect(assert_lsp(u_string_length,
			  wl:lambda_def(defmacro, u_string_length, f_u_string_length, [u_a], [progn, ['#BQ', [length, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_length,
			  wl:arglist_info(u_string_length, f_u_string_length, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_length,
			  wl:init_args(exact_only, f_u_string_length))).
*/
/*
(defmacro string-empty? (a) `(= (length ,a) 0))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:557 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-empty?',[a],['#BQ',[=,[length,['#COMMA',a]],0]]])
wl:lambda_def(defmacro, u_string_empty_c63, f_u_string_empty_c63, [u_a], [progn, ['#BQ', [=, [length, ['#COMMA', u_a]], 0]]]).
wl:arglist_info(u_string_empty_c63, f_u_string_empty_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_empty_c63).

/*

### Compiled:  `U::STRING-EMPTY?` 
*/
f_u_string_empty_c63(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[=, [length, A_Get], 0]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_empty_c63, classof, claz_macro),
   set_opv(u_string_empty_c63, compile_as, kw_operator),
   set_opv(u_string_empty_c63, function, f_u_string_empty_c63),
   _Ignored4=u_string_empty_c63.
/*
:- side_effect(assert_lsp(u_string_empty_c63,
			  wl:lambda_def(defmacro, u_string_empty_c63, f_u_string_empty_c63, [u_a], [progn, ['#BQ', [=, [length, ['#COMMA', u_a]], 0]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_empty_c63,
			  wl:arglist_info(u_string_empty_c63, f_u_string_empty_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_empty_c63,
			  wl:init_args(exact_only, f_u_string_empty_c63))).
*/
/*
(defmacro string-slice (a b c) `(subseq ,a ,b ,c))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:605 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-slice',[a,b,c],['#BQ',[subseq,['#COMMA',a],['#COMMA',b],['#COMMA',c]]]])
wl:lambda_def(defmacro, u_string_slice, f_u_string_slice, [u_a, u_b, u_c], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], ['#COMMA', u_c]]]]).
wl:arglist_info(u_string_slice, f_u_string_slice, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_slice).

/*

### Compiled:  `U::STRING-SLICE` 
*/
f_u_string_slice(A, B, C, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B), bv(u_c, C)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	get_var(Env, u_c, C_Get),
	[subseq, A_Get, B_Get, C_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_slice, classof, claz_macro),
   set_opv(u_string_slice, compile_as, kw_operator),
   set_opv(u_string_slice, function, f_u_string_slice),
   _Ignored4=u_string_slice.
/*
:- side_effect(assert_lsp(u_string_slice,
			  wl:lambda_def(defmacro, u_string_slice, f_u_string_slice, [u_a, u_b, u_c], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], ['#COMMA', u_c]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_slice,
			  wl:arglist_info(u_string_slice, f_u_string_slice, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_slice,
			  wl:init_args(exact_only, f_u_string_slice))).
*/
/*
(defmacro substring (a b c) `(subseq ,a ,b ,c))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:656 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,substring,[a,b,c],['#BQ',[subseq,['#COMMA',a],['#COMMA',b],['#COMMA',c]]]])
wl:lambda_def(defmacro, ext_substring, f_ext_substring, [u_a, u_b, u_c], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], ['#COMMA', u_c]]]]).
wl:arglist_info(ext_substring, f_ext_substring, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_substring).

/*

### Compiled:  `EXT:SUBSTRING` 
*/
f_ext_substring(A, B, C, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B), bv(u_c, C)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	get_var(Env, u_c, C_Get),
	[subseq, A_Get, B_Get, C_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_substring, classof, claz_macro),
   set_opv(ext_substring, compile_as, kw_operator),
   set_opv(ext_substring, function, f_ext_substring),
   _Ignored4=ext_substring.
/*
:- side_effect(assert_lsp(ext_substring,
			  wl:lambda_def(defmacro, ext_substring, f_ext_substring, [u_a, u_b, u_c], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], ['#COMMA', u_c]]]]))).
*/
/*
:- side_effect(assert_lsp(ext_substring,
			  wl:arglist_info(ext_substring, f_ext_substring, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(ext_substring,
			  wl:init_args(exact_only, f_ext_substring))).
*/
/*
(defmacro string-nthtail (a b) `(subseq ,a ,b (length ,a)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:704 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-nthtail',[a,b],['#BQ',[subseq,['#COMMA',a],['#COMMA',b],[length,['#COMMA',a]]]]])
wl:lambda_def(defmacro, u_string_nthtail, f_u_string_nthtail, [u_a, u_b], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], [length, ['#COMMA', u_a]]]]]).
wl:arglist_info(u_string_nthtail, f_u_string_nthtail, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_nthtail).

/*

### Compiled:  `U::STRING-NTHTAIL` 
*/
f_u_string_nthtail(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get9),
	get_var(Env, u_b, B_Get),
	[subseq, A_Get9, B_Get, [length, A_Get9]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_nthtail, classof, claz_macro),
   set_opv(u_string_nthtail, compile_as, kw_operator),
   set_opv(u_string_nthtail, function, f_u_string_nthtail),
   _Ignored4=u_string_nthtail.
/*
:- side_effect(assert_lsp(u_string_nthtail,
			  wl:lambda_def(defmacro, u_string_nthtail, f_u_string_nthtail, [u_a, u_b], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], [length, ['#COMMA', u_a]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_nthtail,
			  wl:arglist_info(u_string_nthtail, f_u_string_nthtail, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_nthtail,
			  wl:init_args(exact_only, f_u_string_nthtail))).
*/
/*
(defmacro nthchdr (a b) `(subseq ,a ,b (length ,a)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:764 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,nthchdr,[a,b],['#BQ',[subseq,['#COMMA',a],['#COMMA',b],[length,['#COMMA',a]]]]])
wl:lambda_def(defmacro, u_nthchdr, f_u_nthchdr, [u_a, u_b], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], [length, ['#COMMA', u_a]]]]]).
wl:arglist_info(u_nthchdr, f_u_nthchdr, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_nthchdr).

/*

### Compiled:  `U::NTHCHDR` 
*/
f_u_nthchdr(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get9),
	get_var(Env, u_b, B_Get),
	[subseq, A_Get9, B_Get, [length, A_Get9]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nthchdr, classof, claz_macro),
   set_opv(u_nthchdr, compile_as, kw_operator),
   set_opv(u_nthchdr, function, f_u_nthchdr),
   _Ignored4=u_nthchdr.
/*
:- side_effect(assert_lsp(u_nthchdr,
			  wl:lambda_def(defmacro, u_nthchdr, f_u_nthchdr, [u_a, u_b], [progn, ['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], [length, ['#COMMA', u_a]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_nthchdr,
			  wl:arglist_info(u_nthchdr, f_u_nthchdr, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_nthchdr, wl:init_args(exact_only, f_u_nthchdr))).
*/
/*
(defmacro string-downcase! (a) `(string-downcase ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:817 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-downcase!',[a],['#BQ',['string-downcase',['#COMMA',a]]]])
wl:lambda_def(defmacro, u_string_downcase_c33, f_u_string_downcase_c33, [u_a], [progn, ['#BQ', [string_downcase, ['#COMMA', u_a]]]]).
wl:arglist_info(u_string_downcase_c33, f_u_string_downcase_c33, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_downcase_c33).

/*

### Compiled:  `U::STRING-DOWNCASE!` 
*/
f_u_string_downcase_c33(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[string_downcase, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_downcase_c33, classof, claz_macro),
   set_opv(u_string_downcase_c33, compile_as, kw_operator),
   set_opv(u_string_downcase_c33, function, f_u_string_downcase_c33),
   _Ignored4=u_string_downcase_c33.
/*
:- side_effect(assert_lsp(u_string_downcase_c33,
			  wl:lambda_def(defmacro, u_string_downcase_c33, f_u_string_downcase_c33, [u_a], [progn, ['#BQ', [string_downcase, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_downcase_c33,
			  wl:arglist_info(u_string_downcase_c33, f_u_string_downcase_c33, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_downcase_c33,
			  wl:init_args(exact_only, f_u_string_downcase_c33))).
*/
/*
(defmacro string-write (a b) `(write-string ,b ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:871 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-write',[a,b],['#BQ',['write-string',['#COMMA',b],['#COMMA',a]]]])
wl:lambda_def(defmacro, u_string_write, f_u_string_write, [u_a, u_b], [progn, ['#BQ', [write_string, ['#COMMA', u_b], ['#COMMA', u_a]]]]).
wl:arglist_info(u_string_write, f_u_string_write, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_write).

/*

### Compiled:  `U::STRING-WRITE` 
*/
f_u_string_write(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[write_string, B_Get, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_write, classof, claz_macro),
   set_opv(u_string_write, compile_as, kw_operator),
   set_opv(u_string_write, function, f_u_string_write),
   _Ignored4=u_string_write.
/*
:- side_effect(assert_lsp(u_string_write,
			  wl:lambda_def(defmacro, u_string_write, f_u_string_write, [u_a, u_b], [progn, ['#BQ', [write_string, ['#COMMA', u_b], ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_write,
			  wl:arglist_info(u_string_write, f_u_string_write, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_write,
			  wl:init_args(exact_only, f_u_string_write))).
*/
/*
(defmacro map-string! (a b) `(map `string ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:923 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'map-string!',[a,b],['#BQ',[map,['#BQ',string],['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_map_string_c33, f_u_map_string_c33, [u_a, u_b], [progn, ['#BQ', [map, ['#BQ', string], ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_map_string_c33, f_u_map_string_c33, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_map_string_c33).

/*

### Compiled:  `U::MAP-STRING!` 
*/
f_u_map_string_c33(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[map, [quote, string], A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_map_string_c33, classof, claz_macro),
   set_opv(u_map_string_c33, compile_as, kw_operator),
   set_opv(u_map_string_c33, function, f_u_map_string_c33),
   _Ignored4=u_map_string_c33.
/*
:- side_effect(assert_lsp(u_map_string_c33,
			  wl:lambda_def(defmacro, u_map_string_c33, f_u_map_string_c33, [u_a, u_b], [progn, ['#BQ', [map, ['#BQ', string], ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_map_string_c33,
			  wl:arglist_info(u_map_string_c33, f_u_map_string_c33, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_map_string_c33,
			  wl:init_args(exact_only, f_u_map_string_c33))).
*/
/*
(defmacro string-equal? (a b) `(string-equal ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:973 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-equal?',[a,b],['#BQ',['string-equal',['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_string_equal_c63, f_u_string_equal_c63, [u_a, u_b], [progn, ['#BQ', [string_equal, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_string_equal_c63, f_u_string_equal_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_equal_c63).

/*

### Compiled:  `U::STRING-EQUAL?` 
*/
f_u_string_equal_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[string_equal, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_equal_c63, classof, claz_macro),
   set_opv(u_string_equal_c63, compile_as, kw_operator),
   set_opv(u_string_equal_c63, function, f_u_string_equal_c63),
   _Ignored4=u_string_equal_c63.
/*
:- side_effect(assert_lsp(u_string_equal_c63,
			  wl:lambda_def(defmacro, u_string_equal_c63, f_u_string_equal_c63, [u_a, u_b], [progn, ['#BQ', [string_equal, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_equal_c63,
			  wl:arglist_info(u_string_equal_c63, f_u_string_equal_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_equal_c63,
			  wl:init_args(exact_only, f_u_string_equal_c63))).
*/
/*
(defmacro chdr (a) `(subseq ,a 1 (length ,a)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1026 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,chdr,[a],['#BQ',[subseq,['#COMMA',a],1,[length,['#COMMA',a]]]]])
wl:lambda_def(defmacro, u_chdr, f_u_chdr, [u_a], [progn, ['#BQ', [subseq, ['#COMMA', u_a], 1, [length, ['#COMMA', u_a]]]]]).
wl:arglist_info(u_chdr, f_u_chdr, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_chdr).

/*

### Compiled:  `U::CHDR` 
*/
f_u_chdr(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get8),
	[subseq, A_Get8, 1, [length, A_Get8]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_chdr, classof, claz_macro),
   set_opv(u_chdr, compile_as, kw_operator),
   set_opv(u_chdr, function, f_u_chdr),
   _Ignored4=u_chdr.
/*
:- side_effect(assert_lsp(u_chdr,
			  wl:lambda_def(defmacro, u_chdr, f_u_chdr, [u_a], [progn, ['#BQ', [subseq, ['#COMMA', u_a], 1, [length, ['#COMMA', u_a]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_chdr,
			  wl:arglist_info(u_chdr, f_u_chdr, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_chdr, wl:init_args(exact_only, f_u_chdr))).
*/
/*
(defmacro nthchar (a b) `(elt ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1073 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,nthchar,[a,b],['#BQ',[elt,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_nthchar, f_u_nthchar, [u_a, u_b], [progn, ['#BQ', [elt, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_nthchar, f_u_nthchar, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_nthchar).

/*

### Compiled:  `U::NTHCHAR` 
*/
f_u_nthchar(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[elt, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nthchar, classof, claz_macro),
   set_opv(u_nthchar, compile_as, kw_operator),
   set_opv(u_nthchar, function, f_u_nthchar),
   _Ignored4=u_nthchar.
/*
:- side_effect(assert_lsp(u_nthchar,
			  wl:lambda_def(defmacro, u_nthchar, f_u_nthchar, [u_a, u_b], [progn, ['#BQ', [elt, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_nthchar,
			  wl:arglist_info(u_nthchar, f_u_nthchar, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_nthchar, wl:init_args(exact_only, f_u_nthchar))).
*/
/*
(defmacro digit? (a b) `(digit-char-p ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1111 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'digit?',[a,b],['#BQ',['digit-char-p',['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_digit_c63, f_u_digit_c63, [u_a, u_b], [progn, ['#BQ', [digit_char_p, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_digit_c63, f_u_digit_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_digit_c63).

/*

### Compiled:  `U::DIGIT?` 
*/
f_u_digit_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[digit_char_p, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_digit_c63, classof, claz_macro),
   set_opv(u_digit_c63, compile_as, kw_operator),
   set_opv(u_digit_c63, function, f_u_digit_c63),
   _Ignored4=u_digit_c63.
/*
:- side_effect(assert_lsp(u_digit_c63,
			  wl:lambda_def(defmacro, u_digit_c63, f_u_digit_c63, [u_a, u_b], [progn, ['#BQ', [digit_char_p, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_digit_c63,
			  wl:arglist_info(u_digit_c63, f_u_digit_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_digit_c63, wl:init_args(exact_only, f_u_digit_c63))).
*/
/*
(defmacro string-append (&rest args) `(concatenate 'string ,@args))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1157 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-append',['&rest',args],['#BQ',[concatenate,[quote,string],['#BQ-COMMA-ELIPSE',args]]]])
wl:lambda_def(defmacro, u_string_append, f_u_string_append, [c38_rest, args], [progn, ['#BQ', [concatenate, [quote, string], ['#BQ-COMMA-ELIPSE', args]]]]).
wl:arglist_info(u_string_append, f_u_string_append, [c38_rest, args], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[args], opt:0, req:0, rest:[args], sublists:0, whole:0}).
wl: init_args(0, f_u_string_append).

/*

### Compiled:  `U::STRING-APPEND` 
*/
f_u_string_append(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(args, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, args, Args_Get),
	[concatenate, [quote, string]|Args_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_append, classof, claz_macro),
   set_opv(u_string_append, compile_as, kw_operator),
   set_opv(u_string_append, function, f_u_string_append),
   _Ignored4=u_string_append.
/*
:- side_effect(assert_lsp(u_string_append,
			  wl:lambda_def(defmacro, u_string_append, f_u_string_append, [c38_rest, args], [progn, ['#BQ', [concatenate, [quote, string], ['#BQ-COMMA-ELIPSE', args]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_append,
			  wl:arglist_info(u_string_append, f_u_string_append, [c38_rest, args], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[args], opt:0, req:0, rest:[args], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_append, wl:init_args(0, f_u_string_append))).
*/
/*
(defmacro any? (a b) `(t-or-nil (some ,a ,b)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1225 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'any?',[a,b],['#BQ',['t-or-nil',[some,['#COMMA',a],['#COMMA',b]]]]])
wl:lambda_def(defmacro, u_any_c63, f_u_any_c63, [u_a, u_b], [progn, ['#BQ', [u_t_or_nil, [some, ['#COMMA', u_a], ['#COMMA', u_b]]]]]).
wl:arglist_info(u_any_c63, f_u_any_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_any_c63).

/*

### Compiled:  `U::ANY?` 
*/
f_u_any_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[u_t_or_nil, [some, A_Get, B_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_any_c63, classof, claz_macro),
   set_opv(u_any_c63, compile_as, kw_operator),
   set_opv(u_any_c63, function, f_u_any_c63),
   _Ignored4=u_any_c63.
/*
:- side_effect(assert_lsp(u_any_c63,
			  wl:lambda_def(defmacro, u_any_c63, f_u_any_c63, [u_a, u_b], [progn, ['#BQ', [u_t_or_nil, [some, ['#COMMA', u_a], ['#COMMA', u_b]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_any_c63,
			  wl:arglist_info(u_any_c63, f_u_any_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_any_c63, wl:init_args(exact_only, f_u_any_c63))).
*/
/*
(defmacro any (a b) `(some ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1272 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,any,[a,b],['#BQ',[some,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_any, f_u_any, [u_a, u_b], [progn, ['#BQ', [some, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_any, f_u_any, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_any).

/*

### Compiled:  `U::ANY` 
*/
f_u_any(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[some, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_any, classof, claz_macro),
   set_opv(u_any, compile_as, kw_operator),
   set_opv(u_any, function, f_u_any),
   _Ignored4=u_any.
/*
:- side_effect(assert_lsp(u_any,
			  wl:lambda_def(defmacro, u_any, f_u_any, [u_a, u_b], [progn, ['#BQ', [some, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_any,
			  wl:arglist_info(u_any, f_u_any, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_any, wl:init_args(exact_only, f_u_any))).
*/
/*
(defmacro null? (a) `(null ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1307 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'null?',[a],['#BQ',[null,['#COMMA',a]]]])
wl:lambda_def(defmacro, u_null_c63, f_u_null_c63, [u_a], [progn, ['#BQ', [null, ['#COMMA', u_a]]]]).
wl:arglist_info(u_null_c63, f_u_null_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_null_c63).

/*

### Compiled:  `U::NULL?` 
*/
f_u_null_c63(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[null, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_null_c63, classof, claz_macro),
   set_opv(u_null_c63, compile_as, kw_operator),
   set_opv(u_null_c63, function, f_u_null_c63),
   _Ignored4=u_null_c63.
/*
:- side_effect(assert_lsp(u_null_c63,
			  wl:lambda_def(defmacro, u_null_c63, f_u_null_c63, [u_a], [progn, ['#BQ', [null, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_null_c63,
			  wl:arglist_info(u_null_c63, f_u_null_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_null_c63, wl:init_args(exact_only, f_u_null_c63))).
*/
/*
(defmacro eq? (a b) `(eql ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1339 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'eq?',[a,b],['#BQ',[eql,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_eq_c63, f_u_eq_c63, [u_a, u_b], [progn, ['#BQ', [eql, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_eq_c63, f_u_eq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_eq_c63).

/*

### Compiled:  `U::EQ?` 
*/
f_u_eq_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[eql, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_eq_c63, classof, claz_macro),
   set_opv(u_eq_c63, compile_as, kw_operator),
   set_opv(u_eq_c63, function, f_u_eq_c63),
   _Ignored4=u_eq_c63.
/*
:- side_effect(assert_lsp(u_eq_c63,
			  wl:lambda_def(defmacro, u_eq_c63, f_u_eq_c63, [u_a, u_b], [progn, ['#BQ', [eql, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_eq_c63,
			  wl:arglist_info(u_eq_c63, f_u_eq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_eq_c63, wl:init_args(exact_only, f_u_eq_c63))).
*/
/*
(defmacro alikeq? (a b) `(equalp ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1373 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'alikeq?',[a,b],['#BQ',[equalp,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_alikeq_c63, f_u_alikeq_c63, [u_a, u_b], [progn, ['#BQ', [equalp, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_alikeq_c63, f_u_alikeq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_alikeq_c63).

/*

### Compiled:  `U::ALIKEQ?` 
*/
f_u_alikeq_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[equalp, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_alikeq_c63, classof, claz_macro),
   set_opv(u_alikeq_c63, compile_as, kw_operator),
   set_opv(u_alikeq_c63, function, f_u_alikeq_c63),
   _Ignored4=u_alikeq_c63.
/*
:- side_effect(assert_lsp(u_alikeq_c63,
			  wl:lambda_def(defmacro, u_alikeq_c63, f_u_alikeq_c63, [u_a, u_b], [progn, ['#BQ', [equalp, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_alikeq_c63,
			  wl:arglist_info(u_alikeq_c63, f_u_alikeq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_alikeq_c63,
			  wl:init_args(exact_only, f_u_alikeq_c63))).
*/
/*
(defmacro neq? (a b) `(not (eql ,a ,b)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1414 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'neq?',[a,b],['#BQ',[not,[eql,['#COMMA',a],['#COMMA',b]]]]])
wl:lambda_def(defmacro, u_neq_c63, f_u_neq_c63, [u_a, u_b], [progn, ['#BQ', [not, [eql, ['#COMMA', u_a], ['#COMMA', u_b]]]]]).
wl:arglist_info(u_neq_c63, f_u_neq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_neq_c63).

/*

### Compiled:  `U::NEQ?` 
*/
f_u_neq_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[not, [eql, A_Get, B_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_neq_c63, classof, claz_macro),
   set_opv(u_neq_c63, compile_as, kw_operator),
   set_opv(u_neq_c63, function, f_u_neq_c63),
   _Ignored4=u_neq_c63.
/*
:- side_effect(assert_lsp(u_neq_c63,
			  wl:lambda_def(defmacro, u_neq_c63, f_u_neq_c63, [u_a, u_b], [progn, ['#BQ', [not, [eql, ['#COMMA', u_a], ['#COMMA', u_b]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_neq_c63,
			  wl:arglist_info(u_neq_c63, f_u_neq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_neq_c63, wl:init_args(exact_only, f_u_neq_c63))).
*/
/*
(defmacro memq? (a b) `(t-or-nil (member ,a ,b)))

;; #-abcl 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1455 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'memq?',[a,b],['#BQ',['t-or-nil',[member,['#COMMA',a],['#COMMA',b]]]]])
wl:lambda_def(defmacro, u_memq_c63, f_u_memq_c63, [u_a, u_b], [progn, ['#BQ', [u_t_or_nil, [member, ['#COMMA', u_a], ['#COMMA', u_b]]]]]).
wl:arglist_info(u_memq_c63, f_u_memq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_memq_c63).

/*

### Compiled:  `U::MEMQ?` 
*/
f_u_memq_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[u_t_or_nil, [member, A_Get, B_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_memq_c63, classof, claz_macro),
   set_opv(u_memq_c63, compile_as, kw_operator),
   set_opv(u_memq_c63, function, f_u_memq_c63),
   _Ignored4=u_memq_c63.
/*
:- side_effect(assert_lsp(u_memq_c63,
			  wl:lambda_def(defmacro, u_memq_c63, f_u_memq_c63, [u_a, u_b], [progn, ['#BQ', [u_t_or_nil, [member, ['#COMMA', u_a], ['#COMMA', u_b]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_memq_c63,
			  wl:arglist_info(u_memq_c63, f_u_memq_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_memq_c63, wl:init_args(exact_only, f_u_memq_c63))).
*/
/*
; #-abcl 
*/
/*
(defmacro memq (a b) `(member ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1517 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,memq,[a,b],['#BQ',[member,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, ext_memq, f_ext_memq, [u_a, u_b], [progn, ['#BQ', [member, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(ext_memq, f_ext_memq, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_memq).

/*

### Compiled:  `EXT:MEMQ` 
*/
f_ext_memq(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[member, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_memq, classof, claz_macro),
   set_opv(ext_memq, compile_as, kw_operator),
   set_opv(ext_memq, function, f_ext_memq),
   _Ignored4=ext_memq.
/*
:- side_effect(assert_lsp(ext_memq,
			  wl:lambda_def(defmacro, ext_memq, f_ext_memq, [u_a, u_b], [progn, ['#BQ', [member, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(ext_memq,
			  wl:arglist_info(ext_memq, f_ext_memq, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(ext_memq, wl:init_args(exact_only, f_ext_memq))).
*/
/*
(defmacro gen-id (symbol) `(gensym ,symbol))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1555 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'gen-id',[symbol],['#BQ',[gensym,['#COMMA',symbol]]]])
wl:lambda_def(defmacro, u_gen_id, f_u_gen_id, [symbol], [progn, ['#BQ', [gensym, ['#COMMA', symbol]]]]).
wl:arglist_info(u_gen_id, f_u_gen_id, [symbol], arginfo{all:[symbol], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol], opt:0, req:[symbol], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gen_id).

/*

### Compiled:  `U::GEN-ID` 
*/
f_u_gen_id(Symbol, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(symbol, Symbol)|Global_env_Ret],
	get_var(Env, symbol, Symbol_Get),
	[gensym, Symbol_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_gen_id, classof, claz_macro),
   set_opv(u_gen_id, compile_as, kw_operator),
   set_opv(u_gen_id, function, f_u_gen_id),
   _Ignored4=u_gen_id.
/*
:- side_effect(assert_lsp(u_gen_id,
			  wl:lambda_def(defmacro, u_gen_id, f_u_gen_id, [symbol], [progn, ['#BQ', [gensym, ['#COMMA', symbol]]]]))).
*/
/*
:- side_effect(assert_lsp(u_gen_id,
			  wl:arglist_info(u_gen_id, f_u_gen_id, [symbol], arginfo{all:[symbol], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol], opt:0, req:[symbol], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gen_id, wl:init_args(exact_only, f_u_gen_id))).
*/
/*
(defmacro div (a b) `(/ ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1600 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,div,[a,b],['#BQ',[/,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_div, f_u_div, [u_a, u_b], [progn, ['#BQ', [/, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_div, f_u_div, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_div).

/*

### Compiled:  `U::DIV` 
*/
f_u_div(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[/, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_div, classof, claz_macro),
   set_opv(u_div, compile_as, kw_operator),
   set_opv(u_div, function, f_u_div),
   _Ignored4=u_div.
/*
:- side_effect(assert_lsp(u_div,
			  wl:lambda_def(defmacro, u_div, f_u_div, [u_a, u_b], [progn, ['#BQ', [/, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_div,
			  wl:arglist_info(u_div, f_u_div, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_div, wl:init_args(exact_only, f_u_div))).
*/
/*
(defmacro procedure? (x) `(functionp ,x))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1632 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'procedure?',[x],['#BQ',[functionp,['#COMMA',x]]]])
wl:lambda_def(defmacro, u_procedure_c63, f_u_procedure_c63, [u_x], [progn, ['#BQ', [functionp, ['#COMMA', u_x]]]]).
wl:arglist_info(u_procedure_c63, f_u_procedure_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_procedure_c63).

/*

### Compiled:  `U::PROCEDURE?` 
*/
f_u_procedure_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[functionp, X_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_procedure_c63, classof, claz_macro),
   set_opv(u_procedure_c63, compile_as, kw_operator),
   set_opv(u_procedure_c63, function, f_u_procedure_c63),
   _Ignored4=u_procedure_c63.
/*
:- side_effect(assert_lsp(u_procedure_c63,
			  wl:lambda_def(defmacro, u_procedure_c63, f_u_procedure_c63, [u_x], [progn, ['#BQ', [functionp, ['#COMMA', u_x]]]]))).
*/
/*
:- side_effect(assert_lsp(u_procedure_c63,
			  wl:arglist_info(u_procedure_c63, f_u_procedure_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_procedure_c63,
			  wl:init_args(exact_only, f_u_procedure_c63))).
*/
/*
(defmacro number? (x) `(numberp ,x))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1674 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'number?',[x],['#BQ',[numberp,['#COMMA',x]]]])
wl:lambda_def(defmacro, u_number_c63, f_u_number_c63, [u_x], [progn, ['#BQ', [numberp, ['#COMMA', u_x]]]]).
wl:arglist_info(u_number_c63, f_u_number_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_number_c63).

/*

### Compiled:  `U::NUMBER?` 
*/
f_u_number_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[numberp, X_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_number_c63, classof, claz_macro),
   set_opv(u_number_c63, compile_as, kw_operator),
   set_opv(u_number_c63, function, f_u_number_c63),
   _Ignored4=u_number_c63.
/*
:- side_effect(assert_lsp(u_number_c63,
			  wl:lambda_def(defmacro, u_number_c63, f_u_number_c63, [u_x], [progn, ['#BQ', [numberp, ['#COMMA', u_x]]]]))).
*/
/*
:- side_effect(assert_lsp(u_number_c63,
			  wl:arglist_info(u_number_c63, f_u_number_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_number_c63,
			  wl:init_args(exact_only, f_u_number_c63))).
*/
/*
(defmacro flonum? (x) `(floatp ,x))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1711 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'flonum?',[x],['#BQ',[floatp,['#COMMA',x]]]])
wl:lambda_def(defmacro, u_flonum_c63, f_u_flonum_c63, [u_x], [progn, ['#BQ', [floatp, ['#COMMA', u_x]]]]).
wl:arglist_info(u_flonum_c63, f_u_flonum_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_flonum_c63).

/*

### Compiled:  `U::FLONUM?` 
*/
f_u_flonum_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[floatp, X_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_flonum_c63, classof, claz_macro),
   set_opv(u_flonum_c63, compile_as, kw_operator),
   set_opv(u_flonum_c63, function, f_u_flonum_c63),
   _Ignored4=u_flonum_c63.
/*
:- side_effect(assert_lsp(u_flonum_c63,
			  wl:lambda_def(defmacro, u_flonum_c63, f_u_flonum_c63, [u_x], [progn, ['#BQ', [floatp, ['#COMMA', u_x]]]]))).
*/
/*
:- side_effect(assert_lsp(u_flonum_c63,
			  wl:arglist_info(u_flonum_c63, f_u_flonum_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_flonum_c63,
			  wl:init_args(exact_only, f_u_flonum_c63))).
*/
/*
(defmacro fixnum->flonum (x) x)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1747 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fixnum->flonum',[x],x])
wl:lambda_def(defmacro, u_fixnum_c62_flonum, f_u_fixnum_c62_flonum, [u_x], [progn, u_x]).
wl:arglist_info(u_fixnum_c62_flonum, f_u_fixnum_c62_flonum, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fixnum_c62_flonum).

/*

### Compiled:  `U::FIXNUM->FLONUM` 
*/
f_u_fixnum_c62_flonum(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	X_Get=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fixnum_c62_flonum, classof, claz_macro),
   set_opv(u_fixnum_c62_flonum, compile_as, kw_operator),
   set_opv(u_fixnum_c62_flonum, function, f_u_fixnum_c62_flonum),
   _Ignored4=u_fixnum_c62_flonum.
/*
:- side_effect(assert_lsp(u_fixnum_c62_flonum,
			  wl:lambda_def(defmacro, u_fixnum_c62_flonum, f_u_fixnum_c62_flonum, [u_x], [progn, u_x]))).
*/
/*
:- side_effect(assert_lsp(u_fixnum_c62_flonum,
			  wl:arglist_info(u_fixnum_c62_flonum, f_u_fixnum_c62_flonum, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fixnum_c62_flonum,
			  wl:init_args(exact_only, f_u_fixnum_c62_flonum))).
*/
/*
(defmacro symbol? (x) `(symbolp ,x))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1779 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'symbol?',[x],['#BQ',[symbolp,['#COMMA',x]]]])
wl:lambda_def(defmacro, u_symbol_c63, f_u_symbol_c63, [u_x], [progn, ['#BQ', [symbolp, ['#COMMA', u_x]]]]).
wl:arglist_info(u_symbol_c63, f_u_symbol_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_symbol_c63).

/*

### Compiled:  `U::SYMBOL?` 
*/
f_u_symbol_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[symbolp, X_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_symbol_c63, classof, claz_macro),
   set_opv(u_symbol_c63, compile_as, kw_operator),
   set_opv(u_symbol_c63, function, f_u_symbol_c63),
   _Ignored4=u_symbol_c63.
/*
:- side_effect(assert_lsp(u_symbol_c63,
			  wl:lambda_def(defmacro, u_symbol_c63, f_u_symbol_c63, [u_x], [progn, ['#BQ', [symbolp, ['#COMMA', u_x]]]]))).
*/
/*
:- side_effect(assert_lsp(u_symbol_c63,
			  wl:arglist_info(u_symbol_c63, f_u_symbol_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_symbol_c63,
			  wl:init_args(exact_only, f_u_symbol_c63))).
*/
/*
(defmacro pair? (a) `(consp ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1816 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'pair?',[a],['#BQ',[consp,['#COMMA',a]]]])
wl:lambda_def(defmacro, u_pair_c63, f_u_pair_c63, [u_a], [progn, ['#BQ', [consp, ['#COMMA', u_a]]]]).
wl:arglist_info(u_pair_c63, f_u_pair_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_pair_c63).

/*

### Compiled:  `U::PAIR?` 
*/
f_u_pair_c63(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[consp, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_pair_c63, classof, claz_macro),
   set_opv(u_pair_c63, compile_as, kw_operator),
   set_opv(u_pair_c63, function, f_u_pair_c63),
   _Ignored4=u_pair_c63.
/*
:- side_effect(assert_lsp(u_pair_c63,
			  wl:lambda_def(defmacro, u_pair_c63, f_u_pair_c63, [u_a], [progn, ['#BQ', [consp, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_pair_c63,
			  wl:arglist_info(u_pair_c63, f_u_pair_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_pair_c63, wl:init_args(exact_only, f_u_pair_c63))).
*/
/*
(defmacro string? (a) `(stringp ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1849 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string?',[a],['#BQ',[stringp,['#COMMA',a]]]])
wl:lambda_def(defmacro, u_string_c63, f_u_string_c63, [u_a], [progn, ['#BQ', [stringp, ['#COMMA', u_a]]]]).
wl:arglist_info(u_string_c63, f_u_string_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_c63).

/*

### Compiled:  `U::STRING?` 
*/
f_u_string_c63(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[stringp, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_c63, classof, claz_macro),
   set_opv(u_string_c63, compile_as, kw_operator),
   set_opv(u_string_c63, function, f_u_string_c63),
   _Ignored4=u_string_c63.
/*
:- side_effect(assert_lsp(u_string_c63,
			  wl:lambda_def(defmacro, u_string_c63, f_u_string_c63, [u_a], [progn, ['#BQ', [stringp, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_c63,
			  wl:arglist_info(u_string_c63, f_u_string_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_c63,
			  wl:init_args(exact_only, f_u_string_c63))).
*/
/*
(defmacro uppercase? (x) `(upper-case-p ,x))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1886 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'uppercase?',[x],['#BQ',['upper-case-p',['#COMMA',x]]]])
wl:lambda_def(defmacro, u_uppercase_c63, f_u_uppercase_c63, [u_x], [progn, ['#BQ', [upper_case_p, ['#COMMA', u_x]]]]).
wl:arglist_info(u_uppercase_c63, f_u_uppercase_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_uppercase_c63).

/*

### Compiled:  `U::UPPERCASE?` 
*/
f_u_uppercase_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[upper_case_p, X_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_uppercase_c63, classof, claz_macro),
   set_opv(u_uppercase_c63, compile_as, kw_operator),
   set_opv(u_uppercase_c63, function, f_u_uppercase_c63),
   _Ignored4=u_uppercase_c63.
/*
:- side_effect(assert_lsp(u_uppercase_c63,
			  wl:lambda_def(defmacro, u_uppercase_c63, f_u_uppercase_c63, [u_x], [progn, ['#BQ', [upper_case_p, ['#COMMA', u_x]]]]))).
*/
/*
:- side_effect(assert_lsp(u_uppercase_c63,
			  wl:arglist_info(u_uppercase_c63, f_u_uppercase_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_uppercase_c63,
			  wl:init_args(exact_only, f_u_uppercase_c63))).
*/
/*
(defmacro delq! (a b) `(delete ,a ,b))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1931 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'delq!',[a,b],['#BQ',[delete,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_delq_c33, f_u_delq_c33, [u_a, u_b], [progn, ['#BQ', [delete, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_delq_c33, f_u_delq_c33, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_delq_c33).

/*

### Compiled:  `U::DELQ!` 
*/
f_u_delq_c33(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[delete, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_delq_c33, classof, claz_macro),
   set_opv(u_delq_c33, compile_as, kw_operator),
   set_opv(u_delq_c33, function, f_u_delq_c33),
   _Ignored4=u_delq_c33.
/*
:- side_effect(assert_lsp(u_delq_c33,
			  wl:lambda_def(defmacro, u_delq_c33, f_u_delq_c33, [u_a, u_b], [progn, ['#BQ', [delete, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_delq_c33,
			  wl:arglist_info(u_delq_c33, f_u_delq_c33, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_delq_c33, wl:init_args(exact_only, f_u_delq_c33))).
*/
/*
(defun listify (a) (if (listp a) a (list a)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1971 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,listify,[a],[if,[listp,a],a,[list,a]]])
wl:lambda_def(defun, u_listify, f_u_listify, [u_a], [[if, [listp, u_a], u_a, [list, u_a]]]).
wl:arglist_info(u_listify, f_u_listify, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_listify).

/*

### Compiled:  `U::LISTIFY` 
*/
f_u_listify(A, FnResult) :-
	nop(global_env(Env)),
	Env17=[bv(u_a, A)|Env],
	get_var(Env17, u_a, A_Get),
	(   is_listp(A_Get)
	->  get_var(Env17, u_a, A_Get11),
	    FnResult=A_Get11
	;   get_var(Env17, u_a, A_Get12),
	    FnResult=[A_Get12]
	).
:- set_opv(f_u_listify, classof, claz_function),
   set_opv(u_listify, compile_as, kw_function),
   set_opv(u_listify, function, f_u_listify),
   _Ignored4=u_listify.
/*
:- side_effect(assert_lsp(u_listify,
			  wl:lambda_def(defun, u_listify, f_u_listify, [u_a], [[if, [listp, u_a], u_a, [list, u_a]]]))).
*/
/*
:- side_effect(assert_lsp(u_listify,
			  wl:arglist_info(u_listify, f_u_listify, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_listify, wl:init_args(exact_only, f_u_listify))).
*/
/*
(defmacro append! (a b) `(nconc (listify ,a) (listify ,b)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2017 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'append!',[a,b],['#BQ',[nconc,[listify,['#COMMA',a]],[listify,['#COMMA',b]]]]])
wl:lambda_def(defmacro, u_append_c33, f_u_append_c33, [u_a, u_b], [progn, ['#BQ', [nconc, [u_listify, ['#COMMA', u_a]], [u_listify, ['#COMMA', u_b]]]]]).
wl:arglist_info(u_append_c33, f_u_append_c33, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_append_c33).

/*

### Compiled:  `U::APPEND!` 
*/
f_u_append_c33(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[nconc, [u_listify, A_Get], [u_listify, B_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_append_c33, classof, claz_macro),
   set_opv(u_append_c33, compile_as, kw_operator),
   set_opv(u_append_c33, function, f_u_append_c33),
   _Ignored4=u_append_c33.
/*
:- side_effect(assert_lsp(u_append_c33,
			  wl:lambda_def(defmacro, u_append_c33, f_u_append_c33, [u_a, u_b], [progn, ['#BQ', [nconc, [u_listify, ['#COMMA', u_a]], [u_listify, ['#COMMA', u_b]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_append_c33,
			  wl:arglist_info(u_append_c33, f_u_append_c33, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_append_c33,
			  wl:init_args(exact_only, f_u_append_c33))).
*/
/*
(defmacro ascii->char (x) `(code-char ,x))

;; #-abcl 

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2077 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ascii->char',[x],['#BQ',['code-char',['#COMMA',x]]]])
wl:lambda_def(defmacro, u_ascii_c62_char, f_u_ascii_c62_char, [u_x], [progn, ['#BQ', [code_char, ['#COMMA', u_x]]]]).
wl:arglist_info(u_ascii_c62_char, f_u_ascii_c62_char, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ascii_c62_char).

/*

### Compiled:  `U::ASCII->CHAR` 
*/
f_u_ascii_c62_char(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[code_char, X_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ascii_c62_char, classof, claz_macro),
   set_opv(u_ascii_c62_char, compile_as, kw_operator),
   set_opv(u_ascii_c62_char, function, f_u_ascii_c62_char),
   _Ignored4=u_ascii_c62_char.
/*
:- side_effect(assert_lsp(u_ascii_c62_char,
			  wl:lambda_def(defmacro, u_ascii_c62_char, f_u_ascii_c62_char, [u_x], [progn, ['#BQ', [code_char, ['#COMMA', u_x]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ascii_c62_char,
			  wl:arglist_info(u_ascii_c62_char, f_u_ascii_c62_char, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ascii_c62_char,
			  wl:init_args(exact_only, f_u_ascii_c62_char))).
*/
/*
; #-abcl 
*/
/*
(defmacro assq (a b) `(assoc ,a ,b))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2133 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,assq,[a,b],['#BQ',[assoc,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, ext_assq, f_ext_assq, [u_a, u_b], [progn, ['#BQ', [assoc, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(ext_assq, f_ext_assq, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_ext_assq).

/*

### Compiled:  `EXT:ASSQ` 
*/
f_ext_assq(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[assoc, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_assq, classof, claz_macro),
   set_opv(ext_assq, compile_as, kw_operator),
   set_opv(ext_assq, function, f_ext_assq),
   _Ignored4=ext_assq.
/*
:- side_effect(assert_lsp(ext_assq,
			  wl:lambda_def(defmacro, ext_assq, f_ext_assq, [u_a, u_b], [progn, ['#BQ', [assoc, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(ext_assq,
			  wl:arglist_info(ext_assq, f_ext_assq, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(ext_assq, wl:init_args(exact_only, f_ext_assq))).
*/
/*
(defmacro increment-me (a) `(setq ,a (+ ,a 1)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2171 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'increment-me',[a],['#BQ',[setq,['#COMMA',a],[+,['#COMMA',a],1]]]])
wl:lambda_def(defmacro, u_increment_me, f_u_increment_me, [u_a], [progn, ['#BQ', [setq, ['#COMMA', u_a], [+, ['#COMMA', u_a], 1]]]]).
wl:arglist_info(u_increment_me, f_u_increment_me, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_increment_me).

/*

### Compiled:  `U::INCREMENT-ME` 
*/
f_u_increment_me(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get8),
	[setq, A_Get8, [+, A_Get8, 1]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_increment_me, classof, claz_macro),
   set_opv(u_increment_me, compile_as, kw_operator),
   set_opv(u_increment_me, function, f_u_increment_me),
   _Ignored4=u_increment_me.
/*
:- side_effect(assert_lsp(u_increment_me,
			  wl:lambda_def(defmacro, u_increment_me, f_u_increment_me, [u_a], [progn, ['#BQ', [setq, ['#COMMA', u_a], [+, ['#COMMA', u_a], 1]]]]))).
*/
/*
:- side_effect(assert_lsp(u_increment_me,
			  wl:arglist_info(u_increment_me, f_u_increment_me, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_increment_me,
			  wl:init_args(exact_only, f_u_increment_me))).
*/
/*
(defmacro string-posq (a b) `(position ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2219 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'string-posq',[a,b],['#BQ',[position,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_string_posq, f_u_string_posq, [u_a, u_b], [progn, ['#BQ', [position, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_string_posq, f_u_string_posq, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_posq).

/*

### Compiled:  `U::STRING-POSQ` 
*/
f_u_string_posq(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[position, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_posq, classof, claz_macro),
   set_opv(u_string_posq, compile_as, kw_operator),
   set_opv(u_string_posq, function, f_u_string_posq),
   _Ignored4=u_string_posq.
/*
:- side_effect(assert_lsp(u_string_posq,
			  wl:lambda_def(defmacro, u_string_posq, f_u_string_posq, [u_a, u_b], [progn, ['#BQ', [position, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_posq,
			  wl:arglist_info(u_string_posq, f_u_string_posq, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_posq,
			  wl:init_args(exact_only, f_u_string_posq))).
*/
/*
(defmacro nth-elem (a b) `(nth ,b ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2266 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'nth-elem',[a,b],['#BQ',[nth,['#COMMA',b],['#COMMA',a]]]])
wl:lambda_def(defmacro, u_nth_elem, f_u_nth_elem, [u_a, u_b], [progn, ['#BQ', [nth, ['#COMMA', u_b], ['#COMMA', u_a]]]]).
wl:arglist_info(u_nth_elem, f_u_nth_elem, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_nth_elem).

/*

### Compiled:  `U::NTH-ELEM` 
*/
f_u_nth_elem(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[nth, B_Get, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nth_elem, classof, claz_macro),
   set_opv(u_nth_elem, compile_as, kw_operator),
   set_opv(u_nth_elem, function, f_u_nth_elem),
   _Ignored4=u_nth_elem.
/*
:- side_effect(assert_lsp(u_nth_elem,
			  wl:lambda_def(defmacro, u_nth_elem, f_u_nth_elem, [u_a, u_b], [progn, ['#BQ', [nth, ['#COMMA', u_b], ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_nth_elem,
			  wl:arglist_info(u_nth_elem, f_u_nth_elem, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_nth_elem, wl:init_args(exact_only, f_u_nth_elem))).
*/
/*
(defmacro newline (a) `(terpri ,a))
;(defmacro -1+ (a) `(+ -1 ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2305 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,newline,[a],['#BQ',[terpri,['#COMMA',a]]]])
wl:lambda_def(defmacro, u_newline, f_u_newline, [u_a], [progn, ['#BQ', [terpri, ['#COMMA', u_a]]]]).
wl:arglist_info(u_newline, f_u_newline, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_newline).

/*

### Compiled:  `U::NEWLINE` 
*/
f_u_newline(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[terpri, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_newline, classof, claz_macro),
   set_opv(u_newline, compile_as, kw_operator),
   set_opv(u_newline, function, f_u_newline),
   _Ignored4=u_newline.
/*
:- side_effect(assert_lsp(u_newline,
			  wl:lambda_def(defmacro, u_newline, f_u_newline, [u_a], [progn, ['#BQ', [terpri, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_newline,
			  wl:arglist_info(u_newline, f_u_newline, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_newline, wl:init_args(exact_only, f_u_newline))).
*/
/*
(defmacro -1+ (a) `(+ -1 ,a))
*/
/*
(defmacro fl+ (a b) `(+ ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2372 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl+',[a,b],['#BQ',[+,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_c43, f_u_fl_c43, [u_a, u_b], [progn, ['#BQ', [+, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_c43, f_u_fl_c43, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_c43).

/*

### Compiled:  `U::FL+` 
*/
f_u_fl_c43(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[+, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c43, classof, claz_macro),
   set_opv(u_fl_c43, compile_as, kw_operator),
   set_opv(u_fl_c43, function, f_u_fl_c43),
   _Ignored4=u_fl_c43.
/*
:- side_effect(assert_lsp(u_fl_c43,
			  wl:lambda_def(defmacro, u_fl_c43, f_u_fl_c43, [u_a, u_b], [progn, ['#BQ', [+, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_c43,
			  wl:arglist_info(u_fl_c43, f_u_fl_c43, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_c43, wl:init_args(exact_only, f_u_fl_c43))).
*/
/*
(defmacro fl- (a b) `(- ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2404 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl-',[a,b],['#BQ',[-,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_flc45, f_u_flc45, [u_a, u_b], [progn, ['#BQ', [-, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_flc45, f_u_flc45, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_flc45).

/*

### Compiled:  `U::FL-` 
*/
f_u_flc45(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[-, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_flc45, classof, claz_macro),
   set_opv(u_flc45, compile_as, kw_operator),
   set_opv(u_flc45, function, f_u_flc45),
   _Ignored4=u_flc45.
/*
:- side_effect(assert_lsp(u_flc45,
			  wl:lambda_def(defmacro, u_flc45, f_u_flc45, [u_a, u_b], [progn, ['#BQ', [-, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_flc45,
			  wl:arglist_info(u_flc45, f_u_flc45, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_flc45, wl:init_args(exact_only, f_u_flc45))).
*/
/*
(defmacro fl* (a b) `(* ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2436 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl*',[a,b],['#BQ',[*,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_xx, f_u_fl_xx, [u_a, u_b], [progn, ['#BQ', [*, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_xx, f_u_fl_xx, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_xx).

/*

### Compiled:  `U::FL*` 
*/
f_u_fl_xx(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[*, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_xx, classof, claz_macro),
   set_opv(u_fl_xx, compile_as, kw_operator),
   set_opv(u_fl_xx, function, f_u_fl_xx),
   _Ignored4=u_fl_xx.
/*
:- side_effect(assert_lsp(u_fl_xx,
			  wl:lambda_def(defmacro, u_fl_xx, f_u_fl_xx, [u_a, u_b], [progn, ['#BQ', [*, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_xx,
			  wl:arglist_info(u_fl_xx, f_u_fl_xx, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_xx, wl:init_args(exact_only, f_u_fl_xx))).
*/
/*
(defmacro fl/ (a b) `(/ ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2468 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl/',[a,b],['#BQ',[/,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_c47, f_u_fl_c47, [u_a, u_b], [progn, ['#BQ', [/, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_c47, f_u_fl_c47, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_c47).

/*

### Compiled:  `U::FL/` 
*/
f_u_fl_c47(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[/, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c47, classof, claz_macro),
   set_opv(u_fl_c47, compile_as, kw_operator),
   set_opv(u_fl_c47, function, f_u_fl_c47),
   _Ignored4=u_fl_c47.
/*
:- side_effect(assert_lsp(u_fl_c47,
			  wl:lambda_def(defmacro, u_fl_c47, f_u_fl_c47, [u_a, u_b], [progn, ['#BQ', [/, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_c47,
			  wl:arglist_info(u_fl_c47, f_u_fl_c47, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_c47, wl:init_args(exact_only, f_u_fl_c47))).
*/
/*
(defmacro fl< (a b) `(< ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2500 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl<',[a,b],['#BQ',[<,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_c60, f_u_fl_c60, [u_a, u_b], [progn, ['#BQ', [<, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_c60, f_u_fl_c60, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_c60).

/*

### Compiled:  `U::FL<` 
*/
f_u_fl_c60(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[<, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c60, classof, claz_macro),
   set_opv(u_fl_c60, compile_as, kw_operator),
   set_opv(u_fl_c60, function, f_u_fl_c60),
   _Ignored4=u_fl_c60.
/*
:- side_effect(assert_lsp(u_fl_c60,
			  wl:lambda_def(defmacro, u_fl_c60, f_u_fl_c60, [u_a, u_b], [progn, ['#BQ', [<, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_c60,
			  wl:arglist_info(u_fl_c60, f_u_fl_c60, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_c60, wl:init_args(exact_only, f_u_fl_c60))).
*/
/*
(defmacro fl> (a b) `(> ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2532 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl>',[a,b],['#BQ',[>,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_c62, f_u_fl_c62, [u_a, u_b], [progn, ['#BQ', [>, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_c62, f_u_fl_c62, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_c62).

/*

### Compiled:  `U::FL>` 
*/
f_u_fl_c62(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[>, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c62, classof, claz_macro),
   set_opv(u_fl_c62, compile_as, kw_operator),
   set_opv(u_fl_c62, function, f_u_fl_c62),
   _Ignored4=u_fl_c62.
/*
:- side_effect(assert_lsp(u_fl_c62,
			  wl:lambda_def(defmacro, u_fl_c62, f_u_fl_c62, [u_a, u_b], [progn, ['#BQ', [>, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_c62,
			  wl:arglist_info(u_fl_c62, f_u_fl_c62, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_c62, wl:init_args(exact_only, f_u_fl_c62))).
*/
/*
(defmacro fl>= (a b) `(>= ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2564 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl>=',[a,b],['#BQ',[>=,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_c62_c61, f_u_fl_c62_c61, [u_a, u_b], [progn, ['#BQ', [>=, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_c62_c61, f_u_fl_c62_c61, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_c62_c61).

/*

### Compiled:  `U::FL>=` 
*/
f_u_fl_c62_c61(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[>=, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c62_c61, classof, claz_macro),
   set_opv(u_fl_c62_c61, compile_as, kw_operator),
   set_opv(u_fl_c62_c61, function, f_u_fl_c62_c61),
   _Ignored4=u_fl_c62_c61.
/*
:- side_effect(assert_lsp(u_fl_c62_c61,
			  wl:lambda_def(defmacro, u_fl_c62_c61, f_u_fl_c62_c61, [u_a, u_b], [progn, ['#BQ', [>=, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_c62_c61,
			  wl:arglist_info(u_fl_c62_c61, f_u_fl_c62_c61, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_c62_c61,
			  wl:init_args(exact_only, f_u_fl_c62_c61))).
*/
/*
(defmacro fl<= (a b) `(<= ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2598 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl<=',[a,b],['#BQ',[<=,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_c60_c61, f_u_fl_c60_c61, [u_a, u_b], [progn, ['#BQ', [<=, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_c60_c61, f_u_fl_c60_c61, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_c60_c61).

/*

### Compiled:  `U::FL<=` 
*/
f_u_fl_c60_c61(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[<=, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c60_c61, classof, claz_macro),
   set_opv(u_fl_c60_c61, compile_as, kw_operator),
   set_opv(u_fl_c60_c61, function, f_u_fl_c60_c61),
   _Ignored4=u_fl_c60_c61.
/*
:- side_effect(assert_lsp(u_fl_c60_c61,
			  wl:lambda_def(defmacro, u_fl_c60_c61, f_u_fl_c60_c61, [u_a, u_b], [progn, ['#BQ', [<=, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_c60_c61,
			  wl:arglist_info(u_fl_c60_c61, f_u_fl_c60_c61, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_c60_c61,
			  wl:init_args(exact_only, f_u_fl_c60_c61))).
*/
/*
(defmacro fl= (a b) `(> ,a ,b))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2632 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fl=',[a,b],['#BQ',[>,['#COMMA',a],['#COMMA',b]]]])
wl:lambda_def(defmacro, u_fl_c61, f_u_fl_c61, [u_a, u_b], [progn, ['#BQ', [>, ['#COMMA', u_a], ['#COMMA', u_b]]]]).
wl:arglist_info(u_fl_c61, f_u_fl_c61, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fl_c61).

/*

### Compiled:  `U::FL=` 
*/
f_u_fl_c61(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[>, A_Get, B_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c61, classof, claz_macro),
   set_opv(u_fl_c61, compile_as, kw_operator),
   set_opv(u_fl_c61, function, f_u_fl_c61),
   _Ignored4=u_fl_c61.
/*
:- side_effect(assert_lsp(u_fl_c61,
			  wl:lambda_def(defmacro, u_fl_c61, f_u_fl_c61, [u_a, u_b], [progn, ['#BQ', [>, ['#COMMA', u_a], ['#COMMA', u_b]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fl_c61,
			  wl:arglist_info(u_fl_c61, f_u_fl_c61, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fl_c61, wl:init_args(exact_only, f_u_fl_c61))).
*/
/*
(defmacro file-exists? (a) `(probe-file ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2664 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'file-exists?',[a],['#BQ',['probe-file',['#COMMA',a]]]])
wl:lambda_def(defmacro, u_file_exists_c63, f_u_file_exists_c63, [u_a], [progn, ['#BQ', [probe_file, ['#COMMA', u_a]]]]).
wl:arglist_info(u_file_exists_c63, f_u_file_exists_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_file_exists_c63).

/*

### Compiled:  `U::FILE-EXISTS?` 
*/
f_u_file_exists_c63(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[probe_file, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_file_exists_c63, classof, claz_macro),
   set_opv(u_file_exists_c63, compile_as, kw_operator),
   set_opv(u_file_exists_c63, function, f_u_file_exists_c63),
   _Ignored4=u_file_exists_c63.
/*
:- side_effect(assert_lsp(u_file_exists_c63,
			  wl:lambda_def(defmacro, u_file_exists_c63, f_u_file_exists_c63, [u_a], [progn, ['#BQ', [probe_file, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_file_exists_c63,
			  wl:arglist_info(u_file_exists_c63, f_u_file_exists_c63, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_file_exists_c63,
			  wl:init_args(exact_only, f_u_file_exists_c63))).
*/
/*
(defmacro comment (a) nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2709 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,comment,[a],[]])
wl:lambda_def(defmacro, u_comment, f_u_comment, [u_a], [progn, []]).
wl:arglist_info(u_comment, f_u_comment, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_comment).

/*

### Compiled:  `U::COMMENT` 
*/
f_u_comment(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	[]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_comment, classof, claz_macro),
   set_opv(u_comment, compile_as, kw_operator),
   set_opv(u_comment, function, f_u_comment),
   _Ignored4=u_comment.
/*
:- side_effect(assert_lsp(u_comment,
			  wl:lambda_def(defmacro, u_comment, f_u_comment, [u_a], [progn, []]))).
*/
/*
:- side_effect(assert_lsp(u_comment,
			  wl:arglist_info(u_comment, f_u_comment, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_comment, wl:init_args(exact_only, f_u_comment))).
*/
/*
(defmacro mem? (a b c) `(t-or-nil (member ,b ,c :test ,a)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2736 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'mem?',[a,b,c],['#BQ',['t-or-nil',[member,['#COMMA',b],['#COMMA',c],':test',['#COMMA',a]]]]])
wl:lambda_def(defmacro, u_mem_c63, f_u_mem_c63, [u_a, u_b, u_c], [progn, ['#BQ', [u_t_or_nil, [member, ['#COMMA', u_b], ['#COMMA', u_c], kw_test, ['#COMMA', u_a]]]]]).
wl:arglist_info(u_mem_c63, f_u_mem_c63, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mem_c63).

/*

### Compiled:  `U::MEM?` 
*/
f_u_mem_c63(A, B, C, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B), bv(u_c, C)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	get_var(Env, u_c, C_Get),
	[u_t_or_nil, [member, B_Get, C_Get, kw_test, A_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_c63, classof, claz_macro),
   set_opv(u_mem_c63, compile_as, kw_operator),
   set_opv(u_mem_c63, function, f_u_mem_c63),
   _Ignored4=u_mem_c63.
/*
:- side_effect(assert_lsp(u_mem_c63,
			  wl:lambda_def(defmacro, u_mem_c63, f_u_mem_c63, [u_a, u_b, u_c], [progn, ['#BQ', [u_t_or_nil, [member, ['#COMMA', u_b], ['#COMMA', u_c], kw_test, ['#COMMA', u_a]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_mem_c63,
			  wl:arglist_info(u_mem_c63, f_u_mem_c63, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mem_c63, wl:init_args(exact_only, f_u_mem_c63))).
*/
/*
(defmacro mem (a b c) `(member ,b ,c :test ,a))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2796 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,mem,[a,b,c],['#BQ',[member,['#COMMA',b],['#COMMA',c],':test',['#COMMA',a]]]])
wl:lambda_def(defmacro, u_mem, f_u_mem, [u_a, u_b, u_c], [progn, ['#BQ', [member, ['#COMMA', u_b], ['#COMMA', u_c], kw_test, ['#COMMA', u_a]]]]).
wl:arglist_info(u_mem, f_u_mem, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mem).

/*

### Compiled:  `U::MEM` 
*/
f_u_mem(A, B, C, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B), bv(u_c, C)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	get_var(Env, u_c, C_Get),
	[member, B_Get, C_Get, kw_test, A_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem, classof, claz_macro),
   set_opv(u_mem, compile_as, kw_operator),
   set_opv(u_mem, function, f_u_mem),
   _Ignored4=u_mem.
/*
:- side_effect(assert_lsp(u_mem,
			  wl:lambda_def(defmacro, u_mem, f_u_mem, [u_a, u_b, u_c], [progn, ['#BQ', [member, ['#COMMA', u_b], ['#COMMA', u_c], kw_test, ['#COMMA', u_a]]]]))).
*/
/*
:- side_effect(assert_lsp(u_mem,
			  wl:arglist_info(u_mem, f_u_mem, [u_a, u_b, u_c], arginfo{all:[u_a, u_b, u_c], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b, u_c], opt:0, req:[u_a, u_b, u_c], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mem, wl:init_args(exact_only, f_u_mem))).
*/
/*
(defmacro every? (a b) `(t-or-nil (every ,a ,b)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2844 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'every?',[a,b],['#BQ',['t-or-nil',[every,['#COMMA',a],['#COMMA',b]]]]])
wl:lambda_def(defmacro, u_every_c63, f_u_every_c63, [u_a, u_b], [progn, ['#BQ', [u_t_or_nil, [every, ['#COMMA', u_a], ['#COMMA', u_b]]]]]).
wl:arglist_info(u_every_c63, f_u_every_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_every_c63).

/*

### Compiled:  `U::EVERY?` 
*/
f_u_every_c63(A, B, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A), bv(u_b, B)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	get_var(Env, u_b, B_Get),
	[u_t_or_nil, [every, A_Get, B_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_every_c63, classof, claz_macro),
   set_opv(u_every_c63, compile_as, kw_operator),
   set_opv(u_every_c63, function, f_u_every_c63),
   _Ignored4=u_every_c63.
/*
:- side_effect(assert_lsp(u_every_c63,
			  wl:lambda_def(defmacro, u_every_c63, f_u_every_c63, [u_a, u_b], [progn, ['#BQ', [u_t_or_nil, [every, ['#COMMA', u_a], ['#COMMA', u_b]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_every_c63,
			  wl:arglist_info(u_every_c63, f_u_every_c63, [u_a, u_b], arginfo{all:[u_a, u_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a, u_b], opt:0, req:[u_a, u_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_every_c63, wl:init_args(exact_only, f_u_every_c63))).
*/
/*
(defmacro tlast (a) `(car (last ,a 1)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2894 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,tlast,[a],['#BQ',[car,[last,['#COMMA',a],1]]]])
wl:lambda_def(defmacro, u_tlast, f_u_tlast, [u_a], [progn, ['#BQ', [car, [last, ['#COMMA', u_a], 1]]]]).
wl:arglist_info(u_tlast, f_u_tlast, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_tlast).

/*

### Compiled:  `U::TLAST` 
*/
f_u_tlast(A, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_a, A)|Global_env_Ret],
	get_var(Env, u_a, A_Get),
	[car, [last, A_Get, 1]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_tlast, classof, claz_macro),
   set_opv(u_tlast, compile_as, kw_operator),
   set_opv(u_tlast, function, f_u_tlast),
   _Ignored4=u_tlast.
/*
:- side_effect(assert_lsp(u_tlast,
			  wl:lambda_def(defmacro, u_tlast, f_u_tlast, [u_a], [progn, ['#BQ', [car, [last, ['#COMMA', u_a], 1]]]]))).
*/
/*
:- side_effect(assert_lsp(u_tlast,
			  wl:arglist_info(u_tlast, f_u_tlast, [u_a], arginfo{all:[u_a], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_a], opt:0, req:[u_a], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_tlast, wl:init_args(exact_only, f_u_tlast))).
*/
/*
(defun standard-input () *standard-input*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2934 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'standard-input',[],'*standard-input*'])
wl:lambda_def(defun, u_standard_input, f_u_standard_input, [], [xx_standard_input_xx]).
wl:arglist_info(u_standard_input, f_u_standard_input, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_standard_input).

/*

### Compiled:  `U::STANDARD-INPUT` 
*/
f_u_standard_input(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, xx_standard_input_xx, Xx_standard_input_xx_Get),
	Xx_standard_input_xx_Get=FnResult.
:- set_opv(f_u_standard_input, classof, claz_function),
   set_opv(u_standard_input, compile_as, kw_function),
   set_opv(u_standard_input, function, f_u_standard_input),
   _Ignored4=u_standard_input.
/*
:- side_effect(assert_lsp(u_standard_input,
			  wl:lambda_def(defun, u_standard_input, f_u_standard_input, [], [xx_standard_input_xx]))).
*/
/*
:- side_effect(assert_lsp(u_standard_input,
			  wl:arglist_info(u_standard_input, f_u_standard_input, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_standard_input,
			  wl:init_args(exact_only, f_u_standard_input))).
*/
/*
(defun standard-output () *standard-output*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2977 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'standard-output',[],'*standard-output*'])
wl:lambda_def(defun, u_standard_output, f_u_standard_output, [], [xx_standard_output_xx]).
wl:arglist_info(u_standard_output, f_u_standard_output, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_standard_output).

/*

### Compiled:  `U::STANDARD-OUTPUT` 
*/
f_u_standard_output(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, xx_standard_output_xx, Xx_standard_output_xx_Get),
	Xx_standard_output_xx_Get=FnResult.
:- set_opv(f_u_standard_output, classof, claz_function),
   set_opv(u_standard_output, compile_as, kw_function),
   set_opv(u_standard_output, function, f_u_standard_output),
   _Ignored4=u_standard_output.
/*
:- side_effect(assert_lsp(u_standard_output,
			  wl:lambda_def(defun, u_standard_output, f_u_standard_output, [], [xx_standard_output_xx]))).
*/
/*
:- side_effect(assert_lsp(u_standard_output,
			  wl:arglist_info(u_standard_output, f_u_standard_output, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_standard_output,
			  wl:init_args(exact_only, f_u_standard_output))).
*/
/*
(defun string-head (x) (char x 0))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:3022 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'string-head',[x],[char,x,0]])
wl:lambda_def(defun, u_string_head, f_u_string_head, [u_x], [[char, u_x, 0]]).
wl:arglist_info(u_string_head, f_u_string_head, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_string_head).

/*

### Compiled:  `U::STRING-HEAD` 
*/
f_u_string_head(X, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_x, X)|Env],
	get_var(Env10, u_x, X_Get),
	cl_char(X_Get, 0, Char_Ret),
	Char_Ret=FnResult.
:- set_opv(f_u_string_head, classof, claz_function),
   set_opv(u_string_head, compile_as, kw_function),
   set_opv(u_string_head, function, f_u_string_head),
   _Ignored4=u_string_head.
/*
:- side_effect(assert_lsp(u_string_head,
			  wl:lambda_def(defun, u_string_head, f_u_string_head, [u_x], [[char, u_x, 0]]))).
*/
/*
:- side_effect(assert_lsp(u_string_head,
			  wl:arglist_info(u_string_head, f_u_string_head, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_head,
			  wl:init_args(exact_only, f_u_string_head))).
*/
/*
(defun walkcdr (fn x)
  (yloop (initial (rest x))
         (ywhile rest)
         (ydo (apply fn (list rest))
              (setq rest (cdr rest)))))

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:3057 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,walkcdr,[fn,x],[yloop,[initial,[rest,x]],[ywhile,rest],[ydo,[apply,fn,[list,rest]],[setq,rest,[cdr,rest]]]]])
wl:lambda_def(defun, u_walkcdr, f_u_walkcdr, [u_fn, u_x], [[u_yloop, [u_initial, [rest, u_x]], [u_ywhile, rest], [u_ydo, [apply, u_fn, [list, rest]], [setq, rest, [cdr, rest]]]]]).
wl:arglist_info(u_walkcdr, f_u_walkcdr, [u_fn, u_x], arginfo{all:[u_fn, u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_fn, u_x], opt:0, req:[u_fn, u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_walkcdr).

/*

### Compiled:  `U::WALKCDR` 
*/
f_u_walkcdr(Fn, X, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_fn, Fn), bv(u_x, X)|Env],
	get_var(AEnv, u_x, X_Get),
	cl_cdr(X_Get, Initial_Param),
	f_u_initial(Initial_Param, Yloop_Param),
	get_var(AEnv, rest, Rest_Get),
	f_u_ywhile(Rest_Get, Ywhile_Ret),
	get_var(AEnv, rest, Rest_Get10),
	get_var(AEnv, u_fn, Fn_Get),
	cl_apply(Fn_Get, [Rest_Get10], Ydo_Param),
	get_var(AEnv, rest, Rest_Get12),
	cl_cdr(Rest_Get12, Rest),
	set_var(AEnv, rest, Rest),
	f_u_ydo(Ydo_Param, Rest, Ydo_Ret),
	f_u_yloop(Yloop_Param, Ywhile_Ret, Ydo_Ret, Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_walkcdr, classof, claz_function),
   set_opv(u_walkcdr, compile_as, kw_function),
   set_opv(u_walkcdr, function, f_u_walkcdr),
   _Ignored4=u_walkcdr.
/*
:- side_effect(assert_lsp(u_walkcdr,
			  wl:lambda_def(defun, u_walkcdr, f_u_walkcdr, [u_fn, u_x], [[u_yloop, [u_initial, [rest, u_x]], [u_ywhile, rest], [u_ydo, [apply, u_fn, [list, rest]], [setq, rest, [cdr, rest]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_walkcdr,
			  wl:arglist_info(u_walkcdr, f_u_walkcdr, [u_fn, u_x], arginfo{all:[u_fn, u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_fn, u_x], opt:0, req:[u_fn, u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_walkcdr, wl:init_args(exact_only, f_u_walkcdr))).
*/
/*
 End of file.
*/


%; Total compilation time: 5.584 seconds


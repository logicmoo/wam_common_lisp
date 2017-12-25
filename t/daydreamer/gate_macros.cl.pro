#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_macros" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:14:33 2017

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
*******************************************************************************
*/
/*
(defmacro fixnum->string (n) `(format nil ""(defmacro fixnum->string (n) `(format nil \"~A\" ,n))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:282 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'fixnum->string',[n],['#BQ',[format,[],'$STRING'("~A"),['#COMMA',n]]]])
wl:lambda_def(defmacro, u_fixnum_c62_string, f_u_fixnum_c62_string, [n], [progn, ['#BQ', [format, [], '$ARRAY'([*], claz_base_character, "~A"), ['#COMMA', n]]]]).
wl:arglist_info(u_fixnum_c62_string, f_u_fixnum_c62_string, [n], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fixnum_c62_string).

/*

### Compiled:  `U::FIXNUM->STRING` 
*/
f_u_fixnum_c62_string(N, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(n, N)|Global_env_Ret],
	get_var(Env, n, N_Get),
	[format, [], '$ARRAY'([*], claz_base_character, "~A"), N_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fixnum_c62_string, classof, claz_macro),
   set_opv(u_fixnum_c62_string, compile_as, kw_operator),
   set_opv(u_fixnum_c62_string, function, f_u_fixnum_c62_string),
   _Ignored4=u_fixnum_c62_string.
/*
:- side_effect(assert_lsp(u_fixnum_c62_string,
			  wl:lambda_def(defmacro, u_fixnum_c62_string, f_u_fixnum_c62_string, [n], [progn, ['#BQ', [format, [], '$ARRAY'([*], claz_base_character, "~A"), ['#COMMA', n]]]]))).
*/
/*
:- side_effect(assert_lsp(u_fixnum_c62_string,
			  wl:arglist_info(u_fixnum_c62_string, f_u_fixnum_c62_string, [n], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fixnum_c62_string,
			  wl:init_args(exact_only, f_u_fixnum_c62_string))).
*/
/*
(defmacro dbg (dbg-var . rest)
  `(progn
    (cond
     ((eq? ,dbg-var t)
      (format t ,@rest))
     (,dbg-var
      (format ,dbg-var ,@rest))
     (else nil))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:336 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,dbg,['dbg-var'|rest],['#BQ',[progn,[cond,[['eq?',['#COMMA','dbg-var'],t],[format,t,['#BQ-COMMA-ELIPSE',rest]]],[['#COMMA','dbg-var'],[format,['#COMMA','dbg-var'],['#BQ-COMMA-ELIPSE',rest]]],[else,[]]]]]])
wl:lambda_def(defmacro, u_dbg, f_u_dbg, [u_dbg_var|rest], [progn, ['#BQ', [progn, [cond, [[u_eq_c63, ['#COMMA', u_dbg_var], t], [format, t, ['#BQ-COMMA-ELIPSE', rest]]], [['#COMMA', u_dbg_var], [format, ['#COMMA', u_dbg_var], ['#BQ-COMMA-ELIPSE', rest]]], [u_else, []]]]]]).
wl:arglist_info(u_dbg, f_u_dbg, [u_dbg_var, '&rest', rest], arginfo{all:[u_dbg_var], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_dbg_var, rest], opt:0, req:[u_dbg_var], rest:[rest], sublists:0, whole:0}).
wl: init_args(1, f_u_dbg).

/*

### Compiled:  `U::DBG` 
*/
f_u_dbg(Dbg_var, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_dbg_var, Dbg_var), bv(rest, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, rest, Rest_Get12),
	get_var(GEnv, u_dbg_var, Dbg_var_Get10),
	[progn, [cond, [[u_eq_c63, Dbg_var_Get10, t], [format, t|Rest_Get12]], [Dbg_var_Get10, [format, Dbg_var_Get10|Rest_Get12]], [u_else, []]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_dbg, classof, claz_macro),
   set_opv(u_dbg, compile_as, kw_operator),
   set_opv(u_dbg, function, f_u_dbg),
   _Ignored4=u_dbg.
/*
:- side_effect(assert_lsp(u_dbg,
			  wl:lambda_def(defmacro, u_dbg, f_u_dbg, [u_dbg_var|rest], [progn, ['#BQ', [progn, [cond, [[u_eq_c63, ['#COMMA', u_dbg_var], t], [format, t, ['#BQ-COMMA-ELIPSE', rest]]], [['#COMMA', u_dbg_var], [format, ['#COMMA', u_dbg_var], ['#BQ-COMMA-ELIPSE', rest]]], [u_else, []]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_dbg,
			  wl:arglist_info(u_dbg, f_u_dbg, [u_dbg_var, '&rest', rest], arginfo{all:[u_dbg_var], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_dbg_var, rest], opt:0, req:[u_dbg_var], rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_dbg, wl:init_args(1, f_u_dbg))).
*/
/*
(defmacro if-interested-in (key . rest)
  `(if (and (assq ',key *ndbg-interests*)
            (or (memq? 'all (assq ',key *ndbg-interests*))
                (any? (lambda (x) (memq? x *ndbg-items*))
                      (cdr (assq ',key *ndbg-interests*)))))
       (progn ,@rest)
       nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:502 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'if-interested-in',[key|rest],['#BQ',[if,[and,[assq,[quote,['#COMMA',key]],'*ndbg-interests*'],[or,['memq?',[quote,all],[assq,[quote,['#COMMA',key]],'*ndbg-interests*']],['any?',[lambda,[x],['memq?',x,'*ndbg-items*']],[cdr,[assq,[quote,['#COMMA',key]],'*ndbg-interests*']]]]],[progn,['#BQ-COMMA-ELIPSE',rest]],[]]]])
wl:lambda_def(defmacro, u_if_interested_in, f_u_if_interested_in, [key|rest], [progn, ['#BQ', [if, [and, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]]]]], [progn, ['#BQ-COMMA-ELIPSE', rest]], []]]]).
wl:arglist_info(u_if_interested_in, f_u_if_interested_in, [key, '&rest', rest], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[key, rest], opt:0, req:[key], rest:[rest], sublists:0, whole:0}).
wl: init_args(1, f_u_if_interested_in).

/*

### Compiled:  `U::IF-INTERESTED-IN` 
*/
f_u_if_interested_in(Key, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(key, Key), bv(rest, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, key, Key_Get9),
	get_var(GEnv, rest, Rest_Get),
	[if, [and, [ext_assq, [quote, Key_Get9], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, Key_Get9], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, Key_Get9], u_xx_ndbg_interests_xx]]]]], [progn|Rest_Get], []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_if_interested_in, classof, claz_macro),
   set_opv(u_if_interested_in, compile_as, kw_operator),
   set_opv(u_if_interested_in, function, f_u_if_interested_in),
   _Ignored4=u_if_interested_in.
/*
:- side_effect(assert_lsp(u_if_interested_in,
			  wl:lambda_def(defmacro, u_if_interested_in, f_u_if_interested_in, [key|rest], [progn, ['#BQ', [if, [and, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]]]]], [progn, ['#BQ-COMMA-ELIPSE', rest]], []]]]))).
*/
/*
:- side_effect(assert_lsp(u_if_interested_in,
			  wl:arglist_info(u_if_interested_in, f_u_if_interested_in, [key, '&rest', rest], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[key, rest], opt:0, req:[key], rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_if_interested_in,
			  wl:init_args(1, f_u_if_interested_in))).
*/
/*
(defmacro ndbg (dbg-stream key . rest)
  `(progn
    (if (and (assq ',key *ndbg-interests*)
             (or (memq? 'all (assq ',key *ndbg-interests*))
                 (any? (lambda (x) (memq? x *ndbg-items*))
                       (cdr (assq ',key *ndbg-interests*)))))
        (cond
         ((eq? ,dbg-stream t)
;          (format (standard-output) ""(defmacro ndbg (dbg-stream key . rest)\n  `(progn\n    (if (and (assq ',key *ndbg-interests*)\n             (or (memq? 'all (assq ',key *ndbg-interests*))\n                 (any? (lambda (x) (memq? x *ndbg-items*))\n                       (cdr (assq ',key *ndbg-interests*)))))\n        (cond\n         ((eq? ,dbg-stream t)\n;          (format (standard-output) \"~&\")\n;          (ndbg-indentation (standard-output))\n          (format (standard-output) ,@rest)\n          t)\n         (,dbg-stream\n;          (format ,dbg-stream \"~&\")\n;          (ndbg-indentation ,dbg-stream)\n          (format ,dbg-stream ,@rest)\n          t)\n         (else nil))\n        nil)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:798 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,ndbg,['dbg-stream',key|rest],['#BQ',[progn,[if,[and,[assq,[quote,['#COMMA',key]],'*ndbg-interests*'],[or,['memq?',[quote,all],[assq,[quote,['#COMMA',key]],'*ndbg-interests*']],['any?',[lambda,[x],['memq?',x,'*ndbg-items*']],[cdr,[assq,[quote,['#COMMA',key]],'*ndbg-interests*']]]]],[cond,[['eq?',['#COMMA','dbg-stream'],t],[format,['standard-output'],['#BQ-COMMA-ELIPSE',rest]],t],[['#COMMA','dbg-stream'],[format,['#COMMA','dbg-stream'],['#BQ-COMMA-ELIPSE',rest]],t],[else,[]]],[]]]]])
wl:lambda_def(defmacro, u_ndbg, f_u_ndbg, [u_dbg_stream, key|rest], [progn, ['#BQ', [progn, [if, [and, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]]]]], [cond, [[u_eq_c63, ['#COMMA', u_dbg_stream], t], [format, [u_standard_output], ['#BQ-COMMA-ELIPSE', rest]], t], [['#COMMA', u_dbg_stream], [format, ['#COMMA', u_dbg_stream], ['#BQ-COMMA-ELIPSE', rest]], t], [u_else, []]], []]]]]).
wl:arglist_info(u_ndbg, f_u_ndbg, [u_dbg_stream, key, '&rest', rest], arginfo{all:[u_dbg_stream, key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_dbg_stream, key, rest], opt:0, req:[u_dbg_stream, key], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, f_u_ndbg).

/*

### Compiled:  `U::NDBG` 
*/
f_u_ndbg(Dbg_stream, Key, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_dbg_stream, Dbg_stream), bv(key, Key), bv(rest, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, key, Key_Get9),
	get_var(GEnv, rest, Rest_Get15),
	get_var(GEnv, u_dbg_stream, Dbg_stream_Get13),
	[progn, [if, [and, [ext_assq, [quote, Key_Get9], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, Key_Get9], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, Key_Get9], u_xx_ndbg_interests_xx]]]]], [cond, [[u_eq_c63, Dbg_stream_Get13, t], [format, [u_standard_output]|Rest_Get15], t], [Dbg_stream_Get13, [format, Dbg_stream_Get13|Rest_Get15], t], [u_else, []]], []]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg, classof, claz_macro),
   set_opv(u_ndbg, compile_as, kw_operator),
   set_opv(u_ndbg, function, f_u_ndbg),
   _Ignored4=u_ndbg.
/*
:- side_effect(assert_lsp(u_ndbg,
			  wl:lambda_def(defmacro, u_ndbg, f_u_ndbg, [u_dbg_stream, key|rest], [progn, ['#BQ', [progn, [if, [and, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]]]]], [cond, [[u_eq_c63, ['#COMMA', u_dbg_stream], t], [format, [u_standard_output], ['#BQ-COMMA-ELIPSE', rest]], t], [['#COMMA', u_dbg_stream], [format, ['#COMMA', u_dbg_stream], ['#BQ-COMMA-ELIPSE', rest]], t], [u_else, []]], []]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg,
			  wl:arglist_info(u_ndbg, f_u_ndbg, [u_dbg_stream, key, '&rest', rest], arginfo{all:[u_dbg_stream, key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_dbg_stream, key, rest], opt:0, req:[u_dbg_stream, key], rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg, wl:init_args(2, f_u_ndbg))).
*/
/*
          (format (standard-output) ""          (format (standard-output) \"~&\")".
*/
/*
          (ndbg-indentation (standard-output))
*/
/*
          (format ,dbg-stream ""          (format ,dbg-stream \"~&\")".
*/
/*
          (ndbg-indentation ,dbg-stream)
*/
/*
(defmacro ndbg-if (key form)
  `(if (and (assq ',key *ndbg-interests*)
            (or (memq? 'all (assq ',key *ndbg-interests*))
                (any? (lambda (x) (memq? x *ndbg-items*))
                      (cdr (assq ',key *ndbg-interests*)))))
       ,form))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1452 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-if',[key,form],['#BQ',[if,[and,[assq,[quote,['#COMMA',key]],'*ndbg-interests*'],[or,['memq?',[quote,all],[assq,[quote,['#COMMA',key]],'*ndbg-interests*']],['any?',[lambda,[x],['memq?',x,'*ndbg-items*']],[cdr,[assq,[quote,['#COMMA',key]],'*ndbg-interests*']]]]],['#COMMA',form]]]])
wl:lambda_def(defmacro, u_ndbg_if, f_u_ndbg_if, [key, u_form], [progn, ['#BQ', [if, [and, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]]]]], ['#COMMA', u_form]]]]).
wl:arglist_info(u_ndbg_if, f_u_ndbg_if, [key, u_form], arginfo{all:[key, u_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[key, u_form], opt:0, req:[key, u_form], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_if).

/*

### Compiled:  `U::NDBG-IF` 
*/
f_u_ndbg_if(Key, Form, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(key, Key), bv(u_form, Form)|Global_env_Ret],
	get_var(Env, key, Key_Get8),
	get_var(Env, u_form, Form_Get),
	[if, [and, [ext_assq, [quote, Key_Get8], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, Key_Get8], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, Key_Get8], u_xx_ndbg_interests_xx]]]]], Form_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_if, classof, claz_macro),
   set_opv(u_ndbg_if, compile_as, kw_operator),
   set_opv(u_ndbg_if, function, f_u_ndbg_if),
   _Ignored4=u_ndbg_if.
/*
:- side_effect(assert_lsp(u_ndbg_if,
			  wl:lambda_def(defmacro, u_ndbg_if, f_u_ndbg_if, [key, u_form], [progn, ['#BQ', [if, [and, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx]]]]], ['#COMMA', u_form]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_if,
			  wl:arglist_info(u_ndbg_if, f_u_ndbg_if, [key, u_form], arginfo{all:[key, u_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[key, u_form], opt:0, req:[key, u_form], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_if, wl:init_args(exact_only, f_u_ndbg_if))).
*/
/*
(defmacro length-one? (x)
  `(and ,x (null? (cdr ,x))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1717 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'length-one?',[x],['#BQ',[and,['#COMMA',x],['null?',[cdr,['#COMMA',x]]]]]])
wl:lambda_def(defmacro, u_length_one_c63, f_u_length_one_c63, [u_x], [progn, ['#BQ', [and, ['#COMMA', u_x], [u_null_c63, [cdr, ['#COMMA', u_x]]]]]]).
wl:arglist_info(u_length_one_c63, f_u_length_one_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_length_one_c63).

/*

### Compiled:  `U::LENGTH-ONE?` 
*/
f_u_length_one_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get8),
	[and, X_Get8, [u_null_c63, [cdr, X_Get8]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_length_one_c63, classof, claz_macro),
   set_opv(u_length_one_c63, compile_as, kw_operator),
   set_opv(u_length_one_c63, function, f_u_length_one_c63),
   _Ignored4=u_length_one_c63.
/*
:- side_effect(assert_lsp(u_length_one_c63,
			  wl:lambda_def(defmacro, u_length_one_c63, f_u_length_one_c63, [u_x], [progn, ['#BQ', [and, ['#COMMA', u_x], [u_null_c63, [cdr, ['#COMMA', u_x]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_length_one_c63,
			  wl:arglist_info(u_length_one_c63, f_u_length_one_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_length_one_c63,
			  wl:init_args(exact_only, f_u_length_one_c63))).
*/
/*
(defmacro nil? (x)
  `(or (null? ,x)
       (eq? ,x 'nil)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1774 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'nil?',[x],['#BQ',[or,['null?',['#COMMA',x]],['eq?',['#COMMA',x],[quote,[]]]]]])
wl:lambda_def(defmacro, u_nil_c63, f_u_nil_c63, [u_x], [progn, ['#BQ', [or, [u_null_c63, ['#COMMA', u_x]], [u_eq_c63, ['#COMMA', u_x], [quote, []]]]]]).
wl:arglist_info(u_nil_c63, f_u_nil_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_nil_c63).

/*

### Compiled:  `U::NIL?` 
*/
f_u_nil_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get8),
	[or, [u_null_c63, X_Get8], [u_eq_c63, X_Get8, [quote, []]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nil_c63, classof, claz_macro),
   set_opv(u_nil_c63, compile_as, kw_operator),
   set_opv(u_nil_c63, function, f_u_nil_c63),
   _Ignored4=u_nil_c63.
/*
:- side_effect(assert_lsp(u_nil_c63,
			  wl:lambda_def(defmacro, u_nil_c63, f_u_nil_c63, [u_x], [progn, ['#BQ', [or, [u_null_c63, ['#COMMA', u_x]], [u_eq_c63, ['#COMMA', u_x], [quote, []]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_nil_c63,
			  wl:arglist_info(u_nil_c63, f_u_nil_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_nil_c63, wl:init_args(exact_only, f_u_nil_c63))).
*/
/*
(defun symbolconc (sym suffix) (intern (CONCATENATE 'string (string sym) (format () ""(defun symbolconc (sym suffix) (intern (CONCATENATE 'string (string sym) (format () \"~A\" suffix))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1835 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,symbolconc,[sym,suffix],[intern,['CONCATENATE',[quote,string],[string,sym],[format,[],'$STRING'("~A"),suffix]]]])
wl:lambda_def(defun, u_symbolconc, f_u_symbolconc, [u_sym, u_suffix], [[intern, [concatenate, [quote, string], [string, u_sym], [format, [], '$ARRAY'([*], claz_base_character, "~A"), u_suffix]]]]).
wl:arglist_info(u_symbolconc, f_u_symbolconc, [u_sym, u_suffix], arginfo{all:[u_sym, u_suffix], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_sym, u_suffix], opt:0, req:[u_sym, u_suffix], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_symbolconc).

/*

### Compiled:  `U::SYMBOLCONC` 
*/
f_u_symbolconc(Sym, Suffix, FnResult) :-
	nop(global_env(Env)),
	Env11=[bv(u_sym, Sym), bv(u_suffix, Suffix)|Env],
	get_var(Env11, u_sym, Sym_Get),
	cl_string(Sym_Get, String_Ret),
	get_var(Env11, u_suffix, Suffix_Get),
	cl_format([[], '$ARRAY'([*], claz_base_character, "~A"), Suffix_Get],
		  Format_Ret),
	cl_concatenate(string, String_Ret, Format_Ret, Intern_Param),
	cl_intern(Intern_Param, Intern_Ret),
	Intern_Ret=FnResult.
:- set_opv(f_u_symbolconc, classof, claz_function),
   set_opv(u_symbolconc, compile_as, kw_function),
   set_opv(u_symbolconc, function, f_u_symbolconc),
   _Ignored4=u_symbolconc.
/*
:- side_effect(assert_lsp(u_symbolconc,
			  wl:lambda_def(defun, u_symbolconc, f_u_symbolconc, [u_sym, u_suffix], [[intern, [concatenate, [quote, string], [string, u_sym], [format, [], '$ARRAY'([*], claz_base_character, "~A"), u_suffix]]]]))).
*/
/*
:- side_effect(assert_lsp(u_symbolconc,
			  wl:arglist_info(u_symbolconc, f_u_symbolconc, [u_sym, u_suffix], arginfo{all:[u_sym, u_suffix], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_sym, u_suffix], opt:0, req:[u_sym, u_suffix], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_symbolconc,
			  wl:init_args(exact_only, f_u_symbolconc))).
*/
/*
(defmacro pc (context-abbr)
  `(cx$print (eval (symbolconc 'CX. ,context-abbr))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1936 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,pc,['context-abbr'],['#BQ',['cx$print',[eval,[symbolconc,[quote,'CX.'],['#COMMA','context-abbr']]]]]])
wl:lambda_def(defmacro, u_pc, f_u_pc, [u_context_abbr], [progn, ['#BQ', [u_cx_c36_print, [eval, [u_symbolconc, [quote, u_cx_c46], ['#COMMA', u_context_abbr]]]]]]).
wl:arglist_info(u_pc, f_u_pc, [u_context_abbr], arginfo{all:[u_context_abbr], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context_abbr], opt:0, req:[u_context_abbr], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_pc).

/*

### Compiled:  `U::PC` 
*/
f_u_pc(Context_abbr, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_context_abbr, Context_abbr)|Global_env_Ret],
	get_var(Env, u_context_abbr, Context_abbr_Get),
	[u_cx_c36_print, [eval, [u_symbolconc, [quote, u_cx_c46], Context_abbr_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_pc, classof, claz_macro),
   set_opv(u_pc, compile_as, kw_operator),
   set_opv(u_pc, function, f_u_pc),
   _Ignored4=u_pc.
/*
:- side_effect(assert_lsp(u_pc,
			  wl:lambda_def(defmacro, u_pc, f_u_pc, [u_context_abbr], [progn, ['#BQ', [u_cx_c36_print, [eval, [u_symbolconc, [quote, u_cx_c46], ['#COMMA', u_context_abbr]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_pc,
			  wl:arglist_info(u_pc, f_u_pc, [u_context_abbr], arginfo{all:[u_context_abbr], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context_abbr], opt:0, req:[u_context_abbr], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_pc, wl:init_args(exact_only, f_u_pc))).
*/
/*
(defmacro pca (context-abbr)
  `(cx$print-ancestors (eval (symbolconc 'CX. ,context-abbr))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2019 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,pca,['context-abbr'],['#BQ',['cx$print-ancestors',[eval,[symbolconc,[quote,'CX.'],['#COMMA','context-abbr']]]]]])
wl:lambda_def(defmacro, u_pca, f_u_pca, [u_context_abbr], [progn, ['#BQ', [u_cx_c36_print_ancestors, [eval, [u_symbolconc, [quote, u_cx_c46], ['#COMMA', u_context_abbr]]]]]]).
wl:arglist_info(u_pca, f_u_pca, [u_context_abbr], arginfo{all:[u_context_abbr], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context_abbr], opt:0, req:[u_context_abbr], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_pca).

/*

### Compiled:  `U::PCA` 
*/
f_u_pca(Context_abbr, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_context_abbr, Context_abbr)|Global_env_Ret],
	get_var(Env, u_context_abbr, Context_abbr_Get),
	[u_cx_c36_print_ancestors, [eval, [u_symbolconc, [quote, u_cx_c46], Context_abbr_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_pca, classof, claz_macro),
   set_opv(u_pca, compile_as, kw_operator),
   set_opv(u_pca, function, f_u_pca),
   _Ignored4=u_pca.
/*
:- side_effect(assert_lsp(u_pca,
			  wl:lambda_def(defmacro, u_pca, f_u_pca, [u_context_abbr], [progn, ['#BQ', [u_cx_c36_print_ancestors, [eval, [u_symbolconc, [quote, u_cx_c46], ['#COMMA', u_context_abbr]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_pca,
			  wl:arglist_info(u_pca, f_u_pca, [u_context_abbr], arginfo{all:[u_context_abbr], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_context_abbr], opt:0, req:[u_context_abbr], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_pca, wl:init_args(exact_only, f_u_pca))).
*/
/*
(defmacro mem-empty-unify (ob obs context)
  `(mem (lambda (x y)
         (bd-and-empty-bd? (ob$unify-cx x y *empty-bd* ,context)))
        ,ob ,obs))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2113 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'mem-empty-unify',[ob,obs,context],['#BQ',[mem,[lambda,[x,y],['bd-and-empty-bd?',['ob$unify-cx',x,y,'*empty-bd*',['#COMMA',context]]]],['#COMMA',ob],['#COMMA',obs]]]])
wl:lambda_def(defmacro, u_mem_empty_unify, f_u_mem_empty_unify, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]).
wl:arglist_info(u_mem_empty_unify, f_u_mem_empty_unify, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mem_empty_unify).

/*

### Compiled:  `U::MEM-EMPTY-UNIFY` 
*/
f_u_mem_empty_unify(Ob, Obs, Context, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_obs, Obs), bv(u_context, Context)|Global_env_Ret],
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_obs, Obs_Get),
	[u_mem, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Get]]], Ob_Get, Obs_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_empty_unify, classof, claz_macro),
   set_opv(u_mem_empty_unify, compile_as, kw_operator),
   set_opv(u_mem_empty_unify, function, f_u_mem_empty_unify),
   _Ignored4=u_mem_empty_unify.
/*
:- side_effect(assert_lsp(u_mem_empty_unify,
			  wl:lambda_def(defmacro, u_mem_empty_unify, f_u_mem_empty_unify, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]))).
*/
/*
:- side_effect(assert_lsp(u_mem_empty_unify,
			  wl:arglist_info(u_mem_empty_unify, f_u_mem_empty_unify, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mem_empty_unify,
			  wl:init_args(exact_only, f_u_mem_empty_unify))).
*/
/*
(defmacro mem-empty-unify? (ob obs context)
  `(mem? (lambda (x y)
         (bd-and-empty-bd? (ob$unify-cx x y *empty-bd* ,context)))
        ,ob ,obs))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2265 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'mem-empty-unify?',[ob,obs,context],['#BQ',['mem?',[lambda,[x,y],['bd-and-empty-bd?',['ob$unify-cx',x,y,'*empty-bd*',['#COMMA',context]]]],['#COMMA',ob],['#COMMA',obs]]]])
wl:lambda_def(defmacro, u_mem_empty_unify_c63, f_u_mem_empty_unify_c63, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem_c63, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]).
wl:arglist_info(u_mem_empty_unify_c63, f_u_mem_empty_unify_c63, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mem_empty_unify_c63).

/*

### Compiled:  `U::MEM-EMPTY-UNIFY?` 
*/
f_u_mem_empty_unify_c63(Ob, Obs, Context, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_obs, Obs), bv(u_context, Context)|Global_env_Ret],
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_obs, Obs_Get),
	[u_mem_c63, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Get]]], Ob_Get, Obs_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_empty_unify_c63, classof, claz_macro),
   set_opv(u_mem_empty_unify_c63, compile_as, kw_operator),
   set_opv(u_mem_empty_unify_c63, function, f_u_mem_empty_unify_c63),
   _Ignored4=u_mem_empty_unify_c63.
/*
:- side_effect(assert_lsp(u_mem_empty_unify_c63,
			  wl:lambda_def(defmacro, u_mem_empty_unify_c63, f_u_mem_empty_unify_c63, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem_c63, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]))).
*/
/*
:- side_effect(assert_lsp(u_mem_empty_unify_c63,
			  wl:arglist_info(u_mem_empty_unify_c63, f_u_mem_empty_unify_c63, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mem_empty_unify_c63,
			  wl:init_args(exact_only, f_u_mem_empty_unify_c63))).
*/
/*
(defmacro mem-unify (ob obs context)
  `(mem (lambda (x y) (ob$unify-cx x y *empty-bd* ,context)) ,ob ,obs))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2419 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'mem-unify',[ob,obs,context],['#BQ',[mem,[lambda,[x,y],['ob$unify-cx',x,y,'*empty-bd*',['#COMMA',context]]],['#COMMA',ob],['#COMMA',obs]]]])
wl:lambda_def(defmacro, u_mem_unify, f_u_mem_unify, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]).
wl:arglist_info(u_mem_unify, f_u_mem_unify, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mem_unify).

/*

### Compiled:  `U::MEM-UNIFY` 
*/
f_u_mem_unify(Ob, Obs, Context, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_obs, Obs), bv(u_context, Context)|Global_env_Ret],
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_obs, Obs_Get),
	[u_mem, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Get]], Ob_Get, Obs_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_unify, classof, claz_macro),
   set_opv(u_mem_unify, compile_as, kw_operator),
   set_opv(u_mem_unify, function, f_u_mem_unify),
   _Ignored4=u_mem_unify.
/*
:- side_effect(assert_lsp(u_mem_unify,
			  wl:lambda_def(defmacro, u_mem_unify, f_u_mem_unify, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]))).
*/
/*
:- side_effect(assert_lsp(u_mem_unify,
			  wl:arglist_info(u_mem_unify, f_u_mem_unify, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mem_unify, wl:init_args(exact_only, f_u_mem_unify))).
*/
/*
(defmacro mem-unify? (ob obs context)
  `(mem? (lambda (x y) (ob$unify-cx x y *empty-bd* ,context)) ,ob ,obs))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2529 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'mem-unify?',[ob,obs,context],['#BQ',['mem?',[lambda,[x,y],['ob$unify-cx',x,y,'*empty-bd*',['#COMMA',context]]],['#COMMA',ob],['#COMMA',obs]]]])
wl:lambda_def(defmacro, u_mem_unify_c63, f_u_mem_unify_c63, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem_c63, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]).
wl:arglist_info(u_mem_unify_c63, f_u_mem_unify_c63, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mem_unify_c63).

/*

### Compiled:  `U::MEM-UNIFY?` 
*/
f_u_mem_unify_c63(Ob, Obs, Context, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_obs, Obs), bv(u_context, Context)|Global_env_Ret],
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_obs, Obs_Get),
	[u_mem_c63, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Get]], Ob_Get, Obs_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_unify_c63, classof, claz_macro),
   set_opv(u_mem_unify_c63, compile_as, kw_operator),
   set_opv(u_mem_unify_c63, function, f_u_mem_unify_c63),
   _Ignored4=u_mem_unify_c63.
/*
:- side_effect(assert_lsp(u_mem_unify_c63,
			  wl:lambda_def(defmacro, u_mem_unify_c63, f_u_mem_unify_c63, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_mem_c63, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]))).
*/
/*
:- side_effect(assert_lsp(u_mem_unify_c63,
			  wl:arglist_info(u_mem_unify_c63, f_u_mem_unify_c63, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mem_unify_c63,
			  wl:init_args(exact_only, f_u_mem_unify_c63))).
*/
/*
(defmacro del-unify! (ob obs context)
  `(del! (lambda (x y) (ob$unify-cx x y *empty-bd* ,context)) ,ob ,obs))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2641 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'del-unify!',[ob,obs,context],['#BQ',['del!',[lambda,[x,y],['ob$unify-cx',x,y,'*empty-bd*',['#COMMA',context]]],['#COMMA',ob],['#COMMA',obs]]]])
wl:lambda_def(defmacro, u_del_unify_c33, f_u_del_unify_c33, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_del_c33, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]).
wl:arglist_info(u_del_unify_c33, f_u_del_unify_c33, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_del_unify_c33).

/*

### Compiled:  `U::DEL-UNIFY!` 
*/
f_u_del_unify_c33(Ob, Obs, Context, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_obs, Obs), bv(u_context, Context)|Global_env_Ret],
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_obs, Obs_Get),
	[u_del_c33, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Get]], Ob_Get, Obs_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_del_unify_c33, classof, claz_macro),
   set_opv(u_del_unify_c33, compile_as, kw_operator),
   set_opv(u_del_unify_c33, function, f_u_del_unify_c33),
   _Ignored4=u_del_unify_c33.
/*
:- side_effect(assert_lsp(u_del_unify_c33,
			  wl:lambda_def(defmacro, u_del_unify_c33, f_u_del_unify_c33, [u_ob, u_obs, u_context], [progn, ['#BQ', [u_del_c33, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, ['#COMMA', u_context]]], ['#COMMA', u_ob], ['#COMMA', u_obs]]]]))).
*/
/*
:- side_effect(assert_lsp(u_del_unify_c33,
			  wl:arglist_info(u_del_unify_c33, f_u_del_unify_c33, [u_ob, u_obs, u_context], arginfo{all:[u_ob, u_obs, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_obs, u_context], opt:0, req:[u_ob, u_obs, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_del_unify_c33,
			  wl:init_args(exact_only, f_u_del_unify_c33))).
*/
/*
(defmacro retrieve-bd->ob (bd)
  `(map 'list (lambda (x) (car x)) ,bd))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2753 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'retrieve-bd->ob',[bd],['#BQ',[map,[quote,list],[lambda,[x],[car,x]],['#COMMA',bd]]]])
wl:lambda_def(defmacro, u_retrieve_bd_c62_ob, f_u_retrieve_bd_c62_ob, [u_bd], [progn, ['#BQ', [map, [quote, list], [lambda, [u_x], [car, u_x]], ['#COMMA', u_bd]]]]).
wl:arglist_info(u_retrieve_bd_c62_ob, f_u_retrieve_bd_c62_ob, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_retrieve_bd_c62_ob).

/*

### Compiled:  `U::RETRIEVE-BD->OB` 
*/
f_u_retrieve_bd_c62_ob(Bd, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_bd, Bd)|Global_env_Ret],
	get_var(Env, u_bd, Bd_Get),
	[map, [quote, list], [lambda, [u_x], [car, u_x]], Bd_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_retrieve_bd_c62_ob, classof, claz_macro),
   set_opv(u_retrieve_bd_c62_ob, compile_as, kw_operator),
   set_opv(u_retrieve_bd_c62_ob, function, f_u_retrieve_bd_c62_ob),
   _Ignored4=u_retrieve_bd_c62_ob.
/*
:- side_effect(assert_lsp(u_retrieve_bd_c62_ob,
			  wl:lambda_def(defmacro, u_retrieve_bd_c62_ob, f_u_retrieve_bd_c62_ob, [u_bd], [progn, ['#BQ', [map, [quote, list], [lambda, [u_x], [car, u_x]], ['#COMMA', u_bd]]]]))).
*/
/*
:- side_effect(assert_lsp(u_retrieve_bd_c62_ob,
			  wl:arglist_info(u_retrieve_bd_c62_ob, f_u_retrieve_bd_c62_ob, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_retrieve_bd_c62_ob,
			  wl:init_args(exact_only, f_u_retrieve_bd_c62_ob))).
*/
/*
(defmacro cx? (x)
  `(and (ob? ,x)
        (eq? (ob$ty ,x) *cx-ob*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2826 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'cx?',[x],['#BQ',[and,['ob?',['#COMMA',x]],['eq?',['ob$ty',['#COMMA',x]],'*cx-ob*']]]])
wl:lambda_def(defmacro, u_cx_c63, f_u_cx_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_x]], u_xx_cx_ob_xx]]]]).
wl:arglist_info(u_cx_c63, f_u_cx_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_cx_c63).

/*

### Compiled:  `U::CX?` 
*/
f_u_cx_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get8),
	[and, [u_ob_c63, X_Get8], [u_eq_c63, [u_ob_c36_ty, X_Get8], u_xx_cx_ob_xx]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_cx_c63, classof, claz_macro),
   set_opv(u_cx_c63, compile_as, kw_operator),
   set_opv(u_cx_c63, function, f_u_cx_c63),
   _Ignored4=u_cx_c63.
/*
:- side_effect(assert_lsp(u_cx_c63,
			  wl:lambda_def(defmacro, u_cx_c63, f_u_cx_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_x]], u_xx_cx_ob_xx]]]]))).
*/
/*
:- side_effect(assert_lsp(u_cx_c63,
			  wl:arglist_info(u_cx_c63, f_u_cx_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_cx_c63, wl:init_args(exact_only, f_u_cx_c63))).
*/
/*
(defmacro touchable-fact? (fact)
  `(not (ty$instance? ,fact 'causal-link)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2897 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'touchable-fact?',[fact],['#BQ',[not,['ty$instance?',['#COMMA',fact],[quote,'causal-link']]]]])
wl:lambda_def(defmacro, u_touchable_fact_c63, f_u_touchable_fact_c63, [u_fact], [progn, ['#BQ', [not, [u_ty_c36_instance_c63, ['#COMMA', u_fact], [quote, u_causal_link]]]]]).
wl:arglist_info(u_touchable_fact_c63, f_u_touchable_fact_c63, [u_fact], arginfo{all:[u_fact], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_fact], opt:0, req:[u_fact], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_touchable_fact_c63).

/*

### Compiled:  `U::TOUCHABLE-FACT?` 
*/
f_u_touchable_fact_c63(Fact, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_fact, Fact)|Global_env_Ret],
	get_var(Env, u_fact, Fact_Get),
	[not, [u_ty_c36_instance_c63, Fact_Get, [quote, u_causal_link]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_touchable_fact_c63, classof, claz_macro),
   set_opv(u_touchable_fact_c63, compile_as, kw_operator),
   set_opv(u_touchable_fact_c63, function, f_u_touchable_fact_c63),
   _Ignored4=u_touchable_fact_c63.
/*
:- side_effect(assert_lsp(u_touchable_fact_c63,
			  wl:lambda_def(defmacro, u_touchable_fact_c63, f_u_touchable_fact_c63, [u_fact], [progn, ['#BQ', [not, [u_ty_c36_instance_c63, ['#COMMA', u_fact], [quote, u_causal_link]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_touchable_fact_c63,
			  wl:arglist_info(u_touchable_fact_c63, f_u_touchable_fact_c63, [u_fact], arginfo{all:[u_fact], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_fact], opt:0, req:[u_fact], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_touchable_fact_c63,
			  wl:init_args(exact_only, f_u_touchable_fact_c63))).
*/
/*
(defmacro ob$instantiate2 (template bindings depth
                                 omit-slots include-slots substit abstract
                                 omit-proc)
  `(if *unify-debugging?*
       (ob$instantiate-dbg ,template ,bindings ,depth
                            ,omit-slots ,include-slots ,substit ,abstract
                            ,omit-proc)
       (ob$instantiate3 ,template ,bindings ,depth
                         ,omit-slots ,include-slots ,substit ,abstract
                         ,omit-proc)))

;
; (ob? obj):
;
; Determine if an arbitrary Lisp object is an ob.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2975 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$instantiate2',[template,bindings,depth,'omit-slots','include-slots',substit,abstract,'omit-proc'],['#BQ',[if,'*unify-debugging?*',['ob$instantiate-dbg',['#COMMA',template],['#COMMA',bindings],['#COMMA',depth],['#COMMA','omit-slots'],['#COMMA','include-slots'],['#COMMA',substit],['#COMMA',abstract],['#COMMA','omit-proc']],['ob$instantiate3',['#COMMA',template],['#COMMA',bindings],['#COMMA',depth],['#COMMA','omit-slots'],['#COMMA','include-slots'],['#COMMA',substit],['#COMMA',abstract],['#COMMA','omit-proc']]]]])
wl:lambda_def(defmacro, u_ob_c36_instantiate2, f_u_ob_c36_instantiate2, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], [progn, ['#BQ', [if, u_xx_unify_debugging_c63_xx, [u_ob_c36_instantiate_dbg, ['#COMMA', u_template], ['#COMMA', bindings], ['#COMMA', u_depth], ['#COMMA', u_omit_slots], ['#COMMA', u_include_slots], ['#COMMA', u_substit], ['#COMMA', u_abstract], ['#COMMA', u_omit_proc]], [u_ob_c36_instantiate3, ['#COMMA', u_template], ['#COMMA', bindings], ['#COMMA', u_depth], ['#COMMA', u_omit_slots], ['#COMMA', u_include_slots], ['#COMMA', u_substit], ['#COMMA', u_abstract], ['#COMMA', u_omit_proc]]]]]).
wl:arglist_info(u_ob_c36_instantiate2, f_u_ob_c36_instantiate2, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate2).

/*

### Compiled:  `U::OB$INSTANTIATE2` 
*/
f_u_ob_c36_instantiate2(Template, Bindings, Depth, Omit_slots, Include_slots, Substit, Abstract, Omit_proc, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_template, Template), bv(bindings, Bindings), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots), bv(u_include_slots, Include_slots), bv(u_substit, Substit), bv(u_abstract, Abstract), bv(u_omit_proc, Omit_proc)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get),
	( get_var(Env, u_abstract, Abstract_Get),
	  get_var(Env, u_depth, Depth_Get)
	),
	get_var(Env, u_include_slots, Include_slots_Get),
	get_var(Env, u_omit_proc, Omit_proc_Get),
	( get_var(Env, bindings, Bindings_Get16),
	  get_var(Env, u_template, Template_Get)
	),
	get_var(Env, u_depth, Depth_Get17),
	( get_var(Env, u_include_slots, Include_slots_Get19),
	  get_var(Env, u_omit_slots, Omit_slots_Get)
	),
	( get_var(Env, u_abstract, Abstract_Get21),
	  get_var(Env, u_substit, Substit_Get)
	),
	get_var(Env, u_omit_proc, Omit_proc_Get22),
	( get_var(Env, u_omit_slots, Omit_slots_Get18),
	  get_var(Env, u_template, Template_Get15)
	),
	get_var(Env, u_substit, Substit_Get20),
	[if, u_xx_unify_debugging_c63_xx, [u_ob_c36_instantiate_dbg, Template_Get, Bindings_Get, Depth_Get, Omit_slots_Get, Include_slots_Get, Substit_Get, Abstract_Get, Omit_proc_Get], [u_ob_c36_instantiate3, Template_Get15, Bindings_Get16, Depth_Get17, Omit_slots_Get18, Include_slots_Get19, Substit_Get20, Abstract_Get21, Omit_proc_Get22]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_instantiate2, classof, claz_macro),
   set_opv(u_ob_c36_instantiate2, compile_as, kw_operator),
   set_opv(u_ob_c36_instantiate2, function, f_u_ob_c36_instantiate2),
   _Ignored4=u_ob_c36_instantiate2.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate2,
			  wl:lambda_def(defmacro, u_ob_c36_instantiate2, f_u_ob_c36_instantiate2, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], [progn, ['#BQ', [if, u_xx_unify_debugging_c63_xx, [u_ob_c36_instantiate_dbg, ['#COMMA', u_template], ['#COMMA', bindings], ['#COMMA', u_depth], ['#COMMA', u_omit_slots], ['#COMMA', u_include_slots], ['#COMMA', u_substit], ['#COMMA', u_abstract], ['#COMMA', u_omit_proc]], [u_ob_c36_instantiate3, ['#COMMA', u_template], ['#COMMA', bindings], ['#COMMA', u_depth], ['#COMMA', u_omit_slots], ['#COMMA', u_include_slots], ['#COMMA', u_substit], ['#COMMA', u_abstract], ['#COMMA', u_omit_proc]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate2,
			  wl:arglist_info(u_ob_c36_instantiate2, f_u_ob_c36_instantiate2, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate2,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate2))).
*/
/*
*/
/*
 (ob? obj):
*/
/*
*/
/*
 Determine if an arbitrary Lisp object is an ob.
*/
/*
*/
/*
(defmacro ob? (obj) `(typep ,obj 'obr))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3570 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob?',[obj],['#BQ',[typep,['#COMMA',obj],[quote,obr]]]])
wl:lambda_def(defmacro, u_ob_c63, f_u_ob_c63, [u_obj], [progn, ['#BQ', [typep, ['#COMMA', u_obj], [quote, u_obr]]]]).
wl:arglist_info(u_ob_c63, f_u_ob_c63, [u_obj], arginfo{all:[u_obj], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obj], opt:0, req:[u_obj], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c63).

/*

### Compiled:  `U::OB?` 
*/
f_u_ob_c63(Obj, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_obj, Obj)|Global_env_Ret],
	get_var(Env, u_obj, Obj_Get),
	[typep, Obj_Get, [quote, u_obr]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c63, classof, claz_macro),
   set_opv(u_ob_c63, compile_as, kw_operator),
   set_opv(u_ob_c63, function, f_u_ob_c63),
   _Ignored4=u_ob_c63.
/*
:- side_effect(assert_lsp(u_ob_c63,
			  wl:lambda_def(defmacro, u_ob_c63, f_u_ob_c63, [u_obj], [progn, ['#BQ', [typep, ['#COMMA', u_obj], [quote, u_obr]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c63,
			  wl:arglist_info(u_ob_c63, f_u_ob_c63, [u_obj], arginfo{all:[u_obj], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obj], opt:0, req:[u_obj], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c63, wl:init_args(exact_only, f_u_ob_c63))).
*/
/*
(defmacro enforce-ob (obj routine)
  `(if (not (ob? ,obj))
       (setq ,obj (error ""(defmacro enforce-ob (obj routine)\n  `(if (not (ob? ,obj))\n       (setq ,obj (error \"~A: ~A not ob\" ,routine ,obj))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3611 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'enforce-ob',[obj,routine],['#BQ',[if,[not,['ob?',['#COMMA',obj]]],[setq,['#COMMA',obj],[error,'$STRING'("~A: ~A not ob"),['#COMMA',routine],['#COMMA',obj]]]]]])
wl:lambda_def(defmacro, u_enforce_ob, f_u_enforce_ob, [u_obj, u_routine], [progn, ['#BQ', [if, [not, [u_ob_c63, ['#COMMA', u_obj]]], [setq, ['#COMMA', u_obj], [error, '$ARRAY'([*], claz_base_character, "~A: ~A not ob"), ['#COMMA', u_routine], ['#COMMA', u_obj]]]]]]).
wl:arglist_info(u_enforce_ob, f_u_enforce_ob, [u_obj, u_routine], arginfo{all:[u_obj, u_routine], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obj, u_routine], opt:0, req:[u_obj, u_routine], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_enforce_ob).

/*

### Compiled:  `U::ENFORCE-OB` 
*/
f_u_enforce_ob(Obj, Routine, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_obj, Obj), bv(u_routine, Routine)|Global_env_Ret],
	get_var(Env, u_obj, Obj_Get8),
	get_var(Env, u_routine, Routine_Get),
	[if, [not, [u_ob_c63, Obj_Get8]], [setq, Obj_Get8, [error, '$ARRAY'([*], claz_base_character, "~A: ~A not ob"), Routine_Get, Obj_Get8]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_enforce_ob, classof, claz_macro),
   set_opv(u_enforce_ob, compile_as, kw_operator),
   set_opv(u_enforce_ob, function, f_u_enforce_ob),
   _Ignored4=u_enforce_ob.
/*
:- side_effect(assert_lsp(u_enforce_ob,
			  wl:lambda_def(defmacro, u_enforce_ob, f_u_enforce_ob, [u_obj, u_routine], [progn, ['#BQ', [if, [not, [u_ob_c63, ['#COMMA', u_obj]]], [setq, ['#COMMA', u_obj], [error, '$ARRAY'([*], claz_base_character, "~A: ~A not ob"), ['#COMMA', u_routine], ['#COMMA', u_obj]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_enforce_ob,
			  wl:arglist_info(u_enforce_ob, f_u_enforce_ob, [u_obj, u_routine], arginfo{all:[u_obj, u_routine], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obj, u_routine], opt:0, req:[u_obj, u_routine], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_enforce_ob,
			  wl:init_args(exact_only, f_u_enforce_ob))).
*/
/*
(defmacro ob$ty (ob)
 `(ob$get ,ob 'type))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3730 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$ty',[ob],['#BQ',['ob$get',['#COMMA',ob],[quote,type]]]])
wl:lambda_def(defmacro, u_ob_c36_ty, f_u_ob_c36_ty, [u_ob], [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_ob], [quote, type]]]]).
wl:arglist_info(u_ob_c36_ty, f_u_ob_c36_ty, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_ty).

/*

### Compiled:  `U::OB$TY` 
*/
f_u_ob_c36_ty(Ob, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob)|Global_env_Ret],
	get_var(Env, u_ob, Ob_Get),
	[u_ob_c36_get, Ob_Get, [quote, type]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_ty, classof, claz_macro),
   set_opv(u_ob_c36_ty, compile_as, kw_operator),
   set_opv(u_ob_c36_ty, function, f_u_ob_c36_ty),
   _Ignored4=u_ob_c36_ty.
/*
:- side_effect(assert_lsp(u_ob_c36_ty,
			  wl:lambda_def(defmacro, u_ob_c36_ty, f_u_ob_c36_ty, [u_ob], [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_ob], [quote, type]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_ty,
			  wl:arglist_info(u_ob_c36_ty, f_u_ob_c36_ty, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_ty, wl:init_args(exact_only, f_u_ob_c36_ty))).
*/
/*
(defmacro ty? (x)
  `(and (ob? ,x)
        (eq? (ob$ty ,x) *ty-ob*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3774 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ty?',[x],['#BQ',[and,['ob?',['#COMMA',x]],['eq?',['ob$ty',['#COMMA',x]],'*ty-ob*']]]])
wl:lambda_def(defmacro, u_ty_c63, f_u_ty_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_x]], u_xx_ty_ob_xx]]]]).
wl:arglist_info(u_ty_c63, f_u_ty_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ty_c63).

/*

### Compiled:  `U::TY?` 
*/
f_u_ty_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get8),
	[and, [u_ob_c63, X_Get8], [u_eq_c63, [u_ob_c36_ty, X_Get8], u_xx_ty_ob_xx]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ty_c63, classof, claz_macro),
   set_opv(u_ty_c63, compile_as, kw_operator),
   set_opv(u_ty_c63, function, f_u_ty_c63),
   _Ignored4=u_ty_c63.
/*
:- side_effect(assert_lsp(u_ty_c63,
			  wl:lambda_def(defmacro, u_ty_c63, f_u_ty_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_x]], u_xx_ty_ob_xx]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ty_c63,
			  wl:arglist_info(u_ty_c63, f_u_ty_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ty_c63, wl:init_args(exact_only, f_u_ty_c63))).
*/
/*
(defmacro path->slot-name (path)
  `(tlast ,path))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3845 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'path->slot-name',[path],['#BQ',[tlast,['#COMMA',path]]]])
wl:lambda_def(defmacro, u_path_c62_slot_name, f_u_path_c62_slot_name, [u_path], [progn, ['#BQ', [u_tlast, ['#COMMA', u_path]]]]).
wl:arglist_info(u_path_c62_slot_name, f_u_path_c62_slot_name, [u_path], arginfo{all:[u_path], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_path], opt:0, req:[u_path], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_path_c62_slot_name).

/*

### Compiled:  `U::PATH->SLOT-NAME` 
*/
f_u_path_c62_slot_name(Path, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_path, Path)|Global_env_Ret],
	get_var(Env, u_path, Path_Get),
	[u_tlast, Path_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_path_c62_slot_name, classof, claz_macro),
   set_opv(u_path_c62_slot_name, compile_as, kw_operator),
   set_opv(u_path_c62_slot_name, function, f_u_path_c62_slot_name),
   _Ignored4=u_path_c62_slot_name.
/*
:- side_effect(assert_lsp(u_path_c62_slot_name,
			  wl:lambda_def(defmacro, u_path_c62_slot_name, f_u_path_c62_slot_name, [u_path], [progn, ['#BQ', [u_tlast, ['#COMMA', u_path]]]]))).
*/
/*
:- side_effect(assert_lsp(u_path_c62_slot_name,
			  wl:arglist_info(u_path_c62_slot_name, f_u_path_c62_slot_name, [u_path], arginfo{all:[u_path], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_path], opt:0, req:[u_path], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_path_c62_slot_name,
			  wl:init_args(exact_only, f_u_path_c62_slot_name))).
*/
/*
(defmacro var? (x)
  `(and (ob? ,x)
        (ty$instance? ,x 'uvar)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3897 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'var?',[x],['#BQ',[and,['ob?',['#COMMA',x]],['ty$instance?',['#COMMA',x],[quote,uvar]]]]])
wl:lambda_def(defmacro, u_var_c63, f_u_var_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_ty_c36_instance_c63, ['#COMMA', u_x], [quote, u_uvar]]]]]).
wl:arglist_info(u_var_c63, f_u_var_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_var_c63).

/*

### Compiled:  `U::VAR?` 
*/
f_u_var_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get8),
	[and, [u_ob_c63, X_Get8], [u_ty_c36_instance_c63, X_Get8, [quote, u_uvar]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_var_c63, classof, claz_macro),
   set_opv(u_var_c63, compile_as, kw_operator),
   set_opv(u_var_c63, function, f_u_var_c63),
   _Ignored4=u_var_c63.
/*
:- side_effect(assert_lsp(u_var_c63,
			  wl:lambda_def(defmacro, u_var_c63, f_u_var_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_ty_c36_instance_c63, ['#COMMA', u_x], [quote, u_uvar]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_var_c63,
			  wl:arglist_info(u_var_c63, f_u_var_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_var_c63, wl:init_args(exact_only, f_u_var_c63))).
*/
/*
(defmacro special? (x)
  `(and (ob? ,x)
        (ty$instance? ,x 'uspecial)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3968 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'special?',[x],['#BQ',[and,['ob?',['#COMMA',x]],['ty$instance?',['#COMMA',x],[quote,uspecial]]]]])
wl:lambda_def(defmacro, u_special_c63, f_u_special_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_ty_c36_instance_c63, ['#COMMA', u_x], [quote, u_uspecial]]]]]).
wl:arglist_info(u_special_c63, f_u_special_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_special_c63).

/*

### Compiled:  `U::SPECIAL?` 
*/
f_u_special_c63(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get8),
	[and, [u_ob_c63, X_Get8], [u_ty_c36_instance_c63, X_Get8, [quote, u_uspecial]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_special_c63, classof, claz_macro),
   set_opv(u_special_c63, compile_as, kw_operator),
   set_opv(u_special_c63, function, f_u_special_c63),
   _Ignored4=u_special_c63.
/*
:- side_effect(assert_lsp(u_special_c63,
			  wl:lambda_def(defmacro, u_special_c63, f_u_special_c63, [u_x], [progn, ['#BQ', [and, [u_ob_c63, ['#COMMA', u_x]], [u_ty_c36_instance_c63, ['#COMMA', u_x], [quote, u_uspecial]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_special_c63,
			  wl:arglist_info(u_special_c63, f_u_special_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_special_c63,
			  wl:init_args(exact_only, f_u_special_c63))).
*/
/*
(defmacro car-eq? (x y)
 `(and (pair? ,x) (eq? (car ,x) ,y)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4047 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'car-eq?',[x,y],['#BQ',[and,['pair?',['#COMMA',x]],['eq?',[car,['#COMMA',x]],['#COMMA',y]]]]])
wl:lambda_def(defmacro, u_car_eq_c63, f_u_car_eq_c63, [u_x, u_y], [progn, ['#BQ', [and, [u_pair_c63, ['#COMMA', u_x]], [u_eq_c63, [car, ['#COMMA', u_x]], ['#COMMA', u_y]]]]]).
wl:arglist_info(u_car_eq_c63, f_u_car_eq_c63, [u_x, u_y], arginfo{all:[u_x, u_y], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x, u_y], opt:0, req:[u_x, u_y], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_car_eq_c63).

/*

### Compiled:  `U::CAR-EQ?` 
*/
f_u_car_eq_c63(X, Y, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X), bv(u_y, Y)|Global_env_Ret],
	get_var(Env, u_x, X_Get8),
	get_var(Env, u_y, Y_Get),
	[and, [u_pair_c63, X_Get8], [u_eq_c63, [car, X_Get8], Y_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_car_eq_c63, classof, claz_macro),
   set_opv(u_car_eq_c63, compile_as, kw_operator),
   set_opv(u_car_eq_c63, function, f_u_car_eq_c63),
   _Ignored4=u_car_eq_c63.
/*
:- side_effect(assert_lsp(u_car_eq_c63,
			  wl:lambda_def(defmacro, u_car_eq_c63, f_u_car_eq_c63, [u_x, u_y], [progn, ['#BQ', [and, [u_pair_c63, ['#COMMA', u_x]], [u_eq_c63, [car, ['#COMMA', u_x]], ['#COMMA', u_y]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_car_eq_c63,
			  wl:arglist_info(u_car_eq_c63, f_u_car_eq_c63, [u_x, u_y], arginfo{all:[u_x, u_y], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x, u_y], opt:0, req:[u_x, u_y], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_car_eq_c63,
			  wl:init_args(exact_only, f_u_car_eq_c63))).
*/
/*
(defmacro variable-name (x)
 `(ob$get ,x 'name))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4110 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'variable-name',[x],['#BQ',['ob$get',['#COMMA',x],[quote,name]]]])
wl:lambda_def(defmacro, u_variable_name, f_u_variable_name, [u_x], [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_x], [quote, sys_name]]]]).
wl:arglist_info(u_variable_name, f_u_variable_name, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_variable_name).

/*

### Compiled:  `U::VARIABLE-NAME` 
*/
f_u_variable_name(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[u_ob_c36_get, X_Get, [quote, sys_name]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_variable_name, classof, claz_macro),
   set_opv(u_variable_name, compile_as, kw_operator),
   set_opv(u_variable_name, function, f_u_variable_name),
   _Ignored4=u_variable_name.
/*
:- side_effect(assert_lsp(u_variable_name,
			  wl:lambda_def(defmacro, u_variable_name, f_u_variable_name, [u_x], [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_x], [quote, sys_name]]]]))).
*/
/*
:- side_effect(assert_lsp(u_variable_name,
			  wl:arglist_info(u_variable_name, f_u_variable_name, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_variable_name,
			  wl:init_args(exact_only, f_u_variable_name))).
*/
/*
(defmacro variable-type (x)
 `(ob$get ,x 'unifies-with))

; Setters: For consistency, access to slots in obr is done through
; these macros.

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4160 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'variable-type',[x],['#BQ',['ob$get',['#COMMA',x],[quote,'unifies-with']]]])
wl:lambda_def(defmacro, u_variable_type, f_u_variable_type, [u_x], [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_x], [quote, u_unifies_with]]]]).
wl:arglist_info(u_variable_type, f_u_variable_type, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_variable_type).

/*

### Compiled:  `U::VARIABLE-TYPE` 
*/
f_u_variable_type(X, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X)|Global_env_Ret],
	get_var(Env, u_x, X_Get),
	[u_ob_c36_get, X_Get, [quote, u_unifies_with]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_variable_type, classof, claz_macro),
   set_opv(u_variable_type, compile_as, kw_operator),
   set_opv(u_variable_type, function, f_u_variable_type),
   _Ignored4=u_variable_type.
/*
:- side_effect(assert_lsp(u_variable_type,
			  wl:lambda_def(defmacro, u_variable_type, f_u_variable_type, [u_x], [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_x], [quote, u_unifies_with]]]]))).
*/
/*
:- side_effect(assert_lsp(u_variable_type,
			  wl:arglist_info(u_variable_type, f_u_variable_type, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_variable_type,
			  wl:init_args(exact_only, f_u_variable_type))).
*/
/*
 Setters: For consistency, access to slots in obr is done through
*/
/*
 these macros.
*/
/*
(defmacro set-obr-obnames (ob val)
  `(setf (obr-obnames ,ob) ,val))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4302 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'set-obr-obnames',[ob,val],['#BQ',[setf,['obr-obnames',['#COMMA',ob]],['#COMMA',val]]]])
wl:lambda_def(defmacro, u_set_obr_obnames, f_u_set_obr_obnames, [u_ob, u_val], [progn, ['#BQ', [setf, [u_obr_obnames, ['#COMMA', u_ob]], ['#COMMA', u_val]]]]).
wl:arglist_info(u_set_obr_obnames, f_u_set_obr_obnames, [u_ob, u_val], arginfo{all:[u_ob, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_val], opt:0, req:[u_ob, u_val], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_set_obr_obnames).

/*

### Compiled:  `U::SET-OBR-OBNAMES` 
*/
f_u_set_obr_obnames(Ob, Val, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_val, Val)|Global_env_Ret],
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_val, Val_Get),
	[setf, [u_obr_obnames, Ob_Get], Val_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_set_obr_obnames, classof, claz_macro),
   set_opv(u_set_obr_obnames, compile_as, kw_operator),
   set_opv(u_set_obr_obnames, function, f_u_set_obr_obnames),
   _Ignored4=u_set_obr_obnames.
/*
:- side_effect(assert_lsp(u_set_obr_obnames,
			  wl:lambda_def(defmacro, u_set_obr_obnames, f_u_set_obr_obnames, [u_ob, u_val], [progn, ['#BQ', [setf, [u_obr_obnames, ['#COMMA', u_ob]], ['#COMMA', u_val]]]]))).
*/
/*
:- side_effect(assert_lsp(u_set_obr_obnames,
			  wl:arglist_info(u_set_obr_obnames, f_u_set_obr_obnames, [u_ob, u_val], arginfo{all:[u_ob, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_val], opt:0, req:[u_ob, u_val], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_set_obr_obnames,
			  wl:init_args(exact_only, f_u_set_obr_obnames))).
*/
/*
(defmacro set-obr-slots (ob val)
  `(setf (obr-slots ,ob) ,val))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4372 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'set-obr-slots',[ob,val],['#BQ',[setf,['obr-slots',['#COMMA',ob]],['#COMMA',val]]]])
wl:lambda_def(defmacro, u_set_obr_slots, f_u_set_obr_slots, [u_ob, u_val], [progn, ['#BQ', [setf, [u_obr_slots, ['#COMMA', u_ob]], ['#COMMA', u_val]]]]).
wl:arglist_info(u_set_obr_slots, f_u_set_obr_slots, [u_ob, u_val], arginfo{all:[u_ob, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_val], opt:0, req:[u_ob, u_val], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_set_obr_slots).

/*

### Compiled:  `U::SET-OBR-SLOTS` 
*/
f_u_set_obr_slots(Ob, Val, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_val, Val)|Global_env_Ret],
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_val, Val_Get),
	[setf, [u_obr_slots, Ob_Get], Val_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_set_obr_slots, classof, claz_macro),
   set_opv(u_set_obr_slots, compile_as, kw_operator),
   set_opv(u_set_obr_slots, function, f_u_set_obr_slots),
   _Ignored4=u_set_obr_slots.
/*
:- side_effect(assert_lsp(u_set_obr_slots,
			  wl:lambda_def(defmacro, u_set_obr_slots, f_u_set_obr_slots, [u_ob, u_val], [progn, ['#BQ', [setf, [u_obr_slots, ['#COMMA', u_ob]], ['#COMMA', u_val]]]]))).
*/
/*
:- side_effect(assert_lsp(u_set_obr_slots,
			  wl:arglist_info(u_set_obr_slots, f_u_set_obr_slots, [u_ob, u_val], arginfo{all:[u_ob, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_val], opt:0, req:[u_ob, u_val], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_set_obr_slots,
			  wl:init_args(exact_only, f_u_set_obr_slots))).
*/
/*
(defmacro set-obr-literal (ob val)
  `(setf (obr-literal ,ob) ,val))

;
; Accessor functions for elements of the (obr-slots self) instance variable,
; which contains a triple of
;   slot-name
;   slot-value (a single value--multiple values for a slot require
;               multiple entries in (obr-slots self))
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'set-obr-literal',[ob,val],['#BQ',[setf,['obr-literal',['#COMMA',ob]],['#COMMA',val]]]])
wl:lambda_def(defmacro, u_set_obr_literal, f_u_set_obr_literal, [u_ob, u_val], [progn, ['#BQ', [setf, [u_obr_literal, ['#COMMA', u_ob]], ['#COMMA', u_val]]]]).
wl:arglist_info(u_set_obr_literal, f_u_set_obr_literal, [u_ob, u_val], arginfo{all:[u_ob, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_val], opt:0, req:[u_ob, u_val], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_set_obr_literal).

/*

### Compiled:  `U::SET-OBR-LITERAL` 
*/
f_u_set_obr_literal(Ob, Val, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob, Ob), bv(u_val, Val)|Global_env_Ret],
	get_var(Env, u_ob, Ob_Get),
	get_var(Env, u_val, Val_Get),
	[setf, [u_obr_literal, Ob_Get], Val_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_set_obr_literal, classof, claz_macro),
   set_opv(u_set_obr_literal, compile_as, kw_operator),
   set_opv(u_set_obr_literal, function, f_u_set_obr_literal),
   _Ignored4=u_set_obr_literal.
/*
:- side_effect(assert_lsp(u_set_obr_literal,
			  wl:lambda_def(defmacro, u_set_obr_literal, f_u_set_obr_literal, [u_ob, u_val], [progn, ['#BQ', [setf, [u_obr_literal, ['#COMMA', u_ob]], ['#COMMA', u_val]]]]))).
*/
/*
:- side_effect(assert_lsp(u_set_obr_literal,
			  wl:arglist_info(u_set_obr_literal, f_u_set_obr_literal, [u_ob, u_val], arginfo{all:[u_ob, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_val], opt:0, req:[u_ob, u_val], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_set_obr_literal,
			  wl:init_args(exact_only, f_u_set_obr_literal))).
*/
/*
*/
/*
 Accessor functions for elements of the (obr-slots self) instance variable,
*/
/*
 which contains a triple of
*/
/*
   slot-name
*/
/*
   slot-value (a single value--multiple values for a slot require
*/
/*
               multiple entries in (obr-slots self))
*/
/*
*/
/*
(defmacro slots-name (slots) `(car ,slots))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4754 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'slots-name',[slots],['#BQ',[car,['#COMMA',slots]]]])
wl:lambda_def(defmacro, u_slots_name, f_u_slots_name, [sys_slots], [progn, ['#BQ', [car, ['#COMMA', sys_slots]]]]).
wl:arglist_info(u_slots_name, f_u_slots_name, [sys_slots], arginfo{all:[sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_slots], opt:0, req:[sys_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_slots_name).

/*

### Compiled:  `U::SLOTS-NAME` 
*/
f_u_slots_name(Slots, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(sys_slots, Slots)|Global_env_Ret],
	get_var(Env, sys_slots, Slots_Get),
	[car, Slots_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_slots_name, classof, claz_macro),
   set_opv(u_slots_name, compile_as, kw_operator),
   set_opv(u_slots_name, function, f_u_slots_name),
   _Ignored4=u_slots_name.
/*
:- side_effect(assert_lsp(u_slots_name,
			  wl:lambda_def(defmacro, u_slots_name, f_u_slots_name, [sys_slots], [progn, ['#BQ', [car, ['#COMMA', sys_slots]]]]))).
*/
/*
:- side_effect(assert_lsp(u_slots_name,
			  wl:arglist_info(u_slots_name, f_u_slots_name, [sys_slots], arginfo{all:[sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_slots], opt:0, req:[sys_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_slots_name,
			  wl:init_args(exact_only, f_u_slots_name))).
*/
/*
(defmacro slots-value (slots) `(cadr ,slots))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4799 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'slots-value',[slots],['#BQ',[cadr,['#COMMA',slots]]]])
wl:lambda_def(defmacro, u_slots_value, f_u_slots_value, [sys_slots], [progn, ['#BQ', [cadr, ['#COMMA', sys_slots]]]]).
wl:arglist_info(u_slots_value, f_u_slots_value, [sys_slots], arginfo{all:[sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_slots], opt:0, req:[sys_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_slots_value).

/*

### Compiled:  `U::SLOTS-VALUE` 
*/
f_u_slots_value(Slots, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(sys_slots, Slots)|Global_env_Ret],
	get_var(Env, sys_slots, Slots_Get),
	[cadr, Slots_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_slots_value, classof, claz_macro),
   set_opv(u_slots_value, compile_as, kw_operator),
   set_opv(u_slots_value, function, f_u_slots_value),
   _Ignored4=u_slots_value.
/*
:- side_effect(assert_lsp(u_slots_value,
			  wl:lambda_def(defmacro, u_slots_value, f_u_slots_value, [sys_slots], [progn, ['#BQ', [cadr, ['#COMMA', sys_slots]]]]))).
*/
/*
:- side_effect(assert_lsp(u_slots_value,
			  wl:arglist_info(u_slots_value, f_u_slots_value, [sys_slots], arginfo{all:[sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_slots], opt:0, req:[sys_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_slots_value,
			  wl:init_args(exact_only, f_u_slots_value))).
*/
/*
(defmacro with-unhidden-default (&rest body)
   `(unwind-protect
       (progn (setq *hidden-default* nil)
              ,@body)
       (setq *hidden-default* t)))

;
; ob$create-empty: create a new empty ob
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4846 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'with-unhidden-default',['&rest',body],['#BQ',['unwind-protect',[progn,[setq,'*hidden-default*',[]],['#BQ-COMMA-ELIPSE',body]],[setq,'*hidden-default*',t]]]])
wl:lambda_def(defmacro, u_with_unhidden_default, f_u_with_unhidden_default, [c38_rest, u_body], [progn, ['#BQ', [unwind_protect, [progn, [setq, u_xx_hidden_default_xx, []], ['#BQ-COMMA-ELIPSE', u_body]], [setq, u_xx_hidden_default_xx, t]]]]).
wl:arglist_info(u_with_unhidden_default, f_u_with_unhidden_default, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}).
wl: init_args(0, f_u_with_unhidden_default).

/*

### Compiled:  `U::WITH-UNHIDDEN-DEFAULT` 
*/
f_u_with_unhidden_default(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_body, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_body, Body_Get),
	[unwind_protect, [progn, [setq, u_xx_hidden_default_xx, []]|Body_Get], [setq, u_xx_hidden_default_xx, t]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_with_unhidden_default, classof, claz_macro),
   set_opv(u_with_unhidden_default, compile_as, kw_operator),
   set_opv(u_with_unhidden_default, function, f_u_with_unhidden_default),
   _Ignored4=u_with_unhidden_default.
/*
:- side_effect(assert_lsp(u_with_unhidden_default,
			  wl:lambda_def(defmacro, u_with_unhidden_default, f_u_with_unhidden_default, [c38_rest, u_body], [progn, ['#BQ', [unwind_protect, [progn, [setq, u_xx_hidden_default_xx, []], ['#BQ-COMMA-ELIPSE', u_body]], [setq, u_xx_hidden_default_xx, t]]]]))).
*/
/*
:- side_effect(assert_lsp(u_with_unhidden_default,
			  wl:arglist_info(u_with_unhidden_default, f_u_with_unhidden_default, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_with_unhidden_default,
			  wl:init_args(0, f_u_with_unhidden_default))).
*/
/*
*/
/*
 ob$create-empty: create a new empty ob
*/
/*
*/
/*
(defmacro ob$create-empty ()
  '(ob$create-named-empty nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5056 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$create-empty',[],[quote,['ob$create-named-empty',[]]]])
wl:lambda_def(defmacro, u_ob_c36_create_empty, f_u_ob_c36_create_empty, [], [progn, [quote, [u_ob_c36_create_named_empty, []]]]).
wl:arglist_info(u_ob_c36_create_empty, f_u_ob_c36_create_empty, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_create_empty).

/*

### Compiled:  `U::OB$CREATE-EMPTY` 
*/
f_u_ob_c36_create_empty(FnResult) :-
	global_env(Global_env_Ret),
	_119212676=Global_env_Ret,
	[u_ob_c36_create_named_empty, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_create_empty, classof, claz_macro),
   set_opv(u_ob_c36_create_empty, compile_as, kw_operator),
   set_opv(u_ob_c36_create_empty, function, f_u_ob_c36_create_empty),
   _Ignored4=u_ob_c36_create_empty.
/*
:- side_effect(assert_lsp(u_ob_c36_create_empty,
			  wl:lambda_def(defmacro, u_ob_c36_create_empty, f_u_ob_c36_create_empty, [], [progn, [quote, [u_ob_c36_create_named_empty, []]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_create_empty,
			  wl:arglist_info(u_ob_c36_create_empty, f_u_ob_c36_create_empty, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_create_empty,
			  wl:init_args(exact_only, f_u_ob_c36_create_empty))).
*/
/*
(defmacro ndbg-newline (stream key)
  `(if-interested-in ,key (do-newline ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5118 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-newline',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['do-newline',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_newline, f_u_ndbg_newline, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_do_newline, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_newline, f_u_ndbg_newline, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_newline).

/*

### Compiled:  `U::NDBG-NEWLINE` 
*/
f_u_ndbg_newline(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_do_newline, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_newline, classof, claz_macro),
   set_opv(u_ndbg_newline, compile_as, kw_operator),
   set_opv(u_ndbg_newline, function, f_u_ndbg_newline),
   _Ignored4=u_ndbg_newline.
/*
:- side_effect(assert_lsp(u_ndbg_newline,
			  wl:lambda_def(defmacro, u_ndbg_newline, f_u_ndbg_newline, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_do_newline, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_newline,
			  wl:arglist_info(u_ndbg_newline, f_u_ndbg_newline, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_newline,
			  wl:init_args(exact_only, f_u_ndbg_newline))).
*/
/*
(defmacro ndbg-large-roman-font (stream key)
  `(if-interested-in ,key (begin-large-roman-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5204 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-large-roman-font',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['begin-large-roman-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_large_roman_font, f_u_ndbg_large_roman_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_large_roman_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_large_roman_font, f_u_ndbg_large_roman_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_large_roman_font).

/*

### Compiled:  `U::NDBG-LARGE-ROMAN-FONT` 
*/
f_u_ndbg_large_roman_font(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_begin_large_roman_font, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_large_roman_font, classof, claz_macro),
   set_opv(u_ndbg_large_roman_font, compile_as, kw_operator),
   set_opv(u_ndbg_large_roman_font, function, f_u_ndbg_large_roman_font),
   _Ignored4=u_ndbg_large_roman_font.
/*
:- side_effect(assert_lsp(u_ndbg_large_roman_font,
			  wl:lambda_def(defmacro, u_ndbg_large_roman_font, f_u_ndbg_large_roman_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_large_roman_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_large_roman_font,
			  wl:arglist_info(u_ndbg_large_roman_font, f_u_ndbg_large_roman_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_large_roman_font,
			  wl:init_args(exact_only, f_u_ndbg_large_roman_font))).
*/
/*
(defmacro ndbg-large-bold-font (stream key)
  `(if-interested-in ,key (begin-large-bold-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5311 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-large-bold-font',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['begin-large-bold-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_large_bold_font, f_u_ndbg_large_bold_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_large_bold_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_large_bold_font, f_u_ndbg_large_bold_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_large_bold_font).

/*

### Compiled:  `U::NDBG-LARGE-BOLD-FONT` 
*/
f_u_ndbg_large_bold_font(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_begin_large_bold_font, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_large_bold_font, classof, claz_macro),
   set_opv(u_ndbg_large_bold_font, compile_as, kw_operator),
   set_opv(u_ndbg_large_bold_font, function, f_u_ndbg_large_bold_font),
   _Ignored4=u_ndbg_large_bold_font.
/*
:- side_effect(assert_lsp(u_ndbg_large_bold_font,
			  wl:lambda_def(defmacro, u_ndbg_large_bold_font, f_u_ndbg_large_bold_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_large_bold_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_large_bold_font,
			  wl:arglist_info(u_ndbg_large_bold_font, f_u_ndbg_large_bold_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_large_bold_font,
			  wl:init_args(exact_only, f_u_ndbg_large_bold_font))).
*/
/*
(defmacro ndbg-roman-font (stream key)
  `(if-interested-in ,key (begin-roman-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5416 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-roman-font',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['begin-roman-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_roman_font, f_u_ndbg_roman_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_roman_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_roman_font, f_u_ndbg_roman_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_roman_font).

/*

### Compiled:  `U::NDBG-ROMAN-FONT` 
*/
f_u_ndbg_roman_font(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_begin_roman_font, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_roman_font, classof, claz_macro),
   set_opv(u_ndbg_roman_font, compile_as, kw_operator),
   set_opv(u_ndbg_roman_font, function, f_u_ndbg_roman_font),
   _Ignored4=u_ndbg_roman_font.
/*
:- side_effect(assert_lsp(u_ndbg_roman_font,
			  wl:lambda_def(defmacro, u_ndbg_roman_font, f_u_ndbg_roman_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_roman_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_roman_font,
			  wl:arglist_info(u_ndbg_roman_font, f_u_ndbg_roman_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_roman_font,
			  wl:init_args(exact_only, f_u_ndbg_roman_font))).
*/
/*
(defmacro ndbg-bold-font (stream key)
  `(if-interested-in ,key (begin-bold-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5511 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-bold-font',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['begin-bold-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_bold_font, f_u_ndbg_bold_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_bold_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_bold_font, f_u_ndbg_bold_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_bold_font).

/*

### Compiled:  `U::NDBG-BOLD-FONT` 
*/
f_u_ndbg_bold_font(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_begin_bold_font, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_bold_font, classof, claz_macro),
   set_opv(u_ndbg_bold_font, compile_as, kw_operator),
   set_opv(u_ndbg_bold_font, function, f_u_ndbg_bold_font),
   _Ignored4=u_ndbg_bold_font.
/*
:- side_effect(assert_lsp(u_ndbg_bold_font,
			  wl:lambda_def(defmacro, u_ndbg_bold_font, f_u_ndbg_bold_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_bold_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_bold_font,
			  wl:arglist_info(u_ndbg_bold_font, f_u_ndbg_bold_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_bold_font,
			  wl:init_args(exact_only, f_u_ndbg_bold_font))).
*/
/*
(defmacro ndbg-italic-font (stream key)
  `(if-interested-in ,key (begin-italic-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5604 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-italic-font',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['begin-italic-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_italic_font, f_u_ndbg_italic_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_italic_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_italic_font, f_u_ndbg_italic_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_italic_font).

/*

### Compiled:  `U::NDBG-ITALIC-FONT` 
*/
f_u_ndbg_italic_font(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_begin_italic_font, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_italic_font, classof, claz_macro),
   set_opv(u_ndbg_italic_font, compile_as, kw_operator),
   set_opv(u_ndbg_italic_font, function, f_u_ndbg_italic_font),
   _Ignored4=u_ndbg_italic_font.
/*
:- side_effect(assert_lsp(u_ndbg_italic_font,
			  wl:lambda_def(defmacro, u_ndbg_italic_font, f_u_ndbg_italic_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_italic_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_italic_font,
			  wl:arglist_info(u_ndbg_italic_font, f_u_ndbg_italic_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_italic_font,
			  wl:init_args(exact_only, f_u_ndbg_italic_font))).
*/
/*
(defmacro ndbg-slanted-font (stream key)
  `(if-interested-in ,key (begin-slanted-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5701 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-slanted-font',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['begin-slanted-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_slanted_font, f_u_ndbg_slanted_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_slanted_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_slanted_font, f_u_ndbg_slanted_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_slanted_font).

/*

### Compiled:  `U::NDBG-SLANTED-FONT` 
*/
f_u_ndbg_slanted_font(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_begin_slanted_font, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_slanted_font, classof, claz_macro),
   set_opv(u_ndbg_slanted_font, compile_as, kw_operator),
   set_opv(u_ndbg_slanted_font, function, f_u_ndbg_slanted_font),
   _Ignored4=u_ndbg_slanted_font.
/*
:- side_effect(assert_lsp(u_ndbg_slanted_font,
			  wl:lambda_def(defmacro, u_ndbg_slanted_font, f_u_ndbg_slanted_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_slanted_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_slanted_font,
			  wl:arglist_info(u_ndbg_slanted_font, f_u_ndbg_slanted_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_slanted_font,
			  wl:init_args(exact_only, f_u_ndbg_slanted_font))).
*/
/*
(defmacro ndbg-end-font (stream key)
  `(if-interested-in ,key (end-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5800 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-end-font',[stream,key],['#BQ',['if-interested-in',['#COMMA',key],['end-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_end_font, f_u_ndbg_end_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_end_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_end_font, f_u_ndbg_end_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ndbg_end_font).

/*

### Compiled:  `U::NDBG-END-FONT` 
*/
f_u_ndbg_end_font(Stream, Key, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(stream, Stream), bv(key, Key)|Global_env_Ret],
	get_var(Env, key, Key_Get),
	get_var(Env, stream, Stream_Get),
	[u_if_interested_in, Key_Get, [u_end_font, Stream_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_end_font, classof, claz_macro),
   set_opv(u_ndbg_end_font, compile_as, kw_operator),
   set_opv(u_ndbg_end_font, function, f_u_ndbg_end_font),
   _Ignored4=u_ndbg_end_font.
/*
:- side_effect(assert_lsp(u_ndbg_end_font,
			  wl:lambda_def(defmacro, u_ndbg_end_font, f_u_ndbg_end_font, [stream, key], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_end_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_end_font,
			  wl:arglist_info(u_ndbg_end_font, f_u_ndbg_end_font, [stream, key], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, key], opt:0, req:[stream, key], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_end_font,
			  wl:init_args(exact_only, f_u_ndbg_end_font))).
*/
/*
(defmacro ndbg-roman (stream key . rest)
  `(if-interested-in ,key
                     (begin-roman-font ,stream)
                     (ndbg ,stream ,key ,@rest)
                     (end-font ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5885 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-roman',[stream,key|rest],['#BQ',['if-interested-in',['#COMMA',key],['begin-roman-font',['#COMMA',stream]],[ndbg,['#COMMA',stream],['#COMMA',key],['#BQ-COMMA-ELIPSE',rest]],['end-font',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_roman, f_u_ndbg_roman, [stream, key|rest], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_roman_font, ['#COMMA', stream]], [u_ndbg, ['#COMMA', stream], ['#COMMA', key], ['#BQ-COMMA-ELIPSE', rest]], [u_end_font, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_roman, f_u_ndbg_roman, [stream, key, '&rest', rest], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[stream, key, rest], opt:0, req:[stream, key], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, f_u_ndbg_roman).

/*

### Compiled:  `U::NDBG-ROMAN` 
*/
f_u_ndbg_roman(Stream, Key, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(stream, Stream), bv(key, Key), bv(rest, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, key, Key_Get11),
	get_var(GEnv, rest, Rest_Get),
	get_var(GEnv, stream, Stream_Get10),
	[u_if_interested_in, Key_Get11, [u_begin_roman_font, Stream_Get10], [u_ndbg, Stream_Get10, Key_Get11|Rest_Get], [u_end_font, Stream_Get10]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_roman, classof, claz_macro),
   set_opv(u_ndbg_roman, compile_as, kw_operator),
   set_opv(u_ndbg_roman, function, f_u_ndbg_roman),
   _Ignored4=u_ndbg_roman.
/*
:- side_effect(assert_lsp(u_ndbg_roman,
			  wl:lambda_def(defmacro, u_ndbg_roman, f_u_ndbg_roman, [stream, key|rest], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_roman_font, ['#COMMA', stream]], [u_ndbg, ['#COMMA', stream], ['#COMMA', key], ['#BQ-COMMA-ELIPSE', rest]], [u_end_font, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_roman,
			  wl:arglist_info(u_ndbg_roman, f_u_ndbg_roman, [stream, key, '&rest', rest], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[stream, key, rest], opt:0, req:[stream, key], rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_roman, wl:init_args(2, f_u_ndbg_roman))).
*/
/*
(defmacro ndbg-roman-nl (stream key . rest)
  `(if-interested-in ,key
                     (begin-roman-font ,stream)
                     (ndbg ,stream ,key ,@rest)
                     (end-font ,stream)
                     (do-newline ,stream)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6091 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ndbg-roman-nl',[stream,key|rest],['#BQ',['if-interested-in',['#COMMA',key],['begin-roman-font',['#COMMA',stream]],[ndbg,['#COMMA',stream],['#COMMA',key],['#BQ-COMMA-ELIPSE',rest]],['end-font',['#COMMA',stream]],['do-newline',['#COMMA',stream]]]]])
wl:lambda_def(defmacro, u_ndbg_roman_nl, f_u_ndbg_roman_nl, [stream, key|rest], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_roman_font, ['#COMMA', stream]], [u_ndbg, ['#COMMA', stream], ['#COMMA', key], ['#BQ-COMMA-ELIPSE', rest]], [u_end_font, ['#COMMA', stream]], [u_do_newline, ['#COMMA', stream]]]]]).
wl:arglist_info(u_ndbg_roman_nl, f_u_ndbg_roman_nl, [stream, key, '&rest', rest], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[stream, key, rest], opt:0, req:[stream, key], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, f_u_ndbg_roman_nl).

/*

### Compiled:  `U::NDBG-ROMAN-NL` 
*/
f_u_ndbg_roman_nl(Stream, Key, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(stream, Stream), bv(key, Key), bv(rest, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, key, Key_Get11),
	get_var(GEnv, rest, Rest_Get),
	get_var(GEnv, stream, Stream_Get10),
	[u_if_interested_in, Key_Get11, [u_begin_roman_font, Stream_Get10], [u_ndbg, Stream_Get10, Key_Get11|Rest_Get], [u_end_font, Stream_Get10], [u_do_newline, Stream_Get10]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_roman_nl, classof, claz_macro),
   set_opv(u_ndbg_roman_nl, compile_as, kw_operator),
   set_opv(u_ndbg_roman_nl, function, f_u_ndbg_roman_nl),
   _Ignored4=u_ndbg_roman_nl.
/*
:- side_effect(assert_lsp(u_ndbg_roman_nl,
			  wl:lambda_def(defmacro, u_ndbg_roman_nl, f_u_ndbg_roman_nl, [stream, key|rest], [progn, ['#BQ', [u_if_interested_in, ['#COMMA', key], [u_begin_roman_font, ['#COMMA', stream]], [u_ndbg, ['#COMMA', stream], ['#COMMA', key], ['#BQ-COMMA-ELIPSE', rest]], [u_end_font, ['#COMMA', stream]], [u_do_newline, ['#COMMA', stream]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_roman_nl,
			  wl:arglist_info(u_ndbg_roman_nl, f_u_ndbg_roman_nl, [stream, key, '&rest', rest], arginfo{all:[stream, key], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[stream, key, rest], opt:0, req:[stream, key], rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ndbg_roman_nl, wl:init_args(2, f_u_ndbg_roman_nl))).
*/
/*
(defmacro ob$create (spec)
  `(ob$readlist ,spec))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6342 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$create',[spec],['#BQ',['ob$readlist',['#COMMA',spec]]]])
wl:lambda_def(defmacro, u_ob_c36_create, f_u_ob_c36_create, [u_spec], [progn, ['#BQ', [u_ob_c36_readlist, ['#COMMA', u_spec]]]]).
wl:arglist_info(u_ob_c36_create, f_u_ob_c36_create, [u_spec], arginfo{all:[u_spec], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_spec], opt:0, req:[u_spec], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_create).

/*

### Compiled:  `U::OB$CREATE` 
*/
f_u_ob_c36_create(Spec, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_spec, Spec)|Global_env_Ret],
	get_var(Env, u_spec, Spec_Get),
	[u_ob_c36_readlist, Spec_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_create, classof, claz_macro),
   set_opv(u_ob_c36_create, compile_as, kw_operator),
   set_opv(u_ob_c36_create, function, f_u_ob_c36_create),
   _Ignored4=u_ob_c36_create.
/*
:- side_effect(assert_lsp(u_ob_c36_create,
			  wl:lambda_def(defmacro, u_ob_c36_create, f_u_ob_c36_create, [u_spec], [progn, ['#BQ', [u_ob_c36_readlist, ['#COMMA', u_spec]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_create,
			  wl:arglist_info(u_ob_c36_create, f_u_ob_c36_create, [u_spec], arginfo{all:[u_spec], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_spec], opt:0, req:[u_spec], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_create,
			  wl:init_args(exact_only, f_u_ob_c36_create))).
*/
/*
(defmacro ob$fcreate (spec)
  `(ob$freadlist ,spec))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6394 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$fcreate',[spec],['#BQ',['ob$freadlist',['#COMMA',spec]]]])
wl:lambda_def(defmacro, u_ob_c36_fcreate, f_u_ob_c36_fcreate, [u_spec], [progn, ['#BQ', [u_ob_c36_freadlist, ['#COMMA', u_spec]]]]).
wl:arglist_info(u_ob_c36_fcreate, f_u_ob_c36_fcreate, [u_spec], arginfo{all:[u_spec], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_spec], opt:0, req:[u_spec], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_fcreate).

/*

### Compiled:  `U::OB$FCREATE` 
*/
f_u_ob_c36_fcreate(Spec, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_spec, Spec)|Global_env_Ret],
	get_var(Env, u_spec, Spec_Get),
	[u_ob_c36_freadlist, Spec_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_fcreate, classof, claz_macro),
   set_opv(u_ob_c36_fcreate, compile_as, kw_operator),
   set_opv(u_ob_c36_fcreate, function, f_u_ob_c36_fcreate),
   _Ignored4=u_ob_c36_fcreate.
/*
:- side_effect(assert_lsp(u_ob_c36_fcreate,
			  wl:lambda_def(defmacro, u_ob_c36_fcreate, f_u_ob_c36_fcreate, [u_spec], [progn, ['#BQ', [u_ob_c36_freadlist, ['#COMMA', u_spec]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_fcreate,
			  wl:arglist_info(u_ob_c36_fcreate, f_u_ob_c36_fcreate, [u_spec], arginfo{all:[u_spec], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_spec], opt:0, req:[u_spec], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_fcreate,
			  wl:init_args(exact_only, f_u_ob_c36_fcreate))).
*/
/*
(defmacro special-priority? (ob1 ob2)
  `(cond
    ((not (special? ,ob1)) nil)
    ((not (special? ,ob2)) t)
    ((eq? (ob$ty ,ob1) (ob$ty ,ob2)) t)
    (else (memq? ,ob2 (memq ,ob1 *special-priorities*)))))
; REALLY: one should really be memq? and the other memq.

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6448 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'special-priority?',[ob1,ob2],['#BQ',[cond,[[not,['special?',['#COMMA',ob1]]],[]],[[not,['special?',['#COMMA',ob2]]],t],[['eq?',['ob$ty',['#COMMA',ob1]],['ob$ty',['#COMMA',ob2]]],t],[else,['memq?',['#COMMA',ob2],[memq,['#COMMA',ob1],'*special-priorities*']]]]]])
wl:lambda_def(defmacro, u_special_priority_c63, f_u_special_priority_c63, [u_ob1, u_ob2], [progn, ['#BQ', [cond, [[not, [u_special_c63, ['#COMMA', u_ob1]]], []], [[not, [u_special_c63, ['#COMMA', u_ob2]]], t], [[u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_ob1]], [u_ob_c36_ty, ['#COMMA', u_ob2]]], t], [u_else, [u_memq_c63, ['#COMMA', u_ob2], [ext_memq, ['#COMMA', u_ob1], u_xx_special_priorities_xx]]]]]]).
wl:arglist_info(u_special_priority_c63, f_u_special_priority_c63, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_special_priority_c63).

/*

### Compiled:  `U::SPECIAL-PRIORITY?` 
*/
f_u_special_priority_c63(Ob1, Ob2, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2)|Global_env_Ret],
	get_var(Env, u_ob1, Ob1_Get9),
	get_var(Env, u_ob2, Ob2_Get10),
	[cond, [[not, [u_special_c63, Ob1_Get9]], []], [[not, [u_special_c63, Ob2_Get10]], t], [[u_eq_c63, [u_ob_c36_ty, Ob1_Get9], [u_ob_c36_ty, Ob2_Get10]], t], [u_else, [u_memq_c63, Ob2_Get10, [ext_memq, Ob1_Get9, u_xx_special_priorities_xx]]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_special_priority_c63, classof, claz_macro),
   set_opv(u_special_priority_c63, compile_as, kw_operator),
   set_opv(u_special_priority_c63, function, f_u_special_priority_c63),
   _Ignored4=u_special_priority_c63.
/*
:- side_effect(assert_lsp(u_special_priority_c63,
			  wl:lambda_def(defmacro, u_special_priority_c63, f_u_special_priority_c63, [u_ob1, u_ob2], [progn, ['#BQ', [cond, [[not, [u_special_c63, ['#COMMA', u_ob1]]], []], [[not, [u_special_c63, ['#COMMA', u_ob2]]], t], [[u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_ob1]], [u_ob_c36_ty, ['#COMMA', u_ob2]]], t], [u_else, [u_memq_c63, ['#COMMA', u_ob2], [ext_memq, ['#COMMA', u_ob1], u_xx_special_priorities_xx]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_special_priority_c63,
			  wl:arglist_info(u_special_priority_c63, f_u_special_priority_c63, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_special_priority_c63,
			  wl:init_args(exact_only, f_u_special_priority_c63))).
*/
/*
 REALLY: one should really be memq? and the other memq.
*/
/*
(defmacro old-special-priority? (ob1 ob2)
  `(cond
    ((not (special? ,ob1)) nil)
    ((not (special? ,ob2)) t)
    ((ty$instance? ,ob1 'uor) t)
    ((and (ty$instance? ,ob1 'uand)
          (ty$instance? ,ob2 'uor))
     nil)
    ((ty$instance? ,ob1 'uand) t)
    ((and (ty$instance? ,ob1 'unot)
          (or (ty$instance? ,ob2 'uor)
              (ty$instance? ,ob2 'uand)))
     nil)
    ((ty$instance? ,ob1 'unot) t)
    ((and (ty?instance? ,ob1 'uproc)
          (or (ty$instance? ,ob2 'uor)
              (ty$instance? ,ob2 'uand)
              (ty$instance? ,ob2 'unot)))
     nil)
    (else t)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6714 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'old-special-priority?',[ob1,ob2],['#BQ',[cond,[[not,['special?',['#COMMA',ob1]]],[]],[[not,['special?',['#COMMA',ob2]]],t],[['ty$instance?',['#COMMA',ob1],[quote,uor]],t],[[and,['ty$instance?',['#COMMA',ob1],[quote,uand]],['ty$instance?',['#COMMA',ob2],[quote,uor]]],[]],[['ty$instance?',['#COMMA',ob1],[quote,uand]],t],[[and,['ty$instance?',['#COMMA',ob1],[quote,unot]],[or,['ty$instance?',['#COMMA',ob2],[quote,uor]],['ty$instance?',['#COMMA',ob2],[quote,uand]]]],[]],[['ty$instance?',['#COMMA',ob1],[quote,unot]],t],[[and,['ty?instance?',['#COMMA',ob1],[quote,uproc]],[or,['ty$instance?',['#COMMA',ob2],[quote,uor]],['ty$instance?',['#COMMA',ob2],[quote,uand]],['ty$instance?',['#COMMA',ob2],[quote,unot]]]],[]],[else,t]]]])
wl:lambda_def(defmacro, u_old_special_priority_c63, f_u_old_special_priority_c63, [u_ob1, u_ob2], [progn, ['#BQ', [cond, [[not, [u_special_c63, ['#COMMA', u_ob1]]], []], [[not, [u_special_c63, ['#COMMA', u_ob2]]], t], [[u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uor]], t], [[and, [u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uand]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uor]]], []], [[u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uand]], t], [[and, [u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_unot]], [or, [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uor]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uand]]]], []], [[u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_unot]], t], [[and, [u_ty_c63_instance_c63, ['#COMMA', u_ob1], [quote, u_uproc]], [or, [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uor]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uand]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_unot]]]], []], [u_else, t]]]]).
wl:arglist_info(u_old_special_priority_c63, f_u_old_special_priority_c63, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_old_special_priority_c63).

/*

### Compiled:  `U::OLD-SPECIAL-PRIORITY?` 
*/
f_u_old_special_priority_c63(Ob1, Ob2, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2)|Global_env_Ret],
	get_var(Env, u_ob1, Ob1_Get9),
	( get_var(Env, u_ob1, Ob1_Get17),
	  get_var(Env, u_ob2, Ob2_Get)
	),
	get_var(Env, u_ob2, Ob2_Get11),
	[cond, [[not, [u_special_c63, Ob1_Get9]], []], [[not, [u_special_c63, Ob2_Get]], t], [[u_ty_c36_instance_c63, Ob1_Get9, [quote, u_uor]], t], [[and, [u_ty_c36_instance_c63, Ob1_Get9, [quote, u_uand]], [u_ty_c36_instance_c63, Ob2_Get11, [quote, u_uor]]], []], [[u_ty_c36_instance_c63, Ob1_Get9, [quote, u_uand]], t], [[and, [u_ty_c36_instance_c63, Ob1_Get9, [quote, u_unot]], [or, [u_ty_c36_instance_c63, Ob2_Get11, [quote, u_uor]], [u_ty_c36_instance_c63, Ob2_Get11, [quote, u_uand]]]], []], [[u_ty_c36_instance_c63, Ob1_Get9, [quote, u_unot]], t], [[and, [u_ty_c63_instance_c63, Ob1_Get17, [quote, u_uproc]], [or, [u_ty_c36_instance_c63, Ob2_Get11, [quote, u_uor]], [u_ty_c36_instance_c63, Ob2_Get11, [quote, u_uand]], [u_ty_c36_instance_c63, Ob2_Get11, [quote, u_unot]]]], []], [u_else, t]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_old_special_priority_c63, classof, claz_macro),
   set_opv(u_old_special_priority_c63, compile_as, kw_operator),
   set_opv(u_old_special_priority_c63, function, f_u_old_special_priority_c63),
   _Ignored4=u_old_special_priority_c63.
/*
:- side_effect(assert_lsp(u_old_special_priority_c63,
			  wl:lambda_def(defmacro, u_old_special_priority_c63, f_u_old_special_priority_c63, [u_ob1, u_ob2], [progn, ['#BQ', [cond, [[not, [u_special_c63, ['#COMMA', u_ob1]]], []], [[not, [u_special_c63, ['#COMMA', u_ob2]]], t], [[u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uor]], t], [[and, [u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uand]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uor]]], []], [[u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uand]], t], [[and, [u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_unot]], [or, [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uor]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uand]]]], []], [[u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_unot]], t], [[and, [u_ty_c63_instance_c63, ['#COMMA', u_ob1], [quote, u_uproc]], [or, [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uor]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uand]], [u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_unot]]]], []], [u_else, t]]]]))).
*/
/*
:- side_effect(assert_lsp(u_old_special_priority_c63,
			  wl:arglist_info(u_old_special_priority_c63, f_u_old_special_priority_c63, [u_ob1, u_ob2], arginfo{all:[u_ob1, u_ob2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2], opt:0, req:[u_ob1, u_ob2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_old_special_priority_c63,
			  wl:init_args(exact_only, f_u_old_special_priority_c63))).
*/
/*
(defmacro var-ty$instance? (x y)
  `(if (null? ,y)
       t
       (and (ob? ,x)
            (ty$instance-of? ,x ,y))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7321 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'var-ty$instance?',[x,y],['#BQ',[if,['null?',['#COMMA',y]],t,[and,['ob?',['#COMMA',x]],['ty$instance-of?',['#COMMA',x],['#COMMA',y]]]]]])
wl:lambda_def(defmacro, u_var_ty_c36_instance_c63, f_u_var_ty_c36_instance_c63, [u_x, u_y], [progn, ['#BQ', [if, [u_null_c63, ['#COMMA', u_y]], t, [and, [u_ob_c63, ['#COMMA', u_x]], [u_ty_c36_instance_of_c63, ['#COMMA', u_x], ['#COMMA', u_y]]]]]]).
wl:arglist_info(u_var_ty_c36_instance_c63, f_u_var_ty_c36_instance_c63, [u_x, u_y], arginfo{all:[u_x, u_y], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x, u_y], opt:0, req:[u_x, u_y], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_var_ty_c36_instance_c63).

/*

### Compiled:  `U::VAR-TY$INSTANCE?` 
*/
f_u_var_ty_c36_instance_c63(X, Y, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_x, X), bv(u_y, Y)|Global_env_Ret],
	get_var(Env, u_x, X_Get9),
	get_var(Env, u_y, Y_Get10),
	[if, [u_null_c63, Y_Get10], t, [and, [u_ob_c63, X_Get9], [u_ty_c36_instance_of_c63, X_Get9, Y_Get10]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_var_ty_c36_instance_c63, classof, claz_macro),
   set_opv(u_var_ty_c36_instance_c63, compile_as, kw_operator),
   set_opv(u_var_ty_c36_instance_c63, function, f_u_var_ty_c36_instance_c63),
   _Ignored4=u_var_ty_c36_instance_c63.
/*
:- side_effect(assert_lsp(u_var_ty_c36_instance_c63,
			  wl:lambda_def(defmacro, u_var_ty_c36_instance_c63, f_u_var_ty_c36_instance_c63, [u_x, u_y], [progn, ['#BQ', [if, [u_null_c63, ['#COMMA', u_y]], t, [and, [u_ob_c63, ['#COMMA', u_x]], [u_ty_c36_instance_of_c63, ['#COMMA', u_x], ['#COMMA', u_y]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_var_ty_c36_instance_c63,
			  wl:arglist_info(u_var_ty_c36_instance_c63, f_u_var_ty_c36_instance_c63, [u_x, u_y], arginfo{all:[u_x, u_y], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x, u_y], opt:0, req:[u_x, u_y], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_var_ty_c36_instance_c63,
			  wl:init_args(exact_only, f_u_var_ty_c36_instance_c63))).
*/
/*
(defmacro type-compatible-vars? (var1 var2)
  `(or *relax-unify-var*
      (null? (variable-type ,var1))
      (null? (variable-type ,var2))
      (memq? (variable-type ,var1) (ty$supertypes* (variable-type ,var2)))
      (memq? (variable-type ,var2) (ty$supertypes* (variable-type ,var1)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7442 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'type-compatible-vars?',[var1,var2],['#BQ',[or,'*relax-unify-var*',['null?',['variable-type',['#COMMA',var1]]],['null?',['variable-type',['#COMMA',var2]]],['memq?',['variable-type',['#COMMA',var1]],['ty$supertypes*',['variable-type',['#COMMA',var2]]]],['memq?',['variable-type',['#COMMA',var2]],['ty$supertypes*',['variable-type',['#COMMA',var1]]]]]]])
wl:lambda_def(defmacro, u_type_compatible_vars_c63, f_u_type_compatible_vars_c63, [u_var1, u_var2], [progn, ['#BQ', [or, u_xx_relax_unify_var_xx, [u_null_c63, [u_variable_type, ['#COMMA', u_var1]]], [u_null_c63, [u_variable_type, ['#COMMA', u_var2]]], [u_memq_c63, [u_variable_type, ['#COMMA', u_var1]], [u_ty_c36_supertypes_xx, [u_variable_type, ['#COMMA', u_var2]]]], [u_memq_c63, [u_variable_type, ['#COMMA', u_var2]], [u_ty_c36_supertypes_xx, [u_variable_type, ['#COMMA', u_var1]]]]]]]).
wl:arglist_info(u_type_compatible_vars_c63, f_u_type_compatible_vars_c63, [u_var1, u_var2], arginfo{all:[u_var1, u_var2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var1, u_var2], opt:0, req:[u_var1, u_var2], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_type_compatible_vars_c63).

/*

### Compiled:  `U::TYPE-COMPATIBLE-VARS?` 
*/
f_u_type_compatible_vars_c63(Var1, Var2, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_var1, Var1), bv(u_var2, Var2)|Global_env_Ret],
	get_var(Env, u_var1, Var1_Get9),
	get_var(Env, u_var2, Var2_Get10),
	[or, u_xx_relax_unify_var_xx, [u_null_c63, [u_variable_type, Var1_Get9]], [u_null_c63, [u_variable_type, Var2_Get10]], [u_memq_c63, [u_variable_type, Var1_Get9], [u_ty_c36_supertypes_xx, [u_variable_type, Var2_Get10]]], [u_memq_c63, [u_variable_type, Var2_Get10], [u_ty_c36_supertypes_xx, [u_variable_type, Var1_Get9]]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_type_compatible_vars_c63, classof, claz_macro),
   set_opv(u_type_compatible_vars_c63, compile_as, kw_operator),
   set_opv(u_type_compatible_vars_c63, function, f_u_type_compatible_vars_c63),
   _Ignored4=u_type_compatible_vars_c63.
/*
:- side_effect(assert_lsp(u_type_compatible_vars_c63,
			  wl:lambda_def(defmacro, u_type_compatible_vars_c63, f_u_type_compatible_vars_c63, [u_var1, u_var2], [progn, ['#BQ', [or, u_xx_relax_unify_var_xx, [u_null_c63, [u_variable_type, ['#COMMA', u_var1]]], [u_null_c63, [u_variable_type, ['#COMMA', u_var2]]], [u_memq_c63, [u_variable_type, ['#COMMA', u_var1]], [u_ty_c36_supertypes_xx, [u_variable_type, ['#COMMA', u_var2]]]], [u_memq_c63, [u_variable_type, ['#COMMA', u_var2]], [u_ty_c36_supertypes_xx, [u_variable_type, ['#COMMA', u_var1]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_type_compatible_vars_c63,
			  wl:arglist_info(u_type_compatible_vars_c63, f_u_type_compatible_vars_c63, [u_var1, u_var2], arginfo{all:[u_var1, u_var2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var1, u_var2], opt:0, req:[u_var1, u_var2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_type_compatible_vars_c63,
			  wl:init_args(exact_only, f_u_type_compatible_vars_c63))).
*/
/*
(defmacro with-inverse-setting-default-off (&rest body)
  `(let ((result nil))
     (inverse-setting-default-off)
     (setq result (progn ,@body))
     (inverse-setting-default-on)
     result))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7736 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'with-inverse-setting-default-off',['&rest',body],['#BQ',[let,[[result,[]]],['inverse-setting-default-off'],[setq,result,[progn,['#BQ-COMMA-ELIPSE',body]]],['inverse-setting-default-on'],result]]])
wl:lambda_def(defmacro, u_with_inverse_setting_default_off, f_u_with_inverse_setting_default_off, [c38_rest, u_body], [progn, ['#BQ', [let, [[u_result, []]], [u_inverse_setting_default_off], [setq, u_result, [progn, ['#BQ-COMMA-ELIPSE', u_body]]], [u_inverse_setting_default_on], u_result]]]).
wl:arglist_info(u_with_inverse_setting_default_off, f_u_with_inverse_setting_default_off, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}).
wl: init_args(0, f_u_with_inverse_setting_default_off).

/*

### Compiled:  `U::WITH-INVERSE-SETTING-DEFAULT-OFF` 
*/
f_u_with_inverse_setting_default_off(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(u_body, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, u_body, Body_Get),
	[let, [[u_result, []]], [u_inverse_setting_default_off], [setq, u_result, [progn|Body_Get]], [u_inverse_setting_default_on], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_with_inverse_setting_default_off, classof, claz_macro),
   set_opv(u_with_inverse_setting_default_off, compile_as, kw_operator),
   set_opv(u_with_inverse_setting_default_off,
	   function,
	   f_u_with_inverse_setting_default_off),
   _Ignored4=u_with_inverse_setting_default_off.
/*
:- side_effect(assert_lsp(u_with_inverse_setting_default_off,
			  wl:lambda_def(defmacro, u_with_inverse_setting_default_off, f_u_with_inverse_setting_default_off, [c38_rest, u_body], [progn, ['#BQ', [let, [[u_result, []]], [u_inverse_setting_default_off], [setq, u_result, [progn, ['#BQ-COMMA-ELIPSE', u_body]]], [u_inverse_setting_default_on], u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_with_inverse_setting_default_off,
			  wl:arglist_info(u_with_inverse_setting_default_off, f_u_with_inverse_setting_default_off, [c38_rest, u_body], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_body], opt:0, req:0, rest:[u_body], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_with_inverse_setting_default_off,
			  wl:init_args(0, f_u_with_inverse_setting_default_off))).
*/
/*
(defmacro bd-bind (var value bindings)
  `(if ,var ; this is for ob$unify-var which might pass a null var name.
       (cons 't (cons (list ,var ,value) (cdr ,bindings)))
       ,bindings))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7933 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'bd-bind',[var,value,bindings],['#BQ',[if,['#COMMA',var],[cons,[quote,t],[cons,[list,['#COMMA',var],['#COMMA',value]],[cdr,['#COMMA',bindings]]]],['#COMMA',bindings]]]])
wl:lambda_def(defmacro, u_bd_bind, f_u_bd_bind, [u_var, u_value, bindings], [progn, ['#BQ', [if, ['#COMMA', u_var], [cons, [quote, t], [cons, [list, ['#COMMA', u_var], ['#COMMA', u_value]], [cdr, ['#COMMA', bindings]]]], ['#COMMA', bindings]]]]).
wl:arglist_info(u_bd_bind, f_u_bd_bind, [u_var, u_value, bindings], arginfo{all:[u_var, u_value, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_value, bindings], opt:0, req:[u_var, u_value, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_bd_bind).

/*

### Compiled:  `U::BD-BIND` 
*/
f_u_bd_bind(Var, Value, Bindings, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_var, Var), bv(u_value, Value), bv(bindings, Bindings)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get11),
	get_var(Env, u_value, Value_Get),
	get_var(Env, u_var, Var_Get8),
	[if, Var_Get8, [cons, [quote, t], [cons, [list, Var_Get8, Value_Get], [cdr, Bindings_Get11]]], Bindings_Get11]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_bind, classof, claz_macro),
   set_opv(u_bd_bind, compile_as, kw_operator),
   set_opv(u_bd_bind, function, f_u_bd_bind),
   _Ignored4=u_bd_bind.
/*
:- side_effect(assert_lsp(u_bd_bind,
			  wl:lambda_def(defmacro, u_bd_bind, f_u_bd_bind, [u_var, u_value, bindings], [progn, ['#BQ', [if, ['#COMMA', u_var], [cons, [quote, t], [cons, [list, ['#COMMA', u_var], ['#COMMA', u_value]], [cdr, ['#COMMA', bindings]]]], ['#COMMA', bindings]]]]))).
*/
/*
:- side_effect(assert_lsp(u_bd_bind,
			  wl:arglist_info(u_bd_bind, f_u_bd_bind, [u_var, u_value, bindings], arginfo{all:[u_var, u_value, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_value, bindings], opt:0, req:[u_var, u_value, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_bd_bind, wl:init_args(exact_only, f_u_bd_bind))).
*/
/*
 this is for ob$unify-var which might pass a null var name.
*/
/*
(defmacro bd-bind! (var value bindings)
;  (if (null? bindings) (error "bd-bind!: null bindings))
  `(setf (cdr ,bindings) (cons (list ,var ,value) (cdr ,bindings))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8124 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'bd-bind!',[var,value,bindings],['#BQ',[setf,[cdr,['#COMMA',bindings]],[cons,[list,['#COMMA',var],['#COMMA',value]],[cdr,['#COMMA',bindings]]]]]])
wl:lambda_def(defmacro, u_bd_bind_c33, f_u_bd_bind_c33, [u_var, u_value, bindings], [progn, ['#BQ', [setf, [cdr, ['#COMMA', bindings]], [cons, [list, ['#COMMA', u_var], ['#COMMA', u_value]], [cdr, ['#COMMA', bindings]]]]]]).
wl:arglist_info(u_bd_bind_c33, f_u_bd_bind_c33, [u_var, u_value, bindings], arginfo{all:[u_var, u_value, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_value, bindings], opt:0, req:[u_var, u_value, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_bd_bind_c33).

/*

### Compiled:  `U::BD-BIND!` 
*/
f_u_bd_bind_c33(Var, Value, Bindings, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_var, Var), bv(u_value, Value), bv(bindings, Bindings)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get10),
	get_var(Env, u_value, Value_Get),
	get_var(Env, u_var, Var_Get),
	[setf, [cdr, Bindings_Get10], [cons, [list, Var_Get, Value_Get], [cdr, Bindings_Get10]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_bind_c33, classof, claz_macro),
   set_opv(u_bd_bind_c33, compile_as, kw_operator),
   set_opv(u_bd_bind_c33, function, f_u_bd_bind_c33),
   _Ignored4=u_bd_bind_c33.
/*
:- side_effect(assert_lsp(u_bd_bind_c33,
			  wl:lambda_def(defmacro, u_bd_bind_c33, f_u_bd_bind_c33, [u_var, u_value, bindings], [progn, ['#BQ', [setf, [cdr, ['#COMMA', bindings]], [cons, [list, ['#COMMA', u_var], ['#COMMA', u_value]], [cdr, ['#COMMA', bindings]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_bd_bind_c33,
			  wl:arglist_info(u_bd_bind_c33, f_u_bd_bind_c33, [u_var, u_value, bindings], arginfo{all:[u_var, u_value, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_value, bindings], opt:0, req:[u_var, u_value, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_bd_bind_c33,
			  wl:init_args(exact_only, f_u_bd_bind_c33))).
*/
/*
  (if (null? bindings) (error "bd-bind!: null bindings))
*/
/*
(defmacro bd-lookup (var bindings)
  `(and ,bindings
        (let ((found (assq ,var (cdr ,bindings))))
          (if found (cadr found) nil))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8292 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'bd-lookup',[var,bindings],['#BQ',[and,['#COMMA',bindings],[let,[[found,[assq,['#COMMA',var],[cdr,['#COMMA',bindings]]]]],[if,found,[cadr,found],[]]]]]])
wl:lambda_def(defmacro, u_bd_lookup, f_u_bd_lookup, [u_var, bindings], [progn, ['#BQ', [and, ['#COMMA', bindings], [let, [[u_found, [ext_assq, ['#COMMA', u_var], [cdr, ['#COMMA', bindings]]]]], [if, u_found, [cadr, u_found], []]]]]]).
wl:arglist_info(u_bd_lookup, f_u_bd_lookup, [u_var, bindings], arginfo{all:[u_var, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, bindings], opt:0, req:[u_var, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_bd_lookup).

/*

### Compiled:  `U::BD-LOOKUP` 
*/
f_u_bd_lookup(Var, Bindings, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_var, Var), bv(bindings, Bindings)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get9),
	get_var(Env, u_var, Var_Get),
	[and, Bindings_Get9, [let, [[u_found, [ext_assq, Var_Get, [cdr, Bindings_Get9]]]], [if, u_found, [cadr, u_found], []]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_lookup, classof, claz_macro),
   set_opv(u_bd_lookup, compile_as, kw_operator),
   set_opv(u_bd_lookup, function, f_u_bd_lookup),
   _Ignored4=u_bd_lookup.
/*
:- side_effect(assert_lsp(u_bd_lookup,
			  wl:lambda_def(defmacro, u_bd_lookup, f_u_bd_lookup, [u_var, bindings], [progn, ['#BQ', [and, ['#COMMA', bindings], [let, [[u_found, [ext_assq, ['#COMMA', u_var], [cdr, ['#COMMA', bindings]]]]], [if, u_found, [cadr, u_found], []]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_bd_lookup,
			  wl:arglist_info(u_bd_lookup, f_u_bd_lookup, [u_var, bindings], arginfo{all:[u_var, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, bindings], opt:0, req:[u_var, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_bd_lookup, wl:init_args(exact_only, f_u_bd_lookup))).
*/
/*
(defmacro bd-hyper-lookup (var bd)
  `(bd-hyper-lookup1 ,var ,bd nil nil))

;
; Extra level to print debugging information.
; Should never be used from the top-level.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8438 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'bd-hyper-lookup',[var,bd],['#BQ',['bd-hyper-lookup1',['#COMMA',var],['#COMMA',bd],[],[]]]])
wl:lambda_def(defmacro, u_bd_hyper_lookup, f_u_bd_hyper_lookup, [u_var, u_bd], [progn, ['#BQ', [u_bd_hyper_lookup1, ['#COMMA', u_var], ['#COMMA', u_bd], [], []]]]).
wl:arglist_info(u_bd_hyper_lookup, f_u_bd_hyper_lookup, [u_var, u_bd], arginfo{all:[u_var, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd], opt:0, req:[u_var, u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_bd_hyper_lookup).

/*

### Compiled:  `U::BD-HYPER-LOOKUP` 
*/
f_u_bd_hyper_lookup(Var, Bd, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_var, Var), bv(u_bd, Bd)|Global_env_Ret],
	get_var(Env, u_bd, Bd_Get),
	get_var(Env, u_var, Var_Get),
	[u_bd_hyper_lookup1, Var_Get, Bd_Get, [], []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_hyper_lookup, classof, claz_macro),
   set_opv(u_bd_hyper_lookup, compile_as, kw_operator),
   set_opv(u_bd_hyper_lookup, function, f_u_bd_hyper_lookup),
   _Ignored4=u_bd_hyper_lookup.
/*
:- side_effect(assert_lsp(u_bd_hyper_lookup,
			  wl:lambda_def(defmacro, u_bd_hyper_lookup, f_u_bd_hyper_lookup, [u_var, u_bd], [progn, ['#BQ', [u_bd_hyper_lookup1, ['#COMMA', u_var], ['#COMMA', u_bd], [], []]]]))).
*/
/*
:- side_effect(assert_lsp(u_bd_hyper_lookup,
			  wl:arglist_info(u_bd_hyper_lookup, f_u_bd_hyper_lookup, [u_var, u_bd], arginfo{all:[u_var, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd], opt:0, req:[u_var, u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_bd_hyper_lookup,
			  wl:init_args(exact_only, f_u_bd_hyper_lookup))).
*/
/*
*/
/*
 Extra level to print debugging information.
*/
/*
 Should never be used from the top-level.
*/
/*
*/
/*
(defmacro ob$unify2 (ob1 ob2 bindings ignore-slots)
  `(if *unify-debugging?*
       (ob$unify-dbg ,ob1 ,ob2 ,bindings ,ignore-slots)
       (ob$unify0 ,ob1 ,ob2 ,bindings ,ignore-slots)))

;
; Top-level unifier call
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8607 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$unify2',[ob1,ob2,bindings,'ignore-slots'],['#BQ',[if,'*unify-debugging?*',['ob$unify-dbg',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],['#COMMA','ignore-slots']],['ob$unify0',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],['#COMMA','ignore-slots']]]]])
wl:lambda_def(defmacro, u_ob_c36_unify2, f_u_ob_c36_unify2, [u_ob1, u_ob2, bindings, u_ignore_slots], [progn, ['#BQ', [if, u_xx_unify_debugging_c63_xx, [u_ob_c36_unify_dbg, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]], [u_ob_c36_unify0, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]]]]).
wl:arglist_info(u_ob_c36_unify2, f_u_ob_c36_unify2, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify2).

/*

### Compiled:  `U::OB$UNIFY2` 
*/
f_u_ob_c36_unify2(Ob1, Ob2, Bindings, Ignore_slots, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get13),
	get_var(Env, u_ignore_slots, Ignore_slots_Get14),
	get_var(Env, u_ob1, Ob1_Get11),
	get_var(Env, u_ob2, Ob2_Get12),
	[if, u_xx_unify_debugging_c63_xx, [u_ob_c36_unify_dbg, Ob1_Get11, Ob2_Get12, Bindings_Get13, Ignore_slots_Get14], [u_ob_c36_unify0, Ob1_Get11, Ob2_Get12, Bindings_Get13, Ignore_slots_Get14]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify2, classof, claz_macro),
   set_opv(u_ob_c36_unify2, compile_as, kw_operator),
   set_opv(u_ob_c36_unify2, function, f_u_ob_c36_unify2),
   _Ignored4=u_ob_c36_unify2.
/*
:- side_effect(assert_lsp(u_ob_c36_unify2,
			  wl:lambda_def(defmacro, u_ob_c36_unify2, f_u_ob_c36_unify2, [u_ob1, u_ob2, bindings, u_ignore_slots], [progn, ['#BQ', [if, u_xx_unify_debugging_c63_xx, [u_ob_c36_unify_dbg, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]], [u_ob_c36_unify0, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify2,
			  wl:arglist_info(u_ob_c36_unify2, f_u_ob_c36_unify2, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify2,
			  wl:init_args(exact_only, f_u_ob_c36_unify2))).
*/
/*
*/
/*
 Top-level unifier call
*/
/*
*/
/*
(defmacro ob$unify1 (ob1 ob2 bindings ignore-slots)
  `(let ((already-matched *already-matched*)
         (result nil))
     (setq *diff?* nil)
     (setq *already-matched* (cons t nil))
     (setq result (ob$unify2 ,ob1 ,ob2 ,bindings ,ignore-slots))
     (setq *already-matched* already-matched)
     result))

;
; Top-level diffifier call
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8826 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$unify1',[ob1,ob2,bindings,'ignore-slots'],['#BQ',[let,[['already-matched','*already-matched*'],[result,[]]],[setq,'*diff?*',[]],[setq,'*already-matched*',[cons,t,[]]],[setq,result,['ob$unify2',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],['#COMMA','ignore-slots']]],[setq,'*already-matched*','already-matched'],result]]])
wl:lambda_def(defmacro, u_ob_c36_unify1, f_u_ob_c36_unify1, [u_ob1, u_ob2, bindings, u_ignore_slots], [progn, ['#BQ', [let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, []], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]]]).
wl:arglist_info(u_ob_c36_unify1, f_u_ob_c36_unify1, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify1).

/*

### Compiled:  `U::OB$UNIFY1` 
*/
f_u_ob_c36_unify1(Ob1, Ob2, Bindings, Ignore_slots, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get),
	get_var(Env, u_ignore_slots, Ignore_slots_Get),
	get_var(Env, u_ob1, Ob1_Get),
	get_var(Env, u_ob2, Ob2_Get),
	[let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, []], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, Ob1_Get, Ob2_Get, Bindings_Get, Ignore_slots_Get]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify1, classof, claz_macro),
   set_opv(u_ob_c36_unify1, compile_as, kw_operator),
   set_opv(u_ob_c36_unify1, function, f_u_ob_c36_unify1),
   _Ignored4=u_ob_c36_unify1.
/*
:- side_effect(assert_lsp(u_ob_c36_unify1,
			  wl:lambda_def(defmacro, u_ob_c36_unify1, f_u_ob_c36_unify1, [u_ob1, u_ob2, bindings, u_ignore_slots], [progn, ['#BQ', [let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, []], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify1,
			  wl:arglist_info(u_ob_c36_unify1, f_u_ob_c36_unify1, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify1,
			  wl:init_args(exact_only, f_u_ob_c36_unify1))).
*/
/*
*/
/*
 Top-level diffifier call
*/
/*
*/
/*
(defmacro ob$diff1 (ob1 ob2 bindings ignore-slots)
  `(let ((already-matched *already-matched*)
         (result nil))
     (setq *diff?* t)
     (setq *already-matched* (cons t nil))
     (setq result (ob$unify2 ,ob1 ,ob2 ,bindings ,ignore-slots))
     (setq *already-matched* already-matched)
     result))

;
; Top-level unifier call
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9170 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$diff1',[ob1,ob2,bindings,'ignore-slots'],['#BQ',[let,[['already-matched','*already-matched*'],[result,[]]],[setq,'*diff?*',t],[setq,'*already-matched*',[cons,t,[]]],[setq,result,['ob$unify2',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],['#COMMA','ignore-slots']]],[setq,'*already-matched*','already-matched'],result]]])
wl:lambda_def(defmacro, u_ob_c36_diff1, f_u_ob_c36_diff1, [u_ob1, u_ob2, bindings, u_ignore_slots], [progn, ['#BQ', [let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, t], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]]]).
wl:arglist_info(u_ob_c36_diff1, f_u_ob_c36_diff1, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_diff1).

/*

### Compiled:  `U::OB$DIFF1` 
*/
f_u_ob_c36_diff1(Ob1, Ob2, Bindings, Ignore_slots, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get),
	get_var(Env, u_ignore_slots, Ignore_slots_Get),
	get_var(Env, u_ob1, Ob1_Get),
	get_var(Env, u_ob2, Ob2_Get),
	[let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, t], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, Ob1_Get, Ob2_Get, Bindings_Get, Ignore_slots_Get]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_diff1, classof, claz_macro),
   set_opv(u_ob_c36_diff1, compile_as, kw_operator),
   set_opv(u_ob_c36_diff1, function, f_u_ob_c36_diff1),
   _Ignored4=u_ob_c36_diff1.
/*
:- side_effect(assert_lsp(u_ob_c36_diff1,
			  wl:lambda_def(defmacro, u_ob_c36_diff1, f_u_ob_c36_diff1, [u_ob1, u_ob2, bindings, u_ignore_slots], [progn, ['#BQ', [let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, t], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_diff1,
			  wl:arglist_info(u_ob_c36_diff1, f_u_ob_c36_diff1, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_diff1,
			  wl:init_args(exact_only, f_u_ob_c36_diff1))).
*/
/*
*/
/*
 Top-level unifier call
*/
/*
*/
/*
(defmacro ob$unify (ob1 ob2 bindings)
  `(ob$unify1 ,ob1 ,ob2 ,bindings nil))

;
; Top-level diffifier call
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9509 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$unify',[ob1,ob2,bindings],['#BQ',['ob$unify1',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],[]]]])
wl:lambda_def(defmacro, u_ob_c36_unify, f_u_ob_c36_unify, [u_ob1, u_ob2, bindings], [progn, ['#BQ', [u_ob_c36_unify1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], []]]]).
wl:arglist_info(u_ob_c36_unify, f_u_ob_c36_unify, [u_ob1, u_ob2, bindings], arginfo{all:[u_ob1, u_ob2, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings], opt:0, req:[u_ob1, u_ob2, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify).

/*

### Compiled:  `U::OB$UNIFY` 
*/
f_u_ob_c36_unify(Ob1, Ob2, Bindings, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get),
	get_var(Env, u_ob1, Ob1_Get),
	get_var(Env, u_ob2, Ob2_Get),
	[u_ob_c36_unify1, Ob1_Get, Ob2_Get, Bindings_Get, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify, classof, claz_macro),
   set_opv(u_ob_c36_unify, compile_as, kw_operator),
   set_opv(u_ob_c36_unify, function, f_u_ob_c36_unify),
   _Ignored4=u_ob_c36_unify.
/*
:- side_effect(assert_lsp(u_ob_c36_unify,
			  wl:lambda_def(defmacro, u_ob_c36_unify, f_u_ob_c36_unify, [u_ob1, u_ob2, bindings], [progn, ['#BQ', [u_ob_c36_unify1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], []]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify,
			  wl:arglist_info(u_ob_c36_unify, f_u_ob_c36_unify, [u_ob1, u_ob2, bindings], arginfo{all:[u_ob1, u_ob2, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings], opt:0, req:[u_ob1, u_ob2, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify,
			  wl:init_args(exact_only, f_u_ob_c36_unify))).
*/
/*
*/
/*
 Top-level diffifier call
*/
/*
*/
/*
(defmacro ob$diff (ob1 ob2 bindings)
  `(ob$diff1 ,ob1 ,ob2 ,bindings nil))

;
; Top-level unifier calls (with context)
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9619 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$diff',[ob1,ob2,bindings],['#BQ',['ob$diff1',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],[]]]])
wl:lambda_def(defmacro, u_ob_c36_diff, f_u_ob_c36_diff, [u_ob1, u_ob2, bindings], [progn, ['#BQ', [u_ob_c36_diff1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], []]]]).
wl:arglist_info(u_ob_c36_diff, f_u_ob_c36_diff, [u_ob1, u_ob2, bindings], arginfo{all:[u_ob1, u_ob2, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings], opt:0, req:[u_ob1, u_ob2, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_diff).

/*

### Compiled:  `U::OB$DIFF` 
*/
f_u_ob_c36_diff(Ob1, Ob2, Bindings, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get),
	get_var(Env, u_ob1, Ob1_Get),
	get_var(Env, u_ob2, Ob2_Get),
	[u_ob_c36_diff1, Ob1_Get, Ob2_Get, Bindings_Get, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_diff, classof, claz_macro),
   set_opv(u_ob_c36_diff, compile_as, kw_operator),
   set_opv(u_ob_c36_diff, function, f_u_ob_c36_diff),
   _Ignored4=u_ob_c36_diff.
/*
:- side_effect(assert_lsp(u_ob_c36_diff,
			  wl:lambda_def(defmacro, u_ob_c36_diff, f_u_ob_c36_diff, [u_ob1, u_ob2, bindings], [progn, ['#BQ', [u_ob_c36_diff1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], []]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_diff,
			  wl:arglist_info(u_ob_c36_diff, f_u_ob_c36_diff, [u_ob1, u_ob2, bindings], arginfo{all:[u_ob1, u_ob2, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings], opt:0, req:[u_ob1, u_ob2, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_diff,
			  wl:init_args(exact_only, f_u_ob_c36_diff))).
*/
/*
*/
/*
 Top-level unifier calls (with context)
*/
/*
*/
/*
(defmacro ob$unify-cx (ob1 ob2 bindings context)
  `(progn
    (setq *unify-context* ,context)
    (ob$unify1 ,ob1 ,ob2 ,bindings nil)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9741 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$unify-cx',[ob1,ob2,bindings,context],['#BQ',[progn,[setq,'*unify-context*',['#COMMA',context]],['ob$unify1',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],[]]]]])
wl:lambda_def(defmacro, u_ob_c36_unify_cx, f_u_ob_c36_unify_cx, [u_ob1, u_ob2, bindings, u_context], [progn, ['#BQ', [progn, [setq, u_xx_unify_context_xx, ['#COMMA', u_context]], [u_ob_c36_unify1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], []]]]]).
wl:arglist_info(u_ob_c36_unify_cx, f_u_ob_c36_unify_cx, [u_ob1, u_ob2, bindings, u_context], arginfo{all:[u_ob1, u_ob2, bindings, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_context], opt:0, req:[u_ob1, u_ob2, bindings, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify_cx).

/*

### Compiled:  `U::OB$UNIFY-CX` 
*/
f_u_ob_c36_unify_cx(Ob1, Ob2, Bindings, Context, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_context, Context)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get),
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_ob1, Ob1_Get),
	get_var(Env, u_ob2, Ob2_Get),
	[progn, [setq, u_xx_unify_context_xx, Context_Get], [u_ob_c36_unify1, Ob1_Get, Ob2_Get, Bindings_Get, []]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify_cx, classof, claz_macro),
   set_opv(u_ob_c36_unify_cx, compile_as, kw_operator),
   set_opv(u_ob_c36_unify_cx, function, f_u_ob_c36_unify_cx),
   _Ignored4=u_ob_c36_unify_cx.
/*
:- side_effect(assert_lsp(u_ob_c36_unify_cx,
			  wl:lambda_def(defmacro, u_ob_c36_unify_cx, f_u_ob_c36_unify_cx, [u_ob1, u_ob2, bindings, u_context], [progn, ['#BQ', [progn, [setq, u_xx_unify_context_xx, ['#COMMA', u_context]], [u_ob_c36_unify1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], []]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_cx,
			  wl:arglist_info(u_ob_c36_unify_cx, f_u_ob_c36_unify_cx, [u_ob1, u_ob2, bindings, u_context], arginfo{all:[u_ob1, u_ob2, bindings, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_context], opt:0, req:[u_ob1, u_ob2, bindings, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_cx,
			  wl:init_args(exact_only, f_u_ob_c36_unify_cx))).
*/
/*
(defmacro ob$unify-cx1 (ob1 ob2 bindings ignore-slots context)
  `(progn
    (setq *unify-context* ,context)
    (ob$unify1 ,ob1 ,ob2 ,bindings ,ignore-slots)))

;
; ob$compare: Compare two obs and produce a substitution
; binding list containing differences.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9879 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$unify-cx1',[ob1,ob2,bindings,'ignore-slots',context],['#BQ',[progn,[setq,'*unify-context*',['#COMMA',context]],['ob$unify1',['#COMMA',ob1],['#COMMA',ob2],['#COMMA',bindings],['#COMMA','ignore-slots']]]]])
wl:lambda_def(defmacro, u_ob_c36_unify_cx1, f_u_ob_c36_unify_cx1, [u_ob1, u_ob2, bindings, u_ignore_slots, u_context], [progn, ['#BQ', [progn, [setq, u_xx_unify_context_xx, ['#COMMA', u_context]], [u_ob_c36_unify1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]]]]).
wl:arglist_info(u_ob_c36_unify_cx1, f_u_ob_c36_unify_cx1, [u_ob1, u_ob2, bindings, u_ignore_slots, u_context], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify_cx1).

/*

### Compiled:  `U::OB$UNIFY-CX1` 
*/
f_u_ob_c36_unify_cx1(Ob1, Ob2, Bindings, Ignore_slots, Context, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots), bv(u_context, Context)|Global_env_Ret],
	get_var(Env, bindings, Bindings_Get),
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_ignore_slots, Ignore_slots_Get),
	get_var(Env, u_ob1, Ob1_Get),
	get_var(Env, u_ob2, Ob2_Get),
	[progn, [setq, u_xx_unify_context_xx, Context_Get], [u_ob_c36_unify1, Ob1_Get, Ob2_Get, Bindings_Get, Ignore_slots_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify_cx1, classof, claz_macro),
   set_opv(u_ob_c36_unify_cx1, compile_as, kw_operator),
   set_opv(u_ob_c36_unify_cx1, function, f_u_ob_c36_unify_cx1),
   _Ignored4=u_ob_c36_unify_cx1.
/*
:- side_effect(assert_lsp(u_ob_c36_unify_cx1,
			  wl:lambda_def(defmacro, u_ob_c36_unify_cx1, f_u_ob_c36_unify_cx1, [u_ob1, u_ob2, bindings, u_ignore_slots, u_context], [progn, ['#BQ', [progn, [setq, u_xx_unify_context_xx, ['#COMMA', u_context]], [u_ob_c36_unify1, ['#COMMA', u_ob1], ['#COMMA', u_ob2], ['#COMMA', bindings], ['#COMMA', u_ignore_slots]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_cx1,
			  wl:arglist_info(u_ob_c36_unify_cx1, f_u_ob_c36_unify_cx1, [u_ob1, u_ob2, bindings, u_ignore_slots, u_context], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_cx1,
			  wl:init_args(exact_only, f_u_ob_c36_unify_cx1))).
*/
/*
*/
/*
 ob$compare: Compare two obs and produce a substitution
*/
/*
 binding list containing differences.
*/
/*
*/
/*
(defmacro ob$compare (source target ignore-slots)
  `(let ((already-matched *already-matched*)
         (result nil))
     (setq *already-matched* (cons t nil))
     (setq result (ob$compare1 ,source
                               ,target
                               *empty-bd*
                               ,ignore-slots
                               (lambda (source target)
                                 (cond
                                  ((and (ty? source)
                                        (ty? target))
                                   (ty$least-common-supertype source target))
                                  ((and (ob$ty source)
                                        (ob$ty target))
                                   (ty$least-common-supertype (ob$ty source)
                                                           (ob$ty target)))
                                  (else nil)))))
     (setq *already-matched* already-matched)
     result))

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:10142 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,'ob$compare',[source,target,'ignore-slots'],['#BQ',[let,[['already-matched','*already-matched*'],[result,[]]],[setq,'*already-matched*',[cons,t,[]]],[setq,result,['ob$compare1',['#COMMA',source],['#COMMA',target],'*empty-bd*',['#COMMA','ignore-slots'],[lambda,[source,target],[cond,[[and,['ty?',source],['ty?',target]],['ty$least-common-supertype',source,target]],[[and,['ob$ty',source],['ob$ty',target]],['ty$least-common-supertype',['ob$ty',source],['ob$ty',target]]],[else,[]]]]]],[setq,'*already-matched*','already-matched'],result]]])
wl:lambda_def(defmacro, u_ob_c36_compare, f_u_ob_c36_compare, [ext_source, u_target, u_ignore_slots], [progn, ['#BQ', [let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_compare1, ['#COMMA', ext_source], ['#COMMA', u_target], u_xx_empty_bd_xx, ['#COMMA', u_ignore_slots], [lambda, [ext_source, u_target], [cond, [[and, [u_ty_c63, ext_source], [u_ty_c63, u_target]], [u_ty_c36_least_common_supertype, ext_source, u_target]], [[and, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]], [u_ty_c36_least_common_supertype, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]]], [u_else, []]]]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]]]).
wl:arglist_info(u_ob_c36_compare, f_u_ob_c36_compare, [ext_source, u_target, u_ignore_slots], arginfo{all:[ext_source, u_target, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[ext_source, u_target, u_ignore_slots], opt:0, req:[ext_source, u_target, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_compare).

/*

### Compiled:  `U::OB$COMPARE` 
*/
f_u_ob_c36_compare(Ext_source, Target, Ignore_slots, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(ext_source, Ext_source), bv(u_target, Target), bv(u_ignore_slots, Ignore_slots)|Global_env_Ret],
	get_var(Env, ext_source, Ext_source_Get),
	get_var(Env, u_ignore_slots, Ignore_slots_Get),
	get_var(Env, u_target, Target_Get),
	[let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_compare1, Ext_source_Get, Target_Get, u_xx_empty_bd_xx, Ignore_slots_Get, [lambda, [ext_source, u_target], [cond, [[and, [u_ty_c63, ext_source], [u_ty_c63, u_target]], [u_ty_c36_least_common_supertype, ext_source, u_target]], [[and, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]], [u_ty_c36_least_common_supertype, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]]], [u_else, []]]]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_compare, classof, claz_macro),
   set_opv(u_ob_c36_compare, compile_as, kw_operator),
   set_opv(u_ob_c36_compare, function, f_u_ob_c36_compare),
   _Ignored4=u_ob_c36_compare.
/*
:- side_effect(assert_lsp(u_ob_c36_compare,
			  wl:lambda_def(defmacro, u_ob_c36_compare, f_u_ob_c36_compare, [ext_source, u_target, u_ignore_slots], [progn, ['#BQ', [let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_compare1, ['#COMMA', ext_source], ['#COMMA', u_target], u_xx_empty_bd_xx, ['#COMMA', u_ignore_slots], [lambda, [ext_source, u_target], [cond, [[and, [u_ty_c63, ext_source], [u_ty_c63, u_target]], [u_ty_c36_least_common_supertype, ext_source, u_target]], [[and, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]], [u_ty_c36_least_common_supertype, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]]], [u_else, []]]]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_compare,
			  wl:arglist_info(u_ob_c36_compare, f_u_ob_c36_compare, [ext_source, u_target, u_ignore_slots], arginfo{all:[ext_source, u_target, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[ext_source, u_target, u_ignore_slots], opt:0, req:[ext_source, u_target, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_compare,
			  wl:init_args(exact_only, f_u_ob_c36_compare))).
*/
/*
 End of file.
*/


%; Total compilation time: 7.902 seconds


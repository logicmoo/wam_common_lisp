#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/logicmoo/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init-30" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Tue Jan 23 00:54:38 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
;; #+BUILTIN Means to ignore since it should already be defined
*/
/*
;; #+WAM-CL Means we want it
*/
/*
;; #+LISP500 Means probably we dont want it
*/
/*
;; #+ALT Alternative definition
*/
/*
;; #+ABCL From ABCL
*/
/*
;; #+SBCL From SBCL
*/
/*
;; #+ECL From ECL
*/
/*
;; #+SICL From SICL
*/
/*
(in-package "SYSTEM")


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:262 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','#:system'])
/*
% macroexpand:-[in_package,system7].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
#+(or WAM-CL LISP500) 
(defmacro check-type (place typespec &optional string)
  `(tagbody
    start
    (unless (typep ,place ',typespec)
      (restart-case
	  (error 'type-error :datum ,place :expected-type ',typespec)
	(store-value (value)
	  (setf ,place value)))
      (go start))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:294 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'check-type',[place,typespec,'&optional',string],['#BQ',[tagbody,start,[unless,[typep,['#COMMA',place],[quote,['#COMMA',typespec]]],['restart-case',[error,[quote,'type-error'],':datum',['#COMMA',place],':expected-type',[quote,['#COMMA',typespec]]],['store-value',[value],[setf,['#COMMA',place],value]]],[go,start]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       check_type,
					       kw_special,
					       sf_check_type)).
*/
wl:lambda_def(defmacro, check_type, mf_check_type, [sys_place, sys_typespec, c38_optional, string], [['#BQ', [tagbody, sys_start, [unless, [typep, ['#COMMA', sys_place], [quote, ['#COMMA', sys_typespec]]], [restart_case, [error, [quote, type_error], kw_datum, ['#COMMA', sys_place], kw_expected_type, [quote, ['#COMMA', sys_typespec]]], [store_value, [sys_value], [setf, ['#COMMA', sys_place], sys_value]]], [go, sys_start]]]]]).
wl:arglist_info(check_type, mf_check_type, [sys_place, sys_typespec, c38_optional, string], arginfo{all:[sys_place, sys_typespec, string], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_typespec, string], opt:[string], req:[sys_place, sys_typespec], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_check_type).

/*

### Compiled Macro Operator: `CL:CHECK-TYPE` 
*/
sf_check_type(MacroEnv, Place_In, Typespec_In, RestNKeys, FResult) :-
	mf_check_type([check_type, Place_In, Typespec_In|RestNKeys],
		      MacroEnv,
		      MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:CHECK-TYPE` 
*/
mf_check_type([check_type, Place_In, Typespec_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_place, Place_In), bv(sys_typespec, Typespec_In), bv(string, String_In)],
	opt_var(MacroEnv, string, String_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_place, Place_Get10),
		  ( get_var(GEnv, sys_place, Place_Get12),
		    get_var(GEnv, sys_typespec, Typespec_Get)
		  ),
		  get_var(GEnv, sys_typespec, Typespec_Get11)
		),
		[tagbody, sys_start, [unless, [typep, Place_Get10, [quote, Typespec_Get]], [restart_case, [error, [quote, type_error], kw_datum, Place_Get10, kw_expected_type, [quote, Typespec_Get11]], [store_value, [sys_value], [setf, Place_Get12, sys_value]]], [go, sys_start]]]=MFResult
	      ),
	      block_exit(check_type, MFResult),
	      true).
:- set_opv(mf_check_type, type_of, sys_macro),
   set_opv(check_type, symbol_function, mf_check_type),
   DefMacroResult=check_type.
/*
:- side_effect(assert_lsp(check_type,
			  lambda_def(defmacro,
				     check_type,
				     mf_check_type,
				     
				     [ sys_place,
				       sys_typespec,
				       c38_optional,
				       string
				     ],
				     
				     [ 
				       [ '#BQ',
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ typep,
					       ['#COMMA', sys_place],
					       [quote, ['#COMMA', sys_typespec]]
					     ],
					     
					     [ restart_case,
					       
					       [ error,
						 [quote, type_error],
						 kw_datum,
						 ['#COMMA', sys_place],
						 kw_expected_type,
						 
						 [ quote,
						   ['#COMMA', sys_typespec]
						 ]
					       ],
					       
					       [ store_value,
						 [sys_value],
						 
						 [ setf,
						   ['#COMMA', sys_place],
						   sys_value
						 ]
					       ]
					     ],
					     [go, sys_start]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(check_type,
			  arglist_info(check_type,
				       mf_check_type,
				       
				       [ sys_place,
					 sys_typespec,
					 c38_optional,
					 string
				       ],
				       arginfo{ all:
						    [ sys_place,
						      sys_typespec,
						      string
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_place,
							sys_typespec,
							string
						      ],
						opt:[string],
						req:[sys_place, sys_typespec],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(check_type, init_args(2, mf_check_type))).
*/
/*
#+(or WAM-CL LISP500) 
(defun abort (&optional condition)
  (invoke-restart (find-restart 'abort condition))
  (error 'control-error))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:598 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,abort,['&optional',condition],['invoke-restart',['find-restart',[quote,abort],condition]],[error,[quote,'control-error']]])
wl:lambda_def(defun, abort, f_abort, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, abort], condition]], [error, [quote, control_error]]]).
wl:arglist_info(abort, f_abort, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(0, f_abort).

/*

### Compiled Function: `CL:ABORT` 
*/
f_abort(RestNKeys, FnResult) :-
	GEnv=[bv(condition, Condition_In)],
	opt_var(Env, condition, Condition_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, condition, Condition_Get),
		  f_find_restart(abort, [Condition_Get], Invoke_restart_Param),
		  f_invoke_restart(Invoke_restart_Param, [], Invoke_restart_Ret),
		  f_error([control_error], Error_Ret)
		),
		Error_Ret=FnResult
	      ),
	      block_exit(abort, FnResult),
	      true).
:- set_opv(abort, symbol_function, f_abort),
   DefunResult=abort.
/*
:- side_effect(assert_lsp(abort,
			  lambda_def(defun,
				     abort,
				     f_abort,
				     [c38_optional, condition],
				     
				     [ 
				       [ invoke_restart,
					 
					 [ find_restart,
					   [quote, abort],
					   condition
					 ]
				       ],
				       [error, [quote, control_error]]
				     ]))).
*/
/*
:- side_effect(assert_lsp(abort,
			  arglist_info(abort,
				       f_abort,
				       [c38_optional, condition],
				       arginfo{ all:[condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[condition],
						opt:[condition],
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(abort, init_args(0, f_abort))).
*/
/*
#+(or WAM-CL LISP500) 
(defun continue (&optional condition)
  (invoke-restart (find-restart 'continue condition)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:739 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,continue,['&optional',condition],['invoke-restart',['find-restart',[quote,continue],condition]]])
wl:lambda_def(defun, continue, f_continue, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, continue], condition]]]).
wl:arglist_info(continue, f_continue, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(0, f_continue).

/*

### Compiled Function: `CL:CONTINUE` 
*/
f_continue(RestNKeys, FnResult) :-
	GEnv=[bv(condition, Condition_In)],
	opt_var(Env, condition, Condition_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, condition, Condition_Get),
		  f_find_restart(continue,
				 [Condition_Get],
				 Invoke_restart_Param),
		  f_invoke_restart(Invoke_restart_Param, [], Invoke_restart_Ret)
		),
		Invoke_restart_Ret=FnResult
	      ),
	      block_exit(continue, FnResult),
	      true).
:- set_opv(continue, symbol_function, f_continue),
   DefunResult=continue.
/*
:- side_effect(assert_lsp(continue,
			  lambda_def(defun,
				     continue,
				     f_continue,
				     [c38_optional, condition],
				     
				     [ 
				       [ invoke_restart,
					 
					 [ find_restart,
					   [quote, continue],
					   condition
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(continue,
			  arglist_info(continue,
				       f_continue,
				       [c38_optional, condition],
				       arginfo{ all:[condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[condition],
						opt:[condition],
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(continue, init_args(0, f_continue))).
*/
/*
#+(or WAM-CL LISP500) 
(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart 'muffle-warning condition))
  (error 'control-error))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:860 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'muffle-warning',['&optional',condition],['invoke-restart',['find-restart',[quote,'muffle-warning'],condition]],[error,[quote,'control-error']]])
wl:lambda_def(defun, muffle_warning, f_muffle_warning, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, muffle_warning], condition]], [error, [quote, control_error]]]).
wl:arglist_info(muffle_warning, f_muffle_warning, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(0, f_muffle_warning).

/*

### Compiled Function: `CL:MUFFLE-WARNING` 
*/
f_muffle_warning(RestNKeys, FnResult) :-
	GEnv=[bv(condition, Condition_In)],
	opt_var(Env, condition, Condition_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, condition, Condition_Get),
		  f_find_restart(muffle_warning,
				 [Condition_Get],
				 Invoke_restart_Param),
		  f_invoke_restart(Invoke_restart_Param, [], Invoke_restart_Ret),
		  f_error([control_error], Error_Ret)
		),
		Error_Ret=FnResult
	      ),
	      block_exit(muffle_warning, FnResult),
	      true).
:- set_opv(muffle_warning, symbol_function, f_muffle_warning),
   DefunResult=muffle_warning.
/*
:- side_effect(assert_lsp(muffle_warning,
			  lambda_def(defun,
				     muffle_warning,
				     f_muffle_warning,
				     [c38_optional, condition],
				     
				     [ 
				       [ invoke_restart,
					 
					 [ find_restart,
					   [quote, muffle_warning],
					   condition
					 ]
				       ],
				       [error, [quote, control_error]]
				     ]))).
*/
/*
:- side_effect(assert_lsp(muffle_warning,
			  arglist_info(muffle_warning,
				       f_muffle_warning,
				       [c38_optional, condition],
				       arginfo{ all:[condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[condition],
						opt:[condition],
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(muffle_warning, init_args(0, f_muffle_warning))).
*/
/*
#+(or WAM-CL LISP500) 
(defun store-value (value &optional condition)
  (invoke-restart (find-restart 'store-value condition) value))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:1019 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'store-value',[value,'&optional',condition],['invoke-restart',['find-restart',[quote,'store-value'],condition],value]])
wl:lambda_def(defun, store_value, f_store_value, [sys_value, c38_optional, condition], [[invoke_restart, [find_restart, [quote, store_value], condition], sys_value]]).
wl:arglist_info(store_value, f_store_value, [sys_value, c38_optional, condition], arginfo{all:[sys_value, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value, condition], opt:[condition], req:[sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_store_value).

/*

### Compiled Function: `CL:STORE-VALUE` 
*/
f_store_value(Value_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_value, Value_In), bv(condition, Condition_In)],
	opt_var(Env, condition, Condition_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, condition, Condition_Get),
		  f_find_restart(store_value,
				 [Condition_Get],
				 Invoke_restart_Param),
		  get_var(GEnv, sys_value, Value_Get),
		  f_invoke_restart(Invoke_restart_Param,
				   [Value_Get],
				   Invoke_restart_Ret)
		),
		Invoke_restart_Ret=FnResult
	      ),
	      block_exit(store_value, FnResult),
	      true).
:- set_opv(store_value, symbol_function, f_store_value),
   DefunResult=store_value.
/*
:- side_effect(assert_lsp(store_value,
			  lambda_def(defun,
				     store_value,
				     f_store_value,
				     [sys_value, c38_optional, condition],
				     
				     [ 
				       [ invoke_restart,
					 
					 [ find_restart,
					   [quote, store_value],
					   condition
					 ],
					 sys_value
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(store_value,
			  arglist_info(store_value,
				       f_store_value,
				       [sys_value, c38_optional, condition],
				       arginfo{ all:[sys_value, condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_value, condition],
						opt:[condition],
						req:[sys_value],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(store_value, init_args(1, f_store_value))).
*/
/*
#+(or WAM-CL LISP500) 
(defun use-value (value &optional condition)
  (invoke-restart (find-restart 'use-value condition) value))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:1158 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'use-value',[value,'&optional',condition],['invoke-restart',['find-restart',[quote,'use-value'],condition],value]])
wl:lambda_def(defun, use_value, f_use_value, [sys_value, c38_optional, condition], [[invoke_restart, [find_restart, [quote, use_value], condition], sys_value]]).
wl:arglist_info(use_value, f_use_value, [sys_value, c38_optional, condition], arginfo{all:[sys_value, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value, condition], opt:[condition], req:[sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_use_value).

/*

### Compiled Function: `CL:USE-VALUE` 
*/
f_use_value(Value_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_value, Value_In), bv(condition, Condition_In)],
	opt_var(Env, condition, Condition_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, condition, Condition_Get),
		  f_find_restart(use_value,
				 [Condition_Get],
				 Invoke_restart_Param),
		  get_var(GEnv, sys_value, Value_Get),
		  f_invoke_restart(Invoke_restart_Param,
				   [Value_Get],
				   Invoke_restart_Ret)
		),
		Invoke_restart_Ret=FnResult
	      ),
	      block_exit(use_value, FnResult),
	      true).
:- set_opv(use_value, symbol_function, f_use_value),
   DefunResult=use_value.
/*
:- side_effect(assert_lsp(use_value,
			  lambda_def(defun,
				     use_value,
				     f_use_value,
				     [sys_value, c38_optional, condition],
				     
				     [ 
				       [ invoke_restart,
					 
					 [ find_restart,
					   [quote, use_value],
					   condition
					 ],
					 sys_value
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(use_value,
			  arglist_info(use_value,
				       f_use_value,
				       [sys_value, c38_optional, condition],
				       arginfo{ all:[sys_value, condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_value, condition],
						opt:[condition],
						req:[sys_value],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(use_value, init_args(1, f_use_value))).
*/
/*
#+(or WAM-CL LISP500) 
(defun integer-string (integer &optional (radix 10))
  (if (= integer 0)
      "0"
      (labels ((recur (i l)
		 (if (= i 0)
		     l
		     (multiple-value-bind (ni r)
			 (floor i radix)
		       (recur ni (cons (code-char (+ (if (< r 10) 48 55) r))
				       l))))))
	(apply #'string (if (< 0 integer)
			    (recur integer nil)
			    (cons (code-char 45) (recur (- integer) nil)))))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:1295 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'integer-string',[integer,'&optional',[radix,10]],[if,[=,integer,0],'$STRING'("0"),[labels,[[recur,[i,l],[if,[=,i,0],l,['multiple-value-bind',[ni,r],[floor,i,radix],[recur,ni,[cons,['code-char',[+,[if,[<,r,10],48,55],r]],l]]]]]],[apply,function(string),[if,[<,0,integer],[recur,integer,[]],[cons,['code-char',45],[recur,[-,integer],[]]]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_integer_string,
					       kw_function,
					       f_sys_integer_string)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_recur,
					       kw_function,
					       f_sys_recur)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_recur,
					       kw_function,
					       f_sys_recur)).
*/
wl:lambda_def(defun, sys_integer_string, f_sys_integer_string, [integer, c38_optional, [sys_radix, 10]], [[if, [=, integer, 0], '$ARRAY'([*], claz_base_character, "0"), [labels, [[sys_recur, [sys_i, sys_l], [if, [=, sys_i, 0], sys_l, [multiple_value_bind, [sys_ni, sys_r], [floor, sys_i, sys_radix], [sys_recur, sys_ni, [cons, [code_char, [+, [if, [<, sys_r, 10], 48, 55], sys_r]], sys_l]]]]]], [apply, function(string), [if, [<, 0, integer], [sys_recur, integer, []], [cons, [code_char, 45], [sys_recur, [-, integer], []]]]]]]]).
wl:arglist_info(sys_integer_string, f_sys_integer_string, [integer, c38_optional, [sys_radix, 10]], arginfo{all:[integer, sys_radix], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[integer, sys_radix], opt:[sys_radix], req:[integer], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_integer_string).

/*

### Compiled Function: `SYS::INTEGER-STRING` 
*/
f_sys_integer_string(Integer_In, RestNKeys, FnResult) :-
	Env11=[bv(integer, Integer_In), bv(sys_radix, Radix_In)],
	opt_var(Env, sys_radix, Radix_In, true, 10, 1, RestNKeys),
	catch(( ( get_var(Env11, integer, Integer_Get),
		  (   Integer_Get=:=0
		  ->  _8620='$ARRAY'([*], claz_base_character, "0")
		  ;   assert_lsp(sys_recur,
				 wl:lambda_def(defun, sys_recur, f_sys_recur1, [sys_i, sys_l], [[if, [=, sys_i, 0], sys_l, [multiple_value_bind, [sys_ni, sys_r], [floor, sys_i, sys_radix], [sys_recur, sys_ni, [cons, [code_char, [+, [if, [<, sys_r, 10], 48, 55], sys_r]], sys_l]]]]])),
		      assert_lsp(sys_recur,
				 wl:arglist_info(sys_recur, f_sys_recur1, [sys_i, sys_l], arginfo{all:[sys_i, sys_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_i, sys_l], opt:0, req:[sys_i, sys_l], rest:0, sublists:0, whole:0})),
		      assert_lsp(sys_recur, wl:init_args(2, f_sys_recur1)),
		      assert_lsp(sys_recur,
				 (f_sys_recur1(I_In, L_In, RestNKeys13, FnResult12):-GEnv=[bv(sys_i, I_In), bv(sys_l, L_In)], catch(((get_var(GEnv, sys_i, I_Get), (I_Get=:=0->get_var(GEnv, sys_l, L_Get), _8816=L_Get;LEnv=[bv(sys_ni, []), bv(sys_r, [])|GEnv], get_var(LEnv, sys_i, I_Get24), get_var(LEnv, sys_radix, Radix_Get), f_floor(I_Get24, [Radix_Get], Floor_Ret), setq_from_values(LEnv, [sys_ni, sys_r]), get_var(LEnv, sys_ni, Ni_Get), get_var(LEnv, sys_r, R_Get), (R_Get<10->_9030=48;_9030=55), get_var(LEnv, sys_r, R_Get31), 'f_+'(_9030, R_Get31, Code_char_Param), f_code_char(Code_char_Param, Code_char_Ret), get_var(LEnv, sys_l, L_Get32), _9024=[Code_char_Ret|L_Get32], f_sys_recur(Ni_Get, _9024, LetResult), _8816=LetResult)), _8816=FnResult12), block_exit(sys_recur, FnResult12), true))),
		      get_var(Env11, integer, Integer_Get37),
		      (   0<Integer_Get37
		      ->  get_var(Env11, integer, Integer_Get40),
			  f_sys_recur1(Integer_Get40, [], TrueResult42),
			  _9236=TrueResult42
		      ;   f_code_char(45, Code_char_Ret53),
			  get_var(Env11, integer, Integer_Get41),
			  'f_-'(0, Integer_Get41, Recur1_Param),
			  f_sys_recur1(Recur1_Param, [], KeysNRest),
			  ElseResult43=[Code_char_Ret53|KeysNRest],
			  _9236=ElseResult43
		      ),
		      f_apply(f_string, _9236, ElseResult44),
		      _8620=ElseResult44
		  )
		),
		_8620=FnResult
	      ),
	      block_exit(sys_integer_string, FnResult),
	      true).
:- set_opv(sys_integer_string, symbol_function, f_sys_integer_string),
   DefunResult=sys_integer_string.
/*
:- side_effect(assert_lsp(sys_integer_string,
			  lambda_def(defun,
				     sys_integer_string,
				     f_sys_integer_string,
				     [integer, c38_optional, [sys_radix, 10]],
				     
				     [ 
				       [ if,
					 [=, integer, 0],
					 '$ARRAY'([*], claz_base_character, "0"),
					 
					 [ labels,
					   
					   [ 
					     [ sys_recur,
					       [sys_i, sys_l],
					       
					       [ if,
						 [=, sys_i, 0],
						 sys_l,
						 
						 [ multiple_value_bind,
						   [sys_ni, sys_r],
						   [floor, sys_i, sys_radix],
						   
						   [ sys_recur,
						     sys_ni,
						     
						     [ cons,
						       
						       [ code_char,
							 
							 [ (+),
							   
							   [ if,
							     [<, sys_r, 10],
							     48,
							     55
							   ],
							   sys_r
							 ]
						       ],
						       sys_l
						     ]
						   ]
						 ]
					       ]
					     ]
					   ],
					   
					   [ apply,
					     function(string),
					     
					     [ if,
					       [<, 0, integer],
					       [sys_recur, integer, []],
					       
					       [ cons,
						 [code_char, 45],
						 [sys_recur, [-, integer], []]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_integer_string,
			  arglist_info(sys_integer_string,
				       f_sys_integer_string,
				       [integer, c38_optional, [sys_radix, 10]],
				       arginfo{ all:[integer, sys_radix],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[integer, sys_radix],
						opt:[sys_radix],
						req:[integer],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_integer_string,
			  init_args(1, f_sys_integer_string))).
*/
/*
#+(or WAM-CL LISP500)
(defun designator-symbol (designator)
  (if (symbolp designator)
      designator
      (find-symbol designator)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:1730 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-symbol',[designator],[if,[symbolp,designator],designator,['find-symbol',designator]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_designator_symbol,
					       kw_function,
					       f_sys_designator_symbol)).
*/
wl:lambda_def(defun, sys_designator_symbol, f_sys_designator_symbol, [sys_designator], [[if, [symbolp, sys_designator], sys_designator, [find_symbol, sys_designator]]]).
wl:arglist_info(sys_designator_symbol, f_sys_designator_symbol, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_designator_symbol).

/*

### Compiled Function: `SYS::DESIGNATOR-SYMBOL` 
*/
f_sys_designator_symbol(Designator_In, FnResult) :-
	GEnv=[bv(sys_designator, Designator_In)],
	catch(( ( get_var(GEnv, sys_designator, Designator_Get),
		  (   is_symbolp(Designator_Get)
		  ->  get_var(GEnv, sys_designator, Designator_Get9),
		      _9448=Designator_Get9
		  ;   get_var(GEnv, sys_designator, Designator_Get10),
		      f_find_symbol(Designator_Get10, ElseResult),
		      _9448=ElseResult
		  )
		),
		_9448=FnResult
	      ),
	      block_exit(sys_designator_symbol, FnResult),
	      true).
:- set_opv(sys_designator_symbol, symbol_function, f_sys_designator_symbol),
   DefunResult=sys_designator_symbol.
/*
:- side_effect(assert_lsp(sys_designator_symbol,
			  lambda_def(defun,
				     sys_designator_symbol,
				     f_sys_designator_symbol,
				     [sys_designator],
				     
				     [ 
				       [ if,
					 [symbolp, sys_designator],
					 sys_designator,
					 [find_symbol, sys_designator]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_symbol,
			  arglist_info(sys_designator_symbol,
				       f_sys_designator_symbol,
				       [sys_designator],
				       arginfo{ all:[sys_designator],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_designator],
						opt:0,
						req:[sys_designator],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_designator_symbol,
			  init_args(x, f_sys_designator_symbol))).
*/
/*
#+BUILTIN 
(defun symbolp (object) (or (null object) (eq (type-of object) 'symbol)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:1874 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[defun,symbolp,[object],[or,[null,object],[eq,['type-of',object],[quote,symbol]]]]]))
/*
#+BUILTIN 
(defun keywordp (object) 
(and (symbolp object)
       (string= (package-name (symbol-package object)) "KEYwORD")))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:1963 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[defun,keywordp,[object],[and,[symbolp,object],['string=',['package-name',['symbol-package',object]],'$STRING'("KEYwORD")]]]]))
/*
#+BUILTIN 
(defun make-symbol (name)
  (let ((symbol (makei 9 0 name nil nil nil nil (- 1) 0)))
    (imakunbound symbol 4)
    (imakunbound symbol 5)
    (imakunbound symbol 6)
    symbol))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:2096 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[defun,'make-symbol',[name],[let,[[symbol,[makei,9,0,name,[],[],[],[],[-,1],0]]],[imakunbound,symbol,4],[imakunbound,symbol,5],[imakunbound,symbol,6],symbol]]]))
/*
#+(or WAM-CL LISP500)
(defvar *gensym-counter* 0)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:2295 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*gensym-counter*',0])
:- set_var(AEnv, xx_gensym_counter_xx, 0).
/*
#+(or WAM-CL LISP500)
(defun gen-sym (&optional x)
  (let ((prefix (if (stringp x) x "G"))
	(suffix (if (fixnump x)
		    x
		    (let ((x *gensym-counter*))
		      (setf *gensym-counter* (+ 1 *gensym-counter*))))))
    (make-symbol (conc-string prefix (integer-string suffix)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:2347 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'gen-sym',['&optional',x],[let,[[prefix,[if,[stringp,x],x,'$STRING'("G")]],[suffix,[if,[fixnump,x],x,[let,[[x,'*gensym-counter*']],[setf,'*gensym-counter*',[+,1,'*gensym-counter*']]]]]],['make-symbol',['conc-string',prefix,['integer-string',suffix]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_gen_sym,
					       kw_function,
					       f_sys_gen_sym)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_conc_string,
					       kw_function,
					       f_sys_conc_string)).
*/
wl:lambda_def(defun, sys_gen_sym, f_sys_gen_sym, [c38_optional, sys_x], [[let, [[sys_prefix, [if, [stringp, sys_x], sys_x, '$ARRAY'([*], claz_base_character, "G")]], [sys_suffix, [if, [sys_fixnump, sys_x], sys_x, [let, [[sys_x, xx_gensym_counter_xx]], [setf, xx_gensym_counter_xx, [+, 1, xx_gensym_counter_xx]]]]]], [make_symbol, [sys_conc_string, sys_prefix, [sys_integer_string, sys_suffix]]]]]).
wl:arglist_info(sys_gen_sym, f_sys_gen_sym, [c38_optional, sys_x], arginfo{all:[sys_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x], opt:[sys_x], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(0, f_sys_gen_sym).

/*

### Compiled Function: `SYS::GEN-SYM` 
*/
f_sys_gen_sym(RestNKeys, FnResult) :-
	GEnv=[bv(sys_x, X_In)],
	opt_var(Env, sys_x, X_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  (   is_stringp(X_Get)
		  ->  get_var(GEnv, sys_x, X_Get13),
		      Prefix_Init=X_Get13
		  ;   Prefix_Init='$ARRAY'([*], claz_base_character, "G")
		  ),
		  get_var(GEnv, sys_x, X_Get17),
		  f_sys_fixnump(X_Get17, IFTEST15),
		  (   IFTEST15\==[]
		  ->  get_var(GEnv, sys_x, X_Get18),
		      Suffix_Init=X_Get18
		  ;   get_var(GEnv,
			      xx_gensym_counter_xx,
			      Xx_gensym_counter_xx_Get),
		      LEnv21=[bv(sys_x, Xx_gensym_counter_xx_Get)|GEnv],
		      get_var(LEnv21,
			      xx_gensym_counter_xx,
			      Xx_gensym_counter_xx_Get24),
		      'f_+'(1, Xx_gensym_counter_xx_Get24, LetResult20),
		      set_var(LEnv21, xx_gensym_counter_xx, LetResult20),
		      Suffix_Init=LetResult20
		  ),
		  LEnv=[bv(sys_prefix, Prefix_Init), bv(sys_suffix, Suffix_Init)|GEnv],
		  get_var(LEnv, sys_prefix, Prefix_Get),
		  get_var(LEnv, sys_suffix, Suffix_Get),
		  f_sys_integer_string(Suffix_Get, [], Integer_string_Ret),
		  f_sys_conc_string(Prefix_Get,
				    Integer_string_Ret,
				    Make_symbol_Param),
		  f_make_symbol(Make_symbol_Param, LetResult)
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_gen_sym, FnResult),
	      true).
:- set_opv(sys_gen_sym, symbol_function, f_sys_gen_sym),
   DefunResult=sys_gen_sym.
/*
:- side_effect(assert_lsp(sys_gen_sym,
			  lambda_def(defun,
				     sys_gen_sym,
				     f_sys_gen_sym,
				     [c38_optional, sys_x],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_prefix,
					     
					     [ if,
					       [stringp, sys_x],
					       sys_x,
					       '$ARRAY'([*],
							claz_base_character,
							"G")
					     ]
					   ],
					   
					   [ sys_suffix,
					     
					     [ if,
					       [sys_fixnump, sys_x],
					       sys_x,
					       
					       [ let,
						 
						 [ 
						   [ sys_x,
						     xx_gensym_counter_xx
						   ]
						 ],
						 
						 [ setf,
						   xx_gensym_counter_xx,
						   [+, 1, xx_gensym_counter_xx]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ make_symbol,
					   
					   [ sys_conc_string,
					     sys_prefix,
					     [sys_integer_string, sys_suffix]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_gen_sym,
			  arglist_info(sys_gen_sym,
				       f_sys_gen_sym,
				       [c38_optional, sys_x],
				       arginfo{ all:[sys_x],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x],
						opt:[sys_x],
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_gen_sym, init_args(0, f_sys_gen_sym))).
*/
/*
(let ((gentemp-counter 0))
  (defun gentemp (&optional (prefix "T") (package *package*))
    (setf gentemp-counter (+ 1 gentemp-counter))
    (intern (conc-string prefix (integer-string gentemp-counter))
	    package)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:2639 **********************/
:-lisp_compile_to_prolog(pkg_sys,[let,[['gentemp-counter',0]],[defun,gentemp,['&optional',[prefix,'$STRING'("T")],[package,'*package*']],[setf,'gentemp-counter',[+,1,'gentemp-counter']],[intern,['conc-string',prefix,['integer-string','gentemp-counter']],package]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_conc_string,
					       kw_function,
					       f_sys_conc_string)).
*/
:- LEnv=[bv(sys_gentemp_counter, 0)|CDR].
wl:lambda_def(defun, gentemp, f_gentemp, [c38_optional, [sys_prefix, '$ARRAY'([*], claz_base_character, "T")], [package, xx_package_xx]], [[setf, sys_gentemp_counter, [+, 1, sys_gentemp_counter]], [intern, [sys_conc_string, sys_prefix, [sys_integer_string, sys_gentemp_counter]], package]]).
wl:arglist_info(gentemp, f_gentemp, [c38_optional, [sys_prefix, '$ARRAY'([*], claz_base_character, "T")], [package, xx_package_xx]], arginfo{all:[sys_prefix, package], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_prefix, package], opt:[sys_prefix, package], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(0, f_gentemp).

/*

### Compiled Function: `CL:GENTEMP` 
*/
f_gentemp(RestNKeys, FnResult) :-
	GEnv=[bv(sys_prefix, Prefix_In), bv(package, Package_In)],
	opt_var(LEnv,
		sys_prefix,
		Prefix_In,
		true,
		'$ARRAY'([*], claz_base_character, "T"),
		1,
		RestNKeys),
	opt_var(LEnv,
		package,
		Package_In,
		get_var(Get_var_Param, xx_package_xx, Xx_package_xx_Get),
		Xx_package_xx_Get,
		2,
		RestNKeys),
	catch(( ( get_var(GEnv, sys_gentemp_counter, Gentemp_counter_Get),
		  'f_+'(1, Gentemp_counter_Get, Gentemp_counter),
		  set_var(GEnv, sys_gentemp_counter, Gentemp_counter),
		  get_var(GEnv, sys_gentemp_counter, Gentemp_counter_Get13),
		  get_var(GEnv, sys_prefix, Prefix_Get),
		  f_sys_integer_string(Gentemp_counter_Get13,
				       [],
				       Integer_string_Ret),
		  f_sys_conc_string(Prefix_Get,
				    Integer_string_Ret,
				    Intern_Param),
		  get_var(GEnv, package, Package_Get),
		  f_intern(Intern_Param, Package_Get, Intern_Ret)
		),
		Intern_Ret=FnResult
	      ),
	      block_exit(gentemp, FnResult),
	      true).
:- set_opv(gentemp, symbol_function, f_gentemp),
   DefunResult=gentemp.
/*
:- side_effect(assert_lsp(gentemp,
			  lambda_def(defun,
				     gentemp,
				     f_gentemp,
				     
				     [ c38_optional,
				       
				       [ sys_prefix,
					 '$ARRAY'([*], claz_base_character, "T")
				       ],
				       [package, xx_package_xx]
				     ],
				     
				     [ 
				       [ setf,
					 sys_gentemp_counter,
					 [+, 1, sys_gentemp_counter]
				       ],
				       
				       [ intern,
					 
					 [ sys_conc_string,
					   sys_prefix,
					   
					   [ sys_integer_string,
					     sys_gentemp_counter
					   ]
					 ],
					 package
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(gentemp,
			  arglist_info(gentemp,
				       f_gentemp,
				       
				       [ c38_optional,
					 
					 [ sys_prefix,
					   '$ARRAY'([*],
						    claz_base_character,
						    "T")
					 ],
					 [package, xx_package_xx]
				       ],
				       arginfo{ all:[sys_prefix, package],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_prefix, package],
						opt:[sys_prefix, package],
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(gentemp, init_args(0, f_gentemp))).
*/
/*
#+BUILTIN 
#+(or WAM-CL LISP500)
(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:2868 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,get,[symbol,indicator,'&optional',default],[getf,['symbol-plist',symbol],indicator,default]]]]))
/*
#+(or WAM-CL LISP500)
(defun (setf get) (new-value symbol indicator &optional default)
  (setf (getf (symbol-plist symbol) indicator default) new-value))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:3005 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,[setf,get],['new-value',symbol,indicator,'&optional',default],[setf,[getf,['symbol-plist',symbol],indicator,default],'new-value']])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       setf_get,
					       kw_function,
					       f_setf_get)).
*/
/*
:- failure(show_call_trace((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _7606, [sys_indicator, sys_default], CDR, Compile_each_Ret), append([[symbol_plist, symbol]|CDR], [CAR13, CAR], Append_Ret), setf_inverse_op(getf, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _7606, [sys_indicator, sys_default], CDR, Compile_each_Ret), append([[symbol_plist, symbol]|CDR], [CAR13, CAR], Append_Ret), setf_inverse_op(getf, Inverse_op_Ret)))).
*/
wl: declared(get, defun_setf(setf_get)).

wl:lambda_def(defun, setf_get, f_setf_get, [sys_new_value, symbol, sys_indicator, c38_optional, sys_default], [[setf, [getf, [symbol_plist, symbol], sys_indicator, sys_default], sys_new_value]]).
wl:arglist_info(setf_get, f_setf_get, [sys_new_value, symbol, sys_indicator, c38_optional, sys_default], arginfo{all:[sys_new_value, symbol, sys_indicator, sys_default], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_new_value, symbol, sys_indicator, sys_default], opt:[sys_default], req:[sys_new_value, symbol, sys_indicator], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_setf_get).

/*

### Compiled Function: `CL::SETF-GET` 
*/
f_setf_get(New_value_In, Symbol_In, Indicator_In, RestNKeys, FnResult) :-
	Setf_Env=[bv(sys_new_value, New_value_In), bv(symbol, Symbol_In), bv(sys_indicator, Indicator_In), bv(sys_default, Default_In)],
	opt_var(Env, sys_default, Default_In, true, [], 1, RestNKeys),
	catch(( ( get_var(Setf_Env, symbol, Symbol_Get),
		  get_var(Setf_Env, sys_new_value, New_value_Get),
		  f_symbol_plist(Symbol_Get, Symbol_plist_Ret),
		  get_var(Setf_Env, sys_default, Default_Get),
		  get_var(Setf_Env, sys_indicator, Indicator_Get),
		  set_place(Setf_Env,
			    setf,
			    [getf, Symbol_plist_Ret, Indicator_Get, Default_Get],
			    [New_value_Get],
			    Setf_R)
		),
		Setf_R=FnResult
	      ),
	      block_exit(setf_get, FnResult),
	      true).
:- set_opv(setf_get, symbol_function, f_setf_get),
   DefunResult=setf_get.
/*
:- side_effect(assert_lsp(setf_get,
			  lambda_def(defun,
				     setf_get,
				     f_setf_get,
				     
				     [ sys_new_value,
				       symbol,
				       sys_indicator,
				       c38_optional,
				       sys_default
				     ],
				     
				     [ 
				       [ setf,
					 
					 [ getf,
					   [symbol_plist, symbol],
					   sys_indicator,
					   sys_default
					 ],
					 sys_new_value
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(setf_get,
			  arglist_info(setf_get,
				       f_setf_get,
				       
				       [ sys_new_value,
					 symbol,
					 sys_indicator,
					 c38_optional,
					 sys_default
				       ],
				       arginfo{ all:
						    [ sys_new_value,
						      symbol,
						      sys_indicator,
						      sys_default
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_new_value,
							symbol,
							sys_indicator,
							sys_default
						      ],
						opt:[sys_default],
						req:
						    [ sys_new_value,
						      symbol,
						      sys_indicator
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(setf_get, init_args(3, f_setf_get))).
*/
/*
#+(or WAM-CL LISP500)
(defun (setf rest) (new-tail list) (setf (cdr list) new-tail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./remf.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remf.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

;;; Adapted from SBCL.

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:3164 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,[setf,rest],['new-tail',list],[setf,[cdr,list],'new-tail']])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       setf_rest,
					       kw_function,
					       f_setf_rest)).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _7796, [], [], true), append([list], [CAR7, CAR], [list, CAR7, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _7730, [], [], true), append([list], [CAR7, CAR], [list, CAR7, CAR]), setf_inverse_op(cdr, rplacd))).
*/
wl: declared(rest, defun_setf(setf_rest)).

wl:lambda_def(defun, setf_rest, f_setf_rest, [sys_new_tail, list], [[setf, [cdr, list], sys_new_tail]]).
wl:arglist_info(setf_rest, f_setf_rest, [sys_new_tail, list], arginfo{all:[sys_new_tail, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_new_tail, list], opt:0, req:[sys_new_tail, list], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_setf_rest).

/*

### Compiled Function: `CL::SETF-REST` 
*/
f_setf_rest(New_tail_In, List_In, FnResult) :-
	GEnv=[bv(sys_new_tail, New_tail_In), bv(list, List_In)],
	catch(( ( get_var(GEnv, list, List_Get),
		  get_var(GEnv, sys_new_tail, New_tail_Get),
		  f_rplacd(List_Get, New_tail_Get, Rplacd_Ret)
		),
		Rplacd_Ret=FnResult
	      ),
	      block_exit(setf_rest, FnResult),
	      true).
:- set_opv(setf_rest, symbol_function, f_setf_rest),
   DefunResult=setf_rest.
/*
:- side_effect(assert_lsp(setf_rest,
			  lambda_def(defun,
				     setf_rest,
				     f_setf_rest,
				     [sys_new_tail, list],
				     [[setf, [cdr, list], sys_new_tail]]))).
*/
/*
:- side_effect(assert_lsp(setf_rest,
			  arglist_info(setf_rest,
				       f_setf_rest,
				       [sys_new_tail, list],
				       arginfo{ all:[sys_new_tail, list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_new_tail, list],
						opt:0,
						req:[sys_new_tail, list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(setf_rest, init_args(x, f_setf_rest))).
*/
/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/
/*
;;; BEGIN FILE ./remf.lisp
*/
/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/
/*
;; remf.lisp
*/
/*
;;
*/
/*
;; Copyright (C) 2003-2005 Peter Graves
*/
/*
;; $Id$
*/
/*
;;
*/
/*
;; This program is free software; you can redistribute it and/or
*/
/*
;; modify it under the terms of the GNU General Public License
*/
/*
;; as published by the Free Software Foundation; either version 2
*/
/*
;; of the License, or (at your option) any later version.
*/
/*
;;
*/
/*
;; This program is distributed in the hope that it will be useful,
*/
/*
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
*/
/*
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*/
/*
;; GNU General Public License for more details.
*/
/*
;;
*/
/*
;; You should have received a copy of the GNU General Public License
*/
/*
;; along with this program; if not, write to the Free Software
*/
/*
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/
/*
;;
*/
/*
;; As a special exception, the copyright holders of this library give you
*/
/*
;; permission to link this library with independent modules to produce an
*/
/*
;; executable, regardless of the license terms of these independent
*/
/*
;; modules, and to copy and distribute the resulting executable under
*/
/*
;; terms of your choice, provided that you also meet, for each linked
*/
/*
;; independent module, the terms and conditions of the license of that
*/
/*
;; module.  An independent module is a module which is not derived from
*/
/*
;; or based on this library.  If you modify this library, you may extend
*/
/*
;; this exception to your version of the library, but you are not
*/
/*
;; obligated to do so.  If you do not wish to do so, delete this
*/
/*
;; exception statement from your version.
*/
/*
;; Adapted from SBCL.
*/
/*
(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
   to hold a property list or (). This list is destructively altered to
   remove the property specified by the indicator. Returns T if such a
   property was present, NIL if not."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
          ;; See ANSI 5.1.3 for why we do out-of-order evaluation
	  (push (list ind-temp indicator) let-list)
	  (push (list (car newval) getter) let-list)
	  `(let* ,(nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error "Odd-length property list in REMF."))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./remf.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:5000 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,remf,[place,indicator,'&environment',env],'$STRING'("Place may be any place expression acceptable to SETF, and is expected\r\n   to hold a property list or (). This list is destructively altered to\r\n   remove the property specified by the indicator. Returns T if such a\r\n   property was present, NIL if not."),['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],['do*',[[d,dummies,[cdr,d]],[v,vals,[cdr,v]],['let-list',[]],['ind-temp',[gensym]],[local1,[gensym]],[local2,[gensym]]],[[null,d],[push,[list,'ind-temp',indicator],'let-list'],[push,[list,[car,newval],getter],'let-list'],['#BQ',['let*',['#COMMA',[nreverse,'let-list']],[do,[[['#COMMA',local1],['#COMMA',[car,newval]],[cddr,['#COMMA',local1]]],[['#COMMA',local2],[],['#COMMA',local1]]],[[atom,['#COMMA',local1]],[]],[cond,[[atom,[cdr,['#COMMA',local1]]],[error,'$STRING'("Odd-length property list in REMF.")]],[[eq,[car,['#COMMA',local1]],['#COMMA','ind-temp']],[cond,[['#COMMA',local2],[rplacd,[cdr,['#COMMA',local2]],[cddr,['#COMMA',local1]]],[return,t]],[t,[setq,['#COMMA',[car,newval]],[cddr,['#COMMA',[car,newval]]]],['#COMMA',setter],[return,t]]]]]]]]],[push,[list,[car,d],[car,v]],'let-list']]]])
/*
% macroexpand:-[push,[list,sys_ind_temp,sys_indicator],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,sys_ind_temp,sys_indicator],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,[car,sys_newval],sys_getter],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_newval],sys_getter],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,[car,sys_d],[car,sys_v]],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_d],[car,sys_v]],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,sys_ind_temp,sys_indicator],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,sys_ind_temp,sys_indicator],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,[car,sys_newval],sys_getter],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_newval],sys_getter],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,[car,sys_d],[car,sys_v]],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_d],[car,sys_v]],sys_let_list]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       remf,
					       kw_special,
					       sf_remf)).
*/
doc: doc_string(remf,
	      _9480,
	      function,
	      "Place may be any place expression acceptable to SETF, and is expected\r\n   to hold a property list or (). This list is destructively altered to\r\n   remove the property specified by the indicator. Returns T if such a\r\n   property was present, NIL if not.").

wl:lambda_def(defmacro, remf, mf_remf, [sys_place, sys_indicator, c38_environment, sys_env], [[multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_env], [do_xx, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]], [sys_let_list, []], [sys_ind_temp, [gensym]], [sys_local1, [gensym]], [sys_local2, [gensym]]], [[null, sys_d], [push, [list, sys_ind_temp, sys_indicator], sys_let_list], [push, [list, [car, sys_newval], sys_getter], sys_let_list], ['#BQ', [let_xx, ['#COMMA', [nreverse, sys_let_list]], [do, [[['#COMMA', sys_local1], ['#COMMA', [car, sys_newval]], [cddr, ['#COMMA', sys_local1]]], [['#COMMA', sys_local2], [], ['#COMMA', sys_local1]]], [[atom, ['#COMMA', sys_local1]], []], [cond, [[atom, [cdr, ['#COMMA', sys_local1]]], [error, '$ARRAY'([*], claz_base_character, "Odd-length property list in REMF.")]], [[eq, [car, ['#COMMA', sys_local1]], ['#COMMA', sys_ind_temp]], [cond, [['#COMMA', sys_local2], [rplacd, [cdr, ['#COMMA', sys_local2]], [cddr, ['#COMMA', sys_local1]]], [return, t]], [t, [setq, ['#COMMA', [car, sys_newval]], [cddr, ['#COMMA', [car, sys_newval]]]], ['#COMMA', sys_setter], [return, t]]]]]]]]], [push, [list, [car, sys_d], [car, sys_v]], sys_let_list]]]]).
wl:arglist_info(remf, mf_remf, [sys_place, sys_indicator, c38_environment, sys_env], arginfo{all:[sys_place, sys_indicator], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[sys_env], key:0, names:[sys_place, sys_indicator, sys_env], opt:0, req:[sys_place, sys_indicator], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_remf).

/*

### Compiled Macro Operator: `CL:REMF` 
*/
sf_remf(Env_In, Place_In, Indicator_In, RestNKeys, FResult) :-
	mf_remf([remf, Place_In, Indicator_In|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:REMF` 
*/
mf_remf([remf, Place_In, Indicator_In|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	CDR=[bv(sys_place, Place_In), bv(sys_indicator, Indicator_In), bv(sys_env, Env_In)],
	catch(( ( LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|CDR],
		  get_var(LEnv, sys_env, Env_Get),
		  get_var(LEnv, sys_place, Place_Get),
		  f_get_setf_expansion(Place_Get, [Env_Get], Setf_expansion_Ret),
		  setq_from_values(LEnv,
				   
				   [ sys_dummies,
				     sys_vals,
				     sys_newval,
				     sys_setter,
				     sys_getter
				   ]),
		  get_var(LEnv, sys_dummies, Dummies_Get),
		  LEnv14=[bv(sys_d, Dummies_Get)|LEnv],
		  get_var(LEnv14, sys_vals, Vals_Get),
		  LEnv19=[bv(sys_v, Vals_Get)|LEnv14],
		  LEnv24=[bv(sys_let_list, [])|LEnv19],
		  f_gensym(Ind_temp_Init),
		  LEnv27=[bv(sys_ind_temp, Ind_temp_Init)|LEnv24],
		  f_gensym(Local1_Init),
		  LEnv31=[bv(sys_local1, Local1_Init)|LEnv27],
		  f_gensym(Local2_Init),
		  BlockExitEnv=[bv(sys_local2, Local2_Init)|LEnv31],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_34), get_var(BlockExitEnv, sys_d, IFTEST77), (IFTEST77==[]->get_var(BlockExitEnv, sys_ind_temp, Ind_temp_Get83), get_var(BlockExitEnv, sys_indicator, Indicator_Get84), CAR=[Ind_temp_Get83, Indicator_Get84], get_var(BlockExitEnv, sys_let_list, Let_list_Get85), Let_list=[CAR|Let_list_Get85], set_var(BlockExitEnv, sys_let_list, Let_list), get_var(BlockExitEnv, sys_newval, Newval_Get86), f_car(Newval_Get86, Car_Ret), get_var(BlockExitEnv, sys_getter, Getter_Get87), CAR128=[Car_Ret, Getter_Get87], get_var(BlockExitEnv, sys_let_list, Let_list_Get88), Let_list118=[CAR128|Let_list_Get88], set_var(BlockExitEnv, sys_let_list, Let_list118), get_var(BlockExitEnv, sys_let_list, Let_list_Get89), f_nreverse(Let_list_Get89, Nreverse_Ret), get_var(BlockExitEnv, sys_local1, Local1_Get90), get_var(BlockExitEnv, sys_newval, Newval_Get91), f_car(Newval_Get91, Car_Ret130), (get_var(BlockExitEnv, sys_ind_temp, Ind_temp_Get98), get_var(BlockExitEnv, sys_local1, Local1_Get92)), (get_var(BlockExitEnv, sys_local1, Local1_Get95), get_var(BlockExitEnv, sys_local2, Local2_Get93)), get_var(BlockExitEnv, sys_local2, Local2_Get99), get_var(BlockExitEnv, sys_newval, Newval_Get102), f_car(Newval_Get102, Car_Ret131), get_var(BlockExitEnv, sys_newval, Newval_Get103), f_car(Newval_Get103, Car_Ret132), get_var(BlockExitEnv, sys_setter, Setter_Get104), throw(block_exit([], [let_xx, Nreverse_Ret, [do, [[Local1_Get90, Car_Ret130, [cddr, Local1_Get92]], [Local2_Get93, [], Local1_Get92]], [[atom, Local1_Get95], []], [cond, [[atom, [cdr, Local1_Get95]], [error, '$ARRAY'([*], claz_base_character, "Odd-length property list in REMF.")]], [[eq, [car, Local1_Get95], Ind_temp_Get98], [cond, [Local2_Get99, [rplacd, [cdr, Local2_Get99], [cddr, Local1_Get95]], [return, t]], [t, [setq, Car_Ret131, [cddr, Car_Ret132]], Setter_Get104, [return, t]]]]]]])), _TBResult=ThrowResult81;get_var(BlockExitEnv, sys_d, D_Get106), f_car(D_Get106, Car_Ret133), get_var(BlockExitEnv, sys_v, V_Get107), f_car(V_Get107, Car_Ret134), CAR135=[Car_Ret133, Car_Ret134], get_var(BlockExitEnv, sys_let_list, Let_list_Get108), Let_list119=[CAR135|Let_list_Get108], set_var(BlockExitEnv, sys_let_list, Let_list119), get_var(BlockExitEnv, sys_d, D_Get109), f_cdr(D_Get109, D), get_var(BlockExitEnv, sys_v, V_Get110), f_cdr(V_Get110, V), set_var(BlockExitEnv, sys_d, D), set_var(BlockExitEnv, sys_v, V), goto(do_label_34, BlockExitEnv), _TBResult=_GORES111)),
					  
					  [ addr(addr_tagbody_36_do_label_34,
						 do_label_34,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, sys_d, IFTEST), (IFTEST==[]->get_var(BlockExitEnv, sys_ind_temp, Get_var_Ret), get_var(BlockExitEnv, sys_indicator, Get_var_Ret137), CAR139=[Get_var_Ret, Get_var_Ret137], get_var(BlockExitEnv, sys_let_list, Get_var_Ret138), Set_var_Ret=[CAR139|Get_var_Ret138], set_var(BlockExitEnv, sys_let_list, Set_var_Ret), get_var(BlockExitEnv, sys_newval, Car_Param), f_car(Car_Param, Car_Ret141), get_var(BlockExitEnv, sys_getter, Get_var_Ret142), CAR143=[Car_Ret141, Get_var_Ret142], get_var(BlockExitEnv, sys_let_list, Let_list_Get49), Set_var_Ret144=[CAR143|Let_list_Get49], set_var(BlockExitEnv, sys_let_list, Set_var_Ret144), get_var(BlockExitEnv, sys_let_list, Let_list_Get50), f_nreverse(Let_list_Get50, Nreverse_Ret145), get_var(BlockExitEnv, sys_local1, Get_var_Ret146), get_var(BlockExitEnv, sys_newval, Newval_Get52), f_car(Newval_Get52, Car_Ret147), (get_var(BlockExitEnv, sys_ind_temp, Ind_temp_Get59), get_var(BlockExitEnv, sys_local1, Local1_Get53)), (get_var(BlockExitEnv, sys_local1, Local1_Get56), get_var(BlockExitEnv, sys_local2, Get_var_Ret148)), get_var(BlockExitEnv, sys_local2, Local2_Get60), get_var(BlockExitEnv, sys_newval, Newval_Get63), f_car(Newval_Get63, Car_Ret149), get_var(BlockExitEnv, sys_newval, Newval_Get64), f_car(Newval_Get64, Car_Ret150), get_var(BlockExitEnv, sys_setter, Get_var_Ret151), throw(block_exit([], [let_xx, Nreverse_Ret145, [do, [[Get_var_Ret146, Car_Ret147, [cddr, Local1_Get53]], [Get_var_Ret148, [], Local1_Get53]], [[atom, Local1_Get56], []], [cond, [[atom, [cdr, Local1_Get56]], [error, '$ARRAY'([*], claz_base_character, "Odd-length property list in REMF.")]], [[eq, [car, Local1_Get56], Ind_temp_Get59], [cond, [Local2_Get60, [rplacd, [cdr, Local2_Get60], [cddr, Local1_Get56]], [return, t]], [t, [setq, Car_Ret149, [cddr, Car_Ret150]], Get_var_Ret151, [return, t]]]]]]])), _13122=ThrowResult;get_var(BlockExitEnv, sys_d, D_Get67), f_car(D_Get67, Car_Ret152), get_var(BlockExitEnv, sys_v, Car_Param123), f_car(Car_Param123, Car_Ret153), CAR154=[Car_Ret152, Car_Ret153], get_var(BlockExitEnv, sys_let_list, Let_list_Get69), Set_var_Ret155=[CAR154|Let_list_Get69], set_var(BlockExitEnv, sys_let_list, Set_var_Ret155), get_var(BlockExitEnv, sys_d, D_Get70), f_cdr(D_Get70, Cdr_Ret), get_var(BlockExitEnv, sys_v, V_Get71), f_cdr(V_Get71, Cdr_Ret157), set_var(BlockExitEnv, sys_d, Cdr_Ret), set_var(BlockExitEnv, sys_v, Cdr_Ret157), goto(do_label_34, BlockExitEnv), _13122=_GORES)))
					  ]),
			  []=LetResult13
			),
			block_exit([], LetResult13),
			true)
		),
		LetResult13=MFResult
	      ),
	      block_exit(remf, MFResult),
	      true).
:- set_opv(mf_remf, type_of, sys_macro),
   set_opv(remf, symbol_function, mf_remf),
   DefMacroResult=remf.
/*
:- side_effect(assert_lsp(remf,
			  doc_string(remf,
				     _9480,
				     function,
				     "Place may be any place expression acceptable to SETF, and is expected\r\n   to hold a property list or (). This list is destructively altered to\r\n   remove the property specified by the indicator. Returns T if such a\r\n   property was present, NIL if not."))).
*/
/*
:- side_effect(assert_lsp(remf,
			  lambda_def(defmacro,
				     remf,
				     mf_remf,
				     
				     [ sys_place,
				       sys_indicator,
				       c38_environment,
				       sys_env
				     ],
				     
				     [ 
				       [ multiple_value_bind,
					 
					 [ sys_dummies,
					   sys_vals,
					   sys_newval,
					   sys_setter,
					   sys_getter
					 ],
					 
					 [ get_setf_expansion,
					   sys_place,
					   sys_env
					 ],
					 
					 [ do_xx,
					   
					   [ [sys_d, sys_dummies, [cdr, sys_d]],
					     [sys_v, sys_vals, [cdr, sys_v]],
					     [sys_let_list, []],
					     [sys_ind_temp, [gensym]],
					     [sys_local1, [gensym]],
					     [sys_local2, [gensym]]
					   ],
					   
					   [ [null, sys_d],
					     
					     [ push,
					       
					       [ list,
						 sys_ind_temp,
						 sys_indicator
					       ],
					       sys_let_list
					     ],
					     
					     [ push,
					       
					       [ list,
						 [car, sys_newval],
						 sys_getter
					       ],
					       sys_let_list
					     ],
					     
					     [ '#BQ',
					       
					       [ let_xx,
						 
						 [ '#COMMA',
						   [nreverse, sys_let_list]
						 ],
						 
						 [ do,
						   
						   [ 
						     [ ['#COMMA', sys_local1],
						       
						       [ '#COMMA',
							 [car, sys_newval]
						       ],
						       
						       [ cddr,
							 ['#COMMA', sys_local1]
						       ]
						     ],
						     
						     [ ['#COMMA', sys_local2],
						       [],
						       ['#COMMA', sys_local1]
						     ]
						   ],
						   
						   [ 
						     [ atom,
						       ['#COMMA', sys_local1]
						     ],
						     []
						   ],
						   
						   [ cond,
						     
						     [ 
						       [ atom,
							 
							 [ cdr,
							   
							   [ '#COMMA',
							     sys_local1
							   ]
							 ]
						       ],
						       
						       [ error,
							 '$ARRAY'([*],
								  claz_base_character,
								  "Odd-length property list in REMF.")
						       ]
						     ],
						     
						     [ 
						       [ eq,
							 
							 [ car,
							   
							   [ '#COMMA',
							     sys_local1
							   ]
							 ],
							 
							 [ '#COMMA',
							   sys_ind_temp
							 ]
						       ],
						       
						       [ cond,
							 
							 [ 
							   [ '#COMMA',
							     sys_local2
							   ],
							   
							   [ rplacd,
							     
							     [ cdr,
							       
							       [ '#COMMA',
								 sys_local2
							       ]
							     ],
							     
							     [ cddr,
							       
							       [ '#COMMA',
								 sys_local1
							       ]
							     ]
							   ],
							   [return, t]
							 ],
							 
							 [ t,
							   
							   [ setq,
							     
							     [ '#COMMA',
							       [car, sys_newval]
							     ],
							     
							     [ cddr,
							       
							       [ '#COMMA',
								 [car, sys_newval]
							       ]
							     ]
							   ],
							   
							   [ '#COMMA',
							     sys_setter
							   ],
							   [return, t]
							 ]
						       ]
						     ]
						   ]
						 ]
					       ]
					     ]
					   ],
					   
					   [ push,
					     [list, [car, sys_d], [car, sys_v]],
					     sys_let_list
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(remf,
			  arglist_info(remf,
				       mf_remf,
				       
				       [ sys_place,
					 sys_indicator,
					 c38_environment,
					 sys_env
				       ],
				       arginfo{ all:[sys_place, sys_indicator],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment],
						env:[sys_env],
						key:0,
						names:
						      [ sys_place,
							sys_indicator,
							sys_env
						      ],
						opt:0,
						req:[sys_place, sys_indicator],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(remf, init_args(2, mf_remf))).
*/
/*
; See ANSI 5.1.3 for why we do out-of-order evaluation
*/
/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/
/*
;;; END FILE ./remf.lisp
*/
/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/
/*
#+(or WAM-CL LISP500)
(defmacro remprop (symbol indicator) `(remf (symbol-plist ,symbol) ,indicator))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:6358 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,remprop,[symbol,indicator],['#BQ',[remf,['symbol-plist',['#COMMA',symbol]],['#COMMA',indicator]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       remprop,
					       kw_macro,
					       mf_remprop)).
*/
wl:lambda_def(defmacro, remprop, mf_remprop, [symbol, sys_indicator], [['#BQ', [remf, [symbol_plist, ['#COMMA', symbol]], ['#COMMA', sys_indicator]]]]).
wl:arglist_info(remprop, mf_remprop, [symbol, sys_indicator], arginfo{all:[symbol, sys_indicator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol, sys_indicator], opt:0, req:[symbol, sys_indicator], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_remprop).

/*

### Compiled Function: `CL:REMPROP` 
*/
f_remprop(MacroEnv, Symbol_In, Indicator_In, RestNKeys, FResult) :-
	mf_remprop([remprop, Symbol_In, Indicator_In|RestNKeys],
		   MacroEnv,
		   MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:REMPROP` 
*/
mf_remprop([remprop, Symbol_In, Indicator_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(symbol, Symbol_In), bv(sys_indicator, Indicator_In)],
	catch(( ( get_var(GEnv, symbol, Symbol_Get),
		  get_var(GEnv, sys_indicator, Indicator_Get)
		),
		[remf, [symbol_plist, Symbol_Get], Indicator_Get]=MFResult
	      ),
	      block_exit(remprop, MFResult),
	      true).
:- set_opv(mf_remprop, type_of, sys_macro),
   set_opv(remprop, symbol_function, mf_remprop),
   DefMacroResult=remprop.
/*
:- side_effect(assert_lsp(remprop,
			  lambda_def(defmacro,
				     remprop,
				     mf_remprop,
				     [symbol, sys_indicator],
				     
				     [ 
				       [ '#BQ',
					 
					 [ remf,
					   [symbol_plist, ['#COMMA', symbol]],
					   ['#COMMA', sys_indicator]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(remprop,
			  arglist_info(remprop,
				       mf_remprop,
				       [symbol, sys_indicator],
				       arginfo{ all:[symbol, sys_indicator],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[symbol, sys_indicator],
						opt:0,
						req:[symbol, sys_indicator],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(remprop, init_args(2, mf_remprop))).
*/
/*
#+BUILTIN 
#+(or WAM-CL LISP500)
(defun makunbound (symbol) (imakunbound symbol 4))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:6464 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,makunbound,[symbol],[imakunbound,symbol,4]]]]))
/*
#+BUILTIN 
#+(or WAM-CL LISP500)
(defun set (symbol value) (setf (symbol-value symbol) value))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:6553 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,set,[symbol,value],[setf,['symbol-value',symbol],value]]]]))
/*
#+(or WAM-CL LISP500)
(defun designator-string (designator)
  (if (stringp designator)
      designator
      (if (characterp designator)
	  (string designator)
	  (symbol-name designator))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:6653 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-string',[designator],[if,[stringp,designator],designator,[if,[characterp,designator],[string,designator],['symbol-name',designator]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_designator_string,
					       kw_function,
					       f_sys_designator_string)).
*/
wl:lambda_def(defun, sys_designator_string, f_sys_designator_string, [sys_designator], [[if, [stringp, sys_designator], sys_designator, [if, [characterp, sys_designator], [string, sys_designator], [symbol_name, sys_designator]]]]).
wl:arglist_info(sys_designator_string, f_sys_designator_string, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_designator_string).

/*

### Compiled Function: `SYS::DESIGNATOR-STRING` 
*/
f_sys_designator_string(Designator_In, FnResult) :-
	GEnv=[bv(sys_designator, Designator_In)],
	catch(( ( get_var(GEnv, sys_designator, Designator_Get),
		  (   is_stringp(Designator_Get)
		  ->  get_var(GEnv, sys_designator, Designator_Get9),
		      _9928=Designator_Get9
		  ;   get_var(GEnv, sys_designator, Designator_Get11),
		      (   string:is_characterp(Designator_Get11)
		      ->  get_var(GEnv, sys_designator, Designator_Get14),
			  f_string(Designator_Get14, TrueResult),
			  ElseResult19=TrueResult
		      ;   get_var(GEnv, sys_designator, Designator_Get15),
			  f_symbol_name(Designator_Get15, ElseResult),
			  ElseResult19=ElseResult
		      ),
		      _9928=ElseResult19
		  )
		),
		_9928=FnResult
	      ),
	      block_exit(sys_designator_string, FnResult),
	      true).
:- set_opv(sys_designator_string, symbol_function, f_sys_designator_string),
   DefunResult=sys_designator_string.
/*
:- side_effect(assert_lsp(sys_designator_string,
			  lambda_def(defun,
				     sys_designator_string,
				     f_sys_designator_string,
				     [sys_designator],
				     
				     [ 
				       [ if,
					 [stringp, sys_designator],
					 sys_designator,
					 
					 [ if,
					   [characterp, sys_designator],
					   [string, sys_designator],
					   [symbol_name, sys_designator]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_string,
			  arglist_info(sys_designator_string,
				       f_sys_designator_string,
				       [sys_designator],
				       arginfo{ all:[sys_designator],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_designator],
						opt:0,
						req:[sys_designator],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_designator_string,
			  init_args(x, f_sys_designator_string))).
*/
/*
#+BUILTIN 
#+(or WAM-CL LISP500)
(defvar *package* (car (cdr *packages*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:6854 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defvar,'*package*',[car,[cdr,'*packages*']]]]]))
/*
#+(or WAM-CL LISP500)
(defun list-all-packages ()
  (copy-list *packages*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:6934 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'list-all-packages',[],['copy-list','*packages*']])
wl:lambda_def(defun, list_all_packages, f_list_all_packages, [], [[copy_list, sys_xx_packages_xx]]).
wl:arglist_info(list_all_packages, f_list_all_packages, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_list_all_packages).

/*

### Compiled Function: `CL:LIST-ALL-PACKAGES` 
*/
f_list_all_packages(FnResult) :-
	GEnv=[],
	catch(( ( get_var(GEnv, sys_xx_packages_xx, Xx_packages_xx_Get),
		  f_copy_list(Xx_packages_xx_Get, Copy_list_Ret)
		),
		Copy_list_Ret=FnResult
	      ),
	      block_exit(list_all_packages, FnResult),
	      true).
:- set_opv(list_all_packages, symbol_function, f_list_all_packages),
   DefunResult=list_all_packages.
/*
:- side_effect(assert_lsp(list_all_packages,
			  lambda_def(defun,
				     list_all_packages,
				     f_list_all_packages,
				     [],
				     [[copy_list, sys_xx_packages_xx]]))).
*/
/*
:- side_effect(assert_lsp(list_all_packages,
			  arglist_info(list_all_packages,
				       f_list_all_packages,
				       [],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[],
						opt:0,
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(list_all_packages, init_args(x, f_list_all_packages))).
*/
/*
#+(or WAM-CL LISP500)
(defun /= (number &rest numbers)
  (tagbody
   start
     (when numbers
       (dolist (n numbers)
	 (when (= number n)
	   (return-from /=)))
       (setq number (pop numbers))
       (go start)))
  t)



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:7015 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,/=,[number,'&rest',numbers],[tagbody,start,[when,numbers,[dolist,[n,numbers],[when,[=,number,n],['return-from',/=]]],[setq,number,[pop,numbers]],[go,start]]],t])
/*
% macroexpand:-[pop,sys_numbers].
*/
/*
% into:-[prog1,[car,sys_numbers],[setq,sys_numbers,[cdr,sys_numbers]]].
*/
/*
% macroexpand:-[pop,sys_numbers].
*/
/*
% into:-[prog1,[car,sys_numbers],[setq,sys_numbers,[cdr,sys_numbers]]].
*/
wl:lambda_def(defun, /=, f_c47_c61, [number, c38_rest, sys_numbers], [[tagbody, sys_start, [when, sys_numbers, [dolist, [sys_n, sys_numbers], [when, [=, number, sys_n], [return_from, /=]]], [setq, number, [pop, sys_numbers]], [go, sys_start]]], t]).
wl:arglist_info(/=, f_c47_c61, [number, c38_rest, sys_numbers], arginfo{all:[number], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[number, sys_numbers], opt:0, req:[number], rest:[sys_numbers], sublists:0, whole:0}).
wl: init_args(1, f_c47_c61).

/*

### Compiled Function: `CL:/=` 
*/
f_c47_c61(Number_In, RestNKeys, FnResult) :-
	AEnv=[bv(number, Number_In), bv(sys_numbers, RestNKeys)],
	catch(( call_addr_block(AEnv,
				(push_label(sys_start), get_var(AEnv, sys_numbers, IFTEST31), (IFTEST31\==[]->get_var(AEnv, sys_numbers, Numbers_Get34), BV44=bv(sys_n, Ele46), BlockExitEnv42=[BV44|AEnv], forall(member(Ele46, Numbers_Get34),  (nb_setarg(2, BV44, Ele46), get_var(BlockExitEnv42, number, Number_Get36), get_var(BlockExitEnv42, sys_n, N_Get37), (Number_Get36=:=N_Get37->set_var(BlockExitEnv42, 'block_ret_/=', []), always('block_exit_/=', BlockExitEnv42);_7954=[]))), get_var(AEnv, sys_numbers, Numbers_Get49), f_car(Numbers_Get49, Number), get_var(AEnv, sys_numbers, Numbers_Get51), f_cdr(Numbers_Get51, Numbers), set_var(AEnv, sys_numbers, Numbers), set_var(AEnv, number, Number), goto(sys_start, AEnv), _TBResult=_GORES52;_TBResult=[])),
				
				[ addr(addr_tagbody_37_sys_start,
				       sys_start,
				       '$unused',
				       AEnv,
				       (get_var(AEnv, sys_numbers, IFTEST), (IFTEST\==[]->get_var(AEnv, sys_numbers, Numbers_Get10), CAR=bv(sys_n, Member_Param), BlockExitEnv=[CAR|AEnv], forall(member(Member_Param, Numbers_Get10),  (nb_setarg(2, CAR, Member_Param), get_var(BlockExitEnv, number, Number_Get), get_var(BlockExitEnv, sys_n, N_Get), (Number_Get=:=N_Get->set_var(BlockExitEnv, 'block_ret_/=', []), always('block_exit_/=', BlockExitEnv);_8518=[]))), get_var(AEnv, sys_numbers, Numbers_Get25), f_car(Numbers_Get25, Car_Ret), get_var(AEnv, sys_numbers, Numbers_Get26), f_cdr(Numbers_Get26, Cdr_Ret), set_var(AEnv, sys_numbers, Cdr_Ret), set_var(AEnv, number, Car_Ret), goto(sys_start, AEnv), _8552=_GORES;_8552=[])))
				]),
		t=FnResult
	      ),
	      block_exit(/=, FnResult),
	      true).
:- set_opv(/=, symbol_function, f_c47_c61),
   DefunResult= /= .
/*
:- side_effect(assert_lsp(/=,
			  lambda_def(defun,
				     /=,
				     f_c47_c61,
				     [number, c38_rest, sys_numbers],
				     
				     [ 
				       [ tagbody,
					 sys_start,
					 
					 [ when,
					   sys_numbers,
					   
					   [ dolist,
					     [sys_n, sys_numbers],
					     
					     [ when,
					       [=, number, sys_n],
					       [return_from, /=]
					     ]
					   ],
					   [setq, number, [pop, sys_numbers]],
					   [go, sys_start]
					 ]
				       ],
				       t
				     ]))).
*/
/*
:- side_effect(assert_lsp(/=,
			  arglist_info(/=,
				       f_c47_c61,
				       [number, c38_rest, sys_numbers],
				       arginfo{ all:[number],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[number, sys_numbers],
						opt:0,
						req:[number],
						rest:[sys_numbers],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(/=, init_args(1, f_c47_c61))).
*/
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun > (&rest numbers) (apply #'< (reverse numbers)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:7257 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,>,['&rest',numbers],[apply,function(<),[reverse,numbers]]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun <= (number &rest numbers)
  (dolist (n numbers t)
    (when (< n number)
      (return-from <=))
    (setq number n)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:7353 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,<=,[number,'&rest',numbers],[dolist,[n,numbers,t],[when,[<,n,number],['return-from',<=]],[setq,number,n]]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun >= (number &rest numbers)
  (dolist (n numbers t)
    (when (< number n)
      (return-from >=))
    (setq number n)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:7521 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,>=,[number,'&rest',numbers],[dolist,[n,numbers,t],[when,[<,number,n],['return-from',>=]],[setq,number,n]]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun max (real &rest reals)
  (dolist (r reals real)
    (when (< real r)
      (setq real r))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:7689 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,max,[real,'&rest',reals],[dolist,[r,reals,real],[when,[<,real,r],[setq,real,r]]]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun min (real &rest reals)
  (dolist (r reals real)
    (when (< r real)
      (setq real r))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:7829 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,min,[real,'&rest',reals],[dolist,[r,reals,real],[when,[<,r,real],[setq,real,r]]]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun oddp (integer)
  (= (mod integer 2) 1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:7969 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,oddp,[integer],[=,[mod,integer,2],1]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun evenp (integer)
  (= (mod integer 2) 0))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8055 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,evenp,[integer],[=,[mod,integer,2],0]]]]))
/*
#+(or WAM-CL LISP500) 
(defun minusp (real)
  (< real 0))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8144 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,minusp,[real],[<,real,0]])
wl:lambda_def(defun, minusp, f_minusp, [real], [[<, real, 0]]).
wl:arglist_info(minusp, f_minusp, [real], arginfo{all:[real], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[real], opt:0, req:[real], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_minusp).

/*

### Compiled Function: `CL:MINUSP` 
*/
f_minusp(Real_In, FnResult) :-
	GEnv=[bv(real, Real_In)],
	catch(( ( get_var(GEnv, real, Real_Get),
		  'f_<'(Real_Get, 0, _9014)
		),
		_9014=FnResult
	      ),
	      block_exit(minusp, FnResult),
	      true).
:- set_opv(minusp, symbol_function, f_minusp),
   DefunResult=minusp.
/*
:- side_effect(assert_lsp(minusp,
			  lambda_def(defun,
				     minusp,
				     f_minusp,
				     [real],
				     [[<, real, 0]]))).
*/
/*
:- side_effect(assert_lsp(minusp,
			  arglist_info(minusp,
				       f_minusp,
				       [real],
				       arginfo{ all:[real],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[real],
						opt:0,
						req:[real],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(minusp, init_args(x, f_minusp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun plusp (real)
  (< 0 real))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8209 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,plusp,[real],[<,0,real]])
wl:lambda_def(defun, plusp, f_plusp, [real], [[<, 0, real]]).
wl:arglist_info(plusp, f_plusp, [real], arginfo{all:[real], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[real], opt:0, req:[real], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_plusp).

/*

### Compiled Function: `CL:PLUSP` 
*/
f_plusp(Real_In, FnResult) :-
	GEnv=[bv(real, Real_In)],
	catch(( ( get_var(GEnv, real, Real_Get),
		  'f_<'(0, Real_Get, _8818)
		),
		_8818=FnResult
	      ),
	      block_exit(plusp, FnResult),
	      true).
:- set_opv(plusp, symbol_function, f_plusp),
   DefunResult=plusp.
/*
:- side_effect(assert_lsp(plusp,
			  lambda_def(defun, plusp, f_plusp, [real], [[<, 0, real]]))).
*/
/*
:- side_effect(assert_lsp(plusp,
			  arglist_info(plusp,
				       f_plusp,
				       [real],
				       arginfo{ all:[real],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[real],
						opt:0,
						req:[real],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(plusp, init_args(x, f_plusp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun zerop (real)
  (= real 0))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8273 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,zerop,[real],[=,real,0]])
wl:lambda_def(defun, zerop, f_zerop, [real], [[=, real, 0]]).
wl:arglist_info(zerop, f_zerop, [real], arginfo{all:[real], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[real], opt:0, req:[real], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_zerop).

/*

### Compiled Function: `CL:ZEROP` 
*/
f_zerop(Real_In, FnResult) :-
	GEnv=[bv(real, Real_In)],
	catch(( ( get_var(GEnv, real, Real_Get),
		  'f_='(Real_Get, 0, _8818)
		),
		_8818=FnResult
	      ),
	      block_exit(zerop, FnResult),
	      true).
:- set_opv(zerop, symbol_function, f_zerop),
   DefunResult=zerop.
/*
:- side_effect(assert_lsp(zerop,
			  lambda_def(defun, zerop, f_zerop, [real], [[=, real, 0]]))).
*/
/*
:- side_effect(assert_lsp(zerop,
			  arglist_info(zerop,
				       f_zerop,
				       [real],
				       arginfo{ all:[real],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[real],
						opt:0,
						req:[real],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(zerop, init_args(x, f_zerop))).
*/
/*
#+(or WAM-CL LISP500) 
(defun abs (number)
  (if (< number 0)
      (- number)
      number))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8337 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,abs,[number],[if,[<,number,0],[-,number],number]])
wl:lambda_def(defun, abs, f_abs, [number], [[if, [<, number, 0], [-, number], number]]).
wl:arglist_info(abs, f_abs, [number], arginfo{all:[number], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[number], opt:0, req:[number], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_abs).

/*

### Compiled Function: `CL:ABS` 
*/
f_abs(Number_In, FnResult) :-
	GEnv=[bv(number, Number_In)],
	catch(( ( get_var(GEnv, number, Number_Get),
		  (   Number_Get<0
		  ->  get_var(GEnv, number, Number_Get9),
		      'f_-'(0, Number_Get9, TrueResult),
		      _9272=TrueResult
		  ;   get_var(GEnv, number, Number_Get10),
		      _9272=Number_Get10
		  )
		),
		_9272=FnResult
	      ),
	      block_exit(abs, FnResult),
	      true).
:- set_opv(abs, symbol_function, f_abs),
   DefunResult=abs.
/*
:- side_effect(assert_lsp(abs,
			  lambda_def(defun,
				     abs,
				     f_abs,
				     [number],
				     [[if, [<, number, 0], [-, number], number]]))).
*/
/*
:- side_effect(assert_lsp(abs,
			  arglist_info(abs,
				       f_abs,
				       [number],
				       arginfo{ all:[number],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[number],
						opt:0,
						req:[number],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(abs, init_args(x, f_abs))).
*/
/*
#+(or WAM-CL LISP500) 
(defun byte (size position)
  (cons size position))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8442 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,byte,[size,position],[cons,size,position]])
wl:lambda_def(defun, byte, f_byte, [sys_size, position], [[cons, sys_size, position]]).
wl:arglist_info(byte, f_byte, [sys_size, position], arginfo{all:[sys_size, position], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_size, position], opt:0, req:[sys_size, position], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_byte).

/*

### Compiled Function: `CL:BYTE` 
*/
f_byte(Size_In, Position_In, FnResult) :-
	GEnv=[bv(sys_size, Size_In), bv(position, Position_In)],
	catch(( ( get_var(GEnv, position, Position_Get),
		  get_var(GEnv, sys_size, Size_Get),
		  _9768=[Size_Get|Position_Get]
		),
		_9768=FnResult
	      ),
	      block_exit(byte, FnResult),
	      true).
:- set_opv(byte, symbol_function, f_byte),
   DefunResult=byte.
/*
:- side_effect(assert_lsp(byte,
			  lambda_def(defun,
				     byte,
				     f_byte,
				     [sys_size, position],
				     [[cons, sys_size, position]]))).
*/
/*
:- side_effect(assert_lsp(byte,
			  arglist_info(byte,
				       f_byte,
				       [sys_size, position],
				       arginfo{ all:[sys_size, position],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_size, position],
						opt:0,
						req:[sys_size, position],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(byte, init_args(x, f_byte))).
*/
/*
#+(or WAM-CL LISP500) 
(defun byte-size (bytespec)
  (car bytespec))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8522 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'byte-size',[bytespec],[car,bytespec]])
wl:lambda_def(defun, byte_size, f_byte_size, [sys_bytespec], [[car, sys_bytespec]]).
wl:arglist_info(byte_size, f_byte_size, [sys_bytespec], arginfo{all:[sys_bytespec], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_bytespec], opt:0, req:[sys_bytespec], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_byte_size).

/*

### Compiled Function: `CL:BYTE-SIZE` 
*/
f_byte_size(Bytespec_In, FnResult) :-
	GEnv=[bv(sys_bytespec, Bytespec_In)],
	catch(( ( get_var(GEnv, sys_bytespec, Bytespec_Get),
		  f_car(Bytespec_Get, Car_Ret)
		),
		Car_Ret=FnResult
	      ),
	      block_exit(byte_size, FnResult),
	      true).
:- set_opv(byte_size, symbol_function, f_byte_size),
   DefunResult=byte_size.
/*
:- side_effect(assert_lsp(byte_size,
			  lambda_def(defun,
				     byte_size,
				     f_byte_size,
				     [sys_bytespec],
				     [[car, sys_bytespec]]))).
*/
/*
:- side_effect(assert_lsp(byte_size,
			  arglist_info(byte_size,
				       f_byte_size,
				       [sys_bytespec],
				       arginfo{ all:[sys_bytespec],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_bytespec],
						opt:0,
						req:[sys_bytespec],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(byte_size, init_args(x, f_byte_size))).
*/
/*
#+(or WAM-CL LISP500) 
(defun byte-position (bytespec)
  (cdr bytespec))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8596 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'byte-position',[bytespec],[cdr,bytespec]])
wl:lambda_def(defun, byte_position, f_byte_position, [sys_bytespec], [[cdr, sys_bytespec]]).
wl:arglist_info(byte_position, f_byte_position, [sys_bytespec], arginfo{all:[sys_bytespec], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_bytespec], opt:0, req:[sys_bytespec], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_byte_position).

/*

### Compiled Function: `CL:BYTE-POSITION` 
*/
f_byte_position(Bytespec_In, FnResult) :-
	GEnv=[bv(sys_bytespec, Bytespec_In)],
	catch(( ( get_var(GEnv, sys_bytespec, Bytespec_Get),
		  f_cdr(Bytespec_Get, Cdr_Ret)
		),
		Cdr_Ret=FnResult
	      ),
	      block_exit(byte_position, FnResult),
	      true).
:- set_opv(byte_position, symbol_function, f_byte_position),
   DefunResult=byte_position.
/*
:- side_effect(assert_lsp(byte_position,
			  lambda_def(defun,
				     byte_position,
				     f_byte_position,
				     [sys_bytespec],
				     [[cdr, sys_bytespec]]))).
*/
/*
:- side_effect(assert_lsp(byte_position,
			  arglist_info(byte_position,
				       f_byte_position,
				       [sys_bytespec],
				       arginfo{ all:[sys_bytespec],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_bytespec],
						opt:0,
						req:[sys_bytespec],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(byte_position, init_args(x, f_byte_position))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char= (&rest characters)
  (apply #'= (mapcar #'char-code characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8674 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char=',['&rest',characters],[apply,function(=),[mapcar,function('char-code'),characters]]])
wl:lambda_def(defun, char_c61, f_char_c61, [c38_rest, sys_characters], [[apply, function(=), [mapcar, function(char_code), sys_characters]]]).
wl:arglist_info(char_c61, f_char_c61, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_c61).

/*

### Compiled Function: `CL:CHAR=` 
*/
f_char_c61(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_code, [Characters_Get], Mapcar_Ret),
		  f_apply('f_=', Mapcar_Ret, Apply_Ret)
		),
		Apply_Ret=FnResult
	      ),
	      block_exit(char_c61, FnResult),
	      true).
:- set_opv(char_c61, symbol_function, f_char_c61),
   DefunResult=char_c61.
/*
:- side_effect(assert_lsp(char_c61,
			  lambda_def(defun,
				     char_c61,
				     f_char_c61,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(=),
					 
					 [ mapcar,
					   function(char_code),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_c61,
			  arglist_info(char_c61,
				       f_char_c61,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_c61, init_args(0, f_char_c61))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char/= (&rest characters)
  (apply #'/= (mapcar #'char-code characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8781 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char/=',['&rest',characters],[apply,function(/=),[mapcar,function('char-code'),characters]]])
wl:lambda_def(defun, char_c47_c61, f_char_c47_c61, [c38_rest, sys_characters], [[apply, function(/=), [mapcar, function(char_code), sys_characters]]]).
wl:arglist_info(char_c47_c61, f_char_c47_c61, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_c47_c61).

/*

### Compiled Function: `CL:CHAR/=` 
*/
f_char_c47_c61(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_code,
			   [Characters_Get],
			   [C47_c61_Param|KeysNRest]),
		  f_c47_c61(C47_c61_Param, KeysNRest, C47_c61_Ret)
		),
		C47_c61_Ret=FnResult
	      ),
	      block_exit(char_c47_c61, FnResult),
	      true).
:- set_opv(char_c47_c61, symbol_function, f_char_c47_c61),
   DefunResult=char_c47_c61.
/*
:- side_effect(assert_lsp(char_c47_c61,
			  lambda_def(defun,
				     char_c47_c61,
				     f_char_c47_c61,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(/=),
					 
					 [ mapcar,
					   function(char_code),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_c47_c61,
			  arglist_info(char_c47_c61,
				       f_char_c47_c61,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_c47_c61, init_args(0, f_char_c47_c61))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char< (&rest characters)
  (apply #'< (mapcar #'char-code characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8890 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char<',['&rest',characters],[apply,function(<),[mapcar,function('char-code'),characters]]])
wl:lambda_def(defun, char_c60, f_char_c60, [c38_rest, sys_characters], [[apply, function(<), [mapcar, function(char_code), sys_characters]]]).
wl:arglist_info(char_c60, f_char_c60, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_c60).

/*

### Compiled Function: `CL:CHAR<` 
*/
f_char_c60(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_code, [Characters_Get], Mapcar_Ret),
		  f_apply('f_<', Mapcar_Ret, Apply_Ret)
		),
		Apply_Ret=FnResult
	      ),
	      block_exit(char_c60, FnResult),
	      true).
:- set_opv(char_c60, symbol_function, f_char_c60),
   DefunResult=char_c60.
/*
:- side_effect(assert_lsp(char_c60,
			  lambda_def(defun,
				     char_c60,
				     f_char_c60,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(<),
					 
					 [ mapcar,
					   function(char_code),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_c60,
			  arglist_info(char_c60,
				       f_char_c60,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_c60, init_args(0, f_char_c60))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char> (&rest characters)
  (apply #'> (mapcar #'char-code characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:8997 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char>',['&rest',characters],[apply,function(>),[mapcar,function('char-code'),characters]]])
wl:lambda_def(defun, char_c62, f_char_c62, [c38_rest, sys_characters], [[apply, function(>), [mapcar, function(char_code), sys_characters]]]).
wl:arglist_info(char_c62, f_char_c62, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_c62).

/*

### Compiled Function: `CL:CHAR>` 
*/
f_char_c62(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_code, [Characters_Get], Mapcar_Ret),
		  f_apply('f_>', Mapcar_Ret, Apply_Ret)
		),
		Apply_Ret=FnResult
	      ),
	      block_exit(char_c62, FnResult),
	      true).
:- set_opv(char_c62, symbol_function, f_char_c62),
   DefunResult=char_c62.
/*
:- side_effect(assert_lsp(char_c62,
			  lambda_def(defun,
				     char_c62,
				     f_char_c62,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(>),
					 
					 [ mapcar,
					   function(char_code),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_c62,
			  arglist_info(char_c62,
				       f_char_c62,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_c62, init_args(0, f_char_c62))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char<= (&rest characters)
  (apply #'<= (mapcar #'char-code characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9104 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char<=',['&rest',characters],[apply,function(<=),[mapcar,function('char-code'),characters]]])
wl:lambda_def(defun, char_c60_c61, f_char_c60_c61, [c38_rest, sys_characters], [[apply, function(<=), [mapcar, function(char_code), sys_characters]]]).
wl:arglist_info(char_c60_c61, f_char_c60_c61, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_c60_c61).

/*

### Compiled Function: `CL:CHAR<=` 
*/
f_char_c60_c61(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_code, [Characters_Get], Mapcar_Ret),
		  f_apply('f_<=', Mapcar_Ret, Apply_Ret)
		),
		Apply_Ret=FnResult
	      ),
	      block_exit(char_c60_c61, FnResult),
	      true).
:- set_opv(char_c60_c61, symbol_function, f_char_c60_c61),
   DefunResult=char_c60_c61.
/*
:- side_effect(assert_lsp(char_c60_c61,
			  lambda_def(defun,
				     char_c60_c61,
				     f_char_c60_c61,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(<=),
					 
					 [ mapcar,
					   function(char_code),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_c60_c61,
			  arglist_info(char_c60_c61,
				       f_char_c60_c61,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_c60_c61, init_args(0, f_char_c60_c61))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char>= (&rest characters)
  (apply #'>= (mapcar #'char-code characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9213 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char>=',['&rest',characters],[apply,function(>=),[mapcar,function('char-code'),characters]]])
wl:lambda_def(defun, char_c62_c61, f_char_c62_c61, [c38_rest, sys_characters], [[apply, function(>=), [mapcar, function(char_code), sys_characters]]]).
wl:arglist_info(char_c62_c61, f_char_c62_c61, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_c62_c61).

/*

### Compiled Function: `CL:CHAR>=` 
*/
f_char_c62_c61(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_code, [Characters_Get], Mapcar_Ret),
		  f_apply('f_>=', Mapcar_Ret, Apply_Ret)
		),
		Apply_Ret=FnResult
	      ),
	      block_exit(char_c62_c61, FnResult),
	      true).
:- set_opv(char_c62_c61, symbol_function, f_char_c62_c61),
   DefunResult=char_c62_c61.
/*
:- side_effect(assert_lsp(char_c62_c61,
			  lambda_def(defun,
				     char_c62_c61,
				     f_char_c62_c61,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(>=),
					 
					 [ mapcar,
					   function(char_code),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_c62_c61,
			  arglist_info(char_c62_c61,
				       f_char_c62_c61,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_c62_c61, init_args(0, f_char_c62_c61))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-equal (&rest characters)
  (apply #'char= (mapcar #'char-upcase characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9322 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-equal',['&rest',characters],[apply,function('char='),[mapcar,function('char-upcase'),characters]]])
wl:lambda_def(defun, char_equal, f_char_equal, [c38_rest, sys_characters], [[apply, function(char_c61), [mapcar, function(char_upcase), sys_characters]]]).
wl:arglist_info(char_equal, f_char_equal, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_equal).

/*

### Compiled Function: `CL:CHAR-EQUAL` 
*/
f_char_equal(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_upcase, [Characters_Get], Char_c61_Param),
		  f_char_c61(Char_c61_Param, Char_c61_Ret)
		),
		Char_c61_Ret=FnResult
	      ),
	      block_exit(char_equal, FnResult),
	      true).
:- set_opv(char_equal, symbol_function, f_char_equal),
   DefunResult=char_equal.
/*
:- side_effect(assert_lsp(char_equal,
			  lambda_def(defun,
				     char_equal,
				     f_char_equal,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(char_c61),
					 
					 [ mapcar,
					   function(char_upcase),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_equal,
			  arglist_info(char_equal,
				       f_char_equal,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_equal, init_args(0, f_char_equal))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-not-equal (&rest characters)
  (apply #'char/= (mapcar #'char-upcase characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9440 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-not-equal',['&rest',characters],[apply,function('char/='),[mapcar,function('char-upcase'),characters]]])
wl:lambda_def(defun, char_not_equal, f_char_not_equal, [c38_rest, sys_characters], [[apply, function(char_c47_c61), [mapcar, function(char_upcase), sys_characters]]]).
wl:arglist_info(char_not_equal, f_char_not_equal, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_not_equal).

/*

### Compiled Function: `CL:CHAR-NOT-EQUAL` 
*/
f_char_not_equal(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_upcase, [Characters_Get], C47_c61_Param),
		  f_char_c47_c61(C47_c61_Param, C47_c61_Ret)
		),
		C47_c61_Ret=FnResult
	      ),
	      block_exit(char_not_equal, FnResult),
	      true).
:- set_opv(char_not_equal, symbol_function, f_char_not_equal),
   DefunResult=char_not_equal.
/*
:- side_effect(assert_lsp(char_not_equal,
			  lambda_def(defun,
				     char_not_equal,
				     f_char_not_equal,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(char_c47_c61),
					 
					 [ mapcar,
					   function(char_upcase),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_not_equal,
			  arglist_info(char_not_equal,
				       f_char_not_equal,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_not_equal, init_args(0, f_char_not_equal))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-lessp (&rest characters)
  (apply #'char< (mapcar #'char-upcase characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9563 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-lessp',['&rest',characters],[apply,function('char<'),[mapcar,function('char-upcase'),characters]]])
wl:lambda_def(defun, char_lessp, f_char_lessp, [c38_rest, sys_characters], [[apply, function(char_c60), [mapcar, function(char_upcase), sys_characters]]]).
wl:arglist_info(char_lessp, f_char_lessp, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_lessp).

/*

### Compiled Function: `CL:CHAR-LESSP` 
*/
f_char_lessp(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_upcase, [Characters_Get], Char_c60_Param),
		  f_char_c60(Char_c60_Param, Char_c60_Ret)
		),
		Char_c60_Ret=FnResult
	      ),
	      block_exit(char_lessp, FnResult),
	      true).
:- set_opv(char_lessp, symbol_function, f_char_lessp),
   DefunResult=char_lessp.
/*
:- side_effect(assert_lsp(char_lessp,
			  lambda_def(defun,
				     char_lessp,
				     f_char_lessp,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(char_c60),
					 
					 [ mapcar,
					   function(char_upcase),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_lessp,
			  arglist_info(char_lessp,
				       f_char_lessp,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_lessp, init_args(0, f_char_lessp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-greaterp (&rest characters)
  (apply #'char> (mapcar #'char-upcase characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9681 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-greaterp',['&rest',characters],[apply,function('char>'),[mapcar,function('char-upcase'),characters]]])
wl:lambda_def(defun, char_greaterp, f_char_greaterp, [c38_rest, sys_characters], [[apply, function(char_c62), [mapcar, function(char_upcase), sys_characters]]]).
wl:arglist_info(char_greaterp, f_char_greaterp, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_greaterp).

/*

### Compiled Function: `CL:CHAR-GREATERP` 
*/
f_char_greaterp(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_upcase, [Characters_Get], Char_c62_Param),
		  f_char_c62(Char_c62_Param, Char_c62_Ret)
		),
		Char_c62_Ret=FnResult
	      ),
	      block_exit(char_greaterp, FnResult),
	      true).
:- set_opv(char_greaterp, symbol_function, f_char_greaterp),
   DefunResult=char_greaterp.
/*
:- side_effect(assert_lsp(char_greaterp,
			  lambda_def(defun,
				     char_greaterp,
				     f_char_greaterp,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(char_c62),
					 
					 [ mapcar,
					   function(char_upcase),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_greaterp,
			  arglist_info(char_greaterp,
				       f_char_greaterp,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_greaterp, init_args(0, f_char_greaterp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-not-greaterp (&rest characters)
  (apply #'char<= (mapcar #'char-upcase characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9802 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-not-greaterp',['&rest',characters],[apply,function('char<='),[mapcar,function('char-upcase'),characters]]])
wl:lambda_def(defun, char_not_greaterp, f_char_not_greaterp, [c38_rest, sys_characters], [[apply, function(char_c60_c61), [mapcar, function(char_upcase), sys_characters]]]).
wl:arglist_info(char_not_greaterp, f_char_not_greaterp, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_not_greaterp).

/*

### Compiled Function: `CL:CHAR-NOT-GREATERP` 
*/
f_char_not_greaterp(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_upcase, [Characters_Get], C60_c61_Param),
		  f_char_c60_c61(C60_c61_Param, C60_c61_Ret)
		),
		C60_c61_Ret=FnResult
	      ),
	      block_exit(char_not_greaterp, FnResult),
	      true).
:- set_opv(char_not_greaterp, symbol_function, f_char_not_greaterp),
   DefunResult=char_not_greaterp.
/*
:- side_effect(assert_lsp(char_not_greaterp,
			  lambda_def(defun,
				     char_not_greaterp,
				     f_char_not_greaterp,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(char_c60_c61),
					 
					 [ mapcar,
					   function(char_upcase),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_not_greaterp,
			  arglist_info(char_not_greaterp,
				       f_char_not_greaterp,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_not_greaterp, init_args(0, f_char_not_greaterp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-not-lessp (&rest characters)
  (apply #'char>= (mapcar #'char-upcase characters)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:9928 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-not-lessp',['&rest',characters],[apply,function('char>='),[mapcar,function('char-upcase'),characters]]])
wl:lambda_def(defun, char_not_lessp, f_char_not_lessp, [c38_rest, sys_characters], [[apply, function(char_c62_c61), [mapcar, function(char_upcase), sys_characters]]]).
wl:arglist_info(char_not_lessp, f_char_not_lessp, [c38_rest, sys_characters], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_characters], opt:0, req:0, rest:[sys_characters], sublists:0, whole:0}).
wl: init_args(0, f_char_not_lessp).

/*

### Compiled Function: `CL:CHAR-NOT-LESSP` 
*/
f_char_not_lessp(RestNKeys, FnResult) :-
	GEnv=[bv(sys_characters, RestNKeys)],
	catch(( ( get_var(GEnv, sys_characters, Characters_Get),
		  f_mapcar(f_char_upcase, [Characters_Get], C62_c61_Param),
		  f_char_c62_c61(C62_c61_Param, C62_c61_Ret)
		),
		C62_c61_Ret=FnResult
	      ),
	      block_exit(char_not_lessp, FnResult),
	      true).
:- set_opv(char_not_lessp, symbol_function, f_char_not_lessp),
   DefunResult=char_not_lessp.
/*
:- side_effect(assert_lsp(char_not_lessp,
			  lambda_def(defun,
				     char_not_lessp,
				     f_char_not_lessp,
				     [c38_rest, sys_characters],
				     
				     [ 
				       [ apply,
					 function(char_c62_c61),
					 
					 [ mapcar,
					   function(char_upcase),
					   sys_characters
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_not_lessp,
			  arglist_info(char_not_lessp,
				       f_char_not_lessp,
				       [c38_rest, sys_characters],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_characters],
						opt:0,
						req:0,
						rest:[sys_characters],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_not_lessp, init_args(0, f_char_not_lessp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun character (character)
  (if (characterp character)
      character
      (let ((string (designator-string character)))
	(if (= (length string) 1)
	    (aref string 0)
	    (error 'type-error :datum string :expected-type '(string 1))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:10051 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,character,[character],[if,[characterp,character],character,[let,[[string,['designator-string',character]]],[if,[=,[length,string],1],[aref,string,0],[error,[quote,'type-error'],':datum',string,':expected-type',[quote,[string,1]]]]]]])
wl:lambda_def(defun, character, f_character, [character], [[if, [characterp, character], character, [let, [[string, [sys_designator_string, character]]], [if, [=, [length, string], 1], [aref, string, 0], [error, [quote, type_error], kw_datum, string, kw_expected_type, [quote, [string, 1]]]]]]]).
wl:arglist_info(character, f_character, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_character).

/*

### Compiled Function: `CL:CHARACTER` 
*/
f_character(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  (   string:is_characterp(Character_Get)
		  ->  get_var(GEnv, character, Character_Get9),
		      _7546=Character_Get9
		  ;   get_var(GEnv, character, Character_Get10),
		      f_sys_designator_string(Character_Get10,
					      Designator_string_Ret),
		      locally_set(string,
				  Designator_string_Ret,
				  (get_var(GEnv, string, String_Get), f_length(String_Get, PredArg1Result), (PredArg1Result=:=1->get_var(GEnv, string, String_Get15), f_aref(String_Get15, [0], TrueResult), ElseResult20=TrueResult;get_var(GEnv, string, String_Get16), f_error([type_error, kw_datum, String_Get16, kw_expected_type, [string, 1]], ElseResult), ElseResult20=ElseResult))),
		      _7546=ElseResult20
		  )
		),
		_7546=FnResult
	      ),
	      block_exit(character, FnResult),
	      true).
:- set_opv(character, symbol_function, f_character),
   DefunResult=character.
/*
:- side_effect(assert_lsp(character,
			  lambda_def(defun,
				     character,
				     f_character,
				     [character],
				     
				     [ 
				       [ if,
					 [characterp, character],
					 character,
					 
					 [ let,
					   
					   [ 
					     [ string,
					       
					       [ sys_designator_string,
						 character
					       ]
					     ]
					   ],
					   
					   [ if,
					     [=, [length, string], 1],
					     [aref, string, 0],
					     
					     [ error,
					       [quote, type_error],
					       kw_datum,
					       string,
					       kw_expected_type,
					       [quote, [string, 1]]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(character,
			  arglist_info(character,
				       f_character,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(character, init_args(x, f_character))).
*/
/*
#+LISP500
(defun characterp (object) (= (ldb '(5 . 0) (ival object)) 24))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:10328 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':LISP500'],[defun,characterp,[object],[=,[ldb,[quote,[5|0]],[ival,object]],24]]]))
/*
#+(or WAM-CL LISP500) 
(defun alpha-char-p (character)
  (let ((code (char-code character)))
    (or (< 64 code 91)
	(< 96 code 123)
	(< 159 code))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:10406 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'alpha-char-p',[character],[let,[[code,['char-code',character]]],[or,[<,64,code,91],[<,96,code,123],[<,159,code]]]])
wl:lambda_def(defun, alpha_char_p, f_alpha_char_p, [character], [[let, [[sys_code, [char_code, character]]], [or, [<, 64, sys_code, 91], [<, 96, sys_code, 123], [<, 159, sys_code]]]]).
wl:arglist_info(alpha_char_p, f_alpha_char_p, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_alpha_char_p).

/*

### Compiled Function: `CL:ALPHA-CHAR-P` 
*/
f_alpha_char_p(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, Code_Init),
		  LEnv=[bv(sys_code, Code_Init)|GEnv],
		  (   get_var(LEnv, sys_code, Code_Get),
		      (   64<Code_Get
		      ->  get_var(LEnv, sys_code, Code_Get14),
			  'f_<'(Code_Get14, 91, TrueResult),
			  FORM1_Res24=TrueResult
		      ;   FORM1_Res24=[]
		      ),
		      FORM1_Res24\==[],
		      LetResult=FORM1_Res24
		  ->  true
		  ;   (   get_var(LEnv, sys_code, Code_Get17),
			  (   96<Code_Get17
			  ->  get_var(LEnv, sys_code, Code_Get20),
			      'f_<'(Code_Get20, 123, TrueResult21),
			      FORM1_Res=TrueResult21
			  ;   FORM1_Res=[]
			  ),
			  FORM1_Res\==[],
			  _10328=FORM1_Res
		      ->  true
		      ;   get_var(LEnv, sys_code, Code_Get22),
			  'f_<'(159, Code_Get22, _23188),
			  _10328=_23188
		      ),
		      LetResult=_10328
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(alpha_char_p, FnResult),
	      true).
:- set_opv(alpha_char_p, symbol_function, f_alpha_char_p),
   DefunResult=alpha_char_p.
/*
:- side_effect(assert_lsp(alpha_char_p,
			  lambda_def(defun,
				     alpha_char_p,
				     f_alpha_char_p,
				     [character],
				     
				     [ 
				       [ let,
					 [[sys_code, [char_code, character]]],
					 
					 [ or,
					   [<, 64, sys_code, 91],
					   [<, 96, sys_code, 123],
					   [<, 159, sys_code]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(alpha_char_p,
			  arglist_info(alpha_char_p,
				       f_alpha_char_p,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(alpha_char_p, init_args(x, f_alpha_char_p))).
*/
/*
#+(or WAM-CL LISP500) 
(defun alphanumericp (character)
  (let ((code (char-code character)))
    (or (< 47 code 58)
	(< 64 code 91)
	(< 96 code 123)
	(< 159 code))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:10564 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,alphanumericp,[character],[let,[[code,['char-code',character]]],[or,[<,47,code,58],[<,64,code,91],[<,96,code,123],[<,159,code]]]])
wl:lambda_def(defun, alphanumericp, f_alphanumericp, [character], [[let, [[sys_code, [char_code, character]]], [or, [<, 47, sys_code, 58], [<, 64, sys_code, 91], [<, 96, sys_code, 123], [<, 159, sys_code]]]]).
wl:arglist_info(alphanumericp, f_alphanumericp, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_alphanumericp).

/*

### Compiled Function: `CL:ALPHANUMERICP` 
*/
f_alphanumericp(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, Code_Init),
		  LEnv=[bv(sys_code, Code_Init)|GEnv],
		  (   get_var(LEnv, sys_code, Code_Get),
		      (   47<Code_Get
		      ->  get_var(LEnv, sys_code, Code_Get14),
			  'f_<'(Code_Get14, 58, TrueResult),
			  FORM1_Res31=TrueResult
		      ;   FORM1_Res31=[]
		      ),
		      FORM1_Res31\==[],
		      LetResult=FORM1_Res31
		  ->  true
		  ;   (   get_var(LEnv, sys_code, Code_Get17),
			  (   64<Code_Get17
			  ->  get_var(LEnv, sys_code, Code_Get20),
			      'f_<'(Code_Get20, 91, TrueResult21),
			      FORM1_Res30=TrueResult21
			  ;   FORM1_Res30=[]
			  ),
			  FORM1_Res30\==[],
			  _10732=FORM1_Res30
		      ->  true
		      ;   (   get_var(LEnv, sys_code, Code_Get23),
			      (   96<Code_Get23
			      ->  get_var(LEnv, sys_code, Code_Get26),
				  'f_<'(Code_Get26, 123, TrueResult27),
				  FORM1_Res=TrueResult27
			      ;   FORM1_Res=[]
			      ),
			      FORM1_Res\==[],
			      _11054=FORM1_Res
			  ->  true
			  ;   get_var(LEnv, sys_code, Code_Get28),
			      'f_<'(159, Code_Get28, _11328),
			      _11054=_11328
			  ),
			  _10732=_11054
		      ),
		      LetResult=_10732
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(alphanumericp, FnResult),
	      true).
:- set_opv(alphanumericp, symbol_function, f_alphanumericp),
   DefunResult=alphanumericp.
/*
:- side_effect(assert_lsp(alphanumericp,
			  lambda_def(defun,
				     alphanumericp,
				     f_alphanumericp,
				     [character],
				     
				     [ 
				       [ let,
					 [[sys_code, [char_code, character]]],
					 
					 [ or,
					   [<, 47, sys_code, 58],
					   [<, 64, sys_code, 91],
					   [<, 96, sys_code, 123],
					   [<, 159, sys_code]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(alphanumericp,
			  arglist_info(alphanumericp,
				       f_alphanumericp,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(alphanumericp, init_args(x, f_alphanumericp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun digit-char (weight &optional (radix 10))
  (when (< weight radix)
    (if (< weight 10)
	(code-char (+ 48 weight))
	(code-char (+ 55 weight)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:10740 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'digit-char',[weight,'&optional',[radix,10]],[when,[<,weight,radix],[if,[<,weight,10],['code-char',[+,48,weight]],['code-char',[+,55,weight]]]]])
wl:lambda_def(defun, digit_char, f_digit_char, [sys_weight, c38_optional, [sys_radix, 10]], [[when, [<, sys_weight, sys_radix], [if, [<, sys_weight, 10], [code_char, [+, 48, sys_weight]], [code_char, [+, 55, sys_weight]]]]]).
wl:arglist_info(digit_char, f_digit_char, [sys_weight, c38_optional, [sys_radix, 10]], arginfo{all:[sys_weight, sys_radix], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_weight, sys_radix], opt:[sys_radix], req:[sys_weight], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_digit_char).

/*

### Compiled Function: `CL:DIGIT-CHAR` 
*/
f_digit_char(Weight_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_weight, Weight_In), bv(sys_radix, Radix_In)],
	opt_var(Env, sys_radix, Radix_In, true, 10, 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_radix, Radix_Get),
		  get_var(GEnv, sys_weight, Weight_Get),
		  (   Weight_Get<Radix_Get
		  ->  get_var(GEnv, sys_weight, Weight_Get14),
		      (   Weight_Get14<10
		      ->  get_var(GEnv, sys_weight, Weight_Get17),
			  'f_+'(48, Weight_Get17, Code_char_Param),
			  f_code_char(Code_char_Param, TrueResult),
			  TrueResult21=TrueResult
		      ;   get_var(GEnv, sys_weight, Weight_Get18),
			  'f_+'(55, Weight_Get18, Code_char_Param26),
			  f_code_char(Code_char_Param26, ElseResult),
			  TrueResult21=ElseResult
		      ),
		      _7302=TrueResult21
		  ;   _7302=[]
		  )
		),
		_7302=FnResult
	      ),
	      block_exit(digit_char, FnResult),
	      true).
:- set_opv(digit_char, symbol_function, f_digit_char),
   DefunResult=digit_char.
/*
:- side_effect(assert_lsp(digit_char,
			  lambda_def(defun,
				     digit_char,
				     f_digit_char,
				     [sys_weight, c38_optional, [sys_radix, 10]],
				     
				     [ 
				       [ when,
					 [<, sys_weight, sys_radix],
					 
					 [ if,
					   [<, sys_weight, 10],
					   [code_char, [+, 48, sys_weight]],
					   [code_char, [+, 55, sys_weight]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(digit_char,
			  arglist_info(digit_char,
				       f_digit_char,
				       
				       [ sys_weight,
					 c38_optional,
					 [sys_radix, 10]
				       ],
				       arginfo{ all:[sys_weight, sys_radix],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_weight, sys_radix],
						opt:[sys_radix],
						req:[sys_weight],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(digit_char, init_args(1, f_digit_char))).
*/
/*
#+(or WAM-CL LISP500) 
(defun digit-char-p (char &optional (radix 10))
  (let* ((code (char-code char))
	 (weight (if (< 47 code 58)
		     (- code 48)
		     (if (< 64 code 91)
			 (- code 55)
			 (when (< 96 code 123)
			   (- code 87))))))
    (and weight (< weight radix) weight)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:10923 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'digit-char-p',[char,'&optional',[radix,10]],['let*',[[code,['char-code',char]],[weight,[if,[<,47,code,58],[-,code,48],[if,[<,64,code,91],[-,code,55],[when,[<,96,code,123],[-,code,87]]]]]],[and,weight,[<,weight,radix],weight]]])
wl:lambda_def(defun, digit_char_p, f_digit_char_p, [char, c38_optional, [sys_radix, 10]], [[let_xx, [[sys_code, [char_code, char]], [sys_weight, [if, [<, 47, sys_code, 58], [-, sys_code, 48], [if, [<, 64, sys_code, 91], [-, sys_code, 55], [when, [<, 96, sys_code, 123], [-, sys_code, 87]]]]]], [and, sys_weight, [<, sys_weight, sys_radix], sys_weight]]]).
wl:arglist_info(digit_char_p, f_digit_char_p, [char, c38_optional, [sys_radix, 10]], arginfo{all:[char, sys_radix], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[char, sys_radix], opt:[sys_radix], req:[char], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_digit_char_p).

/*

### Compiled Function: `CL:DIGIT-CHAR-P` 
*/
f_digit_char_p(Char_In, RestNKeys, FnResult) :-
	GEnv=[bv(char, Char_In), bv(sys_radix, Radix_In)],
	opt_var(Env, sys_radix, Radix_In, true, 10, 1, RestNKeys),
	catch(( ( get_var(GEnv, char, Char_Get),
		  f_char_code(Char_Get, Code_Init),
		  LEnv=[bv(sys_code, Code_Init)|GEnv],
		  get_var(LEnv, sys_code, Code_Get),
		  (   47<Code_Get
		  ->  get_var(LEnv, sys_code, Code_Get21),
		      'f_<'(Code_Get21, 58, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, sys_code, Code_Get23),
		      'f_-'(Code_Get23, 48, TrueResult45),
		      Weight_Init=TrueResult45
		  ;   get_var(LEnv, sys_code, Code_Get27),
		      (   64<Code_Get27
		      ->  get_var(LEnv, sys_code, Code_Get30),
			  'f_<'(Code_Get30, 91, TrueResult31),
			  IFTEST24=TrueResult31
		      ;   IFTEST24=[]
		      ),
		      (   IFTEST24\==[]
		      ->  get_var(LEnv, sys_code, Code_Get32),
			  'f_-'(Code_Get32, 55, TrueResult43),
			  ElseResult46=TrueResult43
		      ;   get_var(LEnv, sys_code, Code_Get36),
			  (   96<Code_Get36
			  ->  get_var(LEnv, sys_code, Code_Get39),
			      'f_<'(Code_Get39, 123, TrueResult40),
			      IFTEST33=TrueResult40
			  ;   IFTEST33=[]
			  ),
			  (   IFTEST33\==[]
			  ->  get_var(LEnv, sys_code, Code_Get41),
			      'f_-'(Code_Get41, 87, TrueResult42),
			      ElseResult=TrueResult42
			  ;   ElseResult=[]
			  ),
			  ElseResult46=ElseResult
		      ),
		      Weight_Init=ElseResult46
		  ),
		  LEnv14=[bv(sys_weight, Weight_Init)|LEnv],
		  get_var(LEnv14, sys_weight, IFTEST48),
		  (   IFTEST48\==[]
		  ->  get_var(LEnv14, sys_radix, Radix_Get),
		      get_var(LEnv14, sys_weight, Weight_Get52),
		      (   Weight_Get52<Radix_Get
		      ->  get_var(LEnv14, sys_weight, Weight_Get57),
			  TrueResult59=Weight_Get57
		      ;   TrueResult59=[]
		      ),
		      LetResult13=TrueResult59
		  ;   LetResult13=[]
		  )
		),
		LetResult13=FnResult
	      ),
	      block_exit(digit_char_p, FnResult),
	      true).
:- set_opv(digit_char_p, symbol_function, f_digit_char_p),
   DefunResult=digit_char_p.
/*
:- side_effect(assert_lsp(digit_char_p,
			  lambda_def(defun,
				     digit_char_p,
				     f_digit_char_p,
				     [char, c38_optional, [sys_radix, 10]],
				     
				     [ 
				       [ let_xx,
					 
					 [ [sys_code, [char_code, char]],
					   
					   [ sys_weight,
					     
					     [ if,
					       [<, 47, sys_code, 58],
					       [-, sys_code, 48],
					       
					       [ if,
						 [<, 64, sys_code, 91],
						 [-, sys_code, 55],
						 
						 [ when,
						   [<, 96, sys_code, 123],
						   [-, sys_code, 87]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ and,
					   sys_weight,
					   [<, sys_weight, sys_radix],
					   sys_weight
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(digit_char_p,
			  arglist_info(digit_char_p,
				       f_digit_char_p,
				       [char, c38_optional, [sys_radix, 10]],
				       arginfo{ all:[char, sys_radix],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[char, sys_radix],
						opt:[sys_radix],
						req:[char],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(digit_char_p, init_args(1, f_digit_char_p))).
*/
/*
#+(or WAM-CL LISP500) 
(defun standard-char-p (character)
  (let ((code (char-code character)))
    (or (= code 10)
	(< 31 code 127))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:11221 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'standard-char-p',[character],[let,[[code,['char-code',character]]],[or,[=,code,10],[<,31,code,127]]]])
wl:lambda_def(defun, standard_char_p, f_standard_char_p, [character], [[let, [[sys_code, [char_code, character]]], [or, [=, sys_code, 10], [<, 31, sys_code, 127]]]]).
wl:arglist_info(standard_char_p, f_standard_char_p, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_standard_char_p).

/*

### Compiled Function: `CL:STANDARD-CHAR-P` 
*/
f_standard_char_p(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, Code_Init),
		  LEnv=[bv(sys_code, Code_Init)|GEnv],
		  (   get_var(LEnv, sys_code, Code_Get),
		      'f_='(Code_Get, 10, FORM1_Res),
		      FORM1_Res\==[],
		      LetResult=FORM1_Res
		  ->  true
		  ;   get_var(LEnv, sys_code, Code_Get12),
		      (   31<Code_Get12
		      ->  get_var(LEnv, sys_code, Code_Get15),
			  'f_<'(Code_Get15, 127, TrueResult),
			  _9704=TrueResult
		      ;   _9704=[]
		      ),
		      LetResult=_9704
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(standard_char_p, FnResult),
	      true).
:- set_opv(standard_char_p, symbol_function, f_standard_char_p),
   DefunResult=standard_char_p.
/*
:- side_effect(assert_lsp(standard_char_p,
			  lambda_def(defun,
				     standard_char_p,
				     f_standard_char_p,
				     [character],
				     
				     [ 
				       [ let,
					 [[sys_code, [char_code, character]]],
					 
					 [ or,
					   [=, sys_code, 10],
					   [<, 31, sys_code, 127]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(standard_char_p,
			  arglist_info(standard_char_p,
				       f_standard_char_p,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(standard_char_p, init_args(x, f_standard_char_p))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-upcase (character)
  (let ((code (char-code character)))
    (if (< 96 code 123)
	(code-char (- code 32))
	character)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:11364 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-upcase',[character],[let,[[code,['char-code',character]]],[if,[<,96,code,123],['code-char',[-,code,32]],character]]])
wl:lambda_def(defun, char_upcase, f_char_upcase, [character], [[let, [[sys_code, [char_code, character]]], [if, [<, 96, sys_code, 123], [code_char, [-, sys_code, 32]], character]]]).
wl:arglist_info(char_upcase, f_char_upcase, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_char_upcase).

/*

### Compiled Function: `CL:CHAR-UPCASE` 
*/
f_char_upcase(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, Code_Init),
		  LEnv=[bv(sys_code, Code_Init)|GEnv],
		  get_var(LEnv, sys_code, Code_Get),
		  (   96<Code_Get
		  ->  get_var(LEnv, sys_code, Code_Get16),
		      'f_<'(Code_Get16, 123, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, sys_code, Code_Get18),
		      'f_-'(Code_Get18, 32, Code_char_Param),
		      f_code_char(Code_char_Param, TrueResult20),
		      LetResult=TrueResult20
		  ;   get_var(LEnv, character, Character_Get19),
		      LetResult=Character_Get19
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(char_upcase, FnResult),
	      true).
:- set_opv(char_upcase, symbol_function, f_char_upcase),
   DefunResult=char_upcase.
/*
:- side_effect(assert_lsp(char_upcase,
			  lambda_def(defun,
				     char_upcase,
				     f_char_upcase,
				     [character],
				     
				     [ 
				       [ let,
					 [[sys_code, [char_code, character]]],
					 
					 [ if,
					   [<, 96, sys_code, 123],
					   [code_char, [-, sys_code, 32]],
					   character
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_upcase,
			  arglist_info(char_upcase,
				       f_char_upcase,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_upcase, init_args(x, f_char_upcase))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-downcase (character)
  (let ((code (char-code character)))
    (if (< 64 code 91)
	(code-char (+ code 32))
	character)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:11527 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-downcase',[character],[let,[[code,['char-code',character]]],[if,[<,64,code,91],['code-char',[+,code,32]],character]]])
wl:lambda_def(defun, char_downcase, f_char_downcase, [character], [[let, [[sys_code, [char_code, character]]], [if, [<, 64, sys_code, 91], [code_char, [+, sys_code, 32]], character]]]).
wl:arglist_info(char_downcase, f_char_downcase, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_char_downcase).

/*

### Compiled Function: `CL:CHAR-DOWNCASE` 
*/
f_char_downcase(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, Code_Init),
		  LEnv=[bv(sys_code, Code_Init)|GEnv],
		  get_var(LEnv, sys_code, Code_Get),
		  (   64<Code_Get
		  ->  get_var(LEnv, sys_code, Code_Get16),
		      'f_<'(Code_Get16, 91, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, sys_code, Code_Get18),
		      'f_+'(Code_Get18, 32, Code_char_Param),
		      f_code_char(Code_char_Param, TrueResult20),
		      LetResult=TrueResult20
		  ;   get_var(LEnv, character, Character_Get19),
		      LetResult=Character_Get19
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(char_downcase, FnResult),
	      true).
:- set_opv(char_downcase, symbol_function, f_char_downcase),
   DefunResult=char_downcase.
/*
:- side_effect(assert_lsp(char_downcase,
			  lambda_def(defun,
				     char_downcase,
				     f_char_downcase,
				     [character],
				     
				     [ 
				       [ let,
					 [[sys_code, [char_code, character]]],
					 
					 [ if,
					   [<, 64, sys_code, 91],
					   [code_char, [+, sys_code, 32]],
					   character
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_downcase,
			  arglist_info(char_downcase,
				       f_char_downcase,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_downcase, init_args(x, f_char_downcase))).
*/
/*
#+(or WAM-CL LISP500) 
(defun upper-case-p (character)
  (< 64 (char-code character) 91))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:11691 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'upper-case-p',[character],[<,64,['char-code',character],91]])
wl:lambda_def(defun, upper_case_p, f_upper_case_p, [character], [[<, 64, [char_code, character], 91]]).
wl:arglist_info(upper_case_p, f_upper_case_p, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_upper_case_p).

/*

### Compiled Function: `CL:UPPER-CASE-P` 
*/
f_upper_case_p(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, PredArg2Result),
		  (   64<PredArg2Result
		  ->  get_var(GEnv, character, Character_Get9),
		      f_char_code(Character_Get9, Char_code_Ret),
		      'f_<'(Char_code_Ret, 91, TrueResult),
		      _6738=TrueResult
		  ;   _6738=[]
		  )
		),
		_6738=FnResult
	      ),
	      block_exit(upper_case_p, FnResult),
	      true).
:- set_opv(upper_case_p, symbol_function, f_upper_case_p),
   DefunResult=upper_case_p.
/*
:- side_effect(assert_lsp(upper_case_p,
			  lambda_def(defun,
				     upper_case_p,
				     f_upper_case_p,
				     [character],
				     [[<, 64, [char_code, character], 91]]))).
*/
/*
:- side_effect(assert_lsp(upper_case_p,
			  arglist_info(upper_case_p,
				       f_upper_case_p,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(upper_case_p, init_args(x, f_upper_case_p))).
*/
/*
#+(or WAM-CL LISP500) 
(defun lower-case-p (character)
  (< 96 (char-code character) 123))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:11786 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'lower-case-p',[character],[<,96,['char-code',character],123]])
wl:lambda_def(defun, lower_case_p, f_lower_case_p, [character], [[<, 96, [char_code, character], 123]]).
wl:arglist_info(lower_case_p, f_lower_case_p, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_lower_case_p).

/*

### Compiled Function: `CL:LOWER-CASE-P` 
*/
f_lower_case_p(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, PredArg2Result),
		  (   96<PredArg2Result
		  ->  get_var(GEnv, character, Character_Get9),
		      f_char_code(Character_Get9, Char_code_Ret),
		      'f_<'(Char_code_Ret, 123, TrueResult),
		      _6738=TrueResult
		  ;   _6738=[]
		  )
		),
		_6738=FnResult
	      ),
	      block_exit(lower_case_p, FnResult),
	      true).
:- set_opv(lower_case_p, symbol_function, f_lower_case_p),
   DefunResult=lower_case_p.
/*
:- side_effect(assert_lsp(lower_case_p,
			  lambda_def(defun,
				     lower_case_p,
				     f_lower_case_p,
				     [character],
				     [[<, 96, [char_code, character], 123]]))).
*/
/*
:- side_effect(assert_lsp(lower_case_p,
			  arglist_info(lower_case_p,
				       f_lower_case_p,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(lower_case_p, init_args(x, f_lower_case_p))).
*/
/*
#+(or WAM-CL LISP500) 
(defun both-case-p (character)
  (or (upper-case-p character) (lower-case-p character)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:11882 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'both-case-p',[character],[or,['upper-case-p',character],['lower-case-p',character]]])
wl:lambda_def(defun, both_case_p, f_both_case_p, [character], [[or, [upper_case_p, character], [lower_case_p, character]]]).
wl:arglist_info(both_case_p, f_both_case_p, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_both_case_p).

/*

### Compiled Function: `CL:BOTH-CASE-P` 
*/
f_both_case_p(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( (   get_var(GEnv, character, Character_Get),
		    f_upper_case_p(Character_Get, FORM1_Res),
		    FORM1_Res\==[],
		    _6766=FORM1_Res
		->  true
		;   get_var(GEnv, character, Character_Get6),
		    f_lower_case_p(Character_Get6, Case_p_Ret),
		    _6766=Case_p_Ret
		),
		_6766=FnResult
	      ),
	      block_exit(both_case_p, FnResult),
	      true).
:- set_opv(both_case_p, symbol_function, f_both_case_p),
   DefunResult=both_case_p.
/*
:- side_effect(assert_lsp(both_case_p,
			  lambda_def(defun,
				     both_case_p,
				     f_both_case_p,
				     [character],
				     
				     [ 
				       [ or,
					 [upper_case_p, character],
					 [lower_case_p, character]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(both_case_p,
			  arglist_info(both_case_p,
				       f_both_case_p,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(both_case_p, init_args(x, f_both_case_p))).
*/
/*
#+(or WAM-CL LISP500) 
(defun char-int (character)
  (char-code character))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:11999 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'char-int',[character],['char-code',character]])
wl:lambda_def(defun, char_int, f_char_int, [character], [[char_code, character]]).
wl:arglist_info(char_int, f_char_int, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_char_int).

/*

### Compiled Function: `CL:CHAR-INT` 
*/
f_char_int(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, Char_code_Ret)
		),
		Char_code_Ret=FnResult
	      ),
	      block_exit(char_int, FnResult),
	      true).
:- set_opv(char_int, symbol_function, f_char_int),
   DefunResult=char_int.
/*
:- side_effect(assert_lsp(char_int,
			  lambda_def(defun,
				     char_int,
				     f_char_int,
				     [character],
				     [[char_code, character]]))).
*/
/*
:- side_effect(assert_lsp(char_int,
			  arglist_info(char_int,
				       f_char_int,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_int, init_args(x, f_char_int))).
*/
/*
#+(or WAM-CL LISP500) 
(defconstant char-code-limit 256)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:12080 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defconstant,'char-code-limit',256])
:- set_var(AEnv, char_code_limit, 256).
/*
#+(or WAM-CL LISP500) 
(let ((char-names '((0 . "Null")
		    (8 . "Backspace")
		    (9 . "Tab")
		    (10 . "Newline")
		    (12 . "Page")
		    (13 . "Return")
		    (32 . "Space")
		    (127 . "Rubout"))))
  (defun char-name (character)
    (let* ((code (char-code character))
	   (name (cdr (assoc code char-names))))
      (or name (when (< code 32)
		 (conc-string "U+" (integer-string code))))))
  (defun name-char (name)
    (setq name (designator-string name))
    (if (< (length name) 2)
	(aref name 0)
	(if (= (char-code (aref name 0)) 85)
	    (code-char (parse-integer name :start 2))
	    (let ((code (car (rassoc name char-names :test #'string-equal))))
	      (when code (code-char code)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:12141 **********************/
:-lisp_compile_to_prolog(pkg_sys,[let,[['char-names',[quote,[[0|'$STRING'("Null")],[8|'$STRING'("Backspace")],[9|'$STRING'("Tab")],[10|'$STRING'("Newline")],[12|'$STRING'("Page")],[13|'$STRING'("Return")],[32|'$STRING'("Space")],[127|'$STRING'("Rubout")]]]]],[defun,'char-name',[character],['let*',[[code,['char-code',character]],[name,[cdr,[assoc,code,'char-names']]]],[or,name,[when,[<,code,32],['conc-string','$STRING'("U+"),['integer-string',code]]]]]],[defun,'name-char',[name],[setq,name,['designator-string',name]],[if,[<,[length,name],2],[aref,name,0],[if,[=,['char-code',[aref,name,0]],85],['code-char',['parse-integer',name,':start',2]],[let,[[code,[car,[rassoc,name,'char-names',':test',function('string-equal')]]]],[when,code,['code-char',code]]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_conc_string,
					       kw_function,
					       f_sys_conc_string)).
*/
:- LEnv=[bv(sys_char_names, [[0|'$ARRAY'([*], claz_base_character, "Null")], [8|'$ARRAY'([*], claz_base_character, "Backspace")], [9|'$ARRAY'([*], claz_base_character, "Tab")], [10|'$ARRAY'([*], claz_base_character, "Newline")], [12|'$ARRAY'([*], claz_base_character, "Page")], [13|'$ARRAY'([*], claz_base_character, "Return")], [32|'$ARRAY'([*], claz_base_character, "Space")], [127|'$ARRAY'([*], claz_base_character, "Rubout")]])|CDR].
wl:lambda_def(defun, char_name, f_char_name, [character], [[let_xx, [[sys_code, [char_code, character]], [sys_name, [cdr, [assoc, sys_code, sys_char_names]]]], [or, sys_name, [when, [<, sys_code, 32], [sys_conc_string, '$ARRAY'([*], claz_base_character, "U+"), [sys_integer_string, sys_code]]]]]]).
wl:arglist_info(char_name, f_char_name, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_char_name).

/*

### Compiled Function: `CL:CHAR-NAME` 
*/
f_char_name(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_char_code(Character_Get, Code_Init),
		  LEnv10=[bv(sys_code, Code_Init)|GEnv],
		  get_var(LEnv10, sys_char_names, Char_names_Get),
		  get_var(LEnv10, sys_code, Code_Get),
		  f_assoc(Code_Get, Char_names_Get, [], Cdr_Param),
		  f_cdr(Cdr_Param, Name_Init),
		  LEnv15=[bv(sys_name, Name_Init)|LEnv10],
		  (   get_var(LEnv15, sys_name, Name_Get),
		      Name_Get\==[],
		      LetResult9=Name_Get
		  ->  true
		  ;   get_var(LEnv15, sys_code, Code_Get21),
		      (   Code_Get21<32
		      ->  get_var(LEnv15, sys_code, Code_Get24),
			  f_sys_integer_string(Code_Get24,
					       [],
					       Integer_string_Ret),
			  f_sys_conc_string('$ARRAY'([*],
						     claz_base_character,
						     "U+"),
					    Integer_string_Ret,
					    TrueResult),
			  _10574=TrueResult
		      ;   _10574=[]
		      ),
		      LetResult9=_10574
		  )
		),
		LetResult9=FnResult
	      ),
	      block_exit(char_name, FnResult),
	      true).
:- set_opv(char_name, symbol_function, f_char_name),
   DefunResult=char_name,
   assert_lsp(name_char,
	      wl:lambda_def(defun, name_char, f_name_char, [sys_name], [[setq, sys_name, [sys_designator_string, sys_name]], [if, [<, [length, sys_name], 2], [aref, sys_name, 0], [if, [=, [char_code, [aref, sys_name, 0]], 85], [code_char, [parse_integer, sys_name, kw_start, 2]], [let, [[sys_code, [car, [rassoc, sys_name, sys_char_names, kw_test, function(string_equal)]]]], [when, sys_code, [code_char, sys_code]]]]]])),
   assert_lsp(name_char,
	      wl:arglist_info(name_char, f_name_char, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0})),
   assert_lsp(name_char, wl:init_args(x, f_name_char)),
   assert_lsp(name_char,
	      (f_name_char(Name_In, FnResult29):-AEnv=[bv(sys_name, Name_In)], catch(((get_var(AEnv, sys_name, Name_Get33), f_sys_designator_string(Name_Get33, Name), set_var(AEnv, sys_name, Name), get_var(AEnv, sys_name, Name_Get35), f_length(Name_Get35, PredArg1Result37), (PredArg1Result37<2->get_var(AEnv, sys_name, Name_Get38), f_aref(Name_Get38, [0], TrueResult57), _10844=TrueResult57;get_var(AEnv, sys_name, Name_Get40), f_aref(Name_Get40, [0], Char_code_Param), f_char_code(Char_code_Param, PredArg1Result42), (PredArg1Result42=:=85->get_var(AEnv, sys_name, Name_Get43), f_parse_integer(Name_Get43, kw_start, 2, Code_char_Param), f_code_char(Code_char_Param, TrueResult55), ElseResult58=TrueResult55;get_var(AEnv, sys_char_names, Char_names_Get48), get_var(AEnv, sys_name, Name_Get47), f_rassoc(Name_Get47, Char_names_Get48, [kw_test, f_string_equal], Car_Param), f_car(Car_Param, Code_Init49), LEnv46=[bv(sys_code, Code_Init49)|AEnv], get_var(LEnv46, sys_code, IFTEST50), (IFTEST50\==[]->get_var(LEnv46, sys_code, Code_Get53), f_code_char(Code_Get53, TrueResult54), LetResult45=TrueResult54;LetResult45=[]), ElseResult58=LetResult45), _10844=ElseResult58)), _10844=FnResult29), block_exit(name_char, FnResult29), true))),
   set_opv(name_char, symbol_function, f_name_char),
   DefunResult60=name_char.
/*
:- side_effect(assert_lsp(char_name,
			  lambda_def(defun,
				     char_name,
				     f_char_name,
				     [character],
				     
				     [ 
				       [ let_xx,
					 
					 [ [sys_code, [char_code, character]],
					   
					   [ sys_name,
					     
					     [ cdr,
					       [assoc, sys_code, sys_char_names]
					     ]
					   ]
					 ],
					 
					 [ or,
					   sys_name,
					   
					   [ when,
					     [<, sys_code, 32],
					     
					     [ sys_conc_string,
					       '$ARRAY'([*],
							claz_base_character,
							"U+"),
					       [sys_integer_string, sys_code]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(char_name,
			  arglist_info(char_name,
				       f_char_name,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(char_name, init_args(x, f_char_name))).
*/
/*
:- side_effect(assert_lsp(name_char,
			  lambda_def(defun,
				     name_char,
				     f_name_char,
				     [sys_name],
				     
				     [ 
				       [ setq,
					 sys_name,
					 [sys_designator_string, sys_name]
				       ],
				       
				       [ if,
					 [<, [length, sys_name], 2],
					 [aref, sys_name, 0],
					 
					 [ if,
					   
					   [ (=),
					     [char_code, [aref, sys_name, 0]],
					     85
					   ],
					   
					   [ code_char,
					     
					     [ parse_integer,
					       sys_name,
					       kw_start,
					       2
					     ]
					   ],
					   
					   [ let,
					     
					     [ 
					       [ sys_code,
						 
						 [ car,
						   
						   [ rassoc,
						     sys_name,
						     sys_char_names,
						     kw_test,
						     function(string_equal)
						   ]
						 ]
					       ]
					     ],
					     
					     [ when,
					       sys_code,
					       [code_char, sys_code]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(name_char,
			  arglist_info(name_char,
				       f_name_char,
				       [sys_name],
				       arginfo{ all:[sys_name],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_name],
						opt:0,
						req:[sys_name],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(name_char, init_args(x, f_name_char))).
*/
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun atom (object) (not (consp object)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:12876 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,atom,[object],[not,[consp,object]]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun rplaca (cons object) (setf (car cons) object) cons)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:12957 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,rplaca,[cons,object],[setf,[car,cons],object],cons]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun rplacd (cons object) (setf (cdr cons) object) cons)



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:13054 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,rplacd,[cons,object],[setf,[cdr,cons],object],cons]]]))
/*
#+(or WAM-CL LISP500) 
(defun copy-tree (tree)
  (if (consp tree) (cons (copy-tree (car tree)) (copy-tree (cdr tree))) tree))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:13155 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'copy-tree',[tree],[if,[consp,tree],[cons,['copy-tree',[car,tree]],['copy-tree',[cdr,tree]]],tree]])
wl:lambda_def(defun, copy_tree, f_copy_tree, [sys_tree], [[if, [consp, sys_tree], [cons, [copy_tree, [car, sys_tree]], [copy_tree, [cdr, sys_tree]]], sys_tree]]).
wl:arglist_info(copy_tree, f_copy_tree, [sys_tree], arginfo{all:[sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_tree], opt:0, req:[sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_copy_tree).

/*

### Compiled Function: `CL:COPY-TREE` 
*/
f_copy_tree(Tree_In, FnResult) :-
	GEnv=[bv(sys_tree, Tree_In)],
	catch(( ( get_var(GEnv, sys_tree, Tree_Get),
		  (   c0nz:is_consp(Tree_Get)
		  ->  get_var(GEnv, sys_tree, Tree_Get9),
		      f_car(Tree_Get9, Copy_tree_Param),
		      f_copy_tree(Copy_tree_Param, Copy_tree_Ret),
		      get_var(GEnv, sys_tree, Tree_Get10),
		      f_cdr(Tree_Get10, Copy_tree_Param18),
		      f_copy_tree(Copy_tree_Param18, Copy_tree_Ret20),
		      TrueResult=[Copy_tree_Ret|Copy_tree_Ret20],
		      _7006=TrueResult
		  ;   get_var(GEnv, sys_tree, Tree_Get11),
		      _7006=Tree_Get11
		  )
		),
		_7006=FnResult
	      ),
	      block_exit(copy_tree, FnResult),
	      true).
:- set_opv(copy_tree, symbol_function, f_copy_tree),
   DefunResult=copy_tree.
/*
:- side_effect(assert_lsp(copy_tree,
			  lambda_def(defun,
				     copy_tree,
				     f_copy_tree,
				     [sys_tree],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ cons,
					   [copy_tree, [car, sys_tree]],
					   [copy_tree, [cdr, sys_tree]]
					 ],
					 sys_tree
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(copy_tree,
			  arglist_info(copy_tree,
				       f_copy_tree,
				       [sys_tree],
				       arginfo{ all:[sys_tree],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_tree],
						opt:0,
						req:[sys_tree],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(copy_tree, init_args(x, f_copy_tree))).
*/
/*
#+(or WAM-CL LISP500) 
(defun sublis (alist tree &rest rest)
  (if (consp tree)
      (let ((a (apply #'sublis alist (car tree) rest))
	    (d (apply #'sublis alist (cdr tree) rest)))
	(if (and (eq a (car tree)) (eq d (cdr tree)))
	    tree
	    (cons a d)))
      (let ((a (apply #'assoc tree alist rest)))
	(if a (cdr a) tree))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:13286 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,sublis,[alist,tree,'&rest',rest],[if,[consp,tree],[let,[[a,[apply,function(sublis),alist,[car,tree],rest]],[d,[apply,function(sublis),alist,[cdr,tree],rest]]],[if,[and,[eq,a,[car,tree]],[eq,d,[cdr,tree]]],tree,[cons,a,d]]],[let,[[a,[apply,function(assoc),tree,alist,rest]]],[if,a,[cdr,a],tree]]]])
wl:lambda_def(defun, sublis, f_sublis, [sys_alist, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [let, [[sys_a, [apply, function(sublis), sys_alist, [car, sys_tree], rest]], [sys_d, [apply, function(sublis), sys_alist, [cdr, sys_tree], rest]]], [if, [and, [eq, sys_a, [car, sys_tree]], [eq, sys_d, [cdr, sys_tree]]], sys_tree, [cons, sys_a, sys_d]]], [let, [[sys_a, [apply, function(assoc), sys_tree, sys_alist, rest]]], [if, sys_a, [cdr, sys_a], sys_tree]]]]).
wl:arglist_info(sublis, f_sublis, [sys_alist, sys_tree, c38_rest, rest], arginfo{all:[sys_alist, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_alist, sys_tree, rest], opt:0, req:[sys_alist, sys_tree], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, f_sublis).

/*

### Compiled Function: `CL:SUBLIS` 
*/
f_sublis(Alist_In, Tree_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_alist, Alist_In), bv(sys_tree, Tree_In), bv(rest, RestNKeys)],
	catch(( ( get_var(GEnv, sys_tree, Tree_Get),
		  (   c0nz:is_consp(Tree_Get)
		  ->  get_var(GEnv, sys_alist, Alist_Get),
		      get_var(GEnv, sys_tree, Tree_Get15),
		      f_car(Tree_Get15, Car_Ret),
		      get_var(GEnv, rest, Rest_Get),
		      f_apply(f_sublis, [Alist_Get, Car_Ret, Rest_Get], A_Init),
		      get_var(GEnv, sys_alist, Alist_Get17),
		      get_var(GEnv, sys_tree, Tree_Get18),
		      f_cdr(Tree_Get18, Cdr_Ret),
		      get_var(GEnv, rest, Rest_Get19),
		      f_apply(f_sublis,
			      [Alist_Get17, Cdr_Ret, Rest_Get19],
			      D_Init),
		      LEnv=[bv(sys_a, A_Init), bv(sys_d, D_Init)|GEnv],
		      get_var(LEnv, sys_a, A_Get),
		      get_var(LEnv, sys_tree, Tree_Get26),
		      f_car(Tree_Get26, PredArg2Result),
		      (   is_eq(A_Get, PredArg2Result)
		      ->  get_var(LEnv, sys_d, D_Get),
			  get_var(LEnv, sys_tree, Tree_Get31),
			  f_cdr(Tree_Get31, Cdr_Ret59),
			  f_eq(D_Get, Cdr_Ret59, TrueResult),
			  IFTEST22=TrueResult
		      ;   IFTEST22=[]
		      ),
		      (   IFTEST22\==[]
		      ->  get_var(LEnv, sys_tree, Tree_Get33),
			  LetResult=Tree_Get33
		      ;   get_var(LEnv, sys_a, A_Get34),
			  get_var(LEnv, sys_d, D_Get35),
			  ElseResult=[A_Get34|D_Get35],
			  LetResult=ElseResult
		      ),
		      _8398=LetResult
		  ;   get_var(GEnv, sys_alist, Alist_Get42),
		      ( get_var(GEnv, rest, Rest_Get43),
			get_var(GEnv, sys_tree, Tree_Get41)
		      ),
		      f_apply(f_assoc,
			      [Tree_Get41, Alist_Get42, Rest_Get43],
			      A_Init44),
		      LEnv40=[bv(sys_a, A_Init44)|GEnv],
		      get_var(LEnv40, sys_a, IFTEST45),
		      (   IFTEST45\==[]
		      ->  get_var(LEnv40, sys_a, A_Get48),
			  f_cdr(A_Get48, TrueResult50),
			  LetResult39=TrueResult50
		      ;   get_var(LEnv40, sys_tree, Tree_Get49),
			  LetResult39=Tree_Get49
		      ),
		      _8398=LetResult39
		  )
		),
		_8398=FnResult
	      ),
	      block_exit(sublis, FnResult),
	      true).
:- set_opv(sublis, symbol_function, f_sublis),
   DefunResult=sublis.
/*
:- side_effect(assert_lsp(sublis,
			  lambda_def(defun,
				     sublis,
				     f_sublis,
				     [sys_alist, sys_tree, c38_rest, rest],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ let,
					   
					   [ 
					     [ sys_a,
					       
					       [ apply,
						 function(sublis),
						 sys_alist,
						 [car, sys_tree],
						 rest
					       ]
					     ],
					     
					     [ sys_d,
					       
					       [ apply,
						 function(sublis),
						 sys_alist,
						 [cdr, sys_tree],
						 rest
					       ]
					     ]
					   ],
					   
					   [ if,
					     
					     [ and,
					       [eq, sys_a, [car, sys_tree]],
					       [eq, sys_d, [cdr, sys_tree]]
					     ],
					     sys_tree,
					     [cons, sys_a, sys_d]
					   ]
					 ],
					 
					 [ let,
					   
					   [ 
					     [ sys_a,
					       
					       [ apply,
						 function(assoc),
						 sys_tree,
						 sys_alist,
						 rest
					       ]
					     ]
					   ],
					   [if, sys_a, [cdr, sys_a], sys_tree]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sublis,
			  arglist_info(sublis,
				       f_sublis,
				       [sys_alist, sys_tree, c38_rest, rest],
				       arginfo{ all:[sys_alist, sys_tree],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_alist,
							sys_tree,
							rest
						      ],
						opt:0,
						req:[sys_alist, sys_tree],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sublis, init_args(2, f_sublis))).
*/
/*
#+(or WAM-CL LISP500) 
(defun nsublis (alist tree &rest rest)
  (if (consp tree)
      (progn
	(setf (car tree) (apply #'nsublis alist (car tree) rest))
	(setf (cdr tree) (apply #'nsublis alist (cdr tree) rest))
	tree)
      (let ((a (apply #'assoc tree alist rest)))
	(if a (cdr a) tree))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:13630 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nsublis,[alist,tree,'&rest',rest],[if,[consp,tree],[progn,[setf,[car,tree],[apply,function(nsublis),alist,[car,tree],rest]],[setf,[cdr,tree],[apply,function(nsublis),alist,[cdr,tree],rest]],tree],[let,[[a,[apply,function(assoc),tree,alist,rest]]],[if,a,[cdr,a],tree]]]])
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _9398, [], [], true), append([sys_tree], [CAR12, CAR], [sys_tree, CAR12, CAR]), setf_inverse_op(car, rplaca))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _9294, [], [], true), append([sys_tree], [CAR12, CAR], [sys_tree, CAR12, CAR]), setf_inverse_op(car, rplaca))).
*/
/*
:-side_effect((compile_each([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_9294,[],[],true),append([sys_tree],[_62832,_62660],[sys_tree,_62832,_62660]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_9294,[],[],true),append([sys_tree],[_38438,_38266],[sys_tree,_38438,_38266]),setf_inverse_op(cdr,rplacd))).
*/
wl:lambda_def(defun, nsublis, f_nsublis, [sys_alist, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [progn, [setf, [car, sys_tree], [apply, function(nsublis), sys_alist, [car, sys_tree], rest]], [setf, [cdr, sys_tree], [apply, function(nsublis), sys_alist, [cdr, sys_tree], rest]], sys_tree], [let, [[sys_a, [apply, function(assoc), sys_tree, sys_alist, rest]]], [if, sys_a, [cdr, sys_a], sys_tree]]]]).
wl:arglist_info(nsublis, f_nsublis, [sys_alist, sys_tree, c38_rest, rest], arginfo{all:[sys_alist, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_alist, sys_tree, rest], opt:0, req:[sys_alist, sys_tree], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, f_nsublis).

/*

### Compiled Function: `CL:NSUBLIS` 
*/
f_nsublis(Alist_In, Tree_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_alist, Alist_In), bv(sys_tree, Tree_In), bv(rest, RestNKeys)],
	catch(( ( get_var(GEnv, sys_tree, Tree_Get),
		  (   c0nz:is_consp(Tree_Get)
		  ->  get_var(GEnv, sys_alist, Alist_Get),
		      get_var(GEnv, sys_tree, Tree_Get13),
		      f_car(Tree_Get13, Car_Ret),
		      get_var(GEnv, rest, Rest_Get),
		      f_apply(f_nsublis,
			      [Alist_Get, Car_Ret, Rest_Get],
			      Apply_Ret),
		      f_rplaca(Tree_Get13, Apply_Ret, Rplaca_Ret),
		      get_var(GEnv, sys_alist, Alist_Get20),
		      get_var(GEnv, sys_tree, Tree_Get19),
		      f_cdr(Tree_Get19, Cdr_Ret),
		      get_var(GEnv, rest, Rest_Get22),
		      f_apply(f_nsublis,
			      [Alist_Get20, Cdr_Ret, Rest_Get22],
			      Apply_Ret47),
		      f_rplacd(Tree_Get19, Apply_Ret47, Rplacd_Ret),
		      get_var(GEnv, sys_tree, Tree_Get23),
		      _8028=Tree_Get23
		  ;   get_var(GEnv, sys_alist, Alist_Get28),
		      ( get_var(GEnv, rest, Rest_Get29),
			get_var(GEnv, sys_tree, Tree_Get27)
		      ),
		      f_apply(f_assoc,
			      [Tree_Get27, Alist_Get28, Rest_Get29],
			      A_Init),
		      LEnv=[bv(sys_a, A_Init)|GEnv],
		      get_var(LEnv, sys_a, IFTEST31),
		      (   IFTEST31\==[]
		      ->  get_var(LEnv, sys_a, A_Get34),
			  f_cdr(A_Get34, TrueResult),
			  LetResult=TrueResult
		      ;   get_var(LEnv, sys_tree, Tree_Get35),
			  LetResult=Tree_Get35
		      ),
		      _8028=LetResult
		  )
		),
		_8028=FnResult
	      ),
	      block_exit(nsublis, FnResult),
	      true).
:- set_opv(nsublis, symbol_function, f_nsublis),
   DefunResult=nsublis.
/*
:- side_effect(assert_lsp(nsublis,
			  lambda_def(defun,
				     nsublis,
				     f_nsublis,
				     [sys_alist, sys_tree, c38_rest, rest],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ progn,
					   
					   [ setf,
					     [car, sys_tree],
					     
					     [ apply,
					       function(nsublis),
					       sys_alist,
					       [car, sys_tree],
					       rest
					     ]
					   ],
					   
					   [ setf,
					     [cdr, sys_tree],
					     
					     [ apply,
					       function(nsublis),
					       sys_alist,
					       [cdr, sys_tree],
					       rest
					     ]
					   ],
					   sys_tree
					 ],
					 
					 [ let,
					   
					   [ 
					     [ sys_a,
					       
					       [ apply,
						 function(assoc),
						 sys_tree,
						 sys_alist,
						 rest
					       ]
					     ]
					   ],
					   [if, sys_a, [cdr, sys_a], sys_tree]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nsublis,
			  arglist_info(nsublis,
				       f_nsublis,
				       [sys_alist, sys_tree, c38_rest, rest],
				       arginfo{ all:[sys_alist, sys_tree],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_alist,
							sys_tree,
							rest
						      ],
						opt:0,
						req:[sys_alist, sys_tree],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nsublis, init_args(2, f_nsublis))).
*/
/*
#+(or WAM-CL LISP500) 
(defun copy-list (list)
  (if (consp list) (cons (car list) (copy-list (cdr list))) list))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:13933 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'copy-list',[list],[if,[consp,list],[cons,[car,list],['copy-list',[cdr,list]]],list]])
wl:lambda_def(defun, copy_list, f_copy_list, [list], [[if, [consp, list], [cons, [car, list], [copy_list, [cdr, list]]], list]]).
wl:arglist_info(copy_list, f_copy_list, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_copy_list).

/*

### Compiled Function: `CL:COPY-LIST` 
*/
f_copy_list(List_In, FnResult) :-
	GEnv=[bv(list, List_In)],
	catch(( ( get_var(GEnv, list, List_Get),
		  (   c0nz:is_consp(List_Get)
		  ->  get_var(GEnv, list, List_Get9),
		      f_car(List_Get9, Car_Ret),
		      get_var(GEnv, list, List_Get10),
		      f_cdr(List_Get10, Copy_list_Param),
		      f_copy_list(Copy_list_Param, Copy_list_Ret),
		      TrueResult=[Car_Ret|Copy_list_Ret],
		      _6956=TrueResult
		  ;   get_var(GEnv, list, List_Get11),
		      _6956=List_Get11
		  )
		),
		_6956=FnResult
	      ),
	      block_exit(copy_list, FnResult),
	      true).
:- set_opv(copy_list, symbol_function, f_copy_list),
   DefunResult=copy_list.
/*
:- side_effect(assert_lsp(copy_list,
			  lambda_def(defun,
				     copy_list,
				     f_copy_list,
				     [list],
				     
				     [ 
				       [ if,
					 [consp, list],
					 
					 [ cons,
					   [car, list],
					   [copy_list, [cdr, list]]
					 ],
					 list
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(copy_list,
			  arglist_info(copy_list,
				       f_copy_list,
				       [list],
				       arginfo{ all:[list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[list],
						opt:0,
						req:[list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(copy_list, init_args(x, f_copy_list))).
*/
/*
#+(or WAM-CL LISP500) 
(defun make-list (size &key initial-element)
  (if (= size 0) nil
      (cons initial-element
	    (make-list (- size 1) :initial-element initial-element))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:14052 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'make-list',[size,'&key','initial-element'],[if,[=,size,0],[],[cons,'initial-element',['make-list',[-,size,1],':initial-element','initial-element']]]])
wl:lambda_def(defun, make_list, f_make_list, [sys_size, c38_key, sys_initial_element], [[if, [=, sys_size, 0], [], [cons, sys_initial_element, [make_list, [-, sys_size, 1], kw_initial_element, sys_initial_element]]]]).
wl:arglist_info(make_list, f_make_list, [sys_size, c38_key, sys_initial_element], arginfo{all:[sys_size], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_initial_element], names:[sys_size, sys_initial_element], opt:0, req:[sys_size], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_make_list).

/*

### Compiled Function: `CL:MAKE-LIST` 
*/
f_make_list(Size_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_size, Size_In), bv(sys_initial_element, Initial_element_In)],
	get_kw(Env,
	       RestNKeys,
	       sys_initial_element,
	       sys_initial_element,
	       Initial_element_In,
	       []=Initial_element_In,
	       Initial_element_P),
	catch(( ( get_var(GEnv, sys_size, Size_Get),
		  (   Size_Get=:=0
		  ->  _7096=[]
		  ;   get_var(GEnv, sys_initial_element, Initial_element_Get),
		      get_var(GEnv, sys_size, Size_Get12),
		      'f_-'(Size_Get12, 1, Make_list_Param),
		      get_var(GEnv, sys_initial_element, Initial_element_Get13),
		      f_make_list(Make_list_Param,
				  [kw_initial_element, Initial_element_Get13],
				  Make_list_Ret),
		      ElseResult=[Initial_element_Get|Make_list_Ret],
		      _7096=ElseResult
		  )
		),
		_7096=FnResult
	      ),
	      block_exit(make_list, FnResult),
	      true).
:- set_opv(make_list, symbol_function, f_make_list),
   DefunResult=make_list.
/*
:- side_effect(assert_lsp(make_list,
			  lambda_def(defun,
				     make_list,
				     f_make_list,
				     [sys_size, c38_key, sys_initial_element],
				     
				     [ 
				       [ if,
					 [=, sys_size, 0],
					 [],
					 
					 [ cons,
					   sys_initial_element,
					   
					   [ make_list,
					     [-, sys_size, 1],
					     kw_initial_element,
					     sys_initial_element
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(make_list,
			  arglist_info(make_list,
				       f_make_list,
				       [sys_size, c38_key, sys_initial_element],
				       arginfo{ all:[sys_size],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[sys_initial_element],
						names:
						      [ sys_size,
							sys_initial_element
						      ],
						opt:0,
						req:[sys_size],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(make_list, init_args(1, f_make_list))).
*/
/*
#+(or WAM-CL LISP500) 
(defun list* (&rest objects)
  (if (cdr objects)
      (cons (car objects) (apply #'list* (cdr objects)))
      (car objects)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:14240 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'list*',['&rest',objects],[if,[cdr,objects],[cons,[car,objects],[apply,function('list*'),[cdr,objects]]],[car,objects]]])
wl:lambda_def(defun, list_xx, f_list_xx, [c38_rest, sys_objects], [[if, [cdr, sys_objects], [cons, [car, sys_objects], [apply, function(list_xx), [cdr, sys_objects]]], [car, sys_objects]]]).
wl:arglist_info(list_xx, f_list_xx, [c38_rest, sys_objects], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_objects], opt:0, req:0, rest:[sys_objects], sublists:0, whole:0}).
wl: init_args(1, f_list_xx).

/*

### Compiled Function: `CL:LIST*` 
*/
f_list_xx(RestNKeys, FnResult) :-
	GEnv=[bv(sys_objects, RestNKeys)],
	catch(( ( get_var(GEnv, sys_objects, Objects_Get),
		  f_cdr(Objects_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_objects, Objects_Get8),
		      f_car(Objects_Get8, Car_Ret),
		      get_var(GEnv, sys_objects, Objects_Get9),
		      f_cdr(Objects_Get9, [List_xx_Param|KeysNRest]),
		      f_list_xx(List_xx_Param, KeysNRest, List_xx_Ret),
		      TrueResult=[Car_Ret|List_xx_Ret],
		      _7088=TrueResult
		  ;   get_var(GEnv, sys_objects, Objects_Get10),
		      f_car(Objects_Get10, ElseResult),
		      _7088=ElseResult
		  )
		),
		_7088=FnResult
	      ),
	      block_exit(list_xx, FnResult),
	      true).
:- set_opv(list_xx, symbol_function, f_list_xx),
   DefunResult=list_xx.
/*
:- side_effect(assert_lsp(list_xx,
			  lambda_def(defun,
				     list_xx,
				     f_list_xx,
				     [c38_rest, sys_objects],
				     
				     [ 
				       [ if,
					 [cdr, sys_objects],
					 
					 [ cons,
					   [car, sys_objects],
					   
					   [ apply,
					     function(list_xx),
					     [cdr, sys_objects]
					   ]
					 ],
					 [car, sys_objects]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(list_xx,
			  arglist_info(list_xx,
				       f_list_xx,
				       [c38_rest, sys_objects],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_objects],
						opt:0,
						req:0,
						rest:[sys_objects],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(list_xx, init_args(1, f_list_xx))).
*/
/*
#+(or WAM-CL LISP500) 
(defun list-length (list)
  (let ((slow list)
	(fast list)
	(odd nil)
	(len 0))
    (tagbody
     start
       (when (atom fast) (return-from list-length len))
       (setf fast (cdr fast))
       (setf len (+ 1 len))
       (when odd (setf slow (cdr slow)))
       (setf odd (not odd))
       (unless (eq slow fast) (go start)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:14398 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'list-length',[list],[let,[[slow,list],[fast,list],[odd,[]],[len,0]],[tagbody,start,[when,[atom,fast],['return-from','list-length',len]],[setf,fast,[cdr,fast]],[setf,len,[+,1,len]],[when,odd,[setf,slow,[cdr,slow]]],[setf,odd,[not,odd]],[unless,[eq,slow,fast],[go,start]]]]])
wl:lambda_def(defun, list_length, f_list_length, [list], [[let, [[sys_slow, list], [sys_fast, list], [sys_odd, []], [sys_len, 0]], [tagbody, sys_start, [when, [atom, sys_fast], [return_from, list_length, sys_len]], [setf, sys_fast, [cdr, sys_fast]], [setf, sys_len, [+, 1, sys_len]], [when, sys_odd, [setf, sys_slow, [cdr, sys_slow]]], [setf, sys_odd, [not, sys_odd]], [unless, [eq, sys_slow, sys_fast], [go, sys_start]]]]]).
wl:arglist_info(list_length, f_list_length, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_list_length).

/*

### Compiled Function: `CL:LIST-LENGTH` 
*/
f_list_length(List_In, FnResult) :-
	GEnv=[bv(list, List_In)],
	catch(( ( get_var(GEnv, list, List_Get9),
		  BlockExitEnv=[bv(sys_slow, List_Get9), bv(sys_fast, List_Get9), bv(sys_odd, []), bv(sys_len, 0)|GEnv],
		  call_addr_block(BlockExitEnv,
				  (push_label(sys_start), get_var(BlockExitEnv, sys_fast, Fast_Get41), (Fast_Get41\=[CAR|CDR]->get_var(BlockExitEnv, sys_len, RetResult44), throw(block_exit(list_length, RetResult44)), _8872=ThrowResult45;_8872=[]), get_var(BlockExitEnv, sys_fast, Fast_Get49), f_cdr(Fast_Get49, Fast), set_var(BlockExitEnv, sys_fast, Fast), get_var(BlockExitEnv, sys_len, Len_Get50), 'f_+'(1, Len_Get50, Len), set_var(BlockExitEnv, sys_len, Len), get_var(BlockExitEnv, sys_odd, IFTEST51), (IFTEST51\==[]->get_var(BlockExitEnv, sys_slow, Slow_Get54), f_cdr(Slow_Get54, TrueResult55), set_var(BlockExitEnv, sys_slow, TrueResult55), _9148=TrueResult55;_9148=[]), get_var(BlockExitEnv, sys_odd, Odd_Get56), f_not(Odd_Get56, Odd), set_var(BlockExitEnv, sys_odd, Odd), get_var(BlockExitEnv, sys_fast, Fast_Get59), get_var(BlockExitEnv, sys_slow, Slow_Get58), (is_eq(Slow_Get58, Fast_Get59)->_TBResult=[];goto(sys_start, BlockExitEnv), _TBResult=_GORES63)),
				  
				  [ addr(addr_tagbody_38_sys_start,
					 sys_start,
					 '$unused',
					 BlockExitEnv,
					 (get_var(BlockExitEnv, sys_fast, Fast_Get), (Fast_Get\=[CAR75|CDR76]->get_var(BlockExitEnv, sys_len, Get_var_Ret), throw(block_exit(list_length, Get_var_Ret)), _9524=ThrowResult;_9524=[]), get_var(BlockExitEnv, sys_fast, Fast_Get22), f_cdr(Fast_Get22, Cdr_Ret), set_var(BlockExitEnv, sys_fast, Cdr_Ret), get_var(BlockExitEnv, sys_len, Len_Get23), 'f_+'(1, Len_Get23, Set_var_Ret), set_var(BlockExitEnv, sys_len, Set_var_Ret), get_var(BlockExitEnv, sys_odd, IFTEST24), (IFTEST24\==[]->get_var(BlockExitEnv, sys_slow, Cdr_Param), f_cdr(Cdr_Param, TrueResult28), set_var(BlockExitEnv, sys_slow, TrueResult28), _9602=TrueResult28;_9602=[]), get_var(BlockExitEnv, sys_odd, Odd_Get29), f_not(Odd_Get29, Not_Ret), set_var(BlockExitEnv, sys_odd, Not_Ret), get_var(BlockExitEnv, sys_fast, Fast_Get32), get_var(BlockExitEnv, sys_slow, Slow_Get31), (is_eq(Slow_Get31, Fast_Get32)->_9648=[];goto(sys_start, BlockExitEnv), _9648=_GORES)))
				  ])
		),
		[]=FnResult
	      ),
	      block_exit(list_length, FnResult),
	      true).
:- set_opv(list_length, symbol_function, f_list_length),
   DefunResult=list_length.
/*
:- side_effect(assert_lsp(list_length,
			  lambda_def(defun,
				     list_length,
				     f_list_length,
				     [list],
				     
				     [ 
				       [ let,
					 
					 [ [sys_slow, list],
					   [sys_fast, list],
					   [sys_odd, []],
					   [sys_len, 0]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ when,
					     [atom, sys_fast],
					     [return_from, list_length, sys_len]
					   ],
					   [setf, sys_fast, [cdr, sys_fast]],
					   [setf, sys_len, [+, 1, sys_len]],
					   
					   [ when,
					     sys_odd,
					     [setf, sys_slow, [cdr, sys_slow]]
					   ],
					   [setf, sys_odd, [not, sys_odd]],
					   
					   [ unless,
					     [eq, sys_slow, sys_fast],
					     [go, sys_start]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(list_length,
			  arglist_info(list_length,
				       f_list_length,
				       [list],
				       arginfo{ all:[list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[list],
						opt:0,
						req:[list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(list_length, init_args(x, f_list_length))).
*/
/*
#+(or WAM-CL LISP500) 
(defun listp (object) (or (consp object) (eq object nil)))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:14769 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,listp,[object],[or,[consp,object],[eq,object,[]]]])
wl:lambda_def(defun, listp, f_listp, [sys_object], [[or, [consp, sys_object], [eq, sys_object, []]]]).
wl:arglist_info(listp, f_listp, [sys_object], arginfo{all:[sys_object], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object], opt:0, req:[sys_object], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_listp).

/*

### Compiled Function: `CL:LISTP` 
*/
f_listp(Object_In, FnResult) :-
	GEnv=[bv(sys_object, Object_In)],
	catch(( (   get_var(GEnv, sys_object, Object_Get),
		    f_consp(Object_Get, FORM1_Res),
		    FORM1_Res\==[],
		    _6784=FORM1_Res
		->  true
		;   get_var(GEnv, sys_object, Object_Get6),
		    f_eq(Object_Get6, [], Eq_Ret),
		    _6784=Eq_Ret
		),
		_6784=FnResult
	      ),
	      block_exit(listp, FnResult),
	      true).
:- set_opv(listp, symbol_function, f_listp),
   DefunResult=listp.
/*
:- side_effect(assert_lsp(listp,
			  lambda_def(defun,
				     listp,
				     f_listp,
				     [sys_object],
				     
				     [ 
				       [ or,
					 [consp, sys_object],
					 [eq, sys_object, []]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(listp,
			  arglist_info(listp,
				       f_listp,
				       [sys_object],
				       arginfo{ all:[sys_object],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_object],
						opt:0,
						req:[sys_object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(listp, init_args(x, f_listp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun nth (n list) (if (< n 1) (car list) (nth (- n 1) (cdr list))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:14859 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nth,[n,list],[if,[<,n,1],[car,list],[nth,[-,n,1],[cdr,list]]]])
wl:lambda_def(defun, nth, f_nth, [sys_n, list], [[if, [<, sys_n, 1], [car, list], [nth, [-, sys_n, 1], [cdr, list]]]]).
wl:arglist_info(nth, f_nth, [sys_n, list], arginfo{all:[sys_n, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_n, list], opt:0, req:[sys_n, list], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_nth).

/*

### Compiled Function: `CL:NTH` 
*/
f_nth(N_In, List_In, FnResult) :-
	GEnv=[bv(sys_n, N_In), bv(list, List_In)],
	catch(( ( get_var(GEnv, sys_n, N_Get),
		  (   N_Get<1
		  ->  get_var(GEnv, list, List_Get),
		      f_car(List_Get, TrueResult),
		      _7036=TrueResult
		  ;   get_var(GEnv, sys_n, N_Get11),
		      'f_-'(N_Get11, 1, Nth_Param),
		      get_var(GEnv, list, List_Get12),
		      f_cdr(List_Get12, Cdr_Ret),
		      f_nth(Nth_Param, Cdr_Ret, ElseResult),
		      _7036=ElseResult
		  )
		),
		_7036=FnResult
	      ),
	      block_exit(nth, FnResult),
	      true).
:- set_opv(nth, symbol_function, f_nth),
   DefunResult=nth.
/*
:- side_effect(assert_lsp(nth,
			  lambda_def(defun,
				     nth,
				     f_nth,
				     [sys_n, list],
				     
				     [ 
				       [ if,
					 [<, sys_n, 1],
					 [car, list],
					 [nth, [-, sys_n, 1], [cdr, list]]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nth,
			  arglist_info(nth,
				       f_nth,
				       [sys_n, list],
				       arginfo{ all:[sys_n, list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_n, list],
						opt:0,
						req:[sys_n, list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nth, init_args(x, f_nth))).
*/
/*
'(defun (setf nth) (new-object n list)
  (if (< n 1)
      (setf (car list) new-object)
      (setf (nth (- n 1) (cdr list)) new-object)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:14954 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[defun,[setf,nth],['new-object',n,list],[if,[<,n,1],[setf,[car,list],'new-object'],[setf,[nth,[-,n,1],[cdr,list]],'new-object']]]])
/*
#+(or WAM-CL LISP500) 
(defun endp (list) (not list))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:15101 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,endp,[list],[not,list]])
wl:lambda_def(defun, endp, f_endp, [list], [[not, list]]).
wl:arglist_info(endp, f_endp, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_endp).

/*

### Compiled Function: `CL:ENDP` 
*/
f_endp(List_In, FnResult) :-
	GEnv=[bv(list, List_In)],
	catch(( ( get_var(GEnv, list, List_Get),
		  f_not(List_Get, Not_Ret)
		),
		Not_Ret=FnResult
	      ),
	      block_exit(endp, FnResult),
	      true).
:- set_opv(endp, symbol_function, f_endp),
   DefunResult=endp.
/*
:- side_effect(assert_lsp(endp,
			  lambda_def(defun, endp, f_endp, [list], [[not, list]]))).
*/
/*
:- side_effect(assert_lsp(endp,
			  arglist_info(endp,
				       f_endp,
				       [list],
				       arginfo{ all:[list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[list],
						opt:0,
						req:[list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(endp, init_args(x, f_endp))).
*/
/*
#+(or WAM-CL LISP500) 
(defun nconc (&rest lists)
  (if (cdr lists)
      (if (car lists)
	  (progn (setf (cdr (last (car lists))) (apply #'nconc (cdr lists)))
		 (car lists))
	  (apply #'nconc (cdr lists)))
      (car lists)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:15159 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nconc,['&rest',lists],[if,[cdr,lists],[if,[car,lists],[progn,[setf,[cdr,[last,[car,lists]]],[apply,function(nconc),[cdr,lists]]],[car,lists]],[apply,function(nconc),[cdr,lists]]],[car,lists]]])
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _8672, [], [], true), append([[last, [car, sys_lists]]], [CAR12, CAR], [[last, [car, sys_lists]], CAR12, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _8626, [], [], true), append([[last, [car, sys_lists]]], [CAR12, CAR], [[last, [car, sys_lists]], CAR12, CAR]), setf_inverse_op(cdr, rplacd))).
*/
wl:lambda_def(defun, nconc, f_nconc, [c38_rest, sys_lists], [[if, [cdr, sys_lists], [if, [car, sys_lists], [progn, [setf, [cdr, [last, [car, sys_lists]]], [apply, function(nconc), [cdr, sys_lists]]], [car, sys_lists]], [apply, function(nconc), [cdr, sys_lists]]], [car, sys_lists]]]).
wl:arglist_info(nconc, f_nconc, [c38_rest, sys_lists], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_lists], opt:0, req:0, rest:[sys_lists], sublists:0, whole:0}).
wl: init_args(0, f_nconc).

/*

### Compiled Function: `CL:NCONC` 
*/
f_nconc(RestNKeys, FnResult) :-
	GEnv=[bv(sys_lists, RestNKeys)],
	catch(( ( get_var(GEnv, sys_lists, Lists_Get),
		  f_cdr(Lists_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_lists, Lists_Get10),
		      f_car(Lists_Get10, IFTEST8),
		      (   IFTEST8\==[]
		      ->  get_var(GEnv, sys_lists, Lists_Get13),
			  f_car(Lists_Get13, Last_Param),
			  f_last(Last_Param, [], Rplacd_Param),
			  get_var(GEnv, sys_lists, Lists_Get14),
			  f_cdr(Lists_Get14, Nconc_Param),
			  f_nconc(Nconc_Param, Nconc_Ret),
			  f_rplacd(Rplacd_Param, Nconc_Ret, Rplacd_Ret),
			  get_var(GEnv, sys_lists, Lists_Get15),
			  f_car(Lists_Get15, TrueResult),
			  TrueResult20=TrueResult
		      ;   get_var(GEnv, sys_lists, Lists_Get16),
			  f_cdr(Lists_Get16, Nconc_Param28),
			  f_nconc(Nconc_Param28, ElseResult),
			  TrueResult20=ElseResult
		      ),
		      _7604=TrueResult20
		  ;   get_var(GEnv, sys_lists, Lists_Get19),
		      f_car(Lists_Get19, ElseResult21),
		      _7604=ElseResult21
		  )
		),
		_7604=FnResult
	      ),
	      block_exit(nconc, FnResult),
	      true).
:- set_opv(nconc, symbol_function, f_nconc),
   DefunResult=nconc.
/*
:- side_effect(assert_lsp(nconc,
			  lambda_def(defun,
				     nconc,
				     f_nconc,
				     [c38_rest, sys_lists],
				     
				     [ 
				       [ if,
					 [cdr, sys_lists],
					 
					 [ if,
					   [car, sys_lists],
					   
					   [ progn,
					     
					     [ setf,
					       [cdr, [last, [car, sys_lists]]],
					       
					       [ apply,
						 function(nconc),
						 [cdr, sys_lists]
					       ]
					     ],
					     [car, sys_lists]
					   ],
					   
					   [ apply,
					     function(nconc),
					     [cdr, sys_lists]
					   ]
					 ],
					 [car, sys_lists]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nconc,
			  arglist_info(nconc,
				       f_nconc,
				       [c38_rest, sys_lists],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_lists],
						opt:0,
						req:0,
						rest:[sys_lists],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nconc, init_args(0, f_nconc))).
*/
/*
#+(or WAM-CL LISP500) 
(defun revappend (list tail)
  (if list
      (revappend (cdr list) (cons (car list) tail))
      tail))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:15397 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,revappend,[list,tail],[if,list,[revappend,[cdr,list],[cons,[car,list],tail]],tail]])
wl:lambda_def(defun, revappend, f_revappend, [list, sys_tail], [[if, list, [revappend, [cdr, list], [cons, [car, list], sys_tail]], sys_tail]]).
wl:arglist_info(revappend, f_revappend, [list, sys_tail], arginfo{all:[list, sys_tail], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list, sys_tail], opt:0, req:[list, sys_tail], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_revappend).

/*

### Compiled Function: `CL:REVAPPEND` 
*/
f_revappend(List_In, Tail_In, FnResult) :-
	GEnv=[bv(list, List_In), bv(sys_tail, Tail_In)],
	catch(( ( get_var(GEnv, list, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, list, List_Get9),
		      f_cdr(List_Get9, Revappend_Param),
		      get_var(GEnv, list, List_Get10),
		      f_car(List_Get10, Car_Ret),
		      get_var(GEnv, sys_tail, Tail_Get),
		      _7062=[Car_Ret|Tail_Get],
		      f_revappend(Revappend_Param, _7062, TrueResult),
		      _6974=TrueResult
		  ;   get_var(GEnv, sys_tail, Tail_Get12),
		      _6974=Tail_Get12
		  )
		),
		_6974=FnResult
	      ),
	      block_exit(revappend, FnResult),
	      true).
:- set_opv(revappend, symbol_function, f_revappend),
   DefunResult=revappend.
/*
:- side_effect(assert_lsp(revappend,
			  lambda_def(defun,
				     revappend,
				     f_revappend,
				     [list, sys_tail],
				     
				     [ 
				       [ if,
					 list,
					 
					 [ revappend,
					   [cdr, list],
					   [cons, [car, list], sys_tail]
					 ],
					 sys_tail
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(revappend,
			  arglist_info(revappend,
				       f_revappend,
				       [list, sys_tail],
				       arginfo{ all:[list, sys_tail],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[list, sys_tail],
						opt:0,
						req:[list, sys_tail],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(revappend, init_args(x, f_revappend))).
*/
/*
#+(or WAM-CL LISP500) 
(defun nreconc (list tail)
  (if list
      (let ((new-list (cdr list)))
	(setf (cdr list) tail)
	(nreconc new-list list))
      tail))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:15532 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nreconc,[list,tail],[if,list,[let,[['new-list',[cdr,list]]],[setf,[cdr,list],tail],[nreconc,'new-list',list]],tail]])
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv, [], [], true), append([list], [CAR15, CAR], [list, CAR15, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv, [], [], true), append([list], [CAR15, CAR], [list, CAR15, CAR]), setf_inverse_op(cdr, rplacd))).
*/
wl:lambda_def(defun, nreconc, f_nreconc, [list, sys_tail], [[if, list, [let, [[sys_new_list, [cdr, list]]], [setf, [cdr, list], sys_tail], [nreconc, sys_new_list, list]], sys_tail]]).
wl:arglist_info(nreconc, f_nreconc, [list, sys_tail], arginfo{all:[list, sys_tail], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list, sys_tail], opt:0, req:[list, sys_tail], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_nreconc).

/*

### Compiled Function: `CL:NRECONC` 
*/
f_nreconc(List_In, Tail_In, FnResult) :-
	GEnv=[bv(list, List_In), bv(sys_tail, Tail_In)],
	catch(( ( get_var(GEnv, list, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, list, List_Get12),
		      f_cdr(List_Get12, New_list_Init),
		      LEnv=[bv(sys_new_list, New_list_Init)|GEnv],
		      get_var(LEnv, list, List_Get16),
		      get_var(LEnv, sys_tail, Tail_Get),
		      f_rplacd(List_Get16, Tail_Get, Rplacd_Ret),
		      get_var(LEnv, list, List_Get19),
		      get_var(LEnv, sys_new_list, New_list_Get),
		      f_nreconc(New_list_Get, List_Get19, LetResult),
		      _7152=LetResult
		  ;   get_var(GEnv, sys_tail, Tail_Get20),
		      _7152=Tail_Get20
		  )
		),
		_7152=FnResult
	      ),
	      block_exit(nreconc, FnResult),
	      true).
:- set_opv(nreconc, symbol_function, f_nreconc),
   DefunResult=nreconc.
/*
:- side_effect(assert_lsp(nreconc,
			  lambda_def(defun,
				     nreconc,
				     f_nreconc,
				     [list, sys_tail],
				     
				     [ 
				       [ if,
					 list,
					 
					 [ let,
					   [[sys_new_list, [cdr, list]]],
					   [setf, [cdr, list], sys_tail],
					   [nreconc, sys_new_list, list]
					 ],
					 sys_tail
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nreconc,
			  arglist_info(nreconc,
				       f_nreconc,
				       [list, sys_tail],
				       arginfo{ all:[list, sys_tail],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[list, sys_tail],
						opt:0,
						req:[list, sys_tail],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nreconc, init_args(x, f_nreconc))).
*/
/*
#+(or WAM-CL LISP500) 
(defun butlast (list &optional (n 1))
  (let* ((r (cons nil nil))
	 (e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e r)
    (tagbody
     start
       (unless (consp list) (return-from butlast nil))
       (unless (< n 1)
	 (setf e (setf (cdr e) (cons (car list) nil)))
	 (setf list (cdr list))
	 (setf n (- n 1))
	 (go start)))
    (cdr r)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:15700 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,butlast,[list,'&optional',[n,1]],['let*',[[r,[cons,[],[]]],[e,list],[m,0]],[tagbody,start,[when,[consp,e],[setf,m,[+,m,1]],[setf,e,[cdr,e]],[go,start]]],[setf,n,[-,m,n]],[setf,e,r],[tagbody,start,[unless,[consp,list],['return-from',butlast,[]]],[unless,[<,n,1],[setf,e,[setf,[cdr,e],[cons,[car,list],[]]]],[setf,list,[cdr,list]],[setf,n,[-,n,1]],[go,start]]],[cdr,r]]])
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], BlockExitEnv, [], [], true), append([sys_e], [CAR56, CAR], [sys_e, CAR56, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], BlockExitEnv, [], [], true), append([sys_e], [CAR56, CAR], [sys_e, CAR56, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], GoEnv, [], [], true), append([sys_e], [CAR78, CAR77], [sys_e, CAR78, CAR77]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], GoEnv, [], [], true), append([sys_e], [CAR78, CAR77], [sys_e, CAR78, CAR77]), setf_inverse_op(cdr, rplacd))).
*/
wl:lambda_def(defun,butlast,f_butlast,[list,c38_optional,[sys_n,1]],[[let_xx,[[sys_r,[cons,[],[]]],[sys_e,list],[sys_m,0]],[tagbody,sys_start,[when,[consp,sys_e],[setf,sys_m,[+,sys_m,1]],[setf,sys_e,[cdr,sys_e]],[go,sys_start]]],[setf,sys_n,[-,sys_m,sys_n]],[setf,sys_e,sys_r],[tagbody,sys_start,[unless,[consp,list],[return_from,butlast,[]]],[unless,[<,sys_n,1],[setf,sys_e,[setf,[cdr,sys_e],[cons,[car,list],[]]]],[setf,list,[cdr,list]],[setf,sys_n,[-,sys_n,1]],[go,sys_start]]],[cdr,sys_r]]]).
wl:arglist_info(butlast,f_butlast,[list,c38_optional,[sys_n,1]],arginfo{all:[list,sys_n],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_n],opt:[sys_n],req:[list],rest:0,sublists:0,whole:0}).
wl:init_args(1,f_butlast).

/*

### Compiled Function: `CL:BUTLAST` 
*/
f_butlast(_9170,_9188,_11212):-_9130=[bv(list,_9170),bv(sys_n,_9172)],opt_var(_9122,sys_n,_9172,true,1,1,_9188),catch(((_9266=[[]],_9228=[bv(sys_r,_9266)|_9130],get_var(_9228,list,_9398),_9356=[bv(sys_e,_9398)|_9228],_9482=[bv(sys_m,0)|_9356],call_addr_block(_9482,(push_label(sys_start),get_var(_9482,sys_e,_9752),(c0nz:is_consp(_9752)->get_var(_9482,sys_m,_9810),'f_+'(_9810,1,_9792),set_var(_9482,sys_m,_9792),get_var(_9482,sys_e,_9840),f_cdr(_9840,_9822),set_var(_9482,sys_e,_9822),goto(sys_start,_9482),_9494=_9868;_9494=[])),[addr(addr_tagbody_39_sys_start,sys_start,'$unused',_9908,(get_var(_9908,sys_e,_9912),(c0nz:is_consp(_9912)->get_var(_9908,sys_m,_9924),'f_+'(_9924,1,_9926),set_var(_9908,sys_m,_9926),get_var(_9908,sys_e,_9930),f_cdr(_9930,_9942),set_var(_9908,sys_e,_9942),goto(sys_start,_9908),_9944=_9948;_9944=[])))]),get_var(_9482,sys_m,_9978),get_var(_9482,sys_n,_9990),'f_-'(_9978,_9990,_9960),set_var(_9482,sys_n,_9960),get_var(_9482,sys_r,_10006),set_var(_9482,sys_e,_10006),call_addr_block(_9482,(push_label(sys_start),get_var(_9482,list,_10586),(c0nz:is_consp(_10586)->_10540=[];throw(block_exit(butlast,[])),_10540=_10670),get_var(_9482,sys_n,_10754),(_10754<1->_10038=[];get_var(_9482,list,_10902),get_var(_9482,sys_e,_10868),f_car(_10902,_10884),_10882=[_10884],f_rplacd(_10868,_10882,_15580),set_var(_9482,sys_e,_15580),get_var(_9482,list,_10932),f_cdr(_10932,_10914),set_var(_9482,list,_10914),get_var(_9482,sys_n,_10962),'f_-'(_10962,1,_15618),set_var(_9482,sys_n,_15618),goto(sys_start,_9482),_10038=_10990)),[addr(addr_tagbody_40_sys_start,sys_start,'$unused',_11018,(get_var(_11018,list,_11032),(c0nz:is_consp(_11032)->_11044=[];throw(block_exit(butlast,[])),_11044=_11048),get_var(_11018,sys_n,_11062),(_11062<1->_11076=[];get_var(_11018,list,_11090),get_var(_11018,sys_e,_11104),f_car(_11090,_15704),_11118=[_15704],f_rplacd(_11104,_11118,_11120),set_var(_11018,sys_e,_11120),get_var(_11018,list,_11124),f_cdr(_11124,_15742),set_var(_11018,list,_15742),get_var(_11018,sys_n,_11140),'f_-'(_11140,1,_15768),set_var(_11018,sys_n,_15768),goto(sys_start,_11018),_11076=_11156)))]),get_var(_9482,sys_r,_11184),f_cdr(_11184,_9330)),_9330=_11212),block_exit(butlast,_11212),true).
:-set_opv(butlast,symbol_function,f_butlast),_7798=butlast.
/*
:-side_effect(assert_lsp(butlast,lambda_def(defun,butlast,f_butlast,[list,c38_optional,[sys_n,1]],[[let_xx,[[sys_r,[cons,[],[]]],[sys_e,list],[sys_m,0]],[tagbody,sys_start,[when,[consp,sys_e],[setf,sys_m,[+,sys_m,1]],[setf,sys_e,[cdr,sys_e]],[go,sys_start]]],[setf,sys_n,[-,sys_m,sys_n]],[setf,sys_e,sys_r],[tagbody,sys_start,[unless,[consp,list],[return_from,butlast,[]]],[unless,[<,sys_n,1],[setf,sys_e,[setf,[cdr,sys_e],[cons,[car,list],[]]]],[setf,list,[cdr,list]],[setf,sys_n,[-,sys_n,1]],[go,sys_start]]],[cdr,sys_r]]]))).
*/
/*
:-side_effect(assert_lsp(butlast,arglist_info(butlast,f_butlast,[list,c38_optional,[sys_n,1]],arginfo{all:[list,sys_n],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_n],opt:[sys_n],req:[list],rest:0,sublists:0,whole:0}))).
*/
/*
:-side_effect(assert_lsp(butlast,init_args(1,f_butlast))).
*/
/*
#+(or WAM-CL LISP500) 
(defun nbutlast (list &optional (n 1))
  (let* ((e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e list)
    (tagbody
     start
       (unless (consp list) (return-from nbutlast nil))
       (unless (< n 2)
	 (setf e (cdr e))
	 (setf n (- n 1))
	 (go start)))
    (setf (cdr e) nil)
    list))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:16192 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nbutlast,[list,'&optional',[n,1]],['let*',[[e,list],[m,0]],[tagbody,start,[when,[consp,e],[setf,m,[+,m,1]],[setf,e,[cdr,e]],[go,start]]],[setf,n,[-,m,n]],[setf,e,list],[tagbody,start,[unless,[consp,list],['return-from',nbutlast,[]]],[unless,[<,n,2],[setf,e,[cdr,e]],[setf,n,[-,n,1]],[go,start]]],[setf,[cdr,e],[]],list]])
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], GoEnv, [], [], true), append([sys_e], [CAR74, CAR], [sys_e, CAR74, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], GoEnv, [], [], true), append([sys_e], [CAR74, CAR], [sys_e, CAR74, CAR]), setf_inverse_op(cdr, rplacd))).
*/
wl:lambda_def(defun,nbutlast,f_nbutlast,[list,c38_optional,[sys_n,1]],[[let_xx,[[sys_e,list],[sys_m,0]],[tagbody,sys_start,[when,[consp,sys_e],[setf,sys_m,[+,sys_m,1]],[setf,sys_e,[cdr,sys_e]],[go,sys_start]]],[setf,sys_n,[-,sys_m,sys_n]],[setf,sys_e,list],[tagbody,sys_start,[unless,[consp,list],[return_from,nbutlast,[]]],[unless,[<,sys_n,2],[setf,sys_e,[cdr,sys_e]],[setf,sys_n,[-,sys_n,1]],[go,sys_start]]],[setf,[cdr,sys_e],[]],list]]).
wl:arglist_info(nbutlast,f_nbutlast,[list,c38_optional,[sys_n,1]],arginfo{all:[list,sys_n],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_n],opt:[sys_n],req:[list],rest:0,sublists:0,whole:0}).
wl:init_args(1,f_nbutlast).

/*

### Compiled Function: `CL:NBUTLAST` 
*/
f_nbutlast(_8774,_8792,_10550):-_8734=[bv(list,_8774),bv(sys_n,_8776)],opt_var(_8726,sys_n,_8776,true,1,1,_8792),catch(((get_var(_8734,list,_8904),_8832=[bv(sys_e,_8904)|_8734],_8988=[bv(sys_m,0)|_8832],call_addr_block(_8988,(push_label(sys_start),get_var(_8988,sys_e,_9258),(c0nz:is_consp(_9258)->get_var(_8988,sys_m,_9316),'f_+'(_9316,1,_9298),set_var(_8988,sys_m,_9298),get_var(_8988,sys_e,_9346),f_cdr(_9346,_9328),set_var(_8988,sys_e,_9328),goto(sys_start,_8988),_9000=_9374;_9000=[])),[addr(addr_tagbody_41_sys_start,sys_start,'$unused',_9414,(get_var(_9414,sys_e,_9418),(c0nz:is_consp(_9418)->get_var(_9414,sys_m,_9430),'f_+'(_9430,1,_9432),set_var(_9414,sys_m,_9432),get_var(_9414,sys_e,_9436),f_cdr(_9436,_9448),set_var(_9414,sys_e,_9448),goto(sys_start,_9414),_9450=_9454;_9450=[])))]),get_var(_8988,sys_m,_9484),get_var(_8988,sys_n,_9496),'f_-'(_9484,_9496,_9466),set_var(_8988,sys_n,_9466),get_var(_8988,list,_9528),set_var(_8988,sys_e,_9528),call_addr_block(_8988,(push_label(sys_start),get_var(_8988,list,_10004),(c0nz:is_consp(_10004)->_9958=[];throw(block_exit(nbutlast,[])),_9958=_10088),get_var(_8988,sys_n,_10172),(_10172<2->_9556=[];get_var(_8988,sys_e,_10230),f_cdr(_10230,_14342),set_var(_8988,sys_e,_14342),get_var(_8988,sys_n,_10260),'f_-'(_10260,1,_14368),set_var(_8988,sys_n,_14368),goto(sys_start,_8988),_9556=_10288)),[addr(addr_tagbody_42_sys_start,sys_start,'$unused',_10316,(get_var(_10316,list,_10330),(c0nz:is_consp(_10330)->_10342=[];throw(block_exit(nbutlast,[])),_10342=_10346),get_var(_10316,sys_n,_10360),(_10360<2->_10374=[];get_var(_10316,sys_e,_10388),f_cdr(_10388,_14430),set_var(_10316,sys_e,_14430),get_var(_10316,sys_n,_10404),'f_-'(_10404,1,_14456),set_var(_10316,sys_n,_14456),goto(sys_start,_10316),_10374=_10420)))]),get_var(_8988,sys_e,_10492),f_rplacd(_10492,[],_10432),get_var(_8988,list,_8962)),_8962=_10550),block_exit(nbutlast,_10550),true).
:-set_opv(nbutlast,symbol_function,f_nbutlast),_7594=nbutlast.
/*
:-side_effect(assert_lsp(nbutlast,lambda_def(defun,nbutlast,f_nbutlast,[list,c38_optional,[sys_n,1]],[[let_xx,[[sys_e,list],[sys_m,0]],[tagbody,sys_start,[when,[consp,sys_e],[setf,sys_m,[+,sys_m,1]],[setf,sys_e,[cdr,sys_e]],[go,sys_start]]],[setf,sys_n,[-,sys_m,sys_n]],[setf,sys_e,list],[tagbody,sys_start,[unless,[consp,list],[return_from,nbutlast,[]]],[unless,[<,sys_n,2],[setf,sys_e,[cdr,sys_e]],[setf,sys_n,[-,sys_n,1]],[go,sys_start]]],[setf,[cdr,sys_e],[]],list]]))).
*/
/*
:-side_effect(assert_lsp(nbutlast,arglist_info(nbutlast,f_nbutlast,[list,c38_optional,[sys_n,1]],arginfo{all:[list,sys_n],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_n],opt:[sys_n],req:[list],rest:0,sublists:0,whole:0}))).
*/
/*
:-side_effect(assert_lsp(nbutlast,init_args(1,f_nbutlast))).
*/
/*
#+(or WAM-CL LISP500) 
(defun last (list &optional (n 1))
  (let* ((e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e list)
    (tagbody
     start
       (when (< n 1) (return-from last e))
       (setf e (cdr e))
       (setf n (- n 1))
       (go start))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:16633 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,last,[list,'&optional',[n,1]],['let*',[[e,list],[m,0]],[tagbody,start,[when,[consp,e],[setf,m,[+,m,1]],[setf,e,[cdr,e]],[go,start]]],[setf,n,[-,m,n]],[setf,e,list],[tagbody,start,[when,[<,n,1],['return-from',last,e]],[setf,e,[cdr,e]],[setf,n,[-,n,1]],[go,start]]]])
wl:lambda_def(defun,last,f_last,[list,c38_optional,[sys_n,1]],[[let_xx,[[sys_e,list],[sys_m,0]],[tagbody,sys_start,[when,[consp,sys_e],[setf,sys_m,[+,sys_m,1]],[setf,sys_e,[cdr,sys_e]],[go,sys_start]]],[setf,sys_n,[-,sys_m,sys_n]],[setf,sys_e,list],[tagbody,sys_start,[when,[<,sys_n,1],[return_from,last,sys_e]],[setf,sys_e,[cdr,sys_e]],[setf,sys_n,[-,sys_n,1]],[go,sys_start]]]]).
wl:arglist_info(last,f_last,[list,c38_optional,[sys_n,1]],arginfo{all:[list,sys_n],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_n],opt:[sys_n],req:[list],rest:0,sublists:0,whole:0}).
wl:init_args(1,f_last).

/*

### Compiled Function: `CL:LAST` 
*/
f_last(_8470,_8488,_9980):-_8430=[bv(list,_8470),bv(sys_n,_8472)],opt_var(_8422,sys_n,_8472,true,1,1,_8488),catch(((get_var(_8430,list,_8600),_8528=[bv(sys_e,_8600)|_8430],_8684=[bv(sys_m,0)|_8528],call_addr_block(_8684,(push_label(sys_start),get_var(_8684,sys_e,_8954),(c0nz:is_consp(_8954)->get_var(_8684,sys_m,_9012),'f_+'(_9012,1,_8994),set_var(_8684,sys_m,_8994),get_var(_8684,sys_e,_9042),f_cdr(_9042,_9024),set_var(_8684,sys_e,_9024),goto(sys_start,_8684),_8696=_9070;_8696=[])),[addr(addr_tagbody_43_sys_start,sys_start,'$unused',_9110,(get_var(_9110,sys_e,_9114),(c0nz:is_consp(_9114)->get_var(_9110,sys_m,_9126),'f_+'(_9126,1,_9128),set_var(_9110,sys_m,_9128),get_var(_9110,sys_e,_9132),f_cdr(_9132,_9144),set_var(_9110,sys_e,_9144),goto(sys_start,_9110),_9146=_9150;_9146=[])))]),get_var(_8684,sys_m,_9180),get_var(_8684,sys_n,_9192),'f_-'(_9180,_9192,_9162),set_var(_8684,sys_n,_9162),get_var(_8684,list,_9224),set_var(_8684,sys_e,_9224),call_addr_block(_8684,(push_label(sys_start),get_var(_8684,sys_n,_9632),(_9632<1->get_var(_8684,sys_e,_9688),throw(block_exit(last,_9688)),_9586=_9716;_9586=[]),get_var(_8684,sys_e,_9802),f_cdr(_9802,_13384),set_var(_8684,sys_e,_13384),get_var(_8684,sys_n,_9832),'f_-'(_9832,1,_13410),set_var(_8684,sys_n,_13410),goto(sys_start,_8684)),[addr(addr_tagbody_44_sys_start,sys_start,'$used',_9876,(get_var(_9876,sys_n,_9890),(_9890<1->get_var(_9876,sys_e,_9904),throw(block_exit(last,_9904)),_9916=_9920;_9916=[]),get_var(_9876,sys_e,_9934),f_cdr(_9934,_13472),set_var(_9876,sys_e,_13472),get_var(_9876,sys_n,_9950),'f_-'(_9950,1,_13498),set_var(_9876,sys_n,_13498),goto(sys_start,_9876)))])),[]=_9980),block_exit(last,_9980),true).
:-set_opv(last,symbol_function,f_last),_7434=last.
/*
:-side_effect(assert_lsp(last,lambda_def(defun,last,f_last,[list,c38_optional,[sys_n,1]],[[let_xx,[[sys_e,list],[sys_m,0]],[tagbody,sys_start,[when,[consp,sys_e],[setf,sys_m,[+,sys_m,1]],[setf,sys_e,[cdr,sys_e]],[go,sys_start]]],[setf,sys_n,[-,sys_m,sys_n]],[setf,sys_e,list],[tagbody,sys_start,[when,[<,sys_n,1],[return_from,last,sys_e]],[setf,sys_e,[cdr,sys_e]],[setf,sys_n,[-,sys_n,1]],[go,sys_start]]]]))).
*/
/*
:-side_effect(assert_lsp(last,arglist_info(last,f_last,[list,c38_optional,[sys_n,1]],arginfo{all:[list,sys_n],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_n],opt:[sys_n],req:[list],rest:0,sublists:0,whole:0}))).
*/
/*
:-side_effect(assert_lsp(last,init_args(1,f_last))).
*/
/*
#+(or WAM-CL LISP500) 
(defun ldiff (list object)
  (let* ((r (cons nil nil))
	 (e r))
    (tagbody
     start
       (unless (or (eq object list) (atom list))
	 (setf e (setf (cdr e) (cons (car list) nil)))
	 (setf list (cdr list))
	 (go start)))
    (cdr r)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:17013 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,ldiff,[list,object],['let*',[[r,[cons,[],[]]],[e,r]],[tagbody,start,[unless,[or,[eq,object,list],[atom,list]],[setf,e,[setf,[cdr,e],[cons,[car,list],[]]]],[setf,list,[cdr,list]],[go,start]]],[cdr,r]]])
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _13438, [], [], true), append([sys_e], [CAR23, CAR], [sys_e, CAR23, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _13416, [], [], true), append([sys_e], [CAR23, CAR], [sys_e, CAR23, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv12, [], [], true), append([sys_e], [CAR38, CAR37], [sys_e, CAR38, CAR37]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv12, [], [], true), append([sys_e], [CAR38, CAR37], [sys_e, CAR38, CAR37]), setf_inverse_op(cdr, rplacd))).
*/
wl:lambda_def(defun,ldiff,f_ldiff,[list,sys_object],[[let_xx,[[sys_r,[cons,[],[]]],[sys_e,sys_r]],[tagbody,sys_start,[unless,[or,[eq,sys_object,list],[atom,list]],[setf,sys_e,[setf,[cdr,sys_e],[cons,[car,list],[]]]],[setf,list,[cdr,list]],[go,sys_start]]],[cdr,sys_r]]]).
wl:arglist_info(ldiff,f_ldiff,[list,sys_object],arginfo{all:[list,sys_object],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_object],opt:0,req:[list,sys_object],rest:0,sublists:0,whole:0}).
wl:init_args(x,f_ldiff).

/*

### Compiled Function: `CL:LDIFF` 
*/
f_ldiff(_7868,_7870,_8912):-_7828=[bv(list,_7868),bv(sys_object,_7870)],catch(((_7938=[[]],_7900=[bv(sys_r,_7938)|_7828],get_var(_7900,sys_r,_8070),_8028=[bv(sys_e,_8070)|_7900],call_addr_block(_8028,(push_label(sys_start),(get_var(_8028,list,_8504),get_var(_8028,sys_object,_8476),f_eq(_8476,_8504,_8560),_8560\==[],_8432=_8560->true;get_var(_8028,list,_8534),f_atom(_8534,_8516),_8432=_8516),(_8432\==[]->_8082=[];get_var(_8028,list,_8680),get_var(_8028,sys_e,_8646),f_car(_8680,_8662),_8660=[_8662],f_rplacd(_8646,_8660,_8572),set_var(_8028,sys_e,_8572),get_var(_8028,list,_8710),f_cdr(_8710,_8692),set_var(_8028,list,_8692),goto(sys_start,_8028),_8082=_8738)),[addr(addr_tagbody_45_sys_start,sys_start,'$unused',_8778,((get_var(_8778,list,_8780),get_var(_8778,sys_object,_8782),f_eq(_8782,_8780,_8784),_8784\==[],_8788=_8784->true;get_var(_8778,list,_8802),f_atom(_8802,_11716),_8788=_11716),(_8788\==[]->_8816=[];get_var(_8778,list,_8820),get_var(_8778,sys_e,_8832),f_car(_8820,_11742),_8836=[_11742],f_rplacd(_8832,_8836,_8838),set_var(_8778,sys_e,_8838),get_var(_8778,list,_8842),f_cdr(_8842,_8854),set_var(_8778,list,_8854),goto(sys_start,_8778),_8816=_8858)))]),get_var(_8028,sys_r,_8886),f_cdr(_8886,_8002)),_8002=_8912),block_exit(ldiff,_8912),true).
:-set_opv(ldiff,symbol_function,f_ldiff),_7120=ldiff.
/*
:-side_effect(assert_lsp(ldiff,lambda_def(defun,ldiff,f_ldiff,[list,sys_object],[[let_xx,[[sys_r,[cons,[],[]]],[sys_e,sys_r]],[tagbody,sys_start,[unless,[or,[eq,sys_object,list],[atom,list]],[setf,sys_e,[setf,[cdr,sys_e],[cons,[car,list],[]]]],[setf,list,[cdr,list]],[go,sys_start]]],[cdr,sys_r]]]))).
*/
/*
:-side_effect(assert_lsp(ldiff,arglist_info(ldiff,f_ldiff,[list,sys_object],arginfo{all:[list,sys_object],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[list,sys_object],opt:0,req:[list,sys_object],rest:0,sublists:0,whole:0}))).
*/
/*
:-side_effect(assert_lsp(ldiff,init_args(x,f_ldiff))).
*/
/*
#+(or WAM-CL LISP500) 
(defun tailp (object list)
  (tagbody
   start
     (when (eq object list) (return-from tailp t))
     (unless (consp list) (return-from tailp nil))
     (setf list (cdr list))
     (go start)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:17288 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,tailp,[object,list],[tagbody,start,[when,[eq,object,list],['return-from',tailp,t]],[unless,[consp,list],['return-from',tailp,[]]],[setf,list,[cdr,list]],[go,start]]])
wl:lambda_def(defun, tailp, f_tailp, [sys_object, list], [[tagbody, sys_start, [when, [eq, sys_object, list], [return_from, tailp, t]], [unless, [consp, list], [return_from, tailp, []]], [setf, list, [cdr, list]], [go, sys_start]]]).
wl:arglist_info(tailp, f_tailp, [sys_object, list], arginfo{all:[sys_object, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object, list], opt:0, req:[sys_object, list], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_tailp).

/*

### Compiled Function: `CL:TAILP` 
*/
f_tailp(Object_In, List_In, FnResult) :-
	BlockExitEnv=[bv(sys_object, Object_In), bv(list, List_In)],
	catch(( call_addr_block(BlockExitEnv,
				(push_label(sys_start), get_var(BlockExitEnv, list, List_Get30), get_var(BlockExitEnv, sys_object, Object_Get29), (is_eq(Object_Get29, List_Get30)->throw(block_exit(tailp, t)), _7826=ThrowResult35;_7826=[]), get_var(BlockExitEnv, list, List_Get39), (c0nz:is_consp(List_Get39)->_8052=[];throw(block_exit(tailp, [])), _8052=ThrowResult43), get_var(BlockExitEnv, list, List_Get46), f_cdr(List_Get46, List), set_var(BlockExitEnv, list, List), goto(sys_start, BlockExitEnv)),
				
				[ addr(addr_tagbody_46_sys_start,
				       sys_start,
				       '$used',
				       BlockExitEnv,
				       (get_var(BlockExitEnv, list, List_Get), get_var(BlockExitEnv, sys_object, Object_Get), (is_eq(Object_Get, List_Get)->throw(block_exit(tailp, t)), _8336=ThrowResult;_8336=[]), get_var(BlockExitEnv, list, List_Get18), (c0nz:is_consp(List_Get18)->_8366=[];throw(block_exit(tailp, [])), _8366=ThrowResult22), get_var(BlockExitEnv, list, List_Get24), f_cdr(List_Get24, Cdr_Ret), set_var(BlockExitEnv, list, Cdr_Ret), goto(sys_start, BlockExitEnv)))
				]),
		[]=FnResult
	      ),
	      block_exit(tailp, FnResult),
	      true).
:- set_opv(tailp, symbol_function, f_tailp),
   DefunResult=tailp.
/*
:- side_effect(assert_lsp(tailp,
			  lambda_def(defun,
				     tailp,
				     f_tailp,
				     [sys_object, list],
				     
				     [ 
				       [ tagbody,
					 sys_start,
					 
					 [ when,
					   [eq, sys_object, list],
					   [return_from, tailp, t]
					 ],
					 
					 [ unless,
					   [consp, list],
					   [return_from, tailp, []]
					 ],
					 [setf, list, [cdr, list]],
					 [go, sys_start]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(tailp,
			  arglist_info(tailp,
				       f_tailp,
				       [sys_object, list],
				       arginfo{ all:[sys_object, list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_object, list],
						opt:0,
						req:[sys_object, list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(tailp, init_args(x, f_tailp))).
*/
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun nthcdr (n list) (if (< n 1) list (nthcdr (- n 1) (cdr list))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:17516 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,nthcdr,[n,list],[if,[<,n,1],list,[nthcdr,[-,n,1],[cdr,list]]]]]]))
/*
#+(or WAM-CL LISP500) 
(defun rest (list) (cdr list))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:17626 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,rest,[list],[cdr,list]])
wl:lambda_def(defun, rest, f_rest, [list], [[cdr, list]]).
wl:arglist_info(rest, f_rest, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_rest).

/*

### Compiled Function: `CL:REST` 
*/
f_rest(List_In, FnResult) :-
	GEnv=[bv(list, List_In)],
	catch(( ( get_var(GEnv, list, List_Get),
		  f_cdr(List_Get, Cdr_Ret)
		),
		Cdr_Ret=FnResult
	      ),
	      block_exit(rest, FnResult),
	      true).
:- set_opv(rest, symbol_function, f_rest),
   DefunResult=rest.
/*
:- side_effect(assert_lsp(rest,
			  lambda_def(defun, rest, f_rest, [list], [[cdr, list]]))).
*/
/*
:- side_effect(assert_lsp(rest,
			  arglist_info(rest,
				       f_rest,
				       [list],
				       arginfo{ all:[list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[list],
						opt:0,
						req:[list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(rest, init_args(x, f_rest))).
*/
/*
#+(or WAM-CL LISP500) 
(labels ((all-end (lists)
	   (dolist (elem lists nil)
	     (unless elem (return-from all-end t))))
	 (all-car (lists)
	   (when lists (cons (caar lists) (all-car (cdr lists)))))
	 (all-cdr (lists)
	   (when lists (cons (cdar lists) (all-cdr (cdr lists))))))
  (defun mapc (function &rest lists)
    (let ((list-1 (car lists)))
      (tagbody
       start
	 (when (all-end lists) (return-from mapc list-1))
	 (apply function (all-car lists))
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun mapcar (function &rest lists)
    (let ((result nil)
	  (end nil))
      (tagbody
       start
	 (when (all-end lists) (return-from mapcar result))
	 (let ((cons (cons (apply function (all-car lists)) nil)))
	   (setf end (if end (setf (cdr end) cons) (setf result cons))))
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun mapl (function &rest lists)
    (let ((list-1 (car lists)))
      (tagbody
       start
	 (when (all-end lists) (return-from mapl list-1))
	 (apply function lists)
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun maplist (function &rest lists)
    (let ((result nil)
	  (end nil))
      (tagbody
       start
	 (when (all-end lists) (return-from maplist result))
	 (let ((cons (cons (apply function lists) nil)))
	   (setf end (if end (setf (cdr end) cons) (setf result cons))))
	 (setf lists (all-cdr lists))
	 (go start)))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:17686 **********************/
:-lisp_compile_to_prolog(pkg_sys,[labels,[['all-end',[lists],[dolist,[elem,lists,[]],[unless,elem,['return-from','all-end',t]]]],['all-car',[lists],[when,lists,[cons,[caar,lists],['all-car',[cdr,lists]]]]],['all-cdr',[lists],[when,lists,[cons,[cdar,lists],['all-cdr',[cdr,lists]]]]]],[defun,mapc,[function,'&rest',lists],[let,[['list-1',[car,lists]]],[tagbody,start,[when,['all-end',lists],['return-from',mapc,'list-1']],[apply,function,['all-car',lists]],[setf,lists,['all-cdr',lists]],[go,start]]]],[defun,mapcar,[function,'&rest',lists],[let,[[result,[]],[end,[]]],[tagbody,start,[when,['all-end',lists],['return-from',mapcar,result]],[let,[[cons,[cons,[apply,function,['all-car',lists]],[]]]],[setf,end,[if,end,[setf,[cdr,end],cons],[setf,result,cons]]]],[setf,lists,['all-cdr',lists]],[go,start]]]],[defun,mapl,[function,'&rest',lists],[let,[['list-1',[car,lists]]],[tagbody,start,[when,['all-end',lists],['return-from',mapl,'list-1']],[apply,function,lists],[setf,lists,['all-cdr',lists]],[go,start]]]],[defun,maplist,[function,'&rest',lists],[let,[[result,[]],[end,[]]],[tagbody,start,[when,['all-end',lists],['return-from',maplist,result]],[let,[[cons,[cons,[apply,function,lists],[]]]],[setf,end,[if,end,[setf,[cdr,end],cons],[setf,result,cons]]]],[setf,lists,['all-cdr',lists]],[go,start]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_all_end,
					       kw_function,
					       f_sys_all_end)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_all_car,
					       kw_function,
					       f_sys_all_car)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_all_car,
					       kw_function,
					       f_sys_all_car)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_all_cdr,
					       kw_function,
					       f_sys_all_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_all_cdr,
					       kw_function,
					       f_sys_all_cdr)).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv98, [], [], true), append([sys_end], [CAR106, CAR], [sys_end, CAR106, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv98, [], [], true), append([sys_end], [CAR106, CAR], [sys_end, CAR106, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv126, [], [], true), append([sys_end], [CAR134, CAR133], [sys_end, CAR134, CAR133]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv126, [], [], true), append([sys_end], [CAR134, CAR133], [sys_end, CAR134, CAR133]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv202, [], [], true), append([sys_end], [CAR210, CAR209], [sys_end, CAR210, CAR209]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv202, [], [], true), append([sys_end], [CAR210, CAR209], [sys_end, CAR210, CAR209]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv230, [], [], true), append([sys_end], [CAR238, CAR237], [sys_end, CAR238, CAR237]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv230, [], [], true), append([sys_end], [CAR238, CAR237], [sys_end, CAR238, CAR237]), setf_inverse_op(cdr, rplacd))).
*/
wl:lambda_def(defun, sys_all_end, f_sys_all_end1, [sys_lists], [[dolist, [sys_elem, sys_lists, []], [unless, sys_elem, [return_from, sys_all_end, t]]]]).
wl:arglist_info(sys_all_end, f_sys_all_end1, [sys_lists], arginfo{all:[sys_lists], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_lists], opt:0, req:[sys_lists], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_all_end1).

/*

### Compiled Function: `SYS::ALL-END` 
*/
f_sys_all_end1(Lists_In, RestNKeys, FnResult) :-
	CDR=[bv(sys_lists, Lists_In)],
	catch(( ( LEnv=[bv([], [])|CDR],
		  get_var(LEnv, sys_lists, Lists_Get),
		  BV=bv(sys_elem, Ele),
		  BlockExitEnv=[BV|LEnv],
		  forall(member(Ele, Lists_Get),
			 ( nb_setarg(2, BV, Ele),
			   get_var(BlockExitEnv, sys_elem, IFTEST),
			   (   IFTEST\==[]
			   ->  _13216=[]
			   ;   throw(block_exit(sys_all_end, t)),
			       _13216=ThrowResult
			   )
			 ))
		),
		[]=FnResult
	      ),
	      block_exit(sys_all_end, FnResult),
	      true).
wl:lambda_def(defun, sys_all_car, f_sys_all_car1, [sys_lists], [[when, sys_lists, [cons, [caar, sys_lists], [sys_all_car, [cdr, sys_lists]]]]]).
wl:arglist_info(sys_all_car, f_sys_all_car1, [sys_lists], arginfo{all:[sys_lists], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_lists], opt:0, req:[sys_lists], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_all_car1).

/*

### Compiled Function: `SYS::ALL-CAR` 
*/
f_sys_all_car1(Lists_In23, RestNKeys22, FnResult21) :-
	GEnv=[bv(sys_lists, Lists_In23)],
	catch(( ( get_var(GEnv, sys_lists, IFTEST24),
		  (   IFTEST24\==[]
		  ->  get_var(GEnv, sys_lists, Lists_Get27),
		      f_caar(Lists_Get27, Caar_Ret),
		      get_var(GEnv, sys_lists, Lists_Get28),
		      f_cdr(Lists_Get28, All_car_Param),
		      f_sys_all_car(All_car_Param, All_car_Ret),
		      TrueResult=[Caar_Ret|All_car_Ret],
		      _13548=TrueResult
		  ;   _13548=[]
		  )
		),
		_13548=FnResult21
	      ),
	      block_exit(sys_all_car, FnResult21),
	      true).
wl:lambda_def(defun, sys_all_cdr, f_sys_all_cdr1, [sys_lists], [[when, sys_lists, [cons, [cdar, sys_lists], [sys_all_cdr, [cdr, sys_lists]]]]]).
wl:arglist_info(sys_all_cdr, f_sys_all_cdr1, [sys_lists], arginfo{all:[sys_lists], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_lists], opt:0, req:[sys_lists], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_all_cdr1).

/*

### Compiled Function: `SYS::ALL-CDR` 
*/
f_sys_all_cdr1(Lists_In33, RestNKeys32, FnResult31) :-
	GEnv250=[bv(sys_lists, Lists_In33)],
	catch(( ( get_var(GEnv250, sys_lists, IFTEST34),
		  (   IFTEST34\==[]
		  ->  get_var(GEnv250, sys_lists, Lists_Get37),
		      f_cdar(Lists_Get37, Cdar_Ret),
		      get_var(GEnv250, sys_lists, Lists_Get38),
		      f_cdr(Lists_Get38, All_cdr_Param),
		      f_sys_all_cdr(All_cdr_Param, All_cdr_Ret),
		      TrueResult39=[Cdar_Ret|All_cdr_Ret],
		      _13798=TrueResult39
		  ;   _13798=[]
		  )
		),
		_13798=FnResult31
	      ),
	      block_exit(sys_all_cdr, FnResult31),
	      true).
wl:lambda_def(defun, mapc, f_mapc, [function, c38_rest, sys_lists], [[let, [[sys_list_1, [car, sys_lists]]], [tagbody, sys_start, [when, [sys_all_end, sys_lists], [return_from, mapc, sys_list_1]], [apply, function, [sys_all_car, sys_lists]], [setf, sys_lists, [sys_all_cdr, sys_lists]], [go, sys_start]]]]).
wl:arglist_info(mapc, f_mapc, [function, c38_rest, sys_lists], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[function, sys_lists], opt:0, req:[function], rest:[sys_lists], sublists:0, whole:0}).
wl: init_args(1, f_mapc).

/*

### Compiled Function: `CL:MAPC` 
*/
f_mapc(Function_In, RestNKeys42, FnResult41) :-
	GEnv251=[bv(function, Function_In), bv(sys_lists, RestNKeys42)],
	catch(( ( get_var(GEnv251, sys_lists, Lists_Get48),
		  f_car(Lists_Get48, List_1_Init),
		  BlockExitEnv=[bv(sys_list_1, List_1_Init)|GEnv251],
		  call_addr_block(BlockExitEnv,
				  (push_label(sys_start), get_var(BlockExitEnv, sys_lists, Lists_Get67), f_sys_all_end1(Lists_Get67, IFTEST65), (IFTEST65\==[]->get_var(BlockExitEnv, sys_list_1, RetResult68), throw(block_exit(mapc, RetResult68)), _14604=ThrowResult69;_14604=[]), get_var(BlockExitEnv, function, Function_Get73), get_var(BlockExitEnv, sys_lists, Lists_Get74), f_sys_all_car1(Lists_Get74, KeysNRest), f_apply(Function_Get73, KeysNRest, Apply_Ret), get_var(BlockExitEnv, sys_lists, Lists_Get75), f_sys_all_cdr1(Lists_Get75, Lists), set_var(BlockExitEnv, sys_lists, Lists), goto(sys_start, BlockExitEnv)),
				  
				  [ addr(addr_tagbody_47_sys_start,
					 sys_start,
					 '$used',
					 BlockExitEnv57,
					 (get_var(BlockExitEnv57, sys_lists, Lists_Get53), f_sys_all_end1(Lists_Get53, IFTEST51), (IFTEST51\==[]->get_var(BlockExitEnv57, sys_list_1, RetResult54), throw(block_exit(mapc, RetResult54)), _14976=ThrowResult55;_14976=[]), get_var(BlockExitEnv57, function, Apply_Param), get_var(BlockExitEnv57, sys_lists, Lists_Get60), f_sys_all_car1(Lists_Get60, KeysNRest261), f_apply(Apply_Param, KeysNRest261, Apply_Ret274), get_var(BlockExitEnv57, sys_lists, Lists_Get61), f_sys_all_cdr1(Lists_Get61, KeysNRest262), set_var(BlockExitEnv57, sys_lists, KeysNRest262), goto(sys_start, BlockExitEnv57)))
				  ])
		),
		[]=FnResult41
	      ),
	      block_exit(mapc, FnResult41),
	      true).
:- set_opv(mapc, symbol_function, f_mapc),
   DefunResult=mapc,
   assert_lsp(mapcar,
	      wl:lambda_def(defun, mapcar, f_mapcar, [function, c38_rest, sys_lists], [[let, [[sys_result, []], [sys_end, []]], [tagbody, sys_start, [when, [sys_all_end, sys_lists], [return_from, mapcar, sys_result]], [let, [[cons, [cons, [apply, function, [sys_all_car, sys_lists]], []]]], [setf, sys_end, [if, sys_end, [setf, [cdr, sys_end], cons], [setf, sys_result, cons]]]], [setf, sys_lists, [sys_all_cdr, sys_lists]], [go, sys_start]]]])),
   assert_lsp(mapcar,
	      wl:arglist_info(mapcar, f_mapcar, [function, c38_rest, sys_lists], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[function, sys_lists], opt:0, req:[function], rest:[sys_lists], sublists:0, whole:0})),
   assert_lsp(mapcar, wl:init_args(1, f_mapcar)),
   assert_lsp(mapcar,
	      (f_mapcar(Function_In82, RestNKeys81, FnResult80):-CDR275=[bv(function, Function_In82), bv(sys_lists, RestNKeys81)], catch(((BlockExitEnv=[bv(sys_result, []), bv(sys_end, [])|CDR275], call_addr_block(BlockExitEnv,  (push_label(sys_start), get_var(BlockExitEnv, sys_lists, Lists_Get118), f_sys_all_end1(Lists_Get118, IFTEST116), (IFTEST116\==[]->get_var(BlockExitEnv, sys_result, RetResult119), throw(block_exit(mapcar, RetResult119)), _15918=ThrowResult120;_15918=[]), get_var(BlockExitEnv, function, Function_Get127), get_var(BlockExitEnv, sys_lists, Lists_Get128), f_sys_all_car1(Lists_Get128, KeysNRest263), f_apply(Function_Get127, KeysNRest263, Apply_Ret276), Cons_Init129=[Apply_Ret276], LEnv126=[bv(cons, Cons_Init129)|BlockExitEnv], get_var(LEnv126, sys_end, IFTEST130), (IFTEST130\==[]->get_var(LEnv126, cons, Cons_Get136), get_var(LEnv126, sys_end, End_Get135), f_rplacd(End_Get135, Cons_Get136, TrueResult138), LetResult125=TrueResult138;get_var(LEnv126, cons, Cons_Get137), set_var(LEnv126, sys_result, Cons_Get137), LetResult125=Cons_Get137), set_var(LEnv126, sys_end, LetResult125), get_var(BlockExitEnv, sys_lists, Lists_Get140), f_sys_all_cdr1(Lists_Get140, Lists253), set_var(BlockExitEnv, sys_lists, Lists253), goto(sys_start, BlockExitEnv)), [addr(addr_tagbody_48_sys_start, sys_start, '$used', BlockExitEnv94,  (get_var(BlockExitEnv94, sys_lists, Lists_Get90), f_sys_all_end1(Lists_Get90, IFTEST88), (IFTEST88\==[]->get_var(BlockExitEnv94, sys_result, RetResult91), throw(block_exit(mapcar, RetResult91)), _16660=ThrowResult92;_16660=[]), get_var(BlockExitEnv94, function, Function_Get99), get_var(BlockExitEnv94, sys_lists, Lists_Get100), f_sys_all_car1(Lists_Get100, KeysNRest264), f_apply(Function_Get99, KeysNRest264, Apply_Ret277), Bv_Ret=[Apply_Ret277], LEnv98=[bv(cons, Bv_Ret)|BlockExitEnv94], get_var(LEnv98, sys_end, IFTEST102), (IFTEST102\==[]->get_var(LEnv98, cons, Get_var_Ret), get_var(LEnv98, sys_end, End_Get107), f_rplacd(End_Get107, Get_var_Ret, TrueResult110), LetResult97=TrueResult110;get_var(LEnv98, cons, Cons_Get109), set_var(LEnv98, sys_result, Cons_Get109), LetResult97=Cons_Get109), set_var(LEnv98, sys_end, LetResult97), get_var(BlockExitEnv94, sys_lists, Lists_Get112), f_sys_all_cdr1(Lists_Get112, KeysNRest265), set_var(BlockExitEnv94, sys_lists, KeysNRest265), goto(sys_start, BlockExitEnv94)))])), []=FnResult80), block_exit(mapcar, FnResult80), true))),
   set_opv(mapcar, symbol_function, f_mapcar),
   DefunResult144=mapcar,
   assert_lsp(mapl,
	      wl:lambda_def(defun, mapl, f_mapl, [function, c38_rest, sys_lists], [[let, [[sys_list_1, [car, sys_lists]]], [tagbody, sys_start, [when, [sys_all_end, sys_lists], [return_from, mapl, sys_list_1]], [apply, function, sys_lists], [setf, sys_lists, [sys_all_cdr, sys_lists]], [go, sys_start]]]])),
   assert_lsp(mapl,
	      wl:arglist_info(mapl, f_mapl, [function, c38_rest, sys_lists], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[function, sys_lists], opt:0, req:[function], rest:[sys_lists], sublists:0, whole:0})),
   assert_lsp(mapl, wl:init_args(1, f_mapl)),
   assert_lsp(mapl,
	      (f_mapl(Function_In147, RestNKeys146, FnResult145):-GEnv254=[bv(function, Function_In147), bv(sys_lists, RestNKeys146)], catch(((get_var(GEnv254, sys_lists, Lists_Get152), f_car(Lists_Get152, List_1_Init153), BlockExitEnv=[bv(sys_list_1, List_1_Init153)|GEnv254], call_addr_block(BlockExitEnv,  (push_label(sys_start), get_var(BlockExitEnv, sys_lists, Lists_Get171), f_sys_all_end1(Lists_Get171, IFTEST169), (IFTEST169\==[]->get_var(BlockExitEnv, sys_list_1, RetResult172), throw(block_exit(mapl, RetResult172)), _17478=ThrowResult173;_17478=[]), get_var(BlockExitEnv, function, Function_Get177), get_var(BlockExitEnv, sys_lists, Lists_Get178), f_apply(Function_Get177, Lists_Get178, Apply_Ret280), get_var(BlockExitEnv, sys_lists, Lists_Get179), f_sys_all_cdr1(Lists_Get179, Lists255), set_var(BlockExitEnv, sys_lists, Lists255), goto(sys_start, BlockExitEnv)), [addr(addr_tagbody_49_sys_start, sys_start, '$used', BlockExitEnv161,  (get_var(BlockExitEnv161, sys_lists, Lists_Get157), f_sys_all_end1(Lists_Get157, IFTEST155), (IFTEST155\==[]->get_var(BlockExitEnv161, sys_list_1, RetResult158), throw(block_exit(mapl, RetResult158)), _17850=ThrowResult159;_17850=[]), get_var(BlockExitEnv161, function, Function_Get163), get_var(BlockExitEnv161, sys_lists, Lists_Get164), f_apply(Function_Get163, Lists_Get164, Apply_Ret281), get_var(BlockExitEnv161, sys_lists, Lists_Get165), f_sys_all_cdr1(Lists_Get165, KeysNRest266), set_var(BlockExitEnv161, sys_lists, KeysNRest266), goto(sys_start, BlockExitEnv161)))])), []=FnResult145), block_exit(mapl, FnResult145), true))),
   set_opv(mapl, symbol_function, f_mapl),
   DefunResult183=mapl,
   assert_lsp(maplist,
	      wl:lambda_def(defun, maplist, f_maplist, [function, c38_rest, sys_lists], [[let, [[sys_result, []], [sys_end, []]], [tagbody, sys_start, [when, [sys_all_end, sys_lists], [return_from, maplist, sys_result]], [let, [[cons, [cons, [apply, function, sys_lists], []]]], [setf, sys_end, [if, sys_end, [setf, [cdr, sys_end], cons], [setf, sys_result, cons]]]], [setf, sys_lists, [sys_all_cdr, sys_lists]], [go, sys_start]]]])),
   assert_lsp(maplist,
	      wl:arglist_info(maplist, f_maplist, [function, c38_rest, sys_lists], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[function, sys_lists], opt:0, req:[function], rest:[sys_lists], sublists:0, whole:0})),
   assert_lsp(maplist, wl:init_args(1, f_maplist)),
   assert_lsp(maplist,
	      (f_maplist(Function_In186, RestNKeys185, FnResult184):-CDR282=[bv(function, Function_In186), bv(sys_lists, RestNKeys185)], catch(((BlockExitEnv=[bv(sys_result, []), bv(sys_end, [])|CDR282], call_addr_block(BlockExitEnv,  (push_label(sys_start), get_var(BlockExitEnv, sys_lists, Lists_Get222), f_sys_all_end1(Lists_Get222, IFTEST220), (IFTEST220\==[]->get_var(BlockExitEnv, sys_result, RetResult223), throw(block_exit(maplist, RetResult223)), _18890=ThrowResult224;_18890=[]), get_var(BlockExitEnv, function, Function_Get231), get_var(BlockExitEnv, sys_lists, Lists_Get232), f_apply(Function_Get231, Lists_Get232, Apply_Ret283), Cons_Init233=[Apply_Ret283], LEnv230=[bv(cons, Cons_Init233)|BlockExitEnv], get_var(LEnv230, sys_end, IFTEST234), (IFTEST234\==[]->get_var(LEnv230, cons, Cons_Get240), get_var(LEnv230, sys_end, End_Get239), f_rplacd(End_Get239, Cons_Get240, TrueResult242), LetResult229=TrueResult242;get_var(LEnv230, cons, Cons_Get241), set_var(LEnv230, sys_result, Cons_Get241), LetResult229=Cons_Get241), set_var(LEnv230, sys_end, LetResult229), get_var(BlockExitEnv, sys_lists, Lists_Get244), f_sys_all_cdr1(Lists_Get244, Lists256), set_var(BlockExitEnv, sys_lists, Lists256), goto(sys_start, BlockExitEnv)), [addr(addr_tagbody_50_sys_start, sys_start, '$used', BlockExitEnv198,  (get_var(BlockExitEnv198, sys_lists, Lists_Get194), f_sys_all_end1(Lists_Get194, IFTEST192), (IFTEST192\==[]->get_var(BlockExitEnv198, sys_result, RetResult195), throw(block_exit(maplist, RetResult195)), _19630=ThrowResult196;_19630=[]), get_var(BlockExitEnv198, function, Function_Get203), get_var(BlockExitEnv198, sys_lists, Lists_Get204), f_apply(Function_Get203, Lists_Get204, Apply_Ret284), Cons_Init205=[Apply_Ret284], LEnv202=[bv(cons, Cons_Init205)|BlockExitEnv198], get_var(LEnv202, sys_end, IFTEST206), (IFTEST206\==[]->get_var(LEnv202, cons, Cons_Get212), get_var(LEnv202, sys_end, End_Get211), f_rplacd(End_Get211, Cons_Get212, TrueResult214), LetResult201=TrueResult214;get_var(LEnv202, cons, Cons_Get213), set_var(LEnv202, sys_result, Cons_Get213), LetResult201=Cons_Get213), set_var(LEnv202, sys_end, LetResult201), get_var(BlockExitEnv198, sys_lists, Lists_Get216), f_sys_all_cdr1(Lists_Get216, KeysNRest267), set_var(BlockExitEnv198, sys_lists, KeysNRest267), goto(sys_start, BlockExitEnv198)))])), []=FnResult184), block_exit(maplist, FnResult184), true))),
   set_opv(maplist, symbol_function, f_maplist),
   DefunResult248=maplist.
/*
:- side_effect(assert_lsp(sys_all_end,
			  lambda_def(defun,
				     sys_all_end,
				     f_sys_all_end1,
				     [sys_lists],
				     
				     [ 
				       [ dolist,
					 [sys_elem, sys_lists, []],
					 
					 [ unless,
					   sys_elem,
					   [return_from, sys_all_end, t]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_all_end,
			  arglist_info(sys_all_end,
				       f_sys_all_end1,
				       [sys_lists],
				       arginfo{ all:[sys_lists],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_lists],
						opt:0,
						req:[sys_lists],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_all_end, init_args(1, f_sys_all_end1))).
*/
/*
:- side_effect(assert_lsp(sys_all_car,
			  lambda_def(defun,
				     sys_all_car,
				     f_sys_all_car1,
				     [sys_lists],
				     
				     [ 
				       [ when,
					 sys_lists,
					 
					 [ cons,
					   [caar, sys_lists],
					   [sys_all_car, [cdr, sys_lists]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_all_car,
			  arglist_info(sys_all_car,
				       f_sys_all_car1,
				       [sys_lists],
				       arginfo{ all:[sys_lists],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_lists],
						opt:0,
						req:[sys_lists],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_all_car, init_args(1, f_sys_all_car1))).
*/
/*
:- side_effect(assert_lsp(sys_all_cdr,
			  lambda_def(defun,
				     sys_all_cdr,
				     f_sys_all_cdr1,
				     [sys_lists],
				     
				     [ 
				       [ when,
					 sys_lists,
					 
					 [ cons,
					   [cdar, sys_lists],
					   [sys_all_cdr, [cdr, sys_lists]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_all_cdr,
			  arglist_info(sys_all_cdr,
				       f_sys_all_cdr1,
				       [sys_lists],
				       arginfo{ all:[sys_lists],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_lists],
						opt:0,
						req:[sys_lists],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_all_cdr, init_args(1, f_sys_all_cdr1))).
*/
/*
:- side_effect(assert_lsp(mapc,
			  lambda_def(defun,
				     mapc,
				     f_mapc,
				     [function, c38_rest, sys_lists],
				     
				     [ 
				       [ let,
					 [[sys_list_1, [car, sys_lists]]],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ when,
					     [sys_all_end, sys_lists],
					     [return_from, mapc, sys_list_1]
					   ],
					   
					   [ apply,
					     function,
					     [sys_all_car, sys_lists]
					   ],
					   
					   [ setf,
					     sys_lists,
					     [sys_all_cdr, sys_lists]
					   ],
					   [go, sys_start]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(mapc,
			  arglist_info(mapc,
				       f_mapc,
				       [function, c38_rest, sys_lists],
				       arginfo{ all:[function],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[function, sys_lists],
						opt:0,
						req:[function],
						rest:[sys_lists],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(mapc, init_args(1, f_mapc))).
*/
/*
:- side_effect(assert_lsp(mapcar,
			  lambda_def(defun,
				     mapcar,
				     f_mapcar,
				     [function, c38_rest, sys_lists],
				     
				     [ 
				       [ let,
					 [[sys_result, []], [sys_end, []]],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ when,
					     [sys_all_end, sys_lists],
					     [return_from, mapcar, sys_result]
					   ],
					   
					   [ let,
					     
					     [ 
					       [ cons,
						 
						 [ cons,
						   
						   [ apply,
						     function,
						     [sys_all_car, sys_lists]
						   ],
						   []
						 ]
					       ]
					     ],
					     
					     [ setf,
					       sys_end,
					       
					       [ if,
						 sys_end,
						 [setf, [cdr, sys_end], cons],
						 [setf, sys_result, cons]
					       ]
					     ]
					   ],
					   
					   [ setf,
					     sys_lists,
					     [sys_all_cdr, sys_lists]
					   ],
					   [go, sys_start]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(mapcar,
			  arglist_info(mapcar,
				       f_mapcar,
				       [function, c38_rest, sys_lists],
				       arginfo{ all:[function],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[function, sys_lists],
						opt:0,
						req:[function],
						rest:[sys_lists],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(mapcar, init_args(1, f_mapcar))).
*/
/*
:- side_effect(assert_lsp(mapl,
			  lambda_def(defun,
				     mapl,
				     f_mapl,
				     [function, c38_rest, sys_lists],
				     
				     [ 
				       [ let,
					 [[sys_list_1, [car, sys_lists]]],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ when,
					     [sys_all_end, sys_lists],
					     [return_from, mapl, sys_list_1]
					   ],
					   [apply, function, sys_lists],
					   
					   [ setf,
					     sys_lists,
					     [sys_all_cdr, sys_lists]
					   ],
					   [go, sys_start]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(mapl,
			  arglist_info(mapl,
				       f_mapl,
				       [function, c38_rest, sys_lists],
				       arginfo{ all:[function],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[function, sys_lists],
						opt:0,
						req:[function],
						rest:[sys_lists],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(mapl, init_args(1, f_mapl))).
*/
/*
:- side_effect(assert_lsp(maplist,
			  lambda_def(defun,
				     maplist,
				     f_maplist,
				     [function, c38_rest, sys_lists],
				     
				     [ 
				       [ let,
					 [[sys_result, []], [sys_end, []]],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ when,
					     [sys_all_end, sys_lists],
					     [return_from, maplist, sys_result]
					   ],
					   
					   [ let,
					     
					     [ 
					       [ cons,
						 
						 [ cons,
						   [apply, function, sys_lists],
						   []
						 ]
					       ]
					     ],
					     
					     [ setf,
					       sys_end,
					       
					       [ if,
						 sys_end,
						 [setf, [cdr, sys_end], cons],
						 [setf, sys_result, cons]
					       ]
					     ]
					   ],
					   
					   [ setf,
					     sys_lists,
					     [sys_all_cdr, sys_lists]
					   ],
					   [go, sys_start]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(maplist,
			  arglist_info(maplist,
				       f_maplist,
				       [function, c38_rest, sys_lists],
				       arginfo{ all:[function],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[function, sys_lists],
						opt:0,
						req:[function],
						rest:[sys_lists],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(maplist, init_args(1, f_maplist))).
*/
/*
#+(or WAM-CL LISP500) 
(defun mapcan (function &rest lists)
  (apply #'nconc (apply #'mapcar function lists)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:19122 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,mapcan,[function,'&rest',lists],[apply,function(nconc),[apply,function(mapcar),function,lists]]])
wl:lambda_def(defun, mapcan, f_mapcan, [function, c38_rest, sys_lists], [[apply, function(nconc), [apply, function(mapcar), function, sys_lists]]]).
wl:arglist_info(mapcan, f_mapcan, [function, c38_rest, sys_lists], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[function, sys_lists], opt:0, req:[function], rest:[sys_lists], sublists:0, whole:0}).
wl: init_args(1, f_mapcan).

/*

### Compiled Function: `CL:MAPCAN` 
*/
f_mapcan(Function_In, RestNKeys, FnResult) :-
	GEnv=[bv(function, Function_In), bv(sys_lists, RestNKeys)],
	catch(( ( get_var(GEnv, function, Function_Get),
		  get_var(GEnv, sys_lists, Lists_Get),
		  f_apply(f_mapcar, [Function_Get, Lists_Get], Nconc_Param),
		  f_nconc(Nconc_Param, Nconc_Ret)
		),
		Nconc_Ret=FnResult
	      ),
	      block_exit(mapcan, FnResult),
	      true).
:- set_opv(mapcan, symbol_function, f_mapcan),
   DefunResult=mapcan.
/*
:- side_effect(assert_lsp(mapcan,
			  lambda_def(defun,
				     mapcan,
				     f_mapcan,
				     [function, c38_rest, sys_lists],
				     
				     [ 
				       [ apply,
					 function(nconc),
					 
					 [ apply,
					   function(mapcar),
					   function,
					   sys_lists
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(mapcan,
			  arglist_info(mapcan,
				       f_mapcan,
				       [function, c38_rest, sys_lists],
				       arginfo{ all:[function],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[function, sys_lists],
						opt:0,
						req:[function],
						rest:[sys_lists],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(mapcan, init_args(1, f_mapcan))).
*/
/*
#+(or WAM-CL LISP500) 
(defun mapcon (function &rest lists)
  (apply #'nconc (apply #'maplist function lists)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:19238 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,mapcon,[function,'&rest',lists],[apply,function(nconc),[apply,function(maplist),function,lists]]])
wl:lambda_def(defun, mapcon, f_mapcon, [function, c38_rest, sys_lists], [[apply, function(nconc), [apply, function(maplist), function, sys_lists]]]).
wl:arglist_info(mapcon, f_mapcon, [function, c38_rest, sys_lists], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[function, sys_lists], opt:0, req:[function], rest:[sys_lists], sublists:0, whole:0}).
wl: init_args(1, f_mapcon).

/*

### Compiled Function: `CL:MAPCON` 
*/
f_mapcon(Function_In, RestNKeys, FnResult) :-
	GEnv=[bv(function, Function_In), bv(sys_lists, RestNKeys)],
	catch(( ( get_var(GEnv, function, Function_Get),
		  get_var(GEnv, sys_lists, Lists_Get),
		  f_apply(f_maplist, [Function_Get, Lists_Get], Nconc_Param),
		  f_nconc(Nconc_Param, Nconc_Ret)
		),
		Nconc_Ret=FnResult
	      ),
	      block_exit(mapcon, FnResult),
	      true).
:- set_opv(mapcon, symbol_function, f_mapcon),
   DefunResult=mapcon.
/*
:- side_effect(assert_lsp(mapcon,
			  lambda_def(defun,
				     mapcon,
				     f_mapcon,
				     [function, c38_rest, sys_lists],
				     
				     [ 
				       [ apply,
					 function(nconc),
					 
					 [ apply,
					   function(maplist),
					   function,
					   sys_lists
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(mapcon,
			  arglist_info(mapcon,
				       f_mapcon,
				       [function, c38_rest, sys_lists],
				       arginfo{ all:[function],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[function, sys_lists],
						opt:0,
						req:[function],
						rest:[sys_lists],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(mapcon, init_args(1, f_mapcon))).
*/
/*
#+(or WAM-CL LISP500) 
(defun acons (key datum alist) (cons (cons key datum) alist))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:19355 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,acons,[key,datum,alist],[cons,[cons,key,datum],alist]])
wl:lambda_def(defun, acons, f_acons, [key, sys_datum, sys_alist], [[cons, [cons, key, sys_datum], sys_alist]]).
wl:arglist_info(acons, f_acons, [key, sys_datum, sys_alist], arginfo{all:[key, sys_datum, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[key, sys_datum, sys_alist], opt:0, req:[key, sys_datum, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_acons).

/*

### Compiled Function: `CL:ACONS` 
*/
f_acons(Key_In, Datum_In, Alist_In, FnResult) :-
	GEnv=[bv(key, Key_In), bv(sys_datum, Datum_In), bv(sys_alist, Alist_In)],
	catch(( ( get_var(GEnv, key, Key_Get),
		  get_var(GEnv, sys_datum, Datum_Get),
		  CAR=[Key_Get|Datum_Get],
		  get_var(GEnv, sys_alist, Alist_Get),
		  _10522=[CAR|Alist_Get]
		),
		_10522=FnResult
	      ),
	      block_exit(acons, FnResult),
	      true).
:- set_opv(acons, symbol_function, f_acons),
   DefunResult=acons.
/*
:- side_effect(assert_lsp(acons,
			  lambda_def(defun,
				     acons,
				     f_acons,
				     [key, sys_datum, sys_alist],
				     [[cons, [cons, key, sys_datum], sys_alist]]))).
*/
/*
:- side_effect(assert_lsp(acons,
			  arglist_info(acons,
				       f_acons,
				       [key, sys_datum, sys_alist],
				       arginfo{ all:[key, sys_datum, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ key,
							sys_datum,
							sys_alist
						      ],
						opt:0,
						req:[key, sys_datum, sys_alist],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(acons, init_args(x, f_acons))).
*/
/*
#+(or WAM-CL LISP500) 
(defun copy-alist (alist)
  (when alist (cons (if (consp (car alist))
			(cons (caar alist) (cdar alist))
			(car alist))
		    (copy-alist (cdr alist)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:19444 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'copy-alist',[alist],[when,alist,[cons,[if,[consp,[car,alist]],[cons,[caar,alist],[cdar,alist]],[car,alist]],['copy-alist',[cdr,alist]]]]])
wl:lambda_def(defun, copy_alist, f_copy_alist, [sys_alist], [[when, sys_alist, [cons, [if, [consp, [car, sys_alist]], [cons, [caar, sys_alist], [cdar, sys_alist]], [car, sys_alist]], [copy_alist, [cdr, sys_alist]]]]]).
wl:arglist_info(copy_alist, f_copy_alist, [sys_alist], arginfo{all:[sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_alist], opt:0, req:[sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_copy_alist).

/*

### Compiled Function: `CL:COPY-ALIST` 
*/
f_copy_alist(Alist_In, FnResult) :-
	GEnv=[bv(sys_alist, Alist_In)],
	catch(( ( get_var(GEnv, sys_alist, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_alist, Alist_Get9),
		      f_car(Alist_Get9, PredArgResult),
		      (   c0nz:is_consp(PredArgResult)
		      ->  get_var(GEnv, sys_alist, Alist_Get12),
			  f_caar(Alist_Get12, Caar_Ret),
			  get_var(GEnv, sys_alist, Alist_Get13),
			  f_cdar(Alist_Get13, Cdar_Ret),
			  TrueResult=[Caar_Ret|Cdar_Ret],
			  CAR=TrueResult
		      ;   get_var(GEnv, sys_alist, Alist_Get14),
			  f_car(Alist_Get14, ElseResult),
			  CAR=ElseResult
		      ),
		      get_var(GEnv, sys_alist, Alist_Get17),
		      f_cdr(Alist_Get17, Copy_alist_Param),
		      f_copy_alist(Copy_alist_Param, Copy_alist_Ret),
		      TrueResult18=[CAR|Copy_alist_Ret],
		      _7326=TrueResult18
		  ;   _7326=[]
		  )
		),
		_7326=FnResult
	      ),
	      block_exit(copy_alist, FnResult),
	      true).
:- set_opv(copy_alist, symbol_function, f_copy_alist),
   DefunResult=copy_alist.
/*
:- side_effect(assert_lsp(copy_alist,
			  lambda_def(defun,
				     copy_alist,
				     f_copy_alist,
				     [sys_alist],
				     
				     [ 
				       [ when,
					 sys_alist,
					 
					 [ cons,
					   
					   [ if,
					     [consp, [car, sys_alist]],
					     
					     [ cons,
					       [caar, sys_alist],
					       [cdar, sys_alist]
					     ],
					     [car, sys_alist]
					   ],
					   [copy_alist, [cdr, sys_alist]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(copy_alist,
			  arglist_info(copy_alist,
				       f_copy_alist,
				       [sys_alist],
				       arginfo{ all:[sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_alist],
						opt:0,
						req:[sys_alist],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(copy_alist, init_args(x, f_copy_alist))).
*/
/*
#+(or WAM-CL LISP500) 
(defun pairlis (keys data &optional alist)
  (tagbody
   start
     (when (and keys data)
       (setf alist (acons (car keys) (car data) alist))
       (setf keys (cdr keys))
       (setf data (cdr data))
       (go start)))
  alist)



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:19631 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,pairlis,[keys,data,'&optional',alist],[tagbody,start,[when,[and,keys,data],[setf,alist,[acons,[car,keys],[car,data],alist]],[setf,keys,[cdr,keys]],[setf,data,[cdr,data]],[go,start]]],alist])
wl:lambda_def(defun,pairlis,f_pairlis,[sys_keys,sys_data,c38_optional,sys_alist],[[tagbody,sys_start,[when,[and,sys_keys,sys_data],[setf,sys_alist,[acons,[car,sys_keys],[car,sys_data],sys_alist]],[setf,sys_keys,[cdr,sys_keys]],[setf,sys_data,[cdr,sys_data]],[go,sys_start]]],sys_alist]).
wl:arglist_info(pairlis,f_pairlis,[sys_keys,sys_data,c38_optional,sys_alist],arginfo{all:[sys_keys,sys_data,sys_alist],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[sys_keys,sys_data,sys_alist],opt:[sys_alist],req:[sys_keys,sys_data],rest:0,sublists:0,whole:0}).
wl:init_args(2,f_pairlis).

/*

### Compiled Function: `CL:PAIRLIS` 
*/
f_pairlis(_7622,_7624,_7642,_8590):-_8394=[bv(sys_keys,_7622),bv(sys_data,_7624),bv(sys_alist,_7626)],opt_var(_7574,sys_alist,_7626,true,[],1,_7642),catch(((call_addr_block(_8394,(push_label(sys_start),get_var(_8394,sys_keys,_8100),(_8100\==[]->get_var(_8394,sys_data,_8176),_8058=_8176;_8058=[]),(_8058\==[]->get_var(_8394,sys_keys,_8222),f_car(_8222,_8204),get_var(_8394,sys_data,_8252),f_car(_8252,_8234),get_var(_8394,sys_alist,_8280),f_acons(_8204,_8234,_8280,_8202),set_var(_8394,sys_alist,_8202),get_var(_8394,sys_keys,_8310),f_cdr(_8310,_8292),set_var(_8394,sys_keys,_8292),get_var(_8394,sys_data,_8340),f_cdr(_8340,_8322),set_var(_8394,sys_data,_8322),goto(sys_start,_8394),_7690=_8368;_7690=[])),[addr(addr_tagbody_51_sys_start,sys_start,'$unused',_8420,(get_var(_8420,sys_keys,_8424),(_8424\==[]->get_var(_8420,sys_data,_8438),_8452=_8438;_8452=[]),(_8452\==[]->get_var(_8420,sys_keys,_8466),f_car(_8466,_11142),get_var(_8420,sys_data,_8482),f_car(_8482,_11180),get_var(_8420,sys_alist,_8496),f_acons(_11142,_11180,_8496,_8498),set_var(_8420,sys_alist,_8498),get_var(_8420,sys_keys,_8502),f_cdr(_8502,_8514),set_var(_8420,sys_keys,_8514),get_var(_8420,sys_data,_8518),f_cdr(_8518,_11242),set_var(_8420,sys_data,_11242),goto(sys_start,_8420),_8532=_8536;_8532=[])))]),get_var(_8394,sys_alist,_8564)),_8564=_8590),block_exit(pairlis,_8590),true).
:-set_opv(pairlis,symbol_function,f_pairlis),_7030=pairlis.
/*
:-side_effect(assert_lsp(pairlis,lambda_def(defun,pairlis,f_pairlis,[sys_keys,sys_data,c38_optional,sys_alist],[[tagbody,sys_start,[when,[and,sys_keys,sys_data],[setf,sys_alist,[acons,[car,sys_keys],[car,sys_data],sys_alist]],[setf,sys_keys,[cdr,sys_keys]],[setf,sys_data,[cdr,sys_data]],[go,sys_start]]],sys_alist]))).
*/
/*
:-side_effect(assert_lsp(pairlis,arglist_info(pairlis,f_pairlis,[sys_keys,sys_data,c38_optional,sys_alist],arginfo{all:[sys_keys,sys_data,sys_alist],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[sys_keys,sys_data,sys_alist],opt:[sys_alist],req:[sys_keys,sys_data],rest:0,sublists:0,whole:0}))).
*/
/*
:-side_effect(assert_lsp(pairlis,init_args(2,f_pairlis))).
*/
/*
#+(or WAM-CL LISP500) 
(defun some-list-2 (predicate list1 list2)
  (tagbody
   start
     (when (and list1 list2)
       (when (funcall predicate (car list1) (car list2))
	 (return-from some-list-2 t))
       (pop list1)
       (pop list2)
       (go start))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:19905 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'some-list-2',[predicate,list1,list2],[tagbody,start,[when,[and,list1,list2],[when,[funcall,predicate,[car,list1],[car,list2]],['return-from','some-list-2',t]],[pop,list1],[pop,list2],[go,start]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_some_list_2,
					       kw_function,
					       f_sys_some_list_2)).
*/
/*
% macroexpand:-[pop,sys_list1].
*/
/*
% into:-[prog1,[car,sys_list1],[setq,sys_list1,[cdr,sys_list1]]].
*/
/*
% macroexpand:-[pop,sys_list2].
*/
/*
% into:-[prog1,[car,sys_list2],[setq,sys_list2,[cdr,sys_list2]]].
*/
/*
% macroexpand:-[pop,sys_list1].
*/
/*
% into:-[prog1,[car,sys_list1],[setq,sys_list1,[cdr,sys_list1]]].
*/
/*
% macroexpand:-[pop,sys_list2].
*/
/*
% into:-[prog1,[car,sys_list2],[setq,sys_list2,[cdr,sys_list2]]].
*/
wl:lambda_def(defun, sys_some_list_2, f_sys_some_list_2, [sys_predicate, sys_list1, sys_list2], [[tagbody, sys_start, [when, [and, sys_list1, sys_list2], [when, [funcall, sys_predicate, [car, sys_list1], [car, sys_list2]], [return_from, sys_some_list_2, t]], [pop, sys_list1], [pop, sys_list2], [go, sys_start]]]]).
wl:arglist_info(sys_some_list_2, f_sys_some_list_2, [sys_predicate, sys_list1, sys_list2], arginfo{all:[sys_predicate, sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_predicate, sys_list1, sys_list2], opt:0, req:[sys_predicate, sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_some_list_2).

/*

### Compiled Function: `SYS::SOME-LIST-2` 
*/
f_sys_some_list_2(Predicate_In, List1_In, List2_In, FnResult) :-
	AEnv=[bv(sys_predicate, Predicate_In), bv(sys_list1, List1_In), bv(sys_list2, List2_In)],
	catch(( call_addr_block(AEnv,
				(push_label(sys_start), get_var(AEnv, sys_list1, IFTEST35), (IFTEST35\==[]->get_var(AEnv, sys_list2, List2_Get38), IFTEST33=List2_Get38;IFTEST33=[]), (IFTEST33\==[]->get_var(AEnv, sys_list1, List1_Get43), get_var(AEnv, sys_predicate, Predicate_Get42), f_car(List1_Get43, Car_Ret), get_var(AEnv, sys_list2, List2_Get44), f_car(List2_Get44, Car_Ret63), f_apply(Predicate_Get42, [Car_Ret, Car_Ret63], IFTEST40), (IFTEST40\==[]->throw(block_exit(sys_some_list_2, t)), _8232=ThrowResult46;_8232=[]), get_var(AEnv, sys_list1, List1_Get49), f_car(List1_Get49, Car_Ret64), get_var(AEnv, sys_list1, List1_Get51), f_cdr(List1_Get51, List1), set_var(AEnv, sys_list1, List1), get_var(AEnv, sys_list2, List2_Get52), f_car(List2_Get52, Car_Ret65), get_var(AEnv, sys_list2, List2_Get53), f_cdr(List2_Get53, List2), set_var(AEnv, sys_list2, List2), goto(sys_start, AEnv), _TBResult=_GORES54;_TBResult=[])),
				
				[ addr(addr_tagbody_52_sys_start,
				       sys_start,
				       '$unused',
				       AEnv,
				       (get_var(AEnv, sys_list1, IFTEST10), (IFTEST10\==[]->get_var(AEnv, sys_list2, List2_Get), IFTEST=List2_Get;IFTEST=[]), (IFTEST\==[]->get_var(AEnv, sys_list1, List1_Get18), get_var(AEnv, sys_predicate, Apply_Param), f_car(List1_Get18, Car_Ret66), get_var(AEnv, sys_list2, List2_Get19), f_car(List2_Get19, Car_Ret67), f_apply(Apply_Param, [Car_Ret66, Car_Ret67], IFTEST15), (IFTEST15\==[]->throw(block_exit(sys_some_list_2, t)), _8766=ThrowResult;_8766=[]), get_var(AEnv, sys_list1, List1_Get24), f_car(List1_Get24, Car_Ret68), get_var(AEnv, sys_list1, List1_Get26), f_cdr(List1_Get26, Cdr_Ret), set_var(AEnv, sys_list1, Cdr_Ret), get_var(AEnv, sys_list2, List2_Get27), f_car(List2_Get27, Car_Ret70), get_var(AEnv, sys_list2, List2_Get28), f_cdr(List2_Get28, Cdr_Ret71), set_var(AEnv, sys_list2, Cdr_Ret71), goto(sys_start, AEnv), _8846=_GORES;_8846=[])))
				]),
		[]=FnResult
	      ),
	      block_exit(sys_some_list_2, FnResult),
	      true).
:- set_opv(sys_some_list_2, symbol_function, f_sys_some_list_2),
   DefunResult=sys_some_list_2.
/*
:- side_effect(assert_lsp(sys_some_list_2,
			  lambda_def(defun,
				     sys_some_list_2,
				     f_sys_some_list_2,
				     [sys_predicate, sys_list1, sys_list2],
				     
				     [ 
				       [ tagbody,
					 sys_start,
					 
					 [ when,
					   [and, sys_list1, sys_list2],
					   
					   [ when,
					     
					     [ funcall,
					       sys_predicate,
					       [car, sys_list1],
					       [car, sys_list2]
					     ],
					     [return_from, sys_some_list_2, t]
					   ],
					   [pop, sys_list1],
					   [pop, sys_list2],
					   [go, sys_start]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_some_list_2,
			  arglist_info(sys_some_list_2,
				       f_sys_some_list_2,
				       [sys_predicate, sys_list1, sys_list2],
				       arginfo{ all:
						    [ sys_predicate,
						      sys_list1,
						      sys_list2
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_list1,
							sys_list2
						      ],
						opt:0,
						req:
						    [ sys_predicate,
						      sys_list1,
						      sys_list2
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_some_list_2, init_args(x, f_sys_some_list_2))).
*/
/*
(flet 
  ((satisfies (object elem &key key test test-not)
	 (let* ((zi (if key (funcall key elem) elem))
		(r (funcall (or test test-not #'eql) object zi)))
	   (if test-not (not r) r)))

       (satisfies-if (predicate elem &key key)
	 (funcall predicate (if key (funcall key elem) elem)))
       (satisfies-if-not (predicate elem &key key)
	 (not (funcall predicate (if key (funcall key elem) elem))))
       (seq-start (sequence &key (start 0) end from-end)
	 (if (listp sequence)
	     (if from-end
		 (let ((acc nil)
		       (sequence (nthcdr start sequence)))
		   (tagbody
		    start
		      (when (and sequence (or (not end) (< start end)))
			(push sequence acc)
			(setf sequence (cdr sequence))
			(setf start (+ 1 start))
			(go start)))
		   (list 3 acc start))
		 (list 2 (nthcdr start sequence) start))
	     (if from-end (cons 1 (- end 1)) (cons 0 start))))
       (seq-position (iter)
	 (case (car iter)
	   ((0 1) (cdr iter))
	   (t (caddr iter))))
       (seq-next (iter)
	 (case (car iter)
	   (0 (setf (cdr iter) (+ 1 (cdr iter))))
	   (1 (setf (cdr iter) (- (cdr iter) 1)))
	   (2 (setf (cadr iter) (cdadr iter))
	      (setf (caddr iter) (+ 1 (caddr iter))))
	   (t (setf (cadr iter) (cdadr iter))
	      (setf (caddr iter) (- (caddr iter) 1)))))
       (seq-ref (sequence iter)
	 (case (car iter)
	   ((0 1) (aref sequence (cdr iter)))
	   (2 (caadr iter))
	   (t (caaadr iter))))
       (seq-set (sequence iter value)
	 (case (car iter)
	   ((0 1) (setf (aref sequence (cdr iter)) value))
	   (2 (setf (caadr iter) value))
	   (t (setf (caaadr iter) value))))
       (seq-end-p (sequence iter &key start end from-end)
	 (case (car iter)
	   (0 (or (= (cdr iter) (length sequence))
		  (and end (= end (cdr iter)))))
	   (1 (< (cdr iter) start))
	   (2 (or (null (cadr iter)) (and end (= end (caddr iter)))))
	   (t (or (null (cadr iter)) (< (caddr iter) start)))))
       (seq-result (sequence iter result)
	 (case (car iter)
	   (0 (make-array (length result)
			  :element-type (array-element-type sequence)
			  :initial-contents (reverse result)))
	   (1 (make-array (length result)
			  :element-type (array-element-type sequence)
			  :initial-contents result))
	   (2 (reverse result))
	   (3 result))))



#+BUILTIN 
  (defun member (item list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies item (car list) rest)
	   (return-from member list))
	 (setf list (cdr list))
	 (go start))))


#+BUILTIN 
  (defun member-if (predicate list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies-if predicate (car list) rest)
	   (return-from member-if list))
	 (setf list (cdr list))
	 (go start))))

#+BUILTIN 
  (defun member-if-not (predicate list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies-if-not predicate (car list) rest)
	   (return-from member-if list))
	 (setf list (cdr list))
	 (go start))))
  (defun subst (new old tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new old (car tree) rest))
	      (d (apply #'subst new old (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies old tree rest) new tree)))
  (defun subst-if (new predicate tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new predicate (car tree) rest))
	      (d (apply #'subst new predicate (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies-if predicate tree rest) new tree)))
  (defun subst-if-not (new predicate tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new predicate (car tree) rest))
	      (d (apply #'subst new predicate (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies-if-not predicate tree rest) new tree)))
  (defun nsubst (new old tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new old (car tree) rest))
	  (setf (cdr tree) (apply #'subst new old (cdr tree) rest))
	  tree)
	(if (apply #'satisfies old tree rest) new tree)))
  (defun nsubst-if (new predicate tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new predicate (car tree) rest))
	  (setf (cdr tree) (apply #'subst new predicate (cdr tree) rest))
	  tree)
	(if (apply #'satisfies-if predicate tree rest) new tree)))
  (defun nsubst-if-not (new predicate tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new predicate (car tree) rest))
	  (setf (cdr tree) (apply #'subst new predicate (cdr tree) rest))
	  tree)
	(if (apply #'satisfies-if-not predicate tree rest) new tree)))

#+BUILTIN 
  (defun assoc (item alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies item (car elem) rest)
	(return-from assoc elem))))
  (defun assoc-if (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if predicate (car elem) rest)
	(return-from assoc-if elem))))
  (defun assoc-if-not (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if-not predicate (car elem) rest)
	(return-from assoc-if-not elem))))
  (defun rassoc (item alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies item (cdr elem) rest)
	(return-from rassoc elem))))
  (defun rassoc-if (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if predicate (cdr elem) rest)
	(return-from rassoc-if elem))))
  (defun rassoc-if-not (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if-not predicate (cdr elem) rest)
	(return-from rassoc-if-not elem))))
  (defun adjoin (item list &rest rest)
    (dolist (elem list (cons item list))
      (when (apply #'satisfies item elem rest)
	(return-from adjoin list))))
  (defun set-exclusive-or (list-1 list-2 &rest rest &key key)
    (let ((result nil))
      (dolist (item list-1)
	(unless (apply #'member (if key (funcall key item) item) list-2 rest)
	  (push item result)))
      (dolist (item list-2)
	(block matches
	  (dolist (elem list-1)
	    (when (apply #'satisfies
			 (if key (funcall key elem) elem) item rest)
	      (return-from matches)))
	  (push item result)))
      result))
  (defun nset-exclusive-or (list-1 list-2 &rest rest &key key)
    (let ((result nil)
	  (list nil)
	  (item nil))
      (tagbody
       start-1
	 (unless list-1 (go start-2))
	 (setf item (car list-1))
	 (setf list list-2)
	 (setf prev nil)
       start-1-in
	 (unless list (go end-1-in))
	 (let ((elem (if key (funcall key (car list)) (car list))))
	   (when (apply #'satisfies item (if key (funcall key elem) elem) rest)
	     (if prev
		 (setf (cdr prev) (cdr list))
		 (setf list-2 (cdr list)))
	     (setf list-1 (cdr list-1))
	     (go start-1)))
	 (setf prev list)
	 (setf list (cdr list))
	 (go start-1-in)
       end-1-in
	 (setf item (cdr list-1))
	 (setf (cdr list-1) result)
	 (unless result (setf end list-1))
	 (setf result list-1)
	 (setf list-1 item)
	 (go start-1)
       start-2
	 (return-from nset-exclusive-or
	   (if end (progn (setf (cdr end) list-2) result) list-2)))))
  (defun fill (sequence item &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (seq-set sequence iter item)
	   (seq-next iter)
	   (go start))))
    sequence)
  (defun every (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (unless (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every nil))
	   (mapc #'seq-next iters)
	   (go start))))
    t)
  (defun some (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (let ((result (apply predicate (mapcar #'seq-ref sequences iters))))
	     (when result (return-from some result)))
	   (mapc #'seq-next iters)
	   (go start)))))
  (defun notevery (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (unless (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every t))
	   (mapc #'seq-next iters)
	   (go start)))))
  (defun notany (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (when (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every nil))
	   (mapc #'seq-next iters)
	   (go start))))
    t)
#+(BUILTIN BORKEN)
  (defun map-into (result-sequence function &rest sequences)
    (let ((result-iter (seq-start result-sequence))
	  (iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (seq-set result-sequence result-iter
		    (apply function (mapcar #'seq-ref sequences iters)))
	   (seq-next result-iter)
	   (mapc #'seq-next iters)
	   (go start))))
    result-sequence)
#+(BUILTIN BORKEN)
  (defun reduce (function sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (if (apply #'seq-end-p sequence iter rest)
	  (funcall function)
	  (let ((elem (seq-ref sequence iter)))
	    (seq-next iter)
	    (unless (apply #'seq-end-p sequence iter rest)
	      (tagbody
	       start
		 (setq elem (funcall function elem (seq-ref sequence iter)))
		 (seq-next iter)
		 (unless (apply #'seq-end-p sequence iter rest)
		   (go start))))
	    elem))))
  (defun count (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies item (seq-ref sequence iter) rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))
  (defun count-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if predicate (seq-ref sequence iter) rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))
  (defun count-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if-not predicate (seq-ref sequence iter)
			rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))

#+BUILTIN 
  (defun find (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies item elem rest)
	       (return-from find elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun find-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies-if predicate elem rest)
	       (return-from find-if elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun find-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies-if-not predicate elem rest)
	       (return-from find-if-not elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun position (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies item (seq-ref sequence iter) rest)
	     (return-from position (seq-position iter)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun position-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if predicate (seq-ref sequence iter) rest)
	     (return-from position-if (seq-position iter)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN
  (defun position-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if-not predicate (seq-ref sequence iter)
			rest)
	     (return-from position-if-not (seq-position iter)))
	   (seq-next iter)
	   (go start)))))
  (defun remove (item sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies item elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
  (defun remove-if (predicate sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies-if predicate elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
  (defun remove-if-not (predicate sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies-if-not predicate elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
)                     


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:20181 **********************/
:-lisp_compile_to_prolog(pkg_sys,[flet,[[satisfies,[object,elem,'&key',key,test,'test-not'],['let*',[[zi,[if,key,[funcall,key,elem],elem]],[r,[funcall,[or,test,'test-not',function(eql)],object,zi]]],[if,'test-not',[not,r],r]]],['satisfies-if',[predicate,elem,'&key',key],[funcall,predicate,[if,key,[funcall,key,elem],elem]]],['satisfies-if-not',[predicate,elem,'&key',key],[not,[funcall,predicate,[if,key,[funcall,key,elem],elem]]]],['seq-start',[sequence,'&key',[start,0],end,'from-end'],[if,[listp,sequence],[if,'from-end',[let,[[acc,[]],[sequence,[nthcdr,start,sequence]]],[tagbody,start,[when,[and,sequence,[or,[not,end],[<,start,end]]],[push,sequence,acc],[setf,sequence,[cdr,sequence]],[setf,start,[+,1,start]],[go,start]]],[list,3,acc,start]],[list,2,[nthcdr,start,sequence],start]],[if,'from-end',[cons,1,[-,end,1]],[cons,0,start]]]],['seq-position',[iter],[case,[car,iter],[[0,1],[cdr,iter]],[t,[caddr,iter]]]],['seq-next',[iter],[case,[car,iter],[0,[setf,[cdr,iter],[+,1,[cdr,iter]]]],[1,[setf,[cdr,iter],[-,[cdr,iter],1]]],[2,[setf,[cadr,iter],[cdadr,iter]],[setf,[caddr,iter],[+,1,[caddr,iter]]]],[t,[setf,[cadr,iter],[cdadr,iter]],[setf,[caddr,iter],[-,[caddr,iter],1]]]]],['seq-ref',[sequence,iter],[case,[car,iter],[[0,1],[aref,sequence,[cdr,iter]]],[2,[caadr,iter]],[t,[caaadr,iter]]]],['seq-set',[sequence,iter,value],[case,[car,iter],[[0,1],[setf,[aref,sequence,[cdr,iter]],value]],[2,[setf,[caadr,iter],value]],[t,[setf,[caaadr,iter],value]]]],['seq-end-p',[sequence,iter,'&key',start,end,'from-end'],[case,[car,iter],[0,[or,[=,[cdr,iter],[length,sequence]],[and,end,[=,end,[cdr,iter]]]]],[1,[<,[cdr,iter],start]],[2,[or,[null,[cadr,iter]],[and,end,[=,end,[caddr,iter]]]]],[t,[or,[null,[cadr,iter]],[<,[caddr,iter],start]]]]],['seq-result',[sequence,iter,result],[case,[car,iter],[0,['make-array',[length,result],':element-type',['array-element-type',sequence],':initial-contents',[reverse,result]]],[1,['make-array',[length,result],':element-type',['array-element-type',sequence],':initial-contents',result]],[2,[reverse,result]],[3,result]]]],[defun,subst,[new,old,tree,'&rest',rest],[if,[consp,tree],[let,[[a,[apply,function(subst),new,old,[car,tree],rest]],[d,[apply,function(subst),new,old,[cdr,tree],rest]]],[if,[and,[eq,a,[car,tree]],[eq,d,[cdr,tree]]],tree,[cons,a,d]]],[if,[apply,function(satisfies),old,tree,rest],new,tree]]],[defun,'subst-if',[new,predicate,tree,'&rest',rest],[if,[consp,tree],[let,[[a,[apply,function(subst),new,predicate,[car,tree],rest]],[d,[apply,function(subst),new,predicate,[cdr,tree],rest]]],[if,[and,[eq,a,[car,tree]],[eq,d,[cdr,tree]]],tree,[cons,a,d]]],[if,[apply,function('satisfies-if'),predicate,tree,rest],new,tree]]],[defun,'subst-if-not',[new,predicate,tree,'&rest',rest],[if,[consp,tree],[let,[[a,[apply,function(subst),new,predicate,[car,tree],rest]],[d,[apply,function(subst),new,predicate,[cdr,tree],rest]]],[if,[and,[eq,a,[car,tree]],[eq,d,[cdr,tree]]],tree,[cons,a,d]]],[if,[apply,function('satisfies-if-not'),predicate,tree,rest],new,tree]]],[defun,nsubst,[new,old,tree,'&rest',rest],[if,[consp,tree],[progn,[setf,[car,tree],[apply,function(subst),new,old,[car,tree],rest]],[setf,[cdr,tree],[apply,function(subst),new,old,[cdr,tree],rest]],tree],[if,[apply,function(satisfies),old,tree,rest],new,tree]]],[defun,'nsubst-if',[new,predicate,tree,'&rest',rest],[if,[consp,tree],[progn,[setf,[car,tree],[apply,function(subst),new,predicate,[car,tree],rest]],[setf,[cdr,tree],[apply,function(subst),new,predicate,[cdr,tree],rest]],tree],[if,[apply,function('satisfies-if'),predicate,tree,rest],new,tree]]],[defun,'nsubst-if-not',[new,predicate,tree,'&rest',rest],[if,[consp,tree],[progn,[setf,[car,tree],[apply,function(subst),new,predicate,[car,tree],rest]],[setf,[cdr,tree],[apply,function(subst),new,predicate,[cdr,tree],rest]],tree],[if,[apply,function('satisfies-if-not'),predicate,tree,rest],new,tree]]],[defun,'assoc-if',[predicate,alist,'&rest',rest],[dolist,[elem,alist],[when,[apply,function('satisfies-if'),predicate,[car,elem],rest],['return-from','assoc-if',elem]]]],[defun,'assoc-if-not',[predicate,alist,'&rest',rest],[dolist,[elem,alist],[when,[apply,function('satisfies-if-not'),predicate,[car,elem],rest],['return-from','assoc-if-not',elem]]]],[defun,rassoc,[item,alist,'&rest',rest],[dolist,[elem,alist],[when,[apply,function(satisfies),item,[cdr,elem],rest],['return-from',rassoc,elem]]]],[defun,'rassoc-if',[predicate,alist,'&rest',rest],[dolist,[elem,alist],[when,[apply,function('satisfies-if'),predicate,[cdr,elem],rest],['return-from','rassoc-if',elem]]]],[defun,'rassoc-if-not',[predicate,alist,'&rest',rest],[dolist,[elem,alist],[when,[apply,function('satisfies-if-not'),predicate,[cdr,elem],rest],['return-from','rassoc-if-not',elem]]]],[defun,adjoin,[item,list,'&rest',rest],[dolist,[elem,list,[cons,item,list]],[when,[apply,function(satisfies),item,elem,rest],['return-from',adjoin,list]]]],[defun,'set-exclusive-or',['list-1','list-2','&rest',rest,'&key',key],[let,[[result,[]]],[dolist,[item,'list-1'],[unless,[apply,function(member),[if,key,[funcall,key,item],item],'list-2',rest],[push,item,result]]],[dolist,[item,'list-2'],[block,matches,[dolist,[elem,'list-1'],[when,[apply,function(satisfies),[if,key,[funcall,key,elem],elem],item,rest],['return-from',matches]]],[push,item,result]]],result]],[defun,'nset-exclusive-or',['list-1','list-2','&rest',rest,'&key',key],[let,[[result,[]],[list,[]],[item,[]]],[tagbody,'start-1',[unless,'list-1',[go,'start-2']],[setf,item,[car,'list-1']],[setf,list,'list-2'],[setf,prev,[]],'start-1-in',[unless,list,[go,'end-1-in']],[let,[[elem,[if,key,[funcall,key,[car,list]],[car,list]]]],[when,[apply,function(satisfies),item,[if,key,[funcall,key,elem],elem],rest],[if,prev,[setf,[cdr,prev],[cdr,list]],[setf,'list-2',[cdr,list]]],[setf,'list-1',[cdr,'list-1']],[go,'start-1']]],[setf,prev,list],[setf,list,[cdr,list]],[go,'start-1-in'],'end-1-in',[setf,item,[cdr,'list-1']],[setf,[cdr,'list-1'],result],[unless,result,[setf,end,'list-1']],[setf,result,'list-1'],[setf,'list-1',item],[go,'start-1'],'start-2',['return-from','nset-exclusive-or',[if,end,[progn,[setf,[cdr,end],'list-2'],result],'list-2']]]]],[defun,fill,[sequence,item,'&rest',rest],[let,[[iter,[apply,function('seq-start'),sequence,rest]]],[tagbody,start,[unless,[apply,function('seq-end-p'),sequence,iter,rest],['seq-set',sequence,iter,item],['seq-next',iter],[go,start]]]],sequence],[defun,every,[predicate,'&rest',sequences],[let,[[iters,[mapcar,function('seq-start'),sequences]]],[tagbody,start,[unless,['some-list-2',function('seq-end-p'),sequences,iters],[unless,[apply,predicate,[mapcar,function('seq-ref'),sequences,iters]],['return-from',every,[]]],[mapc,function('seq-next'),iters],[go,start]]]],t],[defun,some,[predicate,'&rest',sequences],[let,[[iters,[mapcar,function('seq-start'),sequences]]],[tagbody,start,[unless,['some-list-2',function('seq-end-p'),sequences,iters],[let,[[result,[apply,predicate,[mapcar,function('seq-ref'),sequences,iters]]]],[when,result,['return-from',some,result]]],[mapc,function('seq-next'),iters],[go,start]]]]],[defun,notevery,[predicate,'&rest',sequences],[let,[[iters,[mapcar,function('seq-start'),sequences]]],[tagbody,start,[unless,['some-list-2',function('seq-end-p'),sequences,iters],[unless,[apply,predicate,[mapcar,function('seq-ref'),sequences,iters]],['return-from',every,t]],[mapc,function('seq-next'),iters],[go,start]]]]],[defun,notany,[predicate,'&rest',sequences],[let,[[iters,[mapcar,function('seq-start'),sequences]]],[tagbody,start,[unless,['some-list-2',function('seq-end-p'),sequences,iters],[when,[apply,predicate,[mapcar,function('seq-ref'),sequences,iters]],['return-from',every,[]]],[mapc,function('seq-next'),iters],[go,start]]]],t],[defun,count,[item,sequence,'&rest',rest],[let,[[iter,[apply,function('seq-start'),sequence,rest]],[count,0]],[tagbody,start,[unless,[apply,function('seq-end-p'),sequence,iter,rest],[when,[apply,function(satisfies),item,['seq-ref',sequence,iter],rest],[setf,count,[+,1,count]]],['seq-next',iter],[go,start]]],count]],[defun,'count-if',[predicate,sequence,'&rest',rest],[let,[[iter,[apply,function('seq-start'),sequence,rest]],[count,0]],[tagbody,start,[unless,[apply,function('seq-end-p'),sequence,iter,rest],[when,[apply,function('satisfies-if'),predicate,['seq-ref',sequence,iter],rest],[setf,count,[+,1,count]]],['seq-next',iter],[go,start]]],count]],[defun,'count-if-not',[predicate,sequence,'&rest',rest],[let,[[iter,[apply,function('seq-start'),sequence,rest]],[count,0]],[tagbody,start,[unless,[apply,function('seq-end-p'),sequence,iter,rest],[when,[apply,function('satisfies-if-not'),predicate,['seq-ref',sequence,iter],rest],[setf,count,[+,1,count]]],['seq-next',iter],[go,start]]],count]],[defun,remove,[item,sequence,'&rest',rest,'&key',count],[let,[[iter,[apply,function('seq-start'),sequence,rest]],[result,[]]],[tagbody,start,[unless,[apply,function('seq-end-p'),sequence,iter,rest],[let,[[elem,['seq-ref',sequence,iter]]],[unless,[and,[apply,function(satisfies),item,elem,rest],[or,[not,count],[not,[minusp,[decf,count]]]]],[push,elem,result]]],['seq-next',iter],[go,start]]],['seq-result',sequence,iter,result]]],[defun,'remove-if',[predicate,sequence,'&rest',rest,'&key',count],[let,[[iter,[apply,function('seq-start'),sequence,rest]],[result,[]]],[tagbody,start,[unless,[apply,function('seq-end-p'),sequence,iter,rest],[let,[[elem,['seq-ref',sequence,iter]]],[unless,[and,[apply,function('satisfies-if'),predicate,elem,rest],[or,[not,count],[not,[minusp,[decf,count]]]]],[push,elem,result]]],['seq-next',iter],[go,start]]],['seq-result',sequence,iter,result]]],[defun,'remove-if-not',[predicate,sequence,'&rest',rest,'&key',count],[let,[[iter,[apply,function('seq-start'),sequence,rest]],[result,[]]],[tagbody,start,[unless,[apply,function('seq-end-p'),sequence,iter,rest],[let,[[elem,['seq-ref',sequence,iter]]],[unless,[and,[apply,function('satisfies-if-not'),predicate,elem,rest],[or,[not,count],[not,[minusp,[decf,count]]]]],[push,elem,result]]],['seq-next',iter],[go,start]]],['seq-result',sequence,iter,result]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       satisfies,
					       kw_function,
					       f_satisfies)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_satisfies_if,
					       kw_function,
					       f_sys_satisfies_if)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_satisfies_if_not,
					       kw_function,
					       f_sys_satisfies_if_not)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_seq_start,
					       kw_function,
					       f_sys_seq_start)).
*/
/*
% macroexpand:-[push,sequence,sys_acc].
*/
/*
% into:-[setq,sys_acc,[cons,sequence,sys_acc]].
*/
/*
% macroexpand:-[push,sequence,sys_acc].
*/
/*
% into:-[setq,sys_acc,[cons,sequence,sys_acc]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_seq_position,
					       kw_function,
					       f_sys_seq_position)).
*/
/*
% case:-[[[0,1],[cdr,sys_iter]],[t,[caddr,sys_iter]]].
*/
/*
% conds:-[[[sys_memq,_140818,[quote,[0,1]]],[progn,[cdr,sys_iter]]],[t,[progn,[caddr,sys_iter]]]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_seq_next,
					       kw_function,
					       f_sys_seq_next)).
*/
/*
% case:-[[0,[setf,[cdr,sys_iter],[+,1,[cdr,sys_iter]]]],[1,[setf,[cdr,sys_iter],[-,[cdr,sys_iter],1]]],[2,[setf,[cadr,sys_iter],[cdadr,sys_iter]],[setf,[caddr,sys_iter],[+,1,[caddr,sys_iter]]]],[t,[setf,[cadr,sys_iter],[cdadr,sys_iter]],[setf,[caddr,sys_iter],[-,[caddr,sys_iter],1]]]].
*/
/*
% conds:-[[[eq,_147068,[quote,0]],[progn,[setf,[cdr,sys_iter],[+,1,[cdr,sys_iter]]]]],[[eq,_147068,[quote,1]],[progn,[setf,[cdr,sys_iter],[-,[cdr,sys_iter],1]]]],[[eq,_147068,[quote,2]],[progn,[setf,[cadr,sys_iter],[cdadr,sys_iter]],[setf,[caddr,sys_iter],[+,1,[caddr,sys_iter]]]]],[t,[progn,[setf,[cadr,sys_iter],[cdadr,sys_iter]],[setf,[caddr,sys_iter],[-,[caddr,sys_iter],1]]]]].
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_112016,_111450],[sys_iter,_112016,_111450]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_162218,_125036],[sys_iter,_162218,_125036]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_293564,_293392],[sys_iter,_293564,_293392]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_112776,_112748],[sys_iter,_112776,_112748]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_202254,_202082],[sys_iter,_202254,_202082]),setf_inverse_op(cadr,sys_pf_set_cadr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_296526,_296354],[sys_iter,_296526,_296354]),setf_inverse_op(cadr,sys_pf_set_cadr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_168474,_168302],[sys_iter,_168474,_168302]),setf_inverse_op(caddr,sys_pf_set_caddr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_263210,_263038],[sys_iter,_263210,_263038]),setf_inverse_op(caddr,sys_pf_set_caddr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_113918,_113890],[sys_iter,_113918,_113890]),setf_inverse_op(cadr,sys_pf_set_cadr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_165518,_165346],[sys_iter,_165518,_165346]),setf_inverse_op(cadr,sys_pf_set_cadr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_278468,_278296],[sys_iter,_278468,_278296]),setf_inverse_op(caddr,sys_pf_set_caddr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_108072,[],[],true),append([sys_iter],[_114190,_114162],[sys_iter,_114190,_114162]),setf_inverse_op(caddr,sys_pf_set_caddr))).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_seq_ref,
					       kw_function,
					       f_sys_seq_ref)).
*/
/*
% case:-[[[0,1],[aref,sequence,[cdr,sys_iter]]],[2,[caadr,sys_iter]],[t,[caaadr,sys_iter]]].
*/
/*
% conds:-[[[sys_memq,_152280,[quote,[0,1]]],[progn,[aref,sequence,[cdr,sys_iter]]]],[[eq,_152280,[quote,2]],[progn,[caadr,sys_iter]]],[t,[progn,[caaadr,sys_iter]]]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_seq_set,
					       kw_function,
					       f_sys_seq_set)).
*/
/*
% case:-[[[0,1],[setf,[aref,sequence,[cdr,sys_iter]],sys_value]],[2,[setf,[caadr,sys_iter],sys_value]],[t,[setf,[caaadr,sys_iter],sys_value]]].
*/
/*
% conds:-[[[sys_memq,_157480,[quote,[0,1]]],[progn,[setf,[aref,sequence,[cdr,sys_iter]],sys_value]]],[[eq,_157480,[quote,2]],[progn,[setf,[caadr,sys_iter],sys_value]]],[t,[progn,[setf,[caaadr,sys_iter],sys_value]]]].
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_111592,[[[cdr,sys_iter]]],[[[cdr,sys_iter]]],true),append([sequence,[[cdr,sys_iter]]],[_114900,_114872],[sequence,[[cdr,sys_iter]],_114900,_114872]),setf_inverse_op(aref,svref))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_111592,[[[cdr,sys_iter]]],[[[cdr,sys_iter]]],true),append([sequence,[[cdr,sys_iter]]],[_177614,_177442],[sequence,[[cdr,sys_iter]],_177614,_177442]),setf_inverse_op(aref,svref))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_111592,[],[],true),append([sys_iter],[_115062,_115034],[sys_iter,_115062,_115034]),setf_inverse_op(caadr,sys_pf_set_caadr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_111592,[],[],true),append([sys_iter],[_169806,_169634],[sys_iter,_169806,_169634]),setf_inverse_op(caadr,sys_pf_set_caadr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_111592,[],[],true),append([sys_iter],[_288510,_288338],[sys_iter,_288510,_288338]),setf_inverse_op(caaadr,sys_pf_set_caaadr))).
*/
/*
:-side_effect((compile_each([fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_111592,[],[],true),append([sys_iter],[_115230,_115202],[sys_iter,_115230,_115202]),setf_inverse_op(caaadr,sys_pf_set_caaadr))).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_seq_end_p,
					       kw_function,
					       f_sys_seq_end_p)).
*/
/*
% case:-[[0,[or,[=,[cdr,sys_iter],[length,sequence]],[and,sys_end,[=,sys_end,[cdr,sys_iter]]]]],[1,[<,[cdr,sys_iter],sys_start]],[2,[or,[null,[cadr,sys_iter]],[and,sys_end,[=,sys_end,[caddr,sys_iter]]]]],[t,[or,[null,[cadr,sys_iter]],[<,[caddr,sys_iter],sys_start]]]].
*/
/*
% conds:-[[[eq,_364466,[quote,0]],[progn,[or,[=,[cdr,sys_iter],[length,sequence]],[and,sys_end,[=,sys_end,[cdr,sys_iter]]]]]],[[eq,_364466,[quote,1]],[progn,[<,[cdr,sys_iter],sys_start]]],[[eq,_364466,[quote,2]],[progn,[or,[null,[cadr,sys_iter]],[and,sys_end,[=,sys_end,[caddr,sys_iter]]]]]],[t,[progn,[or,[null,[cadr,sys_iter]],[<,[caddr,sys_iter],sys_start]]]]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_seq_result,
					       kw_function,
					       f_sys_seq_result)).
*/
/*
% case:-[[0,[make_array,[length,sys_result],kw_element_type,[array_element_type,sequence],kw_initial_contents,[reverse,sys_result]]],[1,[make_array,[length,sys_result],kw_element_type,[array_element_type,sequence],kw_initial_contents,sys_result]],[2,[reverse,sys_result]],[3,sys_result]].
*/
/*
% conds:-[[[eq,_172824,[quote,0]],[progn,[make_array,[length,sys_result],kw_element_type,[array_element_type,sequence],kw_initial_contents,[reverse,sys_result]]]],[[eq,_172824,[quote,1]],[progn,[make_array,[length,sys_result],kw_element_type,[array_element_type,sequence],kw_initial_contents,sys_result]]],[[eq,_172824,[quote,2]],[progn,[reverse,sys_result]]],[[eq,_172824,[quote,3]],[progn,sys_result]]].
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _158946, [], [], true), append([sys_tree], [CAR505, CAR504], [sys_tree, CAR505, CAR504]), setf_inverse_op(car, rplaca))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _158836, [], [], true), append([sys_tree], [CAR505, CAR504], [sys_tree, CAR505, CAR504]), setf_inverse_op(car, rplaca))).
*/
/*
:-side_effect((compile_each([[fbound(satisfies,kw_function)=function(f_satisfies1),fbound(sys_satisfies_if,kw_function)=function(f_sys_satisfies_if1),fbound(sys_satisfies_if_not,kw_function)=function(f_sys_satisfies_if_not1),fbound(sys_seq_start,kw_function)=function(f_sys_seq_start1),fbound(sys_seq_position,kw_function)=function(f_sys_seq_position1),fbound(sys_seq_next,kw_function)=function(f_sys_seq_next1),fbound(sys_seq_ref,kw_function)=function(f_sys_seq_ref1),fbound(sys_seq_set,kw_function)=function(f_sys_seq_set1),fbound(sys_seq_end_p,kw_function)=function(f_sys_seq_end_p1),fbound(sys_seq_result,kw_function)=function(f_sys_seq_result1)],fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_158836,[],[],true),append([sys_tree],[_298822,_298650],[sys_tree,_298822,_298650]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([[fbound(satisfies,kw_function)=function(f_satisfies1),fbound(sys_satisfies_if,kw_function)=function(f_sys_satisfies_if1),fbound(sys_satisfies_if_not,kw_function)=function(f_sys_satisfies_if_not1),fbound(sys_seq_start,kw_function)=function(f_sys_seq_start1),fbound(sys_seq_position,kw_function)=function(f_sys_seq_position1),fbound(sys_seq_next,kw_function)=function(f_sys_seq_next1),fbound(sys_seq_ref,kw_function)=function(f_sys_seq_ref1),fbound(sys_seq_set,kw_function)=function(f_sys_seq_set1),fbound(sys_seq_end_p,kw_function)=function(f_sys_seq_end_p1),fbound(sys_seq_result,kw_function)=function(f_sys_seq_result1)],fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_158836,[],[],true),append([sys_tree],[_162962,_162934],[sys_tree,_162962,_162934]),setf_inverse_op(cdr,rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _168078, [], [], true), append([sys_tree], [CAR543, CAR542], [sys_tree, CAR543, CAR542]), setf_inverse_op(car, rplaca))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _168078, [], [], true), append([sys_tree], [CAR543, CAR542], [sys_tree, CAR543, CAR542]), setf_inverse_op(car, rplaca))).
*/
/*
:-side_effect((compile_each([[fbound(satisfies,kw_function)=function(f_satisfies1),fbound(sys_satisfies_if,kw_function)=function(f_sys_satisfies_if1),fbound(sys_satisfies_if_not,kw_function)=function(f_sys_satisfies_if_not1),fbound(sys_seq_start,kw_function)=function(f_sys_seq_start1),fbound(sys_seq_position,kw_function)=function(f_sys_seq_position1),fbound(sys_seq_next,kw_function)=function(f_sys_seq_next1),fbound(sys_seq_ref,kw_function)=function(f_sys_seq_ref1),fbound(sys_seq_set,kw_function)=function(f_sys_seq_set1),fbound(sys_seq_end_p,kw_function)=function(f_sys_seq_end_p1),fbound(sys_seq_result,kw_function)=function(f_sys_seq_result1)],fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_168010,[],[],true),append([sys_tree],[_172150,_172122],[sys_tree,_172150,_172122]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([[fbound(satisfies,kw_function)=function(f_satisfies1),fbound(sys_satisfies_if,kw_function)=function(f_sys_satisfies_if1),fbound(sys_satisfies_if_not,kw_function)=function(f_sys_satisfies_if_not1),fbound(sys_seq_start,kw_function)=function(f_sys_seq_start1),fbound(sys_seq_position,kw_function)=function(f_sys_seq_position1),fbound(sys_seq_next,kw_function)=function(f_sys_seq_next1),fbound(sys_seq_ref,kw_function)=function(f_sys_seq_ref1),fbound(sys_seq_set,kw_function)=function(f_sys_seq_set1),fbound(sys_seq_end_p,kw_function)=function(f_sys_seq_end_p1),fbound(sys_seq_result,kw_function)=function(f_sys_seq_result1)],fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_168010,[],[],true),append([sys_tree],[_284464,_284292],[sys_tree,_284464,_284292]),setf_inverse_op(cdr,rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _176904, [], [], true), append([sys_tree], [CAR581, CAR580], [sys_tree, CAR581, CAR580]), setf_inverse_op(car, rplaca))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _176830, [], [], true), append([sys_tree], [CAR581, CAR580], [sys_tree, CAR581, CAR580]), setf_inverse_op(car, rplaca))).
*/
/*
:-side_effect((compile_each([[fbound(satisfies,kw_function)=function(f_satisfies1),fbound(sys_satisfies_if,kw_function)=function(f_sys_satisfies_if1),fbound(sys_satisfies_if_not,kw_function)=function(f_sys_satisfies_if_not1),fbound(sys_seq_start,kw_function)=function(f_sys_seq_start1),fbound(sys_seq_position,kw_function)=function(f_sys_seq_position1),fbound(sys_seq_next,kw_function)=function(f_sys_seq_next1),fbound(sys_seq_ref,kw_function)=function(f_sys_seq_ref1),fbound(sys_seq_set,kw_function)=function(f_sys_seq_set1),fbound(sys_seq_end_p,kw_function)=function(f_sys_seq_end_p1),fbound(sys_seq_result,kw_function)=function(f_sys_seq_result1)],fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_176830,[],[],true),append([sys_tree],[_321612,_321440],[sys_tree,_321612,_321440]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([[fbound(satisfies,kw_function)=function(f_satisfies1),fbound(sys_satisfies_if,kw_function)=function(f_sys_satisfies_if1),fbound(sys_satisfies_if_not,kw_function)=function(f_sys_satisfies_if_not1),fbound(sys_seq_start,kw_function)=function(f_sys_seq_start1),fbound(sys_seq_position,kw_function)=function(f_sys_seq_position1),fbound(sys_seq_next,kw_function)=function(f_sys_seq_next1),fbound(sys_seq_ref,kw_function)=function(f_sys_seq_ref1),fbound(sys_seq_set,kw_function)=function(f_sys_seq_set1),fbound(sys_seq_end_p,kw_function)=function(f_sys_seq_end_p1),fbound(sys_seq_result,kw_function)=function(f_sys_seq_result1)],fbound(sys_all_cdr,kw_function)=function(f_sys_all_cdr1),fbound(sys_all_car,kw_function)=function(f_sys_all_car1),fbound(sys_all_end,kw_function)=function(f_sys_all_end1),fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],_176830,[],[],true),append([sys_tree],[_180956,_180928],[sys_tree,_180956,_180928]),setf_inverse_op(cdr,rplacd))).
*/
/*
% macroexpand:-[push,sys_item,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_item,sys_result]].
*/
/*
% macroexpand:-[push,sys_item,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_item,sys_result]].
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv835, [], [], true), append([sys_prev], [CAR861, CAR860], [sys_prev, CAR861, CAR860]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv835, [], [], true), append([sys_prev], [CAR861, CAR860], [sys_prev, CAR861, CAR860]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv882, [], [], true), append([sys_prev], [CAR908, CAR907], [sys_prev, CAR908, CAR907]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv882, [], [], true), append([sys_prev], [CAR908, CAR907], [sys_prev, CAR908, CAR907]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _247824, [], [], true), append([sys_list_1], [CAR924, CAR923], [sys_list_1, CAR924, CAR923]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _247824, [], [], true), append([sys_list_1], [CAR924, CAR923], [sys_list_1, CAR924, CAR923]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _248416, [], [], true), append([sys_end], [CAR942, CAR941], [sys_end, CAR942, CAR941]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], _248416, [], [], true), append([sys_end], [CAR942, CAR941], [sys_end, CAR942, CAR941]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv967, [], [], true), append([sys_prev], [CAR993, CAR992], [sys_prev, CAR993, CAR992]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)], fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv967, [], [], true), append([sys_prev], [CAR993, CAR992], [sys_prev, CAR993, CAR992]), setf_inverse_op(cdr, rplacd))).
*/
/*
% macroexpand:-[push,sys_elem,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_elem,sys_result]].
*/
/*
% macroexpand:-[push,sys_elem,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_elem,sys_result]].
*/
/*
% macroexpand:-[push,sys_elem,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_elem,sys_result]].
*/
/*
% macroexpand:-[push,sys_elem,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_elem,sys_result]].
*/
/*
% macroexpand:-[push,sys_elem,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_elem,sys_result]].
*/
/*
% macroexpand:-[push,sys_elem,sys_result].
*/
/*
% into:-[setq,sys_result,[cons,sys_elem,sys_result]].
*/
wl:lambda_def(defun, satisfies, f_satisfies1, [sys_object, sys_elem, c38_key, key, sys_test, sys_test_not], [[let_xx, [[sys_zi, [if, key, [funcall, key, sys_elem], sys_elem]], [sys_r, [funcall, [or, sys_test, sys_test_not, function(eql)], sys_object, sys_zi]]], [if, sys_test_not, [not, sys_r], sys_r]]]).
wl:arglist_info(satisfies, f_satisfies1, [sys_object, sys_elem, c38_key, key, sys_test, sys_test_not], arginfo{all:[sys_object, sys_elem], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key, sys_test, sys_test_not], names:[sys_object, sys_elem, key, sys_test, sys_test_not], opt:0, req:[sys_object, sys_elem], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_satisfies1).

/*

### Compiled Function: `CL:SATISFIES` 
*/
f_satisfies1(Object_In, Elem_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_object, Object_In), bv(sys_elem, Elem_In), bv(key, Key_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	catch(( ( get_var(GEnv, key, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, key, Key_Get18),
		      get_var(GEnv, sys_elem, Elem_Get),
		      f_apply(Key_Get18, [Elem_Get], TrueResult),
		      Zi_Init=TrueResult
		  ;   get_var(GEnv, sys_elem, Elem_Get20),
		      Zi_Init=Elem_Get20
		  ),
		  LEnv=[bv(sys_zi, Zi_Init)|GEnv],
		  (   get_var(LEnv, sys_test, Test_Get),
		      Test_Get\==[],
		      Apply_Param=Test_Get
		  ->  true
		  ;   (   get_var(LEnv, sys_test_not, Test_not_Get),
			  Test_not_Get\==[],
			  _64974=Test_not_Get
		      ->  true
		      ;   _64974=f_eql
		      ),
		      Apply_Param=_64974
		  ),
		  get_var(LEnv, sys_object, Object_Get),
		  get_var(LEnv, sys_zi, Zi_Get),
		  f_apply(Apply_Param, [Object_Get, Zi_Get], R_Init),
		  LEnv26=[bv(sys_r, R_Init)|LEnv],
		  get_var(LEnv26, sys_test_not, IFTEST34),
		  (   IFTEST34\==[]
		  ->  get_var(LEnv26, sys_r, R_Get),
		      f_not(R_Get, TrueResult39),
		      LetResult25=TrueResult39
		  ;   get_var(LEnv26, sys_r, R_Get38),
		      LetResult25=R_Get38
		  )
		),
		LetResult25=FnResult
	      ),
	      block_exit(satisfies, FnResult),
	      true).
wl:lambda_def(defun, sys_satisfies_if, f_sys_satisfies_if1, [sys_predicate, sys_elem, c38_key, key], [[funcall, sys_predicate, [if, key, [funcall, key, sys_elem], sys_elem]]]).
wl:arglist_info(sys_satisfies_if, f_sys_satisfies_if1, [sys_predicate, sys_elem, c38_key, key], arginfo{all:[sys_predicate, sys_elem], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_predicate, sys_elem, key], opt:0, req:[sys_predicate, sys_elem], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_sys_satisfies_if1).

/*

### Compiled Function: `SYS::SATISFIES-IF` 
*/
f_sys_satisfies_if1(Predicate_In, Elem_In46, RestNKeys43, FnResult42) :-
	GEnv1645=[bv(sys_predicate, Predicate_In), bv(sys_elem, Elem_In46), bv(key, Key_In47)],
	get_kw(Env, RestNKeys43, key, key, Key_In47, []=Key_In47, Key_P44),
	catch(( ( get_var(GEnv1645, key, IFTEST49),
		  get_var(GEnv1645, sys_predicate, Predicate_Get),
		  (   IFTEST49\==[]
		  ->  get_var(GEnv1645, key, Key_Get52),
		      get_var(GEnv1645, sys_elem, Elem_Get53),
		      f_apply(Key_Get52, [Elem_Get53], TrueResult55),
		      CAR1718=TrueResult55
		  ;   get_var(GEnv1645, sys_elem, Elem_Get54),
		      CAR1718=Elem_Get54
		  ),
		  f_apply(Predicate_Get, [CAR1718], Apply_Ret)
		),
		Apply_Ret=FnResult42
	      ),
	      block_exit(sys_satisfies_if, FnResult42),
	      true).
wl:lambda_def(defun, sys_satisfies_if_not, f_sys_satisfies_if_not1, [sys_predicate, sys_elem, c38_key, key], [[not, [funcall, sys_predicate, [if, key, [funcall, key, sys_elem], sys_elem]]]]).
wl:arglist_info(sys_satisfies_if_not, f_sys_satisfies_if_not1, [sys_predicate, sys_elem, c38_key, key], arginfo{all:[sys_predicate, sys_elem], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_predicate, sys_elem, key], opt:0, req:[sys_predicate, sys_elem], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_sys_satisfies_if_not1).

/*

### Compiled Function: `SYS::SATISFIES-IF-NOT` 
*/
f_sys_satisfies_if_not1(Predicate_In61, Elem_In62, RestNKeys59, FnResult58) :-
	GEnv1646=[bv(sys_predicate, Predicate_In61), bv(sys_elem, Elem_In62), bv(key, Key_In63)],
	get_kw(Env, RestNKeys59, key, key, Key_In63, []=Key_In63, Key_P60),
	catch(( ( get_var(GEnv1646, key, IFTEST65),
		  get_var(GEnv1646, sys_predicate, Predicate_Get64),
		  (   IFTEST65\==[]
		  ->  get_var(GEnv1646, key, Key_Get68),
		      get_var(GEnv1646, sys_elem, Elem_Get69),
		      f_apply(Key_Get68, [Elem_Get69], TrueResult71),
		      CAR1719=TrueResult71
		  ;   get_var(GEnv1646, sys_elem, Elem_Get70),
		      CAR1719=Elem_Get70
		  ),
		  f_apply(Predicate_Get64, [CAR1719], Not_Param),
		  f_not(Not_Param, Not_Ret)
		),
		Not_Ret=FnResult58
	      ),
	      block_exit(sys_satisfies_if_not, FnResult58),
	      true).
wl:lambda_def(defun, sys_seq_start, f_sys_seq_start1, [sequence, c38_key, [sys_start, 0], sys_end, sys_from_end], [[if, [listp, sequence], [if, sys_from_end, [let, [[sys_acc, []], [sequence, [nthcdr, sys_start, sequence]]], [tagbody, sys_start, [when, [and, sequence, [or, [not, sys_end], [<, sys_start, sys_end]]], [push, sequence, sys_acc], [setf, sequence, [cdr, sequence]], [setf, sys_start, [+, 1, sys_start]], [go, sys_start]]], [list, 3, sys_acc, sys_start]], [list, 2, [nthcdr, sys_start, sequence], sys_start]], [if, sys_from_end, [cons, 1, [-, sys_end, 1]], [cons, 0, sys_start]]]]).
wl:arglist_info(sys_seq_start, f_sys_seq_start1, [sequence, c38_key, [sys_start, 0], sys_end, sys_from_end], arginfo{all:[sequence], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_start, sys_end, sys_from_end], names:[sequence, sys_start, sys_end, sys_from_end], opt:0, req:[sequence], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_seq_start1).

/*

### Compiled Function: `SYS::SEQ-START` 
*/
f_sys_seq_start1(Sequence_In, RestNKeys75, FnResult74) :-
	GEnv1647=[bv(sequence, Sequence_In), bv(sys_start, Start_In), bv(sys_end, End_In), bv(sys_from_end, From_end_In)],
	get_kw(Env,
	       RestNKeys75,
	       sys_start,
	       sys_start,
	       Start_In,
	       0=Start_In,
	       Start_P),
	get_kw(Env, RestNKeys75, sys_end, sys_end, End_In, []=End_In, End_P),
	get_kw(Env,
	       RestNKeys75,
	       sys_from_end,
	       sys_from_end,
	       From_end_In,
	       []=From_end_In,
	       From_end_P),
	catch(( ( get_var(GEnv1647, sequence, Sequence_Get),
		  (   s3q:is_listp(Sequence_Get)
		  ->  get_var(GEnv1647, sys_from_end, IFTEST87),
		      (   IFTEST87\==[]
		      ->  get_var(GEnv1647, sequence, Sequence_Get94),
			  get_var(GEnv1647, sys_start, Start_Get),
			  f_nthcdr(Start_Get, Sequence_Get94, Sequence_Init),
			  AEnv=[bv(sys_acc, []), bv(sequence, Sequence_Init)|GEnv1647],
			  call_addr_block(AEnv,
					  (push_label(sys_start), get_var(AEnv, sequence, IFTEST118), (IFTEST118\==[]->(get_var(AEnv, sys_end, End_Get121), f_not(End_Get121, FORM1_Res124), FORM1_Res124\==[], TrueResult125=FORM1_Res124->true;get_var(AEnv, sys_end, End_Get123), get_var(AEnv, sys_start, Start_Get122), 'f_<'(Start_Get122, End_Get123, _67110), TrueResult125=_67110), IFTEST116=TrueResult125;IFTEST116=[]), (IFTEST116\==[]->get_var(AEnv, sequence, Sequence_Get127), get_var(AEnv, sys_acc, Acc_Get128), Acc=[Sequence_Get127|Acc_Get128], set_var(AEnv, sys_acc, Acc), get_var(AEnv, sequence, Sequence_Get129), f_cdr(Sequence_Get129, Sequence), set_var(AEnv, sequence, Sequence), get_var(AEnv, sys_start, Start_Get130), 'f_+'(1, Start_Get130, Start), set_var(AEnv, sys_start, Start), goto(sys_start, AEnv), _TBResult=_GORES131;_TBResult=[])),
					  
					  [ addr(addr_tagbody_53_sys_start,
						 sys_start,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sequence, IFTEST99), (IFTEST99\==[]->(get_var(AEnv, sys_end, Not_Param1685), f_not(Not_Param1685, FORM1_Res105), FORM1_Res105\==[], TrueResult106=FORM1_Res105->true;get_var(AEnv, sys_end, End_Get104), get_var(AEnv, sys_start, Start_Get103), 'f_<'(Start_Get103, End_Get104, _67492), TrueResult106=_67492), IFTEST97=TrueResult106;IFTEST97=[]), (IFTEST97\==[]->get_var(AEnv, sequence, Sequence_Get108), get_var(AEnv, sys_acc, Get_var_Ret), Set_var_Ret=[Sequence_Get108|Get_var_Ret], set_var(AEnv, sys_acc, Set_var_Ret), get_var(AEnv, sequence, Sequence_Get110), f_cdr(Sequence_Get110, Cdr_Ret), set_var(AEnv, sequence, Cdr_Ret), get_var(AEnv, sys_start, Start_Get111), 'f_+'(1, Start_Get111, Set_var_Ret1724), set_var(AEnv, sys_start, Set_var_Ret1724), goto(sys_start, AEnv), _67558=_GORES;_67558=[])))
					  ]),
			  get_var(AEnv, sys_acc, Acc_Get134),
			  get_var(AEnv, sys_start, Start_Get135),
			  LetResult91=[3, Acc_Get134, Start_Get135],
			  TrueResult148=LetResult91
		      ;   get_var(GEnv1647, sequence, Sequence_Get137),
			  get_var(GEnv1647, sys_start, Start_Get136),
			  f_nthcdr(Start_Get136, Sequence_Get137, Nthcdr_Ret),
			  get_var(GEnv1647, sys_start, Start_Get138),
			  ElseResult140=[2, Nthcdr_Ret, Start_Get138],
			  TrueResult148=ElseResult140
		      ),
		      _66228=TrueResult148
		  ;   get_var(GEnv1647, sys_from_end, IFTEST141),
		      (   IFTEST141\==[]
		      ->  get_var(GEnv1647, sys_end, End_Get144),
			  'f_-'(End_Get144, 1, CDR),
			  TrueResult146=[1|CDR],
			  ElseResult149=TrueResult146
		      ;   get_var(GEnv1647, sys_start, Start_Get145),
			  ElseResult147=[0|Start_Get145],
			  ElseResult149=ElseResult147
		      ),
		      _66228=ElseResult149
		  )
		),
		_66228=FnResult74
	      ),
	      block_exit(sys_seq_start, FnResult74),
	      true).
wl:lambda_def(defun, sys_seq_position, f_sys_seq_position1, [sys_iter], [[case, [car, sys_iter], [[0, 1], [cdr, sys_iter]], [t, [caddr, sys_iter]]]]).
wl:arglist_info(sys_seq_position, f_sys_seq_position1, [sys_iter], arginfo{all:[sys_iter], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_iter], opt:0, req:[sys_iter], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_seq_position1).

/*

### Compiled Function: `SYS::SEQ-POSITION` 
*/
f_sys_seq_position1(Iter_In, RestNKeys152, FnResult151) :-
	GEnv1651=[bv(sys_iter, Iter_In)],
	catch(( ( get_var(GEnv1651, sys_iter, Iter_Get),
		  f_car(Iter_Get, Key),
		  f_sys_memq(Key, [0, 1], IFTEST156),
		  (   IFTEST156\==[]
		  ->  get_var(GEnv1651, sys_iter, Iter_Get158),
		      f_cdr(Iter_Get158, TrueResult160),
		      _68084=TrueResult160
		  ;   get_var(GEnv1651, sys_iter, Iter_Get159),
		      f_caddr(Iter_Get159, ElseResult161),
		      _68084=ElseResult161
		  )
		),
		_68084=FnResult151
	      ),
	      block_exit(sys_seq_position, FnResult151),
	      true).
wl:lambda_def(defun, sys_seq_next, f_sys_seq_next1, [sys_iter], [[case, [car, sys_iter], [0, [setf, [cdr, sys_iter], [+, 1, [cdr, sys_iter]]]], [1, [setf, [cdr, sys_iter], [-, [cdr, sys_iter], 1]]], [2, [setf, [cadr, sys_iter], [cdadr, sys_iter]], [setf, [caddr, sys_iter], [+, 1, [caddr, sys_iter]]]], [t, [setf, [cadr, sys_iter], [cdadr, sys_iter]], [setf, [caddr, sys_iter], [-, [caddr, sys_iter], 1]]]]]).
wl:arglist_info(sys_seq_next, f_sys_seq_next1, [sys_iter], arginfo{all:[sys_iter], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_iter], opt:0, req:[sys_iter], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_seq_next1).

/*

### Compiled Function: `SYS::SEQ-NEXT` 
*/
f_sys_seq_next1(Iter_In165, RestNKeys164, FnResult163) :-
	GEnv1652=[bv(sys_iter, Iter_In165)],
	catch(( ( get_var(GEnv1652, sys_iter, Iter_Get166),
		  f_car(Iter_Get166, Key167),
		  (   is_eq(Key167, 0)
		  ->  get_var(GEnv1652, sys_iter, Iter_Get173),
		      f_cdr(Iter_Get173, Cdr_Ret1727),
		      'f_+'(1, Cdr_Ret1727, _68584),
		      f_rplacd(Iter_Get173, _68584, TrueResult203),
		      _68388=TrueResult203
		  ;   (   is_eq(Key167, 1)
		      ->  get_var(GEnv1652, sys_iter, Iter_Get179),
			  f_cdr(Iter_Get179, Cdr_Ret1728),
			  'f_-'(Cdr_Ret1728, 1, _68750),
			  f_rplacd(Iter_Get179, _68750, TrueResult201),
			  ElseResult204=TrueResult201
		      ;   (   is_eq(Key167, 2)
			  ->  get_var(GEnv1652, sys_iter, Iter_Get185),
			      f_cdadr(Iter_Get185, Cdadr_Ret),
			      f_sys_pf_set_cadr(Iter_Get185,
						Cdadr_Ret,
						Set_cadr_Ret),
			      get_var(GEnv1652, sys_iter, Iter_Get189),
			      f_caddr(Iter_Get189, Caddr_Ret),
			      'f_+'(1, Caddr_Ret, _69034),
			      f_sys_pf_set_caddr(Iter_Get189,
						 _69034,
						 TrueResult199),
			      ElseResult202=TrueResult199
			  ;   get_var(GEnv1652, sys_iter, Iter_Get193),
			      f_cdadr(Iter_Get193, Cdadr_Ret1732),
			      f_sys_pf_set_cadr(Iter_Get193,
						Cdadr_Ret1732,
						Set_cadr_Ret1733),
			      get_var(GEnv1652, sys_iter, Iter_Get197),
			      f_caddr(Iter_Get197, Caddr_Ret1734),
			      'f_-'(Caddr_Ret1734, 1, _69276),
			      f_sys_pf_set_caddr(Iter_Get197,
						 _69276,
						 ElseResult200),
			      ElseResult202=ElseResult200
			  ),
			  ElseResult204=ElseResult202
		      ),
		      _68388=ElseResult204
		  )
		),
		_68388=FnResult163
	      ),
	      block_exit(sys_seq_next, FnResult163),
	      true).
wl:lambda_def(defun, sys_seq_ref, f_sys_seq_ref1, [sequence, sys_iter], [[case, [car, sys_iter], [[0, 1], [aref, sequence, [cdr, sys_iter]]], [2, [caadr, sys_iter]], [t, [caaadr, sys_iter]]]]).
wl:arglist_info(sys_seq_ref, f_sys_seq_ref1, [sequence, sys_iter], arginfo{all:[sequence, sys_iter], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iter], opt:0, req:[sequence, sys_iter], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_sys_seq_ref1).

/*

### Compiled Function: `SYS::SEQ-REF` 
*/
f_sys_seq_ref1(Sequence_In208, Iter_In209, RestNKeys207, FnResult206) :-
	GEnv1653=[bv(sequence, Sequence_In208), bv(sys_iter, Iter_In209)],
	catch(( ( get_var(GEnv1653, sys_iter, Iter_Get210),
		  f_car(Iter_Get210, Key211),
		  f_sys_memq(Key211, [0, 1], IFTEST212),
		  (   IFTEST212\==[]
		  ->  get_var(GEnv1653, sequence, Sequence_Get214),
		      get_var(GEnv1653, sys_iter, Iter_Get215),
		      f_cdr(Iter_Get215, Cdr_Ret1735),
		      f_aref(Sequence_Get214, [Cdr_Ret1735], TrueResult223),
		      _69598=TrueResult223
		  ;   (   is_eq(Key211, 2)
		      ->  get_var(GEnv1653, sys_iter, Iter_Get219),
			  f_caadr(Iter_Get219, TrueResult221),
			  ElseResult224=TrueResult221
		      ;   get_var(GEnv1653, sys_iter, Iter_Get220),
			  f_caaadr(Iter_Get220, ElseResult222),
			  ElseResult224=ElseResult222
		      ),
		      _69598=ElseResult224
		  )
		),
		_69598=FnResult206
	      ),
	      block_exit(sys_seq_ref, FnResult206),
	      true).
wl:lambda_def(defun, sys_seq_set, f_sys_seq_set1, [sequence, sys_iter, sys_value], [[case, [car, sys_iter], [[0, 1], [setf, [aref, sequence, [cdr, sys_iter]], sys_value]], [2, [setf, [caadr, sys_iter], sys_value]], [t, [setf, [caaadr, sys_iter], sys_value]]]]).
wl:arglist_info(sys_seq_set, f_sys_seq_set1, [sequence, sys_iter, sys_value], arginfo{all:[sequence, sys_iter, sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iter, sys_value], opt:0, req:[sequence, sys_iter, sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_sys_seq_set1).

/*

### Compiled Function: `SYS::SEQ-SET` 
*/
f_sys_seq_set1(Sequence_In228, Iter_In229, Value_In, RestNKeys227, FnResult226) :-
	GEnv1654=[bv(sequence, Sequence_In228), bv(sys_iter, Iter_In229), bv(sys_value, Value_In)],
	catch(( ( get_var(GEnv1654, sys_iter, Iter_Get231),
		  f_car(Iter_Get231, Key232),
		  f_sys_memq(Key232, [0, 1], IFTEST233),
		  (   IFTEST233\==[]
		  ->  get_var(GEnv1654, sequence, Sequence_Get237),
		      get_var(GEnv1654, sys_iter, Iter_Get239),
		      get_var(GEnv1654, sys_value, Value_Get),
		      f_cdr(Iter_Get239, Cdr_Ret1736),
		      f_svref(Sequence_Get237,
			      Cdr_Ret1736,
			      Value_Get,
			      TrueResult253),
		      _70132=TrueResult253
		  ;   (   is_eq(Key232, 2)
		      ->  get_var(GEnv1654, sys_iter, Iter_Get245),
			  get_var(GEnv1654, sys_value, Value_Get246),
			  f_sys_pf_set_caadr(Iter_Get245,
					     Value_Get246,
					     TrueResult251),
			  ElseResult254=TrueResult251
		      ;   get_var(GEnv1654, sys_iter, Iter_Get249),
			  get_var(GEnv1654, sys_value, Value_Get250),
			  f_sys_pf_set_caaadr(Iter_Get249,
					      Value_Get250,
					      ElseResult252),
			  ElseResult254=ElseResult252
		      ),
		      _70132=ElseResult254
		  )
		),
		_70132=FnResult226
	      ),
	      block_exit(sys_seq_set, FnResult226),
	      true).
wl:lambda_def(defun, sys_seq_end_p, f_sys_seq_end_p1, [sequence, sys_iter, c38_key, sys_start, sys_end, sys_from_end], [[case, [car, sys_iter], [0, [or, [=, [cdr, sys_iter], [length, sequence]], [and, sys_end, [=, sys_end, [cdr, sys_iter]]]]], [1, [<, [cdr, sys_iter], sys_start]], [2, [or, [null, [cadr, sys_iter]], [and, sys_end, [=, sys_end, [caddr, sys_iter]]]]], [t, [or, [null, [cadr, sys_iter]], [<, [caddr, sys_iter], sys_start]]]]]).
wl:arglist_info(sys_seq_end_p, f_sys_seq_end_p1, [sequence, sys_iter, c38_key, sys_start, sys_end, sys_from_end], arginfo{all:[sequence, sys_iter], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_start, sys_end, sys_from_end], names:[sequence, sys_iter, sys_start, sys_end, sys_from_end], opt:0, req:[sequence, sys_iter], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_sys_seq_end_p1).

/*

### Compiled Function: `SYS::SEQ-END-P` 
*/
f_sys_seq_end_p1(Sequence_In261, Iter_In262, RestNKeys257, FnResult256) :-
	GEnv1655=[bv(sequence, Sequence_In261), bv(sys_iter, Iter_In262), bv(sys_start, Start_In263), bv(sys_end, End_In264), bv(sys_from_end, From_end_In265)],
	get_kw(Env,
	       RestNKeys257,
	       sys_start,
	       sys_start,
	       Start_In263,
	       []=Start_In263,
	       Start_P260),
	get_kw(Env,
	       RestNKeys257,
	       sys_end,
	       sys_end,
	       End_In264,
	       []=End_In264,
	       End_P259),
	get_kw(Env,
	       RestNKeys257,
	       sys_from_end,
	       sys_from_end,
	       From_end_In265,
	       []=From_end_In265,
	       From_end_P258),
	catch(( ( get_var(GEnv1655, sys_iter, Iter_Get266),
		  f_car(Iter_Get266, Key267),
		  (   is_eq(Key267, 0)
		  ->  (   get_var(GEnv1655, sys_iter, Iter_Get271),
			  f_cdr(Iter_Get271, Cdr_Ret1737),
			  get_var(GEnv1655, sequence, Sequence_Get272),
			  f_length(Sequence_Get272, Length_Ret),
			  'f_='(Cdr_Ret1737, Length_Ret, FORM1_Res279),
			  FORM1_Res279\==[],
			  TrueResult302=FORM1_Res279
		      ->  true
		      ;   get_var(GEnv1655, sys_end, IFTEST273),
			  (   IFTEST273\==[]
			  ->  get_var(GEnv1655, sys_end, End_Get276),
			      get_var(GEnv1655, sys_iter, Iter_Get277),
			      f_cdr(Iter_Get277, Cdr_Ret1739),
			      'f_='(End_Get276, Cdr_Ret1739, TrueResult278),
			      _71250=TrueResult278
			  ;   _71250=[]
			  ),
			  TrueResult302=_71250
		      ),
		      _71064=TrueResult302
		  ;   (   is_eq(Key267, 1)
		      ->  get_var(GEnv1655, sys_iter, Iter_Get282),
			  f_cdr(Iter_Get282, Cdr_Ret1740),
			  get_var(GEnv1655, sys_start, Start_Get283),
			  'f_<'(Cdr_Ret1740, Start_Get283, TrueResult300),
			  ElseResult303=TrueResult300
		      ;   (   is_eq(Key267, 2)
			  ->  (   get_var(GEnv1655, sys_iter, Iter_Get286),
				  f_cadr(Iter_Get286, Null_Param),
				  f_null(Null_Param, FORM1_Res293),
				  FORM1_Res293\==[],
				  TrueResult298=FORM1_Res293
			      ->  true
			      ;   get_var(GEnv1655, sys_end, IFTEST287),
				  (   IFTEST287\==[]
				  ->  get_var(GEnv1655, sys_end, End_Get290),
				      get_var(GEnv1655, sys_iter, Iter_Get291),
				      f_caddr(Iter_Get291, Caddr_Ret1741),
				      'f_='(End_Get290,
					    Caddr_Ret1741,
					    TrueResult292),
				      _71618=TrueResult292
				  ;   _71618=[]
				  ),
				  TrueResult298=_71618
			      ),
			      ElseResult301=TrueResult298
			  ;   (   get_var(GEnv1655, sys_iter, Iter_Get294),
				  f_cadr(Iter_Get294, Null_Param1687),
				  f_null(Null_Param1687, FORM1_Res297),
				  FORM1_Res297\==[],
				  ElseResult299=FORM1_Res297
			      ->  true
			      ;   get_var(GEnv1655, sys_iter, Iter_Get295),
				  f_caddr(Iter_Get295, Caddr_Ret1742),
				  get_var(GEnv1655, sys_start, Start_Get296),
				  'f_<'(Caddr_Ret1742, Start_Get296, _71838),
				  ElseResult299=_71838
			      ),
			      ElseResult301=ElseResult299
			  ),
			  ElseResult303=ElseResult301
		      ),
		      _71064=ElseResult303
		  )
		),
		_71064=FnResult256
	      ),
	      block_exit(sys_seq_end_p, FnResult256),
	      true).
wl:lambda_def(defun, sys_seq_result, f_sys_seq_result1, [sequence, sys_iter, sys_result], [[case, [car, sys_iter], [0, [make_array, [length, sys_result], kw_element_type, [array_element_type, sequence], kw_initial_contents, [reverse, sys_result]]], [1, [make_array, [length, sys_result], kw_element_type, [array_element_type, sequence], kw_initial_contents, sys_result]], [2, [reverse, sys_result]], [3, sys_result]]]).
wl:arglist_info(sys_seq_result, f_sys_seq_result1, [sequence, sys_iter, sys_result], arginfo{all:[sequence, sys_iter, sys_result], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iter, sys_result], opt:0, req:[sequence, sys_iter, sys_result], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_sys_seq_result1).

/*

### Compiled Function: `SYS::SEQ-RESULT` 
*/
f_sys_seq_result1(Sequence_In307, Iter_In308, Result_In, RestNKeys306, FnResult305) :-
	GEnv1656=[bv(sequence, Sequence_In307), bv(sys_iter, Iter_In308), bv(sys_result, Result_In)],
	catch(( ( get_var(GEnv1656, sys_iter, Iter_Get310),
		  f_car(Iter_Get310, Key311),
		  (   is_eq(Key311, 0)
		  ->  get_var(GEnv1656, sys_result, Result_Get),
		      f_length(Result_Get, Length_Ret1743),
		      get_var(GEnv1656, sequence, Sequence_Get316),
		      f_array_element_type(Sequence_Get316, Element_type_Ret),
		      get_var(GEnv1656, sys_result, Result_Get317),
		      f_reverse(Result_Get317, Reverse_Ret),
		      f_make_array(
				   [ Length_Ret1743,
				     kw_element_type,
				     Element_type_Ret,
				     kw_initial_contents,
				     Reverse_Ret
				   ],
				   TrueResult335),
		      _72224=TrueResult335
		  ;   (   is_eq(Key311, 1)
		      ->  get_var(GEnv1656, sys_result, Result_Get320),
			  f_length(Result_Get320, Length_Ret1746),
			  get_var(GEnv1656, sequence, Sequence_Get321),
			  f_array_element_type(Sequence_Get321,
					       Element_type_Ret1747),
			  get_var(GEnv1656, sys_result, Result_Get322),
			  f_make_array(
				       [ Length_Ret1746,
					 kw_element_type,
					 Element_type_Ret1747,
					 kw_initial_contents,
					 Result_Get322
				       ],
				       TrueResult333),
			  ElseResult336=TrueResult333
		      ;   (   is_eq(Key311, 2)
			  ->  get_var(GEnv1656, sys_result, Result_Get325),
			      f_reverse(Result_Get325, TrueResult331),
			      ElseResult334=TrueResult331
			  ;   (   is_eq(Key311, 3)
			      ->  get_var(GEnv1656, sys_result, Result_Get328),
				  ElseResult332=Result_Get328
			      ;   ElseResult330=[],
				  ElseResult332=ElseResult330
			      ),
			      ElseResult334=ElseResult332
			  ),
			  ElseResult336=ElseResult334
		      ),
		      _72224=ElseResult336
		  )
		),
		_72224=FnResult305
	      ),
	      block_exit(sys_seq_result, FnResult305),
	      true).
wl:lambda_def(defun, subst, f_subst, [sys_new, sys_old, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [let, [[sys_a, [apply, function(subst), sys_new, sys_old, [car, sys_tree], rest]], [sys_d, [apply, function(subst), sys_new, sys_old, [cdr, sys_tree], rest]]], [if, [and, [eq, sys_a, [car, sys_tree]], [eq, sys_d, [cdr, sys_tree]]], sys_tree, [cons, sys_a, sys_d]]], [if, [apply, function(satisfies), sys_old, sys_tree, rest], sys_new, sys_tree]]]).
wl:arglist_info(subst, f_subst, [sys_new, sys_old, sys_tree, c38_rest, rest], arginfo{all:[sys_new, sys_old, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_new, sys_old, sys_tree, rest], opt:0, req:[sys_new, sys_old, sys_tree], rest:[rest], sublists:0, whole:0}).
wl: init_args(3, f_subst).

/*

### Compiled Function: `CL:SUBST` 
*/
f_subst(New_In, Old_In, Tree_In, RestNKeys339, FnResult338) :-
	GEnv1657=[bv(sys_new, New_In), bv(sys_old, Old_In), bv(sys_tree, Tree_In), bv(rest, RestNKeys339)],
	catch(( ( get_var(GEnv1657, sys_tree, Tree_Get),
		  (   c0nz:is_consp(Tree_Get)
		  ->  get_var(GEnv1657, sys_new, New_Get),
		      get_var(GEnv1657, sys_old, Old_Get),
		      get_var(GEnv1657, sys_tree, Tree_Get353),
		      f_car(Tree_Get353, Car_Ret),
		      get_var(GEnv1657, rest, Rest_Get),
		      f_apply(f_subst,
			      [New_Get, Old_Get, Car_Ret, Rest_Get],
			      A_Init),
		      get_var(GEnv1657, sys_new, New_Get355),
		      get_var(GEnv1657, sys_old, Old_Get356),
		      get_var(GEnv1657, sys_tree, Tree_Get357),
		      f_cdr(Tree_Get357, Cdr_Ret1749),
		      get_var(GEnv1657, rest, Rest_Get358),
		      f_apply(f_subst,
			      [New_Get355, Old_Get356, Cdr_Ret1749, Rest_Get358],
			      D_Init),
		      LEnv350=[bv(sys_a, A_Init), bv(sys_d, D_Init)|GEnv1657],
		      get_var(LEnv350, sys_a, A_Get),
		      get_var(LEnv350, sys_tree, Tree_Get365),
		      f_car(Tree_Get365, PredArg2Result),
		      (   is_eq(A_Get, PredArg2Result)
		      ->  get_var(LEnv350, sys_d, D_Get),
			  get_var(LEnv350, sys_tree, Tree_Get370),
			  f_cdr(Tree_Get370, Cdr_Ret1750),
			  f_eq(D_Get, Cdr_Ret1750, TrueResult371),
			  IFTEST361=TrueResult371
		      ;   IFTEST361=[]
		      ),
		      (   IFTEST361\==[]
		      ->  get_var(LEnv350, sys_tree, Tree_Get372),
			  LetResult349=Tree_Get372
		      ;   get_var(LEnv350, sys_a, A_Get373),
			  get_var(LEnv350, sys_d, D_Get374),
			  ElseResult376=[A_Get373|D_Get374],
			  LetResult349=ElseResult376
		      ),
		      _73036=LetResult349
		  ;   ( get_var(GEnv1657, rest, Rest_Get381),
			get_var(GEnv1657, sys_old, Old_Get379)
		      ),
		      get_var(GEnv1657, sys_tree, Tree_Get380),
		      f_apply(f_satisfies1,
			      [Old_Get379, Tree_Get380, Rest_Get381],
			      IFTEST377),
		      (   IFTEST377\==[]
		      ->  get_var(GEnv1657, sys_new, New_Get382),
			  ElseResult387=New_Get382
		      ;   get_var(GEnv1657, sys_tree, Tree_Get383),
			  ElseResult387=Tree_Get383
		      ),
		      _73036=ElseResult387
		  )
		),
		_73036=FnResult338
	      ),
	      block_exit(subst, FnResult338),
	      true).
:- set_opv(subst, symbol_function, f_subst),
   DefunResult=subst,
   assert_lsp(subst_if,
	      wl:lambda_def(defun, subst_if, f_subst_if, [sys_new, sys_predicate, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [let, [[sys_a, [apply, function(subst), sys_new, sys_predicate, [car, sys_tree], rest]], [sys_d, [apply, function(subst), sys_new, sys_predicate, [cdr, sys_tree], rest]]], [if, [and, [eq, sys_a, [car, sys_tree]], [eq, sys_d, [cdr, sys_tree]]], sys_tree, [cons, sys_a, sys_d]]], [if, [apply, function(sys_satisfies_if), sys_predicate, sys_tree, rest], sys_new, sys_tree]]])),
   assert_lsp(subst_if,
	      wl:arglist_info(subst_if, f_subst_if, [sys_new, sys_predicate, sys_tree, c38_rest, rest], arginfo{all:[sys_new, sys_predicate, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_new, sys_predicate, sys_tree, rest], opt:0, req:[sys_new, sys_predicate, sys_tree], rest:[rest], sublists:0, whole:0})),
   assert_lsp(subst_if, wl:init_args(3, f_subst_if)),
   assert_lsp(subst_if,
	      (f_subst_if(New_In392, Predicate_In393, Tree_In394, RestNKeys391, FnResult390):-GEnv1658=[bv(sys_new, New_In392), bv(sys_predicate, Predicate_In393), bv(sys_tree, Tree_In394), bv(rest, RestNKeys391)], catch(((get_var(GEnv1658, sys_tree, Tree_Get397), (c0nz:is_consp(Tree_Get397)->get_var(GEnv1658, sys_new, New_Get403), get_var(GEnv1658, sys_predicate, Predicate_Get404), get_var(GEnv1658, sys_tree, Tree_Get405), f_car(Tree_Get405, Car_Ret1751), get_var(GEnv1658, rest, Rest_Get406), f_apply(f_subst, [New_Get403, Predicate_Get404, Car_Ret1751, Rest_Get406], A_Init411), get_var(GEnv1658, sys_new, New_Get407), get_var(GEnv1658, sys_predicate, Predicate_Get408), get_var(GEnv1658, sys_tree, Tree_Get409), f_cdr(Tree_Get409, Cdr_Ret1752), get_var(GEnv1658, rest, Rest_Get410), f_apply(f_subst, [New_Get407, Predicate_Get408, Cdr_Ret1752, Rest_Get410], D_Init412), LEnv402=[bv(sys_a, A_Init411), bv(sys_d, D_Init412)|GEnv1658], get_var(LEnv402, sys_a, A_Get416), get_var(LEnv402, sys_tree, Tree_Get417), f_car(Tree_Get417, PredArg2Result420), (is_eq(A_Get416, PredArg2Result420)->get_var(LEnv402, sys_d, D_Get421), get_var(LEnv402, sys_tree, Tree_Get422), f_cdr(Tree_Get422, Cdr_Ret1753), f_eq(D_Get421, Cdr_Ret1753, TrueResult423), IFTEST413=TrueResult423;IFTEST413=[]), (IFTEST413\==[]->get_var(LEnv402, sys_tree, Tree_Get424), LetResult401=Tree_Get424;get_var(LEnv402, sys_a, A_Get425), get_var(LEnv402, sys_d, D_Get426), ElseResult428=[A_Get425|D_Get426], LetResult401=ElseResult428), _74228=LetResult401;(get_var(GEnv1658, rest, Rest_Get433), get_var(GEnv1658, sys_predicate, Predicate_Get431)), get_var(GEnv1658, sys_tree, Tree_Get432), f_apply(f_sys_satisfies_if1, [Predicate_Get431, Tree_Get432, Rest_Get433], IFTEST429), (IFTEST429\==[]->get_var(GEnv1658, sys_new, New_Get434), ElseResult439=New_Get434;get_var(GEnv1658, sys_tree, Tree_Get435), ElseResult439=Tree_Get435), _74228=ElseResult439)), _74228=FnResult390), block_exit(subst_if, FnResult390), true))),
   set_opv(subst_if, symbol_function, f_subst_if),
   DefunResult441=subst_if,
   assert_lsp(subst_if_not,
	      wl:lambda_def(defun, subst_if_not, f_subst_if_not, [sys_new, sys_predicate, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [let, [[sys_a, [apply, function(subst), sys_new, sys_predicate, [car, sys_tree], rest]], [sys_d, [apply, function(subst), sys_new, sys_predicate, [cdr, sys_tree], rest]]], [if, [and, [eq, sys_a, [car, sys_tree]], [eq, sys_d, [cdr, sys_tree]]], sys_tree, [cons, sys_a, sys_d]]], [if, [apply, function(sys_satisfies_if_not), sys_predicate, sys_tree, rest], sys_new, sys_tree]]])),
   assert_lsp(subst_if_not,
	      wl:arglist_info(subst_if_not, f_subst_if_not, [sys_new, sys_predicate, sys_tree, c38_rest, rest], arginfo{all:[sys_new, sys_predicate, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_new, sys_predicate, sys_tree, rest], opt:0, req:[sys_new, sys_predicate, sys_tree], rest:[rest], sublists:0, whole:0})),
   assert_lsp(subst_if_not, wl:init_args(3, f_subst_if_not)),
   assert_lsp(subst_if_not,
	      (f_subst_if_not(New_In444, Predicate_In445, Tree_In446, RestNKeys443, FnResult442):-GEnv1659=[bv(sys_new, New_In444), bv(sys_predicate, Predicate_In445), bv(sys_tree, Tree_In446), bv(rest, RestNKeys443)], catch(((get_var(GEnv1659, sys_tree, Tree_Get449), (c0nz:is_consp(Tree_Get449)->get_var(GEnv1659, sys_new, New_Get455), get_var(GEnv1659, sys_predicate, Predicate_Get456), get_var(GEnv1659, sys_tree, Tree_Get457), f_car(Tree_Get457, Car_Ret1754), get_var(GEnv1659, rest, Rest_Get458), f_apply(f_subst, [New_Get455, Predicate_Get456, Car_Ret1754, Rest_Get458], A_Init463), get_var(GEnv1659, sys_new, New_Get459), get_var(GEnv1659, sys_predicate, Predicate_Get460), get_var(GEnv1659, sys_tree, Tree_Get461), f_cdr(Tree_Get461, Cdr_Ret1755), get_var(GEnv1659, rest, Rest_Get462), f_apply(f_subst, [New_Get459, Predicate_Get460, Cdr_Ret1755, Rest_Get462], D_Init464), LEnv454=[bv(sys_a, A_Init463), bv(sys_d, D_Init464)|GEnv1659], get_var(LEnv454, sys_a, A_Get468), get_var(LEnv454, sys_tree, Tree_Get469), f_car(Tree_Get469, PredArg2Result472), (is_eq(A_Get468, PredArg2Result472)->get_var(LEnv454, sys_d, D_Get473), get_var(LEnv454, sys_tree, Tree_Get474), f_cdr(Tree_Get474, Cdr_Ret1756), f_eq(D_Get473, Cdr_Ret1756, TrueResult475), IFTEST465=TrueResult475;IFTEST465=[]), (IFTEST465\==[]->get_var(LEnv454, sys_tree, Tree_Get476), LetResult453=Tree_Get476;get_var(LEnv454, sys_a, A_Get477), get_var(LEnv454, sys_d, D_Get478), ElseResult480=[A_Get477|D_Get478], LetResult453=ElseResult480), _75526=LetResult453;(get_var(GEnv1659, rest, Rest_Get485), get_var(GEnv1659, sys_predicate, Predicate_Get483)), get_var(GEnv1659, sys_tree, Tree_Get484), f_apply(f_sys_satisfies_if_not1, [Predicate_Get483, Tree_Get484, Rest_Get485], IFTEST481), (IFTEST481\==[]->get_var(GEnv1659, sys_new, New_Get486), ElseResult491=New_Get486;get_var(GEnv1659, sys_tree, Tree_Get487), ElseResult491=Tree_Get487), _75526=ElseResult491)), _75526=FnResult442), block_exit(subst_if_not, FnResult442), true))),
   set_opv(subst_if_not, symbol_function, f_subst_if_not),
   DefunResult493=subst_if_not,
   assert_lsp(nsubst,
	      wl:lambda_def(defun, nsubst, f_nsubst, [sys_new, sys_old, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [progn, [setf, [car, sys_tree], [apply, function(subst), sys_new, sys_old, [car, sys_tree], rest]], [setf, [cdr, sys_tree], [apply, function(subst), sys_new, sys_old, [cdr, sys_tree], rest]], sys_tree], [if, [apply, function(satisfies), sys_old, sys_tree, rest], sys_new, sys_tree]]])),
   assert_lsp(nsubst,
	      wl:arglist_info(nsubst, f_nsubst, [sys_new, sys_old, sys_tree, c38_rest, rest], arginfo{all:[sys_new, sys_old, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_new, sys_old, sys_tree, rest], opt:0, req:[sys_new, sys_old, sys_tree], rest:[rest], sublists:0, whole:0})),
   assert_lsp(nsubst, wl:init_args(3, f_nsubst)),
   assert_lsp(nsubst,
	      (f_nsubst(New_In496, Old_In497, Tree_In498, RestNKeys495, FnResult494):-GEnv1660=[bv(sys_new, New_In496), bv(sys_old, Old_In497), bv(sys_tree, Tree_In498), bv(rest, RestNKeys495)], catch(((get_var(GEnv1660, sys_tree, Tree_Get501), (c0nz:is_consp(Tree_Get501)->get_var(GEnv1660, sys_new, New_Get507), get_var(GEnv1660, sys_old, Old_Get508), get_var(GEnv1660, sys_tree, Tree_Get506), f_car(Tree_Get506, Car_Ret1757), get_var(GEnv1660, rest, Rest_Get510), f_apply(f_subst, [New_Get507, Old_Get508, Car_Ret1757, Rest_Get510], Apply_Ret1758), f_rplaca(Tree_Get506, Apply_Ret1758, Rplaca_Ret), get_var(GEnv1660, sys_new, New_Get514), get_var(GEnv1660, sys_old, Old_Get515), get_var(GEnv1660, sys_tree, Tree_Get513), f_cdr(Tree_Get513, Cdr_Ret1760), get_var(GEnv1660, rest, Rest_Get517), f_apply(f_subst, [New_Get514, Old_Get515, Cdr_Ret1760, Rest_Get517], Apply_Ret1761), f_rplacd(Tree_Get513, Apply_Ret1761, Rplacd_Ret), get_var(GEnv1660, sys_tree, Tree_Get518), _76824=Tree_Get518;(get_var(GEnv1660, rest, Rest_Get523), get_var(GEnv1660, sys_old, Old_Get521)), get_var(GEnv1660, sys_tree, Tree_Get522), f_apply(f_satisfies1, [Old_Get521, Tree_Get522, Rest_Get523], IFTEST519), (IFTEST519\==[]->get_var(GEnv1660, sys_new, New_Get524), ElseResult529=New_Get524;get_var(GEnv1660, sys_tree, Tree_Get525), ElseResult529=Tree_Get525), _76824=ElseResult529)), _76824=FnResult494), block_exit(nsubst, FnResult494), true))),
   set_opv(nsubst, symbol_function, f_nsubst),
   DefunResult531=nsubst,
   assert_lsp(nsubst_if,
	      wl:lambda_def(defun, nsubst_if, f_nsubst_if, [sys_new, sys_predicate, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [progn, [setf, [car, sys_tree], [apply, function(subst), sys_new, sys_predicate, [car, sys_tree], rest]], [setf, [cdr, sys_tree], [apply, function(subst), sys_new, sys_predicate, [cdr, sys_tree], rest]], sys_tree], [if, [apply, function(sys_satisfies_if), sys_predicate, sys_tree, rest], sys_new, sys_tree]]])),
   assert_lsp(nsubst_if,
	      wl:arglist_info(nsubst_if, f_nsubst_if, [sys_new, sys_predicate, sys_tree, c38_rest, rest], arginfo{all:[sys_new, sys_predicate, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_new, sys_predicate, sys_tree, rest], opt:0, req:[sys_new, sys_predicate, sys_tree], rest:[rest], sublists:0, whole:0})),
   assert_lsp(nsubst_if, wl:init_args(3, f_nsubst_if)),
   assert_lsp(nsubst_if,
	      (f_nsubst_if(New_In534, Predicate_In535, Tree_In536, RestNKeys533, FnResult532):-GEnv1661=[bv(sys_new, New_In534), bv(sys_predicate, Predicate_In535), bv(sys_tree, Tree_In536), bv(rest, RestNKeys533)], catch(((get_var(GEnv1661, sys_tree, Tree_Get539), (c0nz:is_consp(Tree_Get539)->get_var(GEnv1661, sys_new, New_Get545), get_var(GEnv1661, sys_predicate, Predicate_Get546), get_var(GEnv1661, sys_tree, Tree_Get544), f_car(Tree_Get544, Car_Ret1763), get_var(GEnv1661, rest, Rest_Get548), f_apply(f_subst, [New_Get545, Predicate_Get546, Car_Ret1763, Rest_Get548], Apply_Ret1764), f_rplaca(Tree_Get544, Apply_Ret1764, Rplaca_Ret1765), get_var(GEnv1661, sys_new, New_Get552), get_var(GEnv1661, sys_predicate, Predicate_Get553), get_var(GEnv1661, sys_tree, Tree_Get551), f_cdr(Tree_Get551, Cdr_Ret1766), get_var(GEnv1661, rest, Rest_Get555), f_apply(f_subst, [New_Get552, Predicate_Get553, Cdr_Ret1766, Rest_Get555], Apply_Ret1767), f_rplacd(Tree_Get551, Apply_Ret1767, Rplacd_Ret1768), get_var(GEnv1661, sys_tree, Tree_Get556), _77800=Tree_Get556;(get_var(GEnv1661, rest, Rest_Get561), get_var(GEnv1661, sys_predicate, Predicate_Get559)), get_var(GEnv1661, sys_tree, Tree_Get560), f_apply(f_sys_satisfies_if1, [Predicate_Get559, Tree_Get560, Rest_Get561], IFTEST557), (IFTEST557\==[]->get_var(GEnv1661, sys_new, New_Get562), ElseResult567=New_Get562;get_var(GEnv1661, sys_tree, Tree_Get563), ElseResult567=Tree_Get563), _77800=ElseResult567)), _77800=FnResult532), block_exit(nsubst_if, FnResult532), true))),
   set_opv(nsubst_if, symbol_function, f_nsubst_if),
   DefunResult569=nsubst_if,
   assert_lsp(nsubst_if_not,
	      wl:lambda_def(defun, nsubst_if_not, f_nsubst_if_not, [sys_new, sys_predicate, sys_tree, c38_rest, rest], [[if, [consp, sys_tree], [progn, [setf, [car, sys_tree], [apply, function(subst), sys_new, sys_predicate, [car, sys_tree], rest]], [setf, [cdr, sys_tree], [apply, function(subst), sys_new, sys_predicate, [cdr, sys_tree], rest]], sys_tree], [if, [apply, function(sys_satisfies_if_not), sys_predicate, sys_tree, rest], sys_new, sys_tree]]])),
   assert_lsp(nsubst_if_not,
	      wl:arglist_info(nsubst_if_not, f_nsubst_if_not, [sys_new, sys_predicate, sys_tree, c38_rest, rest], arginfo{all:[sys_new, sys_predicate, sys_tree], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_new, sys_predicate, sys_tree, rest], opt:0, req:[sys_new, sys_predicate, sys_tree], rest:[rest], sublists:0, whole:0})),
   assert_lsp(nsubst_if_not, wl:init_args(3, f_nsubst_if_not)),
   assert_lsp(nsubst_if_not,
	      (f_nsubst_if_not(New_In572, Predicate_In573, Tree_In574, RestNKeys571, FnResult570):-GEnv1662=[bv(sys_new, New_In572), bv(sys_predicate, Predicate_In573), bv(sys_tree, Tree_In574), bv(rest, RestNKeys571)], catch(((get_var(GEnv1662, sys_tree, Tree_Get577), (c0nz:is_consp(Tree_Get577)->get_var(GEnv1662, sys_new, New_Get583), get_var(GEnv1662, sys_predicate, Predicate_Get584), get_var(GEnv1662, sys_tree, Tree_Get582), f_car(Tree_Get582, Car_Ret1769), get_var(GEnv1662, rest, Rest_Get586), f_apply(f_subst, [New_Get583, Predicate_Get584, Car_Ret1769, Rest_Get586], Apply_Ret1770), f_rplaca(Tree_Get582, Apply_Ret1770, Rplaca_Ret1771), get_var(GEnv1662, sys_new, New_Get590), get_var(GEnv1662, sys_predicate, Predicate_Get591), get_var(GEnv1662, sys_tree, Tree_Get589), f_cdr(Tree_Get589, Cdr_Ret1772), get_var(GEnv1662, rest, Rest_Get593), f_apply(f_subst, [New_Get590, Predicate_Get591, Cdr_Ret1772, Rest_Get593], Apply_Ret1773), f_rplacd(Tree_Get589, Apply_Ret1773, Rplacd_Ret1774), get_var(GEnv1662, sys_tree, Tree_Get594), _78776=Tree_Get594;(get_var(GEnv1662, rest, Rest_Get599), get_var(GEnv1662, sys_predicate, Predicate_Get597)), get_var(GEnv1662, sys_tree, Tree_Get598), f_apply(f_sys_satisfies_if_not1, [Predicate_Get597, Tree_Get598, Rest_Get599], IFTEST595), (IFTEST595\==[]->get_var(GEnv1662, sys_new, New_Get600), ElseResult605=New_Get600;get_var(GEnv1662, sys_tree, Tree_Get601), ElseResult605=Tree_Get601), _78776=ElseResult605)), _78776=FnResult570), block_exit(nsubst_if_not, FnResult570), true))),
   set_opv(nsubst_if_not, symbol_function, f_nsubst_if_not),
   DefunResult607=nsubst_if_not,
   assert_lsp(assoc_if,
	      wl:lambda_def(defun, assoc_if, f_assoc_if, [sys_predicate, sys_alist, c38_rest, rest], [[dolist, [sys_elem, sys_alist], [when, [apply, function(sys_satisfies_if), sys_predicate, [car, sys_elem], rest], [return_from, assoc_if, sys_elem]]]])),
   assert_lsp(assoc_if,
	      wl:arglist_info(assoc_if, f_assoc_if, [sys_predicate, sys_alist, c38_rest, rest], arginfo{all:[sys_predicate, sys_alist], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_alist, rest], opt:0, req:[sys_predicate, sys_alist], rest:[rest], sublists:0, whole:0})),
   assert_lsp(assoc_if, wl:init_args(2, f_assoc_if)),
   assert_lsp(assoc_if,
	      (f_assoc_if(Predicate_In610, Alist_In, RestNKeys609, FnResult608):-GEnv1663=[bv(sys_predicate, Predicate_In610), bv(sys_alist, Alist_In), bv(rest, RestNKeys609)], catch(((get_var(GEnv1663, sys_alist, Alist_Get), BV=bv(sys_elem, Ele), BlockExitEnv=[BV|GEnv1663], forall(member(Ele, Alist_Get),  (nb_setarg(2, BV, Ele), get_var(BlockExitEnv, sys_elem, Elem_Get617), get_var(BlockExitEnv, sys_predicate, Predicate_Get616), f_car(Elem_Get617, Car_Ret1775), get_var(BlockExitEnv, rest, Rest_Get618), f_apply(f_sys_satisfies_if1, [Predicate_Get616, Car_Ret1775, Rest_Get618], IFTEST614), (IFTEST614\==[]->get_var(BlockExitEnv, sys_elem, Elem_Get621), throw(block_exit(assoc_if, Elem_Get621)), _79710=ThrowResult;_79710=[])))), _79710=FnResult608), block_exit(assoc_if, FnResult608), true))),
   set_opv(assoc_if, symbol_function, f_assoc_if),
   DefunResult629=assoc_if,
   assert_lsp(assoc_if_not,
	      wl:lambda_def(defun, assoc_if_not, f_assoc_if_not, [sys_predicate, sys_alist, c38_rest, rest], [[dolist, [sys_elem, sys_alist], [when, [apply, function(sys_satisfies_if_not), sys_predicate, [car, sys_elem], rest], [return_from, assoc_if_not, sys_elem]]]])),
   assert_lsp(assoc_if_not,
	      wl:arglist_info(assoc_if_not, f_assoc_if_not, [sys_predicate, sys_alist, c38_rest, rest], arginfo{all:[sys_predicate, sys_alist], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_alist, rest], opt:0, req:[sys_predicate, sys_alist], rest:[rest], sublists:0, whole:0})),
   assert_lsp(assoc_if_not, wl:init_args(2, f_assoc_if_not)),
   assert_lsp(assoc_if_not,
	      (f_assoc_if_not(Predicate_In632, Alist_In633, RestNKeys631, FnResult630):-GEnv1664=[bv(sys_predicate, Predicate_In632), bv(sys_alist, Alist_In633), bv(rest, RestNKeys631)], catch(((get_var(GEnv1664, sys_alist, Alist_Get635), BV646=bv(sys_elem, Ele648), BlockExitEnv644=[BV646|GEnv1664], forall(member(Ele648, Alist_Get635),  (nb_setarg(2, BV646, Ele648), get_var(BlockExitEnv644, sys_elem, Elem_Get639), get_var(BlockExitEnv644, sys_predicate, Predicate_Get638), f_car(Elem_Get639, Car_Ret1776), get_var(BlockExitEnv644, rest, Rest_Get640), f_apply(f_sys_satisfies_if_not1, [Predicate_Get638, Car_Ret1776, Rest_Get640], IFTEST636), (IFTEST636\==[]->get_var(BlockExitEnv644, sys_elem, RetResult641), throw(block_exit(assoc_if_not, RetResult641)), _80202=ThrowResult642;_80202=[])))), _80202=FnResult630), block_exit(assoc_if_not, FnResult630), true))),
   set_opv(assoc_if_not, symbol_function, f_assoc_if_not),
   DefunResult651=assoc_if_not,
   assert_lsp(rassoc,
	      wl:lambda_def(defun, rassoc, f_rassoc, [sys_item, sys_alist, c38_rest, rest], [[dolist, [sys_elem, sys_alist], [when, [apply, function(satisfies), sys_item, [cdr, sys_elem], rest], [return_from, rassoc, sys_elem]]]])),
   assert_lsp(rassoc,
	      wl:arglist_info(rassoc, f_rassoc, [sys_item, sys_alist, c38_rest, rest], arginfo{all:[sys_item, sys_alist], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_item, sys_alist, rest], opt:0, req:[sys_item, sys_alist], rest:[rest], sublists:0, whole:0})),
   assert_lsp(rassoc, wl:init_args(2, f_rassoc)),
   assert_lsp(rassoc,
	      (f_rassoc(Item_In, Alist_In655, RestNKeys653, FnResult652):-GEnv1665=[bv(sys_item, Item_In), bv(sys_alist, Alist_In655), bv(rest, RestNKeys653)], catch(((get_var(GEnv1665, sys_alist, Alist_Get657), BV668=bv(sys_elem, Ele670), BlockExitEnv666=[BV668|GEnv1665], forall(member(Ele670, Alist_Get657),  (nb_setarg(2, BV668, Ele670), get_var(BlockExitEnv666, sys_elem, Elem_Get661), get_var(BlockExitEnv666, sys_item, Item_Get), f_cdr(Elem_Get661, Cdr_Ret1777), get_var(BlockExitEnv666, rest, Rest_Get662), f_apply(f_satisfies1, [Item_Get, Cdr_Ret1777, Rest_Get662], IFTEST658), (IFTEST658\==[]->get_var(BlockExitEnv666, sys_elem, RetResult663), throw(block_exit(rassoc, RetResult663)), _80728=ThrowResult664;_80728=[])))), _80728=FnResult652), block_exit(rassoc, FnResult652), true))),
   set_opv(rassoc, symbol_function, f_rassoc),
   DefunResult673=rassoc,
   assert_lsp(rassoc_if,
	      wl:lambda_def(defun, rassoc_if, f_rassoc_if, [sys_predicate, sys_alist, c38_rest, rest], [[dolist, [sys_elem, sys_alist], [when, [apply, function(sys_satisfies_if), sys_predicate, [cdr, sys_elem], rest], [return_from, rassoc_if, sys_elem]]]])),
   assert_lsp(rassoc_if,
	      wl:arglist_info(rassoc_if, f_rassoc_if, [sys_predicate, sys_alist, c38_rest, rest], arginfo{all:[sys_predicate, sys_alist], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_alist, rest], opt:0, req:[sys_predicate, sys_alist], rest:[rest], sublists:0, whole:0})),
   assert_lsp(rassoc_if, wl:init_args(2, f_rassoc_if)),
   assert_lsp(rassoc_if,
	      (f_rassoc_if(Predicate_In676, Alist_In677, RestNKeys675, FnResult674):-GEnv1666=[bv(sys_predicate, Predicate_In676), bv(sys_alist, Alist_In677), bv(rest, RestNKeys675)], catch(((get_var(GEnv1666, sys_alist, Alist_Get679), BV690=bv(sys_elem, Ele692), BlockExitEnv688=[BV690|GEnv1666], forall(member(Ele692, Alist_Get679),  (nb_setarg(2, BV690, Ele692), get_var(BlockExitEnv688, sys_elem, Elem_Get683), get_var(BlockExitEnv688, sys_predicate, Predicate_Get682), f_cdr(Elem_Get683, Cdr_Ret1778), get_var(BlockExitEnv688, rest, Rest_Get684), f_apply(f_sys_satisfies_if1, [Predicate_Get682, Cdr_Ret1778, Rest_Get684], IFTEST680), (IFTEST680\==[]->get_var(BlockExitEnv688, sys_elem, RetResult685), throw(block_exit(rassoc_if, RetResult685)), _81256=ThrowResult686;_81256=[])))), _81256=FnResult674), block_exit(rassoc_if, FnResult674), true))),
   set_opv(rassoc_if, symbol_function, f_rassoc_if),
   DefunResult695=rassoc_if,
   assert_lsp(rassoc_if_not,
	      wl:lambda_def(defun, rassoc_if_not, f_rassoc_if_not, [sys_predicate, sys_alist, c38_rest, rest], [[dolist, [sys_elem, sys_alist], [when, [apply, function(sys_satisfies_if_not), sys_predicate, [cdr, sys_elem], rest], [return_from, rassoc_if_not, sys_elem]]]])),
   assert_lsp(rassoc_if_not,
	      wl:arglist_info(rassoc_if_not, f_rassoc_if_not, [sys_predicate, sys_alist, c38_rest, rest], arginfo{all:[sys_predicate, sys_alist], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_alist, rest], opt:0, req:[sys_predicate, sys_alist], rest:[rest], sublists:0, whole:0})),
   assert_lsp(rassoc_if_not, wl:init_args(2, f_rassoc_if_not)),
   assert_lsp(rassoc_if_not,
	      (f_rassoc_if_not(Predicate_In698, Alist_In699, RestNKeys697, FnResult696):-GEnv1667=[bv(sys_predicate, Predicate_In698), bv(sys_alist, Alist_In699), bv(rest, RestNKeys697)], catch(((get_var(GEnv1667, sys_alist, Alist_Get701), BV712=bv(sys_elem, Ele714), BlockExitEnv710=[BV712|GEnv1667], forall(member(Ele714, Alist_Get701),  (nb_setarg(2, BV712, Ele714), get_var(BlockExitEnv710, sys_elem, Elem_Get705), get_var(BlockExitEnv710, sys_predicate, Predicate_Get704), f_cdr(Elem_Get705, Cdr_Ret1779), get_var(BlockExitEnv710, rest, Rest_Get706), f_apply(f_sys_satisfies_if_not1, [Predicate_Get704, Cdr_Ret1779, Rest_Get706], IFTEST702), (IFTEST702\==[]->get_var(BlockExitEnv710, sys_elem, RetResult707), throw(block_exit(rassoc_if_not, RetResult707)), _81796=ThrowResult708;_81796=[])))), _81796=FnResult696), block_exit(rassoc_if_not, FnResult696), true))),
   set_opv(rassoc_if_not, symbol_function, f_rassoc_if_not),
   DefunResult717=rassoc_if_not,
   assert_lsp(adjoin,
	      wl:lambda_def(defun, adjoin, f_adjoin, [sys_item, list, c38_rest, rest], [[dolist, [sys_elem, list, [cons, sys_item, list]], [when, [apply, function(satisfies), sys_item, sys_elem, rest], [return_from, adjoin, list]]]])),
   assert_lsp(adjoin,
	      wl:arglist_info(adjoin, f_adjoin, [sys_item, list, c38_rest, rest], arginfo{all:[sys_item, list], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_item, list, rest], opt:0, req:[sys_item, list], rest:[rest], sublists:0, whole:0})),
   assert_lsp(adjoin, wl:init_args(2, f_adjoin)),
   assert_lsp(adjoin,
	      (f_adjoin(Item_In720, List_In, RestNKeys719, FnResult718):-CDR1780=[bv(sys_item, Item_In720), bv(list, List_In), bv(rest, RestNKeys719)], catch(((LEnv725=[bv([cons, sys_item, list], [])|CDR1780], get_var(LEnv725, list, List_Get), BV737=bv(sys_elem, Ele739), BlockExitEnv735=[BV737|LEnv725], forall(member(Ele739, List_Get),  (nb_setarg(2, BV737, Ele739), get_var(BlockExitEnv735, sys_elem, Elem_Get730), (get_var(BlockExitEnv735, rest, Rest_Get731), get_var(BlockExitEnv735, sys_item, Item_Get729)), f_apply(f_satisfies1, [Item_Get729, Elem_Get730, Rest_Get731], IFTEST727), (IFTEST727\==[]->get_var(BlockExitEnv735, list, RetResult732), throw(block_exit(adjoin, RetResult732)), _82406=ThrowResult733;_82406=[]))), get_var(LEnv725, list, List_Get742), get_var(LEnv725, sys_item, Item_Get741), LetResult724=[Item_Get741|List_Get742]), LetResult724=FnResult718), block_exit(adjoin, FnResult718), true))),
   set_opv(adjoin, symbol_function, f_adjoin),
   DefunResult744=adjoin,
   assert_lsp(set_exclusive_or,
	      wl:lambda_def(defun, set_exclusive_or, f_set_exclusive_or, [sys_list_1, sys_list_2, c38_rest, rest, c38_key, key], [[let, [[sys_result, []]], [dolist, [sys_item, sys_list_1], [unless, [apply, function(member), [if, key, [funcall, key, sys_item], sys_item], sys_list_2, rest], [push, sys_item, sys_result]]], [dolist, [sys_item, sys_list_2], [block, sys_matches, [dolist, [sys_elem, sys_list_1], [when, [apply, function(satisfies), [if, key, [funcall, key, sys_elem], sys_elem], sys_item, rest], [return_from, sys_matches]]], [push, sys_item, sys_result]]], sys_result]])),
   assert_lsp(set_exclusive_or,
	      wl:arglist_info(set_exclusive_or, f_set_exclusive_or, [sys_list_1, sys_list_2, c38_rest, rest, c38_key, key], arginfo{all:[sys_list_1, sys_list_2], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:[key], names:[sys_list_1, sys_list_2, rest, key], opt:0, req:[sys_list_1, sys_list_2], rest:[rest], sublists:0, whole:0})),
   assert_lsp(set_exclusive_or, wl:init_args(2, f_set_exclusive_or)),
   assert_lsp(set_exclusive_or,
	      (f_set_exclusive_or(List_1_In, List_2_In, RestNKeys746, FnResult745):-CDR1781=[bv(sys_list_1, List_1_In), bv(sys_list_2, List_2_In), bv(rest, RestNKeys746), bv(key, Key_In751)], get_kw([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)]|Env], RestNKeys746, key, key, Key_In751, []=Key_In751, Key_P747), catch(((LEnv754=[bv(sys_result, [])|CDR1781], get_var(LEnv754, sys_list_1, List_1_Get), BV772=bv(sys_item, Ele774), AEnv768=[BV772|LEnv754], forall(member(Ele774, List_1_Get),  (nb_setarg(2, BV772, Ele774), get_var(AEnv768, key, IFTEST758), (IFTEST758\==[]->get_var(AEnv768, key, Key_Get761), get_var(AEnv768, sys_item, Item_Get762), f_apply(Key_Get761, [Item_Get762], TrueResult764), CAR1782=TrueResult764;get_var(AEnv768, sys_item, Item_Get763), CAR1782=Item_Get763), get_var(AEnv768, rest, Rest_Get767), get_var(AEnv768, sys_list_2, List_2_Get), f_apply(f_member, [CAR1782, List_2_Get, Rest_Get767], IFTEST756), (IFTEST756\==[]->_83110=[];get_var(AEnv768, sys_item, Item_Get769), get_var(AEnv768, sys_result, Result_Get770), ElseResult771=[Item_Get769|Result_Get770], set_var(AEnv768, sys_result, ElseResult771), _83110=ElseResult771))), get_var(LEnv754, sys_list_2, List_2_Get776), BV800=bv(sys_item, Ele802), AEnv797=[BV800|LEnv754], forall(member(Ele802, List_2_Get776),  (nb_setarg(2, BV800, Ele802), catch(((get_var(AEnv797, sys_list_1, List_1_Get777), BV793=bv(sys_elem, Ele795), BlockExitEnv791=[BV793|AEnv797], forall(member(Ele795, List_1_Get777),  (nb_setarg(2, BV793, Ele795), get_var(BlockExitEnv791, key, IFTEST780), (IFTEST780\==[]->get_var(BlockExitEnv791, key, Key_Get783), get_var(BlockExitEnv791, sys_elem, Elem_Get784), f_apply(Key_Get783, [Elem_Get784], TrueResult786), CAR1783=TrueResult786;get_var(BlockExitEnv791, sys_elem, Elem_Get785), CAR1783=Elem_Get785), get_var(BlockExitEnv791, rest, Rest_Get789), get_var(BlockExitEnv791, sys_item, Item_Get788), f_apply(f_satisfies1, [CAR1783, Item_Get788, Rest_Get789], IFTEST778), (IFTEST778\==[]->set_var(BlockExitEnv791, block_ret_sys_matches, []), always(block_exit_sys_matches, BlockExitEnv791);_83654=[]))), get_var(AEnv797, sys_item, Item_Get798), get_var(AEnv797, sys_result, Result_Get799), Result1668=[Item_Get798|Result_Get799], set_var(AEnv797, sys_result, Result1668)), Result1668=Block_exit_Ret), block_exit(sys_matches, Block_exit_Ret), true))), get_var(LEnv754, sys_result, LetResult753)), LetResult753=FnResult745), block_exit(set_exclusive_or, FnResult745), true))),
   set_opv(set_exclusive_or, symbol_function, f_set_exclusive_or),
   DefunResult806=set_exclusive_or,
   assert_lsp(nset_exclusive_or,
	      wl:lambda_def(defun, nset_exclusive_or, f_nset_exclusive_or, [sys_list_1, sys_list_2, c38_rest, rest, c38_key, key], [[let, [[sys_result, []], [list, []], [sys_item, []]], [tagbody, sys_start_1, [unless, sys_list_1, [go, sys_start_2]], [setf, sys_item, [car, sys_list_1]], [setf, list, sys_list_2], [setf, sys_prev, []], sys_start_1_in, [unless, list, [go, sys_end_1_in]], [let, [[sys_elem, [if, key, [funcall, key, [car, list]], [car, list]]]], [when, [apply, function(satisfies), sys_item, [if, key, [funcall, key, sys_elem], sys_elem], rest], [if, sys_prev, [setf, [cdr, sys_prev], [cdr, list]], [setf, sys_list_2, [cdr, list]]], [setf, sys_list_1, [cdr, sys_list_1]], [go, sys_start_1]]], [setf, sys_prev, list], [setf, list, [cdr, list]], [go, sys_start_1_in], sys_end_1_in, [setf, sys_item, [cdr, sys_list_1]], [setf, [cdr, sys_list_1], sys_result], [unless, sys_result, [setf, sys_end, sys_list_1]], [setf, sys_result, sys_list_1], [setf, sys_list_1, sys_item], [go, sys_start_1], sys_start_2, [return_from, nset_exclusive_or, [if, sys_end, [progn, [setf, [cdr, sys_end], sys_list_2], sys_result], sys_list_2]]]]])),
   assert_lsp(nset_exclusive_or,
	      wl:arglist_info(nset_exclusive_or, f_nset_exclusive_or, [sys_list_1, sys_list_2, c38_rest, rest, c38_key, key], arginfo{all:[sys_list_1, sys_list_2], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:[key], names:[sys_list_1, sys_list_2, rest, key], opt:0, req:[sys_list_1, sys_list_2], rest:[rest], sublists:0, whole:0})),
   assert_lsp(nset_exclusive_or, wl:init_args(2, f_nset_exclusive_or)),
   assert_lsp(nset_exclusive_or,
	      (f_nset_exclusive_or(List_1_In810, List_2_In811, RestNKeys808, FnResult807):-CDR1785=[bv(sys_list_1, List_1_In810), bv(sys_list_2, List_2_In811), bv(rest, RestNKeys808), bv(key, Key_In813)], get_kw([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)]|Env], RestNKeys808, key, key, Key_In813, []=Key_In813, Key_P809), catch(((LEnv816=[bv(sys_result, []), bv(list, []), bv(sys_item, [])|CDR1785], call_addr_block(LEnv816,  (push_label(sys_start_1), get_var(LEnv816, sys_list_1, IFTEST951), (IFTEST951\==[]->_88024=[];goto(sys_start_2, LEnv816), _88024=_GORES954), get_var(LEnv816, sys_list_1, List_1_Get957), f_car(List_1_Get957, Item), set_var(LEnv816, sys_item, Item), get_var(LEnv816, sys_list_2, List_2_Get958), set_var(LEnv816, list, List_2_Get958), set_var(LEnv816, sys_prev, []), push_label(sys_start_1_in), get_var(LEnv816, list, IFTEST960), (IFTEST960\==[]->_88240=[];goto(sys_end_1_in, LEnv816), _88240=_GORES963), get_var(LEnv816, key, IFTEST968), (IFTEST968\==[]->get_var(LEnv816, key, Key_Get971), get_var(LEnv816, list, List_Get972), f_car(List_Get972, Car_Ret1786), f_apply(Key_Get971, [Car_Ret1786], TrueResult974), Elem_Init976=TrueResult974;get_var(LEnv816, list, List_Get973), f_car(List_Get973, ElseResult975), Elem_Init976=ElseResult975), LEnv967=[bv(sys_elem, Elem_Init976)|LEnv816], get_var(LEnv967, key, IFTEST980), get_var(LEnv967, sys_item, Item_Get979), (IFTEST980\==[]->get_var(LEnv967, key, Key_Get983), get_var(LEnv967, sys_elem, Elem_Get984), f_apply(Key_Get983, [Elem_Get984], TrueResult986), CAR1787=TrueResult986;get_var(LEnv967, sys_elem, Elem_Get985), CAR1787=Elem_Get985), get_var(LEnv967, rest, Rest_Get988), f_apply(f_satisfies1, [Item_Get979, CAR1787, Rest_Get988], IFTEST977), (IFTEST977\==[]->get_var(LEnv967, sys_prev, IFTEST989), (IFTEST989\==[]->get_var(LEnv967, list, List_Get995), get_var(LEnv967, sys_prev, Prev_Get994), f_cdr(List_Get995, Cdr_Ret1788), f_rplacd(Prev_Get994, Cdr_Ret1788, TrueResult997), _88980=TrueResult997;get_var(LEnv967, list, List_Get996), f_cdr(List_Get996, ElseResult998), set_var(LEnv967, sys_list_2, ElseResult998), _88980=ElseResult998), get_var(LEnv967, sys_list_1, List_1_Get999), f_cdr(List_1_Get999, List_1), set_var(LEnv967, sys_list_1, List_1), goto(sys_start_1, LEnv967), LetResult966=_GORES1000;LetResult966=[]), get_var(LEnv816, list, List_Get1003), set_var(LEnv816, sys_prev, List_Get1003), get_var(LEnv816, list, List_Get1004), f_cdr(List_Get1004, List1671), set_var(LEnv816, list, List1671), goto(sys_start_1_in, LEnv816)), [addr(addr_tagbody_54_sys_start_1, sys_start_1, '$used', GoEnv822,  (get_var(GoEnv822, sys_list_1, IFTEST818), (IFTEST818\==[]->_89456=[];goto(sys_start_2, GoEnv822), _89456=_GORES821), get_var(GoEnv822, sys_list_1, List_1_Get824), f_car(List_1_Get824, Car_Ret1789), set_var(GoEnv822, sys_item, Car_Ret1789), get_var(GoEnv822, sys_list_2, List_2_Get825), set_var(GoEnv822, list, List_2_Get825), set_var(GoEnv822, sys_prev, []), push_label(sys_start_1_in), get_var(GoEnv822, list, IFTEST827), (IFTEST827\==[]->_89516=[];goto(sys_end_1_in, GoEnv822), _89516=_GORES830), get_var(GoEnv822, key, IFTEST836), (IFTEST836\==[]->get_var(GoEnv822, key, Key_Get839), get_var(GoEnv822, list, List_Get840), f_car(List_Get840, Car_Ret1790), f_apply(Key_Get839, [Car_Ret1790], TrueResult842), Bv_Ret=TrueResult842;get_var(GoEnv822, list, List_Get841), f_car(List_Get841, ElseResult843), Bv_Ret=ElseResult843), LEnv835=[bv(sys_elem, Bv_Ret)|GoEnv822], get_var(LEnv835, key, IFTEST848), get_var(LEnv835, sys_item, Item_Get847), (IFTEST848\==[]->get_var(LEnv835, key, Key_Get851), get_var(LEnv835, sys_elem, Elem_Get852), f_apply(Key_Get851, [Elem_Get852], TrueResult854), CAR1792=TrueResult854;get_var(LEnv835, sys_elem, Elem_Get853), CAR1792=Elem_Get853), get_var(LEnv835, rest, Rest_Get856), f_apply(f_satisfies1, [Item_Get847, CAR1792, Rest_Get856], IFTEST845), (IFTEST845\==[]->get_var(LEnv835, sys_prev, IFTEST857), (IFTEST857\==[]->get_var(LEnv835, list, List_Get863), get_var(LEnv835, sys_prev, Prev_Get862), f_cdr(List_Get863, Cdr_Ret1793), f_rplacd(Prev_Get862, Cdr_Ret1793, TrueResult865), _89806=TrueResult865;get_var(LEnv835, list, List_Get864), f_cdr(List_Get864, ElseResult866), set_var(LEnv835, sys_list_2, ElseResult866), _89806=ElseResult866), get_var(LEnv835, sys_list_1, List_1_Get867), f_cdr(List_1_Get867, Cdr_Ret1794), set_var(LEnv835, sys_list_1, Cdr_Ret1794), goto(sys_start_1, LEnv835), LetResult834=_GORES868;LetResult834=[]), get_var(GoEnv822, list, List_Get871), set_var(GoEnv822, sys_prev, List_Get871), get_var(GoEnv822, list, List_Get872), f_cdr(List_Get872, Cdr_Ret1795), set_var(GoEnv822, list, Cdr_Ret1795), goto(sys_start_1_in, GoEnv822))), addr(addr_tagbody_54_sys_start_1_in, sys_start_1_in, '$used', GoEnv878,  (get_var(GoEnv878, list, IFTEST874), (IFTEST874\==[]->_89938=[];goto(sys_end_1_in, GoEnv878), _89938=_GORES877), get_var(GoEnv878, key, IFTEST883), (IFTEST883\==[]->get_var(GoEnv878, key, Key_Get886), get_var(GoEnv878, list, List_Get887), f_car(List_Get887, Car_Ret1796), f_apply(Key_Get886, [Car_Ret1796], TrueResult889), Elem_Init891=TrueResult889;get_var(GoEnv878, list, List_Get888), f_car(List_Get888, ElseResult890), Elem_Init891=ElseResult890), LEnv882=[bv(sys_elem, Elem_Init891)|GoEnv878], get_var(LEnv882, key, IFTEST895), get_var(LEnv882, sys_item, Item_Get894), (IFTEST895\==[]->get_var(LEnv882, key, Key_Get898), get_var(LEnv882, sys_elem, Elem_Get899), f_apply(Key_Get898, [Elem_Get899], TrueResult901), CAR1797=TrueResult901;get_var(LEnv882, sys_elem, Elem_Get900), CAR1797=Elem_Get900), get_var(LEnv882, rest, Rest_Get903), f_apply(f_satisfies1, [Item_Get894, CAR1797, Rest_Get903], IFTEST892), (IFTEST892\==[]->get_var(LEnv882, sys_prev, IFTEST904), (IFTEST904\==[]->get_var(LEnv882, list, List_Get910), get_var(LEnv882, sys_prev, Prev_Get909), f_cdr(List_Get910, Cdr_Ret1798), f_rplacd(Prev_Get909, Cdr_Ret1798, TrueResult912), _90240=TrueResult912;get_var(LEnv882, list, List_Get911), f_cdr(List_Get911, ElseResult913), set_var(LEnv882, sys_list_2, ElseResult913), _90240=ElseResult913), get_var(LEnv882, sys_list_1, List_1_Get914), f_cdr(List_1_Get914, Cdr_Ret1799), set_var(LEnv882, sys_list_1, Cdr_Ret1799), goto(sys_start_1, LEnv882), LetResult881=_GORES915;LetResult881=[]), get_var(GoEnv878, list, List_Get918), set_var(GoEnv878, sys_prev, List_Get918), get_var(GoEnv878, list, List_Get919), f_cdr(List_Get919, Cdr_Ret1800), set_var(GoEnv878, list, Cdr_Ret1800), goto(sys_start_1_in, GoEnv878))), addr(addr_tagbody_54_sys_end_1_in, sys_end_1_in, '$unused', GoEnv935,  (get_var(GoEnv935, sys_list_1, List_1_Get922), f_cdr(List_1_Get922, Cdr_Ret1801), set_var(GoEnv935, sys_item, Cdr_Ret1801), get_var(GoEnv935, sys_list_1, List_1_Get925), get_var(GoEnv935, sys_result, Result_Get926), f_rplacd(List_1_Get925, Result_Get926, Rplacd_Ret1802), get_var(GoEnv935, sys_result, IFTEST927), (IFTEST927\==[]->_90418=[];get_var(GoEnv935, sys_list_1, List_1_Get930), set_var(GoEnv935, sys_end, List_1_Get930), _90418=List_1_Get930), get_var(GoEnv935, sys_list_1, List_1_Get932), set_var(GoEnv935, sys_result, List_1_Get932), get_var(GoEnv935, sys_item, Item_Get933), set_var(GoEnv935, sys_list_1, Item_Get933), goto(sys_start_1, GoEnv935))), addr(addr_tagbody_54_sys_start_2, sys_start_2, '$unused', BlockExitEnv949,  (get_var(BlockExitEnv949, sys_end, IFTEST938), (IFTEST938\==[]->get_var(BlockExitEnv949, sys_end, End_Get943), get_var(BlockExitEnv949, sys_list_2, List_2_Get944), f_rplacd(End_Get943, List_2_Get944, Rplacd_Ret1803), get_var(BlockExitEnv949, sys_result, Result_Get945), RetResult936=Result_Get945;get_var(BlockExitEnv949, sys_list_2, List_2_Get946), RetResult936=List_2_Get946), throw(block_exit(nset_exclusive_or, RetResult936))))])), []=FnResult807), block_exit(nset_exclusive_or, FnResult807), true))),
   set_opv(nset_exclusive_or, symbol_function, f_nset_exclusive_or),
   DefunResult1007=nset_exclusive_or,
   assert_lsp(fill,
	      wl:lambda_def(defun, fill, f_fill, [sequence, sys_item, c38_rest, rest], [[let, [[sys_iter, [apply, function(sys_seq_start), sequence, rest]]], [tagbody, sys_start, [unless, [apply, function(sys_seq_end_p), sequence, sys_iter, rest], [sys_seq_set, sequence, sys_iter, sys_item], [sys_seq_next, sys_iter], [go, sys_start]]]], sequence])),
   assert_lsp(fill,
	      wl:arglist_info(fill, f_fill, [sequence, sys_item, c38_rest, rest], arginfo{all:[sequence, sys_item], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, sys_item, rest], opt:0, req:[sequence, sys_item], rest:[rest], sublists:0, whole:0})),
   assert_lsp(fill, wl:init_args(2, f_fill)),
   assert_lsp(fill,
	      (f_fill(Sequence_In1010, Item_In1011, RestNKeys1009, FnResult1008):-GEnv1672=[bv(sequence, Sequence_In1010), bv(sys_item, Item_In1011), bv(rest, RestNKeys1009)], catch(((get_var(GEnv1672, rest, Rest_Get1017), get_var(GEnv1672, sequence, Sequence_Get1016), f_apply(f_sys_seq_start1, [Sequence_Get1016, Rest_Get1017], Iter_Init), LEnv1015=[bv(sys_iter, Iter_Init)|GEnv1672], call_addr_block(LEnv1015,  (push_label(sys_start), (get_var(LEnv1015, rest, Rest_Get1037), get_var(LEnv1015, sequence, Sequence_Get1035)), get_var(LEnv1015, sys_iter, Iter_Get1036), f_apply(f_sys_seq_end_p1, [Sequence_Get1035, Iter_Get1036, Rest_Get1037], IFTEST1033), (IFTEST1033\==[]->_TBResult1019=[];get_var(LEnv1015, sequence, Sequence_Get1038), get_var(LEnv1015, sys_item, Item_Get1040), get_var(LEnv1015, sys_iter, Iter_Get1039), f_sys_seq_set1(Sequence_Get1038, Iter_Get1039, Item_Get1040, KeysNRest), get_var(LEnv1015, sys_iter, Iter_Get1041), f_sys_seq_next1(Iter_Get1041, KeysNRest1696), goto(sys_start, LEnv1015), _TBResult1019=_GORES1042)), [addr(addr_tagbody_55_sys_start, sys_start, '$unused', GoEnv1030,  ((get_var(GoEnv1030, rest, Rest_Get1024), get_var(GoEnv1030, sequence, Sequence_Get1022)), get_var(GoEnv1030, sys_iter, Iter_Get1023), f_apply(f_sys_seq_end_p1, [Sequence_Get1022, Iter_Get1023, Rest_Get1024], IFTEST1020), (IFTEST1020\==[]->_TBResult1019=[];get_var(GoEnv1030, sequence, Sequence_Get1025), get_var(GoEnv1030, sys_item, Item_Get1027), get_var(GoEnv1030, sys_iter, Iter_Get1026), f_sys_seq_set1(Sequence_Get1025, Iter_Get1026, Item_Get1027, KeysNRest1697), get_var(GoEnv1030, sys_iter, Iter_Get1028), f_sys_seq_next1(Iter_Get1028, KeysNRest1698), goto(sys_start, GoEnv1030), _TBResult1019=_GORES1029)))]), get_var(GEnv1672, sequence, Sequence_Get1045)), Sequence_Get1045=FnResult1008), block_exit(fill, FnResult1008), true))),
   set_opv(fill, symbol_function, f_fill),
   DefunResult1047=fill,
   assert_lsp(every,
	      wl:lambda_def(defun, every, f_every, [sys_predicate, c38_rest, sys_sequences], [[let, [[sys_iters, [mapcar, function(sys_seq_start), sys_sequences]]], [tagbody, sys_start, [unless, [sys_some_list_2, function(sys_seq_end_p), sys_sequences, sys_iters], [unless, [apply, sys_predicate, [mapcar, function(sys_seq_ref), sys_sequences, sys_iters]], [return_from, every, []]], [mapc, function(sys_seq_next), sys_iters], [go, sys_start]]]], t])),
   assert_lsp(every,
	      wl:arglist_info(every, f_every, [sys_predicate, c38_rest, sys_sequences], arginfo{all:[sys_predicate], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_sequences], opt:0, req:[sys_predicate], rest:[sys_sequences], sublists:0, whole:0})),
   assert_lsp(every, wl:init_args(x, f_every)),
   assert_lsp(every,
	      (f_every(Predicate_In1050, FnResult1048):-GEnv1673=[bv(sys_predicate, Predicate_In1050), bv(sys_sequences, [])], catch(((get_var(GEnv1673, sys_sequences, Sequences_Get), f_mapcar(f_sys_seq_start1, [Sequences_Get], Iters_Init), BlockExitEnv=[bv(sys_iters, Iters_Init)|GEnv1673], call_addr_block(BlockExitEnv,  (push_label(sys_start), get_var(BlockExitEnv, sys_iters, Iters_Get1079), get_var(BlockExitEnv, sys_sequences, Sequences_Get1078), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1078, Iters_Get1079, IFTEST1076), (IFTEST1076\==[]->_TBResult1057=[];get_var(BlockExitEnv, sys_iters, Iters_Get1084), get_var(BlockExitEnv, sys_predicate, Predicate_Get1082), get_var(BlockExitEnv, sys_sequences, Sequences_Get1083), f_mapcar(f_sys_seq_ref1, [Sequences_Get1083, Iters_Get1084], Mapcar_Ret), f_apply(Predicate_Get1082, Mapcar_Ret, IFTEST1080), (IFTEST1080\==[]->_92546=[];throw(block_exit(every, [])), _92546=ThrowResult1086), get_var(BlockExitEnv, sys_iters, Iters_Get1089), f_mapc(f_sys_seq_next1, [Iters_Get1089], Mapc_Ret), goto(sys_start, BlockExitEnv), _TBResult1057=_GORES1090)), [addr(addr_tagbody_56_sys_start, sys_start, '$unused', BlockExitEnv1069,  (get_var(BlockExitEnv1069, sys_iters, Get_var_Ret1806), get_var(BlockExitEnv1069, sys_sequences, Sequences_Get1060), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1060, Get_var_Ret1806, IFTEST1058), (IFTEST1058\==[]->_TBResult1057=[];get_var(BlockExitEnv1069, sys_iters, Iters_Get1066), get_var(BlockExitEnv1069, sys_predicate, Predicate_Get1064), get_var(BlockExitEnv1069, sys_sequences, Sequences_Get1065), f_mapcar(f_sys_seq_ref1, [Sequences_Get1065, Iters_Get1066], Mapcar_Ret1807), f_apply(Predicate_Get1064, Mapcar_Ret1807, IFTEST1062), (IFTEST1062\==[]->_92962=[];throw(block_exit(every, [])), _92962=ThrowResult1068), get_var(BlockExitEnv1069, sys_iters, Iters_Get1071), f_mapc(f_sys_seq_next1, [Iters_Get1071], Mapc_Ret1808), goto(sys_start, BlockExitEnv1069), _TBResult1057=_GORES1072)))])), t=FnResult1048), block_exit(every, FnResult1048), true))),
   set_opv(every, symbol_function, f_every),
   DefunResult1094=every,
   assert_lsp(some,
	      wl:lambda_def(defun, some, f_some, [sys_predicate, c38_rest, sys_sequences], [[let, [[sys_iters, [mapcar, function(sys_seq_start), sys_sequences]]], [tagbody, sys_start, [unless, [sys_some_list_2, function(sys_seq_end_p), sys_sequences, sys_iters], [let, [[sys_result, [apply, sys_predicate, [mapcar, function(sys_seq_ref), sys_sequences, sys_iters]]]], [when, sys_result, [return_from, some, sys_result]]], [mapc, function(sys_seq_next), sys_iters], [go, sys_start]]]]])),
   assert_lsp(some,
	      wl:arglist_info(some, f_some, [sys_predicate, c38_rest, sys_sequences], arginfo{all:[sys_predicate], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_sequences], opt:0, req:[sys_predicate], rest:[sys_sequences], sublists:0, whole:0})),
   assert_lsp(some, wl:init_args(1, f_some)),
   assert_lsp(some,
	      (f_some(Predicate_In1097, RestNKeys1096, FnResult1095):-GEnv1674=[bv(sys_predicate, Predicate_In1097), bv(sys_sequences, RestNKeys1096)], catch(((get_var(GEnv1674, sys_sequences, Sequences_Get1102), f_mapcar(f_sys_seq_start1, [Sequences_Get1102], Iters_Init1103), LEnv1101=[bv(sys_iters, Iters_Init1103)|GEnv1674], call_addr_block(LEnv1101,  (push_label(sys_start), get_var(LEnv1101, sys_iters, Iters_Get1132), get_var(LEnv1101, sys_sequences, Sequences_Get1131), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1131, Iters_Get1132, IFTEST1129), (IFTEST1129\==[]->_TBResult1104=[];get_var(LEnv1101, sys_iters, Iters_Get1138), get_var(LEnv1101, sys_predicate, Predicate_Get1136), get_var(LEnv1101, sys_sequences, Sequences_Get1137), f_mapcar(f_sys_seq_ref1, [Sequences_Get1137, Iters_Get1138], Mapcar_Ret1809), f_apply(Predicate_Get1136, Mapcar_Ret1809, Result_Init1139), LEnv1135=[bv(sys_result, Result_Init1139)|LEnv1101], get_var(LEnv1135, sys_result, IFTEST1140), (IFTEST1140\==[]->get_var(LEnv1135, sys_result, RetResult1143), throw(block_exit(some, RetResult1143)), LetResult1134=ThrowResult1144;LetResult1134=[]), get_var(LEnv1101, sys_iters, Iters_Get1148), f_mapc(f_sys_seq_next1, [Iters_Get1148], Mapc_Ret1810), goto(sys_start, LEnv1101), _TBResult1104=_GORES1149)), [addr(addr_tagbody_57_sys_start, sys_start, '$unused', GoEnv1126,  (get_var(GoEnv1126, sys_iters, Iters_Get1108), get_var(GoEnv1126, sys_sequences, Sequences_Get1107), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1107, Iters_Get1108, IFTEST1105), (IFTEST1105\==[]->_TBResult1104=[];get_var(GoEnv1126, sys_iters, Iters_Get1114), get_var(GoEnv1126, sys_predicate, Predicate_Get1112), get_var(GoEnv1126, sys_sequences, Sequences_Get1113), f_mapcar(f_sys_seq_ref1, [Sequences_Get1113, Iters_Get1114], Mapcar_Ret1811), f_apply(Predicate_Get1112, Mapcar_Ret1811, Apply_Ret1812), LEnv1111=[bv(sys_result, Apply_Ret1812)|GoEnv1126], get_var(LEnv1111, sys_result, IFTEST1116), (IFTEST1116\==[]->get_var(LEnv1111, sys_result, RetResult1119), throw(block_exit(some, RetResult1119)), LetResult1110=ThrowResult1120;LetResult1110=[]), get_var(GoEnv1126, sys_iters, Iters_Get1124), f_mapc(f_sys_seq_next1, [Iters_Get1124], Mapc_Ret1813), goto(sys_start, GoEnv1126), _TBResult1104=_GORES1125)))])), []=FnResult1095), block_exit(some, FnResult1095), true))),
   set_opv(some, symbol_function, f_some),
   DefunResult1153=some,
   assert_lsp(notevery,
	      wl:lambda_def(defun, notevery, f_notevery, [sys_predicate, c38_rest, sys_sequences], [[let, [[sys_iters, [mapcar, function(sys_seq_start), sys_sequences]]], [tagbody, sys_start, [unless, [sys_some_list_2, function(sys_seq_end_p), sys_sequences, sys_iters], [unless, [apply, sys_predicate, [mapcar, function(sys_seq_ref), sys_sequences, sys_iters]], [return_from, every, t]], [mapc, function(sys_seq_next), sys_iters], [go, sys_start]]]]])),
   assert_lsp(notevery,
	      wl:arglist_info(notevery, f_notevery, [sys_predicate, c38_rest, sys_sequences], arginfo{all:[sys_predicate], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_sequences], opt:0, req:[sys_predicate], rest:[sys_sequences], sublists:0, whole:0})),
   assert_lsp(notevery, wl:init_args(1, f_notevery)),
   assert_lsp(notevery,
	      (f_notevery(Predicate_In1156, RestNKeys1155, FnResult1154):-GEnv1675=[bv(sys_predicate, Predicate_In1156), bv(sys_sequences, RestNKeys1155)], catch(((get_var(GEnv1675, sys_sequences, Sequences_Get1161), f_mapcar(f_sys_seq_start1, [Sequences_Get1161], Iters_Init1162), BlockExitEnv=[bv(sys_iters, Iters_Init1162)|GEnv1675], call_addr_block(BlockExitEnv,  (push_label(sys_start), get_var(BlockExitEnv, sys_iters, Iters_Get1185), get_var(BlockExitEnv, sys_sequences, Sequences_Get1184), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1184, Iters_Get1185, IFTEST1182), (IFTEST1182\==[]->_TBResult1163=[];get_var(BlockExitEnv, sys_iters, Iters_Get1190), get_var(BlockExitEnv, sys_predicate, Predicate_Get1188), get_var(BlockExitEnv, sys_sequences, Sequences_Get1189), f_mapcar(f_sys_seq_ref1, [Sequences_Get1189, Iters_Get1190], Mapcar_Ret1814), f_apply(Predicate_Get1188, Mapcar_Ret1814, IFTEST1186), (IFTEST1186\==[]->_95542=[];throw(block_exit(every, t)), _95542=ThrowResult1192), get_var(BlockExitEnv, sys_iters, Iters_Get1195), f_mapc(f_sys_seq_next1, [Iters_Get1195], Mapc_Ret1815), goto(sys_start, BlockExitEnv), _TBResult1163=_GORES1196)), [addr(addr_tagbody_58_sys_start, sys_start, '$unused', BlockExitEnv1175,  (get_var(BlockExitEnv1175, sys_iters, Iters_Get1167), get_var(BlockExitEnv1175, sys_sequences, Sequences_Get1166), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1166, Iters_Get1167, IFTEST1164), (IFTEST1164\==[]->_TBResult1163=[];get_var(BlockExitEnv1175, sys_iters, Iters_Get1172), get_var(BlockExitEnv1175, sys_predicate, Predicate_Get1170), get_var(BlockExitEnv1175, sys_sequences, Sequences_Get1171), f_mapcar(f_sys_seq_ref1, [Sequences_Get1171, Iters_Get1172], Mapcar_Ret1816), f_apply(Predicate_Get1170, Mapcar_Ret1816, IFTEST1168), (IFTEST1168\==[]->_95970=[];throw(block_exit(every, t)), _95970=ThrowResult1174), get_var(BlockExitEnv1175, sys_iters, Iters_Get1177), f_mapc(f_sys_seq_next1, [Iters_Get1177], Mapc_Ret1817), goto(sys_start, BlockExitEnv1175), _TBResult1163=_GORES1178)))])), []=FnResult1154), block_exit(notevery, FnResult1154), true))),
   set_opv(notevery, symbol_function, f_notevery),
   DefunResult1200=notevery,
   assert_lsp(notany,
	      wl:lambda_def(defun, notany, f_notany, [sys_predicate, c38_rest, sys_sequences], [[let, [[sys_iters, [mapcar, function(sys_seq_start), sys_sequences]]], [tagbody, sys_start, [unless, [sys_some_list_2, function(sys_seq_end_p), sys_sequences, sys_iters], [when, [apply, sys_predicate, [mapcar, function(sys_seq_ref), sys_sequences, sys_iters]], [return_from, every, []]], [mapc, function(sys_seq_next), sys_iters], [go, sys_start]]]], t])),
   assert_lsp(notany,
	      wl:arglist_info(notany, f_notany, [sys_predicate, c38_rest, sys_sequences], arginfo{all:[sys_predicate], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sys_sequences], opt:0, req:[sys_predicate], rest:[sys_sequences], sublists:0, whole:0})),
   assert_lsp(notany, wl:init_args(1, f_notany)),
   assert_lsp(notany,
	      (f_notany(Predicate_In1203, RestNKeys1202, FnResult1201):-GEnv1676=[bv(sys_predicate, Predicate_In1203), bv(sys_sequences, RestNKeys1202)], catch(((get_var(GEnv1676, sys_sequences, Sequences_Get1208), f_mapcar(f_sys_seq_start1, [Sequences_Get1208], Iters_Init1209), BlockExitEnv=[bv(sys_iters, Iters_Init1209)|GEnv1676], call_addr_block(BlockExitEnv,  (push_label(sys_start), get_var(BlockExitEnv, sys_iters, Iters_Get1232), get_var(BlockExitEnv, sys_sequences, Sequences_Get1231), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1231, Iters_Get1232, IFTEST1229), (IFTEST1229\==[]->_TBResult1210=[];get_var(BlockExitEnv, sys_iters, Iters_Get1237), get_var(BlockExitEnv, sys_predicate, Predicate_Get1235), get_var(BlockExitEnv, sys_sequences, Sequences_Get1236), f_mapcar(f_sys_seq_ref1, [Sequences_Get1236, Iters_Get1237], Mapcar_Ret1818), f_apply(Predicate_Get1235, Mapcar_Ret1818, IFTEST1233), (IFTEST1233\==[]->throw(block_exit(every, [])), _96862=ThrowResult1239;_96862=[]), get_var(BlockExitEnv, sys_iters, Iters_Get1242), f_mapc(f_sys_seq_next1, [Iters_Get1242], Mapc_Ret1819), goto(sys_start, BlockExitEnv), _TBResult1210=_GORES1243)), [addr(addr_tagbody_59_sys_start, sys_start, '$unused', BlockExitEnv1222,  (get_var(BlockExitEnv1222, sys_iters, Iters_Get1214), get_var(BlockExitEnv1222, sys_sequences, Sequences_Get1213), f_sys_some_list_2(f_sys_seq_end_p1, Sequences_Get1213, Iters_Get1214, IFTEST1211), (IFTEST1211\==[]->_TBResult1210=[];get_var(BlockExitEnv1222, sys_iters, Iters_Get1219), get_var(BlockExitEnv1222, sys_predicate, Predicate_Get1217), get_var(BlockExitEnv1222, sys_sequences, Sequences_Get1218), f_mapcar(f_sys_seq_ref1, [Sequences_Get1218, Iters_Get1219], Mapcar_Ret1820), f_apply(Predicate_Get1217, Mapcar_Ret1820, IFTEST1215), (IFTEST1215\==[]->throw(block_exit(every, [])), _97290=ThrowResult1221;_97290=[]), get_var(BlockExitEnv1222, sys_iters, Iters_Get1224), f_mapc(f_sys_seq_next1, [Iters_Get1224], Mapc_Ret1821), goto(sys_start, BlockExitEnv1222), _TBResult1210=_GORES1225)))])), t=FnResult1201), block_exit(notany, FnResult1201), true))),
   set_opv(notany, symbol_function, f_notany),
   DefunResult1247=notany,
   assert_lsp(count,
	      wl:lambda_def(defun, count, f_count, [sys_item, sequence, c38_rest, rest], [[let, [[sys_iter, [apply, function(sys_seq_start), sequence, rest]], [count, 0]], [tagbody, sys_start, [unless, [apply, function(sys_seq_end_p), sequence, sys_iter, rest], [when, [apply, function(satisfies), sys_item, [sys_seq_ref, sequence, sys_iter], rest], [setf, count, [+, 1, count]]], [sys_seq_next, sys_iter], [go, sys_start]]], count]])),
   assert_lsp(count,
	      wl:arglist_info(count, f_count, [sys_item, sequence, c38_rest, rest], arginfo{all:[sys_item, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_item, sequence, rest], opt:0, req:[sys_item, sequence], rest:[rest], sublists:0, whole:0})),
   assert_lsp(count, wl:init_args(2, f_count)),
   assert_lsp(count,
	      (f_count(Item_In1250, Sequence_In1251, RestNKeys1249, FnResult1248):-GEnv1677=[bv(sys_item, Item_In1250), bv(sequence, Sequence_In1251), bv(rest, RestNKeys1249)], catch(((get_var(GEnv1677, rest, Rest_Get1257), get_var(GEnv1677, sequence, Sequence_Get1256), f_apply(f_sys_seq_start1, [Sequence_Get1256, Rest_Get1257], Iter_Init1258), LEnv1255=[bv(sys_iter, Iter_Init1258), bv(count, 0)|GEnv1677], call_addr_block(LEnv1255,  (push_label(sys_start), (get_var(LEnv1255, rest, Rest_Get1282), get_var(LEnv1255, sequence, Sequence_Get1280)), get_var(LEnv1255, sys_iter, Iter_Get1281), f_apply(f_sys_seq_end_p1, [Sequence_Get1280, Iter_Get1281, Rest_Get1282], IFTEST1278), (IFTEST1278\==[]->_TBResult1259=[];get_var(LEnv1255, sequence, Sequence_Get1286), get_var(LEnv1255, sys_item, Item_Get1285), get_var(LEnv1255, sys_iter, Iter_Get1287), f_sys_seq_ref1(Sequence_Get1286, Iter_Get1287, KeysNRest1699), get_var(LEnv1255, rest, Rest_Get1288), f_apply(f_satisfies1, [Item_Get1285, KeysNRest1699, Rest_Get1288], IFTEST1283), (IFTEST1283\==[]->get_var(LEnv1255, count, Count_Get1289), 'f_+'(1, Count_Get1289, TrueResult1290), set_var(LEnv1255, count, TrueResult1290), _98280=TrueResult1290;_98280=[]), get_var(LEnv1255, sys_iter, Iter_Get1291), f_sys_seq_next1(Iter_Get1291, KeysNRest1700), goto(sys_start, LEnv1255), _TBResult1259=_GORES1292)), [addr(addr_tagbody_60_sys_start, sys_start, '$unused', GoEnv1275,  ((get_var(GoEnv1275, rest, Rest_Get1264), get_var(GoEnv1275, sequence, Sequence_Get1262)), get_var(GoEnv1275, sys_iter, Iter_Get1263), f_apply(f_sys_seq_end_p1, [Sequence_Get1262, Iter_Get1263, Rest_Get1264], IFTEST1260), (IFTEST1260\==[]->_TBResult1259=[];get_var(GoEnv1275, sequence, Sequence_Get1268), get_var(GoEnv1275, sys_item, Item_Get1267), get_var(GoEnv1275, sys_iter, Iter_Get1269), f_sys_seq_ref1(Sequence_Get1268, Iter_Get1269, KeysNRest1701), get_var(GoEnv1275, rest, Rest_Get1270), f_apply(f_satisfies1, [Item_Get1267, KeysNRest1701, Rest_Get1270], IFTEST1265), (IFTEST1265\==[]->get_var(GoEnv1275, count, Get_var_Ret1822), 'f_+'(1, Get_var_Ret1822, TrueResult1272), set_var(GoEnv1275, count, TrueResult1272), _98752=TrueResult1272;_98752=[]), get_var(GoEnv1275, sys_iter, Iter_Get1273), f_sys_seq_next1(Iter_Get1273, KeysNRest1702), goto(sys_start, GoEnv1275), _TBResult1259=_GORES1274)))]), get_var(LEnv1255, count, LetResult1254)), LetResult1254=FnResult1248), block_exit(count, FnResult1248), true))),
   set_opv(count, symbol_function, f_count),
   DefunResult1297=count,
   assert_lsp(count_if,
	      wl:lambda_def(defun, count_if, f_count_if, [sys_predicate, sequence, c38_rest, rest], [[let, [[sys_iter, [apply, function(sys_seq_start), sequence, rest]], [count, 0]], [tagbody, sys_start, [unless, [apply, function(sys_seq_end_p), sequence, sys_iter, rest], [when, [apply, function(sys_satisfies_if), sys_predicate, [sys_seq_ref, sequence, sys_iter], rest], [setf, count, [+, 1, count]]], [sys_seq_next, sys_iter], [go, sys_start]]], count]])),
   assert_lsp(count_if,
	      wl:arglist_info(count_if, f_count_if, [sys_predicate, sequence, c38_rest, rest], arginfo{all:[sys_predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sequence, rest], opt:0, req:[sys_predicate, sequence], rest:[rest], sublists:0, whole:0})),
   assert_lsp(count_if, wl:init_args(2, f_count_if)),
   assert_lsp(count_if,
	      (f_count_if(Predicate_In1300, Sequence_In1301, RestNKeys1299, FnResult1298):-GEnv1678=[bv(sys_predicate, Predicate_In1300), bv(sequence, Sequence_In1301), bv(rest, RestNKeys1299)], catch(((get_var(GEnv1678, rest, Rest_Get1307), get_var(GEnv1678, sequence, Sequence_Get1306), f_apply(f_sys_seq_start1, [Sequence_Get1306, Rest_Get1307], Iter_Init1308), LEnv1305=[bv(sys_iter, Iter_Init1308), bv(count, 0)|GEnv1678], call_addr_block(LEnv1305,  (push_label(sys_start), (get_var(LEnv1305, rest, Rest_Get1332), get_var(LEnv1305, sequence, Sequence_Get1330)), get_var(LEnv1305, sys_iter, Iter_Get1331), f_apply(f_sys_seq_end_p1, [Sequence_Get1330, Iter_Get1331, Rest_Get1332], IFTEST1328), (IFTEST1328\==[]->_TBResult1309=[];get_var(LEnv1305, sequence, Sequence_Get1336), get_var(LEnv1305, sys_iter, Iter_Get1337), get_var(LEnv1305, sys_predicate, Predicate_Get1335), f_sys_seq_ref1(Sequence_Get1336, Iter_Get1337, KeysNRest1703), get_var(LEnv1305, rest, Rest_Get1338), f_apply(f_sys_satisfies_if1, [Predicate_Get1335, KeysNRest1703, Rest_Get1338], IFTEST1333), (IFTEST1333\==[]->get_var(LEnv1305, count, Count_Get1339), 'f_+'(1, Count_Get1339, TrueResult1340), set_var(LEnv1305, count, TrueResult1340), _99768=TrueResult1340;_99768=[]), get_var(LEnv1305, sys_iter, Iter_Get1341), f_sys_seq_next1(Iter_Get1341, KeysNRest1704), goto(sys_start, LEnv1305), _TBResult1309=_GORES1342)), [addr(addr_tagbody_61_sys_start, sys_start, '$unused', GoEnv1325,  ((get_var(GoEnv1325, rest, Rest_Get1314), get_var(GoEnv1325, sequence, Sequence_Get1312)), get_var(GoEnv1325, sys_iter, Iter_Get1313), f_apply(f_sys_seq_end_p1, [Sequence_Get1312, Iter_Get1313, Rest_Get1314], IFTEST1310), (IFTEST1310\==[]->_TBResult1309=[];get_var(GoEnv1325, sequence, Sequence_Get1318), get_var(GoEnv1325, sys_iter, Iter_Get1319), get_var(GoEnv1325, sys_predicate, Predicate_Get1317), f_sys_seq_ref1(Sequence_Get1318, Iter_Get1319, KeysNRest1705), get_var(GoEnv1325, rest, Rest_Get1320), f_apply(f_sys_satisfies_if1, [Predicate_Get1317, KeysNRest1705, Rest_Get1320], IFTEST1315), (IFTEST1315\==[]->get_var(GoEnv1325, count, Count_Get1321), 'f_+'(1, Count_Get1321, TrueResult1322), set_var(GoEnv1325, count, TrueResult1322), _100252=TrueResult1322;_100252=[]), get_var(GoEnv1325, sys_iter, Iter_Get1323), f_sys_seq_next1(Iter_Get1323, KeysNRest1706), goto(sys_start, GoEnv1325), _TBResult1309=_GORES1324)))]), get_var(LEnv1305, count, LetResult1304)), LetResult1304=FnResult1298), block_exit(count_if, FnResult1298), true))),
   set_opv(count_if, symbol_function, f_count_if),
   DefunResult1347=count_if,
   assert_lsp(count_if_not,
	      wl:lambda_def(defun, count_if_not, f_count_if_not, [sys_predicate, sequence, c38_rest, rest], [[let, [[sys_iter, [apply, function(sys_seq_start), sequence, rest]], [count, 0]], [tagbody, sys_start, [unless, [apply, function(sys_seq_end_p), sequence, sys_iter, rest], [when, [apply, function(sys_satisfies_if_not), sys_predicate, [sys_seq_ref, sequence, sys_iter], rest], [setf, count, [+, 1, count]]], [sys_seq_next, sys_iter], [go, sys_start]]], count]])),
   assert_lsp(count_if_not,
	      wl:arglist_info(count_if_not, f_count_if_not, [sys_predicate, sequence, c38_rest, rest], arginfo{all:[sys_predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_predicate, sequence, rest], opt:0, req:[sys_predicate, sequence], rest:[rest], sublists:0, whole:0})),
   assert_lsp(count_if_not, wl:init_args(2, f_count_if_not)),
   assert_lsp(count_if_not,
	      (f_count_if_not(Predicate_In1350, Sequence_In1351, RestNKeys1349, FnResult1348):-GEnv1679=[bv(sys_predicate, Predicate_In1350), bv(sequence, Sequence_In1351), bv(rest, RestNKeys1349)], catch(((get_var(GEnv1679, rest, Rest_Get1357), get_var(GEnv1679, sequence, Sequence_Get1356), f_apply(f_sys_seq_start1, [Sequence_Get1356, Rest_Get1357], Iter_Init1358), LEnv1355=[bv(sys_iter, Iter_Init1358), bv(count, 0)|GEnv1679], call_addr_block(LEnv1355,  (push_label(sys_start), (get_var(LEnv1355, rest, Rest_Get1382), get_var(LEnv1355, sequence, Sequence_Get1380)), get_var(LEnv1355, sys_iter, Iter_Get1381), f_apply(f_sys_seq_end_p1, [Sequence_Get1380, Iter_Get1381, Rest_Get1382], IFTEST1378), (IFTEST1378\==[]->_TBResult1359=[];get_var(LEnv1355, sequence, Sequence_Get1386), get_var(LEnv1355, sys_iter, Iter_Get1387), get_var(LEnv1355, sys_predicate, Predicate_Get1385), f_sys_seq_ref1(Sequence_Get1386, Iter_Get1387, KeysNRest1707), get_var(LEnv1355, rest, Rest_Get1388), f_apply(f_sys_satisfies_if_not1, [Predicate_Get1385, KeysNRest1707, Rest_Get1388], IFTEST1383), (IFTEST1383\==[]->get_var(LEnv1355, count, Count_Get1389), 'f_+'(1, Count_Get1389, TrueResult1390), set_var(LEnv1355, count, TrueResult1390), _101268=TrueResult1390;_101268=[]), get_var(LEnv1355, sys_iter, Iter_Get1391), f_sys_seq_next1(Iter_Get1391, KeysNRest1708), goto(sys_start, LEnv1355), _TBResult1359=_GORES1392)), [addr(addr_tagbody_62_sys_start, sys_start, '$unused', GoEnv1375,  ((get_var(GoEnv1375, rest, Rest_Get1364), get_var(GoEnv1375, sequence, Sequence_Get1362)), get_var(GoEnv1375, sys_iter, Iter_Get1363), f_apply(f_sys_seq_end_p1, [Sequence_Get1362, Iter_Get1363, Rest_Get1364], IFTEST1360), (IFTEST1360\==[]->_TBResult1359=[];get_var(GoEnv1375, sequence, Sequence_Get1368), get_var(GoEnv1375, sys_iter, Iter_Get1369), get_var(GoEnv1375, sys_predicate, Predicate_Get1367), f_sys_seq_ref1(Sequence_Get1368, Iter_Get1369, KeysNRest1709), get_var(GoEnv1375, rest, Rest_Get1370), f_apply(f_sys_satisfies_if_not1, [Predicate_Get1367, KeysNRest1709, Rest_Get1370], IFTEST1365), (IFTEST1365\==[]->get_var(GoEnv1375, count, Count_Get1371), 'f_+'(1, Count_Get1371, TrueResult1372), set_var(GoEnv1375, count, TrueResult1372), _101752=TrueResult1372;_101752=[]), get_var(GoEnv1375, sys_iter, Iter_Get1373), f_sys_seq_next1(Iter_Get1373, KeysNRest1710), goto(sys_start, GoEnv1375), _TBResult1359=_GORES1374)))]), get_var(LEnv1355, count, LetResult1354)), LetResult1354=FnResult1348), block_exit(count_if_not, FnResult1348), true))),
   set_opv(count_if_not, symbol_function, f_count_if_not),
   DefunResult1397=count_if_not,
   assert_lsp(remove,
	      wl:lambda_def(defun, remove, f_remove, [sys_item, sequence, c38_rest, rest, c38_key, count], [[let, [[sys_iter, [apply, function(sys_seq_start), sequence, rest]], [sys_result, []]], [tagbody, sys_start, [unless, [apply, function(sys_seq_end_p), sequence, sys_iter, rest], [let, [[sys_elem, [sys_seq_ref, sequence, sys_iter]]], [unless, [and, [apply, function(satisfies), sys_item, sys_elem, rest], [or, [not, count], [not, [minusp, [decf, count]]]]], [push, sys_elem, sys_result]]], [sys_seq_next, sys_iter], [go, sys_start]]], [sys_seq_result, sequence, sys_iter, sys_result]]])),
   assert_lsp(remove,
	      wl:arglist_info(remove, f_remove, [sys_item, sequence, c38_rest, rest, c38_key, count], arginfo{all:[sys_item, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:[count], names:[sys_item, sequence, rest, count], opt:0, req:[sys_item, sequence], rest:[rest], sublists:0, whole:0})),
   assert_lsp(remove, wl:init_args(2, f_remove)),
   assert_lsp(remove,
	      (f_remove(Item_In1401, Sequence_In1402, RestNKeys1399, FnResult1398):-GEnv1680=[bv(sys_item, Item_In1401), bv(sequence, Sequence_In1402), bv(rest, RestNKeys1399), bv(count, Count_In)], get_kw([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)]|Env], RestNKeys1399, count, count, Count_In, []=Count_In, Count_P), catch(((get_var(GEnv1680, rest, Rest_Get1409), get_var(GEnv1680, sequence, Sequence_Get1408), f_apply(f_sys_seq_start1, [Sequence_Get1408, Rest_Get1409], Iter_Init1410), LEnv1407=[bv(sys_iter, Iter_Init1410), bv(sys_result, [])|GEnv1680], call_addr_block(LEnv1407,  (push_label(sys_start), (get_var(LEnv1407, rest, Rest_Get1448), get_var(LEnv1407, sequence, Sequence_Get1446)), get_var(LEnv1407, sys_iter, Iter_Get1447), f_apply(f_sys_seq_end_p1, [Sequence_Get1446, Iter_Get1447, Rest_Get1448], IFTEST1444), (IFTEST1444\==[]->_TBResult1411=[];get_var(LEnv1407, sequence, Sequence_Get1452), get_var(LEnv1407, sys_iter, Iter_Get1453), f_sys_seq_ref1(Sequence_Get1452, Iter_Get1453, Elem_Init1454), Decf_Env=[bv(sys_elem, Elem_Init1454)|LEnv1407], get_var(Decf_Env, rest, Rest_Get1461), get_var(Decf_Env, sys_elem, Elem_Get1460), get_var(Decf_Env, sys_item, Item_Get1459), f_apply(f_satisfies1, [Item_Get1459, Elem_Get1460, Rest_Get1461], IFTEST1457), (IFTEST1457\==[]->(get_var(Decf_Env, count, Count_Get1462), f_not(Count_Get1462, FORM1_Res1465), FORM1_Res1465\==[], TrueResult1466=FORM1_Res1465->true;set_place(Decf_Env, decf, [value, count], [], Decf_R1463), f_minusp(Decf_R1463, Not_Param1688), f_not(Not_Param1688, Not_Ret1823), TrueResult1466=Not_Ret1823), IFTEST1455=TrueResult1466;IFTEST1455=[]), (IFTEST1455\==[]->LetResult1450=[];get_var(Decf_Env, sys_elem, Elem_Get1468), get_var(Decf_Env, sys_result, Result_Get1469), ElseResult1470=[Elem_Get1468|Result_Get1469], set_var(Decf_Env, sys_result, ElseResult1470), LetResult1450=ElseResult1470), get_var(LEnv1407, sys_iter, Iter_Get1471), f_sys_seq_next1(Iter_Get1471, KeysNRest1711), goto(sys_start, LEnv1407), _TBResult1411=_GORES1472)), [addr(addr_tagbody_63_sys_start, sys_start, '$unused', GoEnv1441,  ((get_var(GoEnv1441, rest, Rest_Get1416), get_var(GoEnv1441, sequence, Sequence_Get1414)), get_var(GoEnv1441, sys_iter, Iter_Get1415), f_apply(f_sys_seq_end_p1, [Sequence_Get1414, Iter_Get1415, Rest_Get1416], IFTEST1412), (IFTEST1412\==[]->_TBResult1411=[];get_var(GoEnv1441, sequence, Sequence_Get1420), get_var(GoEnv1441, sys_iter, Iter_Get1421), f_sys_seq_ref1(Sequence_Get1420, Iter_Get1421, Elem_Init1422), Decf_Env=[bv(sys_elem, Elem_Init1422)|GoEnv1441], get_var(Decf_Env, rest, Rest_Get1429), get_var(Decf_Env, sys_elem, Elem_Get1428), get_var(Decf_Env, sys_item, Item_Get1427), f_apply(f_satisfies1, [Item_Get1427, Elem_Get1428, Rest_Get1429], IFTEST1425), (IFTEST1425\==[]->(get_var(Decf_Env, count, Count_Get1430), f_not(Count_Get1430, FORM1_Res1433), FORM1_Res1433\==[], TrueResult1434=FORM1_Res1433->true;set_place(Decf_Env, decf, [value, count], [], Minusp_Param), f_minusp(Minusp_Param, Not_Param1690), f_not(Not_Param1690, Not_Ret1824), TrueResult1434=Not_Ret1824), IFTEST1423=TrueResult1434;IFTEST1423=[]), (IFTEST1423\==[]->LetResult1418=[];get_var(Decf_Env, sys_elem, Elem_Get1436), get_var(Decf_Env, sys_result, Result_Get1437), ElseResult1438=[Elem_Get1436|Result_Get1437], set_var(Decf_Env, sys_result, ElseResult1438), LetResult1418=ElseResult1438), get_var(GoEnv1441, sys_iter, Iter_Get1439), f_sys_seq_next1(Iter_Get1439, KeysNRest1712), goto(sys_start, GoEnv1441), _TBResult1411=_GORES1440)))]), get_var(LEnv1407, sequence, Sequence_Get1475), get_var(LEnv1407, sys_iter, Iter_Get1476), get_var(LEnv1407, sys_result, Result_Get1477), f_sys_seq_result1(Sequence_Get1475, Iter_Get1476, Result_Get1477, LetResult1406)), LetResult1406=FnResult1398), block_exit(remove, FnResult1398), true))),
   set_opv(remove, symbol_function, f_remove),
   DefunResult1479=remove,
   assert_lsp(remove_if,
	      wl:lambda_def(defun, remove_if, f_remove_if, [sys_predicate, sequence, c38_rest, rest, c38_key, count], [[let, [[sys_iter, [apply, function(sys_seq_start), sequence, rest]], [sys_result, []]], [tagbody, sys_start, [unless, [apply, function(sys_seq_end_p), sequence, sys_iter, rest], [let, [[sys_elem, [sys_seq_ref, sequence, sys_iter]]], [unless, [and, [apply, function(sys_satisfies_if), sys_predicate, sys_elem, rest], [or, [not, count], [not, [minusp, [decf, count]]]]], [push, sys_elem, sys_result]]], [sys_seq_next, sys_iter], [go, sys_start]]], [sys_seq_result, sequence, sys_iter, sys_result]]])),
   assert_lsp(remove_if,
	      wl:arglist_info(remove_if, f_remove_if, [sys_predicate, sequence, c38_rest, rest, c38_key, count], arginfo{all:[sys_predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:[count], names:[sys_predicate, sequence, rest, count], opt:0, req:[sys_predicate, sequence], rest:[rest], sublists:0, whole:0})),
   assert_lsp(remove_if, wl:init_args(2, f_remove_if)),
   assert_lsp(remove_if,
	      (f_remove_if(Predicate_In1483, Sequence_In1484, RestNKeys1481, FnResult1480):-GEnv1681=[bv(sys_predicate, Predicate_In1483), bv(sequence, Sequence_In1484), bv(rest, RestNKeys1481), bv(count, Count_In1486)], get_kw([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)]|Env], RestNKeys1481, count, count, Count_In1486, []=Count_In1486, Count_P1482), catch(((get_var(GEnv1681, rest, Rest_Get1491), get_var(GEnv1681, sequence, Sequence_Get1490), f_apply(f_sys_seq_start1, [Sequence_Get1490, Rest_Get1491], Iter_Init1492), LEnv1489=[bv(sys_iter, Iter_Init1492), bv(sys_result, [])|GEnv1681], call_addr_block(LEnv1489,  (push_label(sys_start), (get_var(LEnv1489, rest, Rest_Get1530), get_var(LEnv1489, sequence, Sequence_Get1528)), get_var(LEnv1489, sys_iter, Iter_Get1529), f_apply(f_sys_seq_end_p1, [Sequence_Get1528, Iter_Get1529, Rest_Get1530], IFTEST1526), (IFTEST1526\==[]->_TBResult1493=[];get_var(LEnv1489, sequence, Sequence_Get1534), get_var(LEnv1489, sys_iter, Iter_Get1535), f_sys_seq_ref1(Sequence_Get1534, Iter_Get1535, Elem_Init1536), Decf_Env=[bv(sys_elem, Elem_Init1536)|LEnv1489], get_var(Decf_Env, rest, Rest_Get1543), get_var(Decf_Env, sys_elem, Elem_Get1542), get_var(Decf_Env, sys_predicate, Predicate_Get1541), f_apply(f_sys_satisfies_if1, [Predicate_Get1541, Elem_Get1542, Rest_Get1543], IFTEST1539), (IFTEST1539\==[]->(get_var(Decf_Env, count, Count_Get1544), f_not(Count_Get1544, FORM1_Res1547), FORM1_Res1547\==[], TrueResult1548=FORM1_Res1547->true;set_place(Decf_Env, decf, [value, count], [], Decf_R1545), f_minusp(Decf_R1545, Not_Param1691), f_not(Not_Param1691, Not_Ret1825), TrueResult1548=Not_Ret1825), IFTEST1537=TrueResult1548;IFTEST1537=[]), (IFTEST1537\==[]->LetResult1532=[];get_var(Decf_Env, sys_elem, Elem_Get1550), get_var(Decf_Env, sys_result, Result_Get1551), ElseResult1552=[Elem_Get1550|Result_Get1551], set_var(Decf_Env, sys_result, ElseResult1552), LetResult1532=ElseResult1552), get_var(LEnv1489, sys_iter, Iter_Get1553), f_sys_seq_next1(Iter_Get1553, KeysNRest1713), goto(sys_start, LEnv1489), _TBResult1493=_GORES1554)), [addr(addr_tagbody_64_sys_start, sys_start, '$unused', GoEnv1523,  ((get_var(GoEnv1523, rest, Rest_Get1498), get_var(GoEnv1523, sequence, Sequence_Get1496)), get_var(GoEnv1523, sys_iter, Iter_Get1497), f_apply(f_sys_seq_end_p1, [Sequence_Get1496, Iter_Get1497, Rest_Get1498], IFTEST1494), (IFTEST1494\==[]->_TBResult1493=[];get_var(GoEnv1523, sequence, Sequence_Get1502), get_var(GoEnv1523, sys_iter, Iter_Get1503), f_sys_seq_ref1(Sequence_Get1502, Iter_Get1503, Elem_Init1504), Decf_Env=[bv(sys_elem, Elem_Init1504)|GoEnv1523], get_var(Decf_Env, rest, Rest_Get1511), get_var(Decf_Env, sys_elem, Elem_Get1510), get_var(Decf_Env, sys_predicate, Predicate_Get1509), f_apply(f_sys_satisfies_if1, [Predicate_Get1509, Elem_Get1510, Rest_Get1511], IFTEST1507), (IFTEST1507\==[]->(get_var(Decf_Env, count, Count_Get1512), f_not(Count_Get1512, FORM1_Res1515), FORM1_Res1515\==[], TrueResult1516=FORM1_Res1515->true;set_place(Decf_Env, decf, [value, count], [], Decf_R1513), f_minusp(Decf_R1513, Not_Param1692), f_not(Not_Param1692, Not_Ret1826), TrueResult1516=Not_Ret1826), IFTEST1505=TrueResult1516;IFTEST1505=[]), (IFTEST1505\==[]->LetResult1500=[];get_var(Decf_Env, sys_elem, Elem_Get1518), get_var(Decf_Env, sys_result, Result_Get1519), ElseResult1520=[Elem_Get1518|Result_Get1519], set_var(Decf_Env, sys_result, ElseResult1520), LetResult1500=ElseResult1520), get_var(GoEnv1523, sys_iter, Iter_Get1521), f_sys_seq_next1(Iter_Get1521, KeysNRest1714), goto(sys_start, GoEnv1523), _TBResult1493=_GORES1522)))]), get_var(LEnv1489, sequence, Sequence_Get1557), get_var(LEnv1489, sys_iter, Iter_Get1558), get_var(LEnv1489, sys_result, Result_Get1559), f_sys_seq_result1(Sequence_Get1557, Iter_Get1558, Result_Get1559, LetResult1488)), LetResult1488=FnResult1480), block_exit(remove_if, FnResult1480), true))),
   set_opv(remove_if, symbol_function, f_remove_if),
   DefunResult1561=remove_if,
   assert_lsp(remove_if_not,
	      wl:lambda_def(defun, remove_if_not, f_remove_if_not, [sys_predicate, sequence, c38_rest, rest, c38_key, count], [[let, [[sys_iter, [apply, function(sys_seq_start), sequence, rest]], [sys_result, []]], [tagbody, sys_start, [unless, [apply, function(sys_seq_end_p), sequence, sys_iter, rest], [let, [[sys_elem, [sys_seq_ref, sequence, sys_iter]]], [unless, [and, [apply, function(sys_satisfies_if_not), sys_predicate, sys_elem, rest], [or, [not, count], [not, [minusp, [decf, count]]]]], [push, sys_elem, sys_result]]], [sys_seq_next, sys_iter], [go, sys_start]]], [sys_seq_result, sequence, sys_iter, sys_result]]])),
   assert_lsp(remove_if_not,
	      wl:arglist_info(remove_if_not, f_remove_if_not, [sys_predicate, sequence, c38_rest, rest, c38_key, count], arginfo{all:[sys_predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:[count], names:[sys_predicate, sequence, rest, count], opt:0, req:[sys_predicate, sequence], rest:[rest], sublists:0, whole:0})),
   assert_lsp(remove_if_not, wl:init_args(2, f_remove_if_not)),
   assert_lsp(remove_if_not,
	      (f_remove_if_not(Predicate_In1565, Sequence_In1566, RestNKeys1563, FnResult1562):-GEnv1682=[bv(sys_predicate, Predicate_In1565), bv(sequence, Sequence_In1566), bv(rest, RestNKeys1563), bv(count, Count_In1568)], get_kw([[fbound(satisfies, kw_function)=function(f_satisfies1), fbound(sys_satisfies_if, kw_function)=function(f_sys_satisfies_if1), fbound(sys_satisfies_if_not, kw_function)=function(f_sys_satisfies_if_not1), fbound(sys_seq_start, kw_function)=function(f_sys_seq_start1), fbound(sys_seq_position, kw_function)=function(f_sys_seq_position1), fbound(sys_seq_next, kw_function)=function(f_sys_seq_next1), fbound(sys_seq_ref, kw_function)=function(f_sys_seq_ref1), fbound(sys_seq_set, kw_function)=function(f_sys_seq_set1), fbound(sys_seq_end_p, kw_function)=function(f_sys_seq_end_p1), fbound(sys_seq_result, kw_function)=function(f_sys_seq_result1)]|Env], RestNKeys1563, count, count, Count_In1568, []=Count_In1568, Count_P1564), catch(((get_var(GEnv1682, rest, Rest_Get1573), get_var(GEnv1682, sequence, Sequence_Get1572), f_apply(f_sys_seq_start1, [Sequence_Get1572, Rest_Get1573], Iter_Init1574), LEnv1571=[bv(sys_iter, Iter_Init1574), bv(sys_result, [])|GEnv1682], call_addr_block(LEnv1571,  (push_label(sys_start), (get_var(LEnv1571, rest, Rest_Get1612), get_var(LEnv1571, sequence, Sequence_Get1610)), get_var(LEnv1571, sys_iter, Iter_Get1611), f_apply(f_sys_seq_end_p1, [Sequence_Get1610, Iter_Get1611, Rest_Get1612], IFTEST1608), (IFTEST1608\==[]->_TBResult1575=[];get_var(LEnv1571, sequence, Sequence_Get1616), get_var(LEnv1571, sys_iter, Iter_Get1617), f_sys_seq_ref1(Sequence_Get1616, Iter_Get1617, Elem_Init1618), Decf_Env=[bv(sys_elem, Elem_Init1618)|LEnv1571], get_var(Decf_Env, rest, Rest_Get1625), get_var(Decf_Env, sys_elem, Elem_Get1624), get_var(Decf_Env, sys_predicate, Predicate_Get1623), f_apply(f_sys_satisfies_if_not1, [Predicate_Get1623, Elem_Get1624, Rest_Get1625], IFTEST1621), (IFTEST1621\==[]->(get_var(Decf_Env, count, Count_Get1626), f_not(Count_Get1626, FORM1_Res1629), FORM1_Res1629\==[], TrueResult1630=FORM1_Res1629->true;set_place(Decf_Env, decf, [value, count], [], Decf_R1627), f_minusp(Decf_R1627, Not_Param1693), f_not(Not_Param1693, Not_Ret1827), TrueResult1630=Not_Ret1827), IFTEST1619=TrueResult1630;IFTEST1619=[]), (IFTEST1619\==[]->LetResult1614=[];get_var(Decf_Env, sys_elem, Elem_Get1632), get_var(Decf_Env, sys_result, Result_Get1633), ElseResult1634=[Elem_Get1632|Result_Get1633], set_var(Decf_Env, sys_result, ElseResult1634), LetResult1614=ElseResult1634), get_var(LEnv1571, sys_iter, Iter_Get1635), f_sys_seq_next1(Iter_Get1635, KeysNRest1715), goto(sys_start, LEnv1571), _TBResult1575=_GORES1636)), [addr(addr_tagbody_65_sys_start, sys_start, '$unused', GoEnv1605,  ((get_var(GoEnv1605, rest, Rest_Get1580), get_var(GoEnv1605, sequence, Sequence_Get1578)), get_var(GoEnv1605, sys_iter, Iter_Get1579), f_apply(f_sys_seq_end_p1, [Sequence_Get1578, Iter_Get1579, Rest_Get1580], IFTEST1576), (IFTEST1576\==[]->_TBResult1575=[];get_var(GoEnv1605, sequence, Sequence_Get1584), get_var(GoEnv1605, sys_iter, Iter_Get1585), f_sys_seq_ref1(Sequence_Get1584, Iter_Get1585, Elem_Init1586), Decf_Env=[bv(sys_elem, Elem_Init1586)|GoEnv1605], get_var(Decf_Env, rest, Rest_Get1593), get_var(Decf_Env, sys_elem, Elem_Get1592), get_var(Decf_Env, sys_predicate, Predicate_Get1591), f_apply(f_sys_satisfies_if_not1, [Predicate_Get1591, Elem_Get1592, Rest_Get1593], IFTEST1589), (IFTEST1589\==[]->(get_var(Decf_Env, count, Count_Get1594), f_not(Count_Get1594, FORM1_Res1597), FORM1_Res1597\==[], TrueResult1598=FORM1_Res1597->true;set_place(Decf_Env, decf, [value, count], [], Decf_R1595), f_minusp(Decf_R1595, Not_Param1694), f_not(Not_Param1694, Not_Ret1828), TrueResult1598=Not_Ret1828), IFTEST1587=TrueResult1598;IFTEST1587=[]), (IFTEST1587\==[]->LetResult1582=[];get_var(Decf_Env, sys_elem, Elem_Get1600), get_var(Decf_Env, sys_result, Result_Get1601), ElseResult1602=[Elem_Get1600|Result_Get1601], set_var(Decf_Env, sys_result, ElseResult1602), LetResult1582=ElseResult1602), get_var(GoEnv1605, sys_iter, Iter_Get1603), f_sys_seq_next1(Iter_Get1603, KeysNRest1716), goto(sys_start, GoEnv1605), _TBResult1575=_GORES1604)))]), get_var(LEnv1571, sequence, Sequence_Get1639), get_var(LEnv1571, sys_iter, Iter_Get1640), get_var(LEnv1571, sys_result, Result_Get1641), f_sys_seq_result1(Sequence_Get1639, Iter_Get1640, Result_Get1641, LetResult1570)), LetResult1570=FnResult1562), block_exit(remove_if_not, FnResult1562), true))),
   set_opv(remove_if_not, symbol_function, f_remove_if_not),
   DefunResult1643=remove_if_not.
/*
:- side_effect(assert_lsp(satisfies,
			  lambda_def(defun,
				     satisfies,
				     f_satisfies1,
				     
				     [ sys_object,
				       sys_elem,
				       c38_key,
				       key,
				       sys_test,
				       sys_test_not
				     ],
				     
				     [ 
				       [ let_xx,
					 
					 [ 
					   [ sys_zi,
					     
					     [ if,
					       key,
					       [funcall, key, sys_elem],
					       sys_elem
					     ]
					   ],
					   
					   [ sys_r,
					     
					     [ funcall,
					       
					       [ or,
						 sys_test,
						 sys_test_not,
						 function(eql)
					       ],
					       sys_object,
					       sys_zi
					     ]
					   ]
					 ],
					 [if, sys_test_not, [not, sys_r], sys_r]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(satisfies,
			  arglist_info(satisfies,
				       f_satisfies1,
				       
				       [ sys_object,
					 sys_elem,
					 c38_key,
					 key,
					 sys_test,
					 sys_test_not
				       ],
				       arginfo{ all:[sys_object, sys_elem],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ key,
						      sys_test,
						      sys_test_not
						    ],
						names:
						      [ sys_object,
							sys_elem,
							key,
							sys_test,
							sys_test_not
						      ],
						opt:0,
						req:[sys_object, sys_elem],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(satisfies, init_args(2, f_satisfies1))).
*/
/*
:- side_effect(assert_lsp(sys_satisfies_if,
			  lambda_def(defun,
				     sys_satisfies_if,
				     f_sys_satisfies_if1,
				     [sys_predicate, sys_elem, c38_key, key],
				     
				     [ 
				       [ funcall,
					 sys_predicate,
					 
					 [ if,
					   key,
					   [funcall, key, sys_elem],
					   sys_elem
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_satisfies_if,
			  arglist_info(sys_satisfies_if,
				       f_sys_satisfies_if1,
				       [sys_predicate, sys_elem, c38_key, key],
				       arginfo{ all:[sys_predicate, sys_elem],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:
						      [ sys_predicate,
							sys_elem,
							key
						      ],
						opt:0,
						req:[sys_predicate, sys_elem],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_satisfies_if, init_args(2, f_sys_satisfies_if1))).
*/
/*
:- side_effect(assert_lsp(sys_satisfies_if_not,
			  lambda_def(defun,
				     sys_satisfies_if_not,
				     f_sys_satisfies_if_not1,
				     [sys_predicate, sys_elem, c38_key, key],
				     
				     [ 
				       [ not,
					 
					 [ funcall,
					   sys_predicate,
					   
					   [ if,
					     key,
					     [funcall, key, sys_elem],
					     sys_elem
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_satisfies_if_not,
			  arglist_info(sys_satisfies_if_not,
				       f_sys_satisfies_if_not1,
				       [sys_predicate, sys_elem, c38_key, key],
				       arginfo{ all:[sys_predicate, sys_elem],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:
						      [ sys_predicate,
							sys_elem,
							key
						      ],
						opt:0,
						req:[sys_predicate, sys_elem],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_satisfies_if_not,
			  init_args(2, f_sys_satisfies_if_not1))).
*/
/*
:- side_effect(assert_lsp(sys_seq_start,
			  lambda_def(defun,
				     sys_seq_start,
				     f_sys_seq_start1,
				     
				     [ sequence,
				       c38_key,
				       [sys_start, 0],
				       sys_end,
				       sys_from_end
				     ],
				     
				     [ 
				       [ if,
					 [listp, sequence],
					 
					 [ if,
					   sys_from_end,
					   
					   [ let,
					     
					     [ [sys_acc, []],
					       
					       [ sequence,
						 [nthcdr, sys_start, sequence]
					       ]
					     ],
					     
					     [ tagbody,
					       sys_start,
					       
					       [ when,
						 
						 [ and,
						   sequence,
						   
						   [ or,
						     [not, sys_end],
						     [<, sys_start, sys_end]
						   ]
						 ],
						 [push, sequence, sys_acc],
						 
						 [ setf,
						   sequence,
						   [cdr, sequence]
						 ],
						 
						 [ setf,
						   sys_start,
						   [+, 1, sys_start]
						 ],
						 [go, sys_start]
					       ]
					     ],
					     [list, 3, sys_acc, sys_start]
					   ],
					   
					   [ list,
					     2,
					     [nthcdr, sys_start, sequence],
					     sys_start
					   ]
					 ],
					 
					 [ if,
					   sys_from_end,
					   [cons, 1, [-, sys_end, 1]],
					   [cons, 0, sys_start]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_start,
			  arglist_info(sys_seq_start,
				       f_sys_seq_start1,
				       
				       [ sequence,
					 c38_key,
					 [sys_start, 0],
					 sys_end,
					 sys_from_end
				       ],
				       arginfo{ all:[sequence],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_start,
						      sys_end,
						      sys_from_end
						    ],
						names:
						      [ sequence,
							sys_start,
							sys_end,
							sys_from_end
						      ],
						opt:0,
						req:[sequence],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_seq_start, init_args(1, f_sys_seq_start1))).
*/
/*
:- side_effect(assert_lsp(sys_seq_position,
			  lambda_def(defun,
				     sys_seq_position,
				     f_sys_seq_position1,
				     [sys_iter],
				     
				     [ 
				       [ case,
					 [car, sys_iter],
					 [[0, 1], [cdr, sys_iter]],
					 [t, [caddr, sys_iter]]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_position,
			  arglist_info(sys_seq_position,
				       f_sys_seq_position1,
				       [sys_iter],
				       arginfo{ all:[sys_iter],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_iter],
						opt:0,
						req:[sys_iter],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_seq_position, init_args(1, f_sys_seq_position1))).
*/
/*
:- side_effect(assert_lsp(sys_seq_next,
			  lambda_def(defun,
				     sys_seq_next,
				     f_sys_seq_next1,
				     [sys_iter],
				     
				     [ 
				       [ case,
					 [car, sys_iter],
					 
					 [ 0,
					   
					   [ setf,
					     [cdr, sys_iter],
					     [+, 1, [cdr, sys_iter]]
					   ]
					 ],
					 
					 [ 1,
					   
					   [ setf,
					     [cdr, sys_iter],
					     [-, [cdr, sys_iter], 1]
					   ]
					 ],
					 
					 [ 2,
					   
					   [ setf,
					     [cadr, sys_iter],
					     [cdadr, sys_iter]
					   ],
					   
					   [ setf,
					     [caddr, sys_iter],
					     [+, 1, [caddr, sys_iter]]
					   ]
					 ],
					 
					 [ t,
					   
					   [ setf,
					     [cadr, sys_iter],
					     [cdadr, sys_iter]
					   ],
					   
					   [ setf,
					     [caddr, sys_iter],
					     [-, [caddr, sys_iter], 1]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_next,
			  arglist_info(sys_seq_next,
				       f_sys_seq_next1,
				       [sys_iter],
				       arginfo{ all:[sys_iter],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_iter],
						opt:0,
						req:[sys_iter],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_seq_next, init_args(1, f_sys_seq_next1))).
*/
/*
:- side_effect(assert_lsp(sys_seq_ref,
			  lambda_def(defun,
				     sys_seq_ref,
				     f_sys_seq_ref1,
				     [sequence, sys_iter],
				     
				     [ 
				       [ case,
					 [car, sys_iter],
					 
					 [ [0, 1],
					   [aref, sequence, [cdr, sys_iter]]
					 ],
					 [2, [caadr, sys_iter]],
					 [t, [caaadr, sys_iter]]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_ref,
			  arglist_info(sys_seq_ref,
				       f_sys_seq_ref1,
				       [sequence, sys_iter],
				       arginfo{ all:[sequence, sys_iter],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sequence, sys_iter],
						opt:0,
						req:[sequence, sys_iter],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_seq_ref, init_args(2, f_sys_seq_ref1))).
*/
/*
:- side_effect(assert_lsp(sys_seq_set,
			  lambda_def(defun,
				     sys_seq_set,
				     f_sys_seq_set1,
				     [sequence, sys_iter, sys_value],
				     
				     [ 
				       [ case,
					 [car, sys_iter],
					 
					 [ [0, 1],
					   
					   [ setf,
					     [aref, sequence, [cdr, sys_iter]],
					     sys_value
					   ]
					 ],
					 [2, [setf, [caadr, sys_iter], sys_value]],
					 
					 [ t,
					   [setf, [caaadr, sys_iter], sys_value]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_set,
			  arglist_info(sys_seq_set,
				       f_sys_seq_set1,
				       [sequence, sys_iter, sys_value],
				       arginfo{ all:
						    [ sequence,
						      sys_iter,
						      sys_value
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sequence,
							sys_iter,
							sys_value
						      ],
						opt:0,
						req:
						    [ sequence,
						      sys_iter,
						      sys_value
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_seq_set, init_args(3, f_sys_seq_set1))).
*/
/*
:- side_effect(assert_lsp(sys_seq_end_p,
			  lambda_def(defun,
				     sys_seq_end_p,
				     f_sys_seq_end_p1,
				     
				     [ sequence,
				       sys_iter,
				       c38_key,
				       sys_start,
				       sys_end,
				       sys_from_end
				     ],
				     
				     [ 
				       [ case,
					 [car, sys_iter],
					 
					 [ 0,
					   
					   [ or,
					     
					     [ (=),
					       [cdr, sys_iter],
					       [length, sequence]
					     ],
					     
					     [ and,
					       sys_end,
					       [=, sys_end, [cdr, sys_iter]]
					     ]
					   ]
					 ],
					 [1, [<, [cdr, sys_iter], sys_start]],
					 
					 [ 2,
					   
					   [ or,
					     [null, [cadr, sys_iter]],
					     
					     [ and,
					       sys_end,
					       [=, sys_end, [caddr, sys_iter]]
					     ]
					   ]
					 ],
					 
					 [ t,
					   
					   [ or,
					     [null, [cadr, sys_iter]],
					     [<, [caddr, sys_iter], sys_start]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_end_p,
			  arglist_info(sys_seq_end_p,
				       f_sys_seq_end_p1,
				       
				       [ sequence,
					 sys_iter,
					 c38_key,
					 sys_start,
					 sys_end,
					 sys_from_end
				       ],
				       arginfo{ all:[sequence, sys_iter],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_start,
						      sys_end,
						      sys_from_end
						    ],
						names:
						      [ sequence,
							sys_iter,
							sys_start,
							sys_end,
							sys_from_end
						      ],
						opt:0,
						req:[sequence, sys_iter],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_seq_end_p, init_args(2, f_sys_seq_end_p1))).
*/
/*
:- side_effect(assert_lsp(sys_seq_result,
			  lambda_def(defun,
				     sys_seq_result,
				     f_sys_seq_result1,
				     [sequence, sys_iter, sys_result],
				     
				     [ 
				       [ case,
					 [car, sys_iter],
					 
					 [ 0,
					   
					   [ make_array,
					     [length, sys_result],
					     kw_element_type,
					     [array_element_type, sequence],
					     kw_initial_contents,
					     [reverse, sys_result]
					   ]
					 ],
					 
					 [ 1,
					   
					   [ make_array,
					     [length, sys_result],
					     kw_element_type,
					     [array_element_type, sequence],
					     kw_initial_contents,
					     sys_result
					   ]
					 ],
					 [2, [reverse, sys_result]],
					 [3, sys_result]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_result,
			  arglist_info(sys_seq_result,
				       f_sys_seq_result1,
				       [sequence, sys_iter, sys_result],
				       arginfo{ all:
						    [ sequence,
						      sys_iter,
						      sys_result
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sequence,
							sys_iter,
							sys_result
						      ],
						opt:0,
						req:
						    [ sequence,
						      sys_iter,
						      sys_result
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_seq_result, init_args(3, f_sys_seq_result1))).
*/
/*
:- side_effect(assert_lsp(subst,
			  lambda_def(defun,
				     subst,
				     f_subst,
				     [sys_new, sys_old, sys_tree, c38_rest, rest],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ let,
					   
					   [ 
					     [ sys_a,
					       
					       [ apply,
						 function(subst),
						 sys_new,
						 sys_old,
						 [car, sys_tree],
						 rest
					       ]
					     ],
					     
					     [ sys_d,
					       
					       [ apply,
						 function(subst),
						 sys_new,
						 sys_old,
						 [cdr, sys_tree],
						 rest
					       ]
					     ]
					   ],
					   
					   [ if,
					     
					     [ and,
					       [eq, sys_a, [car, sys_tree]],
					       [eq, sys_d, [cdr, sys_tree]]
					     ],
					     sys_tree,
					     [cons, sys_a, sys_d]
					   ]
					 ],
					 
					 [ if,
					   
					   [ apply,
					     function(satisfies),
					     sys_old,
					     sys_tree,
					     rest
					   ],
					   sys_new,
					   sys_tree
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(subst,
			  arglist_info(subst,
				       f_subst,
				       
				       [ sys_new,
					 sys_old,
					 sys_tree,
					 c38_rest,
					 rest
				       ],
				       arginfo{ all:[sys_new, sys_old, sys_tree],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_new,
							sys_old,
							sys_tree,
							rest
						      ],
						opt:0,
						req:[sys_new, sys_old, sys_tree],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(subst, init_args(3, f_subst))).
*/
/*
:- side_effect(assert_lsp(subst_if,
			  lambda_def(defun,
				     subst_if,
				     f_subst_if,
				     
				     [ sys_new,
				       sys_predicate,
				       sys_tree,
				       c38_rest,
				       rest
				     ],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ let,
					   
					   [ 
					     [ sys_a,
					       
					       [ apply,
						 function(subst),
						 sys_new,
						 sys_predicate,
						 [car, sys_tree],
						 rest
					       ]
					     ],
					     
					     [ sys_d,
					       
					       [ apply,
						 function(subst),
						 sys_new,
						 sys_predicate,
						 [cdr, sys_tree],
						 rest
					       ]
					     ]
					   ],
					   
					   [ if,
					     
					     [ and,
					       [eq, sys_a, [car, sys_tree]],
					       [eq, sys_d, [cdr, sys_tree]]
					     ],
					     sys_tree,
					     [cons, sys_a, sys_d]
					   ]
					 ],
					 
					 [ if,
					   
					   [ apply,
					     function(sys_satisfies_if),
					     sys_predicate,
					     sys_tree,
					     rest
					   ],
					   sys_new,
					   sys_tree
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(subst_if,
			  arglist_info(subst_if,
				       f_subst_if,
				       
				       [ sys_new,
					 sys_predicate,
					 sys_tree,
					 c38_rest,
					 rest
				       ],
				       arginfo{ all:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_new,
							sys_predicate,
							sys_tree,
							rest
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(subst_if, init_args(3, f_subst_if))).
*/
/*
:- side_effect(assert_lsp(subst_if_not,
			  lambda_def(defun,
				     subst_if_not,
				     f_subst_if_not,
				     
				     [ sys_new,
				       sys_predicate,
				       sys_tree,
				       c38_rest,
				       rest
				     ],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ let,
					   
					   [ 
					     [ sys_a,
					       
					       [ apply,
						 function(subst),
						 sys_new,
						 sys_predicate,
						 [car, sys_tree],
						 rest
					       ]
					     ],
					     
					     [ sys_d,
					       
					       [ apply,
						 function(subst),
						 sys_new,
						 sys_predicate,
						 [cdr, sys_tree],
						 rest
					       ]
					     ]
					   ],
					   
					   [ if,
					     
					     [ and,
					       [eq, sys_a, [car, sys_tree]],
					       [eq, sys_d, [cdr, sys_tree]]
					     ],
					     sys_tree,
					     [cons, sys_a, sys_d]
					   ]
					 ],
					 
					 [ if,
					   
					   [ apply,
					     function(sys_satisfies_if_not),
					     sys_predicate,
					     sys_tree,
					     rest
					   ],
					   sys_new,
					   sys_tree
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(subst_if_not,
			  arglist_info(subst_if_not,
				       f_subst_if_not,
				       
				       [ sys_new,
					 sys_predicate,
					 sys_tree,
					 c38_rest,
					 rest
				       ],
				       arginfo{ all:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_new,
							sys_predicate,
							sys_tree,
							rest
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(subst_if_not, init_args(3, f_subst_if_not))).
*/
/*
:- side_effect(assert_lsp(nsubst,
			  lambda_def(defun,
				     nsubst,
				     f_nsubst,
				     [sys_new, sys_old, sys_tree, c38_rest, rest],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ progn,
					   
					   [ setf,
					     [car, sys_tree],
					     
					     [ apply,
					       function(subst),
					       sys_new,
					       sys_old,
					       [car, sys_tree],
					       rest
					     ]
					   ],
					   
					   [ setf,
					     [cdr, sys_tree],
					     
					     [ apply,
					       function(subst),
					       sys_new,
					       sys_old,
					       [cdr, sys_tree],
					       rest
					     ]
					   ],
					   sys_tree
					 ],
					 
					 [ if,
					   
					   [ apply,
					     function(satisfies),
					     sys_old,
					     sys_tree,
					     rest
					   ],
					   sys_new,
					   sys_tree
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nsubst,
			  arglist_info(nsubst,
				       f_nsubst,
				       
				       [ sys_new,
					 sys_old,
					 sys_tree,
					 c38_rest,
					 rest
				       ],
				       arginfo{ all:[sys_new, sys_old, sys_tree],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_new,
							sys_old,
							sys_tree,
							rest
						      ],
						opt:0,
						req:[sys_new, sys_old, sys_tree],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nsubst, init_args(3, f_nsubst))).
*/
/*
:- side_effect(assert_lsp(nsubst_if,
			  lambda_def(defun,
				     nsubst_if,
				     f_nsubst_if,
				     
				     [ sys_new,
				       sys_predicate,
				       sys_tree,
				       c38_rest,
				       rest
				     ],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ progn,
					   
					   [ setf,
					     [car, sys_tree],
					     
					     [ apply,
					       function(subst),
					       sys_new,
					       sys_predicate,
					       [car, sys_tree],
					       rest
					     ]
					   ],
					   
					   [ setf,
					     [cdr, sys_tree],
					     
					     [ apply,
					       function(subst),
					       sys_new,
					       sys_predicate,
					       [cdr, sys_tree],
					       rest
					     ]
					   ],
					   sys_tree
					 ],
					 
					 [ if,
					   
					   [ apply,
					     function(sys_satisfies_if),
					     sys_predicate,
					     sys_tree,
					     rest
					   ],
					   sys_new,
					   sys_tree
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nsubst_if,
			  arglist_info(nsubst_if,
				       f_nsubst_if,
				       
				       [ sys_new,
					 sys_predicate,
					 sys_tree,
					 c38_rest,
					 rest
				       ],
				       arginfo{ all:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_new,
							sys_predicate,
							sys_tree,
							rest
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nsubst_if, init_args(3, f_nsubst_if))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  lambda_def(defun,
				     nsubst_if_not,
				     f_nsubst_if_not,
				     
				     [ sys_new,
				       sys_predicate,
				       sys_tree,
				       c38_rest,
				       rest
				     ],
				     
				     [ 
				       [ if,
					 [consp, sys_tree],
					 
					 [ progn,
					   
					   [ setf,
					     [car, sys_tree],
					     
					     [ apply,
					       function(subst),
					       sys_new,
					       sys_predicate,
					       [car, sys_tree],
					       rest
					     ]
					   ],
					   
					   [ setf,
					     [cdr, sys_tree],
					     
					     [ apply,
					       function(subst),
					       sys_new,
					       sys_predicate,
					       [cdr, sys_tree],
					       rest
					     ]
					   ],
					   sys_tree
					 ],
					 
					 [ if,
					   
					   [ apply,
					     function(sys_satisfies_if_not),
					     sys_predicate,
					     sys_tree,
					     rest
					   ],
					   sys_new,
					   sys_tree
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  arglist_info(nsubst_if_not,
				       f_nsubst_if_not,
				       
				       [ sys_new,
					 sys_predicate,
					 sys_tree,
					 c38_rest,
					 rest
				       ],
				       arginfo{ all:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_new,
							sys_predicate,
							sys_tree,
							rest
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_predicate,
						      sys_tree
						    ],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not, init_args(3, f_nsubst_if_not))).
*/
/*
:- side_effect(assert_lsp(assoc_if,
			  lambda_def(defun,
				     assoc_if,
				     f_assoc_if,
				     [sys_predicate, sys_alist, c38_rest, rest],
				     
				     [ 
				       [ dolist,
					 [sys_elem, sys_alist],
					 
					 [ when,
					   
					   [ apply,
					     function(sys_satisfies_if),
					     sys_predicate,
					     [car, sys_elem],
					     rest
					   ],
					   [return_from, assoc_if, sys_elem]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(assoc_if,
			  arglist_info(assoc_if,
				       f_assoc_if,
				       [sys_predicate, sys_alist, c38_rest, rest],
				       arginfo{ all:[sys_predicate, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_alist,
							rest
						      ],
						opt:0,
						req:[sys_predicate, sys_alist],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(assoc_if, init_args(2, f_assoc_if))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not,
			  lambda_def(defun,
				     assoc_if_not,
				     f_assoc_if_not,
				     [sys_predicate, sys_alist, c38_rest, rest],
				     
				     [ 
				       [ dolist,
					 [sys_elem, sys_alist],
					 
					 [ when,
					   
					   [ apply,
					     function(sys_satisfies_if_not),
					     sys_predicate,
					     [car, sys_elem],
					     rest
					   ],
					   [return_from, assoc_if_not, sys_elem]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not,
			  arglist_info(assoc_if_not,
				       f_assoc_if_not,
				       [sys_predicate, sys_alist, c38_rest, rest],
				       arginfo{ all:[sys_predicate, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_alist,
							rest
						      ],
						opt:0,
						req:[sys_predicate, sys_alist],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not, init_args(2, f_assoc_if_not))).
*/
/*
:- side_effect(assert_lsp(rassoc,
			  lambda_def(defun,
				     rassoc,
				     f_rassoc,
				     [sys_item, sys_alist, c38_rest, rest],
				     
				     [ 
				       [ dolist,
					 [sys_elem, sys_alist],
					 
					 [ when,
					   
					   [ apply,
					     function(satisfies),
					     sys_item,
					     [cdr, sys_elem],
					     rest
					   ],
					   [return_from, rassoc, sys_elem]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(rassoc,
			  arglist_info(rassoc,
				       f_rassoc,
				       [sys_item, sys_alist, c38_rest, rest],
				       arginfo{ all:[sys_item, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_item,
							sys_alist,
							rest
						      ],
						opt:0,
						req:[sys_item, sys_alist],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(rassoc, init_args(2, f_rassoc))).
*/
/*
:- side_effect(assert_lsp(rassoc_if,
			  lambda_def(defun,
				     rassoc_if,
				     f_rassoc_if,
				     [sys_predicate, sys_alist, c38_rest, rest],
				     
				     [ 
				       [ dolist,
					 [sys_elem, sys_alist],
					 
					 [ when,
					   
					   [ apply,
					     function(sys_satisfies_if),
					     sys_predicate,
					     [cdr, sys_elem],
					     rest
					   ],
					   [return_from, rassoc_if, sys_elem]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(rassoc_if,
			  arglist_info(rassoc_if,
				       f_rassoc_if,
				       [sys_predicate, sys_alist, c38_rest, rest],
				       arginfo{ all:[sys_predicate, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_alist,
							rest
						      ],
						opt:0,
						req:[sys_predicate, sys_alist],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(rassoc_if, init_args(2, f_rassoc_if))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  lambda_def(defun,
				     rassoc_if_not,
				     f_rassoc_if_not,
				     [sys_predicate, sys_alist, c38_rest, rest],
				     
				     [ 
				       [ dolist,
					 [sys_elem, sys_alist],
					 
					 [ when,
					   
					   [ apply,
					     function(sys_satisfies_if_not),
					     sys_predicate,
					     [cdr, sys_elem],
					     rest
					   ],
					   
					   [ return_from,
					     rassoc_if_not,
					     sys_elem
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  arglist_info(rassoc_if_not,
				       f_rassoc_if_not,
				       [sys_predicate, sys_alist, c38_rest, rest],
				       arginfo{ all:[sys_predicate, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_alist,
							rest
						      ],
						opt:0,
						req:[sys_predicate, sys_alist],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not, init_args(2, f_rassoc_if_not))).
*/
/*
:- side_effect(assert_lsp(adjoin,
			  lambda_def(defun,
				     adjoin,
				     f_adjoin,
				     [sys_item, list, c38_rest, rest],
				     
				     [ 
				       [ dolist,
					 [sys_elem, list, [cons, sys_item, list]],
					 
					 [ when,
					   
					   [ apply,
					     function(satisfies),
					     sys_item,
					     sys_elem,
					     rest
					   ],
					   [return_from, adjoin, list]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(adjoin,
			  arglist_info(adjoin,
				       f_adjoin,
				       [sys_item, list, c38_rest, rest],
				       arginfo{ all:[sys_item, list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_item, list, rest],
						opt:0,
						req:[sys_item, list],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(adjoin, init_args(2, f_adjoin))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  lambda_def(defun,
				     set_exclusive_or,
				     f_set_exclusive_or,
				     
				     [ sys_list_1,
				       sys_list_2,
				       c38_rest,
				       rest,
				       c38_key,
				       key
				     ],
				     
				     [ 
				       [ let,
					 [[sys_result, []]],
					 
					 [ dolist,
					   [sys_item, sys_list_1],
					   
					   [ unless,
					     
					     [ apply,
					       function(member),
					       
					       [ if,
						 key,
						 [funcall, key, sys_item],
						 sys_item
					       ],
					       sys_list_2,
					       rest
					     ],
					     [push, sys_item, sys_result]
					   ]
					 ],
					 
					 [ dolist,
					   [sys_item, sys_list_2],
					   
					   [ block,
					     sys_matches,
					     
					     [ dolist,
					       [sys_elem, sys_list_1],
					       
					       [ when,
						 
						 [ apply,
						   function(satisfies),
						   
						   [ if,
						     key,
						     [funcall, key, sys_elem],
						     sys_elem
						   ],
						   sys_item,
						   rest
						 ],
						 [return_from, sys_matches]
					       ]
					     ],
					     [push, sys_item, sys_result]
					   ]
					 ],
					 sys_result
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  arglist_info(set_exclusive_or,
				       f_set_exclusive_or,
				       
				       [ sys_list_1,
					 sys_list_2,
					 c38_rest,
					 rest,
					 c38_key,
					 key
				       ],
				       arginfo{ all:[sys_list_1, sys_list_2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:[key],
						names:
						      [ sys_list_1,
							sys_list_2,
							rest,
							key
						      ],
						opt:0,
						req:[sys_list_1, sys_list_2],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or, init_args(2, f_set_exclusive_or))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  lambda_def(defun,
				     nset_exclusive_or,
				     f_nset_exclusive_or,
				     
				     [ sys_list_1,
				       sys_list_2,
				       c38_rest,
				       rest,
				       c38_key,
				       key
				     ],
				     
				     [ 
				       [ let,
					 
					 [ [sys_result, []],
					   [list, []],
					   [sys_item, []]
					 ],
					 
					 [ tagbody,
					   sys_start_1,
					   
					   [ unless,
					     sys_list_1,
					     [go, sys_start_2]
					   ],
					   [setf, sys_item, [car, sys_list_1]],
					   [setf, list, sys_list_2],
					   [setf, sys_prev, []],
					   sys_start_1_in,
					   [unless, list, [go, sys_end_1_in]],
					   
					   [ let,
					     
					     [ 
					       [ sys_elem,
						 
						 [ if,
						   key,
						   [funcall, key, [car, list]],
						   [car, list]
						 ]
					       ]
					     ],
					     
					     [ when,
					       
					       [ apply,
						 function(satisfies),
						 sys_item,
						 
						 [ if,
						   key,
						   [funcall, key, sys_elem],
						   sys_elem
						 ],
						 rest
					       ],
					       
					       [ if,
						 sys_prev,
						 
						 [ setf,
						   [cdr, sys_prev],
						   [cdr, list]
						 ],
						 [setf, sys_list_2, [cdr, list]]
					       ],
					       
					       [ setf,
						 sys_list_1,
						 [cdr, sys_list_1]
					       ],
					       [go, sys_start_1]
					     ]
					   ],
					   [setf, sys_prev, list],
					   [setf, list, [cdr, list]],
					   [go, sys_start_1_in],
					   sys_end_1_in,
					   [setf, sys_item, [cdr, sys_list_1]],
					   [setf, [cdr, sys_list_1], sys_result],
					   
					   [ unless,
					     sys_result,
					     [setf, sys_end, sys_list_1]
					   ],
					   [setf, sys_result, sys_list_1],
					   [setf, sys_list_1, sys_item],
					   [go, sys_start_1],
					   sys_start_2,
					   
					   [ return_from,
					     nset_exclusive_or,
					     
					     [ if,
					       sys_end,
					       
					       [ progn,
						 
						 [ setf,
						   [cdr, sys_end],
						   sys_list_2
						 ],
						 sys_result
					       ],
					       sys_list_2
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  arglist_info(nset_exclusive_or,
				       f_nset_exclusive_or,
				       
				       [ sys_list_1,
					 sys_list_2,
					 c38_rest,
					 rest,
					 c38_key,
					 key
				       ],
				       arginfo{ all:[sys_list_1, sys_list_2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:[key],
						names:
						      [ sys_list_1,
							sys_list_2,
							rest,
							key
						      ],
						opt:0,
						req:[sys_list_1, sys_list_2],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or, init_args(2, f_nset_exclusive_or))).
*/
/*
:- side_effect(assert_lsp(fill,
			  lambda_def(defun,
				     fill,
				     f_fill,
				     [sequence, sys_item, c38_rest, rest],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iter,
					     
					     [ apply,
					       function(sys_seq_start),
					       sequence,
					       rest
					     ]
					   ]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ apply,
					       function(sys_seq_end_p),
					       sequence,
					       sys_iter,
					       rest
					     ],
					     
					     [ sys_seq_set,
					       sequence,
					       sys_iter,
					       sys_item
					     ],
					     [sys_seq_next, sys_iter],
					     [go, sys_start]
					   ]
					 ]
				       ],
				       sequence
				     ]))).
*/
/*
:- side_effect(assert_lsp(fill,
			  arglist_info(fill,
				       f_fill,
				       [sequence, sys_item, c38_rest, rest],
				       arginfo{ all:[sequence, sys_item],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sequence, sys_item, rest],
						opt:0,
						req:[sequence, sys_item],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(fill, init_args(2, f_fill))).
*/
/*
:- side_effect(assert_lsp(every,
			  lambda_def(defun,
				     every,
				     f_every,
				     [sys_predicate, c38_rest, sys_sequences],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iters,
					     
					     [ mapcar,
					       function(sys_seq_start),
					       sys_sequences
					     ]
					   ]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ sys_some_list_2,
					       function(sys_seq_end_p),
					       sys_sequences,
					       sys_iters
					     ],
					     
					     [ unless,
					       
					       [ apply,
						 sys_predicate,
						 
						 [ mapcar,
						   function(sys_seq_ref),
						   sys_sequences,
						   sys_iters
						 ]
					       ],
					       [return_from, every, []]
					     ],
					     
					     [ mapc,
					       function(sys_seq_next),
					       sys_iters
					     ],
					     [go, sys_start]
					   ]
					 ]
				       ],
				       t
				     ]))).
*/
/*
:- side_effect(assert_lsp(every,
			  arglist_info(every,
				       f_every,
				       [sys_predicate, c38_rest, sys_sequences],
				       arginfo{ all:[sys_predicate],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_sequences
						      ],
						opt:0,
						req:[sys_predicate],
						rest:[sys_sequences],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(every, init_args(x, f_every))).
*/
/*
:- side_effect(assert_lsp(some,
			  lambda_def(defun,
				     some,
				     f_some,
				     [sys_predicate, c38_rest, sys_sequences],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iters,
					     
					     [ mapcar,
					       function(sys_seq_start),
					       sys_sequences
					     ]
					   ]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ sys_some_list_2,
					       function(sys_seq_end_p),
					       sys_sequences,
					       sys_iters
					     ],
					     
					     [ let,
					       
					       [ 
						 [ sys_result,
						   
						   [ apply,
						     sys_predicate,
						     
						     [ mapcar,
						       function(sys_seq_ref),
						       sys_sequences,
						       sys_iters
						     ]
						   ]
						 ]
					       ],
					       
					       [ when,
						 sys_result,
						 [return_from, some, sys_result]
					       ]
					     ],
					     
					     [ mapc,
					       function(sys_seq_next),
					       sys_iters
					     ],
					     [go, sys_start]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(some,
			  arglist_info(some,
				       f_some,
				       [sys_predicate, c38_rest, sys_sequences],
				       arginfo{ all:[sys_predicate],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_sequences
						      ],
						opt:0,
						req:[sys_predicate],
						rest:[sys_sequences],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(some, init_args(1, f_some))).
*/
/*
:- side_effect(assert_lsp(notevery,
			  lambda_def(defun,
				     notevery,
				     f_notevery,
				     [sys_predicate, c38_rest, sys_sequences],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iters,
					     
					     [ mapcar,
					       function(sys_seq_start),
					       sys_sequences
					     ]
					   ]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ sys_some_list_2,
					       function(sys_seq_end_p),
					       sys_sequences,
					       sys_iters
					     ],
					     
					     [ unless,
					       
					       [ apply,
						 sys_predicate,
						 
						 [ mapcar,
						   function(sys_seq_ref),
						   sys_sequences,
						   sys_iters
						 ]
					       ],
					       [return_from, every, t]
					     ],
					     
					     [ mapc,
					       function(sys_seq_next),
					       sys_iters
					     ],
					     [go, sys_start]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(notevery,
			  arglist_info(notevery,
				       f_notevery,
				       [sys_predicate, c38_rest, sys_sequences],
				       arginfo{ all:[sys_predicate],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_sequences
						      ],
						opt:0,
						req:[sys_predicate],
						rest:[sys_sequences],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(notevery, init_args(1, f_notevery))).
*/
/*
:- side_effect(assert_lsp(notany,
			  lambda_def(defun,
				     notany,
				     f_notany,
				     [sys_predicate, c38_rest, sys_sequences],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iters,
					     
					     [ mapcar,
					       function(sys_seq_start),
					       sys_sequences
					     ]
					   ]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ sys_some_list_2,
					       function(sys_seq_end_p),
					       sys_sequences,
					       sys_iters
					     ],
					     
					     [ when,
					       
					       [ apply,
						 sys_predicate,
						 
						 [ mapcar,
						   function(sys_seq_ref),
						   sys_sequences,
						   sys_iters
						 ]
					       ],
					       [return_from, every, []]
					     ],
					     
					     [ mapc,
					       function(sys_seq_next),
					       sys_iters
					     ],
					     [go, sys_start]
					   ]
					 ]
				       ],
				       t
				     ]))).
*/
/*
:- side_effect(assert_lsp(notany,
			  arglist_info(notany,
				       f_notany,
				       [sys_predicate, c38_rest, sys_sequences],
				       arginfo{ all:[sys_predicate],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sys_sequences
						      ],
						opt:0,
						req:[sys_predicate],
						rest:[sys_sequences],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(notany, init_args(1, f_notany))).
*/
/*
:- side_effect(assert_lsp(count,
			  lambda_def(defun,
				     count,
				     f_count,
				     [sys_item, sequence, c38_rest, rest],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iter,
					     
					     [ apply,
					       function(sys_seq_start),
					       sequence,
					       rest
					     ]
					   ],
					   [count, 0]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ apply,
					       function(sys_seq_end_p),
					       sequence,
					       sys_iter,
					       rest
					     ],
					     
					     [ when,
					       
					       [ apply,
						 function(satisfies),
						 sys_item,
						 
						 [ sys_seq_ref,
						   sequence,
						   sys_iter
						 ],
						 rest
					       ],
					       [setf, count, [+, 1, count]]
					     ],
					     [sys_seq_next, sys_iter],
					     [go, sys_start]
					   ]
					 ],
					 count
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(count,
			  arglist_info(count,
				       f_count,
				       [sys_item, sequence, c38_rest, rest],
				       arginfo{ all:[sys_item, sequence],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_item, sequence, rest],
						opt:0,
						req:[sys_item, sequence],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(count, init_args(2, f_count))).
*/
/*
:- side_effect(assert_lsp(count_if,
			  lambda_def(defun,
				     count_if,
				     f_count_if,
				     [sys_predicate, sequence, c38_rest, rest],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iter,
					     
					     [ apply,
					       function(sys_seq_start),
					       sequence,
					       rest
					     ]
					   ],
					   [count, 0]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ apply,
					       function(sys_seq_end_p),
					       sequence,
					       sys_iter,
					       rest
					     ],
					     
					     [ when,
					       
					       [ apply,
						 function(sys_satisfies_if),
						 sys_predicate,
						 
						 [ sys_seq_ref,
						   sequence,
						   sys_iter
						 ],
						 rest
					       ],
					       [setf, count, [+, 1, count]]
					     ],
					     [sys_seq_next, sys_iter],
					     [go, sys_start]
					   ]
					 ],
					 count
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(count_if,
			  arglist_info(count_if,
				       f_count_if,
				       [sys_predicate, sequence, c38_rest, rest],
				       arginfo{ all:[sys_predicate, sequence],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sequence,
							rest
						      ],
						opt:0,
						req:[sys_predicate, sequence],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(count_if, init_args(2, f_count_if))).
*/
/*
:- side_effect(assert_lsp(count_if_not,
			  lambda_def(defun,
				     count_if_not,
				     f_count_if_not,
				     [sys_predicate, sequence, c38_rest, rest],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iter,
					     
					     [ apply,
					       function(sys_seq_start),
					       sequence,
					       rest
					     ]
					   ],
					   [count, 0]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ apply,
					       function(sys_seq_end_p),
					       sequence,
					       sys_iter,
					       rest
					     ],
					     
					     [ when,
					       
					       [ apply,
						 function(sys_satisfies_if_not),
						 sys_predicate,
						 
						 [ sys_seq_ref,
						   sequence,
						   sys_iter
						 ],
						 rest
					       ],
					       [setf, count, [+, 1, count]]
					     ],
					     [sys_seq_next, sys_iter],
					     [go, sys_start]
					   ]
					 ],
					 count
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(count_if_not,
			  arglist_info(count_if_not,
				       f_count_if_not,
				       [sys_predicate, sequence, c38_rest, rest],
				       arginfo{ all:[sys_predicate, sequence],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_predicate,
							sequence,
							rest
						      ],
						opt:0,
						req:[sys_predicate, sequence],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(count_if_not, init_args(2, f_count_if_not))).
*/
/*
:- side_effect(assert_lsp(remove,
			  lambda_def(defun,
				     remove,
				     f_remove,
				     
				     [ sys_item,
				       sequence,
				       c38_rest,
				       rest,
				       c38_key,
				       count
				     ],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iter,
					     
					     [ apply,
					       function(sys_seq_start),
					       sequence,
					       rest
					     ]
					   ],
					   [sys_result, []]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ apply,
					       function(sys_seq_end_p),
					       sequence,
					       sys_iter,
					       rest
					     ],
					     
					     [ let,
					       
					       [ 
						 [ sys_elem,
						   
						   [ sys_seq_ref,
						     sequence,
						     sys_iter
						   ]
						 ]
					       ],
					       
					       [ unless,
						 
						 [ and,
						   
						   [ apply,
						     function(satisfies),
						     sys_item,
						     sys_elem,
						     rest
						   ],
						   
						   [ or,
						     [not, count],
						     
						     [ not,
						       [minusp, [decf, count]]
						     ]
						   ]
						 ],
						 [push, sys_elem, sys_result]
					       ]
					     ],
					     [sys_seq_next, sys_iter],
					     [go, sys_start]
					   ]
					 ],
					 
					 [ sys_seq_result,
					   sequence,
					   sys_iter,
					   sys_result
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(remove,
			  arglist_info(remove,
				       f_remove,
				       
				       [ sys_item,
					 sequence,
					 c38_rest,
					 rest,
					 c38_key,
					 count
				       ],
				       arginfo{ all:[sys_item, sequence],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:[count],
						names:
						      [ sys_item,
							sequence,
							rest,
							count
						      ],
						opt:0,
						req:[sys_item, sequence],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(remove, init_args(2, f_remove))).
*/
/*
:- side_effect(assert_lsp(remove_if,
			  lambda_def(defun,
				     remove_if,
				     f_remove_if,
				     
				     [ sys_predicate,
				       sequence,
				       c38_rest,
				       rest,
				       c38_key,
				       count
				     ],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iter,
					     
					     [ apply,
					       function(sys_seq_start),
					       sequence,
					       rest
					     ]
					   ],
					   [sys_result, []]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ apply,
					       function(sys_seq_end_p),
					       sequence,
					       sys_iter,
					       rest
					     ],
					     
					     [ let,
					       
					       [ 
						 [ sys_elem,
						   
						   [ sys_seq_ref,
						     sequence,
						     sys_iter
						   ]
						 ]
					       ],
					       
					       [ unless,
						 
						 [ and,
						   
						   [ apply,
						     function(sys_satisfies_if),
						     sys_predicate,
						     sys_elem,
						     rest
						   ],
						   
						   [ or,
						     [not, count],
						     
						     [ not,
						       [minusp, [decf, count]]
						     ]
						   ]
						 ],
						 [push, sys_elem, sys_result]
					       ]
					     ],
					     [sys_seq_next, sys_iter],
					     [go, sys_start]
					   ]
					 ],
					 
					 [ sys_seq_result,
					   sequence,
					   sys_iter,
					   sys_result
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(remove_if,
			  arglist_info(remove_if,
				       f_remove_if,
				       
				       [ sys_predicate,
					 sequence,
					 c38_rest,
					 rest,
					 c38_key,
					 count
				       ],
				       arginfo{ all:[sys_predicate, sequence],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:[count],
						names:
						      [ sys_predicate,
							sequence,
							rest,
							count
						      ],
						opt:0,
						req:[sys_predicate, sequence],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(remove_if, init_args(2, f_remove_if))).
*/
/*
:- side_effect(assert_lsp(remove_if_not,
			  lambda_def(defun,
				     remove_if_not,
				     f_remove_if_not,
				     
				     [ sys_predicate,
				       sequence,
				       c38_rest,
				       rest,
				       c38_key,
				       count
				     ],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_iter,
					     
					     [ apply,
					       function(sys_seq_start),
					       sequence,
					       rest
					     ]
					   ],
					   [sys_result, []]
					 ],
					 
					 [ tagbody,
					   sys_start,
					   
					   [ unless,
					     
					     [ apply,
					       function(sys_seq_end_p),
					       sequence,
					       sys_iter,
					       rest
					     ],
					     
					     [ let,
					       
					       [ 
						 [ sys_elem,
						   
						   [ sys_seq_ref,
						     sequence,
						     sys_iter
						   ]
						 ]
					       ],
					       
					       [ unless,
						 
						 [ and,
						   
						   [ apply,
						     function(sys_satisfies_if_not),
						     sys_predicate,
						     sys_elem,
						     rest
						   ],
						   
						   [ or,
						     [not, count],
						     
						     [ not,
						       [minusp, [decf, count]]
						     ]
						   ]
						 ],
						 [push, sys_elem, sys_result]
					       ]
					     ],
					     [sys_seq_next, sys_iter],
					     [go, sys_start]
					   ]
					 ],
					 
					 [ sys_seq_result,
					   sequence,
					   sys_iter,
					   sys_result
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(remove_if_not,
			  arglist_info(remove_if_not,
				       f_remove_if_not,
				       
				       [ sys_predicate,
					 sequence,
					 c38_rest,
					 rest,
					 c38_key,
					 count
				       ],
				       arginfo{ all:[sys_predicate, sequence],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:[count],
						names:
						      [ sys_predicate,
							sequence,
							rest,
							count
						      ],
						opt:0,
						req:[sys_predicate, sequence],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(remove_if_not, init_args(2, f_remove_if_not))).
*/
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun null (object) (if object nil t))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:34926 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,null,[object],[if,object,[],t]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun not (object) (if object nil t))




*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:35004 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,not,[object],[if,object,[],t]]]]))
/*
#+(or WAM-CL LISP500) 
(defun mod (x y) (multiple-value-call #'(lambda (q r) r) (floor x y)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:35087 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,mod,[x,y],['multiple-value-call',function([lambda,[q,r],r]),[floor,x,y]]])
wl:lambda_def(defun, mod, f_mod, [sys_x, sys_y], [[multiple_value_call, function([lambda, [sys_q, sys_r], sys_r]), [floor, sys_x, sys_y]]]).
wl:arglist_info(mod, f_mod, [sys_x, sys_y], arginfo{all:[sys_x, sys_y], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_y], opt:0, req:[sys_x, sys_y], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_mod).

/*

### Compiled Function: `CL:MOD` 
*/
f_mod(X_In, Y_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_y, Y_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  get_var(GEnv, sys_y, Y_Get),
		  f_floor(X_Get, [Y_Get], Floor_Ret),
		  nb_current('$mv_return', Nb_current_Ret),
		  f_apply(closure(kw_function,
				  [ClosureEnvironment|GEnv],
				  Whole,
				  R_Get,
				  [sys_q, sys_r],
				  get_var(ClosureEnvironment, sys_r, R_Get),
				  [lambda, [sys_q, sys_r], sys_r]),
			  Nb_current_Ret,
			  Apply_Ret)
		),
		Apply_Ret=FnResult
	      ),
	      block_exit(mod, FnResult),
	      true).
:- set_opv(mod, symbol_function, f_mod),
   DefunResult=(mod).
/*
:- side_effect(assert_lsp((mod),
			  lambda_def(defun,
				     (mod),
				     f_mod,
				     [sys_x, sys_y],
				     
				     [ 
				       [ multiple_value_call,
					 function([lambda, [sys_q, sys_r], sys_r]),
					 [floor, sys_x, sys_y]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp((mod),
			  arglist_info((mod),
				       f_mod,
				       [sys_x, sys_y],
				       arginfo{ all:[sys_x, sys_y],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_y],
						opt:0,
						req:[sys_x, sys_y],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(mod, init_args(x, f_mod))).
*/
/*
#+(or WAM-CL LISP500) 
#+BUILTIN
(defun functionp (object) (eq (type-of object) 'function))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:35185 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[defun,functionp,[object],[eq,['type-of',object],[quote,function]]]]))
/*
#+(or WAM-CL LISP500) 
(defun coerce (object result-type)
  (if (typep object result-type)
      object
      (case result-type
	((t) object)
	(character (character object))
	(function (if (and (consp object) (eq (car object) 'lambda))
		      (eval (list 'function object))
		      (if (fboundp object)
			  (fdefinition object))
			  (error 'type-error :datum object
				 :expected-type result-type)))
	(t (error 'type-error :datum object :expected-type result-type)))))




*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:35286 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,coerce,[object,'result-type'],[if,[typep,object,'result-type'],object,[case,'result-type',[[t],object],[character,[character,object]],[function,[if,[and,[consp,object],[eq,[car,object],[quote,lambda]]],[eval,[list,[quote,function],object]],[if,[fboundp,object],[fdefinition,object]],[error,[quote,'type-error'],':datum',object,':expected-type','result-type']]],[t,[error,[quote,'type-error'],':datum',object,':expected-type','result-type']]]]])
/*
% case:-[[[t],sys_object],[character,[character,sys_object]],[function,[if,[and,[consp,sys_object],[eq,[car,sys_object],[quote,lambda]]],[eval,[list,[quote,function],sys_object]],[if,[fboundp,sys_object],[fdefinition,sys_object]],[error,[quote,type_error],kw_datum,sys_object,kw_expected_type,sys_result_type]]],[t,[error,[quote,type_error],kw_datum,sys_object,kw_expected_type,sys_result_type]]].
*/
/*
% conds:-[[[eq,_33082,[quote,t]],[progn,sys_object]],[[eq,_33082,[quote,character]],[progn,[character,sys_object]]],[[eq,_33082,[quote,function]],[progn,[if,[and,[consp,sys_object],[eq,[car,sys_object],[quote,lambda]]],[eval,[list,[quote,function],sys_object]],[if,[fboundp,sys_object],[fdefinition,sys_object]],[error,[quote,type_error],kw_datum,sys_object,kw_expected_type,sys_result_type]]]],[t,[progn,[error,[quote,type_error],kw_datum,sys_object,kw_expected_type,sys_result_type]]]].
*/
wl:lambda_def(defun, coerce, f_coerce, [sys_object, sys_result_type], [[if, [typep, sys_object, sys_result_type], sys_object, [case, sys_result_type, [[t], sys_object], [character, [character, sys_object]], [function, [if, [and, [consp, sys_object], [eq, [car, sys_object], [quote, lambda]]], [eval, [list, [quote, function], sys_object]], [if, [fboundp, sys_object], [fdefinition, sys_object]], [error, [quote, type_error], kw_datum, sys_object, kw_expected_type, sys_result_type]]], [t, [error, [quote, type_error], kw_datum, sys_object, kw_expected_type, sys_result_type]]]]]).
wl:arglist_info(coerce, f_coerce, [sys_object, sys_result_type], arginfo{all:[sys_object, sys_result_type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object, sys_result_type], opt:0, req:[sys_object, sys_result_type], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_coerce).

/*

### Compiled Function: `CL:COERCE` 
*/
f_coerce(Object_In, Result_type_In, FnResult) :-
	GEnv=[bv(sys_object, Object_In), bv(sys_result_type, Result_type_In)],
	catch(( ( get_var(GEnv, sys_object, Object_Get),
		  get_var(GEnv, sys_result_type, Result_type_Get),
		  f_typep(Object_Get, Result_type_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_object, Object_Get10),
		      _11140=Object_Get10
		  ;   get_var(GEnv, sys_result_type, Key),
		      (   is_eq(Key, t)
		      ->  get_var(GEnv, sys_object, Object_Get16),
			  ElseResult31=Object_Get16
		      ;   (   is_eq(Key, character)
			  ->  get_var(GEnv, sys_object, Object_Get19),
			      f_character(Object_Get19, TrueResult26),
			      ElseResult29=TrueResult26
			  ;   (   is_eq(Key, function)
			      ->  sf_if(
					[ and,
					  [consp, sys_object],
					  [eq, [car, sys_object], [quote, lambda]]
					],
					
					[ eval,
					  [list, [quote, function], sys_object]
					],
					
					[ if,
					  [fboundp, sys_object],
					  [fdefinition, sys_object]
					],
					
					[ error,
					  [quote, type_error],
					  kw_datum,
					  sys_object,
					  kw_expected_type,
					  sys_result_type
					],
					TrueResult),
				  ElseResult27=TrueResult
			      ;   get_var(GEnv, sys_object, Object_Get22),
				  get_var(GEnv,
					  sys_result_type,
					  Result_type_Get23),
				  f_error(
					  [ type_error,
					    kw_datum,
					    Object_Get22,
					    kw_expected_type,
					    Result_type_Get23
					  ],
					  ElseResult),
				  ElseResult27=ElseResult
			      ),
			      ElseResult29=ElseResult27
			  ),
			  ElseResult31=ElseResult29
		      ),
		      _11140=ElseResult31
		  )
		),
		_11140=FnResult
	      ),
	      block_exit(coerce, FnResult),
	      true).
:- set_opv(coerce, symbol_function, f_coerce),
   DefunResult=coerce.
/*
:- side_effect(assert_lsp(coerce,
			  lambda_def(defun,
				     coerce,
				     f_coerce,
				     [sys_object, sys_result_type],
				     
				     [ 
				       [ if,
					 [typep, sys_object, sys_result_type],
					 sys_object,
					 
					 [ case,
					   sys_result_type,
					   [[t], sys_object],
					   [character, [character, sys_object]],
					   
					   [ function,
					     
					     [ if,
					       
					       [ and,
						 [consp, sys_object],
						 
						 [ eq,
						   [car, sys_object],
						   [quote, lambda]
						 ]
					       ],
					       
					       [ eval,
						 
						 [ list,
						   [quote, function],
						   sys_object
						 ]
					       ],
					       
					       [ if,
						 [fboundp, sys_object],
						 [fdefinition, sys_object]
					       ],
					       
					       [ error,
						 [quote, type_error],
						 kw_datum,
						 sys_object,
						 kw_expected_type,
						 sys_result_type
					       ]
					     ]
					   ],
					   
					   [ t,
					     
					     [ error,
					       [quote, type_error],
					       kw_datum,
					       sys_object,
					       kw_expected_type,
					       sys_result_type
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(coerce,
			  arglist_info(coerce,
				       f_coerce,
				       [sys_object, sys_result_type],
				       arginfo{ all:
						    [ sys_object,
						      sys_result_type
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_object,
							sys_result_type
						      ],
						opt:0,
						req:
						    [ sys_object,
						      sys_result_type
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(coerce, init_args(x, f_coerce))).
*/
/*
#+(or WAM-CL LISP500) 
(defmacro deftype (name lambda-list &rest forms)
  `(ensure-type ',name #'(lambda ,lambda-list (block ,name ,@forms))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:35781 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,deftype,[name,'lambda-list','&rest',forms],['#BQ',['ensure-type',[quote,['#COMMA',name]],function([lambda,['#COMMA','lambda-list'],[block,['#COMMA',name],['#BQ-COMMA-ELIPSE',forms]]])]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       deftype,
					       kw_macro,
					       mf_deftype)).
*/
wl:lambda_def(defmacro, deftype, mf_deftype, [sys_name, sys_lambda_list, c38_rest, sys_forms], [['#BQ', [sys_ensure_type, [quote, ['#COMMA', sys_name]], function([lambda, ['#COMMA', sys_lambda_list], [block, ['#COMMA', sys_name], ['#BQ-COMMA-ELIPSE', sys_forms]]])]]]).
wl:arglist_info(deftype, mf_deftype, [sys_name, sys_lambda_list, c38_rest, sys_forms], arginfo{all:[sys_name, sys_lambda_list], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_lambda_list, sys_forms], opt:0, req:[sys_name, sys_lambda_list], rest:[sys_forms], sublists:0, whole:0}).
wl: init_args(2, mf_deftype).

/*

### Compiled Macro Operator: `CL:DEFTYPE` 
*/
sf_deftype(MacroEnv, Name_In, Lambda_list_In, RestNKeys, FResult) :-
	mf_deftype([deftype, Name_In, Lambda_list_In|RestNKeys],
		   MacroEnv,
		   MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFTYPE` 
*/
mf_deftype([deftype, Name_In, Lambda_list_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_name, Name_In), bv(sys_lambda_list, Lambda_list_In), bv(sys_forms, RestNKeys)],
	catch(( get_var(GEnv, sys_name, Name_Get),
		[sys_ensure_type, [quote, Name_Get], function([lambda, ['#COMMA', sys_lambda_list], [block, ['#COMMA', sys_name], ['#BQ-COMMA-ELIPSE', sys_forms]]])]=MFResult
	      ),
	      block_exit(deftype, MFResult),
	      true).
:- set_opv(mf_deftype, type_of, sys_macro),
   set_opv(deftype, symbol_function, mf_deftype),
   DefMacroResult=deftype.
/*
:- side_effect(assert_lsp(deftype,
			  lambda_def(defmacro,
				     deftype,
				     mf_deftype,
				     
				     [ sys_name,
				       sys_lambda_list,
				       c38_rest,
				       sys_forms
				     ],
				     
				     [ 
				       [ '#BQ',
					 
					 [ sys_ensure_type,
					   [quote, ['#COMMA', sys_name]],
					   function(
						    [ lambda,
						      
						      [ '#COMMA',
							sys_lambda_list
						      ],
						      
						      [ block,
							['#COMMA', sys_name],
							
							[ '#BQ-COMMA-ELIPSE',
							  sys_forms
							]
						      ]
						    ])
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(deftype,
			  arglist_info(deftype,
				       mf_deftype,
				       
				       [ sys_name,
					 sys_lambda_list,
					 c38_rest,
					 sys_forms
				       ],
				       arginfo{ all:[sys_name, sys_lambda_list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_lambda_list,
							sys_forms
						      ],
						opt:0,
						req:[sys_name, sys_lambda_list],
						rest:[sys_forms],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(deftype, init_args(2, mf_deftype))).
*/
/*
#+(or WAM-CL LISP500) 
(defun *= (cons number)
  (or (not cons) (eq (car cons) '*) (= (car cons) number)))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:35931 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,*=,[cons,number],[or,[not,cons],[eq,[car,cons],[quote,*]],[=,[car,cons],number]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       *=,
					       kw_function,
					       f_sys_xx_c61)).
*/
wl:lambda_def(defun, *=, f_sys_xx_c61, [cons, number], [[or, [not, cons], [eq, [car, cons], [quote, *]], [=, [car, cons], number]]]).
wl:arglist_info(*=, f_sys_xx_c61, [cons, number], arginfo{all:[cons, number], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[cons, number], opt:0, req:[cons, number], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_sys_xx_c61).

/*

### Compiled Function: `*=` 
*/
f_sys_xx_c61(Cons_In, Number_In, RestNKeys, FnResult) :-
	GEnv=[bv(cons, Cons_In), bv(number, Number_In)],
	catch(( (   get_var(GEnv, cons, Cons_Get),
		    f_not(Cons_Get, FORM1_Res11),
		    FORM1_Res11\==[],
		    _7130=FORM1_Res11
		->  true
		;   (   get_var(GEnv, cons, Cons_Get7),
			f_car(Cons_Get7, Eq_Param),
			f_eq(Eq_Param, *, FORM1_Res),
			FORM1_Res\==[],
			_7150=FORM1_Res
		    ->  true
		    ;   get_var(GEnv, cons, Cons_Get8),
			f_car(Cons_Get8, Car_Ret),
			get_var(GEnv, number, Number_Get),
			'f_='(Car_Ret, Number_Get, _7184),
			_7150=_7184
		    ),
		    _7130=_7150
		),
		_7130=FnResult
	      ),
	      block_exit(*=, FnResult),
	      true).
:- set_opv(*=, symbol_function, f_sys_xx_c61),
   DefunResult= *= .
/*
:- side_effect(assert_lsp(*=,
			  lambda_def(defun,
				     *=,
				     f_sys_xx_c61,
				     [cons, number],
				     
				     [ 
				       [ or,
					 [not, cons],
					 [eq, [car, cons], [quote, *]],
					 [=, [car, cons], number]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(*=,
			  arglist_info(*=,
				       f_sys_xx_c61,
				       [cons, number],
				       arginfo{ all:[cons, number],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[cons, number],
						opt:0,
						req:[cons, number],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(*=, init_args(2, f_sys_xx_c61))).
*/
/*
#+(or WAM-CL LISP500) 
(defun ensure-type (name expander)
  (let ((cons (assoc name *type-expanders*)))
    (if cons
	(setf (cdr cons) expander)
	(push (cons name expander) *type-expanders*))
    name))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:36047 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'ensure-type',[name,expander],[let,[[cons,[assoc,name,'*type-expanders*']]],[if,cons,[setf,[cdr,cons],expander],[push,[cons,name,expander],'*type-expanders*']],name]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_ensure_type,
					       kw_function,
					       f_sys_ensure_type)).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv, [], [], true), append([cons], [CAR16, CAR], [cons, CAR16, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
:- side_effect((compile_each([fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1), fbound(sys_all_car, kw_function)=function(f_sys_all_car1), fbound(sys_all_end, kw_function)=function(f_sys_all_end1), fbound(sys_expand, kw_function)=function(f_sys_expand11), name='GLOBAL', environ=env_1], LEnv, [], [], true), append([cons], [CAR16, CAR], [cons, CAR16, CAR]), setf_inverse_op(cdr, rplacd))).
*/
/*
% macroexpand:-[push,[cons,sys_name,sys_expander],sys_xx_type_expanders_xx].
*/
/*
% into:-[setq,sys_xx_type_expanders_xx,[cons,[cons,sys_name,sys_expander],sys_xx_type_expanders_xx]].
*/
wl:lambda_def(defun, sys_ensure_type, f_sys_ensure_type, [sys_name, sys_expander], [[let, [[cons, [assoc, sys_name, sys_xx_type_expanders_xx]]], [if, cons, [setf, [cdr, cons], sys_expander], [push, [cons, sys_name, sys_expander], sys_xx_type_expanders_xx]], sys_name]]).
wl:arglist_info(sys_ensure_type, f_sys_ensure_type, [sys_name, sys_expander], arginfo{all:[sys_name, sys_expander], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander], opt:0, req:[sys_name, sys_expander], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_ensure_type).

/*

### Compiled Function: `SYS::ENSURE-TYPE` 
*/
f_sys_ensure_type(Name_In, Expander_In, FnResult) :-
	GEnv=[bv(sys_name, Name_In), bv(sys_expander, Expander_In)],
	catch(( ( get_var(GEnv, sys_name, Name_Get),
		  get_var(GEnv,
			  sys_xx_type_expanders_xx,
			  Xx_type_expanders_xx_Get),
		  f_assoc(Name_Get, Xx_type_expanders_xx_Get, [], Cons_Init),
		  LEnv=[bv(cons, Cons_Init)|GEnv],
		  get_var(LEnv, cons, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, cons, Cons_Get17),
		      get_var(LEnv, sys_expander, Expander_Get),
		      f_rplacd(Cons_Get17, Expander_Get, TrueResult),
		      _10420=TrueResult
		  ;   get_var(LEnv, sys_expander, Expander_Get21),
		      get_var(LEnv, sys_name, Name_Get20),
		      CAR29=[Name_Get20|Expander_Get21],
		      get_var(LEnv,
			      sys_xx_type_expanders_xx,
			      Xx_type_expanders_xx_Get22),
		      ElseResult=[CAR29|Xx_type_expanders_xx_Get22],
		      set_var(LEnv, sys_xx_type_expanders_xx, ElseResult),
		      _10420=ElseResult
		  ),
		  get_var(LEnv, sys_name, Name_Get25)
		),
		Name_Get25=FnResult
	      ),
	      block_exit(sys_ensure_type, FnResult),
	      true).
:- set_opv(sys_ensure_type, symbol_function, f_sys_ensure_type),
   DefunResult=sys_ensure_type.
/*
:- side_effect(assert_lsp(sys_ensure_type,
			  lambda_def(defun,
				     sys_ensure_type,
				     f_sys_ensure_type,
				     [sys_name, sys_expander],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ cons,
					     
					     [ assoc,
					       sys_name,
					       sys_xx_type_expanders_xx
					     ]
					   ]
					 ],
					 
					 [ if,
					   cons,
					   [setf, [cdr, cons], sys_expander],
					   
					   [ push,
					     [cons, sys_name, sys_expander],
					     sys_xx_type_expanders_xx
					   ]
					 ],
					 sys_name
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_ensure_type,
			  arglist_info(sys_ensure_type,
				       f_sys_ensure_type,
				       [sys_name, sys_expander],
				       arginfo{ all:[sys_name, sys_expander],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_name, sys_expander],
						opt:0,
						req:[sys_name, sys_expander],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_ensure_type, init_args(x, f_sys_ensure_type))).
*/
/*
#+WAM-CL
#+(or WAM-CL LISP500) 
(defun ensure-package (name nicknames shadow shadowing-import-from use
		       import-from intern export)
  (let ((package (find-package name)))
    (unless package
      (setq package (make-package name :nicknames nicknames)))
    (shadow shadow package)
    (mapc #'(lambda (list)
	      (let ((imported-package (find-package (car list)))
		    (symbol-names (cdr list)))
		(shadowing-import (mapcar #'(lambda (symbol-name)
					      (find-symbol symbol-name
							   imported-package))
					  symbol-names)
				  package)))
	  shadowing-import-from)
    (use-package use package)
    (mapc #'(lambda (list)
	      (let ((imported-package (find-package (car list)))
		    (symbol-names (cdr list)))
		(import (mapcar #'(lambda (symbol-name)
				    (find-symbol symbol-name imported-package))
				symbol-names)
			package)))
	  import-from)
    (mapc #'(lambda (symbol-name) (intern symbol-name package)) intern)
    (export export package)
    package))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:36263 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'ensure-package',[name,nicknames,shadow,'shadowing-import-from',use,'import-from',intern,export],[let,[[package,['find-package',name]]],[unless,package,[setq,package,['make-package',name,':nicknames',nicknames]]],[shadow,shadow,package],[mapc,function([lambda,[list],[let,[['imported-package',['find-package',[car,list]]],['symbol-names',[cdr,list]]],['shadowing-import',[mapcar,function([lambda,['symbol-name'],['find-symbol','symbol-name','imported-package']]),'symbol-names'],package]]]),'shadowing-import-from'],['use-package',use,package],[mapc,function([lambda,[list],[let,[['imported-package',['find-package',[car,list]]],['symbol-names',[cdr,list]]],[import,[mapcar,function([lambda,['symbol-name'],['find-symbol','symbol-name','imported-package']]),'symbol-names'],package]]]),'import-from'],[mapc,function([lambda,['symbol-name'],[intern,'symbol-name',package]]),intern],[export,export,package],package]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_ensure_package,
					       kw_function,
					       f_sys_ensure_package)).
*/
wl:lambda_def(defun, sys_ensure_package, f_sys_ensure_package, [sys_name, sys_nicknames, shadow, sys_shadowing_import_from, sys_use, sys_import_from, intern, export], [[let, [[package, [find_package, sys_name]]], [unless, package, [setq, package, [make_package, sys_name, kw_nicknames, sys_nicknames]]], [shadow, shadow, package], [mapc, function([lambda, [list], [let, [[sys_imported_package, [find_package, [car, list]]], [sys_symbol_names, [cdr, list]]], [shadowing_import, [mapcar, function([lambda, [symbol_name], [find_symbol, symbol_name, sys_imported_package]]), sys_symbol_names], package]]]), sys_shadowing_import_from], [use_package, sys_use, package], [mapc, function([lambda, [list], [let, [[sys_imported_package, [find_package, [car, list]]], [sys_symbol_names, [cdr, list]]], [import, [mapcar, function([lambda, [symbol_name], [find_symbol, symbol_name, sys_imported_package]]), sys_symbol_names], package]]]), sys_import_from], [mapc, function([lambda, [symbol_name], [intern, symbol_name, package]]), intern], [export, export, package], package]]).
wl:arglist_info(sys_ensure_package, f_sys_ensure_package, [sys_name, sys_nicknames, shadow, sys_shadowing_import_from, sys_use, sys_import_from, intern, export], arginfo{all:[sys_name, sys_nicknames, shadow, sys_shadowing_import_from, sys_use, sys_import_from, intern, export], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_nicknames, shadow, sys_shadowing_import_from, sys_use, sys_import_from, intern, export], opt:0, req:[sys_name, sys_nicknames, shadow, sys_shadowing_import_from, sys_use, sys_import_from, intern, export], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_ensure_package).

/*

### Compiled Function: `SYS::ENSURE-PACKAGE` 
*/
f_sys_ensure_package(Name_In, Nicknames_In, Shadow_In, Shadowing_import_from_In, Use_In, Import_from_In, Intern_In, Export_In, FnResult) :-
	GEnv=[bv(sys_name, Name_In), bv(sys_nicknames, Nicknames_In), bv(shadow, Shadow_In), bv(sys_shadowing_import_from, Shadowing_import_from_In), bv(sys_use, Use_In), bv(sys_import_from, Import_from_In), bv(intern, Intern_In), bv(export, Export_In)],
	catch(( ( get_var(GEnv, sys_name, Name_Get),
		  f_find_package(Name_Get, Package_Init),
		  LEnv=[bv(package, Package_Init)|GEnv],
		  get_var(LEnv, package, IFTEST),
		  (   IFTEST\==[]
		  ->  _10462=[]
		  ;   get_var(LEnv, sys_name, Name_Get21),
		      get_var(LEnv, sys_nicknames, Nicknames_Get),
		      f_make_package(Name_Get21,
				     [kw_nicknames, Nicknames_Get],
				     ElseResult),
		      set_var(LEnv, package, ElseResult),
		      _10462=ElseResult
		  ),
		  get_var(LEnv, package, Package_Get25),
		  get_var(LEnv, shadow, Shadow_Get),
		  f_shadow(Shadow_Get, Package_Get25, Shadow_Ret),
		  get_var(LEnv,
			  sys_shadowing_import_from,
			  Shadowing_import_from_Get),
		  f_mapc(closure(kw_function,
				 [ClosureEnvironment43|LEnv],
				 Whole44,
				 LetResult27,
				 [list],
				 (get_var(ClosureEnvironment43, list, List_Get), f_car(List_Get, Find_package_Param), f_find_package(Find_package_Param, Imported_package_Init), get_var(ClosureEnvironment43, list, List_Get30), f_cdr(List_Get30, Symbol_names_Init), LEnv28=[bv(sys_imported_package, Imported_package_Init), bv(sys_symbol_names, Symbol_names_Init)|ClosureEnvironment43], get_var(LEnv28, sys_symbol_names, Symbol_names_Get), f_mapcar(closure(kw_function, [ClosureEnvironment|LEnv28], Whole, LResult, [symbol_name],  (get_var(ClosureEnvironment, symbol_name, Symbol_name_Get), get_var(ClosureEnvironment, sys_imported_package, Imported_package_Get), f_find_symbol(Symbol_name_Get, Imported_package_Get, LResult)), [lambda, [symbol_name], [find_symbol, symbol_name, sys_imported_package]]), [Symbol_names_Get], Shadowing_import_Param), get_var(LEnv28, package, Package_Get40), f_shadowing_import(Shadowing_import_Param, Package_Get40, LetResult27)),
				 
				 [ lambda,
				   [list],
				   
				   [ let,
				     
				     [ 
				       [ sys_imported_package,
					 [find_package, [car, list]]
				       ],
				       [sys_symbol_names, [cdr, list]]
				     ],
				     
				     [ shadowing_import,
				       
				       [ mapcar,
					 function(
						  [ lambda,
						    [symbol_name],
						    
						    [ find_symbol,
						      symbol_name,
						      sys_imported_package
						    ]
						  ]),
					 sys_symbol_names
				       ],
				       package
				     ]
				   ]
				 ]),
			 [Shadowing_import_from_Get],
			 Mapc_Ret),
		  get_var(LEnv, package, Package_Get47),
		  get_var(LEnv, sys_use, Use_Get),
		  f_use_package(Use_Get, Package_Get47, Use_package_Ret),
		  get_var(LEnv, sys_import_from, Import_from_Get),
		  f_mapc(closure(kw_function,
				 [ClosureEnvironment65|LEnv],
				 Whole66,
				 LetResult49,
				 [list],
				 (get_var(ClosureEnvironment65, list, List_Get51), f_car(List_Get51, Find_package_Param83), f_find_package(Find_package_Param83, Imported_package_Init53), get_var(ClosureEnvironment65, list, List_Get52), f_cdr(List_Get52, Symbol_names_Init54), LEnv50=[bv(sys_imported_package, Imported_package_Init53), bv(sys_symbol_names, Symbol_names_Init54)|ClosureEnvironment65], get_var(LEnv50, sys_symbol_names, Symbol_names_Get61), f_mapcar(closure(kw_function, [ClosureEnvironment59|LEnv50], Whole60, LResult57, [symbol_name],  (get_var(ClosureEnvironment59, symbol_name, Symbol_name_Get55), get_var(ClosureEnvironment59, sys_imported_package, Imported_package_Get56), f_find_symbol(Symbol_name_Get55, Imported_package_Get56, LResult57)), [lambda, [symbol_name], [find_symbol, symbol_name, sys_imported_package]]), [Symbol_names_Get61], Import_Param), get_var(LEnv50, package, Package_Get62), f_import(Import_Param, Package_Get62, LetResult49)),
				 
				 [ lambda,
				   [list],
				   
				   [ let,
				     
				     [ 
				       [ sys_imported_package,
					 [find_package, [car, list]]
				       ],
				       [sys_symbol_names, [cdr, list]]
				     ],
				     
				     [ import,
				       
				       [ mapcar,
					 function(
						  [ lambda,
						    [symbol_name],
						    
						    [ find_symbol,
						      symbol_name,
						      sys_imported_package
						    ]
						  ]),
					 sys_symbol_names
				       ],
				       package
				     ]
				   ]
				 ]),
			 [Import_from_Get],
			 Mapc_Ret88),
		  get_var(LEnv, intern, Intern_Get),
		  f_mapc(closure(kw_function,
				 [ClosureEnvironment72|LEnv],
				 Whole73,
				 LResult70,
				 [symbol_name],
				 (get_var(ClosureEnvironment72, package, Package_Get69), get_var(ClosureEnvironment72, symbol_name, Symbol_name_Get68), f_intern(Symbol_name_Get68, Package_Get69, LResult70)),
				 
				 [ lambda,
				   [symbol_name],
				   [intern, symbol_name, package]
				 ]),
			 [Intern_Get],
			 Mapc_Ret89),
		  get_var(LEnv, export, Export_Get),
		  get_var(LEnv, package, Package_Get76),
		  f_export(Export_Get, Package_Get76, Export_Ret),
		  get_var(LEnv, package, Package_Get77)
		),
		Package_Get77=FnResult
	      ),
	      block_exit(sys_ensure_package, FnResult),
	      true).
:- set_opv(sys_ensure_package, symbol_function, f_sys_ensure_package),
   DefunResult=sys_ensure_package.
/*
:- side_effect(assert_lsp(sys_ensure_package,
			  lambda_def(defun,
				     sys_ensure_package,
				     f_sys_ensure_package,
				     
				     [ sys_name,
				       sys_nicknames,
				       shadow,
				       sys_shadowing_import_from,
				       sys_use,
				       sys_import_from,
				       intern,
				       export
				     ],
				     
				     [ 
				       [ let,
					 [[package, [find_package, sys_name]]],
					 
					 [ unless,
					   package,
					   
					   [ setq,
					     package,
					     
					     [ make_package,
					       sys_name,
					       kw_nicknames,
					       sys_nicknames
					     ]
					   ]
					 ],
					 [shadow, shadow, package],
					 
					 [ mapc,
					   function(
						    [ lambda,
						      [list],
						      
						      [ let,
							
							[ 
							  [ sys_imported_package,
							    
							    [ find_package,
							      [car, list]
							    ]
							  ],
							  
							  [ sys_symbol_names,
							    [cdr, list]
							  ]
							],
							
							[ shadowing_import,
							  
							  [ mapcar,
							    function(
								     [ lambda,
								       [symbol_name],
								       
								       [ find_symbol,
									 symbol_name,
									 sys_imported_package
								       ]
								     ]),
							    sys_symbol_names
							  ],
							  package
							]
						      ]
						    ]),
					   sys_shadowing_import_from
					 ],
					 [use_package, sys_use, package],
					 
					 [ mapc,
					   function(
						    [ lambda,
						      [list],
						      
						      [ let,
							
							[ 
							  [ sys_imported_package,
							    
							    [ find_package,
							      [car, list]
							    ]
							  ],
							  
							  [ sys_symbol_names,
							    [cdr, list]
							  ]
							],
							
							[ import,
							  
							  [ mapcar,
							    function(
								     [ lambda,
								       [symbol_name],
								       
								       [ find_symbol,
									 symbol_name,
									 sys_imported_package
								       ]
								     ]),
							    sys_symbol_names
							  ],
							  package
							]
						      ]
						    ]),
					   sys_import_from
					 ],
					 
					 [ mapc,
					   function(
						    [ lambda,
						      [symbol_name],
						      
						      [ intern,
							symbol_name,
							package
						      ]
						    ]),
					   intern
					 ],
					 [export, export, package],
					 package
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_ensure_package,
			  arglist_info(sys_ensure_package,
				       f_sys_ensure_package,
				       
				       [ sys_name,
					 sys_nicknames,
					 shadow,
					 sys_shadowing_import_from,
					 sys_use,
					 sys_import_from,
					 intern,
					 export
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_nicknames,
						      shadow,
						      sys_shadowing_import_from,
						      sys_use,
						      sys_import_from,
						      intern,
						      export
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_nicknames,
							shadow,
							sys_shadowing_import_from,
							sys_use,
							sys_import_from,
							intern,
							export
						      ],
						opt:0,
						req:
						    [ sys_name,
						      sys_nicknames,
						      shadow,
						      sys_shadowing_import_from,
						      sys_use,
						      sys_import_from,
						      intern,
						      export
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_ensure_package,
			  init_args(x, f_sys_ensure_package))).
*/
/*
#+WAM-CL
#+(or WAM-CL LISP500) 
(defmacro defpackage (defined-package-name &rest options)
  (flet ((option (option-name)
	   (mapcan #'(lambda (option)
		       (when (eq (car option) option-name)
			 (mapcar #'designator-string (cdr option))))
		   options))
	 (options (option-name)
	   (mapcan #'(lambda (option)
		       (when (eq (car option) option-name)
			 (list (mapcar #'designator-string (cdr option)))))
		   options)))
    `(ensure-package ,(designator-string defined-package-name)
      ,(option :nicknames)
      ,(option :shadow) ,(options :shadowing-import-from) ,(option :use)
      ,(options :import-from) ,(option :intern) ,(option :export))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:37286 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defpackage,['defined-package-name','&rest',options],[flet,[[option,['option-name'],[mapcan,function([lambda,[option],[when,[eq,[car,option],'option-name'],[mapcar,function('designator-string'),[cdr,option]]]]),options]],[options,['option-name'],[mapcan,function([lambda,[option],[when,[eq,[car,option],'option-name'],[list,[mapcar,function('designator-string'),[cdr,option]]]]]),options]]],['#BQ',['ensure-package',['#COMMA',['designator-string','defined-package-name']],['#COMMA',[option,':nicknames']],['#COMMA',[option,':shadow']],['#COMMA',[options,':shadowing-import-from']],['#COMMA',[option,':use']],['#COMMA',[options,':import-from']],['#COMMA',[option,':intern']],['#COMMA',[option,':export']]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       defpackage,
					       kw_macro,
					       mf_defpackage)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_option,
					       kw_function,
					       f_sys_option)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_options,
					       kw_function,
					       f_sys_options)).
*/
wl:lambda_def(defmacro, defpackage, mf_defpackage, [sys_defined_package_name, c38_rest, sys_options], [[flet, [[sys_option, [sys_option_name], [mapcan, function([lambda, [sys_option], [when, [eq, [car, sys_option], sys_option_name], [mapcar, function(sys_designator_string), [cdr, sys_option]]]]), sys_options]], [sys_options, [sys_option_name], [mapcan, function([lambda, [sys_option], [when, [eq, [car, sys_option], sys_option_name], [list, [mapcar, function(sys_designator_string), [cdr, sys_option]]]]]), sys_options]]], ['#BQ', [sys_ensure_package, ['#COMMA', [sys_designator_string, sys_defined_package_name]], ['#COMMA', [sys_option, kw_nicknames]], ['#COMMA', [sys_option, kw_shadow]], ['#COMMA', [sys_options, kw_shadowing_import_from]], ['#COMMA', [sys_option, kw_use]], ['#COMMA', [sys_options, kw_import_from]], ['#COMMA', [sys_option, kw_intern]], ['#COMMA', [sys_option, kw_export]]]]]]).
wl:arglist_info(defpackage, mf_defpackage, [sys_defined_package_name, c38_rest, sys_options], arginfo{all:[sys_defined_package_name], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_defined_package_name, sys_options], opt:0, req:[sys_defined_package_name], rest:[sys_options], sublists:0, whole:0}).
wl: init_args(1, mf_defpackage).

/*

### Compiled Macro Operator: `CL:DEFPACKAGE` 
*/
sf_defpackage(MacroEnv, Defined_package_name_In, RestNKeys, FResult) :-
	mf_defpackage([defpackage, Defined_package_name_In|RestNKeys],
		      MacroEnv,
		      MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFPACKAGE` 
*/
mf_defpackage([defpackage, Defined_package_name_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	Env=[bv(sys_defined_package_name, Defined_package_name_In), bv(sys_options, RestNKeys)],
	catch(( ( assert_lsp(sys_option,
			     wl:lambda_def(defun, sys_option, f_sys_option1, [sys_option_name], [[mapcan, function([lambda, [sys_option], [when, [eq, [car, sys_option], sys_option_name], [mapcar, function(sys_designator_string), [cdr, sys_option]]]]), sys_options]])),
		  assert_lsp(sys_option,
			     wl:arglist_info(sys_option, f_sys_option1, [sys_option_name], arginfo{all:[sys_option_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_option_name], opt:0, req:[sys_option_name], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_option, wl:init_args(1, f_sys_option1)),
		  assert_lsp(sys_option,
			     (f_sys_option1(Option_name_In, RestNKeys8, FnResult):-GEnv=[bv(sys_option_name, Option_name_In)], catch(((get_var(GEnv, sys_options, Options_Get), f_mapcan(closure(kw_function, [ClosureEnvironment|GEnv], Whole, LResult, [sys_option],  (get_var(ClosureEnvironment, sys_option, Option_Get), f_car(Option_Get, PredArg1Result), get_var(ClosureEnvironment, sys_option_name, Option_name_Get), (is_eq(PredArg1Result, Option_name_Get)->get_var(ClosureEnvironment, sys_option, Option_Get16), f_cdr(Option_Get16, Cdr_Ret), f_mapcar(f_sys_designator_string, [Cdr_Ret], TrueResult), LResult=TrueResult;LResult=[])), [lambda, [sys_option], [when, [eq, [car, sys_option], sys_option_name], [mapcar, function(sys_designator_string), [cdr, sys_option]]]]), [Options_Get], Mapcan_Ret)), Mapcan_Ret=FnResult), block_exit(sys_option, FnResult), true))),
		  assert_lsp(sys_options,
			     wl:lambda_def(defun, sys_options, f_sys_options1, [sys_option_name], [[mapcan, function([lambda, [sys_option], [when, [eq, [car, sys_option], sys_option_name], [list, [mapcar, function(sys_designator_string), [cdr, sys_option]]]]]), sys_options]])),
		  assert_lsp(sys_options,
			     wl:arglist_info(sys_options, f_sys_options1, [sys_option_name], arginfo{all:[sys_option_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_option_name], opt:0, req:[sys_option_name], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_options, wl:init_args(1, f_sys_options1)),
		  assert_lsp(sys_options,
			     (f_sys_options1(Option_name_In26, RestNKeys25, FnResult24):-GEnv45=[bv(sys_option_name, Option_name_In26)], catch(((get_var(GEnv45, sys_options, Options_Get39), f_mapcan(closure(kw_function, [ClosureEnvironment37|GEnv45], Whole38, LResult35, [sys_option],  (get_var(ClosureEnvironment37, sys_option, Option_Get28), f_car(Option_Get28, PredArg1Result31), get_var(ClosureEnvironment37, sys_option_name, Option_name_Get29), (is_eq(PredArg1Result31, Option_name_Get29)->get_var(ClosureEnvironment37, sys_option, Option_Get33), f_cdr(Option_Get33, Cdr_Ret56), f_mapcar(f_sys_designator_string, [Cdr_Ret56], Mapcar_Ret), TrueResult34=[Mapcar_Ret], LResult35=TrueResult34;LResult35=[])), [lambda, [sys_option], [when, [eq, [car, sys_option], sys_option_name], [list, [mapcar, function(sys_designator_string), [cdr, sys_option]]]]]), [Options_Get39], Mapcan_Ret55)), Mapcan_Ret55=FnResult24), block_exit(sys_options, FnResult24), true))),
		  get_var(
			  [ 
			    [ fbound(sys_option, kw_function)=function(f_sys_option1),
			      fbound(sys_options, kw_function)=function(f_sys_options1)
			    ]
			  | Env
			  ],
			  sys_defined_package_name,
			  Defined_package_name_Get),
		  f_sys_designator_string(Defined_package_name_Get,
					  Designator_string_Ret),
		  f_sys_option1(kw_nicknames, KeysNRest),
		  f_sys_option1(kw_shadow, KeysNRest47),
		  f_sys_options1(kw_shadowing_import_from, KeysNRest48),
		  f_sys_option1(kw_use, KeysNRest49),
		  f_sys_options1(kw_import_from, KeysNRest50),
		  f_sys_option1(kw_intern, KeysNRest51),
		  f_sys_option1(kw_export, KeysNRest52)
		),
		[sys_ensure_package, Designator_string_Ret, KeysNRest, KeysNRest47, KeysNRest48, KeysNRest49, KeysNRest50, KeysNRest51, KeysNRest52]=MFResult
	      ),
	      block_exit(defpackage, MFResult),
	      true).
:- set_opv(mf_defpackage, type_of, sys_macro),
   set_opv(defpackage, symbol_function, mf_defpackage),
   DefMacroResult=defpackage.
/*
:- side_effect(assert_lsp(defpackage,
			  lambda_def(defmacro,
				     defpackage,
				     mf_defpackage,
				     
				     [ sys_defined_package_name,
				       c38_rest,
				       sys_options
				     ],
				     
				     [ 
				       [ flet,
					 
					 [ 
					   [ sys_option,
					     [sys_option_name],
					     
					     [ mapcan,
					       function(
							[ lambda,
							  [sys_option],
							  
							  [ when,
							    
							    [ eq,
							      [car, sys_option],
							      sys_option_name
							    ],
							    
							    [ mapcar,
							      function(sys_designator_string),
							      [cdr, sys_option]
							    ]
							  ]
							]),
					       sys_options
					     ]
					   ],
					   
					   [ sys_options,
					     [sys_option_name],
					     
					     [ mapcan,
					       function(
							[ lambda,
							  [sys_option],
							  
							  [ when,
							    
							    [ eq,
							      [car, sys_option],
							      sys_option_name
							    ],
							    
							    [ list,
							      
							      [ mapcar,
								function(sys_designator_string),
								[cdr, sys_option]
							      ]
							    ]
							  ]
							]),
					       sys_options
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ sys_ensure_package,
					     
					     [ '#COMMA',
					       
					       [ sys_designator_string,
						 sys_defined_package_name
					       ]
					     ],
					     
					     [ '#COMMA',
					       [sys_option, kw_nicknames]
					     ],
					     ['#COMMA', [sys_option, kw_shadow]],
					     
					     [ '#COMMA',
					       
					       [ sys_options,
						 kw_shadowing_import_from
					       ]
					     ],
					     ['#COMMA', [sys_option, kw_use]],
					     
					     [ '#COMMA',
					       [sys_options, kw_import_from]
					     ],
					     ['#COMMA', [sys_option, kw_intern]],
					     ['#COMMA', [sys_option, kw_export]]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(defpackage,
			  arglist_info(defpackage,
				       mf_defpackage,
				       
				       [ sys_defined_package_name,
					 c38_rest,
					 sys_options
				       ],
				       arginfo{ all:[sys_defined_package_name],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_defined_package_name,
							sys_options
						      ],
						opt:0,
						req:[sys_defined_package_name],
						rest:[sys_options],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defpackage, init_args(1, mf_defpackage))).
*/
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defun backquote-expand (list level)
  (if (consp list) 
      (if (eq 'backquote (car list)) 
	  (list 'list ''backquote 
		(backquote-expand (car (cdr list)) (+ level 1))) 
	  (if (eq 'unquote (car list)) 
	      (if (= level 0) 
		  (car (cdr list)) 
		  (list 'list ''unquote 
			(backquote-expand (car (cdr list)) (- level 1)))) 
	      (if (eq 'unquote-splicing (car list)) 
		  (if (= level 0) 
		      (values (car (cdr list)) t) 
		      (list 'list ''unquote-splicing 
			    (backquote-expand (car (cdr list)) (- level 1)))) 
		  (labels ((collect (list) 
			     (if (consp list) 
				 (cons (multiple-value-call 
					   #'(lambda (value 
						      &optional splicingp) 
					       (if splicingp 
						   value 
						   (list 'list value))) 
				       (backquote-expand (car list) level)) 
				     (collect (cdr list))) 
				 (list (list 'quote list))))) 
		    (cons 'append (collect list)))))) 
      (list 'quote list))) 

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:37973 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,'backquote-expand',[list,level],[if,[consp,list],[if,[eq,[quote,backquote],[car,list]],[list,[quote,list],[quote,[quote,backquote]],['backquote-expand',[car,[cdr,list]],[+,level,1]]],[if,[eq,[quote,unquote],[car,list]],[if,[=,level,0],[car,[cdr,list]],[list,[quote,list],[quote,[quote,unquote]],['backquote-expand',[car,[cdr,list]],[-,level,1]]]],[if,[eq,[quote,'unquote-splicing'],[car,list]],[if,[=,level,0],[values,[car,[cdr,list]],t],[list,[quote,list],[quote,[quote,'unquote-splicing']],['backquote-expand',[car,[cdr,list]],[-,level,1]]]],[labels,[[collect,[list],[if,[consp,list],[cons,['multiple-value-call',function([lambda,[value,'&optional',splicingp],[if,splicingp,value,[list,[quote,list],value]]]),['backquote-expand',[car,list],level]],[collect,[cdr,list]]],[list,[list,[quote,quote],list]]]]],[cons,[quote,append],[collect,list]]]]]],[list,[quote,quote],list]]]]]))
/*
#+BUILTIN
#+(or WAM-CL LISP500) 
(defmacro backquote (form)
  (backquote-expand form 0))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:38983 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defmacro,backquote,[form],['backquote-expand',form,0]]]]))
/*
#+(or WAM-CL LISP500) 
(defmacro with-simple-restart ((name format-control &rest format-arguments)
			       &rest forms)
  (let ((tag (gensym)))
    `(block ,tag
      (restart-bind
	  ((,name
	    #'(lambda () (return-from ,tag (values nil t)))
	     :interactive-function #'(lambda () nil)
	     :report-function #'(lambda (stream)
				  (apply #'format stream ',format-control
					 ',format-arguments))
	     :test-function #'(lambda () t)))
	,@forms))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:39080 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'with-simple-restart',[[name,'format-control','&rest','format-arguments'],'&rest',forms],[let,[[tag,[gensym]]],['#BQ',[block,['#COMMA',tag],['restart-bind',[[['#COMMA',name],function([lambda,[],['return-from',['#COMMA',tag],[values,[],t]]]),':interactive-function',function([lambda,[],[]]),':report-function',function([lambda,[stream],[apply,function(format),stream,[quote,['#COMMA','format-control']],[quote,['#COMMA','format-arguments']]]]),':test-function',function([lambda,[],t])]],['#BQ-COMMA-ELIPSE',forms]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_all_cdr, kw_function)=function(f_sys_all_cdr1),
						 fbound(sys_all_car, kw_function)=function(f_sys_all_car1),
						 fbound(sys_all_end, kw_function)=function(f_sys_all_end1),
						 fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       with_simple_restart,
					       kw_special,
					       sf_with_simple_restart)).
*/
wl:lambda_def(defmacro, with_simple_restart, mf_with_simple_restart, [[sys_name, sys_format_control, c38_rest, sys_format_arguments], c38_rest, sys_forms], [[let, [[sys_tag, [gensym]]], ['#BQ', [block, ['#COMMA', sys_tag], [restart_bind, [[['#COMMA', sys_name], function([lambda, [], [return_from, ['#COMMA', sys_tag], [values, [], t]]]), kw_interactive_function, function([lambda, [], []]), kw_report_function, function([lambda, [stream], [apply, function(format), stream, [quote, ['#COMMA', sys_format_control]], [quote, ['#COMMA', sys_format_arguments]]]]), kw_test_function, function([lambda, [], t])]], ['#BQ-COMMA-ELIPSE', sys_forms]]]]]]).
wl:arglist_info(with_simple_restart, mf_with_simple_restart, [[sys_name, sys_format_control, c38_rest, sys_format_arguments], c38_rest, sys_forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_forms, sys_name, sys_format_control, sys_format_arguments], opt:0, req:0, rest:[sys_forms], sublists:0, whole:0}).
wl: init_args(1, mf_with_simple_restart).

/*

### Compiled Macro Operator: `CL:WITH-SIMPLE-RESTART` 
*/
sf_with_simple_restart(SubEnv, [Name_In, Format_control_In|Format_arguments_In], RestNKeys, FResult) :-
	mf_with_simple_restart(
			       [ with_simple_restart,
				 
				 [ Name_In,
				   Format_control_In
				 | Format_arguments_In
				 ]
			       | RestNKeys
			       ],
			       SubEnv,
			       MFResult),
	f_sys_env_eval(SubEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:WITH-SIMPLE-RESTART` 
*/
mf_with_simple_restart([with_simple_restart, [Name_In, Format_control_In|Format_arguments_In]|RestNKeys], SubEnv, MFResult) :-
	nop(defmacro),
	CDR=[bv(sys_forms, RestNKeys), bv(sys_name, Name_In), bv(sys_format_control, Format_control_In), bv(sys_format_arguments, Format_arguments_In)],
	catch(( ( f_gensym(Tag_Init),
		  LEnv=[bv(sys_tag, Tag_Init)|CDR],
		  ( get_var(LEnv, sys_forms, Forms_Get),
		    get_var(LEnv, sys_name, Name_Get)
		  ),
		  get_var(LEnv, sys_tag, Tag_Get)
		),
		[block, Tag_Get, [restart_bind, [[Name_Get, function([lambda, [], [return_from, ['#COMMA', sys_tag], [values, [], t]]]), kw_interactive_function, function([lambda, [], []]), kw_report_function, function([lambda, [stream], [apply, function(format), stream, [quote, ['#COMMA', sys_format_control]], [quote, ['#COMMA', sys_format_arguments]]]]), kw_test_function, function([lambda, [], t])]]|Forms_Get]]=MFResult
	      ),
	      block_exit(with_simple_restart, MFResult),
	      true).
:- set_opv(mf_with_simple_restart, type_of, sys_macro),
   set_opv(with_simple_restart, symbol_function, mf_with_simple_restart),
   DefMacroResult=with_simple_restart.
/*
:- side_effect(assert_lsp(with_simple_restart,
			  lambda_def(defmacro,
				     with_simple_restart,
				     mf_with_simple_restart,
				     
				     [ 
				       [ sys_name,
					 sys_format_control,
					 c38_rest,
					 sys_format_arguments
				       ],
				       c38_rest,
				       sys_forms
				     ],
				     
				     [ 
				       [ let,
					 [[sys_tag, [gensym]]],
					 
					 [ '#BQ',
					   
					   [ block,
					     ['#COMMA', sys_tag],
					     
					     [ restart_bind,
					       
					       [ 
						 [ ['#COMMA', sys_name],
						   function(
							    [ lambda,
							      [],
							      
							      [ return_from,
								['#COMMA', sys_tag],
								[values, [], t]
							      ]
							    ]),
						   kw_interactive_function,
						   function([lambda, [], []]),
						   kw_report_function,
						   function(
							    [ lambda,
							      [stream],
							      
							      [ apply,
								function(format),
								stream,
								
								[ quote,
								  
								  [ '#COMMA',
								    sys_format_control
								  ]
								],
								
								[ quote,
								  
								  [ '#COMMA',
								    sys_format_arguments
								  ]
								]
							      ]
							    ]),
						   kw_test_function,
						   function([lambda, [], t])
						 ]
					       ],
					       ['#BQ-COMMA-ELIPSE', sys_forms]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(with_simple_restart,
			  arglist_info(with_simple_restart,
				       mf_with_simple_restart,
				       
				       [ 
					 [ sys_name,
					   sys_format_control,
					   c38_rest,
					   sys_format_arguments
					 ],
					 c38_rest,
					 sys_forms
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_forms,
							sys_name,
							sys_format_control,
							sys_format_arguments
						      ],
						opt:0,
						req:0,
						rest:[sys_forms],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(with_simple_restart,
			  init_args(1, mf_with_simple_restart))).
*/
/*
#+(or WAM-CL LISP500) 
(defun break (&optional format-control &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger (make-condition 'simple-condition
				       :format-control format-control
				       :format-arguments format-arguments))))
  nil)



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-30.lisp:39558 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,break,['&optional','format-control','&rest','format-arguments'],['with-simple-restart',[continue,'$STRING'("Return from BREAK.")],[let,[['*debugger-hook*',[]]],['invoke-debugger',['make-condition',[quote,'simple-condition'],':format-control','format-control',':format-arguments','format-arguments']]]],[]])
/*
% macroexpand:-[with_simple_restart,[continue,'$ARRAY'([*],claz_base_character,"Return from BREAK.")],[let,[[xx_debugger_hook_xx,[]]],[invoke_debugger,[make_condition,[quote,simple_condition],kw_format_control,sys_format_control,kw_format_arguments,sys_format_arguments]]]].
*/
/*
% into:-[block,g21,[restart_bind,[[continue,function([lambda,[],[return_from,['#COMMA',sys_tag],[values,[],t]]]),kw_interactive_function,function([lambda,[],[]]),kw_report_function,function([lambda,[stream],[apply,function(format),stream,[quote,['#COMMA',sys_format_control]],[quote,['#COMMA',sys_format_arguments]]]]),kw_test_function,function([lambda,[],t])]],[let,[[xx_debugger_hook_xx,[]]],[invoke_debugger,[make_condition,[quote,simple_condition],kw_format_control,sys_format_control,kw_format_arguments,sys_format_arguments]]]]].
*/
/*
% macroexpand:-[restart_bind,[[continue,function([lambda,[],[return_from,['#COMMA',sys_tag],[values,[],t]]]),kw_interactive_function,function([lambda,[],[]]),kw_report_function,function([lambda,[stream],[apply,function(format),stream,[quote,['#COMMA',sys_format_control]],[quote,['#COMMA',sys_format_arguments]]]]),kw_test_function,function([lambda,[],t])]],[let,[[xx_debugger_hook_xx,[]]],[invoke_debugger,[make_condition,[quote,simple_condition],kw_format_control,sys_format_control,kw_format_arguments,sys_format_arguments]]]].
*/
/*
% into:-[let,[[sys_xx_restarts_xx,[cons,[sys_make_restart,[quote,continue],function([lambda,[],[return_from,['#COMMA',sys_tag],[values,[],t]]]),kw_interactive_function,function([lambda,[],[]]),kw_report_function,function([lambda,[stream],[apply,function(format),stream,[quote,['#COMMA',sys_format_control]],[quote,['#COMMA',sys_format_arguments]]]]),kw_test_function,function([lambda,[],t])],sys_xx_restarts_xx]]],[let,[[xx_debugger_hook_xx,[]]],[invoke_debugger,[make_condition,[quote,simple_condition],kw_format_control,sys_format_control,kw_format_arguments,sys_format_arguments]]]].
*/
wl:lambda_def(defun, break, f_break, [c38_optional, sys_format_control, c38_rest, sys_format_arguments], [[with_simple_restart, [continue, '$ARRAY'([*], claz_base_character, "Return from BREAK.")], [let, [[xx_debugger_hook_xx, []]], [invoke_debugger, [make_condition, [quote, simple_condition], kw_format_control, sys_format_control, kw_format_arguments, sys_format_arguments]]]], []]).
wl:arglist_info(break, f_break, [c38_optional, sys_format_control, c38_rest, sys_format_arguments], arginfo{all:[sys_format_control], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_format_control, sys_format_arguments], opt:[sys_format_control], req:0, rest:[sys_format_arguments], sublists:0, whole:0}).
wl: init_args(0, f_break).

/*

### Compiled Function: `CL:BREAK` 
*/
f_break(Optionals, FnResult) :-
	GEnv=[bv(sys_format_control, Format_control_In), bv(sys_format_arguments, Optionals)],
	opt_var(Env, sys_format_control, Format_control_In, true, [], 1, Optionals),
	catch(( catch(( ( f_sys_make_restart(continue,
					     closure(kw_function,
						     [BlockExitEnv|GEnv],
						     Whole,
						     ThrowResult,
						     [],
						     (nb_setval('$mv_return', [[], t]), throw(block_exit(['#COMMA', sys_tag], []))),
						     
						     [ lambda,
						       [],
						       
						       [ return_from,
							 ['#COMMA', sys_tag],
							 [values, [], t]
						       ]
						     ]),
					     kw_interactive_function,
					     closure(kw_function,
						     
						     [ ClosureEnvironment18
						     | GEnv
						     ],
						     Whole19,
						     [],
						     [],
						     true,
						     [lambda, [], []]),
					     kw_report_function,
					     closure(kw_function,
						     
						     [ ClosureEnvironment23
						     | GEnv
						     ],
						     Whole24,
						     LResult21,
						     [stream],
						     (get_var(ClosureEnvironment23, stream, Stream_Get), f_apply(f_format, [Stream_Get, ['#COMMA', sys_format_control], ['#COMMA', sys_format_arguments]], LResult21)),
						     
						     [ lambda,
						       [stream],
						       
						       [ apply,
							 function(format),
							 stream,
							 
							 [ quote,
							   
							   [ '#COMMA',
							     sys_format_control
							   ]
							 ],
							 
							 [ quote,
							   
							   [ '#COMMA',
							     sys_format_arguments
							   ]
							 ]
						       ]
						     ]),
					     kw_test_function,
					     closure(kw_function,
						     
						     [ ClosureEnvironment26
						     | GEnv
						     ],
						     Whole27,
						     t,
						     [],
						     true,
						     [lambda, [], t]),
					     Make_restart_Ret),
			  get_var(GEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get),
			  Xx_restarts_xx_Init=[Make_restart_Ret|Xx_restarts_xx_Get],
			  LEnv=[bv(sys_xx_restarts_xx, Xx_restarts_xx_Init)|GEnv],
			  locally_set(xx_debugger_hook_xx,
				      [],
				      (get_var(LEnv, sys_format_arguments, Format_arguments_Get), get_var(LEnv, sys_format_control, Format_control_Get), f_make_condition(simple_condition, kw_format_control, Format_control_Get, kw_format_arguments, Format_arguments_Get, Invoke_debugger_Param), f_invoke_debugger(Invoke_debugger_Param, LetResult)))
			),
			LetResult=Block_exit_Ret
		      ),
		      block_exit(g21, Block_exit_Ret),
		      true),
		[]=FnResult
	      ),
	      block_exit(break, FnResult),
	      true).
:- set_opv(break, symbol_function, f_break),
   DefunResult=break.
/*
:- side_effect(assert_lsp(break,
			  lambda_def(defun,
				     break,
				     f_break,
				     
				     [ c38_optional,
				       sys_format_control,
				       c38_rest,
				       sys_format_arguments
				     ],
				     
				     [ 
				       [ with_simple_restart,
					 
					 [ continue,
					   '$ARRAY'([*],
						    claz_base_character,
						    "Return from BREAK.")
					 ],
					 
					 [ let,
					   [[xx_debugger_hook_xx, []]],
					   
					   [ invoke_debugger,
					     
					     [ make_condition,
					       [quote, simple_condition],
					       kw_format_control,
					       sys_format_control,
					       kw_format_arguments,
					       sys_format_arguments
					     ]
					   ]
					 ]
				       ],
				       []
				     ]))).
*/
/*
:- side_effect(assert_lsp(break,
			  arglist_info(break,
				       f_break,
				       
				       [ c38_optional,
					 sys_format_control,
					 c38_rest,
					 sys_format_arguments
				       ],
				       arginfo{ all:[sys_format_control],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_format_control,
							sys_format_arguments
						      ],
						opt:[sys_format_control],
						req:0,
						rest:[sys_format_arguments],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(break, init_args(0, f_break))).
*/


%; Total compilation time: 99.619 seconds


#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "hello.lisp" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Wed Dec 27 04:14:05 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
(defun main () (print (cons "hello" *ARGS*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp:0 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,main,[],[print,[cons,'$STRING'("hello"),'*ARGS*']]])
wl:lambda_def(defun, u_main, f_u_main, [], [[print, [cons, '$ARRAY'([*], claz_base_character, "hello"), ext_xx_args_xx]]]).
wl:arglist_info(u_main, f_u_main, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_main).

/*

### Compiled:  `U::MAIN` 
*/
f_u_main(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, ext_xx_args_xx, Ext_xx_args_xx_Get),
	Print_Param=['$ARRAY'([*], claz_base_character, "hello")|Ext_xx_args_xx_Get],
	cl_print(Print_Param, Print_Ret),
	Print_Ret=FnResult.
:- set_opv(f_u_main, classof, claz_function),
   set_opv(u_main, compile_as, kw_function),
   set_opv(u_main, function, f_u_main),
   DefunResult=u_main.
/*
:- side_effect(assert_lsp(u_main,
			  wl:lambda_def(defun, u_main, f_u_main, [], [[print, [cons, '$ARRAY'([*], claz_base_character, "hello"), ext_xx_args_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_main,
			  wl:arglist_info(u_main, f_u_main, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_main, wl:init_args(exact_only, f_u_main))).
*/
/*
(main)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp:48 **********************/
:-lisp_compile_to_prolog(pkg_user,[main])
:- f_u_main(_Ignored).


%; Total compilation time: 0.232 seconds


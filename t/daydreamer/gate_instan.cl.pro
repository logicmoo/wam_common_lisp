#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_instan" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:15:09 2017

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
 This file contains the instantiator for obs
*/
/*
*/
/*
 10/13/84: Original version written
*/
/*
  6/30/85: Added *modify*, *expand*
*/
/*
   1/6/86: Changed specials to obs
*/
/*
  1/29/86: Added omit-proc
*/
/*
  1/30/86: Added variables-in
*/
/*
  9/24/86: Got rid of flavors
*/
/*
  9/29/86: Updated to new instantiation algorithm with cycle preservation
*/
/*
*/
/*
*******************************************************************************
*/
/*
(setq *found-obs* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:607 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*found-obs*',[]])
:- set_var(AEnv, setq, u_xx_found_obs_xx, []).
/*
(setq *instan-obs* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:631 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*instan-obs*',[]])
:- set_var(AEnv, setq, u_xx_instan_obs_xx, []).
/*
(setq *any-unbound?* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:656 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*any-unbound?*',[]])
:- set_var(AEnv, setq, u_xx_any_unbound_c63_xx, []).
/*
(defun ob$instantiate (template bindings)
  (ob$instantiate1 template bindings 100 nil nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:683 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instantiate',[template,bindings],['ob$instantiate1',template,bindings,100,[],[]]])
wl:lambda_def(defun, u_ob_c36_instantiate, f_u_ob_c36_instantiate, [u_template, bindings], [[u_ob_c36_instantiate1, u_template, bindings, 100, [], []]]).
wl:arglist_info(u_ob_c36_instantiate, f_u_ob_c36_instantiate, [u_template, bindings], arginfo{all:[u_template, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings], opt:0, req:[u_template, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate).

/*

### Compiled:  `U::OB$INSTANTIATE` 
*/
f_u_ob_c36_instantiate(Template, Bindings, FnResult) :-
	nop(global_env(Env)),
	Env11=[bv(u_template, Template), bv(bindings, Bindings)|Env],
	get_var(Env11, bindings, Bindings_Get),
	get_var(Env11, u_template, Template_Get),
	f_u_ob_c36_instantiate1(Template_Get,
				Bindings_Get,
				100,
				[],
				[],
				C36_instantiate1_Ret),
	C36_instantiate1_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate, classof, claz_function),
   set_opv(u_ob_c36_instantiate, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate, function, f_u_ob_c36_instantiate),
   _Ignored4=u_ob_c36_instantiate.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate,
			  wl:lambda_def(defun, u_ob_c36_instantiate, f_u_ob_c36_instantiate, [u_template, bindings], [[u_ob_c36_instantiate1, u_template, bindings, 100, [], []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate,
			  wl:arglist_info(u_ob_c36_instantiate, f_u_ob_c36_instantiate, [u_template, bindings], arginfo{all:[u_template, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings], opt:0, req:[u_template, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate))).
*/
/*
(defun ob$instantiate1 (template bindings depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 template bindings depth
                    omit-slots include-slots nil nil nil))

;
; substit: a binding list of pairs. Each pair has the thing (ob or otherwise)
; to substitute and the thing to substitute it with.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:777 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instantiate1',[template,bindings,depth,'omit-slots','include-slots'],[setq,'*instan-obs*',[]],[setq,'*any-unbound?*',[]],['ob$instantiate2',template,bindings,depth,'omit-slots','include-slots',[],[],[]]])
wl:lambda_def(defun, u_ob_c36_instantiate1, f_u_ob_c36_instantiate1, [u_template, bindings, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_template, bindings, u_depth, u_omit_slots, u_include_slots, [], [], []]]).
wl:arglist_info(u_ob_c36_instantiate1, f_u_ob_c36_instantiate1, [u_template, bindings, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate1).

/*

### Compiled:  `U::OB$INSTANTIATE1` 
*/
f_u_ob_c36_instantiate1(Template, Bindings, Depth, Omit_slots, Include_slots, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_template, Template), bv(bindings, Bindings), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots), bv(u_include_slots, Include_slots)|Env],
	set_var(AEnv, setq, u_xx_instan_obs_xx, []),
	set_var(AEnv, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_template,
				bindings,
				u_depth,
				u_omit_slots,
				u_include_slots,
				[],
				[],
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate1, classof, claz_function),
   set_opv(u_ob_c36_instantiate1, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate1, function, f_u_ob_c36_instantiate1),
   _Ignored4=u_ob_c36_instantiate1.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate1,
			  wl:lambda_def(defun, u_ob_c36_instantiate1, f_u_ob_c36_instantiate1, [u_template, bindings, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_template, bindings, u_depth, u_omit_slots, u_include_slots, [], [], []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate1,
			  wl:arglist_info(u_ob_c36_instantiate1, f_u_ob_c36_instantiate1, [u_template, bindings, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate1,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate1))).
*/
/*
*/
/*
 substit: a binding list of pairs. Each pair has the thing (ob or otherwise)
*/
/*
 to substitute and the thing to substitute it with.
*/
/*
*/
/*
(defun ob$subst (ob substit depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob *empty-bd* depth
                    omit-slots include-slots substit nil nil))

;
; variabilize?: a predicate determining whether an ob should be abstracted
; and converted into a unique variable. Multiple occurences of the same ob
; will become the same variable.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1143 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$subst',[ob,substit,depth,'omit-slots','include-slots'],[setq,'*instan-obs*',[]],[setq,'*any-unbound?*',[]],['ob$instantiate2',ob,'*empty-bd*',depth,'omit-slots','include-slots',substit,[],[]]])
wl:lambda_def(defun, u_ob_c36_subst, f_u_ob_c36_subst, [u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_xx_empty_bd_xx, u_depth, u_omit_slots, u_include_slots, u_substit, [], []]]).
wl:arglist_info(u_ob_c36_subst, f_u_ob_c36_subst, [u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_subst).

/*

### Compiled:  `U::OB$SUBST` 
*/
f_u_ob_c36_subst(Ob, Substit, Depth, Omit_slots, Include_slots, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob), bv(u_substit, Substit), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots), bv(u_include_slots, Include_slots)|Env],
	set_var(AEnv, setq, u_xx_instan_obs_xx, []),
	set_var(AEnv, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_xx_empty_bd_xx,
				u_depth,
				u_omit_slots,
				u_include_slots,
				u_substit,
				[],
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_subst, classof, claz_function),
   set_opv(u_ob_c36_subst, compile_as, kw_function),
   set_opv(u_ob_c36_subst, function, f_u_ob_c36_subst),
   _Ignored4=u_ob_c36_subst.
/*
:- side_effect(assert_lsp(u_ob_c36_subst,
			  wl:lambda_def(defun, u_ob_c36_subst, f_u_ob_c36_subst, [u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_xx_empty_bd_xx, u_depth, u_omit_slots, u_include_slots, u_substit, [], []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_subst,
			  wl:arglist_info(u_ob_c36_subst, f_u_ob_c36_subst, [u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_ob, u_substit, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_subst,
			  wl:init_args(exact_only, f_u_ob_c36_subst))).
*/
/*
*/
/*
 variabilize?: a predicate determining whether an ob should be abstracted
*/
/*
 and converted into a unique variable. Multiple occurences of the same ob
*/
/*
 will become the same variable.
*/
/*
*/
/*
(defun ob$variabilize (ob variabilize? depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob *empty-bd* depth omit-slots
                    include-slots '(t) variabilize? nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1547 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$variabilize',[ob,'variabilize?',depth,'omit-slots','include-slots'],[setq,'*instan-obs*',[]],[setq,'*any-unbound?*',[]],['ob$instantiate2',ob,'*empty-bd*',depth,'omit-slots','include-slots',[quote,[t]],'variabilize?',[]]])
wl:lambda_def(defun, u_ob_c36_variabilize, f_u_ob_c36_variabilize, [u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_xx_empty_bd_xx, u_depth, u_omit_slots, u_include_slots, [quote, [t]], u_variabilize_c63, []]]).
wl:arglist_info(u_ob_c36_variabilize, f_u_ob_c36_variabilize, [u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_variabilize).

/*

### Compiled:  `U::OB$VARIABILIZE` 
*/
f_u_ob_c36_variabilize(Ob, Variabilize_c63, Depth, Omit_slots, Include_slots, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob), bv(u_variabilize_c63, Variabilize_c63), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots), bv(u_include_slots, Include_slots)|Env],
	set_var(AEnv, setq, u_xx_instan_obs_xx, []),
	set_var(AEnv, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_xx_empty_bd_xx,
				u_depth,
				u_omit_slots,
				u_include_slots,
				[quote, [t]],
				u_variabilize_c63,
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_variabilize, classof, claz_function),
   set_opv(u_ob_c36_variabilize, compile_as, kw_function),
   set_opv(u_ob_c36_variabilize, function, f_u_ob_c36_variabilize),
   _Ignored4=u_ob_c36_variabilize.
/*
:- side_effect(assert_lsp(u_ob_c36_variabilize,
			  wl:lambda_def(defun, u_ob_c36_variabilize, f_u_ob_c36_variabilize, [u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_xx_empty_bd_xx, u_depth, u_omit_slots, u_include_slots, [quote, [t]], u_variabilize_c63, []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_variabilize,
			  wl:arglist_info(u_ob_c36_variabilize, f_u_ob_c36_variabilize, [u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_variabilize,
			  wl:init_args(exact_only, f_u_ob_c36_variabilize))).
*/
/*
(defun ob$varize (ob variabilize?)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob *empty-bd* 100 nil
                    nil '(t) variabilize? nil))

;
; omit-proc: a predicate determining whether an ob should be returned
; as is, without instantiation.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1781 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$varize',[ob,'variabilize?'],[setq,'*instan-obs*',[]],[setq,'*any-unbound?*',[]],['ob$instantiate2',ob,'*empty-bd*',100,[],[],[quote,[t]],'variabilize?',[]]])
wl:lambda_def(defun, u_ob_c36_varize, f_u_ob_c36_varize, [u_ob, u_variabilize_c63], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_xx_empty_bd_xx, 100, [], [], [quote, [t]], u_variabilize_c63, []]]).
wl:arglist_info(u_ob_c36_varize, f_u_ob_c36_varize, [u_ob, u_variabilize_c63], arginfo{all:[u_ob, u_variabilize_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_variabilize_c63], opt:0, req:[u_ob, u_variabilize_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_varize).

/*

### Compiled:  `U::OB$VARIZE` 
*/
f_u_ob_c36_varize(Ob, Variabilize_c63, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob), bv(u_variabilize_c63, Variabilize_c63)|Env],
	set_var(AEnv, setq, u_xx_instan_obs_xx, []),
	set_var(AEnv, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_xx_empty_bd_xx,
				100,
				[],
				[],
				[quote, [t]],
				u_variabilize_c63,
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_varize, classof, claz_function),
   set_opv(u_ob_c36_varize, compile_as, kw_function),
   set_opv(u_ob_c36_varize, function, f_u_ob_c36_varize),
   _Ignored4=u_ob_c36_varize.
/*
:- side_effect(assert_lsp(u_ob_c36_varize,
			  wl:lambda_def(defun, u_ob_c36_varize, f_u_ob_c36_varize, [u_ob, u_variabilize_c63], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_xx_empty_bd_xx, 100, [], [], [quote, [t]], u_variabilize_c63, []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_varize,
			  wl:arglist_info(u_ob_c36_varize, f_u_ob_c36_varize, [u_ob, u_variabilize_c63], arginfo{all:[u_ob, u_variabilize_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_variabilize_c63], opt:0, req:[u_ob, u_variabilize_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_varize,
			  wl:init_args(exact_only, f_u_ob_c36_varize))).
*/
/*
*/
/*
 omit-proc: a predicate determining whether an ob should be returned
*/
/*
 as is, without instantiation.
*/
/*
*/
/*
(defun ob$instan-omit (ob bd omit-proc depth omit-slots include-slots)
  (setq *instan-obs* nil)
  (setq *any-unbound?* nil)
  (ob$instantiate2 ob bd depth omit-slots include-slots nil nil omit-proc))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2066 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instan-omit',[ob,bd,'omit-proc',depth,'omit-slots','include-slots'],[setq,'*instan-obs*',[]],[setq,'*any-unbound?*',[]],['ob$instantiate2',ob,bd,depth,'omit-slots','include-slots',[],[],'omit-proc']])
wl:lambda_def(defun, u_ob_c36_instan_omit, f_u_ob_c36_instan_omit, [u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_bd, u_depth, u_omit_slots, u_include_slots, [], [], u_omit_proc]]).
wl:arglist_info(u_ob_c36_instan_omit, f_u_ob_c36_instan_omit, [u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instan_omit).

/*

### Compiled:  `U::OB$INSTAN-OMIT` 
*/
f_u_ob_c36_instan_omit(Ob, Bd, Omit_proc, Depth, Omit_slots, Include_slots, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob), bv(u_bd, Bd), bv(u_omit_proc, Omit_proc), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots), bv(u_include_slots, Include_slots)|Env],
	set_var(AEnv, setq, u_xx_instan_obs_xx, []),
	set_var(AEnv, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_bd,
				u_depth,
				u_omit_slots,
				u_include_slots,
				[],
				[],
				u_omit_proc,
				Omit_proc16),
	Omit_proc16=FnResult.
:- set_opv(f_u_ob_c36_instan_omit, classof, claz_function),
   set_opv(u_ob_c36_instan_omit, compile_as, kw_function),
   set_opv(u_ob_c36_instan_omit, function, f_u_ob_c36_instan_omit),
   _Ignored4=u_ob_c36_instan_omit.
/*
:- side_effect(assert_lsp(u_ob_c36_instan_omit,
			  wl:lambda_def(defun, u_ob_c36_instan_omit, f_u_ob_c36_instan_omit, [u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], [[setq, u_xx_instan_obs_xx, []], [setq, u_xx_any_unbound_c63_xx, []], [u_ob_c36_instantiate2, u_ob, u_bd, u_depth, u_omit_slots, u_include_slots, [], [], u_omit_proc]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instan_omit,
			  wl:arglist_info(u_ob_c36_instan_omit, f_u_ob_c36_instan_omit, [u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], arginfo{all:[u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], opt:0, req:[u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instan_omit,
			  wl:init_args(exact_only, f_u_ob_c36_instan_omit))).
*/
/*
(setq *instantiate-omit-obs* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2268 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*instantiate-omit-obs*',[]])
:- set_var(AEnv, setq, u_xx_instantiate_omit_obs_xx, []).
/*
(defun ob$instantiate-dbg (template bindings depth
                             omit-slots include-slots substit abstract
                             omit-proc)
  (ndbg-begin)
  (ndbg *gate-dbg* instantiate "Call ob$instantiate3: "(defun ob$instantiate-dbg (template bindings depth\n                             omit-slots include-slots substit abstract\n                             omit-proc)\n  (ndbg-begin)\n  (ndbg *gate-dbg* instantiate \"Call ob$instantiate3: ~A ~A~%\"\n                                 template bindings)\n  (let ((result (ob$instantiate3 template bindings depth\n                                  omit-slots include-slots substit abstract\n                                  omit-proc)))\n    (ndbg *gate-dbg* instantiate \"Return from ob$instantiate3: ~A~%\" result)\n    (ndbg-end)\n    result))\n\n;\n; This should never be called from the top-level, at least without not\n; first doing (setq *instan-obs* nil) and (setq *any-unbound?* nil).\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2303 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instantiate-dbg',[template,bindings,depth,'omit-slots','include-slots',substit,abstract,'omit-proc'],['ndbg-begin'],[ndbg,'*gate-dbg*',instantiate,'$STRING'("Call ob$instantiate3: ~A ~A~%"),template,bindings],[let,[[result,['ob$instantiate3',template,bindings,depth,'omit-slots','include-slots',substit,abstract,'omit-proc']]],[ndbg,'*gate-dbg*',instantiate,'$STRING'("Return from ob$instantiate3: ~A~%"),result],['ndbg-end'],result]])
wl:lambda_def(defun, u_ob_c36_instantiate_dbg, f_u_ob_c36_instantiate_dbg, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], [[u_ndbg_begin], [u_ndbg, u_xx_gate_dbg_xx, u_instantiate, '$ARRAY'([*], claz_base_character, "Call ob$instantiate3: ~A ~A~%"), u_template, bindings], [let, [[u_result, [u_ob_c36_instantiate3, u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]]], [u_ndbg, u_xx_gate_dbg_xx, u_instantiate, '$ARRAY'([*], claz_base_character, "Return from ob$instantiate3: ~A~%"), u_result], [u_ndbg_end], u_result]]).
wl:arglist_info(u_ob_c36_instantiate_dbg, f_u_ob_c36_instantiate_dbg, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate_dbg).

/*

### Compiled:  `U::OB$INSTANTIATE-DBG` 
*/
f_u_ob_c36_instantiate_dbg(Template, Bindings, Depth, Omit_slots, Include_slots, Substit, Abstract, Omit_proc, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_template, Template), bv(bindings, Bindings), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots), bv(u_include_slots, Include_slots), bv(u_substit, Substit), bv(u_abstract, Abstract), bv(u_omit_proc, Omit_proc)|Env],
	f_u_ndbg_begin(Ndbg_begin_Ret),
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_instantiate,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    "Call ob$instantiate3: ~A ~A~%"),
		   u_template,
		   bindings
		 ],
		 Ndbg_Ret),
	get_var(Env22, bindings, Bindings_Get),
	( get_var(Env22, u_abstract, Abstract_Get),
	  get_var(Env22, u_depth, Depth_Get)
	),
	get_var(Env22, u_include_slots, Include_slots_Get),
	get_var(Env22, u_omit_proc, Omit_proc_Get),
	( get_var(Env22, u_omit_slots, Omit_slots_Get),
	  get_var(Env22, u_template, Template_Get)
	),
	get_var(Env22, u_substit, Substit_Get),
	f_u_ob_c36_instantiate3(Template_Get,
				Bindings_Get,
				Depth_Get,
				Omit_slots_Get,
				Include_slots_Get,
				Substit_Get,
				Abstract_Get,
				Omit_proc_Get,
				Result_Init),
	LEnv=[bv(u_result, Result_Init)|Env22],
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_instantiate,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    "Return from ob$instantiate3: ~A~%"),
		   u_result
		 ],
		 Ndbg_Ret33),
	f_u_ndbg_end(Ndbg_end_Ret),
	get_var(LEnv, u_result, Result_Get),
	Result_Get=FnResult.
:- set_opv(f_u_ob_c36_instantiate_dbg, classof, claz_function),
   set_opv(u_ob_c36_instantiate_dbg, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate_dbg, function, f_u_ob_c36_instantiate_dbg),
   _Ignored4=u_ob_c36_instantiate_dbg.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate_dbg,
			  wl:lambda_def(defun, u_ob_c36_instantiate_dbg, f_u_ob_c36_instantiate_dbg, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], [[u_ndbg_begin], [u_ndbg, u_xx_gate_dbg_xx, u_instantiate, '$ARRAY'([*], claz_base_character, "Call ob$instantiate3: ~A ~A~%"), u_template, bindings], [let, [[u_result, [u_ob_c36_instantiate3, u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]]], [u_ndbg, u_xx_gate_dbg_xx, u_instantiate, '$ARRAY'([*], claz_base_character, "Return from ob$instantiate3: ~A~%"), u_result], [u_ndbg_end], u_result]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate_dbg,
			  wl:arglist_info(u_ob_c36_instantiate_dbg, f_u_ob_c36_instantiate_dbg, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate_dbg,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate_dbg))).
*/
/*
*/
/*
 This should never be called from the top-level, at least without not
*/
/*
 first doing (setq *instan-obs* nil) and (setq *any-unbound?* nil).
*/
/*
*/
/*
(defun ob$instantiate3 (template bindings depth
                         omit-slots include-slots substit abstract
                         omit-proc)
  (cond
   ((let ((found (assq template *instan-obs*)))
         (if found
             (cdr found)
             nil)))
   ((and depth (< depth 0))
    template)
   ((and omit-proc (funcall omit-proc template)) template)
   ((not (ob? template)) template)
   ((var? template)
    (let ((found (bd-hyper-lookup (variable-name template) bindings)))
      (if found
          (cond
           ((var? found)
            (setq *any-unbound?* t)
;            (ndbg *gate-dbg* ob-warn "(?"(defun ob$instantiate3 (template bindings depth\n                         omit-slots include-slots substit abstract\n                         omit-proc)\n  (cond\n   ((let ((found (assq template *instan-obs*)))\n         (if found\n             (cdr found)\n             nil)))\n   ((and depth (< depth 0))\n    template)\n   ((and omit-proc (funcall omit-proc template)) template)\n   ((not (ob? template)) template)\n   ((var? template)\n    (let ((found (bd-hyper-lookup (variable-name template) bindings)))\n      (if found\n          (cond\n           ((var? found)\n            (setq *any-unbound?* t)\n;            (ndbg *gate-dbg* ob-warn \"(?~A binding cycle)~%\"\n;                  (variable-name found))\n            found)\n           ((and (ob? found) (vars-in? found))\n            (ob$instantiate2 found bindings (if depth (-1+ depth) nil)\n                             omit-slots\n                             include-slots substit abstract omit-proc))\n           (else found))\n          (progn\n           (setq *any-unbound?* t)\n;           (ndbg *gate-dbg* ob-warn \"(?~A unbound)~%\"\n;                                      (variable-name template))\n           template))))\n   ((special? template)\n    (ob$instan-special template bindings (if depth (-1+ depth) nil)\n                           omit-slots include-slots\n                       substit abstract omit-proc))\n   (else ; (ob? template)\n    (let ((result-ob (ob$create-empty)))\n      (setq *instan-obs* (cons (cons template result-ob) *instan-obs*))\n      (yloop\n       (initial (rest (ob$pairs template))\n                (substitution nil))\n       (ywhile rest)\n       (ydo (if (and (not (memq? (slots-name (car rest)) omit-slots))\n                     (not (memq? (slots-name (car rest))\n                          *permanent-ignore-slots*))\n                     (not (null? (slots-value (car rest)))) ; todo\n                     (if include-slots\n                         (memq? (slots-name (car rest)) include-slots)\n                         t))\n               (progn\n                (setq substitution (bd-lookup (slots-value (car rest)) substit))\n                (ob$add result-ob (slots-name (car rest))\n                     (cond\n                      (substitution substitution)\n                      ((and abstract\n                            (funcall abstract (slots-value (car rest))))\n                       (let ((uniqvar\n                              (make-var (gen-id \"var\")\n                                        (ty$get-major-type\n                                         (ob$ty (slots-value (car rest)))))))\n                         (setq substit (bd-bind! (slots-value (car rest))\n                                                uniqvar substit))\n                         uniqvar))\n                      (else\n                       (if (or (memq? (slots-value (car rest))\n                                      *instantiate-omit-obs*)\n                               (and (ob? (slots-value (car rest)))\n                                    (ob$literal? (slots-value (car rest)))))\n                           (slots-value (car rest))\n                           (ob$instantiate2 (slots-value (car rest))\n                                            bindings (if depth (-1+ depth)\n                                                         nil)\n                                            omit-slots include-slots substit\n                                            abstract omit-proc)))))))\n           (setq rest (cdr rest)))\n       (yresult result-ob))))))\n\n\n#|\nMoved to gate_instan2.cl\n\n(defun ob$instan-special \n (template bindings depth omit-slots include-slots substit abstract omit-proc) \n .. )\n|#\n\n;\n;\n; ob$instantiate!:\n;\n; This version of instantiate does not copy anything.\n; It simply replaces all bound variables with their values.\n;\n                                                   \n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instantiate3',[template,bindings,depth,'omit-slots','include-slots',substit,abstract,'omit-proc'],[cond,[[let,[[found,[assq,template,'*instan-obs*']]],[if,found,[cdr,found],[]]]],[[and,depth,[<,depth,0]],template],[[and,'omit-proc',[funcall,'omit-proc',template]],template],[[not,['ob?',template]],template],[['var?',template],[let,[[found,['bd-hyper-lookup',['variable-name',template],bindings]]],[if,found,[cond,[['var?',found],[setq,'*any-unbound?*',t],found],[[and,['ob?',found],['vars-in?',found]],['ob$instantiate2',found,bindings,[if,depth,['-1+',depth],[]],'omit-slots','include-slots',substit,abstract,'omit-proc']],[else,found]],[progn,[setq,'*any-unbound?*',t],template]]]],[['special?',template],['ob$instan-special',template,bindings,[if,depth,['-1+',depth],[]],'omit-slots','include-slots',substit,abstract,'omit-proc']],[else,[let,[['result-ob',['ob$create-empty']]],[setq,'*instan-obs*',[cons,[cons,template,'result-ob'],'*instan-obs*']],[yloop,[initial,[rest,['ob$pairs',template]],[substitution,[]]],[ywhile,rest],[ydo,[if,[and,[not,['memq?',['slots-name',[car,rest]],'omit-slots']],[not,['memq?',['slots-name',[car,rest]],'*permanent-ignore-slots*']],[not,['null?',['slots-value',[car,rest]]]],[if,'include-slots',['memq?',['slots-name',[car,rest]],'include-slots'],t]],[progn,[setq,substitution,['bd-lookup',['slots-value',[car,rest]],substit]],['ob$add','result-ob',['slots-name',[car,rest]],[cond,[substitution,substitution],[[and,abstract,[funcall,abstract,['slots-value',[car,rest]]]],[let,[[uniqvar,['make-var',['gen-id','$STRING'("var")],['ty$get-major-type',['ob$ty',['slots-value',[car,rest]]]]]]],[setq,substit,['bd-bind!',['slots-value',[car,rest]],uniqvar,substit]],uniqvar]],[else,[if,[or,['memq?',['slots-value',[car,rest]],'*instantiate-omit-obs*'],[and,['ob?',['slots-value',[car,rest]]],['ob$literal?',['slots-value',[car,rest]]]]],['slots-value',[car,rest]],['ob$instantiate2',['slots-value',[car,rest]],bindings,[if,depth,['-1+',depth],[]],'omit-slots','include-slots',substit,abstract,'omit-proc']]]]]]],[setq,rest,[cdr,rest]]],[yresult,'result-ob']]]]]])
wl:lambda_def(defun, u_ob_c36_instantiate3, f_u_ob_c36_instantiate3, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], [[cond, [[let, [[u_found, [ext_assq, u_template, u_xx_instan_obs_xx]]], [if, u_found, [cdr, u_found], []]]], [[and, u_depth, [<, u_depth, 0]], u_template], [[and, u_omit_proc, [funcall, u_omit_proc, u_template]], u_template], [[not, [u_ob_c63, u_template]], u_template], [[u_var_c63, u_template], [let, [[u_found, [u_bd_hyper_lookup, [u_variable_name, u_template], bindings]]], [if, u_found, [cond, [[u_var_c63, u_found], [setq, u_xx_any_unbound_c63_xx, t], u_found], [[and, [u_ob_c63, u_found], [u_vars_in_c63, u_found]], [u_ob_c36_instantiate2, u_found, bindings, [if, u_depth, ['-1+', u_depth], []], u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]], [u_else, u_found]], [progn, [setq, u_xx_any_unbound_c63_xx, t], u_template]]]], [[u_special_c63, u_template], [u_ob_c36_instan_special, u_template, bindings, [if, u_depth, ['-1+', u_depth], []], u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]], [u_else, [let, [[u_result_ob, [u_ob_c36_create_empty]]], [setq, u_xx_instan_obs_xx, [cons, [cons, u_template, u_result_ob], u_xx_instan_obs_xx]], [u_yloop, [u_initial, [rest, [u_ob_c36_pairs, u_template]], [u_substitution, []]], [u_ywhile, rest], [u_ydo, [if, [and, [not, [u_memq_c63, [u_slots_name, [car, rest]], u_omit_slots]], [not, [u_memq_c63, [u_slots_name, [car, rest]], u_xx_permanent_ignore_slots_xx]], [not, [u_null_c63, [u_slots_value, [car, rest]]]], [if, u_include_slots, [u_memq_c63, [u_slots_name, [car, rest]], u_include_slots], t]], [progn, [setq, u_substitution, [u_bd_lookup, [u_slots_value, [car, rest]], u_substit]], [u_ob_c36_add, u_result_ob, [u_slots_name, [car, rest]], [cond, [u_substitution, u_substitution], [[and, u_abstract, [funcall, u_abstract, [u_slots_value, [car, rest]]]], [let, [[u_uniqvar, [u_make_var, [u_gen_id, '$ARRAY'([*], claz_base_character, "var")], [u_ty_c36_get_major_type, [u_ob_c36_ty, [u_slots_value, [car, rest]]]]]]], [setq, u_substit, [u_bd_bind_c33, [u_slots_value, [car, rest]], u_uniqvar, u_substit]], u_uniqvar]], [u_else, [if, [or, [u_memq_c63, [u_slots_value, [car, rest]], u_xx_instantiate_omit_obs_xx], [and, [u_ob_c63, [u_slots_value, [car, rest]]], [u_ob_c36_literal_c63, [u_slots_value, [car, rest]]]]], [u_slots_value, [car, rest]], [u_ob_c36_instantiate2, [u_slots_value, [car, rest]], bindings, [if, u_depth, ['-1+', u_depth], []], u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]]]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result_ob]]]]]]).
wl:arglist_info(u_ob_c36_instantiate3, f_u_ob_c36_instantiate3, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate3).

/*

### Compiled:  `U::OB$INSTANTIATE3` 
*/
f_u_ob_c36_instantiate3(Template, Bindings, Depth, Omit_slots, Include_slots, Substit, Abstract, Omit_proc, LetResult42) :-
	nop(global_env(Env)),
	Env111=[bv(u_template, Template), bv(bindings, Bindings), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots), bv(u_include_slots, Include_slots), bv(u_substit, Substit), bv(u_abstract, Abstract), bv(u_omit_proc, Omit_proc)|Env],
	f_ext_assq(u_template, u_xx_instan_obs_xx, Found_Init),
	LEnv=[bv(u_found, Found_Init)|Env111],
	get_var(LEnv, u_found, IFTEST13),
	(   IFTEST13\==[]
	->  get_var(LEnv, u_found, Found_Get16),
	    cl_cdr(Found_Get16, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  LetResult42=[]
	;   get_var(Env111, u_depth, IFTEST20),
	    (   IFTEST20\==[]
	    ->  get_var(Env111, u_depth, Depth_Get23),
		<(Depth_Get23, 0, TrueResult24),
		IFTEST18=TrueResult24
	    ;   IFTEST18=[]
	    ),
	    (   IFTEST18\==[]
	    ->  get_var(Env111, u_template, Template_Get),
		LetResult42=Template_Get
	    ;   get_var(Env111, u_omit_proc, IFTEST28),
		(   IFTEST28\==[]
		->  get_var(Env111, u_omit_proc, Omit_proc_Get31),
		    get_var(Env111, u_template, Template_Get32),
		    cl_apply(Omit_proc_Get31, [Template_Get32], TrueResult33),
		    IFTEST26=TrueResult33
		;   IFTEST26=[]
		),
		(   IFTEST26\==[]
		->  get_var(Env111, u_template, Template_Get34),
		    LetResult42=Template_Get34
		;   f_u_ob_c63(u_template, PredArgResult),
		    (   PredArgResult==[]
		    ->  get_var(Env111, u_template, Template_Get38),
			LetResult42=Template_Get38
		    ;   f_u_var_c63(u_template, IFTEST39),
			(   IFTEST39\==[]
			->  f_u_bd_hyper_lookup([u_variable_name, u_template],
						bindings,
						Found_Init44),
			    LEnv43=[bv(u_found, Found_Init44)|Env111],
			    get_var(LEnv43, u_found, IFTEST45),
			    (   IFTEST45\==[]
			    ->  f_u_var_c63(u_found, IFTEST48),
				(   IFTEST48\==[]
				->  set_var(LEnv43,
					    setq,
					    u_xx_any_unbound_c63_xx,
					    t),
				    get_var(LEnv43, u_found, Found_Get51),
				    LetResult42=Found_Get51
				;   f_u_ob_c63(u_found, IFTEST54),
				    (   IFTEST54\==[]
				    ->  get_var(LEnv43, u_found, Found_Get56),
					f_u_vars_in_c63(Found_Get56,
							TrueResult57),
					IFTEST52=TrueResult57
				    ;   IFTEST52=[]
				    ),
				    (   IFTEST52\==[]
				    ->  f_u_ob_c36_instantiate2(u_found,
								bindings,
								
								[ if,
								  u_depth,
								  ['-1+', u_depth],
								  []
								],
								u_omit_slots,
								u_include_slots,
								u_substit,
								u_abstract,
								u_omit_proc,
								TrueResult64),
					LetResult42=TrueResult64
				    ;   get_var(LEnv43, u_else, IFTEST58),
					(   IFTEST58\==[]
					->  get_var(LEnv43,
						    u_found,
						    Found_Get61),
					    LetResult42=Found_Get61
					;   LetResult42=[]
					)
				    )
				)
			    ;   set_var(LEnv43, setq, u_xx_any_unbound_c63_xx, t),
				get_var(LEnv43, u_template, Template_Get68),
				LetResult42=Template_Get68
			    )
			;   f_u_special_c63(u_template, IFTEST71),
			    (   IFTEST71\==[]
			    ->  get_var(Env111, bindings, Bindings_Get),
				get_var(Env111, u_depth, IFTEST75),
				get_var(Env111, u_template, Template_Get73),
				(   IFTEST75\==[]
				->  get_var(Env111, u_depth, Depth_Get78),
				    f_u_1_c43(Depth_Get78, TrueResult79),
				    _292951136=TrueResult79
				;   _292951136=[]
				),
				get_var(Env111, u_abstract, Abstract_Get),
				get_var(Env111,
					u_include_slots,
					Include_slots_Get),
				get_var(Env111, u_omit_proc, Omit_proc_Get84),
				get_var(Env111, u_omit_slots, Omit_slots_Get),
				get_var(Env111, u_substit, Substit_Get),
				f_u_ob_c36_instan_special(Template_Get73,
							  Bindings_Get,
							  _292951136,
							  Omit_slots_Get,
							  Include_slots_Get,
							  Substit_Get,
							  Abstract_Get,
							  Omit_proc_Get84,
							  TrueResult98),
				LetResult42=TrueResult98
			    ;   get_var(Env111, u_else, IFTEST85),
				(   IFTEST85\==[]
				->  f_u_ob_c36_create_empty(Result_ob_Init),
				    LEnv90=[bv(u_result_ob, Result_ob_Init)|Env111],
				    get_var(LEnv90, u_result_ob, Result_ob_Get),
				    get_var(LEnv90, u_template, Template_Get93),
				    CAR=[Template_Get93|Result_ob_Get],
				    get_var(LEnv90,
					    u_xx_instan_obs_xx,
					    Xx_instan_obs_xx_Get),
				    Xx_instan_obs_xx=[CAR|Xx_instan_obs_xx_Get],
				    set_var(LEnv90,
					    u_xx_instan_obs_xx,
					    Xx_instan_obs_xx),
				    f_u_yloop(
					      [ 
						[ u_initial,
						  
						  [ rest,
						    
						    [ u_ob_c36_pairs,
						      u_template
						    ]
						  ],
						  [u_substitution, []]
						],
						[u_ywhile, rest],
						
						[ u_ydo,
						  
						  [ if,
						    
						    [ and,
						      
						      [ not,
							
							[ u_memq_c63,
							  
							  [ u_slots_name,
							    [car, rest]
							  ],
							  u_omit_slots
							]
						      ],
						      
						      [ not,
							
							[ u_memq_c63,
							  
							  [ u_slots_name,
							    [car, rest]
							  ],
							  u_xx_permanent_ignore_slots_xx
							]
						      ],
						      
						      [ not,
							
							[ u_null_c63,
							  
							  [ u_slots_value,
							    [car, rest]
							  ]
							]
						      ],
						      
						      [ if,
							u_include_slots,
							
							[ u_memq_c63,
							  
							  [ u_slots_name,
							    [car, rest]
							  ],
							  u_include_slots
							],
							t
						      ]
						    ],
						    
						    [ progn,
						      
						      [ setq,
							u_substitution,
							
							[ u_bd_lookup,
							  
							  [ u_slots_value,
							    [car, rest]
							  ],
							  u_substit
							]
						      ],
						      
						      [ u_ob_c36_add,
							u_result_ob,
							
							[ u_slots_name,
							  [car, rest]
							],
							
							[ cond,
							  
							  [ u_substitution,
							    u_substitution
							  ],
							  
							  [ 
							    [ and,
							      u_abstract,
							      
							      [ funcall,
								u_abstract,
								
								[ u_slots_value,
								  [car, rest]
								]
							      ]
							    ],
							    
							    [ let,
							      
							      [ 
								[ u_uniqvar,
								  
								  [ u_make_var,
								    
								    [ u_gen_id,
								      '$ARRAY'([*],
									       claz_base_character,
									       "var")
								    ],
								    
								    [ u_ty_c36_get_major_type,
								      
								      [ u_ob_c36_ty,
									
									[ u_slots_value,
									  [car, rest]
									]
								      ]
								    ]
								  ]
								]
							      ],
							      
							      [ setq,
								u_substit,
								
								[ u_bd_bind_c33,
								  
								  [ u_slots_value,
								    [car, rest]
								  ],
								  u_uniqvar,
								  u_substit
								]
							      ],
							      u_uniqvar
							    ]
							  ],
							  
							  [ u_else,
							    
							    [ if,
							      
							      [ or,
								
								[ u_memq_c63,
								  
								  [ u_slots_value,
								    [car, rest]
								  ],
								  u_xx_instantiate_omit_obs_xx
								],
								
								[ and,
								  
								  [ u_ob_c63,
								    
								    [ u_slots_value,
								      [car, rest]
								    ]
								  ],
								  
								  [ u_ob_c36_literal_c63,
								    
								    [ u_slots_value,
								      [car, rest]
								    ]
								  ]
								]
							      ],
							      
							      [ u_slots_value,
								[car, rest]
							      ],
							      
							      [ u_ob_c36_instantiate2,
								
								[ u_slots_value,
								  [car, rest]
								],
								bindings,
								
								[ if,
								  u_depth,
								  ['-1+', u_depth],
								  []
								],
								u_omit_slots,
								u_include_slots,
								u_substit,
								u_abstract,
								u_omit_proc
							      ]
							    ]
							  ]
							]
						      ]
						    ]
						  ],
						  [setq, rest, [cdr, rest]]
						],
						[u_yresult, u_result_ob]
					      ],
					      LetResult89),
				    LetResult42=LetResult89
				;   LetResult42=[]
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_instantiate3, classof, claz_function),
   set_opv(u_ob_c36_instantiate3, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate3, function, f_u_ob_c36_instantiate3),
   _Ignored4=u_ob_c36_instantiate3.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate3,
			  wl:lambda_def(defun, u_ob_c36_instantiate3, f_u_ob_c36_instantiate3, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], [[cond, [[let, [[u_found, [ext_assq, u_template, u_xx_instan_obs_xx]]], [if, u_found, [cdr, u_found], []]]], [[and, u_depth, [<, u_depth, 0]], u_template], [[and, u_omit_proc, [funcall, u_omit_proc, u_template]], u_template], [[not, [u_ob_c63, u_template]], u_template], [[u_var_c63, u_template], [let, [[u_found, [u_bd_hyper_lookup, [u_variable_name, u_template], bindings]]], [if, u_found, [cond, [[u_var_c63, u_found], [setq, u_xx_any_unbound_c63_xx, t], u_found], [[and, [u_ob_c63, u_found], [u_vars_in_c63, u_found]], [u_ob_c36_instantiate2, u_found, bindings, [if, u_depth, ['-1+', u_depth], []], u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]], [u_else, u_found]], [progn, [setq, u_xx_any_unbound_c63_xx, t], u_template]]]], [[u_special_c63, u_template], [u_ob_c36_instan_special, u_template, bindings, [if, u_depth, ['-1+', u_depth], []], u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]], [u_else, [let, [[u_result_ob, [u_ob_c36_create_empty]]], [setq, u_xx_instan_obs_xx, [cons, [cons, u_template, u_result_ob], u_xx_instan_obs_xx]], [u_yloop, [u_initial, [rest, [u_ob_c36_pairs, u_template]], [u_substitution, []]], [u_ywhile, rest], [u_ydo, [if, [and, [not, [u_memq_c63, [u_slots_name, [car, rest]], u_omit_slots]], [not, [u_memq_c63, [u_slots_name, [car, rest]], u_xx_permanent_ignore_slots_xx]], [not, [u_null_c63, [u_slots_value, [car, rest]]]], [if, u_include_slots, [u_memq_c63, [u_slots_name, [car, rest]], u_include_slots], t]], [progn, [setq, u_substitution, [u_bd_lookup, [u_slots_value, [car, rest]], u_substit]], [u_ob_c36_add, u_result_ob, [u_slots_name, [car, rest]], [cond, [u_substitution, u_substitution], [[and, u_abstract, [funcall, u_abstract, [u_slots_value, [car, rest]]]], [let, [[u_uniqvar, [u_make_var, [u_gen_id, '$ARRAY'([*], claz_base_character, "var")], [u_ty_c36_get_major_type, [u_ob_c36_ty, [u_slots_value, [car, rest]]]]]]], [setq, u_substit, [u_bd_bind_c33, [u_slots_value, [car, rest]], u_uniqvar, u_substit]], u_uniqvar]], [u_else, [if, [or, [u_memq_c63, [u_slots_value, [car, rest]], u_xx_instantiate_omit_obs_xx], [and, [u_ob_c63, [u_slots_value, [car, rest]]], [u_ob_c36_literal_c63, [u_slots_value, [car, rest]]]]], [u_slots_value, [car, rest]], [u_ob_c36_instantiate2, [u_slots_value, [car, rest]], bindings, [if, u_depth, ['-1+', u_depth], []], u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc]]]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result_ob]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate3,
			  wl:arglist_info(u_ob_c36_instantiate3, f_u_ob_c36_instantiate3, [u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], arginfo{all:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], opt:0, req:[u_template, bindings, u_depth, u_omit_slots, u_include_slots, u_substit, u_abstract, u_omit_proc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate3,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate3))).
*/
/*
            (ndbg *gate-dbg* ob-warn "(?"            (ndbg *gate-dbg* ob-warn \"(?~A binding cycle)~%\"".
*/
/*
                  (variable-name found))
*/
/*
           (ndbg *gate-dbg* ob-warn "(?"           (ndbg *gate-dbg* ob-warn \"(?~A unbound)~%\"".
*/
/*
                                      (variable-name template))
*/
/*
 (ob? template)
*/
/*
 todo
*/
/*
*/
/*
*/
/*
 ob$instantiate!:
*/
/*
*/
/*
 This version of instantiate does not copy anything.
*/
/*
 It simply replaces all bound variables with their values.
*/
/*
*/
/*

Moved to gate_instan2.cl

(defun ob$instan-special 
 (template bindings depth omit-slots include-slots substit abstract omit-proc) 
 .. )
*/
/*
(defun ob$instantiate! (template bindings)
  (ob$instantiate1! template bindings nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:6887 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instantiate!',[template,bindings],['ob$instantiate1!',template,bindings,[]]])
wl:lambda_def(defun, u_ob_c36_instantiate_c33, f_u_ob_c36_instantiate_c33, [u_template, bindings], [[u_ob_c36_instantiate1_c33, u_template, bindings, []]]).
wl:arglist_info(u_ob_c36_instantiate_c33, f_u_ob_c36_instantiate_c33, [u_template, bindings], arginfo{all:[u_template, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings], opt:0, req:[u_template, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate_c33).

/*

### Compiled:  `U::OB$INSTANTIATE!` 
*/
f_u_ob_c36_instantiate_c33(Template, Bindings, FnResult) :-
	nop(global_env(Env)),
	Env11=[bv(u_template, Template), bv(bindings, Bindings)|Env],
	get_var(Env11, bindings, Bindings_Get),
	get_var(Env11, u_template, Template_Get),
	f_u_ob_c36_instantiate1_c33(Template_Get,
				    Bindings_Get,
				    [],
				    Instantiate1_c33_Ret),
	Instantiate1_c33_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate_c33, classof, claz_function),
   set_opv(u_ob_c36_instantiate_c33, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate_c33, function, f_u_ob_c36_instantiate_c33),
   _Ignored4=u_ob_c36_instantiate_c33.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate_c33,
			  wl:lambda_def(defun, u_ob_c36_instantiate_c33, f_u_ob_c36_instantiate_c33, [u_template, bindings], [[u_ob_c36_instantiate1_c33, u_template, bindings, []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate_c33,
			  wl:arglist_info(u_ob_c36_instantiate_c33, f_u_ob_c36_instantiate_c33, [u_template, bindings], arginfo{all:[u_template, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings], opt:0, req:[u_template, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate_c33,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate_c33))).
*/
/*
(defun ob$instantiate1! (template bindings depth)
  (ob$instantiate2! template bindings depth nil nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:6975 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instantiate1!',[template,bindings,depth],['ob$instantiate2!',template,bindings,depth,[],[]]])
wl:lambda_def(defun, u_ob_c36_instantiate1_c33, f_u_ob_c36_instantiate1_c33, [u_template, bindings, u_depth], [[u_ob_c36_instantiate2_c33, u_template, bindings, u_depth, [], []]]).
wl:arglist_info(u_ob_c36_instantiate1_c33, f_u_ob_c36_instantiate1_c33, [u_template, bindings, u_depth], arginfo{all:[u_template, bindings, u_depth], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth], opt:0, req:[u_template, bindings, u_depth], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate1_c33).

/*

### Compiled:  `U::OB$INSTANTIATE1!` 
*/
f_u_ob_c36_instantiate1_c33(Template, Bindings, Depth, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_template, Template), bv(bindings, Bindings), bv(u_depth, Depth)|Env],
	get_var(Env12, bindings, Bindings_Get),
	get_var(Env12, u_depth, Depth_Get),
	get_var(Env12, u_template, Template_Get),
	f_u_ob_c36_instantiate2_c33(Template_Get,
				    Bindings_Get,
				    Depth_Get,
				    [],
				    [],
				    Instantiate2_c33_Ret),
	Instantiate2_c33_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate1_c33, classof, claz_function),
   set_opv(u_ob_c36_instantiate1_c33, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate1_c33, function, f_u_ob_c36_instantiate1_c33),
   _Ignored4=u_ob_c36_instantiate1_c33.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate1_c33,
			  wl:lambda_def(defun, u_ob_c36_instantiate1_c33, f_u_ob_c36_instantiate1_c33, [u_template, bindings, u_depth], [[u_ob_c36_instantiate2_c33, u_template, bindings, u_depth, [], []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate1_c33,
			  wl:arglist_info(u_ob_c36_instantiate1_c33, f_u_ob_c36_instantiate1_c33, [u_template, bindings, u_depth], arginfo{all:[u_template, bindings, u_depth], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth], opt:0, req:[u_template, bindings, u_depth], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate1_c33,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate1_c33))).
*/
/*
(defun ob$instantiate2! (template bindings depth ob slot-name)
  (cond
   ((var? template)
    (let ((found (assq (variable-name template) (cdr bindings))))
      (if found
          (progn
           (ob$remove ob slot-name template)
           (ob$add ob slot-name (cadr found))
           (cadr found))
          (progn
           (ndbg *gate-dbg* ob-warn
             "Warning: No binding for "(defun ob$instantiate2! (template bindings depth ob slot-name)\n  (cond\n   ((var? template)\n    (let ((found (assq (variable-name template) (cdr bindings))))\n      (if found\n          (progn\n           (ob$remove ob slot-name template)\n           (ob$add ob slot-name (cadr found))\n           (cadr found))\n          (progn\n           (ndbg *gate-dbg* ob-warn\n             \"Warning: No binding for ~A in instantiate.~%\"\n             template)\n           template))))\n   ((ob? template)\n    (yloop\n     (initial (rest (ob$pairs template)))\n     (ywhile rest)\n     (ydo (if (and (ob? (slots-value (car rest)))\n                  (ob$literal? (slots-value (car rest))))\n             (slots-value (car rest))\n             (if (number? depth)\n                 (if (> depth 1)\n                     (ob$instantiate2! (slots-value (car rest))\n                                        bindings\n                                        (-1+ depth)\n                                        template\n                                        (slots-name (car rest)))\n                     (slots-value (car rest)))\n                 (ob$instantiate2! (slots-value (car rest))\n                                   bindings\n                                   nil\n                                   template\n                                   (slots-name (car rest)))))\n         (setq rest (cdr rest)))\n     (yresult template)))\n   (else template)))\n\n;\n; Copies an ob down to the given depth. Does NOT replace variables\n; with their values the way ob-instantiate does.\n;\n; (coding assistance from Sergio Alvarado)\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$instantiate2!',[template,bindings,depth,ob,'slot-name'],[cond,[['var?',template],[let,[[found,[assq,['variable-name',template],[cdr,bindings]]]],[if,found,[progn,['ob$remove',ob,'slot-name',template],['ob$add',ob,'slot-name',[cadr,found]],[cadr,found]],[progn,[ndbg,'*gate-dbg*','ob-warn','$STRING'("Warning: No binding for ~A in instantiate.~%"),template],template]]]],[['ob?',template],[yloop,[initial,[rest,['ob$pairs',template]]],[ywhile,rest],[ydo,[if,[and,['ob?',['slots-value',[car,rest]]],['ob$literal?',['slots-value',[car,rest]]]],['slots-value',[car,rest]],[if,['number?',depth],[if,[>,depth,1],['ob$instantiate2!',['slots-value',[car,rest]],bindings,['-1+',depth],template,['slots-name',[car,rest]]],['slots-value',[car,rest]]],['ob$instantiate2!',['slots-value',[car,rest]],bindings,[],template,['slots-name',[car,rest]]]]],[setq,rest,[cdr,rest]]],[yresult,template]]],[else,template]]])
wl:lambda_def(defun, u_ob_c36_instantiate2_c33, f_u_ob_c36_instantiate2_c33, [u_template, bindings, u_depth, u_ob, u_slot_name], [[cond, [[u_var_c63, u_template], [let, [[u_found, [ext_assq, [u_variable_name, u_template], [cdr, bindings]]]], [if, u_found, [progn, [u_ob_c36_remove, u_ob, u_slot_name, u_template], [u_ob_c36_add, u_ob, u_slot_name, [cadr, u_found]], [cadr, u_found]], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: No binding for ~A in instantiate.~%"), u_template], u_template]]]], [[u_ob_c63, u_template], [u_yloop, [u_initial, [rest, [u_ob_c36_pairs, u_template]]], [u_ywhile, rest], [u_ydo, [if, [and, [u_ob_c63, [u_slots_value, [car, rest]]], [u_ob_c36_literal_c63, [u_slots_value, [car, rest]]]], [u_slots_value, [car, rest]], [if, [u_number_c63, u_depth], [if, [>, u_depth, 1], [u_ob_c36_instantiate2_c33, [u_slots_value, [car, rest]], bindings, ['-1+', u_depth], u_template, [u_slots_name, [car, rest]]], [u_slots_value, [car, rest]]], [u_ob_c36_instantiate2_c33, [u_slots_value, [car, rest]], bindings, [], u_template, [u_slots_name, [car, rest]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_template]]], [u_else, u_template]]]).
wl:arglist_info(u_ob_c36_instantiate2_c33, f_u_ob_c36_instantiate2_c33, [u_template, bindings, u_depth, u_ob, u_slot_name], arginfo{all:[u_template, bindings, u_depth, u_ob, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_ob, u_slot_name], opt:0, req:[u_template, bindings, u_depth, u_ob, u_slot_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_instantiate2_c33).

/*

### Compiled:  `U::OB$INSTANTIATE2!` 
*/
f_u_ob_c36_instantiate2_c33(Template, Bindings, Depth, Ob, Slot_name, ElseResult35) :-
	nop(global_env(Env)),
	Env40=[bv(u_template, Template), bv(bindings, Bindings), bv(u_depth, Depth), bv(u_ob, Ob), bv(u_slot_name, Slot_name)|Env],
	f_u_var_c63(u_template, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq([u_variable_name, u_template], [cdr, bindings], Found_Init),
	    LEnv=[bv(u_found, Found_Init)|Env40],
	    get_var(LEnv, u_found, IFTEST13),
	    (   IFTEST13\==[]
	    ->  get_var(LEnv, u_ob, Ob_Get),
		get_var(LEnv, u_slot_name, Slot_name_Get),
		get_var(LEnv, u_template, Template_Get),
		f_u_ob_c36_remove(Ob_Get,
				  Slot_name_Get,
				  Template_Get,
				  C36_remove_Ret),
		get_var(LEnv, u_found, Found_Get21),
		get_var(LEnv, u_ob, Ob_Get19),
		get_var(LEnv, u_slot_name, Slot_name_Get20),
		cl_cadr(Found_Get21, Cadr_Ret),
		f_u_ob_c36_add(Ob_Get19, Slot_name_Get20, Cadr_Ret, C36_add_Ret),
		get_var(LEnv, u_found, Found_Get22),
		cl_cadr(Found_Get22, TrueResult),
		ElseResult35=TrueResult
	    ;   f_u_ndbg(u_xx_gate_dbg_xx,
			 u_ob_warn,
			 
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "Warning: No binding for ~A in instantiate.~%"),
			   u_template
			 ],
			 Ndbg_Ret),
		get_var(LEnv, u_template, Template_Get23),
		ElseResult35=Template_Get23
	    )
	;   f_u_ob_c63(u_template, IFTEST26),
	    (   IFTEST26\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [rest, [u_ob_c36_pairs, u_template]]],
			    [u_ywhile, rest],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ and,
				  [u_ob_c63, [u_slots_value, [car, rest]]],
				  
				  [ u_ob_c36_literal_c63,
				    [u_slots_value, [car, rest]]
				  ]
				],
				[u_slots_value, [car, rest]],
				
				[ if,
				  [u_number_c63, u_depth],
				  
				  [ if,
				    [>, u_depth, 1],
				    
				    [ u_ob_c36_instantiate2_c33,
				      [u_slots_value, [car, rest]],
				      bindings,
				      ['-1+', u_depth],
				      u_template,
				      [u_slots_name, [car, rest]]
				    ],
				    [u_slots_value, [car, rest]]
				  ],
				  
				  [ u_ob_c36_instantiate2_c33,
				    [u_slots_value, [car, rest]],
				    bindings,
				    [],
				    u_template,
				    [u_slots_name, [car, rest]]
				  ]
				]
			      ],
			      [setq, rest, [cdr, rest]]
			    ],
			    [u_yresult, u_template]
			  ],
			  TrueResult34),
		ElseResult35=TrueResult34
	    ;   get_var(Env40, u_else, IFTEST28),
		(   IFTEST28\==[]
		->  get_var(Env40, u_template, Template_Get31),
		    ElseResult35=Template_Get31
		;   ElseResult35=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_instantiate2_c33, classof, claz_function),
   set_opv(u_ob_c36_instantiate2_c33, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate2_c33, function, f_u_ob_c36_instantiate2_c33),
   _Ignored4=u_ob_c36_instantiate2_c33.
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate2_c33,
			  wl:lambda_def(defun, u_ob_c36_instantiate2_c33, f_u_ob_c36_instantiate2_c33, [u_template, bindings, u_depth, u_ob, u_slot_name], [[cond, [[u_var_c63, u_template], [let, [[u_found, [ext_assq, [u_variable_name, u_template], [cdr, bindings]]]], [if, u_found, [progn, [u_ob_c36_remove, u_ob, u_slot_name, u_template], [u_ob_c36_add, u_ob, u_slot_name, [cadr, u_found]], [cadr, u_found]], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: No binding for ~A in instantiate.~%"), u_template], u_template]]]], [[u_ob_c63, u_template], [u_yloop, [u_initial, [rest, [u_ob_c36_pairs, u_template]]], [u_ywhile, rest], [u_ydo, [if, [and, [u_ob_c63, [u_slots_value, [car, rest]]], [u_ob_c36_literal_c63, [u_slots_value, [car, rest]]]], [u_slots_value, [car, rest]], [if, [u_number_c63, u_depth], [if, [>, u_depth, 1], [u_ob_c36_instantiate2_c33, [u_slots_value, [car, rest]], bindings, ['-1+', u_depth], u_template, [u_slots_name, [car, rest]]], [u_slots_value, [car, rest]]], [u_ob_c36_instantiate2_c33, [u_slots_value, [car, rest]], bindings, [], u_template, [u_slots_name, [car, rest]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_template]]], [u_else, u_template]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate2_c33,
			  wl:arglist_info(u_ob_c36_instantiate2_c33, f_u_ob_c36_instantiate2_c33, [u_template, bindings, u_depth, u_ob, u_slot_name], arginfo{all:[u_template, bindings, u_depth, u_ob, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, bindings, u_depth, u_ob, u_slot_name], opt:0, req:[u_template, bindings, u_depth, u_ob, u_slot_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_instantiate2_c33,
			  wl:init_args(exact_only, f_u_ob_c36_instantiate2_c33))).
*/
/*
*/
/*
 Copies an ob down to the given depth. Does NOT replace variables
*/
/*
 with their values the way ob-instantiate does.
*/
/*
*/
/*
 (coding assistance from Sergio Alvarado)
*/
/*
*/
/*
(defun ob$copy (self)
  (setq *found-obs* nil)
  (copy-ob1 self 1 '(top-context)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8671 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$copy',[self],[setq,'*found-obs*',[]],['copy-ob1',self,1,[quote,['top-context']]]])
wl:lambda_def(defun, u_ob_c36_copy, f_u_ob_c36_copy, [u_self], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_self, 1, [quote, [u_top_context]]]]).
wl:arglist_info(u_ob_c36_copy, f_u_ob_c36_copy, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_copy).

/*

### Compiled:  `U::OB$COPY` 
*/
f_u_ob_c36_copy(Self, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_self, Self)|Env],
	set_var(AEnv, setq, u_xx_found_obs_xx, []),
	get_var(AEnv, u_self, Self_Get),
	f_u_copy_ob1(Self_Get, 1, [u_top_context], Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_ob_c36_copy, classof, claz_function),
   set_opv(u_ob_c36_copy, compile_as, kw_function),
   set_opv(u_ob_c36_copy, function, f_u_ob_c36_copy),
   _Ignored4=u_ob_c36_copy.
/*
:- side_effect(assert_lsp(u_ob_c36_copy,
			  wl:lambda_def(defun, u_ob_c36_copy, f_u_ob_c36_copy, [u_self], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_self, 1, [quote, [u_top_context]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_copy,
			  wl:arglist_info(u_ob_c36_copy, f_u_ob_c36_copy, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_copy,
			  wl:init_args(exact_only, f_u_ob_c36_copy))).
*/
/*
(defun ob$copy-deep (self)
  (setq *found-obs* nil)
  (copy-ob1 self 1000 nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8755 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$copy-deep',[self],[setq,'*found-obs*',[]],['copy-ob1',self,1000,[]]])
wl:lambda_def(defun, u_ob_c36_copy_deep, f_u_ob_c36_copy_deep, [u_self], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_self, 1000, []]]).
wl:arglist_info(u_ob_c36_copy_deep, f_u_ob_c36_copy_deep, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_copy_deep).

/*

### Compiled:  `U::OB$COPY-DEEP` 
*/
f_u_ob_c36_copy_deep(Self, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_self, Self)|Env],
	set_var(AEnv, setq, u_xx_found_obs_xx, []),
	get_var(AEnv, u_self, Self_Get),
	f_u_copy_ob1(Self_Get, 1000, [], Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_ob_c36_copy_deep, classof, claz_function),
   set_opv(u_ob_c36_copy_deep, compile_as, kw_function),
   set_opv(u_ob_c36_copy_deep, function, f_u_ob_c36_copy_deep),
   _Ignored4=u_ob_c36_copy_deep.
/*
:- side_effect(assert_lsp(u_ob_c36_copy_deep,
			  wl:lambda_def(defun, u_ob_c36_copy_deep, f_u_ob_c36_copy_deep, [u_self], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_self, 1000, []]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_copy_deep,
			  wl:arglist_info(u_ob_c36_copy_deep, f_u_ob_c36_copy_deep, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_copy_deep,
			  wl:init_args(exact_only, f_u_ob_c36_copy_deep))).
*/
/*
(defun copy-ob (template)
  (setq *found-obs* nil)
  (copy-ob1 template 1 nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8836 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'copy-ob',[template],[setq,'*found-obs*',[]],['copy-ob1',template,1,[]]])
wl:lambda_def(defun, u_copy_ob, f_u_copy_ob, [u_template], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_template, 1, []]]).
wl:arglist_info(u_copy_ob, f_u_copy_ob, [u_template], arginfo{all:[u_template], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template], opt:0, req:[u_template], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_copy_ob).

/*

### Compiled:  `U::COPY-OB` 
*/
f_u_copy_ob(Template, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_template, Template)|Env],
	set_var(AEnv, setq, u_xx_found_obs_xx, []),
	get_var(AEnv, u_template, Template_Get),
	f_u_copy_ob1(Template_Get, 1, [], Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_copy_ob, classof, claz_function),
   set_opv(u_copy_ob, compile_as, kw_function),
   set_opv(u_copy_ob, function, f_u_copy_ob),
   _Ignored4=u_copy_ob.
/*
:- side_effect(assert_lsp(u_copy_ob,
			  wl:lambda_def(defun, u_copy_ob, f_u_copy_ob, [u_template], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_template, 1, []]]))).
*/
/*
:- side_effect(assert_lsp(u_copy_ob,
			  wl:arglist_info(u_copy_ob, f_u_copy_ob, [u_template], arginfo{all:[u_template], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template], opt:0, req:[u_template], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_copy_ob, wl:init_args(exact_only, f_u_copy_ob))).
*/
/*
(defun ob$copy-omit (ob slots)
  (setq *found-obs* nil)
  (copy-ob1 ob 1 slots))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8917 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$copy-omit',[ob,slots],[setq,'*found-obs*',[]],['copy-ob1',ob,1,slots]])
wl:lambda_def(defun, u_ob_c36_copy_omit, f_u_ob_c36_copy_omit, [u_ob, sys_slots], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_ob, 1, sys_slots]]).
wl:arglist_info(u_ob_c36_copy_omit, f_u_ob_c36_copy_omit, [u_ob, sys_slots], arginfo{all:[u_ob, sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, sys_slots], opt:0, req:[u_ob, sys_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_copy_omit).

/*

### Compiled:  `U::OB$COPY-OMIT` 
*/
f_u_ob_c36_copy_omit(Ob, Slots, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob), bv(sys_slots, Slots)|Env],
	set_var(AEnv, setq, u_xx_found_obs_xx, []),
	get_var(AEnv, sys_slots, Slots_Get),
	get_var(AEnv, u_ob, Ob_Get),
	f_u_copy_ob1(Ob_Get, 1, Slots_Get, Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_ob_c36_copy_omit, classof, claz_function),
   set_opv(u_ob_c36_copy_omit, compile_as, kw_function),
   set_opv(u_ob_c36_copy_omit, function, f_u_ob_c36_copy_omit),
   _Ignored4=u_ob_c36_copy_omit.
/*
:- side_effect(assert_lsp(u_ob_c36_copy_omit,
			  wl:lambda_def(defun, u_ob_c36_copy_omit, f_u_ob_c36_copy_omit, [u_ob, sys_slots], [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_ob, 1, sys_slots]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_copy_omit,
			  wl:arglist_info(u_ob_c36_copy_omit, f_u_ob_c36_copy_omit, [u_ob, sys_slots], arginfo{all:[u_ob, sys_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, sys_slots], opt:0, req:[u_ob, sys_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_copy_omit,
			  wl:init_args(exact_only, f_u_ob_c36_copy_omit))).
*/
/*
(defun copy-ob1 (template depth omit-slots)
  (cond
   ((var? template) template)
   ((ob? template)
    (cond    
      ((let ((found (assq template *found-obs*)))
            (if found
                (cadr found)
                nil)))
      (else
       (yloop
        (initial (new-ob (ob$create-empty)))
        (yfor sv in (ob$pairs template))
        (ydo (if (not (memq? (slots-name sv) omit-slots))
                (ob$add new-ob
                      (slots-name sv)
                      (if (and (ob? (slots-value sv))
                               (ob$literal? (slots-value sv)))
                          (slots-value sv)
                          (if (number? depth)
                              (if (> depth 1)
                                  (copy-ob1 (slots-value sv)
                                             (-1+ depth)
                                             omit-slots)
                                  (slots-value sv))
                              (copy-ob1 (slots-value sv)
                                         nil
                                         omit-slots))))))
        (yresult (progn
                  (push (list template new-ob) *found-obs*)
                   new-ob))))))
   (else template)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8999 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'copy-ob1',[template,depth,'omit-slots'],[cond,[['var?',template],template],[['ob?',template],[cond,[[let,[[found,[assq,template,'*found-obs*']]],[if,found,[cadr,found],[]]]],[else,[yloop,[initial,['new-ob',['ob$create-empty']]],[yfor,sv,in,['ob$pairs',template]],[ydo,[if,[not,['memq?',['slots-name',sv],'omit-slots']],['ob$add','new-ob',['slots-name',sv],[if,[and,['ob?',['slots-value',sv]],['ob$literal?',['slots-value',sv]]],['slots-value',sv],[if,['number?',depth],[if,[>,depth,1],['copy-ob1',['slots-value',sv],['-1+',depth],'omit-slots'],['slots-value',sv]],['copy-ob1',['slots-value',sv],[],'omit-slots']]]]]],[yresult,[progn,[push,[list,template,'new-ob'],'*found-obs*'],'new-ob']]]]]],[else,template]]])
wl:lambda_def(defun, u_copy_ob1, f_u_copy_ob1, [u_template, u_depth, u_omit_slots], [[cond, [[u_var_c63, u_template], u_template], [[u_ob_c63, u_template], [cond, [[let, [[u_found, [ext_assq, u_template, u_xx_found_obs_xx]]], [if, u_found, [cadr, u_found], []]]], [u_else, [u_yloop, [u_initial, [u_new_ob, [u_ob_c36_create_empty]]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_template]], [u_ydo, [if, [not, [u_memq_c63, [u_slots_name, u_sv], u_omit_slots]], [u_ob_c36_add, u_new_ob, [u_slots_name, u_sv], [if, [and, [u_ob_c63, [u_slots_value, u_sv]], [u_ob_c36_literal_c63, [u_slots_value, u_sv]]], [u_slots_value, u_sv], [if, [u_number_c63, u_depth], [if, [>, u_depth, 1], [u_copy_ob1, [u_slots_value, u_sv], ['-1+', u_depth], u_omit_slots], [u_slots_value, u_sv]], [u_copy_ob1, [u_slots_value, u_sv], [], u_omit_slots]]]]]], [u_yresult, [progn, [push, [list, u_template, u_new_ob], u_xx_found_obs_xx], u_new_ob]]]]]], [u_else, u_template]]]).
wl:arglist_info(u_copy_ob1, f_u_copy_ob1, [u_template, u_depth, u_omit_slots], arginfo{all:[u_template, u_depth, u_omit_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, u_depth, u_omit_slots], opt:0, req:[u_template, u_depth, u_omit_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_copy_ob1).

/*

### Compiled:  `U::COPY-OB1` 
*/
f_u_copy_ob1(Template, Depth, Omit_slots, ElseResult28) :-
	nop(global_env(Env)),
	Env41=[bv(u_template, Template), bv(u_depth, Depth), bv(u_omit_slots, Omit_slots)|Env],
	f_u_var_c63(u_template, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env41, u_template, Template_Get),
	    ElseResult28=Template_Get
	;   f_u_ob_c63(u_template, IFTEST10),
	    (   IFTEST10\==[]
	    ->  f_ext_assq(u_template, u_xx_found_obs_xx, Found_Init),
		LEnv=[bv(u_found, Found_Init)|Env41],
		get_var(LEnv, u_found, IFTEST18),
		(   IFTEST18\==[]
		->  get_var(LEnv, u_found, Found_Get21),
		    cl_cadr(Found_Get21, TrueResult),
		    IFTEST12=TrueResult
		;   IFTEST12=[]
		),
		(   IFTEST12\==[]
		->  ElseResult28=[]
		;   get_var(Env41, u_else, IFTEST23),
		    (   IFTEST23\==[]
		    ->  f_u_yloop(
				  [ 
				    [ u_initial,
				      [u_new_ob, [u_ob_c36_create_empty]]
				    ],
				    
				    [ u_yfor,
				      u_sv,
				      u_in,
				      [u_ob_c36_pairs, u_template]
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ not,
					  
					  [ u_memq_c63,
					    [u_slots_name, u_sv],
					    u_omit_slots
					  ]
					],
					
					[ u_ob_c36_add,
					  u_new_ob,
					  [u_slots_name, u_sv],
					  
					  [ if,
					    
					    [ and,
					      [u_ob_c63, [u_slots_value, u_sv]],
					      
					      [ u_ob_c36_literal_c63,
						[u_slots_value, u_sv]
					      ]
					    ],
					    [u_slots_value, u_sv],
					    
					    [ if,
					      [u_number_c63, u_depth],
					      
					      [ if,
						[>, u_depth, 1],
						
						[ u_copy_ob1,
						  [u_slots_value, u_sv],
						  ['-1+', u_depth],
						  u_omit_slots
						],
						[u_slots_value, u_sv]
					      ],
					      
					      [ u_copy_ob1,
						[u_slots_value, u_sv],
						[],
						u_omit_slots
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ u_yresult,
				      
				      [ progn,
					
					[ push,
					  [list, u_template, u_new_ob],
					  u_xx_found_obs_xx
					],
					u_new_ob
				      ]
				    ]
				  ],
				  TrueResult26),
			ElseResult28=TrueResult26
		    ;   ElseResult28=[]
		    )
		)
	    ;   get_var(Env41, u_else, IFTEST29),
		(   IFTEST29\==[]
		->  get_var(Env41, u_template, Template_Get32),
		    ElseResult28=Template_Get32
		;   ElseResult28=[]
		)
	    )
	).
:- set_opv(f_u_copy_ob1, classof, claz_function),
   set_opv(u_copy_ob1, compile_as, kw_function),
   set_opv(u_copy_ob1, function, f_u_copy_ob1),
   _Ignored4=u_copy_ob1.
/*
:- side_effect(assert_lsp(u_copy_ob1,
			  wl:lambda_def(defun, u_copy_ob1, f_u_copy_ob1, [u_template, u_depth, u_omit_slots], [[cond, [[u_var_c63, u_template], u_template], [[u_ob_c63, u_template], [cond, [[let, [[u_found, [ext_assq, u_template, u_xx_found_obs_xx]]], [if, u_found, [cadr, u_found], []]]], [u_else, [u_yloop, [u_initial, [u_new_ob, [u_ob_c36_create_empty]]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_template]], [u_ydo, [if, [not, [u_memq_c63, [u_slots_name, u_sv], u_omit_slots]], [u_ob_c36_add, u_new_ob, [u_slots_name, u_sv], [if, [and, [u_ob_c63, [u_slots_value, u_sv]], [u_ob_c36_literal_c63, [u_slots_value, u_sv]]], [u_slots_value, u_sv], [if, [u_number_c63, u_depth], [if, [>, u_depth, 1], [u_copy_ob1, [u_slots_value, u_sv], ['-1+', u_depth], u_omit_slots], [u_slots_value, u_sv]], [u_copy_ob1, [u_slots_value, u_sv], [], u_omit_slots]]]]]], [u_yresult, [progn, [push, [list, u_template, u_new_ob], u_xx_found_obs_xx], u_new_ob]]]]]], [u_else, u_template]]]))).
*/
/*
:- side_effect(assert_lsp(u_copy_ob1,
			  wl:arglist_info(u_copy_ob1, f_u_copy_ob1, [u_template, u_depth, u_omit_slots], arginfo{all:[u_template, u_depth, u_omit_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_template, u_depth, u_omit_slots], opt:0, req:[u_template, u_depth, u_omit_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_copy_ob1, wl:init_args(exact_only, f_u_copy_ob1))).
*/
/*
(defun vars-in? (ob)
  (setq *found-vars* nil)
  (vars-in1? ob))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:10254 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'vars-in?',[ob],[setq,'*found-vars*',[]],['vars-in1?',ob]])
wl:lambda_def(defun, u_vars_in_c63, f_u_vars_in_c63, [u_ob], [[setq, u_xx_found_vars_xx, []], [u_vars_in1_c63, u_ob]]).
wl:arglist_info(u_vars_in_c63, f_u_vars_in_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_vars_in_c63).

/*

### Compiled:  `U::VARS-IN?` 
*/
f_u_vars_in_c63(Ob, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob)|Env],
	set_var(AEnv, setq, u_xx_found_vars_xx, []),
	get_var(AEnv, u_ob, Ob_Get),
	f_u_vars_in1_c63(Ob_Get, In1_c63_Ret),
	In1_c63_Ret=FnResult.
:- set_opv(f_u_vars_in_c63, classof, claz_function),
   set_opv(u_vars_in_c63, compile_as, kw_function),
   set_opv(u_vars_in_c63, function, f_u_vars_in_c63),
   _Ignored4=u_vars_in_c63.
/*
:- side_effect(assert_lsp(u_vars_in_c63,
			  wl:lambda_def(defun, u_vars_in_c63, f_u_vars_in_c63, [u_ob], [[setq, u_xx_found_vars_xx, []], [u_vars_in1_c63, u_ob]]))).
*/
/*
:- side_effect(assert_lsp(u_vars_in_c63,
			  wl:arglist_info(u_vars_in_c63, f_u_vars_in_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_vars_in_c63,
			  wl:init_args(exact_only, f_u_vars_in_c63))).
*/
/*
(setq *vars-in-ignores*
  '(linked-to linked-from linked-to-of linked-from-of
              analogical-episode main-motiv termination-context
              failure-context))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:10320 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*vars-in-ignores*',[quote,['linked-to','linked-from','linked-to-of','linked-from-of','analogical-episode','main-motiv','termination-context','failure-context']]])
:- set_var(AEnv,
	   setq,
	   u_xx_vars_in_ignores_xx,
	   
	   [ u_linked_to,
	     u_linked_from,
	     u_linked_to_of,
	     u_linked_from_of,
	     u_analogical_episode,
	     u_main_motiv,
	     u_termination_context,
	     u_failure_context
	   ]).
/*
(defun vars-in1? (ob)
  (if (memq? ob *found-vars*)
      nil
      (progn
       (setq *found-vars* (cons ob *found-vars*))
       (cond
        ((and (ob? ob) (ob$literal? ob)) nil)
        ((ob? ob)
         (yloop (initial (result nil))
               (yfor sv in (ob$pairs ob))
               (ywhile (not result))
               (ydo (if (and (not (cx? (slots-value sv)))
                            (not (memq? (slots-name sv)
                                        *vars-in-ignores*))
                            (not (memq? (slots-name sv)
                                        *permanent-ignore-slots*)))
                       (if (and (var? (slots-value sv))
                                (not (memq? (slots-value sv) result)))
                           (setq result t)
                           (setq result (vars-in1? (slots-value sv))))))
               (yresult result)))
        (else nil)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:10495 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'vars-in1?',[ob],[if,['memq?',ob,'*found-vars*'],[],[progn,[setq,'*found-vars*',[cons,ob,'*found-vars*']],[cond,[[and,['ob?',ob],['ob$literal?',ob]],[]],[['ob?',ob],[yloop,[initial,[result,[]]],[yfor,sv,in,['ob$pairs',ob]],[ywhile,[not,result]],[ydo,[if,[and,[not,['cx?',['slots-value',sv]]],[not,['memq?',['slots-name',sv],'*vars-in-ignores*']],[not,['memq?',['slots-name',sv],'*permanent-ignore-slots*']]],[if,[and,['var?',['slots-value',sv]],[not,['memq?',['slots-value',sv],result]]],[setq,result,t],[setq,result,['vars-in1?',['slots-value',sv]]]]]],[yresult,result]]],[else,[]]]]]])
wl:lambda_def(defun, u_vars_in1_c63, f_u_vars_in1_c63, [u_ob], [[if, [u_memq_c63, u_ob, u_xx_found_vars_xx], [], [progn, [setq, u_xx_found_vars_xx, [cons, u_ob, u_xx_found_vars_xx]], [cond, [[and, [u_ob_c63, u_ob], [u_ob_c36_literal_c63, u_ob]], []], [[u_ob_c63, u_ob], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ywhile, [not, u_result]], [u_ydo, [if, [and, [not, [u_cx_c63, [u_slots_value, u_sv]]], [not, [u_memq_c63, [u_slots_name, u_sv], u_xx_vars_in_ignores_xx]], [not, [u_memq_c63, [u_slots_name, u_sv], u_xx_permanent_ignore_slots_xx]]], [if, [and, [u_var_c63, [u_slots_value, u_sv]], [not, [u_memq_c63, [u_slots_value, u_sv], u_result]]], [setq, u_result, t], [setq, u_result, [u_vars_in1_c63, [u_slots_value, u_sv]]]]]], [u_yresult, u_result]]], [u_else, []]]]]]).
wl:arglist_info(u_vars_in1_c63, f_u_vars_in1_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_vars_in1_c63).

/*

### Compiled:  `U::VARS-IN1?` 
*/
f_u_vars_in1_c63(Ob, ElseResult25) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob)|Env],
	f_u_memq_c63(u_ob, u_xx_found_vars_xx, IFTEST),
	(   IFTEST\==[]
	->  ElseResult25=[]
	;   get_var(AEnv, u_ob, Ob_Get),
	    get_var(AEnv, u_xx_found_vars_xx, Xx_found_vars_xx_Get),
	    Xx_found_vars_xx=[Ob_Get|Xx_found_vars_xx_Get],
	    set_var(AEnv, u_xx_found_vars_xx, Xx_found_vars_xx),
	    f_u_ob_c63(u_ob, IFTEST14),
	    (   IFTEST14\==[]
	    ->  get_var(AEnv, u_ob, Ob_Get16),
		f_u_ob_c36_literal_c63(Ob_Get16, TrueResult),
		IFTEST12=TrueResult
	    ;   IFTEST12=[]
	    ),
	    (   IFTEST12\==[]
	    ->  ElseResult25=[]
	    ;   f_u_ob_c63(u_ob, IFTEST18),
		(   IFTEST18\==[]
		->  f_u_yloop(
			      [ [u_initial, [u_result, []]],
				[u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
				[u_ywhile, [not, u_result]],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ and,
				      [not, [u_cx_c63, [u_slots_value, u_sv]]],
				      
				      [ not,
					
					[ u_memq_c63,
					  [u_slots_name, u_sv],
					  u_xx_vars_in_ignores_xx
					]
				      ],
				      
				      [ not,
					
					[ u_memq_c63,
					  [u_slots_name, u_sv],
					  u_xx_permanent_ignore_slots_xx
					]
				      ]
				    ],
				    
				    [ if,
				      
				      [ and,
					[u_var_c63, [u_slots_value, u_sv]],
					
					[ not,
					  
					  [ u_memq_c63,
					    [u_slots_value, u_sv],
					    u_result
					  ]
					]
				      ],
				      [setq, u_result, t],
				      
				      [ setq,
					u_result,
					[u_vars_in1_c63, [u_slots_value, u_sv]]
				      ]
				    ]
				  ]
				],
				[u_yresult, u_result]
			      ],
			      TrueResult24),
		    ElseResult25=TrueResult24
		;   get_var(AEnv, u_else, IFTEST20),
		    (   IFTEST20\==[]
		    ->  ElseResult25=[]
		    ;   ElseResult25=[]
		    )
		)
	    )
	).
:- set_opv(f_u_vars_in1_c63, classof, claz_function),
   set_opv(u_vars_in1_c63, compile_as, kw_function),
   set_opv(u_vars_in1_c63, function, f_u_vars_in1_c63),
   _Ignored4=u_vars_in1_c63.
/*
:- side_effect(assert_lsp(u_vars_in1_c63,
			  wl:lambda_def(defun, u_vars_in1_c63, f_u_vars_in1_c63, [u_ob], [[if, [u_memq_c63, u_ob, u_xx_found_vars_xx], [], [progn, [setq, u_xx_found_vars_xx, [cons, u_ob, u_xx_found_vars_xx]], [cond, [[and, [u_ob_c63, u_ob], [u_ob_c36_literal_c63, u_ob]], []], [[u_ob_c63, u_ob], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ywhile, [not, u_result]], [u_ydo, [if, [and, [not, [u_cx_c63, [u_slots_value, u_sv]]], [not, [u_memq_c63, [u_slots_name, u_sv], u_xx_vars_in_ignores_xx]], [not, [u_memq_c63, [u_slots_name, u_sv], u_xx_permanent_ignore_slots_xx]]], [if, [and, [u_var_c63, [u_slots_value, u_sv]], [not, [u_memq_c63, [u_slots_value, u_sv], u_result]]], [setq, u_result, t], [setq, u_result, [u_vars_in1_c63, [u_slots_value, u_sv]]]]]], [u_yresult, u_result]]], [u_else, []]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_vars_in1_c63,
			  wl:arglist_info(u_vars_in1_c63, f_u_vars_in1_c63, [u_ob], arginfo{all:[u_ob], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob], opt:0, req:[u_ob], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_vars_in1_c63,
			  wl:init_args(exact_only, f_u_vars_in1_c63))).
*/
/*
(setq *found-vars* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:11414 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*found-vars*',[]])
:- set_var(AEnv, setq, u_xx_found_vars_xx, []).
/*
(defun variables-in (ob omit-slots)
  (setq *found-vars* nil)
  (variables-in1 ob omit-slots))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:11439 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'variables-in',[ob,'omit-slots'],[setq,'*found-vars*',[]],['variables-in1',ob,'omit-slots']])
wl:lambda_def(defun, u_variables_in, f_u_variables_in, [u_ob, u_omit_slots], [[setq, u_xx_found_vars_xx, []], [u_variables_in1, u_ob, u_omit_slots]]).
wl:arglist_info(u_variables_in, f_u_variables_in, [u_ob, u_omit_slots], arginfo{all:[u_ob, u_omit_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_omit_slots], opt:0, req:[u_ob, u_omit_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_variables_in).

/*

### Compiled:  `U::VARIABLES-IN` 
*/
f_u_variables_in(Ob, Omit_slots, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob), bv(u_omit_slots, Omit_slots)|Env],
	set_var(AEnv, setq, u_xx_found_vars_xx, []),
	get_var(AEnv, u_ob, Ob_Get),
	get_var(AEnv, u_omit_slots, Omit_slots_Get),
	f_u_variables_in1(Ob_Get, Omit_slots_Get, Variables_in1_Ret),
	Variables_in1_Ret=FnResult.
:- set_opv(f_u_variables_in, classof, claz_function),
   set_opv(u_variables_in, compile_as, kw_function),
   set_opv(u_variables_in, function, f_u_variables_in),
   _Ignored4=u_variables_in.
/*
:- side_effect(assert_lsp(u_variables_in,
			  wl:lambda_def(defun, u_variables_in, f_u_variables_in, [u_ob, u_omit_slots], [[setq, u_xx_found_vars_xx, []], [u_variables_in1, u_ob, u_omit_slots]]))).
*/
/*
:- side_effect(assert_lsp(u_variables_in,
			  wl:arglist_info(u_variables_in, f_u_variables_in, [u_ob, u_omit_slots], arginfo{all:[u_ob, u_omit_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_omit_slots], opt:0, req:[u_ob, u_omit_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_variables_in,
			  wl:init_args(exact_only, f_u_variables_in))).
*/
/*
(defun variables-in1 (ob omit-slots)
  (if (memq? ob *found-vars*)
      nil
      (progn
       (setq *found-vars* (cons ob *found-vars*))
       (cond
        ((and (ob? ob) (ob$literal? ob)) nil)
        ((ob? ob)
         (yloop (initial (result nil))
             (yfor sv in (ob$pairs ob))
             (ydo (if (and (not (memq? (slots-name sv) omit-slots))
                          (not (cx? (slots-value sv))))
                  (if (and (var? (slots-value sv))
                          (not (memq? (slots-value sv) result)))
                     (setq result (cons (slots-value sv) result))
                     (setq result (union result (variables-in1 (slots-value
                                                         sv) omit-slots))))))
             (yresult result)))
        (else nil)))))

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:11535 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'variables-in1',[ob,'omit-slots'],[if,['memq?',ob,'*found-vars*'],[],[progn,[setq,'*found-vars*',[cons,ob,'*found-vars*']],[cond,[[and,['ob?',ob],['ob$literal?',ob]],[]],[['ob?',ob],[yloop,[initial,[result,[]]],[yfor,sv,in,['ob$pairs',ob]],[ydo,[if,[and,[not,['memq?',['slots-name',sv],'omit-slots']],[not,['cx?',['slots-value',sv]]]],[if,[and,['var?',['slots-value',sv]],[not,['memq?',['slots-value',sv],result]]],[setq,result,[cons,['slots-value',sv],result]],[setq,result,[union,result,['variables-in1',['slots-value',sv],'omit-slots']]]]]],[yresult,result]]],[else,[]]]]]])
wl:lambda_def(defun, u_variables_in1, f_u_variables_in1, [u_ob, u_omit_slots], [[if, [u_memq_c63, u_ob, u_xx_found_vars_xx], [], [progn, [setq, u_xx_found_vars_xx, [cons, u_ob, u_xx_found_vars_xx]], [cond, [[and, [u_ob_c63, u_ob], [u_ob_c36_literal_c63, u_ob]], []], [[u_ob_c63, u_ob], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ydo, [if, [and, [not, [u_memq_c63, [u_slots_name, u_sv], u_omit_slots]], [not, [u_cx_c63, [u_slots_value, u_sv]]]], [if, [and, [u_var_c63, [u_slots_value, u_sv]], [not, [u_memq_c63, [u_slots_value, u_sv], u_result]]], [setq, u_result, [cons, [u_slots_value, u_sv], u_result]], [setq, u_result, [union, u_result, [u_variables_in1, [u_slots_value, u_sv], u_omit_slots]]]]]], [u_yresult, u_result]]], [u_else, []]]]]]).
wl:arglist_info(u_variables_in1, f_u_variables_in1, [u_ob, u_omit_slots], arginfo{all:[u_ob, u_omit_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_omit_slots], opt:0, req:[u_ob, u_omit_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_variables_in1).

/*

### Compiled:  `U::VARIABLES-IN1` 
*/
f_u_variables_in1(Ob, Omit_slots, ElseResult25) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob, Ob), bv(u_omit_slots, Omit_slots)|Env],
	f_u_memq_c63(u_ob, u_xx_found_vars_xx, IFTEST),
	(   IFTEST\==[]
	->  ElseResult25=[]
	;   get_var(AEnv, u_ob, Ob_Get),
	    get_var(AEnv, u_xx_found_vars_xx, Xx_found_vars_xx_Get),
	    Xx_found_vars_xx=[Ob_Get|Xx_found_vars_xx_Get],
	    set_var(AEnv, u_xx_found_vars_xx, Xx_found_vars_xx),
	    f_u_ob_c63(u_ob, IFTEST14),
	    (   IFTEST14\==[]
	    ->  get_var(AEnv, u_ob, Ob_Get16),
		f_u_ob_c36_literal_c63(Ob_Get16, TrueResult),
		IFTEST12=TrueResult
	    ;   IFTEST12=[]
	    ),
	    (   IFTEST12\==[]
	    ->  ElseResult25=[]
	    ;   f_u_ob_c63(u_ob, IFTEST18),
		(   IFTEST18\==[]
		->  f_u_yloop(
			      [ [u_initial, [u_result, []]],
				[u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ not,
					
					[ u_memq_c63,
					  [u_slots_name, u_sv],
					  u_omit_slots
					]
				      ],
				      [not, [u_cx_c63, [u_slots_value, u_sv]]]
				    ],
				    
				    [ if,
				      
				      [ and,
					[u_var_c63, [u_slots_value, u_sv]],
					
					[ not,
					  
					  [ u_memq_c63,
					    [u_slots_value, u_sv],
					    u_result
					  ]
					]
				      ],
				      
				      [ setq,
					u_result,
					[cons, [u_slots_value, u_sv], u_result]
				      ],
				      
				      [ setq,
					u_result,
					
					[ union,
					  u_result,
					  
					  [ u_variables_in1,
					    [u_slots_value, u_sv],
					    u_omit_slots
					  ]
					]
				      ]
				    ]
				  ]
				],
				[u_yresult, u_result]
			      ],
			      TrueResult24),
		    ElseResult25=TrueResult24
		;   get_var(AEnv, u_else, IFTEST20),
		    (   IFTEST20\==[]
		    ->  ElseResult25=[]
		    ;   ElseResult25=[]
		    )
		)
	    )
	).
:- set_opv(f_u_variables_in1, classof, claz_function),
   set_opv(u_variables_in1, compile_as, kw_function),
   set_opv(u_variables_in1, function, f_u_variables_in1),
   _Ignored4=u_variables_in1.
/*
:- side_effect(assert_lsp(u_variables_in1,
			  wl:lambda_def(defun, u_variables_in1, f_u_variables_in1, [u_ob, u_omit_slots], [[if, [u_memq_c63, u_ob, u_xx_found_vars_xx], [], [progn, [setq, u_xx_found_vars_xx, [cons, u_ob, u_xx_found_vars_xx]], [cond, [[and, [u_ob_c63, u_ob], [u_ob_c36_literal_c63, u_ob]], []], [[u_ob_c63, u_ob], [u_yloop, [u_initial, [u_result, []]], [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ydo, [if, [and, [not, [u_memq_c63, [u_slots_name, u_sv], u_omit_slots]], [not, [u_cx_c63, [u_slots_value, u_sv]]]], [if, [and, [u_var_c63, [u_slots_value, u_sv]], [not, [u_memq_c63, [u_slots_value, u_sv], u_result]]], [setq, u_result, [cons, [u_slots_value, u_sv], u_result]], [setq, u_result, [union, u_result, [u_variables_in1, [u_slots_value, u_sv], u_omit_slots]]]]]], [u_yresult, u_result]]], [u_else, []]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_variables_in1,
			  wl:arglist_info(u_variables_in1, f_u_variables_in1, [u_ob, u_omit_slots], arginfo{all:[u_ob, u_omit_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_omit_slots], opt:0, req:[u_ob, u_omit_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_variables_in1,
			  wl:init_args(exact_only, f_u_variables_in1))).
*/
/*
 End of file.
*/


%; Total compilation time: 5.96 seconds


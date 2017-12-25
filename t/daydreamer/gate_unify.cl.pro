#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_unify" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:15:17 2017

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
 This file contains the OB unifier
*/
/*
*/
/*
 10/13/84: Original version written
*/
/*
  1/24/85: Upgraded to full unifier
*/
/*
  6/30/85: Added *instance-of*, ob$compare
*/
/*
   9/3/85: Added loop checking in unifier
*/
/*
   1/6/86: Changed special forms to obs
*/
/*
  1/24/86: Commented out compile-web-pattern, added relaxation to ob-unify-var
*/
/*
  1/26/86: Added variable-value
*/
/*
  9/24/86: Removed flavors
*/
/*
  9/29/86: Updated to new unification algorithm
*/
/*
  11/2/86: Added UDIST, changed ob$unify-var
*/
/*
*/
/*
*******************************************************************************
*/
/*
(setq *unify-debugging?* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:760 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*unify-debugging?*',[]])
:- set_var(AEnv, setq, u_xx_unify_debugging_c63_xx, []).
/*
(setq *relax-unify-var* nil)

;
; Empty binding list
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:791 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*relax-unify-var*',[]])
:- set_var(AEnv, setq, u_xx_relax_unify_var_xx, []).
/*
*/
/*
 Empty binding list
*/
/*
*/
/*
(setq *empty-bd* '(t))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:846 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*empty-bd*',[quote,[t]]])
:- set_var(AEnv, setq, u_xx_empty_bd_xx, [t]).
/*
(defun bd-and-empty-bd? (bd)
  (if bd
      (if (null? (cdr bd))
          bd
          nil)
      nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:870 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'bd-and-empty-bd?',[bd],[if,bd,[if,['null?',[cdr,bd]],bd,[]],[]]])
wl:lambda_def(defun, u_bd_and_empty_bd_c63, f_u_bd_and_empty_bd_c63, [u_bd], [[if, u_bd, [if, [u_null_c63, [cdr, u_bd]], u_bd, []], []]]).
wl:arglist_info(u_bd_and_empty_bd_c63, f_u_bd_and_empty_bd_c63, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_bd_and_empty_bd_c63).

/*

### Compiled:  `U::BD-AND-EMPTY-BD?` 
*/
f_u_bd_and_empty_bd_c63(Bd, TrueResult14) :-
	nop(global_env(Env)),
	Env17=[bv(u_bd, Bd)|Env],
	get_var(Env17, u_bd, IFTEST),
	(   IFTEST\==[]
	->  f_u_null_c63([cdr, u_bd], IFTEST10),
	    (   IFTEST10\==[]
	    ->  get_var(Env17, u_bd, Bd_Get12),
		TrueResult14=Bd_Get12
	    ;   TrueResult14=[]
	    )
	;   TrueResult14=[]
	).
:- set_opv(f_u_bd_and_empty_bd_c63, classof, claz_function),
   set_opv(u_bd_and_empty_bd_c63, compile_as, kw_function),
   set_opv(u_bd_and_empty_bd_c63, function, f_u_bd_and_empty_bd_c63),
   _Ignored4=u_bd_and_empty_bd_c63.
/*
:- side_effect(assert_lsp(u_bd_and_empty_bd_c63,
			  wl:lambda_def(defun, u_bd_and_empty_bd_c63, f_u_bd_and_empty_bd_c63, [u_bd], [[if, u_bd, [if, [u_null_c63, [cdr, u_bd]], u_bd, []], []]]))).
*/
/*
:- side_effect(assert_lsp(u_bd_and_empty_bd_c63,
			  wl:arglist_info(u_bd_and_empty_bd_c63, f_u_bd_and_empty_bd_c63, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_bd_and_empty_bd_c63,
			  wl:init_args(exact_only, f_u_bd_and_empty_bd_c63))).
*/
/*
(defun empty-bd? (bd)
  (if (null? (cdr bd))
      bd
      nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:976 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'empty-bd?',[bd],[if,['null?',[cdr,bd]],bd,[]]])
wl:lambda_def(defun, u_empty_bd_c63, f_u_empty_bd_c63, [u_bd], [[if, [u_null_c63, [cdr, u_bd]], u_bd, []]]).
wl:arglist_info(u_empty_bd_c63, f_u_empty_bd_c63, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_empty_bd_c63).

/*

### Compiled:  `U::EMPTY-BD?` 
*/
f_u_empty_bd_c63(Bd, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(u_bd, Bd)|Env],
	f_u_null_c63([cdr, u_bd], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env13, u_bd, Bd_Get),
	    FnResult=Bd_Get
	;   FnResult=[]
	).
:- set_opv(f_u_empty_bd_c63, classof, claz_function),
   set_opv(u_empty_bd_c63, compile_as, kw_function),
   set_opv(u_empty_bd_c63, function, f_u_empty_bd_c63),
   _Ignored4=u_empty_bd_c63.
/*
:- side_effect(assert_lsp(u_empty_bd_c63,
			  wl:lambda_def(defun, u_empty_bd_c63, f_u_empty_bd_c63, [u_bd], [[if, [u_null_c63, [cdr, u_bd]], u_bd, []]]))).
*/
/*
:- side_effect(assert_lsp(u_empty_bd_c63,
			  wl:arglist_info(u_empty_bd_c63, f_u_empty_bd_c63, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_empty_bd_c63,
			  wl:init_args(exact_only, f_u_empty_bd_c63))).
*/
/*
(defun non-empty-bd? (bd)
  (if (cdr bd) bd nil))

;
; (bd-lookup var bindings):
; Look up the value of a variable in a binding list returned by ob$unify.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1043 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'non-empty-bd?',[bd],[if,[cdr,bd],bd,[]]])
wl:lambda_def(defun, u_non_empty_bd_c63, f_u_non_empty_bd_c63, [u_bd], [[if, [cdr, u_bd], u_bd, []]]).
wl:arglist_info(u_non_empty_bd_c63, f_u_non_empty_bd_c63, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_non_empty_bd_c63).

/*

### Compiled:  `U::NON-EMPTY-BD?` 
*/
f_u_non_empty_bd_c63(Bd, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_bd, Bd)|Env],
	get_var(Env14, u_bd, Bd_Get),
	cl_cdr(Bd_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env14, u_bd, Bd_Get10),
	    FnResult=Bd_Get10
	;   FnResult=[]
	).
:- set_opv(f_u_non_empty_bd_c63, classof, claz_function),
   set_opv(u_non_empty_bd_c63, compile_as, kw_function),
   set_opv(u_non_empty_bd_c63, function, f_u_non_empty_bd_c63),
   _Ignored4=u_non_empty_bd_c63.
/*
:- side_effect(assert_lsp(u_non_empty_bd_c63,
			  wl:lambda_def(defun, u_non_empty_bd_c63, f_u_non_empty_bd_c63, [u_bd], [[if, [cdr, u_bd], u_bd, []]]))).
*/
/*
:- side_effect(assert_lsp(u_non_empty_bd_c63,
			  wl:arglist_info(u_non_empty_bd_c63, f_u_non_empty_bd_c63, [u_bd], arginfo{all:[u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_bd], opt:0, req:[u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_non_empty_bd_c63,
			  wl:init_args(exact_only, f_u_non_empty_bd_c63))).
*/
/*
*/
/*
 (bd-lookup var bindings):
*/
/*
 Look up the value of a variable in a binding list returned by ob$unify.
*/
/*
*/
/*
(defun bd-create () (cons t nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1200 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'bd-create',[],[cons,t,[]]])
wl:lambda_def(defun, u_bd_create, f_u_bd_create, [], [[cons, t, []]]).
wl:arglist_info(u_bd_create, f_u_bd_create, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_bd_create).

/*

### Compiled:  `U::BD-CREATE` 
*/
f_u_bd_create(FnResult) :-
	nop(global_env(Env)),
	_329620650=Env,
	_329621976=[t],
	_329621976=FnResult.
:- set_opv(f_u_bd_create, classof, claz_function),
   set_opv(u_bd_create, compile_as, kw_function),
   set_opv(u_bd_create, function, f_u_bd_create),
   _Ignored4=u_bd_create.
/*
:- side_effect(assert_lsp(u_bd_create,
			  wl:lambda_def(defun, u_bd_create, f_u_bd_create, [], [[cons, t, []]]))).
*/
/*
:- side_effect(assert_lsp(u_bd_create,
			  wl:arglist_info(u_bd_create, f_u_bd_create, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_bd_create, wl:init_args(exact_only, f_u_bd_create))).
*/
/*
(defun bd-hyper-lookup1 (var bd vars first-level)
  (if (memq? var vars)
      first-level
      (let ((val (bd-lookup var bd)))
           (if (var? val)
               (bd-hyper-lookup1 (variable-name val)
                                 bd
                                 (cons var vars)
                                 (if first-level first-level val))
               val))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1235 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'bd-hyper-lookup1',[var,bd,vars,'first-level'],[if,['memq?',var,vars],'first-level',[let,[[val,['bd-lookup',var,bd]]],[if,['var?',val],['bd-hyper-lookup1',['variable-name',val],bd,[cons,var,vars],[if,'first-level','first-level',val]],val]]]])
wl:lambda_def(defun, u_bd_hyper_lookup1, f_u_bd_hyper_lookup1, [u_var, u_bd, u_vars, u_first_level], [[if, [u_memq_c63, u_var, u_vars], u_first_level, [let, [[u_val, [u_bd_lookup, u_var, u_bd]]], [if, [u_var_c63, u_val], [u_bd_hyper_lookup1, [u_variable_name, u_val], u_bd, [cons, u_var, u_vars], [if, u_first_level, u_first_level, u_val]], u_val]]]]).
wl:arglist_info(u_bd_hyper_lookup1, f_u_bd_hyper_lookup1, [u_var, u_bd, u_vars, u_first_level], arginfo{all:[u_var, u_bd, u_vars, u_first_level], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd, u_vars, u_first_level], opt:0, req:[u_var, u_bd, u_vars, u_first_level], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_bd_hyper_lookup1).

/*

### Compiled:  `U::BD-HYPER-LOOKUP1` 
*/
f_u_bd_hyper_lookup1(Var, Bd, Vars, First_level, LetResult) :-
	nop(global_env(Env)),
	Env33=[bv(u_var, Var), bv(u_bd, Bd), bv(u_vars, Vars), bv(u_first_level, First_level)|Env],
	f_u_memq_c63(u_var, u_vars, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env33, u_first_level, First_level_Get),
	    LetResult=First_level_Get
	;   f_u_bd_lookup(u_var, u_bd, Val_Init),
	    LEnv=[bv(u_val, Val_Init)|Env33],
	    f_u_var_c63(u_val, IFTEST14),
	    (   IFTEST14\==[]
	    ->  f_u_variable_name(u_val, Hyper_lookup1_Param),
		get_var(LEnv, u_bd, Bd_Get),
		get_var(LEnv, u_var, Var_Get),
		get_var(LEnv, u_vars, Vars_Get),
		_330164288=[Var_Get|Vars_Get],
		get_var(LEnv, u_first_level, IFTEST19),
		(   IFTEST19\==[]
		->  get_var(LEnv, u_first_level, First_level_Get22),
		    _330166914=First_level_Get22
		;   get_var(LEnv, u_val, Val_Get),
		    _330166914=Val_Get
		),
		f_u_bd_hyper_lookup1(Hyper_lookup1_Param,
				     Bd_Get,
				     _330164288,
				     _330166914,
				     TrueResult27),
		LetResult=TrueResult27
	    ;   get_var(LEnv, u_val, Val_Get26),
		LetResult=Val_Get26
	    )
	).
:- set_opv(f_u_bd_hyper_lookup1, classof, claz_function),
   set_opv(u_bd_hyper_lookup1, compile_as, kw_function),
   set_opv(u_bd_hyper_lookup1, function, f_u_bd_hyper_lookup1),
   _Ignored4=u_bd_hyper_lookup1.
/*
:- side_effect(assert_lsp(u_bd_hyper_lookup1,
			  wl:lambda_def(defun, u_bd_hyper_lookup1, f_u_bd_hyper_lookup1, [u_var, u_bd, u_vars, u_first_level], [[if, [u_memq_c63, u_var, u_vars], u_first_level, [let, [[u_val, [u_bd_lookup, u_var, u_bd]]], [if, [u_var_c63, u_val], [u_bd_hyper_lookup1, [u_variable_name, u_val], u_bd, [cons, u_var, u_vars], [if, u_first_level, u_first_level, u_val]], u_val]]]]))).
*/
/*
:- side_effect(assert_lsp(u_bd_hyper_lookup1,
			  wl:arglist_info(u_bd_hyper_lookup1, f_u_bd_hyper_lookup1, [u_var, u_bd, u_vars, u_first_level], arginfo{all:[u_var, u_bd, u_vars, u_first_level], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd, u_vars, u_first_level], opt:0, req:[u_var, u_bd, u_vars, u_first_level], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_bd_hyper_lookup1,
			  wl:init_args(exact_only, f_u_bd_hyper_lookup1))).
*/
/*
(defun variable-hyper-lookup (variable bindings)
  (bd-hyper-lookup (variable-name variable) bindings))

;(defun variable-hyper-lookup (variable bindings)
;  (let ((found (assq (variable-name variable)
;                     (cdr bindings))))
;    (if found
;        (if (var? (cadr found))
;            (variable-hyper-lookup1 (cadr found) bindings
;                                    (list (variable-name variable)))
;            (cadr found))
;        nil)))

;(defun variable-hyper-lookup1 (variable bindings names)
;  (if (memq? (variable-name variable) names)
;      nil
;      (let ((found (assq (variable-name variable)
;                         (cdr bindings))))
;        (if found
;            (if (var? (cadr found))
;                (variable-hyper-lookup1 (cadr found) bindings
;                                        (cons (variable-name variable) names))
;                (cadr found))
;            variable))))

;(defun bd-hyper-lookup1 (var bindings)
;  (let ((found (assq var (cdr bindings))))
;    (if found
;        (if (var? (cadr found))
;            (bd-hyper-lookup (variable-name (cadr found)) bindings)
;            (cadr found))
;        var)))

;
; Variables
;
; Examples of macro translation:
; ?Self --> (UVAR name 'self unifies-with PERSON)
; ?Person1 --> (UVAR name 'person1 unifies-with PERSON))
; ?Silly:Person --> (UVAR name 'silly unifies-with PERSON)
; ?:Person --> (UVAR unifies-with PERSON)
; ?? --> (UVAR)
; ?Notatype --> (UVAR name 'notatype)
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'variable-hyper-lookup',[variable,bindings],['bd-hyper-lookup',['variable-name',variable],bindings]])
wl:lambda_def(defun, u_variable_hyper_lookup, f_u_variable_hyper_lookup, [variable, bindings], [[u_bd_hyper_lookup, [u_variable_name, variable], bindings]]).
wl:arglist_info(u_variable_hyper_lookup, f_u_variable_hyper_lookup, [variable, bindings], arginfo{all:[variable, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[variable, bindings], opt:0, req:[variable, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_variable_hyper_lookup).

/*

### Compiled:  `U::VARIABLE-HYPER-LOOKUP` 
*/
f_u_variable_hyper_lookup(Variable, Bindings, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(variable, Variable), bv(bindings, Bindings)|Env],
	f_u_bd_hyper_lookup([u_variable_name, variable], bindings, Bindings12),
	Bindings12=FnResult.
:- set_opv(f_u_variable_hyper_lookup, classof, claz_function),
   set_opv(u_variable_hyper_lookup, compile_as, kw_function),
   set_opv(u_variable_hyper_lookup, function, f_u_variable_hyper_lookup),
   _Ignored4=u_variable_hyper_lookup.
/*
:- side_effect(assert_lsp(u_variable_hyper_lookup,
			  wl:lambda_def(defun, u_variable_hyper_lookup, f_u_variable_hyper_lookup, [variable, bindings], [[u_bd_hyper_lookup, [u_variable_name, variable], bindings]]))).
*/
/*
:- side_effect(assert_lsp(u_variable_hyper_lookup,
			  wl:arglist_info(u_variable_hyper_lookup, f_u_variable_hyper_lookup, [variable, bindings], arginfo{all:[variable, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[variable, bindings], opt:0, req:[variable, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_variable_hyper_lookup,
			  wl:init_args(exact_only, f_u_variable_hyper_lookup))).
*/
/*
(defun variable-hyper-lookup (variable bindings)
*/
/*
  (let ((found (assq (variable-name variable)
*/
/*
                     (cdr bindings))))
*/
/*
    (if found
*/
/*
        (if (var? (cadr found))
*/
/*
            (variable-hyper-lookup1 (cadr found) bindings
*/
/*
                                    (list (variable-name variable)))
*/
/*
            (cadr found))
*/
/*
        nil)))
*/
/*
(defun variable-hyper-lookup1 (variable bindings names)
*/
/*
  (if (memq? (variable-name variable) names)
*/
/*
      nil
*/
/*
      (let ((found (assq (variable-name variable)
*/
/*
                         (cdr bindings))))
*/
/*
        (if found
*/
/*
            (if (var? (cadr found))
*/
/*
                (variable-hyper-lookup1 (cadr found) bindings
*/
/*
                                        (cons (variable-name variable) names))
*/
/*
                (cadr found))
*/
/*
            variable))))
*/
/*
(defun bd-hyper-lookup1 (var bindings)
*/
/*
  (let ((found (assq var (cdr bindings))))
*/
/*
    (if found
*/
/*
        (if (var? (cadr found))
*/
/*
            (bd-hyper-lookup (variable-name (cadr found)) bindings)
*/
/*
            (cadr found))
*/
/*
        var)))
*/
/*
*/
/*
 Variables
*/
/*
*/
/*
 Examples of macro translation:
*/
/*
 ?Self --> (UVAR name 'self unifies-with PERSON)
*/
/*
 ?Person1 --> (UVAR name 'person1 unifies-with PERSON))
*/
/*
 ?Silly:Person --> (UVAR name 'silly unifies-with PERSON)
*/
/*
 ?:Person --> (UVAR unifies-with PERSON)
*/
/*
 ?? --> (UVAR)
*/
/*
 ?Notatype --> (UVAR name 'notatype)
*/
/*
*/
/*
(defun make-var (name type)
  (cond
   ((and name type)
    (ob$fcreate `(UVAR
                    name (QUOTE ,name)
                    unifies-with ,type)))
   (type
    (ob$fcreate `(UVAR
                    unifies-with ,type)))
   (name
    (ob$fcreate `(UVAR
                    name (QUOTE ,name))))
   (else (ob$fcreate '(UVAR)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3107 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'make-var',[name,type],[cond,[[and,name,type],['ob$fcreate',['#BQ',['UVAR',name,['QUOTE',['#COMMA',name]],'unifies-with',['#COMMA',type]]]]],[type,['ob$fcreate',['#BQ',['UVAR','unifies-with',['#COMMA',type]]]]],[name,['ob$fcreate',['#BQ',['UVAR',name,['QUOTE',['#COMMA',name]]]]]],[else,['ob$fcreate',[quote,['UVAR']]]]]])
wl:lambda_def(defun, u_make_var, f_u_make_var, [sys_name, type], [[cond, [[and, sys_name, type], [u_ob_c36_fcreate, ['#BQ', [u_uvar, sys_name, [quote, ['#COMMA', sys_name]], u_unifies_with, ['#COMMA', type]]]]], [type, [u_ob_c36_fcreate, ['#BQ', [u_uvar, u_unifies_with, ['#COMMA', type]]]]], [sys_name, [u_ob_c36_fcreate, ['#BQ', [u_uvar, sys_name, [quote, ['#COMMA', sys_name]]]]]], [u_else, [u_ob_c36_fcreate, [quote, [u_uvar]]]]]]).
wl:arglist_info(u_make_var, f_u_make_var, [sys_name, type], arginfo{all:[sys_name, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, type], opt:0, req:[sys_name, type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_make_var).

/*

### Compiled:  `U::MAKE-VAR` 
*/
f_u_make_var(Name, Type, ElseResult26) :-
	nop(global_env(Env)),
	Env33=[bv(sys_name, Name), bv(type, Type)|Env],
	get_var(Env33, sys_name, IFTEST9),
	(   IFTEST9\==[]
	->  get_var(Env33, type, Type_Get),
	    IFTEST=Type_Get
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_uvar,
				   sys_name,
				   [quote, ['#COMMA', sys_name]],
				   u_unifies_with,
				   ['#COMMA', type]
				 ]
			       ],
			       TrueResult29),
	    ElseResult26=TrueResult29
	;   get_var(Env33, type, IFTEST14),
	    (   IFTEST14\==[]
	    ->  f_u_ob_c36_fcreate(
				   [ '#BQ',
				     [u_uvar, u_unifies_with, ['#COMMA', type]]
				   ],
				   TrueResult27),
		ElseResult26=TrueResult27
	    ;   get_var(Env33, sys_name, IFTEST17),
		(   IFTEST17\==[]
		->  f_u_ob_c36_fcreate(
				       [ '#BQ',
					 
					 [ u_uvar,
					   sys_name,
					   [quote, ['#COMMA', sys_name]]
					 ]
				       ],
				       TrueResult25),
		    ElseResult26=TrueResult25
		;   get_var(Env33, u_else, IFTEST20),
		    (   IFTEST20\==[]
		    ->  f_u_ob_c36_fcreate([quote, [u_uvar]], TrueResult23),
			ElseResult26=TrueResult23
		    ;   ElseResult26=[]
		    )
		)
	    )
	).
:- set_opv(f_u_make_var, classof, claz_function),
   set_opv(u_make_var, compile_as, kw_function),
   set_opv(u_make_var, function, f_u_make_var),
   _Ignored4=u_make_var.
/*
:- side_effect(assert_lsp(u_make_var,
			  wl:lambda_def(defun, u_make_var, f_u_make_var, [sys_name, type], [[cond, [[and, sys_name, type], [u_ob_c36_fcreate, ['#BQ', [u_uvar, sys_name, [quote, ['#COMMA', sys_name]], u_unifies_with, ['#COMMA', type]]]]], [type, [u_ob_c36_fcreate, ['#BQ', [u_uvar, u_unifies_with, ['#COMMA', type]]]]], [sys_name, [u_ob_c36_fcreate, ['#BQ', [u_uvar, sys_name, [quote, ['#COMMA', sys_name]]]]]], [u_else, [u_ob_c36_fcreate, [quote, [u_uvar]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_make_var,
			  wl:arglist_info(u_make_var, f_u_make_var, [sys_name, type], arginfo{all:[sys_name, type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, type], opt:0, req:[sys_name, type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_make_var, wl:init_args(exact_only, f_u_make_var))).
*/
/*
(defun variable-value (var bd)
  (bd-lookup (variable-name var) bd))

;
; (ob$unify ob1 ob2 bindings):
;
; Unifier for obs
; (Looping check code taken from the rhapsody matcher by Scott Turner).
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'variable-value',[var,bd],['bd-lookup',['variable-name',var],bd]])
wl:lambda_def(defun, u_variable_value, f_u_variable_value, [u_var, u_bd], [[u_bd_lookup, [u_variable_name, u_var], u_bd]]).
wl:arglist_info(u_variable_value, f_u_variable_value, [u_var, u_bd], arginfo{all:[u_var, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd], opt:0, req:[u_var, u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_variable_value).

/*

### Compiled:  `U::VARIABLE-VALUE` 
*/
f_u_variable_value(Var, Bd, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_var, Var), bv(u_bd, Bd)|Env],
	f_u_bd_lookup([u_variable_name, u_var], u_bd, Bd12),
	Bd12=FnResult.
:- set_opv(f_u_variable_value, classof, claz_function),
   set_opv(u_variable_value, compile_as, kw_function),
   set_opv(u_variable_value, function, f_u_variable_value),
   _Ignored4=u_variable_value.
/*
:- side_effect(assert_lsp(u_variable_value,
			  wl:lambda_def(defun, u_variable_value, f_u_variable_value, [u_var, u_bd], [[u_bd_lookup, [u_variable_name, u_var], u_bd]]))).
*/
/*
:- side_effect(assert_lsp(u_variable_value,
			  wl:arglist_info(u_variable_value, f_u_variable_value, [u_var, u_bd], arginfo{all:[u_var, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd], opt:0, req:[u_var, u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_variable_value,
			  wl:init_args(exact_only, f_u_variable_value))).
*/
/*
*/
/*
 (ob$unify ob1 ob2 bindings):
*/
/*
*/
/*
 Unifier for obs
*/
/*
 (Looping check code taken from the rhapsody matcher by Scott Turner).
*/
/*
*/
/*
(setq *already-matched* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3647 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*already-matched*',[]])
:- set_var(AEnv, setq, u_xx_already_matched_xx, []).
/*
(setq *diff?* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3677 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*diff?*',[]])
:- set_var(AEnv, setq, u_xx_diff_c63_xx, []).
/*
(defun ob$unify-dbg (ob1 ob2 bindings ignore-slots)
  (ndbg-begin)
  (ndbg *gate-dbg* unify "Call ob$unify: "(defun ob$unify-dbg (ob1 ob2 bindings ignore-slots)\n  (ndbg-begin)\n  (ndbg *gate-dbg* unify \"Call ob$unify: ~A ~A ~A ~A~%\"\n        ob1 ob2 bindings ignore-slots)\n  (let ((result (ob$unify0 ob1 ob2 bindings ignore-slots)))\n    (ndbg *gate-dbg* unify \"Return from ob$unify: ~A~%\" result)\n    (ndbg-end)\n    result))\n\n;\n; List of slots which unification should always ignore.\n;\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3697 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$unify-dbg',[ob1,ob2,bindings,'ignore-slots'],['ndbg-begin'],[ndbg,'*gate-dbg*',unify,'$STRING'("Call ob$unify: ~A ~A ~A ~A~%"),ob1,ob2,bindings,'ignore-slots'],[let,[[result,['ob$unify0',ob1,ob2,bindings,'ignore-slots']]],[ndbg,'*gate-dbg*',unify,'$STRING'("Return from ob$unify: ~A~%"),result],['ndbg-end'],result]])
wl:lambda_def(defun, u_ob_c36_unify_dbg, f_u_ob_c36_unify_dbg, [u_ob1, u_ob2, bindings, u_ignore_slots], [[u_ndbg_begin], [u_ndbg, u_xx_gate_dbg_xx, u_unify, '$ARRAY'([*], claz_base_character, "Call ob$unify: ~A ~A ~A ~A~%"), u_ob1, u_ob2, bindings, u_ignore_slots], [let, [[u_result, [u_ob_c36_unify0, u_ob1, u_ob2, bindings, u_ignore_slots]]], [u_ndbg, u_xx_gate_dbg_xx, u_unify, '$ARRAY'([*], claz_base_character, "Return from ob$unify: ~A~%"), u_result], [u_ndbg_end], u_result]]).
wl:arglist_info(u_ob_c36_unify_dbg, f_u_ob_c36_unify_dbg, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify_dbg).

/*

### Compiled:  `U::OB$UNIFY-DBG` 
*/
f_u_ob_c36_unify_dbg(Ob1, Ob2, Bindings, Ignore_slots, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots)|Env],
	f_u_ndbg_begin(Ndbg_begin_Ret),
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_unify,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    "Call ob$unify: ~A ~A ~A ~A~%"),
		   u_ob1,
		   u_ob2,
		   bindings,
		   u_ignore_slots
		 ],
		 Ndbg_Ret),
	get_var(Env18, bindings, Bindings_Get),
	get_var(Env18, u_ignore_slots, Ignore_slots_Get),
	get_var(Env18, u_ob1, Ob1_Get),
	get_var(Env18, u_ob2, Ob2_Get),
	f_u_ob_c36_unify0(Ob1_Get,
			  Ob2_Get,
			  Bindings_Get,
			  Ignore_slots_Get,
			  Result_Init),
	LEnv=[bv(u_result, Result_Init)|Env18],
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_unify,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    "Return from ob$unify: ~A~%"),
		   u_result
		 ],
		 Ndbg_Ret25),
	f_u_ndbg_end(Ndbg_end_Ret),
	get_var(LEnv, u_result, Result_Get),
	Result_Get=FnResult.
:- set_opv(f_u_ob_c36_unify_dbg, classof, claz_function),
   set_opv(u_ob_c36_unify_dbg, compile_as, kw_function),
   set_opv(u_ob_c36_unify_dbg, function, f_u_ob_c36_unify_dbg),
   _Ignored4=u_ob_c36_unify_dbg.
/*
:- side_effect(assert_lsp(u_ob_c36_unify_dbg,
			  wl:lambda_def(defun, u_ob_c36_unify_dbg, f_u_ob_c36_unify_dbg, [u_ob1, u_ob2, bindings, u_ignore_slots], [[u_ndbg_begin], [u_ndbg, u_xx_gate_dbg_xx, u_unify, '$ARRAY'([*], claz_base_character, "Call ob$unify: ~A ~A ~A ~A~%"), u_ob1, u_ob2, bindings, u_ignore_slots], [let, [[u_result, [u_ob_c36_unify0, u_ob1, u_ob2, bindings, u_ignore_slots]]], [u_ndbg, u_xx_gate_dbg_xx, u_unify, '$ARRAY'([*], claz_base_character, "Return from ob$unify: ~A~%"), u_result], [u_ndbg_end], u_result]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_dbg,
			  wl:arglist_info(u_ob_c36_unify_dbg, f_u_ob_c36_unify_dbg, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_dbg,
			  wl:init_args(exact_only, f_u_ob_c36_unify_dbg))).
*/
/*
*/
/*
 List of slots which unification should always ignore.
*/
/*
*/
/*
(setq *permanent-ignore-slots* '(top-context value weight offset decay
                                             plan-rule plan-subgoalnum
;;;; no no no linked-to-of linked-from-of
                                             input-state?
                                             inference-rule
                                             indexes))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4072 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*permanent-ignore-slots*',[quote,['top-context',value,weight,offset,decay,'plan-rule','plan-subgoalnum','input-state?','inference-rule',indexes]]])
:- set_var(AEnv,
	   setq,
	   u_xx_permanent_ignore_slots_xx,
	   
	   [ u_top_context,
	     u_value,
	     u_weight,
	     u_offset,
	     u_decay,
	     u_plan_rule,
	     u_plan_subgoalnum,
	     u_input_state_c63,
	     u_inference_rule,
	     u_indexes
	   ]).
/*
;;; no no no linked-to-of linked-from-of
*/
/*
(setq *unify-context* nil)

;
; This could be made faster by doing types first. Actually, types
; are done first anyway because they are the first slot.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4430 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*unify-context*',[]])
:- set_var(AEnv, setq, u_xx_unify_context_xx, []).
/*
*/
/*
 This could be made faster by doing types first. Actually, types
*/
/*
 are done first anyway because they are the first slot.
*/
/*
*/
/*
(defun ob$unify0 (ob1 ob2 bindings ignore-slots)
  (if (memq? ob2 (bd-lookup ob1 *already-matched*))
      bindings
      (progn
       (bd-bind! ob1
                 (cons ob2 (bd-lookup ob1 *already-matched*))
                 *already-matched*)
; The below would introduce a semantics which does not conform
; to unification asymmetry.
;       (bd-bind! ob2
;                 (cons ob1 (bd-lookup ob2 *already-matched*))
;                 *already-matched*)
       (let ((result
        (cond
         ((eq? ob1 ob2) bindings)
         ((or (special? ob1) 
              (special? ob2))
          (if (special-priority? ob1 ob2)
              (ob$unify-special ob1 ob2 bindings ignore-slots nil)
              (ob$unify-special ob2 ob1 bindings ignore-slots t)))
         ((var? ob1)
          (ob$unify-var ob1 ob2 bindings ignore-slots nil))
         ((var? ob2)
          (ob$unify-var ob2 ob1 bindings ignore-slots t))
         ((and (ob? ob1) (ob$literal? ob1)) nil)
         ((and (ob? ob2) (ob$literal? ob2)) nil)
         ((and (ob? ob1) (ob? ob2))
          (yloop (initial (unified-slot-indices nil)
                         (ob2-slots (ob$pairs ob2))
                         (constant-slot-index nil)
                         (last-constant-value nil)
                         (new-bindings nil)
                         (found? nil))
                (yfor cur in (ob$pairs ob1)) ; was reverse
                (ywhile bindings)
                (ydo (if (and (not (memq? (car cur) ignore-slots))
                             (not (memq? (car cur) *permanent-ignore-slots*)))
                        (progn
                         (setq constant-slot-index 0)
                         (setq new-bindings nil)
                         (setq found? nil)
                         (setq last-constant-value nil)
                         (yloop (yfor constant-slot-value in ob2-slots)
                                (yuntil found?)
(ydo
 (if (and (eq? (car cur) (slots-name constant-slot-value))
          (not (memq? constant-slot-index unified-slot-indices))
          (setq last-constant-value (slots-value constant-slot-value))
          (setq new-bindings
                (if (eq? (cadr cur) (slots-value constant-slot-value))
                    bindings
                    (ob$unify2 (cadr cur) (slots-value constant-slot-value)
                               bindings ignore-slots))))
     (progn
      (setq found? t)
      (setq unified-slot-indices
            (cons constant-slot-index unified-slot-indices))))
 (increment-me constant-slot-index)))
                         (if found?
                             (setq bindings new-bindings)
                             (if *diff?*
                                 (setq bindings (bd-bind (slots-name cur)
                                                         (list
                                                          (cadr cur)
                                                          last-constant-value)
                                                         bindings))
                                 (setq bindings nil))))))
                (yresult bindings)))
         (else nil))))
        (if result
            result
            (progn
             (bd-bind! ob1
                       (delq! ob2 (bd-lookup ob1 *already-matched*))
                       *already-matched*)
             (bd-bind! ob2
                       (delq! ob1 (bd-lookup ob2 *already-matched*))
                       *already-matched*)
             nil))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$unify0',[ob1,ob2,bindings,'ignore-slots'],[if,['memq?',ob2,['bd-lookup',ob1,'*already-matched*']],bindings,[progn,['bd-bind!',ob1,[cons,ob2,['bd-lookup',ob1,'*already-matched*']],'*already-matched*'],[let,[[result,[cond,[['eq?',ob1,ob2],bindings],[[or,['special?',ob1],['special?',ob2]],[if,['special-priority?',ob1,ob2],['ob$unify-special',ob1,ob2,bindings,'ignore-slots',[]],['ob$unify-special',ob2,ob1,bindings,'ignore-slots',t]]],[['var?',ob1],['ob$unify-var',ob1,ob2,bindings,'ignore-slots',[]]],[['var?',ob2],['ob$unify-var',ob2,ob1,bindings,'ignore-slots',t]],[[and,['ob?',ob1],['ob$literal?',ob1]],[]],[[and,['ob?',ob2],['ob$literal?',ob2]],[]],[[and,['ob?',ob1],['ob?',ob2]],[yloop,[initial,['unified-slot-indices',[]],['ob2-slots',['ob$pairs',ob2]],['constant-slot-index',[]],['last-constant-value',[]],['new-bindings',[]],['found?',[]]],[yfor,cur,in,['ob$pairs',ob1]],[ywhile,bindings],[ydo,[if,[and,[not,['memq?',[car,cur],'ignore-slots']],[not,['memq?',[car,cur],'*permanent-ignore-slots*']]],[progn,[setq,'constant-slot-index',0],[setq,'new-bindings',[]],[setq,'found?',[]],[setq,'last-constant-value',[]],[yloop,[yfor,'constant-slot-value',in,'ob2-slots'],[yuntil,'found?'],[ydo,[if,[and,['eq?',[car,cur],['slots-name','constant-slot-value']],[not,['memq?','constant-slot-index','unified-slot-indices']],[setq,'last-constant-value',['slots-value','constant-slot-value']],[setq,'new-bindings',[if,['eq?',[cadr,cur],['slots-value','constant-slot-value']],bindings,['ob$unify2',[cadr,cur],['slots-value','constant-slot-value'],bindings,'ignore-slots']]]],[progn,[setq,'found?',t],[setq,'unified-slot-indices',[cons,'constant-slot-index','unified-slot-indices']]]],['increment-me','constant-slot-index']]],[if,'found?',[setq,bindings,'new-bindings'],[if,'*diff?*',[setq,bindings,['bd-bind',['slots-name',cur],[list,[cadr,cur],'last-constant-value'],bindings]],[setq,bindings,[]]]]]]],[yresult,bindings]]],[else,[]]]]],[if,result,result,[progn,['bd-bind!',ob1,['delq!',ob2,['bd-lookup',ob1,'*already-matched*']],'*already-matched*'],['bd-bind!',ob2,['delq!',ob1,['bd-lookup',ob2,'*already-matched*']],'*already-matched*'],[]]]]]]])
wl:lambda_def(defun, u_ob_c36_unify0, f_u_ob_c36_unify0, [u_ob1, u_ob2, bindings, u_ignore_slots], [[if, [u_memq_c63, u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx]], bindings, [progn, [u_bd_bind_c33, u_ob1, [cons, u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx]], u_xx_already_matched_xx], [let, [[u_result, [cond, [[u_eq_c63, u_ob1, u_ob2], bindings], [[or, [u_special_c63, u_ob1], [u_special_c63, u_ob2]], [if, [u_special_priority_c63, u_ob1, u_ob2], [u_ob_c36_unify_special, u_ob1, u_ob2, bindings, u_ignore_slots, []], [u_ob_c36_unify_special, u_ob2, u_ob1, bindings, u_ignore_slots, t]]], [[u_var_c63, u_ob1], [u_ob_c36_unify_var, u_ob1, u_ob2, bindings, u_ignore_slots, []]], [[u_var_c63, u_ob2], [u_ob_c36_unify_var, u_ob2, u_ob1, bindings, u_ignore_slots, t]], [[and, [u_ob_c63, u_ob1], [u_ob_c36_literal_c63, u_ob1]], []], [[and, [u_ob_c63, u_ob2], [u_ob_c36_literal_c63, u_ob2]], []], [[and, [u_ob_c63, u_ob1], [u_ob_c63, u_ob2]], [u_yloop, [u_initial, [u_unified_slot_indices, []], [u_ob2_slots, [u_ob_c36_pairs, u_ob2]], [u_constant_slot_index, []], [u_last_constant_value, []], [u_new_bindings, []], [u_found_c63, []]], [u_yfor, u_cur, u_in, [u_ob_c36_pairs, u_ob1]], [u_ywhile, bindings], [u_ydo, [if, [and, [not, [u_memq_c63, [car, u_cur], u_ignore_slots]], [not, [u_memq_c63, [car, u_cur], u_xx_permanent_ignore_slots_xx]]], [progn, [setq, u_constant_slot_index, 0], [setq, u_new_bindings, []], [setq, u_found_c63, []], [setq, u_last_constant_value, []], [u_yloop, [u_yfor, u_constant_slot_value, u_in, u_ob2_slots], [u_yuntil, u_found_c63], [u_ydo, [if, [and, [u_eq_c63, [car, u_cur], [u_slots_name, u_constant_slot_value]], [not, [u_memq_c63, u_constant_slot_index, u_unified_slot_indices]], [setq, u_last_constant_value, [u_slots_value, u_constant_slot_value]], [setq, u_new_bindings, [if, [u_eq_c63, [cadr, u_cur], [u_slots_value, u_constant_slot_value]], bindings, [u_ob_c36_unify2, [cadr, u_cur], [u_slots_value, u_constant_slot_value], bindings, u_ignore_slots]]]], [progn, [setq, u_found_c63, t], [setq, u_unified_slot_indices, [cons, u_constant_slot_index, u_unified_slot_indices]]]], [u_increment_me, u_constant_slot_index]]], [if, u_found_c63, [setq, bindings, u_new_bindings], [if, u_xx_diff_c63_xx, [setq, bindings, [u_bd_bind, [u_slots_name, u_cur], [list, [cadr, u_cur], u_last_constant_value], bindings]], [setq, bindings, []]]]]]], [u_yresult, bindings]]], [u_else, []]]]], [if, u_result, u_result, [progn, [u_bd_bind_c33, u_ob1, [u_delq_c33, u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx]], u_xx_already_matched_xx], [u_bd_bind_c33, u_ob2, [u_delq_c33, u_ob1, [u_bd_lookup, u_ob2, u_xx_already_matched_xx]], u_xx_already_matched_xx], []]]]]]]).
wl:arglist_info(u_ob_c36_unify0, f_u_ob_c36_unify0, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify0).

/*

### Compiled:  `U::OB$UNIFY0` 
*/
f_u_ob_c36_unify0(Ob1, Ob2, Bindings, Ignore_slots, LetResult) :-
	nop(global_env(Env)),
	Env86=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots)|Env],
	f_u_memq_c63(u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env86, bindings, Bindings_Get),
	    LetResult=Bindings_Get
	;   f_u_bd_bind_c33(u_ob1,
			    
			    [ cons,
			      u_ob2,
			      [u_bd_lookup, u_ob1, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx,
			    Xx_already_matched_xx),
	    f_u_eq_c63(u_ob1, u_ob2, IFTEST13),
	    (   IFTEST13\==[]
	    ->  get_var(Env86, bindings, Bindings_Get15),
		ElseResult65=Bindings_Get15
	    ;   (   f_u_special_c63(u_ob1, FORM1_Res),
		    FORM1_Res\==[],
		    IFTEST16=FORM1_Res
		->  true
		;   f_u_special_c63(u_ob2, Special_c63_Ret),
		    IFTEST16=Special_c63_Ret
		),
		(   IFTEST16\==[]
		->  f_u_special_priority_c63(u_ob1, u_ob2, IFTEST19),
		    (   IFTEST19\==[]
		    ->  get_var(Env86, bindings, Bindings_Get23),
			get_var(Env86, u_ignore_slots, Ignore_slots_Get),
			get_var(Env86, u_ob1, Ob1_Get),
			get_var(Env86, u_ob2, Ob2_Get),
			f_u_ob_c36_unify_special(Ob1_Get,
						 Ob2_Get,
						 Bindings_Get23,
						 Ignore_slots_Get,
						 [],
						 TrueResult),
			ElseResult65=TrueResult
		    ;   get_var(Env86, bindings, Bindings_Get27),
			get_var(Env86, u_ignore_slots, Ignore_slots_Get28),
			get_var(Env86, u_ob1, Ob1_Get26),
			get_var(Env86, u_ob2, Ob2_Get25),
			f_u_ob_c36_unify_special(Ob2_Get25,
						 Ob1_Get26,
						 Bindings_Get27,
						 Ignore_slots_Get28,
						 t,
						 ElseResult),
			ElseResult65=ElseResult
		    )
		;   f_u_var_c63(u_ob1, IFTEST31),
		    (   IFTEST31\==[]
		    ->  get_var(Env86, bindings, Bindings_Get35),
			get_var(Env86, u_ignore_slots, Ignore_slots_Get36),
			get_var(Env86, u_ob1, Ob1_Get33),
			get_var(Env86, u_ob2, Ob2_Get34),
			f_u_ob_c36_unify_var(Ob1_Get33,
					     Ob2_Get34,
					     Bindings_Get35,
					     Ignore_slots_Get36,
					     [],
					     TrueResult70),
			ElseResult65=TrueResult70
		    ;   f_u_var_c63(u_ob2, IFTEST37),
			(   IFTEST37\==[]
			->  get_var(Env86, bindings, Bindings_Get41),
			    get_var(Env86, u_ignore_slots, Ignore_slots_Get42),
			    get_var(Env86, u_ob1, Ob1_Get40),
			    get_var(Env86, u_ob2, Ob2_Get39),
			    f_u_ob_c36_unify_var(Ob2_Get39,
						 Ob1_Get40,
						 Bindings_Get41,
						 Ignore_slots_Get42,
						 t,
						 TrueResult68),
			    ElseResult65=TrueResult68
			;   f_u_ob_c63(u_ob1, IFTEST45),
			    (   IFTEST45\==[]
			    ->  get_var(Env86, u_ob1, Ob1_Get47),
				f_u_ob_c36_literal_c63(Ob1_Get47, TrueResult48),
				IFTEST43=TrueResult48
			    ;   IFTEST43=[]
			    ),
			    (   IFTEST43\==[]
			    ->  ElseResult65=[]
			    ;   f_u_ob_c63(u_ob2, IFTEST51),
				(   IFTEST51\==[]
				->  get_var(Env86, u_ob2, Ob2_Get53),
				    f_u_ob_c36_literal_c63(Ob2_Get53,
							   TrueResult54),
				    IFTEST49=TrueResult54
				;   IFTEST49=[]
				),
				(   IFTEST49\==[]
				->  ElseResult65=[]
				;   f_u_ob_c63(u_ob1, IFTEST57),
				    (   IFTEST57\==[]
				    ->  f_u_ob_c63(u_ob2, TrueResult59),
					IFTEST55=TrueResult59
				    ;   IFTEST55=[]
				    ),
				    (   IFTEST55\==[]
				    ->  f_u_yloop(
						  [ 
						    [ u_initial,
						      
						      [ u_unified_slot_indices,
							[]
						      ],
						      
						      [ u_ob2_slots,
							[u_ob_c36_pairs, u_ob2]
						      ],
						      
						      [ u_constant_slot_index,
							[]
						      ],
						      
						      [ u_last_constant_value,
							[]
						      ],
						      [u_new_bindings, []],
						      [u_found_c63, []]
						    ],
						    
						    [ u_yfor,
						      u_cur,
						      u_in,
						      [u_ob_c36_pairs, u_ob1]
						    ],
						    [u_ywhile, bindings],
						    
						    [ u_ydo,
						      
						      [ if,
							
							[ and,
							  
							  [ not,
							    
							    [ u_memq_c63,
							      [car, u_cur],
							      u_ignore_slots
							    ]
							  ],
							  
							  [ not,
							    
							    [ u_memq_c63,
							      [car, u_cur],
							      u_xx_permanent_ignore_slots_xx
							    ]
							  ]
							],
							
							[ progn,
							  
							  [ setq,
							    u_constant_slot_index,
							    0
							  ],
							  
							  [ setq,
							    u_new_bindings,
							    []
							  ],
							  
							  [ setq,
							    u_found_c63,
							    []
							  ],
							  
							  [ setq,
							    u_last_constant_value,
							    []
							  ],
							  
							  [ u_yloop,
							    
							    [ u_yfor,
							      u_constant_slot_value,
							      u_in,
							      u_ob2_slots
							    ],
							    
							    [ u_yuntil,
							      u_found_c63
							    ],
							    
							    [ u_ydo,
							      
							      [ if,
								
								[ and,
								  
								  [ u_eq_c63,
								    [car, u_cur],
								    
								    [ u_slots_name,
								      u_constant_slot_value
								    ]
								  ],
								  
								  [ not,
								    
								    [ u_memq_c63,
								      u_constant_slot_index,
								      u_unified_slot_indices
								    ]
								  ],
								  
								  [ setq,
								    u_last_constant_value,
								    
								    [ u_slots_value,
								      u_constant_slot_value
								    ]
								  ],
								  
								  [ setq,
								    u_new_bindings,
								    
								    [ if,
								      
								      [ u_eq_c63,
									[cadr, u_cur],
									
									[ u_slots_value,
									  u_constant_slot_value
									]
								      ],
								      bindings,
								      
								      [ u_ob_c36_unify2,
									[cadr, u_cur],
									
									[ u_slots_value,
									  u_constant_slot_value
									],
									bindings,
									u_ignore_slots
								      ]
								    ]
								  ]
								],
								
								[ progn,
								  
								  [ setq,
								    u_found_c63,
								    t
								  ],
								  
								  [ setq,
								    u_unified_slot_indices,
								    
								    [ cons,
								      u_constant_slot_index,
								      u_unified_slot_indices
								    ]
								  ]
								]
							      ],
							      
							      [ u_increment_me,
								u_constant_slot_index
							      ]
							    ]
							  ],
							  
							  [ if,
							    u_found_c63,
							    
							    [ setq,
							      bindings,
							      u_new_bindings
							    ],
							    
							    [ if,
							      u_xx_diff_c63_xx,
							      
							      [ setq,
								bindings,
								
								[ u_bd_bind,
								  
								  [ u_slots_name,
								    u_cur
								  ],
								  
								  [ list,
								    [cadr, u_cur],
								    u_last_constant_value
								  ],
								  bindings
								]
							      ],
							      [setq, bindings, []]
							    ]
							  ]
							]
						      ]
						    ],
						    [u_yresult, bindings]
						  ],
						  TrueResult64),
					ElseResult65=TrueResult64
				    ;   get_var(Env86, u_else, IFTEST60),
					(   IFTEST60\==[]
					->  ElseResult65=[]
					;   ElseResult65=[]
					)
				    )
				)
			    )
			)
		    )
		)
	    ),
	    LEnv=[bv(u_result, ElseResult65)|Env86],
	    get_var(LEnv, u_result, IFTEST77),
	    (   IFTEST77\==[]
	    ->  get_var(LEnv, u_result, Result_Get80),
		LetResult=Result_Get80
	    ;   f_u_bd_bind_c33(u_ob1,
				
				[ u_delq_c33,
				  u_ob2,
				  [u_bd_lookup, u_ob1, u_xx_already_matched_xx]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx92),
		f_u_bd_bind_c33(u_ob2,
				
				[ u_delq_c33,
				  u_ob1,
				  [u_bd_lookup, u_ob2, u_xx_already_matched_xx]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx93),
		LetResult=[]
	    )
	).
:- set_opv(f_u_ob_c36_unify0, classof, claz_function),
   set_opv(u_ob_c36_unify0, compile_as, kw_function),
   set_opv(u_ob_c36_unify0, function, f_u_ob_c36_unify0),
   _Ignored4=u_ob_c36_unify0.
/*
:- side_effect(assert_lsp(u_ob_c36_unify0,
			  wl:lambda_def(defun, u_ob_c36_unify0, f_u_ob_c36_unify0, [u_ob1, u_ob2, bindings, u_ignore_slots], [[if, [u_memq_c63, u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx]], bindings, [progn, [u_bd_bind_c33, u_ob1, [cons, u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx]], u_xx_already_matched_xx], [let, [[u_result, [cond, [[u_eq_c63, u_ob1, u_ob2], bindings], [[or, [u_special_c63, u_ob1], [u_special_c63, u_ob2]], [if, [u_special_priority_c63, u_ob1, u_ob2], [u_ob_c36_unify_special, u_ob1, u_ob2, bindings, u_ignore_slots, []], [u_ob_c36_unify_special, u_ob2, u_ob1, bindings, u_ignore_slots, t]]], [[u_var_c63, u_ob1], [u_ob_c36_unify_var, u_ob1, u_ob2, bindings, u_ignore_slots, []]], [[u_var_c63, u_ob2], [u_ob_c36_unify_var, u_ob2, u_ob1, bindings, u_ignore_slots, t]], [[and, [u_ob_c63, u_ob1], [u_ob_c36_literal_c63, u_ob1]], []], [[and, [u_ob_c63, u_ob2], [u_ob_c36_literal_c63, u_ob2]], []], [[and, [u_ob_c63, u_ob1], [u_ob_c63, u_ob2]], [u_yloop, [u_initial, [u_unified_slot_indices, []], [u_ob2_slots, [u_ob_c36_pairs, u_ob2]], [u_constant_slot_index, []], [u_last_constant_value, []], [u_new_bindings, []], [u_found_c63, []]], [u_yfor, u_cur, u_in, [u_ob_c36_pairs, u_ob1]], [u_ywhile, bindings], [u_ydo, [if, [and, [not, [u_memq_c63, [car, u_cur], u_ignore_slots]], [not, [u_memq_c63, [car, u_cur], u_xx_permanent_ignore_slots_xx]]], [progn, [setq, u_constant_slot_index, 0], [setq, u_new_bindings, []], [setq, u_found_c63, []], [setq, u_last_constant_value, []], [u_yloop, [u_yfor, u_constant_slot_value, u_in, u_ob2_slots], [u_yuntil, u_found_c63], [u_ydo, [if, [and, [u_eq_c63, [car, u_cur], [u_slots_name, u_constant_slot_value]], [not, [u_memq_c63, u_constant_slot_index, u_unified_slot_indices]], [setq, u_last_constant_value, [u_slots_value, u_constant_slot_value]], [setq, u_new_bindings, [if, [u_eq_c63, [cadr, u_cur], [u_slots_value, u_constant_slot_value]], bindings, [u_ob_c36_unify2, [cadr, u_cur], [u_slots_value, u_constant_slot_value], bindings, u_ignore_slots]]]], [progn, [setq, u_found_c63, t], [setq, u_unified_slot_indices, [cons, u_constant_slot_index, u_unified_slot_indices]]]], [u_increment_me, u_constant_slot_index]]], [if, u_found_c63, [setq, bindings, u_new_bindings], [if, u_xx_diff_c63_xx, [setq, bindings, [u_bd_bind, [u_slots_name, u_cur], [list, [cadr, u_cur], u_last_constant_value], bindings]], [setq, bindings, []]]]]]], [u_yresult, bindings]]], [u_else, []]]]], [if, u_result, u_result, [progn, [u_bd_bind_c33, u_ob1, [u_delq_c33, u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx]], u_xx_already_matched_xx], [u_bd_bind_c33, u_ob2, [u_delq_c33, u_ob1, [u_bd_lookup, u_ob2, u_xx_already_matched_xx]], u_xx_already_matched_xx], []]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify0,
			  wl:arglist_info(u_ob_c36_unify0, f_u_ob_c36_unify0, [u_ob1, u_ob2, bindings, u_ignore_slots], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify0,
			  wl:init_args(exact_only, f_u_ob_c36_unify0))).
*/
/*
 The below would introduce a semantics which does not conform
*/
/*
 to unification asymmetry.
*/
/*
       (bd-bind! ob2
*/
/*
                 (cons ob1 (bd-lookup ob2 *already-matched*))
*/
/*
                 *already-matched*)
*/
/*
 was reverse
*/
/*
(defun ob$unify-special (ob1 ob2 bindings ignore-slots reverse?)
  (cond
   ((ty$instance? ob1 'uand)
    (yloop (yfor item in (ob$gets ob1 'obj))
           (ywhile bindings)
           (ydo (setq bindings
                      (if reverse?
                          (ob$unify2 ob2 item bindings ignore-slots)
                          (ob$unify2 item ob2 bindings ignore-slots))))
           (yresult bindings)))
   ((ty$instance? ob1 'uor)
    (yloop (yfor item in (ob$gets ob1 'obj))
           (initial (new-bindings nil))
           (yuntil new-bindings)
           (ydo (setq new-bindings
                      (if reverse?
                          (ob$unify2 ob2 item bindings ignore-slots)
                          (ob$unify2 item ob2 bindings ignore-slots))))
           (yresult new-bindings)))
   ((ty$instance? ob1 'unot)
    (if (if reverse?
            (ob$unify2 ob2 (ob$get ob1 'obj) bindings ignore-slots)
            (ob$unify2 (ob$get ob1 'obj) ob2 bindings ignore-slots))
        nil
        bindings))
   ((ty$instance? ob1 'udist)
    (let ((val1 (if (not (var? (ob$get ob1 'obj)))
                    (ob$get ob1 'obj)
                    (bd-hyper-lookup (variable-name (ob$get ob1 'obj))
                                     bindings)))
          (val2 (if (not (var? ob2))
                    ob2
                    (bd-hyper-lookup (variable-name ob2) bindings))))
         (if (and (ob? val1) (ob? val2)
                  (not (var? val1)) (not (var? val2)))
             (if (neq? val1 val2) bindings nil)
             bindings)))
   ((ty$instance? ob1 'uproc)
    (if (eq? ob2 'uproc-answer-true)
        bindings
        (ob$unify-proc ob2 (ob$get ob1 'proc) bindings)))
   ((ty$instance? ob1 'uempty-slots)
    (if (every? (lambda (slot-name)
                 (null? (ob$gets ob2 slot-name)))
                (ob$get ob1 'slots))
        bindings
        nil))
   ((ty$instance? ob1 'uignore-slots)
    (if reverse?
        (ob$unify2 ob2 (ob$get ob1 'pattern) bindings
                   (append ignore-slots (ob$get ob1 'slots)))
        (ob$unify2 (ob$get ob1 'pattern) ob2 bindings
                   (append ignore-slots (ob$get ob1 'slots)))))
   ((ty$instance? ob1 'upath)
    (ob$path ob2 (ob$get ob1 'pattern)
                 (ob$get ob1 'path) bindings))
   ((ty$instance? ob1 'uolpath)
    (ol-path ob2 (ob$get ob1 'pattern) (ob$get ob1 'link)
                 (ob$get ob1 'direction)
                 *unify-context*
                 nil
                 bindings))
   ((ty$instance? ob1 'ueval)
    (ob$eval (ob$get ob1 'proc) bindings))
   ((ty$instance? ob1 'ucode)
    bindings) ; for now
   (else (error "ob$unify: unknown special!! "(defun ob$unify-special (ob1 ob2 bindings ignore-slots reverse?)\n  (cond\n   ((ty$instance? ob1 'uand)\n    (yloop (yfor item in (ob$gets ob1 'obj))\n           (ywhile bindings)\n           (ydo (setq bindings\n                      (if reverse?\n                          (ob$unify2 ob2 item bindings ignore-slots)\n                          (ob$unify2 item ob2 bindings ignore-slots))))\n           (yresult bindings)))\n   ((ty$instance? ob1 'uor)\n    (yloop (yfor item in (ob$gets ob1 'obj))\n           (initial (new-bindings nil))\n           (yuntil new-bindings)\n           (ydo (setq new-bindings\n                      (if reverse?\n                          (ob$unify2 ob2 item bindings ignore-slots)\n                          (ob$unify2 item ob2 bindings ignore-slots))))\n           (yresult new-bindings)))\n   ((ty$instance? ob1 'unot)\n    (if (if reverse?\n            (ob$unify2 ob2 (ob$get ob1 'obj) bindings ignore-slots)\n            (ob$unify2 (ob$get ob1 'obj) ob2 bindings ignore-slots))\n        nil\n        bindings))\n   ((ty$instance? ob1 'udist)\n    (let ((val1 (if (not (var? (ob$get ob1 'obj)))\n                    (ob$get ob1 'obj)\n                    (bd-hyper-lookup (variable-name (ob$get ob1 'obj))\n                                     bindings)))\n          (val2 (if (not (var? ob2))\n                    ob2\n                    (bd-hyper-lookup (variable-name ob2) bindings))))\n         (if (and (ob? val1) (ob? val2)\n                  (not (var? val1)) (not (var? val2)))\n             (if (neq? val1 val2) bindings nil)\n             bindings)))\n   ((ty$instance? ob1 'uproc)\n    (if (eq? ob2 'uproc-answer-true)\n        bindings\n        (ob$unify-proc ob2 (ob$get ob1 'proc) bindings)))\n   ((ty$instance? ob1 'uempty-slots)\n    (if (every? (lambda (slot-name)\n                 (null? (ob$gets ob2 slot-name)))\n                (ob$get ob1 'slots))\n        bindings\n        nil))\n   ((ty$instance? ob1 'uignore-slots)\n    (if reverse?\n        (ob$unify2 ob2 (ob$get ob1 'pattern) bindings\n                   (append ignore-slots (ob$get ob1 'slots)))\n        (ob$unify2 (ob$get ob1 'pattern) ob2 bindings\n                   (append ignore-slots (ob$get ob1 'slots)))))\n   ((ty$instance? ob1 'upath)\n    (ob$path ob2 (ob$get ob1 'pattern)\n                 (ob$get ob1 'path) bindings))\n   ((ty$instance? ob1 'uolpath)\n    (ol-path ob2 (ob$get ob1 'pattern) (ob$get ob1 'link)\n                 (ob$get ob1 'direction)\n                 *unify-context*\n                 nil\n                 bindings))\n   ((ty$instance? ob1 'ueval)\n    (ob$eval (ob$get ob1 'proc) bindings))\n   ((ty$instance? ob1 'ucode)\n    bindings) ; for now\n   (else (error \"ob$unify: unknown special!! ~A\" ob1))))\n\n; The (else t) above basically ignores prioritization of:\n; (ty$instance? ,ob1 'uempty-slots)\n; (ty$instance? ,ob1 'uignore-slots)\n; (ty$instance? ,ob1 'upath)\n; (ty$instance? ,ob1 'uolpath)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$unify-special',[ob1,ob2,bindings,'ignore-slots','reverse?'],[cond,[['ty$instance?',ob1,[quote,uand]],[yloop,[yfor,item,in,['ob$gets',ob1,[quote,obj]]],[ywhile,bindings],[ydo,[setq,bindings,[if,'reverse?',['ob$unify2',ob2,item,bindings,'ignore-slots'],['ob$unify2',item,ob2,bindings,'ignore-slots']]]],[yresult,bindings]]],[['ty$instance?',ob1,[quote,uor]],[yloop,[yfor,item,in,['ob$gets',ob1,[quote,obj]]],[initial,['new-bindings',[]]],[yuntil,'new-bindings'],[ydo,[setq,'new-bindings',[if,'reverse?',['ob$unify2',ob2,item,bindings,'ignore-slots'],['ob$unify2',item,ob2,bindings,'ignore-slots']]]],[yresult,'new-bindings']]],[['ty$instance?',ob1,[quote,unot]],[if,[if,'reverse?',['ob$unify2',ob2,['ob$get',ob1,[quote,obj]],bindings,'ignore-slots'],['ob$unify2',['ob$get',ob1,[quote,obj]],ob2,bindings,'ignore-slots']],[],bindings]],[['ty$instance?',ob1,[quote,udist]],[let,[[val1,[if,[not,['var?',['ob$get',ob1,[quote,obj]]]],['ob$get',ob1,[quote,obj]],['bd-hyper-lookup',['variable-name',['ob$get',ob1,[quote,obj]]],bindings]]],[val2,[if,[not,['var?',ob2]],ob2,['bd-hyper-lookup',['variable-name',ob2],bindings]]]],[if,[and,['ob?',val1],['ob?',val2],[not,['var?',val1]],[not,['var?',val2]]],[if,['neq?',val1,val2],bindings,[]],bindings]]],[['ty$instance?',ob1,[quote,uproc]],[if,['eq?',ob2,[quote,'uproc-answer-true']],bindings,['ob$unify-proc',ob2,['ob$get',ob1,[quote,proc]],bindings]]],[['ty$instance?',ob1,[quote,'uempty-slots']],[if,['every?',[lambda,['slot-name'],['null?',['ob$gets',ob2,'slot-name']]],['ob$get',ob1,[quote,slots]]],bindings,[]]],[['ty$instance?',ob1,[quote,'uignore-slots']],[if,'reverse?',['ob$unify2',ob2,['ob$get',ob1,[quote,pattern]],bindings,[append,'ignore-slots',['ob$get',ob1,[quote,slots]]]],['ob$unify2',['ob$get',ob1,[quote,pattern]],ob2,bindings,[append,'ignore-slots',['ob$get',ob1,[quote,slots]]]]]],[['ty$instance?',ob1,[quote,upath]],['ob$path',ob2,['ob$get',ob1,[quote,pattern]],['ob$get',ob1,[quote,path]],bindings]],[['ty$instance?',ob1,[quote,uolpath]],['ol-path',ob2,['ob$get',ob1,[quote,pattern]],['ob$get',ob1,[quote,link]],['ob$get',ob1,[quote,direction]],'*unify-context*',[],bindings]],[['ty$instance?',ob1,[quote,ueval]],['ob$eval',['ob$get',ob1,[quote,proc]],bindings]],[['ty$instance?',ob1,[quote,ucode]],bindings],[else,[error,'$STRING'("ob$unify: unknown special!! ~A"),ob1]]]])
wl:lambda_def(defun, u_ob_c36_unify_special, f_u_ob_c36_unify_special, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], [[cond, [[u_ty_c36_instance_c63, u_ob1, [quote, u_uand]], [u_yloop, [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]], [u_ywhile, bindings], [u_ydo, [setq, bindings, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, item, bindings, u_ignore_slots], [u_ob_c36_unify2, item, u_ob2, bindings, u_ignore_slots]]]], [u_yresult, bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uor]], [u_yloop, [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]], [u_initial, [u_new_bindings, []]], [u_yuntil, u_new_bindings], [u_ydo, [setq, u_new_bindings, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, item, bindings, u_ignore_slots], [u_ob_c36_unify2, item, u_ob2, bindings, u_ignore_slots]]]], [u_yresult, u_new_bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_unot]], [if, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_obj]], bindings, u_ignore_slots], [u_ob_c36_unify2, [u_ob_c36_get, u_ob1, [quote, u_obj]], u_ob2, bindings, u_ignore_slots]], [], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_udist]], [let, [[u_val1, [if, [not, [u_var_c63, [u_ob_c36_get, u_ob1, [quote, u_obj]]]], [u_ob_c36_get, u_ob1, [quote, u_obj]], [u_bd_hyper_lookup, [u_variable_name, [u_ob_c36_get, u_ob1, [quote, u_obj]]], bindings]]], [u_val2, [if, [not, [u_var_c63, u_ob2]], u_ob2, [u_bd_hyper_lookup, [u_variable_name, u_ob2], bindings]]]], [if, [and, [u_ob_c63, u_val1], [u_ob_c63, u_val2], [not, [u_var_c63, u_val1]], [not, [u_var_c63, u_val2]]], [if, [u_neq_c63, u_val1, u_val2], bindings, []], bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uproc]], [if, [u_eq_c63, u_ob2, [quote, u_uproc_answer_true]], bindings, [u_ob_c36_unify_proc, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_proc]], bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uempty_slots]], [if, [u_every_c63, [lambda, [u_slot_name], [u_null_c63, [u_ob_c36_gets, u_ob2, u_slot_name]]], [u_ob_c36_get, u_ob1, [quote, sys_slots]]], bindings, []]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uignore_slots]], [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], bindings, [append, u_ignore_slots, [u_ob_c36_get, u_ob1, [quote, sys_slots]]]], [u_ob_c36_unify2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], u_ob2, bindings, [append, u_ignore_slots, [u_ob_c36_get, u_ob1, [quote, sys_slots]]]]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_upath]], [u_ob_c36_path, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], [u_ob_c36_get, u_ob1, [quote, u_path]], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uolpath]], [u_ol_path, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], [u_ob_c36_get, u_ob1, [quote, u_link]], [u_ob_c36_get, u_ob1, [quote, u_direction]], u_xx_unify_context_xx, [], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_ueval]], [u_ob_c36_eval, [u_ob_c36_get, u_ob1, [quote, u_proc]], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_ucode]], bindings], [u_else, [error, '$ARRAY'([*], claz_base_character, "ob$unify: unknown special!! ~A"), u_ob1]]]]).
wl:arglist_info(u_ob_c36_unify_special, f_u_ob_c36_unify_special, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify_special).

/*

### Compiled:  `U::OB$UNIFY-SPECIAL` 
*/
f_u_ob_c36_unify_special(Ob1, Ob2, Bindings, Ignore_slots, Reverse_c63, TrueResult62) :-
	nop(global_env(Env)),
	Env145=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots), bv(u_reverse_c63, Reverse_c63)|Env],
	get_var(Env145, u_ob1, Ob1_Get),
	f_u_ty_c36_instance_c63(Ob1_Get, u_uand, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]],
			[u_ywhile, bindings],
			
			[ u_ydo,
			  
			  [ setq,
			    bindings,
			    
			    [ if,
			      u_reverse_c63,
			      
			      [ u_ob_c36_unify2,
				u_ob2,
				item,
				bindings,
				u_ignore_slots
			      ],
			      
			      [ u_ob_c36_unify2,
				item,
				u_ob2,
				bindings,
				u_ignore_slots
			      ]
			    ]
			  ]
			],
			[u_yresult, bindings]
		      ],
		      TrueResult141),
	    TrueResult62=TrueResult141
	;   get_var(Env145, u_ob1, Ob1_Get12),
	    f_u_ty_c36_instance_c63(Ob1_Get12, u_uor, IFTEST10),
	    (   IFTEST10\==[]
	    ->  f_u_yloop(
			  [ 
			    [ u_yfor,
			      item,
			      u_in,
			      [u_ob_c36_gets, u_ob1, [quote, u_obj]]
			    ],
			    [u_initial, [u_new_bindings, []]],
			    [u_yuntil, u_new_bindings],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_new_bindings,
				
				[ if,
				  u_reverse_c63,
				  
				  [ u_ob_c36_unify2,
				    u_ob2,
				    item,
				    bindings,
				    u_ignore_slots
				  ],
				  
				  [ u_ob_c36_unify2,
				    item,
				    u_ob2,
				    bindings,
				    u_ignore_slots
				  ]
				]
			      ]
			    ],
			    [u_yresult, u_new_bindings]
			  ],
			  TrueResult139),
		TrueResult62=TrueResult139
	    ;   get_var(Env145, u_ob1, Ob1_Get15),
		f_u_ty_c36_instance_c63(Ob1_Get15, u_unot, IFTEST13),
		(   IFTEST13\==[]
		->  get_var(Env145, u_reverse_c63, IFTEST18),
		    (   IFTEST18\==[]
		    ->  f_u_ob_c36_unify2(u_ob2,
					  [u_ob_c36_get, u_ob1, [quote, u_obj]],
					  bindings,
					  u_ignore_slots,
					  TrueResult),
			IFTEST16=TrueResult
		    ;   f_u_ob_c36_unify2([u_ob_c36_get, u_ob1, [quote, u_obj]],
					  u_ob2,
					  bindings,
					  u_ignore_slots,
					  ElseResult),
			IFTEST16=ElseResult
		    ),
		    (   IFTEST16\==[]
		    ->  TrueResult62=[]
		    ;   get_var(Env145, bindings, Bindings_Get),
			TrueResult62=Bindings_Get
		    )
		;   get_var(Env145, u_ob1, Ob1_Get27),
		    f_u_ty_c36_instance_c63(Ob1_Get27, u_udist, IFTEST25),
		    (   IFTEST25\==[]
		    ->  f_u_var_c63([u_ob_c36_get, u_ob1, [quote, u_obj]],
				    PredArgResult),
			(   PredArgResult==[]
			->  get_var(Env145, u_ob1, Ob1_Get34),
			    f_u_ob_c36_get(Ob1_Get34, u_obj, TrueResult35),
			    Val1_Init=TrueResult35
			;   f_u_bd_hyper_lookup(
						[ u_variable_name,
						  
						  [ u_ob_c36_get,
						    u_ob1,
						    [quote, u_obj]
						  ]
						],
						bindings,
						ElseResult36),
			    Val1_Init=ElseResult36
			),
			f_u_var_c63(u_ob2, PredArgResult39),
			(   PredArgResult39==[]
			->  get_var(Env145, u_ob2, Ob2_Get),
			    Val2_Init=Ob2_Get
			;   f_u_bd_hyper_lookup([u_variable_name, u_ob2],
						bindings,
						ElseResult42),
			    Val2_Init=ElseResult42
			),
			LEnv=[bv(u_val1, Val1_Init), bv(u_val2, Val2_Init)|Env145],
			f_u_ob_c63(u_val1, IFTEST47),
			(   IFTEST47\==[]
			->  f_u_ob_c63(u_val2, IFTEST49),
			    (   IFTEST49\==[]
			    ->  f_u_var_c63(u_val1, PredArgResult53),
				(   PredArgResult53==[]
				->  f_u_var_c63(u_val2, Not_Param),
				    cl_not(Not_Param, TrueResult54),
				    IFTEST45=TrueResult54
				;   IFTEST45=[]
				)
			    ;   IFTEST45=[]
			    )
			;   IFTEST45=[]
			),
			(   IFTEST45\==[]
			->  f_u_neq_c63(u_val1, u_val2, IFTEST57),
			    (   IFTEST57\==[]
			    ->  get_var(LEnv, bindings, Bindings_Get59),
				TrueResult62=Bindings_Get59
			    ;   TrueResult62=[]
			    )
			;   get_var(LEnv, bindings, Bindings_Get61),
			    TrueResult62=Bindings_Get61
			)
		    ;   get_var(Env145, u_ob1, Ob1_Get66),
			f_u_ty_c36_instance_c63(Ob1_Get66, u_uproc, IFTEST64),
			(   IFTEST64\==[]
			->  f_u_eq_c63(u_ob2,
				       [quote, u_uproc_answer_true],
				       IFTEST67),
			    (   IFTEST67\==[]
			    ->  get_var(Env145, bindings, Bindings_Get69),
				TrueResult62=Bindings_Get69
			    ;   get_var(Env145, u_ob1, Ob1_Get71),
				get_var(Env145, u_ob2, Ob2_Get70),
				f_u_ob_c36_get(Ob1_Get71, u_proc, Proc),
				get_var(Env145, bindings, Bindings_Get72),
				f_u_ob_c36_unify_proc(Ob2_Get70,
						      Proc,
						      Bindings_Get72,
						      ElseResult74),
				TrueResult62=ElseResult74
			    )
			;   get_var(Env145, u_ob1, Ob1_Get77),
			    f_u_ty_c36_instance_c63(Ob1_Get77,
						    u_uempty_slots,
						    IFTEST75),
			    (   IFTEST75\==[]
			    ->  f_u_every_c63(
					      [ lambda,
						[u_slot_name],
						
						[ u_null_c63,
						  
						  [ u_ob_c36_gets,
						    u_ob2,
						    u_slot_name
						  ]
						]
					      ],
					      
					      [ u_ob_c36_get,
						u_ob1,
						[quote, sys_slots]
					      ],
					      IFTEST78),
				(   IFTEST78\==[]
				->  get_var(Env145, bindings, Bindings_Get80),
				    TrueResult62=Bindings_Get80
				;   TrueResult62=[]
				)
			    ;   get_var(Env145, u_ob1, Ob1_Get84),
				f_u_ty_c36_instance_c63(Ob1_Get84,
							u_uignore_slots,
							IFTEST82),
				(   IFTEST82\==[]
				->  get_var(Env145, u_reverse_c63, IFTEST85),
				    (   IFTEST85\==[]
				    ->  f_u_ob_c36_unify2(u_ob2,
							  
							  [ u_ob_c36_get,
							    u_ob1,
							    [quote, u_pattern]
							  ],
							  bindings,
							  
							  [ append,
							    u_ignore_slots,
							    
							    [ u_ob_c36_get,
							      u_ob1,
							      [quote, sys_slots]
							    ]
							  ],
							  TrueResult88),
					TrueResult62=TrueResult88
				    ;   f_u_ob_c36_unify2(
							  [ u_ob_c36_get,
							    u_ob1,
							    [quote, u_pattern]
							  ],
							  u_ob2,
							  bindings,
							  
							  [ append,
							    u_ignore_slots,
							    
							    [ u_ob_c36_get,
							      u_ob1,
							      [quote, sys_slots]
							    ]
							  ],
							  ElseResult89),
					TrueResult62=ElseResult89
				    )
				;   get_var(Env145, u_ob1, Ob1_Get92),
				    f_u_ty_c36_instance_c63(Ob1_Get92,
							    u_upath,
							    IFTEST90),
				    (   IFTEST90\==[]
				    ->  get_var(Env145, u_ob1, Ob1_Get94),
					get_var(Env145, u_ob2, Ob2_Get93),
					f_u_ob_c36_get(Ob1_Get94,
						       u_pattern,
						       Pattern),
					get_var(Env145, u_ob1, Ob1_Get95),
					f_u_ob_c36_get(Ob1_Get95, u_path, Path),
					get_var(Env145,
						bindings,
						Bindings_Get96),
					f_u_ob_c36_path(Ob2_Get93,
							Pattern,
							Path,
							Bindings_Get96,
							TrueResult127),
					TrueResult62=TrueResult127
				    ;   get_var(Env145, u_ob1, Ob1_Get99),
					f_u_ty_c36_instance_c63(Ob1_Get99,
								u_uolpath,
								IFTEST97),
					(   IFTEST97\==[]
					->  get_var(Env145, u_ob1, Ob1_Get101),
					    get_var(Env145, u_ob2, Ob2_Get100),
					    f_u_ob_c36_get(Ob1_Get101,
							   u_pattern,
							   Pattern154),
					    get_var(Env145, u_ob1, Ob1_Get102),
					    f_u_ob_c36_get(Ob1_Get102,
							   u_link,
							   Link),
					    get_var(Env145, u_ob1, Ob1_Get103),
					    f_u_ob_c36_get(Ob1_Get103,
							   u_direction,
							   Direction),
					    get_var(Env145,
						    bindings,
						    Bindings_Get105),
					    get_var(Env145,
						    u_xx_unify_context_xx,
						    Xx_unify_context_xx_Get),
					    f_u_ol_path(Ob2_Get100,
							Pattern154,
							Link,
							Direction,
							Xx_unify_context_xx_Get,
							[],
							Bindings_Get105,
							TrueResult125),
					    TrueResult62=TrueResult125
					;   get_var(Env145, u_ob1, Ob1_Get108),
					    f_u_ty_c36_instance_c63(Ob1_Get108,
								    u_ueval,
								    IFTEST106),
					    (   IFTEST106\==[]
					    ->  get_var(Env145,
							u_ob1,
							Ob1_Get109),
						f_u_ob_c36_get(Ob1_Get109,
							       u_proc,
							       Proc157),
						get_var(Env145,
							bindings,
							Bindings_Get110),
						f_u_ob_c36_eval(Proc157,
								Bindings_Get110,
								TrueResult123),
						TrueResult62=TrueResult123
					    ;   get_var(Env145,
							u_ob1,
							Ob1_Get113),
						f_u_ty_c36_instance_c63(Ob1_Get113,
									u_ucode,
									IFTEST111),
						(   IFTEST111\==[]
						->  get_var(Env145,
							    bindings,
							    Bindings_Get114),
						    TrueResult62=Bindings_Get114
						;   get_var(Env145,
							    u_else,
							    IFTEST115),
						    (   IFTEST115\==[]
						    ->  get_var(Env145,
								u_ob1,
								Ob1_Get118),
							cl_error(
								 [ '$ARRAY'([*],
									    claz_base_character,
									    "ob$unify: unknown special!! ~A"),
								   Ob1_Get118
								 ],
								 TrueResult119),
							TrueResult62=TrueResult119
						    ;   TrueResult62=[]
						    )
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_unify_special, classof, claz_function),
   set_opv(u_ob_c36_unify_special, compile_as, kw_function),
   set_opv(u_ob_c36_unify_special, function, f_u_ob_c36_unify_special),
   _Ignored4=u_ob_c36_unify_special.
/*
:- side_effect(assert_lsp(u_ob_c36_unify_special,
			  wl:lambda_def(defun, u_ob_c36_unify_special, f_u_ob_c36_unify_special, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], [[cond, [[u_ty_c36_instance_c63, u_ob1, [quote, u_uand]], [u_yloop, [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]], [u_ywhile, bindings], [u_ydo, [setq, bindings, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, item, bindings, u_ignore_slots], [u_ob_c36_unify2, item, u_ob2, bindings, u_ignore_slots]]]], [u_yresult, bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uor]], [u_yloop, [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]], [u_initial, [u_new_bindings, []]], [u_yuntil, u_new_bindings], [u_ydo, [setq, u_new_bindings, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, item, bindings, u_ignore_slots], [u_ob_c36_unify2, item, u_ob2, bindings, u_ignore_slots]]]], [u_yresult, u_new_bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_unot]], [if, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_obj]], bindings, u_ignore_slots], [u_ob_c36_unify2, [u_ob_c36_get, u_ob1, [quote, u_obj]], u_ob2, bindings, u_ignore_slots]], [], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_udist]], [let, [[u_val1, [if, [not, [u_var_c63, [u_ob_c36_get, u_ob1, [quote, u_obj]]]], [u_ob_c36_get, u_ob1, [quote, u_obj]], [u_bd_hyper_lookup, [u_variable_name, [u_ob_c36_get, u_ob1, [quote, u_obj]]], bindings]]], [u_val2, [if, [not, [u_var_c63, u_ob2]], u_ob2, [u_bd_hyper_lookup, [u_variable_name, u_ob2], bindings]]]], [if, [and, [u_ob_c63, u_val1], [u_ob_c63, u_val2], [not, [u_var_c63, u_val1]], [not, [u_var_c63, u_val2]]], [if, [u_neq_c63, u_val1, u_val2], bindings, []], bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uproc]], [if, [u_eq_c63, u_ob2, [quote, u_uproc_answer_true]], bindings, [u_ob_c36_unify_proc, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_proc]], bindings]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uempty_slots]], [if, [u_every_c63, [lambda, [u_slot_name], [u_null_c63, [u_ob_c36_gets, u_ob2, u_slot_name]]], [u_ob_c36_get, u_ob1, [quote, sys_slots]]], bindings, []]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uignore_slots]], [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], bindings, [append, u_ignore_slots, [u_ob_c36_get, u_ob1, [quote, sys_slots]]]], [u_ob_c36_unify2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], u_ob2, bindings, [append, u_ignore_slots, [u_ob_c36_get, u_ob1, [quote, sys_slots]]]]]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_upath]], [u_ob_c36_path, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], [u_ob_c36_get, u_ob1, [quote, u_path]], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_uolpath]], [u_ol_path, u_ob2, [u_ob_c36_get, u_ob1, [quote, u_pattern]], [u_ob_c36_get, u_ob1, [quote, u_link]], [u_ob_c36_get, u_ob1, [quote, u_direction]], u_xx_unify_context_xx, [], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_ueval]], [u_ob_c36_eval, [u_ob_c36_get, u_ob1, [quote, u_proc]], bindings]], [[u_ty_c36_instance_c63, u_ob1, [quote, u_ucode]], bindings], [u_else, [error, '$ARRAY'([*], claz_base_character, "ob$unify: unknown special!! ~A"), u_ob1]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_special,
			  wl:arglist_info(u_ob_c36_unify_special, f_u_ob_c36_unify_special, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_special,
			  wl:init_args(exact_only, f_u_ob_c36_unify_special))).
*/
/*
 for now
*/
/*
 The (else t) above basically ignores prioritization of:
*/
/*
 (ty$instance? ,ob1 'uempty-slots)
*/
/*
 (ty$instance? ,ob1 'uignore-slots)
*/
/*
 (ty$instance? ,ob1 'upath)
*/
/*
 (ty$instance? ,ob1 'uolpath)
*/
/*
(defun ob$unify-proc (ob2 proc bd)
 (setq ob2 (ob$concretize ob2 bd))
 (if (concretized? ob2)
     (if (funcall proc ob2)
         bd
         nil)
     bd))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11012 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$unify-proc',[ob2,proc,bd],[setq,ob2,['ob$concretize',ob2,bd]],[if,['concretized?',ob2],[if,[funcall,proc,ob2],bd,[]],bd]])
wl:lambda_def(defun, u_ob_c36_unify_proc, f_u_ob_c36_unify_proc, [u_ob2, u_proc, u_bd], [[setq, u_ob2, [u_ob_c36_concretize, u_ob2, u_bd]], [if, [u_concretized_c63, u_ob2], [if, [funcall, u_proc, u_ob2], u_bd, []], u_bd]]).
wl:arglist_info(u_ob_c36_unify_proc, f_u_ob_c36_unify_proc, [u_ob2, u_proc, u_bd], arginfo{all:[u_ob2, u_proc, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob2, u_proc, u_bd], opt:0, req:[u_ob2, u_proc, u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify_proc).

/*

### Compiled:  `U::OB$UNIFY-PROC` 
*/
f_u_ob_c36_unify_proc(Ob2, Proc, Bd, TrueResult20) :-
	nop(global_env(Env)),
	AEnv=[bv(u_ob2, Ob2), bv(u_proc, Proc), bv(u_bd, Bd)|Env],
	get_var(AEnv, u_bd, Bd_Get),
	get_var(AEnv, u_ob2, Ob2_Get),
	f_u_ob_c36_concretize(Ob2_Get, Bd_Get, Ob227),
	set_var(AEnv, u_ob2, Ob227),
	get_var(AEnv, u_ob2, Ob2_Get12),
	f_u_concretized_c63(Ob2_Get12, IFTEST),
	(   IFTEST\==[]
	->  get_var(AEnv, u_ob2, Ob2_Get16),
	    get_var(AEnv, u_proc, Proc_Get),
	    cl_apply(Proc_Get, [Ob2_Get16], IFTEST13),
	    (   IFTEST13\==[]
	    ->  get_var(AEnv, u_bd, Bd_Get17),
		TrueResult20=Bd_Get17
	    ;   TrueResult20=[]
	    )
	;   get_var(AEnv, u_bd, Bd_Get19),
	    TrueResult20=Bd_Get19
	).
:- set_opv(f_u_ob_c36_unify_proc, classof, claz_function),
   set_opv(u_ob_c36_unify_proc, compile_as, kw_function),
   set_opv(u_ob_c36_unify_proc, function, f_u_ob_c36_unify_proc),
   _Ignored4=u_ob_c36_unify_proc.
/*
:- side_effect(assert_lsp(u_ob_c36_unify_proc,
			  wl:lambda_def(defun, u_ob_c36_unify_proc, f_u_ob_c36_unify_proc, [u_ob2, u_proc, u_bd], [[setq, u_ob2, [u_ob_c36_concretize, u_ob2, u_bd]], [if, [u_concretized_c63, u_ob2], [if, [funcall, u_proc, u_ob2], u_bd, []], u_bd]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_proc,
			  wl:arglist_info(u_ob_c36_unify_proc, f_u_ob_c36_unify_proc, [u_ob2, u_proc, u_bd], arginfo{all:[u_ob2, u_proc, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob2, u_proc, u_bd], opt:0, req:[u_ob2, u_proc, u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_proc,
			  wl:init_args(exact_only, f_u_ob_c36_unify_proc))).
*/
/*
(defun ob$concretize (ob bd)
 (cond
  ((var? ob)
   (ob$concretize-var ob bd))
  ((and (ob? ob)
        (ty$instance? ob 'uand))
   (ob$concretize-and ob bd))
  (else ob)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11171 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$concretize',[ob,bd],[cond,[['var?',ob],['ob$concretize-var',ob,bd]],[[and,['ob?',ob],['ty$instance?',ob,[quote,uand]]],['ob$concretize-and',ob,bd]],[else,ob]]])
wl:lambda_def(defun, u_ob_c36_concretize, f_u_ob_c36_concretize, [u_ob, u_bd], [[cond, [[u_var_c63, u_ob], [u_ob_c36_concretize_var, u_ob, u_bd]], [[and, [u_ob_c63, u_ob], [u_ty_c36_instance_c63, u_ob, [quote, u_uand]]], [u_ob_c36_concretize_and, u_ob, u_bd]], [u_else, u_ob]]]).
wl:arglist_info(u_ob_c36_concretize, f_u_ob_c36_concretize, [u_ob, u_bd], arginfo{all:[u_ob, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_bd], opt:0, req:[u_ob, u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_concretize).

/*

### Compiled:  `U::OB$CONCRETIZE` 
*/
f_u_ob_c36_concretize(Ob, Bd, ElseResult26) :-
	nop(global_env(Env)),
	Env31=[bv(u_ob, Ob), bv(u_bd, Bd)|Env],
	f_u_var_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env31, u_bd, Bd_Get),
	    get_var(Env31, u_ob, Ob_Get),
	    f_u_ob_c36_concretize_var(Ob_Get, Bd_Get, TrueResult27),
	    ElseResult26=TrueResult27
	;   f_u_ob_c63(u_ob, IFTEST13),
	    (   IFTEST13\==[]
	    ->  get_var(Env31, u_ob, Ob_Get15),
		f_u_ty_c36_instance_c63(Ob_Get15, u_uand, TrueResult),
		IFTEST11=TrueResult
	    ;   IFTEST11=[]
	    ),
	    (   IFTEST11\==[]
	    ->  get_var(Env31, u_bd, Bd_Get18),
		get_var(Env31, u_ob, Ob_Get17),
		f_u_ob_c36_concretize_and(Ob_Get17, Bd_Get18, TrueResult25),
		ElseResult26=TrueResult25
	    ;   get_var(Env31, u_else, IFTEST19),
		(   IFTEST19\==[]
		->  get_var(Env31, u_ob, Ob_Get22),
		    ElseResult26=Ob_Get22
		;   ElseResult26=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_concretize, classof, claz_function),
   set_opv(u_ob_c36_concretize, compile_as, kw_function),
   set_opv(u_ob_c36_concretize, function, f_u_ob_c36_concretize),
   _Ignored4=u_ob_c36_concretize.
/*
:- side_effect(assert_lsp(u_ob_c36_concretize,
			  wl:lambda_def(defun, u_ob_c36_concretize, f_u_ob_c36_concretize, [u_ob, u_bd], [[cond, [[u_var_c63, u_ob], [u_ob_c36_concretize_var, u_ob, u_bd]], [[and, [u_ob_c63, u_ob], [u_ty_c36_instance_c63, u_ob, [quote, u_uand]]], [u_ob_c36_concretize_and, u_ob, u_bd]], [u_else, u_ob]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concretize,
			  wl:arglist_info(u_ob_c36_concretize, f_u_ob_c36_concretize, [u_ob, u_bd], arginfo{all:[u_ob, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_bd], opt:0, req:[u_ob, u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concretize,
			  wl:init_args(exact_only, f_u_ob_c36_concretize))).
*/
/*
(defun ob$concretize-and (and-ptn bd)
  (yloop (yfor item in (ob$gets and-ptn 'obj))
         (initial (result nil))
         (yuntil result)
         (ydo (if (var? item)
                  (setq result (ob$concretize-var item bd))))
         (yresult (if (null? result)
                      (progn
                       (format *gate-output*
                               "Warning: ob$concretize-and unsuccessful."(defun ob$concretize-and (and-ptn bd)\n  (yloop (yfor item in (ob$gets and-ptn 'obj))\n         (initial (result nil))\n         (yuntil result)\n         (ydo (if (var? item)\n                  (setq result (ob$concretize-var item bd))))\n         (yresult (if (null? result)\n                      (progn\n                       (format *gate-output*\n                               \"Warning: ob$concretize-and unsuccessful.~%\")\n                       and-ptn)\n                      result))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11345 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$concretize-and',['and-ptn',bd],[yloop,[yfor,item,in,['ob$gets','and-ptn',[quote,obj]]],[initial,[result,[]]],[yuntil,result],[ydo,[if,['var?',item],[setq,result,['ob$concretize-var',item,bd]]]],[yresult,[if,['null?',result],[progn,[format,'*gate-output*','$STRING'("Warning: ob$concretize-and unsuccessful.~%")],'and-ptn'],result]]]])
wl:lambda_def(defun, u_ob_c36_concretize_and, f_u_ob_c36_concretize_and, [u_and_ptn, u_bd], [[u_yloop, [u_yfor, item, u_in, [u_ob_c36_gets, u_and_ptn, [quote, u_obj]]], [u_initial, [u_result, []]], [u_yuntil, u_result], [u_ydo, [if, [u_var_c63, item], [setq, u_result, [u_ob_c36_concretize_var, item, u_bd]]]], [u_yresult, [if, [u_null_c63, u_result], [progn, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Warning: ob$concretize-and unsuccessful.~%")], u_and_ptn], u_result]]]]).
wl:arglist_info(u_ob_c36_concretize_and, f_u_ob_c36_concretize_and, [u_and_ptn, u_bd], arginfo{all:[u_and_ptn, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_and_ptn, u_bd], opt:0, req:[u_and_ptn, u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_concretize_and).

/*

### Compiled:  `U::OB$CONCRETIZE-AND` 
*/
f_u_ob_c36_concretize_and(And_ptn, Bd, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_and_ptn, And_ptn), bv(u_bd, Bd)|Env],
	f_u_yloop(
		  [ [u_yfor, item, u_in, [u_ob_c36_gets, u_and_ptn, [quote, u_obj]]],
		    [u_initial, [u_result, []]],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_var_c63, item],
			[setq, u_result, [u_ob_c36_concretize_var, item, u_bd]]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ if,
			[u_null_c63, u_result],
			
			[ progn,
			  
			  [ format,
			    u_xx_gate_output_xx,
			    '$ARRAY'([*],
				     claz_base_character,
				     "Warning: ob$concretize-and unsuccessful.~%")
			  ],
			  u_and_ptn
			],
			u_result
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_concretize_and, classof, claz_function),
   set_opv(u_ob_c36_concretize_and, compile_as, kw_function),
   set_opv(u_ob_c36_concretize_and, function, f_u_ob_c36_concretize_and),
   _Ignored4=u_ob_c36_concretize_and.
/*
:- side_effect(assert_lsp(u_ob_c36_concretize_and,
			  wl:lambda_def(defun, u_ob_c36_concretize_and, f_u_ob_c36_concretize_and, [u_and_ptn, u_bd], [[u_yloop, [u_yfor, item, u_in, [u_ob_c36_gets, u_and_ptn, [quote, u_obj]]], [u_initial, [u_result, []]], [u_yuntil, u_result], [u_ydo, [if, [u_var_c63, item], [setq, u_result, [u_ob_c36_concretize_var, item, u_bd]]]], [u_yresult, [if, [u_null_c63, u_result], [progn, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Warning: ob$concretize-and unsuccessful.~%")], u_and_ptn], u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concretize_and,
			  wl:arglist_info(u_ob_c36_concretize_and, f_u_ob_c36_concretize_and, [u_and_ptn, u_bd], arginfo{all:[u_and_ptn, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_and_ptn, u_bd], opt:0, req:[u_and_ptn, u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concretize_and,
			  wl:init_args(exact_only, f_u_ob_c36_concretize_and))).
*/
/*
(defun ob$concretize-var (var bd)
  (let ((found (bd-lookup (variable-name var) bd)))
    (if found found var)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11833 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$concretize-var',[var,bd],[let,[[found,['bd-lookup',['variable-name',var],bd]]],[if,found,found,var]]])
wl:lambda_def(defun, u_ob_c36_concretize_var, f_u_ob_c36_concretize_var, [u_var, u_bd], [[let, [[u_found, [u_bd_lookup, [u_variable_name, u_var], u_bd]]], [if, u_found, u_found, u_var]]]).
wl:arglist_info(u_ob_c36_concretize_var, f_u_ob_c36_concretize_var, [u_var, u_bd], arginfo{all:[u_var, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd], opt:0, req:[u_var, u_bd], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_concretize_var).

/*

### Compiled:  `U::OB$CONCRETIZE-VAR` 
*/
f_u_ob_c36_concretize_var(Var, Bd, FnResult) :-
	nop(global_env(Env)),
	Env20=[bv(u_var, Var), bv(u_bd, Bd)|Env],
	f_u_bd_lookup([u_variable_name, u_var], u_bd, Found_Init),
	LEnv=[bv(u_found, Found_Init)|Env20],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get14),
	    FnResult=Found_Get14
	;   get_var(LEnv, u_var, Var_Get),
	    FnResult=Var_Get
	).
:- set_opv(f_u_ob_c36_concretize_var, classof, claz_function),
   set_opv(u_ob_c36_concretize_var, compile_as, kw_function),
   set_opv(u_ob_c36_concretize_var, function, f_u_ob_c36_concretize_var),
   _Ignored4=u_ob_c36_concretize_var.
/*
:- side_effect(assert_lsp(u_ob_c36_concretize_var,
			  wl:lambda_def(defun, u_ob_c36_concretize_var, f_u_ob_c36_concretize_var, [u_var, u_bd], [[let, [[u_found, [u_bd_lookup, [u_variable_name, u_var], u_bd]]], [if, u_found, u_found, u_var]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concretize_var,
			  wl:arglist_info(u_ob_c36_concretize_var, f_u_ob_c36_concretize_var, [u_var, u_bd], arginfo{all:[u_var, u_bd], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var, u_bd], opt:0, req:[u_var, u_bd], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concretize_var,
			  wl:init_args(exact_only, f_u_ob_c36_concretize_var))).
*/
/*
(defun concretized? (var)
  (not (var? var)))

;
; Question mark atom should never get to here.
;
; This routine no longer checks if the types match right even if the variable
; is already bound. This used to be used to handle prebound typed ?Self, but
; now the self-type slot of rules serves this function.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'concretized?',[var],[not,['var?',var]]])
wl:lambda_def(defun, u_concretized_c63, f_u_concretized_c63, [u_var], [[not, [u_var_c63, u_var]]]).
wl:arglist_info(u_concretized_c63, f_u_concretized_c63, [u_var], arginfo{all:[u_var], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var], opt:0, req:[u_var], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_concretized_c63).

/*

### Compiled:  `U::CONCRETIZED?` 
*/
f_u_concretized_c63(Var, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_var, Var)|Env],
	f_u_var_c63(u_var, Not_Param),
	cl_not(Not_Param, Not_Ret),
	Not_Ret=FnResult.
:- set_opv(f_u_concretized_c63, classof, claz_function),
   set_opv(u_concretized_c63, compile_as, kw_function),
   set_opv(u_concretized_c63, function, f_u_concretized_c63),
   _Ignored4=u_concretized_c63.
/*
:- side_effect(assert_lsp(u_concretized_c63,
			  wl:lambda_def(defun, u_concretized_c63, f_u_concretized_c63, [u_var], [[not, [u_var_c63, u_var]]]))).
*/
/*
:- side_effect(assert_lsp(u_concretized_c63,
			  wl:arglist_info(u_concretized_c63, f_u_concretized_c63, [u_var], arginfo{all:[u_var], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_var], opt:0, req:[u_var], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_concretized_c63,
			  wl:init_args(exact_only, f_u_concretized_c63))).
*/
/*
*/
/*
 Question mark atom should never get to here.
*/
/*
*/
/*
 This routine no longer checks if the types match right even if the variable
*/
/*
 is already bound. This used to be used to handle prebound typed ?Self, but
*/
/*
 now the self-type slot of rules serves this function.
*/
/*
*/
/*
(defun ob$unify-var (ob1 ob2 bindings ignore-slots reverse?)
  (let ((val1 (bd-lookup (variable-name ob1) bindings))
        (val2 nil))
; was  (if val1 (setq ob1 val1))
       (if (and val1 (not (var? val1)))
           (setq ob1 val1))
       (if (var? ob2)
           (progn
            (setq val2 (bd-lookup (variable-name ob2) bindings))
            (if (and val2 (not (var? val2)))
                (setq ob2 val2))))
; was       (if val2 (setq ob2 val2))
       (cond
        ((and (var? ob1) (var? ob2))
         (if (type-compatible-vars? ob1 ob2)
             (if *diff?*
                 bindings
                 (bd-bind (variable-name ob2) ob1
                          (bd-bind (variable-name ob1) ob2 bindings)))
             nil))
        ((var? ob1)
         (if (var-ty$instance? ob2 (variable-type ob1))
             (if *diff?*
                 bindings
                 (bd-bind (variable-name ob1) ob2 bindings))
             nil))
        ((var? ob2)
         (if (var-ty$instance? ob1 (variable-type ob2))
             (if *diff?*
                 bindings
                 (bd-bind (variable-name ob2) ob1 bindings))
             nil))
        (else ;(and (not (var? ob1)) (not (var? ob2)))
         (if reverse?
             (ob$unify2 ob2 ob1 bindings ignore-slots)
             (ob$unify2 ob1 ob2 bindings ignore-slots))))))

;
; This is the old incomprehensible version.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$unify-var',[ob1,ob2,bindings,'ignore-slots','reverse?'],[let,[[val1,['bd-lookup',['variable-name',ob1],bindings]],[val2,[]]],[if,[and,val1,[not,['var?',val1]]],[setq,ob1,val1]],[if,['var?',ob2],[progn,[setq,val2,['bd-lookup',['variable-name',ob2],bindings]],[if,[and,val2,[not,['var?',val2]]],[setq,ob2,val2]]]],[cond,[[and,['var?',ob1],['var?',ob2]],[if,['type-compatible-vars?',ob1,ob2],[if,'*diff?*',bindings,['bd-bind',['variable-name',ob2],ob1,['bd-bind',['variable-name',ob1],ob2,bindings]]],[]]],[['var?',ob1],[if,['var-ty$instance?',ob2,['variable-type',ob1]],[if,'*diff?*',bindings,['bd-bind',['variable-name',ob1],ob2,bindings]],[]]],[['var?',ob2],[if,['var-ty$instance?',ob1,['variable-type',ob2]],[if,'*diff?*',bindings,['bd-bind',['variable-name',ob2],ob1,bindings]],[]]],[else,[if,'reverse?',['ob$unify2',ob2,ob1,bindings,'ignore-slots'],['ob$unify2',ob1,ob2,bindings,'ignore-slots']]]]]])
wl:lambda_def(defun, u_ob_c36_unify_var, f_u_ob_c36_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], [[let, [[u_val1, [u_bd_lookup, [u_variable_name, u_ob1], bindings]], [u_val2, []]], [if, [and, u_val1, [not, [u_var_c63, u_val1]]], [setq, u_ob1, u_val1]], [if, [u_var_c63, u_ob2], [progn, [setq, u_val2, [u_bd_lookup, [u_variable_name, u_ob2], bindings]], [if, [and, u_val2, [not, [u_var_c63, u_val2]]], [setq, u_ob2, u_val2]]]], [cond, [[and, [u_var_c63, u_ob1], [u_var_c63, u_ob2]], [if, [u_type_compatible_vars_c63, u_ob1, u_ob2], [if, u_xx_diff_c63_xx, bindings, [u_bd_bind, [u_variable_name, u_ob2], u_ob1, [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]], []]], [[u_var_c63, u_ob1], [if, [u_var_ty_c36_instance_c63, u_ob2, [u_variable_type, u_ob1]], [if, u_xx_diff_c63_xx, bindings, [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]], []]], [[u_var_c63, u_ob2], [if, [u_var_ty_c36_instance_c63, u_ob1, [u_variable_type, u_ob2]], [if, u_xx_diff_c63_xx, bindings, [u_bd_bind, [u_variable_name, u_ob2], u_ob1, bindings]], []]], [u_else, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, u_ob1, bindings, u_ignore_slots], [u_ob_c36_unify2, u_ob1, u_ob2, bindings, u_ignore_slots]]]]]]).
wl:arglist_info(u_ob_c36_unify_var, f_u_ob_c36_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_unify_var).

/*

### Compiled:  `U::OB$UNIFY-VAR` 
*/
f_u_ob_c36_unify_var(Ob1, Ob2, Bindings, Ignore_slots, Reverse_c63, TrueResult44) :-
	nop(global_env(Env)),
	Env85=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots), bv(u_reverse_c63, Reverse_c63)|Env],
	f_u_bd_lookup([u_variable_name, u_ob1], bindings, Val1_Init),
	LEnv=[bv(u_val1, Val1_Init), bv(u_val2, [])|Env85],
	get_var(LEnv, u_val1, IFTEST13),
	(   IFTEST13\==[]
	->  f_u_var_c63(u_val1, Not_Param),
	    cl_not(Not_Param, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_val1, Val1_Get18),
	    set_var(LEnv, u_ob1, Val1_Get18),
	    _356761178=Val1_Get18
	;   _356761178=[]
	),
	f_u_var_c63(u_ob2, IFTEST20),
	(   IFTEST20\==[]
	->  f_u_bd_lookup([u_variable_name, u_ob2], bindings, Bindings91),
	    set_var(LEnv, u_val2, Bindings91),
	    get_var(LEnv, u_val2, IFTEST24),
	    (   IFTEST24\==[]
	    ->  f_u_var_c63(u_val2, Not_Param93),
		cl_not(Not_Param93, TrueResult27),
		IFTEST22=TrueResult27
	    ;   IFTEST22=[]
	    ),
	    (   IFTEST22\==[]
	    ->  get_var(LEnv, u_val2, Val2_Get28),
		set_var(LEnv, u_ob2, Val2_Get28),
		TrueResult30=Val2_Get28
	    ;   TrueResult30=[]
	    )
	;   TrueResult30=[]
	),
	f_u_var_c63(u_ob1, IFTEST33),
	(   IFTEST33\==[]
	->  f_u_var_c63(u_ob2, TrueResult35),
	    IFTEST31=TrueResult35
	;   IFTEST31=[]
	),
	(   IFTEST31\==[]
	->  f_u_type_compatible_vars_c63(u_ob1, u_ob2, IFTEST36),
	    (   IFTEST36\==[]
	    ->  get_var(LEnv, u_xx_diff_c63_xx, IFTEST38),
		(   IFTEST38\==[]
		->  get_var(LEnv, bindings, Bindings_Get),
		    TrueResult44=Bindings_Get
		;   f_u_bd_bind([u_variable_name, u_ob2],
				u_ob1,
				
				[ u_bd_bind,
				  [u_variable_name, u_ob1],
				  u_ob2,
				  bindings
				],
				ElseResult),
		    TrueResult44=ElseResult
		)
	    ;   TrueResult44=[]
	    )
	;   f_u_var_c63(u_ob1, IFTEST45),
	    (   IFTEST45\==[]
	    ->  f_u_var_ty_c36_instance_c63(u_ob2,
					    [u_variable_type, u_ob1],
					    IFTEST47),
		(   IFTEST47\==[]
		->  get_var(LEnv, u_xx_diff_c63_xx, IFTEST49),
		    (   IFTEST49\==[]
		    ->  get_var(LEnv, bindings, Bindings_Get52),
			TrueResult44=Bindings_Get52
		    ;   f_u_bd_bind([u_variable_name, u_ob1],
				    u_ob2,
				    bindings,
				    ElseResult54),
			TrueResult44=ElseResult54
		    )
		;   TrueResult44=[]
		)
	    ;   f_u_var_c63(u_ob2, IFTEST56),
		(   IFTEST56\==[]
		->  f_u_var_ty_c36_instance_c63(u_ob1,
						[u_variable_type, u_ob2],
						IFTEST58),
		    (   IFTEST58\==[]
		    ->  get_var(LEnv, u_xx_diff_c63_xx, IFTEST60),
			(   IFTEST60\==[]
			->  get_var(LEnv, bindings, Bindings_Get63),
			    TrueResult44=Bindings_Get63
			;   f_u_bd_bind([u_variable_name, u_ob2],
					u_ob1,
					bindings,
					ElseResult65),
			    TrueResult44=ElseResult65
			)
		    ;   TrueResult44=[]
		    )
		;   get_var(LEnv, u_else, IFTEST67),
		    (   IFTEST67\==[]
		    ->  get_var(LEnv, u_reverse_c63, IFTEST70),
			(   IFTEST70\==[]
			->  f_u_ob_c36_unify2(u_ob2,
					      u_ob1,
					      bindings,
					      u_ignore_slots,
					      TrueResult73),
			    TrueResult44=TrueResult73
			;   f_u_ob_c36_unify2(u_ob1,
					      u_ob2,
					      bindings,
					      u_ignore_slots,
					      ElseResult74),
			    TrueResult44=ElseResult74
			)
		    ;   TrueResult44=[]
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_unify_var, classof, claz_function),
   set_opv(u_ob_c36_unify_var, compile_as, kw_function),
   set_opv(u_ob_c36_unify_var, function, f_u_ob_c36_unify_var),
   _Ignored4=u_ob_c36_unify_var.
/*
:- side_effect(assert_lsp(u_ob_c36_unify_var,
			  wl:lambda_def(defun, u_ob_c36_unify_var, f_u_ob_c36_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], [[let, [[u_val1, [u_bd_lookup, [u_variable_name, u_ob1], bindings]], [u_val2, []]], [if, [and, u_val1, [not, [u_var_c63, u_val1]]], [setq, u_ob1, u_val1]], [if, [u_var_c63, u_ob2], [progn, [setq, u_val2, [u_bd_lookup, [u_variable_name, u_ob2], bindings]], [if, [and, u_val2, [not, [u_var_c63, u_val2]]], [setq, u_ob2, u_val2]]]], [cond, [[and, [u_var_c63, u_ob1], [u_var_c63, u_ob2]], [if, [u_type_compatible_vars_c63, u_ob1, u_ob2], [if, u_xx_diff_c63_xx, bindings, [u_bd_bind, [u_variable_name, u_ob2], u_ob1, [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]], []]], [[u_var_c63, u_ob1], [if, [u_var_ty_c36_instance_c63, u_ob2, [u_variable_type, u_ob1]], [if, u_xx_diff_c63_xx, bindings, [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]], []]], [[u_var_c63, u_ob2], [if, [u_var_ty_c36_instance_c63, u_ob1, [u_variable_type, u_ob2]], [if, u_xx_diff_c63_xx, bindings, [u_bd_bind, [u_variable_name, u_ob2], u_ob1, bindings]], []]], [u_else, [if, u_reverse_c63, [u_ob_c36_unify2, u_ob2, u_ob1, bindings, u_ignore_slots], [u_ob_c36_unify2, u_ob1, u_ob2, bindings, u_ignore_slots]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_var,
			  wl:arglist_info(u_ob_c36_unify_var, f_u_ob_c36_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_unify_var,
			  wl:init_args(exact_only, f_u_ob_c36_unify_var))).
*/
/*
 was  (if val1 (setq ob1 val1))
*/
/*
 was       (if val2 (setq ob2 val2))
*/
/*
(and (not (var? ob1)) (not (var? ob2)))
*/
/*
*/
/*
 This is the old incomprehensible version.
*/
/*
*/
/*
(defun ob$old-unify-var (ob1 ob2 bindings ignore-slots reverse?)
  (if *relax-unify-var*
      (if (null? (variable-name ob1))
          bindings
          (let ((found (bd-lookup (variable-name ob1) bindings)))
               (if found
                   (ob$unify2 found ob2 bindings ignore-slots)
                   (if (var? ob2)
                       (progn
                        (setq found (bd-lookup (variable-name ob2) bindings))
                        (if found
                            (ob$unify2 ob1 found bindings ignore-slots) ; ?
                            (bd-bind (variable-name ob1) ob2 bindings)))
                       (bd-bind (variable-name ob1) ob2 bindings)))))
      (progn
  (if (and (variable-type ob1)
           (not (var? ob2))
           (not (ty$instance-of? ob2 (variable-type ob1))))
      nil
  (if (null? (variable-name ob1))
      bindings ; but should do type compatibility check
      (let ((found (bd-lookup (variable-name ob1) bindings)))
           (if found
           (ob$unify2 found ob2 bindings ignore-slots)
               (if (var? ob2)
                   (progn
                    (setq found (bd-lookup (variable-name ob2) bindings))
                    (if found
                        (ob$unify2 ob1 found bindings ignore-slots) ; ?
            (if (type-compatible-vars? ob1 ob2)
                        (bd-bind (variable-name ob1) ob2 bindings) nil)))
                   ; should check type compatibility in above line,
                   ; but only if both variables are typed.
                   (if (variable-type ob1)
                       (if (ty$instance-of? ob2 (variable-type ob1))
                   (bd-bind (variable-name ob1) ob2 bindings)
                   nil)
               (bd-bind (variable-name ob1) ob2 bindings))))))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:13661 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$old-unify-var',[ob1,ob2,bindings,'ignore-slots','reverse?'],[if,'*relax-unify-var*',[if,['null?',['variable-name',ob1]],bindings,[let,[[found,['bd-lookup',['variable-name',ob1],bindings]]],[if,found,['ob$unify2',found,ob2,bindings,'ignore-slots'],[if,['var?',ob2],[progn,[setq,found,['bd-lookup',['variable-name',ob2],bindings]],[if,found,['ob$unify2',ob1,found,bindings,'ignore-slots'],['bd-bind',['variable-name',ob1],ob2,bindings]]],['bd-bind',['variable-name',ob1],ob2,bindings]]]]],[progn,[if,[and,['variable-type',ob1],[not,['var?',ob2]],[not,['ty$instance-of?',ob2,['variable-type',ob1]]]],[],[if,['null?',['variable-name',ob1]],bindings,[let,[[found,['bd-lookup',['variable-name',ob1],bindings]]],[if,found,['ob$unify2',found,ob2,bindings,'ignore-slots'],[if,['var?',ob2],[progn,[setq,found,['bd-lookup',['variable-name',ob2],bindings]],[if,found,['ob$unify2',ob1,found,bindings,'ignore-slots'],[if,['type-compatible-vars?',ob1,ob2],['bd-bind',['variable-name',ob1],ob2,bindings],[]]]],[if,['variable-type',ob1],[if,['ty$instance-of?',ob2,['variable-type',ob1]],['bd-bind',['variable-name',ob1],ob2,bindings],[]],['bd-bind',['variable-name',ob1],ob2,bindings]]]]]]]]]])
wl:lambda_def(defun, u_ob_c36_old_unify_var, f_u_ob_c36_old_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], [[if, u_xx_relax_unify_var_xx, [if, [u_null_c63, [u_variable_name, u_ob1]], bindings, [let, [[u_found, [u_bd_lookup, [u_variable_name, u_ob1], bindings]]], [if, u_found, [u_ob_c36_unify2, u_found, u_ob2, bindings, u_ignore_slots], [if, [u_var_c63, u_ob2], [progn, [setq, u_found, [u_bd_lookup, [u_variable_name, u_ob2], bindings]], [if, u_found, [u_ob_c36_unify2, u_ob1, u_found, bindings, u_ignore_slots], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]]]], [progn, [if, [and, [u_variable_type, u_ob1], [not, [u_var_c63, u_ob2]], [not, [u_ty_c36_instance_of_c63, u_ob2, [u_variable_type, u_ob1]]]], [], [if, [u_null_c63, [u_variable_name, u_ob1]], bindings, [let, [[u_found, [u_bd_lookup, [u_variable_name, u_ob1], bindings]]], [if, u_found, [u_ob_c36_unify2, u_found, u_ob2, bindings, u_ignore_slots], [if, [u_var_c63, u_ob2], [progn, [setq, u_found, [u_bd_lookup, [u_variable_name, u_ob2], bindings]], [if, u_found, [u_ob_c36_unify2, u_ob1, u_found, bindings, u_ignore_slots], [if, [u_type_compatible_vars_c63, u_ob1, u_ob2], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings], []]]], [if, [u_variable_type, u_ob1], [if, [u_ty_c36_instance_of_c63, u_ob2, [u_variable_type, u_ob1]], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings], []], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]]]]]]]]]).
wl:arglist_info(u_ob_c36_old_unify_var, f_u_ob_c36_old_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_old_unify_var).

/*

### Compiled:  `U::OB$OLD-UNIFY-VAR` 
*/
f_u_ob_c36_old_unify_var(Ob1, Ob2, Bindings, Ignore_slots, Reverse_c63, TrueResult28) :-
	nop(global_env(Env)),
	Env84=[bv(u_ob1, Ob1), bv(u_ob2, Ob2), bv(bindings, Bindings), bv(u_ignore_slots, Ignore_slots), bv(u_reverse_c63, Reverse_c63)|Env],
	get_var(Env84, u_xx_relax_unify_var_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_null_c63([u_variable_name, u_ob1], IFTEST10),
	    (   IFTEST10\==[]
	    ->  get_var(Env84, bindings, Bindings_Get),
		TrueResult28=Bindings_Get
	    ;   f_u_bd_lookup([u_variable_name, u_ob1], bindings, Found_Init),
		LEnv=[bv(u_found, Found_Init)|Env84],
		get_var(LEnv, u_found, IFTEST17),
		(   IFTEST17\==[]
		->  f_u_ob_c36_unify2(u_found,
				      u_ob2,
				      bindings,
				      u_ignore_slots,
				      TrueResult30),
		    TrueResult28=TrueResult30
		;   f_u_var_c63(u_ob2, IFTEST20),
		    (   IFTEST20\==[]
		    ->  f_u_bd_lookup([u_variable_name, u_ob2],
				      bindings,
				      Bindings90),
			set_var(LEnv, u_found, Bindings90),
			get_var(LEnv, u_found, IFTEST23),
			(   IFTEST23\==[]
			->  f_u_ob_c36_unify2(u_ob1,
					      u_found,
					      bindings,
					      u_ignore_slots,
					      TrueResult),
			    TrueResult28=TrueResult
			;   f_u_bd_bind([u_variable_name, u_ob1],
					u_ob2,
					bindings,
					ElseResult),
			    TrueResult28=ElseResult
			)
		    ;   f_u_bd_bind([u_variable_name, u_ob1],
				    u_ob2,
				    bindings,
				    ElseResult29),
			TrueResult28=ElseResult29
		    )
		)
	    )
	;   f_u_variable_type(u_ob1, IFTEST36),
	    (   IFTEST36\==[]
	    ->  f_u_var_c63(u_ob2, PredArgResult),
		(   PredArgResult==[]
		->  get_var(Env84, u_ob2, Ob2_Get),
		    f_u_variable_type(u_ob1, Variable_type_Ret),
		    f_u_ty_c36_instance_of_c63(Ob2_Get,
					       Variable_type_Ret,
					       Not_Param),
		    cl_not(Not_Param, TrueResult42),
		    IFTEST34=TrueResult42
		;   IFTEST34=[]
		)
	    ;   IFTEST34=[]
	    ),
	    (   IFTEST34\==[]
	    ->  TrueResult28=[]
	    ;   f_u_null_c63([u_variable_name, u_ob1], IFTEST44),
		(   IFTEST44\==[]
		->  get_var(Env84, bindings, Bindings_Get46),
		    TrueResult28=Bindings_Get46
		;   f_u_bd_lookup([u_variable_name, u_ob1],
				  bindings,
				  Found_Init50),
		    LEnv49=[bv(u_found, Found_Init50)|Env84],
		    get_var(LEnv49, u_found, IFTEST51),
		    (   IFTEST51\==[]
		    ->  f_u_ob_c36_unify2(u_found,
					  u_ob2,
					  bindings,
					  u_ignore_slots,
					  TrueResult75),
			TrueResult28=TrueResult75
		    ;   f_u_var_c63(u_ob2, IFTEST54),
			(   IFTEST54\==[]
			->  f_u_bd_lookup([u_variable_name, u_ob2],
					  bindings,
					  Bindings91),
			    set_var(LEnv49, u_found, Bindings91),
			    get_var(LEnv49, u_found, IFTEST57),
			    (   IFTEST57\==[]
			    ->  f_u_ob_c36_unify2(u_ob1,
						  u_found,
						  bindings,
						  u_ignore_slots,
						  TrueResult63),
				TrueResult28=TrueResult63
			    ;   f_u_type_compatible_vars_c63(u_ob1,
							     u_ob2,
							     IFTEST60),
				(   IFTEST60\==[]
				->  f_u_bd_bind([u_variable_name, u_ob1],
						u_ob2,
						bindings,
						TrueResult62),
				    TrueResult28=TrueResult62
				;   TrueResult28=[]
				)
			    )
			;   f_u_variable_type(u_ob1, IFTEST65),
			    (   IFTEST65\==[]
			    ->  get_var(LEnv49, u_ob2, Ob2_Get69),
				f_u_variable_type(u_ob1, Variable_type_Ret94),
				f_u_ty_c36_instance_of_c63(Ob2_Get69,
							   Variable_type_Ret94,
							   IFTEST67),
				(   IFTEST67\==[]
				->  f_u_bd_bind([u_variable_name, u_ob1],
						u_ob2,
						bindings,
						TrueResult70),
				    TrueResult28=TrueResult70
				;   TrueResult28=[]
				)
			    ;   f_u_bd_bind([u_variable_name, u_ob1],
					    u_ob2,
					    bindings,
					    ElseResult72),
				TrueResult28=ElseResult72
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_old_unify_var, classof, claz_function),
   set_opv(u_ob_c36_old_unify_var, compile_as, kw_function),
   set_opv(u_ob_c36_old_unify_var, function, f_u_ob_c36_old_unify_var),
   _Ignored4=u_ob_c36_old_unify_var.
/*
:- side_effect(assert_lsp(u_ob_c36_old_unify_var,
			  wl:lambda_def(defun, u_ob_c36_old_unify_var, f_u_ob_c36_old_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], [[if, u_xx_relax_unify_var_xx, [if, [u_null_c63, [u_variable_name, u_ob1]], bindings, [let, [[u_found, [u_bd_lookup, [u_variable_name, u_ob1], bindings]]], [if, u_found, [u_ob_c36_unify2, u_found, u_ob2, bindings, u_ignore_slots], [if, [u_var_c63, u_ob2], [progn, [setq, u_found, [u_bd_lookup, [u_variable_name, u_ob2], bindings]], [if, u_found, [u_ob_c36_unify2, u_ob1, u_found, bindings, u_ignore_slots], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]]]], [progn, [if, [and, [u_variable_type, u_ob1], [not, [u_var_c63, u_ob2]], [not, [u_ty_c36_instance_of_c63, u_ob2, [u_variable_type, u_ob1]]]], [], [if, [u_null_c63, [u_variable_name, u_ob1]], bindings, [let, [[u_found, [u_bd_lookup, [u_variable_name, u_ob1], bindings]]], [if, u_found, [u_ob_c36_unify2, u_found, u_ob2, bindings, u_ignore_slots], [if, [u_var_c63, u_ob2], [progn, [setq, u_found, [u_bd_lookup, [u_variable_name, u_ob2], bindings]], [if, u_found, [u_ob_c36_unify2, u_ob1, u_found, bindings, u_ignore_slots], [if, [u_type_compatible_vars_c63, u_ob1, u_ob2], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings], []]]], [if, [u_variable_type, u_ob1], [if, [u_ty_c36_instance_of_c63, u_ob2, [u_variable_type, u_ob1]], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings], []], [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]]]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_old_unify_var,
			  wl:arglist_info(u_ob_c36_old_unify_var, f_u_ob_c36_old_unify_var, [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], arginfo{all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], opt:0, req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_old_unify_var,
			  wl:init_args(exact_only, f_u_ob_c36_old_unify_var))).
*/
/*
 ?
*/
/*
 but should do type compatibility check
*/
/*
 ?
*/
/*
 should check type compatibility in above line,
*/
/*
 but only if both variables are typed.
*/
/*
(setq *max-breadth* 10)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:15473 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*max-breadth*',10])
:- set_var(AEnv, setq, u_xx_max_breadth_xx, 10).
/*
(defun ob$path (from-constant to-ptn links bindings)
  (yloop (initial
         (result nil)
         (count 0)
         (next-obs (ob$get-many from-constant links)))
        (yuntil
         (or result
             (if (> count *max-breadth*)
                 (progn
                  (ndbg *gate-dbg* ob-warn
                        "Exceeded max breadth in ob$path."(defun ob$path (from-constant to-ptn links bindings)\n  (yloop (initial\n         (result nil)\n         (count 0)\n         (next-obs (ob$get-many from-constant links)))\n        (yuntil\n         (or result\n             (if (> count *max-breadth*)\n                 (progn\n                  (ndbg *gate-dbg* ob-warn\n                        \"Exceeded max breadth in ob$path.~%\")\n                  t)\n                 nil)))\n        (ywhile next-obs)\n        (ydo\n         (yloop (yfor next-ob in next-obs)\n               (yuntil result)\n               (ydo (setq result (ob$unify to-ptn next-ob bindings))))\n         (if (null? result)\n             (setq next-obs\n                  (walk-append\n                   (lambda (ob) (ob$get-many ob links))\n                   next-obs)))\n         (increment-me count))\n        (yresult result)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:15498 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$path',['from-constant','to-ptn',links,bindings],[yloop,[initial,[result,[]],[count,0],['next-obs',['ob$get-many','from-constant',links]]],[yuntil,[or,result,[if,[>,count,'*max-breadth*'],[progn,[ndbg,'*gate-dbg*','ob-warn','$STRING'("Exceeded max breadth in ob$path.~%")],t],[]]]],[ywhile,'next-obs'],[ydo,[yloop,[yfor,'next-ob',in,'next-obs'],[yuntil,result],[ydo,[setq,result,['ob$unify','to-ptn','next-ob',bindings]]]],[if,['null?',result],[setq,'next-obs',['walk-append',[lambda,[ob],['ob$get-many',ob,links]],'next-obs']]],['increment-me',count]],[yresult,result]]])
wl:lambda_def(defun, u_ob_c36_path, f_u_ob_c36_path, [u_from_constant, u_to_ptn, u_links, bindings], [[u_yloop, [u_initial, [u_result, []], [count, 0], [u_next_obs, [u_ob_c36_get_many, u_from_constant, u_links]]], [u_yuntil, [or, u_result, [if, [>, count, u_xx_max_breadth_xx], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Exceeded max breadth in ob$path.~%")], t], []]]], [u_ywhile, u_next_obs], [u_ydo, [u_yloop, [u_yfor, u_next_ob, u_in, u_next_obs], [u_yuntil, u_result], [u_ydo, [setq, u_result, [u_ob_c36_unify, u_to_ptn, u_next_ob, bindings]]]], [if, [u_null_c63, u_result], [setq, u_next_obs, [u_walk_append, [lambda, [u_ob], [u_ob_c36_get_many, u_ob, u_links]], u_next_obs]]], [u_increment_me, count]], [u_yresult, u_result]]]).
wl:arglist_info(u_ob_c36_path, f_u_ob_c36_path, [u_from_constant, u_to_ptn, u_links, bindings], arginfo{all:[u_from_constant, u_to_ptn, u_links, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from_constant, u_to_ptn, u_links, bindings], opt:0, req:[u_from_constant, u_to_ptn, u_links, bindings], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_path).

/*

### Compiled:  `U::OB$PATH` 
*/
f_u_ob_c36_path(From_constant, To_ptn, Links, Bindings, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_from_constant, From_constant), bv(u_to_ptn, To_ptn), bv(u_links, Links), bv(bindings, Bindings)|Env],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, []],
		      [count, 0],
		      [u_next_obs, [u_ob_c36_get_many, u_from_constant, u_links]]
		    ],
		    
		    [ u_yuntil,
		      
		      [ or,
			u_result,
			
			[ if,
			  [>, count, u_xx_max_breadth_xx],
			  
			  [ progn,
			    
			    [ u_ndbg,
			      u_xx_gate_dbg_xx,
			      u_ob_warn,
			      '$ARRAY'([*],
				       claz_base_character,
				       "Exceeded max breadth in ob$path.~%")
			    ],
			    t
			  ],
			  []
			]
		      ]
		    ],
		    [u_ywhile, u_next_obs],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_yfor, u_next_ob, u_in, u_next_obs],
			[u_yuntil, u_result],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    [u_ob_c36_unify, u_to_ptn, u_next_ob, bindings]
			  ]
			]
		      ],
		      
		      [ if,
			[u_null_c63, u_result],
			
			[ setq,
			  u_next_obs,
			  
			  [ u_walk_append,
			    [lambda, [u_ob], [u_ob_c36_get_many, u_ob, u_links]],
			    u_next_obs
			  ]
			]
		      ],
		      [u_increment_me, count]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_path, classof, claz_function),
   set_opv(u_ob_c36_path, compile_as, kw_function),
   set_opv(u_ob_c36_path, function, f_u_ob_c36_path),
   _Ignored4=u_ob_c36_path.
/*
:- side_effect(assert_lsp(u_ob_c36_path,
			  wl:lambda_def(defun, u_ob_c36_path, f_u_ob_c36_path, [u_from_constant, u_to_ptn, u_links, bindings], [[u_yloop, [u_initial, [u_result, []], [count, 0], [u_next_obs, [u_ob_c36_get_many, u_from_constant, u_links]]], [u_yuntil, [or, u_result, [if, [>, count, u_xx_max_breadth_xx], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Exceeded max breadth in ob$path.~%")], t], []]]], [u_ywhile, u_next_obs], [u_ydo, [u_yloop, [u_yfor, u_next_ob, u_in, u_next_obs], [u_yuntil, u_result], [u_ydo, [setq, u_result, [u_ob_c36_unify, u_to_ptn, u_next_ob, bindings]]]], [if, [u_null_c63, u_result], [setq, u_next_obs, [u_walk_append, [lambda, [u_ob], [u_ob_c36_get_many, u_ob, u_links]], u_next_obs]]], [u_increment_me, count]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_path,
			  wl:arglist_info(u_ob_c36_path, f_u_ob_c36_path, [u_from_constant, u_to_ptn, u_links, bindings], arginfo{all:[u_from_constant, u_to_ptn, u_links, bindings], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_from_constant, u_to_ptn, u_links, bindings], opt:0, req:[u_from_constant, u_to_ptn, u_links, bindings], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_path,
			  wl:init_args(exact_only, f_u_ob_c36_path))).
*/
/*
(setq *uniquified-obs* nil)

;
; The following function seems to be ineffectual. Maybe all references
; to these obs are not being deleted?
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16333 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*uniquified-obs*',[]])
:- set_var(AEnv, setq, u_xx_uniquified_obs_xx, []).
/*
*/
/*
 The following function seems to be ineffectual. Maybe all references
*/
/*
 to these obs are not being deleted?
*/
/*
*/
/*
(defun gc-uniquified-obs ()
  (yloop (yfor ob in *uniquified-obs*)
        (ydo (ob$destroy ob)))
  (setq *uniquified-obs* nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16475 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'gc-uniquified-obs',[],[yloop,[yfor,ob,in,'*uniquified-obs*'],[ydo,['ob$destroy',ob]]],[setq,'*uniquified-obs*',[]]])
wl:lambda_def(defun, u_gc_uniquified_obs, f_u_gc_uniquified_obs, [], [[u_yloop, [u_yfor, u_ob, u_in, u_xx_uniquified_obs_xx], [u_ydo, [u_ob_c36_destroy, u_ob]]], [setq, u_xx_uniquified_obs_xx, []]]).
wl:arglist_info(u_gc_uniquified_obs, f_u_gc_uniquified_obs, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_gc_uniquified_obs).

/*

### Compiled:  `U::GC-UNIQUIFIED-OBS` 
*/
f_u_gc_uniquified_obs(FnResult) :-
	nop(global_env(Env)),
	AEnv=Env,
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, u_xx_uniquified_obs_xx],
		    [u_ydo, [u_ob_c36_destroy, u_ob]]
		  ],
		  Yloop_Ret),
	set_var(AEnv, setq, u_xx_uniquified_obs_xx, []),
	[]=FnResult.
:- set_opv(f_u_gc_uniquified_obs, classof, claz_function),
   set_opv(u_gc_uniquified_obs, compile_as, kw_function),
   set_opv(u_gc_uniquified_obs, function, f_u_gc_uniquified_obs),
   _Ignored4=u_gc_uniquified_obs.
/*
:- side_effect(assert_lsp(u_gc_uniquified_obs,
			  wl:lambda_def(defun, u_gc_uniquified_obs, f_u_gc_uniquified_obs, [], [[u_yloop, [u_yfor, u_ob, u_in, u_xx_uniquified_obs_xx], [u_ydo, [u_ob_c36_destroy, u_ob]]], [setq, u_xx_uniquified_obs_xx, []]]))).
*/
/*
:- side_effect(assert_lsp(u_gc_uniquified_obs,
			  wl:arglist_info(u_gc_uniquified_obs, f_u_gc_uniquified_obs, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_gc_uniquified_obs,
			  wl:init_args(exact_only, f_u_gc_uniquified_obs))).
*/
/*
(defun ob$compare1 (source target substit ignore-slots proc)
  (if (memq? target (bd-lookup source *already-matched*))
      substit
      (progn
       (bd-bind! source
                 (cons target (bd-lookup source *already-matched*))
                 *already-matched*)
       (bd-bind! target
                 (cons source (bd-lookup target *already-matched*))
                 *already-matched*)
       (let ((result
        (cond
         ((eq? source target) substit)
         ((and (ob? source)
               (and (ob$literal? source) (not (ty? source)))) nil)
         ((and (ob? target)
               (and (ob$literal? target) (not (ty? target)))) nil)
         ((eq? (bd-lookup source substit) target) substit)
         ((and (ob? source)
               (not (ty? source))
               (ob? target)
               (not (ty? target)))
          (yloop (initial (compared-slot-indices nil)
                         (target-slots (ob$pairs target))
                         (target-slot-index nil)
                         (new-substit nil)
                         (save-substit substit)
                         (found? nil)
                         (proc-result nil))
                (yfor cur in (ob$pairs source)) ; was reverse
                (ywhile substit)
                (ydo (if (and (not (memq? (car cur) ignore-slots))
                             (not (memq? (car cur) *permanent-ignore-slots*)))
                        (progn
                         (setq target-slot-index 0)
                         (setq new-substit nil)
                         (setq found? nil)
(yloop (yfor target-slot-value in target-slots)
      (yuntil found?)
      (ydo (if (and (eq? (car cur) (slots-name target-slot-value))
                   (not (memq? target-slot-index compared-slot-indices))
                   (setq new-substit (if (eq? (cadr cur)
                                               (slots-value target-slot-value))
                                        substit
                                        (ob$compare1 (cadr cur)
                                                      (slots-value
                                                      target-slot-value)
                                                      substit
                                                      ignore-slots
                                                      proc))))
                                       (progn
                                        (setq found? t)
                                        (setq compared-slot-indices
                                             (cons target-slot-index
                                                   compared-slot-indices))))
                                   (increment-me target-slot-index)))
                         (if found?
                             (setq substit new-substit)
                             (setq substit nil)))))
                (yresult (if (null? substit)
                            (if (setq proc-result (funcall proc source target))
                                (cons 't (cons (list source target proc-result)
                                          (cdr save-substit)))
                                nil)
                            substit))))
         ((and (ty? source) (ty? target))
          (let ((proc-result (funcall proc source target)))
            (if proc-result
                (cons 't (cons (list source target proc-result) (cdr substit)))
                nil)))
         (else nil))))
        (if result
            result
            (progn
             (bd-bind! source
                       (delq! target (bd-lookup source *already-matched*))
                       *already-matched*)
             (bd-bind! target
                       (delq! source (bd-lookup target *already-matched*))
                       *already-matched*)
             nil))))))

; 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16605 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$compare1',[source,target,substit,'ignore-slots',proc],[if,['memq?',target,['bd-lookup',source,'*already-matched*']],substit,[progn,['bd-bind!',source,[cons,target,['bd-lookup',source,'*already-matched*']],'*already-matched*'],['bd-bind!',target,[cons,source,['bd-lookup',target,'*already-matched*']],'*already-matched*'],[let,[[result,[cond,[['eq?',source,target],substit],[[and,['ob?',source],[and,['ob$literal?',source],[not,['ty?',source]]]],[]],[[and,['ob?',target],[and,['ob$literal?',target],[not,['ty?',target]]]],[]],[['eq?',['bd-lookup',source,substit],target],substit],[[and,['ob?',source],[not,['ty?',source]],['ob?',target],[not,['ty?',target]]],[yloop,[initial,['compared-slot-indices',[]],['target-slots',['ob$pairs',target]],['target-slot-index',[]],['new-substit',[]],['save-substit',substit],['found?',[]],['proc-result',[]]],[yfor,cur,in,['ob$pairs',source]],[ywhile,substit],[ydo,[if,[and,[not,['memq?',[car,cur],'ignore-slots']],[not,['memq?',[car,cur],'*permanent-ignore-slots*']]],[progn,[setq,'target-slot-index',0],[setq,'new-substit',[]],[setq,'found?',[]],[yloop,[yfor,'target-slot-value',in,'target-slots'],[yuntil,'found?'],[ydo,[if,[and,['eq?',[car,cur],['slots-name','target-slot-value']],[not,['memq?','target-slot-index','compared-slot-indices']],[setq,'new-substit',[if,['eq?',[cadr,cur],['slots-value','target-slot-value']],substit,['ob$compare1',[cadr,cur],['slots-value','target-slot-value'],substit,'ignore-slots',proc]]]],[progn,[setq,'found?',t],[setq,'compared-slot-indices',[cons,'target-slot-index','compared-slot-indices']]]],['increment-me','target-slot-index']]],[if,'found?',[setq,substit,'new-substit'],[setq,substit,[]]]]]],[yresult,[if,['null?',substit],[if,[setq,'proc-result',[funcall,proc,source,target]],[cons,[quote,t],[cons,[list,source,target,'proc-result'],[cdr,'save-substit']]],[]],substit]]]],[[and,['ty?',source],['ty?',target]],[let,[['proc-result',[funcall,proc,source,target]]],[if,'proc-result',[cons,[quote,t],[cons,[list,source,target,'proc-result'],[cdr,substit]]],[]]]],[else,[]]]]],[if,result,result,[progn,['bd-bind!',source,['delq!',target,['bd-lookup',source,'*already-matched*']],'*already-matched*'],['bd-bind!',target,['delq!',source,['bd-lookup',target,'*already-matched*']],'*already-matched*'],[]]]]]]])
wl:lambda_def(defun, u_ob_c36_compare1, f_u_ob_c36_compare1, [ext_source, u_target, u_substit, u_ignore_slots, u_proc], [[if, [u_memq_c63, u_target, [u_bd_lookup, ext_source, u_xx_already_matched_xx]], u_substit, [progn, [u_bd_bind_c33, ext_source, [cons, u_target, [u_bd_lookup, ext_source, u_xx_already_matched_xx]], u_xx_already_matched_xx], [u_bd_bind_c33, u_target, [cons, ext_source, [u_bd_lookup, u_target, u_xx_already_matched_xx]], u_xx_already_matched_xx], [let, [[u_result, [cond, [[u_eq_c63, ext_source, u_target], u_substit], [[and, [u_ob_c63, ext_source], [and, [u_ob_c36_literal_c63, ext_source], [not, [u_ty_c63, ext_source]]]], []], [[and, [u_ob_c63, u_target], [and, [u_ob_c36_literal_c63, u_target], [not, [u_ty_c63, u_target]]]], []], [[u_eq_c63, [u_bd_lookup, ext_source, u_substit], u_target], u_substit], [[and, [u_ob_c63, ext_source], [not, [u_ty_c63, ext_source]], [u_ob_c63, u_target], [not, [u_ty_c63, u_target]]], [u_yloop, [u_initial, [u_compared_slot_indices, []], [u_target_slots, [u_ob_c36_pairs, u_target]], [u_target_slot_index, []], [u_new_substit, []], [u_save_substit, u_substit], [u_found_c63, []], [u_proc_result, []]], [u_yfor, u_cur, u_in, [u_ob_c36_pairs, ext_source]], [u_ywhile, u_substit], [u_ydo, [if, [and, [not, [u_memq_c63, [car, u_cur], u_ignore_slots]], [not, [u_memq_c63, [car, u_cur], u_xx_permanent_ignore_slots_xx]]], [progn, [setq, u_target_slot_index, 0], [setq, u_new_substit, []], [setq, u_found_c63, []], [u_yloop, [u_yfor, u_target_slot_value, u_in, u_target_slots], [u_yuntil, u_found_c63], [u_ydo, [if, [and, [u_eq_c63, [car, u_cur], [u_slots_name, u_target_slot_value]], [not, [u_memq_c63, u_target_slot_index, u_compared_slot_indices]], [setq, u_new_substit, [if, [u_eq_c63, [cadr, u_cur], [u_slots_value, u_target_slot_value]], u_substit, [u_ob_c36_compare1, [cadr, u_cur], [u_slots_value, u_target_slot_value], u_substit, u_ignore_slots, u_proc]]]], [progn, [setq, u_found_c63, t], [setq, u_compared_slot_indices, [cons, u_target_slot_index, u_compared_slot_indices]]]], [u_increment_me, u_target_slot_index]]], [if, u_found_c63, [setq, u_substit, u_new_substit], [setq, u_substit, []]]]]], [u_yresult, [if, [u_null_c63, u_substit], [if, [setq, u_proc_result, [funcall, u_proc, ext_source, u_target]], [cons, [quote, t], [cons, [list, ext_source, u_target, u_proc_result], [cdr, u_save_substit]]], []], u_substit]]]], [[and, [u_ty_c63, ext_source], [u_ty_c63, u_target]], [let, [[u_proc_result, [funcall, u_proc, ext_source, u_target]]], [if, u_proc_result, [cons, [quote, t], [cons, [list, ext_source, u_target, u_proc_result], [cdr, u_substit]]], []]]], [u_else, []]]]], [if, u_result, u_result, [progn, [u_bd_bind_c33, ext_source, [u_delq_c33, u_target, [u_bd_lookup, ext_source, u_xx_already_matched_xx]], u_xx_already_matched_xx], [u_bd_bind_c33, u_target, [u_delq_c33, ext_source, [u_bd_lookup, u_target, u_xx_already_matched_xx]], u_xx_already_matched_xx], []]]]]]]).
wl:arglist_info(u_ob_c36_compare1, f_u_ob_c36_compare1, [ext_source, u_target, u_substit, u_ignore_slots, u_proc], arginfo{all:[ext_source, u_target, u_substit, u_ignore_slots, u_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[ext_source, u_target, u_substit, u_ignore_slots, u_proc], opt:0, req:[ext_source, u_target, u_substit, u_ignore_slots, u_proc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_compare1).

/*

### Compiled:  `U::OB$COMPARE1` 
*/
f_u_ob_c36_compare1(Ext_source, Target, Substit, Ignore_slots, Proc, LetResult) :-
	nop(global_env(Env)),
	Env93=[bv(ext_source, Ext_source), bv(u_target, Target), bv(u_substit, Substit), bv(u_ignore_slots, Ignore_slots), bv(u_proc, Proc)|Env],
	f_u_memq_c63(u_target,
		     [u_bd_lookup, ext_source, u_xx_already_matched_xx],
		     IFTEST),
	(   IFTEST\==[]
	->  get_var(Env93, u_substit, Substit_Get),
	    LetResult=Substit_Get
	;   f_u_bd_bind_c33(ext_source,
			    
			    [ cons,
			      u_target,
			      [u_bd_lookup, ext_source, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx,
			    Xx_already_matched_xx),
	    f_u_bd_bind_c33(u_target,
			    
			    [ cons,
			      ext_source,
			      [u_bd_lookup, u_target, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx,
			    Xx_already_matched_xx100),
	    f_u_eq_c63(ext_source, u_target, IFTEST13),
	    (   IFTEST13\==[]
	    ->  get_var(Env93, u_substit, Substit_Get15),
		LetResult55=Substit_Get15
	    ;   f_u_ob_c63(ext_source, IFTEST18),
		(   IFTEST18\==[]
		->  get_var(Env93, ext_source, Ext_source_Get),
		    f_u_ob_c36_literal_c63(Ext_source_Get, IFTEST20),
		    (   IFTEST20\==[]
		    ->  f_u_ty_c63(ext_source, Not_Param),
			cl_not(Not_Param, TrueResult),
			IFTEST16=TrueResult
		    ;   IFTEST16=[]
		    )
		;   IFTEST16=[]
		),
		(   IFTEST16\==[]
		->  LetResult55=[]
		;   f_u_ob_c63(u_target, IFTEST27),
		    (   IFTEST27\==[]
		    ->  get_var(Env93, u_target, Target_Get),
			f_u_ob_c36_literal_c63(Target_Get, IFTEST29),
			(   IFTEST29\==[]
			->  f_u_ty_c63(u_target, Not_Param104),
			    cl_not(Not_Param104, TrueResult32),
			    IFTEST25=TrueResult32
			;   IFTEST25=[]
			)
		    ;   IFTEST25=[]
		    ),
		    (   IFTEST25\==[]
		    ->  LetResult55=[]
		    ;   f_u_eq_c63([u_bd_lookup, ext_source, u_substit],
				   u_target,
				   IFTEST34),
			(   IFTEST34\==[]
			->  get_var(Env93, u_substit, Substit_Get36),
			    LetResult55=Substit_Get36
			;   f_u_ob_c63(ext_source, IFTEST39),
			    (   IFTEST39\==[]
			    ->  f_u_ty_c63(ext_source, PredArgResult),
				(   PredArgResult==[]
				->  f_u_ob_c63(u_target, IFTEST44),
				    (   IFTEST44\==[]
				    ->  f_u_ty_c63(u_target, Not_Param105),
					cl_not(Not_Param105, TrueResult46),
					IFTEST37=TrueResult46
				    ;   IFTEST37=[]
				    )
				;   IFTEST37=[]
				)
			    ;   IFTEST37=[]
			    ),
			    (   IFTEST37\==[]
			    ->  f_u_yloop(
					  [ 
					    [ u_initial,
					      [u_compared_slot_indices, []],
					      
					      [ u_target_slots,
						[u_ob_c36_pairs, u_target]
					      ],
					      [u_target_slot_index, []],
					      [u_new_substit, []],
					      [u_save_substit, u_substit],
					      [u_found_c63, []],
					      [u_proc_result, []]
					    ],
					    
					    [ u_yfor,
					      u_cur,
					      u_in,
					      [u_ob_c36_pairs, ext_source]
					    ],
					    [u_ywhile, u_substit],
					    
					    [ u_ydo,
					      
					      [ if,
						
						[ and,
						  
						  [ not,
						    
						    [ u_memq_c63,
						      [car, u_cur],
						      u_ignore_slots
						    ]
						  ],
						  
						  [ not,
						    
						    [ u_memq_c63,
						      [car, u_cur],
						      u_xx_permanent_ignore_slots_xx
						    ]
						  ]
						],
						
						[ progn,
						  [setq, u_target_slot_index, 0],
						  [setq, u_new_substit, []],
						  [setq, u_found_c63, []],
						  
						  [ u_yloop,
						    
						    [ u_yfor,
						      u_target_slot_value,
						      u_in,
						      u_target_slots
						    ],
						    [u_yuntil, u_found_c63],
						    
						    [ u_ydo,
						      
						      [ if,
							
							[ and,
							  
							  [ u_eq_c63,
							    [car, u_cur],
							    
							    [ u_slots_name,
							      u_target_slot_value
							    ]
							  ],
							  
							  [ not,
							    
							    [ u_memq_c63,
							      u_target_slot_index,
							      u_compared_slot_indices
							    ]
							  ],
							  
							  [ setq,
							    u_new_substit,
							    
							    [ if,
							      
							      [ u_eq_c63,
								[cadr, u_cur],
								
								[ u_slots_value,
								  u_target_slot_value
								]
							      ],
							      u_substit,
							      
							      [ u_ob_c36_compare1,
								[cadr, u_cur],
								
								[ u_slots_value,
								  u_target_slot_value
								],
								u_substit,
								u_ignore_slots,
								u_proc
							      ]
							    ]
							  ]
							],
							
							[ progn,
							  [setq, u_found_c63, t],
							  
							  [ setq,
							    u_compared_slot_indices,
							    
							    [ cons,
							      u_target_slot_index,
							      u_compared_slot_indices
							    ]
							  ]
							]
						      ],
						      
						      [ u_increment_me,
							u_target_slot_index
						      ]
						    ]
						  ],
						  
						  [ if,
						    u_found_c63,
						    
						    [ setq,
						      u_substit,
						      u_new_substit
						    ],
						    [setq, u_substit, []]
						  ]
						]
					      ]
					    ],
					    
					    [ u_yresult,
					      
					      [ if,
						[u_null_c63, u_substit],
						
						[ if,
						  
						  [ setq,
						    u_proc_result,
						    
						    [ funcall,
						      u_proc,
						      ext_source,
						      u_target
						    ]
						  ],
						  
						  [ cons,
						    [quote, t],
						    
						    [ cons,
						      
						      [ list,
							ext_source,
							u_target,
							u_proc_result
						      ],
						      [cdr, u_save_substit]
						    ]
						  ],
						  []
						],
						u_substit
					      ]
					    ]
					  ],
					  TrueResult75),
				LetResult55=TrueResult75
			    ;   f_u_ty_c63(ext_source, IFTEST51),
				(   IFTEST51\==[]
				->  f_u_ty_c63(u_target, TrueResult53),
				    IFTEST49=TrueResult53
				;   IFTEST49=[]
				),
				(   IFTEST49\==[]
				->  get_var(Env93, ext_source, Ext_source_Get58),
				    get_var(Env93, u_proc, Proc_Get),
				    get_var(Env93, u_target, Target_Get59),
				    cl_apply(Proc_Get,
					     [Ext_source_Get58, Target_Get59],
					     Proc_result_Init),
				    LEnv56=[bv(u_proc_result, Proc_result_Init)|Env93],
				    get_var(LEnv56, u_proc_result, IFTEST61),
				    (   IFTEST61\==[]
				    ->  get_var(LEnv56,
						ext_source,
						Ext_source_Get64),
					get_var(LEnv56,
						u_proc_result,
						Proc_result_Get66),
					get_var(LEnv56, u_target, Target_Get65),
					CAR=[Ext_source_Get64, Target_Get65, Proc_result_Get66],
					get_var(LEnv56,
						u_substit,
						Substit_Get67),
					cl_cdr(Substit_Get67, Cdr_Ret),
					CDR=[CAR|Cdr_Ret],
					LetResult55=[t|CDR]
				    ;   LetResult55=[]
				    )
				;   get_var(Env93, u_else, IFTEST69),
				    (   IFTEST69\==[]
				    ->  LetResult55=[]
				    ;   LetResult55=[]
				    )
				)
			    )
			)
		    )
		)
	    ),
	    LEnv=[bv(u_result, LetResult55)|Env93],
	    get_var(LEnv, u_result, IFTEST84),
	    (   IFTEST84\==[]
	    ->  get_var(LEnv, u_result, Result_Get87),
		LetResult=Result_Get87
	    ;   f_u_bd_bind_c33(ext_source,
				
				[ u_delq_c33,
				  u_target,
				  
				  [ u_bd_lookup,
				    ext_source,
				    u_xx_already_matched_xx
				  ]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx101),
		f_u_bd_bind_c33(u_target,
				
				[ u_delq_c33,
				  ext_source,
				  
				  [ u_bd_lookup,
				    u_target,
				    u_xx_already_matched_xx
				  ]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx102),
		LetResult=[]
	    )
	).
:- set_opv(f_u_ob_c36_compare1, classof, claz_function),
   set_opv(u_ob_c36_compare1, compile_as, kw_function),
   set_opv(u_ob_c36_compare1, function, f_u_ob_c36_compare1),
   _Ignored4=u_ob_c36_compare1.
/*
:- side_effect(assert_lsp(u_ob_c36_compare1,
			  wl:lambda_def(defun, u_ob_c36_compare1, f_u_ob_c36_compare1, [ext_source, u_target, u_substit, u_ignore_slots, u_proc], [[if, [u_memq_c63, u_target, [u_bd_lookup, ext_source, u_xx_already_matched_xx]], u_substit, [progn, [u_bd_bind_c33, ext_source, [cons, u_target, [u_bd_lookup, ext_source, u_xx_already_matched_xx]], u_xx_already_matched_xx], [u_bd_bind_c33, u_target, [cons, ext_source, [u_bd_lookup, u_target, u_xx_already_matched_xx]], u_xx_already_matched_xx], [let, [[u_result, [cond, [[u_eq_c63, ext_source, u_target], u_substit], [[and, [u_ob_c63, ext_source], [and, [u_ob_c36_literal_c63, ext_source], [not, [u_ty_c63, ext_source]]]], []], [[and, [u_ob_c63, u_target], [and, [u_ob_c36_literal_c63, u_target], [not, [u_ty_c63, u_target]]]], []], [[u_eq_c63, [u_bd_lookup, ext_source, u_substit], u_target], u_substit], [[and, [u_ob_c63, ext_source], [not, [u_ty_c63, ext_source]], [u_ob_c63, u_target], [not, [u_ty_c63, u_target]]], [u_yloop, [u_initial, [u_compared_slot_indices, []], [u_target_slots, [u_ob_c36_pairs, u_target]], [u_target_slot_index, []], [u_new_substit, []], [u_save_substit, u_substit], [u_found_c63, []], [u_proc_result, []]], [u_yfor, u_cur, u_in, [u_ob_c36_pairs, ext_source]], [u_ywhile, u_substit], [u_ydo, [if, [and, [not, [u_memq_c63, [car, u_cur], u_ignore_slots]], [not, [u_memq_c63, [car, u_cur], u_xx_permanent_ignore_slots_xx]]], [progn, [setq, u_target_slot_index, 0], [setq, u_new_substit, []], [setq, u_found_c63, []], [u_yloop, [u_yfor, u_target_slot_value, u_in, u_target_slots], [u_yuntil, u_found_c63], [u_ydo, [if, [and, [u_eq_c63, [car, u_cur], [u_slots_name, u_target_slot_value]], [not, [u_memq_c63, u_target_slot_index, u_compared_slot_indices]], [setq, u_new_substit, [if, [u_eq_c63, [cadr, u_cur], [u_slots_value, u_target_slot_value]], u_substit, [u_ob_c36_compare1, [cadr, u_cur], [u_slots_value, u_target_slot_value], u_substit, u_ignore_slots, u_proc]]]], [progn, [setq, u_found_c63, t], [setq, u_compared_slot_indices, [cons, u_target_slot_index, u_compared_slot_indices]]]], [u_increment_me, u_target_slot_index]]], [if, u_found_c63, [setq, u_substit, u_new_substit], [setq, u_substit, []]]]]], [u_yresult, [if, [u_null_c63, u_substit], [if, [setq, u_proc_result, [funcall, u_proc, ext_source, u_target]], [cons, [quote, t], [cons, [list, ext_source, u_target, u_proc_result], [cdr, u_save_substit]]], []], u_substit]]]], [[and, [u_ty_c63, ext_source], [u_ty_c63, u_target]], [let, [[u_proc_result, [funcall, u_proc, ext_source, u_target]]], [if, u_proc_result, [cons, [quote, t], [cons, [list, ext_source, u_target, u_proc_result], [cdr, u_substit]]], []]]], [u_else, []]]]], [if, u_result, u_result, [progn, [u_bd_bind_c33, ext_source, [u_delq_c33, u_target, [u_bd_lookup, ext_source, u_xx_already_matched_xx]], u_xx_already_matched_xx], [u_bd_bind_c33, u_target, [u_delq_c33, ext_source, [u_bd_lookup, u_target, u_xx_already_matched_xx]], u_xx_already_matched_xx], []]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_compare1,
			  wl:arglist_info(u_ob_c36_compare1, f_u_ob_c36_compare1, [ext_source, u_target, u_substit, u_ignore_slots, u_proc], arginfo{all:[ext_source, u_target, u_substit, u_ignore_slots, u_proc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[ext_source, u_target, u_substit, u_ignore_slots, u_proc], opt:0, req:[ext_source, u_target, u_substit, u_ignore_slots, u_proc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_compare1,
			  wl:init_args(exact_only, f_u_ob_c36_compare1))).
*/
/*
 was reverse
*/
/*
 End of file.
*/


%; Total compilation time: 9.969 seconds


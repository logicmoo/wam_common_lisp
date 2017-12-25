#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init-1" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Mon Dec 25 05:35:18 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
;; setf.lisp
*/
/*
;;
*/
/*
;; Copyright (C) 2003-2006 Peter Graves
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
(in-package "SYSTEM")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1589 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','$STRING'("SYSTEM")])
:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored).
/*
(defun get-setf-method-inverse (form inverse setf-function)
  (let ((new-var (gensym))
        (vars nil)
        (vals nil))
    (dolist (x (cdr form))
      (push (gensym) vars)
      (push x vals))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
            (if setf-function
                `(,@inverse ,new-var ,@vars)
                (if (functionp (car inverse))
                    `(funcall ,@inverse ,@vars ,new-var)
                    `(,@inverse ,@vars ,new-var)))
            `(,(car form) ,@vars))))

;;; If a macro, expand one level and try again.  If not, go for the
;;; SETF function.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1617 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-method-inverse',[form,inverse,'setf-function'],[let,[['new-var',[gensym]],[vars,[]],[vals,[]]],[dolist,[x,[cdr,form]],[push,[gensym],vars],[push,x,vals]],[setq,vals,[nreverse,vals]],[values,vars,vals,[list,'new-var'],[if,'setf-function',['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#COMMA','new-var'],['#BQ-COMMA-ELIPSE',vars]]],[if,[functionp,[car,inverse]],['#BQ',[funcall,['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]],['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]]]],['#BQ',[['#COMMA',[car,form]],['#BQ-COMMA-ELIPSE',vars]]]]]])
wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]).
wl:arglist_info(sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_get_setf_method_inverse).

/*

### Compiled:  `SYS::GET-SETF-METHOD-INVERSE` 
*/
f_sys_get_setf_method_inverse(Form, Inverse, Setf_function, FnResult) :-
	nop(global_env(Env)),
	Env48=[bv(sys_form, Form), bv(sys_inverse, Inverse), bv(sys_setf_function, Setf_function)|Env],
	cl_gensym(New_var_Init),
	LEnv=[bv(sys_new_var, New_var_Init), bv(sys_vars, []), bv(sys_vals, [])|Env48],
	get_var(LEnv, sys_form, Form_Get),
	cl_cdr(Form_Get, List),
	BV=bv(sys_x, Ele),
	Push_Env=[BV|LEnv],
	forall(member(Ele, List),
	       ( nb_setarg(2, BV, Ele),
		 get_var(Push_Env, sys_vars, Vars_Get),
		 set_place(Push_Env, push, [gensym], [Vars_Get], Push_R),
		 get_var(Push_Env, sys_vals, Vals_Get),
		 set_place(Push_Env, push, [value, sys_x], [Vals_Get], Push_R15)
	       )),
	get_var(LEnv, sys_vals, Vals_Get21),
	cl_nreverse(Vals_Get21, Vals),
	set_var(LEnv, sys_vals, Vals),
	get_var(LEnv, sys_new_var, New_var_Get),
	get_var(LEnv, sys_vals, Vals_Get22),
	CAR=[New_var_Get],
	get_var(LEnv, sys_setf_function, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, sys_inverse, Inverse_Get),
	    get_var(LEnv, sys_new_var, New_var_Get28),
	    get_var(LEnv, sys_vars, Vars_Get29),
	    bq_append(Inverse_Get, [New_var_Get28|Vars_Get29], TrueResult42),
	    ElseResult43=TrueResult42
	;   get_var(LEnv, sys_inverse, Inverse_Get31),
	    cl_car(Inverse_Get31, PredArgResult),
	    (   is_functionp(PredArgResult)
	    ->  get_var(LEnv, sys_inverse, Inverse_Get34),
		get_var(LEnv, sys_new_var, New_var_Get36),
		get_var(LEnv, sys_vars, Vars_Get35),
		bq_append(Vars_Get35, [New_var_Get36], Bq_append_Ret),
		bq_append([funcall|Inverse_Get34], Bq_append_Ret, TrueResult),
		ElseResult43=TrueResult
	    ;   get_var(LEnv, sys_inverse, Inverse_Get37),
		get_var(LEnv, sys_new_var, New_var_Get39),
		get_var(LEnv, sys_vars, Vars_Get38),
		bq_append(Vars_Get38, [New_var_Get39], Bq_append_Ret54),
		bq_append(Inverse_Get37, Bq_append_Ret54, ElseResult),
		ElseResult43=ElseResult
	    )
	),
	get_var(LEnv, sys_form, Form_Get44),
	cl_car(Form_Get44, Car_Ret),
	get_var(LEnv, sys_vars, Vars_Get45),
	nb_setval('$mv_return',
		  [sys_vars, Vals_Get22, CAR, ElseResult43, [Car_Ret|Vars_Get45]]),
	sys_vars=FnResult.
:- set_opv(f_sys_get_setf_method_inverse, classof, claz_function),
   set_opv(sys_get_setf_method_inverse, compile_as, kw_function),
   set_opv(sys_get_setf_method_inverse, function, f_sys_get_setf_method_inverse),
   DefunResult=sys_get_setf_method_inverse.
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:arglist_info(sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:init_args(exact_only, f_sys_get_setf_method_inverse))).
*/
/*
;; If a macro, expand one level and try again.  If not, go for the
*/
/*
;; SETF function.
*/
/*
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
        (get-setf-method-inverse form `(funcall #'(setf ,(car form)))
                                 t))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:2259 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'expand-or-get-setf-inverse',[form,environment],['multiple-value-bind',[expansion,expanded],['macroexpand-1',form,environment],[if,expanded,['get-setf-expansion',expansion,environment],['get-setf-method-inverse',form,['#BQ',[funcall,function([setf,['#COMMA',[car,form]]])]],t]]]])
wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]).
wl:arglist_info(sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_expand_or_get_setf_inverse).

/*

### Compiled:  `SYS::EXPAND-OR-GET-SETF-INVERSE` 
*/
f_sys_expand_or_get_setf_inverse(Form, Environment, FnResult) :-
	nop(global_env(Env)),
	Env21=[bv(sys_form, Form), bv(sys_environment, Environment)|Env],
	LEnv=[bv(sys_expansion, []), bv(sys_expanded, [])|Env21],
	get_var(LEnv, sys_environment, Environment_Get),
	get_var(LEnv, sys_form, Form_Get),
	cl_macroexpand_1([Form_Get, Environment_Get], Macroexpand_1_Ret),
	setq_from_values(LEnv, [sys_expansion, sys_expanded]),
	get_var(LEnv, sys_expanded, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, sys_environment, Environment_Get15),
	    get_var(LEnv, sys_expansion, Expansion_Get),
	    cl_get_setf_expansion(Expansion_Get,
				  [Environment_Get15],
				  TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, sys_form, Form_Get16),
	    f_sys_get_setf_method_inverse(Form_Get16,
					  
					  [ funcall,
					    function(
						     [ setf,
						       
						       [ '#COMMA',
							 [car, sys_form]
						       ]
						     ])
					  ],
					  t,
					  ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_sys_expand_or_get_setf_inverse, classof, claz_function),
   set_opv(sys_expand_or_get_setf_inverse, compile_as, kw_function),
   set_opv(sys_expand_or_get_setf_inverse,
	   function,
	   f_sys_expand_or_get_setf_inverse),
   DefunResult=sys_expand_or_get_setf_inverse.
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:arglist_info(sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:init_args(exact_only, f_sys_expand_or_get_setf_inverse))).
*/
/*
(defun get-setf-expansion (form &optional environment)
  (let (temp)
    (cond ((symbolp form)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 form environment)
             (if expanded
                 (get-setf-expansion expansion environment)
                 (let ((new-var (gensym)))
                   (values nil nil (list new-var)
                           `(setq ,form ,new-var) form)))))
          ((setq temp (get (car form) 'setf-inverse))
           (get-setf-method-inverse form `(,temp) nil))
          ((setq temp (get (car form) 'setf-expander))
           (funcall temp form environment))
          (t
           (expand-or-get-setf-inverse form environment)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:2581 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-expansion',[form,'&optional',environment],[let,[temp],[cond,[[symbolp,form],['multiple-value-bind',[expansion,expanded],['macroexpand-1',form,environment],[if,expanded,['get-setf-expansion',expansion,environment],[let,[['new-var',[gensym]]],[values,[],[],[list,'new-var'],['#BQ',[setq,['#COMMA',form],['#COMMA','new-var']]],form]]]]],[[setq,temp,[get,[car,form],[quote,'setf-inverse']]],['get-setf-method-inverse',form,['#BQ',[['#COMMA',temp]]],[]]],[[setq,temp,[get,[car,form],[quote,'setf-expander']]],[funcall,temp,form,environment]],[t,['expand-or-get-setf-inverse',form,environment]]]]])
wl:lambda_def(defun, get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]).
wl:arglist_info(get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_get_setf_expansion).

/*

### Compiled:  `CL:GET-SETF-EXPANSION` 
*/
cl_get_setf_expansion(Form, RestNKeys, LetResult16) :-
	nop(global_env(Env)),
	CDR=[[[bv(sys_form, Form), bv(sys_environment, Environment)]|Env]|Env],
	opt_var(Env, sys_environment, Environment, true, [], 1, RestNKeys),
	LEnv=[bv(sys_temp, [])|CDR],
	get_var(LEnv, sys_form, Form_Get),
	(   is_symbolp(Form_Get)
	->  LEnv17=[bv(sys_expansion, []), bv(sys_expanded, [])|LEnv],
	    get_var(LEnv17, sys_environment, Environment_Get),
	    get_var(LEnv17, sys_form, Form_Get18),
	    cl_macroexpand_1([Form_Get18, Environment_Get], Macroexpand_1_Ret),
	    setq_from_values(LEnv17, [sys_expansion, sys_expanded]),
	    get_var(LEnv17, sys_expanded, IFTEST20),
	    (   IFTEST20\==[]
	    ->  get_var(LEnv17, sys_environment, Environment_Get24),
		get_var(LEnv17, sys_expansion, Expansion_Get),
		cl_get_setf_expansion(Expansion_Get,
				      [Environment_Get24],
				      TrueResult),
		LetResult16=TrueResult
	    ;   cl_gensym(New_var_Init),
		LEnv27=[bv(sys_new_var, New_var_Init)|LEnv17],
		get_var(LEnv27, sys_new_var, New_var_Get),
		CAR=[New_var_Get],
		get_var(LEnv27, sys_form, Form_Get30),
		get_var(LEnv27, sys_new_var, New_var_Get31),
		nb_setval('$mv_return',
			  
			  [ [],
			    [],
			    CAR,
			    [setq, Form_Get30, New_var_Get31],
			    Form_Get30
			  ]),
		LetResult16=[]
	    )
	;   get_var(LEnv, sys_form, Form_Get37),
	    cl_car(Form_Get37, Get_Param),
	    cl_get(Get_Param, sys_setf_inverse, [], IFTEST34),
	    set_var(LEnv, sys_temp, IFTEST34),
	    (   IFTEST34\==[]
	    ->  get_var(LEnv, sys_form, Form_Get38),
		get_var(LEnv, sys_temp, Temp_Get),
		f_sys_get_setf_method_inverse(Form_Get38,
					      [Temp_Get],
					      [],
					      TrueResult50),
		LetResult16=TrueResult50
	    ;   get_var(LEnv, sys_form, Form_Get42),
		cl_car(Form_Get42, Get_Param59),
		cl_get(Get_Param59, sys_setf_expander, [], IFTEST40),
		set_var(LEnv, sys_temp, IFTEST40),
		(   IFTEST40\==[]
		->  get_var(LEnv, sys_environment, Environment_Get45),
		    get_var(LEnv, sys_form, Form_Get44),
		    get_var(LEnv, sys_temp, Temp_Get43),
		    cl_apply(Temp_Get43,
			     [Form_Get44, Environment_Get45],
			     TrueResult48),
		    LetResult16=TrueResult48
		;   get_var(LEnv, sys_environment, Environment_Get47),
		    get_var(LEnv, sys_form, Form_Get46),
		    f_sys_expand_or_get_setf_inverse(Form_Get46,
						     Environment_Get47,
						     ElseResult),
		    LetResult16=ElseResult
		)
	    )
	).
:- set_opv(cl_get_setf_expansion, classof, claz_function),
   set_opv(get_setf_expansion, compile_as, kw_function),
   set_opv(get_setf_expansion, function, cl_get_setf_expansion),
   DefunResult=get_setf_expansion.
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:lambda_def(defun, get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:arglist_info(get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:init_args(1, cl_get_setf_expansion))).
*/
/*
(defmacro abcl-setf (&rest args &environment environment)
  (let ((numargs (length args)))
    (cond
     ((= numargs 2)
      (let ((place (first args))
            (value-form (second args)))
        (if (atom place)
            `(setq ,place ,value-form)
            (progn
              (multiple-value-bind (dummies vals store-vars setter getter)
                  (get-setf-expansion place environment)
                (let ((inverse (get (car place) 'setf-inverse)))
                  (if (and inverse (eq inverse (car setter)))
                      (if (functionp inverse)
                          `(funcall ,inverse ,@(cdr place) ,value-form)
                          `(,inverse ,@(cdr place) ,value-form))
                      (if (or (null store-vars) (cdr store-vars))
                          `(let* (,@(mapcar #'list dummies vals))
                             (multiple-value-bind ,store-vars ,value-form
                               ,setter))
                          `(let* (,@(mapcar #'list dummies vals)
                                    ,(list (car store-vars) value-form))
                               ,setter)))))))))
     ((oddp numargs)
      (error "Odd number of arguments to SETF."))
     (t
      (do ((a args (cddr a)) (l nil))
          ((null a) `(progn ,@(nreverse l)))
        (setq l (cons (list 'setf (car a) (cadr a)) l)))))))

;;; Redefined in define-modify-macro.lisp.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:3317 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'abcl-setf',['&rest',args,'&environment',environment],[let,[[numargs,[length,args]]],[cond,[[=,numargs,2],[let,[[place,[first,args]],['value-form',[second,args]]],[if,[atom,place],['#BQ',[setq,['#COMMA',place],['#COMMA','value-form']]],[progn,['multiple-value-bind',[dummies,vals,'store-vars',setter,getter],['get-setf-expansion',place,environment],[let,[[inverse,[get,[car,place],[quote,'setf-inverse']]]],[if,[and,inverse,[eq,inverse,[car,setter]]],[if,[functionp,inverse],['#BQ',[funcall,['#COMMA',inverse],['#BQ-COMMA-ELIPSE',[cdr,place]],['#COMMA','value-form']]],['#BQ',[['#COMMA',inverse],['#BQ-COMMA-ELIPSE',[cdr,place]],['#COMMA','value-form']]]],[if,[or,[null,'store-vars'],[cdr,'store-vars']],['#BQ',['let*',[['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]]],['multiple-value-bind',['#COMMA','store-vars'],['#COMMA','value-form'],['#COMMA',setter]]]],['#BQ',['let*',[['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],['#COMMA',[list,[car,'store-vars'],'value-form']]],['#COMMA',setter]]]]]]]]]]],[[oddp,numargs],[error,'$STRING'("Odd number of arguments to SETF.")]],[t,[do,[[a,args,[cddr,a]],[l,[]]],[[null,a],['#BQ',[progn,['#BQ-COMMA-ELIPSE',[nreverse,l]]]]],[setq,l,[cons,[list,[quote,setf],[car,a],[cadr,a]],l]]]]]]])
wl:lambda_def(defmacro, sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [progn, [let, [[sys_numargs, [length, args]]], [cond, [[=, sys_numargs, 2], [let, [[sys_place, [first, args]], [sys_value_form, [second, args]]], [if, [atom, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]], [progn, [multiple_value_bind, [sys_dummies, sys_vals, sys_store_vars, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_environment], [let, [[sys_inverse, [get, [car, sys_place], [quote, sys_setf_inverse]]]], [if, [and, sys_inverse, [eq, sys_inverse, [car, sys_setter]]], [if, [functionp, sys_inverse], ['#BQ', [funcall, ['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]], ['#BQ', [['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]]], [if, [or, [null, sys_store_vars], [cdr, sys_store_vars]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]]], [multiple_value_bind, ['#COMMA', sys_store_vars], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], ['#COMMA', [list, [car, sys_store_vars], sys_value_form]]], ['#COMMA', sys_setter]]]]]]]]]]], [[oddp, sys_numargs], [error, '$ARRAY'([*], claz_base_character, "Odd number of arguments to SETF.")]], [t, [do, [[sys_a, args, [cddr, sys_a]], [sys_l, []]], [[null, sys_a], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]]], [setq, sys_l, [cons, [list, [quote, setf], [car, sys_a], [cadr, sys_a]], sys_l]]]]]]]).
wl:arglist_info(sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[args, sys_environment], opt:0, req:0, rest:[args], sublists:0, whole:0}).
wl: init_args(0, f_sys_abcl_setf).

/*

### Compiled:  `SYS::ABCL-SETF` 
*/
f_sys_abcl_setf(RestNKeys, FnResult) :-
	global_env(Get_env_Param),
	GEnv=[[[bv(args, RestNKeys), bv(sys_environment, Environment)]|Get_env_Param]|Get_env_Param],
	get_env(Get_env_Param, sys_environment, Environment),
	catch(( ( get_var(GEnv, args, Args_Get),
		  cl_length(Args_Get, Numargs_Init),
		  LEnv=[bv(sys_numargs, Numargs_Init)|GEnv],
		  get_var(LEnv, sys_numargs, Numargs_Get),
		  (   Numargs_Get=:=2
		  ->  get_var(LEnv, args, Args_Get19),
		      cl_car(Args_Get19, Place_Init),
		      get_var(LEnv, args, Args_Get20),
		      cl_second(Args_Get20, Value_form_Init),
		      LEnv18=[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)|LEnv],
		      get_var(LEnv18, sys_place, Place_Get),
		      (   Place_Get\=[CAR|CDR]
		      ->  get_var(LEnv18, sys_place, Place_Get27),
			  get_var(LEnv18, sys_value_form, Value_form_Get),
			  LetResult17=[setq, Place_Get27, Value_form_Get]
		      ;   LEnv31=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_store_vars, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv18],
			  get_var(LEnv31, sys_environment, Environment_Get),
			  get_var(LEnv31, sys_place, Place_Get32),
			  cl_get_setf_expansion(Place_Get32,
						[Environment_Get],
						Setf_expansion_Ret),
			  setq_from_values(LEnv31,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_store_vars,
					     sys_setter,
					     sys_getter
					   ]),
			  get_var(LEnv31, sys_place, Place_Get37),
			  cl_car(Place_Get37, Get_Param),
			  cl_get(Get_Param, sys_setf_inverse, [], Inverse_Init),
			  LEnv36=[bv(sys_inverse, Inverse_Init)|LEnv31],
			  get_var(LEnv36, sys_inverse, IFTEST41),
			  (   IFTEST41\==[]
			  ->  get_var(LEnv36, sys_inverse, Inverse_Get44),
			      get_var(LEnv36, sys_setter, Setter_Get),
			      cl_car(Setter_Get, Car_Ret),
			      cl_eq(Inverse_Get44, Car_Ret, TrueResult),
			      IFTEST39=TrueResult
			  ;   IFTEST39=[]
			  ),
			  (   IFTEST39\==[]
			  ->  get_var(LEnv36, sys_inverse, Inverse_Get48),
			      (   is_functionp(Inverse_Get48)
			      ->  get_var(LEnv36, sys_inverse, Inverse_Get51),
				  get_var(LEnv36, sys_place, Place_Get52),
				  cl_cdr(Place_Get52, Cdr_Ret),
				  get_var(LEnv36,
					  sys_value_form,
					  Value_form_Get53),
				  bq_append([Inverse_Get51|Cdr_Ret],
					    [Value_form_Get53],
					    Bq_append_Ret),
				  LetResult17=[funcall|Bq_append_Ret]
			      ;   get_var(LEnv36, sys_inverse, Inverse_Get54),
				  get_var(LEnv36, sys_place, Place_Get55),
				  cl_cdr(Place_Get55, Cdr_Ret138),
				  get_var(LEnv36,
					  sys_value_form,
					  Value_form_Get56),
				  bq_append([Inverse_Get54|Cdr_Ret138],
					    [Value_form_Get56],
					    ElseResult),
				  LetResult17=ElseResult
			      )
			  ;   (   get_var(LEnv36,
					  sys_store_vars,
					  Store_vars_Get),
				  cl_null(Store_vars_Get, FORM1_Res),
				  FORM1_Res\==[],
				  IFTEST58=FORM1_Res
			      ->  true
			      ;   get_var(LEnv36,
					  sys_store_vars,
					  Store_vars_Get61),
				  cl_cdr(Store_vars_Get61, Cdr_Ret139),
				  IFTEST58=Cdr_Ret139
			      ),
			      (   IFTEST58\==[]
			      ->  get_var(LEnv36, sys_dummies, Dummies_Get),
				  get_var(LEnv36, sys_vals, Vals_Get),
				  cl_mapcar(cl_list,
					    [Dummies_Get, Vals_Get],
					    Mapcar_Ret),
				  get_var(LEnv36, sys_setter, Setter_Get67),
				  get_var(LEnv36,
					  sys_store_vars,
					  Store_vars_Get65),
				  get_var(LEnv36,
					  sys_value_form,
					  Value_form_Get66),
				  LetResult17=[let_xx, Mapcar_Ret, [multiple_value_bind, Store_vars_Get65, Value_form_Get66, Setter_Get67]]
			      ;   get_var(LEnv36, sys_dummies, Dummies_Get68),
				  get_var(LEnv36, sys_vals, Vals_Get69),
				  cl_mapcar(cl_list,
					    [Dummies_Get68, Vals_Get69],
					    Bq_append_Param),
				  get_var(LEnv36,
					  sys_store_vars,
					  Store_vars_Get70),
				  cl_car(Store_vars_Get70, Car_Ret141),
				  get_var(LEnv36,
					  sys_value_form,
					  Value_form_Get71),
				  CAR143=[Car_Ret141, Value_form_Get71],
				  bq_append(Bq_append_Param,
					    [CAR143],
					    Bq_append_Ret142),
				  get_var(LEnv36, sys_setter, Setter_Get72),
				  LetResult17=[let_xx, Bq_append_Ret142, Setter_Get72]
			      )
			  )
		      )
		  ;   get_var(LEnv, sys_numargs, Numargs_Get77),
		      (   mth:is_oddp(Numargs_Get77)
		      ->  cl_error(
				   [ '$ARRAY'([*],
					      claz_base_character,
					      "Odd number of arguments to SETF.")
				   ],
				   TrueResult119),
			  LetResult17=TrueResult119
		      ;   get_var(LEnv, args, Args_Get83),
			  AEnv=[bv(sys_a, Args_Get83), bv(sys_l, [])|LEnv],
			  catch(( call_addr_block(AEnv,
						  (push_label(do_label_1), get_var(AEnv, sys_a, IFTEST103), (IFTEST103==[]->get_var(AEnv, sys_l, L_Get108), cl_nreverse(L_Get108, Nreverse_Ret), throw(block_exit([], [progn|Nreverse_Ret])), _TBResult=ThrowResult107;get_var(AEnv, sys_a, A_Get111), cl_car(A_Get111, Car_Ret145), get_var(AEnv, sys_a, A_Get112), cl_cadr(A_Get112, Cadr_Ret), CAR147=[setf, Car_Ret145, Cadr_Ret], get_var(AEnv, sys_l, L_Get113), L=[CAR147|L_Get113], set_var(AEnv, sys_l, L), get_var(AEnv, sys_a, A_Get114), cl_cddr(A_Get114, A), set_var(AEnv, sys_a, A), goto(do_label_1, AEnv), _TBResult=_GORES115)),
						  
						  [ addr(addr_tagbody_1_do_label_1,
							 do_label_1,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_a, IFTEST86), (IFTEST86==[]->get_var(AEnv, sys_l, Nreverse_Param), cl_nreverse(Nreverse_Param, Nreverse_Ret148), throw(block_exit([], [progn|Nreverse_Ret148])), _31267692=ThrowResult;get_var(AEnv, sys_a, A_Get94), cl_car(A_Get94, Car_Ret149), get_var(AEnv, sys_a, A_Get95), cl_cadr(A_Get95, Cadr_Ret150), CAR151=[setf, Car_Ret149, Cadr_Ret150], get_var(AEnv, sys_l, L_Get96), Set_var_Ret=[CAR151|L_Get96], set_var(AEnv, sys_l, Set_var_Ret), get_var(AEnv, sys_a, A_Get97), cl_cddr(A_Get97, Cddr_Ret), set_var(AEnv, sys_a, Cddr_Ret), goto(do_label_1, AEnv), _31267692=_GORES)))
						  ]),
				  []=LetResult81
				),
				block_exit([], LetResult81),
				true),
			  LetResult17=LetResult81
		      )
		  )
		),
		LetResult17=MFResult
	      ),
	      block_exit(sys_abcl_setf, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(f_sys_abcl_setf, classof, claz_macro),
   set_opv(sys_abcl_setf, compile_as, kw_operator),
   set_opv(sys_abcl_setf, function, f_sys_abcl_setf),
   DefMacroResult=sys_abcl_setf.
/*
:- side_effect(assert_lsp(sys_abcl_setf,
			  wl:lambda_def(defmacro, sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [progn, [let, [[sys_numargs, [length, args]]], [cond, [[=, sys_numargs, 2], [let, [[sys_place, [first, args]], [sys_value_form, [second, args]]], [if, [atom, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]], [progn, [multiple_value_bind, [sys_dummies, sys_vals, sys_store_vars, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_environment], [let, [[sys_inverse, [get, [car, sys_place], [quote, sys_setf_inverse]]]], [if, [and, sys_inverse, [eq, sys_inverse, [car, sys_setter]]], [if, [functionp, sys_inverse], ['#BQ', [funcall, ['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]], ['#BQ', [['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]]], [if, [or, [null, sys_store_vars], [cdr, sys_store_vars]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]]], [multiple_value_bind, ['#COMMA', sys_store_vars], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], ['#COMMA', [list, [car, sys_store_vars], sys_value_form]]], ['#COMMA', sys_setter]]]]]]]]]]], [[oddp, sys_numargs], [error, '$ARRAY'([*], claz_base_character, "Odd number of arguments to SETF.")]], [t, [do, [[sys_a, args, [cddr, sys_a]], [sys_l, []]], [[null, sys_a], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]]], [setq, sys_l, [cons, [list, [quote, setf], [car, sys_a], [cadr, sys_a]], sys_l]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_abcl_setf,
			  wl:arglist_info(sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[args, sys_environment], opt:0, req:0, rest:[args], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_abcl_setf, wl:init_args(0, f_sys_abcl_setf))).
*/
/*
;; Redefined in define-modify-macro.lisp.
*/
/*
(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

;;; Redefined in define-modify-macro.lisp.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4767 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,incf,[place,'&optional',[delta,1]],['#BQ',[setf,['#COMMA',place],[+,['#COMMA',place],['#COMMA',delta]]]]])
wl:lambda_def(defmacro, incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]).
wl:arglist_info(incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_incf).

/*

### Compiled:  `CL:INCF` 
*/
cl_incf(Place, RestNKeys, FnResult) :-
	global_env(Opt_var_Param),
	GEnv=[[[bv(sys_place, Place), bv(sys_delta, Delta)]|Opt_var_Param]|Opt_var_Param],
	opt_var(Opt_var_Param, sys_delta, Delta, true, 1, 1, RestNKeys),
	get_var(GEnv, sys_delta, Delta_Get),
	get_var(GEnv, sys_place, Place_Get9),
	[setf, Place_Get9, [+, Place_Get9, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_incf, classof, claz_macro),
   set_opv(incf, compile_as, kw_operator),
   set_opv(incf, function, cl_incf),
   DefMacroResult=incf.
/*
:- side_effect(assert_lsp(incf,
			  wl:lambda_def(defmacro, incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]))).
*/
/*
:- side_effect(assert_lsp(incf,
			  wl:arglist_info(incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(incf, wl:init_args(1, cl_incf))).
*/
/*
;; Redefined in define-modify-macro.lisp.
*/
/*
(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

;; (defsetf subseq (sequence start &optional (end nil)) (v)
;;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
;;      ,v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4894 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,decf,[place,'&optional',[delta,1]],['#BQ',[setf,['#COMMA',place],[-,['#COMMA',place],['#COMMA',delta]]]]])
wl:lambda_def(defmacro, decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [-, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]).
wl:arglist_info(decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_decf).

/*

### Compiled:  `CL:DECF` 
*/
cl_decf(Place, RestNKeys, FnResult) :-
	global_env(Opt_var_Param),
	GEnv=[[[bv(sys_place, Place), bv(sys_delta, Delta)]|Opt_var_Param]|Opt_var_Param],
	opt_var(Opt_var_Param, sys_delta, Delta, true, 1, 1, RestNKeys),
	get_var(GEnv, sys_delta, Delta_Get),
	get_var(GEnv, sys_place, Place_Get9),
	[setf, Place_Get9, [-, Place_Get9, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_decf, classof, claz_macro),
   set_opv(decf, compile_as, kw_operator),
   set_opv(decf, function, cl_decf),
   DefMacroResult=decf.
/*
:- side_effect(assert_lsp(decf,
			  wl:lambda_def(defmacro, decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [-, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]))).
*/
/*
:- side_effect(assert_lsp(decf,
			  wl:arglist_info(decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(decf, wl:init_args(1, cl_decf))).
*/
/*
; (defsetf subseq (sequence start &optional (end nil)) (v)
*/
/*
;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
*/
/*
;      ,v))
*/
/*
(defun %set-subseq (sequence start &rest rest)
  (let ((end nil) v)
    (ecase (length rest)
      (1
       (setq v (car rest)))
      (2
       (setq end (car rest)
             v (cadr rest))))
    (progn
      (replace sequence v :start1 start :end1 end)
      v)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5115 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-subseq',[sequence,start,'&rest',rest],[let,[[end,[]],v],[ecase,[length,rest],[1,[setq,v,[car,rest]]],[2,[setq,end,[car,rest],v,[cadr,rest]]]],[progn,[replace,sequence,v,':start1',start,':end1',end],v]]])
/*
% ecase:-[[1,[setq,sys_v,[car,rest]]],[2,[setq,end,[car,rest],sys_v,[cadr,rest]]]].
*/
/*
% conds:-[[[eq,_40432842,[quote,1]],[progn,[setq,sys_v,[car,rest]]]],[[eq,_40432842,[quote,2]],[progn,[setq,end,[car,rest],sys_v,[cadr,rest]]]],[t,[type_error,_40433078,[quote,[member,1,2]]]]].
*/
wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], [[let, [[end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, start, kw_end1, end], sys_v]]]).
wl:arglist_info(sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, start, rest], opt:0, req:[sequence, start], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, f_sys_pf_set_subseq).

/*

### Compiled:  `SYS::%SET-SUBSEQ` 
*/
f_sys_pf_set_subseq(Sequence, Start, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	CDR=[[[bv(sequence, Sequence), bv(start, Start), bv(rest, RestNKeys)]|Env]|Env],
	LEnv=[bv(end, []), bv(sys_v, [])|CDR],
	get_var(LEnv, rest, Rest_Get),
	cl_length(Rest_Get, Key),
	(   is_eq(Key, 1)
	->  get_var(LEnv, rest, Rest_Get17),
	    cl_car(Rest_Get17, TrueResult24),
	    set_var(LEnv, sys_v, TrueResult24),
	    ElseResult25=TrueResult24
	;   is_eq(Key, 2)
	->  get_var(LEnv, rest, Rest_Get20),
	    cl_car(Rest_Get20, End),
	    set_var(LEnv, end, End),
	    get_var(LEnv, rest, Rest_Get21),
	    cl_cadr(Rest_Get21, TrueResult),
	    set_var(LEnv, sys_v, TrueResult),
	    ElseResult25=TrueResult
	;   cl_type_error(CAR, [member, 1, 2], ElseResult),
	    ElseResult25=ElseResult
	),
	get_var(LEnv, end, End_Get),
	get_var(LEnv, sequence, Sequence_Get),
	get_var(LEnv, start, Start_Get),
	get_var(LEnv, sys_v, V_Get),
	cl_replace(Sequence_Get,
		   V_Get,
		   [kw_start1, Start_Get, kw_end1, End_Get],
		   Replace_Ret),
	get_var(LEnv, sys_v, V_Get30),
	V_Get30=FnResult.
:- set_opv(f_sys_pf_set_subseq, classof, claz_function),
   set_opv(sys_pf_set_subseq, compile_as, kw_function),
   set_opv(sys_pf_set_subseq, function, f_sys_pf_set_subseq),
   DefunResult=sys_pf_set_subseq.
/*
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], [[let, [[end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, start, kw_end1, end], sys_v]]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  wl:arglist_info(sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, start, rest], opt:0, req:[sequence, start], rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  wl:init_args(2, f_sys_pf_set_subseq))).
*/
/*
(defun %define-setf-macro (name expander inverse doc)
  (declare (ignore doc)) ; FIXME
  (when inverse
    (put name 'setf-inverse inverse))
  (when expander
    (put name 'setf-expander expander))
  name)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5398 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%define-setf-macro',[name,expander,inverse,doc],[declare,[ignore,doc]],[when,inverse,[put,name,[quote,'setf-inverse'],inverse]],[when,expander,[put,name,[quote,'setf-expander'],expander]],name])
wl:lambda_def(defun, sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [[declare, [ignore, sys_doc]], [when, sys_inverse, [sys_put, sys_name, [quote, sys_setf_inverse], sys_inverse]], [when, sys_expander, [sys_put, sys_name, [quote, sys_setf_expander], sys_expander]], sys_name]).
wl:arglist_info(sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_define_setf_type_macro).

/*

### Compiled:  `SYS::%DEFINE-SETF-MACRO` 
*/
f_sys_pf_define_setf_type_macro(Name, Expander, Inverse, Doc, FnResult) :-
	nop(global_env(Env)),
	Env21=[bv(sys_name, Name), bv(sys_expander, Expander), bv(sys_inverse, Inverse), bv(sys_doc, Doc)|Env],
	cl_declare([ignore, sys_doc], Declare_Ret),
	get_var(Env21, sys_inverse, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env21, sys_inverse, Inverse_Get10),
	    get_var(Env21, sys_name, Name_Get),
	    f_sys_put(Name_Get, sys_setf_inverse, Inverse_Get10, TrueResult),
	    _42708638=TrueResult
	;   _42708638=[]
	),
	get_var(Env21, sys_expander, IFTEST12),
	(   IFTEST12\==[]
	->  get_var(Env21, sys_expander, Expander_Get16),
	    get_var(Env21, sys_name, Name_Get15),
	    f_sys_put(Name_Get15,
		      sys_setf_expander,
		      Expander_Get16,
		      TrueResult17),
	    _42728344=TrueResult17
	;   _42728344=[]
	),
	get_var(Env21, sys_name, Name_Get18),
	Name_Get18=FnResult.
:- set_opv(f_sys_pf_define_setf_type_macro, classof, claz_function),
   set_opv(sys_pf_define_setf_macro, compile_as, kw_function),
   set_opv(sys_pf_define_setf_macro, function, f_sys_pf_define_setf_type_macro),
   DefunResult=sys_pf_define_setf_macro.
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:lambda_def(defun, sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [[declare, [ignore, sys_doc]], [when, sys_inverse, [sys_put, sys_name, [quote, sys_setf_inverse], sys_inverse]], [when, sys_expander, [sys_put, sys_name, [quote, sys_setf_expander], sys_expander]], sys_name]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:arglist_info(sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:init_args(exact_only, f_sys_pf_define_setf_type_macro))).
*/
/*
 FIXME
*/
/*
(defmacro defsetf (access-function update-function)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (put ',access-function 'setf-inverse ',update-function)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5613 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defsetf,['access-function','update-function'],['#BQ',['eval-when',[':load-toplevel',':compile-toplevel',':execute'],[put,[quote,['#COMMA','access-function']],[quote,'setf-inverse'],[quote,['#COMMA','update-function']]]]]])
wl:lambda_def(defmacro, defsetf, cl_defsetf, [sys_access_function, sys_update_function], [progn, ['#BQ', [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, ['#COMMA', sys_access_function]], [quote, sys_setf_inverse], [quote, ['#COMMA', sys_update_function]]]]]]).
wl:arglist_info(defsetf, cl_defsetf, [sys_access_function, sys_update_function], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_defsetf).

/*

### Compiled:  `CL:DEFSETF` 
*/
cl_defsetf(Access_function, Update_function, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(sys_access_function, Access_function), bv(sys_update_function, Update_function)|Global_env_Ret],
	get_var(Env, sys_access_function, Access_function_Get),
	get_var(Env, sys_update_function, Update_function_Get),
	[eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, Access_function_Get], [quote, sys_setf_inverse], [quote, Update_function_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_defsetf, classof, claz_macro),
   set_opv(defsetf, compile_as, kw_operator),
   set_opv(defsetf, function, cl_defsetf),
   DefMacroResult=defsetf.
/*
:- side_effect(assert_lsp(defsetf,
			  wl:lambda_def(defmacro, defsetf, cl_defsetf, [sys_access_function, sys_update_function], [progn, ['#BQ', [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, ['#COMMA', sys_access_function]], [quote, sys_setf_inverse], [quote, ['#COMMA', sys_update_function]]]]]]))).
*/
/*
:- side_effect(assert_lsp(defsetf,
			  wl:arglist_info(defsetf, cl_defsetf, [sys_access_function, sys_update_function], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(defsetf, wl:init_args(exact_only, cl_defsetf))).
*/
/*
(defun %set-caar (x v) (set-car (car x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5791 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caar',[x,v],['set-car',[car,x],v]])
wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caar).

/*

### Compiled:  `SYS::%SET-CAAR` 
*/
f_sys_pf_set_caar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_car(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caar, classof, claz_function),
   set_opv(sys_pf_set_caar, compile_as, kw_function),
   set_opv(sys_pf_set_caar, function, f_sys_pf_set_caar),
   DefunResult=sys_pf_set_caar.
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:arglist_info(sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:init_args(exact_only, f_sys_pf_set_caar))).
*/
/*
(defun %set-cadr (x v) (set-car (cdr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5836 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadr',[x,v],['set-car',[cdr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cadr).

/*

### Compiled:  `SYS::%SET-CADR` 
*/
f_sys_pf_set_cadr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadr, classof, claz_function),
   set_opv(sys_pf_set_cadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadr, function, f_sys_pf_set_cadr),
   DefunResult=sys_pf_set_cadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:arglist_info(sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:init_args(exact_only, f_sys_pf_set_cadr))).
*/
/*
(defun %set-cdar (x v) (set-cdr (car x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5881 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdar',[x,v],['set-cdr',[car,x],v]])
wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdar).

/*

### Compiled:  `SYS::%SET-CDAR` 
*/
f_sys_pf_set_cdar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_car(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdar, classof, claz_function),
   set_opv(sys_pf_set_cdar, compile_as, kw_function),
   set_opv(sys_pf_set_cdar, function, f_sys_pf_set_cdar),
   DefunResult=sys_pf_set_cdar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:arglist_info(sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:init_args(exact_only, f_sys_pf_set_cdar))).
*/
/*
(defun %set-cddr (x v) (set-cdr (cdr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5926 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddr',[x,v],['set-cdr',[cdr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cddr).

/*

### Compiled:  `SYS::%SET-CDDR` 
*/
f_sys_pf_set_cddr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdr(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddr, classof, claz_function),
   set_opv(sys_pf_set_cddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddr, function, f_sys_pf_set_cddr),
   DefunResult=sys_pf_set_cddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:arglist_info(sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:init_args(exact_only, f_sys_pf_set_cddr))).
*/
/*
(defun %set-caaar (x v) (set-car (caar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5971 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaar',[x,v],['set-car',[caar,x],v]])
wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caaar).

/*

### Compiled:  `SYS::%SET-CAAAR` 
*/
f_sys_pf_set_caaar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caar(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaar, classof, claz_function),
   set_opv(sys_pf_set_caaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaar, function, f_sys_pf_set_caaar),
   DefunResult=sys_pf_set_caaar.
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:arglist_info(sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:init_args(exact_only, f_sys_pf_set_caaar))).
*/
/*
(defun %set-cadar (x v) (set-car (cdar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6018 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadar',[x,v],['set-car',[cdar,x],v]])
wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cadar).

/*

### Compiled:  `SYS::%SET-CADAR` 
*/
f_sys_pf_set_cadar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdar(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadar, classof, claz_function),
   set_opv(sys_pf_set_cadar, compile_as, kw_function),
   set_opv(sys_pf_set_cadar, function, f_sys_pf_set_cadar),
   DefunResult=sys_pf_set_cadar.
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:arglist_info(sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:init_args(exact_only, f_sys_pf_set_cadar))).
*/
/*
(defun %set-cdaar (x v) (set-cdr (caar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6065 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaar',[x,v],['set-cdr',[caar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdaar).

/*

### Compiled:  `SYS::%SET-CDAAR` 
*/
f_sys_pf_set_cdaar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caar(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaar, classof, claz_function),
   set_opv(sys_pf_set_cdaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaar, function, f_sys_pf_set_cdaar),
   DefunResult=sys_pf_set_cdaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:arglist_info(sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:init_args(exact_only, f_sys_pf_set_cdaar))).
*/
/*
(defun %set-cddar (x v) (set-cdr (cdar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6112 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddar',[x,v],['set-cdr',[cdar,x],v]])
wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cddar).

/*

### Compiled:  `SYS::%SET-CDDAR` 
*/
f_sys_pf_set_cddar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdar(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddar, classof, claz_function),
   set_opv(sys_pf_set_cddar, compile_as, kw_function),
   set_opv(sys_pf_set_cddar, function, f_sys_pf_set_cddar),
   DefunResult=sys_pf_set_cddar.
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:arglist_info(sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:init_args(exact_only, f_sys_pf_set_cddar))).
*/
/*
(defun %set-caadr (x v) (set-car (cadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6159 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caadr',[x,v],['set-car',[cadr,x],v]])
wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caadr).

/*

### Compiled:  `SYS::%SET-CAADR` 
*/
f_sys_pf_set_caadr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cadr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadr, classof, claz_function),
   set_opv(sys_pf_set_caadr, compile_as, kw_function),
   set_opv(sys_pf_set_caadr, function, f_sys_pf_set_caadr),
   DefunResult=sys_pf_set_caadr.
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:arglist_info(sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:init_args(exact_only, f_sys_pf_set_caadr))).
*/
/*
(defun %set-caddr (x v) (set-car (cddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6206 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caddr',[x,v],['set-car',[cddr,x],v]])
wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caddr).

/*

### Compiled:  `SYS::%SET-CADDR` 
*/
f_sys_pf_set_caddr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddr, classof, claz_function),
   set_opv(sys_pf_set_caddr, compile_as, kw_function),
   set_opv(sys_pf_set_caddr, function, f_sys_pf_set_caddr),
   DefunResult=sys_pf_set_caddr.
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:arglist_info(sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:init_args(exact_only, f_sys_pf_set_caddr))).
*/
/*
(defun %set-cdadr (x v) (set-cdr (cadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6253 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdadr',[x,v],['set-cdr',[cadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdadr).

/*

### Compiled:  `SYS::%SET-CDADR` 
*/
f_sys_pf_set_cdadr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cadr(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadr, classof, claz_function),
   set_opv(sys_pf_set_cdadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdadr, function, f_sys_pf_set_cdadr),
   DefunResult=sys_pf_set_cdadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:arglist_info(sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:init_args(exact_only, f_sys_pf_set_cdadr))).
*/
/*
(defun %set-cdddr (x v) (set-cdr (cddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6300 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdddr',[x,v],['set-cdr',[cddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdddr).

/*

### Compiled:  `SYS::%SET-CDDDR` 
*/
f_sys_pf_set_cdddr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddr(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddr, classof, claz_function),
   set_opv(sys_pf_set_cdddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdddr, function, f_sys_pf_set_cdddr),
   DefunResult=sys_pf_set_cdddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:arglist_info(sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:init_args(exact_only, f_sys_pf_set_cdddr))).
*/
/*
(defun %set-caaaar (x v) (set-car (caaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6347 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaaar',[x,v],['set-car',[caaar,x],v]])
wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caaaar).

/*

### Compiled:  `SYS::%SET-CAAAAR` 
*/
f_sys_pf_set_caaaar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caaar(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaaar, classof, claz_function),
   set_opv(sys_pf_set_caaaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaaar, function, f_sys_pf_set_caaaar),
   DefunResult=sys_pf_set_caaaar.
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:arglist_info(sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:init_args(exact_only, f_sys_pf_set_caaaar))).
*/
/*
(defun %set-cadaar (x v) (set-car (cdaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6396 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadaar',[x,v],['set-car',[cdaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cadaar).

/*

### Compiled:  `SYS::%SET-CADAAR` 
*/
f_sys_pf_set_cadaar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdaar(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadaar, classof, claz_function),
   set_opv(sys_pf_set_cadaar, compile_as, kw_function),
   set_opv(sys_pf_set_cadaar, function, f_sys_pf_set_cadaar),
   DefunResult=sys_pf_set_cadaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:arglist_info(sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:init_args(exact_only, f_sys_pf_set_cadaar))).
*/
/*
(defun %set-cdaaar (x v) (set-cdr (caaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6445 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaaar',[x,v],['set-cdr',[caaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdaaar).

/*

### Compiled:  `SYS::%SET-CDAAAR` 
*/
f_sys_pf_set_cdaaar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caaar(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaaar, classof, claz_function),
   set_opv(sys_pf_set_cdaaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaaar, function, f_sys_pf_set_cdaaar),
   DefunResult=sys_pf_set_cdaaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:arglist_info(sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:init_args(exact_only, f_sys_pf_set_cdaaar))).
*/
/*
(defun %set-cddaar (x v) (set-cdr (cdaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6494 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddaar',[x,v],['set-cdr',[cdaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cddaar).

/*

### Compiled:  `SYS::%SET-CDDAAR` 
*/
f_sys_pf_set_cddaar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdaar(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddaar, classof, claz_function),
   set_opv(sys_pf_set_cddaar, compile_as, kw_function),
   set_opv(sys_pf_set_cddaar, function, f_sys_pf_set_cddaar),
   DefunResult=sys_pf_set_cddaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:arglist_info(sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:init_args(exact_only, f_sys_pf_set_cddaar))).
*/
/*
(defun %set-caadar (x v) (set-car (cadar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6543 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caadar',[x,v],['set-car',[cadar,x],v]])
wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caadar).

/*

### Compiled:  `SYS::%SET-CAADAR` 
*/
f_sys_pf_set_caadar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cadar(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadar, classof, claz_function),
   set_opv(sys_pf_set_caadar, compile_as, kw_function),
   set_opv(sys_pf_set_caadar, function, f_sys_pf_set_caadar),
   DefunResult=sys_pf_set_caadar.
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:arglist_info(sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:init_args(exact_only, f_sys_pf_set_caadar))).
*/
/*
(defun %set-caddar (x v) (set-car (cddar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6592 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caddar',[x,v],['set-car',[cddar,x],v]])
wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caddar).

/*

### Compiled:  `SYS::%SET-CADDAR` 
*/
f_sys_pf_set_caddar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddar(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddar, classof, claz_function),
   set_opv(sys_pf_set_caddar, compile_as, kw_function),
   set_opv(sys_pf_set_caddar, function, f_sys_pf_set_caddar),
   DefunResult=sys_pf_set_caddar.
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:arglist_info(sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:init_args(exact_only, f_sys_pf_set_caddar))).
*/
/*
(defun %set-cdadar (x v) (set-cdr (cadar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6641 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdadar',[x,v],['set-cdr',[cadar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdadar).

/*

### Compiled:  `SYS::%SET-CDADAR` 
*/
f_sys_pf_set_cdadar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cadar(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadar, classof, claz_function),
   set_opv(sys_pf_set_cdadar, compile_as, kw_function),
   set_opv(sys_pf_set_cdadar, function, f_sys_pf_set_cdadar),
   DefunResult=sys_pf_set_cdadar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:arglist_info(sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:init_args(exact_only, f_sys_pf_set_cdadar))).
*/
/*
(defun %set-cdddar (x v) (set-cdr (cddar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6690 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdddar',[x,v],['set-cdr',[cddar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdddar).

/*

### Compiled:  `SYS::%SET-CDDDAR` 
*/
f_sys_pf_set_cdddar(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddar(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddar, classof, claz_function),
   set_opv(sys_pf_set_cdddar, compile_as, kw_function),
   set_opv(sys_pf_set_cdddar, function, f_sys_pf_set_cdddar),
   DefunResult=sys_pf_set_cdddar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:arglist_info(sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:init_args(exact_only, f_sys_pf_set_cdddar))).
*/
/*
(defun %set-caaadr (x v) (set-car (caadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6739 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaadr',[x,v],['set-car',[caadr,x],v]])
wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caaadr).

/*

### Compiled:  `SYS::%SET-CAAADR` 
*/
f_sys_pf_set_caaadr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caadr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaadr, classof, claz_function),
   set_opv(sys_pf_set_caaadr, compile_as, kw_function),
   set_opv(sys_pf_set_caaadr, function, f_sys_pf_set_caaadr),
   DefunResult=sys_pf_set_caaadr.
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:arglist_info(sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:init_args(exact_only, f_sys_pf_set_caaadr))).
*/
/*
(defun %set-cadadr (x v) (set-car (cdadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6788 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadadr',[x,v],['set-car',[cdadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cadadr).

/*

### Compiled:  `SYS::%SET-CADADR` 
*/
f_sys_pf_set_cadadr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdadr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadadr, classof, claz_function),
   set_opv(sys_pf_set_cadadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadadr, function, f_sys_pf_set_cadadr),
   DefunResult=sys_pf_set_cadadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:arglist_info(sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:init_args(exact_only, f_sys_pf_set_cadadr))).
*/
/*
(defun %set-cdaadr (x v) (set-cdr (caadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6837 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaadr',[x,v],['set-cdr',[caadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdaadr).

/*

### Compiled:  `SYS::%SET-CDAADR` 
*/
f_sys_pf_set_cdaadr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caadr(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaadr, classof, claz_function),
   set_opv(sys_pf_set_cdaadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaadr, function, f_sys_pf_set_cdaadr),
   DefunResult=sys_pf_set_cdaadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:arglist_info(sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:init_args(exact_only, f_sys_pf_set_cdaadr))).
*/
/*
(defun %set-cddadr (x v) (set-cdr (cdadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6886 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddadr',[x,v],['set-cdr',[cdadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cddadr).

/*

### Compiled:  `SYS::%SET-CDDADR` 
*/
f_sys_pf_set_cddadr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdadr(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddadr, classof, claz_function),
   set_opv(sys_pf_set_cddadr, compile_as, kw_function),
   set_opv(sys_pf_set_cddadr, function, f_sys_pf_set_cddadr),
   DefunResult=sys_pf_set_cddadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:arglist_info(sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:init_args(exact_only, f_sys_pf_set_cddadr))).
*/
/*
(defun %set-caaddr (x v) (set-car (caddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6935 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaddr',[x,v],['set-car',[caddr,x],v]])
wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_caaddr).

/*

### Compiled:  `SYS::%SET-CAADDR` 
*/
f_sys_pf_set_caaddr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caddr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaddr, classof, claz_function),
   set_opv(sys_pf_set_caaddr, compile_as, kw_function),
   set_opv(sys_pf_set_caaddr, function, f_sys_pf_set_caaddr),
   DefunResult=sys_pf_set_caaddr.
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:arglist_info(sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:init_args(exact_only, f_sys_pf_set_caaddr))).
*/
/*
(defun %set-cadddr (x v) (set-car (cdddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6984 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadddr',[x,v],['set-car',[cdddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cadddr).

/*

### Compiled:  `SYS::%SET-CADDDR` 
*/
f_sys_pf_set_cadddr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdddr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadddr, classof, claz_function),
   set_opv(sys_pf_set_cadddr, compile_as, kw_function),
   set_opv(sys_pf_set_cadddr, function, f_sys_pf_set_cadddr),
   DefunResult=sys_pf_set_cadddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:arglist_info(sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:init_args(exact_only, f_sys_pf_set_cadddr))).
*/
/*
(defun %set-cdaddr (x v) (set-cdr (caddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7033 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaddr',[x,v],['set-cdr',[caddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cdaddr).

/*

### Compiled:  `SYS::%SET-CDADDR` 
*/
f_sys_pf_set_cdaddr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_caddr(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaddr, classof, claz_function),
   set_opv(sys_pf_set_cdaddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaddr, function, f_sys_pf_set_cdaddr),
   DefunResult=sys_pf_set_cdaddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:arglist_info(sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:init_args(exact_only, f_sys_pf_set_cdaddr))).
*/
/*
(defun %set-cddddr (x v) (set-cdr (cdddr x) v))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7082 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddddr',[x,v],['set-cdr',[cdddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_cddddr).

/*

### Compiled:  `SYS::%SET-CDDDDR` 
*/
f_sys_pf_set_cddddr(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cdddr(X_Get, Set_cdr_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddddr, classof, claz_function),
   set_opv(sys_pf_set_cddddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddddr, function, f_sys_pf_set_cddddr),
   DefunResult=sys_pf_set_cddddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:arglist_info(sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:init_args(exact_only, f_sys_pf_set_cddddr))).
*/
/*
(defsetf car set-car)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7133 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,car,'set-car'])
:- cl_defsetf(car, sys_set_car, _Ignored).
/*
(defsetf cdr set-cdr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7156 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdr,'set-cdr'])
:- cl_defsetf(cdr, sys_set_cdr, _Ignored).
/*
(defsetf caar %set-caar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7179 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caar,'%set-caar'])
:- cl_defsetf(caar, sys_pf_set_caar, _Ignored).
/*
(defsetf cadr %set-cadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7205 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadr,'%set-cadr'])
:- cl_defsetf(cadr, sys_pf_set_cadr, _Ignored).
/*
(defsetf cdar %set-cdar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7231 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdar,'%set-cdar'])
:- cl_defsetf(cdar, sys_pf_set_cdar, _Ignored).
/*
(defsetf cddr %set-cddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7257 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddr,'%set-cddr'])
:- cl_defsetf(cddr, sys_pf_set_cddr, _Ignored).
/*
(defsetf caaar %set-caaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7283 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaar,'%set-caaar'])
:- cl_defsetf(caaar, sys_pf_set_caaar, _Ignored).
/*
(defsetf cadar %set-cadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7311 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadar,'%set-cadar'])
:- cl_defsetf(cadar, sys_pf_set_cadar, _Ignored).
/*
(defsetf cdaar %set-cdaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7339 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaar,'%set-cdaar'])
:- cl_defsetf(cdaar, sys_pf_set_cdaar, _Ignored).
/*
(defsetf cddar %set-cddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7367 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddar,'%set-cddar'])
:- cl_defsetf(cddar, sys_pf_set_cddar, _Ignored).
/*
(defsetf caadr %set-caadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7395 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caadr,'%set-caadr'])
:- cl_defsetf(caadr, sys_pf_set_caadr, _Ignored).
/*
(defsetf caddr %set-caddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7423 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caddr,'%set-caddr'])
:- cl_defsetf(caddr, sys_pf_set_caddr, _Ignored).
/*
(defsetf cdadr %set-cdadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7451 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdadr,'%set-cdadr'])
:- cl_defsetf(cdadr, sys_pf_set_cdadr, _Ignored).
/*
(defsetf cdddr %set-cdddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7479 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdddr,'%set-cdddr'])
:- cl_defsetf(cdddr, sys_pf_set_cdddr, _Ignored).
/*
(defsetf caaaar %set-caaaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7507 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaaar,'%set-caaaar'])
:- cl_defsetf(caaaar, sys_pf_set_caaaar, _Ignored).
/*
(defsetf cadaar %set-cadaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7537 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadaar,'%set-cadaar'])
:- cl_defsetf(cadaar, sys_pf_set_cadaar, _Ignored).
/*
(defsetf cdaaar %set-cdaaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7567 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaaar,'%set-cdaaar'])
:- cl_defsetf(cdaaar, sys_pf_set_cdaaar, _Ignored).
/*
(defsetf cddaar %set-cddaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7597 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddaar,'%set-cddaar'])
:- cl_defsetf(cddaar, sys_pf_set_cddaar, _Ignored).
/*
(defsetf caadar %set-caadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7627 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caadar,'%set-caadar'])
:- cl_defsetf(caadar, sys_pf_set_caadar, _Ignored).
/*
(defsetf caddar %set-caddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7657 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caddar,'%set-caddar'])
:- cl_defsetf(caddar, sys_pf_set_caddar, _Ignored).
/*
(defsetf cdadar %set-cdadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7687 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdadar,'%set-cdadar'])
:- cl_defsetf(cdadar, sys_pf_set_cdadar, _Ignored).
/*
(defsetf cdddar %set-cdddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7717 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdddar,'%set-cdddar'])
:- cl_defsetf(cdddar, sys_pf_set_cdddar, _Ignored).
/*
(defsetf caaadr %set-caaadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7747 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaadr,'%set-caaadr'])
:- cl_defsetf(caaadr, sys_pf_set_caaadr, _Ignored).
/*
(defsetf cadadr %set-cadadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7777 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadadr,'%set-cadadr'])
:- cl_defsetf(cadadr, sys_pf_set_cadadr, _Ignored).
/*
(defsetf cdaadr %set-cdaadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7807 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaadr,'%set-cdaadr'])
:- cl_defsetf(cdaadr, sys_pf_set_cdaadr, _Ignored).
/*
(defsetf cddadr %set-cddadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7837 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddadr,'%set-cddadr'])
:- cl_defsetf(cddadr, sys_pf_set_cddadr, _Ignored).
/*
(defsetf caaddr %set-caaddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7867 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaddr,'%set-caaddr'])
:- cl_defsetf(caaddr, sys_pf_set_caaddr, _Ignored).
/*
(defsetf cadddr %set-cadddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7897 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadddr,'%set-cadddr'])
:- cl_defsetf(cadddr, sys_pf_set_cadddr, _Ignored).
/*
(defsetf cdaddr %set-cdaddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7927 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaddr,'%set-cdaddr'])
:- cl_defsetf(cdaddr, sys_pf_set_cdaddr, _Ignored).
/*
(defsetf cddddr %set-cddddr)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7957 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddddr,'%set-cddddr'])
:- cl_defsetf(cddddr, sys_pf_set_cddddr, _Ignored).
/*
(defsetf first set-car)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7989 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,first,'set-car'])
:- cl_defsetf(first, sys_set_car, _Ignored).
/*
(defsetf second %set-cadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8014 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,second,'%set-cadr'])
:- cl_defsetf(second, sys_pf_set_cadr, _Ignored).
/*
(defsetf third %set-caddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8042 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,third,'%set-caddr'])
:- cl_defsetf(third, sys_pf_set_caddr, _Ignored).
/*
(defsetf fourth %set-cadddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8070 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,fourth,'%set-cadddr'])
:- cl_defsetf(fourth, sys_pf_set_cadddr, _Ignored).
/*
(defun %set-fifth (x v) (set-car (cddddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8100 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-fifth',[x,v],['set-car',[cddddr,x],v]])
wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_fifth).

/*

### Compiled:  `SYS::%SET-FIFTH` 
*/
f_sys_pf_set_fifth(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddddr(X_Get, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_fifth, classof, claz_function),
   set_opv(sys_pf_set_fifth, compile_as, kw_function),
   set_opv(sys_pf_set_fifth, function, f_sys_pf_set_fifth),
   DefunResult=sys_pf_set_fifth.
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:arglist_info(sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:init_args(exact_only, f_sys_pf_set_fifth))).
*/
/*
(defsetf fifth %set-fifth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8149 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,fifth,'%set-fifth'])
:- cl_defsetf(fifth, sys_pf_set_fifth, _Ignored).
/*
(defun %set-sixth (x v) (set-car (cdr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8177 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-sixth',[x,v],['set-car',[cdr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_sixth).

/*

### Compiled:  `SYS::%SET-SIXTH` 
*/
f_sys_pf_set_sixth(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddddr(X_Get, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_sixth, classof, claz_function),
   set_opv(sys_pf_set_sixth, compile_as, kw_function),
   set_opv(sys_pf_set_sixth, function, f_sys_pf_set_sixth),
   DefunResult=sys_pf_set_sixth.
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:arglist_info(sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:init_args(exact_only, f_sys_pf_set_sixth))).
*/
/*
(defsetf sixth %set-sixth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8232 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,sixth,'%set-sixth'])
:- cl_defsetf(sixth, sys_pf_set_sixth, _Ignored).
/*
(defun %set-seventh (x v) (set-car (cddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8260 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-seventh',[x,v],['set-car',[cddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_seventh).

/*

### Compiled:  `SYS::%SET-SEVENTH` 
*/
f_sys_pf_set_seventh(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddddr(X_Get, Cddr_Param),
	cl_cddr(Cddr_Param, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_seventh, classof, claz_function),
   set_opv(sys_pf_set_seventh, compile_as, kw_function),
   set_opv(sys_pf_set_seventh, function, f_sys_pf_set_seventh),
   DefunResult=sys_pf_set_seventh.
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:arglist_info(sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:init_args(exact_only, f_sys_pf_set_seventh))).
*/
/*
(defsetf seventh %set-seventh)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8318 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,seventh,'%set-seventh'])
:- cl_defsetf(seventh, sys_pf_set_seventh, _Ignored).
/*
(defun %set-eighth (x v) (set-car (cdddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8350 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-eighth',[x,v],['set-car',[cdddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_eighth).

/*

### Compiled:  `SYS::%SET-EIGHTH` 
*/
f_sys_pf_set_eighth(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddddr(X_Get, Cdddr_Param),
	cl_cdddr(Cdddr_Param, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_eighth, classof, claz_function),
   set_opv(sys_pf_set_eighth, compile_as, kw_function),
   set_opv(sys_pf_set_eighth, function, f_sys_pf_set_eighth),
   DefunResult=sys_pf_set_eighth.
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:arglist_info(sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:init_args(exact_only, f_sys_pf_set_eighth))).
*/
/*
(defsetf eighth %set-eighth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8408 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,eighth,'%set-eighth'])
:- cl_defsetf(eighth, sys_pf_set_eighth, _Ignored).
/*
(defun %set-ninth (x v) (set-car (cddddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8438 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-ninth',[x,v],['set-car',[cddddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_ninth).

/*

### Compiled:  `SYS::%SET-NINTH` 
*/
f_sys_pf_set_ninth(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddddr(X_Get, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_ninth, classof, claz_function),
   set_opv(sys_pf_set_ninth, compile_as, kw_function),
   set_opv(sys_pf_set_ninth, function, f_sys_pf_set_ninth),
   DefunResult=sys_pf_set_ninth.
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:arglist_info(sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:init_args(exact_only, f_sys_pf_set_ninth))).
*/
/*
(defsetf ninth %set-ninth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8496 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,ninth,'%set-ninth'])
:- cl_defsetf(ninth, sys_pf_set_ninth, _Ignored).
/*
(defun %set-tenth (x v) (set-car (cdr (cddddr (cddddr x))) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8524 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-tenth',[x,v],['set-car',[cdr,[cddddr,[cddddr,x]]],v]])
wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]).
wl:arglist_info(sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_pf_set_tenth).

/*

### Compiled:  `SYS::%SET-TENTH` 
*/
f_sys_pf_set_tenth(X, V, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(sys_x, X), bv(sys_v, V)|Env],
	get_var(Env10, sys_x, X_Get),
	cl_cddddr(X_Get, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	get_var(Env10, sys_v, V_Get),
	f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_tenth, classof, claz_function),
   set_opv(sys_pf_set_tenth, compile_as, kw_function),
   set_opv(sys_pf_set_tenth, function, f_sys_pf_set_tenth),
   DefunResult=sys_pf_set_tenth.
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:arglist_info(sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:init_args(exact_only, f_sys_pf_set_tenth))).
*/
/*
(defsetf tenth %set-tenth)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8588 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,tenth,'%set-tenth'])
:- cl_defsetf(tenth, sys_pf_set_tenth, _Ignored).
/*
(defsetf rest set-cdr)
;;Redefined in extensible-sequences-base.lisp
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8618 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,rest,'set-cdr'])
:- cl_defsetf(rest, sys_set_cdr, _Ignored).
/*
;Redefined in extensible-sequences-base.lisp
*/
/*
(defsetf elt %set-elt)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8689 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,elt,'%set-elt'])
:- cl_defsetf(elt, sys_pf_set_elt, _Ignored).
/*
(defsetf nth %set-nth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8713 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,nth,'%set-nth'])
:- cl_defsetf(nth, sys_pf_set_nth, _Ignored).
/*
(defsetf svref svset)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8737 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,svref,svset])
:- cl_defsetf(svref, sys_svset, _Ignored).
/*
(defsetf fill-pointer %set-fill-pointer)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8760 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'fill-pointer','%set-fill-pointer'])
:- cl_defsetf(fill_pointer, sys_pf_set_fill_pointer, _Ignored).
/*
(defsetf subseq %set-subseq)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8802 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,subseq,'%set-subseq'])
:- cl_defsetf(subseq, sys_pf_set_subseq, _Ignored).
/*
(defsetf symbol-value set)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8832 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-value',set])
:- cl_defsetf(symbol_value, set, _Ignored).
/*
(defsetf symbol-function %set-symbol-function)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8860 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-function','%set-symbol-function'])
:- cl_defsetf(symbol_function, sys_pf_set_symbol_function, _Ignored).
/*
(defsetf symbol-plist %set-symbol-plist)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8908 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-plist','%set-symbol-plist'])
:- cl_defsetf(symbol_plist, sys_pf_set_symbol_plist, _Ignored).
/*
(defsetf get put)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8950 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,get,put])
:- cl_defsetf(get, sys_put, _Ignored).
/*
(defsetf gethash puthash)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8969 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,gethash,puthash])
:- cl_defsetf(gethash, sys_puthash, _Ignored).
/*
(defsetf char set-char)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8996 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,char,'set-char'])
:- cl_defsetf(char, sys_set_char, _Ignored).
/*
(defsetf schar set-schar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9021 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,schar,'set-schar'])
:- cl_defsetf(schar, sys_set_schar, _Ignored).
/*
(defsetf logical-pathname-translations %set-logical-pathname-translations)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9048 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'logical-pathname-translations','%set-logical-pathname-translations'])
:- cl_defsetf(logical_pathname_translations,
	      sys_pf_set_logical_pathname_translations,
	      _Ignored).
/*
(defsetf readtable-case %set-readtable-case)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9124 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'readtable-case','%set-readtable-case'])
:- cl_defsetf(readtable_case, sys_pf_set_readtable_case, _Ignored).
/*
(defsetf function-info %set-function-info)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9172 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'function-info','%set-function-info'])
:- cl_defsetf(sys_function_info, sys_pf_set_function_info, _Ignored).
/*
(defsetf stream-external-format %set-stream-external-format)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9218 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'stream-external-format','%set-stream-external-format'])
:- cl_defsetf(stream_external_format,
	      sys_pf_set_stream_external_format,
	      _Ignored).
/*
(defsetf structure-ref structure-set)

;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           sequence routines

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9282 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'structure-ref','structure-set'])
:- cl_defsetf(sys_structure_ref, sys_structure_set, _Ignored).
/*
;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
*/
/*
;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:
*/
/*
;;;
*/
/*
;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
*/
/*
;;;  Copyright (c) 1990, Giuseppe Attardi.
*/
/*
;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
*/
/*
;;;
*/
/*
;;;    This program is free software; you can redistribute it and/or
*/
/*
;;;    modify it under the terms of the GNU Library General Public
*/
/*
;;;    License as published by the Free Software Foundation; either
*/
/*
;;;    version 2 of the License, or (at your option) any later version.
*/
/*
;;;
*/
/*
;;;    See file '../Copyright' for full details.
*/
/*
;;;                           sequence routines
*/
/*
(in-package "SYSTEM")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:10038 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','$STRING'("SYSTEM")])
:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored).
/*
#+ecl-min
(eval-when (:execute)
  (load (merge-pathnames "seqmacros.lsp" *load-truename*)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:10063 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':ecl-min'],['eval-when',[':execute'],[load,['merge-pathnames','$STRING'("seqmacros.lsp"),'*load-truename*']]]]))
/*
(defun error-not-a-sequence (value)
  ; (declare (c-local))
  (signal-type-error value 'sequence))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:10160 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'error-not-a-sequence',[value],['signal-type-error',value,[quote,sequence]]])
wl:lambda_def(defun, sys_error_not_a_sequence, f_sys_error_not_a_sequence, [sys_value], [[sys_signal_type_error, sys_value, [quote, sequence]]]).
wl:arglist_info(sys_error_not_a_sequence, f_sys_error_not_a_sequence, [sys_value], arginfo{all:[sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value], opt:0, req:[sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_error_not_a_sequence).

/*

### Compiled:  `SYS::ERROR-NOT-A-SEQUENCE` 
*/
f_sys_error_not_a_sequence(Value, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(sys_value, Value)|Env],
	get_var(Env9, sys_value, Value_Get),
	f_sys_signal_type_error(Value_Get, sequence, Sequence),
	Sequence=FnResult.
:- set_opv(f_sys_error_not_a_sequence, classof, claz_function),
   set_opv(sys_error_not_a_sequence, compile_as, kw_function),
   set_opv(sys_error_not_a_sequence, function, f_sys_error_not_a_sequence),
   DefunResult=sys_error_not_a_sequence.
/*
:- side_effect(assert_lsp(sys_error_not_a_sequence,
			  wl:lambda_def(defun, sys_error_not_a_sequence, f_sys_error_not_a_sequence, [sys_value], [[sys_signal_type_error, sys_value, [quote, sequence]]]))).
*/
/*
:- side_effect(assert_lsp(sys_error_not_a_sequence,
			  wl:arglist_info(sys_error_not_a_sequence, f_sys_error_not_a_sequence, [sys_value], arginfo{all:[sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value], opt:0, req:[sys_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_error_not_a_sequence,
			  wl:init_args(exact_only, f_sys_error_not_a_sequence))).
*/
/*
 (declare (c-local))
*/
/*
(defun error-sequence-index (sequence index)
  ; (declare (c-local))
  (error 'simple-type-error
         :datum index
         :expected-type 'unsigned-byte
         :format-control "Not a valid index "(defun error-sequence-index (sequence index)\r\n  ; (declare (c-local))\r\n  (error 'simple-type-error\r\n         :datum index\r\n         :expected-type 'unsigned-byte\r\n         :format-control \"Not a valid index ~A into sequence ~A\"\r\n         :format-arguments (list index sequence)))\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:10264 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'error-sequence-index',[sequence,index],[error,[quote,'simple-type-error'],':datum',index,':expected-type',[quote,'unsigned-byte'],':format-control','$STRING'("Not a valid index ~A into sequence ~A"),':format-arguments',[list,index,sequence]]])
wl:lambda_def(defun, sys_error_sequence_index, f_sys_error_sequence_index, [sequence, index], [[error, [quote, simple_type_error], kw_datum, index, kw_expected_type, [quote, unsigned_byte], kw_format_control, '$ARRAY'([*], claz_base_character, "Not a valid index ~A into sequence ~A"), kw_format_arguments, [list, index, sequence]]]).
wl:arglist_info(sys_error_sequence_index, f_sys_error_sequence_index, [sequence, index], arginfo{all:[sequence, index], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, index], opt:0, req:[sequence, index], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_error_sequence_index).

/*

### Compiled:  `SYS::ERROR-SEQUENCE-INDEX` 
*/
f_sys_error_sequence_index(Sequence, Index, FnResult) :-
	nop(global_env(Env)),
	Env11=[bv(sequence, Sequence), bv(index, Index)|Env],
	get_var(Env11, index, Index_Get7),
	get_var(Env11, sequence, Sequence_Get),
	CAR=[Index_Get7, Sequence_Get],
	cl_error(
		 [ simple_type_error,
		   kw_datum,
		   Index_Get7,
		   kw_expected_type,
		   unsigned_byte,
		   kw_format_control,
		   '$ARRAY'([*],
			    claz_base_character,
			    "Not a valid index ~A into sequence ~A"),
		   kw_format_arguments,
		   CAR
		 ],
		 Error_Ret),
	Error_Ret=FnResult.
:- set_opv(f_sys_error_sequence_index, classof, claz_function),
   set_opv(sys_error_sequence_index, compile_as, kw_function),
   set_opv(sys_error_sequence_index, function, f_sys_error_sequence_index),
   DefunResult=sys_error_sequence_index.
/*
:- side_effect(assert_lsp(sys_error_sequence_index,
			  wl:lambda_def(defun, sys_error_sequence_index, f_sys_error_sequence_index, [sequence, index], [[error, [quote, simple_type_error], kw_datum, index, kw_expected_type, [quote, unsigned_byte], kw_format_control, '$ARRAY'([*], claz_base_character, "Not a valid index ~A into sequence ~A"), kw_format_arguments, [list, index, sequence]]]))).
*/
/*
:- side_effect(assert_lsp(sys_error_sequence_index,
			  wl:arglist_info(sys_error_sequence_index, f_sys_error_sequence_index, [sequence, index], arginfo{all:[sequence, index], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, index], opt:0, req:[sequence, index], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_error_sequence_index,
			  wl:init_args(exact_only, f_sys_error_sequence_index))).
*/
/*
 (declare (c-local))
*/
/*
(defun error-sequence-type (type)
  ; (declare (c-local))
  (error 'simple-type-error
         :datum (vector) ;; Any sequence object will do, because it does not belong to TYPE
         :expected-type type
         :format-control ""(defun error-sequence-type (type)\r\n  ; (declare (c-local))\r\n  (error 'simple-type-error\r\n         :datum (vector) ;; Any sequence object will do, because it does not belong to TYPE\r\n         :expected-type type\r\n         :format-control \"~S does not specify a sequence type\"\r\n         :format-arguments (list type)))\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:10547 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'error-sequence-type',[type],[error,[quote,'simple-type-error'],':datum',[vector],':expected-type',type,':format-control','$STRING'("~S does not specify a sequence type"),':format-arguments',[list,type]]])
wl:lambda_def(defun, sys_error_sequence_type, f_sys_error_sequence_type, [type], [[error, [quote, simple_type_error], kw_datum, [vector], kw_expected_type, type, kw_format_control, '$ARRAY'([*], claz_base_character, "~S does not specify a sequence type"), kw_format_arguments, [list, type]]]).
wl:arglist_info(sys_error_sequence_type, f_sys_error_sequence_type, [type], arginfo{all:[type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[type], opt:0, req:[type], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_error_sequence_type).

/*

### Compiled:  `SYS::ERROR-SEQUENCE-TYPE` 
*/
f_sys_error_sequence_type(Type, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(type, Type)|Env],
	cl_vector([], Vector_Ret),
	get_var(Env10, type, Type_Get7),
	CAR=[Type_Get7],
	cl_error(
		 [ simple_type_error,
		   kw_datum,
		   Vector_Ret,
		   kw_expected_type,
		   Type_Get7,
		   kw_format_control,
		   '$ARRAY'([*],
			    claz_base_character,
			    "~S does not specify a sequence type"),
		   kw_format_arguments,
		   CAR
		 ],
		 Error_Ret),
	Error_Ret=FnResult.
:- set_opv(f_sys_error_sequence_type, classof, claz_function),
   set_opv(sys_error_sequence_type, compile_as, kw_function),
   set_opv(sys_error_sequence_type, function, f_sys_error_sequence_type),
   DefunResult=sys_error_sequence_type.
/*
:- side_effect(assert_lsp(sys_error_sequence_type,
			  wl:lambda_def(defun, sys_error_sequence_type, f_sys_error_sequence_type, [type], [[error, [quote, simple_type_error], kw_datum, [vector], kw_expected_type, type, kw_format_control, '$ARRAY'([*], claz_base_character, "~S does not specify a sequence type"), kw_format_arguments, [list, type]]]))).
*/
/*
:- side_effect(assert_lsp(sys_error_sequence_type,
			  wl:arglist_info(sys_error_sequence_type, f_sys_error_sequence_type, [type], arginfo{all:[type], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[type], opt:0, req:[type], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_error_sequence_type,
			  wl:init_args(exact_only, f_sys_error_sequence_type))).
*/
/*
 (declare (c-local))
*/
/*
; Any sequence object will do, because it does not belong to TYPE
*/
/*
(defun error-sequence-length (object type size)
  ; (declare (c-local))
  (error 'simple-type-error
         :format-control
         "Cannot create a sequence of size "(defun error-sequence-length (object type size)\r\n  ; (declare (c-local))\r\n  (error 'simple-type-error\r\n         :format-control\r\n         \"Cannot create a sequence of size ~S which matches type ~S.\"\r\n         :format-arguments (list size type)\r\n         :expected-type type\r\n         :datum object))\r\n\r\n\t\t\t\t \r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:10867 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'error-sequence-length',[object,type,size],[error,[quote,'simple-type-error'],':format-control','$STRING'("Cannot create a sequence of size ~S which matches type ~S."),':format-arguments',[list,size,type],':expected-type',type,':datum',object]])
wl:lambda_def(defun, sys_error_sequence_length, f_sys_error_sequence_length, [sys_object, type, sys_size], [[error, [quote, simple_type_error], kw_format_control, '$ARRAY'([*], claz_base_character, "Cannot create a sequence of size ~S which matches type ~S."), kw_format_arguments, [list, sys_size, type], kw_expected_type, type, kw_datum, sys_object]]).
wl:arglist_info(sys_error_sequence_length, f_sys_error_sequence_length, [sys_object, type, sys_size], arginfo{all:[sys_object, type, sys_size], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object, type, sys_size], opt:0, req:[sys_object, type, sys_size], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_error_sequence_length).

/*

### Compiled:  `SYS::ERROR-SEQUENCE-LENGTH` 
*/
f_sys_error_sequence_length(Object, Type, Size, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(sys_object, Object), bv(type, Type), bv(sys_size, Size)|Env],
	get_var(Env12, sys_size, Size_Get),
	get_var(Env12, type, Type_Get),
	CAR=[Size_Get, Type_Get],
	get_var(Env12, sys_object, Object_Get),
	get_var(Env12, type, Type_Get8),
	cl_error(
		 [ simple_type_error,
		   kw_format_control,
		   '$ARRAY'([*],
			    claz_base_character,
			    "Cannot create a sequence of size ~S which matches type ~S."),
		   kw_format_arguments,
		   CAR,
		   kw_expected_type,
		   Type_Get8,
		   kw_datum,
		   Object_Get
		 ],
		 Error_Ret),
	Error_Ret=FnResult.
:- set_opv(f_sys_error_sequence_length, classof, claz_function),
   set_opv(sys_error_sequence_length, compile_as, kw_function),
   set_opv(sys_error_sequence_length, function, f_sys_error_sequence_length),
   DefunResult=sys_error_sequence_length.
/*
:- side_effect(assert_lsp(sys_error_sequence_length,
			  wl:lambda_def(defun, sys_error_sequence_length, f_sys_error_sequence_length, [sys_object, type, sys_size], [[error, [quote, simple_type_error], kw_format_control, '$ARRAY'([*], claz_base_character, "Cannot create a sequence of size ~S which matches type ~S."), kw_format_arguments, [list, sys_size, type], kw_expected_type, type, kw_datum, sys_object]]))).
*/
/*
:- side_effect(assert_lsp(sys_error_sequence_length,
			  wl:arglist_info(sys_error_sequence_length, f_sys_error_sequence_length, [sys_object, type, sys_size], arginfo{all:[sys_object, type, sys_size], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object, type, sys_size], opt:0, req:[sys_object, type, sys_size], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_error_sequence_length,
			  wl:init_args(exact_only, f_sys_error_sequence_length))).
*/
/*
 (declare (c-local))
*/
/*
(defun make-sequence (type size &key (initial-element nil iesp) &aux sequence)
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (multiple-value-bind (element-type length)
      (closest-sequence-type type)
    (cond ((eq element-type 'LIST)
           (setq sequence (make-list size :initial-element initial-element))
           (unless (subtypep 'LIST type)
             (when (or (and (subtypep type 'NULL) (plusp size))
                       (and (subtypep type 'CONS) (zerop size)))
               (error-sequence-length (make-list size :initial-element initial-element) type 0))))
          (t
           (setq sequence (sys:make-vector (if (eq element-type '*) T element-type)
                                           size nil nil nil nil))
           (when iesp
             (fill-array-with-elt sequence initial-element 0 nil))
           (unless (or (eql length '*) (eql length size))
             (error-sequence-length sequence type size))))
    sequence))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:11177 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'make-sequence',[type,size,'&key',['initial-element',[],iesp],'&aux',sequence],'$STRING'("Args: (type length &key initial-element)\r\nCreates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-\r\nELEMENT is given, then it becomes the elements of the created sequence.  The\r\ndefault value of INITIAL-ELEMENT depends on TYPE."),['multiple-value-bind',['element-type',length],['closest-sequence-type',type],[cond,[[eq,'element-type',[quote,'LIST']],[setq,sequence,['make-list',size,':initial-element','initial-element']],[unless,[subtypep,[quote,'LIST'],type],[when,[or,[and,[subtypep,type,[quote,'NULL']],[plusp,size]],[and,[subtypep,type,[quote,'CONS']],[zerop,size]]],['error-sequence-length',['make-list',size,':initial-element','initial-element'],type,0]]]],[t,[setq,sequence,['sys:make-vector',[if,[eq,'element-type',[quote,*]],'T','element-type'],size,[],[],[],[]]],[when,iesp,['fill-array-with-elt',sequence,'initial-element',0,[]]],[unless,[or,[eql,length,[quote,*]],[eql,length,size]],['error-sequence-length',sequence,type,size]]]],sequence]])
doc: doc_string(make_sequence,
	      _125200828,
	      function,
	      "Args: (type length &key initial-element)\r\nCreates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-\r\nELEMENT is given, then it becomes the elements of the created sequence.  The\r\ndefault value of INITIAL-ELEMENT depends on TYPE.").

wl:lambda_def(defun, make_sequence, cl_make_sequence, [type, sys_size, c38_key, [sys_initial_element, [], sys_iesp], c38_aux, sequence], [[multiple_value_bind, [sys_element_type, length], [sys_closest_sequence_type, type], [cond, [[eq, sys_element_type, [quote, list]], [setq, sequence, [make_list, sys_size, kw_initial_element, sys_initial_element]], [unless, [subtypep, [quote, list], type], [when, [or, [and, [subtypep, type, [quote, null]], [plusp, sys_size]], [and, [subtypep, type, [quote, cons]], [zerop, sys_size]]], [sys_error_sequence_length, [make_list, sys_size, kw_initial_element, sys_initial_element], type, 0]]]], [t, [setq, sequence, [sys_make_vector, [if, [eq, sys_element_type, [quote, *]], t, sys_element_type], sys_size, [], [], [], []]], [when, sys_iesp, [sys_fill_array_with_elt, sequence, sys_initial_element, 0, []]], [unless, [or, [eql, length, [quote, *]], [eql, length, sys_size]], [sys_error_sequence_length, sequence, type, sys_size]]]], sequence]]).
wl:arglist_info(make_sequence, cl_make_sequence, [type, sys_size, c38_key, [sys_initial_element, [], sys_iesp], c38_aux, sequence], arginfo{all:[type, sys_size], allow_other_keys:0, aux:[sequence], body:0, complex:0, env:0, key:[sys_initial_element], names:[type, sys_size, sys_initial_element, sys_iesp, sequence], opt:0, req:[type, sys_size], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_make_sequence).

/*

### Compiled:  `CL:MAKE-SEQUENCE` 
*/
cl_make_sequence(Type, Size, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	CDR=[[[bv(type, Type), bv(sys_size, Size), bv(sys_initial_element, Initial_element), bv(sys_iesp, Initial_element_Present), bv(sequence, Sequence)]|Env]|Env],
	get_kw(Env,
	       RestNKeys,
	       sys_initial_element,
	       sys_initial_element,
	       Initial_element,
	       []=Initial_element,
	       Initial_element_Present),
	aux_var(Env, sequence, Sequence, true, []),
	LEnv=[bv(sys_element_type, []), bv(length, [])|CDR],
	get_var(LEnv, type, Type_Get),
	f_sys_closest_sequence_type(Type_Get, Sequence_type_Ret),
	setq_from_values(LEnv, [sys_element_type, length]),
	get_var(LEnv, sys_element_type, Element_type_Get),
	(   is_eq(Element_type_Get, list)
	->  get_var(LEnv, sys_initial_element, Initial_element_Get),
	    get_var(LEnv, sys_size, Size_Get),
	    cl_make_list(Size_Get,
			 kw_initial_element,
			 Initial_element_Get,
			 Sequence70),
	    set_var(LEnv, sequence, Sequence70),
	    get_var(LEnv, type, Type_Get21),
	    cl_subtypep(list, Type_Get21, IFTEST19),
	    (   IFTEST19\==[]
	    ->  TrueResult61=[]
	    ;   (   get_var(LEnv, type, Type_Get26),
		    cl_subtypep(Type_Get26, null, IFTEST24),
		    (   IFTEST24\==[]
		    ->  get_var(LEnv, sys_size, Size_Get27),
			cl_plusp(Size_Get27, TrueResult),
			FORM1_Res=TrueResult
		    ;   FORM1_Res=[]
		    ),
		    FORM1_Res\==[],
		    IFTEST22=FORM1_Res
		->  true
		;   get_var(LEnv, type, Type_Get31),
		    cl_subtypep(Type_Get31, cons, IFTEST29),
		    (   IFTEST29\==[]
		    ->  get_var(LEnv, sys_size, Size_Get32),
			cl_zerop(Size_Get32, TrueResult33),
			IFTEST22=TrueResult33
		    ;   IFTEST22=[]
		    )
		),
		(   IFTEST22\==[]
		->  get_var(LEnv, sys_initial_element, Initial_element_Get36),
		    get_var(LEnv, sys_size, Size_Get35),
		    cl_make_list(Size_Get35,
				 kw_initial_element,
				 Initial_element_Get36,
				 Sequence_length_Param),
		    get_var(LEnv, type, Type_Get37),
		    f_sys_error_sequence_length(Sequence_length_Param,
						Type_Get37,
						0,
						TrueResult38),
		    TrueResult61=TrueResult38
		;   TrueResult61=[]
		)
	    )
	;   get_var(LEnv, sys_element_type, Element_type_Get41),
	    (   is_eq(Element_type_Get41, *)
	    ->  Make_vector_Param=t
	    ;   get_var(LEnv, sys_element_type, Element_type_Get44),
		Make_vector_Param=Element_type_Get44
	    ),
	    get_var(LEnv, sys_size, Size_Get46),
	    f_sys_make_vector(Make_vector_Param,
			      Size_Get46,
			      [],
			      [],
			      [],
			      [],
			      Sequence71),
	    set_var(LEnv, sequence, Sequence71),
	    get_var(LEnv, sys_iesp, IFTEST47),
	    (   IFTEST47\==[]
	    ->  get_var(LEnv, sequence, Sequence_Get),
		get_var(LEnv, sys_initial_element, Initial_element_Get51),
		f_sys_fill_array_with_elt(Sequence_Get,
					  Initial_element_Get51,
					  0,
					  [],
					  TrueResult52),
		_125490742=TrueResult52
	    ;   _125490742=[]
	    ),
	    (   cl_eql(length, *, FORM1_Res56),
		FORM1_Res56\==[],
		IFTEST53=FORM1_Res56
	    ->  true
	    ;   get_var(LEnv, sys_size, Size_Get55),
		cl_eql(length, Size_Get55, Eql_Ret),
		IFTEST53=Eql_Ret
	    ),
	    (   IFTEST53\==[]
	    ->  TrueResult61=[]
	    ;   get_var(LEnv, sequence, Sequence_Get57),
		get_var(LEnv, sys_size, Size_Get59),
		get_var(LEnv, type, Type_Get58),
		f_sys_error_sequence_length(Sequence_Get57,
					    Type_Get58,
					    Size_Get59,
					    ElseResult60),
		TrueResult61=ElseResult60
	    )
	),
	get_var(LEnv, sequence, Sequence_Get63),
	Sequence_Get63=FnResult.
:- set_opv(cl_make_sequence, classof, claz_function),
   set_opv(make_sequence, compile_as, kw_function),
   set_opv(make_sequence, function, cl_make_sequence),
   DefunResult=make_sequence.
/*
:- side_effect(assert_lsp(make_sequence,
			  doc:doc_string(make_sequence, _125200828, function, "Args: (type length &key initial-element)\r\nCreates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-\r\nELEMENT is given, then it becomes the elements of the created sequence.  The\r\ndefault value of INITIAL-ELEMENT depends on TYPE."))).
*/
/*
:- side_effect(assert_lsp(make_sequence,
			  wl:lambda_def(defun, make_sequence, cl_make_sequence, [type, sys_size, c38_key, [sys_initial_element, [], sys_iesp], c38_aux, sequence], [[multiple_value_bind, [sys_element_type, length], [sys_closest_sequence_type, type], [cond, [[eq, sys_element_type, [quote, list]], [setq, sequence, [make_list, sys_size, kw_initial_element, sys_initial_element]], [unless, [subtypep, [quote, list], type], [when, [or, [and, [subtypep, type, [quote, null]], [plusp, sys_size]], [and, [subtypep, type, [quote, cons]], [zerop, sys_size]]], [sys_error_sequence_length, [make_list, sys_size, kw_initial_element, sys_initial_element], type, 0]]]], [t, [setq, sequence, [sys_make_vector, [if, [eq, sys_element_type, [quote, *]], t, sys_element_type], sys_size, [], [], [], []]], [when, sys_iesp, [sys_fill_array_with_elt, sequence, sys_initial_element, 0, []]], [unless, [or, [eql, length, [quote, *]], [eql, length, sys_size]], [sys_error_sequence_length, sequence, type, sys_size]]]], sequence]]))).
*/
/*
:- side_effect(assert_lsp(make_sequence,
			  wl:arglist_info(make_sequence, cl_make_sequence, [type, sys_size, c38_key, [sys_initial_element, [], sys_iesp], c38_aux, sequence], arginfo{all:[type, sys_size], allow_other_keys:0, aux:[sequence], body:0, complex:0, env:0, key:[sys_initial_element], names:[type, sys_size, sys_initial_element, sys_iesp, sequence], opt:0, req:[type, sys_size], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(make_sequence, wl:init_args(2, cl_make_sequence))).
*/
/*
(defun make-seq-iterator (sequence &optional (start 0))
  (declare (optimize (safety 0)))
  (cond ((fixnump start)
         (let ((aux start))
           (declare (fixnum aux))
           (cond ((minusp aux)
                  (error-sequence-index sequence start))
                 ((listp sequence)
                  (nthcdr aux sequence))
                 ((vectorp sequence)
                  (and (< start (length (truly-the vector sequence)))
                       start))
                 (t
                  (error-not-a-sequence sequence)))))
        ((not (or (listp sequence) (vectorp sequence)))
         (error-not-a-sequence sequence))
        ((integerp start)
         nil)
        (t
         (error-sequence-index sequence start))))



#|
(defun closest-sequence-type (type)
  (let (elt-type length name args)
    (cond ((consp type)
           (setq name (first type) args (cdr type)))
          ((si::instancep type)
           (setf name (class-name (truly-the class type)) args nil))
          (t
           (setq name type args nil)))
    (case name
      ((LIST)
       ;; This is the only descriptor that does not match a real
       ;; array type.
       (setq elt-type 'LIST length '*))
      ((VECTOR)
       (setq elt-type (if (endp args) 'T (first args))
             length (if (endp (rest args)) '* (second args))))
      ((SIMPLE-VECTOR)
       (setq elt-type 'T
             length (if (endp args) '* (first args))))
      #-unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
             length (if (endp args) '* (first args))))
      #+unicode
      ((BASE-STRING SIMPLE-BASE-STRING)
       (setq elt-type 'BASE-CHAR
             length (if (endp args) '* (first args))))
      #+unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'CHARACTER
             length (if (endp args) '* (first args))))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (setq elt-type 'BIT
             length (if (endp args) '* (first args))))
      ((ARRAY SIMPLE-ARRAY)
       (let ((dimension-spec (second args)))
         (cond
           ((eql dimension-spec 1)
            (setf length '*))
           ((and (consp dimension-spec)
                 (null (cdr dimension-spec)))
            (setf length (car dimension-spec)))
           (T (error-sequence-type type))))
       (setq elt-type (upgraded-array-element-type (first args))))
      (t
       ;; We arrive here when the sequence type is not easy to parse.
       ;; We give up trying to guess the length of the sequence.
       ;; Furthermore, we also give up trying to find if the element
       ;; type is *. Instead we just compare with some specialized
       ;; types and otherwise fail.
       (dolist (i '((NIL . NIL)
                    (LIST . LIST)
                    (STRING . CHARACTER)
                    . #.(mapcar #'(lambda (i) `((VECTOR ,i) . ,i)) +upgraded-array-element-types+)
		  ))
                (if (subtypep type 'vector)
                    ;; Does this have to be a type-error?
                    ;; 17.3 for MAKE-SEQUENCE says it should be an error,
                    ;; but does not specialize what kind.
                    (error "Cannot find the element type in vector type "(defun make-seq-iterator (sequence &optional (start 0))\r\n  (declare (optimize (safety 0)))\r\n  (cond ((fixnump start)\r\n         (let ((aux start))\r\n           (declare (fixnum aux))\r\n           (cond ((minusp aux)\r\n                  (error-sequence-index sequence start))\r\n                 ((listp sequence)\r\n                  (nthcdr aux sequence))\r\n                 ((vectorp sequence)\r\n                  (and (< start (length (truly-the vector sequence)))\r\n                       start))\r\n                 (t\r\n                  (error-not-a-sequence sequence)))))\r\n        ((not (or (listp sequence) (vectorp sequence)))\r\n         (error-not-a-sequence sequence))\r\n        ((integerp start)\r\n         nil)\r\n        (t\r\n         (error-sequence-index sequence start))))\r\n\r\n\r\n\r\n#|\r\n(defun closest-sequence-type (type)\r\n  (let (elt-type length name args)\r\n    (cond ((consp type)\r\n           (setq name (first type) args (cdr type)))\r\n          ((si::instancep type)\r\n           (setf name (class-name (truly-the class type)) args nil))\r\n          (t\r\n           (setq name type args nil)))\r\n    (case name\r\n      ((LIST)\r\n       ;; This is the only descriptor that does not match a real\r\n       ;; array type.\r\n       (setq elt-type 'LIST length '*))\r\n      ((VECTOR)\r\n       (setq elt-type (if (endp args) 'T (first args))\r\n             length (if (endp (rest args)) '* (second args))))\r\n      ((SIMPLE-VECTOR)\r\n       (setq elt-type 'T\r\n             length (if (endp args) '* (first args))))\r\n      #-unicode\r\n      ((STRING SIMPLE-STRING)\r\n       (setq elt-type 'BASE-CHAR\r\n             length (if (endp args) '* (first args))))\r\n      #+unicode\r\n      ((BASE-STRING SIMPLE-BASE-STRING)\r\n       (setq elt-type 'BASE-CHAR\r\n             length (if (endp args) '* (first args))))\r\n      #+unicode\r\n      ((STRING SIMPLE-STRING)\r\n       (setq elt-type 'CHARACTER\r\n             length (if (endp args) '* (first args))))\r\n      ((BIT-VECTOR SIMPLE-BIT-VECTOR)\r\n       (setq elt-type 'BIT\r\n             length (if (endp args) '* (first args))))\r\n      ((ARRAY SIMPLE-ARRAY)\r\n       (let ((dimension-spec (second args)))\r\n         (cond\r\n           ((eql dimension-spec 1)\r\n            (setf length '*))\r\n           ((and (consp dimension-spec)\r\n                 (null (cdr dimension-spec)))\r\n            (setf length (car dimension-spec)))\r\n           (T (error-sequence-type type))))\r\n       (setq elt-type (upgraded-array-element-type (first args))))\r\n      (t\r\n       ;; We arrive here when the sequence type is not easy to parse.\r\n       ;; We give up trying to guess the length of the sequence.\r\n       ;; Furthermore, we also give up trying to find if the element\r\n       ;; type is *. Instead we just compare with some specialized\r\n       ;; types and otherwise fail.\r\n       (dolist (i '((NIL . NIL)\r\n                    (LIST . LIST)\r\n                    (STRING . CHARACTER)\r\n                    . #.(mapcar #'(lambda (i) `((VECTOR ,i) . ,i)) +upgraded-array-element-types+)\r\n\t\t  ))\r\n                (if (subtypep type 'vector)\r\n                    ;; Does this have to be a type-error?\r\n                    ;; 17.3 for MAKE-SEQUENCE says it should be an error,\r\n                    ;; but does not specialize what kind.\r\n                    (error \"Cannot find the element type in vector type ~S\" type)\r\n                    (error-sequence-type type))\r\n          (when (subtypep type (car i))\r\n            (setq elt-type (cdr i) length '*)\r\n            ;; The (NIL . NIL) case above\r\n            (unless elt-type\r\n              (error-sequence-type type))\r\n            (return)))))\r\n    (values elt-type length)))\r\n|#\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:12370 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'make-seq-iterator',[sequence,'&optional',[start,0]],[declare,[optimize,[safety,0]]],[cond,[[fixnump,start],[let,[[aux,start]],[declare,[fixnum,aux]],[cond,[[minusp,aux],['error-sequence-index',sequence,start]],[[listp,sequence],[nthcdr,aux,sequence]],[[vectorp,sequence],[and,[<,start,[length,['truly-the',vector,sequence]]],start]],[t,['error-not-a-sequence',sequence]]]]],[[not,[or,[listp,sequence],[vectorp,sequence]]],['error-not-a-sequence',sequence]],[[integerp,start],[]],[t,['error-sequence-index',sequence,start]]]])
wl:lambda_def(defun, sys_make_seq_iterator, f_sys_make_seq_iterator, [sequence, c38_optional, [start, 0]], [[declare, [optimize, [safety, 0]]], [cond, [[sys_fixnump, start], [let, [[sys_aux, start]], [declare, [fixnum, sys_aux]], [cond, [[minusp, sys_aux], [sys_error_sequence_index, sequence, start]], [[listp, sequence], [nthcdr, sys_aux, sequence]], [[vectorp, sequence], [and, [<, start, [length, [ext_truly_the, vector, sequence]]], start]], [t, [sys_error_not_a_sequence, sequence]]]]], [[not, [or, [listp, sequence], [vectorp, sequence]]], [sys_error_not_a_sequence, sequence]], [[integerp, start], []], [t, [sys_error_sequence_index, sequence, start]]]]).
wl:arglist_info(sys_make_seq_iterator, f_sys_make_seq_iterator, [sequence, c38_optional, [start, 0]], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, start], opt:[start], req:[sequence], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_make_seq_iterator).

/*

### Compiled:  `SYS::MAKE-SEQ-ITERATOR` 
*/
f_sys_make_seq_iterator(Sequence, RestNKeys, TrueResult40) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sequence, Sequence), bv(start, Start)]|Env]|Env],
	opt_var(Env, start, Start, true, 0, 1, RestNKeys),
	cl_declare([optimize, [safety, 0]], Declare_Ret),
	get_var(GEnv, start, Start_Get),
	f_sys_fixnump(Start_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, start, Start_Get14),
	    LEnv=[bv(sys_aux, Start_Get14)|GEnv],
	    cl_declare([fixnum, sys_aux], Declare_Ret71),
	    get_var(LEnv, sys_aux, Aux_Get),
	    (   mth:is_minusp(Aux_Get)
	    ->  get_var(LEnv, sequence, Sequence_Get),
		get_var(LEnv, start, Start_Get21),
		f_sys_error_sequence_index(Sequence_Get,
					   Start_Get21,
					   TrueResult44),
		TrueResult40=TrueResult44
	    ;   get_var(LEnv, sequence, Sequence_Get23),
		(   is_listp(Sequence_Get23)
		->  get_var(LEnv, sequence, Sequence_Get27),
		    get_var(LEnv, sys_aux, Aux_Get26),
		    cl_nthcdr(Aux_Get26, Sequence_Get27, TrueResult42),
		    TrueResult40=TrueResult42
		;   get_var(LEnv, sequence, Sequence_Get29),
		    (   is_vectorp(Sequence_Get29)
		    ->  get_var(LEnv, start, Start_Get33),
			f_ext_truly_the(vector, sequence, Sequence69),
			cl_length(Sequence69, PredArg2Result),
			(   Start_Get33<PredArg2Result
			->  get_var(LEnv, start, Start_Get37),
			    TrueResult40=Start_Get37
			;   TrueResult40=[]
			)
		    ;   get_var(LEnv, sequence, Sequence_Get39),
			f_sys_error_not_a_sequence(Sequence_Get39, ElseResult),
			TrueResult40=ElseResult
		    )
		)
	    )
	;   (   get_var(GEnv, sequence, Sequence_Get47),
		cl_listp(Sequence_Get47, FORM1_Res),
		FORM1_Res\==[],
		PredArgResult51=FORM1_Res
	    ->  true
	    ;   get_var(GEnv, sequence, Sequence_Get48),
		cl_vectorp(Sequence_Get48, Vectorp_Ret),
		PredArgResult51=Vectorp_Ret
	    ),
	    (   PredArgResult51==[]
	    ->  get_var(GEnv, sequence, Sequence_Get52),
		f_sys_error_not_a_sequence(Sequence_Get52, TrueResult60),
		TrueResult40=TrueResult60
	    ;   get_var(GEnv, start, Start_Get54),
		(   mth:is_integerp(Start_Get54)
		->  TrueResult40=[]
		;   get_var(GEnv, sequence, Sequence_Get57),
		    get_var(GEnv, start, Start_Get58),
		    f_sys_error_sequence_index(Sequence_Get57,
					       Start_Get58,
					       ElseResult59),
		    TrueResult40=ElseResult59
		)
	    )
	).
:- set_opv(f_sys_make_seq_iterator, classof, claz_function),
   set_opv(sys_make_seq_iterator, compile_as, kw_function),
   set_opv(sys_make_seq_iterator, function, f_sys_make_seq_iterator),
   DefunResult=sys_make_seq_iterator.
/*
:- side_effect(assert_lsp(sys_make_seq_iterator,
			  wl:lambda_def(defun, sys_make_seq_iterator, f_sys_make_seq_iterator, [sequence, c38_optional, [start, 0]], [[declare, [optimize, [safety, 0]]], [cond, [[sys_fixnump, start], [let, [[sys_aux, start]], [declare, [fixnum, sys_aux]], [cond, [[minusp, sys_aux], [sys_error_sequence_index, sequence, start]], [[listp, sequence], [nthcdr, sys_aux, sequence]], [[vectorp, sequence], [and, [<, start, [length, [ext_truly_the, vector, sequence]]], start]], [t, [sys_error_not_a_sequence, sequence]]]]], [[not, [or, [listp, sequence], [vectorp, sequence]]], [sys_error_not_a_sequence, sequence]], [[integerp, start], []], [t, [sys_error_sequence_index, sequence, start]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_make_seq_iterator,
			  wl:arglist_info(sys_make_seq_iterator, f_sys_make_seq_iterator, [sequence, c38_optional, [start, 0]], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, start], opt:[start], req:[sequence], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_make_seq_iterator,
			  wl:init_args(1, f_sys_make_seq_iterator))).
*/
/*

(defun closest-sequence-type (type)
  (let (elt-type length name args)
    (cond ((consp type)
           (setq name (first type) args (cdr type)))
          ((si::instancep type)
           (setf name (class-name (truly-the class type)) args nil))
          (t
           (setq name type args nil)))
    (case name
      ((LIST)
       ;; This is the only descriptor that does not match a real
       ;; array type.
       (setq elt-type 'LIST length '*))
      ((VECTOR)
       (setq elt-type (if (endp args) 'T (first args))
             length (if (endp (rest args)) '* (second args))))
      ((SIMPLE-VECTOR)
       (setq elt-type 'T
             length (if (endp args) '* (first args))))
      #-unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
             length (if (endp args) '* (first args))))
      #+unicode
      ((BASE-STRING SIMPLE-BASE-STRING)
       (setq elt-type 'BASE-CHAR
             length (if (endp args) '* (first args))))
      #+unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'CHARACTER
             length (if (endp args) '* (first args))))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (setq elt-type 'BIT
             length (if (endp args) '* (first args))))
      ((ARRAY SIMPLE-ARRAY)
       (let ((dimension-spec (second args)))
         (cond
           ((eql dimension-spec 1)
            (setf length '*))
           ((and (consp dimension-spec)
                 (null (cdr dimension-spec)))
            (setf length (car dimension-spec)))
           (T (error-sequence-type type))))
       (setq elt-type (upgraded-array-element-type (first args))))
      (t
       ;; We arrive here when the sequence type is not easy to parse.
       ;; We give up trying to guess the length of the sequence.
       ;; Furthermore, we also give up trying to find if the element
       ;; type is *. Instead we just compare with some specialized
       ;; types and otherwise fail.
       (dolist (i '((NIL . NIL)
                    (LIST . LIST)
                    (STRING . CHARACTER)
                    . #.(mapcar #'(lambda (i) `((VECTOR ,i) . ,i)) +upgraded-array-element-types+)
		  ))
                (if (subtypep type 'vector)
                    ;; Does this have to be a type-error?
                    ;; 17.3 for MAKE-SEQUENCE says it should be an error,
                    ;; but does not specialize what kind.
                    (error "Cannot find the element type in vector type "\r\n(defun closest-sequence-type (type)\r\n  (let (elt-type length name args)\r\n    (cond ((consp type)\r\n           (setq name (first type) args (cdr type)))\r\n          ((si::instancep type)\r\n           (setf name (class-name (truly-the class type)) args nil))\r\n          (t\r\n           (setq name type args nil)))\r\n    (case name\r\n      ((LIST)\r\n       ;; This is the only descriptor that does not match a real\r\n       ;; array type.\r\n       (setq elt-type 'LIST length '*))\r\n      ((VECTOR)\r\n       (setq elt-type (if (endp args) 'T (first args))\r\n             length (if (endp (rest args)) '* (second args))))\r\n      ((SIMPLE-VECTOR)\r\n       (setq elt-type 'T\r\n             length (if (endp args) '* (first args))))\r\n      #-unicode\r\n      ((STRING SIMPLE-STRING)\r\n       (setq elt-type 'BASE-CHAR\r\n             length (if (endp args) '* (first args))))\r\n      #+unicode\r\n      ((BASE-STRING SIMPLE-BASE-STRING)\r\n       (setq elt-type 'BASE-CHAR\r\n             length (if (endp args) '* (first args))))\r\n      #+unicode\r\n      ((STRING SIMPLE-STRING)\r\n       (setq elt-type 'CHARACTER\r\n             length (if (endp args) '* (first args))))\r\n      ((BIT-VECTOR SIMPLE-BIT-VECTOR)\r\n       (setq elt-type 'BIT\r\n             length (if (endp args) '* (first args))))\r\n      ((ARRAY SIMPLE-ARRAY)\r\n       (let ((dimension-spec (second args)))\r\n         (cond\r\n           ((eql dimension-spec 1)\r\n            (setf length '*))\r\n           ((and (consp dimension-spec)\r\n                 (null (cdr dimension-spec)))\r\n            (setf length (car dimension-spec)))\r\n           (T (error-sequence-type type))))\r\n       (setq elt-type (upgraded-array-element-type (first args))))\r\n      (t\r\n       ;; We arrive here when the sequence type is not easy to parse.\r\n       ;; We give up trying to guess the length of the sequence.\r\n       ;; Furthermore, we also give up trying to find if the element\r\n       ;; type is *. Instead we just compare with some specialized\r\n       ;; types and otherwise fail.\r\n       (dolist (i '((NIL . NIL)\r\n                    (LIST . LIST)\r\n                    (STRING . CHARACTER)\r\n                    . #.(mapcar #'(lambda (i) `((VECTOR ,i) . ,i)) +upgraded-array-element-types+)\r\n\t\t  ))\r\n                (if (subtypep type 'vector)\r\n                    ;; Does this have to be a type-error?\r\n                    ;; 17.3 for MAKE-SEQUENCE says it should be an error,\r\n                    ;; but does not specialize what kind.\r\n                    (error \"Cannot find the element type in vector type ~S\" type)\r\n                    (error-sequence-type type))\r\n          (when (subtypep type (car i))\r\n            (setq elt-type (cdr i) length '*)\r\n            ;; The (NIL . NIL) case above\r\n            (unless elt-type\r\n              (error-sequence-type type))\r\n            (return)))))\r\n    (values elt-type length)))\r\n".
*/
/*
(defun seq-iterator-ref (sequence iterator)
  (declare (optimize (safety 0)))
  (if (fixnump iterator)
      (aref (truly-the vector sequence) iterator)
      (car (truly-the cons iterator))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:15998 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'seq-iterator-ref',[sequence,iterator],[declare,[optimize,[safety,0]]],[if,[fixnump,iterator],[aref,['truly-the',vector,sequence],iterator],[car,['truly-the',cons,iterator]]]])
wl:lambda_def(defun, sys_seq_iterator_ref, f_sys_seq_iterator_ref, [sequence, sys_iterator], [[declare, [optimize, [safety, 0]]], [if, [sys_fixnump, sys_iterator], [aref, [ext_truly_the, vector, sequence], sys_iterator], [car, [ext_truly_the, cons, sys_iterator]]]]).
wl:arglist_info(sys_seq_iterator_ref, f_sys_seq_iterator_ref, [sequence, sys_iterator], arginfo{all:[sequence, sys_iterator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iterator], opt:0, req:[sequence, sys_iterator], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_seq_iterator_ref).

/*

### Compiled:  `SYS::SEQ-ITERATOR-REF` 
*/
f_sys_seq_iterator_ref(Sequence, Iterator, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(sequence, Sequence), bv(sys_iterator, Iterator)|Env],
	cl_declare([optimize, [safety, 0]], Declare_Ret),
	get_var(Env14, sys_iterator, Iterator_Get),
	f_sys_fixnump(Iterator_Get, IFTEST),
	(   IFTEST\==[]
	->  f_ext_truly_the(vector, sequence, Sequence17),
	    get_var(Env14, sys_iterator, Iterator_Get9),
	    cl_aref(Sequence17, [Iterator_Get9], TrueResult),
	    FnResult=TrueResult
	;   f_ext_truly_the(cons, sys_iterator, Iterator18),
	    cl_car(Iterator18, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_sys_seq_iterator_ref, classof, claz_function),
   set_opv(sys_seq_iterator_ref, compile_as, kw_function),
   set_opv(sys_seq_iterator_ref, function, f_sys_seq_iterator_ref),
   DefunResult=sys_seq_iterator_ref.
/*
:- side_effect(assert_lsp(sys_seq_iterator_ref,
			  wl:lambda_def(defun, sys_seq_iterator_ref, f_sys_seq_iterator_ref, [sequence, sys_iterator], [[declare, [optimize, [safety, 0]]], [if, [sys_fixnump, sys_iterator], [aref, [ext_truly_the, vector, sequence], sys_iterator], [car, [ext_truly_the, cons, sys_iterator]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_ref,
			  wl:arglist_info(sys_seq_iterator_ref, f_sys_seq_iterator_ref, [sequence, sys_iterator], arginfo{all:[sequence, sys_iterator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iterator], opt:0, req:[sequence, sys_iterator], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_ref,
			  wl:init_args(exact_only, f_sys_seq_iterator_ref))).
*/
/*
(defun seq-iterator-set (sequence iterator value)
  (declare (optimize (safety 0)))
  (if (fixnump iterator)
      (setf (aref (truly-the vector sequence) iterator) value)
      (setf (car (truly-the cons iterator)) value)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:16198 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'seq-iterator-set',[sequence,iterator,value],[declare,[optimize,[safety,0]]],[if,[fixnump,iterator],[setf,[aref,['truly-the',vector,sequence],iterator],value],[setf,[car,['truly-the',cons,iterator]],value]]])
wl:lambda_def(defun, sys_seq_iterator_set, f_sys_seq_iterator_set, [sequence, sys_iterator, sys_value], [[declare, [optimize, [safety, 0]]], [if, [sys_fixnump, sys_iterator], [setf, [aref, [ext_truly_the, vector, sequence], sys_iterator], sys_value], [setf, [car, [ext_truly_the, cons, sys_iterator]], sys_value]]]).
wl:arglist_info(sys_seq_iterator_set, f_sys_seq_iterator_set, [sequence, sys_iterator, sys_value], arginfo{all:[sequence, sys_iterator, sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iterator, sys_value], opt:0, req:[sequence, sys_iterator, sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_seq_iterator_set).

/*

### Compiled:  `SYS::SEQ-ITERATOR-SET` 
*/
f_sys_seq_iterator_set(Sequence, Iterator, Value, FnResult) :-
	nop(global_env(Env)),
	Setf_Env=[bv(sequence, Sequence), bv(sys_iterator, Iterator), bv(sys_value, Value)|Env],
	cl_declare([optimize, [safety, 0]], Declare_Ret),
	get_var(Setf_Env, sys_iterator, Iterator_Get),
	f_sys_fixnump(Iterator_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(Setf_Env, sys_value, Value_Get),
	    f_ext_truly_the(vector, sequence, Sequence21),
	    get_var(Setf_Env, sys_iterator, Iterator_Get12),
	    set_place(Setf_Env,
		      setf,
		      [aref, Sequence21, Iterator_Get12],
		      [Value_Get],
		      Setf_R),
	    FnResult=Setf_R
	;   f_ext_truly_the(cons, sys_iterator, Iterator22),
	    get_var(Setf_Env, sys_value, Value_Get13),
	    cl_rplaca(Iterator22, Value_Get13, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_sys_seq_iterator_set, classof, claz_function),
   set_opv(sys_seq_iterator_set, compile_as, kw_function),
   set_opv(sys_seq_iterator_set, function, f_sys_seq_iterator_set),
   DefunResult=sys_seq_iterator_set.
/*
:- side_effect(assert_lsp(sys_seq_iterator_set,
			  wl:lambda_def(defun, sys_seq_iterator_set, f_sys_seq_iterator_set, [sequence, sys_iterator, sys_value], [[declare, [optimize, [safety, 0]]], [if, [sys_fixnump, sys_iterator], [setf, [aref, [ext_truly_the, vector, sequence], sys_iterator], sys_value], [setf, [car, [ext_truly_the, cons, sys_iterator]], sys_value]]]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_set,
			  wl:arglist_info(sys_seq_iterator_set, f_sys_seq_iterator_set, [sequence, sys_iterator, sys_value], arginfo{all:[sequence, sys_iterator, sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iterator, sys_value], opt:0, req:[sequence, sys_iterator, sys_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_set,
			  wl:init_args(exact_only, f_sys_seq_iterator_set))).
*/
/*
(defun seq-iterator-next (sequence iterator)
  (declare (optimize (safety 0)))
  (cond ((fixnump iterator)
         (let ((aux (1+ iterator)))
           (declare (fixnum aux))
           (and (< aux (length (truly-the vector sequence)))
                aux)))
        ((atom iterator)
         (error-not-a-sequence iterator))
        (t
         (setf iterator (cdr (truly-the cons iterator)))
         (unless (listp iterator)
           (error-not-a-sequence iterator))
         iterator)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:16430 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'seq-iterator-next',[sequence,iterator],[declare,[optimize,[safety,0]]],[cond,[[fixnump,iterator],[let,[[aux,['1+',iterator]]],[declare,[fixnum,aux]],[and,[<,aux,[length,['truly-the',vector,sequence]]],aux]]],[[atom,iterator],['error-not-a-sequence',iterator]],[t,[setf,iterator,[cdr,['truly-the',cons,iterator]]],[unless,[listp,iterator],['error-not-a-sequence',iterator]],iterator]]])
wl:lambda_def(defun, sys_seq_iterator_next, f_sys_seq_iterator_next, [sequence, sys_iterator], [[declare, [optimize, [safety, 0]]], [cond, [[sys_fixnump, sys_iterator], [let, [[sys_aux, ['1+', sys_iterator]]], [declare, [fixnum, sys_aux]], [and, [<, sys_aux, [length, [ext_truly_the, vector, sequence]]], sys_aux]]], [[atom, sys_iterator], [sys_error_not_a_sequence, sys_iterator]], [t, [setf, sys_iterator, [cdr, [ext_truly_the, cons, sys_iterator]]], [unless, [listp, sys_iterator], [sys_error_not_a_sequence, sys_iterator]], sys_iterator]]]).
wl:arglist_info(sys_seq_iterator_next, f_sys_seq_iterator_next, [sequence, sys_iterator], arginfo{all:[sequence, sys_iterator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iterator], opt:0, req:[sequence, sys_iterator], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_seq_iterator_next).

/*

### Compiled:  `SYS::SEQ-ITERATOR-NEXT` 
*/
f_sys_seq_iterator_next(Sequence, Iterator, LetResult) :-
	nop(global_env(Env)),
	Env39=[bv(sequence, Sequence), bv(sys_iterator, Iterator)|Env],
	cl_declare([optimize, [safety, 0]], Declare_Ret),
	get_var(Env39, sys_iterator, Iterator_Get),
	f_sys_fixnump(Iterator_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env39, sys_iterator, Iterator_Get12),
	    '1+'(Iterator_Get12, Aux_Init),
	    LEnv=[bv(sys_aux, Aux_Init)|Env39],
	    cl_declare([fixnum, sys_aux], Declare_Ret46),
	    get_var(LEnv, sys_aux, Aux_Get),
	    f_ext_truly_the(vector, sequence, Sequence42),
	    cl_length(Sequence42, PredArg2Result),
	    (   Aux_Get<PredArg2Result
	    ->  get_var(LEnv, sys_aux, Aux_Get19),
		LetResult=Aux_Get19
	    ;   LetResult=[]
	    )
	;   get_var(Env39, sys_iterator, Iterator_Get22),
	    (   Iterator_Get22\=[CAR|CDR]
	    ->  get_var(Env39, sys_iterator, Iterator_Get25),
		f_sys_error_not_a_sequence(Iterator_Get25, TrueResult33),
		LetResult=TrueResult33
	    ;   f_ext_truly_the(cons, sys_iterator, Iterator43),
		cl_cdr(Iterator43, Iterator44),
		set_var(Env39, sys_iterator, Iterator44),
		get_var(Env39, sys_iterator, Iterator_Get27),
		(   is_listp(Iterator_Get27)
		->  _136050130=[]
		;   get_var(Env39, sys_iterator, Iterator_Get30),
		    f_sys_error_not_a_sequence(Iterator_Get30, ElseResult),
		    _136050130=ElseResult
		),
		get_var(Env39, sys_iterator, Iterator_Get32),
		LetResult=Iterator_Get32
	    )
	).
:- set_opv(f_sys_seq_iterator_next, classof, claz_function),
   set_opv(sys_seq_iterator_next, compile_as, kw_function),
   set_opv(sys_seq_iterator_next, function, f_sys_seq_iterator_next),
   DefunResult=sys_seq_iterator_next.
/*
:- side_effect(assert_lsp(sys_seq_iterator_next,
			  wl:lambda_def(defun, sys_seq_iterator_next, f_sys_seq_iterator_next, [sequence, sys_iterator], [[declare, [optimize, [safety, 0]]], [cond, [[sys_fixnump, sys_iterator], [let, [[sys_aux, ['1+', sys_iterator]]], [declare, [fixnum, sys_aux]], [and, [<, sys_aux, [length, [ext_truly_the, vector, sequence]]], sys_aux]]], [[atom, sys_iterator], [sys_error_not_a_sequence, sys_iterator]], [t, [setf, sys_iterator, [cdr, [ext_truly_the, cons, sys_iterator]]], [unless, [listp, sys_iterator], [sys_error_not_a_sequence, sys_iterator]], sys_iterator]]]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_next,
			  wl:arglist_info(sys_seq_iterator_next, f_sys_seq_iterator_next, [sequence, sys_iterator], arginfo{all:[sequence, sys_iterator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sequence, sys_iterator], opt:0, req:[sequence, sys_iterator], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_next,
			  wl:init_args(exact_only, f_sys_seq_iterator_next))).
*/
/*
(defun seq-iterator-list-pop (values-list seq-list iterator-list)
  (declare (optimize (safety 0)))
  (do* ((it-list iterator-list)
        (v-list values-list))
       ((null v-list)
        values-list)
    (let* ((it (cons-car it-list))
           (sequence (cons-car seq-list)))
      (cond ((null it)
             (return nil))
            ((fixnump it)
             (let* ((n it) (s sequence))
               (declare (fixnum n) (vector s))
               (rplaca v-list (aref s n))
               (rplaca it-list (and (< (incf n) (length s)) n))))
            ((atom it)
             (error-not-a-sequence it))
            (t
             (rplaca v-list (cons-car it))
             (unless (listp (setf it (cons-cdr it)))
               (error-not-a-sequence it))
             (rplaca it-list it)))
      (setf v-list (cons-cdr v-list)
            it-list (cons-cdr it-list)
            seq-list (cons-cdr seq-list)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:16941 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'seq-iterator-list-pop',['values-list','seq-list','iterator-list'],[declare,[optimize,[safety,0]]],['do*',[['it-list','iterator-list'],['v-list','values-list']],[[null,'v-list'],'values-list'],['let*',[[it,['cons-car','it-list']],[sequence,['cons-car','seq-list']]],[cond,[[null,it],[return,[]]],[[fixnump,it],['let*',[[n,it],[s,sequence]],[declare,[fixnum,n],[vector,s]],[rplaca,'v-list',[aref,s,n]],[rplaca,'it-list',[and,[<,[incf,n],[length,s]],n]]]],[[atom,it],['error-not-a-sequence',it]],[t,[rplaca,'v-list',['cons-car',it]],[unless,[listp,[setf,it,['cons-cdr',it]]],['error-not-a-sequence',it]],[rplaca,'it-list',it]]],[setf,'v-list',['cons-cdr','v-list'],'it-list',['cons-cdr','it-list'],'seq-list',['cons-cdr','seq-list']]]]])
wl:lambda_def(defun, sys_seq_iterator_list_pop, f_sys_seq_iterator_list_pop, [values_list, sys_seq_list, sys_iterator_list], [[declare, [optimize, [safety, 0]]], [do_xx, [[sys_it_list, sys_iterator_list], [sys_v_list, values_list]], [[null, sys_v_list], values_list], [let_xx, [[sys_it, [sys_cons_car, sys_it_list]], [sequence, [sys_cons_car, sys_seq_list]]], [cond, [[null, sys_it], [return, []]], [[sys_fixnump, sys_it], [let_xx, [[n, sys_it], [sys_s, sequence]], [declare, [fixnum, n], [vector, sys_s]], [rplaca, sys_v_list, [aref, sys_s, n]], [rplaca, sys_it_list, [and, [<, [incf, n], [length, sys_s]], n]]]], [[atom, sys_it], [sys_error_not_a_sequence, sys_it]], [t, [rplaca, sys_v_list, [sys_cons_car, sys_it]], [unless, [listp, [setf, sys_it, [sys_cons_cdr, sys_it]]], [sys_error_not_a_sequence, sys_it]], [rplaca, sys_it_list, sys_it]]], [setf, sys_v_list, [sys_cons_cdr, sys_v_list], sys_it_list, [sys_cons_cdr, sys_it_list], sys_seq_list, [sys_cons_cdr, sys_seq_list]]]]]).
wl:arglist_info(sys_seq_iterator_list_pop, f_sys_seq_iterator_list_pop, [values_list, sys_seq_list, sys_iterator_list], arginfo{all:[values_list, sys_seq_list, sys_iterator_list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[values_list, sys_seq_list, sys_iterator_list], opt:0, req:[values_list, sys_seq_list, sys_iterator_list], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_seq_iterator_list_pop).

/*

### Compiled:  `SYS::SEQ-ITERATOR-LIST-POP` 
*/
f_sys_seq_iterator_list_pop(Values_list, Seq_list, Iterator_list, FnResult) :-
	nop(global_env(Env)),
	Env159=[bv(values_list, Values_list), bv(sys_seq_list, Seq_list), bv(sys_iterator_list, Iterator_list)|Env],
	catch(( ( cl_declare([optimize, [safety, 0]], Declare_Ret),
		  get_var(Env159, sys_iterator_list, Iterator_list_Get),
		  get_var(Env159, values_list, Values_list_Get),
		  BlockExitEnv=[bv(sys_it_list, Iterator_list_Get), bv(sys_v_list, Values_list_Get)|Env159],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_2), get_var(BlockExitEnv, sys_v_list, IFTEST86), (IFTEST86==[]->get_var(BlockExitEnv, values_list, RetResult89), throw(block_exit([], RetResult89)), _TBResult=ThrowResult90;get_var(BlockExitEnv, sys_it_list, It_list_Get96), f_sys_cons_car(It_list_Get96, It_Init98), get_var(BlockExitEnv, sys_seq_list, Seq_list_Get97), f_sys_cons_car(Seq_list_Get97, Sequence_Init99), LEnv95=[bv(sys_it, It_Init98), bv(sequence, Sequence_Init99)|BlockExitEnv], get_var(LEnv95, sys_it, IFTEST100), (IFTEST100==[]->throw(block_exit([], [])), ElseResult147=ThrowResult104;get_var(LEnv95, sys_it, It_Get108), f_sys_fixnump(It_Get108, IFTEST106), (IFTEST106\==[]->get_var(LEnv95, sequence, Sequence_Get113), get_var(LEnv95, sys_it, It_Get112), LEnv111=[bv(n, It_Get112), bv(sys_s, Sequence_Get113)|LEnv95], cl_declare([fixnum, n], [vector, sys_s], Declare_Ret169), get_var(LEnv111, n, N_Get118), get_var(LEnv111, sys_s, S_Get117), get_var(LEnv111, sys_v_list, V_list_Get116), cl_aref(S_Get117, [N_Get118], Aref_Ret), cl_rplaca(V_list_Get116, Aref_Ret, Rplaca_Ret), get_var(LEnv111, sys_it_list, It_list_Get119), set_place(LEnv111, incf, [value, n], [], Incf_R121), get_var(LEnv111, sys_s, S_Get123), cl_length(S_Get123, PredArg2Result126), (Incf_R121<PredArg2Result126->get_var(LEnv111, n, N_Get127), _139489782=N_Get127;_139489782=[]), cl_rplaca(It_list_Get119, _139489782, LetResult110), ElseResult147=LetResult110;get_var(LEnv95, sys_it, It_Get130), (It_Get130\=[CAR|CDR]->get_var(LEnv95, sys_it, It_Get133), f_sys_error_not_a_sequence(It_Get133, TrueResult144), ElseResult147=TrueResult144;get_var(LEnv95, sys_it, It_Get135), get_var(LEnv95, sys_v_list, V_list_Get134), f_sys_cons_car(It_Get135, Cons_car_Ret), cl_rplaca(V_list_Get134, Cons_car_Ret, Rplaca_Ret175), get_var(LEnv95, sys_it, It_Get137), f_sys_cons_cdr(It_Get137, PredArgResult139), set_var(LEnv95, sys_it, PredArgResult139), (is_listp(PredArgResult139)->_139632364=[];get_var(LEnv95, sys_it, It_Get140), f_sys_error_not_a_sequence(It_Get140, ElseResult141), _139632364=ElseResult141), get_var(LEnv95, sys_it, It_Get143), get_var(LEnv95, sys_it_list, It_list_Get142), cl_rplaca(It_list_Get142, It_Get143, ElseResult145), ElseResult147=ElseResult145))), get_var(LEnv95, sys_v_list, V_list_Get150), f_sys_cons_cdr(V_list_Get150, V_list), set_var(LEnv95, sys_v_list, V_list), get_var(LEnv95, sys_it_list, It_list_Get151), f_sys_cons_cdr(It_list_Get151, It_list), set_var(LEnv95, sys_it_list, It_list), get_var(LEnv95, sys_seq_list, Seq_list_Get152), f_sys_cons_cdr(Seq_list_Get152, LetResult94), set_var(LEnv95, sys_seq_list, LetResult94), cl_psetq(Psetq_Ret), goto(do_label_2, BlockExitEnv), _TBResult=_GORES153)),
					  
					  [ addr(addr_tagbody_2_do_label_2,
						 do_label_2,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, sys_v_list, IFTEST), (IFTEST==[]->get_var(BlockExitEnv, values_list, Values_list_Get19), throw(block_exit([], Values_list_Get19)), _140491804=ThrowResult;get_var(BlockExitEnv, sys_it_list, Cons_car_Param), f_sys_cons_car(Cons_car_Param, Cons_car_Ret177), get_var(BlockExitEnv, sys_seq_list, Cons_car_Param166), f_sys_cons_car(Cons_car_Param166, Cons_car_Ret178), LEnv23=[bv(sys_it, Cons_car_Ret177), bv(sequence, Cons_car_Ret178)|BlockExitEnv], get_var(LEnv23, sys_it, IFTEST28), (IFTEST28==[]->throw(block_exit([], [])), _140491984=ThrowResult32;get_var(LEnv23, sys_it, It_Get36), f_sys_fixnump(It_Get36, IFTEST34), (IFTEST34\==[]->get_var(LEnv23, sequence, Sequence_Get), get_var(LEnv23, sys_it, It_Get40), LEnv39=[bv(n, It_Get40), bv(sys_s, Sequence_Get)|LEnv23], cl_declare([fixnum, n], [vector, sys_s], Declare_Ret179), get_var(LEnv39, n, Get_var_Ret), get_var(LEnv39, sys_s, Aref_Param), get_var(LEnv39, sys_v_list, V_list_Get44), cl_aref(Aref_Param, [Get_var_Ret], Aref_Ret181), cl_rplaca(V_list_Get44, Aref_Ret181, Rplaca_Ret182), get_var(LEnv39, sys_it_list, It_list_Get47), set_place(LEnv39, incf, [value, n], [], Incf_R), get_var(LEnv39, sys_s, S_Get51), cl_length(S_Get51, Length_Ret), (Incf_R<Length_Ret->get_var(LEnv39, n, N_Get55), _140492444=N_Get55;_140492444=[]), cl_rplaca(It_list_Get47, _140492444, LetResult38), ElseResult77=LetResult38;get_var(LEnv23, sys_it, It_Get58), (It_Get58\=[CAR184|CDR185]->get_var(LEnv23, sys_it, It_Get61), f_sys_error_not_a_sequence(It_Get61, TrueResult72), ElseResult75=TrueResult72;get_var(LEnv23, sys_it, It_Get63), get_var(LEnv23, sys_v_list, V_list_Get62), f_sys_cons_car(It_Get63, Cons_car_Ret186), cl_rplaca(V_list_Get62, Cons_car_Ret186, Rplaca_Ret187), get_var(LEnv23, sys_it, It_Get65), f_sys_cons_cdr(It_Get65, PredArgResult67), set_var(LEnv23, sys_it, PredArgResult67), (is_listp(PredArgResult67)->_140492808=[];get_var(LEnv23, sys_it, It_Get68), f_sys_error_not_a_sequence(It_Get68, A_sequence_Ret), _140492808=A_sequence_Ret), get_var(LEnv23, sys_it, It_Get71), get_var(LEnv23, sys_it_list, It_list_Get70), cl_rplaca(It_list_Get70, It_Get71, ElseResult73), ElseResult75=ElseResult73), ElseResult77=ElseResult75), _140491984=ElseResult77), get_var(LEnv23, sys_v_list, V_list_Get78), f_sys_cons_cdr(V_list_Get78, Cons_cdr_Ret), set_var(LEnv23, sys_v_list, Cons_cdr_Ret), get_var(LEnv23, sys_it_list, It_list_Get79), f_sys_cons_cdr(It_list_Get79, Cons_cdr_Ret190), set_var(LEnv23, sys_it_list, Cons_cdr_Ret190), get_var(LEnv23, sys_seq_list, Seq_list_Get80), f_sys_cons_cdr(Seq_list_Get80, LetResult22), set_var(LEnv23, sys_seq_list, LetResult22), cl_psetq(Psetq_Ret191), goto(do_label_2, BlockExitEnv), _140491804=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_seq_iterator_list_pop, FnResult),
	      true).
:- set_opv(f_sys_seq_iterator_list_pop, classof, claz_function),
   set_opv(sys_seq_iterator_list_pop, compile_as, kw_function),
   set_opv(sys_seq_iterator_list_pop, function, f_sys_seq_iterator_list_pop),
   DefunResult=sys_seq_iterator_list_pop.
/*
:- side_effect(assert_lsp(sys_seq_iterator_list_pop,
			  wl:lambda_def(defun, sys_seq_iterator_list_pop, f_sys_seq_iterator_list_pop, [values_list, sys_seq_list, sys_iterator_list], [[declare, [optimize, [safety, 0]]], [do_xx, [[sys_it_list, sys_iterator_list], [sys_v_list, values_list]], [[null, sys_v_list], values_list], [let_xx, [[sys_it, [sys_cons_car, sys_it_list]], [sequence, [sys_cons_car, sys_seq_list]]], [cond, [[null, sys_it], [return, []]], [[sys_fixnump, sys_it], [let_xx, [[n, sys_it], [sys_s, sequence]], [declare, [fixnum, n], [vector, sys_s]], [rplaca, sys_v_list, [aref, sys_s, n]], [rplaca, sys_it_list, [and, [<, [incf, n], [length, sys_s]], n]]]], [[atom, sys_it], [sys_error_not_a_sequence, sys_it]], [t, [rplaca, sys_v_list, [sys_cons_car, sys_it]], [unless, [listp, [setf, sys_it, [sys_cons_cdr, sys_it]]], [sys_error_not_a_sequence, sys_it]], [rplaca, sys_it_list, sys_it]]], [setf, sys_v_list, [sys_cons_cdr, sys_v_list], sys_it_list, [sys_cons_cdr, sys_it_list], sys_seq_list, [sys_cons_cdr, sys_seq_list]]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_list_pop,
			  wl:arglist_info(sys_seq_iterator_list_pop, f_sys_seq_iterator_list_pop, [values_list, sys_seq_list, sys_iterator_list], arginfo{all:[values_list, sys_seq_list, sys_iterator_list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[values_list, sys_seq_list, sys_iterator_list], opt:0, req:[values_list, sys_seq_list, sys_iterator_list], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_seq_iterator_list_pop,
			  wl:init_args(exact_only, f_sys_seq_iterator_list_pop))).
*/
/*
(defun coerce-to-list (object)
  (if (listp object)
      object
      (do ((it (make-seq-iterator object) (seq-iterator-next object it))
           (output nil))
          ((null it) (nreverse output))
        (push (seq-iterator-ref object it) output))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:17895 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'coerce-to-list',[object],[if,[listp,object],object,[do,[[it,['make-seq-iterator',object],['seq-iterator-next',object,it]],[output,[]]],[[null,it],[nreverse,output]],[push,['seq-iterator-ref',object,it],output]]]])
wl:lambda_def(defun, sys_coerce_to_list, f_sys_coerce_to_list, [sys_object], [[if, [listp, sys_object], sys_object, [do, [[sys_it, [sys_make_seq_iterator, sys_object], [sys_seq_iterator_next, sys_object, sys_it]], [sys_output, []]], [[null, sys_it], [nreverse, sys_output]], [push, [sys_seq_iterator_ref, sys_object, sys_it], sys_output]]]]).
wl:arglist_info(sys_coerce_to_list, f_sys_coerce_to_list, [sys_object], arginfo{all:[sys_object], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object], opt:0, req:[sys_object], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_coerce_to_list).

/*

### Compiled:  `SYS::COERCE-TO-LIST` 
*/
f_sys_coerce_to_list(Object, FnResult) :-
	nop(global_env(Env)),
	Env60=[bv(sys_object, Object)|Env],
	catch(( ( get_var(Env60, sys_object, Object_Get),
		  (   is_listp(Object_Get)
		  ->  get_var(Env60, sys_object, Object_Get10),
		      _146215066=Object_Get10
		  ;   get_var(Env60, sys_object, Object_Get14),
		      f_sys_make_seq_iterator(Object_Get14, [], It_Init),
		      AEnv=[bv(sys_it, It_Init), bv(sys_output, [])|Env60],
		      catch(( call_addr_block(AEnv,
					      (push_label(do_label_3), get_var(AEnv, sys_it, IFTEST37), (IFTEST37==[]->get_var(AEnv, sys_output, Output_Get42), cl_nreverse(Output_Get42, RetResult40), throw(block_exit([], RetResult40)), _TBResult=ThrowResult41;get_var(AEnv, sys_it, It_Get48), get_var(AEnv, sys_object, Object_Get47), get_var(AEnv, sys_output, Output_Get44), set_place(AEnv, push, [sys_seq_iterator_ref, Object_Get47, It_Get48], [Output_Get44], Push_R45), get_var(AEnv, sys_it, It_Get51), get_var(AEnv, sys_object, Object_Get50), f_sys_seq_iterator_next(Object_Get50, It_Get51, It), set_var(AEnv, sys_it, It), goto(do_label_3, AEnv), _TBResult=_GORES52)),
					      
					      [ addr(addr_tagbody_3_do_label_3,
						     do_label_3,
						     '$unused',
						     AEnv,
						     (get_var(AEnv, sys_it, IFTEST17), (IFTEST17==[]->get_var(AEnv, sys_output, Nreverse_Param), cl_nreverse(Nreverse_Param, Nreverse_Ret), throw(block_exit([], Nreverse_Ret)), _146512478=ThrowResult;get_var(AEnv, sys_it, It_Get28), get_var(AEnv, sys_object, Object_Get27), get_var(AEnv, sys_output, Output_Get24), set_place(AEnv, push, [sys_seq_iterator_ref, Object_Get27, It_Get28], [Output_Get24], Set_place_Ret), get_var(AEnv, sys_it, It_Get31), get_var(AEnv, sys_object, Object_Get30), f_sys_seq_iterator_next(Object_Get30, It_Get31, Iterator_next_Ret), set_var(AEnv, sys_it, Iterator_next_Ret), goto(do_label_3, AEnv), _146512478=_GORES)))
					      ]),
			      []=LetResult
			    ),
			    block_exit([], LetResult),
			    true),
		      _146215066=LetResult
		  )
		),
		_146215066=FnResult
	      ),
	      block_exit(sys_coerce_to_list, FnResult),
	      true).
:- set_opv(f_sys_coerce_to_list, classof, claz_function),
   set_opv(sys_coerce_to_list, compile_as, kw_function),
   set_opv(sys_coerce_to_list, function, f_sys_coerce_to_list),
   DefunResult=sys_coerce_to_list.
/*
:- side_effect(assert_lsp(sys_coerce_to_list,
			  wl:lambda_def(defun, sys_coerce_to_list, f_sys_coerce_to_list, [sys_object], [[if, [listp, sys_object], sys_object, [do, [[sys_it, [sys_make_seq_iterator, sys_object], [sys_seq_iterator_next, sys_object, sys_it]], [sys_output, []]], [[null, sys_it], [nreverse, sys_output]], [push, [sys_seq_iterator_ref, sys_object, sys_it], sys_output]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_coerce_to_list,
			  wl:arglist_info(sys_coerce_to_list, f_sys_coerce_to_list, [sys_object], arginfo{all:[sys_object], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object], opt:0, req:[sys_object], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_coerce_to_list,
			  wl:init_args(exact_only, f_sys_coerce_to_list))).
*/
/*
(defun coerce-to-vector (object elt-type length simple-array-p)
  (let ((output object))
    (unless (and (vectorp object)
                 (or (null simple-array-p) (simple-array-p object))
                 (eq (array-element-type object) elt-type))
      (let* ((final-length (if (eq length '*) (length object) length)))
        (setf output (make-vector elt-type final-length nil nil nil 0))
        (do ((i (make-seq-iterator object) (seq-iterator-next output i))
             (j 0 (truly-the index (1+ j))))
            ((= j final-length)
             (setf object output))
          (declare (index j))
          (setf (aref output j) (seq-iterator-ref object i)))))
    (unless (eq length '*)
      (unless (= length (length output))
        (check-type output `(vector ,elt-type (,length)) "coerced object")))
    output))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:18161 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'coerce-to-vector',[object,'elt-type',length,'simple-array-p'],[let,[[output,object]],[unless,[and,[vectorp,object],[or,[null,'simple-array-p'],['simple-array-p',object]],[eq,['array-element-type',object],'elt-type']],['let*',[['final-length',[if,[eq,length,[quote,*]],[length,object],length]]],[setf,output,['make-vector','elt-type','final-length',[],[],[],0]],[do,[[i,['make-seq-iterator',object],['seq-iterator-next',output,i]],[j,0,['truly-the',index,['1+',j]]]],[[=,j,'final-length'],[setf,object,output]],[declare,[index,j]],[setf,[aref,output,j],['seq-iterator-ref',object,i]]]]],[unless,[eq,length,[quote,*]],[unless,[=,length,[length,output]],['check-type',output,['#BQ',[vector,['#COMMA','elt-type'],[['#COMMA',length]]]],'$STRING'("coerced object")]]],output]])
wl:lambda_def(defun, sys_coerce_to_vector, f_sys_coerce_to_vector, [sys_object, sys_elt_type, length, sys_simple_array_p], [[let, [[sys_output, sys_object]], [unless, [and, [vectorp, sys_object], [or, [null, sys_simple_array_p], [sys_simple_array_p, sys_object]], [eq, [array_element_type, sys_object], sys_elt_type]], [let_xx, [[sys_final_length, [if, [eq, length, [quote, *]], [length, sys_object], length]]], [setf, sys_output, [sys_make_vector, sys_elt_type, sys_final_length, [], [], [], 0]], [do, [[sys_i, [sys_make_seq_iterator, sys_object], [sys_seq_iterator_next, sys_output, sys_i]], [sys_j, 0, [ext_truly_the, index, ['1+', sys_j]]]], [[=, sys_j, sys_final_length], [setf, sys_object, sys_output]], [declare, [index, sys_j]], [setf, [aref, sys_output, sys_j], [sys_seq_iterator_ref, sys_object, sys_i]]]]], [unless, [eq, length, [quote, *]], [unless, [=, length, [length, sys_output]], [check_type, sys_output, ['#BQ', [vector, ['#COMMA', sys_elt_type], [['#COMMA', length]]]], '$ARRAY'([*], claz_base_character, "coerced object")]]], sys_output]]).
wl:arglist_info(sys_coerce_to_vector, f_sys_coerce_to_vector, [sys_object, sys_elt_type, length, sys_simple_array_p], arginfo{all:[sys_object, sys_elt_type, length, sys_simple_array_p], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object, sys_elt_type, length, sys_simple_array_p], opt:0, req:[sys_object, sys_elt_type, length, sys_simple_array_p], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_coerce_to_vector).

/*

### Compiled:  `SYS::COERCE-TO-VECTOR` 
*/
f_sys_coerce_to_vector(Object, Elt_type, Length, Simple_array_p, FnResult) :-
	nop(global_env(Env)),
	Env99=[bv(sys_object, Object), bv(sys_elt_type, Elt_type), bv(length, Length), bv(sys_simple_array_p, Simple_array_p)|Env],
	catch(( ( get_var(Env99, sys_object, Object_Get),
		  LEnv=[bv(sys_output, Object_Get)|Env99],
		  get_var(LEnv, sys_object, Object_Get14),
		  (   is_vectorp(Object_Get14)
		  ->  (   get_var(LEnv, sys_simple_array_p, Simple_array_p_Get),
			  cl_null(Simple_array_p_Get, FORM1_Res),
			  FORM1_Res\==[],
			  IFTEST17=FORM1_Res
		      ->  true
		      ;   get_var(LEnv, sys_object, Object_Get20),
			  f_sys_simple_array_p(Object_Get20, Array_p_Ret),
			  IFTEST17=Array_p_Ret
		      ),
		      (   IFTEST17\==[]
		      ->  get_var(LEnv, sys_object, Object_Get22),
			  cl_array_element_type(Object_Get22, Eq_Param),
			  get_var(LEnv, sys_elt_type, Elt_type_Get),
			  cl_eq(Eq_Param, Elt_type_Get, TrueResult),
			  IFTEST=TrueResult
		      ;   IFTEST=[]
		      )
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  _148813018=[]
		  ;   (   is_eq(length, *)
		      ->  get_var(LEnv, sys_object, Object_Get31),
			  cl_length(Object_Get31, TrueResult32),
			  Final_length_Init=TrueResult32
		      ;   Final_length_Init=length
		      ),
		      LEnv28=[bv(sys_final_length, Final_length_Init)|LEnv],
		      get_var(LEnv28, sys_elt_type, Elt_type_Get34),
		      get_var(LEnv28, sys_final_length, Final_length_Get),
		      f_sys_make_vector(Elt_type_Get34,
					Final_length_Get,
					[],
					[],
					[],
					0,
					Output),
		      set_var(LEnv28, sys_output, Output),
		      get_var(LEnv28, sys_object, Object_Get39),
		      f_sys_make_seq_iterator(Object_Get39, [], I_Init),
		      Setf_Env=[bv(sys_i, I_Init), bv(sys_j, 0)|LEnv28],
		      catch(( call_addr_block(Setf_Env,
					      (push_label(do_label_4), get_var(Setf_Env, sys_final_length, Final_length_Get67), get_var(Setf_Env, sys_j, J_Get66), (J_Get66=:=Final_length_Get67->get_var(Setf_Env, sys_output, RetResult71), set_var(Setf_Env, sys_object, RetResult71), throw(block_exit([], RetResult71)), _TBResult=ThrowResult72;cl_declare([index, sys_j], Declare_Ret), get_var(Setf_Env, sys_i, I_Get76), get_var(Setf_Env, sys_object, Object_Get75), f_sys_seq_iterator_ref(Object_Get75, I_Get76, Iterator_ref_Ret), get_var(Setf_Env, sys_j, J_Get80), get_var(Setf_Env, sys_output, Output_Get79), set_place(Setf_Env, setf, [aref, Output_Get79, J_Get80], [Iterator_ref_Ret], Setf_R77), get_var(Setf_Env, sys_i, I_Get82), get_var(Setf_Env, sys_output, Output_Get81), f_sys_seq_iterator_next(Output_Get81, I_Get82, I), f_ext_truly_the(index, ['1+', sys_j], J), set_var(Setf_Env, sys_i, I), set_var(Setf_Env, sys_j, J), goto(do_label_4, Setf_Env), _TBResult=_GORES83)),
					      
					      [ addr(addr_tagbody_4_do_label_4,
						     do_label_4,
						     '$unused',
						     Setf_Env,
						     (get_var(Setf_Env, sys_final_length, Final_length_Get44), get_var(Setf_Env, sys_j, J_Get), (J_Get=:=Final_length_Get44->get_var(Setf_Env, sys_output, Get_var_Ret), set_var(Setf_Env, sys_object, Get_var_Ret), throw(block_exit([], Get_var_Ret)), _149276284=ThrowResult;cl_declare([index, sys_j], Declare_Ret112), get_var(Setf_Env, sys_i, Get_var_Ret113), get_var(Setf_Env, sys_object, Object_Get52), f_sys_seq_iterator_ref(Object_Get52, Get_var_Ret113, Iterator_ref_Ret114), get_var(Setf_Env, sys_j, J_Get57), get_var(Setf_Env, sys_output, Output_Get56), set_place(Setf_Env, setf, [aref, Output_Get56, J_Get57], [Iterator_ref_Ret114], Set_place_Ret), get_var(Setf_Env, sys_i, I_Get59), get_var(Setf_Env, sys_output, Output_Get58), f_sys_seq_iterator_next(Output_Get58, I_Get59, Iterator_next_Ret), f_ext_truly_the(index, ['1+', sys_j], Truly_the_Ret), set_var(Setf_Env, sys_i, Iterator_next_Ret), set_var(Setf_Env, sys_j, Truly_the_Ret), goto(do_label_4, Setf_Env), _149276284=_GORES)))
					      ]),
			      []=LetResult37
			    ),
			    block_exit([], LetResult37),
			    true),
		      _148813018=LetResult37
		  ),
		  (   is_eq(length, *)
		  ->  ElseResult95=[]
		  ;   get_var(LEnv, sys_output, Output_Get91),
		      cl_length(Output_Get91, PredArg2Result93),
		      (   length=:=PredArg2Result93
		      ->  ElseResult95=[]
		      ;   cl_check_type(sys_output,
					
					[ '#BQ',
					  
					  [ vector,
					    ['#COMMA', sys_elt_type],
					    [['#COMMA', length]]
					  ]
					],
					'$ARRAY'([*],
						 claz_base_character,
						 "coerced object"),
					ElseResult94),
			  ElseResult95=ElseResult94
		      )
		  ),
		  get_var(LEnv, sys_output, Output_Get96)
		),
		Output_Get96=FnResult
	      ),
	      block_exit(sys_coerce_to_vector, FnResult),
	      true).
:- set_opv(f_sys_coerce_to_vector, classof, claz_function),
   set_opv(sys_coerce_to_vector, compile_as, kw_function),
   set_opv(sys_coerce_to_vector, function, f_sys_coerce_to_vector),
   DefunResult=sys_coerce_to_vector.
/*
:- side_effect(assert_lsp(sys_coerce_to_vector,
			  wl:lambda_def(defun, sys_coerce_to_vector, f_sys_coerce_to_vector, [sys_object, sys_elt_type, length, sys_simple_array_p], [[let, [[sys_output, sys_object]], [unless, [and, [vectorp, sys_object], [or, [null, sys_simple_array_p], [sys_simple_array_p, sys_object]], [eq, [array_element_type, sys_object], sys_elt_type]], [let_xx, [[sys_final_length, [if, [eq, length, [quote, *]], [length, sys_object], length]]], [setf, sys_output, [sys_make_vector, sys_elt_type, sys_final_length, [], [], [], 0]], [do, [[sys_i, [sys_make_seq_iterator, sys_object], [sys_seq_iterator_next, sys_output, sys_i]], [sys_j, 0, [ext_truly_the, index, ['1+', sys_j]]]], [[=, sys_j, sys_final_length], [setf, sys_object, sys_output]], [declare, [index, sys_j]], [setf, [aref, sys_output, sys_j], [sys_seq_iterator_ref, sys_object, sys_i]]]]], [unless, [eq, length, [quote, *]], [unless, [=, length, [length, sys_output]], [check_type, sys_output, ['#BQ', [vector, ['#COMMA', sys_elt_type], [['#COMMA', length]]]], '$ARRAY'([*], claz_base_character, "coerced object")]]], sys_output]]))).
*/
/*
:- side_effect(assert_lsp(sys_coerce_to_vector,
			  wl:arglist_info(sys_coerce_to_vector, f_sys_coerce_to_vector, [sys_object, sys_elt_type, length, sys_simple_array_p], arginfo{all:[sys_object, sys_elt_type, length, sys_simple_array_p], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object, sys_elt_type, length, sys_simple_array_p], opt:0, req:[sys_object, sys_elt_type, length, sys_simple_array_p], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_coerce_to_vector,
			  wl:init_args(exact_only, f_sys_coerce_to_vector))).
*/
/*
(defun concatenate (result-type &rest sequences)
  "Args: (type &rest sequences)
Returns a new sequence of the specified type, consisting of all elements of
SEQUENCEs."
  (do* ((length-list (mapcar #'length sequences) (rest length-list))
        (output (make-sequence result-type (apply #'+ length-list)))
        (sequences sequences (rest sequences))
        (i (make-seq-iterator output)))
      ((null sequences) output)
    (do* ((s (first sequences))
          (j (make-seq-iterator s) (seq-iterator-next s j)))
         ((null j))
      (seq-iterator-set output i (seq-iterator-ref s j))
      (setq i (seq-iterator-next output i)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:19012 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,concatenate,['result-type','&rest',sequences],'$STRING'("Args: (type &rest sequences)\r\nReturns a new sequence of the specified type, consisting of all elements of\r\nSEQUENCEs."),['do*',[['length-list',[mapcar,function(length),sequences],[rest,'length-list']],[output,['make-sequence','result-type',[apply,function(+),'length-list']]],[sequences,sequences,[rest,sequences]],[i,['make-seq-iterator',output]]],[[null,sequences],output],['do*',[[s,[first,sequences]],[j,['make-seq-iterator',s],['seq-iterator-next',s,j]]],[[null,j]],['seq-iterator-set',output,i,['seq-iterator-ref',s,j]],[setq,i,['seq-iterator-next',output,i]]]]])
doc: doc_string(concatenate,
	      _154261922,
	      function,
	      "Args: (type &rest sequences)\r\nReturns a new sequence of the specified type, consisting of all elements of\r\nSEQUENCEs.").

wl:lambda_def(defun, concatenate, cl_concatenate, [sys_result_type, c38_rest, sys_sequences], [[do_xx, [[sys_length_list, [mapcar, function(length), sys_sequences], [rest, sys_length_list]], [sys_output, [make_sequence, sys_result_type, [apply, function(+), sys_length_list]]], [sys_sequences, sys_sequences, [rest, sys_sequences]], [sys_i, [sys_make_seq_iterator, sys_output]]], [[null, sys_sequences], sys_output], [do_xx, [[sys_s, [first, sys_sequences]], [sys_j, [sys_make_seq_iterator, sys_s], [sys_seq_iterator_next, sys_s, sys_j]]], [[null, sys_j]], [sys_seq_iterator_set, sys_output, sys_i, [sys_seq_iterator_ref, sys_s, sys_j]], [setq, sys_i, [sys_seq_iterator_next, sys_output, sys_i]]]]]).
wl:arglist_info(concatenate, cl_concatenate, [sys_result_type, c38_rest, sys_sequences], arginfo{all:[sys_result_type], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_result_type, sys_sequences], opt:0, req:[sys_result_type], rest:[sys_sequences], sublists:0, whole:0}).
wl: init_args(1, cl_concatenate).

/*

### Compiled:  `CL:CONCATENATE` 
*/
cl_concatenate(Result_type, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_result_type, Result_type), bv(sys_sequences, RestNKeys)]|Env]|Env],
	catch(( ( get_var(GEnv, sys_sequences, Sequences_Get),
		  cl_mapcar(cl_length, [Sequences_Get], Length_list_Init),
		  get_var(GEnv, sys_length_list, Length_list_Get),
		  get_var(GEnv, sys_result_type, Result_type_Get),
		  cl_apply(+, Length_list_Get, Apply_Ret),
		  cl_make_sequence(Result_type_Get, Apply_Ret, [], Output_Init),
		  get_var(GEnv, sys_output, Output_Get),
		  get_var(GEnv, sys_sequences, Sequences_Get13),
		  f_sys_make_seq_iterator(Output_Get, [], I_Init),
		  BlockExitEnv=[bv(sys_length_list, Length_list_Init), bv(sys_output, Output_Init), bv(sys_sequences, Sequences_Get13), bv(sys_i, I_Init)|GEnv],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_5), get_var(BlockExitEnv, sys_sequences, IFTEST81), (IFTEST81==[]->get_var(BlockExitEnv, sys_output, RetResult84), throw(block_exit([], RetResult84)), _TBResult=ThrowResult85;get_var(BlockExitEnv, sys_sequences, Sequences_Get91), cl_car(Sequences_Get91, S_Init93), get_var(BlockExitEnv, sys_s, S_Get92), f_sys_make_seq_iterator(S_Get92, [], J_Init94), AEnv=[bv(sys_s, S_Init93), bv(sys_j, J_Init94)|BlockExitEnv], catch((call_addr_block(AEnv,  (push_label(do_label_7), get_var(AEnv, sys_j, IFTEST116), (IFTEST116==[]->throw(block_exit([], [])), _TBResult95=ThrowResult120;get_var(AEnv, sys_i, I_Get123), get_var(AEnv, sys_j, J_Get125), get_var(AEnv, sys_output, Output_Get122), get_var(AEnv, sys_s, S_Get124), f_sys_seq_iterator_ref(S_Get124, J_Get125, Iterator_ref_Ret), f_sys_seq_iterator_set(Output_Get122, I_Get123, Iterator_ref_Ret, Iterator_set_Ret), get_var(AEnv, sys_i, I_Get128), get_var(AEnv, sys_output, Output_Get127), f_sys_seq_iterator_next(Output_Get127, I_Get128, I), set_var(AEnv, sys_i, I), get_var(AEnv, sys_j, J_Get130), get_var(AEnv, sys_s, S_Get129), f_sys_seq_iterator_next(S_Get129, J_Get130, J), set_var(AEnv, sys_j, J), goto(do_label_7, AEnv), _TBResult95=_GORES131)), [addr(addr_tagbody_7_do_label_7, do_label_7, '$unused', AEnv,  (get_var(AEnv, sys_j, IFTEST96), (IFTEST96==[]->throw(block_exit([], [])), _TBResult95=ThrowResult100;get_var(AEnv, sys_i, I_Get103), get_var(AEnv, sys_j, J_Get105), get_var(AEnv, sys_output, Output_Get102), get_var(AEnv, sys_s, S_Get104), f_sys_seq_iterator_ref(S_Get104, J_Get105, Iterator_ref_Ret153), f_sys_seq_iterator_set(Output_Get102, I_Get103, Iterator_ref_Ret153, Iterator_set_Ret154), get_var(AEnv, sys_i, I_Get108), get_var(AEnv, sys_output, Output_Get107), f_sys_seq_iterator_next(Output_Get107, I_Get108, Iterator_next_Ret), set_var(AEnv, sys_i, Iterator_next_Ret), get_var(AEnv, sys_j, J_Get110), get_var(AEnv, sys_s, S_Get109), f_sys_seq_iterator_next(S_Get109, J_Get110, Iterator_next_Ret156), set_var(AEnv, sys_j, Iterator_next_Ret156), goto(do_label_7, AEnv), _TBResult95=_GORES111)))]), []=LetResult89), block_exit([], LetResult89), true), get_var(BlockExitEnv, sys_length_list, Length_list_Get135), cl_cdr(Length_list_Get135, Length_list), get_var(BlockExitEnv, sys_sequences, Sequences_Get136), cl_cdr(Sequences_Get136, Sequences), set_var(BlockExitEnv, sys_length_list, Length_list), set_var(BlockExitEnv, sys_sequences, Sequences), goto(do_label_5, BlockExitEnv), _TBResult=_GORES137)),
					  
					  [ addr(addr_tagbody_5_do_label_5,
						 do_label_5,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, sys_sequences, IFTEST), (IFTEST==[]->get_var(BlockExitEnv, sys_output, Output_Get25), throw(block_exit([], Output_Get25)), _156492908=ThrowResult;get_var(BlockExitEnv, sys_sequences, Sequences_Get30), cl_car(Sequences_Get30, Car_Ret), get_var(BlockExitEnv, sys_s, Seq_iterator_Param), f_sys_make_seq_iterator(Seq_iterator_Param, [], Seq_iterator_Ret), AEnv=[bv(sys_s, Car_Ret), bv(sys_j, Seq_iterator_Ret)|BlockExitEnv], catch((call_addr_block(AEnv,  (push_label(do_label_6), get_var(AEnv, sys_j, IFTEST55), (IFTEST55==[]->throw(block_exit([], [])), _TBResult34=ThrowResult59;get_var(AEnv, sys_i, I_Get62), get_var(AEnv, sys_j, J_Get64), get_var(AEnv, sys_output, Output_Get61), get_var(AEnv, sys_s, S_Get63), f_sys_seq_iterator_ref(S_Get63, J_Get64, Iterator_ref_Ret159), f_sys_seq_iterator_set(Output_Get61, I_Get62, Iterator_ref_Ret159, Iterator_set_Ret160), get_var(AEnv, sys_i, I_Get67), get_var(AEnv, sys_output, Output_Get66), f_sys_seq_iterator_next(Output_Get66, I_Get67, Iterator_next_Ret161), set_var(AEnv, sys_i, Iterator_next_Ret161), get_var(AEnv, sys_j, J_Get69), get_var(AEnv, sys_s, S_Get68), f_sys_seq_iterator_next(S_Get68, J_Get69, Iterator_next_Ret162), set_var(AEnv, sys_j, Iterator_next_Ret162), goto(do_label_6, AEnv), _TBResult34=_GORES70)), [addr(addr_tagbody_6_do_label_6, do_label_6, '$unused', AEnv,  (get_var(AEnv, sys_j, IFTEST35), (IFTEST35==[]->throw(block_exit([], [])), _TBResult34=ThrowResult39;get_var(AEnv, sys_i, Get_var_Ret), get_var(AEnv, sys_j, J_Get44), get_var(AEnv, sys_output, Output_Get41), get_var(AEnv, sys_s, S_Get43), f_sys_seq_iterator_ref(S_Get43, J_Get44, Iterator_ref_Ret164), f_sys_seq_iterator_set(Output_Get41, Get_var_Ret, Iterator_ref_Ret164, Iterator_set_Ret165), get_var(AEnv, sys_i, I_Get47), get_var(AEnv, sys_output, Output_Get46), f_sys_seq_iterator_next(Output_Get46, I_Get47, Iterator_next_Ret166), set_var(AEnv, sys_i, Iterator_next_Ret166), get_var(AEnv, sys_j, J_Get49), get_var(AEnv, sys_s, S_Get48), f_sys_seq_iterator_next(S_Get48, J_Get49, Iterator_next_Ret167), set_var(AEnv, sys_j, Iterator_next_Ret167), goto(do_label_6, AEnv), _TBResult34=_GORES)))]), []=LetResult28), block_exit([], LetResult28), true), get_var(BlockExitEnv, sys_length_list, Length_list_Get74), cl_cdr(Length_list_Get74, Cdr_Ret), get_var(BlockExitEnv, sys_sequences, Sequences_Get75), cl_cdr(Sequences_Get75, Cdr_Ret169), set_var(BlockExitEnv, sys_length_list, Cdr_Ret), set_var(BlockExitEnv, sys_sequences, Cdr_Ret169), goto(do_label_5, BlockExitEnv), _156492908=_GORES76)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(concatenate, FnResult),
	      true).
:- set_opv(cl_concatenate, classof, claz_function),
   set_opv(concatenate, compile_as, kw_function),
   set_opv(concatenate, function, cl_concatenate),
   DefunResult=concatenate.
/*
:- side_effect(assert_lsp(concatenate,
			  doc:doc_string(concatenate, _154261922, function, "Args: (type &rest sequences)\r\nReturns a new sequence of the specified type, consisting of all elements of\r\nSEQUENCEs."))).
*/
/*
:- side_effect(assert_lsp(concatenate,
			  wl:lambda_def(defun, concatenate, cl_concatenate, [sys_result_type, c38_rest, sys_sequences], [[do_xx, [[sys_length_list, [mapcar, function(length), sys_sequences], [rest, sys_length_list]], [sys_output, [make_sequence, sys_result_type, [apply, function(+), sys_length_list]]], [sys_sequences, sys_sequences, [rest, sys_sequences]], [sys_i, [sys_make_seq_iterator, sys_output]]], [[null, sys_sequences], sys_output], [do_xx, [[sys_s, [first, sys_sequences]], [sys_j, [sys_make_seq_iterator, sys_s], [sys_seq_iterator_next, sys_s, sys_j]]], [[null, sys_j]], [sys_seq_iterator_set, sys_output, sys_i, [sys_seq_iterator_ref, sys_s, sys_j]], [setq, sys_i, [sys_seq_iterator_next, sys_output, sys_i]]]]]))).
*/
/*
:- side_effect(assert_lsp(concatenate,
			  wl:arglist_info(concatenate, cl_concatenate, [sys_result_type, c38_rest, sys_sequences], arginfo{all:[sys_result_type], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_result_type, sys_sequences], opt:0, req:[sys_result_type], rest:[sys_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(concatenate, wl:init_args(1, cl_concatenate))).
*/
/*
(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (let* ((sequences (list* sequence more-sequences))
         (function (coerce-to-function function))
         output
         it)
    (when result-type
      (let ((l (length sequence)))
        (when more-sequences
          (setf l (reduce #'min more-sequences
                          :initial-value l
                          :key #'length)))
        (setf output (make-sequence result-type l)
              it (make-seq-iterator output))))
    (do-sequences (elt-list sequences :output output)
      (let ((value (apply function elt-list)))
        (when result-type
          (seq-iterator-set output it value)
          (setf it (seq-iterator-next output it)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:19673 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,map,['result-type',function,sequence,'&rest','more-sequences'],'$STRING'("Args: (type function sequence &rest more-sequences)\r\nCreates and returns a sequence of TYPE with K elements, with the N-th element\r\nbeing the value of applying FUNCTION to the N-th elements of the given\r\nSEQUENCEs, where K is the minimum length of the given SEQUENCEs."),['let*',[[sequences,['list*',sequence,'more-sequences']],[function,['coerce-to-function',function]],output,it],[when,'result-type',[let,[[l,[length,sequence]]],[when,'more-sequences',[setf,l,[reduce,function(min),'more-sequences',':initial-value',l,':key',function(length)]]],[setf,output,['make-sequence','result-type',l],it,['make-seq-iterator',output]]]],['do-sequences',['elt-list',sequences,':output',output],[let,[[value,[apply,function,'elt-list']]],[when,'result-type',['seq-iterator-set',output,it,value],[setf,it,['seq-iterator-next',output,it]]]]]]])
doc: doc_string(map,
	      _161216458,
	      function,
	      "Args: (type function sequence &rest more-sequences)\r\nCreates and returns a sequence of TYPE with K elements, with the N-th element\r\nbeing the value of applying FUNCTION to the N-th elements of the given\r\nSEQUENCEs, where K is the minimum length of the given SEQUENCEs.").

wl:lambda_def(defun, map, cl_map, [sys_result_type, function, sequence, c38_rest, sys_more_sequences], [[let_xx, [[sys_sequences, [list_xx, sequence, sys_more_sequences]], [function, [sys_coerce_to_function, function]], sys_output, sys_it], [when, sys_result_type, [let, [[sys_l, [length, sequence]]], [when, sys_more_sequences, [setf, sys_l, [reduce, function(min), sys_more_sequences, kw_initial_value, sys_l, kw_key, function(length)]]], [setf, sys_output, [make_sequence, sys_result_type, sys_l], sys_it, [sys_make_seq_iterator, sys_output]]]], [sys_do_sequences, [sys_elt_list, sys_sequences, kw_output, sys_output], [let, [[sys_value, [apply, function, sys_elt_list]]], [when, sys_result_type, [sys_seq_iterator_set, sys_output, sys_it, sys_value], [setf, sys_it, [sys_seq_iterator_next, sys_output, sys_it]]]]]]]).
wl:arglist_info(map, cl_map, [sys_result_type, function, sequence, c38_rest, sys_more_sequences], arginfo{all:[sys_result_type, function, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_result_type, function, sequence, sys_more_sequences], opt:0, req:[sys_result_type, function, sequence], rest:[sys_more_sequences], sublists:0, whole:0}).
wl: init_args(3, cl_map).

/*

### Compiled:  `CL:MAP` 
*/
cl_map(Result_type, Function, Sequence, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_result_type, Result_type), bv(function, Function), bv(sequence, Sequence), bv(sys_more_sequences, RestNKeys)]|Env]|Env],
	get_var(GEnv, sequence, Sequence_Get),
	get_var(GEnv, sys_more_sequences, More_sequences_Get),
	cl_list_xx(Sequence_Get, More_sequences_Get, Sequences_Init),
	f_sys_coerce_to_function(function, Function_Init),
	LEnv=[bv(sys_sequences, Sequences_Init), bv(function, Function_Init), bv(sys_output, []), bv(sys_it, [])|GEnv],
	get_var(LEnv, sys_result_type, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, sequence, Sequence_Get20),
	    cl_length(Sequence_Get20, L_Init),
	    LEnv19=[bv(sys_l, L_Init)|LEnv],
	    get_var(LEnv19, sys_more_sequences, IFTEST22),
	    (   IFTEST22\==[]
	    ->  get_var(LEnv19, sys_l, L_Get),
		get_var(LEnv19, sys_more_sequences, More_sequences_Get25),
		cl_reduce(cl_min,
			  More_sequences_Get25,
			  kw_initial_value,
			  L_Get,
			  kw_key,
			  cl_length,
			  TrueResult),
		set_var(LEnv19, sys_l, TrueResult),
		_161264782=TrueResult
	    ;   _161264782=[]
	    ),
	    get_var(LEnv19, sys_l, L_Get29),
	    get_var(LEnv19, sys_result_type, Result_type_Get28),
	    cl_make_sequence(Result_type_Get28, L_Get29, [], Output),
	    set_var(LEnv19, sys_output, Output),
	    get_var(LEnv19, sys_output, Output_Get),
	    f_sys_make_seq_iterator(Output_Get, [], LetResult18),
	    set_var(LEnv19, sys_it, LetResult18),
	    _161252534=LetResult18
	;   _161252534=[]
	),
	get_var(LEnv, sys_output, Output_Get33),
	get_var(LEnv, sys_sequences, Sequences_Get),
	f_sys_elt_list(Sequences_Get,
		       kw_output,
		       Output_Get33,
		       Do_sequences_Param),
	get_var(LEnv, sys_elt_list, Elt_list_Get),
	cl_apply(function, Elt_list_Get, Value_Init),
	LEnv36=[bv(sys_value, Value_Init)|LEnv],
	get_var(LEnv36, sys_result_type, IFTEST39),
	(   IFTEST39\==[]
	->  get_var(LEnv36, sys_it, It_Get),
	    get_var(LEnv36, sys_output, Output_Get42),
	    get_var(LEnv36, sys_value, Value_Get),
	    f_sys_seq_iterator_set(Output_Get42,
				   It_Get,
				   Value_Get,
				   Iterator_set_Ret),
	    get_var(LEnv36, sys_it, It_Get46),
	    get_var(LEnv36, sys_output, Output_Get45),
	    f_sys_seq_iterator_next(Output_Get45, It_Get46, TrueResult47),
	    set_var(LEnv36, sys_it, TrueResult47),
	    LetResult35=TrueResult47
	;   LetResult35=[]
	),
	f_sys_do_sequences(Do_sequences_Param, LetResult35, LetResult),
	LetResult=FnResult.
:- set_opv(cl_map, classof, claz_function),
   set_opv(map, compile_as, kw_function),
   set_opv(map, function, cl_map),
   DefunResult=map.
/*
:- side_effect(assert_lsp(map,
			  doc:doc_string(map, _161216458, function, "Args: (type function sequence &rest more-sequences)\r\nCreates and returns a sequence of TYPE with K elements, with the N-th element\r\nbeing the value of applying FUNCTION to the N-th elements of the given\r\nSEQUENCEs, where K is the minimum length of the given SEQUENCEs."))).
*/
/*
:- side_effect(assert_lsp(map,
			  wl:lambda_def(defun, map, cl_map, [sys_result_type, function, sequence, c38_rest, sys_more_sequences], [[let_xx, [[sys_sequences, [list_xx, sequence, sys_more_sequences]], [function, [sys_coerce_to_function, function]], sys_output, sys_it], [when, sys_result_type, [let, [[sys_l, [length, sequence]]], [when, sys_more_sequences, [setf, sys_l, [reduce, function(min), sys_more_sequences, kw_initial_value, sys_l, kw_key, function(length)]]], [setf, sys_output, [make_sequence, sys_result_type, sys_l], sys_it, [sys_make_seq_iterator, sys_output]]]], [sys_do_sequences, [sys_elt_list, sys_sequences, kw_output, sys_output], [let, [[sys_value, [apply, function, sys_elt_list]]], [when, sys_result_type, [sys_seq_iterator_set, sys_output, sys_it, sys_value], [setf, sys_it, [sys_seq_iterator_next, sys_output, sys_it]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(map,
			  wl:arglist_info(map, cl_map, [sys_result_type, function, sequence, c38_rest, sys_more_sequences], arginfo{all:[sys_result_type, function, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_result_type, function, sequence, sys_more_sequences], opt:0, req:[sys_result_type, function, sequence], rest:[sys_more_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(map, wl:init_args(3, cl_map))).
*/
/*
(defun some (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (reckless
   (do-sequences (elt-list (cons sequence more-sequences) :output nil)
     (let ((x (apply predicate elt-list)))
       (when x (return x))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:20707 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,some,[predicate,sequence,'&rest','more-sequences'],'$STRING'("Args: (predicate sequence &rest more-sequences)\r\nReturns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;\r\nNIL otherwise."),[reckless,['do-sequences',['elt-list',[cons,sequence,'more-sequences'],':output',[]],[let,[[x,[apply,predicate,'elt-list']]],[when,x,[return,x]]]]]])
doc: doc_string(some,
	      _164175460,
	      function,
	      "Args: (predicate sequence &rest more-sequences)\r\nReturns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;\r\nNIL otherwise.").

wl:lambda_def(defun, some, cl_some, [predicate, sequence, c38_rest, sys_more_sequences], [[sys_reckless, [sys_do_sequences, [sys_elt_list, [cons, sequence, sys_more_sequences], kw_output, []], [let, [[sys_x, [apply, predicate, sys_elt_list]]], [when, sys_x, [return, sys_x]]]]]]).
wl:arglist_info(some, cl_some, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}).
wl: init_args(2, cl_some).

/*

### Compiled:  `CL:SOME` 
*/
cl_some(Predicate, Sequence, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(predicate, Predicate), bv(sequence, Sequence), bv(sys_more_sequences, RestNKeys)]|Env]|Env],
	catch(( ( get_var(GEnv, sequence, Sequence_Get),
		  get_var(GEnv, sys_more_sequences, More_sequences_Get),
		  Elt_list_Param=[Sequence_Get|More_sequences_Get],
		  f_sys_elt_list(Elt_list_Param,
				 kw_output,
				 [],
				 Do_sequences_Param),
		  get_var(GEnv, predicate, Predicate_Get),
		  get_var(GEnv, sys_elt_list, Elt_list_Get),
		  cl_apply(Predicate_Get, Elt_list_Get, X_Init),
		  LEnv=[bv(sys_x, X_Init)|GEnv],
		  get_var(LEnv, sys_x, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, sys_x, X_Get20),
		      throw(block_exit([], X_Get20)),
		      LetResult=ThrowResult
		  ;   LetResult=[]
		  ),
		  f_sys_do_sequences(Do_sequences_Param,
				     LetResult,
				     Reckless_Param),
		  f_sys_reckless(Reckless_Param, Reckless_Ret)
		),
		Reckless_Ret=FnResult
	      ),
	      block_exit(some, FnResult),
	      true).
:- set_opv(cl_some, classof, claz_function),
   set_opv(some, compile_as, kw_function),
   set_opv(some, function, cl_some),
   DefunResult=some.
/*
:- side_effect(assert_lsp(some,
			  doc:doc_string(some, _164175460, function, "Args: (predicate sequence &rest more-sequences)\r\nReturns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;\r\nNIL otherwise."))).
*/
/*
:- side_effect(assert_lsp(some,
			  wl:lambda_def(defun, some, cl_some, [predicate, sequence, c38_rest, sys_more_sequences], [[sys_reckless, [sys_do_sequences, [sys_elt_list, [cons, sequence, sys_more_sequences], kw_output, []], [let, [[sys_x, [apply, predicate, sys_elt_list]]], [when, sys_x, [return, sys_x]]]]]]))).
*/
/*
:- side_effect(assert_lsp(some,
			  wl:arglist_info(some, cl_some, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(some, wl:init_args(2, cl_some))).
*/
/*
(defun every (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (reckless
   (do-sequences (elt-list (cons sequence more-sequences) :output t)
     (unless (apply predicate elt-list)
       (return nil)))))

#|
(def-seq-bool-parser notany
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (when that-value (return nil))
  t)

(def-seq-bool-parser notevery
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (unless that-value (return t))
  nil)
|#

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:21071 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,every,[predicate,sequence,'&rest','more-sequences'],'$STRING'("Args: (predicate sequence &rest more-sequences)\r\nReturns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."),[reckless,['do-sequences',['elt-list',[cons,sequence,'more-sequences'],':output',t],[unless,[apply,predicate,'elt-list'],[return,[]]]]]])
doc: doc_string(every,
	      _165815428,
	      function,
	      "Args: (predicate sequence &rest more-sequences)\r\nReturns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise.").

wl:lambda_def(defun, every, cl_every, [predicate, sequence, c38_rest, sys_more_sequences], [[sys_reckless, [sys_do_sequences, [sys_elt_list, [cons, sequence, sys_more_sequences], kw_output, t], [unless, [apply, predicate, sys_elt_list], [return, []]]]]]).
wl:arglist_info(every, cl_every, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}).
wl: init_args(2, cl_every).

/*

### Compiled:  `CL:EVERY` 
*/
cl_every(Predicate, Sequence, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	BlockExitEnv=[[[bv(predicate, Predicate), bv(sequence, Sequence), bv(sys_more_sequences, RestNKeys)]|Env]|Env],
	catch(( ( get_var(BlockExitEnv, sequence, Sequence_Get),
		  get_var(BlockExitEnv, sys_more_sequences, More_sequences_Get),
		  Elt_list_Param=[Sequence_Get|More_sequences_Get],
		  f_sys_elt_list(Elt_list_Param, kw_output, t, T),
		  get_var(BlockExitEnv, predicate, Predicate_Get),
		  get_var(BlockExitEnv, sys_elt_list, Elt_list_Get),
		  cl_apply(Predicate_Get, Elt_list_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  _165830636=[]
		  ;   throw(block_exit([], [])),
		      _165830636=ThrowResult
		  ),
		  f_sys_do_sequences(T, _165830636, Reckless_Param),
		  f_sys_reckless(Reckless_Param, Reckless_Ret)
		),
		Reckless_Ret=FnResult
	      ),
	      block_exit(every, FnResult),
	      true).
:- set_opv(cl_every, classof, claz_function),
   set_opv(every, compile_as, kw_function),
   set_opv(every, function, cl_every),
   DefunResult=every.
/*
:- side_effect(assert_lsp(every,
			  doc:doc_string(every, _165815428, function, "Args: (predicate sequence &rest more-sequences)\r\nReturns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."))).
*/
/*
:- side_effect(assert_lsp(every,
			  wl:lambda_def(defun, every, cl_every, [predicate, sequence, c38_rest, sys_more_sequences], [[sys_reckless, [sys_do_sequences, [sys_elt_list, [cons, sequence, sys_more_sequences], kw_output, t], [unless, [apply, predicate, sys_elt_list], [return, []]]]]]))).
*/
/*
:- side_effect(assert_lsp(every,
			  wl:arglist_info(every, cl_every, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(every, wl:init_args(2, cl_every))).
*/
/*

(def-seq-bool-parser notany
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (when that-value (return nil))
  t)

(def-seq-bool-parser notevery
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (unless that-value (return t))
  nil)
*/
/*
(defun every* (predicate &rest sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences
have the same length; NIL otherwise."
  (and (apply #'= (mapcar #'length sequences))
       (apply #'every predicate sequences)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:21852 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'every*',[predicate,'&rest',sequences],'$STRING'("Args: (predicate sequence &rest more-sequences)\r\nReturns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences\r\nhave the same length; NIL otherwise."),[and,[apply,function(=),[mapcar,function(length),sequences]],[apply,function(every),predicate,sequences]]])
doc: doc_string(sys_every_xx,
	      _167280224,
	      function,
	      "Args: (predicate sequence &rest more-sequences)\r\nReturns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences\r\nhave the same length; NIL otherwise.").

wl:lambda_def(defun, sys_every_xx, f_sys_every_xx, [predicate, c38_rest, sys_sequences], [[and, [apply, function(=), [mapcar, function(length), sys_sequences]], [apply, function(every), predicate, sys_sequences]]]).
wl:arglist_info(sys_every_xx, f_sys_every_xx, [predicate, c38_rest, sys_sequences], arginfo{all:[predicate], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sys_sequences], opt:0, req:[predicate], rest:[sys_sequences], sublists:0, whole:0}).
wl: init_args(1, f_sys_every_xx).

/*

### Compiled:  `SYS::EVERY*` 
*/
f_sys_every_xx(Predicate, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(predicate, Predicate), bv(sys_sequences, RestNKeys)]|Env]|Env],
	get_var(GEnv, sys_sequences, Sequences_Get),
	cl_mapcar(cl_length, [Sequences_Get], KeysNRest),
	cl_apply(=, KeysNRest, IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, predicate, Predicate_Get),
	    get_var(GEnv, sys_sequences, Sequences_Get11),
	    cl_apply(cl_every, [Predicate_Get, Sequences_Get11], TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_sys_every_xx, classof, claz_function),
   set_opv(sys_every_xx, compile_as, kw_function),
   set_opv(sys_every_xx, function, f_sys_every_xx),
   DefunResult=sys_every_xx.
/*
:- side_effect(assert_lsp(sys_every_xx,
			  doc:doc_string(sys_every_xx, _167280224, function, "Args: (predicate sequence &rest more-sequences)\r\nReturns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences\r\nhave the same length; NIL otherwise."))).
*/
/*
:- side_effect(assert_lsp(sys_every_xx,
			  wl:lambda_def(defun, sys_every_xx, f_sys_every_xx, [predicate, c38_rest, sys_sequences], [[and, [apply, function(=), [mapcar, function(length), sys_sequences]], [apply, function(every), predicate, sys_sequences]]]))).
*/
/*
:- side_effect(assert_lsp(sys_every_xx,
			  wl:arglist_info(sys_every_xx, f_sys_every_xx, [predicate, c38_rest, sys_sequences], arginfo{all:[predicate], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sys_sequences], opt:0, req:[predicate], rest:[sys_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_every_xx, wl:init_args(1, f_sys_every_xx))).
*/
/*
(defun notany (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (not (apply #'some predicate sequence more-sequences)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:22162 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,notany,[predicate,sequence,'&rest','more-sequences'],'$STRING'("Args: (predicate sequence &rest more-sequences)\r\nReturns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL\r\notherwise."),[not,[apply,function(some),predicate,sequence,'more-sequences']]])
doc: doc_string(notany,
	      _168568416,
	      function,
	      "Args: (predicate sequence &rest more-sequences)\r\nReturns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL\r\notherwise.").

wl:lambda_def(defun, notany, cl_notany, [predicate, sequence, c38_rest, sys_more_sequences], [[not, [apply, function(some), predicate, sequence, sys_more_sequences]]]).
wl:arglist_info(notany, cl_notany, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}).
wl: init_args(2, cl_notany).

/*

### Compiled:  `CL:NOTANY` 
*/
cl_notany(Predicate, Sequence, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(predicate, Predicate), bv(sequence, Sequence), bv(sys_more_sequences, RestNKeys)]|Env]|Env],
	get_var(GEnv, predicate, Predicate_Get),
	get_var(GEnv, sequence, Sequence_Get),
	get_var(GEnv, sys_more_sequences, More_sequences_Get),
	cl_apply(cl_some,
		 [Predicate_Get, Sequence_Get, More_sequences_Get],
		 Not_Param),
	cl_not(Not_Param, Not_Ret),
	Not_Ret=FnResult.
:- set_opv(cl_notany, classof, claz_function),
   set_opv(notany, compile_as, kw_function),
   set_opv(notany, function, cl_notany),
   DefunResult=notany.
/*
:- side_effect(assert_lsp(notany,
			  doc:doc_string(notany, _168568416, function, "Args: (predicate sequence &rest more-sequences)\r\nReturns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL\r\notherwise."))).
*/
/*
:- side_effect(assert_lsp(notany,
			  wl:lambda_def(defun, notany, cl_notany, [predicate, sequence, c38_rest, sys_more_sequences], [[not, [apply, function(some), predicate, sequence, sys_more_sequences]]]))).
*/
/*
:- side_effect(assert_lsp(notany,
			  wl:arglist_info(notany, cl_notany, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(notany, wl:init_args(2, cl_notany))).
*/
/*
(defun notevery (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (not (apply #'every predicate sequence more-sequences)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:22420 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,notevery,[predicate,sequence,'&rest','more-sequences'],'$STRING'("Args: (predicate sequence &rest more-sequences)\r\nReturns T if at least one of the elements in SEQUENCEs does not satisfy\r\nPREDICATE; NIL otherwise."),[not,[apply,function(every),predicate,sequence,'more-sequences']]])
doc: doc_string(notevery,
	      _169760518,
	      function,
	      "Args: (predicate sequence &rest more-sequences)\r\nReturns T if at least one of the elements in SEQUENCEs does not satisfy\r\nPREDICATE; NIL otherwise.").

wl:lambda_def(defun, notevery, cl_notevery, [predicate, sequence, c38_rest, sys_more_sequences], [[not, [apply, function(every), predicate, sequence, sys_more_sequences]]]).
wl:arglist_info(notevery, cl_notevery, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}).
wl: init_args(2, cl_notevery).

/*

### Compiled:  `CL:NOTEVERY` 
*/
cl_notevery(Predicate, Sequence, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(predicate, Predicate), bv(sequence, Sequence), bv(sys_more_sequences, RestNKeys)]|Env]|Env],
	get_var(GEnv, predicate, Predicate_Get),
	get_var(GEnv, sequence, Sequence_Get),
	get_var(GEnv, sys_more_sequences, More_sequences_Get),
	cl_apply(cl_every,
		 [Predicate_Get, Sequence_Get, More_sequences_Get],
		 Not_Param),
	cl_not(Not_Param, Not_Ret),
	Not_Ret=FnResult.
:- set_opv(cl_notevery, classof, claz_function),
   set_opv(notevery, compile_as, kw_function),
   set_opv(notevery, function, cl_notevery),
   DefunResult=notevery.
/*
:- side_effect(assert_lsp(notevery,
			  doc:doc_string(notevery, _169760518, function, "Args: (predicate sequence &rest more-sequences)\r\nReturns T if at least one of the elements in SEQUENCEs does not satisfy\r\nPREDICATE; NIL otherwise."))).
*/
/*
:- side_effect(assert_lsp(notevery,
			  wl:lambda_def(defun, notevery, cl_notevery, [predicate, sequence, c38_rest, sys_more_sequences], [[not, [apply, function(every), predicate, sequence, sys_more_sequences]]]))).
*/
/*
:- side_effect(assert_lsp(notevery,
			  wl:arglist_info(notevery, cl_notevery, [predicate, sequence, c38_rest, sys_more_sequences], arginfo{all:[predicate, sequence], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[predicate, sequence, sys_more_sequences], opt:0, req:[predicate, sequence], rest:[sys_more_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(notevery, wl:init_args(2, cl_notevery))).
*/
/*
(defun map-into (result-sequence function &rest sequences)
"Fills the output sequence with the values returned by applying FUNCTION to the
elements of the given sequences. The i-th element of RESULT-SEQUENCE is the output
of applying FUNCTION to the i-th element of each of the sequences. The map routine
stops when it reaches the end of one of the given sequences."
  (let ((nel (apply #'min (if (vectorp result-sequence)
                              (array-dimension result-sequence 0)
                              (length result-sequence))
                    (mapcar #'length sequences))))
    (declare (fixnum nel))
    ;; Set the fill pointer to the number of iterations
    (when (and (vectorp result-sequence)
               (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) nel))
    ;; Perform mapping
    (do ((ir (make-seq-iterator result-sequence) (seq-iterator-next result-sequence ir))
         (it (mapcar #'make-seq-iterator sequences))
         (val (make-sequence 'list (length sequences))))
        ((null ir) result-sequence)
      (do ((i it (cdr i))
           (v val (cdr v))
           (s sequences (cdr s)))
          ((null i))
        (unless (car i) (return-from map-into result-sequence))
        (rplaca v (seq-iterator-ref (car s) (car i)))
        (rplaca i (seq-iterator-next (car s) (car i))))
      (seq-iterator-set result-sequence ir (apply function val)))))

;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  Copyright (c) 1995, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                        list manipulating routines

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:22694 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'map-into',['result-sequence',function,'&rest',sequences],'$STRING'("Fills the output sequence with the values returned by applying FUNCTION to the\r\nelements of the given sequences. The i-th element of RESULT-SEQUENCE is the output\r\nof applying FUNCTION to the i-th element of each of the sequences. The map routine\r\nstops when it reaches the end of one of the given sequences."),[let,[[nel,[apply,function(min),[if,[vectorp,'result-sequence'],['array-dimension','result-sequence',0],[length,'result-sequence']],[mapcar,function(length),sequences]]]],[declare,[fixnum,nel]],[when,[and,[vectorp,'result-sequence'],['array-has-fill-pointer-p','result-sequence']],[setf,['fill-pointer','result-sequence'],nel]],[do,[[ir,['make-seq-iterator','result-sequence'],['seq-iterator-next','result-sequence',ir]],[it,[mapcar,function('make-seq-iterator'),sequences]],[val,['make-sequence',[quote,list],[length,sequences]]]],[[null,ir],'result-sequence'],[do,[[i,it,[cdr,i]],[v,val,[cdr,v]],[s,sequences,[cdr,s]]],[[null,i]],[unless,[car,i],['return-from','map-into','result-sequence']],[rplaca,v,['seq-iterator-ref',[car,s],[car,i]]],[rplaca,i,['seq-iterator-next',[car,s],[car,i]]]],['seq-iterator-set','result-sequence',ir,[apply,function,val]]]]])
doc: doc_string(map_into,
	      _171091762,
	      function,
	      "Fills the output sequence with the values returned by applying FUNCTION to the\r\nelements of the given sequences. The i-th element of RESULT-SEQUENCE is the output\r\nof applying FUNCTION to the i-th element of each of the sequences. The map routine\r\nstops when it reaches the end of one of the given sequences.").

wl:lambda_def(defun, map_into, cl_map_into, [sys_result_sequence, function, c38_rest, sys_sequences], [[let, [[sys_nel, [apply, function(min), [if, [vectorp, sys_result_sequence], [array_dimension, sys_result_sequence, 0], [length, sys_result_sequence]], [mapcar, function(length), sys_sequences]]]], [declare, [fixnum, sys_nel]], [when, [and, [vectorp, sys_result_sequence], [array_has_fill_pointer_p, sys_result_sequence]], [setf, [fill_pointer, sys_result_sequence], sys_nel]], [do, [[sys_ir, [sys_make_seq_iterator, sys_result_sequence], [sys_seq_iterator_next, sys_result_sequence, sys_ir]], [sys_it, [mapcar, function(sys_make_seq_iterator), sys_sequences]], [sys_val, [make_sequence, [quote, list], [length, sys_sequences]]]], [[null, sys_ir], sys_result_sequence], [do, [[sys_i, sys_it, [cdr, sys_i]], [sys_v, sys_val, [cdr, sys_v]], [sys_s, sys_sequences, [cdr, sys_s]]], [[null, sys_i]], [unless, [car, sys_i], [return_from, map_into, sys_result_sequence]], [rplaca, sys_v, [sys_seq_iterator_ref, [car, sys_s], [car, sys_i]]], [rplaca, sys_i, [sys_seq_iterator_next, [car, sys_s], [car, sys_i]]]], [sys_seq_iterator_set, sys_result_sequence, sys_ir, [apply, function, sys_val]]]]]).
wl:arglist_info(map_into, cl_map_into, [sys_result_sequence, function, c38_rest, sys_sequences], arginfo{all:[sys_result_sequence, function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_result_sequence, function, sys_sequences], opt:0, req:[sys_result_sequence, function], rest:[sys_sequences], sublists:0, whole:0}).
wl: init_args(2, cl_map_into).

/*

### Compiled:  `CL:MAP-INTO` 
*/
cl_map_into(Result_sequence, Function, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_result_sequence, Result_sequence), bv(function, Function), bv(sys_sequences, RestNKeys)]|Env]|Env],
	catch(( ( get_var(GEnv, sys_result_sequence, Result_sequence_Get),
		  (   is_vectorp(Result_sequence_Get)
		  ->  get_var(GEnv, sys_result_sequence, Result_sequence_Get14),
		      cl_array_dimension(Result_sequence_Get14, 0, TrueResult),
		      CAR=TrueResult
		  ;   get_var(GEnv, sys_result_sequence, Result_sequence_Get15),
		      cl_length(Result_sequence_Get15, ElseResult),
		      CAR=ElseResult
		  ),
		  get_var(GEnv, sys_sequences, Sequences_Get),
		  cl_mapcar(cl_length, [Sequences_Get], Mapcar_Ret),
		  cl_apply(cl_min, [CAR, Mapcar_Ret], Nel_Init),
		  LEnv=[bv(sys_nel, Nel_Init)|GEnv],
		  cl_declare([fixnum, sys_nel], Declare_Ret),
		  get_var(LEnv, sys_result_sequence, Result_sequence_Get23),
		  (   is_vectorp(Result_sequence_Get23)
		  ->  get_var(LEnv, sys_result_sequence, Result_sequence_Get26),
		      cl_array_has_fill_pointer_p(Result_sequence_Get26,
						  TrueResult27),
		      IFTEST20=TrueResult27
		  ;   IFTEST20=[]
		  ),
		  (   IFTEST20\==[]
		  ->  get_var(LEnv, sys_nel, Nel_Get),
		      get_var(LEnv, sys_result_sequence, Result_sequence_Get28),
		      f_sys_pf_set_fill_pointer(Result_sequence_Get28,
						Nel_Get,
						TrueResult30),
		      _171134518=TrueResult30
		  ;   _171134518=[]
		  ),
		  get_var(LEnv, sys_result_sequence, Result_sequence_Get34),
		  f_sys_make_seq_iterator(Result_sequence_Get34, [], Ir_Init),
		  get_var(LEnv, sys_sequences, Sequences_Get35),
		  cl_mapcar(f_sys_make_seq_iterator, [Sequences_Get35], It_Init),
		  get_var(LEnv, sys_sequences, Sequences_Get36),
		  cl_length(Sequences_Get36, Length_Ret),
		  cl_make_sequence(list, Length_Ret, [], Val_Init),
		  AEnv=[bv(sys_ir, Ir_Init), bv(sys_it, It_Init), bv(sys_val, Val_Init)|LEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_8), get_var(AEnv, sys_ir, IFTEST123), (IFTEST123==[]->get_var(AEnv, sys_result_sequence, RetResult126), throw(block_exit([], RetResult126)), _TBResult=ThrowResult127;get_var(AEnv, sys_it, It_Get133), get_var(AEnv, sys_sequences, Sequences_Get135), get_var(AEnv, sys_val, Val_Get134), BlockExitEnv=[bv(sys_i, It_Get133), bv(sys_v, Val_Get134), bv(sys_s, Sequences_Get135)|AEnv], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_10), get_var(BlockExitEnv, sys_i, IFTEST168), (IFTEST168==[]->throw(block_exit([], [])), _TBResult139=ThrowResult172;get_var(BlockExitEnv, sys_i, I_Get176), cl_car(I_Get176, IFTEST174), (IFTEST174\==[]->_173372202=[];get_var(BlockExitEnv, sys_result_sequence, RetResult177), throw(block_exit(map_into, RetResult177)), _173372202=ThrowResult178), get_var(BlockExitEnv, sys_s, S_Get182), get_var(BlockExitEnv, sys_v, V_Get181), cl_car(S_Get182, Iterator_ref_Param), get_var(BlockExitEnv, sys_i, I_Get183), cl_car(I_Get183, Car_Ret), f_sys_seq_iterator_ref(Iterator_ref_Param, Car_Ret, Iterator_ref_Ret), cl_rplaca(V_Get181, Iterator_ref_Ret, Rplaca_Ret), get_var(BlockExitEnv, sys_i, I_Get184), get_var(BlockExitEnv, sys_s, S_Get185), cl_car(S_Get185, Iterator_next_Param), get_var(BlockExitEnv, sys_i, I_Get186), cl_car(I_Get186, Car_Ret230), f_sys_seq_iterator_next(Iterator_next_Param, Car_Ret230, Iterator_next_Ret), cl_rplaca(I_Get184, Iterator_next_Ret, Rplaca_Ret232), get_var(BlockExitEnv, sys_i, I_Get187), cl_cdr(I_Get187, I), get_var(BlockExitEnv, sys_v, V_Get188), cl_cdr(V_Get188, V), get_var(BlockExitEnv, sys_s, S_Get189), cl_cdr(S_Get189, S), set_var(BlockExitEnv, sys_i, I), set_var(BlockExitEnv, sys_v, V), set_var(BlockExitEnv, sys_s, S), goto(do_label_10, BlockExitEnv), _TBResult139=_GORES190)), [addr(addr_tagbody_10_do_label_10, do_label_10, '$unused', BlockExitEnv,  (get_var(BlockExitEnv, sys_i, IFTEST140), (IFTEST140==[]->throw(block_exit([], [])), _TBResult139=ThrowResult144;get_var(BlockExitEnv, sys_i, I_Get148), cl_car(I_Get148, IFTEST146), (IFTEST146\==[]->_173680756=[];get_var(BlockExitEnv, sys_result_sequence, RetResult149), throw(block_exit(map_into, RetResult149)), _173680756=ThrowResult150), get_var(BlockExitEnv, sys_s, S_Get155), get_var(BlockExitEnv, sys_v, V_Get154), cl_car(S_Get155, Iterator_ref_Param215), get_var(BlockExitEnv, sys_i, I_Get156), cl_car(I_Get156, Car_Ret233), f_sys_seq_iterator_ref(Iterator_ref_Param215, Car_Ret233, Iterator_ref_Ret234), cl_rplaca(V_Get154, Iterator_ref_Ret234, Rplaca_Ret235), get_var(BlockExitEnv, sys_i, I_Get157), get_var(BlockExitEnv, sys_s, S_Get158), cl_car(S_Get158, Iterator_next_Param216), get_var(BlockExitEnv, sys_i, I_Get159), cl_car(I_Get159, Car_Ret236), f_sys_seq_iterator_next(Iterator_next_Param216, Car_Ret236, Iterator_next_Ret237), cl_rplaca(I_Get157, Iterator_next_Ret237, Rplaca_Ret238), get_var(BlockExitEnv, sys_i, I_Get160), cl_cdr(I_Get160, Cdr_Ret), get_var(BlockExitEnv, sys_v, V_Get161), cl_cdr(V_Get161, Cdr_Ret240), get_var(BlockExitEnv, sys_s, S_Get162), cl_cdr(S_Get162, Cdr_Ret241), set_var(BlockExitEnv, sys_i, Cdr_Ret), set_var(BlockExitEnv, sys_v, Cdr_Ret240), set_var(BlockExitEnv, sys_s, Cdr_Ret241), goto(do_label_10, BlockExitEnv), _TBResult139=_GORES163)))]), []=LetResult131), block_exit([], LetResult131), true), get_var(AEnv, sys_ir, Ir_Get195), get_var(AEnv, sys_result_sequence, Result_sequence_Get194), get_var(AEnv, sys_val, Val_Get196), cl_apply(function, Val_Get196, Apply_Ret), f_sys_seq_iterator_set(Result_sequence_Get194, Ir_Get195, Apply_Ret, Iterator_set_Ret), get_var(AEnv, sys_ir, Ir_Get199), get_var(AEnv, sys_result_sequence, Result_sequence_Get198), f_sys_seq_iterator_next(Result_sequence_Get198, Ir_Get199, Ir), set_var(AEnv, sys_ir, Ir), goto(do_label_8, AEnv), _TBResult=_GORES200)),
					  
					  [ addr(addr_tagbody_8_do_label_8,
						 do_label_8,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_ir, IFTEST41), (IFTEST41==[]->get_var(AEnv, sys_result_sequence, Result_sequence_Get46), throw(block_exit([], Result_sequence_Get46)), _174784870=ThrowResult;get_var(AEnv, sys_it, It_Get), get_var(AEnv, sys_sequences, Sequences_Get53), get_var(AEnv, sys_val, Val_Get), BlockExitEnv=[bv(sys_i, It_Get), bv(sys_v, Val_Get), bv(sys_s, Sequences_Get53)|AEnv], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_9), get_var(BlockExitEnv, sys_i, IFTEST86), (IFTEST86==[]->throw(block_exit([], [])), _TBResult57=ThrowResult90;get_var(BlockExitEnv, sys_i, I_Get94), cl_car(I_Get94, IFTEST92), (IFTEST92\==[]->_174785234=[];get_var(BlockExitEnv, sys_result_sequence, RetResult95), throw(block_exit(map_into, RetResult95)), _174785234=ThrowResult96), get_var(BlockExitEnv, sys_s, S_Get100), get_var(BlockExitEnv, sys_v, V_Get99), cl_car(S_Get100, Iterator_ref_Param217), get_var(BlockExitEnv, sys_i, I_Get101), cl_car(I_Get101, Car_Ret244), f_sys_seq_iterator_ref(Iterator_ref_Param217, Car_Ret244, Iterator_ref_Ret245), cl_rplaca(V_Get99, Iterator_ref_Ret245, Rplaca_Ret246), get_var(BlockExitEnv, sys_i, I_Get102), get_var(BlockExitEnv, sys_s, S_Get103), cl_car(S_Get103, Iterator_next_Param218), get_var(BlockExitEnv, sys_i, I_Get104), cl_car(I_Get104, Car_Ret247), f_sys_seq_iterator_next(Iterator_next_Param218, Car_Ret247, Iterator_next_Ret248), cl_rplaca(I_Get102, Iterator_next_Ret248, Rplaca_Ret249), get_var(BlockExitEnv, sys_i, I_Get105), cl_cdr(I_Get105, Cdr_Ret250), get_var(BlockExitEnv, sys_v, V_Get106), cl_cdr(V_Get106, Cdr_Ret251), get_var(BlockExitEnv, sys_s, S_Get107), cl_cdr(S_Get107, Cdr_Ret252), set_var(BlockExitEnv, sys_i, Cdr_Ret250), set_var(BlockExitEnv, sys_v, Cdr_Ret251), set_var(BlockExitEnv, sys_s, Cdr_Ret252), goto(do_label_9, BlockExitEnv), _TBResult57=_GORES108)), [addr(addr_tagbody_9_do_label_9, do_label_9, '$unused', BlockExitEnv,  (get_var(BlockExitEnv, sys_i, IFTEST58), (IFTEST58==[]->throw(block_exit([], [])), _TBResult57=ThrowResult62;get_var(BlockExitEnv, sys_i, I_Get66), cl_car(I_Get66, IFTEST64), (IFTEST64\==[]->_174785966=[];get_var(BlockExitEnv, sys_result_sequence, RetResult67), throw(block_exit(map_into, RetResult67)), _174785966=ThrowResult68), get_var(BlockExitEnv, sys_s, Car_Param), get_var(BlockExitEnv, sys_v, Rplaca_Param), cl_car(Car_Param, Iterator_ref_Param220), get_var(BlockExitEnv, sys_i, I_Get74), cl_car(I_Get74, Car_Ret253), f_sys_seq_iterator_ref(Iterator_ref_Param220, Car_Ret253, Iterator_ref_Ret254), cl_rplaca(Rplaca_Param, Iterator_ref_Ret254, Rplaca_Ret255), get_var(BlockExitEnv, sys_i, I_Get75), get_var(BlockExitEnv, sys_s, S_Get76), cl_car(S_Get76, Iterator_next_Param222), get_var(BlockExitEnv, sys_i, I_Get77), cl_car(I_Get77, Car_Ret256), f_sys_seq_iterator_next(Iterator_next_Param222, Car_Ret256, Iterator_next_Ret257), cl_rplaca(I_Get75, Iterator_next_Ret257, Rplaca_Ret258), get_var(BlockExitEnv, sys_i, I_Get78), cl_cdr(I_Get78, Cdr_Ret259), get_var(BlockExitEnv, sys_v, V_Get79), cl_cdr(V_Get79, Cdr_Ret260), get_var(BlockExitEnv, sys_s, S_Get80), cl_cdr(S_Get80, Cdr_Ret261), set_var(BlockExitEnv, sys_i, Cdr_Ret259), set_var(BlockExitEnv, sys_v, Cdr_Ret260), set_var(BlockExitEnv, sys_s, Cdr_Ret261), goto(do_label_9, BlockExitEnv), _TBResult57=_GORES)))]), []=LetResult49), block_exit([], LetResult49), true), get_var(AEnv, sys_ir, Ir_Get113), get_var(AEnv, sys_result_sequence, Result_sequence_Get112), get_var(AEnv, sys_val, Val_Get114), cl_apply(function, Val_Get114, Apply_Ret262), f_sys_seq_iterator_set(Result_sequence_Get112, Ir_Get113, Apply_Ret262, Iterator_set_Ret263), get_var(AEnv, sys_ir, Ir_Get117), get_var(AEnv, sys_result_sequence, Result_sequence_Get116), f_sys_seq_iterator_next(Result_sequence_Get116, Ir_Get117, Iterator_next_Ret264), set_var(AEnv, sys_ir, Iterator_next_Ret264), goto(do_label_8, AEnv), _174784870=_GORES118)))
					  ]),
			  []=LetResult32
			),
			block_exit([], LetResult32),
			true)
		),
		LetResult32=FnResult
	      ),
	      block_exit(map_into, FnResult),
	      true).
:- set_opv(cl_map_into, classof, claz_function),
   set_opv(map_into, compile_as, kw_function),
   set_opv(map_into, function, cl_map_into),
   DefunResult=map_into.
/*
:- side_effect(assert_lsp(map_into,
			  doc:doc_string(map_into, _171091762, function, "Fills the output sequence with the values returned by applying FUNCTION to the\r\nelements of the given sequences. The i-th element of RESULT-SEQUENCE is the output\r\nof applying FUNCTION to the i-th element of each of the sequences. The map routine\r\nstops when it reaches the end of one of the given sequences."))).
*/
/*
:- side_effect(assert_lsp(map_into,
			  wl:lambda_def(defun, map_into, cl_map_into, [sys_result_sequence, function, c38_rest, sys_sequences], [[let, [[sys_nel, [apply, function(min), [if, [vectorp, sys_result_sequence], [array_dimension, sys_result_sequence, 0], [length, sys_result_sequence]], [mapcar, function(length), sys_sequences]]]], [declare, [fixnum, sys_nel]], [when, [and, [vectorp, sys_result_sequence], [array_has_fill_pointer_p, sys_result_sequence]], [setf, [fill_pointer, sys_result_sequence], sys_nel]], [do, [[sys_ir, [sys_make_seq_iterator, sys_result_sequence], [sys_seq_iterator_next, sys_result_sequence, sys_ir]], [sys_it, [mapcar, function(sys_make_seq_iterator), sys_sequences]], [sys_val, [make_sequence, [quote, list], [length, sys_sequences]]]], [[null, sys_ir], sys_result_sequence], [do, [[sys_i, sys_it, [cdr, sys_i]], [sys_v, sys_val, [cdr, sys_v]], [sys_s, sys_sequences, [cdr, sys_s]]], [[null, sys_i]], [unless, [car, sys_i], [return_from, map_into, sys_result_sequence]], [rplaca, sys_v, [sys_seq_iterator_ref, [car, sys_s], [car, sys_i]]], [rplaca, sys_i, [sys_seq_iterator_next, [car, sys_s], [car, sys_i]]]], [sys_seq_iterator_set, sys_result_sequence, sys_ir, [apply, function, sys_val]]]]]))).
*/
/*
:- side_effect(assert_lsp(map_into,
			  wl:arglist_info(map_into, cl_map_into, [sys_result_sequence, function, c38_rest, sys_sequences], arginfo{all:[sys_result_sequence, function], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_result_sequence, function, sys_sequences], opt:0, req:[sys_result_sequence, function], rest:[sys_sequences], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(map_into, wl:init_args(2, cl_map_into))).
*/
/*
; Set the fill pointer to the number of iterations
*/
/*
; Perform mapping
*/
/*
;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
*/
/*
;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:
*/
/*
;;;
*/
/*
;;;  Copyright (c) 1995, Giuseppe Attardi.
*/
/*
;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
*/
/*
;;;
*/
/*
;;;    This program is free software; you can redistribute it and/or
*/
/*
;;;    modify it under the terms of the GNU Library General Public
*/
/*
;;;    License as published by the Free Software Foundation; either
*/
/*
;;;    version 2 of the License, or (at your option) any later version.
*/
/*
;;;
*/
/*
;;;    See file '../Copyright' for full details.
*/
/*
;;;                        list manipulating routines
*/
/*
(in-package "SYSTEM")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:24818 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','$STRING'("SYSTEM")])
:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored).
/*
(defun union (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, the union of elements in LIST1 and in LIST2."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (member1 (car x) list2 test test-not key)
      (if last
          (progn (rplacd last (cons (car x) nil))
                 (setq last (cdr last)))
          (progn (setq first (cons (car x) nil))
                 (setq last first))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:24843 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,union,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, the union of elements in LIST1 and in LIST2."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,list2]],[or,first,list2]],[unless,[member1,[car,x],list2,test,'test-not',key],[if,last,[progn,[rplacd,last,[cons,[car,x],[]]],[setq,last,[cdr,last]]],[progn,[setq,first,[cons,[car,x],[]]],[setq,last,first]]]]]])
doc: doc_string(union,
	      _182783770,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, the union of elements in LIST1 and in LIST2.").

wl:lambda_def(defun, union, cl_union, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, sys_list2]], [or, first, sys_list2]], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [progn, [rplacd, last, [cons, [car, sys_x], []]], [setq, last, [cdr, last]]], [progn, [setq, first, [cons, [car, sys_x], []]], [setq, last, first]]]]]]).
wl:arglist_info(union, cl_union, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_union).

/*

### Compiled:  `CL:UNION` 
*/
cl_union(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_11), get_var(AEnv, sys_x, IFTEST56), (IFTEST56==[]->get_var(AEnv, last, IFTEST61), (IFTEST61\==[]->get_var(AEnv, last, Last_Get64), get_var(AEnv, sys_list2, List2_Get65), cl_rplacd(Last_Get64, List2_Get65, TrueResult66), _183371026=TrueResult66;_183371026=[]), (get_var(AEnv, first, First_Get67), First_Get67\==[], RetResult59=First_Get67->true;get_var(AEnv, sys_list2, List2_Get68), RetResult59=List2_Get68), throw(block_exit([], RetResult59)), _TBResult=ThrowResult60;get_var(AEnv, sys_x, X_Get73), cl_car(X_Get73, Member1_Param), get_var(AEnv, key, Key_Get77), get_var(AEnv, sys_list2, List2_Get74), get_var(AEnv, sys_test_not, Test_not_Get76), get_var(AEnv, test, Test_Get75), f_sys_member1(Member1_Param, List2_Get74, Test_Get75, Test_not_Get76, Key_Get77, IFTEST71), (IFTEST71\==[]->ElseResult89=[];get_var(AEnv, last, IFTEST78), (IFTEST78\==[]->get_var(AEnv, last, Last_Get81), get_var(AEnv, sys_x, X_Get82), cl_car(X_Get82, Car_Ret), _183448184=[Car_Ret], cl_rplacd(Last_Get81, _183448184, Rplacd_Ret), get_var(AEnv, last, Last_Get84), cl_cdr(Last_Get84, TrueResult87), set_var(AEnv, last, TrueResult87), ElseResult89=TrueResult87;get_var(AEnv, sys_x, X_Get85), cl_car(X_Get85, Car_Ret109), First=[Car_Ret109], set_var(AEnv, first, First), get_var(AEnv, first, First_Get86), set_var(AEnv, last, First_Get86), ElseResult89=First_Get86)), get_var(AEnv, sys_x, X_Get90), cl_cdr(X_Get90, X), set_var(AEnv, sys_x, X), goto(do_label_11, AEnv), _TBResult=_GORES91)),
					  
					  [ addr(addr_tagbody_11_do_label_11,
						 do_label_11,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST21), (IFTEST21\==[]->get_var(AEnv, last, Last_Get24), get_var(AEnv, sys_list2, Get_var_Ret), cl_rplacd(Last_Get24, Get_var_Ret, Rplacd_Ret111), _183887060=Rplacd_Ret111;_183887060=[]), (get_var(AEnv, first, First_Get), First_Get\==[], Block_exit_Ret=First_Get->true;get_var(AEnv, sys_list2, List2_Get28), Block_exit_Ret=List2_Get28), throw(block_exit([], Block_exit_Ret)), _183887186=ThrowResult;get_var(AEnv, sys_x, X_Get33), cl_car(X_Get33, Member1_Param106), get_var(AEnv, key, Get_var_Ret113), get_var(AEnv, sys_list2, List2_Get34), get_var(AEnv, sys_test_not, Get_var_Ret114), get_var(AEnv, test, Get_var_Ret115), f_sys_member1(Member1_Param106, List2_Get34, Get_var_Ret115, Get_var_Ret114, Get_var_Ret113, IFTEST31), (IFTEST31\==[]->_183887374=[];get_var(AEnv, last, IFTEST38), (IFTEST38\==[]->get_var(AEnv, last, Last_Get41), get_var(AEnv, sys_x, X_Get42), cl_car(X_Get42, Car_Ret116), _183887506=[Car_Ret116], cl_rplacd(Last_Get41, _183887506, Rplacd_Ret117), get_var(AEnv, last, Last_Get44), cl_cdr(Last_Get44, TrueResult47), set_var(AEnv, last, TrueResult47), ElseResult49=TrueResult47;get_var(AEnv, sys_x, X_Get45), cl_car(X_Get45, Car_Ret118), Set_var_Ret=[Car_Ret118], set_var(AEnv, first, Set_var_Ret), get_var(AEnv, first, First_Get46), set_var(AEnv, last, First_Get46), ElseResult49=First_Get46), _183887374=ElseResult49), get_var(AEnv, sys_x, X_Get50), cl_cdr(X_Get50, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_11, AEnv), _183887186=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(union, FnResult),
	      true).
:- set_opv(cl_union, classof, claz_function),
   set_opv(union, compile_as, kw_function),
   set_opv(union, function, cl_union),
   DefunResult=union.
/*
:- side_effect(assert_lsp(union,
			  doc:doc_string(union, _182783770, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, the union of elements in LIST1 and in LIST2."))).
*/
/*
:- side_effect(assert_lsp(union,
			  wl:lambda_def(defun, union, cl_union, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, sys_list2]], [or, first, sys_list2]], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [progn, [rplacd, last, [cons, [car, sys_x], []]], [setq, last, [cdr, last]]], [progn, [setq, first, [cons, [car, sys_x], []]], [setq, last, first]]]]]]))).
*/
/*
:- side_effect(assert_lsp(union,
			  wl:arglist_info(union, cl_union, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(union, wl:init_args(2, cl_union))).
*/
/*
(defun nunion (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive UNION.  Both LIST1 and LIST2 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (member1 (car x) list2 test test-not key)
      (if last
          (rplacd last x)
          (setq first x))
      (setq last x))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:25419 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nunion,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive UNION.  Both LIST1 and LIST2 may be destroyed."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,list2]],[or,first,list2]],[unless,[member1,[car,x],list2,test,'test-not',key],[if,last,[rplacd,last,x],[setq,first,x]],[setq,last,x]]]])
doc: doc_string(nunion,
	      _188159276,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive UNION.  Both LIST1 and LIST2 may be destroyed.").

wl:lambda_def(defun, nunion, cl_nunion, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, sys_list2]], [or, first, sys_list2]], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]).
wl:arglist_info(nunion, cl_nunion, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_nunion).

/*

### Compiled:  `CL:NUNION` 
*/
cl_nunion(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_12), get_var(AEnv, sys_x, IFTEST55), (IFTEST55==[]->get_var(AEnv, last, IFTEST60), (IFTEST60\==[]->get_var(AEnv, last, Last_Get63), get_var(AEnv, sys_list2, List2_Get64), cl_rplacd(Last_Get63, List2_Get64, TrueResult65), _188611252=TrueResult65;_188611252=[]), (get_var(AEnv, first, First_Get66), First_Get66\==[], RetResult58=First_Get66->true;get_var(AEnv, sys_list2, List2_Get67), RetResult58=List2_Get67), throw(block_exit([], RetResult58)), _TBResult=ThrowResult59;get_var(AEnv, sys_x, X_Get72), cl_car(X_Get72, Member1_Param), get_var(AEnv, key, Key_Get76), get_var(AEnv, sys_list2, List2_Get73), get_var(AEnv, sys_test_not, Test_not_Get75), get_var(AEnv, test, Test_Get74), f_sys_member1(Member1_Param, List2_Get73, Test_Get74, Test_not_Get75, Key_Get76, IFTEST70), (IFTEST70\==[]->_188669168=[];get_var(AEnv, last, IFTEST77), (IFTEST77\==[]->get_var(AEnv, last, Last_Get80), get_var(AEnv, sys_x, X_Get81), cl_rplacd(Last_Get80, X_Get81, TrueResult84), _188680564=TrueResult84;get_var(AEnv, sys_x, X_Get83), set_var(AEnv, first, X_Get83), _188680564=X_Get83), get_var(AEnv, sys_x, X_Get86), set_var(AEnv, last, X_Get86), _188669168=X_Get86), get_var(AEnv, sys_x, X_Get88), cl_cdr(X_Get88, X), set_var(AEnv, sys_x, X), goto(do_label_12, AEnv), _TBResult=_GORES89)),
					  
					  [ addr(addr_tagbody_12_do_label_12,
						 do_label_12,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST21), (IFTEST21\==[]->get_var(AEnv, last, Last_Get24), get_var(AEnv, sys_list2, Get_var_Ret), cl_rplacd(Last_Get24, Get_var_Ret, Rplacd_Ret), _188996246=Rplacd_Ret;_188996246=[]), (get_var(AEnv, first, First_Get), First_Get\==[], Block_exit_Ret=First_Get->true;get_var(AEnv, sys_list2, List2_Get28), Block_exit_Ret=List2_Get28), throw(block_exit([], Block_exit_Ret)), _188996372=ThrowResult;get_var(AEnv, sys_x, X_Get33), cl_car(X_Get33, Member1_Param103), get_var(AEnv, key, Get_var_Ret107), get_var(AEnv, sys_list2, List2_Get34), get_var(AEnv, sys_test_not, Get_var_Ret108), get_var(AEnv, test, Get_var_Ret109), f_sys_member1(Member1_Param103, List2_Get34, Get_var_Ret109, Get_var_Ret108, Get_var_Ret107, IFTEST31), (IFTEST31\==[]->_188996560=[];get_var(AEnv, last, IFTEST38), (IFTEST38\==[]->get_var(AEnv, last, Last_Get41), get_var(AEnv, sys_x, X_Get42), cl_rplacd(Last_Get41, X_Get42, TrueResult45), _188996702=TrueResult45;get_var(AEnv, sys_x, X_Get44), set_var(AEnv, first, X_Get44), _188996702=X_Get44), get_var(AEnv, sys_x, X_Get47), set_var(AEnv, last, X_Get47), _188996560=X_Get47), get_var(AEnv, sys_x, X_Get49), cl_cdr(X_Get49, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_12, AEnv), _188996372=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(nunion, FnResult),
	      true).
:- set_opv(cl_nunion, classof, claz_function),
   set_opv(nunion, compile_as, kw_function),
   set_opv(nunion, function, cl_nunion),
   DefunResult=nunion.
/*
:- side_effect(assert_lsp(nunion,
			  doc:doc_string(nunion, _188159276, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive UNION.  Both LIST1 and LIST2 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nunion,
			  wl:lambda_def(defun, nunion, cl_nunion, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, sys_list2]], [or, first, sys_list2]], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]))).
*/
/*
:- side_effect(assert_lsp(nunion,
			  wl:arglist_info(nunion, cl_nunion, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(nunion, wl:init_args(2, cl_nunion))).
*/
/*
(defun intersection (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns a list consisting of those objects that are elements of both LIST1 and
LIST2."
  (do ((x list1 (cdr x))
       (ans))
      ((null x)
       (nreverse ans)) ; optional nreverse: not required by CLtL
    (when (member1 (car x) list2 test test-not key)
        (push (car x) ans))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:25884 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,intersection,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns a list consisting of those objects that are elements of both LIST1 and\r\nLIST2."),[do,[[x,list1,[cdr,x]],[ans]],[[null,x],[nreverse,ans]],[when,[member1,[car,x],list2,test,'test-not',key],[push,[car,x],ans]]]])
doc: doc_string(intersection,
	      _192661302,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns a list consisting of those objects that are elements of both LIST1 and\r\nLIST2.").

wl:lambda_def(defun, intersection, cl_intersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [sys_ans]], [[null, sys_x], [nreverse, sys_ans]], [when, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [push, [car, sys_x], sys_ans]]]]).
wl:arglist_info(intersection, cl_intersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_intersection).

/*

### Compiled:  `CL:INTERSECTION` 
*/
cl_intersection(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([sys_ans], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_13), get_var(AEnv, sys_x, IFTEST42), (IFTEST42==[]->get_var(AEnv, sys_ans, Ans_Get47), cl_nreverse(Ans_Get47, RetResult45), throw(block_exit([], RetResult45)), _TBResult=ThrowResult46;get_var(AEnv, sys_x, X_Get51), cl_car(X_Get51, Member1_Param), get_var(AEnv, key, Key_Get55), get_var(AEnv, sys_list2, List2_Get52), get_var(AEnv, sys_test_not, Test_not_Get54), get_var(AEnv, test, Test_Get53), f_sys_member1(Member1_Param, List2_Get52, Test_Get53, Test_not_Get54, Key_Get55, IFTEST49), (IFTEST49\==[]->get_var(AEnv, sys_ans, Ans_Get56), get_var(AEnv, sys_x, X_Get59), set_place(AEnv, push, [car, X_Get59], [Ans_Get56], Push_R57), _192958356=Push_R57;_192958356=[]), get_var(AEnv, sys_x, X_Get62), cl_cdr(X_Get62, X), set_var(AEnv, sys_x, X), goto(do_label_13, AEnv), _TBResult=_GORES63)),
					  
					  [ addr(addr_tagbody_13_do_label_13,
						 do_label_13,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, sys_ans, Nreverse_Param), cl_nreverse(Nreverse_Param, Nreverse_Ret), throw(block_exit([], Nreverse_Ret)), _193175474=ThrowResult;get_var(AEnv, sys_x, X_Get25), cl_car(X_Get25, Member1_Param78), get_var(AEnv, key, Get_var_Ret), get_var(AEnv, sys_list2, Get_var_Ret81), get_var(AEnv, sys_test_not, Get_var_Ret82), get_var(AEnv, test, Get_var_Ret83), f_sys_member1(Member1_Param78, Get_var_Ret81, Get_var_Ret83, Get_var_Ret82, Get_var_Ret, IFTEST23), (IFTEST23\==[]->get_var(AEnv, sys_ans, Ans_Get30), get_var(AEnv, sys_x, X_Get33), set_place(AEnv, push, [car, X_Get33], [Ans_Get30], Push_R), _193175754=Push_R;_193175754=[]), get_var(AEnv, sys_x, X_Get36), cl_cdr(X_Get36, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_13, AEnv), _193175474=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(intersection, FnResult),
	      true).
:- set_opv(cl_intersection, classof, claz_function),
   set_opv(intersection, compile_as, kw_function),
   set_opv(intersection, function, cl_intersection),
   DefunResult=intersection.
/*
:- side_effect(assert_lsp(intersection,
			  doc:doc_string(intersection, _192661302, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns a list consisting of those objects that are elements of both LIST1 and\r\nLIST2."))).
*/
/*
:- side_effect(assert_lsp(intersection,
			  wl:lambda_def(defun, intersection, cl_intersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [sys_ans]], [[null, sys_x], [nreverse, sys_ans]], [when, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [push, [car, sys_x], sys_ans]]]]))).
*/
/*
:- side_effect(assert_lsp(intersection,
			  wl:arglist_info(intersection, cl_intersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(intersection, wl:init_args(2, cl_intersection))).
*/
/*
 optional nreverse: not required by CLtL
*/
/*
(defun nintersection (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive INTERSECTION.  Only LIST1 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (when (member1 (car x) list2 test test-not key)
      (if last
          (rplacd last x)
          (setq first x))
      (setq last x))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:26309 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nintersection,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive INTERSECTION.  Only LIST1 may be destroyed."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,[]]],first],[when,[member1,[car,x],list2,test,'test-not',key],[if,last,[rplacd,last,x],[setq,first,x]],[setq,last,x]]]])
doc: doc_string(nintersection,
	      _195963852,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive INTERSECTION.  Only LIST1 may be destroyed.").

wl:lambda_def(defun, nintersection, cl_nintersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, []]], first], [when, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]).
wl:arglist_info(nintersection, cl_nintersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_nintersection).

/*

### Compiled:  `CL:NINTERSECTION` 
*/
cl_nintersection(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_14), get_var(AEnv, sys_x, IFTEST52), (IFTEST52==[]->get_var(AEnv, last, IFTEST57), (IFTEST57\==[]->get_var(AEnv, last, Last_Get60), cl_rplacd(Last_Get60, [], TrueResult61), _196381328=TrueResult61;_196381328=[]), get_var(AEnv, first, RetResult55), throw(block_exit([], RetResult55)), _TBResult=ThrowResult56;get_var(AEnv, sys_x, X_Get66), cl_car(X_Get66, Member1_Param), get_var(AEnv, key, Key_Get70), get_var(AEnv, sys_list2, List2_Get67), get_var(AEnv, sys_test_not, Test_not_Get69), get_var(AEnv, test, Test_Get68), f_sys_member1(Member1_Param, List2_Get67, Test_Get68, Test_not_Get69, Key_Get70, IFTEST64), (IFTEST64\==[]->get_var(AEnv, last, IFTEST71), (IFTEST71\==[]->get_var(AEnv, last, Last_Get74), get_var(AEnv, sys_x, X_Get75), cl_rplacd(Last_Get74, X_Get75, TrueResult78), _196430482=TrueResult78;get_var(AEnv, sys_x, X_Get77), set_var(AEnv, first, X_Get77), _196430482=X_Get77), get_var(AEnv, sys_x, X_Get80), set_var(AEnv, last, X_Get80), _196419222=X_Get80;_196419222=[]), get_var(AEnv, sys_x, X_Get82), cl_cdr(X_Get82, X), set_var(AEnv, sys_x, X), goto(do_label_14, AEnv), _TBResult=_GORES83)),
					  
					  [ addr(addr_tagbody_14_do_label_14,
						 do_label_14,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST21), (IFTEST21\==[]->get_var(AEnv, last, Last_Get24), cl_rplacd(Last_Get24, [], Rplacd_Ret), _196732918=Rplacd_Ret;_196732918=[]), get_var(AEnv, first, Get_var_Ret), throw(block_exit([], Get_var_Ret)), _196732960=ThrowResult;get_var(AEnv, sys_x, X_Get30), cl_car(X_Get30, Member1_Param97), get_var(AEnv, key, Get_var_Ret100), get_var(AEnv, sys_list2, Get_var_Ret101), get_var(AEnv, sys_test_not, Get_var_Ret102), get_var(AEnv, test, Get_var_Ret103), f_sys_member1(Member1_Param97, Get_var_Ret101, Get_var_Ret103, Get_var_Ret102, Get_var_Ret100, IFTEST28), (IFTEST28\==[]->get_var(AEnv, last, IFTEST35), (IFTEST35\==[]->get_var(AEnv, last, Last_Get38), get_var(AEnv, sys_x, X_Get39), cl_rplacd(Last_Get38, X_Get39, TrueResult42), _196733270=TrueResult42;get_var(AEnv, sys_x, X_Get41), set_var(AEnv, first, X_Get41), _196733270=X_Get41), get_var(AEnv, sys_x, X_Get44), set_var(AEnv, last, X_Get44), _196733366=X_Get44;_196733366=[]), get_var(AEnv, sys_x, X_Get46), cl_cdr(X_Get46, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_14, AEnv), _196732960=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(nintersection, FnResult),
	      true).
:- set_opv(cl_nintersection, classof, claz_function),
   set_opv(nintersection, compile_as, kw_function),
   set_opv(nintersection, function, cl_nintersection),
   DefunResult=nintersection.
/*
:- side_effect(assert_lsp(nintersection,
			  doc:doc_string(nintersection, _195963852, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive INTERSECTION.  Only LIST1 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nintersection,
			  wl:lambda_def(defun, nintersection, cl_nintersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, []]], first], [when, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]))).
*/
/*
:- side_effect(assert_lsp(nintersection,
			  wl:arglist_info(nintersection, cl_nintersection, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(nintersection, wl:init_args(2, cl_nintersection))).
*/
/*
(defun set-difference (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2."
  (do ((x list1 (cdr x))
       (ans))
      ((null x) (nreverse ans))
    (unless (member1 (car x) list2 test test-not key)
      (push (car x) ans))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:26763 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'set-difference',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2."),[do,[[x,list1,[cdr,x]],[ans]],[[null,x],[nreverse,ans]],[unless,[member1,[car,x],list2,test,'test-not',key],[push,[car,x],ans]]]])
doc: doc_string(set_difference,
	      _200124000,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2.").

wl:lambda_def(defun, set_difference, cl_set_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [sys_ans]], [[null, sys_x], [nreverse, sys_ans]], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [push, [car, sys_x], sys_ans]]]]).
wl:arglist_info(set_difference, cl_set_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_set_difference).

/*

### Compiled:  `CL:SET-DIFFERENCE` 
*/
cl_set_difference(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([sys_ans], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_15), get_var(AEnv, sys_x, IFTEST42), (IFTEST42==[]->get_var(AEnv, sys_ans, Ans_Get47), cl_nreverse(Ans_Get47, RetResult45), throw(block_exit([], RetResult45)), _TBResult=ThrowResult46;get_var(AEnv, sys_x, X_Get51), cl_car(X_Get51, Member1_Param), get_var(AEnv, key, Key_Get55), get_var(AEnv, sys_list2, List2_Get52), get_var(AEnv, sys_test_not, Test_not_Get54), get_var(AEnv, test, Test_Get53), f_sys_member1(Member1_Param, List2_Get52, Test_Get53, Test_not_Get54, Key_Get55, IFTEST49), (IFTEST49\==[]->_200421054=[];get_var(AEnv, sys_ans, Ans_Get56), get_var(AEnv, sys_x, X_Get59), set_place(AEnv, push, [car, X_Get59], [Ans_Get56], Push_R57), _200421054=Push_R57), get_var(AEnv, sys_x, X_Get62), cl_cdr(X_Get62, X), set_var(AEnv, sys_x, X), goto(do_label_15, AEnv), _TBResult=_GORES63)),
					  
					  [ addr(addr_tagbody_15_do_label_15,
						 do_label_15,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, sys_ans, Nreverse_Param), cl_nreverse(Nreverse_Param, Nreverse_Ret), throw(block_exit([], Nreverse_Ret)), _200638172=ThrowResult;get_var(AEnv, sys_x, X_Get25), cl_car(X_Get25, Member1_Param78), get_var(AEnv, key, Get_var_Ret), get_var(AEnv, sys_list2, Get_var_Ret81), get_var(AEnv, sys_test_not, Get_var_Ret82), get_var(AEnv, test, Get_var_Ret83), f_sys_member1(Member1_Param78, Get_var_Ret81, Get_var_Ret83, Get_var_Ret82, Get_var_Ret, IFTEST23), (IFTEST23\==[]->_200638346=[];get_var(AEnv, sys_ans, Ans_Get30), get_var(AEnv, sys_x, X_Get33), set_place(AEnv, push, [car, X_Get33], [Ans_Get30], Push_R), _200638346=Push_R), get_var(AEnv, sys_x, X_Get36), cl_cdr(X_Get36, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_15, AEnv), _200638172=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(set_difference, FnResult),
	      true).
:- set_opv(cl_set_difference, classof, claz_function),
   set_opv(set_difference, compile_as, kw_function),
   set_opv(set_difference, function, cl_set_difference),
   DefunResult=set_difference.
/*
:- side_effect(assert_lsp(set_difference,
			  doc:doc_string(set_difference, _200124000, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2."))).
*/
/*
:- side_effect(assert_lsp(set_difference,
			  wl:lambda_def(defun, set_difference, cl_set_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [sys_ans]], [[null, sys_x], [nreverse, sys_ans]], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [push, [car, sys_x], sys_ans]]]]))).
*/
/*
:- side_effect(assert_lsp(set_difference,
			  wl:arglist_info(set_difference, cl_set_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(set_difference, wl:init_args(2, cl_set_difference))).
*/
/*
(defun nset-difference (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive SET-DIFFERENCE.  Only LIST1 may be destroyed."
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (unless (member1 (car x) list2 test test-not key)
      (if last
          (rplacd last x)
          (setq first x))
      (setq last x))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:27129 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nset-difference',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-DIFFERENCE.  Only LIST1 may be destroyed."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,[]]],first],[unless,[member1,[car,x],list2,test,'test-not',key],[if,last,[rplacd,last,x],[setq,first,x]],[setq,last,x]]]])
doc: doc_string(nset_difference,
	      _203414930,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-DIFFERENCE.  Only LIST1 may be destroyed.").

wl:lambda_def(defun, nset_difference, cl_nset_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, []]], first], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]).
wl:arglist_info(nset_difference, cl_nset_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_nset_difference).

/*

### Compiled:  `CL:NSET-DIFFERENCE` 
*/
cl_nset_difference(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_16), get_var(AEnv, sys_x, IFTEST52), (IFTEST52==[]->get_var(AEnv, last, IFTEST57), (IFTEST57\==[]->get_var(AEnv, last, Last_Get60), cl_rplacd(Last_Get60, [], TrueResult61), _203832406=TrueResult61;_203832406=[]), get_var(AEnv, first, RetResult55), throw(block_exit([], RetResult55)), _TBResult=ThrowResult56;get_var(AEnv, sys_x, X_Get66), cl_car(X_Get66, Member1_Param), get_var(AEnv, key, Key_Get70), get_var(AEnv, sys_list2, List2_Get67), get_var(AEnv, sys_test_not, Test_not_Get69), get_var(AEnv, test, Test_Get68), f_sys_member1(Member1_Param, List2_Get67, Test_Get68, Test_not_Get69, Key_Get70, IFTEST64), (IFTEST64\==[]->_203870300=[];get_var(AEnv, last, IFTEST71), (IFTEST71\==[]->get_var(AEnv, last, Last_Get74), get_var(AEnv, sys_x, X_Get75), cl_rplacd(Last_Get74, X_Get75, TrueResult78), _203881672=TrueResult78;get_var(AEnv, sys_x, X_Get77), set_var(AEnv, first, X_Get77), _203881672=X_Get77), get_var(AEnv, sys_x, X_Get80), set_var(AEnv, last, X_Get80), _203870300=X_Get80), get_var(AEnv, sys_x, X_Get82), cl_cdr(X_Get82, X), set_var(AEnv, sys_x, X), goto(do_label_16, AEnv), _TBResult=_GORES83)),
					  
					  [ addr(addr_tagbody_16_do_label_16,
						 do_label_16,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST21), (IFTEST21\==[]->get_var(AEnv, last, Last_Get24), cl_rplacd(Last_Get24, [], Rplacd_Ret), _204183996=Rplacd_Ret;_204183996=[]), get_var(AEnv, first, Get_var_Ret), throw(block_exit([], Get_var_Ret)), _204184038=ThrowResult;get_var(AEnv, sys_x, X_Get30), cl_car(X_Get30, Member1_Param97), get_var(AEnv, key, Get_var_Ret100), get_var(AEnv, sys_list2, Get_var_Ret101), get_var(AEnv, sys_test_not, Get_var_Ret102), get_var(AEnv, test, Get_var_Ret103), f_sys_member1(Member1_Param97, Get_var_Ret101, Get_var_Ret103, Get_var_Ret102, Get_var_Ret100, IFTEST28), (IFTEST28\==[]->_204184212=[];get_var(AEnv, last, IFTEST35), (IFTEST35\==[]->get_var(AEnv, last, Last_Get38), get_var(AEnv, sys_x, X_Get39), cl_rplacd(Last_Get38, X_Get39, TrueResult42), _204184354=TrueResult42;get_var(AEnv, sys_x, X_Get41), set_var(AEnv, first, X_Get41), _204184354=X_Get41), get_var(AEnv, sys_x, X_Get44), set_var(AEnv, last, X_Get44), _204184212=X_Get44), get_var(AEnv, sys_x, X_Get46), cl_cdr(X_Get46, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_16, AEnv), _204184038=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(nset_difference, FnResult),
	      true).
:- set_opv(cl_nset_difference, classof, claz_function),
   set_opv(nset_difference, compile_as, kw_function),
   set_opv(nset_difference, function, cl_nset_difference),
   DefunResult=nset_difference.
/*
:- side_effect(assert_lsp(nset_difference,
			  doc:doc_string(nset_difference, _203414930, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-DIFFERENCE.  Only LIST1 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nset_difference,
			  wl:lambda_def(defun, nset_difference, cl_nset_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, []]], first], [unless, [sys_member1, [car, sys_x], sys_list2, test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]))).
*/
/*
:- side_effect(assert_lsp(nset_difference,
			  wl:arglist_info(nset_difference, cl_nset_difference, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(nset_difference, wl:init_args(2, cl_nset_difference))).
*/
/*
(defun swap-args (f)
  ; (declare (c-local))
  (and f #'(lambda (x y) (funcall f y x))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:27589 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'swap-args',[f],[and,f,function([lambda,[x,y],[funcall,f,y,x]])]])
wl:lambda_def(defun, sys_swap_args, f_sys_swap_args, [sys_f], [[and, sys_f, function([lambda, [sys_x, sys_y], [funcall, sys_f, sys_y, sys_x]])]]).
wl:arglist_info(sys_swap_args, f_sys_swap_args, [sys_f], arginfo{all:[sys_f], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_f], opt:0, req:[sys_f], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_swap_args).

/*

### Compiled:  `SYS::SWAP-ARGS` 
*/
f_sys_swap_args(F, FnResult) :-
	nop(global_env(Env)),
	Env17=[bv(sys_f, F)|Env],
	get_var(Env17, sys_f, IFTEST),
	(   IFTEST\==[]
	->  FnResult=closure(LEnv, LResult, [sys_x, sys_y],  (get_var(LEnv, sys_f, Get9), get_var(LEnv, sys_x, X_Get), get_var(LEnv, sys_y, Y_Get), cl_apply(Get9, [Y_Get, X_Get], LResult)))
	;   FnResult=[]
	).
:- set_opv(f_sys_swap_args, classof, claz_function),
   set_opv(sys_swap_args, compile_as, kw_function),
   set_opv(sys_swap_args, function, f_sys_swap_args),
   DefunResult=sys_swap_args.
/*
:- side_effect(assert_lsp(sys_swap_args,
			  wl:lambda_def(defun, sys_swap_args, f_sys_swap_args, [sys_f], [[and, sys_f, function([lambda, [sys_x, sys_y], [funcall, sys_f, sys_y, sys_x]])]]))).
*/
/*
:- side_effect(assert_lsp(sys_swap_args,
			  wl:arglist_info(sys_swap_args, f_sys_swap_args, [sys_f], arginfo{all:[sys_f], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_f], opt:0, req:[sys_f], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_swap_args,
			  wl:init_args(exact_only, f_sys_swap_args))).
*/
/*
 (declare (c-local))
*/
/*
(defun set-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2 and
those elements of LIST2 that are not elements of LIST1."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
         (set-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:27683 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'set-exclusive-or',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2 and\r\nthose elements of LIST2 that are not elements of LIST1."),[nconc,['set-difference',list1,list2,':test',test,':test-not','test-not',':key',key],['set-difference',list2,list1,':test',['swap-args',test],':test-not',['swap-args','test-not'],':key',key]]])
doc: doc_string(set_exclusive_or,
	      _208713046,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2 and\r\nthose elements of LIST2 that are not elements of LIST1.").

wl:lambda_def(defun, set_exclusive_or, cl_set_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[nconc, [set_difference, sys_list1, sys_list2, kw_test, test, kw_test_not, sys_test_not, kw_key, key], [set_difference, sys_list2, sys_list1, kw_test, [sys_swap_args, test], kw_test_not, [sys_swap_args, sys_test_not], kw_key, key]]]).
wl:arglist_info(set_exclusive_or, cl_set_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_set_exclusive_or).

/*

### Compiled:  `CL:SET-EXCLUSIVE-OR` 
*/
cl_set_exclusive_or(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_list1, List1_Get),
	get_var(GEnv, sys_list2, List2_Get),
	get_var(GEnv, sys_test_not, Test_not_Get),
	get_var(GEnv, test, Test_Get),
	cl_set_difference(List1_Get,
			  List2_Get,
			  
			  [ kw_test,
			    Test_Get,
			    kw_test_not,
			    Test_not_Get,
			    kw_key,
			    Key_Get
			  ],
			  Set_difference_Ret),
	get_var(GEnv, sys_list1, List1_Get16),
	get_var(GEnv, sys_list2, List2_Get15),
	get_var(GEnv, test, Test_Get17),
	f_sys_swap_args(Test_Get17, Swap_args_Ret),
	get_var(GEnv, sys_test_not, Test_not_Get18),
	f_sys_swap_args(Test_not_Get18, Swap_args_Ret30),
	get_var(GEnv, key, Key_Get19),
	cl_set_difference(List2_Get15,
			  List1_Get16,
			  
			  [ kw_test,
			    Swap_args_Ret,
			    kw_test_not,
			    Swap_args_Ret30,
			    kw_key,
			    Key_Get19
			  ],
			  Set_difference_Ret31),
	cl_nconc([Set_difference_Ret, Set_difference_Ret31], Nconc_Ret),
	Nconc_Ret=FnResult.
:- set_opv(cl_set_exclusive_or, classof, claz_function),
   set_opv(set_exclusive_or, compile_as, kw_function),
   set_opv(set_exclusive_or, function, cl_set_exclusive_or),
   DefunResult=set_exclusive_or.
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  doc:doc_string(set_exclusive_or, _208713046, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2 and\r\nthose elements of LIST2 that are not elements of LIST1."))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  wl:lambda_def(defun, set_exclusive_or, cl_set_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[nconc, [set_difference, sys_list1, sys_list2, kw_test, test, kw_test_not, sys_test_not, kw_key, key], [set_difference, sys_list2, sys_list1, kw_test, [sys_swap_args, test], kw_test_not, [sys_swap_args, sys_test_not], kw_key, key]]]))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  wl:arglist_info(set_exclusive_or, cl_set_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  wl:init_args(2, cl_set_exclusive_or))).
*/
/*
(defun nset-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
         (nset-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:28135 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nset-exclusive-or',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."),[nconc,['set-difference',list1,list2,':test',test,':test-not','test-not',':key',key],['nset-difference',list2,list1,':test',['swap-args',test],':test-not',['swap-args','test-not'],':key',key]]])
doc: doc_string(nset_exclusive_or,
	      _210450166,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed.").

wl:lambda_def(defun, nset_exclusive_or, cl_nset_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[nconc, [set_difference, sys_list1, sys_list2, kw_test, test, kw_test_not, sys_test_not, kw_key, key], [nset_difference, sys_list2, sys_list1, kw_test, [sys_swap_args, test], kw_test_not, [sys_swap_args, sys_test_not], kw_key, key]]]).
wl:arglist_info(nset_exclusive_or, cl_nset_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_nset_exclusive_or).

/*

### Compiled:  `CL:NSET-EXCLUSIVE-OR` 
*/
cl_nset_exclusive_or(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_list1, List1_Get),
	get_var(GEnv, sys_list2, List2_Get),
	get_var(GEnv, sys_test_not, Test_not_Get),
	get_var(GEnv, test, Test_Get),
	cl_set_difference(List1_Get,
			  List2_Get,
			  
			  [ kw_test,
			    Test_Get,
			    kw_test_not,
			    Test_not_Get,
			    kw_key,
			    Key_Get
			  ],
			  Set_difference_Ret),
	get_var(GEnv, sys_list1, List1_Get16),
	get_var(GEnv, sys_list2, List2_Get15),
	get_var(GEnv, test, Test_Get17),
	f_sys_swap_args(Test_Get17, Swap_args_Ret),
	get_var(GEnv, sys_test_not, Test_not_Get18),
	f_sys_swap_args(Test_not_Get18, Swap_args_Ret30),
	get_var(GEnv, key, Key_Get19),
	cl_nset_difference(List2_Get15,
			   List1_Get16,
			   
			   [ kw_test,
			     Swap_args_Ret,
			     kw_test_not,
			     Swap_args_Ret30,
			     kw_key,
			     Key_Get19
			   ],
			   Nset_difference_Ret),
	cl_nconc([Set_difference_Ret, Nset_difference_Ret], Nconc_Ret),
	Nconc_Ret=FnResult.
:- set_opv(cl_nset_exclusive_or, classof, claz_function),
   set_opv(nset_exclusive_or, compile_as, kw_function),
   set_opv(nset_exclusive_or, function, cl_nset_exclusive_or),
   DefunResult=nset_exclusive_or.
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  doc:doc_string(nset_exclusive_or, _210450166, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  wl:lambda_def(defun, nset_exclusive_or, cl_nset_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[nconc, [set_difference, sys_list1, sys_list2, kw_test, test, kw_test_not, sys_test_not, kw_key, key], [nset_difference, sys_list2, sys_list1, kw_test, [sys_swap_args, test], kw_test_not, [sys_swap_args, sys_test_not], kw_key, key]]]))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  wl:arglist_info(nset_exclusive_or, cl_nset_exclusive_or, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  wl:init_args(2, cl_nset_exclusive_or))).
*/
/*
(defun subsetp (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns T if every element of LIST1 is also an element of LIST2.  Returns NIL
otherwise."
  (do ((l list1 (cdr l)))
      ((null l) t)
    (unless (member1 (car l) list2 test test-not key)
      (return nil))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:28523 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,subsetp,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns T if every element of LIST1 is also an element of LIST2.  Returns NIL\r\notherwise."),[do,[[l,list1,[cdr,l]]],[[null,l],t],[unless,[member1,[car,l],list2,test,'test-not',key],[return,[]]]]])
doc: doc_string(subsetp,
	      _212183578,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns T if every element of LIST1 is also an element of LIST2.  Returns NIL\r\notherwise.").

wl:lambda_def(defun, subsetp, cl_subsetp, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_l, sys_list1, [cdr, sys_l]]], [[null, sys_l], t], [unless, [sys_member1, [car, sys_l], sys_list2, test, sys_test_not, key], [return, []]]]]).
wl:arglist_info(subsetp, cl_subsetp, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_subsetp).

/*

### Compiled:  `CL:SUBSETP` 
*/
cl_subsetp(List1, List2, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_list1, List1), bv(sys_list2, List2), bv(test, Test), bv(sys_test_not, Test_not), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, test, test, Test, []=Test, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not,
	       []=Test_not,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_l, List1_Get)|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_17), get_var(AEnv, sys_l, IFTEST39), (IFTEST39==[]->throw(block_exit([], t)), _TBResult=ThrowResult43;get_var(AEnv, sys_l, L_Get47), cl_car(L_Get47, Member1_Param), get_var(AEnv, key, Key_Get51), get_var(AEnv, sys_list2, List2_Get48), get_var(AEnv, sys_test_not, Test_not_Get50), get_var(AEnv, test, Test_Get49), f_sys_member1(Member1_Param, List2_Get48, Test_Get49, Test_not_Get50, Key_Get51, IFTEST45), (IFTEST45\==[]->_212407626=[];throw(block_exit([], [])), _212407626=ThrowResult53), get_var(AEnv, sys_l, L_Get56), cl_cdr(L_Get56, L), set_var(AEnv, sys_l, L), goto(do_label_17, AEnv), _TBResult=_GORES57)),
					  
					  [ addr(addr_tagbody_17_do_label_17,
						 do_label_17,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_l, IFTEST), (IFTEST==[]->throw(block_exit([], t)), _212575366=ThrowResult;get_var(AEnv, sys_l, L_Get24), cl_car(L_Get24, Member1_Param71), get_var(AEnv, key, Get_var_Ret), get_var(AEnv, sys_list2, Get_var_Ret73), get_var(AEnv, sys_test_not, Get_var_Ret74), get_var(AEnv, test, Get_var_Ret75), f_sys_member1(Member1_Param71, Get_var_Ret73, Get_var_Ret75, Get_var_Ret74, Get_var_Ret, IFTEST22), (IFTEST22\==[]->_212575540=[];throw(block_exit([], [])), _212575540=ThrowResult30), get_var(AEnv, sys_l, L_Get33), cl_cdr(L_Get33, Cdr_Ret), set_var(AEnv, sys_l, Cdr_Ret), goto(do_label_17, AEnv), _212575366=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(subsetp, FnResult),
	      true).
:- set_opv(cl_subsetp, classof, claz_function),
   set_opv(subsetp, compile_as, kw_function),
   set_opv(subsetp, function, cl_subsetp),
   DefunResult=subsetp.
/*
:- side_effect(assert_lsp(subsetp,
			  doc:doc_string(subsetp, _212183578, function, "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns T if every element of LIST1 is also an element of LIST2.  Returns NIL\r\notherwise."))).
*/
/*
:- side_effect(assert_lsp(subsetp,
			  wl:lambda_def(defun, subsetp, cl_subsetp, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], [[do, [[sys_l, sys_list1, [cdr, sys_l]]], [[null, sys_l], t], [unless, [sys_member1, [car, sys_l], sys_list2, test, sys_test_not, key], [return, []]]]]))).
*/
/*
:- side_effect(assert_lsp(subsetp,
			  wl:arglist_info(subsetp, cl_subsetp, [sys_list1, sys_list2, c38_key, test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[test, sys_test_not, key], names:[sys_list1, sys_list2, test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(subsetp, wl:init_args(2, cl_subsetp))).
*/
/*
(defun rassoc-if (test alist &key key)
  "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no
such pair exists."
  (rassoc test alist :test #'funcall :key key))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:28863 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'rassoc-if',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no\r\nsuch pair exists."),[rassoc,test,alist,':test',function(funcall),':key',key]])
doc: doc_string(rassoc_if,
	      _215018480,
	      function,
	      "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no\r\nsuch pair exists.").

wl:lambda_def(defun, rassoc_if, cl_rassoc_if, [test, sys_alist, c38_key, key], [[rassoc, test, sys_alist, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(rassoc_if, cl_rassoc_if, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_rassoc_if).

/*

### Compiled:  `CL:RASSOC-IF` 
*/
cl_rassoc_if(Test, Alist, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(test, Test), bv(sys_alist, Alist), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_alist, Alist_Get),
	get_var(GEnv, test, Test_Get),
	cl_rassoc(Test_Get,
		  Alist_Get,
		  [kw_test, cl_funcall, kw_key, Key_Get],
		  Rassoc_Ret),
	Rassoc_Ret=FnResult.
:- set_opv(cl_rassoc_if, classof, claz_function),
   set_opv(rassoc_if, compile_as, kw_function),
   set_opv(rassoc_if, function, cl_rassoc_if),
   DefunResult=rassoc_if.
/*
:- side_effect(assert_lsp(rassoc_if,
			  doc:doc_string(rassoc_if, _215018480, function, "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no\r\nsuch pair exists."))).
*/
/*
:- side_effect(assert_lsp(rassoc_if,
			  wl:lambda_def(defun, rassoc_if, cl_rassoc_if, [test, sys_alist, c38_key, key], [[rassoc, test, sys_alist, kw_test, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(rassoc_if,
			  wl:arglist_info(rassoc_if, cl_rassoc_if, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(rassoc_if, wl:init_args(2, cl_rassoc_if))).
*/
/*
(defun rassoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL
if no such pair exists."
  (rassoc test alist :test-not #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:29052 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'rassoc-if-not',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL\r\nif no such pair exists."),[rassoc,test,alist,':test-not',function(funcall),':key',key]])
doc: doc_string(rassoc_if_not,
	      _216189262,
	      function,
	      "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL\r\nif no such pair exists.").

wl:lambda_def(defun, rassoc_if_not, cl_rassoc_if_not, [test, sys_alist, c38_key, key], [[rassoc, test, sys_alist, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(rassoc_if_not, cl_rassoc_if_not, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_rassoc_if_not).

/*

### Compiled:  `CL:RASSOC-IF-NOT` 
*/
cl_rassoc_if_not(Test, Alist, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(test, Test), bv(sys_alist, Alist), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_alist, Alist_Get),
	get_var(GEnv, test, Test_Get),
	cl_rassoc(Test_Get,
		  Alist_Get,
		  [kw_test_not, cl_funcall, kw_key, Key_Get],
		  Rassoc_Ret),
	Rassoc_Ret=FnResult.
:- set_opv(cl_rassoc_if_not, classof, claz_function),
   set_opv(rassoc_if_not, compile_as, kw_function),
   set_opv(rassoc_if_not, function, cl_rassoc_if_not),
   DefunResult=rassoc_if_not.
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  doc:doc_string(rassoc_if_not, _216189262, function, "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL\r\nif no such pair exists."))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  wl:lambda_def(defun, rassoc_if_not, cl_rassoc_if_not, [test, sys_alist, c38_key, key], [[rassoc, test, sys_alist, kw_test_not, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  wl:arglist_info(rassoc_if_not, cl_rassoc_if_not, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not, wl:init_args(2, cl_rassoc_if_not))).
*/
/*
(defun assoc-if (test alist &key key)
  "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no
such pair exists."
  (assoc test alist :test #'funcall :key key))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:29259 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'assoc-if',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no\r\nsuch pair exists."),[assoc,test,alist,':test',function(funcall),':key',key]])
doc: doc_string(assoc_if,
	      _217357226,
	      function,
	      "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no\r\nsuch pair exists.").

wl:lambda_def(defun, assoc_if, cl_assoc_if, [test, sys_alist, c38_key, key], [[assoc, test, sys_alist, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(assoc_if, cl_assoc_if, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_assoc_if).

/*

### Compiled:  `CL:ASSOC-IF` 
*/
cl_assoc_if(Test, Alist, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(test, Test), bv(sys_alist, Alist), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_alist, Alist_Get),
	get_var(GEnv, test, Test_Get),
	cl_assoc(Test_Get,
		 Alist_Get,
		 [kw_test, cl_funcall, kw_key, Key_Get],
		 Assoc_Ret),
	Assoc_Ret=FnResult.
:- set_opv(cl_assoc_if, classof, claz_function),
   set_opv(assoc_if, compile_as, kw_function),
   set_opv(assoc_if, function, cl_assoc_if),
   DefunResult=assoc_if.
/*
:- side_effect(assert_lsp(assoc_if,
			  doc:doc_string(assoc_if, _217357226, function, "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no\r\nsuch pair exists."))).
*/
/*
:- side_effect(assert_lsp(assoc_if,
			  wl:lambda_def(defun, assoc_if, cl_assoc_if, [test, sys_alist, c38_key, key], [[assoc, test, sys_alist, kw_test, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(assoc_if,
			  wl:arglist_info(assoc_if, cl_assoc_if, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(assoc_if, wl:init_args(2, cl_assoc_if))).
*/
/*
(defun assoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL
if no such pair exists."
  (assoc test alist :test-not #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:29447 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'assoc-if-not',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL\r\nif no such pair exists."),[assoc,test,alist,':test-not',function(funcall),':key',key]])
doc: doc_string(assoc_if_not,
	      _218525218,
	      function,
	      "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL\r\nif no such pair exists.").

wl:lambda_def(defun, assoc_if_not, cl_assoc_if_not, [test, sys_alist, c38_key, key], [[assoc, test, sys_alist, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(assoc_if_not, cl_assoc_if_not, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_assoc_if_not).

/*

### Compiled:  `CL:ASSOC-IF-NOT` 
*/
cl_assoc_if_not(Test, Alist, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(test, Test), bv(sys_alist, Alist), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_alist, Alist_Get),
	get_var(GEnv, test, Test_Get),
	cl_assoc(Test_Get,
		 Alist_Get,
		 [kw_test_not, cl_funcall, kw_key, Key_Get],
		 Assoc_Ret),
	Assoc_Ret=FnResult.
:- set_opv(cl_assoc_if_not, classof, claz_function),
   set_opv(assoc_if_not, compile_as, kw_function),
   set_opv(assoc_if_not, function, cl_assoc_if_not),
   DefunResult=assoc_if_not.
/*
:- side_effect(assert_lsp(assoc_if_not,
			  doc:doc_string(assoc_if_not, _218525218, function, "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL\r\nif no such pair exists."))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not,
			  wl:lambda_def(defun, assoc_if_not, cl_assoc_if_not, [test, sys_alist, c38_key, key], [[assoc, test, sys_alist, kw_test_not, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not,
			  wl:arglist_info(assoc_if_not, cl_assoc_if_not, [test, sys_alist, c38_key, key], arginfo{all:[test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, sys_alist, key], opt:0, req:[test, sys_alist], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not, wl:init_args(2, cl_assoc_if_not))).
*/
/*
(defun member-if (test list &key key)
  "Searches LIST for an element that satisfies TEST.  If found, returns the
sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test #'funcall :key key))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:29652 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'member-if',[test,list,'&key',key],'$STRING'("Searches LIST for an element that satisfies TEST.  If found, returns the\r\nsublist of LIST that begins with the element.  If not found, returns NIL."),[member,test,list,':test',function(funcall),':key',key]])
doc: doc_string(member_if,
	      _219698372,
	      function,
	      "Searches LIST for an element that satisfies TEST.  If found, returns the\r\nsublist of LIST that begins with the element.  If not found, returns NIL.").

wl:lambda_def(defun, member_if, cl_member_if, [test, list, c38_key, key], [[member, test, list, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(member_if, cl_member_if, [test, list, c38_key, key], arginfo{all:[test, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, list, key], opt:0, req:[test, list], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_member_if).

/*

### Compiled:  `CL:MEMBER-IF` 
*/
cl_member_if(Test, List, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(test, Test), bv(list, List), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, list, List_Get),
	get_var(GEnv, test, Test_Get),
	cl_member(Test_Get,
		  List_Get,
		  kw_test,
		  cl_funcall,
		  kw_key,
		  Key_Get,
		  Member_Ret),
	Member_Ret=FnResult.
:- set_opv(cl_member_if, classof, claz_function),
   set_opv(member_if, compile_as, kw_function),
   set_opv(member_if, function, cl_member_if),
   DefunResult=member_if.
/*
:- side_effect(assert_lsp(member_if,
			  doc:doc_string(member_if, _219698372, function, "Searches LIST for an element that satisfies TEST.  If found, returns the\r\nsublist of LIST that begins with the element.  If not found, returns NIL."))).
*/
/*
:- side_effect(assert_lsp(member_if,
			  wl:lambda_def(defun, member_if, cl_member_if, [test, list, c38_key, key], [[member, test, list, kw_test, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(member_if,
			  wl:arglist_info(member_if, cl_member_if, [test, list, c38_key, key], arginfo{all:[test, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, list, key], opt:0, req:[test, list], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(member_if, wl:init_args(2, cl_member_if))).
*/
/*
(defun member-if-not (test list &key key)
  "Searches LIST for an element that does not satisfy TEST.  If found, returns
the sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test-not #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:29892 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'member-if-not',[test,list,'&key',key],'$STRING'("Searches LIST for an element that does not satisfy TEST.  If found, returns\r\nthe sublist of LIST that begins with the element.  If not found, returns NIL."),[member,test,list,':test-not',function(funcall),':key',key]])
doc: doc_string(member_if_not,
	      _220863624,
	      function,
	      "Searches LIST for an element that does not satisfy TEST.  If found, returns\r\nthe sublist of LIST that begins with the element.  If not found, returns NIL.").

wl:lambda_def(defun, member_if_not, cl_member_if_not, [test, list, c38_key, key], [[member, test, list, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(member_if_not, cl_member_if_not, [test, list, c38_key, key], arginfo{all:[test, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, list, key], opt:0, req:[test, list], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_member_if_not).

/*

### Compiled:  `CL:MEMBER-IF-NOT` 
*/
cl_member_if_not(Test, List, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(test, Test), bv(list, List), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, list, List_Get),
	get_var(GEnv, test, Test_Get),
	cl_member(Test_Get,
		  List_Get,
		  kw_test_not,
		  cl_funcall,
		  kw_key,
		  Key_Get,
		  Member_Ret),
	Member_Ret=FnResult.
:- set_opv(cl_member_if_not, classof, claz_function),
   set_opv(member_if_not, compile_as, kw_function),
   set_opv(member_if_not, function, cl_member_if_not),
   DefunResult=member_if_not.
/*
:- side_effect(assert_lsp(member_if_not,
			  doc:doc_string(member_if_not, _220863624, function, "Searches LIST for an element that does not satisfy TEST.  If found, returns\r\nthe sublist of LIST that begins with the element.  If not found, returns NIL."))).
*/
/*
:- side_effect(assert_lsp(member_if_not,
			  wl:lambda_def(defun, member_if_not, cl_member_if_not, [test, list, c38_key, key], [[member, test, list, kw_test_not, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(member_if_not,
			  wl:arglist_info(member_if_not, cl_member_if_not, [test, list, c38_key, key], arginfo{all:[test, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[test, list, key], opt:0, req:[test, list], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(member_if_not, wl:init_args(2, cl_member_if_not))).
*/
/*
(defun subst-if (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.
The original TREE is not destroyed."
  (subst new test tree :test #'funcall :key key))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:30149 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'subst-if',[new,test,tree,'&key',key],'$STRING'("Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.\r\nThe original TREE is not destroyed."),[subst,new,test,tree,':test',function(funcall),':key',key]])
doc: doc_string(subst_if,
	      _222022624,
	      function,
	      "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.\r\nThe original TREE is not destroyed.").

wl:lambda_def(defun, subst_if, cl_subst_if, [sys_new, test, sys_tree, c38_key, key], [[subst, sys_new, test, sys_tree, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(subst_if, cl_subst_if, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, cl_subst_if).

/*

### Compiled:  `CL:SUBST-IF` 
*/
cl_subst_if(New, Test, Tree, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_new, New), bv(test, Test), bv(sys_tree, Tree), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_new, New_Get),
	get_var(GEnv, sys_tree, Tree_Get),
	get_var(GEnv, test, Test_Get),
	cl_subst(New_Get,
		 Test_Get,
		 Tree_Get,
		 kw_test,
		 cl_funcall,
		 kw_key,
		 Key_Get,
		 Subst_Ret),
	Subst_Ret=FnResult.
:- set_opv(cl_subst_if, classof, claz_function),
   set_opv(subst_if, compile_as, kw_function),
   set_opv(subst_if, function, cl_subst_if),
   DefunResult=subst_if.
/*
:- side_effect(assert_lsp(subst_if,
			  doc:doc_string(subst_if, _222022624, function, "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.\r\nThe original TREE is not destroyed."))).
*/
/*
:- side_effect(assert_lsp(subst_if,
			  wl:lambda_def(defun, subst_if, cl_subst_if, [sys_new, test, sys_tree, c38_key, key], [[subst, sys_new, test, sys_tree, kw_test, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(subst_if,
			  wl:arglist_info(subst_if, cl_subst_if, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(subst_if, wl:init_args(3, cl_subst_if))).
*/
/*
(defun subst-if-not (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the
result.  The original TREE is not destroyed."
  (subst new test tree :test-not #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:30363 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'subst-if-not',[new,test,tree,'&key',key],'$STRING'("Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the\r\nresult.  The original TREE is not destroyed."),[subst,new,test,tree,':test-not',function(funcall),':key',key]])
doc: doc_string(subst_if_not,
	      _223251408,
	      function,
	      "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the\r\nresult.  The original TREE is not destroyed.").

wl:lambda_def(defun, subst_if_not, cl_subst_if_not, [sys_new, test, sys_tree, c38_key, key], [[subst, sys_new, test, sys_tree, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(subst_if_not, cl_subst_if_not, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, cl_subst_if_not).

/*

### Compiled:  `CL:SUBST-IF-NOT` 
*/
cl_subst_if_not(New, Test, Tree, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_new, New), bv(test, Test), bv(sys_tree, Tree), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_new, New_Get),
	get_var(GEnv, sys_tree, Tree_Get),
	get_var(GEnv, test, Test_Get),
	cl_subst(New_Get,
		 Test_Get,
		 Tree_Get,
		 kw_test_not,
		 cl_funcall,
		 kw_key,
		 Key_Get,
		 Subst_Ret),
	Subst_Ret=FnResult.
:- set_opv(cl_subst_if_not, classof, claz_function),
   set_opv(subst_if_not, compile_as, kw_function),
   set_opv(subst_if_not, function, cl_subst_if_not),
   DefunResult=subst_if_not.
/*
:- side_effect(assert_lsp(subst_if_not,
			  doc:doc_string(subst_if_not, _223251408, function, "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the\r\nresult.  The original TREE is not destroyed."))).
*/
/*
:- side_effect(assert_lsp(subst_if_not,
			  wl:lambda_def(defun, subst_if_not, cl_subst_if_not, [sys_new, test, sys_tree, c38_key, key], [[subst, sys_new, test, sys_tree, kw_test_not, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(subst_if_not,
			  wl:arglist_info(subst_if_not, cl_subst_if_not, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(subst_if_not, wl:init_args(3, cl_subst_if_not))).
*/
/*
(defun nsubst-if (new test tree &key key)
  "Destructive SUBST-IF. TREE may be modified."
  (nsubst new test tree :test #'funcall :key key))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:30595 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nsubst-if',[new,test,tree,'&key',key],'$STRING'("Destructive SUBST-IF. TREE may be modified."),[nsubst,new,test,tree,':test',function(funcall),':key',key]])
doc: doc_string(nsubst_if,
	      _224466716,
	      function,
	      "Destructive SUBST-IF. TREE may be modified.").

wl:lambda_def(defun, nsubst_if, cl_nsubst_if, [sys_new, test, sys_tree, c38_key, key], [[nsubst, sys_new, test, sys_tree, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(nsubst_if, cl_nsubst_if, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, cl_nsubst_if).

/*

### Compiled:  `CL:NSUBST-IF` 
*/
cl_nsubst_if(New, Test, Tree, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_new, New), bv(test, Test), bv(sys_tree, Tree), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_new, New_Get),
	get_var(GEnv, sys_tree, Tree_Get),
	get_var(GEnv, test, Test_Get),
	cl_nsubst(New_Get,
		  Test_Get,
		  Tree_Get,
		  kw_test,
		  cl_funcall,
		  kw_key,
		  Key_Get,
		  Nsubst_Ret),
	Nsubst_Ret=FnResult.
:- set_opv(cl_nsubst_if, classof, claz_function),
   set_opv(nsubst_if, compile_as, kw_function),
   set_opv(nsubst_if, function, cl_nsubst_if),
   DefunResult=nsubst_if.
/*
:- side_effect(assert_lsp(nsubst_if,
			  doc:doc_string(nsubst_if, _224466716, function, "Destructive SUBST-IF. TREE may be modified."))).
*/
/*
:- side_effect(assert_lsp(nsubst_if,
			  wl:lambda_def(defun, nsubst_if, cl_nsubst_if, [sys_new, test, sys_tree, c38_key, key], [[nsubst, sys_new, test, sys_tree, kw_test, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(nsubst_if,
			  wl:arglist_info(nsubst_if, cl_nsubst_if, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(nsubst_if, wl:init_args(3, cl_nsubst_if))).
*/
/*
(defun nsubst-if-not (new test tree &key key)
  "Destructive SUBST-IF-NOT. TREE may be modified."
  (nsubst new test tree :test-not #'funcall :key key))


#|
(funcall #'(setf macro-function)
	 #'(lambda (name lambda-list &rest body)
	     (list 'progn
		   (list 'funcall '#'(setf macro-function)
			 (list 'function
			       (cons 'lambda (cons lambda-list body)))
			 (list 'quote name))
		   (list 'quote name)))
	 'defmacro)
|#

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:30739 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nsubst-if-not',[new,test,tree,'&key',key],'$STRING'("Destructive SUBST-IF-NOT. TREE may be modified."),[nsubst,new,test,tree,':test-not',function(funcall),':key',key]])
doc: doc_string(nsubst_if_not,
	      _225710308,
	      function,
	      "Destructive SUBST-IF-NOT. TREE may be modified.").

wl:lambda_def(defun, nsubst_if_not, cl_nsubst_if_not, [sys_new, test, sys_tree, c38_key, key], [[nsubst, sys_new, test, sys_tree, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(nsubst_if_not, cl_nsubst_if_not, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, cl_nsubst_if_not).

/*

### Compiled:  `CL:NSUBST-IF-NOT` 
*/
cl_nsubst_if_not(New, Test, Tree, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_new, New), bv(test, Test), bv(sys_tree, Tree), bv(key, Key)]|Env]|Env],
	get_kw(Env, RestNKeys, key, key, Key, []=Key, Key_P),
	get_var(GEnv, key, Key_Get),
	get_var(GEnv, sys_new, New_Get),
	get_var(GEnv, sys_tree, Tree_Get),
	get_var(GEnv, test, Test_Get),
	cl_nsubst(New_Get,
		  Test_Get,
		  Tree_Get,
		  kw_test_not,
		  cl_funcall,
		  kw_key,
		  Key_Get,
		  Nsubst_Ret),
	Nsubst_Ret=FnResult.
:- set_opv(cl_nsubst_if_not, classof, claz_function),
   set_opv(nsubst_if_not, compile_as, kw_function),
   set_opv(nsubst_if_not, function, cl_nsubst_if_not),
   DefunResult=nsubst_if_not.
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  doc:doc_string(nsubst_if_not, _225710308, function, "Destructive SUBST-IF-NOT. TREE may be modified."))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  wl:lambda_def(defun, nsubst_if_not, cl_nsubst_if_not, [sys_new, test, sys_tree, c38_key, key], [[nsubst, sys_new, test, sys_tree, kw_test_not, function(funcall), kw_key, key]]))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  wl:arglist_info(nsubst_if_not, cl_nsubst_if_not, [sys_new, test, sys_tree, c38_key, key], arginfo{all:[sys_new, test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, test, sys_tree, key], opt:0, req:[sys_new, test, sys_tree], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not, wl:init_args(3, cl_nsubst_if_not))).
*/
/*

(funcall #'(setf macro-function)
	 #'(lambda (name lambda-list &rest body)
	     (list 'progn
		   (list 'funcall '#'(setf macro-function)
			 (list 'function
			       (cons 'lambda (cons lambda-list body)))
			 (list 'quote name))
		   (list 'quote name)))
	 'defmacro)
*/
/*
(defmacro defun500 (name lambda-list &rest body)
  (list 'progn
	(list 'funcall '#'(setf fdefinition)
	      (list 'function
		    (list 'lambda lambda-list
			  (cons 'block (cons (if (consp name)
						 (car (cdr name))
						 name)
					     body))))
	      (list 'quote name))
	(list 'quote name)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:31190 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defun500,[name,'lambda-list','&rest',body],[list,[quote,progn],[list,[quote,funcall],[quote,function([setf,fdefinition])],[list,[quote,function],[list,[quote,lambda],'lambda-list',[cons,[quote,block],[cons,[if,[consp,name],[car,[cdr,name]],name],body]]]],[list,[quote,quote],name]],[list,[quote,quote],name]]])
wl:lambda_def(defmacro, sys_defun500, f_sys_defun500, [sys_name, sys_lambda_list, c38_rest, sys_body], [progn, [list, [quote, progn], [list, [quote, funcall], [quote, function([setf, fdefinition])], [list, [quote, function], [list, [quote, lambda], sys_lambda_list, [cons, [quote, block], [cons, [if, [consp, sys_name], [car, [cdr, sys_name]], sys_name], sys_body]]]], [list, [quote, quote], sys_name]], [list, [quote, quote], sys_name]]]).
wl:arglist_info(sys_defun500, f_sys_defun500, [sys_name, sys_lambda_list, c38_rest, sys_body], arginfo{all:[sys_name, sys_lambda_list], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_lambda_list, sys_body], opt:0, req:[sys_name, sys_lambda_list], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(2, f_sys_defun500).

/*

### Compiled:  `SYS::DEFUN500` 
*/
f_sys_defun500(Name, Lambda_list, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(sys_name, Name), bv(sys_lambda_list, Lambda_list), bv(sys_body, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, sys_lambda_list, Lambda_list_Get),
	get_var(GEnv, sys_name, Name_Get),
	(   is_consp(Name_Get)
	->  get_var(GEnv, sys_name, Name_Get12),
	    cl_cdr(Name_Get12, Car_Param),
	    cl_car(Car_Param, TrueResult),
	    CAR=TrueResult
	;   get_var(GEnv, sys_name, Name_Get13),
	    CAR=Name_Get13
	),
	get_var(GEnv, sys_body, Body_Get),
	CDR=[CAR|Body_Get],
	CAR27=[block|CDR],
	CAR28=[lambda, Lambda_list_Get, CAR27],
	CAR30=[function, CAR28],
	get_var(GEnv, sys_name, Name_Get17),
	CAR29=[quote, Name_Get17],
	CAR32=[funcall, function([setf, fdefinition]), CAR30, CAR29],
	get_var(GEnv, sys_name, Name_Get18),
	CAR31=[quote, Name_Get18],
	_226950722=[progn, CAR32, CAR31],
	_226950722=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_sys_defun500, classof, claz_macro),
   set_opv(sys_defun500, compile_as, kw_operator),
   set_opv(sys_defun500, function, f_sys_defun500),
   DefMacroResult=sys_defun500.
/*
:- side_effect(assert_lsp(sys_defun500,
			  wl:lambda_def(defmacro, sys_defun500, f_sys_defun500, [sys_name, sys_lambda_list, c38_rest, sys_body], [progn, [list, [quote, progn], [list, [quote, funcall], [quote, function([setf, fdefinition])], [list, [quote, function], [list, [quote, lambda], sys_lambda_list, [cons, [quote, block], [cons, [if, [consp, sys_name], [car, [cdr, sys_name]], sys_name], sys_body]]]], [list, [quote, quote], sys_name]], [list, [quote, quote], sys_name]]]))).
*/
/*
:- side_effect(assert_lsp(sys_defun500,
			  wl:arglist_info(sys_defun500, f_sys_defun500, [sys_name, sys_lambda_list, c38_rest, sys_body], arginfo{all:[sys_name, sys_lambda_list], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_lambda_list, sys_body], opt:0, req:[sys_name, sys_lambda_list], rest:[sys_body], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_defun500, wl:init_args(2, f_sys_defun500))).
*/
/*
(defmacro setf (place new-value)
  (if (consp place)
      (cons 'funcall (cons (list 'function (list 'setf (car place)))
			   (cons new-value (cdr place))))
      (list 'setq place new-value)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:31506 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,setf,[place,'new-value'],[if,[consp,place],[cons,[quote,funcall],[cons,[list,[quote,function],[list,[quote,setf],[car,place]]],[cons,'new-value',[cdr,place]]]],[list,[quote,setq],place,'new-value']]])
wl:lambda_def(defmacro, setf, cl_setf, [sys_place, sys_new_value], [progn, [if, [consp, sys_place], [cons, [quote, funcall], [cons, [list, [quote, function], [list, [quote, setf], [car, sys_place]]], [cons, sys_new_value, [cdr, sys_place]]]], [list, [quote, setq], sys_place, sys_new_value]]]).
wl:arglist_info(setf, cl_setf, [sys_place, sys_new_value], arginfo{all:[sys_place, sys_new_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_new_value], opt:0, req:[sys_place, sys_new_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_setf).

/*

### Compiled:  `CL:SETF` 
*/
cl_setf(Place, New_value, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(sys_place, Place), bv(sys_new_value, New_value)|Global_env_Ret],
	get_var(Env, sys_place, Place_Get),
	(   is_consp(Place_Get)
	->  get_var(Env, sys_place, Place_Get10),
	    cl_car(Place_Get10, Car_Ret),
	    CAR=[setf, Car_Ret],
	    CAR25=[function, CAR],
	    get_var(Env, sys_new_value, New_value_Get),
	    get_var(Env, sys_place, Place_Get12),
	    cl_cdr(Place_Get12, Cdr_Ret),
	    CDR=[New_value_Get|Cdr_Ret],
	    CDR27=[CAR25|CDR],
	    _228556420=[funcall|CDR27]
	;   get_var(Env, sys_new_value, New_value_Get14),
	    get_var(Env, sys_place, Place_Get13),
	    _228556420=[setq, Place_Get13, New_value_Get14]
	),
	_228556420=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_setf, classof, claz_macro),
   set_opv(setf, compile_as, kw_operator),
   set_opv(setf, function, cl_setf),
   DefMacroResult=setf.
/*
:- side_effect(assert_lsp(setf,
			  wl:lambda_def(defmacro, setf, cl_setf, [sys_place, sys_new_value], [progn, [if, [consp, sys_place], [cons, [quote, funcall], [cons, [list, [quote, function], [list, [quote, setf], [car, sys_place]]], [cons, sys_new_value, [cdr, sys_place]]]], [list, [quote, setq], sys_place, sys_new_value]]]))).
*/
/*
:- side_effect(assert_lsp(setf,
			  wl:arglist_info(setf, cl_setf, [sys_place, sys_new_value], arginfo{all:[sys_place, sys_new_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_new_value], opt:0, req:[sys_place, sys_new_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(setf, wl:init_args(exact_only, cl_setf))).
*/
/*
(defun append (&rest lists)
  (if (cdr lists)
      (let ((list (car lists))
	    (result nil)
	    (end nil))
	(if list
	    (tagbody
	     start
	       (if list
		   (progn
		     (setf end (if end
				   (setf (cdr end) (list (car list)))
				   (setf result (list (car list)))))
		     (setf list (cdr list))
		     (go start)))
	       (setf (cdr end) (apply #'append (cdr lists)))
	       (return-from append result))
	    (apply #'append (cdr lists))))
      (car lists)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:31707 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,append,['&rest',lists],[if,[cdr,lists],[let,[[list,[car,lists]],[result,[]],[end,[]]],[if,list,[tagbody,start,[if,list,[progn,[setf,end,[if,end,[setf,[cdr,end],[list,[car,list]]],[setf,result,[list,[car,list]]]]],[setf,list,[cdr,list]],[go,start]]],[setf,[cdr,end],[apply,function(append),[cdr,lists]]],['return-from',append,result]],[apply,function(append),[cdr,lists]]]],[car,lists]]])
wl:lambda_def(defun, append, cl_append, [c38_rest, sys_lists], [[if, [cdr, sys_lists], [let, [[list, [car, sys_lists]], [sys_result, []], [end, []]], [if, list, [tagbody, start, [if, list, [progn, [setf, end, [if, end, [setf, [cdr, end], [list, [car, list]]], [setf, sys_result, [list, [car, list]]]]], [setf, list, [cdr, list]], [go, start]]], [setf, [cdr, end], [apply, function(append), [cdr, sys_lists]]], [return_from, append, sys_result]], [apply, function(append), [cdr, sys_lists]]]], [car, sys_lists]]]).
wl:arglist_info(append, cl_append, [c38_rest, sys_lists], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_lists], opt:0, req:0, rest:[sys_lists], sublists:0, whole:0}).
wl: init_args(0, cl_append).

/*

### Compiled:  `CL:APPEND` 
*/
cl_append(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_lists, RestNKeys)]|Env]|Env],
	catch(( ( get_var(GEnv, sys_lists, Lists_Get),
		  cl_cdr(Lists_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_lists, Lists_Get13),
		      cl_car(Lists_Get13, List_Init),
		      GoEnv=[bv(list, List_Init), bv(sys_result, []), bv(end, [])|GEnv],
		      get_var(GoEnv, list, IFTEST15),
		      (   IFTEST15\==[]
		      ->  call_addr_block(GoEnv,
					  (push_label(start), get_var(GoEnv, list, IFTEST41), (IFTEST41\==[]->get_var(GoEnv, end, IFTEST44), (IFTEST44\==[]->get_var(GoEnv, end, End_Get47), get_var(GoEnv, list, List_Get48), cl_car(List_Get48, Car_Ret), _230206918=[Car_Ret], cl_rplacd(End_Get47, _230206918, TrueResult50), End=TrueResult50;get_var(GoEnv, list, List_Get49), cl_car(List_Get49, Car_Ret76), ElseResult51=[Car_Ret76], set_var(GoEnv, sys_result, ElseResult51), End=ElseResult51), set_var(GoEnv, end, End), get_var(GoEnv, list, List_Get52), cl_cdr(List_Get52, List), set_var(GoEnv, list, List), goto(start, GoEnv), _230196052=_GORES53;_230196052=[]), get_var(GoEnv, end, End_Get56), get_var(GoEnv, sys_lists, Lists_Get57), cl_cdr(Lists_Get57, KeysNRest), cl_apply(cl_append, KeysNRest, Apply_Ret), cl_rplacd(End_Get56, Apply_Ret, Rplacd_Ret), get_var(GoEnv, sys_result, RetResult58), throw(block_exit(append, RetResult58))),
					  
					  [ addr(addr_tagbody_18_start,
						 start,
						 '$unused',
						 GoEnv,
						 (get_var(GoEnv, list, IFTEST19), (IFTEST19\==[]->get_var(GoEnv, end, IFTEST22), (IFTEST22\==[]->get_var(GoEnv, end, End_Get25), get_var(GoEnv, list, List_Get26), cl_car(List_Get26, Car_Ret79), _230367024=[Car_Ret79], cl_rplacd(End_Get25, _230367024, Rplacd_Ret80), Set_var_Ret83=Rplacd_Ret80;get_var(GoEnv, list, List_Get27), cl_car(List_Get27, Car_Ret81), Set_var_Ret=[Car_Ret81], set_var(GoEnv, sys_result, Set_var_Ret), Set_var_Ret83=Set_var_Ret), set_var(GoEnv, end, Set_var_Ret83), get_var(GoEnv, list, List_Get30), cl_cdr(List_Get30, Cdr_Ret), set_var(GoEnv, list, Cdr_Ret), goto(start, GoEnv), _230367214=_GORES;_230367214=[]), get_var(GoEnv, end, End_Get34), get_var(GoEnv, sys_lists, Lists_Get35), cl_cdr(Lists_Get35, KeysNRest73), cl_apply(cl_append, KeysNRest73, Apply_Ret85), cl_rplacd(End_Get34, Apply_Ret85, Rplacd_Ret86), get_var(GoEnv, sys_result, Get_var_Ret), throw(block_exit(append, Get_var_Ret))))
					  ]),
			  LetResult=[]
		      ;   get_var(GoEnv, sys_lists, Lists_Get62),
			  cl_cdr(Lists_Get62, KeysNRest74),
			  cl_apply(cl_append, KeysNRest74, ElseResult63),
			  LetResult=ElseResult63
		      )
		  ;   get_var(GEnv, sys_lists, Lists_Get64),
		      cl_car(Lists_Get64, ElseResult66),
		      LetResult=ElseResult66
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(append, FnResult),
	      true).
:- set_opv(cl_append, classof, claz_function),
   set_opv(append, compile_as, kw_function),
   set_opv(append, function, cl_append),
   DefunResult=append.
/*
:- side_effect(assert_lsp(append,
			  wl:lambda_def(defun, append, cl_append, [c38_rest, sys_lists], [[if, [cdr, sys_lists], [let, [[list, [car, sys_lists]], [sys_result, []], [end, []]], [if, list, [tagbody, start, [if, list, [progn, [setf, end, [if, end, [setf, [cdr, end], [list, [car, list]]], [setf, sys_result, [list, [car, list]]]]], [setf, list, [cdr, list]], [go, start]]], [setf, [cdr, end], [apply, function(append), [cdr, sys_lists]]], [return_from, append, sys_result]], [apply, function(append), [cdr, sys_lists]]]], [car, sys_lists]]]))).
*/
/*
:- side_effect(assert_lsp(append,
			  wl:arglist_info(append, cl_append, [c38_rest, sys_lists], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_lists], opt:0, req:0, rest:[sys_lists], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(append, wl:init_args(0, cl_append))).
*/
/*
(defparameter *type-expanders* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:32209 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*type-expanders*',[]])
:- set_var(AEnv, defparameter, sys_xx_type_expanders_xx, []).
/*
(defconstant call-arguments-limit 65536)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:32246 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defconstant,'call-arguments-limit',65536])
:- set_var(AEnv, defconstant, call_arguments_limit, 65536).
/*
(defconstant lambda-parameters-limit 65536)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:32288 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defconstant,'lambda-parameters-limit',65536])
:- set_var(AEnv, defconstant, lambda_parameters_limit, 65536).
/*
(defconstant multiple-values-limit 65536)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:32333 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defconstant,'multiple-values-limit',65536])
:- set_var(AEnv, defconstant, multiple_values_limit, 65536).
/*
(defconstant lambda-list-keywords
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:32376 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defconstant,'lambda-list-keywords',[quote,['&allow-other-keys','&aux','&body','&environment','&key','&optional','&rest','&whole']]])
:- set_var(AEnv,
	   defconstant,
	   lambda_list_keywords,
	   
	   [ c38_allow_other_keys,
	     c38_aux,
	     c38_body,
	     c38_environment,
	     c38_key,
	     c38_optional,
	     c38_rest,
	     c38_whole
	   ]).
/*
(defmacro psetq (&rest rest)
  (let ((inits nil)
	(sets nil)
	(list rest))
    (tagbody
     start
       (when (cddr list)
	 (push (list (gensym) (cadr list)) inits)
	 (setq list (cddr list))
	 (go start)))
    (setq list inits)
    (tagbody
     start
       (when (cddr rest)
	 (push (caar list) sets)
	 (push (car rest) sets)
	 (setq list (cdr list))
	 (setq rest (cddr rest))
	 (go start)))
    `(let ,(reverse inits)
      (setq ,@sets ,@rest))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:32490 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,psetq,['&rest',rest],[let,[[inits,[]],[sets,[]],[list,rest]],[tagbody,start,[when,[cddr,list],[push,[list,[gensym],[cadr,list]],inits],[setq,list,[cddr,list]],[go,start]]],[setq,list,inits],[tagbody,start,[when,[cddr,rest],[push,[caar,list],sets],[push,[car,rest],sets],[setq,list,[cdr,list]],[setq,rest,[cddr,rest]],[go,start]]],['#BQ',[let,['#COMMA',[reverse,inits]],[setq,['#BQ-COMMA-ELIPSE',sets],['#BQ-COMMA-ELIPSE',rest]]]]]])
wl:lambda_def(defmacro, psetq, cl_psetq, [c38_rest, rest], [progn, [let, [[sys_inits, []], [sys_sets, []], [list, rest]], [tagbody, start, [when, [cddr, list], [push, [list, [gensym], [cadr, list]], sys_inits], [setq, list, [cddr, list]], [go, start]]], [setq, list, sys_inits], [tagbody, start, [when, [cddr, rest], [push, [caar, list], sys_sets], [push, [car, rest], sys_sets], [setq, list, [cdr, list]], [setq, rest, [cddr, rest]], [go, start]]], ['#BQ', [let, ['#COMMA', [reverse, sys_inits]], [setq, ['#BQ-COMMA-ELIPSE', sys_sets], ['#BQ-COMMA-ELIPSE', rest]]]]]]).
wl:arglist_info(psetq, cl_psetq, [c38_rest, rest], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[rest], opt:0, req:0, rest:[rest], sublists:0, whole:0}).
wl: init_args(1, cl_psetq).

/*

### Compiled:  `CL:PSETQ` 
*/
cl_psetq(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(rest, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	catch(( ( get_var(GEnv, rest, Rest_Get),
		  AEnv=[bv(sys_inits, []), bv(sys_sets, []), bv(list, Rest_Get)|GEnv],
		  call_addr_block(AEnv,
				  (push_label(start), get_var(AEnv, list, List_Get28), cl_cddr(List_Get28, IFTEST26), (IFTEST26\==[]->get_var(AEnv, sys_inits, Inits_Get29), cl_gensym(Gensym_Ret), get_var(AEnv, list, List_Get32), cl_cadr(List_Get32, Cadr_Ret), set_place(AEnv, push, [list, Gensym_Ret, Cadr_Ret], [Inits_Get29], Push_R30), get_var(AEnv, list, List_Get34), cl_cddr(List_Get34, List), set_var(AEnv, list, List), goto(start, AEnv), _TBResult=_GORES35;_TBResult=[])),
				  
				  [ addr(addr_tagbody_19_start,
					 start,
					 '$unused',
					 AEnv,
					 (get_var(AEnv, list, Cddr_Param), cl_cddr(Cddr_Param, IFTEST), (IFTEST\==[]->get_var(AEnv, sys_inits, Get_var_Ret), cl_gensym(Gensym_Ret87), get_var(AEnv, list, List_Get19), cl_cadr(List_Get19, Cadr_Ret88), set_place(AEnv, push, [list, Gensym_Ret87, Cadr_Ret88], [Get_var_Ret], Set_place_Ret), get_var(AEnv, list, List_Get21), cl_cddr(List_Get21, Cddr_Ret), set_var(AEnv, list, Cddr_Ret), goto(start, AEnv), _235856022=_GORES;_235856022=[])))
				  ]),
		  get_var(AEnv, sys_inits, Inits_Get38),
		  set_var(AEnv, list, Inits_Get38),
		  call_addr_block(AEnv,
				  (push_label(start), get_var(AEnv, rest, Rest_Get60), cl_cddr(Rest_Get60, IFTEST58), (IFTEST58\==[]->get_var(AEnv, list, List_Get64), get_var(AEnv, sys_sets, Sets_Get61), set_place(AEnv, push, [caar, List_Get64], [Sets_Get61], Push_R62), get_var(AEnv, rest, Rest_Get67), get_var(AEnv, sys_sets, Sets_Get65), set_place(AEnv, push, [car, Rest_Get67], [Sets_Get65], Push_R66), get_var(AEnv, list, List_Get69), cl_cdr(List_Get69, List80), set_var(AEnv, list, List80), get_var(AEnv, rest, Rest_Get70), cl_cddr(Rest_Get70, Rest), set_var(AEnv, rest, Rest), goto(start, AEnv), _TBResult39=_GORES71;_TBResult39=[])),
				  
				  [ addr(addr_tagbody_20_start,
					 start,
					 '$unused',
					 AEnv,
					 (get_var(AEnv, rest, Rest_Get42), cl_cddr(Rest_Get42, IFTEST40), (IFTEST40\==[]->get_var(AEnv, list, List_Get46), get_var(AEnv, sys_sets, Get_var_Ret91), set_place(AEnv, push, [caar, List_Get46], [Get_var_Ret91], Push_R44), get_var(AEnv, rest, Rest_Get50), get_var(AEnv, sys_sets, Sets_Get47), set_place(AEnv, push, [car, Rest_Get50], [Sets_Get47], Push_R48), get_var(AEnv, list, List_Get52), cl_cdr(List_Get52, Cdr_Ret), set_var(AEnv, list, Cdr_Ret), get_var(AEnv, rest, Rest_Get53), cl_cddr(Rest_Get53, Cddr_Ret93), set_var(AEnv, rest, Cddr_Ret93), goto(start, AEnv), _TBResult39=_GORES54;_TBResult39=[])))
				  ]),
		  get_var(AEnv, sys_inits, Inits_Get74),
		  cl_reverse(Inits_Get74, Reverse_Ret),
		  get_var(AEnv, rest, Rest_Get76),
		  get_var(AEnv, sys_sets, Sets_Get75),
		  bq_append([setq|Sets_Get75], Rest_Get76, Bq_append_Ret)
		),
		[let, Reverse_Ret, Bq_append_Ret]=MFResult
	      ),
	      block_exit(psetq, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_psetq, classof, claz_macro),
   set_opv(psetq, compile_as, kw_operator),
   set_opv(psetq, function, cl_psetq),
   DefMacroResult=psetq.
/*
:- side_effect(assert_lsp(psetq,
			  wl:lambda_def(defmacro, psetq, cl_psetq, [c38_rest, rest], [progn, [let, [[sys_inits, []], [sys_sets, []], [list, rest]], [tagbody, start, [when, [cddr, list], [push, [list, [gensym], [cadr, list]], sys_inits], [setq, list, [cddr, list]], [go, start]]], [setq, list, sys_inits], [tagbody, start, [when, [cddr, rest], [push, [caar, list], sys_sets], [push, [car, rest], sys_sets], [setq, list, [cdr, list]], [setq, rest, [cddr, rest]], [go, start]]], ['#BQ', [let, ['#COMMA', [reverse, sys_inits]], [setq, ['#BQ-COMMA-ELIPSE', sys_sets], ['#BQ-COMMA-ELIPSE', rest]]]]]]))).
*/
/*
:- side_effect(assert_lsp(psetq,
			  wl:arglist_info(psetq, cl_psetq, [c38_rest, rest], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[rest], opt:0, req:0, rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(psetq, wl:init_args(1, cl_psetq))).
*/
/*
(defmacro return (&optional result)
  `(return-from nil ,result))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:32964 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,return,['&optional',result],['#BQ',['return-from',[],['#COMMA',result]]]])
wl:lambda_def(defmacro, return, cl_return, [c38_optional, sys_result], [progn, ['#BQ', [return_from, [], ['#COMMA', sys_result]]]]).
wl:arglist_info(return, cl_return, [c38_optional, sys_result], arginfo{all:[sys_result], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_result], opt:[sys_result], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(rest_only, cl_return).

/*

### Compiled:  `CL:RETURN` 
*/
cl_return(Return_Param, FnResult) :-
	global_env(Opt_var_Param),
	GEnv=[[[bv(sys_result, Result)]|Opt_var_Param]|Opt_var_Param],
	append([], RestNKeys, Return_Param),
	opt_var(Opt_var_Param, sys_result, Result, true, [], 1, RestNKeys),
	get_var(GEnv, sys_result, Result_Get),
	[return_from, [], Result_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_return, classof, claz_macro),
   set_opv(return, compile_as, kw_operator),
   set_opv(return, function, cl_return),
   DefMacroResult=return.
/*
:- side_effect(assert_lsp(return,
			  wl:lambda_def(defmacro, return, cl_return, [c38_optional, sys_result], [progn, ['#BQ', [return_from, [], ['#COMMA', sys_result]]]]))).
*/
/*
:- side_effect(assert_lsp(return,
			  wl:arglist_info(return, cl_return, [c38_optional, sys_result], arginfo{all:[sys_result], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_result], opt:[sys_result], req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(return, wl:init_args(rest_only, cl_return))).
*/
/*
(defmacro when (test-form &rest forms)
  `(if ,test-form (progn ,@forms)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:33032 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,when,['test-form','&rest',forms],['#BQ',[if,['#COMMA','test-form'],[progn,['#BQ-COMMA-ELIPSE',forms]]]]])
wl:lambda_def(defmacro, when, cl_when, [sys_test_form, c38_rest, forms], [progn, ['#BQ', [if, ['#COMMA', sys_test_form], [progn, ['#BQ-COMMA-ELIPSE', forms]]]]]).
wl:arglist_info(when, cl_when, [sys_test_form, c38_rest, forms], arginfo{all:[sys_test_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_test_form, forms], opt:0, req:[sys_test_form], rest:[forms], sublists:0, whole:0}).
wl: init_args(1, cl_when).

/*

### Compiled:  `CL:WHEN` 
*/
cl_when(Test_form, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(sys_test_form, Test_form), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, forms, Forms_Get),
	get_var(GEnv, sys_test_form, Test_form_Get),
	[if, Test_form_Get, [progn|Forms_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_when, classof, claz_macro),
   set_opv(when, compile_as, kw_operator),
   set_opv(when, function, cl_when),
   DefMacroResult=when.
/*
:- side_effect(assert_lsp(when,
			  wl:lambda_def(defmacro, when, cl_when, [sys_test_form, c38_rest, forms], [progn, ['#BQ', [if, ['#COMMA', sys_test_form], [progn, ['#BQ-COMMA-ELIPSE', forms]]]]]))).
*/
/*
:- side_effect(assert_lsp(when,
			  wl:arglist_info(when, cl_when, [sys_test_form, c38_rest, forms], arginfo{all:[sys_test_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_test_form, forms], opt:0, req:[sys_test_form], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(when, wl:init_args(1, cl_when))).
*/
/*
(defmacro unless (test-form &rest forms)
  `(if (not ,test-form) (progn ,@forms)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:33109 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,unless,['test-form','&rest',forms],['#BQ',[if,[not,['#COMMA','test-form']],[progn,['#BQ-COMMA-ELIPSE',forms]]]]])
wl:lambda_def(defmacro, unless, cl_unless, [sys_test_form, c38_rest, forms], [progn, ['#BQ', [if, [not, ['#COMMA', sys_test_form]], [progn, ['#BQ-COMMA-ELIPSE', forms]]]]]).
wl:arglist_info(unless, cl_unless, [sys_test_form, c38_rest, forms], arginfo{all:[sys_test_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_test_form, forms], opt:0, req:[sys_test_form], rest:[forms], sublists:0, whole:0}).
wl: init_args(1, cl_unless).

/*

### Compiled:  `CL:UNLESS` 
*/
cl_unless(Test_form, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(sys_test_form, Test_form), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, forms, Forms_Get),
	get_var(GEnv, sys_test_form, Test_form_Get),
	[if, [not, Test_form_Get], [progn|Forms_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_unless, classof, claz_macro),
   set_opv(unless, compile_as, kw_operator),
   set_opv(unless, function, cl_unless),
   DefMacroResult=unless.
/*
:- side_effect(assert_lsp(unless,
			  wl:lambda_def(defmacro, unless, cl_unless, [sys_test_form, c38_rest, forms], [progn, ['#BQ', [if, [not, ['#COMMA', sys_test_form]], [progn, ['#BQ-COMMA-ELIPSE', forms]]]]]))).
*/
/*
:- side_effect(assert_lsp(unless,
			  wl:arglist_info(unless, cl_unless, [sys_test_form, c38_rest, forms], arginfo{all:[sys_test_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_test_form, forms], opt:0, req:[sys_test_form], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(unless, wl:init_args(1, cl_unless))).
*/
/*
(defmacro and (&rest forms)
  (if forms
      (if (cdr forms)
	  `(when ,(car forms) (and ,@(cdr forms)))
	(car forms))
    `t))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:33194 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,and,['&rest',forms],[if,forms,[if,[cdr,forms],['#BQ',[when,['#COMMA',[car,forms]],[and,['#BQ-COMMA-ELIPSE',[cdr,forms]]]]],[car,forms]],['#BQ',t]]])
wl:lambda_def(defmacro, and, cl_and, [c38_rest, forms], [progn, [if, forms, [if, [cdr, forms], ['#BQ', [when, ['#COMMA', [car, forms]], [and, ['#BQ-COMMA-ELIPSE', [cdr, forms]]]]], [car, forms]], ['#BQ', t]]]).
wl:arglist_info(and, cl_and, [c38_rest, forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[forms], opt:0, req:0, rest:[forms], sublists:0, whole:0}).
wl: init_args(0, cl_and).

/*

### Compiled:  `CL:AND` 
*/
cl_and(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, forms, IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, forms, Forms_Get12),
	    cl_cdr(Forms_Get12, IFTEST10),
	    (   IFTEST10\==[]
	    ->  get_var(GEnv, forms, Forms_Get13),
		cl_car(Forms_Get13, Car_Ret),
		get_var(GEnv, forms, Forms_Get14),
		cl_cdr(Forms_Get14, Cdr_Ret),
		TrueResult=[when, Car_Ret, [and|Cdr_Ret]]
	    ;   get_var(GEnv, forms, Forms_Get15),
		cl_car(Forms_Get15, ElseResult),
		TrueResult=ElseResult
	    )
	;   TrueResult=t
	),
	TrueResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_and, classof, claz_macro),
   set_opv(and, compile_as, kw_operator),
   set_opv(and, function, cl_and),
   DefMacroResult=and.
/*
:- side_effect(assert_lsp(and,
			  wl:lambda_def(defmacro, and, cl_and, [c38_rest, forms], [progn, [if, forms, [if, [cdr, forms], ['#BQ', [when, ['#COMMA', [car, forms]], [and, ['#BQ-COMMA-ELIPSE', [cdr, forms]]]]], [car, forms]], ['#BQ', t]]]))).
*/
/*
:- side_effect(assert_lsp(and,
			  wl:arglist_info(and, cl_and, [c38_rest, forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[forms], opt:0, req:0, rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(and, wl:init_args(0, cl_and))).
*/
/*
(defmacro or (&rest forms)
  (if forms
      (if (cdr forms)
	  (let ((temp (gensym)))
	    `(let ((,temp ,(car forms)))
	      (if ,temp
		  ,temp
		(or ,@(cdr forms)))))
	(car forms))
    `nil))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:33329 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,or,['&rest',forms],[if,forms,[if,[cdr,forms],[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',[car,forms]]]],[if,['#COMMA',temp],['#COMMA',temp],[or,['#BQ-COMMA-ELIPSE',[cdr,forms]]]]]]],[car,forms]],['#BQ',[]]]])
wl:lambda_def(defmacro, or, cl_or, [c38_rest, forms], [progn, [if, forms, [if, [cdr, forms], [let, [[sys_temp, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', [car, forms]]]], [if, ['#COMMA', sys_temp], ['#COMMA', sys_temp], [or, ['#BQ-COMMA-ELIPSE', [cdr, forms]]]]]]], [car, forms]], ['#BQ', []]]]).
wl:arglist_info(or, cl_or, [c38_rest, forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[forms], opt:0, req:0, rest:[forms], sublists:0, whole:0}).
wl: init_args(0, cl_or).

/*

### Compiled:  `CL:OR` 
*/
cl_or(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, forms, IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, forms, Forms_Get12),
	    cl_cdr(Forms_Get12, IFTEST10),
	    (   IFTEST10\==[]
	    ->  cl_gensym(Temp_Init),
		LEnv=[bv(sys_temp, Temp_Init)|GEnv],
		get_var(LEnv, forms, Forms_Get18),
		get_var(LEnv, sys_temp, Temp_Get),
		cl_car(Forms_Get18, Car_Ret),
		get_var(LEnv, forms, Forms_Get21),
		get_var(LEnv, sys_temp, Temp_Get19),
		cl_cdr(Forms_Get21, Cdr_Ret),
		TrueResult=[let, [[Temp_Get, Car_Ret]], [if, Temp_Get19, Temp_Get19, [or|Cdr_Ret]]]
	    ;   get_var(GEnv, forms, Forms_Get22),
		cl_car(Forms_Get22, ElseResult),
		TrueResult=ElseResult
	    )
	;   TrueResult=[]
	),
	TrueResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_or, classof, claz_macro),
   set_opv(or, compile_as, kw_operator),
   set_opv(or, function, cl_or),
   DefMacroResult=or.
/*
:- side_effect(assert_lsp(or,
			  wl:lambda_def(defmacro, or, cl_or, [c38_rest, forms], [progn, [if, forms, [if, [cdr, forms], [let, [[sys_temp, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', [car, forms]]]], [if, ['#COMMA', sys_temp], ['#COMMA', sys_temp], [or, ['#BQ-COMMA-ELIPSE', [cdr, forms]]]]]]], [car, forms]], ['#BQ', []]]]))).
*/
/*
:- side_effect(assert_lsp(or,
			  wl:arglist_info(or, cl_or, [c38_rest, forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[forms], opt:0, req:0, rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(or, wl:init_args(0, cl_or))).
*/
/*
(defmacro cond (&rest clauses)
  (when clauses
    (if (cdar clauses)
	`(if ,(caar clauses)
	     (progn ,@(cdar clauses))
	     (cond ,@(cdr clauses)))
	`(or ,(caar clauses)
	     (cond ,@(cdr clauses))))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:33536 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,cond,['&rest',clauses],[when,clauses,[if,[cdar,clauses],['#BQ',[if,['#COMMA',[caar,clauses]],[progn,['#BQ-COMMA-ELIPSE',[cdar,clauses]]],[cond,['#BQ-COMMA-ELIPSE',[cdr,clauses]]]]],['#BQ',[or,['#COMMA',[caar,clauses]],[cond,['#BQ-COMMA-ELIPSE',[cdr,clauses]]]]]]]])
wl:lambda_def(defmacro, cond, cl_cond, [c38_rest, sys_clauses], [progn, [when, sys_clauses, [if, [cdar, sys_clauses], ['#BQ', [if, ['#COMMA', [caar, sys_clauses]], [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]], [cond, ['#BQ-COMMA-ELIPSE', [cdr, sys_clauses]]]]], ['#BQ', [or, ['#COMMA', [caar, sys_clauses]], [cond, ['#BQ-COMMA-ELIPSE', [cdr, sys_clauses]]]]]]]]).
wl:arglist_info(cond, cl_cond, [c38_rest, sys_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_clauses], opt:0, req:0, rest:[sys_clauses], sublists:0, whole:0}).
wl: init_args(0, cl_cond).

/*

### Compiled:  `CL:COND` 
*/
cl_cond(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(sys_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, sys_clauses, IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, sys_clauses, Clauses_Get12),
	    cl_cdar(Clauses_Get12, IFTEST10),
	    (   IFTEST10\==[]
	    ->  get_var(GEnv, sys_clauses, Clauses_Get13),
		cl_caar(Clauses_Get13, Caar_Ret),
		get_var(GEnv, sys_clauses, Clauses_Get14),
		cl_cdar(Clauses_Get14, Cdar_Ret),
		get_var(GEnv, sys_clauses, Clauses_Get15),
		cl_cdr(Clauses_Get15, Cdr_Ret),
		TrueResult=[if, Caar_Ret, [progn|Cdar_Ret], [cond|Cdr_Ret]]
	    ;   get_var(GEnv, sys_clauses, Clauses_Get16),
		cl_caar(Clauses_Get16, Caar_Ret25),
		get_var(GEnv, sys_clauses, Clauses_Get17),
		cl_cdr(Clauses_Get17, Cdr_Ret26),
		TrueResult=[or, Caar_Ret25, [cond|Cdr_Ret26]]
	    )
	;   TrueResult=[]
	),
	TrueResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_cond, classof, claz_macro),
   set_opv(cond, compile_as, kw_operator),
   set_opv(cond, function, cl_cond),
   DefMacroResult=cond.
/*
:- side_effect(assert_lsp(cond,
			  wl:lambda_def(defmacro, cond, cl_cond, [c38_rest, sys_clauses], [progn, [when, sys_clauses, [if, [cdar, sys_clauses], ['#BQ', [if, ['#COMMA', [caar, sys_clauses]], [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]], [cond, ['#BQ-COMMA-ELIPSE', [cdr, sys_clauses]]]]], ['#BQ', [or, ['#COMMA', [caar, sys_clauses]], [cond, ['#BQ-COMMA-ELIPSE', [cdr, sys_clauses]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(cond,
			  wl:arglist_info(cond, cl_cond, [c38_rest, sys_clauses], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_clauses], opt:0, req:0, rest:[sys_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(cond, wl:init_args(0, cl_cond))).
*/
/*
(defmacro case (keyform &rest clauses)
  (let ((temp (gensym)))
    (labels ((recur (clauses)
	       (when clauses
		 (if (member (caar clauses) '(otherwise t))
		     `(progn ,@(cdar clauses))
		     `(if ,(if (listp (caar clauses))
			       `(member ,temp ',(caar clauses))
			       `(eql ,temp ',(caar clauses)))
		          (progn ,@(cdar clauses))
		          ,(recur (cdr clauses)))))))
      `(let ((,temp ,keyform))
	,(recur clauses)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:33756 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,case,[keyform,'&rest',clauses],[let,[[temp,[gensym]]],[labels,[[recur,[clauses],[when,clauses,[if,[member,[caar,clauses],[quote,[otherwise,t]]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',[cdar,clauses]]]],['#BQ',[if,['#COMMA',[if,[listp,[caar,clauses]],['#BQ',[member,['#COMMA',temp],[quote,['#COMMA',[caar,clauses]]]]],['#BQ',[eql,['#COMMA',temp],[quote,['#COMMA',[caar,clauses]]]]]]],[progn,['#BQ-COMMA-ELIPSE',[cdar,clauses]]],['#COMMA',[recur,[cdr,clauses]]]]]]]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',keyform]]],['#COMMA',[recur,clauses]]]]]]])
wl:lambda_def(defmacro, case, cl_case, [sys_keyform, c38_rest, sys_clauses], [progn, [let, [[sys_temp, [gensym]]], [labels, [[sys_recur, [sys_clauses], [when, sys_clauses, [if, [member, [caar, sys_clauses], [quote, [otherwise, t]]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]]], ['#BQ', [if, ['#COMMA', [if, [listp, [caar, sys_clauses]], ['#BQ', [member, ['#COMMA', sys_temp], [quote, ['#COMMA', [caar, sys_clauses]]]]], ['#BQ', [eql, ['#COMMA', sys_temp], [quote, ['#COMMA', [caar, sys_clauses]]]]]]], [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]], ['#COMMA', [sys_recur, [cdr, sys_clauses]]]]]]]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', sys_keyform]]], ['#COMMA', [sys_recur, sys_clauses]]]]]]]).
wl:arglist_info(case, cl_case, [sys_keyform, c38_rest, sys_clauses], arginfo{all:[sys_keyform], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_keyform, sys_clauses], opt:0, req:[sys_keyform], rest:[sys_clauses], sublists:0, whole:0}).
wl: init_args(1, cl_case).

/*

### Compiled:  `CL:CASE` 
*/
cl_case(Keyform, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(sys_keyform, Keyform), bv(sys_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	cl_gensym(Temp_Init),
	LEnv=[bv(sys_temp, Temp_Init)|CDR],
	must_maplist(must_det_l,
		     
		     [ (assert_lsp(sys_recur, wl:lambda_def(defun, sys_recur, f_sys_recur1, [sys_clauses], [[when, sys_clauses, [if, [member, [caar, sys_clauses], [quote, [otherwise, t]]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]]], ['#BQ', [if, ['#COMMA', [if, [listp, [caar, sys_clauses]], ['#BQ', [member, ['#COMMA', sys_temp], [quote, ['#COMMA', [caar, sys_clauses]]]]], ['#BQ', [eql, ['#COMMA', sys_temp], [quote, ['#COMMA', [caar, sys_clauses]]]]]]], [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]], ['#COMMA', [sys_recur, [cdr, sys_clauses]]]]]]]])), assert_lsp(sys_recur, wl:arglist_info(sys_recur, f_sys_recur1, [sys_clauses], arginfo{all:[sys_clauses], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_clauses], opt:0, req:[sys_clauses], rest:0, sublists:0, whole:0})), !, assert_lsp(sys_recur, wl:init_args(exact_only, f_sys_recur1)), assert_lsp(sys_recur,  (f_sys_recur1(Clauses, FnResult12):-nop(global_env(LEnv)), Env37=[bv(sys_clauses, Clauses)|LEnv], get_var(Env37, sys_clauses, IFTEST), (IFTEST\==[]->get_var(Env37, sys_clauses, Clauses_Get18), cl_caar(Clauses_Get18, Member_Param), cl_member(Member_Param, [otherwise, t], IFTEST16), (IFTEST16\==[]->get_var(Env37, sys_clauses, Clauses_Get19), cl_cdar(Clauses_Get19, Cdar_Ret), FnResult12=[progn|Cdar_Ret];get_var(Env37, sys_clauses, Clauses_Get21), cl_caar(Clauses_Get21, PredArgResult), (is_listp(PredArgResult)->get_var(Env37, sys_clauses, Clauses_Get25), get_var(Env37, sys_temp, Temp_Get), cl_caar(Clauses_Get25, Caar_Ret), CAR=[member, Temp_Get, [quote, Caar_Ret]];get_var(Env37, sys_clauses, Clauses_Get27), get_var(Env37, sys_temp, Temp_Get26), cl_caar(Clauses_Get27, Caar_Ret45), CAR=[eql, Temp_Get26, [quote, Caar_Ret45]]), get_var(Env37, sys_clauses, Clauses_Get28), cl_cdar(Clauses_Get28, Cdar_Ret46), get_var(Env37, sys_clauses, Clauses_Get29), cl_cdr(Clauses_Get29, Recur_Param), f_sys_recur(Recur_Param, Recur_Ret), FnResult12=[if, CAR, [progn|Cdar_Ret46], Recur_Ret]);FnResult12=[]))))
		     ]),
	get_var(LEnv, sys_clauses, Clauses_Get34),
	get_var(LEnv, sys_keyform, Keyform_Get),
	get_var(LEnv, sys_temp, Temp_Get32),
	f_sys_recur1(Clauses_Get34, Recur1_Ret),
	[let, [[Temp_Get32, Keyform_Get]], Recur1_Ret]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_case, classof, claz_macro),
   set_opv(case, compile_as, kw_operator),
   set_opv(case, function, cl_case),
   DefMacroResult=case.
/*
:- side_effect(assert_lsp(case,
			  wl:lambda_def(defmacro, case, cl_case, [sys_keyform, c38_rest, sys_clauses], [progn, [let, [[sys_temp, [gensym]]], [labels, [[sys_recur, [sys_clauses], [when, sys_clauses, [if, [member, [caar, sys_clauses], [quote, [otherwise, t]]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]]], ['#BQ', [if, ['#COMMA', [if, [listp, [caar, sys_clauses]], ['#BQ', [member, ['#COMMA', sys_temp], [quote, ['#COMMA', [caar, sys_clauses]]]]], ['#BQ', [eql, ['#COMMA', sys_temp], [quote, ['#COMMA', [caar, sys_clauses]]]]]]], [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]], ['#COMMA', [sys_recur, [cdr, sys_clauses]]]]]]]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', sys_keyform]]], ['#COMMA', [sys_recur, sys_clauses]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(case,
			  wl:arglist_info(case, cl_case, [sys_keyform, c38_rest, sys_clauses], arginfo{all:[sys_keyform], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_keyform, sys_clauses], opt:0, req:[sys_keyform], rest:[sys_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(case, wl:init_args(1, cl_case))).
*/
/*
(defmacro ecase (keyform &rest clauses)
  (let ((temp (gensym)))
    `(let ((,temp ,keyform))
      (case ,temp ,@clauses
	    (error 'type-error :datum ,temp
		   :expected-type `(member ,@(mapcan #'(lambda (x)
							 (if (listp (car x))
							     (car x)
							     (list (car x))))
						     clauses)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:34222 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,ecase,[keyform,'&rest',clauses],[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',keyform]]],[case,['#COMMA',temp],['#BQ-COMMA-ELIPSE',clauses],[error,[quote,'type-error'],':datum',['#COMMA',temp],':expected-type',['#BQ',[member,['#BQ-COMMA-ELIPSE',[mapcan,function([lambda,[x],[if,[listp,[car,x]],[car,x],[list,[car,x]]]]),clauses]]]]]]]]]])
wl:lambda_def(defmacro, ecase, cl_ecase, [sys_keyform, c38_rest, sys_clauses], [progn, [let, [[sys_temp, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', sys_keyform]]], [case, ['#COMMA', sys_temp], ['#BQ-COMMA-ELIPSE', sys_clauses], [error, [quote, type_error], kw_datum, ['#COMMA', sys_temp], kw_expected_type, ['#BQ', [member, ['#BQ-COMMA-ELIPSE', [mapcan, function([lambda, [sys_x], [if, [listp, [car, sys_x]], [car, sys_x], [list, [car, sys_x]]]]), sys_clauses]]]]]]]]]]).
wl:arglist_info(ecase, cl_ecase, [sys_keyform, c38_rest, sys_clauses], arginfo{all:[sys_keyform], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_keyform, sys_clauses], opt:0, req:[sys_keyform], rest:[sys_clauses], sublists:0, whole:0}).
wl: init_args(1, cl_ecase).

/*

### Compiled:  `CL:ECASE` 
*/
cl_ecase(Keyform, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(sys_keyform, Keyform), bv(sys_clauses, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	cl_gensym(Temp_Init),
	LEnv=[bv(sys_temp, Temp_Init)|CDR],
	get_var(LEnv, sys_clauses, Clauses_Get27),
	get_var(LEnv, sys_keyform, Keyform_Get),
	get_var(LEnv, sys_temp, Temp_Get13),
	cl_mapcan(closure(LEnv25,
			  LResult,
			  [sys_x],
			  (get_var(LEnv25, sys_x, X_Get), cl_car(X_Get, PredArgResult), (is_listp(PredArgResult)->get_var(LEnv25, sys_x, X_Get20), cl_car(X_Get20, TrueResult), LResult=TrueResult;get_var(LEnv25, sys_x, X_Get21), cl_car(X_Get21, Car_Ret), LResult=[Car_Ret]))),
		  Clauses_Get27,
		  Mapcan_Ret),
	bq_append([Temp_Get13|Clauses_Get27],
		  
		  [ 
		    [ error,
		      [quote, type_error],
		      kw_datum,
		      Temp_Get13,
		      kw_expected_type,
		      [quote, [member|Mapcan_Ret]]
		    ]
		  ],
		  Bq_append_Ret),
	[let, [[Temp_Get13, Keyform_Get]], [case|Bq_append_Ret]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_ecase, classof, claz_macro),
   set_opv(ecase, compile_as, kw_operator),
   set_opv(ecase, function, cl_ecase),
   DefMacroResult=ecase.
/*
:- side_effect(assert_lsp(ecase,
			  wl:lambda_def(defmacro, ecase, cl_ecase, [sys_keyform, c38_rest, sys_clauses], [progn, [let, [[sys_temp, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', sys_keyform]]], [case, ['#COMMA', sys_temp], ['#BQ-COMMA-ELIPSE', sys_clauses], [error, [quote, type_error], kw_datum, ['#COMMA', sys_temp], kw_expected_type, ['#BQ', [member, ['#BQ-COMMA-ELIPSE', [mapcan, function([lambda, [sys_x], [if, [listp, [car, sys_x]], [car, sys_x], [list, [car, sys_x]]]]), sys_clauses]]]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(ecase,
			  wl:arglist_info(ecase, cl_ecase, [sys_keyform, c38_rest, sys_clauses], arginfo{all:[sys_keyform], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_keyform, sys_clauses], opt:0, req:[sys_keyform], rest:[sys_clauses], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(ecase, wl:init_args(1, cl_ecase))).
*/
/*
(defmacro multiple-value-bind (vars values-form &rest forms)
  `(multiple-value-call #'(lambda (&optional ,@vars &rest ,(gensym))
			    ,@forms)
                        ,values-form))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:34549 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'multiple-value-bind',[vars,'values-form','&rest',forms],['#BQ',['multiple-value-call',function([lambda,['&optional',['#BQ-COMMA-ELIPSE',vars],'&rest',['#COMMA',[gensym]]],['#BQ-COMMA-ELIPSE',forms]]),['#COMMA','values-form']]]])
wl:lambda_def(defmacro, multiple_value_bind, cl_multiple_value_bind, [sys_vars, sys_values_form, c38_rest, forms], [progn, ['#BQ', [multiple_value_call, function([lambda, [c38_optional, ['#BQ-COMMA-ELIPSE', sys_vars], c38_rest, ['#COMMA', [gensym]]], ['#BQ-COMMA-ELIPSE', forms]]), ['#COMMA', sys_values_form]]]]).
wl:arglist_info(multiple_value_bind, cl_multiple_value_bind, [sys_vars, sys_values_form, c38_rest, forms], arginfo{all:[sys_vars, sys_values_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_vars, sys_values_form, forms], opt:0, req:[sys_vars, sys_values_form], rest:[forms], sublists:0, whole:0}).
wl: init_args(2, cl_multiple_value_bind).

/*

### Compiled:  `CL:MULTIPLE-VALUE-BIND` 
*/
cl_multiple_value_bind(Vars, Values_form, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(sys_vars, Vars), bv(sys_values_form, Values_form), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, sys_values_form, Values_form_Get),
	[multiple_value_call, function([lambda, [c38_optional, ['#BQ-COMMA-ELIPSE', sys_vars], c38_rest, ['#COMMA', [gensym]]], ['#BQ-COMMA-ELIPSE', forms]]), Values_form_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_multiple_value_bind, classof, claz_macro),
   set_opv(multiple_value_bind, compile_as, kw_operator),
   set_opv(multiple_value_bind, function, cl_multiple_value_bind),
   DefMacroResult=multiple_value_bind.
/*
:- side_effect(assert_lsp(multiple_value_bind,
			  wl:lambda_def(defmacro, multiple_value_bind, cl_multiple_value_bind, [sys_vars, sys_values_form, c38_rest, forms], [progn, ['#BQ', [multiple_value_call, function([lambda, [c38_optional, ['#BQ-COMMA-ELIPSE', sys_vars], c38_rest, ['#COMMA', [gensym]]], ['#BQ-COMMA-ELIPSE', forms]]), ['#COMMA', sys_values_form]]]]))).
*/
/*
:- side_effect(assert_lsp(multiple_value_bind,
			  wl:arglist_info(multiple_value_bind, cl_multiple_value_bind, [sys_vars, sys_values_form, c38_rest, forms], arginfo{all:[sys_vars, sys_values_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_vars, sys_values_form, forms], opt:0, req:[sys_vars, sys_values_form], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(multiple_value_bind,
			  wl:init_args(2, cl_multiple_value_bind))).
*/
/*
(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:34738 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'multiple-value-list',[form],['#BQ',['multiple-value-call',function(list),['#COMMA',form]]]])
wl:lambda_def(defmacro, multiple_value_list, cl_multiple_value_list, [sys_form], [progn, ['#BQ', [multiple_value_call, function(list), ['#COMMA', sys_form]]]]).
wl:arglist_info(multiple_value_list, cl_multiple_value_list, [sys_form], arginfo{all:[sys_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form], opt:0, req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_multiple_value_list).

/*

### Compiled:  `CL:MULTIPLE-VALUE-LIST` 
*/
cl_multiple_value_list(Form, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(sys_form, Form)|Global_env_Ret],
	get_var(Env, sys_form, Form_Get),
	[multiple_value_call, function(list), Form_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_multiple_value_list, classof, claz_macro),
   set_opv(multiple_value_list, compile_as, kw_operator),
   set_opv(multiple_value_list, function, cl_multiple_value_list),
   DefMacroResult=multiple_value_list.
/*
:- side_effect(assert_lsp(multiple_value_list,
			  wl:lambda_def(defmacro, multiple_value_list, cl_multiple_value_list, [sys_form], [progn, ['#BQ', [multiple_value_call, function(list), ['#COMMA', sys_form]]]]))).
*/
/*
:- side_effect(assert_lsp(multiple_value_list,
			  wl:arglist_info(multiple_value_list, cl_multiple_value_list, [sys_form], arginfo{all:[sys_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form], opt:0, req:[sys_form], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(multiple_value_list,
			  wl:init_args(exact_only, cl_multiple_value_list))).
*/
/*
(defun values-list (list)
  (apply #'values list))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:34816 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'values-list',[list],[apply,function(values),list]])
wl:lambda_def(defun, values_list, cl_values_list, [list], [[apply, function(values), list]]).
wl:arglist_info(values_list, cl_values_list, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_values_list).

/*

### Compiled:  `CL:VALUES-LIST` 
*/
cl_values_list(List, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(list, List)|Env],
	get_var(Env9, list, List_Get),
	cl_apply(cl_values, List_Get, Apply_Ret),
	Apply_Ret=FnResult.
:- set_opv(cl_values_list, classof, claz_function),
   set_opv(values_list, compile_as, kw_function),
   set_opv(values_list, function, cl_values_list),
   DefunResult=values_list.
/*
:- side_effect(assert_lsp(values_list,
			  wl:lambda_def(defun, values_list, cl_values_list, [list], [[apply, function(values), list]]))).
*/
/*
:- side_effect(assert_lsp(values_list,
			  wl:arglist_info(values_list, cl_values_list, [list], arginfo{all:[list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[list], opt:0, req:[list], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(values_list, wl:init_args(exact_only, cl_values_list))).
*/
/*
(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:34869 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'nth-value',[n,form],['#BQ',[nth,['#COMMA',n],['multiple-value-list',['#COMMA',form]]]]])
wl:lambda_def(defmacro, nth_value, cl_nth_value, [n, sys_form], [progn, ['#BQ', [nth, ['#COMMA', n], [multiple_value_list, ['#COMMA', sys_form]]]]]).
wl:arglist_info(nth_value, cl_nth_value, [n, sys_form], arginfo{all:[n, sys_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n, sys_form], opt:0, req:[n, sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_nth_value).

/*

### Compiled:  `CL:NTH-VALUE` 
*/
cl_nth_value(N, Form, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(n, N), bv(sys_form, Form)|Global_env_Ret],
	get_var(Env, n, N_Get),
	get_var(Env, sys_form, Form_Get),
	[nth, N_Get, [multiple_value_list, Form_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_nth_value, classof, claz_macro),
   set_opv(nth_value, compile_as, kw_operator),
   set_opv(nth_value, function, cl_nth_value),
   DefMacroResult=nth_value.
/*
:- side_effect(assert_lsp(nth_value,
			  wl:lambda_def(defmacro, nth_value, cl_nth_value, [n, sys_form], [progn, ['#BQ', [nth, ['#COMMA', n], [multiple_value_list, ['#COMMA', sys_form]]]]]))).
*/
/*
:- side_effect(assert_lsp(nth_value,
			  wl:arglist_info(nth_value, cl_nth_value, [n, sys_form], arginfo{all:[n, sys_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n, sys_form], opt:0, req:[n, sys_form], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(nth_value, wl:init_args(exact_only, cl_nth_value))).
*/
/*
(defmacro prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:34941 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,prog,[inits,'&rest',forms],['#BQ',[block,[],[let,['#COMMA',inits],[tagbody,['#BQ-COMMA-ELIPSE',forms]]]]]])
wl:lambda_def(defmacro, prog, cl_prog, [sys_inits, c38_rest, forms], [progn, ['#BQ', [block, [], [let, ['#COMMA', sys_inits], [tagbody, ['#BQ-COMMA-ELIPSE', forms]]]]]]).
wl:arglist_info(prog, cl_prog, [sys_inits, c38_rest, forms], arginfo{all:[sys_inits], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_inits, forms], opt:0, req:[sys_inits], rest:[forms], sublists:0, whole:0}).
wl: init_args(1, cl_prog).

/*

### Compiled:  `CL:PROG` 
*/
cl_prog(Inits, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(sys_inits, Inits), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, forms, Forms_Get),
	get_var(GEnv, sys_inits, Inits_Get),
	[block, [], [let, Inits_Get, [tagbody|Forms_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_prog, classof, claz_macro),
   set_opv(prog, compile_as, kw_operator),
   set_opv(prog, function, cl_prog),
   DefMacroResult=prog.
/*
:- side_effect(assert_lsp(prog,
			  wl:lambda_def(defmacro, prog, cl_prog, [sys_inits, c38_rest, forms], [progn, ['#BQ', [block, [], [let, ['#COMMA', sys_inits], [tagbody, ['#BQ-COMMA-ELIPSE', forms]]]]]]))).
*/
/*
:- side_effect(assert_lsp(prog,
			  wl:arglist_info(prog, cl_prog, [sys_inits, c38_rest, forms], arginfo{all:[sys_inits], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_inits, forms], opt:0, req:[sys_inits], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(prog, wl:init_args(1, cl_prog))).
*/
/*
(defmacro prog* (inits &rest forms)
  `(block nil
    (let* ,inits
      (tagbody ,@forms))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:35037 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'prog*',[inits,'&rest',forms],['#BQ',[block,[],['let*',['#COMMA',inits],[tagbody,['#BQ-COMMA-ELIPSE',forms]]]]]])
wl:lambda_def(defmacro, prog_xx, cl_prog_xx, [sys_inits, c38_rest, forms], [progn, ['#BQ', [block, [], [let_xx, ['#COMMA', sys_inits], [tagbody, ['#BQ-COMMA-ELIPSE', forms]]]]]]).
wl:arglist_info(prog_xx, cl_prog_xx, [sys_inits, c38_rest, forms], arginfo{all:[sys_inits], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_inits, forms], opt:0, req:[sys_inits], rest:[forms], sublists:0, whole:0}).
wl: init_args(1, cl_prog_xx).

/*

### Compiled:  `CL:PROG*` 
*/
cl_prog_xx(Inits, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(sys_inits, Inits), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, forms, Forms_Get),
	get_var(GEnv, sys_inits, Inits_Get),
	[block, [], [let_xx, Inits_Get, [tagbody|Forms_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_prog_xx, classof, claz_macro),
   set_opv(prog_xx, compile_as, kw_operator),
   set_opv(prog_xx, function, cl_prog_xx),
   DefMacroResult=prog_xx.
/*
:- side_effect(assert_lsp(prog_xx,
			  wl:lambda_def(defmacro, prog_xx, cl_prog_xx, [sys_inits, c38_rest, forms], [progn, ['#BQ', [block, [], [let_xx, ['#COMMA', sys_inits], [tagbody, ['#BQ-COMMA-ELIPSE', forms]]]]]]))).
*/
/*
:- side_effect(assert_lsp(prog_xx,
			  wl:arglist_info(prog_xx, cl_prog_xx, [sys_inits, c38_rest, forms], arginfo{all:[sys_inits], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_inits, forms], opt:0, req:[sys_inits], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(prog_xx, wl:init_args(1, cl_prog_xx))).
*/
/*
(defmacro prog1 (first-form &rest forms)
  (let ((temp (gensym)))
    `(let ((,temp ,first-form))
      ,@forms
      ,temp)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:35135 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,prog1,['first-form','&rest',forms],[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA','first-form']]],['#BQ-COMMA-ELIPSE',forms],['#COMMA',temp]]]]])
wl:lambda_def(defmacro, prog1, cl_prog1, [sys_first_form, c38_rest, forms], [progn, [let, [[sys_temp, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', sys_first_form]]], ['#BQ-COMMA-ELIPSE', forms], ['#COMMA', sys_temp]]]]]).
wl:arglist_info(prog1, cl_prog1, [sys_first_form, c38_rest, forms], arginfo{all:[sys_first_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_first_form, forms], opt:0, req:[sys_first_form], rest:[forms], sublists:0, whole:0}).
wl: init_args(1, cl_prog1).

/*

### Compiled:  `CL:PROG1` 
*/
cl_prog1(First_form, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(sys_first_form, First_form), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	cl_gensym(Temp_Init),
	LEnv=[bv(sys_temp, Temp_Init)|CDR],
	get_var(LEnv, forms, Forms_Get),
	get_var(LEnv, sys_first_form, First_form_Get),
	get_var(LEnv, sys_temp, Temp_Get14),
	bq_append([[[Temp_Get14, First_form_Get]]|Forms_Get],
		  [Temp_Get14],
		  Bq_append_Ret),
	[let|Bq_append_Ret]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_prog1, classof, claz_macro),
   set_opv(prog1, compile_as, kw_operator),
   set_opv(prog1, function, cl_prog1),
   DefMacroResult=prog1.
/*
:- side_effect(assert_lsp(prog1,
			  wl:lambda_def(defmacro, prog1, cl_prog1, [sys_first_form, c38_rest, forms], [progn, [let, [[sys_temp, [gensym]]], ['#BQ', [let, [[['#COMMA', sys_temp], ['#COMMA', sys_first_form]]], ['#BQ-COMMA-ELIPSE', forms], ['#COMMA', sys_temp]]]]]))).
*/
/*
:- side_effect(assert_lsp(prog1,
			  wl:arglist_info(prog1, cl_prog1, [sys_first_form, c38_rest, forms], arginfo{all:[sys_first_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_first_form, forms], opt:0, req:[sys_first_form], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(prog1, wl:init_args(1, cl_prog1))).
*/
/*
(defmacro prog2 (first-form second-form &rest forms)
  (let ((temp (gensym)))
    `(progn
      ,first-form
      (let ((,temp ,second-form))
	,@forms
	,temp))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:35267 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,prog2,['first-form','second-form','&rest',forms],[let,[[temp,[gensym]]],['#BQ',[progn,['#COMMA','first-form'],[let,[[['#COMMA',temp],['#COMMA','second-form']]],['#BQ-COMMA-ELIPSE',forms],['#COMMA',temp]]]]]])
wl:lambda_def(defmacro, prog2, cl_prog2, [sys_first_form, sys_second_form, c38_rest, forms], [progn, [let, [[sys_temp, [gensym]]], ['#BQ', [progn, ['#COMMA', sys_first_form], [let, [[['#COMMA', sys_temp], ['#COMMA', sys_second_form]]], ['#BQ-COMMA-ELIPSE', forms], ['#COMMA', sys_temp]]]]]]).
wl:arglist_info(prog2, cl_prog2, [sys_first_form, sys_second_form, c38_rest, forms], arginfo{all:[sys_first_form, sys_second_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_first_form, sys_second_form, forms], opt:0, req:[sys_first_form, sys_second_form], rest:[forms], sublists:0, whole:0}).
wl: init_args(2, cl_prog2).

/*

### Compiled:  `CL:PROG2` 
*/
cl_prog2(First_form, Second_form, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(sys_first_form, First_form), bv(sys_second_form, Second_form), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	cl_gensym(Temp_Init),
	LEnv=[bv(sys_temp, Temp_Init)|CDR],
	get_var(LEnv, forms, Forms_Get),
	get_var(LEnv, sys_first_form, First_form_Get),
	get_var(LEnv, sys_second_form, Second_form_Get),
	get_var(LEnv, sys_temp, Temp_Get15),
	bq_append([[[Temp_Get15, Second_form_Get]]|Forms_Get],
		  [Temp_Get15],
		  Bq_append_Ret),
	[progn, First_form_Get, [let|Bq_append_Ret]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_prog2, classof, claz_macro),
   set_opv(prog2, compile_as, kw_operator),
   set_opv(prog2, function, cl_prog2),
   DefMacroResult=prog2.
/*
:- side_effect(assert_lsp(prog2,
			  wl:lambda_def(defmacro, prog2, cl_prog2, [sys_first_form, sys_second_form, c38_rest, forms], [progn, [let, [[sys_temp, [gensym]]], ['#BQ', [progn, ['#COMMA', sys_first_form], [let, [[['#COMMA', sys_temp], ['#COMMA', sys_second_form]]], ['#BQ-COMMA-ELIPSE', forms], ['#COMMA', sys_temp]]]]]]))).
*/
/*
:- side_effect(assert_lsp(prog2,
			  wl:arglist_info(prog2, cl_prog2, [sys_first_form, sys_second_form, c38_rest, forms], arginfo{all:[sys_first_form, sys_second_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_first_form, sys_second_form, forms], opt:0, req:[sys_first_form, sys_second_form], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(prog2, wl:init_args(2, cl_prog2))).
*/
/*
(defun eql (a b)
  (or (eq a b)
      (and (= (ldb '(2 . 0) (ival a)) 3)
	   (= (ldb '(2 . 0) (ival b)) 3)
	   (= (jref a 1) 84)
	   (= (jref b 1) 84)
	   (= a b))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:35436 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,eql,[a,b],[or,[eq,a,b],[and,[=,[ldb,[quote,[2|0]],[ival,a]],3],[=,[ldb,[quote,[2|0]],[ival,b]],3],[=,[jref,a,1],84],[=,[jref,b,1],84],[=,a,b]]]])
wl:lambda_def(defun, eql, cl_eql, [sys_a, sys_b], [[or, [eq, sys_a, sys_b], [and, [=, [ldb, [quote, [2|0]], [sys_ival, sys_a]], 3], [=, [ldb, [quote, [2|0]], [sys_ival, sys_b]], 3], [=, [sys_jref, sys_a, 1], 84], [=, [sys_jref, sys_b, 1], 84], [=, sys_a, sys_b]]]]).
wl:arglist_info(eql, cl_eql, [sys_a, sys_b], arginfo{all:[sys_a, sys_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_a, sys_b], opt:0, req:[sys_a, sys_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_eql).

/*

### Compiled:  `CL:EQL` 
*/
cl_eql(A, B, TrueResult27) :-
	nop(global_env(Env)),
	Env33=[bv(sys_a, A), bv(sys_b, B)|Env],
	(   get_var(Env33, sys_a, A_Get),
	    get_var(Env33, sys_b, B_Get),
	    cl_eq(A_Get, B_Get, FORM1_Res),
	    FORM1_Res\==[],
	    TrueResult27=FORM1_Res
	->  true
	;   get_var(Env33, sys_a, A_Get9),
	    f_sys_ival(A_Get9, Ival_Ret),
	    cl_ldb([2|0], Ival_Ret, PredArg1Result),
	    (   PredArg1Result=:=3
	    ->  get_var(Env33, sys_b, B_Get13),
		f_sys_ival(B_Get13, Ival_Ret37),
		cl_ldb([2|0], Ival_Ret37, PredArg1Result15),
		(   PredArg1Result15=:=3
		->  get_var(Env33, sys_a, A_Get17),
		    f_sys_jref(A_Get17, 1, PredArg1Result19),
		    (   PredArg1Result19=:=84
		    ->  get_var(Env33, sys_b, B_Get21),
			f_sys_jref(B_Get21, 1, PredArg1Result23),
			(   PredArg1Result23=:=84
			->  get_var(Env33, sys_a, A_Get24),
			    get_var(Env33, sys_b, B_Get25),
			    =(A_Get24, B_Get25, TrueResult),
			    TrueResult27=TrueResult
			;   TrueResult27=[]
			)
		    ;   TrueResult27=[]
		    )
		;   TrueResult27=[]
		)
	    ;   TrueResult27=[]
	    )
	).
:- set_opv(cl_eql, classof, claz_function),
   set_opv(eql, compile_as, kw_function),
   set_opv(eql, function, cl_eql),
   DefunResult=eql.
/*
:- side_effect(assert_lsp(eql,
			  wl:lambda_def(defun, eql, cl_eql, [sys_a, sys_b], [[or, [eq, sys_a, sys_b], [and, [=, [ldb, [quote, [2|0]], [sys_ival, sys_a]], 3], [=, [ldb, [quote, [2|0]], [sys_ival, sys_b]], 3], [=, [sys_jref, sys_a, 1], 84], [=, [sys_jref, sys_b, 1], 84], [=, sys_a, sys_b]]]]))).
*/
/*
:- side_effect(assert_lsp(eql,
			  wl:arglist_info(eql, cl_eql, [sys_a, sys_b], arginfo{all:[sys_a, sys_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_a, sys_b], opt:0, req:[sys_a, sys_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(eql, wl:init_args(exact_only, cl_eql))).
*/
/*
(defun equal (a b)
  (or (eql a b)
      (cond
	((not a) nil)
	((consp a) (and (consp b)
			(equal (car a) (car b))
			(equal (cdr a) (cdr b))))
	((stringp a) (and (stringp b)
			  (string= a b)))
	((bit-vector-p a) (and (bit-vector-p b)
			       (= (length a) (length b))
			       (dotimes (i (length a) t)
				 (when (/= (aref a i) (aref b i))
				   (return))))))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:35609 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,equal,[a,b],[or,[eql,a,b],[cond,[[not,a],[]],[[consp,a],[and,[consp,b],[equal,[car,a],[car,b]],[equal,[cdr,a],[cdr,b]]]],[[stringp,a],[and,[stringp,b],['string=',a,b]]],[['bit-vector-p',a],[and,['bit-vector-p',b],[=,[length,a],[length,b]],[dotimes,[i,[length,a],t],[when,[/=,[aref,a,i],[aref,b,i]],[return]]]]]]]])
wl:lambda_def(defun, equal, cl_equal, [sys_a, sys_b], [[or, [eql, sys_a, sys_b], [cond, [[not, sys_a], []], [[consp, sys_a], [and, [consp, sys_b], [equal, [car, sys_a], [car, sys_b]], [equal, [cdr, sys_a], [cdr, sys_b]]]], [[stringp, sys_a], [and, [stringp, sys_b], [string_c61, sys_a, sys_b]]], [[bit_vector_p, sys_a], [and, [bit_vector_p, sys_b], [=, [length, sys_a], [length, sys_b]], [dotimes, [sys_i, [length, sys_a], t], [when, [/=, [aref, sys_a, sys_i], [aref, sys_b, sys_i]], [return]]]]]]]]).
wl:arglist_info(equal, cl_equal, [sys_a, sys_b], arginfo{all:[sys_a, sys_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_a, sys_b], opt:0, req:[sys_a, sys_b], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_equal).

/*

### Compiled:  `CL:EQUAL` 
*/
cl_equal(A, B, TrueResult29) :-
	nop(global_env(Env)),
	Env65=[bv(sys_a, A), bv(sys_b, B)|Env],
	(   get_var(Env65, sys_a, A_Get),
	    get_var(Env65, sys_b, B_Get),
	    cl_eql(A_Get, B_Get, FORM1_Res),
	    FORM1_Res\==[],
	    TrueResult29=FORM1_Res
	->  true
	;   get_var(Env65, sys_a, A_Get9),
	    (   A_Get9==[]
	    ->  TrueResult29=[]
	    ;   get_var(Env65, sys_a, A_Get13),
		(   is_consp(A_Get13)
		->  get_var(Env65, sys_b, B_Get17),
		    (   is_consp(B_Get17)
		    ->  get_var(Env65, sys_a, A_Get21),
			cl_car(A_Get21, PredArg1Result),
			get_var(Env65, sys_b, B_Get22),
			cl_car(B_Get22, PredArg2Result),
			(   is_equal(PredArg1Result, PredArg2Result)
			->  get_var(Env65, sys_a, A_Get26),
			    cl_cdr(A_Get26, Equal_Param),
			    get_var(Env65, sys_b, B_Get27),
			    cl_cdr(B_Get27, Cdr_Ret),
			    cl_equal(Equal_Param, Cdr_Ret, TrueResult),
			    TrueResult29=TrueResult
			;   TrueResult29=[]
			)
		    ;   TrueResult29=[]
		    )
		;   get_var(Env65, sys_a, A_Get31),
		    (   is_stringp(A_Get31)
		    ->  get_var(Env65, sys_b, B_Get35),
			(   is_stringp(B_Get35)
			->  get_var(Env65, sys_a, A_Get38),
			    get_var(Env65, sys_b, B_Get39),
			    cl_string_c61(A_Get38, B_Get39, [], TrueResult40),
			    TrueResult29=TrueResult40
			;   TrueResult29=[]
			)
		    ;   get_var(Env65, sys_a, A_Get43),
			cl_bit_vector_p(A_Get43, IFTEST41),
			(   IFTEST41\==[]
			->  get_var(Env65, sys_b, B_Get46),
			    cl_bit_vector_p(B_Get46, IFTEST44),
			    (   IFTEST44\==[]
			    ->  get_var(Env65, sys_a, A_Get48),
				cl_length(A_Get48, PredArg1Result51),
				get_var(Env65, sys_b, B_Get49),
				cl_length(B_Get49, PredArg2Result52),
				(   PredArg1Result51=:=PredArg2Result52
				->  cl_dotimes([sys_i, [length, sys_a], t],
					       
					       [ when,
						 
						 [ /=,
						   [aref, sys_a, sys_i],
						   [aref, sys_b, sys_i]
						 ],
						 [return]
					       ],
					       TrueResult53),
				    TrueResult29=TrueResult53
				;   TrueResult29=[]
				)
			    ;   TrueResult29=[]
			    )
			;   TrueResult29=[]
			)
		    )
		)
	    )
	).
:- set_opv(cl_equal, classof, claz_function),
   set_opv(equal, compile_as, kw_function),
   set_opv(equal, function, cl_equal),
   DefunResult=equal.
/*
:- side_effect(assert_lsp(equal,
			  wl:lambda_def(defun, equal, cl_equal, [sys_a, sys_b], [[or, [eql, sys_a, sys_b], [cond, [[not, sys_a], []], [[consp, sys_a], [and, [consp, sys_b], [equal, [car, sys_a], [car, sys_b]], [equal, [cdr, sys_a], [cdr, sys_b]]]], [[stringp, sys_a], [and, [stringp, sys_b], [string_c61, sys_a, sys_b]]], [[bit_vector_p, sys_a], [and, [bit_vector_p, sys_b], [=, [length, sys_a], [length, sys_b]], [dotimes, [sys_i, [length, sys_a], t], [when, [/=, [aref, sys_a, sys_i], [aref, sys_b, sys_i]], [return]]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(equal,
			  wl:arglist_info(equal, cl_equal, [sys_a, sys_b], arginfo{all:[sys_a, sys_b], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_a, sys_b], opt:0, req:[sys_a, sys_b], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(equal, wl:init_args(exact_only, cl_equal))).
*/
/*
(defun identity (object) object)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:35994 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,identity,[object],object])
wl:lambda_def(defun, identity, cl_identity, [sys_object], [sys_object]).
wl:arglist_info(identity, cl_identity, [sys_object], arginfo{all:[sys_object], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object], opt:0, req:[sys_object], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_identity).

/*

### Compiled:  `CL:IDENTITY` 
*/
cl_identity(Object, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(sys_object, Object)|Env],
	get_var(Env9, sys_object, Object_Get),
	Object_Get=FnResult.
:- set_opv(cl_identity, classof, claz_function),
   set_opv(identity, compile_as, kw_function),
   set_opv(identity, function, cl_identity),
   DefunResult=identity.
/*
:- side_effect(assert_lsp(identity,
			  wl:lambda_def(defun, identity, cl_identity, [sys_object], [sys_object]))).
*/
/*
:- side_effect(assert_lsp(identity,
			  wl:arglist_info(identity, cl_identity, [sys_object], arginfo{all:[sys_object], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object], opt:0, req:[sys_object], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(identity, wl:init_args(exact_only, cl_identity))).
*/
/*
(defun complement (function)
  #'(lambda (&rest rest) (not (apply function rest))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:36028 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,complement,[function],function([lambda,['&rest',rest],[not,[apply,function,rest]]])])
wl:lambda_def(defun, complement, cl_complement, [function], [function([lambda, [c38_rest, rest], [not, [apply, function, rest]]])]).
wl:arglist_info(complement, cl_complement, [function], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[function], opt:0, req:[function], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_complement).

/*

### Compiled:  `CL:COMPLEMENT` 
*/
cl_complement(Function13, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(function, Function13)|Env],
	closure(LEnv, LResult, [c38_rest, rest],  (get_var(LEnv, rest, Rest_Get), cl_apply(function, Rest_Get, Not_Param), cl_not(Not_Param, LResult)))=FnResult.
:- set_opv(cl_complement, classof, claz_function),
   set_opv(complement, compile_as, kw_function),
   set_opv(complement, function, cl_complement),
   DefunResult=complement.
/*
:- side_effect(assert_lsp(complement,
			  wl:lambda_def(defun, complement, cl_complement, [function], [function([lambda, [c38_rest, rest], [not, [apply, function, rest]]])]))).
*/
/*
:- side_effect(assert_lsp(complement,
			  wl:arglist_info(complement, cl_complement, [function], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[function], opt:0, req:[function], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(complement, wl:init_args(exact_only, cl_complement))).
*/
/*
(defun constantly (value) #'(lambda (&rest rest) value))

#|


(defmacro dotimes ((var count-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(count (gensym)))
    `(block nil
      (let ((,var 0)
	    (,count ,count-form))
	(tagbody
	   ,start
	   (when (< ,var ,count)
	     ,@forms
	     (incf ,var)
	     (go ,start)))
	,result-form))))



(defmacro do (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let ,(dolist (var vars (reverse inits))
	    (push (if (consp var)
		      (list (car var) (cadr var))
		      (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((psetq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))
(defmacro do* (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let* ,(dolist (var vars (reverse inits))
	     (push (if (consp var)
		       (list (car var) (cadr var))
		       (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((setq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))



(defmacro dolist ((var list-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(list (gensym)))
    `(block nil
      (let ((,list ,list-form)
	    (,var nil))
	(tagbody
	   ,start
	   (unless ,list
	     (setf ,var nil)
	     (return-from nil ,result-form))
	   (setf ,var (car ,list))
	   (setf ,list (cdr ,list))
	   ,@forms
	   (go ,start))))))

|#

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:36114 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,constantly,[value],function([lambda,['&rest',rest],value])])
wl:lambda_def(defun, constantly, cl_constantly, [sys_value], [function([lambda, [c38_rest, rest], sys_value])]).
wl:arglist_info(constantly, cl_constantly, [sys_value], arginfo{all:[sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value], opt:0, req:[sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_constantly).

/*

### Compiled:  `CL:CONSTANTLY` 
*/
cl_constantly(Value, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(sys_value, Value)|Env],
	closure(LEnv, Value_Get, [c38_rest, rest], get_var(LEnv, sys_value, Value_Get))=FnResult.
:- set_opv(cl_constantly, classof, claz_function),
   set_opv(constantly, compile_as, kw_function),
   set_opv(constantly, function, cl_constantly),
   DefunResult=constantly.
/*
:- side_effect(assert_lsp(constantly,
			  wl:lambda_def(defun, constantly, cl_constantly, [sys_value], [function([lambda, [c38_rest, rest], sys_value])]))).
*/
/*
:- side_effect(assert_lsp(constantly,
			  wl:arglist_info(constantly, cl_constantly, [sys_value], arginfo{all:[sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value], opt:0, req:[sys_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(constantly, wl:init_args(exact_only, cl_constantly))).
*/
/*



(defmacro dotimes ((var count-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(count (gensym)))
    `(block nil
      (let ((,var 0)
	    (,count ,count-form))
	(tagbody
	   ,start
	   (when (< ,var ,count)
	     ,@forms
	     (incf ,var)
	     (go ,start)))
	,result-form))))



(defmacro do (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let ,(dolist (var vars (reverse inits))
	    (push (if (consp var)
		      (list (car var) (cadr var))
		      (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((psetq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))
(defmacro do* (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let* ,(dolist (var vars (reverse inits))
	     (push (if (consp var)
		       (list (car var) (cadr var))
		       (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((setq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))



(defmacro dolist ((var list-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(list (gensym)))
    `(block nil
      (let ((,list ,list-form)
	    (,var nil))
	(tagbody
	   ,start
	   (unless ,list
	     (setf ,var nil)
	     (return-from nil ,result-form))
	   (setf ,var (car ,list))
	   (setf ,list (cdr ,list))
	   ,@forms
	   (go ,start))))))

*/
/*
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:38027 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'check-type',[place,typespec,'&optional',string],['#BQ',[tagbody,start,[unless,[typep,['#COMMA',place],[quote,['#COMMA',typespec]]],['restart-case',[error,[quote,'type-error'],':datum',['#COMMA',place],':expected-type',[quote,['#COMMA',typespec]]],['store-value',[value],[setf,['#COMMA',place],value]]],[go,start]]]]])
wl:lambda_def(defmacro, check_type, cl_check_type, [sys_place, sys_typespec, c38_optional, string], [progn, ['#BQ', [tagbody, start, [unless, [typep, ['#COMMA', sys_place], [quote, ['#COMMA', sys_typespec]]], [restart_case, [error, [quote, type_error], kw_datum, ['#COMMA', sys_place], kw_expected_type, [quote, ['#COMMA', sys_typespec]]], [store_value, [sys_value], [setf, ['#COMMA', sys_place], sys_value]]], [go, start]]]]]).
wl:arglist_info(check_type, cl_check_type, [sys_place, sys_typespec, c38_optional, string], arginfo{all:[sys_place, sys_typespec, string], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_typespec, string], opt:[string], req:[sys_place, sys_typespec], rest:0, sublists:0, whole:0}).
wl: init_args(2, cl_check_type).

/*

### Compiled:  `CL:CHECK-TYPE` 
*/
cl_check_type(Place, Typespec, RestNKeys, FnResult) :-
	global_env(Opt_var_Param),
	GEnv=[[[bv(sys_place, Place), bv(sys_typespec, Typespec), bv(string, String)]|Opt_var_Param]|Opt_var_Param],
	opt_var(Opt_var_Param, string, String, true, [], 1, RestNKeys),
	get_var(GEnv, sys_place, Place_Get10),
	get_var(GEnv, sys_typespec, Typespec_Get11),
	[tagbody, start, [unless, [typep, Place_Get10, [quote, Typespec_Get11]], [restart_case, [error, [quote, type_error], kw_datum, Place_Get10, kw_expected_type, [quote, Typespec_Get11]], [store_value, [sys_value], [setf, Place_Get10, sys_value]]], [go, start]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_check_type, classof, claz_macro),
   set_opv(check_type, compile_as, kw_operator),
   set_opv(check_type, function, cl_check_type),
   DefMacroResult=check_type.
/*
:- side_effect(assert_lsp(check_type,
			  wl:lambda_def(defmacro, check_type, cl_check_type, [sys_place, sys_typespec, c38_optional, string], [progn, ['#BQ', [tagbody, start, [unless, [typep, ['#COMMA', sys_place], [quote, ['#COMMA', sys_typespec]]], [restart_case, [error, [quote, type_error], kw_datum, ['#COMMA', sys_place], kw_expected_type, [quote, ['#COMMA', sys_typespec]]], [store_value, [sys_value], [setf, ['#COMMA', sys_place], sys_value]]], [go, start]]]]]))).
*/
/*
:- side_effect(assert_lsp(check_type,
			  wl:arglist_info(check_type, cl_check_type, [sys_place, sys_typespec, c38_optional, string], arginfo{all:[sys_place, sys_typespec, string], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_typespec, string], opt:[string], req:[sys_place, sys_typespec], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(check_type, wl:init_args(2, cl_check_type))).
*/
/*
(defun designator-condition (default-type datum arguments)
  (if (symbolp datum)
      (apply #'make-condition datum arguments)
      (if (or (stringp datum) (functionp datum))
	  (make-condition default-type
			  :format-control datum
			  :format-arguments arguments)
	  datum)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:38301 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-condition',['default-type',datum,arguments],[if,[symbolp,datum],[apply,function('make-condition'),datum,arguments],[if,[or,[stringp,datum],[functionp,datum]],['make-condition','default-type',':format-control',datum,':format-arguments',arguments],datum]]])
wl:lambda_def(defun, sys_designator_condition, f_sys_designator_condition, [sys_default_type, sys_datum, sys_arguments], [[if, [symbolp, sys_datum], [apply, function(make_condition), sys_datum, sys_arguments], [if, [or, [stringp, sys_datum], [functionp, sys_datum]], [make_condition, sys_default_type, kw_format_control, sys_datum, kw_format_arguments, sys_arguments], sys_datum]]]).
wl:arglist_info(sys_designator_condition, f_sys_designator_condition, [sys_default_type, sys_datum, sys_arguments], arginfo{all:[sys_default_type, sys_datum, sys_arguments], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_default_type, sys_datum, sys_arguments], opt:0, req:[sys_default_type, sys_datum, sys_arguments], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_designator_condition).

/*

### Compiled:  `SYS::DESIGNATOR-CONDITION` 
*/
f_sys_designator_condition(Default_type, Datum, Arguments, ElseResult24) :-
	nop(global_env(Env)),
	Env27=[bv(sys_default_type, Default_type), bv(sys_datum, Datum), bv(sys_arguments, Arguments)|Env],
	get_var(Env27, sys_datum, Datum_Get),
	(   is_symbolp(Datum_Get)
	->  get_var(Env27, sys_arguments, Arguments_Get),
	    get_var(Env27, sys_datum, Datum_Get10),
	    cl_apply(cl_make_condition,
		     [Datum_Get10, Arguments_Get],
		     TrueResult23),
	    ElseResult24=TrueResult23
	;   (   get_var(Env27, sys_datum, Datum_Get14),
		cl_stringp(Datum_Get14, FORM1_Res),
		FORM1_Res\==[],
		IFTEST12=FORM1_Res
	    ->  true
	    ;   get_var(Env27, sys_datum, Datum_Get15),
		cl_functionp(Datum_Get15, Functionp_Ret),
		IFTEST12=Functionp_Ret
	    ),
	    (   IFTEST12\==[]
	    ->  get_var(Env27, sys_arguments, Arguments_Get19),
		get_var(Env27, sys_datum, Datum_Get18),
		get_var(Env27, sys_default_type, Default_type_Get),
		cl_make_condition(Default_type_Get,
				  kw_format_control,
				  Datum_Get18,
				  kw_format_arguments,
				  Arguments_Get19,
				  TrueResult),
		ElseResult24=TrueResult
	    ;   get_var(Env27, sys_datum, Datum_Get20),
		ElseResult24=Datum_Get20
	    )
	).
:- set_opv(f_sys_designator_condition, classof, claz_function),
   set_opv(sys_designator_condition, compile_as, kw_function),
   set_opv(sys_designator_condition, function, f_sys_designator_condition),
   DefunResult=sys_designator_condition.
/*
:- side_effect(assert_lsp(sys_designator_condition,
			  wl:lambda_def(defun, sys_designator_condition, f_sys_designator_condition, [sys_default_type, sys_datum, sys_arguments], [[if, [symbolp, sys_datum], [apply, function(make_condition), sys_datum, sys_arguments], [if, [or, [stringp, sys_datum], [functionp, sys_datum]], [make_condition, sys_default_type, kw_format_control, sys_datum, kw_format_arguments, sys_arguments], sys_datum]]]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_condition,
			  wl:arglist_info(sys_designator_condition, f_sys_designator_condition, [sys_default_type, sys_datum, sys_arguments], arginfo{all:[sys_default_type, sys_datum, sys_arguments], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_default_type, sys_datum, sys_arguments], opt:0, req:[sys_default_type, sys_datum, sys_arguments], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_designator_condition,
			  wl:init_args(exact_only, f_sys_designator_condition))).
*/
/*
(defun error (datum &rest arguments)
  (let ((condition (designator-condition 'simple-error datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    (invoke-debugger condition)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:38591 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,error,[datum,'&rest',arguments],[let,[[condition,['designator-condition',[quote,'simple-error'],datum,arguments]]],[when,[typep,condition,'*break-on-signals*'],['invoke-debugger',condition]],['invoke-handler',condition],['invoke-debugger',condition]]])
wl:lambda_def(defun, error, cl_error, [sys_datum, c38_rest, sys_arguments], [[let, [[condition, [sys_designator_condition, [quote, simple_error], sys_datum, sys_arguments]]], [when, [typep, condition, xx_break_on_signals_xx], [invoke_debugger, condition]], [sys_invoke_handler, condition], [invoke_debugger, condition]]]).
wl:arglist_info(error, cl_error, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(rest_only, cl_error).

/*

### Compiled:  `CL:ERROR` 
*/
cl_error(Error_Param, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_datum, Datum), bv(sys_arguments, RestNKeys)]|Env]|Env],
	append([Datum], RestNKeys, Error_Param),
	get_var(GEnv, sys_arguments, Arguments_Get),
	get_var(GEnv, sys_datum, Datum_Get),
	f_sys_designator_condition(simple_error,
				   Datum_Get,
				   Arguments_Get,
				   Condition_Init),
	LEnv=[bv(condition, Condition_Init)|GEnv],
	get_var(LEnv, condition, Condition_Get),
	get_var(LEnv, xx_break_on_signals_xx, Xx_break_on_signals_xx_Get),
	cl_typep(Condition_Get, Xx_break_on_signals_xx_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, condition, Condition_Get17),
	    cl_invoke_debugger(Condition_Get17, TrueResult),
	    _269286680=TrueResult
	;   _269286680=[]
	),
	get_var(LEnv, condition, Condition_Get19),
	f_sys_invoke_handler(Condition_Get19, Invoke_handler_Ret),
	get_var(LEnv, condition, Condition_Get20),
	cl_invoke_debugger(Condition_Get20, LetResult),
	LetResult=FnResult.
:- set_opv(cl_error, classof, claz_function),
   set_opv(error, compile_as, kw_function),
   set_opv(error, function, cl_error),
   DefunResult=error.
/*
:- side_effect(assert_lsp(error,
			  wl:lambda_def(defun, error, cl_error, [sys_datum, c38_rest, sys_arguments], [[let, [[condition, [sys_designator_condition, [quote, simple_error], sys_datum, sys_arguments]]], [when, [typep, condition, xx_break_on_signals_xx], [invoke_debugger, condition]], [sys_invoke_handler, condition], [invoke_debugger, condition]]]))).
*/
/*
:- side_effect(assert_lsp(error,
			  wl:arglist_info(error, cl_error, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(error, wl:init_args(rest_only, cl_error))).
*/
/*
(defun cerror (continue-format-control datum &rest arguments)
  `(with-simple-restart (continue continue-format-control)
    (apply #'error datum arguments)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:38855 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,cerror,['continue-format-control',datum,'&rest',arguments],['#BQ',['with-simple-restart',[continue,'continue-format-control'],[apply,function(error),datum,arguments]]]])
wl:lambda_def(defun, cerror, cl_cerror, [sys_continue_format_control, sys_datum, c38_rest, sys_arguments], [['#BQ', [with_simple_restart, [continue, sys_continue_format_control], [apply, function(error), sys_datum, sys_arguments]]]]).
wl:arglist_info(cerror, cl_cerror, [sys_continue_format_control, sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_continue_format_control, sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_continue_format_control, sys_datum, sys_arguments], opt:0, req:[sys_continue_format_control, sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(2, cl_cerror).

/*

### Compiled:  `CL:CERROR` 
*/
cl_cerror(Continue_format_control, Datum, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	_270643278=[[[bv(sys_continue_format_control, Continue_format_control), bv(sys_datum, Datum), bv(sys_arguments, RestNKeys)]|Env]|Env],
	[with_simple_restart, [continue, sys_continue_format_control], [apply, function(error), sys_datum, sys_arguments]]=FnResult.
:- set_opv(cl_cerror, classof, claz_function),
   set_opv(cerror, compile_as, kw_function),
   set_opv(cerror, function, cl_cerror),
   DefunResult=cerror.
/*
:- side_effect(assert_lsp(cerror,
			  wl:lambda_def(defun, cerror, cl_cerror, [sys_continue_format_control, sys_datum, c38_rest, sys_arguments], [['#BQ', [with_simple_restart, [continue, sys_continue_format_control], [apply, function(error), sys_datum, sys_arguments]]]]))).
*/
/*
:- side_effect(assert_lsp(cerror,
			  wl:arglist_info(cerror, cl_cerror, [sys_continue_format_control, sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_continue_format_control, sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_continue_format_control, sys_datum, sys_arguments], opt:0, req:[sys_continue_format_control, sys_datum], rest:[sys_arguments], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(cerror, wl:init_args(2, cl_cerror))).
*/
/*
(defun signal (datum &rest arguments)
  (let ((condition (designator-condition 'simple-condition datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    nil))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:39017 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,signal,[datum,'&rest',arguments],[let,[[condition,['designator-condition',[quote,'simple-condition'],datum,arguments]]],[when,[typep,condition,'*break-on-signals*'],['invoke-debugger',condition]],['invoke-handler',condition],[]]])
wl:lambda_def(defun, signal, cl_signal, [sys_datum, c38_rest, sys_arguments], [[let, [[condition, [sys_designator_condition, [quote, simple_condition], sys_datum, sys_arguments]]], [when, [typep, condition, xx_break_on_signals_xx], [invoke_debugger, condition]], [sys_invoke_handler, condition], []]]).
wl:arglist_info(signal, cl_signal, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(1, cl_signal).

/*

### Compiled:  `CL:SIGNAL` 
*/
cl_signal(Datum, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_datum, Datum), bv(sys_arguments, RestNKeys)]|Env]|Env],
	get_var(GEnv, sys_arguments, Arguments_Get),
	get_var(GEnv, sys_datum, Datum_Get),
	f_sys_designator_condition(simple_condition,
				   Datum_Get,
				   Arguments_Get,
				   Condition_Init),
	LEnv=[bv(condition, Condition_Init)|GEnv],
	get_var(LEnv, condition, Condition_Get),
	get_var(LEnv, xx_break_on_signals_xx, Xx_break_on_signals_xx_Get),
	cl_typep(Condition_Get, Xx_break_on_signals_xx_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, condition, Condition_Get17),
	    cl_invoke_debugger(Condition_Get17, TrueResult),
	    _271601688=TrueResult
	;   _271601688=[]
	),
	get_var(LEnv, condition, Condition_Get19),
	f_sys_invoke_handler(Condition_Get19, Invoke_handler_Ret),
	[]=FnResult.
:- set_opv(cl_signal, classof, claz_function),
   set_opv(signal, compile_as, kw_function),
   set_opv(signal, function, cl_signal),
   DefunResult=signal.
/*
:- side_effect(assert_lsp(signal,
			  wl:lambda_def(defun, signal, cl_signal, [sys_datum, c38_rest, sys_arguments], [[let, [[condition, [sys_designator_condition, [quote, simple_condition], sys_datum, sys_arguments]]], [when, [typep, condition, xx_break_on_signals_xx], [invoke_debugger, condition]], [sys_invoke_handler, condition], []]]))).
*/
/*
:- side_effect(assert_lsp(signal,
			  wl:arglist_info(signal, cl_signal, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(signal, wl:init_args(1, cl_signal))).
*/
/*
(defun warn (datum &rest arguments)
  (restart-case
      (let ((warning (if (symbolp datum)
			 (apply #'make-condition 'warning datum arguments)
			 datum)))
	(signal warning)
	(print-object warning *error-output*))
    (muffle-warning () nil))
  nil)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:39262 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,warn,[datum,'&rest',arguments],['restart-case',[let,[[warning,[if,[symbolp,datum],[apply,function('make-condition'),[quote,warning],datum,arguments],datum]]],[signal,warning],['print-object',warning,'*error-output*']],['muffle-warning',[],[]]],[]])
wl:lambda_def(defun, warn, cl_warn, [sys_datum, c38_rest, sys_arguments], [[restart_case, [let, [[warning, [if, [symbolp, sys_datum], [apply, function(make_condition), [quote, warning], sys_datum, sys_arguments], sys_datum]]], [signal, warning], [print_object, warning, xx_error_output_xx]], [muffle_warning, [], []]], []]).
wl:arglist_info(warn, cl_warn, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(1, cl_warn).

/*

### Compiled:  `CL:WARN` 
*/
cl_warn(Datum, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	_272881174=[[[bv(sys_datum, Datum), bv(sys_arguments, RestNKeys)]|Env]|Env],
	cl_restart_case(
			[ let,
			  
			  [ 
			    [ warning,
			      
			      [ if,
				[symbolp, sys_datum],
				
				[ apply,
				  function(make_condition),
				  [quote, warning],
				  sys_datum,
				  sys_arguments
				],
				sys_datum
			      ]
			    ]
			  ],
			  [signal, warning],
			  [print_object, warning, xx_error_output_xx]
			],
			[muffle_warning, [], []],
			Restart_case_Ret),
	[]=FnResult.
:- set_opv(cl_warn, classof, claz_function),
   set_opv(warn, compile_as, kw_function),
   set_opv(warn, function, cl_warn),
   DefunResult=warn.
/*
:- side_effect(assert_lsp(warn,
			  wl:lambda_def(defun, warn, cl_warn, [sys_datum, c38_rest, sys_arguments], [[restart_case, [let, [[warning, [if, [symbolp, sys_datum], [apply, function(make_condition), [quote, warning], sys_datum, sys_arguments], sys_datum]]], [signal, warning], [print_object, warning, xx_error_output_xx]], [muffle_warning, [], []]], []]))).
*/
/*
:- side_effect(assert_lsp(warn,
			  wl:arglist_info(warn, cl_warn, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(warn, wl:init_args(1, cl_warn))).
*/
/*
'(defun invoke-debugger (condition)
  (let ((debugger-hook *debugger-hook*)
	(*debugger-hook* nil))
    (when debugger-hook
      (funcall debugger-hook condition debugger-hook))
    (format *debug-io* "Entering debugger."'(defun invoke-debugger (condition)\r\n  (let ((debugger-hook *debugger-hook*)\r\n\t(*debugger-hook* nil))\r\n    (when debugger-hook\r\n      (funcall debugger-hook condition debugger-hook))\r\n    (format *debug-io* \"Entering debugger.~%\")\r\n    (princ condition *debug-io*)\r\n    (terpri *debug-io*)\r\n    (let ((restarts (compute-restarts condition))\r\n\t  (stack (makef))\r\n\t  (frame-depth 0)\r\n\t  (active-frame nil))\r\n      (let ((count 0))\r\n\t(dolist (restart restarts)\r\n\t  (format *debug-io* \"~A: \" count)\r\n\t  (princ restart *debug-io*)\r\n\t  (terpri *debug-io*)\r\n\t  (incf count)))\r\n      (setq active-frame (next-function-frame (- stack 20)))\r\n      (show-frame active-frame 0)\r\n      (tagbody\r\n       start\r\n\t (format *debug-io* \";~A> \" frame-depth)\r\n\t (let ((form (read)))\r\n\t   (case form\r\n\t     (:help (format *debug-io* \"Type :help to get help.~%\")\r\n\t\t    (format *debug-io* \"Type :continue <index> to invoke the indexed restart.~%\"))\r\n\t     (:back (do ((frame (next-function-frame (- stack 20))\r\n\t\t\t\t(next-function-frame frame))\r\n\t\t\t (index 0 (+ 1 index)))\r\n\t\t\t((not frame))\r\n\t\t      (show-frame frame index)))\r\n\t     (:up (if (plusp frame-depth)\r\n\t\t      (progn\r\n\t\t\t(decf frame-depth)\r\n\t\t\t(do ((frame (next-function-frame (- stack 20))\r\n\t\t\t\t    (next-function-frame frame))\r\n\t\t\t     (index 0 (+ 1 index)))\r\n\t\t\t    ((= index frame-depth) (setq active-frame frame)))\r\n\t\t\t(show-frame active-frame frame-depth))\r\n\t\t      (format *debug-io* \"Top of stack.~%\")))\r\n\t     (:down (let ((frame (next-function-frame active-frame)))\r\n\t\t      (if frame\r\n\t\t\t  (progn\r\n\t\t\t    (incf frame-depth)\r\n\t\t\t    (setq active-frame frame)\r\n\t\t\t    (show-frame active-frame frame-depth))\r\n\t\t\t  (format *debug-io* \"Bottom of stack.~%\"))))\r\n\t     (:locals (do ((env (fref (- active-frame 1)) (cdr env)))\r\n\t\t\t  ((not env))\r\n\t\t\t(when (symbolp (caar env))\r\n\t\t\t  (format *debug-io* \"~A~%\" (caar env)))))\r\n\t     (:continue (let ((index (read)))\r\n\t\t\t  (invoke-restart-interactively (nth index restarts))))\r\n\t     (t (let ((values (multiple-value-list\r\n\t\t\t       (eval form (fref (- active-frame 1)))))\r\n\t\t      (count 0))\r\n\t\t  (if values\r\n\t\t      (dolist (value values)\r\n\t\t\t(format *debug-io* \";~A: ~S~%\" count value)\r\n\t\t\t(incf count))\r\n\t\t      (format *debug-io* \";No values.~%\")))))\r\n\t   (go start))))))\r\n\r\n\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:39529 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[defun,'invoke-debugger',[condition],[let,[['debugger-hook','*debugger-hook*'],['*debugger-hook*',[]]],[when,'debugger-hook',[funcall,'debugger-hook',condition,'debugger-hook']],[format,'*debug-io*','$STRING'("Entering debugger.~%")],[princ,condition,'*debug-io*'],[terpri,'*debug-io*'],[let,[[restarts,['compute-restarts',condition]],[stack,[makef]],['frame-depth',0],['active-frame',[]]],[let,[[count,0]],[dolist,[restart,restarts],[format,'*debug-io*','$STRING'("~A: "),count],[princ,restart,'*debug-io*'],[terpri,'*debug-io*'],[incf,count]]],[setq,'active-frame',['next-function-frame',[-,stack,20]]],['show-frame','active-frame',0],[tagbody,start,[format,'*debug-io*','$STRING'(";~A> "),'frame-depth'],[let,[[form,[read]]],[case,form,[':help',[format,'*debug-io*','$STRING'("Type :help to get help.~%")],[format,'*debug-io*','$STRING'("Type :continue <index> to invoke the indexed restart.~%")]],[':back',[do,[[frame,['next-function-frame',[-,stack,20]],['next-function-frame',frame]],[index,0,[+,1,index]]],[[not,frame]],['show-frame',frame,index]]],[':up',[if,[plusp,'frame-depth'],[progn,[decf,'frame-depth'],[do,[[frame,['next-function-frame',[-,stack,20]],['next-function-frame',frame]],[index,0,[+,1,index]]],[[=,index,'frame-depth'],[setq,'active-frame',frame]]],['show-frame','active-frame','frame-depth']],[format,'*debug-io*','$STRING'("Top of stack.~%")]]],[':down',[let,[[frame,['next-function-frame','active-frame']]],[if,frame,[progn,[incf,'frame-depth'],[setq,'active-frame',frame],['show-frame','active-frame','frame-depth']],[format,'*debug-io*','$STRING'("Bottom of stack.~%")]]]],[':locals',[do,[[env,[fref,[-,'active-frame',1]],[cdr,env]]],[[not,env]],[when,[symbolp,[caar,env]],[format,'*debug-io*','$STRING'("~A~%"),[caar,env]]]]],[':continue',[let,[[index,[read]]],['invoke-restart-interactively',[nth,index,restarts]]]],[t,[let,[[values,['multiple-value-list',[eval,form,[fref,[-,'active-frame',1]]]]],[count,0]],[if,values,[dolist,[value,values],[format,'*debug-io*','$STRING'(";~A: ~S~%"),count,value],[incf,count]],[format,'*debug-io*','$STRING'(";No values.~%")]]]]],[go,start]]]]]]])
/*
(defun break (&optional format-control &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger (make-condition 'simple-condition
				       :format-control format-control
				       :format-arguments format-arguments))))
  nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:41801 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,break,['&optional','format-control','&rest','format-arguments'],['with-simple-restart',[continue,'$STRING'("Return from BREAK.")],[let,[['*debugger-hook*',[]]],['invoke-debugger',['make-condition',[quote,'simple-condition'],':format-control','format-control',':format-arguments','format-arguments']]]],[]])
wl:lambda_def(defun, break, cl_break, [c38_optional, sys_format_control, c38_rest, sys_format_arguments], [[with_simple_restart, [continue, '$ARRAY'([*], claz_base_character, "Return from BREAK.")], [let, [[xx_debugger_hook_xx, []]], [invoke_debugger, [make_condition, [quote, simple_condition], kw_format_control, sys_format_control, kw_format_arguments, sys_format_arguments]]]], []]).
wl:arglist_info(break, cl_break, [c38_optional, sys_format_control, c38_rest, sys_format_arguments], arginfo{all:[sys_format_control], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_format_control, sys_format_arguments], opt:[sys_format_control], req:0, rest:[sys_format_arguments], sublists:0, whole:0}).
wl: init_args(rest_only, cl_break).

/*

### Compiled:  `CL:BREAK` 
*/
cl_break(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	_274576902=[[[bv(sys_format_control, Format_control), bv(sys_format_arguments, RestNKeys)]|Env]|Env],
	RestNKeys=RestNKeys,
	opt_var(Env, sys_format_control, Format_control, true, [], 1, RestNKeys),
	cl_with_simple_restart(
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
			       ],
			       Simple_restart_Ret),
	[]=FnResult.
:- set_opv(cl_break, classof, claz_function),
   set_opv(break, compile_as, kw_function),
   set_opv(break, function, cl_break),
   DefunResult=break.
/*
:- side_effect(assert_lsp(break,
			  wl:lambda_def(defun, break, cl_break, [c38_optional, sys_format_control, c38_rest, sys_format_arguments], [[with_simple_restart, [continue, '$ARRAY'([*], claz_base_character, "Return from BREAK.")], [let, [[xx_debugger_hook_xx, []]], [invoke_debugger, [make_condition, [quote, simple_condition], kw_format_control, sys_format_control, kw_format_arguments, sys_format_arguments]]]], []]))).
*/
/*
:- side_effect(assert_lsp(break,
			  wl:arglist_info(break, cl_break, [c38_optional, sys_format_control, c38_rest, sys_format_arguments], arginfo{all:[sys_format_control], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_format_control, sys_format_arguments], opt:[sys_format_control], req:0, rest:[sys_format_arguments], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(break, wl:init_args(rest_only, cl_break))).
*/
/*
(defparameter *debugger-hook* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:42117 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*debugger-hook*',[]])
:- set_var(AEnv, defparameter, xx_debugger_hook_xx, []).
/*
(defparameter *break-on-signals* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:42153 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*break-on-signals*',[]])
:- set_var(AEnv, defparameter, xx_break_on_signals_xx, []).
/*
(defparameter *handlers* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:42192 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*handlers*',[]])
:- set_var(AEnv, defparameter, sys_xx_handlers_xx, []).
/*
(defun invoke-handler (condition)
  (dolist (handler *handlers*)
    (when (typep condition (car handler))
      (setq *handlers* (caddr handler))
      (funcall (cadr handler) condition))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:42223 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'invoke-handler',[condition],[dolist,[handler,'*handlers*'],[when,[typep,condition,[car,handler]],[setq,'*handlers*',[caddr,handler]],[funcall,[cadr,handler],condition]]]])
wl:lambda_def(defun, sys_invoke_handler, f_sys_invoke_handler, [condition], [[dolist, [sys_handler, sys_xx_handlers_xx], [when, [typep, condition, [car, sys_handler]], [setq, sys_xx_handlers_xx, [caddr, sys_handler]], [funcall, [cadr, sys_handler], condition]]]]).
wl:arglist_info(sys_invoke_handler, f_sys_invoke_handler, [condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:0, req:[condition], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_invoke_handler).

/*

### Compiled:  `SYS::INVOKE-HANDLER` 
*/
f_sys_invoke_handler(Condition, FnResult) :-
	nop(global_env(Env)),
	Env22=[bv(condition, Condition)|Env],
	get_var(Env22, sys_xx_handlers_xx, Xx_handlers_xx_Get),
	BV=bv(sys_handler, Ele),
	AEnv=[BV|Env22],
	forall(member(Ele, Xx_handlers_xx_Get),
	       ( nb_setarg(2, BV, Ele),
		 get_var(AEnv, condition, Condition_Get),
		 get_var(AEnv, sys_handler, Handler_Get),
		 cl_car(Handler_Get, Car_Ret),
		 cl_typep(Condition_Get, Car_Ret, IFTEST),
		 (   IFTEST\==[]
		 ->  get_var(AEnv, sys_handler, Handler_Get12),
		     cl_caddr(Handler_Get12, Xx_handlers_xx),
		     set_var(AEnv, sys_xx_handlers_xx, Xx_handlers_xx),
		     get_var(AEnv, sys_handler, Handler_Get13),
		     cl_cadr(Handler_Get13, Apply_Param),
		     get_var(AEnv, condition, Condition_Get14),
		     cl_apply(Apply_Param, [Condition_Get14], TrueResult),
		     _276865040=TrueResult
		 ;   _276865040=[]
		 )
	       )),
	_276865040=FnResult.
:- set_opv(f_sys_invoke_handler, classof, claz_function),
   set_opv(sys_invoke_handler, compile_as, kw_function),
   set_opv(sys_invoke_handler, function, f_sys_invoke_handler),
   DefunResult=sys_invoke_handler.
/*
:- side_effect(assert_lsp(sys_invoke_handler,
			  wl:lambda_def(defun, sys_invoke_handler, f_sys_invoke_handler, [condition], [[dolist, [sys_handler, sys_xx_handlers_xx], [when, [typep, condition, [car, sys_handler]], [setq, sys_xx_handlers_xx, [caddr, sys_handler]], [funcall, [cadr, sys_handler], condition]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_invoke_handler,
			  wl:arglist_info(sys_invoke_handler, f_sys_invoke_handler, [condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:0, req:[condition], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_invoke_handler,
			  wl:init_args(exact_only, f_sys_invoke_handler))).
*/
/*
'(defmacro handler-bind (bindings &rest forms)
  (let ((form '*handlers*)
	(handlers (gensym)))
    (dolist (binding (reverse bindings))
      (setq form
	    `(cons (list ',(car binding) ,(cadr binding) ',handlers) ,form)))
    `(let ((handlers *handlers*)
	   (*handlers* ,form))
      ,@forms)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:42421 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[defmacro,'handler-bind',[bindings,'&rest',forms],[let,[[form,[quote,'*handlers*']],[handlers,[gensym]]],[dolist,[binding,[reverse,bindings]],[setq,form,['#BQ',[cons,[list,[quote,['#COMMA',[car,binding]]],['#COMMA',[cadr,binding]],[quote,['#COMMA',handlers]]],['#COMMA',form]]]]],['#BQ',[let,[[handlers,'*handlers*'],['*handlers*',['#COMMA',form]]],['#BQ-COMMA-ELIPSE',forms]]]]]])
/*
'(defmacro handler-case (expression &rest clauses)
  (let ((tag (gensym))
	(bindings nil))
    `(handler-bind
      ,(dolist (clause clauses (reverse bindings))
	 (let ((typespec (car clause))
	       (var-list (cadr clause))
	       (forms (cddr clauses)))
	   (push `(typespec #'(lambda (,(if var-list (car var-list) (gensym)))
				(return-from tag (progn ,@forms))))
		 bindings)))
      ,expression)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:42729 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[defmacro,'handler-case',[expression,'&rest',clauses],[let,[[tag,[gensym]],[bindings,[]]],['#BQ',['handler-bind',['#COMMA',[dolist,[clause,clauses,[reverse,bindings]],[let,[[typespec,[car,clause]],['var-list',[cadr,clause]],[forms,[cddr,clauses]]],[push,['#BQ',[typespec,function([lambda,[['#COMMA',[if,'var-list',[car,'var-list'],[gensym]]]],['return-from',tag,[progn,['#BQ-COMMA-ELIPSE',forms]]]])]],bindings]]]],['#COMMA',expression]]]]]])
/*
(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
    (error (condition) (values nil condition))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:43147 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'ignore-errors',['&rest',forms],['#BQ',['handler-case',[progn,['#BQ-COMMA-ELIPSE',forms]],[error,[condition],[values,[],condition]]]]])
wl:lambda_def(defmacro, ignore_errors, cl_ignore_errors, [c38_rest, forms], [progn, ['#BQ', [handler_case, [progn, ['#BQ-COMMA-ELIPSE', forms]], [error, [condition], [values, [], condition]]]]]).
wl:arglist_info(ignore_errors, cl_ignore_errors, [c38_rest, forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[forms], opt:0, req:0, rest:[forms], sublists:0, whole:0}).
wl: init_args(0, cl_ignore_errors).

/*

### Compiled:  `CL:IGNORE-ERRORS` 
*/
cl_ignore_errors(RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	GEnv=[[[bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	get_var(GEnv, forms, Forms_Get),
	[handler_case, [progn|Forms_Get], [error, [condition], [values, [], condition]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_ignore_errors, classof, claz_macro),
   set_opv(ignore_errors, compile_as, kw_operator),
   set_opv(ignore_errors, function, cl_ignore_errors),
   DefMacroResult=ignore_errors.
/*
:- side_effect(assert_lsp(ignore_errors,
			  wl:lambda_def(defmacro, ignore_errors, cl_ignore_errors, [c38_rest, forms], [progn, ['#BQ', [handler_case, [progn, ['#BQ-COMMA-ELIPSE', forms]], [error, [condition], [values, [], condition]]]]]))).
*/
/*
:- side_effect(assert_lsp(ignore_errors,
			  wl:arglist_info(ignore_errors, cl_ignore_errors, [c38_rest, forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[forms], opt:0, req:0, rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(ignore_errors, wl:init_args(0, cl_ignore_errors))).
*/
/*
(defparameter *restarts* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:43270 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*restarts*',[]])
:- set_var(AEnv, defparameter, sys_xx_restarts_xx, []).
/*
(defun compute-restarts (&optional condition)
  "FIXME restarts associated with conditions"
  (if condition
      *restarts*
      *restarts*))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:43301 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'compute-restarts',['&optional',condition],'$STRING'("FIXME restarts associated with conditions"),[if,condition,'*restarts*','*restarts*']])
doc: doc_string(compute_restarts,
	      _280257358,
	      function,
	      "FIXME restarts associated with conditions").

wl:lambda_def(defun, compute_restarts, cl_compute_restarts, [c38_optional, condition], [[if, condition, sys_xx_restarts_xx, sys_xx_restarts_xx]]).
wl:arglist_info(compute_restarts, cl_compute_restarts, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(rest_only, cl_compute_restarts).

/*

### Compiled:  `CL:COMPUTE-RESTARTS` 
*/
cl_compute_restarts(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(condition, Condition)]|Env]|Env],
	RestNKeys=RestNKeys,
	opt_var(Env, condition, Condition, true, [], 1, RestNKeys),
	get_var(GEnv, condition, IFTEST),
	(   IFTEST\==[]
	->  get_var(GEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get),
	    FnResult=Xx_restarts_xx_Get
	;   get_var(GEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get12),
	    FnResult=Xx_restarts_xx_Get12
	).
:- set_opv(cl_compute_restarts, classof, claz_function),
   set_opv(compute_restarts, compile_as, kw_function),
   set_opv(compute_restarts, function, cl_compute_restarts),
   DefunResult=compute_restarts.
/*
:- side_effect(assert_lsp(compute_restarts,
			  doc:doc_string(compute_restarts, _280257358, function, "FIXME restarts associated with conditions"))).
*/
/*
:- side_effect(assert_lsp(compute_restarts,
			  wl:lambda_def(defun, compute_restarts, cl_compute_restarts, [c38_optional, condition], [[if, condition, sys_xx_restarts_xx, sys_xx_restarts_xx]]))).
*/
/*
:- side_effect(assert_lsp(compute_restarts,
			  wl:arglist_info(compute_restarts, cl_compute_restarts, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(compute_restarts,
			  wl:init_args(rest_only, cl_compute_restarts))).
*/
/*
(defun find-restart (identifier &optional condition)
  (dolist (restart *restarts*)
    (when (eq restart identifier)
      (return restart))
    (when (eq (restart-name restart) identifier)
      (return restart))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:43450 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'find-restart',[identifier,'&optional',condition],[dolist,[restart,'*restarts*'],[when,[eq,restart,identifier],[return,restart]],[when,[eq,['restart-name',restart],identifier],[return,restart]]]])
wl:lambda_def(defun, find_restart, cl_find_restart, [sys_identifier, c38_optional, condition], [[dolist, [restart, sys_xx_restarts_xx], [when, [eq, restart, sys_identifier], [return, restart]], [when, [eq, [restart_name, restart], sys_identifier], [return, restart]]]]).
wl:arglist_info(find_restart, cl_find_restart, [sys_identifier, c38_optional, condition], arginfo{all:[sys_identifier, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_identifier, condition], opt:[condition], req:[sys_identifier], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_find_restart).

/*

### Compiled:  `CL:FIND-RESTART` 
*/
cl_find_restart(Identifier, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_identifier, Identifier), bv(condition, Condition)]|Env]|Env],
	opt_var(Env, condition, Condition, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get),
		  BV=bv(restart, Ele),
		  BlockExitEnv=[BV|GEnv],
		  forall(member(Ele, Xx_restarts_xx_Get),
			 ( nb_setarg(2, BV, Ele),
			   get_var(BlockExitEnv, restart, Restart_Get),
			   get_var(BlockExitEnv, sys_identifier, Identifier_Get),
			   (   is_eq(Restart_Get, Identifier_Get)
			   ->  get_var(BlockExitEnv, restart, Restart_Get17),
			       throw(block_exit([], Restart_Get17)),
			       _281307738=ThrowResult
			   ;   _281307738=[]
			   ),
			   get_var(BlockExitEnv, restart, Restart_Get21),
			   cl_restart_name(Restart_Get21, PredArg1Result24),
			   get_var(BlockExitEnv,
				   sys_identifier,
				   Identifier_Get22),
			   (   is_eq(PredArg1Result24, Identifier_Get22)
			   ->  get_var(BlockExitEnv, restart, RetResult26),
			       throw(block_exit([], RetResult26)),
			       _281303330=ThrowResult27
			   ;   _281303330=[]
			   )
			 ))
		),
		_281303330=FnResult
	      ),
	      block_exit(find_restart, FnResult),
	      true).
:- set_opv(cl_find_restart, classof, claz_function),
   set_opv(find_restart, compile_as, kw_function),
   set_opv(find_restart, function, cl_find_restart),
   DefunResult=find_restart.
/*
:- side_effect(assert_lsp(find_restart,
			  wl:lambda_def(defun, find_restart, cl_find_restart, [sys_identifier, c38_optional, condition], [[dolist, [restart, sys_xx_restarts_xx], [when, [eq, restart, sys_identifier], [return, restart]], [when, [eq, [restart_name, restart], sys_identifier], [return, restart]]]]))).
*/
/*
:- side_effect(assert_lsp(find_restart,
			  wl:arglist_info(find_restart, cl_find_restart, [sys_identifier, c38_optional, condition], arginfo{all:[sys_identifier, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_identifier, condition], opt:[condition], req:[sys_identifier], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(find_restart, wl:init_args(1, cl_find_restart))).
*/
/*
(defun designator-restart (designator)
  (if (restartp designator)
      designator
      (dolist (restart *restarts* (error 'type-error :datum designator
					 :expected-type 'restart))
	(when (eq (restart-name restart) designator)
	  (return restart)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:43677 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-restart',[designator],[if,[restartp,designator],designator,[dolist,[restart,'*restarts*',[error,[quote,'type-error'],':datum',designator,':expected-type',[quote,restart]]],[when,[eq,['restart-name',restart],designator],[return,restart]]]]])
wl:lambda_def(defun, sys_designator_restart, f_sys_designator_restart, [sys_designator], [[if, [sys_restartp, sys_designator], sys_designator, [dolist, [restart, sys_xx_restarts_xx, [error, [quote, type_error], kw_datum, sys_designator, kw_expected_type, [quote, restart]]], [when, [eq, [restart_name, restart], sys_designator], [return, restart]]]]]).
wl:arglist_info(sys_designator_restart, f_sys_designator_restart, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_designator_restart).

/*

### Compiled:  `SYS::DESIGNATOR-RESTART` 
*/
f_sys_designator_restart(Designator, FnResult) :-
	nop(global_env(Env)),
	Env34=[bv(sys_designator, Designator)|Env],
	catch(( ( get_var(Env34, sys_designator, Designator_Get),
		  f_sys_restartp(Designator_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(Env34, sys_designator, Designator_Get9),
		      _282799074=Designator_Get9
		  ;   LEnv=[bv([error, [quote, type_error], kw_datum, sys_designator, kw_expected_type, [quote, restart]], [])|Env34],
		      get_var(LEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get),
		      BV=bv(restart, Ele),
		      BlockExitEnv=[BV|LEnv],
		      forall(member(Ele, Xx_restarts_xx_Get),
			     ( nb_setarg(2, BV, Ele),
			       get_var(BlockExitEnv, restart, Restart_Get),
			       cl_restart_name(Restart_Get, PredArg1Result),
			       get_var(BlockExitEnv,
				       sys_designator,
				       Designator_Get16),
			       (   is_eq(PredArg1Result, Designator_Get16)
			       ->  get_var(BlockExitEnv, restart, Restart_Get22),
				   throw(block_exit([], Restart_Get22)),
				   _282812728=ThrowResult
			       ;   _282812728=[]
			       )
			     )),
		      get_var(LEnv, sys_designator, Designator_Get29),
		      cl_error(
			       [ type_error,
				 kw_datum,
				 Designator_Get29,
				 kw_expected_type,
				 restart
			       ],
			       LetResult),
		      _282799074=LetResult
		  )
		),
		_282799074=FnResult
	      ),
	      block_exit(sys_designator_restart, FnResult),
	      true).
:- set_opv(f_sys_designator_restart, classof, claz_function),
   set_opv(sys_designator_restart, compile_as, kw_function),
   set_opv(sys_designator_restart, function, f_sys_designator_restart),
   DefunResult=sys_designator_restart.
/*
:- side_effect(assert_lsp(sys_designator_restart,
			  wl:lambda_def(defun, sys_designator_restart, f_sys_designator_restart, [sys_designator], [[if, [sys_restartp, sys_designator], sys_designator, [dolist, [restart, sys_xx_restarts_xx, [error, [quote, type_error], kw_datum, sys_designator, kw_expected_type, [quote, restart]]], [when, [eq, [restart_name, restart], sys_designator], [return, restart]]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_restart,
			  wl:arglist_info(sys_designator_restart, f_sys_designator_restart, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_designator_restart,
			  wl:init_args(exact_only, f_sys_designator_restart))).
*/
/*
(defun invoke-restart (restart &rest arguments)
  (setq restart (designator-restart restart))
  (apply (restart-function restart) arguments))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:43943 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'invoke-restart',[restart,'&rest',arguments],[setq,restart,['designator-restart',restart]],[apply,['restart-function',restart],arguments]])
wl:lambda_def(defun, invoke_restart, cl_invoke_restart, [restart, c38_rest, sys_arguments], [[setq, restart, [sys_designator_restart, restart]], [apply, [sys_restart_function, restart], sys_arguments]]).
wl:arglist_info(invoke_restart, cl_invoke_restart, [restart, c38_rest, sys_arguments], arginfo{all:[restart], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[restart, sys_arguments], opt:0, req:[restart], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(1, cl_invoke_restart).

/*

### Compiled:  `CL:INVOKE-RESTART` 
*/
cl_invoke_restart(Restart, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	AEnv=[[[bv(restart, Restart), bv(sys_arguments, RestNKeys)]|Env]|Env],
	get_var(AEnv, restart, Restart_Get),
	f_sys_designator_restart(Restart_Get, Restart14),
	set_var(AEnv, restart, Restart14),
	get_var(AEnv, restart, Restart_Get9),
	f_sys_restart_function(Restart_Get9, Apply_Param),
	get_var(AEnv, sys_arguments, Arguments_Get),
	cl_apply(Apply_Param, Arguments_Get, Apply_Ret),
	Apply_Ret=FnResult.
:- set_opv(cl_invoke_restart, classof, claz_function),
   set_opv(invoke_restart, compile_as, kw_function),
   set_opv(invoke_restart, function, cl_invoke_restart),
   DefunResult=invoke_restart.
/*
:- side_effect(assert_lsp(invoke_restart,
			  wl:lambda_def(defun, invoke_restart, cl_invoke_restart, [restart, c38_rest, sys_arguments], [[setq, restart, [sys_designator_restart, restart]], [apply, [sys_restart_function, restart], sys_arguments]]))).
*/
/*
:- side_effect(assert_lsp(invoke_restart,
			  wl:arglist_info(invoke_restart, cl_invoke_restart, [restart, c38_rest, sys_arguments], arginfo{all:[restart], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[restart, sys_arguments], opt:0, req:[restart], rest:[sys_arguments], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(invoke_restart, wl:init_args(1, cl_invoke_restart))).
*/
/*
(defun invoke-restart-interactively (restart)
  (setq restart (designator-restart restart))
  (apply (restart-function restart)
	 (funcall (restart-interactive-function restart))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:44088 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'invoke-restart-interactively',[restart],[setq,restart,['designator-restart',restart]],[apply,['restart-function',restart],[funcall,['restart-interactive-function',restart]]]])
wl:lambda_def(defun, invoke_restart_interactively, cl_invoke_restart_interactively, [restart], [[setq, restart, [sys_designator_restart, restart]], [apply, [sys_restart_function, restart], [funcall, [sys_restart_interactive_function, restart]]]]).
wl:arglist_info(invoke_restart_interactively, cl_invoke_restart_interactively, [restart], arginfo{all:[restart], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[restart], opt:0, req:[restart], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_invoke_restart_interactively).

/*

### Compiled:  `CL:INVOKE-RESTART-INTERACTIVELY` 
*/
cl_invoke_restart_interactively(Restart, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(restart, Restart)|Env],
	get_var(AEnv, restart, Restart_Get),
	f_sys_designator_restart(Restart_Get, Restart13),
	set_var(AEnv, restart, Restart13),
	get_var(AEnv, restart, Restart_Get8),
	f_sys_restart_function(Restart_Get8, Apply_Param15),
	get_var(AEnv, restart, Restart_Get9),
	f_sys_restart_interactive_function(Restart_Get9, Apply_Param),
	cl_apply(Apply_Param, [], KeysNRest),
	cl_apply(Apply_Param15, KeysNRest, Apply_Ret),
	Apply_Ret=FnResult.
:- set_opv(cl_invoke_restart_interactively, classof, claz_function),
   set_opv(invoke_restart_interactively, compile_as, kw_function),
   set_opv(invoke_restart_interactively,
	   function,
	   cl_invoke_restart_interactively),
   DefunResult=invoke_restart_interactively.
/*
:- side_effect(assert_lsp(invoke_restart_interactively,
			  wl:lambda_def(defun, invoke_restart_interactively, cl_invoke_restart_interactively, [restart], [[setq, restart, [sys_designator_restart, restart]], [apply, [sys_restart_function, restart], [funcall, [sys_restart_interactive_function, restart]]]]))).
*/
/*
:- side_effect(assert_lsp(invoke_restart_interactively,
			  wl:arglist_info(invoke_restart_interactively, cl_invoke_restart_interactively, [restart], arginfo{all:[restart], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[restart], opt:0, req:[restart], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(invoke_restart_interactively,
			  wl:init_args(exact_only, cl_invoke_restart_interactively))).
*/
/*
(defmacro restart-bind (restart-bindings &rest forms)
  (let ((form '*restarts*))
    (dolist (binding (reverse restart-bindings))
      (setq form
	    `(cons (make-restart ',(car binding) ,@(cdr binding)) ,form)))
    `(let ((*restarts* ,form))
      ,@forms)))

#|

(defmacro restart-case (restartable-form &rest clauses)
  (let ((catch-tag (gensym))
	(bindings nil))
    `(catch ',catch-tag
      (restart-bind
	  ,(dolist (clause clauses (reverse bindings))
	     (let ((name (car clause))
		   (lambda-list (cadr clause))
		   (rest (cddr clause))
		   (interactive '#'(lambda () nil))
		   (report '#'(lambda (stream)
				(format stream ""(defmacro restart-bind (restart-bindings &rest forms)\r\n  (let ((form '*restarts*))\r\n    (dolist (binding (reverse restart-bindings))\r\n      (setq form\r\n\t    `(cons (make-restart ',(car binding) ,@(cdr binding)) ,form)))\r\n    `(let ((*restarts* ,form))\r\n      ,@forms)))\r\n\r\n#|\r\n\r\n(defmacro restart-case (restartable-form &rest clauses)\r\n  (let ((catch-tag (gensym))\r\n\t(bindings nil))\r\n    `(catch ',catch-tag\r\n      (restart-bind\r\n\t  ,(dolist (clause clauses (reverse bindings))\r\n\t     (let ((name (car clause))\r\n\t\t   (lambda-list (cadr clause))\r\n\t\t   (rest (cddr clause))\r\n\t\t   (interactive '#'(lambda () nil))\r\n\t\t   (report '#'(lambda (stream)\r\n\t\t\t\t(format stream \"~A\" (car clause))))\r\n\t\t   (test '#'(lambda (condition) t)))\r\n\t       (tagbody\r\n\t\tstart\r\n\t\t  (when (member (car rest) '(:interactive :report :test))\r\n\t\t    (let ((value (cadr rest)))\r\n\t\t      (case (car rest)\r\n\t\t\t(:interactive (setq interactive `(function ,value)))\r\n\t\t\t(:report (setq report\r\n\t\t\t\t       (if (stringp value)\r\n\t\t\t\t\t   `#'(lambda (stream)\r\n\t\t\t\t\t\t(write-string ,value stream))\r\n\t\t\t\t\t   `(function ,value))))\r\n\t\t\t(:test (setq test `(function ,value)))))\r\n\t\t    (setq rest (cddr rest))\r\n\t\t    (go start)))\r\n\t       (push `(,(car clause)\r\n\t\t       #'(lambda ,(cadr clause)\r\n\t\t\t   (throw ',catch-tag (progn ,@rest)))\r\n\t\t       :interactive-function ,interactive\r\n\t\t       :report-function ,report\r\n\t\t       :test-function ,test)\r\n\t\t     bindings)))\r\n\t,restartable-form))))\r\n\r\n\r\n(defmacro with-simple-restart ((name format-control &rest format-arguments)\r\n\t\t\t       &rest forms)\r\n  (let ((tag (gensym)))\r\n    `(block ,tag\r\n      (restart-bind\r\n\t  ((,name\r\n\t    #'(lambda () (return-from ,tag (values nil t)))\r\n\t     :interactive-function #'(lambda () nil)\r\n\t     :report-function #'(lambda (stream)\r\n\t\t\t\t  (apply #'format stream ',format-control\r\n\t\t\t\t\t ',format-arguments))\r\n\t     :test-function #'(lambda () t)))\r\n\t,@forms))))\r\n\r\n\r\n|#\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:44273 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'restart-bind',['restart-bindings','&rest',forms],[let,[[form,[quote,'*restarts*']]],[dolist,[binding,[reverse,'restart-bindings']],[setq,form,['#BQ',[cons,['make-restart',[quote,['#COMMA',[car,binding]]],['#BQ-COMMA-ELIPSE',[cdr,binding]]],['#COMMA',form]]]]],['#BQ',[let,[['*restarts*',['#COMMA',form]]],['#BQ-COMMA-ELIPSE',forms]]]]])
wl:lambda_def(defmacro, restart_bind, cl_restart_bind, [sys_restart_bindings, c38_rest, forms], [progn, [let, [[sys_form, [quote, sys_xx_restarts_xx]]], [dolist, [binding, [reverse, sys_restart_bindings]], [setq, sys_form, ['#BQ', [cons, [sys_make_restart, [quote, ['#COMMA', [car, binding]]], ['#BQ-COMMA-ELIPSE', [cdr, binding]]], ['#COMMA', sys_form]]]]], ['#BQ', [let, [[sys_xx_restarts_xx, ['#COMMA', sys_form]]], ['#BQ-COMMA-ELIPSE', forms]]]]]).
wl:arglist_info(restart_bind, cl_restart_bind, [sys_restart_bindings, c38_rest, forms], arginfo{all:[sys_restart_bindings], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_restart_bindings, forms], opt:0, req:[sys_restart_bindings], rest:[forms], sublists:0, whole:0}).
wl: init_args(1, cl_restart_bind).

/*

### Compiled:  `CL:RESTART-BIND` 
*/
cl_restart_bind(Restart_bindings, RestNKeys, FnResult) :-
	global_env(Global_env_Ret),
	CDR=[[[bv(sys_restart_bindings, Restart_bindings), bv(forms, RestNKeys)]|Global_env_Ret]|Global_env_Ret],
	LEnv=[bv(sys_form, sys_xx_restarts_xx)|CDR],
	get_var(LEnv, sys_restart_bindings, Restart_bindings_Get),
	cl_reverse(Restart_bindings_Get, List),
	BV=bv(binding, Ele),
	AEnv=[BV|LEnv],
	forall(member(Ele, List),
	       ( nb_setarg(2, BV, Ele),
		 get_var(AEnv, binding, Binding_Get),
		 cl_car(Binding_Get, Car_Ret),
		 get_var(AEnv, binding, Binding_Get13),
		 cl_cdr(Binding_Get13, Cdr_Ret),
		 get_var(AEnv, sys_form, Form_Get),
		 set_var(AEnv,
			 setq,
			 sys_form,
			 
			 [ cons,
			   [sys_make_restart, [quote, Car_Ret]|Cdr_Ret],
			   Form_Get
			 ])
	       )),
	get_var(LEnv, forms, Forms_Get),
	get_var(LEnv, sys_form, Form_Get19),
	[let, [[sys_xx_restarts_xx, Form_Get19]]|Forms_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_restart_bind, classof, claz_macro),
   set_opv(restart_bind, compile_as, kw_operator),
   set_opv(restart_bind, function, cl_restart_bind),
   DefMacroResult=restart_bind.
/*
:- side_effect(assert_lsp(restart_bind,
			  wl:lambda_def(defmacro, restart_bind, cl_restart_bind, [sys_restart_bindings, c38_rest, forms], [progn, [let, [[sys_form, [quote, sys_xx_restarts_xx]]], [dolist, [binding, [reverse, sys_restart_bindings]], [setq, sys_form, ['#BQ', [cons, [sys_make_restart, [quote, ['#COMMA', [car, binding]]], ['#BQ-COMMA-ELIPSE', [cdr, binding]]], ['#COMMA', sys_form]]]]], ['#BQ', [let, [[sys_xx_restarts_xx, ['#COMMA', sys_form]]], ['#BQ-COMMA-ELIPSE', forms]]]]]))).
*/
/*
:- side_effect(assert_lsp(restart_bind,
			  wl:arglist_info(restart_bind, cl_restart_bind, [sys_restart_bindings, c38_rest, forms], arginfo{all:[sys_restart_bindings], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_restart_bindings, forms], opt:0, req:[sys_restart_bindings], rest:[forms], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(restart_bind, wl:init_args(1, cl_restart_bind))).
*/
/*


(defmacro restart-case (restartable-form &rest clauses)
  (let ((catch-tag (gensym))
	(bindings nil))
    `(catch ',catch-tag
      (restart-bind
	  ,(dolist (clause clauses (reverse bindings))
	     (let ((name (car clause))
		   (lambda-list (cadr clause))
		   (rest (cddr clause))
		   (interactive '#'(lambda () nil))
		   (report '#'(lambda (stream)
				(format stream ""\r\n\r\n(defmacro restart-case (restartable-form &rest clauses)\r\n  (let ((catch-tag (gensym))\r\n\t(bindings nil))\r\n    `(catch ',catch-tag\r\n      (restart-bind\r\n\t  ,(dolist (clause clauses (reverse bindings))\r\n\t     (let ((name (car clause))\r\n\t\t   (lambda-list (cadr clause))\r\n\t\t   (rest (cddr clause))\r\n\t\t   (interactive '#'(lambda () nil))\r\n\t\t   (report '#'(lambda (stream)\r\n\t\t\t\t(format stream \"~A\" (car clause))))\r\n\t\t   (test '#'(lambda (condition) t)))\r\n\t       (tagbody\r\n\t\tstart\r\n\t\t  (when (member (car rest) '(:interactive :report :test))\r\n\t\t    (let ((value (cadr rest)))\r\n\t\t      (case (car rest)\r\n\t\t\t(:interactive (setq interactive `(function ,value)))\r\n\t\t\t(:report (setq report\r\n\t\t\t\t       (if (stringp value)\r\n\t\t\t\t\t   `#'(lambda (stream)\r\n\t\t\t\t\t\t(write-string ,value stream))\r\n\t\t\t\t\t   `(function ,value))))\r\n\t\t\t(:test (setq test `(function ,value)))))\r\n\t\t    (setq rest (cddr rest))\r\n\t\t    (go start)))\r\n\t       (push `(,(car clause)\r\n\t\t       #'(lambda ,(cadr clause)\r\n\t\t\t   (throw ',catch-tag (progn ,@rest)))\r\n\t\t       :interactive-function ,interactive\r\n\t\t       :report-function ,report\r\n\t\t       :test-function ,test)\r\n\t\t     bindings)))\r\n\t,restartable-form))))\r\n\r\n\r\n(defmacro with-simple-restart ((name format-control &rest format-arguments)\r\n\t\t\t       &rest forms)\r\n  (let ((tag (gensym)))\r\n    `(block ,tag\r\n      (restart-bind\r\n\t  ((,name\r\n\t    #'(lambda () (return-from ,tag (values nil t)))\r\n\t     :interactive-function #'(lambda () nil)\r\n\t     :report-function #'(lambda (stream)\r\n\t\t\t\t  (apply #'format stream ',format-control\r\n\t\t\t\t\t ',format-arguments))\r\n\t     :test-function #'(lambda () t)))\r\n\t,@forms))))\r\n\r\n\r\n".
*/
/*
(defun abort (&optional condition)
  (invoke-restart (find-restart 'abort condition))
  (error 'control-error))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:46185 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,abort,['&optional',condition],['invoke-restart',['find-restart',[quote,abort],condition]],[error,[quote,'control-error']]])
wl:lambda_def(defun, abort, cl_abort, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, abort], condition]], [error, [quote, control_error]]]).
wl:arglist_info(abort, cl_abort, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(rest_only, cl_abort).

/*

### Compiled:  `CL:ABORT` 
*/
cl_abort(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(condition, Condition)]|Env]|Env],
	RestNKeys=RestNKeys,
	opt_var(Env, condition, Condition, true, [], 1, RestNKeys),
	get_var(GEnv, condition, Condition_Get),
	cl_find_restart(abort, [Condition_Get], Invoke_restart_Param),
	cl_invoke_restart(Invoke_restart_Param, [], Invoke_restart_Ret),
	cl_error([control_error], Error_Ret),
	Error_Ret=FnResult.
:- set_opv(cl_abort, classof, claz_function),
   set_opv(abort, compile_as, kw_function),
   set_opv(abort, function, cl_abort),
   DefunResult=abort.
/*
:- side_effect(assert_lsp(abort,
			  wl:lambda_def(defun, abort, cl_abort, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, abort], condition]], [error, [quote, control_error]]]))).
*/
/*
:- side_effect(assert_lsp(abort,
			  wl:arglist_info(abort, cl_abort, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(abort, wl:init_args(rest_only, cl_abort))).
*/
/*
(defun continue (&optional condition)
  (invoke-restart (find-restart 'continue condition)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:46300 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,continue,['&optional',condition],['invoke-restart',['find-restart',[quote,continue],condition]]])
wl:lambda_def(defun, continue, cl_continue, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, continue], condition]]]).
wl:arglist_info(continue, cl_continue, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(rest_only, cl_continue).

/*

### Compiled:  `CL:CONTINUE` 
*/
cl_continue(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(condition, Condition)]|Env]|Env],
	RestNKeys=RestNKeys,
	opt_var(Env, condition, Condition, true, [], 1, RestNKeys),
	get_var(GEnv, condition, Condition_Get),
	cl_find_restart(continue, [Condition_Get], Invoke_restart_Param),
	cl_invoke_restart(Invoke_restart_Param, [], Invoke_restart_Ret),
	Invoke_restart_Ret=FnResult.
:- set_opv(cl_continue, classof, claz_function),
   set_opv(continue, compile_as, kw_function),
   set_opv(continue, function, cl_continue),
   DefunResult=continue.
/*
:- side_effect(assert_lsp(continue,
			  wl:lambda_def(defun, continue, cl_continue, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, continue], condition]]]))).
*/
/*
:- side_effect(assert_lsp(continue,
			  wl:arglist_info(continue, cl_continue, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(continue, wl:init_args(rest_only, cl_continue))).
*/
/*
(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart 'muffle-warning condition))
  (error 'control-error))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:46395 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'muffle-warning',['&optional',condition],['invoke-restart',['find-restart',[quote,'muffle-warning'],condition]],[error,[quote,'control-error']]])
wl:lambda_def(defun, muffle_warning, cl_muffle_warning, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, muffle_warning], condition]], [error, [quote, control_error]]]).
wl:arglist_info(muffle_warning, cl_muffle_warning, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(rest_only, cl_muffle_warning).

/*

### Compiled:  `CL:MUFFLE-WARNING` 
*/
cl_muffle_warning(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(condition, Condition)]|Env]|Env],
	RestNKeys=RestNKeys,
	opt_var(Env, condition, Condition, true, [], 1, RestNKeys),
	get_var(GEnv, condition, Condition_Get),
	cl_find_restart(muffle_warning, [Condition_Get], Invoke_restart_Param),
	cl_invoke_restart(Invoke_restart_Param, [], Invoke_restart_Ret),
	cl_error([control_error], Error_Ret),
	Error_Ret=FnResult.
:- set_opv(cl_muffle_warning, classof, claz_function),
   set_opv(muffle_warning, compile_as, kw_function),
   set_opv(muffle_warning, function, cl_muffle_warning),
   DefunResult=muffle_warning.
/*
:- side_effect(assert_lsp(muffle_warning,
			  wl:lambda_def(defun, muffle_warning, cl_muffle_warning, [c38_optional, condition], [[invoke_restart, [find_restart, [quote, muffle_warning], condition]], [error, [quote, control_error]]]))).
*/
/*
:- side_effect(assert_lsp(muffle_warning,
			  wl:arglist_info(muffle_warning, cl_muffle_warning, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(muffle_warning,
			  wl:init_args(rest_only, cl_muffle_warning))).
*/
/*
(defun store-value (value &optional condition)
  (invoke-restart (find-restart 'store-value condition) value))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:46528 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'store-value',[value,'&optional',condition],['invoke-restart',['find-restart',[quote,'store-value'],condition],value]])
wl:lambda_def(defun, store_value, cl_store_value, [sys_value, c38_optional, condition], [[invoke_restart, [find_restart, [quote, store_value], condition], sys_value]]).
wl:arglist_info(store_value, cl_store_value, [sys_value, c38_optional, condition], arginfo{all:[sys_value, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value, condition], opt:[condition], req:[sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_store_value).

/*

### Compiled:  `CL:STORE-VALUE` 
*/
cl_store_value(Value, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_value, Value), bv(condition, Condition)]|Env]|Env],
	opt_var(Env, condition, Condition, true, [], 1, RestNKeys),
	get_var(GEnv, condition, Condition_Get),
	cl_find_restart(store_value, [Condition_Get], Invoke_restart_Param),
	get_var(GEnv, sys_value, Value_Get),
	cl_invoke_restart(Invoke_restart_Param, [Value_Get], Invoke_restart_Ret),
	Invoke_restart_Ret=FnResult.
:- set_opv(cl_store_value, classof, claz_function),
   set_opv(store_value, compile_as, kw_function),
   set_opv(store_value, function, cl_store_value),
   DefunResult=store_value.
/*
:- side_effect(assert_lsp(store_value,
			  wl:lambda_def(defun, store_value, cl_store_value, [sys_value, c38_optional, condition], [[invoke_restart, [find_restart, [quote, store_value], condition], sys_value]]))).
*/
/*
:- side_effect(assert_lsp(store_value,
			  wl:arglist_info(store_value, cl_store_value, [sys_value, c38_optional, condition], arginfo{all:[sys_value, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value, condition], opt:[condition], req:[sys_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(store_value, wl:init_args(1, cl_store_value))).
*/
/*
(defun use-value (value &optional condition)
  (invoke-restart (find-restart 'use-value condition) value))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:46641 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'use-value',[value,'&optional',condition],['invoke-restart',['find-restart',[quote,'use-value'],condition],value]])
wl:lambda_def(defun, use_value, cl_use_value, [sys_value, c38_optional, condition], [[invoke_restart, [find_restart, [quote, use_value], condition], sys_value]]).
wl:arglist_info(use_value, cl_use_value, [sys_value, c38_optional, condition], arginfo{all:[sys_value, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value, condition], opt:[condition], req:[sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(1, cl_use_value).

/*

### Compiled:  `CL:USE-VALUE` 
*/
cl_use_value(Value, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	GEnv=[[[bv(sys_value, Value), bv(condition, Condition)]|Env]|Env],
	opt_var(Env, condition, Condition, true, [], 1, RestNKeys),
	get_var(GEnv, condition, Condition_Get),
	cl_find_restart(use_value, [Condition_Get], Invoke_restart_Param),
	get_var(GEnv, sys_value, Value_Get),
	cl_invoke_restart(Invoke_restart_Param, [Value_Get], Invoke_restart_Ret),
	Invoke_restart_Ret=FnResult.
:- set_opv(cl_use_value, classof, claz_function),
   set_opv(use_value, compile_as, kw_function),
   set_opv(use_value, function, cl_use_value),
   DefunResult=use_value.
/*
:- side_effect(assert_lsp(use_value,
			  wl:lambda_def(defun, use_value, cl_use_value, [sys_value, c38_optional, condition], [[invoke_restart, [find_restart, [quote, use_value], condition], sys_value]]))).
*/
/*
:- side_effect(assert_lsp(use_value,
			  wl:arglist_info(use_value, cl_use_value, [sys_value, c38_optional, condition], arginfo{all:[sys_value, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value, condition], opt:[condition], req:[sys_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(use_value, wl:init_args(1, cl_use_value))).
*/
/*
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:46754 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'integer-string',[integer,'&optional',[radix,10]],[if,[=,integer,0],'$STRING'("0"),[labels,[[recur,[i,l],[if,[=,i,0],l,['multiple-value-bind',[ni,r],[floor,i,radix],[recur,ni,[cons,['code-char',[+,[if,[<,r,10],48,55],r]],l]]]]]],[apply,function(string),[if,[<,0,integer],[recur,integer,[]],[cons,['code-char',45],[recur,[-,integer],[]]]]]]]])
wl:lambda_def(defun, sys_integer_string, f_sys_integer_string, [integer, c38_optional, [sys_radix, 10]], [[if, [=, integer, 0], '$ARRAY'([*], claz_base_character, "0"), [labels, [[sys_recur, [sys_i, sys_l], [if, [=, sys_i, 0], sys_l, [multiple_value_bind, [sys_ni, sys_r], [floor, sys_i, sys_radix], [sys_recur, sys_ni, [cons, [code_char, [+, [if, [<, sys_r, 10], 48, 55], sys_r]], sys_l]]]]]], [apply, function(string), [if, [<, 0, integer], [sys_recur, integer, []], [cons, [code_char, 45], [sys_recur, [-, integer], []]]]]]]]).
wl:arglist_info(sys_integer_string, f_sys_integer_string, [integer, c38_optional, [sys_radix, 10]], arginfo{all:[integer, sys_radix], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[integer, sys_radix], opt:[sys_radix], req:[integer], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_integer_string).

/*

### Compiled:  `SYS::INTEGER-STRING` 
*/
f_sys_integer_string(Integer, RestNKeys, FnResult) :-
	nop(global_env(Env)),
	Env10=[[[bv(integer, Integer), bv(sys_radix, Radix)]|Env]|Env],
	opt_var(Env, sys_radix, Radix, true, 10, 1, RestNKeys),
	(   integer=:=0
	->  FnResult='$ARRAY'([*], claz_base_character, "0")
	;   must_maplist(must_det_l,
			 
			 [ (assert_lsp(sys_recur, wl:lambda_def(defun, sys_recur, f_sys_recur11, [sys_i, sys_l], [[if, [=, sys_i, 0], sys_l, [multiple_value_bind, [sys_ni, sys_r], [floor, sys_i, sys_radix], [sys_recur, sys_ni, [cons, [code_char, [+, [if, [<, sys_r, 10], 48, 55], sys_r]], sys_l]]]]])), assert_lsp(sys_recur, wl:arglist_info(sys_recur, f_sys_recur11, [sys_i, sys_l], arginfo{all:[sys_i, sys_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_i, sys_l], opt:0, req:[sys_i, sys_l], rest:0, sublists:0, whole:0})), !, assert_lsp(sys_recur, wl:init_args(exact_only, f_sys_recur11)), assert_lsp(sys_recur,  (f_sys_recur11(I, L, FnResult11):-nop(global_env(Env10)), Env41=[bv(sys_i, I), bv(sys_l, L)|Env10], get_var(Env41, sys_i, I_Get), (I_Get=:=0->get_var(Env41, sys_l, L_Get), FnResult11=L_Get;LEnv=[bv(sys_ni, []), bv(sys_r, [])|Env41], get_var(LEnv, sys_i, I_Get20), get_var(LEnv, sys_radix, Radix_Get), cl_floor(I_Get20, [Radix_Get], Floor_Ret), setq_from_values(LEnv, [sys_ni, sys_r]), get_var(LEnv, sys_ni, Ni_Get), get_var(LEnv, sys_r, R_Get), (R_Get<10->_292669248=48;_292669248=55), get_var(LEnv, sys_r, R_Get27), +(_292669248, R_Get27, Code_char_Param), cl_code_char(Code_char_Param, Code_char_Ret), get_var(LEnv, sys_l, L_Get28), _292668206=[Code_char_Ret|L_Get28], f_sys_recur1(Ni_Get, _292668206, LetResult), FnResult11=LetResult))))
			 ]),
	    (   0<integer
	    ->  f_sys_recur11(integer, [], TrueResult34),
		KeysNRest=TrueResult34
	    ;   cl_code_char(45, Code_char_Ret49),
		-(0, integer, Integer44),
		f_sys_recur11(Integer44, [], Recur11_Ret),
		KeysNRest=[Code_char_Ret49|Recur11_Ret]
	    ),
	    cl_apply(cl_string, KeysNRest, ElseResult36),
	    FnResult=ElseResult36
	).
:- set_opv(f_sys_integer_string, classof, claz_function),
   set_opv(sys_integer_string, compile_as, kw_function),
   set_opv(sys_integer_string, function, f_sys_integer_string),
   DefunResult=sys_integer_string.
/*
:- side_effect(assert_lsp(sys_integer_string,
			  wl:lambda_def(defun, sys_integer_string, f_sys_integer_string, [integer, c38_optional, [sys_radix, 10]], [[if, [=, integer, 0], '$ARRAY'([*], claz_base_character, "0"), [labels, [[sys_recur, [sys_i, sys_l], [if, [=, sys_i, 0], sys_l, [multiple_value_bind, [sys_ni, sys_r], [floor, sys_i, sys_radix], [sys_recur, sys_ni, [cons, [code_char, [+, [if, [<, sys_r, 10], 48, 55], sys_r]], sys_l]]]]]], [apply, function(string), [if, [<, 0, integer], [sys_recur, integer, []], [cons, [code_char, 45], [sys_recur, [-, integer], []]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_integer_string,
			  wl:arglist_info(sys_integer_string, f_sys_integer_string, [integer, c38_optional, [sys_radix, 10]], arginfo{all:[integer, sys_radix], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[integer, sys_radix], opt:[sys_radix], req:[integer], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_integer_string,
			  wl:init_args(1, f_sys_integer_string))).
*/
/*
(defun designator-symbol (designator)
  (if (symbolp designator)
      designator
      (find-symbol designator)))

#|
(defun symbolp (object) (or (null object) (eq (type-of object) 'symbol)))
(defun keywordp (object)
  (and (symbolp object)
       (string= (package-name (symbol-package object)) "KEYwORD")))
(defun make-symbol (name)
  (let ((symbol (makei 9 0 name nil nil nil nil (- 1) 0)))
    (imakunbound symbol 4)
    (imakunbound symbol 5)
    (imakunbound symbol 6)
    symbol))
(defvar *gensym-counter* 0)
(defun gen-sym (&optional x)
  (let ((prefix (if (stringp x) x "G"))
	(suffix (if (fixnump x)
		    x
		    (let ((x *gensym-counter*))
		      (setf *gensym-counter* (+ 1 *gensym-counter*))))))
    (make-symbol (conc-string prefix (integer-string suffix)))))
(let ((gentemp-counter 0))
  (defun gentemp (&optional (prefix "T") (package *package*))
    (setf gentemp-counter (+ 1 gentemp-counter))
    (intern (conc-string prefix (integer-string gentemp-counter))
	    package)))


(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))
(defun (setf get) (new-value symbol indicator &optional default)
  (setf (getf (symbol-plist symbol) indicator default) new-value))
(defun (setf rest) (new-tail list) (setf (cdr list) new-tail))

|#

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:47165 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-symbol',[designator],[if,[symbolp,designator],designator,['find-symbol',designator]]])
wl:lambda_def(defun, sys_designator_symbol, f_sys_designator_symbol, [sys_designator], [[if, [symbolp, sys_designator], sys_designator, [find_symbol, sys_designator]]]).
wl:arglist_info(sys_designator_symbol, f_sys_designator_symbol, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_designator_symbol).

/*

### Compiled:  `SYS::DESIGNATOR-SYMBOL` 
*/
f_sys_designator_symbol(Designator, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(sys_designator, Designator)|Env],
	get_var(Env16, sys_designator, Designator_Get),
	(   is_symbolp(Designator_Get)
	->  get_var(Env16, sys_designator, Designator_Get10),
	    FnResult=Designator_Get10
	;   get_var(Env16, sys_designator, Designator_Get11),
	    cl_find_symbol(Designator_Get11, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_sys_designator_symbol, classof, claz_function),
   set_opv(sys_designator_symbol, compile_as, kw_function),
   set_opv(sys_designator_symbol, function, f_sys_designator_symbol),
   DefunResult=sys_designator_symbol.
/*
:- side_effect(assert_lsp(sys_designator_symbol,
			  wl:lambda_def(defun, sys_designator_symbol, f_sys_designator_symbol, [sys_designator], [[if, [symbolp, sys_designator], sys_designator, [find_symbol, sys_designator]]]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_symbol,
			  wl:arglist_info(sys_designator_symbol, f_sys_designator_symbol, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_designator_symbol,
			  wl:init_args(exact_only, f_sys_designator_symbol))).
*/
/*

(defun symbolp (object) (or (null object) (eq (type-of object) 'symbol)))
(defun keywordp (object)
  (and (symbolp object)
       (string= (package-name (symbol-package object)) "KEYwORD")))
(defun make-symbol (name)
  (let ((symbol (makei 9 0 name nil nil nil nil (- 1) 0)))
    (imakunbound symbol 4)
    (imakunbound symbol 5)
    (imakunbound symbol 6)
    symbol))
(defvar *gensym-counter* 0)
(defun gen-sym (&optional x)
  (let ((prefix (if (stringp x) x "G"))
	(suffix (if (fixnump x)
		    x
		    (let ((x *gensym-counter*))
		      (setf *gensym-counter* (+ 1 *gensym-counter*))))))
    (make-symbol (conc-string prefix (integer-string suffix)))))
(let ((gentemp-counter 0))
  (defun gentemp (&optional (prefix "T") (package *package*))
    (setf gentemp-counter (+ 1 gentemp-counter))
    (intern (conc-string prefix (integer-string gentemp-counter))
	    package)))


(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))
(defun (setf get) (new-value symbol indicator &optional default)
  (setf (getf (symbol-plist symbol) indicator default) new-value))
(defun (setf rest) (new-tail list) (setf (cdr list) new-tail))

*/
/*
(defun remprop (symbol indicator) (remf (symbol-plist symbol) indicator))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:48501 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,remprop,[symbol,indicator],[remf,['symbol-plist',symbol],indicator]])
wl:lambda_def(defun, remprop, cl_remprop, [symbol, sys_indicator], [[remf, [symbol_plist, symbol], sys_indicator]]).
wl:arglist_info(remprop, cl_remprop, [symbol, sys_indicator], arginfo{all:[symbol, sys_indicator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol, sys_indicator], opt:0, req:[symbol, sys_indicator], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_remprop).

/*

### Compiled:  `CL:REMPROP` 
*/
cl_remprop(Symbol, Indicator, FnResult) :-
	nop(global_env(Env)),
	Env8=[bv(symbol, Symbol), bv(sys_indicator, Indicator)|Env],
	cl_remf([symbol_plist, symbol], sys_indicator, Indicator11),
	Indicator11=FnResult.
:- set_opv(cl_remprop, classof, claz_function),
   set_opv(remprop, compile_as, kw_function),
   set_opv(remprop, function, cl_remprop),
   DefunResult=remprop.
/*
:- side_effect(assert_lsp(remprop,
			  wl:lambda_def(defun, remprop, cl_remprop, [symbol, sys_indicator], [[remf, [symbol_plist, symbol], sys_indicator]]))).
*/
/*
:- side_effect(assert_lsp(remprop,
			  wl:arglist_info(remprop, cl_remprop, [symbol, sys_indicator], arginfo{all:[symbol, sys_indicator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol, sys_indicator], opt:0, req:[symbol, sys_indicator], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(remprop, wl:init_args(exact_only, cl_remprop))).
*/
/*
(defun boundp (symbol) (iboundp symbol 4))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:48576 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,boundp,[symbol],[iboundp,symbol,4]])
wl:lambda_def(defun, boundp, cl_boundp, [symbol], [[sys_iboundp, symbol, 4]]).
wl:arglist_info(boundp, cl_boundp, [symbol], arginfo{all:[symbol], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol], opt:0, req:[symbol], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_boundp).

/*

### Compiled:  `CL:BOUNDP` 
*/
cl_boundp(Symbol, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(symbol, Symbol)|Env],
	get_var(Env9, symbol, Symbol_Get),
	f_sys_iboundp(Symbol_Get, 4, Iboundp_Ret),
	Iboundp_Ret=FnResult.
:- set_opv(cl_boundp, classof, claz_function),
   set_opv(boundp, compile_as, kw_function),
   set_opv(boundp, function, cl_boundp),
   DefunResult=boundp.
/*
:- side_effect(assert_lsp(boundp,
			  wl:lambda_def(defun, boundp, cl_boundp, [symbol], [[sys_iboundp, symbol, 4]]))).
*/
/*
:- side_effect(assert_lsp(boundp,
			  wl:arglist_info(boundp, cl_boundp, [symbol], arginfo{all:[symbol], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol], opt:0, req:[symbol], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(boundp, wl:init_args(exact_only, cl_boundp))).
*/
/*
(defun makunbound (symbol) (imakunbound symbol 4))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:48620 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,makunbound,[symbol],[imakunbound,symbol,4]])
wl:lambda_def(defun, makunbound, cl_makunbound, [symbol], [[sys_imakunbound, symbol, 4]]).
wl:arglist_info(makunbound, cl_makunbound, [symbol], arginfo{all:[symbol], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol], opt:0, req:[symbol], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_makunbound).

/*

### Compiled:  `CL:MAKUNBOUND` 
*/
cl_makunbound(Symbol, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(symbol, Symbol)|Env],
	get_var(Env9, symbol, Symbol_Get),
	f_sys_imakunbound(Symbol_Get, 4, Imakunbound_Ret),
	Imakunbound_Ret=FnResult.
:- set_opv(cl_makunbound, classof, claz_function),
   set_opv(makunbound, compile_as, kw_function),
   set_opv(makunbound, function, cl_makunbound),
   DefunResult=makunbound.
/*
:- side_effect(assert_lsp(makunbound,
			  wl:lambda_def(defun, makunbound, cl_makunbound, [symbol], [[sys_imakunbound, symbol, 4]]))).
*/
/*
:- side_effect(assert_lsp(makunbound,
			  wl:arglist_info(makunbound, cl_makunbound, [symbol], arginfo{all:[symbol], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol], opt:0, req:[symbol], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(makunbound, wl:init_args(exact_only, cl_makunbound))).
*/
/*
(defun set (symbol value) (setf (symbol-value symbol) value))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:48672 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,set,[symbol,value],[setf,['symbol-value',symbol],value]])
wl:lambda_def(defun, set, cl_set, [symbol, sys_value], [[setf, [symbol_value, symbol], sys_value]]).
wl:arglist_info(set, cl_set, [symbol, sys_value], arginfo{all:[symbol, sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol, sys_value], opt:0, req:[symbol, sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_set).

/*

### Compiled:  `CL:SET` 
*/
cl_set(Symbol, Value, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(symbol, Symbol), bv(sys_value, Value)|Env],
	get_var(Env10, symbol, Symbol_Get),
	get_var(Env10, sys_value, Value_Get),
	cl_set(Symbol_Get, Value_Get, Set_Ret),
	Set_Ret=FnResult.
:- set_opv(cl_set, classof, claz_function),
   set_opv(set, compile_as, kw_function),
   set_opv(set, function, cl_set),
   DefunResult=set.
/*
:- side_effect(assert_lsp(set,
			  wl:lambda_def(defun, set, cl_set, [symbol, sys_value], [[setf, [symbol_value, symbol], sys_value]]))).
*/
/*
:- side_effect(assert_lsp(set,
			  wl:arglist_info(set, cl_set, [symbol, sys_value], arginfo{all:[symbol, sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol, sys_value], opt:0, req:[symbol, sys_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(set, wl:init_args(exact_only, cl_set))).
*/
/*
(defun designator-string (designator)
  (if (stringp designator)
      designator
      (if (characterp designator)
	  (string designator)
	  (symbol-name designator))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:48735 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-string',[designator],[if,[stringp,designator],designator,[if,[characterp,designator],[string,designator],['symbol-name',designator]]]])
wl:lambda_def(defun, sys_designator_string, f_sys_designator_string, [sys_designator], [[if, [stringp, sys_designator], sys_designator, [if, [characterp, sys_designator], [string, sys_designator], [symbol_name, sys_designator]]]]).
wl:arglist_info(sys_designator_string, f_sys_designator_string, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_sys_designator_string).

/*

### Compiled:  `SYS::DESIGNATOR-STRING` 
*/
f_sys_designator_string(Designator, ElseResult20) :-
	nop(global_env(Env)),
	Env23=[bv(sys_designator, Designator)|Env],
	get_var(Env23, sys_designator, Designator_Get),
	(   is_stringp(Designator_Get)
	->  get_var(Env23, sys_designator, Designator_Get10),
	    ElseResult20=Designator_Get10
	;   get_var(Env23, sys_designator, Designator_Get12),
	    (   string:is_characterp(Designator_Get12)
	    ->  get_var(Env23, sys_designator, Designator_Get15),
		cl_string(Designator_Get15, TrueResult),
		ElseResult20=TrueResult
	    ;   get_var(Env23, sys_designator, Designator_Get16),
		cl_symbol_name(Designator_Get16, ElseResult),
		ElseResult20=ElseResult
	    )
	).
:- set_opv(f_sys_designator_string, classof, claz_function),
   set_opv(sys_designator_string, compile_as, kw_function),
   set_opv(sys_designator_string, function, f_sys_designator_string),
   DefunResult=sys_designator_string.
/*
:- side_effect(assert_lsp(sys_designator_string,
			  wl:lambda_def(defun, sys_designator_string, f_sys_designator_string, [sys_designator], [[if, [stringp, sys_designator], sys_designator, [if, [characterp, sys_designator], [string, sys_designator], [symbol_name, sys_designator]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_string,
			  wl:arglist_info(sys_designator_string, f_sys_designator_string, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_designator_string,
			  wl:init_args(exact_only, f_sys_designator_string))).
*/
/*
(defvar *package* (car (cdr *packages*)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:48911 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*package*',[car,[cdr,'*packages*']]])
:- get_var(AEnv, sys_xx_packages_xx, Xx_packages_xx_Get),
   cl_cdr(Xx_packages_xx_Get, Car_Param),
   cl_car(Car_Param, Xx_package_xx),
   set_var(AEnv, defvar, xx_package_xx, Xx_package_xx).
/*
(defun list-all-packages ()
  (copy-list *packages*))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:48954 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'list-all-packages',[],['copy-list','*packages*']])
wl:lambda_def(defun, list_all_packages, cl_list_all_packages, [], [[copy_list, sys_xx_packages_xx]]).
wl:arglist_info(list_all_packages, cl_list_all_packages, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, cl_list_all_packages).

/*

### Compiled:  `CL:LIST-ALL-PACKAGES` 
*/
cl_list_all_packages(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv, sys_xx_packages_xx, Xx_packages_xx_Get),
	cl_copy_list(Xx_packages_xx_Get, Copy_list_Ret),
	Copy_list_Ret=FnResult.
:- set_opv(cl_list_all_packages, classof, claz_function),
   set_opv(list_all_packages, compile_as, kw_function),
   set_opv(list_all_packages, function, cl_list_all_packages),
   DefunResult=list_all_packages.
/*
:- side_effect(assert_lsp(list_all_packages,
			  wl:lambda_def(defun, list_all_packages, cl_list_all_packages, [], [[copy_list, sys_xx_packages_xx]]))).
*/
/*
:- side_effect(assert_lsp(list_all_packages,
			  wl:arglist_info(list_all_packages, cl_list_all_packages, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(list_all_packages,
			  wl:init_args(exact_only, cl_list_all_packages))).
*/
/*
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:49010 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,/=,[number,'&rest',numbers],[tagbody,start,[when,numbers,[dolist,[n,numbers],[when,[=,number,n],['return-from',/=]]],[setq,number,[pop,numbers]],[go,start]]],t])
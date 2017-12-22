#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init-1" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Fri Dec 22 04:05:57 2017

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
:-lisp_compile_to_prolog(pkg_sys,['in-package','$STRING'("SYSTEM")])
:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored6).
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
wl:arglist_info(sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [Form_Param, Inverse_Param, Setf_function_Param], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_get_setf_method_inverse).

/*

### Compiled:  `SYS::GET-SETF-METHOD-INVERSE` 
*/
f_sys_get_setf_method_inverse(Form_Param, Inverse_Param, Setf_function_Param, FnResult) :-
	Env=[bv(sys_form, Form_Param), bv(sys_inverse, Inverse_Param), bv(sys_setf_function, Setf_function_Param)],
	cl_gensym(New_var_Init),
	LEnv=[[bv(sys_new_var, New_var_Init), bv(sys_vars, []), bv(sys_vals, [])]|Env],
	cl_cdr(Form_Param, List),
	BV=bv(sys_x, Ele),
	Env2=[BV|LEnv],
	forall(member(Ele, List),
	       ( nb_setarg(2, BV, Ele),
		 cl_push([gensym], sys_vars, Vars),
		 cl_push(sys_x, sys_vals, Vals)
	       )),
	get_var(LEnv, sys_vals, Vals_Get31),
	cl_nreverse(Vals_Get31, Vals59),
	set_var(LEnv, sys_vals, Vals59),
	get_var(LEnv, sys_new_var, New_var_Get),
	get_var(LEnv, sys_vars, Vars_Get),
	CAR=[New_var_Get],
	(   Setf_function_Param\==[]
	->  get_var(LEnv, sys_new_var, New_var_Get37),
	    get_var(LEnv, sys_vars, Vars_Get38),
	    bq_append(Inverse_Param, [New_var_Get37|Vars_Get38], TrueResult51),
	    ElseResult52=TrueResult51
	;   cl_car(Inverse_Param, PredArgResult),
	    (   is_functionp(PredArgResult)
	    ->  get_var(LEnv, sys_new_var, New_var_Get45),
		get_var(LEnv, sys_vars, Vars_Get44),
		bq_append(Vars_Get44, [New_var_Get45], Bq_append_Ret),
		bq_append([funcall|Inverse_Param], Bq_append_Ret, TrueResult),
		ElseResult52=TrueResult
	    ;   get_var(LEnv, sys_new_var, New_var_Get48),
		get_var(LEnv, sys_vars, Vars_Get47),
		bq_append(Vars_Get47, [New_var_Get48], Bq_append_Ret61),
		bq_append(Inverse_Param, Bq_append_Ret61, ElseResult),
		ElseResult52=ElseResult
	    )
	),
	cl_car(Form_Param, Car_Ret),
	get_var(LEnv, sys_vars, Vars_Get54),
	nb_setval('$mv_return',
		  [Vars_Get, Vals_Get31, CAR, ElseResult52, [Car_Ret|Vars_Get54]]),
	Vars_Get=FnResult.
:- set_opv(f_sys_get_setf_method_inverse, classof, claz_function),
   set_opv(sys_get_setf_method_inverse, compile_as, kw_function),
   set_opv(sys_get_setf_method_inverse, function, f_sys_get_setf_method_inverse),
   _Ignored6=sys_get_setf_method_inverse.
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:arglist_info(sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [Form_Param, Inverse_Param, Setf_function_Param], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:init_args(exact_only, sys_get_setf_method_inverse))).
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
wl:arglist_info(sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [Form_Param, Environment_Param], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_expand_or_get_setf_inverse).

/*

### Compiled:  `SYS::EXPAND-OR-GET-SETF-INVERSE` 
*/
f_sys_expand_or_get_setf_inverse(Form_Param, Environment_Param, FnResult) :-
	Env=[bv(sys_form, Form_Param), bv(sys_environment, Environment_Param)],
	LEnv=[[bv(sys_expansion, []), bv(sys_expanded, [])]|Env],
	cl_macroexpand_1([Form_Param, Environment_Param], Macroexpand_1_Ret),
	setq_from_values(LEnv, [sys_expansion, sys_expanded]),
	get_var(LEnv, sys_expanded, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, sys_expansion, Expansion_Get),
	    cl_get_setf_expansion(Expansion_Get,
				  [Environment_Param],
				  TrueResult),
	    FnResult=TrueResult
	;   f_sys_get_setf_method_inverse(Form_Param,
					  
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
   _Ignored6=sys_expand_or_get_setf_inverse.
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:arglist_info(sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [Form_Param, Environment_Param], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:init_args(exact_only, sys_expand_or_get_setf_inverse))).
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
wl:arglist_info(get_setf_expansion, [sys_form, c38_optional, sys_environment], [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, get_setf_expansion).

/*

### Compiled:  `CL:GET-SETF-EXPANSION` 
*/
cl_get_setf_expansion(Form_Param, RestNKeys, LetResult27) :-
	Env=[bv(sys_form, Form_Param), bv(sys_environment, Environment_Param)],
	opt_var(Env, sys_environment, Environment_Param, true, [], 1, RestNKeys),
	LEnv=[[bv(sys_temp, [])]|Env],
	(   is_symbolp(Form_Param)
	->  LEnv26=[[bv(sys_expansion, []), bv(sys_expanded, [])]|LEnv],
	    get_var(LEnv26, sys_environment, Environment_Get),
	    cl_macroexpand_1([Form_Param, Environment_Get], Macroexpand_1_Ret),
	    setq_from_values(LEnv26, [sys_expansion, sys_expanded]),
	    get_var(LEnv26, sys_expanded, IFTEST30),
	    (   IFTEST30\==[]
	    ->  get_var(LEnv26, sys_environment, Environment_Get34),
		get_var(LEnv26, sys_expansion, Expansion_Get),
		cl_get_setf_expansion(Expansion_Get,
				      [Environment_Get34],
				      TrueResult),
		LetResult27=TrueResult
	    ;   cl_gensym(New_var_Init),
		LEnv35=[[bv(sys_new_var, New_var_Init)]|LEnv26],
		get_var(LEnv35, sys_new_var, New_var_Get),
		CAR=[New_var_Get],
		get_var(LEnv35, sys_new_var, New_var_Get40),
		nb_setval('$mv_return',
			  
			  [ [],
			    [],
			    CAR,
			    [setq, Form_Param, New_var_Get40],
			    Form_Param
			  ]),
		LetResult27=[]
	    )
	;   cl_car(Form_Param, Get_Param),
	    cl_get(Get_Param, sys_setf_inverse, [], IFTEST45),
	    set_var(LEnv, sys_temp, IFTEST45),
	    (   IFTEST45\==[]
	    ->  get_var(LEnv, sys_temp, Temp_Get),
		f_sys_get_setf_method_inverse(Form_Param,
					      [Temp_Get],
					      [],
					      TrueResult60),
		LetResult27=TrueResult60
	    ;   cl_car(Form_Param, Get_Param67),
		cl_get(Get_Param67, sys_setf_expander, [], IFTEST51),
		set_var(LEnv, sys_temp, IFTEST51),
		(   IFTEST51\==[]
		->  get_var(LEnv, sys_environment, Environment_Get55),
		    f_sys_temp(Form_Param, Environment_Get55, TrueResult58),
		    LetResult27=TrueResult58
		;   get_var(LEnv, sys_environment, Environment_Get57),
		    f_sys_expand_or_get_setf_inverse(Form_Param,
						     Environment_Get57,
						     ElseResult),
		    LetResult27=ElseResult
		)
	    )
	).
:- set_opv(cl_get_setf_expansion, classof, claz_function),
   set_opv(get_setf_expansion, compile_as, kw_function),
   set_opv(get_setf_expansion, function, cl_get_setf_expansion),
   _Ignored6=get_setf_expansion.
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:lambda_def(defun, get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:arglist_info(get_setf_expansion, [sys_form, c38_optional, sys_environment], [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:init_args(1, get_setf_expansion))).
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
wl:arglist_info(sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [args, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[args, sys_environment], opt:0, req:0, rest:[args], sublists:0, whole:0}).
wl: init_args(0, sys_abcl_setf).

/*

### Compiled:  `SYS::ABCL-SETF` 
*/
f_sys_abcl_setf(Args_Param, FnResult) :-
	TLEnv7=[bv(args, Args_Param), bv(sys_environment, Environment_Param)],
	get_env(TLEnv7, sys_environment, Environment_Param),
	catch(( ( get_var(TLEnv7, args, Args_Get),
		  cl_length(Args_Get, Numargs_Init),
		  LEnv=[[bv(sys_numargs, Numargs_Init)]|TLEnv7],
		  get_var(LEnv, sys_numargs, Numargs_Get),
		  (   Numargs_Get=:=2
		  ->  get_var(LEnv, args, Args_Get29),
		      cl_car(Args_Get29, Place_Init),
		      get_var(LEnv, args, Args_Get30),
		      cl_second(Args_Get30, Value_form_Init),
		      LEnv27=[[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)]|LEnv],
		      get_var(LEnv27, sys_place, Place_Get),
		      (   Place_Get\=[CAR|CDR]
		      ->  get_var(LEnv27, sys_place, Place_Get37),
			  get_var(LEnv27, sys_value_form, Value_form_Get),
			  LetResult28=[setq, Place_Get37, Value_form_Get]
		      ;   LEnv39=[[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_store_vars, []), bv(sys_setter, []), bv(sys_getter, [])]|LEnv27],
			  get_var(LEnv39, sys_environment, Environment_Get),
			  get_var(LEnv39, sys_place, Place_Get41),
			  cl_get_setf_expansion(Place_Get41,
						[Environment_Get],
						Setf_expansion_Ret),
			  setq_from_values(LEnv39,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_store_vars,
					     sys_setter,
					     sys_getter
					   ]),
			  get_var(LEnv39, sys_place, Place_Get45),
			  cl_car(Place_Get45, Get_Param),
			  cl_get(Get_Param, sys_setf_inverse, [], Inverse_Init),
			  LEnv43=[[bv(sys_inverse, Inverse_Init)]|LEnv39],
			  get_var(LEnv43, sys_inverse, IFTEST49),
			  (   IFTEST49\==[]
			  ->  get_var(LEnv43, sys_inverse, Inverse_Get52),
			      get_var(LEnv43, sys_setter, Setter_Get),
			      cl_car(Setter_Get, Car_Ret),
			      cl_eq(Inverse_Get52, Car_Ret, TrueResult),
			      IFTEST47=TrueResult
			  ;   IFTEST47=[]
			  ),
			  (   IFTEST47\==[]
			  ->  get_var(LEnv43, sys_inverse, Inverse_Get56),
			      (   is_functionp(Inverse_Get56)
			      ->  get_var(LEnv43, sys_inverse, Inverse_Get59),
				  get_var(LEnv43, sys_place, Place_Get60),
				  cl_cdr(Place_Get60, Cdr_Ret),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get61),
				  bq_append([Inverse_Get59|Cdr_Ret],
					    [Value_form_Get61],
					    Bq_append_Ret),
				  LetResult28=[funcall|Bq_append_Ret]
			      ;   get_var(LEnv43, sys_inverse, Inverse_Get62),
				  get_var(LEnv43, sys_place, Place_Get63),
				  cl_cdr(Place_Get63, Cdr_Ret146),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get64),
				  bq_append([Inverse_Get62|Cdr_Ret146],
					    [Value_form_Get64],
					    ElseResult),
				  LetResult28=ElseResult
			      )
			  ;   (   get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get),
				  cl_null(Store_vars_Get, FORM1_Res),
				  FORM1_Res\==[],
				  IFTEST66=FORM1_Res
			      ->  true
			      ;   get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get69),
				  cl_cdr(Store_vars_Get69, Cdr_Ret147),
				  IFTEST66=Cdr_Ret147
			      ),
			      (   IFTEST66\==[]
			      ->  get_var(LEnv43, sys_dummies, Dummies_Get),
				  get_var(LEnv43, sys_vals, Vals_Get),
				  cl_mapcar(function(list),
					    [Dummies_Get, Vals_Get],
					    Mapcar_Ret),
				  get_var(LEnv43, sys_setter, Setter_Get75),
				  get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get73),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get74),
				  LetResult28=[let_xx, Mapcar_Ret, [multiple_value_bind, Store_vars_Get73, Value_form_Get74, Setter_Get75]]
			      ;   get_var(LEnv43, sys_dummies, Dummies_Get76),
				  get_var(LEnv43, sys_vals, Vals_Get77),
				  cl_mapcar(function(list),
					    [Dummies_Get76, Vals_Get77],
					    Bq_append_Param),
				  get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get78),
				  cl_car(Store_vars_Get78, Car_Ret149),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get79),
				  CAR151=[Car_Ret149, Value_form_Get79],
				  bq_append(Bq_append_Param,
					    [CAR151],
					    Bq_append_Ret150),
				  get_var(LEnv43, sys_setter, Setter_Get80),
				  LetResult28=[let_xx, Bq_append_Ret150, Setter_Get80]
			      )
			  )
		      )
		  ;   get_var(LEnv, sys_numargs, Numargs_Get88),
		      (   mth:is_oddp(Numargs_Get88)
		      ->  cl_error(
				   [ '$ARRAY'([*],
					      claz_base_character,
					      "Odd number of arguments to SETF.")
				   ],
				   TrueResult131),
			  LetResult28=TrueResult131
		      ;   get_var(LEnv, args, Args_Get93),
			  GoEnv=[[bv(sys_a, Args_Get93), bv(sys_l, [])]|LEnv],
			  catch(( call_addr_block(GoEnv,
						  (push_label(do_label_1), get_var(GoEnv, sys_a, IFTEST114), (IFTEST114==[]->cl_nreverse(L_Get107, Nreverse_Ret), throw(block_exit([], [progn|Nreverse_Ret])), _TBResult=ThrowResult118;cl_car(IFTEST96, Car_Ret153), cl_cadr(IFTEST96, Cadr_Ret), CAR155=[setf, Car_Ret153, Cadr_Ret], get_var(GoEnv, sys_l, L_Get124), L=[CAR155|L_Get124], set_var(GoEnv, sys_l, L), get_var(GoEnv, sys_a, A_Get125), cl_cddr(A_Get125, A), set_var(GoEnv, sys_a, A), goto(do_label_1, GoEnv), _TBResult=_GORES126)),
						  
						  [ addr(addr_tagbody_1_do_label_1,
							 do_label_1,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_a, IFTEST96), (IFTEST96==[]->get_var(AEnv, sys_l, L_Get107), cl_nreverse(L_Get107, Nreverse_Ret156), throw(block_exit([], [progn|Nreverse_Ret156])), _30242=ThrowResult;cl_car(IFTEST96, Car_Ret157), cl_cadr(IFTEST96, Cadr_Ret158), CAR159=[setf, Car_Ret157, Cadr_Ret158], Set_var_Ret=[CAR159|L_Get107], set_var(AEnv, sys_l, Set_var_Ret), cl_cddr(IFTEST96, Cddr_Ret), set_var(AEnv, sys_a, Cddr_Ret), goto(do_label_1, AEnv), _30242=_GORES)))
						  ]),
				  []=LetResult92
				),
				block_exit([], LetResult92),
				true),
			  LetResult28=LetResult92
		      )
		  )
		),
		LetResult28=MFResult
	      ),
	      block_exit(sys_abcl_setf, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(f_sys_abcl_setf, classof, claz_macro),
   set_opv(sys_abcl_setf, compile_as, kw_operator),
   set_opv(sys_abcl_setf, function, f_sys_abcl_setf),
   _Ignored6=sys_abcl_setf.
/*
:- side_effect(assert_lsp(sys_abcl_setf,
			  wl:lambda_def(defmacro, sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [progn, [let, [[sys_numargs, [length, args]]], [cond, [[=, sys_numargs, 2], [let, [[sys_place, [first, args]], [sys_value_form, [second, args]]], [if, [atom, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]], [progn, [multiple_value_bind, [sys_dummies, sys_vals, sys_store_vars, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_environment], [let, [[sys_inverse, [get, [car, sys_place], [quote, sys_setf_inverse]]]], [if, [and, sys_inverse, [eq, sys_inverse, [car, sys_setter]]], [if, [functionp, sys_inverse], ['#BQ', [funcall, ['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]], ['#BQ', [['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]]], [if, [or, [null, sys_store_vars], [cdr, sys_store_vars]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]]], [multiple_value_bind, ['#COMMA', sys_store_vars], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], ['#COMMA', [list, [car, sys_store_vars], sys_value_form]]], ['#COMMA', sys_setter]]]]]]]]]]], [[oddp, sys_numargs], [error, '$ARRAY'([*], claz_base_character, "Odd number of arguments to SETF.")]], [t, [do, [[sys_a, args, [cddr, sys_a]], [sys_l, []]], [[null, sys_a], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]]], [setq, sys_l, [cons, [list, [quote, setf], [car, sys_a], [cadr, sys_a]], sys_l]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(sys_abcl_setf,
			  wl:arglist_info(sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [args, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[args, sys_environment], opt:0, req:0, rest:[args], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_abcl_setf, wl:init_args(0, sys_abcl_setf))).
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
wl:arglist_info(incf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, incf).

/*

### Compiled:  `CL:INCF` 
*/
cl_incf(Place_Param, RestNKeys, FnResult) :-
	TLEnv7=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
	opt_var(TLEnv7, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
	get_var(TLEnv7, sys_delta, Delta_Get),
	[setf, Place_Param, [+, Place_Param, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_incf, classof, claz_macro),
   set_opv(incf, compile_as, kw_operator),
   set_opv(incf, function, cl_incf),
   _Ignored6=incf.
/*
:- side_effect(assert_lsp(incf,
			  wl:lambda_def(defmacro, incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]))).
*/
/*
:- side_effect(assert_lsp(incf,
			  wl:arglist_info(incf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(incf, wl:init_args(1, incf))).
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
wl:arglist_info(decf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, decf).

/*

### Compiled:  `CL:DECF` 
*/
cl_decf(Place_Param, RestNKeys, FnResult) :-
	TLEnv7=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
	opt_var(TLEnv7, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
	get_var(TLEnv7, sys_delta, Delta_Get),
	[setf, Place_Param, [-, Place_Param, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_decf, classof, claz_macro),
   set_opv(decf, compile_as, kw_operator),
   set_opv(decf, function, cl_decf),
   _Ignored6=decf.
/*
:- side_effect(assert_lsp(decf,
			  wl:lambda_def(defmacro, decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [-, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]))).
*/
/*
:- side_effect(assert_lsp(decf,
			  wl:arglist_info(decf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(decf, wl:init_args(1, decf))).
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
% conds:-[[[eq,_55470,[quote,1]],[progn,[setq,sys_v,[car,rest]]]],[[eq,_55470,[quote,2]],[progn,[setq,end,[car,rest],sys_v,[cadr,rest]]]],[t,[type_error,_55706,[quote,[member,1,2]]]]].
*/
wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], [[let, [[end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, start, kw_end1, end], sys_v]]]).
wl:arglist_info(sys_pf_set_subseq, [sequence, start, c38_rest, rest], [sequence, start, rest], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, start, rest], opt:0, req:[sequence, start], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, sys_pf_set_subseq).

/*

### Compiled:  `SYS::%SET-SUBSEQ` 
*/
f_sys_pf_set_subseq(Sequence_Param, Start_Param, Rest_Param, FnResult) :-
	Env=[bv(sequence, Sequence_Param), bv(start, Start_Param), bv(rest, Rest_Param)],
	LEnv=[[bv(end, []), bv(sys_v, [])]|Env],
	get_var(LEnv, rest, Rest_Get),
	cl_length(Rest_Get, avar(PredArg1Result, att(preserved_var, t, []))),
	(   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), 1)
	->  get_var(LEnv, rest, Rest_Get29),
	    cl_car(Rest_Get29, TrueResult36),
	    set_var(LEnv, sys_v, TrueResult36),
	    ElseResult37=TrueResult36
	;   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), 2)
	->  get_var(LEnv, rest, Rest_Get32),
	    cl_car(Rest_Get32, End),
	    set_var(LEnv, end, End),
	    get_var(LEnv, rest, Rest_Get33),
	    cl_cadr(Rest_Get33, TrueResult),
	    set_var(LEnv, sys_v, TrueResult),
	    ElseResult37=TrueResult
	;   cl_type_error(avar(CAR, att(preserved_var, t, [])),
			  [member, 1, 2],
			  ElseResult),
	    ElseResult37=ElseResult
	),
	get_var(LEnv, end, End_Get),
	get_var(LEnv, sys_v, V_Get),
	cl_replace(Sequence_Param,
		   V_Get,
		   [kw_start1, Start_Param, kw_end1, End_Get],
		   Replace_Ret),
	get_var(LEnv, sys_v, V_Get42),
	V_Get42=FnResult.
:- set_opv(f_sys_pf_set_subseq, classof, claz_function),
   set_opv(sys_pf_set_subseq, compile_as, kw_function),
   set_opv(sys_pf_set_subseq, function, f_sys_pf_set_subseq),
   _Ignored6=sys_pf_set_subseq.
/*
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], [[let, [[end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, start, kw_end1, end], sys_v]]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  wl:arglist_info(sys_pf_set_subseq, [sequence, start, c38_rest, rest], [sequence, start, rest], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, start, rest], opt:0, req:[sequence, start], rest:[rest], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_subseq, wl:init_args(2, sys_pf_set_subseq))).
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
wl:arglist_info(sys_pf_define_setf_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [Name_Param, Expander_Param, Inverse_Param, Doc_Param], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_define_setf_macro).

/*

### Compiled:  `SYS::%DEFINE-SETF-MACRO` 
*/
f_sys_pf_define_setf_type_macro(Name_Param, Expander_Param, Inverse_Param, Doc_Param, Name_Param) :-
	Env=[bv(sys_doc, Doc_Param)],
	cl_declare([ignore, sys_doc], Declare_Ret),
	(   Inverse_Param\==[]
	->  f_sys_put(Name_Param, sys_setf_inverse, Inverse_Param, TrueResult),
	    _24678=TrueResult
	;   _24678=[]
	),
	(   Expander_Param\==[]
	->  f_sys_put(Name_Param,
		      sys_setf_expander,
		      Expander_Param,
		      TrueResult33),
	    _24838=TrueResult33
	;   _24838=[]
	).
:- set_opv(f_sys_pf_define_setf_type_macro, classof, claz_function),
   set_opv(sys_pf_define_setf_macro, compile_as, kw_function),
   set_opv(sys_pf_define_setf_macro, function, f_sys_pf_define_setf_type_macro),
   _Ignored6=sys_pf_define_setf_macro.
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:lambda_def(defun, sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [[declare, [ignore, sys_doc]], [when, sys_inverse, [sys_put, sys_name, [quote, sys_setf_inverse], sys_inverse]], [when, sys_expander, [sys_put, sys_name, [quote, sys_setf_expander], sys_expander]], sys_name]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:arglist_info(sys_pf_define_setf_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [Name_Param, Expander_Param, Inverse_Param, Doc_Param], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:init_args(exact_only, sys_pf_define_setf_macro))).
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
wl:arglist_info(defsetf, [sys_access_function, sys_update_function], [Access_function_Param, Update_function_Param], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, defsetf).

/*

### Compiled:  `CL:DEFSETF` 
*/
cl_defsetf(Access_function_Param, Update_function_Param, FnResult) :-
	[eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, Access_function_Param], [quote, sys_setf_inverse], [quote, Update_function_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_defsetf, classof, claz_macro),
   set_opv(defsetf, compile_as, kw_operator),
   set_opv(defsetf, function, cl_defsetf),
   _Ignored6=defsetf.
/*
:- side_effect(assert_lsp(defsetf,
			  wl:lambda_def(defmacro, defsetf, cl_defsetf, [sys_access_function, sys_update_function], [progn, ['#BQ', [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, ['#COMMA', sys_access_function]], [quote, sys_setf_inverse], [quote, ['#COMMA', sys_update_function]]]]]]))).
*/
/*
:- side_effect(assert_lsp(defsetf,
			  wl:arglist_info(defsetf, [sys_access_function, sys_update_function], [Access_function_Param, Update_function_Param], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(defsetf, wl:init_args(exact_only, defsetf))).
*/
/*
(defun %set-caar (x v) (set-car (car x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5791 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caar',[x,v],['set-car',[car,x],v]])
wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caar).

/*

### Compiled:  `SYS::%SET-CAAR` 
*/
f_sys_pf_set_caar(X_Param, V_Param, FnResult) :-
	cl_car(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caar, classof, claz_function),
   set_opv(sys_pf_set_caar, compile_as, kw_function),
   set_opv(sys_pf_set_caar, function, f_sys_pf_set_caar),
   _Ignored6=sys_pf_set_caar.
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:arglist_info(sys_pf_set_caar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:init_args(exact_only, sys_pf_set_caar))).
*/
/*
(defun %set-cadr (x v) (set-car (cdr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5836 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadr',[x,v],['set-car',[cdr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadr).

/*

### Compiled:  `SYS::%SET-CADR` 
*/
f_sys_pf_set_cadr(X_Param, V_Param, FnResult) :-
	cl_cdr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadr, classof, claz_function),
   set_opv(sys_pf_set_cadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadr, function, f_sys_pf_set_cadr),
   _Ignored6=sys_pf_set_cadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:arglist_info(sys_pf_set_cadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:init_args(exact_only, sys_pf_set_cadr))).
*/
/*
(defun %set-cdar (x v) (set-cdr (car x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5881 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdar',[x,v],['set-cdr',[car,x],v]])
wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdar).

/*

### Compiled:  `SYS::%SET-CDAR` 
*/
f_sys_pf_set_cdar(X_Param, V_Param, FnResult) :-
	cl_car(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdar, classof, claz_function),
   set_opv(sys_pf_set_cdar, compile_as, kw_function),
   set_opv(sys_pf_set_cdar, function, f_sys_pf_set_cdar),
   _Ignored6=sys_pf_set_cdar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:arglist_info(sys_pf_set_cdar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:init_args(exact_only, sys_pf_set_cdar))).
*/
/*
(defun %set-cddr (x v) (set-cdr (cdr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5926 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddr',[x,v],['set-cdr',[cdr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddr).

/*

### Compiled:  `SYS::%SET-CDDR` 
*/
f_sys_pf_set_cddr(X_Param, V_Param, FnResult) :-
	cl_cdr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddr, classof, claz_function),
   set_opv(sys_pf_set_cddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddr, function, f_sys_pf_set_cddr),
   _Ignored6=sys_pf_set_cddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:arglist_info(sys_pf_set_cddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:init_args(exact_only, sys_pf_set_cddr))).
*/
/*
(defun %set-caaar (x v) (set-car (caar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5971 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaar',[x,v],['set-car',[caar,x],v]])
wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaar).

/*

### Compiled:  `SYS::%SET-CAAAR` 
*/
f_sys_pf_set_caaar(X_Param, V_Param, FnResult) :-
	cl_caar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaar, classof, claz_function),
   set_opv(sys_pf_set_caaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaar, function, f_sys_pf_set_caaar),
   _Ignored6=sys_pf_set_caaar.
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:arglist_info(sys_pf_set_caaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:init_args(exact_only, sys_pf_set_caaar))).
*/
/*
(defun %set-cadar (x v) (set-car (cdar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6018 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadar',[x,v],['set-car',[cdar,x],v]])
wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadar).

/*

### Compiled:  `SYS::%SET-CADAR` 
*/
f_sys_pf_set_cadar(X_Param, V_Param, FnResult) :-
	cl_cdar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadar, classof, claz_function),
   set_opv(sys_pf_set_cadar, compile_as, kw_function),
   set_opv(sys_pf_set_cadar, function, f_sys_pf_set_cadar),
   _Ignored6=sys_pf_set_cadar.
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:arglist_info(sys_pf_set_cadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:init_args(exact_only, sys_pf_set_cadar))).
*/
/*
(defun %set-cdaar (x v) (set-cdr (caar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6065 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaar',[x,v],['set-cdr',[caar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaar).

/*

### Compiled:  `SYS::%SET-CDAAR` 
*/
f_sys_pf_set_cdaar(X_Param, V_Param, FnResult) :-
	cl_caar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaar, classof, claz_function),
   set_opv(sys_pf_set_cdaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaar, function, f_sys_pf_set_cdaar),
   _Ignored6=sys_pf_set_cdaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:arglist_info(sys_pf_set_cdaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:init_args(exact_only, sys_pf_set_cdaar))).
*/
/*
(defun %set-cddar (x v) (set-cdr (cdar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6112 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddar',[x,v],['set-cdr',[cdar,x],v]])
wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddar).

/*

### Compiled:  `SYS::%SET-CDDAR` 
*/
f_sys_pf_set_cddar(X_Param, V_Param, FnResult) :-
	cl_cdar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddar, classof, claz_function),
   set_opv(sys_pf_set_cddar, compile_as, kw_function),
   set_opv(sys_pf_set_cddar, function, f_sys_pf_set_cddar),
   _Ignored6=sys_pf_set_cddar.
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:arglist_info(sys_pf_set_cddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:init_args(exact_only, sys_pf_set_cddar))).
*/
/*
(defun %set-caadr (x v) (set-car (cadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6159 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caadr',[x,v],['set-car',[cadr,x],v]])
wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caadr).

/*

### Compiled:  `SYS::%SET-CAADR` 
*/
f_sys_pf_set_caadr(X_Param, V_Param, FnResult) :-
	cl_cadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadr, classof, claz_function),
   set_opv(sys_pf_set_caadr, compile_as, kw_function),
   set_opv(sys_pf_set_caadr, function, f_sys_pf_set_caadr),
   _Ignored6=sys_pf_set_caadr.
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:arglist_info(sys_pf_set_caadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:init_args(exact_only, sys_pf_set_caadr))).
*/
/*
(defun %set-caddr (x v) (set-car (cddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6206 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caddr',[x,v],['set-car',[cddr,x],v]])
wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caddr).

/*

### Compiled:  `SYS::%SET-CADDR` 
*/
f_sys_pf_set_caddr(X_Param, V_Param, FnResult) :-
	cl_cddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddr, classof, claz_function),
   set_opv(sys_pf_set_caddr, compile_as, kw_function),
   set_opv(sys_pf_set_caddr, function, f_sys_pf_set_caddr),
   _Ignored6=sys_pf_set_caddr.
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:arglist_info(sys_pf_set_caddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:init_args(exact_only, sys_pf_set_caddr))).
*/
/*
(defun %set-cdadr (x v) (set-cdr (cadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6253 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdadr',[x,v],['set-cdr',[cadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdadr).

/*

### Compiled:  `SYS::%SET-CDADR` 
*/
f_sys_pf_set_cdadr(X_Param, V_Param, FnResult) :-
	cl_cadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadr, classof, claz_function),
   set_opv(sys_pf_set_cdadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdadr, function, f_sys_pf_set_cdadr),
   _Ignored6=sys_pf_set_cdadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:arglist_info(sys_pf_set_cdadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:init_args(exact_only, sys_pf_set_cdadr))).
*/
/*
(defun %set-cdddr (x v) (set-cdr (cddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6300 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdddr',[x,v],['set-cdr',[cddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdddr).

/*

### Compiled:  `SYS::%SET-CDDDR` 
*/
f_sys_pf_set_cdddr(X_Param, V_Param, FnResult) :-
	cl_cddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddr, classof, claz_function),
   set_opv(sys_pf_set_cdddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdddr, function, f_sys_pf_set_cdddr),
   _Ignored6=sys_pf_set_cdddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:arglist_info(sys_pf_set_cdddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:init_args(exact_only, sys_pf_set_cdddr))).
*/
/*
(defun %set-caaaar (x v) (set-car (caaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6347 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaaar',[x,v],['set-car',[caaar,x],v]])
wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaaar).

/*

### Compiled:  `SYS::%SET-CAAAAR` 
*/
f_sys_pf_set_caaaar(X_Param, V_Param, FnResult) :-
	cl_caaar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaaar, classof, claz_function),
   set_opv(sys_pf_set_caaaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaaar, function, f_sys_pf_set_caaaar),
   _Ignored6=sys_pf_set_caaaar.
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:arglist_info(sys_pf_set_caaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:init_args(exact_only, sys_pf_set_caaaar))).
*/
/*
(defun %set-cadaar (x v) (set-car (cdaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6396 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadaar',[x,v],['set-car',[cdaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadaar).

/*

### Compiled:  `SYS::%SET-CADAAR` 
*/
f_sys_pf_set_cadaar(X_Param, V_Param, FnResult) :-
	cl_cdaar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadaar, classof, claz_function),
   set_opv(sys_pf_set_cadaar, compile_as, kw_function),
   set_opv(sys_pf_set_cadaar, function, f_sys_pf_set_cadaar),
   _Ignored6=sys_pf_set_cadaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:arglist_info(sys_pf_set_cadaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:init_args(exact_only, sys_pf_set_cadaar))).
*/
/*
(defun %set-cdaaar (x v) (set-cdr (caaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6445 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaaar',[x,v],['set-cdr',[caaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaaar).

/*

### Compiled:  `SYS::%SET-CDAAAR` 
*/
f_sys_pf_set_cdaaar(X_Param, V_Param, FnResult) :-
	cl_caaar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaaar, classof, claz_function),
   set_opv(sys_pf_set_cdaaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaaar, function, f_sys_pf_set_cdaaar),
   _Ignored6=sys_pf_set_cdaaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:arglist_info(sys_pf_set_cdaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:init_args(exact_only, sys_pf_set_cdaaar))).
*/
/*
(defun %set-cddaar (x v) (set-cdr (cdaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6494 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddaar',[x,v],['set-cdr',[cdaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddaar).

/*

### Compiled:  `SYS::%SET-CDDAAR` 
*/
f_sys_pf_set_cddaar(X_Param, V_Param, FnResult) :-
	cl_cdaar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddaar, classof, claz_function),
   set_opv(sys_pf_set_cddaar, compile_as, kw_function),
   set_opv(sys_pf_set_cddaar, function, f_sys_pf_set_cddaar),
   _Ignored6=sys_pf_set_cddaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:arglist_info(sys_pf_set_cddaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:init_args(exact_only, sys_pf_set_cddaar))).
*/
/*
(defun %set-caadar (x v) (set-car (cadar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6543 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caadar',[x,v],['set-car',[cadar,x],v]])
wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caadar).

/*

### Compiled:  `SYS::%SET-CAADAR` 
*/
f_sys_pf_set_caadar(X_Param, V_Param, FnResult) :-
	cl_cadar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadar, classof, claz_function),
   set_opv(sys_pf_set_caadar, compile_as, kw_function),
   set_opv(sys_pf_set_caadar, function, f_sys_pf_set_caadar),
   _Ignored6=sys_pf_set_caadar.
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:arglist_info(sys_pf_set_caadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:init_args(exact_only, sys_pf_set_caadar))).
*/
/*
(defun %set-caddar (x v) (set-car (cddar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6592 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caddar',[x,v],['set-car',[cddar,x],v]])
wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caddar).

/*

### Compiled:  `SYS::%SET-CADDAR` 
*/
f_sys_pf_set_caddar(X_Param, V_Param, FnResult) :-
	cl_cddar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddar, classof, claz_function),
   set_opv(sys_pf_set_caddar, compile_as, kw_function),
   set_opv(sys_pf_set_caddar, function, f_sys_pf_set_caddar),
   _Ignored6=sys_pf_set_caddar.
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:arglist_info(sys_pf_set_caddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:init_args(exact_only, sys_pf_set_caddar))).
*/
/*
(defun %set-cdadar (x v) (set-cdr (cadar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6641 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdadar',[x,v],['set-cdr',[cadar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdadar).

/*

### Compiled:  `SYS::%SET-CDADAR` 
*/
f_sys_pf_set_cdadar(X_Param, V_Param, FnResult) :-
	cl_cadar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadar, classof, claz_function),
   set_opv(sys_pf_set_cdadar, compile_as, kw_function),
   set_opv(sys_pf_set_cdadar, function, f_sys_pf_set_cdadar),
   _Ignored6=sys_pf_set_cdadar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:arglist_info(sys_pf_set_cdadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:init_args(exact_only, sys_pf_set_cdadar))).
*/
/*
(defun %set-cdddar (x v) (set-cdr (cddar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6690 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdddar',[x,v],['set-cdr',[cddar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdddar).

/*

### Compiled:  `SYS::%SET-CDDDAR` 
*/
f_sys_pf_set_cdddar(X_Param, V_Param, FnResult) :-
	cl_cddar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddar, classof, claz_function),
   set_opv(sys_pf_set_cdddar, compile_as, kw_function),
   set_opv(sys_pf_set_cdddar, function, f_sys_pf_set_cdddar),
   _Ignored6=sys_pf_set_cdddar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:arglist_info(sys_pf_set_cdddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:init_args(exact_only, sys_pf_set_cdddar))).
*/
/*
(defun %set-caaadr (x v) (set-car (caadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6739 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaadr',[x,v],['set-car',[caadr,x],v]])
wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaadr).

/*

### Compiled:  `SYS::%SET-CAAADR` 
*/
f_sys_pf_set_caaadr(X_Param, V_Param, FnResult) :-
	cl_caadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaadr, classof, claz_function),
   set_opv(sys_pf_set_caaadr, compile_as, kw_function),
   set_opv(sys_pf_set_caaadr, function, f_sys_pf_set_caaadr),
   _Ignored6=sys_pf_set_caaadr.
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:arglist_info(sys_pf_set_caaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:init_args(exact_only, sys_pf_set_caaadr))).
*/
/*
(defun %set-cadadr (x v) (set-car (cdadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6788 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadadr',[x,v],['set-car',[cdadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadadr).

/*

### Compiled:  `SYS::%SET-CADADR` 
*/
f_sys_pf_set_cadadr(X_Param, V_Param, FnResult) :-
	cl_cdadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadadr, classof, claz_function),
   set_opv(sys_pf_set_cadadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadadr, function, f_sys_pf_set_cadadr),
   _Ignored6=sys_pf_set_cadadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:arglist_info(sys_pf_set_cadadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:init_args(exact_only, sys_pf_set_cadadr))).
*/
/*
(defun %set-cdaadr (x v) (set-cdr (caadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6837 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaadr',[x,v],['set-cdr',[caadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaadr).

/*

### Compiled:  `SYS::%SET-CDAADR` 
*/
f_sys_pf_set_cdaadr(X_Param, V_Param, FnResult) :-
	cl_caadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaadr, classof, claz_function),
   set_opv(sys_pf_set_cdaadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaadr, function, f_sys_pf_set_cdaadr),
   _Ignored6=sys_pf_set_cdaadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:arglist_info(sys_pf_set_cdaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:init_args(exact_only, sys_pf_set_cdaadr))).
*/
/*
(defun %set-cddadr (x v) (set-cdr (cdadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6886 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddadr',[x,v],['set-cdr',[cdadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddadr).

/*

### Compiled:  `SYS::%SET-CDDADR` 
*/
f_sys_pf_set_cddadr(X_Param, V_Param, FnResult) :-
	cl_cdadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddadr, classof, claz_function),
   set_opv(sys_pf_set_cddadr, compile_as, kw_function),
   set_opv(sys_pf_set_cddadr, function, f_sys_pf_set_cddadr),
   _Ignored6=sys_pf_set_cddadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:arglist_info(sys_pf_set_cddadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:init_args(exact_only, sys_pf_set_cddadr))).
*/
/*
(defun %set-caaddr (x v) (set-car (caddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6935 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaddr',[x,v],['set-car',[caddr,x],v]])
wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaddr).

/*

### Compiled:  `SYS::%SET-CAADDR` 
*/
f_sys_pf_set_caaddr(X_Param, V_Param, FnResult) :-
	cl_caddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaddr, classof, claz_function),
   set_opv(sys_pf_set_caaddr, compile_as, kw_function),
   set_opv(sys_pf_set_caaddr, function, f_sys_pf_set_caaddr),
   _Ignored6=sys_pf_set_caaddr.
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:arglist_info(sys_pf_set_caaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:init_args(exact_only, sys_pf_set_caaddr))).
*/
/*
(defun %set-cadddr (x v) (set-car (cdddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6984 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadddr',[x,v],['set-car',[cdddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadddr).

/*

### Compiled:  `SYS::%SET-CADDDR` 
*/
f_sys_pf_set_cadddr(X_Param, V_Param, FnResult) :-
	cl_cdddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadddr, classof, claz_function),
   set_opv(sys_pf_set_cadddr, compile_as, kw_function),
   set_opv(sys_pf_set_cadddr, function, f_sys_pf_set_cadddr),
   _Ignored6=sys_pf_set_cadddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:arglist_info(sys_pf_set_cadddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:init_args(exact_only, sys_pf_set_cadddr))).
*/
/*
(defun %set-cdaddr (x v) (set-cdr (caddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7033 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaddr',[x,v],['set-cdr',[caddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaddr).

/*

### Compiled:  `SYS::%SET-CDADDR` 
*/
f_sys_pf_set_cdaddr(X_Param, V_Param, FnResult) :-
	cl_caddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaddr, classof, claz_function),
   set_opv(sys_pf_set_cdaddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaddr, function, f_sys_pf_set_cdaddr),
   _Ignored6=sys_pf_set_cdaddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:arglist_info(sys_pf_set_cdaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:init_args(exact_only, sys_pf_set_cdaddr))).
*/
/*
(defun %set-cddddr (x v) (set-cdr (cdddr x) v))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7082 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddddr',[x,v],['set-cdr',[cdddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddddr).

/*

### Compiled:  `SYS::%SET-CDDDDR` 
*/
f_sys_pf_set_cddddr(X_Param, V_Param, FnResult) :-
	cl_cdddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddddr, classof, claz_function),
   set_opv(sys_pf_set_cddddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddddr, function, f_sys_pf_set_cddddr),
   _Ignored6=sys_pf_set_cddddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:arglist_info(sys_pf_set_cddddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:init_args(exact_only, sys_pf_set_cddddr))).
*/
/*
(defsetf car set-car)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7133 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,car,'set-car'])
/*
% macroexpand:-[defsetf,car,sys_set_car].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,car],[quote,sys_setf_inverse],[quote,sys_set_car]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(car,sys_setf_inverse,sys_set_car,_27680),_27680).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(car, sys_setf_inverse, sys_set_car, _Ignored6),
	   _Ignored6).
/*
(defsetf cdr set-cdr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7156 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdr,'set-cdr'])
/*
% macroexpand:-[defsetf,cdr,sys_set_cdr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdr],[quote,sys_setf_inverse],[quote,sys_set_cdr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdr,sys_setf_inverse,sys_set_cdr,_27710),_27710).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdr, sys_setf_inverse, sys_set_cdr, _Ignored6),
	   _Ignored6).
/*
(defsetf caar %set-caar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7179 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caar,'%set-caar'])
/*
% macroexpand:-[defsetf,caar,sys_pf_set_caar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caar],[quote,sys_setf_inverse],[quote,sys_pf_set_caar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caar,sys_setf_inverse,sys_pf_set_caar,_27744),_27744).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caar, sys_setf_inverse, sys_pf_set_caar, _Ignored6),
	   _Ignored6).
/*
(defsetf cadr %set-cadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7205 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadr,'%set-cadr'])
/*
% macroexpand:-[defsetf,cadr,sys_pf_set_cadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadr,sys_setf_inverse,sys_pf_set_cadr,_27778),_27778).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadr, sys_setf_inverse, sys_pf_set_cadr, _Ignored6),
	   _Ignored6).
/*
(defsetf cdar %set-cdar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7231 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdar,'%set-cdar'])
/*
% macroexpand:-[defsetf,cdar,sys_pf_set_cdar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdar,sys_setf_inverse,sys_pf_set_cdar,_27812),_27812).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdar, sys_setf_inverse, sys_pf_set_cdar, _Ignored6),
	   _Ignored6).
/*
(defsetf cddr %set-cddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7257 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddr,'%set-cddr'])
/*
% macroexpand:-[defsetf,cddr,sys_pf_set_cddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddr,sys_setf_inverse,sys_pf_set_cddr,_27846),_27846).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddr, sys_setf_inverse, sys_pf_set_cddr, _Ignored6),
	   _Ignored6).
/*
(defsetf caaar %set-caaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7283 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaar,'%set-caaar'])
/*
% macroexpand:-[defsetf,caaar,sys_pf_set_caaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaar],[quote,sys_setf_inverse],[quote,sys_pf_set_caaar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaar,sys_setf_inverse,sys_pf_set_caaar,_27880),_27880).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaar, sys_setf_inverse, sys_pf_set_caaar, _Ignored6),
	   _Ignored6).
/*
(defsetf cadar %set-cadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7311 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadar,'%set-cadar'])
/*
% macroexpand:-[defsetf,cadar,sys_pf_set_cadar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadar],[quote,sys_setf_inverse],[quote,sys_pf_set_cadar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadar,sys_setf_inverse,sys_pf_set_cadar,_27914),_27914).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadar, sys_setf_inverse, sys_pf_set_cadar, _Ignored6),
	   _Ignored6).
/*
(defsetf cdaar %set-cdaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7339 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaar,'%set-cdaar'])
/*
% macroexpand:-[defsetf,cdaar,sys_pf_set_cdaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaar,sys_setf_inverse,sys_pf_set_cdaar,_27948),_27948).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaar, sys_setf_inverse, sys_pf_set_cdaar, _Ignored6),
	   _Ignored6).
/*
(defsetf cddar %set-cddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7367 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddar,'%set-cddar'])
/*
% macroexpand:-[defsetf,cddar,sys_pf_set_cddar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddar],[quote,sys_setf_inverse],[quote,sys_pf_set_cddar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddar,sys_setf_inverse,sys_pf_set_cddar,_27982),_27982).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddar, sys_setf_inverse, sys_pf_set_cddar, _Ignored6),
	   _Ignored6).
/*
(defsetf caadr %set-caadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7395 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caadr,'%set-caadr'])
/*
% macroexpand:-[defsetf,caadr,sys_pf_set_caadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caadr],[quote,sys_setf_inverse],[quote,sys_pf_set_caadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caadr,sys_setf_inverse,sys_pf_set_caadr,_28016),_28016).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caadr, sys_setf_inverse, sys_pf_set_caadr, _Ignored6),
	   _Ignored6).
/*
(defsetf caddr %set-caddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7423 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caddr,'%set-caddr'])
/*
% macroexpand:-[defsetf,caddr,sys_pf_set_caddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caddr],[quote,sys_setf_inverse],[quote,sys_pf_set_caddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caddr,sys_setf_inverse,sys_pf_set_caddr,_28050),_28050).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caddr, sys_setf_inverse, sys_pf_set_caddr, _Ignored6),
	   _Ignored6).
/*
(defsetf cdadr %set-cdadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7451 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdadr,'%set-cdadr'])
/*
% macroexpand:-[defsetf,cdadr,sys_pf_set_cdadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdadr,sys_setf_inverse,sys_pf_set_cdadr,_28084),_28084).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdadr, sys_setf_inverse, sys_pf_set_cdadr, _Ignored6),
	   _Ignored6).
/*
(defsetf cdddr %set-cdddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7479 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdddr,'%set-cdddr'])
/*
% macroexpand:-[defsetf,cdddr,sys_pf_set_cdddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdddr,sys_setf_inverse,sys_pf_set_cdddr,_28118),_28118).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdddr, sys_setf_inverse, sys_pf_set_cdddr, _Ignored6),
	   _Ignored6).
/*
(defsetf caaaar %set-caaaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7507 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaaar,'%set-caaaar'])
/*
% macroexpand:-[defsetf,caaaar,sys_pf_set_caaaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaaar],[quote,sys_setf_inverse],[quote,sys_pf_set_caaaar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaaar,sys_setf_inverse,sys_pf_set_caaaar,_28152),_28152).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaaar, sys_setf_inverse, sys_pf_set_caaaar, _Ignored6),
	   _Ignored6).
/*
(defsetf cadaar %set-cadaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7537 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadaar,'%set-cadaar'])
/*
% macroexpand:-[defsetf,cadaar,sys_pf_set_cadaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cadaar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadaar,sys_setf_inverse,sys_pf_set_cadaar,_28186),_28186).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadaar, sys_setf_inverse, sys_pf_set_cadaar, _Ignored6),
	   _Ignored6).
/*
(defsetf cdaaar %set-cdaaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7567 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaaar,'%set-cdaaar'])
/*
% macroexpand:-[defsetf,cdaaar,sys_pf_set_cdaaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaaar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaaar,sys_setf_inverse,sys_pf_set_cdaaar,_28220),_28220).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaaar, sys_setf_inverse, sys_pf_set_cdaaar, _Ignored6),
	   _Ignored6).
/*
(defsetf cddaar %set-cddaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7597 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddaar,'%set-cddaar'])
/*
% macroexpand:-[defsetf,cddaar,sys_pf_set_cddaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cddaar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddaar,sys_setf_inverse,sys_pf_set_cddaar,_28254),_28254).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddaar, sys_setf_inverse, sys_pf_set_cddaar, _Ignored6),
	   _Ignored6).
/*
(defsetf caadar %set-caadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7627 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caadar,'%set-caadar'])
/*
% macroexpand:-[defsetf,caadar,sys_pf_set_caadar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caadar],[quote,sys_setf_inverse],[quote,sys_pf_set_caadar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caadar,sys_setf_inverse,sys_pf_set_caadar,_28288),_28288).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caadar, sys_setf_inverse, sys_pf_set_caadar, _Ignored6),
	   _Ignored6).
/*
(defsetf caddar %set-caddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7657 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caddar,'%set-caddar'])
/*
% macroexpand:-[defsetf,caddar,sys_pf_set_caddar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caddar],[quote,sys_setf_inverse],[quote,sys_pf_set_caddar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caddar,sys_setf_inverse,sys_pf_set_caddar,_28322),_28322).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caddar, sys_setf_inverse, sys_pf_set_caddar, _Ignored6),
	   _Ignored6).
/*
(defsetf cdadar %set-cdadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7687 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdadar,'%set-cdadar'])
/*
% macroexpand:-[defsetf,cdadar,sys_pf_set_cdadar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdadar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdadar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdadar,sys_setf_inverse,sys_pf_set_cdadar,_28356),_28356).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdadar, sys_setf_inverse, sys_pf_set_cdadar, _Ignored6),
	   _Ignored6).
/*
(defsetf cdddar %set-cdddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7717 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdddar,'%set-cdddar'])
/*
% macroexpand:-[defsetf,cdddar,sys_pf_set_cdddar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdddar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdddar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdddar,sys_setf_inverse,sys_pf_set_cdddar,_28390),_28390).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdddar, sys_setf_inverse, sys_pf_set_cdddar, _Ignored6),
	   _Ignored6).
/*
(defsetf caaadr %set-caaadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7747 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaadr,'%set-caaadr'])
/*
% macroexpand:-[defsetf,caaadr,sys_pf_set_caaadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaadr],[quote,sys_setf_inverse],[quote,sys_pf_set_caaadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaadr,sys_setf_inverse,sys_pf_set_caaadr,_28424),_28424).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaadr, sys_setf_inverse, sys_pf_set_caaadr, _Ignored6),
	   _Ignored6).
/*
(defsetf cadadr %set-cadadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7777 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadadr,'%set-cadadr'])
/*
% macroexpand:-[defsetf,cadadr,sys_pf_set_cadadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadadr,sys_setf_inverse,sys_pf_set_cadadr,_28458),_28458).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadadr, sys_setf_inverse, sys_pf_set_cadadr, _Ignored6),
	   _Ignored6).
/*
(defsetf cdaadr %set-cdaadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7807 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaadr,'%set-cdaadr'])
/*
% macroexpand:-[defsetf,cdaadr,sys_pf_set_cdaadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaadr,sys_setf_inverse,sys_pf_set_cdaadr,_28492),_28492).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaadr, sys_setf_inverse, sys_pf_set_cdaadr, _Ignored6),
	   _Ignored6).
/*
(defsetf cddadr %set-cddadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7837 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddadr,'%set-cddadr'])
/*
% macroexpand:-[defsetf,cddadr,sys_pf_set_cddadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddadr,sys_setf_inverse,sys_pf_set_cddadr,_28526),_28526).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddadr, sys_setf_inverse, sys_pf_set_cddadr, _Ignored6),
	   _Ignored6).
/*
(defsetf caaddr %set-caaddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7867 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaddr,'%set-caaddr'])
/*
% macroexpand:-[defsetf,caaddr,sys_pf_set_caaddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaddr],[quote,sys_setf_inverse],[quote,sys_pf_set_caaddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaddr,sys_setf_inverse,sys_pf_set_caaddr,_28560),_28560).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaddr, sys_setf_inverse, sys_pf_set_caaddr, _Ignored6),
	   _Ignored6).
/*
(defsetf cadddr %set-cadddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7897 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadddr,'%set-cadddr'])
/*
% macroexpand:-[defsetf,cadddr,sys_pf_set_cadddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadddr,sys_setf_inverse,sys_pf_set_cadddr,_28594),_28594).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadddr, sys_setf_inverse, sys_pf_set_cadddr, _Ignored6),
	   _Ignored6).
/*
(defsetf cdaddr %set-cdaddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7927 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaddr,'%set-cdaddr'])
/*
% macroexpand:-[defsetf,cdaddr,sys_pf_set_cdaddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaddr,sys_setf_inverse,sys_pf_set_cdaddr,_28628),_28628).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaddr, sys_setf_inverse, sys_pf_set_cdaddr, _Ignored6),
	   _Ignored6).
/*
(defsetf cddddr %set-cddddr)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7957 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddddr,'%set-cddddr'])
/*
% macroexpand:-[defsetf,cddddr,sys_pf_set_cddddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddddr,sys_setf_inverse,sys_pf_set_cddddr,_28664),_28664).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddddr, sys_setf_inverse, sys_pf_set_cddddr, _Ignored6),
	   _Ignored6).
/*
(defsetf first set-car)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7989 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,first,'set-car'])
/*
% macroexpand:-[defsetf,first,sys_set_car].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,first],[quote,sys_setf_inverse],[quote,sys_set_car]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(first,sys_setf_inverse,sys_set_car,_28696),_28696).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(first, sys_setf_inverse, sys_set_car, _Ignored6),
	   _Ignored6).
/*
(defsetf second %set-cadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8014 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,second,'%set-cadr'])
/*
% macroexpand:-[defsetf,second,sys_pf_set_cadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,second],[quote,sys_setf_inverse],[quote,sys_pf_set_cadr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(second,sys_setf_inverse,sys_pf_set_cadr,_28730),_28730).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(second, sys_setf_inverse, sys_pf_set_cadr, _Ignored6),
	   _Ignored6).
/*
(defsetf third %set-caddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8042 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,third,'%set-caddr'])
/*
% macroexpand:-[defsetf,third,sys_pf_set_caddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,third],[quote,sys_setf_inverse],[quote,sys_pf_set_caddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(third,sys_setf_inverse,sys_pf_set_caddr,_28764),_28764).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(third, sys_setf_inverse, sys_pf_set_caddr, _Ignored6),
	   _Ignored6).
/*
(defsetf fourth %set-cadddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8070 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,fourth,'%set-cadddr'])
/*
% macroexpand:-[defsetf,fourth,sys_pf_set_cadddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,fourth],[quote,sys_setf_inverse],[quote,sys_pf_set_cadddr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(fourth,sys_setf_inverse,sys_pf_set_cadddr,_28798),_28798).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(fourth, sys_setf_inverse, sys_pf_set_cadddr, _Ignored6),
	   _Ignored6).
/*
(defun %set-fifth (x v) (set-car (cddddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8100 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-fifth',[x,v],['set-car',[cddddr,x],v]])
wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_fifth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_fifth).

/*

### Compiled:  `SYS::%SET-FIFTH` 
*/
f_sys_pf_set_fifth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_fifth, classof, claz_function),
   set_opv(sys_pf_set_fifth, compile_as, kw_function),
   set_opv(sys_pf_set_fifth, function, f_sys_pf_set_fifth),
   _Ignored6=sys_pf_set_fifth.
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:arglist_info(sys_pf_set_fifth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:init_args(exact_only, sys_pf_set_fifth))).
*/
/*
(defsetf fifth %set-fifth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8149 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,fifth,'%set-fifth'])
/*
% macroexpand:-[defsetf,fifth,sys_pf_set_fifth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,fifth],[quote,sys_setf_inverse],[quote,sys_pf_set_fifth]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(fifth,sys_setf_inverse,sys_pf_set_fifth,_92048),_92048).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(fifth, sys_setf_inverse, sys_pf_set_fifth, _Ignored6),
	   _Ignored6).
/*
(defun %set-sixth (x v) (set-car (cdr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8177 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-sixth',[x,v],['set-car',[cdr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_sixth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_sixth).

/*

### Compiled:  `SYS::%SET-SIXTH` 
*/
f_sys_pf_set_sixth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_sixth, classof, claz_function),
   set_opv(sys_pf_set_sixth, compile_as, kw_function),
   set_opv(sys_pf_set_sixth, function, f_sys_pf_set_sixth),
   _Ignored6=sys_pf_set_sixth.
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:arglist_info(sys_pf_set_sixth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:init_args(exact_only, sys_pf_set_sixth))).
*/
/*
(defsetf sixth %set-sixth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8232 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,sixth,'%set-sixth'])
/*
% macroexpand:-[defsetf,sixth,sys_pf_set_sixth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,sixth],[quote,sys_setf_inverse],[quote,sys_pf_set_sixth]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(sixth,sys_setf_inverse,sys_pf_set_sixth,_92288),_92288).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(sixth, sys_setf_inverse, sys_pf_set_sixth, _Ignored6),
	   _Ignored6).
/*
(defun %set-seventh (x v) (set-car (cddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8260 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-seventh',[x,v],['set-car',[cddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_seventh, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_seventh).

/*

### Compiled:  `SYS::%SET-SEVENTH` 
*/
f_sys_pf_set_seventh(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddr_Param),
	cl_cddr(Cddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_seventh, classof, claz_function),
   set_opv(sys_pf_set_seventh, compile_as, kw_function),
   set_opv(sys_pf_set_seventh, function, f_sys_pf_set_seventh),
   _Ignored6=sys_pf_set_seventh.
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:arglist_info(sys_pf_set_seventh, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:init_args(exact_only, sys_pf_set_seventh))).
*/
/*
(defsetf seventh %set-seventh)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8318 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,seventh,'%set-seventh'])
/*
% macroexpand:-[defsetf,seventh,sys_pf_set_seventh].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,seventh],[quote,sys_setf_inverse],[quote,sys_pf_set_seventh]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(seventh,sys_setf_inverse,sys_pf_set_seventh,_92056),_92056).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(seventh, sys_setf_inverse, sys_pf_set_seventh, _Ignored6),
	   _Ignored6).
/*
(defun %set-eighth (x v) (set-car (cdddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8350 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-eighth',[x,v],['set-car',[cdddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_eighth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_eighth).

/*

### Compiled:  `SYS::%SET-EIGHTH` 
*/
f_sys_pf_set_eighth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cdddr_Param),
	cl_cdddr(Cdddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_eighth, classof, claz_function),
   set_opv(sys_pf_set_eighth, compile_as, kw_function),
   set_opv(sys_pf_set_eighth, function, f_sys_pf_set_eighth),
   _Ignored6=sys_pf_set_eighth.
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:arglist_info(sys_pf_set_eighth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:init_args(exact_only, sys_pf_set_eighth))).
*/
/*
(defsetf eighth %set-eighth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8408 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,eighth,'%set-eighth'])
/*
% macroexpand:-[defsetf,eighth,sys_pf_set_eighth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,eighth],[quote,sys_setf_inverse],[quote,sys_pf_set_eighth]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(eighth,sys_setf_inverse,sys_pf_set_eighth,_91500),_91500).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(eighth, sys_setf_inverse, sys_pf_set_eighth, _Ignored6),
	   _Ignored6).
/*
(defun %set-ninth (x v) (set-car (cddddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8438 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-ninth',[x,v],['set-car',[cddddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_ninth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_ninth).

/*

### Compiled:  `SYS::%SET-NINTH` 
*/
f_sys_pf_set_ninth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_ninth, classof, claz_function),
   set_opv(sys_pf_set_ninth, compile_as, kw_function),
   set_opv(sys_pf_set_ninth, function, f_sys_pf_set_ninth),
   _Ignored6=sys_pf_set_ninth.
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:arglist_info(sys_pf_set_ninth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:init_args(exact_only, sys_pf_set_ninth))).
*/
/*
(defsetf ninth %set-ninth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8496 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,ninth,'%set-ninth'])
/*
% macroexpand:-[defsetf,ninth,sys_pf_set_ninth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,ninth],[quote,sys_setf_inverse],[quote,sys_pf_set_ninth]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(ninth,sys_setf_inverse,sys_pf_set_ninth,_90962),_90962).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(ninth, sys_setf_inverse, sys_pf_set_ninth, _Ignored6),
	   _Ignored6).
/*
(defun %set-tenth (x v) (set-car (cdr (cddddr (cddddr x))) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8524 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-tenth',[x,v],['set-car',[cdr,[cddddr,[cddddr,x]]],v]])
wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]).
wl:arglist_info(sys_pf_set_tenth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_tenth).

/*

### Compiled:  `SYS::%SET-TENTH` 
*/
f_sys_pf_set_tenth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_tenth, classof, claz_function),
   set_opv(sys_pf_set_tenth, compile_as, kw_function),
   set_opv(sys_pf_set_tenth, function, f_sys_pf_set_tenth),
   _Ignored6=sys_pf_set_tenth.
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:arglist_info(sys_pf_set_tenth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:init_args(exact_only, sys_pf_set_tenth))).
*/
/*
(defsetf tenth %set-tenth)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8588 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,tenth,'%set-tenth'])
/*
% macroexpand:-[defsetf,tenth,sys_pf_set_tenth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,tenth],[quote,sys_setf_inverse],[quote,sys_pf_set_tenth]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(tenth,sys_setf_inverse,sys_pf_set_tenth,_91392),_91392).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(tenth, sys_setf_inverse, sys_pf_set_tenth, _Ignored6),
	   _Ignored6).
/*
(defsetf rest set-cdr)
;;Redefined in extensible-sequences-base.lisp
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8618 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,rest,'set-cdr'])
/*
% macroexpand:-[defsetf,rest,sys_set_cdr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,rest],[quote,sys_setf_inverse],[quote,sys_set_cdr]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(rest,sys_setf_inverse,sys_set_cdr,_29380),_29380).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(rest, sys_setf_inverse, sys_set_cdr, _Ignored6),
	   _Ignored6).
/*
;Redefined in extensible-sequences-base.lisp
*/
/*
(defsetf elt %set-elt)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8689 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,elt,'%set-elt'])
/*
% macroexpand:-[defsetf,elt,sys_pf_set_elt].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,elt],[quote,sys_setf_inverse],[quote,sys_pf_set_elt]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(elt,sys_setf_inverse,sys_pf_set_elt,_29382),_29382).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(elt, sys_setf_inverse, sys_pf_set_elt, _Ignored6),
	   _Ignored6).
/*
(defsetf nth %set-nth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8713 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,nth,'%set-nth'])
/*
% macroexpand:-[defsetf,nth,sys_pf_set_nth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,nth],[quote,sys_setf_inverse],[quote,sys_pf_set_nth]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(nth,sys_setf_inverse,sys_pf_set_nth,_29416),_29416).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(nth, sys_setf_inverse, sys_pf_set_nth, _Ignored6),
	   _Ignored6).
/*
(defsetf svref svset)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8737 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,svref,svset])
/*
% macroexpand:-[defsetf,svref,sys_svset].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,svref],[quote,sys_setf_inverse],[quote,sys_svset]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(svref,sys_setf_inverse,sys_svset,_29446),_29446).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(svref, sys_setf_inverse, sys_svset, _Ignored6),
	   _Ignored6).
/*
(defsetf fill-pointer %set-fill-pointer)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8760 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'fill-pointer','%set-fill-pointer'])
/*
% macroexpand:-[defsetf,fill_pointer,sys_pf_set_fill_pointer].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,fill_pointer],[quote,sys_setf_inverse],[quote,sys_pf_set_fill_pointer]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(fill_pointer,sys_setf_inverse,sys_pf_set_fill_pointer,_29522),_29522).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(fill_pointer,
		     sys_setf_inverse,
		     sys_pf_set_fill_pointer,
		     _Ignored6),
	   _Ignored6).
/*
(defsetf subseq %set-subseq)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8802 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,subseq,'%set-subseq'])
/*
% macroexpand:-[defsetf,subseq,sys_pf_set_subseq].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,subseq],[quote,sys_setf_inverse],[quote,sys_pf_set_subseq]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(subseq,sys_setf_inverse,sys_pf_set_subseq,_29518),_29518).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(subseq, sys_setf_inverse, sys_pf_set_subseq, _Ignored6),
	   _Ignored6).
/*
(defsetf symbol-value set)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8832 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-value',set])
/*
% macroexpand:-[defsetf,symbol_value,set].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,symbol_value],[quote,sys_setf_inverse],[quote,set]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(symbol_value,sys_setf_inverse,set,_29552),_29552).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(symbol_value, sys_setf_inverse, set, _Ignored6),
	   _Ignored6).
/*
(defsetf symbol-function %set-symbol-function)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8860 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-function','%set-symbol-function'])
/*
% macroexpand:-[defsetf,symbol_function,sys_pf_set_symbol_function].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,symbol_function],[quote,sys_setf_inverse],[quote,sys_pf_set_symbol_function]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(symbol_function,sys_setf_inverse,sys_pf_set_symbol_function,_29630),_29630).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(symbol_function,
		     sys_setf_inverse,
		     sys_pf_set_symbol_function,
		     _Ignored6),
	   _Ignored6).
/*
(defsetf symbol-plist %set-symbol-plist)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8908 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-plist','%set-symbol-plist'])
/*
% macroexpand:-[defsetf,symbol_plist,sys_pf_set_symbol_plist].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,symbol_plist],[quote,sys_setf_inverse],[quote,sys_pf_set_symbol_plist]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(symbol_plist,sys_setf_inverse,sys_pf_set_symbol_plist,_29658),_29658).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(symbol_plist,
		     sys_setf_inverse,
		     sys_pf_set_symbol_plist,
		     _Ignored6),
	   _Ignored6).
/*
(defsetf get put)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8950 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,get,put])
/*
% macroexpand:-[defsetf,get,sys_put].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,get],[quote,sys_setf_inverse],[quote,sys_put]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(get,sys_setf_inverse,sys_put,_29648),_29648).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(get, sys_setf_inverse, sys_put, _Ignored6),
	   _Ignored6).
/*
(defsetf gethash puthash)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8969 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,gethash,puthash])
/*
% macroexpand:-[defsetf,gethash,sys_puthash].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,gethash],[quote,sys_setf_inverse],[quote,sys_puthash]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(gethash,sys_setf_inverse,sys_puthash,_29688),_29688).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(gethash, sys_setf_inverse, sys_puthash, _Ignored6),
	   _Ignored6).
/*
(defsetf char set-char)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8996 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,char,'set-char'])
/*
% macroexpand:-[defsetf,char,sys_set_char].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,char],[quote,sys_setf_inverse],[quote,sys_set_char]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(char,sys_setf_inverse,sys_set_char,_29722),_29722).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(char, sys_setf_inverse, sys_set_char, _Ignored6),
	   _Ignored6).
/*
(defsetf schar set-schar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9021 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,schar,'set-schar'])
/*
% macroexpand:-[defsetf,schar,sys_set_schar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,schar],[quote,sys_setf_inverse],[quote,sys_set_schar]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(schar,sys_setf_inverse,sys_set_schar,_29756),_29756).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(schar, sys_setf_inverse, sys_set_schar, _Ignored6),
	   _Ignored6).
/*
(defsetf logical-pathname-translations %set-logical-pathname-translations)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9048 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'logical-pathname-translations','%set-logical-pathname-translations'])
/*
% macroexpand:-[defsetf,logical_pathname_translations,sys_pf_set_logical_pathname_translations].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,logical_pathname_translations],[quote,sys_setf_inverse],[quote,sys_pf_set_logical_pathname_translations]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(logical_pathname_translations,sys_setf_inverse,sys_pf_set_logical_pathname_translations,_29852),_29852).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(logical_pathname_translations,
		     sys_setf_inverse,
		     sys_pf_set_logical_pathname_translations,
		     _Ignored6),
	   _Ignored6).
/*
(defsetf readtable-case %set-readtable-case)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9124 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'readtable-case','%set-readtable-case'])
/*
% macroexpand:-[defsetf,readtable_case,sys_pf_set_readtable_case].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,readtable_case],[quote,sys_setf_inverse],[quote,sys_pf_set_readtable_case]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(readtable_case,sys_setf_inverse,sys_pf_set_readtable_case,_29864),_29864).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(readtable_case,
		     sys_setf_inverse,
		     sys_pf_set_readtable_case,
		     _Ignored6),
	   _Ignored6).
/*
(defsetf function-info %set-function-info)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9172 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'function-info','%set-function-info'])
/*
% macroexpand:-[defsetf,sys_function_info,sys_pf_set_function_info].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,sys_function_info],[quote,sys_setf_inverse],[quote,sys_pf_set_function_info]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(sys_function_info,sys_setf_inverse,sys_pf_set_function_info,_29896),_29896).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(sys_function_info,
		     sys_setf_inverse,
		     sys_pf_set_function_info,
		     _Ignored6),
	   _Ignored6).
/*
(defsetf stream-external-format %set-stream-external-format)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9218 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'stream-external-format','%set-stream-external-format'])
/*
% macroexpand:-[defsetf,stream_external_format,sys_pf_set_stream_external_format].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,stream_external_format],[quote,sys_setf_inverse],[quote,sys_pf_set_stream_external_format]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(stream_external_format,sys_setf_inverse,sys_pf_set_stream_external_format,_29944),_29944).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(stream_external_format,
		     sys_setf_inverse,
		     sys_pf_set_stream_external_format,
		     _Ignored6),
	   _Ignored6).
/*
(defsetf structure-ref structure-set)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9282 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'structure-ref','structure-set'])
/*
% macroexpand:-[defsetf,sys_structure_ref,sys_structure_set].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,sys_structure_ref],[quote,sys_setf_inverse],[quote,sys_structure_set]]].
*/
/*
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(sys_structure_ref,sys_setf_inverse,sys_structure_set,_29960),_29960).
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(sys_structure_ref,
		     sys_setf_inverse,
		     sys_structure_set,
		     _Ignored6),
	   _Ignored6).


%; Total compilation time: 6.422 seconds


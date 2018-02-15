#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init-10" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Tue Jan 23 00:53:38 2018

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
(in-package #:system)


;;; define-modify-macro.lisp
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:262 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','#:system'])
/*
% macroexpand:-[in_package,system4].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
;; define-modify-macro.lisp
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
(in-package #:system)

;; FIXME See section 5.1.3.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:1927 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','#:system'])
/*
% macroexpand:-[in_package,system5].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
; FIXME See section 5.1.3.
*/
/*
#+(or (and ABCL ALT) WAM-CL)
(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
	(rest-arg nil)
	(env (gensym))
	(reference (gensym)))
    ;; Parse out the variable names and &REST arg from the lambda list.
    (do ((ll lambda-list (cdr ll))
	 (arg nil))
	((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
	    ((eq arg '&rest)
	     (if (symbolp (cadr ll))
		 (setq rest-arg (cadr ll))
		 (error "Non-symbol &REST arg in definition of "#+(or (and ABCL ALT) WAM-CL)\r\n(defmacro define-modify-macro (name lambda-list function &optional doc-string)\r\n  \"Creates a new read-modify-write macro like PUSH or INCF.\"\r\n  (let ((other-args nil)\r\n\t(rest-arg nil)\r\n\t(env (gensym))\r\n\t(reference (gensym)))\r\n    ;; Parse out the variable names and &REST arg from the lambda list.\r\n    (do ((ll lambda-list (cdr ll))\r\n\t (arg nil))\r\n\t((null ll))\r\n      (setq arg (car ll))\r\n      (cond ((eq arg '&optional))\r\n\t    ((eq arg '&rest)\r\n\t     (if (symbolp (cadr ll))\r\n\t\t (setq rest-arg (cadr ll))\r\n\t\t (error \"Non-symbol &REST arg in definition of ~S.\" name))\r\n\t     (if (null (cddr ll))\r\n\t\t (return nil)\r\n\t\t (error \"Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.\")))\r\n\t    ((memq arg '(&key &allow-other-keys &aux))\r\n\t     (error \"~S not allowed in DEFINE-MODIFY-MACRO lambda list.\" arg))\r\n\t    ((symbolp arg)\r\n\t     (push arg other-args))\r\n\t    ((and (listp arg) (symbolp (car arg)))\r\n\t     (push (car arg) other-args))\r\n\t    (t (error \"Illegal stuff in DEFINE-MODIFY-MACRO lambda list.\"))))\r\n    (setq other-args (nreverse other-args))\r\n    `(eval-when (:compile-toplevel :load-toplevel :execute)\r\n      (defmacro ,name (,reference ,@lambda-list &environment ,env)\r\n        ,doc-string\r\n        (multiple-value-bind (dummies vals newval setter getter)\r\n            (get-setf-expansion ,reference ,env)\r\n          (do ((d dummies (cdr d))\r\n               (v vals (cdr v))\r\n               (let-list nil (cons (list (car d) (car v)) let-list)))\r\n              ((null d)\r\n               (push (list (car newval)\r\n                           ,(if rest-arg\r\n                                `(list* ',function getter ,@other-args ,rest-arg)\r\n                                `(list ',function getter ,@other-args)))\r\n                     let-list)\r\n               `(let* ,(nreverse let-list)\r\n                 ,setter))))))))\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:1981 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'define-modify-macro',[name,'lambda-list',function,'&optional','doc-string'],'$STRING'("Creates a new read-modify-write macro like PUSH or INCF."),[let,[['other-args',[]],['rest-arg',[]],[env,[gensym]],[reference,[gensym]]],[do,[[ll,'lambda-list',[cdr,ll]],[arg,[]]],[[null,ll]],[setq,arg,[car,ll]],[cond,[[eq,arg,[quote,'&optional']]],[[eq,arg,[quote,'&rest']],[if,[symbolp,[cadr,ll]],[setq,'rest-arg',[cadr,ll]],[error,'$STRING'("Non-symbol &REST arg in definition of ~S."),name]],[if,[null,[cddr,ll]],[return,[]],[error,'$STRING'("Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.")]]],[[memq,arg,[quote,['&key','&allow-other-keys','&aux']]],[error,'$STRING'("~S not allowed in DEFINE-MODIFY-MACRO lambda list."),arg]],[[symbolp,arg],[push,arg,'other-args']],[[and,[listp,arg],[symbolp,[car,arg]]],[push,[car,arg],'other-args']],[t,[error,'$STRING'("Illegal stuff in DEFINE-MODIFY-MACRO lambda list.")]]]],[setq,'other-args',[nreverse,'other-args']],['#BQ',['eval-when',[':compile-toplevel',':load-toplevel',':execute'],[defmacro,['#COMMA',name],[['#COMMA',reference],['#BQ-COMMA-ELIPSE','lambda-list'],'&environment',['#COMMA',env]],['#COMMA','doc-string'],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',['#COMMA',reference],['#COMMA',env]],[do,[[d,dummies,[cdr,d]],[v,vals,[cdr,v]],['let-list',[],[cons,[list,[car,d],[car,v]],'let-list']]],[[null,d],[push,[list,[car,newval],['#COMMA',[if,'rest-arg',['#BQ',['list*',[quote,['#COMMA',function]],getter,['#BQ-COMMA-ELIPSE','other-args'],['#COMMA','rest-arg']]],['#BQ',[list,[quote,['#COMMA',function]],getter,['#BQ-COMMA-ELIPSE','other-args']]]]]],'let-list'],['#BQ',['let*',['#COMMA',[nreverse,'let-list']],['#COMMA',setter]]]]]]]]]]])
/*
% macroexpand:-[push,sys_arg,sys_other_args].
*/
/*
% into:-[setq,sys_other_args,[cons,sys_arg,sys_other_args]].
*/
/*
% macroexpand:-[push,[car,sys_arg],sys_other_args].
*/
/*
% into:-[setq,sys_other_args,[cons,[car,sys_arg],sys_other_args]].
*/
/*
% macroexpand:-[push,sys_arg,sys_other_args].
*/
/*
% into:-[setq,sys_other_args,[cons,sys_arg,sys_other_args]].
*/
/*
% macroexpand:-[push,[car,sys_arg],sys_other_args].
*/
/*
% into:-[setq,sys_other_args,[cons,[car,sys_arg],sys_other_args]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       define_modify_macro,
					       kw_special,
					       sf_define_modify_macro)).
*/
doc: doc_string(define_modify_macro,
	      _10326,
	      function,
	      "Creates a new read-modify-write macro like PUSH or INCF.").

wl:lambda_def(defmacro, define_modify_macro, mf_define_modify_type_macro, [sys_name, sys_lambda_list, function, c38_optional, sys_doc_string], [[let, [[sys_other_args, []], [sys_rest_arg, []], [sys_env, [gensym]], [sys_reference, [gensym]]], [do, [[sys_ll, sys_lambda_list, [cdr, sys_ll]], [sys_arg, []]], [[null, sys_ll]], [setq, sys_arg, [car, sys_ll]], [cond, [[eq, sys_arg, [quote, c38_optional]]], [[eq, sys_arg, [quote, c38_rest]], [if, [symbolp, [cadr, sys_ll]], [setq, sys_rest_arg, [cadr, sys_ll]], [error, '$ARRAY'([*], claz_base_character, "Non-symbol &REST arg in definition of ~S."), sys_name]], [if, [null, [cddr, sys_ll]], [return, []], [error, '$ARRAY'([*], claz_base_character, "Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.")]]], [[sys_memq, sys_arg, [quote, [c38_key, c38_allow_other_keys, c38_aux]]], [error, '$ARRAY'([*], claz_base_character, "~S not allowed in DEFINE-MODIFY-MACRO lambda list."), sys_arg]], [[symbolp, sys_arg], [push, sys_arg, sys_other_args]], [[and, [listp, sys_arg], [symbolp, [car, sys_arg]]], [push, [car, sys_arg], sys_other_args]], [t, [error, '$ARRAY'([*], claz_base_character, "Illegal stuff in DEFINE-MODIFY-MACRO lambda list.")]]]], [setq, sys_other_args, [nreverse, sys_other_args]], ['#BQ', [eval_when, [kw_compile_toplevel, kw_load_toplevel, kw_execute], [defmacro, ['#COMMA', sys_name], [['#COMMA', sys_reference], ['#BQ-COMMA-ELIPSE', sys_lambda_list], c38_environment, ['#COMMA', sys_env]], ['#COMMA', sys_doc_string], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, ['#COMMA', sys_reference], ['#COMMA', sys_env]], [do, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]], [sys_let_list, [], [cons, [list, [car, sys_d], [car, sys_v]], sys_let_list]]], [[null, sys_d], [push, [list, [car, sys_newval], ['#COMMA', [if, sys_rest_arg, ['#BQ', [list_xx, [quote, ['#COMMA', function]], sys_getter, ['#BQ-COMMA-ELIPSE', sys_other_args], ['#COMMA', sys_rest_arg]]], ['#BQ', [list, [quote, ['#COMMA', function]], sys_getter, ['#BQ-COMMA-ELIPSE', sys_other_args]]]]]], sys_let_list], ['#BQ', [let_xx, ['#COMMA', [nreverse, sys_let_list]], ['#COMMA', sys_setter]]]]]]]]]]]).
wl:arglist_info(define_modify_macro, mf_define_modify_type_macro, [sys_name, sys_lambda_list, function, c38_optional, sys_doc_string], arginfo{all:[sys_name, sys_lambda_list, function, sys_doc_string], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_lambda_list, function, sys_doc_string], opt:[sys_doc_string], req:[sys_name, sys_lambda_list, function], rest:0, sublists:0, whole:0}).
wl: init_args(3, mf_define_modify_type_macro).

/*

### Compiled Macro Operator: `CL:DEFINE-MODIFY-MACRO` 
*/
sf_define_modify_macro(MacroEnv, Name_In, Lambda_list_In, Function_In, RestNKeys, FResult) :-
	mf_define_modify_type_macro(
				    [ define_modify_macro,
				      Name_In,
				      Lambda_list_In,
				      Function_In
				    | RestNKeys
				    ],
				    MacroEnv,
				    MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFINE-MODIFY-MACRO` 
*/
mf_define_modify_type_macro([define_modify_macro, Name_In, Lambda_list_In, Function_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	CDR=[bv(sys_name, Name_In), bv(sys_lambda_list, Lambda_list_In), bv(function, Function_In), bv(sys_doc_string, Doc_string_In)],
	opt_var(MacroEnv, sys_doc_string, Doc_string_In, true, [], 1, RestNKeys),
	catch(( ( f_gensym(Env_Init),
		  f_gensym(Reference_Init),
		  LEnv=[bv(sys_other_args, []), bv(sys_rest_arg, []), bv(sys_env, Env_Init), bv(sys_reference, Reference_Init)|CDR],
		  get_var(LEnv, sys_lambda_list, Lambda_list_Get),
		  AEnv=[bv(sys_ll, Lambda_list_Get), bv(sys_arg, [])|LEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_19), get_var(AEnv, sys_ll, IFTEST86), (IFTEST86==[]->throw(block_exit([], [])), _TBResult=ThrowResult90;get_var(AEnv, sys_ll, Ll_Get93), f_car(Ll_Get93, Arg), set_var(AEnv, sys_arg, Arg), get_var(AEnv, sys_arg, Arg_Get95), (is_eq(Arg_Get95, c38_optional)->_12264=[];get_var(AEnv, sys_arg, Arg_Get99), (is_eq(Arg_Get99, c38_rest)->get_var(AEnv, sys_ll, Ll_Get103), f_cadr(Ll_Get103, PredArgResult105), (is_symbolp(PredArgResult105)->get_var(AEnv, sys_ll, Ll_Get106), f_cadr(Ll_Get106, TrueResult108), set_var(AEnv, sys_rest_arg, TrueResult108), _12480=TrueResult108;get_var(AEnv, sys_name, Name_Get107), f_error(['$ARRAY'([*], claz_base_character, "Non-symbol &REST arg in definition of ~S."), Name_Get107], ElseResult109), _12480=ElseResult109), get_var(AEnv, sys_ll, Ll_Get112), f_cddr(Ll_Get112, IFTEST110), (IFTEST110==[]->throw(block_exit([], [])), TrueResult143=ThrowResult114;f_error(['$ARRAY'([*], claz_base_character, "Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.")], ElseResult116), TrueResult143=ElseResult116), ElseResult145=TrueResult143;get_var(AEnv, sys_arg, Arg_Get119), f_sys_memq(Arg_Get119, [c38_key, c38_allow_other_keys, c38_aux], IFTEST117), (IFTEST117\==[]->get_var(AEnv, sys_arg, Arg_Get120), f_error(['$ARRAY'([*], claz_base_character, "~S not allowed in DEFINE-MODIFY-MACRO lambda list."), Arg_Get120], TrueResult141), ElseResult144=TrueResult141;get_var(AEnv, sys_arg, Arg_Get122), (is_symbolp(Arg_Get122)->get_var(AEnv, sys_arg, Arg_Get125), get_var(AEnv, sys_other_args, Other_args_Get126), TrueResult139=[Arg_Get125|Other_args_Get126], set_var(AEnv, sys_other_args, TrueResult139), ElseResult142=TrueResult139;get_var(AEnv, sys_arg, Arg_Get130), (s3q:is_listp(Arg_Get130)->get_var(AEnv, sys_arg, Arg_Get133), f_car(Arg_Get133, Symbolp_Param), f_symbolp(Symbolp_Param, TrueResult134), IFTEST127=TrueResult134;IFTEST127=[]), (IFTEST127\==[]->get_var(AEnv, sys_arg, Arg_Get135), f_car(Arg_Get135, Car_Ret), get_var(AEnv, sys_other_args, Other_args_Get136), TrueResult137=[Car_Ret|Other_args_Get136], set_var(AEnv, sys_other_args, TrueResult137), ElseResult140=TrueResult137;f_error(['$ARRAY'([*], claz_base_character, "Illegal stuff in DEFINE-MODIFY-MACRO lambda list.")], ElseResult138), ElseResult140=ElseResult138), ElseResult142=ElseResult140), ElseResult144=ElseResult142), ElseResult145=ElseResult144), _12264=ElseResult145), get_var(AEnv, sys_ll, Ll_Get146), f_cdr(Ll_Get146, Ll), set_var(AEnv, sys_ll, Ll), goto(do_label_19, AEnv), _TBResult=_GORES147)),
					  
					  [ addr(addr_tagbody_19_do_label_19,
						 do_label_19,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_ll, IFTEST), (IFTEST==[]->throw(block_exit([], [])), _13726=ThrowResult;get_var(AEnv, sys_ll, Ll_Get27), f_car(Ll_Get27, Car_Ret177), set_var(AEnv, sys_arg, Car_Ret177), get_var(AEnv, sys_arg, Arg_Get), (is_eq(Arg_Get, c38_optional)->_13772=[];get_var(AEnv, sys_arg, Arg_Get33), (is_eq(Arg_Get33, c38_rest)->get_var(AEnv, sys_ll, Ll_Get37), f_cadr(Ll_Get37, Cadr_Ret), (is_symbolp(Cadr_Ret)->get_var(AEnv, sys_ll, Ll_Get40), f_cadr(Ll_Get40, Cadr_Ret179), set_var(AEnv, sys_rest_arg, Cadr_Ret179), _13820=Cadr_Ret179;get_var(AEnv, sys_name, Get_var_Ret), f_error(['$ARRAY'([*], claz_base_character, "Non-symbol &REST arg in definition of ~S."), Get_var_Ret], Error_Ret), _13820=Error_Ret), get_var(AEnv, sys_ll, Ll_Get46), f_cddr(Ll_Get46, IFTEST44), (IFTEST44==[]->throw(block_exit([], [])), TrueResult77=ThrowResult48;f_error(['$ARRAY'([*], claz_base_character, "Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.")], ElseResult50), TrueResult77=ElseResult50), ElseResult79=TrueResult77;get_var(AEnv, sys_arg, Arg_Get53), f_sys_memq(Arg_Get53, [c38_key, c38_allow_other_keys, c38_aux], IFTEST51), (IFTEST51\==[]->get_var(AEnv, sys_arg, Arg_Get54), f_error(['$ARRAY'([*], claz_base_character, "~S not allowed in DEFINE-MODIFY-MACRO lambda list."), Arg_Get54], TrueResult75), ElseResult78=TrueResult75;get_var(AEnv, sys_arg, Arg_Get56), (is_symbolp(Arg_Get56)->get_var(AEnv, sys_arg, Arg_Get59), get_var(AEnv, sys_other_args, Get_var_Ret182), TrueResult73=[Arg_Get59|Get_var_Ret182], set_var(AEnv, sys_other_args, TrueResult73), ElseResult76=TrueResult73;get_var(AEnv, sys_arg, Arg_Get64), (s3q:is_listp(Arg_Get64)->get_var(AEnv, sys_arg, Arg_Get67), f_car(Arg_Get67, Symbolp_Param174), f_symbolp(Symbolp_Param174, TrueResult68), IFTEST61=TrueResult68;IFTEST61=[]), (IFTEST61\==[]->get_var(AEnv, sys_arg, Arg_Get69), f_car(Arg_Get69, Car_Ret183), get_var(AEnv, sys_other_args, Other_args_Get70), TrueResult71=[Car_Ret183|Other_args_Get70], set_var(AEnv, sys_other_args, TrueResult71), ElseResult74=TrueResult71;f_error(['$ARRAY'([*], claz_base_character, "Illegal stuff in DEFINE-MODIFY-MACRO lambda list.")], ElseResult72), ElseResult74=ElseResult72), ElseResult76=ElseResult74), ElseResult78=ElseResult76), ElseResult79=ElseResult78), _13772=ElseResult79), get_var(AEnv, sys_ll, Ll_Get80), f_cdr(Ll_Get80, Cdr_Ret), set_var(AEnv, sys_ll, Cdr_Ret), goto(do_label_19, AEnv), _13726=_GORES)))
					  ]),
			  []=LetResult15
			),
			block_exit([], LetResult15),
			true),
		  get_var(LEnv, sys_other_args, Other_args_Get152),
		  f_nreverse(Other_args_Get152, Other_args),
		  set_var(LEnv, sys_other_args, Other_args),
		  get_var(LEnv, sys_lambda_list, Lambda_list_Get155),
		  ( get_var(LEnv, sys_env, Env_Get),
		    get_var(LEnv, sys_name, Name_Get153)
		  ),
		  get_var(LEnv, sys_reference, Reference_Get),
		  bq_append([Reference_Get|Lambda_list_Get155],
			    [c38_environment, Env_Get],
			    Bq_append_Ret),
		  get_var(LEnv, sys_doc_string, Doc_string_Get),
		  get_var(LEnv, sys_env, Env_Get159),
		  get_var(LEnv, sys_reference, Reference_Get158),
		  get_var(LEnv, sys_rest_arg, IFTEST160),
		  (   IFTEST160\==[]
		  ->  get_var(LEnv, function, Function_Get),
		      get_var(LEnv, sys_other_args, Other_args_Get164),
		      get_var(LEnv, sys_rest_arg, Rest_arg_Get165),
		      bq_append([sys_getter|Other_args_Get164],
				[Rest_arg_Get165],
				Bq_append_Ret186),
		      CAR=[list_xx, [quote, Function_Get]|Bq_append_Ret186]
		  ;   get_var(LEnv, function, Function_Get166),
		      get_var(LEnv, sys_other_args, Other_args_Get167),
		      CAR=[list, [quote, Function_Get166], sys_getter|Other_args_Get167]
		  )
		),
		[eval_when, [kw_compile_toplevel, kw_load_toplevel, kw_execute], [defmacro, Name_Get153, Bq_append_Ret, Doc_string_Get, [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, Reference_Get158, Env_Get159], [do, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]], [sys_let_list, [], [cons, [list, [car, sys_d], [car, sys_v]], sys_let_list]]], [[null, sys_d], [push, [list, [car, sys_newval], CAR], sys_let_list], ['#BQ', [let_xx, ['#COMMA', [nreverse, sys_let_list]], ['#COMMA', sys_setter]]]]]]]]=MFResult
	      ),
	      block_exit(define_modify_macro, MFResult),
	      true).
:- set_opv(mf_define_modify_type_macro, type_of, sys_macro),
   set_opv(define_modify_macro, symbol_function, mf_define_modify_type_macro),
   DefMacroResult=define_modify_macro.
/*
:- side_effect(assert_lsp(define_modify_macro,
			  doc_string(define_modify_macro,
				     _10326,
				     function,
				     "Creates a new read-modify-write macro like PUSH or INCF."))).
*/
/*
:- side_effect(assert_lsp(define_modify_macro,
			  lambda_def(defmacro,
				     define_modify_macro,
				     mf_define_modify_type_macro,
				     
				     [ sys_name,
				       sys_lambda_list,
				       function,
				       c38_optional,
				       sys_doc_string
				     ],
				     
				     [ 
				       [ let,
					 
					 [ [sys_other_args, []],
					   [sys_rest_arg, []],
					   [sys_env, [gensym]],
					   [sys_reference, [gensym]]
					 ],
					 
					 [ do,
					   
					   [ 
					     [ sys_ll,
					       sys_lambda_list,
					       [cdr, sys_ll]
					     ],
					     [sys_arg, []]
					   ],
					   [[null, sys_ll]],
					   [setq, sys_arg, [car, sys_ll]],
					   
					   [ cond,
					     
					     [ 
					       [ eq,
						 sys_arg,
						 [quote, c38_optional]
					       ]
					     ],
					     
					     [ [eq, sys_arg, [quote, c38_rest]],
					       
					       [ if,
						 [symbolp, [cadr, sys_ll]],
						 
						 [ setq,
						   sys_rest_arg,
						   [cadr, sys_ll]
						 ],
						 
						 [ error,
						   '$ARRAY'([*],
							    claz_base_character,
							    "Non-symbol &REST arg in definition of ~S."),
						   sys_name
						 ]
					       ],
					       
					       [ if,
						 [null, [cddr, sys_ll]],
						 [return, []],
						 
						 [ error,
						   '$ARRAY'([*],
							    claz_base_character,
							    "Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.")
						 ]
					       ]
					     ],
					     
					     [ 
					       [ sys_memq,
						 sys_arg,
						 
						 [ quote,
						   
						   [ c38_key,
						     c38_allow_other_keys,
						     c38_aux
						   ]
						 ]
					       ],
					       
					       [ error,
						 '$ARRAY'([*],
							  claz_base_character,
							  "~S not allowed in DEFINE-MODIFY-MACRO lambda list."),
						 sys_arg
					       ]
					     ],
					     
					     [ [symbolp, sys_arg],
					       [push, sys_arg, sys_other_args]
					     ],
					     
					     [ 
					       [ and,
						 [listp, sys_arg],
						 [symbolp, [car, sys_arg]]
					       ],
					       
					       [ push,
						 [car, sys_arg],
						 sys_other_args
					       ]
					     ],
					     
					     [ t,
					       
					       [ error,
						 '$ARRAY'([*],
							  claz_base_character,
							  "Illegal stuff in DEFINE-MODIFY-MACRO lambda list.")
					       ]
					     ]
					   ]
					 ],
					 
					 [ setq,
					   sys_other_args,
					   [nreverse, sys_other_args]
					 ],
					 
					 [ '#BQ',
					   
					   [ eval_when,
					     
					     [ kw_compile_toplevel,
					       kw_load_toplevel,
					       kw_execute
					     ],
					     
					     [ defmacro,
					       ['#COMMA', sys_name],
					       
					       [ ['#COMMA', sys_reference],
						 
						 [ '#BQ-COMMA-ELIPSE',
						   sys_lambda_list
						 ],
						 c38_environment,
						 ['#COMMA', sys_env]
					       ],
					       ['#COMMA', sys_doc_string],
					       
					       [ multiple_value_bind,
						 
						 [ sys_dummies,
						   sys_vals,
						   sys_newval,
						   sys_setter,
						   sys_getter
						 ],
						 
						 [ get_setf_expansion,
						   ['#COMMA', sys_reference],
						   ['#COMMA', sys_env]
						 ],
						 
						 [ do,
						   
						   [ 
						     [ sys_d,
						       sys_dummies,
						       [cdr, sys_d]
						     ],
						     
						     [ sys_v,
						       sys_vals,
						       [cdr, sys_v]
						     ],
						     
						     [ sys_let_list,
						       [],
						       
						       [ cons,
							 
							 [ list,
							   [car, sys_d],
							   [car, sys_v]
							 ],
							 sys_let_list
						       ]
						     ]
						   ],
						   
						   [ [null, sys_d],
						     
						     [ push,
						       
						       [ list,
							 [car, sys_newval],
							 
							 [ '#COMMA',
							   
							   [ if,
							     sys_rest_arg,
							     
							     [ '#BQ',
							       
							       [ list_xx,
								 
								 [ quote,
								   ['#COMMA', function]
								 ],
								 sys_getter,
								 
								 [ '#BQ-COMMA-ELIPSE',
								   sys_other_args
								 ],
								 
								 [ '#COMMA',
								   sys_rest_arg
								 ]
							       ]
							     ],
							     
							     [ '#BQ',
							       
							       [ list,
								 
								 [ quote,
								   ['#COMMA', function]
								 ],
								 sys_getter,
								 
								 [ '#BQ-COMMA-ELIPSE',
								   sys_other_args
								 ]
							       ]
							     ]
							   ]
							 ]
						       ],
						       sys_let_list
						     ],
						     
						     [ '#BQ',
						       
						       [ let_xx,
							 
							 [ '#COMMA',
							   
							   [ nreverse,
							     sys_let_list
							   ]
							 ],
							 ['#COMMA', sys_setter]
						       ]
						     ]
						   ]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(define_modify_macro,
			  arglist_info(define_modify_macro,
				       mf_define_modify_type_macro,
				       
				       [ sys_name,
					 sys_lambda_list,
					 function,
					 c38_optional,
					 sys_doc_string
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_lambda_list,
						      function,
						      sys_doc_string
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_lambda_list,
							function,
							sys_doc_string
						      ],
						opt:[sys_doc_string],
						req:
						    [ sys_name,
						      sys_lambda_list,
						      function
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(define_modify_macro,
			  init_args(3, mf_define_modify_type_macro))).
*/
/*
; Parse out the variable names and &REST arg from the lambda list.
*/
/*
#+ALT #+ABCL
(define-modify-macro incf-complex (&optional (delta 1)) +
  "The first argument is some location holding a number.  This number is
   incremented by the second argument, DELTA, which defaults to 1.")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:3856 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':ALT'],[#+,':ABCL',['define-modify-macro','incf-complex',['&optional',[delta,1]],+,'$STRING'("The first argument is some location holding a number.  This number is\r\n   incremented by the second argument, DELTA, which defaults to 1.")]]]))
/*
#+ALT #+ABCL
(define-modify-macro decf-complex (&optional (delta 1)) -
  "The first argument is some location holding a number.  This number is
   decremented by the second argument, DELTA, which defaults to 1.")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:4075 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':ALT'],[#+,':ABCL',['define-modify-macro','decf-complex',['&optional',[delta,1]],-,'$STRING'("The first argument is some location holding a number.  This number is\r\n   decremented by the second argument, DELTA, which defaults to 1.")]]]))
/*
#+ALT #+ABCL
(defmacro incf (place &optional (delta 1))
  (cond ((symbolp place)
         (cond ((constantp delta)
                `(setq ,place (+ ,place ,delta)))
               (t
                ;; See section 5.1.3.
                (let ((temp (gensym)))
                  `(let ((,temp ,delta))
                     (setq ,place (+ ,place ,temp)))))))
        ((and (consp place) (eq (car place) 'THE))
         (let ((res (gensym)))
           `(let ((,res (the ,(second place) (+ ,place ,delta))))
              (setf ,(third place) ,res))))
        (t
         `(incf-complex ,place ,delta))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:4294 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':ALT'],[#+,':ABCL',[defmacro,incf,[place,'&optional',[delta,1]],[cond,[[symbolp,place],[cond,[[constantp,delta],['#BQ',[setq,['#COMMA',place],[+,['#COMMA',place],['#COMMA',delta]]]]],[t,[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',delta]]],[setq,['#COMMA',place],[+,['#COMMA',place],['#COMMA',temp]]]]]]]]],[[and,[consp,place],[eq,[car,place],[quote,'THE']]],[let,[[res,[gensym]]],['#BQ',[let,[[['#COMMA',res],[the,['#COMMA',[second,place]],[+,['#COMMA',place],['#COMMA',delta]]]]],[setf,['#COMMA',[third,place]],['#COMMA',res]]]]]],[t,['#BQ',['incf-complex',['#COMMA',place],['#COMMA',delta]]]]]]]]))
/*
; See section 5.1.3.
*/
/*
#+(or WAM-CL LISP500)
(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:4915 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'nth-value',[n,form],['#BQ',[nth,['#COMMA',n],['multiple-value-list',['#COMMA',form]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       nth_value,
					       kw_special,
					       sf_nth_value)).
*/
wl:lambda_def(defmacro, nth_value, mf_nth_value, [sys_n, sys_form], [['#BQ', [nth, ['#COMMA', sys_n], [multiple_value_list, ['#COMMA', sys_form]]]]]).
wl:arglist_info(nth_value, mf_nth_value, [sys_n, sys_form], arginfo{all:[sys_n, sys_form], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_n, sys_form], opt:0, req:[sys_n, sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_nth_value).

/*

### Compiled Macro Operator: `CL:NTH-VALUE` 
*/
sf_nth_value(MacroEnv, N_In, Form_In, RestNKeys, FResult) :-
	mf_nth_value([nth_value, N_In, Form_In|RestNKeys], MacroEnv, MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:NTH-VALUE` 
*/
mf_nth_value([nth_value, N_In, Form_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_n, N_In), bv(sys_form, Form_In)],
	catch(( ( get_var(GEnv, sys_form, Form_Get),
		  get_var(GEnv, sys_n, N_Get)
		),
		[nth, N_Get, [multiple_value_list, Form_Get]]=MFResult
	      ),
	      block_exit(nth_value, MFResult),
	      true).
:- set_opv(mf_nth_value, type_of, sys_macro),
   set_opv(nth_value, symbol_function, mf_nth_value),
   DefMacroResult=nth_value.
/*
:- side_effect(assert_lsp(nth_value,
			  lambda_def(defmacro,
				     nth_value,
				     mf_nth_value,
				     [sys_n, sys_form],
				     
				     [ 
				       [ '#BQ',
					 
					 [ nth,
					   ['#COMMA', sys_n],
					   
					   [ multiple_value_list,
					     ['#COMMA', sys_form]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nth_value,
			  arglist_info(nth_value,
				       mf_nth_value,
				       [sys_n, sys_form],
				       arginfo{ all:[sys_n, sys_form],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_n, sys_form],
						opt:0,
						req:[sys_n, sys_form],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nth_value, init_args(2, mf_nth_value))).
*/
/*
#+WAM-CL
(defmacro incf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number.  This number is
incremented by the second argument, DELTA, which defaults to 1."
  (if (and (symbolp (setq place (%symbol-macroexpand place env)))
           (or (constantp delta)
               (and (symbolp delta)
                    (not (nth-value 1 (%symbol-macroexpand delta env))))))
    `(setq ,place (+ ,place ,delta))
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-method place env)
      (let ((d (gensym)))
        `(let* (,@(mapcar #'list dummies vals)
                (,d ,delta)
                (,(car newval) (+ ,getter ,d)))
           ,setter)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:5012 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,incf,[place,'&optional',[delta,1],'&environment',env],'$STRING'("The first argument is some location holding a number.  This number is\r\nincremented by the second argument, DELTA, which defaults to 1."),[if,[and,[symbolp,[setq,place,['%symbol-macroexpand',place,env]]],[or,[constantp,delta],[and,[symbolp,delta],[not,['nth-value',1,['%symbol-macroexpand',delta,env]]]]]],['#BQ',[setq,['#COMMA',place],[+,['#COMMA',place],['#COMMA',delta]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-method',place,env],[let,[[d,[gensym]]],['#BQ',['let*',[['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',d],['#COMMA',delta]],[['#COMMA',[car,newval]],[+,['#COMMA',getter],['#COMMA',d]]]],['#COMMA',setter]]]]]]])
/*
% macroexpand:-[nth_value,1,[sys_pf_symbol_macroexpand,sys_delta,sys_env]].
*/
/*
% into:-[nth,1,[multiple_value_list,[sys_pf_symbol_macroexpand,sys_delta,sys_env]]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       incf,
					       kw_special,
					       sf_incf)).
*/
doc: doc_string(incf,
	      _8028,
	      function,
	      "The first argument is some location holding a number.  This number is\r\nincremented by the second argument, DELTA, which defaults to 1.").

wl:lambda_def(defmacro, incf, mf_incf, [sys_place, c38_optional, [sys_delta, 1], c38_environment, sys_env], [[if, [and, [symbolp, [setq, sys_place, [sys_pf_symbol_macroexpand, sys_place, sys_env]]], [or, [constantp, sys_delta], [and, [symbolp, sys_delta], [not, [nth_value, 1, [sys_pf_symbol_macroexpand, sys_delta, sys_env]]]]]], ['#BQ', [setq, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [sys_get_setf_method, sys_place, sys_env], [let, [[sys_d, [gensym]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', sys_d], ['#COMMA', sys_delta]], [['#COMMA', [car, sys_newval]], [+, ['#COMMA', sys_getter], ['#COMMA', sys_d]]]], ['#COMMA', sys_setter]]]]]]]).
wl:arglist_info(incf, mf_incf, [sys_place, c38_optional, [sys_delta, 1], c38_environment, sys_env], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[sys_env], key:0, names:[sys_place, sys_delta, sys_env], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, mf_incf).

/*

### Compiled Macro Operator: `CL:INCF` 
*/
sf_incf(Env_In, Place_In, RestNKeys, FResult) :-
	mf_incf([incf, Place_In|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:INCF` 
*/
mf_incf([incf, Place_In|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	AEnv=[bv(sys_place, Place_In), bv(sys_delta, Delta_In), bv(sys_env, Env_In)],
	opt_var(Env_In, sys_delta, Delta_In, true, 1, 1, RestNKeys),
	catch(( ( get_var(AEnv, sys_env, Env_Get),
		  get_var(AEnv, sys_place, Place_Get),
		  f_sys_pf_symbol_macroexpand(Place_Get, Env_Get, PredArgResult),
		  set_var(AEnv, sys_place, PredArgResult),
		  (   is_symbolp(PredArgResult)
		  ->  (   get_var(AEnv, sys_delta, Delta_Get),
			  f_constantp(Delta_Get, FORM1_Res),
			  FORM1_Res\==[],
			  TrueResult27=FORM1_Res
		      ->  true
		      ;   get_var(AEnv, sys_delta, Delta_Get18),
			  (   is_symbolp(Delta_Get18)
			  ->  get_var(AEnv, sys_delta, Delta_Get23),
			      get_var(AEnv, sys_env, Env_Get24),
			      f_sys_pf_symbol_macroexpand(Delta_Get23,
							  Env_Get24,
							  IgnoredRet),
			      nb_current('$mv_return', MV_RETURN),
			      f_nth(1, MV_RETURN, Not_Param),
			      f_not(Not_Param, TrueResult),
			      _8274=TrueResult
			  ;   _8274=[]
			  ),
			  TrueResult27=_8274
		      ),
		      IFTEST=TrueResult27
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(AEnv, sys_delta, Delta_Get30),
		      get_var(AEnv, sys_place, Place_Get28),
		      _8112=[setq, Place_Get28, [+, Place_Get28, Delta_Get30]]
		  ;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|AEnv],
		      get_var(LEnv, sys_env, Env_Get35),
		      get_var(LEnv, sys_place, Place_Get34),
		      f_sys_get_setf_method(Place_Get34,
					    Env_Get35,
					    Setf_method_Ret),
		      setq_from_values(LEnv,
				       
				       [ sys_dummies,
					 sys_vals,
					 sys_newval,
					 sys_setter,
					 sys_getter
				       ]),
		      f_gensym(D_Init),
		      LEnv38=[bv(sys_d, D_Init)|LEnv],
		      get_var(LEnv38, sys_dummies, Dummies_Get),
		      get_var(LEnv38, sys_vals, Vals_Get),
		      f_mapcar(f_list, [Dummies_Get, Vals_Get], Bq_append_Param),
		      get_var(LEnv38, sys_d, D_Get),
		      get_var(LEnv38, sys_delta, Delta_Get43),
		      get_var(LEnv38, sys_newval, Newval_Get),
		      f_car(Newval_Get, Car_Ret),
		      get_var(LEnv38, sys_d, D_Get46),
		      get_var(LEnv38, sys_getter, Getter_Get),
		      bq_append(Bq_append_Param,
				
				[ [D_Get, Delta_Get43],
				  [Car_Ret, [+, Getter_Get, D_Get46]]
				],
				Bq_append_Ret),
		      get_var(LEnv38, sys_setter, Setter_Get),
		      _8112=[let_xx, Bq_append_Ret, Setter_Get]
		  )
		),
		_8112=MFResult
	      ),
	      block_exit(incf, MFResult),
	      true).
:- set_opv(mf_incf, type_of, sys_macro),
   set_opv(incf, symbol_function, mf_incf),
   DefMacroResult=incf.
/*
:- side_effect(assert_lsp(incf,
			  doc_string(incf,
				     _8028,
				     function,
				     "The first argument is some location holding a number.  This number is\r\nincremented by the second argument, DELTA, which defaults to 1."))).
*/
/*
:- side_effect(assert_lsp(incf,
			  lambda_def(defmacro,
				     incf,
				     mf_incf,
				     
				     [ sys_place,
				       c38_optional,
				       [sys_delta, 1],
				       c38_environment,
				       sys_env
				     ],
				     
				     [ 
				       [ if,
					 
					 [ and,
					   
					   [ symbolp,
					     
					     [ setq,
					       sys_place,
					       
					       [ sys_pf_symbol_macroexpand,
						 sys_place,
						 sys_env
					       ]
					     ]
					   ],
					   
					   [ or,
					     [constantp, sys_delta],
					     
					     [ and,
					       [symbolp, sys_delta],
					       
					       [ not,
						 
						 [ nth_value,
						   1,
						   
						   [ sys_pf_symbol_macroexpand,
						     sys_delta,
						     sys_env
						   ]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ setq,
					     ['#COMMA', sys_place],
					     
					     [ (+),
					       ['#COMMA', sys_place],
					       ['#COMMA', sys_delta]
					     ]
					   ]
					 ],
					 
					 [ multiple_value_bind,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_newval,
					     sys_setter,
					     sys_getter
					   ],
					   
					   [ sys_get_setf_method,
					     sys_place,
					     sys_env
					   ],
					   
					   [ let,
					     [[sys_d, [gensym]]],
					     
					     [ '#BQ',
					       
					       [ let_xx,
						 
						 [ 
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ mapcar,
						       function(list),
						       sys_dummies,
						       sys_vals
						     ]
						   ],
						   
						   [ ['#COMMA', sys_d],
						     ['#COMMA', sys_delta]
						   ],
						   
						   [ 
						     [ '#COMMA',
						       [car, sys_newval]
						     ],
						     
						     [ (+),
						       ['#COMMA', sys_getter],
						       ['#COMMA', sys_d]
						     ]
						   ]
						 ],
						 ['#COMMA', sys_setter]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(incf,
			  arglist_info(incf,
				       mf_incf,
				       
				       [ sys_place,
					 c38_optional,
					 [sys_delta, 1],
					 c38_environment,
					 sys_env
				       ],
				       arginfo{ all:[sys_place, sys_delta],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment],
						env:[sys_env],
						key:0,
						names:
						      [ sys_place,
							sys_delta,
							sys_env
						      ],
						opt:[sys_delta],
						req:[sys_place],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(incf, init_args(1, mf_incf))).
*/
/*
#+WAM-CL
(defmacro decf (place &optional (delta 1))
  `(incf ,place (- 0 ,delta)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:5756 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,decf,[place,'&optional',[delta,1]],['#BQ',[incf,['#COMMA',place],[-,0,['#COMMA',delta]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       decf,
					       kw_special,
					       sf_decf)).
*/
wl:lambda_def(defmacro, decf, mf_decf, [sys_place, c38_optional, [sys_delta, 1]], [['#BQ', [incf, ['#COMMA', sys_place], [-, 0, ['#COMMA', sys_delta]]]]]).
wl:arglist_info(decf, mf_decf, [sys_place, c38_optional, [sys_delta, 1]], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, mf_decf).

/*

### Compiled Macro Operator: `CL:DECF` 
*/
sf_decf(MacroEnv, Place_In, RestNKeys, FResult) :-
	mf_decf([decf, Place_In|RestNKeys], MacroEnv, MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DECF` 
*/
mf_decf([decf, Place_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_place, Place_In), bv(sys_delta, Delta_In)],
	opt_var(MacroEnv, sys_delta, Delta_In, true, 1, 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_delta, Delta_Get),
		  get_var(GEnv, sys_place, Place_Get)
		),
		[incf, Place_Get, [-, 0, Delta_Get]]=MFResult
	      ),
	      block_exit(decf, MFResult),
	      true).
:- set_opv(mf_decf, type_of, sys_macro),
   set_opv(decf, symbol_function, mf_decf),
   DefMacroResult=decf.
/*
:- side_effect(assert_lsp(decf,
			  lambda_def(defmacro,
				     decf,
				     mf_decf,
				     [sys_place, c38_optional, [sys_delta, 1]],
				     
				     [ 
				       [ '#BQ',
					 
					 [ incf,
					   ['#COMMA', sys_place],
					   [-, 0, ['#COMMA', sys_delta]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(decf,
			  arglist_info(decf,
				       mf_decf,
				       [sys_place, c38_optional, [sys_delta, 1]],
				       arginfo{ all:[sys_place, sys_delta],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_place, sys_delta],
						opt:[sys_delta],
						req:[sys_place],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(decf, init_args(1, mf_decf))).
*/
/*
#+ALT #+ABCL
(defmacro decf (place &optional (delta 1))
  (cond ((symbolp place)
         (cond ((constantp delta)
                `(setq ,place (- ,place ,delta)))
               (t
                ;; See section 5.1.3.
                (let ((temp (gensym)))
                  `(let ((,temp ,delta))
                     (setq ,place (- ,place ,temp)))))))
        ((and (consp place) (eq (car place) 'THE))
         (let ((res (gensym)))
           `(let ((,res (the ,(second place) (- ,place ,delta))))
              (setf ,(third place) ,res))))
        (t
         `(decf-complex ,place ,delta))))

;;; setf.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
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

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:5844 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':ALT'],[#+,':ABCL',[defmacro,decf,[place,'&optional',[delta,1]],[cond,[[symbolp,place],[cond,[[constantp,delta],['#BQ',[setq,['#COMMA',place],[-,['#COMMA',place],['#COMMA',delta]]]]],[t,[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',delta]]],[setq,['#COMMA',place],[-,['#COMMA',place],['#COMMA',temp]]]]]]]]],[[and,[consp,place],[eq,[car,place],[quote,'THE']]],[let,[[res,[gensym]]],['#BQ',[let,[[['#COMMA',res],[the,['#COMMA',[second,place]],[-,['#COMMA',place],['#COMMA',delta]]]]],[setf,['#COMMA',[third,place]],['#COMMA',res]]]]]],[t,['#BQ',['decf-complex',['#COMMA',place],['#COMMA',delta]]]]]]]]))
/*
; See section 5.1.3.
*/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:8057 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','$STRING'("SYSTEM")])
/*
% macroexpand:-[in_package,'$ARRAY'([*],claz_base_character,"SYSTEM")].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
#+(or ABCL WAM-CL)
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:8082 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-method-inverse',[form,inverse,'setf-function'],[let,[['new-var',[gensym]],[vars,[]],[vals,[]]],[dolist,[x,[cdr,form]],[push,[gensym],vars],[push,x,vals]],[setq,vals,[nreverse,vals]],[values,vars,vals,[list,'new-var'],[if,'setf-function',['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#COMMA','new-var'],['#BQ-COMMA-ELIPSE',vars]]],[if,[functionp,[car,inverse]],['#BQ',[funcall,['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]],['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]]]],['#BQ',[['#COMMA',[car,form]],['#BQ-COMMA-ELIPSE',vars]]]]]])
/*
% macroexpand:-[push,[gensym],sys_vars].
*/
/*
% into:-[setq,sys_vars,[cons,[gensym],sys_vars]].
*/
/*
% macroexpand:-[push,sys_x,sys_vals].
*/
/*
% into:-[setq,sys_vals,[cons,sys_x,sys_vals]].
*/
wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]).
wl:arglist_info(sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_get_setf_method_inverse).

/*

### Compiled Function: `SYS:GET-SETF-METHOD-INVERSE` 
*/
f_sys_get_setf_method_inverse(Form_In, Inverse_In, Setf_function_In, FnResult) :-
	CDR=[bv(sys_form, Form_In), bv(sys_inverse, Inverse_In), bv(sys_setf_function, Setf_function_In)],
	catch(( ( f_gensym(New_var_Init),
		  LEnv=[bv(sys_new_var, New_var_Init), bv(sys_vars, []), bv(sys_vals, [])|CDR],
		  get_var(LEnv, sys_form, Form_Get),
		  f_cdr(Form_Get, List),
		  BV=bv(sys_x, Ele),
		  AEnv=[BV|LEnv],
		  forall(member(Ele, List),
			 ( nb_setarg(2, BV, Ele),
			   f_gensym(Gensym_Ret),
			   get_var(AEnv, sys_vars, Vars_Get),
			   Vars=[Gensym_Ret|Vars_Get],
			   set_var(AEnv, sys_vars, Vars),
			   get_var(AEnv, sys_vals, Vals_Get),
			   get_var(AEnv, sys_x, X_Get),
			   Vals=[X_Get|Vals_Get],
			   set_var(AEnv, sys_vals, Vals)
			 )),
		  get_var(LEnv, sys_vals, Vals_Get21),
		  f_nreverse(Vals_Get21, Vals50),
		  set_var(LEnv, sys_vals, Vals50),
		  get_var(LEnv, sys_new_var, New_var_Get),
		  get_var(LEnv, sys_vals, Vals_Get22),
		  CAR57=[New_var_Get],
		  get_var(LEnv, sys_setf_function, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, sys_inverse, Inverse_Get),
		      get_var(LEnv, sys_new_var, New_var_Get28),
		      get_var(LEnv, sys_vars, Vars_Get29),
		      bq_append(Inverse_Get,
				[New_var_Get28|Vars_Get29],
				TrueResult42),
		      CAR=TrueResult42
		  ;   get_var(LEnv, sys_inverse, Inverse_Get31),
		      f_car(Inverse_Get31, PredArgResult),
		      (   decls:is_functionp(PredArgResult)
		      ->  get_var(LEnv, sys_inverse, Inverse_Get34),
			  get_var(LEnv, sys_new_var, New_var_Get36),
			  get_var(LEnv, sys_vars, Vars_Get35),
			  bq_append(Vars_Get35, [New_var_Get36], Bq_append_Ret),
			  bq_append([funcall|Inverse_Get34],
				    Bq_append_Ret,
				    TrueResult),
			  ElseResult43=TrueResult
		      ;   get_var(LEnv, sys_inverse, Inverse_Get37),
			  get_var(LEnv, sys_new_var, New_var_Get39),
			  get_var(LEnv, sys_vars, Vars_Get38),
			  bq_append(Vars_Get38,
				    [New_var_Get39],
				    Bq_append_Ret54),
			  bq_append(Inverse_Get37, Bq_append_Ret54, ElseResult),
			  ElseResult43=ElseResult
		      ),
		      CAR=ElseResult43
		  ),
		  get_var(LEnv, sys_form, Form_Get44),
		  f_car(Form_Get44, Car_Ret),
		  get_var(LEnv, sys_vars, Vars_Get45),
		  nb_setval('$mv_return',
			    
			    [ sys_vars,
			      Vals_Get22,
			      CAR57,
			      CAR,
			      [Car_Ret|Vars_Get45]
			    ])
		),
		sys_vars=FnResult
	      ),
	      block_exit(sys_get_setf_method_inverse, FnResult),
	      true).
:- set_opv(sys_get_setf_method_inverse,
	   symbol_function,
	   f_sys_get_setf_method_inverse),
   DefunResult=sys_get_setf_method_inverse.
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  lambda_def(defun,
				     sys_get_setf_method_inverse,
				     f_sys_get_setf_method_inverse,
				     [sys_form, sys_inverse, sys_setf_function],
				     
				     [ 
				       [ let,
					 
					 [ [sys_new_var, [gensym]],
					   [sys_vars, []],
					   [sys_vals, []]
					 ],
					 
					 [ dolist,
					   [sys_x, [cdr, sys_form]],
					   [push, [gensym], sys_vars],
					   [push, sys_x, sys_vals]
					 ],
					 [setq, sys_vals, [nreverse, sys_vals]],
					 
					 [ values,
					   sys_vars,
					   sys_vals,
					   [list, sys_new_var],
					   
					   [ if,
					     sys_setf_function,
					     
					     [ '#BQ',
					       
					       [ 
						 [ '#BQ-COMMA-ELIPSE',
						   sys_inverse
						 ],
						 ['#COMMA', sys_new_var],
						 ['#BQ-COMMA-ELIPSE', sys_vars]
					       ]
					     ],
					     
					     [ if,
					       [functionp, [car, sys_inverse]],
					       
					       [ '#BQ',
						 
						 [ funcall,
						   
						   [ '#BQ-COMMA-ELIPSE',
						     sys_inverse
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     sys_vars
						   ],
						   ['#COMMA', sys_new_var]
						 ]
					       ],
					       
					       [ '#BQ',
						 
						 [ 
						   [ '#BQ-COMMA-ELIPSE',
						     sys_inverse
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     sys_vars
						   ],
						   ['#COMMA', sys_new_var]
						 ]
					       ]
					     ]
					   ],
					   
					   [ '#BQ',
					     
					     [ ['#COMMA', [car, sys_form]],
					       ['#BQ-COMMA-ELIPSE', sys_vars]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  arglist_info(sys_get_setf_method_inverse,
				       f_sys_get_setf_method_inverse,
				       
				       [ sys_form,
					 sys_inverse,
					 sys_setf_function
				       ],
				       arginfo{ all:
						    [ sys_form,
						      sys_inverse,
						      sys_setf_function
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_form,
							sys_inverse,
							sys_setf_function
						      ],
						opt:0,
						req:
						    [ sys_form,
						      sys_inverse,
						      sys_setf_function
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  init_args(x, f_sys_get_setf_method_inverse))).
*/
/*
;; If a macro, expand one level and try again.  If not, go for the
*/
/*
;; SETF function.
*/
/*
#+(or ABCL WAM-CL)
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
        (get-setf-method-inverse form `(funcall #'(setf ,(car form)))
                                 t))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:8744 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'expand-or-get-setf-inverse',[form,environment],['multiple-value-bind',[expansion,expanded],['macroexpand-1',form,environment],[if,expanded,['get-setf-expansion',expansion,environment],['get-setf-method-inverse',form,['#BQ',[funcall,function([setf,['#COMMA',[car,form]]])]],t]]]])
wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]).
wl:arglist_info(sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_expand_or_get_setf_inverse).

/*

### Compiled Function: `SYS:EXPAND-OR-GET-SETF-INVERSE` 
*/
f_sys_expand_or_get_setf_inverse(Form_In, Environment_In, FnResult) :-
	CDR=[bv(sys_form, Form_In), bv(sys_environment, Environment_In)],
	catch(( ( LEnv=[bv(sys_expansion, []), bv(sys_expanded, [])|CDR],
		  get_var(LEnv, sys_environment, Environment_Get),
		  get_var(LEnv, sys_form, Form_Get),
		  f_macroexpand_1([Form_Get, Environment_Get],
				  Macroexpand_1_Ret),
		  setq_from_values(LEnv, [sys_expansion, sys_expanded]),
		  get_var(LEnv, sys_expanded, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, sys_environment, Environment_Get15),
		      get_var(LEnv, sys_expansion, Expansion_Get),
		      f_get_setf_expansion(Expansion_Get,
					   [Environment_Get15],
					   TrueResult),
		      LetResult=TrueResult
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
		      LetResult=ElseResult
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_expand_or_get_setf_inverse, FnResult),
	      true).
:- set_opv(sys_expand_or_get_setf_inverse,
	   symbol_function,
	   f_sys_expand_or_get_setf_inverse),
   DefunResult=sys_expand_or_get_setf_inverse.
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  lambda_def(defun,
				     sys_expand_or_get_setf_inverse,
				     f_sys_expand_or_get_setf_inverse,
				     [sys_form, sys_environment],
				     
				     [ 
				       [ multiple_value_bind,
					 [sys_expansion, sys_expanded],
					 
					 [ macroexpand_1,
					   sys_form,
					   sys_environment
					 ],
					 
					 [ if,
					   sys_expanded,
					   
					   [ get_setf_expansion,
					     sys_expansion,
					     sys_environment
					   ],
					   
					   [ sys_get_setf_method_inverse,
					     sys_form,
					     
					     [ '#BQ',
					       
					       [ funcall,
						 function(
							  [ setf,
							    
							    [ '#COMMA',
							      [car, sys_form]
							    ]
							  ])
					       ]
					     ],
					     t
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  arglist_info(sys_expand_or_get_setf_inverse,
				       f_sys_expand_or_get_setf_inverse,
				       [sys_form, sys_environment],
				       arginfo{ all:[sys_form, sys_environment],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_form,
							sys_environment
						      ],
						opt:0,
						req:[sys_form, sys_environment],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  init_args(x, f_sys_expand_or_get_setf_inverse))).
*/
/*
#+(or ABCL WAM-CL)
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
          ((setq temp (get-sysprop  (car form) 'setf-inverse))
           (get-setf-method-inverse form `(,temp) nil))
          ((setq temp (get-sysprop  (car form) 'setf-expander))
           (funcall temp form environment))
          (t
           (expand-or-get-setf-inverse form environment)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:9086 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-expansion',[form,'&optional',environment],[let,[temp],[cond,[[symbolp,form],['multiple-value-bind',[expansion,expanded],['macroexpand-1',form,environment],[if,expanded,['get-setf-expansion',expansion,environment],[let,[['new-var',[gensym]]],[values,[],[],[list,'new-var'],['#BQ',[setq,['#COMMA',form],['#COMMA','new-var']]],form]]]]],[[setq,temp,['get-sysprop',[car,form],[quote,'setf-inverse']]],['get-setf-method-inverse',form,['#BQ',[['#COMMA',temp]]],[]]],[[setq,temp,['get-sysprop',[car,form],[quote,'setf-expander']]],[funcall,temp,form,environment]],[t,['expand-or-get-setf-inverse',form,environment]]]]])
wl:lambda_def(defun, get_setf_expansion, f_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [sys_get_sysprop, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [sys_get_sysprop, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]).
wl:arglist_info(get_setf_expansion, f_get_setf_expansion, [sys_form, c38_optional, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_get_setf_expansion).

/*

### Compiled Function: `CL:GET-SETF-EXPANSION` 
*/
f_get_setf_expansion(Form_In, RestNKeys, FnResult) :-
	CDR=[bv(sys_form, Form_In), bv(sys_environment, Environment_In)],
	opt_var(Env, sys_environment, Environment_In, true, [], 1, RestNKeys),
	catch(( ( LEnv=[bv(sys_temp, [])|CDR],
		  get_var(LEnv, sys_form, Form_Get),
		  (   is_symbolp(Form_Get)
		  ->  LEnv16=[bv(sys_expansion, []), bv(sys_expanded, [])|LEnv],
		      get_var(LEnv16, sys_environment, Environment_Get),
		      get_var(LEnv16, sys_form, Form_Get18),
		      f_macroexpand_1([Form_Get18, Environment_Get],
				      Macroexpand_1_Ret),
		      setq_from_values(LEnv16, [sys_expansion, sys_expanded]),
		      get_var(LEnv16, sys_expanded, IFTEST19),
		      (   IFTEST19\==[]
		      ->  get_var(LEnv16, sys_environment, Environment_Get23),
			  get_var(LEnv16, sys_expansion, Expansion_Get),
			  f_get_setf_expansion(Expansion_Get,
					       [Environment_Get23],
					       TrueResult),
			  LetResult15=TrueResult
		      ;   f_gensym(New_var_Init),
			  LEnv26=[bv(sys_new_var, New_var_Init)|LEnv16],
			  get_var(LEnv26, sys_new_var, New_var_Get),
			  CAR=[New_var_Get],
			  get_var(LEnv26, sys_form, Form_Get29),
			  get_var(LEnv26, sys_new_var, New_var_Get30),
			  nb_setval('$mv_return',
				    
				    [ [],
				      [],
				      CAR,
				      [setq, Form_Get29, New_var_Get30],
				      Form_Get29
				    ]),
			  LetResult15=[]
		      ),
		      LetResult=LetResult15
		  ;   get_var(LEnv, sys_form, Form_Get36),
		      f_car(Form_Get36, Get_sysprop_Param),
		      f_sys_get_sysprop(Get_sysprop_Param,
					sys_setf_inverse,
					[],
					IFTEST33),
		      set_var(LEnv, sys_temp, IFTEST33),
		      (   IFTEST33\==[]
		      ->  get_var(LEnv, sys_form, Form_Get37),
			  get_var(LEnv, sys_temp, Temp_Get),
			  f_sys_get_setf_method_inverse(Form_Get37,
							[Temp_Get],
							[],
							TrueResult49),
			  ElseResult52=TrueResult49
		      ;   get_var(LEnv, sys_form, Form_Get41),
			  f_car(Form_Get41, Get_sysprop_Param56),
			  f_sys_get_sysprop(Get_sysprop_Param56,
					    sys_setf_expander,
					    [],
					    IFTEST39),
			  set_var(LEnv, sys_temp, IFTEST39),
			  (   IFTEST39\==[]
			  ->  get_var(LEnv, sys_environment, Environment_Get44),
			      get_var(LEnv, sys_form, Form_Get43),
			      get_var(LEnv, sys_temp, Temp_Get42),
			      f_apply(Temp_Get42,
				      [Form_Get43, Environment_Get44],
				      TrueResult47),
			      ElseResult50=TrueResult47
			  ;   get_var(LEnv, sys_environment, Environment_Get46),
			      get_var(LEnv, sys_form, Form_Get45),
			      f_sys_expand_or_get_setf_inverse(Form_Get45,
							       Environment_Get46,
							       ElseResult),
			      ElseResult50=ElseResult
			  ),
			  ElseResult52=ElseResult50
		      ),
		      LetResult=ElseResult52
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(get_setf_expansion, FnResult),
	      true).
:- set_opv(get_setf_expansion, symbol_function, f_get_setf_expansion),
   DefunResult=get_setf_expansion.
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  lambda_def(defun,
				     get_setf_expansion,
				     f_get_setf_expansion,
				     [sys_form, c38_optional, sys_environment],
				     
				     [ 
				       [ let,
					 [sys_temp],
					 
					 [ cond,
					   
					   [ [symbolp, sys_form],
					     
					     [ multiple_value_bind,
					       [sys_expansion, sys_expanded],
					       
					       [ macroexpand_1,
						 sys_form,
						 sys_environment
					       ],
					       
					       [ if,
						 sys_expanded,
						 
						 [ get_setf_expansion,
						   sys_expansion,
						   sys_environment
						 ],
						 
						 [ let,
						   [[sys_new_var, [gensym]]],
						   
						   [ values,
						     [],
						     [],
						     [list, sys_new_var],
						     
						     [ '#BQ',
						       
						       [ setq,
							 ['#COMMA', sys_form],
							 
							 [ '#COMMA',
							   sys_new_var
							 ]
						       ]
						     ],
						     sys_form
						   ]
						 ]
					       ]
					     ]
					   ],
					   
					   [ 
					     [ setq,
					       sys_temp,
					       
					       [ sys_get_sysprop,
						 [car, sys_form],
						 [quote, sys_setf_inverse]
					       ]
					     ],
					     
					     [ sys_get_setf_method_inverse,
					       sys_form,
					       ['#BQ', [['#COMMA', sys_temp]]],
					       []
					     ]
					   ],
					   
					   [ 
					     [ setq,
					       sys_temp,
					       
					       [ sys_get_sysprop,
						 [car, sys_form],
						 [quote, sys_setf_expander]
					       ]
					     ],
					     
					     [ funcall,
					       sys_temp,
					       sys_form,
					       sys_environment
					     ]
					   ],
					   
					   [ t,
					     
					     [ sys_expand_or_get_setf_inverse,
					       sys_form,
					       sys_environment
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  arglist_info(get_setf_expansion,
				       f_get_setf_expansion,
				       [sys_form, c38_optional, sys_environment],
				       arginfo{ all:[sys_form, sys_environment],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_form,
							sys_environment
						      ],
						opt:[sys_environment],
						req:[sys_form],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  init_args(1, f_get_setf_expansion))).
*/
/*
#+(or ABCL WAM-CL)
(defmacro setf (&rest args &environment environment)
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
                (let ((inverse (get-sysprop  (car place) 'setf-inverse)))
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


;; (defsetf subseq (sequence start &optional (end nil)) (v)
;;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
;;      ,v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:9860 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,setf,['&rest',args,'&environment',environment],[let,[[numargs,[length,args]]],[cond,[[=,numargs,2],[let,[[place,[first,args]],['value-form',[second,args]]],[if,[atom,place],['#BQ',[setq,['#COMMA',place],['#COMMA','value-form']]],[progn,['multiple-value-bind',[dummies,vals,'store-vars',setter,getter],['get-setf-expansion',place,environment],[let,[[inverse,['get-sysprop',[car,place],[quote,'setf-inverse']]]],[if,[and,inverse,[eq,inverse,[car,setter]]],[if,[functionp,inverse],['#BQ',[funcall,['#COMMA',inverse],['#BQ-COMMA-ELIPSE',[cdr,place]],['#COMMA','value-form']]],['#BQ',[['#COMMA',inverse],['#BQ-COMMA-ELIPSE',[cdr,place]],['#COMMA','value-form']]]],[if,[or,[null,'store-vars'],[cdr,'store-vars']],['#BQ',['let*',[['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]]],['multiple-value-bind',['#COMMA','store-vars'],['#COMMA','value-form'],['#COMMA',setter]]]],['#BQ',['let*',[['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],['#COMMA',[list,[car,'store-vars'],'value-form']]],['#COMMA',setter]]]]]]]]]]],[[oddp,numargs],[error,'$STRING'("Odd number of arguments to SETF.")]],[t,[do,[[a,args,[cddr,a]],[l,[]]],[[null,a],['#BQ',[progn,['#BQ-COMMA-ELIPSE',[nreverse,l]]]]],[setq,l,[cons,[list,[quote,setf],[car,a],[cadr,a]],l]]]]]]])
wl:lambda_def(defmacro, setf, mf_setf, [c38_rest, sys_args, c38_environment, sys_environment], [[let, [[sys_numargs, [length, sys_args]]], [cond, [[=, sys_numargs, 2], [let, [[sys_place, [first, sys_args]], [sys_value_form, [second, sys_args]]], [if, [atom, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]], [progn, [multiple_value_bind, [sys_dummies, sys_vals, sys_store_vars, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_environment], [let, [[sys_inverse, [sys_get_sysprop, [car, sys_place], [quote, sys_setf_inverse]]]], [if, [and, sys_inverse, [eq, sys_inverse, [car, sys_setter]]], [if, [functionp, sys_inverse], ['#BQ', [funcall, ['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]], ['#BQ', [['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]]], [if, [or, [null, sys_store_vars], [cdr, sys_store_vars]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]]], [multiple_value_bind, ['#COMMA', sys_store_vars], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], ['#COMMA', [list, [car, sys_store_vars], sys_value_form]]], ['#COMMA', sys_setter]]]]]]]]]]], [[oddp, sys_numargs], [error, '$ARRAY'([*], claz_base_character, "Odd number of arguments to SETF.")]], [t, [do, [[sys_a, sys_args, [cddr, sys_a]], [sys_l, []]], [[null, sys_a], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]]], [setq, sys_l, [cons, [list, [quote, setf], [car, sys_a], [cadr, sys_a]], sys_l]]]]]]]).
wl:arglist_info(setf, mf_setf, [c38_rest, sys_args, c38_environment, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[sys_args, sys_environment], opt:0, req:0, rest:[sys_args], sublists:0, whole:0}).
wl: init_args(0, mf_setf).

/*

### Compiled Macro Operator: `CL:SETF` 
*/
sf_setf(Environment_In, RestNKeys, FResult) :-
	mf_setf([setf|RestNKeys], Environment_In, MFResult),
	f_sys_env_eval(Environment_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:SETF` 
*/
mf_setf([setf|RestNKeys], Environment_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_args, RestNKeys), bv(sys_environment, Environment_In)],
	catch(( ( get_var(GEnv, sys_args, Args_Get),
		  f_length(Args_Get, Numargs_Init),
		  LEnv=[bv(sys_numargs, Numargs_Init)|GEnv],
		  get_var(LEnv, sys_numargs, Numargs_Get),
		  (   Numargs_Get=:=2
		  ->  get_var(LEnv, sys_args, Args_Get18),
		      f_car(Args_Get18, Place_Init),
		      get_var(LEnv, sys_args, Args_Get19),
		      f_second(Args_Get19, Value_form_Init),
		      LEnv17=[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)|LEnv],
		      get_var(LEnv17, sys_place, Place_Get),
		      (   Place_Get\=[CAR|CDR]
		      ->  get_var(LEnv17, sys_place, Place_Get26),
			  get_var(LEnv17, sys_value_form, Value_form_Get),
			  LetResult16=[setq, Place_Get26, Value_form_Get]
		      ;   LEnv30=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_store_vars, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv17],
			  get_var(LEnv30, sys_environment, Environment_Get),
			  get_var(LEnv30, sys_place, Place_Get31),
			  f_get_setf_expansion(Place_Get31,
					       [Environment_Get],
					       Setf_expansion_Ret),
			  setq_from_values(LEnv30,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_store_vars,
					     sys_setter,
					     sys_getter
					   ]),
			  get_var(LEnv30, sys_place, Place_Get36),
			  f_car(Place_Get36, Get_sysprop_Param),
			  f_sys_get_sysprop(Get_sysprop_Param,
					    sys_setf_inverse,
					    [],
					    Inverse_Init),
			  LEnv35=[bv(sys_inverse, Inverse_Init)|LEnv30],
			  get_var(LEnv35, sys_inverse, IFTEST40),
			  (   IFTEST40\==[]
			  ->  get_var(LEnv35, sys_inverse, Inverse_Get43),
			      get_var(LEnv35, sys_setter, Setter_Get),
			      f_car(Setter_Get, Car_Ret),
			      f_eq(Inverse_Get43, Car_Ret, TrueResult),
			      IFTEST38=TrueResult
			  ;   IFTEST38=[]
			  ),
			  (   IFTEST38\==[]
			  ->  get_var(LEnv35, sys_inverse, Inverse_Get47),
			      (   decls:is_functionp(Inverse_Get47)
			      ->  get_var(LEnv35, sys_inverse, Inverse_Get50),
				  get_var(LEnv35, sys_place, Place_Get51),
				  f_cdr(Place_Get51, Cdr_Ret),
				  get_var(LEnv35,
					  sys_value_form,
					  Value_form_Get52),
				  bq_append([Inverse_Get50|Cdr_Ret],
					    [Value_form_Get52],
					    Bq_append_Ret),
				  TrueResult72=[funcall|Bq_append_Ret]
			      ;   get_var(LEnv35, sys_inverse, Inverse_Get53),
				  get_var(LEnv35, sys_place, Place_Get54),
				  f_cdr(Place_Get54, Cdr_Ret136),
				  get_var(LEnv35,
					  sys_value_form,
					  Value_form_Get55),
				  bq_append([Inverse_Get53|Cdr_Ret136],
					    [Value_form_Get55],
					    ElseResult),
				  TrueResult72=ElseResult
			      ),
			      LetResult34=TrueResult72
			  ;   (   get_var(LEnv35,
					  sys_store_vars,
					  Store_vars_Get),
				  f_null(Store_vars_Get, FORM1_Res),
				  FORM1_Res\==[],
				  IFTEST57=FORM1_Res
			      ->  true
			      ;   get_var(LEnv35,
					  sys_store_vars,
					  Store_vars_Get60),
				  f_cdr(Store_vars_Get60, Cdr_Ret137),
				  IFTEST57=Cdr_Ret137
			      ),
			      (   IFTEST57\==[]
			      ->  get_var(LEnv35, sys_dummies, Dummies_Get),
				  get_var(LEnv35, sys_vals, Vals_Get),
				  f_mapcar(f_list,
					   [Dummies_Get, Vals_Get],
					   Mapcar_Ret),
				  get_var(LEnv35, sys_setter, Setter_Get66),
				  get_var(LEnv35,
					  sys_store_vars,
					  Store_vars_Get64),
				  get_var(LEnv35,
					  sys_value_form,
					  Value_form_Get65),
				  ElseResult73=[let_xx, Mapcar_Ret, [multiple_value_bind, Store_vars_Get64, Value_form_Get65, Setter_Get66]]
			      ;   get_var(LEnv35, sys_dummies, Dummies_Get67),
				  get_var(LEnv35, sys_vals, Vals_Get68),
				  f_mapcar(f_list,
					   [Dummies_Get67, Vals_Get68],
					   Bq_append_Param),
				  get_var(LEnv35,
					  sys_store_vars,
					  Store_vars_Get69),
				  f_car(Store_vars_Get69, Car_Ret139),
				  get_var(LEnv35,
					  sys_value_form,
					  Value_form_Get70),
				  CAR141=[Car_Ret139, Value_form_Get70],
				  bq_append(Bq_append_Param,
					    [CAR141],
					    Bq_append_Ret140),
				  get_var(LEnv35, sys_setter, Setter_Get71),
				  ElseResult73=[let_xx, Bq_append_Ret140, Setter_Get71]
			      ),
			      LetResult34=ElseResult73
			  ),
			  LetResult16=LetResult34
		      ),
		      LetResult=LetResult16
		  ;   get_var(LEnv, sys_numargs, Numargs_Get76),
		      (   mth:is_oddp(Numargs_Get76)
		      ->  f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "Odd number of arguments to SETF.")
				  ],
				  TrueResult118),
			  ElseResult121=TrueResult118
		      ;   get_var(LEnv, sys_args, Args_Get82),
			  AEnv=[bv(sys_a, Args_Get82), bv(sys_l, [])|LEnv],
			  catch(( call_addr_block(AEnv,
						  (push_label(do_label_20), get_var(AEnv, sys_a, IFTEST102), (IFTEST102==[]->get_var(AEnv, sys_l, L_Get107), f_nreverse(L_Get107, Nreverse_Ret), throw(block_exit([], [progn|Nreverse_Ret])), _TBResult=ThrowResult106;get_var(AEnv, sys_a, A_Get110), f_car(A_Get110, Car_Ret143), get_var(AEnv, sys_a, A_Get111), f_cadr(A_Get111, Cadr_Ret), CAR145=[setf, Car_Ret143, Cadr_Ret], get_var(AEnv, sys_l, L_Get112), L=[CAR145|L_Get112], set_var(AEnv, sys_l, L), get_var(AEnv, sys_a, A_Get113), f_cddr(A_Get113, A), set_var(AEnv, sys_a, A), goto(do_label_20, AEnv), _TBResult=_GORES114)),
						  
						  [ addr(addr_tagbody_20_do_label_20,
							 do_label_20,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_a, IFTEST85), (IFTEST85==[]->get_var(AEnv, sys_l, Nreverse_Param), f_nreverse(Nreverse_Param, Nreverse_Ret146), throw(block_exit([], [progn|Nreverse_Ret146])), _12224=ThrowResult;get_var(AEnv, sys_a, A_Get93), f_car(A_Get93, Car_Ret147), get_var(AEnv, sys_a, A_Get94), f_cadr(A_Get94, Cadr_Ret148), CAR149=[setf, Car_Ret147, Cadr_Ret148], get_var(AEnv, sys_l, L_Get95), Set_var_Ret=[CAR149|L_Get95], set_var(AEnv, sys_l, Set_var_Ret), get_var(AEnv, sys_a, A_Get96), f_cddr(A_Get96, Cddr_Ret), set_var(AEnv, sys_a, Cddr_Ret), goto(do_label_20, AEnv), _12224=_GORES)))
						  ]),
				  []=LetResult80
				),
				block_exit([], LetResult80),
				true),
			  ElseResult121=LetResult80
		      ),
		      LetResult=ElseResult121
		  )
		),
		LetResult=MFResult
	      ),
	      block_exit(setf, MFResult),
	      true).
:- set_opv(mf_setf, type_of, sys_macro),
   set_opv(setf, symbol_function, mf_setf),
   DefMacroResult=setf.
/*
:- side_effect(assert_lsp(setf,
			  lambda_def(defmacro,
				     setf,
				     mf_setf,
				     
				     [ c38_rest,
				       sys_args,
				       c38_environment,
				       sys_environment
				     ],
				     
				     [ 
				       [ let,
					 [[sys_numargs, [length, sys_args]]],
					 
					 [ cond,
					   
					   [ [=, sys_numargs, 2],
					     
					     [ let,
					       
					       [ [sys_place, [first, sys_args]],
						 
						 [ sys_value_form,
						   [second, sys_args]
						 ]
					       ],
					       
					       [ if,
						 [atom, sys_place],
						 
						 [ '#BQ',
						   
						   [ setq,
						     ['#COMMA', sys_place],
						     ['#COMMA', sys_value_form]
						   ]
						 ],
						 
						 [ progn,
						   
						   [ multiple_value_bind,
						     
						     [ sys_dummies,
						       sys_vals,
						       sys_store_vars,
						       sys_setter,
						       sys_getter
						     ],
						     
						     [ get_setf_expansion,
						       sys_place,
						       sys_environment
						     ],
						     
						     [ let,
						       
						       [ 
							 [ sys_inverse,
							   
							   [ sys_get_sysprop,
							     [car, sys_place],
							     
							     [ quote,
							       sys_setf_inverse
							     ]
							   ]
							 ]
						       ],
						       
						       [ if,
							 
							 [ and,
							   sys_inverse,
							   
							   [ eq,
							     sys_inverse,
							     [car, sys_setter]
							   ]
							 ],
							 
							 [ if,
							   
							   [ functionp,
							     sys_inverse
							   ],
							   
							   [ '#BQ',
							     
							     [ funcall,
							       
							       [ '#COMMA',
								 sys_inverse
							       ],
							       
							       [ '#BQ-COMMA-ELIPSE',
								 [cdr, sys_place]
							       ],
							       
							       [ '#COMMA',
								 sys_value_form
							       ]
							     ]
							   ],
							   
							   [ '#BQ',
							     
							     [ 
							       [ '#COMMA',
								 sys_inverse
							       ],
							       
							       [ '#BQ-COMMA-ELIPSE',
								 [cdr, sys_place]
							       ],
							       
							       [ '#COMMA',
								 sys_value_form
							       ]
							     ]
							   ]
							 ],
							 
							 [ if,
							   
							   [ or,
							     
							     [ null,
							       sys_store_vars
							     ],
							     
							     [ cdr,
							       sys_store_vars
							     ]
							   ],
							   
							   [ '#BQ',
							     
							     [ let_xx,
							       
							       [ 
								 [ '#BQ-COMMA-ELIPSE',
								   
								   [ mapcar,
								     function(list),
								     sys_dummies,
								     sys_vals
								   ]
								 ]
							       ],
							       
							       [ multiple_value_bind,
								 
								 [ '#COMMA',
								   sys_store_vars
								 ],
								 
								 [ '#COMMA',
								   sys_value_form
								 ],
								 
								 [ '#COMMA',
								   sys_setter
								 ]
							       ]
							     ]
							   ],
							   
							   [ '#BQ',
							     
							     [ let_xx,
							       
							       [ 
								 [ '#BQ-COMMA-ELIPSE',
								   
								   [ mapcar,
								     function(list),
								     sys_dummies,
								     sys_vals
								   ]
								 ],
								 
								 [ '#COMMA',
								   
								   [ list,
								     
								     [ car,
								       sys_store_vars
								     ],
								     sys_value_form
								   ]
								 ]
							       ],
							       
							       [ '#COMMA',
								 sys_setter
							       ]
							     ]
							   ]
							 ]
						       ]
						     ]
						   ]
						 ]
					       ]
					     ]
					   ],
					   
					   [ [oddp, sys_numargs],
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"Odd number of arguments to SETF.")
					     ]
					   ],
					   
					   [ t,
					     
					     [ do,
					       
					       [ [sys_a, sys_args, [cddr, sys_a]],
						 [sys_l, []]
					       ],
					       
					       [ [null, sys_a],
						 
						 [ '#BQ',
						   
						   [ progn,
						     
						     [ '#BQ-COMMA-ELIPSE',
						       [nreverse, sys_l]
						     ]
						   ]
						 ]
					       ],
					       
					       [ setq,
						 sys_l,
						 
						 [ cons,
						   
						   [ list,
						     [quote, setf],
						     [car, sys_a],
						     [cadr, sys_a]
						   ],
						   sys_l
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(setf,
			  arglist_info(setf,
				       mf_setf,
				       
				       [ c38_rest,
					 sys_args,
					 c38_environment,
					 sys_environment
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest, environment],
						env:[sys_environment],
						key:0,
						names:
						      [ sys_args,
							sys_environment
						      ],
						opt:0,
						req:0,
						rest:[sys_args],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(setf, init_args(0, mf_setf))).
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
#+(or ABCL WAM-CL)
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:11430 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-subseq',[sequence,start,'&rest',rest],[let,[[end,[]],v],[ecase,[length,rest],[1,[setq,v,[car,rest]]],[2,[setq,end,[car,rest],v,[cadr,rest]]]],[progn,[replace,sequence,v,':start1',start,':end1',end],v]]])
/*
% ecase:-[[1,[setq,sys_v,[car,rest]]],[2,[setq,sys_end,[car,rest],sys_v,[cadr,rest]]]].
*/
/*
% conds:-[[[eq,_29140,[quote,1]],[progn,[setq,sys_v,[car,rest]]]],[[eq,_29140,[quote,2]],[progn,[setq,sys_end,[car,rest],sys_v,[cadr,rest]]]],[t,[type_error,_29394,[quote,[member,1,2]]]]].
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],type_error,kw_function,f_type_error)).
*/
wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, sys_start, c38_rest, rest], [[let, [[sys_end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, sys_end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, sys_start, kw_end1, sys_end], sys_v]]]).
wl:arglist_info(sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, sys_start, c38_rest, rest], arginfo{all:[sequence, sys_start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, sys_start, rest], opt:0, req:[sequence, sys_start], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, f_sys_pf_set_subseq).

/*

### Compiled Function: `SYS::%SET-SUBSEQ` 
*/
f_sys_pf_set_subseq(Sequence_In, Start_In, RestNKeys, FnResult) :-
	CDR=[bv(sequence, Sequence_In), bv(sys_start, Start_In), bv(rest, RestNKeys)],
	catch(( ( LEnv=[bv(sys_end, []), bv(sys_v, [])|CDR],
		  get_var(LEnv, rest, Rest_Get),
		  f_length(Rest_Get, Key),
		  (   is_eq(Key, 1)
		  ->  get_var(LEnv, rest, Rest_Get17),
		      f_car(Rest_Get17, TrueResult24),
		      set_var(LEnv, sys_v, TrueResult24),
		      _7208=TrueResult24
		  ;   (   is_eq(Key, 2)
		      ->  get_var(LEnv, rest, Rest_Get20),
			  f_car(Rest_Get20, End),
			  set_var(LEnv, sys_end, End),
			  get_var(LEnv, rest, Rest_Get21),
			  f_cadr(Rest_Get21, TrueResult),
			  set_var(LEnv, sys_v, TrueResult),
			  ElseResult25=TrueResult
		      ;   f_type_error(CAR, [member, 1, 2], ElseResult),
			  ElseResult25=ElseResult
		      ),
		      _7208=ElseResult25
		  ),
		  get_var(LEnv, sequence, Sequence_Get),
		  get_var(LEnv, sys_end, End_Get),
		  get_var(LEnv, sys_start, Start_Get),
		  get_var(LEnv, sys_v, V_Get),
		  f_replace(Sequence_Get,
			    V_Get,
			    [kw_start1, Start_Get, kw_end1, End_Get],
			    Replace_Ret),
		  get_var(LEnv, sys_v, V_Get30)
		),
		V_Get30=FnResult
	      ),
	      block_exit(sys_pf_set_subseq, FnResult),
	      true).
:- set_opv(sys_pf_set_subseq, symbol_function, f_sys_pf_set_subseq),
   DefunResult=sys_pf_set_subseq.
/*
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  lambda_def(defun,
				     sys_pf_set_subseq,
				     f_sys_pf_set_subseq,
				     [sequence, sys_start, c38_rest, rest],
				     
				     [ 
				       [ let,
					 [[sys_end, []], sys_v],
					 
					 [ ecase,
					   [length, rest],
					   [1, [setq, sys_v, [car, rest]]],
					   
					   [ 2,
					     
					     [ setq,
					       sys_end,
					       [car, rest],
					       sys_v,
					       [cadr, rest]
					     ]
					   ]
					 ],
					 
					 [ progn,
					   
					   [ replace,
					     sequence,
					     sys_v,
					     kw_start1,
					     sys_start,
					     kw_end1,
					     sys_end
					   ],
					   sys_v
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  arglist_info(sys_pf_set_subseq,
				       f_sys_pf_set_subseq,
				       [sequence, sys_start, c38_rest, rest],
				       arginfo{ all:[sequence, sys_start],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sequence,
							sys_start,
							rest
						      ],
						opt:0,
						req:[sequence, sys_start],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_subseq, init_args(2, f_sys_pf_set_subseq))).
*/
/*
#+(or ABCL WAM-CL)
(defun %define-setf-macro (name expander inverse doc)
  (declare (ignore doc)) ; FIXME
  (when inverse
    (put-sysprop name 'setf-inverse inverse))
  (when expander
    (put-sysprop name 'setf-expander expander))
  name)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:11733 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%define-setf-macro',[name,expander,inverse,doc],[declare,[ignore,doc]],[when,inverse,['put-sysprop',name,[quote,'setf-inverse'],inverse]],[when,expander,['put-sysprop',name,[quote,'setf-expander'],expander]],name])
wl:lambda_def(defun, sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [[declare, [ignore, sys_doc]], [when, sys_inverse, [sys_put_sysprop, sys_name, [quote, sys_setf_inverse], sys_inverse]], [when, sys_expander, [sys_put_sysprop, sys_name, [quote, sys_setf_expander], sys_expander]], sys_name]).
wl:arglist_info(sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}).
wl: init_args(4, f_sys_pf_define_setf_type_macro).

/*

### Compiled Function: `SYS::%DEFINE-SETF-MACRO` 
*/
f_sys_pf_define_setf_type_macro(Name_In, Expander_In, Inverse_In, Doc_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_name, Name_In), bv(sys_expander, Expander_In), bv(sys_inverse, Inverse_In), bv(sys_doc, Doc_In)],
	catch(( ( sf_declare(GEnv, [ignore, sys_doc], Sf_declare_Ret),
		  get_var(GEnv, sys_inverse, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_inverse, Inverse_Get12),
		      get_var(GEnv, sys_name, Name_Get),
		      f_sys_put_sysprop(Name_Get,
					sys_setf_inverse,
					Inverse_Get12,
					[],
					TrueResult),
		      _6944=TrueResult
		  ;   _6944=[]
		  ),
		  get_var(GEnv, sys_expander, IFTEST14),
		  (   IFTEST14\==[]
		  ->  get_var(GEnv, sys_expander, Expander_Get18),
		      get_var(GEnv, sys_name, Name_Get17),
		      f_sys_put_sysprop(Name_Get17,
					sys_setf_expander,
					Expander_Get18,
					[],
					TrueResult19),
		      _7060=TrueResult19
		  ;   _7060=[]
		  ),
		  get_var(GEnv, sys_name, Name_Get20)
		),
		Name_Get20=FnResult
	      ),
	      block_exit(sys_pf_define_setf_macro, FnResult),
	      true).
:- set_opv(sys_pf_define_setf_macro,
	   symbol_function,
	   f_sys_pf_define_setf_type_macro),
   DefunResult=sys_pf_define_setf_macro.
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  lambda_def(defun,
				     sys_pf_define_setf_macro,
				     f_sys_pf_define_setf_type_macro,
				     
				     [ sys_name,
				       sys_expander,
				       sys_inverse,
				       sys_doc
				     ],
				     
				     [ [declare, [ignore, sys_doc]],
				       
				       [ when,
					 sys_inverse,
					 
					 [ sys_put_sysprop,
					   sys_name,
					   [quote, sys_setf_inverse],
					   sys_inverse
					 ]
				       ],
				       
				       [ when,
					 sys_expander,
					 
					 [ sys_put_sysprop,
					   sys_name,
					   [quote, sys_setf_expander],
					   sys_expander
					 ]
				       ],
				       sys_name
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  arglist_info(sys_pf_define_setf_macro,
				       f_sys_pf_define_setf_type_macro,
				       
				       [ sys_name,
					 sys_expander,
					 sys_inverse,
					 sys_doc
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_expander,
						      sys_inverse,
						      sys_doc
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_expander,
							sys_inverse,
							sys_doc
						      ],
						opt:0,
						req:
						    [ sys_name,
						      sys_expander,
						      sys_inverse,
						      sys_doc
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  init_args(4, f_sys_pf_define_setf_type_macro))).
*/
/*
 FIXME
*/
/*
#+(or ABCL WAM-CL)
(defmacro defsetf (access-function update-function)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (put-sysprop ',access-function 'setf-inverse ',update-function)))


;; #+(or ABCL WAM-CL) (flet () ;; FLET BEGIN

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:11986 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defsetf,['access-function','update-function'],['#BQ',['eval-when',[':load-toplevel',':compile-toplevel',':execute'],['put-sysprop',[quote,['#COMMA','access-function']],[quote,'setf-inverse'],[quote,['#COMMA','update-function']]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       defsetf,
					       kw_macro,
					       mf_defsetf)).
*/
wl:lambda_def(defmacro, defsetf, mf_defsetf, [sys_access_function, sys_update_function], [['#BQ', [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put_sysprop, [quote, ['#COMMA', sys_access_function]], [quote, sys_setf_inverse], [quote, ['#COMMA', sys_update_function]]]]]]).
wl:arglist_info(defsetf, mf_defsetf, [sys_access_function, sys_update_function], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_defsetf).

/*

### Compiled Macro Operator: `CL:DEFSETF` 
*/
sf_defsetf(MacroEnv, Access_function_In, Update_function_In, RestNKeys, FResult) :-
	mf_defsetf([defsetf, Access_function_In, Update_function_In|RestNKeys],
		   MacroEnv,
		   MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFSETF` 
*/
mf_defsetf([defsetf, Access_function_In, Update_function_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_access_function, Access_function_In), bv(sys_update_function, Update_function_In)],
	catch(( ( get_var(GEnv, sys_access_function, Access_function_Get),
		  get_var(GEnv, sys_update_function, Update_function_Get)
		),
		[eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put_sysprop, [quote, Access_function_Get], [quote, sys_setf_inverse], [quote, Update_function_Get]]]=MFResult
	      ),
	      block_exit(defsetf, MFResult),
	      true).
:- set_opv(mf_defsetf, type_of, sys_macro),
   set_opv(defsetf, symbol_function, mf_defsetf),
   DefMacroResult=defsetf.
/*
:- side_effect(assert_lsp(defsetf,
			  lambda_def(defmacro,
				     defsetf,
				     mf_defsetf,
				     [sys_access_function, sys_update_function],
				     
				     [ 
				       [ '#BQ',
					 
					 [ eval_when,
					   
					   [ kw_load_toplevel,
					     kw_compile_toplevel,
					     kw_execute
					   ],
					   
					   [ sys_put_sysprop,
					     
					     [ quote,
					       ['#COMMA', sys_access_function]
					     ],
					     [quote, sys_setf_inverse],
					     
					     [ quote,
					       ['#COMMA', sys_update_function]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(defsetf,
			  arglist_info(defsetf,
				       mf_defsetf,
				       
				       [ sys_access_function,
					 sys_update_function
				       ],
				       arginfo{ all:
						    [ sys_access_function,
						      sys_update_function
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_access_function,
							sys_update_function
						      ],
						opt:0,
						req:
						    [ sys_access_function,
						      sys_update_function
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defsetf, init_args(2, mf_defsetf))).
*/
/*
; #+(or ABCL WAM-CL) (flet () ;; FLET BEGIN
*/
/*
(defun %set-caar (x v) (set-car (car x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12242 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caar',[x,v],['set-car',[car,x],v]])
wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caar).

/*

### Compiled Function: `SYS::%SET-CAAR` 
*/
f_sys_pf_set_caar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_car(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caar, FnResult),
	      true).
:- set_opv(sys_pf_set_caar, symbol_function, f_sys_pf_set_caar),
   DefunResult=sys_pf_set_caar.
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  lambda_def(defun,
				     sys_pf_set_caar,
				     f_sys_pf_set_caar,
				     [sys_x, sys_v],
				     [[sys_set_car, [car, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caar,
			  arglist_info(sys_pf_set_caar,
				       f_sys_pf_set_caar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caar, init_args(x, f_sys_pf_set_caar))).
*/
/*
(defun %set-cadr (x v) (set-car (cdr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12287 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadr',[x,v],['set-car',[cdr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cadr).

/*

### Compiled Function: `SYS::%SET-CADR` 
*/
f_sys_pf_set_cadr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cadr, FnResult),
	      true).
:- set_opv(sys_pf_set_cadr, symbol_function, f_sys_pf_set_cadr),
   DefunResult=sys_pf_set_cadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  lambda_def(defun,
				     sys_pf_set_cadr,
				     f_sys_pf_set_cadr,
				     [sys_x, sys_v],
				     [[sys_set_car, [cdr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  arglist_info(sys_pf_set_cadr,
				       f_sys_pf_set_cadr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadr, init_args(x, f_sys_pf_set_cadr))).
*/
/*
(defun %set-cdar (x v) (set-cdr (car x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12332 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdar',[x,v],['set-cdr',[car,x],v]])
wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdar).

/*

### Compiled Function: `SYS::%SET-CDAR` 
*/
f_sys_pf_set_cdar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_car(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdar, FnResult),
	      true).
:- set_opv(sys_pf_set_cdar, symbol_function, f_sys_pf_set_cdar),
   DefunResult=sys_pf_set_cdar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  lambda_def(defun,
				     sys_pf_set_cdar,
				     f_sys_pf_set_cdar,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [car, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  arglist_info(sys_pf_set_cdar,
				       f_sys_pf_set_cdar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdar, init_args(x, f_sys_pf_set_cdar))).
*/
/*
(defun %set-cddr (x v) (set-cdr (cdr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12377 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddr',[x,v],['set-cdr',[cdr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cddr).

/*

### Compiled Function: `SYS::%SET-CDDR` 
*/
f_sys_pf_set_cddr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdr(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cddr, FnResult),
	      true).
:- set_opv(sys_pf_set_cddr, symbol_function, f_sys_pf_set_cddr),
   DefunResult=sys_pf_set_cddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  lambda_def(defun,
				     sys_pf_set_cddr,
				     f_sys_pf_set_cddr,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cdr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  arglist_info(sys_pf_set_cddr,
				       f_sys_pf_set_cddr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddr, init_args(x, f_sys_pf_set_cddr))).
*/
/*
(defun %set-caaar (x v) (set-car (caar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12422 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaar',[x,v],['set-car',[caar,x],v]])
wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caaar).

/*

### Compiled Function: `SYS::%SET-CAAAR` 
*/
f_sys_pf_set_caaar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caar(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caaar, FnResult),
	      true).
:- set_opv(sys_pf_set_caaar, symbol_function, f_sys_pf_set_caaar),
   DefunResult=sys_pf_set_caaar.
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  lambda_def(defun,
				     sys_pf_set_caaar,
				     f_sys_pf_set_caaar,
				     [sys_x, sys_v],
				     [[sys_set_car, [caar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  arglist_info(sys_pf_set_caaar,
				       f_sys_pf_set_caaar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaar, init_args(x, f_sys_pf_set_caaar))).
*/
/*
(defun %set-cadar (x v) (set-car (cdar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12469 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadar',[x,v],['set-car',[cdar,x],v]])
wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cadar).

/*

### Compiled Function: `SYS::%SET-CADAR` 
*/
f_sys_pf_set_cadar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdar(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cadar, FnResult),
	      true).
:- set_opv(sys_pf_set_cadar, symbol_function, f_sys_pf_set_cadar),
   DefunResult=sys_pf_set_cadar.
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  lambda_def(defun,
				     sys_pf_set_cadar,
				     f_sys_pf_set_cadar,
				     [sys_x, sys_v],
				     [[sys_set_car, [cdar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  arglist_info(sys_pf_set_cadar,
				       f_sys_pf_set_cadar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadar, init_args(x, f_sys_pf_set_cadar))).
*/
/*
(defun %set-cdaar (x v) (set-cdr (caar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12516 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaar',[x,v],['set-cdr',[caar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdaar).

/*

### Compiled Function: `SYS::%SET-CDAAR` 
*/
f_sys_pf_set_cdaar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caar(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdaar, FnResult),
	      true).
:- set_opv(sys_pf_set_cdaar, symbol_function, f_sys_pf_set_cdaar),
   DefunResult=sys_pf_set_cdaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  lambda_def(defun,
				     sys_pf_set_cdaar,
				     f_sys_pf_set_cdaar,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [caar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  arglist_info(sys_pf_set_cdaar,
				       f_sys_pf_set_cdaar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaar, init_args(x, f_sys_pf_set_cdaar))).
*/
/*
(defun %set-cddar (x v) (set-cdr (cdar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12563 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddar',[x,v],['set-cdr',[cdar,x],v]])
wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cddar).

/*

### Compiled Function: `SYS::%SET-CDDAR` 
*/
f_sys_pf_set_cddar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdar(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cddar, FnResult),
	      true).
:- set_opv(sys_pf_set_cddar, symbol_function, f_sys_pf_set_cddar),
   DefunResult=sys_pf_set_cddar.
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  lambda_def(defun,
				     sys_pf_set_cddar,
				     f_sys_pf_set_cddar,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cdar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  arglist_info(sys_pf_set_cddar,
				       f_sys_pf_set_cddar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddar, init_args(x, f_sys_pf_set_cddar))).
*/
/*
(defun %set-caadr (x v) (set-car (cadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12610 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caadr',[x,v],['set-car',[cadr,x],v]])
wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caadr).

/*

### Compiled Function: `SYS::%SET-CAADR` 
*/
f_sys_pf_set_caadr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cadr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caadr, FnResult),
	      true).
:- set_opv(sys_pf_set_caadr, symbol_function, f_sys_pf_set_caadr),
   DefunResult=sys_pf_set_caadr.
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  lambda_def(defun,
				     sys_pf_set_caadr,
				     f_sys_pf_set_caadr,
				     [sys_x, sys_v],
				     [[sys_set_car, [cadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  arglist_info(sys_pf_set_caadr,
				       f_sys_pf_set_caadr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadr, init_args(x, f_sys_pf_set_caadr))).
*/
/*
(defun %set-caddr (x v) (set-car (cddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12657 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caddr',[x,v],['set-car',[cddr,x],v]])
wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caddr).

/*

### Compiled Function: `SYS::%SET-CADDR` 
*/
f_sys_pf_set_caddr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caddr, FnResult),
	      true).
:- set_opv(sys_pf_set_caddr, symbol_function, f_sys_pf_set_caddr),
   DefunResult=sys_pf_set_caddr.
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  lambda_def(defun,
				     sys_pf_set_caddr,
				     f_sys_pf_set_caddr,
				     [sys_x, sys_v],
				     [[sys_set_car, [cddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  arglist_info(sys_pf_set_caddr,
				       f_sys_pf_set_caddr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddr, init_args(x, f_sys_pf_set_caddr))).
*/
/*
(defun %set-cdadr (x v) (set-cdr (cadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12704 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdadr',[x,v],['set-cdr',[cadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdadr).

/*

### Compiled Function: `SYS::%SET-CDADR` 
*/
f_sys_pf_set_cdadr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cadr(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdadr, FnResult),
	      true).
:- set_opv(sys_pf_set_cdadr, symbol_function, f_sys_pf_set_cdadr),
   DefunResult=sys_pf_set_cdadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  lambda_def(defun,
				     sys_pf_set_cdadr,
				     f_sys_pf_set_cdadr,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  arglist_info(sys_pf_set_cdadr,
				       f_sys_pf_set_cdadr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadr, init_args(x, f_sys_pf_set_cdadr))).
*/
/*
(defun %set-cdddr (x v) (set-cdr (cddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12751 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdddr',[x,v],['set-cdr',[cddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdddr).

/*

### Compiled Function: `SYS::%SET-CDDDR` 
*/
f_sys_pf_set_cdddr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddr(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdddr, FnResult),
	      true).
:- set_opv(sys_pf_set_cdddr, symbol_function, f_sys_pf_set_cdddr),
   DefunResult=sys_pf_set_cdddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  lambda_def(defun,
				     sys_pf_set_cdddr,
				     f_sys_pf_set_cdddr,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  arglist_info(sys_pf_set_cdddr,
				       f_sys_pf_set_cdddr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddr, init_args(x, f_sys_pf_set_cdddr))).
*/
/*
(defun %set-caaaar (x v) (set-car (caaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12798 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaaar',[x,v],['set-car',[caaar,x],v]])
wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caaaar).

/*

### Compiled Function: `SYS::%SET-CAAAAR` 
*/
f_sys_pf_set_caaaar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caaar(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caaaar, FnResult),
	      true).
:- set_opv(sys_pf_set_caaaar, symbol_function, f_sys_pf_set_caaaar),
   DefunResult=sys_pf_set_caaaar.
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  lambda_def(defun,
				     sys_pf_set_caaaar,
				     f_sys_pf_set_caaaar,
				     [sys_x, sys_v],
				     [[sys_set_car, [caaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  arglist_info(sys_pf_set_caaaar,
				       f_sys_pf_set_caaaar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaaar, init_args(x, f_sys_pf_set_caaaar))).
*/
/*
(defun %set-cadaar (x v) (set-car (cdaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12847 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadaar',[x,v],['set-car',[cdaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cadaar).

/*

### Compiled Function: `SYS::%SET-CADAAR` 
*/
f_sys_pf_set_cadaar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdaar(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cadaar, FnResult),
	      true).
:- set_opv(sys_pf_set_cadaar, symbol_function, f_sys_pf_set_cadaar),
   DefunResult=sys_pf_set_cadaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  lambda_def(defun,
				     sys_pf_set_cadaar,
				     f_sys_pf_set_cadaar,
				     [sys_x, sys_v],
				     [[sys_set_car, [cdaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  arglist_info(sys_pf_set_cadaar,
				       f_sys_pf_set_cadaar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadaar, init_args(x, f_sys_pf_set_cadaar))).
*/
/*
(defun %set-cdaaar (x v) (set-cdr (caaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12896 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaaar',[x,v],['set-cdr',[caaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdaaar).

/*

### Compiled Function: `SYS::%SET-CDAAAR` 
*/
f_sys_pf_set_cdaaar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caaar(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdaaar, FnResult),
	      true).
:- set_opv(sys_pf_set_cdaaar, symbol_function, f_sys_pf_set_cdaaar),
   DefunResult=sys_pf_set_cdaaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  lambda_def(defun,
				     sys_pf_set_cdaaar,
				     f_sys_pf_set_cdaaar,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [caaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  arglist_info(sys_pf_set_cdaaar,
				       f_sys_pf_set_cdaaar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaaar, init_args(x, f_sys_pf_set_cdaaar))).
*/
/*
(defun %set-cddaar (x v) (set-cdr (cdaar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12945 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddaar',[x,v],['set-cdr',[cdaar,x],v]])
wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cddaar).

/*

### Compiled Function: `SYS::%SET-CDDAAR` 
*/
f_sys_pf_set_cddaar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdaar(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cddaar, FnResult),
	      true).
:- set_opv(sys_pf_set_cddaar, symbol_function, f_sys_pf_set_cddaar),
   DefunResult=sys_pf_set_cddaar.
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  lambda_def(defun,
				     sys_pf_set_cddaar,
				     f_sys_pf_set_cddaar,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cdaar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  arglist_info(sys_pf_set_cddaar,
				       f_sys_pf_set_cddaar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddaar, init_args(x, f_sys_pf_set_cddaar))).
*/
/*
(defun %set-caadar (x v) (set-car (cadar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:12994 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caadar',[x,v],['set-car',[cadar,x],v]])
wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caadar).

/*

### Compiled Function: `SYS::%SET-CAADAR` 
*/
f_sys_pf_set_caadar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cadar(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caadar, FnResult),
	      true).
:- set_opv(sys_pf_set_caadar, symbol_function, f_sys_pf_set_caadar),
   DefunResult=sys_pf_set_caadar.
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  lambda_def(defun,
				     sys_pf_set_caadar,
				     f_sys_pf_set_caadar,
				     [sys_x, sys_v],
				     [[sys_set_car, [cadar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  arglist_info(sys_pf_set_caadar,
				       f_sys_pf_set_caadar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caadar, init_args(x, f_sys_pf_set_caadar))).
*/
/*
(defun %set-caddar (x v) (set-car (cddar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13043 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caddar',[x,v],['set-car',[cddar,x],v]])
wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caddar).

/*

### Compiled Function: `SYS::%SET-CADDAR` 
*/
f_sys_pf_set_caddar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddar(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caddar, FnResult),
	      true).
:- set_opv(sys_pf_set_caddar, symbol_function, f_sys_pf_set_caddar),
   DefunResult=sys_pf_set_caddar.
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  lambda_def(defun,
				     sys_pf_set_caddar,
				     f_sys_pf_set_caddar,
				     [sys_x, sys_v],
				     [[sys_set_car, [cddar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  arglist_info(sys_pf_set_caddar,
				       f_sys_pf_set_caddar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caddar, init_args(x, f_sys_pf_set_caddar))).
*/
/*
(defun %set-cdadar (x v) (set-cdr (cadar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13092 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdadar',[x,v],['set-cdr',[cadar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdadar).

/*

### Compiled Function: `SYS::%SET-CDADAR` 
*/
f_sys_pf_set_cdadar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cadar(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdadar, FnResult),
	      true).
:- set_opv(sys_pf_set_cdadar, symbol_function, f_sys_pf_set_cdadar),
   DefunResult=sys_pf_set_cdadar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  lambda_def(defun,
				     sys_pf_set_cdadar,
				     f_sys_pf_set_cdadar,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cadar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  arglist_info(sys_pf_set_cdadar,
				       f_sys_pf_set_cdadar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdadar, init_args(x, f_sys_pf_set_cdadar))).
*/
/*
(defun %set-cdddar (x v) (set-cdr (cddar x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13141 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdddar',[x,v],['set-cdr',[cddar,x],v]])
wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdddar).

/*

### Compiled Function: `SYS::%SET-CDDDAR` 
*/
f_sys_pf_set_cdddar(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddar(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdddar, FnResult),
	      true).
:- set_opv(sys_pf_set_cdddar, symbol_function, f_sys_pf_set_cdddar),
   DefunResult=sys_pf_set_cdddar.
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  lambda_def(defun,
				     sys_pf_set_cdddar,
				     f_sys_pf_set_cdddar,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cddar, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  arglist_info(sys_pf_set_cdddar,
				       f_sys_pf_set_cdddar,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdddar, init_args(x, f_sys_pf_set_cdddar))).
*/
/*
(defun %set-caaadr (x v) (set-car (caadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13190 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaadr',[x,v],['set-car',[caadr,x],v]])
wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caaadr).

/*

### Compiled Function: `SYS::%SET-CAAADR` 
*/
f_sys_pf_set_caaadr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caadr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caaadr, FnResult),
	      true).
:- set_opv(sys_pf_set_caaadr, symbol_function, f_sys_pf_set_caaadr),
   DefunResult=sys_pf_set_caaadr.
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  lambda_def(defun,
				     sys_pf_set_caaadr,
				     f_sys_pf_set_caaadr,
				     [sys_x, sys_v],
				     [[sys_set_car, [caadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  arglist_info(sys_pf_set_caaadr,
				       f_sys_pf_set_caaadr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaadr, init_args(x, f_sys_pf_set_caaadr))).
*/
/*
(defun %set-cadadr (x v) (set-car (cdadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13239 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadadr',[x,v],['set-car',[cdadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cadadr).

/*

### Compiled Function: `SYS::%SET-CADADR` 
*/
f_sys_pf_set_cadadr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdadr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cadadr, FnResult),
	      true).
:- set_opv(sys_pf_set_cadadr, symbol_function, f_sys_pf_set_cadadr),
   DefunResult=sys_pf_set_cadadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  lambda_def(defun,
				     sys_pf_set_cadadr,
				     f_sys_pf_set_cadadr,
				     [sys_x, sys_v],
				     [[sys_set_car, [cdadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  arglist_info(sys_pf_set_cadadr,
				       f_sys_pf_set_cadadr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadadr, init_args(x, f_sys_pf_set_cadadr))).
*/
/*
(defun %set-cdaadr (x v) (set-cdr (caadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13288 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaadr',[x,v],['set-cdr',[caadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdaadr).

/*

### Compiled Function: `SYS::%SET-CDAADR` 
*/
f_sys_pf_set_cdaadr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caadr(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdaadr, FnResult),
	      true).
:- set_opv(sys_pf_set_cdaadr, symbol_function, f_sys_pf_set_cdaadr),
   DefunResult=sys_pf_set_cdaadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  lambda_def(defun,
				     sys_pf_set_cdaadr,
				     f_sys_pf_set_cdaadr,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [caadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  arglist_info(sys_pf_set_cdaadr,
				       f_sys_pf_set_cdaadr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaadr, init_args(x, f_sys_pf_set_cdaadr))).
*/
/*
(defun %set-cddadr (x v) (set-cdr (cdadr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13337 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddadr',[x,v],['set-cdr',[cdadr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cddadr).

/*

### Compiled Function: `SYS::%SET-CDDADR` 
*/
f_sys_pf_set_cddadr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdadr(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cddadr, FnResult),
	      true).
:- set_opv(sys_pf_set_cddadr, symbol_function, f_sys_pf_set_cddadr),
   DefunResult=sys_pf_set_cddadr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  lambda_def(defun,
				     sys_pf_set_cddadr,
				     f_sys_pf_set_cddadr,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cdadr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  arglist_info(sys_pf_set_cddadr,
				       f_sys_pf_set_cddadr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddadr, init_args(x, f_sys_pf_set_cddadr))).
*/
/*
(defun %set-caaddr (x v) (set-car (caddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13386 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-caaddr',[x,v],['set-car',[caddr,x],v]])
wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_caaddr).

/*

### Compiled Function: `SYS::%SET-CAADDR` 
*/
f_sys_pf_set_caaddr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caddr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_caaddr, FnResult),
	      true).
:- set_opv(sys_pf_set_caaddr, symbol_function, f_sys_pf_set_caaddr),
   DefunResult=sys_pf_set_caaddr.
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  lambda_def(defun,
				     sys_pf_set_caaddr,
				     f_sys_pf_set_caaddr,
				     [sys_x, sys_v],
				     [[sys_set_car, [caddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  arglist_info(sys_pf_set_caaddr,
				       f_sys_pf_set_caaddr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_caaddr, init_args(x, f_sys_pf_set_caaddr))).
*/
/*
(defun %set-cadddr (x v) (set-car (cdddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13435 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cadddr',[x,v],['set-car',[cdddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cadddr).

/*

### Compiled Function: `SYS::%SET-CADDDR` 
*/
f_sys_pf_set_cadddr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdddr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cadddr, FnResult),
	      true).
:- set_opv(sys_pf_set_cadddr, symbol_function, f_sys_pf_set_cadddr),
   DefunResult=sys_pf_set_cadddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  lambda_def(defun,
				     sys_pf_set_cadddr,
				     f_sys_pf_set_cadddr,
				     [sys_x, sys_v],
				     [[sys_set_car, [cdddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  arglist_info(sys_pf_set_cadddr,
				       f_sys_pf_set_cadddr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cadddr, init_args(x, f_sys_pf_set_cadddr))).
*/
/*
(defun %set-cdaddr (x v) (set-cdr (caddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13484 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cdaddr',[x,v],['set-cdr',[caddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cdaddr).

/*

### Compiled Function: `SYS::%SET-CDADDR` 
*/
f_sys_pf_set_cdaddr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_caddr(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cdaddr, FnResult),
	      true).
:- set_opv(sys_pf_set_cdaddr, symbol_function, f_sys_pf_set_cdaddr),
   DefunResult=sys_pf_set_cdaddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  lambda_def(defun,
				     sys_pf_set_cdaddr,
				     f_sys_pf_set_cdaddr,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [caddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  arglist_info(sys_pf_set_cdaddr,
				       f_sys_pf_set_cdaddr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cdaddr, init_args(x, f_sys_pf_set_cdaddr))).
*/
/*
(defun %set-cddddr (x v) (set-cdr (cdddr x) v))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13533 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-cddddr',[x,v],['set-cdr',[cdddr,x],v]])
wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_cddddr).

/*

### Compiled Function: `SYS::%SET-CDDDDR` 
*/
f_sys_pf_set_cddddr(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cdddr(X_Get, Set_cdr_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_cdr(Set_cdr_Param, V_Get, Set_cdr_Ret)
		),
		Set_cdr_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_cddddr, FnResult),
	      true).
:- set_opv(sys_pf_set_cddddr, symbol_function, f_sys_pf_set_cddddr),
   DefunResult=sys_pf_set_cddddr.
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  lambda_def(defun,
				     sys_pf_set_cddddr,
				     f_sys_pf_set_cddddr,
				     [sys_x, sys_v],
				     [[sys_set_cdr, [cdddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  arglist_info(sys_pf_set_cddddr,
				       f_sys_pf_set_cddddr,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_cddddr, init_args(x, f_sys_pf_set_cddddr))).
*/
/*
(defsetf car set-car)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13584 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,car,'set-car'])
/*
% macroexpand:-[defsetf,car,sys_set_car].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,car],[quote,sys_setf_inverse],[quote,sys_set_car]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(car, sys_setf_inverse, sys_set_car, [], _Ignored),
	   _Ignored).
/*
(defsetf cdr set-cdr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13607 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdr,'set-cdr'])
/*
% macroexpand:-[defsetf,cdr,sys_set_cdr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdr],[quote,sys_setf_inverse],[quote,sys_set_cdr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdr, sys_setf_inverse, sys_set_cdr, [], _Ignored),
	   _Ignored).
/*
(defsetf caar %set-caar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13630 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caar,'%set-caar'])
/*
% macroexpand:-[defsetf,caar,sys_pf_set_caar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caar],[quote,sys_setf_inverse],[quote,sys_pf_set_caar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caar,
			     sys_setf_inverse,
			     sys_pf_set_caar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cadr %set-cadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13656 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadr,'%set-cadr'])
/*
% macroexpand:-[defsetf,cadr,sys_pf_set_cadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cadr,
			     sys_setf_inverse,
			     sys_pf_set_cadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdar %set-cdar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13682 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdar,'%set-cdar'])
/*
% macroexpand:-[defsetf,cdar,sys_pf_set_cdar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdar,
			     sys_setf_inverse,
			     sys_pf_set_cdar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cddr %set-cddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13708 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddr,'%set-cddr'])
/*
% macroexpand:-[defsetf,cddr,sys_pf_set_cddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cddr,
			     sys_setf_inverse,
			     sys_pf_set_cddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caaar %set-caaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13734 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaar,'%set-caaar'])
/*
% macroexpand:-[defsetf,caaar,sys_pf_set_caaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caaar],[quote,sys_setf_inverse],[quote,sys_pf_set_caaar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caaar,
			     sys_setf_inverse,
			     sys_pf_set_caaar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cadar %set-cadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13762 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadar,'%set-cadar'])
/*
% macroexpand:-[defsetf,cadar,sys_pf_set_cadar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cadar],[quote,sys_setf_inverse],[quote,sys_pf_set_cadar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cadar,
			     sys_setf_inverse,
			     sys_pf_set_cadar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdaar %set-cdaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13790 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaar,'%set-cdaar'])
/*
% macroexpand:-[defsetf,cdaar,sys_pf_set_cdaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdaar,
			     sys_setf_inverse,
			     sys_pf_set_cdaar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cddar %set-cddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13818 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddar,'%set-cddar'])
/*
% macroexpand:-[defsetf,cddar,sys_pf_set_cddar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cddar],[quote,sys_setf_inverse],[quote,sys_pf_set_cddar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cddar,
			     sys_setf_inverse,
			     sys_pf_set_cddar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caadr %set-caadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13846 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caadr,'%set-caadr'])
/*
% macroexpand:-[defsetf,caadr,sys_pf_set_caadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caadr],[quote,sys_setf_inverse],[quote,sys_pf_set_caadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caadr,
			     sys_setf_inverse,
			     sys_pf_set_caadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caddr %set-caddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13874 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caddr,'%set-caddr'])
/*
% macroexpand:-[defsetf,caddr,sys_pf_set_caddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caddr],[quote,sys_setf_inverse],[quote,sys_pf_set_caddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caddr,
			     sys_setf_inverse,
			     sys_pf_set_caddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdadr %set-cdadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13902 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdadr,'%set-cdadr'])
/*
% macroexpand:-[defsetf,cdadr,sys_pf_set_cdadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdadr,
			     sys_setf_inverse,
			     sys_pf_set_cdadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdddr %set-cdddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13930 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdddr,'%set-cdddr'])
/*
% macroexpand:-[defsetf,cdddr,sys_pf_set_cdddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdddr,
			     sys_setf_inverse,
			     sys_pf_set_cdddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caaaar %set-caaaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13958 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaaar,'%set-caaaar'])
/*
% macroexpand:-[defsetf,caaaar,sys_pf_set_caaaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caaaar],[quote,sys_setf_inverse],[quote,sys_pf_set_caaaar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caaaar,
			     sys_setf_inverse,
			     sys_pf_set_caaaar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cadaar %set-cadaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:13988 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadaar,'%set-cadaar'])
/*
% macroexpand:-[defsetf,cadaar,sys_pf_set_cadaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cadaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cadaar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cadaar,
			     sys_setf_inverse,
			     sys_pf_set_cadaar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdaaar %set-cdaaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14018 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaaar,'%set-cdaaar'])
/*
% macroexpand:-[defsetf,cdaaar,sys_pf_set_cdaaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdaaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaaar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdaaar,
			     sys_setf_inverse,
			     sys_pf_set_cdaaar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cddaar %set-cddaar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14048 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddaar,'%set-cddaar'])
/*
% macroexpand:-[defsetf,cddaar,sys_pf_set_cddaar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cddaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cddaar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cddaar,
			     sys_setf_inverse,
			     sys_pf_set_cddaar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caadar %set-caadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14078 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caadar,'%set-caadar'])
/*
% macroexpand:-[defsetf,caadar,sys_pf_set_caadar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caadar],[quote,sys_setf_inverse],[quote,sys_pf_set_caadar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caadar,
			     sys_setf_inverse,
			     sys_pf_set_caadar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caddar %set-caddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14108 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caddar,'%set-caddar'])
/*
% macroexpand:-[defsetf,caddar,sys_pf_set_caddar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caddar],[quote,sys_setf_inverse],[quote,sys_pf_set_caddar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caddar,
			     sys_setf_inverse,
			     sys_pf_set_caddar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdadar %set-cdadar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14138 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdadar,'%set-cdadar'])
/*
% macroexpand:-[defsetf,cdadar,sys_pf_set_cdadar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdadar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdadar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdadar,
			     sys_setf_inverse,
			     sys_pf_set_cdadar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdddar %set-cdddar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14168 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdddar,'%set-cdddar'])
/*
% macroexpand:-[defsetf,cdddar,sys_pf_set_cdddar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdddar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdddar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdddar,
			     sys_setf_inverse,
			     sys_pf_set_cdddar,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caaadr %set-caaadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14198 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaadr,'%set-caaadr'])
/*
% macroexpand:-[defsetf,caaadr,sys_pf_set_caaadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caaadr],[quote,sys_setf_inverse],[quote,sys_pf_set_caaadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caaadr,
			     sys_setf_inverse,
			     sys_pf_set_caaadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cadadr %set-cadadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14228 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadadr,'%set-cadadr'])
/*
% macroexpand:-[defsetf,cadadr,sys_pf_set_cadadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cadadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cadadr,
			     sys_setf_inverse,
			     sys_pf_set_cadadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdaadr %set-cdaadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14258 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaadr,'%set-cdaadr'])
/*
% macroexpand:-[defsetf,cdaadr,sys_pf_set_cdaadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdaadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdaadr,
			     sys_setf_inverse,
			     sys_pf_set_cdaadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cddadr %set-cddadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14288 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddadr,'%set-cddadr'])
/*
% macroexpand:-[defsetf,cddadr,sys_pf_set_cddadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cddadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cddadr,
			     sys_setf_inverse,
			     sys_pf_set_cddadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf caaddr %set-caaddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14318 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,caaddr,'%set-caaddr'])
/*
% macroexpand:-[defsetf,caaddr,sys_pf_set_caaddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,caaddr],[quote,sys_setf_inverse],[quote,sys_pf_set_caaddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(caaddr,
			     sys_setf_inverse,
			     sys_pf_set_caaddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cadddr %set-cadddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14348 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cadddr,'%set-cadddr'])
/*
% macroexpand:-[defsetf,cadddr,sys_pf_set_cadddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cadddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cadddr,
			     sys_setf_inverse,
			     sys_pf_set_cadddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cdaddr %set-cdaddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14378 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cdaddr,'%set-cdaddr'])
/*
% macroexpand:-[defsetf,cdaddr,sys_pf_set_cdaddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cdaddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cdaddr,
			     sys_setf_inverse,
			     sys_pf_set_cdaddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf cddddr %set-cddddr)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14408 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,cddddr,'%set-cddddr'])
/*
% macroexpand:-[defsetf,cddddr,sys_pf_set_cddddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,cddddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(cddddr,
			     sys_setf_inverse,
			     sys_pf_set_cddddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf first set-car)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14440 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,first,'set-car'])
/*
% macroexpand:-[defsetf,first,sys_set_car].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,first],[quote,sys_setf_inverse],[quote,sys_set_car]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(first, sys_setf_inverse, sys_set_car, [], _Ignored),
	   _Ignored).
/*
(defsetf second %set-cadr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14465 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,second,'%set-cadr'])
/*
% macroexpand:-[defsetf,second,sys_pf_set_cadr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,second],[quote,sys_setf_inverse],[quote,sys_pf_set_cadr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(second,
			     sys_setf_inverse,
			     sys_pf_set_cadr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf third %set-caddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14493 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,third,'%set-caddr'])
/*
% macroexpand:-[defsetf,third,sys_pf_set_caddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,third],[quote,sys_setf_inverse],[quote,sys_pf_set_caddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(third,
			     sys_setf_inverse,
			     sys_pf_set_caddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf fourth %set-cadddr)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14521 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,fourth,'%set-cadddr'])
/*
% macroexpand:-[defsetf,fourth,sys_pf_set_cadddr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,fourth],[quote,sys_setf_inverse],[quote,sys_pf_set_cadddr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(fourth,
			     sys_setf_inverse,
			     sys_pf_set_cadddr,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defun %set-fifth (x v) (set-car (cddddr x) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14551 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-fifth',[x,v],['set-car',[cddddr,x],v]])
wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_fifth).

/*

### Compiled Function: `SYS::%SET-FIFTH` 
*/
f_sys_pf_set_fifth(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddddr(X_Get, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_fifth, FnResult),
	      true).
:- set_opv(sys_pf_set_fifth, symbol_function, f_sys_pf_set_fifth),
   DefunResult=sys_pf_set_fifth.
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  lambda_def(defun,
				     sys_pf_set_fifth,
				     f_sys_pf_set_fifth,
				     [sys_x, sys_v],
				     [[sys_set_car, [cddddr, sys_x], sys_v]]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  arglist_info(sys_pf_set_fifth,
				       f_sys_pf_set_fifth,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_fifth, init_args(x, f_sys_pf_set_fifth))).
*/
/*
(defsetf fifth %set-fifth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14600 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,fifth,'%set-fifth'])
/*
% macroexpand:-[defsetf,fifth,sys_pf_set_fifth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,fifth],[quote,sys_setf_inverse],[quote,sys_pf_set_fifth]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(fifth,
			     sys_setf_inverse,
			     sys_pf_set_fifth,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defun %set-sixth (x v) (set-car (cdr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14628 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-sixth',[x,v],['set-car',[cdr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_sixth).

/*

### Compiled Function: `SYS::%SET-SIXTH` 
*/
f_sys_pf_set_sixth(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddddr(X_Get, Cdr_Param),
		  f_cdr(Cdr_Param, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_sixth, FnResult),
	      true).
:- set_opv(sys_pf_set_sixth, symbol_function, f_sys_pf_set_sixth),
   DefunResult=sys_pf_set_sixth.
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  lambda_def(defun,
				     sys_pf_set_sixth,
				     f_sys_pf_set_sixth,
				     [sys_x, sys_v],
				     
				     [ 
				       [ sys_set_car,
					 [cdr, [cddddr, sys_x]],
					 sys_v
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  arglist_info(sys_pf_set_sixth,
				       f_sys_pf_set_sixth,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_sixth, init_args(x, f_sys_pf_set_sixth))).
*/
/*
(defsetf sixth %set-sixth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14683 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,sixth,'%set-sixth'])
/*
% macroexpand:-[defsetf,sixth,sys_pf_set_sixth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,sixth],[quote,sys_setf_inverse],[quote,sys_pf_set_sixth]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(sixth,
			     sys_setf_inverse,
			     sys_pf_set_sixth,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defun %set-seventh (x v) (set-car (cddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14711 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-seventh',[x,v],['set-car',[cddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_seventh).

/*

### Compiled Function: `SYS::%SET-SEVENTH` 
*/
f_sys_pf_set_seventh(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddddr(X_Get, Cddr_Param),
		  f_cddr(Cddr_Param, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_seventh, FnResult),
	      true).
:- set_opv(sys_pf_set_seventh, symbol_function, f_sys_pf_set_seventh),
   DefunResult=sys_pf_set_seventh.
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  lambda_def(defun,
				     sys_pf_set_seventh,
				     f_sys_pf_set_seventh,
				     [sys_x, sys_v],
				     
				     [ 
				       [ sys_set_car,
					 [cddr, [cddddr, sys_x]],
					 sys_v
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  arglist_info(sys_pf_set_seventh,
				       f_sys_pf_set_seventh,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  init_args(x, f_sys_pf_set_seventh))).
*/
/*
(defsetf seventh %set-seventh)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14769 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,seventh,'%set-seventh'])
/*
% macroexpand:-[defsetf,seventh,sys_pf_set_seventh].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,seventh],[quote,sys_setf_inverse],[quote,sys_pf_set_seventh]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(seventh,
			     sys_setf_inverse,
			     sys_pf_set_seventh,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defun %set-eighth (x v) (set-car (cdddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14801 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-eighth',[x,v],['set-car',[cdddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_eighth).

/*

### Compiled Function: `SYS::%SET-EIGHTH` 
*/
f_sys_pf_set_eighth(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddddr(X_Get, Cdddr_Param),
		  f_cdddr(Cdddr_Param, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_eighth, FnResult),
	      true).
:- set_opv(sys_pf_set_eighth, symbol_function, f_sys_pf_set_eighth),
   DefunResult=sys_pf_set_eighth.
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  lambda_def(defun,
				     sys_pf_set_eighth,
				     f_sys_pf_set_eighth,
				     [sys_x, sys_v],
				     
				     [ 
				       [ sys_set_car,
					 [cdddr, [cddddr, sys_x]],
					 sys_v
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  arglist_info(sys_pf_set_eighth,
				       f_sys_pf_set_eighth,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_eighth, init_args(x, f_sys_pf_set_eighth))).
*/
/*
(defsetf eighth %set-eighth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14859 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,eighth,'%set-eighth'])
/*
% macroexpand:-[defsetf,eighth,sys_pf_set_eighth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,eighth],[quote,sys_setf_inverse],[quote,sys_pf_set_eighth]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(eighth,
			     sys_setf_inverse,
			     sys_pf_set_eighth,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defun %set-ninth (x v) (set-car (cddddr (cddddr x)) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14889 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-ninth',[x,v],['set-car',[cddddr,[cddddr,x]],v]])
wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_ninth).

/*

### Compiled Function: `SYS::%SET-NINTH` 
*/
f_sys_pf_set_ninth(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddddr(X_Get, Cddddr_Param),
		  f_cddddr(Cddddr_Param, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_ninth, FnResult),
	      true).
:- set_opv(sys_pf_set_ninth, symbol_function, f_sys_pf_set_ninth),
   DefunResult=sys_pf_set_ninth.
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  lambda_def(defun,
				     sys_pf_set_ninth,
				     f_sys_pf_set_ninth,
				     [sys_x, sys_v],
				     
				     [ 
				       [ sys_set_car,
					 [cddddr, [cddddr, sys_x]],
					 sys_v
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  arglist_info(sys_pf_set_ninth,
				       f_sys_pf_set_ninth,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_ninth, init_args(x, f_sys_pf_set_ninth))).
*/
/*
(defsetf ninth %set-ninth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14947 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,ninth,'%set-ninth'])
/*
% macroexpand:-[defsetf,ninth,sys_pf_set_ninth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,ninth],[quote,sys_setf_inverse],[quote,sys_pf_set_ninth]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(ninth,
			     sys_setf_inverse,
			     sys_pf_set_ninth,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defun %set-tenth (x v) (set-car (cdr (cddddr (cddddr x))) v))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:14975 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'%set-tenth',[x,v],['set-car',[cdr,[cddddr,[cddddr,x]]],v]])
wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]).
wl:arglist_info(sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_pf_set_tenth).

/*

### Compiled Function: `SYS::%SET-TENTH` 
*/
f_sys_pf_set_tenth(X_In, V_In, FnResult) :-
	GEnv=[bv(sys_x, X_In), bv(sys_v, V_In)],
	catch(( ( get_var(GEnv, sys_x, X_Get),
		  f_cddddr(X_Get, Cddddr_Param),
		  f_cddddr(Cddddr_Param, Cdr_Param),
		  f_cdr(Cdr_Param, Set_car_Param),
		  get_var(GEnv, sys_v, V_Get),
		  f_sys_set_car(Set_car_Param, V_Get, Set_car_Ret)
		),
		Set_car_Ret=FnResult
	      ),
	      block_exit(sys_pf_set_tenth, FnResult),
	      true).
:- set_opv(sys_pf_set_tenth, symbol_function, f_sys_pf_set_tenth),
   DefunResult=sys_pf_set_tenth.
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  lambda_def(defun,
				     sys_pf_set_tenth,
				     f_sys_pf_set_tenth,
				     [sys_x, sys_v],
				     
				     [ 
				       [ sys_set_car,
					 [cdr, [cddddr, [cddddr, sys_x]]],
					 sys_v
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  arglist_info(sys_pf_set_tenth,
				       f_sys_pf_set_tenth,
				       [sys_x, sys_v],
				       arginfo{ all:[sys_x, sys_v],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_x, sys_v],
						opt:0,
						req:[sys_x, sys_v],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_pf_set_tenth, init_args(x, f_sys_pf_set_tenth))).
*/
/*
(defsetf tenth %set-tenth)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15039 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,tenth,'%set-tenth'])
/*
% macroexpand:-[defsetf,tenth,sys_pf_set_tenth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,tenth],[quote,sys_setf_inverse],[quote,sys_pf_set_tenth]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(tenth,
			     sys_setf_inverse,
			     sys_pf_set_tenth,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf rest set-cdr)
;;Redefined in extensible-sequences-base.lisp
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15069 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,rest,'set-cdr'])
/*
% macroexpand:-[defsetf,rest,sys_set_cdr].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,rest],[quote,sys_setf_inverse],[quote,sys_set_cdr]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(rest, sys_setf_inverse, sys_set_cdr, [], _Ignored),
	   _Ignored).
/*
;Redefined in extensible-sequences-base.lisp
*/
/*
(defsetf elt %set-elt)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15140 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,elt,'%set-elt'])
/*
% macroexpand:-[defsetf,elt,sys_pf_set_elt].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,elt],[quote,sys_setf_inverse],[quote,sys_pf_set_elt]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(elt, sys_setf_inverse, sys_pf_set_elt, [], _Ignored),
	   _Ignored).
/*
(defsetf nth %set-nth)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15164 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,nth,'%set-nth'])
/*
% macroexpand:-[defsetf,nth,sys_pf_set_nth].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,nth],[quote,sys_setf_inverse],[quote,sys_pf_set_nth]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(nth, sys_setf_inverse, sys_pf_set_nth, [], _Ignored),
	   _Ignored).
/*
(defsetf svref svset)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15188 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,svref,svset])
/*
% macroexpand:-[defsetf,svref,sys_svset].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,svref],[quote,sys_setf_inverse],[quote,sys_svset]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(svref, sys_setf_inverse, sys_svset, [], _Ignored),
	   _Ignored).
/*
(defsetf fill-pointer %set-fill-pointer)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15211 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'fill-pointer','%set-fill-pointer'])
/*
% macroexpand:-[defsetf,fill_pointer,sys_pf_set_fill_pointer].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,fill_pointer],[quote,sys_setf_inverse],[quote,sys_pf_set_fill_pointer]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(fill_pointer,
			     sys_setf_inverse,
			     sys_pf_set_fill_pointer,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf subseq %set-subseq)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15253 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,subseq,'%set-subseq'])
/*
% macroexpand:-[defsetf,subseq,sys_pf_set_subseq].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,subseq],[quote,sys_setf_inverse],[quote,sys_pf_set_subseq]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(subseq,
			     sys_setf_inverse,
			     sys_pf_set_subseq,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf symbol-value set)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15283 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-value',set])
/*
% macroexpand:-[defsetf,symbol_value,set].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,symbol_value],[quote,sys_setf_inverse],[quote,set]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(symbol_value, sys_setf_inverse, set, [], _Ignored),
	   _Ignored).
/*
(defsetf symbol-function %set-symbol-function)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15311 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-function','%set-symbol-function'])
/*
% macroexpand:-[defsetf,symbol_function,sys_pf_set_symbol_function].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,symbol_function],[quote,sys_setf_inverse],[quote,sys_pf_set_symbol_function]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(symbol_function,
			     sys_setf_inverse,
			     sys_pf_set_symbol_function,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf symbol-plist %set-symbol-plist)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15359 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'symbol-plist','%set-symbol-plist'])
/*
% macroexpand:-[defsetf,symbol_plist,sys_pf_set_symbol_plist].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,symbol_plist],[quote,sys_setf_inverse],[quote,sys_pf_set_symbol_plist]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(symbol_plist,
			     sys_setf_inverse,
			     sys_pf_set_symbol_plist,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf get put)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15401 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,get,put])
/*
% macroexpand:-[defsetf,get,sys_put].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,get],[quote,sys_setf_inverse],[quote,sys_put]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(get, sys_setf_inverse, sys_put, [], _Ignored),
	   _Ignored).
/*
(defsetf gethash puthash)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15420 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,gethash,puthash])
/*
% macroexpand:-[defsetf,gethash,sys_puthash].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,gethash],[quote,sys_setf_inverse],[quote,sys_puthash]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(gethash, sys_setf_inverse, sys_puthash, [], _Ignored),
	   _Ignored).
/*
(defsetf char set-char)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15447 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,char,'set-char'])
/*
% macroexpand:-[defsetf,char,sys_set_char].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,char],[quote,sys_setf_inverse],[quote,sys_set_char]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(char, sys_setf_inverse, sys_set_char, [], _Ignored),
	   _Ignored).
/*
(defsetf schar set-schar)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15472 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,schar,'set-schar'])
/*
% macroexpand:-[defsetf,schar,sys_set_schar].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,schar],[quote,sys_setf_inverse],[quote,sys_set_schar]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(schar, sys_setf_inverse, sys_set_schar, [], _Ignored),
	   _Ignored).
/*
(defsetf logical-pathname-translations %set-logical-pathname-translations)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15499 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'logical-pathname-translations','%set-logical-pathname-translations'])
/*
% macroexpand:-[defsetf,logical_pathname_translations,sys_pf_set_logical_pathname_translations].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,logical_pathname_translations],[quote,sys_setf_inverse],[quote,sys_pf_set_logical_pathname_translations]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(logical_pathname_translations,
			     sys_setf_inverse,
			     sys_pf_set_logical_pathname_translations,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf readtable-case %set-readtable-case)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15575 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'readtable-case','%set-readtable-case'])
/*
% macroexpand:-[defsetf,readtable_case,sys_pf_set_readtable_case].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,readtable_case],[quote,sys_setf_inverse],[quote,sys_pf_set_readtable_case]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(readtable_case,
			     sys_setf_inverse,
			     sys_pf_set_readtable_case,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf function-info %set-function-info)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15623 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'function-info','%set-function-info'])
/*
% macroexpand:-[defsetf,sys_function_info,sys_pf_set_function_info].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,sys_function_info],[quote,sys_setf_inverse],[quote,sys_pf_set_function_info]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(sys_function_info,
			     sys_setf_inverse,
			     sys_pf_set_function_info,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf stream-external-format %set-stream-external-format)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15669 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'stream-external-format','%set-stream-external-format'])
/*
% macroexpand:-[defsetf,stream_external_format,sys_pf_set_stream_external_format].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,stream_external_format],[quote,sys_setf_inverse],[quote,sys_pf_set_stream_external_format]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(stream_external_format,
			     sys_setf_inverse,
			     sys_pf_set_stream_external_format,
			     [],
			     _Ignored),
	   _Ignored).
/*
(defsetf structure-ref structure-set)

;; ) ;; FLET END


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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:15733 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,'structure-ref','structure-set'])
/*
% macroexpand:-[defsetf,sys_structure_ref,sys_structure_set].
*/
/*
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put_sysprop,[quote,sys_structure_ref],[quote,sys_setf_inverse],[quote,sys_structure_set]]].
*/
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put_sysprop(sys_structure_ref,
			     sys_setf_inverse,
			     sys_structure_set,
			     [],
			     _Ignored),
	   _Ignored).
/*
; ) ;; FLET END
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:16455 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','$STRING'("SYSTEM")])
/*
% macroexpand:-[in_package,'$ARRAY'([*],claz_base_character,"SYSTEM")].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
#+(or WAM-CL ECL)
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:16482 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,union,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, the union of elements in LIST1 and in LIST2."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,list2]],[or,first,list2]],[unless,[member1,[car,x],list2,test,'test-not',key],[if,last,[progn,[rplacd,last,[cons,[car,x],[]]],[setq,last,[cdr,last]]],[progn,[setq,first,[cons,[car,x],[]]],[setq,last,first]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
doc: doc_string(union,
	      _9950,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, the union of elements in LIST1 and in LIST2.").

wl:lambda_def(defun, union, f_union, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, sys_list2]], [or, first, sys_list2]], [unless, [sys_member1, [car, sys_x], sys_list2, sys_test, sys_test_not, key], [if, last, [progn, [rplacd, last, [cons, [car, sys_x], []]], [setq, last, [cdr, last]]], [progn, [setq, first, [cons, [car, sys_x], []]], [setq, last, first]]]]]]).
wl:arglist_info(union, f_union, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_union).

/*

### Compiled Function: `CL:UNION` 
*/
f_union(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_21), get_var(AEnv, sys_x, IFTEST58), (IFTEST58==[]->get_var(AEnv, last, IFTEST63), (IFTEST63\==[]->get_var(AEnv, last, Last_Get66), get_var(AEnv, sys_list2, List2_Get67), f_rplacd(Last_Get66, List2_Get67, TrueResult68), _11192=TrueResult68;_11192=[]), (get_var(AEnv, first, First_Get69), First_Get69\==[], RetResult61=First_Get69->true;get_var(AEnv, sys_list2, List2_Get70), RetResult61=List2_Get70), throw(block_exit([], RetResult61)), _TBResult=ThrowResult62;get_var(AEnv, sys_x, X_Get75), f_car(X_Get75, Member1_Param), get_var(AEnv, key, Key_Get79), get_var(AEnv, sys_list2, List2_Get76), get_var(AEnv, sys_test, Test_Get77), get_var(AEnv, sys_test_not, Test_not_Get78), f_sys_member1(Member1_Param, List2_Get76, Test_Get77, Test_not_Get78, Key_Get79, IFTEST73), (IFTEST73\==[]->_11446=[];get_var(AEnv, last, IFTEST80), (IFTEST80\==[]->get_var(AEnv, last, Last_Get83), get_var(AEnv, sys_x, X_Get84), f_car(X_Get84, Car_Ret), _11738=[Car_Ret], f_rplacd(Last_Get83, _11738, Rplacd_Ret), get_var(AEnv, last, Last_Get86), f_cdr(Last_Get86, TrueResult89), set_var(AEnv, last, TrueResult89), ElseResult91=TrueResult89;get_var(AEnv, sys_x, X_Get87), f_car(X_Get87, Car_Ret106), First=[Car_Ret106], set_var(AEnv, first, First), get_var(AEnv, first, First_Get88), set_var(AEnv, last, First_Get88), ElseResult91=First_Get88), _11446=ElseResult91), get_var(AEnv, sys_x, X_Get92), f_cdr(X_Get92, X), set_var(AEnv, sys_x, X), goto(do_label_21, AEnv), _TBResult=_GORES93)),
					  
					  [ addr(addr_tagbody_21_do_label_21,
						 do_label_21,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST23), (IFTEST23\==[]->get_var(AEnv, last, Last_Get26), get_var(AEnv, sys_list2, Get_var_Ret), f_rplacd(Last_Get26, Get_var_Ret, Rplacd_Ret108), _12096=Rplacd_Ret108;_12096=[]), (get_var(AEnv, first, First_Get), First_Get\==[], Block_exit_Ret=First_Get->true;get_var(AEnv, sys_list2, List2_Get30), Block_exit_Ret=List2_Get30), throw(block_exit([], Block_exit_Ret)), _12128=ThrowResult;get_var(AEnv, sys_x, X_Get35), f_car(X_Get35, Member1_Param103), get_var(AEnv, key, Get_var_Ret110), get_var(AEnv, sys_list2, List2_Get36), get_var(AEnv, sys_test, Get_var_Ret111), get_var(AEnv, sys_test_not, Get_var_Ret112), f_sys_member1(Member1_Param103, List2_Get36, Get_var_Ret111, Get_var_Ret112, Get_var_Ret110, IFTEST33), (IFTEST33\==[]->_12194=[];get_var(AEnv, last, IFTEST40), (IFTEST40\==[]->get_var(AEnv, last, Last_Get43), get_var(AEnv, sys_x, X_Get44), f_car(X_Get44, Car_Ret113), _12240=[Car_Ret113], f_rplacd(Last_Get43, _12240, Rplacd_Ret114), get_var(AEnv, last, Last_Get46), f_cdr(Last_Get46, TrueResult49), set_var(AEnv, last, TrueResult49), ElseResult51=TrueResult49;get_var(AEnv, sys_x, X_Get47), f_car(X_Get47, Car_Ret115), Set_var_Ret=[Car_Ret115], set_var(AEnv, first, Set_var_Ret), get_var(AEnv, first, First_Get48), set_var(AEnv, last, First_Get48), ElseResult51=First_Get48), _12194=ElseResult51), get_var(AEnv, sys_x, X_Get52), f_cdr(X_Get52, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_21, AEnv), _12128=_GORES)))
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
:- set_opv(union, symbol_function, f_union),
   DefunResult=union.
/*
:- side_effect(assert_lsp(union,
			  doc_string(union,
				     _9950,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, the union of elements in LIST1 and in LIST2."))).
*/
/*
:- side_effect(assert_lsp(union,
			  lambda_def(defun,
				     union,
				     f_union,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ do,
					 
					 [ [sys_x, sys_list1, [cdr, sys_x]],
					   [first],
					   [last]
					 ],
					 
					 [ [null, sys_x],
					   [when, last, [rplacd, last, sys_list2]],
					   [or, first, sys_list2]
					 ],
					 
					 [ unless,
					   
					   [ sys_member1,
					     [car, sys_x],
					     sys_list2,
					     sys_test,
					     sys_test_not,
					     key
					   ],
					   
					   [ if,
					     last,
					     
					     [ progn,
					       
					       [ rplacd,
						 last,
						 [cons, [car, sys_x], []]
					       ],
					       [setq, last, [cdr, last]]
					     ],
					     
					     [ progn,
					       
					       [ setq,
						 first,
						 [cons, [car, sys_x], []]
					       ],
					       [setq, last, first]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(union,
			  arglist_info(union,
				       f_union,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(union, init_args(2, f_union))).
*/
/*
#+(or WAM-CL ECL) 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:17079 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nunion,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive UNION.  Both LIST1 and LIST2 may be destroyed."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,list2]],[or,first,list2]],[unless,[member1,[car,x],list2,test,'test-not',key],[if,last,[rplacd,last,x],[setq,first,x]],[setq,last,x]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
doc: doc_string(nunion,
	      _9380,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive UNION.  Both LIST1 and LIST2 may be destroyed.").

wl:lambda_def(defun, nunion, f_nunion, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, sys_list2]], [or, first, sys_list2]], [unless, [sys_member1, [car, sys_x], sys_list2, sys_test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]).
wl:arglist_info(nunion, f_nunion, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_nunion).

/*

### Compiled Function: `CL:NUNION` 
*/
f_nunion(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_22), get_var(AEnv, sys_x, IFTEST57), (IFTEST57==[]->get_var(AEnv, last, IFTEST62), (IFTEST62\==[]->get_var(AEnv, last, Last_Get65), get_var(AEnv, sys_list2, List2_Get66), f_rplacd(Last_Get65, List2_Get66, TrueResult67), _10580=TrueResult67;_10580=[]), (get_var(AEnv, first, First_Get68), First_Get68\==[], RetResult60=First_Get68->true;get_var(AEnv, sys_list2, List2_Get69), RetResult60=List2_Get69), throw(block_exit([], RetResult60)), _TBResult=ThrowResult61;get_var(AEnv, sys_x, X_Get74), f_car(X_Get74, Member1_Param), get_var(AEnv, key, Key_Get78), get_var(AEnv, sys_list2, List2_Get75), get_var(AEnv, sys_test, Test_Get76), get_var(AEnv, sys_test_not, Test_not_Get77), f_sys_member1(Member1_Param, List2_Get75, Test_Get76, Test_not_Get77, Key_Get78, IFTEST72), (IFTEST72\==[]->_10834=[];get_var(AEnv, last, IFTEST79), (IFTEST79\==[]->get_var(AEnv, last, Last_Get82), get_var(AEnv, sys_x, X_Get83), f_rplacd(Last_Get82, X_Get83, TrueResult86), _11020=TrueResult86;get_var(AEnv, sys_x, X_Get85), set_var(AEnv, first, X_Get85), _11020=X_Get85), get_var(AEnv, sys_x, X_Get88), set_var(AEnv, last, X_Get88), _10834=X_Get88), get_var(AEnv, sys_x, X_Get90), f_cdr(X_Get90, X), set_var(AEnv, sys_x, X), goto(do_label_22, AEnv), _TBResult=_GORES91)),
					  
					  [ addr(addr_tagbody_22_do_label_22,
						 do_label_22,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST23), (IFTEST23\==[]->get_var(AEnv, last, Last_Get26), get_var(AEnv, sys_list2, Get_var_Ret), f_rplacd(Last_Get26, Get_var_Ret, Rplacd_Ret), _11434=Rplacd_Ret;_11434=[]), (get_var(AEnv, first, First_Get), First_Get\==[], Block_exit_Ret=First_Get->true;get_var(AEnv, sys_list2, List2_Get30), Block_exit_Ret=List2_Get30), throw(block_exit([], Block_exit_Ret)), _11466=ThrowResult;get_var(AEnv, sys_x, X_Get35), f_car(X_Get35, Member1_Param100), get_var(AEnv, key, Get_var_Ret104), get_var(AEnv, sys_list2, List2_Get36), get_var(AEnv, sys_test, Get_var_Ret105), get_var(AEnv, sys_test_not, Get_var_Ret106), f_sys_member1(Member1_Param100, List2_Get36, Get_var_Ret105, Get_var_Ret106, Get_var_Ret104, IFTEST33), (IFTEST33\==[]->_11532=[];get_var(AEnv, last, IFTEST40), (IFTEST40\==[]->get_var(AEnv, last, Last_Get43), get_var(AEnv, sys_x, X_Get44), f_rplacd(Last_Get43, X_Get44, TrueResult47), _11590=TrueResult47;get_var(AEnv, sys_x, X_Get46), set_var(AEnv, first, X_Get46), _11590=X_Get46), get_var(AEnv, sys_x, X_Get49), set_var(AEnv, last, X_Get49), _11532=X_Get49), get_var(AEnv, sys_x, X_Get51), f_cdr(X_Get51, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_22, AEnv), _11466=_GORES)))
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
:- set_opv(nunion, symbol_function, f_nunion),
   DefunResult=nunion.
/*
:- side_effect(assert_lsp(nunion,
			  doc_string(nunion,
				     _9380,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive UNION.  Both LIST1 and LIST2 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nunion,
			  lambda_def(defun,
				     nunion,
				     f_nunion,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ do,
					 
					 [ [sys_x, sys_list1, [cdr, sys_x]],
					   [first],
					   [last]
					 ],
					 
					 [ [null, sys_x],
					   [when, last, [rplacd, last, sys_list2]],
					   [or, first, sys_list2]
					 ],
					 
					 [ unless,
					   
					   [ sys_member1,
					     [car, sys_x],
					     sys_list2,
					     sys_test,
					     sys_test_not,
					     key
					   ],
					   
					   [ if,
					     last,
					     [rplacd, last, sys_x],
					     [setq, first, sys_x]
					   ],
					   [setq, last, sys_x]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nunion,
			  arglist_info(nunion,
				       f_nunion,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nunion, init_args(2, f_nunion))).
*/
/*
#+(or WAM-CL ECL) 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:17566 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,intersection,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns a list consisting of those objects that are elements of both LIST1 and\r\nLIST2."),[do,[[x,list1,[cdr,x]],[ans]],[[null,x],[nreverse,ans]],[when,[member1,[car,x],list2,test,'test-not',key],[push,[car,x],ans]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
% macroexpand:-[push,[car,sys_x],sys_ans].
*/
/*
% into:-[setq,sys_ans,[cons,[car,sys_x],sys_ans]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
% macroexpand:-[push,[car,sys_x],sys_ans].
*/
/*
% into:-[setq,sys_ans,[cons,[car,sys_x],sys_ans]].
*/
doc: doc_string(intersection,
	      _9198,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns a list consisting of those objects that are elements of both LIST1 and\r\nLIST2.").

wl:lambda_def(defun, intersection, f_intersection, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [sys_ans]], [[null, sys_x], [nreverse, sys_ans]], [when, [sys_member1, [car, sys_x], sys_list2, sys_test, sys_test_not, key], [push, [car, sys_x], sys_ans]]]]).
wl:arglist_info(intersection, f_intersection, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_intersection).

/*

### Compiled Function: `CL:INTERSECTION` 
*/
f_intersection(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([sys_ans], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_23), get_var(AEnv, sys_x, IFTEST42), (IFTEST42==[]->get_var(AEnv, sys_ans, Ans_Get47), f_nreverse(Ans_Get47, RetResult45), throw(block_exit([], RetResult45)), _TBResult=ThrowResult46;get_var(AEnv, sys_x, X_Get51), f_car(X_Get51, Member1_Param), get_var(AEnv, key, Key_Get55), get_var(AEnv, sys_list2, List2_Get52), get_var(AEnv, sys_test, Test_Get53), get_var(AEnv, sys_test_not, Test_not_Get54), f_sys_member1(Member1_Param, List2_Get52, Test_Get53, Test_not_Get54, Key_Get55, IFTEST49), (IFTEST49\==[]->get_var(AEnv, sys_x, X_Get57), f_car(X_Get57, Car_Ret), get_var(AEnv, sys_ans, Ans_Get58), TrueResult59=[Car_Ret|Ans_Get58], set_var(AEnv, sys_ans, TrueResult59), _10104=TrueResult59;_10104=[]), get_var(AEnv, sys_x, X_Get60), f_cdr(X_Get60, X), set_var(AEnv, sys_x, X), goto(do_label_23, AEnv), _TBResult=_GORES61)),
					  
					  [ addr(addr_tagbody_23_do_label_23,
						 do_label_23,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, sys_ans, Nreverse_Param), f_nreverse(Nreverse_Param, Nreverse_Ret), throw(block_exit([], Nreverse_Ret)), _10532=ThrowResult;get_var(AEnv, sys_x, X_Get27), f_car(X_Get27, Member1_Param71), get_var(AEnv, key, Get_var_Ret), get_var(AEnv, sys_list2, Get_var_Ret75), get_var(AEnv, sys_test, Get_var_Ret76), get_var(AEnv, sys_test_not, Get_var_Ret77), f_sys_member1(Member1_Param71, Get_var_Ret75, Get_var_Ret76, Get_var_Ret77, Get_var_Ret, IFTEST25), (IFTEST25\==[]->get_var(AEnv, sys_x, X_Get33), f_car(X_Get33, Car_Ret78), get_var(AEnv, sys_ans, Ans_Get34), Set_var_Ret=[Car_Ret78|Ans_Get34], set_var(AEnv, sys_ans, Set_var_Ret), _10618=Set_var_Ret;_10618=[]), get_var(AEnv, sys_x, X_Get36), f_cdr(X_Get36, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_23, AEnv), _10532=_GORES)))
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
:- set_opv(intersection, symbol_function, f_intersection),
   DefunResult=intersection.
/*
:- side_effect(assert_lsp(intersection,
			  doc_string(intersection,
				     _9198,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns a list consisting of those objects that are elements of both LIST1 and\r\nLIST2."))).
*/
/*
:- side_effect(assert_lsp(intersection,
			  lambda_def(defun,
				     intersection,
				     f_intersection,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ do,
					 
					 [ [sys_x, sys_list1, [cdr, sys_x]],
					   [sys_ans]
					 ],
					 [[null, sys_x], [nreverse, sys_ans]],
					 
					 [ when,
					   
					   [ sys_member1,
					     [car, sys_x],
					     sys_list2,
					     sys_test,
					     sys_test_not,
					     key
					   ],
					   [push, [car, sys_x], sys_ans]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(intersection,
			  arglist_info(intersection,
				       f_intersection,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(intersection, init_args(2, f_intersection))).
*/
/*
 optional nreverse: not required by CLtL
*/
/*
#+(or WAM-CL ECL) 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:18013 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,nintersection,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive INTERSECTION.  Only LIST1 may be destroyed."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,[]]],first],[when,[member1,[car,x],list2,test,'test-not',key],[if,last,[rplacd,last,x],[setq,first,x]],[setq,last,x]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
doc: doc_string(nintersection,
	      _9274,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive INTERSECTION.  Only LIST1 may be destroyed.").

wl:lambda_def(defun, nintersection, f_nintersection, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, []]], first], [when, [sys_member1, [car, sys_x], sys_list2, sys_test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]).
wl:arglist_info(nintersection, f_nintersection, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_nintersection).

/*

### Compiled Function: `CL:NINTERSECTION` 
*/
f_nintersection(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_24), get_var(AEnv, sys_x, IFTEST54), (IFTEST54==[]->get_var(AEnv, last, IFTEST59), (IFTEST59\==[]->get_var(AEnv, last, Last_Get62), f_rplacd(Last_Get62, [], TrueResult63), _10394=TrueResult63;_10394=[]), get_var(AEnv, first, RetResult57), throw(block_exit([], RetResult57)), _TBResult=ThrowResult58;get_var(AEnv, sys_x, X_Get68), f_car(X_Get68, Member1_Param), get_var(AEnv, key, Key_Get72), get_var(AEnv, sys_list2, List2_Get69), get_var(AEnv, sys_test, Test_Get70), get_var(AEnv, sys_test_not, Test_not_Get71), f_sys_member1(Member1_Param, List2_Get69, Test_Get70, Test_not_Get71, Key_Get72, IFTEST66), (IFTEST66\==[]->get_var(AEnv, last, IFTEST73), (IFTEST73\==[]->get_var(AEnv, last, Last_Get76), get_var(AEnv, sys_x, X_Get77), f_rplacd(Last_Get76, X_Get77, TrueResult80), _10766=TrueResult80;get_var(AEnv, sys_x, X_Get79), set_var(AEnv, first, X_Get79), _10766=X_Get79), get_var(AEnv, sys_x, X_Get82), set_var(AEnv, last, X_Get82), _10580=X_Get82;_10580=[]), get_var(AEnv, sys_x, X_Get84), f_cdr(X_Get84, X), set_var(AEnv, sys_x, X), goto(do_label_24, AEnv), _TBResult=_GORES85)),
					  
					  [ addr(addr_tagbody_24_do_label_24,
						 do_label_24,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST23), (IFTEST23\==[]->get_var(AEnv, last, Last_Get26), f_rplacd(Last_Get26, [], Rplacd_Ret), _11178=Rplacd_Ret;_11178=[]), get_var(AEnv, first, Get_var_Ret), throw(block_exit([], Get_var_Ret)), _11182=ThrowResult;get_var(AEnv, sys_x, X_Get32), f_car(X_Get32, Member1_Param94), get_var(AEnv, key, Get_var_Ret97), get_var(AEnv, sys_list2, Get_var_Ret98), get_var(AEnv, sys_test, Get_var_Ret99), get_var(AEnv, sys_test_not, Get_var_Ret100), f_sys_member1(Member1_Param94, Get_var_Ret98, Get_var_Ret99, Get_var_Ret100, Get_var_Ret97, IFTEST30), (IFTEST30\==[]->get_var(AEnv, last, IFTEST37), (IFTEST37\==[]->get_var(AEnv, last, Last_Get40), get_var(AEnv, sys_x, X_Get41), f_rplacd(Last_Get40, X_Get41, TrueResult44), _11292=TrueResult44;get_var(AEnv, sys_x, X_Get43), set_var(AEnv, first, X_Get43), _11292=X_Get43), get_var(AEnv, sys_x, X_Get46), set_var(AEnv, last, X_Get46), _11322=X_Get46;_11322=[]), get_var(AEnv, sys_x, X_Get48), f_cdr(X_Get48, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_24, AEnv), _11182=_GORES)))
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
:- set_opv(nintersection, symbol_function, f_nintersection),
   DefunResult=nintersection.
/*
:- side_effect(assert_lsp(nintersection,
			  doc_string(nintersection,
				     _9274,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive INTERSECTION.  Only LIST1 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nintersection,
			  lambda_def(defun,
				     nintersection,
				     f_nintersection,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ do,
					 
					 [ [sys_x, sys_list1, [cdr, sys_x]],
					   [first],
					   [last]
					 ],
					 
					 [ [null, sys_x],
					   [when, last, [rplacd, last, []]],
					   first
					 ],
					 
					 [ when,
					   
					   [ sys_member1,
					     [car, sys_x],
					     sys_list2,
					     sys_test,
					     sys_test_not,
					     key
					   ],
					   
					   [ if,
					     last,
					     [rplacd, last, sys_x],
					     [setq, first, sys_x]
					   ],
					   [setq, last, sys_x]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nintersection,
			  arglist_info(nintersection,
				       f_nintersection,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nintersection, init_args(2, f_nintersection))).
*/
/*
#+(or WAM-CL ECL) 
(defun set-difference (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2."
  (do ((x list1 (cdr x))
       (ans))
      ((null x) (nreverse ans))
    (unless (member1 (car x) list2 test test-not key)
      (push (car x) ans))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:18489 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'set-difference',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2."),[do,[[x,list1,[cdr,x]],[ans]],[[null,x],[nreverse,ans]],[unless,[member1,[car,x],list2,test,'test-not',key],[push,[car,x],ans]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
% macroexpand:-[push,[car,sys_x],sys_ans].
*/
/*
% into:-[setq,sys_ans,[cons,[car,sys_x],sys_ans]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
% macroexpand:-[push,[car,sys_x],sys_ans].
*/
/*
% into:-[setq,sys_ans,[cons,[car,sys_x],sys_ans]].
*/
doc: doc_string(set_difference,
	      _9064,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2.").

wl:lambda_def(defun, set_difference, f_set_difference, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [sys_ans]], [[null, sys_x], [nreverse, sys_ans]], [unless, [sys_member1, [car, sys_x], sys_list2, sys_test, sys_test_not, key], [push, [car, sys_x], sys_ans]]]]).
wl:arglist_info(set_difference, f_set_difference, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_set_difference).

/*

### Compiled Function: `CL:SET-DIFFERENCE` 
*/
f_set_difference(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([sys_ans], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_25), get_var(AEnv, sys_x, IFTEST42), (IFTEST42==[]->get_var(AEnv, sys_ans, Ans_Get47), f_nreverse(Ans_Get47, RetResult45), throw(block_exit([], RetResult45)), _TBResult=ThrowResult46;get_var(AEnv, sys_x, X_Get51), f_car(X_Get51, Member1_Param), get_var(AEnv, key, Key_Get55), get_var(AEnv, sys_list2, List2_Get52), get_var(AEnv, sys_test, Test_Get53), get_var(AEnv, sys_test_not, Test_not_Get54), f_sys_member1(Member1_Param, List2_Get52, Test_Get53, Test_not_Get54, Key_Get55, IFTEST49), (IFTEST49\==[]->_9970=[];get_var(AEnv, sys_x, X_Get57), f_car(X_Get57, Car_Ret), get_var(AEnv, sys_ans, Ans_Get58), ElseResult59=[Car_Ret|Ans_Get58], set_var(AEnv, sys_ans, ElseResult59), _9970=ElseResult59), get_var(AEnv, sys_x, X_Get60), f_cdr(X_Get60, X), set_var(AEnv, sys_x, X), goto(do_label_25, AEnv), _TBResult=_GORES61)),
					  
					  [ addr(addr_tagbody_25_do_label_25,
						 do_label_25,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, sys_ans, Nreverse_Param), f_nreverse(Nreverse_Param, Nreverse_Ret), throw(block_exit([], Nreverse_Ret)), _10398=ThrowResult;get_var(AEnv, sys_x, X_Get27), f_car(X_Get27, Member1_Param71), get_var(AEnv, key, Get_var_Ret), get_var(AEnv, sys_list2, Get_var_Ret75), get_var(AEnv, sys_test, Get_var_Ret76), get_var(AEnv, sys_test_not, Get_var_Ret77), f_sys_member1(Member1_Param71, Get_var_Ret75, Get_var_Ret76, Get_var_Ret77, Get_var_Ret, IFTEST25), (IFTEST25\==[]->_10452=[];get_var(AEnv, sys_x, X_Get33), f_car(X_Get33, Car_Ret78), get_var(AEnv, sys_ans, Ans_Get34), Set_var_Ret=[Car_Ret78|Ans_Get34], set_var(AEnv, sys_ans, Set_var_Ret), _10452=Set_var_Ret), get_var(AEnv, sys_x, X_Get36), f_cdr(X_Get36, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_25, AEnv), _10398=_GORES)))
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
:- set_opv(set_difference, symbol_function, f_set_difference),
   DefunResult=set_difference.
/*
:- side_effect(assert_lsp(set_difference,
			  doc_string(set_difference,
				     _9064,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2."))).
*/
/*
:- side_effect(assert_lsp(set_difference,
			  lambda_def(defun,
				     set_difference,
				     f_set_difference,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ do,
					 
					 [ [sys_x, sys_list1, [cdr, sys_x]],
					   [sys_ans]
					 ],
					 [[null, sys_x], [nreverse, sys_ans]],
					 
					 [ unless,
					   
					   [ sys_member1,
					     [car, sys_x],
					     sys_list2,
					     sys_test,
					     sys_test_not,
					     key
					   ],
					   [push, [car, sys_x], sys_ans]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(set_difference,
			  arglist_info(set_difference,
				       f_set_difference,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(set_difference, init_args(2, f_set_difference))).
*/
/*
#+(or WAM-CL ECL) 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:18877 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nset-difference',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-DIFFERENCE.  Only LIST1 may be destroyed."),[do,[[x,list1,[cdr,x]],[first],[last]],[[null,x],[when,last,[rplacd,last,[]]],first],[unless,[member1,[car,x],list2,test,'test-not',key],[if,last,[rplacd,last,x],[setq,first,x]],[setq,last,x]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
doc: doc_string(nset_difference,
	      _9296,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-DIFFERENCE.  Only LIST1 may be destroyed.").

wl:lambda_def(defun, nset_difference, f_nset_difference, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[do, [[sys_x, sys_list1, [cdr, sys_x]], [first], [last]], [[null, sys_x], [when, last, [rplacd, last, []]], first], [unless, [sys_member1, [car, sys_x], sys_list2, sys_test, sys_test_not, key], [if, last, [rplacd, last, sys_x], [setq, first, sys_x]], [setq, last, sys_x]]]]).
wl:arglist_info(nset_difference, f_nset_difference, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_nset_difference).

/*

### Compiled Function: `CL:NSET-DIFFERENCE` 
*/
f_nset_difference(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_x, List1_Get), bv([first], []), bv([last], [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_26), get_var(AEnv, sys_x, IFTEST54), (IFTEST54==[]->get_var(AEnv, last, IFTEST59), (IFTEST59\==[]->get_var(AEnv, last, Last_Get62), f_rplacd(Last_Get62, [], TrueResult63), _10416=TrueResult63;_10416=[]), get_var(AEnv, first, RetResult57), throw(block_exit([], RetResult57)), _TBResult=ThrowResult58;get_var(AEnv, sys_x, X_Get68), f_car(X_Get68, Member1_Param), get_var(AEnv, key, Key_Get72), get_var(AEnv, sys_list2, List2_Get69), get_var(AEnv, sys_test, Test_Get70), get_var(AEnv, sys_test_not, Test_not_Get71), f_sys_member1(Member1_Param, List2_Get69, Test_Get70, Test_not_Get71, Key_Get72, IFTEST66), (IFTEST66\==[]->_10602=[];get_var(AEnv, last, IFTEST73), (IFTEST73\==[]->get_var(AEnv, last, Last_Get76), get_var(AEnv, sys_x, X_Get77), f_rplacd(Last_Get76, X_Get77, TrueResult80), _10788=TrueResult80;get_var(AEnv, sys_x, X_Get79), set_var(AEnv, first, X_Get79), _10788=X_Get79), get_var(AEnv, sys_x, X_Get82), set_var(AEnv, last, X_Get82), _10602=X_Get82), get_var(AEnv, sys_x, X_Get84), f_cdr(X_Get84, X), set_var(AEnv, sys_x, X), goto(do_label_26, AEnv), _TBResult=_GORES85)),
					  
					  [ addr(addr_tagbody_26_do_label_26,
						 do_label_26,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_x, IFTEST), (IFTEST==[]->get_var(AEnv, last, IFTEST23), (IFTEST23\==[]->get_var(AEnv, last, Last_Get26), f_rplacd(Last_Get26, [], Rplacd_Ret), _11200=Rplacd_Ret;_11200=[]), get_var(AEnv, first, Get_var_Ret), throw(block_exit([], Get_var_Ret)), _11204=ThrowResult;get_var(AEnv, sys_x, X_Get32), f_car(X_Get32, Member1_Param94), get_var(AEnv, key, Get_var_Ret97), get_var(AEnv, sys_list2, Get_var_Ret98), get_var(AEnv, sys_test, Get_var_Ret99), get_var(AEnv, sys_test_not, Get_var_Ret100), f_sys_member1(Member1_Param94, Get_var_Ret98, Get_var_Ret99, Get_var_Ret100, Get_var_Ret97, IFTEST30), (IFTEST30\==[]->_11258=[];get_var(AEnv, last, IFTEST37), (IFTEST37\==[]->get_var(AEnv, last, Last_Get40), get_var(AEnv, sys_x, X_Get41), f_rplacd(Last_Get40, X_Get41, TrueResult44), _11316=TrueResult44;get_var(AEnv, sys_x, X_Get43), set_var(AEnv, first, X_Get43), _11316=X_Get43), get_var(AEnv, sys_x, X_Get46), set_var(AEnv, last, X_Get46), _11258=X_Get46), get_var(AEnv, sys_x, X_Get48), f_cdr(X_Get48, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_26, AEnv), _11204=_GORES)))
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
:- set_opv(nset_difference, symbol_function, f_nset_difference),
   DefunResult=nset_difference.
/*
:- side_effect(assert_lsp(nset_difference,
			  doc_string(nset_difference,
				     _9296,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-DIFFERENCE.  Only LIST1 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nset_difference,
			  lambda_def(defun,
				     nset_difference,
				     f_nset_difference,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ do,
					 
					 [ [sys_x, sys_list1, [cdr, sys_x]],
					   [first],
					   [last]
					 ],
					 
					 [ [null, sys_x],
					   [when, last, [rplacd, last, []]],
					   first
					 ],
					 
					 [ unless,
					   
					   [ sys_member1,
					     [car, sys_x],
					     sys_list2,
					     sys_test,
					     sys_test_not,
					     key
					   ],
					   
					   [ if,
					     last,
					     [rplacd, last, sys_x],
					     [setq, first, sys_x]
					   ],
					   [setq, last, sys_x]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nset_difference,
			  arglist_info(nset_difference,
				       f_nset_difference,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nset_difference, init_args(2, f_nset_difference))).
*/
/*
#+(or WAM-CL ECL) 
(defun swap-args (f)
  ; (declare (c-local))
  (and f #'(lambda (x y) (funcall f y x))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:19359 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'swap-args',[f],[and,f,function([lambda,[x,y],[funcall,f,y,x]])]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_swap_args,
					       kw_function,
					       f_sys_swap_args)).
*/
wl:lambda_def(defun, sys_swap_args, f_sys_swap_args, [sys_f], [[and, sys_f, function([lambda, [sys_x, sys_y], [funcall, sys_f, sys_y, sys_x]])]]).
wl:arglist_info(sys_swap_args, f_sys_swap_args, [sys_f], arginfo{all:[sys_f], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_f], opt:0, req:[sys_f], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_swap_args).

/*

### Compiled Function: `SYS::SWAP-ARGS` 
*/
f_sys_swap_args(In, FnResult) :-
	GEnv=[bv(sys_f, In)],
	catch(( ( get_var(GEnv, sys_f, IFTEST),
		  (   IFTEST\==[]
		  ->  _9034=closure(kw_function, [ClosureEnvironment|GEnv], Whole, LResult, [sys_x, sys_y],  (get_var(ClosureEnvironment, sys_f, Get8), get_var(ClosureEnvironment, sys_x, X_Get), get_var(ClosureEnvironment, sys_y, Y_Get), f_apply(Get8, [Y_Get, X_Get], LResult)), [lambda, [sys_x, sys_y], [funcall, sys_f, sys_y, sys_x]])
		  ;   _9034=[]
		  )
		),
		_9034=FnResult
	      ),
	      block_exit(sys_swap_args, FnResult),
	      true).
:- set_opv(sys_swap_args, symbol_function, f_sys_swap_args),
   DefunResult=sys_swap_args.
/*
:- side_effect(assert_lsp(sys_swap_args,
			  lambda_def(defun,
				     sys_swap_args,
				     f_sys_swap_args,
				     [sys_f],
				     
				     [ 
				       [ and,
					 sys_f,
					 function(
						  [ lambda,
						    [sys_x, sys_y],
						    
						    [ funcall,
						      sys_f,
						      sys_y,
						      sys_x
						    ]
						  ])
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_swap_args,
			  arglist_info(sys_swap_args,
				       f_sys_swap_args,
				       [sys_f],
				       arginfo{ all:[sys_f],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_f],
						opt:0,
						req:[sys_f],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_swap_args, init_args(x, f_sys_swap_args))).
*/
/*
 (declare (c-local))
*/
/*
#+(or WAM-CL ECL) 
(defun set-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns, as a list, those elements of LIST1 that are not elements of LIST2 and
those elements of LIST2 that are not elements of LIST1."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
         (set-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:19475 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'set-exclusive-or',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2 and\r\nthose elements of LIST2 that are not elements of LIST1."),[nconc,['set-difference',list1,list2,':test',test,':test-not','test-not',':key',key],['set-difference',list2,list1,':test',['swap-args',test],':test-not',['swap-args','test-not'],':key',key]]])
doc: doc_string(set_exclusive_or,
	      _9514,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2 and\r\nthose elements of LIST2 that are not elements of LIST1.").

wl:lambda_def(defun, set_exclusive_or, f_set_exclusive_or, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[nconc, [set_difference, sys_list1, sys_list2, kw_test, sys_test, kw_test_not, sys_test_not, kw_key, key], [set_difference, sys_list2, sys_list1, kw_test, [sys_swap_args, sys_test], kw_test_not, [sys_swap_args, sys_test_not], kw_key, key]]]).
wl:arglist_info(set_exclusive_or, f_set_exclusive_or, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_set_exclusive_or).

/*

### Compiled Function: `CL:SET-EXCLUSIVE-OR` 
*/
f_set_exclusive_or(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  get_var(GEnv, sys_list2, List2_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  get_var(GEnv, sys_test_not, Test_not_Get),
		  f_set_difference(List1_Get,
				   List2_Get,
				   
				   [ kw_test,
				     Test_Get,
				     kw_test_not,
				     Test_not_Get,
				     kw_key,
				     Key_Get
				   ],
				   Set_difference_Ret),
		  get_var(GEnv, sys_list1, List1_Get18),
		  get_var(GEnv, sys_list2, List2_Get17),
		  get_var(GEnv, sys_test, Test_Get19),
		  f_sys_swap_args(Test_Get19, Swap_args_Ret),
		  get_var(GEnv, sys_test_not, Test_not_Get20),
		  f_sys_swap_args(Test_not_Get20, Swap_args_Ret27),
		  get_var(GEnv, key, Key_Get21),
		  f_set_difference(List2_Get17,
				   List1_Get18,
				   
				   [ kw_test,
				     Swap_args_Ret,
				     kw_test_not,
				     Swap_args_Ret27,
				     kw_key,
				     Key_Get21
				   ],
				   Set_difference_Ret28),
		  f_nconc([Set_difference_Ret, Set_difference_Ret28], Nconc_Ret)
		),
		Nconc_Ret=FnResult
	      ),
	      block_exit(set_exclusive_or, FnResult),
	      true).
:- set_opv(set_exclusive_or, symbol_function, f_set_exclusive_or),
   DefunResult=set_exclusive_or.
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  doc_string(set_exclusive_or,
				     _9514,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns, as a list, those elements of LIST1 that are not elements of LIST2 and\r\nthose elements of LIST2 that are not elements of LIST1."))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  lambda_def(defun,
				     set_exclusive_or,
				     f_set_exclusive_or,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ nconc,
					 
					 [ set_difference,
					   sys_list1,
					   sys_list2,
					   kw_test,
					   sys_test,
					   kw_test_not,
					   sys_test_not,
					   kw_key,
					   key
					 ],
					 
					 [ set_difference,
					   sys_list2,
					   sys_list1,
					   kw_test,
					   [sys_swap_args, sys_test],
					   kw_test_not,
					   [sys_swap_args, sys_test_not],
					   kw_key,
					   key
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or,
			  arglist_info(set_exclusive_or,
				       f_set_exclusive_or,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(set_exclusive_or, init_args(2, f_set_exclusive_or))).
*/
/*
#+(or WAM-CL ECL) 
(defun nset-exclusive-or (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Destructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."
  (nconc (set-difference list1 list2 :test test :test-not test-not :key key)
         (nset-difference list2 list1 :test (swap-args test) :test-not (swap-args test-not) :key key)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:19949 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nset-exclusive-or',[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."),[nconc,['set-difference',list1,list2,':test',test,':test-not','test-not',':key',key],['nset-difference',list2,list1,':test',['swap-args',test],':test-not',['swap-args','test-not'],':key',key]]])
doc: doc_string(nset_exclusive_or,
	      _8802,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed.").

wl:lambda_def(defun, nset_exclusive_or, f_nset_exclusive_or, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[nconc, [set_difference, sys_list1, sys_list2, kw_test, sys_test, kw_test_not, sys_test_not, kw_key, key], [nset_difference, sys_list2, sys_list1, kw_test, [sys_swap_args, sys_test], kw_test_not, [sys_swap_args, sys_test_not], kw_key, key]]]).
wl:arglist_info(nset_exclusive_or, f_nset_exclusive_or, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_nset_exclusive_or).

/*

### Compiled Function: `CL:NSET-EXCLUSIVE-OR` 
*/
f_nset_exclusive_or(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  get_var(GEnv, sys_list2, List2_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  get_var(GEnv, sys_test_not, Test_not_Get),
		  f_set_difference(List1_Get,
				   List2_Get,
				   
				   [ kw_test,
				     Test_Get,
				     kw_test_not,
				     Test_not_Get,
				     kw_key,
				     Key_Get
				   ],
				   Set_difference_Ret),
		  get_var(GEnv, sys_list1, List1_Get18),
		  get_var(GEnv, sys_list2, List2_Get17),
		  get_var(GEnv, sys_test, Test_Get19),
		  f_sys_swap_args(Test_Get19, Swap_args_Ret),
		  get_var(GEnv, sys_test_not, Test_not_Get20),
		  f_sys_swap_args(Test_not_Get20, Swap_args_Ret27),
		  get_var(GEnv, key, Key_Get21),
		  f_nset_difference(List2_Get17,
				    List1_Get18,
				    
				    [ kw_test,
				      Swap_args_Ret,
				      kw_test_not,
				      Swap_args_Ret27,
				      kw_key,
				      Key_Get21
				    ],
				    Nset_difference_Ret),
		  f_nconc([Set_difference_Ret, Nset_difference_Ret], Nconc_Ret)
		),
		Nconc_Ret=FnResult
	      ),
	      block_exit(nset_exclusive_or, FnResult),
	      true).
:- set_opv(nset_exclusive_or, symbol_function, f_nset_exclusive_or),
   DefunResult=nset_exclusive_or.
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  doc_string(nset_exclusive_or,
				     _8802,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nDestructive SET-EXCLUSIVE-OR.  Both LIST1 and LIST2 may be destroyed."))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  lambda_def(defun,
				     nset_exclusive_or,
				     f_nset_exclusive_or,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ nconc,
					 
					 [ set_difference,
					   sys_list1,
					   sys_list2,
					   kw_test,
					   sys_test,
					   kw_test_not,
					   sys_test_not,
					   kw_key,
					   key
					 ],
					 
					 [ nset_difference,
					   sys_list2,
					   sys_list1,
					   kw_test,
					   [sys_swap_args, sys_test],
					   kw_test_not,
					   [sys_swap_args, sys_test_not],
					   kw_key,
					   key
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or,
			  arglist_info(nset_exclusive_or,
				       f_nset_exclusive_or,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nset_exclusive_or, init_args(2, f_nset_exclusive_or))).
*/
/*
#+(or WAM-CL ECL) 
(defun subsetp (list1 list2 &key test test-not key)
  "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)
Returns T if every element of LIST1 is also an element of LIST2.  Returns NIL
otherwise."
  (do ((l list1 (cdr l)))
      ((null l) t)
    (unless (member1 (car l) list2 test test-not key)
      (return nil))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:20359 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,subsetp,[list1,list2,'&key',test,'test-not',key],'$STRING'("Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns T if every element of LIST1 is also an element of LIST2.  Returns NIL\r\notherwise."),[do,[[l,list1,[cdr,l]]],[[null,l],t],[unless,[member1,[car,l],list2,test,'test-not',key],[return,[]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_member1,
					       kw_function,
					       f_sys_member1)).
*/
doc: doc_string(subsetp,
	      _9050,
	      function,
	      "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns T if every element of LIST1 is also an element of LIST2.  Returns NIL\r\notherwise.").

wl:lambda_def(defun, subsetp, f_subsetp, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], [[do, [[sys_l, sys_list1, [cdr, sys_l]]], [[null, sys_l], t], [unless, [sys_member1, [car, sys_l], sys_list2, sys_test, sys_test_not, key], [return, []]]]]).
wl:arglist_info(subsetp, f_subsetp, [sys_list1, sys_list2, c38_key, sys_test, sys_test_not, key], arginfo{all:[sys_list1, sys_list2], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[sys_test, sys_test_not, key], names:[sys_list1, sys_list2, sys_test, sys_test_not, key], opt:0, req:[sys_list1, sys_list2], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_subsetp).

/*

### Compiled Function: `CL:SUBSETP` 
*/
f_subsetp(List1_In, List2_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_list1, List1_In), bv(sys_list2, List2_In), bv(sys_test, Test_In), bv(sys_test_not, Test_not_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, sys_test, sys_test, Test_In, []=Test_In, Test_P),
	get_kw(Env,
	       RestNKeys,
	       sys_test_not,
	       sys_test_not,
	       Test_not_In,
	       []=Test_not_In,
	       Test_not_P),
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_list1, List1_Get),
		  AEnv=[bv(sys_l, List1_Get)|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_27), get_var(AEnv, sys_l, IFTEST41), (IFTEST41==[]->throw(block_exit([], t)), _TBResult=ThrowResult45;get_var(AEnv, sys_l, L_Get49), f_car(L_Get49, Member1_Param), get_var(AEnv, key, Key_Get53), get_var(AEnv, sys_list2, List2_Get50), get_var(AEnv, sys_test, Test_Get51), get_var(AEnv, sys_test_not, Test_not_Get52), f_sys_member1(Member1_Param, List2_Get50, Test_Get51, Test_not_Get52, Key_Get53, IFTEST47), (IFTEST47\==[]->_9916=[];throw(block_exit([], [])), _9916=ThrowResult55), get_var(AEnv, sys_l, L_Get58), f_cdr(L_Get58, L), set_var(AEnv, sys_l, L), goto(do_label_27, AEnv), _TBResult=_GORES59)),
					  
					  [ addr(addr_tagbody_27_do_label_27,
						 do_label_27,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_l, IFTEST), (IFTEST==[]->throw(block_exit([], t)), _10314=ThrowResult;get_var(AEnv, sys_l, L_Get26), f_car(L_Get26, Member1_Param68), get_var(AEnv, key, Get_var_Ret), get_var(AEnv, sys_list2, Get_var_Ret70), get_var(AEnv, sys_test, Get_var_Ret71), get_var(AEnv, sys_test_not, Get_var_Ret72), f_sys_member1(Member1_Param68, Get_var_Ret70, Get_var_Ret71, Get_var_Ret72, Get_var_Ret, IFTEST24), (IFTEST24\==[]->_10368=[];throw(block_exit([], [])), _10368=ThrowResult32), get_var(AEnv, sys_l, L_Get35), f_cdr(L_Get35, Cdr_Ret), set_var(AEnv, sys_l, Cdr_Ret), goto(do_label_27, AEnv), _10314=_GORES)))
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
:- set_opv(subsetp, symbol_function, f_subsetp),
   DefunResult=subsetp.
/*
:- side_effect(assert_lsp(subsetp,
			  doc_string(subsetp,
				     _9050,
				     function,
				     "Args: (list1 list2 &key (key #'identity) (test #'eql) test-not)\r\nReturns T if every element of LIST1 is also an element of LIST2.  Returns NIL\r\notherwise."))).
*/
/*
:- side_effect(assert_lsp(subsetp,
			  lambda_def(defun,
				     subsetp,
				     f_subsetp,
				     
				     [ sys_list1,
				       sys_list2,
				       c38_key,
				       sys_test,
				       sys_test_not,
				       key
				     ],
				     
				     [ 
				       [ do,
					 [[sys_l, sys_list1, [cdr, sys_l]]],
					 [[null, sys_l], t],
					 
					 [ unless,
					   
					   [ sys_member1,
					     [car, sys_l],
					     sys_list2,
					     sys_test,
					     sys_test_not,
					     key
					   ],
					   [return, []]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(subsetp,
			  arglist_info(subsetp,
				       f_subsetp,
				       
				       [ sys_list1,
					 sys_list2,
					 c38_key,
					 sys_test,
					 sys_test_not,
					 key
				       ],
				       arginfo{ all:[sys_list1, sys_list2],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:
						    [ sys_test,
						      sys_test_not,
						      key
						    ],
						names:
						      [ sys_list1,
							sys_list2,
							sys_test,
							sys_test_not,
							key
						      ],
						opt:0,
						req:[sys_list1, sys_list2],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(subsetp, init_args(2, f_subsetp))).
*/
/*
#+(or WAM-CL ECL) 
(defun rassoc-if (test alist &key key)
  "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no
such pair exists."
  (rassoc test alist :test #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:20721 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'rassoc-if',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no\r\nsuch pair exists."),[rassoc,test,alist,':test',function(funcall),':key',key]])
doc: doc_string(rassoc_if,
	      _7868,
	      function,
	      "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no\r\nsuch pair exists.").

wl:lambda_def(defun, rassoc_if, f_rassoc_if, [sys_test, sys_alist, c38_key, key], [[rassoc, sys_test, sys_alist, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(rassoc_if, f_rassoc_if, [sys_test, sys_alist, c38_key, key], arginfo{all:[sys_test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_test, sys_alist, key], opt:0, req:[sys_test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_rassoc_if).

/*

### Compiled Function: `CL:RASSOC-IF` 
*/
f_rassoc_if(Test_In, Alist_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_test, Test_In), bv(sys_alist, Alist_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_alist, Alist_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  f_rassoc(Test_Get,
			   Alist_Get,
			   [kw_test, f_funcall, kw_key, Key_Get],
			   Rassoc_Ret)
		),
		Rassoc_Ret=FnResult
	      ),
	      block_exit(rassoc_if, FnResult),
	      true).
:- set_opv(rassoc_if, symbol_function, f_rassoc_if),
   DefunResult=rassoc_if.
/*
:- side_effect(assert_lsp(rassoc_if,
			  doc_string(rassoc_if,
				     _7868,
				     function,
				     "Returns the first pair in ALIST whose cdr satisfies TEST. Returns NIL if no\r\nsuch pair exists."))).
*/
/*
:- side_effect(assert_lsp(rassoc_if,
			  lambda_def(defun,
				     rassoc_if,
				     f_rassoc_if,
				     [sys_test, sys_alist, c38_key, key],
				     
				     [ 
				       [ rassoc,
					 sys_test,
					 sys_alist,
					 kw_test,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(rassoc_if,
			  arglist_info(rassoc_if,
				       f_rassoc_if,
				       [sys_test, sys_alist, c38_key, key],
				       arginfo{ all:[sys_test, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:[sys_test, sys_alist, key],
						opt:0,
						req:[sys_test, sys_alist],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(rassoc_if, init_args(2, f_rassoc_if))).
*/
/*
#+(or WAM-CL ECL) 
(defun rassoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL
if no such pair exists."
  (rassoc test alist :test-not #'funcall :key key))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:20932 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'rassoc-if-not',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL\r\nif no such pair exists."),[rassoc,test,alist,':test-not',function(funcall),':key',key]])
doc: doc_string(rassoc_if_not,
	      _7956,
	      function,
	      "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL\r\nif no such pair exists.").

wl:lambda_def(defun, rassoc_if_not, f_rassoc_if_not, [sys_test, sys_alist, c38_key, key], [[rassoc, sys_test, sys_alist, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(rassoc_if_not, f_rassoc_if_not, [sys_test, sys_alist, c38_key, key], arginfo{all:[sys_test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_test, sys_alist, key], opt:0, req:[sys_test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_rassoc_if_not).

/*

### Compiled Function: `CL:RASSOC-IF-NOT` 
*/
f_rassoc_if_not(Test_In, Alist_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_test, Test_In), bv(sys_alist, Alist_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_alist, Alist_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  f_rassoc(Test_Get,
			   Alist_Get,
			   [kw_test_not, f_funcall, kw_key, Key_Get],
			   Rassoc_Ret)
		),
		Rassoc_Ret=FnResult
	      ),
	      block_exit(rassoc_if_not, FnResult),
	      true).
:- set_opv(rassoc_if_not, symbol_function, f_rassoc_if_not),
   DefunResult=rassoc_if_not.
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  doc_string(rassoc_if_not,
				     _7956,
				     function,
				     "Returns the first pair in ALIST whose cdr does not satisfy TEST.  Returns NIL\r\nif no such pair exists."))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  lambda_def(defun,
				     rassoc_if_not,
				     f_rassoc_if_not,
				     [sys_test, sys_alist, c38_key, key],
				     
				     [ 
				       [ rassoc,
					 sys_test,
					 sys_alist,
					 kw_test_not,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not,
			  arglist_info(rassoc_if_not,
				       f_rassoc_if_not,
				       [sys_test, sys_alist, c38_key, key],
				       arginfo{ all:[sys_test, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:[sys_test, sys_alist, key],
						opt:0,
						req:[sys_test, sys_alist],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(rassoc_if_not, init_args(2, f_rassoc_if_not))).
*/
/*
#+(or WAM-CL ECL) 
(defun assoc-if (test alist &key key)
  "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no
such pair exists."
  (assoc test alist :test #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:21161 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'assoc-if',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no\r\nsuch pair exists."),[assoc,test,alist,':test',function(funcall),':key',key]])
doc: doc_string(assoc_if,
	      _7882,
	      function,
	      "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no\r\nsuch pair exists.").

wl:lambda_def(defun, assoc_if, f_assoc_if, [sys_test, sys_alist, c38_key, key], [[assoc, sys_test, sys_alist, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(assoc_if, f_assoc_if, [sys_test, sys_alist, c38_key, key], arginfo{all:[sys_test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_test, sys_alist, key], opt:0, req:[sys_test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_assoc_if).

/*

### Compiled Function: `CL:ASSOC-IF` 
*/
f_assoc_if(Test_In, Alist_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_test, Test_In), bv(sys_alist, Alist_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_alist, Alist_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  f_assoc(Test_Get,
			  Alist_Get,
			  [kw_test, f_funcall, kw_key, Key_Get],
			  Assoc_Ret)
		),
		Assoc_Ret=FnResult
	      ),
	      block_exit(assoc_if, FnResult),
	      true).
:- set_opv(assoc_if, symbol_function, f_assoc_if),
   DefunResult=assoc_if.
/*
:- side_effect(assert_lsp(assoc_if,
			  doc_string(assoc_if,
				     _7882,
				     function,
				     "Returns the first pair in ALIST whose car satisfies TEST.  Returns NIL if no\r\nsuch pair exists."))).
*/
/*
:- side_effect(assert_lsp(assoc_if,
			  lambda_def(defun,
				     assoc_if,
				     f_assoc_if,
				     [sys_test, sys_alist, c38_key, key],
				     
				     [ 
				       [ assoc,
					 sys_test,
					 sys_alist,
					 kw_test,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(assoc_if,
			  arglist_info(assoc_if,
				       f_assoc_if,
				       [sys_test, sys_alist, c38_key, key],
				       arginfo{ all:[sys_test, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:[sys_test, sys_alist, key],
						opt:0,
						req:[sys_test, sys_alist],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(assoc_if, init_args(2, f_assoc_if))).
*/
/*
#+(or WAM-CL ECL) 
(defun assoc-if-not (test alist &key key)
  "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL
if no such pair exists."
  (assoc test alist :test-not #'funcall :key key))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:21371 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'assoc-if-not',[test,alist,'&key',key],'$STRING'("Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL\r\nif no such pair exists."),[assoc,test,alist,':test-not',function(funcall),':key',key]])
doc: doc_string(assoc_if_not,
	      _7956,
	      function,
	      "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL\r\nif no such pair exists.").

wl:lambda_def(defun, assoc_if_not, f_assoc_if_not, [sys_test, sys_alist, c38_key, key], [[assoc, sys_test, sys_alist, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(assoc_if_not, f_assoc_if_not, [sys_test, sys_alist, c38_key, key], arginfo{all:[sys_test, sys_alist], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_test, sys_alist, key], opt:0, req:[sys_test, sys_alist], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_assoc_if_not).

/*

### Compiled Function: `CL:ASSOC-IF-NOT` 
*/
f_assoc_if_not(Test_In, Alist_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_test, Test_In), bv(sys_alist, Alist_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_alist, Alist_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  f_assoc(Test_Get,
			  Alist_Get,
			  [kw_test_not, f_funcall, kw_key, Key_Get],
			  Assoc_Ret)
		),
		Assoc_Ret=FnResult
	      ),
	      block_exit(assoc_if_not, FnResult),
	      true).
:- set_opv(assoc_if_not, symbol_function, f_assoc_if_not),
   DefunResult=assoc_if_not.
/*
:- side_effect(assert_lsp(assoc_if_not,
			  doc_string(assoc_if_not,
				     _7956,
				     function,
				     "Returns the first pair in ALIST whose car does not satisfy TEST.  Returns NIL\r\nif no such pair exists."))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not,
			  lambda_def(defun,
				     assoc_if_not,
				     f_assoc_if_not,
				     [sys_test, sys_alist, c38_key, key],
				     
				     [ 
				       [ assoc,
					 sys_test,
					 sys_alist,
					 kw_test_not,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not,
			  arglist_info(assoc_if_not,
				       f_assoc_if_not,
				       [sys_test, sys_alist, c38_key, key],
				       arginfo{ all:[sys_test, sys_alist],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:[sys_test, sys_alist, key],
						opt:0,
						req:[sys_test, sys_alist],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(assoc_if_not, init_args(2, f_assoc_if_not))).
*/
/*
#+(or WAM-CL ECL) 
(defun member-if (test list &key key)
  "Searches LIST for an element that satisfies TEST.  If found, returns the
sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:21598 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'member-if',[test,list,'&key',key],'$STRING'("Searches LIST for an element that satisfies TEST.  If found, returns the\r\nsublist of LIST that begins with the element.  If not found, returns NIL."),[member,test,list,':test',function(funcall),':key',key]])
doc: doc_string(member_if,
	      _8438,
	      function,
	      "Searches LIST for an element that satisfies TEST.  If found, returns the\r\nsublist of LIST that begins with the element.  If not found, returns NIL.").

wl:lambda_def(defun, member_if, f_member_if, [sys_test, list, c38_key, key], [[member, sys_test, list, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(member_if, f_member_if, [sys_test, list, c38_key, key], arginfo{all:[sys_test, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_test, list, key], opt:0, req:[sys_test, list], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_member_if).

/*

### Compiled Function: `CL:MEMBER-IF` 
*/
f_member_if(Test_In, List_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_test, Test_In), bv(list, List_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, list, List_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  f_member(Test_Get,
			   List_Get,
			   [kw_test, f_funcall, kw_key, Key_Get],
			   Member_Ret)
		),
		Member_Ret=FnResult
	      ),
	      block_exit(member_if, FnResult),
	      true).
:- set_opv(member_if, symbol_function, f_member_if),
   DefunResult=member_if.
/*
:- side_effect(assert_lsp(member_if,
			  doc_string(member_if,
				     _8438,
				     function,
				     "Searches LIST for an element that satisfies TEST.  If found, returns the\r\nsublist of LIST that begins with the element.  If not found, returns NIL."))).
*/
/*
:- side_effect(assert_lsp(member_if,
			  lambda_def(defun,
				     member_if,
				     f_member_if,
				     [sys_test, list, c38_key, key],
				     
				     [ 
				       [ member,
					 sys_test,
					 list,
					 kw_test,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(member_if,
			  arglist_info(member_if,
				       f_member_if,
				       [sys_test, list, c38_key, key],
				       arginfo{ all:[sys_test, list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:[sys_test, list, key],
						opt:0,
						req:[sys_test, list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(member_if, init_args(2, f_member_if))).
*/
/*
#+(or WAM-CL ECL) 
(defun member-if-not (test list &key key)
  "Searches LIST for an element that does not satisfy TEST.  If found, returns
the sublist of LIST that begins with the element.  If not found, returns NIL."
  (member test list :test-not #'funcall :key key))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:21860 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'member-if-not',[test,list,'&key',key],'$STRING'("Searches LIST for an element that does not satisfy TEST.  If found, returns\r\nthe sublist of LIST that begins with the element.  If not found, returns NIL."),[member,test,list,':test-not',function(funcall),':key',key]])
doc: doc_string(member_if_not,
	      _8518,
	      function,
	      "Searches LIST for an element that does not satisfy TEST.  If found, returns\r\nthe sublist of LIST that begins with the element.  If not found, returns NIL.").

wl:lambda_def(defun, member_if_not, f_member_if_not, [sys_test, list, c38_key, key], [[member, sys_test, list, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(member_if_not, f_member_if_not, [sys_test, list, c38_key, key], arginfo{all:[sys_test, list], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_test, list, key], opt:0, req:[sys_test, list], rest:0, sublists:0, whole:0}).
wl: init_args(2, f_member_if_not).

/*

### Compiled Function: `CL:MEMBER-IF-NOT` 
*/
f_member_if_not(Test_In, List_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_test, Test_In), bv(list, List_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, list, List_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  f_member(Test_Get,
			   List_Get,
			   [kw_test_not, f_funcall, kw_key, Key_Get],
			   Member_Ret)
		),
		Member_Ret=FnResult
	      ),
	      block_exit(member_if_not, FnResult),
	      true).
:- set_opv(member_if_not, symbol_function, f_member_if_not),
   DefunResult=member_if_not.
/*
:- side_effect(assert_lsp(member_if_not,
			  doc_string(member_if_not,
				     _8518,
				     function,
				     "Searches LIST for an element that does not satisfy TEST.  If found, returns\r\nthe sublist of LIST that begins with the element.  If not found, returns NIL."))).
*/
/*
:- side_effect(assert_lsp(member_if_not,
			  lambda_def(defun,
				     member_if_not,
				     f_member_if_not,
				     [sys_test, list, c38_key, key],
				     
				     [ 
				       [ member,
					 sys_test,
					 list,
					 kw_test_not,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(member_if_not,
			  arglist_info(member_if_not,
				       f_member_if_not,
				       [sys_test, list, c38_key, key],
				       arginfo{ all:[sys_test, list],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:[sys_test, list, key],
						opt:0,
						req:[sys_test, list],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(member_if_not, init_args(2, f_member_if_not))).
*/
/*
#+(or WAM-CL ECL) 
(defun subst-if (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.
The original TREE is not destroyed."
  (subst new test tree :test #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:22139 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'subst-if',[new,test,tree,'&key',key],'$STRING'("Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.\r\nThe original TREE is not destroyed."),[subst,new,test,tree,':test',function(funcall),':key',key]])
doc: doc_string(subst_if,
	      _8144,
	      function,
	      "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.\r\nThe original TREE is not destroyed.").

wl:lambda_def(defun, subst_if, f_subst_if, [sys_new, sys_test, sys_tree, c38_key, key], [[subst, sys_new, sys_test, sys_tree, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(subst_if, f_subst_if, [sys_new, sys_test, sys_tree, c38_key, key], arginfo{all:[sys_new, sys_test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, sys_test, sys_tree, key], opt:0, req:[sys_new, sys_test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_subst_if).

/*

### Compiled Function: `CL:SUBST-IF` 
*/
f_subst_if(New_In, Test_In, Tree_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_new, New_In), bv(sys_test, Test_In), bv(sys_tree, Tree_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_new, New_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  get_var(GEnv, sys_tree, Tree_Get),
		  f_subst(New_Get,
			  Test_Get,
			  Tree_Get,
			  kw_test,
			  f_funcall,
			  kw_key,
			  Key_Get,
			  Subst_Ret)
		),
		Subst_Ret=FnResult
	      ),
	      block_exit(subst_if, FnResult),
	      true).
:- set_opv(subst_if, symbol_function, f_subst_if),
   DefunResult=subst_if.
/*
:- side_effect(assert_lsp(subst_if,
			  doc_string(subst_if,
				     _8144,
				     function,
				     "Substitutes NEW for subtrees of TREE that satisfy TEST and returns the result.\r\nThe original TREE is not destroyed."))).
*/
/*
:- side_effect(assert_lsp(subst_if,
			  lambda_def(defun,
				     subst_if,
				     f_subst_if,
				     [sys_new, sys_test, sys_tree, c38_key, key],
				     
				     [ 
				       [ subst,
					 sys_new,
					 sys_test,
					 sys_tree,
					 kw_test,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(subst_if,
			  arglist_info(subst_if,
				       f_subst_if,
				       [sys_new, sys_test, sys_tree, c38_key, key],
				       arginfo{ all:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:
						      [ sys_new,
							sys_test,
							sys_tree,
							key
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(subst_if, init_args(3, f_subst_if))).
*/
/*
#+(or WAM-CL ECL) 
(defun subst-if-not (new test tree &key key)
  "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the
result.  The original TREE is not destroyed."
  (subst new test tree :test-not #'funcall :key key))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:22375 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'subst-if-not',[new,test,tree,'&key',key],'$STRING'("Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the\r\nresult.  The original TREE is not destroyed."),[subst,new,test,tree,':test-not',function(funcall),':key',key]])
doc: doc_string(subst_if_not,
	      _8232,
	      function,
	      "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the\r\nresult.  The original TREE is not destroyed.").

wl:lambda_def(defun, subst_if_not, f_subst_if_not, [sys_new, sys_test, sys_tree, c38_key, key], [[subst, sys_new, sys_test, sys_tree, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(subst_if_not, f_subst_if_not, [sys_new, sys_test, sys_tree, c38_key, key], arginfo{all:[sys_new, sys_test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, sys_test, sys_tree, key], opt:0, req:[sys_new, sys_test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_subst_if_not).

/*

### Compiled Function: `CL:SUBST-IF-NOT` 
*/
f_subst_if_not(New_In, Test_In, Tree_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_new, New_In), bv(sys_test, Test_In), bv(sys_tree, Tree_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_new, New_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  get_var(GEnv, sys_tree, Tree_Get),
		  f_subst(New_Get,
			  Test_Get,
			  Tree_Get,
			  kw_test_not,
			  f_funcall,
			  kw_key,
			  Key_Get,
			  Subst_Ret)
		),
		Subst_Ret=FnResult
	      ),
	      block_exit(subst_if_not, FnResult),
	      true).
:- set_opv(subst_if_not, symbol_function, f_subst_if_not),
   DefunResult=subst_if_not.
/*
:- side_effect(assert_lsp(subst_if_not,
			  doc_string(subst_if_not,
				     _8232,
				     function,
				     "Substitutes NEW for subtrees of TREE that do not satisfy TEST and returns the\r\nresult.  The original TREE is not destroyed."))).
*/
/*
:- side_effect(assert_lsp(subst_if_not,
			  lambda_def(defun,
				     subst_if_not,
				     f_subst_if_not,
				     [sys_new, sys_test, sys_tree, c38_key, key],
				     
				     [ 
				       [ subst,
					 sys_new,
					 sys_test,
					 sys_tree,
					 kw_test_not,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(subst_if_not,
			  arglist_info(subst_if_not,
				       f_subst_if_not,
				       [sys_new, sys_test, sys_tree, c38_key, key],
				       arginfo{ all:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:
						      [ sys_new,
							sys_test,
							sys_tree,
							key
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(subst_if_not, init_args(3, f_subst_if_not))).
*/
/*
#+(or WAM-CL ECL) 
(defun nsubst-if (new test tree &key key)
  "Destructive SUBST-IF. TREE may be modified."
  (nsubst new test tree :test #'funcall :key key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:22629 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nsubst-if',[new,test,tree,'&key',key],'$STRING'("Destructive SUBST-IF. TREE may be modified."),[nsubst,new,test,tree,':test',function(funcall),':key',key]])
doc: doc_string(nsubst_if,
	      _7370,
	      function,
	      "Destructive SUBST-IF. TREE may be modified.").

wl:lambda_def(defun, nsubst_if, f_nsubst_if, [sys_new, sys_test, sys_tree, c38_key, key], [[nsubst, sys_new, sys_test, sys_tree, kw_test, function(funcall), kw_key, key]]).
wl:arglist_info(nsubst_if, f_nsubst_if, [sys_new, sys_test, sys_tree, c38_key, key], arginfo{all:[sys_new, sys_test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, sys_test, sys_tree, key], opt:0, req:[sys_new, sys_test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_nsubst_if).

/*

### Compiled Function: `CL:NSUBST-IF` 
*/
f_nsubst_if(New_In, Test_In, Tree_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_new, New_In), bv(sys_test, Test_In), bv(sys_tree, Tree_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_new, New_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  get_var(GEnv, sys_tree, Tree_Get),
		  f_nsubst(New_Get,
			   Test_Get,
			   Tree_Get,
			   kw_test,
			   f_funcall,
			   kw_key,
			   Key_Get,
			   Nsubst_Ret)
		),
		Nsubst_Ret=FnResult
	      ),
	      block_exit(nsubst_if, FnResult),
	      true).
:- set_opv(nsubst_if, symbol_function, f_nsubst_if),
   DefunResult=nsubst_if.
/*
:- side_effect(assert_lsp(nsubst_if,
			  doc_string(nsubst_if,
				     _7370,
				     function,
				     "Destructive SUBST-IF. TREE may be modified."))).
*/
/*
:- side_effect(assert_lsp(nsubst_if,
			  lambda_def(defun,
				     nsubst_if,
				     f_nsubst_if,
				     [sys_new, sys_test, sys_tree, c38_key, key],
				     
				     [ 
				       [ nsubst,
					 sys_new,
					 sys_test,
					 sys_tree,
					 kw_test,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nsubst_if,
			  arglist_info(nsubst_if,
				       f_nsubst_if,
				       [sys_new, sys_test, sys_tree, c38_key, key],
				       arginfo{ all:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:
						      [ sys_new,
							sys_test,
							sys_tree,
							key
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nsubst_if, init_args(3, f_nsubst_if))).
*/
/*
#+(or WAM-CL ECL) 
(defun nsubst-if-not (new test tree &key key)
  "Destructive SUBST-IF-NOT. TREE may be modified."
  (nsubst new test tree :test-not #'funcall :key key))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-10.lisp:22795 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'nsubst-if-not',[new,test,tree,'&key',key],'$STRING'("Destructive SUBST-IF-NOT. TREE may be modified."),[nsubst,new,test,tree,':test-not',function(funcall),':key',key]])
doc: doc_string(nsubst_if_not,
	      _7418,
	      function,
	      "Destructive SUBST-IF-NOT. TREE may be modified.").

wl:lambda_def(defun, nsubst_if_not, f_nsubst_if_not, [sys_new, sys_test, sys_tree, c38_key, key], [[nsubst, sys_new, sys_test, sys_tree, kw_test_not, function(funcall), kw_key, key]]).
wl:arglist_info(nsubst_if_not, f_nsubst_if_not, [sys_new, sys_test, sys_tree, c38_key, key], arginfo{all:[sys_new, sys_test, sys_tree], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:[key], names:[sys_new, sys_test, sys_tree, key], opt:0, req:[sys_new, sys_test, sys_tree], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_nsubst_if_not).

/*

### Compiled Function: `CL:NSUBST-IF-NOT` 
*/
f_nsubst_if_not(New_In, Test_In, Tree_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_new, New_In), bv(sys_test, Test_In), bv(sys_tree, Tree_In), bv(key, Key_In)],
	get_kw(Env, RestNKeys, key, key, Key_In, []=Key_In, Key_P),
	catch(( ( get_var(GEnv, sys_new, New_Get),
		  ( get_var(GEnv, key, Key_Get),
		    get_var(GEnv, sys_test, Test_Get)
		  ),
		  get_var(GEnv, sys_tree, Tree_Get),
		  f_nsubst(New_Get,
			   Test_Get,
			   Tree_Get,
			   kw_test_not,
			   f_funcall,
			   kw_key,
			   Key_Get,
			   Nsubst_Ret)
		),
		Nsubst_Ret=FnResult
	      ),
	      block_exit(nsubst_if_not, FnResult),
	      true).
:- set_opv(nsubst_if_not, symbol_function, f_nsubst_if_not),
   DefunResult=nsubst_if_not.
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  doc_string(nsubst_if_not,
				     _7418,
				     function,
				     "Destructive SUBST-IF-NOT. TREE may be modified."))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  lambda_def(defun,
				     nsubst_if_not,
				     f_nsubst_if_not,
				     [sys_new, sys_test, sys_tree, c38_key, key],
				     
				     [ 
				       [ nsubst,
					 sys_new,
					 sys_test,
					 sys_tree,
					 kw_test_not,
					 function(funcall),
					 kw_key,
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not,
			  arglist_info(nsubst_if_not,
				       f_nsubst_if_not,
				       [sys_new, sys_test, sys_tree, c38_key, key],
				       arginfo{ all:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:[key],
						names:
						      [ sys_new,
							sys_test,
							sys_tree,
							key
						      ],
						opt:0,
						req:
						    [ sys_new,
						      sys_test,
						      sys_tree
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(nsubst_if_not, init_args(3, f_nsubst_if_not))).
*/


%; Total compilation time: 41.289 seconds


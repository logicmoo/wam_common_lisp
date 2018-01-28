#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "lib/lsp/setf" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/setf.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sun Jan 28 04:49:20 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
*/
/*
;;;  Copyright (c) 1990, Giuseppe Attardi.
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
;;;                                setf routines
*/
/*
(in-package "SYSTEM")

;;; DEFSETF macro.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/setf.lsp:494 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','$STRING'("SYSTEM")])
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
;; DEFSETF macro.
*/
/*
(defmacro defsetf (access-fn &rest rest)
  "Syntax: (defsetf symbol update-fun [doc])
	or
	(defsetf symbol lambda-list (store-var) {decl | doc}* {form}*)
Defines an expansion
	(setf (SYMBOL arg1 ... argn) value)
	=> (UPDATE-FUN arg1 ... argn value)
	   or
	   (let* ((temp1 ARG1) ... (tempn ARGn) (temp0 value)) rest)
where REST is the value of the last FORM with parameters in LAMBDA-LIST bound
to the symbols TEMP1 ... TEMPn and with STORE-VAR bound to the symbol TEMP0.
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (documentation 'SYMBOL 'setf)."
  (cond ((and (car rest) (or (symbolp (car rest)) (functionp (car rest))))
         `(eval-when (compile load eval)
		 (put-sysprop ',access-fn 'SETF-UPDATE-FN ',(car rest))
                 (rem-sysprop ',access-fn 'SETF-LAMBDA)
                 (rem-sysprop ',access-fn 'SETF-METHOD)
		 (rem-sysprop ',access-fn 'SETF-SYMBOL)
		 ,@(si::expand-set-documentation access-fn 'setf (cadr rest))
                 ',access-fn))
	(t
	 (let* ((store (second rest))
		(args (first rest))
		(body (cddr rest))
		(doc (find-documentation body)))
	   (unless (and (= (list-length store) 1) (symbolp (first store)))
		(error "Single store-variable expected."))
	   (setq rest `(lambda ,args #'(lambda ,store ,@body)))
         `(eval-when (compile load eval)
	      (put-sysprop ',access-fn 'SETF-LAMBDA #'(lambda (,@store ,@args) ,@body))
                 (rem-sysprop ',access-fn 'SETF-UPDATE-FN)
                 (rem-sysprop ',access-fn 'SETF-METHOD)
	      (rem-sysprop ',access-fn 'SETF-SYMBOL)
	      ,@(si::expand-set-documentation access-fn 'setf doc)
	      ',access-fn)))))


;;; DEFINE-SETF-METHOD macro.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/setf.lsp:537 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defsetf,['access-fn','&rest',rest],'$STRING'("Syntax: (defsetf symbol update-fun [doc])\n\tor\n\t(defsetf symbol lambda-list (store-var) {decl | doc}* {form}*)\nDefines an expansion\n\t(setf (SYMBOL arg1 ... argn) value)\n\t=> (UPDATE-FUN arg1 ... argn value)\n\t   or\n\t   (let* ((temp1 ARG1) ... (tempn ARGn) (temp0 value)) rest)\nwhere REST is the value of the last FORM with parameters in LAMBDA-LIST bound\nto the symbols TEMP1 ... TEMPn and with STORE-VAR bound to the symbol TEMP0.\nThe doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved\nby (documentation 'SYMBOL 'setf)."),[cond,[[and,[car,rest],[or,[symbolp,[car,rest]],[functionp,[car,rest]]]],['#BQ',['eval-when',[compile,load,eval],['put-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-UPDATE-FN'],[quote,['#COMMA',[car,rest]]]],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-LAMBDA']],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-METHOD']],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-SYMBOL']],['#BQ-COMMA-ELIPSE',['si::expand-set-documentation','access-fn',[quote,setf],[cadr,rest]]],[quote,['#COMMA','access-fn']]]]],[t,['let*',[[store,[second,rest]],[args,[first,rest]],[body,[cddr,rest]],[doc,['find-documentation',body]]],[unless,[and,[=,['list-length',store],1],[symbolp,[first,store]]],[error,'$STRING'("Single store-variable expected.")]],[setq,rest,['#BQ',[lambda,['#COMMA',args],function([lambda,['#COMMA',store],['#BQ-COMMA-ELIPSE',body]])]]],['#BQ',['eval-when',[compile,load,eval],['put-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-LAMBDA'],function([lambda,[['#BQ-COMMA-ELIPSE',store],['#BQ-COMMA-ELIPSE',args]],['#BQ-COMMA-ELIPSE',body]])],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-UPDATE-FN']],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-METHOD']],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-SYMBOL']],['#BQ-COMMA-ELIPSE',['si::expand-set-documentation','access-fn',[quote,setf],doc]],[quote,['#COMMA','access-fn']]]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_expand_set_documentation,
					       kw_function,
					       f_sys_expand_set_documentation)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_find_documentation,
					       kw_function,
					       f_sys_find_documentation)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_expand_set_documentation,
					       kw_function,
					       f_sys_expand_set_documentation)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       defsetf,
					       kw_special,
					       sf_defsetf)).
*/
doc: doc_string(defsetf,
	      _6806,
	      function,
	      "Syntax: (defsetf symbol update-fun [doc])\n\tor\n\t(defsetf symbol lambda-list (store-var) {decl | doc}* {form}*)\nDefines an expansion\n\t(setf (SYMBOL arg1 ... argn) value)\n\t=> (UPDATE-FUN arg1 ... argn value)\n\t   or\n\t   (let* ((temp1 ARG1) ... (tempn ARGn) (temp0 value)) rest)\nwhere REST is the value of the last FORM with parameters in LAMBDA-LIST bound\nto the symbols TEMP1 ... TEMPn and with STORE-VAR bound to the symbol TEMP0.\nThe doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved\nby (documentation 'SYMBOL 'setf).").

wl:lambda_def(defmacro, defsetf, mf_defsetf, [sys_access_fn, c38_rest, rest], [[cond, [[and, [car, rest], [or, [symbolp, [car, rest]], [functionp, [car, rest]]]], ['#BQ', [eval_when, [compile, load, eval], [sys_put_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_update_fn], [quote, ['#COMMA', [car, rest]]]], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_lambda]], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_method]], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_symbol]], ['#BQ-COMMA-ELIPSE', [sys_expand_set_documentation, sys_access_fn, [quote, setf], [cadr, rest]]], [quote, ['#COMMA', sys_access_fn]]]]], [t, [let_xx, [[sys_store, [second, rest]], [sys_args, [first, rest]], [sys_body, [cddr, rest]], [sys_doc, [sys_find_documentation, sys_body]]], [unless, [and, [=, [list_length, sys_store], 1], [symbolp, [first, sys_store]]], [error, '$ARRAY'([*], claz_base_character, "Single store-variable expected.")]], [setq, rest, ['#BQ', [lambda, ['#COMMA', sys_args], function([lambda, ['#COMMA', sys_store], ['#BQ-COMMA-ELIPSE', sys_body]])]]], ['#BQ', [eval_when, [compile, load, eval], [sys_put_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_lambda], function([lambda, [['#BQ-COMMA-ELIPSE', sys_store], ['#BQ-COMMA-ELIPSE', sys_args]], ['#BQ-COMMA-ELIPSE', sys_body]])], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_update_fn]], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_method]], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_symbol]], ['#BQ-COMMA-ELIPSE', [sys_expand_set_documentation, sys_access_fn, [quote, setf], sys_doc]], [quote, ['#COMMA', sys_access_fn]]]]]]]]).
wl:arglist_info(defsetf, mf_defsetf, [sys_access_fn, c38_rest, rest], arginfo{all:[sys_access_fn], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_access_fn, rest], opt:0, req:[sys_access_fn], rest:[rest], sublists:0, whole:0}).
wl: init_args(1, mf_defsetf).

/*

### Compiled Macro Operator: `CL:DEFSETF` 
*/
sf_defsetf(MacroEnv, Access_fn_In, RestNKeys, FResult) :-
	mf_defsetf([defsetf, Access_fn_In|RestNKeys], MacroEnv, MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFSETF` 
*/
mf_defsetf([defsetf, Access_fn_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_access_fn, Access_fn_In), bv(rest, RestNKeys)],
	catch(( ( get_var(GEnv, rest, Rest_Get),
		  f_car(Rest_Get, IFTEST8),
		  (   IFTEST8\==[]
		  ->  (   get_var(GEnv, rest, Rest_Get11),
			  f_car(Rest_Get11, Symbolp_Param),
			  f_symbolp(Symbolp_Param, FORM1_Res),
			  FORM1_Res\==[],
			  TrueResult=FORM1_Res
		      ->  true
		      ;   get_var(GEnv, rest, Rest_Get12),
			  f_car(Rest_Get12, Functionp_Param),
			  f_functionp(Functionp_Param, Functionp_Ret),
			  TrueResult=Functionp_Ret
		      ),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, rest, Rest_Get16),
		      get_var(GEnv, sys_access_fn, Access_fn_Get),
		      f_car(Rest_Get16, Car_Ret),
		      get_var(GEnv, rest, Rest_Get21),
		      get_var(GEnv, sys_access_fn, Access_fn_Get17),
		      f_cadr(Rest_Get21, Setf),
		      f_sys_expand_set_documentation(Access_fn_Get17,
						     setf,
						     Setf,
						     Set_documentation_Ret),
		      get_var(GEnv, sys_access_fn, Access_fn_Get22),
		      bq_append(
				[ 
				  [ sys_rem_sysprop,
				    [quote, Access_fn_Get17],
				    [quote, sys_setf_symbol]
				  ]
				| Set_documentation_Ret
				],
				[[quote, Access_fn_Get22]],
				Bq_append_Ret),
		      _6864=[eval_when, [compile, load, eval], [sys_put_sysprop, [quote, Access_fn_Get], [quote, sys_setf_update_fn], [quote, Car_Ret]], [sys_rem_sysprop, [quote, Access_fn_Get17], [quote, sys_setf_lambda]], [sys_rem_sysprop, [quote, Access_fn_Get17], [quote, sys_setf_method]]|Bq_append_Ret]
		  ;   get_var(GEnv, rest, Rest_Get26),
		      f_second(Rest_Get26, Store_Init),
		      LEnv=[bv(sys_store, Store_Init)|GEnv],
		      get_var(LEnv, rest, Rest_Get31),
		      f_car(Rest_Get31, Args_Init),
		      LEnv30=[bv(sys_args, Args_Init)|LEnv],
		      get_var(LEnv30, rest, Rest_Get36),
		      f_cddr(Rest_Get36, Body_Init),
		      LEnv35=[bv(sys_body, Body_Init)|LEnv30],
		      get_var(LEnv35, sys_body, Body_Get),
		      f_sys_find_documentation(Body_Get, Doc_Init),
		      LEnv40=[bv(sys_doc, Doc_Init)|LEnv35],
		      get_var(LEnv40, sys_store, Store_Get),
		      f_list_length(Store_Get, PredArg1Result),
		      (   PredArg1Result=:=1
		      ->  get_var(LEnv40, sys_store, Store_Get49),
			  f_car(Store_Get49, Symbolp_Param67),
			  f_symbolp(Symbolp_Param67, TrueResult50),
			  IFTEST43=TrueResult50
		      ;   IFTEST43=[]
		      ),
		      (   IFTEST43\==[]
		      ->  _7732=[]
		      ;   f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "Single store-variable expected.")
				  ],
				  ElseResult),
			  _7732=ElseResult
		      ),
		      get_var(LEnv40, sys_args, Args_Get),
		      set_var(LEnv40,
			      rest,
			      
			      [ lambda,
				Args_Get,
				function(
					 [ lambda,
					   ['#COMMA', sys_store],
					   ['#BQ-COMMA-ELIPSE', sys_body]
					 ])
			      ]),
		      get_var(LEnv40, sys_access_fn, Access_fn_Get54),
		      get_var(LEnv40, sys_doc, Doc_Get),
		      f_sys_expand_set_documentation(Access_fn_Get54,
						     setf,
						     Doc_Get,
						     Set_documentation_Ret72),
		      get_var(LEnv40, sys_access_fn, Access_fn_Get60),
		      bq_append(
				[ 
				  [ sys_rem_sysprop,
				    [quote, Access_fn_Get54],
				    [quote, sys_setf_symbol]
				  ]
				| Set_documentation_Ret72
				],
				[[quote, Access_fn_Get60]],
				Bq_append_Ret73),
		      _6864=[eval_when, [compile, load, eval], [sys_put_sysprop, [quote, Access_fn_Get54], [quote, sys_setf_lambda], function([lambda, [['#BQ-COMMA-ELIPSE', sys_store], ['#BQ-COMMA-ELIPSE', sys_args]], ['#BQ-COMMA-ELIPSE', sys_body]])], [sys_rem_sysprop, [quote, Access_fn_Get54], [quote, sys_setf_update_fn]], [sys_rem_sysprop, [quote, Access_fn_Get54], [quote, sys_setf_method]]|Bq_append_Ret73]
		  )
		),
		_6864=MFResult
	      ),
	      block_exit(defsetf, MFResult),
	      true).
:- set_opv(mf_defsetf, type_of, sys_macro),
   set_opv(defsetf, symbol_function, mf_defsetf),
   DefMacroResult=defsetf.
/*
:- side_effect(assert_lsp(defsetf,
			  doc_string(defsetf,
				     _6806,
				     function,
				     "Syntax: (defsetf symbol update-fun [doc])\n\tor\n\t(defsetf symbol lambda-list (store-var) {decl | doc}* {form}*)\nDefines an expansion\n\t(setf (SYMBOL arg1 ... argn) value)\n\t=> (UPDATE-FUN arg1 ... argn value)\n\t   or\n\t   (let* ((temp1 ARG1) ... (tempn ARGn) (temp0 value)) rest)\nwhere REST is the value of the last FORM with parameters in LAMBDA-LIST bound\nto the symbols TEMP1 ... TEMPn and with STORE-VAR bound to the symbol TEMP0.\nThe doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved\nby (documentation 'SYMBOL 'setf)."))).
*/
/*
:- side_effect(assert_lsp(defsetf,
			  lambda_def(defmacro,
				     defsetf,
				     mf_defsetf,
				     [sys_access_fn, c38_rest, rest],
				     
				     [ 
				       [ cond,
					 
					 [ 
					   [ and,
					     [car, rest],
					     
					     [ or,
					       [symbolp, [car, rest]],
					       [functionp, [car, rest]]
					     ]
					   ],
					   
					   [ '#BQ',
					     
					     [ eval_when,
					       [compile, load, eval],
					       
					       [ sys_put_sysprop,
						 
						 [ quote,
						   ['#COMMA', sys_access_fn]
						 ],
						 [quote, sys_setf_update_fn],
						 [quote, ['#COMMA', [car, rest]]]
					       ],
					       
					       [ sys_rem_sysprop,
						 
						 [ quote,
						   ['#COMMA', sys_access_fn]
						 ],
						 [quote, sys_setf_lambda]
					       ],
					       
					       [ sys_rem_sysprop,
						 
						 [ quote,
						   ['#COMMA', sys_access_fn]
						 ],
						 [quote, sys_setf_method]
					       ],
					       
					       [ sys_rem_sysprop,
						 
						 [ quote,
						   ['#COMMA', sys_access_fn]
						 ],
						 [quote, sys_setf_symbol]
					       ],
					       
					       [ '#BQ-COMMA-ELIPSE',
						 
						 [ sys_expand_set_documentation,
						   sys_access_fn,
						   [quote, setf],
						   [cadr, rest]
						 ]
					       ],
					       
					       [ quote,
						 ['#COMMA', sys_access_fn]
					       ]
					     ]
					   ]
					 ],
					 
					 [ t,
					   
					   [ let_xx,
					     
					     [ [sys_store, [second, rest]],
					       [sys_args, [first, rest]],
					       [sys_body, [cddr, rest]],
					       
					       [ sys_doc,
						 
						 [ sys_find_documentation,
						   sys_body
						 ]
					       ]
					     ],
					     
					     [ unless,
					       
					       [ and,
						 [=, [list_length, sys_store], 1],
						 [symbolp, [first, sys_store]]
					       ],
					       
					       [ error,
						 '$ARRAY'([*],
							  claz_base_character,
							  "Single store-variable expected.")
					       ]
					     ],
					     
					     [ setq,
					       rest,
					       
					       [ '#BQ',
						 
						 [ lambda,
						   ['#COMMA', sys_args],
						   function(
							    [ lambda,
							      
							      [ '#COMMA',
								sys_store
							      ],
							      
							      [ '#BQ-COMMA-ELIPSE',
								sys_body
							      ]
							    ])
						 ]
					       ]
					     ],
					     
					     [ '#BQ',
					       
					       [ eval_when,
						 [compile, load, eval],
						 
						 [ sys_put_sysprop,
						   
						   [ quote,
						     ['#COMMA', sys_access_fn]
						   ],
						   [quote, sys_setf_lambda],
						   function(
							    [ lambda,
							      
							      [ 
								[ '#BQ-COMMA-ELIPSE',
								  sys_store
								],
								
								[ '#BQ-COMMA-ELIPSE',
								  sys_args
								]
							      ],
							      
							      [ '#BQ-COMMA-ELIPSE',
								sys_body
							      ]
							    ])
						 ],
						 
						 [ sys_rem_sysprop,
						   
						   [ quote,
						     ['#COMMA', sys_access_fn]
						   ],
						   [quote, sys_setf_update_fn]
						 ],
						 
						 [ sys_rem_sysprop,
						   
						   [ quote,
						     ['#COMMA', sys_access_fn]
						   ],
						   [quote, sys_setf_method]
						 ],
						 
						 [ sys_rem_sysprop,
						   
						   [ quote,
						     ['#COMMA', sys_access_fn]
						   ],
						   [quote, sys_setf_symbol]
						 ],
						 
						 [ '#BQ-COMMA-ELIPSE',
						   
						   [ sys_expand_set_documentation,
						     sys_access_fn,
						     [quote, setf],
						     sys_doc
						   ]
						 ],
						 
						 [ quote,
						   ['#COMMA', sys_access_fn]
						 ]
					       ]
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
				       [sys_access_fn, c38_rest, rest],
				       arginfo{ all:[sys_access_fn],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_access_fn, rest],
						opt:0,
						req:[sys_access_fn],
						rest:[rest],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defsetf, init_args(1, mf_defsetf))).
*/
/*
;; DEFINE-SETF-METHOD macro.
*/
/*
(defmacro define-setf-expander (access-fn args &rest body)
  "Syntax: (define-setf-expander symbol defmacro-lambda-list {decl | doc}*
          {form}*)
Defines the SETF-method for generalized-variables (SYMBOL ...).
When a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs
given in the DEFINE-SETF-EXPANDER are evaluated in order with the parameters in
DEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five
values
	(var1 ... vark)
	(form1 ... formk)
	(value-var)
	storing-form
	access-form
in order.  These values are collectively called the five gangs of the
generalized variable (SYMBOL arg1 ... argn).  The whole SETF form is then
expanded into
	(let* ((var1 from1) ... (vark formk)
	       (value-var value-form))
	  storing-form)
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (DOCUMENTATION 'SYMBOL 'SETF)."
  (let ((env (member '&environment args :test #'eq)))
    (if env
	(setq args (cons (second env)
			 (nconc (ldiff args env) (cddr env))))
	(progn
	  (setq env (gensym))
	  (setq args (cons env args))
	  (push `(declare (ignore ,env)) body))))
  `(eval-when (compile load eval)
	  (put-sysprop ',access-fn 'SETF-METHOD #'(lambda ,args ,@body))
          (rem-sysprop ',access-fn 'SETF-LAMBDA)
          (rem-sysprop ',access-fn 'SETF-UPDATE-FN)
	  (rem-sysprop ',access-fn 'SETF-SYMBOL)
	  ,@(si::expand-set-documentation access-fn 'setf
					  (find-documentation body))
          ',access-fn))


;;; GET-SETF-METHOD.
;;; It just calls GET-SETF-METHOD-MULTIPLE-VALUE
;;;  and checks the number of the store variable.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/setf.lsp:2227 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'define-setf-expander',['access-fn',args,'&rest',body],'$STRING'("Syntax: (define-setf-expander symbol defmacro-lambda-list {decl | doc}*\n          {form}*)\nDefines the SETF-method for generalized-variables (SYMBOL ...).\nWhen a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs\ngiven in the DEFINE-SETF-EXPANDER are evaluated in order with the parameters in\nDEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five\nvalues\n\t(var1 ... vark)\n\t(form1 ... formk)\n\t(value-var)\n\tstoring-form\n\taccess-form\nin order.  These values are collectively called the five gangs of the\ngeneralized variable (SYMBOL arg1 ... argn).  The whole SETF form is then\nexpanded into\n\t(let* ((var1 from1) ... (vark formk)\n\t       (value-var value-form))\n\t  storing-form)\nThe doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved\nby (DOCUMENTATION 'SYMBOL 'SETF)."),[let,[[env,[member,[quote,'&environment'],args,':test',function(eq)]]],[if,env,[setq,args,[cons,[second,env],[nconc,[ldiff,args,env],[cddr,env]]]],[progn,[setq,env,[gensym]],[setq,args,[cons,env,args]],[push,['#BQ',[declare,[ignore,['#COMMA',env]]]],body]]]],['#BQ',['eval-when',[compile,load,eval],['put-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-METHOD'],function([lambda,['#COMMA',args],['#BQ-COMMA-ELIPSE',body]])],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-LAMBDA']],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-UPDATE-FN']],['rem-sysprop',[quote,['#COMMA','access-fn']],[quote,'SETF-SYMBOL']],['#BQ-COMMA-ELIPSE',['si::expand-set-documentation','access-fn',[quote,setf],['find-documentation',body]]],[quote,['#COMMA','access-fn']]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_find_documentation,
					       kw_function,
					       f_sys_find_documentation)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_expand_set_documentation,
					       kw_function,
					       f_sys_expand_set_documentation)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       define_setf_expander,
					       kw_special,
					       sf_define_setf_expander)).
*/
doc: doc_string(define_setf_expander,
	      _5714,
	      function,
	      "Syntax: (define-setf-expander symbol defmacro-lambda-list {decl | doc}*\n          {form}*)\nDefines the SETF-method for generalized-variables (SYMBOL ...).\nWhen a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs\ngiven in the DEFINE-SETF-EXPANDER are evaluated in order with the parameters in\nDEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five\nvalues\n\t(var1 ... vark)\n\t(form1 ... formk)\n\t(value-var)\n\tstoring-form\n\taccess-form\nin order.  These values are collectively called the five gangs of the\ngeneralized variable (SYMBOL arg1 ... argn).  The whole SETF form is then\nexpanded into\n\t(let* ((var1 from1) ... (vark formk)\n\t       (value-var value-form))\n\t  storing-form)\nThe doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved\nby (DOCUMENTATION 'SYMBOL 'SETF).").

wl:lambda_def(defmacro, define_setf_expander, mf_define_setf_expander, [sys_access_fn, sys_args, c38_rest, sys_body], [[let, [[sys_env, [member, [quote, c38_environment], sys_args, kw_test, function(eq)]]], [if, sys_env, [setq, sys_args, [cons, [second, sys_env], [nconc, [ldiff, sys_args, sys_env], [cddr, sys_env]]]], [progn, [setq, sys_env, [gensym]], [setq, sys_args, [cons, sys_env, sys_args]], [push, ['#BQ', [declare, [ignore, ['#COMMA', sys_env]]]], sys_body]]]], ['#BQ', [eval_when, [compile, load, eval], [sys_put_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_method], function([lambda, ['#COMMA', sys_args], ['#BQ-COMMA-ELIPSE', sys_body]])], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_lambda]], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_update_fn]], [sys_rem_sysprop, [quote, ['#COMMA', sys_access_fn]], [quote, sys_setf_symbol]], ['#BQ-COMMA-ELIPSE', [sys_expand_set_documentation, sys_access_fn, [quote, setf], [sys_find_documentation, sys_body]]], [quote, ['#COMMA', sys_access_fn]]]]]).
wl:arglist_info(define_setf_expander, mf_define_setf_expander, [sys_access_fn, sys_args, c38_rest, sys_body], arginfo{all:[sys_access_fn, sys_args], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_access_fn, sys_args, sys_body], opt:0, req:[sys_access_fn, sys_args], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(2, mf_define_setf_expander).

/*

### Compiled Macro Operator: `CL:DEFINE-SETF-EXPANDER` 
*/
sf_define_setf_expander(MacroEnv, Access_fn_In, Args_In, RestNKeys, FResult) :-
	mf_define_setf_expander(
				[ define_setf_expander,
				  Access_fn_In,
				  Args_In
				| RestNKeys
				],
				MacroEnv,
				MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFINE-SETF-EXPANDER` 
*/
mf_define_setf_expander([define_setf_expander, Access_fn_In, Args_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_access_fn, Access_fn_In), bv(sys_args, Args_In), bv(sys_body, RestNKeys)],
	catch(( ( get_var(GEnv, sys_args, Args_Get),
		  f_member(c38_environment, Args_Get, [kw_test, f_eq], Env_Init),
		  LEnv=[bv(sys_env, Env_Init)|GEnv],
		  get_var(LEnv, sys_env, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, sys_env, Env_Get16),
		      f_second(Env_Get16, Second_Ret),
		      get_var(LEnv, sys_args, Args_Get17),
		      get_var(LEnv, sys_env, Env_Get18),
		      f_ldiff(Args_Get17, Env_Get18, Ldiff_Ret),
		      get_var(LEnv, sys_env, Env_Get19),
		      f_cddr(Env_Get19, Cddr_Ret),
		      f_nconc([Ldiff_Ret, Cddr_Ret], Nconc_Ret),
		      TrueResult=[Second_Ret|Nconc_Ret],
		      set_var(LEnv, sys_args, TrueResult),
		      LetResult=TrueResult
		  ;   f_gensym(Env),
		      set_var(LEnv, sys_env, Env),
		      get_var(LEnv, sys_args, Args_Get21),
		      get_var(LEnv, sys_env, Env_Get20),
		      Args=[Env_Get20|Args_Get21],
		      set_var(LEnv, sys_args, Args),
		      sf_push(LEnv,
			      ['#BQ', [declare, [ignore, ['#COMMA', sys_env]]]],
			      sys_body,
			      ElseResult),
		      LetResult=ElseResult
		  ),
		  get_var(GEnv, sys_access_fn, Access_fn_Get25),
		  get_var(GEnv, sys_body, Body_Get),
		  f_sys_find_documentation(Body_Get, Setf),
		  f_sys_expand_set_documentation(Access_fn_Get25,
						 setf,
						 Setf,
						 Set_documentation_Ret),
		  get_var(GEnv, sys_access_fn, Access_fn_Get30),
		  bq_append(
			    [ 
			      [ sys_rem_sysprop,
				[quote, Access_fn_Get25],
				[quote, sys_setf_symbol]
			      ]
			    | Set_documentation_Ret
			    ],
			    [[quote, Access_fn_Get30]],
			    Bq_append_Ret)
		),
		[eval_when, [compile, load, eval], [sys_put_sysprop, [quote, Access_fn_Get25], [quote, sys_setf_method], function([lambda, ['#COMMA', sys_args], ['#BQ-COMMA-ELIPSE', sys_body]])], [sys_rem_sysprop, [quote, Access_fn_Get25], [quote, sys_setf_lambda]], [sys_rem_sysprop, [quote, Access_fn_Get25], [quote, sys_setf_update_fn]]|Bq_append_Ret]=MFResult
	      ),
	      block_exit(define_setf_expander, MFResult),
	      true).
:- set_opv(mf_define_setf_expander, type_of, sys_macro),
   set_opv(define_setf_expander, symbol_function, mf_define_setf_expander),
   DefMacroResult=define_setf_expander.
/*
:- side_effect(assert_lsp(define_setf_expander,
			  doc_string(define_setf_expander,
				     _5714,
				     function,
				     "Syntax: (define-setf-expander symbol defmacro-lambda-list {decl | doc}*\n          {form}*)\nDefines the SETF-method for generalized-variables (SYMBOL ...).\nWhen a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs\ngiven in the DEFINE-SETF-EXPANDER are evaluated in order with the parameters in\nDEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five\nvalues\n\t(var1 ... vark)\n\t(form1 ... formk)\n\t(value-var)\n\tstoring-form\n\taccess-form\nin order.  These values are collectively called the five gangs of the\ngeneralized variable (SYMBOL arg1 ... argn).  The whole SETF form is then\nexpanded into\n\t(let* ((var1 from1) ... (vark formk)\n\t       (value-var value-form))\n\t  storing-form)\nThe doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved\nby (DOCUMENTATION 'SYMBOL 'SETF)."))).
*/
/*
:- side_effect(assert_lsp(define_setf_expander,
			  lambda_def(defmacro,
				     define_setf_expander,
				     mf_define_setf_expander,
				     
				     [ sys_access_fn,
				       sys_args,
				       c38_rest,
				       sys_body
				     ],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_env,
					     
					     [ member,
					       [quote, c38_environment],
					       sys_args,
					       kw_test,
					       function(eq)
					     ]
					   ]
					 ],
					 
					 [ if,
					   sys_env,
					   
					   [ setq,
					     sys_args,
					     
					     [ cons,
					       [second, sys_env],
					       
					       [ nconc,
						 [ldiff, sys_args, sys_env],
						 [cddr, sys_env]
					       ]
					     ]
					   ],
					   
					   [ progn,
					     [setq, sys_env, [gensym]],
					     
					     [ setq,
					       sys_args,
					       [cons, sys_env, sys_args]
					     ],
					     
					     [ push,
					       
					       [ '#BQ',
						 
						 [ declare,
						   [ignore, ['#COMMA', sys_env]]
						 ]
					       ],
					       sys_body
					     ]
					   ]
					 ]
				       ],
				       
				       [ '#BQ',
					 
					 [ eval_when,
					   [compile, load, eval],
					   
					   [ sys_put_sysprop,
					     [quote, ['#COMMA', sys_access_fn]],
					     [quote, sys_setf_method],
					     function(
						      [ lambda,
							['#COMMA', sys_args],
							
							[ '#BQ-COMMA-ELIPSE',
							  sys_body
							]
						      ])
					   ],
					   
					   [ sys_rem_sysprop,
					     [quote, ['#COMMA', sys_access_fn]],
					     [quote, sys_setf_lambda]
					   ],
					   
					   [ sys_rem_sysprop,
					     [quote, ['#COMMA', sys_access_fn]],
					     [quote, sys_setf_update_fn]
					   ],
					   
					   [ sys_rem_sysprop,
					     [quote, ['#COMMA', sys_access_fn]],
					     [quote, sys_setf_symbol]
					   ],
					   
					   [ '#BQ-COMMA-ELIPSE',
					     
					     [ sys_expand_set_documentation,
					       sys_access_fn,
					       [quote, setf],
					       
					       [ sys_find_documentation,
						 sys_body
					       ]
					     ]
					   ],
					   [quote, ['#COMMA', sys_access_fn]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(define_setf_expander,
			  arglist_info(define_setf_expander,
				       mf_define_setf_expander,
				       
				       [ sys_access_fn,
					 sys_args,
					 c38_rest,
					 sys_body
				       ],
				       arginfo{ all:[sys_access_fn, sys_args],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_access_fn,
							sys_args,
							sys_body
						      ],
						opt:0,
						req:[sys_access_fn, sys_args],
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(define_setf_expander,
			  init_args(2, mf_define_setf_expander))).
*/
/*
;; GET-SETF-METHOD.
*/
/*
;; It just calls GET-SETF-METHOD-MULTIPLE-VALUE
*/
/*
;;  and checks the number of the store variable.
*/
/*
(defun get-setf-expansion (form &optional env)
  "Args: (place)
Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.
Checks if the third gang is a single-element list."
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (unless (= (list-length stores) 1)
	    (error "Multiple store-variables are not allowed."))
    (values vars vals stores store-form access-form)))


;;;; GET-SETF-METHOD-MULTIPLE-VALUE.

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/setf.lsp:3836 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-expansion',[form,'&optional',env],'$STRING'("Args: (place)\nReturns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.\nChecks if the third gang is a single-element list."),['multiple-value-bind',[vars,vals,stores,'store-form','access-form'],['get-setf-method-multiple-value',form,env],[unless,[=,['list-length',stores],1],[error,'$STRING'("Multiple store-variables are not allowed.")]],[values,vars,vals,stores,'store-form','access-form']]])
doc: doc_string(get_setf_expansion,
	      _3852,
	      function,
	      "Args: (place)\nReturns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.\nChecks if the third gang is a single-element list.").

wl:lambda_def(defun, get_setf_expansion, f_get_setf_expansion, [sys_form, c38_optional, sys_env], [[multiple_value_bind, [sys_vars, sys_vals, sys_stores, sys_store_form, sys_access_form], [sys_get_setf_method_multiple_value, sys_form, sys_env], [unless, [=, [list_length, sys_stores], 1], [error, '$ARRAY'([*], claz_base_character, "Multiple store-variables are not allowed.")]], [values, sys_vars, sys_vals, sys_stores, sys_store_form, sys_access_form]]]).
wl:arglist_info(get_setf_expansion, f_get_setf_expansion, [sys_form, c38_optional, sys_env], arginfo{all:[sys_form, sys_env], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_env], opt:[sys_env], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_get_setf_expansion).

/*

### Compiled Function: `CL:GET-SETF-EXPANSION` 
*/
f_get_setf_expansion(Form_In, RestNKeys, FnResult) :-
	CDR=[bv(sys_form, Form_In), bv(sys_env, Env_In)],
	opt_var(Env, sys_env, Env_In, true, [], 1, RestNKeys),
	catch(( ( LEnv=[bv(sys_vars, []), bv(sys_vals, []), bv(sys_stores, []), bv(sys_store_form, []), bv(sys_access_form, [])|CDR],
		  get_var(LEnv, sys_env, Env_Get),
		  get_var(LEnv, sys_form, Form_Get),
		  f_sys_get_setf_method_multiple_value(Form_Get,
						       [Env_Get],
						       Multiple_value_Ret),
		  setq_from_values(LEnv,
				   
				   [ sys_vars,
				     sys_vals,
				     sys_stores,
				     sys_store_form,
				     sys_access_form
				   ]),
		  get_var(LEnv, sys_stores, Stores_Get),
		  f_list_length(Stores_Get, PredArg1Result),
		  (   PredArg1Result=:=1
		  ->  _4038=[]
		  ;   f_error(
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Multiple store-variables are not allowed.")
			      ],
			      ElseResult),
		      _4038=ElseResult
		  ),
		  get_var(LEnv, sys_access_form, Access_form_Get),
		  ( get_var(LEnv, sys_store_form, Store_form_Get),
		    get_var(LEnv, sys_stores, Stores_Get18)
		  ),
		  get_var(LEnv, sys_vals, Vals_Get),
		  nb_setval('$mv_return',
			    
			    [ sys_vars,
			      Vals_Get,
			      Stores_Get18,
			      Store_form_Get,
			      Access_form_Get
			    ])
		),
		sys_vars=FnResult
	      ),
	      block_exit(get_setf_expansion, FnResult),
	      true).
:- set_opv(get_setf_expansion, symbol_function, f_get_setf_expansion),
   DefunResult=get_setf_expansion.
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  doc_string(get_setf_expansion,
				     _3852,
				     function,
				     "Args: (place)\nReturns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.\nChecks if the third gang is a single-element list."))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  lambda_def(defun,
				     get_setf_expansion,
				     f_get_setf_expansion,
				     [sys_form, c38_optional, sys_env],
				     
				     [ 
				       [ multiple_value_bind,
					 
					 [ sys_vars,
					   sys_vals,
					   sys_stores,
					   sys_store_form,
					   sys_access_form
					 ],
					 
					 [ sys_get_setf_method_multiple_value,
					   sys_form,
					   sys_env
					 ],
					 
					 [ unless,
					   [=, [list_length, sys_stores], 1],
					   
					   [ error,
					     '$ARRAY'([*],
						      claz_base_character,
						      "Multiple store-variables are not allowed.")
					   ]
					 ],
					 
					 [ values,
					   sys_vars,
					   sys_vals,
					   sys_stores,
					   sys_store_form,
					   sys_access_form
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(get_setf_expansion,
			  arglist_info(get_setf_expansion,
				       f_get_setf_expansion,
				       [sys_form, c38_optional, sys_env],
				       arginfo{ all:[sys_form, sys_env],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_form, sys_env],
						opt:[sys_env],
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
;;; GET-SETF-METHOD-MULTIPLE-VALUE.
*/
/*
(defun get-setf-method-multiple-value (form &optional env &aux f)
  "Args: (form)
Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.
Does not check if the third gang is a single-element list."
  (flet ((rename-arguments (vars &aux names values all-args)
	   (dolist (item vars)
	     (unless (or (fixnump item) (keywordp item))
	       (push item values)
	       (setq item (gensym))
	       (push item names))
	     (push item all-args))
	   (values (gensym) (nreverse names) (nreverse values) (nreverse all-args))))
    (cond ((and (setq f (macroexpand form env)) (not (equal f form)))
	   (return-from get-setf-method-multiple-value
	     (get-setf-method-multiple-value f env)))
	  ((symbolp form)
	 (let ((store (gensym)))
	   (values nil nil (list store) `(setq ,form ,store) form)))
	((or (not (consp form)) (not (symbolp (car form))))
	 (error "Cannot get the setf-method of "(defun get-setf-method-multiple-value (form &optional env &aux f)\n  \"Args: (form)\nReturns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.\nDoes not check if the third gang is a single-element list.\"\n  (flet ((rename-arguments (vars &aux names values all-args)\n\t   (dolist (item vars)\n\t     (unless (or (fixnump item) (keywordp item))\n\t       (push item values)\n\t       (setq item (gensym))\n\t       (push item names))\n\t     (push item all-args))\n\t   (values (gensym) (nreverse names) (nreverse values) (nreverse all-args))))\n    (cond ((and (setq f (macroexpand form env)) (not (equal f form)))\n\t   (return-from get-setf-method-multiple-value\n\t     (get-setf-method-multiple-value f env)))\n\t  ((symbolp form)\n\t (let ((store (gensym)))\n\t   (values nil nil (list store) `(setq ,form ,store) form)))\n\t((or (not (consp form)) (not (symbolp (car form))))\n\t (error \"Cannot get the setf-method of ~S.\" form))\n\t  ((setq f (get-sysprop (car form) 'SETF-METHOD))\n\t   (apply f env (cdr form)))\n\t(t\n\t   (let* ((name (car form)) writer)\n\t     (multiple-value-bind (store vars inits all)\n\t\t (rename-arguments (cdr form))\n\t       (setq writer\n\t\t     (cond ((setq f (get-sysprop name 'SETF-UPDATE-FN))\n\t\t\t    `(,f ,@all ,store))\n\t\t\t   ((setq f (get-sysprop name 'STRUCTURE-ACCESS))\n\t\t\t    (setf-structure-access (car all) (car f) (cdr f) store))\n\t\t\t   ((setq f (get-sysprop (car form) 'SETF-LAMBDA))\n\t\t\t    (apply f store all))\n\t\t\t   (t\n\t\t\t    `(funcall #'(SETF ,name) ,store ,@all))))\n\t       (values vars inits (list store) writer (cons name all))))))))\n\n;;;; SETF definitions.\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/setf.lsp:4335 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-method-multiple-value',[form,'&optional',env,'&aux',f],'$STRING'("Args: (form)\nReturns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.\nDoes not check if the third gang is a single-element list."),[flet,[['rename-arguments',[vars,'&aux',names,values,'all-args'],[dolist,[item,vars],[unless,[or,[fixnump,item],[keywordp,item]],[push,item,values],[setq,item,[gensym]],[push,item,names]],[push,item,'all-args']],[values,[gensym],[nreverse,names],[nreverse,values],[nreverse,'all-args']]]],[cond,[[and,[setq,f,[macroexpand,form,env]],[not,[equal,f,form]]],['return-from','get-setf-method-multiple-value',['get-setf-method-multiple-value',f,env]]],[[symbolp,form],[let,[[store,[gensym]]],[values,[],[],[list,store],['#BQ',[setq,['#COMMA',form],['#COMMA',store]]],form]]],[[or,[not,[consp,form]],[not,[symbolp,[car,form]]]],[error,'$STRING'("Cannot get the setf-method of ~S."),form]],[[setq,f,['get-sysprop',[car,form],[quote,'SETF-METHOD']]],[apply,f,env,[cdr,form]]],[t,['let*',[[name,[car,form]],writer],['multiple-value-bind',[store,vars,inits,all],['rename-arguments',[cdr,form]],[setq,writer,[cond,[[setq,f,['get-sysprop',name,[quote,'SETF-UPDATE-FN']]],['#BQ',[['#COMMA',f],['#BQ-COMMA-ELIPSE',all],['#COMMA',store]]]],[[setq,f,['get-sysprop',name,[quote,'STRUCTURE-ACCESS']]],['setf-structure-access',[car,all],[car,f],[cdr,f],store]],[[setq,f,['get-sysprop',[car,form],[quote,'SETF-LAMBDA']]],[apply,f,store,all]],[t,['#BQ',[funcall,function(['SETF',['#COMMA',name]]),['#COMMA',store],['#BQ-COMMA-ELIPSE',all]]]]]],[values,vars,inits,[list,store],writer,[cons,name,all]]]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_rename_arguments,
					       kw_function,
					       f_sys_rename_arguments)).
*/
doc: doc_string(sys_get_setf_method_multiple_value,
	      _6990,
	      function,
	      "Args: (form)\nReturns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.\nDoes not check if the third gang is a single-element list.").

wl:lambda_def(defun, sys_get_setf_method_multiple_value, f_sys_get_setf_method_multiple_value, [sys_form, c38_optional, sys_env, c38_aux, sys_f], [[flet, [[sys_rename_arguments, [sys_vars, c38_aux, sys_names, values, sys_all_args], [dolist, [sys_item, sys_vars], [unless, [or, [sys_fixnump, sys_item], [keywordp, sys_item]], [push, sys_item, values], [setq, sys_item, [gensym]], [push, sys_item, sys_names]], [push, sys_item, sys_all_args]], [values, [gensym], [nreverse, sys_names], [nreverse, values], [nreverse, sys_all_args]]]], [cond, [[and, [setq, sys_f, [macroexpand, sys_form, sys_env]], [not, [equal, sys_f, sys_form]]], [return_from, sys_get_setf_method_multiple_value, [sys_get_setf_method_multiple_value, sys_f, sys_env]]], [[symbolp, sys_form], [let, [[sys_store, [gensym]]], [values, [], [], [list, sys_store], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_store]]], sys_form]]], [[or, [not, [consp, sys_form]], [not, [symbolp, [car, sys_form]]]], [error, '$ARRAY'([*], claz_base_character, "Cannot get the setf-method of ~S."), sys_form]], [[setq, sys_f, [sys_get_sysprop, [car, sys_form], [quote, sys_setf_method]]], [apply, sys_f, sys_env, [cdr, sys_form]]], [t, [let_xx, [[sys_name, [car, sys_form]], sys_writer], [multiple_value_bind, [sys_store, sys_vars, sys_inits, sys_all], [sys_rename_arguments, [cdr, sys_form]], [setq, sys_writer, [cond, [[setq, sys_f, [sys_get_sysprop, sys_name, [quote, sys_setf_update_fn]]], ['#BQ', [['#COMMA', sys_f], ['#BQ-COMMA-ELIPSE', sys_all], ['#COMMA', sys_store]]]], [[setq, sys_f, [sys_get_sysprop, sys_name, [quote, sys_structure_access]]], [sys_setf_structure_access, [car, sys_all], [car, sys_f], [cdr, sys_f], sys_store]], [[setq, sys_f, [sys_get_sysprop, [car, sys_form], [quote, sys_setf_lambda]]], [apply, sys_f, sys_store, sys_all]], [t, ['#BQ', [funcall, function([setf, ['#COMMA', sys_name]]), ['#COMMA', sys_store], ['#BQ-COMMA-ELIPSE', sys_all]]]]]], [values, sys_vars, sys_inits, [list, sys_store], sys_writer, [cons, sys_name, sys_all]]]]]]]]).
wl:arglist_info(sys_get_setf_method_multiple_value, f_sys_get_setf_method_multiple_value, [sys_form, c38_optional, sys_env, c38_aux, sys_f], arginfo{all:[sys_form, sys_env], allow_other_keys:0, aux:[sys_f], body:0, complex:0, env:0, key:0, names:[sys_form, sys_env, sys_f], opt:[sys_env], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_get_setf_method_multiple_value).

/*

### Compiled Function: `SYS:GET-SETF-METHOD-MULTIPLE-VALUE` 
*/
f_sys_get_setf_method_multiple_value(Form_In, RestNKeys, FnResult) :-
	Env8=[bv(sys_form, Form_In), bv(sys_env, Env_In), bv(sys_f, In)],
	opt_var(Env, sys_env, Env_In, true, [], 1, RestNKeys),
	aux_var(Env, sys_f, In, true, []),
	catch(( ( assert_lsp(sys_rename_arguments,
			     wl:lambda_def(defun, sys_rename_arguments, f_sys_rename_arguments2, [sys_vars, c38_aux, sys_names, values, sys_all_args], [[dolist, [sys_item, sys_vars], [unless, [or, [sys_fixnump, sys_item], [keywordp, sys_item]], [push, sys_item, values], [setq, sys_item, [gensym]], [push, sys_item, sys_names]], [push, sys_item, sys_all_args]], [values, [gensym], [nreverse, sys_names], [nreverse, values], [nreverse, sys_all_args]]])),
		  assert_lsp(sys_rename_arguments,
			     wl:arglist_info(sys_rename_arguments, f_sys_rename_arguments2, [sys_vars, c38_aux, sys_names, values, sys_all_args], arginfo{all:[sys_vars], allow_other_keys:0, aux:[sys_names, values, sys_all_args], body:0, complex:0, env:0, key:0, names:[sys_vars, sys_names, values, sys_all_args], opt:0, req:[sys_vars], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_rename_arguments,
			     wl:init_args(1, f_sys_rename_arguments2)),
		  assert_lsp(sys_rename_arguments,
			     (f_sys_rename_arguments2(Vars_In, RestNKeys10, FnResult9):-GEnv=[bv(sys_vars, Vars_In), bv(sys_names, Names_In), bv(values, Values_In), bv(sys_all_args, All_args_In)], aux_var(Env8, sys_names, Names_In, true, []), aux_var(Env8, values, Values_In, true, []), aux_var(Env8, sys_all_args, All_args_In, true, []), catch(((get_var(GEnv, sys_vars, Vars_Get), BV=bv(sys_item, Ele), AEnv=[BV|GEnv], forall(member(Ele, Vars_Get),  (nb_setarg(2, BV, Ele), (get_var(AEnv, sys_item, Item_Get), f_sys_fixnump(Item_Get, FORM1_Res), FORM1_Res\==[], IFTEST=FORM1_Res->true;get_var(AEnv, sys_item, Item_Get19), f_keywordp(Item_Get19, Keywordp_Ret), IFTEST=Keywordp_Ret), (IFTEST\==[]->_7260=[];sf_push(AEnv, sys_item, values, Values), f_gensym(Item), set_var(AEnv, sys_item, Item), sf_push(AEnv, sys_item, sys_names, ElseResult), _7260=ElseResult), sf_push(AEnv, sys_item, sys_all_args, All_args))), f_gensym(Gensym_Ret), get_var(GEnv, sys_names, Names_Get), f_nreverse(Names_Get, Nreverse_Ret), get_var(GEnv, values, Values_Get), f_nreverse(Values_Get, Nreverse_Ret136), get_var(GEnv, sys_all_args, All_args_Get), f_nreverse(All_args_Get, Nreverse_Ret137), nb_setval('$mv_return', [Gensym_Ret, Nreverse_Ret, Nreverse_Ret136, Nreverse_Ret137])), Gensym_Ret=FnResult9), block_exit(sys_rename_arguments, FnResult9), true))),
		  get_var(
			  [ 
			    [ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
			    ]
			  | Env8
			  ],
			  sys_env,
			  Env_Get),
		  get_var(
			  [ 
			    [ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
			    ]
			  | Env8
			  ],
			  sys_form,
			  Form_Get),
		  f_macroexpand([Form_Get, Env_Get], IFTEST33),
		  set_var(
			  [ 
			    [ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
			    ]
			  | Env8
			  ],
			  sys_f,
			  IFTEST33),
		  (   IFTEST33\==[]
		  ->  get_var(
			      [ 
				[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
				]
			      | Env8
			      ],
			      sys_f,
			      Get),
		      get_var(
			      [ 
				[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
				]
			      | Env8
			      ],
			      sys_form,
			      Form_Get38),
		      f_equal(Get, Form_Get38, Not_Param),
		      f_not(Not_Param, TrueResult),
		      IFTEST31=TrueResult
		  ;   IFTEST31=[]
		  ),
		  (   IFTEST31\==[]
		  ->  get_var(
			      [ 
				[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
				]
			      | Env8
			      ],
			      sys_env,
			      Env_Get43),
		      get_var(
			      [ 
				[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
				]
			      | Env8
			      ],
			      sys_f,
			      Get42),
		      f_sys_get_setf_method_multiple_value(Get42,
							   [Env_Get43],
							   RetResult),
		      throw(block_exit(sys_get_setf_method_multiple_value,
				       RetResult)),
		      _7098=ThrowResult
		  ;   get_var(
			      [ 
				[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
				]
			      | Env8
			      ],
			      sys_form,
			      Form_Get45),
		      (   is_symbolp(Form_Get45)
		      ->  f_gensym(Store_Init),
			  LEnv=[bv(sys_store, Store_Init), fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)|Env8],
			  get_var(LEnv, sys_store, Store_Get),
			  CAR=[Store_Get],
			  get_var(LEnv, sys_form, Form_Get53),
			  get_var(LEnv, sys_store, Store_Get54),
			  nb_setval('$mv_return',
				    
				    [ [],
				      [],
				      CAR,
				      [setq, Form_Get53, Store_Get54],
				      Form_Get53
				    ]),
			  ElseResult117=[]
		      ;   (   get_var(
				      [ 
					[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					]
				      | Env8
				      ],
				      sys_form,
				      Form_Get58),
			      f_consp(Form_Get58, Not_Param126),
			      f_not(Not_Param126, FORM1_Res60),
			      FORM1_Res60\==[],
			      IFTEST56=FORM1_Res60
			  ->  true
			  ;   get_var(
				      [ 
					[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					]
				      | Env8
				      ],
				      sys_form,
				      Form_Get59),
			      f_car(Form_Get59, Symbolp_Param),
			      f_symbolp(Symbolp_Param, Not_Param128),
			      f_not(Not_Param128, Not_Ret),
			      IFTEST56=Not_Ret
			  ),
			  (   IFTEST56\==[]
			  ->  get_var(
				      [ 
					[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					]
				      | Env8
				      ],
				      sys_form,
				      Form_Get61),
			      f_error(
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "Cannot get the setf-method of ~S."),
					Form_Get61
				      ],
				      TrueResult113),
			      ElseResult115=TrueResult113
			  ;   get_var(
				      [ 
					[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					]
				      | Env8
				      ],
				      sys_form,
				      Form_Get64),
			      f_car(Form_Get64, Get_sysprop_Param),
			      f_sys_get_sysprop(Get_sysprop_Param,
						sys_setf_method,
						[],
						IFTEST62),
			      set_var(
				      [ 
					[ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					]
				      | Env8
				      ],
				      sys_f,
				      IFTEST62),
			      (   IFTEST62\==[]
			      ->  get_var(
					  [ 
					    [ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					    ]
					  | Env8
					  ],
					  sys_env,
					  Env_Get66),
				  get_var(
					  [ 
					    [ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					    ]
					  | Env8
					  ],
					  sys_f,
					  Get65),
				  get_var(
					  [ 
					    [ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					    ]
					  | Env8
					  ],
					  sys_form,
					  Form_Get67),
				  f_cdr(Form_Get67, Cdr_Ret),
				  f_apply(Get65,
					  [Env_Get66, Cdr_Ret],
					  TrueResult112),
				  ElseResult114=TrueResult112
			      ;   get_var(
					  [ 
					    [ fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)
					    ]
					  | Env8
					  ],
					  sys_form,
					  Form_Get71),
				  f_car(Form_Get71, Name_Init),
				  LEnv70=[bv(sys_name, Name_Init), fbound(sys_rename_arguments, kw_function)=function(f_sys_rename_arguments2)|Env8],
				  LEnv75=[bv(sys_writer, [])|LEnv70],
				  LEnv78=[bv(sys_store, []), bv(sys_vars, []), bv(sys_inits, []), bv(sys_all, [])|LEnv75],
				  get_var(LEnv78, sys_form, Form_Get79),
				  f_cdr(Form_Get79, Rename_arguments2_Param),
				  f_sys_rename_arguments2(Rename_arguments2_Param,
							  [],
							  Rename_arguments2_Ret),
				  setq_from_values(LEnv78,
						   
						   [ sys_store,
						     sys_vars,
						     sys_inits,
						     sys_all
						   ]),
				  get_var(LEnv78, sys_name, Name_Get),
				  f_sys_get_sysprop(Name_Get,
						    sys_setf_update_fn,
						    [],
						    IFTEST81),
				  set_var(LEnv78, sys_f, IFTEST81),
				  (   IFTEST81\==[]
				  ->  get_var(LEnv78, sys_all, All_Get),
				      get_var(LEnv78, sys_f, Get84),
				      get_var(LEnv78, sys_store, Store_Get86),
				      bq_append([Get84|All_Get],
						[Store_Get86],
						TrueResult105),
				      Writer=TrueResult105
				  ;   get_var(LEnv78, sys_name, Name_Get89),
				      f_sys_get_sysprop(Name_Get89,
							sys_structure_access,
							[],
							IFTEST87),
				      set_var(LEnv78, sys_f, IFTEST87),
				      (   IFTEST87\==[]
				      ->  get_var(LEnv78, sys_all, All_Get90),
					  f_car(All_Get90,
						Structure_access_Param),
					  get_var(LEnv78, sys_f, Get91),
					  f_car(Get91, Car_Ret),
					  get_var(LEnv78, sys_f, Get92),
					  f_cdr(Get92, Cdr_Ret143),
					  get_var(LEnv78,
						  sys_store,
						  Store_Get93),
					  f_sys_setf_structure_access(Structure_access_Param,
								      Car_Ret,
								      Cdr_Ret143,
								      Store_Get93,
								      TrueResult103),
					  ElseResult106=TrueResult103
				      ;   get_var(LEnv78, sys_form, Form_Get96),
					  f_car(Form_Get96,
						Get_sysprop_Param132),
					  f_sys_get_sysprop(Get_sysprop_Param132,
							    sys_setf_lambda,
							    [],
							    IFTEST94),
					  set_var(LEnv78, sys_f, IFTEST94),
					  (   IFTEST94\==[]
					  ->  get_var(LEnv78,
						      sys_all,
						      All_Get99),
					      get_var(LEnv78, sys_f, Get97),
					      get_var(LEnv78,
						      sys_store,
						      Store_Get98),
					      f_apply(Get97,
						      [Store_Get98, All_Get99],
						      TrueResult102),
					      ElseResult104=TrueResult102
					  ;   get_var(LEnv78,
						      sys_all,
						      All_Get101),
					      get_var(LEnv78,
						      sys_store,
						      Store_Get100),
					      ElseResult104=[funcall, function([setf, ['#COMMA', sys_name]]), Store_Get100|All_Get101]
					  ),
					  ElseResult106=ElseResult104
				      ),
				      Writer=ElseResult106
				  ),
				  set_var(LEnv78, sys_writer, Writer),
				  get_var(LEnv78, sys_inits, Inits_Get),
				  get_var(LEnv78, sys_store, Store_Get108),
				  CAR145=[Store_Get108],
				  get_var(LEnv78, sys_all, All_Get111),
				  get_var(LEnv78, sys_name, Name_Get110),
				  get_var(LEnv78, sys_writer, Writer_Get),
				  CAR144=[Name_Get110|All_Get111],
				  nb_setval('$mv_return',
					    
					    [ sys_vars,
					      Inits_Get,
					      CAR145,
					      Writer_Get,
					      CAR144
					    ]),
				  ElseResult114=sys_vars
			      ),
			      ElseResult115=ElseResult114
			  ),
			  ElseResult117=ElseResult115
		      ),
		      _7098=ElseResult117
		  )
		),
		_7098=FnResult
	      ),
	      block_exit(sys_get_setf_method_multiple_value, FnResult),
	      true).
:- set_opv(sys_get_setf_method_multiple_value,
	   symbol_function,
	   f_sys_get_setf_method_multiple_value),
   DefunResult=sys_get_setf_method_multiple_value.
/*
:- side_effect(assert_lsp(sys_get_setf_method_multiple_value,
			  doc_string(sys_get_setf_method_multiple_value,
				     _6990,
				     function,
				     "Args: (form)\nReturns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.\nDoes not check if the third gang is a single-element list."))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_multiple_value,
			  lambda_def(defun,
				     sys_get_setf_method_multiple_value,
				     f_sys_get_setf_method_multiple_value,
				     
				     [ sys_form,
				       c38_optional,
				       sys_env,
				       c38_aux,
				       sys_f
				     ],
				     
				     [ 
				       [ flet,
					 
					 [ 
					   [ sys_rename_arguments,
					     
					     [ sys_vars,
					       c38_aux,
					       sys_names,
					       values,
					       sys_all_args
					     ],
					     
					     [ dolist,
					       [sys_item, sys_vars],
					       
					       [ unless,
						 
						 [ or,
						   [sys_fixnump, sys_item],
						   [keywordp, sys_item]
						 ],
						 [push, sys_item, values],
						 [setq, sys_item, [gensym]],
						 [push, sys_item, sys_names]
					       ],
					       [push, sys_item, sys_all_args]
					     ],
					     
					     [ values,
					       [gensym],
					       [nreverse, sys_names],
					       [nreverse, values],
					       [nreverse, sys_all_args]
					     ]
					   ]
					 ],
					 
					 [ cond,
					   
					   [ 
					     [ and,
					       
					       [ setq,
						 sys_f,
						 
						 [ macroexpand,
						   sys_form,
						   sys_env
						 ]
					       ],
					       [not, [equal, sys_f, sys_form]]
					     ],
					     
					     [ return_from,
					       sys_get_setf_method_multiple_value,
					       
					       [ sys_get_setf_method_multiple_value,
						 sys_f,
						 sys_env
					       ]
					     ]
					   ],
					   
					   [ [symbolp, sys_form],
					     
					     [ let,
					       [[sys_store, [gensym]]],
					       
					       [ values,
						 [],
						 [],
						 [list, sys_store],
						 
						 [ '#BQ',
						   
						   [ setq,
						     ['#COMMA', sys_form],
						     ['#COMMA', sys_store]
						   ]
						 ],
						 sys_form
					       ]
					     ]
					   ],
					   
					   [ 
					     [ or,
					       [not, [consp, sys_form]],
					       [not, [symbolp, [car, sys_form]]]
					     ],
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"Cannot get the setf-method of ~S."),
					       sys_form
					     ]
					   ],
					   
					   [ 
					     [ setq,
					       sys_f,
					       
					       [ sys_get_sysprop,
						 [car, sys_form],
						 [quote, sys_setf_method]
					       ]
					     ],
					     
					     [ apply,
					       sys_f,
					       sys_env,
					       [cdr, sys_form]
					     ]
					   ],
					   
					   [ t,
					     
					     [ let_xx,
					       
					       [ [sys_name, [car, sys_form]],
						 sys_writer
					       ],
					       
					       [ multiple_value_bind,
						 
						 [ sys_store,
						   sys_vars,
						   sys_inits,
						   sys_all
						 ],
						 
						 [ sys_rename_arguments,
						   [cdr, sys_form]
						 ],
						 
						 [ setq,
						   sys_writer,
						   
						   [ cond,
						     
						     [ 
						       [ setq,
							 sys_f,
							 
							 [ sys_get_sysprop,
							   sys_name,
							   
							   [ quote,
							     sys_setf_update_fn
							   ]
							 ]
						       ],
						       
						       [ '#BQ',
							 
							 [ ['#COMMA', sys_f],
							   
							   [ '#BQ-COMMA-ELIPSE',
							     sys_all
							   ],
							   
							   [ '#COMMA',
							     sys_store
							   ]
							 ]
						       ]
						     ],
						     
						     [ 
						       [ setq,
							 sys_f,
							 
							 [ sys_get_sysprop,
							   sys_name,
							   
							   [ quote,
							     sys_structure_access
							   ]
							 ]
						       ],
						       
						       [ sys_setf_structure_access,
							 [car, sys_all],
							 [car, sys_f],
							 [cdr, sys_f],
							 sys_store
						       ]
						     ],
						     
						     [ 
						       [ setq,
							 sys_f,
							 
							 [ sys_get_sysprop,
							   [car, sys_form],
							   
							   [ quote,
							     sys_setf_lambda
							   ]
							 ]
						       ],
						       
						       [ apply,
							 sys_f,
							 sys_store,
							 sys_all
						       ]
						     ],
						     
						     [ t,
						       
						       [ '#BQ',
							 
							 [ funcall,
							   function(
								    [ setf,
								      ['#COMMA', sys_name]
								    ]),
							   
							   [ '#COMMA',
							     sys_store
							   ],
							   
							   [ '#BQ-COMMA-ELIPSE',
							     sys_all
							   ]
							 ]
						       ]
						     ]
						   ]
						 ],
						 
						 [ values,
						   sys_vars,
						   sys_inits,
						   [list, sys_store],
						   sys_writer,
						   [cons, sys_name, sys_all]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_multiple_value,
			  arglist_info(sys_get_setf_method_multiple_value,
				       f_sys_get_setf_method_multiple_value,
				       
				       [ sys_form,
					 c38_optional,
					 sys_env,
					 c38_aux,
					 sys_f
				       ],
				       arginfo{ all:[sys_form, sys_env],
						allow_other_keys:0,
						aux:[sys_f],
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_form, sys_env, sys_f],
						opt:[sys_env],
						req:[sys_form],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_get_setf_method_multiple_value,
			  init_args(1, f_sys_get_setf_method_multiple_value))).
*/
/*
;;; SETF definitions.
*/
/*
(defsetf car (x) (y) `(progn (rplaca ,x ,y) ,y))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/setf.lsp:5920 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defsetf,car,[x],[y],['#BQ',[progn,[rplaca,['#COMMA',x],['#COMMA',y]],['#COMMA',y]]]])
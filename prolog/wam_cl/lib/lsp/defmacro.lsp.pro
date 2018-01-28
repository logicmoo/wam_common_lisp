#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "lib/lsp/defmacro" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sun Jan 28 01:36:22 2018

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
;;;         defines SYS:DEFMACRO*, the defmacro preprocessor
*/
/*
(in-package #:system)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:505 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','#:system'])
/*
% macroexpand:-[in_package,system6].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))

;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defamcro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:529 **********************/
:-lisp_compile_to_prolog(pkg_sys,['eval-when',[compile],[proclaim,[quote,[optimize,[safety,2],[space,3]]]]])
:- dbginfo(skipping(
		    [ eval_when,
		      [compile],
		      [proclaim, [quote, [optimize, [safety, 2], [space, 3]]]]
		    ])).
/*
;; valid lambda-list to DEFMACRO is:
*/
/*
;;
*/
/*
;;	( [ &whole sym ]
*/
/*
;;	  [ &environment sym ]
*/
/*
;;	  { v }*
*/
/*
;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
*/
/*
;;	  {  [ { &rest | &body } v ]
*/
/*
;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
*/
/*
;;		    [ &allow-other-keys ]]
*/
/*
;;	     [ &aux { sym | ( v [ init ] ) }* ]
*/
/*
;;	  |  . sym }
*/
/*
;;	 )
*/
/*
;;
*/
/*
;; where v is short for { defmacro-lambda-list | sym }.
*/
/*
;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
*/
/*
;; (DEFMACRO <name> <symbol> ... ) is equivalent to
*/
/*
;; (DEFMACRO <name> (&REST <symbol>) ...).
*/
/*
;; Defamcro-lambda-list is defined as:
*/
/*
;;
*/
/*
;;	( { v }*
*/
/*
;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
*/
/*
;;	  {  [ { &rest | &body } v ]
*/
/*
;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
*/
/*
;;		    [ &allow-other-keys ]]
*/
/*
;;	     [ &aux { sym | ( v [ init ] ) }* ]
*/
/*
;;	  |  . sym }
*/
/*
;;	 )
*/
/*
(defvar *dl*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:1485 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*dl*'])
:- sf_defvar(Sf_defvar_Param, sys_xx_dl_xx, _Ignored).
/*
(defvar *key-check*)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:1499 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*key-check*'])
:- sf_defvar(Sf_defvar_Param, sys_xx_key_check_xx, _Ignored).
/*
(defvar *arg-check*)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:1520 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*arg-check*'])
:- sf_defvar(Sf_defvar_Param, sys_xx_arg_check_xx, _Ignored).
/*
(defmacro defmacro (name vl &rest body)
  `(multiple-value-bind (expr doc pprint)
    (sys::expand-defmacro ',name ',vl ',body)
    (sys:define-macro ',name expr doc pprint)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:1542 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defmacro,[name,vl,'&rest',body],['#BQ',['multiple-value-bind',[expr,doc,pprint],['sys::expand-defmacro',[quote,['#COMMA',name]],[quote,['#COMMA',vl]],[quote,['#COMMA',body]]],['sys:define-macro',[quote,['#COMMA',name]],expr,doc,pprint]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       defmacro,
					       kw_special,
					       sf_defmacro)).
*/
wl:lambda_def(defmacro, defmacro, mf_defmacro, [sys_name, sys_vl, c38_rest, sys_body], [['#BQ', [multiple_value_bind, [sys_expr, sys_doc, pprint], [sys_expand_defmacro, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', sys_vl]], [quote, ['#COMMA', sys_body]]], [sys_define_macro, [quote, ['#COMMA', sys_name]], sys_expr, sys_doc, pprint]]]]).
wl:arglist_info(defmacro, mf_defmacro, [sys_name, sys_vl, c38_rest, sys_body], arginfo{all:[sys_name, sys_vl], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_vl, sys_body], opt:0, req:[sys_name, sys_vl], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(2, mf_defmacro).

/*

### Compiled Macro Operator: `CL:DEFMACRO` 
*/
sf_defmacro(MacroEnv, Name_In, Vl_In, RestNKeys, FResult) :-
	mf_defmacro([defmacro, Name_In, Vl_In|RestNKeys], MacroEnv, MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFMACRO` 
*/
mf_defmacro([defmacro, Name_In, Vl_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_name, Name_In), bv(sys_vl, Vl_In), bv(sys_body, RestNKeys)],
	catch(( ( ( get_var(GEnv, sys_body, Body_Get),
		    get_var(GEnv, sys_name, Name_Get)
		  ),
		  get_var(GEnv, sys_name, Name_Get10),
		  get_var(GEnv, sys_vl, Vl_Get)
		),
		[multiple_value_bind, [sys_expr, sys_doc, pprint], [sys_expand_defmacro, [quote, Name_Get], [quote, Vl_Get], [quote, Body_Get]], [sys_define_macro, [quote, Name_Get10], sys_expr, sys_doc, pprint]]=MFResult
	      ),
	      block_exit(defmacro, MFResult),
	      true).
:- set_opv(mf_defmacro, type_of, sys_macro),
   set_opv(defmacro, symbol_function, mf_defmacro),
   DefMacroResult=defmacro.
/*
:- side_effect(assert_lsp(defmacro,
			  lambda_def(defmacro,
				     defmacro,
				     mf_defmacro,
				     [sys_name, sys_vl, c38_rest, sys_body],
				     
				     [ 
				       [ '#BQ',
					 
					 [ multiple_value_bind,
					   [sys_expr, sys_doc, pprint],
					   
					   [ sys_expand_defmacro,
					     [quote, ['#COMMA', sys_name]],
					     [quote, ['#COMMA', sys_vl]],
					     [quote, ['#COMMA', sys_body]]
					   ],
					   
					   [ sys_define_macro,
					     [quote, ['#COMMA', sys_name]],
					     sys_expr,
					     sys_doc,
					     pprint
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(defmacro,
			  arglist_info(defmacro,
				       mf_defmacro,
				       [sys_name, sys_vl, c38_rest, sys_body],
				       arginfo{ all:[sys_name, sys_vl],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_vl,
							sys_body
						      ],
						opt:0,
						req:[sys_name, sys_vl],
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defmacro, init_args(2, mf_defmacro))).
*/
/*
(defun sys::expand-defmacro (name vl body
				 &aux *dl* (*key-check* nil)
				 (*arg-check* nil)
				 doc decls whole ppn (env nil) (envp nil))
  (labels ((dm-vl (vl whole top)
	     (do ((optionalp) (restp) (keyp)
		  (allow-other-keys-p) (auxp)
		  (rest) (allow-other-keys) (keys) (no-check)
		  (n (if top 1 0)) (ppn 0) (v)
		  )
		 ((not (consp vl))
		  (when vl
		    (when restp (dm-bad-key '&rest))
		    (push (list vl (dm-nth-cdr n whole)) *dl*)
		    (setq no-check t))
		  (when (and rest (not allow-other-keys))
		    (push (cons rest keys) *key-check*))
		  (unless no-check (push (cons whole n) *arg-check*))
		  ppn
		  )
	       (declare (fixnum n ppn))
	       (setq v (car vl))
	       (cond
		 ((eq v '&optional)
		  (when optionalp (dm-bad-key '&optional))
		  (setq optionalp t)
		  (pop vl))
		 ((or (eq v '&rest) (eq v '&body))
		  (when restp (dm-bad-key v))
		  (dm-v (second vl) (dm-nth-cdr n whole))
		  (setq restp t optionalp t no-check t)
		  (setq vl (cddr vl))
		  (when (eq v '&body)
		    (setq ppn (if top (the fixnum (1- n)) n))))
		 ((eq v '&key)
		  (when keyp (dm-bad-key '&key))
		  (setq rest (gensym))
		  (push (list rest (dm-nth-cdr n whole)) *dl*)
		  (setq keyp t restp t optionalp t no-check t)
		  (pop vl))
		 ((eq v '&allow-other-keys)
		  (when (or (not keyp) allow-other-keys-p)
		    (dm-bad-key '&allow-other-keys))
		  (setq allow-other-keys-p t)
		  (setq allow-other-keys t)
		  (pop vl))
		 ((eq v '&aux)
		  (when auxp (dm-bad-key '&aux))
		  (setq auxp t allow-other-keys-p t keyp t restp t optionalp t)
		  (pop vl))
		 (auxp
		  (let (x (init nil))
		    (cond ((symbolp v) (setq x v))
			  (t (setq x (car v))
			     (unless (endp (cdr v)) (setq init (second v)))))
		    (dm-v x init))
		  (pop vl))
		 (keyp
		  (let ((temp (gensym)) x k (init nil) (sv nil))
		    (cond ((symbolp v) (setq x v
					     k (intern (string v) 'keyword)))
			  (t (if (symbolp (car v))
				 (setq x (car v)
				       k (intern (string (car v)) 'keyword))
				 (setq x (cadar v) k (caar v)))
			     (unless (endp (cdr v))
			       (setq init (second v))
			       (unless (endp (cddr v))
				 (setq sv (caddr v))))))
		    (dm-v temp `(getf ,rest ,k 'failed))
		    (dm-v x `(if (eq ,temp 'failed) ,init ,temp))
		    (when sv (dm-v sv `(not (eq ,temp 'failed))))
		    (push k keys))
		  (pop vl))
		 (optionalp
		  (let (x (init nil) (sv nil))
		    (cond ((symbolp v) (setq x v))
			  (t (setq x (car v))
			     (unless (endp (cdr v))
			       (setq init (second v))
			       (unless (endp (cddr v))
				 (setq sv (caddr v))))))
		    (dm-v x `(if ,(dm-nth-cdr n whole) ,(dm-nth n whole) ,init))
		    (when sv (dm-v sv `(not (null ,(dm-nth-cdr n whole))))))
		  (incf n)
		  (pop vl)
		  )
		 (t (dm-v v `(if ,(dm-nth-cdr n whole)
			      ,(dm-nth n whole)
			      (dm-too-few-arguments)))
		    (incf n)
		    (pop vl))
		 )))
	   (dm-v (v init)
	     (if (symbolp v)
		 (push (if init (list v init) v) *dl*)
		 (let ((temp (gensym)))
		   (push (if init (list temp init) temp) *dl*)
		   (dm-vl v temp nil))))

	   (dm-nth (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 (list 'CAR v))
		 (1 (list 'CADR v))
		 (2 (list 'CADDR v))
		 (3 (list 'CADDDR v))
		 )))

	   (dm-nth-cdr (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 v)
		 (1 (list 'CDR v))
		 (2 (list 'CDDR v))
		 (3 (list 'CDDDR v))
		 )))
	   )
  (cond ((listp vl))
        ((symbolp vl) (setq vl (list '&rest vl)))
        (t (error "The defmacro-lambda-list fmt90_x1 is not a list." vl)))
  (multiple-value-setq (doc decls body) (find-doc body nil))
  (if (and (listp vl) (eq (car vl) '&whole))
      (setq whole (second vl)
	    vl (cddr vl))
      (setq whole (gensym)))
  (if (setq env (member '&environment vl :test #'eq))
      (setq vl (nconc (ldiff vl env) (cddr env))
	    env (second env))
      (progn
	(setq env (gensym))
	(push `(DECLARE (ignore ,env)) decls)))
  (setq *dl* `(&aux ,env ,whole))
  (setq ppn (dm-vl vl whole t))
  (dolist (kc *key-check*)
          (push `(unless (getf ,(car kc) :allow-other-keys)
                         (do ((vl ,(car kc) (cddr vl)))
                             ((endp vl))
                             (unless (member (car vl) ',(cdr kc))
                                     (dm-key-not-allowed (car vl))
                                     )))
                body))
  (dolist (ac *arg-check*)
          (push `(unless (endp ,(dm-nth-cdr (cdr ac) (car ac)))
                         (dm-too-many-arguments)) body))
  (values `(LAMBDA-BLOCK ,name ,(nreverse *dl*) ,@(nconc decls body))
      doc ppn))
  )

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:1719 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'sys::expand-defmacro',[name,vl,body,'&aux','*dl*',['*key-check*',[]],['*arg-check*',[]],doc,decls,whole,ppn,[env,[]],[envp,[]]],[labels,[['dm-vl',[vl,whole,top],[do,[[optionalp],[restp],[keyp],['allow-other-keys-p'],[auxp],[rest],['allow-other-keys'],[keys],['no-check'],[n,[if,top,1,0]],[ppn,0],[v]],[[not,[consp,vl]],[when,vl,[when,restp,['dm-bad-key',[quote,'&rest']]],[push,[list,vl,['dm-nth-cdr',n,whole]],'*dl*'],[setq,'no-check',t]],[when,[and,rest,[not,'allow-other-keys']],[push,[cons,rest,keys],'*key-check*']],[unless,'no-check',[push,[cons,whole,n],'*arg-check*']],ppn],[declare,[fixnum,n,ppn]],[setq,v,[car,vl]],[cond,[[eq,v,[quote,'&optional']],[when,optionalp,['dm-bad-key',[quote,'&optional']]],[setq,optionalp,t],[pop,vl]],[[or,[eq,v,[quote,'&rest']],[eq,v,[quote,'&body']]],[when,restp,['dm-bad-key',v]],['dm-v',[second,vl],['dm-nth-cdr',n,whole]],[setq,restp,t,optionalp,t,'no-check',t],[setq,vl,[cddr,vl]],[when,[eq,v,[quote,'&body']],[setq,ppn,[if,top,[the,fixnum,['1-',n]],n]]]],[[eq,v,[quote,'&key']],[when,keyp,['dm-bad-key',[quote,'&key']]],[setq,rest,[gensym]],[push,[list,rest,['dm-nth-cdr',n,whole]],'*dl*'],[setq,keyp,t,restp,t,optionalp,t,'no-check',t],[pop,vl]],[[eq,v,[quote,'&allow-other-keys']],[when,[or,[not,keyp],'allow-other-keys-p'],['dm-bad-key',[quote,'&allow-other-keys']]],[setq,'allow-other-keys-p',t],[setq,'allow-other-keys',t],[pop,vl]],[[eq,v,[quote,'&aux']],[when,auxp,['dm-bad-key',[quote,'&aux']]],[setq,auxp,t,'allow-other-keys-p',t,keyp,t,restp,t,optionalp,t],[pop,vl]],[auxp,[let,[x,[init,[]]],[cond,[[symbolp,v],[setq,x,v]],[t,[setq,x,[car,v]],[unless,[endp,[cdr,v]],[setq,init,[second,v]]]]],['dm-v',x,init]],[pop,vl]],[keyp,[let,[[temp,[gensym]],x,k,[init,[]],[sv,[]]],[cond,[[symbolp,v],[setq,x,v,k,[intern,[string,v],[quote,keyword]]]],[t,[if,[symbolp,[car,v]],[setq,x,[car,v],k,[intern,[string,[car,v]],[quote,keyword]]],[setq,x,[cadar,v],k,[caar,v]]],[unless,[endp,[cdr,v]],[setq,init,[second,v]],[unless,[endp,[cddr,v]],[setq,sv,[caddr,v]]]]]],['dm-v',temp,['#BQ',[getf,['#COMMA',rest],['#COMMA',k],[quote,failed]]]],['dm-v',x,['#BQ',[if,[eq,['#COMMA',temp],[quote,failed]],['#COMMA',init],['#COMMA',temp]]]],[when,sv,['dm-v',sv,['#BQ',[not,[eq,['#COMMA',temp],[quote,failed]]]]]],[push,k,keys]],[pop,vl]],[optionalp,[let,[x,[init,[]],[sv,[]]],[cond,[[symbolp,v],[setq,x,v]],[t,[setq,x,[car,v]],[unless,[endp,[cdr,v]],[setq,init,[second,v]],[unless,[endp,[cddr,v]],[setq,sv,[caddr,v]]]]]],['dm-v',x,['#BQ',[if,['#COMMA',['dm-nth-cdr',n,whole]],['#COMMA',['dm-nth',n,whole]],['#COMMA',init]]]],[when,sv,['dm-v',sv,['#BQ',[not,[null,['#COMMA',['dm-nth-cdr',n,whole]]]]]]]],[incf,n],[pop,vl]],[t,['dm-v',v,['#BQ',[if,['#COMMA',['dm-nth-cdr',n,whole]],['#COMMA',['dm-nth',n,whole]],['dm-too-few-arguments']]]],[incf,n],[pop,vl]]]]],['dm-v',[v,init],[if,[symbolp,v],[push,[if,init,[list,v,init],v],'*dl*'],[let,[[temp,[gensym]]],[push,[if,init,[list,temp,init],temp],'*dl*'],['dm-vl',v,temp,[]]]]],['dm-nth',[n,v],['multiple-value-bind',[q,r],[floor,n,4],[declare,[fixnum,q,r]],[dotimes,[i,q],[setq,v,[list,[quote,'CDDDDR'],v]]],[case,r,[0,[list,[quote,'CAR'],v]],[1,[list,[quote,'CADR'],v]],[2,[list,[quote,'CADDR'],v]],[3,[list,[quote,'CADDDR'],v]]]]],['dm-nth-cdr',[n,v],['multiple-value-bind',[q,r],[floor,n,4],[declare,[fixnum,q,r]],[dotimes,[i,q],[setq,v,[list,[quote,'CDDDDR'],v]]],[case,r,[0,v],[1,[list,[quote,'CDR'],v]],[2,[list,[quote,'CDDR'],v]],[3,[list,[quote,'CDDDR'],v]]]]]],[cond,[[listp,vl]],[[symbolp,vl],[setq,vl,[list,[quote,'&rest'],vl]]],[t,[error,'$STRING'("The defmacro-lambda-list ~s is not a list."),vl]]],['multiple-value-setq',[doc,decls,body],['find-doc',body,[]]],[if,[and,[listp,vl],[eq,[car,vl],[quote,'&whole']]],[setq,whole,[second,vl],vl,[cddr,vl]],[setq,whole,[gensym]]],[if,[setq,env,[member,[quote,'&environment'],vl,':test',function(eq)]],[setq,vl,[nconc,[ldiff,vl,env],[cddr,env]],env,[second,env]],[progn,[setq,env,[gensym]],[push,['#BQ',['DECLARE',[ignore,['#COMMA',env]]]],decls]]],[setq,'*dl*',['#BQ',['&aux',['#COMMA',env],['#COMMA',whole]]]],[setq,ppn,['dm-vl',vl,whole,t]],[dolist,[kc,'*key-check*'],[push,['#BQ',[unless,[getf,['#COMMA',[car,kc]],':allow-other-keys'],[do,[[vl,['#COMMA',[car,kc]],[cddr,vl]]],[[endp,vl]],[unless,[member,[car,vl],[quote,['#COMMA',[cdr,kc]]]],['dm-key-not-allowed',[car,vl]]]]]],body]],[dolist,[ac,'*arg-check*'],[push,['#BQ',[unless,[endp,['#COMMA',['dm-nth-cdr',[cdr,ac],[car,ac]]]],['dm-too-many-arguments']]],body]],[values,['#BQ',['LAMBDA-BLOCK',['#COMMA',name],['#COMMA',[nreverse,'*dl*']],['#BQ-COMMA-ELIPSE',[nconc,decls,body]]]],doc,ppn]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_vl,
					       kw_function,
					       f_sys_dm_vl)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth,
					       kw_function,
					       f_sys_dm_nth)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth,
					       kw_function,
					       f_sys_dm_nth)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth,
					       kw_function,
					       f_sys_dm_nth)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth,
					       kw_function,
					       f_sys_dm_nth)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_v,
					       kw_function,
					       f_sys_dm_v)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_vl,
					       kw_function,
					       f_sys_dm_vl)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth,
					       kw_function,
					       f_sys_dm_nth)).
*/
/*
% case:-[[0,[list,[quote,car],sys_v]],[1,[list,[quote,cadr],sys_v]],[2,[list,[quote,caddr],sys_v]],[3,[list,[quote,cadddr],sys_v]]].
*/
/*
% conds:-[[[eq,_204082,[quote,0]],[progn,[list,[quote,car],sys_v]]],[[eq,_204082,[quote,1]],[progn,[list,[quote,cadr],sys_v]]],[[eq,_204082,[quote,2]],[progn,[list,[quote,caddr],sys_v]]],[[eq,_204082,[quote,3]],[progn,[list,[quote,cadddr],sys_v]]]].
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dm_nth_cdr,
					       kw_function,
					       f_sys_dm_nth_cdr)).
*/
/*
% case:-[[0,sys_v],[1,[list,[quote,cdr],sys_v]],[2,[list,[quote,cddr],sys_v]],[3,[list,[quote,cdddr],sys_v]]].
*/
/*
% conds:-[[[eq,_208728,[quote,0]],[progn,sys_v]],[[eq,_208728,[quote,1]],[progn,[list,[quote,cdr],sys_v]]],[[eq,_208728,[quote,2]],[progn,[list,[quote,cddr],sys_v]]],[[eq,_208728,[quote,3]],[progn,[list,[quote,cdddr],sys_v]]]].
*/
wl:lambda_def(defun, sys_expand_defmacro, f_sys_expand_defmacro, [sys_name, sys_vl, sys_body, c38_aux, sys_xx_dl_xx, [sys_xx_key_check_xx, []], [sys_xx_arg_check_xx, []], sys_doc, sys_decls, sys_whole, sys_ppn, [sys_env, []], [sys_envp, []]], [[labels, [[sys_dm_vl, [sys_vl, sys_whole, sys_top], [do, [[sys_optionalp], [sys_restp], [sys_keyp], [sys_allow_other_keys_p], [sys_auxp], [rest], [sys_allow_other_keys], [sys_keys], [sys_no_check], [sys_n, [if, sys_top, 1, 0]], [sys_ppn, 0], [sys_v]], [[not, [consp, sys_vl]], [when, sys_vl, [when, sys_restp, [sys_dm_bad_key, [quote, c38_rest]]], [push, [list, sys_vl, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx], [setq, sys_no_check, t]], [when, [and, rest, [not, sys_allow_other_keys]], [push, [cons, rest, sys_keys], sys_xx_key_check_xx]], [unless, sys_no_check, [push, [cons, sys_whole, sys_n], sys_xx_arg_check_xx]], sys_ppn], [declare, [fixnum, sys_n, sys_ppn]], [setq, sys_v, [car, sys_vl]], [cond, [[eq, sys_v, [quote, c38_optional]], [when, sys_optionalp, [sys_dm_bad_key, [quote, c38_optional]]], [setq, sys_optionalp, t], [pop, sys_vl]], [[or, [eq, sys_v, [quote, c38_rest]], [eq, sys_v, [quote, c38_body]]], [when, sys_restp, [sys_dm_bad_key, sys_v]], [sys_dm_v, [second, sys_vl], [sys_dm_nth_cdr, sys_n, sys_whole]], [setq, sys_restp, t, sys_optionalp, t, sys_no_check, t], [setq, sys_vl, [cddr, sys_vl]], [when, [eq, sys_v, [quote, c38_body]], [setq, sys_ppn, [if, sys_top, [the, fixnum, ['1-', sys_n]], sys_n]]]], [[eq, sys_v, [quote, c38_key]], [when, sys_keyp, [sys_dm_bad_key, [quote, c38_key]]], [setq, rest, [gensym]], [push, [list, rest, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx], [setq, sys_keyp, t, sys_restp, t, sys_optionalp, t, sys_no_check, t], [pop, sys_vl]], [[eq, sys_v, [quote, c38_allow_other_keys]], [when, [or, [not, sys_keyp], sys_allow_other_keys_p], [sys_dm_bad_key, [quote, c38_allow_other_keys]]], [setq, sys_allow_other_keys_p, t], [setq, sys_allow_other_keys, t], [pop, sys_vl]], [[eq, sys_v, [quote, c38_aux]], [when, sys_auxp, [sys_dm_bad_key, [quote, c38_aux]]], [setq, sys_auxp, t, sys_allow_other_keys_p, t, sys_keyp, t, sys_restp, t, sys_optionalp, t], [pop, sys_vl]], [sys_auxp, [let, [sys_x, [sys_init, []]], [cond, [[symbolp, sys_v], [setq, sys_x, sys_v]], [t, [setq, sys_x, [car, sys_v]], [unless, [endp, [cdr, sys_v]], [setq, sys_init, [second, sys_v]]]]], [sys_dm_v, sys_x, sys_init]], [pop, sys_vl]], [sys_keyp, [let, [[sys_temp, [gensym]], sys_x, sys_k, [sys_init, []], [sys_sv, []]], [cond, [[symbolp, sys_v], [setq, sys_x, sys_v, sys_k, [intern, [string, sys_v], [quote, keyword]]]], [t, [if, [symbolp, [car, sys_v]], [setq, sys_x, [car, sys_v], sys_k, [intern, [string, [car, sys_v]], [quote, keyword]]], [setq, sys_x, [cadar, sys_v], sys_k, [caar, sys_v]]], [unless, [endp, [cdr, sys_v]], [setq, sys_init, [second, sys_v]], [unless, [endp, [cddr, sys_v]], [setq, sys_sv, [caddr, sys_v]]]]]], [sys_dm_v, sys_temp, ['#BQ', [getf, ['#COMMA', rest], ['#COMMA', sys_k], [quote, sys_failed]]]], [sys_dm_v, sys_x, ['#BQ', [if, [eq, ['#COMMA', sys_temp], [quote, sys_failed]], ['#COMMA', sys_init], ['#COMMA', sys_temp]]]], [when, sys_sv, [sys_dm_v, sys_sv, ['#BQ', [not, [eq, ['#COMMA', sys_temp], [quote, sys_failed]]]]]], [push, sys_k, sys_keys]], [pop, sys_vl]], [sys_optionalp, [let, [sys_x, [sys_init, []], [sys_sv, []]], [cond, [[symbolp, sys_v], [setq, sys_x, sys_v]], [t, [setq, sys_x, [car, sys_v]], [unless, [endp, [cdr, sys_v]], [setq, sys_init, [second, sys_v]], [unless, [endp, [cddr, sys_v]], [setq, sys_sv, [caddr, sys_v]]]]]], [sys_dm_v, sys_x, ['#BQ', [if, ['#COMMA', [sys_dm_nth_cdr, sys_n, sys_whole]], ['#COMMA', [sys_dm_nth, sys_n, sys_whole]], ['#COMMA', sys_init]]]], [when, sys_sv, [sys_dm_v, sys_sv, ['#BQ', [not, [null, ['#COMMA', [sys_dm_nth_cdr, sys_n, sys_whole]]]]]]]], [incf, sys_n], [pop, sys_vl]], [t, [sys_dm_v, sys_v, ['#BQ', [if, ['#COMMA', [sys_dm_nth_cdr, sys_n, sys_whole]], ['#COMMA', [sys_dm_nth, sys_n, sys_whole]], [sys_dm_too_few_arguments]]]], [incf, sys_n], [pop, sys_vl]]]]], [sys_dm_v, [sys_v, sys_init], [if, [symbolp, sys_v], [push, [if, sys_init, [list, sys_v, sys_init], sys_v], sys_xx_dl_xx], [let, [[sys_temp, [gensym]]], [push, [if, sys_init, [list, sys_temp, sys_init], sys_temp], sys_xx_dl_xx], [sys_dm_vl, sys_v, sys_temp, []]]]], [sys_dm_nth, [sys_n, sys_v], [multiple_value_bind, [sys_q, sys_r], [floor, sys_n, 4], [declare, [fixnum, sys_q, sys_r]], [dotimes, [sys_i, sys_q], [setq, sys_v, [list, [quote, cddddr], sys_v]]], [case, sys_r, [0, [list, [quote, car], sys_v]], [1, [list, [quote, cadr], sys_v]], [2, [list, [quote, caddr], sys_v]], [3, [list, [quote, cadddr], sys_v]]]]], [sys_dm_nth_cdr, [sys_n, sys_v], [multiple_value_bind, [sys_q, sys_r], [floor, sys_n, 4], [declare, [fixnum, sys_q, sys_r]], [dotimes, [sys_i, sys_q], [setq, sys_v, [list, [quote, cddddr], sys_v]]], [case, sys_r, [0, sys_v], [1, [list, [quote, cdr], sys_v]], [2, [list, [quote, cddr], sys_v]], [3, [list, [quote, cdddr], sys_v]]]]]], [cond, [[listp, sys_vl]], [[symbolp, sys_vl], [setq, sys_vl, [list, [quote, c38_rest], sys_vl]]], [t, [error, '$ARRAY'([*], claz_base_character, "The defmacro-lambda-list ~s is not a list."), sys_vl]]], [multiple_value_setq, [sys_doc, sys_decls, sys_body], [sys_find_doc, sys_body, []]], [if, [and, [listp, sys_vl], [eq, [car, sys_vl], [quote, c38_whole]]], [setq, sys_whole, [second, sys_vl], sys_vl, [cddr, sys_vl]], [setq, sys_whole, [gensym]]], [if, [setq, sys_env, [member, [quote, c38_environment], sys_vl, kw_test, function(eq)]], [setq, sys_vl, [nconc, [ldiff, sys_vl, sys_env], [cddr, sys_env]], sys_env, [second, sys_env]], [progn, [setq, sys_env, [gensym]], [push, ['#BQ', [declare, [ignore, ['#COMMA', sys_env]]]], sys_decls]]], [setq, sys_xx_dl_xx, ['#BQ', [c38_aux, ['#COMMA', sys_env], ['#COMMA', sys_whole]]]], [setq, sys_ppn, [sys_dm_vl, sys_vl, sys_whole, t]], [dolist, [sys_kc, sys_xx_key_check_xx], [push, ['#BQ', [unless, [getf, ['#COMMA', [car, sys_kc]], kw_allow_other_keys], [do, [[sys_vl, ['#COMMA', [car, sys_kc]], [cddr, sys_vl]]], [[endp, sys_vl]], [unless, [member, [car, sys_vl], [quote, ['#COMMA', [cdr, sys_kc]]]], [sys_dm_key_not_allowed, [car, sys_vl]]]]]], sys_body]], [dolist, [sys_ac, sys_xx_arg_check_xx], [push, ['#BQ', [unless, [endp, ['#COMMA', [sys_dm_nth_cdr, [cdr, sys_ac], [car, sys_ac]]]], [sys_dm_too_many_arguments]]], sys_body]], [values, ['#BQ', [sys_lambda_block, ['#COMMA', sys_name], ['#COMMA', [nreverse, sys_xx_dl_xx]], ['#BQ-COMMA-ELIPSE', [nconc, sys_decls, sys_body]]]], sys_doc, sys_ppn]]]).
wl:arglist_info(sys_expand_defmacro, f_sys_expand_defmacro, [sys_name, sys_vl, sys_body, c38_aux, sys_xx_dl_xx, [sys_xx_key_check_xx, []], [sys_xx_arg_check_xx, []], sys_doc, sys_decls, sys_whole, sys_ppn, [sys_env, []], [sys_envp, []]], arginfo{all:[sys_name, sys_vl, sys_body], allow_other_keys:0, aux:[sys_xx_dl_xx, sys_xx_key_check_xx, sys_xx_arg_check_xx, sys_doc, sys_decls, sys_whole, sys_ppn, sys_env, sys_envp], body:0, complex:0, env:0, key:0, names:[sys_name, sys_vl, sys_body, sys_xx_dl_xx, sys_xx_key_check_xx, sys_xx_arg_check_xx, sys_doc, sys_decls, sys_whole, sys_ppn, sys_env, sys_envp], opt:0, req:[sys_name, sys_vl, sys_body], rest:0, sublists:0, whole:0}).
wl: init_args(3, f_sys_expand_defmacro).

/*

### Compiled Function: `SYS::EXPAND-DEFMACRO` 
*/
f_sys_expand_defmacro(Name_In, Vl_In, Body_In, RestNKeys, FnResult) :-
	Env=[bv(sys_name, Name_In), bv(sys_vl, Vl_In), bv(sys_body, Body_In), bv(sys_xx_dl_xx, Xx_dl_xx_In), bv(sys_xx_key_check_xx, Xx_key_check_xx_In), bv(sys_xx_arg_check_xx, Xx_arg_check_xx_In), bv(sys_doc, Doc_In), bv(sys_decls, Decls_In), bv(sys_whole, Whole_In), bv(sys_ppn, Ppn_In), bv(sys_env, Env_In), bv(sys_envp, Envp_In)],
	aux_var(Env, sys_xx_dl_xx, Xx_dl_xx_In, true, []),
	aux_var(Env, sys_xx_key_check_xx, Xx_key_check_xx_In, true, []),
	aux_var(Env, sys_xx_arg_check_xx, Xx_arg_check_xx_In, true, []),
	aux_var(Env, sys_doc, Doc_In, true, []),
	aux_var(Env, sys_decls, Decls_In, true, []),
	aux_var(Env, sys_whole, Whole_In, true, []),
	aux_var(Env, sys_ppn, Ppn_In, true, []),
	aux_var(Env, sys_env, Env_In, true, []),
	aux_var(Env, sys_envp, Envp_In, true, []),
	catch(( ( assert_lsp(sys_dm_vl,
			     wl:lambda_def(defun, sys_dm_vl, f_sys_dm_vl5, [sys_vl, sys_whole, sys_top], [[do, [[sys_optionalp], [sys_restp], [sys_keyp], [sys_allow_other_keys_p], [sys_auxp], [rest], [sys_allow_other_keys], [sys_keys], [sys_no_check], [sys_n, [if, sys_top, 1, 0]], [sys_ppn, 0], [sys_v]], [[not, [consp, sys_vl]], [when, sys_vl, [when, sys_restp, [sys_dm_bad_key, [quote, c38_rest]]], [push, [list, sys_vl, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx], [setq, sys_no_check, t]], [when, [and, rest, [not, sys_allow_other_keys]], [push, [cons, rest, sys_keys], sys_xx_key_check_xx]], [unless, sys_no_check, [push, [cons, sys_whole, sys_n], sys_xx_arg_check_xx]], sys_ppn], [declare, [fixnum, sys_n, sys_ppn]], [setq, sys_v, [car, sys_vl]], [cond, [[eq, sys_v, [quote, c38_optional]], [when, sys_optionalp, [sys_dm_bad_key, [quote, c38_optional]]], [setq, sys_optionalp, t], [pop, sys_vl]], [[or, [eq, sys_v, [quote, c38_rest]], [eq, sys_v, [quote, c38_body]]], [when, sys_restp, [sys_dm_bad_key, sys_v]], [sys_dm_v, [second, sys_vl], [sys_dm_nth_cdr, sys_n, sys_whole]], [setq, sys_restp, t, sys_optionalp, t, sys_no_check, t], [setq, sys_vl, [cddr, sys_vl]], [when, [eq, sys_v, [quote, c38_body]], [setq, sys_ppn, [if, sys_top, [the, fixnum, ['1-', sys_n]], sys_n]]]], [[eq, sys_v, [quote, c38_key]], [when, sys_keyp, [sys_dm_bad_key, [quote, c38_key]]], [setq, rest, [gensym]], [push, [list, rest, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx], [setq, sys_keyp, t, sys_restp, t, sys_optionalp, t, sys_no_check, t], [pop, sys_vl]], [[eq, sys_v, [quote, c38_allow_other_keys]], [when, [or, [not, sys_keyp], sys_allow_other_keys_p], [sys_dm_bad_key, [quote, c38_allow_other_keys]]], [setq, sys_allow_other_keys_p, t], [setq, sys_allow_other_keys, t], [pop, sys_vl]], [[eq, sys_v, [quote, c38_aux]], [when, sys_auxp, [sys_dm_bad_key, [quote, c38_aux]]], [setq, sys_auxp, t, sys_allow_other_keys_p, t, sys_keyp, t, sys_restp, t, sys_optionalp, t], [pop, sys_vl]], [sys_auxp, [let, [sys_x, [sys_init, []]], [cond, [[symbolp, sys_v], [setq, sys_x, sys_v]], [t, [setq, sys_x, [car, sys_v]], [unless, [endp, [cdr, sys_v]], [setq, sys_init, [second, sys_v]]]]], [sys_dm_v, sys_x, sys_init]], [pop, sys_vl]], [sys_keyp, [let, [[sys_temp, [gensym]], sys_x, sys_k, [sys_init, []], [sys_sv, []]], [cond, [[symbolp, sys_v], [setq, sys_x, sys_v, sys_k, [intern, [string, sys_v], [quote, keyword]]]], [t, [if, [symbolp, [car, sys_v]], [setq, sys_x, [car, sys_v], sys_k, [intern, [string, [car, sys_v]], [quote, keyword]]], [setq, sys_x, [cadar, sys_v], sys_k, [caar, sys_v]]], [unless, [endp, [cdr, sys_v]], [setq, sys_init, [second, sys_v]], [unless, [endp, [cddr, sys_v]], [setq, sys_sv, [caddr, sys_v]]]]]], [sys_dm_v, sys_temp, ['#BQ', [getf, ['#COMMA', rest], ['#COMMA', sys_k], [quote, sys_failed]]]], [sys_dm_v, sys_x, ['#BQ', [if, [eq, ['#COMMA', sys_temp], [quote, sys_failed]], ['#COMMA', sys_init], ['#COMMA', sys_temp]]]], [when, sys_sv, [sys_dm_v, sys_sv, ['#BQ', [not, [eq, ['#COMMA', sys_temp], [quote, sys_failed]]]]]], [push, sys_k, sys_keys]], [pop, sys_vl]], [sys_optionalp, [let, [sys_x, [sys_init, []], [sys_sv, []]], [cond, [[symbolp, sys_v], [setq, sys_x, sys_v]], [t, [setq, sys_x, [car, sys_v]], [unless, [endp, [cdr, sys_v]], [setq, sys_init, [second, sys_v]], [unless, [endp, [cddr, sys_v]], [setq, sys_sv, [caddr, sys_v]]]]]], [sys_dm_v, sys_x, ['#BQ', [if, ['#COMMA', [sys_dm_nth_cdr, sys_n, sys_whole]], ['#COMMA', [sys_dm_nth, sys_n, sys_whole]], ['#COMMA', sys_init]]]], [when, sys_sv, [sys_dm_v, sys_sv, ['#BQ', [not, [null, ['#COMMA', [sys_dm_nth_cdr, sys_n, sys_whole]]]]]]]], [incf, sys_n], [pop, sys_vl]], [t, [sys_dm_v, sys_v, ['#BQ', [if, ['#COMMA', [sys_dm_nth_cdr, sys_n, sys_whole]], ['#COMMA', [sys_dm_nth, sys_n, sys_whole]], [sys_dm_too_few_arguments]]]], [incf, sys_n], [pop, sys_vl]]]]])),
		  assert_lsp(sys_dm_vl,
			     wl:arglist_info(sys_dm_vl, f_sys_dm_vl5, [sys_vl, sys_whole, sys_top], arginfo{all:[sys_vl, sys_whole, sys_top], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_vl, sys_whole, sys_top], opt:0, req:[sys_vl, sys_whole, sys_top], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_dm_vl, wl:init_args(3, f_sys_dm_vl5)),
		  assert_lsp(sys_dm_vl,
			     (f_sys_dm_vl5(Vl_In19, Whole_In20, Top_In, RestNKeys18, FnResult17):-GEnv=[bv(sys_vl, Vl_In19), bv(sys_whole, Whole_In20), bv(sys_top, Top_In)], catch(((get_var(GEnv, sys_top, IFTEST), (IFTEST\==[]->N_Init=1;N_Init=0), BlockExitEnv=[bv([sys_optionalp], []), bv([sys_restp], []), bv([sys_keyp], []), bv([sys_allow_other_keys_p], []), bv([sys_auxp], []), bv([rest], []), bv([sys_allow_other_keys], []), bv([sys_keys], []), bv([sys_no_check], []), bv(sys_n, N_Init), bv(sys_ppn, 0), bv([sys_v], [])|GEnv], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_5), get_var(BlockExitEnv, sys_vl, Vl_Get260), f_consp(Vl_Get260, PredArgResult262), (PredArgResult262==[]->get_var(BlockExitEnv, sys_vl, IFTEST265), (IFTEST265\==[]->get_var(BlockExitEnv, sys_restp, IFTEST268), (IFTEST268\==[]->f_sys_dm_bad_key(c38_rest, TrueResult271), _23554=TrueResult271;_23554=[]), sf_push(BlockExitEnv, [list, sys_vl, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx, Xx_dl_xx), set_var(BlockExitEnv, sys_no_check, t), _23480=t;_23480=[]), get_var(BlockExitEnv, rest, IFTEST275), (IFTEST275\==[]->get_var(BlockExitEnv, sys_allow_other_keys, Allow_other_keys_Get278), f_not(Allow_other_keys_Get278, TrueResult279), IFTEST273=TrueResult279;IFTEST273=[]), (IFTEST273\==[]->sf_push(BlockExitEnv, [cons, rest, sys_keys], sys_xx_key_check_xx, TrueResult280), _23684=TrueResult280;_23684=[]), get_var(BlockExitEnv, sys_no_check, IFTEST281), (IFTEST281\==[]->_23884=[];sf_push(BlockExitEnv, [cons, sys_whole, sys_n], sys_xx_arg_check_xx, ElseResult284), _23884=ElseResult284), get_var(BlockExitEnv, sys_ppn, RetResult263), throw(block_exit([], RetResult263)), _TBResult=ThrowResult264;sf_declare(BlockExitEnv, [fixnum, sys_n, sys_ppn], Sf_declare_Ret), get_var(BlockExitEnv, sys_vl, Vl_Get287), f_car(Vl_Get287, V), set_var(BlockExitEnv, sys_v, V), get_var(BlockExitEnv, sys_v, V_Get289), (is_eq(V_Get289, c38_optional)->get_var(BlockExitEnv, sys_optionalp, IFTEST292), (IFTEST292\==[]->f_sys_dm_bad_key(c38_optional, TrueResult295), _24150=TrueResult295;_24150=[]), set_var(BlockExitEnv, sys_optionalp, t), sf_pop(BlockExitEnv, sys_vl, TrueResult481), _24030=TrueResult481;(get_var(BlockExitEnv, sys_v, V_Get298), f_eq(V_Get298, c38_rest, FORM1_Res300), FORM1_Res300\==[], IFTEST296=FORM1_Res300->true;get_var(BlockExitEnv, sys_v, V_Get299), f_eq(V_Get299, c38_body, C38_body), IFTEST296=C38_body), (IFTEST296\==[]->get_var(BlockExitEnv, sys_restp, IFTEST301), (IFTEST301\==[]->get_var(BlockExitEnv, sys_v, V_Get304), f_sys_dm_bad_key(V_Get304, TrueResult305), _24384=TrueResult305;_24384=[]), get_var(BlockExitEnv, sys_vl, Vl_Get306), f_second(Vl_Get306, Dm_v_Param), get_var(BlockExitEnv, sys_n, N_Get307), get_var(BlockExitEnv, sys_whole, Whole_Get308), f_sys_dm_nth_cdr(N_Get307, Whole_Get308, Nth_cdr_Ret), f_sys_dm_v(Dm_v_Param, Nth_cdr_Ret, Dm_v_Ret), set_var(BlockExitEnv, sys_restp, t), set_var(BlockExitEnv, sys_optionalp, t), set_var(BlockExitEnv, sys_no_check, t), get_var(BlockExitEnv, sys_vl, Vl_Get309), f_cddr(Vl_Get309, Vl), set_var(BlockExitEnv, sys_vl, Vl), get_var(BlockExitEnv, sys_v, V_Get311), (is_eq(V_Get311, c38_body)->get_var(BlockExitEnv, sys_top, IFTEST314), (IFTEST314\==[]->get_var(BlockExitEnv, sys_n, N_Get317), 'f_1-'(N_Get317, TrueResult319), TrueResult321=TrueResult319;get_var(BlockExitEnv, sys_n, N_Get318), TrueResult321=N_Get318), set_var(BlockExitEnv, sys_ppn, TrueResult321), TrueResult479=TrueResult321;TrueResult479=[]), ElseResult482=TrueResult479;get_var(BlockExitEnv, sys_v, V_Get323), (is_eq(V_Get323, c38_key)->get_var(BlockExitEnv, sys_keyp, IFTEST326), (IFTEST326\==[]->f_sys_dm_bad_key(c38_key, TrueResult329), _25004=TrueResult329;_25004=[]), f_gensym(Rest), set_var(BlockExitEnv, rest, Rest), sf_push(BlockExitEnv, [list, rest, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx, Xx_dl_xx634), set_var(BlockExitEnv, sys_keyp, t), set_var(BlockExitEnv, sys_restp, t), set_var(BlockExitEnv, sys_optionalp, t), set_var(BlockExitEnv, sys_no_check, t), sf_pop(BlockExitEnv, sys_vl, TrueResult477), ElseResult480=TrueResult477;get_var(BlockExitEnv, sys_v, V_Get331), (is_eq(V_Get331, c38_allow_other_keys)->(get_var(BlockExitEnv, sys_keyp, Keyp_Get336), f_not(Keyp_Get336, FORM1_Res338), FORM1_Res338\==[], IFTEST334=FORM1_Res338->true;get_var(BlockExitEnv, sys_allow_other_keys_p, Allow_other_keys_p_Get337), IFTEST334=Allow_other_keys_p_Get337), (IFTEST334\==[]->f_sys_dm_bad_key(c38_allow_other_keys, TrueResult339), _25198=TrueResult339;_25198=[]), set_var(BlockExitEnv, sys_allow_other_keys_p, t), set_var(BlockExitEnv, sys_allow_other_keys, t), sf_pop(BlockExitEnv, sys_vl, TrueResult475), ElseResult478=TrueResult475;get_var(BlockExitEnv, sys_v, V_Get341), (is_eq(V_Get341, c38_aux)->get_var(BlockExitEnv, sys_auxp, IFTEST344), (IFTEST344\==[]->f_sys_dm_bad_key(c38_aux, TrueResult347), _25442=TrueResult347;_25442=[]), set_var(BlockExitEnv, sys_auxp, t), set_var(BlockExitEnv, sys_allow_other_keys_p, t), set_var(BlockExitEnv, sys_keyp, t), set_var(BlockExitEnv, sys_restp, t), set_var(BlockExitEnv, sys_optionalp, t), sf_pop(BlockExitEnv, sys_vl, TrueResult473), ElseResult476=TrueResult473;get_var(BlockExitEnv, sys_auxp, IFTEST348), (IFTEST348\==[]->LEnv353=[bv(sys_x, []), bv(sys_init, [])|BlockExitEnv], get_var(LEnv353, sys_v, V_Get355), (is_symbolp(V_Get355)->get_var(LEnv353, sys_v, V_Get359), set_var(LEnv353, sys_x, V_Get359), _25704=V_Get359;get_var(LEnv353, sys_v, V_Get360), f_car(V_Get360, X), set_var(LEnv353, sys_x, X), get_var(LEnv353, sys_v, V_Get362), f_cdr(V_Get362, PredArgResult364), (s3q:is_endp(PredArgResult364)->ElseResult368=[];get_var(LEnv353, sys_v, V_Get365), f_second(V_Get365, ElseResult366), set_var(LEnv353, sys_init, ElseResult366), ElseResult368=ElseResult366), _25704=ElseResult368), get_var(LEnv353, sys_init, Init_Get370), get_var(LEnv353, sys_x, X_Get369), f_sys_dm_v(X_Get369, Init_Get370, LetResult352), sf_pop(BlockExitEnv, sys_vl, TrueResult471), ElseResult474=TrueResult471;get_var(BlockExitEnv, sys_keyp, IFTEST371), (IFTEST371\==[]->f_gensym(Temp_Init377), LEnv376=[bv(sys_temp, Temp_Init377), bv(sys_x, []), bv(sys_k, []), bv(sys_init, []), bv(sys_sv, [])|BlockExitEnv], get_var(LEnv376, sys_v, V_Get379), (is_symbolp(V_Get379)->get_var(LEnv376, sys_v, V_Get383), set_var(LEnv376, sys_x, V_Get383), get_var(LEnv376, sys_v, V_Get384), f_string(V_Get384, Intern_Param), f_intern(Intern_Param, keyword, TrueResult407), set_var(LEnv376, sys_k, TrueResult407), _26302=TrueResult407;get_var(LEnv376, sys_v, V_Get386), f_car(V_Get386, PredArgResult388), (is_symbolp(PredArgResult388)->get_var(LEnv376, sys_v, V_Get389), f_car(V_Get389, X636), set_var(LEnv376, sys_x, X636), get_var(LEnv376, sys_v, V_Get390), f_car(V_Get390, String_Param), f_string(String_Param, Intern_Param652), f_intern(Intern_Param652, keyword, TrueResult393), set_var(LEnv376, sys_k, TrueResult393), _26464=TrueResult393;get_var(LEnv376, sys_v, V_Get391), f_cadar(V_Get391, X637), set_var(LEnv376, sys_x, X637), get_var(LEnv376, sys_v, V_Get392), f_caar(V_Get392, ElseResult394), set_var(LEnv376, sys_k, ElseResult394), _26464=ElseResult394), get_var(LEnv376, sys_v, V_Get396), f_cdr(V_Get396, PredArgResult398), (s3q:is_endp(PredArgResult398)->ElseResult408=[];get_var(LEnv376, sys_v, V_Get399), f_second(V_Get399, Init), set_var(LEnv376, sys_init, Init), get_var(LEnv376, sys_v, V_Get401), f_cddr(V_Get401, PredArgResult403), (s3q:is_endp(PredArgResult403)->ElseResult406=[];get_var(LEnv376, sys_v, V_Get404), f_caddr(V_Get404, ElseResult405), set_var(LEnv376, sys_sv, ElseResult405), ElseResult406=ElseResult405), ElseResult408=ElseResult406), _26302=ElseResult408), get_var(LEnv376, rest, Rest_Get410), get_var(LEnv376, sys_k, K_Get411), get_var(LEnv376, sys_temp, Temp_Get409), f_sys_dm_v(Temp_Get409, [getf, Rest_Get410, K_Get411, [quote, sys_failed]], Dm_v_Ret666), get_var(LEnv376, sys_init, Init_Get414), get_var(LEnv376, sys_temp, Temp_Get413), get_var(LEnv376, sys_x, X_Get412), f_sys_dm_v(X_Get412, [if, [eq, Temp_Get413, [quote, sys_failed]], Init_Get414, Temp_Get413], Dm_v_Ret667), get_var(LEnv376, sys_sv, IFTEST416), (IFTEST416\==[]->get_var(LEnv376, sys_sv, Sv_Get419), get_var(LEnv376, sys_temp, Temp_Get420), f_sys_dm_v(Sv_Get419, [not, [eq, Temp_Get420, [quote, sys_failed]]], TrueResult421), _27302=TrueResult421;_27302=[]), sf_push(LEnv376, sys_k, sys_keys, LetResult375), sf_pop(BlockExitEnv, sys_vl, TrueResult469), ElseResult472=TrueResult469;get_var(BlockExitEnv, sys_optionalp, IFTEST422), (IFTEST422\==[]->LEnv427=[bv(sys_x, []), bv(sys_init, []), bv(sys_sv, [])|BlockExitEnv], get_var(LEnv427, sys_v, V_Get429), (is_symbolp(V_Get429)->get_var(LEnv427, sys_v, V_Get433), set_var(LEnv427, sys_x, V_Get433), _27620=V_Get433;get_var(LEnv427, sys_v, V_Get434), f_car(V_Get434, X639), set_var(LEnv427, sys_x, X639), get_var(LEnv427, sys_v, V_Get436), f_cdr(V_Get436, PredArgResult438), (s3q:is_endp(PredArgResult438)->ElseResult448=[];get_var(LEnv427, sys_v, V_Get439), f_second(V_Get439, Init640), set_var(LEnv427, sys_init, Init640), get_var(LEnv427, sys_v, V_Get441), f_cddr(V_Get441, PredArgResult443), (s3q:is_endp(PredArgResult443)->ElseResult446=[];get_var(LEnv427, sys_v, V_Get444), f_caddr(V_Get444, ElseResult445), set_var(LEnv427, sys_sv, ElseResult445), ElseResult446=ElseResult445), ElseResult448=ElseResult446), _27620=ElseResult448), get_var(LEnv427, sys_n, N_Get450), get_var(LEnv427, sys_whole, Whole_Get451), get_var(LEnv427, sys_x, X_Get449), f_sys_dm_nth_cdr(N_Get450, Whole_Get451, Nth_cdr_Ret668), get_var(LEnv427, sys_n, N_Get452), get_var(LEnv427, sys_whole, Whole_Get453), f_sys_dm_nth(N_Get452, Whole_Get453, Dm_nth_Ret), get_var(LEnv427, sys_init, Init_Get454), f_sys_dm_v(X_Get449, [if, Nth_cdr_Ret668, Dm_nth_Ret, Init_Get454], Dm_v_Ret670), get_var(LEnv427, sys_sv, IFTEST455), (IFTEST455\==[]->get_var(LEnv427, sys_n, N_Get459), get_var(LEnv427, sys_sv, Sv_Get458), get_var(LEnv427, sys_whole, Whole_Get460), f_sys_dm_nth_cdr(N_Get459, Whole_Get460, Nth_cdr_Ret671), f_sys_dm_v(Sv_Get458, [not, [null, Nth_cdr_Ret671]], TrueResult461), LetResult426=TrueResult461;LetResult426=[]), place_op(BlockExitEnv, incf, sys_n, symbol_value, [], Place_op_Ret), sf_pop(BlockExitEnv, sys_vl, TrueResult467), ElseResult470=TrueResult467;get_var(BlockExitEnv, sys_n, N_Get463), get_var(BlockExitEnv, sys_v, V_Get462), get_var(BlockExitEnv, sys_whole, Whole_Get464), f_sys_dm_nth_cdr(N_Get463, Whole_Get464, Nth_cdr_Ret673), get_var(BlockExitEnv, sys_n, N_Get465), get_var(BlockExitEnv, sys_whole, Whole_Get466), f_sys_dm_nth(N_Get465, Whole_Get466, Dm_nth_Ret674), f_sys_dm_v(V_Get462, [if, Nth_cdr_Ret673, Dm_nth_Ret674, [sys_dm_too_few_arguments]], Dm_v_Ret675), place_op(BlockExitEnv, incf, sys_n, symbol_value, [], Place_op_Ret676), sf_pop(BlockExitEnv, sys_vl, ElseResult468), ElseResult470=ElseResult468), ElseResult472=ElseResult470), ElseResult474=ElseResult472), ElseResult476=ElseResult474), ElseResult478=ElseResult476), ElseResult480=ElseResult478), ElseResult482=ElseResult480), _24030=ElseResult482), sf_psetq(BlockExitEnv, Sf_psetq_Ret), goto(do_label_5, BlockExitEnv), _TBResult=_GORES483)), [addr(addr_tagbody_5_do_label_5, do_label_5, '$unused', BlockExitEnv,  (get_var(BlockExitEnv, sys_vl, Consp_Param), f_consp(Consp_Param, Consp_Ret), (Consp_Ret==[]->get_var(BlockExitEnv, sys_vl, IFTEST36), (IFTEST36\==[]->get_var(BlockExitEnv, sys_restp, IFTEST39), (IFTEST39\==[]->f_sys_dm_bad_key(c38_rest, Bad_key_Ret), _29176=Bad_key_Ret;_29176=[]), sf_push(BlockExitEnv, [list, sys_vl, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx, Sf_push_Ret), set_var(BlockExitEnv, sys_no_check, t), _29180=t;_29180=[]), get_var(BlockExitEnv, rest, IFTEST46), (IFTEST46\==[]->get_var(BlockExitEnv, sys_allow_other_keys, Not_Param), f_not(Not_Param, TrueResult50), IFTEST44=TrueResult50;IFTEST44=[]), (IFTEST44\==[]->sf_push(BlockExitEnv, [cons, rest, sys_keys], sys_xx_key_check_xx, TrueResult51), _29240=TrueResult51;_29240=[]), get_var(BlockExitEnv, sys_no_check, IFTEST52), (IFTEST52\==[]->_29256=[];sf_push(BlockExitEnv, [cons, sys_whole, sys_n], sys_xx_arg_check_xx, Sf_push_Ret681), _29256=Sf_push_Ret681), get_var(BlockExitEnv, sys_ppn, Get_var_Ret), throw(block_exit([], Get_var_Ret)), _29262=ThrowResult;sf_declare(BlockExitEnv, [fixnum, sys_n, sys_ppn], Sf_declare_Ret683), get_var(BlockExitEnv, sys_vl, Vl_Get58), f_car(Vl_Get58, Car_Ret), set_var(BlockExitEnv, sys_v, Car_Ret), get_var(BlockExitEnv, sys_v, V_Get), (is_eq(V_Get, c38_optional)->get_var(BlockExitEnv, sys_optionalp, IFTEST63), (IFTEST63\==[]->f_sys_dm_bad_key(c38_optional, TrueResult66), _29338=TrueResult66;_29338=[]), set_var(BlockExitEnv, sys_optionalp, t), sf_pop(BlockExitEnv, sys_vl, TrueResult252), _29354=TrueResult252;(get_var(BlockExitEnv, sys_v, V_Get69), f_eq(V_Get69, c38_rest, Eq_Ret), Eq_Ret\==[], IFTEST67=Eq_Ret->true;get_var(BlockExitEnv, sys_v, V_Get70), f_eq(V_Get70, c38_body, Eq_Ret686), IFTEST67=Eq_Ret686), (IFTEST67\==[]->get_var(BlockExitEnv, sys_restp, IFTEST72), (IFTEST72\==[]->get_var(BlockExitEnv, sys_v, V_Get75), f_sys_dm_bad_key(V_Get75, TrueResult76), _29444=TrueResult76;_29444=[]), get_var(BlockExitEnv, sys_vl, Vl_Get77), f_second(Vl_Get77, Dm_v_Param656), get_var(BlockExitEnv, sys_n, Nth_cdr_Param), get_var(BlockExitEnv, sys_whole, Get_var_Ret687), f_sys_dm_nth_cdr(Nth_cdr_Param, Get_var_Ret687, Nth_cdr_Ret688), f_sys_dm_v(Dm_v_Param656, Nth_cdr_Ret688, Dm_v_Ret689), set_var(BlockExitEnv, sys_restp, t), set_var(BlockExitEnv, sys_optionalp, t), set_var(BlockExitEnv, sys_no_check, t), get_var(BlockExitEnv, sys_vl, Vl_Get80), f_cddr(Vl_Get80, Cddr_Ret), set_var(BlockExitEnv, sys_vl, Cddr_Ret), get_var(BlockExitEnv, sys_v, V_Get82), (is_eq(V_Get82, c38_body)->get_var(BlockExitEnv, sys_top, IFTEST85), (IFTEST85\==[]->get_var(BlockExitEnv, sys_n, N_Get88), 'f_1-'(N_Get88, TrueResult90), TrueResult92=TrueResult90;get_var(BlockExitEnv, sys_n, N_Get89), TrueResult92=N_Get89), set_var(BlockExitEnv, sys_ppn, TrueResult92), TrueResult250=TrueResult92;TrueResult250=[]), ElseResult253=TrueResult250;get_var(BlockExitEnv, sys_v, V_Get94), (is_eq(V_Get94, c38_key)->get_var(BlockExitEnv, sys_keyp, IFTEST97), (IFTEST97\==[]->f_sys_dm_bad_key(c38_key, TrueResult100), _29640=TrueResult100;_29640=[]), f_gensym(Gensym_Ret), set_var(BlockExitEnv, rest, Gensym_Ret), sf_push(BlockExitEnv, [list, rest, [sys_dm_nth_cdr, sys_n, sys_whole]], sys_xx_dl_xx, Sf_push_Ret692), set_var(BlockExitEnv, sys_keyp, t), set_var(BlockExitEnv, sys_restp, t), set_var(BlockExitEnv, sys_optionalp, t), set_var(BlockExitEnv, sys_no_check, t), sf_pop(BlockExitEnv, sys_vl, TrueResult248), ElseResult251=TrueResult248;get_var(BlockExitEnv, sys_v, V_Get102), (is_eq(V_Get102, c38_allow_other_keys)->(get_var(BlockExitEnv, sys_keyp, Keyp_Get107), f_not(Keyp_Get107, FORM1_Res109), FORM1_Res109\==[], IFTEST105=FORM1_Res109->true;get_var(BlockExitEnv, sys_allow_other_keys_p, Get_var_Ret693), IFTEST105=Get_var_Ret693), (IFTEST105\==[]->f_sys_dm_bad_key(c38_allow_other_keys, TrueResult110), _29746=TrueResult110;_29746=[]), set_var(BlockExitEnv, sys_allow_other_keys_p, t), set_var(BlockExitEnv, sys_allow_other_keys, t), sf_pop(BlockExitEnv, sys_vl, TrueResult246), ElseResult249=TrueResult246;get_var(BlockExitEnv, sys_v, V_Get112), (is_eq(V_Get112, c38_aux)->get_var(BlockExitEnv, sys_auxp, IFTEST115), (IFTEST115\==[]->f_sys_dm_bad_key(c38_aux, TrueResult118), _29818=TrueResult118;_29818=[]), set_var(BlockExitEnv, sys_auxp, t), set_var(BlockExitEnv, sys_allow_other_keys_p, t), set_var(BlockExitEnv, sys_keyp, t), set_var(BlockExitEnv, sys_restp, t), set_var(BlockExitEnv, sys_optionalp, t), sf_pop(BlockExitEnv, sys_vl, TrueResult244), ElseResult247=TrueResult244;get_var(BlockExitEnv, sys_auxp, IFTEST119), (IFTEST119\==[]->LEnv124=[bv(sys_x, []), bv(sys_init, [])|BlockExitEnv], get_var(LEnv124, sys_v, V_Get126), (is_symbolp(V_Get126)->get_var(LEnv124, sys_v, V_Get130), set_var(LEnv124, sys_x, V_Get130), _29904=V_Get130;get_var(LEnv124, sys_v, V_Get131), f_car(V_Get131, Car_Ret694), set_var(LEnv124, sys_x, Car_Ret694), get_var(LEnv124, sys_v, V_Get133), f_cdr(V_Get133, PredArgResult135), (s3q:is_endp(PredArgResult135)->ElseResult139=[];get_var(LEnv124, sys_v, V_Get136), f_second(V_Get136, ElseResult137), set_var(LEnv124, sys_init, ElseResult137), ElseResult139=ElseResult137), _29904=ElseResult139), get_var(LEnv124, sys_init, Get_var_Ret695), get_var(LEnv124, sys_x, Dm_v_Param657), f_sys_dm_v(Dm_v_Param657, Get_var_Ret695, LetResult123), sf_pop(BlockExitEnv, sys_vl, TrueResult242), ElseResult245=TrueResult242;get_var(BlockExitEnv, sys_keyp, IFTEST142), (IFTEST142\==[]->f_gensym(Gensym_Ret696), LEnv147=[bv(sys_temp, Gensym_Ret696), bv(sys_x, []), bv(sys_k, []), bv(sys_init, []), bv(sys_sv, [])|BlockExitEnv], get_var(LEnv147, sys_v, V_Get150), (is_symbolp(V_Get150)->get_var(LEnv147, sys_v, V_Get154), set_var(LEnv147, sys_x, V_Get154), get_var(LEnv147, sys_v, V_Get155), f_string(V_Get155, Intern_Param658), f_intern(Intern_Param658, keyword, TrueResult178), set_var(LEnv147, sys_k, TrueResult178), _30126=TrueResult178;get_var(LEnv147, sys_v, V_Get157), f_car(V_Get157, PredArgResult159), (is_symbolp(PredArgResult159)->get_var(LEnv147, sys_v, V_Get160), f_car(V_Get160, Car_Ret697), set_var(LEnv147, sys_x, Car_Ret697), get_var(LEnv147, sys_v, V_Get161), f_car(V_Get161, String_Param659), f_string(String_Param659, Intern_Param660), f_intern(Intern_Param660, keyword, TrueResult164), set_var(LEnv147, sys_k, TrueResult164), _30204=TrueResult164;get_var(LEnv147, sys_v, V_Get162), f_cadar(V_Get162, Cadar_Ret), set_var(LEnv147, sys_x, Cadar_Ret), get_var(LEnv147, sys_v, V_Get163), f_caar(V_Get163, ElseResult165), set_var(LEnv147, sys_k, ElseResult165), _30204=ElseResult165), get_var(LEnv147, sys_v, V_Get167), f_cdr(V_Get167, PredArgResult169), (s3q:is_endp(PredArgResult169)->ElseResult179=[];get_var(LEnv147, sys_v, V_Get170), f_second(V_Get170, Second_Ret), set_var(LEnv147, sys_init, Second_Ret), get_var(LEnv147, sys_v, V_Get172), f_cddr(V_Get172, PredArgResult174), (s3q:is_endp(PredArgResult174)->ElseResult177=[];get_var(LEnv147, sys_v, V_Get175), f_caddr(V_Get175, ElseResult176), set_var(LEnv147, sys_sv, ElseResult176), ElseResult177=ElseResult176), ElseResult179=ElseResult177), _30126=ElseResult179), get_var(LEnv147, rest, Rest_Get181), get_var(LEnv147, sys_k, Get_var_Ret700), get_var(LEnv147, sys_temp, Dm_v_Param661), f_sys_dm_v(Dm_v_Param661, [getf, Rest_Get181, Get_var_Ret700, [quote, sys_failed]], Dm_v_Ret701), get_var(LEnv147, sys_init, Init_Get185), get_var(LEnv147, sys_temp, Temp_Get184), get_var(LEnv147, sys_x, X_Get183), f_sys_dm_v(X_Get183, [if, [eq, Temp_Get184, [quote, sys_failed]], Init_Get185, Temp_Get184], Dm_v_Ret702), get_var(LEnv147, sys_sv, IFTEST187), (IFTEST187\==[]->get_var(LEnv147, sys_sv, Sv_Get190), get_var(LEnv147, sys_temp, Temp_Get191), f_sys_dm_v(Sv_Get190, [not, [eq, Temp_Get191, [quote, sys_failed]]], TrueResult192), _30498=TrueResult192;_30498=[]), sf_push(LEnv147, sys_k, sys_keys, LetResult146), sf_pop(BlockExitEnv, sys_vl, TrueResult240), ElseResult243=TrueResult240;get_var(BlockExitEnv, sys_optionalp, IFTEST193), (IFTEST193\==[]->LEnv198=[bv(sys_x, []), bv(sys_init, []), bv(sys_sv, [])|BlockExitEnv], get_var(LEnv198, sys_v, V_Get200), (is_symbolp(V_Get200)->get_var(LEnv198, sys_v, V_Get204), set_var(LEnv198, sys_x, V_Get204), _30598=V_Get204;get_var(LEnv198, sys_v, V_Get205), f_car(V_Get205, Car_Ret703), set_var(LEnv198, sys_x, Car_Ret703), get_var(LEnv198, sys_v, V_Get207), f_cdr(V_Get207, PredArgResult209), (s3q:is_endp(PredArgResult209)->ElseResult219=[];get_var(LEnv198, sys_v, V_Get210), f_second(V_Get210, Second_Ret704), set_var(LEnv198, sys_init, Second_Ret704), get_var(LEnv198, sys_v, V_Get212), f_cddr(V_Get212, PredArgResult214), (s3q:is_endp(PredArgResult214)->ElseResult217=[];get_var(LEnv198, sys_v, V_Get215), f_caddr(V_Get215, ElseResult216), set_var(LEnv198, sys_sv, ElseResult216), ElseResult217=ElseResult216), ElseResult219=ElseResult217), _30598=ElseResult219), get_var(LEnv198, sys_n, N_Get221), get_var(LEnv198, sys_whole, Whole_Get222), get_var(LEnv198, sys_x, X_Get220), f_sys_dm_nth_cdr(N_Get221, Whole_Get222, Nth_cdr_Ret705), get_var(LEnv198, sys_n, N_Get223), get_var(LEnv198, sys_whole, Whole_Get224), f_sys_dm_nth(N_Get223, Whole_Get224, Dm_nth_Ret706), get_var(LEnv198, sys_init, Init_Get225), f_sys_dm_v(X_Get220, [if, Nth_cdr_Ret705, Dm_nth_Ret706, Init_Get225], Dm_v_Ret707), get_var(LEnv198, sys_sv, IFTEST226), (IFTEST226\==[]->get_var(LEnv198, sys_n, N_Get230), get_var(LEnv198, sys_sv, Sv_Get229), get_var(LEnv198, sys_whole, Whole_Get231), f_sys_dm_nth_cdr(N_Get230, Whole_Get231, Nth_cdr_Ret708), f_sys_dm_v(Sv_Get229, [not, [null, Nth_cdr_Ret708]], TrueResult232), LetResult197=TrueResult232;LetResult197=[]), place_op(BlockExitEnv, incf, sys_n, symbol_value, [], Place_op_Ret709), sf_pop(BlockExitEnv, sys_vl, TrueResult238), ElseResult241=TrueResult238;get_var(BlockExitEnv, sys_n, N_Get234), get_var(BlockExitEnv, sys_v, V_Get233), get_var(BlockExitEnv, sys_whole, Whole_Get235), f_sys_dm_nth_cdr(N_Get234, Whole_Get235, Nth_cdr_Ret710), get_var(BlockExitEnv, sys_n, N_Get236), get_var(BlockExitEnv, sys_whole, Whole_Get237), f_sys_dm_nth(N_Get236, Whole_Get237, Dm_nth_Ret711), f_sys_dm_v(V_Get233, [if, Nth_cdr_Ret710, Dm_nth_Ret711, [sys_dm_too_few_arguments]], Dm_v_Ret712), place_op(BlockExitEnv, incf, sys_n, symbol_value, [], Place_op_Ret713), sf_pop(BlockExitEnv, sys_vl, ElseResult239), ElseResult241=ElseResult239), ElseResult243=ElseResult241), ElseResult245=ElseResult243), ElseResult247=ElseResult245), ElseResult249=ElseResult247), ElseResult251=ElseResult249), ElseResult253=ElseResult251), _29354=ElseResult253), sf_psetq(BlockExitEnv, Sf_psetq_Ret714), goto(do_label_5, BlockExitEnv), _29262=_GORES)))]), []=LetResult), block_exit([], LetResult), true)), LetResult=FnResult17), block_exit(sys_dm_vl, FnResult17), true))),
		  assert_lsp(sys_dm_v,
			     wl:lambda_def(defun, sys_dm_v, f_sys_dm_v5, [sys_v, sys_init], [[if, [symbolp, sys_v], [push, [if, sys_init, [list, sys_v, sys_init], sys_v], sys_xx_dl_xx], [let, [[sys_temp, [gensym]]], [push, [if, sys_init, [list, sys_temp, sys_init], sys_temp], sys_xx_dl_xx], [sys_dm_vl, sys_v, sys_temp, []]]]])),
		  assert_lsp(sys_dm_v,
			     wl:arglist_info(sys_dm_v, f_sys_dm_v5, [sys_v, sys_init], arginfo{all:[sys_v, sys_init], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_v, sys_init], opt:0, req:[sys_v, sys_init], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_dm_v, wl:init_args(2, f_sys_dm_v5)),
		  assert_lsp(sys_dm_v,
			     (f_sys_dm_v5(V_In, Init_In, RestNKeys490, FnResult489):-GEnv641=[bv(sys_v, V_In), bv(sys_init, Init_In)], catch(((get_var(GEnv641, sys_v, V_Get494), (is_symbolp(V_Get494)->sf_push(GEnv641, [if, sys_init, [list, sys_v, sys_init], sys_v], sys_xx_dl_xx, TrueResult503), _31172=TrueResult503;f_gensym(Temp_Init500), LEnv499=[bv(sys_temp, Temp_Init500)|GEnv641], sf_push(LEnv499, [if, sys_init, [list, sys_temp, sys_init], sys_temp], sys_xx_dl_xx, Xx_dl_xx642), get_var(LEnv499, sys_temp, Temp_Get502), get_var(LEnv499, sys_v, V_Get501), f_sys_dm_vl(V_Get501, Temp_Get502, [], LetResult498), _31172=LetResult498)), _31172=FnResult489), block_exit(sys_dm_v, FnResult489), true))),
		  assert_lsp(sys_dm_nth,
			     wl:lambda_def(defun, sys_dm_nth, f_sys_dm_nth5, [sys_n, sys_v], [[multiple_value_bind, [sys_q, sys_r], [floor, sys_n, 4], [declare, [fixnum, sys_q, sys_r]], [dotimes, [sys_i, sys_q], [setq, sys_v, [list, [quote, cddddr], sys_v]]], [case, sys_r, [0, [list, [quote, car], sys_v]], [1, [list, [quote, cadr], sys_v]], [2, [list, [quote, caddr], sys_v]], [3, [list, [quote, cadddr], sys_v]]]]])),
		  assert_lsp(sys_dm_nth,
			     wl:arglist_info(sys_dm_nth, f_sys_dm_nth5, [sys_n, sys_v], arginfo{all:[sys_n, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_n, sys_v], opt:0, req:[sys_n, sys_v], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_dm_nth, wl:init_args(2, f_sys_dm_nth5)),
		  assert_lsp(sys_dm_nth,
			     (f_sys_dm_nth5(N_In, V_In509, RestNKeys507, FnResult506):-CDR=[bv(sys_n, N_In), bv(sys_v, V_In509)], catch(((LEnv512=[bv(sys_q, []), bv(sys_r, [])|CDR], get_var(LEnv512, sys_n, N_Get513), f_floor(N_Get513, [4], Floor_Ret), setq_from_values(LEnv512, [sys_q, sys_r]), sf_declare(LEnv512, [fixnum, sys_q, sys_r], Sf_declare_Ret717), sf_dotimes(LEnv512, [sys_i, sys_q], [setq, sys_v, [list, [quote, cddddr], sys_v]], Sf_dotimes_Ret), get_var(LEnv512, sys_r, Key), (is_eq(Key, 0)->get_var(LEnv512, sys_v, V_Get519), TrueResult535=[car, V_Get519], LetResult511=TrueResult535;(is_eq(Key, 1)->get_var(LEnv512, sys_v, V_Get522), TrueResult533=[cadr, V_Get522], ElseResult536=TrueResult533;(is_eq(Key, 2)->get_var(LEnv512, sys_v, V_Get525), TrueResult531=[caddr, V_Get525], ElseResult534=TrueResult531;(is_eq(Key, 3)->get_var(LEnv512, sys_v, V_Get528), TrueResult529=[cadddr, V_Get528], ElseResult532=TrueResult529;ElseResult530=[], ElseResult532=ElseResult530), ElseResult534=ElseResult532), ElseResult536=ElseResult534), LetResult511=ElseResult536)), LetResult511=FnResult506), block_exit(sys_dm_nth, FnResult506), true))),
		  assert_lsp(sys_dm_nth_cdr,
			     wl:lambda_def(defun, sys_dm_nth_cdr, f_sys_dm_nth_cdr5, [sys_n, sys_v], [[multiple_value_bind, [sys_q, sys_r], [floor, sys_n, 4], [declare, [fixnum, sys_q, sys_r]], [dotimes, [sys_i, sys_q], [setq, sys_v, [list, [quote, cddddr], sys_v]]], [case, sys_r, [0, sys_v], [1, [list, [quote, cdr], sys_v]], [2, [list, [quote, cddr], sys_v]], [3, [list, [quote, cdddr], sys_v]]]]])),
		  assert_lsp(sys_dm_nth_cdr,
			     wl:arglist_info(sys_dm_nth_cdr, f_sys_dm_nth_cdr5, [sys_n, sys_v], arginfo{all:[sys_n, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_n, sys_v], opt:0, req:[sys_n, sys_v], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_dm_nth_cdr, wl:init_args(2, f_sys_dm_nth_cdr5)),
		  assert_lsp(sys_dm_nth_cdr,
			     (f_sys_dm_nth_cdr5(N_In540, V_In541, RestNKeys539, FnResult538):-CDR719=[bv(sys_n, N_In540), bv(sys_v, V_In541)], catch(((LEnv544=[bv(sys_q, []), bv(sys_r, [])|CDR719], get_var(LEnv544, sys_n, N_Get545), f_floor(N_Get545, [4], Floor_Ret720), setq_from_values(LEnv544, [sys_q, sys_r]), sf_declare(LEnv544, [fixnum, sys_q, sys_r], Sf_declare_Ret721), sf_dotimes(LEnv544, [sys_i, sys_q], [setq, sys_v, [list, [quote, cddddr], sys_v]], Sf_dotimes_Ret722), get_var(LEnv544, sys_r, Key), (is_eq(Key, 0)->get_var(LEnv544, sys_v, V_Get551), LetResult543=V_Get551;(is_eq(Key, 1)->get_var(LEnv544, sys_v, V_Get554), TrueResult565=[cdr, V_Get554], ElseResult568=TrueResult565;(is_eq(Key, 2)->get_var(LEnv544, sys_v, V_Get557), TrueResult563=[cddr, V_Get557], ElseResult566=TrueResult563;(is_eq(Key, 3)->get_var(LEnv544, sys_v, V_Get560), TrueResult561=[cdddr, V_Get560], ElseResult564=TrueResult561;ElseResult562=[], ElseResult564=ElseResult562), ElseResult566=ElseResult564), ElseResult568=ElseResult566), LetResult543=ElseResult568)), LetResult543=FnResult538), block_exit(sys_dm_nth_cdr, FnResult538), true))),
		  get_var(Env, sys_vl, Vl_Get571),
		  (   s3q:is_listp(Vl_Get571)
		  ->  _33076=[]
		  ;   get_var(Env, sys_vl, Vl_Get575),
		      (   is_symbolp(Vl_Get575)
		      ->  get_var(Env, sys_vl, Vl_Get579),
			  TrueResult581=[c38_rest, Vl_Get579],
			  set_var(Env, sys_vl, TrueResult581),
			  ElseResult583=TrueResult581
		      ;   get_var(Env, sys_vl, Vl_Get580),
			  f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "The defmacro-lambda-list ~s is not a list."),
				    Vl_Get580
				  ],
				  ElseResult582),
			  ElseResult583=ElseResult582
		      ),
		      _33076=ElseResult583
		  ),
		  get_var(Env, sys_body, Body_Get),
		  f_sys_find_doc(Body_Get, [], Find_doc_Ret),
		  setq_from_values(Env, [sys_doc, sys_decls, sys_body]),
		  get_var(Env, sys_vl, Vl_Get588),
		  (   s3q:is_listp(Vl_Get588)
		  ->  get_var(Env, sys_vl, Vl_Get591),
		      f_car(Vl_Get591, Eq_Param),
		      f_eq(Eq_Param, c38_whole, TrueResult592),
		      IFTEST585=TrueResult592
		  ;   IFTEST585=[]
		  ),
		  (   IFTEST585\==[]
		  ->  get_var(Env, sys_vl, Vl_Get593),
		      f_second(Vl_Get593, Whole),
		      set_var(Env, sys_whole, Whole),
		      get_var(Env, sys_vl, Vl_Get594),
		      f_cddr(Vl_Get594, TrueResult595),
		      set_var(Env, sys_vl, TrueResult595),
		      _33416=TrueResult595
		  ;   f_gensym(ElseResult596),
		      set_var(Env, sys_whole, ElseResult596),
		      _33416=ElseResult596
		  ),
		  get_var(Env, sys_vl, Vl_Get599),
		  f_member(c38_environment, Vl_Get599, [kw_test, f_eq], IFTEST597),
		  set_var(Env, sys_env, IFTEST597),
		  (   IFTEST597\==[]
		  ->  get_var(Env, sys_env, Env_Get),
		      get_var(Env, sys_vl, Vl_Get600),
		      f_ldiff(Vl_Get600, Env_Get, Ldiff_Ret),
		      get_var(Env, sys_env, Env_Get602),
		      f_cddr(Env_Get602, Cddr_Ret725),
		      f_nconc([Ldiff_Ret, Cddr_Ret725], Vl644),
		      set_var(Env, sys_vl, Vl644),
		      get_var(Env, sys_env, Env_Get603),
		      f_second(Env_Get603, TrueResult604),
		      set_var(Env, sys_env, TrueResult604),
		      _33716=TrueResult604
		  ;   f_gensym(Env645),
		      set_var(Env, sys_env, Env645),
		      sf_push(Env,
			      ['#BQ', [declare, [ignore, ['#COMMA', sys_env]]]],
			      sys_decls,
			      ElseResult605),
		      _33716=ElseResult605
		  ),
		  get_var(Env, sys_env, Env_Get606),
		  get_var(Env, sys_whole, Whole_Get607),
		  set_var(Env, sys_xx_dl_xx, [c38_aux, Env_Get606, Whole_Get607]),
		  get_var(Env, sys_vl, Vl_Get608),
		  get_var(Env, sys_whole, Whole_Get609),
		  f_sys_dm_vl5(Vl_Get608, Whole_Get609, t, T),
		  set_var(Env, sys_ppn, T),
		  get_var(Env, sys_xx_key_check_xx, Xx_key_check_xx_Get),
		  BV=bv(sys_kc, Ele),
		  Env2=[BV|Env],
		  forall(member(Ele, Xx_key_check_xx_Get),
			 ( nb_setarg(2, BV, Ele),
			   sf_push(Env2,
				   
				   [ '#BQ',
				     
				     [ unless,
				       
				       [ getf,
					 ['#COMMA', [car, sys_kc]],
					 kw_allow_other_keys
				       ],
				       
				       [ do,
					 
					 [ 
					   [ sys_vl,
					     ['#COMMA', [car, sys_kc]],
					     [cddr, sys_vl]
					   ]
					 ],
					 [[endp, sys_vl]],
					 
					 [ unless,
					   
					   [ member,
					     [car, sys_vl],
					     [quote, ['#COMMA', [cdr, sys_kc]]]
					   ],
					   
					   [ sys_dm_key_not_allowed,
					     [car, sys_vl]
					   ]
					 ]
				       ]
				     ]
				   ],
				   sys_body,
				   Body)
			 )),
		  get_var(Env, sys_xx_arg_check_xx, Xx_arg_check_xx_Get),
		  BV616=bv(sys_ac, Ele618),
		  Env2617=[BV616|Env],
		  forall(member(Ele618, Xx_arg_check_xx_Get),
			 ( nb_setarg(2, BV616, Ele618),
			   sf_push(Env2617,
				   
				   [ '#BQ',
				     
				     [ unless,
				       
				       [ endp,
					 
					 [ '#COMMA',
					   
					   [ sys_dm_nth_cdr,
					     [cdr, sys_ac],
					     [car, sys_ac]
					   ]
					 ]
				       ],
				       [sys_dm_too_many_arguments]
				     ]
				   ],
				   sys_body,
				   Body648)
			 )),
		  get_var(Env, sys_name, Name_Get),
		  get_var(Env, sys_xx_dl_xx, Xx_dl_xx_Get),
		  f_nreverse(Xx_dl_xx_Get, Nreverse_Ret),
		  get_var(Env, sys_body, Body_Get623),
		  get_var(Env, sys_decls, Decls_Get),
		  f_nconc([Decls_Get, Body_Get623], Nconc_Ret),
		  get_var(Env, sys_doc, Doc_Get),
		  get_var(Env, sys_ppn, Ppn_Get625),
		  nb_setval('$mv_return',
			    
			    [ 
			      [ sys_lambda_block,
				Name_Get,
				Nreverse_Ret
			      | Nconc_Ret
			      ],
			      Doc_Get,
			      Ppn_Get625
			    ])
		),
		[sys_lambda_block, Name_Get, Nreverse_Ret|Nconc_Ret]=FnResult
	      ),
	      block_exit(sys_expand_defmacro, FnResult),
	      true).
:- set_opv(sys_expand_defmacro, symbol_function, f_sys_expand_defmacro),
   DefunResult=sys_expand_defmacro.
/*
:- side_effect(assert_lsp(sys_expand_defmacro,
			  lambda_def(defun,
				     sys_expand_defmacro,
				     f_sys_expand_defmacro,
				     
				     [ sys_name,
				       sys_vl,
				       sys_body,
				       c38_aux,
				       sys_xx_dl_xx,
				       [sys_xx_key_check_xx, []],
				       [sys_xx_arg_check_xx, []],
				       sys_doc,
				       sys_decls,
				       sys_whole,
				       sys_ppn,
				       [sys_env, []],
				       [sys_envp, []]
				     ],
				     
				     [ 
				       [ labels,
					 
					 [ 
					   [ sys_dm_vl,
					     [sys_vl, sys_whole, sys_top],
					     
					     [ do,
					       
					       [ [sys_optionalp],
						 [sys_restp],
						 [sys_keyp],
						 [sys_allow_other_keys_p],
						 [sys_auxp],
						 [rest],
						 [sys_allow_other_keys],
						 [sys_keys],
						 [sys_no_check],
						 [sys_n, [if, sys_top, 1, 0]],
						 [sys_ppn, 0],
						 [sys_v]
					       ],
					       
					       [ [not, [consp, sys_vl]],
						 
						 [ when,
						   sys_vl,
						   
						   [ when,
						     sys_restp,
						     
						     [ sys_dm_bad_key,
						       [quote, c38_rest]
						     ]
						   ],
						   
						   [ push,
						     
						     [ list,
						       sys_vl,
						       
						       [ sys_dm_nth_cdr,
							 sys_n,
							 sys_whole
						       ]
						     ],
						     sys_xx_dl_xx
						   ],
						   [setq, sys_no_check, t]
						 ],
						 
						 [ when,
						   
						   [ and,
						     rest,
						     
						     [ not,
						       sys_allow_other_keys
						     ]
						   ],
						   
						   [ push,
						     [cons, rest, sys_keys],
						     sys_xx_key_check_xx
						   ]
						 ],
						 
						 [ unless,
						   sys_no_check,
						   
						   [ push,
						     [cons, sys_whole, sys_n],
						     sys_xx_arg_check_xx
						   ]
						 ],
						 sys_ppn
					       ],
					       
					       [ declare,
						 [fixnum, sys_n, sys_ppn]
					       ],
					       [setq, sys_v, [car, sys_vl]],
					       
					       [ cond,
						 
						 [ 
						   [ eq,
						     sys_v,
						     [quote, c38_optional]
						   ],
						   
						   [ when,
						     sys_optionalp,
						     
						     [ sys_dm_bad_key,
						       [quote, c38_optional]
						     ]
						   ],
						   [setq, sys_optionalp, t],
						   [pop, sys_vl]
						 ],
						 
						 [ 
						   [ or,
						     
						     [ eq,
						       sys_v,
						       [quote, c38_rest]
						     ],
						     
						     [ eq,
						       sys_v,
						       [quote, c38_body]
						     ]
						   ],
						   
						   [ when,
						     sys_restp,
						     [sys_dm_bad_key, sys_v]
						   ],
						   
						   [ sys_dm_v,
						     [second, sys_vl],
						     
						     [ sys_dm_nth_cdr,
						       sys_n,
						       sys_whole
						     ]
						   ],
						   
						   [ setq,
						     sys_restp,
						     t,
						     sys_optionalp,
						     t,
						     sys_no_check,
						     t
						   ],
						   [setq, sys_vl, [cddr, sys_vl]],
						   
						   [ when,
						     
						     [ eq,
						       sys_v,
						       [quote, c38_body]
						     ],
						     
						     [ setq,
						       sys_ppn,
						       
						       [ if,
							 sys_top,
							 
							 [ the,
							   fixnum,
							   ['1-', sys_n]
							 ],
							 sys_n
						       ]
						     ]
						   ]
						 ],
						 
						 [ [eq, sys_v, [quote, c38_key]],
						   
						   [ when,
						     sys_keyp,
						     
						     [ sys_dm_bad_key,
						       [quote, c38_key]
						     ]
						   ],
						   [setq, rest, [gensym]],
						   
						   [ push,
						     
						     [ list,
						       rest,
						       
						       [ sys_dm_nth_cdr,
							 sys_n,
							 sys_whole
						       ]
						     ],
						     sys_xx_dl_xx
						   ],
						   
						   [ setq,
						     sys_keyp,
						     t,
						     sys_restp,
						     t,
						     sys_optionalp,
						     t,
						     sys_no_check,
						     t
						   ],
						   [pop, sys_vl]
						 ],
						 
						 [ 
						   [ eq,
						     sys_v,
						     
						     [ quote,
						       c38_allow_other_keys
						     ]
						   ],
						   
						   [ when,
						     
						     [ or,
						       [not, sys_keyp],
						       sys_allow_other_keys_p
						     ],
						     
						     [ sys_dm_bad_key,
						       
						       [ quote,
							 c38_allow_other_keys
						       ]
						     ]
						   ],
						   
						   [ setq,
						     sys_allow_other_keys_p,
						     t
						   ],
						   
						   [ setq,
						     sys_allow_other_keys,
						     t
						   ],
						   [pop, sys_vl]
						 ],
						 
						 [ [eq, sys_v, [quote, c38_aux]],
						   
						   [ when,
						     sys_auxp,
						     
						     [ sys_dm_bad_key,
						       [quote, c38_aux]
						     ]
						   ],
						   
						   [ setq,
						     sys_auxp,
						     t,
						     sys_allow_other_keys_p,
						     t,
						     sys_keyp,
						     t,
						     sys_restp,
						     t,
						     sys_optionalp,
						     t
						   ],
						   [pop, sys_vl]
						 ],
						 
						 [ sys_auxp,
						   
						   [ let,
						     [sys_x, [sys_init, []]],
						     
						     [ cond,
						       
						       [ [symbolp, sys_v],
							 [setq, sys_x, sys_v]
						       ],
						       
						       [ t,
							 
							 [ setq,
							   sys_x,
							   [car, sys_v]
							 ],
							 
							 [ unless,
							   [endp, [cdr, sys_v]],
							   
							   [ setq,
							     sys_init,
							     [second, sys_v]
							   ]
							 ]
						       ]
						     ],
						     [sys_dm_v, sys_x, sys_init]
						   ],
						   [pop, sys_vl]
						 ],
						 
						 [ sys_keyp,
						   
						   [ let,
						     
						     [ [sys_temp, [gensym]],
						       sys_x,
						       sys_k,
						       [sys_init, []],
						       [sys_sv, []]
						     ],
						     
						     [ cond,
						       
						       [ [symbolp, sys_v],
							 
							 [ setq,
							   sys_x,
							   sys_v,
							   sys_k,
							   
							   [ intern,
							     [string, sys_v],
							     [quote, keyword]
							   ]
							 ]
						       ],
						       
						       [ t,
							 
							 [ if,
							   
							   [ symbolp,
							     [car, sys_v]
							   ],
							   
							   [ setq,
							     sys_x,
							     [car, sys_v],
							     sys_k,
							     
							     [ intern,
							       
							       [ string,
								 [car, sys_v]
							       ],
							       [quote, keyword]
							     ]
							   ],
							   
							   [ setq,
							     sys_x,
							     [cadar, sys_v],
							     sys_k,
							     [caar, sys_v]
							   ]
							 ],
							 
							 [ unless,
							   [endp, [cdr, sys_v]],
							   
							   [ setq,
							     sys_init,
							     [second, sys_v]
							   ],
							   
							   [ unless,
							     [endp, [cddr, sys_v]],
							     
							     [ setq,
							       sys_sv,
							       [caddr, sys_v]
							     ]
							   ]
							 ]
						       ]
						     ],
						     
						     [ sys_dm_v,
						       sys_temp,
						       
						       [ '#BQ',
							 
							 [ getf,
							   ['#COMMA', rest],
							   ['#COMMA', sys_k],
							   [quote, sys_failed]
							 ]
						       ]
						     ],
						     
						     [ sys_dm_v,
						       sys_x,
						       
						       [ '#BQ',
							 
							 [ if,
							   
							   [ eq,
							     ['#COMMA', sys_temp],
							     [quote, sys_failed]
							   ],
							   ['#COMMA', sys_init],
							   ['#COMMA', sys_temp]
							 ]
						       ]
						     ],
						     
						     [ when,
						       sys_sv,
						       
						       [ sys_dm_v,
							 sys_sv,
							 
							 [ '#BQ',
							   
							   [ not,
							     
							     [ eq,
							       ['#COMMA', sys_temp],
							       [quote, sys_failed]
							     ]
							   ]
							 ]
						       ]
						     ],
						     [push, sys_k, sys_keys]
						   ],
						   [pop, sys_vl]
						 ],
						 
						 [ sys_optionalp,
						   
						   [ let,
						     
						     [ sys_x,
						       [sys_init, []],
						       [sys_sv, []]
						     ],
						     
						     [ cond,
						       
						       [ [symbolp, sys_v],
							 [setq, sys_x, sys_v]
						       ],
						       
						       [ t,
							 
							 [ setq,
							   sys_x,
							   [car, sys_v]
							 ],
							 
							 [ unless,
							   [endp, [cdr, sys_v]],
							   
							   [ setq,
							     sys_init,
							     [second, sys_v]
							   ],
							   
							   [ unless,
							     [endp, [cddr, sys_v]],
							     
							     [ setq,
							       sys_sv,
							       [caddr, sys_v]
							     ]
							   ]
							 ]
						       ]
						     ],
						     
						     [ sys_dm_v,
						       sys_x,
						       
						       [ '#BQ',
							 
							 [ if,
							   
							   [ '#COMMA',
							     
							     [ sys_dm_nth_cdr,
							       sys_n,
							       sys_whole
							     ]
							   ],
							   
							   [ '#COMMA',
							     
							     [ sys_dm_nth,
							       sys_n,
							       sys_whole
							     ]
							   ],
							   ['#COMMA', sys_init]
							 ]
						       ]
						     ],
						     
						     [ when,
						       sys_sv,
						       
						       [ sys_dm_v,
							 sys_sv,
							 
							 [ '#BQ',
							   
							   [ not,
							     
							     [ null,
							       
							       [ '#COMMA',
								 
								 [ sys_dm_nth_cdr,
								   sys_n,
								   sys_whole
								 ]
							       ]
							     ]
							   ]
							 ]
						       ]
						     ]
						   ],
						   [incf, sys_n],
						   [pop, sys_vl]
						 ],
						 
						 [ t,
						   
						   [ sys_dm_v,
						     sys_v,
						     
						     [ '#BQ',
						       
						       [ if,
							 
							 [ '#COMMA',
							   
							   [ sys_dm_nth_cdr,
							     sys_n,
							     sys_whole
							   ]
							 ],
							 
							 [ '#COMMA',
							   
							   [ sys_dm_nth,
							     sys_n,
							     sys_whole
							   ]
							 ],
							 
							 [ sys_dm_too_few_arguments
							 ]
						       ]
						     ]
						   ],
						   [incf, sys_n],
						   [pop, sys_vl]
						 ]
					       ]
					     ]
					   ],
					   
					   [ sys_dm_v,
					     [sys_v, sys_init],
					     
					     [ if,
					       [symbolp, sys_v],
					       
					       [ push,
						 
						 [ if,
						   sys_init,
						   [list, sys_v, sys_init],
						   sys_v
						 ],
						 sys_xx_dl_xx
					       ],
					       
					       [ let,
						 [[sys_temp, [gensym]]],
						 
						 [ push,
						   
						   [ if,
						     sys_init,
						     [list, sys_temp, sys_init],
						     sys_temp
						   ],
						   sys_xx_dl_xx
						 ],
						 [sys_dm_vl, sys_v, sys_temp, []]
					       ]
					     ]
					   ],
					   
					   [ sys_dm_nth,
					     [sys_n, sys_v],
					     
					     [ multiple_value_bind,
					       [sys_q, sys_r],
					       [floor, sys_n, 4],
					       [declare, [fixnum, sys_q, sys_r]],
					       
					       [ dotimes,
						 [sys_i, sys_q],
						 
						 [ setq,
						   sys_v,
						   [list, [quote, cddddr], sys_v]
						 ]
					       ],
					       
					       [ case,
						 sys_r,
						 [0, [list, [quote, car], sys_v]],
						 [1, [list, [quote, cadr], sys_v]],
						 
						 [ 2,
						   [list, [quote, caddr], sys_v]
						 ],
						 
						 [ 3,
						   [list, [quote, cadddr], sys_v]
						 ]
					       ]
					     ]
					   ],
					   
					   [ sys_dm_nth_cdr,
					     [sys_n, sys_v],
					     
					     [ multiple_value_bind,
					       [sys_q, sys_r],
					       [floor, sys_n, 4],
					       [declare, [fixnum, sys_q, sys_r]],
					       
					       [ dotimes,
						 [sys_i, sys_q],
						 
						 [ setq,
						   sys_v,
						   [list, [quote, cddddr], sys_v]
						 ]
					       ],
					       
					       [ case,
						 sys_r,
						 [0, sys_v],
						 [1, [list, [quote, cdr], sys_v]],
						 [2, [list, [quote, cddr], sys_v]],
						 
						 [ 3,
						   [list, [quote, cdddr], sys_v]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ cond,
					   [[listp, sys_vl]],
					   
					   [ [symbolp, sys_vl],
					     
					     [ setq,
					       sys_vl,
					       [list, [quote, c38_rest], sys_vl]
					     ]
					   ],
					   
					   [ t,
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"The defmacro-lambda-list ~s is not a list."),
					       sys_vl
					     ]
					   ]
					 ],
					 
					 [ multiple_value_setq,
					   [sys_doc, sys_decls, sys_body],
					   [sys_find_doc, sys_body, []]
					 ],
					 
					 [ if,
					   
					   [ and,
					     [listp, sys_vl],
					     
					     [ eq,
					       [car, sys_vl],
					       [quote, c38_whole]
					     ]
					   ],
					   
					   [ setq,
					     sys_whole,
					     [second, sys_vl],
					     sys_vl,
					     [cddr, sys_vl]
					   ],
					   [setq, sys_whole, [gensym]]
					 ],
					 
					 [ if,
					   
					   [ setq,
					     sys_env,
					     
					     [ member,
					       [quote, c38_environment],
					       sys_vl,
					       kw_test,
					       function(eq)
					     ]
					   ],
					   
					   [ setq,
					     sys_vl,
					     
					     [ nconc,
					       [ldiff, sys_vl, sys_env],
					       [cddr, sys_env]
					     ],
					     sys_env,
					     [second, sys_env]
					   ],
					   
					   [ progn,
					     [setq, sys_env, [gensym]],
					     
					     [ push,
					       
					       [ '#BQ',
						 
						 [ declare,
						   [ignore, ['#COMMA', sys_env]]
						 ]
					       ],
					       sys_decls
					     ]
					   ]
					 ],
					 
					 [ setq,
					   sys_xx_dl_xx,
					   
					   [ '#BQ',
					     
					     [ c38_aux,
					       ['#COMMA', sys_env],
					       ['#COMMA', sys_whole]
					     ]
					   ]
					 ],
					 
					 [ setq,
					   sys_ppn,
					   [sys_dm_vl, sys_vl, sys_whole, t]
					 ],
					 
					 [ dolist,
					   [sys_kc, sys_xx_key_check_xx],
					   
					   [ push,
					     
					     [ '#BQ',
					       
					       [ unless,
						 
						 [ getf,
						   ['#COMMA', [car, sys_kc]],
						   kw_allow_other_keys
						 ],
						 
						 [ do,
						   
						   [ 
						     [ sys_vl,
						       ['#COMMA', [car, sys_kc]],
						       [cddr, sys_vl]
						     ]
						   ],
						   [[endp, sys_vl]],
						   
						   [ unless,
						     
						     [ member,
						       [car, sys_vl],
						       
						       [ quote,
							 
							 [ '#COMMA',
							   [cdr, sys_kc]
							 ]
						       ]
						     ],
						     
						     [ sys_dm_key_not_allowed,
						       [car, sys_vl]
						     ]
						   ]
						 ]
					       ]
					     ],
					     sys_body
					   ]
					 ],
					 
					 [ dolist,
					   [sys_ac, sys_xx_arg_check_xx],
					   
					   [ push,
					     
					     [ '#BQ',
					       
					       [ unless,
						 
						 [ endp,
						   
						   [ '#COMMA',
						     
						     [ sys_dm_nth_cdr,
						       [cdr, sys_ac],
						       [car, sys_ac]
						     ]
						   ]
						 ],
						 [sys_dm_too_many_arguments]
					       ]
					     ],
					     sys_body
					   ]
					 ],
					 
					 [ values,
					   
					   [ '#BQ',
					     
					     [ sys_lambda_block,
					       ['#COMMA', sys_name],
					       
					       [ '#COMMA',
						 [nreverse, sys_xx_dl_xx]
					       ],
					       
					       [ '#BQ-COMMA-ELIPSE',
						 [nconc, sys_decls, sys_body]
					       ]
					     ]
					   ],
					   sys_doc,
					   sys_ppn
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_expand_defmacro,
			  arglist_info(sys_expand_defmacro,
				       f_sys_expand_defmacro,
				       
				       [ sys_name,
					 sys_vl,
					 sys_body,
					 c38_aux,
					 sys_xx_dl_xx,
					 [sys_xx_key_check_xx, []],
					 [sys_xx_arg_check_xx, []],
					 sys_doc,
					 sys_decls,
					 sys_whole,
					 sys_ppn,
					 [sys_env, []],
					 [sys_envp, []]
				       ],
				       arginfo{ all:[sys_name, sys_vl, sys_body],
						allow_other_keys:0,
						aux:
						    [ sys_xx_dl_xx,
						      sys_xx_key_check_xx,
						      sys_xx_arg_check_xx,
						      sys_doc,
						      sys_decls,
						      sys_whole,
						      sys_ppn,
						      sys_env,
						      sys_envp
						    ],
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_vl,
							sys_body,
							sys_xx_dl_xx,
							sys_xx_key_check_xx,
							sys_xx_arg_check_xx,
							sys_doc,
							sys_decls,
							sys_whole,
							sys_ppn,
							sys_env,
							sys_envp
						      ],
						opt:0,
						req:[sys_name, sys_vl, sys_body],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_expand_defmacro,
			  init_args(3, f_sys_expand_defmacro))).
*/
/*
(defun dm-bad-key (key)
       (error "Defmacro-lambda-list contains illegal use of fmt90_x1." key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:6513 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'dm-bad-key',[key],[error,'$STRING'("Defmacro-lambda-list contains illegal use of ~s."),key]])
wl:lambda_def(defun, sys_dm_bad_key, f_sys_dm_bad_key, [key], [[error, '$ARRAY'([*], claz_base_character, "Defmacro-lambda-list contains illegal use of ~s."), key]]).
wl:arglist_info(sys_dm_bad_key, f_sys_dm_bad_key, [key], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[key], opt:0, req:[key], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_dm_bad_key).

/*

### Compiled Function: `SYS::DM-BAD-KEY` 
*/
f_sys_dm_bad_key(Key_In, FnResult) :-
	GEnv=[bv(key, Key_In)],
	catch(( ( get_var(GEnv, key, Key_Get),
		  f_error(
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "Defmacro-lambda-list contains illegal use of ~s."),
			    Key_Get
			  ],
			  Error_Ret)
		),
		Error_Ret=FnResult
	      ),
	      block_exit(sys_dm_bad_key, FnResult),
	      true).
:- set_opv(sys_dm_bad_key, symbol_function, f_sys_dm_bad_key),
   DefunResult=sys_dm_bad_key.
/*
:- side_effect(assert_lsp(sys_dm_bad_key,
			  lambda_def(defun,
				     sys_dm_bad_key,
				     f_sys_dm_bad_key,
				     [key],
				     
				     [ 
				       [ error,
					 '$ARRAY'([*],
						  claz_base_character,
						  "Defmacro-lambda-list contains illegal use of ~s."),
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_dm_bad_key,
			  arglist_info(sys_dm_bad_key,
				       f_sys_dm_bad_key,
				       [key],
				       arginfo{ all:[key],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[key],
						opt:0,
						req:[key],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_dm_bad_key, init_args(x, f_sys_dm_bad_key))).
*/
/*
(defun dm-too-few-arguments ()
       (error "Too few arguments are supplied to defmacro-lambda-list."))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:6609 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'dm-too-few-arguments',[],[error,'$STRING'("Too few arguments are supplied to defmacro-lambda-list.")]])
wl:lambda_def(defun, sys_dm_too_few_arguments, f_sys_dm_too_few_arguments, [], [[error, '$ARRAY'([*], claz_base_character, "Too few arguments are supplied to defmacro-lambda-list.")]]).
wl:arglist_info(sys_dm_too_few_arguments, f_sys_dm_too_few_arguments, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_dm_too_few_arguments).

/*

### Compiled Function: `SYS::DM-TOO-FEW-ARGUMENTS` 
*/
f_sys_dm_too_few_arguments(FnResult) :-
	_3364=[],
	catch(( f_error(
			[ '$ARRAY'([*],
				   claz_base_character,
				   "Too few arguments are supplied to defmacro-lambda-list.")
			],
			Error_Ret),
		Error_Ret=FnResult
	      ),
	      block_exit(sys_dm_too_few_arguments, FnResult),
	      true).
:- set_opv(sys_dm_too_few_arguments,
	   symbol_function,
	   f_sys_dm_too_few_arguments),
   DefunResult=sys_dm_too_few_arguments.
/*
:- side_effect(assert_lsp(sys_dm_too_few_arguments,
			  lambda_def(defun,
				     sys_dm_too_few_arguments,
				     f_sys_dm_too_few_arguments,
				     [],
				     
				     [ 
				       [ error,
					 '$ARRAY'([*],
						  claz_base_character,
						  "Too few arguments are supplied to defmacro-lambda-list.")
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_dm_too_few_arguments,
			  arglist_info(sys_dm_too_few_arguments,
				       f_sys_dm_too_few_arguments,
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
:- side_effect(assert_lsp(sys_dm_too_few_arguments,
			  init_args(x, f_sys_dm_too_few_arguments))).
*/
/*
(defun dm-too-many-arguments ()
       (error "Too many arguments are supplied to defmacro-lambda-list."))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:6715 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'dm-too-many-arguments',[],[error,'$STRING'("Too many arguments are supplied to defmacro-lambda-list.")]])
wl:lambda_def(defun, sys_dm_too_many_arguments, f_sys_dm_too_many_arguments, [], [[error, '$ARRAY'([*], claz_base_character, "Too many arguments are supplied to defmacro-lambda-list.")]]).
wl:arglist_info(sys_dm_too_many_arguments, f_sys_dm_too_many_arguments, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_dm_too_many_arguments).

/*

### Compiled Function: `SYS::DM-TOO-MANY-ARGUMENTS` 
*/
f_sys_dm_too_many_arguments(FnResult) :-
	_3364=[],
	catch(( f_error(
			[ '$ARRAY'([*],
				   claz_base_character,
				   "Too many arguments are supplied to defmacro-lambda-list.")
			],
			Error_Ret),
		Error_Ret=FnResult
	      ),
	      block_exit(sys_dm_too_many_arguments, FnResult),
	      true).
:- set_opv(sys_dm_too_many_arguments,
	   symbol_function,
	   f_sys_dm_too_many_arguments),
   DefunResult=sys_dm_too_many_arguments.
/*
:- side_effect(assert_lsp(sys_dm_too_many_arguments,
			  lambda_def(defun,
				     sys_dm_too_many_arguments,
				     f_sys_dm_too_many_arguments,
				     [],
				     
				     [ 
				       [ error,
					 '$ARRAY'([*],
						  claz_base_character,
						  "Too many arguments are supplied to defmacro-lambda-list.")
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_dm_too_many_arguments,
			  arglist_info(sys_dm_too_many_arguments,
				       f_sys_dm_too_many_arguments,
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
:- side_effect(assert_lsp(sys_dm_too_many_arguments,
			  init_args(x, f_sys_dm_too_many_arguments))).
*/
/*
(defun dm-key-not-allowed (key)
       (error "The key fmt90_x1 is not allowed." key))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:6823 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'dm-key-not-allowed',[key],[error,'$STRING'("The key ~s is not allowed."),key]])
wl:lambda_def(defun, sys_dm_key_not_allowed, f_sys_dm_key_not_allowed, [key], [[error, '$ARRAY'([*], claz_base_character, "The key ~s is not allowed."), key]]).
wl:arglist_info(sys_dm_key_not_allowed, f_sys_dm_key_not_allowed, [key], arginfo{all:[key], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[key], opt:0, req:[key], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_dm_key_not_allowed).

/*

### Compiled Function: `SYS::DM-KEY-NOT-ALLOWED` 
*/
f_sys_dm_key_not_allowed(Key_In, FnResult) :-
	GEnv=[bv(key, Key_In)],
	catch(( ( get_var(GEnv, key, Key_Get),
		  f_error(
			  [ '$ARRAY'([*],
				     claz_base_character,
				     "The key ~s is not allowed."),
			    Key_Get
			  ],
			  Error_Ret)
		),
		Error_Ret=FnResult
	      ),
	      block_exit(sys_dm_key_not_allowed, FnResult),
	      true).
:- set_opv(sys_dm_key_not_allowed, symbol_function, f_sys_dm_key_not_allowed),
   DefunResult=sys_dm_key_not_allowed.
/*
:- side_effect(assert_lsp(sys_dm_key_not_allowed,
			  lambda_def(defun,
				     sys_dm_key_not_allowed,
				     f_sys_dm_key_not_allowed,
				     [key],
				     
				     [ 
				       [ error,
					 '$ARRAY'([*],
						  claz_base_character,
						  "The key ~s is not allowed."),
					 key
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_dm_key_not_allowed,
			  arglist_info(sys_dm_key_not_allowed,
				       f_sys_dm_key_not_allowed,
				       [key],
				       arginfo{ all:[key],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[key],
						opt:0,
						req:[key],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_dm_key_not_allowed,
			  init_args(x, f_sys_dm_key_not_allowed))).
*/
/*
(defun find-doc (body ignore-doc)
  (if (endp body)
      (values nil nil nil)
      (let ((d (macroexpand (car body))))
        (cond ((stringp d)
               (if (or (endp (cdr body)) ignore-doc)
                   (values nil nil (cons d (cdr body)))
                   (multiple-value-bind (doc decls b) (find-doc (cdr body) t)
                     (declare (ignore doc))
                     (values d decls b))))
              ((and (consp d) (eq (car d) 'DECLARE))
               (multiple-value-bind (doc decls b)
                                    (find-doc (cdr body) ignore-doc)
                 (values doc (cons d decls) b)))
              (t (values nil nil (cons d (cdr body))))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:6905 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'find-doc',[body,'ignore-doc'],[if,[endp,body],[values,[],[],[]],[let,[[d,[macroexpand,[car,body]]]],[cond,[[stringp,d],[if,[or,[endp,[cdr,body]],'ignore-doc'],[values,[],[],[cons,d,[cdr,body]]],['multiple-value-bind',[doc,decls,b],['find-doc',[cdr,body],t],[declare,[ignore,doc]],[values,d,decls,b]]]],[[and,[consp,d],[eq,[car,d],[quote,'DECLARE']]],['multiple-value-bind',[doc,decls,b],['find-doc',[cdr,body],'ignore-doc'],[values,doc,[cons,d,decls],b]]],[t,[values,[],[],[cons,d,[cdr,body]]]]]]]])
wl:lambda_def(defun, sys_find_doc, f_sys_find_doc, [sys_body, sys_ignore_doc], [[if, [endp, sys_body], [values, [], [], []], [let, [[sys_d, [macroexpand, [car, sys_body]]]], [cond, [[stringp, sys_d], [if, [or, [endp, [cdr, sys_body]], sys_ignore_doc], [values, [], [], [cons, sys_d, [cdr, sys_body]]], [multiple_value_bind, [sys_doc, sys_decls, sys_b], [sys_find_doc, [cdr, sys_body], t], [declare, [ignore, sys_doc]], [values, sys_d, sys_decls, sys_b]]]], [[and, [consp, sys_d], [eq, [car, sys_d], [quote, declare]]], [multiple_value_bind, [sys_doc, sys_decls, sys_b], [sys_find_doc, [cdr, sys_body], sys_ignore_doc], [values, sys_doc, [cons, sys_d, sys_decls], sys_b]]], [t, [values, [], [], [cons, sys_d, [cdr, sys_body]]]]]]]]).
wl:arglist_info(sys_find_doc, f_sys_find_doc, [sys_body, sys_ignore_doc], arginfo{all:[sys_body, sys_ignore_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_body, sys_ignore_doc], opt:0, req:[sys_body, sys_ignore_doc], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_find_doc).

/*

### Compiled Function: `SYS::FIND-DOC` 
*/
f_sys_find_doc(Body_In, Ignore_doc_In, FnResult) :-
	GEnv=[bv(sys_body, Body_In), bv(sys_ignore_doc, Ignore_doc_In)],
	catch(( ( get_var(GEnv, sys_body, Body_Get),
		  (   s3q:is_endp(Body_Get)
		  ->  nb_setval('$mv_return', [[], [], []]),
		      _4898=[]
		  ;   get_var(GEnv, sys_body, Body_Get13),
		      f_car(Body_Get13, Car_Ret),
		      f_macroexpand([Car_Ret], D_Init),
		      LEnv=[bv(sys_d, D_Init)|GEnv],
		      get_var(LEnv, sys_d, D_Get),
		      (   is_stringp(D_Get)
		      ->  (   get_var(LEnv, sys_body, Body_Get21),
			      f_cdr(Body_Get21, Endp_Param),
			      f_endp(Endp_Param, FORM1_Res),
			      FORM1_Res\==[],
			      IFTEST19=FORM1_Res
			  ->  true
			  ;   get_var(LEnv, sys_ignore_doc, Ignore_doc_Get),
			      IFTEST19=Ignore_doc_Get
			  ),
			  (   IFTEST19\==[]
			  ->  get_var(LEnv, sys_body, Body_Get25),
			      get_var(LEnv, sys_d, D_Get24),
			      f_cdr(Body_Get25, Cdr_Ret),
			      CAR=[D_Get24|Cdr_Ret],
			      nb_setval('$mv_return', [[], [], CAR]),
			      TrueResult50=[]
			  ;   LEnv28=[bv(sys_doc, []), bv(sys_decls, []), bv(sys_b, [])|LEnv],
			      get_var(LEnv28, sys_body, Body_Get29),
			      f_cdr(Body_Get29, Find_doc_Param),
			      f_sys_find_doc(Find_doc_Param, t, T),
			      setq_from_values(LEnv28,
					       [sys_doc, sys_decls, sys_b]),
			      sf_declare(LEnv28,
					 [ignore, sys_doc],
					 Sf_declare_Ret),
			      get_var(LEnv28, sys_b, B_Get),
			      get_var(LEnv28, sys_decls, Decls_Get),
			      nb_setval('$mv_return', [sys_d, Decls_Get, B_Get]),
			      TrueResult50=sys_d
			  ),
			  LetResult=TrueResult50
		      ;   get_var(LEnv, sys_d, D_Get35),
			  (   c0nz:is_consp(D_Get35)
			  ->  get_var(LEnv, sys_d, D_Get38),
			      f_car(D_Get38, Eq_Param),
			      f_eq(Eq_Param, declare, TrueResult),
			      IFTEST32=TrueResult
			  ;   IFTEST32=[]
			  ),
			  (   IFTEST32\==[]
			  ->  LEnv42=[bv(sys_doc, []), bv(sys_decls, []), bv(sys_b, [])|LEnv],
			      get_var(LEnv42, sys_body, Body_Get43),
			      f_cdr(Body_Get43, Find_doc_Param60),
			      get_var(LEnv42, sys_ignore_doc, Ignore_doc_Get44),
			      f_sys_find_doc(Find_doc_Param60,
					     Ignore_doc_Get44,
					     Find_doc_Ret),
			      setq_from_values(LEnv42,
					       [sys_doc, sys_decls, sys_b]),
			      get_var(LEnv42, sys_d, D_Get45),
			      get_var(LEnv42, sys_decls, Decls_Get46),
			      CAR66=[D_Get45|Decls_Get46],
			      get_var(LEnv42, sys_b, B_Get47),
			      nb_setval('$mv_return', [sys_doc, CAR66, B_Get47]),
			      ElseResult=sys_doc
			  ;   get_var(LEnv, sys_body, Body_Get49),
			      get_var(LEnv, sys_d, D_Get48),
			      f_cdr(Body_Get49, Cdr_Ret67),
			      CAR68=[D_Get48|Cdr_Ret67],
			      nb_setval('$mv_return', [[], [], CAR68]),
			      ElseResult=[]
			  ),
			  LetResult=ElseResult
		      ),
		      _4898=LetResult
		  )
		),
		_4898=FnResult
	      ),
	      block_exit(sys_find_doc, FnResult),
	      true).
:- set_opv(sys_find_doc, symbol_function, f_sys_find_doc),
   DefunResult=sys_find_doc.
/*
:- side_effect(assert_lsp(sys_find_doc,
			  lambda_def(defun,
				     sys_find_doc,
				     f_sys_find_doc,
				     [sys_body, sys_ignore_doc],
				     
				     [ 
				       [ if,
					 [endp, sys_body],
					 [values, [], [], []],
					 
					 [ let,
					   
					   [ 
					     [ sys_d,
					       [macroexpand, [car, sys_body]]
					     ]
					   ],
					   
					   [ cond,
					     
					     [ [stringp, sys_d],
					       
					       [ if,
						 
						 [ or,
						   [endp, [cdr, sys_body]],
						   sys_ignore_doc
						 ],
						 
						 [ values,
						   [],
						   [],
						   [cons, sys_d, [cdr, sys_body]]
						 ],
						 
						 [ multiple_value_bind,
						   [sys_doc, sys_decls, sys_b],
						   
						   [ sys_find_doc,
						     [cdr, sys_body],
						     t
						   ],
						   [declare, [ignore, sys_doc]],
						   
						   [ values,
						     sys_d,
						     sys_decls,
						     sys_b
						   ]
						 ]
					       ]
					     ],
					     
					     [ 
					       [ and,
						 [consp, sys_d],
						 
						 [ eq,
						   [car, sys_d],
						   [quote, declare]
						 ]
					       ],
					       
					       [ multiple_value_bind,
						 [sys_doc, sys_decls, sys_b],
						 
						 [ sys_find_doc,
						   [cdr, sys_body],
						   sys_ignore_doc
						 ],
						 
						 [ values,
						   sys_doc,
						   [cons, sys_d, sys_decls],
						   sys_b
						 ]
					       ]
					     ],
					     
					     [ t,
					       
					       [ values,
						 [],
						 [],
						 [cons, sys_d, [cdr, sys_body]]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_find_doc,
			  arglist_info(sys_find_doc,
				       f_sys_find_doc,
				       [sys_body, sys_ignore_doc],
				       arginfo{ all:[sys_body, sys_ignore_doc],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_body,
							sys_ignore_doc
						      ],
						opt:0,
						req:[sys_body, sys_ignore_doc],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_find_doc, init_args(x, f_sys_find_doc))).
*/
/*
(defun find-declarations (body)
  (if (endp body)
      (values nil nil)
      (let ((d (macroexpand (car body))))
        (cond ((stringp d)
               (if (endp (cdr body))
                   (values nil (list d))
                   (multiple-value-bind (ds b)
                       (find-declarations (cdr body))
                     (values (cons d ds) b))))
              ((and (consp d) (eq (car d) 'DECLARE))
               (multiple-value-bind (ds b)
                   (find-declarations (cdr body))
                 (values (cons d ds) b)))
              (t
               (values nil (cons d (cdr body))))))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defmacro.lsp:7608 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'find-declarations',[body],[if,[endp,body],[values,[],[]],[let,[[d,[macroexpand,[car,body]]]],[cond,[[stringp,d],[if,[endp,[cdr,body]],[values,[],[list,d]],['multiple-value-bind',[ds,b],['find-declarations',[cdr,body]],[values,[cons,d,ds],b]]]],[[and,[consp,d],[eq,[car,d],[quote,'DECLARE']]],['multiple-value-bind',[ds,b],['find-declarations',[cdr,body]],[values,[cons,d,ds],b]]],[t,[values,[],[cons,d,[cdr,body]]]]]]]])
wl:lambda_def(defun, sys_find_declarations, f_sys_find_declarations, [sys_body], [[if, [endp, sys_body], [values, [], []], [let, [[sys_d, [macroexpand, [car, sys_body]]]], [cond, [[stringp, sys_d], [if, [endp, [cdr, sys_body]], [values, [], [list, sys_d]], [multiple_value_bind, [sys_ds, sys_b], [sys_find_declarations, [cdr, sys_body]], [values, [cons, sys_d, sys_ds], sys_b]]]], [[and, [consp, sys_d], [eq, [car, sys_d], [quote, declare]]], [multiple_value_bind, [sys_ds, sys_b], [sys_find_declarations, [cdr, sys_body]], [values, [cons, sys_d, sys_ds], sys_b]]], [t, [values, [], [cons, sys_d, [cdr, sys_body]]]]]]]]).
wl:arglist_info(sys_find_declarations, f_sys_find_declarations, [sys_body], arginfo{all:[sys_body], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_body], opt:0, req:[sys_body], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_find_declarations).

/*

### Compiled Function: `SYS::FIND-DECLARATIONS` 
*/
f_sys_find_declarations(Body_In, FnResult) :-
	GEnv=[bv(sys_body, Body_In)],
	catch(( ( get_var(GEnv, sys_body, Body_Get),
		  (   s3q:is_endp(Body_Get)
		  ->  nb_setval('$mv_return', [[], []]),
		      _4648=[]
		  ;   get_var(GEnv, sys_body, Body_Get12),
		      f_car(Body_Get12, Car_Ret),
		      f_macroexpand([Car_Ret], D_Init),
		      LEnv=[bv(sys_d, D_Init)|GEnv],
		      get_var(LEnv, sys_d, D_Get),
		      (   is_stringp(D_Get)
		      ->  get_var(LEnv, sys_body, Body_Get19),
			  f_cdr(Body_Get19, PredArgResult21),
			  (   s3q:is_endp(PredArgResult21)
			  ->  get_var(LEnv, sys_d, D_Get22),
			      CAR=[D_Get22],
			      nb_setval('$mv_return', [[], CAR]),
			      TrueResult49=[]
			  ;   LEnv25=[bv(sys_ds, []), bv(sys_b, [])|LEnv],
			      get_var(LEnv25, sys_body, Body_Get26),
			      f_cdr(Body_Get26, Find_declarations_Param),
			      f_sys_find_declarations(Find_declarations_Param,
						      Find_declarations_Ret),
			      setq_from_values(LEnv25, [sys_ds, sys_b]),
			      get_var(LEnv25, sys_d, D_Get27),
			      get_var(LEnv25, sys_ds, Ds_Get),
			      LetResult24=[D_Get27|Ds_Get],
			      get_var(LEnv25, sys_b, B_Get),
			      nb_setval('$mv_return', [LetResult24, B_Get]),
			      TrueResult49=LetResult24
			  ),
			  LetResult=TrueResult49
		      ;   get_var(LEnv, sys_d, D_Get34),
			  (   c0nz:is_consp(D_Get34)
			  ->  get_var(LEnv, sys_d, D_Get37),
			      f_car(D_Get37, Eq_Param),
			      f_eq(Eq_Param, declare, TrueResult),
			      IFTEST31=TrueResult
			  ;   IFTEST31=[]
			  ),
			  (   IFTEST31\==[]
			  ->  LEnv41=[bv(sys_ds, []), bv(sys_b, [])|LEnv],
			      get_var(LEnv41, sys_body, Body_Get42),
			      f_cdr(Body_Get42, Find_declarations_Param57),
			      f_sys_find_declarations(Find_declarations_Param57,
						      Find_declarations_Ret61),
			      setq_from_values(LEnv41, [sys_ds, sys_b]),
			      get_var(LEnv41, sys_d, D_Get43),
			      get_var(LEnv41, sys_ds, Ds_Get44),
			      LetResult40=[D_Get43|Ds_Get44],
			      get_var(LEnv41, sys_b, B_Get45),
			      nb_setval('$mv_return', [LetResult40, B_Get45]),
			      ElseResult50=LetResult40
			  ;   get_var(LEnv, sys_body, Body_Get47),
			      get_var(LEnv, sys_d, D_Get46),
			      f_cdr(Body_Get47, Cdr_Ret),
			      CAR63=[D_Get46|Cdr_Ret],
			      nb_setval('$mv_return', [[], CAR63]),
			      ElseResult50=[]
			  ),
			  LetResult=ElseResult50
		      ),
		      _4648=LetResult
		  )
		),
		_4648=FnResult
	      ),
	      block_exit(sys_find_declarations, FnResult),
	      true).
:- set_opv(sys_find_declarations, symbol_function, f_sys_find_declarations),
   DefunResult=sys_find_declarations.
/*
:- side_effect(assert_lsp(sys_find_declarations,
			  lambda_def(defun,
				     sys_find_declarations,
				     f_sys_find_declarations,
				     [sys_body],
				     
				     [ 
				       [ if,
					 [endp, sys_body],
					 [values, [], []],
					 
					 [ let,
					   
					   [ 
					     [ sys_d,
					       [macroexpand, [car, sys_body]]
					     ]
					   ],
					   
					   [ cond,
					     
					     [ [stringp, sys_d],
					       
					       [ if,
						 [endp, [cdr, sys_body]],
						 [values, [], [list, sys_d]],
						 
						 [ multiple_value_bind,
						   [sys_ds, sys_b],
						   
						   [ sys_find_declarations,
						     [cdr, sys_body]
						   ],
						   
						   [ values,
						     [cons, sys_d, sys_ds],
						     sys_b
						   ]
						 ]
					       ]
					     ],
					     
					     [ 
					       [ and,
						 [consp, sys_d],
						 
						 [ eq,
						   [car, sys_d],
						   [quote, declare]
						 ]
					       ],
					       
					       [ multiple_value_bind,
						 [sys_ds, sys_b],
						 
						 [ sys_find_declarations,
						   [cdr, sys_body]
						 ],
						 
						 [ values,
						   [cons, sys_d, sys_ds],
						   sys_b
						 ]
					       ]
					     ],
					     
					     [ t,
					       
					       [ values,
						 [],
						 [cons, sys_d, [cdr, sys_body]]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_find_declarations,
			  arglist_info(sys_find_declarations,
				       f_sys_find_declarations,
				       [sys_body],
				       arginfo{ all:[sys_body],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_body],
						opt:0,
						req:[sys_body],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_find_declarations,
			  init_args(x, f_sys_find_declarations))).
*/


%; Total compilation time: 15.183 seconds


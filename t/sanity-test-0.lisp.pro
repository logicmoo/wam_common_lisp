#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/logicmoo/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "sanity-test-0" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sat Aug 17 13:31:07 2013

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*

;; $Id: examples.lisp,v 1.1 2003/10/21 17:30:56 nhabedi Exp $
;;                          EXAMPLES.LISP
;;           Nick Levine, Ravenbrook Limited, 2003-08-14
;; 
;; These are the examples I expect to use in the tutorial on CLOS
;; at the International Lisp Conference 2003.
;; 
;; This document is mainly for my operational convenience. You might
;; want to raid fragments to help you get started when building CLOS
;; into your Common Lisp applications. Nothing useful will happen if
;; you try to cl:load this document into a lisp image.
;;
;; This document is provided "as is", without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this document.  You may make and
;; distribute verbatim copies of this document provided that you do
;; not charge a fee for this document or for its distribution.
*/
/*
 #+WAM-CL (prolog-call "cls.")
*/
/*
(defun mapcar-visualize (func l) (if (null l) () (cons (apply func (list (first l))) (mapcar func (rest l)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:919 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'mapcar-visualize',[func,l],[if,[null,l],[],[cons,[apply,func,[list,[first,l]]],[mapcar,func,[rest,l]]]]])
wl:lambda_def(defun, u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]).
wl:arglist_info(u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_u_mapcar_visualize).

/*

### Compiled:  `U::MAPCAR-VISUALIZE` 
*/
f_u_mapcar_visualize(Func_In, L_In, FnResult) :-
	GEnv=[bv(u_func, Func_In), bv(u_l, L_In)],
	catch(( ( get_var(GEnv, u_l, IFTEST),
		  (   IFTEST==[]
		  ->  _15702=[]
		  ;   get_var(GEnv, u_func, Func_Get),
		      get_var(GEnv, u_l, L_Get10),
		      f_car(L_Get10, Car_Ret),
		      f_apply(Func_Get, [Car_Ret], Apply_Ret),
		      get_var(GEnv, u_func, Func_Get11),
		      get_var(GEnv, u_l, L_Get12),
		      f_cdr(L_Get12, Cdr_Ret),
		      f_mapcar(Func_Get11, [Cdr_Ret], Mapcar_Ret),
		      ElseResult=[Apply_Ret|Mapcar_Ret],
		      _15702=ElseResult
		  )
		),
		_15702=FnResult
	      ),
	      block_exit(u_mapcar_visualize, FnResult),
	      true).
:- set_opv(u_mapcar_visualize, symbol_function, f_u_mapcar_visualize),
   set_opv(f_u_mapcar_visualize, type_of, compiled_function),
   DefunResult=u_mapcar_visualize.
/*
:- side_effect(assert_lsp(u_mapcar_visualize,
			  lambda_def(defun,
				     u_mapcar_visualize,
				     f_u_mapcar_visualize,
				     [u_func, u_l],
				     
				     [ 
				       [ if,
					 [null, u_l],
					 [],
					 
					 [ cons,
					   [apply, u_func, [list, [first, u_l]]],
					   [mapcar, u_func, [rest, u_l]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(u_mapcar_visualize,
			  arglist_info(u_mapcar_visualize,
				       f_u_mapcar_visualize,
				       [u_func, u_l],
				       arginfo{ all:[u_func, u_l],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[u_func, u_l],
						opt:0,
						req:[u_func, u_l],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(u_mapcar_visualize,
			  init_args(x, f_u_mapcar_visualize))).
*/
/*
(in-package "CL-USER")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1031 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','$STRING'("CL-USER")])
/*
% macroexpand:-[in_package,'$ARRAY'([*],claz_base_character,"CL-USER")].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"CL-USER")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "CL-USER"),
				_Ignored),
	   _Ignored).
/*
(load "sanity-util")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1055 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("sanity-util")])
:- f_load('$ARRAY'([*], claz_base_character, "sanity-util"), [], _Ignored).
/*
'(require 'sanity-util)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1076 **********************/
:-lisp_compile_to_prolog(pkg_user,[quote,[require,[quote,'sanity-util']]])
/*
(write-line "Running smoke test!")

; (progn (prolog-inline "rtrace") (is eq 1 1))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1101 **********************/
:-lisp_compile_to_prolog(pkg_user,['write-line','$STRING'("Running smoke test!")])
:- f_write_line('$ARRAY'([*], claz_base_character, "Running smoke test!"),
		_Ignored).
/*
 (progn (prolog-inline "rtrace") (is eq 1 1))
*/
/*
(is eq 1 1)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1184 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,1])
:- mf_u_is([u_is, eq, 1, 1], _15336, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is equal (list 1 'a 'b) (cons 1 '(a b)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1196 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,1,[quote,a],[quote,b]],[cons,1,[quote,[a,b]]]])
:- mf_u_is(
	   [ u_is,
	     equal,
	     [list, 1, [quote, u_a], [quote, u_b]],
	     [cons, 1, [quote, [u_a, u_b]]]
	   ],
	   _15520,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq 2 (if nil 1 2))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1239 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]])
:- mf_u_is([u_is, eq, 2, [if, [], 1, 2]], _15384, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq t (= 1.0 1))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1263 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[=,1.0,1]])
:- mf_u_is([u_is, eq, t, [=, 1.0, 1]], _15376, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq nil (equal 1.0 1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1283 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],[equal,1.0,1]])
:- mf_u_is([u_is, eq, [], [equal, 1.0, 1]], _15378, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq t (keywordp :k))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1310 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[keywordp,':k']])
:- mf_u_is([u_is, eq, t, [keywordp, kw_k]], _15360, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq 10 (if t 10 20))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1335 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,10,[if,t,10,20]])
:- mf_u_is([u_is, eq, 10, [if, t, 10, 20]], _15384, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq t (stringp "abc"))

;;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2"(is eq t (stringp \"abc\"))\n\n;;  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1360 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[stringp,'$STRING'("abc")]])
:- mf_u_is([u_is, eq, t, [stringp, '$ARRAY'([*], claz_base_character, "abc")]],
	   _19256,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).".
*/
/*
(is equal (subseq "abc" 1) "bc")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1484 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[subseq,'$STRING'("abc"),1],'$STRING'("bc")])
:- mf_u_is(
	   [ u_is,
	     equal,
	     [subseq, '$ARRAY'([*], claz_base_character, "abc"), 1],
	     '$ARRAY'([*], claz_base_character, "bc")
	   ],
	   _15402,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq 1 (if t 1 2))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1518 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[if,t,1,2]])
:- mf_u_is([u_is, eq, 1, [if, t, 1, 2]], _15382, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq 2 (if nil 1 2))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1539 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]])
:- mf_u_is([u_is, eq, 2, [if, [], 1, 2]], _15384, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1563 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,fib,[n],[if,[>,n,1],[+,[fib,[-,n,1]],[fib,[-,n,2]]],1]])
wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).
wl:arglist_info(u_fib, f_u_fib, [n], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_u_fib).

/*

### Compiled:  `U::FIB` 
*/
f_u_fib(N_In, FnResult) :-
	GEnv=[bv(n, N_In)],
	catch(( ( get_var(GEnv, n, N_Get),
		  (   N_Get<1
		  ->  get_var(GEnv, n, N_Get9),
		      'f_-'(N_Get9, 1, Fib_Param),
		      f_u_fib(Fib_Param, Fib_Ret),
		      get_var(GEnv, n, N_Get10),
		      'f_-'(N_Get10, 2, Fib_Param16),
		      f_u_fib(Fib_Param16, Fib_Ret18),
		      'f_+'(Fib_Ret, Fib_Ret18, TrueResult),
		      _15656=TrueResult
		  ;   _15656=1
		  )
		),
		_15656=FnResult
	      ),
	      block_exit(u_fib, FnResult),
	      true).
:- set_opv(u_fib, symbol_function, f_u_fib),
   set_opv(f_u_fib, type_of, compiled_function),
   DefunResult=u_fib.
/*
:- side_effect(assert_lsp(u_fib,
			  lambda_def(defun,
				     u_fib,
				     f_u_fib,
				     [n],
				     
				     [ 
				       [ if,
					 [>, n, 1],
					 [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]],
					 1
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(u_fib,
			  arglist_info(u_fib,
				       f_u_fib,
				       [n],
				       arginfo{ all:[n],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[n],
						opt:0,
						req:[n],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(u_fib, init_args(x, f_u_fib))).
*/
/*
(disassemble #'fib)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1644 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fib)])
:- f_disassemble(f_u_fib, _Ignored).
/*
(is eql 89 (fib 10))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1666 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,89,[fib,10]])
:- mf_u_is([u_is, eql, 89, [u_fib, 10]], _15378, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(defun accum (r) (if (= 0 r) (list 0) (cons r (accum (- r 1)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1690 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,accum,[r],[if,[=,0,r],[list,0],[cons,r,[accum,[-,r,1]]]]])
wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).
wl:arglist_info(u_accum, f_u_accum, [u_r], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_u_accum).

/*

### Compiled:  `U::ACCUM` 
*/
f_u_accum(R_In, FnResult) :-
	GEnv=[bv(u_r, R_In)],
	catch(( ( get_var(GEnv, u_r, R_Get),
		  (   0=:=R_Get
		  ->  TrueResult=[0],
		      _15616=TrueResult
		  ;   get_var(GEnv, u_r, R_Get9),
		      'f_-'(R_Get9, 1, Accum_Param),
		      f_u_accum(Accum_Param, Accum_Ret),
		      ElseResult=[R_Get9|Accum_Ret],
		      _15616=ElseResult
		  )
		),
		_15616=FnResult
	      ),
	      block_exit(u_accum, FnResult),
	      true).
:- set_opv(u_accum, symbol_function, f_u_accum),
   set_opv(f_u_accum, type_of, compiled_function),
   DefunResult=u_accum.
/*
:- side_effect(assert_lsp(u_accum,
			  lambda_def(defun,
				     u_accum,
				     f_u_accum,
				     [u_r],
				     
				     [ 
				       [ if,
					 [=, 0, u_r],
					 [list, 0],
					 [cons, u_r, [u_accum, [-, u_r, 1]]]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(u_accum,
			  arglist_info(u_accum,
				       f_u_accum,
				       [u_r],
				       arginfo{ all:[u_r],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[u_r],
						opt:0,
						req:[u_r],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(u_accum, init_args(x, f_u_accum))).
*/
/*
(disassemble #'accum)
#| DISASSEMBLY FOR:f_u_accum
:- dynamic f_u_accum/2.

f_u_accum(A, G) :-
	(   0=:=A
	->  G=[0]
	;   C is A - 1,
	    f_u_accum(C, D),
	    G=[A|D]
	).

|#
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1756 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(accum)])
:- f_disassemble(f_u_accum, _Ignored).
/*
 DISASSEMBLY FOR:f_u_accum
:- dynamic f_u_accum/2.

f_u_accum(A, G) :-
	(   0=:=A
	->  G=[0]
	;   C is A - 1,
	    f_u_accum(C, D),
	    G=[A|D]
	).

*/
/*
(is equal (list 4 3 2 1 0) (accum 4))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1933 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,4,3,2,1,0],[accum,4]])
:- mf_u_is([u_is, equal, [list, 4, 3, 2, 1, 0], [u_accum, 4]], _15442, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(defmacro defwrap (name) `(defun ,name () 1))
;;; :- ensure_loaded('sanity-test.lisp.trans.pl').
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1972 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,defwrap,[name],['#BQ',[defun,['#COMMA',name],[],1]]])
wl:lambda_def(defmacro, u_defwrap, mf_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]).
wl:arglist_info(u_defwrap, mf_u_defwrap, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(1, mf_u_defwrap).

/*

### Compiled:  `U::DEFWRAP` 
*/
mf_u_defwrap([u_defwrap, Name_In|RestNKeys], PassedMacroEnv, MFResult) :-
	nop(defmacro),
	as_passed_env(PassedMacroEnv, Passed_env_Ret),
	Env=[bv(sys_name, Name_In)|Passed_env_Ret],
	b_setval('$current_env', Env),
	catch(( get_var(Env, sys_name, Name_Get),
		[defun, Name_Get, [], 1]=MFResult
	      ),
	      block_exit(u_defwrap, MFResult),
	      true).
/*

### Compiled:  `U::DEFWRAP` 
*/
sf_u_defwrap(Name_In, RestNKeys, FnResult) :-
	current_env(PassedMacroEnv),
	mf_u_defwrap([u_defwrap, Name_In|RestNKeys], PassedMacroEnv, MFResult),
	f_eval(MFResult, FnResult).
:- set_opv(mf_u_defwrap, type_of, sys_macro),
   set_opv(u_defwrap, symbol_function, mf_u_defwrap),
   DefMacroResult=u_defwrap.
/*
:- side_effect(assert_lsp(u_defwrap,
			  lambda_def(defmacro,
				     u_defwrap,
				     mf_u_defwrap,
				     [sys_name],
				     
				     [ progn,
				       
				       [ '#BQ',
					 [defun, ['#COMMA', sys_name], [], 1]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(u_defwrap,
			  arglist_info(u_defwrap,
				       mf_u_defwrap,
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
:- side_effect(assert_lsp(u_defwrap, init_args(1, mf_u_defwrap))).
*/
/*
;; :- ensure_loaded('sanity-test.lisp.trans.pl').
*/
/*
(defwrap foo)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2069 **********************/
:-lisp_compile_to_prolog(pkg_user,[defwrap,foo])
/*
% macroexpand:-[u_defwrap,u_foo].
*/
/*
% into:-[defun,u_foo,[],1].
*/
wl:lambda_def(defun, u_foo, f_u_foo, [], [1]).
wl:arglist_info(u_foo, f_u_foo, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_u_foo).

/*

### Compiled:  `U::FOO` 
*/
f_u_foo(FnResult) :-
	_15326=[],
	catch(( true,
		1=FnResult
	      ),
	      block_exit(u_foo, FnResult),
	      true).
:- set_opv(u_foo, symbol_function, f_u_foo),
   set_opv(f_u_foo, type_of, compiled_function),
   DefunResult=u_foo.
/*
:- side_effect(assert_lsp(u_foo, lambda_def(defun, u_foo, f_u_foo, [], [1]))).
*/
/*
:- side_effect(assert_lsp(u_foo,
			  arglist_info(u_foo,
				       f_u_foo,
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
:- side_effect(assert_lsp(u_foo, init_args(x, f_u_foo))).
*/
/*
(is eq 1 (foo))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2083 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[foo]])
:- mf_u_is([u_is, eq, 1, [u_foo]], _15382, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is equal (macroexpand-1 '(defwrap foo)) '(defun foo nil 1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2099 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,['macroexpand-1',[quote,[defwrap,foo]]],[quote,[defun,foo,[],1]]])
:- mf_u_is(
	   [ u_is,
	     equal,
	     [macroexpand_1, [quote, [u_defwrap, u_foo]]],
	     [quote, [defun, u_foo, [], 1]]
	   ],
	   _15488,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(write-line "PASSED")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2161 **********************/
:-lisp_compile_to_prolog(pkg_user,['write-line','$STRING'("PASSED")])
:- f_write_line('$ARRAY'([*], claz_base_character, "PASSED"), _Ignored).
/*
(defun fifteen ()
  (let (val)
    (tagbody
      (setq val 1)
      (go point-a)
      (incf val 16)
     point-c
      (incf val 04)
      (go point-b)
      (incf val 32)
     point-a
     point-u ;; unused
      (incf val 02)
      (go point-c)
      (incf val 64)
     point-b
      (incf val 08))
    val))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2184 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,fifteen,[],[let,[val],[tagbody,[setq,val,1],[go,'point-a'],[incf,val,16],'point-c',[incf,val,4],[go,'point-b'],[incf,val,32],'point-a','point-u',[incf,val,2],[go,'point-c'],[incf,val,64],'point-b',[incf,val,8]],val]])
wl:lambda_def(defun,u_fifteen,f_u_fifteen,[],[[let,[u_val],[tagbody,[setq,u_val,1],[go,u_point_a],[incf,u_val,16],u_point_c,[incf,u_val,4],[go,u_point_b],[incf,u_val,32],u_point_a,u_point_u,[incf,u_val,2],[go,u_point_c],[incf,u_val,64],u_point_b,[incf,u_val,8]],u_val]]).
wl:arglist_info(u_fifteen,f_u_fifteen,[],arginfo{all:0,allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[],opt:0,req:0,rest:0,sublists:0,whole:0}).
wl:init_args(x,f_u_fifteen).

/*

### Compiled:  `U::FIFTEEN` 
*/
f_u_fifteen(_16388):-_15974=[],catch(((_16252=[bv(u_val,[])|_15974],call_addr_block(_16252,(set_var(_16252,u_val,1),goto(u_point_a,_16252)),[addr(addr_tagbody_3_u_point_c,u_point_c,'$used',_16308,(place_op(_16308,incf,u_val,symbol_value,[4],_16310),goto(u_point_b,_16308))),addr(addr_tagbody_3_u_point_a,u_point_a,'$used',_16314,(push_label(u_point_u),place_op(_16314,incf,u_val,symbol_value,[2],_18074),goto(u_point_c,_16314))),addr(addr_tagbody_3_u_point_u,u_point_u,'$unused',_16330,(place_op(_16330,incf,u_val,symbol_value,[2],_18102),goto(u_point_c,_16330))),addr(addr_tagbody_3_u_point_b,u_point_b,'$used',_18020,place_op(_18020,incf,u_val,symbol_value,[8],_16348))]),get_var(_16252,u_val,_16012)),_16012=_16388),block_exit(u_fifteen,_16388),true).
:-set_opv(u_fifteen,symbol_function,f_u_fifteen),set_opv(f_u_fifteen,type_of,compiled_function),_15948=u_fifteen.
/*
:-side_effect(assert_lsp(u_fifteen,lambda_def(defun,u_fifteen,f_u_fifteen,[],[[let,[u_val],[tagbody,[setq,u_val,1],[go,u_point_a],[incf,u_val,16],u_point_c,[incf,u_val,4],[go,u_point_b],[incf,u_val,32],u_point_a,u_point_u,[incf,u_val,2],[go,u_point_c],[incf,u_val,64],u_point_b,[incf,u_val,8]],u_val]]))).
*/
/*
:-side_effect(assert_lsp(u_fifteen,arglist_info(u_fifteen,f_u_fifteen,[],arginfo{all:0,allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[],opt:0,req:0,rest:0,sublists:0,whole:0}))).
*/
/*
:-side_effect(assert_lsp(u_fifteen,init_args(x,f_u_fifteen))).
*/
/*
; unused
*/
/*
(disassemble #'fifteen)

#|

/* this first one should get deleted since its inlined away in f_u_fifteen */

addr_tagbody_1_addr_enter_1(Env10) :-
        symbol_setter(Env10, setq, u_val, 1),
        addr_tagbody_1_u_point_a(Env10).
addr_tagbody_1_u_point_c(Incf_Env) :-
        place_op(Incf_Env, incf, [value, u_val], [4], Incf_R),
        addr_tagbody_1_u_point_b(Incf_Env).
addr_tagbody_1_u_point_a(Incf_Env19) :-
        place_op(Incf_Env19, incf, [value, u_val], [2], Incf_R18),
        addr_tagbody_1_u_point_c(Incf_Env19).
addr_tagbody_1_u_point_u(Incf_Env23) :-
        place_op(Incf_Env23, incf, [value, u_val], [2], Incf_R22),
        addr_tagbody_1_u_point_c(Incf_Env23).
addr_tagbody_1_u_point_b(Incf_Env27) :-
        place_op(Incf_Env27, incf, [value, u_val], [8], _GORES15).

f_u_fifteen(MResult) :-
        Env=[],
        catch(( TBEnv=[[bv(u_val, [])]|Env],
                symbol_setter(TBEnv, setq, u_val, 1),
                addr_tagbody_1_u_point_a(TBEnv),
                symbol_value(TBEnv, u_val, U_val_Get),
                U_val_Get=MResult
              ),
              block_exit(u_fifteen, MResult),
              true).

|#

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2498 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fifteen)])
:- f_disassemble(f_u_fifteen, _Ignored).
/*


/* this first one should get deleted since its inlined away in f_u_fifteen */

addr_tagbody_1_addr_enter_1(Env10) :-
        symbol_setter(Env10, setq, u_val, 1),
        addr_tagbody_1_u_point_a(Env10).
addr_tagbody_1_u_point_c(Incf_Env) :-
        place_op(Incf_Env, incf, [value, u_val], [4], Incf_R),
        addr_tagbody_1_u_point_b(Incf_Env).
addr_tagbody_1_u_point_a(Incf_Env19) :-
        place_op(Incf_Env19, incf, [value, u_val], [2], Incf_R18),
        addr_tagbody_1_u_point_c(Incf_Env19).
addr_tagbody_1_u_point_u(Incf_Env23) :-
        place_op(Incf_Env23, incf, [value, u_val], [2], Incf_R22),
        addr_tagbody_1_u_point_c(Incf_Env23).
addr_tagbody_1_u_point_b(Incf_Env27) :-
        place_op(Incf_Env27, incf, [value, u_val], [8], _GORES15).

f_u_fifteen(MResult) :-
        Env=[],
        catch(( TBEnv=[[bv(u_val, [])]|Env],
                symbol_setter(TBEnv, setq, u_val, 1),
                addr_tagbody_1_u_point_a(TBEnv),
                symbol_value(TBEnv, u_val, U_val_Get),
                U_val_Get=MResult
              ),
              block_exit(u_fifteen, MResult),
              true).

*/
/*
(is eq 15 (fifteen))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:3656 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,15,[fifteen]])
:- mf_u_is([u_is, eq, 15, [u_fifteen]], _15378, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(defun do-four () (DO ((temp-one 1 (1+ temp-one) )(temp-two 0 (1- temp-two) ) )((> (- temp-one temp-two) 5) temp-one)() ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:3678 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'do-four',[],['DO',[['temp-one',1,['1+','temp-one']],['temp-two',0,['1-','temp-two']]],[[>,[-,'temp-one','temp-two'],5],'temp-one'],[]]])
/*
:- side_effect(generate_function_or_macro_name($, kw_function, '1+', f_u_1_c43)).
*/
/*
:- side_effect(generate_function_or_macro_name($, kw_function, '1-', f_u_1c45)).
*/
/*
:- side_effect(generate_function_or_macro_name($, kw_function, '1+', f_u_1_c43)).
*/
/*
:- side_effect(generate_function_or_macro_name($, kw_function, '1-', f_u_1c45)).
*/
wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]).
wl:arglist_info(u_do_four, f_u_do_four, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_u_do_four).

/*

### Compiled:  `U::DO-FOUR` 
*/
f_u_do_four(FnResult) :-
	CDR=[],
	catch(( ( BlockExitEnv=[bv(u_temp_one, 1), bv(u_temp_two, 0)|CDR],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_2), get_var(BlockExitEnv, u_temp_one, Temp_one_Get25), get_var(BlockExitEnv, u_temp_two, Temp_two_Get26), 'f_-'(Temp_one_Get25, Temp_two_Get26, PredArg1Result28), (PredArg1Result28<5->get_var(BlockExitEnv, u_temp_one, RetResult29), throw(block_exit([], RetResult29)), _TBResult=ThrowResult30;get_var(BlockExitEnv, u_temp_one, Temp_one_Get33), f_u_1_c43(Temp_one_Get33, Temp_one), get_var(BlockExitEnv, u_temp_two, Temp_two_Get34), f_u_1c45(Temp_two_Get34, Temp_two), set_var(BlockExitEnv, u_temp_one, Temp_one), set_var(BlockExitEnv, u_temp_two, Temp_two), goto(do_label_2, BlockExitEnv), _TBResult=_GORES35)),
					  
					  [ addr(addr_tagbody_4_do_label_2,
						 do_label_2,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, u_temp_one, Get_var_Ret), get_var(BlockExitEnv, u_temp_two, Get_var_Ret45), 'f_-'(Get_var_Ret, Get_var_Ret45, _16456), (_16456<5->get_var(BlockExitEnv, u_temp_one, Temp_one_Get15), throw(block_exit([], Temp_one_Get15)), _16472=ThrowResult;get_var(BlockExitEnv, u_temp_one, Temp_one_Get17), f_u_1_c43(Temp_one_Get17, D1_c43_Ret), get_var(BlockExitEnv, u_temp_two, Temp_two_Get18), f_u_1c45(Temp_two_Get18, D1c45_Ret), set_var(BlockExitEnv, u_temp_one, D1_c43_Ret), set_var(BlockExitEnv, u_temp_two, D1c45_Ret), goto(do_label_2, BlockExitEnv), _16472=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(u_do_four, FnResult),
	      true).
:- set_opv(u_do_four, symbol_function, f_u_do_four),
   set_opv(f_u_do_four, type_of, compiled_function),
   DefunResult=u_do_four.
/*
:- side_effect(assert_lsp(u_do_four,
			  lambda_def(defun,
				     u_do_four,
				     f_u_do_four,
				     [],
				     
				     [ 
				       [ do,
					 
					 [ [u_temp_one, 1, ['1+', u_temp_one]],
					   [u_temp_two, 0, ['1-', u_temp_two]]
					 ],
					 
					 [ [>, [-, u_temp_one, u_temp_two], 5],
					   u_temp_one
					 ],
					 []
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(u_do_four,
			  arglist_info(u_do_four,
				       f_u_do_four,
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
:- side_effect(assert_lsp(u_do_four, init_args(x, f_u_do_four))).
*/
/*
(is = 4  (rtrace (do-four)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:3802 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,=,4,[rtrace,['do-four']]])
:- mf_u_is([u_is, =, 4, [sys_rtrace, [u_do_four]]], _15424, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq 'string_l (DEFUN string_l (x )(COND ((STRINGP x )x )((SYMBOLP x )(symbol-name x ))(T (ERROR "type error" )))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:3832 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,string_l],['DEFUN',string_l,[x],['COND',[['STRINGP',x],x],[['SYMBOLP',x],['symbol-name',x]],['T',['ERROR','$STRING'("type error")]]]]])
:- mf_u_is(
	   [ u_is,
	     eq,
	     [quote, u_string_l],
	     
	     [ defun,
	       u_string_l,
	       [sys_x],
	       
	       [ cond,
		 [[stringp, sys_x], sys_x],
		 [[symbolp, sys_x], [symbol_name, sys_x]],
		 [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]
	       ]
	     ]
	   ],
	   _15676,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq () (TAGBODY 1 (PRINT "hi" )))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:3951 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',1,['PRINT','$STRING'("hi")]]])
:- mf_u_is(
	   [ u_is,
	     eq,
	     [],
	     [tagbody, 1, [print, '$ARRAY'([*], claz_base_character, "hi")]]
	   ],
	   _29232,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq () (TAGBODY a (PRINT "hi" )))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:3989 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',a,['PRINT','$STRING'("hi")]]])
:- mf_u_is(
	   [ u_is,
	     eq,
	     [],
	     [tagbody, u_a, [print, '$ARRAY'([*], claz_base_character, "hi")]]
	   ],
	   _15416,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq () (LET ((val 1 ))NIL ))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4027 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]],[]]])
:- mf_u_is([u_is, eq, [], [let, [[u_val, 1]], []]], _27328, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eq () (LET ((val 1 )) ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4059 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]]]])
:- mf_u_is([u_is, eq, [], [let, [[u_val, 1]]]], _15400, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eql 1 (LET ((val 1 ))val ))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4089 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,1,['LET',[[val,1]],val]])
:- mf_u_is([u_is, eql, 1, [let, [[u_val, 1]], u_val]], _26962, Eval_Param),
   f_eval(Eval_Param, _Ignored).
/*
(is eql 'world (LET ((a 'b) )(LET ((a 'world) )
  (LET ((a 'hello) )(LET ((a a)(*package* (find-package :keyword) ) )(PRINT a) ) )(PRINT a) ) ))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4121 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,[quote,world],['LET',[[a,[quote,b]]],['LET',[[a,[quote,world]]],['LET',[[a,[quote,hello]]],['LET',[[a,a],['*package*',['find-package',':keyword']]],['PRINT',a]]],['PRINT',a]]]])
:- mf_u_is(
	   [ u_is,
	     eql,
	     [quote, u_world],
	     
	     [ let,
	       [[u_a, [quote, u_b]]],
	       
	       [ let,
		 [[u_a, [quote, u_world]]],
		 
		 [ let,
		   [[u_a, [quote, u_hello]]],
		   
		   [ let,
		     [[u_a, u_a], [xx_package_xx, [find_package, kw_keyword]]],
		     [print, u_a]
		   ]
		 ],
		 [print, u_a]
	       ]
	     ]
	   ],
	   _15874,
	   Eval_Param),
   f_eval(Eval_Param, _Ignored).


%; Total compilation time: 1.09 seconds


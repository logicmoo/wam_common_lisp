#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "sanity-test" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Mon Dec 25 15:13:14 2017

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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:919 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'mapcar-visualize',[func,l],[if,[null,l],[],[cons,[apply,func,[list,[first,l]]],[mapcar,func,[rest,l]]]]])
wl:lambda_def(defun, u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]).
wl:arglist_info(u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_mapcar_visualize).

/*

### Compiled:  `U::MAPCAR-VISUALIZE` 
*/
f_u_mapcar_visualize(Func, L, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(u_func, Func), bv(u_l, L)|Env],
	get_var(Env16, u_l, IFTEST),
	(   IFTEST==[]
	->  FnResult=[]
	;   get_var(Env16, u_func, Func_Get),
	    get_var(Env16, u_l, L_Get10),
	    cl_car(L_Get10, Car_Ret),
	    cl_apply(Func_Get, [Car_Ret], Apply_Ret),
	    get_var(Env16, u_func, Func_Get11),
	    get_var(Env16, u_l, L_Get12),
	    cl_cdr(L_Get12, Cdr_Ret),
	    cl_mapcar(Func_Get11, [Cdr_Ret], Mapcar_Ret),
	    FnResult=[Apply_Ret|Mapcar_Ret]
	).
:- set_opv(f_u_mapcar_visualize, classof, claz_function),
   set_opv(u_mapcar_visualize, compile_as, kw_function),
   set_opv(u_mapcar_visualize, function, f_u_mapcar_visualize),
   DefunResult=u_mapcar_visualize.
/*
:- side_effect(assert_lsp(u_mapcar_visualize,
			  wl:lambda_def(defun, u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_mapcar_visualize,
			  wl:arglist_info(u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mapcar_visualize,
			  wl:init_args(exact_only, f_u_mapcar_visualize))).
*/
/*
'(load "../prolog/wam_cl/wam-cl-init")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1031 **********************/
:-lisp_compile_to_prolog(pkg_user,[quote,[load,'$STRING'("../prolog/wam_cl/wam-cl-init")]])
/*
'(load "wam-cl-init")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1070 **********************/
:-lisp_compile_to_prolog(pkg_user,[quote,[load,'$STRING'("wam-cl-init")]])
/*
(in-package "CL-USER")



;; Test macro
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1093 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','$STRING'("CL-USER")])
:- cl_in_package('$ARRAY'([*], claz_base_character, "CL-USER"), _Ignored).
/*
; Test macro
*/
/*
(defmacro is (eqf expected actual)
  (let ((a (gensym "a")) (b (gensym "b")))
    `(let ((,a ,expected) (,b ,actual))
       (if (,eqf ,a ,b)
         (format t "OK: fmt90_x1 is fmt90_x2 to fmt90_x3"(defmacro is (eqf expected actual)\n  (let ((a (gensym \"a\")) (b (gensym \"b\")))\n    `(let ((,a ,expected) (,b ,actual))\n       (if (,eqf ,a ,b)\n         (format t \"OK: ~a is ~a to ~a~%\" ',expected ',eqf ',actual)\n         (progn\n           (format t \"FAILED: when matching ~a and ~a~%\" ,a ,b)\n\t   #+WAM-CL (prolog-inline \"trace\")\n\t   #+CLISP (BREAK)\n\t   #+CLISP (quit 1))\n         ))))\n\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1133 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,is,[eqf,expected,actual],[let,[[a,[gensym,'$STRING'("a")]],[b,[gensym,'$STRING'("b")]]],['#BQ',[let,[[['#COMMA',a],['#COMMA',expected]],[['#COMMA',b],['#COMMA',actual]]],[if,[['#COMMA',eqf],['#COMMA',a],['#COMMA',b]],[format,t,'$STRING'("OK: ~a is ~a to ~a~%"),[quote,['#COMMA',expected]],[quote,['#COMMA',eqf]],[quote,['#COMMA',actual]]],[progn,[format,t,'$STRING'("FAILED: when matching ~a and ~a~%"),['#COMMA',a],['#COMMA',b]],['prolog-inline','$STRING'("trace")]]]]]]])
wl:lambda_def(defmacro, u_is, f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]).
wl:arglist_info(u_is, f_u_is, [u_eqf, u_expected, u_actual], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_is).

/*

### Compiled:  `U::IS` 
*/
f_u_is(Eqf, Expected, Actual, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(u_eqf, Eqf), bv(u_expected, Expected), bv(u_actual, Actual)|Global_env_Ret],
	cl_gensym('$ARRAY'([*], claz_base_character, "a"), A_Init),
	cl_gensym('$ARRAY'([*], claz_base_character, "b"), B_Init),
	LEnv=[bv(u_a, A_Init), bv(u_b, B_Init)|Env],
	get_var(LEnv, u_a, A_Get16),
	get_var(LEnv, u_actual, Actual_Get20),
	get_var(LEnv, u_b, B_Get17),
	get_var(LEnv, u_eqf, Eqf_Get19),
	get_var(LEnv, u_expected, Expected_Get18),
	[let, [[A_Get16, Expected_Get18], [B_Get17, Actual_Get20]], [if, [Eqf_Get19, A_Get16, B_Get17], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, Expected_Get18], [quote, Eqf_Get19], [quote, Actual_Get20]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), A_Get16, B_Get17], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_is, classof, claz_macro),
   set_opv(u_is, compile_as, kw_operator),
   set_opv(u_is, function, f_u_is),
   DefMacroResult=u_is.
/*
:- side_effect(assert_lsp(u_is,
			  wl:lambda_def(defmacro, u_is, f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_is,
			  wl:arglist_info(u_is, f_u_is, [u_eqf, u_expected, u_actual], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_is, wl:init_args(exact_only, f_u_is))).
*/
/*
(write-line "Running smoke test!")

; (progn (prolog-inline "rtrace") (is eq 1 1))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1519 **********************/
:-lisp_compile_to_prolog(pkg_user,['write-line','$STRING'("Running smoke test!")])
:- cl_write_line('$ARRAY'([*], claz_base_character, "Running smoke test!"),
		 _Ignored).
/*
 (progn (prolog-inline "rtrace") (is eq 1 1))
*/
/*
(is eq 1 1)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1602 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,1])
:- f_u_is(eq, 1, 1, _Ignored).
/*
(is equal (list 1 'a 'b) (cons 1 '(a b)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1614 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,1,[quote,a],[quote,b]],[cons,1,[quote,[a,b]]]])
:- f_u_is(equal,
	  [list, 1, [quote, u_a], [quote, u_b]],
	  [cons, 1, [quote, [u_a, u_b]]],
	  _Ignored).
/*
(is eq 2 (if nil 1 2))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1657 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]])
:- f_u_is(eq, 2, [if, [], 1, 2], _Ignored).
/*
(is eq t (keywordp :k))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1681 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[keywordp,':k']])
:- f_u_is(eq, t, [keywordp, kw_k], _Ignored).
/*
(is eq 10 (if t 10 20))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1706 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,10,[if,t,10,20]])
:- f_u_is(eq, 10, [if, t, 10, 20], _Ignored).
/*
(is eq t (stringp "abc"))

;;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2"(is eq t (stringp \"abc\"))\n\n;;  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1731 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[stringp,'$STRING'("abc")]])
:- f_u_is(eq, t, [stringp, '$ARRAY'([*], claz_base_character, "abc")], _Ignored).
/*
;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).".
*/
/*
(is equal (subseq "abc" 1) "bc")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1855 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[subseq,'$STRING'("abc"),1],'$STRING'("bc")])
:- f_u_is(equal,
	  [subseq, '$ARRAY'([*], claz_base_character, "abc"), 1],
	  '$ARRAY'([*], claz_base_character, "bc"),
	  _Ignored).
/*
(is eq 1 (if t 1 2))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1889 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[if,t,1,2]])
:- f_u_is(eq, 1, [if, t, 1, 2], _Ignored).
/*
(is eq 2 (if nil 1 2))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1910 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]])
:- f_u_is(eq, 2, [if, [], 1, 2], _Ignored).
/*
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1934 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,fib,[n],[if,[>,n,1],[+,[fib,[-,n,1]],[fib,[-,n,2]]],1]])
wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).
wl:arglist_info(u_fib, f_u_fib, [n], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fib).

/*

### Compiled:  `U::FIB` 
*/
f_u_fib(N, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(n, N)|Env],
	get_var(Env15, n, N_Get),
	(   N_Get>1
	->  get_var(Env15, n, N_Get10),
	    -(N_Get10, 1, Fib_Param),
	    f_u_fib(Fib_Param, Fib_Ret),
	    get_var(Env15, n, N_Get11),
	    -(N_Get11, 2, Fib_Param18),
	    f_u_fib(Fib_Param18, Fib_Ret20),
	    +(Fib_Ret, Fib_Ret20, TrueResult),
	    FnResult=TrueResult
	;   FnResult=1
	).
:- set_opv(f_u_fib, classof, claz_function),
   set_opv(u_fib, compile_as, kw_function),
   set_opv(u_fib, function, f_u_fib),
   DefunResult=u_fib.
/*
:- side_effect(assert_lsp(u_fib,
			  wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]))).
*/
/*
:- side_effect(assert_lsp(u_fib,
			  wl:arglist_info(u_fib, f_u_fib, [n], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fib, wl:init_args(exact_only, f_u_fib))).
*/
/*
(disassemble #'fib)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2015 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fib)])
:- cl_disassemble(f_u_fib, _Ignored).
/*
(is eql 89 (fib 10))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2037 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,89,[fib,10]])
:- f_u_is(eql, 89, [u_fib, 10], _Ignored).
/*
(defun accum (r) (if (= 0 r) (list 0) (cons r (accum (- r 1)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2061 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,accum,[r],[if,[=,0,r],[list,0],[cons,r,[accum,[-,r,1]]]]])
wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).
wl:arglist_info(u_accum, f_u_accum, [u_r], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_accum).

/*

### Compiled:  `U::ACCUM` 
*/
f_u_accum(R, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(u_r, R)|Env],
	get_var(Env16, u_r, R_Get),
	(   0=:=R_Get
	->  FnResult=[0]
	;   get_var(Env16, u_r, R_Get10),
	    -(R_Get10, 1, Accum_Param),
	    f_u_accum(Accum_Param, Accum_Ret),
	    FnResult=[R_Get10|Accum_Ret]
	).
:- set_opv(f_u_accum, classof, claz_function),
   set_opv(u_accum, compile_as, kw_function),
   set_opv(u_accum, function, f_u_accum),
   DefunResult=u_accum.
/*
:- side_effect(assert_lsp(u_accum,
			  wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_accum,
			  wl:arglist_info(u_accum, f_u_accum, [u_r], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_accum, wl:init_args(exact_only, f_u_accum))).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2127 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(accum)])
:- cl_disassemble(f_u_accum, _Ignored).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2304 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,4,3,2,1,0],[accum,4]])
:- f_u_is(equal, [list, 4, 3, 2, 1, 0], [u_accum, 4], _Ignored).
/*
(defmacro defwrap (name) `(defun ,name () 1))
;;; :- ensure_loaded('sanity-test.lisp.trans.pl').
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2343 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,defwrap,[name],['#BQ',[defun,['#COMMA',name],[],1]]])
wl:lambda_def(defmacro, u_defwrap, f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]).
wl:arglist_info(u_defwrap, f_u_defwrap, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_defwrap).

/*

### Compiled:  `U::DEFWRAP` 
*/
f_u_defwrap(Name, FnResult) :-
	global_env(Global_env_Ret),
	Env=[bv(sys_name, Name)|Global_env_Ret],
	get_var(Env, sys_name, Name_Get),
	[defun, Name_Get, [], 1]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_defwrap, classof, claz_macro),
   set_opv(u_defwrap, compile_as, kw_operator),
   set_opv(u_defwrap, function, f_u_defwrap),
   DefMacroResult=u_defwrap.
/*
:- side_effect(assert_lsp(u_defwrap,
			  wl:lambda_def(defmacro, u_defwrap, f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]))).
*/
/*
:- side_effect(assert_lsp(u_defwrap,
			  wl:arglist_info(u_defwrap, f_u_defwrap, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_defwrap, wl:init_args(exact_only, f_u_defwrap))).
*/
/*
;; :- ensure_loaded('sanity-test.lisp.trans.pl').
*/
/*
(defwrap foo)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2440 **********************/
:-lisp_compile_to_prolog(pkg_user,[defwrap,foo])
:- f_u_defwrap(u_foo, _Ignored).
/*
(is eq 1 (foo))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2454 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[foo]])
:- f_u_is(eq, 1, [u_foo], _Ignored).
/*
(is equal (macroexpand-1 '(defwrap foo)) '(defun foo nil 1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2470 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,['macroexpand-1',[quote,[defwrap,foo]]],[quote,[defun,foo,[],1]]])
:- f_u_is(equal,
	  [macroexpand_1, [quote, [u_defwrap, u_foo]]],
	  [quote, [defun, u_foo, [], 1]],
	  _Ignored).
/*
(write-line "PASSED")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2532 **********************/
:-lisp_compile_to_prolog(pkg_user,['write-line','$STRING'("PASSED")])
:- cl_write_line('$ARRAY'([*], claz_base_character, "PASSED"), _Ignored).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2555 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,fifteen,[],[let,[val],[tagbody,[setq,val,1],[go,'point-a'],[incf,val,16],'point-c',[incf,val,4],[go,'point-b'],[incf,val,32],'point-a','point-u',[incf,val,2],[go,'point-c'],[incf,val,64],'point-b',[incf,val,8]],val]])
wl:lambda_def(defun, u_fifteen, f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]).
wl:arglist_info(u_fifteen, f_u_fifteen, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_fifteen).

/*

### Compiled:  `U::FIFTEEN` 
*/
f_u_fifteen(FnResult) :-
	nop(global_env(Env)),
	CDR=Env,
	catch(( ( AEnv=[bv(u_val, [])|CDR],
		  call_addr_block(AEnv,
				  (set_var(AEnv, setq, u_val, 1), goto(u_point_a, AEnv)),
				  
				  [ addr(addr_tagbody_1_u_point_c,
					 u_point_c,
					 '$used',
					 Incf_Env,
					 (set_place(Incf_Env, incf, [value, u_val], [4], Set_place_Ret), goto(u_point_b, Incf_Env))),
				    addr(addr_tagbody_1_u_point_a,
					 u_point_a,
					 '$used',
					 Incf_Env16,
					 (push_label(u_point_u), set_place(Incf_Env16, incf, [value, u_val], [2], Incf_R15), goto(u_point_c, Incf_Env16))),
				    addr(addr_tagbody_1_u_point_u,
					 u_point_u,
					 '$unused',
					 Incf_Env20,
					 (set_place(Incf_Env20, incf, [value, u_val], [2], Incf_R19), goto(u_point_c, Incf_Env20))),
				    addr(addr_tagbody_1_u_point_b,
					 u_point_b,
					 '$used',
					 Incf_Env24,
					 set_place(Incf_Env24,
						   incf,
						   [value, u_val],
						   [8],
						   _GORES17))
				  ]),
		  get_var(AEnv, u_val, Val_Get)
		),
		Val_Get=FnResult
	      ),
	      block_exit(u_fifteen, FnResult),
	      true).
:- set_opv(f_u_fifteen, classof, claz_function),
   set_opv(u_fifteen, compile_as, kw_function),
   set_opv(u_fifteen, function, f_u_fifteen),
   DefunResult=u_fifteen.
/*
:- side_effect(assert_lsp(u_fifteen,
			  wl:lambda_def(defun, u_fifteen, f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]))).
*/
/*
:- side_effect(assert_lsp(u_fifteen,
			  wl:arglist_info(u_fifteen, f_u_fifteen, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fifteen, wl:init_args(exact_only, f_u_fifteen))).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2869 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fifteen)])
:- cl_disassemble(f_u_fifteen, _Ignored).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4027 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,15,[fifteen]])
:- f_u_is(eq, 15, [u_fifteen], _Ignored).
/*
(defun do-four () (DO ((temp-one 1 (1+ temp-one) )(temp-two 0 (1- temp-two) ) )((> (- temp-one temp-two) 5) temp-one)() ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4049 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'do-four',[],['DO',[['temp-one',1,['1+','temp-one']],['temp-two',0,['1-','temp-two']]],[[>,[-,'temp-one','temp-two'],5],'temp-one'],[]]])
wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]).
wl:arglist_info(u_do_four, f_u_do_four, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_do_four).

/*

### Compiled:  `U::DO-FOUR` 
*/
f_u_do_four(FnResult) :-
	nop(global_env(Env)),
	CDR=Env,
	catch(( ( BlockExitEnv=[bv(u_temp_one, 1), bv(u_temp_two, 0)|CDR],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_1), get_var(BlockExitEnv, u_temp_one, Temp_one_Get27), get_var(BlockExitEnv, u_temp_two, Temp_two_Get28), -(Temp_one_Get27, Temp_two_Get28, PredArg1Result30), (PredArg1Result30>5->get_var(BlockExitEnv, u_temp_one, RetResult31), throw(block_exit([], RetResult31)), _TBResult=ThrowResult32;get_var(BlockExitEnv, u_temp_one, Temp_one_Get35), '1+'(Temp_one_Get35, Temp_one), '1-'(Temp_two_Get28, Temp_two), set_var(BlockExitEnv, u_temp_one, Temp_one), set_var(BlockExitEnv, u_temp_two, Temp_two), goto(do_label_1, BlockExitEnv), _TBResult=_GORES37)),
					  
					  [ addr(addr_tagbody_2_do_label_1,
						 do_label_1,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, u_temp_one, Get_var_Ret), get_var(BlockExitEnv, u_temp_two, Get_var_Ret47), -(Get_var_Ret, Get_var_Ret47, _8112530), (_8112530>5->get_var(BlockExitEnv, u_temp_one, Temp_one_Get17), throw(block_exit([], Temp_one_Get17)), _8112596=ThrowResult;get_var(BlockExitEnv, u_temp_one, Temp_one_Get19), '1+'(Temp_one_Get19, Set_var_Ret), get_var(BlockExitEnv, u_temp_two, Temp_two_Get20), '1-'(Temp_two_Get20, Set_var_Ret49), set_var(BlockExitEnv, u_temp_one, Set_var_Ret), set_var(BlockExitEnv, u_temp_two, Set_var_Ret49), goto(do_label_1, BlockExitEnv), _8112596=_GORES)))
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
:- set_opv(f_u_do_four, classof, claz_function),
   set_opv(u_do_four, compile_as, kw_function),
   set_opv(u_do_four, function, f_u_do_four),
   DefunResult=u_do_four.
/*
:- side_effect(assert_lsp(u_do_four,
			  wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]))).
*/
/*
:- side_effect(assert_lsp(u_do_four,
			  wl:arglist_info(u_do_four, f_u_do_four, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_do_four, wl:init_args(exact_only, f_u_do_four))).
*/
/*
(is = 4  (do-four))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4173 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,=,4,['do-four']])
:- f_u_is(=, 4, [u_do_four], _Ignored).
/*
(is eq 'string_l (DEFUN string_l (x )(COND ((STRINGP x )x )((SYMBOLP x )(symbol-name x ))(T (ERROR "type error" )))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4194 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,string_l],['DEFUN',string_l,[x],['COND',[['STRINGP',x],x],[['SYMBOLP',x],['symbol-name',x]],['T',['ERROR','$STRING'("type error")]]]]])
:- f_u_is(eq,
	  [quote, u_string_l],
	  
	  [ defun,
	    u_string_l,
	    [u_x],
	    
	    [ cond,
	      [[stringp, u_x], u_x],
	      [[symbolp, u_x], [symbol_name, u_x]],
	      [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]
	    ]
	  ],
	  _Ignored).
/*
(is eq () (TAGBODY 1 (PRINT "hi" )))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4313 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',1,['PRINT','$STRING'("hi")]]])
:- f_u_is(eq,
	  [],
	  [tagbody, 1, [print, '$ARRAY'([*], claz_base_character, "hi")]],
	  _Ignored).
/*
(is eq () (TAGBODY a (PRINT "hi" )))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4351 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',a,['PRINT','$STRING'("hi")]]])
:- f_u_is(eq,
	  [],
	  [tagbody, u_a, [print, '$ARRAY'([*], claz_base_character, "hi")]],
	  _Ignored).
/*
(is eq () (LET ((val 1 ))NIL ))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4389 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]],[]]])
:- f_u_is(eq, [], [let, [[u_val, 1]], []], _Ignored).
/*
(is eq () (LET ((val 1 )) ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4421 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]]]])
:- f_u_is(eq, [], [let, [[u_val, 1]]], _Ignored).
/*
(is eql 1 (LET ((val 1 ))val ))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4451 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,1,['LET',[[val,1]],val]])
:- f_u_is(eql, 1, [let, [[u_val, 1]], u_val], _Ignored).
/*
(is eql 'world (LET ((a 'b) )(LET ((a 'world) )
  (LET ((a 'hello) )(LET ((a a)(*package* (find-package :keyword) ) )(PRINT a) ) )(PRINT a) ) ))


;; 3.1. Review of defstruct

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4483 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,[quote,world],['LET',[[a,[quote,b]]],['LET',[[a,[quote,world]]],['LET',[[a,[quote,hello]]],['LET',[[a,a],['*package*',['find-package',':keyword']]],['PRINT',a]]],['PRINT',a]]]])
:- f_u_is(eql,
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
	  ],
	  _Ignored).
/*
; 3.1. Review of defstruct
*/
/*
(progn #+WAM-CL (prolog-inline "nop(trace)")(is eq 'point (defstruct point x y z)))
;; (defstruct point x y z)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4659 **********************/
:-lisp_compile_to_prolog(pkg_user,[progn,['prolog-inline','$STRING'("nop(trace)")],[is,eq,[quote,point],[defstruct,point,x,y,z]]])
:- nop(trace),
   f_u_is(eq, [quote, u_point], [defstruct, u_point, u_x, u_y, u_z], _Ignored).
/*
; (defstruct point x y z)
*/
/*
(is eq 'point4d (defstruct point4d x y z t))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4771 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point4d],[defstruct,point4d,x,y,z,t]])
:- f_u_is(eq, [quote, u_point4d], [defstruct, u_point4d, u_x, u_y, u_z, t], _Ignored).
/*
(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4817 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'distance-from-origin',[point],['let*',[[x,['point-x',point]],[y,['point-y',point]],[z,['point-z',point]]],[sqrt,[+,[*,x,x],[*,y,y],[*,z,z]]]]])
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, f_u_distance_from_origin, [u_point], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_distance_from_origin).

/*

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
*/
f_u_distance_from_origin(Point, FnResult) :-
	nop(global_env(Env)),
	Env23=[bv(u_point, Point)|Env],
	f_u_point_x(Point_Get10, X_Init),
	f_u_point_y(Point_Get10, Y_Init),
	f_u_point_z(Point_Get10, Z_Init),
	LEnv=[bv(u_x, X_Init), bv(u_y, Y_Init), bv(u_z, Z_Init)|Env23],
	get_var(LEnv, u_x, X_Get16),
	*(X_Get16, X_Get16, _10132340),
	get_var(LEnv, u_y, Y_Get18),
	*(Y_Get18, Y_Get18, _10135830),
	+(_10132340, _10135830, _10139648),
	get_var(LEnv, u_z, Z_Get20),
	*(Z_Get20, Z_Get20, _10139946),
	+(_10139648, _10139946, Sqrt_Param),
	cl_sqrt(Sqrt_Param, LetResult),
	LetResult=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:arglist_info(u_distance_from_origin, f_u_distance_from_origin, [u_point], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:init_args(exact_only, f_u_distance_from_origin))).
*/
/*
(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4983 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'reflect-in-y-axis',[point],[setf,['point-y',point],[-,['point-y',point]]]])
wl:lambda_def(defun, u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).
wl:arglist_info(u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_reflect_in_y_axis).

/*

### Compiled:  `U::REFLECT-IN-Y-AXIS` 
*/
f_u_reflect_in_y_axis(Point, FnResult) :-
	nop(global_env(Env)),
	Setf_Env=[bv(u_point, Point)|Env],
	get_var(Setf_Env, u_point, Point_Get),
	f_u_point_y(Point_Get, Point_y_Ret),
	-(0, Point_y_Ret, CAR),
	get_var(Setf_Env, u_point, Point_Get9),
	set_place(Setf_Env, setf, [u_point_y, Point_Get9], [CAR], Setf_R),
	Setf_R=FnResult.
:- set_opv(f_u_reflect_in_y_axis, classof, claz_function),
   set_opv(u_reflect_in_y_axis, compile_as, kw_function),
   set_opv(u_reflect_in_y_axis, function, f_u_reflect_in_y_axis),
   DefunResult=u_reflect_in_y_axis.
/*
:- side_effect(assert_lsp(u_reflect_in_y_axis,
			  wl:lambda_def(defun, u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]))).
*/
/*
:- side_effect(assert_lsp(u_reflect_in_y_axis,
			  wl:arglist_info(u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_reflect_in_y_axis,
			  wl:init_args(exact_only, f_u_reflect_in_y_axis))).
*/
/*
(list (setf my-point (make-point :x 3 :y 4 :z 12)) (setf my-point2 (make-point :x 3 :y 4 :z 12)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5071 **********************/
:-lisp_compile_to_prolog(pkg_user,[list,[setf,'my-point',['make-point',':x',3,':y',4,':z',12]],[setf,'my-point2',['make-point',':x',3,':y',4,':z',12]]])
:- f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, My_point),
   set_var(Set_var_Param, u_my_point, My_point),
   f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, My_point2),
   set_var(Set_var_Param, u_my_point2, My_point2),
   _Ignored=[My_point, My_point2].
/*
(setf my-point3 #S(POINT :X 3 :Y 4 :Z 12))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5169 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point3','$S'(['POINT',':X',3,':Y',4,':Z',12])])
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], _Ignored),
   set_var(Set_var_Param, u_my_point3, _Ignored).
/*
(setf my-point4d (make-point4d :x 3 :y 4 :z 12 :t 1))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5212 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point4d',['make-point4d',':x',3,':y',4,':z',12,':t',1]])
:- f_u_make_point4d(kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1, _Ignored),
   set_var(Set_var_Param, u_my_point4d, _Ignored).
/*
(is eq t (point-p my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5268 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,['point-p','my-point']])
:- f_u_is(eq, t, [u_point_p, u_my_point], _Ignored).
/*
(is eq 'point (type-of my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5298 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point],['type-of','my-point']])
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], _Ignored).
/*
#+IGNORE #+WAM-CL (prolog-call "break")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5333 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':IGNORE'],[#+,':WAM-CL',['prolog-call','$STRING'("break")]]]))
/*
(is eql 13 (progn (print (distance-from-origin my-point))))

;; #+CLISP (BREAK)
;; #+WAM-CL (prolog-call "break")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5374 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,13,[progn,[print,['distance-from-origin','my-point']]]])
:- f_u_is(eql, 13, [progn, [print, [u_distance_from_origin, u_my_point]]], _Ignored).
/*
; #+CLISP (BREAK)
*/
/*
; #+WAM-CL (prolog-call "break")
*/
/*
(is = -4 (reflect-in-y-axis my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5489 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,=,-4,['reflect-in-y-axis','my-point']])
:- f_u_is(=, -4, [u_reflect_in_y_axis, u_my_point], _Ignored).
/*
(is eq my-point my-point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5529 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,'my-point','my-point'])
:- f_u_is(eq, u_my_point, u_my_point, _Ignored).
/*
(setf a-similar-point #s(point :x 3 :y -4 :z 12))

; (is eq t (equal my-point a-similar-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5556 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'a-similar-point','$S'([point,':x',3,':y',-4,':z',12])])
:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], _Ignored),
   set_var(Set_var_Param, u_a_similar_point, _Ignored).
/*
 (is eq t (equal my-point a-similar-point))
*/
/*
(is eq nil (eq my-point a-similar-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5653 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],[eq,'my-point','a-similar-point']])
:- f_u_is(eq, [], [eq, u_my_point, u_a_similar_point], _Ignored).
/*
(equalp my-point a-similar-point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5696 **********************/
:-lisp_compile_to_prolog(pkg_user,[equalp,'my-point','a-similar-point'])
:- get_var(GEnv, u_a_similar_point, A_similar_point_Get),
   get_var(GEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, _Ignored).
/*
(is eq t (equalp my-point a-similar-point) )


;; 3.2. defclass

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5731 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[equalp,'my-point','a-similar-point']])
:- f_u_is(eq, t, [equalp, u_my_point, u_a_similar_point], _Ignored).
/*
; 3.2. defclass
*/
/*
(unintern 'point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5796 **********************/
:-lisp_compile_to_prolog(pkg_user,[unintern,[quote,point]])
:- cl_unintern(u_point, _Ignored).
/*
(defclass point ()
  (x
   y
   z))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5815 **********************/
:-lisp_compile_to_prolog(pkg_user,[defclass,point,[],[x,y,z]])
:- cl_defclass([u_point, [], [u_x, u_y, u_z]], _Ignored).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, symbolname, u_point))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, type, u_point))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, include, []))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_x, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_x, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 1, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_y, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_y, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 2, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_z, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_z, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 3, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, conc_name, "POINT-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, setter_fn, u_setf_point_x, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, accessor, u_point_x, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, setter_fn, u_setf_point_y, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, accessor, u_point_y, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, setter_fn, u_setf_point_z, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, accessor, u_point_z, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_x, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_x, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 1, zlot_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_y, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_y, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 2, zlot_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_z, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_z, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 3, zlot_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, prototype, u_point_prototypical))).
*/
/*
(setf my-point (make-instance 'point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5852 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point',['make-instance',[quote,point]]])
:- cl_make_instance([u_point], _Ignored),
   set_var(Set_var_Param, u_my_point, _Ignored).
/*
(is eq 'point (type-of my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5892 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point],['type-of','my-point']])
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], _Ignored).
/*
(defun set-point-values (point x y z)
  (setf (slot-value point 'x) x
        (slot-value point 'y) y
        (slot-value point 'z) z))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5927 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'set-point-values',[point,x,y,z],[setf,['slot-value',point,[quote,x]],x,['slot-value',point,[quote,y]],y,['slot-value',point,[quote,z]],z]])
wl:lambda_def(defun, u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]).
wl:arglist_info(u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_set_point_values).

/*

### Compiled:  `U::SET-POINT-VALUES` 
*/
f_u_set_point_values(Point, X, Y, Z, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_point, Point), bv(u_x, X), bv(u_y, Y), bv(u_z, Z)|Env],
	get_var(Env14, u_point, Point_Get),
	get_var(Env14, u_x, X_Get),
	set_opv(Point_Get, u_x, X_Get),
	get_var(Env14, u_point, Point_Get8),
	get_var(Env14, u_y, Y_Get),
	set_opv(Point_Get8, u_y, Y_Get),
	get_var(Env14, u_point, Point_Get10),
	get_var(Env14, u_z, Z_Get),
	set_opv(Point_Get10, u_z, Z_Get),
	Z_Get=FnResult.
:- set_opv(f_u_set_point_values, classof, claz_function),
   set_opv(u_set_point_values, compile_as, kw_function),
   set_opv(u_set_point_values, function, f_u_set_point_values),
   DefunResult=u_set_point_values.
/*
:- side_effect(assert_lsp(u_set_point_values,
			  wl:lambda_def(defun, u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]))).
*/
/*
:- side_effect(assert_lsp(u_set_point_values,
			  wl:arglist_info(u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_set_point_values,
			  wl:init_args(exact_only, f_u_set_point_values))).
*/
/*
(set-point-values my-point 3 4 12)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6064 **********************/
:-lisp_compile_to_prolog(pkg_user,['set-point-values','my-point',3,4,12])
:- get_var(GEnv, u_my_point, My_point_Get),
   f_u_set_point_values(My_point_Get, 3, 4, 12, _Ignored).
/*
(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6100 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'distance-from-origin',[point],['with-slots',[x,y,z],point,[sqrt,[+,[*,x,x],[*,y,y],[*,z,z]]]]])
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, f_u_distance_from_origin, [u_point], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_distance_from_origin).

/*

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
*/
f_u_distance_from_origin(Point, FnResult) :-
	nop(global_env(Env)),
	Env23=[bv(u_point, Point)|Env],
	get_opv(Point_Get10, u_x, X_Init),
	get_opv(Point_Get10, u_y, Y_Init),
	get_opv(Point_Get10, u_z, Z_Init),
	LEnv=[bv(u_x, X_Init), bv(u_y, Y_Init), bv(u_z, Z_Init)|Env23],
	get_var(LEnv, u_x, X_Get16),
	*(X_Get16, X_Get16, _15438540),
	get_var(LEnv, u_y, Y_Get18),
	*(Y_Get18, Y_Get18, _15442258),
	+(_15438540, _15442258, _15446304),
	get_var(LEnv, u_z, Z_Get20),
	*(Z_Get20, Z_Get20, _15446602),
	+(_15446304, _15446602, Sqrt_Param),
	cl_sqrt(Sqrt_Param, LetResult),
	LetResult=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:arglist_info(u_distance_from_origin, f_u_distance_from_origin, [u_point], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:init_args(exact_only, f_u_distance_from_origin))).
*/
/*
(DISASSEMBLE #'distance-from-origin)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6213 **********************/
:-lisp_compile_to_prolog(pkg_user,['DISASSEMBLE',function('distance-from-origin')])
:- cl_disassemble(f_u_distance_from_origin, _Ignored).
/*
(distance-from-origin my-point)

;; 3.3. classes are objects

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6252 **********************/
:-lisp_compile_to_prolog(pkg_user,['distance-from-origin','my-point'])
:- get_var(GEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, _Ignored).
/*
; 3.3. classes are objects
*/
/*
(find-class 'point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6314 **********************/
:-lisp_compile_to_prolog(pkg_user,['find-class',[quote,point]])
:- cl_find_class(u_point, _Ignored).
/*
(class-name (find-class 'point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6335 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-name',['find-class',[quote,point]]])
:- cl_find_class(u_point, Class_name_Param),
   cl_class_name(Class_name_Param, _Ignored).
/*
(class-of my-point)

;; #-(or cormanlisp CLISP WAM-CL)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6369 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-of','my-point'])
:- get_var(GEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, _Ignored).
/*
; #-(or cormanlisp CLISP WAM-CL)
*/
/*
(typep my-point (class-of my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6424 **********************/
:-lisp_compile_to_prolog(pkg_user,[typep,'my-point',['class-of','my-point']])
:- get_var(GEnv, u_my_point, My_point_Get5),
   cl_class_of(My_point_Get5, Class_of_Ret),
   cl_typep(My_point_Get5, Class_of_Ret, _Ignored).
/*
(is eq (find-class 'STANDARD-CLASS)
       (class-of (class-of my-point)))

;; 3.4. you don't need clos to use clos

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6462 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,['find-class',[quote,'STANDARD-CLASS']],['class-of',['class-of','my-point']]])
:- f_u_is(eq,
	  [find_class, [quote, standard_class]],
	  [class_of, [class_of, u_my_point]],
	  _Ignored).
/*
; 3.4. you don't need clos to use clos
*/
/*
(let ((the-symbol-class (find-class 'symbol)))
  (values the-symbol-class
          (class-name the-symbol-class)
          (eq the-symbol-class (class-of 'symbol))
          (class-of the-symbol-class)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6579 **********************/
:-lisp_compile_to_prolog(pkg_user,[let,[['the-symbol-class',['find-class',[quote,symbol]]]],[values,'the-symbol-class',['class-name','the-symbol-class'],[eq,'the-symbol-class',['class-of',[quote,symbol]]],['class-of','the-symbol-class']]])
:- cl_find_class(symbol, The_symbol_class_Init),
   LEnv=[bv(u_the_symbol_class, The_symbol_class_Init)|CDR],
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get),
   cl_class_name(The_symbol_class_Get, Class_name_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get9),
   cl_class_of(symbol, Class_of_Ret),
   cl_eq(The_symbol_class_Get9, Class_of_Ret, Eq_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get10),
   cl_class_of(The_symbol_class_Get10, Class_of_Ret15),
   nb_setval('$mv_return',
	     [u_the_symbol_class, Class_name_Ret, Eq_Ret, Class_of_Ret15]).
/*
(find-class t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6785 **********************/
:-lisp_compile_to_prolog(pkg_user,['find-class',t])
:- cl_find_class(t, _Ignored).
/*
(is eq 'foo (defstruct foo))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6801 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,foo],[defstruct,foo]])
:- f_u_is(eq, [quote, u_foo], [defstruct, u_foo], _Ignored).
/*
(is eq (find-class 'foo) (class-of (make-foo)))

;; 3.5 slots

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6831 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,['find-class',[quote,foo]],['class-of',['make-foo']]])
:- f_u_is(eq, [find_class, [quote, u_foo]], [class_of, [u_make_foo]], _Ignored).
/*
; 3.5 slots
*/
/*
(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader daft-z :allocation :class)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6894 **********************/
:-lisp_compile_to_prolog(pkg_user,[defclass,'daft-point',[],[[x,':accessor','daft-x',':initarg',':x'],[y,':accessor','daft-y',':initform',3.14159],[z,':reader','daft-z',':allocation',':class']]])
:- cl_defclass(
	       [ u_daft_point,
		 [],
		 
		 [ [u_x, kw_accessor, u_daft_x, kw_initarg, kw_x],
		   [u_y, kw_accessor, u_daft_y, kw_initform, 3.14159],
		   [u_z, kw_reader, u_daft_z, kw_allocation, kw_class]
		 ]
	       ],
	       _Ignored).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, symbolname, u_daft_point))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, type, u_daft_point))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, include, []))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_x, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_x, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 1, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, name, u_x, zlot_daft_point_x))).
*/
/*
:- side_effect(maybe_add_function(u_daft_x,
				  [obj],
				  ['slot-value', obj, [quote, u_x]],
				  u_daft_x)).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, accessor, u_daft_x, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, initarg, kw_x, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_y, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_y, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 2, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, name, u_y, zlot_daft_point_y))).
*/
/*
:- side_effect(maybe_add_function(u_daft_y,
				  [obj],
				  ['slot-value', obj, [quote, u_y]],
				  u_daft_y)).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, accessor, u_daft_y, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, initform, 3.14159, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_z, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_z, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 3, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, reader, u_daft_z, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, allocation, kw_class, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, conc_name, "DAFT-POINT-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, setter_fn, u_setf_daft_point_x, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, setter_fn, u_setf_daft_point_y, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, setter_fn, u_setf_daft_point_z, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, accessor, u_daft_point_z, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_x, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_x, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 1, zlot_daft_point_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_y, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_y, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 2, zlot_daft_point_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_z, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_z, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 3, zlot_daft_point_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, prototype, u_daft_point_prototypical))).
*/
/*
(setf (slot-value (make-instance 'daft-point) 'z) 42)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7040 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,['slot-value',['make-instance',[quote,'daft-point']],[quote,z]],42])
:- cl_make_instance([u_daft_point], Set_opv_Param),
   set_opv(Set_opv_Param, u_z, 42).
/*
(setf my-daft-point (make-instance 'daft-point :x 19))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7095 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-daft-point',['make-instance',[quote,'daft-point'],':x',19]])
:- cl_make_instance([u_daft_point, kw_x, 19], _Ignored),
   set_var(Set_var_Param, u_my_daft_point, _Ignored).
/*
#+PERFECT 
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (progn #+WAM-CL (prolog-trace) (daft-z my-daft-point)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7152 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':PERFECT'],[list,['daft-x','my-daft-point'],['daft-y','my-daft-point'],[progn,[#+,':WAM-CL',['prolog-trace']],['daft-z','my-daft-point']]]]))
/*
(let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7284 **********************/
:-lisp_compile_to_prolog(pkg_user,[let,[[temp,['make-instance',[quote,'daft-point']]]],[setf,['daft-y',temp],999,['slot-value',temp,[quote,z]],0]])
:- cl_make_instance([u_daft_point], Temp_Init),
   LEnv=[bv(u_temp, Temp_Init)|CDR],
   get_var(LEnv, u_temp, Temp_Get),
   set_place(LEnv, setf, [u_daft_y, Temp_Get], [999], Setf_R),
   get_var(LEnv, u_temp, Temp_Get11),
   set_opv(Temp_Get11, u_z, 0).
/*
#+PERFECT
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

;; 3.6 Subclasses and inheritance

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7386 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':PERFECT'],[list,['daft-x','my-daft-point'],['daft-y','my-daft-point'],['daft-z','my-daft-point']]]))
/*
; 3.6 Subclasses and inheritance
*/
/*
(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7520 **********************/
:-lisp_compile_to_prolog(pkg_user,[defclass,animal,[],[[legs,':reader','leg-count',':initarg',':legs'],['comes-from',':reader','comes-from',':initarg',':comes-from']]])
:- cl_defclass(
	       [ u_animal,
		 [],
		 
		 [ [u_legs, kw_reader, u_leg_count, kw_initarg, kw_legs],
		   
		   [ u_comes_from,
		     kw_reader,
		     u_comes_from,
		     kw_initarg,
		     kw_comes_from
		   ]
		 ]
	       ],
	       _Ignored).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, symbolname, u_animal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, type, u_animal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, include, []))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_legs, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_legs, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 1, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, reader, u_leg_count, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, initarg, kw_legs, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 2, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, reader, u_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, initarg, kw_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, conc_name, "ANIMAL-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, setter_fn, u_setf_animal_legs, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, accessor, u_animal_legs, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, setter_fn, u_setf_animal_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, accessor, u_animal_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_legs, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_legs, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 1, zlot_animal_legs))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_comes_from, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 2, zlot_animal_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, prototype, u_animal_prototypical))).
*/
/*
(defclass mammal (animal)
  ((diet :initform 'antelopes :initarg :diet)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7642 **********************/
:-lisp_compile_to_prolog(pkg_user,[defclass,mammal,[animal],[[diet,':initform',[quote,antelopes],':initarg',':diet']]])
:- cl_defclass(
	       [ u_mammal,
		 [u_animal],
		 [[u_diet, kw_initform, [quote, u_antelopes], kw_initarg, kw_diet]]
	       ],
	       _Ignored).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, symbolname, u_mammal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, type, u_mammal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, include, u_animal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, slot, u_diet, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, keyword, kw_diet, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, ordinal, 1, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, initform, [quote, u_antelopes], zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, initarg, kw_diet, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, conc_name, "MAMMAL-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, setter_fn, u_setf_mammal_diet, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, accessor, u_mammal_diet, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, slot, u_diet, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, keyword, kw_diet, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, ordinal, 1, zlot_mammal_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, prototype, u_mammal_prototypical))).
*/
/*
(defclass aardvark (mammal)
  ((cute-p :accessor cute-p :initform nil)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7717 **********************/
:-lisp_compile_to_prolog(pkg_user,[defclass,aardvark,[mammal],[['cute-p',':accessor','cute-p',':initform',[]]]])
:- cl_defclass(
	       [ u_aardvark,
		 [u_mammal],
		 [[u_cute_p, kw_accessor, u_cute_p, kw_initform, []]]
	       ],
	       _Ignored).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, symbolname, u_aardvark))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, type, u_aardvark))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, include, u_mammal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, slot, u_cute_p, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, keyword, kw_cute_p, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, ordinal, 1, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, name, u_cute_p, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(maybe_add_function(u_cute_p,
				  [obj],
				  ['slot-value', obj, [quote, u_cute_p]],
				  u_cute_p)).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, accessor, u_cute_p, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, initform, [], zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, conc_name, "AARDVARK-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, setter_fn, u_setf_aardvark_cute_p, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, slot, u_cute_p, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, keyword, kw_cute_p, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, ordinal, 1, zlot_aardvark_cute_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, prototype, u_aardvark_prototypical))).
*/
/*
(#-allegro class-direct-superclasses #+allegro aclmop:class-direct-superclasses
   (find-class 'aardvark))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7791 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-direct-superclasses',['find-class',[quote,aardvark]]])
:- cl_find_class(u_aardvark, Direct_superclasses_Param),
   f_clos_class_direct_superclasses(Direct_superclasses_Param, _Ignored).
/*
; ACL needs to instantiate a class before its precedence-list becomes visible
*/
/*
; #+allegro
*/
/*
(make-instance 'aardvark)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7991 **********************/
:-lisp_compile_to_prolog(pkg_user,['make-instance',[quote,aardvark]])
:- cl_make_instance([u_aardvark], _Ignored).
/*
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
   (find-class 'aardvark))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8018 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-precedence-list',['find-class',[quote,aardvark]]])
:- cl_find_class(u_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).
/*
(defclass figurine ()
  ((potter :accessor made-by :initarg :made-by)
   (comes-from :initarg :made-in)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8118 **********************/
:-lisp_compile_to_prolog(pkg_user,[defclass,figurine,[],[[potter,':accessor','made-by',':initarg',':made-by'],['comes-from',':initarg',':made-in']]])
:- cl_defclass(
	       [ u_figurine,
		 [],
		 
		 [ [u_potter, kw_accessor, u_made_by, kw_initarg, kw_made_by],
		   [u_comes_from, kw_initarg, kw_made_in]
		 ]
	       ],
	       _Ignored).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, symbolname, u_figurine))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, type, u_figurine))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, include, []))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_potter, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_potter, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 1, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, name, u_potter, zlot_figurine_potter))).
*/
/*
:- side_effect(maybe_add_function(u_made_by,
				  [obj],
				  ['slot-value', obj, [quote, u_potter]],
				  u_made_by)).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, accessor, u_made_by, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, initarg, kw_made_by, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_comes_from, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_comes_from, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 2, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, initarg, kw_made_in, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, conc_name, "FIGURINE-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, setter_fn, u_setf_figurine_potter, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, setter_fn, u_setf_figurine_comes_from, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, accessor, u_figurine_comes_from, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_potter, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_potter, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 1, zlot_figurine_potter))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_comes_from, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_comes_from, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 2, zlot_figurine_comes_from))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, prototype, u_figurine_prototypical))).
*/
/*
(defclass figurine-aardvark (aardvark figurine)
  ((name :reader aardvark-name :initarg :aardvark-name)
   (diet :initform nil)))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8225 **********************/
:-lisp_compile_to_prolog(pkg_user,[defclass,'figurine-aardvark',[aardvark,figurine],[[name,':reader','aardvark-name',':initarg',':aardvark-name'],[diet,':initform',[]]]])
:- cl_defclass(
	       [ u_figurine_aardvark,
		 [u_aardvark, u_figurine],
		 
		 [ 
		   [ sys_name,
		     kw_reader,
		     u_aardvark_name,
		     kw_initarg,
		     kw_aardvark_name
		   ],
		   [u_diet, kw_initform, []]
		 ]
	       ],
	       _Ignored).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, symbolname, u_figurine_aardvark))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, type, u_figurine_aardvark))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, include, [u_aardvark, u_figurine]))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, sys_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 1, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, reader, u_aardvark_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, initarg, kw_aardvark_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, u_diet, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_diet, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 2, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, initform, [], zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, conc_name, "FIGURINE-AARDVARK-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, setter_fn, u_setf_figurine_aardvark_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, accessor, u_figurine_aardvark_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, setter_fn, u_setf_figurine_aardvark_diet, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, accessor, u_figurine_aardvark_diet, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, sys_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_name, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 1, zlot_figurine_aardvark_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, u_diet, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_diet, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 2, zlot_figurine_aardvark_diet))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, prototype, u_figurine_aardvark_prototypical))).
*/
/*
; ACL needs to instantiate a class before its precedence-list becomes visible
*/
/*
; #+allegro 
*/
/*
(make-instance 'figurine-aardvark)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8449 **********************/
:-lisp_compile_to_prolog(pkg_user,['make-instance',[quote,'figurine-aardvark']])
:- cl_make_instance([u_figurine_aardvark], _Ignored).
/*
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
             (find-class 'figurine-aardvark))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8485 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-precedence-list',['find-class',[quote,'figurine-aardvark']]])
:- cl_find_class(u_figurine_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).
/*
(setf Eric (make-instance 'figurine-aardvark
                          :legs 4
                          :made-by "Jen"
                          :made-in "Brittany"
                          :aardvark-name "Eric"))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8604 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'Eric',['make-instance',[quote,'figurine-aardvark'],':legs',4,':made-by','$STRING'("Jen"),':made-in','$STRING'("Brittany"),':aardvark-name','$STRING'("Eric")]])
:- cl_make_instance(
		    [ u_figurine_aardvark,
		      kw_legs,
		      4,
		      kw_made_by,
		      '$ARRAY'([*], claz_base_character, "Jen"),
		      kw_made_in,
		      '$ARRAY'([*], claz_base_character, "Brittany"),
		      kw_aardvark_name,
		      '$ARRAY'([*], claz_base_character, "Eric")
		    ],
		    _Ignored),
   set_var(Set_var_Param, u_eric, _Ignored).
/*
#+HAS_SHIFTF
(shiftf (cute-p Eric) t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8821 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':HAS_SHIFTF'],[shiftf,['cute-p','Eric'],t]]))
/*
(slot-value Eric 'diet)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8860 **********************/
:-lisp_compile_to_prolog(pkg_user,['slot-value','Eric',[quote,diet]])
:- get_var(GEnv, u_eric, Eric_Get),
   get_opv(Eric_Get, u_diet, _Ignored).


%; Total compilation time: 11.183 seconds


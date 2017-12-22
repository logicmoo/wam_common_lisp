#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "sanity-test" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Fri Dec 22 04:05:57 2017

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
wl:arglist_info(u_mapcar_visualize, [u_func, u_l], [Func_Param, L_Param], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_mapcar_visualize).

/*

### Compiled:  `U::MAPCAR-VISUALIZE` 
*/
f_u_mapcar_visualize(Func_Param, L_Param, FnResult) :-
	Env=[bv(u_func, Func_Param), bv(u_l, L_Param)],
	(   L_Param==[]
	->  FnResult=[]
	;   get_var(Env, list, List_Get),
	    cl_car(L_Param, Car_Ret),
	    f_u_func(List_Get, Car_Ret, Func_Ret),
	    cl_cdr(L_Param, Cdr_Ret),
	    cl_mapcar(Func_Param, [Cdr_Ret], Mapcar_Ret),
	    FnResult=[Func_Ret|Mapcar_Ret]
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
			  wl:arglist_info(u_mapcar_visualize, [u_func, u_l], [Func_Param, L_Param], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_mapcar_visualize,
			  wl:init_args(exact_only, u_mapcar_visualize))).
*/
/*
(load "../prolog/wam_cl/wam-cl-init")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1031 **********************/
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("../prolog/wam_cl/wam-cl-init")])
:- cl_load('$ARRAY'([*], claz_base_character, "../prolog/wam_cl/wam-cl-init"),
	   [],
	   _Ignored).
/*
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp'),
				  lisp_compile_to_prolog_output(<stream>(0x1d443d0)))).
*/
/*
(in-package "CL-USER")



;; Test macro
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1070 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1110 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,is,[eqf,expected,actual],[let,[[a,[gensym,'$STRING'("a")]],[b,[gensym,'$STRING'("b")]]],['#BQ',[let,[[['#COMMA',a],['#COMMA',expected]],[['#COMMA',b],['#COMMA',actual]]],[if,[['#COMMA',eqf],['#COMMA',a],['#COMMA',b]],[format,t,'$STRING'("OK: ~a is ~a to ~a~%"),[quote,['#COMMA',expected]],[quote,['#COMMA',eqf]],[quote,['#COMMA',actual]]],[progn,[format,t,'$STRING'("FAILED: when matching ~a and ~a~%"),['#COMMA',a],['#COMMA',b]],['prolog-inline','$STRING'("trace")]]]]]]])
wl:lambda_def(defmacro, u_is, f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]).
wl:arglist_info(u_is, [u_eqf, u_expected, u_actual], [Eqf_Param, Expected_Param, Actual_Param], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_is).

/*

### Compiled:  `U::IS` 
*/
f_u_is(Eqf_Param, Expected_Param, Actual_Param, FnResult) :-
	TLEnv=[bv(u_eqf, Eqf_Param), bv(u_expected, Expected_Param), bv(u_actual, Actual_Param)],
	cl_gensym('$ARRAY'([*], claz_base_character, "a"), A_Init),
	cl_gensym('$ARRAY'([*], claz_base_character, "b"), B_Init),
	LEnv=[[bv(u_a, A_Init), bv(u_b, B_Init)]|TLEnv],
	get_var(LEnv, u_a, A_Get26),
	get_var(LEnv, u_b, B_Get27),
	[let, [[A_Get26, Expected_Param], [B_Get27, Actual_Param]], [if, [Eqf_Param, A_Get26, B_Get27], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, Expected_Param], [quote, Eqf_Param], [quote, Actual_Param]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), A_Get26, B_Get27], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]=MFResult,
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
			  wl:arglist_info(u_is, [u_eqf, u_expected, u_actual], [Eqf_Param, Expected_Param, Actual_Param], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_is, wl:init_args(exact_only, u_is))).
*/
/*
(write-line "Running smoke test!")

; (progn (prolog-inline "rtrace") (is eq 1 1))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1496 **********************/
:-lisp_compile_to_prolog(pkg_user,['write-line','$STRING'("Running smoke test!")])
:- cl_write_line('$ARRAY'([*], claz_base_character, "Running smoke test!"),
		 _Ignored).
/*
 (progn (prolog-inline "rtrace") (is eq 1 1))
*/
/*
(is eq 1 1)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1579 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,1])
/*
% macroexpand:-[u_is,eq,1,1].
*/
/*
% into:-[let,[[a11,1],[b11,1]],[if,[eq,a11,b11],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,1]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a11,b11],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-_14720=[[bv(a11,1),bv(b11,1)]|_13340],get_var(_14720,a11,_19474),get_var(_14720,b11,_19940),(is_eq(_19474,_19940)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,1],_20206),_13608=_20206;get_var(_14720,a11,_22844),get_var(_14720,b11,_24476),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_22844,_24476],_21324),trace,_13608=_21324).
*/
:- LEnv=[[bv(a11, 1), bv(b11, 1)]|TLEnv],
   get_var(LEnv, a11, A11_Get),
   get_var(LEnv, b11, B11_Get),
   (   is_eq(A11_Get, B11_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   1
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a11, A11_Get12),
       get_var(LEnv, b11, B11_Get13),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A11_Get12,
		   B11_Get13
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is equal (list 1 'a 'b) (cons 1 '(a b)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1591 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,1,[quote,a],[quote,b]],[cons,1,[quote,[a,b]]]])
/*
% macroexpand:-[u_is,equal,[list,1,[quote,u_a],[quote,u_b]],[cons,1,[quote,[u_a,u_b]]]].
*/
/*
% into:-[let,[[a21,[list,1,[quote,u_a],[quote,u_b]]],[b21,[cons,1,[quote,[u_a,u_b]]]]],[if,[equal,a21,b21],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[list,1,[quote,u_a],[quote,u_b]]],[quote,equal],[quote,[cons,1,[quote,[u_a,u_b]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a21,b21],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_list([1,u_a,u_b],_15874),_15912=[1,u_a,u_b],_15536=[[bv(a21,_15874),bv(b21,_15912)]|_13706],get_var(_15536,a21,_24620),get_var(_15536,b21,_25086),(is_equal(_24620,_25086)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[list,1,[quote,u_a],[quote,u_b]],equal,[cons,1,[quote,[u_a,u_b]]]],_25622),_14244=_25622;get_var(_15536,a21,_28530),get_var(_15536,b21,_30162),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_28530,_30162],_27010),trace,_14244=_27010).
*/
:- A21_Init=[1, u_a, u_b],
   B21_Init=[1, u_a, u_b],
   LEnv=[[bv(a21, A21_Init), bv(b21, B21_Init)]|TLEnv],
   get_var(LEnv, a21, A21_Get),
   get_var(LEnv, b21, B21_Get),
   (   is_equal(A21_Get, B21_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [list, 1, [quote, u_a], [quote, u_b]],
		   equal,
		   [cons, 1, [quote, [u_a, u_b]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a21, A21_Get14),
       get_var(LEnv, b21, B21_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A21_Get14,
		   B21_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq 2 (if nil 1 2))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1634 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]])
/*
% macroexpand:-[u_is,eq,2,[if,[],1,2]].
*/
/*
% into:-[let,[[a31,2],[b31,[if,[],1,2]]],[if,[eq,a31,b31],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,2],[quote,eq],[quote,[if,[],1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a31,b31],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-([]\==[]->_15442=1;_15442=2),_15092=[[bv(a31,2),bv(b31,_15442)]|_13592],get_var(_15092,a31,_22648),get_var(_15092,b31,_23114),(is_eq(_22648,_23114)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),2,eq,[if,[],1,2]],_23452),_13932=_23452;get_var(_15092,a31,_26162),get_var(_15092,b31,_27794),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26162,_27794],_24642),trace,_13932=_24642).
*/
:- (   []\==[]
   ->  B31_Init=1
   ;   B31_Init=2
   ),
   LEnv=[[bv(a31, 2), bv(b31, B31_Init)]|TLEnv],
   get_var(LEnv, a31, A31_Get),
   get_var(LEnv, b31, B31_Get),
   (   is_eq(A31_Get, B31_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   2,
		   eq,
		   [if, [], 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a31, A31_Get15),
       get_var(LEnv, b31, B31_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A31_Get15,
		   B31_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq t (keywordp :k))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1658 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[keywordp,':k']])
/*
% macroexpand:-[u_is,eq,t,[keywordp,kw_k]].
*/
/*
% into:-[let,[[a41,t],[b41,[keywordp,kw_k]]],[if,[eq,a41,b41],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[keywordp,kw_k]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a41,b41],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_keywordp(kw_k,_15434),_15084=[[bv(a41,t),bv(b41,_15434)]|_13644],get_var(_15084,a41,_22496),get_var(_15084,b41,_22962),(is_eq(_22496,_22962)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[keywordp,kw_k]],_23264),_13948=_23264;get_var(_15084,a41,_25938),get_var(_15084,b41,_27570),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_25938,_27570],_24418),trace,_13948=_24418).
*/
:- cl_keywordp(kw_k, B41_Init),
   LEnv=[[bv(a41, t), bv(b41, B41_Init)]|TLEnv],
   get_var(LEnv, a41, A41_Get),
   get_var(LEnv, b41, B41_Get),
   (   is_eq(A41_Get, B41_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [keywordp, kw_k]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a41, A41_Get13),
       get_var(LEnv, b41, B41_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A41_Get13,
		   B41_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq 10 (if t 10 20))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1683 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,10,[if,t,10,20]])
/*
% macroexpand:-[u_is,eq,10,[if,t,10,20]].
*/
/*
% into:-[let,[[a51,10],[b51,[if,t,10,20]]],[if,[eq,a51,b51],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,10],[quote,eq],[quote,[if,t,10,20]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a51,b51],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-(t\==[]->_15618=10;_15618=20),_15268=[[bv(a51,10),bv(b51,_15618)]|_13768],get_var(_15268,a51,_22824),get_var(_15268,b51,_23290),(is_eq(_22824,_23290)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),10,eq,[if,t,10,20]],_23628),_14108=_23628;get_var(_15268,a51,_26338),get_var(_15268,b51,_27970),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26338,_27970],_24818),trace,_14108=_24818).
*/
:- (   t\==[]
   ->  B51_Init=10
   ;   B51_Init=20
   ),
   LEnv=[[bv(a51, 10), bv(b51, B51_Init)]|TLEnv],
   get_var(LEnv, a51, A51_Get),
   get_var(LEnv, b51, B51_Get),
   (   is_eq(A51_Get, B51_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   10,
		   eq,
		   [if, t, 10, 20]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a51, A51_Get15),
       get_var(LEnv, b51, B51_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A51_Get15,
		   B51_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq t (stringp "abc"))

;;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2"(is eq t (stringp \"abc\"))\n\n;;  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1708 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[stringp,'$STRING'("abc")]])
/*
% macroexpand:-[u_is,eq,t,[stringp,'$ARRAY'([*],claz_base_character,"abc")]].
*/
/*
% into:-[let,[[a61,t],[b61,[stringp,'$ARRAY'([*],claz_base_character,"abc")]]],[if,[eq,a61,b61],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[stringp,'$ARRAY'([*],claz_base_character,"abc")]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a61,b61],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_stringp('$ARRAY'([*],claz_base_character,"abc"),_15738),_15388=[[bv(a61,t),bv(b61,_15738)]|_13904],get_var(_15388,a61,_22800),get_var(_15388,b61,_23266),(is_eq(_22800,_23266)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[stringp,'$ARRAY'([*],claz_base_character,"abc")]],_23568),_14252=_23568;get_var(_15388,a61,_26242),get_var(_15388,b61,_27874),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26242,_27874],_24722),trace,_14252=_24722).
*/
:- cl_stringp('$ARRAY'([*], claz_base_character, "abc"), B61_Init),
   LEnv=[[bv(a61, t), bv(b61, B61_Init)]|TLEnv],
   get_var(LEnv, a61, A61_Get),
   get_var(LEnv, b61, B61_Get),
   (   is_eq(A61_Get, B61_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [stringp, '$ARRAY'([*], claz_base_character, "abc")]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a61, A61_Get13),
       get_var(LEnv, b61, B61_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A61_Get13,
		   B61_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).".
*/
/*
(is equal (subseq "abc" 1) "bc")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1832 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[subseq,'$STRING'("abc"),1],'$STRING'("bc")])
/*
% macroexpand:-[u_is,equal,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1],'$ARRAY'([*],claz_base_character,"bc")].
*/
/*
% into:-[let,[[a71,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1]],[b71,'$ARRAY'([*],claz_base_character,"bc")]],[if,[equal,a71,b71],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1]],[quote,equal],[quote,'$ARRAY'([*],claz_base_character,"bc")]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a71,b71],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_subseq('$ARRAY'([*],claz_base_character,"abc"),1,_15920),_15582=[[bv(a71,_15920),bv(b71,'$ARRAY'([*],claz_base_character,"bc"))]|_14034],get_var(_15582,a71,_23292),get_var(_15582,b71,_23758),(is_equal(_23292,_23758)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[subseq,'$ARRAY'([*],claz_base_character,"abc"),1],equal,'$ARRAY'([*],claz_base_character,"bc")],_24078),_14434=_24078;get_var(_15582,a71,_26770),get_var(_15582,b71,_28402),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26770,_28402],_25250),trace,_14434=_25250).
*/
:- cl_subseq('$ARRAY'([*], claz_base_character, "abc"), 1, A71_Init),
   LEnv=[[bv(a71, A71_Init), bv(b71, '$ARRAY'([*], claz_base_character, "bc"))]|TLEnv],
   get_var(LEnv, a71, A71_Get),
   get_var(LEnv, b71, B71_Get),
   (   is_equal(A71_Get, B71_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [subseq, '$ARRAY'([*], claz_base_character, "abc"), 1],
		   equal,
		   '$ARRAY'([*], claz_base_character, "bc")
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a71, A71_Get13),
       get_var(LEnv, b71, B71_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A71_Get13,
		   B71_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq 1 (if t 1 2))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1866 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[if,t,1,2]])
/*
% macroexpand:-[u_is,eq,1,[if,t,1,2]].
*/
/*
% into:-[let,[[a81,1],[b81,[if,t,1,2]]],[if,[eq,a81,b81],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,[if,t,1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a81,b81],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-(t\==[]->_15880=1;_15880=2),_15530=[[bv(a81,1),bv(b81,_15880)]|_14030],get_var(_15530,a81,_23086),get_var(_15530,b81,_23552),(is_eq(_23086,_23552)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,[if,t,1,2]],_23890),_14370=_23890;get_var(_15530,a81,_26600),get_var(_15530,b81,_28232),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26600,_28232],_25080),trace,_14370=_25080).
*/
:- (   t\==[]
   ->  B81_Init=1
   ;   B81_Init=2
   ),
   LEnv=[[bv(a81, 1), bv(b81, B81_Init)]|TLEnv],
   get_var(LEnv, a81, A81_Get),
   get_var(LEnv, b81, B81_Get),
   (   is_eq(A81_Get, B81_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   [if, t, 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a81, A81_Get15),
       get_var(LEnv, b81, B81_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A81_Get15,
		   B81_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq 2 (if nil 1 2))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1887 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]])
/*
% macroexpand:-[u_is,eq,2,[if,[],1,2]].
*/
/*
% into:-[let,[[a91,2],[b91,[if,[],1,2]]],[if,[eq,a91,b91],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,2],[quote,eq],[quote,[if,[],1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a91,b91],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-([]\==[]->_15970=1;_15970=2),_15620=[[bv(a91,2),bv(b91,_15970)]|_14120],get_var(_15620,a91,_23176),get_var(_15620,b91,_23642),(is_eq(_23176,_23642)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),2,eq,[if,[],1,2]],_23980),_14460=_23980;get_var(_15620,a91,_26690),get_var(_15620,b91,_28322),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26690,_28322],_25170),trace,_14460=_25170).
*/
:- (   []\==[]
   ->  B91_Init=1
   ;   B91_Init=2
   ),
   LEnv=[[bv(a91, 2), bv(b91, B91_Init)]|TLEnv],
   get_var(LEnv, a91, A91_Get),
   get_var(LEnv, b91, B91_Get),
   (   is_eq(A91_Get, B91_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   2,
		   eq,
		   [if, [], 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a91, A91_Get15),
       get_var(LEnv, b91, B91_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A91_Get15,
		   B91_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1911 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,fib,[n],[if,[>,n,1],[+,[fib,[-,n,1]],[fib,[-,n,2]]],1]])
wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).
wl:arglist_info(u_fib, [n], [N_Param], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_fib).

/*

### Compiled:  `U::FIB` 
*/
f_u_fib(N_Param, FnResult) :-
	(   N_Param>1
	->  -(N_Param, 1, Fib_Param),
	    f_u_fib(Fib_Param, Fib_Ret),
	    -(N_Param, 2, Fib_Param22),
	    f_u_fib(Fib_Param22, Fib_Ret24),
	    +(Fib_Ret, Fib_Ret24, TrueResult),
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
			  wl:arglist_info(u_fib, [n], [N_Param], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fib, wl:init_args(exact_only, u_fib))).
*/
/*
(disassemble #'fib)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1992 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fib)])
:- cl_disassemble(function(u_fib), _Ignored).
/*
(is eql 89 (fib 10))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2014 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,89,[fib,10]])
/*
% macroexpand:-[u_is,eql,89,[u_fib,10]].
*/
/*
% into:-[let,[[a101,89],[b101,[u_fib,10]]],[if,[eql,a101,b101],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,89],[quote,eql],[quote,[u_fib,10]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a101,b101],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-f_u_fib(10,_16044),_15694=[[bv(a101,89),bv(b101,_16044)]|_14254],get_var(_15694,a101,_23232),get_var(_15694,b101,_23698),(is_eql(_23232,_23698)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),89,eql,[u_fib,10]],_24000),_14558=_24000;get_var(_15694,a101,_26716),get_var(_15694,b101,_28390),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26716,_28390],_25154),trace,_14558=_25154).
*/
:- f_u_fib(10, B101_Init),
   LEnv=[[bv(a101, 89), bv(b101, B101_Init)]|TLEnv],
   get_var(LEnv, a101, A101_Get),
   get_var(LEnv, b101, B101_Get),
   (   is_eql(A101_Get, B101_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   89,
		   eql,
		   [u_fib, 10]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a101, A101_Get13),
       get_var(LEnv, b101, B101_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A101_Get13,
		   B101_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(defun accum (r) (if (= 0 r) (list 0) (cons r (accum (- r 1)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2038 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,accum,[r],[if,[=,0,r],[list,0],[cons,r,[accum,[-,r,1]]]]])
wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).
wl:arglist_info(u_accum, [u_r], [R_Param], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_accum).

/*

### Compiled:  `U::ACCUM` 
*/
f_u_accum(R_Param, FnResult) :-
	(   0=:=R_Param
	->  FnResult=[0]
	;   -(R_Param, 1, Accum_Param),
	    f_u_accum(Accum_Param, Accum_Ret),
	    FnResult=[R_Param|Accum_Ret]
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
			  wl:arglist_info(u_accum, [u_r], [R_Param], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_accum, wl:init_args(exact_only, u_accum))).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2104 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(accum)])
:- cl_disassemble(function(u_accum), _Ignored).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2281 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,4,3,2,1,0],[accum,4]])
/*
% macroexpand:-[u_is,equal,[list,4,3,2,1,0],[u_accum,4]].
*/
/*
% into:-[let,[[a12,[list,4,3,2,1,0]],[b12,[u_accum,4]]],[if,[equal,a12,b12],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[list,4,3,2,1,0]],[quote,equal],[quote,[u_accum,4]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a12,b12],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_list([4,3,2,1,0],_16526),f_u_accum(4,_16576),_16188=[[bv(a12,_16526),bv(b12,_16576)]|_14580],get_var(_16188,a12,_25014),get_var(_16188,b12,_25480),(is_equal(_25014,_25480)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[list,4,3,2,1,0],equal,[u_accum,4]],_25890),_14992=_25890;get_var(_16188,a12,_28672),get_var(_16188,b12,_30304),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_28672,_30304],_27152),trace,_14992=_27152).
*/
:- A12_Init=[4, 3, 2, 1, 0],
   f_u_accum(4, B12_Init),
   LEnv=[[bv(a12, A12_Init), bv(b12, B12_Init)]|TLEnv],
   get_var(LEnv, a12, A12_Get),
   get_var(LEnv, b12, B12_Get),
   (   is_equal(A12_Get, B12_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [list, 4, 3, 2, 1, 0],
		   equal,
		   [u_accum, 4]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a12, A12_Get14),
       get_var(LEnv, b12, B12_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A12_Get14,
		   B12_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(defmacro defwrap (name) `(defun ,name () 1))
;;; :- ensure_loaded('sanity-test.lisp.trans.pl').
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2320 **********************/
:-lisp_compile_to_prolog(pkg_user,[defmacro,defwrap,[name],['#BQ',[defun,['#COMMA',name],[],1]]])
wl:lambda_def(defmacro, u_defwrap, f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]).
wl:arglist_info(u_defwrap, [sys_name], [Name_Param], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_defwrap).

/*

### Compiled:  `U::DEFWRAP` 
*/
f_u_defwrap(Name_Param, FnResult) :-
	[defun, Name_Param, [], 1]=MFResult,
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
			  wl:arglist_info(u_defwrap, [sys_name], [Name_Param], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_defwrap, wl:init_args(exact_only, u_defwrap))).
*/
/*
;; :- ensure_loaded('sanity-test.lisp.trans.pl').
*/
/*
(defwrap foo)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2417 **********************/
:-lisp_compile_to_prolog(pkg_user,[defwrap,foo])
/*
% macroexpand:-[u_defwrap,u_foo].
*/
/*
% into:-[defun,u_foo,[],1].
*/
/*
% code:-assert_lsp(u_foo,wl:lambda_def(defun,u_foo,f_u_foo,[],[1])),assert_lsp(u_foo,wl:arglist_info(u_foo,[],[],arginfo{all:0,allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[],opt:0,req:0,rest:0,sublists:0,whole:0})),!,assert_lsp(u_foo,wl:init_args(exact_only,u_foo)),assert_lsp(u_foo,(f_u_foo(_50856):-_46002=[],1=_50856)),set_opv(f_u_foo,classof,claz_function),set_opv(u_foo,compile_as,kw_function),set_opv(u_foo,function,f_u_foo),_17750=u_foo.
*/
wl:lambda_def(defun, u_foo, f_u_foo, [], [1]).
wl:arglist_info(u_foo, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_foo).

/*

### Compiled:  `U::FOO` 
*/
f_u_foo(FnResult) :-
	Env=[],
	1=FnResult.
:- set_opv(f_u_foo, classof, claz_function),
   set_opv(u_foo, compile_as, kw_function),
   set_opv(u_foo, function, f_u_foo),
   DefunResult=u_foo.
/*
:- side_effect(assert_lsp(u_foo, wl:lambda_def(defun, u_foo, f_u_foo, [], [1]))).
*/
/*
:- side_effect(assert_lsp(u_foo,
			  wl:arglist_info(u_foo, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_foo, wl:init_args(exact_only, u_foo))).
*/
/*
(is eq 1 (foo))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2431 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[foo]])
/*
% macroexpand:-[u_is,eq,1,[u_foo]].
*/
/*
% into:-[let,[[a13,1],[b13,[u_foo]]],[if,[eq,a13,b13],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,[u_foo]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a13,b13],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-f_u_foo(_16438),_16088=[[bv(a13,1),bv(b13,_16438)]|_14690],get_var(_16088,a13,_23462),get_var(_16088,b13,_23928),(is_eq(_23462,_23928)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,[u_foo]],_24212),_14976=_24212;get_var(_16088,a13,_26868),get_var(_16088,b13,_28500),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26868,_28500],_25348),trace,_14976=_25348).
*/
:- f_u_foo(B13_Init),
   LEnv=[[bv(a13, 1), bv(b13, B13_Init)]|TLEnv],
   get_var(LEnv, a13, A13_Get),
   get_var(LEnv, b13, B13_Get),
   (   is_eq(A13_Get, B13_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   [u_foo]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a13, A13_Get13),
       get_var(LEnv, b13, B13_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A13_Get13,
		   B13_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is equal (macroexpand-1 '(defwrap foo)) '(defun foo nil 1))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2447 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,equal,['macroexpand-1',[quote,[defwrap,foo]]],[quote,[defun,foo,[],1]]])
/*
% macroexpand:-[u_is,equal,[macroexpand_1,[quote,[u_defwrap,u_foo]]],[quote,[defun,u_foo,[],1]]].
*/
/*
% into:-[let,[[a14,[macroexpand_1,[quote,[u_defwrap,u_foo]]]],[b14,[quote,[defun,u_foo,[],1]]]],[if,[equal,a14,b14],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[macroexpand_1,[quote,[u_defwrap,u_foo]]]],[quote,equal],[quote,[quote,[defun,u_foo,[],1]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a14,b14],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_macroexpand_1([[u_defwrap,u_foo]],_17040),_16702=[[bv(a14,_17040),bv(b14,[defun,u_foo,[],1])]|_14974],get_var(_16702,a14,_24776),get_var(_16702,b14,_25242),(is_equal(_24776,_25242)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[macroexpand_1,[quote,[u_defwrap,u_foo]]],equal,[quote,[defun,u_foo,[],1]]],_25724),_15458=_25724;get_var(_16702,a14,_28578),get_var(_16702,b14,_30210),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_28578,_30210],_27058),trace,_15458=_27058).
*/
:- cl_macroexpand_1([[u_defwrap, u_foo]], A14_Init),
   LEnv=[[bv(a14, A14_Init), bv(b14, [defun, u_foo, [], 1])]|TLEnv],
   get_var(LEnv, a14, A14_Get),
   get_var(LEnv, b14, B14_Get),
   (   is_equal(A14_Get, B14_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [macroexpand_1, [quote, [u_defwrap, u_foo]]],
		   equal,
		   [quote, [defun, u_foo, [], 1]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a14, A14_Get13),
       get_var(LEnv, b14, B14_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A14_Get13,
		   B14_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(write-line "PASSED")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2509 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2532 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,fifteen,[],[let,[val],[tagbody,[setq,val,1],[go,'point-a'],[incf,val,16],'point-c',[incf,val,4],[go,'point-b'],[incf,val,32],'point-a','point-u',[incf,val,2],[go,'point-c'],[incf,val,64],'point-b',[incf,val,8]],val]])
/*
% macroexpand:-[incf,u_val,4].
*/
/*
% into:-[setf,u_val,[+,u_val,4]].
*/
/*
% code:-get_var(_23320,u_val,_20284),+(_20284,4,_20136),set_place(_23320,setf,[value,u_val],[_20136],_19972).
*/
/*
% macroexpand:-[incf,u_val,2].
*/
/*
% into:-[setf,u_val,[+,u_val,2]].
*/
/*
% code:-get_var(_22044,u_val,_21430),+(_21430,2,_37040),set_place(_22044,setf,[value,u_val],[_37040],_23044).
*/
/*
% macroexpand:-[incf,u_val,2].
*/
/*
% into:-[setf,u_val,[+,u_val,2]].
*/
/*
% code:-get_var(_22198,u_val,_21578),+(_21578,2,_37194),set_place(_22198,setf,[value,u_val],[_37194],_23198).
*/
/*
% macroexpand:-[incf,u_val,8].
*/
/*
% into:-[setf,u_val,[+,u_val,8]].
*/
/*
% code:-get_var(_22458,u_val,_21832),+(_21832,8,_37454),set_place(_22458,setf,[value,u_val],[_37454],_23458).
*/
wl:lambda_def(defun, u_fifteen, f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]).
wl:arglist_info(u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_fifteen).

/*

### Compiled:  `U::FIFTEEN` 
*/
f_u_fifteen(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_val, [])]|Env],
		  call_addr_block(GoEnv,
				  (set_var(GoEnv, setq, u_val, 1), goto(u_point_a, GoEnv)),
				  
				  [ addr(addr_tagbody_2_u_point_c,
					 u_point_c,
					 '$used',
					 Setf_Env,
					 (get_var(Setf_Env, u_val, Get_var_Ret), +(Get_var_Ret, 4, CAR49), set_place(Setf_Env, setf, [value, u_val], [CAR49], Set_place_Ret), goto(u_point_b, Setf_Env))),
				    addr(addr_tagbody_2_u_point_a,
					 u_point_a,
					 '$used',
					 Setf_Env,
					 (push_label(u_point_u), get_var(Setf_Env, u_val, Val_Get21), +(Val_Get21, 2, CAR25), set_place(Setf_Env, setf, [value, u_val], [CAR25], Setf_R23), goto(u_point_c, Setf_Env))),
				    addr(addr_tagbody_2_u_point_u,
					 u_point_u,
					 '$unused',
					 Setf_Env,
					 (get_var(Setf_Env, u_val, Val_Get28), +(Val_Get28, 2, CAR32), set_place(Setf_Env, setf, [value, u_val], [CAR32], Setf_R30), goto(u_point_c, Setf_Env))),
				    addr(addr_tagbody_2_u_point_b,
					 u_point_b,
					 '$used',
					 _GEnv36,
					 (get_var(_GEnv36, u_val, Val_Get35), +(Val_Get35, 8, CAR39), set_place(_GEnv36, setf, [value, u_val], [CAR39], _GORES26)))
				  ]),
		  get_var(GoEnv, u_val, Val_Get43)
		),
		Val_Get43=FnResult
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
			  wl:arglist_info(u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_fifteen, wl:init_args(exact_only, u_fifteen))).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2846 **********************/
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fifteen)])
:- cl_disassemble(function(u_fifteen), _Ignored).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4004 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,15,[fifteen]])
/*
% macroexpand:-[u_is,eq,15,[u_fifteen]].
*/
/*
% into:-[let,[[a15,15],[b15,[u_fifteen]]],[if,[eq,a15,b15],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,15],[quote,eq],[quote,[u_fifteen]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a15,b15],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-f_u_fifteen(_16902),_16552=[[bv(a15,15),bv(b15,_16902)]|_15154],get_var(_16552,a15,_23926),get_var(_16552,b15,_24392),(is_eq(_23926,_24392)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),15,eq,[u_fifteen]],_24676),_15440=_24676;get_var(_16552,a15,_27332),get_var(_16552,b15,_28964),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_27332,_28964],_25812),trace,_15440=_25812).
*/
:- f_u_fifteen(B15_Init),
   LEnv=[[bv(a15, 15), bv(b15, B15_Init)]|TLEnv],
   get_var(LEnv, a15, A15_Get),
   get_var(LEnv, b15, B15_Get),
   (   is_eq(A15_Get, B15_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   15,
		   eq,
		   [u_fifteen]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a15, A15_Get13),
       get_var(LEnv, b15, B15_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A15_Get13,
		   B15_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(defun do-four () (DO ((temp-one 1 (1+ temp-one) )(temp-two 0 (1- temp-two) ) )((> (- temp-one temp-two) 5) temp-one)() ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4026 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'do-four',[],['DO',[['temp-one',1,['1+','temp-one']],['temp-two',0,['1-','temp-two']]],[[>,[-,'temp-one','temp-two'],5],'temp-one'],[]]])
wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]).
wl:arglist_info(u_do_four, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_do_four).

/*

### Compiled:  `U::DO-FOUR` 
*/
f_u_do_four(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_temp_one, 1), bv(u_temp_two, 0)]|Env],
		  catch(( call_addr_block(GoEnv,
					  (push_label(do_label_2), get_var(GoEnv, u_temp_one, Temp_one_Get31), get_var(GoEnv, u_temp_two, Temp_two_Get32), -(Temp_one_Get31, Temp_two_Get32, PredArg1Result34), (PredArg1Result34>5->throw(block_exit([], Temp_one_Get21)), _TBResult=ThrowResult36;get_var(GoEnv, u_temp_one, Temp_one_Get39), '1+'(Temp_one_Get39, Temp_one), get_var(GoEnv, u_temp_two, Temp_two_Get40), '1-'(Temp_two_Get40, Temp_two), set_var(GoEnv, u_temp_one, Temp_one), set_var(GoEnv, u_temp_two, Temp_two), goto(do_label_2, GoEnv), _TBResult=_GORES41)),
					  
					  [ addr(addr_tagbody_3_do_label_2,
						 do_label_2,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, u_temp_one, Temp_one_Get21), get_var(BlockExitEnv, u_temp_two, Temp_two_Get24), -(Temp_one_Get21, Temp_two_Get24, _16712), (_16712>5->throw(block_exit([], Temp_one_Get21)), _16714=ThrowResult;'1+'(Temp_one_Get21, Set_var_Ret), '1-'(Temp_two_Get24, Set_var_Ret51), set_var(BlockExitEnv, u_temp_one, Set_var_Ret), set_var(BlockExitEnv, u_temp_two, Set_var_Ret51), goto(do_label_2, BlockExitEnv), _16714=_GORES)))
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
			  wl:arglist_info(u_do_four, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_do_four, wl:init_args(exact_only, u_do_four))).
*/
/*
(is = 4  (do-four))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4150 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,=,4,['do-four']])
/*
% macroexpand:-[u_is,=,4,[u_do_four]].
*/
/*
% into:-[let,[[a16,4],[b16,[u_do_four]]],[if,[=,a16,b16],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,4],[quote,=],[quote,[u_do_four]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a16,b16],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-f_u_do_four(_17154),_16804=[[bv(a16,4),bv(b16,_17154)]|_15406],get_var(_16804,a16,_24012),get_var(_16804,b16,_24478),(_24012=:=_24478->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),4,=,[u_do_four]],_24762),_15692=_24762;get_var(_16804,a16,_27418),get_var(_16804,b16,_29050),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_27418,_29050],_25898),trace,_15692=_25898).
*/
:- f_u_do_four(B16_Init),
   LEnv=[[bv(a16, 4), bv(b16, B16_Init)]|TLEnv],
   get_var(LEnv, a16, A16_Get),
   get_var(LEnv, b16, B16_Get),
   (   A16_Get=:=B16_Get
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   4,
		   (=),
		   [u_do_four]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a16, A16_Get13),
       get_var(LEnv, b16, B16_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A16_Get13,
		   B16_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq 'string_l (DEFUN string_l (x )(COND ((STRINGP x )x )((SYMBOLP x )(symbol-name x ))(T (ERROR "type error" )))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4171 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,string_l],['DEFUN',string_l,[x],['COND',[['STRINGP',x],x],[['SYMBOLP',x],['symbol-name',x]],['T',['ERROR','$STRING'("type error")]]]]])
/*
% macroexpand:-[u_is,eq,[quote,u_string_l],[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]].
*/
/*
% into:-[let,[[a17,[quote,u_string_l]],[b17,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]]],[if,[eq,a17,b17],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_string_l]],[quote,eq],[quote,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a17,b17],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-assert_lsp(u_string_l,wl:lambda_def(defun,u_string_l,f_u_string_l,[u_x],[[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]])),assert_lsp(u_string_l,wl:arglist_info(u_string_l,[u_x],[_17364],arginfo{all:[u_x],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[u_x],opt:0,req:[u_x],rest:0,sublists:0,whole:0})),!,assert_lsp(u_string_l,wl:init_args(exact_only,u_string_l)),assert_lsp(u_string_l,(f_u_string_l(_17364,_17684):-is_stringp(_17364)->_17684=_17364;is_symbolp(_17364)->cl_symbol_name(_17364,_17574),_17684=_17574;cl_error(['$ARRAY'([*],claz_base_character,"type error")],_17630),_17684=_17630)),set_opv(f_u_string_l,classof,claz_function),set_opv(u_string_l,compile_as,kw_function),set_opv(u_string_l,function,f_u_string_l),_18424=u_string_l,_17116=[[bv(a17,u_string_l),bv(b17,_18424)]|_16160],get_var(_17116,a17,_18738),get_var(_17116,b17,_18766),(is_eq(_18738,_18766)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_string_l],eq,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]],_18890),_16990=_18890;get_var(_17116,a17,_18800),get_var(_17116,b17,_18846),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_18800,_18846],_18918),trace,_16990=_18918).
*/
wl:lambda_def(defun, u_string_l, f_u_string_l, [u_x], [[cond, [[stringp, u_x], u_x], [[symbolp, u_x], [symbol_name, u_x]], [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]]]).
wl:arglist_info(u_string_l, [u_x], [X_Get23], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_string_l).

/*

### Compiled:  `U::STRING_L` 
*/
f_u_string_l(X_Get23, ElseResult27) :-
	(   is_stringp(X_Get23)
	->  ElseResult27=X_Get23
	;   is_symbolp(X_Get23)
	->  cl_symbol_name(X_Get23, TrueResult),
	    ElseResult27=TrueResult
	;   cl_error(['$ARRAY'([*], claz_base_character, "type error")],
		     ElseResult),
	    ElseResult27=ElseResult
	).
:- set_opv(f_u_string_l, classof, claz_function),
   set_opv(u_string_l, compile_as, kw_function),
   set_opv(u_string_l, function, f_u_string_l),
   DefunResult=u_string_l,
   LEnv=[[bv(a17, u_string_l), bv(b17, DefunResult)]|TLEnv],
   get_var(LEnv, a17, A17_Get),
   get_var(LEnv, b17, B17_Get),
   (   is_eq(A17_Get, B17_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_string_l],
		   eq,
		   
		   [ defun,
		     u_string_l,
		     [u_x],
		     
		     [ cond,
		       [[stringp, u_x], u_x],
		       [[symbolp, u_x], [symbol_name, u_x]],
		       
		       [ t,
			 
			 [ error,
			   '$ARRAY'([*], claz_base_character, "type error")
			 ]
		       ]
		     ]
		   ]
		 ],
		 TrueResult39),
       LetResult=TrueResult39
   ;   get_var(LEnv, a17, A17_Get37),
       get_var(LEnv, b17, B17_Get38),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A17_Get37,
		   B17_Get38
		 ],
		 ElseResult40),
       trace,
       LetResult=ElseResult40
   ).
/*
:- side_effect(assert_lsp(u_string_l,
			  wl:lambda_def(defun, u_string_l, f_u_string_l, [u_x], [[cond, [[stringp, u_x], u_x], [[symbolp, u_x], [symbol_name, u_x]], [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]]]))).
*/
/*
:- side_effect(assert_lsp(u_string_l,
			  wl:arglist_info(u_string_l, [u_x], [X_Get23], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_string_l, wl:init_args(exact_only, u_string_l))).
*/
/*
(is eq () (TAGBODY 1 (PRINT "hi" )))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4290 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',1,['PRINT','$STRING'("hi")]]])
/*
% macroexpand:-[u_is,eq,[],[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]].
*/
/*
% into:-[let,[[a18,[]],[b18,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[if,[eq,a18,b18],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a18,b18],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-call_addr_block(_15810,(push_label(1),cl_print('$ARRAY'([*],claz_base_character,"hi"),_16356)),[addr(addr_tagbody_4_1,1,'$unused',_16482,cl_print('$ARRAY'([*],claz_base_character,"hi"),_16490))]),_16326=[[bv(a18,[]),bv(b18,[])]|_15810],get_var(_16326,a18,_16798),get_var(_16326,b18,_16826),(is_eq(_16798,_16826)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16840),_16200=_16840;get_var(_16326,a18,_16860),get_var(_16326,b18,_16906),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16860,_16906],_16842),trace,_16200=_16842).
*/
:-call_addr_block(_15722,(push_label(1),cl_print('$ARRAY'([*],claz_base_character,"hi"),_15846)),[addr(addr_tagbody_4_1,1,'$unused',_15874,cl_print('$ARRAY'([*],claz_base_character,"hi"),_15876))]),_15818=[[bv(a18,[]),bv(b18,[])]|_15722],get_var(_15818,a18,_16098),get_var(_15818,b18,_16124),(is_eq(_16098,_16124)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16136),_15706=_16136;get_var(_15818,a18,_16156),get_var(_15818,b18,_16196),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16156,_16196],_16138),trace,_15706=_16138).
/*
(is eq () (TAGBODY a (PRINT "hi" )))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4328 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',a,['PRINT','$STRING'("hi")]]])
/*
% macroexpand:-[u_is,eq,[],[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]].
*/
/*
% into:-[let,[[a19,[]],[b19,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[if,[eq,a19,b19],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a19,b19],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-call_addr_block(_15886,(push_label(u_a),cl_print('$ARRAY'([*],claz_base_character,"hi"),_16432)),[addr(addr_tagbody_5_u_a,u_a,'$unused',_16558,cl_print('$ARRAY'([*],claz_base_character,"hi"),_16566))]),_16402=[[bv(a19,[]),bv(b19,[])]|_15886],get_var(_16402,a19,_16874),get_var(_16402,b19,_16902),(is_eq(_16874,_16902)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16916),_16276=_16916;get_var(_16402,a19,_16936),get_var(_16402,b19,_16982),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16936,_16982],_16918),trace,_16276=_16918).
*/
:-call_addr_block(_15798,(push_label(u_a),cl_print('$ARRAY'([*],claz_base_character,"hi"),_15922)),[addr(addr_tagbody_5_u_a,u_a,'$unused',_15950,cl_print('$ARRAY'([*],claz_base_character,"hi"),_15952))]),_15894=[[bv(a19,[]),bv(b19,[])]|_15798],get_var(_15894,a19,_16174),get_var(_15894,b19,_16200),(is_eq(_16174,_16200)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16212),_15782=_16212;get_var(_15894,a19,_16232),get_var(_15894,b19,_16272),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16232,_16272],_16214),trace,_15782=_16214).
/*
(is eq () (LET ((val 1 ))NIL ))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4366 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]],[]]])
/*
% macroexpand:-[u_is,eq,[],[let,[[u_val,1]],[]]].
*/
/*
% into:-[let,[[a110,[]],[b110,[let,[[u_val,1]],[]]]],[if,[eq,a110,b110],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[let,[[u_val,1]],[]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a110,b110],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-_17618=[[bv(u_val,1)]|_15932],_17480=[[bv(a110,[]),bv(b110,[])]|_15932],get_var(_17480,a110,_22504),get_var(_17480,b110,_22970),(is_eq(_22504,_22970)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[let,[[u_val,1]],[]]],_23344),_16308=_23344;get_var(_17480,a110,_26132),get_var(_17480,b110,_27806),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26132,_27806],_24570),trace,_16308=_24570).
*/
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   LEnv=[[bv(a110, []), bv(b110, [])]|TLEnv],
   get_var(LEnv, a110, A110_Get),
   get_var(LEnv, b110, B110_Get),
   (   is_eq(A110_Get, B110_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [let, [[u_val, 1]], []]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a110, A110_Get15),
       get_var(LEnv, b110, B110_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A110_Get15,
		   B110_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq () (LET ((val 1 )) ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4398 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]]]])
/*
% macroexpand:-[u_is,eq,[],[let,[[u_val,1]]]].
*/
/*
% into:-[let,[[a201,[]],[b201,[let,[[u_val,1]]]]],[if,[eq,a201,b201],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[let,[[u_val,1]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a201,b201],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-_17684=[[bv(u_val,1)]|_16016],_17546=[[bv(a201,[]),bv(b201,[])]|_16016],get_var(_17546,a201,_22558),get_var(_17546,b201,_23024),(is_eq(_22558,_23024)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[let,[[u_val,1]]]],_23380),_16374=_23380;get_var(_17546,a201,_26150),get_var(_17546,b201,_27824),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26150,_27824],_24588),trace,_16374=_24588).
*/
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   LEnv=[[bv(a201, []), bv(b201, [])]|TLEnv],
   get_var(LEnv, a201, A201_Get),
   get_var(LEnv, b201, B201_Get),
   (   is_eq(A201_Get, B201_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [let, [[u_val, 1]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a201, A201_Get15),
       get_var(LEnv, b201, B201_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A201_Get15,
		   B201_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eql 1 (LET ((val 1 ))val ))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4428 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,1,['LET',[[val,1]],val]])
/*
% macroexpand:-[u_is,eql,1,[let,[[u_val,1]],u_val]].
*/
/*
% into:-[let,[[a22,1],[b22,[let,[[u_val,1]],u_val]]],[if,[eql,a22,b22],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eql],[quote,[let,[[u_val,1]],u_val]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a22,b22],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-_18340=[[bv(u_val,1)]|_16124],get_var(_18340,u_val,_18368),_17672=[[bv(a22,1),bv(b22,_18368)]|_16124],get_var(_17672,a22,_32506),get_var(_17672,b22,_32972),(is_eql(_32506,_32972)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eql,[let,[[u_val,1]],u_val]],_33346),_16500=_33346;get_var(_17672,a22,_36092),get_var(_17672,b22,_37724),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36092,_37724],_34572),trace,_16500=_34572).
*/
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   get_var(LEnv6, u_val, Val_Get),
   LEnv=[[bv(a22, 1), bv(b22, Val_Get)]|TLEnv],
   get_var(LEnv, a22, A22_Get),
   get_var(LEnv, b22, B22_Get),
   (   is_eql(A22_Get, B22_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eql,
		   [let, [[u_val, 1]], u_val]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a22, A22_Get17),
       get_var(LEnv, b22, B22_Get18),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A22_Get17,
		   B22_Get18
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eql 'world (LET ((a 'b) )(LET ((a 'world) )
  (LET ((a 'hello) )(LET ((a a)(*package* (find-package :keyword) ) )(PRINT a) ) )(PRINT a) ) ))


;; 3.1. Review of defstruct

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4460 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,[quote,world],['LET',[[a,[quote,b]]],['LET',[[a,[quote,world]]],['LET',[[a,[quote,hello]]],['LET',[[a,a],['*package*',['find-package',':keyword']]],['PRINT',a]]],['PRINT',a]]]])
/*
% macroexpand:-[u_is,eql,[quote,u_world],[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]].
*/
/*
% into:-[let,[[a23,[quote,u_world]],[b23,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]]],[if,[eql,a23,b23],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_world]],[quote,eql],[quote,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a23,b23],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-_18142=[[bv(u_a,u_b)]|_16944],_18214=[[bv(u_a,u_world)]|_18142],_18270=[[bv(u_a,u_hello)]|_18214],get_var(_18270,u_a,_18480),cl_find_package(kw_keyword,_18464),_18326=[[bv(u_a,_18480)]|_18270],save_special(sv(xx_package_xx,_18464,value,_18504)),get_var(_18326,u_a,_18522),cl_print(_18522,_18296),restore_special(sv(xx_package_xx,_18464,value,_18504)),get_var(_18214,u_a,_18580),cl_print(_18580,_18168),_18092=[[bv(a23,u_world),bv(b23,_18168)]|_16944],get_var(_18092,a23,_19124),get_var(_18092,b23,_19152),(is_eql(_19124,_19152)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_world],eql,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]],_19166),_17966=_19166;get_var(_18092,a23,_19186),get_var(_18092,b23,_19232),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_19186,_19232],_19168),trace,_17966=_19168).
*/
:- LEnv6=[[bv(u_a, u_b)]|TLEnv],
   LEnv8=[[bv(u_a, u_world)]|LEnv6],
   LEnv10=[[bv(u_a, u_hello)]|LEnv8],
   get_var(LEnv10, u_a, A_Get),
   cl_find_package(kw_keyword, Xx_package_xx_Init),
   LEnv12=[[bv(u_a, A_Get)]|LEnv10],
   save_special(sv(xx_package_xx, Xx_package_xx_Init, value, Value)),
   get_var(LEnv12, u_a, A_Get17),
   cl_print(A_Get17, LetResult11),
   restore_special(sv(xx_package_xx, Xx_package_xx_Init, value, Value)),
   get_var(LEnv8, u_a, A_Get20),
   cl_print(A_Get20, LetResult9),
   LEnv=[[bv(a23, u_world), bv(b23, LetResult9)]|TLEnv],
   get_var(LEnv, a23, A23_Get),
   get_var(LEnv, b23, B23_Get),
   (   is_eql(A23_Get, B23_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_world],
		   eql,
		   
		   [ let,
		     [[u_a, [quote, u_b]]],
		     
		     [ let,
		       [[u_a, [quote, u_world]]],
		       
		       [ let,
			 [[u_a, [quote, u_hello]]],
			 
			 [ let,
			   
			   [ [u_a, u_a],
			     [xx_package_xx, [find_package, kw_keyword]]
			   ],
			   [print, u_a]
			 ]
		       ],
		       [print, u_a]
		     ]
		   ]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a23, A23_Get30),
       get_var(LEnv, b23, B23_Get31),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A23_Get30,
		   B23_Get31
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
; 3.1. Review of defstruct
*/
/*
(progn #+WAM-CL (prolog-inline "nop(trace)")(is eq 'point (defstruct point x y z)))
;; (defstruct point x y z)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4636 **********************/
:-lisp_compile_to_prolog(pkg_user,[progn,['prolog-inline','$STRING'("nop(trace)")],[is,eq,[quote,point],[defstruct,point,x,y,z]]])
/*
% macroexpand:-[u_is,eq,[quote,u_point],[defstruct,u_point,u_x,u_y,u_z]].
*/
/*
% into:-[let,[[a24,[quote,u_point]],[b24,[defstruct,u_point,u_x,u_y,u_z]]],[if,[eq,a24,b24],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[defstruct,u_point,u_x,u_y,u_z]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a24,b24],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_defstruct([u_point,u_x,u_y,u_z],_19288),_18938=[[bv(a24,u_point),bv(b24,_19288)]|_16750],get_var(_18938,a24,_26554),get_var(_18938,b24,_27020),(is_eq(_26554,_27020)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[defstruct,u_point,u_x,u_y,u_z]],_27412),_17754=_27412;get_var(_18938,a24,_30176),get_var(_18938,b24,_31808),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_30176,_31808],_28656),trace,_17754=_28656).
*/
:- nop(trace),
   cl_defstruct([u_point, u_x, u_y, u_z], B24_Init),
   LEnv=[[bv(a24, u_point), bv(b24, B24_Init)]|TLEnv],
   get_var(LEnv, a24, A24_Get),
   get_var(LEnv, b24, B24_Get),
   (   is_eq(A24_Get, B24_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [defstruct, u_point, u_x, u_y, u_z]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a24, A24_Get13),
       get_var(LEnv, b24, B24_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A24_Get13,
		   B24_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 1, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 2, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 3, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 4, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 5, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 6, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 7, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 8, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 9, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 10, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 11, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 12, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 13, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 14, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 15, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 16, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 17, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 18, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 19, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 20, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, symbolname, u_point))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, type, u_point))).
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
; (defstruct point x y z)
*/
/*
(is eq 'point4d (defstruct point4d x y z t))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4748 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point4d],[defstruct,point4d,x,y,z,t]])
/*
% macroexpand:-[u_is,eq,[quote,u_point4d],[defstruct,u_point4d,u_x,u_y,u_z,t]].
*/
/*
% into:-[let,[[a25,[quote,u_point4d]],[b25,[defstruct,u_point4d,u_x,u_y,u_z,t]]],[if,[eq,a25,b25],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point4d]],[quote,eq],[quote,[defstruct,u_point4d,u_x,u_y,u_z,t]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a25,b25],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_defstruct([u_point4d,u_x,u_y,u_z,t],_19350),_19000=[[bv(a25,u_point4d),bv(b25,_19350)]|_17392],get_var(_19000,a25,_26646),get_var(_19000,b25,_27112),(is_eq(_26646,_27112)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point4d],eq,[defstruct,u_point4d,u_x,u_y,u_z,t]],_27522),_17804=_27522;get_var(_19000,a25,_30304),get_var(_19000,b25,_31936),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_30304,_31936],_28784),trace,_17804=_28784).
*/
:- cl_defstruct([u_point4d, u_x, u_y, u_z, t], B25_Init),
   LEnv=[[bv(a25, u_point4d), bv(b25, B25_Init)]|TLEnv],
   get_var(LEnv, a25, A25_Get),
   get_var(LEnv, b25, B25_Get),
   (   is_eq(A25_Get, B25_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point4d],
		   eq,
		   [defstruct, u_point4d, u_x, u_y, u_z, t]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a25, A25_Get13),
       get_var(LEnv, b25, B25_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A25_Get13,
		   B25_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 1, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 2, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 3, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 4, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 5, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 6, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 7, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 8, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 9, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 10, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 11, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 12, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 13, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 14, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 15, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 16, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 17, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 18, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 19, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 20, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, symbolname, u_point4d))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, type, u_point4d))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, u_x, zlot_point4d_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_x, zlot_point4d_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 1, zlot_point4d_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, u_y, zlot_point4d_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_y, zlot_point4d_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 2, zlot_point4d_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, u_z, zlot_point4d_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_z, zlot_point4d_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 3, zlot_point4d_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, t, zlot_point4d_t))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_t, zlot_point4d_t))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 4, zlot_point4d_t))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, conc_name, "POINT4D-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_x, zlot_point4d_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_x, zlot_point4d_x))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_y, zlot_point4d_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_y, zlot_point4d_y))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_z, zlot_point4d_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_z, zlot_point4d_z))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_t, zlot_point4d_t))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_t, zlot_point4d_t))).
*/
/*
(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4794 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'distance-from-origin',[point],['let*',[[x,['point-x',point]],[y,['point-y',point]],[z,['point-z',point]]],[sqrt,[+,[*,x,x],[*,y,y],[*,z,z]]]]])
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_distance_from_origin).

/*

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
*/
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	f_u_point_x(Point_Param, X_Init),
	LEnv=[[bv(u_x, X_Init)]|Env],
	f_u_point_y(Point_Param, Y_Init),
	LEnv16=[[bv(u_y, Y_Init)]|LEnv],
	f_u_point_z(Point_Param, Z_Init),
	LEnv20=[[bv(u_z, Z_Init)]|LEnv16],
	get_var(LEnv20, u_x, X_Get25),
	*(X_Get25, X_Get25, _19162),
	get_var(LEnv20, u_y, Y_Get27),
	*(Y_Get27, Y_Get27, _19260),
	+(_19162, _19260, _19380),
	get_var(LEnv20, u_z, Z_Get29),
	*(Z_Get29, Z_Get29, _19392),
	+(_19380, _19392, Sqrt_Param),
	cl_sqrt(Sqrt_Param, LetResult17),
	LetResult17=FnResult.
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
			  wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:init_args(exact_only, u_distance_from_origin))).
*/
/*
(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4960 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'reflect-in-y-axis',[point],[setf,['point-y',point],[-,['point-y',point]]]])
wl:lambda_def(defun, u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).
wl:arglist_info(u_reflect_in_y_axis, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_reflect_in_y_axis).

/*

### Compiled:  `U::REFLECT-IN-Y-AXIS` 
*/
f_u_reflect_in_y_axis(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	f_u_point_y(Point_Param, Point_y_Ret),
	-(0, Point_y_Ret, CAR),
	set_place(Env, setf, [u_point_y, Point_Param], [CAR], Setf_R),
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
			  wl:arglist_info(u_reflect_in_y_axis, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_reflect_in_y_axis,
			  wl:init_args(exact_only, u_reflect_in_y_axis))).
*/
/*
(list (setf my-point (make-point :x 3 :y 4 :z 12)) (setf my-point2 (make-point :x 3 :y 4 :z 12)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5048 **********************/
:-lisp_compile_to_prolog(pkg_user,[list,[setf,'my-point',['make-point',':x',3,':y',4,':z',12]],[setf,'my-point2',['make-point',':x',3,':y',4,':z',12]]])
:- f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], Make_point_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_point_Ret], Setf_R),
   f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], Make_point_Ret8),
   set_place(TLEnv, setf, [value, u_my_point2], [Make_point_Ret8], Setf_R6),
   _Ignored=[Setf_R, Setf_R6].
/*
(setf my-point3 #S(POINT :X 3 :Y 4 :Z 12))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5146 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point3','$S'(['POINT',':X',3,':Y',4,':Z',12])])
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_my_point3], [Create_struct_Ret], Setf_R).
/*
(setf my-point4d (make-point4d :x 3 :y 4 :z 12 :t 1))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5189 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point4d',['make-point4d',':x',3,':y',4,':z',12,':t',1]])
:- f_u_make_point4d([kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1], Make_point4d_Ret),
   set_place(TLEnv, setf, [value, u_my_point4d], [Make_point4d_Ret], Setf_R).
/*
(is eq t (point-p my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5245 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,['point-p','my-point']])
/*
% macroexpand:-[u_is,eq,t,[u_point_p,u_my_point]].
*/
/*
% into:-[let,[[a26,t],[b26,[u_point_p,u_my_point]]],[if,[eq,a26,b26],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[u_point_p,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a26,b26],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_18606,u_my_point,_20582),f_u_point_p(_20582,_20454),_20034=[[bv(a26,t),bv(b26,_20454)]|_18606],get_var(_20034,a26,_30162),get_var(_20034,b26,_30628),(is_eq(_30162,_30628)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[u_point_p,u_my_point]],_30930),_18910=_30930;get_var(_20034,a26,_33604),get_var(_20034,b26,_35236),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33604,_35236],_32084),trace,_18910=_32084).
*/
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_point_p(My_point_Get, B26_Init),
   LEnv=[[bv(a26, t), bv(b26, B26_Init)]|TLEnv],
   get_var(LEnv, a26, A26_Get),
   get_var(LEnv, b26, B26_Get),
   (   is_eq(A26_Get, B26_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [u_point_p, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a26, A26_Get14),
       get_var(LEnv, b26, B26_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A26_Get14,
		   B26_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq 'point (type-of my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5275 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point],['type-of','my-point']])
/*
% macroexpand:-[u_is,eq,[quote,u_point],[type_of,u_my_point]].
*/
/*
% into:-[let,[[a27,[quote,u_point]],[b27,[type_of,u_my_point]]],[if,[eq,a27,b27],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[type_of,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a27,b27],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_18728,u_my_point,_20764),cl_type_of(_20764,_20636),_20216=[[bv(a27,u_point),bv(b27,_20636)]|_18728],get_var(_20216,a27,_30394),get_var(_20216,b27,_30860),(is_eq(_30394,_30860)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[type_of,u_my_point]],_31198),_19068=_31198;get_var(_20216,a27,_33908),get_var(_20216,b27,_35540),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33908,_35540],_32388),trace,_19068=_32388).
*/
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_type_of(My_point_Get, B27_Init),
   LEnv=[[bv(a27, u_point), bv(b27, B27_Init)]|TLEnv],
   get_var(LEnv, a27, A27_Get),
   get_var(LEnv, b27, B27_Get),
   (   is_eq(A27_Get, B27_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [type_of, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a27, A27_Get14),
       get_var(LEnv, b27, B27_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A27_Get14,
		   B27_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
#+IGNORE #+WAM-CL (prolog-call "break")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5310 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':IGNORE'],[#+,':WAM-CL',['prolog-call','$STRING'("break")]]]))
/*
(is eql 13 (progn (print (distance-from-origin my-point))))

;; #+CLISP (BREAK)
;; #+WAM-CL (prolog-call "break")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5351 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eql,13,[progn,[print,['distance-from-origin','my-point']]]])
/*
% macroexpand:-[u_is,eql,13,[progn,[print,[u_distance_from_origin,u_my_point]]]].
*/
/*
% into:-[let,[[a28,13],[b28,[progn,[print,[u_distance_from_origin,u_my_point]]]]],[if,[eql,a28,b28],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,13],[quote,eql],[quote,[progn,[print,[u_distance_from_origin,u_my_point]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a28,b28],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_18842,u_my_point,_19380),f_u_distance_from_origin(_19380,_19378),cl_print(_19378,_19374),_19342=[[bv(a28,13),bv(b28,_19374)]|_18842],get_var(_19342,a28,_19838),get_var(_19342,b28,_19866),(is_eql(_19838,_19866)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),13,eql,[progn,[print,[u_distance_from_origin,u_my_point]]]],_19880),_19216=_19880;get_var(_19342,a28,_19900),get_var(_19342,b28,_19946),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_19900,_19946],_19882),trace,_19216=_19882).
*/
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, Print_Param),
   cl_print(Print_Param, B28_Init),
   LEnv=[[bv(a28, 13), bv(b28, B28_Init)]|TLEnv],
   get_var(LEnv, a28, A28_Get),
   get_var(LEnv, b28, B28_Get),
   (   is_eql(A28_Get, B28_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   13,
		   eql,
		   [progn, [print, [u_distance_from_origin, u_my_point]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a28, A28_Get14),
       get_var(LEnv, b28, B28_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A28_Get14,
		   B28_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
; #+CLISP (BREAK)
*/
/*
; #+WAM-CL (prolog-call "break")
*/
/*
(is = -4 (reflect-in-y-axis my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5466 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,=,-4,['reflect-in-y-axis','my-point']])
/*
% macroexpand:-[u_is,=,-4,[u_reflect_in_y_axis,u_my_point]].
*/
/*
% into:-[let,[[a29,-4],[b29,[u_reflect_in_y_axis,u_my_point]]],[if,[=,a29,b29],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,-4],[quote,=],[quote,[u_reflect_in_y_axis,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a29,b29],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_18828,u_my_point,_20804),f_u_reflect_in_y_axis(_20804,_20676),_20256=[[bv(a29,-4),bv(b29,_20676)]|_18828],get_var(_20256,a29,_30230),get_var(_20256,b29,_30696),(_30230=:=_30696->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),-4,=,[u_reflect_in_y_axis,u_my_point]],_30998),_19132=_30998;get_var(_20256,a29,_33672),get_var(_20256,b29,_35304),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33672,_35304],_32152),trace,_19132=_32152).
*/
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_reflect_in_y_axis(My_point_Get, B29_Init),
   LEnv=[[bv(a29, -4), bv(b29, B29_Init)]|TLEnv],
   get_var(LEnv, a29, A29_Get),
   get_var(LEnv, b29, B29_Get),
   (   A29_Get=:=B29_Get
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   -4,
		   (=),
		   [u_reflect_in_y_axis, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a29, A29_Get14),
       get_var(LEnv, b29, B29_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A29_Get14,
		   B29_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(is eq my-point my-point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5506 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,'my-point','my-point'])
/*
% macroexpand:-[u_is,eq,u_my_point,u_my_point].
*/
/*
% into:-[let,[[a210,u_my_point],[b210,u_my_point]],[if,[eq,a210,b210],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_my_point],[quote,eq],[quote,u_my_point]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a210,b210],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_18882,u_my_point,_25368),get_var(_18882,u_my_point,_23506),_20250=[[bv(a210,_25368),bv(b210,_23506)]|_18882],get_var(_20250,a210,_32968),get_var(_20250,b210,_33434),(is_eq(_32968,_33434)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),u_my_point,eq,u_my_point],_33700),_19150=_33700;get_var(_20250,a210,_36380),get_var(_20250,b210,_38054),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36380,_38054],_34818),trace,_19150=_34818).
*/
:- get_var(TLEnv, u_my_point, My_point_Get7),
   LEnv=[[bv(a210, My_point_Get7), bv(b210, My_point_Get7)]|TLEnv],
   get_var(LEnv, a210, A210_Get),
   get_var(LEnv, b210, B210_Get),
   (   is_eq(A210_Get, B210_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   u_my_point,
		   eq,
		   u_my_point
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a210, A210_Get16),
       get_var(LEnv, b210, B210_Get17),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A210_Get16,
		   B210_Get17
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(setf a-similar-point #s(point :x 3 :y -4 :z 12))

; (is eq t (equal my-point a-similar-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5533 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'a-similar-point','$S'([point,':x',3,':y',-4,':z',12])])
:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_a_similar_point], [Create_struct_Ret], Setf_R).
/*
 (is eq t (equal my-point a-similar-point))
*/
/*
(is eq nil (eq my-point a-similar-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5630 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],[eq,'my-point','a-similar-point']])
/*
% macroexpand:-[u_is,eq,[],[eq,u_my_point,u_a_similar_point]].
*/
/*
% into:-[let,[[a301,[]],[b301,[eq,u_my_point,u_a_similar_point]]],[if,[eq,a301,b301],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[eq,u_my_point,u_a_similar_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a301,b301],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_19046,u_my_point,_21070),get_var(_19046,u_a_similar_point,_22864),cl_eq(_21070,_22864,_20936),_20516=[[bv(a301,[]),bv(b301,_20936)]|_19046],get_var(_20516,a301,_33150),get_var(_20516,b301,_33616),(is_eq(_33150,_33616)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[eq,u_my_point,u_a_similar_point]],_33936),_19368=_33936;get_var(_20516,a301,_36670),get_var(_20516,b301,_38344),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36670,_38344],_35108),trace,_19368=_35108).
*/
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_eq(My_point_Get, A_similar_point_Get, B301_Init),
   LEnv=[[bv(a301, []), bv(b301, B301_Init)]|TLEnv],
   get_var(LEnv, a301, A301_Get),
   get_var(LEnv, b301, B301_Get),
   (   is_eq(A301_Get, B301_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [eq, u_my_point, u_a_similar_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a301, A301_Get15),
       get_var(LEnv, b301, B301_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A301_Get15,
		   B301_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(equalp my-point a-similar-point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5673 **********************/
:-lisp_compile_to_prolog(pkg_user,[equalp,'my-point','a-similar-point'])
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, _Ignored).
/*
(is eq t (equalp my-point a-similar-point) )


;; 3.2. defclass

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5708 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[equalp,'my-point','a-similar-point']])
/*
% macroexpand:-[u_is,eq,t,[equalp,u_my_point,u_a_similar_point]].
*/
/*
% into:-[let,[[a32,t],[b32,[equalp,u_my_point,u_a_similar_point]]],[if,[eq,a32,b32],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[equalp,u_my_point,u_a_similar_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a32,b32],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_19140,u_my_point,_21152),get_var(_19140,u_a_similar_point,_22946),cl_equalp(_21152,_22946,_21018),_20598=[[bv(a32,t),bv(b32,_21018)]|_19140],get_var(_20598,a32,_33106),get_var(_20598,b32,_33572),(is_eq(_33106,_33572)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[equalp,u_my_point,u_a_similar_point]],_33892),_19462=_33892;get_var(_20598,a32,_36584),get_var(_20598,b32,_38216),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36584,_38216],_35064),trace,_19462=_35064).
*/
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, B32_Init),
   LEnv=[[bv(a32, t), bv(b32, B32_Init)]|TLEnv],
   get_var(LEnv, a32, A32_Get),
   get_var(LEnv, b32, B32_Get),
   (   is_eq(A32_Get, B32_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [equalp, u_my_point, u_a_similar_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a32, A32_Get15),
       get_var(LEnv, b32, B32_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A32_Get15,
		   B32_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
; 3.2. defclass
*/
/*
(unintern 'point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5773 **********************/
:-lisp_compile_to_prolog(pkg_user,[unintern,[quote,point]])
:- cl_unintern(u_point, _Ignored).
/*
(defclass point ()
  (x
   y
   z))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5792 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5829 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point',['make-instance',[quote,point]]])
:- cl_make_instance([u_point], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_instance_Ret], Setf_R).
/*
(is eq 'point (type-of my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5869 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point],['type-of','my-point']])
/*
% macroexpand:-[u_is,eq,[quote,u_point],[type_of,u_my_point]].
*/
/*
% into:-[let,[[a33,[quote,u_point]],[b33,[type_of,u_my_point]]],[if,[eq,a33,b33],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[type_of,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a33,b33],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-get_var(_19272,u_my_point,_21308),cl_type_of(_21308,_21180),_20760=[[bv(a33,u_point),bv(b33,_21180)]|_19272],get_var(_20760,a33,_30938),get_var(_20760,b33,_31404),(is_eq(_30938,_31404)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[type_of,u_my_point]],_31742),_19612=_31742;get_var(_20760,a33,_34452),get_var(_20760,b33,_36084),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_34452,_36084],_32932),trace,_19612=_32932).
*/
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_type_of(My_point_Get, B33_Init),
   LEnv=[[bv(a33, u_point), bv(b33, B33_Init)]|TLEnv],
   get_var(LEnv, a33, A33_Get),
   get_var(LEnv, b33, B33_Get),
   (   is_eq(A33_Get, B33_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [type_of, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a33, A33_Get14),
       get_var(LEnv, b33, B33_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A33_Get14,
		   B33_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
(defun set-point-values (point x y z)
  (setf (slot-value point 'x) x
        (slot-value point 'y) y
        (slot-value point 'z) z))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5904 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'set-point-values',[point,x,y,z],[setf,['slot-value',point,[quote,x]],x,['slot-value',point,[quote,y]],y,['slot-value',point,[quote,z]],z]])
wl:lambda_def(defun, u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]).
wl:arglist_info(u_set_point_values, [u_point, u_x, u_y, u_z], [Point_Param, X_Param, Y_Param, Z_Param], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_set_point_values).

/*

### Compiled:  `U::SET-POINT-VALUES` 
*/
f_u_set_point_values(Point_Param, X_Param, Y_Param, Z_Param, FnResult) :-
	Env=[bv(u_point, Point_Param), bv(u_x, X_Param), bv(u_y, Y_Param), bv(u_z, Z_Param)],
	set_place(Env, setf, [slot_value, Point_Param, u_x], [X_Param], Setf_R),
	set_place(Env, setf, [slot_value, Point_Param, u_y], [Y_Param], Setf_R23),
	set_place(Env, setf, [slot_value, Point_Param, u_z], [Z_Param], Setf_R26),
	Setf_R26=FnResult.
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
			  wl:arglist_info(u_set_point_values, [u_point, u_x, u_y, u_z], [Point_Param, X_Param, Y_Param, Z_Param], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_set_point_values,
			  wl:init_args(exact_only, u_set_point_values))).
*/
/*
(set-point-values my-point 3 4 12)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6041 **********************/
:-lisp_compile_to_prolog(pkg_user,['set-point-values','my-point',3,4,12])
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_set_point_values(My_point_Get, 3, 4, 12, _Ignored).
/*
(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6077 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'distance-from-origin',[point],['with-slots',[x,y,z],point,[sqrt,[+,[*,x,x],[*,y,y],[*,z,z]]]]])
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_distance_from_origin).

/*

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
*/
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	cl_slot_value(Point_Param, u_x, X_Init),
	cl_slot_value(Point_Param, u_y, Y_Init),
	cl_slot_value(Point_Param, u_z, Z_Init),
	LEnv=[[bv(u_x, X_Init), bv(u_y, Y_Init), bv(u_z, Z_Init)]|Env],
	get_var(LEnv, u_x, X_Get21),
	*(X_Get21, X_Get21, _20146),
	get_var(LEnv, u_y, Y_Get23),
	*(Y_Get23, Y_Get23, _20302),
	+(_20146, _20302, _20480),
	get_var(LEnv, u_z, Z_Get25),
	*(Z_Get25, Z_Get25, _20492),
	+(_20480, _20492, Sqrt_Param),
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
			  wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:init_args(exact_only, u_distance_from_origin))).
*/
/*
(DISASSEMBLE #'distance-from-origin)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6190 **********************/
:-lisp_compile_to_prolog(pkg_user,['DISASSEMBLE',function('distance-from-origin')])
:- cl_disassemble(function(u_distance_from_origin), _Ignored).
/*
(distance-from-origin my-point)

;; 3.3. classes are objects

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6229 **********************/
:-lisp_compile_to_prolog(pkg_user,['distance-from-origin','my-point'])
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, _Ignored).
/*
; 3.3. classes are objects
*/
/*
(find-class 'point)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6291 **********************/
:-lisp_compile_to_prolog(pkg_user,['find-class',[quote,point]])
:- cl_find_class(u_point, _Ignored).
/*
(class-name (find-class 'point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6312 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-name',['find-class',[quote,point]]])
:- cl_find_class(u_point, Class_name_Param),
   cl_class_name(Class_name_Param, _Ignored).
/*
(class-of my-point)

;; #-(or cormanlisp CLISP WAM-CL)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6346 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-of','my-point'])
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, _Ignored).
/*
; #-(or cormanlisp CLISP WAM-CL)
*/
/*
(typep my-point (class-of my-point))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6401 **********************/
:-lisp_compile_to_prolog(pkg_user,[typep,'my-point',['class-of','my-point']])
:- get_var(TLEnv, u_my_point, My_point_Get5),
   cl_class_of(My_point_Get5, Class_of_Ret),
   cl_typep(My_point_Get5, Class_of_Ret, _Ignored).
/*
(is eq (find-class 'STANDARD-CLASS)
       (class-of (class-of my-point)))

;; 3.4. you don't need clos to use clos

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6439 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,['find-class',[quote,'STANDARD-CLASS']],['class-of',['class-of','my-point']]])
/*
% macroexpand:-[u_is,eq,[find_class,[quote,standard_class]],[class_of,[class_of,u_my_point]]].
*/
/*
% into:-[let,[[a34,[find_class,[quote,standard_class]]],[b34,[class_of,[class_of,u_my_point]]]],[if,[eq,a34,b34],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[find_class,[quote,standard_class]]],[quote,eq],[quote,[class_of,[class_of,u_my_point]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a34,b34],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_find_class(standard_class,_20094),get_var(_19526,u_my_point,_20112),cl_class_of(_20112,_20108),cl_class_of(_20108,_20104),_20062=[[bv(a34,_20094),bv(b34,_20104)]|_19526],get_var(_20062,a34,_20584),get_var(_20062,b34,_20612),(is_eq(_20584,_20612)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[find_class,[quote,standard_class]],eq,[class_of,[class_of,u_my_point]]],_20626),_19936=_20626;get_var(_20062,a34,_20646),get_var(_20062,b34,_20692),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_20646,_20692],_20628),trace,_19936=_20628).
*/
:- cl_find_class(standard_class, A34_Init),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, Class_of_Param),
   cl_class_of(Class_of_Param, B34_Init),
   LEnv=[[bv(a34, A34_Init), bv(b34, B34_Init)]|TLEnv],
   get_var(LEnv, a34, A34_Get),
   get_var(LEnv, b34, B34_Get),
   (   is_eq(A34_Get, B34_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [find_class, [quote, standard_class]],
		   eq,
		   [class_of, [class_of, u_my_point]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a34, A34_Get15),
       get_var(LEnv, b34, B34_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A34_Get15,
		   B34_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6556 **********************/
:-lisp_compile_to_prolog(pkg_user,[let,[['the-symbol-class',['find-class',[quote,symbol]]]],[values,'the-symbol-class',['class-name','the-symbol-class'],[eq,'the-symbol-class',['class-of',[quote,symbol]]],['class-of','the-symbol-class']]])
:- cl_find_class(symbol, The_symbol_class_Init),
   LEnv=[[bv(u_the_symbol_class, The_symbol_class_Init)]|TLEnv],
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get8),
   cl_class_name(The_symbol_class_Get8, Class_name_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get9),
   cl_class_of(symbol, Class_of_Ret),
   cl_eq(The_symbol_class_Get9, Class_of_Ret, Eq_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get10),
   cl_class_of(The_symbol_class_Get10, Class_of_Ret14),
   nb_setval('$mv_return',
	     [The_symbol_class_Get8, Class_name_Ret, Eq_Ret, Class_of_Ret14]).
/*
(find-class t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6762 **********************/
:-lisp_compile_to_prolog(pkg_user,['find-class',t])
:- cl_find_class(t, _Ignored).
/*
(is eq 'foo (defstruct foo))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6778 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,foo],[defstruct,foo]])
/*
% macroexpand:-[u_is,eq,[quote,u_foo],[defstruct,u_foo]].
*/
/*
% into:-[let,[[a35,[quote,u_foo]],[b35,[defstruct,u_foo]]],[if,[eq,a35,b35],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_foo]],[quote,eq],[quote,[defstruct,u_foo]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a35,b35],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_defstruct([u_foo],_21412),_21062=[[bv(a35,u_foo),bv(b35,_21412)]|_19574],get_var(_21062,a35,_28540),get_var(_21062,b35,_29006),(is_eq(_28540,_29006)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_foo],eq,[defstruct,u_foo]],_29344),_19914=_29344;get_var(_21062,a35,_32054),get_var(_21062,b35,_33686),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32054,_33686],_30534),trace,_19914=_30534).
*/
:- cl_defstruct([u_foo], B35_Init),
   LEnv=[[bv(a35, u_foo), bv(b35, B35_Init)]|TLEnv],
   get_var(LEnv, a35, A35_Get),
   get_var(LEnv, b35, B35_Get),
   (   is_eq(A35_Get, B35_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_foo],
		   eq,
		   [defstruct, u_foo]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a35, A35_Get13),
       get_var(LEnv, b35, B35_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A35_Get13,
		   B35_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 1, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 2, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 3, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 4, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 5, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 6, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 7, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 8, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 9, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 10, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 11, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 12, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 13, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 14, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 15, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 16, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 17, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 18, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 19, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 20, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_foo, symbolname, u_foo))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_foo, type, u_foo))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_foo, conc_name, "FOO-"))).
*/
/*
(is eq (find-class 'foo) (class-of (make-foo)))

;; 3.5 slots

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6808 **********************/
:-lisp_compile_to_prolog(pkg_user,[is,eq,['find-class',[quote,foo]],['class-of',['make-foo']]])
/*
% macroexpand:-[u_is,eq,[find_class,[quote,u_foo]],[class_of,[u_make_foo]]].
*/
/*
% into:-[let,[[a36,[find_class,[quote,u_foo]]],[b36,[class_of,[u_make_foo]]]],[if,[eq,a36,b36],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[find_class,[quote,u_foo]]],[quote,eq],[quote,[class_of,[u_make_foo]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a36,b36],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
*/
/*
% code:-cl_find_class(u_foo,_20458),f_u_make_foo([],_20472),cl_class_of(_20472,_20468),_20426=[[bv(a36,_20458),bv(b36,_20468)]|_19908],get_var(_20426,a36,_20820),get_var(_20426,b36,_20848),(is_eq(_20820,_20848)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[find_class,[quote,u_foo]],eq,[class_of,[u_make_foo]]],_20862),_20300=_20862;get_var(_20426,a36,_20882),get_var(_20426,b36,_20928),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_20882,_20928],_20864),trace,_20300=_20864).
*/
:- cl_find_class(u_foo, A36_Init),
   f_u_make_foo([], Class_of_Param),
   cl_class_of(Class_of_Param, B36_Init),
   LEnv=[[bv(a36, A36_Init), bv(b36, B36_Init)]|TLEnv],
   get_var(LEnv, a36, A36_Get),
   get_var(LEnv, b36, B36_Get),
   (   is_eq(A36_Get, B36_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [find_class, [quote, u_foo]],
		   eq,
		   [class_of, [u_make_foo]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a36, A36_Get14),
       get_var(LEnv, b36, B36_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A36_Get14,
		   B36_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
; 3.5 slots
*/
/*
(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader daft-z :allocation :class)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6871 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7017 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,['slot-value',['make-instance',[quote,'daft-point']],[quote,z]],42])
:- cl_make_instance([u_daft_point], Make_instance_Ret),
   set_place(TLEnv, setf, [slot_value, Make_instance_Ret, u_z], [42], Setf_R).
/*
(setf my-daft-point (make-instance 'daft-point :x 19))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7072 **********************/
:-lisp_compile_to_prolog(pkg_user,[setf,'my-daft-point',['make-instance',[quote,'daft-point'],':x',19]])
:- cl_make_instance([u_daft_point, kw_x, 19], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_daft_point], [Make_instance_Ret], Setf_R).
/*
#+PERFECT 
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (progn #+WAM-CL (prolog-trace) (daft-z my-daft-point)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7129 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':PERFECT'],[list,['daft-x','my-daft-point'],['daft-y','my-daft-point'],[progn,[#+,':WAM-CL',['prolog-trace']],['daft-z','my-daft-point']]]]))
/*
(let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7261 **********************/
:-lisp_compile_to_prolog(pkg_user,[let,[[temp,['make-instance',[quote,'daft-point']]]],[setf,['daft-y',temp],999,['slot-value',temp,[quote,z]],0]])
:- cl_make_instance([u_daft_point], Temp_Init),
   LEnv=[[bv(u_temp, Temp_Init)]|TLEnv],
   get_var(LEnv, u_temp, Temp_Get),
   set_place(LEnv, setf, [u_daft_y, Temp_Get], [999], Setf_R),
   get_var(LEnv, u_temp, Temp_Get11),
   set_place(LEnv, setf, [slot_value, Temp_Get11, u_z], [0], Setf_R10).
/*
#+PERFECT
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

;; 3.6 Subclasses and inheritance

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7363 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':PERFECT'],[list,['daft-x','my-daft-point'],['daft-y','my-daft-point'],['daft-z','my-daft-point']]]))
/*
; 3.6 Subclasses and inheritance
*/
/*
(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7497 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7619 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7694 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7768 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7968 **********************/
:-lisp_compile_to_prolog(pkg_user,['make-instance',[quote,aardvark]])
:- cl_make_instance([u_aardvark], _Ignored).
/*
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
   (find-class 'aardvark))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7995 **********************/
:-lisp_compile_to_prolog(pkg_user,['class-precedence-list',['find-class',[quote,aardvark]]])
:- cl_find_class(u_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).
/*
(defclass figurine ()
  ((potter :accessor made-by :initarg :made-by)
   (comes-from :initarg :made-in)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8095 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8202 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8426 **********************/
:-lisp_compile_to_prolog(pkg_user,['make-instance',[quote,'figurine-aardvark']])
:- cl_make_instance([u_figurine_aardvark], _Ignored).
/*
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
             (find-class 'figurine-aardvark))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8462 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8581 **********************/
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
		    Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_eric], [Make_instance_Ret], Setf_R).
/*
#+HAS_SHIFTF
(shiftf (cute-p Eric) t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8798 **********************/
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'([flag_removed,[+,':HAS_SHIFTF'],[shiftf,['cute-p','Eric'],t]]))
/*
(slot-value Eric 'diet)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8837 **********************/
:-lisp_compile_to_prolog(pkg_user,['slot-value','Eric',[quote,diet]])
:- get_var(TLEnv, u_eric, Eric_Get),
   cl_slot_value(Eric_Get, u_diet, _Ignored).


%; Total compilation time: 14.922 seconds


```bash
root@gitlab:/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl# swipl repl.pl
Installed packages (38):

i clause_attvars@1.1.118    - An alternate interface to the clause database to allow attributed variables to be asserted
i dictoo@1.1.118            - Dict-like OO Syntax
i each_call_cleanup@1.1.118 - Each Call Redo Setup and Cleanup
i eggdrop@1.1.118           - Hook up to an existing IRC Client called an Eggdrop
i file_scope@1.1.118        - File local scoped efects
i fluxplayer-prolog-engine@0.0.1 - Prolog interface to Slack http://www.slack.com
i gvar_syntax@1.1.118       - Global Variable Syntax
i hook_hybrid@1.1.118       - Hook assert retract call of *specific* predicates
i instant_prolog_docs@1.1.118 - Magically document prolog source files based on predicate and variable naming conventions
i lib_atts@1.1.118          - Common atts.pl interface like https://sicstus.sics.se/sicstus/docs/4.0.0/html/sicstus/lib_002datts.html
i logicmoo_base@1.1.118     - LogicMOO - Extends Prolog Programming to support Dynamic Epistemic Logic (DEL) with Constraints
i logicmoo_experimental@1.1.118 - Various experimental packages - warning: HUGE amount of test data
i logicmoo_nlu@1.1.114      - Various English to Logic Convertors - warning: HUGE amount of test data
i logicmoo_packages@1.1.118 - Various packages - warning: HUGE amount of test data
i logicmoo_planner@1.1.118  - Various PDDLish planners - warning: HUGE amount of test data
i logicmoo_planners@1.1.118 - Various Hybrid HTN Planners speaking PDDLish and OCLh
i logicmoo_utils@1.1.118    - Common predicates used by external Logicmoo Utils and Base
i loop_check@1.1.118        - New simple loop checking
i mpi@1.0                   - Porting of the LAMMPI library of Yap Prolog to SWI-Prolog
i multimodal_dcg@1.1.118    - Reduce floundering of DCGs by constraining and narrowing search
i multivar@1.1.118          - User defined datatypes
i must_trace@1.1.118        - Trace with your eyeballs instead of your fingers
i no_repeats@1.1.118        - New ways to avoid duplicate solutions
i pfc@1.1.118               - Pfc -- a package for forward chaining in Prolog
i predicate_streams@1.1.118 - Implement your own Abstract Predicate Streams
i prologmud@1.1.118         - Online text adventure game - MUD Server
i prologmud_samples@1.1.118 - Online text adventure game - Sample
i s_expression@1.1.118      - Utilities for Handling of S-Expression Lisp/Scheme-Like forms and parsing of KIF, GDL, PDDL, CLIF
i slack_prolog@1.1.118      - Prolog interface to Slack http://www.slack.com
i subclause_expansion@1.1.118 - More use specific versions of term/goal expansion hooks
i tabling_dra@1.1.118       - SWI-Prolog interface to Table-handling procedures for the "dra" interpreter. Written by Feliks Kluzniak at UTD (March 2009)
i transpiler@0.1            - A universal translator for programming languages
i trill@4.1.0               - A tableau probabilistic reasoner in three different versions
i wam_common_lisp@1.1.118   - ANSI Common Lisp implemented in Prolog
i with_open_options@1.1.118 - Utilities to open various objects for read/write
i with_thread_local@1.1.118 - Call a Goal with local assertions
i xlisting@1.1.118          - Selective Interactive Non-Deterministic Tracing
i xlisting_web@1.1.118      - Manipulate and browse prolog runtime over www

```prolog
:- success(always(call((to_lisp_pathname('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl', '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl"))), set_opv(ext_xx_lisp_home_xx, value, '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl"))))))).
```
```prolog
:- success(always(call((to_lisp_pathname("", '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, []))), set_opv(xx_default_pathname_defaults_xx, value, '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, []))))))).
```
```prolog
:- success(always(call(notrace(grovel_math)))).
```
```prolog
:- success(always(lisp_compiled_eval('(defun floor (number &optional divisor)\n  "Return the greatest integer not greater than number, or number/divisor.\n  The second returned value is (mod number divisor)."\n  (if (null divisor)(setq divisor 1))\n  (multiple-value-bind (tru rem) (truncate number divisor)\n    (if (and (not (zerop rem))\n             (if (minusp divisor)\n               (plusp number)\n               (minusp number)))\n      (if (called-for-mv-p)\n        (values (1- tru) (+ rem divisor))\n        (1- tru))\n      (values tru rem))))',
                                     floor))).
```
```prolog
:- success(always(lisp_compiled_eval("(defparameter sys::*output-file-pathname* ())",
                                     sys_xx_output_file_pathname_xx))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog-trace', sys_prolog_trace))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:trace', trace))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog', sys_prolog))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:break', break))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog-call', sys_prolog_call))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog-inline', sys_prolog_inline))).
```
```prolog
:- success(always(lisp_compiled_eval("(defparameter sys::*compiler-mode* :execute)",
                                     sys_xx_compiler_mode_xx))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:get-iprops', sys_get_iprops))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:get-opv', sys_get_opv))).
```
```prolog
:- success(always(lisp_compiled_eval("(defparameter EXT:*ARGS* ())",
                                     ext_xx_args_xx))).
```
```prolog
:- success(always(call(set_program_args([])))).
```
```cl
__        ___    __  __        ____ _
\ \      / / \  |  \/  |      / ___| |
 \ \ /\ / / _ \ | |\/| |_____| |   | |
  \ V  V / ___ \| |  | |_____| |___| |___
   \_/\_/_/   \_\_|  |_|      \____|_____|

Common Lisp, written in Prolog
CL-USER> (LOAD "sanity-test")
```
```prolog
:- lisp_compiled_eval([load, '$ARRAY'([*], claz_base_character, "sanity-test")]).
## COMPILER:- cl_load('$ARRAY'([*], claz_base_character, "sanity-test"), [], Load_Ret).
## EXEC
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp'),
				  lisp_reader_compiled_eval)).
```
```common-lisp

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
```
```prolog
```
```common-lisp
 #+WAM-CL (prolog-call "cls.")
```
```prolog
```
```common-lisp
(defun mapcar-visualize (func l) (if (null l) () (cons (apply func (list (first l))) (mapcar func (rest l)))))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,'mapcar-visualize',[func,l],[if,[null,l],[],[cons,[apply,func,[list,[first,l]]],[mapcar,func,[rest,l]]]]]).
wl:lambda_def(defun, u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]).
wl:arglist_info(u_mapcar_visualize, [u_func, u_l], [Func_Param, L_Param], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_mapcar_visualize).

```

### Compiled:  `U::MAPCAR-VISUALIZE` 
```prolog
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
```
```common-lisp
(load "../prolog/wam_cl/wam-cl-init")

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[load,'$STRING'("../prolog/wam_cl/wam-cl-init")]).
:- cl_load('$ARRAY'([*], claz_base_character, "../prolog/wam_cl/wam-cl-init"),
	   [],
	   _IgnoredResult).
```
```common-lisp
(in-package "CL-USER")



;; Test macro
```
```prolog
:-lisp_compile_to_prolog(pkg_user,['in-package','$STRING'("CL-USER")]).
:- cl_in_package('$ARRAY'([*], claz_base_character, "CL-USER"), _IgnoredResult).
```
```common-lisp
; Test macro
```
```prolog
```
```common-lisp
(defmacro is (eqf expected actual)
  (let ((a (gensym "a")) (b (gensym "b")))
    `(let ((,a ,expected) (,b ,actual))
       (if (,eqf ,a ,b)
         (format t "OK: fmt90_x1 is fmt90_x2 to fmt90_x3"(defmacro is (eqf expected actual)\n  (let ((a (gensym \"a\")) (b (gensym \"b\")))\n    `(let ((,a ,expected) (,b ,actual))\n       (if (,eqf ,a ,b)\n         (format t \"OK: ~a is ~a to ~a~%\" ',expected ',eqf ',actual)\n         (progn\n           (format t \"FAILED: when matching ~a and ~a~%\" ,a ,b)\n\t   #+WAM-CL (prolog-inline \"trace\")\n\t   #+CLISP (BREAK)\n\t   #+CLISP (quit 1))\n         ))))\n\n\n".
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defmacro,is,[eqf,expected,actual],[let,[[a,[gensym,'$STRING'("a")]],[b,[gensym,'$STRING'("b")]]],['#BQ',[let,[[['#COMMA',a],['#COMMA',expected]],[['#COMMA',b],['#COMMA',actual]]],[if,[['#COMMA',eqf],['#COMMA',a],['#COMMA',b]],[format,t,'$STRING'("OK: ~a is ~a to ~a~%"),[quote,['#COMMA',expected]],[quote,['#COMMA',eqf]],[quote,['#COMMA',actual]]],[progn,[format,t,'$STRING'("FAILED: when matching ~a and ~a~%"),['#COMMA',a],['#COMMA',b]],['prolog-inline','$STRING'("trace")]]]]]]]).
wl:lambda_def(defmacro, u_is, f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]).
wl:arglist_info(u_is, [u_eqf, u_expected, u_actual], [Eqf_Param, Expected_Param, Actual_Param], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_is).

```

### Compiled:  `U::IS` 
```prolog
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
```
```common-lisp
(write-line "Running smoke test!")

; (progn (prolog-inline "rtrace") (is eq 1 1))
```
```prolog
:-lisp_compile_to_prolog(pkg_user,['write-line','$STRING'("Running smoke test!")]).
:- cl_write_line('$ARRAY'([*], claz_base_character, "Running smoke test!"),
		 _IgnoredResult).
```
```common-lisp
 (progn (prolog-inline "rtrace") (is eq 1 1))
```
```prolog
```
```common-lisp
(is eq 1 1)
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,1]).
% macroexpand:-[u_is,eq,1,1].
% into:-[let,[[a37,1],[b37,1]],[if,[eq,a37,b37],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,1]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a37,b37],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_21198=[[bv(a37,1),bv(b37,1)]|_19830],get_var(_21198,a37,_25952),get_var(_21198,b37,_26418),(is_eq(_25952,_26418)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,1],_26684),_20098=_26684;get_var(_21198,a37,_29322),get_var(_21198,b37,_30954),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_29322,_30954],_27802),trace,_20098=_27802).
:- LEnv=[[bv(a37, 1), bv(b37, 1)]|TLEnv],
   get_var(LEnv, a37, A37_Get),
   get_var(LEnv, b37, B37_Get),
   (   is_eq(A37_Get, B37_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   1
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a37, A37_Get12),
       get_var(LEnv, b37, B37_Get13),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A37_Get12,
		   B37_Get13
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is equal (list 1 'a 'b) (cons 1 '(a b)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,1,[quote,a],[quote,b]],[cons,1,[quote,[a,b]]]]).
% macroexpand:-[u_is,equal,[list,1,[quote,u_a],[quote,u_b]],[cons,1,[quote,[u_a,u_b]]]].
% into:-[let,[[a38,[list,1,[quote,u_a],[quote,u_b]]],[b38,[cons,1,[quote,[u_a,u_b]]]]],[if,[equal,a38,b38],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[list,1,[quote,u_a],[quote,u_b]]],[quote,equal],[quote,[cons,1,[quote,[u_a,u_b]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a38,b38],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_list([1,u_a,u_b],_22250),_22288=[1,u_a,u_b],_21912=[[bv(a38,_22250),bv(b38,_22288)]|_20094],get_var(_21912,a38,_30996),get_var(_21912,b38,_31462),(is_equal(_30996,_31462)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[list,1,[quote,u_a],[quote,u_b]],equal,[cons,1,[quote,[u_a,u_b]]]],_31998),_20632=_31998;get_var(_21912,a38,_34906),get_var(_21912,b38,_36538),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_34906,_36538],_33386),trace,_20632=_33386).
:- A38_Init=[1, u_a, u_b],
   B38_Init=[1, u_a, u_b],
   LEnv=[[bv(a38, A38_Init), bv(b38, B38_Init)]|TLEnv],
   get_var(LEnv, a38, A38_Get),
   get_var(LEnv, b38, B38_Get),
   (   is_equal(A38_Get, B38_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [list, 1, [quote, u_a], [quote, u_b]],
		   equal,
		   [cons, 1, [quote, [u_a, u_b]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a38, A38_Get14),
       get_var(LEnv, b38, B38_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A38_Get14,
		   B38_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq 2 (if nil 1 2))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]]).
% macroexpand:-[u_is,eq,2,[if,[],1,2]].
% into:-[let,[[a39,2],[b39,[if,[],1,2]]],[if,[eq,a39,b39],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,2],[quote,eq],[quote,[if,[],1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a39,b39],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-([]\==[]->_21870=1;_21870=2),_21520=[[bv(a39,2),bv(b39,_21870)]|_20034],get_var(_21520,a39,_29064),get_var(_21520,b39,_29530),(is_eq(_29064,_29530)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),2,eq,[if,[],1,2]],_29868),_20372=_29868;get_var(_21520,a39,_32578),get_var(_21520,b39,_34210),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32578,_34210],_31058),trace,_20372=_31058).
:- (   []\==[]
   ->  B39_Init=1
   ;   B39_Init=2
   ),
   LEnv=[[bv(a39, 2), bv(b39, B39_Init)]|TLEnv],
   get_var(LEnv, a39, A39_Get),
   get_var(LEnv, b39, B39_Get),
   (   is_eq(A39_Get, B39_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   2,
		   eq,
		   [if, [], 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a39, A39_Get15),
       get_var(LEnv, b39, B39_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A39_Get15,
		   B39_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq t (keywordp :k))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[keywordp,':k']]).
% macroexpand:-[u_is,eq,t,[keywordp,kw_k]].
% into:-[let,[[a310,t],[b310,[keywordp,kw_k]]],[if,[eq,a310,b310],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[keywordp,kw_k]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a310,b310],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_keywordp(kw_k,_21864),_21514=[[bv(a310,t),bv(b310,_21864)]|_20086],get_var(_21514,a310,_29040),get_var(_21514,b310,_29506),(is_eq(_29040,_29506)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[keywordp,kw_k]],_29808),_20390=_29808;get_var(_21514,a310,_32524),get_var(_21514,b310,_34198),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32524,_34198],_30962),trace,_20390=_30962).
:- cl_keywordp(kw_k, B310_Init),
   LEnv=[[bv(a310, t), bv(b310, B310_Init)]|TLEnv],
   get_var(LEnv, a310, A310_Get),
   get_var(LEnv, b310, B310_Get),
   (   is_eq(A310_Get, B310_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [keywordp, kw_k]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a310, A310_Get13),
       get_var(LEnv, b310, B310_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A310_Get13,
		   B310_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq 10 (if t 10 20))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,10,[if,t,10,20]]).
% macroexpand:-[u_is,eq,10,[if,t,10,20]].
% into:-[let,[[a401,10],[b401,[if,t,10,20]]],[if,[eq,a401,b401],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,10],[quote,eq],[quote,[if,t,10,20]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a401,b401],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-(t\==[]->_22036=10;_22036=20),_21686=[[bv(a401,10),bv(b401,_22036)]|_20186],get_var(_21686,a401,_29356),get_var(_21686,b401,_29822),(is_eq(_29356,_29822)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),10,eq,[if,t,10,20]],_30160),_20526=_30160;get_var(_21686,a401,_32912),get_var(_21686,b401,_34586),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32912,_34586],_31350),trace,_20526=_31350).
:- (   t\==[]
   ->  B401_Init=10
   ;   B401_Init=20
   ),
   LEnv=[[bv(a401, 10), bv(b401, B401_Init)]|TLEnv],
   get_var(LEnv, a401, A401_Get),
   get_var(LEnv, b401, B401_Get),
   (   is_eq(A401_Get, B401_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   10,
		   eq,
		   [if, t, 10, 20]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a401, A401_Get15),
       get_var(LEnv, b401, B401_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A401_Get15,
		   B401_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq t (stringp "abc"))

;;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2"(is eq t (stringp \"abc\"))\n\n;;  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).\n".
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[stringp,'$STRING'("abc")]]).
% macroexpand:-[u_is,eq,t,[stringp,'$ARRAY'([*],claz_base_character,"abc")]].
% into:-[let,[[a42,t],[b42,[stringp,'$ARRAY'([*],claz_base_character,"abc")]]],[if,[eq,a42,b42],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[stringp,'$ARRAY'([*],claz_base_character,"abc")]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a42,b42],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_stringp('$ARRAY'([*],claz_base_character,"abc"),_22152),_21802=[[bv(a42,t),bv(b42,_22152)]|_20330],get_var(_21802,a42,_29202),get_var(_21802,b42,_29668),(is_eq(_29202,_29668)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[stringp,'$ARRAY'([*],claz_base_character,"abc")]],_29970),_20678=_29970;get_var(_21802,a42,_32644),get_var(_21802,b42,_34276),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32644,_34276],_31124),trace,_20678=_31124).
:- cl_stringp('$ARRAY'([*], claz_base_character, "abc"), B42_Init),
   LEnv=[[bv(a42, t), bv(b42, B42_Init)]|TLEnv],
   get_var(LEnv, a42, A42_Get),
   get_var(LEnv, b42, B42_Get),
   (   is_eq(A42_Get, B42_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [stringp, '$ARRAY'([*], claz_base_character, "abc")]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a42, A42_Get13),
       get_var(LEnv, b42, B42_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A42_Get13,
		   B42_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).".
```
```prolog
```
```common-lisp
(is equal (subseq "abc" 1) "bc")

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,equal,[subseq,'$STRING'("abc"),1],'$STRING'("bc")]).
% macroexpand:-[u_is,equal,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1],'$ARRAY'([*],claz_base_character,"bc")].
% into:-[let,[[a43,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1]],[b43,'$ARRAY'([*],claz_base_character,"bc")]],[if,[equal,a43,b43],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1]],[quote,equal],[quote,'$ARRAY'([*],claz_base_character,"bc")]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a43,b43],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_subseq('$ARRAY'([*],claz_base_character,"abc"),1,_22310),_21972=[[bv(a43,_22310),bv(b43,'$ARRAY'([*],claz_base_character,"bc"))]|_20438],get_var(_21972,a43,_29682),get_var(_21972,b43,_30148),(is_equal(_29682,_30148)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[subseq,'$ARRAY'([*],claz_base_character,"abc"),1],equal,'$ARRAY'([*],claz_base_character,"bc")],_30468),_20836=_30468;get_var(_21972,a43,_33160),get_var(_21972,b43,_34792),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33160,_34792],_31640),trace,_20836=_31640).
:- cl_subseq('$ARRAY'([*], claz_base_character, "abc"), 1, A43_Init),
   LEnv=[[bv(a43, A43_Init), bv(b43, '$ARRAY'([*], claz_base_character, "bc"))]|TLEnv],
   get_var(LEnv, a43, A43_Get),
   get_var(LEnv, b43, B43_Get),
   (   is_equal(A43_Get, B43_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [subseq, '$ARRAY'([*], claz_base_character, "abc"), 1],
		   equal,
		   '$ARRAY'([*], claz_base_character, "bc")
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a43, A43_Get13),
       get_var(LEnv, b43, B43_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A43_Get13,
		   B43_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq 1 (if t 1 2))
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[if,t,1,2]]).
% macroexpand:-[u_is,eq,1,[if,t,1,2]].
% into:-[let,[[a44,1],[b44,[if,t,1,2]]],[if,[eq,a44,b44],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,[if,t,1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a44,b44],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-(t\==[]->_22262=1;_22262=2),_21912=[[bv(a44,1),bv(b44,_22262)]|_20424],get_var(_21912,a44,_29456),get_var(_21912,b44,_29922),(is_eq(_29456,_29922)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,[if,t,1,2]],_30260),_20764=_30260;get_var(_21912,a44,_32970),get_var(_21912,b44,_34602),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32970,_34602],_31450),trace,_20764=_31450).
:- (   t\==[]
   ->  B44_Init=1
   ;   B44_Init=2
   ),
   LEnv=[[bv(a44, 1), bv(b44, B44_Init)]|TLEnv],
   get_var(LEnv, a44, A44_Get),
   get_var(LEnv, b44, B44_Get),
   (   is_eq(A44_Get, B44_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   [if, t, 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a44, A44_Get15),
       get_var(LEnv, b44, B44_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A44_Get15,
		   B44_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq 2 (if nil 1 2))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,2,[if,[],1,2]]).
% macroexpand:-[u_is,eq,2,[if,[],1,2]].
% into:-[let,[[a45,2],[b45,[if,[],1,2]]],[if,[eq,a45,b45],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,2],[quote,eq],[quote,[if,[],1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a45,b45],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-([]\==[]->_22340=1;_22340=2),_21990=[[bv(a45,2),bv(b45,_22340)]|_20502],get_var(_21990,a45,_29534),get_var(_21990,b45,_30000),(is_eq(_29534,_30000)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),2,eq,[if,[],1,2]],_30338),_20842=_30338;get_var(_21990,a45,_33048),get_var(_21990,b45,_34680),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33048,_34680],_31528),trace,_20842=_31528).
:- (   []\==[]
   ->  B45_Init=1
   ;   B45_Init=2
   ),
   LEnv=[[bv(a45, 2), bv(b45, B45_Init)]|TLEnv],
   get_var(LEnv, a45, A45_Get),
   get_var(LEnv, b45, B45_Get),
   (   is_eq(A45_Get, B45_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   2,
		   eq,
		   [if, [], 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a45, A45_Get15),
       get_var(LEnv, b45, B45_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A45_Get15,
		   B45_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,fib,[n],[if,[>,n,1],[+,[fib,[-,n,1]],[fib,[-,n,2]]],1]]).
wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).
wl:arglist_info(u_fib, [n], [N_Param], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_fib).

```

### Compiled:  `U::FIB` 
```prolog
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
```
```common-lisp
(disassemble #'fib)


```
```prolog
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fib)]).
:- cl_disassemble(function(u_fib), _IgnoredResult).
```
```common-lisp
(is eql 89 (fib 10))



```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eql,89,[fib,10]]).
% macroexpand:-[u_is,eql,89,[u_fib,10]].
% into:-[let,[[a46,89],[b46,[u_fib,10]]],[if,[eql,a46,b46],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,89],[quote,eql],[quote,[u_fib,10]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a46,b46],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_fib(10,_22330),_21980=[[bv(a46,89),bv(b46,_22330)]|_20554],get_var(_21980,a46,_29392),get_var(_21980,b46,_29858),(is_eql(_29392,_29858)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),89,eql,[u_fib,10]],_30160),_20856=_30160;get_var(_21980,a46,_32834),get_var(_21980,b46,_34466),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32834,_34466],_31314),trace,_20856=_31314).
:- f_u_fib(10, B46_Init),
   LEnv=[[bv(a46, 89), bv(b46, B46_Init)]|TLEnv],
   get_var(LEnv, a46, A46_Get),
   get_var(LEnv, b46, B46_Get),
   (   is_eql(A46_Get, B46_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   89,
		   eql,
		   [u_fib, 10]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a46, A46_Get13),
       get_var(LEnv, b46, B46_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A46_Get13,
		   B46_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(defun accum (r) (if (= 0 r) (list 0) (cons r (accum (- r 1)))))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,accum,[r],[if,[=,0,r],[list,0],[cons,r,[accum,[-,r,1]]]]]).
wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).
wl:arglist_info(u_accum, [u_r], [R_Param], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_accum).

```

### Compiled:  `U::ACCUM` 
```prolog
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
```
```common-lisp
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
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(accum)]).
:- cl_disassemble(function(u_accum), _IgnoredResult).
```
```common-lisp
 DISASSEMBLY FOR:f_u_accum
:- dynamic f_u_accum/2.

f_u_accum(A, G) :-
	(   0=:=A
	->  G=[0]
	;   C is A - 1,
	    f_u_accum(C, D),
	    G=[A|D]
	).

```
```prolog
```
```common-lisp
(is equal (list 4 3 2 1 0) (accum 4))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,equal,[list,4,3,2,1,0],[accum,4]]).
% macroexpand:-[u_is,equal,[list,4,3,2,1,0],[u_accum,4]].
% into:-[let,[[a47,[list,4,3,2,1,0]],[b47,[u_accum,4]]],[if,[equal,a47,b47],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[list,4,3,2,1,0]],[quote,equal],[quote,[u_accum,4]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a47,b47],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_list([4,3,2,1,0],_22652),f_u_accum(4,_22702),_22314=[[bv(a47,_22652),bv(b47,_22702)]|_20706],get_var(_22314,a47,_31140),get_var(_22314,b47,_31606),(is_equal(_31140,_31606)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[list,4,3,2,1,0],equal,[u_accum,4]],_32016),_21118=_32016;get_var(_22314,a47,_34798),get_var(_22314,b47,_36430),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_34798,_36430],_33278),trace,_21118=_33278).
:- A47_Init=[4, 3, 2, 1, 0],
   f_u_accum(4, B47_Init),
   LEnv=[[bv(a47, A47_Init), bv(b47, B47_Init)]|TLEnv],
   get_var(LEnv, a47, A47_Get),
   get_var(LEnv, b47, B47_Get),
   (   is_equal(A47_Get, B47_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [list, 4, 3, 2, 1, 0],
		   equal,
		   [u_accum, 4]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a47, A47_Get14),
       get_var(LEnv, b47, B47_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A47_Get14,
		   B47_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(defmacro defwrap (name) `(defun ,name () 1))
;;; :- ensure_loaded('sanity-test.lisp.trans.pl').
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defmacro,defwrap,[name],['#BQ',[defun,['#COMMA',name],[],1]]]).
wl:lambda_def(defmacro, u_defwrap, f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]).
wl:arglist_info(u_defwrap, [sys_name], [Name_Param], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_defwrap).

```

### Compiled:  `U::DEFWRAP` 
```prolog
f_u_defwrap(Name_Param, FnResult) :-
	[defun, Name_Param, [], 1]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_defwrap, classof, claz_macro),
   set_opv(u_defwrap, compile_as, kw_operator),
   set_opv(u_defwrap, function, f_u_defwrap),
   DefMacroResult=u_defwrap.
```
```common-lisp
;; :- ensure_loaded('sanity-test.lisp.trans.pl').
```
```prolog
```
```common-lisp
(defwrap foo)
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defwrap,foo]).
% macroexpand:-[u_defwrap,u_foo].
% into:-[defun,u_foo,[],1].
% code:-assert_lsp(u_foo,wl:lambda_def(defun,u_foo,f_u_foo,[],[1])),assert_lsp(u_foo,wl:arglist_info(u_foo,[],[],arginfo{all:0,allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[],opt:0,req:0,rest:0,sublists:0,whole:0})),!,assert_lsp(u_foo,wl:init_args(exact_only,u_foo)),assert_lsp(u_foo,(f_u_foo(_50554):-_45778=[],1=_50554)),set_opv(f_u_foo,classof,claz_function),set_opv(u_foo,compile_as,kw_function),set_opv(u_foo,function,f_u_foo),_26758=u_foo.
wl:lambda_def(defun, u_foo, f_u_foo, [], [1]).
wl:arglist_info(u_foo, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_foo).

```

### Compiled:  `U::FOO` 
```prolog
f_u_foo(FnResult) :-
	Env=[],
	1=FnResult.
:- set_opv(f_u_foo, classof, claz_function),
   set_opv(u_foo, compile_as, kw_function),
   set_opv(u_foo, function, f_u_foo),
   DefunResult=u_foo.
```
```common-lisp
(is eq 1 (foo))
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,1,[foo]]).
% macroexpand:-[u_is,eq,1,[u_foo]].
% into:-[let,[[a48,1],[b48,[u_foo]]],[if,[eq,a48,b48],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,[u_foo]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a48,b48],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_foo(_22440),_22090=[[bv(a48,1),bv(b48,_22440)]|_20692],get_var(_22090,a48,_29452),get_var(_22090,b48,_29918),(is_eq(_29452,_29918)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,[u_foo]],_30202),_20978=_30202;get_var(_22090,a48,_32858),get_var(_22090,b48,_34490),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32858,_34490],_31338),trace,_20978=_31338).
:- f_u_foo(B48_Init),
   LEnv=[[bv(a48, 1), bv(b48, B48_Init)]|TLEnv],
   get_var(LEnv, a48, A48_Get),
   get_var(LEnv, b48, B48_Get),
   (   is_eq(A48_Get, B48_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   [u_foo]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a48, A48_Get13),
       get_var(LEnv, b48, B48_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A48_Get13,
		   B48_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is equal (macroexpand-1 '(defwrap foo)) '(defun foo nil 1))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,equal,['macroexpand-1',[quote,[defwrap,foo]]],[quote,[defun,foo,[],1]]]).
% macroexpand:-[u_is,equal,[macroexpand_1,[quote,[u_defwrap,u_foo]]],[quote,[defun,u_foo,[],1]]].
% into:-[let,[[a49,[macroexpand_1,[quote,[u_defwrap,u_foo]]]],[b49,[quote,[defun,u_foo,[],1]]]],[if,[equal,a49,b49],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[macroexpand_1,[quote,[u_defwrap,u_foo]]]],[quote,equal],[quote,[quote,[defun,u_foo,[],1]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a49,b49],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_macroexpand_1([[u_defwrap,u_foo]],_22976),_22638=[[bv(a49,_22976),bv(b49,[defun,u_foo,[],1])]|_20910],get_var(_22638,a49,_30712),get_var(_22638,b49,_31178),(is_equal(_30712,_31178)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[macroexpand_1,[quote,[u_defwrap,u_foo]]],equal,[quote,[defun,u_foo,[],1]]],_31660),_21394=_31660;get_var(_22638,a49,_34514),get_var(_22638,b49,_36146),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_34514,_36146],_32994),trace,_21394=_32994).
:- cl_macroexpand_1([[u_defwrap, u_foo]], A49_Init),
   LEnv=[[bv(a49, A49_Init), bv(b49, [defun, u_foo, [], 1])]|TLEnv],
   get_var(LEnv, a49, A49_Get),
   get_var(LEnv, b49, B49_Get),
   (   is_equal(A49_Get, B49_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [macroexpand_1, [quote, [u_defwrap, u_foo]]],
		   equal,
		   [quote, [defun, u_foo, [], 1]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a49, A49_Get13),
       get_var(LEnv, b49, B49_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A49_Get13,
		   B49_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(write-line "PASSED")

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['write-line','$STRING'("PASSED")]).
:- cl_write_line('$ARRAY'([*], claz_base_character, "PASSED"), _IgnoredResult).
```
```common-lisp
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

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,fifteen,[],[let,[val],[tagbody,[setq,val,1],[go,'point-a'],[incf,val,16],'point-c',[incf,val,4],[go,'point-b'],[incf,val,32],'point-a','point-u',[incf,val,2],[go,'point-c'],[incf,val,64],'point-b',[incf,val,8]],val]]).
% macroexpand:-[incf,u_val,4].
% into:-[setf,u_val,[+,u_val,4]].
% code:-get_var(_63446,u_val,_60306),+(_60306,4,_60082),set_place(_63446,setf,[value,u_val],[_60082],_35698).
% macroexpand:-[incf,u_val,2].
% into:-[setf,u_val,[+,u_val,2]].
% code:-get_var(_27544,u_val,_26930),+(_26930,2,_42540),set_place(_27544,setf,[value,u_val],[_42540],_28544).
% macroexpand:-[incf,u_val,2].
% into:-[setf,u_val,[+,u_val,2]].
% code:-get_var(_27680,u_val,_27060),+(_27060,2,_42676),set_place(_27680,setf,[value,u_val],[_42676],_28680).
% macroexpand:-[incf,u_val,8].
% into:-[setf,u_val,[+,u_val,8]].
% code:-get_var(_27940,u_val,_27314),+(_27314,8,_42936),set_place(_27940,setf,[value,u_val],[_42936],_28940).
wl:lambda_def(defun, u_fifteen, f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]).
wl:arglist_info(u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_fifteen).

```

### Compiled:  `U::FIFTEEN` 
```prolog
f_u_fifteen(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_val, [])]|Env],
		  call_addr_block(GoEnv,
				  (set_var(GoEnv, setq, u_val, 1), goto(u_point_a, GoEnv)),
				  
				  [ addr(addr_tagbody_7_u_point_c,
					 u_point_c,
					 '$used',
					 Setf_Env,
					 (get_var(Setf_Env, u_val, Get_var_Ret), +(Get_var_Ret, 4, CAR49), set_place(Setf_Env, setf, [value, u_val], [CAR49], Set_place_Ret), goto(u_point_b, Setf_Env))),
				    addr(addr_tagbody_7_u_point_a,
					 u_point_a,
					 '$used',
					 Setf_Env,
					 (push_label(u_point_u), get_var(Setf_Env, u_val, Val_Get21), +(Val_Get21, 2, CAR25), set_place(Setf_Env, setf, [value, u_val], [CAR25], Setf_R23), goto(u_point_c, Setf_Env))),
				    addr(addr_tagbody_7_u_point_u,
					 u_point_u,
					 '$unused',
					 Setf_Env,
					 (get_var(Setf_Env, u_val, Val_Get28), +(Val_Get28, 2, CAR32), set_place(Setf_Env, setf, [value, u_val], [CAR32], Setf_R30), goto(u_point_c, Setf_Env))),
				    addr(addr_tagbody_7_u_point_b,
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
```
```common-lisp
; unused
```
```prolog
```
```common-lisp
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

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[disassemble,function(fifteen)]).
:- cl_disassemble(function(u_fifteen), _IgnoredResult).
```
```common-lisp


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

```
```prolog
```
```common-lisp
(is eq 15 (fifteen))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,15,[fifteen]]).
% macroexpand:-[u_is,eq,15,[u_fifteen]].
% into:-[let,[[a410,15],[b410,[u_fifteen]]],[if,[eq,a410,b410],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,15],[quote,eq],[quote,[u_fifteen]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a410,b410],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_fifteen(_22658),_22308=[[bv(a410,15),bv(b410,_22658)]|_20910],get_var(_22308,a410,_29796),get_var(_22308,b410,_30262),(is_eq(_29796,_30262)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),15,eq,[u_fifteen]],_30546),_21196=_30546;get_var(_22308,a410,_33244),get_var(_22308,b410,_34918),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33244,_34918],_31682),trace,_21196=_31682).
:- f_u_fifteen(B410_Init),
   LEnv=[[bv(a410, 15), bv(b410, B410_Init)]|TLEnv],
   get_var(LEnv, a410, A410_Get),
   get_var(LEnv, b410, B410_Get),
   (   is_eq(A410_Get, B410_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   15,
		   eq,
		   [u_fifteen]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a410, A410_Get13),
       get_var(LEnv, b410, B410_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A410_Get13,
		   B410_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(defun do-four () (DO ((temp-one 1 (1+ temp-one) )(temp-two 0 (1- temp-two) ) )((> (- temp-one temp-two) 5) temp-one)() ))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,'do-four',[],['DO',[['temp-one',1,['1+','temp-one']],['temp-two',0,['1-','temp-two']]],[[>,[-,'temp-one','temp-two'],5],'temp-one'],[]]]).
wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]).
wl:arglist_info(u_do_four, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_do_four).

```

### Compiled:  `U::DO-FOUR` 
```prolog
f_u_do_four(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_temp_one, 1), bv(u_temp_two, 0)]|Env],
		  catch(( call_addr_block(GoEnv,
					  (push_label(do_label_4), get_var(GoEnv, u_temp_one, Temp_one_Get31), get_var(GoEnv, u_temp_two, Temp_two_Get32), -(Temp_one_Get31, Temp_two_Get32, PredArg1Result34), (PredArg1Result34>5->throw(block_exit([], Temp_one_Get21)), _TBResult=ThrowResult36;get_var(GoEnv, u_temp_one, Temp_one_Get39), '1+'(Temp_one_Get39, Temp_one), get_var(GoEnv, u_temp_two, Temp_two_Get40), '1-'(Temp_two_Get40, Temp_two), set_var(GoEnv, u_temp_one, Temp_one), set_var(GoEnv, u_temp_two, Temp_two), goto(do_label_4, GoEnv), _TBResult=_GORES41)),
					  
					  [ addr(addr_tagbody_8_do_label_4,
						 do_label_4,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, u_temp_one, Temp_one_Get21), get_var(BlockExitEnv, u_temp_two, Temp_two_Get24), -(Temp_one_Get21, Temp_two_Get24, _22326), (_22326>5->throw(block_exit([], Temp_one_Get21)), _22328=ThrowResult;'1+'(Temp_one_Get21, Set_var_Ret), '1-'(Temp_two_Get24, Set_var_Ret51), set_var(BlockExitEnv, u_temp_one, Set_var_Ret), set_var(BlockExitEnv, u_temp_two, Set_var_Ret51), goto(do_label_4, BlockExitEnv), _22328=_GORES)))
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
```
```common-lisp
(is = 4  (do-four))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,=,4,['do-four']]).
% macroexpand:-[u_is,=,4,[u_do_four]].
% into:-[let,[[a501,4],[b501,[u_do_four]]],[if,[=,a501,b501],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,4],[quote,=],[quote,[u_do_four]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a501,b501],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_do_four(_22774),_22424=[[bv(a501,4),bv(b501,_22774)]|_21014],get_var(_22424,a501,_29758),get_var(_22424,b501,_30224),(_29758=:=_30224->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),4,=,[u_do_four]],_30508),_21300=_30508;get_var(_22424,a501,_33206),get_var(_22424,b501,_34880),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33206,_34880],_31644),trace,_21300=_31644).
:- f_u_do_four(B501_Init),
   LEnv=[[bv(a501, 4), bv(b501, B501_Init)]|TLEnv],
   get_var(LEnv, a501, A501_Get),
   get_var(LEnv, b501, B501_Get),
   (   A501_Get=:=B501_Get
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   4,
		   (=),
		   [u_do_four]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a501, A501_Get13),
       get_var(LEnv, b501, B501_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A501_Get13,
		   B501_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq 'string_l (DEFUN string_l (x )(COND ((STRINGP x )x )((SYMBOLP x )(symbol-name x ))(T (ERROR "type error" )))))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,string_l],['DEFUN',string_l,[x],['COND',[['STRINGP',x],x],[['SYMBOLP',x],['symbol-name',x]],['T',['ERROR','$STRING'("type error")]]]]]).
% macroexpand:-[u_is,eq,[quote,u_string_l],[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]].
% into:-[let,[[a52,[quote,u_string_l]],[b52,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]]],[if,[eq,a52,b52],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_string_l]],[quote,eq],[quote,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a52,b52],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-assert_lsp(u_string_l,wl:lambda_def(defun,u_string_l,f_u_string_l,[u_x],[[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]])),assert_lsp(u_string_l,wl:arglist_info(u_string_l,[u_x],[_22682],arginfo{all:[u_x],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[u_x],opt:0,req:[u_x],rest:0,sublists:0,whole:0})),!,assert_lsp(u_string_l,wl:init_args(exact_only,u_string_l)),assert_lsp(u_string_l,(f_u_string_l(_22682,_23002):-is_stringp(_22682)->_23002=_22682;is_symbolp(_22682)->cl_symbol_name(_22682,_22892),_23002=_22892;cl_error(['$ARRAY'([*],claz_base_character,"type error")],_22948),_23002=_22948)),set_opv(f_u_string_l,classof,claz_function),set_opv(u_string_l,compile_as,kw_function),set_opv(u_string_l,function,f_u_string_l),_23704=u_string_l,_22434=[[bv(a52,u_string_l),bv(b52,_23704)]|_21478],get_var(_22434,a52,_24018),get_var(_22434,b52,_24046),(is_eq(_24018,_24046)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_string_l],eq,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]],_24170),_22308=_24170;get_var(_22434,a52,_24080),get_var(_22434,b52,_24126),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_24080,_24126],_24198),trace,_22308=_24198).
wl:lambda_def(defun, u_string_l, f_u_string_l, [u_x], [[cond, [[stringp, u_x], u_x], [[symbolp, u_x], [symbol_name, u_x]], [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]]]).
wl:arglist_info(u_string_l, [u_x], [X_Get23], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_string_l).

```

### Compiled:  `U::STRING_L` 
```prolog
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
   LEnv=[[bv(a52, u_string_l), bv(b52, DefunResult)]|TLEnv],
   get_var(LEnv, a52, A52_Get),
   get_var(LEnv, b52, B52_Get),
   (   is_eq(A52_Get, B52_Get)
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
   ;   get_var(LEnv, a52, A52_Get37),
       get_var(LEnv, b52, B52_Get38),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A52_Get37,
		   B52_Get38
		 ],
		 ElseResult40),
       trace,
       LetResult=ElseResult40
   ).
```
```common-lisp
(is eq () (TAGBODY 1 (PRINT "hi" )))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',1,['PRINT','$STRING'("hi")]]]).
% macroexpand:-[u_is,eq,[],[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]].
% into:-[let,[[a53,[]],[b53,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[if,[eq,a53,b53],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a53,b53],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-call_addr_block(_21210,(push_label(1),cl_print('$ARRAY'([*],claz_base_character,"hi"),_21756)),[addr(addr_tagbody_9_1,1,'$unused',_21882,cl_print('$ARRAY'([*],claz_base_character,"hi"),_21890))]),_21726=[[bv(a53,[]),bv(b53,[])]|_21210],get_var(_21726,a53,_22198),get_var(_21726,b53,_22226),(is_eq(_22198,_22226)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_22240),_21600=_22240;get_var(_21726,a53,_22260),get_var(_21726,b53,_22306),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_22260,_22306],_22242),trace,_21600=_22242).
:-call_addr_block(_21210,(push_label(1),cl_print('$ARRAY'([*],claz_base_character,"hi"),_21334)),[addr(addr_tagbody_9_1,1,'$unused',_21362,cl_print('$ARRAY'([*],claz_base_character,"hi"),_21364))]),_21306=[[bv(a53,[]),bv(b53,[])]|_21210],get_var(_21306,a53,_21586),get_var(_21306,b53,_21612),(is_eq(_21586,_21612)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_21624),_21194=_21624;get_var(_21306,a53,_21644),get_var(_21306,b53,_21684),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_21644,_21684],_21626),trace,_21194=_21626).
```
```common-lisp
(is eq () (TAGBODY a (PRINT "hi" )))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['TAGBODY',a,['PRINT','$STRING'("hi")]]]).
% macroexpand:-[u_is,eq,[],[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]].
% into:-[let,[[a54,[]],[b54,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[if,[eq,a54,b54],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a54,b54],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-call_addr_block(_21286,(push_label(u_a),cl_print('$ARRAY'([*],claz_base_character,"hi"),_21832)),[addr(addr_tagbody_10_u_a,u_a,'$unused',_21958,cl_print('$ARRAY'([*],claz_base_character,"hi"),_21966))]),_21802=[[bv(a54,[]),bv(b54,[])]|_21286],get_var(_21802,a54,_22274),get_var(_21802,b54,_22302),(is_eq(_22274,_22302)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_22316),_21676=_22316;get_var(_21802,a54,_22336),get_var(_21802,b54,_22382),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_22336,_22382],_22318),trace,_21676=_22318).
:-call_addr_block(_21286,(push_label(u_a),cl_print('$ARRAY'([*],claz_base_character,"hi"),_21410)),[addr(addr_tagbody_10_u_a,u_a,'$unused',_21438,cl_print('$ARRAY'([*],claz_base_character,"hi"),_21440))]),_21382=[[bv(a54,[]),bv(b54,[])]|_21286],get_var(_21382,a54,_21662),get_var(_21382,b54,_21688),(is_eq(_21662,_21688)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_21700),_21270=_21700;get_var(_21382,a54,_21720),get_var(_21382,b54,_21760),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_21720,_21760],_21702),trace,_21270=_21702).
```
```common-lisp
(is eq () (LET ((val 1 ))NIL ))
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]],[]]]).
% macroexpand:-[u_is,eq,[],[let,[[u_val,1]],[]]].
% into:-[let,[[a55,[]],[b55,[let,[[u_val,1]],[]]]],[if,[eq,a55,b55],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[let,[[u_val,1]],[]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a55,b55],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_23040=[[bv(u_val,1)]|_21354],_22902=[[bv(a55,[]),bv(b55,[])]|_21354],get_var(_22902,a55,_27842),get_var(_22902,b55,_28308),(is_eq(_27842,_28308)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[let,[[u_val,1]],[]]],_28682),_21730=_28682;get_var(_22902,a55,_31428),get_var(_22902,b55,_33060),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_31428,_33060],_29908),trace,_21730=_29908).
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   LEnv=[[bv(a55, []), bv(b55, [])]|TLEnv],
   get_var(LEnv, a55, A55_Get),
   get_var(LEnv, b55, B55_Get),
   (   is_eq(A55_Get, B55_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [let, [[u_val, 1]], []]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a55, A55_Get15),
       get_var(LEnv, b55, B55_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A55_Get15,
		   B55_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq () (LET ((val 1 )) ))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],['LET',[[val,1]]]]).
% macroexpand:-[u_is,eq,[],[let,[[u_val,1]]]].
% into:-[let,[[a56,[]],[b56,[let,[[u_val,1]]]]],[if,[eq,a56,b56],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[let,[[u_val,1]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a56,b56],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_23048=[[bv(u_val,1)]|_21392],_22910=[[bv(a56,[]),bv(b56,[])]|_21392],get_var(_22910,a56,_27838),get_var(_22910,b56,_28304),(is_eq(_27838,_28304)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[let,[[u_val,1]]]],_28660),_21750=_28660;get_var(_22910,a56,_31388),get_var(_22910,b56,_33020),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_31388,_33020],_29868),trace,_21750=_29868).
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   LEnv=[[bv(a56, []), bv(b56, [])]|TLEnv],
   get_var(LEnv, a56, A56_Get),
   get_var(LEnv, b56, B56_Get),
   (   is_eq(A56_Get, B56_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [let, [[u_val, 1]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a56, A56_Get15),
       get_var(LEnv, b56, B56_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A56_Get15,
		   B56_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eql 1 (LET ((val 1 ))val ))
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eql,1,['LET',[[val,1]],val]]).
% macroexpand:-[u_is,eql,1,[let,[[u_val,1]],u_val]].
% into:-[let,[[a57,1],[b57,[let,[[u_val,1]],u_val]]],[if,[eql,a57,b57],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eql],[quote,[let,[[u_val,1]],u_val]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a57,b57],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_23698=[[bv(u_val,1)]|_21482],get_var(_23698,u_val,_23726),_23030=[[bv(a57,1),bv(b57,_23726)]|_21482],get_var(_23030,a57,_37864),get_var(_23030,b57,_38330),(is_eql(_37864,_38330)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eql,[let,[[u_val,1]],u_val]],_38704),_21858=_38704;get_var(_23030,a57,_41450),get_var(_23030,b57,_43082),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_41450,_43082],_39930),trace,_21858=_39930).
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   get_var(LEnv6, u_val, Val_Get),
   LEnv=[[bv(a57, 1), bv(b57, Val_Get)]|TLEnv],
   get_var(LEnv, a57, A57_Get),
   get_var(LEnv, b57, B57_Get),
   (   is_eql(A57_Get, B57_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eql,
		   [let, [[u_val, 1]], u_val]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a57, A57_Get17),
       get_var(LEnv, b57, B57_Get18),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A57_Get17,
		   B57_Get18
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eql 'world (LET ((a 'b) )(LET ((a 'world) )
  (LET ((a 'hello) )(LET ((a a)(*package* (find-package :keyword) ) )(PRINT a) ) )(PRINT a) ) ))


;; 3.1. Review of defstruct

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eql,[quote,world],['LET',[[a,[quote,b]]],['LET',[[a,[quote,world]]],['LET',[[a,[quote,hello]]],['LET',[[a,a],['*package*',['find-package',':keyword']]],['PRINT',a]]],['PRINT',a]]]]).
% macroexpand:-[u_is,eql,[quote,u_world],[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]].
% into:-[let,[[a58,[quote,u_world]],[b58,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]]],[if,[eql,a58,b58],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_world]],[quote,eql],[quote,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a58,b58],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_25018=[[bv(u_a,u_b)]|_22026],_25092=[[bv(u_a,u_world)]|_25018],_25154=[[bv(u_a,u_hello)]|_25092],get_var(_25154,u_a,_25388),cl_find_package(kw_keyword,_25366),_25216=[[bv(u_a,_25388)]|_25154],save_special(sv(xx_package_xx,_25366,value,_25424)),get_var(_25216,u_a,_25442),cl_print(_25442,_25182),restore_special(sv(xx_package_xx,_25366,value,_25424)),get_var(_25092,u_a,_25526),cl_print(_25526,_25046),_24648=[[bv(a58,u_world),bv(b58,_25046)]|_22026],get_var(_24648,a58,_34480),get_var(_24648,b58,_34946),(is_eql(_34480,_34946)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_world],eql,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]],_35968),_23048=_35968;get_var(_24648,a58,_39362),get_var(_24648,b58,_40994),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_39362,_40994],_37842),trace,_23048=_37842).
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
   LEnv=[[bv(a58, u_world), bv(b58, LetResult9)]|TLEnv],
   get_var(LEnv, a58, A58_Get),
   get_var(LEnv, b58, B58_Get),
   (   is_eql(A58_Get, B58_Get)
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
   ;   get_var(LEnv, a58, A58_Get30),
       get_var(LEnv, b58, B58_Get31),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A58_Get30,
		   B58_Get31
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
; 3.1. Review of defstruct
```
```prolog
```
```common-lisp
(progn #+WAM-CL (prolog-inline "nop(trace)")(is eq 'point (defstruct point x y z)))
;; (defstruct point x y z)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[progn,['prolog-inline','$STRING'("nop(trace)")],[is,eq,[quote,point],[defstruct,point,x,y,z]]]).
% macroexpand:-[u_is,eq,[quote,u_point],[defstruct,u_point,u_x,u_y,u_z]].
% into:-[let,[[a59,[quote,u_point]],[b59,[defstruct,u_point,u_x,u_y,u_z]]],[if,[eq,a59,b59],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[defstruct,u_point,u_x,u_y,u_z]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a59,b59],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_defstruct([u_point,u_x,u_y,u_z],_24408),_24058=[[bv(a59,u_point),bv(b59,_24408)]|_21870],get_var(_24058,a59,_31662),get_var(_24058,b59,_32128),(is_eq(_31662,_32128)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[defstruct,u_point,u_x,u_y,u_z]],_32520),_22874=_32520;get_var(_24058,a59,_35284),get_var(_24058,b59,_36916),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_35284,_36916],_33764),trace,_22874=_33764).
:- nop(trace),
   cl_defstruct([u_point, u_x, u_y, u_z], B59_Init),
   LEnv=[[bv(a59, u_point), bv(b59, B59_Init)]|TLEnv],
   get_var(LEnv, a59, A59_Get),
   get_var(LEnv, b59, B59_Get),
   (   is_eq(A59_Get, B59_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [defstruct, u_point, u_x, u_y, u_z]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a59, A59_Get13),
       get_var(LEnv, b59, B59_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A59_Get13,
		   B59_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
; (defstruct point x y z)
```
```prolog
```
```common-lisp
(is eq 'point4d (defstruct point4d x y z t))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point4d],[defstruct,point4d,x,y,z,t]]).
% macroexpand:-[u_is,eq,[quote,u_point4d],[defstruct,u_point4d,u_x,u_y,u_z,t]].
% into:-[let,[[a510,[quote,u_point4d]],[b510,[defstruct,u_point4d,u_x,u_y,u_z,t]]],[if,[eq,a510,b510],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point4d]],[quote,eq],[quote,[defstruct,u_point4d,u_x,u_y,u_z,t]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a510,b510],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_defstruct([u_point4d,u_x,u_y,u_z,t],_23718),_23368=[[bv(a510,u_point4d),bv(b510,_23718)]|_21760],get_var(_23368,a510,_31140),get_var(_23368,b510,_31606),(is_eq(_31140,_31606)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point4d],eq,[defstruct,u_point4d,u_x,u_y,u_z,t]],_32016),_22172=_32016;get_var(_23368,a510,_34840),get_var(_23368,b510,_36514),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_34840,_36514],_33278),trace,_22172=_33278).
:- cl_defstruct([u_point4d, u_x, u_y, u_z, t], B510_Init),
   LEnv=[[bv(a510, u_point4d), bv(b510, B510_Init)]|TLEnv],
   get_var(LEnv, a510, A510_Get),
   get_var(LEnv, b510, B510_Get),
   (   is_eq(A510_Get, B510_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point4d],
		   eq,
		   [defstruct, u_point4d, u_x, u_y, u_z, t]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a510, A510_Get13),
       get_var(LEnv, b510, B510_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A510_Get13,
		   B510_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,'distance-from-origin',[point],['let*',[[x,['point-x',point]],[y,['point-y',point]],[z,['point-z',point]]],[sqrt,[+,[*,x,x],[*,y,y],[*,z,z]]]]]).
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_distance_from_origin).

```

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
```prolog
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	f_u_point_x(Point_Param, X_Init),
	LEnv=[[bv(u_x, X_Init)]|Env],
	f_u_point_y(Point_Param, Y_Init),
	LEnv16=[[bv(u_y, Y_Init)]|LEnv],
	f_u_point_z(Point_Param, Z_Init),
	LEnv20=[[bv(u_z, Z_Init)]|LEnv16],
	get_var(LEnv20, u_x, X_Get25),
	*(X_Get25, X_Get25, _22646),
	get_var(LEnv20, u_y, Y_Get27),
	*(Y_Get27, Y_Get27, _22744),
	+(_22646, _22744, _22864),
	get_var(LEnv20, u_z, Z_Get29),
	*(Z_Get29, Z_Get29, _22876),
	+(_22864, _22876, Sqrt_Param),
	cl_sqrt(Sqrt_Param, LetResult17),
	LetResult17=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
```
```common-lisp
(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,'reflect-in-y-axis',[point],[setf,['point-y',point],[-,['point-y',point]]]]).
wl:lambda_def(defun, u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).
wl:arglist_info(u_reflect_in_y_axis, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_reflect_in_y_axis).

```

### Compiled:  `U::REFLECT-IN-Y-AXIS` 
```prolog
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
```
```common-lisp
(list (setf my-point (make-point :x 3 :y 4 :z 12)) (setf my-point2 (make-point :x 3 :y 4 :z 12)))
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[list,[setf,'my-point',['make-point',':x',3,':y',4,':z',12]],[setf,'my-point2',['make-point',':x',3,':y',4,':z',12]]]).
:- f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], Make_point_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_point_Ret], Setf_R),
   f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], Make_point_Ret8),
   set_place(TLEnv, setf, [value, u_my_point2], [Make_point_Ret8], Setf_R6),
   _IgnoredResult=[Setf_R, Setf_R6].
```
```common-lisp
(setf my-point3 #S(POINT :X 3 :Y 4 :Z 12))
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point3','$S'(['POINT',':X',3,':Y',4,':Z',12])]).
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_my_point3], [Create_struct_Ret], Setf_R).
```
```common-lisp
(setf my-point4d (make-point4d :x 3 :y 4 :z 12 :t 1))


```
```prolog
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point4d',['make-point4d',':x',3,':y',4,':z',12,':t',1]]).
:- f_u_make_point4d([kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1], Make_point4d_Ret),
   set_place(TLEnv, setf, [value, u_my_point4d], [Make_point4d_Ret], Setf_R).
```
```common-lisp
(is eq t (point-p my-point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,['point-p','my-point']]).
% macroexpand:-[u_is,eq,t,[u_point_p,u_my_point]].
% into:-[let,[[a601,t],[b601,[u_point_p,u_my_point]]],[if,[eq,a601,b601],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[u_point_p,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a601,b601],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22048,u_my_point,_24036),f_u_point_p(_24036,_23908),_23488=[[bv(a601,t),bv(b601,_23908)]|_22048],get_var(_23488,a601,_33742),get_var(_23488,b601,_34208),(is_eq(_33742,_34208)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[u_point_p,u_my_point]],_34510),_22352=_34510;get_var(_23488,a601,_37226),get_var(_23488,b601,_38900),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_37226,_38900],_35664),trace,_22352=_35664).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_point_p(My_point_Get, B601_Init),
   LEnv=[[bv(a601, t), bv(b601, B601_Init)]|TLEnv],
   get_var(LEnv, a601, A601_Get),
   get_var(LEnv, b601, B601_Get),
   (   is_eq(A601_Get, B601_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [u_point_p, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a601, A601_Get14),
       get_var(LEnv, b601, B601_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A601_Get14,
		   B601_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq 'point (type-of my-point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point],['type-of','my-point']]).
% macroexpand:-[u_is,eq,[quote,u_point],[type_of,u_my_point]].
% into:-[let,[[a62,[quote,u_point]],[b62,[type_of,u_my_point]]],[if,[eq,a62,b62],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[type_of,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a62,b62],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22162,u_my_point,_24198),cl_type_of(_24198,_24070),_23650=[[bv(a62,u_point),bv(b62,_24070)]|_22162],get_var(_23650,a62,_33828),get_var(_23650,b62,_34294),(is_eq(_33828,_34294)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[type_of,u_my_point]],_34632),_22502=_34632;get_var(_23650,a62,_37342),get_var(_23650,b62,_38974),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_37342,_38974],_35822),trace,_22502=_35822).
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_type_of(My_point_Get, B62_Init),
   LEnv=[[bv(a62, u_point), bv(b62, B62_Init)]|TLEnv],
   get_var(LEnv, a62, A62_Get),
   get_var(LEnv, b62, B62_Get),
   (   is_eq(A62_Get, B62_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [type_of, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a62, A62_Get14),
       get_var(LEnv, b62, B62_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A62_Get14,
		   B62_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
#+IGNORE #+WAM-CL (prolog-call "break")

```
```prolog
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'(flag_removed(+':IGNORE',[#+,':WAM-CL',['prolog-call','$STRING'("break")]]))).
```
```common-lisp
:- was_info(flag_removed(+':IGNORE',
			 [#+, ':WAM-CL', ['prolog-call', '$STRING'("break")]])).
```
```prolog
```
```common-lisp
(is eql 13 (progn (print (distance-from-origin my-point))))

;; #+CLISP (BREAK)
;; #+WAM-CL (prolog-call "break")

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eql,13,[progn,[print,['distance-from-origin','my-point']]]]).
% macroexpand:-[u_is,eql,13,[progn,[print,[u_distance_from_origin,u_my_point]]]].
% into:-[let,[[a63,13],[b63,[progn,[print,[u_distance_from_origin,u_my_point]]]]],[if,[eql,a63,b63],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,13],[quote,eql],[quote,[progn,[print,[u_distance_from_origin,u_my_point]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a63,b63],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22282,u_my_point,_22820),f_u_distance_from_origin(_22820,_22818),cl_print(_22818,_22814),_22782=[[bv(a63,13),bv(b63,_22814)]|_22282],get_var(_22782,a63,_23278),get_var(_22782,b63,_23306),(is_eql(_23278,_23306)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),13,eql,[progn,[print,[u_distance_from_origin,u_my_point]]]],_23320),_22656=_23320;get_var(_22782,a63,_23340),get_var(_22782,b63,_23386),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_23340,_23386],_23322),trace,_22656=_23322).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, Print_Param),
   cl_print(Print_Param, B63_Init),
   LEnv=[[bv(a63, 13), bv(b63, B63_Init)]|TLEnv],
   get_var(LEnv, a63, A63_Get),
   get_var(LEnv, b63, B63_Get),
   (   is_eql(A63_Get, B63_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   13,
		   eql,
		   [progn, [print, [u_distance_from_origin, u_my_point]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a63, A63_Get14),
       get_var(LEnv, b63, B63_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A63_Get14,
		   B63_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
; #+CLISP (BREAK)
```
```prolog
```
```common-lisp
; #+WAM-CL (prolog-call "break")
```
```prolog
```
```common-lisp
(is = -4 (reflect-in-y-axis my-point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,=,-4,['reflect-in-y-axis','my-point']]).
% macroexpand:-[u_is,=,-4,[u_reflect_in_y_axis,u_my_point]].
% into:-[let,[[a64,-4],[b64,[u_reflect_in_y_axis,u_my_point]]],[if,[=,a64,b64],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,-4],[quote,=],[quote,[u_reflect_in_y_axis,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a64,b64],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22292,u_my_point,_24268),f_u_reflect_in_y_axis(_24268,_24140),_23720=[[bv(a64,-4),bv(b64,_24140)]|_22292],get_var(_23720,a64,_33694),get_var(_23720,b64,_34160),(_33694=:=_34160->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),-4,=,[u_reflect_in_y_axis,u_my_point]],_34462),_22596=_34462;get_var(_23720,a64,_37136),get_var(_23720,b64,_38768),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_37136,_38768],_35616),trace,_22596=_35616).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_reflect_in_y_axis(My_point_Get, B64_Init),
   LEnv=[[bv(a64, -4), bv(b64, B64_Init)]|TLEnv],
   get_var(LEnv, a64, A64_Get),
   get_var(LEnv, b64, B64_Get),
   (   A64_Get=:=B64_Get
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   -4,
		   (=),
		   [u_reflect_in_y_axis, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a64, A64_Get14),
       get_var(LEnv, b64, B64_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A64_Get14,
		   B64_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq my-point my-point)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,'my-point','my-point']).
% macroexpand:-[u_is,eq,u_my_point,u_my_point].
% into:-[let,[[a65,u_my_point],[b65,u_my_point]],[if,[eq,a65,b65],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_my_point],[quote,eq],[quote,u_my_point]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a65,b65],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22340,u_my_point,_28784),get_var(_22340,u_my_point,_26964),_23708=[[bv(a65,_28784),bv(b65,_26964)]|_22340],get_var(_23708,a65,_36258),get_var(_23708,b65,_36724),(is_eq(_36258,_36724)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),u_my_point,eq,u_my_point],_36990),_22608=_36990;get_var(_23708,a65,_39628),get_var(_23708,b65,_41260),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_39628,_41260],_38108),trace,_22608=_38108).
:- get_var(TLEnv, u_my_point, My_point_Get7),
   LEnv=[[bv(a65, My_point_Get7), bv(b65, My_point_Get7)]|TLEnv],
   get_var(LEnv, a65, A65_Get),
   get_var(LEnv, b65, B65_Get),
   (   is_eq(A65_Get, B65_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   u_my_point,
		   eq,
		   u_my_point
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a65, A65_Get16),
       get_var(LEnv, b65, B65_Get17),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A65_Get16,
		   B65_Get17
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(setf a-similar-point #s(point :x 3 :y -4 :z 12))

; (is eq t (equal my-point a-similar-point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[setf,'a-similar-point','$S'([point,':x',3,':y',-4,':z',12])]).
:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_a_similar_point], [Create_struct_Ret], Setf_R).
```
```common-lisp
 (is eq t (equal my-point a-similar-point))
```
```prolog
```
```common-lisp
(is eq nil (eq my-point a-similar-point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[],[eq,'my-point','a-similar-point']]).
% macroexpand:-[u_is,eq,[],[eq,u_my_point,u_a_similar_point]].
% into:-[let,[[a66,[]],[b66,[eq,u_my_point,u_a_similar_point]]],[if,[eq,a66,b66],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[eq,u_my_point,u_a_similar_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a66,b66],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22532,u_my_point,_24544),get_var(_22532,u_a_similar_point,_26338),cl_eq(_24544,_26338,_24410),_23990=[[bv(a66,[]),bv(b66,_24410)]|_22532],get_var(_23990,a66,_36498),get_var(_23990,b66,_36964),(is_eq(_36498,_36964)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[eq,u_my_point,u_a_similar_point]],_37284),_22854=_37284;get_var(_23990,a66,_39976),get_var(_23990,b66,_41608),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_39976,_41608],_38456),trace,_22854=_38456).
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_eq(My_point_Get, A_similar_point_Get, B66_Init),
   LEnv=[[bv(a66, []), bv(b66, B66_Init)]|TLEnv],
   get_var(LEnv, a66, A66_Get),
   get_var(LEnv, b66, B66_Get),
   (   is_eq(A66_Get, B66_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [eq, u_my_point, u_a_similar_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a66, A66_Get15),
       get_var(LEnv, b66, B66_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A66_Get15,
		   B66_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(equalp my-point a-similar-point)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[equalp,'my-point','a-similar-point']).
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, _IgnoredResult).
```
```common-lisp
(is eq t (equalp my-point a-similar-point) )


;; 3.2. defclass

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,t,[equalp,'my-point','a-similar-point']]).
% macroexpand:-[u_is,eq,t,[equalp,u_my_point,u_a_similar_point]].
% into:-[let,[[a67,t],[b67,[equalp,u_my_point,u_a_similar_point]]],[if,[eq,a67,b67],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[equalp,u_my_point,u_a_similar_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a67,b67],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22614,u_my_point,_24626),get_var(_22614,u_a_similar_point,_26420),cl_equalp(_24626,_26420,_24492),_24072=[[bv(a67,t),bv(b67,_24492)]|_22614],get_var(_24072,a67,_36580),get_var(_24072,b67,_37046),(is_eq(_36580,_37046)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[equalp,u_my_point,u_a_similar_point]],_37366),_22936=_37366;get_var(_24072,a67,_40058),get_var(_24072,b67,_41690),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_40058,_41690],_38538),trace,_22936=_38538).
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, B67_Init),
   LEnv=[[bv(a67, t), bv(b67, B67_Init)]|TLEnv],
   get_var(LEnv, a67, A67_Get),
   get_var(LEnv, b67, B67_Get),
   (   is_eq(A67_Get, B67_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [equalp, u_my_point, u_a_similar_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a67, A67_Get15),
       get_var(LEnv, b67, B67_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A67_Get15,
		   B67_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
; 3.2. defclass
```
```prolog
```
```common-lisp
(unintern 'point)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[unintern,[quote,point]]).
:- cl_unintern(u_point, _IgnoredResult).
```
```common-lisp
(defclass point ()
  (x
   y
   z))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defclass,point,[],[x,y,z]]).
:- cl_defclass([u_point, [], [u_x, u_y, u_z]], _IgnoredResult).
```
```common-lisp
(setf my-point (make-instance 'point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[setf,'my-point',['make-instance',[quote,point]]]).
:- cl_make_instance([u_point], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_instance_Ret], Setf_R).
```
```common-lisp
(is eq 'point (type-of my-point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,point],['type-of','my-point']]).
% macroexpand:-[u_is,eq,[quote,u_point],[type_of,u_my_point]].
% into:-[let,[[a68,[quote,u_point]],[b68,[type_of,u_my_point]]],[if,[eq,a68,b68],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[type_of,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a68,b68],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_22738,u_my_point,_24774),cl_type_of(_24774,_24646),_24226=[[bv(a68,u_point),bv(b68,_24646)]|_22738],get_var(_24226,a68,_34404),get_var(_24226,b68,_34870),(is_eq(_34404,_34870)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[type_of,u_my_point]],_35208),_23078=_35208;get_var(_24226,a68,_37918),get_var(_24226,b68,_39550),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_37918,_39550],_36398),trace,_23078=_36398).
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_type_of(My_point_Get, B68_Init),
   LEnv=[[bv(a68, u_point), bv(b68, B68_Init)]|TLEnv],
   get_var(LEnv, a68, A68_Get),
   get_var(LEnv, b68, B68_Get),
   (   is_eq(A68_Get, B68_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [type_of, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a68, A68_Get14),
       get_var(LEnv, b68, B68_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A68_Get14,
		   B68_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(defun set-point-values (point x y z)
  (setf (slot-value point 'x) x
        (slot-value point 'y) y
        (slot-value point 'z) z))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,'set-point-values',[point,x,y,z],[setf,['slot-value',point,[quote,x]],x,['slot-value',point,[quote,y]],y,['slot-value',point,[quote,z]],z]]).
wl:lambda_def(defun, u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]).
wl:arglist_info(u_set_point_values, [u_point, u_x, u_y, u_z], [Point_Param, X_Param, Y_Param, Z_Param], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_set_point_values).

```

### Compiled:  `U::SET-POINT-VALUES` 
```prolog
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
```
```common-lisp
(set-point-values my-point 3 4 12)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['set-point-values','my-point',3,4,12]).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_set_point_values(My_point_Get, 3, 4, 12, _IgnoredResult).
```
```common-lisp
(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))


```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defun,'distance-from-origin',[point],['with-slots',[x,y,z],point,[sqrt,[+,[*,x,x],[*,y,y],[*,z,z]]]]]).
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_distance_from_origin).

```

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
```prolog
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	cl_slot_value(Point_Param, u_x, X_Init),
	cl_slot_value(Point_Param, u_y, Y_Init),
	cl_slot_value(Point_Param, u_z, Z_Init),
	LEnv=[[bv(u_x, X_Init), bv(u_y, Y_Init), bv(u_z, Z_Init)]|Env],
	get_var(LEnv, u_x, X_Get21),
	*(X_Get21, X_Get21, _23514),
	get_var(LEnv, u_y, Y_Get23),
	*(Y_Get23, Y_Get23, _23670),
	+(_23514, _23670, _23848),
	get_var(LEnv, u_z, Z_Get25),
	*(Z_Get25, Z_Get25, _23860),
	+(_23848, _23860, Sqrt_Param),
	cl_sqrt(Sqrt_Param, LetResult),
	LetResult=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
```
```common-lisp
(DISASSEMBLE #'distance-from-origin)


```
```prolog
:-lisp_compile_to_prolog(pkg_user,['DISASSEMBLE',function('distance-from-origin')]).
:- cl_disassemble(function(u_distance_from_origin), _IgnoredResult).
```
```common-lisp
(distance-from-origin my-point)

;; 3.3. classes are objects

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['distance-from-origin','my-point']).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, _IgnoredResult).
```
```common-lisp
; 3.3. classes are objects
```
```prolog
```
```common-lisp
(find-class 'point)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['find-class',[quote,point]]).
:- cl_find_class(u_point, _IgnoredResult).
```
```common-lisp
(class-name (find-class 'point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['class-name',['find-class',[quote,point]]]).
:- cl_find_class(u_point, Class_name_Param),
   cl_class_name(Class_name_Param, _IgnoredResult).
```
```common-lisp
(class-of my-point)

;; #-(or cormanlisp CLISP WAM-CL)
```
```prolog
:-lisp_compile_to_prolog(pkg_user,['class-of','my-point']).
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, _IgnoredResult).
```
```common-lisp
; #-(or cormanlisp CLISP WAM-CL)
```
```prolog
```
```common-lisp
(typep my-point (class-of my-point))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[typep,'my-point',['class-of','my-point']]).
:- get_var(TLEnv, u_my_point, My_point_Get5),
   cl_class_of(My_point_Get5, Class_of_Ret),
   cl_typep(My_point_Get5, Class_of_Ret, _IgnoredResult).
```
```common-lisp
(is eq (find-class 'STANDARD-CLASS)
       (class-of (class-of my-point)))

;; 3.4. you don't need clos to use clos

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,['find-class',[quote,'STANDARD-CLASS']],['class-of',['class-of','my-point']]]).
% macroexpand:-[u_is,eq,[find_class,[quote,standard_class]],[class_of,[class_of,u_my_point]]].
% into:-[let,[[a69,[find_class,[quote,standard_class]]],[b69,[class_of,[class_of,u_my_point]]]],[if,[eq,a69,b69],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[find_class,[quote,standard_class]]],[quote,eq],[quote,[class_of,[class_of,u_my_point]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a69,b69],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_find_class(standard_class,_23450),get_var(_22882,u_my_point,_23468),cl_class_of(_23468,_23464),cl_class_of(_23464,_23460),_23418=[[bv(a69,_23450),bv(b69,_23460)]|_22882],get_var(_23418,a69,_23940),get_var(_23418,b69,_23968),(is_eq(_23940,_23968)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[find_class,[quote,standard_class]],eq,[class_of,[class_of,u_my_point]]],_23982),_23292=_23982;get_var(_23418,a69,_24002),get_var(_23418,b69,_24048),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_24002,_24048],_23984),trace,_23292=_23984).
:- cl_find_class(standard_class, A69_Init),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, Class_of_Param),
   cl_class_of(Class_of_Param, B69_Init),
   LEnv=[[bv(a69, A69_Init), bv(b69, B69_Init)]|TLEnv],
   get_var(LEnv, a69, A69_Get),
   get_var(LEnv, b69, B69_Get),
   (   is_eq(A69_Get, B69_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [find_class, [quote, standard_class]],
		   eq,
		   [class_of, [class_of, u_my_point]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a69, A69_Get15),
       get_var(LEnv, b69, B69_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A69_Get15,
		   B69_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
; 3.4. you don't need clos to use clos
```
```prolog
```
```common-lisp
(let ((the-symbol-class (find-class 'symbol)))
  (values the-symbol-class
          (class-name the-symbol-class)
          (eq the-symbol-class (class-of 'symbol))
          (class-of the-symbol-class)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[let,[['the-symbol-class',['find-class',[quote,symbol]]]],[values,'the-symbol-class',['class-name','the-symbol-class'],[eq,'the-symbol-class',['class-of',[quote,symbol]]],['class-of','the-symbol-class']]]).
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
```
```common-lisp
(find-class t)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['find-class',t]).
:- cl_find_class(t, _IgnoredResult).
```
```common-lisp
(is eq 'foo (defstruct foo))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,[quote,foo],[defstruct,foo]]).
% macroexpand:-[u_is,eq,[quote,u_foo],[defstruct,u_foo]].
% into:-[let,[[a610,[quote,u_foo]],[b610,[defstruct,u_foo]]],[if,[eq,a610,b610],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_foo]],[quote,eq],[quote,[defstruct,u_foo]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a610,b610],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_defstruct([u_foo],_24738),_24388=[[bv(a610,u_foo),bv(b610,_24738)]|_22900],get_var(_24388,a610,_31992),get_var(_24388,b610,_32458),(is_eq(_31992,_32458)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_foo],eq,[defstruct,u_foo]],_32796),_23240=_32796;get_var(_24388,a610,_35548),get_var(_24388,b610,_37222),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_35548,_37222],_33986),trace,_23240=_33986).
:- cl_defstruct([u_foo], B610_Init),
   LEnv=[[bv(a610, u_foo), bv(b610, B610_Init)]|TLEnv],
   get_var(LEnv, a610, A610_Get),
   get_var(LEnv, b610, B610_Get),
   (   is_eq(A610_Get, B610_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_foo],
		   eq,
		   [defstruct, u_foo]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a610, A610_Get13),
       get_var(LEnv, b610, B610_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A610_Get13,
		   B610_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
(is eq (find-class 'foo) (class-of (make-foo)))

;; 3.5 slots

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[is,eq,['find-class',[quote,foo]],['class-of',['make-foo']]]).
% macroexpand:-[u_is,eq,[find_class,[quote,u_foo]],[class_of,[u_make_foo]]].
% into:-[let,[[a701,[find_class,[quote,u_foo]]],[b701,[class_of,[u_make_foo]]]],[if,[eq,a701,b701],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[find_class,[quote,u_foo]]],[quote,eq],[quote,[class_of,[u_make_foo]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a701,b701],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_find_class(u_foo,_23584),f_u_make_foo([],_23598),cl_class_of(_23598,_23594),_23552=[[bv(a701,_23584),bv(b701,_23594)]|_23022],get_var(_23552,a701,_23946),get_var(_23552,b701,_23974),(is_eq(_23946,_23974)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[find_class,[quote,u_foo]],eq,[class_of,[u_make_foo]]],_23988),_23414=_23988;get_var(_23552,a701,_24008),get_var(_23552,b701,_24054),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_24008,_24054],_23990),trace,_23414=_23990).
:- cl_find_class(u_foo, A701_Init),
   f_u_make_foo([], Class_of_Param),
   cl_class_of(Class_of_Param, B701_Init),
   LEnv=[[bv(a701, A701_Init), bv(b701, B701_Init)]|TLEnv],
   get_var(LEnv, a701, A701_Get),
   get_var(LEnv, b701, B701_Get),
   (   is_eq(A701_Get, B701_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [find_class, [quote, u_foo]],
		   eq,
		   [class_of, [u_make_foo]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a701, A701_Get14),
       get_var(LEnv, b701, B701_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A701_Get14,
		   B701_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
```
```common-lisp
; 3.5 slots
```
```prolog
```
```common-lisp
(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader daft-z :allocation :class)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defclass,'daft-point',[],[[x,':accessor','daft-x',':initarg',':x'],[y,':accessor','daft-y',':initform',3.14159],[z,':reader','daft-z',':allocation',':class']]]).
:- cl_defclass(
	       [ u_daft_point,
		 [],
		 
		 [ [u_x, kw_accessor, u_daft_x, kw_initarg, kw_x],
		   [u_y, kw_accessor, u_daft_y, kw_initform, 3.14159],
		   [u_z, kw_reader, u_daft_z, kw_allocation, kw_class]
		 ]
	       ],
	       _IgnoredResult).
```
```common-lisp
(setf (slot-value (make-instance 'daft-point) 'z) 42)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[setf,['slot-value',['make-instance',[quote,'daft-point']],[quote,z]],42]).
:- cl_make_instance([u_daft_point], Make_instance_Ret),
   set_place(TLEnv, setf, [slot_value, Make_instance_Ret, u_z], [42], Setf_R).
```
```common-lisp
(setf my-daft-point (make-instance 'daft-point :x 19))


```
```prolog
:-lisp_compile_to_prolog(pkg_user,[setf,'my-daft-point',['make-instance',[quote,'daft-point'],':x',19]]).
:- cl_make_instance([u_daft_point, kw_x, 19], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_daft_point], [Make_instance_Ret], Setf_R).
```
```common-lisp
#+PERFECT 
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (progn #+WAM-CL (prolog-trace) (daft-z my-daft-point)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'(flag_removed(+':PERFECT',[list,['daft-x','my-daft-point'],['daft-y','my-daft-point'],[progn,[#+,':WAM-CL',['prolog-trace']],['daft-z','my-daft-point']]]))).
```
```common-lisp
:- was_info(flag_removed(+':PERFECT',
			 
			 [ list,
			   ['daft-x', 'my-daft-point'],
			   ['daft-y', 'my-daft-point'],
			   
			   [ progn,
			     [#+, ':WAM-CL', ['prolog-trace']],
			     ['daft-z', 'my-daft-point']
			   ]
			 ])).
```
```prolog
```
```common-lisp
(let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[let,[[temp,['make-instance',[quote,'daft-point']]]],[setf,['daft-y',temp],999,['slot-value',temp,[quote,z]],0]]).
:- cl_make_instance([u_daft_point], Temp_Init),
   LEnv=[[bv(u_temp, Temp_Init)]|TLEnv],
   get_var(LEnv, u_temp, Temp_Get),
   set_place(LEnv, setf, [u_daft_y, Temp_Get], [999], Setf_R),
   get_var(LEnv, u_temp, Temp_Get11),
   set_place(LEnv, setf, [slot_value, Temp_Get11, u_z], [0], Setf_R10).
```
```common-lisp
#+PERFECT
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

;; 3.6 Subclasses and inheritance

```
```prolog
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'(flag_removed(+':PERFECT',[list,['daft-x','my-daft-point'],['daft-y','my-daft-point'],['daft-z','my-daft-point']]))).
```
```common-lisp
:- was_info(flag_removed(+':PERFECT',
			 
			 [ list,
			   ['daft-x', 'my-daft-point'],
			   ['daft-y', 'my-daft-point'],
			   ['daft-z', 'my-daft-point']
			 ])).
```
```prolog
```
```common-lisp
; 3.6 Subclasses and inheritance
```
```prolog
```
```common-lisp
(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defclass,animal,[],[[legs,':reader','leg-count',':initarg',':legs'],['comes-from',':reader','comes-from',':initarg',':comes-from']]]).
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
	       _IgnoredResult).
```
```common-lisp
(defclass mammal (animal)
  ((diet :initform 'antelopes :initarg :diet)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defclass,mammal,[animal],[[diet,':initform',[quote,antelopes],':initarg',':diet']]]).
:- cl_defclass(
	       [ u_mammal,
		 [u_animal],
		 [[u_diet, kw_initform, [quote, u_antelopes], kw_initarg, kw_diet]]
	       ],
	       _IgnoredResult).
```
```common-lisp
(defclass aardvark (mammal)
  ((cute-p :accessor cute-p :initform nil)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defclass,aardvark,[mammal],[['cute-p',':accessor','cute-p',':initform',[]]]]).
:- cl_defclass(
	       [ u_aardvark,
		 [u_mammal],
		 [[u_cute_p, kw_accessor, u_cute_p, kw_initform, []]]
	       ],
	       _IgnoredResult).
```
```common-lisp
(#-allegro class-direct-superclasses #+allegro aclmop:class-direct-superclasses
   (find-class 'aardvark))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro
```
```prolog
:-lisp_compile_to_prolog(pkg_user,['class-direct-superclasses',['find-class',[quote,aardvark]]]).
:- cl_find_class(u_aardvark, Direct_superclasses_Param),
   f_clos_class_direct_superclasses(Direct_superclasses_Param, _IgnoredResult).
```
```common-lisp
; ACL needs to instantiate a class before its precedence-list becomes visible
```
```prolog
```
```common-lisp
; #+allegro
```
```prolog
```
```common-lisp
(make-instance 'aardvark)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['make-instance',[quote,aardvark]]).
:- cl_make_instance([u_aardvark], _IgnoredResult).
```
```common-lisp
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
   (find-class 'aardvark))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['class-precedence-list',['find-class',[quote,aardvark]]]).
:- cl_find_class(u_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _IgnoredResult).
```
```common-lisp
(defclass figurine ()
  ((potter :accessor made-by :initarg :made-by)
   (comes-from :initarg :made-in)))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defclass,figurine,[],[[potter,':accessor','made-by',':initarg',':made-by'],['comes-from',':initarg',':made-in']]]).
:- cl_defclass(
	       [ u_figurine,
		 [],
		 
		 [ [u_potter, kw_accessor, u_made_by, kw_initarg, kw_made_by],
		   [u_comes_from, kw_initarg, kw_made_in]
		 ]
	       ],
	       _IgnoredResult).
```
```common-lisp
(defclass figurine-aardvark (aardvark figurine)
  ((name :reader aardvark-name :initarg :aardvark-name)
   (diet :initform nil)))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro 
```
```prolog
:-lisp_compile_to_prolog(pkg_user,[defclass,'figurine-aardvark',[aardvark,figurine],[[name,':reader','aardvark-name',':initarg',':aardvark-name'],[diet,':initform',[]]]]).
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
	       _IgnoredResult).
```
```common-lisp
; ACL needs to instantiate a class before its precedence-list becomes visible
```
```prolog
```
```common-lisp
; #+allegro 
```
```prolog
```
```common-lisp
(make-instance 'figurine-aardvark)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['make-instance',[quote,'figurine-aardvark']]).
:- cl_make_instance([u_figurine_aardvark], _IgnoredResult).
```
```common-lisp
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
             (find-class 'figurine-aardvark))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['class-precedence-list',['find-class',[quote,'figurine-aardvark']]]).
:- cl_find_class(u_figurine_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _IgnoredResult).
```
```common-lisp
(setf Eric (make-instance 'figurine-aardvark
                          :legs 4
                          :made-by "Jen"
                          :made-in "Brittany"
                          :aardvark-name "Eric"))

```
```prolog
:-lisp_compile_to_prolog(pkg_user,[setf,'Eric',['make-instance',[quote,'figurine-aardvark'],':legs',4,':made-by','$STRING'("Jen"),':made-in','$STRING'("Brittany"),':aardvark-name','$STRING'("Eric")]]).
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
```
```common-lisp
#+HAS_SHIFTF
(shiftf (cute-p Eric) t)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,'$COMMENT'(flag_removed(+':HAS_SHIFTF',[shiftf,['cute-p','Eric'],t]))).
```
```common-lisp
:- was_info(flag_removed(+':HAS_SHIFTF', [shiftf, ['cute-p', 'Eric'], t])).
```
```prolog
```
```common-lisp
(slot-value Eric 'diet)

```
```prolog
:-lisp_compile_to_prolog(pkg_user,['slot-value','Eric',[quote,diet]]).
:- get_var(TLEnv, u_eric, Eric_Get),
   cl_slot_value(Eric_Get, u_diet, _IgnoredResult).
T
```


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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:886 **********************/
:- lisp_compile_to_prolog(pkg_user, ['in-package', '$STRING'("CL-USER")]).
:- cl_in_package('$ARRAY'([*],
			  claz_base_character,
			  
			  [ '#\\'('C'),
			    '#\\'('L'),
			    '#\\'(-),
			    '#\\'('U'),
			    '#\\'('S'),
			    '#\\'('E'),
			    '#\\'('R')
			  ]),
		 In_package_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:910 **********************/
:- lisp_compile_to_prolog(pkg_user, ['prolog-call', '$STRING'("cls.")]).
:- (   cls
   ->  _rPrevRes=t
   ;   _rPrevRes=[]
   ).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:941 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'mapcar-visualize',
			    [func, l],
			    
			    [ if,
			      [null, l],
			      [],
			      
			      [ cons,
				[apply, func, [list, [first, l]]],
				[mapcar, func, [rest, l]]
			      ]
			    ]
			  ]).
/* 
alphas=[u_l, u_func].
type=ctx.
var_tracker(u_func)=rw{name:u_func, p:1, r:2, ret:0, u:0, vars:[U_func_Param, U_func_Get, U_func_Get49], w:1}.
var_tracker(u_l)=rw{name:u_l, p:1, r:3, ret:0, u:0, vars:[U_l_Param, IFTEST, U_l_Get43, U_l_Get53], w:1}.
 */

% asserting...
wl:arglist_info(f_u_mapcar_visualize, [u_func, u_l], [U_func_Param, U_l_Param], arginfo{all:2, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:2, rest:0}).

% asserting...
function_lambda(defun(u_mapcar_visualize), f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]).

% asserting...
f_u_mapcar_visualize(U_func_Param, U_l_Param, _rPrevRes) :-
	catch((   U_l_Param==[]
	      ->  _rPrevRes=[]
	      ;   cl_car(U_l_Param, Cl_car_Ret),
		  List_Ret=[Cl_car_Ret],
		  cl_apply(U_func_Param, List_Ret, Apply_Ret),
		  cl_cdr(U_l_Param, Cl_cdr_Ret),
		  cl_mapcar(U_func_Param, Cl_cdr_Ret, Mapcar_Ret),
		  _rPrevRes=[Apply_Ret|Mapcar_Ret]
	      ),
	      block_exit(u_mapcar_visualize, _rPrevRes),
	      true).
:- set_opv(f_u_mapcar_visualize, classof, claz_compiled_function),
   set_opv(u_mapcar_visualize, compile_as, kw_function),
   set_opv(u_mapcar_visualize, function, f_u_mapcar_visualize).
/*; Test macro*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    (is),
			    [eqf, expected, actual],
			    
			    [ let,
			      
			      [ [a, [gensym, '$STRING'("a")]],
				[b, [gensym, '$STRING'("b")]]
			      ],
			      
			      [ '#BQ',
				
				[ let,
				  
				  [ [['#COMMA', a], ['#COMMA', expected]],
				    [['#COMMA', b], ['#COMMA', actual]]
				  ],
				  
				  [ if,
				    
				    [ not,
				      
				      [ ['#COMMA', eqf],
					['#COMMA', a],
					['#COMMA', b]
				      ]
				    ],
				    
				    [ progn,
				      
				      [ format,
					t,
					'$STRING'("FAILED: when matching ~a and ~a~%"),
					['#COMMA', a],
					['#COMMA', b]
				      ],
				      ['prolog-inline', '$STRING'("trace")],
				      [quote, [quit, 1]]
				    ],
				    
				    [ format,
				      t,
				      '$STRING'("OK: ~a is ~a to ~a~%"),
				      [quote, ['#COMMA', expected]],
				      [quote, ['#COMMA', eqf]],
				      [quote, ['#COMMA', actual]]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_b, u_a, u_actual, u_expected, u_eqf, f_u_is].
type=ctx.
var_tracker(u_actual)=rw{name:u_actual, p:1, r:0, ret:0, u:0, vars:[U_actual_Param], w:1}.
var_tracker(u_eqf)=rw{name:u_eqf, p:1, r:0, ret:0, u:0, vars:[U_eqf_Param], w:1}.
var_tracker(u_expected)=rw{name:u_expected, p:1, r:0, ret:0, u:0, vars:[U_expected_Param], w:1}.
 */

% asserting...
wl:arglist_info(f_u_is, [u_eqf, u_expected, u_actual], [U_eqf_Param, U_expected_Param, U_actual_Param], arginfo{all:3, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:3, rest:0}).

% asserting...
macro_lambda(defmacro(u_is), f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, ['#\\'(a)])]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, ['#\\'(b)])]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [not, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, ['#\\'('F'), '#\\'('A'), '#\\'('I'), '#\\'('L'), '#\\'('E'), '#\\'('D'), '#\\'(:), '#\\'(' '), '#\\'(w), '#\\'(h), '#\\'(e), '#\\'(n), '#\\'(' '), '#\\'(m), '#\\'(a), '#\\'(t), '#\\'(c), '#\\'(h), '#\\'(i), '#\\'(n), '#\\'(g), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(' '), '#\\'(a), '#\\'(n), '#\\'(d), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(~), '#\\'('%')]), ['#COMMA', u_a], ['#COMMA', u_b]], [u_prolog_inline, '$ARRAY'([*], claz_base_character, ['#\\'(t), '#\\'(r), '#\\'(a), '#\\'(c), '#\\'(e)])], [quote, [ext_quit, 1]]], [format, t, '$ARRAY'([*], claz_base_character, ['#\\'('O'), '#\\'('K'), '#\\'(:), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(' '), '#\\'(i), '#\\'(s), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(' '), '#\\'(t), '#\\'(o), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(~), '#\\'('%')]), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]]]]]]], [u_b, u_a, u_actual, u_expected, u_eqf, f_u_is]).

% asserting...
f_u_is(U_eqf_Param, U_expected_Param, U_actual_Param, FnResult) :-
	Env=[bv(u_eqf, U_eqf_Param), bv(u_expected, U_expected_Param), bv(u_actual, U_actual_Param)],
	catch(( cl_gensym('$ARRAY'([*], claz_base_character, ['#\\'(a)]),
			  Gensym_Ret),
		cl_gensym('$ARRAY'([*], claz_base_character, ['#\\'(b)]),
			  Gensym_Ret37),
		LETENV=[[bv(u_a, Gensym_Ret), bv(u_b, Gensym_Ret37)]|Env],
		get_var(LETENV, u_a, U_a_Get),
		get_var(LETENV, u_expected, U_expected_Get),
		get_var(LETENV, u_b, U_b_Get),
		get_var(LETENV, u_actual, U_actual_Get),
		get_var(LETENV, u_eqf, U_eqf_Get),
		get_var(LETENV, u_a, U_a_Get84),
		get_var(LETENV, u_b, U_b_Get91),
		get_var(LETENV, u_a, U_a_Get98),
		get_var(LETENV, u_b, U_b_Get105),
		get_var(LETENV, u_expected, U_expected_Get112),
		get_var(LETENV, u_eqf, U_eqf_Get119),
		get_var(LETENV, u_actual, U_actual_Get126),
		[let, [[U_a_Get, U_expected_Get], [U_b_Get, U_actual_Get]], [if, [not, [U_eqf_Get, U_a_Get84, U_b_Get91]], [progn, [format, t, '$ARRAY'([*], claz_base_character, ['#\\'('F'), '#\\'('A'), '#\\'('I'), '#\\'('L'), '#\\'('E'), '#\\'('D'), '#\\'(:), '#\\'(' '), '#\\'(w), '#\\'(h), '#\\'(e), '#\\'(n), '#\\'(' '), '#\\'(m), '#\\'(a), '#\\'(t), '#\\'(c), '#\\'(h), '#\\'(i), '#\\'(n), '#\\'(g), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(' '), '#\\'(a), '#\\'(n), '#\\'(d), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(~), '#\\'('%')]), U_a_Get98, U_b_Get105], [u_prolog_inline, '$ARRAY'([*], claz_base_character, ['#\\'(t), '#\\'(r), '#\\'(a), '#\\'(c), '#\\'(e)])], [quote, [ext_quit, 1]]], [format, t, '$ARRAY'([*], claz_base_character, ['#\\'('O'), '#\\'('K'), '#\\'(:), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(' '), '#\\'(i), '#\\'(s), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(' '), '#\\'(t), '#\\'(o), '#\\'(' '), '#\\'(~), '#\\'(a), '#\\'(~), '#\\'('%')]), [quote, U_expected_Get112], [quote, U_eqf_Get119], [quote, U_actual_Get126]]]]=MResult
	      ),
	      block_exit(u_is, MResult),
	      true),
	cl_eval(MResult, FnResult).
:- set_opv(f_u_is, classof, claz_macro),
   set_opv(u_is, compile_as, kw_operator),
   set_opv(u_is, function, f_u_is).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1476 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['write-line', '$STRING'("Running smoke test!")]).
:- cl_write_line('$ARRAY'([*],
			  claz_base_character,
			  
			  [ '#\\'('R'),
			    '#\\'(u),
			    '#\\'(n),
			    '#\\'(n),
			    '#\\'(i),
			    '#\\'(n),
			    '#\\'(g),
			    '#\\'(' '),
			    '#\\'(s),
			    '#\\'(m),
			    '#\\'(o),
			    '#\\'(k),
			    '#\\'(e),
			    '#\\'(' '),
			    '#\\'(t),
			    '#\\'(e),
			    '#\\'(s),
			    '#\\'(t),
			    '#\\'(!)
			  ]),
		 Write_line_Ret).
/* (progn (prolog-inline "rtrace") (is eq 1 1))*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1559 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, 1]).
:- f_u_is(eq, 1, 1, U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    equal,
			    [list, 1, [quote, a], [quote, b]],
			    [cons, 1, [quote, [a, b]]]
			  ]).
:- f_u_is(equal,
	  [list, 1, [quote, u_a], [quote, u_b]],
	  [cons, 1, [quote, [u_a, u_b]]],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1614 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 2, [if, [], 1, 2]]).
:- f_u_is(eq, 2, [if, [], 1, 2], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1638 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, [keywordp, ':k']]).
:- f_u_is(eq, t, [keywordp, kw_k], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1663 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 10, [if, t, 10, 20]]).
:- f_u_is(eq, 10, [if, t, 10, 20], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1688 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, [stringp, '$STRING'("abc")]]).
:- f_u_is(eq,
	  t,
	  
	  [ stringp,
	    '$ARRAY'([*],
		     claz_base_character,
		     ['#\\'(a), '#\\'(b), '#\\'(c)])
	  ],
	  U_is_Ret).
/*;  "FAI "this has ben fix" LED: when matching ~a and ~a~%", ['#\\'(b), '#\\'(c)], "bc", t).*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    equal,
			    [subseq, '$STRING'("abc"), 1],
			    '$STRING'("bc")
			  ]).
:- f_u_is(equal,
	  
	  [ subseq,
	    '$ARRAY'([*],
		     claz_base_character,
		     ['#\\'(a), '#\\'(b), '#\\'(c)]),
	    1
	  ],
	  '$ARRAY'([*], claz_base_character, ['#\\'(b), '#\\'(c)]),
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1846 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, [if, t, 1, 2]]).
:- f_u_is(eq, 1, [if, t, 1, 2], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1867 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 2, [if, [], 1, 2]]).
:- f_u_is(eq, 2, [if, [], 1, 2], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1891 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    fib,
			    [n],
			    [if, [>, n, 1], [+, [fib, [-, n, 1]], [fib, [-, n, 2]]], 1]
			  ]).
/* 
alphas=[n].
type=ctx.
var_tracker(n)=rw{name:n, p:1, r:3, ret:0, u:0, vars:[N_Param, N_Get, N_Get39, N_Get46], w:1}.
 */

% asserting...
wl:arglist_info(f_u_fib, [n], [N_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[n], opt:0, req:1, rest:0}).

% asserting...
function_lambda(defun(u_fib), f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).

% asserting...
f_u_fib(N_Param, _rPrevRes) :-
	catch(( >(N_Param, 1, IFTEST),
		(   IFTEST\==[]
		->  -(N_Param, 1, C45_Ret),
		    f_u_fib(C45_Ret, U_fib_Ret),
		    -(N_Param, 2, C45_Ret48),
		    f_u_fib(C45_Ret48, U_fib_Ret49),
		    +(U_fib_Ret, U_fib_Ret49, C43_Ret),
		    _rPrevRes=C43_Ret
		;   _rPrevRes=1
		)
	      ),
	      block_exit(u_fib, _rPrevRes),
	      true).
:- set_opv(f_u_fib, classof, claz_compiled_function),
   set_opv(u_fib, compile_as, kw_function),
   set_opv(u_fib, function, f_u_fib).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1972 **********************/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(fib)]).
:- cl_disassemble(function(u_fib), Disassemble_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1994 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eql, 89, [fib, 10]]).
:- f_u_is(eql, 89, [u_fib, 10], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2018 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    accum,
			    [r],
			    [if, [=, 0, r], [list, 0], [cons, r, [accum, [-, r, 1]]]]
			  ]).
/* 
alphas=[u_r].
type=ctx.
var_tracker(u_r)=rw{name:u_r, p:1, r:3, ret:0, u:0, vars:[U_r_Param, U_r_Get, U_r_Get38, U_r_Get42], w:1}.
 */

% asserting...
wl:arglist_info(f_u_accum, [u_r], [U_r_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:1, rest:0}).

% asserting...
function_lambda(defun(u_accum), f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).

% asserting...
f_u_accum(U_r_Param, _rPrevRes) :-
	catch((   0=:=U_r_Param
	      ->  _rPrevRes=[0]
	      ;   -(U_r_Param, 1, C45_Ret),
		  f_u_accum(C45_Ret, U_accum_Ret),
		  _rPrevRes=[U_r_Param|U_accum_Ret]
	      ),
	      block_exit(u_accum, _rPrevRes),
	      true).
:- set_opv(f_u_accum, classof, claz_compiled_function),
   set_opv(u_accum, compile_as, kw_function),
   set_opv(u_accum, function, f_u_accum).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2084 **********************/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(accum)]).
:- cl_disassemble(function(u_accum), Disassemble_Ret).
/* DISASSEMBLY FOR:f_u_accum
:- dynamic f_u_accum/2.

f_u_accum(A, G) :-
	(   0=:=A
	->  G=[0]
	;   C is A - 1,
	    f_u_accum(C, D),
	    G=[A|D]
	).

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2261 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, equal, [list, 4, 3, 2, 1, 0], [accum, 4]]).
:- f_u_is(equal, [list, 4, 3, 2, 1, 0], [u_accum, 4], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2300 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    defwrap,
			    [name],
			    ['#BQ', [defun, ['#COMMA', name], [], 1]]
			  ]).
/* 
alphas=[u_name, f_u_defwrap].
type=ctx.
var_tracker(u_name)=rw{name:u_name, p:1, r:0, ret:0, u:0, vars:[U_name_Param], w:1}.
 */

% asserting...
wl:arglist_info(f_u_defwrap, [u_name], [U_name_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_name], opt:0, req:1, rest:0}).

% asserting...
macro_lambda(defmacro(u_defwrap), f_u_defwrap, [u_name], [progn, ['#BQ', [defun, ['#COMMA', u_name], [], 1]]], [u_name, f_u_defwrap]).

% asserting...
f_u_defwrap(U_name_Param, FnResult) :-
	Env=[bv(u_name, U_name_Param)],
	catch(( get_var(Env, u_name, U_name_Get),
		[defun, U_name_Get, [], 1]=MResult
	      ),
	      block_exit(u_defwrap, MResult),
	      true),
	cl_eval(MResult, FnResult).
:- set_opv(f_u_defwrap, classof, claz_macro),
   set_opv(u_defwrap, compile_as, kw_operator),
   set_opv(u_defwrap, function, f_u_defwrap).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2346 **********************/
:- lisp_compile_to_prolog(pkg_user, [defwrap, foo]).
:- f_u_defwrap(u_foo, U_defwrap_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2360 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, [foo]]).
:- f_u_is(eq, 1, [u_foo], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2376 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    equal,
			    ['macroexpand-1', [quote, [defwrap, foo]]],
			    [quote, [defun, foo, [], 1]]
			  ]).
:- f_u_is(equal,
	  [macroexpand_1, [quote, [u_defwrap, u_foo]]],
	  [quote, [defun, u_foo, [], 1]],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2438 **********************/
:- lisp_compile_to_prolog(pkg_user, ['write-line', '$STRING'("PASSED")]).
:- cl_write_line('$ARRAY'([*],
			  claz_base_character,
			  
			  [ '#\\'('P'),
			    '#\\'('A'),
			    '#\\'('S'),
			    '#\\'('S'),
			    '#\\'('E'),
			    '#\\'('D')
			  ]),
		 Write_line_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2461 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    fifteen,
			    [],
			    
			    [ let,
			      [val],
			      
			      [ tagbody,
				[setq, val, 1],
				[go, 'point-a'],
				[incf, val, 16],
				'point-c',
				[incf, val, 4],
				[go, 'point-b'],
				[incf, val, 32],
				'point-a',
				'point-u',
				[incf, val, 2],
				[go, 'point-c'],
				[incf, val, 64],
				'point-b',
				[incf, val, 8]
			      ],
			      val
			    ]
			  ]).
/* 
alphas=[u_val].
type=ctx.
var_tracker(u_val)=rw{name:u_val, p:0, r:1, ret:0, u:0, vars:[U_val_Get], w:5}.
 */

% asserting...
wl:arglist_info(f_u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0}).

% asserting...
function_lambda(defun(u_fifteen), f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]).

% asserting...
f_u_fifteen(MResult) :-
	Env=[],
	catch(( TBEnv=[[bv(u_val, [])]|Env],
		addr_tagbody_4_addr_enter_4(TBEnv),
		get_var(TBEnv, u_val, U_val_Get),
		U_val_Get=MResult
	      ),
	      block_exit(u_fifteen, MResult),
	      true).
:- set_opv(f_u_fifteen, classof, claz_compiled_function),
   set_opv(u_fifteen, compile_as, kw_function),
   set_opv(u_fifteen, function, f_u_fifteen).
/*; unused*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2775 **********************/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(fifteen)]).
:- cl_disassemble(function(u_fifteen), Disassemble_Ret).
/*

/* this first one should get deleted since its inlined away in f_u_fifteen */

addr_tagbody_1_addr_enter_1(Env10) :-
        set_var(Env10, setq, u_val, 1),
        addr_tagbody_1_u_point_a(Env10).
addr_tagbody_1_u_point_c(Incf_Env) :-
        set_place(Incf_Env, incf, [value, u_val], [4], Incf_R),
        addr_tagbody_1_u_point_b(Incf_Env).
addr_tagbody_1_u_point_a(Incf_Env19) :-
        set_place(Incf_Env19, incf, [value, u_val], [2], Incf_R18),
        addr_tagbody_1_u_point_c(Incf_Env19).
addr_tagbody_1_u_point_u(Incf_Env23) :-
        set_place(Incf_Env23, incf, [value, u_val], [2], Incf_R22),
        addr_tagbody_1_u_point_c(Incf_Env23).
addr_tagbody_1_u_point_b(Incf_Env27) :-
        set_place(Incf_Env27, incf, [value, u_val], [8], _GORES15).

f_u_fifteen(MResult) :-
        Env=[],
        catch(( TBEnv=[[bv(u_val, [])]|Env],
                set_var(TBEnv, setq, u_val, 1),
                addr_tagbody_1_u_point_a(TBEnv),
                get_var(TBEnv, u_val, U_val_Get),
                U_val_Get=MResult
              ),
              block_exit(u_fifteen, MResult),
              true).

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:3933 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 15, [fifteen]]).
:- f_u_is(eq, 15, [u_fifteen], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:3956 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eq,
			    [quote, string_l],
			    
			    [ 'DEFUN',
			      string_l,
			      [x],
			      
			      [ 'COND',
				[['STRINGP', x], x],
				[['SYMBOLP', x], ['symbol-name', x]],
				['T', ['ERROR', '$STRING'("type error")]]
			      ]
			    ]
			  ]).
:- f_u_is(eq,
	  [quote, u_string_l],
	  
	  [ defun,
	    u_string_l,
	    [u_x],
	    
	    [ cond,
	      [[stringp, u_x], u_x],
	      [[symbolp, u_x], [symbol_name, u_x]],
	      
	      [ t,
		
		[ error,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ '#\\'(t),
			     '#\\'(y),
			     '#\\'(p),
			     '#\\'(e),
			     '#\\'(' '),
			     '#\\'(e),
			     '#\\'(r),
			     '#\\'(r),
			     '#\\'(o),
			     '#\\'(r)
			   ])
		]
	      ]
	    ]
	  ],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4075 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], ['TAGBODY', 1, ['PRINT', '$STRING'("hi")]]]).
:- f_u_is(eq,
	  [],
	  
	  [ tagbody,
	    1,
	    [print, '$ARRAY'([*], claz_base_character, ['#\\'(h), '#\\'(i)])]
	  ],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4113 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], ['TAGBODY', a, ['PRINT', '$STRING'("hi")]]]).
:- f_u_is(eq,
	  [],
	  
	  [ tagbody,
	    u_a,
	    [print, '$ARRAY'([*], claz_base_character, ['#\\'(h), '#\\'(i)])]
	  ],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4151 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]], []]]).
:- f_u_is(eq, [], [let, [[u_val, 1]], []], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4183 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]]]]).
:- f_u_is(eq, [], [let, [[u_val, 1]]], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4213 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eql, 1, ['LET', [[val, 1]], val]]).
:- f_u_is(eql, 1, [let, [[u_val, 1]], u_val], U_is_Ret).
/*; 3.1. Review of defstruct*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4276 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ progn,
			    ['prolog-inline', '$STRING'("nop(trace)")],
			    [is, eq, [quote, point], [defstruct, point, x, y, z]]
			  ]).
:- nop(trace),
   f_u_is(eq, [quote, u_point], [defstruct, u_point, u_x, u_y, u_z], U_is_Ret).
/*; (defstruct point x y z)*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4388 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point4d], [defstruct, point4d, x, y, z, t]]).
:- f_u_is(eq, [quote, u_point4d], [defstruct, u_point4d, u_x, u_y, u_z, t], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4434 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'distance-from-origin',
			    [point],
			    
			    [ 'let*',
			      
			      [ [x, ['point-x', point]],
				[y, ['point-y', point]],
				[z, ['point-z', point]]
			      ],
			      [sqrt, [+, [*, x, x], [*, y, y], [*, z, z]]]
			    ]
			  ]).
/* 
alphas=[u_z, u_y, u_x, u_point].
type=ctx.
var_tracker(u_point)=rw{name:u_point, p:1, r:3, ret:0, u:0, vars:[U_point_Param, U_point_Get, U_point_Get41, U_point_Get53], w:1}.
var_tracker(u_x)=rw{name:u_x, p:0, r:2, ret:0, u:0, vars:[U_x_Get72, U_x_Get72], w:0}.
var_tracker(u_y)=rw{name:u_y, p:0, r:2, ret:0, u:0, vars:[U_y_Get78, U_y_Get78], w:0}.
var_tracker(u_z)=rw{name:u_z, p:0, r:2, ret:0, u:0, vars:[U_z_Get86, U_z_Get86], w:0}.
 */

% asserting...
wl:arglist_info(f_u_distance_from_origin, [u_point], [U_point_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:1, rest:0}).

% asserting...
function_lambda(defun(u_distance_from_origin), f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).

% asserting...
f_u_distance_from_origin(U_point_Param, MResult) :-
	Env=[bv(u_point, U_point_Param)],
	catch(( f_u_point_x(U_point_Param, U_point_x_Ret),
		LETENV=[[bv(u_x, U_point_x_Ret)]|Env],
		f_u_point_y(U_point_Param, U_point_y_Ret),
		LETENV45=[[bv(u_y, U_point_y_Ret)]|LETENV],
		f_u_point_z(U_point_Param, U_point_z_Ret),
		LETENV57=[[bv(u_z, U_point_z_Ret)]|LETENV45],
		get_var(LETENV57, u_x, U_x_Get72),
		*(U_x_Get72, U_x_Get72, Xx_Ret),
		get_var(LETENV57, u_y, U_y_Get78),
		*(U_y_Get78, U_y_Get78, Xx_Ret79),
		+(Xx_Ret, Xx_Ret79, C43_Ret),
		get_var(LETENV57, u_z, U_z_Get86),
		*(U_z_Get86, U_z_Get86, Xx_Ret87),
		+(C43_Ret, Xx_Ret87, C43_Ret88),
		cl_sqrt(C43_Ret88, Sqrt_Ret),
		Sqrt_Ret=MResult
	      ),
	      block_exit(u_distance_from_origin, MResult),
	      true).
:- set_opv(f_u_distance_from_origin, classof, claz_compiled_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4600 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reflect-in-y-axis',
			    [point],
			    [setf, ['point-y', point], [-, ['point-y', point]]]
			  ]).
/* 
alphas=[u_point].
type=ctx.
var_tracker(u_point)=rw{name:u_point, p:1, r:3, ret:0, u:0, vars:[U_point_Param, U_point_Get, U_point_Get38], w:1}.
 */

% asserting...
wl:arglist_info(f_u_reflect_in_y_axis, [u_point], [U_point_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:1, rest:0}).

% asserting...
function_lambda(defun(u_reflect_in_y_axis), f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).

% asserting...
f_u_reflect_in_y_axis(U_point_Param, MResult) :-
	Setf_Env=[bv(u_point, U_point_Param)],
	catch(( f_u_point_y(U_point_Param, U_point_y_Ret),
		-(0, U_point_y_Ret, C45_Ret),
		set_place(Setf_Env,
			 setf,
			 [u_point_y, U_point_Param],
			 [C45_Ret],
			 Setf_R),
		Setf_R=MResult
	      ),
	      block_exit(u_reflect_in_y_axis, MResult),
	      true).
:- set_opv(f_u_reflect_in_y_axis, classof, claz_compiled_function),
   set_opv(u_reflect_in_y_axis, compile_as, kw_function),
   set_opv(u_reflect_in_y_axis, function, f_u_reflect_in_y_axis).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ list,
			    
			    [ setf,
			      'my-point',
			      ['make-point', ':x', 3, ':y', 4, ':z', 12]
			    ],
			    
			    [ setf,
			      'my-point2',
			      ['make-point', ':x', 3, ':y', 4, ':z', 12]
			    ]
			  ]).
:- f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], U_make_point_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [U_make_point_Ret], Setf_R),
   f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], U_make_point_Ret34),
   set_place(TLEnv, setf, [value, u_my_point2], [U_make_point_Ret34], Setf_R35),
   List_Ret=[Setf_R, Setf_R35].

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4786 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-point3',
			    '$S'(['POINT', ':X', 3, ':Y', 4, ':Z', 12])
			  ]).
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], _14554),
   set_place(TLEnv, setf, [value, u_my_point3], [_14554], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-point4d',
			    ['make-point4d', ':x', 3, ':y', 4, ':z', 12, ':t', 1]
			  ]).
:- f_u_make_point4d([kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1], U_make_point4d_Ret),
   set_place(TLEnv, setf, [value, u_my_point4d], [U_make_point4d_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4885 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, ['point-p', 'my-point']]).
:- f_u_is(eq, t, [u_point_p, u_my_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4915 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point], ['type-of', 'my-point']]).
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], U_is_Ret).
/*flag_removed(+ :IGNORE,[#+,:WAM-CL,[prolog-call,$STRING(break)]])*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4991 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eql,
			    13,
			    
			    [ progn,
			      [print, ['distance-from-origin', 'my-point']]
			    ]
			  ]).
:- f_u_is(eql, 13, [progn, [print, [u_distance_from_origin, u_my_point]]], U_is_Ret).
/*; #+CLISP (BREAK)*/
/*; #+WAM-CL (prolog-call "break")*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5106 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, =, -4, ['reflect-in-y-axis', 'my-point']]).
:- f_u_is(=, -4, [u_reflect_in_y_axis, u_my_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5146 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 'my-point', 'my-point']).
:- f_u_is(eq, u_my_point, u_my_point, U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5173 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'a-similar-point',
			    '$S'([point, ':x', 3, ':y', -4, ':z', 12])
			  ]).
:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], _33278),
   set_place(TLEnv, setf, [value, u_a_similar_point], [_33278], Setf_R).
/* (is eq t (equal my-point a-similar-point))*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5270 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], [eq, 'my-point', 'a-similar-point']]).
:- f_u_is(eq, [], [eq, u_my_point, u_a_similar_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5313 **********************/
:- lisp_compile_to_prolog(pkg_user, [equalp, 'my-point', 'a-similar-point']).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   get_var(TLEnv, u_a_similar_point, U_a_similar_point_Get),
   cl_equalp(U_my_point_Get, U_a_similar_point_Get, Equalp_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5348 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, t, [equalp, 'my-point', 'a-similar-point']]).
:- f_u_is(eq, t, [equalp, u_my_point, u_a_similar_point], U_is_Ret).
/*; 3.2. defclass*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5413 **********************/
:- lisp_compile_to_prolog(pkg_user, [unintern, [quote, point]]).
:- cl_unintern(u_point, Unintern_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5432 **********************/
:- lisp_compile_to_prolog(pkg_user, [defclass, point, [], [x, y, z]]).
:- cl_defclass([u_point, [], [u_x, u_y, u_z]], Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setf, 'my-point', ['make-instance', [quote, point]]]).
:- cl_make_instance([u_point], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_instance_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5509 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point], ['type-of', 'my-point']]).
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5544 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'set-point-values',
			    [point, x, y, z],
			    
			    [ setf,
			      ['slot-value', point, [quote, x]],
			      x,
			      ['slot-value', point, [quote, y]],
			      y,
			      ['slot-value', point, [quote, z]],
			      z
			    ]
			  ]).
/* 
alphas=[u_z, u_y, u_x, u_point].
type=ctx.
var_tracker(u_point)=rw{name:u_point, p:1, r:6, ret:0, u:0, vars:[U_point_Param, U_point_Get, U_point_Get46, U_point_Get51], w:1}.
var_tracker(u_x)=rw{name:u_x, p:1, r:1, ret:0, u:0, vars:[U_x_Param, U_x_Get], w:1}.
var_tracker(u_y)=rw{name:u_y, p:1, r:1, ret:0, u:0, vars:[U_y_Param, U_y_Get], w:1}.
var_tracker(u_z)=rw{name:u_z, p:1, r:1, ret:0, u:0, vars:[U_z_Param, U_z_Get], w:1}.
 */

% asserting...
wl:arglist_info(f_u_set_point_values, [u_point, u_x, u_y, u_z], [U_point_Param, U_x_Param, U_y_Param, U_z_Param], arginfo{all:4, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:4, rest:0}).

% asserting...
function_lambda(defun(u_set_point_values), f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]).

% asserting...
f_u_set_point_values(U_point_Param, U_x_Param, U_y_Param, U_z_Param, MResult) :-
	Setf_Env=[bv(u_point, U_point_Param), bv(u_x, U_x_Param), bv(u_y, U_y_Param), bv(u_z, U_z_Param)],
	catch(( set_place(Setf_Env,
			 setf,
			 [slot_value, U_point_Param, u_x],
			 [U_x_Param],
			 Setf_R),
		set_place(Setf_Env,
			 setf,
			 [slot_value, U_point_Param, u_y],
			 [U_y_Param],
			 Setf_R45),
		set_place(Setf_Env,
			 setf,
			 [slot_value, U_point_Param, u_z],
			 [U_z_Param],
			 Setf_R50),
		Setf_R50=MResult
	      ),
	      block_exit(u_set_point_values, MResult),
	      true).
:- set_opv(f_u_set_point_values, classof, claz_compiled_function),
   set_opv(u_set_point_values, compile_as, kw_function),
   set_opv(u_set_point_values, function, f_u_set_point_values).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5681 **********************/
:- lisp_compile_to_prolog(pkg_user, ['set-point-values', 'my-point', 3, 4, 12]).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   f_u_set_point_values(U_my_point_Get, 3, 4, 12, U_set_point_values_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5717 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'distance-from-origin',
			    [point],
			    
			    [ 'with-slots',
			      [x, y, z],
			      point,
			      [sqrt, [+, [*, x, x], [*, y, y], [*, z, z]]]
			    ]
			  ]).
/* 
alphas=[u_z, u_y, u_x, u_point].
type=ctx.
var_tracker(u_point)=rw{name:u_point, p:1, r:3, ret:0, u:0, vars:[U_point_Param, U_point_Get, U_point_Get35, U_point_Get40], w:1}.
var_tracker(u_x)=rw{name:u_x, p:0, r:2, ret:0, u:0, vars:[U_x_Get56, U_x_Get56], w:0}.
var_tracker(u_y)=rw{name:u_y, p:0, r:2, ret:0, u:0, vars:[U_y_Get62, U_y_Get62], w:0}.
var_tracker(u_z)=rw{name:u_z, p:0, r:2, ret:0, u:0, vars:[U_z_Get70, U_z_Get70], w:0}.
 */

% asserting...
wl:arglist_info(f_u_distance_from_origin, [u_point], [U_point_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:1, rest:0}).

% asserting...
function_lambda(defun(u_distance_from_origin), f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).

% asserting...
f_u_distance_from_origin(U_point_Param, MResult) :-
	Env=[bv(u_point, U_point_Param)],
	catch(( cl_slot_value(U_point_Param, u_x, Slot_value_Ret),
		cl_slot_value(U_point_Param, u_y, Slot_value_Ret37),
		cl_slot_value(U_point_Param, u_z, Slot_value_Ret42),
		LETENV=[[bv(u_x, Slot_value_Ret), bv(u_y, Slot_value_Ret37), bv(u_z, Slot_value_Ret42)]|Env],
		get_var(LETENV, u_x, U_x_Get56),
		*(U_x_Get56, U_x_Get56, Xx_Ret),
		get_var(LETENV, u_y, U_y_Get62),
		*(U_y_Get62, U_y_Get62, Xx_Ret63),
		+(Xx_Ret, Xx_Ret63, C43_Ret),
		get_var(LETENV, u_z, U_z_Get70),
		*(U_z_Get70, U_z_Get70, Xx_Ret71),
		+(C43_Ret, Xx_Ret71, C43_Ret72),
		cl_sqrt(C43_Ret72, Sqrt_Ret),
		Sqrt_Ret=MResult
	      ),
	      block_exit(u_distance_from_origin, MResult),
	      true).
:- set_opv(f_u_distance_from_origin, classof, claz_compiled_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5830 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['DISASSEMBLE', function('distance-from-origin')]).
:- cl_disassemble(function(u_distance_from_origin), Disassemble_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5869 **********************/
:- lisp_compile_to_prolog(pkg_user, ['distance-from-origin', 'my-point']).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   f_u_distance_from_origin(U_my_point_Get, U_distance_from_origin_Ret).
/*; 3.3. classes are objects*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5931 **********************/
:- lisp_compile_to_prolog(pkg_user, ['find-class', [quote, point]]).
:- cl_find_class(u_point, Find_class_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5952 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['class-name', ['find-class', [quote, point]]]).
:- cl_find_class(u_point, Find_class_Ret),
   cl_class_name(Find_class_Ret, Class_name_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5986 **********************/
:- lisp_compile_to_prolog(pkg_user, ['class-of', 'my-point']).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   cl_class_of(U_my_point_Get, Class_of_Ret).
/*; #-(or cormanlisp CLISP WAM-CL)*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6041 **********************/
:- lisp_compile_to_prolog(pkg_user, [typep, 'my-point', ['class-of', 'my-point']]).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   get_var(TLEnv, u_my_point, U_my_point_Get20),
   cl_class_of(U_my_point_Get20, Class_of_Ret),
   cl_typep(U_my_point_Get, Class_of_Ret, Typep_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6079 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eq,
			    ['find-class', [quote, 'STANDARD-CLASS']],
			    ['class-of', ['class-of', 'my-point']]
			  ]).
:- f_u_is(eq,
	  [find_class, [quote, standard_class]],
	  [class_of, [class_of, u_my_point]],
	  U_is_Ret).
/*; 3.4. you don't need clos to use clos*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6196 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ let,
			    
			    [ 
			      [ 'the-symbol-class',
				['find-class', [quote, symbol]]
			      ]
			    ],
			    
			    [ values,
			      'the-symbol-class',
			      ['class-name', 'the-symbol-class'],
			      
			      [ eq,
				'the-symbol-class',
				['class-of', [quote, symbol]]
			      ],
			      ['class-of', 'the-symbol-class']
			    ]
			  ]).
:- cl_find_class(symbol, Find_class_Ret),
   LETENV=[[bv(u_the_symbol_class, Find_class_Ret)]|TLEnv],
   get_var(LETENV, u_the_symbol_class, U_the_symbol_class_Get),
   get_var(LETENV, u_the_symbol_class, U_the_symbol_class_Get28),
   cl_class_name(U_the_symbol_class_Get28, Class_name_Ret),
   get_var(LETENV, u_the_symbol_class, U_the_symbol_class_Get32),
   cl_class_of(symbol, Class_of_Ret),
   cl_eq(U_the_symbol_class_Get32, Class_of_Ret, Eq_Ret),
   get_var(LETENV, u_the_symbol_class, U_the_symbol_class_Get39),
   cl_class_of(U_the_symbol_class_Get39, Class_of_Ret40),
   nb_setval('$mv_return',
	     [U_the_symbol_class_Get, Class_name_Ret, Eq_Ret, Class_of_Ret40]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6402 **********************/
:- lisp_compile_to_prolog(pkg_user, ['find-class', t]).
:- cl_find_class(t, Find_class_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6418 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [quote, foo], [defstruct, foo]]).
:- f_u_is(eq, [quote, u_foo], [defstruct, u_foo], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6448 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eq,
			    ['find-class', [quote, foo]],
			    ['class-of', ['make-foo']]
			  ]).
:- f_u_is(eq, [find_class, [quote, u_foo]], [class_of, [u_make_foo]], U_is_Ret).
/*; 3.5 slots*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6511 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defclass,
			    'daft-point',
			    [],
			    
			    [ [x, ':accessor', 'daft-x', ':initarg', ':x'],
			      [y, ':accessor', 'daft-y', ':initform', 3.14159],
			      [z, ':reader', 'daft-z', ':allocation', ':class']
			    ]
			  ]).
:- cl_defclass(
	       [ u_daft_point,
		 [],
		 
		 [ [u_x, kw_accessor, u_daft_x, kw_initarg, kw_x],
		   [u_y, kw_accessor, u_daft_y, kw_initform, 3.14159],
		   [u_z, kw_reader, u_daft_z, kw_allocation, kw_class]
		 ]
	       ],
	       Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6657 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    
			    [ 'slot-value',
			      ['make-instance', [quote, 'daft-point']],
			      [quote, z]
			    ],
			    42
			  ]).
:- cl_make_instance([u_daft_point], Make_instance_Ret),
   set_place(TLEnv, setf, [slot_value, Make_instance_Ret, u_z], [42], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6712 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-daft-point',
			    ['make-instance', [quote, 'daft-point'], ':x', 19]
			  ]).
:- cl_make_instance([u_daft_point, kw_x, 19], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_daft_point], [Make_instance_Ret], Setf_R).
/*flag_removed(+ :PERFECT,[list,[daft-x,my-daft-point],[daft-y,my-daft-point],[progn,[#+,:WAM-CL,[prolog-trace]],[daft-z,my-daft-point]]])*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6901 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ let,
			    [[temp, ['make-instance', [quote, 'daft-point']]]],
			    
			    [ setf,
			      ['daft-y', temp],
			      999,
			      ['slot-value', temp, [quote, z]],
			      0
			    ]
			  ]).
:- cl_make_instance([u_daft_point], Make_instance_Ret),
   Setf_Env=[[bv(u_temp, Make_instance_Ret)]|TLEnv],
   get_var(Setf_Env, u_temp, U_temp_Get),
   set_place(Setf_Env, setf, [u_daft_y, U_temp_Get], [999], Setf_R),
   get_var(Setf_Env, u_temp, U_temp_Get32),
   set_place(Setf_Env, setf, [slot_value, U_temp_Get32, u_z], [0], Setf_R31).
/*flag_removed(+ :PERFECT,[list,[daft-x,my-daft-point],[daft-y,my-daft-point],[daft-z,my-daft-point]])*/
/*; 3.6 Subclasses and inheritance*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7137 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defclass,
			    animal,
			    [],
			    
			    [ [legs, ':reader', 'leg-count', ':initarg', ':legs'],
			      
			      [ 'comes-from',
				':reader',
				'comes-from',
				':initarg',
				':comes-from'
			      ]
			    ]
			  ]).
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
	       Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7259 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defclass,
			    mammal,
			    [animal],
			    
			    [ 
			      [ diet,
				':initform',
				[quote, antelopes],
				':initarg',
				':diet'
			      ]
			    ]
			  ]).
:- cl_defclass(
	       [ u_mammal,
		 [u_animal],
		 [[u_diet, kw_initform, [quote, u_antelopes], kw_initarg, kw_diet]]
	       ],
	       Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7334 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defclass,
			    aardvark,
			    [mammal],
			    [['cute-p', ':accessor', 'cute-p', ':initform', []]]
			  ]).
:- cl_defclass(
	       [ u_aardvark,
		 [u_mammal],
		 [[u_cute_p, kw_accessor, u_cute_p, kw_initform, []]]
	       ],
	       Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7408 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-direct-superclasses',
			    ['find-class', [quote, aardvark]]
			  ]).
:- cl_find_class(u_aardvark, Find_class_Ret),
   f_clos_class_direct_superclasses(Find_class_Ret,
				    Clos_class_direct_superclasses_Ret).
/*; ACL needs to instantiate a class before its precedence-list becomes visible*/
/*; #+allegro*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7608 **********************/
:- lisp_compile_to_prolog(pkg_user, ['make-instance', [quote, aardvark]]).
:- cl_make_instance([u_aardvark], Make_instance_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7635 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-precedence-list',
			    ['find-class', [quote, aardvark]]
			  ]).
:- cl_find_class(u_aardvark, Find_class_Ret),
   f_clos_class_precedence_list(Find_class_Ret, Clos_class_precedence_list_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7735 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defclass,
			    figurine,
			    [],
			    
			    [ 
			      [ potter,
				':accessor',
				'made-by',
				':initarg',
				':made-by'
			      ],
			      ['comes-from', ':initarg', ':made-in']
			    ]
			  ]).
:- cl_defclass(
	       [ u_figurine,
		 [],
		 
		 [ [u_potter, kw_accessor, u_made_by, kw_initarg, kw_made_by],
		   [u_comes_from, kw_initarg, kw_made_in]
		 ]
	       ],
	       Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7842 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defclass,
			    'figurine-aardvark',
			    [aardvark, figurine],
			    
			    [ 
			      [ name,
				':reader',
				'aardvark-name',
				':initarg',
				':aardvark-name'
			      ],
			      [diet, ':initform', []]
			    ]
			  ]).
:- cl_defclass(
	       [ u_figurine_aardvark,
		 [u_aardvark, u_figurine],
		 
		 [ 
		   [ u_name,
		     kw_reader,
		     u_aardvark_name,
		     kw_initarg,
		     kw_aardvark_name
		   ],
		   [u_diet, kw_initform, []]
		 ]
	       ],
	       Defclass_Ret).
/*; ACL needs to instantiate a class before its precedence-list becomes visible*/
/*; #+allegro */

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8066 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['make-instance', [quote, 'figurine-aardvark']]).
:- cl_make_instance([u_figurine_aardvark], Make_instance_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8102 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-precedence-list',
			    ['find-class', [quote, 'figurine-aardvark']]
			  ]).
:- cl_find_class(u_figurine_aardvark, Find_class_Ret),
   f_clos_class_precedence_list(Find_class_Ret, Clos_class_precedence_list_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8221 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'Eric',
			    
			    [ 'make-instance',
			      [quote, 'figurine-aardvark'],
			      ':legs',
			      4,
			      ':made-by',
			      '$STRING'("Jen"),
			      ':made-in',
			      '$STRING'("Brittany"),
			      ':aardvark-name',
			      '$STRING'("Eric")
			    ]
			  ]).
:- cl_make_instance(
		    [ u_figurine_aardvark,
		      kw_legs,
		      4,
		      kw_made_by,
		      '$ARRAY'([*],
			       claz_base_character,
			       ['#\\'('J'), '#\\'(e), '#\\'(n)]),
		      kw_made_in,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ '#\\'('B'),
				 '#\\'(r),
				 '#\\'(i),
				 '#\\'(t),
				 '#\\'(t),
				 '#\\'(a),
				 '#\\'(n),
				 '#\\'(y)
			       ]),
		      kw_aardvark_name,
		      '$ARRAY'([*],
			       claz_base_character,
			       ['#\\'('E'), '#\\'(r), '#\\'(i), '#\\'(c)])
		    ],
		    Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_eric], [Make_instance_Ret], Setf_R).
/*flag_removed(+ :HAS_SHIFTF,[shiftf,[cute-p,Eric],t])*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8477 **********************/
:- lisp_compile_to_prolog(pkg_user, ['slot-value', 'Eric', [quote, diet]]).
:- get_var(TLEnv, u_eric, U_eric_Get),
   cl_slot_value(U_eric_Get, u_diet, Slot_value_Ret).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\n;; $Id: examples.lisp,v 1.1 2003/10/21 17:30:56 nhabedi Exp $\n;;                          EXAMPLES.LISP\n;;           Nick Levine, Ravenbrook Limited, 2003-08-14\n;; \n;; These are the examples I expect to use in the tutorial on CLOS\n;; at the International Lisp Conference 2003.\n;; \n;; This document is mainly for my operational convenience. You might\n;; want to raid fragments to help you get started when building CLOS\n;; into your Common Lisp applications. Nothing useful will happen if\n;; you try to cl:load this document into a lisp image.\n;;\n;; This document is provided \"as is\", without any express or implied\n;; warranty.  In no event will the author be held liable for any\n;; damages arising from the use of this document.  You may make and\n;; distribute verbatim copies of this document provided that you do\n;; not charge a fee for this document or for its distribution.\n",
				     2,
				     2)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:886 **********************/
:- lisp_compile_to_prolog(pkg_user, ['in-package', '$STRING'("CL-USER")]).
:- cl_in_package('$ARRAY'([*], claz_base_character, "CL-USER"), In_package_Ret).

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
var_tracker(u_func)=rw{name:u_func, p:1, r:2, ret:0, u:0, vars:[U_func_Param, U_func_Get, U_func_Get48], w:1}.
var_tracker(u_l)=rw{name:u_l, p:1, r:3, ret:0, u:0, vars:[U_l_Param, IFTEST, U_l_Get42, U_l_Get52], w:1}.
 */

% asserting... u_mapcar_visualize 
wl:arglist_info(u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [U_func_Param, U_l_Param], arginfo{all:2, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:2, rest:0}).

% asserting... u_mapcar_visualize 
lambda_def(defun,(u_mapcar_visualize), f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]).

% asserting... u_mapcar_visualize 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:941 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; Test macro", 1, 1055)).
:- true.

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

% asserting... u_is 
wl:arglist_info(u_is, f_u_is, [u_eqf, u_expected, u_actual], [U_eqf_Param, U_expected_Param, U_actual_Param], arginfo{all:3, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:3, rest:0}).

% asserting... u_is 
lambda_def(defmacro,(u_is), f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [not, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")], [quote, [ext_quit, 1]]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]]]]]]], [u_b, u_a, u_actual, u_expected, u_eqf, f_u_is]).

% asserting... u_is 
f_u_is(U_eqf_Param, U_expected_Param, U_actual_Param, FnResult) :-
	Env=[bv(u_eqf, U_eqf_Param), bv(u_expected, U_expected_Param), bv(u_actual, U_actual_Param)],
	catch(( cl_gensym('$ARRAY'([*], claz_base_character, "a"), Gensym_Ret),
		cl_gensym('$ARRAY'([*], claz_base_character, "b"), Gensym_Ret36),
		LEnv=[[bv(u_a, Gensym_Ret), bv(u_b, Gensym_Ret36)]|Env],
		get_var(LEnv, u_a, U_a_Get),
		get_var(LEnv, u_actual, U_actual_Get),
		( get_var(LEnv, u_a, U_a_Get83),
		  get_var(LEnv, u_b, U_b_Get)
		),
		( get_var(LEnv, u_a, U_a_Get97),
		  get_var(LEnv, u_expected, U_expected_Get)
		),
		get_var(LEnv, u_actual, U_actual_Get125),
		( get_var(LEnv, u_b, U_b_Get90),
		  get_var(LEnv, u_eqf, U_eqf_Get)
		),
		get_var(LEnv, u_eqf, U_eqf_Get118),
		get_var(LEnv, u_expected, U_expected_Get111),
		[let, [[U_a_Get, U_expected_Get], [U_b_Get, U_actual_Get]], [if, [not, [U_eqf_Get, U_a_Get83, U_b_Get90]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), U_a_Get97, U_b_Get90], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")], [quote, [ext_quit, 1]]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, U_expected_Get111], [quote, U_eqf_Get118], [quote, U_actual_Get125]]]]=MResult
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
:- cl_write_line('$ARRAY'([*], claz_base_character, "Running smoke test!"),
		 Write_line_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1476 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (progn (prolog-inline \"rtrace\") (is eq 1 1))",
				     1,
				     1513)).
:- true.

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
:- f_u_is(eq, t, [stringp, '$ARRAY'([*], claz_base_character, "abc")], U_is_Ret).
	  
/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).",
				     1,
				     1716)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:1812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    equal,
			    [subseq, '$STRING'("abc"), 1],
			    '$STRING'("bc")
			  ]).
:- f_u_is(equal,
	  [subseq, '$ARRAY'([*], claz_base_character, "abc"), 1],
	  '$ARRAY'([*], claz_base_character, "bc"),
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
var_tracker(n)=rw{name:n, p:1, r:3, ret:0, u:0, vars:[N_Param, N_Get, N_Get37, N_Get44], w:1}.
 */

% asserting... u_fib 
wl:arglist_info(u_fib, f_u_fib, [n], [N_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[n], opt:0, req:1, rest:0}).

% asserting... u_fib 
lambda_def(defun,(u_fib), f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).

% asserting... u_fib 
f_u_fib(N_Param, _rPrevRes) :-
	catch((   N_Param>1
		->  -(N_Param, 1, C45_Ret),
		    f_u_fib(C45_Ret, U_fib_Ret),
		  -(N_Param, 2, C45_Ret46),
		  f_u_fib(C45_Ret46, U_fib_Ret47),
		  +(U_fib_Ret, U_fib_Ret47, C43_Ret),
		    _rPrevRes=C43_Ret
		;   _rPrevRes=1
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

% asserting... u_accum 
wl:arglist_info(u_accum, f_u_accum, [u_r], [U_r_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:1, rest:0}).

% asserting... u_accum 
lambda_def(defun,(u_accum), f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).

% asserting... u_accum 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2084 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" DISASSEMBLY FOR:f_u_accum\n:- dynamic f_u_accum/2.\n\nf_u_accum(A, G) :-\n\t(   0=:=A\n\t->  G=[0]\n\t;   C is A - 1,\n\t    f_u_accum(C, D),\n\t    G=[A|D]\n\t).\n\n",
				     2,
				     2108)).
:- true.

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
alphas=[sys_name, f_u_defwrap].
type=ctx.
var_tracker(sys_name)=rw{name:sys_name, p:1, r:0, ret:0, u:0, vars:[Sys_name_Param], w:1}.
 */

% asserting... u_defwrap 
wl:arglist_info(u_defwrap, f_u_defwrap, [sys_name], [Sys_name_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:1, rest:0}).

% asserting... u_defwrap 
lambda_def(defmacro,(u_defwrap), f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]], [sys_name, f_u_defwrap]).

% asserting... u_defwrap 
f_u_defwrap(Sys_name_Param, FnResult) :-
	Env=[bv(sys_name, Sys_name_Param)],
	catch(( get_var(Env, sys_name, Sys_name_Get),
		[defun, Sys_name_Get, [], 1]=MResult
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
:- cl_write_line('$ARRAY'([*], claz_base_character, "PASSED"), Write_line_Ret).

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

% asserting... u_fifteen 
addr_tagbody_3_addr_enter_3(Env) :-
	set_var(Env, setq, u_val, 1),
	addr_tagbody_3_u_point_a(Env).

% asserting... u_fifteen 
addr_tagbody_3_u_point_c(Env) :-
	set_place(Env, incf, u_val, 4, _rPrevRes),
	addr_tagbody_3_u_point_b(Env).

% asserting... u_fifteen 
addr_tagbody_3_u_point_a(Env) :-
	set_place(Env, incf, u_val, 2, _rPrevRes63),
	addr_tagbody_3_u_point_c(Env).

% asserting... u_fifteen 
addr_tagbody_3_u_point_u(Env) :-
	set_place(Env, incf, u_val, 2, _rPrevRes76),
	addr_tagbody_3_u_point_c(Env).

% asserting... u_fifteen 
addr_tagbody_3_u_point_b(_rEnv83) :-
	set_place(_rEnv83, incf, u_val, 8, _GORES50).
/* 
alphas=[u_val].
type=ctx.
var_tracker(u_val)=rw{name:u_val, p:0, r:1, ret:0, u:0, vars:[U_val_Get], w:5}.
 */

% asserting... u_fifteen 
wl:arglist_info(u_fifteen, f_u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0}).

% asserting... u_fifteen 
lambda_def(defun,(u_fifteen), f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]).

% asserting... u_fifteen 
f_u_fifteen(MResult) :-
	Env=[],
	catch(( TBEnv=[[bv(u_val, [])]|Env],
		set_var(TBEnv, setq, u_val, 1),
		addr_tagbody_3_u_point_a(TBEnv),
		nop(TBRet=[]),
		get_var(TBEnv, u_val, U_val_Get),
		U_val_Get=MResult
	      ),
	      block_exit(u_fifteen, MResult),
	      true).
:- set_opv(f_u_fifteen, classof, claz_compiled_function),
   set_opv(u_fifteen, compile_as, kw_function),
   set_opv(u_fifteen, function, f_u_fifteen).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2461 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; unused", 14, 2662)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2775 **********************/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(fifteen)]).
:- cl_disassemble(function(u_fifteen), Disassemble_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:2775 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\n\n/* this first one should get deleted since its inlined away in f_u_fifteen */\n\naddr_tagbody_1_addr_enter_1(Env10) :-\n        symbol_setter(Env10, setq, u_val, 1),\n        addr_tagbody_1_u_point_a(Env10).\naddr_tagbody_1_u_point_c(Incf_Env) :-\n        place_op(Incf_Env, incf, [value, u_val], [4], Incf_R),\n        addr_tagbody_1_u_point_b(Incf_Env).\naddr_tagbody_1_u_point_a(Incf_Env19) :-\n        place_op(Incf_Env19, incf, [value, u_val], [2], Incf_R18),\n        addr_tagbody_1_u_point_c(Incf_Env19).\naddr_tagbody_1_u_point_u(Incf_Env23) :-\n        place_op(Incf_Env23, incf, [value, u_val], [2], Incf_R22),\n        addr_tagbody_1_u_point_c(Incf_Env23).\naddr_tagbody_1_u_point_b(Incf_Env27) :-\n        place_op(Incf_Env27, incf, [value, u_val], [8], _GORES15).\n\nf_u_fifteen(MResult) :-\n        Env=[],\n        catch(( TBEnv=[[bv(u_val, [])]|Env],\n                symbol_setter(TBEnv, setq, u_val, 1),\n                addr_tagbody_1_u_point_a(TBEnv),\n                symbol_value(TBEnv, u_val, U_val_Get),\n                U_val_Get=MResult\n              ),\n              block_exit(u_fifteen, MResult),\n              true).\n\n",
				     2,
				     2802)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:3933 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 15, [fifteen]]).
:- f_u_is(eq, 15, [u_fifteen], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:3955 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'do-four',
			    [],
			    
			    [ 'DO',
			      
			      [ ['temp-one', 1, ['1+', 'temp-one']],
				['temp-two', 0, ['1-', 'temp-two']]
			      ],
			      [[>, [-, 'temp-one', 'temp-two'], 5], 'temp-one'],
			      []
			    ]
			  ]).

% asserting... u_do_four 
addr_tagbody_4_addr_enter_4(BlockExitEnv) :-
	get_var(BlockExitEnv, u_temp_one, U_temp_one_Get56),
	get_var(BlockExitEnv, u_temp_two, U_temp_two_Get76),
	-(U_temp_one_Get56, U_temp_two_Get76, C45_Ret),
	(   C45_Ret>5
	->  throw(block_exit([], U_temp_one_Get56)),
	    _rPrevRes140=ThrowResult
	;   '1+'(U_temp_one_Get56, D1_c43_Ret),
	    '1-'(U_temp_two_Get76, D1c45_Ret),
	    set_var(BlockExitEnv, u_temp_one, D1_c43_Ret),
	    set_var(BlockExitEnv, u_temp_two, D1c45_Ret),
	    addr_tagbody_4_do_label_u_do_four2(BlockExitEnv),
	    _rPrevRes140=_GORES
	).

% asserting... u_do_four 
addr_tagbody_4_do_label_u_do_four2(BlockExitEnv) :-
	get_var(BlockExitEnv, u_temp_one, U_temp_one_Get96),
	get_var(BlockExitEnv, u_temp_two, U_temp_two_Get99),
	-(U_temp_one_Get96, U_temp_two_Get99, C45_Ret100),
	(   C45_Ret100>5
	->  get_var(BlockExitEnv, u_temp_one, RetResult105),
	    throw(block_exit([], RetResult105)),
	    _rPrevRes140=ThrowResult106
	;   get_var(BlockExitEnv, u_temp_one, U_temp_one_Get126),
	    '1+'(U_temp_one_Get126, D1_c43_Ret127),
	    get_var(BlockExitEnv, u_temp_two, U_temp_two_Get130),
	    '1-'(U_temp_two_Get130, D1c45_Ret131),
	    set_var(BlockExitEnv, u_temp_one, D1_c43_Ret127),
	    set_var(BlockExitEnv, u_temp_two, D1c45_Ret131),
	    addr_tagbody_4_do_label_u_do_four2(BlockExitEnv),
	    _rPrevRes140=_GORES134
	).
/*
alphas=[u_temp_two, u_temp_one].
type=ctx.
var_tracker(u_temp_one)=rw{name:u_temp_one, p:0, r:6, ret:0, u:0, vars:[U_temp_one_Get56, U_temp_one_Get96, RetResult105, U_temp_one_Get126], w:2}.
var_tracker(u_temp_two)=rw{name:u_temp_two, p:0, r:4, ret:0, u:0, vars:[U_temp_two_Get76, U_temp_two_Get99, U_temp_two_Get130], w:2}.
 */

% asserting... u_do_four 
wl:arglist_info(u_do_four, f_u_do_four, [], [], arginfo{all:0, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0}).

% asserting... u_do_four 
lambda_def(defun,(u_do_four), f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]).

% asserting... u_do_four 
f_u_do_four(MResult) :-
        Env=[],
	catch(( LEnv=[[bv(u_temp_one, 1), bv(u_temp_two, 0)]|Env],
		catch(( get_var(LEnv, u_temp_one, U_temp_one),
			get_var(LEnv, u_temp_two, U_temp_two),
			-(U_temp_one, U_temp_two, _12980),
			(   _12980>5
			->  throw(block_exit([], U_temp_one)),
			    _12982=_12984
			;   '1+'(U_temp_one, Set_var_Ret),
			    '1-'(U_temp_two, Set_var_Ret157),
			    set_var(LEnv, u_temp_one, Set_var_Ret),
			    set_var(LEnv, u_temp_two, Set_var_Ret157),
			    addr_tagbody_4_do_label_u_do_four2(LEnv),
			    _12990=_12992
              ),
			nop(TBRet=[]),
			TBRet=_rPrevRes147
		      ),
		      block_exit([], _rPrevRes147),
		      true),
		_rPrevRes147=MResult
	      ),
	      block_exit(u_do_four, MResult),
              true).
:- set_opv(f_u_do_four, classof, claz_compiled_function),
   set_opv(u_do_four, compile_as, kw_function),
   set_opv(u_do_four, function, f_u_do_four).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4079 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, =, 4, ['do-four']]).
:- f_u_is(=, 4, [u_do_four], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4100 **********************/
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
	      [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]
	    ]
	  ],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4219 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], ['TAGBODY', 1, ['PRINT', '$STRING'("hi")]]]).
:- f_u_is(eq,
	  [],
	  [tagbody, 1, [print, '$ARRAY'([*], claz_base_character, "hi")]],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4257 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], ['TAGBODY', a, ['PRINT', '$STRING'("hi")]]]).
:- f_u_is(eq,
	  [],
	  [tagbody, u_a, [print, '$ARRAY'([*], claz_base_character, "hi")]],
	  U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4295 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]], []]]).
:- f_u_is(eq, [], [let, [[u_val, 1]], []], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4327 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]]]]).
:- f_u_is(eq, [], [let, [[u_val, 1]]], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4357 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eql, 1, ['LET', [[val, 1]], val]]).
:- f_u_is(eql, 1, [let, [[u_val, 1]], u_val], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4357 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.1. Review of defstruct", 1, 4392)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4420 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ progn,
			    ['prolog-inline', '$STRING'("nop(trace)")],
			    [is, eq, [quote, point], [defstruct, point, x, y, z]]
			  ]).
:- nop(trace),
   f_u_is(eq, [quote, u_point], [defstruct, u_point, u_x, u_y, u_z], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4420 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; (defstruct point x y z)", 1, 4505)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4532 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point4d], [defstruct, point4d, x, y, z, t]]).
:- f_u_is(eq, [quote, u_point4d], [defstruct, u_point4d, u_x, u_y, u_z, t], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4578 **********************/
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
var_tracker(u_point)=rw{name:u_point, p:1, r:3, ret:0, u:0, vars:[U_point_Param, U_point_Get, U_point_Get40, U_point_Get52], w:1}.
var_tracker(u_x)=rw{name:u_x, p:0, r:2, ret:0, u:0, vars:[U_x_Get71, U_x_Get71], w:0}.
var_tracker(u_y)=rw{name:u_y, p:0, r:2, ret:0, u:0, vars:[U_y_Get77, U_y_Get77], w:0}.
var_tracker(u_z)=rw{name:u_z, p:0, r:2, ret:0, u:0, vars:[U_z_Get85, U_z_Get85], w:0}.
 */

% asserting... u_distance_from_origin 
wl:arglist_info(u_distance_from_origin, f_u_distance_from_origin, [u_point], [U_point_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:1, rest:0}).

% asserting... u_distance_from_origin 
lambda_def(defun,(u_distance_from_origin), f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).

% asserting... u_distance_from_origin 
f_u_distance_from_origin(U_point_Param, MResult) :-
	Env=[bv(u_point, U_point_Param)],
	catch(( f_u_point_x(U_point_Param, U_point_x_Ret),
		LEnv=[[bv(u_x, U_point_x_Ret)]|Env],
		f_u_point_y(U_point_Param, U_point_y_Ret),
		LEnv44=[[bv(u_y, U_point_y_Ret)]|LEnv],
		f_u_point_z(U_point_Param, U_point_z_Ret),
		LEnv56=[[bv(u_z, U_point_z_Ret)]|LEnv44],
		get_var(LEnv56, u_x, U_x_Get71),
		*(U_x_Get71, U_x_Get71, Xx_Ret),
		get_var(LEnv56, u_y, U_y_Get77),
		*(U_y_Get77, U_y_Get77, Xx_Ret78),
		+(Xx_Ret, Xx_Ret78, C43_Ret),
		get_var(LEnv56, u_z, U_z_Get85),
		*(U_z_Get85, U_z_Get85, Xx_Ret86),
		+(C43_Ret, Xx_Ret86, C43_Ret87),
		cl_sqrt(C43_Ret87, Sqrt_Ret),
		Sqrt_Ret=MResult
	      ),
	      block_exit(u_distance_from_origin, MResult),
	      true).
:- set_opv(f_u_distance_from_origin, classof, claz_compiled_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4744 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reflect-in-y-axis',
			    [point],
			    [setf, ['point-y', point], [-, ['point-y', point]]]
			  ]).
/* 
alphas=[u_point].
type=ctx.
var_tracker(u_point)=rw{name:u_point, p:1, r:1, ret:0, u:0, vars:[U_point_Param, U_point_Get], w:2}.
 */

% asserting... u_reflect_in_y_axis 
wl:arglist_info(u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [U_point_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:1, rest:0}).

% asserting... u_reflect_in_y_axis 
lambda_def(defun,(u_reflect_in_y_axis), f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).

% asserting... u_reflect_in_y_axis 
f_u_reflect_in_y_axis(U_point_Param, MResult) :-
	Env=[bv(u_point, U_point_Param)],
	catch(( get_var(Env, u_point, U_point_Get),
		f_u_point_y(U_point_Get, U_point_y_Ret),
		-(0, U_point_y_Ret, C45_Ret),
		set_place(Env, setf, [u_point_y, u_point], C45_Ret, _rPrevRes),
		_rPrevRes=MResult
	      ),
	      block_exit(u_reflect_in_y_axis, MResult),
	      true).
:- set_opv(f_u_reflect_in_y_axis, classof, claz_compiled_function),
   set_opv(u_reflect_in_y_axis, compile_as, kw_function),
   set_opv(u_reflect_in_y_axis, function, f_u_reflect_in_y_axis).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4832 **********************/
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
:- f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, U_make_point_Ret),
   set_place(TLEnv, setf, u_my_point, U_make_point_Ret, Set_place_Ret),
   f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, U_make_point_Ret33),
   set_place(TLEnv, setf, u_my_point2, U_make_point_Ret33, Set_place_Ret38),
   List_Ret=[Set_place_Ret, Set_place_Ret38].

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4930 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-point3',
			    '$S'(['POINT', ':X', 3, ':Y', 4, ':Z', 12])
			  ]).
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, u_my_point3, Create_struct_Ret, _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:4973 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-point4d',
			    ['make-point4d', ':x', 3, ':y', 4, ':z', 12, ':t', 1]
			  ]).
:- f_u_make_point4d(kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1, U_make_point4d_Ret),
   set_place(TLEnv, setf, u_my_point4d, U_make_point4d_Ret, _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5029 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, ['point-p', 'my-point']]).
:- f_u_is(eq, t, [u_point_p, u_my_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5059 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point], ['type-of', 'my-point']]).
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5094 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(
				     [ flag_removed,
				       [+, ':IGNORE'],
				       
				       [ #+,
					 ':WAM-CL',
					 ['prolog-call', '$STRING'("break")]
				       ]
				     ])).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eql,
			    13,
			    
			    [ progn,
			      [print, ['distance-from-origin', 'my-point']]
			    ]
			  ]).
:- f_u_is(eql, 13, [progn, [print, [u_distance_from_origin, u_my_point]]], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5135 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #+CLISP (BREAK)", 1, 5197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; #+WAM-CL (prolog-call \"break\")",
				     1,
				     5216)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5250 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, =, -4, ['reflect-in-y-axis', 'my-point']]).
:- f_u_is(=, -4, [u_reflect_in_y_axis, u_my_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5290 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 'my-point', 'my-point']).
:- f_u_is(eq, u_my_point, u_my_point, U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5317 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'a-similar-point',
			    '$S'([point, ':x', 3, ':y', -4, ':z', 12])
			  ]).
:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, u_a_similar_point, Create_struct_Ret, _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5317 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (is eq t (equal my-point a-similar-point))",
				     1,
				     5369)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5414 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], [eq, 'my-point', 'a-similar-point']]).
:- f_u_is(eq, [], [eq, u_my_point, u_a_similar_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5457 **********************/
:- lisp_compile_to_prolog(pkg_user, [equalp, 'my-point', 'a-similar-point']).
:- get_var(TLEnv, u_a_similar_point, U_a_similar_point_Get),
   get_var(TLEnv, u_my_point, U_my_point_Get),
   cl_equalp(U_my_point_Get, U_a_similar_point_Get, Equalp_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, t, [equalp, 'my-point', 'a-similar-point']]).
:- f_u_is(eq, t, [equalp, u_my_point, u_a_similar_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5492 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; 3.2. defclass", 1, 5540)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5557 **********************/
:- lisp_compile_to_prolog(pkg_user, [unintern, [quote, point]]).
:- cl_unintern(u_point, Unintern_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5576 **********************/
:- lisp_compile_to_prolog(pkg_user, [defclass, point, [], [x, y, z]]).
:- cl_defclass([u_point, [], [u_x, u_y, u_z]], Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5613 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setf, 'my-point', ['make-instance', [quote, point]]]).
:- cl_make_instance([u_point], Make_instance_Ret),
   set_place(TLEnv, setf, u_my_point, Make_instance_Ret, _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point], ['type-of', 'my-point']]).
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5688 **********************/
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
var_tracker(u_point)=rw{name:u_point, p:1, r:0, ret:0, u:0, vars:[U_point_Param], w:4}.
var_tracker(u_x)=rw{name:u_x, p:1, r:1, ret:0, u:0, vars:[U_x_Param, U_x_Get], w:1}.
var_tracker(u_y)=rw{name:u_y, p:1, r:1, ret:0, u:0, vars:[U_y_Param, U_y_Get], w:1}.
var_tracker(u_z)=rw{name:u_z, p:1, r:1, ret:0, u:0, vars:[U_z_Param, U_z_Get], w:1}.
 */

% asserting... u_set_point_values 
wl:arglist_info(u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [U_point_Param, U_x_Param, U_y_Param, U_z_Param], arginfo{all:4, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:4, rest:0}).

% asserting... u_set_point_values 
lambda_def(defun,(u_set_point_values), f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]).

% asserting... u_set_point_values 
f_u_set_point_values(U_point_Param, U_x_Param, U_y_Param, U_z_Param, MResult) :-
	Env=[bv(u_point, U_point_Param), bv(u_x, U_x_Param), bv(u_y, U_y_Param), bv(u_z, U_z_Param)],
	catch(( set_place(Env,
			 setf,
			  [slot_value, u_point, [quote, u_x]],
			  U_x_Param,
			  Set_place_Ret),
		set_place(Env,
			 setf,
			  [slot_value, u_point, [quote, u_y]],
			  U_y_Param,
			  Set_place_Ret49),
		set_place(Env,
			 setf,
			  [slot_value, u_point, [quote, u_z]],
			  U_z_Param,
			  _rPrevRes),
		_rPrevRes=MResult
	      ),
	      block_exit(u_set_point_values, MResult),
	      true).
:- set_opv(f_u_set_point_values, classof, claz_compiled_function),
   set_opv(u_set_point_values, compile_as, kw_function),
   set_opv(u_set_point_values, function, f_u_set_point_values).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5825 **********************/
:- lisp_compile_to_prolog(pkg_user, ['set-point-values', 'my-point', 3, 4, 12]).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   f_u_set_point_values(U_my_point_Get, 3, 4, 12, U_set_point_values_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5861 **********************/
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
var_tracker(u_point)=rw{name:u_point, p:1, r:3, ret:0, u:0, vars:[U_point_Param, U_point_Get, U_point_Get34, U_point_Get39], w:1}.
var_tracker(u_x)=rw{name:u_x, p:0, r:2, ret:0, u:0, vars:[U_x_Get55, U_x_Get55], w:0}.
var_tracker(u_y)=rw{name:u_y, p:0, r:2, ret:0, u:0, vars:[U_y_Get61, U_y_Get61], w:0}.
var_tracker(u_z)=rw{name:u_z, p:0, r:2, ret:0, u:0, vars:[U_z_Get69, U_z_Get69], w:0}.
 */

% asserting... u_distance_from_origin 
wl:arglist_info(u_distance_from_origin, f_u_distance_from_origin, [u_point], [U_point_Param], arginfo{all:1, allow_other_keys:0, aux:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:1, rest:0}).

% asserting... u_distance_from_origin 
lambda_def(defun,(u_distance_from_origin), f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).

% asserting... u_distance_from_origin 
f_u_distance_from_origin(U_point_Param, MResult) :-
	Env=[bv(u_point, U_point_Param)],
	catch(( cl_slot_value(U_point_Param, u_x, Slot_value_Ret),
		cl_slot_value(U_point_Param, u_y, Slot_value_Ret36),
		cl_slot_value(U_point_Param, u_z, Slot_value_Ret41),
		LEnv=[[bv(u_x, Slot_value_Ret), bv(u_y, Slot_value_Ret36), bv(u_z, Slot_value_Ret41)]|Env],
		get_var(LEnv, u_x, U_x_Get55),
		*(U_x_Get55, U_x_Get55, Xx_Ret),
		get_var(LEnv, u_y, U_y_Get61),
		*(U_y_Get61, U_y_Get61, Xx_Ret62),
		+(Xx_Ret, Xx_Ret62, C43_Ret),
		get_var(LEnv, u_z, U_z_Get69),
		*(U_z_Get69, U_z_Get69, Xx_Ret70),
		+(C43_Ret, Xx_Ret70, C43_Ret71),
		cl_sqrt(C43_Ret71, Sqrt_Ret),
		Sqrt_Ret=MResult
	      ),
	      block_exit(u_distance_from_origin, MResult),
	      true).
:- set_opv(f_u_distance_from_origin, classof, claz_compiled_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:5974 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['DISASSEMBLE', function('distance-from-origin')]).
:- cl_disassemble(function(u_distance_from_origin), Disassemble_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6013 **********************/
:- lisp_compile_to_prolog(pkg_user, ['distance-from-origin', 'my-point']).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   f_u_distance_from_origin(U_my_point_Get, U_distance_from_origin_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6013 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.3. classes are objects", 1, 6047)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6075 **********************/
:- lisp_compile_to_prolog(pkg_user, ['find-class', [quote, point]]).
:- cl_find_class(u_point, Find_class_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6096 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['class-name', ['find-class', [quote, point]]]).
:- cl_find_class(u_point, Find_class_Ret),
   cl_class_name(Find_class_Ret, Class_name_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6130 **********************/
:- lisp_compile_to_prolog(pkg_user, ['class-of', 'my-point']).
:- get_var(TLEnv, u_my_point, U_my_point_Get),
   cl_class_of(U_my_point_Get, Class_of_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6130 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; #-(or cormanlisp CLISP WAM-CL)",
				     1,
				     6152)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6185 **********************/
:- lisp_compile_to_prolog(pkg_user, [typep, 'my-point', ['class-of', 'my-point']]).
:- get_var(TLEnv, u_my_point, U_my_point_Get20),
   cl_class_of(U_my_point_Get20, Class_of_Ret),
   cl_typep(U_my_point_Get20, Class_of_Ret, Typep_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6223 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6223 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.4. you don't need clos to use clos",
				     1,
				     6300)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6340 **********************/
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
   LEnv=[[bv(u_the_symbol_class, Find_class_Ret)]|TLEnv],
   get_var(LEnv, u_the_symbol_class, U_the_symbol_class_Get28),
   cl_class_name(U_the_symbol_class_Get28, Class_name_Ret),
   get_var(LEnv, u_the_symbol_class, U_the_symbol_class_Get32),
   cl_class_of(symbol, Class_of_Ret),
   cl_eq(U_the_symbol_class_Get32, Class_of_Ret, Eq_Ret),
   get_var(LEnv, u_the_symbol_class, U_the_symbol_class_Get39),
   cl_class_of(U_the_symbol_class_Get39, Class_of_Ret40),
   nb_setval('$mv_return',
	     [U_the_symbol_class_Get28, Class_name_Ret, Eq_Ret, Class_of_Ret40]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6546 **********************/
:- lisp_compile_to_prolog(pkg_user, ['find-class', t]).
:- cl_find_class(t, Find_class_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6562 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [quote, foo], [defstruct, foo]]).
:- f_u_is(eq, [quote, u_foo], [defstruct, u_foo], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6592 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eq,
			    ['find-class', [quote, foo]],
			    ['class-of', ['make-foo']]
			  ]).
:- f_u_is(eq, [find_class, [quote, u_foo]], [class_of, [u_make_foo]], U_is_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6592 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; 3.5 slots", 1, 6642)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6655 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6801 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    
			    [ 'slot-value',
			      ['make-instance', [quote, 'daft-point']],
			      [quote, z]
			    ],
			    42
			  ]).
:- set_place(TLEnv,
	     setf,
	     [slot_value, [make_instance, [quote, u_daft_point]], [quote, u_z]],
	     42,
	     _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6856 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-daft-point',
			    ['make-instance', [quote, 'daft-point'], ':x', 19]
			  ]).
:- cl_make_instance([u_daft_point, kw_x, 19], Make_instance_Ret),
   set_place(TLEnv, setf, u_my_daft_point, Make_instance_Ret, _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:6913 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(
				     [ flag_removed,
				       [+, ':PERFECT'],
				       
				       [ list,
					 ['daft-x', 'my-daft-point'],
					 ['daft-y', 'my-daft-point'],
					 
					 [ progn,
					   [#+, ':WAM-CL', ['prolog-trace']],
					   ['daft-z', 'my-daft-point']
					 ]
				       ]
				     ])).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7045 **********************/
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
   LEnv=[[bv(u_temp, Make_instance_Ret)]|TLEnv],
   set_place(LEnv, setf, [u_daft_y, u_temp], 999, Set_place_Ret),
   set_place(LEnv, setf, [slot_value, u_temp, [quote, u_z]], 0, _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7147 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(
				     [ flag_removed,
				       [+, ':PERFECT'],

				       [ list,
					 ['daft-x', 'my-daft-point'],
					 ['daft-y', 'my-daft-point'],
					 ['daft-z', 'my-daft-point']
				       ]
				     ])).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7147 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.6 Subclasses and inheritance",
				     1,
				     7247)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7281 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7403 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7478 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7552 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-direct-superclasses',
			    ['find-class', [quote, aardvark]]
			  ]).
:- cl_find_class(u_aardvark, Find_class_Ret),
   f_clos_class_direct_superclasses(Find_class_Ret,
				    Clos_class_direct_superclasses_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7552 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; ACL needs to instantiate a class before its precedence-list becomes visible",
				     1,
				     7661)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7552 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #+allegro", 1, 7740)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7752 **********************/
:- lisp_compile_to_prolog(pkg_user, ['make-instance', [quote, aardvark]]).
:- cl_make_instance([u_aardvark], Make_instance_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7779 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-precedence-list',
			    ['find-class', [quote, aardvark]]
			  ]).
:- cl_find_class(u_aardvark, Find_class_Ret),
   f_clos_class_precedence_list(Find_class_Ret, Clos_class_precedence_list_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7879 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7986 **********************/
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
		   [ sys_name,
		     kw_reader,
		     u_aardvark_name,
		     kw_initarg,
		     kw_aardvark_name
		   ],
		   [u_diet, kw_initform, []]
		 ]
	       ],
	       Defclass_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7986 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; ACL needs to instantiate a class before its precedence-list becomes visible",
				     1,
				     8118)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:7986 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #+allegro ", 1, 8197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8210 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['make-instance', [quote, 'figurine-aardvark']]).
:- cl_make_instance([u_figurine_aardvark], Make_instance_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8246 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-precedence-list',
			    ['find-class', [quote, 'figurine-aardvark']]
			  ]).
:- cl_find_class(u_figurine_aardvark, Find_class_Ret),
   f_clos_class_precedence_list(Find_class_Ret, Clos_class_precedence_list_Ret).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8365 **********************/
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
		      '$ARRAY'([*], claz_base_character, "Jen"),
		      kw_made_in,
		      '$ARRAY'([*], claz_base_character, "Brittany"),
		      kw_aardvark_name,
		      '$ARRAY'([*], claz_base_character, "Eric")
		    ],
		    Make_instance_Ret),
   set_place(TLEnv, setf, u_eric, Make_instance_Ret, _rPrevRes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8582 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(
				     [ flag_removed,
				       [+, ':HAS_SHIFTF'],
				       [shiftf, ['cute-p', 'Eric'], t]
				     ])).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/sanity-test.lisp:8621 **********************/
:- lisp_compile_to_prolog(pkg_user, ['slot-value', 'Eric', [quote, diet]]).
:- get_var(TLEnv, u_eric, U_eric_Get),
   cl_slot_value(U_eric_Get, u_diet, Slot_value_Ret).

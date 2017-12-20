
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "sanity-test" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
% Start time: Tue Dec 19 20:51:58 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\n;; $Id: examples.lisp,v 1.1 2003/10/21 17:30:56 nhabedi Exp $\n;;                          EXAMPLES.LISP\n;;           Nick Levine, Ravenbrook Limited, 2003-08-14\n;; \n;; These are the examples I expect to use in the tutorial on CLOS\n;; at the International Lisp Conference 2003.\n;; \n;; This document is mainly for my operational convenience. You might\n;; want to raid fragments to help you get started when building CLOS\n;; into your Common Lisp applications. Nothing useful will happen if\n;; you try to cl:load this document into a lisp image.\n;;\n;; This document is provided \"as is\", without any express or implied\n;; warranty.  In no event will the author be held liable for any\n;; damages arising from the use of this document.  You may make and\n;; distribute verbatim copies of this document provided that you do\n;; not charge a fee for this document or for its distribution.\n",
				     2,
				     2)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:886 **********************/
:- lisp_compile_to_prolog(pkg_user, ['prolog-call', '$STRING'("cls.")]).
:- (   cls
   ->  _Ignored=t
   ;   _Ignored=[]
   ).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:917 **********************/
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

% annotating U::MAPCAR-VISUALIZE 
wl: lambda_def(defun,
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
	      ]).


% annotating U::MAPCAR-VISUALIZE 
wl: arglist_info(u_mapcar_visualize,
		[u_func, u_l],
		[Func_Param, L_Param],
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
		       }).

:-  !.

% annotating U::MAPCAR-VISUALIZE 
wl: init_args(exact_only, u_mapcar_visualize).


% annotating U::MAPCAR-VISUALIZE 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1029 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [load, '$STRING'("../prolog/wam_cl/wam-cl-init")]).
:- cl_load('$ARRAY'([*],
		    claz_base_character,
		    
		    [ #\('.'),
		      #\('.'),
		      #\(/),
		      #\(p),
		      #\(r),
		      #\(o),
		      #\(l),
		      #\(o),
		      #\(g),
		      #\(/),
		      #\(w),
		      #\(a),
		      #\(m),
		      #\('_'),
		      #\(c),
		      #\(l),
		      #\(/),
		      #\(w),
		      #\(a),
		      #\(m),
		      #\(-),
		      #\(c),
		      #\(l),
		      #\(-),
		      #\(i),
		      #\(n),
		      #\(i),
		      #\(t)
		    ]),
	   [],
	   _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1068 **********************/
:- lisp_compile_to_prolog(pkg_user, ['in-package', '$STRING'("CL-USER")]).
:- cl_in_package('$ARRAY'([*],
			  claz_base_character,
			  
			  [ #\('C'),
			    #\('L'),
			    #\(-),
			    #\('U'),
			    #\('S'),
			    #\('E'),
			    #\('R')
			  ]),
		 _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1068 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; Test macro", 1, 1095)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1108 **********************/
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
				    [['#COMMA', eqf], ['#COMMA', a], ['#COMMA', b]],
				    
				    [ format,
				      t,
				      '$STRING'("OK: ~a is ~a to ~a~%"),
				      [quote, ['#COMMA', expected]],
				      [quote, ['#COMMA', eqf]],
				      [quote, ['#COMMA', actual]]
				    ],
				    
				    [ progn,
				      
				      [ format,
					t,
					'$STRING'("FAILED: when matching ~a and ~a~%"),
					['#COMMA', a],
					['#COMMA', b]
				      ],
				      ['prolog-inline', '$STRING'("trace")]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::IS 
wl: lambda_def(defmacro,
	      u_is,
	      f_u_is,
	      [u_eqf, u_expected, u_actual],
	      
	      [ progn,
		
		[ let,
		  
		  [ [u_a, [gensym, '$ARRAY'([*], claz_base_character, [#\(a)])]],
		    [u_b, [gensym, '$ARRAY'([*], claz_base_character, [#\(b)])]]
		  ],
		  
		  [ '#BQ',
		    
		    [ let,
		      
		      [ [['#COMMA', u_a], ['#COMMA', u_expected]],
			[['#COMMA', u_b], ['#COMMA', u_actual]]
		      ],
		      
		      [ if,
			[['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]],
			
			[ format,
			  t,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('O'),
				     #\('K'),
				     #\(:),
				     #\(' '),
				     #\(~),
				     #\(a),
				     #\(' '),
				     #\(i),
				     #\(s),
				     #\(' '),
				     #\(~),
				     #\(a),
				     #\(' '),
				     #\(t),
				     #\(o),
				     #\(' '),
				     #\(~),
				     #\(a),
				     #\(~),
				     #\('%')
				   ]),
			  [quote, ['#COMMA', u_expected]],
			  [quote, ['#COMMA', u_eqf]],
			  [quote, ['#COMMA', u_actual]]
			],
			
			[ progn,
			  
			  [ format,
			    t,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('F'),
				       #\('A'),
				       #\('I'),
				       #\('L'),
				       #\('E'),
				       #\('D'),
				       #\(:),
				       #\(' '),
				       #\(w),
				       #\(h),
				       #\(e),
				       #\(n),
				       #\(' '),
				       #\(m),
				       #\(a),
				       #\(t),
				       #\(c),
				       #\(h),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(~),
				       #\(a),
				       #\(' '),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(~),
				       #\(a),
				       #\(~),
				       #\('%')
				     ]),
			    ['#COMMA', u_a],
			    ['#COMMA', u_b]
			  ],
			  
			  [ sys_prolog_inline,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(t), #\(r), #\(a), #\(c), #\(e)])
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::IS 
wl: arglist_info(u_is,
		[u_eqf, u_expected, u_actual],
		[Eqf_Param, Expected_Param, Actual_Param],
		arginfo{ all:[u_eqf, u_expected, u_actual],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_eqf, u_expected, u_actual],
			 opt:0,
			 req:[u_eqf, u_expected, u_actual],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::IS 
wl: init_args(exact_only, u_is).


% annotating U::IS 
f_u_is(Eqf_Param, Expected_Param, Actual_Param, FnResult) :-
	TLEnv=[bv(u_eqf, Eqf_Param), bv(u_expected, Expected_Param), bv(u_actual, Actual_Param)],
	cl_gensym('$ARRAY'([*], claz_base_character, [#\(a)]), A_Init),
	cl_gensym('$ARRAY'([*], claz_base_character, [#\(b)]), B_Init),
	LEnv=[[bv(u_a, A_Init), bv(u_b, B_Init)]|TLEnv],
	get_var(LEnv, u_a, A_Get28),
	get_var(LEnv, u_b, B_Get29),
	LetResult=[let, [[A_Get28, Expected_Param], [B_Get29, Actual_Param]], [if, [Eqf_Param, A_Get28, B_Get29], [format, t, '$ARRAY'([*], claz_base_character, [#\('O'), #\('K'), #\(:), #\(' '), #\(~), #\(a), #\(' '), #\(i), #\(s), #\(' '), #\(~), #\(a), #\(' '), #\(t), #\(o), #\(' '), #\(~), #\(a), #\(~), #\('%')]), [quote, Expected_Param], [quote, Eqf_Param], [quote, Actual_Param]], [progn, [format, t, '$ARRAY'([*], claz_base_character, [#\('F'), #\('A'), #\('I'), #\('L'), #\('E'), #\('D'), #\(:), #\(' '), #\(w), #\(h), #\(e), #\(n), #\(' '), #\(m), #\(a), #\(t), #\(c), #\(h), #\(i), #\(n), #\(g), #\(' '), #\(~), #\(a), #\(' '), #\(a), #\(n), #\(d), #\(' '), #\(~), #\(a), #\(~), #\('%')]), A_Get28, B_Get29], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, [#\(t), #\(r), #\(a), #\(c), #\(e)])]]]],
	LetResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_is, classof, claz_macro),
   set_opv(u_is, compile_as, kw_operator),
   set_opv(u_is, function, f_u_is),
   DefMacroResult=u_is.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1494 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['write-line', '$STRING'("Running smoke test!")]).
:- cl_write_line('$ARRAY'([*],
			  claz_base_character,
			  
			  [ #\('R'),
			    #\(u),
			    #\(n),
			    #\(n),
			    #\(i),
			    #\(n),
			    #\(g),
			    #\(' '),
			    #\(s),
			    #\(m),
			    #\(o),
			    #\(k),
			    #\(e),
			    #\(' '),
			    #\(t),
			    #\(e),
			    #\(s),
			    #\(t),
			    #\(!)
			  ]),
		 _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1494 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (progn (prolog-inline \"rtrace\") (is eq 1 1))",
				     1,
				     1531)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1577 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, 1]).
:- f_u_is(eq, 1, 1, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1589 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    equal,
			    [list, 1, [quote, a], [quote, b]],
			    [cons, 1, [quote, [a, b]]]
			  ]).
:- f_u_is(equal,
	  [list, 1, [quote, u_a], [quote, u_b]],
	  [cons, 1, [quote, [u_a, u_b]]],
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1632 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 2, [if, [], 1, 2]]).
:- f_u_is(eq, 2, [if, [], 1, 2], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1656 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, [keywordp, ':k']]).
:- f_u_is(eq, t, [keywordp, kw_k], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1681 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 10, [if, t, 10, 20]]).
:- f_u_is(eq, 10, [if, t, 10, 20], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1706 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, [stringp, '$STRING'("abc")]]).
:- f_u_is(eq,
	  t,
	  [stringp, '$ARRAY'([*], claz_base_character, [#\(a), #\(b), #\(c)])],
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1706 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).",
				     1,
				     1734)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1830 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    equal,
			    [subseq, '$STRING'("abc"), 1],
			    '$STRING'("bc")
			  ]).
:- f_u_is(equal,
	  [subseq, '$ARRAY'([*], claz_base_character, [#\(a), #\(b), #\(c)]), 1],
	  '$ARRAY'([*], claz_base_character, [#\(b), #\(c)]),
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1864 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, [if, t, 1, 2]]).
:- f_u_is(eq, 1, [if, t, 1, 2], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1885 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 2, [if, [], 1, 2]]).
:- f_u_is(eq, 2, [if, [], 1, 2], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1909 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    fib,
			    [n],
			    [if, [>, n, 1], [+, [fib, [-, n, 1]], [fib, [-, n, 2]]], 1]
			  ]).

% annotating U::FIB 
wl: lambda_def(defun,
	      u_fib,
	      f_u_fib,
	      [n],
	      [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).


% annotating U::FIB 
wl: arglist_info(u_fib,
		[n],
		[N_Param],
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
		       }).

:-  !.

% annotating U::FIB 
wl: init_args(exact_only, u_fib).


% annotating U::FIB 
f_u_fib(N_Param, FnResult) :-
	(   N_Param>1
	->  -(N_Param, 1, Fib_Param),
	    f_u_fib(Fib_Param, Fib_Ret),
	    -(N_Param, 2, Fib_Param23),
	    f_u_fib(Fib_Param23, Fib_Ret25),
	    +(Fib_Ret, Fib_Ret25, TrueResult),
	    FnResult=TrueResult
	;   FnResult=1
	).
:- set_opv(f_u_fib, classof, claz_function),
   set_opv(u_fib, compile_as, kw_function),
   set_opv(u_fib, function, f_u_fib),
   DefunResult=u_fib.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:1990 **********************/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(fib)]).
:- cl_disassemble(function(u_fib), _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2012 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eql, 89, [fib, 10]]).
:- f_u_is(eql, 89, [u_fib, 10], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2036 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    accum,
			    [r],
			    [if, [=, 0, r], [list, 0], [cons, r, [accum, [-, r, 1]]]]
			  ]).

% annotating U::ACCUM 
wl: lambda_def(defun,
	      u_accum,
	      f_u_accum,
	      [u_r],
	      [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).


% annotating U::ACCUM 
wl: arglist_info(u_accum,
		[u_r],
		[R_Param],
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
		       }).

:-  !.

% annotating U::ACCUM 
wl: init_args(exact_only, u_accum).


% annotating U::ACCUM 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2102 **********************/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(accum)]).
:- cl_disassemble(function(u_accum), _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2102 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" DISASSEMBLY FOR:f_u_accum\n:- dynamic f_u_accum/2.\n\nf_u_accum(A, G) :-\n\t(   0=:=A\n\t->  G=[0]\n\t;   C is A - 1,\n\t    f_u_accum(C, D),\n\t    G=[A|D]\n\t).\n\n",
				     2,
				     2126)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2279 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, equal, [list, 4, 3, 2, 1, 0], [accum, 4]]).
:- f_u_is(equal, [list, 4, 3, 2, 1, 0], [u_accum, 4], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2318 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    defwrap,
			    [name],
			    ['#BQ', [defun, ['#COMMA', name], [], 1]]
			  ]).

% annotating U::DEFWRAP 
wl: lambda_def(defmacro,
	      u_defwrap,
	      f_u_defwrap,
	      [sys_name],
	      [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]).


% annotating U::DEFWRAP 
wl: arglist_info(u_defwrap,
		[sys_name],
		[Name_Param],
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
		       }).

:-  !.

% annotating U::DEFWRAP 
wl: init_args(exact_only, u_defwrap).


% annotating U::DEFWRAP 
f_u_defwrap(Name_Param, FnResult) :-
	[defun, Name_Param, [], 1]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_defwrap, classof, claz_macro),
   set_opv(u_defwrap, compile_as, kw_operator),
   set_opv(u_defwrap, function, f_u_defwrap),
   DefMacroResult=u_defwrap.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2318 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; :- ensure_loaded('sanity-test.lisp.trans.pl').",
				     1,
				     2365)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2415 **********************/
:- lisp_compile_to_prolog(pkg_user, [defwrap, foo]).
:- f_u_defwrap(u_foo, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2429 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, [foo]]).
:- f_u_is(eq, 1, [u_foo], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2445 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    equal,
			    ['macroexpand-1', [quote, [defwrap, foo]]],
			    [quote, [defun, foo, [], 1]]
			  ]).
:- f_u_is(equal,
	  [macroexpand_1, [quote, [u_defwrap, u_foo]]],
	  [quote, [defun, u_foo, [], 1]],
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2507 **********************/
:- lisp_compile_to_prolog(pkg_user, ['write-line', '$STRING'("PASSED")]).
:- cl_write_line('$ARRAY'([*],
			  claz_base_character,
			  [#\('P'), #\('A'), #\('S'), #\('S'), #\('E'), #\('D')]),
		 _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2530 **********************/
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

% annotating U::FIFTEEN 
wl: lambda_def(defun,
	      u_fifteen,
	      f_u_fifteen,
	      [],
	      
	      [ 
		[ let,
		  [u_val],
		  
		  [ tagbody,
		    [setq, u_val, 1],
		    [go, u_point_a],
		    [incf, u_val, 16],
		    u_point_c,
		    [incf, u_val, 4],
		    [go, u_point_b],
		    [incf, u_val, 32],
		    u_point_a,
		    u_point_u,
		    [incf, u_val, 2],
		    [go, u_point_c],
		    [incf, u_val, 64],
		    u_point_b,
		    [incf, u_val, 8]
		  ],
		  u_val
		]
	      ]).


% annotating U::FIFTEEN 
wl: arglist_info(u_fifteen,
		[],
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
		       }).

:-  !.

% annotating U::FIFTEEN 
wl: init_args(exact_only, u_fifteen).


% annotating U::FIFTEEN 
f_u_fifteen(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_val, [])]|Env],
		  call_addr_block(GoEnv,
				  (set_var(GoEnv, setq, u_val, 1), goto(u_point_a, GoEnv)),
				  
				  [ addr(addr_tagbody_2_u_point_c,
					 u_point_c,
					 '$used',
					 Incf_Env,
					 (set_place(Incf_Env, incf, [value, u_val], [4], Set_place_Ret), goto(u_point_b, Incf_Env))),
				    addr(addr_tagbody_2_u_point_a,
					 u_point_a,
					 '$used',
					 Incf_Env20,
					 (push_label(u_point_u), set_place(Incf_Env20, incf, [value, u_val], [2], Incf_R19), goto(u_point_c, Incf_Env20))),
				    addr(addr_tagbody_2_u_point_u,
					 u_point_u,
					 '$unused',
					 Incf_Env24,
					 (set_place(Incf_Env24, incf, [value, u_val], [2], Incf_R23), goto(u_point_c, Incf_Env24))),
				    addr(addr_tagbody_2_u_point_b,
					 u_point_b,
					 '$used',
					 Incf_Env28,
					 set_place(Incf_Env28,
						   incf,
						   [value, u_val],
						   [8],
						   _GORES21))
				  ]),
		  get_var(GoEnv, u_val, Val_Get),
		  LetResult=Val_Get
		),
		LetResult=FnResult
	      ),
	      block_exit(u_fifteen, FnResult),
	      true).
:- set_opv(f_u_fifteen, classof, claz_function),
   set_opv(u_fifteen, compile_as, kw_function),
   set_opv(u_fifteen, function, f_u_fifteen),
   DefunResult=u_fifteen.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2530 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; unused", 14, 2731)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2844 **********************/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(fifteen)]).
:- cl_disassemble(function(u_fifteen), _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:2844 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\n\n/* this first one should get deleted since its inlined away in f_u_fifteen */\n\naddr_tagbody_1_addr_enter_1(Env10) :-\n        symbol_setter(Env10, setq, u_val, 1),\n        addr_tagbody_1_u_point_a(Env10).\naddr_tagbody_1_u_point_c(Incf_Env) :-\n        place_op(Incf_Env, incf, [value, u_val], [4], Incf_R),\n        addr_tagbody_1_u_point_b(Incf_Env).\naddr_tagbody_1_u_point_a(Incf_Env19) :-\n        place_op(Incf_Env19, incf, [value, u_val], [2], Incf_R18),\n        addr_tagbody_1_u_point_c(Incf_Env19).\naddr_tagbody_1_u_point_u(Incf_Env23) :-\n        place_op(Incf_Env23, incf, [value, u_val], [2], Incf_R22),\n        addr_tagbody_1_u_point_c(Incf_Env23).\naddr_tagbody_1_u_point_b(Incf_Env27) :-\n        place_op(Incf_Env27, incf, [value, u_val], [8], _GORES15).\n\nf_u_fifteen(MResult) :-\n        Env=[],\n        catch(( TBEnv=[[bv(u_val, [])]|Env],\n                symbol_setter(TBEnv, setq, u_val, 1),\n                addr_tagbody_1_u_point_a(TBEnv),\n                symbol_value(TBEnv, u_val, U_val_Get),\n                U_val_Get=MResult\n              ),\n              block_exit(u_fifteen, MResult),\n              true).\n\n",
				     2,
				     2871)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4002 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 15, [fifteen]]).
:- f_u_is(eq, 15, [u_fifteen], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4024 **********************/
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

% annotating U::DO-FOUR 
wl: lambda_def(defun,
	      u_do_four,
	      f_u_do_four,
	      [],
	      
	      [ 
		[ do,
		  
		  [ [u_temp_one, 1, ['1+', u_temp_one]],
		    [u_temp_two, 0, ['1-', u_temp_two]]
		  ],
		  [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one],
		  []
		]
	      ]).


% annotating U::DO-FOUR 
wl: arglist_info(u_do_four,
		[],
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
		       }).

:-  !.

% annotating U::DO-FOUR 
wl: init_args(exact_only, u_do_four).


% annotating U::DO-FOUR 
f_u_do_four(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_temp_one, 1), bv(u_temp_two, 0)]|Env],
		  catch(( call_addr_block(GoEnv,
					  (push_label(do_label_2), get_var(GoEnv, u_temp_one, Temp_one_Get33), get_var(GoEnv, u_temp_two, Temp_two_Get35), -(Temp_one_Get33, Temp_two_Get35, PredArg1Result37), (PredArg1Result37>5->throw(block_exit([], Temp_one_Get23)), _TBResult=ThrowResult39;get_var(GoEnv, u_temp_one, Temp_one_Get42), '1+'(Temp_one_Get42, Temp_one), get_var(GoEnv, u_temp_two, Temp_two_Get43), '1-'(Temp_two_Get43, Temp_two), set_var(GoEnv, u_temp_one, Temp_one), set_var(GoEnv, u_temp_two, Temp_two), goto(do_label_2, GoEnv), _TBResult=_GORES44)),
					  
					  [ addr(addr_tagbody_3_do_label_2,
						 do_label_2,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, u_temp_one, Temp_one_Get23), get_var(BlockExitEnv, u_temp_two, Temp_two_Get26), -(Temp_one_Get23, Temp_two_Get26, _14752), (_14752>5->throw(block_exit([], Temp_one_Get23)), _14754=ThrowResult;'1+'(Temp_one_Get23, Set_var_Ret), '1-'(Temp_two_Get26, Set_var_Ret54), set_var(BlockExitEnv, u_temp_one, Set_var_Ret), set_var(BlockExitEnv, u_temp_two, Set_var_Ret54), goto(do_label_2, BlockExitEnv), _14754=_GORES)))
					  ]),
			  []=Block_exit_Ret
			),
			block_exit([], Block_exit_Ret),
			true),
		  LetResult=Block_exit_Ret
		),
		LetResult=FnResult
	      ),
	      block_exit(u_do_four, FnResult),
	      true).
:- set_opv(f_u_do_four, classof, claz_function),
   set_opv(u_do_four, compile_as, kw_function),
   set_opv(u_do_four, function, f_u_do_four),
   DefunResult=u_do_four.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4148 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, =, 4, ['do-four']]).
:- f_u_is(=, 4, [u_do_four], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4169 **********************/
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
			   
			   [ #\(t),
			     #\(y),
			     #\(p),
			     #\(e),
			     #\(' '),
			     #\(e),
			     #\(r),
			     #\(r),
			     #\(o),
			     #\(r)
			   ])
		]
	      ]
	    ]
	  ],
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], ['TAGBODY', 1, ['PRINT', '$STRING'("hi")]]]).
:- f_u_is(eq,
	  [],
	  [tagbody, 1, [print, '$ARRAY'([*], claz_base_character, [#\(h), #\(i)])]],
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4326 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], ['TAGBODY', a, ['PRINT', '$STRING'("hi")]]]).
:- f_u_is(eq,
	  [],
	  
	  [ tagbody,
	    u_a,
	    [print, '$ARRAY'([*], claz_base_character, [#\(h), #\(i)])]
	  ],
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4364 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]], []]]).
:- f_u_is(eq, [], [let, [[u_val, 1]], []], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4396 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]]]]).
:- f_u_is(eq, [], [let, [[u_val, 1]]], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4426 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eql, 1, ['LET', [[val, 1]], val]]).
:- f_u_is(eql, 1, [let, [[u_val, 1]], u_val], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4426 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.1. Review of defstruct", 1, 4461)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4489 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ progn,
			    ['prolog-inline', '$STRING'("nop(trace)")],
			    [is, eq, [quote, point], [defstruct, point, x, y, z]]
			  ]).
:- nop(trace),
   f_u_is(eq, [quote, u_point], [defstruct, u_point, u_x, u_y, u_z], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4489 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; (defstruct point x y z)", 1, 4574)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4601 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point4d], [defstruct, point4d, x, y, z, t]]).
:- f_u_is(eq, [quote, u_point4d], [defstruct, u_point4d, u_x, u_y, u_z, t], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4647 **********************/
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

% annotating U::DISTANCE-FROM-ORIGIN 
wl: lambda_def(defun,
	      u_distance_from_origin,
	      f_u_distance_from_origin,
	      [u_point],
	      
	      [ 
		[ let_xx,
		  
		  [ [u_x, [u_point_x, u_point]],
		    [u_y, [u_point_y, u_point]],
		    [u_z, [u_point_z, u_point]]
		  ],
		  [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]
		]
	      ]).


% annotating U::DISTANCE-FROM-ORIGIN 
wl: arglist_info(u_distance_from_origin,
		[u_point],
		[Point_Param],
		arginfo{ all:[u_point],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_point],
			 opt:0,
			 req:[u_point],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DISTANCE-FROM-ORIGIN 
wl: init_args(exact_only, u_distance_from_origin).


% annotating U::DISTANCE-FROM-ORIGIN 
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	f_u_point_x(Point_Param, X_Init),
	LEnv=[[bv(u_x, X_Init)]|Env],
	f_u_point_y(Point_Param, Y_Init),
	Env=[[bv(u_y, Y_Init)]|LEnv],
	f_u_point_z(Point_Param, Z_Init),
	Env=[[bv(u_z, Z_Init)]|Env],
	get_var(Env, u_x, X_Get29),
	*(X_Get29, X_Get29, _14922),
	get_var(Env, u_y, Y_Get31),
	*(Y_Get31, Y_Get31, _15034),
	+(_14922, _15034, _15154),
	get_var(Env, u_z, Z_Get33),
	*(Z_Get33, Z_Get33, _15166),
	+(_15154, _15166, Sqrt_Param),
	cl_sqrt(Sqrt_Param, Sqrt_Ret),
	LetResult=Sqrt_Ret,
	LetResult=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4813 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reflect-in-y-axis',
			    [point],
			    [setf, ['point-y', point], [-, ['point-y', point]]]
			  ]).

% annotating U::REFLECT-IN-Y-AXIS 
wl: lambda_def(defun,
	      u_reflect_in_y_axis,
	      f_u_reflect_in_y_axis,
	      [u_point],
	      [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).


% annotating U::REFLECT-IN-Y-AXIS 
wl: arglist_info(u_reflect_in_y_axis,
		[u_point],
		[Point_Param],
		arginfo{ all:[u_point],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_point],
			 opt:0,
			 req:[u_point],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REFLECT-IN-Y-AXIS 
wl: init_args(exact_only, u_reflect_in_y_axis).


% annotating U::REFLECT-IN-Y-AXIS 
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4901 **********************/
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
:- f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, Make_point_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_point_Ret], Setf_R),
   f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, Make_point_Ret9),
   set_place(TLEnv, setf, [value, u_my_point2], [Make_point_Ret9], Setf_R7),
   _Ignored=[Setf_R, Setf_R7].

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:4999 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-point3',
			    '$S'(['POINT', ':X', 3, ':Y', 4, ':Z', 12])
			  ]).
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_my_point3], [Create_struct_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5042 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-point4d',
			    ['make-point4d', ':x', 3, ':y', 4, ':z', 12, ':t', 1]
			  ]).
:- f_u_make_point4d(kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1, Make_point4d_Ret),
   set_place(TLEnv, setf, [value, u_my_point4d], [Make_point4d_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5098 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, ['point-p', 'my-point']]).
:- f_u_is(eq, t, [u_point_p, u_my_point], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5128 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point], ['type-of', 'my-point']]).
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5163 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eql,
			    13,
			    
			    [ progn,
			      [print, ['distance-from-origin', 'my-point']]
			    ]
			  ]).
:- f_u_is(eql, 13, [progn, [print, [u_distance_from_origin, u_my_point]]], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #+CLISP (BREAK)", 1, 5266)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; #+WAM-CL (prolog-call \"break\")",
				     1,
				     5285)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5319 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, =, -4, ['reflect-in-y-axis', 'my-point']]).
:- f_u_is(=, -4, [u_reflect_in_y_axis, u_my_point], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5359 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 'my-point', 'my-point']).
:- f_u_is(eq, u_my_point, u_my_point, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5386 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'a-similar-point',
			    '$S'([point, ':x', 3, ':y', -4, ':z', 12])
			  ]).
:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_a_similar_point], [Create_struct_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5386 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (is eq t (equal my-point a-similar-point))",
				     1,
				     5438)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5483 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [], [eq, 'my-point', 'a-similar-point']]).
:- f_u_is(eq, [], [eq, u_my_point, u_a_similar_point], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5526 **********************/
:- lisp_compile_to_prolog(pkg_user, [equalp, 'my-point', 'a-similar-point']).
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, t, [equalp, 'my-point', 'a-similar-point']]).
:- f_u_is(eq, t, [equalp, u_my_point, u_a_similar_point], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5561 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; 3.2. defclass", 1, 5609)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5626 **********************/
:- lisp_compile_to_prolog(pkg_user, [unintern, [quote, point]]).
:- cl_unintern(u_point, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5645 **********************/
:- lisp_compile_to_prolog(pkg_user, [defclass, point, [], [x, y, z]]).
:- cl_defclass([u_point, [], [u_x, u_y, u_z]], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5682 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setf, 'my-point', ['make-instance', [quote, point]]]).
:- cl_make_instance([u_point], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_instance_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5722 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [is, eq, [quote, point], ['type-of', 'my-point']]).
:- f_u_is(eq, [quote, u_point], [type_of, u_my_point], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5757 **********************/
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

% annotating U::SET-POINT-VALUES 
wl: lambda_def(defun,
	      u_set_point_values,
	      f_u_set_point_values,
	      [u_point, u_x, u_y, u_z],
	      
	      [ 
		[ setf,
		  [slot_value, u_point, [quote, u_x]],
		  u_x,
		  [slot_value, u_point, [quote, u_y]],
		  u_y,
		  [slot_value, u_point, [quote, u_z]],
		  u_z
		]
	      ]).


% annotating U::SET-POINT-VALUES 
wl: arglist_info(u_set_point_values,
		[u_point, u_x, u_y, u_z],
		[Point_Param, X_Param, Y_Param, Z_Param],
		arginfo{ all:[u_point, u_x, u_y, u_z],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_point, u_x, u_y, u_z],
			 opt:0,
			 req:[u_point, u_x, u_y, u_z],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-POINT-VALUES 
wl: init_args(exact_only, u_set_point_values).


% annotating U::SET-POINT-VALUES 
f_u_set_point_values(Point_Param, X_Param, Y_Param, Z_Param, FnResult) :-
	Env=[bv(u_point, Point_Param), bv(u_x, X_Param), bv(u_y, Y_Param), bv(u_z, Z_Param)],
	set_place(Env, setf, [slot_value, Point_Param, u_x], [X_Param], Setf_R),
	set_place(Env, setf, [slot_value, Point_Param, u_y], [Y_Param], Setf_R24),
	set_place(Env, setf, [slot_value, Point_Param, u_z], [Z_Param], Setf_R27),
	Setf_R27=FnResult.
:- set_opv(f_u_set_point_values, classof, claz_function),
   set_opv(u_set_point_values, compile_as, kw_function),
   set_opv(u_set_point_values, function, f_u_set_point_values),
   DefunResult=u_set_point_values.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5894 **********************/
:- lisp_compile_to_prolog(pkg_user, ['set-point-values', 'my-point', 3, 4, 12]).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_set_point_values(My_point_Get, 3, 4, 12, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:5930 **********************/
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

% annotating U::DISTANCE-FROM-ORIGIN 
wl: lambda_def(defun,
	      u_distance_from_origin,
	      f_u_distance_from_origin,
	      [u_point],
	      
	      [ 
		[ with_slots,
		  [u_x, u_y, u_z],
		  u_point,
		  [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]
		]
	      ]).


% annotating U::DISTANCE-FROM-ORIGIN 
wl: arglist_info(u_distance_from_origin,
		[u_point],
		[Point_Param],
		arginfo{ all:[u_point],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_point],
			 opt:0,
			 req:[u_point],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DISTANCE-FROM-ORIGIN 
wl: init_args(exact_only, u_distance_from_origin).


% annotating U::DISTANCE-FROM-ORIGIN 
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	cl_slot_value(Point_Param, u_x, X_Init),
	cl_slot_value(Point_Param, u_y, Y_Init),
	cl_slot_value(Point_Param, u_z, Z_Init),
	LEnv=[[bv(u_x, X_Init), bv(u_y, Y_Init), bv(u_z, Z_Init)]|Env],
	get_var(LEnv, u_x, X_Get23),
	*(X_Get23, X_Get23, _15120),
	get_var(LEnv, u_y, Y_Get25),
	*(Y_Get25, Y_Get25, _15244),
	+(_15120, _15244, _15364),
	get_var(LEnv, u_z, Z_Get27),
	*(Z_Get27, Z_Get27, _15376),
	+(_15364, _15376, Sqrt_Param),
	cl_sqrt(Sqrt_Param, Sqrt_Ret),
	LetResult=Sqrt_Ret,
	LetResult=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6043 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['DISASSEMBLE', function('distance-from-origin')]).
:- cl_disassemble(function(u_distance_from_origin), _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6082 **********************/
:- lisp_compile_to_prolog(pkg_user, ['distance-from-origin', 'my-point']).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.3. classes are objects", 1, 6116)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6144 **********************/
:- lisp_compile_to_prolog(pkg_user, ['find-class', [quote, point]]).
:- cl_find_class(u_point, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6165 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['class-name', ['find-class', [quote, point]]]).
:- cl_find_class(u_point, Class_name_Param),
   cl_class_name(Class_name_Param, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6199 **********************/
:- lisp_compile_to_prolog(pkg_user, ['class-of', 'my-point']).
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6199 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; #-(or cormanlisp CLISP WAM-CL)",
				     1,
				     6221)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6254 **********************/
:- lisp_compile_to_prolog(pkg_user, [typep, 'my-point', ['class-of', 'my-point']]).
:- get_var(TLEnv, u_my_point, My_point_Get7),
   cl_class_of(My_point_Get7, Class_of_Ret),
   cl_typep(My_point_Get7, Class_of_Ret, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6292 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eq,
			    ['find-class', [quote, 'STANDARD-CLASS']],
			    ['class-of', ['class-of', 'my-point']]
			  ]).
:- f_u_is(eq,
	  [find_class, [quote, standard_class]],
	  [class_of, [class_of, u_my_point]],
	  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6292 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.4. you don't need clos to use clos",
				     1,
				     6369)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6409 **********************/
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
:- cl_find_class(symbol, The_symbol_class_Init),
   LEnv=[[bv(u_the_symbol_class, The_symbol_class_Init)]|TLEnv],
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get10),
   cl_class_name(The_symbol_class_Get10, Class_name_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get11),
   cl_class_of(symbol, Class_of_Ret),
   cl_eq(The_symbol_class_Get11, Class_of_Ret, Eq_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get12),
   cl_class_of(The_symbol_class_Get12, Class_of_Ret16),
   nb_setval('$mv_return',
	     [The_symbol_class_Get10, Class_name_Ret, Eq_Ret, Class_of_Ret16]),
   LetResult=The_symbol_class_Get10.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6615 **********************/
:- lisp_compile_to_prolog(pkg_user, ['find-class', t]).
:- cl_find_class(t, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6631 **********************/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [quote, foo], [defstruct, foo]]).
:- f_u_is(eq, [quote, u_foo], [defstruct, u_foo], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ (is),
			    eq,
			    ['find-class', [quote, foo]],
			    ['class-of', ['make-foo']]
			  ]).
:- f_u_is(eq, [find_class, [quote, u_foo]], [class_of, [u_make_foo]], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6661 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; 3.5 slots", 1, 6711)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6724 **********************/
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
	       _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6870 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6925 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setf,
			    'my-daft-point',
			    ['make-instance', [quote, 'daft-point'], ':x', 19]
			  ]).
:- cl_make_instance([u_daft_point, kw_x, 19], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_daft_point], [Make_instance_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:6982 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7114 **********************/
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
:- cl_make_instance([u_daft_point], Temp_Init),
   Env=[[bv(u_temp, Temp_Init)]|TLEnv],
   get_var(Env, u_temp, Temp_Get),
   set_place(Env, setf, [u_daft_y, Temp_Get], [999], Setf_R),
   get_var(Env, u_temp, Temp_Get13),
   set_place(Env, setf, [slot_value, Temp_Get13, u_z], [0], Setf_R12),
   LetResult=Setf_R12.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7216 **********************/
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7216 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; 3.6 Subclasses and inheritance",
				     1,
				     7316)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7350 **********************/
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
	       _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7472 **********************/
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
	       _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7547 **********************/
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
	       _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7621 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-direct-superclasses',
			    ['find-class', [quote, aardvark]]
			  ]).
:- cl_find_class(u_aardvark, Direct_superclasses_Param),
   f_clos_class_direct_superclasses(Direct_superclasses_Param, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7621 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; ACL needs to instantiate a class before its precedence-list becomes visible",
				     1,
				     7730)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7621 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #+allegro", 1, 7809)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7821 **********************/
:- lisp_compile_to_prolog(pkg_user, ['make-instance', [quote, aardvark]]).
:- cl_make_instance([u_aardvark], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-precedence-list',
			    ['find-class', [quote, aardvark]]
			  ]).
:- cl_find_class(u_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:7948 **********************/
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
	       _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8055 **********************/
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
	       _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8055 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; ACL needs to instantiate a class before its precedence-list becomes visible",
				     1,
				     8187)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8055 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #+allegro ", 1, 8266)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8279 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['make-instance', [quote, 'figurine-aardvark']]).
:- cl_make_instance([u_figurine_aardvark], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8315 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'class-precedence-list',
			    ['find-class', [quote, 'figurine-aardvark']]
			  ]).
:- cl_find_class(u_figurine_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8434 **********************/
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
		      '$ARRAY'([*], claz_base_character, [#\('J'), #\(e), #\(n)]),
		      kw_made_in,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('B'),
				 #\(r),
				 #\(i),
				 #\(t),
				 #\(t),
				 #\(a),
				 #\(n),
				 #\(y)
			       ]),
		      kw_aardvark_name,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\('E'), #\(r), #\(i), #\(c)])
		    ],
		    Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_eric], [Make_instance_Ret], Setf_R).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8651 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(
				     [ flag_removed,
				       [+, ':HAS_SHIFTF'],
				       [shiftf, ['cute-p', 'Eric'], t]
				     ])).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp:8690 **********************/
:- lisp_compile_to_prolog(pkg_user, ['slot-value', 'Eric', [quote, diet]]).
:- get_var(TLEnv, u_eric, Eric_Get),
   cl_slot_value(Eric_Get, u_diet, _Ignored).


% Total time: 10.012 seconds


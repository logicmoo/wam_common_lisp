
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_macros" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:14:49 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:282 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fixnum->string',
			    [n],
			    ['#BQ', [format, [], '$STRING'("~A"), ['#COMMA', n]]]
			  ]).

% annotating U::FIXNUM->STRING 
wl: lambda_def(defmacro,
	      u_fixnum_c62_string,
	      f_u_fixnum_c62_string,
	      [n],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ format,
		    [],
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		    ['#COMMA', n]
		  ]
		]
	      ]).


% annotating U::FIXNUM->STRING 
wl: arglist_info(u_fixnum_c62_string,
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

% annotating U::FIXNUM->STRING 
wl: init_args(exact_only, u_fixnum_c62_string).


% annotating U::FIXNUM->STRING 
f_u_fixnum_c62_string(N_Param, FnResult) :-
	[format, [], '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]), N_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fixnum_c62_string, classof, claz_macro),
   set_opv(u_fixnum_c62_string, compile_as, kw_operator),
   set_opv(u_fixnum_c62_string, function, f_u_fixnum_c62_string),
   DefMacroResult=u_fixnum_c62_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:336 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    dbg,
			    ['dbg-var'|rest],
			    
			    [ '#BQ',
			      
			      [ progn,
				
				[ cond,
				  
				  [ ['eq?', ['#COMMA', 'dbg-var'], t],
				    [format, t, ['#BQ-COMMA-ELIPSE', rest]]
				  ],
				  
				  [ ['#COMMA', 'dbg-var'],
				    
				    [ format,
				      ['#COMMA', 'dbg-var'],
				      ['#BQ-COMMA-ELIPSE', rest]
				    ]
				  ],
				  [else, []]
				]
			      ]
			    ]
			  ]).

% annotating U::DBG 
wl: lambda_def(defmacro,
	      u_dbg,
	      f_u_dbg,
	      [u_dbg_var|rest],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ progn,
		    
		    [ cond,
		      
		      [ [u_eq_c63, ['#COMMA', u_dbg_var], t],
			[format, t, ['#BQ-COMMA-ELIPSE', rest]]
		      ],
		      
		      [ ['#COMMA', u_dbg_var],
			
			[ format,
			  ['#COMMA', u_dbg_var],
			  ['#BQ-COMMA-ELIPSE', rest]
			]
		      ],
		      [u_else, []]
		    ]
		  ]
		]
	      ]).


% annotating U::DBG 
wl: arglist_info(u_dbg,
		[u_dbg_var, '&rest', rest],
		[u_dbg_var, rest],
		arginfo{ all:[u_dbg_var],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_dbg_var, rest],
			 opt:0,
			 req:[u_dbg_var],
			 rest:[rest],
			 sublists:0,
			 whole:0
		       }).


% annotating U::DBG 
wl: init_args(1, u_dbg).


% annotating U::DBG 
f_u_dbg(Dbg_var_Param, Rest_Param, FnResult) :-
	TLEnv3=[bv(u_dbg_var, Dbg_var_Param), bv(rest, Rest_Param)],
	get_var(TLEnv3, rest, Rest_Get19),
	[progn, [cond, [[u_eq_c63, Dbg_var_Param, t], [format, t|Rest_Get19]], [Dbg_var_Param, [format, Dbg_var_Param|Rest_Get19]], [u_else, []]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_dbg, classof, claz_macro),
   set_opv(u_dbg, compile_as, kw_operator),
   set_opv(u_dbg, function, f_u_dbg),
   DefMacroResult=u_dbg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:502 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'if-interested-in',
			    [key|rest],
			    
			    [ '#BQ',
			      
			      [ if,
				
				[ and,
				  
				  [ assq,
				    [quote, ['#COMMA', key]],
				    '*ndbg-interests*'
				  ],
				  
				  [ or,
				    
				    [ 'memq?',
				      [quote, all],
				      
				      [ assq,
					[quote, ['#COMMA', key]],
					'*ndbg-interests*'
				      ]
				    ],
				    
				    [ 'any?',
				      [lambda, [x], ['memq?', x, '*ndbg-items*']],
				      
				      [ cdr,
					
					[ assq,
					  [quote, ['#COMMA', key]],
					  '*ndbg-interests*'
					]
				      ]
				    ]
				  ]
				],
				[progn, ['#BQ-COMMA-ELIPSE', rest]],
				[]
			      ]
			    ]
			  ]).

% annotating U::IF-INTERESTED-IN 
wl: lambda_def(defmacro,
	      u_if_interested_in,
	      f_u_if_interested_in,
	      [key|rest],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    
		    [ and,
		      [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx],
		      
		      [ or,
			
			[ u_memq_c63,
			  [quote, u_all],
			  
			  [ ext_assq,
			    [quote, ['#COMMA', key]],
			    u_xx_ndbg_interests_xx
			  ]
			],
			
			[ u_any_c63,
			  [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]],
			  
			  [ cdr,
			    
			    [ ext_assq,
			      [quote, ['#COMMA', key]],
			      u_xx_ndbg_interests_xx
			    ]
			  ]
			]
		      ]
		    ],
		    [progn, ['#BQ-COMMA-ELIPSE', rest]],
		    []
		  ]
		]
	      ]).


% annotating U::IF-INTERESTED-IN 
wl: arglist_info(u_if_interested_in,
		[key, '&rest', rest],
		[key, rest],
		arginfo{ all:[key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[key, rest],
			 opt:0,
			 req:[key],
			 rest:[rest],
			 sublists:0,
			 whole:0
		       }).


% annotating U::IF-INTERESTED-IN 
wl: init_args(1, u_if_interested_in).


% annotating U::IF-INTERESTED-IN 
f_u_if_interested_in(Key_Param, Rest_Param, FnResult) :-
	TLEnv3=[bv(key, Key_Param), bv(rest, Rest_Param)],
	get_var(TLEnv3, rest, Rest_Get),
	[if, [and, [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx]]]]], [progn|Rest_Get], []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_if_interested_in, classof, claz_macro),
   set_opv(u_if_interested_in, compile_as, kw_operator),
   set_opv(u_if_interested_in, function, f_u_if_interested_in),
   DefMacroResult=u_if_interested_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:798 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    ndbg,
			    ['dbg-stream', key|rest],
			    
			    [ '#BQ',
			      
			      [ progn,
				
				[ if,
				  
				  [ and,
				    
				    [ assq,
				      [quote, ['#COMMA', key]],
				      '*ndbg-interests*'
				    ],
				    
				    [ or,
				      
				      [ 'memq?',
					[quote, all],
					
					[ assq,
					  [quote, ['#COMMA', key]],
					  '*ndbg-interests*'
					]
				      ],
				      
				      [ 'any?',
					
					[ lambda,
					  [x],
					  ['memq?', x, '*ndbg-items*']
					],
					
					[ cdr,
					  
					  [ assq,
					    [quote, ['#COMMA', key]],
					    '*ndbg-interests*'
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ cond,
				    
				    [ ['eq?', ['#COMMA', 'dbg-stream'], t],
				      
				      [ format,
					['standard-output'],
					['#BQ-COMMA-ELIPSE', rest]
				      ],
				      t
				    ],
				    
				    [ ['#COMMA', 'dbg-stream'],
				      
				      [ format,
					['#COMMA', 'dbg-stream'],
					['#BQ-COMMA-ELIPSE', rest]
				      ],
				      t
				    ],
				    [else, []]
				  ],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::NDBG 
wl: lambda_def(defmacro,
	      u_ndbg,
	      f_u_ndbg,
	      [u_dbg_stream, key|rest],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ progn,
		    
		    [ if,
		      
		      [ and,
			
			[ ext_assq,
			  [quote, ['#COMMA', key]],
			  u_xx_ndbg_interests_xx
			],
			
			[ or,
			  
			  [ u_memq_c63,
			    [quote, u_all],
			    
			    [ ext_assq,
			      [quote, ['#COMMA', key]],
			      u_xx_ndbg_interests_xx
			    ]
			  ],
			  
			  [ u_any_c63,
			    [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]],
			    
			    [ cdr,
			      
			      [ ext_assq,
				[quote, ['#COMMA', key]],
				u_xx_ndbg_interests_xx
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ cond,
			
			[ [u_eq_c63, ['#COMMA', u_dbg_stream], t],
			  
			  [ format,
			    [u_standard_output],
			    ['#BQ-COMMA-ELIPSE', rest]
			  ],
			  t
			],
			
			[ ['#COMMA', u_dbg_stream],
			  
			  [ format,
			    ['#COMMA', u_dbg_stream],
			    ['#BQ-COMMA-ELIPSE', rest]
			  ],
			  t
			],
			[u_else, []]
		      ],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::NDBG 
wl: arglist_info(u_ndbg,
		[u_dbg_stream, key, '&rest', rest],
		[u_dbg_stream, key, rest],
		arginfo{ all:[u_dbg_stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_dbg_stream, key, rest],
			 opt:0,
			 req:[u_dbg_stream, key],
			 rest:[rest],
			 sublists:0,
			 whole:0
		       }).


% annotating U::NDBG 
wl: init_args(2, u_ndbg).


% annotating U::NDBG 
f_u_ndbg(Dbg_stream_Param, Key_Param, Rest_Param, FnResult) :-
	TLEnv3=[bv(u_dbg_stream, Dbg_stream_Param), bv(key, Key_Param), bv(rest, Rest_Param)],
	get_var(TLEnv3, rest, Rest_Get24),
	[progn, [if, [and, [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx]]]]], [cond, [[u_eq_c63, Dbg_stream_Param, t], [format, [u_standard_output]|Rest_Get24], t], [Dbg_stream_Param, [format, Dbg_stream_Param|Rest_Get24], t], [u_else, []]], []]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg, classof, claz_macro),
   set_opv(u_ndbg, compile_as, kw_operator),
   set_opv(u_ndbg, function, f_u_ndbg),
   DefMacroResult=u_ndbg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:798 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          (format (standard-output) \"~&\")",
				     1,
				     1116)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:798 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          (ndbg-indentation (standard-output))",
				     1,
				     1159)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:798 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          (format ,dbg-stream \"~&\")",
				     1,
				     1286)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:798 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          (ndbg-indentation ,dbg-stream)",
				     1,
				     1323)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1452 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-if',
			    [key, form],
			    
			    [ '#BQ',
			      
			      [ if,
				
				[ and,
				  
				  [ assq,
				    [quote, ['#COMMA', key]],
				    '*ndbg-interests*'
				  ],
				  
				  [ or,
				    
				    [ 'memq?',
				      [quote, all],
				      
				      [ assq,
					[quote, ['#COMMA', key]],
					'*ndbg-interests*'
				      ]
				    ],
				    
				    [ 'any?',
				      [lambda, [x], ['memq?', x, '*ndbg-items*']],
				      
				      [ cdr,
					
					[ assq,
					  [quote, ['#COMMA', key]],
					  '*ndbg-interests*'
					]
				      ]
				    ]
				  ]
				],
				['#COMMA', form]
			      ]
			    ]
			  ]).

% annotating U::NDBG-IF 
wl: lambda_def(defmacro,
	      u_ndbg_if,
	      f_u_ndbg_if,
	      [key, u_form],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    
		    [ and,
		      [ext_assq, [quote, ['#COMMA', key]], u_xx_ndbg_interests_xx],
		      
		      [ or,
			
			[ u_memq_c63,
			  [quote, u_all],
			  
			  [ ext_assq,
			    [quote, ['#COMMA', key]],
			    u_xx_ndbg_interests_xx
			  ]
			],
			
			[ u_any_c63,
			  [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]],
			  
			  [ cdr,
			    
			    [ ext_assq,
			      [quote, ['#COMMA', key]],
			      u_xx_ndbg_interests_xx
			    ]
			  ]
			]
		      ]
		    ],
		    ['#COMMA', u_form]
		  ]
		]
	      ]).


% annotating U::NDBG-IF 
wl: arglist_info(u_ndbg_if,
		[key, u_form],
		[Key_Param, Form_Param],
		arginfo{ all:[key, u_form],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[key, u_form],
			 opt:0,
			 req:[key, u_form],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-IF 
wl: init_args(exact_only, u_ndbg_if).


% annotating U::NDBG-IF 
f_u_ndbg_if(Key_Param, Form_Param, FnResult) :-
	[if, [and, [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx], [or, [u_memq_c63, [quote, u_all], [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx]], [u_any_c63, [lambda, [u_x], [u_memq_c63, u_x, u_xx_ndbg_items_xx]], [cdr, [ext_assq, [quote, Key_Param], u_xx_ndbg_interests_xx]]]]], Form_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_if, classof, claz_macro),
   set_opv(u_ndbg_if, compile_as, kw_operator),
   set_opv(u_ndbg_if, function, f_u_ndbg_if),
   DefMacroResult=u_ndbg_if.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1717 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'length-one?',
			    [x],
			    
			    [ '#BQ',
			      [and, ['#COMMA', x], ['null?', [cdr, ['#COMMA', x]]]]
			    ]
			  ]).

% annotating U::LENGTH-ONE? 
wl: lambda_def(defmacro,
	      u_length_one_c63,
	      f_u_length_one_c63,
	      [u_x],
	      
	      [ progn,
		['#BQ', [and, ['#COMMA', u_x], [u_null_c63, [cdr, ['#COMMA', u_x]]]]]
	      ]).


% annotating U::LENGTH-ONE? 
wl: arglist_info(u_length_one_c63,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::LENGTH-ONE? 
wl: init_args(exact_only, u_length_one_c63).


% annotating U::LENGTH-ONE? 
f_u_length_one_c63(X_Param, FnResult) :-
	[and, X_Param, [u_null_c63, [cdr, X_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_length_one_c63, classof, claz_macro),
   set_opv(u_length_one_c63, compile_as, kw_operator),
   set_opv(u_length_one_c63, function, f_u_length_one_c63),
   DefMacroResult=u_length_one_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1774 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'nil?',
			    [x],
			    
			    [ '#BQ',
			      
			      [ or,
				['null?', ['#COMMA', x]],
				['eq?', ['#COMMA', x], [quote, []]]
			      ]
			    ]
			  ]).

% annotating U::NIL? 
wl: lambda_def(defmacro,
	      u_nil_c63,
	      f_u_nil_c63,
	      [u_x],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ or,
		    [u_null_c63, ['#COMMA', u_x]],
		    [u_eq_c63, ['#COMMA', u_x], [quote, []]]
		  ]
		]
	      ]).


% annotating U::NIL? 
wl: arglist_info(u_nil_c63,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NIL? 
wl: init_args(exact_only, u_nil_c63).


% annotating U::NIL? 
f_u_nil_c63(X_Param, FnResult) :-
	[or, [u_null_c63, X_Param], [u_eq_c63, X_Param, [quote, []]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nil_c63, classof, claz_macro),
   set_opv(u_nil_c63, compile_as, kw_operator),
   set_opv(u_nil_c63, function, f_u_nil_c63),
   DefMacroResult=u_nil_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1835 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    symbolconc,
			    [sym, suffix],
			    
			    [ intern,
			      
			      [ 'CONCATENATE',
				[quote, string],
				[string, sym],
				[format, [], '$STRING'("~A"), suffix]
			      ]
			    ]
			  ]).

% annotating U::SYMBOLCONC 
wl: lambda_def(defun,
	      u_symbolconc,
	      f_u_symbolconc,
	      [u_sym, u_suffix],
	      
	      [ 
		[ intern,
		  
		  [ concatenate,
		    [quote, string],
		    [string, u_sym],
		    
		    [ format,
		      [],
		      '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		      u_suffix
		    ]
		  ]
		]
	      ]).


% annotating U::SYMBOLCONC 
wl: arglist_info(u_symbolconc,
		[u_sym, u_suffix],
		[Sym_Param, Suffix_Param],
		arginfo{ all:[u_sym, u_suffix],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_sym, u_suffix],
			 opt:0,
			 req:[u_sym, u_suffix],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SYMBOLCONC 
wl: init_args(exact_only, u_symbolconc).


% annotating U::SYMBOLCONC 
f_u_symbolconc(Sym_Param, Suffix_Param, FnResult) :-
	cl_string(Sym_Param, String_Ret),
	cl_format(
		  [ [],
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		    Suffix_Param
		  ],
		  Format_Ret),
	cl_concatenate(string, String_Ret, Format_Ret, Intern_Param),
	cl_intern(Intern_Param, Intern_Ret),
	Intern_Ret=FnResult.
:- set_opv(f_u_symbolconc, classof, claz_function),
   set_opv(u_symbolconc, compile_as, kw_function),
   set_opv(u_symbolconc, function, f_u_symbolconc),
   DefunResult=u_symbolconc.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:1936 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    pc,
			    ['context-abbr'],
			    
			    [ '#BQ',
			      
			      [ 'cx$print',
				
				[ eval,
				  
				  [ symbolconc,
				    [quote, 'CX.'],
				    ['#COMMA', 'context-abbr']
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::PC 
wl: lambda_def(defmacro,
	      u_pc,
	      f_u_pc,
	      [u_context_abbr],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_cx_c36_print,
		    
		    [ eval,
		      
		      [ u_symbolconc,
			[quote, u_cx_c46],
			['#COMMA', u_context_abbr]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::PC 
wl: arglist_info(u_pc,
		[u_context_abbr],
		[Context_abbr_Param],
		arginfo{ all:[u_context_abbr],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context_abbr],
			 opt:0,
			 req:[u_context_abbr],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PC 
wl: init_args(exact_only, u_pc).


% annotating U::PC 
f_u_pc(Context_abbr_Param, FnResult) :-
	[u_cx_c36_print, [eval, [u_symbolconc, [quote, u_cx_c46], Context_abbr_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_pc, classof, claz_macro),
   set_opv(u_pc, compile_as, kw_operator),
   set_opv(u_pc, function, f_u_pc),
   DefMacroResult=u_pc.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2019 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    pca,
			    ['context-abbr'],
			    
			    [ '#BQ',
			      
			      [ 'cx$print-ancestors',
				
				[ eval,
				  
				  [ symbolconc,
				    [quote, 'CX.'],
				    ['#COMMA', 'context-abbr']
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::PCA 
wl: lambda_def(defmacro,
	      u_pca,
	      f_u_pca,
	      [u_context_abbr],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_cx_c36_print_ancestors,
		    
		    [ eval,
		      
		      [ u_symbolconc,
			[quote, u_cx_c46],
			['#COMMA', u_context_abbr]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::PCA 
wl: arglist_info(u_pca,
		[u_context_abbr],
		[Context_abbr_Param],
		arginfo{ all:[u_context_abbr],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context_abbr],
			 opt:0,
			 req:[u_context_abbr],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PCA 
wl: init_args(exact_only, u_pca).


% annotating U::PCA 
f_u_pca(Context_abbr_Param, FnResult) :-
	[u_cx_c36_print_ancestors, [eval, [u_symbolconc, [quote, u_cx_c46], Context_abbr_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_pca, classof, claz_macro),
   set_opv(u_pca, compile_as, kw_operator),
   set_opv(u_pca, function, f_u_pca),
   DefMacroResult=u_pca.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2113 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'mem-empty-unify',
			    [ob, obs, context],
			    
			    [ '#BQ',
			      
			      [ mem,
				
				[ lambda,
				  [x, y],
				  
				  [ 'bd-and-empty-bd?',
				    
				    [ 'ob$unify-cx',
				      x,
				      y,
				      '*empty-bd*',
				      ['#COMMA', context]
				    ]
				  ]
				],
				['#COMMA', ob],
				['#COMMA', obs]
			      ]
			    ]
			  ]).

% annotating U::MEM-EMPTY-UNIFY 
wl: lambda_def(defmacro,
	      u_mem_empty_unify,
	      f_u_mem_empty_unify,
	      [u_ob, u_obs, u_context],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_mem,
		    
		    [ lambda,
		      [u_x, u_y],
		      
		      [ u_bd_and_empty_bd_c63,
			
			[ u_ob_c36_unify_cx,
			  u_x,
			  u_y,
			  u_xx_empty_bd_xx,
			  ['#COMMA', u_context]
			]
		      ]
		    ],
		    ['#COMMA', u_ob],
		    ['#COMMA', u_obs]
		  ]
		]
	      ]).


% annotating U::MEM-EMPTY-UNIFY 
wl: arglist_info(u_mem_empty_unify,
		[u_ob, u_obs, u_context],
		[Ob_Param, Obs_Param, Context_Param],
		arginfo{ all:[u_ob, u_obs, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_obs, u_context],
			 opt:0,
			 req:[u_ob, u_obs, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MEM-EMPTY-UNIFY 
wl: init_args(exact_only, u_mem_empty_unify).


% annotating U::MEM-EMPTY-UNIFY 
f_u_mem_empty_unify(Ob_Param, Obs_Param, Context_Param, FnResult) :-
	[u_mem, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Param]]], Ob_Param, Obs_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_empty_unify, classof, claz_macro),
   set_opv(u_mem_empty_unify, compile_as, kw_operator),
   set_opv(u_mem_empty_unify, function, f_u_mem_empty_unify),
   DefMacroResult=u_mem_empty_unify.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2265 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'mem-empty-unify?',
			    [ob, obs, context],
			    
			    [ '#BQ',
			      
			      [ 'mem?',
				
				[ lambda,
				  [x, y],
				  
				  [ 'bd-and-empty-bd?',
				    
				    [ 'ob$unify-cx',
				      x,
				      y,
				      '*empty-bd*',
				      ['#COMMA', context]
				    ]
				  ]
				],
				['#COMMA', ob],
				['#COMMA', obs]
			      ]
			    ]
			  ]).

% annotating U::MEM-EMPTY-UNIFY? 
wl: lambda_def(defmacro,
	      u_mem_empty_unify_c63,
	      f_u_mem_empty_unify_c63,
	      [u_ob, u_obs, u_context],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_mem_c63,
		    
		    [ lambda,
		      [u_x, u_y],
		      
		      [ u_bd_and_empty_bd_c63,
			
			[ u_ob_c36_unify_cx,
			  u_x,
			  u_y,
			  u_xx_empty_bd_xx,
			  ['#COMMA', u_context]
			]
		      ]
		    ],
		    ['#COMMA', u_ob],
		    ['#COMMA', u_obs]
		  ]
		]
	      ]).


% annotating U::MEM-EMPTY-UNIFY? 
wl: arglist_info(u_mem_empty_unify_c63,
		[u_ob, u_obs, u_context],
		[Ob_Param, Obs_Param, Context_Param],
		arginfo{ all:[u_ob, u_obs, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_obs, u_context],
			 opt:0,
			 req:[u_ob, u_obs, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MEM-EMPTY-UNIFY? 
wl: init_args(exact_only, u_mem_empty_unify_c63).


% annotating U::MEM-EMPTY-UNIFY? 
f_u_mem_empty_unify_c63(Ob_Param, Obs_Param, Context_Param, FnResult) :-
	[u_mem_c63, [lambda, [u_x, u_y], [u_bd_and_empty_bd_c63, [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Param]]], Ob_Param, Obs_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_empty_unify_c63, classof, claz_macro),
   set_opv(u_mem_empty_unify_c63, compile_as, kw_operator),
   set_opv(u_mem_empty_unify_c63, function, f_u_mem_empty_unify_c63),
   DefMacroResult=u_mem_empty_unify_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2419 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'mem-unify',
			    [ob, obs, context],
			    
			    [ '#BQ',
			      
			      [ mem,
				
				[ lambda,
				  [x, y],
				  
				  [ 'ob$unify-cx',
				    x,
				    y,
				    '*empty-bd*',
				    ['#COMMA', context]
				  ]
				],
				['#COMMA', ob],
				['#COMMA', obs]
			      ]
			    ]
			  ]).

% annotating U::MEM-UNIFY 
wl: lambda_def(defmacro,
	      u_mem_unify,
	      f_u_mem_unify,
	      [u_ob, u_obs, u_context],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_mem,
		    
		    [ lambda,
		      [u_x, u_y],
		      
		      [ u_ob_c36_unify_cx,
			u_x,
			u_y,
			u_xx_empty_bd_xx,
			['#COMMA', u_context]
		      ]
		    ],
		    ['#COMMA', u_ob],
		    ['#COMMA', u_obs]
		  ]
		]
	      ]).


% annotating U::MEM-UNIFY 
wl: arglist_info(u_mem_unify,
		[u_ob, u_obs, u_context],
		[Ob_Param, Obs_Param, Context_Param],
		arginfo{ all:[u_ob, u_obs, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_obs, u_context],
			 opt:0,
			 req:[u_ob, u_obs, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MEM-UNIFY 
wl: init_args(exact_only, u_mem_unify).


% annotating U::MEM-UNIFY 
f_u_mem_unify(Ob_Param, Obs_Param, Context_Param, FnResult) :-
	[u_mem, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Param]], Ob_Param, Obs_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_unify, classof, claz_macro),
   set_opv(u_mem_unify, compile_as, kw_operator),
   set_opv(u_mem_unify, function, f_u_mem_unify),
   DefMacroResult=u_mem_unify.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'mem-unify?',
			    [ob, obs, context],
			    
			    [ '#BQ',
			      
			      [ 'mem?',
				
				[ lambda,
				  [x, y],
				  
				  [ 'ob$unify-cx',
				    x,
				    y,
				    '*empty-bd*',
				    ['#COMMA', context]
				  ]
				],
				['#COMMA', ob],
				['#COMMA', obs]
			      ]
			    ]
			  ]).

% annotating U::MEM-UNIFY? 
wl: lambda_def(defmacro,
	      u_mem_unify_c63,
	      f_u_mem_unify_c63,
	      [u_ob, u_obs, u_context],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_mem_c63,
		    
		    [ lambda,
		      [u_x, u_y],
		      
		      [ u_ob_c36_unify_cx,
			u_x,
			u_y,
			u_xx_empty_bd_xx,
			['#COMMA', u_context]
		      ]
		    ],
		    ['#COMMA', u_ob],
		    ['#COMMA', u_obs]
		  ]
		]
	      ]).


% annotating U::MEM-UNIFY? 
wl: arglist_info(u_mem_unify_c63,
		[u_ob, u_obs, u_context],
		[Ob_Param, Obs_Param, Context_Param],
		arginfo{ all:[u_ob, u_obs, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_obs, u_context],
			 opt:0,
			 req:[u_ob, u_obs, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MEM-UNIFY? 
wl: init_args(exact_only, u_mem_unify_c63).


% annotating U::MEM-UNIFY? 
f_u_mem_unify_c63(Ob_Param, Obs_Param, Context_Param, FnResult) :-
	[u_mem_c63, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Param]], Ob_Param, Obs_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_unify_c63, classof, claz_macro),
   set_opv(u_mem_unify_c63, compile_as, kw_operator),
   set_opv(u_mem_unify_c63, function, f_u_mem_unify_c63),
   DefMacroResult=u_mem_unify_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'del-unify!',
			    [ob, obs, context],
			    
			    [ '#BQ',
			      
			      [ 'del!',
				
				[ lambda,
				  [x, y],
				  
				  [ 'ob$unify-cx',
				    x,
				    y,
				    '*empty-bd*',
				    ['#COMMA', context]
				  ]
				],
				['#COMMA', ob],
				['#COMMA', obs]
			      ]
			    ]
			  ]).

% annotating U::DEL-UNIFY! 
wl: lambda_def(defmacro,
	      u_del_unify_c33,
	      f_u_del_unify_c33,
	      [u_ob, u_obs, u_context],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_del_c33,
		    
		    [ lambda,
		      [u_x, u_y],
		      
		      [ u_ob_c36_unify_cx,
			u_x,
			u_y,
			u_xx_empty_bd_xx,
			['#COMMA', u_context]
		      ]
		    ],
		    ['#COMMA', u_ob],
		    ['#COMMA', u_obs]
		  ]
		]
	      ]).


% annotating U::DEL-UNIFY! 
wl: arglist_info(u_del_unify_c33,
		[u_ob, u_obs, u_context],
		[Ob_Param, Obs_Param, Context_Param],
		arginfo{ all:[u_ob, u_obs, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_obs, u_context],
			 opt:0,
			 req:[u_ob, u_obs, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DEL-UNIFY! 
wl: init_args(exact_only, u_del_unify_c33).


% annotating U::DEL-UNIFY! 
f_u_del_unify_c33(Ob_Param, Obs_Param, Context_Param, FnResult) :-
	[u_del_c33, [lambda, [u_x, u_y], [u_ob_c36_unify_cx, u_x, u_y, u_xx_empty_bd_xx, Context_Param]], Ob_Param, Obs_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_del_unify_c33, classof, claz_macro),
   set_opv(u_del_unify_c33, compile_as, kw_operator),
   set_opv(u_del_unify_c33, function, f_u_del_unify_c33),
   DefMacroResult=u_del_unify_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2753 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'retrieve-bd->ob',
			    [bd],
			    
			    [ '#BQ',
			      
			      [ map,
				[quote, list],
				[lambda, [x], [car, x]],
				['#COMMA', bd]
			      ]
			    ]
			  ]).

% annotating U::RETRIEVE-BD->OB 
wl: lambda_def(defmacro,
	      u_retrieve_bd_c62_ob,
	      f_u_retrieve_bd_c62_ob,
	      [u_bd],
	      
	      [ progn,
		
		[ '#BQ',
		  [map, [quote, list], [lambda, [u_x], [car, u_x]], ['#COMMA', u_bd]]
		]
	      ]).


% annotating U::RETRIEVE-BD->OB 
wl: arglist_info(u_retrieve_bd_c62_ob,
		[u_bd],
		[Bd_Param],
		arginfo{ all:[u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd],
			 opt:0,
			 req:[u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RETRIEVE-BD->OB 
wl: init_args(exact_only, u_retrieve_bd_c62_ob).


% annotating U::RETRIEVE-BD->OB 
f_u_retrieve_bd_c62_ob(Bd_Param, FnResult) :-
	[map, [quote, list], [lambda, [u_x], [car, u_x]], Bd_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_retrieve_bd_c62_ob, classof, claz_macro),
   set_opv(u_retrieve_bd_c62_ob, compile_as, kw_operator),
   set_opv(u_retrieve_bd_c62_ob, function, f_u_retrieve_bd_c62_ob),
   DefMacroResult=u_retrieve_bd_c62_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2826 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'cx?',
			    [x],
			    
			    [ '#BQ',
			      
			      [ and,
				['ob?', ['#COMMA', x]],
				['eq?', ['ob$ty', ['#COMMA', x]], '*cx-ob*']
			      ]
			    ]
			  ]).

% annotating U::CX? 
wl: lambda_def(defmacro,
	      u_cx_c63,
	      f_u_cx_c63,
	      [u_x],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ and,
		    [u_ob_c63, ['#COMMA', u_x]],
		    [u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_x]], u_xx_cx_ob_xx]
		  ]
		]
	      ]).


% annotating U::CX? 
wl: arglist_info(u_cx_c63,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX? 
wl: init_args(exact_only, u_cx_c63).


% annotating U::CX? 
f_u_cx_c63(X_Param, FnResult) :-
	[and, [u_ob_c63, X_Param], [u_eq_c63, [u_ob_c36_ty, X_Param], u_xx_cx_ob_xx]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_cx_c63, classof, claz_macro),
   set_opv(u_cx_c63, compile_as, kw_operator),
   set_opv(u_cx_c63, function, f_u_cx_c63),
   DefMacroResult=u_cx_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2897 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'touchable-fact?',
			    [fact],
			    
			    [ '#BQ',
			      
			      [ not,
				
				[ 'ty$instance?',
				  ['#COMMA', fact],
				  [quote, 'causal-link']
				]
			      ]
			    ]
			  ]).

% annotating U::TOUCHABLE-FACT? 
wl: lambda_def(defmacro,
	      u_touchable_fact_c63,
	      f_u_touchable_fact_c63,
	      [u_fact],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ not,
		    
		    [ u_ty_c36_instance_c63,
		      ['#COMMA', u_fact],
		      [quote, u_causal_link]
		    ]
		  ]
		]
	      ]).


% annotating U::TOUCHABLE-FACT? 
wl: arglist_info(u_touchable_fact_c63,
		[u_fact],
		[Fact_Param],
		arginfo{ all:[u_fact],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fact],
			 opt:0,
			 req:[u_fact],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TOUCHABLE-FACT? 
wl: init_args(exact_only, u_touchable_fact_c63).


% annotating U::TOUCHABLE-FACT? 
f_u_touchable_fact_c63(Fact_Param, FnResult) :-
	[not, [u_ty_c36_instance_c63, Fact_Param, [quote, u_causal_link]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_touchable_fact_c63, classof, claz_macro),
   set_opv(u_touchable_fact_c63, compile_as, kw_operator),
   set_opv(u_touchable_fact_c63, function, f_u_touchable_fact_c63),
   DefMacroResult=u_touchable_fact_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2975 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$instantiate2',
			    
			    [ template,
			      bindings,
			      depth,
			      'omit-slots',
			      'include-slots',
			      substit,
			      abstract,
			      'omit-proc'
			    ],
			    
			    [ '#BQ',
			      
			      [ if,
				'*unify-debugging?*',
				
				[ 'ob$instantiate-dbg',
				  ['#COMMA', template],
				  ['#COMMA', bindings],
				  ['#COMMA', depth],
				  ['#COMMA', 'omit-slots'],
				  ['#COMMA', 'include-slots'],
				  ['#COMMA', substit],
				  ['#COMMA', abstract],
				  ['#COMMA', 'omit-proc']
				],
				
				[ 'ob$instantiate3',
				  ['#COMMA', template],
				  ['#COMMA', bindings],
				  ['#COMMA', depth],
				  ['#COMMA', 'omit-slots'],
				  ['#COMMA', 'include-slots'],
				  ['#COMMA', substit],
				  ['#COMMA', abstract],
				  ['#COMMA', 'omit-proc']
				]
			      ]
			    ]
			  ]).

% annotating U::OB$INSTANTIATE2 
wl: lambda_def(defmacro,
	      u_ob_c36_instantiate2,
	      f_u_ob_c36_instantiate2,
	      
	      [ u_template,
		bindings,
		u_depth,
		u_omit_slots,
		u_include_slots,
		u_substit,
		u_abstract,
		u_omit_proc
	      ],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    u_xx_unify_debugging_c63_xx,
		    
		    [ u_ob_c36_instantiate_dbg,
		      ['#COMMA', u_template],
		      ['#COMMA', bindings],
		      ['#COMMA', u_depth],
		      ['#COMMA', u_omit_slots],
		      ['#COMMA', u_include_slots],
		      ['#COMMA', u_substit],
		      ['#COMMA', u_abstract],
		      ['#COMMA', u_omit_proc]
		    ],
		    
		    [ u_ob_c36_instantiate3,
		      ['#COMMA', u_template],
		      ['#COMMA', bindings],
		      ['#COMMA', u_depth],
		      ['#COMMA', u_omit_slots],
		      ['#COMMA', u_include_slots],
		      ['#COMMA', u_substit],
		      ['#COMMA', u_abstract],
		      ['#COMMA', u_omit_proc]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$INSTANTIATE2 
wl: arglist_info(u_ob_c36_instantiate2,
		
		[ u_template,
		  bindings,
		  u_depth,
		  u_omit_slots,
		  u_include_slots,
		  u_substit,
		  u_abstract,
		  u_omit_proc
		],
		
		[ Template_Param,
		  Bindings_Param,
		  Depth_Param,
		  Omit_slots_Param,
		  Include_slots_Param,
		  Substit_Param,
		  Abstract_Param,
		  Omit_proc_Param
		],
		arginfo{ all:
			     [ u_template,
			       bindings,
			       u_depth,
			       u_omit_slots,
			       u_include_slots,
			       u_substit,
			       u_abstract,
			       u_omit_proc
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_template,
				 bindings,
				 u_depth,
				 u_omit_slots,
				 u_include_slots,
				 u_substit,
				 u_abstract,
				 u_omit_proc
			       ],
			 opt:0,
			 req:
			     [ u_template,
			       bindings,
			       u_depth,
			       u_omit_slots,
			       u_include_slots,
			       u_substit,
			       u_abstract,
			       u_omit_proc
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTANTIATE2 
wl: init_args(exact_only, u_ob_c36_instantiate2).


% annotating U::OB$INSTANTIATE2 
f_u_ob_c36_instantiate2(Template_Param, Bindings_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, Substit_Param, Abstract_Param, Omit_proc_Param, FnResult) :-
	[if, u_xx_unify_debugging_c63_xx, [u_ob_c36_instantiate_dbg, Template_Param, Bindings_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, Substit_Param, Abstract_Param, Omit_proc_Param], [u_ob_c36_instantiate3, Template_Param, Bindings_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, Substit_Param, Abstract_Param, Omit_proc_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_instantiate2, classof, claz_macro),
   set_opv(u_ob_c36_instantiate2, compile_as, kw_operator),
   set_opv(u_ob_c36_instantiate2, function, f_u_ob_c36_instantiate2),
   DefMacroResult=u_ob_c36_instantiate2.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2975 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3502)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2975 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (ob? obj):", 1, 3504)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2975 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3517)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2975 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Determine if an arbitrary Lisp object is an ob.",
				     1,
				     3519)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:2975 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3569)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3570 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob?',
			    [obj],
			    ['#BQ', [typep, ['#COMMA', obj], [quote, obr]]]
			  ]).

% annotating U::OB? 
wl: lambda_def(defmacro,
	      u_ob_c63,
	      f_u_ob_c63,
	      [u_obj],
	      [progn, ['#BQ', [typep, ['#COMMA', u_obj], [quote, u_obr]]]]).


% annotating U::OB? 
wl: arglist_info(u_ob_c63,
		[u_obj],
		[Obj_Param],
		arginfo{ all:[u_obj],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obj],
			 opt:0,
			 req:[u_obj],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB? 
wl: init_args(exact_only, u_ob_c63).


% annotating U::OB? 
f_u_ob_c63(Obj_Param, FnResult) :-
	[typep, Obj_Param, [quote, u_obr]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c63, classof, claz_macro),
   set_opv(u_ob_c63, compile_as, kw_operator),
   set_opv(u_ob_c63, function, f_u_ob_c63),
   DefMacroResult=u_ob_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3611 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'enforce-ob',
			    [obj, routine],
			    
			    [ '#BQ',
			      
			      [ if,
				[not, ['ob?', ['#COMMA', obj]]],
				
				[ setq,
				  ['#COMMA', obj],
				  
				  [ error,
				    '$STRING'("~A: ~A not ob"),
				    ['#COMMA', routine],
				    ['#COMMA', obj]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::ENFORCE-OB 
wl: lambda_def(defmacro,
	      u_enforce_ob,
	      f_u_enforce_ob,
	      [u_obj, u_routine],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    [not, [u_ob_c63, ['#COMMA', u_obj]]],
		    
		    [ setq,
		      ['#COMMA', u_obj],
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(~),
				   #\('A'),
				   #\(:),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(n),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(o),
				   #\(b)
				 ]),
			['#COMMA', u_routine],
			['#COMMA', u_obj]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::ENFORCE-OB 
wl: arglist_info(u_enforce_ob,
		[u_obj, u_routine],
		[Obj_Param, Routine_Param],
		arginfo{ all:[u_obj, u_routine],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obj, u_routine],
			 opt:0,
			 req:[u_obj, u_routine],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ENFORCE-OB 
wl: init_args(exact_only, u_enforce_ob).


% annotating U::ENFORCE-OB 
f_u_enforce_ob(Obj_Param, Routine_Param, FnResult) :-
	[if, [not, [u_ob_c63, Obj_Param]], [setq, Obj_Param, [error, '$ARRAY'([*], claz_base_character, [#\(~), #\('A'), #\(:), #\(' '), #\(~), #\('A'), #\(' '), #\(n), #\(o), #\(t), #\(' '), #\(o), #\(b)]), Routine_Param, Obj_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_enforce_ob, classof, claz_macro),
   set_opv(u_enforce_ob, compile_as, kw_operator),
   set_opv(u_enforce_ob, function, f_u_enforce_ob),
   DefMacroResult=u_enforce_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3730 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$ty',
			    [ob],
			    ['#BQ', ['ob$get', ['#COMMA', ob], [quote, type]]]
			  ]).

% annotating U::OB$TY 
wl: lambda_def(defmacro,
	      u_ob_c36_ty,
	      f_u_ob_c36_ty,
	      [u_ob],
	      [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_ob], [quote, type]]]]).


% annotating U::OB$TY 
wl: arglist_info(u_ob_c36_ty,
		[u_ob],
		[Ob_Param],
		arginfo{ all:[u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob],
			 opt:0,
			 req:[u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$TY 
wl: init_args(exact_only, u_ob_c36_ty).


% annotating U::OB$TY 
f_u_ob_c36_ty(Ob_Param, FnResult) :-
	[u_ob_c36_get, Ob_Param, [quote, type]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_ty, classof, claz_macro),
   set_opv(u_ob_c36_ty, compile_as, kw_operator),
   set_opv(u_ob_c36_ty, function, f_u_ob_c36_ty),
   DefMacroResult=u_ob_c36_ty.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3774 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ty?',
			    [x],
			    
			    [ '#BQ',
			      
			      [ and,
				['ob?', ['#COMMA', x]],
				['eq?', ['ob$ty', ['#COMMA', x]], '*ty-ob*']
			      ]
			    ]
			  ]).

% annotating U::TY? 
wl: lambda_def(defmacro,
	      u_ty_c63,
	      f_u_ty_c63,
	      [u_x],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ and,
		    [u_ob_c63, ['#COMMA', u_x]],
		    [u_eq_c63, [u_ob_c36_ty, ['#COMMA', u_x]], u_xx_ty_ob_xx]
		  ]
		]
	      ]).


% annotating U::TY? 
wl: arglist_info(u_ty_c63,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY? 
wl: init_args(exact_only, u_ty_c63).


% annotating U::TY? 
f_u_ty_c63(X_Param, FnResult) :-
	[and, [u_ob_c63, X_Param], [u_eq_c63, [u_ob_c36_ty, X_Param], u_xx_ty_ob_xx]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ty_c63, classof, claz_macro),
   set_opv(u_ty_c63, compile_as, kw_operator),
   set_opv(u_ty_c63, function, f_u_ty_c63),
   DefMacroResult=u_ty_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3845 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'path->slot-name',
			    [path],
			    ['#BQ', [tlast, ['#COMMA', path]]]
			  ]).

% annotating U::PATH->SLOT-NAME 
wl: lambda_def(defmacro,
	      u_path_c62_slot_name,
	      f_u_path_c62_slot_name,
	      [u_path],
	      [progn, ['#BQ', [u_tlast, ['#COMMA', u_path]]]]).


% annotating U::PATH->SLOT-NAME 
wl: arglist_info(u_path_c62_slot_name,
		[u_path],
		[Path_Param],
		arginfo{ all:[u_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_path],
			 opt:0,
			 req:[u_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PATH->SLOT-NAME 
wl: init_args(exact_only, u_path_c62_slot_name).


% annotating U::PATH->SLOT-NAME 
f_u_path_c62_slot_name(Path_Param, FnResult) :-
	[u_tlast, Path_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_path_c62_slot_name, classof, claz_macro),
   set_opv(u_path_c62_slot_name, compile_as, kw_operator),
   set_opv(u_path_c62_slot_name, function, f_u_path_c62_slot_name),
   DefMacroResult=u_path_c62_slot_name.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3897 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'var?',
			    [x],
			    
			    [ '#BQ',
			      
			      [ and,
				['ob?', ['#COMMA', x]],
				['ty$instance?', ['#COMMA', x], [quote, uvar]]
			      ]
			    ]
			  ]).

% annotating U::VAR? 
wl: lambda_def(defmacro,
	      u_var_c63,
	      f_u_var_c63,
	      [u_x],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ and,
		    [u_ob_c63, ['#COMMA', u_x]],
		    [u_ty_c36_instance_c63, ['#COMMA', u_x], [quote, u_uvar]]
		  ]
		]
	      ]).


% annotating U::VAR? 
wl: arglist_info(u_var_c63,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VAR? 
wl: init_args(exact_only, u_var_c63).


% annotating U::VAR? 
f_u_var_c63(X_Param, FnResult) :-
	[and, [u_ob_c63, X_Param], [u_ty_c36_instance_c63, X_Param, [quote, u_uvar]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_var_c63, classof, claz_macro),
   set_opv(u_var_c63, compile_as, kw_operator),
   set_opv(u_var_c63, function, f_u_var_c63),
   DefMacroResult=u_var_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:3968 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'special?',
			    [x],
			    
			    [ '#BQ',
			      
			      [ and,
				['ob?', ['#COMMA', x]],
				['ty$instance?', ['#COMMA', x], [quote, uspecial]]
			      ]
			    ]
			  ]).

% annotating U::SPECIAL? 
wl: lambda_def(defmacro,
	      u_special_c63,
	      f_u_special_c63,
	      [u_x],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ and,
		    [u_ob_c63, ['#COMMA', u_x]],
		    [u_ty_c36_instance_c63, ['#COMMA', u_x], [quote, u_uspecial]]
		  ]
		]
	      ]).


% annotating U::SPECIAL? 
wl: arglist_info(u_special_c63,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SPECIAL? 
wl: init_args(exact_only, u_special_c63).


% annotating U::SPECIAL? 
f_u_special_c63(X_Param, FnResult) :-
	[and, [u_ob_c63, X_Param], [u_ty_c36_instance_c63, X_Param, [quote, u_uspecial]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_special_c63, classof, claz_macro),
   set_opv(u_special_c63, compile_as, kw_operator),
   set_opv(u_special_c63, function, f_u_special_c63),
   DefMacroResult=u_special_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4047 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'car-eq?',
			    [x, y],
			    
			    [ '#BQ',
			      
			      [ and,
				['pair?', ['#COMMA', x]],
				['eq?', [car, ['#COMMA', x]], ['#COMMA', y]]
			      ]
			    ]
			  ]).

% annotating U::CAR-EQ? 
wl: lambda_def(defmacro,
	      u_car_eq_c63,
	      f_u_car_eq_c63,
	      [u_x, u_y],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ and,
		    [u_pair_c63, ['#COMMA', u_x]],
		    [u_eq_c63, [car, ['#COMMA', u_x]], ['#COMMA', u_y]]
		  ]
		]
	      ]).


% annotating U::CAR-EQ? 
wl: arglist_info(u_car_eq_c63,
		[u_x, u_y],
		[X_Param, Y_Param],
		arginfo{ all:[u_x, u_y],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x, u_y],
			 opt:0,
			 req:[u_x, u_y],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CAR-EQ? 
wl: init_args(exact_only, u_car_eq_c63).


% annotating U::CAR-EQ? 
f_u_car_eq_c63(X_Param, Y_Param, FnResult) :-
	[and, [u_pair_c63, X_Param], [u_eq_c63, [car, X_Param], Y_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_car_eq_c63, classof, claz_macro),
   set_opv(u_car_eq_c63, compile_as, kw_operator),
   set_opv(u_car_eq_c63, function, f_u_car_eq_c63),
   DefMacroResult=u_car_eq_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4110 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'variable-name',
			    [x],
			    ['#BQ', ['ob$get', ['#COMMA', x], [quote, name]]]
			  ]).

% annotating U::VARIABLE-NAME 
wl: lambda_def(defmacro,
	      u_variable_name,
	      f_u_variable_name,
	      [u_x],
	      [progn, ['#BQ', [u_ob_c36_get, ['#COMMA', u_x], [quote, sys_name]]]]).


% annotating U::VARIABLE-NAME 
wl: arglist_info(u_variable_name,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VARIABLE-NAME 
wl: init_args(exact_only, u_variable_name).


% annotating U::VARIABLE-NAME 
f_u_variable_name(X_Param, FnResult) :-
	[u_ob_c36_get, X_Param, [quote, sys_name]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_variable_name, classof, claz_macro),
   set_opv(u_variable_name, compile_as, kw_operator),
   set_opv(u_variable_name, function, f_u_variable_name),
   DefMacroResult=u_variable_name.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4160 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'variable-type',
			    [x],
			    
			    [ '#BQ',
			      ['ob$get', ['#COMMA', x], [quote, 'unifies-with']]
			    ]
			  ]).

% annotating U::VARIABLE-TYPE 
wl: lambda_def(defmacro,
	      u_variable_type,
	      f_u_variable_type,
	      [u_x],
	      
	      [ progn,
		['#BQ', [u_ob_c36_get, ['#COMMA', u_x], [quote, u_unifies_with]]]
	      ]).


% annotating U::VARIABLE-TYPE 
wl: arglist_info(u_variable_type,
		[u_x],
		[X_Param],
		arginfo{ all:[u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x],
			 opt:0,
			 req:[u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VARIABLE-TYPE 
wl: init_args(exact_only, u_variable_type).


% annotating U::VARIABLE-TYPE 
f_u_variable_type(X_Param, FnResult) :-
	[u_ob_c36_get, X_Param, [quote, u_unifies_with]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_variable_type, classof, claz_macro),
   set_opv(u_variable_type, compile_as, kw_operator),
   set_opv(u_variable_type, function, f_u_variable_type),
   DefMacroResult=u_variable_type.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4160 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Setters: For consistency, access to slots in obr is done through",
				     1,
				     4219)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4160 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" these macros.", 1, 4286)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4302 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'set-obr-obnames',
			    [ob, val],
			    
			    [ '#BQ',
			      
			      [ setf,
				['obr-obnames', ['#COMMA', ob]],
				['#COMMA', val]
			      ]
			    ]
			  ]).

% annotating U::SET-OBR-OBNAMES 
wl: lambda_def(defmacro,
	      u_set_obr_obnames,
	      f_u_set_obr_obnames,
	      [u_ob, u_val],
	      
	      [ progn,
		
		[ '#BQ',
		  [setf, [u_obr_obnames, ['#COMMA', u_ob]], ['#COMMA', u_val]]
		]
	      ]).


% annotating U::SET-OBR-OBNAMES 
wl: arglist_info(u_set_obr_obnames,
		[u_ob, u_val],
		[Ob_Param, Val_Param],
		arginfo{ all:[u_ob, u_val],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_val],
			 opt:0,
			 req:[u_ob, u_val],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-OBR-OBNAMES 
wl: init_args(exact_only, u_set_obr_obnames).


% annotating U::SET-OBR-OBNAMES 
f_u_set_obr_obnames(Ob_Param, Val_Param, FnResult) :-
	[setf, [u_obr_obnames, Ob_Param], Val_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_set_obr_obnames, classof, claz_macro),
   set_opv(u_set_obr_obnames, compile_as, kw_operator),
   set_opv(u_set_obr_obnames, function, f_u_set_obr_obnames),
   DefMacroResult=u_set_obr_obnames.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4372 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'set-obr-slots',
			    [ob, val],
			    
			    [ '#BQ',
			      
			      [ setf,
				['obr-slots', ['#COMMA', ob]],
				['#COMMA', val]
			      ]
			    ]
			  ]).

% annotating U::SET-OBR-SLOTS 
wl: lambda_def(defmacro,
	      u_set_obr_slots,
	      f_u_set_obr_slots,
	      [u_ob, u_val],
	      
	      [ progn,
		['#BQ', [setf, [u_obr_slots, ['#COMMA', u_ob]], ['#COMMA', u_val]]]
	      ]).


% annotating U::SET-OBR-SLOTS 
wl: arglist_info(u_set_obr_slots,
		[u_ob, u_val],
		[Ob_Param, Val_Param],
		arginfo{ all:[u_ob, u_val],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_val],
			 opt:0,
			 req:[u_ob, u_val],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-OBR-SLOTS 
wl: init_args(exact_only, u_set_obr_slots).


% annotating U::SET-OBR-SLOTS 
f_u_set_obr_slots(Ob_Param, Val_Param, FnResult) :-
	[setf, [u_obr_slots, Ob_Param], Val_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_set_obr_slots, classof, claz_macro),
   set_opv(u_set_obr_slots, compile_as, kw_operator),
   set_opv(u_set_obr_slots, function, f_u_set_obr_slots),
   DefMacroResult=u_set_obr_slots.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'set-obr-literal',
			    [ob, val],
			    
			    [ '#BQ',
			      
			      [ setf,
				['obr-literal', ['#COMMA', ob]],
				['#COMMA', val]
			      ]
			    ]
			  ]).

% annotating U::SET-OBR-LITERAL 
wl: lambda_def(defmacro,
	      u_set_obr_literal,
	      f_u_set_obr_literal,
	      [u_ob, u_val],
	      
	      [ progn,
		
		[ '#BQ',
		  [setf, [u_obr_literal, ['#COMMA', u_ob]], ['#COMMA', u_val]]
		]
	      ]).


% annotating U::SET-OBR-LITERAL 
wl: arglist_info(u_set_obr_literal,
		[u_ob, u_val],
		[Ob_Param, Val_Param],
		arginfo{ all:[u_ob, u_val],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_val],
			 opt:0,
			 req:[u_ob, u_val],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-OBR-LITERAL 
wl: init_args(exact_only, u_set_obr_literal).


% annotating U::SET-OBR-LITERAL 
f_u_set_obr_literal(Ob_Param, Val_Param, FnResult) :-
	[setf, [u_obr_literal, Ob_Param], Val_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_set_obr_literal, classof, claz_macro),
   set_opv(u_set_obr_literal, compile_as, kw_operator),
   set_opv(u_set_obr_literal, function, f_u_set_obr_literal),
   DefMacroResult=u_set_obr_literal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4509)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Accessor functions for elements of the (obr-slots self) instance variable,",
				     1,
				     4511)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" which contains a triple of", 1, 4588)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("   slot-name", 1, 4617)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   slot-value (a single value--multiple values for a slot require",
				     1,
				     4631)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               multiple entries in (obr-slots self))",
				     1,
				     4698)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4438 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4752)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4754 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'slots-name',
			    [slots],
			    ['#BQ', [car, ['#COMMA', slots]]]
			  ]).

% annotating U::SLOTS-NAME 
wl: lambda_def(defmacro,
	      u_slots_name,
	      f_u_slots_name,
	      [sys_slots],
	      [progn, ['#BQ', [car, ['#COMMA', sys_slots]]]]).


% annotating U::SLOTS-NAME 
wl: arglist_info(u_slots_name,
		[sys_slots],
		[Slots_Param],
		arginfo{ all:[sys_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_slots],
			 opt:0,
			 req:[sys_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SLOTS-NAME 
wl: init_args(exact_only, u_slots_name).


% annotating U::SLOTS-NAME 
f_u_slots_name(Slots_Param, FnResult) :-
	[car, Slots_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_slots_name, classof, claz_macro),
   set_opv(u_slots_name, compile_as, kw_operator),
   set_opv(u_slots_name, function, f_u_slots_name),
   DefMacroResult=u_slots_name.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'slots-value',
			    [slots],
			    ['#BQ', [cadr, ['#COMMA', slots]]]
			  ]).

% annotating U::SLOTS-VALUE 
wl: lambda_def(defmacro,
	      u_slots_value,
	      f_u_slots_value,
	      [sys_slots],
	      [progn, ['#BQ', [cadr, ['#COMMA', sys_slots]]]]).


% annotating U::SLOTS-VALUE 
wl: arglist_info(u_slots_value,
		[sys_slots],
		[Slots_Param],
		arginfo{ all:[sys_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_slots],
			 opt:0,
			 req:[sys_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SLOTS-VALUE 
wl: init_args(exact_only, u_slots_value).


% annotating U::SLOTS-VALUE 
f_u_slots_value(Slots_Param, FnResult) :-
	[cadr, Slots_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_slots_value, classof, claz_macro),
   set_opv(u_slots_value, compile_as, kw_operator),
   set_opv(u_slots_value, function, f_u_slots_value),
   DefMacroResult=u_slots_value.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4846 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'with-unhidden-default',
			    ['&rest', body],
			    
			    [ '#BQ',
			      
			      [ 'unwind-protect',
				
				[ progn,
				  [setq, '*hidden-default*', []],
				  ['#BQ-COMMA-ELIPSE', body]
				],
				[setq, '*hidden-default*', t]
			      ]
			    ]
			  ]).

% annotating U::WITH-UNHIDDEN-DEFAULT 
wl: lambda_def(defmacro,
	      u_with_unhidden_default,
	      f_u_with_unhidden_default,
	      [c38_rest, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ unwind_protect,
		    
		    [ progn,
		      [setq, u_xx_hidden_default_xx, []],
		      ['#BQ-COMMA-ELIPSE', u_body]
		    ],
		    [setq, u_xx_hidden_default_xx, t]
		  ]
		]
	      ]).


% annotating U::WITH-UNHIDDEN-DEFAULT 
wl: arglist_info(u_with_unhidden_default,
		[c38_rest, u_body],
		[u_body],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 sublists:0,
			 whole:0
		       }).


% annotating U::WITH-UNHIDDEN-DEFAULT 
wl: init_args(rest_only, u_with_unhidden_default).


% annotating U::WITH-UNHIDDEN-DEFAULT 
f_u_with_unhidden_default(Whole, FnResult) :-
	append([], Body_Param, Whole),
	TLEnv3=[bv(u_body, Body_Param)],
	get_var(TLEnv3, u_body, Body_Get),
	[unwind_protect, [progn, [setq, u_xx_hidden_default_xx, []]|Body_Get], [setq, u_xx_hidden_default_xx, t]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_with_unhidden_default, classof, claz_macro),
   set_opv(u_with_unhidden_default, compile_as, kw_operator),
   set_opv(u_with_unhidden_default, function, f_u_with_unhidden_default),
   DefMacroResult=u_with_unhidden_default.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4846 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5012)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4846 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ob$create-empty: create a new empty ob",
				     1,
				     5014)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:4846 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5055)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5056 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$create-empty',
			    [],
			    [quote, ['ob$create-named-empty', []]]
			  ]).

% annotating U::OB$CREATE-EMPTY 
wl: lambda_def(defmacro,
	      u_ob_c36_create_empty,
	      f_u_ob_c36_create_empty,
	      [],
	      [progn, [quote, [u_ob_c36_create_named_empty, []]]]).


% annotating U::OB$CREATE-EMPTY 
wl: arglist_info(u_ob_c36_create_empty,
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

% annotating U::OB$CREATE-EMPTY 
wl: init_args(exact_only, u_ob_c36_create_empty).


% annotating U::OB$CREATE-EMPTY 
f_u_ob_c36_create_empty(FnResult) :-
	TLEnv3=[],
	[u_ob_c36_create_named_empty, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_create_empty, classof, claz_macro),
   set_opv(u_ob_c36_create_empty, compile_as, kw_operator),
   set_opv(u_ob_c36_create_empty, function, f_u_ob_c36_create_empty),
   DefMacroResult=u_ob_c36_create_empty.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5118 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-newline',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['do-newline', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-NEWLINE 
wl: lambda_def(defmacro,
	      u_ndbg_newline,
	      f_u_ndbg_newline,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_do_newline, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-NEWLINE 
wl: arglist_info(u_ndbg_newline,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-NEWLINE 
wl: init_args(exact_only, u_ndbg_newline).


% annotating U::NDBG-NEWLINE 
f_u_ndbg_newline(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_do_newline, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_newline, classof, claz_macro),
   set_opv(u_ndbg_newline, compile_as, kw_operator),
   set_opv(u_ndbg_newline, function, f_u_ndbg_newline),
   DefMacroResult=u_ndbg_newline.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-large-roman-font',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-large-roman-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-LARGE-ROMAN-FONT 
wl: lambda_def(defmacro,
	      u_ndbg_large_roman_font,
	      f_u_ndbg_large_roman_font,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_large_roman_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-LARGE-ROMAN-FONT 
wl: arglist_info(u_ndbg_large_roman_font,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-LARGE-ROMAN-FONT 
wl: init_args(exact_only, u_ndbg_large_roman_font).


% annotating U::NDBG-LARGE-ROMAN-FONT 
f_u_ndbg_large_roman_font(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_begin_large_roman_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_large_roman_font, classof, claz_macro),
   set_opv(u_ndbg_large_roman_font, compile_as, kw_operator),
   set_opv(u_ndbg_large_roman_font, function, f_u_ndbg_large_roman_font),
   DefMacroResult=u_ndbg_large_roman_font.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-large-bold-font',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-large-bold-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-LARGE-BOLD-FONT 
wl: lambda_def(defmacro,
	      u_ndbg_large_bold_font,
	      f_u_ndbg_large_bold_font,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_large_bold_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-LARGE-BOLD-FONT 
wl: arglist_info(u_ndbg_large_bold_font,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-LARGE-BOLD-FONT 
wl: init_args(exact_only, u_ndbg_large_bold_font).


% annotating U::NDBG-LARGE-BOLD-FONT 
f_u_ndbg_large_bold_font(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_begin_large_bold_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_large_bold_font, classof, claz_macro),
   set_opv(u_ndbg_large_bold_font, compile_as, kw_operator),
   set_opv(u_ndbg_large_bold_font, function, f_u_ndbg_large_bold_font),
   DefMacroResult=u_ndbg_large_bold_font.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5416 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-roman-font',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-roman-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-ROMAN-FONT 
wl: lambda_def(defmacro,
	      u_ndbg_roman_font,
	      f_u_ndbg_roman_font,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_roman_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-ROMAN-FONT 
wl: arglist_info(u_ndbg_roman_font,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-ROMAN-FONT 
wl: init_args(exact_only, u_ndbg_roman_font).


% annotating U::NDBG-ROMAN-FONT 
f_u_ndbg_roman_font(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_begin_roman_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_roman_font, classof, claz_macro),
   set_opv(u_ndbg_roman_font, compile_as, kw_operator),
   set_opv(u_ndbg_roman_font, function, f_u_ndbg_roman_font),
   DefMacroResult=u_ndbg_roman_font.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5511 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-bold-font',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-bold-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-BOLD-FONT 
wl: lambda_def(defmacro,
	      u_ndbg_bold_font,
	      f_u_ndbg_bold_font,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_bold_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-BOLD-FONT 
wl: arglist_info(u_ndbg_bold_font,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-BOLD-FONT 
wl: init_args(exact_only, u_ndbg_bold_font).


% annotating U::NDBG-BOLD-FONT 
f_u_ndbg_bold_font(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_begin_bold_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_bold_font, classof, claz_macro),
   set_opv(u_ndbg_bold_font, compile_as, kw_operator),
   set_opv(u_ndbg_bold_font, function, f_u_ndbg_bold_font),
   DefMacroResult=u_ndbg_bold_font.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5604 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-italic-font',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-italic-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-ITALIC-FONT 
wl: lambda_def(defmacro,
	      u_ndbg_italic_font,
	      f_u_ndbg_italic_font,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_italic_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-ITALIC-FONT 
wl: arglist_info(u_ndbg_italic_font,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-ITALIC-FONT 
wl: init_args(exact_only, u_ndbg_italic_font).


% annotating U::NDBG-ITALIC-FONT 
f_u_ndbg_italic_font(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_begin_italic_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_italic_font, classof, claz_macro),
   set_opv(u_ndbg_italic_font, compile_as, kw_operator),
   set_opv(u_ndbg_italic_font, function, f_u_ndbg_italic_font),
   DefMacroResult=u_ndbg_italic_font.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5701 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-slanted-font',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-slanted-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-SLANTED-FONT 
wl: lambda_def(defmacro,
	      u_ndbg_slanted_font,
	      f_u_ndbg_slanted_font,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_slanted_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-SLANTED-FONT 
wl: arglist_info(u_ndbg_slanted_font,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-SLANTED-FONT 
wl: init_args(exact_only, u_ndbg_slanted_font).


% annotating U::NDBG-SLANTED-FONT 
f_u_ndbg_slanted_font(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_begin_slanted_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_slanted_font, classof, claz_macro),
   set_opv(u_ndbg_slanted_font, compile_as, kw_operator),
   set_opv(u_ndbg_slanted_font, function, f_u_ndbg_slanted_font),
   DefMacroResult=u_ndbg_slanted_font.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5800 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-end-font',
			    [stream, key],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['end-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-END-FONT 
wl: lambda_def(defmacro,
	      u_ndbg_end_font,
	      f_u_ndbg_end_font,
	      [stream, key],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_end_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-END-FONT 
wl: arglist_info(u_ndbg_end_font,
		[stream, key],
		[Stream_Param, Key_Param],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, key],
			 opt:0,
			 req:[stream, key],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-END-FONT 
wl: init_args(exact_only, u_ndbg_end_font).


% annotating U::NDBG-END-FONT 
f_u_ndbg_end_font(Stream_Param, Key_Param, FnResult) :-
	[u_if_interested_in, Key_Param, [u_end_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_end_font, classof, claz_macro),
   set_opv(u_ndbg_end_font, compile_as, kw_operator),
   set_opv(u_ndbg_end_font, function, f_u_ndbg_end_font),
   DefMacroResult=u_ndbg_end_font.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:5885 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-roman',
			    [stream, key|rest],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-roman-font', ['#COMMA', stream]],
				
				[ ndbg,
				  ['#COMMA', stream],
				  ['#COMMA', key],
				  ['#BQ-COMMA-ELIPSE', rest]
				],
				['end-font', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-ROMAN 
wl: lambda_def(defmacro,
	      u_ndbg_roman,
	      f_u_ndbg_roman,
	      [stream, key|rest],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_roman_font, ['#COMMA', stream]],
		    
		    [ u_ndbg,
		      ['#COMMA', stream],
		      ['#COMMA', key],
		      ['#BQ-COMMA-ELIPSE', rest]
		    ],
		    [u_end_font, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-ROMAN 
wl: arglist_info(u_ndbg_roman,
		[stream, key, '&rest', rest],
		[stream, key, rest],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[stream, key, rest],
			 opt:0,
			 req:[stream, key],
			 rest:[rest],
			 sublists:0,
			 whole:0
		       }).


% annotating U::NDBG-ROMAN 
wl: init_args(2, u_ndbg_roman).


% annotating U::NDBG-ROMAN 
f_u_ndbg_roman(Stream_Param, Key_Param, Rest_Param, FnResult) :-
	TLEnv3=[bv(stream, Stream_Param), bv(key, Key_Param), bv(rest, Rest_Param)],
	get_var(TLEnv3, rest, Rest_Get),
	[u_if_interested_in, Key_Param, [u_begin_roman_font, Stream_Param], [u_ndbg, Stream_Param, Key_Param|Rest_Get], [u_end_font, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_roman, classof, claz_macro),
   set_opv(u_ndbg_roman, compile_as, kw_operator),
   set_opv(u_ndbg_roman, function, f_u_ndbg_roman),
   DefMacroResult=u_ndbg_roman.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6091 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ndbg-roman-nl',
			    [stream, key|rest],
			    
			    [ '#BQ',
			      
			      [ 'if-interested-in',
				['#COMMA', key],
				['begin-roman-font', ['#COMMA', stream]],
				
				[ ndbg,
				  ['#COMMA', stream],
				  ['#COMMA', key],
				  ['#BQ-COMMA-ELIPSE', rest]
				],
				['end-font', ['#COMMA', stream]],
				['do-newline', ['#COMMA', stream]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-ROMAN-NL 
wl: lambda_def(defmacro,
	      u_ndbg_roman_nl,
	      f_u_ndbg_roman_nl,
	      [stream, key|rest],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_if_interested_in,
		    ['#COMMA', key],
		    [u_begin_roman_font, ['#COMMA', stream]],
		    
		    [ u_ndbg,
		      ['#COMMA', stream],
		      ['#COMMA', key],
		      ['#BQ-COMMA-ELIPSE', rest]
		    ],
		    [u_end_font, ['#COMMA', stream]],
		    [u_do_newline, ['#COMMA', stream]]
		  ]
		]
	      ]).


% annotating U::NDBG-ROMAN-NL 
wl: arglist_info(u_ndbg_roman_nl,
		[stream, key, '&rest', rest],
		[stream, key, rest],
		arginfo{ all:[stream, key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[stream, key, rest],
			 opt:0,
			 req:[stream, key],
			 rest:[rest],
			 sublists:0,
			 whole:0
		       }).


% annotating U::NDBG-ROMAN-NL 
wl: init_args(2, u_ndbg_roman_nl).


% annotating U::NDBG-ROMAN-NL 
f_u_ndbg_roman_nl(Stream_Param, Key_Param, Rest_Param, FnResult) :-
	TLEnv3=[bv(stream, Stream_Param), bv(key, Key_Param), bv(rest, Rest_Param)],
	get_var(TLEnv3, rest, Rest_Get),
	[u_if_interested_in, Key_Param, [u_begin_roman_font, Stream_Param], [u_ndbg, Stream_Param, Key_Param|Rest_Get], [u_end_font, Stream_Param], [u_do_newline, Stream_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ndbg_roman_nl, classof, claz_macro),
   set_opv(u_ndbg_roman_nl, compile_as, kw_operator),
   set_opv(u_ndbg_roman_nl, function, f_u_ndbg_roman_nl),
   DefMacroResult=u_ndbg_roman_nl.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6342 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$create',
			    [spec],
			    ['#BQ', ['ob$readlist', ['#COMMA', spec]]]
			  ]).

% annotating U::OB$CREATE 
wl: lambda_def(defmacro,
	      u_ob_c36_create,
	      f_u_ob_c36_create,
	      [u_spec],
	      [progn, ['#BQ', [u_ob_c36_readlist, ['#COMMA', u_spec]]]]).


% annotating U::OB$CREATE 
wl: arglist_info(u_ob_c36_create,
		[u_spec],
		[Spec_Param],
		arginfo{ all:[u_spec],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_spec],
			 opt:0,
			 req:[u_spec],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$CREATE 
wl: init_args(exact_only, u_ob_c36_create).


% annotating U::OB$CREATE 
f_u_ob_c36_create(Spec_Param, FnResult) :-
	[u_ob_c36_readlist, Spec_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_create, classof, claz_macro),
   set_opv(u_ob_c36_create, compile_as, kw_operator),
   set_opv(u_ob_c36_create, function, f_u_ob_c36_create),
   DefMacroResult=u_ob_c36_create.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6394 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$fcreate',
			    [spec],
			    ['#BQ', ['ob$freadlist', ['#COMMA', spec]]]
			  ]).

% annotating U::OB$FCREATE 
wl: lambda_def(defmacro,
	      u_ob_c36_fcreate,
	      f_u_ob_c36_fcreate,
	      [u_spec],
	      [progn, ['#BQ', [u_ob_c36_freadlist, ['#COMMA', u_spec]]]]).


% annotating U::OB$FCREATE 
wl: arglist_info(u_ob_c36_fcreate,
		[u_spec],
		[Spec_Param],
		arginfo{ all:[u_spec],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_spec],
			 opt:0,
			 req:[u_spec],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$FCREATE 
wl: init_args(exact_only, u_ob_c36_fcreate).


% annotating U::OB$FCREATE 
f_u_ob_c36_fcreate(Spec_Param, FnResult) :-
	[u_ob_c36_freadlist, Spec_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_fcreate, classof, claz_macro),
   set_opv(u_ob_c36_fcreate, compile_as, kw_operator),
   set_opv(u_ob_c36_fcreate, function, f_u_ob_c36_fcreate),
   DefMacroResult=u_ob_c36_fcreate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6448 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'special-priority?',
			    [ob1, ob2],
			    
			    [ '#BQ',
			      
			      [ cond,
				[[not, ['special?', ['#COMMA', ob1]]], []],
				[[not, ['special?', ['#COMMA', ob2]]], t],
				
				[ 
				  [ 'eq?',
				    ['ob$ty', ['#COMMA', ob1]],
				    ['ob$ty', ['#COMMA', ob2]]
				  ],
				  t
				],
				
				[ else,
				  
				  [ 'memq?',
				    ['#COMMA', ob2],
				    
				    [ memq,
				      ['#COMMA', ob1],
				      '*special-priorities*'
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::SPECIAL-PRIORITY? 
wl: lambda_def(defmacro,
	      u_special_priority_c63,
	      f_u_special_priority_c63,
	      [u_ob1, u_ob2],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ cond,
		    [[not, [u_special_c63, ['#COMMA', u_ob1]]], []],
		    [[not, [u_special_c63, ['#COMMA', u_ob2]]], t],
		    
		    [ 
		      [ u_eq_c63,
			[u_ob_c36_ty, ['#COMMA', u_ob1]],
			[u_ob_c36_ty, ['#COMMA', u_ob2]]
		      ],
		      t
		    ],
		    
		    [ u_else,
		      
		      [ u_memq_c63,
			['#COMMA', u_ob2],
			[ext_memq, ['#COMMA', u_ob1], u_xx_special_priorities_xx]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::SPECIAL-PRIORITY? 
wl: arglist_info(u_special_priority_c63,
		[u_ob1, u_ob2],
		[Ob1_Param, Ob2_Param],
		arginfo{ all:[u_ob1, u_ob2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2],
			 opt:0,
			 req:[u_ob1, u_ob2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SPECIAL-PRIORITY? 
wl: init_args(exact_only, u_special_priority_c63).


% annotating U::SPECIAL-PRIORITY? 
f_u_special_priority_c63(Ob1_Param, Ob2_Param, FnResult) :-
	[cond, [[not, [u_special_c63, Ob1_Param]], []], [[not, [u_special_c63, Ob2_Param]], t], [[u_eq_c63, [u_ob_c36_ty, Ob1_Param], [u_ob_c36_ty, Ob2_Param]], t], [u_else, [u_memq_c63, Ob2_Param, [ext_memq, Ob1_Param, u_xx_special_priorities_xx]]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_special_priority_c63, classof, claz_macro),
   set_opv(u_special_priority_c63, compile_as, kw_operator),
   set_opv(u_special_priority_c63, function, f_u_special_priority_c63),
   DefMacroResult=u_special_priority_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6448 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" REALLY: one should really be memq? and the other memq.",
				     1,
				     6657)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:6714 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'old-special-priority?',
			    [ob1, ob2],
			    
			    [ '#BQ',
			      
			      [ cond,
				[[not, ['special?', ['#COMMA', ob1]]], []],
				[[not, ['special?', ['#COMMA', ob2]]], t],
				
				[ ['ty$instance?', ['#COMMA', ob1], [quote, uor]],
				  t
				],
				
				[ 
				  [ and,
				    
				    [ 'ty$instance?',
				      ['#COMMA', ob1],
				      [quote, uand]
				    ],
				    
				    [ 'ty$instance?',
				      ['#COMMA', ob2],
				      [quote, uor]
				    ]
				  ],
				  []
				],
				
				[ ['ty$instance?', ['#COMMA', ob1], [quote, uand]],
				  t
				],
				
				[ 
				  [ and,
				    
				    [ 'ty$instance?',
				      ['#COMMA', ob1],
				      [quote, unot]
				    ],
				    
				    [ or,
				      
				      [ 'ty$instance?',
					['#COMMA', ob2],
					[quote, uor]
				      ],
				      
				      [ 'ty$instance?',
					['#COMMA', ob2],
					[quote, uand]
				      ]
				    ]
				  ],
				  []
				],
				
				[ ['ty$instance?', ['#COMMA', ob1], [quote, unot]],
				  t
				],
				
				[ 
				  [ and,
				    
				    [ 'ty?instance?',
				      ['#COMMA', ob1],
				      [quote, uproc]
				    ],
				    
				    [ or,
				      
				      [ 'ty$instance?',
					['#COMMA', ob2],
					[quote, uor]
				      ],
				      
				      [ 'ty$instance?',
					['#COMMA', ob2],
					[quote, uand]
				      ],
				      
				      [ 'ty$instance?',
					['#COMMA', ob2],
					[quote, unot]
				      ]
				    ]
				  ],
				  []
				],
				[else, t]
			      ]
			    ]
			  ]).

% annotating U::OLD-SPECIAL-PRIORITY? 
wl: lambda_def(defmacro,
	      u_old_special_priority_c63,
	      f_u_old_special_priority_c63,
	      [u_ob1, u_ob2],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ cond,
		    [[not, [u_special_c63, ['#COMMA', u_ob1]]], []],
		    [[not, [u_special_c63, ['#COMMA', u_ob2]]], t],
		    [[u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uor]], t],
		    
		    [ 
		      [ and,
			
			[ u_ty_c36_instance_c63,
			  ['#COMMA', u_ob1],
			  [quote, u_uand]
			],
			[u_ty_c36_instance_c63, ['#COMMA', u_ob2], [quote, u_uor]]
		      ],
		      []
		    ],
		    
		    [ [u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_uand]],
		      t
		    ],
		    
		    [ 
		      [ and,
			
			[ u_ty_c36_instance_c63,
			  ['#COMMA', u_ob1],
			  [quote, u_unot]
			],
			
			[ or,
			  
			  [ u_ty_c36_instance_c63,
			    ['#COMMA', u_ob2],
			    [quote, u_uor]
			  ],
			  
			  [ u_ty_c36_instance_c63,
			    ['#COMMA', u_ob2],
			    [quote, u_uand]
			  ]
			]
		      ],
		      []
		    ],
		    
		    [ [u_ty_c36_instance_c63, ['#COMMA', u_ob1], [quote, u_unot]],
		      t
		    ],
		    
		    [ 
		      [ and,
			
			[ u_ty_c63_instance_c63,
			  ['#COMMA', u_ob1],
			  [quote, u_uproc]
			],
			
			[ or,
			  
			  [ u_ty_c36_instance_c63,
			    ['#COMMA', u_ob2],
			    [quote, u_uor]
			  ],
			  
			  [ u_ty_c36_instance_c63,
			    ['#COMMA', u_ob2],
			    [quote, u_uand]
			  ],
			  
			  [ u_ty_c36_instance_c63,
			    ['#COMMA', u_ob2],
			    [quote, u_unot]
			  ]
			]
		      ],
		      []
		    ],
		    [u_else, t]
		  ]
		]
	      ]).


% annotating U::OLD-SPECIAL-PRIORITY? 
wl: arglist_info(u_old_special_priority_c63,
		[u_ob1, u_ob2],
		[Ob1_Param, Ob2_Param],
		arginfo{ all:[u_ob1, u_ob2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2],
			 opt:0,
			 req:[u_ob1, u_ob2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OLD-SPECIAL-PRIORITY? 
wl: init_args(exact_only, u_old_special_priority_c63).


% annotating U::OLD-SPECIAL-PRIORITY? 
f_u_old_special_priority_c63(Ob1_Param, Ob2_Param, FnResult) :-
	[cond, [[not, [u_special_c63, Ob1_Param]], []], [[not, [u_special_c63, Ob2_Param]], t], [[u_ty_c36_instance_c63, Ob1_Param, [quote, u_uor]], t], [[and, [u_ty_c36_instance_c63, Ob1_Param, [quote, u_uand]], [u_ty_c36_instance_c63, Ob2_Param, [quote, u_uor]]], []], [[u_ty_c36_instance_c63, Ob1_Param, [quote, u_uand]], t], [[and, [u_ty_c36_instance_c63, Ob1_Param, [quote, u_unot]], [or, [u_ty_c36_instance_c63, Ob2_Param, [quote, u_uor]], [u_ty_c36_instance_c63, Ob2_Param, [quote, u_uand]]]], []], [[u_ty_c36_instance_c63, Ob1_Param, [quote, u_unot]], t], [[and, [u_ty_c63_instance_c63, Ob1_Param, [quote, u_uproc]], [or, [u_ty_c36_instance_c63, Ob2_Param, [quote, u_uor]], [u_ty_c36_instance_c63, Ob2_Param, [quote, u_uand]], [u_ty_c36_instance_c63, Ob2_Param, [quote, u_unot]]]], []], [u_else, t]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_old_special_priority_c63, classof, claz_macro),
   set_opv(u_old_special_priority_c63, compile_as, kw_operator),
   set_opv(u_old_special_priority_c63, function, f_u_old_special_priority_c63),
   DefMacroResult=u_old_special_priority_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7321 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'var-ty$instance?',
			    [x, y],
			    
			    [ '#BQ',
			      
			      [ if,
				['null?', ['#COMMA', y]],
				t,
				
				[ and,
				  ['ob?', ['#COMMA', x]],
				  
				  [ 'ty$instance-of?',
				    ['#COMMA', x],
				    ['#COMMA', y]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::VAR-TY$INSTANCE? 
wl: lambda_def(defmacro,
	      u_var_ty_c36_instance_c63,
	      f_u_var_ty_c36_instance_c63,
	      [u_x, u_y],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    [u_null_c63, ['#COMMA', u_y]],
		    t,
		    
		    [ and,
		      [u_ob_c63, ['#COMMA', u_x]],
		      [u_ty_c36_instance_of_c63, ['#COMMA', u_x], ['#COMMA', u_y]]
		    ]
		  ]
		]
	      ]).


% annotating U::VAR-TY$INSTANCE? 
wl: arglist_info(u_var_ty_c36_instance_c63,
		[u_x, u_y],
		[X_Param, Y_Param],
		arginfo{ all:[u_x, u_y],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x, u_y],
			 opt:0,
			 req:[u_x, u_y],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VAR-TY$INSTANCE? 
wl: init_args(exact_only, u_var_ty_c36_instance_c63).


% annotating U::VAR-TY$INSTANCE? 
f_u_var_ty_c36_instance_c63(X_Param, Y_Param, FnResult) :-
	[if, [u_null_c63, Y_Param], t, [and, [u_ob_c63, X_Param], [u_ty_c36_instance_of_c63, X_Param, Y_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_var_ty_c36_instance_c63, classof, claz_macro),
   set_opv(u_var_ty_c36_instance_c63, compile_as, kw_operator),
   set_opv(u_var_ty_c36_instance_c63, function, f_u_var_ty_c36_instance_c63),
   DefMacroResult=u_var_ty_c36_instance_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7442 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'type-compatible-vars?',
			    [var1, var2],
			    
			    [ '#BQ',
			      
			      [ or,
				'*relax-unify-var*',
				['null?', ['variable-type', ['#COMMA', var1]]],
				['null?', ['variable-type', ['#COMMA', var2]]],
				
				[ 'memq?',
				  ['variable-type', ['#COMMA', var1]],
				  
				  [ 'ty$supertypes*',
				    ['variable-type', ['#COMMA', var2]]
				  ]
				],
				
				[ 'memq?',
				  ['variable-type', ['#COMMA', var2]],
				  
				  [ 'ty$supertypes*',
				    ['variable-type', ['#COMMA', var1]]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::TYPE-COMPATIBLE-VARS? 
wl: lambda_def(defmacro,
	      u_type_compatible_vars_c63,
	      f_u_type_compatible_vars_c63,
	      [u_var1, u_var2],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ or,
		    u_xx_relax_unify_var_xx,
		    [u_null_c63, [u_variable_type, ['#COMMA', u_var1]]],
		    [u_null_c63, [u_variable_type, ['#COMMA', u_var2]]],
		    
		    [ u_memq_c63,
		      [u_variable_type, ['#COMMA', u_var1]],
		      
		      [ u_ty_c36_supertypes_xx,
			[u_variable_type, ['#COMMA', u_var2]]
		      ]
		    ],
		    
		    [ u_memq_c63,
		      [u_variable_type, ['#COMMA', u_var2]],
		      
		      [ u_ty_c36_supertypes_xx,
			[u_variable_type, ['#COMMA', u_var1]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::TYPE-COMPATIBLE-VARS? 
wl: arglist_info(u_type_compatible_vars_c63,
		[u_var1, u_var2],
		[Var1_Param, Var2_Param],
		arginfo{ all:[u_var1, u_var2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var1, u_var2],
			 opt:0,
			 req:[u_var1, u_var2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TYPE-COMPATIBLE-VARS? 
wl: init_args(exact_only, u_type_compatible_vars_c63).


% annotating U::TYPE-COMPATIBLE-VARS? 
f_u_type_compatible_vars_c63(Var1_Param, Var2_Param, FnResult) :-
	[or, u_xx_relax_unify_var_xx, [u_null_c63, [u_variable_type, Var1_Param]], [u_null_c63, [u_variable_type, Var2_Param]], [u_memq_c63, [u_variable_type, Var1_Param], [u_ty_c36_supertypes_xx, [u_variable_type, Var2_Param]]], [u_memq_c63, [u_variable_type, Var2_Param], [u_ty_c36_supertypes_xx, [u_variable_type, Var1_Param]]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_type_compatible_vars_c63, classof, claz_macro),
   set_opv(u_type_compatible_vars_c63, compile_as, kw_operator),
   set_opv(u_type_compatible_vars_c63, function, f_u_type_compatible_vars_c63),
   DefMacroResult=u_type_compatible_vars_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7736 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'with-inverse-setting-default-off',
			    ['&rest', body],
			    
			    [ '#BQ',
			      
			      [ let,
				[[result, []]],
				['inverse-setting-default-off'],
				
				[ setq,
				  result,
				  [progn, ['#BQ-COMMA-ELIPSE', body]]
				],
				['inverse-setting-default-on'],
				result
			      ]
			    ]
			  ]).

% annotating U::WITH-INVERSE-SETTING-DEFAULT-OFF 
wl: lambda_def(defmacro,
	      u_with_inverse_setting_default_off,
	      f_u_with_inverse_setting_default_off,
	      [c38_rest, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    [[u_result, []]],
		    [u_inverse_setting_default_off],
		    [setq, u_result, [progn, ['#BQ-COMMA-ELIPSE', u_body]]],
		    [u_inverse_setting_default_on],
		    u_result
		  ]
		]
	      ]).


% annotating U::WITH-INVERSE-SETTING-DEFAULT-OFF 
wl: arglist_info(u_with_inverse_setting_default_off,
		[c38_rest, u_body],
		[u_body],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 sublists:0,
			 whole:0
		       }).


% annotating U::WITH-INVERSE-SETTING-DEFAULT-OFF 
wl: init_args(rest_only, u_with_inverse_setting_default_off).


% annotating U::WITH-INVERSE-SETTING-DEFAULT-OFF 
f_u_with_inverse_setting_default_off(Whole, FnResult) :-
	append([], Body_Param, Whole),
	TLEnv3=[bv(u_body, Body_Param)],
	get_var(TLEnv3, u_body, Body_Get),
	[let, [[u_result, []]], [u_inverse_setting_default_off], [setq, u_result, [progn|Body_Get]], [u_inverse_setting_default_on], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_with_inverse_setting_default_off, classof, claz_macro),
   set_opv(u_with_inverse_setting_default_off, compile_as, kw_operator),
   set_opv(u_with_inverse_setting_default_off,
	   function,
	   f_u_with_inverse_setting_default_off),
   DefMacroResult=u_with_inverse_setting_default_off.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7933 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'bd-bind',
			    [var, value, bindings],
			    
			    [ '#BQ',
			      
			      [ if,
				['#COMMA', var],
				
				[ cons,
				  [quote, t],
				  
				  [ cons,
				    [list, ['#COMMA', var], ['#COMMA', value]],
				    [cdr, ['#COMMA', bindings]]
				  ]
				],
				['#COMMA', bindings]
			      ]
			    ]
			  ]).

% annotating U::BD-BIND 
wl: lambda_def(defmacro,
	      u_bd_bind,
	      f_u_bd_bind,
	      [u_var, u_value, bindings],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    ['#COMMA', u_var],
		    
		    [ cons,
		      [quote, t],
		      
		      [ cons,
			[list, ['#COMMA', u_var], ['#COMMA', u_value]],
			[cdr, ['#COMMA', bindings]]
		      ]
		    ],
		    ['#COMMA', bindings]
		  ]
		]
	      ]).


% annotating U::BD-BIND 
wl: arglist_info(u_bd_bind,
		[u_var, u_value, bindings],
		[Var_Param, Value_Param, Bindings_Param],
		arginfo{ all:[u_var, u_value, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var, u_value, bindings],
			 opt:0,
			 req:[u_var, u_value, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-BIND 
wl: init_args(exact_only, u_bd_bind).


% annotating U::BD-BIND 
f_u_bd_bind(Var_Param, Value_Param, Bindings_Param, FnResult) :-
	[if, Var_Param, [cons, [quote, t], [cons, [list, Var_Param, Value_Param], [cdr, Bindings_Param]]], Bindings_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_bind, classof, claz_macro),
   set_opv(u_bd_bind, compile_as, kw_operator),
   set_opv(u_bd_bind, function, f_u_bd_bind),
   DefMacroResult=u_bd_bind.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:7933 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" this is for ob$unify-var which might pass a null var name.",
				     13,
				     7985)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8124 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'bd-bind!',
			    [var, value, bindings],
			    
			    [ '#BQ',
			      
			      [ setf,
				[cdr, ['#COMMA', bindings]],
				
				[ cons,
				  [list, ['#COMMA', var], ['#COMMA', value]],
				  [cdr, ['#COMMA', bindings]]
				]
			      ]
			    ]
			  ]).

% annotating U::BD-BIND! 
wl: lambda_def(defmacro,
	      u_bd_bind_c33,
	      f_u_bd_bind_c33,
	      [u_var, u_value, bindings],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ setf,
		    [cdr, ['#COMMA', bindings]],
		    
		    [ cons,
		      [list, ['#COMMA', u_var], ['#COMMA', u_value]],
		      [cdr, ['#COMMA', bindings]]
		    ]
		  ]
		]
	      ]).


% annotating U::BD-BIND! 
wl: arglist_info(u_bd_bind_c33,
		[u_var, u_value, bindings],
		[Var_Param, Value_Param, Bindings_Param],
		arginfo{ all:[u_var, u_value, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var, u_value, bindings],
			 opt:0,
			 req:[u_var, u_value, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-BIND! 
wl: init_args(exact_only, u_bd_bind_c33).


% annotating U::BD-BIND! 
f_u_bd_bind_c33(Var_Param, Value_Param, Bindings_Param, FnResult) :-
	[setf, [cdr, Bindings_Param], [cons, [list, Var_Param, Value_Param], [cdr, Bindings_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_bind_c33, classof, claz_macro),
   set_opv(u_bd_bind_c33, compile_as, kw_operator),
   set_opv(u_bd_bind_c33, function, f_u_bd_bind_c33),
   DefMacroResult=u_bd_bind_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8124 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (if (null? bindings) (error \"bd-bind!: null bindings))",
				     1,
				     8165)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8292 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'bd-lookup',
			    [var, bindings],
			    
			    [ '#BQ',
			      
			      [ and,
				['#COMMA', bindings],
				
				[ let,
				  
				  [ 
				    [ found,
				      
				      [ assq,
					['#COMMA', var],
					[cdr, ['#COMMA', bindings]]
				      ]
				    ]
				  ],
				  [if, found, [cadr, found], []]
				]
			      ]
			    ]
			  ]).

% annotating U::BD-LOOKUP 
wl: lambda_def(defmacro,
	      u_bd_lookup,
	      f_u_bd_lookup,
	      [u_var, bindings],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ and,
		    ['#COMMA', bindings],
		    
		    [ let,
		      
		      [ 
			[ u_found,
			  
			  [ ext_assq,
			    ['#COMMA', u_var],
			    [cdr, ['#COMMA', bindings]]
			  ]
			]
		      ],
		      [if, u_found, [cadr, u_found], []]
		    ]
		  ]
		]
	      ]).


% annotating U::BD-LOOKUP 
wl: arglist_info(u_bd_lookup,
		[u_var, bindings],
		[Var_Param, Bindings_Param],
		arginfo{ all:[u_var, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var, bindings],
			 opt:0,
			 req:[u_var, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-LOOKUP 
wl: init_args(exact_only, u_bd_lookup).


% annotating U::BD-LOOKUP 
f_u_bd_lookup(Var_Param, Bindings_Param, FnResult) :-
	[and, Bindings_Param, [let, [[u_found, [ext_assq, Var_Param, [cdr, Bindings_Param]]]], [if, u_found, [cadr, u_found], []]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_lookup, classof, claz_macro),
   set_opv(u_bd_lookup, compile_as, kw_operator),
   set_opv(u_bd_lookup, function, f_u_bd_lookup),
   DefMacroResult=u_bd_lookup.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'bd-hyper-lookup',
			    [var, bd],
			    
			    [ '#BQ',
			      
			      [ 'bd-hyper-lookup1',
				['#COMMA', var],
				['#COMMA', bd],
				[],
				[]
			      ]
			    ]
			  ]).

% annotating U::BD-HYPER-LOOKUP 
wl: lambda_def(defmacro,
	      u_bd_hyper_lookup,
	      f_u_bd_hyper_lookup,
	      [u_var, u_bd],
	      
	      [ progn,
		
		[ '#BQ',
		  [u_bd_hyper_lookup1, ['#COMMA', u_var], ['#COMMA', u_bd], [], []]
		]
	      ]).


% annotating U::BD-HYPER-LOOKUP 
wl: arglist_info(u_bd_hyper_lookup,
		[u_var, u_bd],
		[Var_Param, Bd_Param],
		arginfo{ all:[u_var, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var, u_bd],
			 opt:0,
			 req:[u_var, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-HYPER-LOOKUP 
wl: init_args(exact_only, u_bd_hyper_lookup).


% annotating U::BD-HYPER-LOOKUP 
f_u_bd_hyper_lookup(Var_Param, Bd_Param, FnResult) :-
	[u_bd_hyper_lookup1, Var_Param, Bd_Param, [], []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_bd_hyper_lookup, classof, claz_macro),
   set_opv(u_bd_hyper_lookup, compile_as, kw_operator),
   set_opv(u_bd_hyper_lookup, function, f_u_bd_hyper_lookup),
   DefMacroResult=u_bd_hyper_lookup.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8438 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8515)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Extra level to print debugging information.",
				     1,
				     8517)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Should never be used from the top-level.",
				     1,
				     8563)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8438 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8606)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8607 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$unify2',
			    [ob1, ob2, bindings, 'ignore-slots'],
			    
			    [ '#BQ',
			      
			      [ if,
				'*unify-debugging?*',
				
				[ 'ob$unify-dbg',
				  ['#COMMA', ob1],
				  ['#COMMA', ob2],
				  ['#COMMA', bindings],
				  ['#COMMA', 'ignore-slots']
				],
				
				[ 'ob$unify0',
				  ['#COMMA', ob1],
				  ['#COMMA', ob2],
				  ['#COMMA', bindings],
				  ['#COMMA', 'ignore-slots']
				]
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY2 
wl: lambda_def(defmacro,
	      u_ob_c36_unify2,
	      f_u_ob_c36_unify2,
	      [u_ob1, u_ob2, bindings, u_ignore_slots],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    u_xx_unify_debugging_c63_xx,
		    
		    [ u_ob_c36_unify_dbg,
		      ['#COMMA', u_ob1],
		      ['#COMMA', u_ob2],
		      ['#COMMA', bindings],
		      ['#COMMA', u_ignore_slots]
		    ],
		    
		    [ u_ob_c36_unify0,
		      ['#COMMA', u_ob1],
		      ['#COMMA', u_ob2],
		      ['#COMMA', bindings],
		      ['#COMMA', u_ignore_slots]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$UNIFY2 
wl: arglist_info(u_ob_c36_unify2,
		[u_ob1, u_ob2, bindings, u_ignore_slots],
		[Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY2 
wl: init_args(exact_only, u_ob_c36_unify2).


% annotating U::OB$UNIFY2 
f_u_ob_c36_unify2(Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param, FnResult) :-
	[if, u_xx_unify_debugging_c63_xx, [u_ob_c36_unify_dbg, Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param], [u_ob_c36_unify0, Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify2, classof, claz_macro),
   set_opv(u_ob_c36_unify2, compile_as, kw_operator),
   set_opv(u_ob_c36_unify2, function, f_u_ob_c36_unify2),
   DefMacroResult=u_ob_c36_unify2.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8607 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8798)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8607 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level unifier call", 1, 8800)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8607 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8825)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8826 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$unify1',
			    [ob1, ob2, bindings, 'ignore-slots'],
			    
			    [ '#BQ',
			      
			      [ let,
				
				[ ['already-matched', '*already-matched*'],
				  [result, []]
				],
				[setq, '*diff?*', []],
				[setq, '*already-matched*', [cons, t, []]],
				
				[ setq,
				  result,
				  
				  [ 'ob$unify2',
				    ['#COMMA', ob1],
				    ['#COMMA', ob2],
				    ['#COMMA', bindings],
				    ['#COMMA', 'ignore-slots']
				  ]
				],
				[setq, '*already-matched*', 'already-matched'],
				result
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY1 
wl: lambda_def(defmacro,
	      u_ob_c36_unify1,
	      f_u_ob_c36_unify1,
	      [u_ob1, u_ob2, bindings, u_ignore_slots],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    
		    [ [u_already_matched, u_xx_already_matched_xx],
		      [u_result, []]
		    ],
		    [setq, u_xx_diff_c63_xx, []],
		    [setq, u_xx_already_matched_xx, [cons, t, []]],
		    
		    [ setq,
		      u_result,
		      
		      [ u_ob_c36_unify2,
			['#COMMA', u_ob1],
			['#COMMA', u_ob2],
			['#COMMA', bindings],
			['#COMMA', u_ignore_slots]
		      ]
		    ],
		    [setq, u_xx_already_matched_xx, u_already_matched],
		    u_result
		  ]
		]
	      ]).


% annotating U::OB$UNIFY1 
wl: arglist_info(u_ob_c36_unify1,
		[u_ob1, u_ob2, bindings, u_ignore_slots],
		[Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY1 
wl: init_args(exact_only, u_ob_c36_unify1).


% annotating U::OB$UNIFY1 
f_u_ob_c36_unify1(Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param, FnResult) :-
	[let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, []], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify1, classof, claz_macro),
   set_opv(u_ob_c36_unify1, compile_as, kw_operator),
   set_opv(u_ob_c36_unify1, function, f_u_ob_c36_unify1),
   DefMacroResult=u_ob_c36_unify1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8826 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9140)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8826 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level diffifier call", 1, 9142)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:8826 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9169)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9170 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$diff1',
			    [ob1, ob2, bindings, 'ignore-slots'],
			    
			    [ '#BQ',
			      
			      [ let,
				
				[ ['already-matched', '*already-matched*'],
				  [result, []]
				],
				[setq, '*diff?*', t],
				[setq, '*already-matched*', [cons, t, []]],
				
				[ setq,
				  result,
				  
				  [ 'ob$unify2',
				    ['#COMMA', ob1],
				    ['#COMMA', ob2],
				    ['#COMMA', bindings],
				    ['#COMMA', 'ignore-slots']
				  ]
				],
				[setq, '*already-matched*', 'already-matched'],
				result
			      ]
			    ]
			  ]).

% annotating U::OB$DIFF1 
wl: lambda_def(defmacro,
	      u_ob_c36_diff1,
	      f_u_ob_c36_diff1,
	      [u_ob1, u_ob2, bindings, u_ignore_slots],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    
		    [ [u_already_matched, u_xx_already_matched_xx],
		      [u_result, []]
		    ],
		    [setq, u_xx_diff_c63_xx, t],
		    [setq, u_xx_already_matched_xx, [cons, t, []]],
		    
		    [ setq,
		      u_result,
		      
		      [ u_ob_c36_unify2,
			['#COMMA', u_ob1],
			['#COMMA', u_ob2],
			['#COMMA', bindings],
			['#COMMA', u_ignore_slots]
		      ]
		    ],
		    [setq, u_xx_already_matched_xx, u_already_matched],
		    u_result
		  ]
		]
	      ]).


% annotating U::OB$DIFF1 
wl: arglist_info(u_ob_c36_diff1,
		[u_ob1, u_ob2, bindings, u_ignore_slots],
		[Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$DIFF1 
wl: init_args(exact_only, u_ob_c36_diff1).


% annotating U::OB$DIFF1 
f_u_ob_c36_diff1(Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param, FnResult) :-
	[let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_diff_c63_xx, t], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_unify2, Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_diff1, classof, claz_macro),
   set_opv(u_ob_c36_diff1, compile_as, kw_operator),
   set_opv(u_ob_c36_diff1, function, f_u_ob_c36_diff1),
   DefMacroResult=u_ob_c36_diff1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9170 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9481)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9170 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level unifier call", 1, 9483)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9170 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9508)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9509 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$unify',
			    [ob1, ob2, bindings],
			    
			    [ '#BQ',
			      
			      [ 'ob$unify1',
				['#COMMA', ob1],
				['#COMMA', ob2],
				['#COMMA', bindings],
				[]
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY 
wl: lambda_def(defmacro,
	      u_ob_c36_unify,
	      f_u_ob_c36_unify,
	      [u_ob1, u_ob2, bindings],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_ob_c36_unify1,
		    ['#COMMA', u_ob1],
		    ['#COMMA', u_ob2],
		    ['#COMMA', bindings],
		    []
		  ]
		]
	      ]).


% annotating U::OB$UNIFY 
wl: arglist_info(u_ob_c36_unify,
		[u_ob1, u_ob2, bindings],
		[Ob1_Param, Ob2_Param, Bindings_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY 
wl: init_args(exact_only, u_ob_c36_unify).


% annotating U::OB$UNIFY 
f_u_ob_c36_unify(Ob1_Param, Ob2_Param, Bindings_Param, FnResult) :-
	[u_ob_c36_unify1, Ob1_Param, Ob2_Param, Bindings_Param, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify, classof, claz_macro),
   set_opv(u_ob_c36_unify, compile_as, kw_operator),
   set_opv(u_ob_c36_unify, function, f_u_ob_c36_unify),
   DefMacroResult=u_ob_c36_unify.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9509 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9589)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9509 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level diffifier call", 1, 9591)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9509 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9618)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$diff',
			    [ob1, ob2, bindings],
			    
			    [ '#BQ',
			      
			      [ 'ob$diff1',
				['#COMMA', ob1],
				['#COMMA', ob2],
				['#COMMA', bindings],
				[]
			      ]
			    ]
			  ]).

% annotating U::OB$DIFF 
wl: lambda_def(defmacro,
	      u_ob_c36_diff,
	      f_u_ob_c36_diff,
	      [u_ob1, u_ob2, bindings],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_ob_c36_diff1,
		    ['#COMMA', u_ob1],
		    ['#COMMA', u_ob2],
		    ['#COMMA', bindings],
		    []
		  ]
		]
	      ]).


% annotating U::OB$DIFF 
wl: arglist_info(u_ob_c36_diff,
		[u_ob1, u_ob2, bindings],
		[Ob1_Param, Ob2_Param, Bindings_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$DIFF 
wl: init_args(exact_only, u_ob_c36_diff).


% annotating U::OB$DIFF 
f_u_ob_c36_diff(Ob1_Param, Ob2_Param, Bindings_Param, FnResult) :-
	[u_ob_c36_diff1, Ob1_Param, Ob2_Param, Bindings_Param, []]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_diff, classof, claz_macro),
   set_opv(u_ob_c36_diff, compile_as, kw_operator),
   set_opv(u_ob_c36_diff, function, f_u_ob_c36_diff),
   DefMacroResult=u_ob_c36_diff.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9697)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level unifier calls (with context)",
				     1,
				     9699)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9740)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9741 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$unify-cx',
			    [ob1, ob2, bindings, context],
			    
			    [ '#BQ',
			      
			      [ progn,
				[setq, '*unify-context*', ['#COMMA', context]],
				
				[ 'ob$unify1',
				  ['#COMMA', ob1],
				  ['#COMMA', ob2],
				  ['#COMMA', bindings],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY-CX 
wl: lambda_def(defmacro,
	      u_ob_c36_unify_cx,
	      f_u_ob_c36_unify_cx,
	      [u_ob1, u_ob2, bindings, u_context],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ progn,
		    [setq, u_xx_unify_context_xx, ['#COMMA', u_context]],
		    
		    [ u_ob_c36_unify1,
		      ['#COMMA', u_ob1],
		      ['#COMMA', u_ob2],
		      ['#COMMA', bindings],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::OB$UNIFY-CX 
wl: arglist_info(u_ob_c36_unify_cx,
		[u_ob1, u_ob2, bindings, u_context],
		[Ob1_Param, Ob2_Param, Bindings_Param, Context_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings, u_context],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY-CX 
wl: init_args(exact_only, u_ob_c36_unify_cx).


% annotating U::OB$UNIFY-CX 
f_u_ob_c36_unify_cx(Ob1_Param, Ob2_Param, Bindings_Param, Context_Param, FnResult) :-
	[progn, [setq, u_xx_unify_context_xx, Context_Param], [u_ob_c36_unify1, Ob1_Param, Ob2_Param, Bindings_Param, []]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify_cx, classof, claz_macro),
   set_opv(u_ob_c36_unify_cx, compile_as, kw_operator),
   set_opv(u_ob_c36_unify_cx, function, f_u_ob_c36_unify_cx),
   DefMacroResult=u_ob_c36_unify_cx.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9879 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$unify-cx1',
			    [ob1, ob2, bindings, 'ignore-slots', context],
			    
			    [ '#BQ',
			      
			      [ progn,
				[setq, '*unify-context*', ['#COMMA', context]],
				
				[ 'ob$unify1',
				  ['#COMMA', ob1],
				  ['#COMMA', ob2],
				  ['#COMMA', bindings],
				  ['#COMMA', 'ignore-slots']
				]
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY-CX1 
wl: lambda_def(defmacro,
	      u_ob_c36_unify_cx1,
	      f_u_ob_c36_unify_cx1,
	      [u_ob1, u_ob2, bindings, u_ignore_slots, u_context],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ progn,
		    [setq, u_xx_unify_context_xx, ['#COMMA', u_context]],
		    
		    [ u_ob_c36_unify1,
		      ['#COMMA', u_ob1],
		      ['#COMMA', u_ob2],
		      ['#COMMA', bindings],
		      ['#COMMA', u_ignore_slots]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$UNIFY-CX1 
wl: arglist_info(u_ob_c36_unify_cx1,
		[u_ob1, u_ob2, bindings, u_ignore_slots, u_context],
		
		[ Ob1_Param,
		  Ob2_Param,
		  Bindings_Param,
		  Ignore_slots_Param,
		  Context_Param
		],
		arginfo{ all:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings, u_ignore_slots, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY-CX1 
wl: init_args(exact_only, u_ob_c36_unify_cx1).


% annotating U::OB$UNIFY-CX1 
f_u_ob_c36_unify_cx1(Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param, Context_Param, FnResult) :-
	[progn, [setq, u_xx_unify_context_xx, Context_Param], [u_ob_c36_unify1, Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_unify_cx1, classof, claz_macro),
   set_opv(u_ob_c36_unify_cx1, compile_as, kw_operator),
   set_opv(u_ob_c36_unify_cx1, function, f_u_ob_c36_unify_cx1),
   DefMacroResult=u_ob_c36_unify_cx1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9879 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10042)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9879 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ob$compare: Compare two obs and produce a substitution",
				     1,
				     10044)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9879 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" binding list containing differences.",
				     1,
				     10101)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:9879 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10140)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:10142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ob$compare',
			    [source, target, 'ignore-slots'],
			    
			    [ '#BQ',
			      
			      [ let,
				
				[ ['already-matched', '*already-matched*'],
				  [result, []]
				],
				[setq, '*already-matched*', [cons, t, []]],
				
				[ setq,
				  result,
				  
				  [ 'ob$compare1',
				    ['#COMMA', source],
				    ['#COMMA', target],
				    '*empty-bd*',
				    ['#COMMA', 'ignore-slots'],
				    
				    [ lambda,
				      [source, target],
				      
				      [ cond,
					
					[ [and, ['ty?', source], ['ty?', target]],
					  
					  [ 'ty$least-common-supertype',
					    source,
					    target
					  ]
					],
					
					[ 
					  [ and,
					    ['ob$ty', source],
					    ['ob$ty', target]
					  ],
					  
					  [ 'ty$least-common-supertype',
					    ['ob$ty', source],
					    ['ob$ty', target]
					  ]
					],
					[else, []]
				      ]
				    ]
				  ]
				],
				[setq, '*already-matched*', 'already-matched'],
				result
			      ]
			    ]
			  ]).

% annotating U::OB$COMPARE 
wl: lambda_def(defmacro,
	      u_ob_c36_compare,
	      f_u_ob_c36_compare,
	      [ext_source, u_target, u_ignore_slots],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    
		    [ [u_already_matched, u_xx_already_matched_xx],
		      [u_result, []]
		    ],
		    [setq, u_xx_already_matched_xx, [cons, t, []]],
		    
		    [ setq,
		      u_result,
		      
		      [ u_ob_c36_compare1,
			['#COMMA', ext_source],
			['#COMMA', u_target],
			u_xx_empty_bd_xx,
			['#COMMA', u_ignore_slots],
			
			[ lambda,
			  [ext_source, u_target],
			  
			  [ cond,
			    
			    [ [and, [u_ty_c63, ext_source], [u_ty_c63, u_target]],
			      
			      [ u_ty_c36_least_common_supertype,
				ext_source,
				u_target
			      ]
			    ],
			    
			    [ 
			      [ and,
				[u_ob_c36_ty, ext_source],
				[u_ob_c36_ty, u_target]
			      ],
			      
			      [ u_ty_c36_least_common_supertype,
				[u_ob_c36_ty, ext_source],
				[u_ob_c36_ty, u_target]
			      ]
			    ],
			    [u_else, []]
			  ]
			]
		      ]
		    ],
		    [setq, u_xx_already_matched_xx, u_already_matched],
		    u_result
		  ]
		]
	      ]).


% annotating U::OB$COMPARE 
wl: arglist_info(u_ob_c36_compare,
		[ext_source, u_target, u_ignore_slots],
		[Ext_source_Param, Target_Param, Ignore_slots_Param],
		arginfo{ all:[ext_source, u_target, u_ignore_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[ext_source, u_target, u_ignore_slots],
			 opt:0,
			 req:[ext_source, u_target, u_ignore_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$COMPARE 
wl: init_args(exact_only, u_ob_c36_compare).


% annotating U::OB$COMPARE 
f_u_ob_c36_compare(Ext_source_Param, Target_Param, Ignore_slots_Param, FnResult) :-
	TLEnv3=[bv(ext_source, Ext_source_Param), bv(u_target, Target_Param), bv(u_ignore_slots, Ignore_slots_Param)],
	[let, [[u_already_matched, u_xx_already_matched_xx], [u_result, []]], [setq, u_xx_already_matched_xx, [cons, t, []]], [setq, u_result, [u_ob_c36_compare1, Ext_source_Param, Target_Param, u_xx_empty_bd_xx, Ignore_slots_Param, [lambda, [ext_source, u_target], [cond, [[and, [u_ty_c63, ext_source], [u_ty_c63, u_target]], [u_ty_c36_least_common_supertype, ext_source, u_target]], [[and, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]], [u_ty_c36_least_common_supertype, [u_ob_c36_ty, ext_source], [u_ob_c36_ty, u_target]]], [u_else, []]]]]], [setq, u_xx_already_matched_xx, u_already_matched], u_result]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ob_c36_compare, classof, claz_macro),
   set_opv(u_ob_c36_compare, compile_as, kw_operator),
   set_opv(u_ob_c36_compare, function, f_u_ob_c36_compare),
   DefMacroResult=u_ob_c36_compare.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_macros.cl:10142 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 11122)).
:- true.


% Total time: 5.523 seconds


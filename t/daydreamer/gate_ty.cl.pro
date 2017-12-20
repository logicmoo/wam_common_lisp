
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_ty" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:32 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This file contains the type mechanism with simple inheritance",
				     1,
				     202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:265 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 266)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:267 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 6/29/85: Original version written",
				     1,
				     268)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:303 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/24/86: Added major types", 1, 304)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:332 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/23/86: Rewrote to be flavorless",
				     1,
				     333)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:368 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 369)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:370 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     371)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:451 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['ob$decl-inverses', [quote, isa], [quote, 'isa-of']]).
:- f_u_ob_c36_decl_inverses(u_isa, u_isa_of, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:451 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(ob$decl-inverses 'type 'type-of)",
				     1,
				     485)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:520 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$instance?',
			    [self, 'type-name'],
			    
			    [ if,
			      [not, ['ob?', self]],
			      
			      [ progn,
				
				[ error,
				  '$STRING'("ty$instance?: ~A not ob"),
				  self
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'ob-warn',
				  '$STRING'("Warning: ty$instance?: ~A not ob"),
				  self
				],
				[]
			      ],
			      
			      [ and,
				['ty?', ['ob$get', self, [quote, type]]],
				
				[ or,
				  
				  [ 'eq?',
				    'type-name',
				    ['ob$get', self, [quote, type]]
				  ],
				  
				  [ 'any?',
				    
				    [ lambda,
				      [x],
				      ['memq?', 'type-name', ['ob$names', x]]
				    ],
				    
				    [ 'ty$supertypes*',
				      ['ob$get', self, [quote, type]]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::TY$INSTANCE? 
wl: lambda_def(defun,
	      u_ty_c36_instance_c63,
	      f_u_ty_c36_instance_c63,
	      [u_self, u_type_name],
	      
	      [ 
		[ if,
		  [not, [u_ob_c63, u_self]],
		  
		  [ progn,
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(t),
				 #\(y),
				 #\($),
				 #\(i),
				 #\(n),
				 #\(s),
				 #\(t),
				 #\(a),
				 #\(n),
				 #\(c),
				 #\(e),
				 #\(?),
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
		      u_self
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_ob_warn,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('W'),
				 #\(a),
				 #\(r),
				 #\(n),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\(:),
				 #\(' '),
				 #\(t),
				 #\(y),
				 #\($),
				 #\(i),
				 #\(n),
				 #\(s),
				 #\(t),
				 #\(a),
				 #\(n),
				 #\(c),
				 #\(e),
				 #\(?),
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
		      u_self
		    ],
		    []
		  ],
		  
		  [ and,
		    [u_ty_c63, [u_ob_c36_get, u_self, [quote, type]]],
		    
		    [ or,
		      
		      [ u_eq_c63,
			u_type_name,
			[u_ob_c36_get, u_self, [quote, type]]
		      ],
		      
		      [ u_any_c63,
			
			[ lambda,
			  [u_x],
			  [u_memq_c63, u_type_name, [u_ob_c36_names, u_x]]
			],
			
			[ u_ty_c36_supertypes_xx,
			  [u_ob_c36_get, u_self, [quote, type]]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::TY$INSTANCE? 
wl: arglist_info(u_ty_c36_instance_c63,
		[u_self, u_type_name],
		[Self_Param, Type_name_Param],
		arginfo{ all:[u_self, u_type_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_type_name],
			 opt:0,
			 req:[u_self, u_type_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$INSTANCE? 
wl: init_args(exact_only, u_ty_c36_instance_c63).


% annotating U::TY$INSTANCE? 
f_u_ty_c36_instance_c63(Self_Param, Type_name_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_type_name, Type_name_Param)],
	f_u_ob_c63(u_self, PredArgResult),
	(   PredArgResult==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\(t),
				  #\(y),
				  #\($),
				  #\(i),
				  #\(n),
				  #\(s),
				  #\(t),
				  #\(a),
				  #\(n),
				  #\(c),
				  #\(e),
				  #\(?),
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
		       Self_Param
		     ],
		     Error_Ret),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_ob_warn,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('W'),
					   #\(a),
					   #\(r),
					   #\(n),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(:),
					   #\(' '),
					   #\(t),
					   #\(y),
					   #\($),
					   #\(i),
					   #\(n),
					   #\(s),
					   #\(t),
					   #\(a),
					   #\(n),
					   #\(c),
					   #\(e),
					   #\(?),
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
				u_self
			      ],
			      Roman_nl_Ret),
	    FnResult=[]
	;   f_u_ty_c63([u_ob_c36_get, u_self, [quote, type]], IFTEST18),
	    (   IFTEST18\==[]
	    ->  (   f_u_eq_c63(u_type_name,
			       [u_ob_c36_get, u_self, [quote, type]],
			       FORM1_Res),
		    FORM1_Res\==[],
		    FnResult=FORM1_Res
		->  true
		;   f_u_any_c63(
				[ lambda,
				  [u_x],
				  
				  [ u_memq_c63,
				    u_type_name,
				    [u_ob_c36_names, u_x]
				  ]
				],
				
				[ u_ty_c36_supertypes_xx,
				  [u_ob_c36_get, u_self, [quote, type]]
				],
				Any_c63_Ret),
		    FnResult=Any_c63_Ret
		)
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_ty_c36_instance_c63, classof, claz_function),
   set_opv(u_ty_c36_instance_c63, compile_as, kw_function),
   set_opv(u_ty_c36_instance_c63, function, f_u_ty_c36_instance_c63),
   DefunResult=u_ty_c36_instance_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$instance-of?',
			    [self, type],
			    
			    [ and,
			      ['ty?', ['ob$get', self, [quote, type]]],
			      
			      [ 'memq?',
				type,
				
				[ 'ty$supertypes*',
				  ['ob$get', self, [quote, type]]
				]
			      ]
			    ]
			  ]).

% annotating U::TY$INSTANCE-OF? 
wl: lambda_def(defun,
	      u_ty_c36_instance_of_c63,
	      f_u_ty_c36_instance_of_c63,
	      [u_self, type],
	      
	      [ 
		[ and,
		  [u_ty_c63, [u_ob_c36_get, u_self, [quote, type]]],
		  
		  [ u_memq_c63,
		    type,
		    
		    [ u_ty_c36_supertypes_xx,
		      [u_ob_c36_get, u_self, [quote, type]]
		    ]
		  ]
		]
	      ]).


% annotating U::TY$INSTANCE-OF? 
wl: arglist_info(u_ty_c36_instance_of_c63,
		[u_self, type],
		[Self_Param, Type_Param],
		arginfo{ all:[u_self, type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, type],
			 opt:0,
			 req:[u_self, type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$INSTANCE-OF? 
wl: init_args(exact_only, u_ty_c36_instance_of_c63).


% annotating U::TY$INSTANCE-OF? 
f_u_ty_c36_instance_of_c63(Self_Param, Type_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(type, Type_Param)],
	f_u_ty_c63([u_ob_c36_get, u_self, [quote, type]], IFTEST),
	(   IFTEST\==[]
	->  f_u_memq_c63(type,
			 
			 [ u_ty_c36_supertypes_xx,
			   [u_ob_c36_get, u_self, [quote, type]]
			 ],
			 TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_ty_c36_instance_of_c63, classof, claz_function),
   set_opv(u_ty_c36_instance_of_c63, compile_as, kw_function),
   set_opv(u_ty_c36_instance_of_c63, function, f_u_ty_c36_instance_of_c63),
   DefunResult=u_ty_c36_instance_of_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:983 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1112)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ppformat = (<prop | nil> <slotnames> <optional-slotnames>)",
				     1,
				     1114)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:983 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1175)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1177 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$create',
			    [name, 'parent-names', ppformat],
			    
			    [ 'let*',
			      
			      [ [temp, []],
				
				[ parents,
				  
				  [ map,
				    [quote, list],
				    
				    [ lambda,
				      [x],
				      [setq, temp, ['ob$name->ob', x]],
				      
				      [ if,
					['null?', temp],
					
					[ error,
					  '$STRING'("ty$create ~A: ~A not defined yet.~%"),
					  name,
					  x
					],
					temp
				      ]
				    ],
				    'parent-names'
				  ]
				],
				[type, ['ty$new', name, parents]]
			      ],
			      
			      [ cond,
				
				[ ppformat,
				  ['ob$set', type, [quote, ppformat], ppformat]
				],
				
				[ parents,
				  
				  [ 'ob$set',
				    type,
				    [quote, ppformat],
				    ['ob$get', [car, parents], [quote, ppformat]]
				  ]
				],
				
				[ t,
				  
				  [ 'ob$set',
				    type,
				    [quote, ppformat],
				    
				    [ quote,
				      
				      [ prop,
					[actor, from, to, obj],
					[actor, from, to, obj]
				      ]
				    ]
				  ]
				]
			      ],
			      type
			    ]
			  ]).

% annotating U::TY$CREATE 
wl: lambda_def(defun,
	      u_ty_c36_create,
	      f_u_ty_c36_create,
	      [sys_name, u_parent_names, u_ppformat],
	      
	      [ 
		[ let_xx,
		  
		  [ [u_temp, []],
		    
		    [ u_parents,
		      
		      [ map,
			[quote, list],
			
			[ lambda,
			  [u_x],
			  [setq, u_temp, [u_ob_c36_name_c62_ob, u_x]],
			  
			  [ if,
			    [u_null_c63, u_temp],
			    
			    [ error,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(t),
					 #\(y),
					 #\($),
					 #\(c),
					 #\(r),
					 #\(e),
					 #\(a),
					 #\(t),
					 #\(e),
					 #\(' '),
					 #\(~),
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
					 #\(d),
					 #\(e),
					 #\(f),
					 #\(i),
					 #\(n),
					 #\(e),
					 #\(d),
					 #\(' '),
					 #\(y),
					 #\(e),
					 #\(t),
					 #\('.'),
					 #\(~),
					 #\('%')
				       ]),
			      sys_name,
			      u_x
			    ],
			    u_temp
			  ]
			],
			u_parent_names
		      ]
		    ],
		    [type, [u_ty_c36_new, sys_name, u_parents]]
		  ],
		  
		  [ cond,
		    
		    [ u_ppformat,
		      [u_ob_c36_set, type, [quote, u_ppformat], u_ppformat]
		    ],
		    
		    [ u_parents,
		      
		      [ u_ob_c36_set,
			type,
			[quote, u_ppformat],
			[u_ob_c36_get, [car, u_parents], [quote, u_ppformat]]
		      ]
		    ],
		    
		    [ t,
		      
		      [ u_ob_c36_set,
			type,
			[quote, u_ppformat],
			
			[ quote,
			  
			  [ u_prop,
			    [u_actor, u_from, u_to, u_obj],
			    [u_actor, u_from, u_to, u_obj]
			  ]
			]
		      ]
		    ]
		  ],
		  type
		]
	      ]).


% annotating U::TY$CREATE 
wl: arglist_info(u_ty_c36_create,
		[sys_name, u_parent_names, u_ppformat],
		[Name_Param, Parent_names_Param, Ppformat_Param],
		arginfo{ all:[sys_name, u_parent_names, u_ppformat],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_name, u_parent_names, u_ppformat],
			 opt:0,
			 req:[sys_name, u_parent_names, u_ppformat],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$CREATE 
wl: init_args(exact_only, u_ty_c36_create).


% annotating U::TY$CREATE 
f_u_ty_c36_create(Name_Param, Parent_names_Param, Ppformat_Param, FnResult) :-
	Env=[bv(sys_name, Name_Param), bv(u_parent_names, Parent_names_Param), bv(u_ppformat, Ppformat_Param)],
	LEnv=[[bv(u_temp, [])]|Env],
	Lambda=closure([Env|LEnv], LResult, [u_x],  (get_var(Env, u_x, X_Get), f_u_ob_c36_name_c62_ob(X_Get, Temp), set_var(Env, u_temp, Temp), f_u_null_c63(u_temp, IFTEST), (IFTEST\==[]->get_var(Env, u_x, X_Get26), cl_error(['$ARRAY'([*], claz_base_character, [#\(t), #\(y), #\($), #\(c), #\(r), #\(e), #\(a), #\(t), #\(e), #\(' '), #\(~), #\('A'), #\(:), #\(' '), #\(~), #\('A'), #\(' '), #\(n), #\(o), #\(t), #\(' '), #\(d), #\(e), #\(f), #\(i), #\(n), #\(e), #\(d), #\(' '), #\(y), #\(e), #\(t), #\('.'), #\(~), #\('%')]), Name_Param, X_Get26], TrueResult), LResult=TrueResult;get_var(Env, u_temp, Temp_Get), LResult=Temp_Get))),
	cl_map(list, Lambda, Parent_names_Param, Parents_Init),
	Env=[[bv(u_parents, Parents_Init)]|LEnv],
	get_var(Env, u_parents, Parents_Get),
	f_u_ty_c36_new(Name_Param, Parents_Get, Type_Init),
	Env=[[bv(type, Type_Init)]|Env],
	(   Ppformat_Param\==[]
	->  get_var(Env, type, Type_Get),
	    f_u_ob_c36_set(Type_Get, u_ppformat, Ppformat_Param, TrueResult56),
	    ElseResult57=TrueResult56
	;   get_var(Env, u_parents, IFTEST48),
	    (   IFTEST48\==[]
	    ->  get_var(Env, type, Type_Get51),
		get_var(Env, u_parents, Parents_Get52),
		cl_car(Parents_Get52, C36_get_Param),
		f_u_ob_c36_get(C36_get_Param, u_ppformat, Ppformat),
		f_u_ob_c36_set(Type_Get51, u_ppformat, Ppformat, TrueResult54),
		ElseResult57=TrueResult54
	    ;   get_var(Env, type, Type_Get53),
		f_u_ob_c36_set(Type_Get53,
			       u_ppformat,
			       
			       [ u_prop,
				 [u_actor, u_from, u_to, u_obj],
				 [u_actor, u_from, u_to, u_obj]
			       ],
			       ElseResult55),
		ElseResult57=ElseResult55
	    )
	),
	get_var(Env, type, Type_Get58),
	LetResult=Type_Get58,
	LetResult=FnResult.
:- set_opv(f_u_ty_c36_create, classof, claz_function),
   set_opv(u_ty_c36_create, compile_as, kw_function),
   set_opv(u_ty_c36_create, function, f_u_ty_c36_create),
   DefunResult=u_ty_c36_create.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1842 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$fcreate',
			    [name, 'parent-names', slots],
			    
			    [ 'ty$create',
			      name,
			      'parent-names',
			      [list, [], slots, []]
			    ]
			  ]).

% annotating U::TY$FCREATE 
wl: lambda_def(defun,
	      u_ty_c36_fcreate,
	      f_u_ty_c36_fcreate,
	      [sys_name, u_parent_names, sys_slots],
	      
	      [ 
		[ u_ty_c36_create,
		  sys_name,
		  u_parent_names,
		  [list, [], sys_slots, []]
		]
	      ]).


% annotating U::TY$FCREATE 
wl: arglist_info(u_ty_c36_fcreate,
		[sys_name, u_parent_names, sys_slots],
		[Name_Param, Parent_names_Param, Slots_Param],
		arginfo{ all:[sys_name, u_parent_names, sys_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_name, u_parent_names, sys_slots],
			 opt:0,
			 req:[sys_name, u_parent_names, sys_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$FCREATE 
wl: init_args(exact_only, u_ty_c36_fcreate).


% annotating U::TY$FCREATE 
f_u_ty_c36_fcreate(Name_Param, Parent_names_Param, Slots_Param, FnResult) :-
	_134044=[[], Slots_Param, []],
	f_u_ty_c36_create(Name_Param,
			  Parent_names_Param,
			  _134044,
			  C36_create_Ret),
	C36_create_Ret=FnResult.
:- set_opv(f_u_ty_c36_fcreate, classof, claz_function),
   set_opv(u_ty_c36_fcreate, compile_as, kw_function),
   set_opv(u_ty_c36_fcreate, function, f_u_ty_c36_fcreate),
   DefunResult=u_ty_c36_fcreate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1941 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$new',
			    [name, supertypes],
			    
			    [ let,
			      
			      [ [type, ['ob$create-named-empty', name]],
				[temp, []]
			      ],
			      ['ob$set', type, [quote, type], '*ty-ob*'],
			      [setq, '*types*', [cons, type, '*types*']],
			      
			      [ yloop,
				[yfor, supertype, in, supertypes],
				[ydo, ['ob$add', type, [quote, isa], supertype]]
			      ],
			      [setq, temp, ['ob$create-empty']],
			      ['ob$add', temp, [quote, type], type],
			      ['ob$set-literal', type, t],
			      ['ob$add', type, [quote, exemplar], temp],
			      type
			    ]
			  ]).

% annotating U::TY$NEW 
wl: lambda_def(defun,
	      u_ty_c36_new,
	      f_u_ty_c36_new,
	      [sys_name, u_supertypes],
	      
	      [ 
		[ let,
		  [[type, [u_ob_c36_create_named_empty, sys_name]], [u_temp, []]],
		  [u_ob_c36_set, type, [quote, type], u_xx_ty_ob_xx],
		  [setq, u_xx_types_xx, [cons, type, u_xx_types_xx]],
		  
		  [ u_yloop,
		    [u_yfor, u_supertype, u_in, u_supertypes],
		    [u_ydo, [u_ob_c36_add, type, [quote, u_isa], u_supertype]]
		  ],
		  [setq, u_temp, [u_ob_c36_create_empty]],
		  [u_ob_c36_add, u_temp, [quote, type], type],
		  [u_ob_c36_set_literal, type, t],
		  [u_ob_c36_add, type, [quote, u_exemplar], u_temp],
		  type
		]
	      ]).


% annotating U::TY$NEW 
wl: arglist_info(u_ty_c36_new,
		[sys_name, u_supertypes],
		[Name_Param, Supertypes_Param],
		arginfo{ all:[sys_name, u_supertypes],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_name, u_supertypes],
			 opt:0,
			 req:[sys_name, u_supertypes],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$NEW 
wl: init_args(exact_only, u_ty_c36_new).


% annotating U::TY$NEW 
f_u_ty_c36_new(Name_Param, Supertypes_Param, FnResult) :-
	Env=[bv(sys_name, Name_Param), bv(u_supertypes, Supertypes_Param)],
	f_u_ob_c36_create_named_empty(Name_Param, Type_Init),
	LEnv=[[bv(type, Type_Init), bv(u_temp, [])]|Env],
	get_var(LEnv, type, Type_Get),
	get_var(LEnv, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
	f_u_ob_c36_set(Type_Get, type, Xx_ty_ob_xx_Get, C36_set_Ret),
	get_var(LEnv, type, Type_Get21),
	get_var(LEnv, u_xx_types_xx, Xx_types_xx_Get),
	Xx_types_xx=[Type_Get21|Xx_types_xx_Get],
	set_var(LEnv, u_xx_types_xx, Xx_types_xx),
	f_u_yloop(
		  [ [u_yfor, u_supertype, u_in, u_supertypes],
		    [u_ydo, [u_ob_c36_add, type, [quote, u_isa], u_supertype]]
		  ],
		  Yloop_Ret),
	f_u_ob_c36_create_empty(Temp),
	set_var(LEnv, u_temp, Temp),
	get_var(LEnv, type, Type_Get24),
	get_var(LEnv, u_temp, Temp_Get27),
	f_u_ob_c36_add(Temp_Get27, type, Type_Get24, C36_add_Ret),
	get_var(LEnv, type, Type_Get25),
	f_u_ob_c36_set_literal(Type_Get25, t, T),
	get_var(LEnv, type, Type_Get26),
	f_u_ob_c36_add(Type_Get26, u_exemplar, Temp_Get27, C36_add_Ret37),
	get_var(LEnv, type, Type_Get28),
	LetResult=Type_Get28,
	LetResult=FnResult.
:- set_opv(f_u_ty_c36_new, classof, claz_function),
   set_opv(u_ty_c36_new, compile_as, kw_function),
   set_opv(u_ty_c36_new, function, f_u_ty_c36_new),
   DefunResult=u_ty_c36_new.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1941 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Exemplars are used by the DAYDREAMER generator.",
				     8,
				     2219)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1941 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Return new type", 8, 2412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:1941 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This is way recursive!", 1, 2445)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*ty-ob*',
			    ['ob$create-named-empty', [quote, ty]]
			  ]).
:- f_u_ob_c36_create_named_empty(u_ty, _Ignored),
   set_var(TLEnv3, u_xx_ty_ob_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2512 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ob$set',
			    '*ty-ob*',
			    [quote, ppformat],
			    [quote, [[], [exemplar]]]
			  ]).
:- get_var(TLEnv3, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
   f_u_ob_c36_set(Xx_ty_ob_xx_Get, u_ppformat, [[], [u_exemplar]], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2557 **********************/
:- lisp_compile_to_prolog(pkg_user, ['ob$set-literal', '*ty-ob*', t]).
:- get_var(TLEnv3, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
   f_u_ob_c36_set_literal(Xx_ty_ob_xx_Get, t, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2585 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*types*', [list, '*ty-ob*']]).
:- get_var(TLEnv3, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get),
   _Ignored=[Xx_ty_ob_xx_Get],
   set_var(TLEnv3, u_xx_types_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2615 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  ['ob$set', '*ty-ob*', [quote, type], '*ty-ob*']).
:- get_var(TLEnv3, u_xx_ty_ob_xx, Xx_ty_ob_xx_Get6),
   f_u_ob_c36_set(Xx_ty_ob_xx_Get6, type, Xx_ty_ob_xx_Get6, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2647 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$major-type',
			    ['type-name'],
			    
			    [ 'ob$set',
			      ['ob$name->ob', 'type-name'],
			      [quote, 'major-type?'],
			      t
			    ]
			  ]).

% annotating U::TY$MAJOR-TYPE 
wl: lambda_def(defun,
	      u_ty_c36_major_type,
	      f_u_ty_c36_major_type,
	      [u_type_name],
	      
	      [ 
		[ u_ob_c36_set,
		  [u_ob_c36_name_c62_ob, u_type_name],
		  [quote, u_major_type_c63],
		  t
		]
	      ]).


% annotating U::TY$MAJOR-TYPE 
wl: arglist_info(u_ty_c36_major_type,
		[u_type_name],
		[Type_name_Param],
		arginfo{ all:[u_type_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_type_name],
			 opt:0,
			 req:[u_type_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$MAJOR-TYPE 
wl: init_args(exact_only, u_ty_c36_major_type).


% annotating U::TY$MAJOR-TYPE 
f_u_ty_c36_major_type(Type_name_Param, FnResult) :-
	f_u_ob_c36_name_c62_ob(Type_name_Param, C36_set_Param),
	f_u_ob_c36_set(C36_set_Param, u_major_type_c63, t, T),
	T=FnResult.
:- set_opv(f_u_ty_c36_major_type, classof, claz_function),
   set_opv(u_ty_c36_major_type, compile_as, kw_function),
   set_opv(u_ty_c36_major_type, function, f_u_ty_c36_major_type),
   DefunResult=u_ty_c36_major_type.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2732 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$display',
			    [],
			    
			    [ yloop,
			      [yfor, type, in, '*types*'],
			      [ydo, ['ob$unhide', type]]
			    ]
			  ]).

% annotating U::TY$DISPLAY 
wl: lambda_def(defun,
	      u_ty_c36_display,
	      f_u_ty_c36_display,
	      [],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, type, u_in, u_xx_types_xx],
		  [u_ydo, [u_ob_c36_unhide, type]]
		]
	      ]).


% annotating U::TY$DISPLAY 
wl: arglist_info(u_ty_c36_display,
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

% annotating U::TY$DISPLAY 
wl: init_args(exact_only, u_ty_c36_display).


% annotating U::TY$DISPLAY 
f_u_ty_c36_display(FnResult) :-
	Env=[],
	f_u_yloop(
		  [ [u_yfor, type, u_in, u_xx_types_xx],
		    [u_ydo, [u_ob_c36_unhide, type]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_display, classof, claz_function),
   set_opv(u_ty_c36_display, compile_as, kw_function),
   set_opv(u_ty_c36_display, function, f_u_ty_c36_display),
   DefunResult=u_ty_c36_display.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2820 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$supertypes',
			    [self],
			    ['ob$gets', self, [quote, isa]]
			  ]).

% annotating U::TY$SUPERTYPES 
wl: lambda_def(defun,
	      u_ty_c36_supertypes,
	      f_u_ty_c36_supertypes,
	      [u_self],
	      [[u_ob_c36_gets, u_self, [quote, u_isa]]]).


% annotating U::TY$SUPERTYPES 
wl: arglist_info(u_ty_c36_supertypes,
		[u_self],
		[Self_Param],
		arginfo{ all:[u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self],
			 opt:0,
			 req:[u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$SUPERTYPES 
wl: init_args(exact_only, u_ty_c36_supertypes).


% annotating U::TY$SUPERTYPES 
f_u_ty_c36_supertypes(Self_Param, FnResult) :-
	f_u_ob_c36_gets(Self_Param, u_isa, Isa),
	Isa=FnResult.
:- set_opv(f_u_ty_c36_supertypes, classof, claz_function),
   set_opv(u_ty_c36_supertypes, compile_as, kw_function),
   set_opv(u_ty_c36_supertypes, function, f_u_ty_c36_supertypes),
   DefunResult=u_ty_c36_supertypes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2872 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$subtypes',
			    [self],
			    ['ob$gets', self, [quote, 'isa-of']]
			  ]).

% annotating U::TY$SUBTYPES 
wl: lambda_def(defun,
	      u_ty_c36_subtypes,
	      f_u_ty_c36_subtypes,
	      [u_self],
	      [[u_ob_c36_gets, u_self, [quote, u_isa_of]]]).


% annotating U::TY$SUBTYPES 
wl: arglist_info(u_ty_c36_subtypes,
		[u_self],
		[Self_Param],
		arginfo{ all:[u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self],
			 opt:0,
			 req:[u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$SUBTYPES 
wl: init_args(exact_only, u_ty_c36_subtypes).


% annotating U::TY$SUBTYPES 
f_u_ty_c36_subtypes(Self_Param, FnResult) :-
	f_u_ob_c36_gets(Self_Param, u_isa_of, Isa_of),
	Isa_of=FnResult.
:- set_opv(f_u_ty_c36_subtypes, classof, claz_function),
   set_opv(u_ty_c36_subtypes, compile_as, kw_function),
   set_opv(u_ty_c36_subtypes, function, f_u_ty_c36_subtypes),
   DefunResult=u_ty_c36_subtypes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:2925 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$supertypes*',
			    [self],
			    
			    [ yloop,
			      [initial, [result, []], [x, []]],
			      [yfor, type, in, ['ob$gets', self, [quote, isa]]],
			      
			      [ ydo,
				[setq, x, ['ty$supertypes*', type]],
				
				[ if,
				  [not, ['null?', x]],
				  [setq, result, [union, result, x]]
				]
			      ],
			      [yresult, [cons, self, result]]
			    ]
			  ]).

% annotating U::TY$SUPERTYPES* 
wl: lambda_def(defun,
	      u_ty_c36_supertypes_xx,
	      f_u_ty_c36_supertypes_xx,
	      [u_self],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_x, []]],
		  [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa]]],
		  
		  [ u_ydo,
		    [setq, u_x, [u_ty_c36_supertypes_xx, type]],
		    
		    [ if,
		      [not, [u_null_c63, u_x]],
		      [setq, u_result, [union, u_result, u_x]]
		    ]
		  ],
		  [u_yresult, [cons, u_self, u_result]]
		]
	      ]).


% annotating U::TY$SUPERTYPES* 
wl: arglist_info(u_ty_c36_supertypes_xx,
		[u_self],
		[Self_Param],
		arginfo{ all:[u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self],
			 opt:0,
			 req:[u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$SUPERTYPES* 
wl: init_args(exact_only, u_ty_c36_supertypes_xx).


% annotating U::TY$SUPERTYPES* 
f_u_ty_c36_supertypes_xx(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_x, []]],
		    [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa]]],
		    
		    [ u_ydo,
		      [setq, u_x, [u_ty_c36_supertypes_xx, type]],
		      
		      [ if,
			[not, [u_null_c63, u_x]],
			[setq, u_result, [union, u_result, u_x]]
		      ]
		    ],
		    [u_yresult, [cons, u_self, u_result]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_supertypes_xx, classof, claz_function),
   set_opv(u_ty_c36_supertypes_xx, compile_as, kw_function),
   set_opv(u_ty_c36_supertypes_xx, function, f_u_ty_c36_supertypes_xx),
   DefunResult=u_ty_c36_supertypes_xx.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3222 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$supertype-of?',
			    [self, type],
			    ['memq?', type, ['ty$supertypes*', self]]
			  ]).

% annotating U::TY$SUPERTYPE-OF? 
wl: lambda_def(defun,
	      u_ty_c36_supertype_of_c63,
	      f_u_ty_c36_supertype_of_c63,
	      [u_self, type],
	      [[u_memq_c63, type, [u_ty_c36_supertypes_xx, u_self]]]).


% annotating U::TY$SUPERTYPE-OF? 
wl: arglist_info(u_ty_c36_supertype_of_c63,
		[u_self, type],
		[Self_Param, Type_Param],
		arginfo{ all:[u_self, type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, type],
			 opt:0,
			 req:[u_self, type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$SUPERTYPE-OF? 
wl: init_args(exact_only, u_ty_c36_supertype_of_c63).


% annotating U::TY$SUPERTYPE-OF? 
f_u_ty_c36_supertype_of_c63(Self_Param, Type_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(type, Type_Param)],
	f_u_memq_c63(type, [u_ty_c36_supertypes_xx, u_self], Memq_c63_Ret),
	Memq_c63_Ret=FnResult.
:- set_opv(f_u_ty_c36_supertype_of_c63, classof, claz_function),
   set_opv(u_ty_c36_supertype_of_c63, compile_as, kw_function),
   set_opv(u_ty_c36_supertype_of_c63, function, f_u_ty_c36_supertype_of_c63),
   DefunResult=u_ty_c36_supertype_of_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3297 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$subtypes*',
			    [self],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, type, in, ['ob$gets', self, [quote, 'isa-of']]],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  [append, result, ['ty$subtypes*', type]]
				]
			      ],
			      [yresult, [cons, self, result]]
			    ]
			  ]).

% annotating U::TY$SUBTYPES* 
wl: lambda_def(defun,
	      u_ty_c36_subtypes_xx,
	      f_u_ty_c36_subtypes_xx,
	      [u_self],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa_of]]],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      [append, u_result, [u_ty_c36_subtypes_xx, type]]
		    ]
		  ],
		  [u_yresult, [cons, u_self, u_result]]
		]
	      ]).


% annotating U::TY$SUBTYPES* 
wl: arglist_info(u_ty_c36_subtypes_xx,
		[u_self],
		[Self_Param],
		arginfo{ all:[u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self],
			 opt:0,
			 req:[u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$SUBTYPES* 
wl: init_args(exact_only, u_ty_c36_subtypes_xx).


% annotating U::TY$SUBTYPES* 
f_u_ty_c36_subtypes_xx(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, type, u_in, [u_ob_c36_gets, u_self, [quote, u_isa_of]]],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			[append, u_result, [u_ty_c36_subtypes_xx, type]]
		      ]
		    ],
		    [u_yresult, [cons, u_self, u_result]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_subtypes_xx, classof, claz_function),
   set_opv(u_ty_c36_subtypes_xx, compile_as, kw_function),
   set_opv(u_ty_c36_subtypes_xx, function, f_u_ty_c36_subtypes_xx),
   DefunResult=u_ty_c36_subtypes_xx.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3506 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$subtype-of?',
			    [self, type],
			    ['memq?', type, ['ty$subtypes*', self]]
			  ]).

% annotating U::TY$SUBTYPE-OF? 
wl: lambda_def(defun,
	      u_ty_c36_subtype_of_c63,
	      f_u_ty_c36_subtype_of_c63,
	      [u_self, type],
	      [[u_memq_c63, type, [u_ty_c36_subtypes_xx, u_self]]]).


% annotating U::TY$SUBTYPE-OF? 
wl: arglist_info(u_ty_c36_subtype_of_c63,
		[u_self, type],
		[Self_Param, Type_Param],
		arginfo{ all:[u_self, type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, type],
			 opt:0,
			 req:[u_self, type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$SUBTYPE-OF? 
wl: init_args(exact_only, u_ty_c36_subtype_of_c63).


% annotating U::TY$SUBTYPE-OF? 
f_u_ty_c36_subtype_of_c63(Self_Param, Type_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(type, Type_Param)],
	f_u_memq_c63(type, [u_ty_c36_subtypes_xx, u_self], Memq_c63_Ret),
	Memq_c63_Ret=FnResult.
:- set_opv(f_u_ty_c36_subtype_of_c63, classof, claz_function),
   set_opv(u_ty_c36_subtype_of_c63, compile_as, kw_function),
   set_opv(u_ty_c36_subtype_of_c63, function, f_u_ty_c36_subtype_of_c63),
   DefunResult=u_ty_c36_subtype_of_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3577 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$least-common-supertype',
			    [type1, type2],
			    
			    [ yloop,
			      
			      [ initial,
				['supertypes*2', ['ty$supertypes*', type2]],
				[result, []]
			      ],
			      [yfor, supertype1, in, ['ty$supertypes*', type1]],
			      [yuntil, result],
			      
			      [ ydo,
				
				[ if,
				  ['memq?', supertype1, 'supertypes*2'],
				  [setq, result, supertype1]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::TY$LEAST-COMMON-SUPERTYPE 
wl: lambda_def(defun,
	      u_ty_c36_least_common_supertype,
	      f_u_ty_c36_least_common_supertype,
	      [u_type1, u_type2],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_supertypes_xx_2, [u_ty_c36_supertypes_xx, u_type2]],
		    [u_result, []]
		  ],
		  [u_yfor, u_supertype1, u_in, [u_ty_c36_supertypes_xx, u_type1]],
		  [u_yuntil, u_result],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_memq_c63, u_supertype1, u_supertypes_xx_2],
		      [setq, u_result, u_supertype1]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::TY$LEAST-COMMON-SUPERTYPE 
wl: arglist_info(u_ty_c36_least_common_supertype,
		[u_type1, u_type2],
		[Type1_Param, Type2_Param],
		arginfo{ all:[u_type1, u_type2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_type1, u_type2],
			 opt:0,
			 req:[u_type1, u_type2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$LEAST-COMMON-SUPERTYPE 
wl: init_args(exact_only, u_ty_c36_least_common_supertype).


% annotating U::TY$LEAST-COMMON-SUPERTYPE 
f_u_ty_c36_least_common_supertype(Type1_Param, Type2_Param, FnResult) :-
	Env=[bv(u_type1, Type1_Param), bv(u_type2, Type2_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_supertypes_xx_2, [u_ty_c36_supertypes_xx, u_type2]],
		      [u_result, []]
		    ],
		    
		    [ u_yfor,
		      u_supertype1,
		      u_in,
		      [u_ty_c36_supertypes_xx, u_type1]
		    ],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_memq_c63, u_supertype1, u_supertypes_xx_2],
			[setq, u_result, u_supertype1]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_least_common_supertype, classof, claz_function),
   set_opv(u_ty_c36_least_common_supertype, compile_as, kw_function),
   set_opv(u_ty_c36_least_common_supertype,
	   function,
	   f_u_ty_c36_least_common_supertype),
   DefunResult=u_ty_c36_least_common_supertype.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:3907 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$basic-type-distance',
			    [ancestor, type],
			    
			    [ let,
			      
			      [ 
				[ position,
				  [position, ancestor, ['ty$supertypes*', type]]
				]
			      ],
			      [if, position, [+, 1, position], '*max-fixnum*']
			    ]
			  ]).

% annotating U::TY$BASIC-TYPE-DISTANCE 
wl: lambda_def(defun,
	      u_ty_c36_basic_type_distance,
	      f_u_ty_c36_basic_type_distance,
	      [u_ancestor, type],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ position,
		      [position, u_ancestor, [u_ty_c36_supertypes_xx, type]]
		    ]
		  ],
		  [if, position, [+, 1, position], u_xx_max_fixnum_xx]
		]
	      ]).


% annotating U::TY$BASIC-TYPE-DISTANCE 
wl: arglist_info(u_ty_c36_basic_type_distance,
		[u_ancestor, type],
		[Ancestor_Param, Type_Param],
		arginfo{ all:[u_ancestor, type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ancestor, type],
			 opt:0,
			 req:[u_ancestor, type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$BASIC-TYPE-DISTANCE 
wl: init_args(exact_only, u_ty_c36_basic_type_distance).


% annotating U::TY$BASIC-TYPE-DISTANCE 
f_u_ty_c36_basic_type_distance(Ancestor_Param, Type_Param, FnResult) :-
	Env=[bv(u_ancestor, Ancestor_Param), bv(type, Type_Param)],
	f_u_ty_c36_supertypes_xx(Type_Param, Supertypes_xx_Ret),
	cl_position(Ancestor_Param, Supertypes_xx_Ret, Position_Init),
	LEnv=[[bv(position, Position_Init)]|Env],
	get_var(LEnv, position, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, position, Position_Get23),
	    +(1, Position_Get23, TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_xx_max_fixnum_xx, Xx_max_fixnum_xx_Get),
	    FnResult=Xx_max_fixnum_xx_Get
	).
:- set_opv(f_u_ty_c36_basic_type_distance, classof, claz_function),
   set_opv(u_ty_c36_basic_type_distance, compile_as, kw_function),
   set_opv(u_ty_c36_basic_type_distance,
	   function,
	   f_u_ty_c36_basic_type_distance),
   DefunResult=u_ty_c36_basic_type_distance.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:4067 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$distance',
			    [type1, type2],
			    
			    [ let,
			      
			      [ 
				[ lcs,
				  ['ty$least-common-supertype', type1, type2]
				]
			      ],
			      
			      [ if,
				lcs,
				
				[ min,
				  ['ty$basic-type-distance', lcs, type1],
				  ['ty$basic-type-distance', lcs, type2]
				],
				'*max-fixnum*'
			      ]
			    ]
			  ]).

% annotating U::TY$DISTANCE 
wl: lambda_def(defun,
	      u_ty_c36_distance,
	      f_u_ty_c36_distance,
	      [u_type1, u_type2],
	      
	      [ 
		[ let,
		  [[u_lcs, [u_ty_c36_least_common_supertype, u_type1, u_type2]]],
		  
		  [ if,
		    u_lcs,
		    
		    [ min,
		      [u_ty_c36_basic_type_distance, u_lcs, u_type1],
		      [u_ty_c36_basic_type_distance, u_lcs, u_type2]
		    ],
		    u_xx_max_fixnum_xx
		  ]
		]
	      ]).


% annotating U::TY$DISTANCE 
wl: arglist_info(u_ty_c36_distance,
		[u_type1, u_type2],
		[Type1_Param, Type2_Param],
		arginfo{ all:[u_type1, u_type2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_type1, u_type2],
			 opt:0,
			 req:[u_type1, u_type2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$DISTANCE 
wl: init_args(exact_only, u_ty_c36_distance).


% annotating U::TY$DISTANCE 
f_u_ty_c36_distance(Type1_Param, Type2_Param, FnResult) :-
	Env=[bv(u_type1, Type1_Param), bv(u_type2, Type2_Param)],
	f_u_ty_c36_least_common_supertype(Type1_Param, Type2_Param, Lcs_Init),
	LEnv=[[bv(u_lcs, Lcs_Init)]|Env],
	get_var(LEnv, u_lcs, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_lcs, Lcs_Get23),
	    f_u_ty_c36_basic_type_distance(Lcs_Get23, Type1_Param, Min_Param),
	    get_var(LEnv, u_lcs, Lcs_Get25),
	    f_u_ty_c36_basic_type_distance(Lcs_Get25,
					   Type2_Param,
					   Type_distance_Ret),
	    cl_min(Min_Param, Type_distance_Ret, TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_xx_max_fixnum_xx, Xx_max_fixnum_xx_Get),
	    FnResult=Xx_max_fixnum_xx_Get
	).
:- set_opv(f_u_ty_c36_distance, classof, claz_function),
   set_opv(u_ty_c36_distance, compile_as, kw_function),
   set_opv(u_ty_c36_distance, function, f_u_ty_c36_distance),
   DefunResult=u_ty_c36_distance.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:4301 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty$get-major-type',
			    [self],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, type, in, ['ty$supertypes*', self]],
			      [ywhile, [not, result]],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  
				  [ if,
				    ['ob$get', type, [quote, 'major-type?']],
				    type,
				    []
				  ]
				]
			      ],
			      [yresult, [if, result, result, self]]
			    ]
			  ]).

% annotating U::TY$GET-MAJOR-TYPE 
wl: lambda_def(defun,
	      u_ty_c36_get_major_type,
	      f_u_ty_c36_get_major_type,
	      [u_self],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, type, u_in, [u_ty_c36_supertypes_xx, u_self]],
		  [u_ywhile, [not, u_result]],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      
		      [ if,
			[u_ob_c36_get, type, [quote, u_major_type_c63]],
			type,
			[]
		      ]
		    ]
		  ],
		  [u_yresult, [if, u_result, u_result, u_self]]
		]
	      ]).


% annotating U::TY$GET-MAJOR-TYPE 
wl: arglist_info(u_ty_c36_get_major_type,
		[u_self],
		[Self_Param],
		arginfo{ all:[u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self],
			 opt:0,
			 req:[u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TY$GET-MAJOR-TYPE 
wl: init_args(exact_only, u_ty_c36_get_major_type).


% annotating U::TY$GET-MAJOR-TYPE 
f_u_ty_c36_get_major_type(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, type, u_in, [u_ty_c36_supertypes_xx, u_self]],
		    [u_ywhile, [not, u_result]],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			
			[ if,
			  [u_ob_c36_get, type, [quote, u_major_type_c63]],
			  type,
			  []
			]
		      ]
		    ],
		    [u_yresult, [if, u_result, u_result, u_self]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ty_c36_get_major_type, classof, claz_function),
   set_opv(u_ty_c36_get_major_type, compile_as, kw_function),
   set_opv(u_ty_c36_get_major_type, function, f_u_ty_c36_get_major_type),
   DefunResult=u_ty_c36_get_major_type.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_ty.cl:4301 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 4623)).
:- true.


% Total time: 2.281 seconds


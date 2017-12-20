
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_cx" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:14:54 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" This file contains:", 1, 202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:223 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Context mechanism for obs", 1, 224)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:251 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 252)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:253 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 6/22/85: Original version written",
				     1,
				     254)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:289 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 6/30/85: Added rule comments", 1, 290)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:320 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/6/86: Removed old rules and truth maintenance",
				     1,
				     321)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/24/86: Added inheriting of mutations-tried?",
				     1,
				     372)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:419 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/25/86: Added pseudo-sprouts", 1, 420)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:451 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/27/86: Added and tested type hashing",
				     1,
				     452)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 7/19/86: Added touched-facts", 1, 493)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:523 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/24/86: Rewrote code to be flavorless",
				     1,
				     524)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:564 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 565)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:566 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: must enforce first arg being a context.",
				     1,
				     567)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:614 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 615)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     617)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setq, '*cx-ob*', ['ty$create', [quote, 'CX'], [], []]]).
:- f_u_ty_c36_create(u_cx, [], [], _Ignored),
   set_var(TLEnv3, u_xx_cx_ob_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:739 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*next-cx-number*', 1]).
:- set_var(TLEnv3, setq, u_xx_next_cx_number_xx, 1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:766 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, 'cx$create', [], ['cx$sprout', []]]).

% annotating U::CX$CREATE 
wl: lambda_def(defun,
	      u_cx_c36_create,
	      f_u_cx_c36_create,
	      [],
	      [[u_cx_c36_sprout, []]]).


% annotating U::CX$CREATE 
wl: arglist_info(u_cx_c36_create,
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

% annotating U::CX$CREATE 
wl: init_args(exact_only, u_cx_c36_create).


% annotating U::CX$CREATE 
f_u_cx_c36_create(FnResult) :-
	Env=[],
	f_u_cx_c36_sprout([], C36_sprout_Ret),
	C36_sprout_Ret=FnResult.
:- set_opv(f_u_cx_c36_create, classof, claz_function),
   set_opv(u_cx_c36_create, compile_as, kw_function),
   set_opv(u_cx_c36_create, function, f_u_cx_c36_create),
   DefunResult=u_cx_c36_create.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:806 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$sprout',
			    [parent],
			    
			    [ let,
			      [[self, ['ob$fcreate', [quote, ['CX']]]]],
			      
			      [ 'ob$add-unique-name',
				self,
				
				[ 'string->symbol',
				  
				  [ 'string-append',
				    '$STRING'("CX."),
				    
				    [ prog1,
				      ['fixnum->string', '*next-cx-number*'],
				      ['increment-me', '*next-cx-number*']
				    ]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['cx?', parent],
				  ['ob$set', self, [quote, parent], parent],
				  ['cx$add-child', parent, self],
				  
				  [ 'ob$set',
				    self,
				    [quote, 'mutations-tried?'],
				    
				    [ 'ob$get',
				      parent,
				      [quote, 'mutations-tried?']
				    ]
				  ],
				  
				  [ if,
				    ['ob$get', parent, [quote, timeout]],
				    
				    [ 'ob$set',
				      self,
				      [quote, timeout],
				      [-, ['ob$get', parent, [quote, timeout]], 1]
				    ]
				  ],
				  
				  [ 'ob$set',
				    self,
				    [quote, 'pseudo-sprout?'],
				    ['ob$get', parent, [quote, 'pseudo-sprout?']]
				  ],
				  
				  [ 'ob$set',
				    self,
				    [quote, ancestors],
				    
				    [ cons,
				      parent,
				      ['ob$get', parent, [quote, ancestors]]
				    ]
				  ],
				  
				  [ 'ob$set',
				    self,
				    [quote, 'all-obs'],
				    
				    [ 'copy-list',
				      ['ob$get', parent, [quote, 'all-obs']]
				    ]
				  ],
				  
				  [ 'ob$set',
				    self,
				    [quote, 'type-hashing'],
				    
				    [ map,
				      [quote, list],
				      [lambda, [x], ['copy-list', x]],
				      ['ob$get', parent, [quote, 'type-hashing']]
				    ]
				  ],
				  
				  [ 'ob$set',
				    self,
				    [quote, 'gen-switches'],
				    ['ob$get', parent, [quote, 'gen-switches']]
				  ],
				  
				  [ 'ob$set',
				    self,
				    [quote, 'touched-facts'],
				    ['ob$get', parent, [quote, 'touched-facts']]
				  ]
				],
				[['null?', parent]],
				
				[ else,
				  
				  [ error,
				    '$STRING'("cx$sprout: parent is not a context or NIL.")
				  ]
				]
			      ],
			      
			      [ if,
				parent,
				
				[ ndbg,
				  '*gate-dbg*',
				  context,
				  '$STRING'("~A --> ~A~%"),
				  parent,
				  self
				]
			      ],
			      self
			    ]
			  ]).

% annotating U::CX$SPROUT 
wl: lambda_def(defun,
	      u_cx_c36_sprout,
	      f_u_cx_c36_sprout,
	      [u_parent],
	      
	      [ 
		[ let,
		  [[u_self, [u_ob_c36_fcreate, [quote, [u_cx]]]]],
		  
		  [ u_ob_c36_add_unique_name,
		    u_self,
		    
		    [ u_string_c62_symbol,
		      
		      [ u_string_append,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\('C'), #\('X'), #\('.')]),
			
			[ prog1,
			  [u_fixnum_c62_string, u_xx_next_cx_number_xx],
			  [u_increment_me, u_xx_next_cx_number_xx]
			]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_cx_c63, u_parent],
		      [u_ob_c36_set, u_self, [quote, u_parent], u_parent],
		      [u_cx_c36_add_child, u_parent, u_self],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_mutations_tried_c63],
			[u_ob_c36_get, u_parent, [quote, u_mutations_tried_c63]]
		      ],
		      
		      [ if,
			[u_ob_c36_get, u_parent, [quote, ext_timeout]],
			
			[ u_ob_c36_set,
			  u_self,
			  [quote, ext_timeout],
			  [-, [u_ob_c36_get, u_parent, [quote, ext_timeout]], 1]
			]
		      ],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_pseudo_sprout_c63],
			[u_ob_c36_get, u_parent, [quote, u_pseudo_sprout_c63]]
		      ],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_ancestors],
			
			[ cons,
			  u_parent,
			  [u_ob_c36_get, u_parent, [quote, u_ancestors]]
			]
		      ],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_all_obs],
			[copy_list, [u_ob_c36_get, u_parent, [quote, u_all_obs]]]
		      ],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_type_hashing],
			
			[ map,
			  [quote, list],
			  [lambda, [u_x], [copy_list, u_x]],
			  [u_ob_c36_get, u_parent, [quote, u_type_hashing]]
			]
		      ],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_gen_switches],
			[u_ob_c36_get, u_parent, [quote, u_gen_switches]]
		      ],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_touched_facts],
			[u_ob_c36_get, u_parent, [quote, u_touched_facts]]
		      ]
		    ],
		    [[u_null_c63, u_parent]],
		    
		    [ u_else,
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(c),
				   #\(x),
				   #\($),
				   #\(s),
				   #\(p),
				   #\(r),
				   #\(o),
				   #\(u),
				   #\(t),
				   #\(:),
				   #\(' '),
				   #\(p),
				   #\(a),
				   #\(r),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(' '),
				   #\(i),
				   #\(s),
				   #\(' '),
				   #\(n),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(c),
				   #\(o),
				   #\(n),
				   #\(t),
				   #\(e),
				   #\(x),
				   #\(t),
				   #\(' '),
				   #\(o),
				   #\(r),
				   #\(' '),
				   #\('N'),
				   #\('I'),
				   #\('L'),
				   #\('.')
				 ])
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_parent,
		    
		    [ u_ndbg,
		      u_xx_gate_dbg_xx,
		      u_context,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('A'),
				 #\(' '),
				 #\(-),
				 #\(-),
				 #\(>),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(~),
				 #\('%')
			       ]),
		      u_parent,
		      u_self
		    ]
		  ],
		  u_self
		]
	      ]).


% annotating U::CX$SPROUT 
wl: arglist_info(u_cx_c36_sprout,
		[u_parent],
		[Parent_Param],
		arginfo{ all:[u_parent],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_parent],
			 opt:0,
			 req:[u_parent],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$SPROUT 
wl: init_args(exact_only, u_cx_c36_sprout).


% annotating U::CX$SPROUT 
f_u_cx_c36_sprout(Parent_Param, FnResult) :-
	Env=[bv(u_parent, Parent_Param)],
	f_u_ob_c36_fcreate([quote, [u_cx]], Self_Init),
	LEnv=[[bv(u_self, Self_Init)]|Env],
	get_var(LEnv, u_self, Self_Get),
	f_u_string_c62_symbol(
			      [ u_string_append,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\('C'), #\('X'), #\('.')]),
				
				[ prog1,
				  [u_fixnum_c62_string, u_xx_next_cx_number_xx],
				  [u_increment_me, u_xx_next_cx_number_xx]
				]
			      ],
			      C62_symbol_Ret),
	f_u_ob_c36_add_unique_name(Self_Get, C62_symbol_Ret, Unique_name_Ret),
	f_u_cx_c63(u_parent, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_self, Self_Get19),
	    f_u_ob_c36_set(Self_Get19, u_parent, Parent_Param, C36_set_Ret),
	    get_var(LEnv, u_self, Self_Get22),
	    f_u_cx_c36_add_child(Parent_Param, Self_Get22, Add_child_Ret),
	    get_var(LEnv, u_self, Self_Get23),
	    f_u_ob_c36_get(Parent_Param,
			   u_mutations_tried_c63,
			   Mutations_tried_c63),
	    f_u_ob_c36_set(Self_Get23,
			   u_mutations_tried_c63,
			   Mutations_tried_c63,
			   C36_set_Ret82),
	    f_u_ob_c36_get(Parent_Param, ext_timeout, IFTEST25),
	    (   IFTEST25\==[]
	    ->  get_var(LEnv, u_self, Self_Get28),
		f_u_ob_c36_get(Parent_Param, ext_timeout, Ext_timeout),
		-(Ext_timeout, 1, Ext_timeout68),
		f_u_ob_c36_set(Self_Get28,
			       ext_timeout,
			       Ext_timeout68,
			       TrueResult),
		_122428=TrueResult
	    ;   _122428=[]
	    ),
	    get_var(LEnv, u_self, Self_Get31),
	    f_u_ob_c36_get(Parent_Param, u_pseudo_sprout_c63, Pseudo_sprout_c63),
	    f_u_ob_c36_set(Self_Get31,
			   u_pseudo_sprout_c63,
			   Pseudo_sprout_c63,
			   C36_set_Ret83),
	    get_var(LEnv, u_self, Self_Get33),
	    f_u_ob_c36_get(Parent_Param, u_ancestors, Ancestors),
	    Ancestors71=[Parent_Param|Ancestors],
	    f_u_ob_c36_set(Self_Get33, u_ancestors, Ancestors71, C36_set_Ret84),
	    get_var(LEnv, u_self, Self_Get36),
	    f_u_ob_c36_get(Parent_Param, u_all_obs, All_obs),
	    cl_copy_list(All_obs, All_obs73),
	    f_u_ob_c36_set(Self_Get36, u_all_obs, All_obs73, C36_set_Ret85),
	    get_var(LEnv, u_self, Self_Get38),
	    Lambda=closure([Env40|LEnv], LResult, [u_x],  (get_var(Env40, u_x, X_Get), cl_copy_list(X_Get, LResult))),
	    f_u_ob_c36_get(Parent_Param, u_type_hashing, Type_hashing),
	    cl_map(list, Lambda, Type_hashing, Type_hashing75),
	    f_u_ob_c36_set(Self_Get38,
			   u_type_hashing,
			   Type_hashing75,
			   C36_set_Ret86),
	    get_var(LEnv, u_self, Self_Get45),
	    f_u_ob_c36_get(Parent_Param, u_gen_switches, Gen_switches),
	    f_u_ob_c36_set(Self_Get45,
			   u_gen_switches,
			   Gen_switches,
			   C36_set_Ret87),
	    get_var(LEnv, u_self, Self_Get47),
	    f_u_ob_c36_get(Parent_Param, u_touched_facts, Touched_facts),
	    f_u_ob_c36_set(Self_Get47,
			   u_touched_facts,
			   Touched_facts,
			   TrueResult57),
	    ElseResult56=TrueResult57
	;   f_u_null_c63(u_parent, IFTEST49),
	    (   IFTEST49\==[]
	    ->  ElseResult56=[]
	    ;   get_var(LEnv, u_else, IFTEST51),
		(   IFTEST51\==[]
		->  cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					
					[ #\(c),
					  #\(x),
					  #\($),
					  #\(s),
					  #\(p),
					  #\(r),
					  #\(o),
					  #\(u),
					  #\(t),
					  #\(:),
					  #\(' '),
					  #\(p),
					  #\(a),
					  #\(r),
					  #\(e),
					  #\(n),
					  #\(t),
					  #\(' '),
					  #\(i),
					  #\(s),
					  #\(' '),
					  #\(n),
					  #\(o),
					  #\(t),
					  #\(' '),
					  #\(a),
					  #\(' '),
					  #\(c),
					  #\(o),
					  #\(n),
					  #\(t),
					  #\(e),
					  #\(x),
					  #\(t),
					  #\(' '),
					  #\(o),
					  #\(r),
					  #\(' '),
					  #\('N'),
					  #\('I'),
					  #\('L'),
					  #\('.')
					])
			     ],
			     TrueResult54),
		    ElseResult56=TrueResult54
		;   ElseResult56=[]
		)
	    )
	),
	(   Parent_Param\==[]
	->  f_u_ndbg(u_xx_gate_dbg_xx,
		     u_context,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\(~),
				  #\('A'),
				  #\(' '),
				  #\(-),
				  #\(-),
				  #\(>),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(~),
				  #\('%')
				]),
		       u_parent,
		       u_self
		     ],
		     TrueResult62),
	    _123450=TrueResult62
	;   _123450=[]
	),
	get_var(LEnv, u_self, Self_Get63),
	LetResult=Self_Get63,
	LetResult=FnResult.
:- set_opv(f_u_cx_c36_sprout, classof, claz_function),
   set_opv(u_cx_c36_sprout, compile_as, kw_function),
   set_opv(u_cx_c36_sprout, function, f_u_cx_c36_sprout),
   DefunResult=u_cx_c36_sprout.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:806 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2032)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:806 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Getters", 1, 2034)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:806 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2044)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$parent',
			    [self],
			    ['ob$get', self, [quote, parent]]
			  ]).

% annotating U::CX$PARENT 
wl: lambda_def(defun,
	      u_cx_c36_parent,
	      f_u_cx_c36_parent,
	      [u_self],
	      [[u_ob_c36_get, u_self, [quote, u_parent]]]).


% annotating U::CX$PARENT 
wl: arglist_info(u_cx_c36_parent,
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

% annotating U::CX$PARENT 
wl: init_args(exact_only, u_cx_c36_parent).


% annotating U::CX$PARENT 
f_u_cx_c36_parent(Self_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_parent, Parent),
	Parent=FnResult.
:- set_opv(f_u_cx_c36_parent, classof, claz_function),
   set_opv(u_cx_c36_parent, compile_as, kw_function),
   set_opv(u_cx_c36_parent, function, f_u_cx_c36_parent),
   DefunResult=u_cx_c36_parent.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2096 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$ancestors',
			    [self],
			    ['ob$get', self, [quote, ancestors]]
			  ]).

% annotating U::CX$ANCESTORS 
wl: lambda_def(defun,
	      u_cx_c36_ancestors,
	      f_u_cx_c36_ancestors,
	      [u_self],
	      [[u_ob_c36_get, u_self, [quote, u_ancestors]]]).


% annotating U::CX$ANCESTORS 
wl: arglist_info(u_cx_c36_ancestors,
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

% annotating U::CX$ANCESTORS 
wl: init_args(exact_only, u_cx_c36_ancestors).


% annotating U::CX$ANCESTORS 
f_u_cx_c36_ancestors(Self_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_ancestors, Ancestors),
	Ancestors=FnResult.
:- set_opv(f_u_cx_c36_ancestors, classof, claz_function),
   set_opv(u_cx_c36_ancestors, compile_as, kw_function),
   set_opv(u_cx_c36_ancestors, function, f_u_cx_c36_ancestors),
   DefunResult=u_cx_c36_ancestors.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2152 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$children',
			    [self],
			    ['ob$get', self, [quote, children]]
			  ]).

% annotating U::CX$CHILDREN 
wl: lambda_def(defun,
	      u_cx_c36_children,
	      f_u_cx_c36_children,
	      [u_self],
	      [[u_ob_c36_get, u_self, [quote, u_children]]]).


% annotating U::CX$CHILDREN 
wl: arglist_info(u_cx_c36_children,
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

% annotating U::CX$CHILDREN 
wl: init_args(exact_only, u_cx_c36_children).


% annotating U::CX$CHILDREN 
f_u_cx_c36_children(Self_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_children, Children),
	Children=FnResult.
:- set_opv(f_u_cx_c36_children, classof, claz_function),
   set_opv(u_cx_c36_children, compile_as, kw_function),
   set_opv(u_cx_c36_children, function, f_u_cx_c36_children),
   DefunResult=u_cx_c36_children.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2206 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$set-last-sprout-con',
			    [self, val],
			    
			    [ ndbg,
			      '*gate-dbg*',
			      rule,
			      '$STRING'("setting last sprout concept = ~A in ~A~%"),
			      val,
			      self
			    ],
			    
			    [ if,
			      val,
			      
			      [ let,
				[[parent, ['ob$get', self, [quote, parent]]]],
				['ob$set', self, [quote, 'last-sprout-con'], val],
				
				[ if,
				  
				  [ and,
				    parent,
				    ['null?', ['cx$last-sprout-con', parent]]
				  ],
				  ['cx$set-last-sprout-con', parent, val]
				]
			      ]
			    ]
			  ]).

% annotating U::CX$SET-LAST-SPROUT-CON 
wl: lambda_def(defun,
	      u_cx_c36_set_last_sprout_con,
	      f_u_cx_c36_set_last_sprout_con,
	      [u_self, u_val],
	      
	      [ 
		[ u_ndbg,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(s),
			     #\(e),
			     #\(t),
			     #\(t),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(l),
			     #\(a),
			     #\(s),
			     #\(t),
			     #\(' '),
			     #\(s),
			     #\(p),
			     #\(r),
			     #\(o),
			     #\(u),
			     #\(t),
			     #\(' '),
			     #\(c),
			     #\(o),
			     #\(n),
			     #\(c),
			     #\(e),
			     #\(p),
			     #\(t),
			     #\(' '),
			     #\(=),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(~),
			     #\('%')
			   ]),
		  u_val,
		  u_self
		],
		
		[ if,
		  u_val,
		  
		  [ let,
		    [[u_parent, [u_ob_c36_get, u_self, [quote, u_parent]]]],
		    [u_ob_c36_set, u_self, [quote, u_last_sprout_con], u_val],
		    
		    [ if,
		      
		      [ and,
			u_parent,
			[u_null_c63, [u_cx_c36_last_sprout_con, u_parent]]
		      ],
		      [u_cx_c36_set_last_sprout_con, u_parent, u_val]
		    ]
		  ]
		]
	      ]).


% annotating U::CX$SET-LAST-SPROUT-CON 
wl: arglist_info(u_cx_c36_set_last_sprout_con,
		[u_self, u_val],
		[Self_Param, Val_Param],
		arginfo{ all:[u_self, u_val],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_val],
			 opt:0,
			 req:[u_self, u_val],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$SET-LAST-SPROUT-CON 
wl: init_args(exact_only, u_cx_c36_set_last_sprout_con).


% annotating U::CX$SET-LAST-SPROUT-CON 
f_u_cx_c36_set_last_sprout_con(Self_Param, Val_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_val, Val_Param)],
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_rule,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    
			    [ #\(s),
			      #\(e),
			      #\(t),
			      #\(t),
			      #\(i),
			      #\(n),
			      #\(g),
			      #\(' '),
			      #\(l),
			      #\(a),
			      #\(s),
			      #\(t),
			      #\(' '),
			      #\(s),
			      #\(p),
			      #\(r),
			      #\(o),
			      #\(u),
			      #\(t),
			      #\(' '),
			      #\(c),
			      #\(o),
			      #\(n),
			      #\(c),
			      #\(e),
			      #\(p),
			      #\(t),
			      #\(' '),
			      #\(=),
			      #\(' '),
			      #\(~),
			      #\('A'),
			      #\(' '),
			      #\(i),
			      #\(n),
			      #\(' '),
			      #\(~),
			      #\('A'),
			      #\(~),
			      #\('%')
			    ]),
		   u_val,
		   u_self
		 ],
		 Ndbg_Ret),
	(   Val_Param\==[]
	->  f_u_ob_c36_get(Self_Param, u_parent, Parent_Init),
	    LEnv=[[bv(u_parent, Parent_Init)]|Env],
	    f_u_ob_c36_set(Self_Param, u_last_sprout_con, Val_Param, C36_set_Ret),
	    get_var(LEnv, u_parent, IFTEST26),
	    (   IFTEST26\==[]
	    ->  f_u_null_c63([u_cx_c36_last_sprout_con, u_parent], TrueResult),
		IFTEST24=TrueResult
	    ;   IFTEST24=[]
	    ),
	    (   IFTEST24\==[]
	    ->  get_var(LEnv, u_parent, Parent_Get30),
		f_u_cx_c36_set_last_sprout_con(Parent_Get30,
					       Val_Param,
					       TrueResult32),
		FnResult=TrueResult32
	    ;   FnResult=[]
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_set_last_sprout_con, classof, claz_function),
   set_opv(u_cx_c36_set_last_sprout_con, compile_as, kw_function),
   set_opv(u_cx_c36_set_last_sprout_con,
	   function,
	   f_u_cx_c36_set_last_sprout_con),
   DefunResult=u_cx_c36_set_last_sprout_con.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$last-sprout-con',
			    [self],
			    ['ob$get', self, [quote, 'last-sprout-con']]
			  ]).

% annotating U::CX$LAST-SPROUT-CON 
wl: lambda_def(defun,
	      u_cx_c36_last_sprout_con,
	      f_u_cx_c36_last_sprout_con,
	      [u_self],
	      [[u_ob_c36_get, u_self, [quote, u_last_sprout_con]]]).


% annotating U::CX$LAST-SPROUT-CON 
wl: arglist_info(u_cx_c36_last_sprout_con,
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

% annotating U::CX$LAST-SPROUT-CON 
wl: init_args(exact_only, u_cx_c36_last_sprout_con).


% annotating U::CX$LAST-SPROUT-CON 
f_u_cx_c36_last_sprout_con(Self_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_last_sprout_con, Last_sprout_con),
	Last_sprout_con=FnResult.
:- set_opv(f_u_cx_c36_last_sprout_con, classof, claz_function),
   set_opv(u_cx_c36_last_sprout_con, compile_as, kw_function),
   set_opv(u_cx_c36_last_sprout_con, function, f_u_cx_c36_last_sprout_con),
   DefunResult=u_cx_c36_last_sprout_con.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Pseudo sprouts don't inherit info from their parent (or ancestors).",
				     1,
				     2617)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" In particular, the top-context of an ob (an optimization) is no",
				     1,
				     2687)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" longer guaranteed to be an ancestor of a context in which that",
				     1,
				     2753)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ob is asserted. In accordance, the top-context is never used for",
				     1,
				     2818)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" a pseudo-sprout context. Pseudo-sprouts are effectively root contexts",
				     1,
				     2885)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" but think they are sprouts of the specified context as far as &parent",
				     1,
				     2957)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" and &ancestor related calls go. By the way, all descendents of a",
				     1,
				     3029)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:2548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" pseudo sprout are pseudo sprouts.",
				     1,
				     3096)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:3131 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$pseudo-sprout-of',
			    [self, context],
			    
			    [ if,
			      ['ob$get', self, [quote, parent]],
			      
			      [ error,
				'$STRING'("Cannot make a pseudo sprout out of a context with a parent")
			      ],
			      
			      [ progn,
				['ob$set', self, [quote, parent], context],
				
				[ 'ob$set',
				  self,
				  [quote, ancestors],
				  
				  [ cons,
				    context,
				    ['ob$get', context, [quote, ancestors]]
				  ]
				],
				['cx$add-child', context, self],
				['ob$set', self, [quote, 'pseudo-sprout?'], t]
			      ]
			    ]
			  ]).

% annotating U::CX$PSEUDO-SPROUT-OF 
wl: lambda_def(defun,
	      u_cx_c36_pseudo_sprout_of,
	      f_u_cx_c36_pseudo_sprout_of,
	      [u_self, u_context],
	      
	      [ 
		[ if,
		  [u_ob_c36_get, u_self, [quote, u_parent]],
		  
		  [ error,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('C'),
			       #\(a),
			       #\(n),
			       #\(n),
			       #\(o),
			       #\(t),
			       #\(' '),
			       #\(m),
			       #\(a),
			       #\(k),
			       #\(e),
			       #\(' '),
			       #\(a),
			       #\(' '),
			       #\(p),
			       #\(s),
			       #\(e),
			       #\(u),
			       #\(d),
			       #\(o),
			       #\(' '),
			       #\(s),
			       #\(p),
			       #\(r),
			       #\(o),
			       #\(u),
			       #\(t),
			       #\(' '),
			       #\(o),
			       #\(u),
			       #\(t),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(a),
			       #\(' '),
			       #\(c),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(x),
			       #\(t),
			       #\(' '),
			       #\(w),
			       #\(i),
			       #\(t),
			       #\(h),
			       #\(' '),
			       #\(a),
			       #\(' '),
			       #\(p),
			       #\(a),
			       #\(r),
			       #\(e),
			       #\(n),
			       #\(t)
			     ])
		  ],
		  
		  [ progn,
		    [u_ob_c36_set, u_self, [quote, u_parent], u_context],
		    
		    [ u_ob_c36_set,
		      u_self,
		      [quote, u_ancestors],
		      
		      [ cons,
			u_context,
			[u_ob_c36_get, u_context, [quote, u_ancestors]]
		      ]
		    ],
		    [u_cx_c36_add_child, u_context, u_self],
		    [u_ob_c36_set, u_self, [quote, u_pseudo_sprout_c63], t]
		  ]
		]
	      ]).


% annotating U::CX$PSEUDO-SPROUT-OF 
wl: arglist_info(u_cx_c36_pseudo_sprout_of,
		[u_self, u_context],
		[Self_Param, Context_Param],
		arginfo{ all:[u_self, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_context],
			 opt:0,
			 req:[u_self, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$PSEUDO-SPROUT-OF 
wl: init_args(exact_only, u_cx_c36_pseudo_sprout_of).


% annotating U::CX$PSEUDO-SPROUT-OF 
f_u_cx_c36_pseudo_sprout_of(Self_Param, Context_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_parent, IFTEST),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('C'),
				  #\(a),
				  #\(n),
				  #\(n),
				  #\(o),
				  #\(t),
				  #\(' '),
				  #\(m),
				  #\(a),
				  #\(k),
				  #\(e),
				  #\(' '),
				  #\(a),
				  #\(' '),
				  #\(p),
				  #\(s),
				  #\(e),
				  #\(u),
				  #\(d),
				  #\(o),
				  #\(' '),
				  #\(s),
				  #\(p),
				  #\(r),
				  #\(o),
				  #\(u),
				  #\(t),
				  #\(' '),
				  #\(o),
				  #\(u),
				  #\(t),
				  #\(' '),
				  #\(o),
				  #\(f),
				  #\(' '),
				  #\(a),
				  #\(' '),
				  #\(c),
				  #\(o),
				  #\(n),
				  #\(t),
				  #\(e),
				  #\(x),
				  #\(t),
				  #\(' '),
				  #\(w),
				  #\(i),
				  #\(t),
				  #\(h),
				  #\(' '),
				  #\(a),
				  #\(' '),
				  #\(p),
				  #\(a),
				  #\(r),
				  #\(e),
				  #\(n),
				  #\(t)
				])
		     ],
		     TrueResult),
	    FnResult=TrueResult
	;   f_u_ob_c36_set(Self_Param, u_parent, Context_Param, C36_set_Ret),
	    f_u_ob_c36_get(Context_Param, u_ancestors, Ancestors),
	    Ancestors30=[Context_Param|Ancestors],
	    f_u_ob_c36_set(Self_Param, u_ancestors, Ancestors30, C36_set_Ret32),
	    f_u_cx_c36_add_child(Context_Param, Self_Param, Add_child_Ret),
	    f_u_ob_c36_set(Self_Param, u_pseudo_sprout_c63, t, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_pseudo_sprout_of, classof, claz_function),
   set_opv(u_cx_c36_pseudo_sprout_of, compile_as, kw_function),
   set_opv(u_cx_c36_pseudo_sprout_of, function, f_u_cx_c36_pseudo_sprout_of),
   DefunResult=u_cx_c36_pseudo_sprout_of.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:3479 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$touch-fact',
			    [self, fact],
			    
			    [ if,
			      
			      [ and,
				['touchable-fact?', fact],
				
				[ not,
				  
				  [ 'memq?',
				    fact,
				    ['ob$get', self, [quote, 'touched-facts']]
				  ]
				]
			      ],
			      
			      [ 'ob$set',
				self,
				[quote, 'touched-facts'],
				
				[ cons,
				  fact,
				  ['ob$get', self, [quote, 'touched-facts']]
				]
			      ]
			    ]
			  ]).

% annotating U::CX$TOUCH-FACT 
wl: lambda_def(defun,
	      u_cx_c36_touch_fact,
	      f_u_cx_c36_touch_fact,
	      [u_self, u_fact],
	      
	      [ 
		[ if,
		  
		  [ and,
		    [u_touchable_fact_c63, u_fact],
		    
		    [ not,
		      
		      [ u_memq_c63,
			u_fact,
			[u_ob_c36_get, u_self, [quote, u_touched_facts]]
		      ]
		    ]
		  ],
		  
		  [ u_ob_c36_set,
		    u_self,
		    [quote, u_touched_facts],
		    
		    [ cons,
		      u_fact,
		      [u_ob_c36_get, u_self, [quote, u_touched_facts]]
		    ]
		  ]
		]
	      ]).


% annotating U::CX$TOUCH-FACT 
wl: arglist_info(u_cx_c36_touch_fact,
		[u_self, u_fact],
		[Self_Param, Fact_Param],
		arginfo{ all:[u_self, u_fact],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_fact],
			 opt:0,
			 req:[u_self, u_fact],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$TOUCH-FACT 
wl: init_args(exact_only, u_cx_c36_touch_fact).


% annotating U::CX$TOUCH-FACT 
f_u_cx_c36_touch_fact(Self_Param, Fact_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_fact, Fact_Param)],
	f_u_touchable_fact_c63(u_fact, IFTEST16),
	(   IFTEST16\==[]
	->  f_u_memq_c63(u_fact,
			 [u_ob_c36_get, u_self, [quote, u_touched_facts]],
			 Not_Param),
	    cl_not(Not_Param, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Self_Param, u_touched_facts, Touched_facts),
	    Touched_facts26=[Fact_Param|Touched_facts],
	    f_u_ob_c36_set(Self_Param,
			   u_touched_facts,
			   Touched_facts26,
			   TrueResult22),
	    FnResult=TrueResult22
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_touch_fact, classof, claz_function),
   set_opv(u_cx_c36_touch_fact, compile_as, kw_function),
   set_opv(u_cx_c36_touch_fact, function, f_u_cx_c36_touch_fact),
   DefunResult=u_cx_c36_touch_fact.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:3685 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$sorted-all-obs',
			    [self],
			    
			    [ if,
			      ['ob$get', self, [quote, 'pseudo-sprout?']],
			      
			      [ progn,
				
				[ format,
				  '*gate-output*',
				  '$STRING'("GATE bug warning: Can't sort a pseudo-sprout~%")
				],
				['ob$get', self, [quote, 'all-obs']]
			      ],
			      
			      [ yloop,
				
				[ initial,
				  [result, []],
				  [rest, ['ob$get', self, [quote, 'all-obs']]]
				],
				
				[ yfor,
				  context,
				  in,
				  
				  [ cons,
				    self,
				    ['ob$get', self, [quote, ancestors]]
				  ]
				],
				
				[ ydo,
				  
				  [ yloop,
				    [yfor, ob, in, rest],
				    
				    [ ydo,
				      
				      [ if,
					
					[ 'memq?',
					  context,
					  ['ob$gets', ob, [quote, 'top-context']]
					],
					
					[ progn,
					  [setq, result, [cons, ob, result]],
					  [setq, rest, ['delq!', ob, rest]]
					]
				      ]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::CX$SORTED-ALL-OBS 
wl: lambda_def(defun,
	      u_cx_c36_sorted_all_obs,
	      f_u_cx_c36_sorted_all_obs,
	      [u_self],
	      
	      [ 
		[ if,
		  [u_ob_c36_get, u_self, [quote, u_pseudo_sprout_c63]],
		  
		  [ progn,
		    
		    [ format,
		      u_xx_gate_output_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('G'),
				 #\('A'),
				 #\('T'),
				 #\('E'),
				 #\(' '),
				 #\(b),
				 #\(u),
				 #\(g),
				 #\(' '),
				 #\(w),
				 #\(a),
				 #\(r),
				 #\(n),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\(:),
				 #\(' '),
				 #\('C'),
				 #\(a),
				 #\(n),
				 #\('\''),
				 #\(t),
				 #\(' '),
				 #\(s),
				 #\(o),
				 #\(r),
				 #\(t),
				 #\(' '),
				 #\(a),
				 #\(' '),
				 #\(p),
				 #\(s),
				 #\(e),
				 #\(u),
				 #\(d),
				 #\(o),
				 #\(-),
				 #\(s),
				 #\(p),
				 #\(r),
				 #\(o),
				 #\(u),
				 #\(t),
				 #\(~),
				 #\('%')
			       ])
		    ],
		    [u_ob_c36_get, u_self, [quote, u_all_obs]]
		  ],
		  
		  [ u_yloop,
		    
		    [ u_initial,
		      [u_result, []],
		      [rest, [u_ob_c36_get, u_self, [quote, u_all_obs]]]
		    ],
		    
		    [ u_yfor,
		      u_context,
		      u_in,
		      [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]
		    ],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_yfor, u_ob, u_in, rest],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_memq_c63,
			      u_context,
			      [u_ob_c36_gets, u_ob, [quote, u_top_context]]
			    ],
			    
			    [ progn,
			      [setq, u_result, [cons, u_ob, u_result]],
			      [setq, rest, [u_delq_c33, u_ob, rest]]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::CX$SORTED-ALL-OBS 
wl: arglist_info(u_cx_c36_sorted_all_obs,
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

% annotating U::CX$SORTED-ALL-OBS 
wl: init_args(exact_only, u_cx_c36_sorted_all_obs).


% annotating U::CX$SORTED-ALL-OBS 
f_u_cx_c36_sorted_all_obs(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_ob_c36_get(Self_Param, u_pseudo_sprout_c63, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	    cl_format(
		      [ Xx_gate_output_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('G'),
				   #\('A'),
				   #\('T'),
				   #\('E'),
				   #\(' '),
				   #\(b),
				   #\(u),
				   #\(g),
				   #\(' '),
				   #\(w),
				   #\(a),
				   #\(r),
				   #\(n),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(:),
				   #\(' '),
				   #\('C'),
				   #\(a),
				   #\(n),
				   #\('\''),
				   #\(t),
				   #\(' '),
				   #\(s),
				   #\(o),
				   #\(r),
				   #\(t),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(p),
				   #\(s),
				   #\(e),
				   #\(u),
				   #\(d),
				   #\(o),
				   #\(-),
				   #\(s),
				   #\(p),
				   #\(r),
				   #\(o),
				   #\(u),
				   #\(t),
				   #\(~),
				   #\('%')
				 ])
		      ],
		      Format_Ret),
	    f_u_ob_c36_get(Self_Param, u_all_obs, TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ 
			[ u_initial,
			  [u_result, []],
			  [rest, [u_ob_c36_get, u_self, [quote, u_all_obs]]]
			],
			
			[ u_yfor,
			  u_context,
			  u_in,
			  
			  [ cons,
			    u_self,
			    [u_ob_c36_get, u_self, [quote, u_ancestors]]
			  ]
			],
			
			[ u_ydo,
			  
			  [ u_yloop,
			    [u_yfor, u_ob, u_in, rest],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ u_memq_c63,
				  u_context,
				  [u_ob_c36_gets, u_ob, [quote, u_top_context]]
				],
				
				[ progn,
				  [setq, u_result, [cons, u_ob, u_result]],
				  [setq, rest, [u_delq_c33, u_ob, rest]]
				]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_sorted_all_obs, classof, claz_function),
   set_opv(u_cx_c36_sorted_all_obs, compile_as, kw_function),
   set_opv(u_cx_c36_sorted_all_obs, function, f_u_cx_c36_sorted_all_obs),
   DefunResult=u_cx_c36_sorted_all_obs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4360 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$add-child',
			    [self, child],
			    
			    [ 'ob$set',
			      self,
			      [quote, children],
			      [cons, child, ['ob$get', self, [quote, children]]]
			    ]
			  ]).

% annotating U::CX$ADD-CHILD 
wl: lambda_def(defun,
	      u_cx_c36_add_child,
	      f_u_cx_c36_add_child,
	      [u_self, u_child],
	      
	      [ 
		[ u_ob_c36_set,
		  u_self,
		  [quote, u_children],
		  [cons, u_child, [u_ob_c36_get, u_self, [quote, u_children]]]
		]
	      ]).


% annotating U::CX$ADD-CHILD 
wl: arglist_info(u_cx_c36_add_child,
		[u_self, u_child],
		[Self_Param, Child_Param],
		arginfo{ all:[u_self, u_child],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_child],
			 opt:0,
			 req:[u_self, u_child],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$ADD-CHILD 
wl: init_args(exact_only, u_cx_c36_add_child).


% annotating U::CX$ADD-CHILD 
f_u_cx_c36_add_child(Self_Param, Child_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_children, Children),
	Children20=[Child_Param|Children],
	f_u_ob_c36_set(Self_Param, u_children, Children20, C36_set_Ret),
	C36_set_Ret=FnResult.
:- set_opv(f_u_cx_c36_add_child, classof, claz_function),
   set_opv(u_cx_c36_add_child, compile_as, kw_function),
   set_opv(u_cx_c36_add_child, function, f_u_cx_c36_add_child),
   DefunResult=u_cx_c36_add_child.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4458 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$most-recent-child',
			    [self],
			    
			    [ if,
			      ['ob$get', self, [quote, children]],
			      [car, ['ob$get', self, [quote, children]]],
			      []
			    ]
			  ]).

% annotating U::CX$MOST-RECENT-CHILD 
wl: lambda_def(defun,
	      u_cx_c36_most_recent_child,
	      f_u_cx_c36_most_recent_child,
	      [u_self],
	      
	      [ 
		[ if,
		  [u_ob_c36_get, u_self, [quote, u_children]],
		  [car, [u_ob_c36_get, u_self, [quote, u_children]]],
		  []
		]
	      ]).


% annotating U::CX$MOST-RECENT-CHILD 
wl: arglist_info(u_cx_c36_most_recent_child,
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

% annotating U::CX$MOST-RECENT-CHILD 
wl: init_args(exact_only, u_cx_c36_most_recent_child).


% annotating U::CX$MOST-RECENT-CHILD 
f_u_cx_c36_most_recent_child(Self_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_children, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Self_Param, u_children, Children),
	    cl_car(Children, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_most_recent_child, classof, claz_function),
   set_opv(u_cx_c36_most_recent_child, compile_as, kw_function),
   set_opv(u_cx_c36_most_recent_child, function, f_u_cx_c36_most_recent_child),
   DefunResult=u_cx_c36_most_recent_child.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4572 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$leaf-descendants',
			    [self],
			    
			    [ if,
			      ['null?', ['ob$get', self, [quote, children]]],
			      [list, self],
			      
			      [ yloop,
				[initial, [result, []]],
				
				[ yfor,
				  child,
				  in,
				  ['ob$get', self, [quote, children]]
				],
				
				[ ydo,
				  
				  [ setq,
				    result,
				    
				    [ append,
				      result,
				      ['cx$leaf-descendants', child]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::CX$LEAF-DESCENDANTS 
wl: lambda_def(defun,
	      u_cx_c36_leaf_descendants,
	      f_u_cx_c36_leaf_descendants,
	      [u_self],
	      
	      [ 
		[ if,
		  [u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]],
		  [list, u_self],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    
		    [ u_yfor,
		      u_child,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			[append, u_result, [u_cx_c36_leaf_descendants, u_child]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::CX$LEAF-DESCENDANTS 
wl: arglist_info(u_cx_c36_leaf_descendants,
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

% annotating U::CX$LEAF-DESCENDANTS 
wl: init_args(exact_only, u_cx_c36_leaf_descendants).


% annotating U::CX$LEAF-DESCENDANTS 
f_u_cx_c36_leaf_descendants(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_null_c63([u_ob_c36_get, u_self, [quote, u_children]], IFTEST),
	(   IFTEST\==[]
	->  FnResult=[Self_Param]
	;   f_u_yloop(
		      [ [u_initial, [u_result, []]],
			
			[ u_yfor,
			  u_child,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_children]]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ append,
			      u_result,
			      [u_cx_c36_leaf_descendants, u_child]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_leaf_descendants, classof, claz_function),
   set_opv(u_cx_c36_leaf_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_leaf_descendants, function, f_u_cx_c36_leaf_descendants),
   DefunResult=u_cx_c36_leaf_descendants.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:4862 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$descendants',
			    [self],
			    
			    [ if,
			      ['null?', ['ob$get', self, [quote, children]]],
			      [list, self],
			      
			      [ yloop,
				[initial, [result, [list, self]]],
				
				[ yfor,
				  child,
				  in,
				  ['ob$get', self, [quote, children]]
				],
				
				[ ydo,
				  
				  [ setq,
				    result,
				    [append, result, ['cx$descendants', child]]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::CX$DESCENDANTS 
wl: lambda_def(defun,
	      u_cx_c36_descendants,
	      f_u_cx_c36_descendants,
	      [u_self],
	      
	      [ 
		[ if,
		  [u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]],
		  [list, u_self],
		  
		  [ u_yloop,
		    [u_initial, [u_result, [list, u_self]]],
		    
		    [ u_yfor,
		      u_child,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			[append, u_result, [u_cx_c36_descendants, u_child]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::CX$DESCENDANTS 
wl: arglist_info(u_cx_c36_descendants,
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

% annotating U::CX$DESCENDANTS 
wl: init_args(exact_only, u_cx_c36_descendants).


% annotating U::CX$DESCENDANTS 
f_u_cx_c36_descendants(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_null_c63([u_ob_c36_get, u_self, [quote, u_children]], IFTEST),
	(   IFTEST\==[]
	->  FnResult=[Self_Param]
	;   f_u_yloop(
		      [ [u_initial, [u_result, [list, u_self]]],
			
			[ u_yfor,
			  u_child,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_children]]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    [append, u_result, [u_cx_c36_descendants, u_child]]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_descendants, classof, claz_function),
   set_opv(u_cx_c36_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_descendants, function, f_u_cx_c36_descendants),
   DefunResult=u_cx_c36_descendants.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5150 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$tree-print',
			    [self],
			    
			    [ map,
			      [quote, list],
			      [lambda, [x], ['cx$print', x]],
			      ['cx$descendants', self]
			    ]
			  ]).

% annotating U::CX$TREE-PRINT 
wl: lambda_def(defun,
	      u_cx_c36_tree_print,
	      f_u_cx_c36_tree_print,
	      [u_self],
	      
	      [ 
		[ map,
		  [quote, list],
		  [lambda, [u_x], [u_cx_c36_print, u_x]],
		  [u_cx_c36_descendants, u_self]
		]
	      ]).


% annotating U::CX$TREE-PRINT 
wl: arglist_info(u_cx_c36_tree_print,
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

% annotating U::CX$TREE-PRINT 
wl: init_args(exact_only, u_cx_c36_tree_print).


% annotating U::CX$TREE-PRINT 
f_u_cx_c36_tree_print(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	Lambda=closure([Env13|Env], LResult, [u_x],  (get_var(Env13, u_x, X_Get), f_u_cx_c36_print(X_Get, LResult))),
	f_u_cx_c36_descendants(Self_Param, C36_descendants_Ret),
	cl_map(list, Lambda, C36_descendants_Ret, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_cx_c36_tree_print, classof, claz_function),
   set_opv(u_cx_c36_tree_print, compile_as, kw_function),
   set_opv(u_cx_c36_tree_print, function, f_u_cx_c36_tree_print),
   DefunResult=u_cx_c36_tree_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$root',
			    [self],
			    
			    [ if,
			      ['ob$get', self, [quote, ancestors]],
			      [tlast, ['ob$get', self, [quote, ancestors]]],
			      self
			    ]
			  ]).

% annotating U::CX$ROOT 
wl: lambda_def(defun,
	      u_cx_c36_root,
	      f_u_cx_c36_root,
	      [u_self],
	      
	      [ 
		[ if,
		  [u_ob_c36_get, u_self, [quote, u_ancestors]],
		  [u_tlast, [u_ob_c36_get, u_self, [quote, u_ancestors]]],
		  u_self
		]
	      ]).


% annotating U::CX$ROOT 
wl: arglist_info(u_cx_c36_root,
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

% annotating U::CX$ROOT 
wl: init_args(exact_only, u_cx_c36_root).


% annotating U::CX$ROOT 
f_u_cx_c36_root(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_ob_c36_get(Self_Param, u_ancestors, IFTEST),
	(   IFTEST\==[]
	->  f_u_tlast([u_ob_c36_get, u_self, [quote, u_ancestors]], TrueResult),
	    FnResult=TrueResult
	;   FnResult=Self_Param
	).
:- set_opv(f_u_cx_c36_root, classof, claz_function),
   set_opv(u_cx_c36_root, compile_as, kw_function),
   set_opv(u_cx_c36_root, function, f_u_cx_c36_root),
   DefunResult=u_cx_c36_root.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5356 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*disallow-non-leaf?*', []]).
:- set_var(TLEnv3, setq, u_xx_disallow_non_leaf_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5356 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: The above should be set to t if you are using truth",
				     1,
				     5389)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5356 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" maintenance!", 1, 5449)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*hashing?*', t]).
:- set_var(TLEnv3, setq, u_xx_hashing_c63_xx, t).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Hashing flag is global, not on a context-by-context basis. So,",
				     1,
				     5485)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" it is recommended not to change this flag during a session.",
				     1,
				     5550)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Actually, you will be OK if after you change it, you never access",
				     1,
				     5612)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" any contexts used before the change.",
				     1,
				     5680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" RESTRICTION if you are using hashing:",
				     1,
				     5720)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Asserted obs cannot change type. If you want to change the type of",
				     1,
				     5760)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" an ob, first retract it from all the contexts in which it is",
				     1,
				     5829)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" asserted, change the type, then reassert it in all the contexts.",
				     1,
				     5892)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Otherwise, hashing is recommended for faster operation.",
				     1,
				     5960)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Typical speedups (in min:sec)", 1, 6018)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 5:27 no hashing (almost all compiled)",
				     1,
				     6050)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 3:31 hashing with no use of cx$get-all-ty (almost all compiled)",
				     1,
				     6090)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 3:12 hashing with use of cx$get-all-ty (almost all compiled)",
				     1,
				     6156)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 3:02 hashing with use of cx$get-all-ty (all compiled)",
				     1,
				     6219)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 4:05 hashing with use of cx$get-all-ty and no unify reverse (all compiled)",
				     1,
				     6275)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 3:39 hashing with use of cx$get-all-ty and no unify reverse",
				     1,
				     6352)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      and add slot reversal (all compiled but add-slot-val)",
				     1,
				     6414)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 3:06 hashing with use of cx$get-all-ty and no unify reverse",
				     1,
				     6475)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      and add slot reversal (all compiled)",
				     1,
				     6537)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" --- below not run in fresh GATE (garbage and other factors will cause",
				     1,
				     6581)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("     increase)", 1, 6653)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 5:10 no hashing with use of cx$get-all-ty (all compiled)",
				     1,
				     6669)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 3:54 hashing with use of cx$get-all-ty and no unify reverse (all compiled)",
				     1,
				     6728)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 5:22 no hashing with use of cx$get-all-ty and no unify reverse (all compiled)",
				     1,
				     6805)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Very strange: why is it apparently slower with no unify reverse??!!!",
				     1,
				     6886)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Because more predictive slots (e.g., type) are last.",
				     1,
				     6957)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Changed add-slot-value to prepend.",
				     1,
				     7012)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" We could also hash on the type of the OBJ, and other common ob",
				     1,
				     7050)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:5464 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" attributes.", 1, 7115)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:7129 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$assert-hash',
			    [self, ob],
			    
			    [ let,
			      [[type, ['ob$ty', ob]]],
			      
			      [ if,
				['ty?', type],
				
				[ let,
				  
				  [ 
				    [ found,
				      
				      [ assq,
					type,
					['ob$get', self, [quote, 'type-hashing']]
				      ]
				    ]
				  ],
				  
				  [ if,
				    found,
				    [setf, [cdr, found], [cons, ob, [cdr, found]]],
				    
				    [ 'ob$set',
				      self,
				      [quote, 'type-hashing'],
				      
				      [ cons,
					[list, type, ob],
					['ob$get', self, [quote, 'type-hashing']]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::CX$ASSERT-HASH 
wl: lambda_def(defun,
	      u_cx_c36_assert_hash,
	      f_u_cx_c36_assert_hash,
	      [u_self, u_ob],
	      
	      [ 
		[ let,
		  [[type, [u_ob_c36_ty, u_ob]]],
		  
		  [ if,
		    [u_ty_c63, type],
		    
		    [ let,
		      
		      [ 
			[ u_found,
			  
			  [ ext_assq,
			    type,
			    [u_ob_c36_get, u_self, [quote, u_type_hashing]]
			  ]
			]
		      ],
		      
		      [ if,
			u_found,
			[setf, [cdr, u_found], [cons, u_ob, [cdr, u_found]]],
			
			[ u_ob_c36_set,
			  u_self,
			  [quote, u_type_hashing],
			  
			  [ cons,
			    [list, type, u_ob],
			    [u_ob_c36_get, u_self, [quote, u_type_hashing]]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::CX$ASSERT-HASH 
wl: arglist_info(u_cx_c36_assert_hash,
		[u_self, u_ob],
		[Self_Param, Ob_Param],
		arginfo{ all:[u_self, u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob],
			 opt:0,
			 req:[u_self, u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$ASSERT-HASH 
wl: init_args(exact_only, u_cx_c36_assert_hash).


% annotating U::CX$ASSERT-HASH 
f_u_cx_c36_assert_hash(Self_Param, Ob_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param)],
	f_u_ob_c36_ty(u_ob, Type_Init),
	LEnv=[[bv(type, Type_Init)]|Env],
	f_u_ty_c63(type, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    Setf_Env=[[bv(u_found, Found_Init)]|LEnv],
	    get_var(Setf_Env, u_found, IFTEST22),
	    (   IFTEST22\==[]
	    ->  get_var(Setf_Env, u_found, Found_Get27),
		cl_cdr(Found_Get27, Cdr_Ret),
		CAR=[Ob_Param|Cdr_Ret],
		get_var(Setf_Env, u_found, Found_Get30),
		set_place(Setf_Env, setf, [cdr, Found_Get30], [CAR], Setf_R),
		FnResult=Setf_R
	    ;   get_var(Setf_Env, type, Type_Get),
		CAR45=[Type_Get, Ob_Param],
		f_u_ob_c36_get(Self_Param, u_type_hashing, Type_hashing),
		Type_hashing42=[CAR45|Type_hashing],
		f_u_ob_c36_set(Self_Param,
			       u_type_hashing,
			       Type_hashing42,
			       ElseResult),
		FnResult=ElseResult
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_assert_hash, classof, claz_function),
   set_opv(u_cx_c36_assert_hash, compile_as, kw_function),
   set_opv(u_cx_c36_assert_hash, function, f_u_cx_c36_assert_hash),
   DefunResult=u_cx_c36_assert_hash.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:7465 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retract-unhash',
			    [self, ob],
			    
			    [ let,
			      [[type, ['ob$ty', ob]]],
			      
			      [ if,
				['ty?', type],
				
				[ let,
				  
				  [ 
				    [ found,
				      
				      [ assq,
					type,
					['ob$get', self, [quote, 'type-hashing']]
				      ]
				    ]
				  ],
				  
				  [ if,
				    found,
				    
				    [ progn,
				      
				      [ if,
					[not, ['memq?', ob, [cdr, found]]],
					
					[ error,
					  '$STRING'("cx$retract-unhash: I can't unhash ~A"),
					  ob
					]
				      ],
				      
				      [ setf,
					[cdr, found],
					['delq!', ob, [cdr, found]]
				      ]
				    ],
				    
				    [ error,
				      '$STRING'("cx$retract-unhash: Strange, I can't seem to unhash ~A"),
				      ob
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::CX$RETRACT-UNHASH 
wl: lambda_def(defun,
	      u_cx_c36_retract_unhash,
	      f_u_cx_c36_retract_unhash,
	      [u_self, u_ob],
	      
	      [ 
		[ let,
		  [[type, [u_ob_c36_ty, u_ob]]],
		  
		  [ if,
		    [u_ty_c63, type],
		    
		    [ let,
		      
		      [ 
			[ u_found,
			  
			  [ ext_assq,
			    type,
			    [u_ob_c36_get, u_self, [quote, u_type_hashing]]
			  ]
			]
		      ],
		      
		      [ if,
			u_found,
			
			[ progn,
			  
			  [ if,
			    [not, [u_memq_c63, u_ob, [cdr, u_found]]],
			    
			    [ error,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(c),
					 #\(x),
					 #\($),
					 #\(r),
					 #\(e),
					 #\(t),
					 #\(r),
					 #\(a),
					 #\(c),
					 #\(t),
					 #\(-),
					 #\(u),
					 #\(n),
					 #\(h),
					 #\(a),
					 #\(s),
					 #\(h),
					 #\(:),
					 #\(' '),
					 #\('I'),
					 #\(' '),
					 #\(c),
					 #\(a),
					 #\(n),
					 #\('\''),
					 #\(t),
					 #\(' '),
					 #\(u),
					 #\(n),
					 #\(h),
					 #\(a),
					 #\(s),
					 #\(h),
					 #\(' '),
					 #\(~),
					 #\('A')
				       ]),
			      u_ob
			    ]
			  ],
			  [setf, [cdr, u_found], [u_delq_c33, u_ob, [cdr, u_found]]]
			],
			
			[ error,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(c),
				     #\(x),
				     #\($),
				     #\(r),
				     #\(e),
				     #\(t),
				     #\(r),
				     #\(a),
				     #\(c),
				     #\(t),
				     #\(-),
				     #\(u),
				     #\(n),
				     #\(h),
				     #\(a),
				     #\(s),
				     #\(h),
				     #\(:),
				     #\(' '),
				     #\('S'),
				     #\(t),
				     #\(r),
				     #\(a),
				     #\(n),
				     #\(g),
				     #\(e),
				     #\(','),
				     #\(' '),
				     #\('I'),
				     #\(' '),
				     #\(c),
				     #\(a),
				     #\(n),
				     #\('\''),
				     #\(t),
				     #\(' '),
				     #\(s),
				     #\(e),
				     #\(e),
				     #\(m),
				     #\(' '),
				     #\(t),
				     #\(o),
				     #\(' '),
				     #\(u),
				     #\(n),
				     #\(h),
				     #\(a),
				     #\(s),
				     #\(h),
				     #\(' '),
				     #\(~),
				     #\('A')
				   ]),
			  u_ob
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::CX$RETRACT-UNHASH 
wl: arglist_info(u_cx_c36_retract_unhash,
		[u_self, u_ob],
		[Self_Param, Ob_Param],
		arginfo{ all:[u_self, u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob],
			 opt:0,
			 req:[u_self, u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRACT-UNHASH 
wl: init_args(exact_only, u_cx_c36_retract_unhash).


% annotating U::CX$RETRACT-UNHASH 
f_u_cx_c36_retract_unhash(Self_Param, Ob_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param)],
	f_u_ob_c36_ty(u_ob, Type_Init),
	LEnv=[[bv(type, Type_Init)]|Env],
	f_u_ty_c63(type, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    Setf_Env=[[bv(u_found, Found_Init)]|LEnv],
	    get_var(Setf_Env, u_found, IFTEST22),
	    (   IFTEST22\==[]
	    ->  f_u_memq_c63(u_ob, [cdr, u_found], PredArgResult),
		(   PredArgResult==[]
		->  cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					
					[ #\(c),
					  #\(x),
					  #\($),
					  #\(r),
					  #\(e),
					  #\(t),
					  #\(r),
					  #\(a),
					  #\(c),
					  #\(t),
					  #\(-),
					  #\(u),
					  #\(n),
					  #\(h),
					  #\(a),
					  #\(s),
					  #\(h),
					  #\(:),
					  #\(' '),
					  #\('I'),
					  #\(' '),
					  #\(c),
					  #\(a),
					  #\(n),
					  #\('\''),
					  #\(t),
					  #\(' '),
					  #\(u),
					  #\(n),
					  #\(h),
					  #\(a),
					  #\(s),
					  #\(h),
					  #\(' '),
					  #\(~),
					  #\('A')
					]),
			       Ob_Param
			     ],
			     TrueResult),
		    _120896=TrueResult
		;   _120896=[]
		),
		f_u_delq_c33(u_ob, [cdr, u_found], Delq_c33_Ret),
		get_var(Setf_Env, u_found, Found_Get33),
		set_place(Setf_Env,
			  setf,
			  [cdr, Found_Get33],
			  [Delq_c33_Ret],
			  Setf_R),
		FnResult=Setf_R
	    ;   cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\(c),
				      #\(x),
				      #\($),
				      #\(r),
				      #\(e),
				      #\(t),
				      #\(r),
				      #\(a),
				      #\(c),
				      #\(t),
				      #\(-),
				      #\(u),
				      #\(n),
				      #\(h),
				      #\(a),
				      #\(s),
				      #\(h),
				      #\(:),
				      #\(' '),
				      #\('S'),
				      #\(t),
				      #\(r),
				      #\(a),
				      #\(n),
				      #\(g),
				      #\(e),
				      #\(','),
				      #\(' '),
				      #\('I'),
				      #\(' '),
				      #\(c),
				      #\(a),
				      #\(n),
				      #\('\''),
				      #\(t),
				      #\(' '),
				      #\(s),
				      #\(e),
				      #\(e),
				      #\(m),
				      #\(' '),
				      #\(t),
				      #\(o),
				      #\(' '),
				      #\(u),
				      #\(n),
				      #\(h),
				      #\(a),
				      #\(s),
				      #\(h),
				      #\(' '),
				      #\(~),
				      #\('A')
				    ]),
			   Ob_Param
			 ],
			 ElseResult),
		FnResult=ElseResult
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_retract_unhash, classof, claz_function),
   set_opv(u_cx_c36_retract_unhash, compile_as, kw_function),
   set_opv(u_cx_c36_retract_unhash, function, f_u_cx_c36_retract_unhash),
   DefunResult=u_cx_c36_retract_unhash.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:7934 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-hash',
			    [self, ob],
			    
			    [ let,
			      [[type, ['ob$ty', ob]]],
			      
			      [ if,
				
				[ and,
				  ['ty?', type],
				  [not, ['ty$instance?', ob, [quote, 'USPECIAL']]],
				  [not, ['ty$instance?', ob, [quote, 'UVAR']]]
				],
				
				[ let,
				  
				  [ 
				    [ found,
				      
				      [ assq,
					type,
					['ob$get', self, [quote, 'type-hashing']]
				      ]
				    ]
				  ],
				  [if, found, [cdr, found], []]
				],
				['ob$get', self, [quote, 'all-obs']]
			      ]
			    ]
			  ]).

% annotating U::CX$RETRIEVE-HASH 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_hash,
	      f_u_cx_c36_retrieve_hash,
	      [u_self, u_ob],
	      
	      [ 
		[ let,
		  [[type, [u_ob_c36_ty, u_ob]]],
		  
		  [ if,
		    
		    [ and,
		      [u_ty_c63, type],
		      [not, [u_ty_c36_instance_c63, u_ob, [quote, u_uspecial]]],
		      [not, [u_ty_c36_instance_c63, u_ob, [quote, u_uvar]]]
		    ],
		    
		    [ let,
		      
		      [ 
			[ u_found,
			  
			  [ ext_assq,
			    type,
			    [u_ob_c36_get, u_self, [quote, u_type_hashing]]
			  ]
			]
		      ],
		      [if, u_found, [cdr, u_found], []]
		    ],
		    [u_ob_c36_get, u_self, [quote, u_all_obs]]
		  ]
		]
	      ]).


% annotating U::CX$RETRIEVE-HASH 
wl: arglist_info(u_cx_c36_retrieve_hash,
		[u_self, u_ob],
		[Self_Param, Ob_Param],
		arginfo{ all:[u_self, u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob],
			 opt:0,
			 req:[u_self, u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE-HASH 
wl: init_args(exact_only, u_cx_c36_retrieve_hash).


% annotating U::CX$RETRIEVE-HASH 
f_u_cx_c36_retrieve_hash(Self_Param, Ob_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param)],
	f_u_ob_c36_ty(u_ob, Type_Init),
	LEnv=[[bv(type, Type_Init)]|Env],
	f_u_ty_c63(type, IFTEST19),
	(   IFTEST19\==[]
	->  f_u_ty_c36_instance_c63(Ob_Param, u_uspecial, PredArgResult),
	    (   PredArgResult==[]
	    ->  f_u_ty_c36_instance_c63(Ob_Param, u_uvar, Uvar),
		cl_not(Uvar, TrueResult),
		IFTEST=TrueResult
	    ;   IFTEST=[]
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    Env=[[bv(u_found, Found_Init)]|LEnv],
	    get_var(Env, u_found, IFTEST32),
	    (   IFTEST32\==[]
	    ->  get_var(Env, u_found, Found_Get36),
		cl_cdr(Found_Get36, TrueResult37),
		FnResult=TrueResult37
	    ;   FnResult=[]
	    )
	;   f_u_ob_c36_get(Self_Param, u_all_obs, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_retrieve_hash, classof, claz_function),
   set_opv(u_cx_c36_retrieve_hash, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_hash, function, f_u_cx_c36_retrieve_hash),
   DefunResult=u_cx_c36_retrieve_hash.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-hash-type',
			    [self, type],
			    
			    [ let,
			      
			      [ 
				[ found,
				  
				  [ assq,
				    type,
				    ['ob$get', self, [quote, 'type-hashing']]
				  ]
				]
			      ],
			      [if, found, [cdr, found], []]
			    ]
			  ]).

% annotating U::CX$RETRIEVE-HASH-TYPE 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_hash_type,
	      f_u_cx_c36_retrieve_hash_type,
	      [u_self, type],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_found,
		      
		      [ ext_assq,
			type,
			[u_ob_c36_get, u_self, [quote, u_type_hashing]]
		      ]
		    ]
		  ],
		  [if, u_found, [cdr, u_found], []]
		]
	      ]).


% annotating U::CX$RETRIEVE-HASH-TYPE 
wl: arglist_info(u_cx_c36_retrieve_hash_type,
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

% annotating U::CX$RETRIEVE-HASH-TYPE 
wl: init_args(exact_only, u_cx_c36_retrieve_hash_type).


% annotating U::CX$RETRIEVE-HASH-TYPE 
f_u_cx_c36_retrieve_hash_type(Self_Param, Type_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(type, Type_Param)],
	f_ext_assq(type,
		   [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		   Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get21),
	    cl_cdr(Found_Get21, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_retrieve_hash_type, classof, claz_function),
   set_opv(u_cx_c36_retrieve_hash_type, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_hash_type, function, f_u_cx_c36_retrieve_hash_type),
   DefunResult=u_cx_c36_retrieve_hash_type.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8425 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$walk',
			    [self, proc],
			    
			    [ yloop,
			      [yfor, ob, in, ['ob$get', self, [quote, 'all-obs']]],
			      [ydo, [funcall, proc, ob]]
			    ]
			  ]).

% annotating U::CX$WALK 
wl: lambda_def(defun,
	      u_cx_c36_walk,
	      f_u_cx_c36_walk,
	      [u_self, u_proc],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		  [u_ydo, [funcall, u_proc, u_ob]]
		]
	      ]).


% annotating U::CX$WALK 
wl: arglist_info(u_cx_c36_walk,
		[u_self, u_proc],
		[Self_Param, Proc_Param],
		arginfo{ all:[u_self, u_proc],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_proc],
			 opt:0,
			 req:[u_self, u_proc],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$WALK 
wl: init_args(exact_only, u_cx_c36_walk).


% annotating U::CX$WALK 
f_u_cx_c36_walk(Self_Param, Proc_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_proc, Proc_Param)],
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		    [u_ydo, [funcall, u_proc, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_walk, classof, claz_function),
   set_opv(u_cx_c36_walk, compile_as, kw_function),
   set_opv(u_cx_c36_walk, function, f_u_cx_c36_walk),
   DefunResult=u_cx_c36_walk.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8532 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$walk-type',
			    [self, proc, type],
			    
			    [ if,
			      '*hashing?*',
			      
			      [ yloop,
				
				[ yfor,
				  ob,
				  in,
				  ['cx$retrieve-hash-type', self, type]
				],
				[ydo, [funcall, proc, ob]]
			      ],
			      
			      [ yloop,
				[yfor, ob, in, ['ob$get', self, [quote, 'all-obs']]],
				
				[ ydo,
				  
				  [ if,
				    ['eq?', type, ['ob$ty', ob]],
				    [funcall, proc, ob]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::CX$WALK-TYPE 
wl: lambda_def(defun,
	      u_cx_c36_walk_type,
	      f_u_cx_c36_walk_type,
	      [u_self, u_proc, type],
	      
	      [ 
		[ if,
		  u_xx_hashing_c63_xx,
		  
		  [ u_yloop,
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_cx_c36_retrieve_hash_type, u_self, type]
		    ],
		    [u_ydo, [funcall, u_proc, u_ob]]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, type, [u_ob_c36_ty, u_ob]],
			[funcall, u_proc, u_ob]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::CX$WALK-TYPE 
wl: arglist_info(u_cx_c36_walk_type,
		[u_self, u_proc, type],
		[Self_Param, Proc_Param, Type_Param],
		arginfo{ all:[u_self, u_proc, type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_proc, type],
			 opt:0,
			 req:[u_self, u_proc, type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$WALK-TYPE 
wl: init_args(exact_only, u_cx_c36_walk_type).


% annotating U::CX$WALK-TYPE 
f_u_cx_c36_walk_type(Self_Param, Proc_Param, Type_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_proc, Proc_Param), bv(type, Type_Param)],
	get_var(Env, u_xx_hashing_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ 
			[ u_yfor,
			  u_ob,
			  u_in,
			  [u_cx_c36_retrieve_hash_type, u_self, type]
			],
			[u_ydo, [funcall, u_proc, u_ob]]
		      ],
		      TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ 
			[ u_yfor,
			  u_ob,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_all_obs]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_eq_c63, type, [u_ob_c36_ty, u_ob]],
			    [funcall, u_proc, u_ob]
			  ]
			]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_walk_type, classof, claz_function),
   set_opv(u_cx_c36_walk_type, compile_as, kw_function),
   set_opv(u_cx_c36_walk_type, function, f_u_cx_c36_walk_type),
   DefunResult=u_cx_c36_walk_type.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8819 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$get-all',
			    [self],
			    ['ob$get', self, [quote, 'all-obs']]
			  ]).

% annotating U::CX$GET-ALL 
wl: lambda_def(defun,
	      u_cx_c36_get_all,
	      f_u_cx_c36_get_all,
	      [u_self],
	      [[u_ob_c36_get, u_self, [quote, u_all_obs]]]).


% annotating U::CX$GET-ALL 
wl: arglist_info(u_cx_c36_get_all,
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

% annotating U::CX$GET-ALL 
wl: init_args(exact_only, u_cx_c36_get_all).


% annotating U::CX$GET-ALL 
f_u_cx_c36_get_all(Self_Param, FnResult) :-
	f_u_ob_c36_get(Self_Param, u_all_obs, All_obs),
	All_obs=FnResult.
:- set_opv(f_u_cx_c36_get_all, classof, claz_function),
   set_opv(u_cx_c36_get_all, compile_as, kw_function),
   set_opv(u_cx_c36_get_all, function, f_u_cx_c36_get_all),
   DefunResult=u_cx_c36_get_all.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8819 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8872)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8819 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Use of this will artificially slow down non-hashing, but hashing has",
				     1,
				     8874)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8819 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" already been proven without this.",
				     1,
				     8945)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8819 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8981)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:8982 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$get-all-ty',
			    [self, type],
			    
			    [ if,
			      '*hashing?*',
			      
			      [ let,
				
				[ 
				  [ found,
				    
				    [ assq,
				      type,
				      ['ob$get', self, [quote, 'type-hashing']]
				    ]
				  ]
				],
				[if, found, [cdr, found], []]
			      ],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, ob, in, ['ob$get', self, [quote, 'all-obs']]],
				
				[ ydo,
				  
				  [ if,
				    ['eq?', type, ['ob$ty', ob]],
				    [setq, result, [cons, ob, result]]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::CX$GET-ALL-TY 
wl: lambda_def(defun,
	      u_cx_c36_get_all_ty,
	      f_u_cx_c36_get_all_ty,
	      [u_self, type],
	      
	      [ 
		[ if,
		  u_xx_hashing_c63_xx,
		  
		  [ let,
		    
		    [ 
		      [ u_found,
			
			[ ext_assq,
			  type,
			  [u_ob_c36_get, u_self, [quote, u_type_hashing]]
			]
		      ]
		    ],
		    [if, u_found, [cdr, u_found], []]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, type, [u_ob_c36_ty, u_ob]],
			[setq, u_result, [cons, u_ob, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::CX$GET-ALL-TY 
wl: arglist_info(u_cx_c36_get_all_ty,
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

% annotating U::CX$GET-ALL-TY 
wl: init_args(exact_only, u_cx_c36_get_all_ty).


% annotating U::CX$GET-ALL-TY 
f_u_cx_c36_get_all_ty(Self_Param, Type_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(type, Type_Param)],
	get_var(Env, u_xx_hashing_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(type,
		       [u_ob_c36_get, u_self, [quote, u_type_hashing]],
		       Found_Init),
	    LEnv=[[bv(u_found, Found_Init)]|Env],
	    get_var(LEnv, u_found, IFTEST20),
	    (   IFTEST20\==[]
	    ->  get_var(LEnv, u_found, Found_Get24),
		cl_cdr(Found_Get24, TrueResult),
		FnResult=TrueResult
	    ;   FnResult=[]
	    )
	;   f_u_yloop(
		      [ [u_initial, [u_result, []]],
			
			[ u_yfor,
			  u_ob,
			  u_in,
			  [u_ob_c36_get, u_self, [quote, u_all_obs]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_eq_c63, type, [u_ob_c36_ty, u_ob]],
			    [setq, u_result, [cons, u_ob, u_result]]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_get_all_ty, classof, claz_function),
   set_opv(u_cx_c36_get_all_ty, compile_as, kw_function),
   set_opv(u_cx_c36_get_all_ty, function, f_u_cx_c36_get_all_ty),
   DefunResult=u_cx_c36_get_all_ty.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:9374 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$stats',
			    [self],
			    
			    [ format,
			      '*gate-output*',
			      '$STRING'("Hash stats for ~A~%"),
			      self
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				
				[ total,
				  [length, ['ob$get', self, [quote, 'all-obs']]]
				],
				[count, 0],
				[len, []]
			      ],
			      
			      [ yfor,
				elem,
				in,
				['ob$get', self, [quote, 'type-hashing']]
			      ],
			      
			      [ ydo,
				[setq, len, [length, [cdr, elem]]],
				[setq, count, [+, len, count]],
				
				[ format,
				  '*gate-output*',
				  '$STRING'("~A has ~A entries (~A percent)~%"),
				  [car, elem],
				  len,
				  
				  [ 'flonum->fixnum',
				    
				    [ 'fl*',
				      
				      [ 'fl/',
					['fixnum->flonum', len],
					['fixnum->flonum', total]
				      ],
				      100.0
				    ]
				  ]
				]
			      ],
			      
			      [ yresult,
				
				[ format,
				  '*gate-output*',
				  '$STRING'("There are ~A non typed-hashed entries (~A percent)~%"),
				  [-, total, count],
				  
				  [ 'flonum->fixnum',
				    
				    [ 'fl*',
				      
				      [ 'fl/',
					['fixnum->flonum', [-, total, count]],
					['fixnum->flonum', total]
				      ],
				      100.0
				    ]
				  ]
				],
				
				[ format,
				  '*gate-output*',
				  '$STRING'("Total of ~A entries~%"),
				  total
				]
			      ]
			    ]
			  ]).

% annotating U::CX$STATS 
wl: lambda_def(defun,
	      u_cx_c36_stats,
	      f_u_cx_c36_stats,
	      [u_self],
	      
	      [ 
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('H'),
			     #\(a),
			     #\(s),
			     #\(h),
			     #\(' '),
			     #\(s),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(~),
			     #\('%')
			   ]),
		  u_self
		],
		
		[ u_yloop,
		  
		  [ u_initial,
		    [u_total, [length, [u_ob_c36_get, u_self, [quote, u_all_obs]]]],
		    [count, 0],
		    [u_len, []]
		  ],
		  
		  [ u_yfor,
		    u_elem,
		    u_in,
		    [u_ob_c36_get, u_self, [quote, u_type_hashing]]
		  ],
		  
		  [ u_ydo,
		    [setq, u_len, [length, [cdr, u_elem]]],
		    [setq, count, [+, u_len, count]],
		    
		    [ format,
		      u_xx_gate_output_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('A'),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(s),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(e),
				 #\(n),
				 #\(t),
				 #\(r),
				 #\(i),
				 #\(e),
				 #\(s),
				 #\(' '),
				 #\('('),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(p),
				 #\(e),
				 #\(r),
				 #\(c),
				 #\(e),
				 #\(n),
				 #\(t),
				 #\(')'),
				 #\(~),
				 #\('%')
			       ]),
		      [car, u_elem],
		      u_len,
		      
		      [ u_flonum_c62_fixnum,
			
			[ u_fl_xx,
			  
			  [ u_fl_c47,
			    [u_fixnum_c62_flonum, u_len],
			    [u_fixnum_c62_flonum, u_total]
			  ],
			  100.0
			]
		      ]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ format,
		      u_xx_gate_output_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('T'),
				 #\(h),
				 #\(e),
				 #\(r),
				 #\(e),
				 #\(' '),
				 #\(a),
				 #\(r),
				 #\(e),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(n),
				 #\(' '),
				 #\(t),
				 #\(y),
				 #\(p),
				 #\(e),
				 #\(d),
				 #\(-),
				 #\(h),
				 #\(a),
				 #\(s),
				 #\(h),
				 #\(e),
				 #\(d),
				 #\(' '),
				 #\(e),
				 #\(n),
				 #\(t),
				 #\(r),
				 #\(i),
				 #\(e),
				 #\(s),
				 #\(' '),
				 #\('('),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(p),
				 #\(e),
				 #\(r),
				 #\(c),
				 #\(e),
				 #\(n),
				 #\(t),
				 #\(')'),
				 #\(~),
				 #\('%')
			       ]),
		      [-, u_total, count],
		      
		      [ u_flonum_c62_fixnum,
			
			[ u_fl_xx,
			  
			  [ u_fl_c47,
			    [u_fixnum_c62_flonum, [-, u_total, count]],
			    [u_fixnum_c62_flonum, u_total]
			  ],
			  100.0
			]
		      ]
		    ],
		    
		    [ format,
		      u_xx_gate_output_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('T'),
				 #\(o),
				 #\(t),
				 #\(a),
				 #\(l),
				 #\(' '),
				 #\(o),
				 #\(f),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(e),
				 #\(n),
				 #\(t),
				 #\(r),
				 #\(i),
				 #\(e),
				 #\(s),
				 #\(~),
				 #\('%')
			       ]),
		      u_total
		    ]
		  ]
		]
	      ]).


% annotating U::CX$STATS 
wl: arglist_info(u_cx_c36_stats,
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

% annotating U::CX$STATS 
wl: init_args(exact_only, u_cx_c36_stats).


% annotating U::CX$STATS 
f_u_cx_c36_stats(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('H'),
			       #\(a),
			       #\(s),
			       #\(h),
			       #\(' '),
			       #\(s),
			       #\(t),
			       #\(a),
			       #\(t),
			       #\(s),
			       #\(' '),
			       #\(f),
			       #\(o),
			       #\(r),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(~),
			       #\('%')
			     ]),
		    Self_Param
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      
		      [ u_total,
			[length, [u_ob_c36_get, u_self, [quote, u_all_obs]]]
		      ],
		      [count, 0],
		      [u_len, []]
		    ],
		    
		    [ u_yfor,
		      u_elem,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_type_hashing]]
		    ],
		    
		    [ u_ydo,
		      [setq, u_len, [length, [cdr, u_elem]]],
		      [setq, count, [+, u_len, count]],
		      
		      [ format,
			u_xx_gate_output_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(~),
				   #\('A'),
				   #\(' '),
				   #\(h),
				   #\(a),
				   #\(s),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(r),
				   #\(i),
				   #\(e),
				   #\(s),
				   #\(' '),
				   #\('('),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(p),
				   #\(e),
				   #\(r),
				   #\(c),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(')'),
				   #\(~),
				   #\('%')
				 ]),
			[car, u_elem],
			u_len,
			
			[ u_flonum_c62_fixnum,
			  
			  [ u_fl_xx,
			    
			    [ u_fl_c47,
			      [u_fixnum_c62_flonum, u_len],
			      [u_fixnum_c62_flonum, u_total]
			    ],
			    100.0
			  ]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ format,
			u_xx_gate_output_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('T'),
				   #\(h),
				   #\(e),
				   #\(r),
				   #\(e),
				   #\(' '),
				   #\(a),
				   #\(r),
				   #\(e),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(n),
				   #\(o),
				   #\(n),
				   #\(' '),
				   #\(t),
				   #\(y),
				   #\(p),
				   #\(e),
				   #\(d),
				   #\(-),
				   #\(h),
				   #\(a),
				   #\(s),
				   #\(h),
				   #\(e),
				   #\(d),
				   #\(' '),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(r),
				   #\(i),
				   #\(e),
				   #\(s),
				   #\(' '),
				   #\('('),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(p),
				   #\(e),
				   #\(r),
				   #\(c),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(')'),
				   #\(~),
				   #\('%')
				 ]),
			[-, u_total, count],
			
			[ u_flonum_c62_fixnum,
			  
			  [ u_fl_xx,
			    
			    [ u_fl_c47,
			      [u_fixnum_c62_flonum, [-, u_total, count]],
			      [u_fixnum_c62_flonum, u_total]
			    ],
			    100.0
			  ]
			]
		      ],
		      
		      [ format,
			u_xx_gate_output_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('T'),
				   #\(o),
				   #\(t),
				   #\(a),
				   #\(l),
				   #\(' '),
				   #\(o),
				   #\(f),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(r),
				   #\(i),
				   #\(e),
				   #\(s),
				   #\(~),
				   #\('%')
				 ]),
			u_total
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_stats, classof, claz_function),
   set_opv(u_cx_c36_stats, compile_as, kw_function),
   set_opv(u_cx_c36_stats, function, f_u_cx_c36_stats),
   DefunResult=u_cx_c36_stats.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:10339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$assert',
			    [self, ob],
			    
			    [ if,
			      
			      [ and,
				'*disallow-non-leaf?*',
				['ob$get', self, [quote, children]]
			      ],
			      
			      [ error,
				'$STRING'("Cannot assert ~A in non-leaf context ~A."),
				ob,
				self
			      ]
			    ],
			    ['assert-dbg', ob, self],
			    
			    [ if,
			      ['cx$true?', self, ob],
			      
			      [ ndbg,
				'*gate-dbg*',
				context,
				'$STRING'("Assert: ~A already true in ~A~%"),
				ob,
				self
			      ],
			      
			      [ progn,
				
				[ if,
				  '*gen-stream*',
				  
				  [ generate,
				    ob,
				    self,
				    
				    [ append,
				      '*global-switches*',
				      ['ob$get', self, [quote, 'gen-switches']]
				    ]
				  ]
				],
				[if, '*hashing?*', ['cx$assert-hash', self, ob]],
				
				[ 'ob$set',
				  self,
				  [quote, 'add-obs'],
				  [cons, ob, ['ob$get', self, [quote, 'add-obs']]]
				],
				
				[ 'ob$set',
				  self,
				  [quote, 'all-obs'],
				  [cons, ob, ['ob$get', self, [quote, 'all-obs']]]
				],
				
				[ if,
				  ['touchable-fact?', ob],
				  
				  [ 'ob$set',
				    self,
				    [quote, 'touched-facts'],
				    
				    [ cons,
				      ob,
				      ['ob$get', self, [quote, 'touched-facts']]
				    ]
				  ]
				],
				['ob$add', ob, [quote, 'top-context'], self]
			      ]
			    ],
			    ob
			  ]).

% annotating U::CX$ASSERT 
wl: lambda_def(defun,
	      u_cx_c36_assert,
	      f_u_cx_c36_assert,
	      [u_self, u_ob],
	      
	      [ 
		[ if,
		  
		  [ and,
		    u_xx_disallow_non_leaf_c63_xx,
		    [u_ob_c36_get, u_self, [quote, u_children]]
		  ],
		  
		  [ error,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('C'),
			       #\(a),
			       #\(n),
			       #\(n),
			       #\(o),
			       #\(t),
			       #\(' '),
			       #\(a),
			       #\(s),
			       #\(s),
			       #\(e),
			       #\(r),
			       #\(t),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(n),
			       #\(o),
			       #\(n),
			       #\(-),
			       #\(l),
			       #\(e),
			       #\(a),
			       #\(f),
			       #\(' '),
			       #\(c),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(x),
			       #\(t),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\('.')
			     ]),
		    u_ob,
		    u_self
		  ]
		],
		[u_assert_dbg, u_ob, u_self],
		
		[ if,
		  [u_cx_c36_true_c63, u_self, u_ob],
		  
		  [ u_ndbg,
		    u_xx_gate_dbg_xx,
		    u_context,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('A'),
			       #\(s),
			       #\(s),
			       #\(e),
			       #\(r),
			       #\(t),
			       #\(:),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(a),
			       #\(l),
			       #\(r),
			       #\(e),
			       #\(a),
			       #\(d),
			       #\(y),
			       #\(' '),
			       #\(t),
			       #\(r),
			       #\(u),
			       #\(e),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(~),
			       #\('%')
			     ]),
		    u_ob,
		    u_self
		  ],
		  
		  [ progn,
		    
		    [ if,
		      u_xx_gen_stream_xx,
		      
		      [ u_generate,
			u_ob,
			u_self,
			
			[ append,
			  u_xx_global_switches_xx,
			  [u_ob_c36_get, u_self, [quote, u_gen_switches]]
			]
		      ]
		    ],
		    
		    [ if,
		      u_xx_hashing_c63_xx,
		      [u_cx_c36_assert_hash, u_self, u_ob]
		    ],
		    
		    [ u_ob_c36_set,
		      u_self,
		      [quote, u_add_obs],
		      [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_add_obs]]]
		    ],
		    
		    [ u_ob_c36_set,
		      u_self,
		      [quote, u_all_obs],
		      [cons, u_ob, [u_ob_c36_get, u_self, [quote, u_all_obs]]]
		    ],
		    
		    [ if,
		      [u_touchable_fact_c63, u_ob],
		      
		      [ u_ob_c36_set,
			u_self,
			[quote, u_touched_facts],
			
			[ cons,
			  u_ob,
			  [u_ob_c36_get, u_self, [quote, u_touched_facts]]
			]
		      ]
		    ],
		    [u_ob_c36_add, u_ob, [quote, u_top_context], u_self]
		  ]
		],
		u_ob
	      ]).


% annotating U::CX$ASSERT 
wl: arglist_info(u_cx_c36_assert,
		[u_self, u_ob],
		[Self_Param, Ob_Param],
		arginfo{ all:[u_self, u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob],
			 opt:0,
			 req:[u_self, u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$ASSERT 
wl: init_args(exact_only, u_cx_c36_assert).


% annotating U::CX$ASSERT 
f_u_cx_c36_assert(Self_Param, Ob_Param, Ob_Param) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param)],
	get_var(Env, u_xx_disallow_non_leaf_c63_xx, IFTEST16),
	(   IFTEST16\==[]
	->  f_u_ob_c36_get(Self_Param, u_children, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('C'),
				  #\(a),
				  #\(n),
				  #\(n),
				  #\(o),
				  #\(t),
				  #\(' '),
				  #\(a),
				  #\(s),
				  #\(s),
				  #\(e),
				  #\(r),
				  #\(t),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(n),
				  #\(o),
				  #\(n),
				  #\(-),
				  #\(l),
				  #\(e),
				  #\(a),
				  #\(f),
				  #\(' '),
				  #\(c),
				  #\(o),
				  #\(n),
				  #\(t),
				  #\(e),
				  #\(x),
				  #\(t),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\('.')
				]),
		       Ob_Param,
		       Self_Param
		     ],
		     TrueResult23),
	    _122382=TrueResult23
	;   _122382=[]
	),
	f_u_assert_dbg(Ob_Param, Self_Param, Assert_dbg_Ret),
	f_u_cx_c36_true_c63(Self_Param, Ob_Param, IFTEST26),
	(   IFTEST26\==[]
	->  f_u_ndbg(u_xx_gate_dbg_xx,
		     u_context,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('A'),
				  #\(s),
				  #\(s),
				  #\(e),
				  #\(r),
				  #\(t),
				  #\(:),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(a),
				  #\(l),
				  #\(r),
				  #\(e),
				  #\(a),
				  #\(d),
				  #\(y),
				  #\(' '),
				  #\(t),
				  #\(r),
				  #\(u),
				  #\(e),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(~),
				  #\('%')
				]),
		       u_ob,
		       u_self
		     ],
		     TrueResult58),
	    _122700=TrueResult58
	;   get_var(Env, u_xx_gen_stream_xx, IFTEST30),
	    (   IFTEST30\==[]
	    ->  get_var(Env, u_xx_global_switches_xx, Xx_global_switches_xx_Get),
		f_u_ob_c36_get(Self_Param, u_gen_switches, Gen_switches),
		cl_append(Xx_global_switches_xx_Get, Gen_switches, Append_Ret),
		f_u_generate(Ob_Param, Self_Param, Append_Ret, TrueResult37),
		_122804=TrueResult37
	    ;   _122804=[]
	    ),
	    get_var(Env, u_xx_hashing_c63_xx, IFTEST38),
	    (   IFTEST38\==[]
	    ->  f_u_cx_c36_assert_hash(Self_Param, Ob_Param, TrueResult43),
		_123106=TrueResult43
	    ;   _123106=[]
	    ),
	    f_u_ob_c36_get(Self_Param, u_add_obs, Add_obs),
	    Add_obs65=[Ob_Param|Add_obs],
	    f_u_ob_c36_set(Self_Param, u_add_obs, Add_obs65, C36_set_Ret),
	    f_u_ob_c36_get(Self_Param, u_all_obs, All_obs),
	    All_obs67=[Ob_Param|All_obs],
	    f_u_ob_c36_set(Self_Param, u_all_obs, All_obs67, C36_set_Ret73),
	    f_u_touchable_fact_c63(u_ob, IFTEST50),
	    (   IFTEST50\==[]
	    ->  f_u_ob_c36_get(Self_Param, u_touched_facts, Touched_facts),
		Touched_facts69=[Ob_Param|Touched_facts],
		f_u_ob_c36_set(Self_Param,
			       u_touched_facts,
			       Touched_facts69,
			       TrueResult55),
		_123524=TrueResult55
	    ;   _123524=[]
	    ),
	    f_u_ob_c36_add(Ob_Param, u_top_context, Self_Param, ElseResult),
	    _122700=ElseResult
	).
:- set_opv(f_u_cx_c36_assert, classof, claz_function),
   set_opv(u_cx_c36_assert, compile_as, kw_function),
   set_opv(u_cx_c36_assert, function, f_u_cx_c36_assert),
   DefunResult=u_cx_c36_assert.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11152 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'assert-dbg',
			    [ob, self],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Assert ~A in ~A"),
			      ob,
			      ['ob->string', self]
			    ]
			  ]).

% annotating U::ASSERT-DBG 
wl: lambda_def(defun,
	      u_assert_dbg,
	      f_u_assert_dbg,
	      [u_ob, u_self],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(s),
			     #\(s),
			     #\(e),
			     #\(r),
			     #\(t),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_ob,
		  [u_ob_c62_string, u_self]
		]
	      ]).


% annotating U::ASSERT-DBG 
wl: arglist_info(u_assert_dbg,
		[u_ob, u_self],
		[Ob_Param, Self_Param],
		arginfo{ all:[u_ob, u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_self],
			 opt:0,
			 req:[u_ob, u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ASSERT-DBG 
wl: init_args(exact_only, u_assert_dbg).


% annotating U::ASSERT-DBG 
f_u_assert_dbg(Ob_Param, Self_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_self, Self_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(s),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(t),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ob,
			    [u_ob_c62_string, u_self]
			  ],
			  Roman_nl_Ret),
	Roman_nl_Ret=FnResult.
:- set_opv(f_u_assert_dbg, classof, claz_function),
   set_opv(u_assert_dbg, compile_as, kw_function),
   set_opv(u_assert_dbg, function, f_u_assert_dbg),
   DefunResult=u_assert_dbg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11152 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ob$pr ob *gate-dbg* *ob-print-options*)",
				     1,
				     11257)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11152 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (do-newline *gate-dbg*)", 1, 11301)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'retract-dbg',
			    [ob, self],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Retract ~A in ~A"),
			      ob,
			      ['ob->string', self]
			    ]
			  ]).

% annotating U::RETRACT-DBG 
wl: lambda_def(defun,
	      u_retract_dbg,
	      f_u_retract_dbg,
	      [u_ob, u_self],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(e),
			     #\(t),
			     #\(r),
			     #\(a),
			     #\(c),
			     #\(t),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_ob,
		  [u_ob_c62_string, u_self]
		]
	      ]).


% annotating U::RETRACT-DBG 
wl: arglist_info(u_retract_dbg,
		[u_ob, u_self],
		[Ob_Param, Self_Param],
		arginfo{ all:[u_ob, u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_self],
			 opt:0,
			 req:[u_ob, u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RETRACT-DBG 
wl: init_args(exact_only, u_retract_dbg).


% annotating U::RETRACT-DBG 
f_u_retract_dbg(Ob_Param, Self_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_self, Self_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(t),
				       #\(r),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ob,
			    [u_ob_c62_string, u_self]
			  ],
			  Roman_nl_Ret),
	Roman_nl_Ret=FnResult.
:- set_opv(f_u_retract_dbg, classof, claz_function),
   set_opv(u_retract_dbg, compile_as, kw_function),
   set_opv(u_retract_dbg, function, f_u_retract_dbg),
   DefunResult=u_retract_dbg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ob$pr ob *gate-dbg* *ob-print-options*)",
				     1,
				     11437)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (do-newline *gate-dbg*)", 1, 11481)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11510 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ctxt-unify-semantics?*', []]).
:- set_var(TLEnv3, setq, u_xx_ctxt_unify_semantics_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retract',
			    [self, ob],
			    
			    [ if,
			      
			      [ and,
				'*disallow-non-leaf?*',
				['ob$get', self, [quote, children]]
			      ],
			      
			      [ error,
				'$STRING'("Cannot retract ~A in non-leaf context ~A."),
				ob,
				self
			      ]
			    ],
			    
			    [ if,
			      [not, ['cx$true?', self, ob]],
			      
			      [ ndbg,
				'*gate-dbg*',
				context,
				'$STRING'("Retract: ~A already false in ~A~%"),
				ob,
				self
			      ],
			      
			      [ let,
				[[found, []]],
				['retract-dbg', ob, self],
				
				[ if,
				  
				  [ 'memq?',
				    self,
				    ['ob$gets', ob, [quote, 'top-context']]
				  ],
				  ['ob$remove', ob, [quote, 'top-context'], self]
				],
				
				[ if,
				  '*ctxt-unify-semantics?*',
				  
				  [ setq,
				    found,
				    
				    [ or,
				      
				      [ memq,
					ob,
					['ob$get', self, [quote, 'add-obs']]
				      ],
				      
				      [ 'mem-empty-unify',
					ob,
					['ob$get', self, [quote, 'add-obs']],
					self
				      ]
				    ]
				  ],
				  
				  [ setq,
				    found,
				    
				    [ memq,
				      ob,
				      ['ob$get', self, [quote, 'add-obs']]
				    ]
				  ]
				],
				
				[ if,
				  found,
				  
				  [ progn,
				    
				    [ if,
				      '*hashing?*',
				      ['cx$retract-unhash', self, [car, found]]
				    ],
				    
				    [ 'ob$set',
				      self,
				      [quote, 'touched-facts'],
				      
				      [ 'delq!',
					[car, found],
					
					[ 'ob$get',
					  self,
					  [quote, 'touched-facts']
					]
				      ]
				    ],
				    
				    [ 'ob$set',
				      self,
				      [quote, 'add-obs'],
				      
				      [ 'delq!',
					[car, found],
					['ob$get', self, [quote, 'add-obs']]
				      ]
				    ],
				    
				    [ 'ob$set',
				      self,
				      [quote, 'all-obs'],
				      
				      [ 'delq!',
					[car, found],
					['ob$get', self, [quote, 'all-obs']]
				      ]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ if,
				      '*ctxt-unify-semantics?*',
				      
				      [ setq,
					found,
					
					[ or,
					  
					  [ memq,
					    ob,
					    ['ob$get', self, [quote, 'all-obs']]
					  ],
					  
					  [ 'mem-empty-unify',
					    ob,
					    ['ob$get', self, [quote, 'all-obs']],
					    self
					  ]
					]
				      ],
				      
				      [ setq,
					found,
					
					[ memq,
					  ob,
					  ['ob$get', self, [quote, 'all-obs']]
					]
				      ]
				    ],
				    
				    [ if,
				      found,
				      
				      [ progn,
					
					[ if,
					  '*hashing?*',
					  
					  [ 'cx$retract-unhash',
					    self,
					    [car, found]
					  ]
					],
					
					[ 'ob$set',
					  self,
					  [quote, 'remove-obs'],
					  
					  [ cons,
					    [car, found],
					    
					    [ 'ob$get',
					      self,
					      [quote, 'remove-obs']
					    ]
					  ]
					],
					
					[ 'ob$set',
					  self,
					  [quote, 'touched-facts'],
					  
					  [ 'delq!',
					    [car, found],
					    
					    [ 'ob$get',
					      self,
					      [quote, 'touched-facts']
					    ]
					  ]
					],
					
					[ if,
					  '*ctxt-unify-semantics?*',
					  
					  [ 'ob$set',
					    self,
					    [quote, 'all-obs'],
					    
					    [ 'del-unify!',
					      [car, found],
					      
					      [ 'ob$get',
						self,
						[quote, 'all-obs']
					      ],
					      self
					    ]
					  ],
					  
					  [ 'ob$set',
					    self,
					    [quote, 'all-obs'],
					    
					    [ 'delq!',
					      [car, found],
					      
					      [ 'ob$get',
						self,
						[quote, 'all-obs']
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ error,
					'$STRING'("Retract: cannot find ~A."),
					ob
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    ob
			  ]).

% annotating U::CX$RETRACT 
wl: lambda_def(defun,
	      u_cx_c36_retract,
	      f_u_cx_c36_retract,
	      [u_self, u_ob],
	      
	      [ 
		[ if,
		  
		  [ and,
		    u_xx_disallow_non_leaf_c63_xx,
		    [u_ob_c36_get, u_self, [quote, u_children]]
		  ],
		  
		  [ error,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('C'),
			       #\(a),
			       #\(n),
			       #\(n),
			       #\(o),
			       #\(t),
			       #\(' '),
			       #\(r),
			       #\(e),
			       #\(t),
			       #\(r),
			       #\(a),
			       #\(c),
			       #\(t),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(n),
			       #\(o),
			       #\(n),
			       #\(-),
			       #\(l),
			       #\(e),
			       #\(a),
			       #\(f),
			       #\(' '),
			       #\(c),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(x),
			       #\(t),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\('.')
			     ]),
		    u_ob,
		    u_self
		  ]
		],
		
		[ if,
		  [not, [u_cx_c36_true_c63, u_self, u_ob]],
		  
		  [ u_ndbg,
		    u_xx_gate_dbg_xx,
		    u_context,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('R'),
			       #\(e),
			       #\(t),
			       #\(r),
			       #\(a),
			       #\(c),
			       #\(t),
			       #\(:),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(a),
			       #\(l),
			       #\(r),
			       #\(e),
			       #\(a),
			       #\(d),
			       #\(y),
			       #\(' '),
			       #\(f),
			       #\(a),
			       #\(l),
			       #\(s),
			       #\(e),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(~),
			       #\('%')
			     ]),
		    u_ob,
		    u_self
		  ],
		  
		  [ let,
		    [[u_found, []]],
		    [u_retract_dbg, u_ob, u_self],
		    
		    [ if,
		      
		      [ u_memq_c63,
			u_self,
			[u_ob_c36_gets, u_ob, [quote, u_top_context]]
		      ],
		      [u_ob_c36_remove, u_ob, [quote, u_top_context], u_self]
		    ],
		    
		    [ if,
		      u_xx_ctxt_unify_semantics_c63_xx,
		      
		      [ setq,
			u_found,
			
			[ or,
			  
			  [ ext_memq,
			    u_ob,
			    [u_ob_c36_get, u_self, [quote, u_add_obs]]
			  ],
			  
			  [ u_mem_empty_unify,
			    u_ob,
			    [u_ob_c36_get, u_self, [quote, u_add_obs]],
			    u_self
			  ]
			]
		      ],
		      
		      [ setq,
			u_found,
			
			[ ext_memq,
			  u_ob,
			  [u_ob_c36_get, u_self, [quote, u_add_obs]]
			]
		      ]
		    ],
		    
		    [ if,
		      u_found,
		      
		      [ progn,
			
			[ if,
			  u_xx_hashing_c63_xx,
			  [u_cx_c36_retract_unhash, u_self, [car, u_found]]
			],
			
			[ u_ob_c36_set,
			  u_self,
			  [quote, u_touched_facts],
			  
			  [ u_delq_c33,
			    [car, u_found],
			    [u_ob_c36_get, u_self, [quote, u_touched_facts]]
			  ]
			],
			
			[ u_ob_c36_set,
			  u_self,
			  [quote, u_add_obs],
			  
			  [ u_delq_c33,
			    [car, u_found],
			    [u_ob_c36_get, u_self, [quote, u_add_obs]]
			  ]
			],
			
			[ u_ob_c36_set,
			  u_self,
			  [quote, u_all_obs],
			  
			  [ u_delq_c33,
			    [car, u_found],
			    [u_ob_c36_get, u_self, [quote, u_all_obs]]
			  ]
			]
		      ],
		      
		      [ progn,
			
			[ if,
			  u_xx_ctxt_unify_semantics_c63_xx,
			  
			  [ setq,
			    u_found,
			    
			    [ or,
			      
			      [ ext_memq,
				u_ob,
				[u_ob_c36_get, u_self, [quote, u_all_obs]]
			      ],
			      
			      [ u_mem_empty_unify,
				u_ob,
				[u_ob_c36_get, u_self, [quote, u_all_obs]],
				u_self
			      ]
			    ]
			  ],
			  
			  [ setq,
			    u_found,
			    
			    [ ext_memq,
			      u_ob,
			      [u_ob_c36_get, u_self, [quote, u_all_obs]]
			    ]
			  ]
			],
			
			[ if,
			  u_found,
			  
			  [ progn,
			    
			    [ if,
			      u_xx_hashing_c63_xx,
			      [u_cx_c36_retract_unhash, u_self, [car, u_found]]
			    ],
			    
			    [ u_ob_c36_set,
			      u_self,
			      [quote, u_remove_obs],
			      
			      [ cons,
				[car, u_found],
				[u_ob_c36_get, u_self, [quote, u_remove_obs]]
			      ]
			    ],
			    
			    [ u_ob_c36_set,
			      u_self,
			      [quote, u_touched_facts],
			      
			      [ u_delq_c33,
				[car, u_found],
				[u_ob_c36_get, u_self, [quote, u_touched_facts]]
			      ]
			    ],
			    
			    [ if,
			      u_xx_ctxt_unify_semantics_c63_xx,
			      
			      [ u_ob_c36_set,
				u_self,
				[quote, u_all_obs],
				
				[ u_del_unify_c33,
				  [car, u_found],
				  [u_ob_c36_get, u_self, [quote, u_all_obs]],
				  u_self
				]
			      ],
			      
			      [ u_ob_c36_set,
				u_self,
				[quote, u_all_obs],
				
				[ u_delq_c33,
				  [car, u_found],
				  [u_ob_c36_get, u_self, [quote, u_all_obs]]
				]
			      ]
			    ]
			  ],
			  
			  [ error,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(t),
				       #\(r),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(:),
				       #\(' '),
				       #\(c),
				       #\(a),
				       #\(n),
				       #\(n),
				       #\(o),
				       #\(t),
				       #\(' '),
				       #\(f),
				       #\(i),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\('.')
				     ]),
			    u_ob
			  ]
			]
		      ]
		    ]
		  ]
		],
		u_ob
	      ]).


% annotating U::CX$RETRACT 
wl: arglist_info(u_cx_c36_retract,
		[u_self, u_ob],
		[Self_Param, Ob_Param],
		arginfo{ all:[u_self, u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob],
			 opt:0,
			 req:[u_self, u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRACT 
wl: init_args(exact_only, u_cx_c36_retract).


% annotating U::CX$RETRACT 
f_u_cx_c36_retract(Self_Param, Ob_Param, Ob_Param) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param)],
	get_var(Env, u_xx_disallow_non_leaf_c63_xx, IFTEST16),
	(   IFTEST16\==[]
	->  f_u_ob_c36_get(Self_Param, u_children, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('C'),
				  #\(a),
				  #\(n),
				  #\(n),
				  #\(o),
				  #\(t),
				  #\(' '),
				  #\(r),
				  #\(e),
				  #\(t),
				  #\(r),
				  #\(a),
				  #\(c),
				  #\(t),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(n),
				  #\(o),
				  #\(n),
				  #\(-),
				  #\(l),
				  #\(e),
				  #\(a),
				  #\(f),
				  #\(' '),
				  #\(c),
				  #\(o),
				  #\(n),
				  #\(t),
				  #\(e),
				  #\(x),
				  #\(t),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\('.')
				]),
		       Ob_Param,
		       Self_Param
		     ],
		     TrueResult23),
	    _127206=TrueResult23
	;   _127206=[]
	),
	f_u_cx_c36_true_c63(Self_Param, Ob_Param, PredArgResult),
	(   PredArgResult==[]
	->  f_u_ndbg(u_xx_gate_dbg_xx,
		     u_context,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('R'),
				  #\(e),
				  #\(t),
				  #\(r),
				  #\(a),
				  #\(c),
				  #\(t),
				  #\(:),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(a),
				  #\(l),
				  #\(r),
				  #\(e),
				  #\(a),
				  #\(d),
				  #\(y),
				  #\(' '),
				  #\(f),
				  #\(a),
				  #\(l),
				  #\(s),
				  #\(e),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(~),
				  #\('%')
				]),
		       u_ob,
		       u_self
		     ],
		     TrueResult88),
	    TrueResult84=TrueResult88
	;   LEnv=[[bv(u_found, [])]|Env],
	    f_u_retract_dbg(Ob_Param, Self_Param, Retract_dbg_Ret),
	    f_u_memq_c63(u_self,
			 [u_ob_c36_gets, u_ob, [quote, u_top_context]],
			 IFTEST34),
	    (   IFTEST34\==[]
	    ->  f_u_ob_c36_remove(Ob_Param,
				  u_top_context,
				  Self_Param,
				  TrueResult38),
		_127682=TrueResult38
	    ;   _127682=[]
	    ),
	    get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST39),
	    (   IFTEST39\==[]
	    ->  (   f_ext_memq(u_ob,
			       [u_ob_c36_get, u_self, [quote, u_add_obs]],
			       FORM1_Res),
		    FORM1_Res\==[],
		    TrueResult43=FORM1_Res
		->  true
		;   f_u_mem_empty_unify(u_ob,
					
					[ u_ob_c36_get,
					  u_self,
					  [quote, u_add_obs]
					],
					u_self,
					Self),
		    TrueResult43=Self
		),
		set_var(LEnv, u_found, TrueResult43),
		_127810=TrueResult43
	    ;   f_ext_memq(u_ob,
			   [u_ob_c36_get, u_self, [quote, u_add_obs]],
			   ElseResult),
		set_var(LEnv, u_found, ElseResult),
		_127810=ElseResult
	    ),
	    get_var(LEnv, u_found, IFTEST45),
	    (   IFTEST45\==[]
	    ->  get_var(LEnv, u_xx_hashing_c63_xx, IFTEST48),
		(   IFTEST48\==[]
		->  get_var(LEnv, u_found, Found_Get52),
		    cl_car(Found_Get52, Car_Ret),
		    f_u_cx_c36_retract_unhash(Self_Param, Car_Ret, TrueResult53),
		    _128086=TrueResult53
		;   _128086=[]
		),
		f_u_delq_c33([car, u_found],
			     [u_ob_c36_get, u_self, [quote, u_touched_facts]],
			     Touched_facts),
		f_u_ob_c36_set(Self_Param,
			       u_touched_facts,
			       Touched_facts,
			       C36_set_Ret),
		f_u_delq_c33([car, u_found],
			     [u_ob_c36_get, u_self, [quote, u_add_obs]],
			     Add_obs),
		f_u_ob_c36_set(Self_Param, u_add_obs, Add_obs, C36_set_Ret106),
		f_u_delq_c33([car, u_found],
			     [u_ob_c36_get, u_self, [quote, u_all_obs]],
			     All_obs),
		f_u_ob_c36_set(Self_Param, u_all_obs, All_obs, TrueResult86),
		TrueResult84=TrueResult86
	    ;   get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST57),
		(   IFTEST57\==[]
		->  (   f_ext_memq(u_ob,
				   [u_ob_c36_get, u_self, [quote, u_all_obs]],
				   FORM1_Res60),
			FORM1_Res60\==[],
			TrueResult61=FORM1_Res60
		    ->  true
		    ;   f_u_mem_empty_unify(u_ob,
					    
					    [ u_ob_c36_get,
					      u_self,
					      [quote, u_all_obs]
					    ],
					    u_self,
					    Self97),
			TrueResult61=Self97
		    ),
		    set_var(LEnv, u_found, TrueResult61),
		    _128434=TrueResult61
		;   f_ext_memq(u_ob,
			       [u_ob_c36_get, u_self, [quote, u_all_obs]],
			       ElseResult62),
		    set_var(LEnv, u_found, ElseResult62),
		    _128434=ElseResult62
		),
		get_var(LEnv, u_found, IFTEST63),
		(   IFTEST63\==[]
		->  get_var(LEnv, u_xx_hashing_c63_xx, IFTEST66),
		    (   IFTEST66\==[]
		    ->  get_var(LEnv, u_found, Found_Get70),
			cl_car(Found_Get70, Car_Ret107),
			f_u_cx_c36_retract_unhash(Self_Param,
						  Car_Ret107,
						  TrueResult71),
			_128668=TrueResult71
		    ;   _128668=[]
		    ),
		    get_var(LEnv, u_found, Found_Get73),
		    cl_car(Found_Get73, Car_Ret108),
		    f_u_ob_c36_get(Self_Param, u_remove_obs, Remove_obs),
		    Remove_obs99=[Car_Ret108|Remove_obs],
		    f_u_ob_c36_set(Self_Param,
				   u_remove_obs,
				   Remove_obs99,
				   C36_set_Ret109),
		    f_u_delq_c33([car, u_found],
				 [u_ob_c36_get, u_self, [quote, u_touched_facts]],
				 Touched_facts100),
		    f_u_ob_c36_set(Self_Param,
				   u_touched_facts,
				   Touched_facts100,
				   C36_set_Ret110),
		    get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST76),
		    (   IFTEST76\==[]
		    ->  f_u_del_unify_c33([car, u_found],
					  
					  [ u_ob_c36_get,
					    u_self,
					    [quote, u_all_obs]
					  ],
					  u_self,
					  Self101),
			f_u_ob_c36_set(Self_Param,
				       u_all_obs,
				       Self101,
				       TrueResult81),
			TrueResult84=TrueResult81
		    ;   f_u_delq_c33([car, u_found],
				     [u_ob_c36_get, u_self, [quote, u_all_obs]],
				     All_obs102),
			f_u_ob_c36_set(Self_Param,
				       u_all_obs,
				       All_obs102,
				       ElseResult82),
			TrueResult84=ElseResult82
		    )
		;   cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					
					[ #\('R'),
					  #\(e),
					  #\(t),
					  #\(r),
					  #\(a),
					  #\(c),
					  #\(t),
					  #\(:),
					  #\(' '),
					  #\(c),
					  #\(a),
					  #\(n),
					  #\(n),
					  #\(o),
					  #\(t),
					  #\(' '),
					  #\(f),
					  #\(i),
					  #\(n),
					  #\(d),
					  #\(' '),
					  #\(~),
					  #\('A'),
					  #\('.')
					]),
			       Ob_Param
			     ],
			     ElseResult85),
		    TrueResult84=ElseResult85
		)
	    )
	).
:- set_opv(f_u_cx_c36_retract, classof, claz_function),
   set_opv(u_cx_c36_retract, compile_as, kw_function),
   set_opv(u_cx_c36_retract, function, f_u_cx_c36_retract),
   DefunResult=u_cx_c36_retract.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If touched-facts inheritence is disabled, the line",
				     19,
				     13256)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" below is unnecessary, because a touched-fact would",
				     19,
				     13327)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" have to be in add-obs.", 19, 13398)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: For unify semantics, I still don't see why we",
				     19,
				     14050)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" have to unify here. Won't we get an inconsistent state",
				     19,
				     14122)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" if more than one one element matches? Besides, we've",
				     19,
				     14197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" already done the unify.", 19, 14270)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (yloop (yfor justificand in",
				     1,
				     14296)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                             (cx$justificands self (car found)))",
				     1,
				     14343)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                         (ydo (ndbg *gate-dbg* context",
				     1,
				     14409)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                            \"Retracting dependent assertion ~A~%\" justificand)",
				     1,
				     14465)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:11546 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                             (cx$retract self justificand)))",
				     1,
				     14545)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:14693 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$copy',
			    [self, parent],
			    
			    [ yloop,
			      
			      [ initial,
				['new-context', ['cx$sprout', parent]],
				['new-ob', []]
			      ],
			      [yfor, ob, in, ['ob$get', self, [quote, 'all-obs']]],
			      
			      [ ydo,
				[setq, 'new-ob', ob],
				['cx$assert', 'new-context', 'new-ob']
			      ],
			      [yresult, 'new-context']
			    ]
			  ]).

% annotating U::CX$COPY 
wl: lambda_def(defun,
	      u_cx_c36_copy,
	      f_u_cx_c36_copy,
	      [u_self, u_parent],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_new_context, [u_cx_c36_sprout, u_parent]],
		    [u_new_ob, []]
		  ],
		  [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		  
		  [ u_ydo,
		    [setq, u_new_ob, u_ob],
		    [u_cx_c36_assert, u_new_context, u_new_ob]
		  ],
		  [u_yresult, u_new_context]
		]
	      ]).


% annotating U::CX$COPY 
wl: arglist_info(u_cx_c36_copy,
		[u_self, u_parent],
		[Self_Param, Parent_Param],
		arginfo{ all:[u_self, u_parent],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_parent],
			 opt:0,
			 req:[u_self, u_parent],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$COPY 
wl: init_args(exact_only, u_cx_c36_copy).


% annotating U::CX$COPY 
f_u_cx_c36_copy(Self_Param, Parent_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_parent, Parent_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_new_context, [u_cx_c36_sprout, u_parent]],
		      [u_new_ob, []]
		    ],
		    [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_all_obs]]],
		    
		    [ u_ydo,
		      [setq, u_new_ob, u_ob],
		      [u_cx_c36_assert, u_new_context, u_new_ob]
		    ],
		    [u_yresult, u_new_context]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_copy, classof, claz_function),
   set_opv(u_cx_c36_copy, compile_as, kw_function),
   set_opv(u_cx_c36_copy, function, f_u_cx_c36_copy),
   DefunResult=u_cx_c36_copy.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:14693 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was (ob$copy ob) but this would kill all links!",
				     32,
				     14882)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:14693 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (ob$set new-ob 'top-context new-context)",
				     1,
				     14932)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:14693 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (ob$removes new-ob 'top-context)",
				     1,
				     14987)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:15112 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*retrieve-ignore-slots*', []]).
:- set_var(TLEnv3, setq, u_xx_retrieve_ignore_slots_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:15112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns ( <bd: (<retrieved-ob> (<var> <value>)...)> ...)",
				     1,
				     15149)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:15207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve',
			    [self, pattern],
			    
			    [ yloop,
			      [initial, [result, []], [bindings, []]],
			      
			      [ yfor,
				ob,
				in,
				
				[ if,
				  '*hashing?*',
				  ['cx$retrieve-hash', self, pattern],
				  ['ob$get', self, [quote, 'all-obs']]
				]
			      ],
			      
			      [ ydo,
				
				[ setq,
				  bindings,
				  
				  [ 'ob$unify-cx1',
				    pattern,
				    ob,
				    '*empty-bd*',
				    '*retrieve-ignore-slots*',
				    self
				  ]
				],
				
				[ if,
				  bindings,
				  
				  [ setq,
				    result,
				    [cons, [cons, ob, [cdr, bindings]], result]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::CX$RETRIEVE 
wl: lambda_def(defun,
	      u_cx_c36_retrieve,
	      f_u_cx_c36_retrieve,
	      [u_self, u_pattern],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [bindings, []]],
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    
		    [ if,
		      u_xx_hashing_c63_xx,
		      [u_cx_c36_retrieve_hash, u_self, u_pattern],
		      [u_ob_c36_get, u_self, [quote, u_all_obs]]
		    ]
		  ],
		  
		  [ u_ydo,
		    
		    [ setq,
		      bindings,
		      
		      [ u_ob_c36_unify_cx1,
			u_pattern,
			u_ob,
			u_xx_empty_bd_xx,
			u_xx_retrieve_ignore_slots_xx,
			u_self
		      ]
		    ],
		    
		    [ if,
		      bindings,
		      
		      [ setq,
			u_result,
			[cons, [cons, u_ob, [cdr, bindings]], u_result]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::CX$RETRIEVE 
wl: arglist_info(u_cx_c36_retrieve,
		[u_self, u_pattern],
		[Self_Param, Pattern_Param],
		arginfo{ all:[u_self, u_pattern],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_pattern],
			 opt:0,
			 req:[u_self, u_pattern],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE 
wl: init_args(exact_only, u_cx_c36_retrieve).


% annotating U::CX$RETRIEVE 
f_u_cx_c36_retrieve(Self_Param, Pattern_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_pattern, Pattern_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [bindings, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			u_xx_hashing_c63_xx,
			[u_cx_c36_retrieve_hash, u_self, u_pattern],
			[u_ob_c36_get, u_self, [quote, u_all_obs]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			bindings,
			
			[ u_ob_c36_unify_cx1,
			  u_pattern,
			  u_ob,
			  u_xx_empty_bd_xx,
			  u_xx_retrieve_ignore_slots_xx,
			  u_self
			]
		      ],
		      
		      [ if,
			bindings,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_ob, [cdr, bindings]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve, classof, claz_function),
   set_opv(u_cx_c36_retrieve, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve, function, f_u_cx_c36_retrieve),
   DefunResult=u_cx_c36_retrieve.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:15704 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-bd',
			    [self, pattern, bd],
			    
			    [ yloop,
			      [initial, [result, []], [bindings, []]],
			      
			      [ yfor,
				ob,
				in,
				
				[ if,
				  '*hashing?*',
				  ['cx$retrieve-hash', self, pattern],
				  ['ob$get', self, [quote, 'all-obs']]
				]
			      ],
			      
			      [ ydo,
				
				[ setq,
				  bindings,
				  
				  [ 'ob$unify-cx1',
				    pattern,
				    ob,
				    bd,
				    '*retrieve-ignore-slots*',
				    self
				  ]
				],
				
				[ if,
				  bindings,
				  
				  [ setq,
				    result,
				    [cons, [cons, ob, [cdr, bindings]], result]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::CX$RETRIEVE-BD 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_bd,
	      f_u_cx_c36_retrieve_bd,
	      [u_self, u_pattern, u_bd],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [bindings, []]],
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    
		    [ if,
		      u_xx_hashing_c63_xx,
		      [u_cx_c36_retrieve_hash, u_self, u_pattern],
		      [u_ob_c36_get, u_self, [quote, u_all_obs]]
		    ]
		  ],
		  
		  [ u_ydo,
		    
		    [ setq,
		      bindings,
		      
		      [ u_ob_c36_unify_cx1,
			u_pattern,
			u_ob,
			u_bd,
			u_xx_retrieve_ignore_slots_xx,
			u_self
		      ]
		    ],
		    
		    [ if,
		      bindings,
		      
		      [ setq,
			u_result,
			[cons, [cons, u_ob, [cdr, bindings]], u_result]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::CX$RETRIEVE-BD 
wl: arglist_info(u_cx_c36_retrieve_bd,
		[u_self, u_pattern, u_bd],
		[Self_Param, Pattern_Param, Bd_Param],
		arginfo{ all:[u_self, u_pattern, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_pattern, u_bd],
			 opt:0,
			 req:[u_self, u_pattern, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE-BD 
wl: init_args(exact_only, u_cx_c36_retrieve_bd).


% annotating U::CX$RETRIEVE-BD 
f_u_cx_c36_retrieve_bd(Self_Param, Pattern_Param, Bd_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_pattern, Pattern_Param), bv(u_bd, Bd_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [bindings, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			u_xx_hashing_c63_xx,
			[u_cx_c36_retrieve_hash, u_self, u_pattern],
			[u_ob_c36_get, u_self, [quote, u_all_obs]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			bindings,
			
			[ u_ob_c36_unify_cx1,
			  u_pattern,
			  u_ob,
			  u_bd,
			  u_xx_retrieve_ignore_slots_xx,
			  u_self
			]
		      ],
		      
		      [ if,
			bindings,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_ob, [cdr, bindings]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_bd, classof, claz_function),
   set_opv(u_cx_c36_retrieve_bd, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_bd, function, f_u_cx_c36_retrieve_bd),
   DefunResult=u_cx_c36_retrieve_bd.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16199 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-number',
			    [self, pattern, number],
			    
			    [ yloop,
			      [initial, [result, []], [bindings, []]],
			      
			      [ yfor,
				ob,
				in,
				
				[ if,
				  '*hashing?',
				  ['cx$retrieve-hash', self, pattern],
				  ['ob$get', self, [quote, 'all-obs']]
				]
			      ],
			      [ywhile, [<, [length, result], number]],
			      
			      [ ydo,
				
				[ setq,
				  bindings,
				  ['ob$unify-cx', pattern, ob, '*empty-bd*', self]
				],
				
				[ if,
				  bindings,
				  
				  [ setq,
				    result,
				    [cons, [cons, ob, [cdr, bindings]], result]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::CX$RETRIEVE-NUMBER 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_number,
	      f_u_cx_c36_retrieve_number,
	      [u_self, u_pattern, number],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [bindings, []]],
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    
		    [ if,
		      u_xx_hashing_c63,
		      [u_cx_c36_retrieve_hash, u_self, u_pattern],
		      [u_ob_c36_get, u_self, [quote, u_all_obs]]
		    ]
		  ],
		  [u_ywhile, [<, [length, u_result], number]],
		  
		  [ u_ydo,
		    
		    [ setq,
		      bindings,
		      
		      [ u_ob_c36_unify_cx,
			u_pattern,
			u_ob,
			u_xx_empty_bd_xx,
			u_self
		      ]
		    ],
		    
		    [ if,
		      bindings,
		      
		      [ setq,
			u_result,
			[cons, [cons, u_ob, [cdr, bindings]], u_result]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::CX$RETRIEVE-NUMBER 
wl: arglist_info(u_cx_c36_retrieve_number,
		[u_self, u_pattern, number],
		[Self_Param, Pattern_Param, Number_Param],
		arginfo{ all:[u_self, u_pattern, number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_pattern, number],
			 opt:0,
			 req:[u_self, u_pattern, number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE-NUMBER 
wl: init_args(exact_only, u_cx_c36_retrieve_number).


% annotating U::CX$RETRIEVE-NUMBER 
f_u_cx_c36_retrieve_number(Self_Param, Pattern_Param, Number_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_pattern, Pattern_Param), bv(number, Number_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [bindings, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			u_xx_hashing_c63,
			[u_cx_c36_retrieve_hash, u_self, u_pattern],
			[u_ob_c36_get, u_self, [quote, u_all_obs]]
		      ]
		    ],
		    [u_ywhile, [<, [length, u_result], number]],
		    
		    [ u_ydo,
		      
		      [ setq,
			bindings,
			
			[ u_ob_c36_unify_cx,
			  u_pattern,
			  u_ob,
			  u_xx_empty_bd_xx,
			  u_self
			]
		      ],
		      
		      [ if,
			bindings,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_ob, [cdr, bindings]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_number, classof, claz_function),
   set_opv(u_cx_c36_retrieve_number, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_number, function, f_u_cx_c36_retrieve_number),
   DefunResult=u_cx_c36_retrieve_number.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$leaf?',
			    [self],
			    ['null?', ['ob$get', self, [quote, children]]]
			  ]).

% annotating U::CX$LEAF? 
wl: lambda_def(defun,
	      u_cx_c36_leaf_c63,
	      f_u_cx_c36_leaf_c63,
	      [u_self],
	      [[u_null_c63, [u_ob_c36_get, u_self, [quote, u_children]]]]).


% annotating U::CX$LEAF? 
wl: arglist_info(u_cx_c36_leaf_c63,
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

% annotating U::CX$LEAF? 
wl: init_args(exact_only, u_cx_c36_leaf_c63).


% annotating U::CX$LEAF? 
f_u_cx_c36_leaf_c63(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_null_c63([u_ob_c36_get, u_self, [quote, u_children]], Null_c63_Ret),
	Null_c63_Ret=FnResult.
:- set_opv(f_u_cx_c36_leaf_c63, classof, claz_function),
   set_opv(u_cx_c36_leaf_c63, compile_as, kw_function),
   set_opv(u_cx_c36_leaf_c63, function, f_u_cx_c36_leaf_c63),
   DefunResult=u_cx_c36_leaf_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun cx$justificands (self ob)",
				     1,
				     16759)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (yloop (initial (result nil))",
				     1,
				     16793)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (yfor justificand in (ob$gets ob 'justifies))",
				     1,
				     16826)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (ydo (if (cx$true? self justificand)",
				     1,
				     16882)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (setq result (cons justificand result))))",
				     1,
				     16929)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:16699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (yresult result)))", 1, 16990)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:17019 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$true?',
			    [self, ob],
			    
			    [ if,
			      '*ctxt-unify-semantics?*',
			      
			      [ let,
				[['top-contexts', []], [obs, []]],
				
				[ if,
				  
				  [ and,
				    
				    [ not,
				      ['ob$get', self, [quote, 'pseudo-sprout?']]
				    ],
				    
				    [ setq,
				      'top-contexts',
				      ['ob$gets', ob, [quote, 'top-context']]
				    ]
				  ],
				  
				  [ yloop,
				    [initial, [result, []], ['found?', []]],
				    [yfor, 'top-context', in, 'top-contexts'],
				    [yuntil, [or, result, 'found?']],
				    
				    [ ydo,
				      
				      [ cond,
					
					[ ['eq?', 'top-context', self],
					  [setq, result, t]
					],
					
					[ 
					  [ 'memq?',
					    'top-context',
					    ['ob$get', self, [quote, ancestors]]
					  ],
					  [setq, 'found?', t],
					  [setq, result, t],
					  
					  [ yloop,
					    
					    [ yfor,
					      context,
					      in,
					      
					      [ cdr,
						
						[ memq,
						  'top-context',
						  
						  [ reverse,
						    
						    [ cons,
						      self,
						      
						      [ 'ob$get',
							self,
							[quote, ancestors]
						      ]
						    ]
						  ]
						]
					      ]
					    ],
					    
					    [ ydo,
					      
					      [ cond,
						
						[ 
						  [ 'memq?',
						    ob,
						    
						    [ 'ob$get',
						      context,
						      [quote, 'add-obs']
						    ]
						  ],
						  [setq, result, t]
						],
						
						[ 
						  [ 'memq?',
						    ob,
						    
						    [ 'ob$get',
						      context,
						      [quote, 'remove-obs']
						    ]
						  ],
						  [setq, result, []]
						]
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    [yresult, result]
				  ],
				  
				  [ progn,
				    
				    [ setq,
				      obs,
				      
				      [ if,
					'*hashing?*',
					['cx$retrieve-hash', self, ob],
					['ob$get', self, [quote, 'all-obs']]
				      ]
				    ],
				    
				    [ if,
				      '*ctxt-unify-semantics?*',
				      
				      [ or,
					['memq?', ob, obs],
					['mem-empty-unify?', ob, obs, self]
				      ],
				      ['memq?', ob, obs]
				    ]
				  ]
				]
			      ],
			      
			      [ 'memq?',
				ob,
				
				[ if,
				  '*hashing?*',
				  ['cx$retrieve-hash', self, ob],
				  ['ob$get', self, [quote, 'all-obs']]
				]
			      ]
			    ]
			  ]).

% annotating U::CX$TRUE? 
wl: lambda_def(defun,
	      u_cx_c36_true_c63,
	      f_u_cx_c36_true_c63,
	      [u_self, u_ob],
	      
	      [ 
		[ if,
		  u_xx_ctxt_unify_semantics_c63_xx,
		  
		  [ let,
		    [[u_top_contexts, []], [u_obs, []]],
		    
		    [ if,
		      
		      [ and,
			
			[ not,
			  [u_ob_c36_get, u_self, [quote, u_pseudo_sprout_c63]]
			],
			
			[ setq,
			  u_top_contexts,
			  [u_ob_c36_gets, u_ob, [quote, u_top_context]]
			]
		      ],
		      
		      [ u_yloop,
			[u_initial, [u_result, []], [u_found_c63, []]],
			[u_yfor, u_top_context, u_in, u_top_contexts],
			[u_yuntil, [or, u_result, u_found_c63]],
			
			[ u_ydo,
			  
			  [ cond,
			    
			    [ [u_eq_c63, u_top_context, u_self],
			      [setq, u_result, t]
			    ],
			    
			    [ 
			      [ u_memq_c63,
				u_top_context,
				[u_ob_c36_get, u_self, [quote, u_ancestors]]
			      ],
			      [setq, u_found_c63, t],
			      [setq, u_result, t],
			      
			      [ u_yloop,
				
				[ u_yfor,
				  u_context,
				  u_in,
				  
				  [ cdr,
				    
				    [ ext_memq,
				      u_top_context,
				      
				      [ reverse,
					
					[ cons,
					  u_self,
					  
					  [ u_ob_c36_get,
					    u_self,
					    [quote, u_ancestors]
					  ]
					]
				      ]
				    ]
				  ]
				],
				
				[ u_ydo,
				  
				  [ cond,
				    
				    [ 
				      [ u_memq_c63,
					u_ob,
					
					[ u_ob_c36_get,
					  u_context,
					  [quote, u_add_obs]
					]
				      ],
				      [setq, u_result, t]
				    ],
				    
				    [ 
				      [ u_memq_c63,
					u_ob,
					
					[ u_ob_c36_get,
					  u_context,
					  [quote, u_remove_obs]
					]
				      ],
				      [setq, u_result, []]
				    ]
				  ]
				]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      
		      [ progn,
			
			[ setq,
			  u_obs,
			  
			  [ if,
			    u_xx_hashing_c63_xx,
			    [u_cx_c36_retrieve_hash, u_self, u_ob],
			    [u_ob_c36_get, u_self, [quote, u_all_obs]]
			  ]
			],
			
			[ if,
			  u_xx_ctxt_unify_semantics_c63_xx,
			  
			  [ or,
			    [u_memq_c63, u_ob, u_obs],
			    [u_mem_empty_unify_c63, u_ob, u_obs, u_self]
			  ],
			  [u_memq_c63, u_ob, u_obs]
			]
		      ]
		    ]
		  ],
		  
		  [ u_memq_c63,
		    u_ob,
		    
		    [ if,
		      u_xx_hashing_c63_xx,
		      [u_cx_c36_retrieve_hash, u_self, u_ob],
		      [u_ob_c36_get, u_self, [quote, u_all_obs]]
		    ]
		  ]
		]
	      ]).


% annotating U::CX$TRUE? 
wl: arglist_info(u_cx_c36_true_c63,
		[u_self, u_ob],
		[Self_Param, Ob_Param],
		arginfo{ all:[u_self, u_ob],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob],
			 opt:0,
			 req:[u_self, u_ob],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$TRUE? 
wl: init_args(exact_only, u_cx_c36_true_c63).


% annotating U::CX$TRUE? 
f_u_cx_c36_true_c63(Self_Param, Ob_Param, TrueResult40) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param)],
	get_var(Env, u_xx_ctxt_unify_semantics_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  LEnv=[[bv(u_top_contexts, []), bv(u_obs, [])]|Env],
	    f_u_ob_c36_get(Self_Param, u_pseudo_sprout_c63, PredArgResult),
	    (   PredArgResult==[]
	    ->  f_u_ob_c36_gets(Ob_Param, u_top_context, TrueResult),
		set_var(LEnv, u_top_contexts, TrueResult),
		IFTEST19=TrueResult
	    ;   IFTEST19=[]
	    ),
	    (   IFTEST19\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [u_result, []], [u_found_c63, []]],
			    [u_yfor, u_top_context, u_in, u_top_contexts],
			    [u_yuntil, [or, u_result, u_found_c63]],
			    
			    [ u_ydo,
			      
			      [ cond,
				
				[ [u_eq_c63, u_top_context, u_self],
				  [setq, u_result, t]
				],
				
				[ 
				  [ u_memq_c63,
				    u_top_context,
				    [u_ob_c36_get, u_self, [quote, u_ancestors]]
				  ],
				  [setq, u_found_c63, t],
				  [setq, u_result, t],
				  
				  [ u_yloop,
				    
				    [ u_yfor,
				      u_context,
				      u_in,
				      
				      [ cdr,
					
					[ ext_memq,
					  u_top_context,
					  
					  [ reverse,
					    
					    [ cons,
					      u_self,
					      
					      [ u_ob_c36_get,
						u_self,
						[quote, u_ancestors]
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ u_ydo,
				      
				      [ cond,
					
					[ 
					  [ u_memq_c63,
					    u_ob,
					    
					    [ u_ob_c36_get,
					      u_context,
					      [quote, u_add_obs]
					    ]
					  ],
					  [setq, u_result, t]
					],
					
					[ 
					  [ u_memq_c63,
					    u_ob,
					    
					    [ u_ob_c36_get,
					      u_context,
					      [quote, u_remove_obs]
					    ]
					  ],
					  [setq, u_result, []]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    [u_yresult, u_result]
			  ],
			  TrueResult42),
		TrueResult40=TrueResult42
	    ;   get_var(LEnv, u_xx_hashing_c63_xx, IFTEST28),
		(   IFTEST28\==[]
		->  f_u_cx_c36_retrieve_hash(Self_Param, Ob_Param, TrueResult34),
		    Obs=TrueResult34
		;   f_u_ob_c36_get(Self_Param, u_all_obs, ElseResult),
		    Obs=ElseResult
		),
		set_var(LEnv, u_obs, Obs),
		get_var(LEnv, u_xx_ctxt_unify_semantics_c63_xx, IFTEST36),
		(   IFTEST36\==[]
		->  (   f_u_memq_c63(u_ob, u_obs, FORM1_Res),
			FORM1_Res\==[],
			TrueResult40=FORM1_Res
		    ->  true
		    ;   f_u_mem_empty_unify_c63(u_ob, u_obs, u_self, Self),
			TrueResult40=Self
		    )
		;   f_u_memq_c63(u_ob, u_obs, ElseResult41),
		    TrueResult40=ElseResult41
		)
	    )
	;   f_u_memq_c63(u_ob,
			 
			 [ if,
			   u_xx_hashing_c63_xx,
			   [u_cx_c36_retrieve_hash, u_self, u_ob],
			   [u_ob_c36_get, u_self, [quote, u_all_obs]]
			 ],
			 ElseResult45),
	    TrueResult40=ElseResult45
	).
:- set_opv(f_u_cx_c36_true_c63, classof, claz_function),
   set_opv(u_cx_c36_true_c63, compile_as, kw_function),
   set_opv(u_cx_c36_true_c63, function, f_u_cx_c36_true_c63),
   DefunResult=u_cx_c36_true_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:17019 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (if (memq? ob &remove-obs) nil <else>) for above?",
				     1,
				     18423)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:18475 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$hypothesize',
			    [self, retracts, asserts],
			    
			    [ let,
			      [['new-context', ['cx$sprout', self]]],
			      
			      [ yloop,
				[yfor, assertion, in, retracts],
				[ydo, ['cx$retract', 'new-context', assertion]]
			      ],
			      
			      [ yloop,
				[yfor, assertion, in, asserts],
				[ydo, ['cx$assert', 'new-context', assertion]]
			      ],
			      'new-context'
			    ]
			  ]).

% annotating U::CX$HYPOTHESIZE 
wl: lambda_def(defun,
	      u_cx_c36_hypothesize,
	      f_u_cx_c36_hypothesize,
	      [u_self, u_retracts, u_asserts],
	      
	      [ 
		[ let,
		  [[u_new_context, [u_cx_c36_sprout, u_self]]],
		  
		  [ u_yloop,
		    [u_yfor, u_assertion, u_in, u_retracts],
		    [u_ydo, [u_cx_c36_retract, u_new_context, u_assertion]]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_assertion, u_in, u_asserts],
		    [u_ydo, [u_cx_c36_assert, u_new_context, u_assertion]]
		  ],
		  u_new_context
		]
	      ]).


% annotating U::CX$HYPOTHESIZE 
wl: arglist_info(u_cx_c36_hypothesize,
		[u_self, u_retracts, u_asserts],
		[Self_Param, Retracts_Param, Asserts_Param],
		arginfo{ all:[u_self, u_retracts, u_asserts],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_retracts, u_asserts],
			 opt:0,
			 req:[u_self, u_retracts, u_asserts],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$HYPOTHESIZE 
wl: init_args(exact_only, u_cx_c36_hypothesize).


% annotating U::CX$HYPOTHESIZE 
f_u_cx_c36_hypothesize(Self_Param, Retracts_Param, Asserts_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_retracts, Retracts_Param), bv(u_asserts, Asserts_Param)],
	f_u_cx_c36_sprout(Self_Param, New_context_Init),
	LEnv=[[bv(u_new_context, New_context_Init)]|Env],
	f_u_yloop(
		  [ [u_yfor, u_assertion, u_in, u_retracts],
		    [u_ydo, [u_cx_c36_retract, u_new_context, u_assertion]]
		  ],
		  Yloop_Ret),
	f_u_yloop(
		  [ [u_yfor, u_assertion, u_in, u_asserts],
		    [u_ydo, [u_cx_c36_assert, u_new_context, u_assertion]]
		  ],
		  Yloop_Ret25),
	get_var(LEnv, u_new_context, New_context_Get),
	LetResult=New_context_Get,
	LetResult=FnResult.
:- set_opv(f_u_cx_c36_hypothesize, classof, claz_function),
   set_opv(u_cx_c36_hypothesize, compile_as, kw_function),
   set_opv(u_cx_c36_hypothesize, function, f_u_cx_c36_hypothesize),
   DefunResult=u_cx_c36_hypothesize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:18764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$generate',
			    [self],
			    
			    [ format,
			      '*gen-stream*',
			      '$STRING'("~%-----~%Contents (generate) of ~A:~%"),
			      self
			    ],
			    
			    [ yloop,
			      [yfor, ob, in, ['cx$sorted-all-obs', self]],
			      [ydo, [generate, ob, [], self]]
			    ],
			    [format, '*gen-stream*', '$STRING'("~&-----~%")]
			  ]).

% annotating U::CX$GENERATE 
wl: lambda_def(defun,
	      u_cx_c36_generate,
	      f_u_cx_c36_generate,
	      [u_self],
	      
	      [ 
		[ format,
		  u_xx_gen_stream_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\('%'),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(~),
			     #\('%'),
			     #\('C'),
			     #\(o),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(n),
			     #\(t),
			     #\(s),
			     #\(' '),
			     #\('('),
			     #\(g),
			     #\(e),
			     #\(n),
			     #\(e),
			     #\(r),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(')'),
			     #\(' '),
			     #\(o),
			     #\(f),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(:),
			     #\(~),
			     #\('%')
			   ]),
		  u_self
		],
		
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		  [u_ydo, [u_generate, u_ob, [], u_self]]
		],
		
		[ format,
		  u_xx_gen_stream_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(~),
			     #\('%')
			   ])
		]
	      ]).


% annotating U::CX$GENERATE 
wl: arglist_info(u_cx_c36_generate,
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

% annotating U::CX$GENERATE 
wl: init_args(exact_only, u_cx_c36_generate).


% annotating U::CX$GENERATE 
f_u_cx_c36_generate(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	get_var(Env, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get),
	cl_format(
		  [ Xx_gen_stream_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(~),
			       #\('%'),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(~),
			       #\('%'),
			       #\('C'),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(n),
			       #\(t),
			       #\(s),
			       #\(' '),
			       #\('('),
			       #\(g),
			       #\(e),
			       #\(n),
			       #\(e),
			       #\(r),
			       #\(a),
			       #\(t),
			       #\(e),
			       #\(')'),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(:),
			       #\(~),
			       #\('%')
			     ]),
		    Self_Param
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		    [u_ydo, [u_generate, u_ob, [], u_self]]
		  ],
		  Yloop_Ret),
	get_var(Env, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get14),
	cl_format(
		  [ Xx_gen_stream_xx_Get14,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(~),
			       #\(&),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(~),
			       #\('%')
			     ])
		  ],
		  Format_Ret19),
	Format_Ret19=FnResult.
:- set_opv(f_u_cx_c36_generate, classof, claz_function),
   set_opv(u_cx_c36_generate, compile_as, kw_function),
   set_opv(u_cx_c36_generate, function, f_u_cx_c36_generate),
   DefunResult=u_cx_c36_generate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:18983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$print',
			    [self],
			    
			    [ format,
			      '*gate-output*',
			      '$STRING'("~&-----~%Contents of ~A:~%"),
			      self
			    ],
			    
			    [ yloop,
			      [yfor, ob, in, ['cx$sorted-all-obs', self]],
			      
			      [ ydo,
				
				[ progn,
				  
				  [ format,
				    t,
				    '$STRING'("#{~A: "),
				    ['ob$name', ob]
				  ],
				  ['ob$print', ob, '*gate-output*'],
				  [format, t, '$STRING'("}")]
				],
				[newline, '*gate-output*']
			      ]
			    ],
			    [format, '*gate-output*', '$STRING'("-----~%")],
			    []
			  ]).

% annotating U::CX$PRINT 
wl: lambda_def(defun,
	      u_cx_c36_print,
	      f_u_cx_c36_print,
	      [u_self],
	      
	      [ 
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(~),
			     #\('%'),
			     #\('C'),
			     #\(o),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(n),
			     #\(t),
			     #\(s),
			     #\(' '),
			     #\(o),
			     #\(f),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(:),
			     #\(~),
			     #\('%')
			   ]),
		  u_self
		],
		
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		  
		  [ u_ydo,
		    
		    [ progn,
		      
		      [ format,
			t,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(#), #\('{'), #\(~), #\('A'), #\(:), #\(' ')]),
			[u_ob_c36_name, u_ob]
		      ],
		      [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
		      [format, t, '$ARRAY'([*], claz_base_character, [#\('}')])]
		    ],
		    [u_newline, u_xx_gate_output_xx]
		  ]
		],
		
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(-), #\(-), #\(-), #\(-), #\(-), #\(~), #\('%')])
		],
		[]
	      ]).


% annotating U::CX$PRINT 
wl: arglist_info(u_cx_c36_print,
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

% annotating U::CX$PRINT 
wl: init_args(exact_only, u_cx_c36_print).


% annotating U::CX$PRINT 
f_u_cx_c36_print(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(~),
			       #\(&),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(~),
			       #\('%'),
			       #\('C'),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(n),
			       #\(t),
			       #\(s),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(:),
			       #\(~),
			       #\('%')
			     ]),
		    Self_Param
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		    
		    [ u_ydo,
		      
		      [ progn,
			
			[ format,
			  t,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(#), #\('{'), #\(~), #\('A'), #\(:), #\(' ')]),
			  [u_ob_c36_name, u_ob]
			],
			[u_ob_c36_print, u_ob, u_xx_gate_output_xx],
			[format, t, '$ARRAY'([*], claz_base_character, [#\('}')])]
		      ],
		      [u_newline, u_xx_gate_output_xx]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get14),
	cl_format(
		  [ Xx_gate_output_xx_Get14,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(-), #\(-), #\(-), #\(-), #\(-), #\(~), #\('%')])
		  ],
		  Format_Ret19),
	[]=FnResult.
:- set_opv(f_u_cx_c36_print, classof, claz_function),
   set_opv(u_cx_c36_print, compile_as, kw_function),
   set_opv(u_cx_c36_print, function, f_u_cx_c36_print),
   DefunResult=u_cx_c36_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:19292 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$print-actions',
			    [self],
			    
			    [ format,
			      '*gate-output*',
			      '$STRING'("~&-----~%Contents (actions) of ~A:~%"),
			      self
			    ],
			    
			    [ yloop,
			      [yfor, ob, in, ['cx$sorted-all-obs', self]],
			      
			      [ ydo,
				
				[ if,
				  ['ty$instance?', ob, [quote, action]],
				  
				  [ progn,
				    ['ob$print', ob, '*gate-output*'],
				    [newline, '*gate-output*']
				  ]
				]
			      ]
			    ],
			    [format, '*gate-output*', '$STRING'("-----~%")],
			    []
			  ]).

% annotating U::CX$PRINT-ACTIONS 
wl: lambda_def(defun,
	      u_cx_c36_print_actions,
	      f_u_cx_c36_print_actions,
	      [u_self],
	      
	      [ 
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(~),
			     #\('%'),
			     #\('C'),
			     #\(o),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(n),
			     #\(t),
			     #\(s),
			     #\(' '),
			     #\('('),
			     #\(a),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(s),
			     #\(')'),
			     #\(' '),
			     #\(o),
			     #\(f),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(:),
			     #\(~),
			     #\('%')
			   ]),
		  u_self
		],
		
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_ty_c36_instance_c63, u_ob, [quote, u_action]],
		      
		      [ progn,
			[u_ob_c36_print, u_ob, u_xx_gate_output_xx],
			[u_newline, u_xx_gate_output_xx]
		      ]
		    ]
		  ]
		],
		
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(-), #\(-), #\(-), #\(-), #\(-), #\(~), #\('%')])
		],
		[]
	      ]).


% annotating U::CX$PRINT-ACTIONS 
wl: arglist_info(u_cx_c36_print_actions,
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

% annotating U::CX$PRINT-ACTIONS 
wl: init_args(exact_only, u_cx_c36_print_actions).


% annotating U::CX$PRINT-ACTIONS 
f_u_cx_c36_print_actions(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(~),
			       #\(&),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(~),
			       #\('%'),
			       #\('C'),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(n),
			       #\(t),
			       #\(s),
			       #\(' '),
			       #\('('),
			       #\(a),
			       #\(c),
			       #\(t),
			       #\(i),
			       #\(o),
			       #\(n),
			       #\(s),
			       #\(')'),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(:),
			       #\(~),
			       #\('%')
			     ]),
		    Self_Param
		  ],
		  Format_Ret),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_cx_c36_sorted_all_obs, u_self]],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ty_c36_instance_c63, u_ob, [quote, u_action]],
			
			[ progn,
			  [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
			  [u_newline, u_xx_gate_output_xx]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get14),
	cl_format(
		  [ Xx_gate_output_xx_Get14,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(-), #\(-), #\(-), #\(-), #\(-), #\(~), #\('%')])
		  ],
		  Format_Ret19),
	[]=FnResult.
:- set_opv(f_u_cx_c36_print_actions, classof, claz_function),
   set_opv(u_cx_c36_print_actions, compile_as, kw_function),
   set_opv(u_cx_c36_print_actions, function, f_u_cx_c36_print_actions),
   DefunResult=u_cx_c36_print_actions.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:19645 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$print-diffs',
			    [self],
			    
			    [ format,
			      '*gate-output*',
			      '$STRING'("~&-----~%Differential contents of ~A:~%"),
			      self
			    ],
			    
			    [ format,
			      '*gate-output*',
			      '$STRING'("~&Additions:~%")
			    ],
			    
			    [ yloop,
			      [yfor, ob, in, ['ob$get', self, [quote, 'add-obs']]],
			      
			      [ ydo,
				['ob$print', ob, '*gate-output*'],
				[newline, '*gate-output*']
			      ]
			    ],
			    
			    [ format,
			      '*gate-output*',
			      '$STRING'("~&Removals:~%")
			    ],
			    
			    [ yloop,
			      
			      [ yfor,
				ob,
				in,
				['ob$get', self, [quote, 'remove-obs']]
			      ],
			      
			      [ ydo,
				['ob$print', ob, '*gate-output*'],
				[newline, '*gate-output*']
			      ]
			    ],
			    []
			  ]).

% annotating U::CX$PRINT-DIFFS 
wl: lambda_def(defun,
	      u_cx_c36_print_diffs,
	      f_u_cx_c36_print_diffs,
	      [u_self],
	      
	      [ 
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(-),
			     #\(~),
			     #\('%'),
			     #\('D'),
			     #\(i),
			     #\(f),
			     #\(f),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(t),
			     #\(i),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(c),
			     #\(o),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(n),
			     #\(t),
			     #\(s),
			     #\(' '),
			     #\(o),
			     #\(f),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(:),
			     #\(~),
			     #\('%')
			   ]),
		  u_self
		],
		
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\('A'),
			     #\(d),
			     #\(d),
			     #\(i),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(s),
			     #\(:),
			     #\(~),
			     #\('%')
			   ])
		],
		
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_add_obs]]],
		  
		  [ u_ydo,
		    [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
		    [u_newline, u_xx_gate_output_xx]
		  ]
		],
		
		[ format,
		  u_xx_gate_output_xx,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\('R'),
			     #\(e),
			     #\(m),
			     #\(o),
			     #\(v),
			     #\(a),
			     #\(l),
			     #\(s),
			     #\(:),
			     #\(~),
			     #\('%')
			   ])
		],
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    [u_ob_c36_get, u_self, [quote, u_remove_obs]]
		  ],
		  
		  [ u_ydo,
		    [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
		    [u_newline, u_xx_gate_output_xx]
		  ]
		],
		[]
	      ]).


% annotating U::CX$PRINT-DIFFS 
wl: arglist_info(u_cx_c36_print_diffs,
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

% annotating U::CX$PRINT-DIFFS 
wl: init_args(exact_only, u_cx_c36_print_diffs).


% annotating U::CX$PRINT-DIFFS 
f_u_cx_c36_print_diffs(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	cl_format(
		  [ Xx_gate_output_xx_Get,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(~),
			       #\(&),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(~),
			       #\('%'),
			       #\('D'),
			       #\(i),
			       #\(f),
			       #\(f),
			       #\(e),
			       #\(r),
			       #\(e),
			       #\(n),
			       #\(t),
			       #\(i),
			       #\(a),
			       #\(l),
			       #\(' '),
			       #\(c),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(n),
			       #\(t),
			       #\(s),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(:),
			       #\(~),
			       #\('%')
			     ]),
		    Self_Param
		  ],
		  Format_Ret),
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get14),
	cl_format(
		  [ Xx_gate_output_xx_Get14,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(~),
			       #\(&),
			       #\('A'),
			       #\(d),
			       #\(d),
			       #\(i),
			       #\(t),
			       #\(i),
			       #\(o),
			       #\(n),
			       #\(s),
			       #\(:),
			       #\(~),
			       #\('%')
			     ])
		  ],
		  Format_Ret19),
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_add_obs]]],
		    
		    [ u_ydo,
		      [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
		      [u_newline, u_xx_gate_output_xx]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get15),
	cl_format(
		  [ Xx_gate_output_xx_Get15,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(~),
			       #\(&),
			       #\('R'),
			       #\(e),
			       #\(m),
			       #\(o),
			       #\(v),
			       #\(a),
			       #\(l),
			       #\(s),
			       #\(:),
			       #\(~),
			       #\('%')
			     ])
		  ],
		  Format_Ret21),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_remove_obs]]
		    ],
		    
		    [ u_ydo,
		      [u_ob_c36_print, u_ob, u_xx_gate_output_xx],
		      [u_newline, u_xx_gate_output_xx]
		    ]
		  ],
		  Yloop_Ret22),
	[]=FnResult.
:- set_opv(f_u_cx_c36_print_diffs, classof, claz_function),
   set_opv(u_cx_c36_print_diffs, compile_as, kw_function),
   set_opv(u_cx_c36_print_diffs, function, f_u_cx_c36_print_diffs),
   DefunResult=u_cx_c36_print_diffs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20090 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$print-ancestors',
			    [self],
			    
			    [ yloop,
			      
			      [ yfor,
				context,
				in,
				
				[ reverse,
				  
				  [ cons,
				    self,
				    ['ob$get', self, [quote, ancestors]]
				  ]
				]
			      ],
			      [ydo, ['cx$print-diffs', context]]
			    ]
			  ]).

% annotating U::CX$PRINT-ANCESTORS 
wl: lambda_def(defun,
	      u_cx_c36_print_ancestors,
	      f_u_cx_c36_print_ancestors,
	      [u_self],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_yfor,
		    u_context,
		    u_in,
		    
		    [ reverse,
		      [cons, u_self, [u_ob_c36_get, u_self, [quote, u_ancestors]]]
		    ]
		  ],
		  [u_ydo, [u_cx_c36_print_diffs, u_context]]
		]
	      ]).


% annotating U::CX$PRINT-ANCESTORS 
wl: arglist_info(u_cx_c36_print_ancestors,
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

% annotating U::CX$PRINT-ANCESTORS 
wl: init_args(exact_only, u_cx_c36_print_ancestors).


% annotating U::CX$PRINT-ANCESTORS 
f_u_cx_c36_print_ancestors(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_context,
		      u_in,
		      
		      [ reverse,
			
			[ cons,
			  u_self,
			  [u_ob_c36_get, u_self, [quote, u_ancestors]]
			]
		      ]
		    ],
		    [u_ydo, [u_cx_c36_print_diffs, u_context]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_print_ancestors, classof, claz_function),
   set_opv(u_cx_c36_print_ancestors, compile_as, kw_function),
   set_opv(u_cx_c36_print_ancestors, function, f_u_cx_c36_print_ancestors),
   DefunResult=u_cx_c36_print_ancestors.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20240 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$show-descendants',
			    [self],
			    
			    [ yloop,
			      [yfor, ob, in, ['ob$get', self, [quote, children]]],
			      [ydo, ['cx$show-descendants1', ob]]
			    ]
			  ]).

% annotating U::CX$SHOW-DESCENDANTS 
wl: lambda_def(defun,
	      u_cx_c36_show_descendants,
	      f_u_cx_c36_show_descendants,
	      [u_self],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]],
		  [u_ydo, [u_cx_c36_show_descendants1, u_ob]]
		]
	      ]).


% annotating U::CX$SHOW-DESCENDANTS 
wl: arglist_info(u_cx_c36_show_descendants,
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

% annotating U::CX$SHOW-DESCENDANTS 
wl: init_args(exact_only, u_cx_c36_show_descendants).


% annotating U::CX$SHOW-DESCENDANTS 
f_u_cx_c36_show_descendants(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_show_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_show_descendants, classof, claz_function),
   set_opv(u_cx_c36_show_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_show_descendants, function, f_u_cx_c36_show_descendants),
   DefunResult=u_cx_c36_show_descendants.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20364 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$show-descendants1',
			    [self],
			    ['ob$unhide', self],
			    
			    [ yloop,
			      [yfor, ob, in, ['ob$get', self, [quote, children]]],
			      [ydo, ['cx$show-descendants1', ob]]
			    ]
			  ]).

% annotating U::CX$SHOW-DESCENDANTS1 
wl: lambda_def(defun,
	      u_cx_c36_show_descendants1,
	      f_u_cx_c36_show_descendants1,
	      [u_self],
	      
	      [ [u_ob_c36_unhide, u_self],
		
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]],
		  [u_ydo, [u_cx_c36_show_descendants1, u_ob]]
		]
	      ]).


% annotating U::CX$SHOW-DESCENDANTS1 
wl: arglist_info(u_cx_c36_show_descendants1,
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

% annotating U::CX$SHOW-DESCENDANTS1 
wl: init_args(exact_only, u_cx_c36_show_descendants1).


% annotating U::CX$SHOW-DESCENDANTS1 
f_u_cx_c36_show_descendants1(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_ob_c36_unhide(Self_Param, C36_unhide_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_show_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_show_descendants1, classof, claz_function),
   set_opv(u_cx_c36_show_descendants1, compile_as, kw_function),
   set_opv(u_cx_c36_show_descendants1, function, f_u_cx_c36_show_descendants1),
   DefunResult=u_cx_c36_show_descendants1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20508 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$unshow-descendants',
			    [self],
			    
			    [ yloop,
			      [yfor, ob, in, ['ob$get', self, [quote, children]]],
			      [ydo, ['cx$unshow-descendants1', ob]]
			    ]
			  ]).

% annotating U::CX$UNSHOW-DESCENDANTS 
wl: lambda_def(defun,
	      u_cx_c36_unshow_descendants,
	      f_u_cx_c36_unshow_descendants,
	      [u_self],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]],
		  [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]
		]
	      ]).


% annotating U::CX$UNSHOW-DESCENDANTS 
wl: arglist_info(u_cx_c36_unshow_descendants,
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

% annotating U::CX$UNSHOW-DESCENDANTS 
wl: init_args(exact_only, u_cx_c36_unshow_descendants).


% annotating U::CX$UNSHOW-DESCENDANTS 
f_u_cx_c36_unshow_descendants(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_unshow_descendants, classof, claz_function),
   set_opv(u_cx_c36_unshow_descendants, compile_as, kw_function),
   set_opv(u_cx_c36_unshow_descendants, function, f_u_cx_c36_unshow_descendants),
   DefunResult=u_cx_c36_unshow_descendants.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20636 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$unshow-descendants1',
			    [self],
			    ['ob$hide', self],
			    
			    [ yloop,
			      [yfor, ob, in, ['ob$get', self, [quote, children]]],
			      [ydo, ['cx$unshow-descendants1', ob]]
			    ]
			  ]).

% annotating U::CX$UNSHOW-DESCENDANTS1 
wl: lambda_def(defun,
	      u_cx_c36_unshow_descendants1,
	      f_u_cx_c36_unshow_descendants1,
	      [u_self],
	      
	      [ [u_ob_c36_hide, u_self],
		
		[ u_yloop,
		  [u_yfor, u_ob, u_in, [u_ob_c36_get, u_self, [quote, u_children]]],
		  [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]
		]
	      ]).


% annotating U::CX$UNSHOW-DESCENDANTS1 
wl: arglist_info(u_cx_c36_unshow_descendants1,
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

% annotating U::CX$UNSHOW-DESCENDANTS1 
wl: init_args(exact_only, u_cx_c36_unshow_descendants1).


% annotating U::CX$UNSHOW-DESCENDANTS1 
f_u_cx_c36_unshow_descendants1(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_ob_c36_hide(Self_Param, C36_hide_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_ob_c36_get, u_self, [quote, u_children]]
		    ],
		    [u_ydo, [u_cx_c36_unshow_descendants1, u_ob]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_unshow_descendants1, classof, claz_function),
   set_opv(u_cx_c36_unshow_descendants1, compile_as, kw_function),
   set_opv(u_cx_c36_unshow_descendants1,
	   function,
	   f_u_cx_c36_unshow_descendants1),
   DefunResult=u_cx_c36_unshow_descendants1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20636 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 20783)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20636 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Context sensitive links", 1, 20785)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20636 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 20811)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20813 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ob$decl-inverses',
			    [quote, 'linked-to'],
			    [quote, 'linked-to-of']
			  ]).
:- f_u_ob_c36_decl_inverses(u_linked_to, u_linked_to_of, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20857 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ob$decl-inverses',
			    [quote, 'linked-from'],
			    [quote, 'linked-from-of']
			  ]).
:- f_u_ob_c36_decl_inverses(u_linked_from, u_linked_from_of, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:20906 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ol-get',
			    [ob, 'link-type', dir, context],
			    
			    [ let,
			      
			      [ 
				[ links,
				  
				  [ 'ob$gets',
				    ob,
				    
				    [ if,
				      ['eq?', dir, [quote, backward]],
				      [quote, 'linked-to-of'],
				      [quote, 'linked-from-of']
				    ]
				  ]
				],
				
				[ 'other-dir',
				  
				  [ if,
				    ['eq?', dir, [quote, backward]],
				    [quote, 'linked-from'],
				    [quote, 'linked-to']
				  ]
				]
			      ],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, link, in, links],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      ['cx$true?', context, link],
				      ['ty$instance-of?', link, 'link-type']
				    ],
				    
				    [ setq,
				      result,
				      
				      [ append,
					['ob$gets', link, 'other-dir'],
					result
				      ]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::OL-GET 
wl: lambda_def(defun,
	      u_ol_get,
	      f_u_ol_get,
	      [u_ob, u_link_type, ext_dir, u_context],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_links,
		      
		      [ u_ob_c36_gets,
			u_ob,
			
			[ if,
			  [u_eq_c63, ext_dir, [quote, u_backward]],
			  [quote, u_linked_to_of],
			  [quote, u_linked_from_of]
			]
		      ]
		    ],
		    
		    [ u_other_dir,
		      
		      [ if,
			[u_eq_c63, ext_dir, [quote, u_backward]],
			[quote, u_linked_from],
			[quote, u_linked_to]
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			
			[ setq,
			  u_result,
			  [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::OL-GET 
wl: arglist_info(u_ol_get,
		[u_ob, u_link_type, ext_dir, u_context],
		[Ob_Param, Link_type_Param, Ext_dir_Param, Context_Param],
		arginfo{ all:[u_ob, u_link_type, ext_dir, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_link_type, ext_dir, u_context],
			 opt:0,
			 req:[u_ob, u_link_type, ext_dir, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OL-GET 
wl: init_args(exact_only, u_ol_get).


% annotating U::OL-GET 
f_u_ol_get(Ob_Param, Link_type_Param, Ext_dir_Param, Context_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(ext_dir, Ext_dir_Param), bv(u_context, Context_Param)],
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST),
	(   IFTEST\==[]
	->  _122810=u_linked_to_of
	;   _122810=u_linked_from_of
	),
	f_u_ob_c36_gets(Ob_Param, _122810, Links_Init),
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST23),
	(   IFTEST23\==[]
	->  Other_dir_Init=u_linked_from
	;   Other_dir_Init=u_linked_to
	),
	LEnv=[[bv(u_links, Links_Init), bv(u_other_dir, Other_dir_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			
			[ setq,
			  u_result,
			  [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_ol_get, classof, claz_function),
   set_opv(u_ol_get, compile_as, kw_function),
   set_opv(u_ol_get, function, f_u_ol_get),
   DefunResult=u_ol_get.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21491 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ol-set',
			    ['from-ob', 'link-type', 'to-ob', context],
			    
			    [ 'cx$assert',
			      context,
			      
			      [ 'ob$fcreate',
				
				[ '#BQ',
				  
				  [ [quote, ['#COMMA', 'link-type']],
				    'linked-from',
				    ['#COMMA', 'from-ob'],
				    'linked-to',
				    ['#COMMA', 'to-ob']
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OL-SET 
wl: lambda_def(defun,
	      u_ol_set,
	      f_u_ol_set,
	      [u_from_ob, u_link_type, u_to_ob, u_context],
	      
	      [ 
		[ u_cx_c36_assert,
		  u_context,
		  
		  [ u_ob_c36_fcreate,
		    
		    [ '#BQ',
		      
		      [ [quote, ['#COMMA', u_link_type]],
			u_linked_from,
			['#COMMA', u_from_ob],
			u_linked_to,
			['#COMMA', u_to_ob]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OL-SET 
wl: arglist_info(u_ol_set,
		[u_from_ob, u_link_type, u_to_ob, u_context],
		[From_ob_Param, Link_type_Param, To_ob_Param, Context_Param],
		arginfo{ all:[u_from_ob, u_link_type, u_to_ob, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_from_ob, u_link_type, u_to_ob, u_context],
			 opt:0,
			 req:[u_from_ob, u_link_type, u_to_ob, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OL-SET 
wl: init_args(exact_only, u_ol_set).


% annotating U::OL-SET 
f_u_ol_set(From_ob_Param, Link_type_Param, To_ob_Param, Context_Param, FnResult) :-
	Env=[bv(u_from_ob, From_ob_Param), bv(u_link_type, Link_type_Param), bv(u_to_ob, To_ob_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_fcreate(
			   [ '#BQ',
			     
			     [ [quote, ['#COMMA', u_link_type]],
			       u_linked_from,
			       ['#COMMA', u_from_ob],
			       u_linked_to,
			       ['#COMMA', u_to_ob]
			     ]
			   ],
			   C36_fcreate_Ret),
	f_u_cx_c36_assert(Context_Param, C36_fcreate_Ret, C36_assert_Ret),
	C36_assert_Ret=FnResult.
:- set_opv(f_u_ol_set, classof, claz_function),
   set_opv(u_ol_set, compile_as, kw_function),
   set_opv(u_ol_set, function, f_u_ol_set),
   DefunResult=u_ol_set.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21706 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'has-link?',
			    [ob, direction, type, context],
			    
			    [ let,
			      [[links, ['ob$gets', ob, direction]]],
			      
			      [ 'any?',
				
				[ lambda,
				  [x],
				  
				  [ and,
				    ['cx$true?', context, x],
				    ['ty$instance-of?', x, type]
				  ]
				],
				links
			      ]
			    ]
			  ]).

% annotating U::HAS-LINK? 
wl: lambda_def(defun,
	      u_has_link_c63,
	      f_u_has_link_c63,
	      [u_ob, u_direction, type, u_context],
	      
	      [ 
		[ let,
		  [[u_links, [u_ob_c36_gets, u_ob, u_direction]]],
		  
		  [ u_any_c63,
		    
		    [ lambda,
		      [u_x],
		      
		      [ and,
			[u_cx_c36_true_c63, u_context, u_x],
			[u_ty_c36_instance_of_c63, u_x, type]
		      ]
		    ],
		    u_links
		  ]
		]
	      ]).


% annotating U::HAS-LINK? 
wl: arglist_info(u_has_link_c63,
		[u_ob, u_direction, type, u_context],
		[Ob_Param, Direction_Param, Type_Param, Context_Param],
		arginfo{ all:[u_ob, u_direction, type, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_direction, type, u_context],
			 opt:0,
			 req:[u_ob, u_direction, type, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::HAS-LINK? 
wl: init_args(exact_only, u_has_link_c63).


% annotating U::HAS-LINK? 
f_u_has_link_c63(Ob_Param, Direction_Param, Type_Param, Context_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_direction, Direction_Param), bv(type, Type_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_gets(Ob_Param, Direction_Param, Links_Init),
	LEnv=[[bv(u_links, Links_Init)]|Env],
	f_u_any_c63(
		    [ lambda,
		      [u_x],
		      
		      [ and,
			[u_cx_c36_true_c63, u_context, u_x],
			[u_ty_c36_instance_of_c63, u_x, type]
		      ]
		    ],
		    u_links,
		    Links),
	LetResult=Links,
	LetResult=FnResult.
:- set_opv(f_u_has_link_c63, classof, claz_function),
   set_opv(u_has_link_c63, compile_as, kw_function),
   set_opv(u_has_link_c63, function, f_u_has_link_c63),
   DefunResult=u_has_link_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21706 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns forward linked obs (e.g., results)",
				     1,
				     21904)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21948 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-links',
			    [ob, 'link-type', context],
			    
			    [ let,
			      
			      [ 
				[ links,
				  ['ob$gets', ob, [quote, 'linked-from-of']]
				]
			      ],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, link, in, links],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      ['cx$true?', context, link],
				      ['ty$instance-of?', link, 'link-type']
				    ],
				    
				    [ setq,
				      result,
				      ['append!', result, [list, link]]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::GET-LINKS 
wl: lambda_def(defun,
	      u_get_links,
	      f_u_get_links,
	      [u_ob, u_link_type, u_context],
	      
	      [ 
		[ let,
		  [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_from_of]]]],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::GET-LINKS 
wl: arglist_info(u_get_links,
		[u_ob, u_link_type, u_context],
		[Ob_Param, Link_type_Param, Context_Param],
		arginfo{ all:[u_ob, u_link_type, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_link_type, u_context],
			 opt:0,
			 req:[u_ob, u_link_type, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-LINKS 
wl: init_args(exact_only, u_get_links).


% annotating U::GET-LINKS 
f_u_get_links(Ob_Param, Link_type_Param, Context_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_gets(Ob_Param, u_linked_from_of, Links_Init),
	LEnv=[[bv(u_links, Links_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_get_links, classof, claz_function),
   set_opv(u_get_links, compile_as, kw_function),
   set_opv(u_get_links, function, f_u_get_links),
   DefunResult=u_get_links.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:21948 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns backward linked obs (e.g., causes)",
				     1,
				     22306)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:22350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-links-from',
			    [ob, 'link-type', context],
			    
			    [ let,
			      [[links, ['ob$gets', ob, [quote, 'linked-to-of']]]],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, link, in, links],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      ['cx$true?', context, link],
				      ['ty$instance-of?', link, 'link-type']
				    ],
				    
				    [ setq,
				      result,
				      ['append!', result, [list, link]]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::GET-LINKS-FROM 
wl: lambda_def(defun,
	      u_get_links_from,
	      f_u_get_links_from,
	      [u_ob, u_link_type, u_context],
	      
	      [ 
		[ let,
		  [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_to_of]]]],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::GET-LINKS-FROM 
wl: arglist_info(u_get_links_from,
		[u_ob, u_link_type, u_context],
		[Ob_Param, Link_type_Param, Context_Param],
		arginfo{ all:[u_ob, u_link_type, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_link_type, u_context],
			 opt:0,
			 req:[u_ob, u_link_type, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-LINKS-FROM 
wl: init_args(exact_only, u_get_links_from).


% annotating U::GET-LINKS-FROM 
f_u_get_links_from(Ob_Param, Link_type_Param, Context_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_gets(Ob_Param, u_linked_to_of, Links_Init),
	LEnv=[[bv(u_links, Links_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_cx_c36_true_c63, u_context, u_link],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_get_links_from, classof, claz_function),
   set_opv(u_get_links_from, compile_as, kw_function),
   set_opv(u_get_links_from, function, f_u_get_links_from),
   DefunResult=u_get_links_from.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:22710 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ol-path',
			    
			    [ ob1,
			      ob2,
			      'link-type',
			      dir,
			      context,
			      predicate,
			      bindings
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				[result, []],
				[count, 0],
				
				[ 'next-obs',
				  ['ol-get', ob1, 'link-type', dir, context]
				]
			      ],
			      
			      [ yuntil,
				
				[ or,
				  result,
				  
				  [ if,
				    [>, count, '*max-breadth*'],
				    
				    [ progn,
				      
				      [ ndbg,
					'*gate-dbg*',
					'ob-warn',
					'$STRING'("Exceeded max breadth in ob-link-path.~%")
				      ],
				      t
				    ],
				    []
				  ]
				]
			      ],
			      [ywhile, 'next-obs'],
			      
			      [ ydo,
				
				[ yloop,
				  [yfor, 'next-ob', in, 'next-obs'],
				  [yuntil, result],
				  
				  [ ydo,
				    
				    [ setq,
				      result,
				      
				      [ if,
					['procedure?', predicate],
					[funcall, predicate, ob2, 'next-ob'],
					
					[ 'ob$unify-cx',
					  ob2,
					  'next-ob',
					  bindings,
					  context
					]
				      ]
				    ],
				    
				    [ if,
				      result,
				      
				      [ setq,
					result,
					[cons, 'next-ob', [cdr, result]]
				      ]
				    ]
				  ]
				],
				
				[ if,
				  ['null?', result],
				  
				  [ setq,
				    'next-obs',
				    
				    [ 'walk-append',
				      
				      [ lambda,
					[ob],
					['ol-get', ob, 'link-type', dir, context]
				      ],
				      'next-obs'
				    ]
				  ]
				],
				['increment-me', count]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::OL-PATH 
wl: lambda_def(defun,
	      u_ol_path,
	      f_u_ol_path,
	      [u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_result, []],
		    [count, 0],
		    
		    [ u_next_obs,
		      [u_ol_get, u_ob1, u_link_type, ext_dir, u_context]
		    ]
		  ],
		  
		  [ u_yuntil,
		    
		    [ or,
		      u_result,
		      
		      [ if,
			[>, count, u_xx_max_breadth_xx],
			
			[ progn,
			  
			  [ u_ndbg,
			    u_xx_gate_dbg_xx,
			    u_ob_warn,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('E'),
				       #\(x),
				       #\(c),
				       #\(e),
				       #\(e),
				       #\(d),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(m),
				       #\(a),
				       #\(x),
				       #\(' '),
				       #\(b),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(d),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(o),
				       #\(b),
				       #\(-),
				       #\(l),
				       #\(i),
				       #\(n),
				       #\(k),
				       #\(-),
				       #\(p),
				       #\(a),
				       #\(t),
				       #\(h),
				       #\('.'),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  t
			],
			[]
		      ]
		    ]
		  ],
		  [u_ywhile, u_next_obs],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      [u_yfor, u_next_ob, u_in, u_next_obs],
		      [u_yuntil, u_result],
		      
		      [ u_ydo,
			
			[ setq,
			  u_result,
			  
			  [ if,
			    [u_procedure_c63, predicate],
			    [funcall, predicate, u_ob2, u_next_ob],
			    
			    [ u_ob_c36_unify_cx,
			      u_ob2,
			      u_next_ob,
			      bindings,
			      u_context
			    ]
			  ]
			],
			
			[ if,
			  u_result,
			  [setq, u_result, [cons, u_next_ob, [cdr, u_result]]]
			]
		      ]
		    ],
		    
		    [ if,
		      [u_null_c63, u_result],
		      
		      [ setq,
			u_next_obs,
			
			[ u_walk_append,
			  
			  [ lambda,
			    [u_ob],
			    [u_ol_get, u_ob, u_link_type, ext_dir, u_context]
			  ],
			  u_next_obs
			]
		      ]
		    ],
		    [u_increment_me, count]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::OL-PATH 
wl: arglist_info(u_ol_path,
		[u_ob1, u_ob2, u_link_type, ext_dir, u_context, predicate, bindings],
		
		[ Ob1_Param,
		  Ob2_Param,
		  Link_type_Param,
		  Ext_dir_Param,
		  Context_Param,
		  Predicate_Param,
		  Bindings_Param
		],
		arginfo{ all:
			     [ u_ob1,
			       u_ob2,
			       u_link_type,
			       ext_dir,
			       u_context,
			       predicate,
			       bindings
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob1,
				 u_ob2,
				 u_link_type,
				 ext_dir,
				 u_context,
				 predicate,
				 bindings
			       ],
			 opt:0,
			 req:
			     [ u_ob1,
			       u_ob2,
			       u_link_type,
			       ext_dir,
			       u_context,
			       predicate,
			       bindings
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OL-PATH 
wl: init_args(exact_only, u_ol_path).


% annotating U::OL-PATH 
f_u_ol_path(Ob1_Param, Ob2_Param, Link_type_Param, Ext_dir_Param, Context_Param, Predicate_Param, Bindings_Param, FnResult) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param), bv(u_link_type, Link_type_Param), bv(ext_dir, Ext_dir_Param), bv(u_context, Context_Param), bv(predicate, Predicate_Param), bv(bindings, Bindings_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, []],
		      [count, 0],
		      
		      [ u_next_obs,
			[u_ol_get, u_ob1, u_link_type, ext_dir, u_context]
		      ]
		    ],
		    
		    [ u_yuntil,
		      
		      [ or,
			u_result,
			
			[ if,
			  [>, count, u_xx_max_breadth_xx],
			  
			  [ progn,
			    
			    [ u_ndbg,
			      u_xx_gate_dbg_xx,
			      u_ob_warn,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('E'),
					 #\(x),
					 #\(c),
					 #\(e),
					 #\(e),
					 #\(d),
					 #\(e),
					 #\(d),
					 #\(' '),
					 #\(m),
					 #\(a),
					 #\(x),
					 #\(' '),
					 #\(b),
					 #\(r),
					 #\(e),
					 #\(a),
					 #\(d),
					 #\(t),
					 #\(h),
					 #\(' '),
					 #\(i),
					 #\(n),
					 #\(' '),
					 #\(o),
					 #\(b),
					 #\(-),
					 #\(l),
					 #\(i),
					 #\(n),
					 #\(k),
					 #\(-),
					 #\(p),
					 #\(a),
					 #\(t),
					 #\(h),
					 #\('.'),
					 #\(~),
					 #\('%')
				       ])
			    ],
			    t
			  ],
			  []
			]
		      ]
		    ],
		    [u_ywhile, u_next_obs],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_yfor, u_next_ob, u_in, u_next_obs],
			[u_yuntil, u_result],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ if,
			      [u_procedure_c63, predicate],
			      [funcall, predicate, u_ob2, u_next_ob],
			      
			      [ u_ob_c36_unify_cx,
				u_ob2,
				u_next_ob,
				bindings,
				u_context
			      ]
			    ]
			  ],
			  
			  [ if,
			    u_result,
			    [setq, u_result, [cons, u_next_ob, [cdr, u_result]]]
			  ]
			]
		      ],
		      
		      [ if,
			[u_null_c63, u_result],
			
			[ setq,
			  u_next_obs,
			  
			  [ u_walk_append,
			    
			    [ lambda,
			      [u_ob],
			      [u_ol_get, u_ob, u_link_type, ext_dir, u_context]
			    ],
			    u_next_obs
			  ]
			]
		      ],
		      [u_increment_me, count]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ol_path, classof, claz_function),
   set_opv(u_ol_path, compile_as, kw_function),
   set_opv(u_ol_path, function, f_u_ol_path),
   DefunResult=u_ol_path.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_cx.cl:22710 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 23819)).
:- true.


% Total time: 9.864 seconds



% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_reversal" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:13:16 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/26/86: Reversal algorithm", 1, 208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:237 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/24/86: Removed sends", 1, 238)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:262 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/28/86: Rewrote for new undo-causes",
				     1,
				     263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:301 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 302)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:303 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     304)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:384 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 386)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:387 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Failure Reversal", 1, 388)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:406 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 407)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:408 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    reversal,
			    [goal, rev, context, 'top-level-goal', rule, bd],
			    
			    [ let,
			      [['failed-goals', ['ob$gets', rev, [quote, obj]]]],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Reversal for ~A in ~A, top-level-goal = ~A"),
				'failed-goals',
				context,
				'top-level-goal'
			      ],
			      
			      [ if,
				
				[ 'inferred-top-level-goal?',
				  [car, 'failed-goals']
				],
				
				[ 'reverse-undo-causes',
				  'failed-goals',
				  context,
				  'top-level-goal',
				  rule,
				  bd,
				  goal
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::REVERSAL 
wl: lambda_def(defun,
	      u_reversal,
	      f_u_reversal,
	      [u_goal, u_rev, u_context, u_top_level_goal, u_rule, u_bd],
	      
	      [ 
		[ let,
		  [[u_failed_goals, [u_ob_c36_gets, u_rev, [quote, u_obj]]]],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('R'),
			       #\(e),
			       #\(v),
			       #\(e),
			       #\(r),
			       #\(s),
			       #\(a),
			       #\(l),
			       #\(' '),
			       #\(f),
			       #\(o),
			       #\(r),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(','),
			       #\(' '),
			       #\(t),
			       #\(o),
			       #\(p),
			       #\(-),
			       #\(l),
			       #\(e),
			       #\(v),
			       #\(e),
			       #\(l),
			       #\(-),
			       #\(g),
			       #\(o),
			       #\(a),
			       #\(l),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_failed_goals,
		    u_context,
		    u_top_level_goal
		  ],
		  
		  [ if,
		    [u_inferred_top_level_goal_c63, [car, u_failed_goals]],
		    
		    [ u_reverse_undo_causes,
		      u_failed_goals,
		      u_context,
		      u_top_level_goal,
		      u_rule,
		      u_bd,
		      u_goal
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::REVERSAL 
wl: arglist_info(u_reversal,
		[u_goal, u_rev, u_context, u_top_level_goal, u_rule, u_bd],
		
		[ Goal_Param,
		  Rev_Param,
		  Context_Param,
		  Top_level_goal_Param,
		  Rule_Param,
		  Bd_Param
		],
		arginfo{ all:
			     [ u_goal,
			       u_rev,
			       u_context,
			       u_top_level_goal,
			       u_rule,
			       u_bd
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_rev,
				 u_context,
				 u_top_level_goal,
				 u_rule,
				 u_bd
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       u_rev,
			       u_context,
			       u_top_level_goal,
			       u_rule,
			       u_bd
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REVERSAL 
wl: init_args(exact_only, u_reversal).


% annotating U::REVERSAL 
f_u_reversal(Goal_Param, Rev_Param, Context_Param, Top_level_goal_Param, Rule_Param, Bd_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_rev, Rev_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_rule, Rule_Param), bv(u_bd, Bd_Param)],
	f_u_ob_c36_gets(Rev_Param, u_obj, Failed_goals_Init),
	LEnv=[[bv(u_failed_goals, Failed_goals_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(v),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(p),
				       #\(-),
				       #\(l),
				       #\(e),
				       #\(v),
				       #\(e),
				       #\(l),
				       #\(-),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_failed_goals,
			    u_context,
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	get_var(LEnv, u_failed_goals, Failed_goals_Get),
	cl_car(Failed_goals_Get, Goal_c63_Param),
	f_u_inferred_top_level_goal_c63(Goal_c63_Param, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_failed_goals, Failed_goals_Get30),
	    f_u_reverse_undo_causes(Failed_goals_Get30,
				    Context_Param,
				    Top_level_goal_Param,
				    Rule_Param,
				    Bd_Param,
				    Goal_Param,
				    TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_reversal, classof, claz_function),
   set_opv(u_reversal, compile_as, kw_function),
   set_opv(u_reversal, function, f_u_reversal),
   DefunResult=u_reversal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:801 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reverse-alterns',
			    
			    [ 'failed-goal',
			      context,
			      'top-level-goal',
			      rule,
			      bd,
			      goal
			    ],
			    
			    [ let,
			      
			      [ ['sprouted-contexts', []],
				[intends, []],
				
				[ 'old-top-level-goal',
				  
				  [ 'ob$get',
				    'failed-goal',
				    [quote, 'top-level-goal']
				  ]
				],
				['planning-path', []],
				['activation-context', []],
				['termination-context', []]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Reverse alterns for ~A in ~A top = ~A"),
				'failed-goal',
				context,
				'top-level-goal'
			      ],
			      
			      [ setq,
				'activation-context',
				
				[ 'ob$get',
				  'old-top-level-goal',
				  [quote, 'activation-context']
				]
			      ],
			      
			      [ setq,
				'termination-context',
				
				[ 'ob$get',
				  'old-top-level-goal',
				  [quote, 'termination-context']
				]
			      ],
			      
			      [ setq,
				'planning-path',
				
				[ memq,
				  'activation-context',
				  
				  [ reverse,
				    
				    [ cons,
				      'termination-context',
				      ['cx$ancestors', 'termination-context']
				    ]
				  ]
				]
			      ],
			      
			      [ yloop,
				
				[ initial,
				  [rest, 'planning-path'],
				  ['sprouted-context', []]
				],
				[yuntil, ['null?', [cdr, rest]]],
				
				[ ydo,
				  
				  [ yloop,
				    
				    [ yfor,
				      child,
				      in,
				      
				      [ 'prune-possibilities',
					['cx$children', [car, rest]]
				      ]
				    ],
				    
				    [ ydo,
				      
				      [ if,
					['neq?', child, [cadr, rest]],
					
					[ progn,
					  
					  [ 'delay-dbgs',
					    [quote, 'to-be-set'],
					    
					    [ 'rule-fire-msg',
					      rule,
					      '$STRING'("coded plan"),
					      context,
					      bd,
					      [],
					      goal
					    ],
					    
					    [ 'ndbg-roman-nl',
					      '*gate-dbg*',
					      rule,
					      '$STRING'("Reverse alterns")
					    ],
					    
					    [ setq,
					      'sprouted-context',
					      
					      [ 'reversal-sprout-alternative',
						child,
						'old-top-level-goal',
						context,
						'top-level-goal',
						1.0,
						t
					      ]
					    ],
					    
					    [ setq,
					      xxcontext,
					      'sprouted-context'
					    ],
					    
					    [ setq,
					      'sprouted-contexts',
					      
					      [ cons,
						'sprouted-context',
						'sprouted-contexts'
					      ]
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  [setq, rest, [cdr, rest]]
				]
			      ],
			      'sprouted-contexts'
			    ]
			  ]).

% annotating U::REVERSE-ALTERNS 
wl: lambda_def(defun,
	      u_reverse_alterns,
	      f_u_reverse_alterns,
	      [u_failed_goal, u_context, u_top_level_goal, u_rule, u_bd, u_goal],
	      
	      [ 
		[ let,
		  
		  [ [u_sprouted_contexts, []],
		    [u_intends, []],
		    
		    [ u_old_top_level_goal,
		      [u_ob_c36_get, u_failed_goal, [quote, u_top_level_goal]]
		    ],
		    [u_planning_path, []],
		    [u_activation_context, []],
		    [u_termination_context, []]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('R'),
			       #\(e),
			       #\(v),
			       #\(e),
			       #\(r),
			       #\(s),
			       #\(e),
			       #\(' '),
			       #\(a),
			       #\(l),
			       #\(t),
			       #\(e),
			       #\(r),
			       #\(n),
			       #\(s),
			       #\(' '),
			       #\(f),
			       #\(o),
			       #\(r),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(t),
			       #\(o),
			       #\(p),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_failed_goal,
		    u_context,
		    u_top_level_goal
		  ],
		  
		  [ setq,
		    u_activation_context,
		    
		    [ u_ob_c36_get,
		      u_old_top_level_goal,
		      [quote, u_activation_context]
		    ]
		  ],
		  
		  [ setq,
		    u_termination_context,
		    
		    [ u_ob_c36_get,
		      u_old_top_level_goal,
		      [quote, u_termination_context]
		    ]
		  ],
		  
		  [ setq,
		    u_planning_path,
		    
		    [ ext_memq,
		      u_activation_context,
		      
		      [ reverse,
			
			[ cons,
			  u_termination_context,
			  [u_cx_c36_ancestors, u_termination_context]
			]
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [rest, u_planning_path], [u_sprouted_context, []]],
		    [u_yuntil, [u_null_c63, [cdr, rest]]],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_child,
			  u_in,
			  
			  [ u_prune_possibilities,
			    [u_cx_c36_children, [car, rest]]
			  ]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_neq_c63, u_child, [cadr, rest]],
			    
			    [ progn,
			      
			      [ u_delay_dbgs,
				[quote, u_to_be_set],
				
				[ u_rule_fire_msg,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(c),
					     #\(o),
					     #\(d),
					     #\(e),
					     #\(d),
					     #\(' '),
					     #\(p),
					     #\(l),
					     #\(a),
					     #\(n)
					   ]),
				  u_context,
				  u_bd,
				  [],
				  u_goal
				],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('R'),
					     #\(e),
					     #\(v),
					     #\(e),
					     #\(r),
					     #\(s),
					     #\(e),
					     #\(' '),
					     #\(a),
					     #\(l),
					     #\(t),
					     #\(e),
					     #\(r),
					     #\(n),
					     #\(s)
					   ])
				],
				
				[ setq,
				  u_sprouted_context,
				  
				  [ u_reversal_sprout_alternative,
				    u_child,
				    u_old_top_level_goal,
				    u_context,
				    u_top_level_goal,
				    1.0,
				    t
				  ]
				],
				[setq, u_xxcontext, u_sprouted_context],
				
				[ setq,
				  u_sprouted_contexts,
				  
				  [ cons,
				    u_sprouted_context,
				    u_sprouted_contexts
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      [setq, rest, [cdr, rest]]
		    ]
		  ],
		  u_sprouted_contexts
		]
	      ]).


% annotating U::REVERSE-ALTERNS 
wl: arglist_info(u_reverse_alterns,
		[u_failed_goal, u_context, u_top_level_goal, u_rule, u_bd, u_goal],
		
		[ Failed_goal_Param,
		  Context_Param,
		  Top_level_goal_Param,
		  Rule_Param,
		  Bd_Param,
		  Goal_Param
		],
		arginfo{ all:
			     [ u_failed_goal,
			       u_context,
			       u_top_level_goal,
			       u_rule,
			       u_bd,
			       u_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_failed_goal,
				 u_context,
				 u_top_level_goal,
				 u_rule,
				 u_bd,
				 u_goal
			       ],
			 opt:0,
			 req:
			     [ u_failed_goal,
			       u_context,
			       u_top_level_goal,
			       u_rule,
			       u_bd,
			       u_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REVERSE-ALTERNS 
wl: init_args(exact_only, u_reverse_alterns).


% annotating U::REVERSE-ALTERNS 
f_u_reverse_alterns(Failed_goal_Param, Context_Param, Top_level_goal_Param, Rule_Param, Bd_Param, Goal_Param, FnResult) :-
	Env=[bv(u_failed_goal, Failed_goal_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_rule, Rule_Param), bv(u_bd, Bd_Param), bv(u_goal, Goal_Param)],
	f_u_ob_c36_get(Failed_goal_Param,
		       u_top_level_goal,
		       Old_top_level_goal_Init),
	LEnv=[[bv(u_sprouted_contexts, []), bv(u_intends, []), bv(u_old_top_level_goal, Old_top_level_goal_Init), bv(u_planning_path, []), bv(u_activation_context, []), bv(u_termination_context, [])]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(v),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(e),
				       #\(' '),
				       #\(a),
				       #\(l),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(p),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_failed_goal,
			    u_context,
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	get_var(LEnv, u_old_top_level_goal, Old_top_level_goal_Get),
	f_u_ob_c36_get(Old_top_level_goal_Get,
		       u_activation_context,
		       Activation_context),
	set_var(LEnv, u_activation_context, Activation_context),
	get_var(LEnv, u_old_top_level_goal, Old_top_level_goal_Get28),
	f_u_ob_c36_get(Old_top_level_goal_Get28,
		       u_termination_context,
		       Termination_context),
	set_var(LEnv, u_termination_context, Termination_context),
	f_ext_memq(u_activation_context,
		   
		   [ reverse,
		     
		     [ cons,
		       u_termination_context,
		       [u_cx_c36_ancestors, u_termination_context]
		     ]
		   ],
		   Planning_path),
	set_var(LEnv, u_planning_path, Planning_path),
	f_u_yloop(
		  [ [u_initial, [rest, u_planning_path], [u_sprouted_context, []]],
		    [u_yuntil, [u_null_c63, [cdr, rest]]],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_child,
			  u_in,
			  
			  [ u_prune_possibilities,
			    [u_cx_c36_children, [car, rest]]
			  ]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_neq_c63, u_child, [cadr, rest]],
			    
			    [ progn,
			      
			      [ u_delay_dbgs,
				[quote, u_to_be_set],
				
				[ u_rule_fire_msg,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(c),
					     #\(o),
					     #\(d),
					     #\(e),
					     #\(d),
					     #\(' '),
					     #\(p),
					     #\(l),
					     #\(a),
					     #\(n)
					   ]),
				  u_context,
				  u_bd,
				  [],
				  u_goal
				],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('R'),
					     #\(e),
					     #\(v),
					     #\(e),
					     #\(r),
					     #\(s),
					     #\(e),
					     #\(' '),
					     #\(a),
					     #\(l),
					     #\(t),
					     #\(e),
					     #\(r),
					     #\(n),
					     #\(s)
					   ])
				],
				
				[ setq,
				  u_sprouted_context,
				  
				  [ u_reversal_sprout_alternative,
				    u_child,
				    u_old_top_level_goal,
				    u_context,
				    u_top_level_goal,
				    1.0,
				    t
				  ]
				],
				[setq, u_xxcontext, u_sprouted_context],
				
				[ setq,
				  u_sprouted_contexts,
				  
				  [ cons,
				    u_sprouted_context,
				    u_sprouted_contexts
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      [setq, rest, [cdr, rest]]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_sprouted_contexts, Sprouted_contexts_Get),
	LetResult=Sprouted_contexts_Get,
	LetResult=FnResult.
:- set_opv(f_u_reverse_alterns, classof, claz_function),
   set_opv(u_reverse_alterns, compile_as, kw_function),
   set_opv(u_reverse_alterns, function, f_u_reverse_alterns),
   DefunResult=u_reverse_alterns.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reversal-sprout-alternative',
			    
			    [ 'old-context',
			      'old-top-level-goal',
			      'new-context',
			      'new-top-level-goal',
			      ordering,
			      'do-intends?'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Reversal sprout alternative")
			    ],
			    
			    [ let,
			      
			      [ 
				[ result,
				  
				  [ 'sprout-alternative-past',
				    'old-context',
				    'old-top-level-goal',
				    'new-context',
				    'new-top-level-goal'
				  ]
				],
				['sprouted-context', []]
			      ],
			      [setq, 'sprouted-context', [car, result]],
			      [setq, 'old-top-level-goal', [cadr, result]],
			      ['set-ordering', 'sprouted-context', ordering],
			      
			      [ 'no-gen',
				
				[ 'cx$assert',
				  'sprouted-context',
				  'new-top-level-goal'
				]
			      ],
			      
			      [ if,
				'do-intends?',
				
				[ 'cx$assert',
				  'sprouted-context',
				  
				  [ 'ob$fcreate',
				    
				    [ '#BQ',
				      
				      [ 'INTENDS',
					'linked-from',
					['#COMMA', 'new-top-level-goal'],
					'linked-to',
					['#COMMA', 'old-top-level-goal'],
					rule,
					'Reversal-Plan'
				      ]
				    ]
				  ]
				]
			      ],
			      'sprouted-context'
			    ]
			  ]).

% annotating U::REVERSAL-SPROUT-ALTERNATIVE 
wl: lambda_def(defun,
	      u_reversal_sprout_alternative,
	      f_u_reversal_sprout_alternative,
	      
	      [ u_old_context,
		u_old_top_level_goal,
		u_new_context,
		u_new_top_level_goal,
		u_ordering,
		u_do_intends_c63
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(e),
			     #\(v),
			     #\(e),
			     #\(r),
			     #\(s),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(s),
			     #\(p),
			     #\(r),
			     #\(o),
			     #\(u),
			     #\(t),
			     #\(' '),
			     #\(a),
			     #\(l),
			     #\(t),
			     #\(e),
			     #\(r),
			     #\(n),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(v),
			     #\(e)
			   ])
		],
		
		[ let,
		  
		  [ 
		    [ u_result,
		      
		      [ u_sprout_alternative_past,
			u_old_context,
			u_old_top_level_goal,
			u_new_context,
			u_new_top_level_goal
		      ]
		    ],
		    [u_sprouted_context, []]
		  ],
		  [setq, u_sprouted_context, [car, u_result]],
		  [setq, u_old_top_level_goal, [cadr, u_result]],
		  [u_set_ordering, u_sprouted_context, u_ordering],
		  
		  [ u_no_gen,
		    [u_cx_c36_assert, u_sprouted_context, u_new_top_level_goal]
		  ],
		  
		  [ if,
		    u_do_intends_c63,
		    
		    [ u_cx_c36_assert,
		      u_sprouted_context,
		      
		      [ u_ob_c36_fcreate,
			
			[ '#BQ',
			  
			  [ u_intends,
			    u_linked_from,
			    ['#COMMA', u_new_top_level_goal],
			    u_linked_to,
			    ['#COMMA', u_old_top_level_goal],
			    u_rule,
			    u_reversal_plan
			  ]
			]
		      ]
		    ]
		  ],
		  u_sprouted_context
		]
	      ]).


% annotating U::REVERSAL-SPROUT-ALTERNATIVE 
wl: arglist_info(u_reversal_sprout_alternative,
		
		[ u_old_context,
		  u_old_top_level_goal,
		  u_new_context,
		  u_new_top_level_goal,
		  u_ordering,
		  u_do_intends_c63
		],
		
		[ Old_context_Param,
		  Old_top_level_goal_Param,
		  New_context_Param,
		  New_top_level_goal_Param,
		  Ordering_Param,
		  Do_intends_c63_Param
		],
		arginfo{ all:
			     [ u_old_context,
			       u_old_top_level_goal,
			       u_new_context,
			       u_new_top_level_goal,
			       u_ordering,
			       u_do_intends_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_old_context,
				 u_old_top_level_goal,
				 u_new_context,
				 u_new_top_level_goal,
				 u_ordering,
				 u_do_intends_c63
			       ],
			 opt:0,
			 req:
			     [ u_old_context,
			       u_old_top_level_goal,
			       u_new_context,
			       u_new_top_level_goal,
			       u_ordering,
			       u_do_intends_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REVERSAL-SPROUT-ALTERNATIVE 
wl: init_args(exact_only, u_reversal_sprout_alternative).


% annotating U::REVERSAL-SPROUT-ALTERNATIVE 
f_u_reversal_sprout_alternative(Old_context_Param, Old_top_level_goal_Param, New_context_Param, New_top_level_goal_Param, Ordering_Param, Do_intends_c63_Param, Sprouted_context_Get37) :-
	Env=[bv(u_old_context, Old_context_Param), bv(u_old_top_level_goal, Old_top_level_goal_Param), bv(u_new_context, New_context_Param), bv(u_new_top_level_goal, New_top_level_goal_Param), bv(u_ordering, Ordering_Param), bv(u_do_intends_c63, Do_intends_c63_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(v),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(s),
				       #\(p),
				       #\(r),
				       #\(o),
				       #\(u),
				       #\(t),
				       #\(' '),
				       #\(a),
				       #\(l),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(n),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(v),
				       #\(e)
				     ])
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_old_top_level_goal, Old_top_level_goal_Get),
	f_u_sprout_alternative_past(Old_context_Param,
				    Old_top_level_goal_Get,
				    New_context_Param,
				    New_top_level_goal_Param,
				    Result_Init),
	LEnv=[[bv(u_result, Result_Init), bv(u_sprouted_context, [])]|Env],
	get_var(LEnv, u_result, Result_Get),
	cl_car(Result_Get, Sprouted_context),
	set_var(LEnv, u_sprouted_context, Sprouted_context),
	get_var(LEnv, u_result, Result_Get31),
	cl_cadr(Result_Get31, Old_top_level_goal),
	set_var(LEnv, u_old_top_level_goal, Old_top_level_goal),
	get_var(LEnv, u_sprouted_context, Sprouted_context_Get37),
	f_u_set_ordering(Sprouted_context_Get37,
			 Ordering_Param,
			 Set_ordering_Ret),
	f_u_no_gen([[u_cx_c36_assert, u_sprouted_context, u_new_top_level_goal]],
		   No_gen_Ret),
	(   Do_intends_c63_Param\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_intends,
				   u_linked_from,
				   ['#COMMA', u_new_top_level_goal],
				   u_linked_to,
				   ['#COMMA', u_old_top_level_goal],
				   u_rule,
				   u_reversal_plan
				 ]
			       ],
			       C36_fcreate_Ret),
	    f_u_cx_c36_assert(Sprouted_context_Get37,
			      C36_fcreate_Ret,
			      TrueResult),
	    _90188=TrueResult
	;   _90188=[]
	).
:- set_opv(f_u_reversal_sprout_alternative, classof, claz_function),
   set_opv(u_reversal_sprout_alternative, compile_as, kw_function),
   set_opv(u_reversal_sprout_alternative,
	   function,
	   f_u_reversal_sprout_alternative),
   DefunResult=u_reversal_sprout_alternative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Sprout an alternative past context",
				     3,
				     2760)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Bring in all emotions?", 8, 3185)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" I don't think this is needed. Emotions are always in *reality*.",
				     8,
				     3217)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (add-emotions sprouted-context new-context)",
				     8,
				     3290)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Bring in the top-level goal (Would be done by above, if above",
				     8,
				     3343)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" were done).", 8, 3414)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 8, 3499)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Make old top-level-goal actually be a subgoal of the",
				     8,
				     3508)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" current REVERSAL top-level-goal.",
				     8,
				     3570)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Left over task slots associated with old top-level goal",
				     8,
				     3612)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" shouldn't make any difference (except maybe for clarity",
				     8,
				     3677)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:2563 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" in debugging). We can clear them here if we want.",
				     8,
				     3742)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4099 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*reverse-leaf-thresh*', 0.5]).
:- set_var(TLEnv3, setq, u_xx_reverse_leaf_thresh_xx, 0.5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4099 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Ordering of sprouted-contexts is in inverse proportion to",
				     1,
				     4134)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4099 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the realities of the leafs from which those contexts were",
				     1,
				     4194)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4099 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" derived. Lower reality assumptions are better candidates",
				     1,
				     4254)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4099 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" for replanning.", 1, 4313)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reverse-leafs',
			    
			    [ 'old-top-level-goal',
			      context,
			      'top-level-goal',
			      rule,
			      bd,
			      goal
			    ],
			    
			    [ let,
			      
			      [ ['sprouted-contexts', []],
				['sprouted-context', []],
				[intends, []],
				['old-context', []],
				
				[ leafs,
				  
				  [ 'get-leafs',
				    'old-top-level-goal',
				    '*intends-ob*',
				    [quote, forward],
				    
				    [ 'ob$get',
				      'old-top-level-goal',
				      [quote, 'termination-context']
				    ]
				  ]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Reverse leafs for ~A in ~A top = ~A"),
				'old-top-level-goal',
				context,
				'top-level-goal'
			      ],
			      
			      [ yloop,
				[yfor, leaf, in, leafs],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'fl<',
				      [strength, ['ob$get', leaf, [quote, obj]]],
				      '*reverse-leaf-thresh*'
				    ],
				    
				    [ progn,
				      
				      [ setq,
					'old-context',
					
					[ 'ob$get',
					  leaf,
					  [quote, 'activation-context']
					]
				      ],
				      
				      [ 'delay-dbgs',
					[quote, 'to-be-set'],
					
					[ 'rule-fire-msg',
					  rule,
					  '$STRING'("coded plan"),
					  context,
					  bd,
					  [],
					  goal
					],
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("Reverse leafs")
					],
					
					[ setq,
					  'sprouted-context',
					  
					  [ 'reversal-sprout-alternative',
					    'old-context',
					    'old-top-level-goal',
					    context,
					    'top-level-goal',
					    
					    [ 'fl/',
					      1.0,
					      
					      [ strength,
						['ob$get', leaf, [quote, obj]]
					      ]
					    ],
					    t
					  ]
					],
					[setq, xxcontext, 'sprouted-context']
				      ],
				      
				      [ setq,
					'sprouted-contexts',
					
					[ cons,
					  'sprouted-context',
					  'sprouted-contexts'
					]
				      ],
				      
				      [ 'cx$retract',
					'sprouted-context',
					['ob$get', leaf, [quote, obj]]
				      ]
				    ]
				  ]
				]
			      ],
			      'sprouted-contexts'
			    ]
			  ]).

% annotating U::REVERSE-LEAFS 
wl: lambda_def(defun,
	      u_reverse_leafs,
	      f_u_reverse_leafs,
	      
	      [ u_old_top_level_goal,
		u_context,
		u_top_level_goal,
		u_rule,
		u_bd,
		u_goal
	      ],
	      
	      [ 
		[ let,
		  
		  [ [u_sprouted_contexts, []],
		    [u_sprouted_context, []],
		    [u_intends, []],
		    [u_old_context, []],
		    
		    [ u_leafs,
		      
		      [ u_get_leafs,
			u_old_top_level_goal,
			u_xx_intends_ob_xx,
			[quote, u_forward],
			
			[ u_ob_c36_get,
			  u_old_top_level_goal,
			  [quote, u_termination_context]
			]
		      ]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('R'),
			       #\(e),
			       #\(v),
			       #\(e),
			       #\(r),
			       #\(s),
			       #\(e),
			       #\(' '),
			       #\(l),
			       #\(e),
			       #\(a),
			       #\(f),
			       #\(s),
			       #\(' '),
			       #\(f),
			       #\(o),
			       #\(r),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(t),
			       #\(o),
			       #\(p),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_old_top_level_goal,
		    u_context,
		    u_top_level_goal
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_leaf, u_in, u_leafs],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_fl_c60,
			  [u_strength, [u_ob_c36_get, u_leaf, [quote, u_obj]]],
			  u_xx_reverse_leaf_thresh_xx
			],
			
			[ progn,
			  
			  [ setq,
			    u_old_context,
			    [u_ob_c36_get, u_leaf, [quote, u_activation_context]]
			  ],
			  
			  [ u_delay_dbgs,
			    [quote, u_to_be_set],
			    
			    [ u_rule_fire_msg,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(c),
					 #\(o),
					 #\(d),
					 #\(e),
					 #\(d),
					 #\(' '),
					 #\(p),
					 #\(l),
					 #\(a),
					 #\(n)
				       ]),
			      u_context,
			      u_bd,
			      [],
			      u_goal
			    ],
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('R'),
					 #\(e),
					 #\(v),
					 #\(e),
					 #\(r),
					 #\(s),
					 #\(e),
					 #\(' '),
					 #\(l),
					 #\(e),
					 #\(a),
					 #\(f),
					 #\(s)
				       ])
			    ],
			    
			    [ setq,
			      u_sprouted_context,
			      
			      [ u_reversal_sprout_alternative,
				u_old_context,
				u_old_top_level_goal,
				u_context,
				u_top_level_goal,
				
				[ u_fl_c47,
				  1.0,
				  
				  [ u_strength,
				    [u_ob_c36_get, u_leaf, [quote, u_obj]]
				  ]
				],
				t
			      ]
			    ],
			    [setq, u_xxcontext, u_sprouted_context]
			  ],
			  
			  [ setq,
			    u_sprouted_contexts,
			    [cons, u_sprouted_context, u_sprouted_contexts]
			  ],
			  
			  [ u_cx_c36_retract,
			    u_sprouted_context,
			    [u_ob_c36_get, u_leaf, [quote, u_obj]]
			  ]
			]
		      ]
		    ]
		  ],
		  u_sprouted_contexts
		]
	      ]).


% annotating U::REVERSE-LEAFS 
wl: arglist_info(u_reverse_leafs,
		
		[ u_old_top_level_goal,
		  u_context,
		  u_top_level_goal,
		  u_rule,
		  u_bd,
		  u_goal
		],
		
		[ Old_top_level_goal_Param,
		  Context_Param,
		  Top_level_goal_Param,
		  Rule_Param,
		  Bd_Param,
		  Goal_Param
		],
		arginfo{ all:
			     [ u_old_top_level_goal,
			       u_context,
			       u_top_level_goal,
			       u_rule,
			       u_bd,
			       u_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_old_top_level_goal,
				 u_context,
				 u_top_level_goal,
				 u_rule,
				 u_bd,
				 u_goal
			       ],
			 opt:0,
			 req:
			     [ u_old_top_level_goal,
			       u_context,
			       u_top_level_goal,
			       u_rule,
			       u_bd,
			       u_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REVERSE-LEAFS 
wl: init_args(exact_only, u_reverse_leafs).


% annotating U::REVERSE-LEAFS 
f_u_reverse_leafs(Old_top_level_goal_Param, Context_Param, Top_level_goal_Param, Rule_Param, Bd_Param, Goal_Param, FnResult) :-
	Env=[bv(u_old_top_level_goal, Old_top_level_goal_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_rule, Rule_Param), bv(u_bd, Bd_Param), bv(u_goal, Goal_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_ob_c36_get(Old_top_level_goal_Param,
		       u_termination_context,
		       Termination_context),
	f_u_get_leafs(Old_top_level_goal_Param,
		      Xx_intends_ob_xx_Get,
		      u_forward,
		      Termination_context,
		      Leafs_Init),
	LEnv=[[bv(u_sprouted_contexts, []), bv(u_sprouted_context, []), bv(u_intends, []), bv(u_old_context, []), bv(u_leafs, Leafs_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(v),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(e),
				       #\(' '),
				       #\(l),
				       #\(e),
				       #\(a),
				       #\(f),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(p),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_old_top_level_goal,
			    u_context,
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_yfor, u_leaf, u_in, u_leafs],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_fl_c60,
			  [u_strength, [u_ob_c36_get, u_leaf, [quote, u_obj]]],
			  u_xx_reverse_leaf_thresh_xx
			],
			
			[ progn,
			  
			  [ setq,
			    u_old_context,
			    [u_ob_c36_get, u_leaf, [quote, u_activation_context]]
			  ],
			  
			  [ u_delay_dbgs,
			    [quote, u_to_be_set],
			    
			    [ u_rule_fire_msg,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(c),
					 #\(o),
					 #\(d),
					 #\(e),
					 #\(d),
					 #\(' '),
					 #\(p),
					 #\(l),
					 #\(a),
					 #\(n)
				       ]),
			      u_context,
			      u_bd,
			      [],
			      u_goal
			    ],
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('R'),
					 #\(e),
					 #\(v),
					 #\(e),
					 #\(r),
					 #\(s),
					 #\(e),
					 #\(' '),
					 #\(l),
					 #\(e),
					 #\(a),
					 #\(f),
					 #\(s)
				       ])
			    ],
			    
			    [ setq,
			      u_sprouted_context,
			      
			      [ u_reversal_sprout_alternative,
				u_old_context,
				u_old_top_level_goal,
				u_context,
				u_top_level_goal,
				
				[ u_fl_c47,
				  1.0,
				  
				  [ u_strength,
				    [u_ob_c36_get, u_leaf, [quote, u_obj]]
				  ]
				],
				t
			      ]
			    ],
			    [setq, u_xxcontext, u_sprouted_context]
			  ],
			  
			  [ setq,
			    u_sprouted_contexts,
			    [cons, u_sprouted_context, u_sprouted_contexts]
			  ],
			  
			  [ u_cx_c36_retract,
			    u_sprouted_context,
			    [u_ob_c36_get, u_leaf, [quote, u_obj]]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_sprouted_contexts, Sprouted_contexts_Get),
	LetResult=Sprouted_contexts_Get,
	LetResult=FnResult.
:- set_opv(f_u_reverse_leafs, classof, claz_function),
   set_opv(u_reverse_leafs, compile_as, kw_function),
   set_opv(u_reverse_leafs, function, f_u_reverse_leafs),
   DefunResult=u_reverse_leafs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Sprout an alternative past context",
				     18,
				     5060)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Retract the leaf objective (so we have to plan for",
				     18,
				     5960)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" it instead of shakily assuming its truth)",
				     18,
				     6030)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4330 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6172)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4330 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" REVERSE-UNDO-CAUSES:", 1, 6174)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:4330 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:6199 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*next-prule-number*', 1]).
:- set_var(TLEnv3, setq, u_xx_next_prule_number_xx, 1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:6229 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*new-personal-goals*', []]).
:- set_var(TLEnv3, setq, u_xx_new_personal_goals_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:6229 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\nMoved to dd_reversal2.cl\n\n; Treating multiple failed-goals required a cross product of the\n; negated leaf causes with which I am not prepared to deal.\n(defun reverse-undo-causes \n  (failed-goals context top-level-goal rule bd goal)\n (let ((sprouted-contexts nil) (sprouted-context nil)\n        (intends nil)\n       (leaf-causes (get-leaf-causes (car failed-goals)\n                                     (ob$get (car failed-goals)\n                                             'termination-context)))\n       (failed-goal-obj (ob$get (car failed-goals) 'obj))\n       (old-top-level-goal (ob$get (car failed-goals) 'top-level-goal))\n       (backwards-planning-path nil) (old-context nil) (rand nil)\n       (new-rule nil)\n       (uor-obj nil) (predictor nil) (p-goal-uid nil) (input-states nil)\n       (cfg-term-ctxt (ob$get (car failed-goals) 'termination-context))\n       (path nil) (prev-context nil))\n  (ndbg-roman-nl *gate-dbg* rule \"Reverse undo causes for ~A in ~A top = ~A\"\n                 (car failed-goals) context top-level-goal)\n  (setq old-context (ob$get old-top-level-goal 'activation-context))\n  (setq backwards-planning-path\n       (reverse (memq old-context\n                      (reverse (cons cfg-term-ctxt\n                                     (cx$ancestors cfg-term-ctxt))))))\n  (if (null? backwards-planning-path)\n      (error \"Null backwards planning path.\"))\n  (ndbg-roman-nl *gate-dbg* rule \"Bckwds plng path = ~A\"\n                 backwards-planning-path)\n  (yloop\n   (yfor leaf-cause in leaf-causes)\n   (ydo\n    (ndbg-roman-nl *gate-dbg* rule \"Considering leaf cause ~A\" leaf-cause)\n    (if (ty$instance? leaf-cause 'not)\n     (progn\n      (if (ty$instance? (ob$get leaf-cause 'obj) 'long-term-state)\n          (progn\n           (setq *new-personal-goals*\n                (cons (ob$get leaf-cause 'obj) *new-personal-goals*))\n       (activate-top-level-goal\n        (ob$fcreate `(ACTIVE-GOAL obj ,(ob$get leaf-cause 'obj)))\n        *reality-lookahead* *empty-bd*\n        (ob$fcreate `(RULE emotion (POS-EMOTION strength ,(strength goal) )))))\n       (progn\n        ; set up rand object\n        (setq rand (ob$fcreate '(RAND)))\n        (yloop\n         (yfor leaf-cause1 in leaf-causes)\n         (ydo\n          (if (neq? leaf-cause1 leaf-cause)\n           (progn\n            (setq uor-obj (ob$fcreate '(ROR))) ; was UOR, but gets killed by vblz\n            (ob$add uor-obj 'obj leaf-cause1)\n            (ob$add uor-obj 'obj (ob$fcreate `(ACTIVE-GOAL obj ,leaf-cause1)))\n            (setq predictor (predicting-state leaf-cause1))\n            (if predictor\n             (progn\n              (ob$add uor-obj 'obj predictor)\n              (ob$add uor-obj 'obj (ob$fcreate `(ACTIVE-GOAL obj ,predictor)))))\n            (ob$add rand 'obj uor-obj)))))\n        ; set up rules\n        (setq p-goal-uid\n             (string->symbol (string-append \"PRESERVATION\"\n                                            (fixnum->string\n                                             *next-prule-number*))))\n        (setq new-rule\n         (ob$fcreate `(RULE subgoal ,rand\n                            goal (ACTIVE-GOAL\n                                  obj (PRESERVATION obj ,failed-goal-obj\n                                                    uid ',p-goal-uid))\n                            is 'inference-only\n                            plausibility 0.9)))\n        (setq new-rule\n             (ob$variabilize new-rule #'varize-object? nil *link-slots* nil))\n        (ob$add-unique-name new-rule\n                            (string->symbol\n                             (string-append \"PRESERVATION-INF.\"\n                                            (fixnum->string\n                                             *next-prule-number*))))\n        (add-rule-print new-rule)\n        (setq new-rule\n         (ob$fcreate `(RULE subgoal ,(ob$get leaf-cause 'obj)\n                            goal (PRESERVATION obj ,failed-goal-obj\n                                               uid ',p-goal-uid)\n                            is 'plan-only\n                            plausibility\nMoved to dd_reversal2.cl\n\n; Treating multiple failed-goals required a cross product of the\n; negated leaf causes with which I am not prepared to deal.\n(defun reverse-undo-causes \n  (failed-goals context top-level-goal rule bd goal)\n (let ((sprouted-contexts nil) (sprouted-context nil)\n        (intends nil)\n       (leaf-causes (get-leaf-causes (car failed-goals)\n                                     (ob$get (car failed-goals)\n                                             'termination-context)))\n       (failed-goal-obj (ob$get (car failed-goals) 'obj))\n       (old-top-level-goal (ob$get (car failed-goals) 'top-level-goal))\n       (backwards-planning-path nil) (old-context nil) (rand nil)\n       (new-rule nil)\n       (uor-obj nil) (predictor nil) (p-goal-uid nil) (input-states nil)\n       (cfg-term-ctxt (ob$get (car failed-goals) 'termination-context))\n       (path nil) (prev-context nil))\n  (ndbg-roman-nl *gate-dbg* rule \"Reverse undo causes for ~A in ~A top = ~A\"\n                 (car failed-goals) context top-level-goal)\n  (setq old-context (ob$get old-top-level-goal 'activation-context))\n  (setq backwards-planning-path\n       (reverse (memq old-context\n                      (reverse (cons cfg-term-ctxt\n                                     (cx$ancestors cfg-term-ctxt))))))\n  (if (null? backwards-planning-path)\n      (error \"Null backwards planning path.\"))\n  (ndbg-roman-nl *gate-dbg* rule \"Bckwds plng path = ~A\"\n                 backwards-planning-path)\n  (yloop\n   (yfor leaf-cause in leaf-causes)\n   (ydo\n    (ndbg-roman-nl *gate-dbg* rule \"Considering leaf cause ~A\" leaf-cause)\n    (if (ty$instance? leaf-cause 'not)\n     (progn\n      (if (ty$instance? (ob$get leaf-cause 'obj) 'long-term-state)\n          (progn\n           (setq *new-personal-goals*\n                (cons (ob$get leaf-cause 'obj) *new-personal-goals*))\n       (activate-top-level-goal\n        (ob$fcreate `(ACTIVE-GOAL obj ,(ob$get leaf-cause 'obj)))\n        *reality-lookahead* *empty-bd*\n        (ob$fcreate `(RULE emotion (POS-EMOTION strength ,(strength goal) )))))\n       (progn\n        ; set up rand object\n        (setq rand (ob$fcreate '(RAND)))\n        (yloop\n         (yfor leaf-cause1 in leaf-causes)\n         (ydo\n          (if (neq? leaf-cause1 leaf-cause)\n           (progn\n            (setq uor-obj (ob$fcreate '(ROR))) ; was UOR, but gets killed by vblz\n            (ob$add uor-obj 'obj leaf-cause1)\n            (ob$add uor-obj 'obj (ob$fcreate `(ACTIVE-GOAL obj ,leaf-cause1)))\n            (setq predictor (predicting-state leaf-cause1))\n            (if predictor\n             (progn\n              (ob$add uor-obj 'obj predictor)\n              (ob$add uor-obj 'obj (ob$fcreate `(ACTIVE-GOAL obj ,predictor)))))\n            (ob$add rand 'obj uor-obj)))))\n        ; set up rules\n        (setq p-goal-uid\n             (string->symbol (string-append \"PRESERVATION\"\n                                            (fixnum->string\n                                             *next-prule-number*))))\n        (setq new-rule\n         (ob$fcreate `(RULE subgoal ,rand\n                            goal (ACTIVE-GOAL\n                                  obj (PRESERVATION obj ,failed-goal-obj\n                                                    uid ',p-goal-uid))\n                            is 'inference-only\n                            plausibility 0.9)))\n        (setq new-rule\n             (ob$variabilize new-rule #'varize-object? nil *link-slots* nil))\n        (ob$add-unique-name new-rule\n                            (string->symbol\n                             (string-append \"PRESERVATION-INF.\"\n                                            (fixnum->string\n                                             *next-prule-number*))))\n        (add-rule-print new-rule)\n        (setq new-rule\n         (ob$fcreate `(RULE subgoal ,(ob$get leaf-cause 'obj)\n                            goal (PRESERVATION obj ,failed-goal-obj\n                                               uid ',p-goal-uid)\n                            is 'plan-only\n                            plausibility 0.9)))\n        (setq new-rule\n             (ob$variabilize new-rule #'varize-object? nil *link-slots* nil))\n        (ob$add-unique-name new-rule\n                            (string->symbol\n                             (string-append \"PRESERVATION-PLAN.\"\n                                            (fixnum->string\n                                             *next-prule-number*))))\n        (add-rule-print new-rule)\n        (increment-me *next-prule-number*)\n        ; replan\n        (setq input-states nil)\n        (setq path backwards-planning-path)\n        (setq old-context nil)\n        (yloop (ydo (setq prev-context old-context)\n                  (setq old-context (car path))\n                  (setq input-states (union input-states\n                                           (cx$input-states old-context)))\n                  (setq path (cdr path)))\n              (ywhile path))\n;              (yuntil \n;               (prog1\n;                (progn\n;                 (cx$assert-many old-context input-states)\n;                 (not (show rand old-context *empty-bd* *me-belief-path*)))\n;                (cx$retract-many old-context input-states)))\n;        (if (null? prev-context)\n;            (progn\n;             (error \"null prev context\")\n;             (setq prev-context (ob$get (car failed-goals)\n;                                       'termination-context))))\n        ; This line added for new alg.\n        (setq old-context (ob$get old-top-level-goal 'activation-context))\n        ; Sprout an alternative past context\n        (delay-dbgs 'to-be-set\n         (rule-fire-msg rule \"coded plan\" context bd nil goal)\n         (ndbg-roman-nl *gate-dbg* rule \"Reverse undo cause\")\n         (setq sprouted-context\n              (reversal-sprout-alternative old-context old-top-level-goal\n                                           context top-level-goal\n                                           1.0 t))\n         (setq xxcontext sprouted-context)\n         (no-gen (cx$assert-many sprouted-context input-states))\n         (setq sprouted-contexts (cons sprouted-context\n                                      sprouted-contexts)))))))))\n      sprouted-contexts))\n",
				     2,
				     6264)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16563 **********************/
:- lisp_compile_to_prolog(pkg_user, xt).
:- get_var(TLEnv3, u_xt, Xt_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16566 **********************/
:- lisp_compile_to_prolog(pkg_user, 'old-top-level-goal').
:- get_var(TLEnv3, u_old_top_level_goal, Old_top_level_goal_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16584 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16585 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16586 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Change all remaining planning structure to be on behalf",
				     8,
				     16596)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" of new-top-level-goal.", 8, 16661)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Without replace-linked-ob this would clobber the top-level goals",
				     8,
				     16693)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" on things which are shared with the original episode contexts.",
				     8,
				     16767)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: altern would be to have a cx$copy that copies obs and yet",
				     8,
				     16839)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16586 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" preserves links.", 8, 16912)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16937 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ yloop,
			    [yfor, ob, in, ['cx$get-all', 'sprouted-context']],
			    
			    [ ydo,
			      
			      [ if,
				['ty$instance?', ob, [quote, goal]],
				
				[ progn,
				  
				  [ setq,
				    ob,
				    
				    [ 'replace-linked-ob',
				      ob,
				      'sprouted-context',
				      '*me-belief-path*',
				      '*empty-bd*'
				    ]
				  ],
				  
				  [ 'ob$set',
				    ob,
				    [quote, 'top-level-goal'],
				    'new-top-level-goal'
				  ]
				]
			      ]
			    ]
			  ]).
:- f_u_yloop(
	     [ [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_sprouted_context]],
	       
	       [ u_ydo,
		 
		 [ if,
		   [u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
		   
		   [ progn,
		     
		     [ setq,
		       u_ob,
		       
		       [ u_replace_linked_ob,
			 u_ob,
			 u_sprouted_context,
			 u_xx_me_belief_path_xx,
			 u_xx_empty_bd_xx
		       ]
		     ],
		     
		     [ u_ob_c36_set,
		       u_ob,
		       [quote, u_top_level_goal],
		       u_new_top_level_goal
		     ]
		   ]
		 ]
	       ]
	     ],
	     _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16937 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Also, fix up activation contexts to be here. This isn't",
				     8,
				     17278)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16937 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" strictly necessary (?) because failure reversal (which uses",
				     8,
				     17343)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:16937 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" that slot so far) will never get run on these trcs...",
				     8,
				     17412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17474 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ yloop,
			    
			    [ yfor,
			      ob,
			      in,
			      
			      [ 'cx$get-all-ty',
				'sprouted-context',
				'*active-goal-ob*'
			      ]
			    ],
			    
			    [ ydo,
			      
			      [ 'ob$set',
				ob,
				[quote, 'activation-context'],
				'sprouted-context'
			      ]
			    ]
			  ]).
:- f_u_yloop(
	     [ 
	       [ u_yfor,
		 u_ob,
		 u_in,
		 
		 [ u_cx_c36_get_all_ty,
		   u_sprouted_context,
		   u_xx_active_goal_ob_xx
		 ]
	       ],
	       
	       [ u_ydo,
		 
		 [ u_ob_c36_set,
		   u_ob,
		   [quote, u_activation_context],
		   u_sprouted_context
		 ]
	       ]
	     ],
	     _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17621 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [list, 'sprouted-context', 'old-top-level-goal']).
:- get_var(TLEnv3, u_old_top_level_goal, Old_top_level_goal_Get),
   get_var(TLEnv3, u_sprouted_context, Sprouted_context_Get),
   _Ignored=[Sprouted_context_Get, Old_top_level_goal_Get].

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17663 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17664 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17669)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Garbage collect away any planning structure not on behalf of any of",
				     1,
				     17671)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the specified top-level-goals.",
				     1,
				     17741)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (May want to use this other than from sprout-alternative-past in order",
				     1,
				     17774)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  to unclutter contexts.)", 1, 17847)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17874)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: the INTENDS removal is a bit brute force but it probably works.",
				     1,
				     17876)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17948)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Has to be extended also to get rid of relative planning",
				     1,
				     17950)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" structure--- (BELIEVE MS1 (ACTIVE-GOAL ...))",
				     1,
				     18014)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:17665 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 18061)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gc-plans',
			    [context, 'top-level-goals'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Gc plans for ~A in ~A"),
			      'top-level-goals',
			      context
			    ],
			    
			    [ yloop,
			      [initial, [deps, []]],
			      [yfor, ob, in, ['cx$get-all', context]],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    ['ty$instance?', ob, [quote, goal]],
				    
				    [ not,
				      
				      [ 'memq?',
					['ob$get', ob, [quote, 'top-level-goal']],
					'top-level-goals'
				      ]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ setq,
				      deps,
				      ['get-links', ob, '*intends-ob*', context]
				    ],
				    
				    [ yloop,
				      [yfor, dep, in, deps],
				      [ydo, ['cx$retract', context, dep]]
				    ],
				    ['cx$retract', context, ob]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GC-PLANS 
wl: lambda_def(defun,
	      u_gc_plans,
	      f_u_gc_plans,
	      [u_context, u_top_level_goals],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('G'),
			     #\(c),
			     #\(' '),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
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
		  u_top_level_goals,
		  u_context
		],
		
		[ u_yloop,
		  [u_initial, [u_deps, []]],
		  [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			[u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
			
			[ not,
			  
			  [ u_memq_c63,
			    [u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
			    u_top_level_goals
			  ]
			]
		      ],
		      
		      [ progn,
			
			[ setq,
			  u_deps,
			  [u_get_links, u_ob, u_xx_intends_ob_xx, u_context]
			],
			
			[ u_yloop,
			  [u_yfor, u_dep, u_in, u_deps],
			  [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			],
			[u_cx_c36_retract, u_context, u_ob]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GC-PLANS 
wl: arglist_info(u_gc_plans,
		[u_context, u_top_level_goals],
		[Context_Param, Top_level_goals_Param],
		arginfo{ all:[u_context, u_top_level_goals],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_top_level_goals],
			 opt:0,
			 req:[u_context, u_top_level_goals],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GC-PLANS 
wl: init_args(exact_only, u_gc_plans).


% annotating U::GC-PLANS 
f_u_gc_plans(Context_Param, Top_level_goals_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_top_level_goals, Top_level_goals_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('G'),
				       #\(c),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
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
			    u_top_level_goals,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_deps, []]],
		    [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
			  
			  [ not,
			    
			    [ u_memq_c63,
			      [u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
			      u_top_level_goals
			    ]
			  ]
			],
			
			[ progn,
			  
			  [ setq,
			    u_deps,
			    [u_get_links, u_ob, u_xx_intends_ob_xx, u_context]
			  ],
			  
			  [ u_yloop,
			    [u_yfor, u_dep, u_in, u_deps],
			    [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			  ],
			  [u_cx_c36_retract, u_context, u_ob]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_gc_plans, classof, claz_function),
   set_opv(u_gc_plans, compile_as, kw_function),
   set_opv(u_gc_plans, function, f_u_gc_plans),
   DefunResult=u_gc_plans.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:18661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gc-plans1',
			    [context, 'top-level-goals'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Gc plans for ~A in ~A"),
			      'top-level-goals',
			      context
			    ],
			    
			    [ yloop,
			      [initial, [deps, []]],
			      [yfor, ob, in, ['cx$get-all', context]],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    ['ty$instance?', ob, [quote, goal]],
				    
				    [ 'memq?',
				      ['ob$get', ob, [quote, 'top-level-goal']],
				      'top-level-goals'
				    ]
				  ],
				  
				  [ progn,
				    
				    [ setq,
				      deps,
				      ['get-links', ob, '*intends-ob*', context]
				    ],
				    
				    [ yloop,
				      [yfor, dep, in, deps],
				      [ydo, ['cx$retract', context, dep]]
				    ],
				    
				    [ if,
				      [not, ['memq?', ob, 'top-level-goals']],
				      ['cx$retract', context, ob]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GC-PLANS1 
wl: lambda_def(defun,
	      u_gc_plans1,
	      f_u_gc_plans1,
	      [u_context, u_top_level_goals],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('G'),
			     #\(c),
			     #\(' '),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
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
		  u_top_level_goals,
		  u_context
		],
		
		[ u_yloop,
		  [u_initial, [u_deps, []]],
		  [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			[u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
			
			[ u_memq_c63,
			  [u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
			  u_top_level_goals
			]
		      ],
		      
		      [ progn,
			
			[ setq,
			  u_deps,
			  [u_get_links, u_ob, u_xx_intends_ob_xx, u_context]
			],
			
			[ u_yloop,
			  [u_yfor, u_dep, u_in, u_deps],
			  [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			],
			
			[ if,
			  [not, [u_memq_c63, u_ob, u_top_level_goals]],
			  [u_cx_c36_retract, u_context, u_ob]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GC-PLANS1 
wl: arglist_info(u_gc_plans1,
		[u_context, u_top_level_goals],
		[Context_Param, Top_level_goals_Param],
		arginfo{ all:[u_context, u_top_level_goals],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_top_level_goals],
			 opt:0,
			 req:[u_context, u_top_level_goals],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GC-PLANS1 
wl: init_args(exact_only, u_gc_plans1).


% annotating U::GC-PLANS1 
f_u_gc_plans1(Context_Param, Top_level_goals_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_top_level_goals, Top_level_goals_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('G'),
				       #\(c),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
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
			    u_top_level_goals,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_deps, []]],
		    [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
			  
			  [ u_memq_c63,
			    [u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
			    u_top_level_goals
			  ]
			],
			
			[ progn,
			  
			  [ setq,
			    u_deps,
			    [u_get_links, u_ob, u_xx_intends_ob_xx, u_context]
			  ],
			  
			  [ u_yloop,
			    [u_yfor, u_dep, u_in, u_deps],
			    [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			  ],
			  
			  [ if,
			    [not, [u_memq_c63, u_ob, u_top_level_goals]],
			    [u_cx_c36_retract, u_context, u_ob]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_gc_plans1, classof, claz_function),
   set_opv(u_gc_plans1, compile_as, kw_function),
   set_opv(u_gc_plans1, function, f_u_gc_plans1),
   DefunResult=u_gc_plans1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:18661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                     (not (memq? ob top-level-goals))",
				     1,
				     18908)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:18661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Don't clobber the top-level goal itself.",
				     18,
				     19242)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:19390 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gc-emotions',
			    [context],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Gc emotions in ~A"),
			      context
			    ],
			    
			    [ yloop,
			      [initial, [deps, []]],
			      [yfor, ob, in, ['cx$get-all', context]],
			      
			      [ ydo,
				
				[ if,
				  ['ty$instance?', ob, [quote, emotion]],
				  
				  [ progn,
				    
				    [ setq,
				      deps,
				      
				      [ 'get-links',
					ob,
					'*dependency-ob*',
					context
				      ]
				    ],
				    
				    [ yloop,
				      [yfor, dep, in, deps],
				      [ydo, ['cx$retract', context, dep]]
				    ],
				    
				    [ setq,
				      deps,
				      
				      [ 'get-links-from',
					ob,
					'*dependency-ob*',
					context
				      ]
				    ],
				    
				    [ yloop,
				      [yfor, dep, in, deps],
				      [ydo, ['cx$retract', context, dep]]
				    ],
				    ['cx$retract', context, ob]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GC-EMOTIONS 
wl: lambda_def(defun,
	      u_gc_emotions,
	      f_u_gc_emotions,
	      [u_context],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('G'),
			     #\(c),
			     #\(' '),
			     #\(e),
			     #\(m),
			     #\(o),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_context
		],
		
		[ u_yloop,
		  [u_initial, [u_deps, []]],
		  [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_ty_c36_instance_c63, u_ob, [quote, u_emotion]],
		      
		      [ progn,
			
			[ setq,
			  u_deps,
			  [u_get_links, u_ob, u_xx_dependency_ob_xx, u_context]
			],
			
			[ u_yloop,
			  [u_yfor, u_dep, u_in, u_deps],
			  [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			],
			
			[ setq,
			  u_deps,
			  
			  [ u_get_links_from,
			    u_ob,
			    u_xx_dependency_ob_xx,
			    u_context
			  ]
			],
			
			[ u_yloop,
			  [u_yfor, u_dep, u_in, u_deps],
			  [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			],
			[u_cx_c36_retract, u_context, u_ob]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GC-EMOTIONS 
wl: arglist_info(u_gc_emotions,
		[u_context],
		[Context_Param],
		arginfo{ all:[u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context],
			 opt:0,
			 req:[u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GC-EMOTIONS 
wl: init_args(exact_only, u_gc_emotions).


% annotating U::GC-EMOTIONS 
f_u_gc_emotions(Context_Param, FnResult) :-
	Env=[bv(u_context, Context_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('G'),
				       #\(c),
				       #\(' '),
				       #\(e),
				       #\(m),
				       #\(o),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_deps, []]],
		    [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ty_c36_instance_c63, u_ob, [quote, u_emotion]],
			
			[ progn,
			  
			  [ setq,
			    u_deps,
			    [u_get_links, u_ob, u_xx_dependency_ob_xx, u_context]
			  ],
			  
			  [ u_yloop,
			    [u_yfor, u_dep, u_in, u_deps],
			    [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			  ],
			  
			  [ setq,
			    u_deps,
			    
			    [ u_get_links_from,
			      u_ob,
			      u_xx_dependency_ob_xx,
			      u_context
			    ]
			  ],
			  
			  [ u_yloop,
			    [u_yfor, u_dep, u_in, u_deps],
			    [u_ydo, [u_cx_c36_retract, u_context, u_dep]]
			  ],
			  [u_cx_c36_retract, u_context, u_ob]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_gc_emotions, classof, claz_function),
   set_opv(u_gc_emotions, compile_as, kw_function),
   set_opv(u_gc_emotions, function, f_u_gc_emotions),
   DefunResult=u_gc_emotions.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal.cl:19390 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 20025)).
:- true.


% Total time: 2.329 seconds


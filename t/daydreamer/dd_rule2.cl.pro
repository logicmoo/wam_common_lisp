
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_rule2" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:14:10 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 11/19/86: Broke up into two files",
				     1,
				     208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:243 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 244)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:245 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     246)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:326 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Is it redundant also to run fact plans in run-plan? (no 's') Will they",
				     1,
				     328)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:406 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" all already have been tried before?",
				     1,
				     407)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-plans',
			    ['top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Running plans in ~A for ~A bp ~A"),
			      context,
			      'top-level-goal',
			      'belief-path'
			    ],
			    
			    [ let,
			      
			      [ ['rule-fired?', []],
				[elems, ['cx$get-all', context]]
			      ],
			      
			      [ yloop,
				[yfor, elem, in, elems],
				[yuntil, 'rule-fired?'],
				
				[ ydo,
				  
				  [ setq,
				    elem,
				    ['absolute->relative', elem, 'belief-path']
				  ],
				  
				  [ if,
				    
				    [ and,
				      elem,
				      
				      [ 'ty$instance?',
					elem,
					[quote, 'active-goal']
				      ],
				      
				      [ not,
					['ty$instance?', elem, [quote, 'p-goal']]
				      ],
				      
				      [ 'eq?',
					
					[ 'ob$get',
					  elem,
					  [quote, 'top-level-goal']
					],
					'top-level-goal'
				      ],
				      
				      [ not,
					
					[ 'has-link-relative?',
					  elem,
					  [quote, 'linked-from-of'],
					  '*intends-ob*',
					  context,
					  'belief-path'
					]
				      ],
				      ['preservation-goal-subgoal?', elem],
				      ['seq-head?', elem, context, 'belief-path']
				    ],
				    
				    [ setq,
				      'rule-fired?',
				      
				      [ or,
					
					[ 'run-plan',
					  elem,
					  'top-level-goal',
					  context,
					  'belief-path'
					],
					'rule-fired?'
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				[not, 'rule-fired?'],
				
				[ yloop,
				  [yfor, elem, in, elems],
				  
				  [ ydo,
				    
				    [ setq,
				      elem,
				      
				      [ 'absolute->relative',
					elem,
					'belief-path'
				      ]
				    ],
				    
				    [ cond,
				      
				      [ 
					[ and,
					  elem,
					  
					  [ 'ty$instance?',
					    elem,
					    [quote, 'active-goal']
					  ],
					  
					  [ not,
					    
					    [ 'ty$instance?',
					      elem,
					      [quote, 'p-goal']
					    ]
					  ],
					  
					  [ 'eq?',
					    
					    [ 'ob$get',
					      elem,
					      [quote, 'top-level-goal']
					    ],
					    'top-level-goal'
					  ],
					  
					  [ not,
					    
					    [ 'has-link-relative?',
					      elem,
					      [quote, 'linked-from-of'],
					      '*intends-ob*',
					      context,
					      'belief-path'
					    ]
					  ],
					  
					  [ not,
					    
					    [ 'preservation-goal-subgoal?',
					      elem
					    ]
					  ],
					  
					  [ 'seq-head?',
					    elem,
					    context,
					    'belief-path'
					  ]
					],
					
					[ setq,
					  'rule-fired?',
					  
					  [ or,
					    
					    [ 'run-plan',
					      elem,
					      'top-level-goal',
					      context,
					      'belief-path'
					    ],
					    'rule-fired?'
					  ]
					]
				      ],
				      
				      [ 
					[ and,
					  [],
					  elem,
					  
					  [ 'ty$instance?',
					    elem,
					    [quote, 'active-goal']
					  ],
					  
					  [ not,
					    
					    [ 'ty$instance?',
					      elem,
					      [quote, 'p-goal']
					    ]
					  ],
					  
					  [ 'eq?',
					    
					    [ 'ob$get',
					      elem,
					      [quote, 'top-level-goal']
					    ],
					    'top-level-goal'
					  ]
					],
					
					[ setq,
					  'rule-fired?',
					  
					  [ or,
					    
					    [ 'run-fact-plan',
					      elem,
					      'top-level-goal',
					      context,
					      'belief-path'
					    ],
					    'rule-fired?'
					  ]
					]
				      ]
				    ]
				  ],
				  [yresult, 'rule-fired?']
				],
				'rule-fired?'
			      ]
			    ]
			  ]).

% annotating U::RUN-PLANS 
wl: lambda_def(defun,
	      u_run_plans,
	      f_u_run_plans,
	      [u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(n),
			     #\(n),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(b),
			     #\(p),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_context,
		  u_top_level_goal,
		  u_belief_path
		],
		
		[ let,
		  
		  [ [u_rule_fired_c63, []],
		    [u_elems, [u_cx_c36_get_all, u_context]]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_elem, u_in, u_elems],
		    [u_yuntil, u_rule_fired_c63],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_elem,
			[u_absolute_c62_relative, u_elem, u_belief_path]
		      ],
		      
		      [ if,
			
			[ and,
			  u_elem,
			  [u_ty_c36_instance_c63, u_elem, [quote, u_active_goal]],
			  
			  [ not,
			    [u_ty_c36_instance_c63, u_elem, [quote, u_p_goal]]
			  ],
			  
			  [ u_eq_c63,
			    [u_ob_c36_get, u_elem, [quote, u_top_level_goal]],
			    u_top_level_goal
			  ],
			  
			  [ not,
			    
			    [ u_has_link_relative_c63,
			      u_elem,
			      [quote, u_linked_from_of],
			      u_xx_intends_ob_xx,
			      u_context,
			      u_belief_path
			    ]
			  ],
			  [u_preservation_goal_subgoal_c63, u_elem],
			  [u_seq_head_c63, u_elem, u_context, u_belief_path]
			],
			
			[ setq,
			  u_rule_fired_c63,
			  
			  [ or,
			    
			    [ u_run_plan,
			      u_elem,
			      u_top_level_goal,
			      u_context,
			      u_belief_path
			    ],
			    u_rule_fired_c63
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [not, u_rule_fired_c63],
		    
		    [ u_yloop,
		      [u_yfor, u_elem, u_in, u_elems],
		      
		      [ u_ydo,
			
			[ setq,
			  u_elem,
			  [u_absolute_c62_relative, u_elem, u_belief_path]
			],
			
			[ cond,
			  
			  [ 
			    [ and,
			      u_elem,
			      
			      [ u_ty_c36_instance_c63,
				u_elem,
				[quote, u_active_goal]
			      ],
			      
			      [ not,
				
				[ u_ty_c36_instance_c63,
				  u_elem,
				  [quote, u_p_goal]
				]
			      ],
			      
			      [ u_eq_c63,
				[u_ob_c36_get, u_elem, [quote, u_top_level_goal]],
				u_top_level_goal
			      ],
			      
			      [ not,
				
				[ u_has_link_relative_c63,
				  u_elem,
				  [quote, u_linked_from_of],
				  u_xx_intends_ob_xx,
				  u_context,
				  u_belief_path
				]
			      ],
			      [not, [u_preservation_goal_subgoal_c63, u_elem]],
			      [u_seq_head_c63, u_elem, u_context, u_belief_path]
			    ],
			    
			    [ setq,
			      u_rule_fired_c63,
			      
			      [ or,
				
				[ u_run_plan,
				  u_elem,
				  u_top_level_goal,
				  u_context,
				  u_belief_path
				],
				u_rule_fired_c63
			      ]
			    ]
			  ],
			  
			  [ 
			    [ and,
			      [],
			      u_elem,
			      
			      [ u_ty_c36_instance_c63,
				u_elem,
				[quote, u_active_goal]
			      ],
			      
			      [ not,
				
				[ u_ty_c36_instance_c63,
				  u_elem,
				  [quote, u_p_goal]
				]
			      ],
			      
			      [ u_eq_c63,
				[u_ob_c36_get, u_elem, [quote, u_top_level_goal]],
				u_top_level_goal
			      ]
			    ],
			    
			    [ setq,
			      u_rule_fired_c63,
			      
			      [ or,
				
				[ u_run_fact_plan,
				  u_elem,
				  u_top_level_goal,
				  u_context,
				  u_belief_path
				],
				u_rule_fired_c63
			      ]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_rule_fired_c63]
		    ],
		    u_rule_fired_c63
		  ]
		]
	      ]).


% annotating U::RUN-PLANS 
wl: arglist_info(u_run_plans,
		[u_top_level_goal, u_context, u_belief_path],
		[Top_level_goal_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_top_level_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_top_level_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-PLANS 
wl: init_args(exact_only, u_run_plans).


% annotating U::RUN-PLANS 
f_u_run_plans(Top_level_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
				       #\(n),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(b),
				       #\(p),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_context,
			    u_top_level_goal,
			    u_belief_path
			  ],
			  Roman_nl_Ret),
	f_u_cx_c36_get_all(Context_Param, Elems_Init),
	LEnv=[[bv(u_rule_fired_c63, []), bv(u_elems, Elems_Init)]|Env],
	f_u_yloop(
		  [ [u_yfor, u_elem, u_in, u_elems],
		    [u_yuntil, u_rule_fired_c63],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_elem,
			[u_absolute_c62_relative, u_elem, u_belief_path]
		      ],
		      
		      [ if,
			
			[ and,
			  u_elem,
			  [u_ty_c36_instance_c63, u_elem, [quote, u_active_goal]],
			  
			  [ not,
			    [u_ty_c36_instance_c63, u_elem, [quote, u_p_goal]]
			  ],
			  
			  [ u_eq_c63,
			    [u_ob_c36_get, u_elem, [quote, u_top_level_goal]],
			    u_top_level_goal
			  ],
			  
			  [ not,
			    
			    [ u_has_link_relative_c63,
			      u_elem,
			      [quote, u_linked_from_of],
			      u_xx_intends_ob_xx,
			      u_context,
			      u_belief_path
			    ]
			  ],
			  [u_preservation_goal_subgoal_c63, u_elem],
			  [u_seq_head_c63, u_elem, u_context, u_belief_path]
			],
			
			[ setq,
			  u_rule_fired_c63,
			  
			  [ or,
			    
			    [ u_run_plan,
			      u_elem,
			      u_top_level_goal,
			      u_context,
			      u_belief_path
			    ],
			    u_rule_fired_c63
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_rule_fired_c63, Rule_fired_c63_Get),
	(   Rule_fired_c63_Get==[]
	->  f_u_yloop(
		      [ [u_yfor, u_elem, u_in, u_elems],
			
			[ u_ydo,
			  
			  [ setq,
			    u_elem,
			    [u_absolute_c62_relative, u_elem, u_belief_path]
			  ],
			  
			  [ cond,
			    
			    [ 
			      [ and,
				u_elem,
				
				[ u_ty_c36_instance_c63,
				  u_elem,
				  [quote, u_active_goal]
				],
				
				[ not,
				  
				  [ u_ty_c36_instance_c63,
				    u_elem,
				    [quote, u_p_goal]
				  ]
				],
				
				[ u_eq_c63,
				  
				  [ u_ob_c36_get,
				    u_elem,
				    [quote, u_top_level_goal]
				  ],
				  u_top_level_goal
				],
				
				[ not,
				  
				  [ u_has_link_relative_c63,
				    u_elem,
				    [quote, u_linked_from_of],
				    u_xx_intends_ob_xx,
				    u_context,
				    u_belief_path
				  ]
				],
				[not, [u_preservation_goal_subgoal_c63, u_elem]],
				
				[ u_seq_head_c63,
				  u_elem,
				  u_context,
				  u_belief_path
				]
			      ],
			      
			      [ setq,
				u_rule_fired_c63,
				
				[ or,
				  
				  [ u_run_plan,
				    u_elem,
				    u_top_level_goal,
				    u_context,
				    u_belief_path
				  ],
				  u_rule_fired_c63
				]
			      ]
			    ],
			    
			    [ 
			      [ and,
				[],
				u_elem,
				
				[ u_ty_c36_instance_c63,
				  u_elem,
				  [quote, u_active_goal]
				],
				
				[ not,
				  
				  [ u_ty_c36_instance_c63,
				    u_elem,
				    [quote, u_p_goal]
				  ]
				],
				
				[ u_eq_c63,
				  
				  [ u_ob_c36_get,
				    u_elem,
				    [quote, u_top_level_goal]
				  ],
				  u_top_level_goal
				]
			      ],
			      
			      [ setq,
				u_rule_fired_c63,
				
				[ or,
				  
				  [ u_run_fact_plan,
				    u_elem,
				    u_top_level_goal,
				    u_context,
				    u_belief_path
				  ],
				  u_rule_fired_c63
				]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_rule_fired_c63]
		      ],
		      TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_rule_fired_c63, Rule_fired_c63_Get25),
	    FnResult=Rule_fired_c63_Get25
	).
:- set_opv(f_u_run_plans, classof, claz_function),
   set_opv(u_run_plans, compile_as, kw_function),
   set_opv(u_run_plans, function, f_u_run_plans),
   DefunResult=u_run_plans.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (New) p-goals override any other goals. If there is a p-goal, then",
				     2,
				     610)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" that's all we do this time. Plus we only do one p-goal at",
				     2,
				     680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" a time (thus the until; in the case of regular planning,",
				     2,
				     741)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" seq next will ensure that only one subgoal is planned for at",
				     2,
				     801)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" a time; if there is no seq next, then the user desired for",
				     2,
				     865)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" more than one to be planned at a time...)",
				     2,
				     927)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Added the below", 14, 1519)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end and", 16, 1602)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end and", 18, 2325)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This case would allow fact plans to fire in non-seq order.",
				     10,
				     2483)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This does not work for EXPERIENCE1 in the beginning when",
				     10,
				     2553)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" it needs to keep ?Person2 as a variable so that serendipity",
				     10,
				     2621)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" can occur later.", 10, 2692)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Fact plans are also done by the above, so the below isn't",
				     10,
				     2720)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" strictly necessary.", 10, 2789)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:444 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (not (ob$get elem 'seq-next-of)) or like above",
				     11,
				     3044)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3329 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'preservation-goal-subgoal?',
			    [goal],
			    
			    [ or,
			      
			      [ 'ty$instance?',
				['ob$get', goal, [quote, obj]],
				[quote, preservation]
			      ],
			      ['ob$get', goal, [quote, 'preservation-subgoal?']]
			    ]
			  ]).

% annotating U::PRESERVATION-GOAL-SUBGOAL? 
wl: lambda_def(defun,
	      u_preservation_goal_subgoal_c63,
	      f_u_preservation_goal_subgoal_c63,
	      [u_goal],
	      
	      [ 
		[ or,
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    [quote, u_preservation]
		  ],
		  [u_ob_c36_get, u_goal, [quote, u_preservation_subgoal_c63]]
		]
	      ]).


% annotating U::PRESERVATION-GOAL-SUBGOAL? 
wl: arglist_info(u_preservation_goal_subgoal_c63,
		[u_goal],
		[Goal_Param],
		arginfo{ all:[u_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal],
			 opt:0,
			 req:[u_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRESERVATION-GOAL-SUBGOAL? 
wl: init_args(exact_only, u_preservation_goal_subgoal_c63).


% annotating U::PRESERVATION-GOAL-SUBGOAL? 
f_u_preservation_goal_subgoal_c63(Goal_Param, FnResult) :-
	(   f_u_ob_c36_get(Goal_Param, u_obj, Obj),
	    f_u_ty_c36_instance_c63(Obj, u_preservation, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_ob_c36_get(Goal_Param,
			   u_preservation_subgoal_c63,
			   Preservation_subgoal_c63),
	    FnResult=Preservation_subgoal_c63
	).
:- set_opv(f_u_preservation_goal_subgoal_c63, classof, claz_function),
   set_opv(u_preservation_goal_subgoal_c63, compile_as, kw_function),
   set_opv(u_preservation_goal_subgoal_c63,
	   function,
	   f_u_preservation_goal_subgoal_c63),
   DefunResult=u_preservation_goal_subgoal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3329 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (Active-Goal Me ?x)", 1, 3472)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3329 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (Believe Debra (Active-Goal Debra ?x)",
				     1,
				     3494)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3329 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (Believe Debra (Believe Me (Active-Goal Me ?x)))",
				     1,
				     3534)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3329 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Below to be determined empirically",
				     1,
				     3586)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3622 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*perf-sim-thresh*', 0.0]).
:- set_var(TLEnv3, setq, u_xx_perf_sim_thresh_xx, 0.0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3651 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*perf-reality-thresh*', 0.6]).
:- set_var(TLEnv3, setq, u_xx_perf_reality_thresh_xx, 0.6).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3684 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*perf-desir-thresh*', 0.0]).
:- set_var(TLEnv3, setq, u_xx_perf_desir_thresh_xx, 0.0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3684 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" who knows?", 32, 3716)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3728 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*goal-relaxation-realism*', 0.2]).
:- set_var(TLEnv3, setq, u_xx_goal_relaxation_realism_xx, 0.2).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3766 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*fanciful-sim-thresh*', 0.0]).
:- set_var(TLEnv3, setq, u_xx_fanciful_sim_thresh_xx, 0.0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*realistic-sim-thresh*', 0.0]).
:- set_var(TLEnv3, setq, u_xx_realistic_sim_thresh_xx, 0.0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3835)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Order (and prune) episodes within candidates:",
				     1,
				     3837)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Real top-level-goal: Order according to reality*similarity*desirability",
				     1,
				     3885)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                        with threshold cutoffs for each",
				     1,
				     3961)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   Note: We want to keep around some alternatives for reversal; that's",
				     1,
				     4018)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         why why don't just prune all but the highest.",
				     1,
				     4090)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Imaginary-realistic top-level-goal: Order according to",
				     1,
				     4146)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       reality*similarity*desirability with threshold only for similarity",
				     1,
				     4205)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Imaginary-fanciful top-level goal: Order according to similarity with",
				     1,
				     4280)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("       threshold.", 1, 4354)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:3799 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4373)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:4374 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'order-candidates',
			    ['goal-obj', candidates, 'top-level-goal'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Order candidates ~A"),
			      candidates
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				[sim, []],
				[ordering, []],
				['any-episode?', []]
			      ],
			      [yfor, candidate, in, candidates],
			      
			      [ ydo,
				
				[ yloop,
				  
				  [ yfor,
				    episode,
				    in,
				    ['candidate-episodes', candidate]
				  ],
				  
				  [ ydo,
				    
				    [ setq,
				      sim,
				      
				      [ 'ob$similarity',
					'goal-obj',
					['ob$pget', episode, [quote, [goal, obj]]]
				      ]
				    ],
				    
				    [ cond,
				      
				      [ ['real?', 'top-level-goal'],
					
					[ setq,
					  ordering,
					  
					  [ 'fl*',
					    
					    [ thresh,
					      
					      [ 'ob$get',
						episode,
						[quote, realism]
					      ],
					      '*perf-reality-thresh*'
					    ],
					    
					    [ 'fl*',
					      [thresh, sim, '*perf-sim-thresh*'],
					      
					      [ thresh,
						
						[ 'ob$get',
						  episode,
						  [quote, desirability]
						],
						'*perf-desir-thresh*'
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ 
					[ 'imaginary-realistic?',
					  'top-level-goal'
					],
					
					[ setq,
					  ordering,
					  
					  [ 'fl*',
					    ['ob$get', episode, [quote, realism]],
					    
					    [ 'fl*',
					      
					      [ thresh,
						sim,
						'*realistic-sim-thresh*'
					      ],
					      
					      [ 'ob$get',
						episode,
						[quote, desirability]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ 
					[ 'imaginary-fanciful?',
					  'top-level-goal'
					],
					
					[ setq,
					  ordering,
					  [thresh, sim, '*fanciful-sim-thresh*']
					]
				      ]
				    ],
				    
				    [ 'ob$set',
				      episode,
				      [quote, ordering],
				      ordering
				    ],
				    
				    [ if,
				      ['fl>', ordering, 0.0],
				      [setq, 'any-episode?', t]
				    ]
				  ]
				]
			      ],
			      [yresult, 'any-episode?']
			    ]
			  ]).

% annotating U::ORDER-CANDIDATES 
wl: lambda_def(defun,
	      u_order_candidates,
	      f_u_order_candidates,
	      [u_goal_obj, u_candidates, u_top_level_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('O'),
			     #\(r),
			     #\(d),
			     #\(e),
			     #\(r),
			     #\(' '),
			     #\(c),
			     #\(a),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(d),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_candidates
		],
		
		[ u_yloop,
		  
		  [ u_initial,
		    [u_sim, []],
		    [u_ordering, []],
		    [u_any_episode_c63, []]
		  ],
		  [u_yfor, u_candidate, u_in, u_candidates],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      
		      [ u_yfor,
			u_episode,
			u_in,
			[u_candidate_episodes, u_candidate]
		      ],
		      
		      [ u_ydo,
			
			[ setq,
			  u_sim,
			  
			  [ u_ob_c36_similarity,
			    u_goal_obj,
			    [u_ob_c36_pget, u_episode, [quote, [u_goal, u_obj]]]
			  ]
			],
			
			[ cond,
			  
			  [ [u_real_c63, u_top_level_goal],
			    
			    [ setq,
			      u_ordering,
			      
			      [ u_fl_xx,
				
				[ u_thresh,
				  [u_ob_c36_get, u_episode, [quote, u_realism]],
				  u_xx_perf_reality_thresh_xx
				],
				
				[ u_fl_xx,
				  [u_thresh, u_sim, u_xx_perf_sim_thresh_xx],
				  
				  [ u_thresh,
				    
				    [ u_ob_c36_get,
				      u_episode,
				      [quote, u_desirability]
				    ],
				    u_xx_perf_desir_thresh_xx
				  ]
				]
			      ]
			    ]
			  ],
			  
			  [ [u_imaginary_realistic_c63, u_top_level_goal],
			    
			    [ setq,
			      u_ordering,
			      
			      [ u_fl_xx,
				[u_ob_c36_get, u_episode, [quote, u_realism]],
				
				[ u_fl_xx,
				  
				  [ u_thresh,
				    u_sim,
				    u_xx_realistic_sim_thresh_xx
				  ],
				  
				  [ u_ob_c36_get,
				    u_episode,
				    [quote, u_desirability]
				  ]
				]
			      ]
			    ]
			  ],
			  
			  [ [u_imaginary_fanciful_c63, u_top_level_goal],
			    
			    [ setq,
			      u_ordering,
			      [u_thresh, u_sim, u_xx_fanciful_sim_thresh_xx]
			    ]
			  ]
			],
			[u_ob_c36_set, u_episode, [quote, u_ordering], u_ordering],
			
			[ if,
			  [u_fl_c62, u_ordering, 0.0],
			  [setq, u_any_episode_c63, t]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_any_episode_c63]
		]
	      ]).


% annotating U::ORDER-CANDIDATES 
wl: arglist_info(u_order_candidates,
		[u_goal_obj, u_candidates, u_top_level_goal],
		[Goal_obj_Param, Candidates_Param, Top_level_goal_Param],
		arginfo{ all:[u_goal_obj, u_candidates, u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal_obj, u_candidates, u_top_level_goal],
			 opt:0,
			 req:[u_goal_obj, u_candidates, u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ORDER-CANDIDATES 
wl: init_args(exact_only, u_order_candidates).


% annotating U::ORDER-CANDIDATES 
f_u_order_candidates(Goal_obj_Param, Candidates_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_goal_obj, Goal_obj_Param), bv(u_candidates, Candidates_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('O'),
				       #\(r),
				       #\(d),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(c),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(d),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_candidates
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_sim, []],
		      [u_ordering, []],
		      [u_any_episode_c63, []]
		    ],
		    [u_yfor, u_candidate, u_in, u_candidates],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_episode,
			  u_in,
			  [u_candidate_episodes, u_candidate]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_sim,
			    
			    [ u_ob_c36_similarity,
			      u_goal_obj,
			      [u_ob_c36_pget, u_episode, [quote, [u_goal, u_obj]]]
			    ]
			  ],
			  
			  [ cond,
			    
			    [ [u_real_c63, u_top_level_goal],
			      
			      [ setq,
				u_ordering,
				
				[ u_fl_xx,
				  
				  [ u_thresh,
				    [u_ob_c36_get, u_episode, [quote, u_realism]],
				    u_xx_perf_reality_thresh_xx
				  ],
				  
				  [ u_fl_xx,
				    [u_thresh, u_sim, u_xx_perf_sim_thresh_xx],
				    
				    [ u_thresh,
				      
				      [ u_ob_c36_get,
					u_episode,
					[quote, u_desirability]
				      ],
				      u_xx_perf_desir_thresh_xx
				    ]
				  ]
				]
			      ]
			    ],
			    
			    [ [u_imaginary_realistic_c63, u_top_level_goal],
			      
			      [ setq,
				u_ordering,
				
				[ u_fl_xx,
				  [u_ob_c36_get, u_episode, [quote, u_realism]],
				  
				  [ u_fl_xx,
				    
				    [ u_thresh,
				      u_sim,
				      u_xx_realistic_sim_thresh_xx
				    ],
				    
				    [ u_ob_c36_get,
				      u_episode,
				      [quote, u_desirability]
				    ]
				  ]
				]
			      ]
			    ],
			    
			    [ [u_imaginary_fanciful_c63, u_top_level_goal],
			      
			      [ setq,
				u_ordering,
				[u_thresh, u_sim, u_xx_fanciful_sim_thresh_xx]
			      ]
			    ]
			  ],
			  
			  [ u_ob_c36_set,
			    u_episode,
			    [quote, u_ordering],
			    u_ordering
			  ],
			  
			  [ if,
			    [u_fl_c62, u_ordering, 0.0],
			    [setq, u_any_episode_c63, t]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_any_episode_c63]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_order_candidates, classof, claz_function),
   set_opv(u_order_candidates, compile_as, kw_function),
   set_opv(u_order_candidates, function, f_u_order_candidates),
   DefunResult=u_order_candidates.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:4374 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 'ordering and 'bd are used purely as temporary",
				     23,
				     5940)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:4374 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" variables which need dure only until all plans",
				     23,
				     6011)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:4374 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" are sprouted, which will not be interrupted",
				     23,
				     6082)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:4374 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" by other top-level goals.", 23, 6150)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:6304 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'candidates->episodes',
			    [candidates],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Candidates to episodes ~A"),
			      candidates
			    ],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, candidate, in, candidates],
			      
			      [ ydo,
				
				[ yloop,
				  
				  [ yfor,
				    episode,
				    in,
				    ['candidate-episodes', candidate]
				  ],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ 'fl>',
					['ob$get', episode, [quote, ordering]],
					0.0
				      ],
				      
				      [ progn,
					
					[ 'ob$set',
					  episode,
					  [quote, bd],
					  ['candidate-bd', candidate]
					],
					[setq, result, [cons, episode, result]]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ yresult,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Result = ~A"),
				  result
				],
				result
			      ]
			    ]
			  ]).

% annotating U::CANDIDATES->EPISODES 
wl: lambda_def(defun,
	      u_candidates_c62_episodes,
	      f_u_candidates_c62_episodes,
	      [u_candidates],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('C'),
			     #\(a),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(d),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(t),
			     #\(o),
			     #\(' '),
			     #\(e),
			     #\(p),
			     #\(i),
			     #\(s),
			     #\(o),
			     #\(d),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_candidates
		],
		
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_candidate, u_in, u_candidates],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      
		      [ u_yfor,
			u_episode,
			u_in,
			[u_candidate_episodes, u_candidate]
		      ],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ u_fl_c62,
			    [u_ob_c36_get, u_episode, [quote, u_ordering]],
			    0.0
			  ],
			  
			  [ progn,
			    
			    [ u_ob_c36_set,
			      u_episode,
			      [quote, u_bd],
			      [u_candidate_bd, u_candidate]
			    ],
			    [setq, u_result, [cons, u_episode, u_result]]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('R'),
				 #\(e),
				 #\(s),
				 #\(u),
				 #\(l),
				 #\(t),
				 #\(' '),
				 #\(=),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_result
		    ],
		    u_result
		  ]
		]
	      ]).


% annotating U::CANDIDATES->EPISODES 
wl: arglist_info(u_candidates_c62_episodes,
		[u_candidates],
		[Candidates_Param],
		arginfo{ all:[u_candidates],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_candidates],
			 opt:0,
			 req:[u_candidates],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CANDIDATES->EPISODES 
wl: init_args(exact_only, u_candidates_c62_episodes).


% annotating U::CANDIDATES->EPISODES 
f_u_candidates_c62_episodes(Candidates_Param, FnResult) :-
	Env=[bv(u_candidates, Candidates_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('C'),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(d),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(e),
				       #\(p),
				       #\(i),
				       #\(s),
				       #\(o),
				       #\(d),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_candidates
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_candidate, u_in, u_candidates],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_episode,
			  u_in,
			  [u_candidate_episodes, u_candidate]
			],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_fl_c62,
			      [u_ob_c36_get, u_episode, [quote, u_ordering]],
			      0.0
			    ],
			    
			    [ progn,
			      
			      [ u_ob_c36_set,
				u_episode,
				[quote, u_bd],
				[u_candidate_bd, u_candidate]
			      ],
			      [setq, u_result, [cons, u_episode, u_result]]
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('R'),
				   #\(e),
				   #\(s),
				   #\(u),
				   #\(l),
				   #\(t),
				   #\(' '),
				   #\(=),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_result
		      ],
		      u_result
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_candidates_c62_episodes, classof, claz_function),
   set_opv(u_candidates_c62_episodes, compile_as, kw_function),
   set_opv(u_candidates_c62_episodes, function, f_u_candidates_c62_episodes),
   DefunResult=u_candidates_c62_episodes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:6940 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'find-candidate-rules',
			    
			    [ 'goal-obj',
			      'believe-other?',
			      'belief-path',
			      context
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Find candidate rules for obj ~A in ~A"),
			      'goal-obj',
			      context
			    ],
			    
			    [ yloop,
			      [initial, [candidates, []]],
			      
			      [ yfor,
				rule,
				in,
				['collect-planning-rules', 'goal-obj']
			      ],
			      
			      [ ydo,
				
				[ if,
				  ['plan?', rule],
				  
				  [ yloop,
				    
				    [ yfor,
				      bd,
				      in,
				      
				      [ 'rule-applications',
					'goal-obj',
					context,
					rule,
					'belief-path',
					'believe-other?'
				      ]
				    ],
				    
				    [ ydo,
				      
				      [ setq,
					candidates,
					
					[ cons,
					  
					  [ 'candidate-create',
					    rule,
					    bd,
					    ['episode-retrieve', rule]
					  ],
					  candidates
					]
				      ]
				    ]
				  ]
				]
			      ],
			      [yresult, candidates]
			    ]
			  ]).

% annotating U::FIND-CANDIDATE-RULES 
wl: lambda_def(defun,
	      u_find_candidate_rules,
	      f_u_find_candidate_rules,
	      [u_goal_obj, u_believe_other_c63, u_belief_path, u_context],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('F'),
			     #\(i),
			     #\(n),
			     #\(d),
			     #\(' '),
			     #\(c),
			     #\(a),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(d),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(' '),
			     #\(r),
			     #\(u),
			     #\(l),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(o),
			     #\(b),
			     #\(j),
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
		  u_goal_obj,
		  u_context
		],
		
		[ u_yloop,
		  [u_initial, [u_candidates, []]],
		  [u_yfor, u_rule, u_in, [u_collect_planning_rules, u_goal_obj]],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_plan_c63, u_rule],
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_bd,
			  u_in,
			  
			  [ u_rule_applications,
			    u_goal_obj,
			    u_context,
			    u_rule,
			    u_belief_path,
			    u_believe_other_c63
			  ]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_candidates,
			    
			    [ cons,
			      
			      [ u_candidate_create,
				u_rule,
				u_bd,
				[u_episode_retrieve, u_rule]
			      ],
			      u_candidates
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_candidates]
		]
	      ]).


% annotating U::FIND-CANDIDATE-RULES 
wl: arglist_info(u_find_candidate_rules,
		[u_goal_obj, u_believe_other_c63, u_belief_path, u_context],
		
		[ Goal_obj_Param,
		  Believe_other_c63_Param,
		  Belief_path_Param,
		  Context_Param
		],
		arginfo{ all:
			     [ u_goal_obj,
			       u_believe_other_c63,
			       u_belief_path,
			       u_context
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal_obj,
				 u_believe_other_c63,
				 u_belief_path,
				 u_context
			       ],
			 opt:0,
			 req:
			     [ u_goal_obj,
			       u_believe_other_c63,
			       u_belief_path,
			       u_context
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FIND-CANDIDATE-RULES 
wl: init_args(exact_only, u_find_candidate_rules).


% annotating U::FIND-CANDIDATE-RULES 
f_u_find_candidate_rules(Goal_obj_Param, Believe_other_c63_Param, Belief_path_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal_obj, Goal_obj_Param), bv(u_believe_other_c63, Believe_other_c63_Param), bv(u_belief_path, Belief_path_Param), bv(u_context, Context_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('F'),
				       #\(i),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(c),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(d),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(' '),
				       #\(r),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(o),
				       #\(b),
				       #\(j),
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
			    u_goal_obj,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_candidates, []]],
		    [u_yfor, u_rule, u_in, [u_collect_planning_rules, u_goal_obj]],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_plan_c63, u_rule],
			
			[ u_yloop,
			  
			  [ u_yfor,
			    u_bd,
			    u_in,
			    
			    [ u_rule_applications,
			      u_goal_obj,
			      u_context,
			      u_rule,
			      u_belief_path,
			      u_believe_other_c63
			    ]
			  ],
			  
			  [ u_ydo,
			    
			    [ setq,
			      u_candidates,
			      
			      [ cons,
				
				[ u_candidate_create,
				  u_rule,
				  u_bd,
				  [u_episode_retrieve, u_rule]
				],
				u_candidates
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_candidates]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_find_candidate_rules, classof, claz_function),
   set_opv(u_find_candidate_rules, compile_as, kw_function),
   set_opv(u_find_candidate_rules, function, f_u_find_candidate_rules),
   DefunResult=u_find_candidate_rules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:7579 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'collect-planning-rules',
			    ['goal-obj'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Collecting planning rules for ~A"),
			      'goal-obj'
			    ],
			    
			    [ let,
			      [[rules, ['backward-chain-rules', 'goal-obj']]],
			      
			      [ if,
				['eq?', rules, '*rules*'],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'rule-long',
				  '$STRING'("Using full rule set")
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'rule-long',
				  '$STRING'("Collected rules = ~A"),
				  rules
				]
			      ],
			      rules
			    ]
			  ]).

% annotating U::COLLECT-PLANNING-RULES 
wl: lambda_def(defun,
	      u_collect_planning_rules,
	      f_u_collect_planning_rules,
	      [u_goal_obj],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('C'),
			     #\(o),
			     #\(l),
			     #\(l),
			     #\(e),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(n),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(r),
			     #\(u),
			     #\(l),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_goal_obj
		],
		
		[ let,
		  [[u_rules, [u_backward_chain_rules, u_goal_obj]]],
		  
		  [ if,
		    [u_eq_c63, u_rules, u_xx_rules_xx],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule_long,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('U'),
				 #\(s),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\(' '),
				 #\(f),
				 #\(u),
				 #\(l),
				 #\(l),
				 #\(' '),
				 #\(r),
				 #\(u),
				 #\(l),
				 #\(e),
				 #\(' '),
				 #\(s),
				 #\(e),
				 #\(t)
			       ])
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule_long,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('C'),
				 #\(o),
				 #\(l),
				 #\(l),
				 #\(e),
				 #\(c),
				 #\(t),
				 #\(e),
				 #\(d),
				 #\(' '),
				 #\(r),
				 #\(u),
				 #\(l),
				 #\(e),
				 #\(s),
				 #\(' '),
				 #\(=),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_rules
		    ]
		  ],
		  u_rules
		]
	      ]).


% annotating U::COLLECT-PLANNING-RULES 
wl: arglist_info(u_collect_planning_rules,
		[u_goal_obj],
		[Goal_obj_Param],
		arginfo{ all:[u_goal_obj],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal_obj],
			 opt:0,
			 req:[u_goal_obj],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::COLLECT-PLANNING-RULES 
wl: init_args(exact_only, u_collect_planning_rules).


% annotating U::COLLECT-PLANNING-RULES 
f_u_collect_planning_rules(Goal_obj_Param, FnResult) :-
	Env=[bv(u_goal_obj, Goal_obj_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('C'),
				       #\(o),
				       #\(l),
				       #\(l),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(n),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(r),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_goal_obj
			  ],
			  Roman_nl_Ret),
	f_u_backward_chain_rules(u_goal_obj, Rules_Init),
	LEnv=[[bv(u_rules, Rules_Init)]|Env],
	f_u_eq_c63(u_rules, u_xx_rules_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('U'),
					   #\(s),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(' '),
					   #\(f),
					   #\(u),
					   #\(l),
					   #\(l),
					   #\(' '),
					   #\(r),
					   #\(u),
					   #\(l),
					   #\(e),
					   #\(' '),
					   #\(s),
					   #\(e),
					   #\(t)
					 ])
			      ],
			      TrueResult),
	    _113242=TrueResult
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('C'),
					   #\(o),
					   #\(l),
					   #\(l),
					   #\(e),
					   #\(c),
					   #\(t),
					   #\(e),
					   #\(d),
					   #\(' '),
					   #\(r),
					   #\(u),
					   #\(l),
					   #\(e),
					   #\(s),
					   #\(' '),
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_rules
			      ],
			      ElseResult),
	    _113242=ElseResult
	),
	get_var(LEnv, u_rules, Rules_Get),
	LetResult=Rules_Get,
	LetResult=FnResult.
:- set_opv(f_u_collect_planning_rules, classof, claz_function),
   set_opv(u_collect_planning_rules, compile_as, kw_function),
   set_opv(u_collect_planning_rules, function, f_u_collect_planning_rules),
   DefunResult=u_collect_planning_rules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule2.cl:7579 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 7973)).
:- true.


% Total time: 1.669 seconds


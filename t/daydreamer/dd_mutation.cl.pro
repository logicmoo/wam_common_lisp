
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_mutation" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:13:05 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1/12/86: Began adding code for action mutation",
				     1,
				     208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 7/22/86: Redid mutations to use serendipity mechanism",
				     1,
				     257)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:312 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/25/86: Got rid of flavors", 1, 313)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:342 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 343)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:344 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     345)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:425 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    mutations,
			    [action, context],
			    
			    [ yloop,
			      [initial, [result, []], [bd, []]],
			      [yfor, mut, in, '*mutations*'],
			      
			      [ ydo,
				
				[ if,
				  
				  [ setq,
				    bd,
				    ['ob$unify', [car, mut], action, '*empty-bd*']
				  ],
				  
				  [ setq,
				    result,
				    
				    [ cons,
				      ['ob$instantiate', [cadr, mut], bd],
				      result
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::MUTATIONS 
wl: lambda_def(defun,
	      u_mutations,
	      f_u_mutations,
	      [u_action, u_context],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_bd, []]],
		  [u_yfor, u_mut, u_in, u_xx_mutations_xx],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ setq,
			u_bd,
			[u_ob_c36_unify, [car, u_mut], u_action, u_xx_empty_bd_xx]
		      ],
		      
		      [ setq,
			u_result,
			
			[ cons,
			  [u_ob_c36_instantiate, [cadr, u_mut], u_bd],
			  u_result
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::MUTATIONS 
wl: arglist_info(u_mutations,
		[u_action, u_context],
		[Action_Param, Context_Param],
		arginfo{ all:[u_action, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action, u_context],
			 opt:0,
			 req:[u_action, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MUTATIONS 
wl: init_args(exact_only, u_mutations).


% annotating U::MUTATIONS 
f_u_mutations(Action_Param, Context_Param, FnResult) :-
	Env=[bv(u_action, Action_Param), bv(u_context, Context_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_bd, []]],
		    [u_yfor, u_mut, u_in, u_xx_mutations_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ setq,
			  u_bd,
			  
			  [ u_ob_c36_unify,
			    [car, u_mut],
			    u_action,
			    u_xx_empty_bd_xx
			  ]
			],
			
			[ setq,
			  u_result,
			  
			  [ cons,
			    [u_ob_c36_instantiate, [cadr, u_mut], u_bd],
			    u_result
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_mutations, classof, claz_function),
   set_opv(u_mutations, compile_as, kw_function),
   set_opv(u_mutations, function, f_u_mutations),
   DefunResult=u_mutations.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:425 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 760)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:425 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Returns NIL or T.", 1, 762)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:425 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 782)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'action-mutations',
			    ['top-level-goal', 'backtrack-wall'],
			    
			    [ if,
			      
			      [ 'null?',
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'run-mutations?']
				]
			      ],
			      
			      [ progn,
				
				[ 'ob$add',
				  'top-level-goal',
				  [quote, 'run-mutations?'],
				  t
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Action mutations for ~A"),
				  'top-level-goal'
				],
				
				[ yloop,
				  [initial, ['mutated-actions', []], [result, []]],
				  
				  [ yfor,
				    leaf,
				    in,
				    ['cx$leaf-descendants', 'backtrack-wall']
				  ],
				  [yuntil, result],
				  
				  [ ydo,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Trying leaf context ~A"),
				      leaf
				    ],
				    
				    [ if,
				      
				      [ 'null?',
					
					[ 'ob$get',
					  leaf,
					  [quote, 'mutations-tried?']
					]
				      ],
				      
				      [ progn,
					
					[ 'ob$set',
					  leaf,
					  [quote, 'mutations-tried?'],
					  t
					],
					
					[ yloop,
					  
					  [ yfor,
					    ob,
					    in,
					    
					    [ 'cx$get-all-ty',
					      leaf,
					      '*active-goal-ob*'
					    ]
					  ],
					  
					  [ ydo,
					    
					    [ if,
					      
					      [ 'ty$instance?',
						['ob$get', ob, [quote, obj]],
						[quote, action]
					      ],
					      
					      [ progn,
						
						[ 'ndbg-roman-nl',
						  '*gate-dbg*',
						  rule,
						  '$STRING'("Mutating action goal ~A"),
						  ob
						],
						
						[ setq,
						  'mutated-actions',
						  
						  [ mutations,
						    ['ob$get', ob, [quote, obj]],
						    leaf
						  ]
						],
						
						[ yloop,
						  
						  [ yfor,
						    'mutated-action',
						    in,
						    'mutated-actions'
						  ],
						  
						  [ ydo,
						    
						    [ if,
						      
						      [ 'action-mutation',
							'top-level-goal',
							leaf,
							'mutated-action',
							ob
						      ],
						      [setq, result, t]
						    ]
						  ]
						]
					      ]
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ],
			      []
			    ]
			  ]).

% annotating U::ACTION-MUTATIONS 
wl: lambda_def(defun,
	      u_action_mutations,
	      f_u_action_mutations,
	      [u_top_level_goal, u_backtrack_wall],
	      
	      [ 
		[ if,
		  
		  [ u_null_c63,
		    
		    [ u_ob_c36_get,
		      u_top_level_goal,
		      [quote, u_run_mutations_c63]
		    ]
		  ],
		  
		  [ progn,
		    
		    [ u_ob_c36_add,
		      u_top_level_goal,
		      [quote, u_run_mutations_c63],
		      t
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('A'),
				 #\(c),
				 #\(t),
				 #\(i),
				 #\(o),
				 #\(n),
				 #\(' '),
				 #\(m),
				 #\(u),
				 #\(t),
				 #\(a),
				 #\(t),
				 #\(i),
				 #\(o),
				 #\(n),
				 #\(s),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_top_level_goal
		    ],
		    
		    [ u_yloop,
		      [u_initial, [u_mutated_actions, []], [u_result, []]],
		      
		      [ u_yfor,
			u_leaf,
			u_in,
			[u_cx_c36_leaf_descendants, u_backtrack_wall]
		      ],
		      [u_yuntil, u_result],
		      
		      [ u_ydo,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('T'),
				     #\(r),
				     #\(y),
				     #\(i),
				     #\(n),
				     #\(g),
				     #\(' '),
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
				     #\('A')
				   ]),
			  u_leaf
			],
			
			[ if,
			  
			  [ u_null_c63,
			    
			    [ u_ob_c36_get,
			      u_leaf,
			      [quote, u_mutations_tried_c63]
			    ]
			  ],
			  
			  [ progn,
			    
			    [ u_ob_c36_set,
			      u_leaf,
			      [quote, u_mutations_tried_c63],
			      t
			    ],
			    
			    [ u_yloop,
			      
			      [ u_yfor,
				u_ob,
				u_in,
				
				[ u_cx_c36_get_all_ty,
				  u_leaf,
				  u_xx_active_goal_ob_xx
				]
			      ],
			      
			      [ u_ydo,
				
				[ if,
				  
				  [ u_ty_c36_instance_c63,
				    [u_ob_c36_get, u_ob, [quote, u_obj]],
				    [quote, u_action]
				  ],
				  
				  [ progn,
				    
				    [ u_ndbg_roman_nl,
				      u_xx_gate_dbg_xx,
				      u_rule,
				      '$ARRAY'([*],
					       claz_base_character,
					       
					       [ #\('M'),
						 #\(u),
						 #\(t),
						 #\(a),
						 #\(t),
						 #\(i),
						 #\(n),
						 #\(g),
						 #\(' '),
						 #\(a),
						 #\(c),
						 #\(t),
						 #\(i),
						 #\(o),
						 #\(n),
						 #\(' '),
						 #\(g),
						 #\(o),
						 #\(a),
						 #\(l),
						 #\(' '),
						 #\(~),
						 #\('A')
					       ]),
				      u_ob
				    ],
				    
				    [ setq,
				      u_mutated_actions,
				      
				      [ u_mutations,
					[u_ob_c36_get, u_ob, [quote, u_obj]],
					u_leaf
				      ]
				    ],
				    
				    [ u_yloop,
				      
				      [ u_yfor,
					u_mutated_action,
					u_in,
					u_mutated_actions
				      ],
				      
				      [ u_ydo,
					
					[ if,
					  
					  [ u_action_mutation,
					    u_top_level_goal,
					    u_leaf,
					    u_mutated_action,
					    u_ob
					  ],
					  [setq, u_result, t]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ],
		  []
		]
	      ]).


% annotating U::ACTION-MUTATIONS 
wl: arglist_info(u_action_mutations,
		[u_top_level_goal, u_backtrack_wall],
		[Top_level_goal_Param, Backtrack_wall_Param],
		arginfo{ all:[u_top_level_goal, u_backtrack_wall],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_backtrack_wall],
			 opt:0,
			 req:[u_top_level_goal, u_backtrack_wall],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ACTION-MUTATIONS 
wl: init_args(exact_only, u_action_mutations).


% annotating U::ACTION-MUTATIONS 
f_u_action_mutations(Top_level_goal_Param, Backtrack_wall_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_backtrack_wall, Backtrack_wall_Param)],
	f_u_null_c63(
		     [ u_ob_c36_get,
		       u_top_level_goal,
		       [quote, u_run_mutations_c63]
		     ],
		     IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_add(Top_level_goal_Param, u_run_mutations_c63, t, T),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('A'),
					   #\(c),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(' '),
					   #\(m),
					   #\(u),
					   #\(t),
					   #\(a),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(s),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_top_level_goal
			      ],
			      Roman_nl_Ret),
	    f_u_yloop(
		      [ [u_initial, [u_mutated_actions, []], [u_result, []]],
			
			[ u_yfor,
			  u_leaf,
			  u_in,
			  [u_cx_c36_leaf_descendants, u_backtrack_wall]
			],
			[u_yuntil, u_result],
			
			[ u_ydo,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
				       #\(r),
				       #\(y),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
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
				       #\('A')
				     ]),
			    u_leaf
			  ],
			  
			  [ if,
			    
			    [ u_null_c63,
			      
			      [ u_ob_c36_get,
				u_leaf,
				[quote, u_mutations_tried_c63]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ u_ob_c36_set,
				u_leaf,
				[quote, u_mutations_tried_c63],
				t
			      ],
			      
			      [ u_yloop,
				
				[ u_yfor,
				  u_ob,
				  u_in,
				  
				  [ u_cx_c36_get_all_ty,
				    u_leaf,
				    u_xx_active_goal_ob_xx
				  ]
				],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ u_ty_c36_instance_c63,
				      [u_ob_c36_get, u_ob, [quote, u_obj]],
				      [quote, u_action]
				    ],
				    
				    [ progn,
				      
				      [ u_ndbg_roman_nl,
					u_xx_gate_dbg_xx,
					u_rule,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('M'),
						   #\(u),
						   #\(t),
						   #\(a),
						   #\(t),
						   #\(i),
						   #\(n),
						   #\(g),
						   #\(' '),
						   #\(a),
						   #\(c),
						   #\(t),
						   #\(i),
						   #\(o),
						   #\(n),
						   #\(' '),
						   #\(g),
						   #\(o),
						   #\(a),
						   #\(l),
						   #\(' '),
						   #\(~),
						   #\('A')
						 ]),
					u_ob
				      ],
				      
				      [ setq,
					u_mutated_actions,
					
					[ u_mutations,
					  [u_ob_c36_get, u_ob, [quote, u_obj]],
					  u_leaf
					]
				      ],
				      
				      [ u_yloop,
					
					[ u_yfor,
					  u_mutated_action,
					  u_in,
					  u_mutated_actions
					],
					
					[ u_ydo,
					  
					  [ if,
					    
					    [ u_action_mutation,
					      u_top_level_goal,
					      u_leaf,
					      u_mutated_action,
					      u_ob
					    ],
					    [setq, u_result, t]
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_action_mutations, classof, claz_function),
   set_opv(u_action_mutations, compile_as, kw_function),
   set_opv(u_action_mutations, function, f_u_action_mutations),
   DefunResult=u_action_mutations.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For now, only attempt mutations once for a given top-level goal.",
				     3,
				     842)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note that planning loops do not result in failed goals.",
				     11,
				     1498)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" They just leave unplanned active goals.",
				     11,
				     1566)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2191)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" See if a given action mutation pans out (via serendipity mechanism).",
				     1,
				     2193)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" First try serendipity from the supergoal of the action goal.",
				     1,
				     2264)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If that doesn't work, try serendipity from the top (well, actually",
				     1,
				     2327)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the subgoal of the daydreaming goal).",
				     1,
				     2396)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: we might also want to invoke serendipity for other tasks as well!",
				     1,
				     2436)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2510)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Returns T or NIL.", 1, 2512)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:783 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2532)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2533 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*action-mutations?*', []]).
:- set_var(TLEnv3, setq, u_xx_action_mutations_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'action-mutation',
			    
			    [ 'daydreaming-goal',
			      leaf,
			      'mutated-action',
			      'mutated-action-goal'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Trying mutated action ~A"),
			      'mutated-action'
			    ],
			    
			    [ 'unwind-protect',
			      
			      [ let,
				
				[ 
				  [ 'bottom-goal',
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'SUCCEEDED-GOAL',
					  obj,
					  ['#COMMA', 'mutated-action']
					]
				      ]
				    ]
				  ]
				],
				[setq, '*action-mutations?*', t],
				
				[ if,
				  
				  [ 'serendipity-recognize-apply',
				    'daydreaming-goal',
				    
				    [ 'goal-supergoal',
				      'mutated-action-goal',
				      leaf
				    ],
				    ['bottom-rules', 'mutated-action'],
				    'bottom-goal'
				  ],
				  t,
				  
				  [ let,
				    
				    [ 
				      [ subgoal,
					['dd-goal-subgoal', 'daydreaming-goal']
				      ]
				    ],
				    
				    [ if,
				      [],
				      
				      [ if,
					
					[ 'serendipity-recognize-apply',
					  'daydreaming-goal',
					  subgoal,
					  ['bottom-rules', 'mutated-action'],
					  'bottom-goal'
					],
					t,
					[]
				      ],
				      []
				    ]
				  ]
				]
			      ],
			      [setq, '*action-mutations?*', []]
			    ]
			  ]).

% annotating U::ACTION-MUTATION 
wl: lambda_def(defun,
	      u_action_mutation,
	      f_u_action_mutation,
	      
	      [ u_daydreaming_goal,
		u_leaf,
		u_mutated_action,
		u_mutated_action_goal
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('T'),
			     #\(r),
			     #\(y),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(m),
			     #\(u),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(d),
			     #\(' '),
			     #\(a),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_mutated_action
		],
		
		[ unwind_protect,
		  
		  [ let,
		    
		    [ 
		      [ u_bottom_goal,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_succeeded_goal,
			      u_obj,
			      ['#COMMA', u_mutated_action]
			    ]
			  ]
			]
		      ]
		    ],
		    [setq, u_xx_action_mutations_c63_xx, t],
		    
		    [ if,
		      
		      [ u_serendipity_recognize_apply,
			u_daydreaming_goal,
			[u_goal_supergoal, u_mutated_action_goal, u_leaf],
			[u_bottom_rules, u_mutated_action],
			u_bottom_goal
		      ],
		      t,
		      
		      [ let,
			[[u_subgoal, [u_dd_goal_subgoal, u_daydreaming_goal]]],
			
			[ if,
			  [],
			  
			  [ if,
			    
			    [ u_serendipity_recognize_apply,
			      u_daydreaming_goal,
			      u_subgoal,
			      [u_bottom_rules, u_mutated_action],
			      u_bottom_goal
			    ],
			    t,
			    []
			  ],
			  []
			]
		      ]
		    ]
		  ],
		  [setq, u_xx_action_mutations_c63_xx, []]
		]
	      ]).


% annotating U::ACTION-MUTATION 
wl: arglist_info(u_action_mutation,
		
		[ u_daydreaming_goal,
		  u_leaf,
		  u_mutated_action,
		  u_mutated_action_goal
		],
		
		[ Daydreaming_goal_Param,
		  Leaf_Param,
		  Mutated_action_Param,
		  Mutated_action_goal_Param
		],
		arginfo{ all:
			     [ u_daydreaming_goal,
			       u_leaf,
			       u_mutated_action,
			       u_mutated_action_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_daydreaming_goal,
				 u_leaf,
				 u_mutated_action,
				 u_mutated_action_goal
			       ],
			 opt:0,
			 req:
			     [ u_daydreaming_goal,
			       u_leaf,
			       u_mutated_action,
			       u_mutated_action_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ACTION-MUTATION 
wl: init_args(exact_only, u_action_mutation).


% annotating U::ACTION-MUTATION 
f_u_action_mutation(Daydreaming_goal_Param, Leaf_Param, Mutated_action_Param, Mutated_action_goal_Param, FnResult) :-
	Env=[bv(u_daydreaming_goal, Daydreaming_goal_Param), bv(u_leaf, Leaf_Param), bv(u_mutated_action, Mutated_action_Param), bv(u_mutated_action_goal, Mutated_action_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
				       #\(r),
				       #\(y),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(m),
				       #\(u),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_mutated_action
			  ],
			  Roman_nl_Ret),
	cl_unwind_protect(
			  [ let,
			    
			    [ 
			      [ u_bottom_goal,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_succeeded_goal,
				      u_obj,
				      ['#COMMA', u_mutated_action]
				    ]
				  ]
				]
			      ]
			    ],
			    [setq, u_xx_action_mutations_c63_xx, t],
			    
			    [ if,
			      
			      [ u_serendipity_recognize_apply,
				u_daydreaming_goal,
				
				[ u_goal_supergoal,
				  u_mutated_action_goal,
				  u_leaf
				],
				[u_bottom_rules, u_mutated_action],
				u_bottom_goal
			      ],
			      t,
			      
			      [ let,
				
				[ 
				  [ u_subgoal,
				    [u_dd_goal_subgoal, u_daydreaming_goal]
				  ]
				],
				
				[ if,
				  [],
				  
				  [ if,
				    
				    [ u_serendipity_recognize_apply,
				      u_daydreaming_goal,
				      u_subgoal,
				      [u_bottom_rules, u_mutated_action],
				      u_bottom_goal
				    ],
				    t,
				    []
				  ],
				  []
				]
			      ]
			    ]
			  ],
			  [setq, u_xx_action_mutations_c63_xx, []],
			  Unwind_protect_Ret),
	Unwind_protect_Ret=FnResult.
:- set_opv(f_u_action_mutation, classof, claz_function),
   set_opv(u_action_mutation, compile_as, kw_function),
   set_opv(u_action_mutation, function, f_u_action_mutation),
   DefunResult=u_action_mutation.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" was subgoal", 20, 3161)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3491)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" What follows is code that is not currently being used.",
				     1,
				     3493)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3550)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3553)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Action mutation generation", 1, 3555)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:2565 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3584)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:3586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'type-mutations',
			    [action],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Find type mutations for ~A"),
			      action
			    ],
			    
			    [ let,
			      
			      [ [mutation1, ['ob$copy', action]],
				[mutation2, ['ob$copy', action]]
			      ],
			      
			      [ cond,
				
				[ ['ty$instance?', action, [quote, ptrans]],
				  
				  [ 'ob$set',
				    mutation1,
				    [quote, type],
				    '*mtrans-ob*'
				  ],
				  
				  [ 'ob$set',
				    mutation2,
				    [quote, type],
				    '*atrans-ob*'
				  ],
				  [list, action, mutation1, mutation2]
				],
				
				[ ['ty$instance?', action, [quote, mtrans]],
				  
				  [ 'ob$set',
				    mutation1,
				    [quote, type],
				    '*ptrans-ob*'
				  ],
				  
				  [ 'ob$set',
				    mutation2,
				    [quote, type],
				    '*atrans-ob*'
				  ],
				  [list, action, mutation1, mutation2]
				],
				
				[ ['ty$instance?', action, [quote, atrans]],
				  
				  [ 'ob$set',
				    mutation1,
				    [quote, type],
				    '*mtrans-ob*'
				  ],
				  
				  [ 'ob$set',
				    mutation2,
				    [quote, type],
				    '*ptrans-ob*'
				  ],
				  [list, action, mutation1, mutation2]
				],
				[else, []]
			      ]
			    ]
			  ]).

% annotating U::TYPE-MUTATIONS 
wl: lambda_def(defun,
	      u_type_mutations,
	      f_u_type_mutations,
	      [u_action],
	      
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
			     #\(t),
			     #\(y),
			     #\(p),
			     #\(e),
			     #\(' '),
			     #\(m),
			     #\(u),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_action
		],
		
		[ let,
		  
		  [ [u_mutation1, [u_ob_c36_copy, u_action]],
		    [u_mutation2, [u_ob_c36_copy, u_action]]
		  ],
		  
		  [ cond,
		    
		    [ [u_ty_c36_instance_c63, u_action, [quote, u_ptrans]],
		      
		      [ u_ob_c36_set,
			u_mutation1,
			[quote, type],
			u_xx_mtrans_ob_xx
		      ],
		      
		      [ u_ob_c36_set,
			u_mutation2,
			[quote, type],
			u_xx_atrans_ob_xx
		      ],
		      [list, u_action, u_mutation1, u_mutation2]
		    ],
		    
		    [ [u_ty_c36_instance_c63, u_action, [quote, u_mtrans]],
		      
		      [ u_ob_c36_set,
			u_mutation1,
			[quote, type],
			u_xx_ptrans_ob_xx
		      ],
		      
		      [ u_ob_c36_set,
			u_mutation2,
			[quote, type],
			u_xx_atrans_ob_xx
		      ],
		      [list, u_action, u_mutation1, u_mutation2]
		    ],
		    
		    [ [u_ty_c36_instance_c63, u_action, [quote, u_atrans]],
		      
		      [ u_ob_c36_set,
			u_mutation1,
			[quote, type],
			u_xx_mtrans_ob_xx
		      ],
		      
		      [ u_ob_c36_set,
			u_mutation2,
			[quote, type],
			u_xx_ptrans_ob_xx
		      ],
		      [list, u_action, u_mutation1, u_mutation2]
		    ],
		    [u_else, []]
		  ]
		]
	      ]).


% annotating U::TYPE-MUTATIONS 
wl: arglist_info(u_type_mutations,
		[u_action],
		[Action_Param],
		arginfo{ all:[u_action],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action],
			 opt:0,
			 req:[u_action],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TYPE-MUTATIONS 
wl: init_args(exact_only, u_type_mutations).


% annotating U::TYPE-MUTATIONS 
f_u_type_mutations(Action_Param, ElseResult54) :-
	Env=[bv(u_action, Action_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('F'),
				       #\(i),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(t),
				       #\(y),
				       #\(p),
				       #\(e),
				       #\(' '),
				       #\(m),
				       #\(u),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_action
			  ],
			  Roman_nl_Ret),
	f_u_ob_c36_copy(Action_Param, Mutation1_Init),
	f_u_ob_c36_copy(Action_Param, Mutation2_Init),
	LEnv=[[bv(u_mutation1, Mutation1_Init), bv(u_mutation2, Mutation2_Init)]|Env],
	f_u_ty_c36_instance_c63(Action_Param, u_ptrans, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_mutation1, Mutation1_Get),
	    get_var(LEnv, u_xx_mtrans_ob_xx, Xx_mtrans_ob_xx_Get),
	    f_u_ob_c36_set(Mutation1_Get, type, Xx_mtrans_ob_xx_Get, C36_set_Ret),
	    get_var(LEnv, u_mutation2, Mutation2_Get),
	    get_var(LEnv, u_xx_atrans_ob_xx, Xx_atrans_ob_xx_Get),
	    f_u_ob_c36_set(Mutation2_Get,
			   type,
			   Xx_atrans_ob_xx_Get,
			   C36_set_Ret63),
	    get_var(LEnv, u_mutation1, Mutation1_Get27),
	    get_var(LEnv, u_mutation2, Mutation2_Get28),
	    ElseResult54=[Action_Param, Mutation1_Get27, Mutation2_Get28]
	;   f_u_ty_c36_instance_c63(Action_Param, u_mtrans, IFTEST29),
	    (   IFTEST29\==[]
	    ->  get_var(LEnv, u_mutation1, Mutation1_Get32),
		get_var(LEnv, u_xx_ptrans_ob_xx, Xx_ptrans_ob_xx_Get),
		f_u_ob_c36_set(Mutation1_Get32,
			       type,
			       Xx_ptrans_ob_xx_Get,
			       C36_set_Ret64),
		get_var(LEnv, u_mutation2, Mutation2_Get34),
		get_var(LEnv, u_xx_atrans_ob_xx, Xx_atrans_ob_xx_Get35),
		f_u_ob_c36_set(Mutation2_Get34,
			       type,
			       Xx_atrans_ob_xx_Get35,
			       C36_set_Ret65),
		get_var(LEnv, u_mutation1, Mutation1_Get37),
		get_var(LEnv, u_mutation2, Mutation2_Get38),
		ElseResult54=[Action_Param, Mutation1_Get37, Mutation2_Get38]
	    ;   f_u_ty_c36_instance_c63(Action_Param, u_atrans, IFTEST39),
		(   IFTEST39\==[]
		->  get_var(LEnv, u_mutation1, Mutation1_Get42),
		    get_var(LEnv, u_xx_mtrans_ob_xx, Xx_mtrans_ob_xx_Get43),
		    f_u_ob_c36_set(Mutation1_Get42,
				   type,
				   Xx_mtrans_ob_xx_Get43,
				   C36_set_Ret66),
		    get_var(LEnv, u_mutation2, Mutation2_Get44),
		    get_var(LEnv, u_xx_ptrans_ob_xx, Xx_ptrans_ob_xx_Get45),
		    f_u_ob_c36_set(Mutation2_Get44,
				   type,
				   Xx_ptrans_ob_xx_Get45,
				   C36_set_Ret67),
		    get_var(LEnv, u_mutation1, Mutation1_Get47),
		    get_var(LEnv, u_mutation2, Mutation2_Get48),
		    ElseResult54=[Action_Param, Mutation1_Get47, Mutation2_Get48]
		;   get_var(LEnv, u_else, IFTEST49),
		    (   IFTEST49\==[]
		    ->  ElseResult54=[]
		    ;   ElseResult54=[]
		    )
		)
	    )
	).
:- set_opv(f_u_type_mutations, classof, claz_function),
   set_opv(u_type_mutations, compile_as, kw_function),
   set_opv(u_type_mutations, function, f_u_type_mutations),
   DefunResult=u_type_mutations.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:3586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" need to copy w/o links", 38, 3760)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:4275 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'normalize-action!',
			    [action, context],
			    
			    [ cond,
			      
			      [ ['ty$instance?', action, [quote, ptrans]],
				['normalize-ptrans!', action, context]
			      ],
			      
			      [ ['ty$instance?', action, [quote, mtrans]],
				['normalize-mtrans!', action, context]
			      ],
			      
			      [ ['ty$instance?', action, [quote, atrans]],
				['normalize-atrans!', action, context]
			      ],
			      [else, action]
			    ]
			  ]).

% annotating U::NORMALIZE-ACTION! 
wl: lambda_def(defun,
	      u_normalize_action_c33,
	      f_u_normalize_action_c33,
	      [u_action, u_context],
	      
	      [ 
		[ cond,
		  
		  [ [u_ty_c36_instance_c63, u_action, [quote, u_ptrans]],
		    [u_normalize_ptrans_c33, u_action, u_context]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_action, [quote, u_mtrans]],
		    [u_normalize_mtrans_c33, u_action, u_context]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_action, [quote, u_atrans]],
		    [u_normalize_atrans_c33, u_action, u_context]
		  ],
		  [u_else, u_action]
		]
	      ]).


% annotating U::NORMALIZE-ACTION! 
wl: arglist_info(u_normalize_action_c33,
		[u_action, u_context],
		[Action_Get32, Context_Param],
		arginfo{ all:[u_action, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action, u_context],
			 opt:0,
			 req:[u_action, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NORMALIZE-ACTION! 
wl: init_args(exact_only, u_normalize_action_c33).


% annotating U::NORMALIZE-ACTION! 
f_u_normalize_action_c33(Action_Get32, Context_Param, ElseResult36) :-
	Env=[bv(u_action, Action_Get32), bv(u_context, Context_Param)],
	f_u_ty_c36_instance_c63(Action_Get32, u_ptrans, IFTEST),
	(   IFTEST\==[]
	->  f_u_normalize_ptrans_c33(Action_Get32, Context_Param, TrueResult39),
	    ElseResult36=TrueResult39
	;   f_u_ty_c36_instance_c63(Action_Get32, u_mtrans, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_u_normalize_mtrans_c33(Action_Get32,
					 Context_Param,
					 TrueResult37),
		ElseResult36=TrueResult37
	    ;   f_u_ty_c36_instance_c63(Action_Get32, u_atrans, IFTEST24),
		(   IFTEST24\==[]
		->  f_u_normalize_atrans_c33(Action_Get32,
					     Context_Param,
					     TrueResult35),
		    ElseResult36=TrueResult35
		;   get_var(Env, u_else, IFTEST29),
		    (   IFTEST29\==[]
		    ->  ElseResult36=Action_Get32
		    ;   ElseResult36=[]
		    )
		)
	    )
	).
:- set_opv(f_u_normalize_action_c33, classof, claz_function),
   set_opv(u_normalize_action_c33, compile_as, kw_function),
   set_opv(u_normalize_action_c33, function, f_u_normalize_action_c33),
   DefunResult=u_normalize_action_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:4567 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'normalize-ptrans!',
			    [action, context],
			    
			    [ let,
			      
			      [ [from, ['ob$get', action, [quote, from]]],
				[to, ['ob$get', action, [quote, to]]],
				[obj, ['ob$get', action, [quote, obj]]]
			      ],
			      
			      [ cond,
				
				[ ['eq?', from, [quote, 'some-object']],
				  ['ob$set', action, from, '*location-var*']
				],
				
				[ 
				  [ and,
				    from,
				    
				    [ not,
				      ['ty$instance?', from, [quote, location]]
				    ],
				    ['ty$instance?', from, [quote, person]]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, from],
				    ['object->location', from, context]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['eq?', to, [quote, 'some-object']],
				  ['ob$set', action, to, '*location-var*']
				],
				
				[ 
				  [ and,
				    to,
				    [not, ['ty$instance?', to, [quote, location]]],
				    ['ty$instance?', to, [quote, person]]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, to],
				    ['object->location', to, context]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['eq?', obj, [quote, 'some-object']],
				  ['ob$set', action, to, '*phys-obj-var*']
				],
				
				[ 
				  [ and,
				    obj,
				    
				    [ not,
				      ['ty$instance?', obj, [quote, 'phys-obj']]
				    ],
				    ['ty$instance?', obj, [quote, 'mental-obj']]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, obj],
				    
				    [ 'ob$fcreate',
				      ['#BQ', ['PHYS-OBJ', obj, ['#COMMA', obj]]]
				    ]
				  ]
				]
			      ],
			      action
			    ]
			  ]).

% annotating U::NORMALIZE-PTRANS! 
wl: lambda_def(defun,
	      u_normalize_ptrans_c33,
	      f_u_normalize_ptrans_c33,
	      [u_action, u_context],
	      
	      [ 
		[ let,
		  
		  [ [u_from, [u_ob_c36_get, u_action, [quote, u_from]]],
		    [u_to, [u_ob_c36_get, u_action, [quote, u_to]]],
		    [u_obj, [u_ob_c36_get, u_action, [quote, u_obj]]]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_from, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_from, u_xx_location_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_from,
			
			[ not,
			  [u_ty_c36_instance_c63, u_from, [quote, u_location]]
			],
			[u_ty_c36_instance_c63, u_from, [quote, u_person]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_from],
			[u_object_c62_location, u_from, u_context]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_to, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_to, u_xx_location_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_to,
			[not, [u_ty_c36_instance_c63, u_to, [quote, u_location]]],
			[u_ty_c36_instance_c63, u_to, [quote, u_person]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_to],
			[u_object_c62_location, u_to, u_context]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_obj, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_to, u_xx_phys_obj_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_obj,
			[not, [u_ty_c36_instance_c63, u_obj, [quote, u_phys_obj]]],
			[u_ty_c36_instance_c63, u_obj, [quote, u_mental_obj]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_obj],
			
			[ u_ob_c36_fcreate,
			  ['#BQ', [u_phys_obj, u_obj, ['#COMMA', u_obj]]]
			]
		      ]
		    ]
		  ],
		  u_action
		]
	      ]).


% annotating U::NORMALIZE-PTRANS! 
wl: arglist_info(u_normalize_ptrans_c33,
		[u_action, u_context],
		[Action_Param, Context_Param],
		arginfo{ all:[u_action, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action, u_context],
			 opt:0,
			 req:[u_action, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NORMALIZE-PTRANS! 
wl: init_args(exact_only, u_normalize_ptrans_c33).


% annotating U::NORMALIZE-PTRANS! 
f_u_normalize_ptrans_c33(Action_Param, Context_Param, Action_Param) :-
	Env=[bv(u_action, Action_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_get(Action_Param, u_from, From_Init),
	f_u_ob_c36_get(Action_Param, u_to, To_Init),
	f_u_ob_c36_get(Action_Param, u_obj, Obj_Init),
	LEnv=[[bv(u_from, From_Init), bv(u_to, To_Init), bv(u_obj, Obj_Init)]|Env],
	f_u_eq_c63(u_from, [quote, u_some_object], IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_from, From_Get),
	    get_var(LEnv, u_xx_location_var_xx, Xx_location_var_xx_Get),
	    f_u_ob_c36_set(Action_Param,
			   From_Get,
			   Xx_location_var_xx_Get,
			   TrueResult45),
	    ElseResult46=TrueResult45
	;   get_var(LEnv, u_from, IFTEST30),
	    (   IFTEST30\==[]
	    ->  get_var(LEnv, u_from, From_Get34),
		f_u_ty_c36_instance_c63(From_Get34, u_location, PredArgResult),
		(   PredArgResult==[]
		->  get_var(LEnv, u_from, From_Get37),
		    f_u_ty_c36_instance_c63(From_Get37, u_person, TrueResult),
		    IFTEST28=TrueResult
		;   IFTEST28=[]
		)
	    ;   IFTEST28=[]
	    ),
	    (   IFTEST28\==[]
	    ->  get_var(LEnv, u_from, From_Get41),
		f_u_object_c62_location(From_Get41, Context_Param, From),
		f_u_ob_c36_set(Action_Param, u_from, From, TrueResult43),
		ElseResult46=TrueResult43
	    ;   ElseResult46=[]
	    )
	),
	f_u_eq_c63(u_to, [quote, u_some_object], IFTEST47),
	(   IFTEST47\==[]
	->  get_var(LEnv, u_to, To_Get),
	    get_var(LEnv, u_xx_location_var_xx, Xx_location_var_xx_Get51),
	    f_u_ob_c36_set(Action_Param,
			   To_Get,
			   Xx_location_var_xx_Get51,
			   TrueResult69),
	    ElseResult70=TrueResult69
	;   get_var(LEnv, u_to, IFTEST54),
	    (   IFTEST54\==[]
	    ->  get_var(LEnv, u_to, To_Get58),
		f_u_ty_c36_instance_c63(To_Get58, u_location, PredArgResult60),
		(   PredArgResult60==[]
		->  get_var(LEnv, u_to, To_Get61),
		    f_u_ty_c36_instance_c63(To_Get61, u_person, TrueResult62),
		    IFTEST52=TrueResult62
		;   IFTEST52=[]
		)
	    ;   IFTEST52=[]
	    ),
	    (   IFTEST52\==[]
	    ->  get_var(LEnv, u_to, To_Get65),
		f_u_object_c62_location(To_Get65, Context_Param, To),
		f_u_ob_c36_set(Action_Param, u_to, To, TrueResult67),
		ElseResult70=TrueResult67
	    ;   ElseResult70=[]
	    )
	),
	f_u_eq_c63(u_obj, [quote, u_some_object], IFTEST71),
	(   IFTEST71\==[]
	->  get_var(LEnv, u_to, To_Get74),
	    get_var(LEnv, u_xx_phys_obj_var_xx, Xx_phys_obj_var_xx_Get),
	    f_u_ob_c36_set(Action_Param,
			   To_Get74,
			   Xx_phys_obj_var_xx_Get,
			   TrueResult91),
	    ElseResult92=TrueResult91
	;   get_var(LEnv, u_obj, IFTEST78),
	    (   IFTEST78\==[]
	    ->  get_var(LEnv, u_obj, Obj_Get82),
		f_u_ty_c36_instance_c63(Obj_Get82, u_phys_obj, PredArgResult84),
		(   PredArgResult84==[]
		->  get_var(LEnv, u_obj, Obj_Get85),
		    f_u_ty_c36_instance_c63(Obj_Get85,
					    u_mental_obj,
					    TrueResult86),
		    IFTEST76=TrueResult86
		;   IFTEST76=[]
		)
	    ;   IFTEST76=[]
	    ),
	    (   IFTEST76\==[]
	    ->  f_u_ob_c36_fcreate(['#BQ', [u_phys_obj, u_obj, ['#COMMA', u_obj]]],
				   Obj),
		f_u_ob_c36_set(Action_Param, u_obj, Obj, TrueResult89),
		ElseResult92=TrueResult89
	    ;   ElseResult92=[]
	    )
	).
:- set_opv(f_u_normalize_ptrans_c33, classof, claz_function),
   set_opv(u_normalize_ptrans_c33, compile_as, kw_function),
   set_opv(u_normalize_ptrans_c33, function, f_u_normalize_ptrans_c33),
   DefunResult=u_normalize_ptrans_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:5502 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'normalize-mtrans!',
			    [action, context],
			    
			    [ let,
			      
			      [ [from, ['ob$get', action, [quote, from]]],
				[to, ['ob$get', action, [quote, to]]],
				[obj, ['ob$get', action, [quote, obj]]]
			      ],
			      
			      [ cond,
				
				[ ['eq?', from, [quote, 'some-object']],
				  ['ob$set', action, from, '*person-var*']
				],
				
				[ 
				  [ and,
				    from,
				    [not, ['ty$instance?', from, [quote, person]]],
				    ['ty$instance?', from, [quote, location]]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, from],
				    ['location->object', from, context]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['eq?', to, [quote, 'some-object']],
				  ['ob$set', action, to, '*person-var*']
				],
				
				[ 
				  [ and,
				    to,
				    [not, ['ty$instance?', to, [quote, person]]],
				    ['ty$instance?', to, [quote, location]]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, to],
				    ['location->object', to, context]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['eq?', obj, [quote, 'some-object']],
				  ['ob$set', action, to, '*mental-obj-var*']
				],
				
				[ 
				  [ and,
				    obj,
				    
				    [ not,
				      
				      [ 'ty$instance?',
					obj,
					[quote, 'mental-obj']
				      ]
				    ],
				    ['ty$instance?', obj, [quote, 'phys-obj']]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, obj],
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					['MENTAL-OBJ', obj, ['#COMMA', obj]]
				      ]
				    ]
				  ]
				]
			      ],
			      action
			    ]
			  ]).

% annotating U::NORMALIZE-MTRANS! 
wl: lambda_def(defun,
	      u_normalize_mtrans_c33,
	      f_u_normalize_mtrans_c33,
	      [u_action, u_context],
	      
	      [ 
		[ let,
		  
		  [ [u_from, [u_ob_c36_get, u_action, [quote, u_from]]],
		    [u_to, [u_ob_c36_get, u_action, [quote, u_to]]],
		    [u_obj, [u_ob_c36_get, u_action, [quote, u_obj]]]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_from, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_from, u_xx_person_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_from,
			[not, [u_ty_c36_instance_c63, u_from, [quote, u_person]]],
			[u_ty_c36_instance_c63, u_from, [quote, u_location]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_from],
			[u_location_c62_object, u_from, u_context]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_to, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_to, u_xx_person_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_to,
			[not, [u_ty_c36_instance_c63, u_to, [quote, u_person]]],
			[u_ty_c36_instance_c63, u_to, [quote, u_location]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_to],
			[u_location_c62_object, u_to, u_context]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_obj, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_to, u_xx_mental_obj_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_obj,
			
			[ not,
			  [u_ty_c36_instance_c63, u_obj, [quote, u_mental_obj]]
			],
			[u_ty_c36_instance_c63, u_obj, [quote, u_phys_obj]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_obj],
			
			[ u_ob_c36_fcreate,
			  ['#BQ', [u_mental_obj, u_obj, ['#COMMA', u_obj]]]
			]
		      ]
		    ]
		  ],
		  u_action
		]
	      ]).


% annotating U::NORMALIZE-MTRANS! 
wl: arglist_info(u_normalize_mtrans_c33,
		[u_action, u_context],
		[Action_Param, Context_Param],
		arginfo{ all:[u_action, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action, u_context],
			 opt:0,
			 req:[u_action, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NORMALIZE-MTRANS! 
wl: init_args(exact_only, u_normalize_mtrans_c33).


% annotating U::NORMALIZE-MTRANS! 
f_u_normalize_mtrans_c33(Action_Param, Context_Param, Action_Param) :-
	Env=[bv(u_action, Action_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_get(Action_Param, u_from, From_Init),
	f_u_ob_c36_get(Action_Param, u_to, To_Init),
	f_u_ob_c36_get(Action_Param, u_obj, Obj_Init),
	LEnv=[[bv(u_from, From_Init), bv(u_to, To_Init), bv(u_obj, Obj_Init)]|Env],
	f_u_eq_c63(u_from, [quote, u_some_object], IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_from, From_Get),
	    get_var(LEnv, u_xx_person_var_xx, Xx_person_var_xx_Get),
	    f_u_ob_c36_set(Action_Param,
			   From_Get,
			   Xx_person_var_xx_Get,
			   TrueResult45),
	    ElseResult46=TrueResult45
	;   get_var(LEnv, u_from, IFTEST30),
	    (   IFTEST30\==[]
	    ->  get_var(LEnv, u_from, From_Get34),
		f_u_ty_c36_instance_c63(From_Get34, u_person, PredArgResult),
		(   PredArgResult==[]
		->  get_var(LEnv, u_from, From_Get37),
		    f_u_ty_c36_instance_c63(From_Get37, u_location, TrueResult),
		    IFTEST28=TrueResult
		;   IFTEST28=[]
		)
	    ;   IFTEST28=[]
	    ),
	    (   IFTEST28\==[]
	    ->  get_var(LEnv, u_from, From_Get41),
		f_u_location_c62_object(From_Get41, Context_Param, From),
		f_u_ob_c36_set(Action_Param, u_from, From, TrueResult43),
		ElseResult46=TrueResult43
	    ;   ElseResult46=[]
	    )
	),
	f_u_eq_c63(u_to, [quote, u_some_object], IFTEST47),
	(   IFTEST47\==[]
	->  get_var(LEnv, u_to, To_Get),
	    get_var(LEnv, u_xx_person_var_xx, Xx_person_var_xx_Get51),
	    f_u_ob_c36_set(Action_Param,
			   To_Get,
			   Xx_person_var_xx_Get51,
			   TrueResult69),
	    ElseResult70=TrueResult69
	;   get_var(LEnv, u_to, IFTEST54),
	    (   IFTEST54\==[]
	    ->  get_var(LEnv, u_to, To_Get58),
		f_u_ty_c36_instance_c63(To_Get58, u_person, PredArgResult60),
		(   PredArgResult60==[]
		->  get_var(LEnv, u_to, To_Get61),
		    f_u_ty_c36_instance_c63(To_Get61, u_location, TrueResult62),
		    IFTEST52=TrueResult62
		;   IFTEST52=[]
		)
	    ;   IFTEST52=[]
	    ),
	    (   IFTEST52\==[]
	    ->  get_var(LEnv, u_to, To_Get65),
		f_u_location_c62_object(To_Get65, Context_Param, To),
		f_u_ob_c36_set(Action_Param, u_to, To, TrueResult67),
		ElseResult70=TrueResult67
	    ;   ElseResult70=[]
	    )
	),
	f_u_eq_c63(u_obj, [quote, u_some_object], IFTEST71),
	(   IFTEST71\==[]
	->  get_var(LEnv, u_to, To_Get74),
	    get_var(LEnv, u_xx_mental_obj_var_xx, Xx_mental_obj_var_xx_Get),
	    f_u_ob_c36_set(Action_Param,
			   To_Get74,
			   Xx_mental_obj_var_xx_Get,
			   TrueResult91),
	    ElseResult92=TrueResult91
	;   get_var(LEnv, u_obj, IFTEST78),
	    (   IFTEST78\==[]
	    ->  get_var(LEnv, u_obj, Obj_Get82),
		f_u_ty_c36_instance_c63(Obj_Get82,
					u_mental_obj,
					PredArgResult84),
		(   PredArgResult84==[]
		->  get_var(LEnv, u_obj, Obj_Get85),
		    f_u_ty_c36_instance_c63(Obj_Get85, u_phys_obj, TrueResult86),
		    IFTEST76=TrueResult86
		;   IFTEST76=[]
		)
	    ;   IFTEST76=[]
	    ),
	    (   IFTEST76\==[]
	    ->  f_u_ob_c36_fcreate(
				   [ '#BQ',
				     [u_mental_obj, u_obj, ['#COMMA', u_obj]]
				   ],
				   Obj),
		f_u_ob_c36_set(Action_Param, u_obj, Obj, TrueResult89),
		ElseResult92=TrueResult89
	    ;   ElseResult92=[]
	    )
	).
:- set_opv(f_u_normalize_mtrans_c33, classof, claz_function),
   set_opv(u_normalize_mtrans_c33, compile_as, kw_function),
   set_opv(u_normalize_mtrans_c33, function, f_u_normalize_mtrans_c33),
   DefunResult=u_normalize_mtrans_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:6447 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'normalize-atrans!',
			    [action, context],
			    
			    [ let,
			      
			      [ [from, ['ob$get', action, [quote, from]]],
				[to, ['ob$get', action, [quote, to]]],
				[obj, ['ob$get', action, [quote, obj]]]
			      ],
			      
			      [ cond,
				
				[ ['eq?', from, [quote, 'some-object']],
				  ['ob$set', action, from, '*person-var*']
				],
				
				[ 
				  [ and,
				    from,
				    [not, ['ty$instance?', from, [quote, person]]],
				    ['ty$instance?', from, [quote, location]]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, from],
				    ['location->object', from, context]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['eq?', to, [quote, 'some-object']],
				  ['ob$set', action, to, '*person-var*']
				],
				
				[ 
				  [ and,
				    to,
				    [not, ['ty$instance?', to, [quote, person]]],
				    ['ty$instance?', to, [quote, location]]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, to],
				    ['location->object', to, context]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['eq?', obj, [quote, 'some-object']],
				  ['ob$set', action, to, '*phys-obj-var*']
				],
				
				[ 
				  [ and,
				    obj,
				    
				    [ not,
				      ['ty$instance?', obj, [quote, 'phys-obj']]
				    ],
				    ['ty$instance?', obj, [quote, 'mental-obj']]
				  ],
				  
				  [ 'ob$set',
				    action,
				    [quote, obj],
				    
				    [ 'ob$fcreate',
				      ['#BQ', ['PHYS-OBJ', obj, ['#COMMA', obj]]]
				    ]
				  ]
				]
			      ],
			      action
			    ]
			  ]).

% annotating U::NORMALIZE-ATRANS! 
wl: lambda_def(defun,
	      u_normalize_atrans_c33,
	      f_u_normalize_atrans_c33,
	      [u_action, u_context],
	      
	      [ 
		[ let,
		  
		  [ [u_from, [u_ob_c36_get, u_action, [quote, u_from]]],
		    [u_to, [u_ob_c36_get, u_action, [quote, u_to]]],
		    [u_obj, [u_ob_c36_get, u_action, [quote, u_obj]]]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_from, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_from, u_xx_person_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_from,
			[not, [u_ty_c36_instance_c63, u_from, [quote, u_person]]],
			[u_ty_c36_instance_c63, u_from, [quote, u_location]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_from],
			[u_location_c62_object, u_from, u_context]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_to, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_to, u_xx_person_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_to,
			[not, [u_ty_c36_instance_c63, u_to, [quote, u_person]]],
			[u_ty_c36_instance_c63, u_to, [quote, u_location]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_to],
			[u_location_c62_object, u_to, u_context]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_eq_c63, u_obj, [quote, u_some_object]],
		      [u_ob_c36_set, u_action, u_to, u_xx_phys_obj_var_xx]
		    ],
		    
		    [ 
		      [ and,
			u_obj,
			[not, [u_ty_c36_instance_c63, u_obj, [quote, u_phys_obj]]],
			[u_ty_c36_instance_c63, u_obj, [quote, u_mental_obj]]
		      ],
		      
		      [ u_ob_c36_set,
			u_action,
			[quote, u_obj],
			
			[ u_ob_c36_fcreate,
			  ['#BQ', [u_phys_obj, u_obj, ['#COMMA', u_obj]]]
			]
		      ]
		    ]
		  ],
		  u_action
		]
	      ]).


% annotating U::NORMALIZE-ATRANS! 
wl: arglist_info(u_normalize_atrans_c33,
		[u_action, u_context],
		[Action_Param, Context_Param],
		arginfo{ all:[u_action, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action, u_context],
			 opt:0,
			 req:[u_action, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NORMALIZE-ATRANS! 
wl: init_args(exact_only, u_normalize_atrans_c33).


% annotating U::NORMALIZE-ATRANS! 
f_u_normalize_atrans_c33(Action_Param, Context_Param, Action_Param) :-
	Env=[bv(u_action, Action_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_get(Action_Param, u_from, From_Init),
	f_u_ob_c36_get(Action_Param, u_to, To_Init),
	f_u_ob_c36_get(Action_Param, u_obj, Obj_Init),
	LEnv=[[bv(u_from, From_Init), bv(u_to, To_Init), bv(u_obj, Obj_Init)]|Env],
	f_u_eq_c63(u_from, [quote, u_some_object], IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_from, From_Get),
	    get_var(LEnv, u_xx_person_var_xx, Xx_person_var_xx_Get),
	    f_u_ob_c36_set(Action_Param,
			   From_Get,
			   Xx_person_var_xx_Get,
			   TrueResult45),
	    ElseResult46=TrueResult45
	;   get_var(LEnv, u_from, IFTEST30),
	    (   IFTEST30\==[]
	    ->  get_var(LEnv, u_from, From_Get34),
		f_u_ty_c36_instance_c63(From_Get34, u_person, PredArgResult),
		(   PredArgResult==[]
		->  get_var(LEnv, u_from, From_Get37),
		    f_u_ty_c36_instance_c63(From_Get37, u_location, TrueResult),
		    IFTEST28=TrueResult
		;   IFTEST28=[]
		)
	    ;   IFTEST28=[]
	    ),
	    (   IFTEST28\==[]
	    ->  get_var(LEnv, u_from, From_Get41),
		f_u_location_c62_object(From_Get41, Context_Param, From),
		f_u_ob_c36_set(Action_Param, u_from, From, TrueResult43),
		ElseResult46=TrueResult43
	    ;   ElseResult46=[]
	    )
	),
	f_u_eq_c63(u_to, [quote, u_some_object], IFTEST47),
	(   IFTEST47\==[]
	->  get_var(LEnv, u_to, To_Get),
	    get_var(LEnv, u_xx_person_var_xx, Xx_person_var_xx_Get51),
	    f_u_ob_c36_set(Action_Param,
			   To_Get,
			   Xx_person_var_xx_Get51,
			   TrueResult69),
	    ElseResult70=TrueResult69
	;   get_var(LEnv, u_to, IFTEST54),
	    (   IFTEST54\==[]
	    ->  get_var(LEnv, u_to, To_Get58),
		f_u_ty_c36_instance_c63(To_Get58, u_person, PredArgResult60),
		(   PredArgResult60==[]
		->  get_var(LEnv, u_to, To_Get61),
		    f_u_ty_c36_instance_c63(To_Get61, u_location, TrueResult62),
		    IFTEST52=TrueResult62
		;   IFTEST52=[]
		)
	    ;   IFTEST52=[]
	    ),
	    (   IFTEST52\==[]
	    ->  get_var(LEnv, u_to, To_Get65),
		f_u_location_c62_object(To_Get65, Context_Param, To),
		f_u_ob_c36_set(Action_Param, u_to, To, TrueResult67),
		ElseResult70=TrueResult67
	    ;   ElseResult70=[]
	    )
	),
	f_u_eq_c63(u_obj, [quote, u_some_object], IFTEST71),
	(   IFTEST71\==[]
	->  get_var(LEnv, u_to, To_Get74),
	    get_var(LEnv, u_xx_phys_obj_var_xx, Xx_phys_obj_var_xx_Get),
	    f_u_ob_c36_set(Action_Param,
			   To_Get74,
			   Xx_phys_obj_var_xx_Get,
			   TrueResult91),
	    ElseResult92=TrueResult91
	;   get_var(LEnv, u_obj, IFTEST78),
	    (   IFTEST78\==[]
	    ->  get_var(LEnv, u_obj, Obj_Get82),
		f_u_ty_c36_instance_c63(Obj_Get82, u_phys_obj, PredArgResult84),
		(   PredArgResult84==[]
		->  get_var(LEnv, u_obj, Obj_Get85),
		    f_u_ty_c36_instance_c63(Obj_Get85,
					    u_mental_obj,
					    TrueResult86),
		    IFTEST76=TrueResult86
		;   IFTEST76=[]
		)
	    ;   IFTEST76=[]
	    ),
	    (   IFTEST76\==[]
	    ->  f_u_ob_c36_fcreate(['#BQ', [u_phys_obj, u_obj, ['#COMMA', u_obj]]],
				   Obj),
		f_u_ob_c36_set(Action_Param, u_obj, Obj, TrueResult89),
		ElseResult92=TrueResult89
	    ;   ElseResult92=[]
	    )
	).
:- set_opv(f_u_normalize_atrans_c33, classof, claz_function),
   set_opv(u_normalize_atrans_c33, compile_as, kw_function),
   set_opv(u_normalize_atrans_c33, function, f_u_normalize_atrans_c33),
   DefunResult=u_normalize_atrans_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:6447 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" generates list of substitution binding lists for each possible permutation",
				     1,
				     7374)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:6447 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" of a list of objects", 1, 7451)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'permutation-substs',
			    [objs],
			    
			    [ yloop,
			      
			      [ initial,
				[result, []],
				[perms, ['permute-list', objs]]
			      ],
			      [yfor, perm, in, perms],
			      
			      [ ydo,
				
				[ yloop,
				  [initial, [bd, []]],
				  [yfor, elem1, in, objs],
				  [yfor, elem2, in, perm],
				  [ydo, [setq, bd, [cons, [list, elem1, elem2], bd]]],
				  
				  [ yresult,
				    
				    [ setq,
				      result,
				      [cons, [cons, [quote, t], bd], result]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::PERMUTATION-SUBSTS 
wl: lambda_def(defun,
	      u_permutation_substs,
	      f_u_permutation_substs,
	      [u_objs],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_perms, [u_permute_list, u_objs]]],
		  [u_yfor, u_perm, u_in, u_perms],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      [u_initial, [u_bd, []]],
		      [u_yfor, u_elem1, u_in, u_objs],
		      [u_yfor, u_elem2, u_in, u_perm],
		      [u_ydo, [setq, u_bd, [cons, [list, u_elem1, u_elem2], u_bd]]],
		      
		      [ u_yresult,
			[setq, u_result, [cons, [cons, [quote, t], u_bd], u_result]]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::PERMUTATION-SUBSTS 
wl: arglist_info(u_permutation_substs,
		[u_objs],
		[Objs_Param],
		arginfo{ all:[u_objs],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_objs],
			 opt:0,
			 req:[u_objs],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERMUTATION-SUBSTS 
wl: init_args(exact_only, u_permutation_substs).


% annotating U::PERMUTATION-SUBSTS 
f_u_permutation_substs(Objs_Param, FnResult) :-
	Env=[bv(u_objs, Objs_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, []],
		      [u_perms, [u_permute_list, u_objs]]
		    ],
		    [u_yfor, u_perm, u_in, u_perms],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_initial, [u_bd, []]],
			[u_yfor, u_elem1, u_in, u_objs],
			[u_yfor, u_elem2, u_in, u_perm],
			[u_ydo, [setq, u_bd, [cons, [list, u_elem1, u_elem2], u_bd]]],
			
			[ u_yresult,
			  
			  [ setq,
			    u_result,
			    [cons, [cons, [quote, t], u_bd], u_result]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_permutation_substs, classof, claz_function),
   set_opv(u_permutation_substs, compile_as, kw_function),
   set_opv(u_permutation_substs, function, f_u_permutation_substs),
   DefunResult=u_permutation_substs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:7907 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'permute-list',
			    [lst],
			    
			    [ cond,
			      [['null?', [cdr, lst]], [list, lst]],
			      
			      [ else,
				
				[ yloop,
				  [initial, [result1, []], [result2, []]],
				  [yfor, elem1, in, lst],
				  
				  [ ydo,
				    
				    [ setq,
				      result2,
				      ['permute-list', [delq, elem1, lst]]
				    ],
				    
				    [ yloop,
				      [yfor, elem2, in, result2],
				      
				      [ ydo,
					
					[ setq,
					  result1,
					  [cons, [cons, elem1, elem2], result1]
					]
				      ]
				    ]
				  ],
				  [yresult, result1]
				]
			      ]
			    ]
			  ]).

% annotating U::PERMUTE-LIST 
wl: lambda_def(defun,
	      u_permute_list,
	      f_u_permute_list,
	      [u_lst],
	      
	      [ 
		[ cond,
		  [[u_null_c63, [cdr, u_lst]], [list, u_lst]],
		  
		  [ u_else,
		    
		    [ u_yloop,
		      [u_initial, [u_result1, []], [u_result2, []]],
		      [u_yfor, u_elem1, u_in, u_lst],
		      
		      [ u_ydo,
			
			[ setq,
			  u_result2,
			  [u_permute_list, [u_delq, u_elem1, u_lst]]
			],
			
			[ u_yloop,
			  [u_yfor, u_elem2, u_in, u_result2],
			  
			  [ u_ydo,
			    
			    [ setq,
			      u_result1,
			      [cons, [cons, u_elem1, u_elem2], u_result1]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result1]
		    ]
		  ]
		]
	      ]).


% annotating U::PERMUTE-LIST 
wl: arglist_info(u_permute_list,
		[u_lst],
		[Lst_Param],
		arginfo{ all:[u_lst],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst],
			 opt:0,
			 req:[u_lst],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERMUTE-LIST 
wl: init_args(exact_only, u_permute_list).


% annotating U::PERMUTE-LIST 
f_u_permute_list(Lst_Param, ElseResult21) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_null_c63([cdr, u_lst], IFTEST),
	(   IFTEST\==[]
	->  ElseResult21=[Lst_Param]
	;   get_var(Env, u_else, IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [u_result1, []], [u_result2, []]],
			    [u_yfor, u_elem1, u_in, u_lst],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_result2,
				[u_permute_list, [u_delq, u_elem1, u_lst]]
			      ],
			      
			      [ u_yloop,
				[u_yfor, u_elem2, u_in, u_result2],
				
				[ u_ydo,
				  
				  [ setq,
				    u_result1,
				    [cons, [cons, u_elem1, u_elem2], u_result1]
				  ]
				]
			      ]
			    ],
			    [u_yresult, u_result1]
			  ],
			  TrueResult),
		ElseResult21=TrueResult
	    ;   ElseResult21=[]
	    )
	).
:- set_opv(f_u_permute_list, classof, claz_function),
   set_opv(u_permute_list, compile_as, kw_function),
   set_opv(u_permute_list, function, f_u_permute_list),
   DefunResult=u_permute_list.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:8381 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'permutation-mutations',
			    [action],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Find permutation mutations for ~A"),
			      action
			    ],
			    
			    [ yloop,
			      [initial, [result, []]],
			      
			      [ yfor,
				subst,
				in,
				
				[ cdr,
				  ['permutation-substs', ['objects-in', action]]
				]
			      ],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  
				  [ cons,
				    ['ob$subst', action, subst, [], [], []],
				    result
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::PERMUTATION-MUTATIONS 
wl: lambda_def(defun,
	      u_permutation_mutations,
	      f_u_permutation_mutations,
	      [u_action],
	      
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
			     #\(p),
			     #\(e),
			     #\(r),
			     #\(m),
			     #\(u),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(' '),
			     #\(m),
			     #\(u),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_action
		],
		
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  
		  [ u_yfor,
		    subst,
		    u_in,
		    [cdr, [u_permutation_substs, [u_objects_in, u_action]]]
		  ],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      [cons, [u_ob_c36_subst, u_action, subst, [], [], []], u_result]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::PERMUTATION-MUTATIONS 
wl: arglist_info(u_permutation_mutations,
		[u_action],
		[Action_Param],
		arginfo{ all:[u_action],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action],
			 opt:0,
			 req:[u_action],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERMUTATION-MUTATIONS 
wl: init_args(exact_only, u_permutation_mutations).


% annotating U::PERMUTATION-MUTATIONS 
f_u_permutation_mutations(Action_Param, FnResult) :-
	Env=[bv(u_action, Action_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('F'),
				       #\(i),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(p),
				       #\(e),
				       #\(r),
				       #\(m),
				       #\(u),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(m),
				       #\(u),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_action
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    
		    [ u_yfor,
		      subst,
		      u_in,
		      [cdr, [u_permutation_substs, [u_objects_in, u_action]]]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			
			[ cons,
			  [u_ob_c36_subst, u_action, subst, [], [], []],
			  u_result
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_permutation_mutations, classof, claz_function),
   set_opv(u_permutation_mutations, compile_as, kw_function),
   set_opv(u_permutation_mutations, function, f_u_permutation_mutations),
   DefunResult=u_permutation_mutations.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:8381 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" cdr is intended to remove the identity substitution--it",
				     9,
				     8607)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:8381 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" may end up as last, though.", 9, 8673)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:8808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'substitution-mutations',
			    [action],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Find substitution mutations for ~A"),
			      action
			    ],
			    
			    [ if,
			      ['ob$literal?', action],
			      [list, action],
			      
			      [ yloop,
				
				[ initial,
				  [result, [list, ['ob$create-empty']]],
				  [ob1, []],
				  [ob2, []],
				  [temp, []]
				],
				[yfor, sv, in, ['ob$pairs', action]],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'ty$instance?',
				      ['slots-value', sv],
				      [quote, object]
				    ],
				    
				    [ progn,
				      [setq, ob2, ['ob$copy', [tlast, result]]],
				      
				      [ yloop,
					[yfor, ob, in, result],
					
					[ ydo,
					  
					  [ 'ob$add',
					    ob,
					    ['slots-name', sv],
					    ['slots-value', sv]
					  ]
					]
				      ],
				      
				      [ 'ob$add',
					ob2,
					['slots-name', sv],
					[quote, 'some-object']
				      ],
				      [setq, result, [cons, ob2, result]]
				    ],
				    
				    [ progn,
				      
				      [ setq,
					temp,
					
					[ 'substitution-mutations',
					  ['slots-value', sv]
					]
				      ],
				      
				      [ yloop,
					[initial, ['new-result', []]],
					[yfor, ob1, in, temp],
					
					[ ydo,
					  
					  [ yloop,
					    [yfor, ob2, in, result],
					    
					    [ ydo,
					      [setq, ob2, ['ob$copy', ob2]],
					      
					      [ 'ob$add',
						ob2,
						['slots-name', sv],
						ob1
					      ],
					      
					      [ setq,
						'new-result',
						
						[ 'append!',
						  'new-result',
						  [list, ob2]
						]
					      ]
					    ]
					  ]
					],
					[yresult, [setq, result, 'new-result']]
				      ]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::SUBSTITUTION-MUTATIONS 
wl: lambda_def(defun,
	      u_substitution_mutations,
	      f_u_substitution_mutations,
	      [u_action],
	      
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
			     #\(s),
			     #\(u),
			     #\(b),
			     #\(s),
			     #\(t),
			     #\(i),
			     #\(t),
			     #\(u),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(' '),
			     #\(m),
			     #\(u),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_action
		],
		
		[ if,
		  [u_ob_c36_literal_c63, u_action],
		  [list, u_action],
		  
		  [ u_yloop,
		    
		    [ u_initial,
		      [u_result, [list, [u_ob_c36_create_empty]]],
		      [u_ob1, []],
		      [u_ob2, []],
		      [u_temp, []]
		    ],
		    [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_action]],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_ty_c36_instance_c63,
			  [u_slots_value, u_sv],
			  [quote, u_object]
			],
			
			[ progn,
			  [setq, u_ob2, [u_ob_c36_copy, [u_tlast, u_result]]],
			  
			  [ u_yloop,
			    [u_yfor, u_ob, u_in, u_result],
			    
			    [ u_ydo,
			      
			      [ u_ob_c36_add,
				u_ob,
				[u_slots_name, u_sv],
				[u_slots_value, u_sv]
			      ]
			    ]
			  ],
			  
			  [ u_ob_c36_add,
			    u_ob2,
			    [u_slots_name, u_sv],
			    [quote, u_some_object]
			  ],
			  [setq, u_result, [cons, u_ob2, u_result]]
			],
			
			[ progn,
			  
			  [ setq,
			    u_temp,
			    [u_substitution_mutations, [u_slots_value, u_sv]]
			  ],
			  
			  [ u_yloop,
			    [u_initial, [u_new_result, []]],
			    [u_yfor, u_ob1, u_in, u_temp],
			    
			    [ u_ydo,
			      
			      [ u_yloop,
				[u_yfor, u_ob2, u_in, u_result],
				
				[ u_ydo,
				  [setq, u_ob2, [u_ob_c36_copy, u_ob2]],
				  
				  [ u_ob_c36_add,
				    u_ob2,
				    [u_slots_name, u_sv],
				    u_ob1
				  ],
				  
				  [ setq,
				    u_new_result,
				    [u_append_c33, u_new_result, [list, u_ob2]]
				  ]
				]
			      ]
			    ],
			    [u_yresult, [setq, u_result, u_new_result]]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::SUBSTITUTION-MUTATIONS 
wl: arglist_info(u_substitution_mutations,
		[u_action],
		[Action_Param],
		arginfo{ all:[u_action],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_action],
			 opt:0,
			 req:[u_action],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBSTITUTION-MUTATIONS 
wl: init_args(exact_only, u_substitution_mutations).


% annotating U::SUBSTITUTION-MUTATIONS 
f_u_substitution_mutations(Action_Param, FnResult) :-
	Env=[bv(u_action, Action_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('F'),
				       #\(i),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(b),
				       #\(s),
				       #\(t),
				       #\(i),
				       #\(t),
				       #\(u),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(m),
				       #\(u),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_action
			  ],
			  Roman_nl_Ret),
	f_u_ob_c36_literal_c63(Action_Param, IFTEST),
	(   IFTEST\==[]
	->  FnResult=[Action_Param]
	;   f_u_yloop(
		      [ 
			[ u_initial,
			  [u_result, [list, [u_ob_c36_create_empty]]],
			  [u_ob1, []],
			  [u_ob2, []],
			  [u_temp, []]
			],
			[u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_action]],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_ty_c36_instance_c63,
			      [u_slots_value, u_sv],
			      [quote, u_object]
			    ],
			    
			    [ progn,
			      [setq, u_ob2, [u_ob_c36_copy, [u_tlast, u_result]]],
			      
			      [ u_yloop,
				[u_yfor, u_ob, u_in, u_result],
				
				[ u_ydo,
				  
				  [ u_ob_c36_add,
				    u_ob,
				    [u_slots_name, u_sv],
				    [u_slots_value, u_sv]
				  ]
				]
			      ],
			      
			      [ u_ob_c36_add,
				u_ob2,
				[u_slots_name, u_sv],
				[quote, u_some_object]
			      ],
			      [setq, u_result, [cons, u_ob2, u_result]]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_temp,
				
				[ u_substitution_mutations,
				  [u_slots_value, u_sv]
				]
			      ],
			      
			      [ u_yloop,
				[u_initial, [u_new_result, []]],
				[u_yfor, u_ob1, u_in, u_temp],
				
				[ u_ydo,
				  
				  [ u_yloop,
				    [u_yfor, u_ob2, u_in, u_result],
				    
				    [ u_ydo,
				      [setq, u_ob2, [u_ob_c36_copy, u_ob2]],
				      
				      [ u_ob_c36_add,
					u_ob2,
					[u_slots_name, u_sv],
					u_ob1
				      ],
				      
				      [ setq,
					u_new_result,
					
					[ u_append_c33,
					  u_new_result,
					  [list, u_ob2]
					]
				      ]
				    ]
				  ]
				],
				[u_yresult, [setq, u_result, u_new_result]]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_substitution_mutations, classof, claz_function),
   set_opv(u_substitution_mutations, compile_as, kw_function),
   set_opv(u_substitution_mutations, function, f_u_substitution_mutations),
   DefunResult=u_substitution_mutations.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:8808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" have to add other instan code to handle literal and type obs right?",
				     10,
				     9159)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:8808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (setq ob1 (ob$copy (tlast result)))",
				     1,
				     9312)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:8808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (ob$add ob1 (slots-name sv) *me*)",
				     1,
				     9546)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:10362 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*mutation-timeout*', 4]).
:- set_var(TLEnv3, setq, u_xx_mutation_timeout_xx, 4).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:10391 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'replan-mut',
			    [goal],
			    
			    [ let,
			      
			      [ 
				[ sprout,
				  
				  [ 'cx$sprout',
				    ['ob$get', goal, [quote, 'top-context']]
				  ]
				]
			      ],
			      ['ob$set', sprout, [quote, 'mutations-tried?'], t],
			      [list, sprout]
			    ]
			  ]).

% annotating U::REPLAN-MUT 
wl: lambda_def(defun,
	      u_replan_mut,
	      f_u_replan_mut,
	      [u_goal],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_sprout,
		      
		      [ u_cx_c36_sprout,
			[u_ob_c36_get, u_goal, [quote, u_top_context]]
		      ]
		    ]
		  ],
		  [u_ob_c36_set, u_sprout, [quote, u_mutations_tried_c63], t],
		  [list, u_sprout]
		]
	      ]).


% annotating U::REPLAN-MUT 
wl: arglist_info(u_replan_mut,
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

% annotating U::REPLAN-MUT 
wl: init_args(exact_only, u_replan_mut).


% annotating U::REPLAN-MUT 
f_u_replan_mut(Goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ob_c36_get(Goal_Param, u_top_context, Top_context),
	f_u_cx_c36_sprout(Top_context, Sprout_Init),
	LEnv=[[bv(u_sprout, Sprout_Init)]|Env],
	get_var(LEnv, u_sprout, Sprout_Get),
	f_u_ob_c36_set(Sprout_Get, u_mutations_tried_c63, t, T),
	get_var(LEnv, u_sprout, Sprout_Get18),
	LetResult=[Sprout_Get18],
	LetResult=FnResult.
:- set_opv(f_u_replan_mut, classof, claz_function),
   set_opv(u_replan_mut, compile_as, kw_function),
   set_opv(u_replan_mut, function, f_u_replan_mut),
   DefunResult=u_replan_mut.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:10540 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'redo-plans-with-mutations?',
			    ['top-level-goal', leaf],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Redo plans with mutations")
			    ],
			    
			    [ yloop,
			      [initial, ['sprouted-contexts', []]],
			      
			      [ yfor,
				ob,
				in,
				['cx$get-all-ty', leaf, '*active-goal-ob*']
			      ],
			      
			      [ ydo,
				
				[ if,
				  
				  [ 'eq?',
				    ['ob$get', ob, [quote, 'top-level-goal']],
				    'top-level-goal'
				  ],
				  
				  [ setq,
				    'sprouted-contexts',
				    
				    [ append,
				      
				      [ 'run-mutation-plans',
					ob,
					'top-level-goal',
					['ob$get', ob, [quote, 'top-context']]
				      ],
				      'sprouted-contexts'
				    ]
				  ]
				]
			      ],
			      [yresult, 'sprouted-contexts']
			    ]
			  ]).

% annotating U::REDO-PLANS-WITH-MUTATIONS? 
wl: lambda_def(defun,
	      u_redo_plans_with_mutations_c63,
	      f_u_redo_plans_with_mutations_c63,
	      [u_top_level_goal, u_leaf],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(e),
			     #\(d),
			     #\(o),
			     #\(' '),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(s),
			     #\(' '),
			     #\(w),
			     #\(i),
			     #\(t),
			     #\(h),
			     #\(' '),
			     #\(m),
			     #\(u),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(s)
			   ])
		],
		
		[ u_yloop,
		  [u_initial, [u_sprouted_contexts, []]],
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    [u_cx_c36_get_all_ty, u_leaf, u_xx_active_goal_ob_xx]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ u_eq_c63,
			[u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
			u_top_level_goal
		      ],
		      
		      [ setq,
			u_sprouted_contexts,
			
			[ append,
			  
			  [ u_run_mutation_plans,
			    u_ob,
			    u_top_level_goal,
			    [u_ob_c36_get, u_ob, [quote, u_top_context]]
			  ],
			  u_sprouted_contexts
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_sprouted_contexts]
		]
	      ]).


% annotating U::REDO-PLANS-WITH-MUTATIONS? 
wl: arglist_info(u_redo_plans_with_mutations_c63,
		[u_top_level_goal, u_leaf],
		[Top_level_goal_Param, Leaf_Param],
		arginfo{ all:[u_top_level_goal, u_leaf],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_leaf],
			 opt:0,
			 req:[u_top_level_goal, u_leaf],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REDO-PLANS-WITH-MUTATIONS? 
wl: init_args(exact_only, u_redo_plans_with_mutations_c63).


% annotating U::REDO-PLANS-WITH-MUTATIONS? 
f_u_redo_plans_with_mutations_c63(Top_level_goal_Param, Leaf_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_leaf, Leaf_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(d),
				       #\(o),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(s),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(m),
				       #\(u),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(s)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_sprouted_contexts, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      [u_cx_c36_get_all_ty, u_leaf, u_xx_active_goal_ob_xx]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_eq_c63,
			  [u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
			  u_top_level_goal
			],
			
			[ setq,
			  u_sprouted_contexts,
			  
			  [ append,
			    
			    [ u_run_mutation_plans,
			      u_ob,
			      u_top_level_goal,
			      [u_ob_c36_get, u_ob, [quote, u_top_context]]
			    ],
			    u_sprouted_contexts
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_redo_plans_with_mutations_c63, classof, claz_function),
   set_opv(u_redo_plans_with_mutations_c63, compile_as, kw_function),
   set_opv(u_redo_plans_with_mutations_c63,
	   function,
	   f_u_redo_plans_with_mutations_c63),
   DefunResult=u_redo_plans_with_mutations_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'mutation-result?',
			    [fact, context],
			    
			    [ 'ol-path',
			      fact,
			      [],
			      '*dependency-ob*',
			      [quote, backward],
			      context,
			      
			      [ lambda,
				[dummy, ob],
				['mutation-action?', ob, context]
			      ],
			      []
			    ]
			  ]).

% annotating U::MUTATION-RESULT? 
wl: lambda_def(defun,
	      u_mutation_result_c63,
	      f_u_mutation_result_c63,
	      [u_fact, u_context],
	      
	      [ 
		[ u_ol_path,
		  u_fact,
		  [],
		  u_xx_dependency_ob_xx,
		  [quote, u_backward],
		  u_context,
		  
		  [ lambda,
		    [u_dummy, u_ob],
		    [u_mutation_action_c63, u_ob, u_context]
		  ],
		  []
		]
	      ]).


% annotating U::MUTATION-RESULT? 
wl: arglist_info(u_mutation_result_c63,
		[u_fact, u_context],
		[Fact_Param, Context_Param],
		arginfo{ all:[u_fact, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fact, u_context],
			 opt:0,
			 req:[u_fact, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MUTATION-RESULT? 
wl: init_args(exact_only, u_mutation_result_c63).


% annotating U::MUTATION-RESULT? 
f_u_mutation_result_c63(Fact_Param, Context_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	Lambda=closure([Env|Env], LResult, [u_dummy, u_ob],  (get_var(Env, u_ob, Ob_Get), f_u_mutation_action_c63(Ob_Get, Context_Param, LResult))),
	f_u_ol_path(Fact_Param,
		    [],
		    Xx_dependency_ob_xx_Get,
		    u_backward,
		    Context_Param,
		    Lambda,
		    [],
		    Ol_path_Ret),
	Ol_path_Ret=FnResult.
:- set_opv(f_u_mutation_result_c63, classof, claz_function),
   set_opv(u_mutation_result_c63, compile_as, kw_function),
   set_opv(u_mutation_result_c63, function, f_u_mutation_result_c63),
   DefunResult=u_mutation_result_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11206 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'mutation-action?',
			    [ob, context],
			    ['ob$get', ob, [quote, mutant]]
			  ]).

% annotating U::MUTATION-ACTION? 
wl: lambda_def(defun,
	      u_mutation_action_c63,
	      f_u_mutation_action_c63,
	      [u_ob, u_context],
	      [[u_ob_c36_get, u_ob, [quote, u_mutant]]]).


% annotating U::MUTATION-ACTION? 
wl: arglist_info(u_mutation_action_c63,
		[u_ob, u_context],
		[Ob_Param, Context_Param],
		arginfo{ all:[u_ob, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context],
			 opt:0,
			 req:[u_ob, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MUTATION-ACTION? 
wl: init_args(exact_only, u_mutation_action_c63).


% annotating U::MUTATION-ACTION? 
f_u_mutation_action_c63(Ob_Param, Context_Param, FnResult) :-
	Env=[bv(u_context, Context_Param)],
	f_u_ob_c36_get(Ob_Param, u_mutant, Mutant),
	Mutant=FnResult.
:- set_opv(f_u_mutation_action_c63, classof, claz_function),
   set_opv(u_mutation_action_c63, compile_as, kw_function),
   set_opv(u_mutation_action_c63, function, f_u_mutation_action_c63),
   DefunResult=u_mutation_action_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11267 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-mutation-plans',
			    [goal, 'top-level-goal', context],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Trying mutation plans for ~A in ~A"),
			      goal,
			      context
			    ],
			    
			    [ let,
			      
			      [ ['goal-obj', ['ob$get', goal, [quote, obj]]],
				[bds, []],
				['sprouted-context', []],
				['sprouted-contexts', []]
			      ],
			      
			      [ yloop,
				[initial, [bds, []]],
				
				[ yfor,
				  'mutated-plan-context',
				  in,
				  
				  [ 'ob$get',
				    'top-level-goal',
				    [quote, 'mutation-plan-contexts']
				  ]
				],
				
				[ ydo,
				  
				  [ setq,
				    bds,
				    
				    [ 'cx$retrieve',
				      'mutated-plan-context',
				      'goal-obj'
				    ]
				  ],
				  
				  [ yloop,
				    [yfor, bd, in, bds],
				    
				    [ ydo,
				      
				      [ if,
					
					[ 'mutation-result?',
					  [car, bd],
					  'mutated-plan-context'
					],
					
					[ progn,
					  
					  [ 'ndbg-roman',
					    '*gate-dbg*',
					    rule,
					    '$STRING'("Mutation plan")
					  ],
					  
					  [ 'ndbg-roman',
					    '*gate-dbg*',
					    rule,
					    '$STRING'(" for ~A in ~A"),
					    goal,
					    context
					  ],
					  ['ndbg-newline', '*gate-dbg*', rule],
					  
					  [ setq,
					    'sprouted-context',
					    ['cx$sprout', context]
					  ],
					  
					  [ 'delay-dbgs',
					    'sprouted-context',
					    
					    [ 'ob$set',
					      'sprouted-context',
					      [quote, 'mutations-tried?'],
					      t
					    ],
					    
					    [ 'ob$removes',
					      'sprouted-context',
					      [quote, timeout]
					    ],
					    
					    [ setq,
					      'sprouted-contexts',
					      
					      [ cons,
						'sprouted-context',
						'sprouted-contexts'
					      ]
					    ],
					    
					    [ 'inference-chain->plan-trc',
					      'mutated-plan-context',
					      'sprouted-context',
					      [car, bd],
					      goal,
					      bd,
					      'top-level-goal',
					      '*active-goal-ob*'
					    ]
					  ]
					]
				      ]
				    ]
				  ]
				],
				[yresult, 'sprouted-contexts']
			      ]
			    ]
			  ]).

% annotating U::RUN-MUTATION-PLANS 
wl: lambda_def(defun,
	      u_run_mutation_plans,
	      f_u_run_mutation_plans,
	      [u_goal, u_top_level_goal, u_context],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('T'),
			     #\(r),
			     #\(y),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(m),
			     #\(u),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
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
		  u_goal,
		  u_context
		],
		
		[ let,
		  
		  [ [u_goal_obj, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		    [u_bds, []],
		    [u_sprouted_context, []],
		    [u_sprouted_contexts, []]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_bds, []]],
		    
		    [ u_yfor,
		      u_mutated_plan_context,
		      u_in,
		      
		      [ u_ob_c36_get,
			u_top_level_goal,
			[quote, u_mutation_plan_contexts]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_bds,
			[u_cx_c36_retrieve, u_mutated_plan_context, u_goal_obj]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_bd, u_in, u_bds],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_mutation_result_c63,
			      [car, u_bd],
			      u_mutated_plan_context
			    ],
			    
			    [ progn,
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('M'),
					   #\(u),
					   #\(t),
					   #\(a),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(' '),
					   #\(p),
					   #\(l),
					   #\(a),
					   #\(n)
					 ])
			      ],
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
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
				u_goal,
				u_context
			      ],
			      [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			      
			      [ setq,
				u_sprouted_context,
				[u_cx_c36_sprout, u_context]
			      ],
			      
			      [ u_delay_dbgs,
				u_sprouted_context,
				
				[ u_ob_c36_set,
				  u_sprouted_context,
				  [quote, u_mutations_tried_c63],
				  t
				],
				
				[ u_ob_c36_removes,
				  u_sprouted_context,
				  [quote, ext_timeout]
				],
				
				[ setq,
				  u_sprouted_contexts,
				  
				  [ cons,
				    u_sprouted_context,
				    u_sprouted_contexts
				  ]
				],
				
				[ u_inference_chain_c62_plan_trc,
				  u_mutated_plan_context,
				  u_sprouted_context,
				  [car, u_bd],
				  u_goal,
				  u_bd,
				  u_top_level_goal,
				  u_xx_active_goal_ob_xx
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ]
		]
	      ]).


% annotating U::RUN-MUTATION-PLANS 
wl: arglist_info(u_run_mutation_plans,
		[u_goal, u_top_level_goal, u_context],
		[Goal_Param, Top_level_goal_Param, Context_Param],
		arginfo{ all:[u_goal, u_top_level_goal, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_top_level_goal, u_context],
			 opt:0,
			 req:[u_goal, u_top_level_goal, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-MUTATION-PLANS 
wl: init_args(exact_only, u_run_mutation_plans).


% annotating U::RUN-MUTATION-PLANS 
f_u_run_mutation_plans(Goal_Param, Top_level_goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
				       #\(r),
				       #\(y),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(m),
				       #\(u),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
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
			    u_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_ob_c36_get(Goal_Param, u_obj, Goal_obj_Init),
	LEnv=[[bv(u_goal_obj, Goal_obj_Init), bv(u_bds, []), bv(u_sprouted_context, []), bv(u_sprouted_contexts, [])]|Env],
	f_u_yloop(
		  [ [u_initial, [u_bds, []]],
		    
		    [ u_yfor,
		      u_mutated_plan_context,
		      u_in,
		      
		      [ u_ob_c36_get,
			u_top_level_goal,
			[quote, u_mutation_plan_contexts]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_bds,
			[u_cx_c36_retrieve, u_mutated_plan_context, u_goal_obj]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_bd, u_in, u_bds],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_mutation_result_c63,
			      [car, u_bd],
			      u_mutated_plan_context
			    ],
			    
			    [ progn,
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('M'),
					   #\(u),
					   #\(t),
					   #\(a),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(' '),
					   #\(p),
					   #\(l),
					   #\(a),
					   #\(n)
					 ])
			      ],
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
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
				u_goal,
				u_context
			      ],
			      [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			      
			      [ setq,
				u_sprouted_context,
				[u_cx_c36_sprout, u_context]
			      ],
			      
			      [ u_delay_dbgs,
				u_sprouted_context,
				
				[ u_ob_c36_set,
				  u_sprouted_context,
				  [quote, u_mutations_tried_c63],
				  t
				],
				
				[ u_ob_c36_removes,
				  u_sprouted_context,
				  [quote, ext_timeout]
				],
				
				[ setq,
				  u_sprouted_contexts,
				  
				  [ cons,
				    u_sprouted_context,
				    u_sprouted_contexts
				  ]
				],
				
				[ u_inference_chain_c62_plan_trc,
				  u_mutated_plan_context,
				  u_sprouted_context,
				  [car, u_bd],
				  u_goal,
				  u_bd,
				  u_top_level_goal,
				  u_xx_active_goal_ob_xx
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_run_mutation_plans, classof, claz_function),
   set_opv(u_run_mutation_plans, compile_as, kw_function),
   set_opv(u_run_mutation_plans, function, f_u_run_mutation_plans),
   DefunResult=u_run_mutation_plans.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11267 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  was retrieve-all; why, I don't know--retrieve always retrieves all",
				     1,
				     11765)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11267 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the above is to prevent mutations being tried on",
				     26,
				     12447)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11267 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" any leaves which already involve mutations.",
				     26,
				     12523)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11267 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below splices in a plan resulting from an",
				     26,
				     12804)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:11267 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" inference chain from another context!",
				     26,
				     12877)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:13258 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inference-chain->plan-trc',
			    
			    [ 'inf-context',
			      'plan-context',
			      fact,
			      goal,
			      bd,
			      'top-level-goal',
			      'goal-type'
			    ],
			    
			    [ let,
			      
			      [ 
				[ 'root-goal',
				  
				  [ 'plan-instantiate',
				    goal,
				    bd,
				    'plan-context',
				    'top-level-goal',
				    '*me-belief-path*',
				    []
				  ]
				],
				
				[ dependencies,
				  
				  [ 'ol-get',
				    fact,
				    '*dependency-ob*',
				    [quote, backward],
				    'inf-context'
				  ]
				],
				[intends, []]
			      ],
			      
			      [ yloop,
				[yfor, dependency, in, dependencies],
				
				[ ydo,
				  
				  [ setq,
				    intends,
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'INTENDS',
					  'linked-from',
					  ['#COMMA', 'root-goal'],
					  'linked-to',
					  
					  [ '#COMMA',
					    
					    [ 'inference-chain->plan-trc1',
					      'inf-context',
					      'plan-context',
					      
					      [ 'ob$get',
						dependency,
						[quote, 'linked-to']
					      ],
					      'top-level-goal',
					      'goal-type'
					    ]
					  ],
					  rule,
					  
					  [ '#COMMA',
					    ['ob$get', dependency, [quote, rule]]
					  ],
					  'seq?',
					  t
					]
				      ]
				    ]
				  ],
				  ['cx$assert', 'plan-context', intends]
				]
			      ],
			      'root-goal'
			    ]
			  ]).

% annotating U::INFERENCE-CHAIN->PLAN-TRC 
wl: lambda_def(defun,
	      u_inference_chain_c62_plan_trc,
	      f_u_inference_chain_c62_plan_trc,
	      
	      [ u_inf_context,
		u_plan_context,
		u_fact,
		u_goal,
		u_bd,
		u_top_level_goal,
		u_goal_type
	      ],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_root_goal,
		      
		      [ u_plan_instantiate,
			u_goal,
			u_bd,
			u_plan_context,
			u_top_level_goal,
			u_xx_me_belief_path_xx,
			[]
		      ]
		    ],
		    
		    [ u_dependencies,
		      
		      [ u_ol_get,
			u_fact,
			u_xx_dependency_ob_xx,
			[quote, u_backward],
			u_inf_context
		      ]
		    ],
		    [u_intends, []]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_dependency, u_in, u_dependencies],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_intends,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_intends,
			      u_linked_from,
			      ['#COMMA', u_root_goal],
			      u_linked_to,
			      
			      [ '#COMMA',
				
				[ u_inference_chain_c62_plan_trc1,
				  u_inf_context,
				  u_plan_context,
				  
				  [ u_ob_c36_get,
				    u_dependency,
				    [quote, u_linked_to]
				  ],
				  u_top_level_goal,
				  u_goal_type
				]
			      ],
			      u_rule,
			      
			      [ '#COMMA',
				[u_ob_c36_get, u_dependency, [quote, u_rule]]
			      ],
			      u_seq_c63,
			      t
			    ]
			  ]
			]
		      ],
		      [u_cx_c36_assert, u_plan_context, u_intends]
		    ]
		  ],
		  u_root_goal
		]
	      ]).


% annotating U::INFERENCE-CHAIN->PLAN-TRC 
wl: arglist_info(u_inference_chain_c62_plan_trc,
		
		[ u_inf_context,
		  u_plan_context,
		  u_fact,
		  u_goal,
		  u_bd,
		  u_top_level_goal,
		  u_goal_type
		],
		
		[ Inf_context_Param,
		  Plan_context_Param,
		  Fact_Param,
		  Goal_Param,
		  Bd_Param,
		  Top_level_goal_Param,
		  Goal_type_Param
		],
		arginfo{ all:
			     [ u_inf_context,
			       u_plan_context,
			       u_fact,
			       u_goal,
			       u_bd,
			       u_top_level_goal,
			       u_goal_type
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_inf_context,
				 u_plan_context,
				 u_fact,
				 u_goal,
				 u_bd,
				 u_top_level_goal,
				 u_goal_type
			       ],
			 opt:0,
			 req:
			     [ u_inf_context,
			       u_plan_context,
			       u_fact,
			       u_goal,
			       u_bd,
			       u_top_level_goal,
			       u_goal_type
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFERENCE-CHAIN->PLAN-TRC 
wl: init_args(exact_only, u_inference_chain_c62_plan_trc).


% annotating U::INFERENCE-CHAIN->PLAN-TRC 
f_u_inference_chain_c62_plan_trc(Inf_context_Param, Plan_context_Param, Fact_Param, Goal_Param, Bd_Param, Top_level_goal_Param, Goal_type_Param, FnResult) :-
	Env=[bv(u_inf_context, Inf_context_Param), bv(u_plan_context, Plan_context_Param), bv(u_fact, Fact_Param), bv(u_goal, Goal_Param), bv(u_bd, Bd_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_goal_type, Goal_type_Param)],
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_plan_instantiate(Goal_Param,
			     Bd_Param,
			     Plan_context_Param,
			     Top_level_goal_Param,
			     Xx_me_belief_path_xx_Get,
			     [],
			     Root_goal_Init),
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	f_u_ol_get(Fact_Param,
		   Xx_dependency_ob_xx_Get,
		   u_backward,
		   Inf_context_Param,
		   Dependencies_Init),
	LEnv=[[bv(u_root_goal, Root_goal_Init), bv(u_dependencies, Dependencies_Init), bv(u_intends, [])]|Env],
	f_u_yloop(
		  [ [u_yfor, u_dependency, u_in, u_dependencies],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_intends,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_intends,
			      u_linked_from,
			      ['#COMMA', u_root_goal],
			      u_linked_to,
			      
			      [ '#COMMA',
				
				[ u_inference_chain_c62_plan_trc1,
				  u_inf_context,
				  u_plan_context,
				  
				  [ u_ob_c36_get,
				    u_dependency,
				    [quote, u_linked_to]
				  ],
				  u_top_level_goal,
				  u_goal_type
				]
			      ],
			      u_rule,
			      
			      [ '#COMMA',
				[u_ob_c36_get, u_dependency, [quote, u_rule]]
			      ],
			      u_seq_c63,
			      t
			    ]
			  ]
			]
		      ],
		      [u_cx_c36_assert, u_plan_context, u_intends]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_root_goal, Root_goal_Get),
	LetResult=Root_goal_Get,
	LetResult=FnResult.
:- set_opv(f_u_inference_chain_c62_plan_trc, classof, claz_function),
   set_opv(u_inference_chain_c62_plan_trc, compile_as, kw_function),
   set_opv(u_inference_chain_c62_plan_trc,
	   function,
	   f_u_inference_chain_c62_plan_trc),
   DefunResult=u_inference_chain_c62_plan_trc.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:13258 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Plan instantiate now returns nil if goal equals top-level-goal",
				     3,
				     13396)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:13258 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" so this will have to be rewritten.",
				     3,
				     13463)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:14455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inference-chain->plan-trc1',
			    
			    [ 'inf-context',
			      'plan-context',
			      fact,
			      'top-level-goal',
			      'goal-type'
			    ],
			    
			    [ let,
			      
			      [ 
				[ goal,
				  
				  [ 'ob$fcreate',
				    ['#BQ', ['NOTYPE', obj, ['#COMMA', fact]]]
				  ]
				],
				
				[ dependencies,
				  
				  [ 'ol-get',
				    fact,
				    '*dependency-ob*',
				    [quote, backward],
				    'inf-context'
				  ]
				],
				[intends, []]
			      ],
			      ['ob$add', goal, [quote, type], 'goal-type'],
			      
			      [ yloop,
				[yfor, dependency, in, dependencies],
				
				[ ydo,
				  
				  [ setq,
				    intends,
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'INTENDS',
					  'linked-from',
					  ['#COMMA', goal],
					  'linked-to',
					  
					  [ '#COMMA',
					    
					    [ 'inference-chain->plan-trc1',
					      'inf-context',
					      'plan-context',
					      
					      [ 'ob$get',
						dependency,
						[quote, 'linked-to']
					      ],
					      'top-level-goal',
					      'goal-type'
					    ]
					  ],
					  rule,
					  
					  [ '#COMMA',
					    ['ob$get', dependency, [quote, rule]]
					  ],
					  'seq?',
					  t
					]
				      ]
				    ]
				  ],
				  ['cx$assert', 'plan-context', intends]
				]
			      ],
			      ['cx$assert', 'plan-context', goal],
			      goal
			    ]
			  ]).

% annotating U::INFERENCE-CHAIN->PLAN-TRC1 
wl: lambda_def(defun,
	      u_inference_chain_c62_plan_trc1,
	      f_u_inference_chain_c62_plan_trc1,
	      
	      [ u_inf_context,
		u_plan_context,
		u_fact,
		u_top_level_goal,
		u_goal_type
	      ],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_goal,
		      
		      [ u_ob_c36_fcreate,
			['#BQ', [u_notype, u_obj, ['#COMMA', u_fact]]]
		      ]
		    ],
		    
		    [ u_dependencies,
		      
		      [ u_ol_get,
			u_fact,
			u_xx_dependency_ob_xx,
			[quote, u_backward],
			u_inf_context
		      ]
		    ],
		    [u_intends, []]
		  ],
		  [u_ob_c36_add, u_goal, [quote, type], u_goal_type],
		  
		  [ u_yloop,
		    [u_yfor, u_dependency, u_in, u_dependencies],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_intends,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_intends,
			      u_linked_from,
			      ['#COMMA', u_goal],
			      u_linked_to,
			      
			      [ '#COMMA',
				
				[ u_inference_chain_c62_plan_trc1,
				  u_inf_context,
				  u_plan_context,
				  
				  [ u_ob_c36_get,
				    u_dependency,
				    [quote, u_linked_to]
				  ],
				  u_top_level_goal,
				  u_goal_type
				]
			      ],
			      u_rule,
			      
			      [ '#COMMA',
				[u_ob_c36_get, u_dependency, [quote, u_rule]]
			      ],
			      u_seq_c63,
			      t
			    ]
			  ]
			]
		      ],
		      [u_cx_c36_assert, u_plan_context, u_intends]
		    ]
		  ],
		  [u_cx_c36_assert, u_plan_context, u_goal],
		  u_goal
		]
	      ]).


% annotating U::INFERENCE-CHAIN->PLAN-TRC1 
wl: arglist_info(u_inference_chain_c62_plan_trc1,
		
		[ u_inf_context,
		  u_plan_context,
		  u_fact,
		  u_top_level_goal,
		  u_goal_type
		],
		
		[ Inf_context_Param,
		  Plan_context_Param,
		  Fact_Param,
		  Top_level_goal_Param,
		  Goal_type_Param
		],
		arginfo{ all:
			     [ u_inf_context,
			       u_plan_context,
			       u_fact,
			       u_top_level_goal,
			       u_goal_type
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_inf_context,
				 u_plan_context,
				 u_fact,
				 u_top_level_goal,
				 u_goal_type
			       ],
			 opt:0,
			 req:
			     [ u_inf_context,
			       u_plan_context,
			       u_fact,
			       u_top_level_goal,
			       u_goal_type
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFERENCE-CHAIN->PLAN-TRC1 
wl: init_args(exact_only, u_inference_chain_c62_plan_trc1).


% annotating U::INFERENCE-CHAIN->PLAN-TRC1 
f_u_inference_chain_c62_plan_trc1(Inf_context_Param, Plan_context_Param, Fact_Param, Top_level_goal_Param, Goal_type_Param, FnResult) :-
	Env=[bv(u_inf_context, Inf_context_Param), bv(u_plan_context, Plan_context_Param), bv(u_fact, Fact_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_goal_type, Goal_type_Param)],
	f_u_ob_c36_fcreate(['#BQ', [u_notype, u_obj, ['#COMMA', u_fact]]],
			   Goal_Init),
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	f_u_ol_get(Fact_Param,
		   Xx_dependency_ob_xx_Get,
		   u_backward,
		   Inf_context_Param,
		   Dependencies_Init),
	LEnv=[[bv(u_goal, Goal_Init), bv(u_dependencies, Dependencies_Init), bv(u_intends, [])]|Env],
	get_var(LEnv, u_goal, Goal_Get),
	f_u_ob_c36_add(Goal_Get, type, Goal_type_Param, C36_add_Ret),
	f_u_yloop(
		  [ [u_yfor, u_dependency, u_in, u_dependencies],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_intends,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_intends,
			      u_linked_from,
			      ['#COMMA', u_goal],
			      u_linked_to,
			      
			      [ '#COMMA',
				
				[ u_inference_chain_c62_plan_trc1,
				  u_inf_context,
				  u_plan_context,
				  
				  [ u_ob_c36_get,
				    u_dependency,
				    [quote, u_linked_to]
				  ],
				  u_top_level_goal,
				  u_goal_type
				]
			      ],
			      u_rule,
			      
			      [ '#COMMA',
				[u_ob_c36_get, u_dependency, [quote, u_rule]]
			      ],
			      u_seq_c63,
			      t
			    ]
			  ]
			]
		      ],
		      [u_cx_c36_assert, u_plan_context, u_intends]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_goal, Goal_Get31),
	f_u_cx_c36_assert(Plan_context_Param, Goal_Get31, C36_assert_Ret),
	get_var(LEnv, u_goal, Goal_Get32),
	LetResult=Goal_Get32,
	LetResult=FnResult.
:- set_opv(f_u_inference_chain_c62_plan_trc1, classof, claz_function),
   set_opv(u_inference_chain_c62_plan_trc1, compile_as, kw_function),
   set_opv(u_inference_chain_c62_plan_trc1,
	   function,
	   f_u_inference_chain_c62_plan_trc1),
   DefunResult=u_inference_chain_c62_plan_trc1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_mutation.cl:14455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 15520)).
:- true.


% Total time: 5.682 seconds


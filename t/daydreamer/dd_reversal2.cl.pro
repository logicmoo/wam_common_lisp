
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_reversal2" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:13:18 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Treating multiple failed-goals required a cross product of the",
				     1,
				     2)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:66 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" negated leaf causes with which I am not prepared to deal.",
				     1,
				     67)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reverse-undo-causes',
			    
			    [ 'failed-goals',
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
				
				[ 'leaf-causes',
				  
				  [ 'get-leaf-causes',
				    [car, 'failed-goals'],
				    
				    [ 'ob$get',
				      [car, 'failed-goals'],
				      [quote, 'termination-context']
				    ]
				  ]
				],
				
				[ 'failed-goal-obj',
				  ['ob$get', [car, 'failed-goals'], [quote, obj]]
				],
				
				[ 'old-top-level-goal',
				  
				  [ 'ob$get',
				    [car, 'failed-goals'],
				    [quote, 'top-level-goal']
				  ]
				],
				['backwards-planning-path', []],
				['old-context', []],
				[rand, []],
				['new-rule', []],
				['uor-obj', []],
				[predictor, []],
				['p-goal-uid', []],
				['input-states', []],
				
				[ 'cfg-term-ctxt',
				  
				  [ 'ob$get',
				    [car, 'failed-goals'],
				    [quote, 'termination-context']
				  ]
				],
				[path, []],
				['prev-context', []]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Reverse undo causes for ~A in ~A top = ~A"),
				[car, 'failed-goals'],
				context,
				'top-level-goal'
			      ],
			      
			      [ setq,
				'old-context',
				
				[ 'ob$get',
				  'old-top-level-goal',
				  [quote, 'activation-context']
				]
			      ],
			      
			      [ setq,
				'backwards-planning-path',
				
				[ reverse,
				  
				  [ memq,
				    'old-context',
				    
				    [ reverse,
				      
				      [ cons,
					'cfg-term-ctxt',
					['cx$ancestors', 'cfg-term-ctxt']
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				['null?', 'backwards-planning-path'],
				
				[ error,
				  '$STRING'("Null backwards planning path.")
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Bckwds plng path = ~A"),
				'backwards-planning-path'
			      ],
			      
			      [ yloop,
				[yfor, 'leaf-cause', in, 'leaf-causes'],
				
				[ ydo,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Considering leaf cause ~A"),
				    'leaf-cause'
				  ],
				  
				  [ if,
				    ['ty$instance?', 'leaf-cause', [quote, not]],
				    
				    [ progn,
				      
				      [ if,
					
					[ 'ty$instance?',
					  ['ob$get', 'leaf-cause', [quote, obj]],
					  [quote, 'long-term-state']
					],
					
					[ progn,
					  
					  [ setq,
					    '*new-personal-goals*',
					    
					    [ cons,
					      
					      [ 'ob$get',
						'leaf-cause',
						[quote, obj]
					      ],
					      '*new-personal-goals*'
					    ]
					  ],
					  
					  [ 'activate-top-level-goal',
					    
					    [ 'ob$fcreate',
					      
					      [ '#BQ',
						
						[ 'ACTIVE-GOAL',
						  obj,
						  
						  [ '#COMMA',
						    
						    [ 'ob$get',
						      'leaf-cause',
						      [quote, obj]
						    ]
						  ]
						]
					      ]
					    ],
					    '*reality-lookahead*',
					    '*empty-bd*',
					    
					    [ 'ob$fcreate',
					      
					      [ '#BQ',
						
						[ 'RULE',
						  emotion,
						  
						  [ 'POS-EMOTION',
						    strength,
						    ['#COMMA', [strength, goal]]
						  ]
						]
					      ]
					    ]
					  ]
					],
					
					[ progn,
					  
					  [ setq,
					    rand,
					    ['ob$fcreate', [quote, ['RAND']]]
					  ],
					  
					  [ yloop,
					    
					    [ yfor,
					      'leaf-cause1',
					      in,
					      'leaf-causes'
					    ],
					    
					    [ ydo,
					      
					      [ if,
						
						[ 'neq?',
						  'leaf-cause1',
						  'leaf-cause'
						],
						
						[ progn,
						  
						  [ setq,
						    'uor-obj',
						    
						    [ 'ob$fcreate',
						      [quote, ['ROR']]
						    ]
						  ],
						  
						  [ 'ob$add',
						    'uor-obj',
						    [quote, obj],
						    'leaf-cause1'
						  ],
						  
						  [ 'ob$add',
						    'uor-obj',
						    [quote, obj],
						    
						    [ 'ob$fcreate',
						      
						      [ '#BQ',
							
							[ 'ACTIVE-GOAL',
							  obj,
							  
							  [ '#COMMA',
							    'leaf-cause1'
							  ]
							]
						      ]
						    ]
						  ],
						  
						  [ setq,
						    predictor,
						    
						    [ 'predicting-state',
						      'leaf-cause1'
						    ]
						  ],
						  
						  [ if,
						    predictor,
						    
						    [ progn,
						      
						      [ 'ob$add',
							'uor-obj',
							[quote, obj],
							predictor
						      ],
						      
						      [ 'ob$add',
							'uor-obj',
							[quote, obj],
							
							[ 'ob$fcreate',
							  
							  [ '#BQ',
							    
							    [ 'ACTIVE-GOAL',
							      obj,
							      
							      [ '#COMMA',
								predictor
							      ]
							    ]
							  ]
							]
						      ]
						    ]
						  ],
						  
						  [ 'ob$add',
						    rand,
						    [quote, obj],
						    'uor-obj'
						  ]
						]
					      ]
					    ]
					  ],
					  
					  [ setq,
					    'p-goal-uid',
					    
					    [ 'string->symbol',
					      
					      [ 'string-append',
						'$STRING'("PRESERVATION"),
						
						[ 'fixnum->string',
						  '*next-prule-number*'
						]
					      ]
					    ]
					  ],
					  
					  [ setq,
					    'new-rule',
					    
					    [ 'ob$fcreate',
					      
					      [ '#BQ',
						
						[ 'RULE',
						  subgoal,
						  ['#COMMA', rand],
						  goal,
						  
						  [ 'ACTIVE-GOAL',
						    obj,
						    
						    [ 'PRESERVATION',
						      obj,
						      
						      [ '#COMMA',
							'failed-goal-obj'
						      ],
						      uid,
						      
						      [ quote,
							
							[ '#COMMA',
							  'p-goal-uid'
							]
						      ]
						    ]
						  ],
						  (is),
						  [quote, 'inference-only'],
						  plausibility,
						  0.9
						]
					      ]
					    ]
					  ],
					  
					  [ setq,
					    'new-rule',
					    
					    [ 'ob$variabilize',
					      'new-rule',
					      function('varize-object?'),
					      [],
					      '*link-slots*',
					      []
					    ]
					  ],
					  
					  [ 'ob$add-unique-name',
					    'new-rule',
					    
					    [ 'string->symbol',
					      
					      [ 'string-append',
						'$STRING'("PRESERVATION-INF."),
						
						[ 'fixnum->string',
						  '*next-prule-number*'
						]
					      ]
					    ]
					  ],
					  ['add-rule-print', 'new-rule'],
					  
					  [ setq,
					    'new-rule',
					    
					    [ 'ob$fcreate',
					      
					      [ '#BQ',
						
						[ 'RULE',
						  subgoal,
						  
						  [ '#COMMA',
						    
						    [ 'ob$get',
						      'leaf-cause',
						      [quote, obj]
						    ]
						  ],
						  goal,
						  
						  [ 'PRESERVATION',
						    obj,
						    
						    [ '#COMMA',
						      'failed-goal-obj'
						    ],
						    uid,
						    
						    [ quote,
						      ['#COMMA', 'p-goal-uid']
						    ]
						  ],
						  (is),
						  [quote, 'plan-only'],
						  plausibility,
						  0.9
						]
					      ]
					    ]
					  ],
					  
					  [ setq,
					    'new-rule',
					    
					    [ 'ob$variabilize',
					      'new-rule',
					      function('varize-object?'),
					      [],
					      '*link-slots*',
					      []
					    ]
					  ],
					  
					  [ 'ob$add-unique-name',
					    'new-rule',
					    
					    [ 'string->symbol',
					      set,
					      up,
					      rules,
					      
					      [ setq,
						'p-goal-uid',
						
						[ 'string->symbol',
						  
						  [ 'string-append',
						    '$STRING'("PRESERVATION"),
						    
						    [ 'fixnum->string',
						      '*next-prule-number*'
						    ]
						  ]
						]
					      ],
					      
					      [ setq,
						'new-rule',
						
						[ 'ob$fcreate',
						  
						  [ '#BQ',
						    
						    [ 'RULE',
						      subgoal,
						      ['#COMMA', rand],
						      goal,
						      
						      [ 'ACTIVE-GOAL',
							obj,
							
							[ 'PRESERVATION',
							  obj,
							  
							  [ '#COMMA',
							    'failed-goal-obj'
							  ],
							  uid,
							  
							  [ quote,
							    
							    [ '#COMMA',
							      'p-goal-uid'
							    ]
							  ]
							]
						      ],
						      (is),
						      [quote, 'inference-only'],
						      plausibility,
						      0.9
						    ]
						  ]
						]
					      ],
					      
					      [ setq,
						'new-rule',
						
						[ 'ob$variabilize',
						  'new-rule',
						  function('varize-object?'),
						  [],
						  '*link-slots*',
						  []
						]
					      ],
					      
					      [ 'ob$add-unique-name',
						'new-rule',
						
						[ 'string->symbol',
						  
						  [ 'string-append',
						    '$STRING'("PRESERVATION-INF."),
						    
						    [ 'fixnum->string',
						      '*next-prule-number*'
						    ]
						  ]
						]
					      ],
					      ['add-rule-print', 'new-rule'],
					      
					      [ setq,
						'new-rule',
						
						[ 'ob$fcreate',
						  
						  [ '#BQ',
						    
						    [ 'RULE',
						      subgoal,
						      
						      [ '#COMMA',
							
							[ 'ob$get',
							  'leaf-cause',
							  [quote, obj]
							]
						      ],
						      goal,
						      
						      [ 'PRESERVATION',
							obj,
							
							[ '#COMMA',
							  'failed-goal-obj'
							],
							uid,
							
							[ quote,
							  
							  [ '#COMMA',
							    'p-goal-uid'
							  ]
							]
						      ],
						      (is),
						      [quote, 'plan-only'],
						      plausibility,
						      0.9
						    ]
						  ]
						]
					      ],
					      
					      [ setq,
						'new-rule',
						
						[ 'ob$variabilize',
						  'new-rule',
						  function('varize-object?'),
						  [],
						  '*link-slots*',
						  []
						]
					      ],
					      
					      [ 'ob$add-unique-name',
						'new-rule',
						
						[ 'string->symbol',
						  
						  [ 'string-append',
						    '$STRING'("PRESERVATION-PLAN."),
						    
						    [ 'fixnum->string',
						      '*next-prule-number*'
						    ]
						  ]
						]
					      ],
					      ['add-rule-print', 'new-rule'],
					      
					      [ 'increment-me',
						'*next-prule-number*'
					      ],
					      [setq, 'input-states', []],
					      
					      [ setq,
						path,
						'backwards-planning-path'
					      ],
					      [setq, 'old-context', []],
					      
					      [ yloop,
						
						[ ydo,
						  
						  [ setq,
						    'prev-context',
						    'old-context'
						  ],
						  
						  [ setq,
						    'old-context',
						    [car, path]
						  ],
						  
						  [ setq,
						    'input-states',
						    
						    [ union,
						      'input-states',
						      
						      [ 'cx$input-states',
							'old-context'
						      ]
						    ]
						  ],
						  [setq, path, [cdr, path]]
						],
						[ywhile, path]
					      ],
					      
					      [ setq,
						'old-context',
						
						[ 'ob$get',
						  'old-top-level-goal',
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
						  '$STRING'("Reverse undo cause")
						],
						
						[ setq,
						  'sprouted-context',
						  
						  [ 'reversal-sprout-alternative',
						    'old-context',
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
						
						[ 'no-gen',
						  
						  [ 'cx$assert-many',
						    'sprouted-context',
						    'input-states'
						  ]
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
				      ]
				    ]
				  ],
				  'sprouted-contexts'
				]
			      ],
			      'Sprout',
			      an,
			      alternative,
			      past,
			      context,
			      
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
				  '$STRING'("Reverse undo cause")
				],
				
				[ setq,
				  'sprouted-context',
				  
				  [ 'reversal-sprout-alternative',
				    'old-context',
				    'old-top-level-goal',
				    context,
				    'top-level-goal',
				    1.0,
				    t
				  ]
				],
				[setq, xxcontext, 'sprouted-context'],
				
				[ 'no-gen',
				  
				  [ 'cx$assert-many',
				    'sprouted-context',
				    'input-states'
				  ]
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
			  ]).

% annotating U::REVERSE-UNDO-CAUSES 
wl: lambda_def(defun,
	      u_reverse_undo_causes,
	      f_u_reverse_undo_causes,
	      [u_failed_goals, u_context, u_top_level_goal, u_rule, u_bd, u_goal],
	      
	      [ 
		[ let,
		  
		  [ [u_sprouted_contexts, []],
		    [u_sprouted_context, []],
		    [u_intends, []],
		    
		    [ u_leaf_causes,
		      
		      [ u_get_leaf_causes,
			[car, u_failed_goals],
			
			[ u_ob_c36_get,
			  [car, u_failed_goals],
			  [quote, u_termination_context]
			]
		      ]
		    ],
		    
		    [ u_failed_goal_obj,
		      [u_ob_c36_get, [car, u_failed_goals], [quote, u_obj]]
		    ],
		    
		    [ u_old_top_level_goal,
		      
		      [ u_ob_c36_get,
			[car, u_failed_goals],
			[quote, u_top_level_goal]
		      ]
		    ],
		    [u_backwards_planning_path, []],
		    [u_old_context, []],
		    [u_rand, []],
		    [u_new_rule, []],
		    [u_uor_obj, []],
		    [u_predictor, []],
		    [u_p_goal_uid, []],
		    [u_input_states, []],
		    
		    [ u_cfg_term_ctxt,
		      
		      [ u_ob_c36_get,
			[car, u_failed_goals],
			[quote, u_termination_context]
		      ]
		    ],
		    [u_path, []],
		    [u_prev_context, []]
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
			       #\(u),
			       #\(n),
			       #\(d),
			       #\(o),
			       #\(' '),
			       #\(c),
			       #\(a),
			       #\(u),
			       #\(s),
			       #\(e),
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
		    [car, u_failed_goals],
		    u_context,
		    u_top_level_goal
		  ],
		  
		  [ setq,
		    u_old_context,
		    
		    [ u_ob_c36_get,
		      u_old_top_level_goal,
		      [quote, u_activation_context]
		    ]
		  ],
		  
		  [ setq,
		    u_backwards_planning_path,
		    
		    [ reverse,
		      
		      [ ext_memq,
			u_old_context,
			
			[ reverse,
			  
			  [ cons,
			    u_cfg_term_ctxt,
			    [u_cx_c36_ancestors, u_cfg_term_ctxt]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, u_backwards_planning_path],
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('N'),
				 #\(u),
				 #\(l),
				 #\(l),
				 #\(' '),
				 #\(b),
				 #\(a),
				 #\(c),
				 #\(k),
				 #\(w),
				 #\(a),
				 #\(r),
				 #\(d),
				 #\(s),
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
				 #\(p),
				 #\(a),
				 #\(t),
				 #\(h),
				 #\('.')
			       ])
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('B'),
			       #\(c),
			       #\(k),
			       #\(w),
			       #\(d),
			       #\(s),
			       #\(' '),
			       #\(p),
			       #\(l),
			       #\(n),
			       #\(g),
			       #\(' '),
			       #\(p),
			       #\(a),
			       #\(t),
			       #\(h),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_backwards_planning_path
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_leaf_cause, u_in, u_leaf_causes],
		    
		    [ u_ydo,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('C'),
				   #\(o),
				   #\(n),
				   #\(s),
				   #\(i),
				   #\(d),
				   #\(e),
				   #\(r),
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
				   #\(a),
				   #\(u),
				   #\(s),
				   #\(e),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_leaf_cause
		      ],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_leaf_cause, [quote, not]],
			
			[ progn,
			  
			  [ if,
			    
			    [ u_ty_c36_instance_c63,
			      [u_ob_c36_get, u_leaf_cause, [quote, u_obj]],
			      [quote, u_long_term_state]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_xx_new_personal_goals_xx,
				
				[ cons,
				  [u_ob_c36_get, u_leaf_cause, [quote, u_obj]],
				  u_xx_new_personal_goals_xx
				]
			      ],
			      
			      [ u_activate_top_level_goal,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_active_goal,
				      u_obj,
				      
				      [ '#COMMA',
					
					[ u_ob_c36_get,
					  u_leaf_cause,
					  [quote, u_obj]
					]
				      ]
				    ]
				  ]
				],
				u_xx_reality_lookahead_xx,
				u_xx_empty_bd_xx,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_rule,
				      u_emotion,
				      
				      [ u_pos_emotion,
					u_strength,
					['#COMMA', [u_strength, u_goal]]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_rand,
				[u_ob_c36_fcreate, [quote, [u_rand]]]
			      ],
			      
			      [ u_yloop,
				[u_yfor, u_leaf_cause1, u_in, u_leaf_causes],
				
				[ u_ydo,
				  
				  [ if,
				    [u_neq_c63, u_leaf_cause1, u_leaf_cause],
				    
				    [ progn,
				      
				      [ setq,
					u_uor_obj,
					[u_ob_c36_fcreate, [quote, [u_ror]]]
				      ],
				      
				      [ u_ob_c36_add,
					u_uor_obj,
					[quote, u_obj],
					u_leaf_cause1
				      ],
				      
				      [ u_ob_c36_add,
					u_uor_obj,
					[quote, u_obj],
					
					[ u_ob_c36_fcreate,
					  
					  [ '#BQ',
					    
					    [ u_active_goal,
					      u_obj,
					      ['#COMMA', u_leaf_cause1]
					    ]
					  ]
					]
				      ],
				      
				      [ setq,
					u_predictor,
					[u_predicting_state, u_leaf_cause1]
				      ],
				      
				      [ if,
					u_predictor,
					
					[ progn,
					  
					  [ u_ob_c36_add,
					    u_uor_obj,
					    [quote, u_obj],
					    u_predictor
					  ],
					  
					  [ u_ob_c36_add,
					    u_uor_obj,
					    [quote, u_obj],
					    
					    [ u_ob_c36_fcreate,
					      
					      [ '#BQ',
						
						[ u_active_goal,
						  u_obj,
						  ['#COMMA', u_predictor]
						]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ u_ob_c36_add,
					u_rand,
					[quote, u_obj],
					u_uor_obj
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_p_goal_uid,
				
				[ u_string_c62_symbol,
				  
				  [ u_string_append,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('P'),
					       #\('R'),
					       #\('E'),
					       #\('S'),
					       #\('E'),
					       #\('R'),
					       #\('V'),
					       #\('A'),
					       #\('T'),
					       #\('I'),
					       #\('O'),
					       #\('N')
					     ]),
				    
				    [ u_fixnum_c62_string,
				      u_xx_next_prule_number_xx
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_rule,
				      u_subgoal,
				      ['#COMMA', u_rand],
				      u_goal,
				      
				      [ u_active_goal,
					u_obj,
					
					[ u_preservation,
					  u_obj,
					  ['#COMMA', u_failed_goal_obj],
					  u_uid,
					  [quote, ['#COMMA', u_p_goal_uid]]
					]
				      ],
				      u_is,
				      [quote, u_inference_only],
				      u_plausibility,
				      0.9
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_variabilize,
				  u_new_rule,
				  function(u_varize_object_c63),
				  [],
				  u_xx_link_slots_xx,
				  []
				]
			      ],
			      
			      [ u_ob_c36_add_unique_name,
				u_new_rule,
				
				[ u_string_c62_symbol,
				  
				  [ u_string_append,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('P'),
					       #\('R'),
					       #\('E'),
					       #\('S'),
					       #\('E'),
					       #\('R'),
					       #\('V'),
					       #\('A'),
					       #\('T'),
					       #\('I'),
					       #\('O'),
					       #\('N'),
					       #\(-),
					       #\('I'),
					       #\('N'),
					       #\('F'),
					       #\('.')
					     ]),
				    
				    [ u_fixnum_c62_string,
				      u_xx_next_prule_number_xx
				    ]
				  ]
				]
			      ],
			      [u_add_rule_print, u_new_rule],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_rule,
				      u_subgoal,
				      
				      [ '#COMMA',
					
					[ u_ob_c36_get,
					  u_leaf_cause,
					  [quote, u_obj]
					]
				      ],
				      u_goal,
				      
				      [ u_preservation,
					u_obj,
					['#COMMA', u_failed_goal_obj],
					u_uid,
					[quote, ['#COMMA', u_p_goal_uid]]
				      ],
				      u_is,
				      [quote, u_plan_only],
				      u_plausibility,
				      0.9
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_variabilize,
				  u_new_rule,
				  function(u_varize_object_c63),
				  [],
				  u_xx_link_slots_xx,
				  []
				]
			      ],
			      
			      [ u_ob_c36_add_unique_name,
				u_new_rule,
				
				[ u_string_c62_symbol,
				  set,
				  u_up,
				  u_rules,
				  
				  [ setq,
				    u_p_goal_uid,
				    
				    [ u_string_c62_symbol,
				      
				      [ u_string_append,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('P'),
						   #\('R'),
						   #\('E'),
						   #\('S'),
						   #\('E'),
						   #\('R'),
						   #\('V'),
						   #\('A'),
						   #\('T'),
						   #\('I'),
						   #\('O'),
						   #\('N')
						 ]),
					
					[ u_fixnum_c62_string,
					  u_xx_next_prule_number_xx
					]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_fcreate,
				      
				      [ '#BQ',
					
					[ u_rule,
					  u_subgoal,
					  ['#COMMA', u_rand],
					  u_goal,
					  
					  [ u_active_goal,
					    u_obj,
					    
					    [ u_preservation,
					      u_obj,
					      ['#COMMA', u_failed_goal_obj],
					      u_uid,
					      [quote, ['#COMMA', u_p_goal_uid]]
					    ]
					  ],
					  u_is,
					  [quote, u_inference_only],
					  u_plausibility,
					  0.9
					]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_variabilize,
				      u_new_rule,
				      function(u_varize_object_c63),
				      [],
				      u_xx_link_slots_xx,
				      []
				    ]
				  ],
				  
				  [ u_ob_c36_add_unique_name,
				    u_new_rule,
				    
				    [ u_string_c62_symbol,
				      
				      [ u_string_append,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('P'),
						   #\('R'),
						   #\('E'),
						   #\('S'),
						   #\('E'),
						   #\('R'),
						   #\('V'),
						   #\('A'),
						   #\('T'),
						   #\('I'),
						   #\('O'),
						   #\('N'),
						   #\(-),
						   #\('I'),
						   #\('N'),
						   #\('F'),
						   #\('.')
						 ]),
					
					[ u_fixnum_c62_string,
					  u_xx_next_prule_number_xx
					]
				      ]
				    ]
				  ],
				  [u_add_rule_print, u_new_rule],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_fcreate,
				      
				      [ '#BQ',
					
					[ u_rule,
					  u_subgoal,
					  
					  [ '#COMMA',
					    
					    [ u_ob_c36_get,
					      u_leaf_cause,
					      [quote, u_obj]
					    ]
					  ],
					  u_goal,
					  
					  [ u_preservation,
					    u_obj,
					    ['#COMMA', u_failed_goal_obj],
					    u_uid,
					    [quote, ['#COMMA', u_p_goal_uid]]
					  ],
					  u_is,
					  [quote, u_plan_only],
					  u_plausibility,
					  0.9
					]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_variabilize,
				      u_new_rule,
				      function(u_varize_object_c63),
				      [],
				      u_xx_link_slots_xx,
				      []
				    ]
				  ],
				  
				  [ u_ob_c36_add_unique_name,
				    u_new_rule,
				    
				    [ u_string_c62_symbol,
				      
				      [ u_string_append,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('P'),
						   #\('R'),
						   #\('E'),
						   #\('S'),
						   #\('E'),
						   #\('R'),
						   #\('V'),
						   #\('A'),
						   #\('T'),
						   #\('I'),
						   #\('O'),
						   #\('N'),
						   #\(-),
						   #\('P'),
						   #\('L'),
						   #\('A'),
						   #\('N'),
						   #\('.')
						 ]),
					
					[ u_fixnum_c62_string,
					  u_xx_next_prule_number_xx
					]
				      ]
				    ]
				  ],
				  [u_add_rule_print, u_new_rule],
				  [u_increment_me, u_xx_next_prule_number_xx],
				  [setq, u_input_states, []],
				  [setq, u_path, u_backwards_planning_path],
				  [setq, u_old_context, []],
				  
				  [ u_yloop,
				    
				    [ u_ydo,
				      [setq, u_prev_context, u_old_context],
				      [setq, u_old_context, [car, u_path]],
				      
				      [ setq,
					u_input_states,
					
					[ union,
					  u_input_states,
					  
					  [ u_cx_c36_input_states,
					    u_old_context
					  ]
					]
				      ],
				      [setq, u_path, [cdr, u_path]]
				    ],
				    [u_ywhile, u_path]
				  ],
				  
				  [ setq,
				    u_old_context,
				    
				    [ u_ob_c36_get,
				      u_old_top_level_goal,
				      [quote, u_activation_context]
				    ]
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
						 #\(u),
						 #\(n),
						 #\(d),
						 #\(o),
						 #\(' '),
						 #\(c),
						 #\(a),
						 #\(u),
						 #\(s),
						 #\(e)
					       ])
				    ],
				    
				    [ setq,
				      u_sprouted_context,
				      
				      [ u_reversal_sprout_alternative,
					u_old_context,
					u_old_top_level_goal,
					u_context,
					u_top_level_goal,
					1.0,
					t
				      ]
				    ],
				    [setq, u_xxcontext, u_sprouted_context],
				    
				    [ u_no_gen,
				      
				      [ u_cx_c36_assert_many,
					u_sprouted_context,
					u_input_states
				      ]
				    ],
				    
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
			  ]
			]
		      ],
		      u_sprouted_contexts
		    ]
		  ],
		  u_sprout,
		  u_an,
		  u_alternative,
		  u_past,
		  u_context,
		  
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
				 #\(u),
				 #\(n),
				 #\(d),
				 #\(o),
				 #\(' '),
				 #\(c),
				 #\(a),
				 #\(u),
				 #\(s),
				 #\(e)
			       ])
		    ],
		    
		    [ setq,
		      u_sprouted_context,
		      
		      [ u_reversal_sprout_alternative,
			u_old_context,
			u_old_top_level_goal,
			u_context,
			u_top_level_goal,
			1.0,
			t
		      ]
		    ],
		    [setq, u_xxcontext, u_sprouted_context],
		    
		    [ u_no_gen,
		      [u_cx_c36_assert_many, u_sprouted_context, u_input_states]
		    ],
		    
		    [ setq,
		      u_sprouted_contexts,
		      [cons, u_sprouted_context, u_sprouted_contexts]
		    ]
		  ]
		]
	      ]).


% annotating U::REVERSE-UNDO-CAUSES 
wl: arglist_info(u_reverse_undo_causes,
		[u_failed_goals, u_context, u_top_level_goal, u_rule, u_bd, u_goal],
		
		[ Failed_goals_Param,
		  Context_Param,
		  Top_level_goal_Param,
		  Rule_Param,
		  Bd_Param,
		  Goal_Param
		],
		arginfo{ all:
			     [ u_failed_goals,
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
			       [ u_failed_goals,
				 u_context,
				 u_top_level_goal,
				 u_rule,
				 u_bd,
				 u_goal
			       ],
			 opt:0,
			 req:
			     [ u_failed_goals,
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

% annotating U::REVERSE-UNDO-CAUSES 
wl: init_args(exact_only, u_reverse_undo_causes).


% annotating U::REVERSE-UNDO-CAUSES 
f_u_reverse_undo_causes(Failed_goals_Param, Context_Param, Top_level_goal_Param, Rule_Param, Bd_Param, Goal_Param, FnResult) :-
	Env=[bv(u_failed_goals, Failed_goals_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_rule, Rule_Param), bv(u_bd, Bd_Param), bv(u_goal, Goal_Param)],
	cl_car(Failed_goals_Param, Leaf_causes_Param),
	cl_car(Failed_goals_Param, C36_get_Param),
	f_u_ob_c36_get(C36_get_Param,
		       u_termination_context,
		       Termination_context),
	f_u_get_leaf_causes(Leaf_causes_Param,
			    Termination_context,
			    Leaf_causes_Init),
	cl_car(Failed_goals_Param, C36_get_Param50),
	f_u_ob_c36_get(C36_get_Param50, u_obj, Failed_goal_obj_Init),
	cl_car(Failed_goals_Param, C36_get_Param51),
	f_u_ob_c36_get(C36_get_Param51,
		       u_top_level_goal,
		       Old_top_level_goal_Init),
	cl_car(Failed_goals_Param, C36_get_Param52),
	f_u_ob_c36_get(C36_get_Param52,
		       u_termination_context,
		       Cfg_term_ctxt_Init),
	LEnv=[[bv(u_sprouted_contexts, []), bv(u_sprouted_context, []), bv(u_intends, []), bv(u_leaf_causes, Leaf_causes_Init), bv(u_failed_goal_obj, Failed_goal_obj_Init), bv(u_old_top_level_goal, Old_top_level_goal_Init), bv(u_backwards_planning_path, []), bv(u_old_context, []), bv(u_rand, []), bv(u_new_rule, []), bv(u_uor_obj, []), bv(u_predictor, []), bv(u_p_goal_uid, []), bv(u_input_states, []), bv(u_cfg_term_ctxt, Cfg_term_ctxt_Init), bv(u_path, []), bv(u_prev_context, [])]|Env],
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
				       #\(u),
				       #\(n),
				       #\(d),
				       #\(o),
				       #\(' '),
				       #\(c),
				       #\(a),
				       #\(u),
				       #\(s),
				       #\(e),
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
			    [car, u_failed_goals],
			    u_context,
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	get_var(LEnv, u_old_top_level_goal, Old_top_level_goal_Get),
	f_u_ob_c36_get(Old_top_level_goal_Get,
		       u_activation_context,
		       Activation_context),
	set_var(LEnv, u_old_context, Activation_context),
	f_ext_memq(u_old_context,
		   
		   [ reverse,
		     
		     [ cons,
		       u_cfg_term_ctxt,
		       [u_cx_c36_ancestors, u_cfg_term_ctxt]
		     ]
		   ],
		   Reverse_Param),
	cl_reverse(Reverse_Param, Backwards_planning_path),
	set_var(LEnv, u_backwards_planning_path, Backwards_planning_path),
	f_u_null_c63(u_backwards_planning_path, IFTEST),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('N'),
				  #\(u),
				  #\(l),
				  #\(l),
				  #\(' '),
				  #\(b),
				  #\(a),
				  #\(c),
				  #\(k),
				  #\(w),
				  #\(a),
				  #\(r),
				  #\(d),
				  #\(s),
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
				  #\(p),
				  #\(a),
				  #\(t),
				  #\(h),
				  #\('.')
				])
		     ],
		     TrueResult),
	    _111088=TrueResult
	;   _111088=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('B'),
				       #\(c),
				       #\(k),
				       #\(w),
				       #\(d),
				       #\(s),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(p),
				       #\(a),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_backwards_planning_path
			  ],
			  Roman_nl_Ret55),
	f_u_yloop(
		  [ [u_yfor, u_leaf_cause, u_in, u_leaf_causes],
		    
		    [ u_ydo,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('C'),
				   #\(o),
				   #\(n),
				   #\(s),
				   #\(i),
				   #\(d),
				   #\(e),
				   #\(r),
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
				   #\(a),
				   #\(u),
				   #\(s),
				   #\(e),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_leaf_cause
		      ],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_leaf_cause, [quote, not]],
			
			[ progn,
			  
			  [ if,
			    
			    [ u_ty_c36_instance_c63,
			      [u_ob_c36_get, u_leaf_cause, [quote, u_obj]],
			      [quote, u_long_term_state]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_xx_new_personal_goals_xx,
				
				[ cons,
				  [u_ob_c36_get, u_leaf_cause, [quote, u_obj]],
				  u_xx_new_personal_goals_xx
				]
			      ],
			      
			      [ u_activate_top_level_goal,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_active_goal,
				      u_obj,
				      
				      [ '#COMMA',
					
					[ u_ob_c36_get,
					  u_leaf_cause,
					  [quote, u_obj]
					]
				      ]
				    ]
				  ]
				],
				u_xx_reality_lookahead_xx,
				u_xx_empty_bd_xx,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_rule,
				      u_emotion,
				      
				      [ u_pos_emotion,
					u_strength,
					['#COMMA', [u_strength, u_goal]]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_rand,
				[u_ob_c36_fcreate, [quote, [u_rand]]]
			      ],
			      
			      [ u_yloop,
				[u_yfor, u_leaf_cause1, u_in, u_leaf_causes],
				
				[ u_ydo,
				  
				  [ if,
				    [u_neq_c63, u_leaf_cause1, u_leaf_cause],
				    
				    [ progn,
				      
				      [ setq,
					u_uor_obj,
					[u_ob_c36_fcreate, [quote, [u_ror]]]
				      ],
				      
				      [ u_ob_c36_add,
					u_uor_obj,
					[quote, u_obj],
					u_leaf_cause1
				      ],
				      
				      [ u_ob_c36_add,
					u_uor_obj,
					[quote, u_obj],
					
					[ u_ob_c36_fcreate,
					  
					  [ '#BQ',
					    
					    [ u_active_goal,
					      u_obj,
					      ['#COMMA', u_leaf_cause1]
					    ]
					  ]
					]
				      ],
				      
				      [ setq,
					u_predictor,
					[u_predicting_state, u_leaf_cause1]
				      ],
				      
				      [ if,
					u_predictor,
					
					[ progn,
					  
					  [ u_ob_c36_add,
					    u_uor_obj,
					    [quote, u_obj],
					    u_predictor
					  ],
					  
					  [ u_ob_c36_add,
					    u_uor_obj,
					    [quote, u_obj],
					    
					    [ u_ob_c36_fcreate,
					      
					      [ '#BQ',
						
						[ u_active_goal,
						  u_obj,
						  ['#COMMA', u_predictor]
						]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ u_ob_c36_add,
					u_rand,
					[quote, u_obj],
					u_uor_obj
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_p_goal_uid,
				
				[ u_string_c62_symbol,
				  
				  [ u_string_append,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('P'),
					       #\('R'),
					       #\('E'),
					       #\('S'),
					       #\('E'),
					       #\('R'),
					       #\('V'),
					       #\('A'),
					       #\('T'),
					       #\('I'),
					       #\('O'),
					       #\('N')
					     ]),
				    
				    [ u_fixnum_c62_string,
				      u_xx_next_prule_number_xx
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_rule,
				      u_subgoal,
				      ['#COMMA', u_rand],
				      u_goal,
				      
				      [ u_active_goal,
					u_obj,
					
					[ u_preservation,
					  u_obj,
					  ['#COMMA', u_failed_goal_obj],
					  u_uid,
					  [quote, ['#COMMA', u_p_goal_uid]]
					]
				      ],
				      u_is,
				      [quote, u_inference_only],
				      u_plausibility,
				      0.9
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_variabilize,
				  u_new_rule,
				  function(u_varize_object_c63),
				  [],
				  u_xx_link_slots_xx,
				  []
				]
			      ],
			      
			      [ u_ob_c36_add_unique_name,
				u_new_rule,
				
				[ u_string_c62_symbol,
				  
				  [ u_string_append,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('P'),
					       #\('R'),
					       #\('E'),
					       #\('S'),
					       #\('E'),
					       #\('R'),
					       #\('V'),
					       #\('A'),
					       #\('T'),
					       #\('I'),
					       #\('O'),
					       #\('N'),
					       #\(-),
					       #\('I'),
					       #\('N'),
					       #\('F'),
					       #\('.')
					     ]),
				    
				    [ u_fixnum_c62_string,
				      u_xx_next_prule_number_xx
				    ]
				  ]
				]
			      ],
			      [u_add_rule_print, u_new_rule],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_fcreate,
				  
				  [ '#BQ',
				    
				    [ u_rule,
				      u_subgoal,
				      
				      [ '#COMMA',
					
					[ u_ob_c36_get,
					  u_leaf_cause,
					  [quote, u_obj]
					]
				      ],
				      u_goal,
				      
				      [ u_preservation,
					u_obj,
					['#COMMA', u_failed_goal_obj],
					u_uid,
					[quote, ['#COMMA', u_p_goal_uid]]
				      ],
				      u_is,
				      [quote, u_plan_only],
				      u_plausibility,
				      0.9
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				u_new_rule,
				
				[ u_ob_c36_variabilize,
				  u_new_rule,
				  function(u_varize_object_c63),
				  [],
				  u_xx_link_slots_xx,
				  []
				]
			      ],
			      
			      [ u_ob_c36_add_unique_name,
				u_new_rule,
				
				[ u_string_c62_symbol,
				  set,
				  u_up,
				  u_rules,
				  
				  [ setq,
				    u_p_goal_uid,
				    
				    [ u_string_c62_symbol,
				      
				      [ u_string_append,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('P'),
						   #\('R'),
						   #\('E'),
						   #\('S'),
						   #\('E'),
						   #\('R'),
						   #\('V'),
						   #\('A'),
						   #\('T'),
						   #\('I'),
						   #\('O'),
						   #\('N')
						 ]),
					
					[ u_fixnum_c62_string,
					  u_xx_next_prule_number_xx
					]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_fcreate,
				      
				      [ '#BQ',
					
					[ u_rule,
					  u_subgoal,
					  ['#COMMA', u_rand],
					  u_goal,
					  
					  [ u_active_goal,
					    u_obj,
					    
					    [ u_preservation,
					      u_obj,
					      ['#COMMA', u_failed_goal_obj],
					      u_uid,
					      [quote, ['#COMMA', u_p_goal_uid]]
					    ]
					  ],
					  u_is,
					  [quote, u_inference_only],
					  u_plausibility,
					  0.9
					]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_variabilize,
				      u_new_rule,
				      function(u_varize_object_c63),
				      [],
				      u_xx_link_slots_xx,
				      []
				    ]
				  ],
				  
				  [ u_ob_c36_add_unique_name,
				    u_new_rule,
				    
				    [ u_string_c62_symbol,
				      
				      [ u_string_append,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('P'),
						   #\('R'),
						   #\('E'),
						   #\('S'),
						   #\('E'),
						   #\('R'),
						   #\('V'),
						   #\('A'),
						   #\('T'),
						   #\('I'),
						   #\('O'),
						   #\('N'),
						   #\(-),
						   #\('I'),
						   #\('N'),
						   #\('F'),
						   #\('.')
						 ]),
					
					[ u_fixnum_c62_string,
					  u_xx_next_prule_number_xx
					]
				      ]
				    ]
				  ],
				  [u_add_rule_print, u_new_rule],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_fcreate,
				      
				      [ '#BQ',
					
					[ u_rule,
					  u_subgoal,
					  
					  [ '#COMMA',
					    
					    [ u_ob_c36_get,
					      u_leaf_cause,
					      [quote, u_obj]
					    ]
					  ],
					  u_goal,
					  
					  [ u_preservation,
					    u_obj,
					    ['#COMMA', u_failed_goal_obj],
					    u_uid,
					    [quote, ['#COMMA', u_p_goal_uid]]
					  ],
					  u_is,
					  [quote, u_plan_only],
					  u_plausibility,
					  0.9
					]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_new_rule,
				    
				    [ u_ob_c36_variabilize,
				      u_new_rule,
				      function(u_varize_object_c63),
				      [],
				      u_xx_link_slots_xx,
				      []
				    ]
				  ],
				  
				  [ u_ob_c36_add_unique_name,
				    u_new_rule,
				    
				    [ u_string_c62_symbol,
				      
				      [ u_string_append,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('P'),
						   #\('R'),
						   #\('E'),
						   #\('S'),
						   #\('E'),
						   #\('R'),
						   #\('V'),
						   #\('A'),
						   #\('T'),
						   #\('I'),
						   #\('O'),
						   #\('N'),
						   #\(-),
						   #\('P'),
						   #\('L'),
						   #\('A'),
						   #\('N'),
						   #\('.')
						 ]),
					
					[ u_fixnum_c62_string,
					  u_xx_next_prule_number_xx
					]
				      ]
				    ]
				  ],
				  [u_add_rule_print, u_new_rule],
				  [u_increment_me, u_xx_next_prule_number_xx],
				  [setq, u_input_states, []],
				  [setq, u_path, u_backwards_planning_path],
				  [setq, u_old_context, []],
				  
				  [ u_yloop,
				    
				    [ u_ydo,
				      [setq, u_prev_context, u_old_context],
				      [setq, u_old_context, [car, u_path]],
				      
				      [ setq,
					u_input_states,
					
					[ union,
					  u_input_states,
					  
					  [ u_cx_c36_input_states,
					    u_old_context
					  ]
					]
				      ],
				      [setq, u_path, [cdr, u_path]]
				    ],
				    [u_ywhile, u_path]
				  ],
				  
				  [ setq,
				    u_old_context,
				    
				    [ u_ob_c36_get,
				      u_old_top_level_goal,
				      [quote, u_activation_context]
				    ]
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
						 #\(u),
						 #\(n),
						 #\(d),
						 #\(o),
						 #\(' '),
						 #\(c),
						 #\(a),
						 #\(u),
						 #\(s),
						 #\(e)
					       ])
				    ],
				    
				    [ setq,
				      u_sprouted_context,
				      
				      [ u_reversal_sprout_alternative,
					u_old_context,
					u_old_top_level_goal,
					u_context,
					u_top_level_goal,
					1.0,
					t
				      ]
				    ],
				    [setq, u_xxcontext, u_sprouted_context],
				    
				    [ u_no_gen,
				      
				      [ u_cx_c36_assert_many,
					u_sprouted_context,
					u_input_states
				      ]
				    ],
				    
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
			  ]
			]
		      ],
		      u_sprouted_contexts
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_alternative, Alternative_Get),
	get_var(LEnv, u_an, An_Get),
	get_var(LEnv, u_past, Past_Get),
	get_var(LEnv, u_sprout, Sprout_Get),
	f_u_delay_dbgs([quote, u_to_be_set],
		       
		       [ 
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
				      #\(u),
				      #\(n),
				      #\(d),
				      #\(o),
				      #\(' '),
				      #\(c),
				      #\(a),
				      #\(u),
				      #\(s),
				      #\(e)
				    ])
			 ],
			 
			 [ setq,
			   u_sprouted_context,
			   
			   [ u_reversal_sprout_alternative,
			     u_old_context,
			     u_old_top_level_goal,
			     u_context,
			     u_top_level_goal,
			     1.0,
			     t
			   ]
			 ],
			 [setq, u_xxcontext, u_sprouted_context],
			 
			 [ u_no_gen,
			   
			   [ u_cx_c36_assert_many,
			     u_sprouted_context,
			     u_input_states
			   ]
			 ],
			 
			 [ setq,
			   u_sprouted_contexts,
			   [cons, u_sprouted_context, u_sprouted_contexts]
			 ]
		       ],
		       Delay_dbgs_Ret),
	LetResult=Delay_dbgs_Ret,
	LetResult=FnResult.
:- set_opv(f_u_reverse_undo_causes, classof, claz_function),
   set_opv(u_reverse_undo_causes, compile_as, kw_function),
   set_opv(u_reverse_undo_causes, function, f_u_reverse_undo_causes),
   DefunResult=u_reverse_undo_causes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" set up rand object", 9, 2060)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was UOR, but gets killed by vblz",
				     48,
				     2303)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" set up rules", 9, 2747)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" replan", 9, 4503)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              (yuntil ", 1, 4922)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("               (prog1", 1, 4946)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (progn", 1, 4969)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (cx$assert-many old-context input-states)",
				     1,
				     4993)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (not (show rand old-context *empty-bd* *me-belief-path*)))",
				     1,
				     5053)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (cx$retract-many old-context input-states)))",
				     1,
				     5130)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (if (null? prev-context)",
				     1,
				     5192)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("            (progn", 1, 5226)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (error \"null prev context\")",
				     1,
				     5246)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (setq prev-context (ob$get (car failed-goals)",
				     1,
				     5288)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                       'termination-context))))",
				     1,
				     5348)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This line added for new alg.", 9, 5421)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_reversal2.cl:126 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Sprout an alternative past context",
				     9,
				     5535)).
:- true.


% Total time: 1.78 seconds


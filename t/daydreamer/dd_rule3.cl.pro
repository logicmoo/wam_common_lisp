
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_rule3" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:14:12 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: We need to break this function into several to make it more",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:68 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" readable.", 1, 69)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-plan',
			    [goal, 'top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Run plan for ~A in ~A"),
			      goal,
			      context
			    ],
			    
			    [ if,
			      
			      [ not,
				
				[ 'planning-loop?',
				  goal,
				  context,
				  'top-level-goal',
				  'belief-path'
				]
			      ],
			      
			      [ let,
				
				[ ['goal-obj', ['ob$get', goal, [quote, obj]]],
				  ['sprouted-contexts', []],
				  [candidates, []],
				  
				  [ 'existing-analogical-ep?',
				    
				    [ 'ob$get',
				      goal,
				      [quote, 'analogical-episode']
				    ]
				  ]
				],
				
				[ if,
				  'existing-analogical-ep?',
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Try existing analogical plans")
				    ],
				    
				    [ yloop,
				      
				      [ yfor,
					episode,
					in,
					
					[ 'ob$gets',
					  goal,
					  [quote, 'analogical-episode']
					]
				      ],
				      
				      [ ydo,
					
					[ setq,
					  'sprouted-contexts',
					  
					  [ 'append!',
					    
					    [ 'try-analogical-plan',
					      goal,
					      'goal-obj',
					      context,
					      episode,
					      'belief-path',
					      'top-level-goal'
					    ],
					    'sprouted-contexts'
					  ]
					]
				      ]
				    ]
				  ]
				],
				
				[ if,
				  [],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Try mutation plans")
				    ],
				    
				    [ setq,
				      'sprouted-contexts',
				      
				      [ 'run-mutation-plans',
					goal,
					'top-level-goal',
					context
				      ]
				    ]
				  ]
				],
				
				[ if,
				  
				  [ and,
				    ['null?', 'sprouted-contexts'],
				    
				    [ not,
				      
				      [ 'ty$instance?',
					'goal-obj',
					[quote, action]
				      ]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Try fact plans")
				    ],
				    
				    [ setq,
				      'sprouted-contexts',
				      
				      [ 'run-fact-plan',
					goal,
					'top-level-goal',
					context,
					'belief-path'
				      ]
				    ]
				  ]
				],
				
				[ if,
				  ['null?', 'sprouted-contexts'],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Try rules and episodes")
				    ],
				    
				    [ setq,
				      candidates,
				      
				      [ 'find-candidate-rules',
					'goal-obj',
					[],
					'belief-path',
					context
				      ]
				    ],
				    
				    [ if,
				      
				      [ 'order-candidates',
					'goal-obj',
					candidates,
					'top-level-goal'
				      ],
				      
				      [ yloop,
					
					[ yfor,
					  episode,
					  in,
					  ['candidates->episodes', candidates]
					],
					
					[ ydo,
					  
					  [ setq,
					    'sprouted-contexts',
					    
					    [ 'append!',
					      
					      [ 'run-analogical-plan',
						goal,
						'goal-obj',
						context,
						['ob$get', episode, [quote, bd]],
						
						[ 'ob$get',
						  episode,
						  [quote, goal]
						],
						
						[ 'ob$get',
						  episode,
						  [quote, context]
						],
						
						[ 'ob$get',
						  episode,
						  [quote, rule]
						],
						
						[ 'ob$get',
						  episode,
						  [quote, ordering]
						],
						'belief-path',
						'top-level-goal',
						episode,
						t
					      ],
					      'sprouted-contexts'
					    ]
					  ]
					]
				      ],
				      
				      [ yloop,
					[yfor, candidate, in, candidates],
					
					[ ydo,
					  
					  [ if,
					    
					    [ not,
					      
					      [ 'constructed-plan?',
						['candidate-rule', candidate]
					      ]
					    ],
					    generic,
					    planning,
					    
					    [ but,
					      other,
					      ones,
					      ['#COMMA', 'e.g.'],
					      ['#COMMA', from],
					      reversal,
					      ['#COMMA', may],
					      'be?'
					    ],
					    
					    [ yloop,
					      [yfor, candidate, in, candidates],
					      
					      [ ydo,
						
						[ if,
						  
						  [ not,
						    
						    [ 'constructed-plan?',
						      
						      [ 'candidate-rule',
							candidate
						      ]
						    ]
						  ],
						  
						  [ progn,
						    
						    [ setq,
						      'sprouted-contexts',
						      
						      [ 'append!',
							
							[ 'run-generic-plan',
							  goal,
							  'goal-obj',
							  context,
							  
							  [ 'candidate-rule',
							    candidate
							  ],
							  'belief-path',
							  [],
							  
							  [ 'candidate-bd',
							    candidate
							  ],
							  'top-level-goal'
							],
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
				      
				      [ if,
					['null?', 'sprouted-contexts'],
					
					[ progn,
					  
					  [ 'ndbg-roman-nl',
					    '*gate-dbg*',
					    rule,
					    '$STRING'("Try believe others")
					  ],
					  
					  [ if,
					    
					    [ and,
					      
					      [ 'ty$instance?',
						'goal-obj',
						[quote, believe]
					      ],
					      
					      [ 'neq?',
						[car, 'belief-path'],
						
						[ 'ob$get',
						  'goal-obj',
						  [quote, actor]
						]
					      ],
					      
					      [ 'ty$instance?',
						
						[ 'ob$get',
						  'goal-obj',
						  [quote, obj]
						],
						[quote, 'mental-state']
					      ]
					    ],
					    
					    [ let,
					      
					      [ 
						[ 'new-goal-obj',
						  
						  [ 'ob$get',
						    'goal-obj',
						    [quote, obj]
						  ]
						]
					      ],
					      
					      [ yloop,
						
						[ initial,
						  
						  [ 'believe-other?',
						    
						    [ 'ob$get',
						      'goal-obj',
						      [quote, actor]
						    ]
						  ]
						],
						
						[ yfor,
						  rule,
						  in,
						  
						  [ 'collect-planning-rules',
						    'new-goal-obj'
						  ]
						],
						
						[ ydo,
						  
						  [ if,
						    ['plan?', rule],
						    
						    [ progn,
						      
						      [ setq,
							'sprouted-contexts',
							
							[ 'append!',
							  
							  [ 'try-generic-plan',
							    goal,
							    'new-goal-obj',
							    context,
							    rule,
							    'belief-path',
							    'believe-other?',
							    'top-level-goal'
							  ],
							  'sprouted-contexts'
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
				      
				      [ if,
					
					[ and,
					  ['null?', 'sprouted-contexts'],
					  
					  [ progn,
					    
					    [ 'ndbg-roman-nl',
					      '*gate-dbg*',
					      rule,
					      '$STRING'("Try subgoal relaxation")
					    ],
					    t
					  ],
					  
					  [ not,
					    
					    [ 'top-goal?',
					      goal,
					      context,
					      'belief-path'
					    ]
					  ],
					  
					  [ or,
					    [not, ['real?', 'top-level-goal']],
					    [not, ['vars-in?', 'goal-obj']]
					  ],
					  
					  [ not,
					    
					    [ 'ty$instance?',
					      'goal-obj',
					      [quote, action]
					    ]
					  ],
					  
					  [ not,
					    
					    [ 'ty$instance?',
					      'goal-obj',
					      [quote, minimization]
					    ]
					  ],
					  ['me-belief-path?', 'belief-path'],
					  
					  [ cond,
					    
					    [ 
					      [ or,
						['real?', 'top-level-goal'],
						
						[ 'imaginary-realistic?',
						  'top-level-goal'
						]
					      ],
					      
					      [ if,
						
						[ 'ob?',
						  
						  [ 'ob$pget',
						    goal,
						    [quote, [obj, actor]]
						  ]
						],
						
						[ if,
						  
						  [ and,
						    
						    [ 'ob$pget',
						      goal,
						      [quote, [obj, actor]]
						    ],
						    
						    [ 'not-me?',
						      
						      [ 'ob$pget',
							goal,
							[quote, [obj, actor]]
						      ]
						    ]
						  ],
						  t,
						  []
						],
						t
					      ]
					    ],
					    [else, t]
					  ]
					],
					
					[ let,
					  
					  [ 
					    [ 'sprouted-context',
					      ['cx$sprout', context]
					    ]
					  ],
					  
					  [ 'delay-dbgs',
					    'sprouted-context',
					    
					    [ 'set-ordering',
					      'sprouted-context',
					      0.1
					    ],
					    
					    [ 'ndbg-roman',
					      '*gate-dbg*',
					      rule,
					      '$STRING'("Subgoal relaxation")
					    ],
					    
					    [ 'ndbg-roman-nl',
					      '*gate-dbg*',
					      rule,
					      '$STRING'(", ~A succeeds"),
					      goal
					    ],
					    ['ndbg-newline', '*gate-dbg*', rule],
					    
					    [ 'gen-relaxation',
					      'sprouted-context',
					      
					      [ 'cx$assert-relative',
						'sprouted-context',
						'goal-obj',
						'belief-path'
					      ]
					    ],
					    
					    [ setq,
					      goal,
					      
					      [ 'make-goal-success',
						goal,
						'sprouted-context',
						[],
						'belief-path',
						'*empty-bd*'
					      ]
					    ],
					    
					    [ 'ob$pset',
					      goal,
					      [quote, [obj, strength]],
					      '*goal-relaxation-realism*'
					    ]
					  ],
					  
					  [ setq,
					    'sprouted-contexts',
					    [list, 'sprouted-context']
					  ]
					]
				      ],
				      'sprouted-contexts'
				    ],
				    []
				  ]
				],
				we,
				are,
				'OK'
			      ],
			      'ob$pset',
			      goal,
			      [quote, [obj, strength]],
			      '*goal-relaxation-realism*'
			    ],
			    
			    [ setq,
			      'sprouted-contexts',
			      [list, 'sprouted-context']
			    ]
			  ]).

% annotating U::RUN-PLAN 
wl: lambda_def(defun,
	      u_run_plan,
	      f_u_run_plan,
	      [u_goal, u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(n),
			     #\(' '),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(n),
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
		
		[ if,
		  
		  [ not,
		    
		    [ u_planning_loop_c63,
		      u_goal,
		      u_context,
		      u_top_level_goal,
		      u_belief_path
		    ]
		  ],
		  
		  [ let,
		    
		    [ [u_goal_obj, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		      [u_sprouted_contexts, []],
		      [u_candidates, []],
		      
		      [ u_existing_analogical_ep_c63,
			[u_ob_c36_get, u_goal, [quote, u_analogical_episode]]
		      ]
		    ],
		    
		    [ if,
		      u_existing_analogical_ep_c63,
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('T'),
				     #\(r),
				     #\(y),
				     #\(' '),
				     #\(e),
				     #\(x),
				     #\(i),
				     #\(s),
				     #\(t),
				     #\(i),
				     #\(n),
				     #\(g),
				     #\(' '),
				     #\(a),
				     #\(n),
				     #\(a),
				     #\(l),
				     #\(o),
				     #\(g),
				     #\(i),
				     #\(c),
				     #\(a),
				     #\(l),
				     #\(' '),
				     #\(p),
				     #\(l),
				     #\(a),
				     #\(n),
				     #\(s)
				   ])
			],
			
			[ u_yloop,
			  
			  [ u_yfor,
			    u_episode,
			    u_in,
			    
			    [ u_ob_c36_gets,
			      u_goal,
			      [quote, u_analogical_episode]
			    ]
			  ],
			  
			  [ u_ydo,
			    
			    [ setq,
			      u_sprouted_contexts,
			      
			      [ u_append_c33,
				
				[ u_try_analogical_plan,
				  u_goal,
				  u_goal_obj,
				  u_context,
				  u_episode,
				  u_belief_path,
				  u_top_level_goal
				],
				u_sprouted_contexts
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ if,
		      [],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('T'),
				     #\(r),
				     #\(y),
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
				     #\(s)
				   ])
			],
			
			[ setq,
			  u_sprouted_contexts,
			  
			  [ u_run_mutation_plans,
			    u_goal,
			    u_top_level_goal,
			    u_context
			  ]
			]
		      ]
		    ],
		    
		    [ if,
		      
		      [ and,
			[u_null_c63, u_sprouted_contexts],
			
			[ not,
			  [u_ty_c36_instance_c63, u_goal_obj, [quote, u_action]]
			]
		      ],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('T'),
				     #\(r),
				     #\(y),
				     #\(' '),
				     #\(f),
				     #\(a),
				     #\(c),
				     #\(t),
				     #\(' '),
				     #\(p),
				     #\(l),
				     #\(a),
				     #\(n),
				     #\(s)
				   ])
			],
			
			[ setq,
			  u_sprouted_contexts,
			  
			  [ u_run_fact_plan,
			    u_goal,
			    u_top_level_goal,
			    u_context,
			    u_belief_path
			  ]
			]
		      ]
		    ],
		    
		    [ if,
		      [u_null_c63, u_sprouted_contexts],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('T'),
				     #\(r),
				     #\(y),
				     #\(' '),
				     #\(r),
				     #\(u),
				     #\(l),
				     #\(e),
				     #\(s),
				     #\(' '),
				     #\(a),
				     #\(n),
				     #\(d),
				     #\(' '),
				     #\(e),
				     #\(p),
				     #\(i),
				     #\(s),
				     #\(o),
				     #\(d),
				     #\(e),
				     #\(s)
				   ])
			],
			
			[ setq,
			  u_candidates,
			  
			  [ u_find_candidate_rules,
			    u_goal_obj,
			    [],
			    u_belief_path,
			    u_context
			  ]
			],
			
			[ if,
			  
			  [ u_order_candidates,
			    u_goal_obj,
			    u_candidates,
			    u_top_level_goal
			  ],
			  
			  [ u_yloop,
			    
			    [ u_yfor,
			      u_episode,
			      u_in,
			      [u_candidates_c62_episodes, u_candidates]
			    ],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_sprouted_contexts,
				
				[ u_append_c33,
				  
				  [ u_run_analogical_plan,
				    u_goal,
				    u_goal_obj,
				    u_context,
				    [u_ob_c36_get, u_episode, [quote, u_bd]],
				    [u_ob_c36_get, u_episode, [quote, u_goal]],
				    [u_ob_c36_get, u_episode, [quote, u_context]],
				    [u_ob_c36_get, u_episode, [quote, u_rule]],
				    
				    [ u_ob_c36_get,
				      u_episode,
				      [quote, u_ordering]
				    ],
				    u_belief_path,
				    u_top_level_goal,
				    u_episode,
				    t
				  ],
				  u_sprouted_contexts
				]
			      ]
			    ]
			  ],
			  
			  [ u_yloop,
			    [u_yfor, u_candidate, u_in, u_candidates],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ not,
				  
				  [ u_constructed_plan_c63,
				    [u_candidate_rule, u_candidate]
				  ]
				],
				u_generic,
				u_planning,
				
				[ u_but,
				  u_other,
				  u_ones,
				  ['#COMMA', u_e_c46_g_c46],
				  ['#COMMA', u_from],
				  u_reversal,
				  ['#COMMA', u_may],
				  u_be_c63
				],
				
				[ u_yloop,
				  [u_yfor, u_candidate, u_in, u_candidates],
				  
				  [ u_ydo,
				    
				    [ if,
				      
				      [ not,
					
					[ u_constructed_plan_c63,
					  [u_candidate_rule, u_candidate]
					]
				      ],
				      
				      [ progn,
					
					[ setq,
					  u_sprouted_contexts,
					  
					  [ u_append_c33,
					    
					    [ u_run_generic_plan,
					      u_goal,
					      u_goal_obj,
					      u_context,
					      [u_candidate_rule, u_candidate],
					      u_belief_path,
					      [],
					      [u_candidate_bd, u_candidate],
					      u_top_level_goal
					    ],
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
			  
			  [ if,
			    [u_null_c63, u_sprouted_contexts],
			    
			    [ progn,
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('T'),
					   #\(r),
					   #\(y),
					   #\(' '),
					   #\(b),
					   #\(e),
					   #\(l),
					   #\(i),
					   #\(e),
					   #\(v),
					   #\(e),
					   #\(' '),
					   #\(o),
					   #\(t),
					   #\(h),
					   #\(e),
					   #\(r),
					   #\(s)
					 ])
			      ],
			      
			      [ if,
				
				[ and,
				  
				  [ u_ty_c36_instance_c63,
				    u_goal_obj,
				    [quote, u_believe]
				  ],
				  
				  [ u_neq_c63,
				    [car, u_belief_path],
				    [u_ob_c36_get, u_goal_obj, [quote, u_actor]]
				  ],
				  
				  [ u_ty_c36_instance_c63,
				    [u_ob_c36_get, u_goal_obj, [quote, u_obj]],
				    [quote, u_mental_state]
				  ]
				],
				
				[ let,
				  
				  [ 
				    [ u_new_goal_obj,
				      [u_ob_c36_get, u_goal_obj, [quote, u_obj]]
				    ]
				  ],
				  
				  [ u_yloop,
				    
				    [ u_initial,
				      
				      [ u_believe_other_c63,
					
					[ u_ob_c36_get,
					  u_goal_obj,
					  [quote, u_actor]
					]
				      ]
				    ],
				    
				    [ u_yfor,
				      u_rule,
				      u_in,
				      
				      [ u_collect_planning_rules,
					u_new_goal_obj
				      ]
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					[u_plan_c63, u_rule],
					
					[ progn,
					  
					  [ setq,
					    u_sprouted_contexts,
					    
					    [ u_append_c33,
					      
					      [ u_try_generic_plan,
						u_goal,
						u_new_goal_obj,
						u_context,
						u_rule,
						u_belief_path,
						u_believe_other_c63,
						u_top_level_goal
					      ],
					      u_sprouted_contexts
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
			  
			  [ if,
			    
			    [ and,
			      [u_null_c63, u_sprouted_contexts],
			      
			      [ progn,
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('T'),
					     #\(r),
					     #\(y),
					     #\(' '),
					     #\(s),
					     #\(u),
					     #\(b),
					     #\(g),
					     #\(o),
					     #\(a),
					     #\(l),
					     #\(' '),
					     #\(r),
					     #\(e),
					     #\(l),
					     #\(a),
					     #\(x),
					     #\(a),
					     #\(t),
					     #\(i),
					     #\(o),
					     #\(n)
					   ])
				],
				t
			      ],
			      
			      [ not,
				
				[ u_top_goal_c63,
				  u_goal,
				  u_context,
				  u_belief_path
				]
			      ],
			      
			      [ or,
				[not, [u_real_c63, u_top_level_goal]],
				[not, [u_vars_in_c63, u_goal_obj]]
			      ],
			      
			      [ not,
				
				[ u_ty_c36_instance_c63,
				  u_goal_obj,
				  [quote, u_action]
				]
			      ],
			      
			      [ not,
				
				[ u_ty_c36_instance_c63,
				  u_goal_obj,
				  [quote, u_minimization]
				]
			      ],
			      [u_me_belief_path_c63, u_belief_path],
			      
			      [ cond,
				
				[ 
				  [ or,
				    [u_real_c63, u_top_level_goal],
				    
				    [ u_imaginary_realistic_c63,
				      u_top_level_goal
				    ]
				  ],
				  
				  [ if,
				    
				    [ u_ob_c63,
				      
				      [ u_ob_c36_pget,
					u_goal,
					[quote, [u_obj, u_actor]]
				      ]
				    ],
				    
				    [ if,
				      
				      [ and,
					
					[ u_ob_c36_pget,
					  u_goal,
					  [quote, [u_obj, u_actor]]
					],
					
					[ u_not_me_c63,
					  
					  [ u_ob_c36_pget,
					    u_goal,
					    [quote, [u_obj, u_actor]]
					  ]
					]
				      ],
				      t,
				      []
				    ],
				    t
				  ]
				],
				[u_else, t]
			      ]
			    ],
			    
			    [ let,
			      
			      [ 
				[ u_sprouted_context,
				  [u_cx_c36_sprout, u_context]
				]
			      ],
			      
			      [ u_delay_dbgs,
				u_sprouted_context,
				[u_set_ordering, u_sprouted_context, 0.1],
				
				[ u_ndbg_roman,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('S'),
					     #\(u),
					     #\(b),
					     #\(g),
					     #\(o),
					     #\(a),
					     #\(l),
					     #\(' '),
					     #\(r),
					     #\(e),
					     #\(l),
					     #\(a),
					     #\(x),
					     #\(a),
					     #\(t),
					     #\(i),
					     #\(o),
					     #\(n)
					   ])
				],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(','),
					     #\(' '),
					     #\(~),
					     #\('A'),
					     #\(' '),
					     #\(s),
					     #\(u),
					     #\(c),
					     #\(c),
					     #\(e),
					     #\(e),
					     #\(d),
					     #\(s)
					   ]),
				  u_goal
				],
				[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
				
				[ u_gen_relaxation,
				  u_sprouted_context,
				  
				  [ u_cx_c36_assert_relative,
				    u_sprouted_context,
				    u_goal_obj,
				    u_belief_path
				  ]
				],
				
				[ setq,
				  u_goal,
				  
				  [ u_make_goal_success,
				    u_goal,
				    u_sprouted_context,
				    [],
				    u_belief_path,
				    u_xx_empty_bd_xx
				  ]
				],
				
				[ u_ob_c36_pset,
				  u_goal,
				  [quote, [u_obj, u_strength]],
				  u_xx_goal_relaxation_realism_xx
				]
			      ],
			      
			      [ setq,
				u_sprouted_contexts,
				[list, u_sprouted_context]
			      ]
			    ]
			  ],
			  u_sprouted_contexts
			],
			[]
		      ]
		    ],
		    u_we,
		    u_are,
		    u_ok
		  ],
		  u_ob_c36_pset,
		  u_goal,
		  [quote, [u_obj, u_strength]],
		  u_xx_goal_relaxation_realism_xx
		],
		[setq, u_sprouted_contexts, [list, u_sprouted_context]]
	      ]).


% annotating U::RUN-PLAN 
wl: arglist_info(u_run_plan,
		[u_goal, u_top_level_goal, u_context, u_belief_path],
		
		[ Goal_Param,
		  Top_level_goal_Param,
		  Context_Param,
		  Belief_path_Param
		],
		arginfo{ all:[u_goal, u_top_level_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_top_level_goal,
				 u_context,
				 u_belief_path
			       ],
			 opt:0,
			 req:[u_goal, u_top_level_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-PLAN 
wl: init_args(exact_only, u_run_plan).


% annotating U::RUN-PLAN 
f_u_run_plan(Goal_Param, Top_level_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(n),
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
	cl_if(
	      [ not,
		
		[ u_planning_loop_c63,
		  u_goal,
		  u_context,
		  u_top_level_goal,
		  u_belief_path
		]
	      ],
	      
	      [ let,
		
		[ [u_goal_obj, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		  [u_sprouted_contexts, []],
		  [u_candidates, []],
		  
		  [ u_existing_analogical_ep_c63,
		    [u_ob_c36_get, u_goal, [quote, u_analogical_episode]]
		  ]
		],
		
		[ if,
		  u_existing_analogical_ep_c63,
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('T'),
				 #\(r),
				 #\(y),
				 #\(' '),
				 #\(e),
				 #\(x),
				 #\(i),
				 #\(s),
				 #\(t),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\(' '),
				 #\(a),
				 #\(n),
				 #\(a),
				 #\(l),
				 #\(o),
				 #\(g),
				 #\(i),
				 #\(c),
				 #\(a),
				 #\(l),
				 #\(' '),
				 #\(p),
				 #\(l),
				 #\(a),
				 #\(n),
				 #\(s)
			       ])
		    ],
		    
		    [ u_yloop,
		      
		      [ u_yfor,
			u_episode,
			u_in,
			[u_ob_c36_gets, u_goal, [quote, u_analogical_episode]]
		      ],
		      
		      [ u_ydo,
			
			[ setq,
			  u_sprouted_contexts,
			  
			  [ u_append_c33,
			    
			    [ u_try_analogical_plan,
			      u_goal,
			      u_goal_obj,
			      u_context,
			      u_episode,
			      u_belief_path,
			      u_top_level_goal
			    ],
			    u_sprouted_contexts
			  ]
			]
		      ]
		    ]
		  ]
		],
		
		[ if,
		  [],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('T'),
				 #\(r),
				 #\(y),
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
				 #\(s)
			       ])
		    ],
		    
		    [ setq,
		      u_sprouted_contexts,
		      [u_run_mutation_plans, u_goal, u_top_level_goal, u_context]
		    ]
		  ]
		],
		
		[ if,
		  
		  [ and,
		    [u_null_c63, u_sprouted_contexts],
		    [not, [u_ty_c36_instance_c63, u_goal_obj, [quote, u_action]]]
		  ],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('T'),
				 #\(r),
				 #\(y),
				 #\(' '),
				 #\(f),
				 #\(a),
				 #\(c),
				 #\(t),
				 #\(' '),
				 #\(p),
				 #\(l),
				 #\(a),
				 #\(n),
				 #\(s)
			       ])
		    ],
		    
		    [ setq,
		      u_sprouted_contexts,
		      
		      [ u_run_fact_plan,
			u_goal,
			u_top_level_goal,
			u_context,
			u_belief_path
		      ]
		    ]
		  ]
		],
		
		[ if,
		  [u_null_c63, u_sprouted_contexts],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('T'),
				 #\(r),
				 #\(y),
				 #\(' '),
				 #\(r),
				 #\(u),
				 #\(l),
				 #\(e),
				 #\(s),
				 #\(' '),
				 #\(a),
				 #\(n),
				 #\(d),
				 #\(' '),
				 #\(e),
				 #\(p),
				 #\(i),
				 #\(s),
				 #\(o),
				 #\(d),
				 #\(e),
				 #\(s)
			       ])
		    ],
		    
		    [ setq,
		      u_candidates,
		      
		      [ u_find_candidate_rules,
			u_goal_obj,
			[],
			u_belief_path,
			u_context
		      ]
		    ],
		    
		    [ if,
		      
		      [ u_order_candidates,
			u_goal_obj,
			u_candidates,
			u_top_level_goal
		      ],
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_episode,
			  u_in,
			  [u_candidates_c62_episodes, u_candidates]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_sprouted_contexts,
			    
			    [ u_append_c33,
			      
			      [ u_run_analogical_plan,
				u_goal,
				u_goal_obj,
				u_context,
				[u_ob_c36_get, u_episode, [quote, u_bd]],
				[u_ob_c36_get, u_episode, [quote, u_goal]],
				[u_ob_c36_get, u_episode, [quote, u_context]],
				[u_ob_c36_get, u_episode, [quote, u_rule]],
				[u_ob_c36_get, u_episode, [quote, u_ordering]],
				u_belief_path,
				u_top_level_goal,
				u_episode,
				t
			      ],
			      u_sprouted_contexts
			    ]
			  ]
			]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_candidate, u_in, u_candidates],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ not,
			      
			      [ u_constructed_plan_c63,
				[u_candidate_rule, u_candidate]
			      ]
			    ],
			    u_generic,
			    u_planning,
			    
			    [ u_but,
			      u_other,
			      u_ones,
			      ['#COMMA', u_e_c46_g_c46],
			      ['#COMMA', u_from],
			      u_reversal,
			      ['#COMMA', u_may],
			      u_be_c63
			    ],
			    
			    [ u_yloop,
			      [u_yfor, u_candidate, u_in, u_candidates],
			      
			      [ u_ydo,
				
				[ if,
				  
				  [ not,
				    
				    [ u_constructed_plan_c63,
				      [u_candidate_rule, u_candidate]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ setq,
				      u_sprouted_contexts,
				      
				      [ u_append_c33,
					
					[ u_run_generic_plan,
					  u_goal,
					  u_goal_obj,
					  u_context,
					  [u_candidate_rule, u_candidate],
					  u_belief_path,
					  [],
					  [u_candidate_bd, u_candidate],
					  u_top_level_goal
					],
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
		      
		      [ if,
			[u_null_c63, u_sprouted_contexts],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
				       #\(r),
				       #\(y),
				       #\(' '),
				       #\(b),
				       #\(e),
				       #\(l),
				       #\(i),
				       #\(e),
				       #\(v),
				       #\(e),
				       #\(' '),
				       #\(o),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(r),
				       #\(s)
				     ])
			  ],
			  
			  [ if,
			    
			    [ and,
			      
			      [ u_ty_c36_instance_c63,
				u_goal_obj,
				[quote, u_believe]
			      ],
			      
			      [ u_neq_c63,
				[car, u_belief_path],
				[u_ob_c36_get, u_goal_obj, [quote, u_actor]]
			      ],
			      
			      [ u_ty_c36_instance_c63,
				[u_ob_c36_get, u_goal_obj, [quote, u_obj]],
				[quote, u_mental_state]
			      ]
			    ],
			    
			    [ let,
			      
			      [ 
				[ u_new_goal_obj,
				  [u_ob_c36_get, u_goal_obj, [quote, u_obj]]
				]
			      ],
			      
			      [ u_yloop,
				
				[ u_initial,
				  
				  [ u_believe_other_c63,
				    [u_ob_c36_get, u_goal_obj, [quote, u_actor]]
				  ]
				],
				
				[ u_yfor,
				  u_rule,
				  u_in,
				  [u_collect_planning_rules, u_new_goal_obj]
				],
				
				[ u_ydo,
				  
				  [ if,
				    [u_plan_c63, u_rule],
				    
				    [ progn,
				      
				      [ setq,
					u_sprouted_contexts,
					
					[ u_append_c33,
					  
					  [ u_try_generic_plan,
					    u_goal,
					    u_new_goal_obj,
					    u_context,
					    u_rule,
					    u_belief_path,
					    u_believe_other_c63,
					    u_top_level_goal
					  ],
					  u_sprouted_contexts
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
		      
		      [ if,
			
			[ and,
			  [u_null_c63, u_sprouted_contexts],
			  
			  [ progn,
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('T'),
					 #\(r),
					 #\(y),
					 #\(' '),
					 #\(s),
					 #\(u),
					 #\(b),
					 #\(g),
					 #\(o),
					 #\(a),
					 #\(l),
					 #\(' '),
					 #\(r),
					 #\(e),
					 #\(l),
					 #\(a),
					 #\(x),
					 #\(a),
					 #\(t),
					 #\(i),
					 #\(o),
					 #\(n)
				       ])
			    ],
			    t
			  ],
			  
			  [ not,
			    [u_top_goal_c63, u_goal, u_context, u_belief_path]
			  ],
			  
			  [ or,
			    [not, [u_real_c63, u_top_level_goal]],
			    [not, [u_vars_in_c63, u_goal_obj]]
			  ],
			  
			  [ not,
			    
			    [ u_ty_c36_instance_c63,
			      u_goal_obj,
			      [quote, u_action]
			    ]
			  ],
			  
			  [ not,
			    
			    [ u_ty_c36_instance_c63,
			      u_goal_obj,
			      [quote, u_minimization]
			    ]
			  ],
			  [u_me_belief_path_c63, u_belief_path],
			  
			  [ cond,
			    
			    [ 
			      [ or,
				[u_real_c63, u_top_level_goal],
				[u_imaginary_realistic_c63, u_top_level_goal]
			      ],
			      
			      [ if,
				
				[ u_ob_c63,
				  
				  [ u_ob_c36_pget,
				    u_goal,
				    [quote, [u_obj, u_actor]]
				  ]
				],
				
				[ if,
				  
				  [ and,
				    
				    [ u_ob_c36_pget,
				      u_goal,
				      [quote, [u_obj, u_actor]]
				    ],
				    
				    [ u_not_me_c63,
				      
				      [ u_ob_c36_pget,
					u_goal,
					[quote, [u_obj, u_actor]]
				      ]
				    ]
				  ],
				  t,
				  []
				],
				t
			      ]
			    ],
			    [u_else, t]
			  ]
			],
			
			[ let,
			  [[u_sprouted_context, [u_cx_c36_sprout, u_context]]],
			  
			  [ u_delay_dbgs,
			    u_sprouted_context,
			    [u_set_ordering, u_sprouted_context, 0.1],
			    
			    [ u_ndbg_roman,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('S'),
					 #\(u),
					 #\(b),
					 #\(g),
					 #\(o),
					 #\(a),
					 #\(l),
					 #\(' '),
					 #\(r),
					 #\(e),
					 #\(l),
					 #\(a),
					 #\(x),
					 #\(a),
					 #\(t),
					 #\(i),
					 #\(o),
					 #\(n)
				       ])
			    ],
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(','),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(s),
					 #\(u),
					 #\(c),
					 #\(c),
					 #\(e),
					 #\(e),
					 #\(d),
					 #\(s)
				       ]),
			      u_goal
			    ],
			    [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			    
			    [ u_gen_relaxation,
			      u_sprouted_context,
			      
			      [ u_cx_c36_assert_relative,
				u_sprouted_context,
				u_goal_obj,
				u_belief_path
			      ]
			    ],
			    
			    [ setq,
			      u_goal,
			      
			      [ u_make_goal_success,
				u_goal,
				u_sprouted_context,
				[],
				u_belief_path,
				u_xx_empty_bd_xx
			      ]
			    ],
			    
			    [ u_ob_c36_pset,
			      u_goal,
			      [quote, [u_obj, u_strength]],
			      u_xx_goal_relaxation_realism_xx
			    ]
			  ],
			  [setq, u_sprouted_contexts, [list, u_sprouted_context]]
			]
		      ],
		      u_sprouted_contexts
		    ],
		    []
		  ]
		],
		u_we,
		u_are,
		u_ok
	      ],
	      u_ob_c36_pset,
	      u_goal,
	      [quote, [u_obj, u_strength]],
	      u_xx_goal_relaxation_realism_xx,
	      Xx_goal_relaxation_realism_xx),
	get_var(Env, u_sprouted_context, Sprouted_context_Get),
	Sprouted_contexts=[Sprouted_context_Get],
	set_var(Env, u_sprouted_contexts, Sprouted_contexts),
	Sprouted_contexts=FnResult.
:- set_opv(f_u_run_plan, classof, claz_function),
   set_opv(u_run_plan, compile_as, kw_function),
   set_opv(u_run_plan, function, f_u_run_plan),
   DefunResult=u_run_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: In fact the below is necessary to give 'fact plans' priority",
				     4,
				     446)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" over generic planning so that action mutations function to",
				     4,
				     518)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" provide new possibilities. Only this isn't implemented clearly.",
				     4,
				     582)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" But I got rid of this since fact plans are now in here.",
				     1,
				     648)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   (if (cx$retrieve-relative context goal-obj belief-path)",
				     1,
				     706)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (progn (ndbg-roman-nl *gate-dbg* rule",
				     1,
				     766)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                \"Warning: Goal obj ~A already true in ~A\"",
				     1,
				     812)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                    goal-obj context)",
				     1,
				     871)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("              nil))", 1, 910)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 935)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Highest priority--existing analogical plan.",
				     5,
				     941)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 991)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Ordering is 1.0: There should only be one",
				     21,
				     1263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" analogical goal at lower levels anyway--I don't know why",
				     21,
				     1327)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" this is a loop.", 21, 1406)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 1654)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Mutation plans are next highest priority.",
				     5,
				     1660)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 1708)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(and (me-belief-path? belief-path)",
				     13,
				     1722)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (null? sprouted-contexts)", 14, 1771)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$gets top-level-goal 'mutation-plan-contexts))",
				     14,
				     1812)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 2127)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Facts are next highest priority.",
				     5,
				     2133)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 2172)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Actions cannot be satisfied by fact plans, though.",
				     5,
				     2178)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 2235)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 2544)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Otherwise, try analogical plans or generic rules, depending.",
				     5,
				     2550)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 2617)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Get candidate rules and episodes",
				     5,
				     2623)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If there are any episodes whose ordering is greater than",
				     3,
				     2878)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 0.0, go for analogical planning; otherwise use generic",
				     3,
				     2939)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" planning.", 3, 2998)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (Since leaf goals shouldn't be indexed, these analogical plans",
				     3,
				     3012)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" WILL go through, and so we don't need to have generic plans",
				     3,
				     3079)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" take over if no analogical plans fire).",
				     3,
				     3143)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Rules generated automatically from input episodes are not used in",
				     7,
				     3912)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" generic planning (but other ones, e.g., from reversal, may be?)",
				     7,
				     3986)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 4666)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If still no luck, try believe others.",
				     5,
				     4672)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 4716)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (or (ty$instance? (ob$get goal-obj 'obj)",
				     1,
				     5016)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                   'mental-state)",
				     1,
				     5075)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                     (ty$instance? (ob$get goal-obj 'obj)",
				     1,
				     5126)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                   'personal-attribute))",
				     1,
				     5185)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 5855)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Eventually apply analogical plans in a relaxed way here?",
				     5,
				     5861)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 5924)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If still no luck, employ reality relaxation to a high degree",
				     5,
				     5930)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" and assume goal succeeds", 5, 5997)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 5, 6028)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" for now...", 46, 6324)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Can only relax goals involving other actors than me for",
				     14,
				     6501)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" real planning.", 14, 6572)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(imaginary-fanciful? top-level-goal)",
				     21,
				     6957)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Can only do below if goal is fully instantiated.",
				     14,
				     7446)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" could just wait..", 45, 7621)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Have to copy goal objective!! This setting will affect",
				     13,
				     7653)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" other contexts! (If make-goal-success does deep copy, however,",
				     13,
				     7728)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule3.cl:80 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" we are OK).", 13, 7805)).
:- true.


% Total time: 1.335 seconds


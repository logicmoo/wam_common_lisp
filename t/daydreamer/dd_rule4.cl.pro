
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_rule4" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:14:13 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'planning-loop?',
			    [goal, context, 'top-level-goal', 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-xtra',
			      '$STRING'("Plan loop check ~A"),
			      goal
			    ],
			    
			    [ let,
			      
			      [ 
				[ 'pl?',
				  
				  [ 'mem?',
				    
				    [ lambda,
				      [x, y],
				      
				      [ and,
					
					[ not,
					  
					  [ 'vars-in?',
					    ['ob$get', x, [quote, obj]]
					  ]
					],
					
					[ not,
					  
					  [ 'vars-in?',
					    ['ob$get', y, [quote, obj]]
					  ]
					],
					
					[ 'ob$unify',
					  ['ob$get', x, [quote, obj]],
					  ['ob$get', y, [quote, obj]],
					  '*empty-bd*'
					]
				      ]
				    ],
				    goal,
				    
				    [ cdr,
				      
				      [ 'goal-supergoals',
					goal,
					context,
					'belief-path'
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				'pl?',
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Is loop.")
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'rule-xtra',
				  '$STRING'("Is not loop.")
				]
			      ],
			      'pl?'
			    ]
			  ]).

% annotating U::PLANNING-LOOP? 
wl: lambda_def(defun,
	      u_planning_loop_c63,
	      f_u_planning_loop_c63,
	      [u_goal, u_context, u_top_level_goal, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_xtra,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(' '),
			     #\(l),
			     #\(o),
			     #\(o),
			     #\(p),
			     #\(' '),
			     #\(c),
			     #\(h),
			     #\(e),
			     #\(c),
			     #\(k),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_goal
		],
		
		[ let,
		  
		  [ 
		    [ u_pl_c63,
		      
		      [ u_mem_c63,
			
			[ lambda,
			  [u_x, u_y],
			  
			  [ and,
			    
			    [ not,
			      [u_vars_in_c63, [u_ob_c36_get, u_x, [quote, u_obj]]]
			    ],
			    
			    [ not,
			      [u_vars_in_c63, [u_ob_c36_get, u_y, [quote, u_obj]]]
			    ],
			    
			    [ u_ob_c36_unify,
			      [u_ob_c36_get, u_x, [quote, u_obj]],
			      [u_ob_c36_get, u_y, [quote, u_obj]],
			      u_xx_empty_bd_xx
			    ]
			  ]
			],
			u_goal,
			
			[ cdr,
			  [u_goal_supergoals, u_goal, u_context, u_belief_path]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_pl_c63,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('I'),
				 #\(s),
				 #\(' '),
				 #\(l),
				 #\(o),
				 #\(o),
				 #\(p),
				 #\('.')
			       ])
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule_xtra,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('I'),
				 #\(s),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(l),
				 #\(o),
				 #\(o),
				 #\(p),
				 #\('.')
			       ])
		    ]
		  ],
		  u_pl_c63
		]
	      ]).


% annotating U::PLANNING-LOOP? 
wl: arglist_info(u_planning_loop_c63,
		[u_goal, u_context, u_top_level_goal, u_belief_path],
		
		[ Goal_Param,
		  Context_Param,
		  Top_level_goal_Param,
		  Belief_path_Param
		],
		arginfo{ all:[u_goal, u_context, u_top_level_goal, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_context,
				 u_top_level_goal,
				 u_belief_path
			       ],
			 opt:0,
			 req:[u_goal, u_context, u_top_level_goal, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PLANNING-LOOP? 
wl: init_args(exact_only, u_planning_loop_c63).


% annotating U::PLANNING-LOOP? 
f_u_planning_loop_c63(Goal_Param, Context_Param, Top_level_goal_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(' '),
				       #\(l),
				       #\(o),
				       #\(o),
				       #\(p),
				       #\(' '),
				       #\(c),
				       #\(h),
				       #\(e),
				       #\(c),
				       #\(k),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_goal
			  ],
			  Roman_nl_Ret),
	f_u_mem_c63(
		    [ lambda,
		      [u_x, u_y],
		      
		      [ and,
			[not, [u_vars_in_c63, [u_ob_c36_get, u_x, [quote, u_obj]]]],
			[not, [u_vars_in_c63, [u_ob_c36_get, u_y, [quote, u_obj]]]],
			
			[ u_ob_c36_unify,
			  [u_ob_c36_get, u_x, [quote, u_obj]],
			  [u_ob_c36_get, u_y, [quote, u_obj]],
			  u_xx_empty_bd_xx
			]
		      ]
		    ],
		    u_goal,
		    [cdr, [u_goal_supergoals, u_goal, u_context, u_belief_path]],
		    Pl_c63_Init),
	LEnv=[[bv(u_pl_c63, Pl_c63_Init)]|Env],
	get_var(LEnv, u_pl_c63, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(s),
					   #\(' '),
					   #\(l),
					   #\(o),
					   #\(o),
					   #\(p),
					   #\('.')
					 ])
			      ],
			      TrueResult),
	    _115254=TrueResult
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_xtra,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(s),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(l),
					   #\(o),
					   #\(o),
					   #\(p),
					   #\('.')
					 ])
			      ],
			      ElseResult),
	    _115254=ElseResult
	),
	get_var(LEnv, u_pl_c63, Pl_c63_Get27),
	LetResult=Pl_c63_Get27,
	LetResult=FnResult.
:- set_opv(f_u_planning_loop_c63, classof, claz_function),
   set_opv(u_planning_loop_c63, compile_as, kw_function),
   set_opv(u_planning_loop_c63, function, f_u_planning_loop_c63),
   DefunResult=u_planning_loop_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 905)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate-o',
			    [ob, bd],
			    
			    [ 'ob$instan-omit',
			      ob,
			      bd,
			      function('varize-object?'),
			      [],
			      '*link-slots*',
			      []
			    ]
			  ]).

% annotating U::OB$INSTANTIATE-O 
wl: lambda_def(defun,
	      u_ob_c36_instantiate_o,
	      f_u_ob_c36_instantiate_o,
	      [u_ob, u_bd],
	      
	      [ 
		[ u_ob_c36_instan_omit,
		  u_ob,
		  u_bd,
		  function(u_varize_object_c63),
		  [],
		  u_xx_link_slots_xx,
		  []
		]
	      ]).


% annotating U::OB$INSTANTIATE-O 
wl: arglist_info(u_ob_c36_instantiate_o,
		[u_ob, u_bd],
		[Ob_Param, Bd_Param],
		arginfo{ all:[u_ob, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_bd],
			 opt:0,
			 req:[u_ob, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTANTIATE-O 
wl: init_args(exact_only, u_ob_c36_instantiate_o).


% annotating U::OB$INSTANTIATE-O 
f_u_ob_c36_instantiate_o(Ob_Param, Bd_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_bd, Bd_Param)],
	get_var(Env, u_xx_link_slots_xx, Xx_link_slots_xx_Get),
	f_u_ob_c36_instan_omit(Ob_Param,
			       Bd_Param,
			       function(u_varize_object_c63),
			       [],
			       Xx_link_slots_xx_Get,
			       [],
			       Instan_omit_Ret),
	Instan_omit_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate_o, classof, claz_function),
   set_opv(u_ob_c36_instantiate_o, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate_o, function, f_u_ob_c36_instantiate_o),
   DefunResult=u_ob_c36_instantiate_o.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" subgoal-objs should be in correct forward seq order",
				     1,
				     736)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 790)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" plan-no-gen :", 1, 792)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      nil = do generate subgoal always",
				     1,
				     808)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        t = never generate subgoal",
				     1,
				     848)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" activate = generate only when activated",
				     1,
				     884)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  outcome = generate only on outcome",
				     1,
				     926)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:638 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 964)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:965 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ga-gen-on-activate?',
			    ['gen-advice'],
			    
			    [ or,
			      ['null?', 'gen-advice'],
			      ['eq?', 'gen-advice', [quote, activate]]
			    ]
			  ]).

% annotating U::GA-GEN-ON-ACTIVATE? 
wl: lambda_def(defun,
	      u_ga_gen_on_activate_c63,
	      f_u_ga_gen_on_activate_c63,
	      [u_gen_advice],
	      
	      [ 
		[ or,
		  [u_null_c63, u_gen_advice],
		  [u_eq_c63, u_gen_advice, [quote, u_activate]]
		]
	      ]).


% annotating U::GA-GEN-ON-ACTIVATE? 
wl: arglist_info(u_ga_gen_on_activate_c63,
		[u_gen_advice],
		[Gen_advice_Param],
		arginfo{ all:[u_gen_advice],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_gen_advice],
			 opt:0,
			 req:[u_gen_advice],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GA-GEN-ON-ACTIVATE? 
wl: init_args(exact_only, u_ga_gen_on_activate_c63).


% annotating U::GA-GEN-ON-ACTIVATE? 
f_u_ga_gen_on_activate_c63(Gen_advice_Param, FnResult) :-
	Env=[bv(u_gen_advice, Gen_advice_Param)],
	(   f_u_null_c63(u_gen_advice, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_eq_c63(u_gen_advice, [quote, u_activate], Eq_c63_Ret),
	    FnResult=Eq_c63_Ret
	).
:- set_opv(f_u_ga_gen_on_activate_c63, classof, claz_function),
   set_opv(u_ga_gen_on_activate_c63, compile_as, kw_function),
   set_opv(u_ga_gen_on_activate_c63, function, f_u_ga_gen_on_activate_c63),
   DefunResult=u_ga_gen_on_activate_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:1066 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ga-gen-on-outcome?',
			    ['gen-advice'],
			    
			    [ or,
			      ['null?', 'gen-advice'],
			      ['eq?', 'gen-advice', [quote, outcome]]
			    ]
			  ]).

% annotating U::GA-GEN-ON-OUTCOME? 
wl: lambda_def(defun,
	      u_ga_gen_on_outcome_c63,
	      f_u_ga_gen_on_outcome_c63,
	      [u_gen_advice],
	      
	      [ 
		[ or,
		  [u_null_c63, u_gen_advice],
		  [u_eq_c63, u_gen_advice, [quote, u_outcome]]
		]
	      ]).


% annotating U::GA-GEN-ON-OUTCOME? 
wl: arglist_info(u_ga_gen_on_outcome_c63,
		[u_gen_advice],
		[Gen_advice_Param],
		arginfo{ all:[u_gen_advice],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_gen_advice],
			 opt:0,
			 req:[u_gen_advice],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GA-GEN-ON-OUTCOME? 
wl: init_args(exact_only, u_ga_gen_on_outcome_c63).


% annotating U::GA-GEN-ON-OUTCOME? 
f_u_ga_gen_on_outcome_c63(Gen_advice_Param, FnResult) :-
	Env=[bv(u_gen_advice, Gen_advice_Param)],
	(   f_u_null_c63(u_gen_advice, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_eq_c63(u_gen_advice, [quote, u_outcome], Eq_c63_Ret),
	    FnResult=Eq_c63_Ret
	).
:- set_opv(f_u_ga_gen_on_outcome_c63, classof, claz_function),
   set_opv(u_ga_gen_on_outcome_c63, compile_as, kw_function),
   set_opv(u_ga_gen_on_outcome_c63, function, f_u_ga_gen_on_outcome_c63),
   DefunResult=u_ga_gen_on_outcome_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:1165 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'instan-and-activate-subgoals',
			    
			    [ goal,
			      'subgoal-objs',
			      bd,
			      rule,
			      'sprouted-context',
			      'seq?',
			      'analogical-subgoals',
			      'believe-other?',
			      'top-level-goal',
			      'belief-path'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Instantiate and activate subgoals")
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				[subgoalnum, 0],
				['active-subgoals', []],
				['analogical-subgoal', []],
				
				[ 'plan-no-gen',
				  ['ob$get', rule, [quote, 'plan-no-gen']]
				],
				['gen-advice', []]
			      ],
			      [yfor, 'subgoal-obj', in, 'subgoal-objs'],
			      
			      [ ydo,
				
				[ if,
				  'analogical-subgoals',
				  
				  [ setq,
				    'analogical-subgoal',
				    [car, 'analogical-subgoals']
				  ]
				],
				
				[ if,
				  'plan-no-gen',
				  
				  [ progn,
				    [setq, 'gen-advice', [car, 'plan-no-gen']],
				    [setq, 'plan-no-gen', [cdr, 'plan-no-gen']]
				  ]
				],
				
				[ setq,
				  'subgoal-obj',
				  ['ob$instantiate-o', 'subgoal-obj', bd]
				],
				
				[ if,
				  ['ty$instance?', 'subgoal-obj', [quote, rnot]],
				  
				  [ 'ob$set',
				    'subgoal-obj',
				    [quote, type],
				    '*not-ob*'
				  ]
				],
				
				[ if,
				  'believe-other?',
				  
				  [ setq,
				    'subgoal-obj',
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'BELIEVE',
					  actor,
					  ['#COMMA', 'believe-other?'],
					  obj,
					  ['#COMMA', 'subgoal-obj']
					]
				      ]
				    ]
				  ],
				  
				  [ 'ob$set',
				    'subgoal-obj',
				    [quote, 'plan-rule'],
				    rule
				  ]
				],
				
				[ 'ob$set',
				  'subgoal-obj',
				  [quote, 'plan-subgoalnum'],
				  subgoalnum
				],
				
				[ setq,
				  'active-subgoals',
				  
				  [ cons,
				    
				    [ 'activate-subgoal',
				      goal,
				      'subgoal-obj',
				      'sprouted-context',
				      rule,
				      'analogical-subgoal',
				      'seq?',
				      'belief-path',
				      'top-level-goal',
				      'gen-advice'
				    ],
				    'active-subgoals'
				  ]
				],
				
				[ if,
				  'analogical-subgoals',
				  
				  [ setq,
				    'analogical-subgoals',
				    [cdr, 'analogical-subgoals']
				  ]
				],
				[setq, subgoalnum, [+, 1, subgoalnum]]
			      ],
			      
			      [ yresult,
				
				[ progn,
				  
				  [ setq,
				    'active-subgoals',
				    [reverse, 'active-subgoals']
				  ],
				  [if, 'seq?', ['make-seq', 'active-subgoals']],
				  'active-subgoals'
				]
			      ]
			    ]
			  ]).

% annotating U::INSTAN-AND-ACTIVATE-SUBGOALS 
wl: lambda_def(defun,
	      u_instan_and_activate_subgoals,
	      f_u_instan_and_activate_subgoals,
	      
	      [ u_goal,
		u_subgoal_objs,
		u_bd,
		u_rule,
		u_sprouted_context,
		u_seq_c63,
		u_analogical_subgoals,
		u_believe_other_c63,
		u_top_level_goal,
		u_belief_path
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('I'),
			     #\(n),
			     #\(s),
			     #\(t),
			     #\(a),
			     #\(n),
			     #\(t),
			     #\(i),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(' '),
			     #\(a),
			     #\(n),
			     #\(d),
			     #\(' '),
			     #\(a),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(v),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(' '),
			     #\(s),
			     #\(u),
			     #\(b),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
			     #\(s)
			   ])
		],
		
		[ u_yloop,
		  
		  [ u_initial,
		    [u_subgoalnum, 0],
		    [u_active_subgoals, []],
		    [u_analogical_subgoal, []],
		    
		    [ u_plan_no_gen,
		      [u_ob_c36_get, u_rule, [quote, u_plan_no_gen]]
		    ],
		    [u_gen_advice, []]
		  ],
		  [u_yfor, u_subgoal_obj, u_in, u_subgoal_objs],
		  
		  [ u_ydo,
		    
		    [ if,
		      u_analogical_subgoals,
		      [setq, u_analogical_subgoal, [car, u_analogical_subgoals]]
		    ],
		    
		    [ if,
		      u_plan_no_gen,
		      
		      [ progn,
			[setq, u_gen_advice, [car, u_plan_no_gen]],
			[setq, u_plan_no_gen, [cdr, u_plan_no_gen]]
		      ]
		    ],
		    
		    [ setq,
		      u_subgoal_obj,
		      [u_ob_c36_instantiate_o, u_subgoal_obj, u_bd]
		    ],
		    
		    [ if,
		      [u_ty_c36_instance_c63, u_subgoal_obj, [quote, u_rnot]],
		      [u_ob_c36_set, u_subgoal_obj, [quote, type], u_xx_not_ob_xx]
		    ],
		    
		    [ if,
		      u_believe_other_c63,
		      
		      [ setq,
			u_subgoal_obj,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_believe,
			      u_actor,
			      ['#COMMA', u_believe_other_c63],
			      u_obj,
			      ['#COMMA', u_subgoal_obj]
			    ]
			  ]
			]
		      ],
		      [u_ob_c36_set, u_subgoal_obj, [quote, u_plan_rule], u_rule]
		    ],
		    
		    [ u_ob_c36_set,
		      u_subgoal_obj,
		      [quote, u_plan_subgoalnum],
		      u_subgoalnum
		    ],
		    
		    [ setq,
		      u_active_subgoals,
		      
		      [ cons,
			
			[ u_activate_subgoal,
			  u_goal,
			  u_subgoal_obj,
			  u_sprouted_context,
			  u_rule,
			  u_analogical_subgoal,
			  u_seq_c63,
			  u_belief_path,
			  u_top_level_goal,
			  u_gen_advice
			],
			u_active_subgoals
		      ]
		    ],
		    
		    [ if,
		      u_analogical_subgoals,
		      [setq, u_analogical_subgoals, [cdr, u_analogical_subgoals]]
		    ],
		    [setq, u_subgoalnum, [+, 1, u_subgoalnum]]
		  ],
		  
		  [ u_yresult,
		    
		    [ progn,
		      [setq, u_active_subgoals, [reverse, u_active_subgoals]],
		      [if, u_seq_c63, [u_make_seq, u_active_subgoals]],
		      u_active_subgoals
		    ]
		  ]
		]
	      ]).


% annotating U::INSTAN-AND-ACTIVATE-SUBGOALS 
wl: arglist_info(u_instan_and_activate_subgoals,
		
		[ u_goal,
		  u_subgoal_objs,
		  u_bd,
		  u_rule,
		  u_sprouted_context,
		  u_seq_c63,
		  u_analogical_subgoals,
		  u_believe_other_c63,
		  u_top_level_goal,
		  u_belief_path
		],
		
		[ Goal_Param,
		  Subgoal_objs_Param,
		  Bd_Param,
		  Rule_Param,
		  Sprouted_context_Param,
		  Seq_c63_Param,
		  Analogical_subgoals_Param,
		  Believe_other_c63_Param,
		  Top_level_goal_Param,
		  Belief_path_Param
		],
		arginfo{ all:
			     [ u_goal,
			       u_subgoal_objs,
			       u_bd,
			       u_rule,
			       u_sprouted_context,
			       u_seq_c63,
			       u_analogical_subgoals,
			       u_believe_other_c63,
			       u_top_level_goal,
			       u_belief_path
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_subgoal_objs,
				 u_bd,
				 u_rule,
				 u_sprouted_context,
				 u_seq_c63,
				 u_analogical_subgoals,
				 u_believe_other_c63,
				 u_top_level_goal,
				 u_belief_path
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       u_subgoal_objs,
			       u_bd,
			       u_rule,
			       u_sprouted_context,
			       u_seq_c63,
			       u_analogical_subgoals,
			       u_believe_other_c63,
			       u_top_level_goal,
			       u_belief_path
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INSTAN-AND-ACTIVATE-SUBGOALS 
wl: init_args(exact_only, u_instan_and_activate_subgoals).


% annotating U::INSTAN-AND-ACTIVATE-SUBGOALS 
f_u_instan_and_activate_subgoals(Goal_Param, Subgoal_objs_Param, Bd_Param, Rule_Param, Sprouted_context_Param, Seq_c63_Param, Analogical_subgoals_Param, Believe_other_c63_Param, Top_level_goal_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_subgoal_objs, Subgoal_objs_Param), bv(u_bd, Bd_Param), bv(u_rule, Rule_Param), bv(u_sprouted_context, Sprouted_context_Param), bv(u_seq_c63, Seq_c63_Param), bv(u_analogical_subgoals, Analogical_subgoals_Param), bv(u_believe_other_c63, Believe_other_c63_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(s),
				       #\(t),
				       #\(a),
				       #\(n),
				       #\(t),
				       #\(i),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(' '),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(v),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(b),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(s)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_subgoalnum, 0],
		      [u_active_subgoals, []],
		      [u_analogical_subgoal, []],
		      
		      [ u_plan_no_gen,
			[u_ob_c36_get, u_rule, [quote, u_plan_no_gen]]
		      ],
		      [u_gen_advice, []]
		    ],
		    [u_yfor, u_subgoal_obj, u_in, u_subgoal_objs],
		    
		    [ u_ydo,
		      
		      [ if,
			u_analogical_subgoals,
			
			[ setq,
			  u_analogical_subgoal,
			  [car, u_analogical_subgoals]
			]
		      ],
		      
		      [ if,
			u_plan_no_gen,
			
			[ progn,
			  [setq, u_gen_advice, [car, u_plan_no_gen]],
			  [setq, u_plan_no_gen, [cdr, u_plan_no_gen]]
			]
		      ],
		      
		      [ setq,
			u_subgoal_obj,
			[u_ob_c36_instantiate_o, u_subgoal_obj, u_bd]
		      ],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_subgoal_obj, [quote, u_rnot]],
			
			[ u_ob_c36_set,
			  u_subgoal_obj,
			  [quote, type],
			  u_xx_not_ob_xx
			]
		      ],
		      
		      [ if,
			u_believe_other_c63,
			
			[ setq,
			  u_subgoal_obj,
			  
			  [ u_ob_c36_fcreate,
			    
			    [ '#BQ',
			      
			      [ u_believe,
				u_actor,
				['#COMMA', u_believe_other_c63],
				u_obj,
				['#COMMA', u_subgoal_obj]
			      ]
			    ]
			  ]
			],
			
			[ u_ob_c36_set,
			  u_subgoal_obj,
			  [quote, u_plan_rule],
			  u_rule
			]
		      ],
		      
		      [ u_ob_c36_set,
			u_subgoal_obj,
			[quote, u_plan_subgoalnum],
			u_subgoalnum
		      ],
		      
		      [ setq,
			u_active_subgoals,
			
			[ cons,
			  
			  [ u_activate_subgoal,
			    u_goal,
			    u_subgoal_obj,
			    u_sprouted_context,
			    u_rule,
			    u_analogical_subgoal,
			    u_seq_c63,
			    u_belief_path,
			    u_top_level_goal,
			    u_gen_advice
			  ],
			  u_active_subgoals
			]
		      ],
		      
		      [ if,
			u_analogical_subgoals,
			
			[ setq,
			  u_analogical_subgoals,
			  [cdr, u_analogical_subgoals]
			]
		      ],
		      [setq, u_subgoalnum, [+, 1, u_subgoalnum]]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			[setq, u_active_subgoals, [reverse, u_active_subgoals]],
			[if, u_seq_c63, [u_make_seq, u_active_subgoals]],
			u_active_subgoals
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_instan_and_activate_subgoals, classof, claz_function),
   set_opv(u_instan_and_activate_subgoals, compile_as, kw_function),
   set_opv(u_instan_and_activate_subgoals,
	   function,
	   f_u_instan_and_activate_subgoals),
   DefunResult=u_instan_and_activate_subgoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:1165 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: we should uniquify any unbound variables",
				     13,
				     1988)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:1165 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" accumulatively through instantiation.",
				     13,
				     2049)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:1165 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Above is reverse because subgoals come in forward and we cons, which",
				     1,
				     3284)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:1165 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" reverses, so we have to reverse again.",
				     1,
				     3355)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:3396 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'activate-subgoal',
			    
			    [ goal,
			      'subgoal-obj',
			      context,
			      rule,
			      'analogical-subgoal',
			      'seq?',
			      'belief-path',
			      'top-level-goal',
			      'gen-advice'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Activate subgoal for ~A obj ~A in ~A"),
			      goal,
			      'subgoal-obj',
			      context
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-xtra',
			      '$STRING'("Analogical subgoal = ~A"),
			      'analogical-subgoal'
			    ],
			    
			    [ let,
			      
			      [ 
				[ 'analogical-links?',
				  
				  [ if,
				    'analogical-subgoal',
				    
				    [ 'goal-intends-links-uo',
				      'analogical-subgoal',
				      
				      [ 'ob$get',
					'analogical-subgoal',
					[quote, 'top-context']
				      ],
				      '*me-belief-path*'
				    ],
				    []
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  'analogical-subgoal',
				  ['null?', 'analogical-links?']
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Analogical plan for ~A in ~A will bottom out"),
				  goal,
				  context
				]
			      ],
			      
			      [ 'let*',
				
				[ 
				  [ subgoal,
				    
				    [ if,
				      
				      [ and,
					'analogical-subgoal',
					'analogical-links?'
				      ],
				      
				      [ 'ob$fcreate',
					
					[ '#BQ',
					  
					  [ 'ACTIVE-GOAL',
					    obj,
					    ['#COMMA', 'subgoal-obj'],
					    'top-level-goal',
					    ['#COMMA', 'top-level-goal'],
					    'activation-context',
					    ['#COMMA', context],
					    'analogical-episode',
					    
					    [ '#COMMA',
					      
					      [ 'ob$get',
						'analogical-subgoal',
						[quote, episode]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ 'ob$fcreate',
					
					[ '#BQ',
					  
					  [ 'ACTIVE-GOAL',
					    'top-level-goal',
					    ['#COMMA', 'top-level-goal'],
					    'activation-context',
					    ['#COMMA', context],
					    obj,
					    ['#COMMA', 'subgoal-obj']
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ intends,
				    
				    [ if,
				      'seq?',
				      
				      [ 'ob$fcreate',
					
					[ '#BQ',
					  
					  [ 'INTENDS',
					    'linked-from',
					    ['#COMMA', goal],
					    'linked-to',
					    ['#COMMA', subgoal],
					    rule,
					    ['#COMMA', rule],
					    'seq?',
					    [quote, t]
					  ]
					]
				      ],
				      
				      [ 'ob$fcreate',
					
					[ '#BQ',
					  
					  [ 'INTENDS',
					    'linked-from',
					    ['#COMMA', goal],
					    'linked-to',
					    ['#COMMA', subgoal],
					    rule,
					    ['#COMMA', rule]
					  ]
					]
				      ]
				    ]
				  ]
				],
				
				[ if,
				  'gen-advice',
				  
				  [ 'ob$set',
				    subgoal,
				    [quote, 'gen-advice'],
				    'gen-advice'
				  ]
				],
				
				[ if,
				  ['preservation-goal-subgoal?', goal],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      'rule-xtra',
				      '$STRING'("Is preservation subgoal")
				    ],
				    
				    [ 'ob$set',
				      subgoal,
				      [quote, 'preservation-subgoal?'],
				      t
				    ]
				  ]
				],
				
				[ if,
				  
				  [ or,
				    
				    [ 'null?',
				      ['ga-gen-on-activate?', 'gen-advice']
				    ],
				    
				    [ 'cx$retrieve-relative',
				      context,
				      'subgoal-obj',
				      'belief-path'
				    ]
				  ],
				  
				  [ 'no-gen',
				    
				    [ 'cx$assert-relative',
				      context,
				      intends,
				      'belief-path'
				    ],
				    
				    [ 'cx$assert-relative',
				      context,
				      subgoal,
				      'belief-path'
				    ]
				  ],
				  
				  [ progn,
				    
				    [ 'cx$assert-relative',
				      context,
				      intends,
				      'belief-path'
				    ],
				    
				    [ 'cx$assert-relative',
				      context,
				      subgoal,
				      'belief-path'
				    ]
				  ]
				],
				subgoal
			      ]
			    ]
			  ]).

% annotating U::ACTIVATE-SUBGOAL 
wl: lambda_def(defun,
	      u_activate_subgoal,
	      f_u_activate_subgoal,
	      
	      [ u_goal,
		u_subgoal_obj,
		u_context,
		u_rule,
		u_analogical_subgoal,
		u_seq_c63,
		u_belief_path,
		u_top_level_goal,
		u_gen_advice
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(v),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(' '),
			     #\(s),
			     #\(u),
			     #\(b),
			     #\(g),
			     #\(o),
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
		  u_goal,
		  u_subgoal_obj,
		  u_context
		],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_xtra,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
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
			     #\(s),
			     #\(u),
			     #\(b),
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
		  u_analogical_subgoal
		],
		
		[ let,
		  
		  [ 
		    [ u_analogical_links_c63,
		      
		      [ if,
			u_analogical_subgoal,
			
			[ u_goal_intends_links_uo,
			  u_analogical_subgoal,
			  
			  [ u_ob_c36_get,
			    u_analogical_subgoal,
			    [quote, u_top_context]
			  ],
			  u_xx_me_belief_path_xx
			],
			[]
		      ]
		    ]
		  ],
		  
		  [ if,
		    
		    [ and,
		      u_analogical_subgoal,
		      [u_null_c63, u_analogical_links_c63]
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('A'),
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
				 #\(w),
				 #\(i),
				 #\(l),
				 #\(l),
				 #\(' '),
				 #\(b),
				 #\(o),
				 #\(t),
				 #\(t),
				 #\(o),
				 #\(m),
				 #\(' '),
				 #\(o),
				 #\(u),
				 #\(t)
			       ]),
		      u_goal,
		      u_context
		    ]
		  ],
		  
		  [ let_xx,
		    
		    [ 
		      [ u_subgoal,
			
			[ if,
			  [and, u_analogical_subgoal, u_analogical_links_c63],
			  
			  [ u_ob_c36_fcreate,
			    
			    [ '#BQ',
			      
			      [ u_active_goal,
				u_obj,
				['#COMMA', u_subgoal_obj],
				u_top_level_goal,
				['#COMMA', u_top_level_goal],
				u_activation_context,
				['#COMMA', u_context],
				u_analogical_episode,
				
				[ '#COMMA',
				  
				  [ u_ob_c36_get,
				    u_analogical_subgoal,
				    [quote, u_episode]
				  ]
				]
			      ]
			    ]
			  ],
			  
			  [ u_ob_c36_fcreate,
			    
			    [ '#BQ',
			      
			      [ u_active_goal,
				u_top_level_goal,
				['#COMMA', u_top_level_goal],
				u_activation_context,
				['#COMMA', u_context],
				u_obj,
				['#COMMA', u_subgoal_obj]
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ u_intends,
			
			[ if,
			  u_seq_c63,
			  
			  [ u_ob_c36_fcreate,
			    
			    [ '#BQ',
			      
			      [ u_intends,
				u_linked_from,
				['#COMMA', u_goal],
				u_linked_to,
				['#COMMA', u_subgoal],
				u_rule,
				['#COMMA', u_rule],
				u_seq_c63,
				[quote, t]
			      ]
			    ]
			  ],
			  
			  [ u_ob_c36_fcreate,
			    
			    [ '#BQ',
			      
			      [ u_intends,
				u_linked_from,
				['#COMMA', u_goal],
				u_linked_to,
				['#COMMA', u_subgoal],
				u_rule,
				['#COMMA', u_rule]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ if,
		      u_gen_advice,
		      
		      [ u_ob_c36_set,
			u_subgoal,
			[quote, u_gen_advice],
			u_gen_advice
		      ]
		    ],
		    
		    [ if,
		      [u_preservation_goal_subgoal_c63, u_goal],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('I'),
				     #\(s),
				     #\(' '),
				     #\(p),
				     #\(r),
				     #\(e),
				     #\(s),
				     #\(e),
				     #\(r),
				     #\(v),
				     #\(a),
				     #\(t),
				     #\(i),
				     #\(o),
				     #\(n),
				     #\(' '),
				     #\(s),
				     #\(u),
				     #\(b),
				     #\(g),
				     #\(o),
				     #\(a),
				     #\(l)
				   ])
			],
			
			[ u_ob_c36_set,
			  u_subgoal,
			  [quote, u_preservation_subgoal_c63],
			  t
			]
		      ]
		    ],
		    
		    [ if,
		      
		      [ or,
			[u_null_c63, [u_ga_gen_on_activate_c63, u_gen_advice]],
			
			[ u_cx_c36_retrieve_relative,
			  u_context,
			  u_subgoal_obj,
			  u_belief_path
			]
		      ],
		      
		      [ u_no_gen,
			
			[ u_cx_c36_assert_relative,
			  u_context,
			  u_intends,
			  u_belief_path
			],
			
			[ u_cx_c36_assert_relative,
			  u_context,
			  u_subgoal,
			  u_belief_path
			]
		      ],
		      
		      [ progn,
			
			[ u_cx_c36_assert_relative,
			  u_context,
			  u_intends,
			  u_belief_path
			],
			
			[ u_cx_c36_assert_relative,
			  u_context,
			  u_subgoal,
			  u_belief_path
			]
		      ]
		    ],
		    u_subgoal
		  ]
		]
	      ]).


% annotating U::ACTIVATE-SUBGOAL 
wl: arglist_info(u_activate_subgoal,
		
		[ u_goal,
		  u_subgoal_obj,
		  u_context,
		  u_rule,
		  u_analogical_subgoal,
		  u_seq_c63,
		  u_belief_path,
		  u_top_level_goal,
		  u_gen_advice
		],
		
		[ Goal_Param,
		  Subgoal_obj_Param,
		  Context_Param,
		  Rule_Param,
		  Analogical_subgoal_Param,
		  Seq_c63_Param,
		  Belief_path_Param,
		  Top_level_goal_Param,
		  Gen_advice_Param
		],
		arginfo{ all:
			     [ u_goal,
			       u_subgoal_obj,
			       u_context,
			       u_rule,
			       u_analogical_subgoal,
			       u_seq_c63,
			       u_belief_path,
			       u_top_level_goal,
			       u_gen_advice
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_subgoal_obj,
				 u_context,
				 u_rule,
				 u_analogical_subgoal,
				 u_seq_c63,
				 u_belief_path,
				 u_top_level_goal,
				 u_gen_advice
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       u_subgoal_obj,
			       u_context,
			       u_rule,
			       u_analogical_subgoal,
			       u_seq_c63,
			       u_belief_path,
			       u_top_level_goal,
			       u_gen_advice
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ACTIVATE-SUBGOAL 
wl: init_args(exact_only, u_activate_subgoal).


% annotating U::ACTIVATE-SUBGOAL 
f_u_activate_subgoal(Goal_Param, Subgoal_obj_Param, Context_Param, Rule_Param, Analogical_subgoal_Param, Seq_c63_Param, Belief_path_Param, Top_level_goal_Param, Gen_advice_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_subgoal_obj, Subgoal_obj_Param), bv(u_context, Context_Param), bv(u_rule, Rule_Param), bv(u_analogical_subgoal, Analogical_subgoal_Param), bv(u_seq_c63, Seq_c63_Param), bv(u_belief_path, Belief_path_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_gen_advice, Gen_advice_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(v),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(b),
				       #\(g),
				       #\(o),
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
			    u_goal,
			    u_subgoal_obj,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
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
				       #\(s),
				       #\(u),
				       #\(b),
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
			    u_analogical_subgoal
			  ],
			  Roman_nl_Ret100),
	(   Analogical_subgoal_Param\==[]
	->  f_u_ob_c36_get(Analogical_subgoal_Param, u_top_context, Top_context),
	    get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    f_u_goal_intends_links_uo(Analogical_subgoal_Param,
				      Top_context,
				      Xx_me_belief_path_xx_Get,
				      TrueResult),
	    Analogical_links_c63_Init=TrueResult
	;   Analogical_links_c63_Init=[]
	),
	LEnv=[[bv(u_analogical_links_c63, Analogical_links_c63_Init)]|Env],
	(   Analogical_subgoal_Param\==[]
	->  f_u_null_c63(u_analogical_links_c63, TrueResult44),
	    IFTEST38=TrueResult44
	;   IFTEST38=[]
	),
	(   IFTEST38\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('A'),
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
					   #\(w),
					   #\(i),
					   #\(l),
					   #\(l),
					   #\(' '),
					   #\(b),
					   #\(o),
					   #\(t),
					   #\(t),
					   #\(o),
					   #\(m),
					   #\(' '),
					   #\(o),
					   #\(u),
					   #\(t)
					 ]),
				u_goal,
				u_context
			      ],
			      TrueResult45),
	    _120858=TrueResult45
	;   _120858=[]
	),
	(   Analogical_subgoal_Param\==[]
	->  get_var(LEnv, u_analogical_links_c63, Analogical_links_c63_Get),
	    IFTEST48=Analogical_links_c63_Get
	;   IFTEST48=[]
	),
	(   IFTEST48\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_active_goal,
				   u_obj,
				   ['#COMMA', u_subgoal_obj],
				   u_top_level_goal,
				   ['#COMMA', u_top_level_goal],
				   u_activation_context,
				   ['#COMMA', u_context],
				   u_analogical_episode,
				   
				   [ '#COMMA',
				     
				     [ u_ob_c36_get,
				       u_analogical_subgoal,
				       [quote, u_episode]
				     ]
				   ]
				 ]
			       ],
			       TrueResult55),
	    Subgoal_Init=TrueResult55
	;   f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_active_goal,
				   u_top_level_goal,
				   ['#COMMA', u_top_level_goal],
				   u_activation_context,
				   ['#COMMA', u_context],
				   u_obj,
				   ['#COMMA', u_subgoal_obj]
				 ]
			       ],
			       ElseResult),
	    Subgoal_Init=ElseResult
	),
	Env=[[bv(u_subgoal, Subgoal_Init)]|LEnv],
	(   Seq_c63_Param\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_intends,
				   u_linked_from,
				   ['#COMMA', u_goal],
				   u_linked_to,
				   ['#COMMA', u_subgoal],
				   u_rule,
				   ['#COMMA', u_rule],
				   u_seq_c63,
				   [quote, t]
				 ]
			       ],
			       TrueResult64),
	    Intends_Init=TrueResult64
	;   f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_intends,
				   u_linked_from,
				   ['#COMMA', u_goal],
				   u_linked_to,
				   ['#COMMA', u_subgoal],
				   u_rule,
				   ['#COMMA', u_rule]
				 ]
			       ],
			       ElseResult65),
	    Intends_Init=ElseResult65
	),
	Env=[[bv(u_intends, Intends_Init)]|Env],
	(   Gen_advice_Param\==[]
	->  get_var(Env, u_subgoal, Subgoal_Get),
	    f_u_ob_c36_set(Subgoal_Get,
			   u_gen_advice,
			   Gen_advice_Param,
			   TrueResult73),
	    _121644=TrueResult73
	;   _121644=[]
	),
	f_u_preservation_goal_subgoal_c63(Goal_Param, IFTEST74),
	(   IFTEST74\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_xtra,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(s),
					   #\(' '),
					   #\(p),
					   #\(r),
					   #\(e),
					   #\(s),
					   #\(e),
					   #\(r),
					   #\(v),
					   #\(a),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(' '),
					   #\(s),
					   #\(u),
					   #\(b),
					   #\(g),
					   #\(o),
					   #\(a),
					   #\(l)
					 ])
			      ],
			      Roman_nl_Ret101),
	    get_var(Env, u_subgoal, Subgoal_Get77),
	    f_u_ob_c36_set(Subgoal_Get77,
			   u_preservation_subgoal_c63,
			   t,
			   TrueResult78),
	    _121888=TrueResult78
	;   _121888=[]
	),
	(   f_u_null_c63([u_ga_gen_on_activate_c63, u_gen_advice], FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST79=FORM1_Res
	->  true
	;   f_u_cx_c36_retrieve_relative(Context_Param,
					 Subgoal_obj_Param,
					 Belief_path_Param,
					 Retrieve_relative_Ret),
	    IFTEST79=Retrieve_relative_Ret
	),
	(   IFTEST79\==[]
	->  f_u_no_gen(
		       [ 
			 [ u_cx_c36_assert_relative,
			   u_context,
			   u_intends,
			   u_belief_path
			 ],
			 
			 [ u_cx_c36_assert_relative,
			   u_context,
			   u_subgoal,
			   u_belief_path
			 ]
		       ],
		       TrueResult91),
	    _122020=TrueResult91
	;   get_var(Env, u_intends, Intends_Get),
	    f_u_cx_c36_assert_relative(Context_Param,
				       Intends_Get,
				       Belief_path_Param,
				       Assert_relative_Ret),
	    get_var(Env, u_subgoal, Subgoal_Get89),
	    f_u_cx_c36_assert_relative(Context_Param,
				       Subgoal_Get89,
				       Belief_path_Param,
				       ElseResult92),
	    _122020=ElseResult92
	),
	get_var(Env, u_subgoal, Subgoal_Get93),
	LetResult=Subgoal_Get93,
	LetResult=FnResult.
:- set_opv(f_u_activate_subgoal, classof, claz_function),
   set_opv(u_activate_subgoal, compile_as, kw_function),
   set_opv(u_activate_subgoal, function, f_u_activate_subgoal),
   DefunResult=u_activate_subgoal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:3396 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below is redundant", 8, 4071)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:3396 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (if (ty$instance? subgoal-obj 'rnot)",
				     1,
				     4096)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:3396 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (ob$set subgoal-obj 'type *not-ob*))",
				     1,
				     4141)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:6390 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'planner-empty-bd',
			    ['belief-path'],
			    
			    [ 'bd-bind',
			      [quote, self],
			      [car, 'belief-path'],
			      '*empty-bd*'
			    ]
			  ]).

% annotating U::PLANNER-EMPTY-BD 
wl: lambda_def(defun,
	      u_planner_empty_bd,
	      f_u_planner_empty_bd,
	      [u_belief_path],
	      
	      [ 
		[ u_bd_bind,
		  [quote, u_self],
		  [car, u_belief_path],
		  u_xx_empty_bd_xx
		]
	      ]).


% annotating U::PLANNER-EMPTY-BD 
wl: arglist_info(u_planner_empty_bd,
		[u_belief_path],
		[Belief_path_Param],
		arginfo{ all:[u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_belief_path],
			 opt:0,
			 req:[u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PLANNER-EMPTY-BD 
wl: init_args(exact_only, u_planner_empty_bd).


% annotating U::PLANNER-EMPTY-BD 
f_u_planner_empty_bd(Belief_path_Param, FnResult) :-
	Env=[bv(u_belief_path, Belief_path_Param)],
	f_u_bd_bind([quote, u_self],
		    [car, u_belief_path],
		    u_xx_empty_bd_xx,
		    Xx_empty_bd_xx),
	Xx_empty_bd_xx=FnResult.
:- set_opv(f_u_planner_empty_bd, classof, claz_function),
   set_opv(u_planner_empty_bd, compile_as, kw_function),
   set_opv(u_planner_empty_bd, function, f_u_planner_empty_bd),
   DefunResult=u_planner_empty_bd.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:6390 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Now incorporates bindings from retrieved deletes.",
				     1,
				     6478)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:6390 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Is currently only called for believe others.",
				     1,
				     6530)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:6576 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'try-generic-plan',
			    
			    [ goal,
			      'goal-obj',
			      context,
			      rule,
			      'belief-path',
			      'believe-other?',
			      'top-level-goal'
			    ],
			    
			    [ yloop,
			      [initial, ['sprouted-contexts', []]],
			      
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
				  'sprouted-contexts',
				  
				  [ 'append!',
				    
				    [ 'run-generic-plan',
				      goal,
				      'goal-obj',
				      context,
				      rule,
				      'belief-path',
				      'believe-other?',
				      bd,
				      'top-level-goal'
				    ],
				    'sprouted-contexts'
				  ]
				]
			      ],
			      [yresult, 'sprouted-contexts']
			    ]
			  ]).

% annotating U::TRY-GENERIC-PLAN 
wl: lambda_def(defun,
	      u_try_generic_plan,
	      f_u_try_generic_plan,
	      
	      [ u_goal,
		u_goal_obj,
		u_context,
		u_rule,
		u_belief_path,
		u_believe_other_c63,
		u_top_level_goal
	      ],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_sprouted_contexts, []]],
		  
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
		      u_sprouted_contexts,
		      
		      [ u_append_c33,
			
			[ u_run_generic_plan,
			  u_goal,
			  u_goal_obj,
			  u_context,
			  u_rule,
			  u_belief_path,
			  u_believe_other_c63,
			  u_bd,
			  u_top_level_goal
			],
			u_sprouted_contexts
		      ]
		    ]
		  ],
		  [u_yresult, u_sprouted_contexts]
		]
	      ]).


% annotating U::TRY-GENERIC-PLAN 
wl: arglist_info(u_try_generic_plan,
		
		[ u_goal,
		  u_goal_obj,
		  u_context,
		  u_rule,
		  u_belief_path,
		  u_believe_other_c63,
		  u_top_level_goal
		],
		
		[ Goal_Param,
		  Goal_obj_Param,
		  Context_Param,
		  Rule_Param,
		  Belief_path_Param,
		  Believe_other_c63_Param,
		  Top_level_goal_Param
		],
		arginfo{ all:
			     [ u_goal,
			       u_goal_obj,
			       u_context,
			       u_rule,
			       u_belief_path,
			       u_believe_other_c63,
			       u_top_level_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_goal_obj,
				 u_context,
				 u_rule,
				 u_belief_path,
				 u_believe_other_c63,
				 u_top_level_goal
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       u_goal_obj,
			       u_context,
			       u_rule,
			       u_belief_path,
			       u_believe_other_c63,
			       u_top_level_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TRY-GENERIC-PLAN 
wl: init_args(exact_only, u_try_generic_plan).


% annotating U::TRY-GENERIC-PLAN 
f_u_try_generic_plan(Goal_Param, Goal_obj_Param, Context_Param, Rule_Param, Belief_path_Param, Believe_other_c63_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_goal_obj, Goal_obj_Param), bv(u_context, Context_Param), bv(u_rule, Rule_Param), bv(u_belief_path, Belief_path_Param), bv(u_believe_other_c63, Believe_other_c63_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_yloop(
		  [ [u_initial, [u_sprouted_contexts, []]],
		    
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
			u_sprouted_contexts,
			
			[ u_append_c33,
			  
			  [ u_run_generic_plan,
			    u_goal,
			    u_goal_obj,
			    u_context,
			    u_rule,
			    u_belief_path,
			    u_believe_other_c63,
			    u_bd,
			    u_top_level_goal
			  ],
			  u_sprouted_contexts
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_try_generic_plan, classof, claz_function),
   set_opv(u_try_generic_plan, compile_as, kw_function),
   set_opv(u_try_generic_plan, function, f_u_try_generic_plan),
   DefunResult=u_try_generic_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:6576 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ndbg-roman-nl *gate-dbg* rule \"Try generic plan for ~A obj ~A in ~A\"",
				     1,
				     6698)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:6576 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 goal goal-obj context)",
				     1,
				     6771)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-applications',
			    
			    [ 'goal-obj',
			      context,
			      rule,
			      'belief-path',
			      'believe-other?'
			    ],
			    
			    [ 'let*',
			      
			      [ ['rule-goal-obj', ['ob$get', rule, [quote, goal]]],
				
				[ 'rule-initial',
				  ['ob$get', rule, [quote, initial]]
				],
				
				[ 'rule-tlg',
				  ['ob$get', rule, [quote, 'top-level-goal']]
				],
				
				[ bd,
				  
				  [ if,
				    'believe-other?',
				    
				    [ 'bd-bind',
				      [quote, self],
				      'believe-other?',
				      '*empty-bd*'
				    ],
				    ['planner-empty-bd', 'belief-path']
				  ]
				],
				[temp, []]
			      ],
			      
			      [ if,
				
				[ 'self-type-ok?',
				  rule,
				  ['bd-lookup', [quote, self], bd]
				],
				
				[ progn,
				  
				  [ setq,
				    bd,
				    
				    [ 'ob$unify-cx',
				      'rule-goal-obj',
				      'goal-obj',
				      bd,
				      context
				    ]
				  ],
				  
				  [ if,
				    
				    [ and,
				      bd,
				      'rule-tlg',
				      
				      [ 'null?',
					
					[ 'ob$unify',
					  'rule-tlg',
					  
					  [ 'ob$get',
					    '*top-level-goal*',
					    [quote, obj]
					  ],
					  bd
					]
				      ]
				    ],
				    [setq, bd, []]
				  ],
				  
				  [ if,
				    bd,
				    
				    [ if,
				      'rule-initial',
				      
				      [ progn,
					
					[ setq,
					  temp,
					  
					  [ show,
					    'rule-initial',
					    context,
					    bd,
					    'belief-path'
					  ]
					],
					[if, temp, [list, [car, [car, temp]]], []]
				      ],
				      [list, bd]
				    ],
				    []
				  ]
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::RULE-APPLICATIONS 
wl: lambda_def(defun,
	      u_rule_applications,
	      f_u_rule_applications,
	      [u_goal_obj, u_context, u_rule, u_belief_path, u_believe_other_c63],
	      
	      [ 
		[ let_xx,
		  
		  [ [u_rule_goal_obj, [u_ob_c36_get, u_rule, [quote, u_goal]]],
		    [u_rule_initial, [u_ob_c36_get, u_rule, [quote, u_initial]]],
		    
		    [ u_rule_tlg,
		      [u_ob_c36_get, u_rule, [quote, u_top_level_goal]]
		    ],
		    
		    [ u_bd,
		      
		      [ if,
			u_believe_other_c63,
			
			[ u_bd_bind,
			  [quote, u_self],
			  u_believe_other_c63,
			  u_xx_empty_bd_xx
			],
			[u_planner_empty_bd, u_belief_path]
		      ]
		    ],
		    [u_temp, []]
		  ],
		  
		  [ if,
		    
		    [ u_self_type_ok_c63,
		      u_rule,
		      [u_bd_lookup, [quote, u_self], u_bd]
		    ],
		    
		    [ progn,
		      
		      [ setq,
			u_bd,
			
			[ u_ob_c36_unify_cx,
			  u_rule_goal_obj,
			  u_goal_obj,
			  u_bd,
			  u_context
			]
		      ],
		      
		      [ if,
			
			[ and,
			  u_bd,
			  u_rule_tlg,
			  
			  [ u_null_c63,
			    
			    [ u_ob_c36_unify,
			      u_rule_tlg,
			      
			      [ u_ob_c36_get,
				u_xx_top_level_goal_xx,
				[quote, u_obj]
			      ],
			      u_bd
			    ]
			  ]
			],
			[setq, u_bd, []]
		      ],
		      
		      [ if,
			u_bd,
			
			[ if,
			  u_rule_initial,
			  
			  [ progn,
			    
			    [ setq,
			      u_temp,
			      
			      [ u_show,
				u_rule_initial,
				u_context,
				u_bd,
				u_belief_path
			      ]
			    ],
			    [if, u_temp, [list, [car, [car, u_temp]]], []]
			  ],
			  [list, u_bd]
			],
			[]
		      ]
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::RULE-APPLICATIONS 
wl: arglist_info(u_rule_applications,
		
		[ u_goal_obj,
		  u_context,
		  u_rule,
		  u_belief_path,
		  u_believe_other_c63
		],
		
		[ Goal_obj_Param,
		  Context_Param,
		  Rule_Param,
		  Belief_path_Param,
		  Believe_other_c63_Param
		],
		arginfo{ all:
			     [ u_goal_obj,
			       u_context,
			       u_rule,
			       u_belief_path,
			       u_believe_other_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal_obj,
				 u_context,
				 u_rule,
				 u_belief_path,
				 u_believe_other_c63
			       ],
			 opt:0,
			 req:
			     [ u_goal_obj,
			       u_context,
			       u_rule,
			       u_belief_path,
			       u_believe_other_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-APPLICATIONS 
wl: init_args(exact_only, u_rule_applications).


% annotating U::RULE-APPLICATIONS 
f_u_rule_applications(Goal_obj_Param, Context_Param, Rule_Param, Belief_path_Param, Believe_other_c63_Param, TrueResult75) :-
	Env=[bv(u_goal_obj, Goal_obj_Param), bv(u_context, Context_Param), bv(u_rule, Rule_Param), bv(u_belief_path, Belief_path_Param), bv(u_believe_other_c63, Believe_other_c63_Param)],
	f_u_ob_c36_get(Rule_Param, u_goal, Rule_goal_obj_Init),
	LEnv=[[bv(u_rule_goal_obj, Rule_goal_obj_Init)]|Env],
	f_u_ob_c36_get(Rule_Param, u_initial, Rule_initial_Init),
	Env=[[bv(u_rule_initial, Rule_initial_Init)]|LEnv],
	f_u_ob_c36_get(Rule_Param, u_top_level_goal, Rule_tlg_Init),
	Env=[[bv(u_rule_tlg, Rule_tlg_Init)]|Env],
	(   Believe_other_c63_Param\==[]
	->  f_u_bd_bind([quote, u_self],
			u_believe_other_c63,
			u_xx_empty_bd_xx,
			TrueResult),
	    Bd_Init=TrueResult
	;   f_u_planner_empty_bd(Belief_path_Param, ElseResult),
	    Bd_Init=ElseResult
	),
	LEnv34=[[bv(u_bd, Bd_Init)]|Env],
	Env=[[bv(u_temp, [])]|LEnv34],
	f_u_self_type_ok_c63(u_rule,
			     [u_bd_lookup, [quote, u_self], u_bd],
			     IFTEST46),
	(   IFTEST46\==[]
	->  f_u_ob_c36_unify_cx(u_rule_goal_obj,
				u_goal_obj,
				u_bd,
				u_context,
				Context),
	    set_var(Env, u_bd, Context),
	    get_var(Env, u_bd, IFTEST51),
	    (   IFTEST51\==[]
	    ->  get_var(Env, u_rule_tlg, IFTEST54),
		(   IFTEST54\==[]
		->  f_u_null_c63(
				 [ u_ob_c36_unify,
				   u_rule_tlg,
				   
				   [ u_ob_c36_get,
				     u_xx_top_level_goal_xx,
				     [quote, u_obj]
				   ],
				   u_bd
				 ],
				 TrueResult57),
		    IFTEST49=TrueResult57
		;   IFTEST49=[]
		)
	    ;   IFTEST49=[]
	    ),
	    (   IFTEST49\==[]
	    ->  set_var(Env, setq, u_bd, []),
		_117356=[]
	    ;   _117356=[]
	    ),
	    get_var(Env, u_bd, IFTEST59),
	    (   IFTEST59\==[]
	    ->  get_var(Env, u_rule_initial, IFTEST62),
		(   IFTEST62\==[]
		->  get_var(Env, u_bd, Bd_Get67),
		    get_var(Env, u_rule_initial, Rule_initial_Get65),
		    f_u_show(Rule_initial_Get65,
			     Context_Param,
			     Bd_Get67,
			     Belief_path_Param,
			     Temp),
		    set_var(Env, u_temp, Temp),
		    get_var(Env, u_temp, IFTEST69),
		    (   IFTEST69\==[]
		    ->  cl_car(IFTEST69, Car_Param),
			cl_car(Car_Param, Car_Ret),
			TrueResult75=[Car_Ret]
		    ;   TrueResult75=[]
		    )
		;   get_var(Env, u_bd, Bd_Get74),
		    TrueResult75=[Bd_Get74]
		)
	    ;   TrueResult75=[]
	    )
	;   TrueResult75=[]
	).
:- set_opv(f_u_rule_applications, classof, claz_function),
   set_opv(u_rule_applications, compile_as, kw_function),
   set_opv(u_rule_applications, function, f_u_rule_applications),
   DefunResult=u_rule_applications.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ndbg-roman-nl *gate-dbg* rule \"Find rule applications for ~A obj ~A in ~A\"",
				     1,
				     7351)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        rule goal-obj context)", 1, 7430)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was (cx$retrieve-bd context rule-initial bd)",
				     23,
				     8342)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Initial must be satisfied; was (list bd) for optional satisfaction",
				     1,
				     8450)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" put ndbgs in", 22, 8540)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" add stripping to goal-obj in case of believe-other?",
				     1,
				     8630)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8684)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ALL subgoals are activated, whether true or not, so that they get into the",
				     1,
				     8686)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" planning rule (used by analogical planning). Unbound variables are later",
				     1,
				     8763)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:7274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" concretized by fact planning or analogical planning.",
				     1,
				     8838)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-generic-plan',
			    
			    [ goal,
			      'goal-obj',
			      context,
			      rule,
			      'belief-path',
			      'believe-other?',
			      bd,
			      'top-level-goal'
			    ],
			    ['ndbg-add-item', rule],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Run generic plan ~A for ~A in ~A"),
			      rule,
			      goal,
			      context
			    ],
			    
			    [ let,
			      
			      [ 
				[ 'rule-subgoal-obj',
				  ['ob$get', rule, [quote, subgoal]]
				],
				['new-goal', []],
				['sprouted-context', []],
				['sprouted-contexts', []]
			      ],
			      
			      [ cond,
				
				[ 
				  [ 'ty$instance?',
				    'rule-subgoal-obj',
				    [quote, 'RCODE']
				  ],
				  
				  [ let,
				    [['old-ob-bindings', '*ob-bindings*']],
				    [setq, '*ob-bindings*', bd],
				    
				    [ setq,
				      'sprouted-contexts',
				      
				      [ eval,
					
					[ 'ob$get',
					  'rule-subgoal-obj',
					  [quote, obj]
					]
				      ]
				    ],
				    [setq, '*ob-bindings*', 'old-ob-bindings']
				  ]
				],
				
				[ 
				  [ 'ty$instance?',
				    'rule-subgoal-obj',
				    [quote, 'ROR']
				  ],
				  
				  [ error,
				    '$STRING'("Planning rule ~A has ROR as subgoal--not allowed"),
				    rule
				  ]
				],
				
				[ else,
				  
				  [ setq,
				    'sprouted-context',
				    ['cx$sprout', context]
				  ],
				  
				  [ 'delay-dbgs',
				    'sprouted-context',
				    
				    [ cond,
				      
				      [ 'believe-other?',
					
					[ 'rule-fire-msg',
					  rule,
					  '$STRING'("backward vicarious plan"),
					  context,
					  bd,
					  'sprouted-context',
					  goal
					]
				      ],
				      
				      [ ['me-belief-path?', 'belief-path'],
					
					[ 'rule-fire-msg',
					  rule,
					  '$STRING'("plan"),
					  context,
					  bd,
					  'sprouted-context',
					  goal
					]
				      ],
				      
				      [ else,
					
					[ 'rule-fire-msg',
					  rule,
					  '$STRING'("forward vicarious plan"),
					  context,
					  bd,
					  'sprouted-context',
					  goal
					]
				      ]
				    ],
				    
				    [ 'set-ordering',
				      'sprouted-context',
				      ['ob$get', rule, [quote, plausibility]]
				    ],
				    
				    [ if,
				      ['vars-in?', 'goal-obj'],
				      
				      [ setq,
					'new-goal',
					
					[ or,
					  
					  [ 'plan-instantiate',
					    goal,
					    ['bd-no-var-bds', bd],
					    'sprouted-context',
					    'top-level-goal',
					    'belief-path',
					    []
					  ],
					  goal
					]
				      ],
				      [setq, 'new-goal', goal]
				    ],
				    
				    [ 'instan-and-activate-subgoals',
				      'new-goal',
				      ['rule-subgoal-objs', rule],
				      bd,
				      rule,
				      'sprouted-context',
				      
				      [ 'ty$instance?',
					'rule-subgoal-obj',
					[quote, rseq]
				      ],
				      [],
				      'believe-other?',
				      'top-level-goal',
				      'belief-path'
				    ]
				  ],
				  
				  [ setq,
				    'sprouted-contexts',
				    [list, 'sprouted-context']
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  ['ob$get', rule, [quote, 'halt?']],
				  [not, ['dd-goal?', 'top-level-goal']]
				],
				
				[ 'ob$set',
				  'top-level-goal',
				  [quote, status],
				  [quote, 'fired-halt']
				]
			      ],
			      ['ndbg-remove-item', rule],
			      'sprouted-contexts'
			    ]
			  ]).

% annotating U::RUN-GENERIC-PLAN 
wl: lambda_def(defun,
	      u_run_generic_plan,
	      f_u_run_generic_plan,
	      
	      [ u_goal,
		u_goal_obj,
		u_context,
		u_rule,
		u_belief_path,
		u_believe_other_c63,
		u_bd,
		u_top_level_goal
	      ],
	      
	      [ [u_ndbg_add_item, u_rule],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(n),
			     #\(' '),
			     #\(g),
			     #\(e),
			     #\(n),
			     #\(e),
			     #\(r),
			     #\(i),
			     #\(c),
			     #\(' '),
			     #\(p),
			     #\(l),
			     #\(a),
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
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_rule,
		  u_goal,
		  u_context
		],
		
		[ let,
		  
		  [ 
		    [ u_rule_subgoal_obj,
		      [u_ob_c36_get, u_rule, [quote, u_subgoal]]
		    ],
		    [u_new_goal, []],
		    [u_sprouted_context, []],
		    [u_sprouted_contexts, []]
		  ],
		  
		  [ cond,
		    
		    [ 
		      [ u_ty_c36_instance_c63,
			u_rule_subgoal_obj,
			[quote, u_rcode]
		      ],
		      
		      [ let,
			[[u_old_ob_bindings, u_xx_ob_bindings_xx]],
			[setq, u_xx_ob_bindings_xx, u_bd],
			
			[ setq,
			  u_sprouted_contexts,
			  
			  [ eval,
			    [u_ob_c36_get, u_rule_subgoal_obj, [quote, u_obj]]
			  ]
			],
			[setq, u_xx_ob_bindings_xx, u_old_ob_bindings]
		      ]
		    ],
		    
		    [ [u_ty_c36_instance_c63, u_rule_subgoal_obj, [quote, u_ror]],
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('P'),
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
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(h),
				   #\(a),
				   #\(s),
				   #\(' '),
				   #\('R'),
				   #\('O'),
				   #\('R'),
				   #\(' '),
				   #\(a),
				   #\(s),
				   #\(' '),
				   #\(s),
				   #\(u),
				   #\(b),
				   #\(g),
				   #\(o),
				   #\(a),
				   #\(l),
				   #\(-),
				   #\(-),
				   #\(n),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(a),
				   #\(l),
				   #\(l),
				   #\(o),
				   #\(w),
				   #\(e),
				   #\(d)
				 ]),
			u_rule
		      ]
		    ],
		    
		    [ u_else,
		      [setq, u_sprouted_context, [u_cx_c36_sprout, u_context]],
		      
		      [ u_delay_dbgs,
			u_sprouted_context,
			
			[ cond,
			  
			  [ u_believe_other_c63,
			    
			    [ u_rule_fire_msg,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(b),
					 #\(a),
					 #\(c),
					 #\(k),
					 #\(w),
					 #\(a),
					 #\(r),
					 #\(d),
					 #\(' '),
					 #\(v),
					 #\(i),
					 #\(c),
					 #\(a),
					 #\(r),
					 #\(i),
					 #\(o),
					 #\(u),
					 #\(s),
					 #\(' '),
					 #\(p),
					 #\(l),
					 #\(a),
					 #\(n)
				       ]),
			      u_context,
			      u_bd,
			      u_sprouted_context,
			      u_goal
			    ]
			  ],
			  
			  [ [u_me_belief_path_c63, u_belief_path],
			    
			    [ u_rule_fire_msg,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(p), #\(l), #\(a), #\(n)]),
			      u_context,
			      u_bd,
			      u_sprouted_context,
			      u_goal
			    ]
			  ],
			  
			  [ u_else,
			    
			    [ u_rule_fire_msg,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(f),
					 #\(o),
					 #\(r),
					 #\(w),
					 #\(a),
					 #\(r),
					 #\(d),
					 #\(' '),
					 #\(v),
					 #\(i),
					 #\(c),
					 #\(a),
					 #\(r),
					 #\(i),
					 #\(o),
					 #\(u),
					 #\(s),
					 #\(' '),
					 #\(p),
					 #\(l),
					 #\(a),
					 #\(n)
				       ]),
			      u_context,
			      u_bd,
			      u_sprouted_context,
			      u_goal
			    ]
			  ]
			],
			
			[ u_set_ordering,
			  u_sprouted_context,
			  [u_ob_c36_get, u_rule, [quote, u_plausibility]]
			],
			
			[ if,
			  [u_vars_in_c63, u_goal_obj],
			  
			  [ setq,
			    u_new_goal,
			    
			    [ or,
			      
			      [ u_plan_instantiate,
				u_goal,
				[u_bd_no_var_bds, u_bd],
				u_sprouted_context,
				u_top_level_goal,
				u_belief_path,
				[]
			      ],
			      u_goal
			    ]
			  ],
			  [setq, u_new_goal, u_goal]
			],
			
			[ u_instan_and_activate_subgoals,
			  u_new_goal,
			  [u_rule_subgoal_objs, u_rule],
			  u_bd,
			  u_rule,
			  u_sprouted_context,
			  
			  [ u_ty_c36_instance_c63,
			    u_rule_subgoal_obj,
			    [quote, u_rseq]
			  ],
			  [],
			  u_believe_other_c63,
			  u_top_level_goal,
			  u_belief_path
			]
		      ],
		      [setq, u_sprouted_contexts, [list, u_sprouted_context]]
		    ]
		  ],
		  
		  [ if,
		    
		    [ and,
		      [u_ob_c36_get, u_rule, [quote, u_halt_c63]],
		      [not, [u_dd_goal_c63, u_top_level_goal]]
		    ],
		    
		    [ u_ob_c36_set,
		      u_top_level_goal,
		      [quote, u_status],
		      [quote, u_fired_halt]
		    ]
		  ],
		  [u_ndbg_remove_item, u_rule],
		  u_sprouted_contexts
		]
	      ]).


% annotating U::RUN-GENERIC-PLAN 
wl: arglist_info(u_run_generic_plan,
		
		[ u_goal,
		  u_goal_obj,
		  u_context,
		  u_rule,
		  u_belief_path,
		  u_believe_other_c63,
		  u_bd,
		  u_top_level_goal
		],
		
		[ Goal_Param,
		  Goal_obj_Param,
		  Context_Param,
		  Rule_Param,
		  Belief_path_Param,
		  Believe_other_c63_Param,
		  Bd_Param,
		  Top_level_goal_Param
		],
		arginfo{ all:
			     [ u_goal,
			       u_goal_obj,
			       u_context,
			       u_rule,
			       u_belief_path,
			       u_believe_other_c63,
			       u_bd,
			       u_top_level_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_goal_obj,
				 u_context,
				 u_rule,
				 u_belief_path,
				 u_believe_other_c63,
				 u_bd,
				 u_top_level_goal
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       u_goal_obj,
			       u_context,
			       u_rule,
			       u_belief_path,
			       u_believe_other_c63,
			       u_bd,
			       u_top_level_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-GENERIC-PLAN 
wl: init_args(exact_only, u_run_generic_plan).


% annotating U::RUN-GENERIC-PLAN 
f_u_run_generic_plan(Goal_Param, Goal_obj_Param, Context_Param, Rule_Param, Belief_path_Param, Believe_other_c63_Param, Bd_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_goal_obj, Goal_obj_Param), bv(u_context, Context_Param), bv(u_rule, Rule_Param), bv(u_belief_path, Belief_path_Param), bv(u_believe_other_c63, Believe_other_c63_Param), bv(u_bd, Bd_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ndbg_add_item(Rule_Param, Add_item_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
				       #\(' '),
				       #\(g),
				       #\(e),
				       #\(n),
				       #\(e),
				       #\(r),
				       #\(i),
				       #\(c),
				       #\(' '),
				       #\(p),
				       #\(l),
				       #\(a),
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
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_rule,
			    u_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_ob_c36_get(Rule_Param, u_subgoal, Rule_subgoal_obj_Init),
	LEnv=[[bv(u_rule_subgoal_obj, Rule_subgoal_obj_Init), bv(u_new_goal, []), bv(u_sprouted_context, []), bv(u_sprouted_contexts, [])]|Env],
	get_var(LEnv, u_rule_subgoal_obj, Rule_subgoal_obj_Get),
	f_u_ty_c36_instance_c63(Rule_subgoal_obj_Get, u_rcode, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_xx_ob_bindings_xx, Xx_ob_bindings_xx_Get),
	    Env=[[bv(u_old_ob_bindings, Xx_ob_bindings_xx_Get)]|LEnv],
	    set_var(Env, u_xx_ob_bindings_xx, Bd_Param),
	    cl_eval([u_ob_c36_get, u_rule_subgoal_obj, [quote, u_obj]],
		    Sprouted_contexts),
	    set_var(Env, u_sprouted_contexts, Sprouted_contexts),
	    get_var(Env, u_old_ob_bindings, Old_ob_bindings_Get),
	    set_var(Env, u_xx_ob_bindings_xx, Old_ob_bindings_Get),
	    ElseResult55=Old_ob_bindings_Get
	;   get_var(LEnv, u_rule_subgoal_obj, Rule_subgoal_obj_Get45),
	    f_u_ty_c36_instance_c63(Rule_subgoal_obj_Get45, u_ror, IFTEST43),
	    (   IFTEST43\==[]
	    ->  cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\('P'),
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
				      #\(' '),
				      #\(~),
				      #\('A'),
				      #\(' '),
				      #\(h),
				      #\(a),
				      #\(s),
				      #\(' '),
				      #\('R'),
				      #\('O'),
				      #\('R'),
				      #\(' '),
				      #\(a),
				      #\(s),
				      #\(' '),
				      #\(s),
				      #\(u),
				      #\(b),
				      #\(g),
				      #\(o),
				      #\(a),
				      #\(l),
				      #\(-),
				      #\(-),
				      #\(n),
				      #\(o),
				      #\(t),
				      #\(' '),
				      #\(a),
				      #\(l),
				      #\(l),
				      #\(o),
				      #\(w),
				      #\(e),
				      #\(d)
				    ]),
			   Rule_Param
			 ],
			 TrueResult54),
		ElseResult55=TrueResult54
	    ;   get_var(LEnv, u_else, IFTEST47),
		(   IFTEST47\==[]
		->  f_u_cx_c36_sprout(Context_Param, Sprouted_context),
		    set_var(LEnv, u_sprouted_context, Sprouted_context),
		    f_u_delay_dbgs(u_sprouted_context,
				   
				   [ 
				     [ cond,
				       
				       [ u_believe_other_c63,
					 
					 [ u_rule_fire_msg,
					   u_rule,
					   '$ARRAY'([*],
						    claz_base_character,
						    
						    [ #\(b),
						      #\(a),
						      #\(c),
						      #\(k),
						      #\(w),
						      #\(a),
						      #\(r),
						      #\(d),
						      #\(' '),
						      #\(v),
						      #\(i),
						      #\(c),
						      #\(a),
						      #\(r),
						      #\(i),
						      #\(o),
						      #\(u),
						      #\(s),
						      #\(' '),
						      #\(p),
						      #\(l),
						      #\(a),
						      #\(n)
						    ]),
					   u_context,
					   u_bd,
					   u_sprouted_context,
					   u_goal
					 ]
				       ],
				       
				       [ [u_me_belief_path_c63, u_belief_path],
					 
					 [ u_rule_fire_msg,
					   u_rule,
					   '$ARRAY'([*],
						    claz_base_character,
						    [#\(p), #\(l), #\(a), #\(n)]),
					   u_context,
					   u_bd,
					   u_sprouted_context,
					   u_goal
					 ]
				       ],
				       
				       [ u_else,
					 
					 [ u_rule_fire_msg,
					   u_rule,
					   '$ARRAY'([*],
						    claz_base_character,
						    
						    [ #\(f),
						      #\(o),
						      #\(r),
						      #\(w),
						      #\(a),
						      #\(r),
						      #\(d),
						      #\(' '),
						      #\(v),
						      #\(i),
						      #\(c),
						      #\(a),
						      #\(r),
						      #\(i),
						      #\(o),
						      #\(u),
						      #\(s),
						      #\(' '),
						      #\(p),
						      #\(l),
						      #\(a),
						      #\(n)
						    ]),
					   u_context,
					   u_bd,
					   u_sprouted_context,
					   u_goal
					 ]
				       ]
				     ],
				     
				     [ u_set_ordering,
				       u_sprouted_context,
				       
				       [ u_ob_c36_get,
					 u_rule,
					 [quote, u_plausibility]
				       ]
				     ],
				     
				     [ if,
				       [u_vars_in_c63, u_goal_obj],
				       
				       [ setq,
					 u_new_goal,
					 
					 [ or,
					   
					   [ u_plan_instantiate,
					     u_goal,
					     [u_bd_no_var_bds, u_bd],
					     u_sprouted_context,
					     u_top_level_goal,
					     u_belief_path,
					     []
					   ],
					   u_goal
					 ]
				       ],
				       [setq, u_new_goal, u_goal]
				     ],
				     
				     [ u_instan_and_activate_subgoals,
				       u_new_goal,
				       [u_rule_subgoal_objs, u_rule],
				       u_bd,
				       u_rule,
				       u_sprouted_context,
				       
				       [ u_ty_c36_instance_c63,
					 u_rule_subgoal_obj,
					 [quote, u_rseq]
				       ],
				       [],
				       u_believe_other_c63,
				       u_top_level_goal,
				       u_belief_path
				     ]
				   ],
				   Delay_dbgs_Ret),
		    get_var(LEnv, u_sprouted_context, Sprouted_context_Get),
		    TrueResult=[Sprouted_context_Get],
		    set_var(LEnv, u_sprouted_contexts, TrueResult),
		    ElseResult55=TrueResult
		;   ElseResult55=[]
		)
	    )
	),
	f_u_ob_c36_get(Rule_Param, u_halt_c63, IFTEST60),
	(   IFTEST60\==[]
	->  f_u_dd_goal_c63(Top_level_goal_Param, Not_Param),
	    cl_not(Not_Param, TrueResult64),
	    IFTEST58=TrueResult64
	;   IFTEST58=[]
	),
	(   IFTEST58\==[]
	->  f_u_ob_c36_set(Top_level_goal_Param,
			   u_status,
			   u_fired_halt,
			   TrueResult66),
	    _121936=TrueResult66
	;   _121936=[]
	),
	f_u_ndbg_remove_item(Rule_Param, Remove_item_Ret),
	get_var(LEnv, u_sprouted_contexts, Sprouted_contexts_Get),
	LetResult=Sprouted_contexts_Get,
	LetResult=FnResult.
:- set_opv(f_u_run_generic_plan, classof, claz_function),
   set_opv(u_run_generic_plan, compile_as, kw_function),
   set_opv(u_run_generic_plan, function, f_u_run_generic_plan),
   DefunResult=u_run_generic_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (rule-goal-obj (ob$get rule 'goal)) never used",
				     1,
				     9194)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The lambda object of RCODE is responsible for:",
				     15,
				     9452)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Taking arguments (goal context top-level-goal rule bd)",
				     15,
				     9515)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Returning a list of sprouted contexts (yes, RCODE, a",
				     15,
				     9588)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   single rule may actually cause several plans to",
				     15,
				     9659)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   be generated, e.g., Reversal-Plan does this)",
				     15,
				     9725)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Creating INTENDS links in each sprouted context",
				     15,
				     9788)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Setting the ordering for each sprouted context",
				     15,
				     9854)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   (using [set-ordering sprout value])",
				     15,
				     9919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Linearization using the following code:",
				     15,
				     9973)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (delay-dbgs sprouted-context", 15, 10031)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (rule-fire-msg rule \"plan\" context bd sprouted-context goal)",
				     11,
				     10072)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (ndbg-roman-nl *gate-dbg* rule \"Coded plan\")",
				     15,
				     10154)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      <other stuff that produces rule output>)",
				     15,
				     10220)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 15, 10282)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              (intends (ob$fcreate `(INTENDS linked-from ,goal",
				     1,
				     10284)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                               rule ,rule)))",
				     1,
				     10348)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (setq sprouted-context (cx$sprout context))",
				     1,
				     10410)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (set-ordering sprouted-context",
				     1,
				     10468)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                            (ob$get rule 'plausibility))",
				     1,
				     10513)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (was) to stop later firing", 1, 10705)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (cx$assert-relative sprouted-context intends belief-path)",
				     1,
				     10734)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below modification enables variable instantiation",
				     13,
				     11584)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" from the goal unification. Iynwim.",
				     13,
				     11652)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              (any? (lambda (var) (bd-lookup (variable-name var) bd))",
				     1,
				     11725)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:8892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                       (variables-in goal-obj))",
				     1,
				     11796)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bd-no-var-bds',
			    [bd],
			    
			    [ let,
			      [['new-bd', []], [val, []]],
			      
			      [ 'bd-walk',
				
				[ lambda,
				  [var],
				  [setq, val, ['bd-hyper-lookup', var, bd]],
				  
				  [ if,
				    [and, val, [not, ['var?', val]]],
				    
				    [ setq,
				      'new-bd',
				      [cons, [list, var, val], 'new-bd']
				    ]
				  ]
				],
				bd
			      ],
			      [cons, [quote, t], 'new-bd']
			    ]
			  ]).

% annotating U::BD-NO-VAR-BDS 
wl: lambda_def(defun,
	      u_bd_no_var_bds,
	      f_u_bd_no_var_bds,
	      [u_bd],
	      
	      [ 
		[ let,
		  [[u_new_bd, []], [u_val, []]],
		  
		  [ u_bd_walk,
		    
		    [ lambda,
		      [u_var],
		      [setq, u_val, [u_bd_hyper_lookup, u_var, u_bd]],
		      
		      [ if,
			[and, u_val, [not, [u_var_c63, u_val]]],
			[setq, u_new_bd, [cons, [list, u_var, u_val], u_new_bd]]
		      ]
		    ],
		    u_bd
		  ],
		  [cons, [quote, t], u_new_bd]
		]
	      ]).


% annotating U::BD-NO-VAR-BDS 
wl: arglist_info(u_bd_no_var_bds,
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

% annotating U::BD-NO-VAR-BDS 
wl: init_args(exact_only, u_bd_no_var_bds).


% annotating U::BD-NO-VAR-BDS 
f_u_bd_no_var_bds(Bd_Param, FnResult) :-
	Env=[bv(u_bd, Bd_Param)],
	LEnv=[[bv(u_new_bd, []), bv(u_val, [])]|Env],
	Lambda=closure([Env|LEnv], LResult, [u_var],  (f_u_bd_hyper_lookup(u_var, u_bd, Bd), set_var(Env, u_val, Bd), get_var(Env, u_val, IFTEST17), (IFTEST17\==[]->f_u_var_c63(u_val, Not_Param), cl_not(Not_Param, TrueResult), IFTEST=TrueResult;IFTEST=[]), (IFTEST\==[]->get_var(Env, u_var, Var_Get), CAR=[Var_Get, IFTEST17], get_var(Env, u_new_bd, New_bd_Get31), TrueResult25=[CAR|New_bd_Get31], set_var(Env, u_new_bd, TrueResult25), LResult=TrueResult25;LResult=[]))),
	f_u_bd_walk(Lambda, Bd_Param, Bd_walk_Ret),
	LetResult=[t|New_bd_Get31],
	LetResult=FnResult.
:- set_opv(f_u_bd_no_var_bds, classof, claz_function),
   set_opv(u_bd_no_var_bds, compile_as, kw_function),
   set_opv(u_bd_no_var_bds, function, f_u_bd_no_var_bds),
   DefunResult=u_bd_no_var_bds.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13155)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The code has to special case on goal activation rules to not",
				     1,
				     13157)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" initiate a goal if it is already satisfied (by why would it be initiated",
				     1,
				     13220)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" if already satisfied?)", 1, 13295)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13320)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Perhaps reality scaling should be done on the inference side, rather",
				     1,
				     13322)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" than the planning side. However, this doesn't quite feel right.",
				     1,
				     13393)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:12848 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Or on both sides? (then make the numbers higher?)",
				     1,
				     13459)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:13511 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-inferences',
			    [context, 'top-level-goal', 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Run inferences in ~A, bp = ~A"),
			      context,
			      'belief-path'
			    ],
			    
			    [ yloop,
			      [initial, ['ever-fired?', []]],
			      
			      [ ywhile,
				
				[ 'run-inferences1',
				  context,
				  'top-level-goal',
				  'belief-path'
				]
			      ],
			      [ydo, [setq, 'ever-fired?', t]],
			      [yresult, 'ever-fired?']
			    ]
			  ]).

% annotating U::RUN-INFERENCES 
wl: lambda_def(defun,
	      u_run_inferences,
	      f_u_run_inferences,
	      [u_context, u_top_level_goal, u_belief_path],
	      
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
			     #\(i),
			     #\(n),
			     #\(f),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(c),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(','),
			     #\(' '),
			     #\(b),
			     #\(p),
			     #\(' '),
			     #\(=),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_context,
		  u_belief_path
		],
		
		[ u_yloop,
		  [u_initial, [u_ever_fired_c63, []]],
		  
		  [ u_ywhile,
		    
		    [ u_run_inferences1,
		      u_context,
		      u_top_level_goal,
		      u_belief_path
		    ]
		  ],
		  [u_ydo, [setq, u_ever_fired_c63, t]],
		  [u_yresult, u_ever_fired_c63]
		]
	      ]).


% annotating U::RUN-INFERENCES 
wl: arglist_info(u_run_inferences,
		[u_context, u_top_level_goal, u_belief_path],
		[Context_Param, Top_level_goal_Param, Belief_path_Param],
		arginfo{ all:[u_context, u_top_level_goal, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_top_level_goal, u_belief_path],
			 opt:0,
			 req:[u_context, u_top_level_goal, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-INFERENCES 
wl: init_args(exact_only, u_run_inferences).


% annotating U::RUN-INFERENCES 
f_u_run_inferences(Context_Param, Top_level_goal_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(b),
				       #\(p),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_context,
			    u_belief_path
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_ever_fired_c63, []]],
		    
		    [ u_ywhile,
		      
		      [ u_run_inferences1,
			u_context,
			u_top_level_goal,
			u_belief_path
		      ]
		    ],
		    [u_ydo, [setq, u_ever_fired_c63, t]],
		    [u_yresult, u_ever_fired_c63]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_run_inferences, classof, claz_function),
   set_opv(u_run_inferences, compile_as, kw_function),
   set_opv(u_run_inferences, function, f_u_run_inferences),
   DefunResult=u_run_inferences.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:13836 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dbg-bo',
			    [rule, goal, context],
			    
			    [ let,
			      
			      [ ['goal-obj', ['ob$get', goal, [quote, obj]]],
				[other, ['ob$get', goal, [quote, actor]]],
				[result, []]
			      ],
			      [trace, 'ob$unify0'],
			      
			      [ setq,
				result,
				
				[ 'rule-applications',
				  'goal-obj',
				  context,
				  rule,
				  '*me-belief-path*',
				  other
				]
			      ],
			      [untrace, 'ob$unify0'],
			      result
			    ]
			  ]).

% annotating U::DBG-BO 
wl: lambda_def(defun,
	      u_dbg_bo,
	      f_u_dbg_bo,
	      [u_rule, u_goal, u_context],
	      
	      [ 
		[ let,
		  
		  [ [u_goal_obj, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		    [u_other, [u_ob_c36_get, u_goal, [quote, u_actor]]],
		    [u_result, []]
		  ],
		  [trace, u_ob_c36_unify0],
		  
		  [ setq,
		    u_result,
		    
		    [ u_rule_applications,
		      u_goal_obj,
		      u_context,
		      u_rule,
		      u_xx_me_belief_path_xx,
		      u_other
		    ]
		  ],
		  [untrace, u_ob_c36_unify0],
		  u_result
		]
	      ]).


% annotating U::DBG-BO 
wl: arglist_info(u_dbg_bo,
		[u_rule, u_goal, u_context],
		[Rule_Param, Goal_Param, Context_Param],
		arginfo{ all:[u_rule, u_goal, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_goal, u_context],
			 opt:0,
			 req:[u_rule, u_goal, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DBG-BO 
wl: init_args(exact_only, u_dbg_bo).


% annotating U::DBG-BO 
f_u_dbg_bo(Rule_Param, Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_get(Goal_Param, u_obj, Goal_obj_Init),
	f_u_ob_c36_get(Goal_Param, u_actor, Other_Init),
	LEnv=[[bv(u_goal_obj, Goal_obj_Init), bv(u_other, Other_Init), bv(u_result, [])]|Env],
	cl_trace(u_ob_c36_unify0, Trace_Ret),
	get_var(LEnv, u_goal_obj, Goal_obj_Get),
	get_var(LEnv, u_other, Other_Get),
	get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_rule_applications(Goal_obj_Get,
			      Context_Param,
			      Rule_Param,
			      Xx_me_belief_path_xx_Get,
			      Other_Get,
			      Result31),
	set_var(LEnv, u_result, Result31),
	cl_untrace(u_ob_c36_unify0, Untrace_Ret),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_dbg_bo, classof, claz_function),
   set_opv(u_dbg_bo, compile_as, kw_function),
   set_opv(u_dbg_bo, function, f_u_dbg_bo),
   DefunResult=u_dbg_bo.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:14107 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dbg-prule',
			    [rule, 'goal-obj', context],
			    
			    [ let,
			      [[result, []]],
			      [trace, 'ob$unify0'],
			      
			      [ setq,
				result,
				
				[ 'rule-applications',
				  'goal-obj',
				  context,
				  rule,
				  '*me-belief-path*',
				  []
				]
			      ],
			      [untrace, 'ob$unify0'],
			      result
			    ]
			  ]).

% annotating U::DBG-PRULE 
wl: lambda_def(defun,
	      u_dbg_prule,
	      f_u_dbg_prule,
	      [u_rule, u_goal_obj, u_context],
	      
	      [ 
		[ let,
		  [[u_result, []]],
		  [trace, u_ob_c36_unify0],
		  
		  [ setq,
		    u_result,
		    
		    [ u_rule_applications,
		      u_goal_obj,
		      u_context,
		      u_rule,
		      u_xx_me_belief_path_xx,
		      []
		    ]
		  ],
		  [untrace, u_ob_c36_unify0],
		  u_result
		]
	      ]).


% annotating U::DBG-PRULE 
wl: arglist_info(u_dbg_prule,
		[u_rule, u_goal_obj, u_context],
		[Rule_Param, Goal_obj_Param, Context_Param],
		arginfo{ all:[u_rule, u_goal_obj, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_goal_obj, u_context],
			 opt:0,
			 req:[u_rule, u_goal_obj, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DBG-PRULE 
wl: init_args(exact_only, u_dbg_prule).


% annotating U::DBG-PRULE 
f_u_dbg_prule(Rule_Param, Goal_obj_Param, Context_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_goal_obj, Goal_obj_Param), bv(u_context, Context_Param)],
	LEnv=[[bv(u_result, [])]|Env],
	cl_trace(u_ob_c36_unify0, Trace_Ret),
	get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_rule_applications(Goal_obj_Param,
			      Context_Param,
			      Rule_Param,
			      Xx_me_belief_path_xx_Get,
			      [],
			      Result26),
	set_var(LEnv, u_result, Result26),
	cl_untrace(u_ob_c36_unify0, Untrace_Ret),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_dbg_prule, classof, claz_function),
   set_opv(u_dbg_prule, compile_as, kw_function),
   set_opv(u_dbg_prule, function, f_u_dbg_prule),
   DefunResult=u_dbg_prule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:14373 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dbg-prulex',
			    [rule, 'goal-obj', context],
			    
			    [ 'rule-applications',
			      'goal-obj',
			      context,
			      rule,
			      '*me-belief-path*',
			      []
			    ]
			  ]).

% annotating U::DBG-PRULEX 
wl: lambda_def(defun,
	      u_dbg_prulex,
	      f_u_dbg_prulex,
	      [u_rule, u_goal_obj, u_context],
	      
	      [ 
		[ u_rule_applications,
		  u_goal_obj,
		  u_context,
		  u_rule,
		  u_xx_me_belief_path_xx,
		  []
		]
	      ]).


% annotating U::DBG-PRULEX 
wl: arglist_info(u_dbg_prulex,
		[u_rule, u_goal_obj, u_context],
		[Rule_Param, Goal_obj_Param, Context_Param],
		arginfo{ all:[u_rule, u_goal_obj, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_goal_obj, u_context],
			 opt:0,
			 req:[u_rule, u_goal_obj, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DBG-PRULEX 
wl: init_args(exact_only, u_dbg_prulex).


% annotating U::DBG-PRULEX 
f_u_dbg_prulex(Rule_Param, Goal_obj_Param, Context_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_goal_obj, Goal_obj_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_rule_applications(Goal_obj_Param,
			      Context_Param,
			      Rule_Param,
			      Xx_me_belief_path_xx_Get,
			      [],
			      Rule_applications_Ret),
	Rule_applications_Ret=FnResult.
:- set_opv(f_u_dbg_prulex, classof, claz_function),
   set_opv(u_dbg_prulex, compile_as, kw_function),
   set_opv(u_dbg_prulex, function, f_u_dbg_prulex),
   DefunResult=u_dbg_prulex.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:14482 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dbg-irule',
			    [rule, context],
			    
			    [ let,
			      [['show-results', []]],
			      [trace, 'ob$unify0'],
			      
			      [ setq,
				'show-results',
				
				[ show,
				  ['ob$get', rule, [quote, subgoal]],
				  
				  [ if,
				    ['ob$get', rule, [quote, 'reality-subgoal']],
				    '*reality*',
				    context
				  ],
				  ['planner-empty-bd', '*me-belief-path*'],
				  '*me-belief-path*'
				]
			      ],
			      [untrace, 'ob$unify0'],
			      'show-results'
			    ]
			  ]).

% annotating U::DBG-IRULE 
wl: lambda_def(defun,
	      u_dbg_irule,
	      f_u_dbg_irule,
	      [u_rule, u_context],
	      
	      [ 
		[ let,
		  [[u_show_results, []]],
		  [trace, u_ob_c36_unify0],
		  
		  [ setq,
		    u_show_results,
		    
		    [ u_show,
		      [u_ob_c36_get, u_rule, [quote, u_subgoal]],
		      
		      [ if,
			[u_ob_c36_get, u_rule, [quote, u_reality_subgoal]],
			u_xx_reality_xx,
			u_context
		      ],
		      [u_planner_empty_bd, u_xx_me_belief_path_xx],
		      u_xx_me_belief_path_xx
		    ]
		  ],
		  [untrace, u_ob_c36_unify0],
		  u_show_results
		]
	      ]).


% annotating U::DBG-IRULE 
wl: arglist_info(u_dbg_irule,
		[u_rule, u_context],
		[Rule_Param, Context_Param],
		arginfo{ all:[u_rule, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_context],
			 opt:0,
			 req:[u_rule, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DBG-IRULE 
wl: init_args(exact_only, u_dbg_irule).


% annotating U::DBG-IRULE 
f_u_dbg_irule(Rule_Param, Context_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_context, Context_Param)],
	LEnv=[[bv(u_show_results, [])]|Env],
	cl_trace(u_ob_c36_unify0, Trace_Ret),
	f_u_ob_c36_get(Rule_Param, u_subgoal, Subgoal),
	f_u_ob_c36_get(Rule_Param, u_reality_subgoal, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_xx_reality_xx, Xx_reality_xx_Get),
	    _115284=Xx_reality_xx_Get
	;   _115284=Context_Param
	),
	get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_planner_empty_bd(Xx_me_belief_path_xx_Get, Empty_bd_Ret),
	get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get26),
	f_u_show(Subgoal,
		 _115284,
		 Empty_bd_Ret,
		 Xx_me_belief_path_xx_Get26,
		 Show_results),
	set_var(LEnv, u_show_results, Show_results),
	cl_untrace(u_ob_c36_unify0, Untrace_Ret),
	get_var(LEnv, u_show_results, Show_results_Get),
	LetResult=Show_results_Get,
	LetResult=FnResult.
:- set_opv(f_u_dbg_irule, classof, claz_function),
   set_opv(u_dbg_irule, compile_as, kw_function),
   set_opv(u_dbg_irule, function, f_u_dbg_irule),
   DefunResult=u_dbg_irule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:14944 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dbg-irulex',
			    [rule, context],
			    
			    [ show,
			      ['ob$get', rule, [quote, subgoal]],
			      
			      [ if,
				['ob$get', rule, [quote, 'reality-subgoal']],
				'*reality*',
				context
			      ],
			      ['planner-empty-bd', '*me-belief-path*'],
			      '*me-belief-path*'
			    ]
			  ]).

% annotating U::DBG-IRULEX 
wl: lambda_def(defun,
	      u_dbg_irulex,
	      f_u_dbg_irulex,
	      [u_rule, u_context],
	      
	      [ 
		[ u_show,
		  [u_ob_c36_get, u_rule, [quote, u_subgoal]],
		  
		  [ if,
		    [u_ob_c36_get, u_rule, [quote, u_reality_subgoal]],
		    u_xx_reality_xx,
		    u_context
		  ],
		  [u_planner_empty_bd, u_xx_me_belief_path_xx],
		  u_xx_me_belief_path_xx
		]
	      ]).


% annotating U::DBG-IRULEX 
wl: arglist_info(u_dbg_irulex,
		[u_rule, u_context],
		[Rule_Param, Context_Param],
		arginfo{ all:[u_rule, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_context],
			 opt:0,
			 req:[u_rule, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DBG-IRULEX 
wl: init_args(exact_only, u_dbg_irulex).


% annotating U::DBG-IRULEX 
f_u_dbg_irulex(Rule_Param, Context_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_get(Rule_Param, u_subgoal, Subgoal),
	f_u_ob_c36_get(Rule_Param, u_reality_subgoal, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get),
	    _114874=Xx_reality_xx_Get
	;   _114874=Context_Param
	),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_planner_empty_bd(Xx_me_belief_path_xx_Get, Empty_bd_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get23),
	f_u_show(Subgoal,
		 _114874,
		 Empty_bd_Ret,
		 Xx_me_belief_path_xx_Get23,
		 Show_Ret),
	Show_Ret=FnResult.
:- set_opv(f_u_dbg_irulex, classof, claz_function),
   set_opv(u_dbg_irulex, compile_as, kw_function),
   set_opv(u_dbg_irulex, function, f_u_dbg_irulex),
   DefunResult=u_dbg_irulex.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:15142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-inferences1',
			    [context, 'top-level-goal', 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ assertions,
				  
				  [ 'beliefs-of',
				    ['ob$get', context, [quote, 'touched-facts']],
				    'belief-path'
				  ]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'rule-long',
				'$STRING'("Inference cycle in ~A"),
				context
			      ],
			      
			      [ if,
				['me-belief-path?', 'belief-path'],
				['ob$removes', context, [quote, 'touched-facts']]
			      ],
			      
			      [ yloop,
				
				[ initial,
				  ['show-results', []],
				  [ctxt, []],
				  ['rule-fired?', []],
				  ['already-inferred', []]
				],
				
				[ yfor,
				  rule,
				  in,
				  ['collect-inference-rules', assertions]
				],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ 'self-type-ok?',
					rule,
					[car, 'belief-path']
				      ],
				      
				      [ or,
					['null?', '*top-level-goal*'],
					
					[ 'null?',
					  
					  [ 'ob$get',
					    rule,
					    [quote, 'top-level-goal']
					  ]
					],
					
					[ 'ob$unify',
					  
					  [ 'ob$get',
					    rule,
					    [quote, 'top-level-goal']
					  ],
					  
					  [ 'ob$get',
					    '*top-level-goal*',
					    [quote, obj]
					  ],
					  ['planner-empty-bd', 'belief-path']
					]
				      ]
				    ],
				    
				    [ progn,
				      ['ndbg-add-item', rule],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					inference,
					'$STRING'("Considering rule ~A"),
					rule
				      ],
				      
				      [ if,
					['global-inference?', rule],
					[setq, ctxt, '*reality*'],
					[setq, ctxt, context]
				      ],
				      
				      [ if,
					
					[ and,
					  ['inference?', rule],
					  
					  [ setq,
					    'show-results',
					    
					    [ show,
					      ['ob$get', rule, [quote, subgoal]],
					      
					      [ if,
						
						[ 'ob$get',
						  rule,
						  [quote, 'reality-subgoal']
						],
						'*reality*',
						context
					      ],
					      
					      [ 'planner-empty-bd',
						'belief-path'
					      ],
					      'belief-path'
					    ]
					  ]
					],
					
					[ progn,
					  
					  [ setq,
					    'already-inferred',
					    
					    [ 'facts-inferred-by',
					      rule,
					      ctxt,
					      'belief-path'
					    ]
					  ],
					  
					  [ yloop,
					    
					    [ yfor,
					      'show-result',
					      in,
					      'show-results'
					    ],
					    
					    [ ydo,
					      
					      [ 'ndbg-roman-nl',
						'*gate-dbg*',
						inference,
						'$STRING'("Considering show result ~A"),
						'show-result'
					      ],
					      
					      [ if,
						
						[ not,
						  
						  [ 'already-inferred?',
						    'show-result',
						    ctxt,
						    'already-inferred',
						    'belief-path',
						    rule
						  ]
						],
						
						[ progn,
						  
						  [ setq,
						    'rule-fired?',
						    
						    [ or,
						      
						      [ 'inference-fire',
							context,
							ctxt,
							'top-level-goal',
							'belief-path',
							rule,
							'show-result'
						      ],
						      'rule-fired?'
						    ]
						  ]
						]
					      ]
					    ]
					  ]
					]
				      ],
				      ['ndbg-remove-item', rule]
				    ]
				  ]
				],
				[yresult, 'rule-fired?']
			      ]
			    ]
			  ]).

% annotating U::RUN-INFERENCES1 
wl: lambda_def(defun,
	      u_run_inferences1,
	      f_u_run_inferences1,
	      [u_context, u_top_level_goal, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_assertions,
		      
		      [ u_beliefs_of,
			[u_ob_c36_get, u_context, [quote, u_touched_facts]],
			u_belief_path
		      ]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule_long,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('I'),
			       #\(n),
			       #\(f),
			       #\(e),
			       #\(r),
			       #\(e),
			       #\(n),
			       #\(c),
			       #\(e),
			       #\(' '),
			       #\(c),
			       #\(y),
			       #\(c),
			       #\(l),
			       #\(e),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_context
		  ],
		  
		  [ if,
		    [u_me_belief_path_c63, u_belief_path],
		    [u_ob_c36_removes, u_context, [quote, u_touched_facts]]
		  ],
		  
		  [ u_yloop,
		    
		    [ u_initial,
		      [u_show_results, []],
		      [u_ctxt, []],
		      [u_rule_fired_c63, []],
		      [u_already_inferred, []]
		    ],
		    
		    [ u_yfor,
		      u_rule,
		      u_in,
		      [u_collect_inference_rules, u_assertions]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_self_type_ok_c63, u_rule, [car, u_belief_path]],
			  
			  [ or,
			    [u_null_c63, u_xx_top_level_goal_xx],
			    
			    [ u_null_c63,
			      [u_ob_c36_get, u_rule, [quote, u_top_level_goal]]
			    ],
			    
			    [ u_ob_c36_unify,
			      [u_ob_c36_get, u_rule, [quote, u_top_level_goal]],
			      
			      [ u_ob_c36_get,
				u_xx_top_level_goal_xx,
				[quote, u_obj]
			      ],
			      [u_planner_empty_bd, u_belief_path]
			    ]
			  ]
			],
			
			[ progn,
			  [u_ndbg_add_item, u_rule],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_inference,
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
				       #\(r),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_rule
			  ],
			  
			  [ if,
			    [u_global_inference_c63, u_rule],
			    [setq, u_ctxt, u_xx_reality_xx],
			    [setq, u_ctxt, u_context]
			  ],
			  
			  [ if,
			    
			    [ and,
			      [u_inference_c63, u_rule],
			      
			      [ setq,
				u_show_results,
				
				[ u_show,
				  [u_ob_c36_get, u_rule, [quote, u_subgoal]],
				  
				  [ if,
				    
				    [ u_ob_c36_get,
				      u_rule,
				      [quote, u_reality_subgoal]
				    ],
				    u_xx_reality_xx,
				    u_context
				  ],
				  [u_planner_empty_bd, u_belief_path],
				  u_belief_path
				]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_already_inferred,
				
				[ u_facts_inferred_by,
				  u_rule,
				  u_ctxt,
				  u_belief_path
				]
			      ],
			      
			      [ u_yloop,
				[u_yfor, u_show_result, u_in, u_show_results],
				
				[ u_ydo,
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_inference,
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
					       #\(s),
					       #\(h),
					       #\(o),
					       #\(w),
					       #\(' '),
					       #\(r),
					       #\(e),
					       #\(s),
					       #\(u),
					       #\(l),
					       #\(t),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_show_result
				  ],
				  
				  [ if,
				    
				    [ not,
				      
				      [ u_already_inferred_c63,
					u_show_result,
					u_ctxt,
					u_already_inferred,
					u_belief_path,
					u_rule
				      ]
				    ],
				    
				    [ progn,
				      
				      [ setq,
					u_rule_fired_c63,
					
					[ or,
					  
					  [ u_inference_fire,
					    u_context,
					    u_ctxt,
					    u_top_level_goal,
					    u_belief_path,
					    u_rule,
					    u_show_result
					  ],
					  u_rule_fired_c63
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ],
			  [u_ndbg_remove_item, u_rule]
			]
		      ]
		    ],
		    [u_yresult, u_rule_fired_c63]
		  ]
		]
	      ]).


% annotating U::RUN-INFERENCES1 
wl: arglist_info(u_run_inferences1,
		[u_context, u_top_level_goal, u_belief_path],
		[Context_Param, Top_level_goal_Param, Belief_path_Param],
		arginfo{ all:[u_context, u_top_level_goal, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_top_level_goal, u_belief_path],
			 opt:0,
			 req:[u_context, u_top_level_goal, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-INFERENCES1 
wl: init_args(exact_only, u_run_inferences1).


% annotating U::RUN-INFERENCES1 
f_u_run_inferences1(Context_Param, Top_level_goal_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ob_c36_get(Context_Param, u_touched_facts, Touched_facts),
	f_u_beliefs_of(Touched_facts, Belief_path_Param, Assertions_Init),
	LEnv=[[bv(u_assertions, Assertions_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(' '),
				       #\(c),
				       #\(y),
				       #\(c),
				       #\(l),
				       #\(e),
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
	f_u_me_belief_path_c63(u_belief_path, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_removes(Context_Param, u_touched_facts, TrueResult),
	    _120826=TrueResult
	;   _120826=[]
	),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_show_results, []],
		      [u_ctxt, []],
		      [u_rule_fired_c63, []],
		      [u_already_inferred, []]
		    ],
		    
		    [ u_yfor,
		      u_rule,
		      u_in,
		      [u_collect_inference_rules, u_assertions]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_self_type_ok_c63, u_rule, [car, u_belief_path]],
			  
			  [ or,
			    [u_null_c63, u_xx_top_level_goal_xx],
			    
			    [ u_null_c63,
			      [u_ob_c36_get, u_rule, [quote, u_top_level_goal]]
			    ],
			    
			    [ u_ob_c36_unify,
			      [u_ob_c36_get, u_rule, [quote, u_top_level_goal]],
			      
			      [ u_ob_c36_get,
				u_xx_top_level_goal_xx,
				[quote, u_obj]
			      ],
			      [u_planner_empty_bd, u_belief_path]
			    ]
			  ]
			],
			
			[ progn,
			  [u_ndbg_add_item, u_rule],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_inference,
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
				       #\(r),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_rule
			  ],
			  
			  [ if,
			    [u_global_inference_c63, u_rule],
			    [setq, u_ctxt, u_xx_reality_xx],
			    [setq, u_ctxt, u_context]
			  ],
			  
			  [ if,
			    
			    [ and,
			      [u_inference_c63, u_rule],
			      
			      [ setq,
				u_show_results,
				
				[ u_show,
				  [u_ob_c36_get, u_rule, [quote, u_subgoal]],
				  
				  [ if,
				    
				    [ u_ob_c36_get,
				      u_rule,
				      [quote, u_reality_subgoal]
				    ],
				    u_xx_reality_xx,
				    u_context
				  ],
				  [u_planner_empty_bd, u_belief_path],
				  u_belief_path
				]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_already_inferred,
				
				[ u_facts_inferred_by,
				  u_rule,
				  u_ctxt,
				  u_belief_path
				]
			      ],
			      
			      [ u_yloop,
				[u_yfor, u_show_result, u_in, u_show_results],
				
				[ u_ydo,
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_inference,
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
					       #\(s),
					       #\(h),
					       #\(o),
					       #\(w),
					       #\(' '),
					       #\(r),
					       #\(e),
					       #\(s),
					       #\(u),
					       #\(l),
					       #\(t),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_show_result
				  ],
				  
				  [ if,
				    
				    [ not,
				      
				      [ u_already_inferred_c63,
					u_show_result,
					u_ctxt,
					u_already_inferred,
					u_belief_path,
					u_rule
				      ]
				    ],
				    
				    [ progn,
				      
				      [ setq,
					u_rule_fired_c63,
					
					[ or,
					  
					  [ u_inference_fire,
					    u_context,
					    u_ctxt,
					    u_top_level_goal,
					    u_belief_path,
					    u_rule,
					    u_show_result
					  ],
					  u_rule_fired_c63
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ],
			  [u_ndbg_remove_item, u_rule]
			]
		      ]
		    ],
		    [u_yresult, u_rule_fired_c63]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_run_inferences1, classof, claz_function),
   set_opv(u_run_inferences1, compile_as, kw_function),
   set_opv(u_run_inferences1, function, f_u_run_inferences1),
   DefunResult=u_run_inferences1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:15142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This feature is never used?", 35, 16042)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:15142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Below is moved from before if.",
				     11,
				     16422)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:17237 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inference-fire',
			    
			    [ context,
			      ctxt,
			      'top-level-goal',
			      'belief-path',
			      rule,
			      'show-result'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      inference,
			      '$STRING'("Inference fire")
			    ],
			    
			    [ let,
			      [[assertion, []], ['fired?', []]],
			      
			      [ yloop,
				[yfor, elem, in, ['ob$gets', rule, [quote, goal]]],
				
				[ ydo,
				  
				  [ setq,
				    assertion,
				    
				    [ 'ob$instan-strength',
				      elem,
				      [car, 'show-result']
				    ]
				  ],
				  
				  [ if,
				    ['ty$instance?', assertion, [quote, rcode]],
				    
				    [ progn,
				      
				      [ 'possible-fired-msg',
					'fired?',
					rule,
					context,
					'show-result',
					'belief-path'
				      ],
				      [setq, 'fired?', t],
				      
				      [ 'inference-rcode',
					assertion,
					'show-result'
				      ]
				    ],
				    
				    [ setq,
				      'fired?',
				      
				      [ or,
					
					[ 'inference-assert',
					  assertion,
					  ctxt,
					  'top-level-goal',
					  'belief-path',
					  rule,
					  'show-result',
					  'fired?'
					],
					'fired?'
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				'fired?',
				
				[ yloop,
				  
				  [ yfor,
				    elem,
				    in,
				    ['ob$gets', rule, [quote, delete]]
				  ],
				  
				  [ ydo,
				    
				    [ 'inference-retract',
				      
				      [ 'ob$instantiate-o',
					elem,
					[car, 'show-result']
				      ],
				      ctxt,
				      'belief-path'
				    ]
				  ]
				]
			      ],
			      'fired?'
			    ]
			  ]).

% annotating U::INFERENCE-FIRE 
wl: lambda_def(defun,
	      u_inference_fire,
	      f_u_inference_fire,
	      
	      [ u_context,
		u_ctxt,
		u_top_level_goal,
		u_belief_path,
		u_rule,
		u_show_result
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_inference,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('I'),
			     #\(n),
			     #\(f),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(c),
			     #\(e),
			     #\(' '),
			     #\(f),
			     #\(i),
			     #\(r),
			     #\(e)
			   ])
		],
		
		[ let,
		  [[u_assertion, []], [u_fired_c63, []]],
		  
		  [ u_yloop,
		    [u_yfor, u_elem, u_in, [u_ob_c36_gets, u_rule, [quote, u_goal]]],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_assertion,
			[u_ob_c36_instan_strength, u_elem, [car, u_show_result]]
		      ],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_assertion, [quote, u_rcode]],
			
			[ progn,
			  
			  [ u_possible_fired_msg,
			    u_fired_c63,
			    u_rule,
			    u_context,
			    u_show_result,
			    u_belief_path
			  ],
			  [setq, u_fired_c63, t],
			  [u_inference_rcode, u_assertion, u_show_result]
			],
			
			[ setq,
			  u_fired_c63,
			  
			  [ or,
			    
			    [ u_inference_assert,
			      u_assertion,
			      u_ctxt,
			      u_top_level_goal,
			      u_belief_path,
			      u_rule,
			      u_show_result,
			      u_fired_c63
			    ],
			    u_fired_c63
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_fired_c63,
		    
		    [ u_yloop,
		      
		      [ u_yfor,
			u_elem,
			u_in,
			[u_ob_c36_gets, u_rule, [quote, delete]]
		      ],
		      
		      [ u_ydo,
			
			[ u_inference_retract,
			  [u_ob_c36_instantiate_o, u_elem, [car, u_show_result]],
			  u_ctxt,
			  u_belief_path
			]
		      ]
		    ]
		  ],
		  u_fired_c63
		]
	      ]).


% annotating U::INFERENCE-FIRE 
wl: arglist_info(u_inference_fire,
		
		[ u_context,
		  u_ctxt,
		  u_top_level_goal,
		  u_belief_path,
		  u_rule,
		  u_show_result
		],
		
		[ Context_Param,
		  Ctxt_Param,
		  Top_level_goal_Param,
		  Belief_path_Param,
		  Rule_Param,
		  Show_result_Param
		],
		arginfo{ all:
			     [ u_context,
			       u_ctxt,
			       u_top_level_goal,
			       u_belief_path,
			       u_rule,
			       u_show_result
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_context,
				 u_ctxt,
				 u_top_level_goal,
				 u_belief_path,
				 u_rule,
				 u_show_result
			       ],
			 opt:0,
			 req:
			     [ u_context,
			       u_ctxt,
			       u_top_level_goal,
			       u_belief_path,
			       u_rule,
			       u_show_result
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFERENCE-FIRE 
wl: init_args(exact_only, u_inference_fire).


% annotating U::INFERENCE-FIRE 
f_u_inference_fire(Context_Param, Ctxt_Param, Top_level_goal_Param, Belief_path_Param, Rule_Param, Show_result_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_ctxt, Ctxt_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_belief_path, Belief_path_Param), bv(u_rule, Rule_Param), bv(u_show_result, Show_result_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_inference,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(' '),
				       #\(f),
				       #\(i),
				       #\(r),
				       #\(e)
				     ])
			  ],
			  Roman_nl_Ret),
	LEnv=[[bv(u_assertion, []), bv(u_fired_c63, [])]|Env],
	f_u_yloop(
		  [ [u_yfor, u_elem, u_in, [u_ob_c36_gets, u_rule, [quote, u_goal]]],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_assertion,
			[u_ob_c36_instan_strength, u_elem, [car, u_show_result]]
		      ],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_assertion, [quote, u_rcode]],
			
			[ progn,
			  
			  [ u_possible_fired_msg,
			    u_fired_c63,
			    u_rule,
			    u_context,
			    u_show_result,
			    u_belief_path
			  ],
			  [setq, u_fired_c63, t],
			  [u_inference_rcode, u_assertion, u_show_result]
			],
			
			[ setq,
			  u_fired_c63,
			  
			  [ or,
			    
			    [ u_inference_assert,
			      u_assertion,
			      u_ctxt,
			      u_top_level_goal,
			      u_belief_path,
			      u_rule,
			      u_show_result,
			      u_fired_c63
			    ],
			    u_fired_c63
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_fired_c63, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ 
			[ u_yfor,
			  u_elem,
			  u_in,
			  [u_ob_c36_gets, u_rule, [quote, delete]]
			],
			
			[ u_ydo,
			  
			  [ u_inference_retract,
			    
			    [ u_ob_c36_instantiate_o,
			      u_elem,
			      [car, u_show_result]
			    ],
			    u_ctxt,
			    u_belief_path
			  ]
			]
		      ],
		      TrueResult),
	    _118284=TrueResult
	;   _118284=[]
	),
	get_var(LEnv, u_fired_c63, Fired_c63_Get29),
	LetResult=Fired_c63_Get29,
	LetResult=FnResult.
:- set_opv(f_u_inference_fire, classof, claz_function),
   set_opv(u_inference_fire, compile_as, kw_function),
   set_opv(u_inference_fire, function, f_u_inference_fire),
   DefunResult=u_inference_fire.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:17237 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Too bad we can't do retracts first, so they don't retract assertions.",
				     4,
				     18071)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:17237 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (But we need the complex fire criteria from above.)",
				     4,
				     18146)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:18413 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'possible-fired-msg',
			    ['old-fired?', rule, context, 'show-result', bp],
			    
			    [ if,
			      ['null?', 'old-fired?'],
			      
			      [ if,
				['me-belief-path?', bp],
				
				[ 'rule-fire-msg',
				  rule,
				  '$STRING'("inference"),
				  context,
				  [car, 'show-result'],
				  [],
				  []
				],
				
				[ 'rule-fire-msg',
				  rule,
				  '$STRING'("forward vicarious inference"),
				  context,
				  [car, 'show-result'],
				  [],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::POSSIBLE-FIRED-MSG 
wl: lambda_def(defun,
	      u_possible_fired_msg,
	      f_u_possible_fired_msg,
	      [u_old_fired_c63, u_rule, u_context, u_show_result, u_bp],
	      
	      [ 
		[ if,
		  [u_null_c63, u_old_fired_c63],
		  
		  [ if,
		    [u_me_belief_path_c63, u_bp],
		    
		    [ u_rule_fire_msg,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(i),
				 #\(n),
				 #\(f),
				 #\(e),
				 #\(r),
				 #\(e),
				 #\(n),
				 #\(c),
				 #\(e)
			       ]),
		      u_context,
		      [car, u_show_result],
		      [],
		      []
		    ],
		    
		    [ u_rule_fire_msg,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(f),
				 #\(o),
				 #\(r),
				 #\(w),
				 #\(a),
				 #\(r),
				 #\(d),
				 #\(' '),
				 #\(v),
				 #\(i),
				 #\(c),
				 #\(a),
				 #\(r),
				 #\(i),
				 #\(o),
				 #\(u),
				 #\(s),
				 #\(' '),
				 #\(i),
				 #\(n),
				 #\(f),
				 #\(e),
				 #\(r),
				 #\(e),
				 #\(n),
				 #\(c),
				 #\(e)
			       ]),
		      u_context,
		      [car, u_show_result],
		      [],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::POSSIBLE-FIRED-MSG 
wl: arglist_info(u_possible_fired_msg,
		[u_old_fired_c63, u_rule, u_context, u_show_result, u_bp],
		
		[ Old_fired_c63_Param,
		  Rule_Param,
		  Context_Param,
		  Show_result_Param,
		  Bp_Param
		],
		arginfo{ all:
			     [ u_old_fired_c63,
			       u_rule,
			       u_context,
			       u_show_result,
			       u_bp
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_old_fired_c63,
				 u_rule,
				 u_context,
				 u_show_result,
				 u_bp
			       ],
			 opt:0,
			 req:
			     [ u_old_fired_c63,
			       u_rule,
			       u_context,
			       u_show_result,
			       u_bp
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::POSSIBLE-FIRED-MSG 
wl: init_args(exact_only, u_possible_fired_msg).


% annotating U::POSSIBLE-FIRED-MSG 
f_u_possible_fired_msg(Old_fired_c63_Param, Rule_Param, Context_Param, Show_result_Param, Bp_Param, TrueResult32) :-
	Env=[bv(u_old_fired_c63, Old_fired_c63_Param), bv(u_rule, Rule_Param), bv(u_context, Context_Param), bv(u_show_result, Show_result_Param), bv(u_bp, Bp_Param)],
	f_u_null_c63(u_old_fired_c63, IFTEST),
	(   IFTEST\==[]
	->  f_u_me_belief_path_c63(u_bp, IFTEST22),
	    (   IFTEST22\==[]
	    ->  cl_car(Show_result_Param, Car_Ret),
		f_u_rule_fire_msg(Rule_Param,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(i),
					     #\(n),
					     #\(f),
					     #\(e),
					     #\(r),
					     #\(e),
					     #\(n),
					     #\(c),
					     #\(e)
					   ]),
				  Context_Param,
				  Car_Ret,
				  [],
				  [],
				  TrueResult),
		TrueResult32=TrueResult
	    ;   cl_car(Show_result_Param, Car_Ret36),
		f_u_rule_fire_msg(Rule_Param,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(f),
					     #\(o),
					     #\(r),
					     #\(w),
					     #\(a),
					     #\(r),
					     #\(d),
					     #\(' '),
					     #\(v),
					     #\(i),
					     #\(c),
					     #\(a),
					     #\(r),
					     #\(i),
					     #\(o),
					     #\(u),
					     #\(s),
					     #\(' '),
					     #\(i),
					     #\(n),
					     #\(f),
					     #\(e),
					     #\(r),
					     #\(e),
					     #\(n),
					     #\(c),
					     #\(e)
					   ]),
				  Context_Param,
				  Car_Ret36,
				  [],
				  [],
				  ElseResult),
		TrueResult32=ElseResult
	    )
	;   TrueResult32=[]
	).
:- set_opv(f_u_possible_fired_msg, classof, claz_function),
   set_opv(u_possible_fired_msg, compile_as, kw_function),
   set_opv(u_possible_fired_msg, function, f_u_possible_fired_msg),
   DefunResult=u_possible_fired_msg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:18737 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inference-retract',
			    [ob, ctxt, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Inference retract ~A in ~A"),
			      ob,
			      ctxt
			    ],
			    
			    [ let,
			      
			      [ 
				[ found,
				  
				  [ 'cx$retrieve',
				    ctxt,
				    ['relative->absolute', ob, 'belief-path']
				  ]
				]
			      ],
			      
			      [ if,
				['null?', found],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Inference-retract: ~A already false in ~A"),
				  ob,
				  ctxt
				],
				
				[ let,
				  
				  [ 
				    [ leafs,
				      
				      [ 'get-leaf-causes',
					[car, [car, found]],
					ctxt
				      ]
				    ]
				  ],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Leafs = ~A"),
				    leafs
				  ],
				  
				  [ yloop,
				    [initial, ['found?', []]],
				    [yfor, leaf, in, leafs],
				    
				    [ ydo,
				      
				      [ if,
					['ty$instance?', leaf, [quote, action]],
					
					[ progn,
					  ['retract-dependencies', leaf, ctxt],
					  [setq, 'found?', t]
					]
				      ]
				    ],
				    
				    [ yresult,
				      
				      [ if,
					['null?', 'found?'],
					['cx$retract', ctxt, [car, [car, found]]]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::INFERENCE-RETRACT 
wl: lambda_def(defun,
	      u_inference_retract,
	      f_u_inference_retract,
	      [u_ob, u_ctxt, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('I'),
			     #\(n),
			     #\(f),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(c),
			     #\(e),
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
			     #\(~),
			     #\('A')
			   ]),
		  u_ob,
		  u_ctxt
		],
		
		[ let,
		  
		  [ 
		    [ u_found,
		      
		      [ u_cx_c36_retrieve,
			u_ctxt,
			[u_relative_c62_absolute, u_ob, u_belief_path]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, u_found],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('I'),
				 #\(n),
				 #\(f),
				 #\(e),
				 #\(r),
				 #\(e),
				 #\(n),
				 #\(c),
				 #\(e),
				 #\(-),
				 #\(r),
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
				 #\('A')
			       ]),
		      u_ob,
		      u_ctxt
		    ],
		    
		    [ let,
		      
		      [ 
			[ u_leafs,
			  [u_get_leaf_causes, [car, [car, u_found]], u_ctxt]
			]
		      ],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('L'),
				   #\(e),
				   #\(a),
				   #\(f),
				   #\(s),
				   #\(' '),
				   #\(=),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_leafs
		      ],
		      
		      [ u_yloop,
			[u_initial, [u_found_c63, []]],
			[u_yfor, u_leaf, u_in, u_leafs],
			
			[ u_ydo,
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_leaf, [quote, u_action]],
			    
			    [ progn,
			      [u_retract_dependencies, u_leaf, u_ctxt],
			      [setq, u_found_c63, t]
			    ]
			  ]
			],
			
			[ u_yresult,
			  
			  [ if,
			    [u_null_c63, u_found_c63],
			    [u_cx_c36_retract, u_ctxt, [car, [car, u_found]]]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::INFERENCE-RETRACT 
wl: arglist_info(u_inference_retract,
		[u_ob, u_ctxt, u_belief_path],
		[Ob_Param, Ctxt_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_ctxt, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_ctxt, u_belief_path],
			 opt:0,
			 req:[u_ob, u_ctxt, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFERENCE-RETRACT 
wl: init_args(exact_only, u_inference_retract).


% annotating U::INFERENCE-RETRACT 
f_u_inference_retract(Ob_Param, Ctxt_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_ctxt, Ctxt_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e),
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
				       #\(~),
				       #\('A')
				     ]),
			    u_ob,
			    u_ctxt
			  ],
			  Roman_nl_Ret),
	f_u_relative_c62_absolute(Ob_Param, Belief_path_Param, C62_absolute_Ret),
	f_u_cx_c36_retrieve(Ctxt_Param, C62_absolute_Ret, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	f_u_null_c63(u_found, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(n),
					   #\(f),
					   #\(e),
					   #\(r),
					   #\(e),
					   #\(n),
					   #\(c),
					   #\(e),
					   #\(-),
					   #\(r),
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
					   #\('A')
					 ]),
				u_ob,
				u_ctxt
			      ],
			      TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_found, Found_Get),
	    cl_car(Found_Get, Car_Param),
	    cl_car(Car_Param, Leaf_causes_Param),
	    f_u_get_leaf_causes(Leaf_causes_Param, Ctxt_Param, Leafs_Init),
	    LEnv24=[[bv(u_leafs, Leafs_Init)]|LEnv],
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('L'),
					   #\(e),
					   #\(a),
					   #\(f),
					   #\(s),
					   #\(' '),
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_leafs
			      ],
			      Roman_nl_Ret39),
	    f_u_yloop(
		      [ [u_initial, [u_found_c63, []]],
			[u_yfor, u_leaf, u_in, u_leafs],
			
			[ u_ydo,
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_leaf, [quote, u_action]],
			    
			    [ progn,
			      [u_retract_dependencies, u_leaf, u_ctxt],
			      [setq, u_found_c63, t]
			    ]
			  ]
			],
			
			[ u_yresult,
			  
			  [ if,
			    [u_null_c63, u_found_c63],
			    [u_cx_c36_retract, u_ctxt, [car, [car, u_found]]]
			  ]
			]
		      ],
		      Yloop_Ret),
	    FnResult=Yloop_Ret
	).
:- set_opv(f_u_inference_retract, classof, claz_function),
   set_opv(u_inference_retract, compile_as, kw_function),
   set_opv(u_inference_retract, function, f_u_inference_retract),
   DefunResult=u_inference_retract.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:18737 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Alternatively, could maintain 'invalidated' justifications for",
				     1,
				     19589)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:18737 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" beliefs.", 1, 19654)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:19664 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'retract-dependencies',
			    [ob, ctxt],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Retracting dependencies of ~A in ~A"),
			      ob,
			      ctxt
			    ],
			    
			    [ if,
			      
			      [ and,
				[not, ['ty$instance?', ob, [quote, believe]]],
				[not, ['ty$instance?', ob, [quote, goal]]]
			      ],
			      
			      [ progn,
				
				[ yloop,
				  [initial, [dependee, []]],
				  
				  [ yfor,
				    'd-link1',
				    in,
				    ['get-links', ob, '*dependency-ob*', ctxt]
				  ],
				  
				  [ ydo,
				    
				    [ setq,
				      dependee,
				      ['ob$get', 'd-link1', [quote, 'linked-to']]
				    ],
				    
				    [ yloop,
				      
				      [ yfor,
					'd-link2',
					in,
					
					[ 'get-links-from',
					  dependee,
					  '*dependency-ob*',
					  ctxt
					]
				      ],
				      [ydo, ['cx$retract', ctxt, 'd-link2']]
				    ],
				    ['retract-dependencies', dependee, ctxt]
				  ]
				],
				['cx$retract', ctxt, ob]
			      ],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Bottoms out at ~A"),
				  ob
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::RETRACT-DEPENDENCIES 
wl: lambda_def(defun,
	      u_retract_dependencies,
	      f_u_retract_dependencies,
	      [u_ob, u_ctxt],
	      
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
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(d),
			     #\(e),
			     #\(p),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(e),
			     #\(n),
			     #\(c),
			     #\(i),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(o),
			     #\(f),
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
		  u_ctxt
		],
		
		[ if,
		  
		  [ and,
		    [not, [u_ty_c36_instance_c63, u_ob, [quote, u_believe]]],
		    [not, [u_ty_c36_instance_c63, u_ob, [quote, u_goal]]]
		  ],
		  
		  [ progn,
		    
		    [ u_yloop,
		      [u_initial, [u_dependee, []]],
		      
		      [ u_yfor,
			u_d_link1,
			u_in,
			[u_get_links, u_ob, u_xx_dependency_ob_xx, u_ctxt]
		      ],
		      
		      [ u_ydo,
			
			[ setq,
			  u_dependee,
			  [u_ob_c36_get, u_d_link1, [quote, u_linked_to]]
			],
			
			[ u_yloop,
			  
			  [ u_yfor,
			    u_d_link2,
			    u_in,
			    
			    [ u_get_links_from,
			      u_dependee,
			      u_xx_dependency_ob_xx,
			      u_ctxt
			    ]
			  ],
			  [u_ydo, [u_cx_c36_retract, u_ctxt, u_d_link2]]
			],
			[u_retract_dependencies, u_dependee, u_ctxt]
		      ]
		    ],
		    [u_cx_c36_retract, u_ctxt, u_ob]
		  ],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('B'),
				 #\(o),
				 #\(t),
				 #\(t),
				 #\(o),
				 #\(m),
				 #\(s),
				 #\(' '),
				 #\(o),
				 #\(u),
				 #\(t),
				 #\(' '),
				 #\(a),
				 #\(t),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_ob
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::RETRACT-DEPENDENCIES 
wl: arglist_info(u_retract_dependencies,
		[u_ob, u_ctxt],
		[Ob_Param, Ctxt_Param],
		arginfo{ all:[u_ob, u_ctxt],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_ctxt],
			 opt:0,
			 req:[u_ob, u_ctxt],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RETRACT-DEPENDENCIES 
wl: init_args(exact_only, u_retract_dependencies).


% annotating U::RETRACT-DEPENDENCIES 
f_u_retract_dependencies(Ob_Param, Ctxt_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_ctxt, Ctxt_Param)],
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
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(d),
				       #\(e),
				       #\(p),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(i),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(o),
				       #\(f),
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
			    u_ctxt
			  ],
			  Roman_nl_Ret),
	f_u_ty_c36_instance_c63(Ob_Param, u_believe, PredArgResult),
	(   PredArgResult==[]
	->  f_u_ty_c36_instance_c63(Ob_Param, u_goal, Goal),
	    cl_not(Goal, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_initial, [u_dependee, []]],
			
			[ u_yfor,
			  u_d_link1,
			  u_in,
			  [u_get_links, u_ob, u_xx_dependency_ob_xx, u_ctxt]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_dependee,
			    [u_ob_c36_get, u_d_link1, [quote, u_linked_to]]
			  ],
			  
			  [ u_yloop,
			    
			    [ u_yfor,
			      u_d_link2,
			      u_in,
			      
			      [ u_get_links_from,
				u_dependee,
				u_xx_dependency_ob_xx,
				u_ctxt
			      ]
			    ],
			    [u_ydo, [u_cx_c36_retract, u_ctxt, u_d_link2]]
			  ],
			  [u_retract_dependencies, u_dependee, u_ctxt]
			]
		      ],
		      Yloop_Ret),
	    f_u_cx_c36_retract(Ctxt_Param, Ob_Param, TrueResult24),
	    FnResult=TrueResult24
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('B'),
					   #\(o),
					   #\(t),
					   #\(t),
					   #\(o),
					   #\(m),
					   #\(s),
					   #\(' '),
					   #\(o),
					   #\(u),
					   #\(t),
					   #\(' '),
					   #\(a),
					   #\(t),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_ob
			      ],
			      Roman_nl_Ret30),
	    FnResult=[]
	).
:- set_opv(f_u_retract_dependencies, classof, claz_function),
   set_opv(u_retract_dependencies, compile_as, kw_function),
   set_opv(u_retract_dependencies, function, f_u_retract_dependencies),
   DefunResult=u_retract_dependencies.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:19664 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For the case of an ACTIVE-GOAL whose object has a UPROC strength.",
				     1,
				     20371)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:20438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instan-strength',
			    [ob, bd],
			    
			    [ if,
			      
			      [ and,
				['ty$instance?', ob, [quote, 'active-goal']],
				
				[ 'ob?',
				  
				  [ 'ob$get',
				    ['ob$get', ob, [quote, obj]],
				    [quote, strength]
				  ]
				]
			      ],
			      
			      [ let,
				
				[ 
				  [ 'strength-val',
				    
				    [ 'ob$get',
				      ['ob$get', ob, [quote, obj]],
				      [quote, strength]
				    ]
				  ],
				  [result, ['ob$instantiate-o', ob, bd]]
				],
				
				[ 'set-strength',
				  ['ob$get', result, [quote, obj]],
				  'strength-val'
				],
				result
			      ],
			      ['ob$instantiate-o', ob, bd]
			    ]
			  ]).

% annotating U::OB$INSTAN-STRENGTH 
wl: lambda_def(defun,
	      u_ob_c36_instan_strength,
	      f_u_ob_c36_instan_strength,
	      [u_ob, u_bd],
	      
	      [ 
		[ if,
		  
		  [ and,
		    [u_ty_c36_instance_c63, u_ob, [quote, u_active_goal]],
		    
		    [ u_ob_c63,
		      
		      [ u_ob_c36_get,
			[u_ob_c36_get, u_ob, [quote, u_obj]],
			[quote, u_strength]
		      ]
		    ]
		  ],
		  
		  [ let,
		    
		    [ 
		      [ u_strength_val,
			
			[ u_ob_c36_get,
			  [u_ob_c36_get, u_ob, [quote, u_obj]],
			  [quote, u_strength]
			]
		      ],
		      [u_result, [u_ob_c36_instantiate_o, u_ob, u_bd]]
		    ],
		    
		    [ u_set_strength,
		      [u_ob_c36_get, u_result, [quote, u_obj]],
		      u_strength_val
		    ],
		    u_result
		  ],
		  [u_ob_c36_instantiate_o, u_ob, u_bd]
		]
	      ]).


% annotating U::OB$INSTAN-STRENGTH 
wl: arglist_info(u_ob_c36_instan_strength,
		[u_ob, u_bd],
		[Ob_Param, Bd_Param],
		arginfo{ all:[u_ob, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_bd],
			 opt:0,
			 req:[u_ob, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTAN-STRENGTH 
wl: init_args(exact_only, u_ob_c36_instan_strength).


% annotating U::OB$INSTAN-STRENGTH 
f_u_ob_c36_instan_strength(Ob_Param, Bd_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_bd, Bd_Param)],
	f_u_ty_c36_instance_c63(Ob_Param, u_active_goal, IFTEST16),
	(   IFTEST16\==[]
	->  f_u_ob_c63(
		       [ u_ob_c36_get,
			 [u_ob_c36_get, u_ob, [quote, u_obj]],
			 [quote, u_strength]
		       ],
		       TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Ob_Param, u_obj, Obj),
	    f_u_ob_c36_get(Obj, u_strength, Strength_val_Init),
	    f_u_ob_c36_instantiate_o(Ob_Param, Bd_Param, Result_Init),
	    LEnv=[[bv(u_strength_val, Strength_val_Init), bv(u_result, Result_Init)]|Env],
	    f_u_set_strength([u_ob_c36_get, u_result, [quote, u_obj]],
			     u_strength_val,
			     Strength_val),
	    get_var(LEnv, u_result, Result_Get),
	    FnResult=Result_Get
	;   f_u_ob_c36_instantiate_o(Ob_Param, Bd_Param, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ob_c36_instan_strength, classof, claz_function),
   set_opv(u_ob_c36_instan_strength, compile_as, kw_function),
   set_opv(u_ob_c36_instan_strength, function, f_u_ob_c36_instan_strength),
   DefunResult=u_ob_c36_instan_strength.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:20438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note that NOTs are not asserted in the context, so dependencies",
				     1,
				     20792)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:20438 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" point to NOTs not in the context",
				     1,
				     20858)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:20892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inference-rcode',
			    [assertion, 'show-result'],
			    
			    [ let,
			      [['old-ob-bindings', '*ob-bindings*']],
			      [setq, '*ob-bindings*', [car, 'show-result']],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Executing coded inference")
			      ],
			      [eval, ['ob$get', assertion, [quote, obj]]],
			      [setq, '*ob-bindings*', 'old-ob-bindings']
			    ]
			  ]).

% annotating U::INFERENCE-RCODE 
wl: lambda_def(defun,
	      u_inference_rcode,
	      f_u_inference_rcode,
	      [u_assertion, u_show_result],
	      
	      [ 
		[ let,
		  [[u_old_ob_bindings, u_xx_ob_bindings_xx]],
		  [setq, u_xx_ob_bindings_xx, [car, u_show_result]],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('E'),
			       #\(x),
			       #\(e),
			       #\(c),
			       #\(u),
			       #\(t),
			       #\(i),
			       #\(n),
			       #\(g),
			       #\(' '),
			       #\(c),
			       #\(o),
			       #\(d),
			       #\(e),
			       #\(d),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(f),
			       #\(e),
			       #\(r),
			       #\(e),
			       #\(n),
			       #\(c),
			       #\(e)
			     ])
		  ],
		  [eval, [u_ob_c36_get, u_assertion, [quote, u_obj]]],
		  [setq, u_xx_ob_bindings_xx, u_old_ob_bindings]
		]
	      ]).


% annotating U::INFERENCE-RCODE 
wl: arglist_info(u_inference_rcode,
		[u_assertion, u_show_result],
		[Assertion_Param, Show_result_Param],
		arginfo{ all:[u_assertion, u_show_result],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_assertion, u_show_result],
			 opt:0,
			 req:[u_assertion, u_show_result],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFERENCE-RCODE 
wl: init_args(exact_only, u_inference_rcode).


% annotating U::INFERENCE-RCODE 
f_u_inference_rcode(Assertion_Param, Show_result_Param, FnResult) :-
	Env=[bv(u_assertion, Assertion_Param), bv(u_show_result, Show_result_Param)],
	get_var(Env, u_xx_ob_bindings_xx, Xx_ob_bindings_xx_Get),
	LEnv=[[bv(u_old_ob_bindings, Xx_ob_bindings_xx_Get)]|Env],
	cl_car(Show_result_Param, Xx_ob_bindings_xx),
	set_var(LEnv, u_xx_ob_bindings_xx, Xx_ob_bindings_xx),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('E'),
				       #\(x),
				       #\(e),
				       #\(c),
				       #\(u),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(d),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e)
				     ])
			  ],
			  Roman_nl_Ret),
	cl_eval([u_ob_c36_get, u_assertion, [quote, u_obj]], Eval_Ret),
	get_var(LEnv, u_old_ob_bindings, Old_ob_bindings_Get),
	set_var(LEnv, u_xx_ob_bindings_xx, Old_ob_bindings_Get),
	LetResult=Old_ob_bindings_Get,
	LetResult=FnResult.
:- set_opv(f_u_inference_rcode, classof, claz_function),
   set_opv(u_inference_rcode, compile_as, kw_function),
   set_opv(u_inference_rcode, function, f_u_inference_rcode),
   DefunResult=u_inference_rcode.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:20892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" actually, code should work for inferences and sprouters both.",
				     8,
				     21101)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:20892 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" They should both take a context arg.",
				     8,
				     21172)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21295 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'empty-bd-in',
			    [retrieved],
			    
			    [ any,
			      [lambda, [x], [if, ['empty-bd?', x], x, []]],
			      retrieved
			    ]
			  ]).

% annotating U::EMPTY-BD-IN 
wl: lambda_def(defun,
	      u_empty_bd_in,
	      f_u_empty_bd_in,
	      [u_retrieved],
	      
	      [ 
		[ u_any,
		  [lambda, [u_x], [if, [u_empty_bd_c63, u_x], u_x, []]],
		  u_retrieved
		]
	      ]).


% annotating U::EMPTY-BD-IN 
wl: arglist_info(u_empty_bd_in,
		[u_retrieved],
		[Retrieved_Param],
		arginfo{ all:[u_retrieved],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_retrieved],
			 opt:0,
			 req:[u_retrieved],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EMPTY-BD-IN 
wl: init_args(exact_only, u_empty_bd_in).


% annotating U::EMPTY-BD-IN 
f_u_empty_bd_in(Retrieved_Param, FnResult) :-
	Env=[bv(u_retrieved, Retrieved_Param)],
	f_u_any([lambda, [u_x], [if, [u_empty_bd_c63, u_x], u_x, []]],
		u_retrieved,
		Retrieved),
	Retrieved=FnResult.
:- set_opv(f_u_empty_bd_in, classof, claz_function),
   set_opv(u_empty_bd_in, compile_as, kw_function),
   set_opv(u_empty_bd_in, function, f_u_empty_bd_in),
   DefunResult=u_empty_bd_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21384 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*inf-fire-thresh*', 0.8]).
:- set_var(TLEnv3, setq, u_xx_inf_fire_thresh_xx, 0.8).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21384 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: there must be a better way",
				     30,
				     21414)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21384 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" of deciding when a new instantiation of an inference should",
				     1,
				     21449)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21384 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" be allowed to fire.", 1, 21511)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21384 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Note, if another inference sums into an existing assertion,",
				     1,
				     21534)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21384 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" that inference will fire over and over again, because no note",
				     1,
				     21602)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21384 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" is made that that fact was also inferred from that inference.",
				     1,
				     21666)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inference-assert',
			    
			    [ assertion,
			      ctxt,
			      'top-level-goal',
			      'belief-path',
			      rule,
			      'show-result',
			      'fired?'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      inference,
			      '$STRING'("Inference assert")
			    ],
			    
			    [ let,
			      [[temp, []]],
			      
			      [ cond,
				
				[ 
				  [ and,
				    
				    [ or,
				      ['null?', 'top-level-goal'],
				      ['dd-goal?', assertion],
				      
				      [ 'eq?',
					
					[ 'ob$get',
					  'top-level-goal',
					  [quote, 'planning-type']
					],
					[quote, real]
				      ]
				    ],
				    ['me-belief-path?', 'belief-path'],
				    
				    [ 'ty$instance?',
				      assertion,
				      [quote, 'active-goal']
				    ],
				    
				    [ not,
				      
				      [ 'ty$instance?',
					['ob$get', assertion, [quote, obj]],
					[quote, preservation]
				      ]
				    ]
				  ],
				  
				  [ if,
				    [not, ['top-level-goal-exists?', assertion]],
				    
				    [ progn,
				      
				      [ 'possible-fired-msg',
					'fired?',
					rule,
					ctxt,
					'show-result',
					'belief-path'
				      ],
				      
				      [ 'ob$set',
					assertion,
					[quote, 'inference-rule'],
					rule
				      ],
				      
				      [ 'activate-top-level-goal',
					assertion,
					ctxt,
					[car, 'show-result'],
					rule
				      ],
				      t
				    ],
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					inference,
					'$STRING'("Ignoring duplicate tlg inference")
				      ],
				      []
				    ]
				  ]
				],
				
				[ 
				  [ setq,
				    temp,
				    
				    [ 'empty-bd-in',
				      
				      [ 'cx$retrieve-relative',
					ctxt,
					assertion,
					'belief-path'
				      ]
				    ]
				  ],
				  [setq, assertion, [car, temp]],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    inference,
				    '$STRING'("Retrieved ~A"),
				    assertion
				  ],
				  
				  [ if,
				    
				    [ 'fl<',
				      [strength, assertion],
				      '*inf-fire-thresh*'
				    ],
				    
				    [ progn,
				      
				      [ 'possible-fired-msg',
					'fired?',
					rule,
					ctxt,
					'show-result',
					'belief-path'
				      ],
				      
				      [ 'ob$add',
					assertion,
					[quote, 'inference-rule'],
					rule
				      ],
				      
				      [ 'make-dependency',
					[cdr, 'show-result'],
					assertion,
					rule,
					ctxt,
					'belief-path',
					['ob$get', rule, [quote, plausibility]],
					[car, 'show-result']
				      ],
				      t
				    ],
				    []
				  ]
				],
				
				[ 
				  [ and,
				    ['ty$instance?', assertion, [quote, believe]],
				    
				    [ 'not-me?',
				      ['ob$get', assertion, [quote, actor]]
				    ],
				    
				    [ 'ty$instance?',
				      ['ob$get', assertion, [quote, obj]],
				      [quote, 'active-goal']
				    ]
				  ],
				  
				  [ if,
				    
				    [ not,
				      
				      [ 'other-goal-exists?',
					['ob$get', assertion, [quote, actor]],
					
					[ 'ob$pget',
					  assertion,
					  [quote, [obj, obj]]
					],
					ctxt
				      ]
				    ],
				    
				    [ progn,
				      
				      [ 'possible-fired-msg',
					'fired?',
					rule,
					ctxt,
					'show-result',
					'belief-path'
				      ],
				      
				      [ 'ob$set',
					assertion,
					[quote, 'inference-rule'],
					rule
				      ],
				      
				      [ 'activate-other-top-goal',
					['ob$get', assertion, [quote, obj]],
					'top-level-goal',
					ctxt,
					
					[ '->belief-path',
					  ['ob$get', assertion, [quote, actor]]
					]
				      ],
				      
				      [ 'make-dependency',
					[cdr, 'show-result'],
					assertion,
					rule,
					ctxt,
					'belief-path',
					['ob$get', rule, [quote, plausibility]],
					[car, 'show-result']
				      ],
				      t
				    ],
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					inference,
					'$STRING'("Ignoring duplicate other goal inference")
				      ],
				      []
				    ]
				  ]
				],
				
				[ ['ty$instance?', assertion, [quote, goal]],
				  
				  [ if,
				    [not, ['goal-exists?', assertion, ctxt]],
				    
				    [ progn,
				      
				      [ 'possible-fired-msg',
					'fired?',
					rule,
					ctxt,
					'show-result',
					'belief-path'
				      ],
				      
				      [ 'ob$set',
					assertion,
					[quote, 'inference-rule'],
					rule
				      ],
				      
				      [ 'ob$add',
					assertion,
					[quote, 'top-level-goal'],
					'top-level-goal'
				      ],
				      
				      [ 'cx$assert-relative',
					ctxt,
					assertion,
					'belief-path'
				      ],
				      
				      [ 'make-dependency',
					[cdr, 'show-result'],
					assertion,
					rule,
					ctxt,
					'belief-path',
					['ob$get', rule, [quote, plausibility]],
					[car, 'show-result']
				      ],
				      
				      [ if,
					
					[ and,
					  ['me-belief-path?', 'belief-path'],
					  
					  [ or,
					    
					    [ 'ty$instance?',
					      assertion,
					      [quote, 'succeeded-goal']
					    ],
					    
					    [ 'ty$instance?',
					      assertion,
					      [quote, 'failed-goal']
					    ]
					  ],
					  ['personal-goal?', assertion]
					],
					
					[ 'personal-goal-outcome',
					  assertion,
					  ctxt,
					  'top-level-goal'
					]
				      ],
				      t
				    ],
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					inference,
					'$STRING'("Ignoring duplicate goal inference")
				      ],
				      []
				    ]
				  ]
				],
				
				[ else,
				  
				  [ 'possible-fired-msg',
				    'fired?',
				    rule,
				    ctxt,
				    'show-result',
				    'belief-path'
				  ],
				  
				  [ 'ob$set',
				    assertion,
				    [quote, 'inference-rule'],
				    rule
				  ],
				  
				  [ if,
				    ['ob$get', rule, [quote, 'inf-no-gen']],
				    
				    [ 'no-gen',
				      
				      [ 'cx$assert-relative',
					ctxt,
					assertion,
					'belief-path'
				      ]
				    ],
				    
				    [ 'cx$assert-relative',
				      ctxt,
				      assertion,
				      'belief-path'
				    ]
				  ],
				  
				  [ 'make-dependency',
				    [cdr, 'show-result'],
				    assertion,
				    rule,
				    ctxt,
				    'belief-path',
				    ['ob$get', rule, [quote, plausibility]],
				    [car, 'show-result']
				  ],
				  t
				]
			      ]
			    ]
			  ]).

% annotating U::INFERENCE-ASSERT 
wl: lambda_def(defun,
	      u_inference_assert,
	      f_u_inference_assert,
	      
	      [ u_assertion,
		u_ctxt,
		u_top_level_goal,
		u_belief_path,
		u_rule,
		u_show_result,
		u_fired_c63
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_inference,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('I'),
			     #\(n),
			     #\(f),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(c),
			     #\(e),
			     #\(' '),
			     #\(a),
			     #\(s),
			     #\(s),
			     #\(e),
			     #\(r),
			     #\(t)
			   ])
		],
		
		[ let,
		  [[u_temp, []]],
		  
		  [ cond,
		    
		    [ 
		      [ and,
			
			[ or,
			  [u_null_c63, u_top_level_goal],
			  [u_dd_goal_c63, u_assertion],
			  
			  [ u_eq_c63,
			    
			    [ u_ob_c36_get,
			      u_top_level_goal,
			      [quote, u_planning_type]
			    ],
			    [quote, real]
			  ]
			],
			[u_me_belief_path_c63, u_belief_path],
			
			[ u_ty_c36_instance_c63,
			  u_assertion,
			  [quote, u_active_goal]
			],
			
			[ not,
			  
			  [ u_ty_c36_instance_c63,
			    [u_ob_c36_get, u_assertion, [quote, u_obj]],
			    [quote, u_preservation]
			  ]
			]
		      ],
		      
		      [ if,
			[not, [u_top_level_goal_exists_c63, u_assertion]],
			
			[ progn,
			  
			  [ u_possible_fired_msg,
			    u_fired_c63,
			    u_rule,
			    u_ctxt,
			    u_show_result,
			    u_belief_path
			  ],
			  
			  [ u_ob_c36_set,
			    u_assertion,
			    [quote, u_inference_rule],
			    u_rule
			  ],
			  
			  [ u_activate_top_level_goal,
			    u_assertion,
			    u_ctxt,
			    [car, u_show_result],
			    u_rule
			  ],
			  t
			],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_inference,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(g),
				       #\(n),
				       #\(o),
				       #\(r),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(d),
				       #\(u),
				       #\(p),
				       #\(l),
				       #\(i),
				       #\(c),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(' '),
				       #\(t),
				       #\(l),
				       #\(g),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e)
				     ])
			  ],
			  []
			]
		      ]
		    ],
		    
		    [ 
		      [ setq,
			u_temp,
			
			[ u_empty_bd_in,
			  
			  [ u_cx_c36_retrieve_relative,
			    u_ctxt,
			    u_assertion,
			    u_belief_path
			  ]
			]
		      ],
		      [setq, u_assertion, [car, u_temp]],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_inference,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('R'),
				   #\(e),
				   #\(t),
				   #\(r),
				   #\(i),
				   #\(e),
				   #\(v),
				   #\(e),
				   #\(d),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_assertion
		      ],
		      
		      [ if,
			
			[ u_fl_c60,
			  [u_strength, u_assertion],
			  u_xx_inf_fire_thresh_xx
			],
			
			[ progn,
			  
			  [ u_possible_fired_msg,
			    u_fired_c63,
			    u_rule,
			    u_ctxt,
			    u_show_result,
			    u_belief_path
			  ],
			  
			  [ u_ob_c36_add,
			    u_assertion,
			    [quote, u_inference_rule],
			    u_rule
			  ],
			  
			  [ u_make_dependency,
			    [cdr, u_show_result],
			    u_assertion,
			    u_rule,
			    u_ctxt,
			    u_belief_path,
			    [u_ob_c36_get, u_rule, [quote, u_plausibility]],
			    [car, u_show_result]
			  ],
			  t
			],
			[]
		      ]
		    ],
		    
		    [ 
		      [ and,
			[u_ty_c36_instance_c63, u_assertion, [quote, u_believe]],
			
			[ u_not_me_c63,
			  [u_ob_c36_get, u_assertion, [quote, u_actor]]
			],
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_assertion, [quote, u_obj]],
			  [quote, u_active_goal]
			]
		      ],
		      
		      [ if,
			
			[ not,
			  
			  [ u_other_goal_exists_c63,
			    [u_ob_c36_get, u_assertion, [quote, u_actor]],
			    [u_ob_c36_pget, u_assertion, [quote, [u_obj, u_obj]]],
			    u_ctxt
			  ]
			],
			
			[ progn,
			  
			  [ u_possible_fired_msg,
			    u_fired_c63,
			    u_rule,
			    u_ctxt,
			    u_show_result,
			    u_belief_path
			  ],
			  
			  [ u_ob_c36_set,
			    u_assertion,
			    [quote, u_inference_rule],
			    u_rule
			  ],
			  
			  [ u_activate_other_top_goal,
			    [u_ob_c36_get, u_assertion, [quote, u_obj]],
			    u_top_level_goal,
			    u_ctxt,
			    
			    [ u_c62_belief_path,
			      [u_ob_c36_get, u_assertion, [quote, u_actor]]
			    ]
			  ],
			  
			  [ u_make_dependency,
			    [cdr, u_show_result],
			    u_assertion,
			    u_rule,
			    u_ctxt,
			    u_belief_path,
			    [u_ob_c36_get, u_rule, [quote, u_plausibility]],
			    [car, u_show_result]
			  ],
			  t
			],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_inference,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(g),
				       #\(n),
				       #\(o),
				       #\(r),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(d),
				       #\(u),
				       #\(p),
				       #\(l),
				       #\(i),
				       #\(c),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(' '),
				       #\(o),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e)
				     ])
			  ],
			  []
			]
		      ]
		    ],
		    
		    [ [u_ty_c36_instance_c63, u_assertion, [quote, u_goal]],
		      
		      [ if,
			[not, [u_goal_exists_c63, u_assertion, u_ctxt]],
			
			[ progn,
			  
			  [ u_possible_fired_msg,
			    u_fired_c63,
			    u_rule,
			    u_ctxt,
			    u_show_result,
			    u_belief_path
			  ],
			  
			  [ u_ob_c36_set,
			    u_assertion,
			    [quote, u_inference_rule],
			    u_rule
			  ],
			  
			  [ u_ob_c36_add,
			    u_assertion,
			    [quote, u_top_level_goal],
			    u_top_level_goal
			  ],
			  
			  [ u_cx_c36_assert_relative,
			    u_ctxt,
			    u_assertion,
			    u_belief_path
			  ],
			  
			  [ u_make_dependency,
			    [cdr, u_show_result],
			    u_assertion,
			    u_rule,
			    u_ctxt,
			    u_belief_path,
			    [u_ob_c36_get, u_rule, [quote, u_plausibility]],
			    [car, u_show_result]
			  ],
			  
			  [ if,
			    
			    [ and,
			      [u_me_belief_path_c63, u_belief_path],
			      
			      [ or,
				
				[ u_ty_c36_instance_c63,
				  u_assertion,
				  [quote, u_succeeded_goal]
				],
				
				[ u_ty_c36_instance_c63,
				  u_assertion,
				  [quote, u_failed_goal]
				]
			      ],
			      [u_personal_goal_c63, u_assertion]
			    ],
			    
			    [ u_personal_goal_outcome,
			      u_assertion,
			      u_ctxt,
			      u_top_level_goal
			    ]
			  ],
			  t
			],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_inference,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(g),
				       #\(n),
				       #\(o),
				       #\(r),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(d),
				       #\(u),
				       #\(p),
				       #\(l),
				       #\(i),
				       #\(c),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(' '),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e)
				     ])
			  ],
			  []
			]
		      ]
		    ],
		    
		    [ u_else,
		      
		      [ u_possible_fired_msg,
			u_fired_c63,
			u_rule,
			u_ctxt,
			u_show_result,
			u_belief_path
		      ],
		      
		      [ u_ob_c36_set,
			u_assertion,
			[quote, u_inference_rule],
			u_rule
		      ],
		      
		      [ if,
			[u_ob_c36_get, u_rule, [quote, u_inf_no_gen]],
			
			[ u_no_gen,
			  
			  [ u_cx_c36_assert_relative,
			    u_ctxt,
			    u_assertion,
			    u_belief_path
			  ]
			],
			
			[ u_cx_c36_assert_relative,
			  u_ctxt,
			  u_assertion,
			  u_belief_path
			]
		      ],
		      
		      [ u_make_dependency,
			[cdr, u_show_result],
			u_assertion,
			u_rule,
			u_ctxt,
			u_belief_path,
			[u_ob_c36_get, u_rule, [quote, u_plausibility]],
			[car, u_show_result]
		      ],
		      t
		    ]
		  ]
		]
	      ]).


% annotating U::INFERENCE-ASSERT 
wl: arglist_info(u_inference_assert,
		
		[ u_assertion,
		  u_ctxt,
		  u_top_level_goal,
		  u_belief_path,
		  u_rule,
		  u_show_result,
		  u_fired_c63
		],
		
		[ Assertion_Param,
		  Ctxt_Param,
		  Top_level_goal_Param,
		  Belief_path_Param,
		  Rule_Param,
		  Show_result_Param,
		  Fired_c63_Param
		],
		arginfo{ all:
			     [ u_assertion,
			       u_ctxt,
			       u_top_level_goal,
			       u_belief_path,
			       u_rule,
			       u_show_result,
			       u_fired_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_assertion,
				 u_ctxt,
				 u_top_level_goal,
				 u_belief_path,
				 u_rule,
				 u_show_result,
				 u_fired_c63
			       ],
			 opt:0,
			 req:
			     [ u_assertion,
			       u_ctxt,
			       u_top_level_goal,
			       u_belief_path,
			       u_rule,
			       u_show_result,
			       u_fired_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFERENCE-ASSERT 
wl: init_args(exact_only, u_inference_assert).


% annotating U::INFERENCE-ASSERT 
f_u_inference_assert(Assertion_Param, Ctxt_Param, Top_level_goal_Param, Belief_path_Param, Rule_Param, Show_result_Param, Fired_c63_Param, TrueResult184) :-
	Env=[bv(u_assertion, Assertion_Param), bv(u_ctxt, Ctxt_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_belief_path, Belief_path_Param), bv(u_rule, Rule_Param), bv(u_show_result, Show_result_Param), bv(u_fired_c63, Fired_c63_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_inference,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(' '),
				       #\(a),
				       #\(s),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(t)
				     ])
			  ],
			  Roman_nl_Ret),
	LEnv=[[bv(u_temp, [])]|Env],
	(   f_u_null_c63(u_top_level_goal, FORM1_Res33),
	    FORM1_Res33\==[],
	    IFTEST28=FORM1_Res33
	->  true
	;   get_var(LEnv, u_assertion, Assertion_Get),
	    f_u_dd_goal_c63(Assertion_Get, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST28=FORM1_Res
	->  true
	;   f_u_eq_c63([u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		       [quote, real],
		       Eq_c63_Ret),
	    IFTEST28=Eq_c63_Ret
	),
	(   IFTEST28\==[]
	->  f_u_me_belief_path_c63(u_belief_path, IFTEST34),
	    (   IFTEST34\==[]
	    ->  get_var(LEnv, u_assertion, Assertion_Get38),
		f_u_ty_c36_instance_c63(Assertion_Get38,
					u_active_goal,
					IFTEST36),
		(   IFTEST36\==[]
		->  get_var(LEnv, u_assertion, Assertion_Get39),
		    f_u_ob_c36_get(Assertion_Get39, u_obj, Obj),
		    f_u_ty_c36_instance_c63(Obj, u_preservation, Preservation),
		    cl_not(Preservation, TrueResult),
		    IFTEST=TrueResult
		;   IFTEST=[]
		)
	    ;   IFTEST=[]
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_top_level_goal_exists_c63(Assertion_Param, PredArgResult),
	    (   PredArgResult==[]
	    ->  f_u_possible_fired_msg(Fired_c63_Param,
				       Rule_Param,
				       Ctxt_Param,
				       Show_result_Param,
				       Belief_path_Param,
				       Fired_msg_Ret),
		f_u_ob_c36_set(Assertion_Param,
			       u_inference_rule,
			       Rule_Param,
			       C36_set_Ret),
		cl_car(Show_result_Param, Car_Ret),
		f_u_activate_top_level_goal(Assertion_Param,
					    Ctxt_Param,
					    Car_Ret,
					    Rule_Param,
					    Level_goal_Ret),
		TrueResult184=t
	    ;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_inference,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('I'),
					       #\(g),
					       #\(n),
					       #\(o),
					       #\(r),
					       #\(i),
					       #\(n),
					       #\(g),
					       #\(' '),
					       #\(d),
					       #\(u),
					       #\(p),
					       #\(l),
					       #\(i),
					       #\(c),
					       #\(a),
					       #\(t),
					       #\(e),
					       #\(' '),
					       #\(t),
					       #\(l),
					       #\(g),
					       #\(' '),
					       #\(i),
					       #\(n),
					       #\(f),
					       #\(e),
					       #\(r),
					       #\(e),
					       #\(n),
					       #\(c),
					       #\(e)
					     ])
				  ],
				  Roman_nl_Ret218),
		TrueResult184=[]
	    )
	;   get_var(LEnv, u_assertion, Assertion_Get61),
	    f_u_cx_c36_retrieve_relative(Ctxt_Param,
					 Assertion_Get61,
					 Belief_path_Param,
					 Bd_in_Param),
	    f_u_empty_bd_in(Bd_in_Param, IFTEST58),
	    set_var(LEnv, u_temp, IFTEST58),
	    (   IFTEST58\==[]
	    ->  get_var(LEnv, u_temp, Temp_Get),
		cl_car(Temp_Get, Assertion),
		set_var(LEnv, u_assertion, Assertion),
		f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_inference,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('R'),
					       #\(e),
					       #\(t),
					       #\(r),
					       #\(i),
					       #\(e),
					       #\(v),
					       #\(e),
					       #\(d),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_assertion
				  ],
				  Roman_nl_Ret219),
		f_u_fl_c60([u_strength, u_assertion],
			   u_xx_inf_fire_thresh_xx,
			   IFTEST64),
		(   IFTEST64\==[]
		->  f_u_possible_fired_msg(Fired_c63_Param,
					   Rule_Param,
					   Ctxt_Param,
					   Show_result_Param,
					   Belief_path_Param,
					   Fired_msg_Ret220),
		    get_var(LEnv, u_assertion, Assertion_Get71),
		    f_u_ob_c36_add(Assertion_Get71,
				   u_inference_rule,
				   Rule_Param,
				   C36_add_Ret),
		    cl_cdr(Show_result_Param, Make_dependency_Param),
		    get_var(LEnv, u_assertion, Assertion_Get74),
		    f_u_ob_c36_get(Rule_Param, u_plausibility, Plausibility),
		    cl_car(Show_result_Param, Car_Ret222),
		    f_u_make_dependency(Make_dependency_Param,
					Assertion_Get74,
					Rule_Param,
					Ctxt_Param,
					Belief_path_Param,
					Plausibility,
					Car_Ret222,
					Make_dependency_Ret),
		    TrueResult184=t
		;   TrueResult184=[]
		)
	    ;   get_var(LEnv, u_assertion, Assertion_Get84),
		f_u_ty_c36_instance_c63(Assertion_Get84, u_believe, IFTEST82),
		(   IFTEST82\==[]
		->  get_var(LEnv, u_assertion, Assertion_Get87),
		    f_u_ob_c36_get(Assertion_Get87, u_actor, Actor),
		    f_u_not_me_c63(Actor, IFTEST85),
		    (   IFTEST85\==[]
		    ->  get_var(LEnv, u_assertion, Assertion_Get88),
			f_u_ob_c36_get(Assertion_Get88, u_obj, Obj199),
			f_u_ty_c36_instance_c63(Obj199,
						u_active_goal,
						TrueResult89),
			IFTEST80=TrueResult89
		    ;   IFTEST80=[]
		    )
		;   IFTEST80=[]
		),
		(   IFTEST80\==[]
		->  get_var(LEnv, u_assertion, Assertion_Get92),
		    f_u_ob_c36_get(Assertion_Get92, u_actor, Actor200),
		    get_var(LEnv, u_assertion, Assertion_Get93),
		    f_u_ob_c36_pget(Assertion_Get93,
				    [u_obj, u_obj],
				    C36_pget_Ret),
		    f_u_other_goal_exists_c63(Actor200,
					      C36_pget_Ret,
					      Ctxt_Param,
					      PredArgResult96),
		    (   PredArgResult96==[]
		    ->  f_u_possible_fired_msg(Fired_c63_Param,
					       Rule_Param,
					       Ctxt_Param,
					       Show_result_Param,
					       Belief_path_Param,
					       Fired_msg_Ret225),
			get_var(LEnv, u_assertion, Assertion_Get102),
			f_u_ob_c36_set(Assertion_Get102,
				       u_inference_rule,
				       Rule_Param,
				       C36_set_Ret226),
			get_var(LEnv, u_assertion, Assertion_Get104),
			f_u_ob_c36_get(Assertion_Get104, u_obj, Obj201),
			get_var(LEnv, u_assertion, Assertion_Get107),
			f_u_ob_c36_get(Assertion_Get107, u_actor, Actor202),
			f_u_c62_belief_path(Actor202, Belief_path_Ret),
			f_u_activate_other_top_goal(Obj201,
						    Top_level_goal_Param,
						    Ctxt_Param,
						    Belief_path_Ret,
						    Top_goal_Ret),
			cl_cdr(Show_result_Param, Make_dependency_Param209),
			get_var(LEnv, u_assertion, Assertion_Get109),
			f_u_ob_c36_get(Rule_Param,
				       u_plausibility,
				       Plausibility203),
			cl_car(Show_result_Param, Car_Ret229),
			f_u_make_dependency(Make_dependency_Param209,
					    Assertion_Get109,
					    Rule_Param,
					    Ctxt_Param,
					    Belief_path_Param,
					    Plausibility203,
					    Car_Ret229,
					    Make_dependency_Ret230),
			TrueResult184=t
		    ;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
					  u_inference,
					  
					  [ '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\('I'),
						       #\(g),
						       #\(n),
						       #\(o),
						       #\(r),
						       #\(i),
						       #\(n),
						       #\(g),
						       #\(' '),
						       #\(d),
						       #\(u),
						       #\(p),
						       #\(l),
						       #\(i),
						       #\(c),
						       #\(a),
						       #\(t),
						       #\(e),
						       #\(' '),
						       #\(o),
						       #\(t),
						       #\(h),
						       #\(e),
						       #\(r),
						       #\(' '),
						       #\(g),
						       #\(o),
						       #\(a),
						       #\(l),
						       #\(' '),
						       #\(i),
						       #\(n),
						       #\(f),
						       #\(e),
						       #\(r),
						       #\(e),
						       #\(n),
						       #\(c),
						       #\(e)
						     ])
					  ],
					  Roman_nl_Ret231),
			TrueResult184=[]
		    )
		;   get_var(LEnv, u_assertion, Assertion_Get117),
		    f_u_ty_c36_instance_c63(Assertion_Get117, u_goal, IFTEST115),
		    (   IFTEST115\==[]
		    ->  get_var(LEnv, u_assertion, Assertion_Get119),
			f_u_goal_exists_c63(Assertion_Get119,
					    Ctxt_Param,
					    PredArgResult122),
			(   PredArgResult122==[]
			->  f_u_possible_fired_msg(Fired_c63_Param,
						   Rule_Param,
						   Ctxt_Param,
						   Show_result_Param,
						   Belief_path_Param,
						   Fired_msg_Ret232),
			    get_var(LEnv, u_assertion, Assertion_Get128),
			    f_u_ob_c36_set(Assertion_Get128,
					   u_inference_rule,
					   Rule_Param,
					   C36_set_Ret233),
			    get_var(LEnv, u_assertion, Assertion_Get130),
			    f_u_ob_c36_add(Assertion_Get130,
					   u_top_level_goal,
					   Top_level_goal_Param,
					   C36_add_Ret234),
			    get_var(LEnv, u_assertion, Assertion_Get133),
			    f_u_cx_c36_assert_relative(Ctxt_Param,
						       Assertion_Get133,
						       Belief_path_Param,
						       Assert_relative_Ret),
			    cl_cdr(Show_result_Param, Make_dependency_Param210),
			    get_var(LEnv, u_assertion, Assertion_Get136),
			    f_u_ob_c36_get(Rule_Param,
					   u_plausibility,
					   Plausibility204),
			    cl_car(Show_result_Param, Car_Ret236),
			    f_u_make_dependency(Make_dependency_Param210,
						Assertion_Get136,
						Rule_Param,
						Ctxt_Param,
						Belief_path_Param,
						Plausibility204,
						Car_Ret236,
						Make_dependency_Ret237),
			    f_u_me_belief_path_c63(u_belief_path, IFTEST144),
			    (   IFTEST144\==[]
			    ->  (   get_var(LEnv, u_assertion, Assertion_Get148),
				    f_u_ty_c36_instance_c63(Assertion_Get148,
							    u_succeeded_goal,
							    FORM1_Res150),
				    FORM1_Res150\==[],
				    IFTEST146=FORM1_Res150
				->  true
				;   get_var(LEnv, u_assertion, Assertion_Get149),
				    f_u_ty_c36_instance_c63(Assertion_Get149,
							    u_failed_goal,
							    Failed_goal),
				    IFTEST146=Failed_goal
				),
				(   IFTEST146\==[]
				->  get_var(LEnv, u_assertion, Assertion_Get151),
				    f_u_personal_goal_c63(Assertion_Get151,
							  TrueResult152),
				    IFTEST142=TrueResult152
				;   IFTEST142=[]
				)
			    ;   IFTEST142=[]
			    ),
			    (   IFTEST142\==[]
			    ->  get_var(LEnv, u_assertion, Assertion_Get154),
				f_u_personal_goal_outcome(Assertion_Get154,
							  Ctxt_Param,
							  Top_level_goal_Param,
							  TrueResult157),
				_131376=TrueResult157
			    ;   _131376=[]
			    ),
			    TrueResult184=t
			;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
					      u_inference,
					      
					      [ '$ARRAY'([*],
							 claz_base_character,
							 
							 [ #\('I'),
							   #\(g),
							   #\(n),
							   #\(o),
							   #\(r),
							   #\(i),
							   #\(n),
							   #\(g),
							   #\(' '),
							   #\(d),
							   #\(u),
							   #\(p),
							   #\(l),
							   #\(i),
							   #\(c),
							   #\(a),
							   #\(t),
							   #\(e),
							   #\(' '),
							   #\(g),
							   #\(o),
							   #\(a),
							   #\(l),
							   #\(' '),
							   #\(i),
							   #\(n),
							   #\(f),
							   #\(e),
							   #\(r),
							   #\(e),
							   #\(n),
							   #\(c),
							   #\(e)
							 ])
					      ],
					      Roman_nl_Ret238),
			    TrueResult184=[]
			)
		    ;   get_var(LEnv, u_else, IFTEST158),
			(   IFTEST158\==[]
			->  f_u_possible_fired_msg(Fired_c63_Param,
						   Rule_Param,
						   Ctxt_Param,
						   Show_result_Param,
						   Belief_path_Param,
						   Fired_msg_Ret239),
			    get_var(LEnv, u_assertion, Assertion_Get166),
			    f_u_ob_c36_set(Assertion_Get166,
					   u_inference_rule,
					   Rule_Param,
					   C36_set_Ret240),
			    f_u_ob_c36_get(Rule_Param, u_inf_no_gen, IFTEST168),
			    (   IFTEST168\==[]
			    ->  f_u_no_gen(
					   [ 
					     [ u_cx_c36_assert_relative,
					       u_ctxt,
					       u_assertion,
					       u_belief_path
					     ]
					   ],
					   TrueResult174),
				_132218=TrueResult174
			    ;   get_var(LEnv, u_assertion, Assertion_Get172),
				f_u_cx_c36_assert_relative(Ctxt_Param,
							   Assertion_Get172,
							   Belief_path_Param,
							   ElseResult),
				_132218=ElseResult
			    ),
			    cl_cdr(Show_result_Param, Make_dependency_Param211),
			    get_var(LEnv, u_assertion, Assertion_Get177),
			    f_u_ob_c36_get(Rule_Param,
					   u_plausibility,
					   Plausibility206),
			    cl_car(Show_result_Param, Car_Ret241),
			    f_u_make_dependency(Make_dependency_Param211,
						Assertion_Get177,
						Rule_Param,
						Ctxt_Param,
						Belief_path_Param,
						Plausibility206,
						Car_Ret241,
						Make_dependency_Ret242),
			    TrueResult184=t
			;   TrueResult184=[]
			)
		    )
		)
	    )
	).
:- set_opv(f_u_inference_assert, classof, claz_function),
   set_opv(u_inference_assert, compile_as, kw_function),
   set_opv(u_inference_assert, function, f_u_inference_assert),
   DefunResult=u_inference_assert.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Activate new top-level self goal",
				     5,
				     21938)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end or", 17, 22112)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" necessary?", 51, 22467)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Other goal activation", 5, 23263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Goal inferences.", 5, 24257)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" as yet, is only a global indication--can't specify to",
				     1,
				     25331)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:21729 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" gen some goals and not others.",
				     1,
				     25387)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25731 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*exists-ignores*',
			    
			    [ quote,
			      
			      [ strength,
				'top-level-goal',
				'linked-from-of',
				'linked-to-of',
				'termination-context'
			      ]
			    ]
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_exists_ignores_xx,
	   
	   [ u_strength,
	     u_top_level_goal,
	     u_linked_from_of,
	     u_linked_to_of,
	     u_termination_context
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'top-level-goal-exists?',
			    [goal],
			    
			    [ mem,
			      
			      [ lambda,
				[x, y],
				
				[ and,
				  ['tlg?', y, '*reality*'],
				  
				  [ 'ob$unify1',
				    ['ob$get', x, [quote, obj]],
				    ['ob$get', y, [quote, obj]],
				    '*empty-bd*',
				    '*exists-ignores*'
				  ],
				  [not, ['consider-as-new-goal?', x, y]]
				]
			      ],
			      goal,
			      
			      [ append,
				
				[ 'cx$get-all-ty',
				  '*reality-lookahead*',
				  '*succeeded-goal-ob*'
				],
				
				[ 'cx$get-all-ty',
				  '*reality-lookahead*',
				  '*failed-goal-ob*'
				],
				
				[ 'cx$get-all-ty',
				  '*reality-lookahead*',
				  '*active-goal-ob*'
				]
			      ]
			    ]
			  ]).

% annotating U::TOP-LEVEL-GOAL-EXISTS? 
wl: lambda_def(defun,
	      u_top_level_goal_exists_c63,
	      f_u_top_level_goal_exists_c63,
	      [u_goal],
	      
	      [ 
		[ u_mem,
		  
		  [ lambda,
		    [u_x, u_y],
		    
		    [ and,
		      [u_tlg_c63, u_y, u_xx_reality_xx],
		      
		      [ u_ob_c36_unify1,
			[u_ob_c36_get, u_x, [quote, u_obj]],
			[u_ob_c36_get, u_y, [quote, u_obj]],
			u_xx_empty_bd_xx,
			u_xx_exists_ignores_xx
		      ],
		      [not, [u_consider_as_new_goal_c63, u_x, u_y]]
		    ]
		  ],
		  u_goal,
		  
		  [ append,
		    
		    [ u_cx_c36_get_all_ty,
		      u_xx_reality_lookahead_xx,
		      u_xx_succeeded_goal_ob_xx
		    ],
		    
		    [ u_cx_c36_get_all_ty,
		      u_xx_reality_lookahead_xx,
		      u_xx_failed_goal_ob_xx
		    ],
		    
		    [ u_cx_c36_get_all_ty,
		      u_xx_reality_lookahead_xx,
		      u_xx_active_goal_ob_xx
		    ]
		  ]
		]
	      ]).


% annotating U::TOP-LEVEL-GOAL-EXISTS? 
wl: arglist_info(u_top_level_goal_exists_c63,
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

% annotating U::TOP-LEVEL-GOAL-EXISTS? 
wl: init_args(exact_only, u_top_level_goal_exists_c63).


% annotating U::TOP-LEVEL-GOAL-EXISTS? 
f_u_top_level_goal_exists_c63(Goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_mem(
		[ lambda,
		  [u_x, u_y],
		  
		  [ and,
		    [u_tlg_c63, u_y, u_xx_reality_xx],
		    
		    [ u_ob_c36_unify1,
		      [u_ob_c36_get, u_x, [quote, u_obj]],
		      [u_ob_c36_get, u_y, [quote, u_obj]],
		      u_xx_empty_bd_xx,
		      u_xx_exists_ignores_xx
		    ],
		    [not, [u_consider_as_new_goal_c63, u_x, u_y]]
		  ]
		],
		u_goal,
		
		[ append,
		  
		  [ u_cx_c36_get_all_ty,
		    u_xx_reality_lookahead_xx,
		    u_xx_succeeded_goal_ob_xx
		  ],
		  
		  [ u_cx_c36_get_all_ty,
		    u_xx_reality_lookahead_xx,
		    u_xx_failed_goal_ob_xx
		  ],
		  
		  [ u_cx_c36_get_all_ty,
		    u_xx_reality_lookahead_xx,
		    u_xx_active_goal_ob_xx
		  ]
		],
		Mem_Ret),
	Mem_Ret=FnResult.
:- set_opv(f_u_top_level_goal_exists_c63, classof, claz_function),
   set_opv(u_top_level_goal_exists_c63, compile_as, kw_function),
   set_opv(u_top_level_goal_exists_c63, function, f_u_top_level_goal_exists_c63),
   DefunResult=u_top_level_goal_exists_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (or (mem (lambda (x y) (ob$unify1 x (ob$get y 'obj) *empty-bd*",
				     1,
				     26368)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                    *exists-ignores*))",
				     1,
				     26434)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (ob$get goal 'obj)", 1, 26490)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           *top-level-goals*)", 1, 26521)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (mem (lambda (x y) (and (tlg? y *reality*)",
				     1,
				     26552)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                              (ob$unify1 x (ob$get y 'obj) *empty-bd*",
				     1,
				     26602)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                         *exists-ignores*)))",
				     1,
				     26673)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (ob$get goal 'obj)", 1, 26735)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (cx$get-all-ty *reality* *succeeded-goal-ob*))",
				     1,
				     26766)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (mem (lambda (x y) (and (tlg? y *reality*)",
				     1,
				     26825)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                              (ob$unify1 x (ob$get y 'obj) *empty-bd*",
				     1,
				     26875)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                        *exists-ignores*)))",
				     1,
				     26946)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (ob$get goal 'obj)", 1, 27007)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:25865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (cx$get-all-ty *reality* *failed-goal-ob*))))",
				     1,
				     27038)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27096 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-exists?',
			    [goal, context],
			    
			    [ if,
			      ['ty$instance?', goal, [quote, 'p-goal']],
			      
			      [ mem,
				
				[ lambda,
				  [x, y],
				  
				  [ 'ob$unify1',
				    x,
				    ['ob$get', y, [quote, obj]],
				    '*empty-bd*',
				    '*exists-ignores*'
				  ]
				],
				['ob$get', goal, [quote, obj]],
				['cx$get-all-ty', context, '*active-p-goal-ob*']
			      ],
			      
			      [ or,
				['top-level-goal-exists?', goal],
				
				[ mem,
				  
				  [ lambda,
				    [x, y],
				    
				    [ and,
				      
				      [ 'ob$unify1',
					['ob$get', x, [quote, obj]],
					['ob$get', y, [quote, obj]],
					'*empty-bd*',
					'*exists-ignores*'
				      ],
				      [not, ['consider-as-new-goal?', x, y]]
				    ]
				  ],
				  goal,
				  
				  [ append,
				    
				    [ 'cx$get-all-ty',
				      context,
				      '*succeeded-goal-ob*'
				    ],
				    
				    [ 'cx$get-all-ty',
				      context,
				      '*failed-goal-ob*'
				    ],
				    
				    [ 'cx$get-all-ty',
				      context,
				      '*active-goal-ob*'
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GOAL-EXISTS? 
wl: lambda_def(defun,
	      u_goal_exists_c63,
	      f_u_goal_exists_c63,
	      [u_goal, u_context],
	      
	      [ 
		[ if,
		  [u_ty_c36_instance_c63, u_goal, [quote, u_p_goal]],
		  
		  [ u_mem,
		    
		    [ lambda,
		      [u_x, u_y],
		      
		      [ u_ob_c36_unify1,
			u_x,
			[u_ob_c36_get, u_y, [quote, u_obj]],
			u_xx_empty_bd_xx,
			u_xx_exists_ignores_xx
		      ]
		    ],
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    [u_cx_c36_get_all_ty, u_context, u_xx_active_p_goal_ob_xx]
		  ],
		  
		  [ or,
		    [u_top_level_goal_exists_c63, u_goal],
		    
		    [ u_mem,
		      
		      [ lambda,
			[u_x, u_y],
			
			[ and,
			  
			  [ u_ob_c36_unify1,
			    [u_ob_c36_get, u_x, [quote, u_obj]],
			    [u_ob_c36_get, u_y, [quote, u_obj]],
			    u_xx_empty_bd_xx,
			    u_xx_exists_ignores_xx
			  ],
			  [not, [u_consider_as_new_goal_c63, u_x, u_y]]
			]
		      ],
		      u_goal,
		      
		      [ append,
			
			[ u_cx_c36_get_all_ty,
			  u_context,
			  u_xx_succeeded_goal_ob_xx
			],
			[u_cx_c36_get_all_ty, u_context, u_xx_failed_goal_ob_xx],
			[u_cx_c36_get_all_ty, u_context, u_xx_active_goal_ob_xx]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GOAL-EXISTS? 
wl: arglist_info(u_goal_exists_c63,
		[u_goal, u_context],
		[Goal_Param, Context_Param],
		arginfo{ all:[u_goal, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context],
			 opt:0,
			 req:[u_goal, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GOAL-EXISTS? 
wl: init_args(exact_only, u_goal_exists_c63).


% annotating U::GOAL-EXISTS? 
f_u_goal_exists_c63(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	f_u_ty_c36_instance_c63(Goal_Param, u_p_goal, IFTEST),
	(   IFTEST\==[]
	->  f_u_mem(
		    [ lambda,
		      [u_x, u_y],
		      
		      [ u_ob_c36_unify1,
			u_x,
			[u_ob_c36_get, u_y, [quote, u_obj]],
			u_xx_empty_bd_xx,
			u_xx_exists_ignores_xx
		      ]
		    ],
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    [u_cx_c36_get_all_ty, u_context, u_xx_active_p_goal_ob_xx],
		    TrueResult),
	    FnResult=TrueResult
	;   f_u_top_level_goal_exists_c63(Goal_Param, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_mem(
		    [ lambda,
		      [u_x, u_y],
		      
		      [ and,
			
			[ u_ob_c36_unify1,
			  [u_ob_c36_get, u_x, [quote, u_obj]],
			  [u_ob_c36_get, u_y, [quote, u_obj]],
			  u_xx_empty_bd_xx,
			  u_xx_exists_ignores_xx
			],
			[not, [u_consider_as_new_goal_c63, u_x, u_y]]
		      ]
		    ],
		    u_goal,
		    
		    [ append,
		      
		      [ u_cx_c36_get_all_ty,
			u_context,
			u_xx_succeeded_goal_ob_xx
		      ],
		      [u_cx_c36_get_all_ty, u_context, u_xx_failed_goal_ob_xx],
		      [u_cx_c36_get_all_ty, u_context, u_xx_active_goal_ob_xx]
		    ],
		    Mem_Ret),
	    FnResult=Mem_Ret
	).
:- set_opv(f_u_goal_exists_c63, classof, claz_function),
   set_opv(u_goal_exists_c63, compile_as, kw_function),
   set_opv(u_goal_exists_c63, function, f_u_goal_exists_c63),
   DefunResult=u_goal_exists_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27096 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" then", 7, 27172)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27096 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" else", 7, 27390)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'consider-as-new-goal?',
			    ['new-goal', 'old-goal'],
			    
			    [ and,
			      
			      [ 'neq?',
				['ob$ty', 'new-goal'],
				['ob$ty', 'old-goal']
			      ],
			      
			      [ if,
				
				[ 'ty$instance?',
				  'new-goal',
				  [quote, 'active-goal']
				],
				[not, ['dd-goal?', 'old-goal']],
				t
			      ]
			    ]
			  ]).

% annotating U::CONSIDER-AS-NEW-GOAL? 
wl: lambda_def(defun,
	      u_consider_as_new_goal_c63,
	      f_u_consider_as_new_goal_c63,
	      [u_new_goal, u_old_goal],
	      
	      [ 
		[ and,
		  
		  [ u_neq_c63,
		    [u_ob_c36_ty, u_new_goal],
		    [u_ob_c36_ty, u_old_goal]
		  ],
		  
		  [ if,
		    [u_ty_c36_instance_c63, u_new_goal, [quote, u_active_goal]],
		    [not, [u_dd_goal_c63, u_old_goal]],
		    t
		  ]
		]
	      ]).


% annotating U::CONSIDER-AS-NEW-GOAL? 
wl: arglist_info(u_consider_as_new_goal_c63,
		[u_new_goal, u_old_goal],
		[New_goal_Param, Old_goal_Param],
		arginfo{ all:[u_new_goal, u_old_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_new_goal, u_old_goal],
			 opt:0,
			 req:[u_new_goal, u_old_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CONSIDER-AS-NEW-GOAL? 
wl: init_args(exact_only, u_consider_as_new_goal_c63).


% annotating U::CONSIDER-AS-NEW-GOAL? 
f_u_consider_as_new_goal_c63(New_goal_Param, Old_goal_Param, TrueResult21) :-
	Env=[bv(u_new_goal, New_goal_Param), bv(u_old_goal, Old_goal_Param)],
	f_u_neq_c63([u_ob_c36_ty, u_new_goal], [u_ob_c36_ty, u_old_goal], IFTEST),
	(   IFTEST\==[]
	->  f_u_ty_c36_instance_c63(New_goal_Param, u_active_goal, IFTEST16),
	    (   IFTEST16\==[]
	    ->  f_u_dd_goal_c63(Old_goal_Param, Not_Param),
		cl_not(Not_Param, TrueResult),
		TrueResult21=TrueResult
	    ;   TrueResult21=t
	    )
	;   TrueResult21=[]
	).
:- set_opv(f_u_consider_as_new_goal_c63, classof, claz_function),
   set_opv(u_consider_as_new_goal_c63, compile_as, kw_function),
   set_opv(u_consider_as_new_goal_c63, function, f_u_consider_as_new_goal_c63),
   DefunResult=u_consider_as_new_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  Don't reactivate succeeded or failed goals.",
				     1,
				     27928)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (not (and (ty$instance? new-goal ^active-goal)",
				     1,
				     27975)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (or (ty$instance? old-goal ^succeeded-goal)",
				     1,
				     28025)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:27878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (ty$instance? old-gaol ^failed-goal))))",
				     1,
				     28082)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'other-goal-exists?',
			    [actor, 'goal-obj', context],
			    
			    [ 'any?',
			      
			      [ lambda,
				[belief],
				
				[ and,
				  
				  [ 'eq?',
				    actor,
				    ['ob$get', belief, [quote, actor]]
				  ],
				  
				  [ 'ty$instance?',
				    ['ob$get', belief, [quote, obj]],
				    [quote, goal]
				  ],
				  
				  [ 'ob$unify1',
				    'goal-obj',
				    ['ob$pget', belief, [quote, [obj, obj]]],
				    '*empty-bd*',
				    '*exists-ignores*'
				  ]
				]
			      ],
			      ['cx$get-all-ty', context, '*believe-ob*']
			    ]
			  ]).

% annotating U::OTHER-GOAL-EXISTS? 
wl: lambda_def(defun,
	      u_other_goal_exists_c63,
	      f_u_other_goal_exists_c63,
	      [u_actor, u_goal_obj, u_context],
	      
	      [ 
		[ u_any_c63,
		  
		  [ lambda,
		    [u_belief],
		    
		    [ and,
		      
		      [ u_eq_c63,
			u_actor,
			[u_ob_c36_get, u_belief, [quote, u_actor]]
		      ],
		      
		      [ u_ty_c36_instance_c63,
			[u_ob_c36_get, u_belief, [quote, u_obj]],
			[quote, u_goal]
		      ],
		      
		      [ u_ob_c36_unify1,
			u_goal_obj,
			[u_ob_c36_pget, u_belief, [quote, [u_obj, u_obj]]],
			u_xx_empty_bd_xx,
			u_xx_exists_ignores_xx
		      ]
		    ]
		  ],
		  [u_cx_c36_get_all_ty, u_context, u_xx_believe_ob_xx]
		]
	      ]).


% annotating U::OTHER-GOAL-EXISTS? 
wl: arglist_info(u_other_goal_exists_c63,
		[u_actor, u_goal_obj, u_context],
		[Actor_Param, Goal_obj_Param, Context_Param],
		arginfo{ all:[u_actor, u_goal_obj, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_actor, u_goal_obj, u_context],
			 opt:0,
			 req:[u_actor, u_goal_obj, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OTHER-GOAL-EXISTS? 
wl: init_args(exact_only, u_other_goal_exists_c63).


% annotating U::OTHER-GOAL-EXISTS? 
f_u_other_goal_exists_c63(Actor_Param, Goal_obj_Param, Context_Param, FnResult) :-
	Env=[bv(u_actor, Actor_Param), bv(u_goal_obj, Goal_obj_Param), bv(u_context, Context_Param)],
	f_u_any_c63(
		    [ lambda,
		      [u_belief],
		      
		      [ and,
			
			[ u_eq_c63,
			  u_actor,
			  [u_ob_c36_get, u_belief, [quote, u_actor]]
			],
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_belief, [quote, u_obj]],
			  [quote, u_goal]
			],
			
			[ u_ob_c36_unify1,
			  u_goal_obj,
			  [u_ob_c36_pget, u_belief, [quote, [u_obj, u_obj]]],
			  u_xx_empty_bd_xx,
			  u_xx_exists_ignores_xx
			]
		      ]
		    ],
		    [u_cx_c36_get_all_ty, u_context, u_xx_believe_ob_xx],
		    Any_c63_Ret),
	Any_c63_Ret=FnResult.
:- set_opv(f_u_other_goal_exists_c63, classof, claz_function),
   set_opv(u_other_goal_exists_c63, compile_as, kw_function),
   set_opv(u_other_goal_exists_c63, function, f_u_other_goal_exists_c63),
   DefunResult=u_other_goal_exists_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'activate-other-top-goal',
			    [goal, 'top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("******************")
			    ],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Activate top-level goal")
			    ],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'(" ~A for ~A in ~A"),
			      goal,
			      [car, 'belief-path'],
			      context
			    ],
			    ['ndbg-newline', '*gate-dbg*', rule],
			    ['cx$assert-relative', context, goal, 'belief-path'],
			    
			    [ 'ob$add',
			      goal,
			      [quote, 'top-level-goal'],
			      'top-level-goal'
			    ]
			  ]).

% annotating U::ACTIVATE-OTHER-TOP-GOAL 
wl: lambda_def(defun,
	      u_activate_other_top_goal,
	      f_u_activate_other_top_goal,
	      [u_goal, u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*),
			     #\(*)
			   ])
		],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(v),
			     #\(a),
			     #\(t),
			     #\(e),
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
			     #\(' '),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l)
			   ])
		],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
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
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_goal,
		  [car, u_belief_path],
		  u_context
		],
		[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		[u_cx_c36_assert_relative, u_context, u_goal, u_belief_path],
		
		[ u_ob_c36_add,
		  u_goal,
		  [quote, u_top_level_goal],
		  u_top_level_goal
		]
	      ]).


% annotating U::ACTIVATE-OTHER-TOP-GOAL 
wl: arglist_info(u_activate_other_top_goal,
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

% annotating U::ACTIVATE-OTHER-TOP-GOAL 
wl: init_args(exact_only, u_activate_other_top_goal).


% annotating U::ACTIVATE-OTHER-TOP-GOAL 
f_u_activate_other_top_goal(Goal_Param, Top_level_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*),
				       #\(*)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\('A'),
				    #\(c),
				    #\(t),
				    #\(i),
				    #\(v),
				    #\(a),
				    #\(t),
				    #\(e),
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
				    #\(' '),
				    #\(g),
				    #\(o),
				    #\(a),
				    #\(l)
				  ])
		       ],
		       Ndbg_roman_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\(' '),
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
				    #\(i),
				    #\(n),
				    #\(' '),
				    #\(~),
				    #\('A')
				  ]),
			 u_goal,
			 [car, u_belief_path],
			 u_context
		       ],
		       Ndbg_roman_Ret28),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	f_u_cx_c36_assert_relative(Context_Param,
				   Goal_Param,
				   Belief_path_Param,
				   Assert_relative_Ret),
	f_u_ob_c36_add(Goal_Param,
		       u_top_level_goal,
		       Top_level_goal_Param,
		       C36_add_Ret),
	C36_add_Ret=FnResult.
:- set_opv(f_u_activate_other_top_goal, classof, claz_function),
   set_opv(u_activate_other_top_goal, compile_as, kw_function),
   set_opv(u_activate_other_top_goal, function, f_u_activate_other_top_goal),
   DefunResult=u_activate_other_top_goal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" In theory, bindings would have to be checked also (since there are",
				     1,
				     29052)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" several possible mappings). But this is sufficient for the cases",
				     1,
				     29121)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" which have arisen so far.", 1, 29188)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 29216)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" There can only be one set of antecedents connected to a consequent on",
				     1,
				     29218)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" behalf of a given rule (in accordance with above assumption).",
				     1,
				     29290)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:28641 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 29354)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:29355 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'already-inferred?',
			    
			    [ 'show-result',
			      context,
			      'already-inferred',
			      'belief-path',
			      rule
			    ],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, fact, in, 'already-inferred'],
			      [yuntil, result],
			      
			      [ ydo,
				
				[ if,
				  
				  [ 'same-as-show-result',
				    
				    [ reverse,
				      
				      [ 'ol-get-relative-rule',
					fact,
					'*dependency-ob*',
					[quote, backward],
					context,
					'belief-path',
					rule
				      ]
				    ],
				    'show-result'
				  ],
				  [setq, result, t]
				]
			      ],
			      
			      [ yresult,
				
				[ progn,
				  
				  [ if,
				    ['null?', result],
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      inference,
				      '$STRING'("Not already inferred; show = ~A, facts = ~A in ~A"),
				      'show-result',
				      'already-inferred',
				      context
				    ],
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      inference,
				      '$STRING'("Already inferred; show = ~A, facts = ~A in ~A"),
				      'show-result',
				      'already-inferred',
				      context
				    ]
				  ],
				  result
				]
			      ]
			    ]
			  ]).

% annotating U::ALREADY-INFERRED? 
wl: lambda_def(defun,
	      u_already_inferred_c63,
	      f_u_already_inferred_c63,
	      
	      [ u_show_result,
		u_context,
		u_already_inferred,
		u_belief_path,
		u_rule
	      ],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_fact, u_in, u_already_inferred],
		  [u_yuntil, u_result],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ u_same_as_show_result,
			
			[ reverse,
			  
			  [ u_ol_get_relative_rule,
			    u_fact,
			    u_xx_dependency_ob_xx,
			    [quote, u_backward],
			    u_context,
			    u_belief_path,
			    u_rule
			  ]
			],
			u_show_result
		      ],
		      [setq, u_result, t]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ progn,
		      
		      [ if,
			[u_null_c63, u_result],
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_inference,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('N'),
				     #\(o),
				     #\(t),
				     #\(' '),
				     #\(a),
				     #\(l),
				     #\(r),
				     #\(e),
				     #\(a),
				     #\(d),
				     #\(y),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(f),
				     #\(e),
				     #\(r),
				     #\(r),
				     #\(e),
				     #\(d),
				     #\(;),
				     #\(' '),
				     #\(s),
				     #\(h),
				     #\(o),
				     #\(w),
				     #\(' '),
				     #\(=),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(','),
				     #\(' '),
				     #\(f),
				     #\(a),
				     #\(c),
				     #\(t),
				     #\(s),
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
				     #\('A')
				   ]),
			  u_show_result,
			  u_already_inferred,
			  u_context
			],
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_inference,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('A'),
				     #\(l),
				     #\(r),
				     #\(e),
				     #\(a),
				     #\(d),
				     #\(y),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(f),
				     #\(e),
				     #\(r),
				     #\(r),
				     #\(e),
				     #\(d),
				     #\(;),
				     #\(' '),
				     #\(s),
				     #\(h),
				     #\(o),
				     #\(w),
				     #\(' '),
				     #\(=),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(','),
				     #\(' '),
				     #\(f),
				     #\(a),
				     #\(c),
				     #\(t),
				     #\(s),
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
				     #\('A')
				   ]),
			  u_show_result,
			  u_already_inferred,
			  u_context
			]
		      ],
		      u_result
		    ]
		  ]
		]
	      ]).


% annotating U::ALREADY-INFERRED? 
wl: arglist_info(u_already_inferred_c63,
		
		[ u_show_result,
		  u_context,
		  u_already_inferred,
		  u_belief_path,
		  u_rule
		],
		
		[ Show_result_Param,
		  Context_Param,
		  Already_inferred_Param,
		  Belief_path_Param,
		  Rule_Param
		],
		arginfo{ all:
			     [ u_show_result,
			       u_context,
			       u_already_inferred,
			       u_belief_path,
			       u_rule
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_show_result,
				 u_context,
				 u_already_inferred,
				 u_belief_path,
				 u_rule
			       ],
			 opt:0,
			 req:
			     [ u_show_result,
			       u_context,
			       u_already_inferred,
			       u_belief_path,
			       u_rule
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ALREADY-INFERRED? 
wl: init_args(exact_only, u_already_inferred_c63).


% annotating U::ALREADY-INFERRED? 
f_u_already_inferred_c63(Show_result_Param, Context_Param, Already_inferred_Param, Belief_path_Param, Rule_Param, FnResult) :-
	Env=[bv(u_show_result, Show_result_Param), bv(u_context, Context_Param), bv(u_already_inferred, Already_inferred_Param), bv(u_belief_path, Belief_path_Param), bv(u_rule, Rule_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_fact, u_in, u_already_inferred],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_same_as_show_result,
			  
			  [ reverse,
			    
			    [ u_ol_get_relative_rule,
			      u_fact,
			      u_xx_dependency_ob_xx,
			      [quote, u_backward],
			      u_context,
			      u_belief_path,
			      u_rule
			    ]
			  ],
			  u_show_result
			],
			[setq, u_result, t]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			
			[ if,
			  [u_null_c63, u_result],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_inference,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('N'),
				       #\(o),
				       #\(t),
				       #\(' '),
				       #\(a),
				       #\(l),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(d),
				       #\(y),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(r),
				       #\(e),
				       #\(d),
				       #\(;),
				       #\(' '),
				       #\(s),
				       #\(h),
				       #\(o),
				       #\(w),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(f),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(s),
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
				       #\('A')
				     ]),
			    u_show_result,
			    u_already_inferred,
			    u_context
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_inference,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(l),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(d),
				       #\(y),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(f),
				       #\(e),
				       #\(r),
				       #\(r),
				       #\(e),
				       #\(d),
				       #\(;),
				       #\(' '),
				       #\(s),
				       #\(h),
				       #\(o),
				       #\(w),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(f),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(s),
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
				       #\('A')
				     ]),
			    u_show_result,
			    u_already_inferred,
			    u_context
			  ]
			],
			u_result
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_already_inferred_c63, classof, claz_function),
   set_opv(u_already_inferred_c63, compile_as, kw_function),
   set_opv(u_already_inferred_c63, function, f_u_already_inferred_c63),
   DefunResult=u_already_inferred_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:30405 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'collect-inference-rules',
			    [assertions],
			    
			    [ if,
			      [<, [length, assertions], 10],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'rule-long',
				'$STRING'("Collecting inferences rules for ~A"),
				assertions
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'rule-long',
				'$STRING'("Collecting inferences rules for many assertions")
			      ]
			    ],
			    
			    [ yloop,
			      [initial, [result, []], [temp, []]],
			      [yfor, assertion, in, assertions],
			      [yuntil, ['eq?', result, '*rules*']],
			      
			      [ ydo,
				[setq, temp, ['forward-chain-rules', assertion]],
				
				[ if,
				  ['eq?', temp, '*rules*'],
				  [setq, result, '*rules*'],
				  [setq, result, [union, result, temp]]
				]
			      ],
			      
			      [ yresult,
				
				[ progn,
				  
				  [ if,
				    ['eq?', result, '*rules*'],
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      'rule-long',
				      '$STRING'("Using full rule set")
				    ],
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      'rule-long',
				      '$STRING'("Collected rules = ~A"),
				      result
				    ]
				  ],
				  result
				]
			      ]
			    ]
			  ]).

% annotating U::COLLECT-INFERENCE-RULES 
wl: lambda_def(defun,
	      u_collect_inference_rules,
	      f_u_collect_inference_rules,
	      [u_assertions],
	      
	      [ 
		[ if,
		  [<, [length, u_assertions], 10],
		  
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
			       #\(i),
			       #\(n),
			       #\(f),
			       #\(e),
			       #\(r),
			       #\(e),
			       #\(n),
			       #\(c),
			       #\(e),
			       #\(s),
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
		    u_assertions
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
			       #\(i),
			       #\(n),
			       #\(g),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(f),
			       #\(e),
			       #\(r),
			       #\(e),
			       #\(n),
			       #\(c),
			       #\(e),
			       #\(s),
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
			       #\(m),
			       #\(a),
			       #\(n),
			       #\(y),
			       #\(' '),
			       #\(a),
			       #\(s),
			       #\(s),
			       #\(e),
			       #\(r),
			       #\(t),
			       #\(i),
			       #\(o),
			       #\(n),
			       #\(s)
			     ])
		  ]
		],
		
		[ u_yloop,
		  [u_initial, [u_result, []], [u_temp, []]],
		  [u_yfor, u_assertion, u_in, u_assertions],
		  [u_yuntil, [u_eq_c63, u_result, u_xx_rules_xx]],
		  
		  [ u_ydo,
		    [setq, u_temp, [u_forward_chain_rules, u_assertion]],
		    
		    [ if,
		      [u_eq_c63, u_temp, u_xx_rules_xx],
		      [setq, u_result, u_xx_rules_xx],
		      [setq, u_result, [union, u_result, u_temp]]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ progn,
		      
		      [ if,
			[u_eq_c63, u_result, u_xx_rules_xx],
			
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
			  u_result
			]
		      ],
		      u_result
		    ]
		  ]
		]
	      ]).


% annotating U::COLLECT-INFERENCE-RULES 
wl: arglist_info(u_collect_inference_rules,
		[u_assertions],
		[Assertions_Param],
		arginfo{ all:[u_assertions],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_assertions],
			 opt:0,
			 req:[u_assertions],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::COLLECT-INFERENCE-RULES 
wl: init_args(exact_only, u_collect_inference_rules).


% annotating U::COLLECT-INFERENCE-RULES 
f_u_collect_inference_rules(Assertions_Param, FnResult) :-
	Env=[bv(u_assertions, Assertions_Param)],
	cl_length(Assertions_Param, PredArg1Result),
	(   PredArg1Result<10
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
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
					   #\(i),
					   #\(n),
					   #\(f),
					   #\(e),
					   #\(r),
					   #\(e),
					   #\(n),
					   #\(c),
					   #\(e),
					   #\(s),
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
				u_assertions
			      ],
			      TrueResult),
	    _119762=TrueResult
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
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(' '),
					   #\(i),
					   #\(n),
					   #\(f),
					   #\(e),
					   #\(r),
					   #\(e),
					   #\(n),
					   #\(c),
					   #\(e),
					   #\(s),
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
					   #\(m),
					   #\(a),
					   #\(n),
					   #\(y),
					   #\(' '),
					   #\(a),
					   #\(s),
					   #\(s),
					   #\(e),
					   #\(r),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(s)
					 ])
			      ],
			      ElseResult),
	    _119762=ElseResult
	),
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_temp, []]],
		    [u_yfor, u_assertion, u_in, u_assertions],
		    [u_yuntil, [u_eq_c63, u_result, u_xx_rules_xx]],
		    
		    [ u_ydo,
		      [setq, u_temp, [u_forward_chain_rules, u_assertion]],
		      
		      [ if,
			[u_eq_c63, u_temp, u_xx_rules_xx],
			[setq, u_result, u_xx_rules_xx],
			[setq, u_result, [union, u_result, u_temp]]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			
			[ if,
			  [u_eq_c63, u_result, u_xx_rules_xx],
			  
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
			    u_result
			  ]
			],
			u_result
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_collect_inference_rules, classof, claz_function),
   set_opv(u_collect_inference_rules, compile_as, kw_function),
   set_opv(u_collect_inference_rules, function, f_u_collect_inference_rules),
   DefunResult=u_collect_inference_rules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:31291 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'slow-facts-inferred-by',
			    [rule, context, 'belief-path'],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, ob, in, ['cx$get-all', context]],
			      
			      [ ydo,
				
				[ setq,
				  ob,
				  ['absolute->relative', ob, 'belief-path']
				],
				
				[ if,
				  
				  [ 'any?',
				    
				    [ lambda,
				      [x],
				      ['eq?', rule, ['ob$get', x, [quote, rule]]]
				    ],
				    
				    [ 'get-dependencies',
				      ob,
				      context,
				      'belief-path'
				    ]
				  ],
				  [setq, result, [cons, ob, result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::SLOW-FACTS-INFERRED-BY 
wl: lambda_def(defun,
	      u_slow_facts_inferred_by,
	      f_u_slow_facts_inferred_by,
	      [u_rule, u_context, u_belief_path],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		  
		  [ u_ydo,
		    [setq, u_ob, [u_absolute_c62_relative, u_ob, u_belief_path]],
		    
		    [ if,
		      
		      [ u_any_c63,
			
			[ lambda,
			  [u_x],
			  [u_eq_c63, u_rule, [u_ob_c36_get, u_x, [quote, u_rule]]]
			],
			[u_get_dependencies, u_ob, u_context, u_belief_path]
		      ],
		      [setq, u_result, [cons, u_ob, u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::SLOW-FACTS-INFERRED-BY 
wl: arglist_info(u_slow_facts_inferred_by,
		[u_rule, u_context, u_belief_path],
		[Rule_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_rule, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_context, u_belief_path],
			 opt:0,
			 req:[u_rule, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SLOW-FACTS-INFERRED-BY 
wl: init_args(exact_only, u_slow_facts_inferred_by).


% annotating U::SLOW-FACTS-INFERRED-BY 
f_u_slow_facts_inferred_by(Rule_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		    
		    [ u_ydo,
		      [setq, u_ob, [u_absolute_c62_relative, u_ob, u_belief_path]],
		      
		      [ if,
			
			[ u_any_c63,
			  
			  [ lambda,
			    [u_x],
			    
			    [ u_eq_c63,
			      u_rule,
			      [u_ob_c36_get, u_x, [quote, u_rule]]
			    ]
			  ],
			  [u_get_dependencies, u_ob, u_context, u_belief_path]
			],
			[setq, u_result, [cons, u_ob, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_slow_facts_inferred_by, classof, claz_function),
   set_opv(u_slow_facts_inferred_by, compile_as, kw_function),
   set_opv(u_slow_facts_inferred_by, function, f_u_slow_facts_inferred_by),
   DefunResult=u_slow_facts_inferred_by.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:31686 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'facts-inferred-by',
			    [rule, context, 'belief-path'],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, ob, in, ['cx$get-all', context]],
			      
			      [ ydo,
				
				[ setq,
				  ob,
				  ['absolute->relative', ob, 'belief-path']
				],
				
				[ if,
				  
				  [ and,
				    ob,
				    
				    [ 'memq?',
				      rule,
				      ['ob$gets', ob, [quote, 'inference-rule']]
				    ]
				  ],
				  [setq, result, [cons, ob, result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::FACTS-INFERRED-BY 
wl: lambda_def(defun,
	      u_facts_inferred_by,
	      f_u_facts_inferred_by,
	      [u_rule, u_context, u_belief_path],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		  
		  [ u_ydo,
		    [setq, u_ob, [u_absolute_c62_relative, u_ob, u_belief_path]],
		    
		    [ if,
		      
		      [ and,
			u_ob,
			
			[ u_memq_c63,
			  u_rule,
			  [u_ob_c36_gets, u_ob, [quote, u_inference_rule]]
			]
		      ],
		      [setq, u_result, [cons, u_ob, u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::FACTS-INFERRED-BY 
wl: arglist_info(u_facts_inferred_by,
		[u_rule, u_context, u_belief_path],
		[Rule_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_rule, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_context, u_belief_path],
			 opt:0,
			 req:[u_rule, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FACTS-INFERRED-BY 
wl: init_args(exact_only, u_facts_inferred_by).


% annotating U::FACTS-INFERRED-BY 
f_u_facts_inferred_by(Rule_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		    
		    [ u_ydo,
		      [setq, u_ob, [u_absolute_c62_relative, u_ob, u_belief_path]],
		      
		      [ if,
			
			[ and,
			  u_ob,
			  
			  [ u_memq_c63,
			    u_rule,
			    [u_ob_c36_gets, u_ob, [quote, u_inference_rule]]
			  ]
			],
			[setq, u_result, [cons, u_ob, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_facts_inferred_by, classof, claz_function),
   set_opv(u_facts_inferred_by, compile_as, kw_function),
   set_opv(u_facts_inferred_by, function, f_u_facts_inferred_by),
   DefunResult=u_facts_inferred_by.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:32015 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'beliefs-of',
			    [facts, 'belief-path'],
			    
			    [ yloop,
			      [initial, [result, []], [temp, []]],
			      [yfor, fact, in, facts],
			      
			      [ ydo,
				
				[ if,
				  
				  [ setq,
				    temp,
				    ['absolute->relative', fact, 'belief-path']
				  ],
				  [setq, result, [cons, temp, result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::BELIEFS-OF 
wl: lambda_def(defun,
	      u_beliefs_of,
	      f_u_beliefs_of,
	      [u_facts, u_belief_path],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_temp, []]],
		  [u_yfor, u_fact, u_in, u_facts],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ setq,
			u_temp,
			[u_absolute_c62_relative, u_fact, u_belief_path]
		      ],
		      [setq, u_result, [cons, u_temp, u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::BELIEFS-OF 
wl: arglist_info(u_beliefs_of,
		[u_facts, u_belief_path],
		[Facts_Param, Belief_path_Param],
		arginfo{ all:[u_facts, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_facts, u_belief_path],
			 opt:0,
			 req:[u_facts, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BELIEFS-OF 
wl: init_args(exact_only, u_beliefs_of).


% annotating U::BELIEFS-OF 
f_u_beliefs_of(Facts_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_facts, Facts_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_temp, []]],
		    [u_yfor, u_fact, u_in, u_facts],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ setq,
			  u_temp,
			  [u_absolute_c62_relative, u_fact, u_belief_path]
			],
			[setq, u_result, [cons, u_temp, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_beliefs_of, classof, claz_function),
   set_opv(u_beliefs_of, compile_as, kw_function),
   set_opv(u_beliefs_of, function, f_u_beliefs_of),
   DefunResult=u_beliefs_of.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:32288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'same-as-show-result',
			    [lst, 'show-result'],
			    
			    [ if,
			      [=, [length, lst], [length, [cdr, 'show-result']]],
			      
			      [ yloop,
				
				[ initial,
				  [result, t],
				  [l, lst],
				  [s, [cdr, 'show-result']]
				],
				[ywhile, l],
				[yuntil, ['null?', result]],
				
				[ ydo,
				  
				  [ if,
				    ['neq?', [car, l], [caar, s]],
				    [setq, result, []]
				  ],
				  [setq, l, [cdr, l]],
				  [setq, s, [cdr, s]]
				],
				[yresult, result]
			      ],
			      []
			    ]
			  ]).

% annotating U::SAME-AS-SHOW-RESULT 
wl: lambda_def(defun,
	      u_same_as_show_result,
	      f_u_same_as_show_result,
	      [u_lst, u_show_result],
	      
	      [ 
		[ if,
		  [=, [length, u_lst], [length, [cdr, u_show_result]]],
		  
		  [ u_yloop,
		    
		    [ u_initial,
		      [u_result, t],
		      [u_l, u_lst],
		      [u_s, [cdr, u_show_result]]
		    ],
		    [u_ywhile, u_l],
		    [u_yuntil, [u_null_c63, u_result]],
		    
		    [ u_ydo,
		      [if, [u_neq_c63, [car, u_l], [caar, u_s]], [setq, u_result, []]],
		      [setq, u_l, [cdr, u_l]],
		      [setq, u_s, [cdr, u_s]]
		    ],
		    [u_yresult, u_result]
		  ],
		  []
		]
	      ]).


% annotating U::SAME-AS-SHOW-RESULT 
wl: arglist_info(u_same_as_show_result,
		[u_lst, u_show_result],
		[Lst_Param, Show_result_Param],
		arginfo{ all:[u_lst, u_show_result],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst, u_show_result],
			 opt:0,
			 req:[u_lst, u_show_result],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SAME-AS-SHOW-RESULT 
wl: init_args(exact_only, u_same_as_show_result).


% annotating U::SAME-AS-SHOW-RESULT 
f_u_same_as_show_result(Lst_Param, Show_result_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param), bv(u_show_result, Show_result_Param)],
	cl_length(Lst_Param, PredArg1Result),
	cl_cdr(Show_result_Param, Length_Param),
	cl_length(Length_Param, PredArg2Result),
	(   PredArg1Result=:=PredArg2Result
	->  f_u_yloop(
		      [ 
			[ u_initial,
			  [u_result, t],
			  [u_l, u_lst],
			  [u_s, [cdr, u_show_result]]
			],
			[u_ywhile, u_l],
			[u_yuntil, [u_null_c63, u_result]],
			
			[ u_ydo,
			  
			  [ if,
			    [u_neq_c63, [car, u_l], [caar, u_s]],
			    [setq, u_result, []]
			  ],
			  [setq, u_l, [cdr, u_l]],
			  [setq, u_s, [cdr, u_s]]
			],
			[u_yresult, u_result]
		      ],
		      TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_same_as_show_result, classof, claz_function),
   set_opv(u_same_as_show_result, compile_as, kw_function),
   set_opv(u_same_as_show_result, function, f_u_same_as_show_result),
   DefunResult=u_same_as_show_result.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:32288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Let's not bother having others do p-goal planning for now.",
				     1,
				     32748)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:32808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-p-goals',
			    [context, 'top-level-goal'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Running p-goals in ~A"),
			      context
			    ],
			    
			    [ yloop,
			      [initial, ['sprouted-contexts', []]],
			      
			      [ yfor,
				elem,
				in,
				['cx$get-all-ty', context, '*active-p-goal-ob*']
			      ],
			      
			      [ ydo,
				
				[ if,
				  
				  [ not,
				    
				    [ 'has-link?',
				      elem,
				      [quote, 'linked-from-of'],
				      '*intends-ob*',
				      context
				    ]
				  ],
				  
				  [ setq,
				    'sprouted-contexts',
				    
				    [ 'append!',
				      
				      [ 'plan-p-goal',
					elem,
					context,
					'top-level-goal'
				      ],
				      'sprouted-contexts'
				    ]
				  ]
				]
			      ],
			      [yresult, 'sprouted-contexts']
			    ]
			  ]).

% annotating U::RUN-P-GOALS 
wl: lambda_def(defun,
	      u_run_p_goals,
	      f_u_run_p_goals,
	      [u_context, u_top_level_goal],
	      
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
			     #\(-),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
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
		  [u_initial, [u_sprouted_contexts, []]],
		  
		  [ u_yfor,
		    u_elem,
		    u_in,
		    [u_cx_c36_get_all_ty, u_context, u_xx_active_p_goal_ob_xx]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ not,
			
			[ u_has_link_c63,
			  u_elem,
			  [quote, u_linked_from_of],
			  u_xx_intends_ob_xx,
			  u_context
			]
		      ],
		      
		      [ setq,
			u_sprouted_contexts,
			
			[ u_append_c33,
			  [u_plan_p_goal, u_elem, u_context, u_top_level_goal],
			  u_sprouted_contexts
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_sprouted_contexts]
		]
	      ]).


% annotating U::RUN-P-GOALS 
wl: arglist_info(u_run_p_goals,
		[u_context, u_top_level_goal],
		[Context_Param, Top_level_goal_Param],
		arginfo{ all:[u_context, u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_top_level_goal],
			 opt:0,
			 req:[u_context, u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-P-GOALS 
wl: init_args(exact_only, u_run_p_goals).


% annotating U::RUN-P-GOALS 
f_u_run_p_goals(Context_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param)],
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
				       #\(-),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
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
		  [ [u_initial, [u_sprouted_contexts, []]],
		    
		    [ u_yfor,
		      u_elem,
		      u_in,
		      [u_cx_c36_get_all_ty, u_context, u_xx_active_p_goal_ob_xx]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ not,
			  
			  [ u_has_link_c63,
			    u_elem,
			    [quote, u_linked_from_of],
			    u_xx_intends_ob_xx,
			    u_context
			  ]
			],
			
			[ setq,
			  u_sprouted_contexts,
			  
			  [ u_append_c33,
			    [u_plan_p_goal, u_elem, u_context, u_top_level_goal],
			    u_sprouted_contexts
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_run_p_goals, classof, claz_function),
   set_opv(u_run_p_goals, compile_as, kw_function),
   set_opv(u_run_p_goals, function, f_u_run_p_goals),
   DefunResult=u_run_p_goals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:32808 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 33308)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:32808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" P-goal planning: so far this is only used in rationalization daydream.",
				     1,
				     33310)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:32808 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 33383)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule4.cl:33384 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'plan-p-goal',
			    ['p-goal', context, 'top-level-goal'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Planning for p-goal ~A in ~A"),
			      'p-goal',
			      context
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				['sprouted-context', []],
				['sprouted-contexts', []],
				['subgoal-obj', []]
			      ],
			      
			      [ yfor,
				'leaf-cause',
				in,
				['get-leaf-causes', 'p-goal', context]
			      ],
			      
			      [ ydo,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'rule-xtra',
				  '$STRING'("leaf-cause = ~A"),
				  'leaf-cause'
				],
				
				[ if,
				  ['ty$instance?', 'leaf-cause', [quote, not]],
				  
				  [ progn,
				    
				    [ setq,
				      'sprouted-context',
				      ['cx$sprout', context]
				    ],
				    
				    [ setq,
				      'sprouted-contexts',
				      
				      [ cons,
					'sprouted-context',
					'sprouted-contexts'
				      ]
				    ],
				    
				    [ 'delay-dbgs',
				      'sprouted-context',
				      
				      [ setq,
					'subgoal-obj',
					
					[ 'ob$copy',
					  ['ob$get', 'leaf-cause', [quote, obj]]
					]
				      ],
				      
				      [ 'ob$set',
					'subgoal-obj',
					[quote, 'plan-subgoalnum'],
					0
				      ],
				      
				      [ 'activate-subgoal',
					'p-goal',
					'subgoal-obj',
					'sprouted-context',
					
					[ 'ob$fcreate',
					  
					  [ quote,
					    ['RULE', 'constructed?', [quote, t]]
					  ]
					],
					[],
					t,
					'*me-belief-path*',
					'top-level-goal',
					t
				      ],
				      ['set-ordering', 'sprouted-context', 1.0]
				    ]
				  ]
				]
			      ],
			      [yresult, 'sprouted-contexts']
			    ]
			  ]).

% annotating U::PLAN-P-GOAL 
wl: lambda_def(defun,
	      u_plan_p_goal,
	      f_u_plan_p_goal,
	      [u_p_goal, u_context, u_top_level_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(n),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(p),
			     #\(-),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
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
		  u_p_goal,
		  u_context
		],
		
		[ u_yloop,
		  
		  [ u_initial,
		    [u_sprouted_context, []],
		    [u_sprouted_contexts, []],
		    [u_subgoal_obj, []]
		  ],
		  
		  [ u_yfor,
		    u_leaf_cause,
		    u_in,
		    [u_get_leaf_causes, u_p_goal, u_context]
		  ],
		  
		  [ u_ydo,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule_xtra,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(l),
				 #\(e),
				 #\(a),
				 #\(f),
				 #\(-),
				 #\(c),
				 #\(a),
				 #\(u),
				 #\(s),
				 #\(e),
				 #\(' '),
				 #\(=),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_leaf_cause
		    ],
		    
		    [ if,
		      [u_ty_c36_instance_c63, u_leaf_cause, [quote, not]],
		      
		      [ progn,
			[setq, u_sprouted_context, [u_cx_c36_sprout, u_context]],
			
			[ setq,
			  u_sprouted_contexts,
			  [cons, u_sprouted_context, u_sprouted_contexts]
			],
			
			[ u_delay_dbgs,
			  u_sprouted_context,
			  
			  [ setq,
			    u_subgoal_obj,
			    
			    [ u_ob_c36_copy,
			      [u_ob_c36_get, u_leaf_cause, [quote, u_obj]]
			    ]
			  ],
			  
			  [ u_ob_c36_set,
			    u_subgoal_obj,
			    [quote, u_plan_subgoalnum],
			    0
			  ],
			  
			  [ u_activate_subgoal,
			    u_p_goal,
			    u_subgoal_obj,
			    u_sprouted_context,
			    
			    [ u_ob_c36_fcreate,
			      [quote, [u_rule, u_constructed_c63, [quote, t]]]
			    ],
			    [],
			    t,
			    u_xx_me_belief_path_xx,
			    u_top_level_goal,
			    t
			  ],
			  [u_set_ordering, u_sprouted_context, 1.0]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_sprouted_contexts]
		]
	      ]).


% annotating U::PLAN-P-GOAL 
wl: arglist_info(u_plan_p_goal,
		[u_p_goal, u_context, u_top_level_goal],
		[P_goal_Param, Context_Param, Top_level_goal_Param],
		arginfo{ all:[u_p_goal, u_context, u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_p_goal, u_context, u_top_level_goal],
			 opt:0,
			 req:[u_p_goal, u_context, u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PLAN-P-GOAL 
wl: init_args(exact_only, u_plan_p_goal).


% annotating U::PLAN-P-GOAL 
f_u_plan_p_goal(P_goal_Param, Context_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_p_goal, P_goal_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(n),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(p),
				       #\(-),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
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
			    u_p_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_sprouted_context, []],
		      [u_sprouted_contexts, []],
		      [u_subgoal_obj, []]
		    ],
		    
		    [ u_yfor,
		      u_leaf_cause,
		      u_in,
		      [u_get_leaf_causes, u_p_goal, u_context]
		    ],
		    
		    [ u_ydo,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule_xtra,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(l),
				   #\(e),
				   #\(a),
				   #\(f),
				   #\(-),
				   #\(c),
				   #\(a),
				   #\(u),
				   #\(s),
				   #\(e),
				   #\(' '),
				   #\(=),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_leaf_cause
		      ],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_leaf_cause, [quote, not]],
			
			[ progn,
			  
			  [ setq,
			    u_sprouted_context,
			    [u_cx_c36_sprout, u_context]
			  ],
			  
			  [ setq,
			    u_sprouted_contexts,
			    [cons, u_sprouted_context, u_sprouted_contexts]
			  ],
			  
			  [ u_delay_dbgs,
			    u_sprouted_context,
			    
			    [ setq,
			      u_subgoal_obj,
			      
			      [ u_ob_c36_copy,
				[u_ob_c36_get, u_leaf_cause, [quote, u_obj]]
			      ]
			    ],
			    
			    [ u_ob_c36_set,
			      u_subgoal_obj,
			      [quote, u_plan_subgoalnum],
			      0
			    ],
			    
			    [ u_activate_subgoal,
			      u_p_goal,
			      u_subgoal_obj,
			      u_sprouted_context,
			      
			      [ u_ob_c36_fcreate,
				[quote, [u_rule, u_constructed_c63, [quote, t]]]
			      ],
			      [],
			      t,
			      u_xx_me_belief_path_xx,
			      u_top_level_goal,
			      t
			    ],
			    [u_set_ordering, u_sprouted_context, 1.0]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_plan_p_goal, classof, claz_function),
   set_opv(u_plan_p_goal, compile_as, kw_function),
   set_opv(u_plan_p_goal, function, f_u_plan_p_goal),
   DefunResult=u_plan_p_goal.


% Total time: 11.776 seconds


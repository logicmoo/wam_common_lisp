
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_cntrl" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:12:09 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";(break)", 1, 528)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/25/86: Removed flavors", 1, 208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:235 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 19990504: ported to Common Lisp", 1, 236)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:269 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 270)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:271 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     272)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:352 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 354)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:355 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Top-level functions", 1, 356)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:377 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 378)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:379 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setq, '*starting-state*', [quote, daydreaming]]).
:- set_var(TLEnv3, setq, u_xx_starting_state_xx, u_daydreaming).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    daydreamer,
			    [],
			    ['ndbg-reset'],
			    
			    [ if,
			      '*typeset?*',
			      
			      [ format,
				'*gate-dbg*',
				'$STRING'("\\begin{flushleft}~%")
			      ]
			    ],
			    ['ndbg-roman-nl', '*gate-dbg*', rule, '*dd-version*'],
			    [setq, '*state*', [quote, suspended]],
			    ['daydreamer-initialize'],
			    ['set-state', '*starting-state*'],
			    
			    [ 'run-inferences',
			      '*reality-lookahead*',
			      [],
			      '*me-belief-path*'
			    ],
			    ['daydreamer-control0'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("DAYDREAMER terminates")
			    ],
			    
			    [ if,
			      '*typeset?*',
			      
			      [ format,
				'*gate-dbg*',
				'$STRING'("\\end{flushleft}~%")
			      ]
			    ],
			    t
			  ]).

% annotating U::DAYDREAMER 
wl: lambda_def(defun,
	      u_daydreamer,
	      f_u_daydreamer,
	      [],
	      
	      [ [u_ndbg_reset],
		
		[ if,
		  u_xx_typeset_c63_xx,
		  
		  [ format,
		    u_xx_gate_dbg_xx,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(\),
			       #\(b),
			       #\(e),
			       #\(g),
			       #\(i),
			       #\(n),
			       #\('{'),
			       #\(f),
			       #\(l),
			       #\(u),
			       #\(s),
			       #\(h),
			       #\(l),
			       #\(e),
			       #\(f),
			       #\(t),
			       #\('}'),
			       #\(~),
			       #\('%')
			     ])
		  ]
		],
		[u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, u_xx_dd_version_xx],
		[setq, u_xx_state_xx, [quote, u_suspended]],
		[u_daydreamer_initialize],
		[u_set_state, u_xx_starting_state_xx],
		
		[ u_run_inferences,
		  u_xx_reality_lookahead_xx,
		  [],
		  u_xx_me_belief_path_xx
		],
		[u_daydreamer_control0],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('D'),
			     #\('A'),
			     #\('Y'),
			     #\('D'),
			     #\('R'),
			     #\('E'),
			     #\('A'),
			     #\('M'),
			     #\('E'),
			     #\('R'),
			     #\(' '),
			     #\(t),
			     #\(e),
			     #\(r),
			     #\(m),
			     #\(i),
			     #\(n),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\(s)
			   ])
		],
		
		[ if,
		  u_xx_typeset_c63_xx,
		  
		  [ format,
		    u_xx_gate_dbg_xx,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(\),
			       #\(e),
			       #\(n),
			       #\(d),
			       #\('{'),
			       #\(f),
			       #\(l),
			       #\(u),
			       #\(s),
			       #\(h),
			       #\(l),
			       #\(e),
			       #\(f),
			       #\(t),
			       #\('}'),
			       #\(~),
			       #\('%')
			     ])
		  ]
		],
		t
	      ]).


% annotating U::DAYDREAMER 
wl: arglist_info(u_daydreamer,
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

% annotating U::DAYDREAMER 
wl: init_args(exact_only, u_daydreamer).


% annotating U::DAYDREAMER 
f_u_daydreamer(FnResult) :-
	Env=[],
	f_u_ndbg_reset(Ndbg_reset_Ret),
	get_var(Env, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(\),
				   #\(b),
				   #\(e),
				   #\(g),
				   #\(i),
				   #\(n),
				   #\('{'),
				   #\(f),
				   #\(l),
				   #\(u),
				   #\(s),
				   #\(h),
				   #\(l),
				   #\(e),
				   #\(f),
				   #\(t),
				   #\('}'),
				   #\(~),
				   #\('%')
				 ])
		      ],
		      TrueResult),
	    _39346=TrueResult
	;   _39346=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  [u_xx_dd_version_xx],
			  Roman_nl_Ret),
	set_var(Env, setq, u_xx_state_xx, u_suspended),
	f_u_daydreamer_initialize(Daydreamer_initialize_Ret),
	get_var(Env, u_xx_starting_state_xx, Xx_starting_state_xx_Get),
	f_u_set_state(Xx_starting_state_xx_Get, Set_state_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	f_u_run_inferences(Xx_reality_lookahead_xx_Get,
			   [],
			   Xx_me_belief_path_xx_Get,
			   Run_inferences_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('D'),
				       #\('A'),
				       #\('Y'),
				       #\('D'),
				       #\('R'),
				       #\('E'),
				       #\('A'),
				       #\('M'),
				       #\('E'),
				       #\('R'),
				       #\(' '),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(m),
				       #\(i),
				       #\(n),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\(s)
				     ])
			  ],
			  Roman_nl_Ret31),
	get_var(Env, u_xx_typeset_c63_xx, IFTEST18),
	(   IFTEST18\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get21),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get21,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(\),
				   #\(e),
				   #\(n),
				   #\(d),
				   #\('{'),
				   #\(f),
				   #\(l),
				   #\(u),
				   #\(s),
				   #\(h),
				   #\(l),
				   #\(e),
				   #\(f),
				   #\(t),
				   #\('}'),
				   #\(~),
				   #\('%')
				 ])
		      ],
		      TrueResult22),
	    _39842=TrueResult22
	;   _39842=[]
	),
	t=FnResult.
:- set_opv(f_u_daydreamer, classof, claz_function),
   set_opv(u_daydreamer, compile_as, kw_function),
   set_opv(u_daydreamer, function, f_u_daydreamer),
   DefunResult=u_daydreamer.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Get off the ground by running inferences which will activate",
				     3,
				     656)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:418 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" some top-level goals.", 3, 721)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Run the top-level emotion-directed control loop.",
				     3,
				     807)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1010 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'dd-continue', [], ['daydreamer-control0']]).

% annotating U::DD-CONTINUE 
wl: lambda_def(defun,
	      u_dd_continue,
	      f_u_dd_continue,
	      [],
	      [[u_daydreamer_control0]]).


% annotating U::DD-CONTINUE 
wl: arglist_info(u_dd_continue,
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

% annotating U::DD-CONTINUE 
wl: init_args(exact_only, u_dd_continue).


% annotating U::DD-CONTINUE 
f_u_dd_continue(FnResult) :-
	Env=[],
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_dd_continue, classof, claz_function),
   set_opv(u_dd_continue, compile_as, kw_function),
   set_opv(u_dd_continue, function, f_u_dd_continue),
   DefunResult=u_dd_continue.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1010 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This will pick up from the first subgoal in the set of subgoals of",
				     1,
				     1059)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1010 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" which goal is a part. We need to find the first context in which there",
				     1,
				     1128)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1010 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" is a plan for goal and back up one.",
				     1,
				     1201)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1238 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    pickup,
			    [goal],
			    
			    [ let,
			      
			      [ 
				[ context,
				  ['ob$get', goal, [quote, 'activation-context']]
				],
				
				[ 'top-level-goal',
				  ['ob$get', goal, [quote, 'top-level-goal']]
				]
			      ],
			      ['ob$set', context, [quote, children], []],
			      ['set-next-context', 'top-level-goal', context],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("DAYDREAMER ~A pickup"),
				goal
			      ],
			      ['daydreamer-control0']
			    ]
			  ]).

% annotating U::PICKUP 
wl: lambda_def(defun,
	      u_pickup,
	      f_u_pickup,
	      [u_goal],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_context,
		      [u_ob_c36_get, u_goal, [quote, u_activation_context]]
		    ],
		    
		    [ u_top_level_goal,
		      [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]
		    ]
		  ],
		  [u_ob_c36_set, u_context, [quote, u_children], []],
		  [u_set_next_context, u_top_level_goal, u_context],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('D'),
			       #\('A'),
			       #\('Y'),
			       #\('D'),
			       #\('R'),
			       #\('E'),
			       #\('A'),
			       #\('M'),
			       #\('E'),
			       #\('R'),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(p),
			       #\(i),
			       #\(c),
			       #\(k),
			       #\(u),
			       #\(p)
			     ]),
		    u_goal
		  ],
		  [u_daydreamer_control0]
		]
	      ]).


% annotating U::PICKUP 
wl: arglist_info(u_pickup,
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

% annotating U::PICKUP 
wl: init_args(exact_only, u_pickup).


% annotating U::PICKUP 
f_u_pickup(Goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ob_c36_get(Goal_Param, u_activation_context, Context_Init),
	f_u_ob_c36_get(Goal_Param, u_top_level_goal, Top_level_goal_Init),
	LEnv=[[bv(u_context, Context_Init), bv(u_top_level_goal, Top_level_goal_Init)]|Env],
	get_var(LEnv, u_context, Context_Get),
	f_u_ob_c36_set(Context_Get, u_children, [], C36_set_Ret),
	get_var(LEnv, u_context, Context_Get21),
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_set_next_context(Top_level_goal_Get,
			     Context_Get21,
			     Next_context_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('D'),
				       #\('A'),
				       #\('Y'),
				       #\('D'),
				       #\('R'),
				       #\('E'),
				       #\('A'),
				       #\('M'),
				       #\('E'),
				       #\('R'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(p),
				       #\(i),
				       #\(c),
				       #\(k),
				       #\(u),
				       #\(p)
				     ]),
			    u_goal
			  ],
			  Roman_nl_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	LetResult=Daydreamer_control0_Ret,
	LetResult=FnResult.
:- set_opv(f_u_pickup, classof, claz_function),
   set_opv(u_pickup, compile_as, kw_function),
   set_opv(u_pickup, function, f_u_pickup),
   DefunResult=u_pickup.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1553 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'pickup-in',
			    [goal, context],
			    
			    [ let,
			      
			      [ 
				[ 'top-level-goal',
				  ['ob$get', goal, [quote, 'top-level-goal']]
				]
			      ],
			      ['ob$set', context, [quote, children], []],
			      ['set-next-context', 'top-level-goal', context],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("DAYDREAMER ~A pickup"),
				goal
			      ],
			      ['daydreamer-control0']
			    ]
			  ]).

% annotating U::PICKUP-IN 
wl: lambda_def(defun,
	      u_pickup_in,
	      f_u_pickup_in,
	      [u_goal, u_context],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_top_level_goal,
		      [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]
		    ]
		  ],
		  [u_ob_c36_set, u_context, [quote, u_children], []],
		  [u_set_next_context, u_top_level_goal, u_context],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('D'),
			       #\('A'),
			       #\('Y'),
			       #\('D'),
			       #\('R'),
			       #\('E'),
			       #\('A'),
			       #\('M'),
			       #\('E'),
			       #\('R'),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(p),
			       #\(i),
			       #\(c),
			       #\(k),
			       #\(u),
			       #\(p)
			     ]),
		    u_goal
		  ],
		  [u_daydreamer_control0]
		]
	      ]).


% annotating U::PICKUP-IN 
wl: arglist_info(u_pickup_in,
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

% annotating U::PICKUP-IN 
wl: init_args(exact_only, u_pickup_in).


% annotating U::PICKUP-IN 
f_u_pickup_in(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_get(Goal_Param, u_top_level_goal, Top_level_goal_Init),
	LEnv=[[bv(u_top_level_goal, Top_level_goal_Init)]|Env],
	f_u_ob_c36_set(Context_Param, u_children, [], C36_set_Ret),
	get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	f_u_set_next_context(Top_level_goal_Get,
			     Context_Param,
			     Next_context_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('D'),
				       #\('A'),
				       #\('Y'),
				       #\('D'),
				       #\('R'),
				       #\('E'),
				       #\('A'),
				       #\('M'),
				       #\('E'),
				       #\('R'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(p),
				       #\(i),
				       #\(c),
				       #\(k),
				       #\(u),
				       #\(p)
				     ]),
			    u_goal
			  ],
			  Roman_nl_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	LetResult=Daydreamer_control0_Ret,
	LetResult=FnResult.
:- set_opv(f_u_pickup_in, classof, claz_function),
   set_opv(u_pickup_in, compile_as, kw_function),
   set_opv(u_pickup_in, function, f_u_pickup_in),
   DefunResult=u_pickup_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1827 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    resume,
			    [goal],
			    ['change-tlg-status', goal, [quote, runable]],
			    ['daydreamer-control0']
			  ]).

% annotating U::RESUME 
wl: lambda_def(defun,
	      u_resume,
	      f_u_resume,
	      [u_goal],
	      
	      [ [u_change_tlg_status, u_goal, [quote, u_runable]],
		[u_daydreamer_control0]
	      ]).


% annotating U::RESUME 
wl: arglist_info(u_resume,
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

% annotating U::RESUME 
wl: init_args(exact_only, u_resume).


% annotating U::RESUME 
f_u_resume(Goal_Param, FnResult) :-
	f_u_change_tlg_status(Goal_Param, u_runable, Runable),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_resume, classof, claz_function),
   set_opv(u_resume, compile_as, kw_function),
   set_opv(u_resume, function, f_u_resume),
   DefunResult=u_resume.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:1910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'resume-infs',
			    [],
			    
			    [ yloop,
			      [yfor, fact, in, '*needs*'],
			      
			      [ ydo,
				['cx$touch-fact', '*reality-lookahead*', fact]
			      ]
			    ],
			    
			    [ 'run-inferences',
			      '*reality-lookahead*',
			      [],
			      '*me-belief-path*'
			    ],
			    ['set-state', [quote, performance]],
			    ['daydreamer-control0']
			  ]).

% annotating U::RESUME-INFS 
wl: lambda_def(defun,
	      u_resume_infs,
	      f_u_resume_infs,
	      [],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_fact, u_in, u_xx_needs_xx],
		  
		  [ u_ydo,
		    [u_cx_c36_touch_fact, u_xx_reality_lookahead_xx, u_fact]
		  ]
		],
		
		[ u_run_inferences,
		  u_xx_reality_lookahead_xx,
		  [],
		  u_xx_me_belief_path_xx
		],
		[u_set_state, [quote, u_performance]],
		[u_daydreamer_control0]
	      ]).


% annotating U::RESUME-INFS 
wl: arglist_info(u_resume_infs,
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

% annotating U::RESUME-INFS 
wl: init_args(exact_only, u_resume_infs).


% annotating U::RESUME-INFS 
f_u_resume_infs(FnResult) :-
	Env=[],
	f_u_yloop(
		  [ [u_yfor, u_fact, u_in, u_xx_needs_xx],
		    
		    [ u_ydo,
		      [u_cx_c36_touch_fact, u_xx_reality_lookahead_xx, u_fact]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	f_u_run_inferences(Xx_reality_lookahead_xx_Get,
			   [],
			   Xx_me_belief_path_xx_Get,
			   Run_inferences_Ret),
	f_u_set_state(u_performance, Set_state_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_resume_infs, classof, claz_function),
   set_opv(u_resume_infs, compile_as, kw_function),
   set_opv(u_resume_infs, function, f_u_resume_infs),
   DefunResult=u_resume_infs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'resume-enter',
			    [],
			    
			    [ 'enter-concepts',
			      '*reality-lookahead*',
			      '*me-belief-path*'
			    ],
			    
			    [ 'run-inferences',
			      '*reality-lookahead*',
			      [],
			      '*me-belief-path*'
			    ],
			    ['set-state', [quote, performance]],
			    ['daydreamer-control0']
			  ]).

% annotating U::RESUME-ENTER 
wl: lambda_def(defun,
	      u_resume_enter,
	      f_u_resume_enter,
	      [],
	      
	      [ 
		[ u_enter_concepts,
		  u_xx_reality_lookahead_xx,
		  u_xx_me_belief_path_xx
		],
		
		[ u_run_inferences,
		  u_xx_reality_lookahead_xx,
		  [],
		  u_xx_me_belief_path_xx
		],
		[u_set_state, [quote, u_performance]],
		[u_daydreamer_control0]
	      ]).


% annotating U::RESUME-ENTER 
wl: arglist_info(u_resume_enter,
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

% annotating U::RESUME-ENTER 
wl: init_args(exact_only, u_resume_enter).


% annotating U::RESUME-ENTER 
f_u_resume_enter(FnResult) :-
	Env=[],
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	f_u_enter_concepts(Xx_reality_lookahead_xx_Get,
			   Xx_me_belief_path_xx_Get,
			   Enter_concepts_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get13),
	get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get12),
	f_u_run_inferences(Xx_reality_lookahead_xx_Get12,
			   [],
			   Xx_me_belief_path_xx_Get13,
			   Run_inferences_Ret),
	f_u_set_state(u_performance, Set_state_Ret),
	f_u_daydreamer_control0(Daydreamer_control0_Ret),
	Daydreamer_control0_Ret=FnResult.
:- set_opv(f_u_resume_enter, classof, claz_function),
   set_opv(u_resume_enter, compile_as, kw_function),
   set_opv(u_resume_enter, function, f_u_resume_enter),
   DefunResult=u_resume_enter.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2325 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'update-initial',
			    [cx],
			    
			    [ yloop,
			      [yfor, f, in, '*initial-facts*'],
			      [ydo, ['cx$assert', f, cx]]
			    ]
			  ]).

% annotating U::UPDATE-INITIAL 
wl: lambda_def(defun,
	      u_update_initial,
	      f_u_update_initial,
	      [u_cx],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_f, u_in, u_xx_initial_facts_xx],
		  [u_ydo, [u_cx_c36_assert, u_f, u_cx]]
		]
	      ]).


% annotating U::UPDATE-INITIAL 
wl: arglist_info(u_update_initial,
		[u_cx],
		[Cx_Param],
		arginfo{ all:[u_cx],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_cx],
			 opt:0,
			 req:[u_cx],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::UPDATE-INITIAL 
wl: init_args(exact_only, u_update_initial).


% annotating U::UPDATE-INITIAL 
f_u_update_initial(Cx_Param, FnResult) :-
	Env=[bv(u_cx, Cx_Param)],
	f_u_yloop(
		  [ [u_yfor, u_f, u_in, u_xx_initial_facts_xx],
		    [u_ydo, [u_cx_c36_assert, u_f, u_cx]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_update_initial, classof, claz_function),
   set_opv(u_update_initial, compile_as, kw_function),
   set_opv(u_update_initial, function, f_u_update_initial),
   DefunResult=u_update_initial.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2424 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'new-daydreamer',
			    [],
			    [setq, '*first-time-initialize?*', t],
			    [daydreamer]
			  ]).

% annotating U::NEW-DAYDREAMER 
wl: lambda_def(defun,
	      u_new_daydreamer,
	      f_u_new_daydreamer,
	      [],
	      [[setq, u_xx_first_time_initialize_c63_xx, t], [u_daydreamer]]).


% annotating U::NEW-DAYDREAMER 
wl: arglist_info(u_new_daydreamer,
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

% annotating U::NEW-DAYDREAMER 
wl: init_args(exact_only, u_new_daydreamer).


% annotating U::NEW-DAYDREAMER 
f_u_new_daydreamer(FnResult) :-
	Env=[],
	set_var(Env, setq, u_xx_first_time_initialize_c63_xx, t),
	f_u_daydreamer(Daydreamer_Ret),
	Daydreamer_Ret=FnResult.
:- set_opv(f_u_new_daydreamer, classof, claz_function),
   set_opv(u_new_daydreamer, compile_as, kw_function),
   set_opv(u_new_daydreamer, function, f_u_new_daydreamer),
   DefunResult=u_new_daydreamer.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2502 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*state*', []]).
:- set_var(TLEnv3, setq, u_xx_state_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2522 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'set-state',
			    [state],
			    
			    [ if,
			      
			      [ not,
				
				[ 'memq?',
				  state,
				  [quote, [suspended, performance, daydreaming]]
				]
			      ],
			      
			      [ 'set-state',
				
				[ error,
				  '$STRING'("~A is not a valid state."),
				  state
				]
			      ],
			      
			      [ if,
				['neq?', state, '*state*'],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("State changes from ~A to ~A"),
				    '*state*',
				    state
				  ],
				  [setq, '*state*', state]
				]
			      ]
			    ]
			  ]).

% annotating U::SET-STATE 
wl: lambda_def(defun,
	      u_set_state,
	      f_u_set_state,
	      [u_state],
	      
	      [ 
		[ if,
		  
		  [ not,
		    
		    [ u_memq_c63,
		      u_state,
		      [quote, [u_suspended, u_performance, u_daydreaming]]
		    ]
		  ],
		  
		  [ u_set_state,
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('A'),
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
				 #\(v),
				 #\(a),
				 #\(l),
				 #\(i),
				 #\(d),
				 #\(' '),
				 #\(s),
				 #\(t),
				 #\(a),
				 #\(t),
				 #\(e),
				 #\('.')
			       ]),
		      u_state
		    ]
		  ],
		  
		  [ if,
		    [u_neq_c63, u_state, u_xx_state_xx],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('S'),
				   #\(t),
				   #\(a),
				   #\(t),
				   #\(e),
				   #\(' '),
				   #\(c),
				   #\(h),
				   #\(a),
				   #\(n),
				   #\(g),
				   #\(e),
				   #\(s),
				   #\(' '),
				   #\(f),
				   #\(r),
				   #\(o),
				   #\(m),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(t),
				   #\(o),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_xx_state_xx,
			u_state
		      ],
		      [setq, u_xx_state_xx, u_state]
		    ]
		  ]
		]
	      ]).


% annotating U::SET-STATE 
wl: arglist_info(u_set_state,
		[u_state],
		[State_Get18],
		arginfo{ all:[u_state],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_state],
			 opt:0,
			 req:[u_state],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-STATE 
wl: init_args(exact_only, u_set_state).


% annotating U::SET-STATE 
f_u_set_state(State_Get18, FnResult) :-
	Env=[bv(u_state, State_Get18)],
	f_u_memq_c63(u_state,
		     [quote, [u_suspended, u_performance, u_daydreaming]],
		     PredArgResult),
	(   PredArgResult==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\(~),
				  #\('A'),
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
				  #\(v),
				  #\(a),
				  #\(l),
				  #\(i),
				  #\(d),
				  #\(' '),
				  #\(s),
				  #\(t),
				  #\(a),
				  #\(t),
				  #\(e),
				  #\('.')
				]),
		       State_Get18
		     ],
		     Set_state_Param),
	    f_u_set_state(Set_state_Param, TrueResult20),
	    FnResult=TrueResult20
	;   f_u_neq_c63(u_state, u_xx_state_xx, IFTEST16),
	    (   IFTEST16\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('S'),
					       #\(t),
					       #\(a),
					       #\(t),
					       #\(e),
					       #\(' '),
					       #\(c),
					       #\(h),
					       #\(a),
					       #\(n),
					       #\(g),
					       #\(e),
					       #\(s),
					       #\(' '),
					       #\(f),
					       #\(r),
					       #\(o),
					       #\(m),
					       #\(' '),
					       #\(~),
					       #\('A'),
					       #\(' '),
					       #\(t),
					       #\(o),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_xx_state_xx,
				    u_state
				  ],
				  Roman_nl_Ret),
		set_var(Env, u_xx_state_xx, State_Get18),
		FnResult=State_Get18
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_set_state, classof, claz_function),
   set_opv(u_set_state, compile_as, kw_function),
   set_opv(u_set_state, function, f_u_set_state),
   DefunResult=u_set_state.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'performance-mode?',
			    [],
			    ['eq?', '*state*', [quote, performance]]
			  ]).

% annotating U::PERFORMANCE-MODE? 
wl: lambda_def(defun,
	      u_performance_mode_c63,
	      f_u_performance_mode_c63,
	      [],
	      [[u_eq_c63, u_xx_state_xx, [quote, u_performance]]]).


% annotating U::PERFORMANCE-MODE? 
wl: arglist_info(u_performance_mode_c63,
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

% annotating U::PERFORMANCE-MODE? 
wl: init_args(exact_only, u_performance_mode_c63).


% annotating U::PERFORMANCE-MODE? 
f_u_performance_mode_c63(FnResult) :-
	Env=[],
	f_u_eq_c63(u_xx_state_xx, [quote, u_performance], Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_performance_mode_c63, classof, claz_function),
   set_opv(u_performance_mode_c63, compile_as, kw_function),
   set_opv(u_performance_mode_c63, function, f_u_performance_mode_c63),
   DefunResult=u_performance_mode_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2937 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'daydreaming-mode?',
			    [],
			    ['eq?', '*state*', [quote, daydreaming]]
			  ]).

% annotating U::DAYDREAMING-MODE? 
wl: lambda_def(defun,
	      u_daydreaming_mode_c63,
	      f_u_daydreaming_mode_c63,
	      [],
	      [[u_eq_c63, u_xx_state_xx, [quote, u_daydreaming]]]).


% annotating U::DAYDREAMING-MODE? 
wl: arglist_info(u_daydreaming_mode_c63,
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

% annotating U::DAYDREAMING-MODE? 
wl: init_args(exact_only, u_daydreaming_mode_c63).


% annotating U::DAYDREAMING-MODE? 
f_u_daydreaming_mode_c63(FnResult) :-
	Env=[],
	f_u_eq_c63(u_xx_state_xx, [quote, u_daydreaming], Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_daydreaming_mode_c63, classof, claz_function),
   set_opv(u_daydreaming_mode_c63, compile_as, kw_function),
   set_opv(u_daydreaming_mode_c63, function, f_u_daydreaming_mode_c63),
   DefunResult=u_daydreaming_mode_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:2996 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*reality*', []]).
:- set_var(TLEnv3, setq, u_xx_reality_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3017 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*reality-lookahead*', []]).
:- set_var(TLEnv3, setq, u_xx_reality_lookahead_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3048 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*primal-reality*', []]).
:- set_var(TLEnv3, setq, u_xx_primal_reality_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3076 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*first-time-initialize?*', t]).
:- set_var(TLEnv3, setq, u_xx_first_time_initialize_c63_xx, t).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3111 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*initial-reality*', []]).
:- set_var(TLEnv3, setq, u_xx_initial_reality_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3141 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*entered-concepts*', []]).
:- set_var(TLEnv3, setq, u_xx_entered_concepts_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3172 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'daydreamer-initialize',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Initialize DAYDREAMER")
			    ],
			    
			    [ if,
			      '*first-time-initialize?*',
			      ['first-time-initialize']
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("~%Creating initial reality context...")
			    ],
			    [setq, '*top-level-goals*', []],
			    [setq, '*top-level-goal*', []],
			    [setq, '*emotions*', []],
			    
			    [ setq,
			      '*reality*',
			      ['cx$sprout', '*primal-reality*']
			    ],
			    [setq, '*initial-reality*', '*reality*'],
			    [setq, '*entered-concepts*', []],
			    [setq, '*reality-lookahead*', '*reality*'],
			    ['need-init', '*reality*'],
			    ['epmem-initialize']
			  ]).

% annotating U::DAYDREAMER-INITIALIZE 
wl: lambda_def(defun,
	      u_daydreamer_initialize,
	      f_u_daydreamer_initialize,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('I'),
			     #\(n),
			     #\(i),
			     #\(t),
			     #\(i),
			     #\(a),
			     #\(l),
			     #\(i),
			     #\(z),
			     #\(e),
			     #\(' '),
			     #\('D'),
			     #\('A'),
			     #\('Y'),
			     #\('D'),
			     #\('R'),
			     #\('E'),
			     #\('A'),
			     #\('M'),
			     #\('E'),
			     #\('R')
			   ])
		],
		
		[ if,
		  u_xx_first_time_initialize_c63_xx,
		  [u_first_time_initialize]
		],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\('%'),
			     #\('C'),
			     #\(r),
			     #\(e),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(i),
			     #\(t),
			     #\(i),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(r),
			     #\(e),
			     #\(a),
			     #\(l),
			     #\(i),
			     #\(t),
			     #\(y),
			     #\(' '),
			     #\(c),
			     #\(o),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(x),
			     #\(t),
			     #\('.'),
			     #\('.'),
			     #\('.')
			   ])
		],
		[setq, u_xx_top_level_goals_xx, []],
		[setq, u_xx_top_level_goal_xx, []],
		[setq, u_xx_emotions_xx, []],
		
		[ setq,
		  u_xx_reality_xx,
		  [u_cx_c36_sprout, u_xx_primal_reality_xx]
		],
		[setq, u_xx_initial_reality_xx, u_xx_reality_xx],
		[setq, u_xx_entered_concepts_xx, []],
		[setq, u_xx_reality_lookahead_xx, u_xx_reality_xx],
		[u_need_init, u_xx_reality_xx],
		[u_epmem_initialize]
	      ]).


% annotating U::DAYDREAMER-INITIALIZE 
wl: arglist_info(u_daydreamer_initialize,
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

% annotating U::DAYDREAMER-INITIALIZE 
wl: init_args(exact_only, u_daydreamer_initialize).


% annotating U::DAYDREAMER-INITIALIZE 
f_u_daydreamer_initialize(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(i),
				       #\(t),
				       #\(i),
				       #\(a),
				       #\(l),
				       #\(i),
				       #\(z),
				       #\(e),
				       #\(' '),
				       #\('D'),
				       #\('A'),
				       #\('Y'),
				       #\('D'),
				       #\('R'),
				       #\('E'),
				       #\('A'),
				       #\('M'),
				       #\('E'),
				       #\('R')
				     ])
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_first_time_initialize_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_first_time_initialize(TrueResult),
	    _41620=TrueResult
	;   _41620=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('%'),
				       #\('C'),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(i),
				       #\(t),
				       #\(i),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(l),
				       #\(i),
				       #\(t),
				       #\(y),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(x),
				       #\(t),
				       #\('.'),
				       #\('.'),
				       #\('.')
				     ])
			  ],
			  Roman_nl_Ret22),
	set_var(Env, setq, u_xx_top_level_goals_xx, []),
	set_var(Env, setq, u_xx_top_level_goal_xx, []),
	set_var(Env, setq, u_xx_emotions_xx, []),
	get_var(Env, u_xx_primal_reality_xx, Xx_primal_reality_xx_Get),
	f_u_cx_c36_sprout(Xx_primal_reality_xx_Get, Xx_reality_xx),
	set_var(Env, u_xx_reality_xx, Xx_reality_xx),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get16),
	set_var(Env, u_xx_initial_reality_xx, Xx_reality_xx_Get16),
	set_var(Env, setq, u_xx_entered_concepts_xx, []),
	set_var(Env, u_xx_reality_lookahead_xx, Xx_reality_xx_Get16),
	f_u_need_init(Xx_reality_xx_Get16, Need_init_Ret),
	f_u_epmem_initialize(Epmem_initialize_Ret),
	Epmem_initialize_Ret=FnResult.
:- set_opv(f_u_daydreamer_initialize, classof, claz_function),
   set_opv(u_daydreamer_initialize, compile_as, kw_function),
   set_opv(u_daydreamer_initialize, function, f_u_daydreamer_initialize),
   DefunResult=u_daydreamer_initialize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3172 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" for debugging", 38, 3577)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3710 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dbg-info',
			    [],
			    ['cx$tree-print', '*initial-reality*']
			  ]).

% annotating U::DBG-INFO 
wl: lambda_def(defun,
	      u_dbg_info,
	      f_u_dbg_info,
	      [],
	      [[u_cx_c36_tree_print, u_xx_initial_reality_xx]]).


% annotating U::DBG-INFO 
wl: arglist_info(u_dbg_info,
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

% annotating U::DBG-INFO 
wl: init_args(exact_only, u_dbg_info).


% annotating U::DBG-INFO 
f_u_dbg_info(FnResult) :-
	Env=[],
	get_var(Env, u_xx_initial_reality_xx, Xx_initial_reality_xx_Get),
	f_u_cx_c36_tree_print(Xx_initial_reality_xx_Get, Tree_print_Ret),
	Tree_print_Ret=FnResult.
:- set_opv(f_u_dbg_info, classof, claz_function),
   set_opv(u_dbg_info, compile_as, kw_function),
   set_opv(u_dbg_info, function, f_u_dbg_info),
   DefunResult=u_dbg_info.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3767 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'first-time-initialize',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Performing first-time initialization")
			    ],
			    ['no-gen', ['initialize-primal-reality']],
			    [setq, '*first-time-initialize?*', []]
			  ]).

% annotating U::FIRST-TIME-INITIALIZE 
wl: lambda_def(defun,
	      u_first_time_initialize,
	      f_u_first_time_initialize,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(e),
			     #\(r),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(m),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(f),
			     #\(i),
			     #\(r),
			     #\(s),
			     #\(t),
			     #\(-),
			     #\(t),
			     #\(i),
			     #\(m),
			     #\(e),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(i),
			     #\(t),
			     #\(i),
			     #\(a),
			     #\(l),
			     #\(i),
			     #\(z),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n)
			   ])
		],
		[u_no_gen, [u_initialize_primal_reality]],
		[setq, u_xx_first_time_initialize_c63_xx, []]
	      ]).


% annotating U::FIRST-TIME-INITIALIZE 
wl: arglist_info(u_first_time_initialize,
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

% annotating U::FIRST-TIME-INITIALIZE 
wl: init_args(exact_only, u_first_time_initialize).


% annotating U::FIRST-TIME-INITIALIZE 
f_u_first_time_initialize(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(e),
				       #\(r),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(f),
				       #\(i),
				       #\(r),
				       #\(s),
				       #\(t),
				       #\(-),
				       #\(t),
				       #\(i),
				       #\(m),
				       #\(e),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(i),
				       #\(t),
				       #\(i),
				       #\(a),
				       #\(l),
				       #\(i),
				       #\(z),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_no_gen([[u_initialize_primal_reality]], No_gen_Ret),
	set_var(Env, setq, u_xx_first_time_initialize_c63_xx, []),
	[]=FnResult.
:- set_opv(f_u_first_time_initialize, classof, claz_function),
   set_opv(u_first_time_initialize, compile_as, kw_function),
   set_opv(u_first_time_initialize, function, f_u_first_time_initialize),
   DefunResult=u_first_time_initialize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3951 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*initial-facts*', []]).
:- set_var(TLEnv3, setq, u_xx_initial_facts_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:3979 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'loadable-subsets?',
			    [subsets],
			    
			    [ or,
			      ['nil?', subsets],
			      
			      [ 'any?',
				[lambda, [x], ['memq?', x, '*subsets*']],
				subsets
			      ]
			    ]
			  ]).

% annotating U::LOADABLE-SUBSETS? 
wl: lambda_def(defun,
	      u_loadable_subsets_c63,
	      f_u_loadable_subsets_c63,
	      [u_subsets],
	      
	      [ 
		[ or,
		  [u_nil_c63, u_subsets],
		  
		  [ u_any_c63,
		    [lambda, [u_x], [u_memq_c63, u_x, u_xx_subsets_xx]],
		    u_subsets
		  ]
		]
	      ]).


% annotating U::LOADABLE-SUBSETS? 
wl: arglist_info(u_loadable_subsets_c63,
		[u_subsets],
		[Subsets_Param],
		arginfo{ all:[u_subsets],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subsets],
			 opt:0,
			 req:[u_subsets],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::LOADABLE-SUBSETS? 
wl: init_args(exact_only, u_loadable_subsets_c63).


% annotating U::LOADABLE-SUBSETS? 
f_u_loadable_subsets_c63(Subsets_Param, FnResult) :-
	Env=[bv(u_subsets, Subsets_Param)],
	(   f_u_nil_c63(u_subsets, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_any_c63([lambda, [u_x], [u_memq_c63, u_x, u_xx_subsets_xx]],
			u_subsets,
			Subsets),
	    FnResult=Subsets
	).
:- set_opv(f_u_loadable_subsets_c63, classof, claz_function),
   set_opv(u_loadable_subsets_c63, compile_as, kw_function),
   set_opv(u_loadable_subsets_c63, function, f_u_loadable_subsets_c63),
   DefunResult=u_loadable_subsets_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4092 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'initialize-primal-reality',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Creating primal reality...")
			    ],
			    [setq, '*primal-reality*', ['cx$create']],
			    
			    [ yloop,
			      [yfor, assertion, in, '*initial-facts*'],
			      [ydo, ['cx$assert', '*primal-reality*', assertion]]
			    ]
			  ]).

% annotating U::INITIALIZE-PRIMAL-REALITY 
wl: lambda_def(defun,
	      u_initialize_primal_reality,
	      f_u_initialize_primal_reality,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('C'),
			     #\(r),
			     #\(e),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(p),
			     #\(r),
			     #\(i),
			     #\(m),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(r),
			     #\(e),
			     #\(a),
			     #\(l),
			     #\(i),
			     #\(t),
			     #\(y),
			     #\('.'),
			     #\('.'),
			     #\('.')
			   ])
		],
		[setq, u_xx_primal_reality_xx, [u_cx_c36_create]],
		
		[ u_yloop,
		  [u_yfor, u_assertion, u_in, u_xx_initial_facts_xx],
		  [u_ydo, [u_cx_c36_assert, u_xx_primal_reality_xx, u_assertion]]
		]
	      ]).


% annotating U::INITIALIZE-PRIMAL-REALITY 
wl: arglist_info(u_initialize_primal_reality,
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

% annotating U::INITIALIZE-PRIMAL-REALITY 
wl: init_args(exact_only, u_initialize_primal_reality).


% annotating U::INITIALIZE-PRIMAL-REALITY 
f_u_initialize_primal_reality(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('C'),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(p),
				       #\(r),
				       #\(i),
				       #\(m),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(l),
				       #\(i),
				       #\(t),
				       #\(y),
				       #\('.'),
				       #\('.'),
				       #\('.')
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_cx_c36_create(Xx_primal_reality_xx),
	set_var(Env, u_xx_primal_reality_xx, Xx_primal_reality_xx),
	f_u_yloop(
		  [ [u_yfor, u_assertion, u_in, u_xx_initial_facts_xx],
		    
		    [ u_ydo,
		      [u_cx_c36_assert, u_xx_primal_reality_xx, u_assertion]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_initialize_primal_reality, classof, claz_function),
   set_opv(u_initialize_primal_reality, compile_as, kw_function),
   set_opv(u_initialize_primal_reality, function, f_u_initialize_primal_reality),
   DefunResult=u_initialize_primal_reality.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4092 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4332)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4092 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level control loop: repeatedly select the most highly motivated",
				     1,
				     4334)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4092 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" available top-level goal and run that goal.",
				     1,
				     4404)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4092 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4450)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:4451 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'daydreamer-control0',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Running emotion-driven control loop...")
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				[candidates, []],
				[strikes, 0],
				['top-level-goal', []]
			      ],
			      [yuntil, [>, strikes, 2]],
			      
			      [ ydo,
				['need-decay'],
				['emotion-decay'],
				
				[ setq,
				  candidates,
				  ['most-highly-motivated-goals']
				],
				[format, ['standard-output'], '$STRING'(":")],
				['force-output', ['standard-output']],
				
				[ cond,
				  
				  [ ['null?', candidates],
				    
				    [ if,
				      ['performance-mode?'],
				      
				      [ progn,
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("No more goals to run; switching to daydreaming mode")
					],
					[setq, strikes, [+, 1, strikes]],
					['set-state', [quote, daydreaming]]
				      ],
				      
				      [ progn,
					
					[ if,
					  
					  [ 'null?',
					    ['environmental-object-input']
					  ],
					  
					  [ progn,
					    
					    [ 'ndbg-roman-nl',
					      '*gate-dbg*',
					      rule,
					      '$STRING'("No more goals to run; switching to performance mode")
					    ],
					    [setq, strikes, [+, 1, strikes]],
					    
					    [ yloop,
					      
					      [ yfor,
						goal,
						in,
						'*top-level-goals*'
					      ],
					      
					      [ ydo,
						
						[ if,
						  
						  [ and,
						    
						    [ 'eq?',
						      [quote, waiting],
						      
						      [ 'ob$get',
							goal,
							[quote, status]
						      ]
						    ],
						    
						    [ 'eq?',
						      [quote, real],
						      
						      [ 'ob$get',
							goal,
							
							[ quote,
							  'planning-type'
							]
						      ]
						    ]
						  ],
						  
						  [ progn,
						    
						    [ 'change-tlg-status',
						      goal,
						      [quote, runable]
						    ]
						  ]
						]
					      ]
					    ],
					    ['set-state', [quote, performance]]
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ ['memq?', 'top-level-goal', candidates],
				    [setq, strikes, 0],
				    ['daydreamer-control1', 'top-level-goal']
				  ],
				  
				  [ [=, [length, candidates], 1],
				    [setq, strikes, 0],
				    [setq, 'top-level-goal', [car, candidates]],
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Switching to new top-level goal")
				    ],
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'(" ~A"),
				      'top-level-goal'
				    ],
				    ['ndbg-newline', '*gate-dbg*', rule],
				    ['daydreamer-control1', 'top-level-goal']
				  ],
				  
				  [ else,
				    [setq, strikes, 0],
				    
				    [ setq,
				      'top-level-goal',
				      ['random-element', candidates]
				    ],
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Switching to new top-level goal\n                          (broke tie)")
				    ],
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'(" ~A"),
				      'top-level-goal'
				    ],
				    ['ndbg-newline', '*gate-dbg*', rule],
				    ['daydreamer-control1', 'top-level-goal']
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::DAYDREAMER-CONTROL0 
wl: lambda_def(defun,
	      u_daydreamer_control0,
	      f_u_daydreamer_control0,
	      [],
	      
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
			     #\(e),
			     #\(m),
			     #\(o),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(-),
			     #\(d),
			     #\(r),
			     #\(i),
			     #\(v),
			     #\(e),
			     #\(n),
			     #\(' '),
			     #\(c),
			     #\(o),
			     #\(n),
			     #\(t),
			     #\(r),
			     #\(o),
			     #\(l),
			     #\(' '),
			     #\(l),
			     #\(o),
			     #\(o),
			     #\(p),
			     #\('.'),
			     #\('.'),
			     #\('.')
			   ])
		],
		
		[ u_yloop,
		  
		  [ u_initial,
		    [u_candidates, []],
		    [u_strikes, 0],
		    [u_top_level_goal, []]
		  ],
		  [u_yuntil, [>, u_strikes, 2]],
		  
		  [ u_ydo,
		    [u_need_decay],
		    [u_emotion_decay],
		    [setq, u_candidates, [u_most_highly_motivated_goals]],
		    
		    [ format,
		      [u_standard_output],
		      '$ARRAY'([*], claz_base_character, [#\(:)])
		    ],
		    [force_output, [u_standard_output]],
		    
		    [ cond,
		      
		      [ [u_null_c63, u_candidates],
			
			[ if,
			  [u_performance_mode_c63],
			  
			  [ progn,
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('N'),
					 #\(o),
					 #\(' '),
					 #\(m),
					 #\(o),
					 #\(r),
					 #\(e),
					 #\(' '),
					 #\(g),
					 #\(o),
					 #\(a),
					 #\(l),
					 #\(s),
					 #\(' '),
					 #\(t),
					 #\(o),
					 #\(' '),
					 #\(r),
					 #\(u),
					 #\(n),
					 #\(;),
					 #\(' '),
					 #\(s),
					 #\(w),
					 #\(i),
					 #\(t),
					 #\(c),
					 #\(h),
					 #\(i),
					 #\(n),
					 #\(g),
					 #\(' '),
					 #\(t),
					 #\(o),
					 #\(' '),
					 #\(d),
					 #\(a),
					 #\(y),
					 #\(d),
					 #\(r),
					 #\(e),
					 #\(a),
					 #\(m),
					 #\(i),
					 #\(n),
					 #\(g),
					 #\(' '),
					 #\(m),
					 #\(o),
					 #\(d),
					 #\(e)
				       ])
			    ],
			    [setq, u_strikes, [+, 1, u_strikes]],
			    [u_set_state, [quote, u_daydreaming]]
			  ],
			  
			  [ progn,
			    
			    [ if,
			      [u_null_c63, [u_environmental_object_input]],
			      
			      [ progn,
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('N'),
					     #\(o),
					     #\(' '),
					     #\(m),
					     #\(o),
					     #\(r),
					     #\(e),
					     #\(' '),
					     #\(g),
					     #\(o),
					     #\(a),
					     #\(l),
					     #\(s),
					     #\(' '),
					     #\(t),
					     #\(o),
					     #\(' '),
					     #\(r),
					     #\(u),
					     #\(n),
					     #\(;),
					     #\(' '),
					     #\(s),
					     #\(w),
					     #\(i),
					     #\(t),
					     #\(c),
					     #\(h),
					     #\(i),
					     #\(n),
					     #\(g),
					     #\(' '),
					     #\(t),
					     #\(o),
					     #\(' '),
					     #\(p),
					     #\(e),
					     #\(r),
					     #\(f),
					     #\(o),
					     #\(r),
					     #\(m),
					     #\(a),
					     #\(n),
					     #\(c),
					     #\(e),
					     #\(' '),
					     #\(m),
					     #\(o),
					     #\(d),
					     #\(e)
					   ])
				],
				[setq, u_strikes, [+, 1, u_strikes]],
				
				[ u_yloop,
				  [u_yfor, u_goal, u_in, u_xx_top_level_goals_xx],
				  
				  [ u_ydo,
				    
				    [ if,
				      
				      [ and,
					
					[ u_eq_c63,
					  [quote, u_waiting],
					  
					  [ u_ob_c36_get,
					    u_goal,
					    [quote, u_status]
					  ]
					],
					
					[ u_eq_c63,
					  [quote, real],
					  
					  [ u_ob_c36_get,
					    u_goal,
					    [quote, u_planning_type]
					  ]
					]
				      ],
				      
				      [ progn,
					
					[ u_change_tlg_status,
					  u_goal,
					  [quote, u_runable]
					]
				      ]
				    ]
				  ]
				],
				[u_set_state, [quote, u_performance]]
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ [u_memq_c63, u_top_level_goal, u_candidates],
			[setq, u_strikes, 0],
			[u_daydreamer_control1, u_top_level_goal]
		      ],
		      
		      [ [=, [length, u_candidates], 1],
			[setq, u_strikes, 0],
			[setq, u_top_level_goal, [car, u_candidates]],
			
			[ u_ndbg_roman,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('S'),
				     #\(w),
				     #\(i),
				     #\(t),
				     #\(c),
				     #\(h),
				     #\(i),
				     #\(n),
				     #\(g),
				     #\(' '),
				     #\(t),
				     #\(o),
				     #\(' '),
				     #\(n),
				     #\(e),
				     #\(w),
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
				   [#\(' '), #\(~), #\('A')]),
			  u_top_level_goal
			],
			[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			[u_daydreamer_control1, u_top_level_goal]
		      ],
		      
		      [ u_else,
			[setq, u_strikes, 0],
			
			[ setq,
			  u_top_level_goal,
			  [u_random_element, u_candidates]
			],
			
			[ u_ndbg_roman,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('S'),
				     #\(w),
				     #\(i),
				     #\(t),
				     #\(c),
				     #\(h),
				     #\(i),
				     #\(n),
				     #\(g),
				     #\(' '),
				     #\(t),
				     #\(o),
				     #\(' '),
				     #\(n),
				     #\(e),
				     #\(w),
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
				     #\(l),
				     #\('\n'),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\(' '),
				     #\('('),
				     #\(b),
				     #\(r),
				     #\(o),
				     #\(k),
				     #\(e),
				     #\(' '),
				     #\(t),
				     #\(i),
				     #\(e),
				     #\(')')
				   ])
			],
			
			[ u_ndbg_roman,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(' '), #\(~), #\('A')]),
			  u_top_level_goal
			],
			[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			[u_daydreamer_control1, u_top_level_goal]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::DAYDREAMER-CONTROL0 
wl: arglist_info(u_daydreamer_control0,
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

% annotating U::DAYDREAMER-CONTROL0 
wl: init_args(exact_only, u_daydreamer_control0).


% annotating U::DAYDREAMER-CONTROL0 
f_u_daydreamer_control0(FnResult) :-
	Env=[],
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
				       #\(e),
				       #\(m),
				       #\(o),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(-),
				       #\(d),
				       #\(r),
				       #\(i),
				       #\(v),
				       #\(e),
				       #\(n),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(n),
				       #\(t),
				       #\(r),
				       #\(o),
				       #\(l),
				       #\(' '),
				       #\(l),
				       #\(o),
				       #\(o),
				       #\(p),
				       #\('.'),
				       #\('.'),
				       #\('.')
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_candidates, []],
		      [u_strikes, 0],
		      [u_top_level_goal, []]
		    ],
		    [u_yuntil, [>, u_strikes, 2]],
		    
		    [ u_ydo,
		      [u_need_decay],
		      [u_emotion_decay],
		      [setq, u_candidates, [u_most_highly_motivated_goals]],
		      
		      [ format,
			[u_standard_output],
			'$ARRAY'([*], claz_base_character, [#\(:)])
		      ],
		      [force_output, [u_standard_output]],
		      
		      [ cond,
			
			[ [u_null_c63, u_candidates],
			  
			  [ if,
			    [u_performance_mode_c63],
			    
			    [ progn,
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('N'),
					   #\(o),
					   #\(' '),
					   #\(m),
					   #\(o),
					   #\(r),
					   #\(e),
					   #\(' '),
					   #\(g),
					   #\(o),
					   #\(a),
					   #\(l),
					   #\(s),
					   #\(' '),
					   #\(t),
					   #\(o),
					   #\(' '),
					   #\(r),
					   #\(u),
					   #\(n),
					   #\(;),
					   #\(' '),
					   #\(s),
					   #\(w),
					   #\(i),
					   #\(t),
					   #\(c),
					   #\(h),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(' '),
					   #\(t),
					   #\(o),
					   #\(' '),
					   #\(d),
					   #\(a),
					   #\(y),
					   #\(d),
					   #\(r),
					   #\(e),
					   #\(a),
					   #\(m),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(' '),
					   #\(m),
					   #\(o),
					   #\(d),
					   #\(e)
					 ])
			      ],
			      [setq, u_strikes, [+, 1, u_strikes]],
			      [u_set_state, [quote, u_daydreaming]]
			    ],
			    
			    [ progn,
			      
			      [ if,
				[u_null_c63, [u_environmental_object_input]],
				
				[ progn,
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_rule,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('N'),
					       #\(o),
					       #\(' '),
					       #\(m),
					       #\(o),
					       #\(r),
					       #\(e),
					       #\(' '),
					       #\(g),
					       #\(o),
					       #\(a),
					       #\(l),
					       #\(s),
					       #\(' '),
					       #\(t),
					       #\(o),
					       #\(' '),
					       #\(r),
					       #\(u),
					       #\(n),
					       #\(;),
					       #\(' '),
					       #\(s),
					       #\(w),
					       #\(i),
					       #\(t),
					       #\(c),
					       #\(h),
					       #\(i),
					       #\(n),
					       #\(g),
					       #\(' '),
					       #\(t),
					       #\(o),
					       #\(' '),
					       #\(p),
					       #\(e),
					       #\(r),
					       #\(f),
					       #\(o),
					       #\(r),
					       #\(m),
					       #\(a),
					       #\(n),
					       #\(c),
					       #\(e),
					       #\(' '),
					       #\(m),
					       #\(o),
					       #\(d),
					       #\(e)
					     ])
				  ],
				  [setq, u_strikes, [+, 1, u_strikes]],
				  
				  [ u_yloop,
				    
				    [ u_yfor,
				      u_goal,
				      u_in,
				      u_xx_top_level_goals_xx
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ and,
					  
					  [ u_eq_c63,
					    [quote, u_waiting],
					    
					    [ u_ob_c36_get,
					      u_goal,
					      [quote, u_status]
					    ]
					  ],
					  
					  [ u_eq_c63,
					    [quote, real],
					    
					    [ u_ob_c36_get,
					      u_goal,
					      [quote, u_planning_type]
					    ]
					  ]
					],
					
					[ progn,
					  
					  [ u_change_tlg_status,
					    u_goal,
					    [quote, u_runable]
					  ]
					]
				      ]
				    ]
				  ],
				  [u_set_state, [quote, u_performance]]
				]
			      ]
			    ]
			  ]
			],
			
			[ [u_memq_c63, u_top_level_goal, u_candidates],
			  [setq, u_strikes, 0],
			  [u_daydreamer_control1, u_top_level_goal]
			],
			
			[ [=, [length, u_candidates], 1],
			  [setq, u_strikes, 0],
			  [setq, u_top_level_goal, [car, u_candidates]],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(c),
				       #\(h),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(n),
				       #\(e),
				       #\(w),
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
				     [#\(' '), #\(~), #\('A')]),
			    u_top_level_goal
			  ],
			  [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			  [u_daydreamer_control1, u_top_level_goal]
			],
			
			[ u_else,
			  [setq, u_strikes, 0],
			  
			  [ setq,
			    u_top_level_goal,
			    [u_random_element, u_candidates]
			  ],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(c),
				       #\(h),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(n),
				       #\(e),
				       #\(w),
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
				       #\(l),
				       #\('\n'),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\(' '),
				       #\('('),
				       #\(b),
				       #\(r),
				       #\(o),
				       #\(k),
				       #\(e),
				       #\(' '),
				       #\(t),
				       #\(i),
				       #\(e),
				       #\(')')
				     ])
			  ],
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(~), #\('A')]),
			    u_top_level_goal
			  ],
			  [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			  [u_daydreamer_control1, u_top_level_goal]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_daydreamer_control0, classof, claz_function),
   set_opv(u_daydreamer_control0, compile_as, kw_function),
   set_opv(u_daydreamer_control0, function, f_u_daydreamer_control0),
   DefunResult=u_daydreamer_control0.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:6894 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*need-decay-factor*', 0.98]).
:- set_var(TLEnv3, setq, u_xx_need_decay_factor_xx, 0.98).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:6926 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'need-decay',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Need decay.")
			    ],
			    
			    [ yloop,
			      [yfor, need, in, '*needs*'],
			      
			      [ ydo,
				
				[ 'set-strength',
				  need,
				  
				  [ 'fl*',
				    '*need-decay-factor*',
				    [strength, need]
				  ]
				],
				['cx$touch-fact', '*reality*', need]
			      ]
			    ]
			  ]).

% annotating U::NEED-DECAY 
wl: lambda_def(defun,
	      u_need_decay,
	      f_u_need_decay,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('N'),
			     #\(e),
			     #\(e),
			     #\(d),
			     #\(' '),
			     #\(d),
			     #\(e),
			     #\(c),
			     #\(a),
			     #\(y),
			     #\('.')
			   ])
		],
		
		[ u_yloop,
		  [u_yfor, u_need, u_in, u_xx_needs_xx],
		  
		  [ u_ydo,
		    
		    [ u_set_strength,
		      u_need,
		      [u_fl_xx, u_xx_need_decay_factor_xx, [u_strength, u_need]]
		    ],
		    [u_cx_c36_touch_fact, u_xx_reality_xx, u_need]
		  ]
		]
	      ]).


% annotating U::NEED-DECAY 
wl: arglist_info(u_need_decay,
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

% annotating U::NEED-DECAY 
wl: init_args(exact_only, u_need_decay).


% annotating U::NEED-DECAY 
f_u_need_decay(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('N'),
				       #\(e),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(d),
				       #\(e),
				       #\(c),
				       #\(a),
				       #\(y),
				       #\('.')
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_yfor, u_need, u_in, u_xx_needs_xx],
		    
		    [ u_ydo,
		      
		      [ u_set_strength,
			u_need,
			
			[ u_fl_xx,
			  u_xx_need_decay_factor_xx,
			  [u_strength, u_need]
			]
		      ],
		      [u_cx_c36_touch_fact, u_xx_reality_xx, u_need]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_need_decay, classof, claz_function),
   set_opv(u_need_decay, compile_as, kw_function),
   set_opv(u_need_decay, function, f_u_need_decay),
   DefunResult=u_need_decay.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7155 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*emotion-decay-factor*', 0.95]).
:- set_var(TLEnv3, setq, u_xx_emotion_decay_factor_xx, 0.95).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7190 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*emotion-gc-threshold*', 0.15]).
:- set_var(TLEnv3, setq, u_xx_emotion_gc_threshold_xx, 0.15).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7190 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Only non-motivating emotions are subject to decay.",
				     1,
				     7227)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7279 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'emotion-decay',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Emotion decay.")
			    ],
			    
			    [ yloop,
			      [yfor, emot, in, '*emotions*'],
			      
			      [ ydo,
				
				[ if,
				  [not, ['motivating-emotion?', emot]],
				  
				  [ progn,
				    
				    [ 'set-strength',
				      emot,
				      
				      [ 'fl*',
					'*emotion-decay-factor*',
					[strength, emot]
				      ]
				    ],
				    ['cx$touch-fact', '*reality*', emot],
				    
				    [ if,
				      
				      [ 'fl<',
					[strength, emot],
					'*emotion-gc-threshold*'
				      ],
				      
				      [ progn,
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("Emotion ~A below threshold."),
					  emot
					],
					['cx$retract', '*reality*', emot],
					['emotion-delete', emot]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::EMOTION-DECAY 
wl: lambda_def(defun,
	      u_emotion_decay,
	      f_u_emotion_decay,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('E'),
			     #\(m),
			     #\(o),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(' '),
			     #\(d),
			     #\(e),
			     #\(c),
			     #\(a),
			     #\(y),
			     #\('.')
			   ])
		],
		
		[ u_yloop,
		  [u_yfor, u_emot, u_in, u_xx_emotions_xx],
		  
		  [ u_ydo,
		    
		    [ if,
		      [not, [u_motivating_emotion_c63, u_emot]],
		      
		      [ progn,
			
			[ u_set_strength,
			  u_emot,
			  
			  [ u_fl_xx,
			    u_xx_emotion_decay_factor_xx,
			    [u_strength, u_emot]
			  ]
			],
			[u_cx_c36_touch_fact, u_xx_reality_xx, u_emot],
			
			[ if,
			  
			  [ u_fl_c60,
			    [u_strength, u_emot],
			    u_xx_emotion_gc_threshold_xx
			  ],
			  
			  [ progn,
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('E'),
					 #\(m),
					 #\(o),
					 #\(t),
					 #\(i),
					 #\(o),
					 #\(n),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(b),
					 #\(e),
					 #\(l),
					 #\(o),
					 #\(w),
					 #\(' '),
					 #\(t),
					 #\(h),
					 #\(r),
					 #\(e),
					 #\(s),
					 #\(h),
					 #\(o),
					 #\(l),
					 #\(d),
					 #\('.')
				       ]),
			      u_emot
			    ],
			    [u_cx_c36_retract, u_xx_reality_xx, u_emot],
			    [u_emotion_delete, u_emot]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::EMOTION-DECAY 
wl: arglist_info(u_emotion_decay,
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

% annotating U::EMOTION-DECAY 
wl: init_args(exact_only, u_emotion_decay).


% annotating U::EMOTION-DECAY 
f_u_emotion_decay(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('E'),
				       #\(m),
				       #\(o),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(d),
				       #\(e),
				       #\(c),
				       #\(a),
				       #\(y),
				       #\('.')
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_yfor, u_emot, u_in, u_xx_emotions_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			[not, [u_motivating_emotion_c63, u_emot]],
			
			[ progn,
			  
			  [ u_set_strength,
			    u_emot,
			    
			    [ u_fl_xx,
			      u_xx_emotion_decay_factor_xx,
			      [u_strength, u_emot]
			    ]
			  ],
			  [u_cx_c36_touch_fact, u_xx_reality_xx, u_emot],
			  
			  [ if,
			    
			    [ u_fl_c60,
			      [u_strength, u_emot],
			      u_xx_emotion_gc_threshold_xx
			    ],
			    
			    [ progn,
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('E'),
					   #\(m),
					   #\(o),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(b),
					   #\(e),
					   #\(l),
					   #\(o),
					   #\(w),
					   #\(' '),
					   #\(t),
					   #\(h),
					   #\(r),
					   #\(e),
					   #\(s),
					   #\(h),
					   #\(o),
					   #\(l),
					   #\(d),
					   #\('.')
					 ]),
				u_emot
			      ],
			      [u_cx_c36_retract, u_xx_reality_xx, u_emot],
			      [u_emotion_delete, u_emot]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_emotion_decay, classof, claz_function),
   set_opv(u_emotion_decay, compile_as, kw_function),
   set_opv(u_emotion_decay, function, f_u_emotion_decay),
   DefunResult=u_emotion_decay.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7820 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'emotion-delete',
			    [emot],
			    [setq, '*emotions*', ['delq!', emot, '*emotions*']],
			    '*emotions*'
			  ]).

% annotating U::EMOTION-DELETE 
wl: lambda_def(defun,
	      u_emotion_delete,
	      f_u_emotion_delete,
	      [u_emot],
	      
	      [ [setq, u_xx_emotions_xx, [u_delq_c33, u_emot, u_xx_emotions_xx]],
		u_xx_emotions_xx
	      ]).


% annotating U::EMOTION-DELETE 
wl: arglist_info(u_emotion_delete,
		[u_emot],
		[Emot_Param],
		arginfo{ all:[u_emot],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emot],
			 opt:0,
			 req:[u_emot],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EMOTION-DELETE 
wl: init_args(exact_only, u_emotion_delete).


% annotating U::EMOTION-DELETE 
f_u_emotion_delete(Emot_Param, FnResult) :-
	Env=[bv(u_emot, Emot_Param)],
	f_u_delq_c33(u_emot, u_xx_emotions_xx, Xx_emotions_xx),
	set_var(Env, u_xx_emotions_xx, Xx_emotions_xx),
	get_var(Env, u_xx_emotions_xx, Xx_emotions_xx_Get),
	Xx_emotions_xx_Get=FnResult.
:- set_opv(f_u_emotion_delete, classof, claz_function),
   set_opv(u_emotion_delete, compile_as, kw_function),
   set_opv(u_emotion_delete, function, f_u_emotion_delete),
   DefunResult=u_emotion_delete.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:7908 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'emotion-add',
			    [emot],
			    
			    [ if,
			      [not, ['memq?', emot, '*emotions*']],
			      [setq, '*emotions*', [cons, emot, '*emotions*']]
			    ],
			    emot
			  ]).

% annotating U::EMOTION-ADD 
wl: lambda_def(defun,
	      u_emotion_add,
	      f_u_emotion_add,
	      [u_emot],
	      
	      [ 
		[ if,
		  [not, [u_memq_c63, u_emot, u_xx_emotions_xx]],
		  [setq, u_xx_emotions_xx, [cons, u_emot, u_xx_emotions_xx]]
		],
		u_emot
	      ]).


% annotating U::EMOTION-ADD 
wl: arglist_info(u_emotion_add,
		[u_emot],
		[Emot_Param],
		arginfo{ all:[u_emot],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emot],
			 opt:0,
			 req:[u_emot],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EMOTION-ADD 
wl: init_args(exact_only, u_emotion_add).


% annotating U::EMOTION-ADD 
f_u_emotion_add(Emot_Param, Emot_Param) :-
	Env=[bv(u_emot, Emot_Param)],
	f_u_memq_c63(u_emot, u_xx_emotions_xx, PredArgResult),
	(   PredArgResult==[]
	->  get_var(Env, u_xx_emotions_xx, Xx_emotions_xx_Get),
	    TrueResult=[Emot_Param|Xx_emotions_xx_Get],
	    set_var(Env, u_xx_emotions_xx, TrueResult),
	    _41958=TrueResult
	;   _41958=[]
	).
:- set_opv(f_u_emotion_add, classof, claz_function),
   set_opv(u_emotion_add, compile_as, kw_function),
   set_opv(u_emotion_add, function, f_u_emotion_add),
   DefunResult=u_emotion_add.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8027 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'motivating-emotion?',
			    [emot],
			    
			    [ 'any?',
			      [lambda, [ob], ['ty$instance?', ob, [quote, goal]]],
			      
			      [ 'get-dependees',
				emot,
				'*reality*',
				'*me-belief-path*'
			      ]
			    ]
			  ]).

% annotating U::MOTIVATING-EMOTION? 
wl: lambda_def(defun,
	      u_motivating_emotion_c63,
	      f_u_motivating_emotion_c63,
	      [u_emot],
	      
	      [ 
		[ u_any_c63,
		  [lambda, [u_ob], [u_ty_c36_instance_c63, u_ob, [quote, u_goal]]],
		  
		  [ u_get_dependees,
		    u_emot,
		    u_xx_reality_xx,
		    u_xx_me_belief_path_xx
		  ]
		]
	      ]).


% annotating U::MOTIVATING-EMOTION? 
wl: arglist_info(u_motivating_emotion_c63,
		[u_emot],
		[Emot_Param],
		arginfo{ all:[u_emot],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emot],
			 opt:0,
			 req:[u_emot],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MOTIVATING-EMOTION? 
wl: init_args(exact_only, u_motivating_emotion_c63).


% annotating U::MOTIVATING-EMOTION? 
f_u_motivating_emotion_c63(Emot_Param, FnResult) :-
	Env=[bv(u_emot, Emot_Param)],
	f_u_any_c63(
		    [ lambda,
		      [u_ob],
		      [u_ty_c36_instance_c63, u_ob, [quote, u_goal]]
		    ],
		    
		    [ u_get_dependees,
		      u_emot,
		      u_xx_reality_xx,
		      u_xx_me_belief_path_xx
		    ],
		    Any_c63_Ret),
	Any_c63_Ret=FnResult.
:- set_opv(f_u_motivating_emotion_c63, classof, claz_function),
   set_opv(u_motivating_emotion_c63, compile_as, kw_function),
   set_opv(u_motivating_emotion_c63, function, f_u_motivating_emotion_c63),
   DefunResult=u_motivating_emotion_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8027 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" or (memq? ob *top-level-goals*)",
				     47,
				     8108)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8200 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*top-level-goals*', []]).
:- set_var(TLEnv3, setq, u_xx_top_level_goals_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8200 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8231)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8200 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Find the top-level goals which are most highly motivated and are",
				     1,
				     8233)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8200 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" not halted, and if in performance mode, are not imaginary.",
				     1,
				     8300)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8200 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8361)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'most-highly-motivated-goals',
			    [],
			    
			    [ yloop,
			      
			      [ initial,
				['highest-strength', 0.0],
				[candidates, []]
			      ],
			      [yfor, 'top-level-goal', in, '*top-level-goals*'],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    
				    [ 'eq?',
				      [quote, runable],
				      
				      [ 'ob$get',
					'top-level-goal',
					[quote, status]
				      ]
				    ],
				    
				    [ or,
				      ['daydreaming-mode?'],
				      
				      [ 'neq?',
					[quote, imaginary],
					
					[ 'ob$get',
					  'top-level-goal',
					  [quote, 'planning-type']
					]
				      ]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ cond,
				      
				      [ 
					[ (=),
					  [strength, 'top-level-goal'],
					  'highest-strength'
					],
					
					[ setq,
					  candidates,
					  [cons, 'top-level-goal', candidates]
					]
				      ],
				      
				      [ 
					[ (>),
					  [strength, 'top-level-goal'],
					  'highest-strength'
					],
					
					[ setq,
					  'highest-strength',
					  [strength, 'top-level-goal']
					],
					
					[ setq,
					  candidates,
					  [list, 'top-level-goal']
					]
				      ]
				    ]
				  ]
				]
			      ],
			      [yresult, candidates]
			    ]
			  ]).

% annotating U::MOST-HIGHLY-MOTIVATED-GOALS 
wl: lambda_def(defun,
	      u_most_highly_motivated_goals,
	      f_u_most_highly_motivated_goals,
	      [],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_highest_strength, 0.0], [u_candidates, []]],
		  [u_yfor, u_top_level_goal, u_in, u_xx_top_level_goals_xx],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			
			[ u_eq_c63,
			  [quote, u_runable],
			  [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
			],
			
			[ or,
			  [u_daydreaming_mode_c63],
			  
			  [ u_neq_c63,
			    [quote, u_imaginary],
			    
			    [ u_ob_c36_get,
			      u_top_level_goal,
			      [quote, u_planning_type]
			    ]
			  ]
			]
		      ],
		      
		      [ progn,
			
			[ cond,
			  
			  [ 
			    [ (=),
			      [u_strength, u_top_level_goal],
			      u_highest_strength
			    ],
			    
			    [ setq,
			      u_candidates,
			      [cons, u_top_level_goal, u_candidates]
			    ]
			  ],
			  
			  [ 
			    [ (>),
			      [u_strength, u_top_level_goal],
			      u_highest_strength
			    ],
			    
			    [ setq,
			      u_highest_strength,
			      [u_strength, u_top_level_goal]
			    ],
			    [setq, u_candidates, [list, u_top_level_goal]]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_candidates]
		]
	      ]).


% annotating U::MOST-HIGHLY-MOTIVATED-GOALS 
wl: arglist_info(u_most_highly_motivated_goals,
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

% annotating U::MOST-HIGHLY-MOTIVATED-GOALS 
wl: init_args(exact_only, u_most_highly_motivated_goals).


% annotating U::MOST-HIGHLY-MOTIVATED-GOALS 
f_u_most_highly_motivated_goals(FnResult) :-
	Env=[],
	f_u_yloop(
		  [ [u_initial, [u_highest_strength, 0.0], [u_candidates, []]],
		    [u_yfor, u_top_level_goal, u_in, u_xx_top_level_goals_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ u_eq_c63,
			    [quote, u_runable],
			    [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
			  ],
			  
			  [ or,
			    [u_daydreaming_mode_c63],
			    
			    [ u_neq_c63,
			      [quote, u_imaginary],
			      
			      [ u_ob_c36_get,
				u_top_level_goal,
				[quote, u_planning_type]
			      ]
			    ]
			  ]
			],
			
			[ progn,
			  
			  [ cond,
			    
			    [ 
			      [ (=),
				[u_strength, u_top_level_goal],
				u_highest_strength
			      ],
			      
			      [ setq,
				u_candidates,
				[cons, u_top_level_goal, u_candidates]
			      ]
			    ],
			    
			    [ 
			      [ (>),
				[u_strength, u_top_level_goal],
				u_highest_strength
			      ],
			      
			      [ setq,
				u_highest_strength,
				[u_strength, u_top_level_goal]
			      ],
			      [setq, u_candidates, [list, u_top_level_goal]]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_candidates]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_most_highly_motivated_goals, classof, claz_function),
   set_opv(u_most_highly_motivated_goals, compile_as, kw_function),
   set_opv(u_most_highly_motivated_goals,
	   function,
	   f_u_most_highly_motivated_goals),
   DefunResult=u_most_highly_motivated_goals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (emotion nil)", 1, 8442)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (setq emotion (ob$get top-level-goal 'emotion))",
				     1,
				     8846)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (if (null? emotion)",
				     1,
				     8913)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (error \"No motivating emotion found for ~A\"",
				     1,
				     8952)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                             top-level-goal))",
				     1,
				     9019)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9470)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Debugging functions", 1, 9472)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:8362 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9494)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-tasks',
			    [],
			    
			    [ yloop,
			      [yfor, c, in, '*top-level-goals*'],
			      
			      [ ydo,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  task,
				  '$STRING'("~A concern ~A motiv ~A status ~A"),
				  
				  [ if,
				    
				    [ 'eq?',
				      [quote, imaginary],
				      ['ob$get', c, [quote, 'planning-type']]
				    ],
				    '$STRING'("Daydreaming goal"),
				    '$STRING'("Personal goal")
				  ],
				  ['tlg->string', c],
				  [strength, c],
				  ['ob$get', c, [quote, status]]
				]
			      ]
			    ]
			  ]).

% annotating U::PRINT-TASKS 
wl: lambda_def(defun,
	      u_print_tasks,
	      f_u_print_tasks,
	      [],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_c, u_in, u_xx_top_level_goals_xx],
		  
		  [ u_ydo,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_task,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('A'),
				 #\(' '),
				 #\(c),
				 #\(o),
				 #\(n),
				 #\(c),
				 #\(e),
				 #\(r),
				 #\(n),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(m),
				 #\(o),
				 #\(t),
				 #\(i),
				 #\(v),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(s),
				 #\(t),
				 #\(a),
				 #\(t),
				 #\(u),
				 #\(s),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      
		      [ if,
			
			[ u_eq_c63,
			  [quote, u_imaginary],
			  [u_ob_c36_get, u_c, [quote, u_planning_type]]
			],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('D'),
				   #\(a),
				   #\(y),
				   #\(d),
				   #\(r),
				   #\(e),
				   #\(a),
				   #\(m),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(g),
				   #\(o),
				   #\(a),
				   #\(l)
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('P'),
				   #\(e),
				   #\(r),
				   #\(s),
				   #\(o),
				   #\(n),
				   #\(a),
				   #\(l),
				   #\(' '),
				   #\(g),
				   #\(o),
				   #\(a),
				   #\(l)
				 ])
		      ],
		      [u_tlg_c62_string, u_c],
		      [u_strength, u_c],
		      [u_ob_c36_get, u_c, [quote, u_status]]
		    ]
		  ]
		]
	      ]).


% annotating U::PRINT-TASKS 
wl: arglist_info(u_print_tasks,
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

% annotating U::PRINT-TASKS 
wl: init_args(exact_only, u_print_tasks).


% annotating U::PRINT-TASKS 
f_u_print_tasks(FnResult) :-
	Env=[],
	f_u_yloop(
		  [ [u_yfor, u_c, u_in, u_xx_top_level_goals_xx],
		    
		    [ u_ydo,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_task,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(~),
				   #\('A'),
				   #\(' '),
				   #\(c),
				   #\(o),
				   #\(n),
				   #\(c),
				   #\(e),
				   #\(r),
				   #\(n),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(m),
				   #\(o),
				   #\(t),
				   #\(i),
				   #\(v),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(s),
				   #\(t),
				   #\(a),
				   #\(t),
				   #\(u),
				   #\(s),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			
			[ if,
			  
			  [ u_eq_c63,
			    [quote, u_imaginary],
			    [u_ob_c36_get, u_c, [quote, u_planning_type]]
			  ],
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('D'),
				     #\(a),
				     #\(y),
				     #\(d),
				     #\(r),
				     #\(e),
				     #\(a),
				     #\(m),
				     #\(i),
				     #\(n),
				     #\(g),
				     #\(' '),
				     #\(g),
				     #\(o),
				     #\(a),
				     #\(l)
				   ]),
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('P'),
				     #\(e),
				     #\(r),
				     #\(s),
				     #\(o),
				     #\(n),
				     #\(a),
				     #\(l),
				     #\(' '),
				     #\(g),
				     #\(o),
				     #\(a),
				     #\(l)
				   ])
			],
			[u_tlg_c62_string, u_c],
			[u_strength, u_c],
			[u_ob_c36_get, u_c, [quote, u_status]]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_print_tasks, classof, claz_function),
   set_opv(u_print_tasks, compile_as, kw_function),
   set_opv(u_print_tasks, function, f_u_print_tasks),
   DefunResult=u_print_tasks.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9917)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Control algorithm for a particular top-level-goal:",
				     1,
				     9919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1) Run one step of the planner on the next context.",
				     1,
				     9972)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 2) If the top-level goal succeeded, terminate planning for this",
				     1,
				     10026)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("    top-level goal.", 1, 10092)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 3) Otherwise if running the planner produced no sprouts, then",
				     1,
				     10113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    attempt to backtrack.", 1, 10177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 4) Otherwise set the next context to run to be one of the sprouts,",
				     1,
				     10204)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    selected at random.", 1, 10273)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:9496 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10298)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:10299 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*top-level-goal*', []]).
:- set_var(TLEnv3, setq, u_xx_top_level_goal_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:10328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'daydreamer-control1',
			    ['top-level-goal'],
			    
			    [ let,
			      
			      [ 
				[ 'next-context',
				  ['get-next-context', 'top-level-goal']
				],
				[sprouts, []],
				
				[ 'tlg-switch?',
				  ['neq?', 'top-level-goal', '*top-level-goal*']
				],
				['succeeded-goal', []]
			      ],
			      [if, 'tlg-switch?', ['gen-new-paragraph']],
			      [setq, '*top-level-goal*', 'top-level-goal'],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'rule-long',
				'$STRING'("Running control algorithm for ~A in ~A"),
				'top-level-goal',
				'next-context'
			      ],
			      
			      [ if,
				
				[ and,
				  
				  [ 'number?',
				    ['ob$get', 'next-context', [quote, timeout]]
				  ],
				  
				  [ <=,
				    ['ob$get', 'next-context', [quote, timeout]],
				    0
				  ]
				],
				
				[ progn,
				  
				  [ 'ndbg-roman',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Timeout")
				  ],
				  
				  [ 'ndbg-roman',
				    '*gate-dbg*',
				    rule,
				    '$STRING'(" on ~A"),
				    'next-context'
				  ],
				  ['ndbg-newline', '*gate-dbg*', rule],
				  
				  [ 'backtrack-top-level-goal',
				    'top-level-goal',
				    'next-context'
				  ]
				],
				
				[ progn,
				  
				  [ 'run-rules',
				    'top-level-goal',
				    'next-context',
				    'tlg-switch?'
				  ],
				  
				  [ if,
				    
				    [ setq,
				      'succeeded-goal',
				      
				      [ 'find-top-level-goal-outcome?',
					'top-level-goal',
					'next-context',
					'*succeeded-goal-ob*'
				      ]
				    ],
				    
				    [ 'terminate-top-level-goal',
				      'top-level-goal',
				      'succeeded-goal',
				      'next-context'
				    ],
				    
				    [ progn,
				      
				      [ if,
					
					[ or,
					  
					  [ 'eq?',
					    [quote, runable],
					    
					    [ 'ob$get',
					      'top-level-goal',
					      [quote, status]
					    ]
					  ],
					  
					  [ 'eq?',
					    [quote, 'fired-halt'],
					    
					    [ 'ob$get',
					      'top-level-goal',
					      [quote, status]
					    ]
					  ]
					],
					
					[ progn,
					  
					  [ if,
					    
					    [ 'eq?',
					      [quote, 'fired-halt'],
					      
					      [ 'ob$get',
						'top-level-goal',
						[quote, status]
					      ]
					    ],
					    
					    [ 'change-tlg-status',
					      'top-level-goal',
					      [quote, halted]
					    ]
					  ],
					  
					  [ setq,
					    sprouts,
					    
					    [ 'prune-possibilities',
					      ['cx$children', 'next-context']
					    ]
					  ],
					  
					  [ if,
					    ['null?', sprouts],
					    
					    [ 'backtrack-top-level-goal',
					      'top-level-goal',
					      'next-context'
					    ],
					    
					    [ 'set-next-context',
					      'top-level-goal',
					      [car, sprouts]
					    ]
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::DAYDREAMER-CONTROL1 
wl: lambda_def(defun,
	      u_daydreamer_control1,
	      f_u_daydreamer_control1,
	      [u_top_level_goal],
	      
	      [ 
		[ let,
		  
		  [ [u_next_context, [u_get_next_context, u_top_level_goal]],
		    [u_sprouts, []],
		    
		    [ u_tlg_switch_c63,
		      [u_neq_c63, u_top_level_goal, u_xx_top_level_goal_xx]
		    ],
		    [u_succeeded_goal, []]
		  ],
		  [if, u_tlg_switch_c63, [u_gen_new_paragraph]],
		  [setq, u_xx_top_level_goal_xx, u_top_level_goal],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule_long,
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
			       #\(c),
			       #\(o),
			       #\(n),
			       #\(t),
			       #\(r),
			       #\(o),
			       #\(l),
			       #\(' '),
			       #\(a),
			       #\(l),
			       #\(g),
			       #\(o),
			       #\(r),
			       #\(i),
			       #\(t),
			       #\(h),
			       #\(m),
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
		    u_top_level_goal,
		    u_next_context
		  ],
		  
		  [ if,
		    
		    [ and,
		      
		      [ u_number_c63,
			[u_ob_c36_get, u_next_context, [quote, ext_timeout]]
		      ],
		      [<=, [u_ob_c36_get, u_next_context, [quote, ext_timeout]], 0]
		    ],
		    
		    [ progn,
		      
		      [ u_ndbg_roman,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\('T'), #\(i), #\(m), #\(e), #\(o), #\(u), #\(t)])
		      ],
		      
		      [ u_ndbg_roman,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(o), #\(n), #\(' '), #\(~), #\('A')]),
			u_next_context
		      ],
		      [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		      
		      [ u_backtrack_top_level_goal,
			u_top_level_goal,
			u_next_context
		      ]
		    ],
		    
		    [ progn,
		      
		      [ u_run_rules,
			u_top_level_goal,
			u_next_context,
			u_tlg_switch_c63
		      ],
		      
		      [ if,
			
			[ setq,
			  u_succeeded_goal,
			  
			  [ u_find_top_level_goal_outcome_c63,
			    u_top_level_goal,
			    u_next_context,
			    u_xx_succeeded_goal_ob_xx
			  ]
			],
			
			[ u_terminate_top_level_goal,
			  u_top_level_goal,
			  u_succeeded_goal,
			  u_next_context
			],
			
			[ progn,
			  
			  [ if,
			    
			    [ or,
			      
			      [ u_eq_c63,
				[quote, u_runable],
				
				[ u_ob_c36_get,
				  u_top_level_goal,
				  [quote, u_status]
				]
			      ],
			      
			      [ u_eq_c63,
				[quote, u_fired_halt],
				
				[ u_ob_c36_get,
				  u_top_level_goal,
				  [quote, u_status]
				]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ if,
				
				[ u_eq_c63,
				  [quote, u_fired_halt],
				  
				  [ u_ob_c36_get,
				    u_top_level_goal,
				    [quote, u_status]
				  ]
				],
				
				[ u_change_tlg_status,
				  u_top_level_goal,
				  [quote, u_halted]
				]
			      ],
			      
			      [ setq,
				u_sprouts,
				
				[ u_prune_possibilities,
				  [u_cx_c36_children, u_next_context]
				]
			      ],
			      
			      [ if,
				[u_null_c63, u_sprouts],
				
				[ u_backtrack_top_level_goal,
				  u_top_level_goal,
				  u_next_context
				],
				
				[ u_set_next_context,
				  u_top_level_goal,
				  [car, u_sprouts]
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::DAYDREAMER-CONTROL1 
wl: arglist_info(u_daydreamer_control1,
		[u_top_level_goal],
		[Top_level_goal_Param],
		arginfo{ all:[u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal],
			 opt:0,
			 req:[u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DAYDREAMER-CONTROL1 
wl: init_args(exact_only, u_daydreamer_control1).


% annotating U::DAYDREAMER-CONTROL1 
f_u_daydreamer_control1(Top_level_goal_Param, TrueResult58) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_get_next_context(Top_level_goal_Param, Next_context_Init),
	f_u_neq_c63(u_top_level_goal,
		    u_xx_top_level_goal_xx,
		    Tlg_switch_c63_Init),
	LEnv=[[bv(u_next_context, Next_context_Init), bv(u_sprouts, []), bv(u_tlg_switch_c63, Tlg_switch_c63_Init), bv(u_succeeded_goal, [])]|Env],
	get_var(LEnv, u_tlg_switch_c63, IFTEST),
	(   IFTEST\==[]
	->  f_u_gen_new_paragraph(TrueResult),
	    _47400=TrueResult
	;   _47400=[]
	),
	set_var(LEnv, u_xx_top_level_goal_xx, Top_level_goal_Param),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
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
				       #\(c),
				       #\(o),
				       #\(n),
				       #\(t),
				       #\(r),
				       #\(o),
				       #\(l),
				       #\(' '),
				       #\(a),
				       #\(l),
				       #\(g),
				       #\(o),
				       #\(r),
				       #\(i),
				       #\(t),
				       #\(h),
				       #\(m),
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
			    u_top_level_goal,
			    u_next_context
			  ],
			  Roman_nl_Ret),
	f_u_number_c63([u_ob_c36_get, u_next_context, [quote, ext_timeout]],
		       IFTEST25),
	(   IFTEST25\==[]
	->  get_var(LEnv, u_next_context, Next_context_Get),
	    f_u_ob_c36_get(Next_context_Get, ext_timeout, Ext_timeout),
	    <=(Ext_timeout, 0, TrueResult28),
	    IFTEST23=TrueResult28
	;   IFTEST23=[]
	),
	(   IFTEST23\==[]
	->  f_u_ndbg_roman(u_xx_gate_dbg_xx,
			   u_rule,
			   
			   [ '$ARRAY'([*],
				      claz_base_character,
				      
				      [ #\('T'),
					#\(i),
					#\(m),
					#\(e),
					#\(o),
					#\(u),
					#\(t)
				      ])
			   ],
			   Ndbg_roman_Ret),
	    f_u_ndbg_roman(u_xx_gate_dbg_xx,
			   u_rule,
			   
			   [ '$ARRAY'([*],
				      claz_base_character,
				      
				      [ #\(' '),
					#\(o),
					#\(n),
					#\(' '),
					#\(~),
					#\('A')
				      ]),
			     u_next_context
			   ],
			   Ndbg_roman_Ret71),
	    f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	    get_var(LEnv, u_next_context, Next_context_Get30),
	    f_u_backtrack_top_level_goal(Top_level_goal_Param,
					 Next_context_Get30,
					 TrueResult61),
	    TrueResult58=TrueResult61
	;   get_var(LEnv, u_next_context, Next_context_Get32),
	    get_var(LEnv, u_tlg_switch_c63, Tlg_switch_c63_Get33),
	    f_u_run_rules(Top_level_goal_Param,
			  Next_context_Get32,
			  Tlg_switch_c63_Get33,
			  Run_rules_Ret),
	    get_var(LEnv, u_next_context, Next_context_Get37),
	    get_var(LEnv,
		    u_xx_succeeded_goal_ob_xx,
		    Xx_succeeded_goal_ob_xx_Get),
	    f_u_find_top_level_goal_outcome_c63(Top_level_goal_Param,
						Next_context_Get37,
						Xx_succeeded_goal_ob_xx_Get,
						IFTEST34),
	    set_var(LEnv, u_succeeded_goal, IFTEST34),
	    (   IFTEST34\==[]
	    ->  get_var(LEnv, u_next_context, Next_context_Get41),
		get_var(LEnv, u_succeeded_goal, Succeeded_goal_Get),
		f_u_terminate_top_level_goal(Top_level_goal_Param,
					     Succeeded_goal_Get,
					     Next_context_Get41,
					     TrueResult59),
		TrueResult58=TrueResult59
	    ;   (   f_u_eq_c63([quote, u_runable],
			       
			       [ u_ob_c36_get,
				 u_top_level_goal,
				 [quote, u_status]
			       ],
			       FORM1_Res),
		    FORM1_Res\==[],
		    IFTEST42=FORM1_Res
		->  true
		;   f_u_eq_c63([quote, u_fired_halt],
			       
			       [ u_ob_c36_get,
				 u_top_level_goal,
				 [quote, u_status]
			       ],
			       Eq_c63_Ret),
		    IFTEST42=Eq_c63_Ret
		),
		(   IFTEST42\==[]
		->  f_u_eq_c63([quote, u_fired_halt],
			       
			       [ u_ob_c36_get,
				 u_top_level_goal,
				 [quote, u_status]
			       ],
			       IFTEST45),
		    (   IFTEST45\==[]
		    ->  f_u_change_tlg_status(Top_level_goal_Param,
					      u_halted,
					      TrueResult48),
			_48314=TrueResult48
		    ;   _48314=[]
		    ),
		    get_var(LEnv, u_next_context, Next_context_Get49),
		    f_u_cx_c36_children(Next_context_Get49,
					Prune_possibilities_Param),
		    f_u_prune_possibilities(Prune_possibilities_Param, Sprouts),
		    set_var(LEnv, u_sprouts, Sprouts),
		    f_u_null_c63(u_sprouts, IFTEST50),
		    (   IFTEST50\==[]
		    ->  get_var(LEnv, u_next_context, Next_context_Get53),
			f_u_backtrack_top_level_goal(Top_level_goal_Param,
						     Next_context_Get53,
						     TrueResult56),
			TrueResult58=TrueResult56
		    ;   get_var(LEnv, u_sprouts, Sprouts_Get),
			cl_car(Sprouts_Get, Car_Ret),
			f_u_set_next_context(Top_level_goal_Param,
					     Car_Ret,
					     ElseResult),
			TrueResult58=ElseResult
		    )
		;   TrueResult58=[]
		)
	    )
	).
:- set_opv(f_u_daydreamer_control1, classof, claz_function),
   set_opv(u_daydreamer_control1, compile_as, kw_function),
   set_opv(u_daydreamer_control1, function, f_u_daydreamer_control1),
   DefunResult=u_daydreamer_control1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:10328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: eventually we may wish to run scenario generator",
				     17,
				     11436)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:10328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" past top-level goal success.", 17, 11509)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:10328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" should include case of top-level goal failure",
				     22,
				     12122)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12363 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'change-tlg-status',
			    [tlg, status],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Change status of ~A to ~A"),
			      ['tlg->string', tlg],
			      status
			    ],
			    ['ob$set', tlg, [quote, status], status]
			  ]).

% annotating U::CHANGE-TLG-STATUS 
wl: lambda_def(defun,
	      u_change_tlg_status,
	      f_u_change_tlg_status,
	      [u_tlg, u_status],
	      
	      [ 
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('C'),
			     #\(h),
			     #\(a),
			     #\(n),
			     #\(g),
			     #\(e),
			     #\(' '),
			     #\(s),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(u),
			     #\(s),
			     #\(' '),
			     #\(o),
			     #\(f),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(t),
			     #\(o),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  [u_tlg_c62_string, u_tlg],
		  u_status
		],
		[u_ob_c36_set, u_tlg, [quote, u_status], u_status]
	      ]).


% annotating U::CHANGE-TLG-STATUS 
wl: arglist_info(u_change_tlg_status,
		[u_tlg, u_status],
		[Tlg_Param, Status_Param],
		arginfo{ all:[u_tlg, u_status],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_tlg, u_status],
			 opt:0,
			 req:[u_tlg, u_status],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CHANGE-TLG-STATUS 
wl: init_args(exact_only, u_change_tlg_status).


% annotating U::CHANGE-TLG-STATUS 
f_u_change_tlg_status(Tlg_Param, Status_Param, FnResult) :-
	Env=[bv(u_tlg, Tlg_Param), bv(u_status, Status_Param)],
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\('C'),
				    #\(h),
				    #\(a),
				    #\(n),
				    #\(g),
				    #\(e),
				    #\(' '),
				    #\(s),
				    #\(t),
				    #\(a),
				    #\(t),
				    #\(u),
				    #\(s),
				    #\(' '),
				    #\(o),
				    #\(f),
				    #\(' '),
				    #\(~),
				    #\('A'),
				    #\(' '),
				    #\(t),
				    #\(o),
				    #\(' '),
				    #\(~),
				    #\('A')
				  ]),
			 [u_tlg_c62_string, u_tlg],
			 u_status
		       ],
		       Ndbg_roman_Ret),
	f_u_ob_c36_set(Tlg_Param, u_status, Status_Param, C36_set_Ret),
	C36_set_Ret=FnResult.
:- set_opv(f_u_change_tlg_status, classof, claz_function),
   set_opv(u_change_tlg_status, compile_as, kw_function),
   set_opv(u_change_tlg_status, function, f_u_change_tlg_status),
   DefunResult=u_change_tlg_status.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12363 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Can't print below because it might be FIRED-HALT, a weird state.",
				     1,
				     12506)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12363 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              (ob$get tlg 'status)",
				     1,
				     12573)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12662 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-next-context',
			    ['top-level-goal'],
			    
			    [ if,
			      
			      [ 'eq?',
				[quote, imaginary],
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'planning-type']
				]
			      ],
			      
			      [ 'ob$get',
				'top-level-goal',
				[quote, 'next-context']
			      ],
			      '*reality-lookahead*'
			    ]
			  ]).

% annotating U::GET-NEXT-CONTEXT 
wl: lambda_def(defun,
	      u_get_next_context,
	      f_u_get_next_context,
	      [u_top_level_goal],
	      
	      [ 
		[ if,
		  
		  [ u_eq_c63,
		    [quote, u_imaginary],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		  ],
		  [u_ob_c36_get, u_top_level_goal, [quote, u_next_context]],
		  u_xx_reality_lookahead_xx
		]
	      ]).


% annotating U::GET-NEXT-CONTEXT 
wl: arglist_info(u_get_next_context,
		[u_top_level_goal],
		[Top_level_goal_Param],
		arginfo{ all:[u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal],
			 opt:0,
			 req:[u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-NEXT-CONTEXT 
wl: init_args(exact_only, u_get_next_context).


% annotating U::GET-NEXT-CONTEXT 
f_u_get_next_context(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Top_level_goal_Param, u_next_context, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	    FnResult=Xx_reality_lookahead_xx_Get
	).
:- set_opv(f_u_get_next_context, classof, claz_function),
   set_opv(u_get_next_context, compile_as, kw_function),
   set_opv(u_get_next_context, function, f_u_get_next_context),
   DefunResult=u_get_next_context.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:12838 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'set-next-context',
			    ['top-level-goal', 'next-context'],
			    
			    [ if,
			      
			      [ 'eq?',
				[quote, imaginary],
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'planning-type']
				]
			      ],
			      
			      [ 'ob$set',
				'top-level-goal',
				[quote, 'next-context'],
				'next-context'
			      ],
			      [setq, '*reality-lookahead*', 'next-context']
			    ]
			  ]).

% annotating U::SET-NEXT-CONTEXT 
wl: lambda_def(defun,
	      u_set_next_context,
	      f_u_set_next_context,
	      [u_top_level_goal, u_next_context],
	      
	      [ 
		[ if,
		  
		  [ u_eq_c63,
		    [quote, u_imaginary],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		  ],
		  
		  [ u_ob_c36_set,
		    u_top_level_goal,
		    [quote, u_next_context],
		    u_next_context
		  ],
		  [setq, u_xx_reality_lookahead_xx, u_next_context]
		]
	      ]).


% annotating U::SET-NEXT-CONTEXT 
wl: arglist_info(u_set_next_context,
		[u_top_level_goal, u_next_context],
		[Top_level_goal_Param, Next_context_Param],
		arginfo{ all:[u_top_level_goal, u_next_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_next_context],
			 opt:0,
			 req:[u_top_level_goal, u_next_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-NEXT-CONTEXT 
wl: init_args(exact_only, u_set_next_context).


% annotating U::SET-NEXT-CONTEXT 
f_u_set_next_context(Top_level_goal_Param, Next_context_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_next_context, Next_context_Param)],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_set(Top_level_goal_Param,
			   u_next_context,
			   Next_context_Param,
			   TrueResult),
	    FnResult=TrueResult
	;   set_var(Env, u_xx_reality_lookahead_xx, Next_context_Param),
	    FnResult=Next_context_Param
	).
:- set_opv(f_u_set_next_context, classof, claz_function),
   set_opv(u_set_next_context, compile_as, kw_function),
   set_opv(u_set_next_context, function, f_u_set_next_context),
   DefunResult=u_set_next_context.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-backtrack-wall',
			    ['top-level-goal'],
			    
			    [ if,
			      
			      [ 'eq?',
				[quote, imaginary],
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'planning-type']
				]
			      ],
			      
			      [ 'ob$get',
				'top-level-goal',
				[quote, 'backtrack-wall']
			      ],
			      '*reality*'
			    ]
			  ]).

% annotating U::GET-BACKTRACK-WALL 
wl: lambda_def(defun,
	      u_get_backtrack_wall,
	      f_u_get_backtrack_wall,
	      [u_top_level_goal],
	      
	      [ 
		[ if,
		  
		  [ u_eq_c63,
		    [quote, u_imaginary],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		  ],
		  [u_ob_c36_get, u_top_level_goal, [quote, u_backtrack_wall]],
		  u_xx_reality_xx
		]
	      ]).


% annotating U::GET-BACKTRACK-WALL 
wl: arglist_info(u_get_backtrack_wall,
		[u_top_level_goal],
		[Top_level_goal_Param],
		arginfo{ all:[u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal],
			 opt:0,
			 req:[u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-BACKTRACK-WALL 
wl: init_args(exact_only, u_get_backtrack_wall).


% annotating U::GET-BACKTRACK-WALL 
f_u_get_backtrack_wall(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Top_level_goal_Param, u_backtrack_wall, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get),
	    FnResult=Xx_reality_xx_Get
	).
:- set_opv(f_u_get_backtrack_wall, classof, claz_function),
   set_opv(u_get_backtrack_wall, compile_as, kw_function),
   set_opv(u_get_backtrack_wall, function, f_u_get_backtrack_wall),
   DefunResult=u_get_backtrack_wall.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'find-top-level-goal-outcome?',
			    ['top-level-goal', context, 'goal-type'],
			    
			    [ yloop,
			      [initial, ['found?', []]],
			      
			      [ yfor,
				elem,
				in,
				['cx$get-all-ty', context, 'goal-type']
			      ],
			      [yuntil, 'found?'],
			      
			      [ ydo,
				
				[ if,
				  ['eq?', 'top-level-goal', elem],
				  [setq, 'found?', elem]
				]
			      ],
			      [yresult, 'found?']
			    ]
			  ]).

% annotating U::FIND-TOP-LEVEL-GOAL-OUTCOME? 
wl: lambda_def(defun,
	      u_find_top_level_goal_outcome_c63,
	      f_u_find_top_level_goal_outcome_c63,
	      [u_top_level_goal, u_context, u_goal_type],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_found_c63, []]],
		  
		  [ u_yfor,
		    u_elem,
		    u_in,
		    [u_cx_c36_get_all_ty, u_context, u_goal_type]
		  ],
		  [u_yuntil, u_found_c63],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_eq_c63, u_top_level_goal, u_elem],
		      [setq, u_found_c63, u_elem]
		    ]
		  ],
		  [u_yresult, u_found_c63]
		]
	      ]).


% annotating U::FIND-TOP-LEVEL-GOAL-OUTCOME? 
wl: arglist_info(u_find_top_level_goal_outcome_c63,
		[u_top_level_goal, u_context, u_goal_type],
		[Top_level_goal_Param, Context_Param, Goal_type_Param],
		arginfo{ all:[u_top_level_goal, u_context, u_goal_type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_context, u_goal_type],
			 opt:0,
			 req:[u_top_level_goal, u_context, u_goal_type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FIND-TOP-LEVEL-GOAL-OUTCOME? 
wl: init_args(exact_only, u_find_top_level_goal_outcome_c63).


% annotating U::FIND-TOP-LEVEL-GOAL-OUTCOME? 
f_u_find_top_level_goal_outcome_c63(Top_level_goal_Param, Context_Param, Goal_type_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_goal_type, Goal_type_Param)],
	f_u_yloop(
		  [ [u_initial, [u_found_c63, []]],
		    
		    [ u_yfor,
		      u_elem,
		      u_in,
		      [u_cx_c36_get_all_ty, u_context, u_goal_type]
		    ],
		    [u_yuntil, u_found_c63],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_top_level_goal, u_elem],
			[setq, u_found_c63, u_elem]
		      ]
		    ],
		    [u_yresult, u_found_c63]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_find_top_level_goal_outcome_c63, classof, claz_function),
   set_opv(u_find_top_level_goal_outcome_c63, compile_as, kw_function),
   set_opv(u_find_top_level_goal_outcome_c63,
	   function,
	   f_u_find_top_level_goal_outcome_c63),
   DefunResult=u_find_top_level_goal_outcome_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was (ob$get elem 'active-goal)",
				     45,
				     13461)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'terminate-top-level-goal',
			    
			    [ 'top-level-goal',
			      'resolved-goal',
			      'resolution-context'
			    ],
			    ['reality-stabilize'],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Terminating planning for top-level goal")
			    ],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'(" ~A"),
			      'top-level-goal'
			    ],
			    ['ndbg-newline', '*gate-dbg*', rule],
			    ['task-print-plans', 'top-level-goal'],
			    
			    [ 'ob$set',
			      'resolved-goal',
			      [quote, 'termination-context'],
			      'resolution-context'
			    ],
			    
			    [ setq,
			      '*top-level-goals*',
			      ['delq!', 'top-level-goal', '*top-level-goals*']
			    ],
			    ['print-tasks'],
			    
			    [ 'remove-motivating-emotions',
			      '*reality*',
			      'resolved-goal'
			    ],
			    
			    [ if,
			      
			      [ or,
				[not, ['dd-goal?', 'resolved-goal']],
				
				[ 'ty$instance?',
				  ['ob$get', 'resolved-goal', [quote, obj]],
				  [quote, revenge]
				]
			      ],
			      
			      [ progn,
				
				[ 'emotional-response',
				  'resolved-goal',
				  [],
				  
				  [ strength,
				    ['ob$get', 'resolved-goal', [quote, obj]]
				  ],
				  'resolution-context'
				]
			      ]
			    ],
			    
			    [ if,
			      
			      [ 'ty$instance?',
				'resolved-goal',
				[quote, 'succeeded-goal']
			      ],
			      
			      [ 'episode-store-top-goal',
				'resolved-goal',
				'resolution-context'
			      ]
			    ],
			    
			    [ if,
			      
			      [ 'eq?',
				[quote, imaginary],
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'planning-type']
				]
			      ],
			      
			      [ 'no-gen',
				['cx$assert', '*reality*', 'resolved-goal'],
				
				[ if,
				  
				  [ 'ty$instance?',
				    'resolved-goal',
				    [quote, 'succeeded-goal']
				  ],
				  
				  [ 'cx$assert',
				    '*reality*',
				    ['ob$get', 'resolved-goal', [quote, obj]]
				  ]
				]
			      ]
			    ],
			    
			    [ if,
			      
			      [ and,
				
				[ 'eq?',
				  [quote, real],
				  
				  [ 'ob$get',
				    'top-level-goal',
				    [quote, 'planning-type']
				  ]
				],
				
				[ 'ty$instance?',
				  'resolved-goal',
				  [quote, 'failed-goal']
				]
			      ],
			      
			      [ 'no-gen',
				['cx$assert', '*reality*', 'resolved-goal']
			      ]
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-xtra',
			      '$STRING'("About to sprout ~A"),
			      '*reality*'
			    ],
			    [setq, '*reality*', ['cx$sprout', '*reality*']],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-xtra',
			      '$STRING'("Back from sprouting")
			    ],
			    [setq, '*reality-lookahead*', '*reality*'],
			    
			    [ 'clear-subgoals',
			      'resolved-goal',
			      '*reality*',
			      '*me-belief-path*'
			    ]
			  ]).

% annotating U::TERMINATE-TOP-LEVEL-GOAL 
wl: lambda_def(defun,
	      u_terminate_top_level_goal,
	      f_u_terminate_top_level_goal,
	      [u_top_level_goal, u_resolved_goal, u_resolution_context],
	      
	      [ [u_reality_stabilize],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('T'),
			     #\(e),
			     #\(r),
			     #\(m),
			     #\(i),
			     #\(n),
			     #\(a),
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
			     #\(f),
			     #\(o),
			     #\(r),
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
		  '$ARRAY'([*], claz_base_character, [#\(' '), #\(~), #\('A')]),
		  u_top_level_goal
		],
		[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		[u_task_print_plans, u_top_level_goal],
		
		[ u_ob_c36_set,
		  u_resolved_goal,
		  [quote, u_termination_context],
		  u_resolution_context
		],
		
		[ setq,
		  u_xx_top_level_goals_xx,
		  [u_delq_c33, u_top_level_goal, u_xx_top_level_goals_xx]
		],
		[u_print_tasks],
		[u_remove_motivating_emotions, u_xx_reality_xx, u_resolved_goal],
		
		[ if,
		  
		  [ or,
		    [not, [u_dd_goal_c63, u_resolved_goal]],
		    
		    [ u_ty_c36_instance_c63,
		      [u_ob_c36_get, u_resolved_goal, [quote, u_obj]],
		      [quote, u_revenge]
		    ]
		  ],
		  
		  [ progn,
		    
		    [ u_emotional_response,
		      u_resolved_goal,
		      [],
		      
		      [ u_strength,
			[u_ob_c36_get, u_resolved_goal, [quote, u_obj]]
		      ],
		      u_resolution_context
		    ]
		  ]
		],
		
		[ if,
		  
		  [ u_ty_c36_instance_c63,
		    u_resolved_goal,
		    [quote, u_succeeded_goal]
		  ],
		  
		  [ u_episode_store_top_goal,
		    u_resolved_goal,
		    u_resolution_context
		  ]
		],
		
		[ if,
		  
		  [ u_eq_c63,
		    [quote, u_imaginary],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		  ],
		  
		  [ u_no_gen,
		    [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal],
		    
		    [ if,
		      
		      [ u_ty_c36_instance_c63,
			u_resolved_goal,
			[quote, u_succeeded_goal]
		      ],
		      
		      [ u_cx_c36_assert,
			u_xx_reality_xx,
			[u_ob_c36_get, u_resolved_goal, [quote, u_obj]]
		      ]
		    ]
		  ]
		],
		
		[ if,
		  
		  [ and,
		    
		    [ u_eq_c63,
		      [quote, real],
		      [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		    ],
		    
		    [ u_ty_c36_instance_c63,
		      u_resolved_goal,
		      [quote, u_failed_goal]
		    ]
		  ],
		  [u_no_gen, [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal]]
		],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_xtra,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(b),
			     #\(o),
			     #\(u),
			     #\(t),
			     #\(' '),
			     #\(t),
			     #\(o),
			     #\(' '),
			     #\(s),
			     #\(p),
			     #\(r),
			     #\(o),
			     #\(u),
			     #\(t),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_xx_reality_xx
		],
		[setq, u_xx_reality_xx, [u_cx_c36_sprout, u_xx_reality_xx]],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_xtra,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('B'),
			     #\(a),
			     #\(c),
			     #\(k),
			     #\(' '),
			     #\(f),
			     #\(r),
			     #\(o),
			     #\(m),
			     #\(' '),
			     #\(s),
			     #\(p),
			     #\(r),
			     #\(o),
			     #\(u),
			     #\(t),
			     #\(i),
			     #\(n),
			     #\(g)
			   ])
		],
		[setq, u_xx_reality_lookahead_xx, u_xx_reality_xx],
		
		[ u_clear_subgoals,
		  u_resolved_goal,
		  u_xx_reality_xx,
		  u_xx_me_belief_path_xx
		]
	      ]).


% annotating U::TERMINATE-TOP-LEVEL-GOAL 
wl: arglist_info(u_terminate_top_level_goal,
		[u_top_level_goal, u_resolved_goal, u_resolution_context],
		
		[ Top_level_goal_Param,
		  Resolved_goal_Param,
		  Resolution_context_Param
		],
		arginfo{ all:
			     [ u_top_level_goal,
			       u_resolved_goal,
			       u_resolution_context
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_top_level_goal,
				 u_resolved_goal,
				 u_resolution_context
			       ],
			 opt:0,
			 req:
			     [ u_top_level_goal,
			       u_resolved_goal,
			       u_resolution_context
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TERMINATE-TOP-LEVEL-GOAL 
wl: init_args(exact_only, u_terminate_top_level_goal).


% annotating U::TERMINATE-TOP-LEVEL-GOAL 
f_u_terminate_top_level_goal(Top_level_goal_Param, Resolved_goal_Param, Resolution_context_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_resolved_goal, Resolved_goal_Param), bv(u_resolution_context, Resolution_context_Param)],
	f_u_reality_stabilize(Reality_stabilize_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\('T'),
				    #\(e),
				    #\(r),
				    #\(m),
				    #\(i),
				    #\(n),
				    #\(a),
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
				    #\(f),
				    #\(o),
				    #\(r),
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
				  [#\(' '), #\(~), #\('A')]),
			 u_top_level_goal
		       ],
		       Ndbg_roman_Ret60),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	f_u_task_print_plans(Top_level_goal_Param, Print_plans_Ret),
	f_u_ob_c36_set(Resolved_goal_Param,
		       u_termination_context,
		       Resolution_context_Param,
		       C36_set_Ret),
	f_u_delq_c33(u_top_level_goal,
		     u_xx_top_level_goals_xx,
		     Xx_top_level_goals_xx),
	set_var(Env, u_xx_top_level_goals_xx, Xx_top_level_goals_xx),
	f_u_print_tasks(Print_tasks_Ret),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get45),
	f_u_remove_motivating_emotions(Xx_reality_xx_Get45,
				       Resolved_goal_Param,
				       Motivating_emotions_Ret),
	(   f_u_dd_goal_c63(Resolved_goal_Param, Not_Param),
	    cl_not(Not_Param, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   f_u_ob_c36_get(Resolved_goal_Param, u_obj, Obj),
	    f_u_ty_c36_instance_c63(Obj, u_revenge, Revenge),
	    IFTEST=Revenge
	),
	(   IFTEST\==[]
	->  f_u_strength([u_ob_c36_get, u_resolved_goal, [quote, u_obj]],
			 Strength_Ret),
	    f_u_emotional_response(Resolved_goal_Param,
				   [],
				   Strength_Ret,
				   Resolution_context_Param,
				   TrueResult),
	    _49302=TrueResult
	;   _49302=[]
	),
	f_u_ty_c36_instance_c63(Resolved_goal_Param, u_succeeded_goal, IFTEST29),
	(   IFTEST29\==[]
	->  f_u_episode_store_top_goal(Resolved_goal_Param,
				       Resolution_context_Param,
				       TrueResult34),
	    _49496=TrueResult34
	;   _49496=[]
	),
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST35),
	(   IFTEST35\==[]
	->  f_u_no_gen(
		       [ [u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal],
			 
			 [ if,
			   
			   [ u_ty_c36_instance_c63,
			     u_resolved_goal,
			     [quote, u_succeeded_goal]
			   ],
			   
			   [ u_cx_c36_assert,
			     u_xx_reality_xx,
			     [u_ob_c36_get, u_resolved_goal, [quote, u_obj]]
			   ]
			 ]
		       ],
		       TrueResult37),
	    _49670=TrueResult37
	;   _49670=[]
	),
	f_u_eq_c63([quote, real],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST40),
	(   IFTEST40\==[]
	->  f_u_ty_c36_instance_c63(Resolved_goal_Param,
				    u_failed_goal,
				    TrueResult43),
	    IFTEST38=TrueResult43
	;   IFTEST38=[]
	),
	(   IFTEST38\==[]
	->  f_u_no_gen([[u_cx_c36_assert, u_xx_reality_xx, u_resolved_goal]],
		       TrueResult44),
	    _49742=TrueResult44
	;   _49742=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(b),
				       #\(o),
				       #\(u),
				       #\(t),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(s),
				       #\(p),
				       #\(r),
				       #\(o),
				       #\(u),
				       #\(t),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_xx_reality_xx
			  ],
			  Roman_nl_Ret),
	f_u_cx_c36_sprout(Xx_reality_xx_Get45, Xx_reality_xx),
	set_var(Env, u_xx_reality_xx, Xx_reality_xx),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('B'),
				       #\(a),
				       #\(c),
				       #\(k),
				       #\(' '),
				       #\(f),
				       #\(r),
				       #\(o),
				       #\(m),
				       #\(' '),
				       #\(s),
				       #\(p),
				       #\(r),
				       #\(o),
				       #\(u),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g)
				     ])
			  ],
			  Roman_nl_Ret67),
	set_var(Env, u_xx_reality_lookahead_xx, Xx_reality_xx_Get45),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_clear_subgoals(Resolved_goal_Param,
			   Xx_reality_xx_Get45,
			   Xx_me_belief_path_xx_Get,
			   Clear_subgoals_Ret),
	Clear_subgoals_Ret=FnResult.
:- set_opv(f_u_terminate_top_level_goal, classof, claz_function),
   set_opv(u_terminate_top_level_goal, compile_as, kw_function),
   set_opv(u_terminate_top_level_goal, function, f_u_terminate_top_level_goal),
   DefunResult=u_terminate_top_level_goal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" necessary?", 23, 13699)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Stop any future planning on this top-level goal",
				     3,
				     13973)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Update to trace the list of tasks",
				     3,
				     14093)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Remove emotional motivators for active goal (which got carried",
				     3,
				     14147)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" over to resolved goal)", 3, 14214)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Invoke general emotional response",
				     3,
				     14296)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (gen-overall-emot-state) Now done in dd_gen",
				     1,
				     14607)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Store the successful planning episode in episodic memory",
				     3,
				     14664)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If top-level goal was imaginary planning, assert final goal status",
				     3,
				     14841)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (and objective if goal success) into reality context.",
				     3,
				     14912)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If top-level goal was real planning and goal failure, we better assert",
				     3,
				     15209)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the failure into the reality context in case it isn't already there.",
				     3,
				     15284)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (gc-plans1 *reality* (list top-level-goal))",
				     1,
				     15794)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If we use gc-plans1 here, we must modify it so it does not retract",
				     1,
				     15840)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:13561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" side-effect goal outcomes.", 1, 15909)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:15940 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'remove-motivating-emotions',
			    [context, goal],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Removing motivating emotions of ~A in ~A"),
			      goal,
			      context
			    ],
			    
			    [ let,
			      
			      [ 
				[ dependencies,
				  
				  [ 'get-dependencies',
				    goal,
				    context,
				    '*me-belief-path*'
				  ]
				]
			      ],
			      
			      [ yloop,
				[yfor, dependency, in, dependencies],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'ty$instance?',
				      
				      [ 'ob$get',
					dependency,
					[quote, 'linked-from']
				      ],
				      [quote, emotion]
				    ],
				    
				    [ progn,
				      ['cx$retract', context, dependency],
				      
				      [ 'remove-if-free',
					
					[ 'ob$get',
					  dependency,
					  [quote, 'linked-from']
					],
					context
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::REMOVE-MOTIVATING-EMOTIONS 
wl: lambda_def(defun,
	      u_remove_motivating_emotions,
	      f_u_remove_motivating_emotions,
	      [u_context, u_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(e),
			     #\(m),
			     #\(o),
			     #\(v),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(m),
			     #\(o),
			     #\(t),
			     #\(i),
			     #\(v),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(n),
			     #\(g),
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
		  u_goal,
		  u_context
		],
		
		[ let,
		  
		  [ 
		    [ u_dependencies,
		      
		      [ u_get_dependencies,
			u_goal,
			u_context,
			u_xx_me_belief_path_xx
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_dependency, u_in, u_dependencies],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_dependency, [quote, u_linked_from]],
			  [quote, u_emotion]
			],
			
			[ progn,
			  [u_cx_c36_retract, u_context, u_dependency],
			  
			  [ u_remove_if_free,
			    [u_ob_c36_get, u_dependency, [quote, u_linked_from]],
			    u_context
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::REMOVE-MOTIVATING-EMOTIONS 
wl: arglist_info(u_remove_motivating_emotions,
		[u_context, u_goal],
		[Context_Param, Goal_Param],
		arginfo{ all:[u_context, u_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_goal],
			 opt:0,
			 req:[u_context, u_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REMOVE-MOTIVATING-EMOTIONS 
wl: init_args(exact_only, u_remove_motivating_emotions).


% annotating U::REMOVE-MOTIVATING-EMOTIONS 
f_u_remove_motivating_emotions(Context_Param, Goal_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_goal, Goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(m),
				       #\(o),
				       #\(v),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(m),
				       #\(o),
				       #\(t),
				       #\(i),
				       #\(v),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
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
			    u_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_get_dependencies(Goal_Param,
			     Context_Param,
			     Xx_me_belief_path_xx_Get,
			     Dependencies_Init),
	LEnv=[[bv(u_dependencies, Dependencies_Init)]|Env],
	f_u_yloop(
		  [ [u_yfor, u_dependency, u_in, u_dependencies],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_dependency, [quote, u_linked_from]],
			  [quote, u_emotion]
			],
			
			[ progn,
			  [u_cx_c36_retract, u_context, u_dependency],
			  
			  [ u_remove_if_free,
			    [u_ob_c36_get, u_dependency, [quote, u_linked_from]],
			    u_context
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_remove_motivating_emotions, classof, claz_function),
   set_opv(u_remove_motivating_emotions, compile_as, kw_function),
   set_opv(u_remove_motivating_emotions,
	   function,
	   f_u_remove_motivating_emotions),
   DefunResult=u_remove_motivating_emotions.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16519 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'remove-if-free',
			    [emotion, context],
			    
			    [ let,
			      
			      [ 
				[ dependees,
				  
				  [ 'get-dependees',
				    emotion,
				    context,
				    '*me-belief-path*'
				  ]
				],
				
				[ dependencies,
				  
				  [ 'get-dependencies',
				    emotion,
				    context,
				    '*me-belief-path*'
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  ['null?', dependees],
				  ['null?', dependencies]
				],
				
				[ progn,
				  ['cx$retract', context, emotion],
				  ['emotion-delete', emotion]
				]
			      ]
			    ]
			  ]).

% annotating U::REMOVE-IF-FREE 
wl: lambda_def(defun,
	      u_remove_if_free,
	      f_u_remove_if_free,
	      [u_emotion, u_context],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_dependees,
		      
		      [ u_get_dependees,
			u_emotion,
			u_context,
			u_xx_me_belief_path_xx
		      ]
		    ],
		    
		    [ u_dependencies,
		      
		      [ u_get_dependencies,
			u_emotion,
			u_context,
			u_xx_me_belief_path_xx
		      ]
		    ]
		  ],
		  
		  [ if,
		    [and, [u_null_c63, u_dependees], [u_null_c63, u_dependencies]],
		    
		    [ progn,
		      [u_cx_c36_retract, u_context, u_emotion],
		      [u_emotion_delete, u_emotion]
		    ]
		  ]
		]
	      ]).


% annotating U::REMOVE-IF-FREE 
wl: arglist_info(u_remove_if_free,
		[u_emotion, u_context],
		[Emotion_Param, Context_Param],
		arginfo{ all:[u_emotion, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emotion, u_context],
			 opt:0,
			 req:[u_emotion, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REMOVE-IF-FREE 
wl: init_args(exact_only, u_remove_if_free).


% annotating U::REMOVE-IF-FREE 
f_u_remove_if_free(Emotion_Param, Context_Param, FnResult) :-
	Env=[bv(u_emotion, Emotion_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_get_dependees(Emotion_Param,
			  Context_Param,
			  Xx_me_belief_path_xx_Get,
			  Dependees_Init),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get21),
	f_u_get_dependencies(Emotion_Param,
			     Context_Param,
			     Xx_me_belief_path_xx_Get21,
			     Dependencies_Init),
	LEnv=[[bv(u_dependees, Dependees_Init), bv(u_dependencies, Dependencies_Init)]|Env],
	f_u_null_c63(u_dependees, IFTEST26),
	(   IFTEST26\==[]
	->  f_u_null_c63(u_dependencies, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_cx_c36_retract(Context_Param, Emotion_Param, C36_retract_Ret),
	    f_u_emotion_delete(Emotion_Param, TrueResult33),
	    FnResult=TrueResult33
	;   FnResult=[]
	).
:- set_opv(f_u_remove_if_free, classof, claz_function),
   set_opv(u_remove_if_free, compile_as, kw_function),
   set_opv(u_remove_if_free, function, f_u_remove_if_free),
   DefunResult=u_remove_if_free.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16519 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Assumes reality is stabilized.",
				     1,
				     16878)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'emotional-response',
			    ['resolved-goal', 'altern?', realism, context],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Emotional responses for ~A in ~A"),
			      'resolved-goal',
			      context
			    ],
			    
			    [ 'let*',
			      
			      [ 
				[ 'other-causes',
				  ['get-other-causes', 'resolved-goal', context]
				],
				[emot, []]
			      ],
			      
			      [ setq,
				emot,
				
				[ if,
				  'other-causes',
				  
				  [ if,
				    'altern?',
				    
				    [ if,
				      
				      [ 'ty$instance?',
					'resolved-goal',
					[quote, 'succeeded-goal']
				      ],
				      
				      [ 'ob$fcreate',
					
					[ '#BQ',
					  
					  [ 'NEG-EMOTION',
					    to,
					    
					    [ '#COMMA',
					      
					      [ 'ob$get',
						[car, 'other-causes'],
						[quote, actor]
					      ]
					    ],
					    'altern?',
					    t
					  ]
					]
				      ],
				      
				      [ 'ob$fcreate',
					[quote, ['POS-EMOTION', 'altern?', t]]
				      ]
				    ],
				    
				    [ if,
				      
				      [ 'ty$instance?',
					'resolved-goal',
					[quote, 'succeeded-goal']
				      ],
				      ['ob$fcreate', [quote, ['POS-EMOTION']]],
				      
				      [ 'ob$fcreate',
					
					[ '#BQ',
					  
					  [ 'NEG-EMOTION',
					    to,
					    
					    [ '#COMMA',
					      
					      [ 'ob$get',
						[car, 'other-causes'],
						[quote, actor]
					      ]
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ if,
				    'altern?',
				    
				    [ if,
				      
				      [ 'ty$instance?',
					'resolved-goal',
					[quote, 'succeeded-goal']
				      ],
				      
				      [ 'ob$fcreate',
					[quote, ['NEG-EMOTION', 'altern?', t]]
				      ],
				      
				      [ 'ob$fcreate',
					[quote, ['POS-EMOTION', 'altern?', t]]
				      ]
				    ],
				    
				    [ if,
				      
				      [ 'ty$instance?',
					'resolved-goal',
					[quote, 'succeeded-goal']
				      ],
				      ['ob$fcreate', [quote, ['POS-EMOTION']]],
				      ['ob$fcreate', [quote, ['NEG-EMOTION']]]
				    ]
				  ]
				]
			      ],
			      
			      [ 'add-emotion',
				'resolved-goal',
				emot,
				realism,
				context
			      ],
			      emot
			    ]
			  ]).

% annotating U::EMOTIONAL-RESPONSE 
wl: lambda_def(defun,
	      u_emotional_response,
	      f_u_emotional_response,
	      [u_resolved_goal, u_altern_c63, u_realism, u_context],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('E'),
			     #\(m),
			     #\(o),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(r),
			     #\(e),
			     #\(s),
			     #\(p),
			     #\(o),
			     #\(n),
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
			     #\('A')
			   ]),
		  u_resolved_goal,
		  u_context
		],
		
		[ let_xx,
		  
		  [ 
		    [ u_other_causes,
		      [u_get_other_causes, u_resolved_goal, u_context]
		    ],
		    [u_emot, []]
		  ],
		  
		  [ setq,
		    u_emot,
		    
		    [ if,
		      u_other_causes,
		      
		      [ if,
			u_altern_c63,
			
			[ if,
			  
			  [ u_ty_c36_instance_c63,
			    u_resolved_goal,
			    [quote, u_succeeded_goal]
			  ],
			  
			  [ u_ob_c36_fcreate,
			    
			    [ '#BQ',
			      
			      [ u_neg_emotion,
				u_to,
				
				[ '#COMMA',
				  
				  [ u_ob_c36_get,
				    [car, u_other_causes],
				    [quote, u_actor]
				  ]
				],
				u_altern_c63,
				t
			      ]
			    ]
			  ],
			  
			  [ u_ob_c36_fcreate,
			    [quote, [u_pos_emotion, u_altern_c63, t]]
			  ]
			],
			
			[ if,
			  
			  [ u_ty_c36_instance_c63,
			    u_resolved_goal,
			    [quote, u_succeeded_goal]
			  ],
			  [u_ob_c36_fcreate, [quote, [u_pos_emotion]]],
			  
			  [ u_ob_c36_fcreate,
			    
			    [ '#BQ',
			      
			      [ u_neg_emotion,
				u_to,
				
				[ '#COMMA',
				  
				  [ u_ob_c36_get,
				    [car, u_other_causes],
				    [quote, u_actor]
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ if,
			u_altern_c63,
			
			[ if,
			  
			  [ u_ty_c36_instance_c63,
			    u_resolved_goal,
			    [quote, u_succeeded_goal]
			  ],
			  
			  [ u_ob_c36_fcreate,
			    [quote, [u_neg_emotion, u_altern_c63, t]]
			  ],
			  
			  [ u_ob_c36_fcreate,
			    [quote, [u_pos_emotion, u_altern_c63, t]]
			  ]
			],
			
			[ if,
			  
			  [ u_ty_c36_instance_c63,
			    u_resolved_goal,
			    [quote, u_succeeded_goal]
			  ],
			  [u_ob_c36_fcreate, [quote, [u_pos_emotion]]],
			  [u_ob_c36_fcreate, [quote, [u_neg_emotion]]]
			]
		      ]
		    ]
		  ],
		  [u_add_emotion, u_resolved_goal, u_emot, u_realism, u_context],
		  u_emot
		]
	      ]).


% annotating U::EMOTIONAL-RESPONSE 
wl: arglist_info(u_emotional_response,
		[u_resolved_goal, u_altern_c63, u_realism, u_context],
		
		[ Resolved_goal_Param,
		  Altern_c63_Param,
		  Realism_Param,
		  Context_Param
		],
		arginfo{ all:
			     [ u_resolved_goal,
			       u_altern_c63,
			       u_realism,
			       u_context
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_resolved_goal,
				 u_altern_c63,
				 u_realism,
				 u_context
			       ],
			 opt:0,
			 req:
			     [ u_resolved_goal,
			       u_altern_c63,
			       u_realism,
			       u_context
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EMOTIONAL-RESPONSE 
wl: init_args(exact_only, u_emotional_response).


% annotating U::EMOTIONAL-RESPONSE 
f_u_emotional_response(Resolved_goal_Param, Altern_c63_Param, Realism_Param, Context_Param, FnResult) :-
	Env=[bv(u_resolved_goal, Resolved_goal_Param), bv(u_altern_c63, Altern_c63_Param), bv(u_realism, Realism_Param), bv(u_context, Context_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('E'),
				       #\(m),
				       #\(o),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(s),
				       #\(p),
				       #\(o),
				       #\(n),
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
				       #\('A')
				     ]),
			    u_resolved_goal,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_get_other_causes(Resolved_goal_Param,
			     Context_Param,
			     Other_causes_Init),
	LEnv=[[bv(u_other_causes, Other_causes_Init)]|Env],
	Env=[[bv(u_emot, [])]|LEnv],
	get_var(Env, u_other_causes, IFTEST),
	(   IFTEST\==[]
	->  (   Altern_c63_Param\==[]
	    ->  f_u_ty_c36_instance_c63(Resolved_goal_Param,
					u_succeeded_goal,
					IFTEST32),
		(   IFTEST32\==[]
		->  f_u_ob_c36_fcreate(
				       [ '#BQ',
					 
					 [ u_neg_emotion,
					   u_to,
					   
					   [ '#COMMA',
					     
					     [ u_ob_c36_get,
					       [car, u_other_causes],
					       [quote, u_actor]
					     ]
					   ],
					   u_altern_c63,
					   t
					 ]
				       ],
				       TrueResult),
		    TrueResult42=TrueResult
		;   f_u_ob_c36_fcreate([quote, [u_pos_emotion, u_altern_c63, t]],
				       ElseResult),
		    TrueResult42=ElseResult
		)
	    ;   f_u_ty_c36_instance_c63(Resolved_goal_Param,
					u_succeeded_goal,
					IFTEST37),
		(   IFTEST37\==[]
		->  f_u_ob_c36_fcreate([quote, [u_pos_emotion]], TrueResult40),
		    TrueResult42=TrueResult40
		;   f_u_ob_c36_fcreate(
				       [ '#BQ',
					 
					 [ u_neg_emotion,
					   u_to,
					   
					   [ '#COMMA',
					     
					     [ u_ob_c36_get,
					       [car, u_other_causes],
					       [quote, u_actor]
					     ]
					   ]
					 ]
				       ],
				       ElseResult41),
		    TrueResult42=ElseResult41
		)
	    )
	;   Altern_c63_Param\==[]
	->  f_u_ty_c36_instance_c63(Resolved_goal_Param,
				    u_succeeded_goal,
				    IFTEST47),
	    (   IFTEST47\==[]
	    ->  f_u_ob_c36_fcreate([quote, [u_neg_emotion, u_altern_c63, t]],
				   TrueResult50),
		TrueResult42=TrueResult50
	    ;   f_u_ob_c36_fcreate([quote, [u_pos_emotion, u_altern_c63, t]],
				   ElseResult51),
		TrueResult42=ElseResult51
	    )
	;   f_u_ty_c36_instance_c63(Resolved_goal_Param,
				    u_succeeded_goal,
				    IFTEST52),
	    (   IFTEST52\==[]
	    ->  f_u_ob_c36_fcreate([quote, [u_pos_emotion]], TrueResult55),
		TrueResult42=TrueResult55
	    ;   f_u_ob_c36_fcreate([quote, [u_neg_emotion]], ElseResult56),
		TrueResult42=ElseResult56
	    )
	),
	set_var(Env, u_emot, TrueResult42),
	get_var(Env, u_emot, Emot_Get65),
	f_u_add_emotion(Resolved_goal_Param,
			Emot_Get65,
			Realism_Param,
			Context_Param,
			Add_emotion_Ret),
	LetResult=Emot_Get65,
	LetResult=FnResult.
:- set_opv(f_u_emotional_response, classof, claz_function),
   set_opv(u_emotional_response, compile_as, kw_function),
   set_opv(u_emotional_response, function, f_u_emotional_response),
   DefunResult=u_emotional_response.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Actually, as coded currently, get-other-causes returns a bd list",
				     3,
				     17094)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (whose car is an other cause).",
				     3,
				     17163)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" angry at someone for not doing something",
				     27,
				     17459)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Should we substitute function 'first-non-me'",
				     27,
				     17528)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" which is used in generator? No, other-causes",
				     27,
				     17601)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:16910 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" will not contain a me.", 27, 17674)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18855 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*emotions*', []]).
:- set_var(TLEnv3, setq, u_xx_emotions_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18855 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note that goal may not be in *reality*. Will this cause inconsistencies",
				     1,
				     18879)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18855 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" later?", 1, 18953)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18961 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-emotion',
			    [goal, emotion, realism, context],
			    ['emotion-add', emotion],
			    
			    [ 'add-depend',
			      '*reality*',
			      goal,
			      emotion,
			      realism,
			      0.0,
			      0.0,
			      []
			    ],
			    ['cx$assert', '*reality*', emotion],
			    
			    [ if,
			      ['neq?', '*reality*', context],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'rule-long',
				  '$STRING'("Running reality inferences")
				],
				
				[ 'run-inferences',
				  '*reality*',
				  [],
				  '*me-belief-path*'
				]
			      ]
			    ]
			  ]).

% annotating U::ADD-EMOTION 
wl: lambda_def(defun,
	      u_add_emotion,
	      f_u_add_emotion,
	      [u_goal, u_emotion, u_realism, u_context],
	      
	      [ [u_emotion_add, u_emotion],
		
		[ u_add_depend,
		  u_xx_reality_xx,
		  u_goal,
		  u_emotion,
		  u_realism,
		  0.0,
		  0.0,
		  []
		],
		[u_cx_c36_assert, u_xx_reality_xx, u_emotion],
		
		[ if,
		  [u_neq_c63, u_xx_reality_xx, u_context],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule_long,
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
				 #\(r),
				 #\(e),
				 #\(a),
				 #\(l),
				 #\(i),
				 #\(t),
				 #\(y),
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
				 #\(s)
			       ])
		    ],
		    
		    [ u_run_inferences,
		      u_xx_reality_xx,
		      [],
		      u_xx_me_belief_path_xx
		    ]
		  ]
		]
	      ]).


% annotating U::ADD-EMOTION 
wl: arglist_info(u_add_emotion,
		[u_goal, u_emotion, u_realism, u_context],
		[Goal_Param, Emotion_Param, Realism_Param, Context_Param],
		arginfo{ all:[u_goal, u_emotion, u_realism, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_emotion, u_realism, u_context],
			 opt:0,
			 req:[u_goal, u_emotion, u_realism, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ADD-EMOTION 
wl: init_args(exact_only, u_add_emotion).


% annotating U::ADD-EMOTION 
f_u_add_emotion(Goal_Param, Emotion_Param, Realism_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_emotion, Emotion_Param), bv(u_realism, Realism_Param), bv(u_context, Context_Param)],
	f_u_emotion_add(Emotion_Param, Emotion_add_Ret),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_add_depend(Xx_reality_xx_Get,
		       Goal_Param,
		       Emotion_Param,
		       Realism_Param,
		       0.0,
		       0.0,
		       [],
		       Add_depend_Ret),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get23),
	f_u_cx_c36_assert(Xx_reality_xx_Get23, Emotion_Param, C36_assert_Ret),
	f_u_neq_c63(u_xx_reality_xx, u_context, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_long,
			      
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
					   #\(r),
					   #\(e),
					   #\(a),
					   #\(l),
					   #\(i),
					   #\(t),
					   #\(y),
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
					   #\(s)
					 ])
			      ],
			      Roman_nl_Ret),
	    get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get27),
	    f_u_run_inferences(Xx_reality_xx_Get27,
			       [],
			       Xx_me_belief_path_xx_Get,
			       TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_add_emotion, classof, claz_function),
   set_opv(u_add_emotion, compile_as, kw_function),
   set_opv(u_add_emotion, function, f_u_add_emotion),
   DefunResult=u_add_emotion.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18961 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The purpose of the below is to activate any daydreaming goals",
				     3,
				     19128)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18961 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" that should be activated as a result of the new emotion.",
				     3,
				     19194)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:18961 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This is called from run-inferences.",
				     1,
				     19429)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'personal-goal-outcome',
			    [goal, context, 'top-level-goal'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Personal goal outcome ~A in ~A"),
			      goal,
			      context
			    ],
			    ['reality-stabilize'],
			    
			    [ 'ob$set',
			      goal,
			      [quote, 'termination-context'],
			      context
			    ],
			    
			    [ let,
			      
			      [ 
				[ emot,
				  
				  [ 'emotional-response',
				    goal,
				    ['altern?', context],
				    [strength, ['ob$get', goal, [quote, obj]]],
				    context
				  ]
				]
			      ],
			      
			      [ if,
				
				[ 'ty$instance?',
				  ['ob$get', 'top-level-goal', [quote, obj]],
				  [quote, rationalization]
				],
				
				[ 'divert-emot-to-tlg',
				  emot,
				  context,
				  'top-level-goal'
				]
			      ]
			    ]
			  ]).

% annotating U::PERSONAL-GOAL-OUTCOME 
wl: lambda_def(defun,
	      u_personal_goal_outcome,
	      f_u_personal_goal_outcome,
	      [u_goal, u_context, u_top_level_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(e),
			     #\(r),
			     #\(s),
			     #\(o),
			     #\(n),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(o),
			     #\(u),
			     #\(t),
			     #\(c),
			     #\(o),
			     #\(m),
			     #\(e),
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
		[u_reality_stabilize],
		[u_ob_c36_set, u_goal, [quote, u_termination_context], u_context],
		
		[ let,
		  
		  [ 
		    [ u_emot,
		      
		      [ u_emotional_response,
			u_goal,
			[u_altern_c63, u_context],
			[u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]],
			u_context
		      ]
		    ]
		  ],
		  
		  [ if,
		    
		    [ u_ty_c36_instance_c63,
		      [u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
		      [quote, u_rationalization]
		    ],
		    [u_divert_emot_to_tlg, u_emot, u_context, u_top_level_goal]
		  ]
		]
	      ]).


% annotating U::PERSONAL-GOAL-OUTCOME 
wl: arglist_info(u_personal_goal_outcome,
		[u_goal, u_context, u_top_level_goal],
		[Goal_Param, Context_Param, Top_level_goal_Param],
		arginfo{ all:[u_goal, u_context, u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context, u_top_level_goal],
			 opt:0,
			 req:[u_goal, u_context, u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERSONAL-GOAL-OUTCOME 
wl: init_args(exact_only, u_personal_goal_outcome).


% annotating U::PERSONAL-GOAL-OUTCOME 
f_u_personal_goal_outcome(Goal_Param, Context_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(o),
				       #\(n),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(o),
				       #\(u),
				       #\(t),
				       #\(c),
				       #\(o),
				       #\(m),
				       #\(e),
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
	f_u_reality_stabilize(Reality_stabilize_Ret),
	f_u_ob_c36_set(Goal_Param,
		       u_termination_context,
		       Context_Param,
		       C36_set_Ret),
	f_u_altern_c63(Context_Param, Altern_c63_Ret),
	f_u_strength([u_ob_c36_get, u_goal, [quote, u_obj]], Strength_Ret),
	f_u_emotional_response(Goal_Param,
			       Altern_c63_Ret,
			       Strength_Ret,
			       Context_Param,
			       Emot_Init),
	LEnv=[[bv(u_emot, Emot_Init)]|Env],
	f_u_ob_c36_get(Top_level_goal_Param, u_obj, Obj),
	f_u_ty_c36_instance_c63(Obj, u_rationalization, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_emot, Emot_Get),
	    f_u_divert_emot_to_tlg(Emot_Get,
				   Context_Param,
				   Top_level_goal_Param,
				   TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_personal_goal_outcome, classof, claz_function),
   set_opv(u_personal_goal_outcome, compile_as, kw_function),
   set_opv(u_personal_goal_outcome, function, f_u_personal_goal_outcome),
   DefunResult=u_personal_goal_outcome.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" yes?", 23, 19628)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (gen-overall-emot-state) Now done in dd_gen",
				     1,
				     19945)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Divert strength of new emotion to main motivator of a top-level goal.",
				     1,
				     20001)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Used for rationalization and surprise.",
				     1,
				     20073)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  Why do we have to do it this way? Why do we have to divert to the",
				     1,
				     20114)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" main motivator? Why can't we just connect a new emotion up to the",
				     1,
				     20183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  goal and recalculate its value? Was this because of something that is",
				     1,
				     20251)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" no longer true?", 1, 20324)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The reason is that 1) Rationalization-Inf1 works off of the emotion",
				     1,
				     20342)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" which originally activated rationalization; 2) we actually want",
				     1,
				     20412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" an initial NEG-EMOTION to be nulled out of existence through",
				     1,
				     20478)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" rationalization rather than simply summing in POS-EMOTIONs to",
				     1,
				     20541)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:19466 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" counteract.", 1, 20605)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:20618 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'divert-emot-to-tlg',
			    [emot, context, 'top-level-goal'],
			    
			    [ let,
			      
			      [ 
				[ 'main-motiv',
				  
				  [ 'ob$get',
				    'top-level-goal',
				    [quote, 'main-motiv']
				  ]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Divert strength of ~A to ~A"),
				emot,
				'top-level-goal'
			      ],
			      
			      [ 'modify-strength',
				'*reality*',
				'main-motiv',
				[strength, emot],
				['sign-correction', emot, 'main-motiv']
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Null out charge of ~A"),
				emot
			      ],
			      
			      [ 'modify-strength',
				'*reality*',
				emot,
				[strength, emot],
				-1.0
			      ],
			      ['print-tasks']
			    ]
			  ]).

% annotating U::DIVERT-EMOT-TO-TLG 
wl: lambda_def(defun,
	      u_divert_emot_to_tlg,
	      f_u_divert_emot_to_tlg,
	      [u_emot, u_context, u_top_level_goal],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_main_motiv,
		      [u_ob_c36_get, u_top_level_goal, [quote, u_main_motiv]]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('D'),
			       #\(i),
			       #\(v),
			       #\(e),
			       #\(r),
			       #\(t),
			       #\(' '),
			       #\(s),
			       #\(t),
			       #\(r),
			       #\(e),
			       #\(n),
			       #\(g),
			       #\(t),
			       #\(h),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(t),
			       #\(o),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_emot,
		    u_top_level_goal
		  ],
		  
		  [ u_modify_strength,
		    u_xx_reality_xx,
		    u_main_motiv,
		    [u_strength, u_emot],
		    [u_sign_correction, u_emot, u_main_motiv]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('N'),
			       #\(u),
			       #\(l),
			       #\(l),
			       #\(' '),
			       #\(o),
			       #\(u),
			       #\(t),
			       #\(' '),
			       #\(c),
			       #\(h),
			       #\(a),
			       #\(r),
			       #\(g),
			       #\(e),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_emot
		  ],
		  
		  [ u_modify_strength,
		    u_xx_reality_xx,
		    u_emot,
		    [u_strength, u_emot],
		    -1.0
		  ],
		  [u_print_tasks]
		]
	      ]).


% annotating U::DIVERT-EMOT-TO-TLG 
wl: arglist_info(u_divert_emot_to_tlg,
		[u_emot, u_context, u_top_level_goal],
		[Emot_Param, Context_Param, Top_level_goal_Param],
		arginfo{ all:[u_emot, u_context, u_top_level_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emot, u_context, u_top_level_goal],
			 opt:0,
			 req:[u_emot, u_context, u_top_level_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DIVERT-EMOT-TO-TLG 
wl: init_args(exact_only, u_divert_emot_to_tlg).


% annotating U::DIVERT-EMOT-TO-TLG 
f_u_divert_emot_to_tlg(Emot_Param, Context_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_emot, Emot_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ob_c36_get(Top_level_goal_Param, u_main_motiv, Main_motiv_Init),
	LEnv=[[bv(u_main_motiv, Main_motiv_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('D'),
				       #\(i),
				       #\(v),
				       #\(e),
				       #\(r),
				       #\(t),
				       #\(' '),
				       #\(s),
				       #\(t),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(g),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_emot,
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	get_var(LEnv, u_main_motiv, Main_motiv_Get),
	get_var(LEnv, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_strength(u_emot, Strength_Ret),
	get_var(LEnv, u_main_motiv, Main_motiv_Get24),
	f_u_sign_correction(Emot_Param, Main_motiv_Get24, Sign_correction_Ret),
	f_u_modify_strength(Xx_reality_xx_Get,
			    Main_motiv_Get,
			    Strength_Ret,
			    Sign_correction_Ret,
			    Modify_strength_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('N'),
				       #\(u),
				       #\(l),
				       #\(l),
				       #\(' '),
				       #\(o),
				       #\(u),
				       #\(t),
				       #\(' '),
				       #\(c),
				       #\(h),
				       #\(a),
				       #\(r),
				       #\(g),
				       #\(e),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_emot
			  ],
			  Roman_nl_Ret33),
	get_var(LEnv, u_xx_reality_xx, Xx_reality_xx_Get25),
	f_u_strength(u_emot, Strength_Ret34),
	f_u_modify_strength(Xx_reality_xx_Get25,
			    Emot_Param,
			    Strength_Ret34,
			    -1.0,
			    Modify_strength_Ret35),
	f_u_print_tasks(Print_tasks_Ret),
	LetResult=Print_tasks_Ret,
	LetResult=FnResult.
:- set_opv(f_u_divert_emot_to_tlg, classof, claz_function),
   set_opv(u_divert_emot_to_tlg, compile_as, kw_function),
   set_opv(u_divert_emot_to_tlg, function, f_u_divert_emot_to_tlg),
   DefunResult=u_divert_emot_to_tlg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21116 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'sign-correction',
			    [emot1, emot2],
			    
			    [ cond,
			      
			      [ 
				[ or,
				  
				  [ and,
				    
				    [ 'ty$instance?',
				      emot1,
				      [quote, 'neg-emotion']
				    ],
				    
				    [ 'ty$instance?',
				      emot2,
				      [quote, 'pos-emotion']
				    ]
				  ],
				  
				  [ and,
				    
				    [ 'ty$instance?',
				      emot1,
				      [quote, 'pos-emotion']
				    ],
				    
				    [ 'ty$instance?',
				      emot2,
				      [quote, 'neg-emotion']
				    ]
				  ]
				],
				-1.0
			      ],
			      [else, 1.0]
			    ]
			  ]).

% annotating U::SIGN-CORRECTION 
wl: lambda_def(defun,
	      u_sign_correction,
	      f_u_sign_correction,
	      [u_emot1, u_emot2],
	      
	      [ 
		[ cond,
		  
		  [ 
		    [ or,
		      
		      [ and,
			[u_ty_c36_instance_c63, u_emot1, [quote, u_neg_emotion]],
			[u_ty_c36_instance_c63, u_emot2, [quote, u_pos_emotion]]
		      ],
		      
		      [ and,
			[u_ty_c36_instance_c63, u_emot1, [quote, u_pos_emotion]],
			[u_ty_c36_instance_c63, u_emot2, [quote, u_neg_emotion]]
		      ]
		    ],
		    -1.0
		  ],
		  [u_else, 1.0]
		]
	      ]).


% annotating U::SIGN-CORRECTION 
wl: arglist_info(u_sign_correction,
		[u_emot1, u_emot2],
		[Emot1_Param, Emot2_Param],
		arginfo{ all:[u_emot1, u_emot2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emot1, u_emot2],
			 opt:0,
			 req:[u_emot1, u_emot2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SIGN-CORRECTION 
wl: init_args(exact_only, u_sign_correction).


% annotating U::SIGN-CORRECTION 
f_u_sign_correction(Emot1_Param, Emot2_Param, ElseResult31) :-
	Env=[bv(u_emot1, Emot1_Param), bv(u_emot2, Emot2_Param)],
	(   f_u_ty_c36_instance_c63(Emot1_Param, u_neg_emotion, IFTEST16),
	    (   IFTEST16\==[]
	    ->  f_u_ty_c36_instance_c63(Emot2_Param, u_pos_emotion, TrueResult),
		FORM1_Res=TrueResult
	    ;   FORM1_Res=[]
	    ),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   f_u_ty_c36_instance_c63(Emot1_Param, u_pos_emotion, IFTEST21),
	    (   IFTEST21\==[]
	    ->  f_u_ty_c36_instance_c63(Emot2_Param,
					u_neg_emotion,
					TrueResult25),
		IFTEST=TrueResult25
	    ;   IFTEST=[]
	    )
	),
	(   IFTEST\==[]
	->  ElseResult31= -1.0
	;   get_var(Env, u_else, IFTEST27),
	    (   IFTEST27\==[]
	    ->  ElseResult31=1.0
	    ;   ElseResult31=[]
	    )
	).
:- set_opv(f_u_sign_correction, classof, claz_function),
   set_opv(u_sign_correction, compile_as, kw_function),
   set_opv(u_sign_correction, function, f_u_sign_correction),
   DefunResult=u_sign_correction.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21379 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'emotion-sign',
			    [emot],
			    
			    [ if,
			      ['ty$instance?', emot, [quote, 'neg-emotion']],
			      -1.0,
			      1.0
			    ]
			  ]).

% annotating U::EMOTION-SIGN 
wl: lambda_def(defun,
	      u_emotion_sign,
	      f_u_emotion_sign,
	      [u_emot],
	      
	      [ 
		[ if,
		  [u_ty_c36_instance_c63, u_emot, [quote, u_neg_emotion]],
		  -1.0,
		  1.0
		]
	      ]).


% annotating U::EMOTION-SIGN 
wl: arglist_info(u_emotion_sign,
		[u_emot],
		[Emot_Param],
		arginfo{ all:[u_emot],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emot],
			 opt:0,
			 req:[u_emot],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EMOTION-SIGN 
wl: init_args(exact_only, u_emotion_sign).


% annotating U::EMOTION-SIGN 
f_u_emotion_sign(Emot_Param, FnResult) :-
	f_u_ty_c36_instance_c63(Emot_Param, u_neg_emotion, IFTEST),
	(   IFTEST\==[]
	->  FnResult= -1.0
	;   FnResult=1.0
	).
:- set_opv(f_u_emotion_sign, classof, claz_function),
   set_opv(u_emotion_sign, compile_as, kw_function),
   set_opv(u_emotion_sign, function, f_u_emotion_sign),
   DefunResult=u_emotion_sign.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'backtrack-top-level-goal',
			    ['top-level-goal', 'next-context'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Attempting to backtrack for top-level goal ~A in ~A"),
			      'top-level-goal',
			      'next-context'
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				
				[ 'backtrack-wall',
				  ['get-backtrack-wall', 'top-level-goal']
				],
				[sprouts, []],
				['done?', []]
			      ],
			      [yuntil, 'done?'],
			      
			      [ ydo,
				
				[ if,
				  ['eq?', 'backtrack-wall', 'next-context'],
				  
				  [ progn,
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Top-level goal")
				    ],
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'(" ~A"),
				      'top-level-goal'
				    ],
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      rule,
				      '$STRING'(" fails: all possibilities exhausted")
				    ],
				    
				    [ 'all-possibilities-failed',
				      'top-level-goal',
				      'backtrack-wall'
				    ],
				    [setq, 'done?', t]
				  ],
				  
				  [ progn,
				    
				    [ setq,
				      sprouts,
				      
				      [ 'prune-possibilities',
					
					[ 'cx$children',
					  ['cx$parent', 'next-context']
					]
				      ]
				    ],
				    
				    [ if,
				      sprouts,
				      
				      [ progn,
					[setq, 'next-context', [car, sprouts]],
					
					[ 'set-next-context',
					  'top-level-goal',
					  'next-context'
					],
					
					[ 'ndbg-roman',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("Backtracking")
					],
					
					[ 'ndbg-roman',
					  '*gate-dbg*',
					  rule,
					  '$STRING'(" to next context of ~A for ~A"),
					  'next-context',
					  'top-level-goal'
					],
					['ndbg-newline', '*gate-dbg*', rule],
					[setq, 'done?', t]
				      ],
				      
				      [ setq,
					'next-context',
					['cx$parent', 'next-context']
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::BACKTRACK-TOP-LEVEL-GOAL 
wl: lambda_def(defun,
	      u_backtrack_top_level_goal,
	      f_u_backtrack_top_level_goal,
	      [u_top_level_goal, u_next_context],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(t),
			     #\(t),
			     #\(e),
			     #\(m),
			     #\(p),
			     #\(t),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(t),
			     #\(o),
			     #\(' '),
			     #\(b),
			     #\(a),
			     #\(c),
			     #\(k),
			     #\(t),
			     #\(r),
			     #\(a),
			     #\(c),
			     #\(k),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
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
		  u_top_level_goal,
		  u_next_context
		],
		
		[ u_yloop,
		  
		  [ u_initial,
		    [u_backtrack_wall, [u_get_backtrack_wall, u_top_level_goal]],
		    [u_sprouts, []],
		    [u_done_c63, []]
		  ],
		  [u_yuntil, u_done_c63],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_eq_c63, u_backtrack_wall, u_next_context],
		      
		      [ progn,
			
			[ u_ndbg_roman,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('T'),
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
				   [#\(' '), #\(~), #\('A')]),
			  u_top_level_goal
			],
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(' '),
				     #\(f),
				     #\(a),
				     #\(i),
				     #\(l),
				     #\(s),
				     #\(:),
				     #\(' '),
				     #\(a),
				     #\(l),
				     #\(l),
				     #\(' '),
				     #\(p),
				     #\(o),
				     #\(s),
				     #\(s),
				     #\(i),
				     #\(b),
				     #\(i),
				     #\(l),
				     #\(i),
				     #\(t),
				     #\(i),
				     #\(e),
				     #\(s),
				     #\(' '),
				     #\(e),
				     #\(x),
				     #\(h),
				     #\(a),
				     #\(u),
				     #\(s),
				     #\(t),
				     #\(e),
				     #\(d)
				   ])
			],
			
			[ u_all_possibilities_failed,
			  u_top_level_goal,
			  u_backtrack_wall
			],
			[setq, u_done_c63, t]
		      ],
		      
		      [ progn,
			
			[ setq,
			  u_sprouts,
			  
			  [ u_prune_possibilities,
			    
			    [ u_cx_c36_children,
			      [u_cx_c36_parent, u_next_context]
			    ]
			  ]
			],
			
			[ if,
			  u_sprouts,
			  
			  [ progn,
			    [setq, u_next_context, [car, u_sprouts]],
			    
			    [ u_set_next_context,
			      u_top_level_goal,
			      u_next_context
			    ],
			    
			    [ u_ndbg_roman,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('B'),
					 #\(a),
					 #\(c),
					 #\(k),
					 #\(t),
					 #\(r),
					 #\(a),
					 #\(c),
					 #\(k),
					 #\(i),
					 #\(n),
					 #\(g)
				       ])
			    ],
			    
			    [ u_ndbg_roman,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(' '),
					 #\(t),
					 #\(o),
					 #\(' '),
					 #\(n),
					 #\(e),
					 #\(x),
					 #\(t),
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
					 #\(f),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(f),
					 #\(o),
					 #\(r),
					 #\(' '),
					 #\(~),
					 #\('A')
				       ]),
			      u_next_context,
			      u_top_level_goal
			    ],
			    [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			    [setq, u_done_c63, t]
			  ],
			  
			  [ setq,
			    u_next_context,
			    [u_cx_c36_parent, u_next_context]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::BACKTRACK-TOP-LEVEL-GOAL 
wl: arglist_info(u_backtrack_top_level_goal,
		[u_top_level_goal, u_next_context],
		[Top_level_goal_Param, Next_context_Param],
		arginfo{ all:[u_top_level_goal, u_next_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_next_context],
			 opt:0,
			 req:[u_top_level_goal, u_next_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BACKTRACK-TOP-LEVEL-GOAL 
wl: init_args(exact_only, u_backtrack_top_level_goal).


% annotating U::BACKTRACK-TOP-LEVEL-GOAL 
f_u_backtrack_top_level_goal(Top_level_goal_Param, Next_context_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_next_context, Next_context_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(t),
				       #\(t),
				       #\(e),
				       #\(m),
				       #\(p),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(b),
				       #\(a),
				       #\(c),
				       #\(k),
				       #\(t),
				       #\(r),
				       #\(a),
				       #\(c),
				       #\(k),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
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
			    u_top_level_goal,
			    u_next_context
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      
		      [ u_backtrack_wall,
			[u_get_backtrack_wall, u_top_level_goal]
		      ],
		      [u_sprouts, []],
		      [u_done_c63, []]
		    ],
		    [u_yuntil, u_done_c63],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_backtrack_wall, u_next_context],
			
			[ progn,
			  
			  [ u_ndbg_roman,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
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
				     [#\(' '), #\(~), #\('A')]),
			    u_top_level_goal
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(f),
				       #\(a),
				       #\(i),
				       #\(l),
				       #\(s),
				       #\(:),
				       #\(' '),
				       #\(a),
				       #\(l),
				       #\(l),
				       #\(' '),
				       #\(p),
				       #\(o),
				       #\(s),
				       #\(s),
				       #\(i),
				       #\(b),
				       #\(i),
				       #\(l),
				       #\(i),
				       #\(t),
				       #\(i),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(e),
				       #\(x),
				       #\(h),
				       #\(a),
				       #\(u),
				       #\(s),
				       #\(t),
				       #\(e),
				       #\(d)
				     ])
			  ],
			  
			  [ u_all_possibilities_failed,
			    u_top_level_goal,
			    u_backtrack_wall
			  ],
			  [setq, u_done_c63, t]
			],
			
			[ progn,
			  
			  [ setq,
			    u_sprouts,
			    
			    [ u_prune_possibilities,
			      
			      [ u_cx_c36_children,
				[u_cx_c36_parent, u_next_context]
			      ]
			    ]
			  ],
			  
			  [ if,
			    u_sprouts,
			    
			    [ progn,
			      [setq, u_next_context, [car, u_sprouts]],
			      
			      [ u_set_next_context,
				u_top_level_goal,
				u_next_context
			      ],
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('B'),
					   #\(a),
					   #\(c),
					   #\(k),
					   #\(t),
					   #\(r),
					   #\(a),
					   #\(c),
					   #\(k),
					   #\(i),
					   #\(n),
					   #\(g)
					 ])
			      ],
			      
			      [ u_ndbg_roman,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(t),
					   #\(o),
					   #\(' '),
					   #\(n),
					   #\(e),
					   #\(x),
					   #\(t),
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
					   #\(f),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_next_context,
				u_top_level_goal
			      ],
			      [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			      [setq, u_done_c63, t]
			    ],
			    
			    [ setq,
			      u_next_context,
			      [u_cx_c36_parent, u_next_context]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_backtrack_top_level_goal, classof, claz_function),
   set_opv(u_backtrack_top_level_goal, compile_as, kw_function),
   set_opv(u_backtrack_top_level_goal, function, f_u_backtrack_top_level_goal),
   DefunResult=u_backtrack_top_level_goal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below will do for now.", 18, 22177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If return value is NIL, there were no successful",
				     18,
				     22297)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  mutations and so the top-level goal is terminated.",
				     18,
				     22365)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If return value is T, there was a successful mutation",
				     18,
				     22436)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  and the next-context of this top-level-goal has",
				     18,
				     22509)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  already been set by that process.",
				     18,
				     22577)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Was (setq done? (null? returned-next-context)) because this",
				     18,
				     22631)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" algorithm had to do some more backup in the old mutation alg.",
				     18,
				     22710)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" was random-element", 57, 23102)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" but we want to go by ordering, so no random...",
				     23,
				     23145)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23660)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Currently, DAYDREAMER stops upon the first success and so the below",
				     1,
				     23662)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" function is invoked if there was not a single success.",
				     1,
				     23732)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23789)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns leaf if should continue because more contexts have been",
				     1,
				     23791)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" sprouted, or NIL if we should stop.",
				     1,
				     23857)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23895)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: What about eventual mutation exhaustion?",
				     1,
				     23897)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23946)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Possibly, a top-level goal failure has been asserted in some context;",
				     1,
				     23948)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" But we have to do this in the 'resolution context', which in this",
				     1,
				     24020)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" case is backtrack-wall.", 1, 24088)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:21469 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 24114)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'all-possibilities-failed',
			    ['top-level-goal', 'backtrack-wall'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("All possibilities failed for ~A in ~A"),
			      'top-level-goal',
			      'backtrack-wall'
			    ],
			    
			    [ let,
			      [[result, []]],
			      
			      [ if,
				
				[ and,
				  ['daydreaming-mode?'],
				  
				  [ 'eq?',
				    [quote, imaginary],
				    
				    [ 'ob$get',
				      'top-level-goal',
				      [quote, 'planning-type']
				    ]
				  ],
				  
				  [ setq,
				    result,
				    
				    [ 'action-mutations',
				      'top-level-goal',
				      'backtrack-wall'
				    ]
				  ]
				],
				result,
				
				[ progn,
				  
				  [ 'terminate-top-level-goal',
				    'top-level-goal',
				    
				    [ 'make-goal-failure',
				      'top-level-goal',
				      'backtrack-wall',
				      [],
				      '*me-belief-path*',
				      'top-level-goal',
				      t
				    ],
				    'backtrack-wall'
				  ],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::ALL-POSSIBILITIES-FAILED 
wl: lambda_def(defun,
	      u_all_possibilities_failed,
	      f_u_all_possibilities_failed,
	      [u_top_level_goal, u_backtrack_wall],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(l),
			     #\(l),
			     #\(' '),
			     #\(p),
			     #\(o),
			     #\(s),
			     #\(s),
			     #\(i),
			     #\(b),
			     #\(i),
			     #\(l),
			     #\(i),
			     #\(t),
			     #\(i),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(a),
			     #\(i),
			     #\(l),
			     #\(e),
			     #\(d),
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
		  u_top_level_goal,
		  u_backtrack_wall
		],
		
		[ let,
		  [[u_result, []]],
		  
		  [ if,
		    
		    [ and,
		      [u_daydreaming_mode_c63],
		      
		      [ u_eq_c63,
			[quote, u_imaginary],
			
			[ u_ob_c36_get,
			  u_top_level_goal,
			  [quote, u_planning_type]
			]
		      ],
		      
		      [ setq,
			u_result,
			[u_action_mutations, u_top_level_goal, u_backtrack_wall]
		      ]
		    ],
		    u_result,
		    
		    [ progn,
		      
		      [ u_terminate_top_level_goal,
			u_top_level_goal,
			
			[ u_make_goal_failure,
			  u_top_level_goal,
			  u_backtrack_wall,
			  [],
			  u_xx_me_belief_path_xx,
			  u_top_level_goal,
			  t
			],
			u_backtrack_wall
		      ],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::ALL-POSSIBILITIES-FAILED 
wl: arglist_info(u_all_possibilities_failed,
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

% annotating U::ALL-POSSIBILITIES-FAILED 
wl: init_args(exact_only, u_all_possibilities_failed).


% annotating U::ALL-POSSIBILITIES-FAILED 
f_u_all_possibilities_failed(Top_level_goal_Param, Backtrack_wall_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_backtrack_wall, Backtrack_wall_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(l),
				       #\(l),
				       #\(' '),
				       #\(p),
				       #\(o),
				       #\(s),
				       #\(s),
				       #\(i),
				       #\(b),
				       #\(i),
				       #\(l),
				       #\(i),
				       #\(t),
				       #\(i),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(a),
				       #\(i),
				       #\(l),
				       #\(e),
				       #\(d),
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
			    u_top_level_goal,
			    u_backtrack_wall
			  ],
			  Roman_nl_Ret),
	LEnv=[[bv(u_result, [])]|Env],
	f_u_daydreaming_mode_c63(IFTEST18),
	(   IFTEST18\==[]
	->  f_u_eq_c63([quote, u_imaginary],
		       [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		       IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_action_mutations(Top_level_goal_Param,
				     Backtrack_wall_Param,
				     TrueResult),
		set_var(LEnv, u_result, TrueResult),
		IFTEST=TrueResult
	    ;   IFTEST=[]
	    )
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_result, Result_Get),
	    FnResult=Result_Get
	;   get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    f_u_make_goal_failure(Top_level_goal_Param,
				  Backtrack_wall_Param,
				  [],
				  Xx_me_belief_path_xx_Get,
				  Top_level_goal_Param,
				  t,
				  T),
	    f_u_terminate_top_level_goal(Top_level_goal_Param,
					 T,
					 Backtrack_wall_Param,
					 Level_goal_Ret),
	    FnResult=[]
	).
:- set_opv(f_u_all_possibilities_failed, classof, claz_function),
   set_opv(u_all_possibilities_failed, compile_as, kw_function),
   set_opv(u_all_possibilities_failed, function, f_u_all_possibilities_failed),
   DefunResult=u_all_possibilities_failed.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (let ((failed-goal (ob$fcreate `(FAILED-GOAL obj ,(ob$get",
				     1,
				     24517)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                                           top-level-goal",
				     1,
				     24582)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                                           'obj))))))",
				     1,
				     24657)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (if (interrogate \"Break? (before tlg failure) \") (breakpoint))",
				     1,
				     24741)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This is pruning possibilities after they are generated. Another way",
				     1,
				     25146)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:24115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" is to prune before they are generated.",
				     1,
				     25216)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'prune-possibilities',
			    [contexts],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Pruning possibilities from ~A"),
			      contexts
			    ],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, context, in, contexts],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    
				    [ not,
				      ['ob$get', context, [quote, 'rules-run?']]
				    ],
				    
				    [ not,
				      
				      [ 'ob$get',
					context,
					[quote, 'dd-goal-sprout?']
				      ]
				    ]
				  ],
				  [setq, result, [cons, context, result]]
				]
			      ],
			      
			      [ yresult,
				
				[ sort,
				  result,
				  
				  [ lambda,
				    [context1, context2],
				    
				    [ (>),
				      [ordering, context1],
				      [ordering, context2]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::PRUNE-POSSIBILITIES 
wl: lambda_def(defun,
	      u_prune_possibilities,
	      f_u_prune_possibilities,
	      [u_contexts],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(r),
			     #\(u),
			     #\(n),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(p),
			     #\(o),
			     #\(s),
			     #\(s),
			     #\(i),
			     #\(b),
			     #\(i),
			     #\(l),
			     #\(i),
			     #\(t),
			     #\(i),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(f),
			     #\(r),
			     #\(o),
			     #\(m),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_contexts
		],
		
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_context, u_in, u_contexts],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			[not, [u_ob_c36_get, u_context, [quote, u_rules_run_c63]]],
			
			[ not,
			  
			  [ u_ob_c36_get,
			    u_context,
			    [quote, u_dd_goal_sprout_c63]
			  ]
			]
		      ],
		      [setq, u_result, [cons, u_context, u_result]]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ sort,
		      u_result,
		      
		      [ lambda,
			[u_context1, u_context2],
			[>, [u_ordering, u_context1], [u_ordering, u_context2]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::PRUNE-POSSIBILITIES 
wl: arglist_info(u_prune_possibilities,
		[u_contexts],
		[Contexts_Param],
		arginfo{ all:[u_contexts],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_contexts],
			 opt:0,
			 req:[u_contexts],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRUNE-POSSIBILITIES 
wl: init_args(exact_only, u_prune_possibilities).


% annotating U::PRUNE-POSSIBILITIES 
f_u_prune_possibilities(Contexts_Param, FnResult) :-
	Env=[bv(u_contexts, Contexts_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(r),
				       #\(u),
				       #\(n),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(p),
				       #\(o),
				       #\(s),
				       #\(s),
				       #\(i),
				       #\(b),
				       #\(i),
				       #\(l),
				       #\(i),
				       #\(t),
				       #\(i),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(f),
				       #\(r),
				       #\(o),
				       #\(m),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_contexts
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_context, u_in, u_contexts],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ not,
			    [u_ob_c36_get, u_context, [quote, u_rules_run_c63]]
			  ],
			  
			  [ not,
			    
			    [ u_ob_c36_get,
			      u_context,
			      [quote, u_dd_goal_sprout_c63]
			    ]
			  ]
			],
			[setq, u_result, [cons, u_context, u_result]]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ sort,
			u_result,
			
			[ lambda,
			  [u_context1, u_context2],
			  [>, [u_ordering, u_context1], [u_ordering, u_context2]]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_prune_possibilities, classof, claz_function),
   set_opv(u_prune_possibilities, compile_as, kw_function),
   set_opv(u_prune_possibilities, function, f_u_prune_possibilities),
   DefunResult=u_prune_possibilities.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" will the above prevent backups to the first dd goal context?",
				     9,
				     25553)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Extra slots associated with a top-level goal:",
				     1,
				     25832)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       status: 'runable (if this goal is ready to run)",
				     1,
				     25880)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               'halted (if this goal is halted)",
				     1,
				     25936)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               'waiting (if this goal is waiting to be performed)",
				     1,
				     25985)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       active-goal: used for the top-level goal upon replacement",
				     1,
				     26052)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       planning-type: 'real or 'imaginary",
				     1,
				     26118)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       backtrack-wall: (only if planning-type = 'imaginary) backtrack",
				     1,
				     26161)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                       wall context",
				     1,
				     26232)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       next-context: (only if planning-type = 'imaginary) next context",
				     1,
				     26269)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                     to run", 1, 26341)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       mutation-plan-contexts: slot values contain ideas for new plans",
				     1,
				     26370)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       run-mutations?: t or nil",
				     1,
				     26442)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       termination-context: not set until the top-level goal is terminated",
				     1,
				     26475)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        main-motiv: main motivation emotion",
				     1,
				     26551)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 26596)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       For top-level goals AND subgoals:",
				     1,
				     26598)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       activation-context: points to the context in which the goal",
				     1,
				     26640)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              was first activated.",
				     1,
				     26708)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       top-level-goal: points to the top-level goal for all subgoals",
				     1,
				     26744)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               and, of course, the top-level goal itself",
				     1,
				     26814)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 26872)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Termination and activation contexts are more or less at this time",
				     1,
				     26874)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" for REVERSAL. (But, see also uses of 'top-context which really",
				     1,
				     26942)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" are referring to 'activation-context)",
				     1,
				     27007)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:25256 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 27047)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:27048 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*genable-emot-thresh*', 0.1]).
:- set_var(TLEnv3, setq, u_xx_genable_emot_thresh_xx, 0.1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:27082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'activate-top-level-goal',
			    [goal, context, bd, rule],
			    
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
			      '$STRING'(" ~A in ~A"),
			      goal,
			      context
			    ],
			    ['ndbg-newline', '*gate-dbg*', rule],
			    
			    [ let,
			      
			      [ [emotions, ['ob$gets', rule, [quote, emotion]]],
				['ddg?', ['dd-goal?', goal]],
				['new-context', []],
				['any-emot', []],
				['main-motiv', []]
			      ],
			      
			      [ setq,
				'*top-level-goals*',
				[cons, goal, '*top-level-goals*']
			      ],
			      
			      [ if,
				'ddg?',
				['no-gen', ['cx$assert', context, goal]],
				['cx$assert', context, goal]
			      ],
			      ['ob$add', goal, [quote, 'top-level-goal'], goal],
			      
			      [ if,
				
				[ 'null?',
				  ['ob$get', rule, [quote, 'initial-status']]
				],
				['ob$add', goal, [quote, status], [quote, runable]],
				
				[ 'ob$add',
				  goal,
				  [quote, status],
				  ['ob$get', rule, [quote, 'initial-status']]
				]
			      ],
			      
			      [ yloop,
				[yfor, emotion, in, emotions],
				
				[ ydo,
				  
				  [ if,
				    ['var?', emotion],
				    
				    [ setq,
				      'main-motiv',
				      
				      [ setq,
					emotion,
					['ob$instantiate', emotion, bd]
				      ]
				    ],
				    
				    [ setq,
				      'any-emot',
				      
				      [ setq,
					emotion,
					['ob$instantiate', emotion, bd]
				      ]
				    ]
				  ],
				  
				  [ 'add-depend',
				    context,
				    emotion,
				    goal,
				    1.0,
				    0.0,
				    0.0,
				    []
				  ],
				  ['emotion-add', emotion],
				  
				  [ if,
				    
				    [ or,
				      ['eq?', emotion, 'main-motiv'],
				      
				      [ 'fl<',
					[strength, emotion],
					'*genable-emot-thresh*'
				      ]
				    ],
				    ['no-gen', ['cx$assert', context, emotion]],
				    ['cx$assert', context, emotion]
				  ]
				]
			      ],
			      
			      [ if,
				['null?', 'main-motiv'],
				[setq, 'main-motiv', 'any-emot']
			      ],
			      
			      [ 'ob$add',
				goal,
				[quote, 'main-motiv'],
				'main-motiv'
			      ],
			      
			      [ if,
				'ddg?',
				
				[ progn,
				  
				  [ 'ob$add',
				    goal,
				    [quote, 'planning-type'],
				    [quote, imaginary]
				  ],
				  [setq, 'new-context', ['cx$sprout', context]],
				  
				  [ 'ob$add',
				    'new-context',
				    [quote, 'dd-goal-sprout?'],
				    t
				  ],
				  
				  [ 'ob$add',
				    goal,
				    [quote, 'backtrack-wall'],
				    'new-context'
				  ],
				  
				  [ 'ob$add',
				    goal,
				    [quote, 'activation-context'],
				    'new-context'
				  ],
				  
				  [ 'ob$add',
				    goal,
				    [quote, 'next-context'],
				    'new-context'
				  ]
				],
				
				[ progn,
				  
				  [ 'ob$add',
				    goal,
				    [quote, 'activation-context'],
				    context
				  ],
				  
				  [ 'ob$add',
				    goal,
				    [quote, 'planning-type'],
				    [quote, real]
				  ]
				]
			      ],
			      ['print-tasks']
			    ]
			  ]).

% annotating U::ACTIVATE-TOP-LEVEL-GOAL 
wl: lambda_def(defun,
	      u_activate_top_level_goal,
	      f_u_activate_top_level_goal,
	      [u_goal, u_context, u_bd, u_rule],
	      
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
		
		[ let,
		  
		  [ [u_emotions, [u_ob_c36_gets, u_rule, [quote, u_emotion]]],
		    [u_ddg_c63, [u_dd_goal_c63, u_goal]],
		    [u_new_context, []],
		    [u_any_emot, []],
		    [u_main_motiv, []]
		  ],
		  
		  [ setq,
		    u_xx_top_level_goals_xx,
		    [cons, u_goal, u_xx_top_level_goals_xx]
		  ],
		  
		  [ if,
		    u_ddg_c63,
		    [u_no_gen, [u_cx_c36_assert, u_context, u_goal]],
		    [u_cx_c36_assert, u_context, u_goal]
		  ],
		  [u_ob_c36_add, u_goal, [quote, u_top_level_goal], u_goal],
		  
		  [ if,
		    
		    [ u_null_c63,
		      [u_ob_c36_get, u_rule, [quote, u_initial_status]]
		    ],
		    [u_ob_c36_add, u_goal, [quote, u_status], [quote, u_runable]],
		    
		    [ u_ob_c36_add,
		      u_goal,
		      [quote, u_status],
		      [u_ob_c36_get, u_rule, [quote, u_initial_status]]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_emotion, u_in, u_emotions],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_var_c63, u_emotion],
			
			[ setq,
			  u_main_motiv,
			  
			  [ setq,
			    u_emotion,
			    [u_ob_c36_instantiate, u_emotion, u_bd]
			  ]
			],
			
			[ setq,
			  u_any_emot,
			  
			  [ setq,
			    u_emotion,
			    [u_ob_c36_instantiate, u_emotion, u_bd]
			  ]
			]
		      ],
		      [u_add_depend, u_context, u_emotion, u_goal, 1.0, 0.0, 0.0, []],
		      [u_emotion_add, u_emotion],
		      
		      [ if,
			
			[ or,
			  [u_eq_c63, u_emotion, u_main_motiv],
			  
			  [ u_fl_c60,
			    [u_strength, u_emotion],
			    u_xx_genable_emot_thresh_xx
			  ]
			],
			[u_no_gen, [u_cx_c36_assert, u_context, u_emotion]],
			[u_cx_c36_assert, u_context, u_emotion]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, u_main_motiv],
		    [setq, u_main_motiv, u_any_emot]
		  ],
		  [u_ob_c36_add, u_goal, [quote, u_main_motiv], u_main_motiv],
		  
		  [ if,
		    u_ddg_c63,
		    
		    [ progn,
		      
		      [ u_ob_c36_add,
			u_goal,
			[quote, u_planning_type],
			[quote, u_imaginary]
		      ],
		      [setq, u_new_context, [u_cx_c36_sprout, u_context]],
		      
		      [ u_ob_c36_add,
			u_new_context,
			[quote, u_dd_goal_sprout_c63],
			t
		      ],
		      
		      [ u_ob_c36_add,
			u_goal,
			[quote, u_backtrack_wall],
			u_new_context
		      ],
		      
		      [ u_ob_c36_add,
			u_goal,
			[quote, u_activation_context],
			u_new_context
		      ],
		      
		      [ u_ob_c36_add,
			u_goal,
			[quote, u_next_context],
			u_new_context
		      ]
		    ],
		    
		    [ progn,
		      
		      [ u_ob_c36_add,
			u_goal,
			[quote, u_activation_context],
			u_context
		      ],
		      
		      [ u_ob_c36_add,
			u_goal,
			[quote, u_planning_type],
			[quote, real]
		      ]
		    ]
		  ],
		  [u_print_tasks]
		]
	      ]).


% annotating U::ACTIVATE-TOP-LEVEL-GOAL 
wl: arglist_info(u_activate_top_level_goal,
		[u_goal, u_context, u_bd, u_rule],
		[Goal_Param, Context_Param, Bd_Param, Rule_Param],
		arginfo{ all:[u_goal, u_context, u_bd, u_rule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context, u_bd, u_rule],
			 opt:0,
			 req:[u_goal, u_context, u_bd, u_rule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ACTIVATE-TOP-LEVEL-GOAL 
wl: init_args(exact_only, u_activate_top_level_goal).


% annotating U::ACTIVATE-TOP-LEVEL-GOAL 
f_u_activate_top_level_goal(Goal_Param, Context_Param, Bd_Param, Rule_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_bd, Bd_Param), bv(u_rule, Rule_Param)],
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
				    #\(i),
				    #\(n),
				    #\(' '),
				    #\(~),
				    #\('A')
				  ]),
			 u_goal,
			 u_context
		       ],
		       Ndbg_roman_Ret76),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	f_u_ob_c36_gets(Rule_Param, u_emotion, Emotions_Init),
	f_u_dd_goal_c63(Goal_Param, Ddg_c63_Init),
	LEnv=[[bv(u_emotions, Emotions_Init), bv(u_ddg_c63, Ddg_c63_Init), bv(u_new_context, []), bv(u_any_emot, []), bv(u_main_motiv, [])]|Env],
	get_var(LEnv, u_xx_top_level_goals_xx, Xx_top_level_goals_xx_Get),
	Xx_top_level_goals_xx=[Goal_Param|Xx_top_level_goals_xx_Get],
	set_var(LEnv, u_xx_top_level_goals_xx, Xx_top_level_goals_xx),
	get_var(LEnv, u_ddg_c63, IFTEST),
	(   IFTEST\==[]
	->  f_u_no_gen([[u_cx_c36_assert, u_context, u_goal]], TrueResult),
	    _53614=TrueResult
	;   f_u_cx_c36_assert(Context_Param, Goal_Param, ElseResult),
	    _53614=ElseResult
	),
	f_u_ob_c36_add(Goal_Param, u_top_level_goal, Goal_Param, C36_add_Ret),
	f_u_null_c63([u_ob_c36_get, u_rule, [quote, u_initial_status]], IFTEST36),
	(   IFTEST36\==[]
	->  f_u_ob_c36_add(Goal_Param, u_status, u_runable, TrueResult41),
	    _53852=TrueResult41
	;   f_u_ob_c36_get(Rule_Param, u_initial_status, Initial_status),
	    f_u_ob_c36_add(Goal_Param, u_status, Initial_status, ElseResult42),
	    _53852=ElseResult42
	),
	f_u_yloop(
		  [ [u_yfor, u_emotion, u_in, u_emotions],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_var_c63, u_emotion],
			
			[ setq,
			  u_main_motiv,
			  
			  [ setq,
			    u_emotion,
			    [u_ob_c36_instantiate, u_emotion, u_bd]
			  ]
			],
			
			[ setq,
			  u_any_emot,
			  
			  [ setq,
			    u_emotion,
			    [u_ob_c36_instantiate, u_emotion, u_bd]
			  ]
			]
		      ],
		      [u_add_depend, u_context, u_emotion, u_goal, 1.0, 0.0, 0.0, []],
		      [u_emotion_add, u_emotion],
		      
		      [ if,
			
			[ or,
			  [u_eq_c63, u_emotion, u_main_motiv],
			  
			  [ u_fl_c60,
			    [u_strength, u_emotion],
			    u_xx_genable_emot_thresh_xx
			  ]
			],
			[u_no_gen, [u_cx_c36_assert, u_context, u_emotion]],
			[u_cx_c36_assert, u_context, u_emotion]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	f_u_null_c63(u_main_motiv, IFTEST43),
	(   IFTEST43\==[]
	->  get_var(LEnv, u_any_emot, Any_emot_Get),
	    set_var(LEnv, u_main_motiv, Any_emot_Get),
	    _54058=Any_emot_Get
	;   _54058=[]
	),
	get_var(LEnv, u_main_motiv, Main_motiv_Get),
	f_u_ob_c36_add(Goal_Param, u_main_motiv, Main_motiv_Get, C36_add_Ret79),
	get_var(LEnv, u_ddg_c63, IFTEST49),
	(   IFTEST49\==[]
	->  f_u_ob_c36_add(Goal_Param, u_planning_type, u_imaginary, Imaginary),
	    f_u_cx_c36_sprout(Context_Param, New_context),
	    set_var(LEnv, u_new_context, New_context),
	    get_var(LEnv, u_new_context, New_context_Get56),
	    f_u_ob_c36_add(New_context_Get56, u_dd_goal_sprout_c63, t, T),
	    f_u_ob_c36_add(Goal_Param,
			   u_backtrack_wall,
			   New_context_Get56,
			   C36_add_Ret80),
	    f_u_ob_c36_add(Goal_Param,
			   u_activation_context,
			   New_context_Get56,
			   C36_add_Ret81),
	    f_u_ob_c36_add(Goal_Param,
			   u_next_context,
			   New_context_Get56,
			   TrueResult64),
	    _54314=TrueResult64
	;   f_u_ob_c36_add(Goal_Param,
			   u_activation_context,
			   Context_Param,
			   C36_add_Ret82),
	    f_u_ob_c36_add(Goal_Param, u_planning_type, real, ElseResult65),
	    _54314=ElseResult65
	),
	f_u_print_tasks(Print_tasks_Ret),
	LetResult=Print_tasks_Ret,
	LetResult=FnResult.
:- set_opv(f_u_activate_top_level_goal, classof, claz_function),
   set_opv(u_activate_top_level_goal, compile_as, kw_function),
   set_opv(u_activate_top_level_goal, function, f_u_activate_top_level_goal),
   DefunResult=u_activate_top_level_goal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:27082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" should really batch strength recalculations above",
				     8,
				     28496)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_cntrl.cl:27082 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 29160)).
:- true.


% Total time: 9.733 seconds


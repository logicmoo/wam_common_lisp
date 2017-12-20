
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_epis" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:12:19 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; #- :abcl (compile-file \"dd_gen\")",
				     1,
				     563)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #+ :wamcl ", 1, 599)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  7/22/86: Added object episodic memory",
				     1,
				     208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:248 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   8/5/86: Wrote new storage/retrieval functions",
				     1,
				     249)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:298 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/25/86: Took out flavors", 1, 299)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:327 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 10/12/86: Added episode descendants",
				     1,
				     328)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:365 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 366)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:367 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     368)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:448 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 450)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:451 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Episode creation", 1, 452)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:470 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 471)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:472 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ty$create',
			    [quote, 'EPISODE'],
			    [],
			    
			    [ quote,
			      
			      [ [],
				
				[ rule,
				  goal,
				  context,
				  realism,
				  desirability,
				  ordering
				],
				[ordering]
			      ]
			    ]
			  ]).
:- f_u_ty_c36_create(u_episode,
		     [],
		     
		     [ [],
		       
		       [ u_rule,
			 u_goal,
			 u_context,
			 u_realism,
			 u_desirability,
			 u_ordering
		       ],
		       [u_ordering]
		     ],
		     _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:603 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*infinite-thresh*', 100]).
:- set_var(TLEnv3, setq, u_xx_infinite_thresh_xx, 100).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:633 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-and-store-episode',
			    
			    [ rule,
			      goal,
			      context,
			      realism,
			      desirability,
			      'hidden?',
			      children
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'ep-store',
			      '$STRING'("Make episode for goal ~A"),
			      goal
			    ],
			    
			    [ let,
			      
			      [ 
				[ ep,
				  
				  [ 'make-episode',
				    rule,
				    goal,
				    context,
				    realism,
				    desirability
				  ]
				]
			      ],
			      
			      [ if,
				children,
				
				[ progn,
				  ['ob$set', ep, [quote, children], children],
				  
				  [ 'ob$set',
				    ep,
				    [quote, descendants],
				    
				    [ yloop,
				      [initial, [result, [list, ep]]],
				      [yfor, child, in, children],
				      
				      [ ydo,
					['ob$set', child, [quote, parent], ep],
					
					[ setq,
					  result,
					  
					  [ 'append!',
					    result,
					    
					    [ 'copy-list',
					      
					      [ 'ob$get',
						child,
						[quote, descendants]
					      ]
					    ]
					  ]
					]
				      ],
				      [yresult, result]
				    ]
				  ]
				],
				['ob$set', ep, [quote, descendants], [list, ep]]
			      ],
			      
			      [ if,
				'hidden?',
				
				[ progn,
				  
				  [ 'ob$set',
				    ep,
				    [quote, 'plan-threshold'],
				    '*infinite-thresh*'
				  ],
				  
				  [ 'ob$set',
				    ep,
				    [quote, 'reminding-threshold'],
				    '*infinite-thresh*'
				  ],
				  ['epmem-store', ep, rule, [], []]
				],
				
				[ progn,
				  ['ob$set', rule, [quote, 'accessible?'], t],
				  ['epmem-store', ep, rule, t, t]
				]
			      ],
			      ep
			    ]
			  ]).

% annotating U::MAKE-AND-STORE-EPISODE 
wl: lambda_def(defun,
	      u_make_and_store_episode,
	      f_u_make_and_store_episode,
	      
	      [ u_rule,
		u_goal,
		u_context,
		u_realism,
		u_desirability,
		u_hidden_c63,
		u_children
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_ep_store,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('M'),
			     #\(a),
			     #\(k),
			     #\(e),
			     #\(' '),
			     #\(e),
			     #\(p),
			     #\(i),
			     #\(s),
			     #\(o),
			     #\(d),
			     #\(e),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_goal
		],
		
		[ let,
		  
		  [ 
		    [ u_ep,
		      
		      [ u_make_episode,
			u_rule,
			u_goal,
			u_context,
			u_realism,
			u_desirability
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_children,
		    
		    [ progn,
		      [u_ob_c36_set, u_ep, [quote, u_children], u_children],
		      
		      [ u_ob_c36_set,
			u_ep,
			[quote, u_descendants],
			
			[ u_yloop,
			  [u_initial, [u_result, [list, u_ep]]],
			  [u_yfor, u_child, u_in, u_children],
			  
			  [ u_ydo,
			    [u_ob_c36_set, u_child, [quote, u_parent], u_ep],
			    
			    [ setq,
			      u_result,
			      
			      [ u_append_c33,
				u_result,
				
				[ copy_list,
				  [u_ob_c36_get, u_child, [quote, u_descendants]]
				]
			      ]
			    ]
			  ],
			  [u_yresult, u_result]
			]
		      ]
		    ],
		    [u_ob_c36_set, u_ep, [quote, u_descendants], [list, u_ep]]
		  ],
		  
		  [ if,
		    u_hidden_c63,
		    
		    [ progn,
		      
		      [ u_ob_c36_set,
			u_ep,
			[quote, u_plan_threshold],
			u_xx_infinite_thresh_xx
		      ],
		      
		      [ u_ob_c36_set,
			u_ep,
			[quote, u_reminding_threshold],
			u_xx_infinite_thresh_xx
		      ],
		      [u_epmem_store, u_ep, u_rule, [], []]
		    ],
		    
		    [ progn,
		      [u_ob_c36_set, u_rule, [quote, u_accessible_c63], t],
		      [u_epmem_store, u_ep, u_rule, t, t]
		    ]
		  ],
		  u_ep
		]
	      ]).


% annotating U::MAKE-AND-STORE-EPISODE 
wl: arglist_info(u_make_and_store_episode,
		
		[ u_rule,
		  u_goal,
		  u_context,
		  u_realism,
		  u_desirability,
		  u_hidden_c63,
		  u_children
		],
		
		[ Rule_Param,
		  Goal_Param,
		  Context_Param,
		  Realism_Param,
		  Desirability_Param,
		  Hidden_c63_Param,
		  Children_Param
		],
		arginfo{ all:
			     [ u_rule,
			       u_goal,
			       u_context,
			       u_realism,
			       u_desirability,
			       u_hidden_c63,
			       u_children
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_rule,
				 u_goal,
				 u_context,
				 u_realism,
				 u_desirability,
				 u_hidden_c63,
				 u_children
			       ],
			 opt:0,
			 req:
			     [ u_rule,
			       u_goal,
			       u_context,
			       u_realism,
			       u_desirability,
			       u_hidden_c63,
			       u_children
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-AND-STORE-EPISODE 
wl: init_args(exact_only, u_make_and_store_episode).


% annotating U::MAKE-AND-STORE-EPISODE 
f_u_make_and_store_episode(Rule_Param, Goal_Param, Context_Param, Realism_Param, Desirability_Param, Hidden_c63_Param, Children_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_realism, Realism_Param), bv(u_desirability, Desirability_Param), bv(u_hidden_c63, Hidden_c63_Param), bv(u_children, Children_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_ep_store,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('M'),
				       #\(a),
				       #\(k),
				       #\(e),
				       #\(' '),
				       #\(e),
				       #\(p),
				       #\(i),
				       #\(s),
				       #\(o),
				       #\(d),
				       #\(e),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_goal
			  ],
			  Roman_nl_Ret),
	f_u_make_episode(Rule_Param,
			 Goal_Param,
			 Context_Param,
			 Realism_Param,
			 Desirability_Param,
			 Ep_Init),
	LEnv=[[bv(u_ep, Ep_Init)]|Env],
	(   Children_Param\==[]
	->  get_var(LEnv, u_ep, Ep_Get),
	    f_u_ob_c36_set(Ep_Get, u_children, Children_Param, C36_set_Ret),
	    get_var(LEnv, u_ep, Ep_Get38),
	    f_u_yloop(
		      [ [u_initial, [u_result, [list, u_ep]]],
			[u_yfor, u_child, u_in, u_children],
			
			[ u_ydo,
			  [u_ob_c36_set, u_child, [quote, u_parent], u_ep],
			  
			  [ setq,
			    u_result,
			    
			    [ u_append_c33,
			      u_result,
			      
			      [ copy_list,
				[u_ob_c36_get, u_child, [quote, u_descendants]]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      Descendants),
	    f_u_ob_c36_set(Ep_Get38, u_descendants, Descendants, TrueResult),
	    _50682=TrueResult
	;   get_var(LEnv, u_ep, Ep_Get39),
	    Descendants61=[Ep_Get39],
	    f_u_ob_c36_set(Ep_Get39, u_descendants, Descendants61, ElseResult),
	    _50682=ElseResult
	),
	(   Hidden_c63_Param\==[]
	->  get_var(LEnv, u_ep, Ep_Get46),
	    get_var(LEnv, u_xx_infinite_thresh_xx, Xx_infinite_thresh_xx_Get),
	    f_u_ob_c36_set(Ep_Get46,
			   u_plan_threshold,
			   Xx_infinite_thresh_xx_Get,
			   C36_set_Ret65),
	    get_var(LEnv, u_ep, Ep_Get48),
	    get_var(LEnv, u_xx_infinite_thresh_xx, Xx_infinite_thresh_xx_Get49),
	    f_u_ob_c36_set(Ep_Get48,
			   u_reminding_threshold,
			   Xx_infinite_thresh_xx_Get49,
			   C36_set_Ret66),
	    get_var(LEnv, u_ep, Ep_Get50),
	    f_u_epmem_store(Ep_Get50, Rule_Param, [], [], TrueResult55),
	    _51008=TrueResult55
	;   f_u_ob_c36_set(Rule_Param, u_accessible_c63, t, T),
	    get_var(LEnv, u_ep, Ep_Get53),
	    f_u_epmem_store(Ep_Get53, Rule_Param, t, t, ElseResult56),
	    _51008=ElseResult56
	),
	get_var(LEnv, u_ep, Ep_Get57),
	LetResult=Ep_Get57,
	LetResult=FnResult.
:- set_opv(f_u_make_and_store_episode, classof, claz_function),
   set_opv(u_make_and_store_episode, compile_as, kw_function),
   set_opv(u_make_and_store_episode, function, f_u_make_and_store_episode),
   DefunResult=u_make_and_store_episode.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:633 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" includes self", 38, 1010)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:1806 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'accessible?',
			    [rule],
			    ['ob$get', rule, [quote, 'accessible?']]
			  ]).

% annotating U::ACCESSIBLE? 
wl: lambda_def(defun,
	      u_accessible_c63,
	      f_u_accessible_c63,
	      [u_rule],
	      [[u_ob_c36_get, u_rule, [quote, u_accessible_c63]]]).


% annotating U::ACCESSIBLE? 
wl: arglist_info(u_accessible_c63,
		[u_rule],
		[Rule_Param],
		arginfo{ all:[u_rule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule],
			 opt:0,
			 req:[u_rule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ACCESSIBLE? 
wl: init_args(exact_only, u_accessible_c63).


% annotating U::ACCESSIBLE? 
f_u_accessible_c63(Rule_Param, FnResult) :-
	f_u_ob_c36_get(Rule_Param, u_accessible_c63, Accessible_c63),
	Accessible_c63=FnResult.
:- set_opv(f_u_accessible_c63, classof, claz_function),
   set_opv(u_accessible_c63, compile_as, kw_function),
   set_opv(u_accessible_c63, function, f_u_accessible_c63),
   DefunResult=u_accessible_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:1863 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inaccessible?',
			    [rule],
			    ['null?', ['ob$get', rule, [quote, 'accessible?']]]
			  ]).

% annotating U::INACCESSIBLE? 
wl: lambda_def(defun,
	      u_inaccessible_c63,
	      f_u_inaccessible_c63,
	      [u_rule],
	      [[u_null_c63, [u_ob_c36_get, u_rule, [quote, u_accessible_c63]]]]).


% annotating U::INACCESSIBLE? 
wl: arglist_info(u_inaccessible_c63,
		[u_rule],
		[Rule_Param],
		arginfo{ all:[u_rule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule],
			 opt:0,
			 req:[u_rule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INACCESSIBLE? 
wl: init_args(exact_only, u_inaccessible_c63).


% annotating U::INACCESSIBLE? 
f_u_inaccessible_c63(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_null_c63([u_ob_c36_get, u_rule, [quote, u_accessible_c63]],
		     Null_c63_Ret),
	Null_c63_Ret=FnResult.
:- set_opv(f_u_inaccessible_c63, classof, claz_function),
   set_opv(u_inaccessible_c63, compile_as, kw_function),
   set_opv(u_inaccessible_c63, function, f_u_inaccessible_c63),
   DefunResult=u_inaccessible_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:1930 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*next-ep-number*', 1]).
:- set_var(TLEnv3, setq, u_xx_next_ep_number_xx, 1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:1930 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below is for debugging purposes only.",
				     1,
				     1958)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2001 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*episodes*', []]).
:- set_var(TLEnv3, setq, u_xx_episodes_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2001 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" We want two level eps to be stored. E.g., Harrison goes to Cairo.",
				     1,
				     2025)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2092 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-episode',
			    [rule, goal, context, realism, desirability],
			    
			    [ let,
			      
			      [ 
				[ ep,
				  
				  [ 'ob$fcreate',
				    
				    [ '#BQ',
				      
				      [ 'EPISODE',
					rule,
					['#COMMA', rule],
					goal,
					['#COMMA', goal],
					context,
					['#COMMA', context],
					realism,
					
					[ '#COMMA',
					  
					  [ or,
					    realism,
					    
					    [ strength,
					      ['ob$get', goal, [quote, obj]]
					    ]
					  ]
					],
					desirability,
					['#COMMA', [or, desirability, 1.0]]
				      ]
				    ]
				  ]
				]
			      ],
			      ['ob$set', goal, [quote, episode], ep],
			      
			      [ 'ob$add-unique-name',
				ep,
				
				[ 'string->symbol',
				  
				  [ 'string-append',
				    '$STRING'("EPISODE."),
				    ['fixnum->string', '*next-ep-number*']
				  ]
				]
			      ],
			      ['increment-me', '*next-ep-number*'],
			      [setq, '*episodes*', [cons, ep, '*episodes*']],
			      ep
			    ]
			  ]).

% annotating U::MAKE-EPISODE 
wl: lambda_def(defun,
	      u_make_episode,
	      f_u_make_episode,
	      [u_rule, u_goal, u_context, u_realism, u_desirability],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_ep,
		      
		      [ u_ob_c36_fcreate,
			
			[ '#BQ',
			  
			  [ u_episode,
			    u_rule,
			    ['#COMMA', u_rule],
			    u_goal,
			    ['#COMMA', u_goal],
			    u_context,
			    ['#COMMA', u_context],
			    u_realism,
			    
			    [ '#COMMA',
			      
			      [ or,
				u_realism,
				
				[ u_strength,
				  [u_ob_c36_get, u_goal, [quote, u_obj]]
				]
			      ]
			    ],
			    u_desirability,
			    ['#COMMA', [or, u_desirability, 1.0]]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_ob_c36_set, u_goal, [quote, u_episode], u_ep],
		  
		  [ u_ob_c36_add_unique_name,
		    u_ep,
		    
		    [ u_string_c62_symbol,
		      
		      [ u_string_append,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('E'),
				   #\('P'),
				   #\('I'),
				   #\('S'),
				   #\('O'),
				   #\('D'),
				   #\('E'),
				   #\('.')
				 ]),
			[u_fixnum_c62_string, u_xx_next_ep_number_xx]
		      ]
		    ]
		  ],
		  [u_increment_me, u_xx_next_ep_number_xx],
		  [setq, u_xx_episodes_xx, [cons, u_ep, u_xx_episodes_xx]],
		  u_ep
		]
	      ]).


% annotating U::MAKE-EPISODE 
wl: arglist_info(u_make_episode,
		[u_rule, u_goal, u_context, u_realism, u_desirability],
		
		[ Rule_Param,
		  Goal_Param,
		  Context_Param,
		  Realism_Param,
		  Desirability_Param
		],
		arginfo{ all:
			     [ u_rule,
			       u_goal,
			       u_context,
			       u_realism,
			       u_desirability
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_rule,
				 u_goal,
				 u_context,
				 u_realism,
				 u_desirability
			       ],
			 opt:0,
			 req:
			     [ u_rule,
			       u_goal,
			       u_context,
			       u_realism,
			       u_desirability
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-EPISODE 
wl: init_args(exact_only, u_make_episode).


% annotating U::MAKE-EPISODE 
f_u_make_episode(Rule_Param, Goal_Param, Context_Param, Realism_Param, Desirability_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_realism, Realism_Param), bv(u_desirability, Desirability_Param)],
	f_u_ob_c36_fcreate(
			   [ '#BQ',
			     
			     [ u_episode,
			       u_rule,
			       ['#COMMA', u_rule],
			       u_goal,
			       ['#COMMA', u_goal],
			       u_context,
			       ['#COMMA', u_context],
			       u_realism,
			       
			       [ '#COMMA',
				 
				 [ or,
				   u_realism,
				   
				   [ u_strength,
				     [u_ob_c36_get, u_goal, [quote, u_obj]]
				   ]
				 ]
			       ],
			       u_desirability,
			       ['#COMMA', [or, u_desirability, 1.0]]
			     ]
			   ],
			   Ep_Init),
	LEnv=[[bv(u_ep, Ep_Init)]|Env],
	get_var(LEnv, u_ep, Ep_Get),
	f_u_ob_c36_set(Goal_Param, u_episode, Ep_Get, C36_set_Ret),
	get_var(LEnv, u_ep, Ep_Get26),
	f_u_string_c62_symbol(
			      [ u_string_append,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('E'),
					   #\('P'),
					   #\('I'),
					   #\('S'),
					   #\('O'),
					   #\('D'),
					   #\('E'),
					   #\('.')
					 ]),
				[u_fixnum_c62_string, u_xx_next_ep_number_xx]
			      ],
			      C62_symbol_Ret),
	f_u_ob_c36_add_unique_name(Ep_Get26, C62_symbol_Ret, Unique_name_Ret),
	f_u_increment_me(u_xx_next_ep_number_xx, Increment_me_Ret),
	get_var(LEnv, u_ep, Ep_Get27),
	get_var(LEnv, u_xx_episodes_xx, Xx_episodes_xx_Get),
	Xx_episodes_xx=[Ep_Get27|Xx_episodes_xx_Get],
	set_var(LEnv, u_xx_episodes_xx, Xx_episodes_xx),
	get_var(LEnv, u_ep, Ep_Get29),
	LetResult=Ep_Get29,
	LetResult=FnResult.
:- set_opv(f_u_make_episode, classof, claz_function),
   set_opv(u_make_episode, compile_as, kw_function),
   set_opv(u_make_episode, function, f_u_make_episode),
   DefunResult=u_make_episode.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2092 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: I guess 1.0 is default?", 35, 2502)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2092 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2764)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2092 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Episode storage and retrieval", 1, 2766)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2092 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2798)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2800 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*episodic-memory*', []]).
:- set_var(TLEnv3, setq, u_xx_episodic_memory_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2830 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'epmem-init',
			    [],
			    [setq, '*episodic-memory*', ['cx$create']],
			    
			    [ 'ob$add-name',
			      '*episodic-memory*',
			      [quote, 'episodic-memory']
			    ],
			    '*episodic-memory*'
			  ]).

% annotating U::EPMEM-INIT 
wl: lambda_def(defun,
	      u_epmem_init,
	      f_u_epmem_init,
	      [],
	      
	      [ [setq, u_xx_episodic_memory_xx, [u_cx_c36_create]],
		
		[ u_ob_c36_add_name,
		  u_xx_episodic_memory_xx,
		  [quote, u_episodic_memory]
		],
		u_xx_episodic_memory_xx
	      ]).


% annotating U::EPMEM-INIT 
wl: arglist_info(u_epmem_init,
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

% annotating U::EPMEM-INIT 
wl: init_args(exact_only, u_epmem_init).


% annotating U::EPMEM-INIT 
f_u_epmem_init(FnResult) :-
	Env=[],
	f_u_cx_c36_create(Xx_episodic_memory_xx),
	set_var(Env, u_xx_episodic_memory_xx, Xx_episodic_memory_xx),
	get_var(Env, u_xx_episodic_memory_xx, Xx_episodic_memory_xx_Get11),
	f_u_ob_c36_add_name(Xx_episodic_memory_xx_Get11,
			    u_episodic_memory,
			    Episodic_memory),
	Xx_episodic_memory_xx_Get11=FnResult.
:- set_opv(f_u_epmem_init, classof, claz_function),
   set_opv(u_epmem_init, compile_as, kw_function),
   set_opv(u_epmem_init, function, f_u_epmem_init),
   DefunResult=u_epmem_init.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:2963 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'epmem-initialize',
			    [],
			    [setq, '*recent-episodes*', []],
			    [setq, '*recent-indices*', []]
			  ]).

% annotating U::EPMEM-INITIALIZE 
wl: lambda_def(defun,
	      u_epmem_initialize,
	      f_u_epmem_initialize,
	      [],
	      
	      [ [setq, u_xx_recent_episodes_xx, []],
		[setq, u_xx_recent_indices_xx, []]
	      ]).


% annotating U::EPMEM-INITIALIZE 
wl: arglist_info(u_epmem_initialize,
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

% annotating U::EPMEM-INITIALIZE 
wl: init_args(exact_only, u_epmem_initialize).


% annotating U::EPMEM-INITIALIZE 
f_u_epmem_initialize(FnResult) :-
	Env=[],
	set_var(Env, setq, u_xx_recent_episodes_xx, []),
	set_var(Env, setq, u_xx_recent_indices_xx, []),
	[]=FnResult.
:- set_opv(f_u_epmem_initialize, classof, claz_function),
   set_opv(u_epmem_initialize, compile_as, kw_function),
   set_opv(u_epmem_initialize, function, f_u_epmem_initialize),
   DefunResult=u_epmem_initialize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3053 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'epmem-store',
			    
			    [ episode,
			      index,
			      'needed-for-plan?',
			      'needed-for-reminding?'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'ep-store',
			      '$STRING'("Storing ~A under ~A"),
			      episode,
			      index
			    ],
			    
			    [ setq,
			      index,
			      ['index-intern', index, [quote, 'new-ok']]
			    ],
			    ['ob$add', index, [quote, indexes], episode],
			    ['ob$add', episode, [quote, 'indexed-under'], index],
			    
			    [ if,
			      'needed-for-plan?',
			      
			      [ 'ob$set',
				episode,
				[quote, 'plan-threshold'],
				
				[ (+),
				  1,
				  
				  [ or,
				    
				    [ 'ob$get',
				      episode,
				      [quote, 'plan-threshold']
				    ],
				    0
				  ]
				]
			      ]
			    ],
			    
			    [ if,
			      'needed-for-reminding?',
			      
			      [ 'ob$set',
				episode,
				[quote, 'reminding-threshold'],
				
				[ (+),
				  1,
				  
				  [ or,
				    
				    [ 'ob$get',
				      episode,
				      [quote, 'reminding-threshold']
				    ],
				    0
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::EPMEM-STORE 
wl: lambda_def(defun,
	      u_epmem_store,
	      f_u_epmem_store,
	      
	      [ u_episode,
		index,
		u_needed_for_plan_c63,
		u_needed_for_reminding_c63
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_ep_store,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('S'),
			     #\(t),
			     #\(o),
			     #\(r),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(u),
			     #\(n),
			     #\(d),
			     #\(e),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_episode,
		  index
		],
		[setq, index, [u_index_intern, index, [quote, u_new_ok]]],
		[u_ob_c36_add, index, [quote, u_indexes], u_episode],
		[u_ob_c36_add, u_episode, [quote, u_indexed_under], index],
		
		[ if,
		  u_needed_for_plan_c63,
		  
		  [ u_ob_c36_set,
		    u_episode,
		    [quote, u_plan_threshold],
		    
		    [ (+),
		      1,
		      [or, [u_ob_c36_get, u_episode, [quote, u_plan_threshold]], 0]
		    ]
		  ]
		],
		
		[ if,
		  u_needed_for_reminding_c63,
		  
		  [ u_ob_c36_set,
		    u_episode,
		    [quote, u_reminding_threshold],
		    
		    [ (+),
		      1,
		      
		      [ or,
			[u_ob_c36_get, u_episode, [quote, u_reminding_threshold]],
			0
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::EPMEM-STORE 
wl: arglist_info(u_epmem_store,
		
		[ u_episode,
		  index,
		  u_needed_for_plan_c63,
		  u_needed_for_reminding_c63
		],
		
		[ Episode_Param,
		  Index_Param,
		  Needed_for_plan_c63_Param,
		  Needed_for_reminding_c63_Param
		],
		arginfo{ all:
			     [ u_episode,
			       index,
			       u_needed_for_plan_c63,
			       u_needed_for_reminding_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_episode,
				 index,
				 u_needed_for_plan_c63,
				 u_needed_for_reminding_c63
			       ],
			 opt:0,
			 req:
			     [ u_episode,
			       index,
			       u_needed_for_plan_c63,
			       u_needed_for_reminding_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPMEM-STORE 
wl: init_args(exact_only, u_epmem_store).


% annotating U::EPMEM-STORE 
f_u_epmem_store(Episode_Param, Index_Param, Needed_for_plan_c63_Param, Needed_for_reminding_c63_Param, FnResult) :-
	Env=[bv(u_episode, Episode_Param), bv(index, Index_Param), bv(u_needed_for_plan_c63, Needed_for_plan_c63_Param), bv(u_needed_for_reminding_c63, Needed_for_reminding_c63_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_ep_store,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(t),
				       #\(o),
				       #\(r),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(u),
				       #\(n),
				       #\(d),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_episode,
			    index
			  ],
			  Roman_nl_Ret),
	get_var(Env, index, Index_Get),
	f_u_index_intern(Index_Get, u_new_ok, New_ok),
	set_var(Env, index, New_ok),
	get_var(Env, index, Index_Get19),
	f_u_ob_c36_add(Index_Get19, u_indexes, Episode_Param, C36_add_Ret),
	get_var(Env, index, Index_Get22),
	f_u_ob_c36_add(Episode_Param,
		       u_indexed_under,
		       Index_Get22,
		       C36_add_Ret44),
	(   Needed_for_plan_c63_Param\==[]
	->  (   f_u_ob_c36_get(Episode_Param, u_plan_threshold, FORM1_Res),
		FORM1_Res\==[],
		_50044=FORM1_Res
	    ->  true
	    ;   _50044=0
	    ),
	    +(1, _50044, Plan_threshold),
	    f_u_ob_c36_set(Episode_Param,
			   u_plan_threshold,
			   Plan_threshold,
			   TrueResult),
	    _49942=TrueResult
	;   _49942=[]
	),
	(   Needed_for_reminding_c63_Param\==[]
	->  (   f_u_ob_c36_get(Episode_Param,
			       u_reminding_threshold,
			       FORM1_Res35),
		FORM1_Res35\==[],
		_50202=FORM1_Res35
	    ->  true
	    ;   _50202=0
	    ),
	    +(1, _50202, Reminding_threshold),
	    f_u_ob_c36_set(Episode_Param,
			   u_reminding_threshold,
			   Reminding_threshold,
			   TrueResult36),
	    FnResult=TrueResult36
	;   FnResult=[]
	).
:- set_opv(f_u_epmem_store, classof, claz_function),
   set_opv(u_epmem_store, compile_as, kw_function),
   set_opv(u_epmem_store, function, f_u_epmem_store),
   DefunResult=u_epmem_store.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3582 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*epmem-marks*', []]).
:- set_var(TLEnv3, setq, u_xx_epmem_marks_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3608 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, 'mark-init', [], ['mark-unmark-all']]).

% annotating U::MARK-INIT 
wl: lambda_def(defun, u_mark_init, f_u_mark_init, [], [[u_mark_unmark_all]]).


% annotating U::MARK-INIT 
wl: arglist_info(u_mark_init,
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

% annotating U::MARK-INIT 
wl: init_args(exact_only, u_mark_init).


% annotating U::MARK-INIT 
f_u_mark_init(FnResult) :-
	Env=[],
	f_u_mark_unmark_all(Unmark_all_Ret),
	Unmark_all_Ret=FnResult.
:- set_opv(f_u_mark_init, classof, claz_function),
   set_opv(u_mark_init, compile_as, kw_function),
   set_opv(u_mark_init, function, f_u_mark_init),
   DefunResult=u_mark_init.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3608 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" currently as a safety precaution",
				     1,
				     3649)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3608 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(setq *epmem-marks* nil)", 1, 3684)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3710 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'mark-unmark-all',
			    [],
			    
			    [ yloop,
			      [yfor, mark, in, '*epmem-marks*'],
			      [ydo, ['ob$removes', mark, [quote, marks]]]
			    ],
			    [setq, '*epmem-marks*', []]
			  ]).

% annotating U::MARK-UNMARK-ALL 
wl: lambda_def(defun,
	      u_mark_unmark_all,
	      f_u_mark_unmark_all,
	      [],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_mark, u_in, u_xx_epmem_marks_xx],
		  [u_ydo, [u_ob_c36_removes, u_mark, [quote, u_marks]]]
		],
		[setq, u_xx_epmem_marks_xx, []]
	      ]).


% annotating U::MARK-UNMARK-ALL 
wl: arglist_info(u_mark_unmark_all,
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

% annotating U::MARK-UNMARK-ALL 
wl: init_args(exact_only, u_mark_unmark_all).


% annotating U::MARK-UNMARK-ALL 
f_u_mark_unmark_all(FnResult) :-
	Env=[],
	f_u_yloop(
		  [ [u_yfor, u_mark, u_in, u_xx_epmem_marks_xx],
		    [u_ydo, [u_ob_c36_removes, u_mark, [quote, u_marks]]]
		  ],
		  Yloop_Ret),
	set_var(Env, setq, u_xx_epmem_marks_xx, []),
	[]=FnResult.
:- set_opv(f_u_mark_unmark_all, classof, claz_function),
   set_opv(u_mark_unmark_all, compile_as, kw_function),
   set_opv(u_mark_unmark_all, function, f_u_mark_unmark_all),
   DefunResult=u_mark_unmark_all.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3710 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" later change to replace slot value to 0 for",
				     1,
				     3816)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3710 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" less garbage.", 1, 3862)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3906 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'mark-mark',
			    [ob],
			    
			    [ let,
			      
			      [ 
				[ marks,
				  [+, 1, [or, ['ob$get', ob, [quote, marks]], 0]]
				]
			      ],
			      ['ob$set', ob, [quote, marks], marks],
			      
			      [ if,
				['eq?', marks, 1],
				
				[ setq,
				  '*epmem-marks*',
				  [cons, ob, '*epmem-marks*']
				]
			      ],
			      marks
			    ]
			  ]).

% annotating U::MARK-MARK 
wl: lambda_def(defun,
	      u_mark_mark,
	      f_u_mark_mark,
	      [u_ob],
	      
	      [ 
		[ let,
		  [[u_marks, [+, 1, [or, [u_ob_c36_get, u_ob, [quote, u_marks]], 0]]]],
		  [u_ob_c36_set, u_ob, [quote, u_marks], u_marks],
		  
		  [ if,
		    [u_eq_c63, u_marks, 1],
		    [setq, u_xx_epmem_marks_xx, [cons, u_ob, u_xx_epmem_marks_xx]]
		  ],
		  u_marks
		]
	      ]).


% annotating U::MARK-MARK 
wl: arglist_info(u_mark_mark,
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

% annotating U::MARK-MARK 
wl: init_args(exact_only, u_mark_mark).


% annotating U::MARK-MARK 
f_u_mark_mark(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	(   f_u_ob_c36_get(Ob_Param, u_marks, FORM1_Res),
	    FORM1_Res\==[],
	    _48878=FORM1_Res
	->  true
	;   _48878=0
	),
	+(1, _48878, Marks_Init),
	LEnv=[[bv(u_marks, Marks_Init)]|Env],
	get_var(LEnv, u_marks, Marks_Get),
	f_u_ob_c36_set(Ob_Param, u_marks, Marks_Get, C36_set_Ret),
	f_u_eq_c63(u_marks, 1, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_xx_epmem_marks_xx, Xx_epmem_marks_xx_Get),
	    TrueResult=[Ob_Param|Xx_epmem_marks_xx_Get],
	    set_var(LEnv, u_xx_epmem_marks_xx, TrueResult),
	    _49142=TrueResult
	;   _49142=[]
	),
	get_var(LEnv, u_marks, Marks_Get25),
	LetResult=Marks_Get25,
	LetResult=FnResult.
:- set_opv(f_u_mark_mark, classof, claz_function),
   set_opv(u_mark_mark, compile_as, kw_function),
   set_opv(u_mark_mark, function, f_u_mark_mark),
   DefunResult=u_mark_mark.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3906 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This function is used to retrieve episodes in planning.",
				     1,
				     4108)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3906 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" epmem-reminding is later called on an episode returned by this",
				     1,
				     4166)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3906 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" procedure if in fact other heuristics decide to use that episode.",
				     1,
				     4231)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:3906 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (Thus only \"appropriate\" episodes are actually recalled in this case.)",
				     1,
				     4299)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'episode-retrieve',
			    [rule],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-xtra',
			      '$STRING'("Find potential episodes for ~A"),
			      rule
			    ],
			    
			    [ let,
			      
			      [ 
				[ new,
				  
				  [ 'epmem-retrieve1',
				    [list, rule],
				    [],
				    [quote, 'plan-threshold']
				  ]
				],
				[result, []]
			      ],
			      
			      [ yloop,
				[yfor, ep, in, ['ob$gets', rule, [quote, indexes]]],
				
				[ ydo,
				  
				  [ if,
				    
				    [ or,
				      ['memq?', ep, new],
				      ['recent-episode?', ep]
				    ],
				    [setq, result, [cons, ep, result]]
				  ]
				]
			      ],
			      
			      [ if,
				result,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'rule-xtra',
				  '$STRING'("Potential episodes = ~A"),
				  result
				]
			      ],
			      result
			    ]
			  ]).

% annotating U::EPISODE-RETRIEVE 
wl: lambda_def(defun,
	      u_episode_retrieve,
	      f_u_episode_retrieve,
	      [u_rule],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_xtra,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('F'),
			     #\(i),
			     #\(n),
			     #\(d),
			     #\(' '),
			     #\(p),
			     #\(o),
			     #\(t),
			     #\(e),
			     #\(n),
			     #\(t),
			     #\(i),
			     #\(a),
			     #\(l),
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
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_rule
		],
		
		[ let,
		  
		  [ 
		    [ u_new,
		      
		      [ u_epmem_retrieve1,
			[list, u_rule],
			[],
			[quote, u_plan_threshold]
		      ]
		    ],
		    [u_result, []]
		  ],
		  
		  [ u_yloop,
		    
		    [ u_yfor,
		      u_ep,
		      u_in,
		      [u_ob_c36_gets, u_rule, [quote, u_indexes]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ or,
			  [u_memq_c63, u_ep, u_new],
			  [u_recent_episode_c63, u_ep]
			],
			[setq, u_result, [cons, u_ep, u_result]]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_result,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule_xtra,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('P'),
				 #\(o),
				 #\(t),
				 #\(e),
				 #\(n),
				 #\(t),
				 #\(i),
				 #\(a),
				 #\(l),
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
	      ]).


% annotating U::EPISODE-RETRIEVE 
wl: arglist_info(u_episode_retrieve,
		[u_rule],
		[Rule_Param],
		arginfo{ all:[u_rule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule],
			 opt:0,
			 req:[u_rule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-RETRIEVE 
wl: init_args(exact_only, u_episode_retrieve).


% annotating U::EPISODE-RETRIEVE 
f_u_episode_retrieve(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('F'),
				       #\(i),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(p),
				       #\(o),
				       #\(t),
				       #\(e),
				       #\(n),
				       #\(t),
				       #\(i),
				       #\(a),
				       #\(l),
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
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_rule
			  ],
			  Roman_nl_Ret),
	Epmem_retrieve1_Param=[Rule_Param],
	f_u_epmem_retrieve1(Epmem_retrieve1_Param,
			    [],
			    u_plan_threshold,
			    New_Init),
	LEnv=[[bv(u_new, New_Init), bv(u_result, [])]|Env],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ep,
		      u_in,
		      [u_ob_c36_gets, u_rule, [quote, u_indexes]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ or,
			  [u_memq_c63, u_ep, u_new],
			  [u_recent_episode_c63, u_ep]
			],
			[setq, u_result, [cons, u_ep, u_result]]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_result, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_xtra,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('P'),
					   #\(o),
					   #\(t),
					   #\(e),
					   #\(n),
					   #\(t),
					   #\(i),
					   #\(a),
					   #\(l),
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
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_result
			      ],
			      TrueResult),
	    _50354=TrueResult
	;   _50354=[]
	),
	get_var(LEnv, u_result, Result_Get21),
	LetResult=Result_Get21,
	LetResult=FnResult.
:- set_opv(f_u_episode_retrieve, classof, claz_function),
   set_opv(u_episode_retrieve, compile_as, kw_function),
   set_opv(u_episode_retrieve, function, f_u_episode_retrieve),
   DefunResult=u_episode_retrieve.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'epmem-retrieve',
			    [indices, 'serendipity?', 'threshold-type'],
			    
			    [ let,
			      
			      [ 
				[ eps,
				  
				  [ 'epmem-retrieve1',
				    indices,
				    'serendipity?',
				    'threshold-type'
				  ]
				]
			      ],
			      
			      [ yloop,
				[yfor, ep, in, eps],
				[ydo, ['epmem-reminding', ep, [], []]]
			      ],
			      eps
			    ]
			  ]).

% annotating U::EPMEM-RETRIEVE 
wl: lambda_def(defun,
	      u_epmem_retrieve,
	      f_u_epmem_retrieve,
	      [u_indices, u_serendipity_c63, u_threshold_type],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_eps,
		      
		      [ u_epmem_retrieve1,
			u_indices,
			u_serendipity_c63,
			u_threshold_type
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_ep, u_in, u_eps],
		    [u_ydo, [u_epmem_reminding, u_ep, [], []]]
		  ],
		  u_eps
		]
	      ]).


% annotating U::EPMEM-RETRIEVE 
wl: arglist_info(u_epmem_retrieve,
		[u_indices, u_serendipity_c63, u_threshold_type],
		[Indices_Param, Serendipity_c63_Param, Threshold_type_Param],
		arginfo{ all:[u_indices, u_serendipity_c63, u_threshold_type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_indices, u_serendipity_c63, u_threshold_type],
			 opt:0,
			 req:[u_indices, u_serendipity_c63, u_threshold_type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPMEM-RETRIEVE 
wl: init_args(exact_only, u_epmem_retrieve).


% annotating U::EPMEM-RETRIEVE 
f_u_epmem_retrieve(Indices_Param, Serendipity_c63_Param, Threshold_type_Param, FnResult) :-
	Env=[bv(u_indices, Indices_Param), bv(u_serendipity_c63, Serendipity_c63_Param), bv(u_threshold_type, Threshold_type_Param)],
	f_u_epmem_retrieve1(Indices_Param,
			    Serendipity_c63_Param,
			    Threshold_type_Param,
			    Eps_Init),
	LEnv=[[bv(u_eps, Eps_Init)]|Env],
	f_u_yloop(
		  [ [u_yfor, u_ep, u_in, u_eps],
		    [u_ydo, [u_epmem_reminding, u_ep, [], []]]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_eps, Eps_Get),
	LetResult=Eps_Get,
	LetResult=FnResult.
:- set_opv(f_u_epmem_retrieve, classof, claz_function),
   set_opv(u_epmem_retrieve, compile_as, kw_function),
   set_opv(u_epmem_retrieve, function, f_u_epmem_retrieve),
   DefunResult=u_epmem_retrieve.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If serendipity? is T, then one less the normal threshold (kind specified",
				     1,
				     5130)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" by threshold-type) will result in retrieval (since serendipity can be",
				     1,
				     5205)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" thought of as providing an extra index).",
				     1,
				     5277)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:4903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This does NOT retrieve episodes that are already recent.",
				     1,
				     5320)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:5378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'epmem-retrieve1',
			    [indices, 'serendipity?', 'threshold-type'],
			    ['mark-init'],
			    
			    [ yloop,
			      [initial, [result, []], [marks, []], [threshx, []]],
			      [yfor, index, in, indices],
			      
			      [ ydo,
				
				[ if,
				  
				  [ setq,
				    index,
				    ['index-intern', index, [quote, old]]
				  ],
				  
				  [ yloop,
				    
				    [ yfor,
				      episode,
				      in,
				      ['ob$gets', index, [quote, indexes]]
				    ],
				    
				    [ ydo,
				      
				      [ if,
					[not, ['recent-episode?', episode]],
					
					[ progn,
					  [setq, marks, ['mark-mark', episode]],
					  
					  [ 'ndbg-roman-nl',
					    '*gate-dbg*',
					    remind,
					    '$STRING'("~A marks on ~A"),
					    marks,
					    episode
					  ],
					  
					  [ setq,
					    threshx,
					    
					    [ if,
					      'serendipity?',
					      
					      [ (-),
						
						[ 'ob$get',
						  episode,
						  'threshold-type'
						],
						1
					      ],
					      
					      [ 'ob$get',
						episode,
						'threshold-type'
					      ]
					    ]
					  ],
					  
					  [ 'ndbg-roman-nl',
					    '*gate-dbg*',
					    remind,
					    '$STRING'("Net thresh for ~A is ~A"),
					    episode,
					    threshx
					  ],
					  
					  [ cond,
					    
					    [ [=, marks, threshx],
					      
					      [ setq,
						result,
						[cons, episode, result]
					      ]
					    ],
					    
					    [ [>, marks, threshx],
					      
					      [ if,
						['memq?', episode, result],
						
						[ 'ndbg-roman-nl',
						  '*gate-dbg*',
						  remind,
						  '$STRING'("Overdetermined epmem-retrieve ~A"),
						  episode
						],
						
						[ setq,
						  result,
						  [cons, episode, result]
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
			      
			      [ yresult,
				
				[ progn,
				  ['mark-unmark-all'],
				  
				  [ if,
				    result,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      remind,
				      '$STRING'("epmem-retrieve1 returns ~A"),
				      result
				    ]
				  ],
				  result
				]
			      ]
			    ]
			  ]).

% annotating U::EPMEM-RETRIEVE1 
wl: lambda_def(defun,
	      u_epmem_retrieve1,
	      f_u_epmem_retrieve1,
	      [u_indices, u_serendipity_c63, u_threshold_type],
	      
	      [ [u_mark_init],
		
		[ u_yloop,
		  [u_initial, [u_result, []], [u_marks, []], [u_threshx, []]],
		  [u_yfor, index, u_in, u_indices],
		  
		  [ u_ydo,
		    
		    [ if,
		      [setq, index, [u_index_intern, index, [quote, u_old]]],
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_episode,
			  u_in,
			  [u_ob_c36_gets, index, [quote, u_indexes]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [not, [u_recent_episode_c63, u_episode]],
			    
			    [ progn,
			      [setq, u_marks, [u_mark_mark, u_episode]],
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_remind,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(~),
					   #\('A'),
					   #\(' '),
					   #\(m),
					   #\(a),
					   #\(r),
					   #\(k),
					   #\(s),
					   #\(' '),
					   #\(o),
					   #\(n),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_marks,
				u_episode
			      ],
			      
			      [ setq,
				u_threshx,
				
				[ if,
				  u_serendipity_c63,
				  
				  [ (-),
				    [u_ob_c36_get, u_episode, u_threshold_type],
				    1
				  ],
				  [u_ob_c36_get, u_episode, u_threshold_type]
				]
			      ],
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_remind,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('N'),
					   #\(e),
					   #\(t),
					   #\(' '),
					   #\(t),
					   #\(h),
					   #\(r),
					   #\(e),
					   #\(s),
					   #\(h),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(i),
					   #\(s),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_episode,
				u_threshx
			      ],
			      
			      [ cond,
				
				[ [=, u_marks, u_threshx],
				  [setq, u_result, [cons, u_episode, u_result]]
				],
				
				[ [>, u_marks, u_threshx],
				  
				  [ if,
				    [u_memq_c63, u_episode, u_result],
				    
				    [ u_ndbg_roman_nl,
				      u_xx_gate_dbg_xx,
				      u_remind,
				      '$ARRAY'([*],
					       claz_base_character,
					       
					       [ #\('O'),
						 #\(v),
						 #\(e),
						 #\(r),
						 #\(d),
						 #\(e),
						 #\(t),
						 #\(e),
						 #\(r),
						 #\(m),
						 #\(i),
						 #\(n),
						 #\(e),
						 #\(d),
						 #\(' '),
						 #\(e),
						 #\(p),
						 #\(m),
						 #\(e),
						 #\(m),
						 #\(-),
						 #\(r),
						 #\(e),
						 #\(t),
						 #\(r),
						 #\(i),
						 #\(e),
						 #\(v),
						 #\(e),
						 #\(' '),
						 #\(~),
						 #\('A')
					       ]),
				      u_episode
				    ],
				    [setq, u_result, [cons, u_episode, u_result]]
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ progn,
		      [u_mark_unmark_all],
		      
		      [ if,
			u_result,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_remind,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(e),
				     #\(p),
				     #\(m),
				     #\(e),
				     #\(m),
				     #\(-),
				     #\(r),
				     #\(e),
				     #\(t),
				     #\(r),
				     #\(i),
				     #\(e),
				     #\(v),
				     #\(e),
				     #\('1'),
				     #\(' '),
				     #\(r),
				     #\(e),
				     #\(t),
				     #\(u),
				     #\(r),
				     #\(n),
				     #\(s),
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


% annotating U::EPMEM-RETRIEVE1 
wl: arglist_info(u_epmem_retrieve1,
		[u_indices, u_serendipity_c63, u_threshold_type],
		[Indices_Param, Serendipity_c63_Param, Threshold_type_Param],
		arginfo{ all:[u_indices, u_serendipity_c63, u_threshold_type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_indices, u_serendipity_c63, u_threshold_type],
			 opt:0,
			 req:[u_indices, u_serendipity_c63, u_threshold_type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPMEM-RETRIEVE1 
wl: init_args(exact_only, u_epmem_retrieve1).


% annotating U::EPMEM-RETRIEVE1 
f_u_epmem_retrieve1(Indices_Param, Serendipity_c63_Param, Threshold_type_Param, FnResult) :-
	Env=[bv(u_indices, Indices_Param), bv(u_serendipity_c63, Serendipity_c63_Param), bv(u_threshold_type, Threshold_type_Param)],
	f_u_mark_init(Mark_init_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_marks, []], [u_threshx, []]],
		    [u_yfor, index, u_in, u_indices],
		    
		    [ u_ydo,
		      
		      [ if,
			[setq, index, [u_index_intern, index, [quote, u_old]]],
			
			[ u_yloop,
			  
			  [ u_yfor,
			    u_episode,
			    u_in,
			    [u_ob_c36_gets, index, [quote, u_indexes]]
			  ],
			  
			  [ u_ydo,
			    
			    [ if,
			      [not, [u_recent_episode_c63, u_episode]],
			      
			      [ progn,
				[setq, u_marks, [u_mark_mark, u_episode]],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_remind,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(~),
					     #\('A'),
					     #\(' '),
					     #\(m),
					     #\(a),
					     #\(r),
					     #\(k),
					     #\(s),
					     #\(' '),
					     #\(o),
					     #\(n),
					     #\(' '),
					     #\(~),
					     #\('A')
					   ]),
				  u_marks,
				  u_episode
				],
				
				[ setq,
				  u_threshx,
				  
				  [ if,
				    u_serendipity_c63,
				    
				    [ (-),
				      
				      [ u_ob_c36_get,
					u_episode,
					u_threshold_type
				      ],
				      1
				    ],
				    [u_ob_c36_get, u_episode, u_threshold_type]
				  ]
				],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_remind,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('N'),
					     #\(e),
					     #\(t),
					     #\(' '),
					     #\(t),
					     #\(h),
					     #\(r),
					     #\(e),
					     #\(s),
					     #\(h),
					     #\(' '),
					     #\(f),
					     #\(o),
					     #\(r),
					     #\(' '),
					     #\(~),
					     #\('A'),
					     #\(' '),
					     #\(i),
					     #\(s),
					     #\(' '),
					     #\(~),
					     #\('A')
					   ]),
				  u_episode,
				  u_threshx
				],
				
				[ cond,
				  
				  [ [=, u_marks, u_threshx],
				    [setq, u_result, [cons, u_episode, u_result]]
				  ],
				  
				  [ [>, u_marks, u_threshx],
				    
				    [ if,
				      [u_memq_c63, u_episode, u_result],
				      
				      [ u_ndbg_roman_nl,
					u_xx_gate_dbg_xx,
					u_remind,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('O'),
						   #\(v),
						   #\(e),
						   #\(r),
						   #\(d),
						   #\(e),
						   #\(t),
						   #\(e),
						   #\(r),
						   #\(m),
						   #\(i),
						   #\(n),
						   #\(e),
						   #\(d),
						   #\(' '),
						   #\(e),
						   #\(p),
						   #\(m),
						   #\(e),
						   #\(m),
						   #\(-),
						   #\(r),
						   #\(e),
						   #\(t),
						   #\(r),
						   #\(i),
						   #\(e),
						   #\(v),
						   #\(e),
						   #\(' '),
						   #\(~),
						   #\('A')
						 ]),
					u_episode
				      ],
				      
				      [ setq,
					u_result,
					[cons, u_episode, u_result]
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
		    
		    [ u_yresult,
		      
		      [ progn,
			[u_mark_unmark_all],
			
			[ if,
			  u_result,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_remind,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(e),
				       #\(p),
				       #\(m),
				       #\(e),
				       #\(m),
				       #\(-),
				       #\(r),
				       #\(e),
				       #\(t),
				       #\(r),
				       #\(i),
				       #\(e),
				       #\(v),
				       #\(e),
				       #\('1'),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(t),
				       #\(u),
				       #\(r),
				       #\(n),
				       #\(s),
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
:- set_opv(f_u_epmem_retrieve1, classof, claz_function),
   set_opv(u_epmem_retrieve1, compile_as, kw_function),
   set_opv(u_epmem_retrieve1, function, f_u_epmem_retrieve1),
   DefunResult=u_epmem_retrieve1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:5378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Assumes index is already copied if this is necessary.",
				     1,
				     6941)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:5378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" new? = 'new-ok if it is OK to create a new index, else 'old",
				     1,
				     6997)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:5378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note that rule indices don't get asserted in *episodic-memory*.",
				     1,
				     7059)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7124 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'index-intern',
			    [index, 'new?'],
			    
			    [ if,
			      ['ty$instance?', index, [quote, rule]],
			      index,
			      
			      [ let,
				
				[ 
				  [ found,
				    ['cx$retrieve', '*episodic-memory*', index]
				  ]
				],
				
				[ if,
				  found,
				  [caar, found],
				  
				  [ if,
				    ['eq?', 'new?', [quote, 'new-ok']],
				    
				    [ progn,
				      ['cx$assert', '*episodic-memory*', index],
				      index
				    ],
				    []
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::INDEX-INTERN 
wl: lambda_def(defun,
	      u_index_intern,
	      f_u_index_intern,
	      [index, u_new_c63],
	      
	      [ 
		[ if,
		  [u_ty_c36_instance_c63, index, [quote, u_rule]],
		  index,
		  
		  [ let,
		    
		    [ 
		      [ u_found,
			[u_cx_c36_retrieve, u_xx_episodic_memory_xx, index]
		      ]
		    ],
		    
		    [ if,
		      u_found,
		      [caar, u_found],
		      
		      [ if,
			[u_eq_c63, u_new_c63, [quote, u_new_ok]],
			
			[ progn,
			  [u_cx_c36_assert, u_xx_episodic_memory_xx, index],
			  index
			],
			[]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::INDEX-INTERN 
wl: arglist_info(u_index_intern,
		[index, u_new_c63],
		[Index_Get32, New_c63_Param],
		arginfo{ all:[index, u_new_c63],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[index, u_new_c63],
			 opt:0,
			 req:[index, u_new_c63],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INDEX-INTERN 
wl: init_args(exact_only, u_index_intern).


% annotating U::INDEX-INTERN 
f_u_index_intern(Index_Get32, New_c63_Param, FnResult) :-
	Env=[bv(index, Index_Get32), bv(u_new_c63, New_c63_Param)],
	f_u_ty_c36_instance_c63(Index_Get32, u_rule, IFTEST),
	(   IFTEST\==[]
	->  FnResult=Index_Get32
	;   get_var(Env, u_xx_episodic_memory_xx, Xx_episodic_memory_xx_Get),
	    f_u_cx_c36_retrieve(Xx_episodic_memory_xx_Get,
				Index_Get32,
				Found_Init),
	    LEnv=[[bv(u_found, Found_Init)]|Env],
	    get_var(LEnv, u_found, IFTEST23),
	    (   IFTEST23\==[]
	    ->  get_var(LEnv, u_found, Found_Get27),
		cl_caar(Found_Get27, TrueResult34),
		FnResult=TrueResult34
	    ;   f_u_eq_c63(u_new_c63, [quote, u_new_ok], IFTEST28),
		(   IFTEST28\==[]
		->  get_var(LEnv,
			    u_xx_episodic_memory_xx,
			    Xx_episodic_memory_xx_Get30),
		    f_u_cx_c36_assert(Xx_episodic_memory_xx_Get30,
				      Index_Get32,
				      C36_assert_Ret),
		    FnResult=Index_Get32
		;   FnResult=[]
		)
	    )
	).
:- set_opv(f_u_index_intern, classof, claz_function),
   set_opv(u_index_intern, compile_as, kw_function),
   set_opv(u_index_intern, function, f_u_index_intern),
   DefunResult=u_index_intern.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7124 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Retrieve returns a list of bindings, where the car of each",
				     12,
				     7273)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7124 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" is not T but rather the retrieved ob.",
				     12,
				     7345)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7124 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7610)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7124 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Reminding mechanism", 1, 7612)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7124 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7634)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7636 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*recent-indices*', []]).
:- set_var(TLEnv3, setq, u_xx_recent_indices_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7636 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: get rid of the superfluous consing.",
				     1,
				     7666)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7709 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    remindings,
			    [],
			    
			    [ 'epmem-retrieve',
			      
			      [ append,
				'*recent-indices*',
				['get-emotion-indices']
			      ],
			      [],
			      [quote, 'reminding-threshold']
			    ]
			  ]).

% annotating U::REMINDINGS 
wl: lambda_def(defun,
	      u_remindings,
	      f_u_remindings,
	      [],
	      
	      [ 
		[ u_epmem_retrieve,
		  [append, u_xx_recent_indices_xx, [u_get_emotion_indices]],
		  [],
		  [quote, u_reminding_threshold]
		]
	      ]).


% annotating U::REMINDINGS 
wl: arglist_info(u_remindings,
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

% annotating U::REMINDINGS 
wl: init_args(exact_only, u_remindings).


% annotating U::REMINDINGS 
f_u_remindings(FnResult) :-
	Env=[],
	get_var(Env, u_xx_recent_indices_xx, Xx_recent_indices_xx_Get),
	f_u_get_emotion_indices(Emotion_indices_Ret),
	cl_append(Xx_recent_indices_xx_Get,
		  Emotion_indices_Ret,
		  Epmem_retrieve_Param),
	f_u_epmem_retrieve(Epmem_retrieve_Param,
			   [],
			   u_reminding_threshold,
			   Reminding_threshold),
	Reminding_threshold=FnResult.
:- set_opv(f_u_remindings, classof, claz_function),
   set_opv(u_remindings, compile_as, kw_function),
   set_opv(u_remindings, function, f_u_remindings),
   DefunResult=u_remindings.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*pos-emot-ptn*',
			    ['ob$fcreate', [quote, ['POS-EMOTION']]]
			  ]).
:- f_u_ob_c36_fcreate([quote, [u_pos_emotion]], _Ignored),
   set_var(TLEnv3, u_xx_pos_emot_ptn_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7919 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*neg-emot-ptn*',
			    ['ob$fcreate', [quote, ['NEG-EMOTION']]]
			  ]).
:- f_u_ob_c36_fcreate([quote, [u_neg_emotion]], _Ignored),
   set_var(TLEnv3, u_xx_neg_emot_ptn_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:7969 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*pos-neg-list*',
			    [list, '*pos-emot-ptn*', '*neg-emot-ptn*']
			  ]).
:- get_var(TLEnv3, u_xx_neg_emot_ptn_xx, Xx_neg_emot_ptn_xx_Get),
   get_var(TLEnv3, u_xx_pos_emot_ptn_xx, Xx_pos_emot_ptn_xx_Get),
   _Ignored=[Xx_pos_emot_ptn_xx_Get, Xx_neg_emot_ptn_xx_Get],
   set_var(TLEnv3, u_xx_pos_neg_list_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8028 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setq, '*pos-list*', [list, '*pos-emot-ptn*']]).
:- get_var(TLEnv3, u_xx_pos_emot_ptn_xx, Xx_pos_emot_ptn_xx_Get),
   _Ignored=[Xx_pos_emot_ptn_xx_Get],
   set_var(TLEnv3, u_xx_pos_list_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setq, '*neg-list*', [list, '*neg-emot-ptn*']]).
:- get_var(TLEnv3, u_xx_neg_emot_ptn_xx, Xx_neg_emot_ptn_xx_Get),
   _Ignored=[Xx_neg_emot_ptn_xx_Get],
   set_var(TLEnv3, u_xx_neg_list_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: in the future, we would like to index on the \"quality\" of the",
				     1,
				     8110)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" emotion (e.g., embarrassment) in addition to the sign.",
				     1,
				     8180)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8236 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-emotion-indices',
			    [],
			    
			    [ cond,
			      
			      [ ['fl>', '*overall-emotional-state*', 1.0],
				'*pos-list*'
			      ],
			      
			      [ ['fl<', '*overall-emotional-state*', -1.0],
				'*neg-list*'
			      ],
			      [else, []]
			    ]
			  ]).

% annotating U::GET-EMOTION-INDICES 
wl: lambda_def(defun,
	      u_get_emotion_indices,
	      f_u_get_emotion_indices,
	      [],
	      
	      [ 
		[ cond,
		  
		  [ [u_fl_c62, u_xx_overall_emotional_state_xx, 1.0],
		    u_xx_pos_list_xx
		  ],
		  
		  [ [u_fl_c60, u_xx_overall_emotional_state_xx, -1.0],
		    u_xx_neg_list_xx
		  ],
		  [u_else, []]
		]
	      ]).


% annotating U::GET-EMOTION-INDICES 
wl: arglist_info(u_get_emotion_indices,
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

% annotating U::GET-EMOTION-INDICES 
wl: init_args(exact_only, u_get_emotion_indices).


% annotating U::GET-EMOTION-INDICES 
f_u_get_emotion_indices(ElseResult21) :-
	Env=[],
	f_u_fl_c62(u_xx_overall_emotional_state_xx, 1.0, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_pos_list_xx, Xx_pos_list_xx_Get),
	    ElseResult21=Xx_pos_list_xx_Get
	;   f_u_fl_c60(u_xx_overall_emotional_state_xx, -1.0, IFTEST13),
	    (   IFTEST13\==[]
	    ->  get_var(Env, u_xx_neg_list_xx, Xx_neg_list_xx_Get),
		ElseResult21=Xx_neg_list_xx_Get
	    ;   get_var(Env, u_else, IFTEST16),
		(   IFTEST16\==[]
		->  ElseResult21=[]
		;   ElseResult21=[]
		)
	    )
	).
:- set_opv(f_u_get_emotion_indices, classof, claz_function),
   set_opv(u_get_emotion_indices, compile_as, kw_function),
   set_opv(u_get_emotion_indices, function, f_u_get_emotion_indices),
   DefunResult=u_get_emotion_indices.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8396 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*recent-index-max-length*', 6]).
:- set_var(TLEnv3, setq, u_xx_recent_index_max_length_xx, 6).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8432 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-recent-index',
			    [index],
			    
			    [ if,
			      [not, ['memq?', index, '*recent-indices*']],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  remind,
				  '$STRING'("Activate index ~A"),
				  index
				],
				
				[ if,
				  
				  [ (>=),
				    [length, '*recent-indices*'],
				    '*recent-index-max-length*'
				  ],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      remind,
				      '$STRING'("Index ~A fades"),
				      [car, '*recent-indices*']
				    ],
				    
				    [ setq,
				      '*recent-indices*',
				      [cdr, '*recent-indices*']
				    ]
				  ]
				],
				
				[ setq,
				  '*recent-indices*',
				  ['append!', '*recent-indices*', [list, index]]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				remind,
				'$STRING'("Index ~A already active"),
				index
			      ]
			    ]
			  ]).

% annotating U::ADD-RECENT-INDEX 
wl: lambda_def(defun,
	      u_add_recent_index,
	      f_u_add_recent_index,
	      [index],
	      
	      [ 
		[ if,
		  [not, [u_memq_c63, index, u_xx_recent_indices_xx]],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_remind,
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
				 #\(i),
				 #\(n),
				 #\(d),
				 #\(e),
				 #\(x),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      index
		    ],
		    
		    [ if,
		      
		      [ (>=),
			[length, u_xx_recent_indices_xx],
			u_xx_recent_index_max_length_xx
		      ],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_remind,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('I'),
				     #\(n),
				     #\(d),
				     #\(e),
				     #\(x),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(' '),
				     #\(f),
				     #\(a),
				     #\(d),
				     #\(e),
				     #\(s)
				   ]),
			  [car, u_xx_recent_indices_xx]
			],
			
			[ setq,
			  u_xx_recent_indices_xx,
			  [cdr, u_xx_recent_indices_xx]
			]
		      ]
		    ],
		    
		    [ setq,
		      u_xx_recent_indices_xx,
		      [u_append_c33, u_xx_recent_indices_xx, [list, index]]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_remind,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('I'),
			       #\(n),
			       #\(d),
			       #\(e),
			       #\(x),
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
			       #\(a),
			       #\(c),
			       #\(t),
			       #\(i),
			       #\(v),
			       #\(e)
			     ]),
		    index
		  ]
		]
	      ]).


% annotating U::ADD-RECENT-INDEX 
wl: arglist_info(u_add_recent_index,
		[index],
		[Index_Param],
		arginfo{ all:[index],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[index],
			 opt:0,
			 req:[index],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ADD-RECENT-INDEX 
wl: init_args(exact_only, u_add_recent_index).


% annotating U::ADD-RECENT-INDEX 
f_u_add_recent_index(Index_Param, FnResult) :-
	Env=[bv(index, Index_Param)],
	f_u_memq_c63(index, u_xx_recent_indices_xx, PredArgResult),
	(   PredArgResult==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_remind,
			      
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
					   #\(i),
					   #\(n),
					   #\(d),
					   #\(e),
					   #\(x),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				index
			      ],
			      Roman_nl_Ret),
	    get_var(Env, u_xx_recent_indices_xx, Xx_recent_indices_xx_Get21),
	    cl_length(Xx_recent_indices_xx_Get21, PredArg1Result),
	    get_var(Env,
		    u_xx_recent_index_max_length_xx,
		    Xx_recent_index_max_length_xx_Get),
	    (   PredArg1Result>=Xx_recent_index_max_length_xx_Get
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_remind,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('I'),
					       #\(n),
					       #\(d),
					       #\(e),
					       #\(x),
					       #\(' '),
					       #\(~),
					       #\('A'),
					       #\(' '),
					       #\(f),
					       #\(a),
					       #\(d),
					       #\(e),
					       #\(s)
					     ]),
				    [car, u_xx_recent_indices_xx]
				  ],
				  Roman_nl_Ret28),
		cl_cdr(Xx_recent_indices_xx_Get21, TrueResult),
		set_var(Env, u_xx_recent_indices_xx, TrueResult),
		_51248=TrueResult
	    ;   _51248=[]
	    ),
	    f_u_append_c33(u_xx_recent_indices_xx, [list, index], TrueResult23),
	    set_var(Env, u_xx_recent_indices_xx, TrueResult23),
	    FnResult=TrueResult23
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_remind,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(n),
					   #\(d),
					   #\(e),
					   #\(x),
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
					   #\(a),
					   #\(c),
					   #\(t),
					   #\(i),
					   #\(v),
					   #\(e)
					 ]),
				index
			      ],
			      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_add_recent_index, classof, claz_function),
   set_opv(u_add_recent_index, compile_as, kw_function),
   set_opv(u_add_recent_index, function, f_u_add_recent_index),
   DefunResult=u_add_recent_index.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8432 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9016)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8432 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Environmental object input", 1, 9018)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:8432 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9047)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9049 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'environmental-object-input',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Taking optional object or concept input")
			    ],
			    
			    [ let,
			      
			      [ 
				[ concepts,
				  
				  [ 'enter-concepts',
				    '*reality*',
				    '*me-belief-path*'
				  ]
				]
			      ],
			      
			      [ if,
				['null?', concepts],
				[],
				
				[ let,
				  
				  [ 
				    [ result1,
				      ['run-object-serendipities', concepts]
				    ],
				    [result2, ['entered-concept-serendipity']]
				  ],
				  [or, result1, result2]
				]
			      ]
			    ]
			  ]).

% annotating U::ENVIRONMENTAL-OBJECT-INPUT 
wl: lambda_def(defun,
	      u_environmental_object_input,
	      f_u_environmental_object_input,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('T'),
			     #\(a),
			     #\(k),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(o),
			     #\(p),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(o),
			     #\(b),
			     #\(j),
			     #\(e),
			     #\(c),
			     #\(t),
			     #\(' '),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(c),
			     #\(o),
			     #\(n),
			     #\(c),
			     #\(e),
			     #\(p),
			     #\(t),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(p),
			     #\(u),
			     #\(t)
			   ])
		],
		
		[ let,
		  
		  [ 
		    [ u_concepts,
		      
		      [ u_enter_concepts,
			u_xx_reality_xx,
			u_xx_me_belief_path_xx
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, u_concepts],
		    [],
		    
		    [ let,
		      
		      [ [u_result1, [u_run_object_serendipities, u_concepts]],
			[u_result2, [u_entered_concept_serendipity]]
		      ],
		      [or, u_result1, u_result2]
		    ]
		  ]
		]
	      ]).


% annotating U::ENVIRONMENTAL-OBJECT-INPUT 
wl: arglist_info(u_environmental_object_input,
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

% annotating U::ENVIRONMENTAL-OBJECT-INPUT 
wl: init_args(exact_only, u_environmental_object_input).


% annotating U::ENVIRONMENTAL-OBJECT-INPUT 
f_u_environmental_object_input(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
				       #\(a),
				       #\(k),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(o),
				       #\(p),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(o),
				       #\(b),
				       #\(j),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(' '),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(p),
				       #\(t),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(p),
				       #\(u),
				       #\(t)
				     ])
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_enter_concepts(Xx_reality_xx_Get,
			   Xx_me_belief_path_xx_Get,
			   Concepts_Init),
	LEnv=[[bv(u_concepts, Concepts_Init)]|Env],
	f_u_null_c63(u_concepts, IFTEST),
	(   IFTEST\==[]
	->  FnResult=[]
	;   get_var(LEnv, u_concepts, Concepts_Get),
	    f_u_run_object_serendipities(Concepts_Get, Result1_Init),
	    f_u_entered_concept_serendipity(Result2_Init),
	    Env=[[bv(u_result1, Result1_Init), bv(u_result2, Result2_Init)]|LEnv],
	    (   get_var(Env, u_result1, Result1_Get),
		Result1_Get\==[],
		FnResult=Result1_Get
	    ->  true
	    ;   get_var(Env, u_result2, Result2_Get),
		FnResult=Result2_Get
	    )
	).
:- set_opv(f_u_environmental_object_input, classof, claz_function),
   set_opv(u_environmental_object_input, compile_as, kw_function),
   set_opv(u_environmental_object_input,
	   function,
	   f_u_environmental_object_input),
   DefunResult=u_environmental_object_input.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9449 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-object-serendipities',
			    [concepts],
			    
			    [ let,
			      
			      [ 
				[ episodes,
				  
				  [ 'epmem-retrieve1',
				    ['append!', concepts, '*recent-indices*'],
				    t,
				    [quote, 'reminding-threshold']
				  ]
				],
				['old-recent-episodes', '*recent-episodes*'],
				[temp, []]
			      ],
			      
			      [ setq,
				'*recent-episodes*',
				['append!', '*recent-episodes*', episodes]
			      ],
			      [setq, temp, ['run-serendipities']],
			      [setq, '*recent-episodes*', 'old-recent-episodes'],
			      
			      [ if,
				temp,
				
				[ yloop,
				  [yfor, episode, in, episodes],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ 'any?',
					[lambda, [d], ['memq?', d, [cdr, temp]]],
					['ob$get', episode, [quote, descendants]]
				      ],
				      ['epmem-reminding', episode, t, []]
				    ]
				  ]
				]
			      ],
			      temp
			    ]
			  ]).

% annotating U::RUN-OBJECT-SERENDIPITIES 
wl: lambda_def(defun,
	      u_run_object_serendipities,
	      f_u_run_object_serendipities,
	      [u_concepts],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_episodes,
		      
		      [ u_epmem_retrieve1,
			[u_append_c33, u_concepts, u_xx_recent_indices_xx],
			t,
			[quote, u_reminding_threshold]
		      ]
		    ],
		    [u_old_recent_episodes, u_xx_recent_episodes_xx],
		    [u_temp, []]
		  ],
		  
		  [ setq,
		    u_xx_recent_episodes_xx,
		    [u_append_c33, u_xx_recent_episodes_xx, u_episodes]
		  ],
		  [setq, u_temp, [u_run_serendipities]],
		  [setq, u_xx_recent_episodes_xx, u_old_recent_episodes],
		  
		  [ if,
		    u_temp,
		    
		    [ u_yloop,
		      [u_yfor, u_episode, u_in, u_episodes],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ u_any_c63,
			    [lambda, [u_d], [u_memq_c63, u_d, [cdr, u_temp]]],
			    [u_ob_c36_get, u_episode, [quote, u_descendants]]
			  ],
			  [u_epmem_reminding, u_episode, t, []]
			]
		      ]
		    ]
		  ],
		  u_temp
		]
	      ]).


% annotating U::RUN-OBJECT-SERENDIPITIES 
wl: arglist_info(u_run_object_serendipities,
		[u_concepts],
		[Concepts_Param],
		arginfo{ all:[u_concepts],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_concepts],
			 opt:0,
			 req:[u_concepts],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-OBJECT-SERENDIPITIES 
wl: init_args(exact_only, u_run_object_serendipities).


% annotating U::RUN-OBJECT-SERENDIPITIES 
f_u_run_object_serendipities(Concepts_Param, IFTEST) :-
	Env=[bv(u_concepts, Concepts_Param)],
	f_u_append_c33(u_concepts, u_xx_recent_indices_xx, Xx_recent_indices_xx),
	f_u_epmem_retrieve1(Xx_recent_indices_xx,
			    t,
			    u_reminding_threshold,
			    Episodes_Init),
	get_var(Env, u_xx_recent_episodes_xx, Xx_recent_episodes_xx_Get),
	LEnv=[[bv(u_episodes, Episodes_Init), bv(u_old_recent_episodes, Xx_recent_episodes_xx_Get), bv(u_temp, [])]|Env],
	f_u_append_c33(u_xx_recent_episodes_xx, u_episodes, Episodes),
	set_var(LEnv, u_xx_recent_episodes_xx, Episodes),
	f_u_run_serendipities(Temp),
	set_var(LEnv, u_temp, Temp),
	get_var(LEnv, u_old_recent_episodes, Old_recent_episodes_Get),
	set_var(LEnv, u_xx_recent_episodes_xx, Old_recent_episodes_Get),
	get_var(LEnv, u_temp, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_yfor, u_episode, u_in, u_episodes],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_any_c63,
			      [lambda, [u_d], [u_memq_c63, u_d, [cdr, u_temp]]],
			      [u_ob_c36_get, u_episode, [quote, u_descendants]]
			    ],
			    [u_epmem_reminding, u_episode, t, []]
			  ]
			]
		      ],
		      TrueResult),
	    _51714=TrueResult
	;   _51714=[]
	).
:- set_opv(f_u_run_object_serendipities, classof, claz_function),
   set_opv(u_run_object_serendipities, compile_as, kw_function),
   set_opv(u_run_object_serendipities, function, f_u_run_object_serendipities),
   DefunResult=u_run_object_serendipities.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9449 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" was (cdr temp)", 51, 9902)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9449 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10159)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9449 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Episode recency mechanism", 1, 10161)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:9449 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10189)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10191 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*recent-episodes*', []]).
:- set_var(TLEnv3, setq, u_xx_recent_episodes_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10221 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*recent-ep-max-length*', 4]).
:- set_var(TLEnv3, setq, u_xx_recent_ep_max_length_xx, 4).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10254 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-recent',
			    [episode],
			    
			    [ yloop,
			      [yfor, ep, in, '*recent-episodes*'],
			      
			      [ ydo,
				
				[ if,
				  
				  [ 'memq?',
				    ep,
				    ['ob$get', episode, [quote, descendants]]
				  ],
				  
				  [ setq,
				    '*recent-episodes*',
				    ['delq!', ep, '*recent-episodes*']
				  ]
				]
			      ]
			    ],
			    
			    [ if,
			      
			      [ (>=),
				[length, '*recent-episodes*'],
				'*recent-ep-max-length*'
			      ],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Episode ~A fades"),
				  [car, '*recent-episodes*']
				],
				
				[ setq,
				  '*recent-episodes*',
				  [cdr, '*recent-episodes*']
				]
			      ]
			    ],
			    
			    [ setq,
			      '*recent-episodes*',
			      ['append!', '*recent-episodes*', [list, episode]]
			    ]
			  ]).

% annotating U::ADD-RECENT 
wl: lambda_def(defun,
	      u_add_recent,
	      f_u_add_recent,
	      [u_episode],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_ep, u_in, u_xx_recent_episodes_xx],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ u_memq_c63,
			u_ep,
			[u_ob_c36_get, u_episode, [quote, u_descendants]]
		      ],
		      
		      [ setq,
			u_xx_recent_episodes_xx,
			[u_delq_c33, u_ep, u_xx_recent_episodes_xx]
		      ]
		    ]
		  ]
		],
		
		[ if,
		  
		  [ (>=),
		    [length, u_xx_recent_episodes_xx],
		    u_xx_recent_ep_max_length_xx
		  ],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('E'),
				 #\(p),
				 #\(i),
				 #\(s),
				 #\(o),
				 #\(d),
				 #\(e),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(f),
				 #\(a),
				 #\(d),
				 #\(e),
				 #\(s)
			       ]),
		      [car, u_xx_recent_episodes_xx]
		    ],
		    
		    [ setq,
		      u_xx_recent_episodes_xx,
		      [cdr, u_xx_recent_episodes_xx]
		    ]
		  ]
		],
		
		[ setq,
		  u_xx_recent_episodes_xx,
		  [u_append_c33, u_xx_recent_episodes_xx, [list, u_episode]]
		]
	      ]).


% annotating U::ADD-RECENT 
wl: arglist_info(u_add_recent,
		[u_episode],
		[Episode_Param],
		arginfo{ all:[u_episode],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_episode],
			 opt:0,
			 req:[u_episode],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ADD-RECENT 
wl: init_args(exact_only, u_add_recent).


% annotating U::ADD-RECENT 
f_u_add_recent(Episode_Param, FnResult) :-
	Env=[bv(u_episode, Episode_Param)],
	f_u_yloop(
		  [ [u_yfor, u_ep, u_in, u_xx_recent_episodes_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_memq_c63,
			  u_ep,
			  [u_ob_c36_get, u_episode, [quote, u_descendants]]
			],
			
			[ setq,
			  u_xx_recent_episodes_xx,
			  [u_delq_c33, u_ep, u_xx_recent_episodes_xx]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(Env, u_xx_recent_episodes_xx, Xx_recent_episodes_xx_Get18),
	cl_length(Xx_recent_episodes_xx_Get18, PredArg1Result),
	get_var(Env,
		u_xx_recent_ep_max_length_xx,
		Xx_recent_ep_max_length_xx_Get),
	(   PredArg1Result>=Xx_recent_ep_max_length_xx_Get
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('E'),
					   #\(p),
					   #\(i),
					   #\(s),
					   #\(o),
					   #\(d),
					   #\(e),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(f),
					   #\(a),
					   #\(d),
					   #\(e),
					   #\(s)
					 ]),
				[car, u_xx_recent_episodes_xx]
			      ],
			      Roman_nl_Ret),
	    cl_cdr(Xx_recent_episodes_xx_Get18, TrueResult),
	    set_var(Env, u_xx_recent_episodes_xx, TrueResult),
	    _51358=TrueResult
	;   _51358=[]
	),
	f_u_append_c33(u_xx_recent_episodes_xx,
		       [list, u_episode],
		       Xx_recent_episodes_xx),
	set_var(Env, u_xx_recent_episodes_xx, Xx_recent_episodes_xx),
	Xx_recent_episodes_xx=FnResult.
:- set_opv(f_u_add_recent, classof, claz_function),
   set_opv(u_add_recent, compile_as, kw_function),
   set_opv(u_add_recent, function, f_u_add_recent),
   DefunResult=u_add_recent.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'recent-episode?',
			    [episode],
			    
			    [ 'any?',
			      
			      [ lambda,
				[ep],
				
				[ 'memq?',
				  episode,
				  ['ob$get', ep, [quote, descendants]]
				]
			      ],
			      '*recent-episodes*'
			    ]
			  ]).

% annotating U::RECENT-EPISODE? 
wl: lambda_def(defun,
	      u_recent_episode_c63,
	      f_u_recent_episode_c63,
	      [u_episode],
	      
	      [ 
		[ u_any_c63,
		  
		  [ lambda,
		    [u_ep],
		    
		    [ u_memq_c63,
		      u_episode,
		      [u_ob_c36_get, u_ep, [quote, u_descendants]]
		    ]
		  ],
		  u_xx_recent_episodes_xx
		]
	      ]).


% annotating U::RECENT-EPISODE? 
wl: arglist_info(u_recent_episode_c63,
		[u_episode],
		[Episode_Param],
		arginfo{ all:[u_episode],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_episode],
			 opt:0,
			 req:[u_episode],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RECENT-EPISODE? 
wl: init_args(exact_only, u_recent_episode_c63).


% annotating U::RECENT-EPISODE? 
f_u_recent_episode_c63(Episode_Param, FnResult) :-
	Env=[bv(u_episode, Episode_Param)],
	f_u_any_c63(
		    [ lambda,
		      [u_ep],
		      
		      [ u_memq_c63,
			u_episode,
			[u_ob_c36_get, u_ep, [quote, u_descendants]]
		      ]
		    ],
		    u_xx_recent_episodes_xx,
		    Xx_recent_episodes_xx),
	Xx_recent_episodes_xx=FnResult.
:- set_opv(f_u_recent_episode_c63, classof, claz_function),
   set_opv(u_recent_episode_c63, compile_as, kw_function),
   set_opv(u_recent_episode_c63, function, f_u_recent_episode_c63),
   DefunResult=u_recent_episode_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was (memq? episode *recent-episodes*)",
				     1,
				     10889)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: if an episode is defined after the system is already going,",
				     1,
				     10930)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:10764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" this should be called.", 1, 10998)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'epmem-reminding',
			    [episode, 'no-serendipities?', 'new-stored-ep?'],
			    
			    [ if,
			      [not, ['recent-episode?', episode]],
			      
			      [ progn,
				
				[ if,
				  [not, 'new-stored-ep?'],
				  
				  [ progn,
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("Episodic reminding")
				    ],
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'(" of ~A"),
				      episode
				    ],
				    ['ndbg-newline', '*gate-dbg*', rule],
				    ['generate-episode', episode]
				  ]
				],
				['add-recent', episode],
				
				[ yloop,
				  
				  [ yfor,
				    index,
				    in,
				    
				    [ 'ob$gets',
				      episode,
				      [quote, 'indexed-under']
				    ]
				  ],
				  
				  [ ydo,
				    
				    [ if,
				      ['ty$instance?', index, [quote, emotion]],
				      
				      [ if,
					[not, 'new-stored-ep?'],
					
					[ progn,
					  
					  [ setq,
					    '*reality*',
					    '*reality-lookahead*'
					  ],
					  
					  [ 'ndbg-roman-nl',
					    '*gate-dbg*',
					    rule,
					    '$STRING'("Reactivate emotion")
					  ],
					  
					  [ 'add-emotion',
					    ['ob$get', episode, [quote, goal]],
					    index,
					    ['ob$get', episode, [quote, realism]],
					    '*reality*'
					  ]
					]
				      ],
				      ['add-recent-index', index]
				    ]
				  ]
				],
				
				[ if,
				  ['null?', 'no-serendipities?'],
				  
				  [ 'run-serendipity',
				    ['inaccessible-planning-rules', episode],
				    []
				  ]
				],
				[remindings]
			      ]
			    ]
			  ]).

% annotating U::EPMEM-REMINDING 
wl: lambda_def(defun,
	      u_epmem_reminding,
	      f_u_epmem_reminding,
	      [u_episode, u_no_serendipities_c63, u_new_stored_ep_c63],
	      
	      [ 
		[ if,
		  [not, [u_recent_episode_c63, u_episode]],
		  
		  [ progn,
		    
		    [ if,
		      [not, u_new_stored_ep_c63],
		      
		      [ progn,
			
			[ u_ndbg_roman,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('E'),
				     #\(p),
				     #\(i),
				     #\(s),
				     #\(o),
				     #\(d),
				     #\(i),
				     #\(c),
				     #\(' '),
				     #\(r),
				     #\(e),
				     #\(m),
				     #\(i),
				     #\(n),
				     #\(d),
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
				   [#\(' '), #\(o), #\(f), #\(' '), #\(~), #\('A')]),
			  u_episode
			],
			[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			[u_generate_episode, u_episode]
		      ]
		    ],
		    [u_add_recent, u_episode],
		    
		    [ u_yloop,
		      
		      [ u_yfor,
			index,
			u_in,
			[u_ob_c36_gets, u_episode, [quote, u_indexed_under]]
		      ],
		      
		      [ u_ydo,
			
			[ if,
			  [u_ty_c36_instance_c63, index, [quote, u_emotion]],
			  
			  [ if,
			    [not, u_new_stored_ep_c63],
			    
			    [ progn,
			      [setq, u_xx_reality_xx, u_xx_reality_lookahead_xx],
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('R'),
					   #\(e),
					   #\(a),
					   #\(c),
					   #\(t),
					   #\(i),
					   #\(v),
					   #\(a),
					   #\(t),
					   #\(e),
					   #\(' '),
					   #\(e),
					   #\(m),
					   #\(o),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n)
					 ])
			      ],
			      
			      [ u_add_emotion,
				[u_ob_c36_get, u_episode, [quote, u_goal]],
				index,
				[u_ob_c36_get, u_episode, [quote, u_realism]],
				u_xx_reality_xx
			      ]
			    ]
			  ],
			  [u_add_recent_index, index]
			]
		      ]
		    ],
		    
		    [ if,
		      [u_null_c63, u_no_serendipities_c63],
		      
		      [ u_run_serendipity,
			[u_inaccessible_planning_rules, u_episode],
			[]
		      ]
		    ],
		    [u_remindings]
		  ]
		]
	      ]).


% annotating U::EPMEM-REMINDING 
wl: arglist_info(u_epmem_reminding,
		[u_episode, u_no_serendipities_c63, u_new_stored_ep_c63],
		
		[ Episode_Param,
		  No_serendipities_c63_Param,
		  New_stored_ep_c63_Param
		],
		arginfo{ all:
			     [ u_episode,
			       u_no_serendipities_c63,
			       u_new_stored_ep_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_episode,
				 u_no_serendipities_c63,
				 u_new_stored_ep_c63
			       ],
			 opt:0,
			 req:
			     [ u_episode,
			       u_no_serendipities_c63,
			       u_new_stored_ep_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPMEM-REMINDING 
wl: init_args(exact_only, u_epmem_reminding).


% annotating U::EPMEM-REMINDING 
f_u_epmem_reminding(Episode_Param, No_serendipities_c63_Param, New_stored_ep_c63_Param, FnResult) :-
	Env=[bv(u_episode, Episode_Param), bv(u_no_serendipities_c63, No_serendipities_c63_Param), bv(u_new_stored_ep_c63, New_stored_ep_c63_Param)],
	f_u_recent_episode_c63(Episode_Param, PredArgResult),
	(   PredArgResult==[]
	->  (   New_stored_ep_c63_Param==[]
	    ->  f_u_ndbg_roman(u_xx_gate_dbg_xx,
			       u_rule,
			       
			       [ '$ARRAY'([*],
					  claz_base_character,
					  
					  [ #\('E'),
					    #\(p),
					    #\(i),
					    #\(s),
					    #\(o),
					    #\(d),
					    #\(i),
					    #\(c),
					    #\(' '),
					    #\(r),
					    #\(e),
					    #\(m),
					    #\(i),
					    #\(n),
					    #\(d),
					    #\(i),
					    #\(n),
					    #\(g)
					  ])
			       ],
			       Ndbg_roman_Ret),
		f_u_ndbg_roman(u_xx_gate_dbg_xx,
			       u_rule,
			       
			       [ '$ARRAY'([*],
					  claz_base_character,
					  
					  [ #\(' '),
					    #\(o),
					    #\(f),
					    #\(' '),
					    #\(~),
					    #\('A')
					  ]),
				 u_episode
			       ],
			       Ndbg_roman_Ret37),
		f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
		f_u_generate_episode(Episode_Param, TrueResult),
		_53470=TrueResult
	    ;   _53470=[]
	    ),
	    f_u_add_recent(Episode_Param, Add_recent_Ret),
	    f_u_yloop(
		      [ 
			[ u_yfor,
			  index,
			  u_in,
			  [u_ob_c36_gets, u_episode, [quote, u_indexed_under]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_ty_c36_instance_c63, index, [quote, u_emotion]],
			    
			    [ if,
			      [not, u_new_stored_ep_c63],
			      
			      [ progn,
				
				[ setq,
				  u_xx_reality_xx,
				  u_xx_reality_lookahead_xx
				],
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('R'),
					     #\(e),
					     #\(a),
					     #\(c),
					     #\(t),
					     #\(i),
					     #\(v),
					     #\(a),
					     #\(t),
					     #\(e),
					     #\(' '),
					     #\(e),
					     #\(m),
					     #\(o),
					     #\(t),
					     #\(i),
					     #\(o),
					     #\(n)
					   ])
				],
				
				[ u_add_emotion,
				  [u_ob_c36_get, u_episode, [quote, u_goal]],
				  index,
				  [u_ob_c36_get, u_episode, [quote, u_realism]],
				  u_xx_reality_xx
				]
			      ]
			    ],
			    [u_add_recent_index, index]
			  ]
			]
		      ],
		      Yloop_Ret),
	    f_u_null_c63(u_no_serendipities_c63, IFTEST27),
	    (   IFTEST27\==[]
	    ->  f_u_inaccessible_planning_rules(Episode_Param,
						Run_serendipity_Param),
		f_u_run_serendipity(Run_serendipity_Param, [], TrueResult30),
		_53652=TrueResult30
	    ;   _53652=[]
	    ),
	    f_u_remindings(TrueResult31),
	    FnResult=TrueResult31
	;   FnResult=[]
	).
:- set_opv(f_u_epmem_reminding, classof, claz_function),
   set_opv(u_epmem_reminding, compile_as, kw_function),
   set_opv(u_epmem_reminding, function, f_u_epmem_reminding),
   DefunResult=u_epmem_reminding.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 11142)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Print out stuff", 3, 11146)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 11166)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 11394)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Add to recent episodes.", 3, 11398)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 11426)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 11453)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Add other indices of episode to recent indices.",
				     3,
				     11457)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: this effectively results in a `reminding' link (subject",
				     3,
				     11509)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   to threshold requirements) from any two episodes having",
				     3,
				     11575)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("   the same index.", 3, 11637)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 11659)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 12146)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Run serendipities unless told not to.",
				     3,
				     12150)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: inacc. planning rules can be plans OR inferences!",
				     3,
				     12192)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 12252)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 12355)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Get any new remindings from indices now active.",
				     3,
				     12359)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:11022 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 3, 12411)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12431 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*auto-rule-plausibility*', 0.7]).
:- set_var(TLEnv3, setq, u_xx_auto_rule_plausibility_xx, 0.7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12468 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'episode-defn-goal', [defn], [car, defn]]).

% annotating U::EPISODE-DEFN-GOAL 
wl: lambda_def(defun,
	      u_episode_defn_goal,
	      f_u_episode_defn_goal,
	      [u_defn],
	      [[car, u_defn]]).


% annotating U::EPISODE-DEFN-GOAL 
wl: arglist_info(u_episode_defn_goal,
		[u_defn],
		[Defn_Param],
		arginfo{ all:[u_defn],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_defn],
			 opt:0,
			 req:[u_defn],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-DEFN-GOAL 
wl: init_args(exact_only, u_episode_defn_goal).


% annotating U::EPISODE-DEFN-GOAL 
f_u_episode_defn_goal(Defn_Param, FnResult) :-
	cl_car(Defn_Param, Car_Ret),
	Car_Ret=FnResult.
:- set_opv(f_u_episode_defn_goal, classof, claz_function),
   set_opv(u_episode_defn_goal, compile_as, kw_function),
   set_opv(u_episode_defn_goal, function, f_u_episode_defn_goal),
   DefunResult=u_episode_defn_goal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12515 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'episode-defn-subgoals?', [defn], [cdr, defn]]).

% annotating U::EPISODE-DEFN-SUBGOALS? 
wl: lambda_def(defun,
	      u_episode_defn_subgoals_c63,
	      f_u_episode_defn_subgoals_c63,
	      [u_defn],
	      [[cdr, u_defn]]).


% annotating U::EPISODE-DEFN-SUBGOALS? 
wl: arglist_info(u_episode_defn_subgoals_c63,
		[u_defn],
		[Defn_Param],
		arginfo{ all:[u_defn],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_defn],
			 opt:0,
			 req:[u_defn],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-DEFN-SUBGOALS? 
wl: init_args(exact_only, u_episode_defn_subgoals_c63).


% annotating U::EPISODE-DEFN-SUBGOALS? 
f_u_episode_defn_subgoals_c63(Defn_Param, FnResult) :-
	cl_cdr(Defn_Param, Cdr_Ret),
	Cdr_Ret=FnResult.
:- set_opv(f_u_episode_defn_subgoals_c63, classof, claz_function),
   set_opv(u_episode_defn_subgoals_c63, compile_as, kw_function),
   set_opv(u_episode_defn_subgoals_c63, function, f_u_episode_defn_subgoals_c63),
   DefunResult=u_episode_defn_subgoals_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12567 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'episode-defn-rule', [defn], [cadr, defn]]).

% annotating U::EPISODE-DEFN-RULE 
wl: lambda_def(defun,
	      u_episode_defn_rule,
	      f_u_episode_defn_rule,
	      [u_defn],
	      [[cadr, u_defn]]).


% annotating U::EPISODE-DEFN-RULE 
wl: arglist_info(u_episode_defn_rule,
		[u_defn],
		[Defn_Param],
		arginfo{ all:[u_defn],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_defn],
			 opt:0,
			 req:[u_defn],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-DEFN-RULE 
wl: init_args(exact_only, u_episode_defn_rule).


% annotating U::EPISODE-DEFN-RULE 
f_u_episode_defn_rule(Defn_Param, FnResult) :-
	cl_cadr(Defn_Param, Cadr_Ret),
	Cadr_Ret=FnResult.
:- set_opv(f_u_episode_defn_rule, classof, claz_function),
   set_opv(u_episode_defn_rule, compile_as, kw_function),
   set_opv(u_episode_defn_rule, function, f_u_episode_defn_rule),
   DefunResult=u_episode_defn_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12615 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'episode-defn-subgoals', [defn], [cdddr, defn]]).

% annotating U::EPISODE-DEFN-SUBGOALS 
wl: lambda_def(defun,
	      u_episode_defn_subgoals,
	      f_u_episode_defn_subgoals,
	      [u_defn],
	      [[cdddr, u_defn]]).


% annotating U::EPISODE-DEFN-SUBGOALS 
wl: arglist_info(u_episode_defn_subgoals,
		[u_defn],
		[Defn_Param],
		arginfo{ all:[u_defn],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_defn],
			 opt:0,
			 req:[u_defn],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-DEFN-SUBGOALS 
wl: init_args(exact_only, u_episode_defn_subgoals).


% annotating U::EPISODE-DEFN-SUBGOALS 
f_u_episode_defn_subgoals(Defn_Param, FnResult) :-
	cl_cdddr(Defn_Param, Cdddr_Ret),
	Cdddr_Ret=FnResult.
:- set_opv(f_u_episode_defn_subgoals, classof, claz_function),
   set_opv(u_episode_defn_subgoals, compile_as, kw_function),
   set_opv(u_episode_defn_subgoals, function, f_u_episode_defn_subgoals),
   DefunResult=u_episode_defn_subgoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12668 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'episode-defn-plan-no-gen',
			    [defn],
			    [caddr, defn]
			  ]).

% annotating U::EPISODE-DEFN-PLAN-NO-GEN 
wl: lambda_def(defun,
	      u_episode_defn_plan_no_gen,
	      f_u_episode_defn_plan_no_gen,
	      [u_defn],
	      [[caddr, u_defn]]).


% annotating U::EPISODE-DEFN-PLAN-NO-GEN 
wl: arglist_info(u_episode_defn_plan_no_gen,
		[u_defn],
		[Defn_Param],
		arginfo{ all:[u_defn],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_defn],
			 opt:0,
			 req:[u_defn],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-DEFN-PLAN-NO-GEN 
wl: init_args(exact_only, u_episode_defn_plan_no_gen).


% annotating U::EPISODE-DEFN-PLAN-NO-GEN 
f_u_episode_defn_plan_no_gen(Defn_Param, FnResult) :-
	cl_caddr(Defn_Param, Caddr_Ret),
	Caddr_Ret=FnResult.
:- set_opv(f_u_episode_defn_plan_no_gen, classof, claz_function),
   set_opv(u_episode_defn_plan_no_gen, compile_as, kw_function),
   set_opv(u_episode_defn_plan_no_gen, function, f_u_episode_defn_plan_no_gen),
   DefunResult=u_episode_defn_plan_no_gen.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12668 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If T, non top-level goals of a hand-coded episode are not accessible",
				     1,
				     12725)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12668 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" directly for planning.", 1, 12796)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12820 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*hidden-ep-subgoals?*', t]).
:- set_var(TLEnv3, setq, u_xx_hidden_ep_subgoals_c63_xx, t).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12852 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'episode-defn->stored-episode',
			    ['episode-defn', context, 'hidden?'],
			    
			    [ let,
			      
			      [ 
				[ goal,
				  
				  [ 'ob$create',
				    
				    [ '#BQ',
				      
				      [ 'SUCCEEDED-GOAL',
					obj,
					
					[ '#COMMA',
					  ['episode-defn-goal', 'episode-defn']
					]
				      ]
				    ]
				  ]
				],
				[rule, []],
				[intend, []],
				[intends, []],
				[subgoal, []],
				[subgoals, []],
				[realism, 1.0],
				['num-subgoals', []],
				[weight, []],
				
				[ 'plan-no-gen',
				  ['episode-defn-plan-no-gen', 'episode-defn']
				]
			      ],
			      
			      [ if,
				
				[ not,
				  ['episode-defn-subgoals?', 'episode-defn']
				],
				
				[ progn,
				  ['no-gen', ['cx$assert', context, goal]],
				  goal
				],
				
				[ progn,
				  
				  [ setq,
				    rule,
				    ['episode-defn-rule', 'episode-defn']
				  ],
				  
				  [ if,
				    ['eq?', rule, [quote, 'induce-rule']],
				    [setq, rule, []],
				    
				    [ progn,
				      [setq, rule, ['ob$name->ob', rule]],
				      
				      [ if,
					['null?', rule],
					
					[ error,
					  '$STRING'("Rule ~A not defined yet; (ret) to induce"),
					  ['episode-defn-rule', 'episode-defn']
					]
				      ]
				    ]
				  ],
				  [setq, realism, 0.0],
				  
				  [ setq,
				    'num-subgoals',
				    
				    [ 'fixnum->flonum',
				      
				      [ length,
					
					[ 'episode-defn-subgoals',
					  'episode-defn'
					]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    weight,
				    
				    [ if,
				      rule,
				      
				      [ 'fl/',
					['ob$get', rule, [quote, plausibility]],
					'num-subgoals'
				      ],
				      
				      [ 'fl/',
					'*auto-rule-plausibility*',
					'num-subgoals'
				      ]
				    ]
				  ],
				  
				  [ yloop,
				    [initial, [subgoalnum, 0]],
				    
				    [ yfor,
				      'subgoal-spec',
				      in,
				      ['episode-defn-subgoals', 'episode-defn']
				    ],
				    
				    [ ydo,
				      
				      [ setq,
					subgoal,
					
					[ 'episode-defn->stored-episode',
					  'subgoal-spec',
					  context,
					  
					  [ or,
					    '*hidden-ep-subgoals?*',
					    'hidden?'
					  ]
					]
				      ],
				      
				      [ 'ob$set',
					['ob$get', subgoal, [quote, obj]],
					[quote, 'plan-subgoalnum'],
					subgoalnum
				      ],
				      
				      [ setq,
					realism,
					
					[ 'fl+',
					  realism,
					  
					  [ 'fl*',
					    weight,
					    
					    [ strength,
					      ['ob$get', subgoal, [quote, obj]]
					    ]
					  ]
					]
				      ],
				      
				      [ setq,
					subgoals,
					[append, subgoals, [list, subgoal]]
				      ],
				      
				      [ if,
					rule,
					
					[ setq,
					  intend,
					  
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
					  ]
					],
					
					[ setq,
					  intend,
					  
					  [ 'ob$fcreate',
					    
					    [ '#BQ',
					      
					      [ 'INTENDS',
						'linked-from',
						['#COMMA', goal],
						'linked-to',
						['#COMMA', subgoal],
						'seq?',
						[quote, t]
					      ]
					    ]
					  ]
					]
				      ],
				      [setq, intends, [cons, intend, intends]],
				      ['no-gen', ['cx$assert', context, intend]],
				      [setq, subgoalnum, [+, 1, subgoalnum]]
				    ]
				  ],
				  ['no-gen', ['cx$assert', context, goal]],
				  
				  [ if,
				    ['null?', rule],
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("Generating rule automatically.")
				      ],
				      
				      [ setq,
					rule,
					
					[ 'plan->rule',
					  ['ob$get', goal, [quote, obj]],
					  ['subgoal-objs', subgoals],
					  '*auto-rule-plausibility*',
					  function('*episodic-rule-name-genproc*')
					]
				      ],
				      
				      [ if,
					[not, ['nil?', 'plan-no-gen']],
					
					[ 'ob$set',
					  rule,
					  [quote, 'plan-no-gen'],
					  'plan-no-gen'
					]
				      ],
				      
				      [ yloop,
					[yfor, i, in, intends],
					[ydo, ['ob$add', i, [quote, rule], rule]]
				      ]
				    ]
				  ],
				  
				  [ 'ob$set',
				    goal,
				    [quote, [obj, strength]],
				    realism
				  ],
				  
				  [ 'make-and-store-episode',
				    rule,
				    goal,
				    context,
				    realism,
				    [],
				    'hidden?',
				    ['subgoals->eps', subgoals]
				  ],
				  goal
				]
			      ]
			    ]
			  ]).

% annotating U::EPISODE-DEFN->STORED-EPISODE 
wl: lambda_def(defun,
	      u_episode_defn_c62_stored_episode,
	      f_u_episode_defn_c62_stored_episode,
	      [u_episode_defn, u_context, u_hidden_c63],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_goal,
		      
		      [ u_ob_c36_create,
			
			[ '#BQ',
			  
			  [ u_succeeded_goal,
			    u_obj,
			    ['#COMMA', [u_episode_defn_goal, u_episode_defn]]
			  ]
			]
		      ]
		    ],
		    [u_rule, []],
		    [u_intend, []],
		    [u_intends, []],
		    [u_subgoal, []],
		    [u_subgoals, []],
		    [u_realism, 1.0],
		    [u_num_subgoals, []],
		    [u_weight, []],
		    
		    [ u_plan_no_gen,
		      [u_episode_defn_plan_no_gen, u_episode_defn]
		    ]
		  ],
		  
		  [ if,
		    [not, [u_episode_defn_subgoals_c63, u_episode_defn]],
		    
		    [ progn,
		      [u_no_gen, [u_cx_c36_assert, u_context, u_goal]],
		      u_goal
		    ],
		    
		    [ progn,
		      [setq, u_rule, [u_episode_defn_rule, u_episode_defn]],
		      
		      [ if,
			[u_eq_c63, u_rule, [quote, u_induce_rule]],
			[setq, u_rule, []],
			
			[ progn,
			  [setq, u_rule, [u_ob_c36_name_c62_ob, u_rule]],
			  
			  [ if,
			    [u_null_c63, u_rule],
			    
			    [ error,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('R'),
					 #\(u),
					 #\(l),
					 #\(e),
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
					 #\(;),
					 #\(' '),
					 #\('('),
					 #\(r),
					 #\(e),
					 #\(t),
					 #\(')'),
					 #\(' '),
					 #\(t),
					 #\(o),
					 #\(' '),
					 #\(i),
					 #\(n),
					 #\(d),
					 #\(u),
					 #\(c),
					 #\(e)
				       ]),
			      [u_episode_defn_rule, u_episode_defn]
			    ]
			  ]
			]
		      ],
		      [setq, u_realism, 0.0],
		      
		      [ setq,
			u_num_subgoals,
			
			[ u_fixnum_c62_flonum,
			  [length, [u_episode_defn_subgoals, u_episode_defn]]
			]
		      ],
		      
		      [ setq,
			u_weight,
			
			[ if,
			  u_rule,
			  
			  [ u_fl_c47,
			    [u_ob_c36_get, u_rule, [quote, u_plausibility]],
			    u_num_subgoals
			  ],
			  
			  [ u_fl_c47,
			    u_xx_auto_rule_plausibility_xx,
			    u_num_subgoals
			  ]
			]
		      ],
		      
		      [ u_yloop,
			[u_initial, [u_subgoalnum, 0]],
			
			[ u_yfor,
			  u_subgoal_spec,
			  u_in,
			  [u_episode_defn_subgoals, u_episode_defn]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_subgoal,
			    
			    [ u_episode_defn_c62_stored_episode,
			      u_subgoal_spec,
			      u_context,
			      [or, u_xx_hidden_ep_subgoals_c63_xx, u_hidden_c63]
			    ]
			  ],
			  
			  [ u_ob_c36_set,
			    [u_ob_c36_get, u_subgoal, [quote, u_obj]],
			    [quote, u_plan_subgoalnum],
			    u_subgoalnum
			  ],
			  
			  [ setq,
			    u_realism,
			    
			    [ u_fl_c43,
			      u_realism,
			      
			      [ u_fl_xx,
				u_weight,
				
				[ u_strength,
				  [u_ob_c36_get, u_subgoal, [quote, u_obj]]
				]
			      ]
			    ]
			  ],
			  
			  [ setq,
			    u_subgoals,
			    [append, u_subgoals, [list, u_subgoal]]
			  ],
			  
			  [ if,
			    u_rule,
			    
			    [ setq,
			      u_intend,
			      
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
			      ]
			    ],
			    
			    [ setq,
			      u_intend,
			      
			      [ u_ob_c36_fcreate,
				
				[ '#BQ',
				  
				  [ u_intends,
				    u_linked_from,
				    ['#COMMA', u_goal],
				    u_linked_to,
				    ['#COMMA', u_subgoal],
				    u_seq_c63,
				    [quote, t]
				  ]
				]
			      ]
			    ]
			  ],
			  [setq, u_intends, [cons, u_intend, u_intends]],
			  [u_no_gen, [u_cx_c36_assert, u_context, u_intend]],
			  [setq, u_subgoalnum, [+, 1, u_subgoalnum]]
			]
		      ],
		      [u_no_gen, [u_cx_c36_assert, u_context, u_goal]],
		      
		      [ if,
			[u_null_c63, u_rule],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('G'),
				       #\(e),
				       #\(n),
				       #\(e),
				       #\(r),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(r),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(' '),
				       #\(a),
				       #\(u),
				       #\(t),
				       #\(o),
				       #\(m),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(c),
				       #\(a),
				       #\(l),
				       #\(l),
				       #\(y),
				       #\('.')
				     ])
			  ],
			  
			  [ setq,
			    u_rule,
			    
			    [ u_plan_c62_rule,
			      [u_ob_c36_get, u_goal, [quote, u_obj]],
			      [u_subgoal_objs, u_subgoals],
			      u_xx_auto_rule_plausibility_xx,
			      function(u_xx_episodic_rule_name_genproc_xx)
			    ]
			  ],
			  
			  [ if,
			    [not, [u_nil_c63, u_plan_no_gen]],
			    
			    [ u_ob_c36_set,
			      u_rule,
			      [quote, u_plan_no_gen],
			      u_plan_no_gen
			    ]
			  ],
			  
			  [ u_yloop,
			    [u_yfor, u_i, u_in, u_intends],
			    [u_ydo, [u_ob_c36_add, u_i, [quote, u_rule], u_rule]]
			  ]
			]
		      ],
		      
		      [ u_ob_c36_set,
			u_goal,
			[quote, [u_obj, u_strength]],
			u_realism
		      ],
		      
		      [ u_make_and_store_episode,
			u_rule,
			u_goal,
			u_context,
			u_realism,
			[],
			u_hidden_c63,
			[u_subgoals_c62_eps, u_subgoals]
		      ],
		      u_goal
		    ]
		  ]
		]
	      ]).


% annotating U::EPISODE-DEFN->STORED-EPISODE 
wl: arglist_info(u_episode_defn_c62_stored_episode,
		[u_episode_defn, u_context, u_hidden_c63],
		[Episode_defn_Param, Context_Param, Hidden_c63_Param],
		arginfo{ all:[u_episode_defn, u_context, u_hidden_c63],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_episode_defn, u_context, u_hidden_c63],
			 opt:0,
			 req:[u_episode_defn, u_context, u_hidden_c63],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-DEFN->STORED-EPISODE 
wl: init_args(exact_only, u_episode_defn_c62_stored_episode).


% annotating U::EPISODE-DEFN->STORED-EPISODE 
f_u_episode_defn_c62_stored_episode(Episode_defn_Param, Context_Param, Hidden_c63_Param, FnResult) :-
	Env=[bv(u_episode_defn, Episode_defn_Param), bv(u_context, Context_Param), bv(u_hidden_c63, Hidden_c63_Param)],
	f_u_ob_c36_create(
			  [ '#BQ',
			    
			    [ u_succeeded_goal,
			      u_obj,
			      ['#COMMA', [u_episode_defn_goal, u_episode_defn]]
			    ]
			  ],
			  Goal_Init),
	f_u_episode_defn_plan_no_gen(Episode_defn_Param, Plan_no_gen_Init),
	LEnv=[[bv(u_goal, Goal_Init), bv(u_rule, []), bv(u_intend, []), bv(u_intends, []), bv(u_subgoal, []), bv(u_subgoals, []), bv(u_realism, 1.0), bv(u_num_subgoals, []), bv(u_weight, []), bv(u_plan_no_gen, Plan_no_gen_Init)]|Env],
	f_u_episode_defn_subgoals_c63(Episode_defn_Param, PredArgResult),
	(   PredArgResult==[]
	->  f_u_no_gen([[u_cx_c36_assert, u_context, u_goal]], No_gen_Ret),
	    get_var(LEnv, u_goal, Goal_Get),
	    FnResult=Goal_Get
	;   f_u_episode_defn_rule(Episode_defn_Param, Rule),
	    set_var(LEnv, u_rule, Rule),
	    f_u_eq_c63(u_rule, [quote, u_induce_rule], IFTEST28),
	    (   IFTEST28\==[]
	    ->  set_var(LEnv, setq, u_rule, []),
		ElseResult=[]
	    ;   get_var(LEnv, u_rule, Rule_Get),
		f_u_ob_c36_name_c62_ob(Rule_Get, Rule67),
		set_var(LEnv, u_rule, Rule67),
		f_u_null_c63(u_rule, IFTEST31),
		(   IFTEST31\==[]
		->  f_u_episode_defn_rule(Episode_defn_Param, Defn_rule_Ret),
		    cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					
					[ #\('R'),
					  #\(u),
					  #\(l),
					  #\(e),
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
					  #\(;),
					  #\(' '),
					  #\('('),
					  #\(r),
					  #\(e),
					  #\(t),
					  #\(')'),
					  #\(' '),
					  #\(t),
					  #\(o),
					  #\(' '),
					  #\(i),
					  #\(n),
					  #\(d),
					  #\(u),
					  #\(c),
					  #\(e)
					]),
			       Defn_rule_Ret
			     ],
			     TrueResult),
		    ElseResult=TrueResult
		;   ElseResult=[]
		)
	    ),
	    set_var(LEnv, setq, u_realism, 0.0),
	    f_u_fixnum_c62_flonum(
				  [ length,
				    [u_episode_defn_subgoals, u_episode_defn]
				  ],
				  Num_subgoals),
	    set_var(LEnv, u_num_subgoals, Num_subgoals),
	    get_var(LEnv, u_rule, IFTEST36),
	    (   IFTEST36\==[]
	    ->  f_u_fl_c47([u_ob_c36_get, u_rule, [quote, u_plausibility]],
			   u_num_subgoals,
			   TrueResult39),
		Weight=TrueResult39
	    ;   f_u_fl_c47(u_xx_auto_rule_plausibility_xx,
			   u_num_subgoals,
			   ElseResult40),
		Weight=ElseResult40
	    ),
	    set_var(LEnv, u_weight, Weight),
	    f_u_yloop(
		      [ [u_initial, [u_subgoalnum, 0]],
			
			[ u_yfor,
			  u_subgoal_spec,
			  u_in,
			  [u_episode_defn_subgoals, u_episode_defn]
			],
			
			[ u_ydo,
			  
			  [ setq,
			    u_subgoal,
			    
			    [ u_episode_defn_c62_stored_episode,
			      u_subgoal_spec,
			      u_context,
			      [or, u_xx_hidden_ep_subgoals_c63_xx, u_hidden_c63]
			    ]
			  ],
			  
			  [ u_ob_c36_set,
			    [u_ob_c36_get, u_subgoal, [quote, u_obj]],
			    [quote, u_plan_subgoalnum],
			    u_subgoalnum
			  ],
			  
			  [ setq,
			    u_realism,
			    
			    [ u_fl_c43,
			      u_realism,
			      
			      [ u_fl_xx,
				u_weight,
				
				[ u_strength,
				  [u_ob_c36_get, u_subgoal, [quote, u_obj]]
				]
			      ]
			    ]
			  ],
			  
			  [ setq,
			    u_subgoals,
			    [append, u_subgoals, [list, u_subgoal]]
			  ],
			  
			  [ if,
			    u_rule,
			    
			    [ setq,
			      u_intend,
			      
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
			      ]
			    ],
			    
			    [ setq,
			      u_intend,
			      
			      [ u_ob_c36_fcreate,
				
				[ '#BQ',
				  
				  [ u_intends,
				    u_linked_from,
				    ['#COMMA', u_goal],
				    u_linked_to,
				    ['#COMMA', u_subgoal],
				    u_seq_c63,
				    [quote, t]
				  ]
				]
			      ]
			    ]
			  ],
			  [setq, u_intends, [cons, u_intend, u_intends]],
			  [u_no_gen, [u_cx_c36_assert, u_context, u_intend]],
			  [setq, u_subgoalnum, [+, 1, u_subgoalnum]]
			]
		      ],
		      Yloop_Ret),
	    f_u_no_gen([[u_cx_c36_assert, u_context, u_goal]], No_gen_Ret75),
	    f_u_null_c63(u_rule, IFTEST41),
	    (   IFTEST41\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('G'),
					       #\(e),
					       #\(n),
					       #\(e),
					       #\(r),
					       #\(a),
					       #\(t),
					       #\(i),
					       #\(n),
					       #\(g),
					       #\(' '),
					       #\(r),
					       #\(u),
					       #\(l),
					       #\(e),
					       #\(' '),
					       #\(a),
					       #\(u),
					       #\(t),
					       #\(o),
					       #\(m),
					       #\(a),
					       #\(t),
					       #\(i),
					       #\(c),
					       #\(a),
					       #\(l),
					       #\(l),
					       #\(y),
					       #\('.')
					     ])
				  ],
				  Roman_nl_Ret),
		get_var(LEnv, u_goal, Goal_Get43),
		f_u_ob_c36_get(Goal_Get43, u_obj, Obj),
		get_var(LEnv, u_subgoals, Subgoals_Get),
		f_u_subgoal_objs(Subgoals_Get, Subgoal_objs_Ret),
		get_var(LEnv,
			u_xx_auto_rule_plausibility_xx,
			Xx_auto_rule_plausibility_xx_Get),
		f_u_plan_c62_rule(Obj,
				  Subgoal_objs_Ret,
				  Xx_auto_rule_plausibility_xx_Get,
				  function(u_xx_episodic_rule_name_genproc_xx),
				  Rule71),
		set_var(LEnv, u_rule, Rule71),
		f_u_nil_c63(u_plan_no_gen, PredArgResult48),
		(   PredArgResult48==[]
		->  get_var(LEnv, u_plan_no_gen, Plan_no_gen_Get),
		    get_var(LEnv, u_rule, Rule_Get49),
		    f_u_ob_c36_set(Rule_Get49,
				   u_plan_no_gen,
				   Plan_no_gen_Get,
				   TrueResult51),
		    _61948=TrueResult51
		;   _61948=[]
		),
		f_u_yloop(
			  [ [u_yfor, u_i, u_in, u_intends],
			    [u_ydo, [u_ob_c36_add, u_i, [quote, u_rule], u_rule]]
			  ],
			  TrueResult52),
		_61708=TrueResult52
	    ;   _61708=[]
	    ),
	    get_var(LEnv, u_goal, Goal_Get53),
	    get_var(LEnv, u_realism, Realism_Get58),
	    f_u_ob_c36_set(Goal_Get53,
			   [u_obj, u_strength],
			   Realism_Get58,
			   C36_set_Ret),
	    get_var(LEnv, u_goal, Goal_Get56),
	    get_var(LEnv, u_rule, Rule_Get55),
	    get_var(LEnv, u_subgoals, Subgoals_Get60),
	    f_u_subgoals_c62_eps(Subgoals_Get60, C62_eps_Ret),
	    f_u_make_and_store_episode(Rule_Get55,
				       Goal_Get56,
				       Context_Param,
				       Realism_Get58,
				       [],
				       Hidden_c63_Param,
				       C62_eps_Ret,
				       Store_episode_Ret),
	    get_var(LEnv, u_goal, Goal_Get61),
	    FnResult=Goal_Get61
	).
:- set_opv(f_u_episode_defn_c62_stored_episode, classof, claz_function),
   set_opv(u_episode_defn_c62_stored_episode, compile_as, kw_function),
   set_opv(u_episode_defn_c62_stored_episode,
	   function,
	   f_u_episode_defn_c62_stored_episode),
   DefunResult=u_episode_defn_c62_stored_episode.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:12852 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (check-episodic-plan rule (ob$get goal 'obj) subgoals)",
				     1,
				     15861)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16124 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'subgoals->eps',
			    [subgoals],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, subgoal, in, subgoals],
			      
			      [ ydo,
				
				[ if,
				  ['ob$get', subgoal, [quote, episode]],
				  
				  [ setq,
				    result,
				    
				    [ append,
				      result,
				      
				      [ list,
					['ob$get', subgoal, [quote, episode]]
				      ]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::SUBGOALS->EPS 
wl: lambda_def(defun,
	      u_subgoals_c62_eps,
	      f_u_subgoals_c62_eps,
	      [u_subgoals],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_subgoal, u_in, u_subgoals],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_ob_c36_get, u_subgoal, [quote, u_episode]],
		      
		      [ setq,
			u_result,
			
			[ append,
			  u_result,
			  [list, [u_ob_c36_get, u_subgoal, [quote, u_episode]]]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::SUBGOALS->EPS 
wl: arglist_info(u_subgoals_c62_eps,
		[u_subgoals],
		[Subgoals_Param],
		arginfo{ all:[u_subgoals],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subgoals],
			 opt:0,
			 req:[u_subgoals],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBGOALS->EPS 
wl: init_args(exact_only, u_subgoals_c62_eps).


% annotating U::SUBGOALS->EPS 
f_u_subgoals_c62_eps(Subgoals_Param, FnResult) :-
	Env=[bv(u_subgoals, Subgoals_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_subgoal, u_in, u_subgoals],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ob_c36_get, u_subgoal, [quote, u_episode]],
			
			[ setq,
			  u_result,
			  
			  [ append,
			    u_result,
			    [list, [u_ob_c36_get, u_subgoal, [quote, u_episode]]]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_subgoals_c62_eps, classof, claz_function),
   set_opv(u_subgoals_c62_eps, compile_as, kw_function),
   set_opv(u_subgoals_c62_eps, function, f_u_subgoals_c62_eps),
   DefunResult=u_subgoals_c62_eps.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16433 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'constructed-plan?',
			    [rule],
			    ['ob$get', rule, [quote, 'constructed?']]
			  ]).

% annotating U::CONSTRUCTED-PLAN? 
wl: lambda_def(defun,
	      u_constructed_plan_c63,
	      f_u_constructed_plan_c63,
	      [u_rule],
	      [[u_ob_c36_get, u_rule, [quote, u_constructed_c63]]]).


% annotating U::CONSTRUCTED-PLAN? 
wl: arglist_info(u_constructed_plan_c63,
		[u_rule],
		[Rule_Param],
		arginfo{ all:[u_rule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule],
			 opt:0,
			 req:[u_rule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CONSTRUCTED-PLAN? 
wl: init_args(exact_only, u_constructed_plan_c63).


% annotating U::CONSTRUCTED-PLAN? 
f_u_constructed_plan_c63(Rule_Param, FnResult) :-
	f_u_ob_c36_get(Rule_Param, u_constructed_c63, Constructed_c63),
	Constructed_c63=FnResult.
:- set_opv(f_u_constructed_plan_c63, classof, claz_function),
   set_opv(u_constructed_plan_c63, compile_as, kw_function),
   set_opv(u_constructed_plan_c63, function, f_u_constructed_plan_c63),
   DefunResult=u_constructed_plan_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16497 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*next-erule-number*', 1]).
:- set_var(TLEnv3, setq, u_xx_next_erule_number_xx, 1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16527 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    '*episodic-rule-name-genproc*',
			    [],
			    
			    [ 'string->symbol',
			      
			      [ 'string-append',
				'$STRING'("EPISODIC-RULE."),
				
				[ prog1,
				  ['fixnum->string', '*next-erule-number*'],
				  ['increment-me', '*next-erule-number*']
				]
			      ]
			    ]
			  ]).

% annotating U::*EPISODIC-RULE-NAME-GENPROC* 
wl: lambda_def(defun,
	      u_xx_episodic_rule_name_genproc_xx,
	      f_u_xx_episodic_rule_name_genproc_xx,
	      [],
	      
	      [ 
		[ u_string_c62_symbol,
		  
		  [ u_string_append,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('E'),
			       #\('P'),
			       #\('I'),
			       #\('S'),
			       #\('O'),
			       #\('D'),
			       #\('I'),
			       #\('C'),
			       #\(-),
			       #\('R'),
			       #\('U'),
			       #\('L'),
			       #\('E'),
			       #\('.')
			     ]),
		    
		    [ prog1,
		      [u_fixnum_c62_string, u_xx_next_erule_number_xx],
		      [u_increment_me, u_xx_next_erule_number_xx]
		    ]
		  ]
		]
	      ]).


% annotating U::*EPISODIC-RULE-NAME-GENPROC* 
wl: arglist_info(u_xx_episodic_rule_name_genproc_xx,
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

% annotating U::*EPISODIC-RULE-NAME-GENPROC* 
wl: init_args(exact_only, u_xx_episodic_rule_name_genproc_xx).


% annotating U::*EPISODIC-RULE-NAME-GENPROC* 
f_u_xx_episodic_rule_name_genproc_xx(FnResult) :-
	Env=[],
	f_u_string_c62_symbol(
			      [ u_string_append,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('E'),
					   #\('P'),
					   #\('I'),
					   #\('S'),
					   #\('O'),
					   #\('D'),
					   #\('I'),
					   #\('C'),
					   #\(-),
					   #\('R'),
					   #\('U'),
					   #\('L'),
					   #\('E'),
					   #\('.')
					 ]),
				
				[ prog1,
				  
				  [ u_fixnum_c62_string,
				    u_xx_next_erule_number_xx
				  ],
				  [u_increment_me, u_xx_next_erule_number_xx]
				]
			      ],
			      C62_symbol_Ret),
	C62_symbol_Ret=FnResult.
:- set_opv(f_u_xx_episodic_rule_name_genproc_xx, classof, claz_function),
   set_opv(u_xx_episodic_rule_name_genproc_xx, compile_as, kw_function),
   set_opv(u_xx_episodic_rule_name_genproc_xx,
	   function,
	   f_u_xx_episodic_rule_name_genproc_xx),
   DefunResult=u_xx_episodic_rule_name_genproc_xx.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:16747 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'plan->rule',
			    [goal, subgoals, plausibility, 'name-gen-proc'],
			    
			    [ let,
			      [['concrete-rule', []], ['abstract-rule', []]],
			      
			      [ if,
				[cdr, subgoals],
				
				[ setq,
				  'concrete-rule',
				  
				  [ 'ob$create',
				    
				    [ '#BQ',
				      
				      [ 'RULE',
					subgoal,
					['RSEQ', ['#BQ-COMMA-ELIPSE', subgoals]],
					goal,
					['#COMMA', goal],
					plausibility,
					['#COMMA', plausibility],
					(is),
					[quote, 'plan-only']
				      ]
				    ]
				  ]
				],
				
				[ setq,
				  'concrete-rule',
				  
				  [ 'ob$create',
				    
				    [ '#BQ',
				      
				      [ 'RULE',
					subgoal,
					['#COMMA', [car, subgoals]],
					goal,
					['#COMMA', goal],
					plausibility,
					['#COMMA', plausibility],
					(is),
					[quote, 'plan-only']
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				'abstract-rule',
				
				[ 'ob$variabilize',
				  'concrete-rule',
				  function('varize-object?'),
				  [],
				  '*link-slots*',
				  []
				]
			      ],
			      
			      [ 'ob$add-unique-name',
				'abstract-rule',
				[funcall, 'name-gen-proc']
			      ],
			      
			      [ 'ob$set',
				'abstract-rule',
				[quote, 'constructed?'],
				t
			      ],
			      ['add-rule-print', 'abstract-rule'],
			      'abstract-rule'
			    ]
			  ]).

% annotating U::PLAN->RULE 
wl: lambda_def(defun,
	      u_plan_c62_rule,
	      f_u_plan_c62_rule,
	      [u_goal, u_subgoals, u_plausibility, u_name_gen_proc],
	      
	      [ 
		[ let,
		  [[u_concrete_rule, []], [u_abstract_rule, []]],
		  
		  [ if,
		    [cdr, u_subgoals],
		    
		    [ setq,
		      u_concrete_rule,
		      
		      [ u_ob_c36_create,
			
			[ '#BQ',
			  
			  [ u_rule,
			    u_subgoal,
			    [u_rseq, ['#BQ-COMMA-ELIPSE', u_subgoals]],
			    u_goal,
			    ['#COMMA', u_goal],
			    u_plausibility,
			    ['#COMMA', u_plausibility],
			    u_is,
			    [quote, u_plan_only]
			  ]
			]
		      ]
		    ],
		    
		    [ setq,
		      u_concrete_rule,
		      
		      [ u_ob_c36_create,
			
			[ '#BQ',
			  
			  [ u_rule,
			    u_subgoal,
			    ['#COMMA', [car, u_subgoals]],
			    u_goal,
			    ['#COMMA', u_goal],
			    u_plausibility,
			    ['#COMMA', u_plausibility],
			    u_is,
			    [quote, u_plan_only]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ setq,
		    u_abstract_rule,
		    
		    [ u_ob_c36_variabilize,
		      u_concrete_rule,
		      function(u_varize_object_c63),
		      [],
		      u_xx_link_slots_xx,
		      []
		    ]
		  ],
		  
		  [ u_ob_c36_add_unique_name,
		    u_abstract_rule,
		    [funcall, u_name_gen_proc]
		  ],
		  [u_ob_c36_set, u_abstract_rule, [quote, u_constructed_c63], t],
		  [u_add_rule_print, u_abstract_rule],
		  u_abstract_rule
		]
	      ]).


% annotating U::PLAN->RULE 
wl: arglist_info(u_plan_c62_rule,
		[u_goal, u_subgoals, u_plausibility, u_name_gen_proc],
		
		[ Goal_Param,
		  Subgoals_Param,
		  Plausibility_Param,
		  Name_gen_proc_Param
		],
		arginfo{ all:
			     [ u_goal,
			       u_subgoals,
			       u_plausibility,
			       u_name_gen_proc
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 u_subgoals,
				 u_plausibility,
				 u_name_gen_proc
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       u_subgoals,
			       u_plausibility,
			       u_name_gen_proc
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PLAN->RULE 
wl: init_args(exact_only, u_plan_c62_rule).


% annotating U::PLAN->RULE 
f_u_plan_c62_rule(Goal_Param, Subgoals_Param, Plausibility_Param, Name_gen_proc_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_subgoals, Subgoals_Param), bv(u_plausibility, Plausibility_Param), bv(u_name_gen_proc, Name_gen_proc_Param)],
	LEnv=[[bv(u_concrete_rule, []), bv(u_abstract_rule, [])]|Env],
	cl_cdr(Subgoals_Param, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_create(
			      [ '#BQ',
				
				[ u_rule,
				  u_subgoal,
				  [u_rseq, ['#BQ-COMMA-ELIPSE', u_subgoals]],
				  u_goal,
				  ['#COMMA', u_goal],
				  u_plausibility,
				  ['#COMMA', u_plausibility],
				  u_is,
				  [quote, u_plan_only]
				]
			      ],
			      TrueResult),
	    set_var(LEnv, u_concrete_rule, TrueResult),
	    _54844=TrueResult
	;   f_u_ob_c36_create(
			      [ '#BQ',
				
				[ u_rule,
				  u_subgoal,
				  ['#COMMA', [car, u_subgoals]],
				  u_goal,
				  ['#COMMA', u_goal],
				  u_plausibility,
				  ['#COMMA', u_plausibility],
				  u_is,
				  [quote, u_plan_only]
				]
			      ],
			      ElseResult),
	    set_var(LEnv, u_concrete_rule, ElseResult),
	    _54844=ElseResult
	),
	get_var(LEnv, u_concrete_rule, Concrete_rule_Get),
	get_var(LEnv, u_xx_link_slots_xx, Xx_link_slots_xx_Get),
	f_u_ob_c36_variabilize(Concrete_rule_Get,
			       function(u_varize_object_c63),
			       [],
			       Xx_link_slots_xx_Get,
			       [],
			       Abstract_rule),
	set_var(LEnv, u_abstract_rule, Abstract_rule),
	get_var(LEnv, u_abstract_rule, Abstract_rule_Get29),
	f_u_name_gen_proc(Gen_proc_Ret),
	f_u_ob_c36_add_unique_name(Abstract_rule_Get29,
				   Gen_proc_Ret,
				   Unique_name_Ret),
	f_u_ob_c36_set(Abstract_rule_Get29, u_constructed_c63, t, T),
	f_u_add_rule_print(Abstract_rule_Get29, Rule_print_Ret),
	LetResult=Abstract_rule_Get29,
	LetResult=FnResult.
:- set_opv(f_u_plan_c62_rule, classof, claz_function),
   set_opv(u_plan_c62_rule, compile_as, kw_function),
   set_opv(u_plan_c62_rule, function, f_u_plan_c62_rule),
   DefunResult=u_plan_c62_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:17639 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'varize-object?',
			    [x],
			    
			    [ and,
			      ['ob?', x],
			      [not, ['vars-in?', x]],
			      
			      [ or,
				
				[ and,
				  ['ty$instance?', x, [quote, object]],
				  
				  [ not,
				    ['ty$instance?', x, [quote, 'no-varize-obj']]
				  ]
				],
				['ty$instance?', x, [quote, city]],
				['ty$instance?', x, [quote, location]]
			      ]
			    ]
			  ]).

% annotating U::VARIZE-OBJECT? 
wl: lambda_def(defun,
	      u_varize_object_c63,
	      f_u_varize_object_c63,
	      [u_x],
	      
	      [ 
		[ and,
		  [u_ob_c63, u_x],
		  [not, [u_vars_in_c63, u_x]],
		  
		  [ or,
		    
		    [ and,
		      [u_ty_c36_instance_c63, u_x, [quote, u_object]],
		      
		      [ not,
			[u_ty_c36_instance_c63, u_x, [quote, u_no_varize_obj]]
		      ]
		    ],
		    [u_ty_c36_instance_c63, u_x, [quote, u_city]],
		    [u_ty_c36_instance_c63, u_x, [quote, u_location]]
		  ]
		]
	      ]).


% annotating U::VARIZE-OBJECT? 
wl: arglist_info(u_varize_object_c63,
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

% annotating U::VARIZE-OBJECT? 
wl: init_args(exact_only, u_varize_object_c63).


% annotating U::VARIZE-OBJECT? 
f_u_varize_object_c63(X_Param, TrueResult27) :-
	Env=[bv(u_x, X_Param)],
	f_u_ob_c63(u_x, IFTEST),
	(   IFTEST\==[]
	->  f_u_vars_in_c63(X_Param, PredArgResult),
	    (   PredArgResult==[]
	    ->  (   f_u_ty_c36_instance_c63(X_Param, u_object, IFTEST18),
		    (   IFTEST18\==[]
		    ->  f_u_ty_c36_instance_c63(X_Param,
						u_no_varize_obj,
						No_varize_obj),
			cl_not(No_varize_obj, TrueResult),
			FORM1_Res26=TrueResult
		    ;   FORM1_Res26=[]
		    ),
		    FORM1_Res26\==[],
		    TrueResult27=FORM1_Res26
		->  true
		;   f_u_ty_c36_instance_c63(X_Param, u_city, FORM1_Res),
		    FORM1_Res\==[],
		    TrueResult27=FORM1_Res
		->  true
		;   f_u_ty_c36_instance_c63(X_Param, u_location, Location),
		    TrueResult27=Location
		)
	    ;   TrueResult27=[]
	    )
	;   TrueResult27=[]
	).
:- set_opv(f_u_varize_object_c63, classof, claz_function),
   set_opv(u_varize_object_c63, compile_as, kw_function),
   set_opv(u_varize_object_c63, function, f_u_varize_object_c63),
   DefunResult=u_varize_object_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:17878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'subgoal-objs',
			    [subgoals],
			    
			    [ map,
			      [quote, list],
			      [lambda, [x], ['ob$get', x, [quote, obj]]],
			      subgoals
			    ]
			  ]).

% annotating U::SUBGOAL-OBJS 
wl: lambda_def(defun,
	      u_subgoal_objs,
	      f_u_subgoal_objs,
	      [u_subgoals],
	      
	      [ 
		[ map,
		  [quote, list],
		  [lambda, [u_x], [u_ob_c36_get, u_x, [quote, u_obj]]],
		  u_subgoals
		]
	      ]).


% annotating U::SUBGOAL-OBJS 
wl: arglist_info(u_subgoal_objs,
		[u_subgoals],
		[Subgoals_Param],
		arginfo{ all:[u_subgoals],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subgoals],
			 opt:0,
			 req:[u_subgoals],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBGOAL-OBJS 
wl: init_args(exact_only, u_subgoal_objs).


% annotating U::SUBGOAL-OBJS 
f_u_subgoal_objs(Subgoals_Param, FnResult) :-
	Env=[bv(u_subgoals, Subgoals_Param)],
	Lambda=closure([Env13|Env], LResult, [u_x],  (get_var(Env13, u_x, X_Get), f_u_ob_c36_get(X_Get, u_obj, LResult))),
	cl_map(list, Lambda, Subgoals_Param, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_subgoal_objs, classof, claz_function),
   set_opv(u_subgoal_objs, compile_as, kw_function),
   set_opv(u_subgoal_objs, function, f_u_subgoal_objs),
   DefunResult=u_subgoal_objs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:17878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" We don't count goals that terminated before the beginning of this",
				     1,
				     17964)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:17878 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" top-level goal's 'scenario'.", 1, 18032)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'scenario-desirability',
			    [context, 'top-level-goal'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      desire,
			      '$STRING'("Assess scenario desirability in ~A"),
			      context
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				[result, 0.0],
				
				[ 'pre-scenario-contexts',
				  
				  [ 'cx$ancestors',
				    
				    [ 'ob$get',
				      'top-level-goal',
				      [quote, 'activation-context']
				    ]
				  ]
				]
			      ],
			      
			      [ yfor,
				ob,
				in,
				
				[ append,
				  ['cx$get-all-ty', context, '*failed-goal-ob*'],
				  
				  [ 'cx$get-all-ty',
				    context,
				    '*succeeded-goal-ob*'
				  ]
				]
			      ],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    ['personal-goal?', ob],
				    
				    [ not,
				      
				      [ 'memq?',
					
					[ 'ob$get',
					  ob,
					  [quote, 'termination-context']
					],
					'pre-scenario-contexts'
				      ]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      desire,
				      '$STRING'("~A (~A)"),
				      ob,
				      [strength, ob]
				    ],
				    
				    [ cond,
				      
				      [ 
					[ 'ty$instance?',
					  ob,
					  [quote, 'succeeded-goal']
					],
					
					[ setq,
					  result,
					  ['fl+', result, [strength, ob]]
					]
				      ],
				      
				      [ 
					[ 'ty$instance?',
					  ob,
					  [quote, 'active-goal']
					],
					
					[ setq,
					  result,
					  ['fl-', result, [strength, ob]]
					]
				      ],
				      
				      [ ['ty$instance?', ob, [quote, 'p-goal']],
					
					[ setq,
					  result,
					  ['fl-', result, [strength, ob]]
					]
				      ],
				      
				      [ 
					[ 'ty$instance?',
					  ob,
					  [quote, 'failed-goal']
					],
					
					[ setq,
					  result,
					  ['fl-', result, [strength, ob]]
					]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ yresult,
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Scenario desirability = ~A"),
				    result
				  ],
				  result
				]
			      ]
			    ]
			  ]).

% annotating U::SCENARIO-DESIRABILITY 
wl: lambda_def(defun,
	      u_scenario_desirability,
	      f_u_scenario_desirability,
	      [u_context, u_top_level_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_desire,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(s),
			     #\(s),
			     #\(e),
			     #\(s),
			     #\(s),
			     #\(' '),
			     #\(s),
			     #\(c),
			     #\(e),
			     #\(n),
			     #\(a),
			     #\(r),
			     #\(i),
			     #\(o),
			     #\(' '),
			     #\(d),
			     #\(e),
			     #\(s),
			     #\(i),
			     #\(r),
			     #\(a),
			     #\(b),
			     #\(i),
			     #\(l),
			     #\(i),
			     #\(t),
			     #\(y),
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
		  
		  [ u_initial,
		    [u_result, 0.0],
		    
		    [ u_pre_scenario_contexts,
		      
		      [ u_cx_c36_ancestors,
			
			[ u_ob_c36_get,
			  u_top_level_goal,
			  [quote, u_activation_context]
			]
		      ]
		    ]
		  ],
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    
		    [ append,
		      [u_cx_c36_get_all_ty, u_context, u_xx_failed_goal_ob_xx],
		      
		      [ u_cx_c36_get_all_ty,
			u_context,
			u_xx_succeeded_goal_ob_xx
		      ]
		    ]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			[u_personal_goal_c63, u_ob],
			
			[ not,
			  
			  [ u_memq_c63,
			    [u_ob_c36_get, u_ob, [quote, u_termination_context]],
			    u_pre_scenario_contexts
			  ]
			]
		      ],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_desire,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(~),
				     #\('A'),
				     #\(' '),
				     #\('('),
				     #\(~),
				     #\('A'),
				     #\(')')
				   ]),
			  u_ob,
			  [u_strength, u_ob]
			],
			
			[ cond,
			  
			  [ 
			    [ u_ty_c36_instance_c63,
			      u_ob,
			      [quote, u_succeeded_goal]
			    ],
			    
			    [ setq,
			      u_result,
			      [u_fl_c43, u_result, [u_strength, u_ob]]
			    ]
			  ],
			  
			  [ [u_ty_c36_instance_c63, u_ob, [quote, u_active_goal]],
			    
			    [ setq,
			      u_result,
			      [u_flc45, u_result, [u_strength, u_ob]]
			    ]
			  ],
			  
			  [ [u_ty_c36_instance_c63, u_ob, [quote, u_p_goal]],
			    
			    [ setq,
			      u_result,
			      [u_flc45, u_result, [u_strength, u_ob]]
			    ]
			  ],
			  
			  [ [u_ty_c36_instance_c63, u_ob, [quote, u_failed_goal]],
			    
			    [ setq,
			      u_result,
			      [u_flc45, u_result, [u_strength, u_ob]]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('S'),
				   #\(c),
				   #\(e),
				   #\(n),
				   #\(a),
				   #\(r),
				   #\(i),
				   #\(o),
				   #\(' '),
				   #\(d),
				   #\(e),
				   #\(s),
				   #\(i),
				   #\(r),
				   #\(a),
				   #\(b),
				   #\(i),
				   #\(l),
				   #\(i),
				   #\(t),
				   #\(y),
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
		]
	      ]).


% annotating U::SCENARIO-DESIRABILITY 
wl: arglist_info(u_scenario_desirability,
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

% annotating U::SCENARIO-DESIRABILITY 
wl: init_args(exact_only, u_scenario_desirability).


% annotating U::SCENARIO-DESIRABILITY 
f_u_scenario_desirability(Context_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_desire,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(s),
				       #\(s),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(' '),
				       #\(s),
				       #\(c),
				       #\(e),
				       #\(n),
				       #\(a),
				       #\(r),
				       #\(i),
				       #\(o),
				       #\(' '),
				       #\(d),
				       #\(e),
				       #\(s),
				       #\(i),
				       #\(r),
				       #\(a),
				       #\(b),
				       #\(i),
				       #\(l),
				       #\(i),
				       #\(t),
				       #\(y),
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
		  [ 
		    [ u_initial,
		      [u_result, 0.0],
		      
		      [ u_pre_scenario_contexts,
			
			[ u_cx_c36_ancestors,
			  
			  [ u_ob_c36_get,
			    u_top_level_goal,
			    [quote, u_activation_context]
			  ]
			]
		      ]
		    ],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ append,
			[u_cx_c36_get_all_ty, u_context, u_xx_failed_goal_ob_xx],
			
			[ u_cx_c36_get_all_ty,
			  u_context,
			  u_xx_succeeded_goal_ob_xx
			]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_personal_goal_c63, u_ob],
			  
			  [ not,
			    
			    [ u_memq_c63,
			      
			      [ u_ob_c36_get,
				u_ob,
				[quote, u_termination_context]
			      ],
			      u_pre_scenario_contexts
			    ]
			  ]
			],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_desire,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('A'),
				       #\(' '),
				       #\('('),
				       #\(~),
				       #\('A'),
				       #\(')')
				     ]),
			    u_ob,
			    [u_strength, u_ob]
			  ],
			  
			  [ cond,
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_succeeded_goal]
			      ],
			      
			      [ setq,
				u_result,
				[u_fl_c43, u_result, [u_strength, u_ob]]
			      ]
			    ],
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_active_goal]
			      ],
			      
			      [ setq,
				u_result,
				[u_flc45, u_result, [u_strength, u_ob]]
			      ]
			    ],
			    
			    [ [u_ty_c36_instance_c63, u_ob, [quote, u_p_goal]],
			      
			      [ setq,
				u_result,
				[u_flc45, u_result, [u_strength, u_ob]]
			      ]
			    ],
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_failed_goal]
			      ],
			      
			      [ setq,
				u_result,
				[u_flc45, u_result, [u_strength, u_ob]]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('S'),
				     #\(c),
				     #\(e),
				     #\(n),
				     #\(a),
				     #\(r),
				     #\(i),
				     #\(o),
				     #\(' '),
				     #\(d),
				     #\(e),
				     #\(s),
				     #\(i),
				     #\(r),
				     #\(a),
				     #\(b),
				     #\(i),
				     #\(l),
				     #\(i),
				     #\(t),
				     #\(y),
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
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_scenario_desirability, classof, claz_function),
   set_opv(u_scenario_desirability, compile_as, kw_function),
   set_opv(u_scenario_desirability, function, f_u_scenario_desirability),
   DefunResult=u_scenario_desirability.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19371)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This is called upon top-level goal failure or success.",
				     1,
				     19373)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19430)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: don't store episode if derived from another episode by",
				     1,
				     19432)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" analogy without repairs.", 1, 19495)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19522)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: use situation assumptions as indices?",
				     1,
				     19524)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:18062 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19570)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'episode-store-top-goal',
			    ['top-level-goal', context],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Store episode")
			    ],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'(" ~A in ~A"),
			      'top-level-goal',
			      context
			    ],
			    ['ndbg-newline', '*gate-dbg*', rule],
			    
			    [ 'no-gen',
			      
			      [ let,
				
				[ 
				  [ desirability,
				    
				    [ 'scenario-desirability',
				      context,
				      'top-level-goal'
				    ]
				  ],
				  [ep, []],
				  
				  [ 'result-emot',
				    
				    [ 'goal->result-emotion',
				      'top-level-goal',
				      '*reality*'
				    ]
				  ]
				],
				
				[ if,
				  
				  [ 'ty$instance?',
				    ['ob$get', 'top-level-goal', [quote, obj]],
				    [quote, skipindex]
				  ],
				  
				  [ setq,
				    ep,
				    
				    [ 'episode-store1',
				      
				      [ car,
					
					[ 'goal-subgoals',
					  'top-level-goal',
					  context,
					  '*me-belief-path*'
					]
				      ],
				      context,
				      desirability
				    ]
				  ],
				  
				  [ setq,
				    ep,
				    
				    [ 'episode-store1',
				      'top-level-goal',
				      context,
				      desirability
				    ]
				  ]
				],
				
				[ if,
				  ep,
				  
				  [ progn,
				    
				    [ if,
				      'result-emot',
				      ['epmem-store', ep, 'result-emot', [], []]
				    ],
				    
				    [ yloop,
				      
				      [ yfor,
					index,
					in,
					
					[ 'find-misc-indices',
					  context,
					  'top-level-goal'
					]
				      ],
				      [ydo, ['epmem-store', ep, index, [], []]]
				    ],
				    ['epmem-reminding', ep, t, t],
				    ep
				  ],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::EPISODE-STORE-TOP-GOAL 
wl: lambda_def(defun,
	      u_episode_store_top_goal,
	      f_u_episode_store_top_goal,
	      [u_top_level_goal, u_context],
	      
	      [ 
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('S'),
			     #\(t),
			     #\(o),
			     #\(r),
			     #\(e),
			     #\(' '),
			     #\(e),
			     #\(p),
			     #\(i),
			     #\(s),
			     #\(o),
			     #\(d),
			     #\(e)
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
		  u_top_level_goal,
		  u_context
		],
		[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		
		[ u_no_gen,
		  
		  [ let,
		    
		    [ 
		      [ u_desirability,
			[u_scenario_desirability, u_context, u_top_level_goal]
		      ],
		      [u_ep, []],
		      
		      [ u_result_emot,
			
			[ u_goal_c62_result_emotion,
			  u_top_level_goal,
			  u_xx_reality_xx
			]
		      ]
		    ],
		    
		    [ if,
		      
		      [ u_ty_c36_instance_c63,
			[u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
			[quote, u_skipindex]
		      ],
		      
		      [ setq,
			u_ep,
			
			[ u_episode_store1,
			  
			  [ car,
			    
			    [ u_goal_subgoals,
			      u_top_level_goal,
			      u_context,
			      u_xx_me_belief_path_xx
			    ]
			  ],
			  u_context,
			  u_desirability
			]
		      ],
		      
		      [ setq,
			u_ep,
			
			[ u_episode_store1,
			  u_top_level_goal,
			  u_context,
			  u_desirability
			]
		      ]
		    ],
		    
		    [ if,
		      u_ep,
		      
		      [ progn,
			
			[ if,
			  u_result_emot,
			  [u_epmem_store, u_ep, u_result_emot, [], []]
			],
			
			[ u_yloop,
			  
			  [ u_yfor,
			    index,
			    u_in,
			    [u_find_misc_indices, u_context, u_top_level_goal]
			  ],
			  [u_ydo, [u_epmem_store, u_ep, index, [], []]]
			],
			[u_epmem_reminding, u_ep, t, t],
			u_ep
		      ],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::EPISODE-STORE-TOP-GOAL 
wl: arglist_info(u_episode_store_top_goal,
		[u_top_level_goal, u_context],
		[Top_level_goal_Param, Context_Param],
		arginfo{ all:[u_top_level_goal, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_context],
			 opt:0,
			 req:[u_top_level_goal, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-STORE-TOP-GOAL 
wl: init_args(exact_only, u_episode_store_top_goal).


% annotating U::EPISODE-STORE-TOP-GOAL 
f_u_episode_store_top_goal(Top_level_goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param)],
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\('S'),
				    #\(t),
				    #\(o),
				    #\(r),
				    #\(e),
				    #\(' '),
				    #\(e),
				    #\(p),
				    #\(i),
				    #\(s),
				    #\(o),
				    #\(d),
				    #\(e)
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
			 u_top_level_goal,
			 u_context
		       ],
		       Ndbg_roman_Ret18),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	f_u_no_gen(
		   [ 
		     [ let,
		       
		       [ 
			 [ u_desirability,
			   
			   [ u_scenario_desirability,
			     u_context,
			     u_top_level_goal
			   ]
			 ],
			 [u_ep, []],
			 
			 [ u_result_emot,
			   
			   [ u_goal_c62_result_emotion,
			     u_top_level_goal,
			     u_xx_reality_xx
			   ]
			 ]
		       ],
		       
		       [ if,
			 
			 [ u_ty_c36_instance_c63,
			   [u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
			   [quote, u_skipindex]
			 ],
			 
			 [ setq,
			   u_ep,
			   
			   [ u_episode_store1,
			     
			     [ car,
			       
			       [ u_goal_subgoals,
				 u_top_level_goal,
				 u_context,
				 u_xx_me_belief_path_xx
			       ]
			     ],
			     u_context,
			     u_desirability
			   ]
			 ],
			 
			 [ setq,
			   u_ep,
			   
			   [ u_episode_store1,
			     u_top_level_goal,
			     u_context,
			     u_desirability
			   ]
			 ]
		       ],
		       
		       [ if,
			 u_ep,
			 
			 [ progn,
			   
			   [ if,
			     u_result_emot,
			     [u_epmem_store, u_ep, u_result_emot, [], []]
			   ],
			   
			   [ u_yloop,
			     
			     [ u_yfor,
			       index,
			       u_in,
			       
			       [ u_find_misc_indices,
				 u_context,
				 u_top_level_goal
			       ]
			     ],
			     [u_ydo, [u_epmem_store, u_ep, index, [], []]]
			   ],
			   [u_epmem_reminding, u_ep, t, t],
			   u_ep
			 ],
			 []
		       ]
		     ]
		   ],
		   No_gen_Ret),
	No_gen_Ret=FnResult.
:- set_opv(f_u_episode_store_top_goal, classof, claz_function),
   set_opv(u_episode_store_top_goal, compile_as, kw_function),
   set_opv(u_episode_store_top_goal, function, f_u_episode_store_top_goal),
   DefunResult=u_episode_store_top_goal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: index under causing state (personal goal)",
				     13,
				     20403)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       or causing emotion (daydreaming goal).",
				     13,
				     20465)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" In below, indices are needed neither for plan nor for",
				     13,
				     20524)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" reminding. But wouldn't we like to make it require, say,",
				     13,
				     20592)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" half the misc indices for a reminding?",
				     13,
				     20663)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Do housekeeping for a recent episode (but without the",
				     13,
				     20851)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" reminding hoopla; also no serendipities since those",
				     13,
				     20919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:19571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" are run whenever a new rule is induced).",
				     13,
				     20985)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21100 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal->result-emotion',
			    [goal, context],
			    
			    [ let,
			      
			      [ 
				[ result,
				  
				  [ prune,
				    
				    [ 'ol-get',
				      goal,
				      '*dependency-ob*',
				      [quote, forward],
				      context
				    ],
				    
				    [ lambda,
				      [x],
				      ['ty$instance?', x, [quote, emotion]]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				result,
				
				[ progn,
				  
				  [ if,
				    [cdr, result],
				    
				    [ error,
				      '$STRING'("More than one result emotion for ~A?"),
				      goal
				    ]
				  ],
				  [car, result]
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::GOAL->RESULT-EMOTION 
wl: lambda_def(defun,
	      u_goal_c62_result_emotion,
	      f_u_goal_c62_result_emotion,
	      [u_goal, u_context],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_result,
		      
		      [ u_prune,
			
			[ u_ol_get,
			  u_goal,
			  u_xx_dependency_ob_xx,
			  [quote, u_forward],
			  u_context
			],
			
			[ lambda,
			  [u_x],
			  [u_ty_c36_instance_c63, u_x, [quote, u_emotion]]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_result,
		    
		    [ progn,
		      
		      [ if,
			[cdr, u_result],
			
			[ error,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('M'),
				     #\(o),
				     #\(r),
				     #\(e),
				     #\(' '),
				     #\(t),
				     #\(h),
				     #\(a),
				     #\(n),
				     #\(' '),
				     #\(o),
				     #\(n),
				     #\(e),
				     #\(' '),
				     #\(r),
				     #\(e),
				     #\(s),
				     #\(u),
				     #\(l),
				     #\(t),
				     #\(' '),
				     #\(e),
				     #\(m),
				     #\(o),
				     #\(t),
				     #\(i),
				     #\(o),
				     #\(n),
				     #\(' '),
				     #\(f),
				     #\(o),
				     #\(r),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(?)
				   ]),
			  u_goal
			]
		      ],
		      [car, u_result]
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::GOAL->RESULT-EMOTION 
wl: arglist_info(u_goal_c62_result_emotion,
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

% annotating U::GOAL->RESULT-EMOTION 
wl: init_args(exact_only, u_goal_c62_result_emotion).


% annotating U::GOAL->RESULT-EMOTION 
f_u_goal_c62_result_emotion(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	f_u_ol_get(Goal_Param,
		   Xx_dependency_ob_xx_Get,
		   u_forward,
		   Context_Param,
		   Prune_Param),
	Lambda=closure([Env20|Env], LResult, [u_x],  (get_var(Env20, u_x, X_Get), f_u_ty_c36_instance_c63(X_Get, u_emotion, LResult))),
	f_u_prune(Prune_Param, Lambda, Result_Init),
	LEnv=[[bv(u_result, Result_Init)]|Env],
	get_var(LEnv, u_result, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_result, Result_Get31),
	    cl_cdr(Result_Get31, IFTEST29),
	    (   IFTEST29\==[]
	    ->  cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\('M'),
				      #\(o),
				      #\(r),
				      #\(e),
				      #\(' '),
				      #\(t),
				      #\(h),
				      #\(a),
				      #\(n),
				      #\(' '),
				      #\(o),
				      #\(n),
				      #\(e),
				      #\(' '),
				      #\(r),
				      #\(e),
				      #\(s),
				      #\(u),
				      #\(l),
				      #\(t),
				      #\(' '),
				      #\(e),
				      #\(m),
				      #\(o),
				      #\(t),
				      #\(i),
				      #\(o),
				      #\(n),
				      #\(' '),
				      #\(f),
				      #\(o),
				      #\(r),
				      #\(' '),
				      #\(~),
				      #\('A'),
				      #\(?)
				    ]),
			   Goal_Param
			 ],
			 TrueResult),
		_55442=TrueResult
	    ;   _55442=[]
	    ),
	    get_var(LEnv, u_result, Result_Get34),
	    cl_car(Result_Get34, TrueResult35),
	    FnResult=TrueResult35
	;   FnResult=[]
	).
:- set_opv(f_u_goal_c62_result_emotion, classof, claz_function),
   set_opv(u_goal_c62_result_emotion, compile_as, kw_function),
   set_opv(u_goal_c62_result_emotion, function, f_u_goal_c62_result_emotion),
   DefunResult=u_goal_c62_result_emotion.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21100 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Checks to be removed once they seem to hold.",
				     13,
				     21330)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21100 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (if (any? (lambda (x) (not (ty$instance? x 'emotion)))",
				     1,
				     21475)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21100 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      result)", 1, 21543)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21100 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (error \"Not all of ~A are emotions!!\" result))",
				     1,
				     21574)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21682 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal->motiv-emotion',
			    [goal, context],
			    ['ob$get', goal, [quote, 'main-motiv']]
			  ]).

% annotating U::GOAL->MOTIV-EMOTION 
wl: lambda_def(defun,
	      u_goal_c62_motiv_emotion,
	      f_u_goal_c62_motiv_emotion,
	      [u_goal, u_context],
	      [[u_ob_c36_get, u_goal, [quote, u_main_motiv]]]).


% annotating U::GOAL->MOTIV-EMOTION 
wl: arglist_info(u_goal_c62_motiv_emotion,
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

% annotating U::GOAL->MOTIV-EMOTION 
wl: init_args(exact_only, u_goal_c62_motiv_emotion).


% annotating U::GOAL->MOTIV-EMOTION 
f_u_goal_c62_motiv_emotion(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_context, Context_Param)],
	f_u_ob_c36_get(Goal_Param, u_main_motiv, Main_motiv),
	Main_motiv=FnResult.
:- set_opv(f_u_goal_c62_motiv_emotion, classof, claz_function),
   set_opv(u_goal_c62_motiv_emotion, compile_as, kw_function),
   set_opv(u_goal_c62_motiv_emotion, function, f_u_goal_c62_motiv_emotion),
   DefunResult=u_goal_c62_motiv_emotion.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21682 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This is expensive and should only be called upon indexing of",
				     1,
				     21755)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21682 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" a final episode.", 1, 21818)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21836 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'find-misc-indices',
			    [context, 'top-level-goal'],
			    
			    [ yloop,
			      
			      [ initial,
				[result, []],
				
				[ ancestors,
				  
				  [ 'cx$ancestors',
				    
				    [ 'ob$get',
				      'top-level-goal',
				      [quote, 'activation-context']
				    ]
				  ]
				]
			      ],
			      [yfor, ob, in, ['cx$get-all', context]],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    
				    [ not,
				      
				      [ 'any?',
					[lambda, [x], ['memq?', x, ancestors]],
					['ob$gets', ob, [quote, 'top-context']]
				      ]
				    ],
				    
				    [ or,
				      ['ty$instance?', ob, [quote, goal]],
				      ['ty$instance?', ob, [quote, state]]
				    ]
				  ],
				  
				  [ setq,
				    result,
				    
				    [ prune,
				      [union, result, ['objects-in', ob]],
				      [lambda, [elem], [not, ['me?', elem]]]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::FIND-MISC-INDICES 
wl: lambda_def(defun,
	      u_find_misc_indices,
	      f_u_find_misc_indices,
	      [u_context, u_top_level_goal],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_result, []],
		    
		    [ u_ancestors,
		      
		      [ u_cx_c36_ancestors,
			
			[ u_ob_c36_get,
			  u_top_level_goal,
			  [quote, u_activation_context]
			]
		      ]
		    ]
		  ],
		  [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			
			[ not,
			  
			  [ u_any_c63,
			    [lambda, [u_x], [u_memq_c63, u_x, u_ancestors]],
			    [u_ob_c36_gets, u_ob, [quote, u_top_context]]
			  ]
			],
			
			[ or,
			  [u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
			  [u_ty_c36_instance_c63, u_ob, [quote, u_state]]
			]
		      ],
		      
		      [ setq,
			u_result,
			
			[ u_prune,
			  [union, u_result, [u_objects_in, u_ob]],
			  [lambda, [u_elem], [not, [u_me_c63, u_elem]]]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::FIND-MISC-INDICES 
wl: arglist_info(u_find_misc_indices,
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

% annotating U::FIND-MISC-INDICES 
wl: init_args(exact_only, u_find_misc_indices).


% annotating U::FIND-MISC-INDICES 
f_u_find_misc_indices(Context_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, []],
		      
		      [ u_ancestors,
			
			[ u_cx_c36_ancestors,
			  
			  [ u_ob_c36_get,
			    u_top_level_goal,
			    [quote, u_activation_context]
			  ]
			]
		      ]
		    ],
		    [u_yfor, u_ob, u_in, [u_cx_c36_get_all, u_context]],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ not,
			    
			    [ u_any_c63,
			      [lambda, [u_x], [u_memq_c63, u_x, u_ancestors]],
			      [u_ob_c36_gets, u_ob, [quote, u_top_context]]
			    ]
			  ],
			  
			  [ or,
			    [u_ty_c36_instance_c63, u_ob, [quote, u_goal]],
			    [u_ty_c36_instance_c63, u_ob, [quote, u_state]]
			  ]
			],
			
			[ setq,
			  u_result,
			  
			  [ u_prune,
			    [union, u_result, [u_objects_in, u_ob]],
			    [lambda, [u_elem], [not, [u_me_c63, u_elem]]]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_find_misc_indices, classof, claz_function),
   set_opv(u_find_misc_indices, compile_as, kw_function),
   set_opv(u_find_misc_indices, function, f_u_find_misc_indices),
   DefunResult=u_find_misc_indices.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21836 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" First condition checks that ob was asserted in activation context",
				     5,
				     22082)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21836 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" or later.", 5, 22154)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:21836 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: should not include other common indices.",
				     5,
				     22364)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:22564 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'episode-store1',
			    [goal, context, desirability],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'ep-store',
			      '$STRING'("Store goal of episode ~A, realism ~A"),
			      goal,
			      [strength, ['ob$get', goal, [quote, obj]]]
			    ],
			    
			    [ 'let*',
			      
			      [ [rule, []],
				
				[ subgoals,
				  
				  [ 'goal-subgoals',
				    goal,
				    context,
				    '*me-belief-path*'
				  ]
				],
				[ep, []]
			      ],
			      
			      [ if,
				subgoals,
				
				[ progn,
				  
				  [ setq,
				    rule,
				    
				    [ 'goal-subgoals-rule',
				      goal,
				      context,
				      '*me-belief-path*'
				    ]
				  ],
				  
				  [ yloop,
				    [yfor, subgoal, in, subgoals],
				    
				    [ ydo,
				      ['episode-store1', subgoal, context, []]
				    ]
				  ],
				  
				  [ setq,
				    ep,
				    
				    [ 'make-and-store-episode',
				      rule,
				      goal,
				      context,
				      [strength, ['ob$get', goal, [quote, obj]]],
				      desirability,
				      [],
				      ['subgoals->eps', subgoals]
				    ]
				  ]
				]
			      ],
			      ep
			    ]
			  ]).

% annotating U::EPISODE-STORE1 
wl: lambda_def(defun,
	      u_episode_store1,
	      f_u_episode_store1,
	      [u_goal, u_context, u_desirability],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_ep_store,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('S'),
			     #\(t),
			     #\(o),
			     #\(r),
			     #\(e),
			     #\(' '),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(o),
			     #\(f),
			     #\(' '),
			     #\(e),
			     #\(p),
			     #\(i),
			     #\(s),
			     #\(o),
			     #\(d),
			     #\(e),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(','),
			     #\(' '),
			     #\(r),
			     #\(e),
			     #\(a),
			     #\(l),
			     #\(i),
			     #\(s),
			     #\(m),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_goal,
		  [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]]
		],
		
		[ let_xx,
		  
		  [ [u_rule, []],
		    
		    [ u_subgoals,
		      
		      [ u_goal_subgoals,
			u_goal,
			u_context,
			u_xx_me_belief_path_xx
		      ]
		    ],
		    [u_ep, []]
		  ],
		  
		  [ if,
		    u_subgoals,
		    
		    [ progn,
		      
		      [ setq,
			u_rule,
			
			[ u_goal_subgoals_rule,
			  u_goal,
			  u_context,
			  u_xx_me_belief_path_xx
			]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_subgoal, u_in, u_subgoals],
			[u_ydo, [u_episode_store1, u_subgoal, u_context, []]]
		      ],
		      
		      [ setq,
			u_ep,
			
			[ u_make_and_store_episode,
			  u_rule,
			  u_goal,
			  u_context,
			  [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]],
			  u_desirability,
			  [],
			  [u_subgoals_c62_eps, u_subgoals]
			]
		      ]
		    ]
		  ],
		  u_ep
		]
	      ]).


% annotating U::EPISODE-STORE1 
wl: arglist_info(u_episode_store1,
		[u_goal, u_context, u_desirability],
		[Goal_Param, Context_Param, Desirability_Param],
		arginfo{ all:[u_goal, u_context, u_desirability],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context, u_desirability],
			 opt:0,
			 req:[u_goal, u_context, u_desirability],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODE-STORE1 
wl: init_args(exact_only, u_episode_store1).


% annotating U::EPISODE-STORE1 
f_u_episode_store1(Goal_Param, Context_Param, Desirability_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_desirability, Desirability_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_ep_store,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(t),
				       #\(o),
				       #\(r),
				       #\(e),
				       #\(' '),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(' '),
				       #\(e),
				       #\(p),
				       #\(i),
				       #\(s),
				       #\(o),
				       #\(d),
				       #\(e),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(l),
				       #\(i),
				       #\(s),
				       #\(m),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_goal,
			    [u_strength, [u_ob_c36_get, u_goal, [quote, u_obj]]]
			  ],
			  Roman_nl_Ret),
	LEnv=[[bv(u_rule, [])]|Env],
	get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_goal_subgoals(Goal_Param,
			  Context_Param,
			  Xx_me_belief_path_xx_Get,
			  Subgoals_Init),
	LEnv18=[[bv(u_subgoals, Subgoals_Init)]|LEnv],
	Env=[[bv(u_ep, [])]|LEnv18],
	get_var(Env, u_subgoals, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get33),
	    f_u_goal_subgoals_rule(Goal_Param,
				   Context_Param,
				   Xx_me_belief_path_xx_Get33,
				   Rule),
	    set_var(Env, u_rule, Rule),
	    f_u_yloop(
		      [ [u_yfor, u_subgoal, u_in, u_subgoals],
			[u_ydo, [u_episode_store1, u_subgoal, u_context, []]]
		      ],
		      Yloop_Ret),
	    get_var(Env, u_rule, Rule_Get),
	    f_u_strength([u_ob_c36_get, u_goal, [quote, u_obj]], Strength_Ret),
	    get_var(Env, u_subgoals, Subgoals_Get38),
	    f_u_subgoals_c62_eps(Subgoals_Get38, C62_eps_Ret),
	    f_u_make_and_store_episode(Rule_Get,
				       Goal_Param,
				       Context_Param,
				       Strength_Ret,
				       Desirability_Param,
				       [],
				       C62_eps_Ret,
				       TrueResult),
	    set_var(Env, u_ep, TrueResult),
	    _56666=TrueResult
	;   _56666=[]
	),
	get_var(Env, u_ep, Ep_Get),
	LetResult=Ep_Get,
	LetResult=FnResult.
:- set_opv(f_u_episode_store1, classof, claz_function),
   set_opv(u_episode_store1, compile_as, kw_function),
   set_opv(u_episode_store1, function, f_u_episode_store1),
   DefunResult=u_episode_store1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:22564 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23365)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:22564 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Similarity metric for episodic retrieval",
				     1,
				     23367)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:22564 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23410)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23412 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$similarity',
			    [ob1, ob2],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      simil,
			      '$STRING'("Assess similarity between ~A and ~A"),
			      ob1,
			      ob2
			    ],
			    
			    [ let,
			      [[result, ['ob$similarity1', ob1, ob2]]],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				simil,
				'$STRING'("Similarity between ~A and ~A = ~A"),
				ob1,
				ob2,
				result
			      ],
			      result
			    ]
			  ]).

% annotating U::OB$SIMILARITY 
wl: lambda_def(defun,
	      u_ob_c36_similarity,
	      f_u_ob_c36_similarity,
	      [u_ob1, u_ob2],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_simil,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(s),
			     #\(s),
			     #\(e),
			     #\(s),
			     #\(s),
			     #\(' '),
			     #\(s),
			     #\(i),
			     #\(m),
			     #\(i),
			     #\(l),
			     #\(a),
			     #\(r),
			     #\(i),
			     #\(t),
			     #\(y),
			     #\(' '),
			     #\(b),
			     #\(e),
			     #\(t),
			     #\(w),
			     #\(e),
			     #\(e),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(a),
			     #\(n),
			     #\(d),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_ob1,
		  u_ob2
		],
		
		[ let,
		  [[u_result, [u_ob_c36_similarity1, u_ob1, u_ob2]]],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_simil,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('S'),
			       #\(i),
			       #\(m),
			       #\(i),
			       #\(l),
			       #\(a),
			       #\(r),
			       #\(i),
			       #\(t),
			       #\(y),
			       #\(' '),
			       #\(b),
			       #\(e),
			       #\(t),
			       #\(w),
			       #\(e),
			       #\(e),
			       #\(n),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(a),
			       #\(n),
			       #\(d),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_ob1,
		    u_ob2,
		    u_result
		  ],
		  u_result
		]
	      ]).


% annotating U::OB$SIMILARITY 
wl: arglist_info(u_ob_c36_similarity,
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

% annotating U::OB$SIMILARITY 
wl: init_args(exact_only, u_ob_c36_similarity).


% annotating U::OB$SIMILARITY 
f_u_ob_c36_similarity(Ob1_Param, Ob2_Param, FnResult) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_simil,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(s),
				       #\(s),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(' '),
				       #\(s),
				       #\(i),
				       #\(m),
				       #\(i),
				       #\(l),
				       #\(a),
				       #\(r),
				       #\(i),
				       #\(t),
				       #\(y),
				       #\(' '),
				       #\(b),
				       #\(e),
				       #\(t),
				       #\(w),
				       #\(e),
				       #\(e),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ob1,
			    u_ob2
			  ],
			  Roman_nl_Ret),
	f_u_ob_c36_similarity1(Ob1_Param, Ob2_Param, Result_Init),
	LEnv=[[bv(u_result, Result_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_simil,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(i),
				       #\(m),
				       #\(i),
				       #\(l),
				       #\(a),
				       #\(r),
				       #\(i),
				       #\(t),
				       #\(y),
				       #\(' '),
				       #\(b),
				       #\(e),
				       #\(t),
				       #\(w),
				       #\(e),
				       #\(e),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ob1,
			    u_ob2,
			    u_result
			  ],
			  Roman_nl_Ret24),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_ob_c36_similarity, classof, claz_function),
   set_opv(u_ob_c36_similarity, compile_as, kw_function),
   set_opv(u_ob_c36_similarity, function, f_u_ob_c36_similarity),
   DefunResult=u_ob_c36_similarity.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23687 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'distance->similarity',
			    [x],
			    ['fl-', 1.0, ['fl*', 0.25, ['fixnum->flonum', x]]]
			  ]).

% annotating U::DISTANCE->SIMILARITY 
wl: lambda_def(defun,
	      u_distance_c62_similarity,
	      f_u_distance_c62_similarity,
	      [u_x],
	      [[u_flc45, 1.0, [u_fl_xx, 0.25, [u_fixnum_c62_flonum, u_x]]]]).


% annotating U::DISTANCE->SIMILARITY 
wl: arglist_info(u_distance_c62_similarity,
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

% annotating U::DISTANCE->SIMILARITY 
wl: init_args(exact_only, u_distance_c62_similarity).


% annotating U::DISTANCE->SIMILARITY 
f_u_distance_c62_similarity(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_flc45(1.0, [u_fl_xx, 0.25, [u_fixnum_c62_flonum, u_x]], Flc45_Ret),
	Flc45_Ret=FnResult.
:- set_opv(f_u_distance_c62_similarity, classof, claz_function),
   set_opv(u_distance_c62_similarity, compile_as, kw_function),
   set_opv(u_distance_c62_similarity, function, f_u_distance_c62_similarity),
   DefunResult=u_distance_c62_similarity.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23762 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ty-distance',
			    [type1, type2],
			    
			    [ if,
			      [and, ['ty?', type1], ['ty?', type2]],
			      ['ty$distance', type1, type2],
			      0
			    ]
			  ]).

% annotating U::TY-DISTANCE 
wl: lambda_def(defun,
	      u_ty_distance,
	      f_u_ty_distance,
	      [u_type1, u_type2],
	      
	      [ 
		[ if,
		  [and, [u_ty_c63, u_type1], [u_ty_c63, u_type2]],
		  [u_ty_c36_distance, u_type1, u_type2],
		  0
		]
	      ]).


% annotating U::TY-DISTANCE 
wl: arglist_info(u_ty_distance,
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

% annotating U::TY-DISTANCE 
wl: init_args(exact_only, u_ty_distance).


% annotating U::TY-DISTANCE 
f_u_ty_distance(Type1_Param, Type2_Param, FnResult) :-
	Env=[bv(u_type1, Type1_Param), bv(u_type2, Type2_Param)],
	f_u_ty_c63(u_type1, IFTEST16),
	(   IFTEST16\==[]
	->  f_u_ty_c63(u_type2, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_ty_c36_distance(Type1_Param, Type2_Param, TrueResult21),
	    FnResult=TrueResult21
	;   FnResult=0
	).
:- set_opv(f_u_ty_distance, classof, claz_function),
   set_opv(u_ty_distance, compile_as, kw_function),
   set_opv(u_ty_distance, function, f_u_ty_distance),
   DefunResult=u_ty_distance.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23762 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23875)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23762 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: We have to take 1/type-distance, no? What was original alg in notebook?",
				     1,
				     23877)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23762 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 23957)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23958 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$similarity1',
			    [ob1, ob2],
			    
			    [ cond,
			      [['eq?', ob1, ob2], 1.0],
			      
			      [ 
				[ or,
				  ['pair?', ob1],
				  ['symbol?', ob1],
				  ['string?', ob1],
				  ['pair?', ob2],
				  ['symbol?', ob2],
				  ['string?', ob2]
				],
				0.0
			      ],
			      
			      [ [and, ['var?', ob1], ['var?', ob2]],
				
				[ 'distance->similarity',
				  
				  [ 'ty-distance',
				    ['variable-type', ob1],
				    ['variable-type', ob2]
				  ]
				]
			      ],
			      
			      [ ['var?', ob1],
				
				[ 'distance->similarity',
				  
				  [ 'ty-distance',
				    ['variable-type', ob1],
				    ['ob$ty', ob2]
				  ]
				]
			      ],
			      
			      [ ['var?', ob2],
				
				[ 'distance->similarity',
				  
				  [ 'ty-distance',
				    ['ob$ty', ob1],
				    ['variable-type', ob2]
				  ]
				]
			      ],
			      
			      [ 
				[ and,
				  ['ty$instance?', ob1, [quote, object]],
				  ['ty$instance?', ob2, [quote, object]]
				],
				
				[ 'distance->similarity',
				  ['ty-distance', ['ob$ty', ob1], ['ob$ty', ob2]]
				]
			      ],
			      
			      [ [and, ['ob?', ob1], ['ob?', ob2]],
				
				[ yloop,
				  
				  [ initial,
				    [val2, []],
				    [temp, []],
				    
				    [ result,
				      
				      [ 'distance->similarity',
					
					[ 'ty-distance',
					  ['ob$ty', ob1],
					  ['ob$ty', ob2]
					]
				      ]
				    ]
				  ],
				  [yuntil, [=, result, '*min-flonum*']],
				  [yfor, sv, in, ['ob$pairs', ob1]],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ and,
					
					[ 'neq?',
					  ['slots-name', sv],
					  [quote, type]
					],
					
					[ 'neq?',
					  ['slots-name', sv],
					  [quote, strength]
					],
					
					[ not,
					  
					  [ 'memq?',
					    ['slots-name', sv],
					    '*permanent-ignore-slots*'
					  ]
					]
				      ],
				      
				      [ progn,
					
					[ setq,
					  val2,
					  ['ob$gets', ob2, ['slots-name', sv]]
					],
					
					[ if,
					  ['null?', val2],
					  [setq, result, '*min-flonum*'],
					  
					  [ progn,
					    
					    [ setq,
					      temp,
					      
					      [ apply,
						[quote, max],
						
						[ map,
						  [quote, list],
						  
						  [ lambda,
						    [x],
						    
						    [ 'ob$similarity1',
						      ['slots-value', sv],
						      x
						    ]
						  ],
						  val2
						]
					      ]
					    ],
					    
					    [ if,
					      [not, [=, temp, '*min-flonum*']],
					      
					      [ setq,
						result,
						
						[ 'fl+',
						  result,
						  ['fl*', 0.5, temp]
						]
					      ],
					      [setq, result, '*min-flonum*']
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ],
			      
			      [ else,
				
				[ error,
				  '$STRING'("Bug: ob$similarity got unknown stuff: ~A ~A"),
				  ob1,
				  ob2
				]
			      ]
			    ]
			  ]).

% annotating U::OB$SIMILARITY1 
wl: lambda_def(defun,
	      u_ob_c36_similarity1,
	      f_u_ob_c36_similarity1,
	      [u_ob1, u_ob2],
	      
	      [ 
		[ cond,
		  [[u_eq_c63, u_ob1, u_ob2], 1.0],
		  
		  [ 
		    [ or,
		      [u_pair_c63, u_ob1],
		      [u_symbol_c63, u_ob1],
		      [u_string_c63, u_ob1],
		      [u_pair_c63, u_ob2],
		      [u_symbol_c63, u_ob2],
		      [u_string_c63, u_ob2]
		    ],
		    0.0
		  ],
		  
		  [ [and, [u_var_c63, u_ob1], [u_var_c63, u_ob2]],
		    
		    [ u_distance_c62_similarity,
		      
		      [ u_ty_distance,
			[u_variable_type, u_ob1],
			[u_variable_type, u_ob2]
		      ]
		    ]
		  ],
		  
		  [ [u_var_c63, u_ob1],
		    
		    [ u_distance_c62_similarity,
		      
		      [ u_ty_distance,
			[u_variable_type, u_ob1],
			[u_ob_c36_ty, u_ob2]
		      ]
		    ]
		  ],
		  
		  [ [u_var_c63, u_ob2],
		    
		    [ u_distance_c62_similarity,
		      
		      [ u_ty_distance,
			[u_ob_c36_ty, u_ob1],
			[u_variable_type, u_ob2]
		      ]
		    ]
		  ],
		  
		  [ 
		    [ and,
		      [u_ty_c36_instance_c63, u_ob1, [quote, u_object]],
		      [u_ty_c36_instance_c63, u_ob2, [quote, u_object]]
		    ],
		    
		    [ u_distance_c62_similarity,
		      [u_ty_distance, [u_ob_c36_ty, u_ob1], [u_ob_c36_ty, u_ob2]]
		    ]
		  ],
		  
		  [ [and, [u_ob_c63, u_ob1], [u_ob_c63, u_ob2]],
		    
		    [ u_yloop,
		      
		      [ u_initial,
			[u_val2, []],
			[u_temp, []],
			
			[ u_result,
			  
			  [ u_distance_c62_similarity,
			    
			    [ u_ty_distance,
			      [u_ob_c36_ty, u_ob1],
			      [u_ob_c36_ty, u_ob2]
			    ]
			  ]
			]
		      ],
		      [u_yuntil, [=, u_result, u_xx_min_flonum_xx]],
		      [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob1]],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ and,
			    [u_neq_c63, [u_slots_name, u_sv], [quote, type]],
			    [u_neq_c63, [u_slots_name, u_sv], [quote, u_strength]],
			    
			    [ not,
			      
			      [ u_memq_c63,
				[u_slots_name, u_sv],
				u_xx_permanent_ignore_slots_xx
			      ]
			    ]
			  ],
			  
			  [ progn,
			    
			    [ setq,
			      u_val2,
			      [u_ob_c36_gets, u_ob2, [u_slots_name, u_sv]]
			    ],
			    
			    [ if,
			      [u_null_c63, u_val2],
			      [setq, u_result, u_xx_min_flonum_xx],
			      
			      [ progn,
				
				[ setq,
				  u_temp,
				  
				  [ apply,
				    [quote, max],
				    
				    [ map,
				      [quote, list],
				      
				      [ lambda,
					[u_x],
					
					[ u_ob_c36_similarity1,
					  [u_slots_value, u_sv],
					  u_x
					]
				      ],
				      u_val2
				    ]
				  ]
				],
				
				[ if,
				  [not, [=, u_temp, u_xx_min_flonum_xx]],
				  
				  [ setq,
				    u_result,
				    [u_fl_c43, u_result, [u_fl_xx, 0.5, u_temp]]
				  ],
				  [setq, u_result, u_xx_min_flonum_xx]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('B'),
				 #\(u),
				 #\(g),
				 #\(:),
				 #\(' '),
				 #\(o),
				 #\(b),
				 #\($),
				 #\(s),
				 #\(i),
				 #\(m),
				 #\(i),
				 #\(l),
				 #\(a),
				 #\(r),
				 #\(i),
				 #\(t),
				 #\(y),
				 #\(' '),
				 #\(g),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(u),
				 #\(n),
				 #\(k),
				 #\(n),
				 #\(o),
				 #\(w),
				 #\(n),
				 #\(' '),
				 #\(s),
				 #\(t),
				 #\(u),
				 #\(f),
				 #\(f),
				 #\(:),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_ob1,
		      u_ob2
		    ]
		  ]
		]
	      ]).


% annotating U::OB$SIMILARITY1 
wl: arglist_info(u_ob_c36_similarity1,
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

% annotating U::OB$SIMILARITY1 
wl: init_args(exact_only, u_ob_c36_similarity1).


% annotating U::OB$SIMILARITY1 
f_u_ob_c36_similarity1(Ob1_Param, Ob2_Param, ElseResult52) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param)],
	f_u_eq_c63(u_ob1, u_ob2, IFTEST),
	(   IFTEST\==[]
	->  ElseResult52=1.0
	;   (   f_u_pair_c63(u_ob1, FORM1_Res22),
		FORM1_Res22\==[],
		IFTEST16=FORM1_Res22
	    ->  true
	    ;   f_u_symbol_c63(u_ob1, FORM1_Res21),
		FORM1_Res21\==[],
		IFTEST16=FORM1_Res21
	    ->  true
	    ;   f_u_string_c63(u_ob1, FORM1_Res20),
		FORM1_Res20\==[],
		IFTEST16=FORM1_Res20
	    ->  true
	    ;   f_u_pair_c63(u_ob2, FORM1_Res19),
		FORM1_Res19\==[],
		IFTEST16=FORM1_Res19
	    ->  true
	    ;   f_u_symbol_c63(u_ob2, FORM1_Res),
		FORM1_Res\==[],
		IFTEST16=FORM1_Res
	    ->  true
	    ;   f_u_string_c63(u_ob2, String_c63_Ret),
		IFTEST16=String_c63_Ret
	    ),
	    (   IFTEST16\==[]
	    ->  ElseResult52=0.0
	    ;   f_u_var_c63(u_ob1, IFTEST25),
		(   IFTEST25\==[]
		->  f_u_var_c63(u_ob2, TrueResult),
		    IFTEST23=TrueResult
		;   IFTEST23=[]
		),
		(   IFTEST23\==[]
		->  f_u_variable_type(u_ob1, Ty_distance_Param),
		    f_u_variable_type(u_ob2, Variable_type_Ret),
		    f_u_ty_distance(Ty_distance_Param,
				    Variable_type_Ret,
				    C62_similarity_Param),
		    f_u_distance_c62_similarity(C62_similarity_Param,
						TrueResult59),
		    ElseResult52=TrueResult59
		;   f_u_var_c63(u_ob1, IFTEST28),
		    (   IFTEST28\==[]
		    ->  f_u_variable_type(u_ob1, Ty_distance_Param67),
			f_u_ob_c36_ty(u_ob2, C36_ty_Ret),
			f_u_ty_distance(Ty_distance_Param67,
					C36_ty_Ret,
					C62_similarity_Param68),
			f_u_distance_c62_similarity(C62_similarity_Param68,
						    TrueResult57),
			ElseResult52=TrueResult57
		    ;   f_u_var_c63(u_ob2, IFTEST30),
			(   IFTEST30\==[]
			->  f_u_ob_c36_ty(u_ob1, Ty_distance_Param69),
			    f_u_variable_type(u_ob2, Variable_type_Ret76),
			    f_u_ty_distance(Ty_distance_Param69,
					    Variable_type_Ret76,
					    C62_similarity_Param70),
			    f_u_distance_c62_similarity(C62_similarity_Param70,
							TrueResult55),
			    ElseResult52=TrueResult55
			;   f_u_ty_c36_instance_c63(Ob1_Param,
						    u_object,
						    IFTEST34),
			    (   IFTEST34\==[]
			    ->  f_u_ty_c36_instance_c63(Ob2_Param,
							u_object,
							TrueResult38),
				IFTEST32=TrueResult38
			    ;   IFTEST32=[]
			    ),
			    (   IFTEST32\==[]
			    ->  f_u_ob_c36_ty(u_ob1, Ty_distance_Param71),
				f_u_ob_c36_ty(u_ob2, C36_ty_Ret77),
				f_u_ty_distance(Ty_distance_Param71,
						C36_ty_Ret77,
						C62_similarity_Param72),
				f_u_distance_c62_similarity(C62_similarity_Param72,
							    TrueResult53),
				ElseResult52=TrueResult53
			    ;   f_u_ob_c63(u_ob1, IFTEST41),
				(   IFTEST41\==[]
				->  f_u_ob_c63(u_ob2, TrueResult43),
				    IFTEST39=TrueResult43
				;   IFTEST39=[]
				),
				(   IFTEST39\==[]
				->  f_u_yloop(
					      [ 
						[ u_initial,
						  [u_val2, []],
						  [u_temp, []],
						  
						  [ u_result,
						    
						    [ u_distance_c62_similarity,
						      
						      [ u_ty_distance,
							[u_ob_c36_ty, u_ob1],
							[u_ob_c36_ty, u_ob2]
						      ]
						    ]
						  ]
						],
						
						[ u_yuntil,
						  
						  [ (=),
						    u_result,
						    u_xx_min_flonum_xx
						  ]
						],
						
						[ u_yfor,
						  u_sv,
						  u_in,
						  [u_ob_c36_pairs, u_ob1]
						],
						
						[ u_ydo,
						  
						  [ if,
						    
						    [ and,
						      
						      [ u_neq_c63,
							[u_slots_name, u_sv],
							[quote, type]
						      ],
						      
						      [ u_neq_c63,
							[u_slots_name, u_sv],
							[quote, u_strength]
						      ],
						      
						      [ not,
							
							[ u_memq_c63,
							  [u_slots_name, u_sv],
							  u_xx_permanent_ignore_slots_xx
							]
						      ]
						    ],
						    
						    [ progn,
						      
						      [ setq,
							u_val2,
							
							[ u_ob_c36_gets,
							  u_ob2,
							  [u_slots_name, u_sv]
							]
						      ],
						      
						      [ if,
							[u_null_c63, u_val2],
							
							[ setq,
							  u_result,
							  u_xx_min_flonum_xx
							],
							
							[ progn,
							  
							  [ setq,
							    u_temp,
							    
							    [ apply,
							      [quote, max],
							      
							      [ map,
								[quote, list],
								
								[ lambda,
								  [u_x],
								  
								  [ u_ob_c36_similarity1,
								    
								    [ u_slots_value,
								      u_sv
								    ],
								    u_x
								  ]
								],
								u_val2
							      ]
							    ]
							  ],
							  
							  [ if,
							    
							    [ not,
							      
							      [ (=),
								u_temp,
								u_xx_min_flonum_xx
							      ]
							    ],
							    
							    [ setq,
							      u_result,
							      
							      [ u_fl_c43,
								u_result,
								
								[ u_fl_xx,
								  0.5,
								  u_temp
								]
							      ]
							    ],
							    
							    [ setq,
							      u_result,
							      u_xx_min_flonum_xx
							    ]
							  ]
							]
						      ]
						    ]
						  ]
						],
						[u_yresult, u_result]
					      ],
					      TrueResult51),
				    ElseResult52=TrueResult51
				;   get_var(Env, u_else, IFTEST44),
				    (   IFTEST44\==[]
				    ->  cl_error(
						 [ '$ARRAY'([*],
							    claz_base_character,
							    
							    [ #\('B'),
							      #\(u),
							      #\(g),
							      #\(:),
							      #\(' '),
							      #\(o),
							      #\(b),
							      #\($),
							      #\(s),
							      #\(i),
							      #\(m),
							      #\(i),
							      #\(l),
							      #\(a),
							      #\(r),
							      #\(i),
							      #\(t),
							      #\(y),
							      #\(' '),
							      #\(g),
							      #\(o),
							      #\(t),
							      #\(' '),
							      #\(u),
							      #\(n),
							      #\(k),
							      #\(n),
							      #\(o),
							      #\(w),
							      #\(n),
							      #\(' '),
							      #\(s),
							      #\(t),
							      #\(u),
							      #\(f),
							      #\(f),
							      #\(:),
							      #\(' '),
							      #\(~),
							      #\('A'),
							      #\(' '),
							      #\(~),
							      #\('A')
							    ]),
						   Ob1_Param,
						   Ob2_Param
						 ],
						 TrueResult49),
					ElseResult52=TrueResult49
				    ;   ElseResult52=[]
				    )
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_similarity1, classof, claz_function),
   set_opv(u_ob_c36_similarity1, compile_as, kw_function),
   set_opv(u_ob_c36_similarity1, function, f_u_ob_c36_similarity1),
   DefunResult=u_ob_c36_similarity1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23958 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" should do for now", 55, 24124)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23958 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Could keep track of used slots as in unify?",
				     11,
				     25007)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23958 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" below code really assumes no multiple slot values",
				     11,
				     25107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:23958 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" non-homomorphic obs", 51, 25554)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:26158 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*min-flonum*', -10000.0]).
:- set_var(TLEnv3, setq, u_xx_min_flonum_xx, -10000.0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:26158 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" T has such a constant?", 30, 26188)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:26213 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'try-analogical-plan',
			    
			    [ goal,
			      'goal-obj',
			      context,
			      'analogical-episode',
			      'belief-path',
			      'top-level-goal'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Try analogical plan for ~A in ~A ep ~A"),
			      goal,
			      context,
			      'analogical-episode'
			    ],
			    
			    [ yloop,
			      [initial, ['sprouted-contexts', []]],
			      
			      [ yfor,
				bd,
				in,
				
				[ 'rule-applications',
				  'goal-obj',
				  context,
				  ['ob$get', 'analogical-episode', [quote, rule]],
				  'belief-path',
				  []
				]
			      ],
			      
			      [ ydo,
				
				[ setq,
				  'sprouted-contexts',
				  
				  [ 'append!',
				    
				    [ 'run-analogical-plan',
				      goal,
				      'goal-obj',
				      context,
				      bd,
				      
				      [ 'ob$get',
					'analogical-episode',
					[quote, goal]
				      ],
				      
				      [ 'ob$get',
					'analogical-episode',
					[quote, context]
				      ],
				      
				      [ 'ob$get',
					'analogical-episode',
					[quote, rule]
				      ],
				      1.0,
				      'belief-path',
				      'top-level-goal',
				      'analogical-episode',
				      []
				    ],
				    'sprouted-contexts'
				  ]
				]
			      ],
			      [yresult, 'sprouted-contexts']
			    ]
			  ]).

% annotating U::TRY-ANALOGICAL-PLAN 
wl: lambda_def(defun,
	      u_try_analogical_plan,
	      f_u_try_analogical_plan,
	      
	      [ u_goal,
		u_goal_obj,
		u_context,
		u_analogical_episode,
		u_belief_path,
		u_top_level_goal
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('T'),
			     #\(r),
			     #\(y),
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
			     #\(e),
			     #\(p),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_goal,
		  u_context,
		  u_analogical_episode
		],
		
		[ u_yloop,
		  [u_initial, [u_sprouted_contexts, []]],
		  
		  [ u_yfor,
		    u_bd,
		    u_in,
		    
		    [ u_rule_applications,
		      u_goal_obj,
		      u_context,
		      [u_ob_c36_get, u_analogical_episode, [quote, u_rule]],
		      u_belief_path,
		      []
		    ]
		  ],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_sprouted_contexts,
		      
		      [ u_append_c33,
			
			[ u_run_analogical_plan,
			  u_goal,
			  u_goal_obj,
			  u_context,
			  u_bd,
			  [u_ob_c36_get, u_analogical_episode, [quote, u_goal]],
			  
			  [ u_ob_c36_get,
			    u_analogical_episode,
			    [quote, u_context]
			  ],
			  [u_ob_c36_get, u_analogical_episode, [quote, u_rule]],
			  1.0,
			  u_belief_path,
			  u_top_level_goal,
			  u_analogical_episode,
			  []
			],
			u_sprouted_contexts
		      ]
		    ]
		  ],
		  [u_yresult, u_sprouted_contexts]
		]
	      ]).


% annotating U::TRY-ANALOGICAL-PLAN 
wl: arglist_info(u_try_analogical_plan,
		
		[ u_goal,
		  u_goal_obj,
		  u_context,
		  u_analogical_episode,
		  u_belief_path,
		  u_top_level_goal
		],
		
		[ Goal_Param,
		  Goal_obj_Param,
		  Context_Param,
		  Analogical_episode_Param,
		  Belief_path_Param,
		  Top_level_goal_Param
		],
		arginfo{ all:
			     [ u_goal,
			       u_goal_obj,
			       u_context,
			       u_analogical_episode,
			       u_belief_path,
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
				 u_analogical_episode,
				 u_belief_path,
				 u_top_level_goal
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       u_goal_obj,
			       u_context,
			       u_analogical_episode,
			       u_belief_path,
			       u_top_level_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TRY-ANALOGICAL-PLAN 
wl: init_args(exact_only, u_try_analogical_plan).


% annotating U::TRY-ANALOGICAL-PLAN 
f_u_try_analogical_plan(Goal_Param, Goal_obj_Param, Context_Param, Analogical_episode_Param, Belief_path_Param, Top_level_goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_goal_obj, Goal_obj_Param), bv(u_context, Context_Param), bv(u_analogical_episode, Analogical_episode_Param), bv(u_belief_path, Belief_path_Param), bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
				       #\(r),
				       #\(y),
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
				       #\(e),
				       #\(p),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_goal,
			    u_context,
			    u_analogical_episode
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_sprouted_contexts, []]],
		    
		    [ u_yfor,
		      u_bd,
		      u_in,
		      
		      [ u_rule_applications,
			u_goal_obj,
			u_context,
			[u_ob_c36_get, u_analogical_episode, [quote, u_rule]],
			u_belief_path,
			[]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_sprouted_contexts,
			
			[ u_append_c33,
			  
			  [ u_run_analogical_plan,
			    u_goal,
			    u_goal_obj,
			    u_context,
			    u_bd,
			    [u_ob_c36_get, u_analogical_episode, [quote, u_goal]],
			    
			    [ u_ob_c36_get,
			      u_analogical_episode,
			      [quote, u_context]
			    ],
			    [u_ob_c36_get, u_analogical_episode, [quote, u_rule]],
			    1.0,
			    u_belief_path,
			    u_top_level_goal,
			    u_analogical_episode,
			    []
			  ],
			  u_sprouted_contexts
			]
		      ]
		    ],
		    [u_yresult, u_sprouted_contexts]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_try_analogical_plan, classof, claz_function),
   set_opv(u_try_analogical_plan, compile_as, kw_function),
   set_opv(u_try_analogical_plan, function, f_u_try_analogical_plan),
   DefunResult=u_try_analogical_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*relaxed-analogy-realism*', 0.5]).
:- set_var(TLEnv3, setq, u_xx_relaxed_analogy_realism_xx, 0.5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" new vsn -- but doesn't do verification; that is left to",
				     1,
				     27272)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" try-analogical-plan since when analogical plan is first invoked,",
				     1,
				     27330)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" it is automatically verified.", 1, 27397)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 27429)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Need to deal with initial slot in here? (Note, however, possible",
				     1,
				     27431)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27233 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" conflictions with mutation4, etc.)",
				     1,
				     27504)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27540 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-analogical-plan',
			    
			    [ 'target-goal',
			      'target-goal-obj',
			      'target-context',
			      'target-bd',
			      'source-goal',
			      'source-context',
			      rule,
			      ordering,
			      'belief-path',
			      'top-level-goal',
			      'analogical-episode',
			      'reminding?'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Run analogical plan for ~A in ~A"),
			      'target-goal',
			      'target-context'
			    ],
			    
			    [ 'let*',
			      
			      [ 
				[ 'source-subgoals',
				  
				  [ 'goal-subgoals',
				    'source-goal',
				    'source-context',
				    '*me-belief-path*'
				  ]
				],
				
				[ 'source-goal-obj',
				  ['ob$get', 'source-goal', [quote, obj]]
				],
				['r-subgoal-objs', ['rule-subgoal-objs', rule]],
				[bd, []],
				['sprouted-context', []],
				
				[ 'seq?',
				  
				  [ 'goal-subgoals-seq?',
				    'source-goal',
				    'source-context'
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['null?', 'source-subgoals'],
				  
				  [ error,
				    '$STRING'("I thought bottoming out was detected in activate-subgoal")
				  ],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Analogical plan for ~A in ~A bottoms out"),
				    'target-goal',
				    'target-context'
				  ],
				  []
				],
				
				[ else,
				  
				  [ setq,
				    'sprouted-context',
				    ['cx$sprout', 'target-context']
				  ],
				  
				  [ 'delay-dbgs',
				    'sprouted-context',
				    
				    [ 'set-ordering',
				      'sprouted-context',
				      ordering
				    ],
				    
				    [ setq,
				      bd,
				      
				      [ 'episodic-unify',
					['ob$get', rule, [quote, goal]],
					'r-subgoal-objs',
					'source-goal-obj',
					'source-subgoals',
					'target-goal',
					'sprouted-context',
					'target-bd',
					'belief-path',
					t,
					'top-level-goal',
					'analogical-episode'
				      ]
				    ],
				    
				    [ if,
				      ['ob?', [car, bd]],
				      
				      [ progn,
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  'rule-xtra',
					  '$STRING'("Resetting target goal from ~A to ~A"),
					  'target-goal',
					  [car, bd]
					],
					[setq, 'target-goal', [car, bd]]
				      ]
				    ],
				    
				    [ if,
				      'reminding?',
				      
				      [ progn,
					
					[ 'epmem-reminding',
					  'analogical-episode',
					  [],
					  []
					],
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("Apply episode ~A"),
					  ['ob->string', 'analogical-episode']
					],
					
					[ 'rule-fire-msg',
					  rule,
					  '$STRING'("analogical plan"),
					  'target-context',
					  bd,
					  'sprouted-context',
					  'target-goal'
					]
				      ],
				      
				      [ progn,
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("Apply suggested episode ~A"),
					  ['ob->string', 'analogical-episode']
					],
					
					[ 'rule-fire-msg',
					  rule,
					  '$STRING'("analogical plan"),
					  'target-context',
					  bd,
					  'sprouted-context',
					  'target-goal'
					],
					['ndbg-newline', '*gate-dbg*', rule]
				      ]
				    ],
				    
				    [ 'instan-and-activate-subgoals',
				      'target-goal',
				      'r-subgoal-objs',
				      bd,
				      rule,
				      'sprouted-context',
				      'seq?',
				      'source-subgoals',
				      [],
				      'top-level-goal',
				      'belief-path'
				    ]
				  ],
				  [list, 'sprouted-context']
				]
			      ]
			    ]
			  ]).

% annotating U::RUN-ANALOGICAL-PLAN 
wl: lambda_def(defun,
	      u_run_analogical_plan,
	      f_u_run_analogical_plan,
	      
	      [ u_target_goal,
		u_target_goal_obj,
		u_target_context,
		u_target_bd,
		u_source_goal,
		u_source_context,
		u_rule,
		u_ordering,
		u_belief_path,
		u_top_level_goal,
		u_analogical_episode,
		u_reminding_c63
	      ],
	      
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
		  u_target_goal,
		  u_target_context
		],
		
		[ let_xx,
		  
		  [ 
		    [ u_source_subgoals,
		      
		      [ u_goal_subgoals,
			u_source_goal,
			u_source_context,
			u_xx_me_belief_path_xx
		      ]
		    ],
		    
		    [ u_source_goal_obj,
		      [u_ob_c36_get, u_source_goal, [quote, u_obj]]
		    ],
		    [u_r_subgoal_objs, [u_rule_subgoal_objs, u_rule]],
		    [u_bd, []],
		    [u_sprouted_context, []],
		    
		    [ u_seq_c63,
		      [u_goal_subgoals_seq_c63, u_source_goal, u_source_context]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_null_c63, u_source_subgoals],
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('I'),
				   #\(' '),
				   #\(t),
				   #\(h),
				   #\(o),
				   #\(u),
				   #\(g),
				   #\(h),
				   #\(t),
				   #\(' '),
				   #\(b),
				   #\(o),
				   #\(t),
				   #\(t),
				   #\(o),
				   #\(m),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(o),
				   #\(u),
				   #\(t),
				   #\(' '),
				   #\(w),
				   #\(a),
				   #\(s),
				   #\(' '),
				   #\(d),
				   #\(e),
				   #\(t),
				   #\(e),
				   #\(c),
				   #\(t),
				   #\(e),
				   #\(d),
				   #\(' '),
				   #\(i),
				   #\(n),
				   #\(' '),
				   #\(a),
				   #\(c),
				   #\(t),
				   #\(i),
				   #\(v),
				   #\(a),
				   #\(t),
				   #\(e),
				   #\(-),
				   #\(s),
				   #\(u),
				   #\(b),
				   #\(g),
				   #\(o),
				   #\(a),
				   #\(l)
				 ])
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
				   #\(b),
				   #\(o),
				   #\(t),
				   #\(t),
				   #\(o),
				   #\(m),
				   #\(s),
				   #\(' '),
				   #\(o),
				   #\(u),
				   #\(t)
				 ]),
			u_target_goal,
			u_target_context
		      ],
		      []
		    ],
		    
		    [ u_else,
		      
		      [ setq,
			u_sprouted_context,
			[u_cx_c36_sprout, u_target_context]
		      ],
		      
		      [ u_delay_dbgs,
			u_sprouted_context,
			[u_set_ordering, u_sprouted_context, u_ordering],
			
			[ setq,
			  u_bd,
			  
			  [ u_episodic_unify,
			    [u_ob_c36_get, u_rule, [quote, u_goal]],
			    u_r_subgoal_objs,
			    u_source_goal_obj,
			    u_source_subgoals,
			    u_target_goal,
			    u_sprouted_context,
			    u_target_bd,
			    u_belief_path,
			    t,
			    u_top_level_goal,
			    u_analogical_episode
			  ]
			],
			
			[ if,
			  [u_ob_c63, [car, u_bd]],
			  
			  [ progn,
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule_xtra,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('R'),
					 #\(e),
					 #\(s),
					 #\(e),
					 #\(t),
					 #\(t),
					 #\(i),
					 #\(n),
					 #\(g),
					 #\(' '),
					 #\(t),
					 #\(a),
					 #\(r),
					 #\(g),
					 #\(e),
					 #\(t),
					 #\(' '),
					 #\(g),
					 #\(o),
					 #\(a),
					 #\(l),
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
			      u_target_goal,
			      [car, u_bd]
			    ],
			    [setq, u_target_goal, [car, u_bd]]
			  ]
			],
			
			[ if,
			  u_reminding_c63,
			  
			  [ progn,
			    [u_epmem_reminding, u_analogical_episode, [], []],
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('A'),
					 #\(p),
					 #\(p),
					 #\(l),
					 #\(y),
					 #\(' '),
					 #\(e),
					 #\(p),
					 #\(i),
					 #\(s),
					 #\(o),
					 #\(d),
					 #\(e),
					 #\(' '),
					 #\(~),
					 #\('A')
				       ]),
			      [u_ob_c62_string, u_analogical_episode]
			    ],
			    
			    [ u_rule_fire_msg,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(a),
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
					 #\(n)
				       ]),
			      u_target_context,
			      u_bd,
			      u_sprouted_context,
			      u_target_goal
			    ]
			  ],
			  
			  [ progn,
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('A'),
					 #\(p),
					 #\(p),
					 #\(l),
					 #\(y),
					 #\(' '),
					 #\(s),
					 #\(u),
					 #\(g),
					 #\(g),
					 #\(e),
					 #\(s),
					 #\(t),
					 #\(e),
					 #\(d),
					 #\(' '),
					 #\(e),
					 #\(p),
					 #\(i),
					 #\(s),
					 #\(o),
					 #\(d),
					 #\(e),
					 #\(' '),
					 #\(~),
					 #\('A')
				       ]),
			      [u_ob_c62_string, u_analogical_episode]
			    ],
			    
			    [ u_rule_fire_msg,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(a),
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
					 #\(n)
				       ]),
			      u_target_context,
			      u_bd,
			      u_sprouted_context,
			      u_target_goal
			    ],
			    [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]
			  ]
			],
			
			[ u_instan_and_activate_subgoals,
			  u_target_goal,
			  u_r_subgoal_objs,
			  u_bd,
			  u_rule,
			  u_sprouted_context,
			  u_seq_c63,
			  u_source_subgoals,
			  [],
			  u_top_level_goal,
			  u_belief_path
			]
		      ],
		      [list, u_sprouted_context]
		    ]
		  ]
		]
	      ]).


% annotating U::RUN-ANALOGICAL-PLAN 
wl: arglist_info(u_run_analogical_plan,
		
		[ u_target_goal,
		  u_target_goal_obj,
		  u_target_context,
		  u_target_bd,
		  u_source_goal,
		  u_source_context,
		  u_rule,
		  u_ordering,
		  u_belief_path,
		  u_top_level_goal,
		  u_analogical_episode,
		  u_reminding_c63
		],
		
		[ Target_goal_Param,
		  Target_goal_obj_Param,
		  Target_context_Param,
		  Target_bd_Param,
		  Source_goal_Param,
		  Source_context_Param,
		  Rule_Param,
		  Ordering_Param,
		  Belief_path_Param,
		  Top_level_goal_Param,
		  Analogical_episode_Param,
		  Reminding_c63_Param
		],
		arginfo{ all:
			     [ u_target_goal,
			       u_target_goal_obj,
			       u_target_context,
			       u_target_bd,
			       u_source_goal,
			       u_source_context,
			       u_rule,
			       u_ordering,
			       u_belief_path,
			       u_top_level_goal,
			       u_analogical_episode,
			       u_reminding_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_target_goal,
				 u_target_goal_obj,
				 u_target_context,
				 u_target_bd,
				 u_source_goal,
				 u_source_context,
				 u_rule,
				 u_ordering,
				 u_belief_path,
				 u_top_level_goal,
				 u_analogical_episode,
				 u_reminding_c63
			       ],
			 opt:0,
			 req:
			     [ u_target_goal,
			       u_target_goal_obj,
			       u_target_context,
			       u_target_bd,
			       u_source_goal,
			       u_source_context,
			       u_rule,
			       u_ordering,
			       u_belief_path,
			       u_top_level_goal,
			       u_analogical_episode,
			       u_reminding_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-ANALOGICAL-PLAN 
wl: init_args(exact_only, u_run_analogical_plan).


% annotating U::RUN-ANALOGICAL-PLAN 
f_u_run_analogical_plan(Target_goal_Param, Target_goal_obj_Param, Target_context_Param, Target_bd_Param, Source_goal_Param, Source_context_Param, Rule_Param, Ordering_Param, Belief_path_Param, Top_level_goal_Param, Analogical_episode_Param, Reminding_c63_Param, ElseResult70) :-
	Env=[bv(u_target_goal, Target_goal_Param), bv(u_target_goal_obj, Target_goal_obj_Param), bv(u_target_context, Target_context_Param), bv(u_target_bd, Target_bd_Param), bv(u_source_goal, Source_goal_Param), bv(u_source_context, Source_context_Param), bv(u_rule, Rule_Param), bv(u_ordering, Ordering_Param), bv(u_belief_path, Belief_path_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_analogical_episode, Analogical_episode_Param), bv(u_reminding_c63, Reminding_c63_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
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
			    u_target_goal,
			    u_target_context
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_goal_subgoals(Source_goal_Param,
			  Source_context_Param,
			  Xx_me_belief_path_xx_Get,
			  Source_subgoals_Init),
	LEnv=[[bv(u_source_subgoals, Source_subgoals_Init)]|Env],
	f_u_ob_c36_get(Source_goal_Param, u_obj, Source_goal_obj_Init),
	Env=[[bv(u_source_goal_obj, Source_goal_obj_Init)]|LEnv],
	f_u_rule_subgoal_objs(Rule_Param, R_subgoal_objs_Init),
	LEnv45=[[bv(u_r_subgoal_objs, R_subgoal_objs_Init)]|Env],
	LEnv50=[[bv(u_bd, [])]|LEnv45],
	Env=[[bv(u_sprouted_context, [])]|LEnv50],
	f_u_goal_subgoals_seq_c63(Source_goal_Param,
				  Source_context_Param,
				  Seq_c63_Init),
	Env=[[bv(u_seq_c63, Seq_c63_Init)]|Env],
	f_u_null_c63(u_source_subgoals, IFTEST),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('I'),
				  #\(' '),
				  #\(t),
				  #\(h),
				  #\(o),
				  #\(u),
				  #\(g),
				  #\(h),
				  #\(t),
				  #\(' '),
				  #\(b),
				  #\(o),
				  #\(t),
				  #\(t),
				  #\(o),
				  #\(m),
				  #\(i),
				  #\(n),
				  #\(g),
				  #\(' '),
				  #\(o),
				  #\(u),
				  #\(t),
				  #\(' '),
				  #\(w),
				  #\(a),
				  #\(s),
				  #\(' '),
				  #\(d),
				  #\(e),
				  #\(t),
				  #\(e),
				  #\(c),
				  #\(t),
				  #\(e),
				  #\(d),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(a),
				  #\(c),
				  #\(t),
				  #\(i),
				  #\(v),
				  #\(a),
				  #\(t),
				  #\(e),
				  #\(-),
				  #\(s),
				  #\(u),
				  #\(b),
				  #\(g),
				  #\(o),
				  #\(a),
				  #\(l)
				])
		     ],
		     Error_Ret),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
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
					   #\(b),
					   #\(o),
					   #\(t),
					   #\(t),
					   #\(o),
					   #\(m),
					   #\(s),
					   #\(' '),
					   #\(o),
					   #\(u),
					   #\(t)
					 ]),
				u_target_goal,
				u_target_context
			      ],
			      Roman_nl_Ret81),
	    ElseResult70=[]
	;   get_var(Env, u_else, IFTEST62),
	    (   IFTEST62\==[]
	    ->  f_u_cx_c36_sprout(Target_context_Param, Sprouted_context),
		set_var(Env, u_sprouted_context, Sprouted_context),
		f_u_delay_dbgs(u_sprouted_context,
			       
			       [ 
				 [ u_set_ordering,
				   u_sprouted_context,
				   u_ordering
				 ],
				 
				 [ setq,
				   u_bd,
				   
				   [ u_episodic_unify,
				     [u_ob_c36_get, u_rule, [quote, u_goal]],
				     u_r_subgoal_objs,
				     u_source_goal_obj,
				     u_source_subgoals,
				     u_target_goal,
				     u_sprouted_context,
				     u_target_bd,
				     u_belief_path,
				     t,
				     u_top_level_goal,
				     u_analogical_episode
				   ]
				 ],
				 
				 [ if,
				   [u_ob_c63, [car, u_bd]],
				   
				   [ progn,
				     
				     [ u_ndbg_roman_nl,
				       u_xx_gate_dbg_xx,
				       u_rule_xtra,
				       '$ARRAY'([*],
						claz_base_character,
						
						[ #\('R'),
						  #\(e),
						  #\(s),
						  #\(e),
						  #\(t),
						  #\(t),
						  #\(i),
						  #\(n),
						  #\(g),
						  #\(' '),
						  #\(t),
						  #\(a),
						  #\(r),
						  #\(g),
						  #\(e),
						  #\(t),
						  #\(' '),
						  #\(g),
						  #\(o),
						  #\(a),
						  #\(l),
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
				       u_target_goal,
				       [car, u_bd]
				     ],
				     [setq, u_target_goal, [car, u_bd]]
				   ]
				 ],
				 
				 [ if,
				   u_reminding_c63,
				   
				   [ progn,
				     
				     [ u_epmem_reminding,
				       u_analogical_episode,
				       [],
				       []
				     ],
				     
				     [ u_ndbg_roman_nl,
				       u_xx_gate_dbg_xx,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						
						[ #\('A'),
						  #\(p),
						  #\(p),
						  #\(l),
						  #\(y),
						  #\(' '),
						  #\(e),
						  #\(p),
						  #\(i),
						  #\(s),
						  #\(o),
						  #\(d),
						  #\(e),
						  #\(' '),
						  #\(~),
						  #\('A')
						]),
				       [u_ob_c62_string, u_analogical_episode]
				     ],
				     
				     [ u_rule_fire_msg,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						
						[ #\(a),
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
						  #\(n)
						]),
				       u_target_context,
				       u_bd,
				       u_sprouted_context,
				       u_target_goal
				     ]
				   ],
				   
				   [ progn,
				     
				     [ u_ndbg_roman_nl,
				       u_xx_gate_dbg_xx,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						
						[ #\('A'),
						  #\(p),
						  #\(p),
						  #\(l),
						  #\(y),
						  #\(' '),
						  #\(s),
						  #\(u),
						  #\(g),
						  #\(g),
						  #\(e),
						  #\(s),
						  #\(t),
						  #\(e),
						  #\(d),
						  #\(' '),
						  #\(e),
						  #\(p),
						  #\(i),
						  #\(s),
						  #\(o),
						  #\(d),
						  #\(e),
						  #\(' '),
						  #\(~),
						  #\('A')
						]),
				       [u_ob_c62_string, u_analogical_episode]
				     ],
				     
				     [ u_rule_fire_msg,
				       u_rule,
				       '$ARRAY'([*],
						claz_base_character,
						
						[ #\(a),
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
						  #\(n)
						]),
				       u_target_context,
				       u_bd,
				       u_sprouted_context,
				       u_target_goal
				     ],
				     [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]
				   ]
				 ],
				 
				 [ u_instan_and_activate_subgoals,
				   u_target_goal,
				   u_r_subgoal_objs,
				   u_bd,
				   u_rule,
				   u_sprouted_context,
				   u_seq_c63,
				   u_source_subgoals,
				   [],
				   u_top_level_goal,
				   u_belief_path
				 ]
			       ],
			       Delay_dbgs_Ret),
		get_var(Env, u_sprouted_context, Sprouted_context_Get),
		ElseResult70=[Sprouted_context_Get]
	    ;   ElseResult70=[]
	    )
	).
:- set_opv(f_u_run_analogical_plan, classof, claz_function),
   set_opv(u_run_analogical_plan, compile_as, kw_function),
   set_opv(u_run_analogical_plan, function, f_u_run_analogical_plan),
   DefunResult=u_run_analogical_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:27540 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" was target context", 47, 28914)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:30137 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'episodic-unify',
			    
			    [ 'rule-goal-obj',
			      'r-subgoal-objs',
			      'source-goal-obj',
			      'source-subgoals',
			      'target-goal',
			      'target-context',
			      'target-bd',
			      'belief-path',
			      'disallow-failures?',
			      'top-level-goal',
			      episode
			    ],
			    
			    [ let,
			      
			      [ 
				[ bd,
				  
				  [ 'ob$unify',
				    'rule-goal-obj',
				    'source-goal-obj',
				    ['planner-empty-bd', 'belief-path']
				  ]
				],
				
				[ 'seren-ep',
				  
				  [ if,
				    episode,
				    ['ob$get', episode, [quote, 'seren-ep']],
				    []
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['null?', bd],
				  
				  [ if,
				    'disallow-failures?',
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("~A does not correspond with ~A"),
					'rule-goal-obj',
					'source-goal-obj'
				      ],
				      
				      [ error,
					'$STRING'("bd nil in analogical plan!!!")
				      ],
				      []
				    ],
				    []
				  ]
				],
				
				[ else,
				  
				  [ yloop,
				    
				    [ initial,
				      ['source-subgoalsx', 'source-subgoals']
				    ],
				    
				    [ yfor,
				      'rule-subgoal-obj',
				      in,
				      'r-subgoal-objs'
				    ],
				    [yuntil, ['null?', bd]],
				    
				    [ ydo,
				      
				      [ setq,
					bd,
					
					[ 'ob$unify',
					  'rule-subgoal-obj',
					  
					  [ 'ob$get',
					    [car, 'source-subgoalsx'],
					    [quote, obj]
					  ],
					  bd
					]
				      ],
				      
				      [ if,
					
					[ and,
					  ['null?', bd],
					  'disallow-failures?'
					],
					
					[ progn,
					  
					  [ 'ndbg-roman-nl',
					    '*gate-dbg*',
					    rule,
					    '$STRING'("~A does not correspond with ~A"),
					    'rule-subgoal-obj',
					    
					    [ 'ob$get',
					      [car, 'source-subgoalsx'],
					      [quote, obj]
					    ]
					  ],
					  
					  [ error,
					    '$STRING'("bd nil in analogical plan!!")
					  ]
					]
				      ],
				      
				      [ setq,
					'source-subgoalsx',
					[cdr, 'source-subgoalsx']
				      ]
				    ]
				  ],
				  
				  [ if,
				    ['null?', bd],
				    [],
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					analogy,
					'$STRING'("Target-bd:")
				      ],
				      
				      [ 'if-interested-in',
					analogy,
					['bd-print', 'target-bd', '*gate-dbg*']
				      ],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					analogy,
					'$STRING'("Bd:")
				      ],
				      
				      [ 'if-interested-in',
					analogy,
					['bd-print', bd, '*gate-dbg*']
				      ],
				      
				      [ 'bd-special-append',
					'target-bd',
					bd,
					'target-goal',
					'target-context',
					'belief-path',
					'top-level-goal',
					[],
					
					[ lambda,
					  [x],
					  
					  [ 'analogy-instantiatible1?',
					    x,
					    'seren-ep'
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::EPISODIC-UNIFY 
wl: lambda_def(defun,
	      u_episodic_unify,
	      f_u_episodic_unify,
	      
	      [ u_rule_goal_obj,
		u_r_subgoal_objs,
		u_source_goal_obj,
		u_source_subgoals,
		u_target_goal,
		u_target_context,
		u_target_bd,
		u_belief_path,
		u_disallow_failures_c63,
		u_top_level_goal,
		u_episode
	      ],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_bd,
		      
		      [ u_ob_c36_unify,
			u_rule_goal_obj,
			u_source_goal_obj,
			[u_planner_empty_bd, u_belief_path]
		      ]
		    ],
		    
		    [ u_seren_ep,
		      
		      [ if,
			u_episode,
			[u_ob_c36_get, u_episode, [quote, u_seren_ep]],
			[]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_null_c63, u_bd],
		      
		      [ if,
			u_disallow_failures_c63,
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('A'),
				       #\(' '),
				       #\(d),
				       #\(o),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(t),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(r),
				       #\(r),
				       #\(e),
				       #\(s),
				       #\(p),
				       #\(o),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_rule_goal_obj,
			    u_source_goal_obj
			  ],
			  
			  [ error,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(b),
				       #\(d),
				       #\(' '),
				       #\(n),
				       #\(i),
				       #\(l),
				       #\(' '),
				       #\(i),
				       #\(n),
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
				       #\(!),
				       #\(!),
				       #\(!)
				     ])
			  ],
			  []
			],
			[]
		      ]
		    ],
		    
		    [ u_else,
		      
		      [ u_yloop,
			[u_initial, [u_source_subgoalsx, u_source_subgoals]],
			[u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs],
			[u_yuntil, [u_null_c63, u_bd]],
			
			[ u_ydo,
			  
			  [ setq,
			    u_bd,
			    
			    [ u_ob_c36_unify,
			      u_rule_subgoal_obj,
			      
			      [ u_ob_c36_get,
				[car, u_source_subgoalsx],
				[quote, u_obj]
			      ],
			      u_bd
			    ]
			  ],
			  
			  [ if,
			    [and, [u_null_c63, u_bd], u_disallow_failures_c63],
			    
			    [ progn,
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(~),
					   #\('A'),
					   #\(' '),
					   #\(d),
					   #\(o),
					   #\(e),
					   #\(s),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(c),
					   #\(o),
					   #\(r),
					   #\(r),
					   #\(e),
					   #\(s),
					   #\(p),
					   #\(o),
					   #\(n),
					   #\(d),
					   #\(' '),
					   #\(w),
					   #\(i),
					   #\(t),
					   #\(h),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_rule_subgoal_obj,
				
				[ u_ob_c36_get,
				  [car, u_source_subgoalsx],
				  [quote, u_obj]
				]
			      ],
			      
			      [ error,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(b),
					   #\(d),
					   #\(' '),
					   #\(n),
					   #\(i),
					   #\(l),
					   #\(' '),
					   #\(i),
					   #\(n),
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
					   #\(!),
					   #\(!)
					 ])
			      ]
			    ]
			  ],
			  [setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]
			]
		      ],
		      
		      [ if,
			[u_null_c63, u_bd],
			[],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_analogy,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('T'),
				       #\(a),
				       #\(r),
				       #\(g),
				       #\(e),
				       #\(t),
				       #\(-),
				       #\(b),
				       #\(d),
				       #\(:)
				     ])
			  ],
			  
			  [ u_if_interested_in,
			    u_analogy,
			    [u_bd_print, u_target_bd, u_xx_gate_dbg_xx]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_analogy,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\('B'), #\(d), #\(:)])
			  ],
			  
			  [ u_if_interested_in,
			    u_analogy,
			    [u_bd_print, u_bd, u_xx_gate_dbg_xx]
			  ],
			  
			  [ u_bd_special_append,
			    u_target_bd,
			    u_bd,
			    u_target_goal,
			    u_target_context,
			    u_belief_path,
			    u_top_level_goal,
			    [],
			    
			    [ lambda,
			      [u_x],
			      [u_analogy_instantiatible1_c63, u_x, u_seren_ep]
			    ]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::EPISODIC-UNIFY 
wl: arglist_info(u_episodic_unify,
		
		[ u_rule_goal_obj,
		  u_r_subgoal_objs,
		  u_source_goal_obj,
		  u_source_subgoals,
		  u_target_goal,
		  u_target_context,
		  u_target_bd,
		  u_belief_path,
		  u_disallow_failures_c63,
		  u_top_level_goal,
		  u_episode
		],
		
		[ Rule_goal_obj_Param,
		  R_subgoal_objs_Param,
		  Source_goal_obj_Param,
		  Source_subgoals_Param,
		  Target_goal_Param,
		  Target_context_Param,
		  Target_bd_Param,
		  Belief_path_Param,
		  Disallow_failures_c63_Param,
		  Top_level_goal_Param,
		  Episode_Param
		],
		arginfo{ all:
			     [ u_rule_goal_obj,
			       u_r_subgoal_objs,
			       u_source_goal_obj,
			       u_source_subgoals,
			       u_target_goal,
			       u_target_context,
			       u_target_bd,
			       u_belief_path,
			       u_disallow_failures_c63,
			       u_top_level_goal,
			       u_episode
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_rule_goal_obj,
				 u_r_subgoal_objs,
				 u_source_goal_obj,
				 u_source_subgoals,
				 u_target_goal,
				 u_target_context,
				 u_target_bd,
				 u_belief_path,
				 u_disallow_failures_c63,
				 u_top_level_goal,
				 u_episode
			       ],
			 opt:0,
			 req:
			     [ u_rule_goal_obj,
			       u_r_subgoal_objs,
			       u_source_goal_obj,
			       u_source_subgoals,
			       u_target_goal,
			       u_target_context,
			       u_target_bd,
			       u_belief_path,
			       u_disallow_failures_c63,
			       u_top_level_goal,
			       u_episode
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EPISODIC-UNIFY 
wl: init_args(exact_only, u_episodic_unify).


% annotating U::EPISODIC-UNIFY 
f_u_episodic_unify(Rule_goal_obj_Param, R_subgoal_objs_Param, Source_goal_obj_Param, Source_subgoals_Param, Target_goal_Param, Target_context_Param, Target_bd_Param, Belief_path_Param, Disallow_failures_c63_Param, Top_level_goal_Param, Episode_Param, TrueResult66) :-
	Env=[bv(u_rule_goal_obj, Rule_goal_obj_Param), bv(u_r_subgoal_objs, R_subgoal_objs_Param), bv(u_source_goal_obj, Source_goal_obj_Param), bv(u_source_subgoals, Source_subgoals_Param), bv(u_target_goal, Target_goal_Param), bv(u_target_context, Target_context_Param), bv(u_target_bd, Target_bd_Param), bv(u_belief_path, Belief_path_Param), bv(u_disallow_failures_c63, Disallow_failures_c63_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_episode, Episode_Param)],
	f_u_ob_c36_unify(u_rule_goal_obj,
			 u_source_goal_obj,
			 [u_planner_empty_bd, u_belief_path],
			 Bd_Init),
	(   Episode_Param\==[]
	->  f_u_ob_c36_get(Episode_Param, u_seren_ep, TrueResult),
	    Seren_ep_Init=TrueResult
	;   Seren_ep_Init=[]
	),
	LEnv=[[bv(u_bd, Bd_Init), bv(u_seren_ep, Seren_ep_Init)]|Env],
	f_u_null_c63(u_bd, IFTEST41),
	(   IFTEST41\==[]
	->  (   Disallow_failures_c63_Param\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(~),
					       #\('A'),
					       #\(' '),
					       #\(d),
					       #\(o),
					       #\(e),
					       #\(s),
					       #\(' '),
					       #\(n),
					       #\(o),
					       #\(t),
					       #\(' '),
					       #\(c),
					       #\(o),
					       #\(r),
					       #\(r),
					       #\(e),
					       #\(s),
					       #\(p),
					       #\(o),
					       #\(n),
					       #\(d),
					       #\(' '),
					       #\(w),
					       #\(i),
					       #\(t),
					       #\(h),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_rule_goal_obj,
				    u_source_goal_obj
				  ],
				  Roman_nl_Ret),
		cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\(b),
				      #\(d),
				      #\(' '),
				      #\(n),
				      #\(i),
				      #\(l),
				      #\(' '),
				      #\(i),
				      #\(n),
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
				      #\(!),
				      #\(!),
				      #\(!)
				    ])
			 ],
			 Error_Ret),
		TrueResult66=[]
	    ;   TrueResult66=[]
	    )
	;   get_var(LEnv, u_else, IFTEST47),
	    (   IFTEST47\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [u_source_subgoalsx, u_source_subgoals]],
			    [u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs],
			    [u_yuntil, [u_null_c63, u_bd]],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_bd,
				
				[ u_ob_c36_unify,
				  u_rule_subgoal_obj,
				  
				  [ u_ob_c36_get,
				    [car, u_source_subgoalsx],
				    [quote, u_obj]
				  ],
				  u_bd
				]
			      ],
			      
			      [ if,
				
				[ and,
				  [u_null_c63, u_bd],
				  u_disallow_failures_c63
				],
				
				[ progn,
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_rule,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(~),
					       #\('A'),
					       #\(' '),
					       #\(d),
					       #\(o),
					       #\(e),
					       #\(s),
					       #\(' '),
					       #\(n),
					       #\(o),
					       #\(t),
					       #\(' '),
					       #\(c),
					       #\(o),
					       #\(r),
					       #\(r),
					       #\(e),
					       #\(s),
					       #\(p),
					       #\(o),
					       #\(n),
					       #\(d),
					       #\(' '),
					       #\(w),
					       #\(i),
					       #\(t),
					       #\(h),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_rule_subgoal_obj,
				    
				    [ u_ob_c36_get,
				      [car, u_source_subgoalsx],
				      [quote, u_obj]
				    ]
				  ],
				  
				  [ error,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(b),
					       #\(d),
					       #\(' '),
					       #\(n),
					       #\(i),
					       #\(l),
					       #\(' '),
					       #\(i),
					       #\(n),
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
					       #\(!),
					       #\(!)
					     ])
				  ]
				]
			      ],
			      
			      [ setq,
				u_source_subgoalsx,
				[cdr, u_source_subgoalsx]
			      ]
			    ]
			  ],
			  Yloop_Ret),
		f_u_null_c63(u_bd, IFTEST50),
		(   IFTEST50\==[]
		->  TrueResult66=[]
		;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				      u_analogy,
				      
				      [ '$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('T'),
						   #\(a),
						   #\(r),
						   #\(g),
						   #\(e),
						   #\(t),
						   #\(-),
						   #\(b),
						   #\(d),
						   #\(:)
						 ])
				      ],
				      Roman_nl_Ret75),
		    f_u_if_interested_in(u_analogy,
					 
					 [ 
					   [ u_bd_print,
					     u_target_bd,
					     u_xx_gate_dbg_xx
					   ]
					 ],
					 Interested_in_Ret),
		    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				      u_analogy,
				      
				      [ '$ARRAY'([*],
						 claz_base_character,
						 [#\('B'), #\(d), #\(:)])
				      ],
				      Roman_nl_Ret77),
		    f_u_if_interested_in(u_analogy,
					 [[u_bd_print, u_bd, u_xx_gate_dbg_xx]],
					 Interested_in_Ret78),
		    get_var(LEnv, u_bd, Bd_Get),
		    Lambda=closure([Env|LEnv], LResult, [u_x],  (get_var(Env, u_seren_ep, Seren_ep_Get), get_var(Env, u_x, X_Get), f_u_analogy_instantiatible1_c63(X_Get, Seren_ep_Get, LResult))),
		    f_u_bd_special_append(Target_bd_Param,
					  Bd_Get,
					  Target_goal_Param,
					  Target_context_Param,
					  Belief_path_Param,
					  Top_level_goal_Param,
					  [],
					  Lambda,
					  ElseResult),
		    TrueResult66=ElseResult
		)
	    ;   TrueResult66=[]
	    )
	).
:- set_opv(f_u_episodic_unify, classof, claz_function),
   set_opv(u_episodic_unify, compile_as, kw_function),
   set_opv(u_episodic_unify, function, f_u_episodic_unify),
   DefunResult=u_episodic_unify.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:30137 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" In following, order has to be not reversed.",
				     5,
				     30867)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:30137 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Verification function for reading in episodes.",
				     1,
				     32001)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:32049 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'check-episodic-plan',
			    [rule, 'source-goal-obj', 'source-subgoals'],
			    
			    [ 'let*',
			      
			      [ ['rule-goal-obj', ['ob$get', rule, [quote, goal]]],
				['r-subgoal-objs', ['rule-subgoal-objs', rule]],
				
				[ bd,
				  
				  [ 'ob$unify',
				    'rule-goal-obj',
				    'source-goal-obj',
				    ['planner-empty-bd', '*me-belief-path*']
				  ]
				]
			      ],
			      
			      [ if,
				['null?', bd],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Warning: ~A does not correspond with ~A, rule ~A"),
				  'rule-goal-obj',
				  'source-goal-obj',
				  rule
				],
				
				[ yloop,
				  
				  [ initial,
				    ['source-subgoalsx', 'source-subgoals']
				  ],
				  
				  [ yfor,
				    'rule-subgoal-obj',
				    in,
				    'r-subgoal-objs'
				  ],
				  [yuntil, ['null?', bd]],
				  
				  [ ydo,
				    
				    [ setq,
				      bd,
				      
				      [ 'ob$unify',
					'rule-subgoal-obj',
					
					[ 'ob$get',
					  [car, 'source-subgoalsx'],
					  [quote, obj]
					],
					bd
				      ]
				    ],
				    
				    [ if,
				      ['null?', bd],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("Warning: ~A does not correspond with ~A, rule ~A"),
					'rule-subgoal-obj',
					
					[ 'ob$get',
					  [car, 'source-subgoalsx'],
					  [quote, obj]
					],
					rule
				      ]
				    ],
				    
				    [ setq,
				      'source-subgoalsx',
				      [cdr, 'source-subgoalsx']
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::CHECK-EPISODIC-PLAN 
wl: lambda_def(defun,
	      u_check_episodic_plan,
	      f_u_check_episodic_plan,
	      [u_rule, u_source_goal_obj, u_source_subgoals],
	      
	      [ 
		[ let_xx,
		  
		  [ [u_rule_goal_obj, [u_ob_c36_get, u_rule, [quote, u_goal]]],
		    [u_r_subgoal_objs, [u_rule_subgoal_objs, u_rule]],
		    
		    [ u_bd,
		      
		      [ u_ob_c36_unify,
			u_rule_goal_obj,
			u_source_goal_obj,
			[u_planner_empty_bd, u_xx_me_belief_path_xx]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, u_bd],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
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
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(d),
				 #\(o),
				 #\(e),
				 #\(s),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(c),
				 #\(o),
				 #\(r),
				 #\(r),
				 #\(e),
				 #\(s),
				 #\(p),
				 #\(o),
				 #\(n),
				 #\(d),
				 #\(' '),
				 #\(w),
				 #\(i),
				 #\(t),
				 #\(h),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(','),
				 #\(' '),
				 #\(r),
				 #\(u),
				 #\(l),
				 #\(e),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_rule_goal_obj,
		      u_source_goal_obj,
		      u_rule
		    ],
		    
		    [ u_yloop,
		      [u_initial, [u_source_subgoalsx, u_source_subgoals]],
		      [u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs],
		      [u_yuntil, [u_null_c63, u_bd]],
		      
		      [ u_ydo,
			
			[ setq,
			  u_bd,
			  
			  [ u_ob_c36_unify,
			    u_rule_subgoal_obj,
			    
			    [ u_ob_c36_get,
			      [car, u_source_subgoalsx],
			      [quote, u_obj]
			    ],
			    u_bd
			  ]
			],
			
			[ if,
			  [u_null_c63, u_bd],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
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
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(d),
				       #\(o),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(t),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(r),
				       #\(r),
				       #\(e),
				       #\(s),
				       #\(p),
				       #\(o),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(r),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_rule_subgoal_obj,
			    
			    [ u_ob_c36_get,
			      [car, u_source_subgoalsx],
			      [quote, u_obj]
			    ],
			    u_rule
			  ]
			],
			[setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::CHECK-EPISODIC-PLAN 
wl: arglist_info(u_check_episodic_plan,
		[u_rule, u_source_goal_obj, u_source_subgoals],
		[Rule_Param, Source_goal_obj_Param, Source_subgoals_Param],
		arginfo{ all:[u_rule, u_source_goal_obj, u_source_subgoals],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_source_goal_obj, u_source_subgoals],
			 opt:0,
			 req:[u_rule, u_source_goal_obj, u_source_subgoals],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CHECK-EPISODIC-PLAN 
wl: init_args(exact_only, u_check_episodic_plan).


% annotating U::CHECK-EPISODIC-PLAN 
f_u_check_episodic_plan(Rule_Param, Source_goal_obj_Param, Source_subgoals_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_source_goal_obj, Source_goal_obj_Param), bv(u_source_subgoals, Source_subgoals_Param)],
	f_u_ob_c36_get(Rule_Param, u_goal, Rule_goal_obj_Init),
	LEnv=[[bv(u_rule_goal_obj, Rule_goal_obj_Init)]|Env],
	f_u_rule_subgoal_objs(Rule_Param, R_subgoal_objs_Init),
	LEnv20=[[bv(u_r_subgoal_objs, R_subgoal_objs_Init)]|LEnv],
	f_u_ob_c36_unify(u_rule_goal_obj,
			 u_source_goal_obj,
			 [u_planner_empty_bd, u_xx_me_belief_path_xx],
			 Bd_Init),
	LEnv25=[[bv(u_bd, Bd_Init)]|LEnv20],
	f_u_null_c63(u_bd, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
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
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(d),
					   #\(o),
					   #\(e),
					   #\(s),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(c),
					   #\(o),
					   #\(r),
					   #\(r),
					   #\(e),
					   #\(s),
					   #\(p),
					   #\(o),
					   #\(n),
					   #\(d),
					   #\(' '),
					   #\(w),
					   #\(i),
					   #\(t),
					   #\(h),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(','),
					   #\(' '),
					   #\(r),
					   #\(u),
					   #\(l),
					   #\(e),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_rule_goal_obj,
				u_source_goal_obj,
				u_rule
			      ],
			      TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ [u_initial, [u_source_subgoalsx, u_source_subgoals]],
			[u_yfor, u_rule_subgoal_obj, u_in, u_r_subgoal_objs],
			[u_yuntil, [u_null_c63, u_bd]],
			
			[ u_ydo,
			  
			  [ setq,
			    u_bd,
			    
			    [ u_ob_c36_unify,
			      u_rule_subgoal_obj,
			      
			      [ u_ob_c36_get,
				[car, u_source_subgoalsx],
				[quote, u_obj]
			      ],
			      u_bd
			    ]
			  ],
			  
			  [ if,
			    [u_null_c63, u_bd],
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
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
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(d),
					 #\(o),
					 #\(e),
					 #\(s),
					 #\(' '),
					 #\(n),
					 #\(o),
					 #\(t),
					 #\(' '),
					 #\(c),
					 #\(o),
					 #\(r),
					 #\(r),
					 #\(e),
					 #\(s),
					 #\(p),
					 #\(o),
					 #\(n),
					 #\(d),
					 #\(' '),
					 #\(w),
					 #\(i),
					 #\(t),
					 #\(h),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(','),
					 #\(' '),
					 #\(r),
					 #\(u),
					 #\(l),
					 #\(e),
					 #\(' '),
					 #\(~),
					 #\('A')
				       ]),
			      u_rule_subgoal_obj,
			      
			      [ u_ob_c36_get,
				[car, u_source_subgoalsx],
				[quote, u_obj]
			      ],
			      u_rule
			    ]
			  ],
			  [setq, u_source_subgoalsx, [cdr, u_source_subgoalsx]]
			]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_check_episodic_plan, classof, claz_function),
   set_opv(u_check_episodic_plan, compile_as, kw_function),
   set_opv(u_check_episodic_plan, function, f_u_check_episodic_plan),
   DefunResult=u_check_episodic_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33240 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'generate-episode',
			    [episode],
			    
			    [ generate1,
			      episode,
			      '*global-switches*',
			      ['ob$get', episode, [quote, context]],
			      '*me-belief-path*'
			    ]
			  ]).

% annotating U::GENERATE-EPISODE 
wl: lambda_def(defun,
	      u_generate_episode,
	      f_u_generate_episode,
	      [u_episode],
	      
	      [ 
		[ u_generate1,
		  u_episode,
		  u_xx_global_switches_xx,
		  [u_ob_c36_get, u_episode, [quote, u_context]],
		  u_xx_me_belief_path_xx
		]
	      ]).


% annotating U::GENERATE-EPISODE 
wl: arglist_info(u_generate_episode,
		[u_episode],
		[Episode_Param],
		arginfo{ all:[u_episode],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_episode],
			 opt:0,
			 req:[u_episode],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GENERATE-EPISODE 
wl: init_args(exact_only, u_generate_episode).


% annotating U::GENERATE-EPISODE 
f_u_generate_episode(Episode_Param, FnResult) :-
	Env=[bv(u_episode, Episode_Param)],
	get_var(Env, u_xx_global_switches_xx, Xx_global_switches_xx_Get),
	f_u_ob_c36_get(Episode_Param, u_context, Context),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_generate1(Episode_Param,
		      Xx_global_switches_xx_Get,
		      Context,
		      Xx_me_belief_path_xx_Get,
		      Generate1_Ret),
	Generate1_Ret=FnResult.
:- set_opv(f_u_generate_episode, classof, claz_function),
   set_opv(u_generate_episode, compile_as, kw_function),
   set_opv(u_generate_episode, function, f_u_generate_episode),
   DefunResult=u_generate_episode.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33372 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'task-print-plans',
			    [tlg],
			    
			    [ yloop,
			      [initial, ['continue?', t]],
			      
			      [ yfor,
				leaf,
				in,
				
				[ 'cx$leaf-descendants',
				  ['get-backtrack-wall', tlg]
				]
			      ],
			      [ywhile, 'continue?'],
			      
			      [ ydo,
				
				[ format,
				  '*gate-dbg*',
				  '$STRING'("Leaf context ~A~%"),
				  leaf
				],
				['plan-print', tlg, leaf],
				[setq, 'continue?', t]
			      ],
			      [yresult, []]
			    ]
			  ]).

% annotating U::TASK-PRINT-PLANS 
wl: lambda_def(defun,
	      u_task_print_plans,
	      f_u_task_print_plans,
	      [u_tlg],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_continue_c63, t]],
		  
		  [ u_yfor,
		    u_leaf,
		    u_in,
		    [u_cx_c36_leaf_descendants, [u_get_backtrack_wall, u_tlg]]
		  ],
		  [u_ywhile, u_continue_c63],
		  
		  [ u_ydo,
		    
		    [ format,
		      u_xx_gate_dbg_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('L'),
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
				 #\(~),
				 #\('%')
			       ]),
		      u_leaf
		    ],
		    [u_plan_print, u_tlg, u_leaf],
		    [setq, u_continue_c63, t]
		  ],
		  [u_yresult, []]
		]
	      ]).


% annotating U::TASK-PRINT-PLANS 
wl: arglist_info(u_task_print_plans,
		[u_tlg],
		[Tlg_Param],
		arginfo{ all:[u_tlg],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_tlg],
			 opt:0,
			 req:[u_tlg],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TASK-PRINT-PLANS 
wl: init_args(exact_only, u_task_print_plans).


% annotating U::TASK-PRINT-PLANS 
f_u_task_print_plans(Tlg_Param, FnResult) :-
	Env=[bv(u_tlg, Tlg_Param)],
	f_u_yloop(
		  [ [u_initial, [u_continue_c63, t]],
		    
		    [ u_yfor,
		      u_leaf,
		      u_in,
		      [u_cx_c36_leaf_descendants, [u_get_backtrack_wall, u_tlg]]
		    ],
		    [u_ywhile, u_continue_c63],
		    
		    [ u_ydo,
		      
		      [ format,
			u_xx_gate_dbg_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('L'),
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
				   #\(~),
				   #\('%')
				 ]),
			u_leaf
		      ],
		      [u_plan_print, u_tlg, u_leaf],
		      [setq, u_continue_c63, t]
		    ],
		    [u_yresult, []]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_task_print_plans, classof, claz_function),
   set_opv(u_task_print_plans, compile_as, kw_function),
   set_opv(u_task_print_plans, function, f_u_task_print_plans),
   DefunResult=u_task_print_plans.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33372 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (setq continue? (interrogate \"More? \"))",
				     1,
				     33625)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33734 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ep-print',
			    [episode],
			    
			    [ 'plan-print1',
			      ['ob$get', episode, [quote, goal]],
			      ['ob$get', episode, [quote, context]],
			      '*gate-dbg*',
			      0
			    ]
			  ]).

% annotating U::EP-PRINT 
wl: lambda_def(defun,
	      u_ep_print,
	      f_u_ep_print,
	      [u_episode],
	      
	      [ 
		[ u_plan_print1,
		  [u_ob_c36_get, u_episode, [quote, u_goal]],
		  [u_ob_c36_get, u_episode, [quote, u_context]],
		  u_xx_gate_dbg_xx,
		  0
		]
	      ]).


% annotating U::EP-PRINT 
wl: arglist_info(u_ep_print,
		[u_episode],
		[Episode_Param],
		arginfo{ all:[u_episode],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_episode],
			 opt:0,
			 req:[u_episode],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EP-PRINT 
wl: init_args(exact_only, u_ep_print).


% annotating U::EP-PRINT 
f_u_ep_print(Episode_Param, FnResult) :-
	Env=[bv(u_episode, Episode_Param)],
	f_u_ob_c36_get(Episode_Param, u_goal, Goal),
	f_u_ob_c36_get(Episode_Param, u_context, Context),
	get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	f_u_plan_print1(Goal, Context, Xx_gate_dbg_xx_Get, 0, Plan_print1_Ret),
	Plan_print1_Ret=FnResult.
:- set_opv(f_u_ep_print, classof, claz_function),
   set_opv(u_ep_print, compile_as, kw_function),
   set_opv(u_ep_print, function, f_u_ep_print),
   DefunResult=u_ep_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33840 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'plan-print',
			    [goal, context],
			    ['plan-print1', goal, context, '*gate-dbg*', 0]
			  ]).

% annotating U::PLAN-PRINT 
wl: lambda_def(defun,
	      u_plan_print,
	      f_u_plan_print,
	      [u_goal, u_context],
	      [[u_plan_print1, u_goal, u_context, u_xx_gate_dbg_xx, 0]]).


% annotating U::PLAN-PRINT 
wl: arglist_info(u_plan_print,
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

% annotating U::PLAN-PRINT 
wl: init_args(exact_only, u_plan_print).


% annotating U::PLAN-PRINT 
f_u_plan_print(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	f_u_plan_print1(Goal_Param,
			Context_Param,
			Xx_gate_dbg_xx_Get,
			0,
			Plan_print1_Ret),
	Plan_print1_Ret=FnResult.
:- set_opv(f_u_plan_print, classof, claz_function),
   set_opv(u_plan_print, compile_as, kw_function),
   set_opv(u_plan_print, function, f_u_plan_print),
   DefunResult=u_plan_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33840 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" There is probably no such thing as always-prop",
				     1,
				     33918)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:33966 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*ep-print-options*',
			    [quote, [parens, 'always-prop', 'no-newline']]
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_ep_print_options_xx,
	   [u_parens, u_always_prop, u_no_newline]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:34026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'plan-print1',
			    [goal, context, stream, level],
			    ['print-spaces', stream, [*, level, 2]],
			    
			    [ cond,
			      
			      [ ['ty$instance?', goal, [quote, 'active-goal']],
				
				[ if,
				  '*typeset?*',
				  
				  [ format,
				    stream,
				    '$STRING'("[~A: (\\typepp{AG}. "),
				    ['ob->string', goal]
				  ],
				  
				  [ format,
				    stream,
				    '$STRING'("[~A: (AG. "),
				    ['ob->string', goal]
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', goal, [quote, 'failed-goal']],
				
				[ if,
				  '*typeset?*',
				  
				  [ format,
				    stream,
				    '$STRING'("[~A: (\\typepp{FG}. "),
				    ['ob->string', goal]
				  ],
				  
				  [ format,
				    stream,
				    '$STRING'("[~A: (FG. "),
				    ['ob->string', goal]
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', goal, [quote, 'succeeded-goal']],
				
				[ if,
				  '*typeset?*',
				  
				  [ format,
				    stream,
				    '$STRING'("[~A: (\\typepp{SG}. "),
				    ['ob->string', goal]
				  ],
				  
				  [ format,
				    stream,
				    '$STRING'("[~A: (SG. "),
				    ['ob->string', goal]
				  ]
				]
			      ],
			      
			      [ else,
				
				[ format,
				  stream,
				  '$STRING'("[~A: (?? "),
				  ['ob->string', goal]
				]
			      ]
			    ],
			    
			    [ if,
			      ['ob$get', goal, [quote, obj]],
			      
			      [ 'ob$pr',
				['ob$get', goal, [quote, obj]],
				stream,
				'*ep-print-options*'
			      ],
			      [format, stream, '$STRING'("--")]
			    ],
			    
			    [ if,
			      ['ob$get', goal, [quote, episode]],
			      
			      [ format,
				stream,
				'$STRING'(") ~A]"),
				['ob->string', ['ob$get', goal, [quote, episode]]]
			      ],
			      [format, stream, '$STRING'(")]")]
			    ],
			    ['do-newline', stream],
			    
			    [ yloop,
			      
			      [ yfor,
				subgoal,
				in,
				
				[ 'goal-subgoals',
				  goal,
				  context,
				  '*me-belief-path*'
				]
			      ],
			      
			      [ ydo,
				
				[ 'plan-print1',
				  subgoal,
				  context,
				  stream,
				  [+, 1, level]
				]
			      ]
			    ]
			  ]).

% annotating U::PLAN-PRINT1 
wl: lambda_def(defun,
	      u_plan_print1,
	      f_u_plan_print1,
	      [u_goal, u_context, stream, u_level],
	      
	      [ [u_print_spaces, stream, [*, u_level, 2]],
		
		[ cond,
		  
		  [ [u_ty_c36_instance_c63, u_goal, [quote, u_active_goal]],
		    
		    [ if,
		      u_xx_typeset_c63_xx,
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('['),
				   #\(~),
				   #\('A'),
				   #\(:),
				   #\(' '),
				   #\('('),
				   #\(\),
				   #\(t),
				   #\(y),
				   #\(p),
				   #\(e),
				   #\(p),
				   #\(p),
				   #\('{'),
				   #\('A'),
				   #\('G'),
				   #\('}'),
				   #\('.'),
				   #\(' ')
				 ]),
			[u_ob_c62_string, u_goal]
		      ],
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('['),
				   #\(~),
				   #\('A'),
				   #\(:),
				   #\(' '),
				   #\('('),
				   #\('A'),
				   #\('G'),
				   #\('.'),
				   #\(' ')
				 ]),
			[u_ob_c62_string, u_goal]
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_goal, [quote, u_failed_goal]],
		    
		    [ if,
		      u_xx_typeset_c63_xx,
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('['),
				   #\(~),
				   #\('A'),
				   #\(:),
				   #\(' '),
				   #\('('),
				   #\(\),
				   #\(t),
				   #\(y),
				   #\(p),
				   #\(e),
				   #\(p),
				   #\(p),
				   #\('{'),
				   #\('F'),
				   #\('G'),
				   #\('}'),
				   #\('.'),
				   #\(' ')
				 ]),
			[u_ob_c62_string, u_goal]
		      ],
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('['),
				   #\(~),
				   #\('A'),
				   #\(:),
				   #\(' '),
				   #\('('),
				   #\('F'),
				   #\('G'),
				   #\('.'),
				   #\(' ')
				 ]),
			[u_ob_c62_string, u_goal]
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_goal, [quote, u_succeeded_goal]],
		    
		    [ if,
		      u_xx_typeset_c63_xx,
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('['),
				   #\(~),
				   #\('A'),
				   #\(:),
				   #\(' '),
				   #\('('),
				   #\(\),
				   #\(t),
				   #\(y),
				   #\(p),
				   #\(e),
				   #\(p),
				   #\(p),
				   #\('{'),
				   #\('S'),
				   #\('G'),
				   #\('}'),
				   #\('.'),
				   #\(' ')
				 ]),
			[u_ob_c62_string, u_goal]
		      ],
		      
		      [ format,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('['),
				   #\(~),
				   #\('A'),
				   #\(:),
				   #\(' '),
				   #\('('),
				   #\('S'),
				   #\('G'),
				   #\('.'),
				   #\(' ')
				 ]),
			[u_ob_c62_string, u_goal]
		      ]
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ format,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('['),
				 #\(~),
				 #\('A'),
				 #\(:),
				 #\(' '),
				 #\('('),
				 #\(?),
				 #\(?),
				 #\(' ')
			       ]),
		      [u_ob_c62_string, u_goal]
		    ]
		  ]
		],
		
		[ if,
		  [u_ob_c36_get, u_goal, [quote, u_obj]],
		  
		  [ u_ob_c36_pr,
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    stream,
		    u_xx_ep_print_options_xx
		  ],
		  
		  [ format,
		    stream,
		    '$ARRAY'([*], claz_base_character, [#\(-), #\(-)])
		  ]
		],
		
		[ if,
		  [u_ob_c36_get, u_goal, [quote, u_episode]],
		  
		  [ format,
		    stream,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(')'), #\(' '), #\(~), #\('A'), #\(']')]),
		    [u_ob_c62_string, [u_ob_c36_get, u_goal, [quote, u_episode]]]
		  ],
		  
		  [ format,
		    stream,
		    '$ARRAY'([*], claz_base_character, [#\(')'), #\(']')])
		  ]
		],
		[u_do_newline, stream],
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_subgoal,
		    u_in,
		    [u_goal_subgoals, u_goal, u_context, u_xx_me_belief_path_xx]
		  ],
		  
		  [ u_ydo,
		    [u_plan_print1, u_subgoal, u_context, stream, [+, 1, u_level]]
		  ]
		]
	      ]).


% annotating U::PLAN-PRINT1 
wl: arglist_info(u_plan_print1,
		[u_goal, u_context, stream, u_level],
		[Goal_Param, Context_Param, Stream_Param, Level_Param],
		arginfo{ all:[u_goal, u_context, stream, u_level],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context, stream, u_level],
			 opt:0,
			 req:[u_goal, u_context, stream, u_level],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PLAN-PRINT1 
wl: init_args(exact_only, u_plan_print1).


% annotating U::PLAN-PRINT1 
f_u_plan_print1(Goal_Param, Context_Param, Stream_Param, Level_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(stream, Stream_Param), bv(u_level, Level_Param)],
	*(Level_Param, 2, _62916),
	f_u_print_spaces(Stream_Param, _62916, Print_spaces_Ret),
	f_u_ty_c36_instance_c63(Goal_Param, u_active_goal, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_typeset_c63_xx, IFTEST23),
	    (   IFTEST23\==[]
	    ->  f_u_ob_c62_string(Goal_Param, C62_string_Ret),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('['),
				       #\(~),
				       #\('A'),
				       #\(:),
				       #\(' '),
				       #\('('),
				       #\(\),
				       #\(t),
				       #\(y),
				       #\(p),
				       #\(e),
				       #\(p),
				       #\(p),
				       #\('{'),
				       #\('A'),
				       #\('G'),
				       #\('}'),
				       #\('.'),
				       #\(' ')
				     ]),
			    C62_string_Ret
			  ],
			  TrueResult),
		TrueResult63=TrueResult
	    ;   f_u_ob_c62_string(Goal_Param, C62_string_Ret93),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('['),
				       #\(~),
				       #\('A'),
				       #\(:),
				       #\(' '),
				       #\('('),
				       #\('A'),
				       #\('G'),
				       #\('.'),
				       #\(' ')
				     ]),
			    C62_string_Ret93
			  ],
			  ElseResult),
		TrueResult63=ElseResult
	    )
	;   f_u_ty_c36_instance_c63(Goal_Param, u_failed_goal, IFTEST32),
	    (   IFTEST32\==[]
	    ->  get_var(Env, u_xx_typeset_c63_xx, IFTEST35),
		(   IFTEST35\==[]
		->  f_u_ob_c62_string(Goal_Param, C62_string_Ret94),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('['),
					   #\(~),
					   #\('A'),
					   #\(:),
					   #\(' '),
					   #\('('),
					   #\(\),
					   #\(t),
					   #\(y),
					   #\(p),
					   #\(e),
					   #\(p),
					   #\(p),
					   #\('{'),
					   #\('F'),
					   #\('G'),
					   #\('}'),
					   #\('.'),
					   #\(' ')
					 ]),
				C62_string_Ret94
			      ],
			      TrueResult42),
		    TrueResult63=TrueResult42
		;   f_u_ob_c62_string(Goal_Param, C62_string_Ret95),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('['),
					   #\(~),
					   #\('A'),
					   #\(:),
					   #\(' '),
					   #\('('),
					   #\('F'),
					   #\('G'),
					   #\('.'),
					   #\(' ')
					 ]),
				C62_string_Ret95
			      ],
			      ElseResult43),
		    TrueResult63=ElseResult43
		)
	    ;   f_u_ty_c36_instance_c63(Goal_Param, u_succeeded_goal, IFTEST44),
		(   IFTEST44\==[]
		->  get_var(Env, u_xx_typeset_c63_xx, IFTEST47),
		    (   IFTEST47\==[]
		    ->  f_u_ob_c62_string(Goal_Param, C62_string_Ret96),
			cl_format(
				  [ Stream_Param,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('['),
					       #\(~),
					       #\('A'),
					       #\(:),
					       #\(' '),
					       #\('('),
					       #\(\),
					       #\(t),
					       #\(y),
					       #\(p),
					       #\(e),
					       #\(p),
					       #\(p),
					       #\('{'),
					       #\('S'),
					       #\('G'),
					       #\('}'),
					       #\('.'),
					       #\(' ')
					     ]),
				    C62_string_Ret96
				  ],
				  TrueResult54),
			TrueResult63=TrueResult54
		    ;   f_u_ob_c62_string(Goal_Param, C62_string_Ret97),
			cl_format(
				  [ Stream_Param,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('['),
					       #\(~),
					       #\('A'),
					       #\(:),
					       #\(' '),
					       #\('('),
					       #\('S'),
					       #\('G'),
					       #\('.'),
					       #\(' ')
					     ]),
				    C62_string_Ret97
				  ],
				  ElseResult55),
			TrueResult63=ElseResult55
		    )
		;   get_var(Env, u_else, IFTEST56),
		    (   IFTEST56\==[]
		    ->  f_u_ob_c62_string(Goal_Param, C62_string_Ret98),
			cl_format(
				  [ Stream_Param,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('['),
					       #\(~),
					       #\('A'),
					       #\(:),
					       #\(' '),
					       #\('('),
					       #\(?),
					       #\(?),
					       #\(' ')
					     ]),
				    C62_string_Ret98
				  ],
				  TrueResult61),
			TrueResult63=TrueResult61
		    ;   TrueResult63=[]
		    )
		)
	    )
	),
	f_u_ob_c36_get(Goal_Param, u_obj, IFTEST69),
	(   IFTEST69\==[]
	->  f_u_ob_c36_get(Goal_Param, u_obj, Obj),
	    get_var(Env, u_xx_ep_print_options_xx, Xx_ep_print_options_xx_Get),
	    f_u_ob_c36_pr(Obj,
			  Stream_Param,
			  Xx_ep_print_options_xx_Get,
			  TrueResult76),
	    _64334=TrueResult76
	;   cl_format(
		      [ Stream_Param,
			'$ARRAY'([*], claz_base_character, [#\(-), #\(-)])
		      ],
		      ElseResult77),
	    _64334=ElseResult77
	),
	f_u_ob_c36_get(Goal_Param, u_episode, IFTEST78),
	(   IFTEST78\==[]
	->  f_u_ob_c36_get(Goal_Param, u_episode, Episode),
	    f_u_ob_c62_string(Episode, C62_string_Ret99),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(')'), #\(' '), #\(~), #\('A'), #\(']')]),
			C62_string_Ret99
		      ],
		      TrueResult84),
	    _64650=TrueResult84
	;   cl_format(
		      [ Stream_Param,
			'$ARRAY'([*], claz_base_character, [#\(')'), #\(']')])
		      ],
		      ElseResult85),
	    _64650=ElseResult85
	),
	f_u_do_newline(Stream_Param, Do_newline_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_subgoal,
		      u_in,
		      
		      [ u_goal_subgoals,
			u_goal,
			u_context,
			u_xx_me_belief_path_xx
		      ]
		    ],
		    
		    [ u_ydo,
		      [u_plan_print1, u_subgoal, u_context, stream, [+, 1, u_level]]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_plan_print1, classof, claz_function),
   set_opv(u_plan_print1, compile_as, kw_function),
   set_opv(u_plan_print1, function, f_u_plan_print1),
   DefunResult=u_plan_print1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_epis.cl:34026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 35113)).
:- true.


% Total time: 13.437 seconds


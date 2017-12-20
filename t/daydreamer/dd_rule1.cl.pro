
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_rule1" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:13:51 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  6/22/85: First version with contexts written (3.0)",
				     1,
				     208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:261 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  8/27/85: Changed to new rule syntax (3.1)",
				     1,
				     262)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:306 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 11/26/85: Modified rules to be obs (3.2)",
				     1,
				     307)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:349 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/14/86: Changed non-action goal successes to 'fact plans'",
				     1,
				     350)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:411 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/19/86: Wrote inference chain to planning trc",
				     1,
				     412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:461 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/24/86: Finished adding new analogy code and planner mods",
				     1,
				     462)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:523 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/28/86: Added use of cx$get-all-ty",
				     1,
				     524)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:562 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  2/15/86: Started adding new tracing and linearization, running off nu-dd3",
				     1,
				     563)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:639 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  7/19/86: Added chaining mechanism",
				     1,
				     640)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:676 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/23/86: Got rid of flavors", 1, 677)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:707 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 708)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:709 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     710)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:790 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*linearized?*', t]).
:- set_var(TLEnv3, setq, u_xx_linearized_c63_xx, t).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:790 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 816)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:790 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer rules", 1, 818)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:790 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 837)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:839 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ty$create',
			    [quote, 'RULE'],
			    [],
			    
			    [ quote,
			      
			      [ [],
				
				[ subgoal,
				  goal,
				  delete,
				  initial,
				  emotion,
				  (is),
				  comment,
				  plausibility,
				  'self-type',
				  'initial-status',
				  'halt?',
				  'inf-comments',
				  'plan-comments',
				  'top-level-goal',
				  'no-pp-all',
				  'inf-no-gen',
				  'plan-no-gen',
				  'reality-subgoal',
				  script
				],
				
				[ 'self-type',
				  'inf-comments',
				  'plan-comments',
				  'inf-no-gen',
				  'top-level-goal',
				  'plan-no-gen',
				  'reality-subgoal'
				]
			      ]
			    ]
			  ]).
:- f_u_ty_c36_create(u_rule,
		     [],
		     
		     [ [],
		       
		       [ u_subgoal,
			 u_goal,
			 delete,
			 u_initial,
			 u_emotion,
			 u_is,
			 u_comment,
			 u_plausibility,
			 u_self_type,
			 u_initial_status,
			 u_halt_c63,
			 u_inf_comments,
			 u_plan_comments,
			 u_top_level_goal,
			 u_no_pp_all,
			 u_inf_no_gen,
			 u_plan_no_gen,
			 u_reality_subgoal,
			 u_script
		       ],
		       
		       [ u_self_type,
			 u_inf_comments,
			 u_plan_comments,
			 u_inf_no_gen,
			 u_top_level_goal,
			 u_plan_no_gen,
			 u_reality_subgoal
		       ]
		     ],
		     _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1472 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*rules*', []]).
:- set_var(TLEnv3, setq, u_xx_rules_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-fire-msg',
			    [rule, kind, context, bd, 'sprouted-context', goal],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("******************")
			    ],
			    
			    [ format,
			      ['standard-output'],
			      '$STRING'("~A "),
			      ['ob->string', rule]
			    ],
			    ['force-output', ['standard-output']],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("~A fired as ~A "),
			      ['ob->string', rule],
			      kind
			    ],
			    
			    [ if,
			      goal,
			      
			      [ progn,
				['ndbg-newline', '*gate-dbg*', rule],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("for ~A"),
				  goal
				]
			      ]
			    ],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("in ~A"),
			      ['ob->string', context]
			    ],
			    
			    [ if,
			      'sprouted-context',
			      
			      [ 'ndbg-roman',
				'*gate-dbg*',
				rule,
				'$STRING'(" sprouting ~A"),
				['ob->string', 'sprouted-context']
			      ]
			    ],
			    ['ndbg-newline', '*gate-dbg*', rule],
			    
			    [ 'if-interested-in',
			      rule,
			      ['print-comments', '*gate-dbg*', rule, kind]
			    ],
			    
			    [ 'if-interested-in',
			      rule,
			      
			      [ if,
				[not, '*typeset?*'],
				['ndbg-newline', '*gate-dbg*', rule]
			      ],
			      ['bd-print', bd, '*gate-dbg*']
			    ]
			  ]).

% annotating U::RULE-FIRE-MSG 
wl: lambda_def(defun,
	      u_rule_fire_msg,
	      f_u_rule_fire_msg,
	      [u_rule, u_kind, u_context, u_bd, u_sprouted_context, u_goal],
	      
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
		
		[ format,
		  [u_standard_output],
		  '$ARRAY'([*], claz_base_character, [#\(~), #\('A'), #\(' ')]),
		  [u_ob_c62_string, u_rule]
		],
		[force_output, [u_standard_output]],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\('A'),
			     #\(' '),
			     #\(f),
			     #\(i),
			     #\(r),
			     #\(e),
			     #\(d),
			     #\(' '),
			     #\(a),
			     #\(s),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' ')
			   ]),
		  [u_ob_c62_string, u_rule],
		  u_kind
		],
		
		[ if,
		  u_goal,
		  
		  [ progn,
		    [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(f), #\(o), #\(r), #\(' '), #\(~), #\('A')]),
		      u_goal
		    ]
		  ]
		],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(i), #\(n), #\(' '), #\(~), #\('A')]),
		  [u_ob_c62_string, u_context]
		],
		
		[ if,
		  u_sprouted_context,
		  
		  [ u_ndbg_roman,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(' '),
			       #\(s),
			       #\(p),
			       #\(r),
			       #\(o),
			       #\(u),
			       #\(t),
			       #\(i),
			       #\(n),
			       #\(g),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    [u_ob_c62_string, u_sprouted_context]
		  ]
		],
		[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		
		[ u_if_interested_in,
		  u_rule,
		  [u_print_comments, u_xx_gate_dbg_xx, u_rule, u_kind]
		],
		
		[ u_if_interested_in,
		  u_rule,
		  
		  [ if,
		    [not, u_xx_typeset_c63_xx],
		    [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]
		  ],
		  [u_bd_print, u_bd, u_xx_gate_dbg_xx]
		]
	      ]).


% annotating U::RULE-FIRE-MSG 
wl: arglist_info(u_rule_fire_msg,
		[u_rule, u_kind, u_context, u_bd, u_sprouted_context, u_goal],
		
		[ Rule_Param,
		  Kind_Param,
		  Context_Param,
		  Bd_Param,
		  Sprouted_context_Param,
		  Goal_Param
		],
		arginfo{ all:
			     [ u_rule,
			       u_kind,
			       u_context,
			       u_bd,
			       u_sprouted_context,
			       u_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_rule,
				 u_kind,
				 u_context,
				 u_bd,
				 u_sprouted_context,
				 u_goal
			       ],
			 opt:0,
			 req:
			     [ u_rule,
			       u_kind,
			       u_context,
			       u_bd,
			       u_sprouted_context,
			       u_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-FIRE-MSG 
wl: init_args(exact_only, u_rule_fire_msg).


% annotating U::RULE-FIRE-MSG 
f_u_rule_fire_msg(Rule_Param, Kind_Param, Context_Param, Bd_Param, Sprouted_context_Param, Goal_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_kind, Kind_Param), bv(u_context, Context_Param), bv(u_bd, Bd_Param), bv(u_sprouted_context, Sprouted_context_Param), bv(u_goal, Goal_Param)],
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
	f_u_standard_output(Standard_output_Ret),
	f_u_ob_c62_string(Rule_Param, C62_string_Ret),
	cl_format(
		  [ Standard_output_Ret,
		    '$ARRAY'([*], claz_base_character, [#\(~), #\('A'), #\(' ')]),
		    C62_string_Ret
		  ],
		  Format_Ret),
	f_u_standard_output(Force_output_Param),
	cl_force_output(Force_output_Param, Force_output_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\(~),
				    #\('A'),
				    #\(' '),
				    #\(f),
				    #\(i),
				    #\(r),
				    #\(e),
				    #\(d),
				    #\(' '),
				    #\(a),
				    #\(s),
				    #\(' '),
				    #\(~),
				    #\('A'),
				    #\(' ')
				  ]),
			 [u_ob_c62_string, u_rule],
			 u_kind
		       ],
		       Ndbg_roman_Ret),
	(   Goal_Param\==[]
	->  f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(f),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_goal
			      ],
			      TrueResult),
	    _107628=TrueResult
	;   _107628=[]
	),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  [#\(i), #\(n), #\(' '), #\(~), #\('A')]),
			 [u_ob_c62_string, u_context]
		       ],
		       Ndbg_roman_Ret42),
	(   Sprouted_context_Param\==[]
	->  f_u_ndbg_roman(u_xx_gate_dbg_xx,
			   u_rule,
			   
			   [ '$ARRAY'([*],
				      claz_base_character,
				      
				      [ #\(' '),
					#\(s),
					#\(p),
					#\(r),
					#\(o),
					#\(u),
					#\(t),
					#\(i),
					#\(n),
					#\(g),
					#\(' '),
					#\(~),
					#\('A')
				      ]),
			     [u_ob_c62_string, u_sprouted_context]
			   ],
			   TrueResult30),
	    _107716=TrueResult30
	;   _107716=[]
	),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule34),
	f_u_if_interested_in(u_rule,
			     
			     [ 
			       [ u_print_comments,
				 u_xx_gate_dbg_xx,
				 u_rule,
				 u_kind
			       ]
			     ],
			     Interested_in_Ret),
	f_u_if_interested_in(u_rule,
			     
			     [ 
			       [ if,
				 [not, u_xx_typeset_c63_xx],
				 [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]
			       ],
			       [u_bd_print, u_bd, u_xx_gate_dbg_xx]
			     ],
			     Interested_in_Ret44),
	Interested_in_Ret44=FnResult.
:- set_opv(f_u_rule_fire_msg, classof, claz_function),
   set_opv(u_rule_fire_msg, compile_as, kw_function),
   set_opv(u_rule_fire_msg, function, f_u_rule_fire_msg),
   DefunResult=u_rule_fire_msg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    (if-interested-in rule (ob$print rule *gate-dbg*)",
				     1,
				     2199)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              (if (not *typeset?*)",
				     1,
				     2254)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (ndbg-newline *gate-dbg* rule))",
				     1,
				     2290)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (bd-print bd *gate-dbg*))",
				     1,
				     2341)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Example:", 1, 2554)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" \\begin{tabular}{|ll|} \\hline", 1, 2565)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" IF & this is a test \\\\", 1, 2596)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    & of the system \\\\", 1, 2621)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" THEN & this is a test \\\\ \\hline",
				     1,
				     2645)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" \\end{tabular}", 1, 2679)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:1492 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2695)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:2697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-comments',
			    [stream, rule, kind],
			    
			    [ let,
			      
			      [ 
				[ com,
				  
				  [ cdr,
				    
				    [ 'ob$get',
				      rule,
				      
				      [ if,
					
					[ 'string-equal?',
					  kind,
					  '$STRING'("inference")
					],
					[quote, 'inf-comments'],
					[quote, 'plan-comments']
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				com,
				
				[ if,
				  '*typeset?*',
				  
				  [ progn,
				    
				    [ format,
				      stream,
				      '$STRING'("\\end{flushleft}~%")
				    ],
				    
				    [ format,
				      stream,
				      '$STRING'("\\begin{center}\\begin{tabular}{|ll|} \\hline~%")
				    ],
				    
				    [ format,
				      stream,
				      '$STRING'("{\\sl{}IF} & {\\rm{}")
				    ],
				    
				    [ setq,
				      com,
				      
				      [ cdr,
					['print-if-then-strings', com, stream]
				      ]
				    ],
				    
				    [ format,
				      stream,
				      '$STRING'("}\\\\~% {\\sl{}THEN} & {\\rm{}")
				    ],
				    ['print-if-then-strings', com, stream],
				    
				    [ format,
				      stream,
				      '$STRING'("} \\\\ \\hline~%\\end{tabular}\\end{center}")
				    ],
				    
				    [ format,
				      stream,
				      '$STRING'("\\begin{flushleft}~%")
				    ]
				  ],
				  
				  [ progn,
				    
				    [ format,
				      stream,
				      '$STRING'("-------------------------------------------------------~%")
				    ],
				    [format, stream, '$STRING'("IF   ")],
				    
				    [ setq,
				      com,
				      
				      [ cdr,
					['print-if-then-strings', com, stream]
				      ]
				    ],
				    [format, stream, '$STRING'("~%THEN ")],
				    ['print-if-then-strings', com, stream],
				    
				    [ format,
				      stream,
				      '$STRING'("~%-------------------------------------------------------~%")
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::PRINT-COMMENTS 
wl: lambda_def(defun,
	      u_print_comments,
	      f_u_print_comments,
	      [stream, u_rule, u_kind],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_com,
		      
		      [ cdr,
			
			[ u_ob_c36_get,
			  u_rule,
			  
			  [ if,
			    
			    [ u_string_equal_c63,
			      u_kind,
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
				       ])
			    ],
			    [quote, u_inf_comments],
			    [quote, u_plan_comments]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_com,
		    
		    [ if,
		      u_xx_typeset_c63_xx,
		      
		      [ progn,
			
			[ format,
			  stream,
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
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(\),
				     #\(b),
				     #\(e),
				     #\(g),
				     #\(i),
				     #\(n),
				     #\('{'),
				     #\(c),
				     #\(e),
				     #\(n),
				     #\(t),
				     #\(e),
				     #\(r),
				     #\('}'),
				     #\(\),
				     #\(b),
				     #\(e),
				     #\(g),
				     #\(i),
				     #\(n),
				     #\('{'),
				     #\(t),
				     #\(a),
				     #\(b),
				     #\(u),
				     #\(l),
				     #\(a),
				     #\(r),
				     #\('}'),
				     #\('{'),
				     #\('|'),
				     #\(l),
				     #\(l),
				     #\('|'),
				     #\('}'),
				     #\(' '),
				     #\(\),
				     #\(h),
				     #\(l),
				     #\(i),
				     #\(n),
				     #\(e),
				     #\(~),
				     #\('%')
				   ])
			],
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('{'),
				     #\(\),
				     #\(s),
				     #\(l),
				     #\('{'),
				     #\('}'),
				     #\('I'),
				     #\('F'),
				     #\('}'),
				     #\(' '),
				     #\(&),
				     #\(' '),
				     #\('{'),
				     #\(\),
				     #\(r),
				     #\(m),
				     #\('{'),
				     #\('}')
				   ])
			],
			
			[ setq,
			  u_com,
			  [cdr, [u_print_if_then_strings, u_com, stream]]
			],
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('}'),
				     #\(\),
				     #\(\),
				     #\(~),
				     #\('%'),
				     #\(' '),
				     #\('{'),
				     #\(\),
				     #\(s),
				     #\(l),
				     #\('{'),
				     #\('}'),
				     #\('T'),
				     #\('H'),
				     #\('E'),
				     #\('N'),
				     #\('}'),
				     #\(' '),
				     #\(&),
				     #\(' '),
				     #\('{'),
				     #\(\),
				     #\(r),
				     #\(m),
				     #\('{'),
				     #\('}')
				   ])
			],
			[u_print_if_then_strings, u_com, stream],
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('}'),
				     #\(' '),
				     #\(\),
				     #\(\),
				     #\(' '),
				     #\(\),
				     #\(h),
				     #\(l),
				     #\(i),
				     #\(n),
				     #\(e),
				     #\(~),
				     #\('%'),
				     #\(\),
				     #\(e),
				     #\(n),
				     #\(d),
				     #\('{'),
				     #\(t),
				     #\(a),
				     #\(b),
				     #\(u),
				     #\(l),
				     #\(a),
				     #\(r),
				     #\('}'),
				     #\(\),
				     #\(e),
				     #\(n),
				     #\(d),
				     #\('{'),
				     #\(c),
				     #\(e),
				     #\(n),
				     #\(t),
				     #\(e),
				     #\(r),
				     #\('}')
				   ])
			],
			
			[ format,
			  stream,
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
		      
		      [ progn,
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(~),
				     #\('%')
				   ])
			],
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\('I'), #\('F'), #\(' '), #\(' '), #\(' ')])
			],
			
			[ setq,
			  u_com,
			  [cdr, [u_print_if_then_strings, u_com, stream]]
			],
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(~),
				     #\('%'),
				     #\('T'),
				     #\('H'),
				     #\('E'),
				     #\('N'),
				     #\(' ')
				   ])
			],
			[u_print_if_then_strings, u_com, stream],
			
			[ format,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(~),
				     #\('%'),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(-),
				     #\(~),
				     #\('%')
				   ])
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::PRINT-COMMENTS 
wl: arglist_info(u_print_comments,
		[stream, u_rule, u_kind],
		[Stream_Param, Rule_Param, Kind_Param],
		arginfo{ all:[stream, u_rule, u_kind],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream, u_rule, u_kind],
			 opt:0,
			 req:[stream, u_rule, u_kind],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-COMMENTS 
wl: init_args(exact_only, u_print_comments).


% annotating U::PRINT-COMMENTS 
f_u_print_comments(Stream_Param, Rule_Param, Kind_Param, TrueResult49) :-
	Env=[bv(stream, Stream_Param), bv(u_rule, Rule_Param), bv(u_kind, Kind_Param)],
	f_u_string_equal_c63(u_kind,
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
			     IFTEST),
	(   IFTEST\==[]
	->  _110214=u_inf_comments
	;   _110214=u_plan_comments
	),
	f_u_ob_c36_get(Rule_Param, _110214, Cdr_Param),
	cl_cdr(Cdr_Param, Com_Init),
	LEnv=[[bv(u_com, Com_Init)]|Env],
	get_var(LEnv, u_com, IFTEST22),
	(   IFTEST22\==[]
	->  get_var(LEnv, u_xx_typeset_c63_xx, IFTEST26),
	    (   IFTEST26\==[]
	    ->  cl_format(
			  [ Stream_Param,
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
			  Format_Ret),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(\),
				       #\(b),
				       #\(e),
				       #\(g),
				       #\(i),
				       #\(n),
				       #\('{'),
				       #\(c),
				       #\(e),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\('}'),
				       #\(\),
				       #\(b),
				       #\(e),
				       #\(g),
				       #\(i),
				       #\(n),
				       #\('{'),
				       #\(t),
				       #\(a),
				       #\(b),
				       #\(u),
				       #\(l),
				       #\(a),
				       #\(r),
				       #\('}'),
				       #\('{'),
				       #\('|'),
				       #\(l),
				       #\(l),
				       #\('|'),
				       #\('}'),
				       #\(' '),
				       #\(\),
				       #\(h),
				       #\(l),
				       #\(i),
				       #\(n),
				       #\(e),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  Format_Ret58),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('{'),
				       #\(\),
				       #\(s),
				       #\(l),
				       #\('{'),
				       #\('}'),
				       #\('I'),
				       #\('F'),
				       #\('}'),
				       #\(' '),
				       #\(&),
				       #\(' '),
				       #\('{'),
				       #\(\),
				       #\(r),
				       #\(m),
				       #\('{'),
				       #\('}')
				     ])
			  ],
			  Format_Ret59),
		f_u_print_if_then_strings(IFTEST22, Stream_Param, Cdr_Param55),
		cl_cdr(Cdr_Param55, Com),
		set_var(LEnv, u_com, Com),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('}'),
				       #\(\),
				       #\(\),
				       #\(~),
				       #\('%'),
				       #\(' '),
				       #\('{'),
				       #\(\),
				       #\(s),
				       #\(l),
				       #\('{'),
				       #\('}'),
				       #\('T'),
				       #\('H'),
				       #\('E'),
				       #\('N'),
				       #\('}'),
				       #\(' '),
				       #\(&),
				       #\(' '),
				       #\('{'),
				       #\(\),
				       #\(r),
				       #\(m),
				       #\('{'),
				       #\('}')
				     ])
			  ],
			  Format_Ret60),
		f_u_print_if_then_strings(IFTEST22,
					  Stream_Param,
					  Then_strings_Ret),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('}'),
				       #\(' '),
				       #\(\),
				       #\(\),
				       #\(' '),
				       #\(\),
				       #\(h),
				       #\(l),
				       #\(i),
				       #\(n),
				       #\(e),
				       #\(~),
				       #\('%'),
				       #\(\),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\('{'),
				       #\(t),
				       #\(a),
				       #\(b),
				       #\(u),
				       #\(l),
				       #\(a),
				       #\(r),
				       #\('}'),
				       #\(\),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\('{'),
				       #\(c),
				       #\(e),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\('}')
				     ])
			  ],
			  Format_Ret62),
		cl_format(
			  [ Stream_Param,
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
		TrueResult49=TrueResult
	    ;   cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  Format_Ret63),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\('I'), #\('F'), #\(' '), #\(' '), #\(' ')])
			  ],
			  Format_Ret64),
		get_var(LEnv, u_com, Com_Get41),
		f_u_print_if_then_strings(Com_Get41, Stream_Param, Cdr_Param56),
		cl_cdr(Cdr_Param56, Com53),
		set_var(LEnv, u_com, Com53),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('%'),
				       #\('T'),
				       #\('H'),
				       #\('E'),
				       #\('N'),
				       #\(' ')
				     ])
			  ],
			  Format_Ret65),
		get_var(LEnv, u_com, Com_Get44),
		f_u_print_if_then_strings(Com_Get44,
					  Stream_Param,
					  Then_strings_Ret66),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('%'),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  ElseResult),
		TrueResult49=ElseResult
	    )
	;   TrueResult49=[]
	).
:- set_opv(f_u_print_comments, classof, claz_function),
   set_opv(u_print_comments, compile_as, kw_function),
   set_opv(u_print_comments, function, f_u_print_comments),
   DefunResult=u_print_comments.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:2697 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("               (progn", 1, 3013)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:2697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (format stream \"\\\\begin{flushleft}~%\")",
				     1,
				     3036)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:2697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (format stream \"English description for rule as ~A:~%\"",
				     1,
				     3092)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:2697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                        kind)", 1, 3164)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:2697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (format stream \"\\\\end{flushleft}~%\"))",
				     1,
				     3195)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:4073 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*if-then-length*', 40]).
:- set_var(TLEnv3, setq, u_xx_if_then_length_xx, 40).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:4073 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Use 'et' for 'and' if you do not want it to break into separate lines.",
				     1,
				     4102)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:4174 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-if-then-strings',
			    [com, stream],
			    
			    [ yloop,
			      
			      [ initial,
				[len, 0],
				['last-word', '$STRING'("silly")]
			      ],
			      
			      [ yuntil,
				
				[ or,
				  ['null?', com],
				  ['eq?', [car, com], [quote, then]]
				]
			      ],
			      
			      [ ydo,
				
				[ yloop,
				  
				  [ yfor,
				    word,
				    in,
				    ['break-into-words', [car, com]]
				  ],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ or,
					[>, len, '*if-then-length*'],
					
					[ 'string-equal?',
					  'last-word',
					  '$STRING'("and")
					]
				      ],
				      
				      [ progn,
					
					[ if,
					  '*typeset?*',
					  [format, stream, '$STRING'("} ")]
					],
					['do-newline', stream],
					
					[ if,
					  '*typeset?*',
					  
					  [ format,
					    stream,
					    '$STRING'(" & {\\rm{}")
					  ],
					  [format, stream, '$STRING'("     ")]
					],
					[setq, len, 0]
				      ]
				    ],
				    [setq, 'last-word', word],
				    
				    [ cond,
				      
				      [ ['uppercase?', ['string-head', word]],
					['begin-head-font', stream],
					[format, stream, '$STRING'("~A "), word],
					['end-font', stream],
					
					[ setq,
					  len,
					  [+, 1, len, ['string-length', word]]
					]
				      ],
				      
				      [ ['string-equal?', word, '$STRING'("et")],
					[format, stream, '$STRING'("and ")],
					[setq, len, [+, len, 4]]
				      ],
				      
				      [ else,
					[format, stream, '$STRING'("~A "), word],
					
					[ setq,
					  len,
					  [+, 1, len, ['string-length', word]]
					]
				      ]
				    ]
				  ]
				],
				[setq, com, [cdr, com]]
			      ],
			      [yresult, com]
			    ]
			  ]).

% annotating U::PRINT-IF-THEN-STRINGS 
wl: lambda_def(defun,
	      u_print_if_then_strings,
	      f_u_print_if_then_strings,
	      [u_com, stream],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_len, 0],
		    
		    [ u_last_word,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(s), #\(i), #\(l), #\(l), #\(y)])
		    ]
		  ],
		  
		  [ u_yuntil,
		    
		    [ or,
		      [u_null_c63, u_com],
		      [u_eq_c63, [car, u_com], [quote, u_then]]
		    ]
		  ],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      [u_yfor, ext_word, u_in, [u_break_into_words, [car, u_com]]],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ or,
			    [>, u_len, u_xx_if_then_length_xx],
			    
			    [ u_string_equal_c63,
			      u_last_word,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(a), #\(n), #\(d)])
			    ]
			  ],
			  
			  [ progn,
			    
			    [ if,
			      u_xx_typeset_c63_xx,
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\('}'), #\(' ')])
			      ]
			    ],
			    [u_do_newline, stream],
			    
			    [ if,
			      u_xx_typeset_c63_xx,
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(&),
					   #\(' '),
					   #\('{'),
					   #\(\),
					   #\(r),
					   #\(m),
					   #\('{'),
					   #\('}')
					 ])
			      ],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(' '),
					   #\(' '),
					   #\(' '),
					   #\(' ')
					 ])
			      ]
			    ],
			    [setq, u_len, 0]
			  ]
			],
			[setq, u_last_word, ext_word],
			
			[ cond,
			  
			  [ [u_uppercase_c63, [u_string_head, ext_word]],
			    [u_begin_head_font, stream],
			    
			    [ format,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(~), #\('A'), #\(' ')]),
			      ext_word
			    ],
			    [u_end_font, stream],
			    
			    [ setq,
			      u_len,
			      [+, 1, u_len, [u_string_length, ext_word]]
			    ]
			  ],
			  
			  [ 
			    [ u_string_equal_c63,
			      ext_word,
			      '$ARRAY'([*], claz_base_character, [#\(e), #\(t)])
			    ],
			    
			    [ format,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(a), #\(n), #\(d), #\(' ')])
			    ],
			    [setq, u_len, [+, u_len, 4]]
			  ],
			  
			  [ u_else,
			    
			    [ format,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(~), #\('A'), #\(' ')]),
			      ext_word
			    ],
			    
			    [ setq,
			      u_len,
			      [+, 1, u_len, [u_string_length, ext_word]]
			    ]
			  ]
			]
		      ]
		    ],
		    [setq, u_com, [cdr, u_com]]
		  ],
		  [u_yresult, u_com]
		]
	      ]).


% annotating U::PRINT-IF-THEN-STRINGS 
wl: arglist_info(u_print_if_then_strings,
		[u_com, stream],
		[Com_Param, Stream_Param],
		arginfo{ all:[u_com, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_com, stream],
			 opt:0,
			 req:[u_com, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-IF-THEN-STRINGS 
wl: init_args(exact_only, u_print_if_then_strings).


% annotating U::PRINT-IF-THEN-STRINGS 
f_u_print_if_then_strings(Com_Param, Stream_Param, FnResult) :-
	Env=[bv(u_com, Com_Param), bv(stream, Stream_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_len, 0],
		      
		      [ u_last_word,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(s), #\(i), #\(l), #\(l), #\(y)])
		      ]
		    ],
		    
		    [ u_yuntil,
		      
		      [ or,
			[u_null_c63, u_com],
			[u_eq_c63, [car, u_com], [quote, u_then]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			
			[ u_yfor,
			  ext_word,
			  u_in,
			  [u_break_into_words, [car, u_com]]
			],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ or,
			      [>, u_len, u_xx_if_then_length_xx],
			      
			      [ u_string_equal_c63,
				u_last_word,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(a), #\(n), #\(d)])
			      ]
			    ],
			    
			    [ progn,
			      
			      [ if,
				u_xx_typeset_c63_xx,
				
				[ format,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\('}'), #\(' ')])
				]
			      ],
			      [u_do_newline, stream],
			      
			      [ if,
				u_xx_typeset_c63_xx,
				
				[ format,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(' '),
					     #\(&),
					     #\(' '),
					     #\('{'),
					     #\(\),
					     #\(r),
					     #\(m),
					     #\('{'),
					     #\('}')
					   ])
				],
				
				[ format,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(' '),
					     #\(' '),
					     #\(' '),
					     #\(' '),
					     #\(' ')
					   ])
				]
			      ],
			      [setq, u_len, 0]
			    ]
			  ],
			  [setq, u_last_word, ext_word],
			  
			  [ cond,
			    
			    [ [u_uppercase_c63, [u_string_head, ext_word]],
			      [u_begin_head_font, stream],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(~), #\('A'), #\(' ')]),
				ext_word
			      ],
			      [u_end_font, stream],
			      
			      [ setq,
				u_len,
				[+, 1, u_len, [u_string_length, ext_word]]
			      ]
			    ],
			    
			    [ 
			      [ u_string_equal_c63,
				ext_word,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(e), #\(t)])
			      ],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(a), #\(n), #\(d), #\(' ')])
			      ],
			      [setq, u_len, [+, u_len, 4]]
			    ],
			    
			    [ u_else,
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(~), #\('A'), #\(' ')]),
				ext_word
			      ],
			      
			      [ setq,
				u_len,
				[+, 1, u_len, [u_string_length, ext_word]]
			      ]
			    ]
			  ]
			]
		      ],
		      [setq, u_com, [cdr, u_com]]
		    ],
		    [u_yresult, u_com]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_print_if_then_strings, classof, claz_function),
   set_opv(u_print_if_then_strings, compile_as, kw_function),
   set_opv(u_print_if_then_strings, function, f_u_print_if_then_strings),
   DefunResult=u_print_if_then_strings.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'break-into-words',
			    [string],
			    
			    [ yloop,
			      [initial, [result, []], [pos, []]],
			      [yuntil, ['string-empty?', string]],
			      
			      [ ydo,
				[setq, pos, ['string-posq', #\(' '), string]],
				
				[ if,
				  pos,
				  
				  [ progn,
				    
				    [ setq,
				      result,
				      
				      [ 'append!',
					result,
					[list, ['string-slice', string, 0, pos]]
				      ]
				    ],
				    
				    [ setq,
				      string,
				      ['string-nthtail', string, [+, 1, pos]]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ setq,
				      result,
				      ['append!', result, [list, string]]
				    ],
				    [setq, string, '$STRING'("")]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::BREAK-INTO-WORDS 
wl: lambda_def(defun,
	      u_break_into_words,
	      f_u_break_into_words,
	      [string],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_pos, []]],
		  [u_yuntil, [u_string_empty_c63, string]],
		  
		  [ u_ydo,
		    [setq, u_pos, [u_string_posq, #\(' '), string]],
		    
		    [ if,
		      u_pos,
		      
		      [ progn,
			
			[ setq,
			  u_result,
			  
			  [ u_append_c33,
			    u_result,
			    [list, [u_string_slice, string, 0, u_pos]]
			  ]
			],
			[setq, string, [u_string_nthtail, string, [+, 1, u_pos]]]
		      ],
		      
		      [ progn,
			[setq, u_result, [u_append_c33, u_result, [list, string]]],
			[setq, string, '$ARRAY'([*], claz_base_character, [])]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::BREAK-INTO-WORDS 
wl: arglist_info(u_break_into_words,
		[string],
		[String_Param],
		arginfo{ all:[string],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[string],
			 opt:0,
			 req:[string],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BREAK-INTO-WORDS 
wl: init_args(exact_only, u_break_into_words).


% annotating U::BREAK-INTO-WORDS 
f_u_break_into_words(String_Param, FnResult) :-
	Env=[bv(string, String_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_pos, []]],
		    [u_yuntil, [u_string_empty_c63, string]],
		    
		    [ u_ydo,
		      [setq, u_pos, [u_string_posq, #\(' '), string]],
		      
		      [ if,
			u_pos,
			
			[ progn,
			  
			  [ setq,
			    u_result,
			    
			    [ u_append_c33,
			      u_result,
			      [list, [u_string_slice, string, 0, u_pos]]
			    ]
			  ],
			  [setq, string, [u_string_nthtail, string, [+, 1, u_pos]]]
			],
			
			[ progn,
			  
			  [ setq,
			    u_result,
			    [u_append_c33, u_result, [list, string]]
			  ],
			  [setq, string, '$ARRAY'([*], claz_base_character, [])]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_break_into_words, classof, claz_function),
   set_opv(u_break_into_words, compile_as, kw_function),
   set_opv(u_break_into_words, function, f_u_break_into_words),
   DefunResult=u_break_into_words.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5818)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Unenforced assumptions about rule base:",
				     1,
				     5820)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Every action must have a precondition plan, even if it is only TRUE->ACTION",
				     1,
				     5862)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Assume only one rule matches each action, and that each rule contains",
				     1,
				     5942)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   all the necessary preconditions for any matchee",
				     1,
				     6016)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6068)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Syntax:", 1, 6070)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6080)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (define-rule name subsets spec)",
				     1,
				     6082)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" where spec =", 1, 6116)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("       (RULE", 1, 6131)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        [comment <string> <string> ...]",
				     1,
				     6145)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        plausibility <number>", 1, 6186)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        subgoal <pattern>", 1, 6217)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        goal <pattern>", 1, 6244)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        [delete <pattern> <pattern> ...]",
				     1,
				     6268)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        [is inference-only |", 1, 6310)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             plan-only | plan-only-no-auto | action-plan |",
				     1,
				     6340)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             global-inference])", 1, 6400)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6433)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   Optional slots in goal slot: WEIGHT, OFFSET, DECAY",
				     1,
				     6435)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (otherwise plausibility is used, right?)",
				     1,
				     6490)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6539)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   Inferences always sum into assertions existing in the",
				     1,
				     6541)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   appropriate context", 1, 6599)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6623)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   There is no reason maintenance, i.e., things aren't",
				     1,
				     6625)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   removed upon removal of their justifications.",
				     1,
				     6681)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:5272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6731)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:6733 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'undefine-rule',
			    [rule],
			    ['rule-destroy-chaining', rule],
			    ['ob$remove-all', rule],
			    ['ob$set', rule, [quote, is], [quote, destroyed]],
			    [setq, '*rules*', ['delq!', rule, '*rules*']],
			    t
			  ]).

% annotating U::UNDEFINE-RULE 
wl: lambda_def(defun,
	      u_undefine_rule,
	      f_u_undefine_rule,
	      [u_rule],
	      
	      [ [u_rule_destroy_chaining, u_rule],
		[u_ob_c36_remove_all, u_rule],
		[u_ob_c36_set, u_rule, [quote, u_is], [quote, u_destroyed]],
		[setq, u_xx_rules_xx, [u_delq_c33, u_rule, u_xx_rules_xx]],
		t
	      ]).


% annotating U::UNDEFINE-RULE 
wl: arglist_info(u_undefine_rule,
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

% annotating U::UNDEFINE-RULE 
wl: init_args(exact_only, u_undefine_rule).


% annotating U::UNDEFINE-RULE 
f_u_undefine_rule(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_rule_destroy_chaining(Rule_Param, Destroy_chaining_Ret),
	f_u_ob_c36_remove_all(Rule_Param, Remove_all_Ret),
	f_u_ob_c36_set(Rule_Param, u_is, u_destroyed, Destroyed),
	f_u_delq_c33(u_rule, u_xx_rules_xx, Xx_rules_xx),
	set_var(Env, u_xx_rules_xx, Xx_rules_xx),
	t=FnResult.
:- set_opv(f_u_undefine_rule, classof, claz_function),
   set_opv(u_undefine_rule, compile_as, kw_function),
   set_opv(u_undefine_rule, function, f_u_undefine_rule),
   DefunResult=u_undefine_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:6890 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-destroy-chaining',
			    [rule],
			    
			    [ yloop,
			      
			      [ yfor,
				r,
				in,
				['ob$gets', rule, [quote, 'forward-chain']]
			      ],
			      [ydo, ['remove-chaining', rule, r]]
			    ]
			  ]).

% annotating U::RULE-DESTROY-CHAINING 
wl: lambda_def(defun,
	      u_rule_destroy_chaining,
	      f_u_rule_destroy_chaining,
	      [u_rule],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_yfor,
		    u_r,
		    u_in,
		    [u_ob_c36_gets, u_rule, [quote, u_forward_chain]]
		  ],
		  [u_ydo, [u_remove_chaining, u_rule, u_r]]
		]
	      ]).


% annotating U::RULE-DESTROY-CHAINING 
wl: arglist_info(u_rule_destroy_chaining,
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

% annotating U::RULE-DESTROY-CHAINING 
wl: init_args(exact_only, u_rule_destroy_chaining).


% annotating U::RULE-DESTROY-CHAINING 
f_u_rule_destroy_chaining(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_r,
		      u_in,
		      [u_ob_c36_gets, u_rule, [quote, u_forward_chain]]
		    ],
		    [u_ydo, [u_remove_chaining, u_rule, u_r]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_rule_destroy_chaining, classof, claz_function),
   set_opv(u_rule_destroy_chaining, compile_as, kw_function),
   set_opv(u_rule_destroy_chaining, function, f_u_rule_destroy_chaining),
   DefunResult=u_rule_destroy_chaining.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:6890 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If modified, change add-rule-print.",
				     1,
				     7020)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:7057 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-rule',
			    [rule],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Adding rule ~A"),
			      ['ob->string', rule]
			    ],
			    [setq, '*rules*', [cons, rule, '*rules*']],
			    ['rule-create-chaining', rule]
			  ]).

% annotating U::ADD-RULE 
wl: lambda_def(defun,
	      u_add_rule,
	      f_u_add_rule,
	      [u_rule],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(d),
			     #\(d),
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
		  [u_ob_c62_string, u_rule]
		],
		[setq, u_xx_rules_xx, [cons, u_rule, u_xx_rules_xx]],
		[u_rule_create_chaining, u_rule]
	      ]).


% annotating U::ADD-RULE 
wl: arglist_info(u_add_rule,
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

% annotating U::ADD-RULE 
wl: init_args(exact_only, u_add_rule).


% annotating U::ADD-RULE 
f_u_add_rule(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(d),
				       #\(d),
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
			    [u_ob_c62_string, u_rule]
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_rules_xx, Xx_rules_xx_Get),
	Xx_rules_xx=[Rule_Param|Xx_rules_xx_Get],
	set_var(Env, u_xx_rules_xx, Xx_rules_xx),
	f_u_rule_create_chaining(Rule_Param, Create_chaining_Ret),
	Create_chaining_Ret=FnResult.
:- set_opv(f_u_add_rule, classof, claz_function),
   set_opv(u_add_rule, compile_as, kw_function),
   set_opv(u_add_rule, function, f_u_add_rule),
   DefunResult=u_add_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:7057 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If modified, change add-rule.", 1, 7219)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:7250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-rule-print',
			    [rule],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Adding rule ")
			    ],
			    ['dbg-print-ob', rule],
			    [setq, '*rules*', [cons, rule, '*rules*']],
			    ['rule-create-chaining', rule]
			  ]).

% annotating U::ADD-RULE-PRINT 
wl: lambda_def(defun,
	      u_add_rule_print,
	      f_u_add_rule_print,
	      [u_rule],
	      
	      [ 
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(d),
			     #\(d),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(r),
			     #\(u),
			     #\(l),
			     #\(e),
			     #\(' ')
			   ])
		],
		[u_dbg_print_ob, u_rule],
		[setq, u_xx_rules_xx, [cons, u_rule, u_xx_rules_xx]],
		[u_rule_create_chaining, u_rule]
	      ]).


% annotating U::ADD-RULE-PRINT 
wl: arglist_info(u_add_rule_print,
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

% annotating U::ADD-RULE-PRINT 
wl: init_args(exact_only, u_add_rule_print).


% annotating U::ADD-RULE-PRINT 
f_u_add_rule_print(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\('A'),
				    #\(d),
				    #\(d),
				    #\(i),
				    #\(n),
				    #\(g),
				    #\(' '),
				    #\(r),
				    #\(u),
				    #\(l),
				    #\(e),
				    #\(' ')
				  ])
		       ],
		       Ndbg_roman_Ret),
	f_u_dbg_print_ob(Rule_Param, Print_ob_Ret),
	get_var(Env, u_xx_rules_xx, Xx_rules_xx_Get),
	Xx_rules_xx=[Rule_Param|Xx_rules_xx_Get],
	set_var(Env, u_xx_rules_xx, Xx_rules_xx),
	f_u_rule_create_chaining(Rule_Param, Create_chaining_Ret),
	Create_chaining_Ret=FnResult.
:- set_opv(f_u_add_rule_print, classof, claz_function),
   set_opv(u_add_rule_print, compile_as, kw_function),
   set_opv(u_add_rule_print, function, f_u_add_rule_print),
   DefunResult=u_add_rule_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:7416 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dbg-print-ob',
			    [ob],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("~A:"),
			      ['ob->string', ob]
			    ],
			    ['ob$pr', ob, '*gate-dbg*', '*ob-print-options*'],
			    ['ndbg-newline', '*gate-dbg*', rule]
			  ]).

% annotating U::DBG-PRINT-OB 
wl: lambda_def(defun,
	      u_dbg_print_ob,
	      f_u_dbg_print_ob,
	      [u_ob],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*], claz_base_character, [#\(~), #\('A'), #\(:)]),
		  [u_ob_c62_string, u_ob]
		],
		[u_ob_c36_pr, u_ob, u_xx_gate_dbg_xx, u_xx_ob_print_options_xx],
		[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule]
	      ]).


% annotating U::DBG-PRINT-OB 
wl: arglist_info(u_dbg_print_ob,
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

% annotating U::DBG-PRINT-OB 
wl: init_args(exact_only, u_dbg_print_ob).


% annotating U::DBG-PRINT-OB 
f_u_dbg_print_ob(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     [#\(~), #\('A'), #\(:)]),
			    [u_ob_c62_string, u_ob]
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	get_var(Env, u_xx_ob_print_options_xx, Xx_ob_print_options_xx_Get),
	f_u_ob_c36_pr(Ob_Param,
		      Xx_gate_dbg_xx_Get,
		      Xx_ob_print_options_xx_Get,
		      C36_pr_Ret),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	Rule=FnResult.
:- set_opv(f_u_dbg_print_ob, classof, claz_function),
   set_opv(u_dbg_print_ob, compile_as, kw_function),
   set_opv(u_dbg_print_ob, function, f_u_dbg_print_ob),
   DefunResult=u_dbg_print_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:7575 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'check-rule',
			    [rule, name],
			    
			    [ if,
			      ['null?', ['ob$get', rule, [quote, plausibility]]],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Warning: ~A has no plausibility."),
				  name
				],
				['ob$add', rule, [quote, plausibility], 1.0]
			      ]
			    ],
			    
			    [ if,
			      
			      [ and,
				['plan?', rule],
				['ob$get', rule, [quote, subgoal]],
				
				[ 'ty$instance?',
				  ['ob$get', rule, [quote, subgoal]],
				  [quote, rand]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Warning: ~A should be RSEQ?"),
				name
			      ]
			    ],
			    
			    [ if,
			      
			      [ and,
				['ob$get', rule, [quote, goal]],
				
				[ 'ty$instance?',
				  ['ob$get', rule, [quote, goal]],
				  [quote, action]
				],
				[not, ['action-plan?', rule]]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Warning: ~A should be action-plan?"),
				name
			      ]
			    ],
			    
			    [ if,
			      
			      [ and,
				['action-plan?', rule],
				['ob$get', rule, [quote, goal]],
				
				[ not,
				  
				  [ 'ty$instance?',
				    ['ob$get', rule, [quote, goal]],
				    [quote, action]
				  ]
				]
			      ],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Warning: Either ~A should not be an action-plan"),
				  name
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'(" or you forgot to declare ~A as type ACTION"),
				  
				  [ 'ob$name',
				    
				    [ 'ob$get',
				      ['ob$get', rule, [quote, goal]],
				      [quote, type]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::CHECK-RULE 
wl: lambda_def(defun,
	      u_check_rule,
	      f_u_check_rule,
	      [u_rule, sys_name],
	      
	      [ 
		[ if,
		  [u_null_c63, [u_ob_c36_get, u_rule, [quote, u_plausibility]]],
		  
		  [ progn,
		    
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
				 #\(h),
				 #\(a),
				 #\(s),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(' '),
				 #\(p),
				 #\(l),
				 #\(a),
				 #\(u),
				 #\(s),
				 #\(i),
				 #\(b),
				 #\(i),
				 #\(l),
				 #\(i),
				 #\(t),
				 #\(y),
				 #\('.')
			       ]),
		      sys_name
		    ],
		    [u_ob_c36_add, u_rule, [quote, u_plausibility], 1.0]
		  ]
		],
		
		[ if,
		  
		  [ and,
		    [u_plan_c63, u_rule],
		    [u_ob_c36_get, u_rule, [quote, u_subgoal]],
		    
		    [ u_ty_c36_instance_c63,
		      [u_ob_c36_get, u_rule, [quote, u_subgoal]],
		      [quote, u_rand]
		    ]
		  ],
		  
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
			       #\(s),
			       #\(h),
			       #\(o),
			       #\(u),
			       #\(l),
			       #\(d),
			       #\(' '),
			       #\(b),
			       #\(e),
			       #\(' '),
			       #\('R'),
			       #\('S'),
			       #\('E'),
			       #\('Q'),
			       #\(?)
			     ]),
		    sys_name
		  ]
		],
		
		[ if,
		  
		  [ and,
		    [u_ob_c36_get, u_rule, [quote, u_goal]],
		    
		    [ u_ty_c36_instance_c63,
		      [u_ob_c36_get, u_rule, [quote, u_goal]],
		      [quote, u_action]
		    ],
		    [not, [u_action_plan_c63, u_rule]]
		  ],
		  
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
			       #\(s),
			       #\(h),
			       #\(o),
			       #\(u),
			       #\(l),
			       #\(d),
			       #\(' '),
			       #\(b),
			       #\(e),
			       #\(' '),
			       #\(a),
			       #\(c),
			       #\(t),
			       #\(i),
			       #\(o),
			       #\(n),
			       #\(-),
			       #\(p),
			       #\(l),
			       #\(a),
			       #\(n),
			       #\(?)
			     ]),
		    sys_name
		  ]
		],
		
		[ if,
		  
		  [ and,
		    [u_action_plan_c63, u_rule],
		    [u_ob_c36_get, u_rule, [quote, u_goal]],
		    
		    [ not,
		      
		      [ u_ty_c36_instance_c63,
			[u_ob_c36_get, u_rule, [quote, u_goal]],
			[quote, u_action]
		      ]
		    ]
		  ],
		  
		  [ progn,
		    
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
				 #\('E'),
				 #\(i),
				 #\(t),
				 #\(h),
				 #\(e),
				 #\(r),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(s),
				 #\(h),
				 #\(o),
				 #\(u),
				 #\(l),
				 #\(d),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(b),
				 #\(e),
				 #\(' '),
				 #\(a),
				 #\(n),
				 #\(' '),
				 #\(a),
				 #\(c),
				 #\(t),
				 #\(i),
				 #\(o),
				 #\(n),
				 #\(-),
				 #\(p),
				 #\(l),
				 #\(a),
				 #\(n)
			       ]),
		      sys_name
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(o),
				 #\(r),
				 #\(' '),
				 #\(y),
				 #\(o),
				 #\(u),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(g),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' '),
				 #\(d),
				 #\(e),
				 #\(c),
				 #\(l),
				 #\(a),
				 #\(r),
				 #\(e),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(a),
				 #\(s),
				 #\(' '),
				 #\(t),
				 #\(y),
				 #\(p),
				 #\(e),
				 #\(' '),
				 #\('A'),
				 #\('C'),
				 #\('T'),
				 #\('I'),
				 #\('O'),
				 #\('N')
			       ]),
		      
		      [ u_ob_c36_name,
			
			[ u_ob_c36_get,
			  [u_ob_c36_get, u_rule, [quote, u_goal]],
			  [quote, type]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::CHECK-RULE 
wl: arglist_info(u_check_rule,
		[u_rule, sys_name],
		[Rule_Param, Name_Param],
		arginfo{ all:[u_rule, sys_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, sys_name],
			 opt:0,
			 req:[u_rule, sys_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CHECK-RULE 
wl: init_args(exact_only, u_check_rule).


% annotating U::CHECK-RULE 
f_u_check_rule(Rule_Param, Name_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(sys_name, Name_Param)],
	f_u_null_c63([u_ob_c36_get, u_rule, [quote, u_plausibility]], IFTEST),
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
					   #\(h),
					   #\(a),
					   #\(s),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(' '),
					   #\(p),
					   #\(l),
					   #\(a),
					   #\(u),
					   #\(s),
					   #\(i),
					   #\(b),
					   #\(i),
					   #\(l),
					   #\(i),
					   #\(t),
					   #\(y),
					   #\('.')
					 ]),
				sys_name
			      ],
			      Roman_nl_Ret),
	    f_u_ob_c36_add(Rule_Param, u_plausibility, 1.0, TrueResult),
	    _110496=TrueResult
	;   _110496=[]
	),
	f_u_plan_c63(Rule_Param, IFTEST20),
	(   IFTEST20\==[]
	->  f_u_ob_c36_get(Rule_Param, u_subgoal, IFTEST23),
	    (   IFTEST23\==[]
	    ->  f_u_ob_c36_get(Rule_Param, u_subgoal, Subgoal),
		f_u_ty_c36_instance_c63(Subgoal, u_rand, TrueResult27),
		IFTEST18=TrueResult27
	    ;   IFTEST18=[]
	    )
	;   IFTEST18=[]
	),
	(   IFTEST18\==[]
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
					   #\(s),
					   #\(h),
					   #\(o),
					   #\(u),
					   #\(l),
					   #\(d),
					   #\(' '),
					   #\(b),
					   #\(e),
					   #\(' '),
					   #\('R'),
					   #\('S'),
					   #\('E'),
					   #\('Q'),
					   #\(?)
					 ]),
				sys_name
			      ],
			      TrueResult29),
	    _110572=TrueResult29
	;   _110572=[]
	),
	f_u_ob_c36_get(Rule_Param, u_goal, IFTEST32),
	(   IFTEST32\==[]
	->  f_u_ob_c36_get(Rule_Param, u_goal, Goal),
	    f_u_ty_c36_instance_c63(Goal, u_action, IFTEST35),
	    (   IFTEST35\==[]
	    ->  f_u_action_plan_c63(Rule_Param, Not_Param),
		cl_not(Not_Param, TrueResult39),
		IFTEST30=TrueResult39
	    ;   IFTEST30=[]
	    )
	;   IFTEST30=[]
	),
	(   IFTEST30\==[]
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
					   #\(s),
					   #\(h),
					   #\(o),
					   #\(u),
					   #\(l),
					   #\(d),
					   #\(' '),
					   #\(b),
					   #\(e),
					   #\(' '),
					   #\(a),
					   #\(c),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(-),
					   #\(p),
					   #\(l),
					   #\(a),
					   #\(n),
					   #\(?)
					 ]),
				sys_name
			      ],
			      TrueResult41),
	    _110870=TrueResult41
	;   _110870=[]
	),
	f_u_action_plan_c63(Rule_Param, IFTEST44),
	(   IFTEST44\==[]
	->  f_u_ob_c36_get(Rule_Param, u_goal, IFTEST47),
	    (   IFTEST47\==[]
	    ->  f_u_ob_c36_get(Rule_Param, u_goal, Goal58),
		f_u_ty_c36_instance_c63(Goal58, u_action, Action),
		cl_not(Action, TrueResult51),
		IFTEST42=TrueResult51
	    ;   IFTEST42=[]
	    )
	;   IFTEST42=[]
	),
	(   IFTEST42\==[]
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
					   #\('E'),
					   #\(i),
					   #\(t),
					   #\(h),
					   #\(e),
					   #\(r),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(s),
					   #\(h),
					   #\(o),
					   #\(u),
					   #\(l),
					   #\(d),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(b),
					   #\(e),
					   #\(' '),
					   #\(a),
					   #\(n),
					   #\(' '),
					   #\(a),
					   #\(c),
					   #\(t),
					   #\(i),
					   #\(o),
					   #\(n),
					   #\(-),
					   #\(p),
					   #\(l),
					   #\(a),
					   #\(n)
					 ]),
				sys_name
			      ],
			      Roman_nl_Ret62),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\(y),
					   #\(o),
					   #\(u),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(r),
					   #\(g),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(t),
					   #\(o),
					   #\(' '),
					   #\(d),
					   #\(e),
					   #\(c),
					   #\(l),
					   #\(a),
					   #\(r),
					   #\(e),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(a),
					   #\(s),
					   #\(' '),
					   #\(t),
					   #\(y),
					   #\(p),
					   #\(e),
					   #\(' '),
					   #\('A'),
					   #\('C'),
					   #\('T'),
					   #\('I'),
					   #\('O'),
					   #\('N')
					 ]),
				
				[ u_ob_c36_name,
				  
				  [ u_ob_c36_get,
				    [u_ob_c36_get, u_rule, [quote, u_goal]],
				    [quote, type]
				  ]
				]
			      ],
			      TrueResult53),
	    FnResult=TrueResult53
	;   FnResult=[]
	).
:- set_opv(f_u_check_rule, classof, claz_function),
   set_opv(u_check_rule, compile_as, kw_function),
   set_opv(u_check_rule, function, f_u_check_rule),
   DefunResult=u_check_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8588 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inference?',
			    [rule],
			    
			    [ 'memq?',
			      ['ob$get', rule, [quote, is]],
			      [quote, [[], 'inference-only', 'global-inference']]
			    ]
			  ]).

% annotating U::INFERENCE? 
wl: lambda_def(defun,
	      u_inference_c63,
	      f_u_inference_c63,
	      [u_rule],
	      
	      [ 
		[ u_memq_c63,
		  [u_ob_c36_get, u_rule, [quote, u_is]],
		  [quote, [[], u_inference_only, u_global_inference]]
		]
	      ]).


% annotating U::INFERENCE? 
wl: arglist_info(u_inference_c63,
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

% annotating U::INFERENCE? 
wl: init_args(exact_only, u_inference_c63).


% annotating U::INFERENCE? 
f_u_inference_c63(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_memq_c63([u_ob_c36_get, u_rule, [quote, u_is]],
		     [quote, [[], u_inference_only, u_global_inference]],
		     Memq_c63_Ret),
	Memq_c63_Ret=FnResult.
:- set_opv(f_u_inference_c63, classof, claz_function),
   set_opv(u_inference_c63, compile_as, kw_function),
   set_opv(u_inference_c63, function, f_u_inference_c63),
   DefunResult=u_inference_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8681 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'plan?',
			    [rule],
			    
			    [ memq,
			      ['ob$get', rule, [quote, is]],
			      
			      [ quote,
				
				[ [],
				  'plan-only',
				  'plan-only-no-auto',
				  'action-plan'
				]
			      ]
			    ]
			  ]).

% annotating U::PLAN? 
wl: lambda_def(defun,
	      u_plan_c63,
	      f_u_plan_c63,
	      [u_rule],
	      
	      [ 
		[ ext_memq,
		  [u_ob_c36_get, u_rule, [quote, u_is]],
		  [quote, [[], u_plan_only, u_plan_only_no_auto, u_action_plan]]
		]
	      ]).


% annotating U::PLAN? 
wl: arglist_info(u_plan_c63,
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

% annotating U::PLAN? 
wl: init_args(exact_only, u_plan_c63).


% annotating U::PLAN? 
f_u_plan_c63(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_ext_memq([u_ob_c36_get, u_rule, [quote, u_is]],
		   [quote, [[], u_plan_only, u_plan_only_no_auto, u_action_plan]],
		   Ext_memq_Ret),
	Ext_memq_Ret=FnResult.
:- set_opv(f_u_plan_c63, classof, claz_function),
   set_opv(u_plan_c63, compile_as, kw_function),
   set_opv(u_plan_c63, function, f_u_plan_c63),
   DefunResult=u_plan_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8784 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'plan-only-no-auto?',
			    [rule],
			    
			    [ 'eq?',
			      ['ob$get', rule, [quote, is]],
			      [quote, 'plan-only-no-auto']
			    ]
			  ]).

% annotating U::PLAN-ONLY-NO-AUTO? 
wl: lambda_def(defun,
	      u_plan_only_no_auto_c63,
	      f_u_plan_only_no_auto_c63,
	      [u_rule],
	      
	      [ 
		[ u_eq_c63,
		  [u_ob_c36_get, u_rule, [quote, u_is]],
		  [quote, u_plan_only_no_auto]
		]
	      ]).


% annotating U::PLAN-ONLY-NO-AUTO? 
wl: arglist_info(u_plan_only_no_auto_c63,
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

% annotating U::PLAN-ONLY-NO-AUTO? 
wl: init_args(exact_only, u_plan_only_no_auto_c63).


% annotating U::PLAN-ONLY-NO-AUTO? 
f_u_plan_only_no_auto_c63(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_eq_c63([u_ob_c36_get, u_rule, [quote, u_is]],
		   [quote, u_plan_only_no_auto],
		   Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_plan_only_no_auto_c63, classof, claz_function),
   set_opv(u_plan_only_no_auto_c63, compile_as, kw_function),
   set_opv(u_plan_only_no_auto_c63, function, f_u_plan_only_no_auto_c63),
   DefunResult=u_plan_only_no_auto_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8864 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'global-inference?',
			    [rule],
			    
			    [ 'eq?',
			      ['ob$get', rule, [quote, is]],
			      [quote, 'global-inference']
			    ]
			  ]).

% annotating U::GLOBAL-INFERENCE? 
wl: lambda_def(defun,
	      u_global_inference_c63,
	      f_u_global_inference_c63,
	      [u_rule],
	      
	      [ 
		[ u_eq_c63,
		  [u_ob_c36_get, u_rule, [quote, u_is]],
		  [quote, u_global_inference]
		]
	      ]).


% annotating U::GLOBAL-INFERENCE? 
wl: arglist_info(u_global_inference_c63,
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

% annotating U::GLOBAL-INFERENCE? 
wl: init_args(exact_only, u_global_inference_c63).


% annotating U::GLOBAL-INFERENCE? 
f_u_global_inference_c63(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_eq_c63([u_ob_c36_get, u_rule, [quote, u_is]],
		   [quote, u_global_inference],
		   Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_global_inference_c63, classof, claz_function),
   set_opv(u_global_inference_c63, compile_as, kw_function),
   set_opv(u_global_inference_c63, function, f_u_global_inference_c63),
   DefunResult=u_global_inference_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8942 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'action-plan?',
			    [rule],
			    
			    [ 'eq?',
			      ['ob$get', rule, [quote, is]],
			      [quote, 'action-plan']
			    ]
			  ]).

% annotating U::ACTION-PLAN? 
wl: lambda_def(defun,
	      u_action_plan_c63,
	      f_u_action_plan_c63,
	      [u_rule],
	      
	      [ 
		[ u_eq_c63,
		  [u_ob_c36_get, u_rule, [quote, u_is]],
		  [quote, u_action_plan]
		]
	      ]).


% annotating U::ACTION-PLAN? 
wl: arglist_info(u_action_plan_c63,
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

% annotating U::ACTION-PLAN? 
wl: init_args(exact_only, u_action_plan_c63).


% annotating U::ACTION-PLAN? 
f_u_action_plan_c63(Rule_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_eq_c63([u_ob_c36_get, u_rule, [quote, u_is]],
		   [quote, u_action_plan],
		   Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_action_plan_c63, classof, claz_function),
   set_opv(u_action_plan_c63, compile_as, kw_function),
   set_opv(u_action_plan_c63, function, f_u_action_plan_c63),
   DefunResult=u_action_plan_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8942 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9011)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8942 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Rule chaining mechanism", 1, 9013)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8942 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9039)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:8942 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Compile a new rule into the chaining graph.",
				     1,
				     9042)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-create-chaining',
			    [rule1],
			    
			    [ yloop,
			      [yfor, rule2, in, '*rules*'],
			      
			      [ ydo,
				['rule-create-chaining1', rule1, rule2],
				['rule-create-chaining1', rule2, rule1]
			      ]
			    ]
			  ]).

% annotating U::RULE-CREATE-CHAINING 
wl: lambda_def(defun,
	      u_rule_create_chaining,
	      f_u_rule_create_chaining,
	      [u_rule1],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_rule2, u_in, u_xx_rules_xx],
		  
		  [ u_ydo,
		    [u_rule_create_chaining1, u_rule1, u_rule2],
		    [u_rule_create_chaining1, u_rule2, u_rule1]
		  ]
		]
	      ]).


% annotating U::RULE-CREATE-CHAINING 
wl: arglist_info(u_rule_create_chaining,
		[u_rule1],
		[Rule1_Param],
		arginfo{ all:[u_rule1],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule1],
			 opt:0,
			 req:[u_rule1],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-CREATE-CHAINING 
wl: init_args(exact_only, u_rule_create_chaining).


% annotating U::RULE-CREATE-CHAINING 
f_u_rule_create_chaining(Rule1_Param, FnResult) :-
	Env=[bv(u_rule1, Rule1_Param)],
	f_u_yloop(
		  [ [u_yfor, u_rule2, u_in, u_xx_rules_xx],
		    
		    [ u_ydo,
		      [u_rule_create_chaining1, u_rule1, u_rule2],
		      [u_rule_create_chaining1, u_rule2, u_rule1]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_rule_create_chaining, classof, claz_function),
   set_opv(u_rule_create_chaining, compile_as, kw_function),
   set_opv(u_rule_create_chaining, function, f_u_rule_create_chaining),
   DefunResult=u_rule_create_chaining.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" An alternative tack would be to create two separate graphs: one",
				     1,
				     9258)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" for plans and one for inferences. But note that plans often function",
				     1,
				     9324)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" as inferences. So doing this may involve some difficult-to-predict",
				     1,
				     9395)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" consequences. I am taking the more sure-fire route of putting both",
				     1,
				     9464)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" in the same graph and testing for type of rule upon use (which is",
				     1,
				     9533)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" a fast test anyway; nothing compared to unification).",
				     1,
				     9601)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9657)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" delete slots are ignored for the purpose of the chaining graph.",
				     1,
				     9659)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: might implement rep invariant that for a planning rule, the",
				     1,
				     9726)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" length of the subgoal's objs must be the same as the number of",
				     1,
				     9794)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" embedded patterns.", 1, 9859)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9087 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note that subgoalnum is whacky for inferences.",
				     1,
				     9880)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:9928 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-embedded-subgoals',
			    [rule],
			    
			    [ cond,
			      [['ob$get', rule, [quote, 'embedded-subgoals']]],
			      
			      [ else,
				
				[ 'ob$set',
				  rule,
				  [quote, 'embedded-subgoals'],
				  
				  [ 'embedded-patterns',
				    ['ob$get', rule, [quote, subgoal]]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::RULE-EMBEDDED-SUBGOALS 
wl: lambda_def(defun,
	      u_rule_embedded_subgoals,
	      f_u_rule_embedded_subgoals,
	      [u_rule],
	      
	      [ 
		[ cond,
		  [[u_ob_c36_get, u_rule, [quote, u_embedded_subgoals]]],
		  
		  [ u_else,
		    
		    [ u_ob_c36_set,
		      u_rule,
		      [quote, u_embedded_subgoals],
		      
		      [ u_embedded_patterns,
			[u_ob_c36_get, u_rule, [quote, u_subgoal]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::RULE-EMBEDDED-SUBGOALS 
wl: arglist_info(u_rule_embedded_subgoals,
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

% annotating U::RULE-EMBEDDED-SUBGOALS 
wl: init_args(exact_only, u_rule_embedded_subgoals).


% annotating U::RULE-EMBEDDED-SUBGOALS 
f_u_rule_embedded_subgoals(Rule_Param, ElseResult22) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_ob_c36_get(Rule_Param, u_embedded_subgoals, IFTEST),
	(   IFTEST\==[]
	->  ElseResult22=[]
	;   get_var(Env, u_else, IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_ob_c36_get(Rule_Param, u_subgoal, Subgoal),
		f_u_embedded_patterns(Subgoal, Embedded_subgoals),
		f_u_ob_c36_set(Rule_Param,
			       u_embedded_subgoals,
			       Embedded_subgoals,
			       TrueResult),
		ElseResult22=TrueResult
	    ;   ElseResult22=[]
	    )
	).
:- set_opv(f_u_rule_embedded_subgoals, classof, claz_function),
   set_opv(u_rule_embedded_subgoals, compile_as, kw_function),
   set_opv(u_rule_embedded_subgoals, function, f_u_rule_embedded_subgoals),
   DefunResult=u_rule_embedded_subgoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-create-chaining1',
			    [rule1, rule2],
			    
			    [ let,
			      
			      [ ['rule1-goals', ['ob$gets', rule1, [quote, goal]]],
				
				[ 'rule2-subgoals',
				  ['rule-embedded-subgoals', rule2]
				],
				[dir1, []],
				[dir2, []]
			      ],
			      
			      [ if,
				['eq?', 'rule2-subgoals', [quote, rnot]],
				
				[ progn,
				  
				  [ 'ob$set',
				    rule2,
				    [quote, 'number-of-subgoals'],
				    0
				  ],
				  ['add-chaining', rule1, rule2, 0],
				  t
				],
				
				[ progn,
				  
				  [ 'ob$set',
				    rule2,
				    [quote, 'number-of-subgoals'],
				    [length, 'rule2-subgoals']
				  ],
				  
				  [ yloop,
				    [yfor, 'rule1-goal', in, 'rule1-goals'],
				    
				    [ ydo,
				      
				      [ yloop,
					[initial, [i, 0]],
					
					[ yfor,
					  'rule2-subgoal',
					  in,
					  'rule2-subgoals'
					],
					
					[ ydo,
					  
					  [ if,
					    
					    [ 'possible-unify?',
					      'rule1-goal',
					      'rule2-subgoal'
					    ],
					    
					    [ progn,
					      
					      [ setq,
						dir1,
						
						[ 'ob$unify',
						  'rule1-goal',
						  'rule2-subgoal',
						  '*empty-bd*'
						]
					      ],
					      
					      [ setq,
						dir2,
						
						[ 'ob$unify',
						  'rule2-subgoal',
						  'rule1-goal',
						  '*empty-bd*'
						]
					      ]
					    ],
					    
					    [ progn,
					      [setq, dir1, []],
					      [setq, dir2, []]
					    ]
					  ],
					  
					  [ if,
					    [or, dir1, dir2],
					    
					    [ progn,
					      ['add-chaining', rule1, rule2, i]
					    ]
					  ],
					  [setq, i, [+, i, 1]]
					]
				      ]
				    ],
				    [yresult, t]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::RULE-CREATE-CHAINING1 
wl: lambda_def(defun,
	      u_rule_create_chaining1,
	      f_u_rule_create_chaining1,
	      [u_rule1, u_rule2],
	      
	      [ 
		[ let,
		  
		  [ [u_rule1_goals, [u_ob_c36_gets, u_rule1, [quote, u_goal]]],
		    [u_rule2_subgoals, [u_rule_embedded_subgoals, u_rule2]],
		    [u_dir1, []],
		    [u_dir2, []]
		  ],
		  
		  [ if,
		    [u_eq_c63, u_rule2_subgoals, [quote, u_rnot]],
		    
		    [ progn,
		      [u_ob_c36_set, u_rule2, [quote, u_number_of_subgoals], 0],
		      [u_add_chaining, u_rule1, u_rule2, 0],
		      t
		    ],
		    
		    [ progn,
		      
		      [ u_ob_c36_set,
			u_rule2,
			[quote, u_number_of_subgoals],
			[length, u_rule2_subgoals]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_rule1_goal, u_in, u_rule1_goals],
			
			[ u_ydo,
			  
			  [ u_yloop,
			    [u_initial, [u_i, 0]],
			    [u_yfor, u_rule2_subgoal, u_in, u_rule2_subgoals],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ u_possible_unify_c63,
				  u_rule1_goal,
				  u_rule2_subgoal
				],
				
				[ progn,
				  
				  [ setq,
				    u_dir1,
				    
				    [ u_ob_c36_unify,
				      u_rule1_goal,
				      u_rule2_subgoal,
				      u_xx_empty_bd_xx
				    ]
				  ],
				  
				  [ setq,
				    u_dir2,
				    
				    [ u_ob_c36_unify,
				      u_rule2_subgoal,
				      u_rule1_goal,
				      u_xx_empty_bd_xx
				    ]
				  ]
				],
				[progn, [setq, u_dir1, []], [setq, u_dir2, []]]
			      ],
			      
			      [ if,
				[or, u_dir1, u_dir2],
				[progn, [u_add_chaining, u_rule1, u_rule2, u_i]]
			      ],
			      [setq, u_i, [+, u_i, 1]]
			    ]
			  ]
			],
			[u_yresult, t]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::RULE-CREATE-CHAINING1 
wl: arglist_info(u_rule_create_chaining1,
		[u_rule1, u_rule2],
		[Rule1_Param, Rule2_Param],
		arginfo{ all:[u_rule1, u_rule2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule1, u_rule2],
			 opt:0,
			 req:[u_rule1, u_rule2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-CREATE-CHAINING1 
wl: init_args(exact_only, u_rule_create_chaining1).


% annotating U::RULE-CREATE-CHAINING1 
f_u_rule_create_chaining1(Rule1_Param, Rule2_Param, FnResult) :-
	Env=[bv(u_rule1, Rule1_Param), bv(u_rule2, Rule2_Param)],
	f_u_ob_c36_gets(Rule1_Param, u_goal, Rule1_goals_Init),
	f_u_rule_embedded_subgoals(Rule2_Param, Rule2_subgoals_Init),
	LEnv=[[bv(u_rule1_goals, Rule1_goals_Init), bv(u_rule2_subgoals, Rule2_subgoals_Init), bv(u_dir1, []), bv(u_dir2, [])]|Env],
	f_u_eq_c63(u_rule2_subgoals, [quote, u_rnot], IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_set(Rule2_Param, u_number_of_subgoals, 0, C36_set_Ret),
	    f_u_add_chaining(Rule1_Param, Rule2_Param, 0, Add_chaining_Ret),
	    FnResult=t
	;   get_var(LEnv, u_rule2_subgoals, Rule2_subgoals_Get),
	    cl_length(Rule2_subgoals_Get, Number_of_subgoals),
	    f_u_ob_c36_set(Rule2_Param,
			   u_number_of_subgoals,
			   Number_of_subgoals,
			   C36_set_Ret34),
	    f_u_yloop(
		      [ [u_yfor, u_rule1_goal, u_in, u_rule1_goals],
			
			[ u_ydo,
			  
			  [ u_yloop,
			    [u_initial, [u_i, 0]],
			    [u_yfor, u_rule2_subgoal, u_in, u_rule2_subgoals],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ u_possible_unify_c63,
				  u_rule1_goal,
				  u_rule2_subgoal
				],
				
				[ progn,
				  
				  [ setq,
				    u_dir1,
				    
				    [ u_ob_c36_unify,
				      u_rule1_goal,
				      u_rule2_subgoal,
				      u_xx_empty_bd_xx
				    ]
				  ],
				  
				  [ setq,
				    u_dir2,
				    
				    [ u_ob_c36_unify,
				      u_rule2_subgoal,
				      u_rule1_goal,
				      u_xx_empty_bd_xx
				    ]
				  ]
				],
				[progn, [setq, u_dir1, []], [setq, u_dir2, []]]
			      ],
			      
			      [ if,
				[or, u_dir1, u_dir2],
				[progn, [u_add_chaining, u_rule1, u_rule2, u_i]]
			      ],
			      [setq, u_i, [+, u_i, 1]]
			    ]
			  ]
			],
			[u_yresult, t]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_rule_create_chaining1, classof, claz_function),
   set_opv(u_rule_create_chaining1, compile_as, kw_function),
   set_opv(u_rule_create_chaining1, function, f_u_rule_create_chaining1),
   DefunResult=u_rule_create_chaining1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (if (or (null? dir1) (null? dir2))",
				     1,
				     11040)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               (progn", 1, 11087)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (ndbg-roman-nl *gate-dbg* rule",
				     1,
				     11110)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         \"warning: Asymmetrical unify in rule chaining ~A ~A ~A ~A ~A ~A\"",
				     1,
				     11158)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 rule1 rule2 rule1-goal rule2-subgoal dir1 dir2)",
				     1,
				     11233)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (ndbg-roman-nl *gate-dbg* rule \"Offenders:\")",
				     1,
				     11299)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (po rule1-goal)",
				     1,
				     11361)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (po rule2-subgoal)))",
				     1,
				     11394)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns 'RNOT if RNOT is embedded within.",
				     1,
				     11498)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Also, a pattern having DEPENDENCY is treated as if it were an RNOT",
				     1,
				     11542)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (since DEPENDENCIES don't act as touched obs; will this cause a problem",
				     1,
				     11611)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:10117 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" if ever a dependency but nothing else is asserted? Should never be.)",
				     1,
				     11685)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:11755 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'embedded-patterns',
			    [ob],
			    
			    [ cond,
			      
			      [ 
				[ or,
				  ['ty$instance?', ob, [quote, rseq]],
				  ['ty$instance?', ob, [quote, rand]],
				  ['ty$instance?', ob, [quote, ror]]
				],
				
				[ yloop,
				  [initial, [result, []], [temp, []]],
				  [yfor, y, in, ['ob$gets', ob, [quote, obj]]],
				  
				  [ ydo,
				    [setq, temp, ['embedded-patterns', y]],
				    
				    [ if,
				      ['neq?', temp, [quote, rnot]],
				      [setq, result, ['append!', result, temp]],
				      [setq, result, [quote, rnot]]
				    ]
				  ],
				  [ywhile, ['neq?', result, [quote, rnot]]],
				  [yresult, result]
				]
			      ],
			      [['ty$instance?', ob, [quote, rnot]], [quote, rnot]],
			      
			      [ ['ty$instance?', ob, [quote, dependency]],
				[quote, rnot]
			      ],
			      [else, [list, ob]]
			    ]
			  ]).

% annotating U::EMBEDDED-PATTERNS 
wl: lambda_def(defun,
	      u_embedded_patterns,
	      f_u_embedded_patterns,
	      [u_ob],
	      
	      [ 
		[ cond,
		  
		  [ 
		    [ or,
		      [u_ty_c36_instance_c63, u_ob, [quote, u_rseq]],
		      [u_ty_c36_instance_c63, u_ob, [quote, u_rand]],
		      [u_ty_c36_instance_c63, u_ob, [quote, u_ror]]
		    ],
		    
		    [ u_yloop,
		      [u_initial, [u_result, []], [u_temp, []]],
		      [u_yfor, u_y, u_in, [u_ob_c36_gets, u_ob, [quote, u_obj]]],
		      
		      [ u_ydo,
			[setq, u_temp, [u_embedded_patterns, u_y]],
			
			[ if,
			  [u_neq_c63, u_temp, [quote, u_rnot]],
			  [setq, u_result, [u_append_c33, u_result, u_temp]],
			  [setq, u_result, [quote, u_rnot]]
			]
		      ],
		      [u_ywhile, [u_neq_c63, u_result, [quote, u_rnot]]],
		      [u_yresult, u_result]
		    ]
		  ],
		  [[u_ty_c36_instance_c63, u_ob, [quote, u_rnot]], [quote, u_rnot]],
		  
		  [ [u_ty_c36_instance_c63, u_ob, [quote, u_dependency]],
		    [quote, u_rnot]
		  ],
		  [u_else, [list, u_ob]]
		]
	      ]).


% annotating U::EMBEDDED-PATTERNS 
wl: arglist_info(u_embedded_patterns,
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

% annotating U::EMBEDDED-PATTERNS 
wl: init_args(exact_only, u_embedded_patterns).


% annotating U::EMBEDDED-PATTERNS 
f_u_embedded_patterns(Ob_Param, ElseResult31) :-
	Env=[bv(u_ob, Ob_Param)],
	(   f_u_ty_c36_instance_c63(Ob_Param, u_rseq, FORM1_Res18),
	    FORM1_Res18\==[],
	    IFTEST=FORM1_Res18
	->  true
	;   f_u_ty_c36_instance_c63(Ob_Param, u_rand, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   f_u_ty_c36_instance_c63(Ob_Param, u_ror, Ror),
	    IFTEST=Ror
	),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_initial, [u_result, []], [u_temp, []]],
			[u_yfor, u_y, u_in, [u_ob_c36_gets, u_ob, [quote, u_obj]]],
			
			[ u_ydo,
			  [setq, u_temp, [u_embedded_patterns, u_y]],
			  
			  [ if,
			    [u_neq_c63, u_temp, [quote, u_rnot]],
			    [setq, u_result, [u_append_c33, u_result, u_temp]],
			    [setq, u_result, [quote, u_rnot]]
			  ]
			],
			[u_ywhile, [u_neq_c63, u_result, [quote, u_rnot]]],
			[u_yresult, u_result]
		      ],
		      TrueResult33),
	    ElseResult31=TrueResult33
	;   f_u_ty_c36_instance_c63(Ob_Param, u_rnot, IFTEST19),
	    (   IFTEST19\==[]
	    ->  ElseResult31=u_rnot
	    ;   f_u_ty_c36_instance_c63(Ob_Param, u_dependency, IFTEST22),
		(   IFTEST22\==[]
		->  ElseResult31=u_rnot
		;   get_var(Env, u_else, IFTEST25),
		    (   IFTEST25\==[]
		    ->  ElseResult31=[Ob_Param]
		    ;   ElseResult31=[]
		    )
		)
	    )
	).
:- set_opv(f_u_embedded_patterns, classof, claz_function),
   set_opv(u_embedded_patterns, compile_as, kw_function),
   set_opv(u_embedded_patterns, function, f_u_embedded_patterns),
   DefunResult=u_embedded_patterns.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:11755 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Save chaining info in forms useful both for regular planning/inferencing",
				     1,
				     12340)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:11755 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" and for rule intersection.", 1, 12415)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:12443 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-chaining',
			    ['from-rule', 'to-rule', subgoalnum],
			    
			    [ if,
			      
			      [ not,
				
				[ 'memq?',
				  'to-rule',
				  
				  [ 'ob$gets',
				    'from-rule',
				    [quote, 'forward-chain']
				  ]
				]
			      ],
			      
			      [ progn,
				
				[ 'ob$add',
				  'from-rule',
				  [quote, 'forward-chain'],
				  'to-rule'
				],
				
				[ 'ob$add',
				  'to-rule',
				  [quote, 'backward-chain'],
				  'from-rule'
				]
			      ]
			    ],
			    
			    [ 'ob$add',
			      'from-rule',
			      [quote, 'forward-chain-nums'],
			      [list, 'to-rule', subgoalnum]
			    ],
			    
			    [ 'ob$add',
			      'to-rule',
			      [quote, 'backward-chain-nums'],
			      [list, 'from-rule', subgoalnum]
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      chain,
			      '$STRING'("~&~A subgoal[~A] <-- ~A goal"),
			      'to-rule',
			      subgoalnum,
			      'from-rule'
			    ]
			  ]).

% annotating U::ADD-CHAINING 
wl: lambda_def(defun,
	      u_add_chaining,
	      f_u_add_chaining,
	      [u_from_rule, u_to_rule, u_subgoalnum],
	      
	      [ 
		[ if,
		  
		  [ not,
		    
		    [ u_memq_c63,
		      u_to_rule,
		      [u_ob_c36_gets, u_from_rule, [quote, u_forward_chain]]
		    ]
		  ],
		  
		  [ progn,
		    
		    [ u_ob_c36_add,
		      u_from_rule,
		      [quote, u_forward_chain],
		      u_to_rule
		    ],
		    
		    [ u_ob_c36_add,
		      u_to_rule,
		      [quote, u_backward_chain],
		      u_from_rule
		    ]
		  ]
		],
		
		[ u_ob_c36_add,
		  u_from_rule,
		  [quote, u_forward_chain_nums],
		  [list, u_to_rule, u_subgoalnum]
		],
		
		[ u_ob_c36_add,
		  u_to_rule,
		  [quote, u_backward_chain_nums],
		  [list, u_from_rule, u_subgoalnum]
		],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_chain,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(s),
			     #\(u),
			     #\(b),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
			     #\('['),
			     #\(~),
			     #\('A'),
			     #\(']'),
			     #\(' '),
			     #\(<),
			     #\(-),
			     #\(-),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l)
			   ]),
		  u_to_rule,
		  u_subgoalnum,
		  u_from_rule
		]
	      ]).


% annotating U::ADD-CHAINING 
wl: arglist_info(u_add_chaining,
		[u_from_rule, u_to_rule, u_subgoalnum],
		[From_rule_Param, To_rule_Param, Subgoalnum_Param],
		arginfo{ all:[u_from_rule, u_to_rule, u_subgoalnum],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_from_rule, u_to_rule, u_subgoalnum],
			 opt:0,
			 req:[u_from_rule, u_to_rule, u_subgoalnum],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ADD-CHAINING 
wl: init_args(exact_only, u_add_chaining).


% annotating U::ADD-CHAINING 
f_u_add_chaining(From_rule_Param, To_rule_Param, Subgoalnum_Param, FnResult) :-
	Env=[bv(u_from_rule, From_rule_Param), bv(u_to_rule, To_rule_Param), bv(u_subgoalnum, Subgoalnum_Param)],
	f_u_memq_c63(u_to_rule,
		     [u_ob_c36_gets, u_from_rule, [quote, u_forward_chain]],
		     PredArgResult),
	(   PredArgResult==[]
	->  f_u_ob_c36_add(From_rule_Param,
			   u_forward_chain,
			   To_rule_Param,
			   C36_add_Ret),
	    f_u_ob_c36_add(To_rule_Param,
			   u_backward_chain,
			   From_rule_Param,
			   TrueResult),
	    _108272=TrueResult
	;   _108272=[]
	),
	Forward_chain_nums=[To_rule_Param, Subgoalnum_Param],
	f_u_ob_c36_add(From_rule_Param,
		       u_forward_chain_nums,
		       Forward_chain_nums,
		       C36_add_Ret35),
	Backward_chain_nums=[From_rule_Param, Subgoalnum_Param],
	f_u_ob_c36_add(To_rule_Param,
		       u_backward_chain_nums,
		       Backward_chain_nums,
		       C36_add_Ret36),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_chain,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\(&),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(b),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\('['),
				       #\(~),
				       #\('A'),
				       #\(']'),
				       #\(' '),
				       #\(<),
				       #\(-),
				       #\(-),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l)
				     ]),
			    u_to_rule,
			    u_subgoalnum,
			    u_from_rule
			  ],
			  Roman_nl_Ret),
	Roman_nl_Ret=FnResult.
:- set_opv(f_u_add_chaining, classof, claz_function),
   set_opv(u_add_chaining, compile_as, kw_function),
   set_opv(u_add_chaining, function, f_u_add_chaining),
   DefunResult=u_add_chaining.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:12443 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ndbg-roman-nl *gate-dbg* chain \"Chaining from ~A goal to ~A subgoal[~A]\"",
				     1,
				     12807)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:12443 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 from-rule to-rule subgoalnum)",
				     1,
				     12884)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'remove-chaining',
			    ['from-rule', 'to-rule'],
			    
			    [ 'ob$remove',
			      'from-rule',
			      [quote, 'forward-chain'],
			      'to-rule'
			    ],
			    
			    [ 'ob$remove',
			      'to-rule',
			      [quote, 'backward-chain'],
			      'from-rule'
			    ],
			    
			    [ yloop,
			      
			      [ yfor,
				pair,
				in,
				
				[ 'ob$gets',
				  'from-rule',
				  [quote, 'forward-chain-nums']
				]
			      ],
			      
			      [ ydo,
				
				[ if,
				  ['eq?', 'to-rule', [car, pair]],
				  
				  [ 'ob$remove',
				    'from-rule',
				    [quote, 'forward-chain-nums'],
				    pair
				  ]
				]
			      ]
			    ],
			    
			    [ yloop,
			      
			      [ yfor,
				pair,
				in,
				
				[ 'ob$gets',
				  'to-rule',
				  [quote, 'backward-chain-nums']
				]
			      ],
			      
			      [ ydo,
				
				[ if,
				  ['eq?', 'from-rule', [car, pair]],
				  
				  [ 'ob$remove',
				    'to-rule',
				    [quote, 'backward-chain-nums'],
				    pair
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::REMOVE-CHAINING 
wl: lambda_def(defun,
	      u_remove_chaining,
	      f_u_remove_chaining,
	      [u_from_rule, u_to_rule],
	      
	      [ 
		[ u_ob_c36_remove,
		  u_from_rule,
		  [quote, u_forward_chain],
		  u_to_rule
		],
		
		[ u_ob_c36_remove,
		  u_to_rule,
		  [quote, u_backward_chain],
		  u_from_rule
		],
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_pair,
		    u_in,
		    [u_ob_c36_gets, u_from_rule, [quote, u_forward_chain_nums]]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_eq_c63, u_to_rule, [car, u_pair]],
		      
		      [ u_ob_c36_remove,
			u_from_rule,
			[quote, u_forward_chain_nums],
			u_pair
		      ]
		    ]
		  ]
		],
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_pair,
		    u_in,
		    [u_ob_c36_gets, u_to_rule, [quote, u_backward_chain_nums]]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_eq_c63, u_from_rule, [car, u_pair]],
		      
		      [ u_ob_c36_remove,
			u_to_rule,
			[quote, u_backward_chain_nums],
			u_pair
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::REMOVE-CHAINING 
wl: arglist_info(u_remove_chaining,
		[u_from_rule, u_to_rule],
		[From_rule_Param, To_rule_Param],
		arginfo{ all:[u_from_rule, u_to_rule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_from_rule, u_to_rule],
			 opt:0,
			 req:[u_from_rule, u_to_rule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REMOVE-CHAINING 
wl: init_args(exact_only, u_remove_chaining).


% annotating U::REMOVE-CHAINING 
f_u_remove_chaining(From_rule_Param, To_rule_Param, FnResult) :-
	Env=[bv(u_from_rule, From_rule_Param), bv(u_to_rule, To_rule_Param)],
	f_u_ob_c36_remove(From_rule_Param,
			  u_forward_chain,
			  To_rule_Param,
			  C36_remove_Ret),
	f_u_ob_c36_remove(To_rule_Param,
			  u_backward_chain,
			  From_rule_Param,
			  C36_remove_Ret21),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_pair,
		      u_in,
		      [u_ob_c36_gets, u_from_rule, [quote, u_forward_chain_nums]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_to_rule, [car, u_pair]],
			
			[ u_ob_c36_remove,
			  u_from_rule,
			  [quote, u_forward_chain_nums],
			  u_pair
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_pair,
		      u_in,
		      [u_ob_c36_gets, u_to_rule, [quote, u_backward_chain_nums]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_from_rule, [car, u_pair]],
			
			[ u_ob_c36_remove,
			  u_to_rule,
			  [quote, u_backward_chain_nums],
			  u_pair
			]
		      ]
		    ]
		  ],
		  Yloop_Ret23),
	Yloop_Ret23=FnResult.
:- set_opv(f_u_remove_chaining, classof, claz_function),
   set_opv(u_remove_chaining, compile_as, kw_function),
   set_opv(u_remove_chaining, function, f_u_remove_chaining),
   DefunResult=u_remove_chaining.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ob$set from-rule 'forward-chain-nums",
				     1,
				     13530)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (del! (lambda (x y) (eq? x (car y)))",
				     1,
				     13571)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 to-rule", 1, 13620)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (ob$get from-rule 'forward-chain-nums)))",
				     1,
				     13646)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ob$set to-rule 'backward-chain-nums",
				     1,
				     13705)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (del! (lambda (x y) (eq? x (car y)))",
				     1,
				     13745)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 from-rule", 1, 13794)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (ob$get to-rule 'backward-chain-nums))))",
				     1,
				     13822)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13882)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Rule processes", 1, 13884)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13045 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13901)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-rules',
			    ['top-level-goal', context, 'tlg-switch?'],
			    
			    [ let,
			      
			      [ ['rule-ever-fired?', []],
				['halted?', []],
				[con, []]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("----------------------~A--------------------"),
				['ob->string', context]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Running rules for ~A"),
				'top-level-goal'
			      ],
			      
			      [ if,
				
				[ and,
				  [not, 'tlg-switch?'],
				  
				  [ setq,
				    con,
				    
				    [ 'cx$last-sprout-con',
				      ['cx$parent', context]
				    ]
				  ]
				],
				
				[ progn,
				  
				  [ generate1,
				    con,
				    [quote, [[backtrack, t], [tense, gerund]]],
				    context,
				    '*me-belief-path*'
				  ]
				]
			      ],
			      
			      [ if,
				['null?', ['cx$parent', context]],
				[error, '$STRING'("context has no parent")],
				
				[ 'cx$set-last-sprout-con',
				  ['cx$parent', context],
				  
				  [ 'ob$get',
				    context,
				    [quote, 'first-gened-concept']
				  ]
				]
			      ],
			      
			      [ if,
				'*linearized?*',
				['cx$print-sprout-trace', context]
			      ],
			      
			      [ yloop,
				[initial, [bp, []], ['rule-fired?', []]],
				[yuntil, 'halted?'],
				[yfor, other, in, [others, context]],
				
				[ ydo,
				  [setq, bp, ['->belief-path', other]],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    'rule-xtra',
				    '$STRING'("Belief path is ~A"),
				    bp
				  ],
				  
				  [ setq,
				    'rule-fired?',
				    
				    [ 'perform-goal-successes',
				      'top-level-goal',
				      context,
				      bp
				    ]
				  ],
				  
				  [ setq,
				    'rule-ever-fired?',
				    [or, 'rule-ever-fired?', 'rule-fired?']
				  ],
				  
				  [ if,
				    
				    [ 'neq?',
				      [quote, runable],
				      
				      [ 'ob$get',
					'top-level-goal',
					[quote, status]
				      ]
				    ],
				    [setq, 'halted?', t],
				    
				    [ progn,
				      
				      [ if,
					['me?', other],
					
					[ setq,
					  'rule-ever-fired?',
					  
					  [ or,
					    
					    [ 'run-p-goals',
					      context,
					      'top-level-goal'
					    ],
					    'rule-ever-fired?'
					  ]
					]
				      ],
				      
				      [ setq,
					'rule-ever-fired?',
					
					[ or,
					  
					  [ 'run-plans',
					    'top-level-goal',
					    context,
					    bp
					  ],
					  'rule-ever-fired?'
					]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				[not, 'halted?'],
				['ob$set', context, [quote, 'rules-run?'], t],
				['ob$removes', context, [quote, 'sprout-trace']]
			      ],
			      'rule-ever-fired?'
			    ]
			  ]).

% annotating U::RUN-RULES 
wl: lambda_def(defun,
	      u_run_rules,
	      f_u_run_rules,
	      [u_top_level_goal, u_context, u_tlg_switch_c63],
	      
	      [ 
		[ let,
		  [[u_rule_ever_fired_c63, []], [u_halted_c63, []], [u_con, []]],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(~),
			       #\('A'),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-),
			       #\(-)
			     ]),
		    [u_ob_c62_string, u_context]
		  ],
		  
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
		    u_top_level_goal
		  ],
		  
		  [ if,
		    
		    [ and,
		      [not, u_tlg_switch_c63],
		      
		      [ setq,
			u_con,
			[u_cx_c36_last_sprout_con, [u_cx_c36_parent, u_context]]
		      ]
		    ],
		    
		    [ progn,
		      
		      [ u_generate1,
			u_con,
			[quote, [[u_backtrack, t], [u_tense, u_gerund]]],
			u_context,
			u_xx_me_belief_path_xx
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, [u_cx_c36_parent, u_context]],
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(c),
				 #\(o),
				 #\(n),
				 #\(t),
				 #\(e),
				 #\(x),
				 #\(t),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(s),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(' '),
				 #\(p),
				 #\(a),
				 #\(r),
				 #\(e),
				 #\(n),
				 #\(t)
			       ])
		    ],
		    
		    [ u_cx_c36_set_last_sprout_con,
		      [u_cx_c36_parent, u_context],
		      [u_ob_c36_get, u_context, [quote, u_first_gened_concept]]
		    ]
		  ],
		  
		  [ if,
		    u_xx_linearized_c63_xx,
		    [u_cx_c36_print_sprout_trace, u_context]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_bp, []], [u_rule_fired_c63, []]],
		    [u_yuntil, u_halted_c63],
		    [u_yfor, u_other, u_in, [u_others, u_context]],
		    
		    [ u_ydo,
		      [setq, u_bp, [u_c62_belief_path, u_other]],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule_xtra,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('B'),
				   #\(e),
				   #\(l),
				   #\(i),
				   #\(e),
				   #\(f),
				   #\(' '),
				   #\(p),
				   #\(a),
				   #\(t),
				   #\(h),
				   #\(' '),
				   #\(i),
				   #\(s),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_bp
		      ],
		      
		      [ setq,
			u_rule_fired_c63,
			
			[ u_perform_goal_successes,
			  u_top_level_goal,
			  u_context,
			  u_bp
			]
		      ],
		      
		      [ setq,
			u_rule_ever_fired_c63,
			[or, u_rule_ever_fired_c63, u_rule_fired_c63]
		      ],
		      
		      [ if,
			
			[ u_neq_c63,
			  [quote, u_runable],
			  [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
			],
			[setq, u_halted_c63, t],
			
			[ progn,
			  
			  [ if,
			    [u_me_c63, u_other],
			    
			    [ setq,
			      u_rule_ever_fired_c63,
			      
			      [ or,
				[u_run_p_goals, u_context, u_top_level_goal],
				u_rule_ever_fired_c63
			      ]
			    ]
			  ],
			  
			  [ setq,
			    u_rule_ever_fired_c63,
			    
			    [ or,
			      [u_run_plans, u_top_level_goal, u_context, u_bp],
			      u_rule_ever_fired_c63
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [not, u_halted_c63],
		    [u_ob_c36_set, u_context, [quote, u_rules_run_c63], t],
		    [u_ob_c36_removes, u_context, [quote, u_sprout_trace]]
		  ],
		  u_rule_ever_fired_c63
		]
	      ]).


% annotating U::RUN-RULES 
wl: arglist_info(u_run_rules,
		[u_top_level_goal, u_context, u_tlg_switch_c63],
		[Top_level_goal_Param, Context_Param, Tlg_switch_c63_Param],
		arginfo{ all:[u_top_level_goal, u_context, u_tlg_switch_c63],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_context, u_tlg_switch_c63],
			 opt:0,
			 req:[u_top_level_goal, u_context, u_tlg_switch_c63],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-RULES 
wl: init_args(exact_only, u_run_rules).


% annotating U::RUN-RULES 
f_u_run_rules(Top_level_goal_Param, Context_Param, Tlg_switch_c63_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_tlg_switch_c63, Tlg_switch_c63_Param)],
	LEnv=[[bv(u_rule_ever_fired_c63, []), bv(u_halted_c63, []), bv(u_con, [])]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(~),
				       #\('A'),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-),
				       #\(-)
				     ]),
			    [u_ob_c62_string, u_context]
			  ],
			  Roman_nl_Ret),
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
			    u_top_level_goal
			  ],
			  Roman_nl_Ret57),
	(   Tlg_switch_c63_Param==[]
	->  f_u_cx_c36_parent(Context_Param, Sprout_con_Param),
	    f_u_cx_c36_last_sprout_con(Sprout_con_Param, TrueResult),
	    set_var(LEnv, u_con, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_con, Con_Get),
	    get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    f_u_generate1(Con_Get,
			  [[u_backtrack, t], [u_tense, u_gerund]],
			  Context_Param,
			  Xx_me_belief_path_xx_Get,
			  TrueResult30),
	    _112974=TrueResult30
	;   _112974=[]
	),
	f_u_null_c63([u_cx_c36_parent, u_context], IFTEST31),
	(   IFTEST31\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\(c),
				  #\(o),
				  #\(n),
				  #\(t),
				  #\(e),
				  #\(x),
				  #\(t),
				  #\(' '),
				  #\(h),
				  #\(a),
				  #\(s),
				  #\(' '),
				  #\(n),
				  #\(o),
				  #\(' '),
				  #\(p),
				  #\(a),
				  #\(r),
				  #\(e),
				  #\(n),
				  #\(t)
				])
		     ],
		     TrueResult35),
	    _113378=TrueResult35
	;   f_u_cx_c36_parent(Context_Param, Sprout_con_Param55),
	    f_u_ob_c36_get(Context_Param,
			   u_first_gened_concept,
			   First_gened_concept),
	    f_u_cx_c36_set_last_sprout_con(Sprout_con_Param55,
					   First_gened_concept,
					   ElseResult),
	    _113378=ElseResult
	),
	get_var(LEnv, u_xx_linearized_c63_xx, IFTEST37),
	(   IFTEST37\==[]
	->  f_u_cx_c36_print_sprout_trace(Context_Param, TrueResult41),
	    _113524=TrueResult41
	;   _113524=[]
	),
	f_u_yloop(
		  [ [u_initial, [u_bp, []], [u_rule_fired_c63, []]],
		    [u_yuntil, u_halted_c63],
		    [u_yfor, u_other, u_in, [u_others, u_context]],
		    
		    [ u_ydo,
		      [setq, u_bp, [u_c62_belief_path, u_other]],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule_xtra,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('B'),
				   #\(e),
				   #\(l),
				   #\(i),
				   #\(e),
				   #\(f),
				   #\(' '),
				   #\(p),
				   #\(a),
				   #\(t),
				   #\(h),
				   #\(' '),
				   #\(i),
				   #\(s),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_bp
		      ],
		      
		      [ setq,
			u_rule_fired_c63,
			
			[ u_perform_goal_successes,
			  u_top_level_goal,
			  u_context,
			  u_bp
			]
		      ],
		      
		      [ setq,
			u_rule_ever_fired_c63,
			[or, u_rule_ever_fired_c63, u_rule_fired_c63]
		      ],
		      
		      [ if,
			
			[ u_neq_c63,
			  [quote, u_runable],
			  [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
			],
			[setq, u_halted_c63, t],
			
			[ progn,
			  
			  [ if,
			    [u_me_c63, u_other],
			    
			    [ setq,
			      u_rule_ever_fired_c63,
			      
			      [ or,
				[u_run_p_goals, u_context, u_top_level_goal],
				u_rule_ever_fired_c63
			      ]
			    ]
			  ],
			  
			  [ setq,
			    u_rule_ever_fired_c63,
			    
			    [ or,
			      [u_run_plans, u_top_level_goal, u_context, u_bp],
			      u_rule_ever_fired_c63
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_halted_c63, Halted_c63_Get),
	(   Halted_c63_Get==[]
	->  f_u_ob_c36_set(Context_Param, u_rules_run_c63, t, TrueResult48),
	    _113716=TrueResult48
	;   f_u_ob_c36_removes(Context_Param, u_sprout_trace, ElseResult49),
	    _113716=ElseResult49
	),
	get_var(LEnv, u_rule_ever_fired_c63, Rule_ever_fired_c63_Get),
	LetResult=Rule_ever_fired_c63_Get,
	LetResult=FnResult.
:- set_opv(f_u_run_rules, classof, claz_function),
   set_opv(u_run_rules, compile_as, kw_function),
   set_opv(u_run_rules, function, f_u_run_rules),
   DefunResult=u_run_rules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Experimentally, run inferences if above did not",
				     10,
				     15174)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" run and touched facts.", 10, 15233)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ---> put this into perform-goal-successes instead....",
				     10,
				     15267)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If it works, can get rid of temp variable above.",
				     10,
				     15332)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (if (and (me-belief-path? bp)",
				     1,
				     15383)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (null? rule-fired?)",
				     1,
				     15423)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (ob$get context 'touched-facts))",
				     1,
				     15462)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (run-inferences context top-level-goal bp))",
				     1,
				     15514)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Todo: actually, we", 48, 16123)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" should only flag it not to be reprinted. For later",
				     8,
				     16151)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:13903 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" graphics and debugging we would like to leave it.",
				     8,
				     16211)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:16289 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-goal-successes',
			    ['top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Perform goal successes in ~A"),
			      context
			    ],
			    
			    [ yloop,
			      [initial, ['ever-fired?', []]],
			      
			      [ ywhile,
				
				[ and,
				  
				  [ 'perform-goal-successes1',
				    'top-level-goal',
				    context,
				    'belief-path'
				  ],
				  
				  [ 'eq?',
				    [quote, runable],
				    ['ob$get', 'top-level-goal', [quote, status]]
				  ],
				  
				  [ progn,
				    
				    [ if,
				      ['me-belief-path?', 'belief-path'],
				      ['entered-concept-serendipity']
				    ],
				    t
				  ]
				]
			      ],
			      [ydo, [setq, 'ever-fired?', t]],
			      [yresult, 'ever-fired?']
			    ]
			  ]).

% annotating U::PERFORM-GOAL-SUCCESSES 
wl: lambda_def(defun,
	      u_perform_goal_successes,
	      f_u_perform_goal_successes,
	      [u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(e),
			     #\(r),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(m),
			     #\(' '),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(s),
			     #\(u),
			     #\(c),
			     #\(c),
			     #\(e),
			     #\(s),
			     #\(s),
			     #\(e),
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
		  [u_initial, [u_ever_fired_c63, []]],
		  
		  [ u_ywhile,
		    
		    [ and,
		      
		      [ u_perform_goal_successes1,
			u_top_level_goal,
			u_context,
			u_belief_path
		      ],
		      
		      [ u_eq_c63,
			[quote, u_runable],
			[u_ob_c36_get, u_top_level_goal, [quote, u_status]]
		      ],
		      
		      [ progn,
			
			[ if,
			  [u_me_belief_path_c63, u_belief_path],
			  [u_entered_concept_serendipity]
			],
			t
		      ]
		    ]
		  ],
		  [u_ydo, [setq, u_ever_fired_c63, t]],
		  [u_yresult, u_ever_fired_c63]
		]
	      ]).


% annotating U::PERFORM-GOAL-SUCCESSES 
wl: arglist_info(u_perform_goal_successes,
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

% annotating U::PERFORM-GOAL-SUCCESSES 
wl: init_args(exact_only, u_perform_goal_successes).


% annotating U::PERFORM-GOAL-SUCCESSES 
f_u_perform_goal_successes(Top_level_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(e),
				       #\(r),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
				       #\(' '),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(c),
				       #\(c),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(e),
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
		  [ [u_initial, [u_ever_fired_c63, []]],
		    
		    [ u_ywhile,
		      
		      [ and,
			
			[ u_perform_goal_successes1,
			  u_top_level_goal,
			  u_context,
			  u_belief_path
			],
			
			[ u_eq_c63,
			  [quote, u_runable],
			  [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
			],
			
			[ progn,
			  
			  [ if,
			    [u_me_belief_path_c63, u_belief_path],
			    [u_entered_concept_serendipity]
			  ],
			  t
			]
		      ]
		    ],
		    [u_ydo, [setq, u_ever_fired_c63, t]],
		    [u_yresult, u_ever_fired_c63]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_perform_goal_successes, classof, claz_function),
   set_opv(u_perform_goal_successes, compile_as, kw_function),
   set_opv(u_perform_goal_successes, function, f_u_perform_goal_successes),
   DefunResult=u_perform_goal_successes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:16816 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-goal-successes1',
			    ['top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Goal successes cycle")
			    ],
			    
			    [ 'let*',
			      
			      [ 
				[ fired1,
				  
				  [ 'perform-non-action-goal-successes',
				    'top-level-goal',
				    context,
				    'belief-path'
				  ]
				],
				
				[ fired2,
				  
				  [ 'perform-action-goal-successes',
				    'top-level-goal',
				    context,
				    'belief-path',
				    fired1
				  ]
				]
			      ],
			      [or, fired1, fired2]
			    ]
			  ]).

% annotating U::PERFORM-GOAL-SUCCESSES1 
wl: lambda_def(defun,
	      u_perform_goal_successes1,
	      f_u_perform_goal_successes1,
	      [u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('G'),
			     #\(o),
			     #\(a),
			     #\(l),
			     #\(' '),
			     #\(s),
			     #\(u),
			     #\(c),
			     #\(c),
			     #\(e),
			     #\(s),
			     #\(s),
			     #\(e),
			     #\(s),
			     #\(' '),
			     #\(c),
			     #\(y),
			     #\(c),
			     #\(l),
			     #\(e)
			   ])
		],
		
		[ let_xx,
		  
		  [ 
		    [ u_fired1,
		      
		      [ u_perform_non_action_goal_successes,
			u_top_level_goal,
			u_context,
			u_belief_path
		      ]
		    ],
		    
		    [ u_fired2,
		      
		      [ u_perform_action_goal_successes,
			u_top_level_goal,
			u_context,
			u_belief_path,
			u_fired1
		      ]
		    ]
		  ],
		  [or, u_fired1, u_fired2]
		]
	      ]).


% annotating U::PERFORM-GOAL-SUCCESSES1 
wl: arglist_info(u_perform_goal_successes1,
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

% annotating U::PERFORM-GOAL-SUCCESSES1 
wl: init_args(exact_only, u_perform_goal_successes1).


% annotating U::PERFORM-GOAL-SUCCESSES1 
f_u_perform_goal_successes1(Top_level_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('G'),
				       #\(o),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(c),
				       #\(c),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(c),
				       #\(y),
				       #\(c),
				       #\(l),
				       #\(e)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_perform_non_action_goal_successes(Top_level_goal_Param,
					      Context_Param,
					      Belief_path_Param,
					      Fired1_Init),
	LEnv=[[bv(u_fired1, Fired1_Init)]|Env],
	get_var(LEnv, u_fired1, Fired1_Get),
	f_u_perform_action_goal_successes(Top_level_goal_Param,
					  Context_Param,
					  Belief_path_Param,
					  Fired1_Get,
					  Fired2_Init),
	Env=[[bv(u_fired2, Fired2_Init)]|LEnv],
	(   get_var(Env, u_fired1, Fired1_Get30),
	    Fired1_Get30\==[],
	    FnResult=Fired1_Get30
	->  true
	;   get_var(Env, u_fired2, Fired2_Get),
	    FnResult=Fired2_Get
	).
:- set_opv(f_u_perform_goal_successes1, classof, claz_function),
   set_opv(u_perform_goal_successes1, compile_as, kw_function),
   set_opv(u_perform_goal_successes1, function, f_u_perform_goal_successes1),
   DefunResult=u_perform_goal_successes1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:17257 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*enter-concepts?*', []]).
:- set_var(TLEnv3, setq, u_xx_enter_concepts_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:17287 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-action-goal-successes',
			    ['top-level-goal', context, 'belief-path', 'fired1?'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Perform action goal successes in ~A"),
			      context
			    ],
			    
			    [ let,
			      
			      [ ['fired?', []],
				['old-enter-concepts', '*enter-concepts?*'],
				[concepts, []]
			      ],
			      [setq, '*enter-concepts?*', []],
			      
			      [ yloop,
				
				[ yfor,
				  ob,
				  in,
				  
				  [ if,
				    ['me-belief-path?', 'belief-path'],
				    
				    [ 'cx$get-all-ty',
				      context,
				      '*active-goal-ob*'
				    ],
				    ['cx$get-all-ty', context, '*believe-ob*']
				  ]
				],
				
				[ ywhile,
				  
				  [ 'eq?',
				    [quote, runable],
				    ['ob$get', 'top-level-goal', [quote, status]]
				  ]
				],
				
				[ ydo,
				  
				  [ setq,
				    'fired?',
				    
				    [ or,
				      
				      [ 'perform-action-goal-success',
					ob,
					'top-level-goal',
					context,
					'belief-path'
				      ],
				      'fired?'
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				['ob$get', context, [quote, 'touched-facts']],
				
				[ progn,
				  
				  [ 'run-inferences',
				    context,
				    'top-level-goal',
				    'belief-path'
				  ],
				  
				  [ if,
				    
				    [ and,
				      'fired?',
				      ['not-me-belief-path?', 'belief-path']
				    ],
				    
				    [ 'run-inferences',
				      context,
				      'top-level-goal',
				      '*me-belief-path*'
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				'*enter-concepts?*',
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Taking optional concept input")
				  ],
				  
				  [ if,
				    
				    [ setq,
				      concepts,
				      ['enter-concepts', context, 'belief-path']
				    ],
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("Concepts entered")
				      ],
				      
				      [ 'run-inferences',
					context,
					'top-level-goal',
					'belief-path'
				      ],
				      
				      [ if,
					['me-belief-path?', 'belief-path'],
					['rule-induction', concepts, context]
				      ]
				    ]
				  ]
				]
			      ],
			      [setq, '*enter-concepts?*', 'old-enter-concepts'],
			      'fired?'
			    ]
			  ]).

% annotating U::PERFORM-ACTION-GOAL-SUCCESSES 
wl: lambda_def(defun,
	      u_perform_action_goal_successes,
	      f_u_perform_action_goal_successes,
	      [u_top_level_goal, u_context, u_belief_path, u_fired1_c63],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(e),
			     #\(r),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(m),
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
			     #\(s),
			     #\(u),
			     #\(c),
			     #\(c),
			     #\(e),
			     #\(s),
			     #\(s),
			     #\(e),
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
		
		[ let,
		  
		  [ [u_fired_c63, []],
		    [u_old_enter_concepts, u_xx_enter_concepts_c63_xx],
		    [u_concepts, []]
		  ],
		  [setq, u_xx_enter_concepts_c63_xx, []],
		  
		  [ u_yloop,
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			[u_me_belief_path_c63, u_belief_path],
			[u_cx_c36_get_all_ty, u_context, u_xx_active_goal_ob_xx],
			[u_cx_c36_get_all_ty, u_context, u_xx_believe_ob_xx]
		      ]
		    ],
		    
		    [ u_ywhile,
		      
		      [ u_eq_c63,
			[quote, u_runable],
			[u_ob_c36_get, u_top_level_goal, [quote, u_status]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_fired_c63,
			
			[ or,
			  
			  [ u_perform_action_goal_success,
			    u_ob,
			    u_top_level_goal,
			    u_context,
			    u_belief_path
			  ],
			  u_fired_c63
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_ob_c36_get, u_context, [quote, u_touched_facts]],
		    
		    [ progn,
		      
		      [ u_run_inferences,
			u_context,
			u_top_level_goal,
			u_belief_path
		      ],
		      
		      [ if,
			
			[ and,
			  u_fired_c63,
			  [u_not_me_belief_path_c63, u_belief_path]
			],
			
			[ u_run_inferences,
			  u_context,
			  u_top_level_goal,
			  u_xx_me_belief_path_xx
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_xx_enter_concepts_c63_xx,
		    
		    [ progn,
		      
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
		      
		      [ if,
			
			[ setq,
			  u_concepts,
			  [u_enter_concepts, u_context, u_belief_path]
			],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('C'),
				       #\(o),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(p),
				       #\(t),
				       #\(s),
				       #\(' '),
				       #\(e),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(d)
				     ])
			  ],
			  
			  [ u_run_inferences,
			    u_context,
			    u_top_level_goal,
			    u_belief_path
			  ],
			  
			  [ if,
			    [u_me_belief_path_c63, u_belief_path],
			    [u_rule_induction, u_concepts, u_context]
			  ]
			]
		      ]
		    ]
		  ],
		  [setq, u_xx_enter_concepts_c63_xx, u_old_enter_concepts],
		  u_fired_c63
		]
	      ]).


% annotating U::PERFORM-ACTION-GOAL-SUCCESSES 
wl: arglist_info(u_perform_action_goal_successes,
		[u_top_level_goal, u_context, u_belief_path, u_fired1_c63],
		
		[ Top_level_goal_Param,
		  Context_Param,
		  Belief_path_Param,
		  Fired1_c63_Param
		],
		arginfo{ all:
			     [ u_top_level_goal,
			       u_context,
			       u_belief_path,
			       u_fired1_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_top_level_goal,
				 u_context,
				 u_belief_path,
				 u_fired1_c63
			       ],
			 opt:0,
			 req:
			     [ u_top_level_goal,
			       u_context,
			       u_belief_path,
			       u_fired1_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERFORM-ACTION-GOAL-SUCCESSES 
wl: init_args(exact_only, u_perform_action_goal_successes).


% annotating U::PERFORM-ACTION-GOAL-SUCCESSES 
f_u_perform_action_goal_successes(Top_level_goal_Param, Context_Param, Belief_path_Param, Fired1_c63_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param), bv(u_fired1_c63, Fired1_c63_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(e),
				       #\(r),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
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
				       #\(s),
				       #\(u),
				       #\(c),
				       #\(c),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(e),
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
	get_var(Env, u_xx_enter_concepts_c63_xx, Xx_enter_concepts_c63_xx_Get),
	LEnv=[[bv(u_fired_c63, []), bv(u_old_enter_concepts, Xx_enter_concepts_c63_xx_Get), bv(u_concepts, [])]|Env],
	set_var(LEnv, setq, u_xx_enter_concepts_c63_xx, []),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			[u_me_belief_path_c63, u_belief_path],
			[u_cx_c36_get_all_ty, u_context, u_xx_active_goal_ob_xx],
			[u_cx_c36_get_all_ty, u_context, u_xx_believe_ob_xx]
		      ]
		    ],
		    
		    [ u_ywhile,
		      
		      [ u_eq_c63,
			[quote, u_runable],
			[u_ob_c36_get, u_top_level_goal, [quote, u_status]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_fired_c63,
			
			[ or,
			  
			  [ u_perform_action_goal_success,
			    u_ob,
			    u_top_level_goal,
			    u_context,
			    u_belief_path
			  ],
			  u_fired_c63
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	f_u_ob_c36_get(Context_Param, u_touched_facts, IFTEST),
	(   IFTEST\==[]
	->  f_u_run_inferences(Context_Param,
			       Top_level_goal_Param,
			       Belief_path_Param,
			       Run_inferences_Ret),
	    get_var(LEnv, u_fired_c63, IFTEST31),
	    (   IFTEST31\==[]
	    ->  f_u_not_me_belief_path_c63(u_belief_path, TrueResult),
		IFTEST29=TrueResult
	    ;   IFTEST29=[]
	    ),
	    (   IFTEST29\==[]
	    ->  get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
		f_u_run_inferences(Context_Param,
				   Top_level_goal_Param,
				   Xx_me_belief_path_xx_Get,
				   TrueResult38),
		TrueResult39=TrueResult38
	    ;   TrueResult39=[]
	    )
	;   TrueResult39=[]
	),
	get_var(LEnv, u_xx_enter_concepts_c63_xx, IFTEST40),
	(   IFTEST40\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
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
			      Roman_nl_Ret64),
	    f_u_enter_concepts(Context_Param, Belief_path_Param, IFTEST43),
	    set_var(LEnv, u_concepts, IFTEST43),
	    (   IFTEST43\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('C'),
					       #\(o),
					       #\(n),
					       #\(c),
					       #\(e),
					       #\(p),
					       #\(t),
					       #\(s),
					       #\(' '),
					       #\(e),
					       #\(n),
					       #\(t),
					       #\(e),
					       #\(r),
					       #\(e),
					       #\(d)
					     ])
				  ],
				  Roman_nl_Ret65),
		f_u_run_inferences(Context_Param,
				   Top_level_goal_Param,
				   Belief_path_Param,
				   Run_inferences_Ret66),
		f_u_me_belief_path_c63(u_belief_path, IFTEST50),
		(   IFTEST50\==[]
		->  get_var(LEnv, u_concepts, Concepts_Get),
		    f_u_rule_induction(Concepts_Get,
				       Context_Param,
				       TrueResult54),
		    TrueResult55=TrueResult54
		;   TrueResult55=[]
		)
	    ;   TrueResult55=[]
	    )
	;   TrueResult55=[]
	),
	get_var(LEnv, u_old_enter_concepts, Old_enter_concepts_Get),
	set_var(LEnv, u_xx_enter_concepts_c63_xx, Old_enter_concepts_Get),
	get_var(LEnv, u_fired_c63, Fired_c63_Get58),
	LetResult=Fired_c63_Get58,
	LetResult=FnResult.
:- set_opv(f_u_perform_action_goal_successes, classof, claz_function),
   set_opv(u_perform_action_goal_successes, compile_as, kw_function),
   set_opv(u_perform_action_goal_successes,
	   function,
	   f_u_perform_action_goal_successes),
   DefunResult=u_perform_action_goal_successes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:17287 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Will get asserted active goals from previous sprouting.",
				     8,
				     18073)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:17287 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (or fired? (eq? fired1? 'inferences))",
				     41,
				     18171)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:18941 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-action-goal-success',
			    [ob, 'top-level-goal', context, 'belief-path'],
			    
			    [ if,
			      
			      [ if,
				['me-belief-path?', 'belief-path'],
				
				[ and,
				  
				  [ 'eq?',
				    'top-level-goal',
				    ['ob$get', ob, [quote, 'top-level-goal']]
				  ],
				  
				  [ 'ty$instance?',
				    ['ob$get', ob, [quote, obj]],
				    [quote, action]
				  ],
				  
				  [ 'subgoals-completed?',
				    ob,
				    context,
				    'belief-path'
				  ]
				],
				
				[ and,
				  
				  [ setq,
				    ob,
				    ['absolute->relative', ob, 'belief-path']
				  ],
				  ['ty$instance?', ob, [quote, 'active-goal']],
				  
				  [ 'eq?',
				    'top-level-goal',
				    ['ob$get', ob, [quote, 'top-level-goal']]
				  ],
				  
				  [ 'ty$instance?',
				    ['ob$get', ob, [quote, obj]],
				    [quote, action]
				  ],
				  
				  [ 'subgoals-completed?',
				    ob,
				    context,
				    'belief-path'
				  ]
				]
			      ],
			      
			      [ let,
				
				[ 
				  [ rule,
				    
				    [ 'goal-subgoals-rule',
				      ob,
				      context,
				      'belief-path'
				    ]
				  ],
				  
				  [ actors,
				    
				    [ 'ob$gets',
				      ['ob$get', ob, [quote, obj]],
				      [quote, actor]
				    ]
				  ]
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'rule-long',
				  '$STRING'("Perform action goal success for ~A in ~A"),
				  ob,
				  context
				],
				
				[ if,
				  ['performance-mode?'],
				  
				  [ cond,
				    
				    [ ['vars-in?', ['ob$get', ob, [quote, obj]]],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("About to perform real action but variables in action")
				      ],
				      
				      [ 'change-tlg-status',
					'top-level-goal',
					[quote, halted]
				      ],
				      []
				    ],
				    
				    [ 
				      [ and,
					['me-in?', actors],
					['non-mes', actors]
				      ],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("Perform external shared action")
				      ],
				      
				      [ 'perform-action',
					ob,
					'top-level-goal',
					context,
					'belief-path',
					rule,
					t
				      ]
				    ],
				    
				    [ ['non-mes', actors],
				      
				      [ 'perform-other-action',
					ob,
					'top-level-goal',
					context,
					'belief-path',
					rule
				      ]
				    ],
				    
				    [ else,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("Perform external action")
				      ],
				      
				      [ 'perform-action',
					ob,
					'top-level-goal',
					context,
					'belief-path',
					rule,
					[]
				      ],
				      
				      [ if,
					['me-belief-path?', 'belief-path'],
					[setq, '*enter-concepts?*', t]
				      ],
				      t
				    ]
				  ],
				  
				  [ if,
				    
				    [ 'eq?',
				      [quote, real],
				      
				      [ 'ob$get',
					'top-level-goal',
					[quote, 'planning-type']
				      ]
				    ],
				    
				    [ progn,
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					rule,
					'$STRING'("About to perform real action but not in performance mode")
				      ],
				      
				      [ 'change-tlg-status',
					'top-level-goal',
					[quote, waiting]
				      ],
				      []
				    ],
				    
				    [ 'perform-action',
				      ob,
				      'top-level-goal',
				      context,
				      'belief-path',
				      rule,
				      []
				    ]
				  ]
				]
			      ],
			      []
			    ]
			  ]).

% annotating U::PERFORM-ACTION-GOAL-SUCCESS 
wl: lambda_def(defun,
	      u_perform_action_goal_success,
	      f_u_perform_action_goal_success,
	      [u_ob, u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ if,
		  
		  [ if,
		    [u_me_belief_path_c63, u_belief_path],
		    
		    [ and,
		      
		      [ u_eq_c63,
			u_top_level_goal,
			[u_ob_c36_get, u_ob, [quote, u_top_level_goal]]
		      ],
		      
		      [ u_ty_c36_instance_c63,
			[u_ob_c36_get, u_ob, [quote, u_obj]],
			[quote, u_action]
		      ],
		      [u_subgoals_completed_c63, u_ob, u_context, u_belief_path]
		    ],
		    
		    [ and,
		      [setq, u_ob, [u_absolute_c62_relative, u_ob, u_belief_path]],
		      [u_ty_c36_instance_c63, u_ob, [quote, u_active_goal]],
		      
		      [ u_eq_c63,
			u_top_level_goal,
			[u_ob_c36_get, u_ob, [quote, u_top_level_goal]]
		      ],
		      
		      [ u_ty_c36_instance_c63,
			[u_ob_c36_get, u_ob, [quote, u_obj]],
			[quote, u_action]
		      ],
		      [u_subgoals_completed_c63, u_ob, u_context, u_belief_path]
		    ]
		  ],
		  
		  [ let,
		    
		    [ 
		      [ u_rule,
			[u_goal_subgoals_rule, u_ob, u_context, u_belief_path]
		      ],
		      
		      [ u_actors,
			
			[ u_ob_c36_gets,
			  [u_ob_c36_get, u_ob, [quote, u_obj]],
			  [quote, u_actor]
			]
		      ]
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule_long,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('P'),
				 #\(e),
				 #\(r),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(m),
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
				 #\(s),
				 #\(u),
				 #\(c),
				 #\(c),
				 #\(e),
				 #\(s),
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
		      u_ob,
		      u_context
		    ],
		    
		    [ if,
		      [u_performance_mode_c63],
		      
		      [ cond,
			
			[ [u_vars_in_c63, [u_ob_c36_get, u_ob, [quote, u_obj]]],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
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
				       #\(p),
				       #\(e),
				       #\(r),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(b),
				       #\(u),
				       #\(t),
				       #\(' '),
				       #\(v),
				       #\(a),
				       #\(r),
				       #\(i),
				       #\(a),
				       #\(b),
				       #\(l),
				       #\(e),
				       #\(s),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n)
				     ])
			  ],
			  
			  [ u_change_tlg_status,
			    u_top_level_goal,
			    [quote, u_halted]
			  ],
			  []
			],
			
			[ [and, [u_me_in_c63, u_actors], [u_non_mes, u_actors]],
			  
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
				       #\(' '),
				       #\(e),
				       #\(x),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(n),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(s),
				       #\(h),
				       #\(a),
				       #\(r),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n)
				     ])
			  ],
			  
			  [ u_perform_action,
			    u_ob,
			    u_top_level_goal,
			    u_context,
			    u_belief_path,
			    u_rule,
			    t
			  ]
			],
			
			[ [u_non_mes, u_actors],
			  
			  [ u_perform_other_action,
			    u_ob,
			    u_top_level_goal,
			    u_context,
			    u_belief_path,
			    u_rule
			  ]
			],
			
			[ u_else,
			  
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
				       #\(' '),
				       #\(e),
				       #\(x),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(n),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n)
				     ])
			  ],
			  
			  [ u_perform_action,
			    u_ob,
			    u_top_level_goal,
			    u_context,
			    u_belief_path,
			    u_rule,
			    []
			  ],
			  
			  [ if,
			    [u_me_belief_path_c63, u_belief_path],
			    [setq, u_xx_enter_concepts_c63_xx, t]
			  ],
			  t
			]
		      ],
		      
		      [ if,
			
			[ u_eq_c63,
			  [quote, real],
			  
			  [ u_ob_c36_get,
			    u_top_level_goal,
			    [quote, u_planning_type]
			  ]
			],
			
			[ progn,
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_rule,
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
				       #\(p),
				       #\(e),
				       #\(r),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(l),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(b),
				       #\(u),
				       #\(t),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(t),
				       #\(' '),
				       #\(i),
				       #\(n),
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
			  
			  [ u_change_tlg_status,
			    u_top_level_goal,
			    [quote, u_waiting]
			  ],
			  []
			],
			
			[ u_perform_action,
			  u_ob,
			  u_top_level_goal,
			  u_context,
			  u_belief_path,
			  u_rule,
			  []
			]
		      ]
		    ]
		  ],
		  []
		]
	      ]).


% annotating U::PERFORM-ACTION-GOAL-SUCCESS 
wl: arglist_info(u_perform_action_goal_success,
		[u_ob, u_top_level_goal, u_context, u_belief_path],
		
		[ Ob_Param,
		  Top_level_goal_Param,
		  Context_Param,
		  Belief_path_Param
		],
		arginfo{ all:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERFORM-ACTION-GOAL-SUCCESS 
wl: init_args(exact_only, u_perform_action_goal_success).


% annotating U::PERFORM-ACTION-GOAL-SUCCESS 
f_u_perform_action_goal_success(Ob_Param, Top_level_goal_Param, Context_Param, Belief_path_Param, ElseResult100) :-
	Env=[bv(u_ob, Ob_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_me_belief_path_c63(u_belief_path, IFTEST20),
	(   IFTEST20\==[]
	->  f_u_eq_c63(u_top_level_goal,
		       [u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
		       IFTEST22),
	    (   IFTEST22\==[]
	    ->  get_var(Env, u_ob, Ob_Get),
		f_u_ob_c36_get(Ob_Get, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_action, IFTEST24),
		(   IFTEST24\==[]
		->  get_var(Env, u_ob, Ob_Get27),
		    f_u_subgoals_completed_c63(Ob_Get27,
					       Context_Param,
					       Belief_path_Param,
					       TrueResult),
		    IFTEST=TrueResult
		;   IFTEST=[]
		)
	    ;   IFTEST=[]
	    )
	;   get_var(Env, u_ob, Ob_Get34),
	    f_u_absolute_c62_relative(Ob_Get34, Belief_path_Param, IFTEST32),
	    set_var(Env, u_ob, IFTEST32),
	    (   IFTEST32\==[]
	    ->  get_var(Env, u_ob, Ob_Get38),
		f_u_ty_c36_instance_c63(Ob_Get38, u_active_goal, IFTEST36),
		(   IFTEST36\==[]
		->  f_u_eq_c63(u_top_level_goal,
			       [u_ob_c36_get, u_ob, [quote, u_top_level_goal]],
			       IFTEST39),
		    (   IFTEST39\==[]
		    ->  get_var(Env, u_ob, Ob_Get43),
			f_u_ob_c36_get(Ob_Get43, u_obj, Obj119),
			f_u_ty_c36_instance_c63(Obj119, u_action, IFTEST41),
			(   IFTEST41\==[]
			->  get_var(Env, u_ob, Ob_Get44),
			    f_u_subgoals_completed_c63(Ob_Get44,
						       Context_Param,
						       Belief_path_Param,
						       TrueResult47),
			    IFTEST=TrueResult47
			;   IFTEST=[]
			)
		    ;   IFTEST=[]
		    )
		;   IFTEST=[]
		)
	    ;   IFTEST=[]
	    )
	),
	(   IFTEST\==[]
	->  get_var(Env, u_ob, Ob_Get55),
	    f_u_goal_subgoals_rule(Ob_Get55,
				   Context_Param,
				   Belief_path_Param,
				   Rule_Init),
	    get_var(Env, u_ob, Ob_Get58),
	    f_u_ob_c36_get(Ob_Get58, u_obj, Obj120),
	    f_u_ob_c36_gets(Obj120, u_actor, Actors_Init),
	    LEnv=[[bv(u_rule, Rule_Init), bv(u_actors, Actors_Init)]|Env],
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('P'),
					   #\(e),
					   #\(r),
					   #\(f),
					   #\(o),
					   #\(r),
					   #\(m),
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
					   #\(s),
					   #\(u),
					   #\(c),
					   #\(c),
					   #\(e),
					   #\(s),
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
				u_ob,
				u_context
			      ],
			      Roman_nl_Ret),
	    f_u_performance_mode_c63(IFTEST61),
	    (   IFTEST61\==[]
	    ->  get_var(LEnv, u_ob, Ob_Get65),
		f_u_ob_c36_get(Ob_Get65, u_obj, Obj121),
		f_u_vars_in_c63(Obj121, IFTEST63),
		(   IFTEST63\==[]
		->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				      u_rule,
				      
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
						   #\(p),
						   #\(e),
						   #\(r),
						   #\(f),
						   #\(o),
						   #\(r),
						   #\(m),
						   #\(' '),
						   #\(r),
						   #\(e),
						   #\(a),
						   #\(l),
						   #\(' '),
						   #\(a),
						   #\(c),
						   #\(t),
						   #\(i),
						   #\(o),
						   #\(n),
						   #\(' '),
						   #\(b),
						   #\(u),
						   #\(t),
						   #\(' '),
						   #\(v),
						   #\(a),
						   #\(r),
						   #\(i),
						   #\(a),
						   #\(b),
						   #\(l),
						   #\(e),
						   #\(s),
						   #\(' '),
						   #\(i),
						   #\(n),
						   #\(' '),
						   #\(a),
						   #\(c),
						   #\(t),
						   #\(i),
						   #\(o),
						   #\(n)
						 ])
				      ],
				      Roman_nl_Ret125),
		    f_u_change_tlg_status(Top_level_goal_Param,
					  u_halted,
					  Halted),
		    ElseResult100=[]
		;   get_var(LEnv, u_actors, Actors_Get),
		    f_u_me_in_c63(Actors_Get, IFTEST70),
		    (   IFTEST70\==[]
		    ->  get_var(LEnv, u_actors, Actors_Get73),
			f_u_non_mes(Actors_Get73, TrueResult74),
			IFTEST68=TrueResult74
		    ;   IFTEST68=[]
		    ),
		    (   IFTEST68\==[]
		    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
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
						       #\(' '),
						       #\(e),
						       #\(x),
						       #\(t),
						       #\(e),
						       #\(r),
						       #\(n),
						       #\(a),
						       #\(l),
						       #\(' '),
						       #\(s),
						       #\(h),
						       #\(a),
						       #\(r),
						       #\(e),
						       #\(d),
						       #\(' '),
						       #\(a),
						       #\(c),
						       #\(t),
						       #\(i),
						       #\(o),
						       #\(n)
						     ])
					  ],
					  Roman_nl_Ret126),
			get_var(LEnv, u_ob, Ob_Get75),
			get_var(LEnv, u_rule, Rule_Get),
			f_u_perform_action(Ob_Get75,
					   Top_level_goal_Param,
					   Context_Param,
					   Belief_path_Param,
					   Rule_Get,
					   t,
					   TrueResult101),
			ElseResult100=TrueResult101
		    ;   get_var(LEnv, u_actors, Actors_Get82),
			f_u_non_mes(Actors_Get82, IFTEST80),
			(   IFTEST80\==[]
			->  get_var(LEnv, u_ob, Ob_Get83),
			    get_var(LEnv, u_rule, Rule_Get87),
			    f_u_perform_other_action(Ob_Get83,
						     Top_level_goal_Param,
						     Context_Param,
						     Belief_path_Param,
						     Rule_Get87,
						     TrueResult99),
			    ElseResult100=TrueResult99
			;   get_var(LEnv, u_else, IFTEST88),
			    (   IFTEST88\==[]
			    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
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
							       #\(' '),
							       #\(e),
							       #\(x),
							       #\(t),
							       #\(e),
							       #\(r),
							       #\(n),
							       #\(a),
							       #\(l),
							       #\(' '),
							       #\(a),
							       #\(c),
							       #\(t),
							       #\(i),
							       #\(o),
							       #\(n)
							     ])
						  ],
						  Roman_nl_Ret127),
				get_var(LEnv, u_ob, Ob_Get91),
				get_var(LEnv, u_rule, Rule_Get95),
				f_u_perform_action(Ob_Get91,
						   Top_level_goal_Param,
						   Context_Param,
						   Belief_path_Param,
						   Rule_Get95,
						   [],
						   Perform_action_Ret),
				f_u_me_belief_path_c63(u_belief_path, IFTEST96),
				(   IFTEST96\==[]
				->  set_var(LEnv,
					    setq,
					    u_xx_enter_concepts_c63_xx,
					    t),
				    _117828=t
				;   _117828=[]
				),
				ElseResult100=t
			    ;   ElseResult100=[]
			    )
			)
		    )
		)
	    ;   f_u_eq_c63([quote, real],
			   
			   [ u_ob_c36_get,
			     u_top_level_goal,
			     [quote, u_planning_type]
			   ],
			   IFTEST104),
		(   IFTEST104\==[]
		->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				      u_rule,
				      
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
						   #\(p),
						   #\(e),
						   #\(r),
						   #\(f),
						   #\(o),
						   #\(r),
						   #\(m),
						   #\(' '),
						   #\(r),
						   #\(e),
						   #\(a),
						   #\(l),
						   #\(' '),
						   #\(a),
						   #\(c),
						   #\(t),
						   #\(i),
						   #\(o),
						   #\(n),
						   #\(' '),
						   #\(b),
						   #\(u),
						   #\(t),
						   #\(' '),
						   #\(n),
						   #\(o),
						   #\(t),
						   #\(' '),
						   #\(i),
						   #\(n),
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
				      Roman_nl_Ret129),
		    f_u_change_tlg_status(Top_level_goal_Param,
					  u_waiting,
					  Waiting),
		    ElseResult100=[]
		;   get_var(LEnv, u_ob, Ob_Get107),
		    get_var(LEnv, u_rule, Rule_Get111),
		    f_u_perform_action(Ob_Get107,
				       Top_level_goal_Param,
				       Context_Param,
				       Belief_path_Param,
				       Rule_Get111,
				       [],
				       ElseResult112),
		    ElseResult100=ElseResult112
		)
	    )
	;   ElseResult100=[]
	).
:- set_opv(f_u_perform_action_goal_success, classof, claz_function),
   set_opv(u_perform_action_goal_success, compile_as, kw_function),
   set_opv(u_perform_action_goal_success,
	   function,
	   f_u_perform_action_goal_success),
   DefunResult=u_perform_action_goal_success.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:18941 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Should also probably run inferences off of entered concepts",
				     1,
				     21075)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:21142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-other-action',
			    [ob, 'top-level-goal', context, 'belief-path', rule],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Perform other action ~A in ~A"),
			      ob,
			      context
			    ],
			    
			    [ let,
			      
			      [ ['found-bd', []],
				['found-concept', []],
				
				[ concepts,
				  ['enter-concepts', context, 'belief-path']
				]
			      ],
			      
			      [ yloop,
				[initial, [action, ['ob$get', ob, [quote, obj]]]],
				[yfor, concept, in, concepts],
				[yuntil, 'found-bd'],
				
				[ ydo,
				  
				  [ setq,
				    'found-bd',
				    ['ob$unify', action, concept, '*empty-bd*']
				  ],
				  
				  [ if,
				    'found-bd',
				    [setq, 'found-concept', concept]
				  ]
				]
			      ],
			      
			      [ if,
				'found-bd',
				
				[ progn,
				  
				  [ 'ob$set',
				    'found-concept',
				    [quote, 'inference-rule'],
				    rule
				  ],
				  
				  [ 'make-goal-success',
				    ob,
				    context,
				    'found-concept',
				    'belief-path',
				    'found-bd'
				  ]
				],
				
				[ 'make-goal-failure',
				  ob,
				  context,
				  [tlast, concepts],
				  'belief-path',
				  'top-level-goal',
				  []
				]
			      ],
			      [setq, '*reality*', '*reality-lookahead*'],
			      t
			    ]
			  ]).

% annotating U::PERFORM-OTHER-ACTION 
wl: lambda_def(defun,
	      u_perform_other_action,
	      f_u_perform_other_action,
	      [u_ob, u_top_level_goal, u_context, u_belief_path, u_rule],
	      
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
			     #\(' '),
			     #\(o),
			     #\(t),
			     #\(h),
			     #\(e),
			     #\(r),
			     #\(' '),
			     #\(a),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
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
		  u_context
		],
		
		[ let,
		  
		  [ [u_found_bd, []],
		    [u_found_concept, []],
		    [u_concepts, [u_enter_concepts, u_context, u_belief_path]]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_action, [u_ob_c36_get, u_ob, [quote, u_obj]]]],
		    [u_yfor, u_concept, u_in, u_concepts],
		    [u_yuntil, u_found_bd],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_found_bd,
			[u_ob_c36_unify, u_action, u_concept, u_xx_empty_bd_xx]
		      ],
		      [if, u_found_bd, [setq, u_found_concept, u_concept]]
		    ]
		  ],
		  
		  [ if,
		    u_found_bd,
		    
		    [ progn,
		      
		      [ u_ob_c36_set,
			u_found_concept,
			[quote, u_inference_rule],
			u_rule
		      ],
		      
		      [ u_make_goal_success,
			u_ob,
			u_context,
			u_found_concept,
			u_belief_path,
			u_found_bd
		      ]
		    ],
		    
		    [ u_make_goal_failure,
		      u_ob,
		      u_context,
		      [u_tlast, u_concepts],
		      u_belief_path,
		      u_top_level_goal,
		      []
		    ]
		  ],
		  [setq, u_xx_reality_xx, u_xx_reality_lookahead_xx],
		  t
		]
	      ]).


% annotating U::PERFORM-OTHER-ACTION 
wl: arglist_info(u_perform_other_action,
		[u_ob, u_top_level_goal, u_context, u_belief_path, u_rule],
		
		[ Ob_Param,
		  Top_level_goal_Param,
		  Context_Param,
		  Belief_path_Param,
		  Rule_Param
		],
		arginfo{ all:
			     [ u_ob,
			       u_top_level_goal,
			       u_context,
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
			       [ u_ob,
				 u_top_level_goal,
				 u_context,
				 u_belief_path,
				 u_rule
			       ],
			 opt:0,
			 req:
			     [ u_ob,
			       u_top_level_goal,
			       u_context,
			       u_belief_path,
			       u_rule
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERFORM-OTHER-ACTION 
wl: init_args(exact_only, u_perform_other_action).


% annotating U::PERFORM-OTHER-ACTION 
f_u_perform_other_action(Ob_Param, Top_level_goal_Param, Context_Param, Belief_path_Param, Rule_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param), bv(u_rule, Rule_Param)],
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
				       #\(' '),
				       #\(o),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
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
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_enter_concepts(Context_Param, Belief_path_Param, Concepts_Init),
	LEnv=[[bv(u_found_bd, []), bv(u_found_concept, []), bv(u_concepts, Concepts_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_action, [u_ob_c36_get, u_ob, [quote, u_obj]]]],
		    [u_yfor, u_concept, u_in, u_concepts],
		    [u_yuntil, u_found_bd],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_found_bd,
			[u_ob_c36_unify, u_action, u_concept, u_xx_empty_bd_xx]
		      ],
		      [if, u_found_bd, [setq, u_found_concept, u_concept]]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_found_bd, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found_concept, Found_concept_Get),
	    f_u_ob_c36_set(Found_concept_Get,
			   u_inference_rule,
			   Rule_Param,
			   C36_set_Ret),
	    get_var(LEnv, u_found_bd, Found_bd_Get35),
	    get_var(LEnv, u_found_concept, Found_concept_Get33),
	    f_u_make_goal_success(Ob_Param,
				  Context_Param,
				  Found_concept_Get33,
				  Belief_path_Param,
				  Found_bd_Get35,
				  TrueResult),
	    _111074=TrueResult
	;   f_u_tlast(u_concepts, Tlast_Ret),
	    f_u_make_goal_failure(Ob_Param,
				  Context_Param,
				  Tlast_Ret,
				  Belief_path_Param,
				  Top_level_goal_Param,
				  [],
				  ElseResult),
	    _111074=ElseResult
	),
	get_var(LEnv, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	set_var(LEnv, u_xx_reality_xx, Xx_reality_lookahead_xx_Get),
	LetResult=t,
	LetResult=FnResult.
:- set_opv(f_u_perform_other_action, classof, claz_function),
   set_opv(u_perform_other_action, compile_as, kw_function),
   set_opv(u_perform_other_action, function, f_u_perform_other_action),
   DefunResult=u_perform_other_action.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:21142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was concept action (other order).",
				     4,
				     21625)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:21978 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-action',
			    
			    [ ob,
			      'top-level-goal',
			      context,
			      'belief-path',
			      rule,
			      'shared?'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Perform action goal ~A in ~A"),
			      ob,
			      context
			    ],
			    
			    [ if,
			      
			      [ or,
				[not, 'shared?'],
				
				[ 'shared-action-approved?',
				  ['ob$get', ob, [quote, obj]]
				]
			      ],
			      
			      [ progn,
				
				[ setq,
				  ob,
				  
				  [ 'make-goal-success',
				    ob,
				    context,
				    [],
				    'belief-path',
				    '*empty-bd*'
				  ]
				],
				
				[ 'ob$set',
				  ['ob$get', ob, [quote, obj]],
				  [quote, 'inference-rule'],
				  rule
				],
				
				[ 'cx$hyper-assert-relative',
				  context,
				  ['ob$get', ob, [quote, obj]],
				  'belief-path'
				]
			      ],
			      
			      [ 'make-goal-failure',
				ob,
				context,
				[],
				'belief-path',
				'top-level-goal',
				[]
			      ]
			    ],
			    
			    [ if,
			      
			      [ 'eq?',
				[quote, real],
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'planning-type']
				]
			      ],
			      [setq, '*reality*', '*reality-lookahead*']
			    ],
			    t
			  ]).

% annotating U::PERFORM-ACTION 
wl: lambda_def(defun,
	      u_perform_action,
	      f_u_perform_action,
	      
	      [ u_ob,
		u_top_level_goal,
		u_context,
		u_belief_path,
		u_rule,
		u_shared_c63
	      ],
	      
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
			     #\('A'),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_ob,
		  u_context
		],
		
		[ if,
		  
		  [ or,
		    [not, u_shared_c63],
		    
		    [ u_shared_action_approved_c63,
		      [u_ob_c36_get, u_ob, [quote, u_obj]]
		    ]
		  ],
		  
		  [ progn,
		    
		    [ setq,
		      u_ob,
		      
		      [ u_make_goal_success,
			u_ob,
			u_context,
			[],
			u_belief_path,
			u_xx_empty_bd_xx
		      ]
		    ],
		    
		    [ u_ob_c36_set,
		      [u_ob_c36_get, u_ob, [quote, u_obj]],
		      [quote, u_inference_rule],
		      u_rule
		    ],
		    
		    [ u_cx_c36_hyper_assert_relative,
		      u_context,
		      [u_ob_c36_get, u_ob, [quote, u_obj]],
		      u_belief_path
		    ]
		  ],
		  
		  [ u_make_goal_failure,
		    u_ob,
		    u_context,
		    [],
		    u_belief_path,
		    u_top_level_goal,
		    []
		  ]
		],
		
		[ if,
		  
		  [ u_eq_c63,
		    [quote, real],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		  ],
		  [setq, u_xx_reality_xx, u_xx_reality_lookahead_xx]
		],
		t
	      ]).


% annotating U::PERFORM-ACTION 
wl: arglist_info(u_perform_action,
		
		[ u_ob,
		  u_top_level_goal,
		  u_context,
		  u_belief_path,
		  u_rule,
		  u_shared_c63
		],
		
		[ Ob_Param,
		  Top_level_goal_Param,
		  Context_Param,
		  Belief_path_Param,
		  Rule_Param,
		  Shared_c63_Param
		],
		arginfo{ all:
			     [ u_ob,
			       u_top_level_goal,
			       u_context,
			       u_belief_path,
			       u_rule,
			       u_shared_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob,
				 u_top_level_goal,
				 u_context,
				 u_belief_path,
				 u_rule,
				 u_shared_c63
			       ],
			 opt:0,
			 req:
			     [ u_ob,
			       u_top_level_goal,
			       u_context,
			       u_belief_path,
			       u_rule,
			       u_shared_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERFORM-ACTION 
wl: init_args(exact_only, u_perform_action).


% annotating U::PERFORM-ACTION 
f_u_perform_action(Ob_Param, Top_level_goal_Param, Context_Param, Belief_path_Param, Rule_Param, Shared_c63_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param), bv(u_rule, Rule_Param), bv(u_shared_c63, Shared_c63_Param)],
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
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ob,
			    u_context
			  ],
			  Roman_nl_Ret),
	(   cl_not(Shared_c63_Param, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   get_var(Env, u_ob, Ob_Get),
	    f_u_ob_c36_get(Ob_Get, u_obj, Obj),
	    f_u_shared_action_approved_c63(Obj, Approved_c63_Ret),
	    IFTEST=Approved_c63_Ret
	),
	(   IFTEST\==[]
	->  get_var(Env, u_ob, Ob_Get27),
	    get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get),
	    f_u_make_goal_success(Ob_Get27,
				  Context_Param,
				  [],
				  Belief_path_Param,
				  Xx_empty_bd_xx_Get,
				  Ob),
	    set_var(Env, u_ob, Ob),
	    get_var(Env, u_ob, Ob_Get31),
	    f_u_ob_c36_get(Ob_Get31, u_obj, Obj50),
	    f_u_ob_c36_set(Obj50, u_inference_rule, Rule_Param, C36_set_Ret),
	    get_var(Env, u_ob, Ob_Get34),
	    f_u_ob_c36_get(Ob_Get34, u_obj, Obj51),
	    f_u_cx_c36_hyper_assert_relative(Context_Param,
					     Obj51,
					     Belief_path_Param,
					     TrueResult),
	    _110990=TrueResult
	;   get_var(Env, u_ob, Ob_Get36),
	    f_u_make_goal_failure(Ob_Get36,
				  Context_Param,
				  [],
				  Belief_path_Param,
				  Top_level_goal_Param,
				  [],
				  ElseResult),
	    _110990=ElseResult
	),
	f_u_eq_c63([quote, real],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST42),
	(   IFTEST42\==[]
	->  get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	    set_var(Env, u_xx_reality_xx, Xx_reality_lookahead_xx_Get),
	    _111590=Xx_reality_lookahead_xx_Get
	;   _111590=[]
	),
	t=FnResult.
:- set_opv(f_u_perform_action, classof, claz_function),
   set_opv(u_perform_action, compile_as, kw_function),
   set_opv(u_perform_action, function, f_u_perform_action),
   DefunResult=u_perform_action.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:21978 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Can only perform below if ob obj is full instantiated?",
				     3,
				     22132)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:21978 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Maybe later I will want to input that the other party",
				     1,
				     22661)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:21978 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" does not go along with a shared action.",
				     1,
				     22723)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'shared-action-approved?', [action], t]).

% annotating U::SHARED-ACTION-APPROVED? 
wl: lambda_def(defun,
	      u_shared_action_approved_c63,
	      f_u_shared_action_approved_c63,
	      [u_action],
	      [t]).


% annotating U::SHARED-ACTION-APPROVED? 
wl: arglist_info(u_shared_action_approved_c63,
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

% annotating U::SHARED-ACTION-APPROVED? 
wl: init_args(exact_only, u_shared_action_approved_c63).


% annotating U::SHARED-ACTION-APPROVED? 
f_u_shared_action_approved_c63(Action_Param, FnResult) :-
	Env=[bv(u_action, Action_Param)],
	t=FnResult.
:- set_opv(f_u_shared_action_approved_c63, classof, claz_function),
   set_opv(u_shared_action_approved_c63, compile_as, kw_function),
   set_opv(u_shared_action_approved_c63,
	   function,
	   f_u_shared_action_approved_c63),
   DefunResult=u_shared_action_approved_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-non-action-goal-successes',
			    ['top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Perform non-action goal successes in ~A"),
			      context
			    ],
			    
			    [ yloop,
			      [initial, ['fired?', []]],
			      
			      [ yfor,
				ob,
				in,
				
				[ if,
				  ['me-belief-path?', 'belief-path'],
				  ['cx$get-all-ty', context, '*active-goal-ob*'],
				  ['cx$get-all-ty', context, '*believe-ob*']
				]
			      ],
			      
			      [ ydo,
				
				[ if,
				  ['not-me-belief-path?', 'belief-path'],
				  
				  [ progn,
				    
				    [ setq,
				      ob,
				      ['absolute->relative', ob, 'belief-path']
				    ],
				    
				    [ if,
				      
				      [ and,
					
					[ 'ty$instance?',
					  ob,
					  [quote, 'active-goal']
					],
					
					[ not,
					  
					  [ 'ty$instance?',
					    ob,
					    [quote, 'active-p-goal']
					  ]
					],
					
					[ not,
					  
					  [ 'ty$instance?',
					    ['ob$get', ob, [quote, obj]],
					    [quote, action]
					  ]
					],
					
					[ not,
					  
					  [ 'memq?',
					    context,
					    
					    [ 'ob$gets',
					      ob,
					      [quote, 'failed-context']
					    ]
					  ]
					],
					
					[ or,
					  
					  [ 'eq?',
					    'top-level-goal',
					    
					    [ 'ob$get',
					      ob,
					      [quote, 'top-level-goal']
					    ]
					  ],
					  [not, ['real?', 'top-level-goal']]
					]
				      ],
				      
				      [ setq,
					'fired?',
					
					[ 'or-inf',
					  
					  [ 'perform-non-action-goal-success',
					    ob,
					    'top-level-goal',
					    context,
					    'belief-path'
					  ],
					  'fired?'
					]
				      ]
				    ]
				  ],
				  
				  [ if,
				    
				    [ and,
				      
				      [ not,
					
					[ 'ty$instance?',
					  ['ob$get', ob, [quote, obj]],
					  [quote, action]
					]
				      ],
				      
				      [ not,
					
					[ 'ty$instance?',
					  ob,
					  [quote, 'active-p-goal']
					]
				      ],
				      
				      [ not,
					
					[ 'memq?',
					  context,
					  
					  [ 'ob$gets',
					    ob,
					    [quote, 'failed-context']
					  ]
					]
				      ],
				      
				      [ or,
					
					[ 'eq?',
					  'top-level-goal',
					  
					  [ 'ob$get',
					    ob,
					    [quote, 'top-level-goal']
					  ]
					],
					[not, ['real?', 'top-level-goal']]
				      ]
				    ],
				    
				    [ setq,
				      'fired?',
				      
				      [ 'or-inf',
					
					[ 'perform-non-action-goal-success',
					  ob,
					  'top-level-goal',
					  context,
					  'belief-path'
					],
					'fired?'
				      ]
				    ]
				  ]
				]
			      ],
			      [yresult, 'fired?']
			    ]
			  ]).

% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESSES 
wl: lambda_def(defun,
	      u_perform_non_action_goal_successes,
	      f_u_perform_non_action_goal_successes,
	      [u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(e),
			     #\(r),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(m),
			     #\(' '),
			     #\(n),
			     #\(o),
			     #\(n),
			     #\(-),
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
			     #\(s),
			     #\(u),
			     #\(c),
			     #\(c),
			     #\(e),
			     #\(s),
			     #\(s),
			     #\(e),
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
		  [u_initial, [u_fired_c63, []]],
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    
		    [ if,
		      [u_me_belief_path_c63, u_belief_path],
		      [u_cx_c36_get_all_ty, u_context, u_xx_active_goal_ob_xx],
		      [u_cx_c36_get_all_ty, u_context, u_xx_believe_ob_xx]
		    ]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_not_me_belief_path_c63, u_belief_path],
		      
		      [ progn,
			
			[ setq,
			  u_ob,
			  [u_absolute_c62_relative, u_ob, u_belief_path]
			],
			
			[ if,
			  
			  [ and,
			    [u_ty_c36_instance_c63, u_ob, [quote, u_active_goal]],
			    
			    [ not,
			      
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_active_p_goal]
			      ]
			    ],
			    
			    [ not,
			      
			      [ u_ty_c36_instance_c63,
				[u_ob_c36_get, u_ob, [quote, u_obj]],
				[quote, u_action]
			      ]
			    ],
			    
			    [ not,
			      
			      [ u_memq_c63,
				u_context,
				[u_ob_c36_gets, u_ob, [quote, u_failed_context]]
			      ]
			    ],
			    
			    [ or,
			      
			      [ u_eq_c63,
				u_top_level_goal,
				[u_ob_c36_get, u_ob, [quote, u_top_level_goal]]
			      ],
			      [not, [u_real_c63, u_top_level_goal]]
			    ]
			  ],
			  
			  [ setq,
			    u_fired_c63,
			    
			    [ u_or_inf,
			      
			      [ u_perform_non_action_goal_success,
				u_ob,
				u_top_level_goal,
				u_context,
				u_belief_path
			      ],
			      u_fired_c63
			    ]
			  ]
			]
		      ],
		      
		      [ if,
			
			[ and,
			  
			  [ not,
			    
			    [ u_ty_c36_instance_c63,
			      [u_ob_c36_get, u_ob, [quote, u_obj]],
			      [quote, u_action]
			    ]
			  ],
			  
			  [ not,
			    
			    [ u_ty_c36_instance_c63,
			      u_ob,
			      [quote, u_active_p_goal]
			    ]
			  ],
			  
			  [ not,
			    
			    [ u_memq_c63,
			      u_context,
			      [u_ob_c36_gets, u_ob, [quote, u_failed_context]]
			    ]
			  ],
			  
			  [ or,
			    
			    [ u_eq_c63,
			      u_top_level_goal,
			      [u_ob_c36_get, u_ob, [quote, u_top_level_goal]]
			    ],
			    [not, [u_real_c63, u_top_level_goal]]
			  ]
			],
			
			[ setq,
			  u_fired_c63,
			  
			  [ u_or_inf,
			    
			    [ u_perform_non_action_goal_success,
			      u_ob,
			      u_top_level_goal,
			      u_context,
			      u_belief_path
			    ],
			    u_fired_c63
			  ]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_fired_c63]
		]
	      ]).


% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESSES 
wl: arglist_info(u_perform_non_action_goal_successes,
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

% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESSES 
wl: init_args(exact_only, u_perform_non_action_goal_successes).


% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESSES 
f_u_perform_non_action_goal_successes(Top_level_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(e),
				       #\(r),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(n),
				       #\(-),
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
				       #\(s),
				       #\(u),
				       #\(c),
				       #\(c),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(e),
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
		  [ [u_initial, [u_fired_c63, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			[u_me_belief_path_c63, u_belief_path],
			[u_cx_c36_get_all_ty, u_context, u_xx_active_goal_ob_xx],
			[u_cx_c36_get_all_ty, u_context, u_xx_believe_ob_xx]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_not_me_belief_path_c63, u_belief_path],
			
			[ progn,
			  
			  [ setq,
			    u_ob,
			    [u_absolute_c62_relative, u_ob, u_belief_path]
			  ],
			  
			  [ if,
			    
			    [ and,
			      
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_active_goal]
			      ],
			      
			      [ not,
				
				[ u_ty_c36_instance_c63,
				  u_ob,
				  [quote, u_active_p_goal]
				]
			      ],
			      
			      [ not,
				
				[ u_ty_c36_instance_c63,
				  [u_ob_c36_get, u_ob, [quote, u_obj]],
				  [quote, u_action]
				]
			      ],
			      
			      [ not,
				
				[ u_memq_c63,
				  u_context,
				  
				  [ u_ob_c36_gets,
				    u_ob,
				    [quote, u_failed_context]
				  ]
				]
			      ],
			      
			      [ or,
				
				[ u_eq_c63,
				  u_top_level_goal,
				  [u_ob_c36_get, u_ob, [quote, u_top_level_goal]]
				],
				[not, [u_real_c63, u_top_level_goal]]
			      ]
			    ],
			    
			    [ setq,
			      u_fired_c63,
			      
			      [ u_or_inf,
				
				[ u_perform_non_action_goal_success,
				  u_ob,
				  u_top_level_goal,
				  u_context,
				  u_belief_path
				],
				u_fired_c63
			      ]
			    ]
			  ]
			],
			
			[ if,
			  
			  [ and,
			    
			    [ not,
			      
			      [ u_ty_c36_instance_c63,
				[u_ob_c36_get, u_ob, [quote, u_obj]],
				[quote, u_action]
			      ]
			    ],
			    
			    [ not,
			      
			      [ u_ty_c36_instance_c63,
				u_ob,
				[quote, u_active_p_goal]
			      ]
			    ],
			    
			    [ not,
			      
			      [ u_memq_c63,
				u_context,
				[u_ob_c36_gets, u_ob, [quote, u_failed_context]]
			      ]
			    ],
			    
			    [ or,
			      
			      [ u_eq_c63,
				u_top_level_goal,
				[u_ob_c36_get, u_ob, [quote, u_top_level_goal]]
			      ],
			      [not, [u_real_c63, u_top_level_goal]]
			    ]
			  ],
			  
			  [ setq,
			    u_fired_c63,
			    
			    [ u_or_inf,
			      
			      [ u_perform_non_action_goal_success,
				u_ob,
				u_top_level_goal,
				u_context,
				u_belief_path
			      ],
			      u_fired_c63
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_fired_c63]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_perform_non_action_goal_successes, classof, claz_function),
   set_opv(u_perform_non_action_goal_successes, compile_as, kw_function),
   set_opv(u_perform_non_action_goal_successes,
	   function,
	   f_u_perform_non_action_goal_successes),
   DefunResult=u_perform_non_action_goal_successes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Below not needed anymore. Seq logic fixed.",
				     22,
				     23531)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                     (seq-head? ob context belief-path)",
				     1,
				     23576)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22808 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end and", 24, 23857)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                    (seq-head? ob context belief-path)",
				     1,
				     24251)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22808 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("end and", 23, 24529)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:22808 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Should return sprouted contexts",
				     1,
				     24817)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'perform-non-action-goal-success',
			    [ob, 'top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Perform non-action goal success for ~A in ~A"),
			      ob,
			      context
			    ],
			    
			    [ 'let*',
			      
			      [ 
				[ 'intends-links',
				  
				  [ 'goal-intends-links-uo',
				    ob,
				    context,
				    'belief-path'
				  ]
				],
				[rule, []],
				['no-auto?', []]
			      ],
			      
			      [ if,
				'intends-links',
				
				[ progn,
				  
				  [ setq,
				    rule,
				    
				    [ 'ob$get',
				      [car, 'intends-links'],
				      [quote, rule]
				    ]
				  ],
				  
				  [ setq,
				    'no-auto?',
				    ['plan-only-no-auto?', rule]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ 
				  [ and,
				    'intends-links',
				    ['null?', 'no-auto?'],
				    
				    [ 'any-subgoals-failed?',
				      ob,
				      context,
				      'belief-path'
				    ]
				  ],
				  
				  [ 'make-goal-failure',
				    ob,
				    context,
				    [],
				    'belief-path',
				    'top-level-goal',
				    []
				  ],
				  t
				],
				
				[ else,
				  
				  [ let,
				    
				    [ [bds, []],
				      ['fired?', []],
				      ['sprouted-context', []]
				    ],
				    
				    [ cond,
				      
				      [ 
					[ or,
					  
					  [ if,
					    
					    [ 'ty$instance?',
					      ['ob$get', ob, [quote, obj]],
					      [quote, rtrue]
					    ],
					    [setq, bds, [list, '*empty-bd*']],
					    []
					  ],
					  
					  [ setq,
					    bds,
					    
					    [ 'cx$retrieve-relative',
					      context,
					      ['ob$get', ob, [quote, obj]],
					      'belief-path'
					    ]
					  ]
					],
					
					[ map,
					  [quote, list],
					  
					  [ lambda,
					    [bd],
					    
					    [ if,
					      [not, ['empty-bd?', bd]],
					      
					      [ progn,
						
						[ 'ndbg-roman-nl',
						  '*gate-dbg*',
						  'rule-long',
						  '$STRING'("Non-empty bd ~A, so doing nothing"),
						  bd
						]
					      ],
					      
					      [ progn,
						
						[ setq,
						  'sprouted-context',
						  context
						],
						[setq, 'fired?', t],
						
						[ if,
						  'intends-links',
						  
						  [ setq,
						    ob,
						    
						    [ 'make-goal-success',
						      ob,
						      'sprouted-context',
						      
						      [ if,
							
							[ 'neq?',
							  [quote, t],
							  [car, bd]
							],
							[car, bd],
							[]
						      ],
						      'belief-path',
						      bd
						    ]
						  ],
						  
						  [ 'no-gen',
						    
						    [ setq,
						      ob,
						      
						      [ 'make-goal-success',
							ob,
							'sprouted-context',
							
							[ if,
							  
							  [ 'neq?',
							    [quote, t],
							    [car, bd]
							  ],
							  [car, bd],
							  []
							],
							'belief-path',
							bd
						      ]
						    ]
						  ]
						]
					      ]
					    ]
					  ],
					  bds
					],
					
					[ if,
					  
					  [ and,
					    'fired?',
					    'intends-links',
					    
					    [ not,
					      
					      [ 'subgoals-completed?',
						ob,
						context,
						'belief-path'
					      ]
					    ]
					  ],
					  
					  [ progn,
					    
					    [ 'ndbg-roman',
					      '*gate-dbg*',
					      rule,
					      '$STRING'("Fortuitous goal success")
					    ],
					    
					    [ 'ndbg-roman',
					      '*gate-dbg*',
					      rule,
					      '$STRING'(" ~A in ~A"),
					      ob,
					      context
					    ],
					    ['ndbg-newline', '*gate-dbg*', rule],
					    
					    [ 'clear-subgoals',
					      ob,
					      context,
					      'belief-path'
					    ],
					    
					    [ if,
					      
					      [ 'neq?',
						'top-level-goal',
						
						[ 'ob$get',
						  ob,
						  [quote, 'top-level-goal']
						]
					      ],
					      
					      [ surprise,
						
						[ 'ob$get',
						  ob,
						  [quote, 'top-level-goal']
						]
					      ]
					    ]
					  ]
					],
					'fired?'
				      ],
				      
				      [ 
					[ and,
					  ['null?', 'no-auto?'],
					  
					  [ 'subgoals-completed?',
					    ob,
					    context,
					    'belief-path'
					  ]
					],
					
					[ 'set-strength',
					  ['ob$get', ob, [quote, obj]],
					  
					  [ 'calculate-supergoal-obj-strength',
					    
					    [ map,
					      [quote, list],
					      
					      [ lambda,
						[x],
						
						[ 'ob$get',
						  x,
						  [quote, 'linked-to']
						]
					      ],
					      'intends-links'
					    ],
					    rule
					  ]
					],
					
					[ setq,
					  ob,
					  
					  [ 'make-goal-success',
					    ob,
					    context,
					    [],
					    'belief-path',
					    '*empty-bd*'
					  ]
					],
					
					[ 'ob$set',
					  ['ob$get', ob, [quote, obj]],
					  [quote, 'inference-rule'],
					  rule
					],
					
					[ 'cx$assert-relative',
					  context,
					  ['ob$get', ob, [quote, obj]],
					  'belief-path'
					],
					
					[ 'fake-inference-deletes',
					  
					  [ map,
					    [quote, list],
					    
					    [ lambda,
					      [x],
					      ['ob$get', x, [quote, obj]]
					    ],
					    
					    [ 'goal-subgoals',
					      ob,
					      context,
					      'belief-path'
					    ]
					  ],
					  rule,
					  context,
					  'belief-path'
					],
					[quote, inferences]
				      ],
				      [else, []]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESS 
wl: lambda_def(defun,
	      u_perform_non_action_goal_success,
	      f_u_perform_non_action_goal_success,
	      [u_ob, u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('P'),
			     #\(e),
			     #\(r),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(m),
			     #\(' '),
			     #\(n),
			     #\(o),
			     #\(n),
			     #\(-),
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
			     #\(s),
			     #\(u),
			     #\(c),
			     #\(c),
			     #\(e),
			     #\(s),
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
		  u_ob,
		  u_context
		],
		
		[ let_xx,
		  
		  [ 
		    [ u_intends_links,
		      [u_goal_intends_links_uo, u_ob, u_context, u_belief_path]
		    ],
		    [u_rule, []],
		    [u_no_auto_c63, []]
		  ],
		  
		  [ if,
		    u_intends_links,
		    
		    [ progn,
		      
		      [ setq,
			u_rule,
			[u_ob_c36_get, [car, u_intends_links], [quote, u_rule]]
		      ],
		      [setq, u_no_auto_c63, [u_plan_only_no_auto_c63, u_rule]]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ 
		      [ and,
			u_intends_links,
			[u_null_c63, u_no_auto_c63],
			
			[ u_any_subgoals_failed_c63,
			  u_ob,
			  u_context,
			  u_belief_path
			]
		      ],
		      
		      [ u_make_goal_failure,
			u_ob,
			u_context,
			[],
			u_belief_path,
			u_top_level_goal,
			[]
		      ],
		      t
		    ],
		    
		    [ u_else,
		      
		      [ let,
			[[u_bds, []], [u_fired_c63, []], [u_sprouted_context, []]],
			
			[ cond,
			  
			  [ 
			    [ or,
			      
			      [ if,
				
				[ u_ty_c36_instance_c63,
				  [u_ob_c36_get, u_ob, [quote, u_obj]],
				  [quote, u_rtrue]
				],
				[setq, u_bds, [list, u_xx_empty_bd_xx]],
				[]
			      ],
			      
			      [ setq,
				u_bds,
				
				[ u_cx_c36_retrieve_relative,
				  u_context,
				  [u_ob_c36_get, u_ob, [quote, u_obj]],
				  u_belief_path
				]
			      ]
			    ],
			    
			    [ map,
			      [quote, list],
			      
			      [ lambda,
				[u_bd],
				
				[ if,
				  [not, [u_empty_bd_c63, u_bd]],
				  
				  [ progn,
				    
				    [ u_ndbg_roman_nl,
				      u_xx_gate_dbg_xx,
				      u_rule_long,
				      '$ARRAY'([*],
					       claz_base_character,
					       
					       [ #\('N'),
						 #\(o),
						 #\(n),
						 #\(-),
						 #\(e),
						 #\(m),
						 #\(p),
						 #\(t),
						 #\(y),
						 #\(' '),
						 #\(b),
						 #\(d),
						 #\(' '),
						 #\(~),
						 #\('A'),
						 #\(','),
						 #\(' '),
						 #\(s),
						 #\(o),
						 #\(' '),
						 #\(d),
						 #\(o),
						 #\(i),
						 #\(n),
						 #\(g),
						 #\(' '),
						 #\(n),
						 #\(o),
						 #\(t),
						 #\(h),
						 #\(i),
						 #\(n),
						 #\(g)
					       ]),
				      u_bd
				    ]
				  ],
				  
				  [ progn,
				    [setq, u_sprouted_context, u_context],
				    [setq, u_fired_c63, t],
				    
				    [ if,
				      u_intends_links,
				      
				      [ setq,
					u_ob,
					
					[ u_make_goal_success,
					  u_ob,
					  u_sprouted_context,
					  
					  [ if,
					    [u_neq_c63, [quote, t], [car, u_bd]],
					    [car, u_bd],
					    []
					  ],
					  u_belief_path,
					  u_bd
					]
				      ],
				      
				      [ u_no_gen,
					
					[ setq,
					  u_ob,
					  
					  [ u_make_goal_success,
					    u_ob,
					    u_sprouted_context,
					    
					    [ if,
					      [u_neq_c63, [quote, t], [car, u_bd]],
					      [car, u_bd],
					      []
					    ],
					    u_belief_path,
					    u_bd
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ],
			      u_bds
			    ],
			    
			    [ if,
			      
			      [ and,
				u_fired_c63,
				u_intends_links,
				
				[ not,
				  
				  [ u_subgoals_completed_c63,
				    u_ob,
				    u_context,
				    u_belief_path
				  ]
				]
			      ],
			      
			      [ progn,
				
				[ u_ndbg_roman,
				  u_xx_gate_dbg_xx,
				  u_rule,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('F'),
					     #\(o),
					     #\(r),
					     #\(t),
					     #\(u),
					     #\(i),
					     #\(t),
					     #\(o),
					     #\(u),
					     #\(s),
					     #\(' '),
					     #\(g),
					     #\(o),
					     #\(a),
					     #\(l),
					     #\(' '),
					     #\(s),
					     #\(u),
					     #\(c),
					     #\(c),
					     #\(e),
					     #\(s),
					     #\(s)
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
				  u_ob,
				  u_context
				],
				[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
				
				[ u_clear_subgoals,
				  u_ob,
				  u_context,
				  u_belief_path
				],
				
				[ if,
				  
				  [ u_neq_c63,
				    u_top_level_goal,
				    
				    [ u_ob_c36_get,
				      u_ob,
				      [quote, u_top_level_goal]
				    ]
				  ],
				  
				  [ u_surprise,
				    
				    [ u_ob_c36_get,
				      u_ob,
				      [quote, u_top_level_goal]
				    ]
				  ]
				]
			      ]
			    ],
			    u_fired_c63
			  ],
			  
			  [ 
			    [ and,
			      [u_null_c63, u_no_auto_c63],
			      
			      [ u_subgoals_completed_c63,
				u_ob,
				u_context,
				u_belief_path
			      ]
			    ],
			    
			    [ u_set_strength,
			      [u_ob_c36_get, u_ob, [quote, u_obj]],
			      
			      [ u_calculate_supergoal_obj_strength,
				
				[ map,
				  [quote, list],
				  
				  [ lambda,
				    [u_x],
				    [u_ob_c36_get, u_x, [quote, u_linked_to]]
				  ],
				  u_intends_links
				],
				u_rule
			      ]
			    ],
			    
			    [ setq,
			      u_ob,
			      
			      [ u_make_goal_success,
				u_ob,
				u_context,
				[],
				u_belief_path,
				u_xx_empty_bd_xx
			      ]
			    ],
			    
			    [ u_ob_c36_set,
			      [u_ob_c36_get, u_ob, [quote, u_obj]],
			      [quote, u_inference_rule],
			      u_rule
			    ],
			    
			    [ u_cx_c36_assert_relative,
			      u_context,
			      [u_ob_c36_get, u_ob, [quote, u_obj]],
			      u_belief_path
			    ],
			    
			    [ u_fake_inference_deletes,
			      
			      [ map,
				[quote, list],
				
				[ lambda,
				  [u_x],
				  [u_ob_c36_get, u_x, [quote, u_obj]]
				],
				[u_goal_subgoals, u_ob, u_context, u_belief_path]
			      ],
			      u_rule,
			      u_context,
			      u_belief_path
			    ],
			    [quote, u_inferences]
			  ],
			  [u_else, []]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESS 
wl: arglist_info(u_perform_non_action_goal_success,
		[u_ob, u_top_level_goal, u_context, u_belief_path],
		
		[ Ob_Param,
		  Top_level_goal_Param,
		  Context_Param,
		  Belief_path_Param
		],
		arginfo{ all:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESS 
wl: init_args(exact_only, u_perform_non_action_goal_success).


% annotating U::PERFORM-NON-ACTION-GOAL-SUCCESS 
f_u_perform_non_action_goal_success(Ob_Param, Top_level_goal_Param, Context_Param, Belief_path_Param, Fired_c63_Get115) :-
	Env=[bv(u_ob, Ob_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('P'),
				       #\(e),
				       #\(r),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(m),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(n),
				       #\(-),
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
				       #\(s),
				       #\(u),
				       #\(c),
				       #\(c),
				       #\(e),
				       #\(s),
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
			    u_ob,
			    u_context
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_ob, Ob_Get),
	f_u_goal_intends_links_uo(Ob_Get,
				  Context_Param,
				  Belief_path_Param,
				  Intends_links_Init),
	LEnv=[[bv(u_intends_links, Intends_links_Init)]|Env],
	LEnv24=[[bv(u_rule, [])]|LEnv],
	Env=[[bv(u_no_auto_c63, [])]|LEnv24],
	get_var(Env, u_intends_links, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_intends_links, Intends_links_Get32),
	    cl_car(Intends_links_Get32, C36_get_Param),
	    f_u_ob_c36_get(C36_get_Param, u_rule, Rule),
	    set_var(Env, u_rule, Rule),
	    get_var(Env, u_rule, Rule_Get129),
	    f_u_plan_only_no_auto_c63(Rule_Get129, TrueResult),
	    set_var(Env, u_no_auto_c63, TrueResult),
	    _119210=TrueResult
	;   _119210=[]
	),
	get_var(Env, u_intends_links, IFTEST37),
	(   IFTEST37\==[]
	->  f_u_null_c63(u_no_auto_c63, IFTEST40),
	    (   IFTEST40\==[]
	    ->  get_var(Env, u_ob, Ob_Get42),
		f_u_any_subgoals_failed_c63(Ob_Get42,
					    Context_Param,
					    Belief_path_Param,
					    TrueResult45),
		IFTEST35=TrueResult45
	    ;   IFTEST35=[]
	    )
	;   IFTEST35=[]
	),
	(   IFTEST35\==[]
	->  f_u_make_goal_failure(Ob_Param,
				  Context_Param,
				  [],
				  Belief_path_Param,
				  Top_level_goal_Param,
				  [],
				  Goal_failure_Ret),
	    Fired_c63_Get115=t
	;   get_var(Env, u_else, IFTEST51),
	    (   IFTEST51\==[]
	    ->  Env=[[bv(u_bds, []), bv(u_fired_c63, []), bv(u_sprouted_context, [])]|Env],
		(   get_var(Env, u_ob, Ob_Get60),
		    f_u_ob_c36_get(Ob_Get60, u_obj, Obj),
		    f_u_ty_c36_instance_c63(Obj, u_rtrue, IFTEST58),
		    (   IFTEST58\==[]
		    ->  get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get),
			TrueResult63=[Xx_empty_bd_xx_Get],
			set_var(Env, u_bds, TrueResult63),
			FORM1_Res=TrueResult63
		    ;   FORM1_Res=[]
		    ),
		    FORM1_Res\==[],
		    IFTEST56=FORM1_Res
		->  true
		;   get_var(Env, u_ob, Ob_Get65),
		    f_u_ob_c36_get(Ob_Get65, u_obj, Obj161),
		    f_u_cx_c36_retrieve_relative(Context_Param,
						 Obj161,
						 Belief_path_Param,
						 Bds),
		    set_var(Env, u_bds, Bds),
		    IFTEST56=Bds
		),
		(   IFTEST56\==[]
		->  Lambda=closure([Env|Env], LResult, [u_bd],  (get_var(Env, u_bd, Bd_Get), f_u_empty_bd_c63(Bd_Get, PredArgResult), (PredArgResult==[]->f_u_ndbg_roman_nl(u_xx_gate_dbg_xx, u_rule_long, ['$ARRAY'([*], claz_base_character, [#\('N'), #\(o), #\(n), #\(-), #\(e), #\(m), #\(p), #\(t), #\(y), #\(' '), #\(b), #\(d), #\(' '), #\(~), #\('A'), #\(','), #\(' '), #\(s), #\(o), #\(' '), #\(d), #\(o), #\(i), #\(n), #\(g), #\(' '), #\(n), #\(o), #\(t), #\(h), #\(i), #\(n), #\(g)]), u_bd], TrueResult88), LResult=TrueResult88;set_var(Env, u_sprouted_context, Context_Param), set_var(Env, setq, u_fired_c63, t), get_var(Env, u_intends_links, IFTEST75), (IFTEST75\==[]->get_var(Env, u_ob, Ob_Get78), get_var(Env, u_sprouted_context, Sprouted_context_Get), f_u_neq_c63([quote, t], [car, u_bd], IFTEST80), (IFTEST80\==[]->get_var(Env, u_bd, Bd_Get82), cl_car(Bd_Get82, TrueResult83), _120894=TrueResult83;_120894=[]), get_var(Env, u_bd, Bd_Get85), f_u_make_goal_success(Ob_Get78, Sprouted_context_Get, _120894, Belief_path_Param, Bd_Get85, TrueResult86), set_var(Env, u_ob, TrueResult86), ElseResult89=TrueResult86;f_u_no_gen([[setq, u_ob, [u_make_goal_success, u_ob, u_sprouted_context, [if, [u_neq_c63, [quote, t], [car, u_bd]], [car, u_bd], []], u_belief_path, u_bd]]], ElseResult), ElseResult89=ElseResult), LResult=ElseResult89))),
		    get_var(Env, u_bds, Bds_Get),
		    cl_map(list, Lambda, Bds_Get, Map_Ret),
		    get_var(Env, u_fired_c63, Fired_c63_Get115),
		    (   Fired_c63_Get115\==[]
		    ->  get_var(Env, u_intends_links, IFTEST99),
			(   IFTEST99\==[]
			->  get_var(Env, u_ob, Ob_Get102),
			    f_u_subgoals_completed_c63(Ob_Get102,
						       Context_Param,
						       Belief_path_Param,
						       Not_Param),
			    cl_not(Not_Param, TrueResult105),
			    IFTEST94=TrueResult105
			;   IFTEST94=[]
			)
		    ;   IFTEST94=[]
		    ),
		    (   IFTEST94\==[]
		    ->  f_u_ndbg_roman(u_xx_gate_dbg_xx,
				       u_rule,
				       
				       [ '$ARRAY'([*],
						  claz_base_character,
						  
						  [ #\('F'),
						    #\(o),
						    #\(r),
						    #\(t),
						    #\(u),
						    #\(i),
						    #\(t),
						    #\(o),
						    #\(u),
						    #\(s),
						    #\(' '),
						    #\(g),
						    #\(o),
						    #\(a),
						    #\(l),
						    #\(' '),
						    #\(s),
						    #\(u),
						    #\(c),
						    #\(c),
						    #\(e),
						    #\(s),
						    #\(s)
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
					 u_ob,
					 u_context
				       ],
				       Ndbg_roman_Ret175),
			f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule163),
			get_var(Env, u_ob, Ob_Get107),
			f_u_clear_subgoals(Ob_Get107,
					   Context_Param,
					   Belief_path_Param,
					   Clear_subgoals_Ret),
			f_u_neq_c63(u_top_level_goal,
				    
				    [ u_ob_c36_get,
				      u_ob,
				      [quote, u_top_level_goal]
				    ],
				    IFTEST110),
			(   IFTEST110\==[]
			->  get_var(Env, u_ob, Ob_Get112),
			    f_u_ob_c36_get(Ob_Get112,
					   u_top_level_goal,
					   Top_level_goal),
			    f_u_surprise(Top_level_goal, TrueResult113),
			    TrueResult114=TrueResult113
			;   TrueResult114=[]
			)
		    ;   TrueResult114=[]
		    )
		;   f_u_null_c63(u_no_auto_c63, IFTEST118),
		    (   IFTEST118\==[]
		    ->  get_var(Env, u_ob, Ob_Get120),
			f_u_subgoals_completed_c63(Ob_Get120,
						   Context_Param,
						   Belief_path_Param,
						   TrueResult123),
			IFTEST116=TrueResult123
		    ;   IFTEST116=[]
		    ),
		    (   IFTEST116\==[]
		    ->  f_u_set_strength([u_ob_c36_get, u_ob, [quote, u_obj]],
					 
					 [ u_calculate_supergoal_obj_strength,
					   
					   [ map,
					     [quote, list],
					     
					     [ lambda,
					       [u_x],
					       
					       [ u_ob_c36_get,
						 u_x,
						 [quote, u_linked_to]
					       ]
					     ],
					     u_intends_links
					   ],
					   u_rule
					 ],
					 Set_strength_Ret),
			get_var(Env, u_ob, Ob_Get124),
			get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get127),
			f_u_make_goal_success(Ob_Get124,
					      Context_Param,
					      [],
					      Belief_path_Param,
					      Xx_empty_bd_xx_Get127,
					      Ob),
			set_var(Env, u_ob, Ob),
			get_var(Env, u_ob, Ob_Get128),
			f_u_ob_c36_get(Ob_Get128, u_obj, Obj166),
			f_u_ob_c36_set(Obj166,
				       u_inference_rule,
				       Rule_Get129,
				       C36_set_Ret),
			get_var(Env, u_ob, Ob_Get131),
			f_u_ob_c36_get(Ob_Get131, u_obj, Obj167),
			f_u_cx_c36_assert_relative(Context_Param,
						   Obj167,
						   Belief_path_Param,
						   Assert_relative_Ret),
			Lambda136=closure([Env134|Env], LResult135, [u_x],  (get_var(Env134, u_x, X_Get), f_u_ob_c36_get(X_Get, u_obj, LResult135))),
			get_var(Env, u_ob, Ob_Get138),
			f_u_goal_subgoals(Ob_Get138,
					  Context_Param,
					  Belief_path_Param,
					  Goal_subgoals_Ret),
			cl_map(list,
			       Lambda136,
			       Goal_subgoals_Ret,
			       Inference_deletes_Param),
			f_u_fake_inference_deletes(Inference_deletes_Param,
						   Rule_Get129,
						   Context_Param,
						   Belief_path_Param,
						   Inference_deletes_Ret),
			Fired_c63_Get115=u_inferences
		    ;   get_var(Env, u_else, IFTEST144),
			(   IFTEST144\==[]
			->  Fired_c63_Get115=[]
			;   Fired_c63_Get115=[]
			)
		    )
		)
	    ;   Fired_c63_Get115=[]
	    )
	).
:- set_opv(f_u_perform_non_action_goal_success, classof, claz_function),
   set_opv(u_perform_non_action_goal_success, compile_as, kw_function),
   set_opv(u_perform_non_action_goal_success,
	   function,
	   f_u_perform_non_action_goal_success),
   DefunResult=u_perform_non_action_goal_success.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For non-empty bd, this is done in run-plan",
				     18,
				     25916)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (if-interested-in rule (bd-print bd *gate-dbg*))",
				     1,
				     25985)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: this is inelegant; needs to be redid (redone).",
				     1,
				     26186)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (if (and (neq? top-level-goal (ob$get ob 'top-level-goal))",
				     1,
				     26241)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                           (eq? 'runable (ob$get ob 'top-level-goal)))",
				     1,
				     26319)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (progn", 1, 26391)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (ndbg-roman-nl *gate-dbg* rule-long",
				     1,
				     26421)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                      \"Wake up that top-level goal though\")",
				     1,
				     26480)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                       (surprise (ob$get ob 'top-level-goal))))",
				     1,
				     26557)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end block end if", 20, 27960)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:24850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Clears subgoals. Does not include goal.",
				     1,
				     28802)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:28843 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'clear-subgoals',
			    [goal, context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-xtra',
			      '$STRING'("Clearing subgoals of ~A in ~A"),
			      goal,
			      context
			    ],
			    
			    [ yloop,
			      
			      [ yfor,
				subgoal,
				in,
				
				[ 'goal-subgoals-uo',
				  goal,
				  context,
				  'belief-path'
				]
			      ],
			      
			      [ ydo,
				
				[ 'clear-subgoals',
				  subgoal,
				  context,
				  'belief-path'
				],
				
				[ 'cx$retract-relative',
				  context,
				  subgoal,
				  'belief-path'
				]
			      ]
			    ],
			    
			    [ yloop,
			      
			      [ yfor,
				intends,
				in,
				
				[ 'goal-intends-links-uo',
				  goal,
				  context,
				  'belief-path'
				]
			      ],
			      
			      [ ydo,
				
				[ 'cx$retract-relative',
				  context,
				  intends,
				  'belief-path'
				]
			      ]
			    ]
			  ]).

% annotating U::CLEAR-SUBGOALS 
wl: lambda_def(defun,
	      u_clear_subgoals,
	      f_u_clear_subgoals,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_xtra,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('C'),
			     #\(l),
			     #\(e),
			     #\(a),
			     #\(r),
			     #\(i),
			     #\(n),
			     #\(g),
			     #\(' '),
			     #\(s),
			     #\(u),
			     #\(b),
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l),
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
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_subgoal,
		    u_in,
		    [u_goal_subgoals_uo, u_goal, u_context, u_belief_path]
		  ],
		  
		  [ u_ydo,
		    [u_clear_subgoals, u_subgoal, u_context, u_belief_path],
		    
		    [ u_cx_c36_retract_relative,
		      u_context,
		      u_subgoal,
		      u_belief_path
		    ]
		  ]
		],
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_intends,
		    u_in,
		    [u_goal_intends_links_uo, u_goal, u_context, u_belief_path]
		  ],
		  
		  [ u_ydo,
		    
		    [ u_cx_c36_retract_relative,
		      u_context,
		      u_intends,
		      u_belief_path
		    ]
		  ]
		]
	      ]).


% annotating U::CLEAR-SUBGOALS 
wl: arglist_info(u_clear_subgoals,
		[u_goal, u_context, u_belief_path],
		[Goal_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CLEAR-SUBGOALS 
wl: init_args(exact_only, u_clear_subgoals).


% annotating U::CLEAR-SUBGOALS 
f_u_clear_subgoals(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('C'),
				       #\(l),
				       #\(e),
				       #\(a),
				       #\(r),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(b),
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l),
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
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_subgoal,
		      u_in,
		      [u_goal_subgoals_uo, u_goal, u_context, u_belief_path]
		    ],
		    
		    [ u_ydo,
		      [u_clear_subgoals, u_subgoal, u_context, u_belief_path],
		      
		      [ u_cx_c36_retract_relative,
			u_context,
			u_subgoal,
			u_belief_path
		      ]
		    ]
		  ],
		  Yloop_Ret),
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_intends,
		      u_in,
		      [u_goal_intends_links_uo, u_goal, u_context, u_belief_path]
		    ],
		    
		    [ u_ydo,
		      
		      [ u_cx_c36_retract_relative,
			u_context,
			u_intends,
			u_belief_path
		      ]
		    ]
		  ],
		  Yloop_Ret20),
	Yloop_Ret20=FnResult.
:- set_opv(f_u_clear_subgoals, classof, claz_function),
   set_opv(u_clear_subgoals, compile_as, kw_function),
   set_opv(u_clear_subgoals, function, f_u_clear_subgoals),
   DefunResult=u_clear_subgoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:29325 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'fake-inference-deletes',
			    ['subgoal-objs', rule, ctxt, 'belief-path'],
			    
			    [ let,
			      
			      [ [bd, '*empty-bd*'],
				[deletes, ['ob$gets', rule, [quote, delete]]]
			      ],
			      
			      [ if,
				deletes,
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("FAKE INFERENCE ~A ~A"),
				    rule,
				    ctxt
				  ],
				  
				  [ yloop,
				    
				    [ yfor,
				      'rule-subgoal-obj',
				      in,
				      ['ob$gets', rule, [quote, subgoal]]
				    ],
				    [yfor, 'subgoal-obj', in, 'subgoal-objs'],
				    
				    [ ydo,
				      
				      [ setq,
					bd,
					
					[ 'ob$unify',
					  'rule-subgoal-obj',
					  'subgoal-obj',
					  bd
					]
				      ]
				    ]
				  ],
				  
				  [ if,
				    ['null?', bd],
				    
				    [ error,
				      '$STRING'("null bd in fake-inference-deletes")
				    ],
				    [setq, bd, '*empty-bd*']
				  ],
				  
				  [ yloop,
				    [yfor, elem, in, deletes],
				    
				    [ ydo,
				      
				      [ 'cx$retract-relative',
					ctxt,
					['ob$instantiate-o', elem, bd],
					'belief-path'
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::FAKE-INFERENCE-DELETES 
wl: lambda_def(defun,
	      u_fake_inference_deletes,
	      f_u_fake_inference_deletes,
	      [u_subgoal_objs, u_rule, u_ctxt, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ [u_bd, u_xx_empty_bd_xx],
		    [u_deletes, [u_ob_c36_gets, u_rule, [quote, delete]]]
		  ],
		  
		  [ if,
		    u_deletes,
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('F'),
				   #\('A'),
				   #\('K'),
				   #\('E'),
				   #\(' '),
				   #\('I'),
				   #\('N'),
				   #\('F'),
				   #\('E'),
				   #\('R'),
				   #\('E'),
				   #\('N'),
				   #\('C'),
				   #\('E'),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_rule,
			u_ctxt
		      ],
		      
		      [ u_yloop,
			
			[ u_yfor,
			  u_rule_subgoal_obj,
			  u_in,
			  [u_ob_c36_gets, u_rule, [quote, u_subgoal]]
			],
			[u_yfor, u_subgoal_obj, u_in, u_subgoal_objs],
			
			[ u_ydo,
			  
			  [ setq,
			    u_bd,
			    
			    [ u_ob_c36_unify,
			      u_rule_subgoal_obj,
			      u_subgoal_obj,
			      u_bd
			    ]
			  ]
			]
		      ],
		      
		      [ if,
			[u_null_c63, u_bd],
			
			[ error,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(n),
				     #\(u),
				     #\(l),
				     #\(l),
				     #\(' '),
				     #\(b),
				     #\(d),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(' '),
				     #\(f),
				     #\(a),
				     #\(k),
				     #\(e),
				     #\(-),
				     #\(i),
				     #\(n),
				     #\(f),
				     #\(e),
				     #\(r),
				     #\(e),
				     #\(n),
				     #\(c),
				     #\(e),
				     #\(-),
				     #\(d),
				     #\(e),
				     #\(l),
				     #\(e),
				     #\(t),
				     #\(e),
				     #\(s)
				   ])
			],
			[setq, u_bd, u_xx_empty_bd_xx]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_elem, u_in, u_deletes],
			
			[ u_ydo,
			  
			  [ u_cx_c36_retract_relative,
			    u_ctxt,
			    [u_ob_c36_instantiate_o, u_elem, u_bd],
			    u_belief_path
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::FAKE-INFERENCE-DELETES 
wl: arglist_info(u_fake_inference_deletes,
		[u_subgoal_objs, u_rule, u_ctxt, u_belief_path],
		[Subgoal_objs_Param, Rule_Param, Ctxt_Param, Belief_path_Param],
		arginfo{ all:[u_subgoal_objs, u_rule, u_ctxt, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subgoal_objs, u_rule, u_ctxt, u_belief_path],
			 opt:0,
			 req:[u_subgoal_objs, u_rule, u_ctxt, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FAKE-INFERENCE-DELETES 
wl: init_args(exact_only, u_fake_inference_deletes).


% annotating U::FAKE-INFERENCE-DELETES 
f_u_fake_inference_deletes(Subgoal_objs_Param, Rule_Param, Ctxt_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_subgoal_objs, Subgoal_objs_Param), bv(u_rule, Rule_Param), bv(u_ctxt, Ctxt_Param), bv(u_belief_path, Belief_path_Param)],
	get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get),
	f_u_ob_c36_gets(Rule_Param, delete, Deletes_Init),
	LEnv=[[bv(u_bd, Xx_empty_bd_xx_Get), bv(u_deletes, Deletes_Init)]|Env],
	get_var(LEnv, u_deletes, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('F'),
					   #\('A'),
					   #\('K'),
					   #\('E'),
					   #\(' '),
					   #\('I'),
					   #\('N'),
					   #\('F'),
					   #\('E'),
					   #\('R'),
					   #\('E'),
					   #\('N'),
					   #\('C'),
					   #\('E'),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_rule,
				u_ctxt
			      ],
			      Roman_nl_Ret),
	    f_u_yloop(
		      [ 
			[ u_yfor,
			  u_rule_subgoal_obj,
			  u_in,
			  [u_ob_c36_gets, u_rule, [quote, u_subgoal]]
			],
			[u_yfor, u_subgoal_obj, u_in, u_subgoal_objs],
			
			[ u_ydo,
			  
			  [ setq,
			    u_bd,
			    
			    [ u_ob_c36_unify,
			      u_rule_subgoal_obj,
			      u_subgoal_obj,
			      u_bd
			    ]
			  ]
			]
		      ],
		      Yloop_Ret),
	    f_u_null_c63(u_bd, IFTEST28),
	    (   IFTEST28\==[]
	    ->  cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\(n),
				      #\(u),
				      #\(l),
				      #\(l),
				      #\(' '),
				      #\(b),
				      #\(d),
				      #\(' '),
				      #\(i),
				      #\(n),
				      #\(' '),
				      #\(f),
				      #\(a),
				      #\(k),
				      #\(e),
				      #\(-),
				      #\(i),
				      #\(n),
				      #\(f),
				      #\(e),
				      #\(r),
				      #\(e),
				      #\(n),
				      #\(c),
				      #\(e),
				      #\(-),
				      #\(d),
				      #\(e),
				      #\(l),
				      #\(e),
				      #\(t),
				      #\(e),
				      #\(s)
				    ])
			 ],
			 TrueResult),
		_112220=TrueResult
	    ;   get_var(LEnv, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get30),
		set_var(LEnv, u_bd, Xx_empty_bd_xx_Get30),
		_112220=Xx_empty_bd_xx_Get30
	    ),
	    f_u_yloop(
		      [ [u_yfor, u_elem, u_in, u_deletes],
			
			[ u_ydo,
			  
			  [ u_cx_c36_retract_relative,
			    u_ctxt,
			    [u_ob_c36_instantiate_o, u_elem, u_bd],
			    u_belief_path
			  ]
			]
		      ],
		      TrueResult33),
	    FnResult=TrueResult33
	;   FnResult=[]
	).
:- set_opv(f_u_fake_inference_deletes, classof, claz_function),
   set_opv(u_fake_inference_deletes, compile_as, kw_function),
   set_opv(u_fake_inference_deletes, function, f_u_fake_inference_deletes),
   DefunResult=u_fake_inference_deletes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:30109 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'or-inf',
			    [a, b],
			    
			    [ if,
			      [or, a, b],
			      
			      [ if,
				
				[ or,
				  ['eq?', a, [quote, inferences]],
				  ['eq?', b, [quote, inferences]]
				],
				[quote, inferences],
				t
			      ],
			      []
			    ]
			  ]).

% annotating U::OR-INF 
wl: lambda_def(defun,
	      u_or_inf,
	      f_u_or_inf,
	      [u_a, u_b],
	      
	      [ 
		[ if,
		  [or, u_a, u_b],
		  
		  [ if,
		    
		    [ or,
		      [u_eq_c63, u_a, [quote, u_inferences]],
		      [u_eq_c63, u_b, [quote, u_inferences]]
		    ],
		    [quote, u_inferences],
		    t
		  ],
		  []
		]
	      ]).


% annotating U::OR-INF 
wl: arglist_info(u_or_inf,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OR-INF 
wl: init_args(exact_only, u_or_inf).


% annotating U::OR-INF 
f_u_or_inf(A_Param, B_Param, FnResult) :-
	Env=[bv(u_a, A_Param), bv(u_b, B_Param)],
	(   A_Param\==[],
	    IFTEST=A_Param
	->  true
	;   IFTEST=B_Param
	),
	(   IFTEST\==[]
	->  (   f_u_eq_c63(u_a, [quote, u_inferences], FORM1_Res21),
		FORM1_Res21\==[],
		IFTEST19=FORM1_Res21
	    ->  true
	    ;   f_u_eq_c63(u_b, [quote, u_inferences], Eq_c63_Ret),
		IFTEST19=Eq_c63_Ret
	    ),
	    (   IFTEST19\==[]
	    ->  FnResult=u_inferences
	    ;   FnResult=t
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_or_inf, classof, claz_function),
   set_opv(u_or_inf, compile_as, kw_function),
   set_opv(u_or_inf, function, f_u_or_inf),
   DefunResult=u_or_inf.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:30109 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: there is a flaw in run-fact-plan. It thinks the goal succeeds even",
				     1,
				     30262)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:30109 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" if there are remaining unbound variables in the instantiated result.",
				     1,
				     30337)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:30407 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-fact-plan',
			    [ob, 'top-level-goal', context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-long',
			      '$STRING'("Run fact plan for ~A in ~A"),
			      ob,
			      context
			    ],
			    
			    [ let,
			      
			      [ 
				[ bds,
				  
				  [ 'cx$retrieve-relative',
				    context,
				    ['ob$get', ob, [quote, obj]],
				    'belief-path'
				  ]
				],
				['sprouted-context', []],
				['sprouted-contexts', []]
			      ],
			      
			      [ map,
				[quote, list],
				
				[ lambda,
				  [bd],
				  
				  [ if,
				    [not, ['empty-bd?', bd]],
				    
				    [ progn,
				      
				      [ setq,
					'sprouted-context',
					['cx$sprout', context]
				      ],
				      
				      [ 'delay-dbgs',
					'sprouted-context',
					
					[ 'set-ordering',
					  'sprouted-context',
					  1.0
					],
					
					[ setq,
					  'sprouted-contexts',
					  
					  [ cons,
					    'sprouted-context',
					    'sprouted-contexts'
					  ]
					],
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("Fact plan ~A found"),
					  [car, bd]
					],
					
					[ 'if-interested-in',
					  rule,
					  ['ob$sprint', [car, bd], '*gate-dbg*'],
					  ['ndbg-newline', '*gate-dbg*', rule],
					  ['bd-print', bd, '*gate-dbg*']
					],
					
					[ 'make-goal-success',
					  ob,
					  'sprouted-context',
					  [car, bd],
					  'belief-path',
					  bd
					]
				      ]
				    ]
				  ]
				],
				bds
			      ],
			      'sprouted-contexts'
			    ]
			  ]).

% annotating U::RUN-FACT-PLAN 
wl: lambda_def(defun,
	      u_run_fact_plan,
	      f_u_run_fact_plan,
	      [u_ob, u_top_level_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(n),
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
		  u_ob,
		  u_context
		],
		
		[ let,
		  
		  [ 
		    [ u_bds,
		      
		      [ u_cx_c36_retrieve_relative,
			u_context,
			[u_ob_c36_get, u_ob, [quote, u_obj]],
			u_belief_path
		      ]
		    ],
		    [u_sprouted_context, []],
		    [u_sprouted_contexts, []]
		  ],
		  
		  [ map,
		    [quote, list],
		    
		    [ lambda,
		      [u_bd],
		      
		      [ if,
			[not, [u_empty_bd_c63, u_bd]],
			
			[ progn,
			  
			  [ setq,
			    u_sprouted_context,
			    [u_cx_c36_sprout, u_context]
			  ],
			  
			  [ u_delay_dbgs,
			    u_sprouted_context,
			    [u_set_ordering, u_sprouted_context, 1.0],
			    
			    [ setq,
			      u_sprouted_contexts,
			      [cons, u_sprouted_context, u_sprouted_contexts]
			    ],
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('F'),
					 #\(a),
					 #\(c),
					 #\(t),
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
					 #\(u),
					 #\(n),
					 #\(d)
				       ]),
			      [car, u_bd]
			    ],
			    
			    [ u_if_interested_in,
			      u_rule,
			      [u_ob_c36_sprint, [car, u_bd], u_xx_gate_dbg_xx],
			      [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
			      [u_bd_print, u_bd, u_xx_gate_dbg_xx]
			    ],
			    
			    [ u_make_goal_success,
			      u_ob,
			      u_sprouted_context,
			      [car, u_bd],
			      u_belief_path,
			      u_bd
			    ]
			  ]
			]
		      ]
		    ],
		    u_bds
		  ],
		  u_sprouted_contexts
		]
	      ]).


% annotating U::RUN-FACT-PLAN 
wl: arglist_info(u_run_fact_plan,
		[u_ob, u_top_level_goal, u_context, u_belief_path],
		
		[ Ob_Param,
		  Top_level_goal_Param,
		  Context_Param,
		  Belief_path_Param
		],
		arginfo{ all:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_top_level_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-FACT-PLAN 
wl: init_args(exact_only, u_run_fact_plan).


% annotating U::RUN-FACT-PLAN 
f_u_run_fact_plan(Ob_Param, Top_level_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
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
			    u_ob,
			    u_context
			  ],
			  Roman_nl_Ret),
	f_u_ob_c36_get(Ob_Param, u_obj, Obj),
	f_u_cx_c36_retrieve_relative(Context_Param,
				     Obj,
				     Belief_path_Param,
				     Bds_Init),
	LEnv=[[bv(u_bds, Bds_Init), bv(u_sprouted_context, []), bv(u_sprouted_contexts, [])]|Env],
	Lambda=closure([Env|LEnv], LResult, [u_bd],  (get_var(Env, u_bd, Bd_Get), f_u_empty_bd_c63(Bd_Get, PredArgResult), (PredArgResult==[]->f_u_cx_c36_sprout(Context_Param, Sprouted_context), set_var(Env, u_sprouted_context, Sprouted_context), f_u_delay_dbgs(u_sprouted_context, [[u_set_ordering, u_sprouted_context, 1.0], [setq, u_sprouted_contexts, [cons, u_sprouted_context, u_sprouted_contexts]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, [#\('F'), #\(a), #\(c), #\(t), #\(' '), #\(p), #\(l), #\(a), #\(n), #\(' '), #\(~), #\('A'), #\(' '), #\(f), #\(o), #\(u), #\(n), #\(d)]), [car, u_bd]], [u_if_interested_in, u_rule, [u_ob_c36_sprint, [car, u_bd], u_xx_gate_dbg_xx], [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule], [u_bd_print, u_bd, u_xx_gate_dbg_xx]], [u_make_goal_success, u_ob, u_sprouted_context, [car, u_bd], u_belief_path, u_bd]], TrueResult), LResult=TrueResult;LResult=[]))),
	get_var(LEnv, u_bds, Bds_Get),
	cl_map(list, Lambda, Bds_Get, Map_Ret),
	get_var(LEnv, u_sprouted_contexts, Sprouted_contexts_Get),
	LetResult=Sprouted_contexts_Get,
	LetResult=FnResult.
:- set_opv(f_u_run_fact_plan, classof, claz_function),
   set_opv(u_run_fact_plan, compile_as, kw_function),
   set_opv(u_run_fact_plan, function, f_u_run_fact_plan),
   DefunResult=u_run_fact_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:30407 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Order of subgoals does not matter.",
				     1,
				     31422)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31458 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'calculate-supergoal-obj-strength',
			    [subgoals, rule],
			    
			    [ yloop,
			      
			      [ initial,
				[result, 0.0],
				
				[ weight,
				  
				  [ 'fl/',
				    ['ob$get', rule, [quote, plausibility]],
				    ['fixnum->flonum', [length, subgoals]]
				  ]
				]
			      ],
			      [yfor, subgoal, in, subgoals],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  
				  [ 'fl+',
				    result,
				    
				    [ 'fl*',
				      weight,
				      
				      [ strength,
					['ob$get', subgoal, [quote, obj]]
				      ]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::CALCULATE-SUPERGOAL-OBJ-STRENGTH 
wl: lambda_def(defun,
	      u_calculate_supergoal_obj_strength,
	      f_u_calculate_supergoal_obj_strength,
	      [u_subgoals, u_rule],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_result, 0.0],
		    
		    [ u_weight,
		      
		      [ u_fl_c47,
			[u_ob_c36_get, u_rule, [quote, u_plausibility]],
			[u_fixnum_c62_flonum, [length, u_subgoals]]
		      ]
		    ]
		  ],
		  [u_yfor, u_subgoal, u_in, u_subgoals],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      
		      [ u_fl_c43,
			u_result,
			
			[ u_fl_xx,
			  u_weight,
			  [u_strength, [u_ob_c36_get, u_subgoal, [quote, u_obj]]]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::CALCULATE-SUPERGOAL-OBJ-STRENGTH 
wl: arglist_info(u_calculate_supergoal_obj_strength,
		[u_subgoals, u_rule],
		[Subgoals_Param, Rule_Param],
		arginfo{ all:[u_subgoals, u_rule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subgoals, u_rule],
			 opt:0,
			 req:[u_subgoals, u_rule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CALCULATE-SUPERGOAL-OBJ-STRENGTH 
wl: init_args(exact_only, u_calculate_supergoal_obj_strength).


% annotating U::CALCULATE-SUPERGOAL-OBJ-STRENGTH 
f_u_calculate_supergoal_obj_strength(Subgoals_Param, Rule_Param, FnResult) :-
	Env=[bv(u_subgoals, Subgoals_Param), bv(u_rule, Rule_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, 0.0],
		      
		      [ u_weight,
			
			[ u_fl_c47,
			  [u_ob_c36_get, u_rule, [quote, u_plausibility]],
			  [u_fixnum_c62_flonum, [length, u_subgoals]]
			]
		      ]
		    ],
		    [u_yfor, u_subgoal, u_in, u_subgoals],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			
			[ u_fl_c43,
			  u_result,
			  
			  [ u_fl_xx,
			    u_weight,
			    
			    [ u_strength,
			      [u_ob_c36_get, u_subgoal, [quote, u_obj]]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_calculate_supergoal_obj_strength, classof, claz_function),
   set_opv(u_calculate_supergoal_obj_strength, compile_as, kw_function),
   set_opv(u_calculate_supergoal_obj_strength,
	   function,
	   f_u_calculate_supergoal_obj_strength),
   DefunResult=u_calculate_supergoal_obj_strength.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'subgoals-completed?',
			    ['active-goal', context, 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ subgoals,
				  
				  [ 'goal-subgoals-uo',
				    'active-goal',
				    context,
				    'belief-path'
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  subgoals,
				  
				  [ 'every?',
				    
				    [ lambda,
				      [x],
				      
				      [ 'ty$instance?',
					x,
					[quote, 'succeeded-goal']
				      ]
				    ],
				    subgoals
				  ]
				],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Subgoals of ~A completed"),
				    'active-goal'
				  ],
				  t
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::SUBGOALS-COMPLETED? 
wl: lambda_def(defun,
	      u_subgoals_completed_c63,
	      f_u_subgoals_completed_c63,
	      [u_active_goal, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_subgoals,
		      
		      [ u_goal_subgoals_uo,
			u_active_goal,
			u_context,
			u_belief_path
		      ]
		    ]
		  ],
		  
		  [ if,
		    
		    [ and,
		      u_subgoals,
		      
		      [ u_every_c63,
			
			[ lambda,
			  [u_x],
			  [u_ty_c36_instance_c63, u_x, [quote, u_succeeded_goal]]
			],
			u_subgoals
		      ]
		    ],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
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
				   #\(s),
				   #\(' '),
				   #\(o),
				   #\(f),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(c),
				   #\(o),
				   #\(m),
				   #\(p),
				   #\(l),
				   #\(e),
				   #\(t),
				   #\(e),
				   #\(d)
				 ]),
			u_active_goal
		      ],
		      t
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::SUBGOALS-COMPLETED? 
wl: arglist_info(u_subgoals_completed_c63,
		[u_active_goal, u_context, u_belief_path],
		[Active_goal_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_active_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_active_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_active_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBGOALS-COMPLETED? 
wl: init_args(exact_only, u_subgoals_completed_c63).


% annotating U::SUBGOALS-COMPLETED? 
f_u_subgoals_completed_c63(Active_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_active_goal, Active_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_goal_subgoals_uo(Active_goal_Param,
			     Context_Param,
			     Belief_path_Param,
			     Subgoals_Init),
	LEnv=[[bv(u_subgoals, Subgoals_Init)]|Env],
	get_var(LEnv, u_subgoals, IFTEST24),
	(   IFTEST24\==[]
	->  f_u_every_c63(
			  [ lambda,
			    [u_x],
			    
			    [ u_ty_c36_instance_c63,
			      u_x,
			      [quote, u_succeeded_goal]
			    ]
			  ],
			  u_subgoals,
			  TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('S'),
					   #\(u),
					   #\(b),
					   #\(g),
					   #\(o),
					   #\(a),
					   #\(l),
					   #\(s),
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(c),
					   #\(o),
					   #\(m),
					   #\(p),
					   #\(l),
					   #\(e),
					   #\(t),
					   #\(e),
					   #\(d)
					 ]),
				u_active_goal
			      ],
			      Roman_nl_Ret),
	    FnResult=t
	;   FnResult=[]
	).
:- set_opv(f_u_subgoals_completed_c63, classof, claz_function),
   set_opv(u_subgoals_completed_c63, compile_as, kw_function),
   set_opv(u_subgoals_completed_c63, function, f_u_subgoals_completed_c63),
   DefunResult=u_subgoals_completed_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 32230)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Comments on goal completion:", 1, 32232)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Could we do the below when inferences fire, instead of looping",
				     1,
				     32263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" through everything? Should the below create some sort of",
				     1,
				     32328)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 'achieves' link? (below is above now)",
				     1,
				     32387)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 32427)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below isn't nice from a debugging printout pov: it asserts",
				     1,
				     32429)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" before changing to success and thuis the assert message",
				     1,
				     32494)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" may appear mislaeading.", 1, 32552)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 32578)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" FOR ANALOGICAL Non-realistic goals, we need to assume goal success",
				     1,
				     32580)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" if all subgoals succeed.", 1, 32649)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 32676)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" --> The below should be modified to go until it quiesces, now",
				     1,
				     32678)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" that auto subgoal -> goal success exists. Or should we",
				     1,
				     32742)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:31850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" keep this stuff in different contexts?",
				     1,
				     32799)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:32840 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'any-subgoals-failed?',
			    ['active-goal', context, 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ subgoals,
				  
				  [ 'goal-subgoals-uo',
				    'active-goal',
				    context,
				    'belief-path'
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  subgoals,
				  
				  [ 'any?',
				    
				    [ lambda,
				      [x],
				      ['ty$instance?', x, [quote, 'failed-goal']]
				    ],
				    subgoals
				  ]
				],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Subgoal of ~A failed"),
				    'active-goal'
				  ],
				  t
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::ANY-SUBGOALS-FAILED? 
wl: lambda_def(defun,
	      u_any_subgoals_failed_c63,
	      f_u_any_subgoals_failed_c63,
	      [u_active_goal, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_subgoals,
		      
		      [ u_goal_subgoals_uo,
			u_active_goal,
			u_context,
			u_belief_path
		      ]
		    ]
		  ],
		  
		  [ if,
		    
		    [ and,
		      u_subgoals,
		      
		      [ u_any_c63,
			
			[ lambda,
			  [u_x],
			  [u_ty_c36_instance_c63, u_x, [quote, u_failed_goal]]
			],
			u_subgoals
		      ]
		    ],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
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
				   #\(o),
				   #\(f),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(f),
				   #\(a),
				   #\(i),
				   #\(l),
				   #\(e),
				   #\(d)
				 ]),
			u_active_goal
		      ],
		      t
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::ANY-SUBGOALS-FAILED? 
wl: arglist_info(u_any_subgoals_failed_c63,
		[u_active_goal, u_context, u_belief_path],
		[Active_goal_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_active_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_active_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_active_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ANY-SUBGOALS-FAILED? 
wl: init_args(exact_only, u_any_subgoals_failed_c63).


% annotating U::ANY-SUBGOALS-FAILED? 
f_u_any_subgoals_failed_c63(Active_goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_active_goal, Active_goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_goal_subgoals_uo(Active_goal_Param,
			     Context_Param,
			     Belief_path_Param,
			     Subgoals_Init),
	LEnv=[[bv(u_subgoals, Subgoals_Init)]|Env],
	get_var(LEnv, u_subgoals, IFTEST24),
	(   IFTEST24\==[]
	->  f_u_any_c63(
			[ lambda,
			  [u_x],
			  [u_ty_c36_instance_c63, u_x, [quote, u_failed_goal]]
			],
			u_subgoals,
			TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('S'),
					   #\(u),
					   #\(b),
					   #\(g),
					   #\(o),
					   #\(a),
					   #\(l),
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(f),
					   #\(a),
					   #\(i),
					   #\(l),
					   #\(e),
					   #\(d)
					 ]),
				u_active_goal
			      ],
			      Roman_nl_Ret),
	    FnResult=t
	;   FnResult=[]
	).
:- set_opv(f_u_any_subgoals_failed_c63, classof, claz_function),
   set_opv(u_any_subgoals_failed_c63, compile_as, kw_function),
   set_opv(u_any_subgoals_failed_c63, function, f_u_any_subgoals_failed_c63),
   DefunResult=u_any_subgoals_failed_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33209 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'seq-head?',
			    [goal, context, 'belief-path'],
			    
			    [ not,
			      
			      [ 'any?',
				
				[ lambda,
				  [x],
				  ['cx$true-relative', context, x, 'belief-path']
				],
				['ob$gets', goal, [quote, 'seq-next-of']]
			      ]
			    ]
			  ]).

% annotating U::SEQ-HEAD? 
wl: lambda_def(defun,
	      u_seq_head_c63,
	      f_u_seq_head_c63,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ not,
		  
		  [ u_any_c63,
		    
		    [ lambda,
		      [u_x],
		      [u_cx_c36_true_relative, u_context, u_x, u_belief_path]
		    ],
		    [u_ob_c36_gets, u_goal, [quote, u_seq_next_of]]
		  ]
		]
	      ]).


% annotating U::SEQ-HEAD? 
wl: arglist_info(u_seq_head_c63,
		[u_goal, u_context, u_belief_path],
		[Goal_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SEQ-HEAD? 
wl: init_args(exact_only, u_seq_head_c63).


% annotating U::SEQ-HEAD? 
f_u_seq_head_c63(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_any_c63(
		    [ lambda,
		      [u_x],
		      [u_cx_c36_true_relative, u_context, u_x, u_belief_path]
		    ],
		    [u_ob_c36_gets, u_goal, [quote, u_seq_next_of]],
		    Not_Param),
	cl_not(Not_Param, Not_Ret),
	Not_Ret=FnResult.
:- set_opv(f_u_seq_head_c63, classof, claz_function),
   set_opv(u_seq_head_c63, compile_as, kw_function),
   set_opv(u_seq_head_c63, function, f_u_seq_head_c63),
   DefunResult=u_seq_head_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33209 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Assumes goal is seq-head.", 1, 33366)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33393 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'next-in-seq',
			    [goal, context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'rule-xtra',
			      '$STRING'("Go to next in RSEQ from ~A in ~A"),
			      goal,
			      context
			    ],
			    
			    [ yloop,
			      
			      [ yfor,
				'seq-next',
				in,
				['ob$gets', goal, [quote, 'seq-next']]
			      ],
			      
			      [ ydo,
				
				[ if,
				  
				  [ 'cx$true-relative',
				    context,
				    'seq-next',
				    'belief-path'
				  ],
				  
				  [ progn,
				    
				    [ 'ob$remove',
				      goal,
				      [quote, 'seq-next'],
				      'seq-next'
				    ],
				    
				    [ if,
				      
				      [ 'ty$instance?',
					'seq-next',
					[quote, 'succeeded-goal']
				      ],
				      
				      [ 'next-in-seq',
					'seq-next',
					context,
					'belief-path'
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::NEXT-IN-SEQ 
wl: lambda_def(defun,
	      u_next_in_seq,
	      f_u_next_in_seq,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule_xtra,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('G'),
			     #\(o),
			     #\(' '),
			     #\(t),
			     #\(o),
			     #\(' '),
			     #\(n),
			     #\(e),
			     #\(x),
			     #\(t),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\('R'),
			     #\('S'),
			     #\('E'),
			     #\('Q'),
			     #\(' '),
			     #\(f),
			     #\(r),
			     #\(o),
			     #\(m),
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
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_seq_next,
		    u_in,
		    [u_ob_c36_gets, u_goal, [quote, u_seq_next]]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ u_cx_c36_true_relative,
			u_context,
			u_seq_next,
			u_belief_path
		      ],
		      
		      [ progn,
			[u_ob_c36_remove, u_goal, [quote, u_seq_next], u_seq_next],
			
			[ if,
			  
			  [ u_ty_c36_instance_c63,
			    u_seq_next,
			    [quote, u_succeeded_goal]
			  ],
			  [u_next_in_seq, u_seq_next, u_context, u_belief_path]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::NEXT-IN-SEQ 
wl: arglist_info(u_next_in_seq,
		[u_goal, u_context, u_belief_path],
		[Goal_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_goal, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_context, u_belief_path],
			 opt:0,
			 req:[u_goal, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NEXT-IN-SEQ 
wl: init_args(exact_only, u_next_in_seq).


% annotating U::NEXT-IN-SEQ 
f_u_next_in_seq(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('G'),
				       #\(o),
				       #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(n),
				       #\(e),
				       #\(x),
				       #\(t),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\('R'),
				       #\('S'),
				       #\('E'),
				       #\('Q'),
				       #\(' '),
				       #\(f),
				       #\(r),
				       #\(o),
				       #\(m),
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
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_seq_next,
		      u_in,
		      [u_ob_c36_gets, u_goal, [quote, u_seq_next]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_seq_next,
			  u_belief_path
			],
			
			[ progn,
			  
			  [ u_ob_c36_remove,
			    u_goal,
			    [quote, u_seq_next],
			    u_seq_next
			  ],
			  
			  [ if,
			    
			    [ u_ty_c36_instance_c63,
			      u_seq_next,
			      [quote, u_succeeded_goal]
			    ],
			    [u_next_in_seq, u_seq_next, u_context, u_belief_path]
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_next_in_seq, classof, claz_function),
   set_opv(u_next_in_seq, compile_as, kw_function),
   set_opv(u_next_in_seq, function, f_u_next_in_seq),
   DefunResult=u_next_in_seq.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-goal-success',
			    
			    [ 'active-goal',
			      context,
			      dependency,
			      'belief-path',
			      bd
			    ],
			    
			    [ let,
			      
			      [ 
				[ 'top-level-goal',
				  
				  [ 'ob$get',
				    'active-goal',
				    [quote, 'top-level-goal']
				  ]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("******************")
			      ],
			      
			      [ 'ndbg-roman',
				'*gate-dbg*',
				rule,
				'$STRING'("Goal")
			      ],
			      
			      [ 'ndbg-roman',
				'*gate-dbg*',
				rule,
				'$STRING'(" ~A"),
				'active-goal'
			      ],
			      
			      [ 'ndbg-roman',
				'*gate-dbg*',
				rule,
				'$STRING'(" succeeds")
			      ],
			      
			      [ 'ndbg-roman',
				'*gate-dbg*',
				rule,
				'$STRING'(" in ~A"),
				context
			      ],
			      ['ndbg-newline', '*gate-dbg*', rule],
			      
			      [ if,
				
				[ 'neq?',
				  [quote, runable],
				  ['ob$get', 'top-level-goal', [quote, status]]
				],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Waking up halted or waiting top-level goal")
				  ],
				  
				  [ 'change-tlg-status',
				    'top-level-goal',
				    [quote, runable]
				  ]
				]
			      ],
			      
			      [ let,
				
				[ 
				  [ 'new-ob',
				    
				    [ or,
				      
				      [ 'plan-instantiate',
					'active-goal',
					bd,
					context,
					'top-level-goal',
					'belief-path',
					t
				      ],
				      'active-goal'
				    ]
				  ]
				],
				
				[ if,
				  ['seq-head?', 'new-ob', context, 'belief-path'],
				  
				  [ 'next-in-seq',
				    'new-ob',
				    context,
				    'belief-path'
				  ]
				],
				
				[ if,
				  dependency,
				  
				  [ 'cx$assert-relative',
				    context,
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'DEPENDENCY',
					  'linked-from',
					  ['#COMMA', dependency],
					  'linked-to',
					  
					  [ '#COMMA',
					    ['ob$get', 'new-ob', [quote, obj]]
					  ],
					  weight,
					  1.0,
					  offset,
					  0.0,
					  decay,
					  0.0
					]
				      ]
				    ],
				    'belief-path'
				  ]
				],
				
				[ if,
				  
				  [ and,
				    ['personal-goal?', 'new-ob'],
				    ['neq?', 'top-level-goal', 'active-goal'],
				    ['me-belief-path?', 'belief-path']
				  ],
				  
				  [ 'personal-goal-outcome',
				    'new-ob',
				    context,
				    'top-level-goal'
				  ]
				],
				'new-ob'
			      ]
			    ]
			  ]).

% annotating U::MAKE-GOAL-SUCCESS 
wl: lambda_def(defun,
	      u_make_goal_success,
	      f_u_make_goal_success,
	      [u_active_goal, u_context, u_dependency, u_belief_path, u_bd],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_top_level_goal,
		      [u_ob_c36_get, u_active_goal, [quote, u_top_level_goal]]
		    ]
		  ],
		  
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
			     [#\('G'), #\(o), #\(a), #\(l)])
		  ],
		  
		  [ u_ndbg_roman,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*], claz_base_character, [#\(' '), #\(~), #\('A')]),
		    u_active_goal
		  ],
		  
		  [ u_ndbg_roman,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(' '),
			       #\(s),
			       #\(u),
			       #\(c),
			       #\(c),
			       #\(e),
			       #\(e),
			       #\(d),
			       #\(s)
			     ])
		  ],
		  
		  [ u_ndbg_roman,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(' '), #\(i), #\(n), #\(' '), #\(~), #\('A')]),
		    u_context
		  ],
		  [u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		  
		  [ if,
		    
		    [ u_neq_c63,
		      [quote, u_runable],
		      [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
		    ],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('W'),
				   #\(a),
				   #\(k),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(u),
				   #\(p),
				   #\(' '),
				   #\(h),
				   #\(a),
				   #\(l),
				   #\(t),
				   #\(e),
				   #\(d),
				   #\(' '),
				   #\(o),
				   #\(r),
				   #\(' '),
				   #\(w),
				   #\(a),
				   #\(i),
				   #\(t),
				   #\(i),
				   #\(n),
				   #\(g),
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
		      [u_change_tlg_status, u_top_level_goal, [quote, u_runable]]
		    ]
		  ],
		  
		  [ let,
		    
		    [ 
		      [ u_new_ob,
			
			[ or,
			  
			  [ u_plan_instantiate,
			    u_active_goal,
			    u_bd,
			    u_context,
			    u_top_level_goal,
			    u_belief_path,
			    t
			  ],
			  u_active_goal
			]
		      ]
		    ],
		    
		    [ if,
		      [u_seq_head_c63, u_new_ob, u_context, u_belief_path],
		      [u_next_in_seq, u_new_ob, u_context, u_belief_path]
		    ],
		    
		    [ if,
		      u_dependency,
		      
		      [ u_cx_c36_assert_relative,
			u_context,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_dependency,
			      u_linked_from,
			      ['#COMMA', u_dependency],
			      u_linked_to,
			      ['#COMMA', [u_ob_c36_get, u_new_ob, [quote, u_obj]]],
			      u_weight,
			      1.0,
			      u_offset,
			      0.0,
			      u_decay,
			      0.0
			    ]
			  ]
			],
			u_belief_path
		      ]
		    ],
		    
		    [ if,
		      
		      [ and,
			[u_personal_goal_c63, u_new_ob],
			[u_neq_c63, u_top_level_goal, u_active_goal],
			[u_me_belief_path_c63, u_belief_path]
		      ],
		      
		      [ u_personal_goal_outcome,
			u_new_ob,
			u_context,
			u_top_level_goal
		      ]
		    ],
		    u_new_ob
		  ]
		]
	      ]).


% annotating U::MAKE-GOAL-SUCCESS 
wl: arglist_info(u_make_goal_success,
		[u_active_goal, u_context, u_dependency, u_belief_path, u_bd],
		
		[ Active_goal_Param,
		  Context_Param,
		  Dependency_Param,
		  Belief_path_Param,
		  Bd_Param
		],
		arginfo{ all:
			     [ u_active_goal,
			       u_context,
			       u_dependency,
			       u_belief_path,
			       u_bd
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_active_goal,
				 u_context,
				 u_dependency,
				 u_belief_path,
				 u_bd
			       ],
			 opt:0,
			 req:
			     [ u_active_goal,
			       u_context,
			       u_dependency,
			       u_belief_path,
			       u_bd
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-GOAL-SUCCESS 
wl: init_args(exact_only, u_make_goal_success).


% annotating U::MAKE-GOAL-SUCCESS 
f_u_make_goal_success(Active_goal_Param, Context_Param, Dependency_Param, Belief_path_Param, Bd_Param, FnResult) :-
	Env=[bv(u_active_goal, Active_goal_Param), bv(u_context, Context_Param), bv(u_dependency, Dependency_Param), bv(u_belief_path, Belief_path_Param), bv(u_bd, Bd_Param)],
	f_u_ob_c36_get(Active_goal_Param, u_top_level_goal, Top_level_goal_Init),
	LEnv=[[bv(u_top_level_goal, Top_level_goal_Init)]|Env],
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
				  [#\('G'), #\(o), #\(a), #\(l)])
		       ],
		       Ndbg_roman_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  [#\(' '), #\(~), #\('A')]),
			 u_active_goal
		       ],
		       Ndbg_roman_Ret75),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  
				  [ #\(' '),
				    #\(s),
				    #\(u),
				    #\(c),
				    #\(c),
				    #\(e),
				    #\(e),
				    #\(d),
				    #\(s)
				  ])
		       ],
		       Ndbg_roman_Ret76),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  [#\(' '), #\(i), #\(n), #\(' '), #\(~), #\('A')]),
			 u_context
		       ],
		       Ndbg_roman_Ret77),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	f_u_neq_c63([quote, u_runable],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_status]],
		    IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('W'),
					   #\(a),
					   #\(k),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(' '),
					   #\(u),
					   #\(p),
					   #\(' '),
					   #\(h),
					   #\(a),
					   #\(l),
					   #\(t),
					   #\(e),
					   #\(d),
					   #\(' '),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\(w),
					   #\(a),
					   #\(i),
					   #\(t),
					   #\(i),
					   #\(n),
					   #\(g),
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
			      Roman_nl_Ret78),
	    get_var(LEnv, u_top_level_goal, Top_level_goal_Get),
	    f_u_change_tlg_status(Top_level_goal_Get, u_runable, TrueResult),
	    _113638=TrueResult
	;   _113638=[]
	),
	(   get_var(LEnv, u_top_level_goal, Top_level_goal_Get34),
	    f_u_plan_instantiate(Active_goal_Param,
				 Bd_Param,
				 Context_Param,
				 Top_level_goal_Get34,
				 Belief_path_Param,
				 t,
				 FORM1_Res),
	    FORM1_Res\==[],
	    New_ob_Init=FORM1_Res
	->  true
	;   New_ob_Init=Active_goal_Param
	),
	Env=[[bv(u_new_ob, New_ob_Init)]|LEnv],
	get_var(Env, u_new_ob, New_ob_Get),
	f_u_seq_head_c63(New_ob_Get, Context_Param, Belief_path_Param, IFTEST39),
	(   IFTEST39\==[]
	->  get_var(Env, u_new_ob, New_ob_Get45),
	    f_u_next_in_seq(New_ob_Get45,
			    Context_Param,
			    Belief_path_Param,
			    TrueResult48),
	    _114036=TrueResult48
	;   _114036=[]
	),
	(   Dependency_Param\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_dependency,
				   u_linked_from,
				   ['#COMMA', u_dependency],
				   u_linked_to,
				   
				   [ '#COMMA',
				     [u_ob_c36_get, u_new_ob, [quote, u_obj]]
				   ],
				   u_weight,
				   1.0,
				   u_offset,
				   0.0,
				   u_decay,
				   0.0
				 ]
			       ],
			       C36_fcreate_Ret),
	    f_u_cx_c36_assert_relative(Context_Param,
				       C36_fcreate_Ret,
				       Belief_path_Param,
				       TrueResult54),
	    _114368=TrueResult54
	;   _114368=[]
	),
	get_var(Env, u_new_ob, New_ob_Get59),
	f_u_personal_goal_c63(New_ob_Get59, IFTEST57),
	(   IFTEST57\==[]
	->  f_u_neq_c63(u_top_level_goal, u_active_goal, IFTEST60),
	    (   IFTEST60\==[]
	    ->  f_u_me_belief_path_c63(u_belief_path, TrueResult62),
		IFTEST55=TrueResult62
	    ;   IFTEST55=[]
	    )
	;   IFTEST55=[]
	),
	(   IFTEST55\==[]
	->  get_var(Env, u_new_ob, New_ob_Get64),
	    get_var(Env, u_top_level_goal, Top_level_goal_Get66),
	    f_u_personal_goal_outcome(New_ob_Get64,
				      Context_Param,
				      Top_level_goal_Get66,
				      TrueResult67),
	    _114556=TrueResult67
	;   _114556=[]
	),
	get_var(Env, u_new_ob, New_ob_Get68),
	LetResult=New_ob_Get68,
	LetResult=FnResult.
:- set_opv(f_u_make_goal_success, classof, claz_function),
   set_opv(u_make_goal_success, compile_as, kw_function),
   set_opv(u_make_goal_success, function, f_u_make_goal_success),
   DefunResult=u_make_goal_success.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Don't sprout context. It should already be sprouted if the caller wants that.",
				     1,
				     34005)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Who, if anyone, uses the action link created below?",
				     1,
				     35308)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Was used for rationalization LEADTO detection.",
				     1,
				     35362)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    (if (ty$instance? (ob$get goal 'obj) 'action)",
				     1,
				     35411)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (ol-set-relative new-ob",
				     1,
				     35462)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                         *action-link-ob*",
				     1,
				     35495)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                         (ob$get goal 'obj)",
				     1,
				     35538)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                         context",
				     1,
				     35583)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                         belief-path))",
				     1,
				     35617)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below generates emotional responses",
				     8,
				     35664)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" for subgoalized personal goals in REVERSAL, etc.",
				     8,
				     35713)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Was only for subgoals of top-level-goal",
				     1,
				     35854)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (eq? (goal-supergoal new-ob context) top-level-goal)",
				     1,
				     35896)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 36094)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" plan-instantiate: if bd not empty, instantiate everything in the planning",
				     1,
				     36096)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" structure with the new bindings.",
				     1,
				     36172)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 36207)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Does not instantiate goals if no variables in the goal (except for the",
				     1,
				     36209)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" active-goal which is always instantiated).",
				     1,
				     36282)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 36327)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The top-level goal is never replaced, since top-level goals are",
				     1,
				     36329)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" used as unique, global task identifiers. However, the obj slot",
				     1,
				     36395)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" may be instantiated, since the objective of a top-level goal is",
				     1,
				     36460)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" global (the same across contexts).",
				     1,
				     36526)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (But what about on final top-level goal success?)",
				     1,
				     36563)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:33869 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 36615)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'plan-instantiate',
			    
			    [ 'active-goal',
			      bd,
			      context,
			      'top-level-goal',
			      'belief-path',
			      'goal-success?'
			    ],
			    
			    [ let,
			      [['new-ob', []]],
			      
			      [ if,
				[not, ['empty-bd?', bd]],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Instantiating plan for ~A"),
				  ['tlg->string', 'top-level-goal']
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'rule-long',
				'$STRING'("Ag = ~A, cx = ~A, bd = ~A, tlg = ~A"),
				'active-goal',
				context,
				bd,
				'top-level-goal'
			      ],
			      
			      [ 'no-gen',
				
				[ if,
				  [not, ['empty-bd?', bd]],
				  
				  [ yloop,
				    [yfor, elem, in, ['cx$get-all', context]],
				    
				    [ ydo,
				      
				      [ setq,
					elem,
					
					[ 'absolute->relative',
					  elem,
					  'belief-path'
					]
				      ],
				      
				      [ if,
					
					[ and,
					  elem,
					  ['ty$instance?', elem, [quote, goal]],
					  
					  [ 'eq?',
					    
					    [ 'ob$get',
					      elem,
					      [quote, 'top-level-goal']
					    ],
					    'top-level-goal'
					  ],
					  ['neq?', elem, 'active-goal'],
					  
					  [ 'vars-in?',
					    ['ob$get', elem, [quote, obj]]
					  ]
					],
					
					[ if,
					  ['eq?', elem, 'top-level-goal'],
					  ['replace-obj', elem, bd],
					  
					  [ 'replace-linked-ob',
					    elem,
					    context,
					    'belief-path',
					    bd
					  ]
					]
				      ]
				    ]
				  ]
				],
				
				[ if,
				  ['neq?', 'active-goal', 'top-level-goal'],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      'rule-xtra',
				      '$STRING'("ag = ~A, tlg = ~A"),
				      'active-goal',
				      'top-level-goal'
				    ],
				    
				    [ setq,
				      'new-ob',
				      
				      [ 'replace-linked-ob',
					'active-goal',
					context,
					'belief-path',
					bd
				      ]
				    ]
				  ],
				  ['replace-obj', 'top-level-goal', bd]
				]
			      ],
			      
			      [ if,
				'goal-success?',
				
				[ let,
				  [['goal-ob', [or, 'new-ob', 'active-goal']]],
				  
				  [ 'cx$retract-relative',
				    context,
				    'goal-ob',
				    'belief-path'
				  ],
				  
				  [ 'ob$set',
				    'goal-ob',
				    [quote, type],
				    '*succeeded-goal-ob*'
				  ],
				  
				  [ if,
				    
				    [ and,
				      [not, ['dd-goal?', 'active-goal']],
				      
				      [ 'ga-gen-on-outcome?',
					
					[ 'ob$get',
					  'goal-ob',
					  [quote, 'gen-advice']
					]
				      ]
				    ],
				    
				    [ 'cx$assert-relative',
				      context,
				      'goal-ob',
				      'belief-path'
				    ],
				    
				    [ 'no-gen',
				      
				      [ 'cx$assert-relative',
					context,
					'goal-ob',
					'belief-path'
				      ]
				    ]
				  ]
				]
			      ],
			      'new-ob'
			    ]
			  ]).

% annotating U::PLAN-INSTANTIATE 
wl: lambda_def(defun,
	      u_plan_instantiate,
	      f_u_plan_instantiate,
	      
	      [ u_active_goal,
		u_bd,
		u_context,
		u_top_level_goal,
		u_belief_path,
		u_goal_success_c63
	      ],
	      
	      [ 
		[ let,
		  [[u_new_ob, []]],
		  
		  [ if,
		    [not, [u_empty_bd_c63, u_bd]],
		    
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
				 #\(i),
				 #\(n),
				 #\(g),
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
				 #\('A')
			       ]),
		      [u_tlg_c62_string, u_top_level_goal]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule_long,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('A'),
			       #\(g),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(','),
			       #\(' '),
			       #\(c),
			       #\(x),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(','),
			       #\(' '),
			       #\(b),
			       #\(d),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(','),
			       #\(' '),
			       #\(t),
			       #\(l),
			       #\(g),
			       #\(' '),
			       #\(=),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_active_goal,
		    u_context,
		    u_bd,
		    u_top_level_goal
		  ],
		  
		  [ u_no_gen,
		    
		    [ if,
		      [not, [u_empty_bd_c63, u_bd]],
		      
		      [ u_yloop,
			[u_yfor, u_elem, u_in, [u_cx_c36_get_all, u_context]],
			
			[ u_ydo,
			  
			  [ setq,
			    u_elem,
			    [u_absolute_c62_relative, u_elem, u_belief_path]
			  ],
			  
			  [ if,
			    
			    [ and,
			      u_elem,
			      [u_ty_c36_instance_c63, u_elem, [quote, u_goal]],
			      
			      [ u_eq_c63,
				[u_ob_c36_get, u_elem, [quote, u_top_level_goal]],
				u_top_level_goal
			      ],
			      [u_neq_c63, u_elem, u_active_goal],
			      
			      [ u_vars_in_c63,
				[u_ob_c36_get, u_elem, [quote, u_obj]]
			      ]
			    ],
			    
			    [ if,
			      [u_eq_c63, u_elem, u_top_level_goal],
			      [u_replace_obj, u_elem, u_bd],
			      
			      [ u_replace_linked_ob,
				u_elem,
				u_context,
				u_belief_path,
				u_bd
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ if,
		      [u_neq_c63, u_active_goal, u_top_level_goal],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(a),
				     #\(g),
				     #\(' '),
				     #\(=),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(','),
				     #\(' '),
				     #\(t),
				     #\(l),
				     #\(g),
				     #\(' '),
				     #\(=),
				     #\(' '),
				     #\(~),
				     #\('A')
				   ]),
			  u_active_goal,
			  u_top_level_goal
			],
			
			[ setq,
			  u_new_ob,
			  
			  [ u_replace_linked_ob,
			    u_active_goal,
			    u_context,
			    u_belief_path,
			    u_bd
			  ]
			]
		      ],
		      [u_replace_obj, u_top_level_goal, u_bd]
		    ]
		  ],
		  
		  [ if,
		    u_goal_success_c63,
		    
		    [ let,
		      [[u_goal_ob, [or, u_new_ob, u_active_goal]]],
		      
		      [ u_cx_c36_retract_relative,
			u_context,
			u_goal_ob,
			u_belief_path
		      ],
		      
		      [ u_ob_c36_set,
			u_goal_ob,
			[quote, type],
			u_xx_succeeded_goal_ob_xx
		      ],
		      
		      [ if,
			
			[ and,
			  [not, [u_dd_goal_c63, u_active_goal]],
			  
			  [ u_ga_gen_on_outcome_c63,
			    [u_ob_c36_get, u_goal_ob, [quote, u_gen_advice]]
			  ]
			],
			
			[ u_cx_c36_assert_relative,
			  u_context,
			  u_goal_ob,
			  u_belief_path
			],
			
			[ u_no_gen,
			  
			  [ u_cx_c36_assert_relative,
			    u_context,
			    u_goal_ob,
			    u_belief_path
			  ]
			]
		      ]
		    ]
		  ],
		  u_new_ob
		]
	      ]).


% annotating U::PLAN-INSTANTIATE 
wl: arglist_info(u_plan_instantiate,
		
		[ u_active_goal,
		  u_bd,
		  u_context,
		  u_top_level_goal,
		  u_belief_path,
		  u_goal_success_c63
		],
		
		[ Active_goal_Param,
		  Bd_Param,
		  Context_Param,
		  Top_level_goal_Param,
		  Belief_path_Param,
		  Goal_success_c63_Param
		],
		arginfo{ all:
			     [ u_active_goal,
			       u_bd,
			       u_context,
			       u_top_level_goal,
			       u_belief_path,
			       u_goal_success_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_active_goal,
				 u_bd,
				 u_context,
				 u_top_level_goal,
				 u_belief_path,
				 u_goal_success_c63
			       ],
			 opt:0,
			 req:
			     [ u_active_goal,
			       u_bd,
			       u_context,
			       u_top_level_goal,
			       u_belief_path,
			       u_goal_success_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PLAN-INSTANTIATE 
wl: init_args(exact_only, u_plan_instantiate).


% annotating U::PLAN-INSTANTIATE 
f_u_plan_instantiate(Active_goal_Param, Bd_Param, Context_Param, Top_level_goal_Param, Belief_path_Param, Goal_success_c63_Param, FnResult) :-
	Env=[bv(u_active_goal, Active_goal_Param), bv(u_bd, Bd_Param), bv(u_context, Context_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_belief_path, Belief_path_Param), bv(u_goal_success_c63, Goal_success_c63_Param)],
	LEnv=[[bv(u_new_ob, [])]|Env],
	f_u_empty_bd_c63(Bd_Param, PredArgResult),
	(   PredArgResult==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
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
					   #\(i),
					   #\(n),
					   #\(g),
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
					   #\('A')
					 ]),
				[u_tlg_c62_string, u_top_level_goal]
			      ],
			      TrueResult),
	    _115714=TrueResult
	;   _115714=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(g),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(c),
				       #\(x),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(b),
				       #\(d),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(','),
				       #\(' '),
				       #\(t),
				       #\(l),
				       #\(g),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_active_goal,
			    u_context,
			    u_bd,
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	f_u_no_gen(
		   [ 
		     [ if,
		       [not, [u_empty_bd_c63, u_bd]],
		       
		       [ u_yloop,
			 [u_yfor, u_elem, u_in, [u_cx_c36_get_all, u_context]],
			 
			 [ u_ydo,
			   
			   [ setq,
			     u_elem,
			     [u_absolute_c62_relative, u_elem, u_belief_path]
			   ],
			   
			   [ if,
			     
			     [ and,
			       u_elem,
			       [u_ty_c36_instance_c63, u_elem, [quote, u_goal]],
			       
			       [ u_eq_c63,
				 
				 [ u_ob_c36_get,
				   u_elem,
				   [quote, u_top_level_goal]
				 ],
				 u_top_level_goal
			       ],
			       [u_neq_c63, u_elem, u_active_goal],
			       
			       [ u_vars_in_c63,
				 [u_ob_c36_get, u_elem, [quote, u_obj]]
			       ]
			     ],
			     
			     [ if,
			       [u_eq_c63, u_elem, u_top_level_goal],
			       [u_replace_obj, u_elem, u_bd],
			       
			       [ u_replace_linked_ob,
				 u_elem,
				 u_context,
				 u_belief_path,
				 u_bd
			       ]
			     ]
			   ]
			 ]
		       ]
		     ],
		     
		     [ if,
		       [u_neq_c63, u_active_goal, u_top_level_goal],
		       
		       [ progn,
			 
			 [ u_ndbg_roman_nl,
			   u_xx_gate_dbg_xx,
			   u_rule_xtra,
			   '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\(a),
				      #\(g),
				      #\(' '),
				      #\(=),
				      #\(' '),
				      #\(~),
				      #\('A'),
				      #\(','),
				      #\(' '),
				      #\(t),
				      #\(l),
				      #\(g),
				      #\(' '),
				      #\(=),
				      #\(' '),
				      #\(~),
				      #\('A')
				    ]),
			   u_active_goal,
			   u_top_level_goal
			 ],
			 
			 [ setq,
			   u_new_ob,
			   
			   [ u_replace_linked_ob,
			     u_active_goal,
			     u_context,
			     u_belief_path,
			     u_bd
			   ]
			 ]
		       ],
		       [u_replace_obj, u_top_level_goal, u_bd]
		     ]
		   ],
		   No_gen_Ret),
	(   Goal_success_c63_Param\==[]
	->  (   get_var(LEnv, u_new_ob, New_ob_Get),
		New_ob_Get\==[],
		Goal_ob_Init=New_ob_Get
	    ->  true
	    ;   Goal_ob_Init=Active_goal_Param
	    ),
	    Env=[[bv(u_goal_ob, Goal_ob_Init)]|LEnv],
	    get_var(Env, u_goal_ob, Goal_ob_Get),
	    f_u_cx_c36_retract_relative(Context_Param,
					Goal_ob_Get,
					Belief_path_Param,
					Retract_relative_Ret),
	    get_var(Env, u_goal_ob, Goal_ob_Get43),
	    get_var(Env, u_xx_succeeded_goal_ob_xx, Xx_succeeded_goal_ob_xx_Get),
	    f_u_ob_c36_set(Goal_ob_Get43,
			   type,
			   Xx_succeeded_goal_ob_xx_Get,
			   C36_set_Ret),
	    f_u_dd_goal_c63(Active_goal_Param, PredArgResult50),
	    (   PredArgResult50==[]
	    ->  get_var(Env, u_goal_ob, Goal_ob_Get51),
		f_u_ob_c36_get(Goal_ob_Get51, u_gen_advice, Gen_advice),
		f_u_ga_gen_on_outcome_c63(Gen_advice, TrueResult52),
		IFTEST45=TrueResult52
	    ;   IFTEST45=[]
	    ),
	    (   IFTEST45\==[]
	    ->  get_var(Env, u_goal_ob, Goal_ob_Get54),
		f_u_cx_c36_assert_relative(Context_Param,
					   Goal_ob_Get54,
					   Belief_path_Param,
					   TrueResult56),
		_115844=TrueResult56
	    ;   f_u_no_gen(
			   [ 
			     [ u_cx_c36_assert_relative,
			       u_context,
			       u_goal_ob,
			       u_belief_path
			     ]
			   ],
			   ElseResult),
		_115844=ElseResult
	    )
	;   _115844=[]
	),
	get_var(LEnv, u_new_ob, New_ob_Get60),
	LetResult=New_ob_Get60,
	LetResult=FnResult.
:- set_opv(f_u_plan_instantiate, classof, claz_function),
   set_opv(u_plan_instantiate, compile_as, kw_function),
   set_opv(u_plan_instantiate, function, f_u_plan_instantiate),
   DefunResult=u_plan_instantiate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (neq? elem top-level-goal) ", 24, 37285)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" save some object copying", 49, 37518)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end and", 21, 37565)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" We need to retract and reassert because obs in contexts with",
				     12,
				     38177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" hashing are restricted not to change type.",
				     12,
				     38251)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (eq? active-goal top-level-goal)",
				     21,
				     38431)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 38742)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" In the below, we should also stop activity on other active subgoals,",
				     1,
				     38744)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" no? But watch out--not in the case where this is called from",
				     1,
				     38815)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" all-possibilities-exhausted? ... Now I don't see why this is so; I am going",
				     1,
				     38878)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to try it anyway and see what happens. ... As of early November 86, there",
				     1,
				     38956)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" seems to be no trouble with this.",
				     1,
				     39032)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 39068)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Must check for \"protection violations\": if the objective of a",
				     1,
				     39070)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" succeeded subgoal is not true, the subgoal becomes a failed subgoal.",
				     1,
				     39140)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Don't check goals whose objectives are actions.",
				     1,
				     39211)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" But only do this if the rule does not say not to (i.e., 'scripts'",
				     1,
				     39261)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" like M-RESTAURANT do not obey this property).",
				     1,
				     39329)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:36616 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 39377)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-goal-failure',
			    
			    [ 'active-goal',
			      context,
			      dependency,
			      'belief-path',
			      'top-level-goal',
			      'all-poss-failed?'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("******************")
			    ],
			    ['ndbg-roman', '*gate-dbg*', rule, '$STRING'("Goal")],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'(" ~A"),
			      'active-goal'
			    ],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'(" fails")
			    ],
			    
			    [ 'ndbg-roman',
			      '*gate-dbg*',
			      rule,
			      '$STRING'(" in ~A"),
			      context
			    ],
			    ['ndbg-newline', '*gate-dbg*', rule],
			    
			    [ let,
			      [['new-ob', 'active-goal']],
			      
			      [ if,
				[not, ['eq?', 'active-goal', 'top-level-goal']],
				
				[ progn,
				  
				  [ 'clear-subgoals',
				    'active-goal',
				    context,
				    'belief-path'
				  ],
				  
				  [ setq,
				    'new-ob',
				    
				    [ 'replace-linked-ob',
				      'active-goal',
				      context,
				      'belief-path',
				      '*empty-bd*'
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				
				[ or,
				  [not, ['eq?', 'active-goal', 'top-level-goal']],
				  'all-poss-failed?'
				],
				
				[ progn,
				  
				  [ 'cx$retract-relative',
				    context,
				    'new-ob',
				    'belief-path'
				  ],
				  
				  [ 'ob$set',
				    'new-ob',
				    [quote, type],
				    '*failed-goal-ob*'
				  ],
				  
				  [ if,
				    
				    [ and,
				      [not, ['dd-goal?', 'new-ob']],
				      
				      [ 'ga-gen-on-outcome?',
					
					[ 'ob$get',
					  'new-ob',
					  [quote, 'gen-advice']
					]
				      ]
				    ],
				    
				    [ 'cx$assert-relative',
				      context,
				      'new-ob',
				      'belief-path'
				    ],
				    
				    [ 'no-gen',
				      
				      [ 'cx$assert-relative',
					context,
					'new-ob',
					'belief-path'
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  ['eq?', 'active-goal', 'top-level-goal'],
				  ['null?', 'all-poss-failed?']
				],
				
				[ 'ob$add',
				  'active-goal',
				  [quote, 'failed-context'],
				  context
				]
			      ],
			      
			      [ if,
				dependency,
				
				[ 'cx$assert-relative',
				  context,
				  
				  [ 'ob$fcreate',
				    
				    [ '#BQ',
				      
				      [ 'DEPENDENCY',
					'linked-from',
					['#COMMA', dependency],
					'linked-to',
					['#COMMA', 'top-level-goal'],
					weight,
					1.0,
					offset,
					0.0,
					decay,
					0.0
				      ]
				    ]
				  ],
				  'belief-path'
				]
			      ],
			      'new-ob'
			    ]
			  ]).

% annotating U::MAKE-GOAL-FAILURE 
wl: lambda_def(defun,
	      u_make_goal_failure,
	      f_u_make_goal_failure,
	      
	      [ u_active_goal,
		u_context,
		u_dependency,
		u_belief_path,
		u_top_level_goal,
		u_all_poss_failed_c63
	      ],
	      
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
			   [#\('G'), #\(o), #\(a), #\(l)])
		],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*], claz_base_character, [#\(' '), #\(~), #\('A')]),
		  u_active_goal
		],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(' '), #\(f), #\(a), #\(i), #\(l), #\(s)])
		],
		
		[ u_ndbg_roman,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(' '), #\(i), #\(n), #\(' '), #\(~), #\('A')]),
		  u_context
		],
		[u_ndbg_newline, u_xx_gate_dbg_xx, u_rule],
		
		[ let,
		  [[u_new_ob, u_active_goal]],
		  
		  [ if,
		    [not, [u_eq_c63, u_active_goal, u_top_level_goal]],
		    
		    [ progn,
		      [u_clear_subgoals, u_active_goal, u_context, u_belief_path],
		      
		      [ setq,
			u_new_ob,
			
			[ u_replace_linked_ob,
			  u_active_goal,
			  u_context,
			  u_belief_path,
			  u_xx_empty_bd_xx
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    
		    [ or,
		      [not, [u_eq_c63, u_active_goal, u_top_level_goal]],
		      u_all_poss_failed_c63
		    ],
		    
		    [ progn,
		      
		      [ u_cx_c36_retract_relative,
			u_context,
			u_new_ob,
			u_belief_path
		      ],
		      
		      [ u_ob_c36_set,
			u_new_ob,
			[quote, type],
			u_xx_failed_goal_ob_xx
		      ],
		      
		      [ if,
			
			[ and,
			  [not, [u_dd_goal_c63, u_new_ob]],
			  
			  [ u_ga_gen_on_outcome_c63,
			    [u_ob_c36_get, u_new_ob, [quote, u_gen_advice]]
			  ]
			],
			
			[ u_cx_c36_assert_relative,
			  u_context,
			  u_new_ob,
			  u_belief_path
			],
			
			[ u_no_gen,
			  
			  [ u_cx_c36_assert_relative,
			    u_context,
			    u_new_ob,
			    u_belief_path
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    
		    [ and,
		      [u_eq_c63, u_active_goal, u_top_level_goal],
		      [u_null_c63, u_all_poss_failed_c63]
		    ],
		    
		    [ u_ob_c36_add,
		      u_active_goal,
		      [quote, u_failed_context],
		      u_context
		    ]
		  ],
		  
		  [ if,
		    u_dependency,
		    
		    [ u_cx_c36_assert_relative,
		      u_context,
		      
		      [ u_ob_c36_fcreate,
			
			[ '#BQ',
			  
			  [ u_dependency,
			    u_linked_from,
			    ['#COMMA', u_dependency],
			    u_linked_to,
			    ['#COMMA', u_top_level_goal],
			    u_weight,
			    1.0,
			    u_offset,
			    0.0,
			    u_decay,
			    0.0
			  ]
			]
		      ],
		      u_belief_path
		    ]
		  ],
		  u_new_ob
		]
	      ]).


% annotating U::MAKE-GOAL-FAILURE 
wl: arglist_info(u_make_goal_failure,
		
		[ u_active_goal,
		  u_context,
		  u_dependency,
		  u_belief_path,
		  u_top_level_goal,
		  u_all_poss_failed_c63
		],
		
		[ Active_goal_Param,
		  Context_Param,
		  Dependency_Param,
		  Belief_path_Param,
		  Top_level_goal_Param,
		  All_poss_failed_c63_Param
		],
		arginfo{ all:
			     [ u_active_goal,
			       u_context,
			       u_dependency,
			       u_belief_path,
			       u_top_level_goal,
			       u_all_poss_failed_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_active_goal,
				 u_context,
				 u_dependency,
				 u_belief_path,
				 u_top_level_goal,
				 u_all_poss_failed_c63
			       ],
			 opt:0,
			 req:
			     [ u_active_goal,
			       u_context,
			       u_dependency,
			       u_belief_path,
			       u_top_level_goal,
			       u_all_poss_failed_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-GOAL-FAILURE 
wl: init_args(exact_only, u_make_goal_failure).


% annotating U::MAKE-GOAL-FAILURE 
f_u_make_goal_failure(Active_goal_Param, Context_Param, Dependency_Param, Belief_path_Param, Top_level_goal_Param, All_poss_failed_c63_Param, New_ob_Get45) :-
	Env=[bv(u_active_goal, Active_goal_Param), bv(u_context, Context_Param), bv(u_dependency, Dependency_Param), bv(u_belief_path, Belief_path_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_all_poss_failed_c63, All_poss_failed_c63_Param)],
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
				  [#\('G'), #\(o), #\(a), #\(l)])
		       ],
		       Ndbg_roman_Ret),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  [#\(' '), #\(~), #\('A')]),
			 u_active_goal
		       ],
		       Ndbg_roman_Ret83),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  [#\(' '), #\(f), #\(a), #\(i), #\(l), #\(s)])
		       ],
		       Ndbg_roman_Ret84),
	f_u_ndbg_roman(u_xx_gate_dbg_xx,
		       u_rule,
		       
		       [ '$ARRAY'([*],
				  claz_base_character,
				  [#\(' '), #\(i), #\(n), #\(' '), #\(~), #\('A')]),
			 u_context
		       ],
		       Ndbg_roman_Ret85),
	f_u_ndbg_newline(u_xx_gate_dbg_xx, u_rule, Rule),
	LEnv=[[bv(u_new_ob, Active_goal_Param)]|Env],
	f_u_eq_c63(u_active_goal, u_top_level_goal, PredArgResult),
	(   PredArgResult==[]
	->  f_u_clear_subgoals(Active_goal_Param,
			       Context_Param,
			       Belief_path_Param,
			       Clear_subgoals_Ret),
	    get_var(LEnv, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get),
	    f_u_replace_linked_ob(Active_goal_Param,
				  Context_Param,
				  Belief_path_Param,
				  Xx_empty_bd_xx_Get,
				  TrueResult),
	    set_var(LEnv, u_new_ob, TrueResult),
	    _114900=TrueResult
	;   _114900=[]
	),
	(   f_u_eq_c63(u_active_goal, u_top_level_goal, Top_level_goal),
	    cl_not(Top_level_goal, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST38=FORM1_Res
	->  true
	;   IFTEST38=All_poss_failed_c63_Param
	),
	(   IFTEST38\==[]
	->  get_var(LEnv, u_new_ob, New_ob_Get45),
	    f_u_cx_c36_retract_relative(Context_Param,
					New_ob_Get45,
					Belief_path_Param,
					Retract_relative_Ret),
	    get_var(LEnv, u_xx_failed_goal_ob_xx, Xx_failed_goal_ob_xx_Get),
	    f_u_ob_c36_set(New_ob_Get45,
			   type,
			   Xx_failed_goal_ob_xx_Get,
			   C36_set_Ret),
	    f_u_dd_goal_c63(New_ob_Get45, PredArgResult52),
	    (   PredArgResult52==[]
	    ->  f_u_ob_c36_get(New_ob_Get45, u_gen_advice, Gen_advice),
		f_u_ga_gen_on_outcome_c63(Gen_advice, TrueResult54),
		IFTEST47=TrueResult54
	    ;   IFTEST47=[]
	    ),
	    (   IFTEST47\==[]
	    ->  f_u_cx_c36_assert_relative(Context_Param,
					   New_ob_Get45,
					   Belief_path_Param,
					   TrueResult58),
		TrueResult60=TrueResult58
	    ;   f_u_no_gen(
			   [ 
			     [ u_cx_c36_assert_relative,
			       u_context,
			       u_new_ob,
			       u_belief_path
			     ]
			   ],
			   ElseResult),
		TrueResult60=ElseResult
	    )
	;   TrueResult60=[]
	),
	f_u_eq_c63(u_active_goal, u_top_level_goal, IFTEST63),
	(   IFTEST63\==[]
	->  f_u_null_c63(u_all_poss_failed_c63, TrueResult65),
	    IFTEST61=TrueResult65
	;   IFTEST61=[]
	),
	(   IFTEST61\==[]
	->  f_u_ob_c36_add(Active_goal_Param,
			   u_failed_context,
			   Context_Param,
			   TrueResult68),
	    _115956=TrueResult68
	;   _115956=[]
	),
	(   Dependency_Param\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_dependency,
				   u_linked_from,
				   ['#COMMA', u_dependency],
				   u_linked_to,
				   ['#COMMA', u_top_level_goal],
				   u_weight,
				   1.0,
				   u_offset,
				   0.0,
				   u_decay,
				   0.0
				 ]
			       ],
			       C36_fcreate_Ret),
	    f_u_cx_c36_assert_relative(Context_Param,
				       C36_fcreate_Ret,
				       Belief_path_Param,
				       TrueResult74),
	    _116172=TrueResult74
	;   _116172=[]
	).
:- set_opv(f_u_make_goal_failure, classof, claz_function),
   set_opv(u_make_goal_failure, compile_as, kw_function),
   set_opv(u_make_goal_failure, function, f_u_make_goal_failure),
   DefunResult=u_make_goal_failure.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" We need to retract and reassert because obs in contexts with",
				     5,
				     40053)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" hashing are restricted not to change type.",
				     5,
				     40120)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (if (eq? active-goal top-level-goal)",
				     1,
				     40319)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (ob$set new-ob 'active-goal active-goal))",
				     1,
				     40365)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Since any failed subgoal implies top-level goal failure (in",
				     8,
				     40861)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" this context at least), we can connect dependency directly",
				     8,
				     40930)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to top-level goal. (was new-ob).",
				     8,
				     40998)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:39378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: must omit because else a cx would get copied.",
				     1,
				     41518)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:41571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'replace-linked-ob',
			    [ob, context, 'belief-path', bd],
			    
			    [ 'no-gen',
			      
			      [ 'replace-linked-ob1',
				ob,
				context,
				'belief-path',
				
				[ 'ob$instan-omit',
				  ob,
				  bd,
				  function('varize-object?'),
				  [],
				  '*link-slots*',
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::REPLACE-LINKED-OB 
wl: lambda_def(defun,
	      u_replace_linked_ob,
	      f_u_replace_linked_ob,
	      [u_ob, u_context, u_belief_path, u_bd],
	      
	      [ 
		[ u_no_gen,
		  
		  [ u_replace_linked_ob1,
		    u_ob,
		    u_context,
		    u_belief_path,
		    
		    [ u_ob_c36_instan_omit,
		      u_ob,
		      u_bd,
		      function(u_varize_object_c63),
		      [],
		      u_xx_link_slots_xx,
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::REPLACE-LINKED-OB 
wl: arglist_info(u_replace_linked_ob,
		[u_ob, u_context, u_belief_path, u_bd],
		[Ob_Param, Context_Param, Belief_path_Param, Bd_Param],
		arginfo{ all:[u_ob, u_context, u_belief_path, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context, u_belief_path, u_bd],
			 opt:0,
			 req:[u_ob, u_context, u_belief_path, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REPLACE-LINKED-OB 
wl: init_args(exact_only, u_replace_linked_ob).


% annotating U::REPLACE-LINKED-OB 
f_u_replace_linked_ob(Ob_Param, Context_Param, Belief_path_Param, Bd_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param), bv(u_bd, Bd_Param)],
	f_u_no_gen(
		   [ 
		     [ u_replace_linked_ob1,
		       u_ob,
		       u_context,
		       u_belief_path,
		       
		       [ u_ob_c36_instan_omit,
			 u_ob,
			 u_bd,
			 function(u_varize_object_c63),
			 [],
			 u_xx_link_slots_xx,
			 []
		       ]
		     ]
		   ],
		   No_gen_Ret),
	No_gen_Ret=FnResult.
:- set_opv(f_u_replace_linked_ob, classof, claz_function),
   set_opv(u_replace_linked_ob, compile_as, kw_function),
   set_opv(u_replace_linked_ob, function, f_u_replace_linked_ob),
   DefunResult=u_replace_linked_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:41571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" cannot be 0, because we want to instantiate",
				     30,
				     41771)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:41571 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the goal obj with bd.", 30, 41846)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:41941 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'replace-linked-ob1',
			    [ob, context, 'belief-path', 'ob-copy'],
			    
			    [ let,
			      
			      [ 
				[ 'from-links',
				  ['ob$gets', ob, [quote, 'linked-from-of']]
				],
				
				[ 'to-links',
				  ['ob$gets', ob, [quote, 'linked-to-of']]
				],
				
				[ 'seq-nexts',
				  ['ob$gets', ob, [quote, 'seq-next']]
				],
				
				[ 'seq-next-ofs',
				  ['ob$gets', ob, [quote, 'seq-next-of']]
				],
				[episode, ['ob$get', ob, [quote, episode]]],
				
				[ 'preserve-values',
				  
				  [ map,
				    [quote, list],
				    
				    [ lambda,
				      ['slot-name'],
				      ['ob$get', ob, 'slot-name']
				    ],
				    '*preserve-link-slots*'
				  ]
				],
				
				[ 'plan-rule',
				  
				  [ if,
				    ['ob$get', ob, [quote, obj]],
				    
				    [ 'ob$get',
				      ['ob$get', ob, [quote, obj]],
				      [quote, 'plan-rule']
				    ],
				    []
				  ]
				],
				
				[ 'plan-subgoalnum',
				  
				  [ if,
				    ['ob$get', ob, [quote, obj]],
				    
				    [ 'ob$get',
				      ['ob$get', ob, [quote, obj]],
				      [quote, 'plan-subgoalnum']
				    ],
				    []
				  ]
				],
				
				[ 'obj-strength',
				  
				  [ if,
				    ['ob$get', ob, [quote, obj]],
				    
				    [ 'ob$get',
				      ['ob$get', ob, [quote, obj]],
				      [quote, strength]
				    ],
				    []
				  ]
				],
				[link, []]
			      ],
			      ['cx$retract-relative', context, ob, 'belief-path'],
			      
			      [ 'cx$assert-relative',
				context,
				'ob-copy',
				'belief-path'
			      ],
			      
			      [ yloop,
				[yfor, 'from-link', in, 'from-links'],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'cx$true-relative',
				      context,
				      'from-link',
				      'belief-path'
				    ],
				    
				    [ progn,
				      
				      [ setq,
					link,
					
					[ 'ob$copy-omit',
					  'from-link',
					  [quote, ['top-context']]
					]
				      ],
				      
				      [ 'ob$set',
					link,
					[quote, 'linked-from'],
					'ob-copy'
				      ],
				      
				      [ 'cx$retract-relative',
					context,
					'from-link',
					'belief-path'
				      ],
				      
				      [ 'cx$assert-relative',
					context,
					link,
					'belief-path'
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ yloop,
				[yfor, 'to-link', in, 'to-links'],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'cx$true-relative',
				      context,
				      'to-link',
				      'belief-path'
				    ],
				    
				    [ progn,
				      
				      [ setq,
					link,
					
					[ 'ob$copy-omit',
					  'to-link',
					  [quote, ['top-context']]
					]
				      ],
				      
				      [ 'ob$set',
					link,
					[quote, 'linked-to'],
					'ob-copy'
				      ],
				      
				      [ 'cx$retract-relative',
					context,
					'to-link',
					'belief-path'
				      ],
				      
				      [ 'cx$assert-relative',
					context,
					link,
					'belief-path'
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ yloop,
				[yfor, 'seq-next', in, 'seq-nexts'],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'cx$true-relative',
				      context,
				      'seq-next',
				      'belief-path'
				    ],
				    
				    [ 'ob$add',
				      'ob-copy',
				      [quote, 'seq-next'],
				      'seq-next'
				    ]
				  ]
				]
			      ],
			      
			      [ yloop,
				[yfor, 'seq-next-of', in, 'seq-next-ofs'],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'cx$true-relative',
				      context,
				      'seq-next-of',
				      'belief-path'
				    ],
				    
				    [ 'ob$add',
				      'ob-copy',
				      [quote, 'seq-next-of'],
				      'seq-next-of'
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				episode,
				
				[ progn,
				  ['ob$set', 'ob-copy', [quote, episode], episode],
				  
				  [ if,
				    ['ty$instance?', 'ob-copy', [quote, goal]],
				    ['ob$set', episode, [quote, goal], 'ob-copy']
				  ]
				]
			      ],
			      
			      [ if,
				'plan-rule',
				
				[ 'ob$set',
				  ['ob$get', 'ob-copy', [quote, obj]],
				  [quote, 'plan-rule'],
				  'plan-rule'
				]
			      ],
			      
			      [ if,
				'plan-subgoalnum',
				
				[ 'ob$set',
				  ['ob$get', 'ob-copy', [quote, obj]],
				  [quote, 'plan-subgoalnum'],
				  'plan-subgoalnum'
				]
			      ],
			      
			      [ if,
				'obj-strength',
				
				[ 'set-strength',
				  ['ob$get', 'ob-copy', [quote, obj]],
				  'obj-strength'
				]
			      ],
			      
			      [ yloop,
				[initial, ['slot-values', 'preserve-values']],
				[yfor, 'slot-name', in, '*preserve-link-slots*'],
				
				[ ydo,
				  
				  [ if,
				    [car, 'slot-values'],
				    
				    [ 'ob$set',
				      'ob-copy',
				      'slot-name',
				      [car, 'slot-values']
				    ]
				  ],
				  [setq, 'slot-values', [cdr, 'slot-values']]
				]
			      ],
			      'ob-copy'
			    ]
			  ]).

% annotating U::REPLACE-LINKED-OB1 
wl: lambda_def(defun,
	      u_replace_linked_ob1,
	      f_u_replace_linked_ob1,
	      [u_ob, u_context, u_belief_path, u_ob_copy],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_from_links,
		      [u_ob_c36_gets, u_ob, [quote, u_linked_from_of]]
		    ],
		    [u_to_links, [u_ob_c36_gets, u_ob, [quote, u_linked_to_of]]],
		    [u_seq_nexts, [u_ob_c36_gets, u_ob, [quote, u_seq_next]]],
		    
		    [ u_seq_next_ofs,
		      [u_ob_c36_gets, u_ob, [quote, u_seq_next_of]]
		    ],
		    [u_episode, [u_ob_c36_get, u_ob, [quote, u_episode]]],
		    
		    [ u_preserve_values,
		      
		      [ map,
			[quote, list],
			[lambda, [u_slot_name], [u_ob_c36_get, u_ob, u_slot_name]],
			u_xx_preserve_link_slots_xx
		      ]
		    ],
		    
		    [ u_plan_rule,
		      
		      [ if,
			[u_ob_c36_get, u_ob, [quote, u_obj]],
			
			[ u_ob_c36_get,
			  [u_ob_c36_get, u_ob, [quote, u_obj]],
			  [quote, u_plan_rule]
			],
			[]
		      ]
		    ],
		    
		    [ u_plan_subgoalnum,
		      
		      [ if,
			[u_ob_c36_get, u_ob, [quote, u_obj]],
			
			[ u_ob_c36_get,
			  [u_ob_c36_get, u_ob, [quote, u_obj]],
			  [quote, u_plan_subgoalnum]
			],
			[]
		      ]
		    ],
		    
		    [ u_obj_strength,
		      
		      [ if,
			[u_ob_c36_get, u_ob, [quote, u_obj]],
			
			[ u_ob_c36_get,
			  [u_ob_c36_get, u_ob, [quote, u_obj]],
			  [quote, u_strength]
			],
			[]
		      ]
		    ],
		    [u_link, []]
		  ],
		  [u_cx_c36_retract_relative, u_context, u_ob, u_belief_path],
		  [u_cx_c36_assert_relative, u_context, u_ob_copy, u_belief_path],
		  
		  [ u_yloop,
		    [u_yfor, u_from_link, u_in, u_from_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_from_link,
			  u_belief_path
			],
			
			[ progn,
			  
			  [ setq,
			    u_link,
			    
			    [ u_ob_c36_copy_omit,
			      u_from_link,
			      [quote, [u_top_context]]
			    ]
			  ],
			  
			  [ u_ob_c36_set,
			    u_link,
			    [quote, u_linked_from],
			    u_ob_copy
			  ],
			  
			  [ u_cx_c36_retract_relative,
			    u_context,
			    u_from_link,
			    u_belief_path
			  ],
			  
			  [ u_cx_c36_assert_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_to_link, u_in, u_to_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_to_link,
			  u_belief_path
			],
			
			[ progn,
			  
			  [ setq,
			    u_link,
			    
			    [ u_ob_c36_copy_omit,
			      u_to_link,
			      [quote, [u_top_context]]
			    ]
			  ],
			  [u_ob_c36_set, u_link, [quote, u_linked_to], u_ob_copy],
			  
			  [ u_cx_c36_retract_relative,
			    u_context,
			    u_to_link,
			    u_belief_path
			  ],
			  
			  [ u_cx_c36_assert_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_seq_next, u_in, u_seq_nexts],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_seq_next,
			  u_belief_path
			],
			[u_ob_c36_add, u_ob_copy, [quote, u_seq_next], u_seq_next]
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_seq_next_of, u_in, u_seq_next_ofs],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_seq_next_of,
			  u_belief_path
			],
			
			[ u_ob_c36_add,
			  u_ob_copy,
			  [quote, u_seq_next_of],
			  u_seq_next_of
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_episode,
		    
		    [ progn,
		      [u_ob_c36_set, u_ob_copy, [quote, u_episode], u_episode],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_ob_copy, [quote, u_goal]],
			[u_ob_c36_set, u_episode, [quote, u_goal], u_ob_copy]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_plan_rule,
		    
		    [ u_ob_c36_set,
		      [u_ob_c36_get, u_ob_copy, [quote, u_obj]],
		      [quote, u_plan_rule],
		      u_plan_rule
		    ]
		  ],
		  
		  [ if,
		    u_plan_subgoalnum,
		    
		    [ u_ob_c36_set,
		      [u_ob_c36_get, u_ob_copy, [quote, u_obj]],
		      [quote, u_plan_subgoalnum],
		      u_plan_subgoalnum
		    ]
		  ],
		  
		  [ if,
		    u_obj_strength,
		    
		    [ u_set_strength,
		      [u_ob_c36_get, u_ob_copy, [quote, u_obj]],
		      u_obj_strength
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_slot_values, u_preserve_values]],
		    [u_yfor, u_slot_name, u_in, u_xx_preserve_link_slots_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			[car, u_slot_values],
			
			[ u_ob_c36_set,
			  u_ob_copy,
			  u_slot_name,
			  [car, u_slot_values]
			]
		      ],
		      [setq, u_slot_values, [cdr, u_slot_values]]
		    ]
		  ],
		  u_ob_copy
		]
	      ]).


% annotating U::REPLACE-LINKED-OB1 
wl: arglist_info(u_replace_linked_ob1,
		[u_ob, u_context, u_belief_path, u_ob_copy],
		[Ob_Param, Context_Param, Belief_path_Param, Ob_copy_Param],
		arginfo{ all:[u_ob, u_context, u_belief_path, u_ob_copy],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context, u_belief_path, u_ob_copy],
			 opt:0,
			 req:[u_ob, u_context, u_belief_path, u_ob_copy],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REPLACE-LINKED-OB1 
wl: init_args(exact_only, u_replace_linked_ob1).


% annotating U::REPLACE-LINKED-OB1 
f_u_replace_linked_ob1(Ob_Param, Context_Param, Belief_path_Param, Ob_copy_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param), bv(u_ob_copy, Ob_copy_Param)],
	f_u_ob_c36_gets(Ob_Param, u_linked_from_of, From_links_Init),
	f_u_ob_c36_gets(Ob_Param, u_linked_to_of, To_links_Init),
	f_u_ob_c36_gets(Ob_Param, u_seq_next, Seq_nexts_Init),
	f_u_ob_c36_gets(Ob_Param, u_seq_next_of, Seq_next_ofs_Init),
	f_u_ob_c36_get(Ob_Param, u_episode, Episode_Init),
	Lambda=closure([Env|Env], LResult, [u_slot_name],  (get_var(Env, u_slot_name, Slot_name_Get), f_u_ob_c36_get(Ob_Param, Slot_name_Get, LResult))),
	get_var(Env, u_xx_preserve_link_slots_xx, Xx_preserve_link_slots_xx_Get),
	cl_map(list, Lambda, Xx_preserve_link_slots_xx_Get, Preserve_values_Init),
	f_u_ob_c36_get(Ob_Param, u_obj, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Ob_Param, u_obj, Obj),
	    f_u_ob_c36_get(Obj, u_plan_rule, TrueResult),
	    Plan_rule_Init=TrueResult
	;   Plan_rule_Init=[]
	),
	f_u_ob_c36_get(Ob_Param, u_obj, IFTEST38),
	(   IFTEST38\==[]
	->  f_u_ob_c36_get(Ob_Param, u_obj, Obj96),
	    f_u_ob_c36_get(Obj96, u_plan_subgoalnum, TrueResult42),
	    Plan_subgoalnum_Init=TrueResult42
	;   Plan_subgoalnum_Init=[]
	),
	f_u_ob_c36_get(Ob_Param, u_obj, IFTEST43),
	(   IFTEST43\==[]
	->  f_u_ob_c36_get(Ob_Param, u_obj, Obj97),
	    f_u_ob_c36_get(Obj97, u_strength, TrueResult47),
	    Obj_strength_Init=TrueResult47
	;   Obj_strength_Init=[]
	),
	LEnv=[[bv(u_from_links, From_links_Init), bv(u_to_links, To_links_Init), bv(u_seq_nexts, Seq_nexts_Init), bv(u_seq_next_ofs, Seq_next_ofs_Init), bv(u_episode, Episode_Init), bv(u_preserve_values, Preserve_values_Init), bv(u_plan_rule, Plan_rule_Init), bv(u_plan_subgoalnum, Plan_subgoalnum_Init), bv(u_obj_strength, Obj_strength_Init), bv(u_link, [])]|Env],
	f_u_cx_c36_retract_relative(Context_Param,
				    Ob_Param,
				    Belief_path_Param,
				    Retract_relative_Ret),
	f_u_cx_c36_assert_relative(Context_Param,
				   Ob_copy_Param,
				   Belief_path_Param,
				   Assert_relative_Ret),
	f_u_yloop(
		  [ [u_yfor, u_from_link, u_in, u_from_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_from_link,
			  u_belief_path
			],
			
			[ progn,
			  
			  [ setq,
			    u_link,
			    
			    [ u_ob_c36_copy_omit,
			      u_from_link,
			      [quote, [u_top_context]]
			    ]
			  ],
			  
			  [ u_ob_c36_set,
			    u_link,
			    [quote, u_linked_from],
			    u_ob_copy
			  ],
			  
			  [ u_cx_c36_retract_relative,
			    u_context,
			    u_from_link,
			    u_belief_path
			  ],
			  
			  [ u_cx_c36_assert_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	f_u_yloop(
		  [ [u_yfor, u_to_link, u_in, u_to_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_to_link,
			  u_belief_path
			],
			
			[ progn,
			  
			  [ setq,
			    u_link,
			    
			    [ u_ob_c36_copy_omit,
			      u_to_link,
			      [quote, [u_top_context]]
			    ]
			  ],
			  [u_ob_c36_set, u_link, [quote, u_linked_to], u_ob_copy],
			  
			  [ u_cx_c36_retract_relative,
			    u_context,
			    u_to_link,
			    u_belief_path
			  ],
			  
			  [ u_cx_c36_assert_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			]
		      ]
		    ]
		  ],
		  Yloop_Ret103),
	f_u_yloop(
		  [ [u_yfor, u_seq_next, u_in, u_seq_nexts],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_seq_next,
			  u_belief_path
			],
			[u_ob_c36_add, u_ob_copy, [quote, u_seq_next], u_seq_next]
		      ]
		    ]
		  ],
		  Yloop_Ret104),
	f_u_yloop(
		  [ [u_yfor, u_seq_next_of, u_in, u_seq_next_ofs],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_cx_c36_true_relative,
			  u_context,
			  u_seq_next_of,
			  u_belief_path
			],
			
			[ u_ob_c36_add,
			  u_ob_copy,
			  [quote, u_seq_next_of],
			  u_seq_next_of
			]
		      ]
		    ]
		  ],
		  Yloop_Ret105),
	get_var(LEnv, u_episode, IFTEST64),
	(   IFTEST64\==[]
	->  get_var(LEnv, u_episode, Episode_Get68),
	    f_u_ob_c36_set(Ob_copy_Param, u_episode, Episode_Get68, C36_set_Ret),
	    f_u_ty_c36_instance_c63(Ob_copy_Param, u_goal, IFTEST69),
	    (   IFTEST69\==[]
	    ->  get_var(LEnv, u_episode, Episode_Get72),
		f_u_ob_c36_set(Episode_Get72,
			       u_goal,
			       Ob_copy_Param,
			       TrueResult74),
		TrueResult75=TrueResult74
	    ;   TrueResult75=[]
	    )
	;   TrueResult75=[]
	),
	get_var(LEnv, u_plan_rule, IFTEST76),
	(   IFTEST76\==[]
	->  f_u_ob_c36_get(Ob_copy_Param, u_obj, Obj98),
	    get_var(LEnv, u_plan_rule, Plan_rule_Get80),
	    f_u_ob_c36_set(Obj98, u_plan_rule, Plan_rule_Get80, TrueResult81),
	    _121472=TrueResult81
	;   _121472=[]
	),
	get_var(LEnv, u_plan_subgoalnum, IFTEST82),
	(   IFTEST82\==[]
	->  f_u_ob_c36_get(Ob_copy_Param, u_obj, Obj99),
	    get_var(LEnv, u_plan_subgoalnum, Plan_subgoalnum_Get86),
	    f_u_ob_c36_set(Obj99,
			   u_plan_subgoalnum,
			   Plan_subgoalnum_Get86,
			   TrueResult87),
	    _121668=TrueResult87
	;   _121668=[]
	),
	get_var(LEnv, u_obj_strength, IFTEST88),
	(   IFTEST88\==[]
	->  f_u_set_strength([u_ob_c36_get, u_ob_copy, [quote, u_obj]],
			     u_obj_strength,
			     TrueResult91),
	    _121894=TrueResult91
	;   _121894=[]
	),
	f_u_yloop(
		  [ [u_initial, [u_slot_values, u_preserve_values]],
		    [u_yfor, u_slot_name, u_in, u_xx_preserve_link_slots_xx],
		    
		    [ u_ydo,
		      
		      [ if,
			[car, u_slot_values],
			
			[ u_ob_c36_set,
			  u_ob_copy,
			  u_slot_name,
			  [car, u_slot_values]
			]
		      ],
		      [setq, u_slot_values, [cdr, u_slot_values]]
		    ]
		  ],
		  Yloop_Ret107),
	LetResult=Ob_copy_Param,
	LetResult=FnResult.
:- set_opv(f_u_replace_linked_ob1, classof, claz_function),
   set_opv(u_replace_linked_ob1, compile_as, kw_function),
   set_opv(u_replace_linked_ob1, function, f_u_replace_linked_ob1),
   DefunResult=u_replace_linked_ob1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:44731 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'replace-obj',
			    [goal, bd],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Replace obj of ~A with ~A"),
			      goal,
			      bd
			    ],
			    
			    [ 'let*',
			      
			      [ ['goal-obj', ['ob$get', goal, [quote, obj]]],
				
				[ 'new-obj',
				  
				  [ 'ob$instan-omit',
				    'goal-obj',
				    bd,
				    function('varize-object?'),
				    [],
				    '*link-slots*',
				    []
				  ]
				]
			      ],
			      
			      [ 'ob$set',
				'new-obj',
				[quote, 'plan-rule'],
				['ob$get', 'goal-obj', [quote, 'plan-rule']]
			      ],
			      
			      [ 'ob$set',
				'new-obj',
				[quote, 'plan-subgoalnum'],
				
				[ 'ob$get',
				  'goal-obj',
				  [quote, 'plan-subgoalnum']
				]
			      ],
			      
			      [ if,
				['ob$get', 'goal-obj', [quote, strength]],
				
				[ 'set-strength',
				  'new-obj',
				  ['ob$get', 'goal-obj', [quote, strength]]
				]
			      ],
			      ['ob$set', goal, [quote, obj], 'new-obj']
			    ]
			  ]).

% annotating U::REPLACE-OBJ 
wl: lambda_def(defun,
	      u_replace_obj,
	      f_u_replace_obj,
	      [u_goal, u_bd],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(e),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(c),
			     #\(e),
			     #\(' '),
			     #\(o),
			     #\(b),
			     #\(j),
			     #\(' '),
			     #\(o),
			     #\(f),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(w),
			     #\(i),
			     #\(t),
			     #\(h),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_goal,
		  u_bd
		],
		
		[ let_xx,
		  
		  [ [u_goal_obj, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		    
		    [ u_new_obj,
		      
		      [ u_ob_c36_instan_omit,
			u_goal_obj,
			u_bd,
			function(u_varize_object_c63),
			[],
			u_xx_link_slots_xx,
			[]
		      ]
		    ]
		  ],
		  
		  [ u_ob_c36_set,
		    u_new_obj,
		    [quote, u_plan_rule],
		    [u_ob_c36_get, u_goal_obj, [quote, u_plan_rule]]
		  ],
		  
		  [ u_ob_c36_set,
		    u_new_obj,
		    [quote, u_plan_subgoalnum],
		    [u_ob_c36_get, u_goal_obj, [quote, u_plan_subgoalnum]]
		  ],
		  
		  [ if,
		    [u_ob_c36_get, u_goal_obj, [quote, u_strength]],
		    
		    [ u_set_strength,
		      u_new_obj,
		      [u_ob_c36_get, u_goal_obj, [quote, u_strength]]
		    ]
		  ],
		  [u_ob_c36_set, u_goal, [quote, u_obj], u_new_obj]
		]
	      ]).


% annotating U::REPLACE-OBJ 
wl: arglist_info(u_replace_obj,
		[u_goal, u_bd],
		[Goal_Param, Bd_Param],
		arginfo{ all:[u_goal, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_bd],
			 opt:0,
			 req:[u_goal, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REPLACE-OBJ 
wl: init_args(exact_only, u_replace_obj).


% annotating U::REPLACE-OBJ 
f_u_replace_obj(Goal_Param, Bd_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_bd, Bd_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(c),
				       #\(e),
				       #\(' '),
				       #\(o),
				       #\(b),
				       #\(j),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_goal,
			    u_bd
			  ],
			  Roman_nl_Ret),
	f_u_ob_c36_get(Goal_Param, u_obj, Goal_obj_Init),
	LEnv=[[bv(u_goal_obj, Goal_obj_Init)]|Env],
	get_var(LEnv, u_goal_obj, Goal_obj_Get),
	get_var(LEnv, u_xx_link_slots_xx, Xx_link_slots_xx_Get),
	f_u_ob_c36_instan_omit(Goal_obj_Get,
			       Bd_Param,
			       function(u_varize_object_c63),
			       [],
			       Xx_link_slots_xx_Get,
			       [],
			       New_obj_Init),
	Env=[[bv(u_new_obj, New_obj_Init)]|LEnv],
	get_var(Env, u_goal_obj, Goal_obj_Get27),
	get_var(Env, u_new_obj, New_obj_Get),
	f_u_ob_c36_get(Goal_obj_Get27, u_plan_rule, Plan_rule),
	f_u_ob_c36_set(New_obj_Get, u_plan_rule, Plan_rule, C36_set_Ret),
	get_var(Env, u_goal_obj, Goal_obj_Get29),
	get_var(Env, u_new_obj, New_obj_Get28),
	f_u_ob_c36_get(Goal_obj_Get29, u_plan_subgoalnum, Plan_subgoalnum),
	f_u_ob_c36_set(New_obj_Get28,
		       u_plan_subgoalnum,
		       Plan_subgoalnum,
		       C36_set_Ret43),
	get_var(Env, u_goal_obj, Goal_obj_Get32),
	f_u_ob_c36_get(Goal_obj_Get32, u_strength, IFTEST),
	(   IFTEST\==[]
	->  f_u_set_strength(u_new_obj,
			     [u_ob_c36_get, u_goal_obj, [quote, u_strength]],
			     TrueResult),
	    _113436=TrueResult
	;   _113436=[]
	),
	get_var(Env, u_new_obj, New_obj_Get35),
	f_u_ob_c36_set(Goal_Param, u_obj, New_obj_Get35, C36_set_Ret44),
	LetResult=C36_set_Ret44,
	LetResult=FnResult.
:- set_opv(f_u_replace_obj, classof, claz_function),
   set_opv(u_replace_obj, compile_as, kw_function),
   set_opv(u_replace_obj, function, f_u_replace_obj),
   DefunResult=u_replace_obj.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_rule1.cl:44731 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 45246)).
:- true.


% Total time: 17.57 seconds


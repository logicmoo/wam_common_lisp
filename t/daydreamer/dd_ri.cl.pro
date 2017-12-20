
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_ri" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:13:20 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" This file contains:", 1, 208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:229 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Serendipity mechanism including rule intersection and verification",
				     1,
				     230)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:298 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 299)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:300 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 7/19/86: First version written", 1, 301)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 7/22/86: Generalized code for personal and daydreaming goals",
				     1,
				     334)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:396 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/25/86: Took out flavors", 1, 397)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:424 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 425)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:426 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     427)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:507 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 509)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:510 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Run serendipities for any recent entered concepts.",
				     1,
				     511)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:563 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 564)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:565 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'entered-concept-serendipity',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("Running entered concept serendipity")
			    ],
			    
			    [ let,
			      [[result, []]],
			      
			      [ yloop,
				[yfor, fact, in, '*entered-concepts*'],
				[yuntil, result],
				
				[ ydo,
				  [setq, result, ['run-fact-serendipity', fact]]
				]
			      ],
			      [setq, '*entered-concepts*', []],
			      result
			    ]
			  ]).

% annotating U::ENTERED-CONCEPT-SERENDIPITY 
wl: lambda_def(defun,
	      u_entered_concept_serendipity,
	      f_u_entered_concept_serendipity,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
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
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(d),
			     #\(' '),
			     #\(c),
			     #\(o),
			     #\(n),
			     #\(c),
			     #\(e),
			     #\(p),
			     #\(t),
			     #\(' '),
			     #\(s),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(p),
			     #\(i),
			     #\(t),
			     #\(y)
			   ])
		],
		
		[ let,
		  [[u_result, []]],
		  
		  [ u_yloop,
		    [u_yfor, u_fact, u_in, u_xx_entered_concepts_xx],
		    [u_yuntil, u_result],
		    [u_ydo, [setq, u_result, [u_run_fact_serendipity, u_fact]]]
		  ],
		  [setq, u_xx_entered_concepts_xx, []],
		  u_result
		]
	      ]).


% annotating U::ENTERED-CONCEPT-SERENDIPITY 
wl: arglist_info(u_entered_concept_serendipity,
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

% annotating U::ENTERED-CONCEPT-SERENDIPITY 
wl: init_args(exact_only, u_entered_concept_serendipity).


% annotating U::ENTERED-CONCEPT-SERENDIPITY 
f_u_entered_concept_serendipity(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
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
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(p),
				       #\(t),
				       #\(' '),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(p),
				       #\(i),
				       #\(t),
				       #\(y)
				     ])
			  ],
			  Roman_nl_Ret),
	LEnv=[[bv(u_result, [])]|Env],
	f_u_yloop(
		  [ [u_yfor, u_fact, u_in, u_xx_entered_concepts_xx],
		    [u_yuntil, u_result],
		    [u_ydo, [setq, u_result, [u_run_fact_serendipity, u_fact]]]
		  ],
		  Yloop_Ret),
	set_var(LEnv, setq, u_xx_entered_concepts_xx, []),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_entered_concept_serendipity, classof, claz_function),
   set_opv(u_entered_concept_serendipity, compile_as, kw_function),
   set_opv(u_entered_concept_serendipity,
	   function,
	   f_u_entered_concept_serendipity),
   DefunResult=u_entered_concept_serendipity.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:565 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 899)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:565 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Run serendipities for a fact.", 1, 901)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:565 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 933)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:934 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-fact-serendipity',
			    [fact],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("Running fact serendipity for ~A"),
			      fact
			    ],
			    
			    [ 'run-serendipity',
			      ['bottom-rules', fact],
			      
			      [ 'ob$fcreate',
				['#BQ', ['SUCCEEDED-GOAL', obj, ['#COMMA', fact]]]
			      ]
			    ]
			  ]).

% annotating U::RUN-FACT-SERENDIPITY 
wl: lambda_def(defun,
	      u_run_fact_serendipity,
	      f_u_run_fact_serendipity,
	      [u_fact],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
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
			     #\(f),
			     #\(a),
			     #\(c),
			     #\(t),
			     #\(' '),
			     #\(s),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(p),
			     #\(i),
			     #\(t),
			     #\(y),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_fact
		],
		
		[ u_run_serendipity,
		  [u_bottom_rules, u_fact],
		  
		  [ u_ob_c36_fcreate,
		    ['#BQ', [u_succeeded_goal, u_obj, ['#COMMA', u_fact]]]
		  ]
		]
	      ]).


% annotating U::RUN-FACT-SERENDIPITY 
wl: arglist_info(u_run_fact_serendipity,
		[u_fact],
		[Fact_Param],
		arginfo{ all:[u_fact],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fact],
			 opt:0,
			 req:[u_fact],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-FACT-SERENDIPITY 
wl: init_args(exact_only, u_run_fact_serendipity).


% annotating U::RUN-FACT-SERENDIPITY 
f_u_run_fact_serendipity(Fact_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
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
				       #\(f),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(' '),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(p),
				       #\(i),
				       #\(t),
				       #\(y),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_fact
			  ],
			  Roman_nl_Ret),
	f_u_bottom_rules(Fact_Param, Run_serendipity_Param),
	f_u_ob_c36_fcreate(['#BQ', [u_succeeded_goal, u_obj, ['#COMMA', u_fact]]],
			   C36_fcreate_Ret),
	f_u_run_serendipity(Run_serendipity_Param,
			    C36_fcreate_Ret,
			    Run_serendipity_Ret),
	Run_serendipity_Ret=FnResult.
:- set_opv(f_u_run_fact_serendipity, classof, claz_function),
   set_opv(u_run_fact_serendipity, compile_as, kw_function),
   set_opv(u_run_fact_serendipity, function, f_u_run_fact_serendipity),
   DefunResult=u_run_fact_serendipity.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:934 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1151)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:934 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Run serendipities for all recent episodes. (However, are not",
				     1,
				     1153)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:934 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" serendipities checked upon episode retrieval? Why would we need",
				     1,
				     1216)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:934 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" this batch-mode function?)", 1, 1282)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:934 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1311)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1312 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-serendipities',
			    [],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("Run serendipities")
			    ],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, episode, in, '*recent-episodes*'],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  
				  [ 't-list-append!',
				    result,
				    
				    [ 'run-serendipity',
				      ['inaccessible-planning-rules', episode],
				      []
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::RUN-SERENDIPITIES 
wl: lambda_def(defun,
	      u_run_serendipities,
	      f_u_run_serendipities,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(n),
			     #\(' '),
			     #\(s),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(p),
			     #\(i),
			     #\(t),
			     #\(i),
			     #\(e),
			     #\(s)
			   ])
		],
		
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_episode, u_in, u_xx_recent_episodes_xx],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      
		      [ u_t_list_append_c33,
			u_result,
			
			[ u_run_serendipity,
			  [u_inaccessible_planning_rules, u_episode],
			  []
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::RUN-SERENDIPITIES 
wl: arglist_info(u_run_serendipities,
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

% annotating U::RUN-SERENDIPITIES 
wl: init_args(exact_only, u_run_serendipities).


% annotating U::RUN-SERENDIPITIES 
f_u_run_serendipities(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
				       #\(' '),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(p),
				       #\(i),
				       #\(t),
				       #\(i),
				       #\(e),
				       #\(s)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_episode, u_in, u_xx_recent_episodes_xx],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			
			[ u_t_list_append_c33,
			  u_result,
			  
			  [ u_run_serendipity,
			    [u_inaccessible_planning_rules, u_episode],
			    []
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_run_serendipities, classof, claz_function),
   set_opv(u_run_serendipities, compile_as, kw_function),
   set_opv(u_run_serendipities, function, f_u_run_serendipities),
   DefunResult=u_run_serendipities.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1312 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: add invocation of this function in activate-top-level-goal.",
				     1,
				     1628)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1312 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1696)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1312 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note that this works for personal goals but not for daydreaming goals",
				     1,
				     1698)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1312 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" since a subgoal has not yet been activated.",
				     1,
				     1770)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'upon-activate-top-level-goal',
			    ['top-level-goal'],
			    
			    [ yloop,
			      [yfor, episode, in, '*recent-episodes*'],
			      
			      [ ydo,
				
				[ 'run-top-level-goal-serendipity',
				  'top-level-goal',
				  ['inaccessible-planning-rules', episode],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::UPON-ACTIVATE-TOP-LEVEL-GOAL 
wl: lambda_def(defun,
	      u_upon_activate_top_level_goal,
	      f_u_upon_activate_top_level_goal,
	      [u_top_level_goal],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_episode, u_in, u_xx_recent_episodes_xx],
		  
		  [ u_ydo,
		    
		    [ u_run_top_level_goal_serendipity,
		      u_top_level_goal,
		      [u_inaccessible_planning_rules, u_episode],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::UPON-ACTIVATE-TOP-LEVEL-GOAL 
wl: arglist_info(u_upon_activate_top_level_goal,
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

% annotating U::UPON-ACTIVATE-TOP-LEVEL-GOAL 
wl: init_args(exact_only, u_upon_activate_top_level_goal).


% annotating U::UPON-ACTIVATE-TOP-LEVEL-GOAL 
f_u_upon_activate_top_level_goal(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_yloop(
		  [ [u_yfor, u_episode, u_in, u_xx_recent_episodes_xx],
		    
		    [ u_ydo,
		      
		      [ u_run_top_level_goal_serendipity,
			u_top_level_goal,
			[u_inaccessible_planning_rules, u_episode],
			[]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_upon_activate_top_level_goal, classof, claz_function),
   set_opv(u_upon_activate_top_level_goal, compile_as, kw_function),
   set_opv(u_upon_activate_top_level_goal,
	   function,
	   f_u_upon_activate_top_level_goal),
   DefunResult=u_upon_activate_top_level_goal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2097)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Run serendipities for a set of bottom rules and a given bottom goal.",
				     1,
				     2099)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (If bottom-goal is non-NIL, then bottom-rules must correspond",
				     1,
				     2170)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("  to this goal.)", 1, 2234)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2252)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below is also called when a new episodic planning rule is induced.",
				     1,
				     2254)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2327)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: bottom-rules is actually list of (<bottom-rule> . <subgoalnum>),",
				     1,
				     2329)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" where <subgoalnum> may be nil.", 1, 2402)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:1815 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2435)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-serendipity',
			    ['bottom-rules', 'bottom-goal'],
			    
			    [ if,
			      'bottom-rules',
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  'seren-long',
				  '$STRING'("Run serendipity for ~A and ~A"),
				  'bottom-rules',
				  'bottom-goal'
				],
				
				[ yloop,
				  [initial, [result, []]],
				  
				  [ yfor,
				    'top-level-goal',
				    in,
				    '*top-level-goals*'
				  ],
				  
				  [ ydo,
				    
				    [ setq,
				      result,
				      
				      [ 't-list-append!',
					result,
					
					[ 'run-top-level-goal-serendipity',
					  'top-level-goal',
					  'bottom-rules',
					  'bottom-goal'
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

% annotating U::RUN-SERENDIPITY 
wl: lambda_def(defun,
	      u_run_serendipity,
	      f_u_run_serendipity,
	      [u_bottom_rules, u_bottom_goal],
	      
	      [ 
		[ if,
		  u_bottom_rules,
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_seren_long,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('R'),
				 #\(u),
				 #\(n),
				 #\(' '),
				 #\(s),
				 #\(e),
				 #\(r),
				 #\(e),
				 #\(n),
				 #\(d),
				 #\(i),
				 #\(p),
				 #\(i),
				 #\(t),
				 #\(y),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(r),
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
		      u_bottom_rules,
		      u_bottom_goal
		    ],
		    
		    [ u_yloop,
		      [u_initial, [u_result, []]],
		      [u_yfor, u_top_level_goal, u_in, u_xx_top_level_goals_xx],
		      
		      [ u_ydo,
			
			[ setq,
			  u_result,
			  
			  [ u_t_list_append_c33,
			    u_result,
			    
			    [ u_run_top_level_goal_serendipity,
			      u_top_level_goal,
			      u_bottom_rules,
			      u_bottom_goal
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


% annotating U::RUN-SERENDIPITY 
wl: arglist_info(u_run_serendipity,
		[u_bottom_rules, u_bottom_goal],
		[Bottom_rules_Param, Bottom_goal_Param],
		arginfo{ all:[u_bottom_rules, u_bottom_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bottom_rules, u_bottom_goal],
			 opt:0,
			 req:[u_bottom_rules, u_bottom_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-SERENDIPITY 
wl: init_args(exact_only, u_run_serendipity).


% annotating U::RUN-SERENDIPITY 
f_u_run_serendipity(Bottom_rules_Param, Bottom_goal_Param, FnResult) :-
	Env=[bv(u_bottom_rules, Bottom_rules_Param), bv(u_bottom_goal, Bottom_goal_Param)],
	(   Bottom_rules_Param\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_seren_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('R'),
					   #\(u),
					   #\(n),
					   #\(' '),
					   #\(s),
					   #\(e),
					   #\(r),
					   #\(e),
					   #\(n),
					   #\(d),
					   #\(i),
					   #\(p),
					   #\(i),
					   #\(t),
					   #\(y),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(r),
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
				u_bottom_rules,
				u_bottom_goal
			      ],
			      Roman_nl_Ret),
	    f_u_yloop(
		      [ [u_initial, [u_result, []]],
			[u_yfor, u_top_level_goal, u_in, u_xx_top_level_goals_xx],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ u_t_list_append_c33,
			      u_result,
			      
			      [ u_run_top_level_goal_serendipity,
				u_top_level_goal,
				u_bottom_rules,
				u_bottom_goal
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
:- set_opv(f_u_run_serendipity, classof, claz_function),
   set_opv(u_run_serendipity, compile_as, kw_function),
   set_opv(u_run_serendipity, function, f_u_run_serendipity),
   DefunResult=u_run_serendipity.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2932)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Run serendipities for a set of bottom rules, bottom goal, and a given",
				     1,
				     2934)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" top-level goal.", 1, 3006)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3024)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: bottom-rules is actually list of (<bottom-rule> . <subgoalnum>),",
				     1,
				     3026)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" where <subgoalnum> may be nil.", 1, 3099)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3132)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-top-level-goal-serendipity',
			    ['top-level-goal', 'bottom-rules', 'bottom-goal'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("Run serendipity for top-level goal ~A"),
			      'top-level-goal'
			    ],
			    
			    [ let,
			      [[subgoal, []]],
			      
			      [ cond,
				
				[ 
				  [ and,
				    ['dd-goal?', 'top-level-goal'],
				    
				    [ setq,
				      subgoal,
				      ['dd-goal-subgoal', 'top-level-goal']
				    ]
				  ],
				  
				  [ 'serendipity-recognize-apply',
				    'top-level-goal',
				    subgoal,
				    'bottom-rules',
				    'bottom-goal'
				  ]
				],
				
				[ 
				  [ and,
				    ['personal-goal?', 'top-level-goal'],
				    
				    [ 'vars-in?',
				      ['ob$get', 'top-level-goal', [quote, obj]]
				    ],
				    
				    [ 'eq?',
				      [quote, halted],
				      
				      [ 'ob$get',
					'top-level-goal',
					[quote, status]
				      ]
				    ],
				    
				    [ 'null?',
				      
				      [ 'ob$gets',
					'top-level-goal',
					[quote, 'analogical-episode']
				      ]
				    ]
				  ],
				  
				  [ 'serendipity-recognize-apply',
				    'top-level-goal',
				    [],
				    'bottom-rules',
				    'bottom-goal'
				  ]
				],
				[else, []]
			      ]
			    ]
			  ]).

% annotating U::RUN-TOP-LEVEL-GOAL-SERENDIPITY 
wl: lambda_def(defun,
	      u_run_top_level_goal_serendipity,
	      f_u_run_top_level_goal_serendipity,
	      [u_top_level_goal, u_bottom_rules, u_bottom_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(n),
			     #\(' '),
			     #\(s),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(p),
			     #\(i),
			     #\(t),
			     #\(y),
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
			     #\('A')
			   ]),
		  u_top_level_goal
		],
		
		[ let,
		  [[u_subgoal, []]],
		  
		  [ cond,
		    
		    [ 
		      [ and,
			[u_dd_goal_c63, u_top_level_goal],
			[setq, u_subgoal, [u_dd_goal_subgoal, u_top_level_goal]]
		      ],
		      
		      [ u_serendipity_recognize_apply,
			u_top_level_goal,
			u_subgoal,
			u_bottom_rules,
			u_bottom_goal
		      ]
		    ],
		    
		    [ 
		      [ and,
			[u_personal_goal_c63, u_top_level_goal],
			
			[ u_vars_in_c63,
			  [u_ob_c36_get, u_top_level_goal, [quote, u_obj]]
			],
			
			[ u_eq_c63,
			  [quote, u_halted],
			  [u_ob_c36_get, u_top_level_goal, [quote, u_status]]
			],
			
			[ u_null_c63,
			  
			  [ u_ob_c36_gets,
			    u_top_level_goal,
			    [quote, u_analogical_episode]
			  ]
			]
		      ],
		      
		      [ u_serendipity_recognize_apply,
			u_top_level_goal,
			[],
			u_bottom_rules,
			u_bottom_goal
		      ]
		    ],
		    [u_else, []]
		  ]
		]
	      ]).


% annotating U::RUN-TOP-LEVEL-GOAL-SERENDIPITY 
wl: arglist_info(u_run_top_level_goal_serendipity,
		[u_top_level_goal, u_bottom_rules, u_bottom_goal],
		[Top_level_goal_Param, Bottom_rules_Param, Bottom_goal_Param],
		arginfo{ all:[u_top_level_goal, u_bottom_rules, u_bottom_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_goal, u_bottom_rules, u_bottom_goal],
			 opt:0,
			 req:[u_top_level_goal, u_bottom_rules, u_bottom_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-TOP-LEVEL-GOAL-SERENDIPITY 
wl: init_args(exact_only, u_run_top_level_goal_serendipity).


% annotating U::RUN-TOP-LEVEL-GOAL-SERENDIPITY 
f_u_run_top_level_goal_serendipity(Top_level_goal_Param, Bottom_rules_Param, Bottom_goal_Param, ElseResult51) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_bottom_rules, Bottom_rules_Param), bv(u_bottom_goal, Bottom_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(n),
				       #\(' '),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(p),
				       #\(i),
				       #\(t),
				       #\(y),
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
				       #\('A')
				     ]),
			    u_top_level_goal
			  ],
			  Roman_nl_Ret),
	LEnv=[[bv(u_subgoal, [])]|Env],
	f_u_dd_goal_c63(Top_level_goal_Param, IFTEST20),
	(   IFTEST20\==[]
	->  f_u_dd_goal_subgoal(Top_level_goal_Param, TrueResult),
	    set_var(LEnv, u_subgoal, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_subgoal, Subgoal_Get),
	    f_u_serendipity_recognize_apply(Top_level_goal_Param,
					    Subgoal_Get,
					    Bottom_rules_Param,
					    Bottom_goal_Param,
					    TrueResult52),
	    ElseResult51=TrueResult52
	;   f_u_personal_goal_c63(Top_level_goal_Param, IFTEST32),
	    (   IFTEST32\==[]
	    ->  f_u_ob_c36_get(Top_level_goal_Param, u_obj, Obj),
		f_u_vars_in_c63(Obj, IFTEST35),
		(   IFTEST35\==[]
		->  f_u_eq_c63([quote, u_halted],
			       
			       [ u_ob_c36_get,
				 u_top_level_goal,
				 [quote, u_status]
			       ],
			       IFTEST38),
		    (   IFTEST38\==[]
		    ->  f_u_null_c63(
				     [ u_ob_c36_gets,
				       u_top_level_goal,
				       [quote, u_analogical_episode]
				     ],
				     TrueResult40),
			IFTEST30=TrueResult40
		    ;   IFTEST30=[]
		    )
		;   IFTEST30=[]
		)
	    ;   IFTEST30=[]
	    ),
	    (   IFTEST30\==[]
	    ->  f_u_serendipity_recognize_apply(Top_level_goal_Param,
						[],
						Bottom_rules_Param,
						Bottom_goal_Param,
						TrueResult50),
		ElseResult51=TrueResult50
	    ;   get_var(LEnv, u_else, IFTEST46),
		(   IFTEST46\==[]
		->  ElseResult51=[]
		;   ElseResult51=[]
		)
	    )
	).
:- set_opv(f_u_run_top_level_goal_serendipity, classof, claz_function),
   set_opv(u_run_top_level_goal_serendipity, compile_as, kw_function),
   set_opv(u_run_top_level_goal_serendipity,
	   function,
	   f_u_run_top_level_goal_serendipity),
   DefunResult=u_run_top_level_goal_serendipity.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The above is needed in particular to prevent running serendipities",
				     9,
				     3709)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" for \"He gives me a newspaper\" on tlg EMPLOYMENT.",
				     9,
				     3786)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4026)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Recognize and apply serendipities",
				     1,
				     4028)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4064)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns NIL or (T <ep> <ep> ...) where <ep> is an episode retrieved",
				     1,
				     4066)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" in the process.", 1, 4136)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4154)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If subgoal is non-NIL, then top-level-goal must be a daydreaming goal.",
				     1,
				     4156)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4229)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If bottom-goal is non-NIL, then it is assumed that bottom-rules all apply",
				     1,
				     4231)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to this goal. Otherwise, bottom-rules are unrelated to any particular",
				     1,
				     4307)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" bottom goal.", 1, 4379)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4394)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: bottom-rules is actually list of (<bottom-rule> . <subgoalnum>),",
				     1,
				     4396)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" where <subgoalnum> may be nil.", 1, 4469)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:3133 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4502)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:4503 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'serendipity-recognize-apply',
			    
			    [ 'top-level-goal',
			      subgoal,
			      'bottom-rules',
			      'bottom-goal'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("Recognize and apply serendipity")
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				['episode-spec', []],
				[result, []],
				['ri-paths', []]
			      ],
			      
			      [ yfor,
				'top-rule',
				in,
				
				[ 'top-rules',
				  
				  [ 'ob$get',
				    [or, subgoal, 'top-level-goal'],
				    [quote, obj]
				  ]
				]
			      ],
			      
			      [ ydo,
				[setq, 'ri-paths', []],
				
				[ yloop,
				  [yfor, 'bottom-rule', in, 'bottom-rules'],
				  
				  [ ydo,
				    
				    [ setq,
				      'ri-paths',
				      
				      [ 'append!',
					'ri-paths',
					
					[ 'rule-intersection',
					  'top-rule',
					  'bottom-rule',
					  '*recent-episodes*'
					]
				      ]
				    ]
				  ],
				  
				  [ yresult,
				    
				    [ if,
				      
				      [ and,
					
					[ setq,
					  'ri-paths',
					  ['condense-paths', 'ri-paths']
					],
					
					[ setq,
					  'episode-spec',
					  
					  [ 'rip-paths->episode',
					    'ri-paths',
					    [or, subgoal, 'top-level-goal'],
					    'bottom-goal'
					  ]
					]
				      ],
				      
				      [ progn,
					
					[ if,
					  t,
					  
					  [ setq,
					    result,
					    
					    [ cons,
					      [quote, t],
					      [car, 'episode-spec']
					    ]
					  ],
					  
					  [ setq,
					    result,
					    
					    [ cons,
					      [quote, t],
					      
					      [ 'append!',
						[cdr, result],
						[car, 'episode-spec']
					      ]
					    ]
					  ]
					],
					
					[ if,
					  subgoal,
					  
					  [ 'new-analogical-dd-goal-plan',
					    'top-level-goal',
					    subgoal,
					    [cdr, 'episode-spec']
					  ],
					  
					  [ 'new-analogical-pers-goal-plan',
					    'top-level-goal',
					    [cdr, 'episode-spec']
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::SERENDIPITY-RECOGNIZE-APPLY 
wl: lambda_def(defun,
	      u_serendipity_recognize_apply,
	      f_u_serendipity_recognize_apply,
	      [u_top_level_goal, u_subgoal, u_bottom_rules, u_bottom_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(e),
			     #\(c),
			     #\(o),
			     #\(g),
			     #\(n),
			     #\(i),
			     #\(z),
			     #\(e),
			     #\(' '),
			     #\(a),
			     #\(n),
			     #\(d),
			     #\(' '),
			     #\(a),
			     #\(p),
			     #\(p),
			     #\(l),
			     #\(y),
			     #\(' '),
			     #\(s),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(p),
			     #\(i),
			     #\(t),
			     #\(y)
			   ])
		],
		
		[ u_yloop,
		  
		  [ u_initial,
		    [u_episode_spec, []],
		    [u_result, []],
		    [u_ri_paths, []]
		  ],
		  
		  [ u_yfor,
		    u_top_rule,
		    u_in,
		    
		    [ u_top_rules,
		      
		      [ u_ob_c36_get,
			[or, u_subgoal, u_top_level_goal],
			[quote, u_obj]
		      ]
		    ]
		  ],
		  
		  [ u_ydo,
		    [setq, u_ri_paths, []],
		    
		    [ u_yloop,
		      [u_yfor, u_bottom_rule, u_in, u_bottom_rules],
		      
		      [ u_ydo,
			
			[ setq,
			  u_ri_paths,
			  
			  [ u_append_c33,
			    u_ri_paths,
			    
			    [ u_rule_intersection,
			      u_top_rule,
			      u_bottom_rule,
			      u_xx_recent_episodes_xx
			    ]
			  ]
			]
		      ],
		      
		      [ u_yresult,
			
			[ if,
			  
			  [ and,
			    [setq, u_ri_paths, [u_condense_paths, u_ri_paths]],
			    
			    [ setq,
			      u_episode_spec,
			      
			      [ u_rip_paths_c62_episode,
				u_ri_paths,
				[or, u_subgoal, u_top_level_goal],
				u_bottom_goal
			      ]
			    ]
			  ],
			  
			  [ progn,
			    
			    [ if,
			      t,
			      
			      [ setq,
				u_result,
				[cons, [quote, t], [car, u_episode_spec]]
			      ],
			      
			      [ setq,
				u_result,
				
				[ cons,
				  [quote, t],
				  
				  [ u_append_c33,
				    [cdr, u_result],
				    [car, u_episode_spec]
				  ]
				]
			      ]
			    ],
			    
			    [ if,
			      u_subgoal,
			      
			      [ u_new_analogical_dd_goal_plan,
				u_top_level_goal,
				u_subgoal,
				[cdr, u_episode_spec]
			      ],
			      
			      [ u_new_analogical_pers_goal_plan,
				u_top_level_goal,
				[cdr, u_episode_spec]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::SERENDIPITY-RECOGNIZE-APPLY 
wl: arglist_info(u_serendipity_recognize_apply,
		[u_top_level_goal, u_subgoal, u_bottom_rules, u_bottom_goal],
		
		[ Top_level_goal_Param,
		  Subgoal_Param,
		  Bottom_rules_Param,
		  Bottom_goal_Param
		],
		arginfo{ all:
			     [ u_top_level_goal,
			       u_subgoal,
			       u_bottom_rules,
			       u_bottom_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_top_level_goal,
				 u_subgoal,
				 u_bottom_rules,
				 u_bottom_goal
			       ],
			 opt:0,
			 req:
			     [ u_top_level_goal,
			       u_subgoal,
			       u_bottom_rules,
			       u_bottom_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SERENDIPITY-RECOGNIZE-APPLY 
wl: init_args(exact_only, u_serendipity_recognize_apply).


% annotating U::SERENDIPITY-RECOGNIZE-APPLY 
f_u_serendipity_recognize_apply(Top_level_goal_Param, Subgoal_Param, Bottom_rules_Param, Bottom_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param), bv(u_subgoal, Subgoal_Param), bv(u_bottom_rules, Bottom_rules_Param), bv(u_bottom_goal, Bottom_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(c),
				       #\(o),
				       #\(g),
				       #\(n),
				       #\(i),
				       #\(z),
				       #\(e),
				       #\(' '),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(a),
				       #\(p),
				       #\(p),
				       #\(l),
				       #\(y),
				       #\(' '),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(p),
				       #\(i),
				       #\(t),
				       #\(y)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_episode_spec, []],
		      [u_result, []],
		      [u_ri_paths, []]
		    ],
		    
		    [ u_yfor,
		      u_top_rule,
		      u_in,
		      
		      [ u_top_rules,
			
			[ u_ob_c36_get,
			  [or, u_subgoal, u_top_level_goal],
			  [quote, u_obj]
			]
		      ]
		    ],
		    
		    [ u_ydo,
		      [setq, u_ri_paths, []],
		      
		      [ u_yloop,
			[u_yfor, u_bottom_rule, u_in, u_bottom_rules],
			
			[ u_ydo,
			  
			  [ setq,
			    u_ri_paths,
			    
			    [ u_append_c33,
			      u_ri_paths,
			      
			      [ u_rule_intersection,
				u_top_rule,
				u_bottom_rule,
				u_xx_recent_episodes_xx
			      ]
			    ]
			  ]
			],
			
			[ u_yresult,
			  
			  [ if,
			    
			    [ and,
			      [setq, u_ri_paths, [u_condense_paths, u_ri_paths]],
			      
			      [ setq,
				u_episode_spec,
				
				[ u_rip_paths_c62_episode,
				  u_ri_paths,
				  [or, u_subgoal, u_top_level_goal],
				  u_bottom_goal
				]
			      ]
			    ],
			    
			    [ progn,
			      
			      [ if,
				t,
				
				[ setq,
				  u_result,
				  [cons, [quote, t], [car, u_episode_spec]]
				],
				
				[ setq,
				  u_result,
				  
				  [ cons,
				    [quote, t],
				    
				    [ u_append_c33,
				      [cdr, u_result],
				      [car, u_episode_spec]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				u_subgoal,
				
				[ u_new_analogical_dd_goal_plan,
				  u_top_level_goal,
				  u_subgoal,
				  [cdr, u_episode_spec]
				],
				
				[ u_new_analogical_pers_goal_plan,
				  u_top_level_goal,
				  [cdr, u_episode_spec]
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_serendipity_recognize_apply, classof, claz_function),
   set_opv(u_serendipity_recognize_apply, compile_as, kw_function),
   set_opv(u_serendipity_recognize_apply,
	   function,
	   f_u_serendipity_recognize_apply),
   DefunResult=u_serendipity_recognize_apply.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:4503 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" We do a bit of batching here (so that we can use condense-paths).",
				     4,
				     4853)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:4503 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (null? result)", 17, 5442)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:5900 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'condense-paths',
			    ['ri-paths'],
			    
			    [ if,
			      '*action-mutations?*',
			      'ri-paths',
			      
			      [ 'remove-sublist-duplicates',
				'ri-paths',
				[lambda, [x], ['ri-pathelt-rule', x]]
			      ]
			    ]
			  ]).

% annotating U::CONDENSE-PATHS 
wl: lambda_def(defun,
	      u_condense_paths,
	      f_u_condense_paths,
	      [u_ri_paths],
	      
	      [ 
		[ if,
		  u_xx_action_mutations_c63_xx,
		  u_ri_paths,
		  
		  [ u_remove_sublist_duplicates,
		    u_ri_paths,
		    [lambda, [u_x], [u_ri_pathelt_rule, u_x]]
		  ]
		]
	      ]).


% annotating U::CONDENSE-PATHS 
wl: arglist_info(u_condense_paths,
		[u_ri_paths],
		[Ri_paths_Param],
		arginfo{ all:[u_ri_paths],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ri_paths],
			 opt:0,
			 req:[u_ri_paths],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CONDENSE-PATHS 
wl: init_args(exact_only, u_condense_paths).


% annotating U::CONDENSE-PATHS 
f_u_condense_paths(Ri_paths_Param, FnResult) :-
	Env=[bv(u_ri_paths, Ri_paths_Param)],
	get_var(Env, u_xx_action_mutations_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  FnResult=Ri_paths_Param
	;   Lambda=closure([ClosureEnvironment|Env], LResult, [u_x], f_u_ri_pathelt_rule(u_x, LResult)),
	    f_u_remove_sublist_duplicates(Ri_paths_Param, Lambda, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_condense_paths, classof, claz_function),
   set_opv(u_condense_paths, compile_as, kw_function),
   set_opv(u_condense_paths, function, f_u_condense_paths),
   DefunResult=u_condense_paths.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:6053 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'remove-sublist-duplicates',
			    [lsts, 'elem-accessor'],
			    
			    [ cond,
			      [['null?', lsts], lsts],
			      [['null?', [cdr, lsts]], lsts],
			      
			      [ else,
				
				[ let,
				  
				  [ 
				    [ rest,
				      
				      [ 'remove-sublist-duplicates',
					[cdr, lsts],
					'elem-accessor'
				      ]
				    ]
				  ],
				  
				  [ if,
				    
				    [ 'any?',
				      
				      [ lambda,
					[x],
					
					[ 'sublist?',
					  [car, lsts],
					  x,
					  'elem-accessor'
					]
				      ],
				      rest
				    ],
				    rest,
				    
				    [ yloop,
				      [yfor, relem, in, rest],
				      
				      [ ydo,
					
					[ if,
					  
					  [ 'sublist?',
					    relem,
					    [car, lsts],
					    'elem-accessor'
					  ],
					  [setq, rest, ['delq!', relem, rest]]
					]
				      ],
				      [yresult, [cons, [car, lsts], rest]]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::REMOVE-SUBLIST-DUPLICATES 
wl: lambda_def(defun,
	      u_remove_sublist_duplicates,
	      f_u_remove_sublist_duplicates,
	      [u_lsts, u_elem_accessor],
	      
	      [ 
		[ cond,
		  [[u_null_c63, u_lsts], u_lsts],
		  [[u_null_c63, [cdr, u_lsts]], u_lsts],
		  
		  [ u_else,
		    
		    [ let,
		      
		      [ 
			[ rest,
			  
			  [ u_remove_sublist_duplicates,
			    [cdr, u_lsts],
			    u_elem_accessor
			  ]
			]
		      ],
		      
		      [ if,
			
			[ u_any_c63,
			  
			  [ lambda,
			    [u_x],
			    [u_sublist_c63, [car, u_lsts], u_x, u_elem_accessor]
			  ],
			  rest
			],
			rest,
			
			[ u_yloop,
			  [u_yfor, u_relem, u_in, rest],
			  
			  [ u_ydo,
			    
			    [ if,
			      
			      [ u_sublist_c63,
				u_relem,
				[car, u_lsts],
				u_elem_accessor
			      ],
			      [setq, rest, [u_delq_c33, u_relem, rest]]
			    ]
			  ],
			  [u_yresult, [cons, [car, u_lsts], rest]]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::REMOVE-SUBLIST-DUPLICATES 
wl: arglist_info(u_remove_sublist_duplicates,
		[u_lsts, u_elem_accessor],
		[Lsts_Get25, Elem_accessor_Param],
		arginfo{ all:[u_lsts, u_elem_accessor],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lsts, u_elem_accessor],
			 opt:0,
			 req:[u_lsts, u_elem_accessor],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::REMOVE-SUBLIST-DUPLICATES 
wl: init_args(exact_only, u_remove_sublist_duplicates).


% annotating U::REMOVE-SUBLIST-DUPLICATES 
f_u_remove_sublist_duplicates(Lsts_Get25, Elem_accessor_Param, ElseResult37) :-
	Env=[bv(u_lsts, Lsts_Get25), bv(u_elem_accessor, Elem_accessor_Param)],
	f_u_null_c63(u_lsts, IFTEST),
	(   IFTEST\==[]
	->  ElseResult37=Lsts_Get25
	;   f_u_null_c63([cdr, u_lsts], IFTEST17),
	    (   IFTEST17\==[]
	    ->  ElseResult37=Lsts_Get25
	    ;   get_var(Env, u_else, IFTEST20),
		(   IFTEST20\==[]
		->  cl_cdr(Lsts_Get25, Sublist_duplicates_Param),
		    f_u_remove_sublist_duplicates(Sublist_duplicates_Param,
						  Elem_accessor_Param,
						  Rest_Init),
		    LEnv=[[bv(rest, Rest_Init)]|Env],
		    f_u_any_c63(
				[ lambda,
				  [u_x],
				  
				  [ u_sublist_c63,
				    [car, u_lsts],
				    u_x,
				    u_elem_accessor
				  ]
				],
				rest,
				IFTEST28),
		    (   IFTEST28\==[]
		    ->  get_var(LEnv, rest, Rest_Get),
			ElseResult37=Rest_Get
		    ;   f_u_yloop(
				  [ [u_yfor, u_relem, u_in, rest],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ u_sublist_c63,
					  u_relem,
					  [car, u_lsts],
					  u_elem_accessor
					],
					[setq, rest, [u_delq_c33, u_relem, rest]]
				      ]
				    ],
				    [u_yresult, [cons, [car, u_lsts], rest]]
				  ],
				  ElseResult),
			ElseResult37=ElseResult
		    )
		;   ElseResult37=[]
		)
	    )
	).
:- set_opv(f_u_remove_sublist_duplicates, classof, claz_function),
   set_opv(u_remove_sublist_duplicates, compile_as, kw_function),
   set_opv(u_remove_sublist_duplicates, function, f_u_remove_sublist_duplicates),
   DefunResult=u_remove_sublist_duplicates.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:6590 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'sublist?',
			    [lst1, lst2, 'elem-accessor'],
			    
			    [ yloop,
			      [initial, [result, t], [lst2x, lst2]],
			      [yfor, elem1, in, lst1],
			      [ywhile, result],
			      
			      [ ydo,
				
				[ if,
				  
				  [ 'neq?',
				    [apply, 'elem-accessor', [list, elem1]],
				    [apply, 'elem-accessor', [list, [car, lst2x]]]
				  ],
				  [setq, result, []]
				],
				[setq, lst2x, [cdr, lst2x]]
			      ],
			      
			      [ yresult,
				[and, result, [<=, [length, lst1], [length, lst2]]]
			      ]
			    ]
			  ]).

% annotating U::SUBLIST? 
wl: lambda_def(defun,
	      u_sublist_c63,
	      f_u_sublist_c63,
	      [u_lst1, u_lst2, u_elem_accessor],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, t], [u_lst2x, u_lst2]],
		  [u_yfor, u_elem1, u_in, u_lst1],
		  [u_ywhile, u_result],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ u_neq_c63,
			[apply, u_elem_accessor, [list, u_elem1]],
			[apply, u_elem_accessor, [list, [car, u_lst2x]]]
		      ],
		      [setq, u_result, []]
		    ],
		    [setq, u_lst2x, [cdr, u_lst2x]]
		  ],
		  
		  [ u_yresult,
		    [and, u_result, [<=, [length, u_lst1], [length, u_lst2]]]
		  ]
		]
	      ]).


% annotating U::SUBLIST? 
wl: arglist_info(u_sublist_c63,
		[u_lst1, u_lst2, u_elem_accessor],
		[Lst1_Param, Lst2_Param, Elem_accessor_Param],
		arginfo{ all:[u_lst1, u_lst2, u_elem_accessor],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst1, u_lst2, u_elem_accessor],
			 opt:0,
			 req:[u_lst1, u_lst2, u_elem_accessor],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBLIST? 
wl: init_args(exact_only, u_sublist_c63).


% annotating U::SUBLIST? 
f_u_sublist_c63(Lst1_Param, Lst2_Param, Elem_accessor_Param, FnResult) :-
	Env=[bv(u_lst1, Lst1_Param), bv(u_lst2, Lst2_Param), bv(u_elem_accessor, Elem_accessor_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, t], [u_lst2x, u_lst2]],
		    [u_yfor, u_elem1, u_in, u_lst1],
		    [u_ywhile, u_result],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ u_neq_c63,
			  [apply, u_elem_accessor, [list, u_elem1]],
			  [apply, u_elem_accessor, [list, [car, u_lst2x]]]
			],
			[setq, u_result, []]
		      ],
		      [setq, u_lst2x, [cdr, u_lst2x]]
		    ],
		    
		    [ u_yresult,
		      [and, u_result, [<=, [length, u_lst1], [length, u_lst2]]]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_sublist_c63, classof, claz_function),
   set_opv(u_sublist_c63, compile_as, kw_function),
   set_opv(u_sublist_c63, function, f_u_sublist_c63),
   DefunResult=u_sublist_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:6590 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6960)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:6590 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Create a new plan (by analogy) for a personal goal",
				     1,
				     6962)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:6590 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (Clobbers existing plans!)", 1, 7015)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:6590 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7044)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'new-analogical-pers-goal-plan',
			    ['top-level-pers-goal', 'analogical-episode'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Serendipity for ~A personal goal"),
			      ['tlg->string', 'top-level-pers-goal']
			    ],
			    ['ep-print', 'analogical-episode'],
			    ['reality-stabilize'],
			    [setq, '*reality*', ['cx$sprout', '*reality*']],
			    [setq, '*reality-lookahead*', '*reality*'],
			    
			    [ 'gc-plans1',
			      '*reality*',
			      [list, 'top-level-pers-goal']
			    ],
			    
			    [ 'ob$set',
			      'top-level-pers-goal',
			      [quote, 'analogical-episode'],
			      'analogical-episode'
			    ],
			    [surprise, 'top-level-pers-goal']
			  ]).

% annotating U::NEW-ANALOGICAL-PERS-GOAL-PLAN 
wl: lambda_def(defun,
	      u_new_analogical_pers_goal_plan,
	      f_u_new_analogical_pers_goal_plan,
	      [u_top_level_pers_goal, u_analogical_episode],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('S'),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(p),
			     #\(i),
			     #\(t),
			     #\(y),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(p),
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
			   ]),
		  [u_tlg_c62_string, u_top_level_pers_goal]
		],
		[u_ep_print, u_analogical_episode],
		[u_reality_stabilize],
		[setq, u_xx_reality_xx, [u_cx_c36_sprout, u_xx_reality_xx]],
		[setq, u_xx_reality_lookahead_xx, u_xx_reality_xx],
		[u_gc_plans1, u_xx_reality_xx, [list, u_top_level_pers_goal]],
		
		[ u_ob_c36_set,
		  u_top_level_pers_goal,
		  [quote, u_analogical_episode],
		  u_analogical_episode
		],
		[u_surprise, u_top_level_pers_goal]
	      ]).


% annotating U::NEW-ANALOGICAL-PERS-GOAL-PLAN 
wl: arglist_info(u_new_analogical_pers_goal_plan,
		[u_top_level_pers_goal, u_analogical_episode],
		[Top_level_pers_goal_Param, Analogical_episode_Param],
		arginfo{ all:[u_top_level_pers_goal, u_analogical_episode],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_level_pers_goal, u_analogical_episode],
			 opt:0,
			 req:[u_top_level_pers_goal, u_analogical_episode],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NEW-ANALOGICAL-PERS-GOAL-PLAN 
wl: init_args(exact_only, u_new_analogical_pers_goal_plan).


% annotating U::NEW-ANALOGICAL-PERS-GOAL-PLAN 
f_u_new_analogical_pers_goal_plan(Top_level_pers_goal_Param, Analogical_episode_Param, FnResult) :-
	Env=[bv(u_top_level_pers_goal, Top_level_pers_goal_Param), bv(u_analogical_episode, Analogical_episode_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(p),
				       #\(i),
				       #\(t),
				       #\(y),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(p),
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
				     ]),
			    [u_tlg_c62_string, u_top_level_pers_goal]
			  ],
			  Roman_nl_Ret),
	f_u_ep_print(Analogical_episode_Param, Ep_print_Ret),
	f_u_reality_stabilize(Reality_stabilize_Ret),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get16),
	f_u_cx_c36_sprout(Xx_reality_xx_Get16, Xx_reality_xx),
	set_var(Env, u_xx_reality_xx, Xx_reality_xx),
	set_var(Env, u_xx_reality_lookahead_xx, Xx_reality_xx_Get16),
	_91648=[Top_level_pers_goal_Param],
	f_u_gc_plans1(Xx_reality_xx_Get16, _91648, Gc_plans1_Ret),
	f_u_ob_c36_set(Top_level_pers_goal_Param,
		       u_analogical_episode,
		       Analogical_episode_Param,
		       C36_set_Ret),
	f_u_surprise(Top_level_pers_goal_Param, Surprise_Ret),
	Surprise_Ret=FnResult.
:- set_opv(f_u_new_analogical_pers_goal_plan, classof, claz_function),
   set_opv(u_new_analogical_pers_goal_plan, compile_as, kw_function),
   set_opv(u_new_analogical_pers_goal_plan,
	   function,
	   f_u_new_analogical_pers_goal_plan),
   DefunResult=u_new_analogical_pers_goal_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Create new context", 3, 7300)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Clobber existing planning for this top-level goal.",
				     3,
				     7403)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Set up a new analogical plan for the top-level goal.",
				     3,
				     7509)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Give the top-level goal some more emotional charge.",
				     3,
				     7636)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7725)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Create a new plan (by analogy) for a daydreaming goal",
				     1,
				     7727)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (Leaves existing plans around as other possibilities.)",
				     1,
				     7783)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7045 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7840)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7841 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'new-analogical-dd-goal-plan',
			    ['top-level-dd-goal', subgoal, 'analogical-episode'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Serendipity for ~A daydreaming goal"),
			      ['tlg->string', 'top-level-dd-goal']
			    ],
			    ['ep-print', 'analogical-episode'],
			    
			    [ let,
			      
			      [ 
				[ 'new-context',
				  
				  [ 'cx$sprout',
				    
				    [ 'ob$get',
				      subgoal,
				      [quote, 'activation-context']
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				'*action-mutations?*',
				
				[ 'cx$assert',
				  'new-context',
				  ['ob$fcreate', [quote, ['MUTATION']]]
				]
			      ],
			      
			      [ setq,
				subgoal,
				
				[ 'replace-linked-ob',
				  subgoal,
				  'new-context',
				  '*me-belief-path*',
				  '*empty-bd*'
				]
			      ],
			      
			      [ 'ob$set',
				subgoal,
				[quote, 'analogical-episode'],
				'analogical-episode'
			      ],
			      
			      [ 'set-next-context',
				'top-level-dd-goal',
				'new-context'
			      ],
			      ['reality-stabilize'],
			      [surprise, 'top-level-dd-goal']
			    ]
			  ]).

% annotating U::NEW-ANALOGICAL-DD-GOAL-PLAN 
wl: lambda_def(defun,
	      u_new_analogical_dd_goal_plan,
	      f_u_new_analogical_dd_goal_plan,
	      [u_top_level_dd_goal, u_subgoal, u_analogical_episode],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('S'),
			     #\(e),
			     #\(r),
			     #\(e),
			     #\(n),
			     #\(d),
			     #\(i),
			     #\(p),
			     #\(i),
			     #\(t),
			     #\(y),
			     #\(' '),
			     #\(f),
			     #\(o),
			     #\(r),
			     #\(' '),
			     #\(~),
			     #\('A'),
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
			     #\(g),
			     #\(o),
			     #\(a),
			     #\(l)
			   ]),
		  [u_tlg_c62_string, u_top_level_dd_goal]
		],
		[u_ep_print, u_analogical_episode],
		
		[ let,
		  
		  [ 
		    [ u_new_context,
		      
		      [ u_cx_c36_sprout,
			[u_ob_c36_get, u_subgoal, [quote, u_activation_context]]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_xx_action_mutations_c63_xx,
		    
		    [ u_cx_c36_assert,
		      u_new_context,
		      [u_ob_c36_fcreate, [quote, [u_mutation]]]
		    ]
		  ],
		  
		  [ setq,
		    u_subgoal,
		    
		    [ u_replace_linked_ob,
		      u_subgoal,
		      u_new_context,
		      u_xx_me_belief_path_xx,
		      u_xx_empty_bd_xx
		    ]
		  ],
		  
		  [ u_ob_c36_set,
		    u_subgoal,
		    [quote, u_analogical_episode],
		    u_analogical_episode
		  ],
		  [u_set_next_context, u_top_level_dd_goal, u_new_context],
		  [u_reality_stabilize],
		  [u_surprise, u_top_level_dd_goal]
		]
	      ]).


% annotating U::NEW-ANALOGICAL-DD-GOAL-PLAN 
wl: arglist_info(u_new_analogical_dd_goal_plan,
		[u_top_level_dd_goal, u_subgoal, u_analogical_episode],
		
		[ Top_level_dd_goal_Param,
		  Subgoal_Param,
		  Analogical_episode_Param
		],
		arginfo{ all:
			     [ u_top_level_dd_goal,
			       u_subgoal,
			       u_analogical_episode
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_top_level_dd_goal,
				 u_subgoal,
				 u_analogical_episode
			       ],
			 opt:0,
			 req:
			     [ u_top_level_dd_goal,
			       u_subgoal,
			       u_analogical_episode
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NEW-ANALOGICAL-DD-GOAL-PLAN 
wl: init_args(exact_only, u_new_analogical_dd_goal_plan).


% annotating U::NEW-ANALOGICAL-DD-GOAL-PLAN 
f_u_new_analogical_dd_goal_plan(Top_level_dd_goal_Param, Subgoal_Param, Analogical_episode_Param, FnResult) :-
	Env=[bv(u_top_level_dd_goal, Top_level_dd_goal_Param), bv(u_subgoal, Subgoal_Param), bv(u_analogical_episode, Analogical_episode_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(p),
				       #\(i),
				       #\(t),
				       #\(y),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(~),
				       #\('A'),
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
				       #\(g),
				       #\(o),
				       #\(a),
				       #\(l)
				     ]),
			    [u_tlg_c62_string, u_top_level_dd_goal]
			  ],
			  Roman_nl_Ret),
	f_u_ep_print(Analogical_episode_Param, Ep_print_Ret),
	get_var(Env, u_subgoal, Subgoal_Get),
	f_u_ob_c36_get(Subgoal_Get, u_activation_context, Activation_context),
	f_u_cx_c36_sprout(Activation_context, New_context_Init),
	LEnv=[[bv(u_new_context, New_context_Init)]|Env],
	get_var(LEnv, u_xx_action_mutations_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_new_context, New_context_Get),
	    f_u_ob_c36_fcreate([quote, [u_mutation]], C36_fcreate_Ret),
	    f_u_cx_c36_assert(New_context_Get, C36_fcreate_Ret, TrueResult),
	    _92278=TrueResult
	;   _92278=[]
	),
	get_var(LEnv, u_new_context, New_context_Get28),
	get_var(LEnv, u_subgoal, Subgoal_Get27),
	get_var(LEnv, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get),
	get_var(LEnv, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_replace_linked_ob(Subgoal_Get27,
			      New_context_Get28,
			      Xx_me_belief_path_xx_Get,
			      Xx_empty_bd_xx_Get,
			      Subgoal),
	set_var(LEnv, u_subgoal, Subgoal),
	get_var(LEnv, u_subgoal, Subgoal_Get31),
	f_u_ob_c36_set(Subgoal_Get31,
		       u_analogical_episode,
		       Analogical_episode_Param,
		       C36_set_Ret),
	get_var(LEnv, u_new_context, New_context_Get34),
	f_u_set_next_context(Top_level_dd_goal_Param,
			     New_context_Get34,
			     Next_context_Ret),
	f_u_reality_stabilize(Reality_stabilize_Ret),
	f_u_surprise(Top_level_dd_goal_Param, Surprise_Ret),
	LetResult=Surprise_Ret,
	LetResult=FnResult.
:- set_opv(f_u_new_analogical_dd_goal_plan, classof, claz_function),
   set_opv(u_new_analogical_dd_goal_plan, compile_as, kw_function),
   set_opv(u_new_analogical_dd_goal_plan,
	   function,
	   f_u_new_analogical_dd_goal_plan),
   DefunResult=u_new_analogical_dd_goal_plan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7841 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copy the subgoal in the new context.",
				     8,
				     8285)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7841 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Set up a new analogical plan for the subgoal.",
				     8,
				     8433)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7841 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Set the next context of the top-level goal to this new context.",
				     8,
				     8551)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:7841 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Give the top-level goal some more emotional charge.",
				     8,
				     8680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'reality-stabilize',
			    [],
			    
			    [ if,
			      ['neq?', '*reality*', '*reality-lookahead*'],
			      [setq, '*reality*', '*reality-lookahead*']
			    ]
			  ]).

% annotating U::REALITY-STABILIZE 
wl: lambda_def(defun,
	      u_reality_stabilize,
	      f_u_reality_stabilize,
	      [],
	      
	      [ 
		[ if,
		  [u_neq_c63, u_xx_reality_xx, u_xx_reality_lookahead_xx],
		  [setq, u_xx_reality_xx, u_xx_reality_lookahead_xx]
		]
	      ]).


% annotating U::REALITY-STABILIZE 
wl: arglist_info(u_reality_stabilize,
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

% annotating U::REALITY-STABILIZE 
wl: init_args(exact_only, u_reality_stabilize).


% annotating U::REALITY-STABILIZE 
f_u_reality_stabilize(FnResult) :-
	Env=[],
	f_u_neq_c63(u_xx_reality_xx, u_xx_reality_lookahead_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_reality_lookahead_xx, Xx_reality_lookahead_xx_Get),
	    set_var(Env, u_xx_reality_xx, Xx_reality_lookahead_xx_Get),
	    FnResult=Xx_reality_lookahead_xx_Get
	;   FnResult=[]
	).
:- set_opv(f_u_reality_stabilize, classof, claz_function),
   set_opv(u_reality_stabilize, compile_as, kw_function),
   set_opv(u_reality_stabilize, function, f_u_reality_stabilize),
   DefunResult=u_reality_stabilize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8799 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8917)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Find the (unique) first-level subgoal of a daydreaming goal.",
				     1,
				     8919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8799 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8982)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dd-goal-subgoal',
			    ['dd-goal'],
			    
			    [ let,
			      
			      [ 
				[ subgoals,
				  
				  [ 'goal-subgoals',
				    'dd-goal',
				    
				    [ car,
				      
				      [ 'cx$children',
					
					[ 'ob$get',
					  'dd-goal',
					  [quote, 'activation-context']
					]
				      ]
				    ],
				    '*me-belief-path*'
				  ]
				]
			      ],
			      
			      [ if,
				subgoals,
				
				[ if,
				  [cdr, subgoals],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      'rule-long',
				      '$STRING'("Non-unique subgoal ~A"),
				      'dd-goal'
				    ],
				    []
				  ],
				  
				  [ if,
				    
				    [ not,
				      
				      [ 'ty$instance?',
					['ob$get', [car, subgoals], [quote, obj]],
					[quote, rtrue]
				      ]
				    ],
				    [car, subgoals],
				    []
				  ]
				],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    'rule-long',
				    '$STRING'("No subgoal found ~A"),
				    'dd-goal'
				  ],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::DD-GOAL-SUBGOAL 
wl: lambda_def(defun,
	      u_dd_goal_subgoal,
	      f_u_dd_goal_subgoal,
	      [u_dd_goal],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_subgoals,
		      
		      [ u_goal_subgoals,
			u_dd_goal,
			
			[ car,
			  
			  [ u_cx_c36_children,
			    
			    [ u_ob_c36_get,
			      u_dd_goal,
			      [quote, u_activation_context]
			    ]
			  ]
			],
			u_xx_me_belief_path_xx
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_subgoals,
		    
		    [ if,
		      [cdr, u_subgoals],
		      
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
				     #\(u),
				     #\(n),
				     #\(i),
				     #\(q),
				     #\(u),
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
				     #\(~),
				     #\('A')
				   ]),
			  u_dd_goal
			],
			[]
		      ],
		      
		      [ if,
			
			[ not,
			  
			  [ u_ty_c36_instance_c63,
			    [u_ob_c36_get, [car, u_subgoals], [quote, u_obj]],
			    [quote, u_rtrue]
			  ]
			],
			[car, u_subgoals],
			[]
		      ]
		    ],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule_long,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('N'),
				   #\(o),
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
				   #\(u),
				   #\(n),
				   #\(d),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_dd_goal
		      ],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::DD-GOAL-SUBGOAL 
wl: arglist_info(u_dd_goal_subgoal,
		[u_dd_goal],
		[Dd_goal_Param],
		arginfo{ all:[u_dd_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_dd_goal],
			 opt:0,
			 req:[u_dd_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DD-GOAL-SUBGOAL 
wl: init_args(exact_only, u_dd_goal_subgoal).


% annotating U::DD-GOAL-SUBGOAL 
f_u_dd_goal_subgoal(Dd_goal_Param, TrueResult32) :-
	Env=[bv(u_dd_goal, Dd_goal_Param)],
	f_u_ob_c36_get(Dd_goal_Param, u_activation_context, Activation_context),
	f_u_cx_c36_children(Activation_context, Car_Param),
	cl_car(Car_Param, Car_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_goal_subgoals(Dd_goal_Param,
			  Car_Ret,
			  Xx_me_belief_path_xx_Get,
			  Subgoals_Init),
	LEnv=[[bv(u_subgoals, Subgoals_Init)]|Env],
	get_var(LEnv, u_subgoals, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_subgoals, Subgoals_Get24),
	    cl_cdr(Subgoals_Get24, IFTEST22),
	    (   IFTEST22\==[]
	    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_rule_long,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('N'),
					       #\(o),
					       #\(n),
					       #\(-),
					       #\(u),
					       #\(n),
					       #\(i),
					       #\(q),
					       #\(u),
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
					       #\(~),
					       #\('A')
					     ]),
				    u_dd_goal
				  ],
				  Roman_nl_Ret),
		TrueResult32=[]
	    ;   get_var(LEnv, u_subgoals, Subgoals_Get26),
		cl_car(Subgoals_Get26, C36_get_Param),
		f_u_ob_c36_get(C36_get_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_rtrue, PredArgResult),
		(   PredArgResult==[]
		->  get_var(LEnv, u_subgoals, Subgoals_Get29),
		    cl_car(Subgoals_Get29, TrueResult),
		    TrueResult32=TrueResult
		;   TrueResult32=[]
		)
	    )
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('N'),
					   #\(o),
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
					   #\(u),
					   #\(n),
					   #\(d),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_dd_goal
			      ],
			      Roman_nl_Ret41),
	    TrueResult32=[]
	).
:- set_opv(f_u_dd_goal_subgoal, classof, claz_function),
   set_opv(u_dd_goal_subgoal, compile_as, kw_function),
   set_opv(u_dd_goal_subgoal, function, f_u_dd_goal_subgoal),
   DefunResult=u_dd_goal_subgoal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end if", 17, 9651)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9778)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below will later be extended to report on inaccessible inference",
				     1,
				     9780)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" rules (once path to episode is also so extended).",
				     1,
				     9851)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9903)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Wouldn't we also like to use the episode in constructing the trace?",
				     1,
				     9905)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" I don't think we do currently. Have to carry along bottom-subgoals?",
				     1,
				     9975)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:8983 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10045)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inaccessible-planning-rules',
			    [episode],
			    
			    [ yloop,
			      [initial, [result, []], [rule, []]],
			      
			      [ yfor,
				ep,
				in,
				['ob$get', episode, [quote, descendants]]
			      ],
			      
			      [ ydo,
				[setq, rule, ['ob$get', ep, [quote, rule]]],
				
				[ if,
				  
				  [ and,
				    ['inaccessible?', rule],
				    [not, ['memq?', rule, result]]
				  ],
				  [setq, result, [cons, [cons, rule, []], result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::INACCESSIBLE-PLANNING-RULES 
wl: lambda_def(defun,
	      u_inaccessible_planning_rules,
	      f_u_inaccessible_planning_rules,
	      [u_episode],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_rule, []]],
		  
		  [ u_yfor,
		    u_ep,
		    u_in,
		    [u_ob_c36_get, u_episode, [quote, u_descendants]]
		  ],
		  
		  [ u_ydo,
		    [setq, u_rule, [u_ob_c36_get, u_ep, [quote, u_rule]]],
		    
		    [ if,
		      
		      [ and,
			[u_inaccessible_c63, u_rule],
			[not, [u_memq_c63, u_rule, u_result]]
		      ],
		      [setq, u_result, [cons, [cons, u_rule, []], u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::INACCESSIBLE-PLANNING-RULES 
wl: arglist_info(u_inaccessible_planning_rules,
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

% annotating U::INACCESSIBLE-PLANNING-RULES 
wl: init_args(exact_only, u_inaccessible_planning_rules).


% annotating U::INACCESSIBLE-PLANNING-RULES 
f_u_inaccessible_planning_rules(Episode_Param, FnResult) :-
	Env=[bv(u_episode, Episode_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_rule, []]],
		    
		    [ u_yfor,
		      u_ep,
		      u_in,
		      [u_ob_c36_get, u_episode, [quote, u_descendants]]
		    ],
		    
		    [ u_ydo,
		      [setq, u_rule, [u_ob_c36_get, u_ep, [quote, u_rule]]],
		      
		      [ if,
			
			[ and,
			  [u_inaccessible_c63, u_rule],
			  [not, [u_memq_c63, u_rule, u_result]]
			],
			[setq, u_result, [cons, [cons, u_rule, []], u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_inaccessible_planning_rules, classof, claz_function),
   set_opv(u_inaccessible_planning_rules, compile_as, kw_function),
   set_opv(u_inaccessible_planning_rules,
	   function,
	   f_u_inaccessible_planning_rules),
   DefunResult=u_inaccessible_planning_rules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (yloop (initial (result nil)", 1, 10427)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (rule nil))", 1, 10459)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (yfor intends in (cx$get-all-ty (ob$get episode 'context) *intends-ob*))",
				     1,
				     10489)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        (ydo ", 1, 10571)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (setq rule (ob$get intends 'rule))",
				     1,
				     10586)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (if (and (inaccessible? rule)",
				     1,
				     10631)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (not (memq? rule result)))",
				     1,
				     10671)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (setq result (cons rule result))))",
				     1,
				     10717)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (yresult result)))", 1, 10766)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10795)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Generate surprise emotion associated with top-level goal and unhalt the",
				     1,
				     10797)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" task if it was halted.", 1, 10871)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10046 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10896)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10897 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*surprise-strength*', 0.25]).
:- set_var(TLEnv3, setq, u_xx_surprise_strength_xx, 0.25).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:10930 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    surprise,
			    ['top-level-goal'],
			    
			    [ let,
			      
			      [ 
				[ 'main-motiv',
				  
				  [ 'ob$get',
				    'top-level-goal',
				    [quote, 'main-motiv']
				  ]
				],
				[emot, []]
			      ],
			      
			      [ setq,
				emot,
				
				[ 'ob$fcreate',
				  
				  [ if,
				    
				    [ 'ty$instance?',
				      'main-motiv',
				      [quote, 'pos-emotion']
				    ],
				    
				    [ '#BQ',
				      
				      [ 'POS-SURPRISE',
					strength,
					['#COMMA', '*surprise-strength*']
				      ]
				    ],
				    
				    [ '#BQ',
				      
				      [ 'NEG-SURPRISE',
					strength,
					['#COMMA', '*surprise-strength*']
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Generate surprise emotion")
			      ],
			      ['cx$assert', '*reality*', emot],
			      
			      [ 'divert-emot-to-tlg',
				emot,
				'*reality*',
				'top-level-goal'
			      ],
			      
			      [ if,
				
				[ 'neq?',
				  ['ob$get', 'top-level-goal', [quote, status]],
				  [quote, runable]
				],
				
				[ 'change-tlg-status',
				  'top-level-goal',
				  [quote, runable]
				]
			      ]
			    ]
			  ]).

% annotating U::SURPRISE 
wl: lambda_def(defun,
	      u_surprise,
	      f_u_surprise,
	      [u_top_level_goal],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_main_motiv,
		      [u_ob_c36_get, u_top_level_goal, [quote, u_main_motiv]]
		    ],
		    [u_emot, []]
		  ],
		  
		  [ setq,
		    u_emot,
		    
		    [ u_ob_c36_fcreate,
		      
		      [ if,
			
			[ u_ty_c36_instance_c63,
			  u_main_motiv,
			  [quote, u_pos_emotion]
			],
			
			[ '#BQ',
			  
			  [ u_pos_surprise,
			    u_strength,
			    ['#COMMA', u_xx_surprise_strength_xx]
			  ]
			],
			
			[ '#BQ',
			  
			  [ u_neg_surprise,
			    u_strength,
			    ['#COMMA', u_xx_surprise_strength_xx]
			  ]
			]
		      ]
		    ]
		  ],
		  
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
			       #\(e),
			       #\(' '),
			       #\(s),
			       #\(u),
			       #\(r),
			       #\(p),
			       #\(r),
			       #\(i),
			       #\(s),
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
		  [u_cx_c36_assert, u_xx_reality_xx, u_emot],
		  
		  [ u_divert_emot_to_tlg,
		    u_emot,
		    u_xx_reality_xx,
		    u_top_level_goal
		  ],
		  
		  [ if,
		    
		    [ u_neq_c63,
		      [u_ob_c36_get, u_top_level_goal, [quote, u_status]],
		      [quote, u_runable]
		    ],
		    [u_change_tlg_status, u_top_level_goal, [quote, u_runable]]
		  ]
		]
	      ]).


% annotating U::SURPRISE 
wl: arglist_info(u_surprise,
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

% annotating U::SURPRISE 
wl: init_args(exact_only, u_surprise).


% annotating U::SURPRISE 
f_u_surprise(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_ob_c36_get(Top_level_goal_Param, u_main_motiv, Main_motiv_Init),
	LEnv=[[bv(u_main_motiv, Main_motiv_Init), bv(u_emot, [])]|Env],
	f_u_ob_c36_fcreate(
			   [ if,
			     
			     [ u_ty_c36_instance_c63,
			       u_main_motiv,
			       [quote, u_pos_emotion]
			     ],
			     
			     [ '#BQ',
			       
			       [ u_pos_surprise,
				 u_strength,
				 ['#COMMA', u_xx_surprise_strength_xx]
			       ]
			     ],
			     
			     [ '#BQ',
			       
			       [ u_neg_surprise,
				 u_strength,
				 ['#COMMA', u_xx_surprise_strength_xx]
			       ]
			     ]
			   ],
			   Emot),
	set_var(LEnv, u_emot, Emot),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
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
				       #\(e),
				       #\(' '),
				       #\(s),
				       #\(u),
				       #\(r),
				       #\(p),
				       #\(r),
				       #\(i),
				       #\(s),
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
			  Roman_nl_Ret),
	get_var(LEnv, u_emot, Emot_Get19),
	get_var(LEnv, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_cx_c36_assert(Xx_reality_xx_Get, Emot_Get19, C36_assert_Ret),
	get_var(LEnv, u_xx_reality_xx, Xx_reality_xx_Get20),
	f_u_divert_emot_to_tlg(Emot_Get19,
			       Xx_reality_xx_Get20,
			       Top_level_goal_Param,
			       To_tlg_Ret),
	f_u_neq_c63([u_ob_c36_get, u_top_level_goal, [quote, u_status]],
		    [quote, u_runable],
		    IFTEST),
	(   IFTEST\==[]
	->  f_u_change_tlg_status(Top_level_goal_Param, u_runable, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_surprise, classof, claz_function),
   set_opv(u_surprise, compile_as, kw_function),
   set_opv(u_surprise, function, f_u_surprise),
   DefunResult=u_surprise.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:11547 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'top-rules',
			    [fact],
			    
			    [ let,
			      [[rules, ['backward-chain-rules', fact]]],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, rule, in, rules],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      ['plan?', rule],
				      [not, ['constructed-plan?', rule]],
				      
				      [ 'ob$unify',
					['ob$get', rule, [quote, goal]],
					fact,
					'*empty-me-bd*'
				      ]
				    ],
				    [setq, result, [cons, rule, result]]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::TOP-RULES 
wl: lambda_def(defun,
	      u_top_rules,
	      f_u_top_rules,
	      [u_fact],
	      
	      [ 
		[ let,
		  [[u_rules, [u_backward_chain_rules, u_fact]]],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_rule, u_in, u_rules],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_plan_c63, u_rule],
			  [not, [u_constructed_plan_c63, u_rule]],
			  
			  [ u_ob_c36_unify,
			    [u_ob_c36_get, u_rule, [quote, u_goal]],
			    u_fact,
			    u_xx_empty_me_bd_xx
			  ]
			],
			[setq, u_result, [cons, u_rule, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::TOP-RULES 
wl: arglist_info(u_top_rules,
		[u_fact],
		[Fact_Param],
		arginfo{ all:[u_fact],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fact],
			 opt:0,
			 req:[u_fact],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TOP-RULES 
wl: init_args(exact_only, u_top_rules).


% annotating U::TOP-RULES 
f_u_top_rules(Fact_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param)],
	f_u_backward_chain_rules(u_fact, Rules_Init),
	LEnv=[[bv(u_rules, Rules_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_rule, u_in, u_rules],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_plan_c63, u_rule],
			  [not, [u_constructed_plan_c63, u_rule]],
			  
			  [ u_ob_c36_unify,
			    [u_ob_c36_get, u_rule, [quote, u_goal]],
			    u_fact,
			    u_xx_empty_me_bd_xx
			  ]
			],
			[setq, u_result, [cons, u_rule, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_top_rules, classof, claz_function),
   set_opv(u_top_rules, compile_as, kw_function),
   set_opv(u_top_rules, function, f_u_top_rules),
   DefunResult=u_top_rules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:11547 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This will not include subgoals which are just variables, as this",
				     1,
				     11930)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:11547 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" is plainly absurde. Or so it would seem...",
				     1,
				     11997)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12041 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bottom-rules',
			    [fact],
			    
			    [ let,
			      [[rules, ['forward-chain-rules', fact]]],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, rule, in, rules],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      ['plan?', rule],
				      [not, ['constructed-plan?', rule]]
				    ],
				    
				    [ yloop,
				      [initial, [subgoalnum, 0]],
				      
				      [ yfor,
					subgoal,
					in,
					['rule-subgoal-objs', rule]
				      ],
				      
				      [ yuntil,
					
					[ prog1,
					  
					  [ if,
					    
					    [ and,
					      [not, ['var?', subgoal]],
					      
					      [ 'ob$unify',
						subgoal,
						fact,
						'*empty-me-bd*'
					      ]
					    ],
					    
					    [ setq,
					      result,
					      
					      [ cons,
						[cons, rule, subgoalnum],
						result
					      ]
					    ],
					    []
					  ],
					  [setq, subgoalnum, [+, 1, subgoalnum]]
					]
				      ]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::BOTTOM-RULES 
wl: lambda_def(defun,
	      u_bottom_rules,
	      f_u_bottom_rules,
	      [u_fact],
	      
	      [ 
		[ let,
		  [[u_rules, [u_forward_chain_rules, u_fact]]],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_rule, u_in, u_rules],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_plan_c63, u_rule],
			  [not, [u_constructed_plan_c63, u_rule]]
			],
			
			[ u_yloop,
			  [u_initial, [u_subgoalnum, 0]],
			  [u_yfor, u_subgoal, u_in, [u_rule_subgoal_objs, u_rule]],
			  
			  [ u_yuntil,
			    
			    [ prog1,
			      
			      [ if,
				
				[ and,
				  [not, [u_var_c63, u_subgoal]],
				  
				  [ u_ob_c36_unify,
				    u_subgoal,
				    u_fact,
				    u_xx_empty_me_bd_xx
				  ]
				],
				
				[ setq,
				  u_result,
				  [cons, [cons, u_rule, u_subgoalnum], u_result]
				],
				[]
			      ],
			      [setq, u_subgoalnum, [+, 1, u_subgoalnum]]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::BOTTOM-RULES 
wl: arglist_info(u_bottom_rules,
		[u_fact],
		[Fact_Param],
		arginfo{ all:[u_fact],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fact],
			 opt:0,
			 req:[u_fact],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BOTTOM-RULES 
wl: init_args(exact_only, u_bottom_rules).


% annotating U::BOTTOM-RULES 
f_u_bottom_rules(Fact_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param)],
	f_u_forward_chain_rules(u_fact, Rules_Init),
	LEnv=[[bv(u_rules, Rules_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_rule, u_in, u_rules],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_plan_c63, u_rule],
			  [not, [u_constructed_plan_c63, u_rule]]
			],
			
			[ u_yloop,
			  [u_initial, [u_subgoalnum, 0]],
			  [u_yfor, u_subgoal, u_in, [u_rule_subgoal_objs, u_rule]],
			  
			  [ u_yuntil,
			    
			    [ prog1,
			      
			      [ if,
				
				[ and,
				  [not, [u_var_c63, u_subgoal]],
				  
				  [ u_ob_c36_unify,
				    u_subgoal,
				    u_fact,
				    u_xx_empty_me_bd_xx
				  ]
				],
				
				[ setq,
				  u_result,
				  [cons, [cons, u_rule, u_subgoalnum], u_result]
				],
				[]
			      ],
			      [setq, u_subgoalnum, [+, 1, u_subgoalnum]]
			    ]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_bottom_rules, classof, claz_function),
   set_opv(u_bottom_rules, compile_as, kw_function),
   set_opv(u_bottom_rules, function, f_u_bottom_rules),
   DefunResult=u_bottom_rules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12041 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Path lengths will be at most twice the below depth.",
				     1,
				     12804)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12041 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 5 3 3 works for lovers1 without lovers",
				     1,
				     12858)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12898 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*max-ri-depth*', 5]).
:- set_var(TLEnv3, setq, u_xx_max_ri_depth_xx, 5).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12898 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Try 4", 25, 12923)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12930 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*max-paths-found*', 3]).
:- set_var(TLEnv3, setq, u_xx_max_paths_found_xx, 3).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12930 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" may need to be 6 for some applications",
				     28,
				     12958)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12998 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*max-rip-paths*', 3]).
:- set_var(TLEnv3, setq, u_xx_max_rip_paths_xx, 3).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:12998 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Try 5 5", 26, 13024)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*paths-found*', 0]).
:- set_var(TLEnv3, setq, u_xx_paths_found_xx, 0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The above two aren't the same because after ri is performed,",
				     1,
				     13057)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the result is sorted (currently by path length).",
				     1,
				     13120)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13172)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Rule intersection search.", 1, 13174)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns a list of ri-paths, where an ri-path is a list of ri-pathelts.",
				     1,
				     13204)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13277)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: bottom-rule is actually (<bottom-rule> . <subgoalnum>),",
				     1,
				     13279)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" where <subgoalnum> may be nil.",
				     1,
				     13343)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13033 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13376)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13377 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-intersection',
			    ['top-rule', 'bottom-rule', episodes],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("Rule intersection from ~A to ~A"),
			      'top-rule',
			      'bottom-rule'
			    ],
			    [setq, '*paths-found*', 0],
			    
			    [ let,
			      
			      [ 
				[ result,
				  
				  [ 'rule-intersection1',
				    'top-rule',
				    [list, [list, 'top-rule', []]],
				    [car, 'bottom-rule'],
				    
				    [ list,
				      
				      [ list,
					[car, 'bottom-rule'],
					[cdr, 'bottom-rule']
				      ]
				    ],
				    episodes,
				    1
				  ]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'seren-long',
				'$STRING'("Rule intersection returns ~A"),
				result
			      ],
			      result
			    ]
			  ]).

% annotating U::RULE-INTERSECTION 
wl: lambda_def(defun,
	      u_rule_intersection,
	      f_u_rule_intersection,
	      [u_top_rule, u_bottom_rule, u_episodes],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(l),
			     #\(e),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(r),
			     #\(s),
			     #\(e),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
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
		  u_top_rule,
		  u_bottom_rule
		],
		[setq, u_xx_paths_found_xx, 0],
		
		[ let,
		  
		  [ 
		    [ u_result,
		      
		      [ u_rule_intersection1,
			u_top_rule,
			[list, [list, u_top_rule, []]],
			[car, u_bottom_rule],
			[list, [list, [car, u_bottom_rule], [cdr, u_bottom_rule]]],
			u_episodes,
			1
		      ]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_seren_long,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('R'),
			       #\(u),
			       #\(l),
			       #\(e),
			       #\(' '),
			       #\(i),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(r),
			       #\(s),
			       #\(e),
			       #\(c),
			       #\(t),
			       #\(i),
			       #\(o),
			       #\(n),
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
		  ],
		  u_result
		]
	      ]).


% annotating U::RULE-INTERSECTION 
wl: arglist_info(u_rule_intersection,
		[u_top_rule, u_bottom_rule, u_episodes],
		[Top_rule_Param, Bottom_rule_Param, Episodes_Param],
		arginfo{ all:[u_top_rule, u_bottom_rule, u_episodes],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_top_rule, u_bottom_rule, u_episodes],
			 opt:0,
			 req:[u_top_rule, u_bottom_rule, u_episodes],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-INTERSECTION 
wl: init_args(exact_only, u_rule_intersection).


% annotating U::RULE-INTERSECTION 
f_u_rule_intersection(Top_rule_Param, Bottom_rule_Param, Episodes_Param, FnResult) :-
	Env=[bv(u_top_rule, Top_rule_Param), bv(u_bottom_rule, Bottom_rule_Param), bv(u_episodes, Episodes_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
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
			    u_top_rule,
			    u_bottom_rule
			  ],
			  Roman_nl_Ret),
	set_var(Env, setq, u_xx_paths_found_xx, 0),
	CAR=[Top_rule_Param, []],
	_92922=[CAR],
	cl_car(Bottom_rule_Param, Car_Ret),
	cl_car(Bottom_rule_Param, Car_Ret32),
	cl_cdr(Bottom_rule_Param, Cdr_Ret),
	CAR34=[Car_Ret32, Cdr_Ret],
	_92992=[CAR34],
	f_u_rule_intersection1(Top_rule_Param,
			       _92922,
			       Car_Ret,
			       _92992,
			       Episodes_Param,
			       1,
			       Result_Init),
	LEnv=[[bv(u_result, Result_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
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
			  ],
			  Roman_nl_Ret35),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_rule_intersection, classof, claz_function),
   set_opv(u_rule_intersection, compile_as, kw_function),
   set_opv(u_rule_intersection, function, f_u_rule_intersection),
   DefunResult=u_rule_intersection.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13377 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: This algorithm could be made very efficient by compiling",
				     1,
				     13998)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13377 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" routings in advance! Perhaps, but I think dealing with cycles",
				     1,
				     14063)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:13377 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" may be difficult.", 1, 14127)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:14146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-intersection1',
			    
			    [ 'top-rule',
			      'backward-path',
			      'bottom-rule',
			      'forward-path',
			      episodes,
			      depth
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      ri,
			      '$STRING'("Ri1 ~A ~A ~A ~A ~A ~A"),
			      'top-rule',
			      'backward-path',
			      'bottom-rule',
			      'forward-path',
			      episodes,
			      depth
			    ],
			    
			    [ if,
			      [>, depth, '*max-ri-depth*'],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  ri,
				  '$STRING'("Ri1 depth exceeded")
				],
				[]
			      ],
			      
			      [ let,
				
				[ [result, []],
				  ['backward-car', [car, 'backward-path']],
				  ['forward-car', [car, 'forward-path']]
				],
				
				[ if,
				  ['eq?', 'top-rule', 'bottom-rule'],
				  
				  [ progn,
				    
				    [ setq,
				      result,
				      
				      [ list,
					
					[ append,
					  [reverse, [cdr, 'backward-path']],
					  'forward-path'
					]
				      ]
				    ],
				    
				    [ setq,
				      '*paths-found*',
				      [+, '*paths-found*', 1]
				    ]
				  ]
				],
				
				[ if,
				  
				  [ and,
				    
				    [ 'eq?',
				      ['ri-pathelt-rule', 'backward-car'],
				      
				      [ 'ri-pathelt-rule',
					[cadr, 'forward-path']
				      ]
				    ],
				    
				    [ 'eq?',
				      ['ri-pathelt-rule', 'forward-car'],
				      
				      [ 'ri-pathelt-rule',
					[cadr, 'backward-path']
				      ]
				    ],
				    
				    [ 'eq?',
				      ['ri-pathelt-subgoalnum', 'forward-car'],
				      
				      [ 'ri-pathelt-subgoalnum',
					[cadr, 'backward-path']
				      ]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ setq,
				      result,
				      
				      [ cons,
					
					[ append,
					  [reverse, [cdr, 'backward-path']],
					  [cdr, 'forward-path']
					],
					result
				      ]
				    ],
				    
				    [ setq,
				      '*paths-found*',
				      [+, '*paths-found*', 1]
				    ]
				  ]
				],
				
				[ if,
				  [<, '*paths-found*', '*max-paths-found*'],
				  
				  [ yloop,
				    
				    [ initial,
				      ['f-ep', []],
				      ['b-ep', []],
				      ['new-backward-path', []],
				      ['new-forward-path', []]
				    ],
				    
				    [ yfor,
				      f,
				      in,
				      
				      [ 'ob$gets',
					'bottom-rule',
					[quote, 'forward-chain-nums']
				      ]
				    ],
				    
				    [ ydo,
				      
				      [ yloop,
					
					[ yfor,
					  b,
					  in,
					  
					  [ 'ob$gets',
					    'top-rule',
					    [quote, 'backward-chain-nums']
					  ]
					],
					
					[ ydo,
					  
					  [ if,
					    
					    [ and,
					      ['plan?', ['chain-rule', f]],
					      ['plan?', ['chain-rule', b]],
					      
					      [ setq,
						'f-ep',
						
						[ 'ri-useable-rule?',
						  ['chain-rule', f],
						  episodes
						]
					      ],
					      
					      [ setq,
						'b-ep',
						
						[ 'ri-useable-rule?',
						  ['chain-rule', b],
						  episodes
						]
					      ],
					      
					      [ not,
						
						[ 'in-path-twice?',
						  ['chain-rule', f],
						  'backward-path'
						]
					      ],
					      
					      [ not,
						
						[ 'in-path-twice?',
						  ['chain-rule', f],
						  'forward-path'
						]
					      ],
					      
					      [ not,
						
						[ and,
						  
						  [ 'in-path?',
						    ['chain-rule', f],
						    'backward-path'
						  ],
						  
						  [ 'in-path?',
						    ['chain-rule', f],
						    'forward-path'
						  ]
						]
					      ],
					      
					      [ not,
						
						[ 'in-path-twice?',
						  ['chain-rule', b],
						  'backward-path'
						]
					      ],
					      
					      [ not,
						
						[ 'in-path-twice?',
						  ['chain-rule', b],
						  'forward-path'
						]
					      ],
					      
					      [ not,
						
						[ and,
						  
						  [ 'in-path?',
						    ['chain-rule', b],
						    'backward-path'
						  ],
						  
						  [ 'in-path?',
						    ['chain-rule', b],
						    'forward-path'
						  ]
						]
					      ]
					    ],
					    
					    [ progn,
					      
					      [ setq,
						'new-backward-path',
						
						[ cons,
						  
						  [ 'ri-pathelt-make',
						    ['chain-rule', b],
						    [],
						    
						    [ if,
						      ['ob?', 'b-ep'],
						      [list, 'b-ep'],
						      []
						    ]
						  ],
						  
						  [ cons,
						    
						    [ 'ri-pathelt-make',
						      
						      [ 'ri-pathelt-rule',
							'backward-car'
						      ],
						      ['chain-num', b],
						      
						      [ 'ri-pathelt-episodes',
							'backward-car'
						      ]
						    ],
						    [cdr, 'backward-path']
						  ]
						]
					      ],
					      
					      [ setq,
						'new-forward-path',
						
						[ cons,
						  
						  [ 'ri-pathelt-make',
						    ['chain-rule', f],
						    ['chain-num', f],
						    
						    [ if,
						      ['ob?', 'f-ep'],
						      [list, 'f-ep'],
						      []
						    ]
						  ],
						  'forward-path'
						]
					      ],
					      
					      [ setq,
						result,
						
						[ 'append!',
						  result,
						  
						  [ 'rule-intersection1',
						    ['chain-rule', b],
						    'new-backward-path',
						    ['chain-rule', f],
						    'new-forward-path',
						    episodes,
						    [+, 1, depth]
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
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  ri,
					  '$STRING'("Ri1 returns ~A"),
					  result
					],
					result
				      ]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      ri,
				      '$STRING'("Ri1 returns ~A (maxed out)"),
				      result
				    ],
				    result
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::RULE-INTERSECTION1 
wl: lambda_def(defun,
	      u_rule_intersection1,
	      f_u_rule_intersection1,
	      
	      [ u_top_rule,
		u_backward_path,
		u_bottom_rule,
		u_forward_path,
		u_episodes,
		u_depth
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_ri,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(i),
			     #\('1'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_top_rule,
		  u_backward_path,
		  u_bottom_rule,
		  u_forward_path,
		  u_episodes,
		  u_depth
		],
		
		[ if,
		  [>, u_depth, u_xx_max_ri_depth_xx],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_ri,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('R'),
				 #\(i),
				 #\('1'),
				 #\(' '),
				 #\(d),
				 #\(e),
				 #\(p),
				 #\(t),
				 #\(h),
				 #\(' '),
				 #\(e),
				 #\(x),
				 #\(c),
				 #\(e),
				 #\(e),
				 #\(d),
				 #\(e),
				 #\(d)
			       ])
		    ],
		    []
		  ],
		  
		  [ let,
		    
		    [ [u_result, []],
		      [u_backward_car, [car, u_backward_path]],
		      [u_forward_car, [car, u_forward_path]]
		    ],
		    
		    [ if,
		      [u_eq_c63, u_top_rule, u_bottom_rule],
		      
		      [ progn,
			
			[ setq,
			  u_result,
			  
			  [ list,
			    
			    [ append,
			      [reverse, [cdr, u_backward_path]],
			      u_forward_path
			    ]
			  ]
			],
			[setq, u_xx_paths_found_xx, [+, u_xx_paths_found_xx, 1]]
		      ]
		    ],
		    
		    [ if,
		      
		      [ and,
			
			[ u_eq_c63,
			  [u_ri_pathelt_rule, u_backward_car],
			  [u_ri_pathelt_rule, [cadr, u_forward_path]]
			],
			
			[ u_eq_c63,
			  [u_ri_pathelt_rule, u_forward_car],
			  [u_ri_pathelt_rule, [cadr, u_backward_path]]
			],
			
			[ u_eq_c63,
			  [u_ri_pathelt_subgoalnum, u_forward_car],
			  [u_ri_pathelt_subgoalnum, [cadr, u_backward_path]]
			]
		      ],
		      
		      [ progn,
			
			[ setq,
			  u_result,
			  
			  [ cons,
			    
			    [ append,
			      [reverse, [cdr, u_backward_path]],
			      [cdr, u_forward_path]
			    ],
			    u_result
			  ]
			],
			[setq, u_xx_paths_found_xx, [+, u_xx_paths_found_xx, 1]]
		      ]
		    ],
		    
		    [ if,
		      [<, u_xx_paths_found_xx, u_xx_max_paths_found_xx],
		      
		      [ u_yloop,
			
			[ u_initial,
			  [u_f_ep, []],
			  [u_b_ep, []],
			  [u_new_backward_path, []],
			  [u_new_forward_path, []]
			],
			
			[ u_yfor,
			  u_f,
			  u_in,
			  
			  [ u_ob_c36_gets,
			    u_bottom_rule,
			    [quote, u_forward_chain_nums]
			  ]
			],
			
			[ u_ydo,
			  
			  [ u_yloop,
			    
			    [ u_yfor,
			      u_b,
			      u_in,
			      
			      [ u_ob_c36_gets,
				u_top_rule,
				[quote, u_backward_chain_nums]
			      ]
			    ],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ and,
				  [u_plan_c63, [u_chain_rule, u_f]],
				  [u_plan_c63, [u_chain_rule, u_b]],
				  
				  [ setq,
				    u_f_ep,
				    
				    [ u_ri_useable_rule_c63,
				      [u_chain_rule, u_f],
				      u_episodes
				    ]
				  ],
				  
				  [ setq,
				    u_b_ep,
				    
				    [ u_ri_useable_rule_c63,
				      [u_chain_rule, u_b],
				      u_episodes
				    ]
				  ],
				  
				  [ not,
				    
				    [ u_in_path_twice_c63,
				      [u_chain_rule, u_f],
				      u_backward_path
				    ]
				  ],
				  
				  [ not,
				    
				    [ u_in_path_twice_c63,
				      [u_chain_rule, u_f],
				      u_forward_path
				    ]
				  ],
				  
				  [ not,
				    
				    [ and,
				      
				      [ u_in_path_c63,
					[u_chain_rule, u_f],
					u_backward_path
				      ],
				      
				      [ u_in_path_c63,
					[u_chain_rule, u_f],
					u_forward_path
				      ]
				    ]
				  ],
				  
				  [ not,
				    
				    [ u_in_path_twice_c63,
				      [u_chain_rule, u_b],
				      u_backward_path
				    ]
				  ],
				  
				  [ not,
				    
				    [ u_in_path_twice_c63,
				      [u_chain_rule, u_b],
				      u_forward_path
				    ]
				  ],
				  
				  [ not,
				    
				    [ and,
				      
				      [ u_in_path_c63,
					[u_chain_rule, u_b],
					u_backward_path
				      ],
				      
				      [ u_in_path_c63,
					[u_chain_rule, u_b],
					u_forward_path
				      ]
				    ]
				  ]
				],
				
				[ progn,
				  
				  [ setq,
				    u_new_backward_path,
				    
				    [ cons,
				      
				      [ u_ri_pathelt_make,
					[u_chain_rule, u_b],
					[],
					
					[ if,
					  [u_ob_c63, u_b_ep],
					  [list, u_b_ep],
					  []
					]
				      ],
				      
				      [ cons,
					
					[ u_ri_pathelt_make,
					  [u_ri_pathelt_rule, u_backward_car],
					  [u_chain_num, u_b],
					  
					  [ u_ri_pathelt_episodes,
					    u_backward_car
					  ]
					],
					[cdr, u_backward_path]
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_new_forward_path,
				    
				    [ cons,
				      
				      [ u_ri_pathelt_make,
					[u_chain_rule, u_f],
					[u_chain_num, u_f],
					
					[ if,
					  [u_ob_c63, u_f_ep],
					  [list, u_f_ep],
					  []
					]
				      ],
				      u_forward_path
				    ]
				  ],
				  
				  [ setq,
				    u_result,
				    
				    [ u_append_c33,
				      u_result,
				      
				      [ u_rule_intersection1,
					[u_chain_rule, u_b],
					u_new_backward_path,
					[u_chain_rule, u_f],
					u_new_forward_path,
					u_episodes,
					[+, 1, u_depth]
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
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_ri,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('R'),
					 #\(i),
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
			    ],
			    u_result
			  ]
			]
		      ],
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_ri,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('R'),
				     #\(i),
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
				     #\('A'),
				     #\(' '),
				     #\('('),
				     #\(m),
				     #\(a),
				     #\(x),
				     #\(e),
				     #\(d),
				     #\(' '),
				     #\(o),
				     #\(u),
				     #\(t),
				     #\(')')
				   ]),
			  u_result
			],
			u_result
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::RULE-INTERSECTION1 
wl: arglist_info(u_rule_intersection1,
		
		[ u_top_rule,
		  u_backward_path,
		  u_bottom_rule,
		  u_forward_path,
		  u_episodes,
		  u_depth
		],
		
		[ Top_rule_Param,
		  Backward_path_Param,
		  Bottom_rule_Param,
		  Forward_path_Param,
		  Episodes_Param,
		  Depth_Param
		],
		arginfo{ all:
			     [ u_top_rule,
			       u_backward_path,
			       u_bottom_rule,
			       u_forward_path,
			       u_episodes,
			       u_depth
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_top_rule,
				 u_backward_path,
				 u_bottom_rule,
				 u_forward_path,
				 u_episodes,
				 u_depth
			       ],
			 opt:0,
			 req:
			     [ u_top_rule,
			       u_backward_path,
			       u_bottom_rule,
			       u_forward_path,
			       u_episodes,
			       u_depth
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-INTERSECTION1 
wl: init_args(exact_only, u_rule_intersection1).


% annotating U::RULE-INTERSECTION1 
f_u_rule_intersection1(Top_rule_Param, Backward_path_Param, Bottom_rule_Param, Forward_path_Param, Episodes_Param, Depth_Param, FnResult) :-
	Env=[bv(u_top_rule, Top_rule_Param), bv(u_backward_path, Backward_path_Param), bv(u_bottom_rule, Bottom_rule_Param), bv(u_forward_path, Forward_path_Param), bv(u_episodes, Episodes_Param), bv(u_depth, Depth_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_ri,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(i),
				       #\('1'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_top_rule,
			    u_backward_path,
			    u_bottom_rule,
			    u_forward_path,
			    u_episodes,
			    u_depth
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_max_ri_depth_xx, Xx_max_ri_depth_xx_Get),
	(   Depth_Param>Xx_max_ri_depth_xx_Get
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_ri,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('R'),
					   #\(i),
					   #\('1'),
					   #\(' '),
					   #\(d),
					   #\(e),
					   #\(p),
					   #\(t),
					   #\(h),
					   #\(' '),
					   #\(e),
					   #\(x),
					   #\(c),
					   #\(e),
					   #\(e),
					   #\(d),
					   #\(e),
					   #\(d)
					 ])
			      ],
			      Roman_nl_Ret73),
	    FnResult=[]
	;   cl_car(Backward_path_Param, Backward_car_Init),
	    cl_car(Forward_path_Param, Forward_car_Init),
	    LEnv=[[bv(u_result, []), bv(u_backward_car, Backward_car_Init), bv(u_forward_car, Forward_car_Init)]|Env],
	    f_u_eq_c63(u_top_rule, u_bottom_rule, IFTEST34),
	    (   IFTEST34\==[]
	    ->  cl_cdr(Backward_path_Param, Reverse_Param),
		cl_reverse(Reverse_Param, Append_Param),
		cl_append(Append_Param, Forward_path_Param, Append_Ret),
		Result66=[Append_Ret],
		set_var(LEnv, u_result, Result66),
		get_var(LEnv, u_xx_paths_found_xx, Xx_paths_found_xx_Get),
		+(Xx_paths_found_xx_Get, 1, TrueResult),
		set_var(LEnv, u_xx_paths_found_xx, TrueResult),
		_102234=TrueResult
	    ;   _102234=[]
	    ),
	    f_u_eq_c63([u_ri_pathelt_rule, u_backward_car],
		       [u_ri_pathelt_rule, [cadr, u_forward_path]],
		       IFTEST43),
	    (   IFTEST43\==[]
	    ->  f_u_eq_c63([u_ri_pathelt_rule, u_forward_car],
			   [u_ri_pathelt_rule, [cadr, u_backward_path]],
			   IFTEST45),
		(   IFTEST45\==[]
		->  f_u_eq_c63([u_ri_pathelt_subgoalnum, u_forward_car],
			       
			       [ u_ri_pathelt_subgoalnum,
				 [cadr, u_backward_path]
			       ],
			       TrueResult47),
		    IFTEST41=TrueResult47
		;   IFTEST41=[]
		)
	    ;   IFTEST41=[]
	    ),
	    (   IFTEST41\==[]
	    ->  cl_cdr(Backward_path_Param, Reverse_Param70),
		cl_reverse(Reverse_Param70, Append_Param71),
		cl_cdr(Forward_path_Param, Cdr_Ret),
		cl_append(Append_Param71, Cdr_Ret, Append_Ret76),
		get_var(LEnv, u_result, Result_Get),
		Result67=[Append_Ret76|Result_Get],
		set_var(LEnv, u_result, Result67),
		get_var(LEnv, u_xx_paths_found_xx, Xx_paths_found_xx_Get52),
		+(Xx_paths_found_xx_Get52, 1, TrueResult53),
		set_var(LEnv, u_xx_paths_found_xx, TrueResult53),
		_102512=TrueResult53
	    ;   _102512=[]
	    ),
	    get_var(LEnv, u_xx_max_paths_found_xx, Xx_max_paths_found_xx_Get),
	    get_var(LEnv, u_xx_paths_found_xx, Xx_paths_found_xx_Get55),
	    (   Xx_paths_found_xx_Get55<Xx_max_paths_found_xx_Get
	    ->  f_u_yloop(
			  [ 
			    [ u_initial,
			      [u_f_ep, []],
			      [u_b_ep, []],
			      [u_new_backward_path, []],
			      [u_new_forward_path, []]
			    ],
			    
			    [ u_yfor,
			      u_f,
			      u_in,
			      
			      [ u_ob_c36_gets,
				u_bottom_rule,
				[quote, u_forward_chain_nums]
			      ]
			    ],
			    
			    [ u_ydo,
			      
			      [ u_yloop,
				
				[ u_yfor,
				  u_b,
				  u_in,
				  
				  [ u_ob_c36_gets,
				    u_top_rule,
				    [quote, u_backward_chain_nums]
				  ]
				],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ and,
				      [u_plan_c63, [u_chain_rule, u_f]],
				      [u_plan_c63, [u_chain_rule, u_b]],
				      
				      [ setq,
					u_f_ep,
					
					[ u_ri_useable_rule_c63,
					  [u_chain_rule, u_f],
					  u_episodes
					]
				      ],
				      
				      [ setq,
					u_b_ep,
					
					[ u_ri_useable_rule_c63,
					  [u_chain_rule, u_b],
					  u_episodes
					]
				      ],
				      
				      [ not,
					
					[ u_in_path_twice_c63,
					  [u_chain_rule, u_f],
					  u_backward_path
					]
				      ],
				      
				      [ not,
					
					[ u_in_path_twice_c63,
					  [u_chain_rule, u_f],
					  u_forward_path
					]
				      ],
				      
				      [ not,
					
					[ and,
					  
					  [ u_in_path_c63,
					    [u_chain_rule, u_f],
					    u_backward_path
					  ],
					  
					  [ u_in_path_c63,
					    [u_chain_rule, u_f],
					    u_forward_path
					  ]
					]
				      ],
				      
				      [ not,
					
					[ u_in_path_twice_c63,
					  [u_chain_rule, u_b],
					  u_backward_path
					]
				      ],
				      
				      [ not,
					
					[ u_in_path_twice_c63,
					  [u_chain_rule, u_b],
					  u_forward_path
					]
				      ],
				      
				      [ not,
					
					[ and,
					  
					  [ u_in_path_c63,
					    [u_chain_rule, u_b],
					    u_backward_path
					  ],
					  
					  [ u_in_path_c63,
					    [u_chain_rule, u_b],
					    u_forward_path
					  ]
					]
				      ]
				    ],
				    
				    [ progn,
				      
				      [ setq,
					u_new_backward_path,
					
					[ cons,
					  
					  [ u_ri_pathelt_make,
					    [u_chain_rule, u_b],
					    [],
					    
					    [ if,
					      [u_ob_c63, u_b_ep],
					      [list, u_b_ep],
					      []
					    ]
					  ],
					  
					  [ cons,
					    
					    [ u_ri_pathelt_make,
					      
					      [ u_ri_pathelt_rule,
						u_backward_car
					      ],
					      [u_chain_num, u_b],
					      
					      [ u_ri_pathelt_episodes,
						u_backward_car
					      ]
					    ],
					    [cdr, u_backward_path]
					  ]
					]
				      ],
				      
				      [ setq,
					u_new_forward_path,
					
					[ cons,
					  
					  [ u_ri_pathelt_make,
					    [u_chain_rule, u_f],
					    [u_chain_num, u_f],
					    
					    [ if,
					      [u_ob_c63, u_f_ep],
					      [list, u_f_ep],
					      []
					    ]
					  ],
					  u_forward_path
					]
				      ],
				      
				      [ setq,
					u_result,
					
					[ u_append_c33,
					  u_result,
					  
					  [ u_rule_intersection1,
					    [u_chain_rule, u_b],
					    u_new_backward_path,
					    [u_chain_rule, u_f],
					    u_new_forward_path,
					    u_episodes,
					    [+, 1, u_depth]
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
				
				[ u_ndbg_roman_nl,
				  u_xx_gate_dbg_xx,
				  u_ri,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('R'),
					     #\(i),
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
				],
				u_result
			      ]
			    ]
			  ],
			  TrueResult61),
		FnResult=TrueResult61
	    ;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_ri,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('R'),
					       #\(i),
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
					       #\('A'),
					       #\(' '),
					       #\('('),
					       #\(m),
					       #\(a),
					       #\(x),
					       #\(e),
					       #\(d),
					       #\(' '),
					       #\(o),
					       #\(u),
					       #\(t),
					       #\(')')
					     ]),
				    u_result
				  ],
				  Roman_nl_Ret77),
		get_var(LEnv, u_result, Result_Get60),
		FnResult=Result_Get60
	    )
	).
:- set_opv(f_u_rule_intersection1, classof, claz_function),
   set_opv(u_rule_intersection1, compile_as, kw_function),
   set_opv(u_rule_intersection1, function, f_u_rule_intersection1),
   DefunResult=u_rule_intersection1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:14146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" I feel the following two cases are disjoint, but am not positive.",
				     4,
				     14627)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17625 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'in-path-twice?',
			    [rule, path],
			    
			    [ let,
			      [['once-in?', ['in-path?', rule, path]]],
			      
			      [ if,
				'once-in?',
				['in-path?', rule, [cdr, 'once-in?']],
				[]
			      ]
			    ]
			  ]).

% annotating U::IN-PATH-TWICE? 
wl: lambda_def(defun,
	      u_in_path_twice_c63,
	      f_u_in_path_twice_c63,
	      [u_rule, u_path],
	      
	      [ 
		[ let,
		  [[u_once_in_c63, [u_in_path_c63, u_rule, u_path]]],
		  
		  [ if,
		    u_once_in_c63,
		    [u_in_path_c63, u_rule, [cdr, u_once_in_c63]],
		    []
		  ]
		]
	      ]).


% annotating U::IN-PATH-TWICE? 
wl: arglist_info(u_in_path_twice_c63,
		[u_rule, u_path],
		[Rule_Param, Path_Param],
		arginfo{ all:[u_rule, u_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_path],
			 opt:0,
			 req:[u_rule, u_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::IN-PATH-TWICE? 
wl: init_args(exact_only, u_in_path_twice_c63).


% annotating U::IN-PATH-TWICE? 
f_u_in_path_twice_c63(Rule_Param, Path_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_path, Path_Param)],
	f_u_in_path_c63(Rule_Param, Path_Param, Once_in_c63_Init),
	LEnv=[[bv(u_once_in_c63, Once_in_c63_Init)]|Env],
	get_var(LEnv, u_once_in_c63, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_once_in_c63, Once_in_c63_Get24),
	    cl_cdr(Once_in_c63_Get24, Cdr_Ret),
	    f_u_in_path_c63(Rule_Param, Cdr_Ret, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_in_path_twice_c63, classof, claz_function),
   set_opv(u_in_path_twice_c63, compile_as, kw_function),
   set_opv(u_in_path_twice_c63, function, f_u_in_path_twice_c63),
   DefunResult=u_in_path_twice_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'in-path?',
			    [rule, path],
			    
			    [ mem,
			      [lambda, [o, e], ['eq?', o, ['ri-pathelt-rule', e]]],
			      rule,
			      path
			    ]
			  ]).

% annotating U::IN-PATH? 
wl: lambda_def(defun,
	      u_in_path_c63,
	      f_u_in_path_c63,
	      [u_rule, u_path],
	      
	      [ 
		[ u_mem,
		  [lambda, [u_o, u_e], [u_eq_c63, u_o, [u_ri_pathelt_rule, u_e]]],
		  u_rule,
		  u_path
		]
	      ]).


% annotating U::IN-PATH? 
wl: arglist_info(u_in_path_c63,
		[u_rule, u_path],
		[Rule_Param, Path_Param],
		arginfo{ all:[u_rule, u_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_path],
			 opt:0,
			 req:[u_rule, u_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::IN-PATH? 
wl: init_args(exact_only, u_in_path_c63).


% annotating U::IN-PATH? 
f_u_in_path_c63(Rule_Param, Path_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_path, Path_Param)],
	f_u_mem([lambda, [u_o, u_e], [u_eq_c63, u_o, [u_ri_pathelt_rule, u_e]]],
		u_rule,
		u_path,
		Path),
	Path=FnResult.
:- set_opv(f_u_in_path_c63, classof, claz_function),
   set_opv(u_in_path_c63, compile_as, kw_function),
   set_opv(u_in_path_c63, function, f_u_in_path_c63),
   DefunResult=u_in_path_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17887)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Assumes rule is a planning rule.",
				     1,
				     17889)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns NIL, T, or episode.", 1, 17924)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17954)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Should this function return episodes for plain planning rules?",
				     1,
				     17956)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Should we then incorporate metrics here?",
				     1,
				     18021)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 18064)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18065 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ri-useable-rule?',
			    [rule, episodes],
			    
			    [ cond,
			      [[not, ['constructed-plan?', rule]], t],
			      [['rule-in-any-episode?', rule, episodes]],
			      [else, []]
			    ]
			  ]).

% annotating U::RI-USEABLE-RULE? 
wl: lambda_def(defun,
	      u_ri_useable_rule_c63,
	      f_u_ri_useable_rule_c63,
	      [u_rule, u_episodes],
	      
	      [ 
		[ cond,
		  [[not, [u_constructed_plan_c63, u_rule]], t],
		  [[u_rule_in_any_episode_c63, u_rule, u_episodes]],
		  [u_else, []]
		]
	      ]).


% annotating U::RI-USEABLE-RULE? 
wl: arglist_info(u_ri_useable_rule_c63,
		[u_rule, u_episodes],
		[Rule_Param, Episodes_Param],
		arginfo{ all:[u_rule, u_episodes],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_episodes],
			 opt:0,
			 req:[u_rule, u_episodes],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RI-USEABLE-RULE? 
wl: init_args(exact_only, u_ri_useable_rule_c63).


% annotating U::RI-USEABLE-RULE? 
f_u_ri_useable_rule_c63(Rule_Param, Episodes_Param, ElseResult26) :-
	Env=[bv(u_rule, Rule_Param), bv(u_episodes, Episodes_Param)],
	f_u_constructed_plan_c63(Rule_Param, PredArgResult),
	(   PredArgResult==[]
	->  ElseResult26=t
	;   f_u_rule_in_any_episode_c63(Rule_Param, Episodes_Param, IFTEST18),
	    (   IFTEST18\==[]
	    ->  ElseResult26=[]
	    ;   get_var(Env, u_else, IFTEST22),
		(   IFTEST22\==[]
		->  ElseResult26=[]
		;   ElseResult26=[]
		)
	    )
	).
:- set_opv(f_u_ri_useable_rule_c63, classof, claz_function),
   set_opv(u_ri_useable_rule_c63, compile_as, kw_function),
   set_opv(u_ri_useable_rule_c63, function, f_u_ri_useable_rule_c63),
   DefunResult=u_ri_useable_rule_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18065 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 18211)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18065 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns NIL or episode.", 1, 18213)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18065 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 18239)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18065 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: This should be optimized. For example, you can keep a list",
				     1,
				     18241)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18065 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" of rules associated with each episode.",
				     1,
				     18308)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18065 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 18349)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-in-any-episode?',
			    [rule, episodes],
			    
			    [ any,
			      
			      [ lambda,
				[episode],
				['rule-in-episode?', rule, episode]
			      ],
			      episodes
			    ]
			  ]).

% annotating U::RULE-IN-ANY-EPISODE? 
wl: lambda_def(defun,
	      u_rule_in_any_episode_c63,
	      f_u_rule_in_any_episode_c63,
	      [u_rule, u_episodes],
	      
	      [ 
		[ u_any,
		  
		  [ lambda,
		    [u_episode],
		    [u_rule_in_episode_c63, u_rule, u_episode]
		  ],
		  u_episodes
		]
	      ]).


% annotating U::RULE-IN-ANY-EPISODE? 
wl: arglist_info(u_rule_in_any_episode_c63,
		[u_rule, u_episodes],
		[Rule_Param, Episodes_Param],
		arginfo{ all:[u_rule, u_episodes],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_episodes],
			 opt:0,
			 req:[u_rule, u_episodes],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-IN-ANY-EPISODE? 
wl: init_args(exact_only, u_rule_in_any_episode_c63).


% annotating U::RULE-IN-ANY-EPISODE? 
f_u_rule_in_any_episode_c63(Rule_Param, Episodes_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_episodes, Episodes_Param)],
	f_u_any([lambda, [u_episode], [u_rule_in_episode_c63, u_rule, u_episode]],
		u_episodes,
		Episodes),
	Episodes=FnResult.
:- set_opv(f_u_rule_in_any_episode_c63, classof, claz_function),
   set_opv(u_rule_in_any_episode_c63, compile_as, kw_function),
   set_opv(u_rule_in_any_episode_c63, function, f_u_rule_in_any_episode_c63),
   DefunResult=u_rule_in_any_episode_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This old code does not take into account enclosing episodes",
				     1,
				     18472)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (let ((eps (ob$gets rule 'indexes)) ; was (episode-retrieve rule)",
				     1,
				     18534)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (result nil))", 1, 18603)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (any? (lambda (ep) (if (memq? ep episodes) (setq result ep) nil))",
				     1,
				     18626)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("             eps)", 1, 18700)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("       result))", 1, 18719)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns not episode, necessarily, but the actual sub-episode in",
				     1,
				     18737)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" which this rule is used.", 1, 18803)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-in-episode?',
			    [rule, episode],
			    
			    [ any,
			      
			      [ lambda,
				[ep],
				
				[ if,
				  ['eq?', rule, ['ob$get', ep, [quote, rule]]],
				  ep,
				  []
				]
			      ],
			      ['ob$get', episode, [quote, descendants]]
			    ]
			  ]).

% annotating U::RULE-IN-EPISODE? 
wl: lambda_def(defun,
	      u_rule_in_episode_c63,
	      f_u_rule_in_episode_c63,
	      [u_rule, u_episode],
	      
	      [ 
		[ u_any,
		  
		  [ lambda,
		    [u_ep],
		    
		    [ if,
		      [u_eq_c63, u_rule, [u_ob_c36_get, u_ep, [quote, u_rule]]],
		      u_ep,
		      []
		    ]
		  ],
		  [u_ob_c36_get, u_episode, [quote, u_descendants]]
		]
	      ]).


% annotating U::RULE-IN-EPISODE? 
wl: arglist_info(u_rule_in_episode_c63,
		[u_rule, u_episode],
		[Rule_Param, Episode_Param],
		arginfo{ all:[u_rule, u_episode],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_episode],
			 opt:0,
			 req:[u_rule, u_episode],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RULE-IN-EPISODE? 
wl: init_args(exact_only, u_rule_in_episode_c63).


% annotating U::RULE-IN-EPISODE? 
f_u_rule_in_episode_c63(Rule_Param, Episode_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_episode, Episode_Param)],
	f_u_any(
		[ lambda,
		  [u_ep],
		  
		  [ if,
		    [u_eq_c63, u_rule, [u_ob_c36_get, u_ep, [quote, u_rule]]],
		    u_ep,
		    []
		  ]
		],
		[u_ob_c36_get, u_episode, [quote, u_descendants]],
		Any_Ret),
	Any_Ret=FnResult.
:- set_opv(f_u_rule_in_episode_c63, classof, claz_function),
   set_opv(u_rule_in_episode_c63, compile_as, kw_function),
   set_opv(u_rule_in_episode_c63, function, f_u_rule_in_episode_c63),
   DefunResult=u_rule_in_episode_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (any (lambda (intends) (if (eq? rule (ob$get intends 'rule))",
				     1,
				     19019)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                             (ob$get (ob$get intends 'linked-from) 'episode)",
				     1,
				     19083)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                             nil))",
				     1,
				     19161)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (cx$get-all-ty (ob$get episode 'context) *intends-ob*)))",
				     1,
				     19197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Verification of rule intersection search.",
				     1,
				     19265)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19309)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" From ri-paths, create first episode(s) suitable for use in analogical",
				     1,
				     19311)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" planning. Returns NIL (if unsuccessful) or an episode-spec",
				     1,
				     19383)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (<accessed-eps> . <ep>)", 1, 19444)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19470)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Perhaps we could add some heuristics to select certain successful",
				     1,
				     19472)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" episodes over others.", 1, 19540)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19564)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This function favors SHORTER paths. Should this be?",
				     1,
				     19566)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note that before this is called, substrings are pruned.",
				     1,
				     19620)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 19678)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Destroy and otherwise GC contexts etc. that are thrown",
				     1,
				     19680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:18829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" away when an rip-path verification fails.",
				     1,
				     19743)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:19786 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rip-paths->episode',
			    ['ri-paths', 'top-level-goal', 'bottom-goal'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("Rule intersection paths to episode")
			    ],
			    
			    [ 'no-gen',
			      
			      [ yloop,
				[initial, [result, []], [i, 0]],
				
				[ yfor,
				  'ri-path',
				  in,
				  ['sort-ri-paths', 'ri-paths']
				],
				[ywhile, [<, i, '*max-rip-paths*']],
				
				[ yuntil,
				  
				  [ and,
				    [>, [length, 'ri-path'], 1],
				    
				    [ or,
				      ['null?', '*action-mutations?*'],
				      [<, [length, 'ri-path'], 5]
				    ],
				    [setq, i, [+, 1, i]],
				    
				    [ setq,
				      result,
				      
				      [ 'ri-path->episode',
					'ri-path',
					'top-level-goal',
					'bottom-goal'
				      ]
				    ]
				  ]
				],
				
				[ yresult,
				  
				  [ progn,
				    
				    [ 'ndbg-roman-nl',
				      '*gate-dbg*',
				      'seren-long',
				      '$STRING'("Result ~A, i = ~A"),
				      result,
				      i
				    ],
				    result
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::RIP-PATHS->EPISODE 
wl: lambda_def(defun,
	      u_rip_paths_c62_episode,
	      f_u_rip_paths_c62_episode,
	      [u_ri_paths, u_top_level_goal, u_bottom_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(u),
			     #\(l),
			     #\(e),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(r),
			     #\(s),
			     #\(e),
			     #\(c),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(' '),
			     #\(p),
			     #\(a),
			     #\(t),
			     #\(h),
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
			     #\(e)
			   ])
		],
		
		[ u_no_gen,
		  
		  [ u_yloop,
		    [u_initial, [u_result, []], [u_i, 0]],
		    [u_yfor, u_ri_path, u_in, [u_sort_ri_paths, u_ri_paths]],
		    [u_ywhile, [<, u_i, u_xx_max_rip_paths_xx]],
		    
		    [ u_yuntil,
		      
		      [ and,
			[>, [length, u_ri_path], 1],
			
			[ or,
			  [u_null_c63, u_xx_action_mutations_c63_xx],
			  [<, [length, u_ri_path], 5]
			],
			[setq, u_i, [+, 1, u_i]],
			
			[ setq,
			  u_result,
			  
			  [ u_ri_path_c62_episode,
			    u_ri_path,
			    u_top_level_goal,
			    u_bottom_goal
			  ]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_seren_long,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\('R'),
				     #\(e),
				     #\(s),
				     #\(u),
				     #\(l),
				     #\(t),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(','),
				     #\(' '),
				     #\(i),
				     #\(' '),
				     #\(=),
				     #\(' '),
				     #\(~),
				     #\('A')
				   ]),
			  u_result,
			  u_i
			],
			u_result
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::RIP-PATHS->EPISODE 
wl: arglist_info(u_rip_paths_c62_episode,
		[u_ri_paths, u_top_level_goal, u_bottom_goal],
		[Ri_paths_Param, Top_level_goal_Param, Bottom_goal_Param],
		arginfo{ all:[u_ri_paths, u_top_level_goal, u_bottom_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ri_paths, u_top_level_goal, u_bottom_goal],
			 opt:0,
			 req:[u_ri_paths, u_top_level_goal, u_bottom_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RIP-PATHS->EPISODE 
wl: init_args(exact_only, u_rip_paths_c62_episode).


% annotating U::RIP-PATHS->EPISODE 
f_u_rip_paths_c62_episode(Ri_paths_Param, Top_level_goal_Param, Bottom_goal_Param, FnResult) :-
	Env=[bv(u_ri_paths, Ri_paths_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_bottom_goal, Bottom_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(u),
				       #\(l),
				       #\(e),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(p),
				       #\(a),
				       #\(t),
				       #\(h),
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
				       #\(e)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_no_gen(
		   [ 
		     [ u_yloop,
		       [u_initial, [u_result, []], [u_i, 0]],
		       [u_yfor, u_ri_path, u_in, [u_sort_ri_paths, u_ri_paths]],
		       [u_ywhile, [<, u_i, u_xx_max_rip_paths_xx]],
		       
		       [ u_yuntil,
			 
			 [ and,
			   [>, [length, u_ri_path], 1],
			   
			   [ or,
			     [u_null_c63, u_xx_action_mutations_c63_xx],
			     [<, [length, u_ri_path], 5]
			   ],
			   [setq, u_i, [+, 1, u_i]],
			   
			   [ setq,
			     u_result,
			     
			     [ u_ri_path_c62_episode,
			       u_ri_path,
			       u_top_level_goal,
			       u_bottom_goal
			     ]
			   ]
			 ]
		       ],
		       
		       [ u_yresult,
			 
			 [ progn,
			   
			   [ u_ndbg_roman_nl,
			     u_xx_gate_dbg_xx,
			     u_seren_long,
			     '$ARRAY'([*],
				      claz_base_character,
				      
				      [ #\('R'),
					#\(e),
					#\(s),
					#\(u),
					#\(l),
					#\(t),
					#\(' '),
					#\(~),
					#\('A'),
					#\(','),
					#\(' '),
					#\(i),
					#\(' '),
					#\(=),
					#\(' '),
					#\(~),
					#\('A')
				      ]),
			     u_result,
			     u_i
			   ],
			   u_result
			 ]
		       ]
		     ]
		   ],
		   No_gen_Ret),
	No_gen_Ret=FnResult.
:- set_opv(f_u_rip_paths_c62_episode, classof, claz_function),
   set_opv(u_rip_paths_c62_episode, compile_as, kw_function),
   set_opv(u_rip_paths_c62_episode, function, f_u_rip_paths_c62_episode),
   DefunResult=u_rip_paths_c62_episode.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:19786 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" end no-gen and define", 4, 20549)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:19786 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For action mutations, favor shorter paths; otherwise favor longer paths.",
				     1,
				     20574)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:19786 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (ad hoc?)", 1, 20649)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:20660 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'sort-ri-paths',
			    ['ri-paths'],
			    
			    [ if,
			      '*action-mutations?*',
			      
			      [ sort,
				'ri-paths',
				
				[ lambda,
				  [path1, path2],
				  [<, [length, path1], [length, path2]]
				]
			      ],
			      
			      [ sort,
				'ri-paths',
				
				[ lambda,
				  [path1, path2],
				  [>, [length, path1], [length, path2]]
				]
			      ]
			    ]
			  ]).

% annotating U::SORT-RI-PATHS 
wl: lambda_def(defun,
	      u_sort_ri_paths,
	      f_u_sort_ri_paths,
	      [u_ri_paths],
	      
	      [ 
		[ if,
		  u_xx_action_mutations_c63_xx,
		  
		  [ sort,
		    u_ri_paths,
		    
		    [ lambda,
		      [u_path1, u_path2],
		      [<, [length, u_path1], [length, u_path2]]
		    ]
		  ],
		  
		  [ sort,
		    u_ri_paths,
		    
		    [ lambda,
		      [u_path1, u_path2],
		      [>, [length, u_path1], [length, u_path2]]
		    ]
		  ]
		]
	      ]).


% annotating U::SORT-RI-PATHS 
wl: arglist_info(u_sort_ri_paths,
		[u_ri_paths],
		[Ri_paths_Param],
		arginfo{ all:[u_ri_paths],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ri_paths],
			 opt:0,
			 req:[u_ri_paths],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SORT-RI-PATHS 
wl: init_args(exact_only, u_sort_ri_paths).


% annotating U::SORT-RI-PATHS 
f_u_sort_ri_paths(Ri_paths_Param, FnResult) :-
	Env=[bv(u_ri_paths, Ri_paths_Param)],
	get_var(Env, u_xx_action_mutations_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  Lambda=closure([Env|Env], LResult, [u_path1, u_path2],  (get_var(Env, u_path1, Path1_Get), cl_length(Path1_Get, Length_Ret), get_var(Env, u_path2, Path2_Get), cl_length(Path2_Get, Length_Ret36), <(Length_Ret, Length_Ret36, LResult))),
	    cl_sort(Ri_paths_Param, Lambda, TrueResult),
	    FnResult=TrueResult
	;   Lambda29=closure([Env|Env], LResult28, [u_path1, u_path2],  (get_var(Env, u_path1, Path1_Get24), cl_length(Path1_Get24, Length_Ret37), get_var(Env, u_path2, Path2_Get26), cl_length(Path2_Get26, Length_Ret38), >(Length_Ret37, Length_Ret38, LResult28))),
	    cl_sort(Ri_paths_Param, Lambda29, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_sort_ri_paths, classof, claz_function),
   set_opv(u_sort_ri_paths, compile_as, kw_function),
   set_opv(u_sort_ri_paths, function, f_u_sort_ri_paths),
   DefunResult=u_sort_ri_paths.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:20660 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 20904)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:20660 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" From an ri-path, create episode(s) suitable for use in analogical planning.",
				     1,
				     20906)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:20660 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns NIL (if unsuccessful) or an episode-spec (<accessed-eps> . <ep>)",
				     1,
				     20984)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:20660 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 21059)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:20660 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" An assumption is that the top-rule unifies with the top-level-goal.",
				     1,
				     21061)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:20660 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 21131)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:21132 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ri-path->episode',
			    ['ri-path', 'top-level-goal', 'bottom-goal'],
			    
			    [ let,
			      
			      [ 
				[ result,
				  
				  [ if,
				    [],
				    
				    [ 'ri-path->episode0',
				      'ri-path',
				      'top-level-goal',
				      'bottom-goal'
				    ],
				    
				    [ 'with-no-dbg',
				      
				      [ 'ri-path->episode0',
					'ri-path',
					'top-level-goal',
					'bottom-goal'
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				['null?', result],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Rule intersection for ~A of length ~A not verified"),
				  ['tlg->string', 'top-level-goal'],
				  [length, 'ri-path']
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Rule intersection for ~A of length ~A verified"),
				  ['tlg->string', 'top-level-goal'],
				  [length, 'ri-path']
				]
			      ],
			      result
			    ]
			  ]).

% annotating U::RI-PATH->EPISODE 
wl: lambda_def(defun,
	      u_ri_path_c62_episode,
	      f_u_ri_path_c62_episode,
	      [u_ri_path, u_top_level_goal, u_bottom_goal],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_result,
		      
		      [ if,
			[],
			
			[ u_ri_path_c62_episode0,
			  u_ri_path,
			  u_top_level_goal,
			  u_bottom_goal
			],
			
			[ u_with_no_dbg,
			  
			  [ u_ri_path_c62_episode0,
			    u_ri_path,
			    u_top_level_goal,
			    u_bottom_goal
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, u_result],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('R'),
				 #\(u),
				 #\(l),
				 #\(e),
				 #\(' '),
				 #\(i),
				 #\(n),
				 #\(t),
				 #\(e),
				 #\(r),
				 #\(s),
				 #\(e),
				 #\(c),
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
				 #\(' '),
				 #\(o),
				 #\(f),
				 #\(' '),
				 #\(l),
				 #\(e),
				 #\(n),
				 #\(g),
				 #\(t),
				 #\(h),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(v),
				 #\(e),
				 #\(r),
				 #\(i),
				 #\(f),
				 #\(i),
				 #\(e),
				 #\(d)
			       ]),
		      [u_tlg_c62_string, u_top_level_goal],
		      [length, u_ri_path]
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('R'),
				 #\(u),
				 #\(l),
				 #\(e),
				 #\(' '),
				 #\(i),
				 #\(n),
				 #\(t),
				 #\(e),
				 #\(r),
				 #\(s),
				 #\(e),
				 #\(c),
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
				 #\(' '),
				 #\(o),
				 #\(f),
				 #\(' '),
				 #\(l),
				 #\(e),
				 #\(n),
				 #\(g),
				 #\(t),
				 #\(h),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(v),
				 #\(e),
				 #\(r),
				 #\(i),
				 #\(f),
				 #\(i),
				 #\(e),
				 #\(d)
			       ]),
		      [u_tlg_c62_string, u_top_level_goal],
		      [length, u_ri_path]
		    ]
		  ],
		  u_result
		]
	      ]).


% annotating U::RI-PATH->EPISODE 
wl: arglist_info(u_ri_path_c62_episode,
		[u_ri_path, u_top_level_goal, u_bottom_goal],
		[Ri_path_Param, Top_level_goal_Param, Bottom_goal_Param],
		arginfo{ all:[u_ri_path, u_top_level_goal, u_bottom_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ri_path, u_top_level_goal, u_bottom_goal],
			 opt:0,
			 req:[u_ri_path, u_top_level_goal, u_bottom_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RI-PATH->EPISODE 
wl: init_args(exact_only, u_ri_path_c62_episode).


% annotating U::RI-PATH->EPISODE 
f_u_ri_path_c62_episode(Ri_path_Param, Top_level_goal_Param, Bottom_goal_Param, FnResult) :-
	Env=[bv(u_ri_path, Ri_path_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_bottom_goal, Bottom_goal_Param)],
	(   []\==[]
	->  f_u_ri_path_c62_episode0(Ri_path_Param,
				     Top_level_goal_Param,
				     Bottom_goal_Param,
				     TrueResult),
	    Result_Init=TrueResult
	;   f_u_with_no_dbg(
			    [ 
			      [ u_ri_path_c62_episode0,
				u_ri_path,
				u_top_level_goal,
				u_bottom_goal
			      ]
			    ],
			    ElseResult),
	    Result_Init=ElseResult
	),
	LEnv=[[bv(u_result, Result_Init)]|Env],
	f_u_null_c63(u_result, IFTEST26),
	(   IFTEST26\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('R'),
					   #\(u),
					   #\(l),
					   #\(e),
					   #\(' '),
					   #\(i),
					   #\(n),
					   #\(t),
					   #\(e),
					   #\(r),
					   #\(s),
					   #\(e),
					   #\(c),
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
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(' '),
					   #\(l),
					   #\(e),
					   #\(n),
					   #\(g),
					   #\(t),
					   #\(h),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(v),
					   #\(e),
					   #\(r),
					   #\(i),
					   #\(f),
					   #\(i),
					   #\(e),
					   #\(d)
					 ]),
				[u_tlg_c62_string, u_top_level_goal],
				[length, u_ri_path]
			      ],
			      TrueResult28),
	    _94886=TrueResult28
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('R'),
					   #\(u),
					   #\(l),
					   #\(e),
					   #\(' '),
					   #\(i),
					   #\(n),
					   #\(t),
					   #\(e),
					   #\(r),
					   #\(s),
					   #\(e),
					   #\(c),
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
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(' '),
					   #\(l),
					   #\(e),
					   #\(n),
					   #\(g),
					   #\(t),
					   #\(h),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(v),
					   #\(e),
					   #\(r),
					   #\(i),
					   #\(f),
					   #\(i),
					   #\(e),
					   #\(d)
					 ]),
				[u_tlg_c62_string, u_top_level_goal],
				[length, u_ri_path]
			      ],
			      ElseResult29),
	    _94886=ElseResult29
	),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_ri_path_c62_episode, classof, claz_function),
   set_opv(u_ri_path_c62_episode, compile_as, kw_function),
   set_opv(u_ri_path_c62_episode, function, f_u_ri_path_c62_episode),
   DefunResult=u_ri_path_c62_episode.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:21132 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (interrogate \"Debugs? \")", 25, 21218)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:21891 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ri-path->episode0',
			    ['ri-path', 'top-level-goal', 'bottom-goal'],
			    
			    [ 'let*',
			      
			      [ [context, ['cx$create']],
				
				[ 'top-rule',
				  ['ri-pathelt-rule', [car, 'ri-path']]
				],
				
				[ bd,
				  
				  [ 'ob$unify',
				    ['ob$get', 'top-rule', [quote, goal]],
				    ['ob$get', 'top-level-goal', [quote, obj]],
				    '*empty-me-bd*'
				  ]
				]
			      ],
			      
			      [ setq,
				'top-level-goal',
				
				[ 'ob$fcreate',
				  
				  [ '#BQ',
				    
				    [ 'ACTIVE-GOAL',
				      obj,
				      
				      [ '#COMMA',
					
					[ 'ob$get',
					  'top-level-goal',
					  [quote, obj]
					]
				      ],
				      'activation-context',
				      ['#COMMA', context]
				    ]
				  ]
				]
			      ],
			      
			      [ 'ob$set',
				'top-level-goal',
				[quote, 'top-level-goal'],
				'top-level-goal'
			      ],
			      ['cx$assert', context, 'top-level-goal'],
			      
			      [ 'ri-path->episode1',
				'ri-path',
				'top-level-goal',
				context,
				bd,
				[],
				'top-level-goal',
				'bottom-goal'
			      ]
			    ]
			  ]).

% annotating U::RI-PATH->EPISODE0 
wl: lambda_def(defun,
	      u_ri_path_c62_episode0,
	      f_u_ri_path_c62_episode0,
	      [u_ri_path, u_top_level_goal, u_bottom_goal],
	      
	      [ 
		[ let_xx,
		  
		  [ [u_context, [u_cx_c36_create]],
		    [u_top_rule, [u_ri_pathelt_rule, [car, u_ri_path]]],
		    
		    [ u_bd,
		      
		      [ u_ob_c36_unify,
			[u_ob_c36_get, u_top_rule, [quote, u_goal]],
			[u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
			u_xx_empty_me_bd_xx
		      ]
		    ]
		  ],
		  
		  [ setq,
		    u_top_level_goal,
		    
		    [ u_ob_c36_fcreate,
		      
		      [ '#BQ',
			
			[ u_active_goal,
			  u_obj,
			  
			  [ '#COMMA',
			    [u_ob_c36_get, u_top_level_goal, [quote, u_obj]]
			  ],
			  u_activation_context,
			  ['#COMMA', u_context]
			]
		      ]
		    ]
		  ],
		  
		  [ u_ob_c36_set,
		    u_top_level_goal,
		    [quote, u_top_level_goal],
		    u_top_level_goal
		  ],
		  [u_cx_c36_assert, u_context, u_top_level_goal],
		  
		  [ u_ri_path_c62_episode1,
		    u_ri_path,
		    u_top_level_goal,
		    u_context,
		    u_bd,
		    [],
		    u_top_level_goal,
		    u_bottom_goal
		  ]
		]
	      ]).


% annotating U::RI-PATH->EPISODE0 
wl: arglist_info(u_ri_path_c62_episode0,
		[u_ri_path, u_top_level_goal, u_bottom_goal],
		[Ri_path_Param, Top_level_goal_Param, Bottom_goal_Param],
		arginfo{ all:[u_ri_path, u_top_level_goal, u_bottom_goal],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ri_path, u_top_level_goal, u_bottom_goal],
			 opt:0,
			 req:[u_ri_path, u_top_level_goal, u_bottom_goal],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RI-PATH->EPISODE0 
wl: init_args(exact_only, u_ri_path_c62_episode0).


% annotating U::RI-PATH->EPISODE0 
f_u_ri_path_c62_episode0(Ri_path_Param, Top_level_goal_Param, Bottom_goal_Param, FnResult) :-
	Env=[bv(u_ri_path, Ri_path_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_bottom_goal, Bottom_goal_Param)],
	f_u_cx_c36_create(Context_Init),
	LEnv=[[bv(u_context, Context_Init)]|Env],
	f_u_ri_pathelt_rule([car, u_ri_path], Top_rule_Init),
	LEnv19=[[bv(u_top_rule, Top_rule_Init)]|LEnv],
	f_u_ob_c36_unify([u_ob_c36_get, u_top_rule, [quote, u_goal]],
			 [u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
			 u_xx_empty_me_bd_xx,
			 Bd_Init),
	Env=[[bv(u_bd, Bd_Init)]|LEnv19],
	f_u_ob_c36_fcreate(
			   [ '#BQ',
			     
			     [ u_active_goal,
			       u_obj,
			       
			       [ '#COMMA',
				 [u_ob_c36_get, u_top_level_goal, [quote, u_obj]]
			       ],
			       u_activation_context,
			       ['#COMMA', u_context]
			     ]
			   ],
			   Top_level_goal),
	set_var(Env, u_top_level_goal, Top_level_goal),
	get_var(Env, u_top_level_goal, Top_level_goal_Get27),
	f_u_ob_c36_set(Top_level_goal_Get27,
		       u_top_level_goal,
		       Top_level_goal_Get27,
		       C36_set_Ret),
	get_var(Env, u_context, Context_Get),
	get_var(Env, u_top_level_goal, Top_level_goal_Get29),
	f_u_cx_c36_assert(Context_Get, Top_level_goal_Get29, C36_assert_Ret),
	get_var(Env, u_bd, Bd_Get),
	get_var(Env, u_context, Context_Get32),
	get_var(Env, u_top_level_goal, Top_level_goal_Get31),
	f_u_ri_path_c62_episode1(Ri_path_Param,
				 Top_level_goal_Get31,
				 Context_Get32,
				 Bd_Get,
				 [],
				 Top_level_goal_Get31,
				 Bottom_goal_Param,
				 C62_episode1_Ret),
	LetResult=C62_episode1_Ret,
	LetResult=FnResult.
:- set_opv(f_u_ri_path_c62_episode0, classof, claz_function),
   set_opv(u_ri_path_c62_episode0, compile_as, kw_function),
   set_opv(u_ri_path_c62_episode0, function, f_u_ri_path_c62_episode0),
   DefunResult=u_ri_path_c62_episode0.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:21891 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 22585)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:21891 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This function must work for inferences as well as plans? When?",
				     1,
				     22587)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:21891 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 22652)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ri-path->episode1',
			    
			    [ 'ri-path',
			      goal,
			      context,
			      bd,
			      'ep-goal',
			      'top-level-goal',
			      'bottom-goal'
			    ],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      'seren-long',
			      '$STRING'("ri-path->episode1 ~A ~A ~A ~A ~A ~A ~A"),
			      'ri-path',
			      goal,
			      context,
			      bd,
			      'ep-goal',
			      'top-level-goal',
			      'bottom-goal'
			    ],
			    
			    [ 'let*',
			      
			      [ [pathelt, [car, 'ri-path']],
				[rule, ['ri-pathelt-rule', pathelt]],
				['subgoal-objs', ['rule-subgoal-objs', rule]],
				[subgoalnum, ['ri-pathelt-subgoalnum', pathelt]],
				
				[ episode,
				  [car, ['ri-pathelt-episodes', pathelt]]
				],
				['ep-subgoals', []],
				[subgoals, []],
				['result-ep', []],
				[temp, []],
				['nth-subgoal', []]
			      ],
			      
			      [ if,
				episode,
				
				[ setq,
				  'ep-goal',
				  ['ob$get', episode, [quote, goal]]
				]
			      ],
			      
			      [ if,
				'ep-goal',
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    'seren-long',
				    '$STRING'("ep-goal")
				  ],
				  
				  [ setq,
				    'ep-subgoals',
				    
				    [ 'goal-subgoals',
				      'ep-goal',
				      
				      [ 'ob$get',
					'ep-goal',
					[quote, 'top-context']
				      ],
				      '*me-belief-path*'
				    ]
				  ],
				  
				  [ if,
				    
				    [ and,
				      ['null?', [cdr, 'ri-path']],
				      'bottom-goal'
				    ],
				    
				    [ progn,
				      
				      [ if,
					['null?', subgoalnum],
					
					[ progn,
					  
					  [ error,
					    '$STRING'("subgoalnum is nil")
					  ],
					  [setq, subgoalnum, 0]
					]
				      ],
				      
				      [ setf,
					['nth-elem', 'ep-subgoals', subgoalnum],
					'bottom-goal'
				      ],
				      
				      [ setq,
					bd,
					
					[ 'episodic-unify',
					  ['ob$get', rule, [quote, goal]],
					  'subgoal-objs',
					  ['ob$get', 'ep-goal', [quote, obj]],
					  'ep-subgoals',
					  goal,
					  context,
					  bd,
					  '*me-belief-path*',
					  [],
					  'top-level-goal',
					  []
					]
				      ]
				    ],
				    
				    [ setq,
				      bd,
				      
				      [ 'episodic-unify',
					['ob$get', rule, [quote, goal]],
					'subgoal-objs',
					['ob$get', 'ep-goal', [quote, obj]],
					'ep-subgoals',
					goal,
					context,
					bd,
					'*me-belief-path*',
					t,
					'top-level-goal',
					[]
				      ]
				    ]
				  ]
				],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    'seren-long',
				    '$STRING'("no ep-goal")
				  ],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    'seren-long',
				    '$STRING'("post bd ~A"),
				    bd
				  ],
				  
				  [ if,
				    
				    [ and,
				      bd,
				      ['null?', [cdr, 'ri-path']],
				      'bottom-goal'
				    ],
				    
				    [ let,
				      
				      [ 
					[ 'source-bd',
					  
					  [ 'ob$unify',
					    
					    [ 'nth-elem',
					      'subgoal-objs',
					      subgoalnum
					    ],
					    
					    [ 'ob$get',
					      'bottom-goal',
					      [quote, obj]
					    ],
					    bd
					  ]
					]
				      ],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					'seren-long',
					'$STRING'("source bd = ~A"),
					'source-bd'
				      ],
				      
				      [ if,
					'source-bd',
					
					[ setq,
					  bd,
					  
					  [ 'bd-special-append',
					    bd,
					    'source-bd',
					    goal,
					    context,
					    '*me-belief-path*',
					    'top-level-goal',
					    [],
					    [lambda, [x], t]
					  ]
					],
					[setq, bd, []]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				[and, bd, ['ob?', [car, bd]]],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    'seren-long',
				    '$STRING'("Resetting goal from ~A to ~A"),
				    goal,
				    [car, bd]
				  ],
				  [setq, goal, [car, bd]]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'seren-long',
				'$STRING'("bd = ~A"),
				bd
			      ],
			      
			      [ if,
				['null?', bd],
				[],
				
				[ progn,
				  
				  [ setq,
				    subgoals,
				    
				    [ 'instan-and-activate-subgoals',
				      goal,
				      'subgoal-objs',
				      bd,
				      rule,
				      context,
				      
				      [ 'ty$instance?',
					['ob$get', rule, [quote, subgoal]],
					[quote, rseq]
				      ],
				      [],
				      [],
				      'top-level-goal',
				      '*me-belief-path*'
				    ]
				  ],
				  
				  [ setq,
				    'result-ep',
				    ['make-episode', rule, goal, context, [], []]
				  ],
				  ['ob$set', 'result-ep', [quote, 'seren-ep'], t],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    'seren-long',
				    '$STRING'("Ep ~A for goal ~A"),
				    'result-ep',
				    goal
				  ],
				  
				  [ if,
				    [cdr, 'ri-path'],
				    
				    [ progn,
				      
				      [ setq,
					'nth-subgoal',
					['nth-elem', subgoals, subgoalnum]
				      ],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					'seren-long',
					'$STRING'("Unify ~A and ~A"),
					
					[ 'ob$get',
					  ['ri-pathelt-rule', [cadr, 'ri-path']],
					  [quote, goal]
					],
					['ob$get', 'nth-subgoal', [quote, obj]]
				      ],
				      
				      [ setq,
					bd,
					
					[ 'ob$unify',
					  
					  [ 'ob$get',
					    
					    [ 'ri-pathelt-rule',
					      [cadr, 'ri-path']
					    ],
					    [quote, goal]
					  ],
					  ['ob$get', 'nth-subgoal', [quote, obj]],
					  bd
					]
				      ],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					'seren-long',
					'$STRING'("bd now = ~A"),
					bd
				      ],
				      
				      [ if,
					['null?', bd],
					[],
					
					[ progn,
					  
					  [ if,
					    
					    [ 'vars-in?',
					      
					      [ 'ob$get',
						'nth-subgoal',
						[quote, obj]
					      ]
					    ],
					    
					    [ setq,
					      'nth-subgoal',
					      
					      [ or,
						
						[ 'plan-instantiate',
						  'nth-subgoal',
						  ['bd-no-var-bds', bd],
						  context,
						  'top-level-goal',
						  '*me-belief-path*',
						  []
						],
						'nth-subgoal'
					      ]
					    ]
					  ],
					  
					  [ if,
					    
					    [ setq,
					      temp,
					      
					      [ 'ri-path->episode1',
						[cdr, 'ri-path'],
						'nth-subgoal',
						context,
						bd,
						
						[ if,
						  'ep-subgoals',
						  
						  [ 'nth-elem',
						    'ep-subgoals',
						    subgoalnum
						  ],
						  []
						],
						'top-level-goal',
						'bottom-goal'
					      ]
					    ],
					    
					    [ if,
					      episode,
					      
					      [ cons,
						[cons, episode, [car, temp]],
						'result-ep'
					      ],
					      [cons, [car, temp], 'result-ep']
					    ],
					    []
					  ]
					]
				      ]
				    ],
				    [cons, [], 'result-ep']
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::RI-PATH->EPISODE1 
wl: lambda_def(defun,
	      u_ri_path_c62_episode1,
	      f_u_ri_path_c62_episode1,
	      
	      [ u_ri_path,
		u_goal,
		u_context,
		u_bd,
		u_ep_goal,
		u_top_level_goal,
		u_bottom_goal
	      ],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_seren_long,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(r),
			     #\(i),
			     #\(-),
			     #\(p),
			     #\(a),
			     #\(t),
			     #\(h),
			     #\(-),
			     #\(>),
			     #\(e),
			     #\(p),
			     #\(i),
			     #\(s),
			     #\(o),
			     #\(d),
			     #\(e),
			     #\('1'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_ri_path,
		  u_goal,
		  u_context,
		  u_bd,
		  u_ep_goal,
		  u_top_level_goal,
		  u_bottom_goal
		],
		
		[ let_xx,
		  
		  [ [u_pathelt, [car, u_ri_path]],
		    [u_rule, [u_ri_pathelt_rule, u_pathelt]],
		    [u_subgoal_objs, [u_rule_subgoal_objs, u_rule]],
		    [u_subgoalnum, [u_ri_pathelt_subgoalnum, u_pathelt]],
		    [u_episode, [car, [u_ri_pathelt_episodes, u_pathelt]]],
		    [u_ep_subgoals, []],
		    [u_subgoals, []],
		    [u_result_ep, []],
		    [u_temp, []],
		    [u_nth_subgoal, []]
		  ],
		  
		  [ if,
		    u_episode,
		    [setq, u_ep_goal, [u_ob_c36_get, u_episode, [quote, u_goal]]]
		  ],
		  
		  [ if,
		    u_ep_goal,
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_seren_long,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(e), #\(p), #\(-), #\(g), #\(o), #\(a), #\(l)])
		      ],
		      
		      [ setq,
			u_ep_subgoals,
			
			[ u_goal_subgoals,
			  u_ep_goal,
			  [u_ob_c36_get, u_ep_goal, [quote, u_top_context]],
			  u_xx_me_belief_path_xx
			]
		      ],
		      
		      [ if,
			[and, [u_null_c63, [cdr, u_ri_path]], u_bottom_goal],
			
			[ progn,
			  
			  [ if,
			    [u_null_c63, u_subgoalnum],
			    
			    [ progn,
			      
			      [ error,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(s),
					   #\(u),
					   #\(b),
					   #\(g),
					   #\(o),
					   #\(a),
					   #\(l),
					   #\(n),
					   #\(u),
					   #\(m),
					   #\(' '),
					   #\(i),
					   #\(s),
					   #\(' '),
					   #\(n),
					   #\(i),
					   #\(l)
					 ])
			      ],
			      [setq, u_subgoalnum, 0]
			    ]
			  ],
			  
			  [ setf,
			    [u_nth_elem, u_ep_subgoals, u_subgoalnum],
			    u_bottom_goal
			  ],
			  
			  [ setq,
			    u_bd,
			    
			    [ u_episodic_unify,
			      [u_ob_c36_get, u_rule, [quote, u_goal]],
			      u_subgoal_objs,
			      [u_ob_c36_get, u_ep_goal, [quote, u_obj]],
			      u_ep_subgoals,
			      u_goal,
			      u_context,
			      u_bd,
			      u_xx_me_belief_path_xx,
			      [],
			      u_top_level_goal,
			      []
			    ]
			  ]
			],
			
			[ setq,
			  u_bd,
			  
			  [ u_episodic_unify,
			    [u_ob_c36_get, u_rule, [quote, u_goal]],
			    u_subgoal_objs,
			    [u_ob_c36_get, u_ep_goal, [quote, u_obj]],
			    u_ep_subgoals,
			    u_goal,
			    u_context,
			    u_bd,
			    u_xx_me_belief_path_xx,
			    t,
			    u_top_level_goal,
			    []
			  ]
			]
		      ]
		    ],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_seren_long,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(n),
				   #\(o),
				   #\(' '),
				   #\(e),
				   #\(p),
				   #\(-),
				   #\(g),
				   #\(o),
				   #\(a),
				   #\(l)
				 ])
		      ],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_seren_long,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(p),
				   #\(o),
				   #\(s),
				   #\(t),
				   #\(' '),
				   #\(b),
				   #\(d),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_bd
		      ],
		      
		      [ if,
			[and, u_bd, [u_null_c63, [cdr, u_ri_path]], u_bottom_goal],
			
			[ let,
			  
			  [ 
			    [ u_source_bd,
			      
			      [ u_ob_c36_unify,
				[u_nth_elem, u_subgoal_objs, u_subgoalnum],
				[u_ob_c36_get, u_bottom_goal, [quote, u_obj]],
				u_bd
			      ]
			    ]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_seren_long,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(s),
				       #\(o),
				       #\(u),
				       #\(r),
				       #\(c),
				       #\(e),
				       #\(' '),
				       #\(b),
				       #\(d),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_source_bd
			  ],
			  
			  [ if,
			    u_source_bd,
			    
			    [ setq,
			      u_bd,
			      
			      [ u_bd_special_append,
				u_bd,
				u_source_bd,
				u_goal,
				u_context,
				u_xx_me_belief_path_xx,
				u_top_level_goal,
				[],
				[lambda, [u_x], t]
			      ]
			    ],
			    [setq, u_bd, []]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [and, u_bd, [u_ob_c63, [car, u_bd]]],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_seren_long,
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
			u_goal,
			[car, u_bd]
		      ],
		      [setq, u_goal, [car, u_bd]]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_seren_long,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(b), #\(d), #\(' '), #\(=), #\(' '), #\(~), #\('A')]),
		    u_bd
		  ],
		  
		  [ if,
		    [u_null_c63, u_bd],
		    [],
		    
		    [ progn,
		      
		      [ setq,
			u_subgoals,
			
			[ u_instan_and_activate_subgoals,
			  u_goal,
			  u_subgoal_objs,
			  u_bd,
			  u_rule,
			  u_context,
			  
			  [ u_ty_c36_instance_c63,
			    [u_ob_c36_get, u_rule, [quote, u_subgoal]],
			    [quote, u_rseq]
			  ],
			  [],
			  [],
			  u_top_level_goal,
			  u_xx_me_belief_path_xx
			]
		      ],
		      
		      [ setq,
			u_result_ep,
			[u_make_episode, u_rule, u_goal, u_context, [], []]
		      ],
		      [u_ob_c36_set, u_result_ep, [quote, u_seren_ep], t],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_seren_long,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('E'),
				   #\(p),
				   #\(' '),
				   #\(~),
				   #\('A'),
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
			u_result_ep,
			u_goal
		      ],
		      
		      [ if,
			[cdr, u_ri_path],
			
			[ progn,
			  
			  [ setq,
			    u_nth_subgoal,
			    [u_nth_elem, u_subgoals, u_subgoalnum]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_seren_long,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('U'),
				       #\(n),
				       #\(i),
				       #\(f),
				       #\(y),
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
			    
			    [ u_ob_c36_get,
			      [u_ri_pathelt_rule, [cadr, u_ri_path]],
			      [quote, u_goal]
			    ],
			    [u_ob_c36_get, u_nth_subgoal, [quote, u_obj]]
			  ],
			  
			  [ setq,
			    u_bd,
			    
			    [ u_ob_c36_unify,
			      
			      [ u_ob_c36_get,
				[u_ri_pathelt_rule, [cadr, u_ri_path]],
				[quote, u_goal]
			      ],
			      [u_ob_c36_get, u_nth_subgoal, [quote, u_obj]],
			      u_bd
			    ]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_seren_long,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(b),
				       #\(d),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(w),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_bd
			  ],
			  
			  [ if,
			    [u_null_c63, u_bd],
			    [],
			    
			    [ progn,
			      
			      [ if,
				
				[ u_vars_in_c63,
				  [u_ob_c36_get, u_nth_subgoal, [quote, u_obj]]
				],
				
				[ setq,
				  u_nth_subgoal,
				  
				  [ or,
				    
				    [ u_plan_instantiate,
				      u_nth_subgoal,
				      [u_bd_no_var_bds, u_bd],
				      u_context,
				      u_top_level_goal,
				      u_xx_me_belief_path_xx,
				      []
				    ],
				    u_nth_subgoal
				  ]
				]
			      ],
			      
			      [ if,
				
				[ setq,
				  u_temp,
				  
				  [ u_ri_path_c62_episode1,
				    [cdr, u_ri_path],
				    u_nth_subgoal,
				    u_context,
				    u_bd,
				    
				    [ if,
				      u_ep_subgoals,
				      [u_nth_elem, u_ep_subgoals, u_subgoalnum],
				      []
				    ],
				    u_top_level_goal,
				    u_bottom_goal
				  ]
				],
				
				[ if,
				  u_episode,
				  
				  [ cons,
				    [cons, u_episode, [car, u_temp]],
				    u_result_ep
				  ],
				  [cons, [car, u_temp], u_result_ep]
				],
				[]
			      ]
			    ]
			  ]
			],
			[cons, [], u_result_ep]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::RI-PATH->EPISODE1 
wl: arglist_info(u_ri_path_c62_episode1,
		
		[ u_ri_path,
		  u_goal,
		  u_context,
		  u_bd,
		  u_ep_goal,
		  u_top_level_goal,
		  u_bottom_goal
		],
		
		[ Ri_path_Param,
		  Goal_Param,
		  Context_Param,
		  Bd_Param,
		  Ep_goal_Param,
		  Top_level_goal_Param,
		  Bottom_goal_Param
		],
		arginfo{ all:
			     [ u_ri_path,
			       u_goal,
			       u_context,
			       u_bd,
			       u_ep_goal,
			       u_top_level_goal,
			       u_bottom_goal
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ri_path,
				 u_goal,
				 u_context,
				 u_bd,
				 u_ep_goal,
				 u_top_level_goal,
				 u_bottom_goal
			       ],
			 opt:0,
			 req:
			     [ u_ri_path,
			       u_goal,
			       u_context,
			       u_bd,
			       u_ep_goal,
			       u_top_level_goal,
			       u_bottom_goal
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RI-PATH->EPISODE1 
wl: init_args(exact_only, u_ri_path_c62_episode1).


% annotating U::RI-PATH->EPISODE1 
f_u_ri_path_c62_episode1(Ri_path_Param, Goal_Param, Context_Param, Bd_Param, Ep_goal_Param, Top_level_goal_Param, Bottom_goal_Param, TrueResult187) :-
	Env=[bv(u_ri_path, Ri_path_Param), bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_bd, Bd_Param), bv(u_ep_goal, Ep_goal_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_bottom_goal, Bottom_goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(r),
				       #\(i),
				       #\(-),
				       #\(p),
				       #\(a),
				       #\(t),
				       #\(h),
				       #\(-),
				       #\(>),
				       #\(e),
				       #\(p),
				       #\(i),
				       #\(s),
				       #\(o),
				       #\(d),
				       #\(e),
				       #\('1'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ri_path,
			    u_goal,
			    u_context,
			    u_bd,
			    u_ep_goal,
			    u_top_level_goal,
			    u_bottom_goal
			  ],
			  Roman_nl_Ret),
	cl_car(Ri_path_Param, Pathelt_Init),
	LEnv=[[bv(u_pathelt, Pathelt_Init)]|Env],
	f_u_ri_pathelt_rule(u_pathelt, Rule_Init),
	Env=[[bv(u_rule, Rule_Init)]|LEnv],
	get_var(Env, u_rule, Rule_Get),
	f_u_rule_subgoal_objs(Rule_Get, Subgoal_objs_Init),
	LEnv31=[[bv(u_subgoal_objs, Subgoal_objs_Init)]|Env],
	f_u_ri_pathelt_subgoalnum(u_pathelt, Subgoalnum_Init),
	LEnv36=[[bv(u_subgoalnum, Subgoalnum_Init)]|LEnv31],
	f_u_ri_pathelt_episodes(u_pathelt, Car_Param),
	cl_car(Car_Param, Episode_Init),
	LEnv39=[[bv(u_episode, Episode_Init)]|LEnv36],
	LEnv42=[[bv(u_ep_subgoals, [])]|LEnv39],
	LEnv44=[[bv(u_subgoals, [])]|LEnv42],
	LEnv46=[[bv(u_result_ep, [])]|LEnv44],
	LEnv48=[[bv(u_temp, [])]|LEnv46],
	Setf_Env=[[bv(u_nth_subgoal, [])]|LEnv48],
	get_var(Setf_Env, u_episode, IFTEST),
	(   IFTEST\==[]
	->  get_var(Setf_Env, u_episode, Episode_Get56),
	    f_u_ob_c36_get(Episode_Get56, u_goal, TrueResult),
	    set_var(Setf_Env, u_ep_goal, TrueResult),
	    _107762=TrueResult
	;   _107762=[]
	),
	get_var(Setf_Env, u_ep_goal, IFTEST58),
	(   IFTEST58\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_seren_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(e),
					   #\(p),
					   #\(-),
					   #\(g),
					   #\(o),
					   #\(a),
					   #\(l)
					 ])
			      ],
			      Roman_nl_Ret221),
	    get_var(Setf_Env, u_ep_goal, Ep_goal_Get61),
	    f_u_ob_c36_get(Ep_goal_Get61, u_top_context, Top_context),
	    get_var(Setf_Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    f_u_goal_subgoals(Ep_goal_Get61,
			      Top_context,
			      Xx_me_belief_path_xx_Get,
			      Ep_subgoals),
	    set_var(Setf_Env, u_ep_subgoals, Ep_subgoals),
	    f_u_null_c63([cdr, u_ri_path], IFTEST66),
	    (   IFTEST66\==[]
	    ->  IFTEST64=Bottom_goal_Param
	    ;   IFTEST64=[]
	    ),
	    (   IFTEST64\==[]
	    ->  f_u_null_c63(u_subgoalnum, IFTEST70),
		(   IFTEST70\==[]
		->  cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					
					[ #\(s),
					  #\(u),
					  #\(b),
					  #\(g),
					  #\(o),
					  #\(a),
					  #\(l),
					  #\(n),
					  #\(u),
					  #\(m),
					  #\(' '),
					  #\(i),
					  #\(s),
					  #\(' '),
					  #\(n),
					  #\(i),
					  #\(l)
					])
			     ],
			     Error_Ret),
		    set_var(Setf_Env, setq, u_subgoalnum, 0),
		    _108308=0
		;   _108308=[]
		),
		get_var(Setf_Env, u_ep_subgoals, Ep_subgoals_Get80),
		get_var(Setf_Env, u_subgoalnum, Subgoalnum_Get),
		set_place(Setf_Env,
			  setf,
			  [u_nth_elem, Ep_subgoals_Get80, Subgoalnum_Get],
			  [Bottom_goal_Param],
			  Setf_R),
		get_var(Setf_Env, u_rule, Rule_Get77),
		f_u_ob_c36_get(Rule_Get77, u_goal, Goal),
		get_var(Setf_Env, u_ep_goal, Ep_goal_Get79),
		get_var(Setf_Env, u_subgoal_objs, Subgoal_objs_Get),
		f_u_ob_c36_get(Ep_goal_Get79, u_obj, Obj),
		get_var(Setf_Env, u_bd, Bd_Get),
		get_var(Setf_Env,
			u_xx_me_belief_path_xx,
			Xx_me_belief_path_xx_Get84),
		f_u_episodic_unify(Goal,
				   Subgoal_objs_Get,
				   Obj,
				   Ep_subgoals_Get80,
				   Goal_Param,
				   Context_Param,
				   Bd_Get,
				   Xx_me_belief_path_xx_Get84,
				   [],
				   Top_level_goal_Param,
				   [],
				   TrueResult95),
		set_var(Setf_Env, u_bd, TrueResult95),
		TrueResult125=TrueResult95
	    ;   get_var(Setf_Env, u_rule, Rule_Get86),
		f_u_ob_c36_get(Rule_Get86, u_goal, Goal208),
		get_var(Setf_Env, u_ep_goal, Ep_goal_Get88),
		get_var(Setf_Env, u_subgoal_objs, Subgoal_objs_Get87),
		f_u_ob_c36_get(Ep_goal_Get88, u_obj, Obj209),
		get_var(Setf_Env, u_bd, Bd_Get92),
		get_var(Setf_Env,
			u_xx_me_belief_path_xx,
			Xx_me_belief_path_xx_Get93),
		f_u_episodic_unify(Goal208,
				   Subgoal_objs_Get87,
				   Obj209,
				   Ep_subgoals_Get80,
				   Goal_Param,
				   Context_Param,
				   Bd_Get92,
				   Xx_me_belief_path_xx_Get93,
				   t,
				   Top_level_goal_Param,
				   [],
				   ElseResult),
		set_var(Setf_Env, u_bd, ElseResult),
		TrueResult125=ElseResult
	    )
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_seren_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(n),
					   #\(o),
					   #\(' '),
					   #\(e),
					   #\(p),
					   #\(-),
					   #\(g),
					   #\(o),
					   #\(a),
					   #\(l)
					 ])
			      ],
			      Roman_nl_Ret223),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_seren_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(p),
					   #\(o),
					   #\(s),
					   #\(t),
					   #\(' '),
					   #\(b),
					   #\(d),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_bd
			      ],
			      Roman_nl_Ret224),
	    get_var(Setf_Env, u_bd, IFTEST99),
	    (   IFTEST99\==[]
	    ->  f_u_null_c63([cdr, u_ri_path], IFTEST102),
		(   IFTEST102\==[]
		->  IFTEST97=Bottom_goal_Param
		;   IFTEST97=[]
		)
	    ;   IFTEST97=[]
	    ),
	    (   IFTEST97\==[]
	    ->  f_u_ob_c36_unify([u_nth_elem, u_subgoal_objs, u_subgoalnum],
				 [u_ob_c36_get, u_bottom_goal, [quote, u_obj]],
				 u_bd,
				 Source_bd_Init),
		Env=[[bv(u_source_bd, Source_bd_Init)]|Setf_Env],
		f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_seren_long,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(s),
					       #\(o),
					       #\(u),
					       #\(r),
					       #\(c),
					       #\(e),
					       #\(' '),
					       #\(b),
					       #\(d),
					       #\(' '),
					       #\(=),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_source_bd
				  ],
				  Roman_nl_Ret225),
		get_var(Env, u_source_bd, IFTEST110),
		(   IFTEST110\==[]
		->  get_var(Env, u_bd, Bd_Get114),
		    get_var(Env, u_source_bd, Source_bd_Get115),
		    get_var(Env,
			    u_xx_me_belief_path_xx,
			    Xx_me_belief_path_xx_Get118),
		    Lambda=closure([ClosureEnvironment|Env], t, [u_x], true),
		    f_u_bd_special_append(Bd_Get114,
					  Source_bd_Get115,
					  Goal_Param,
					  Context_Param,
					  Xx_me_belief_path_xx_Get118,
					  Top_level_goal_Param,
					  [],
					  Lambda,
					  TrueResult122),
		    set_var(Env, u_bd, TrueResult122),
		    TrueResult125=TrueResult122
		;   set_var(Env, setq, u_bd, []),
		    TrueResult125=[]
		)
	    ;   TrueResult125=[]
	    )
	),
	get_var(Setf_Env, u_bd, IFTEST129),
	(   IFTEST129\==[]
	->  f_u_ob_c63([car, u_bd], TrueResult132),
	    IFTEST127=TrueResult132
	;   IFTEST127=[]
	),
	(   IFTEST127\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_seren_long,
			      
			      [ '$ARRAY'([*],
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
				u_goal,
				[car, u_bd]
			      ],
			      Roman_nl_Ret226),
	    get_var(Setf_Env, u_bd, Bd_Get133),
	    cl_car(Bd_Get133, TrueResult134),
	    set_var(Setf_Env, u_goal, TrueResult134),
	    _109836=TrueResult134
	;   _109836=[]
	),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_seren_long,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(b),
				       #\(d),
				       #\(' '),
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_bd
			  ],
			  Roman_nl_Ret227),
	f_u_null_c63(u_bd, IFTEST135),
	(   IFTEST135\==[]
	->  TrueResult187=[]
	;   get_var(Setf_Env, u_bd, Bd_Get139),
	    get_var(Setf_Env, u_goal, Goal_Get137),
	    get_var(Setf_Env, u_rule, Rule_Get140),
	    get_var(Setf_Env, u_subgoal_objs, Subgoal_objs_Get138),
	    f_u_ob_c36_get(Rule_Get140, u_subgoal, Subgoal),
	    f_u_ty_c36_instance_c63(Subgoal, u_rseq, Rseq),
	    get_var(Setf_Env,
		    u_xx_me_belief_path_xx,
		    Xx_me_belief_path_xx_Get144),
	    f_u_instan_and_activate_subgoals(Goal_Get137,
					     Subgoal_objs_Get138,
					     Bd_Get139,
					     Rule_Get140,
					     Context_Param,
					     Rseq,
					     [],
					     [],
					     Top_level_goal_Param,
					     Xx_me_belief_path_xx_Get144,
					     Subgoals),
	    set_var(Setf_Env, u_subgoals, Subgoals),
	    get_var(Setf_Env, u_goal, Goal_Get146),
	    get_var(Setf_Env, u_rule, Rule_Get145),
	    f_u_make_episode(Rule_Get145,
			     Goal_Get146,
			     Context_Param,
			     [],
			     [],
			     Result_ep),
	    set_var(Setf_Env, u_result_ep, Result_ep),
	    get_var(Setf_Env, u_result_ep, Result_ep_Get182),
	    f_u_ob_c36_set(Result_ep_Get182, u_seren_ep, t, T),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_seren_long,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('E'),
					   #\(p),
					   #\(' '),
					   #\(~),
					   #\('A'),
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
				u_result_ep,
				u_goal
			      ],
			      Roman_nl_Ret228),
	    cl_cdr(Ri_path_Param, IFTEST149),
	    (   IFTEST149\==[]
	    ->  f_u_nth_elem(u_subgoals, u_subgoalnum, Subgoalnum),
		set_var(Setf_Env, u_nth_subgoal, Subgoalnum),
		f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_seren_long,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('U'),
					       #\(n),
					       #\(i),
					       #\(f),
					       #\(y),
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
				    
				    [ u_ob_c36_get,
				      [u_ri_pathelt_rule, [cadr, u_ri_path]],
				      [quote, u_goal]
				    ],
				    [u_ob_c36_get, u_nth_subgoal, [quote, u_obj]]
				  ],
				  Roman_nl_Ret229),
		f_u_ob_c36_unify(
				 [ u_ob_c36_get,
				   [u_ri_pathelt_rule, [cadr, u_ri_path]],
				   [quote, u_goal]
				 ],
				 [u_ob_c36_get, u_nth_subgoal, [quote, u_obj]],
				 u_bd,
				 Bd),
		set_var(Setf_Env, u_bd, Bd),
		f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_seren_long,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(b),
					       #\(d),
					       #\(' '),
					       #\(n),
					       #\(o),
					       #\(w),
					       #\(' '),
					       #\(=),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    u_bd
				  ],
				  Roman_nl_Ret230),
		f_u_null_c63(u_bd, IFTEST152),
		(   IFTEST152\==[]
		->  TrueResult187=[]
		;   get_var(Setf_Env, u_nth_subgoal, Nth_subgoal_Get),
		    f_u_ob_c36_get(Nth_subgoal_Get, u_obj, Obj217),
		    f_u_vars_in_c63(Obj217, IFTEST154),
		    (   IFTEST154\==[]
		    ->  (   get_var(Setf_Env, u_bd, Bd_Get158),
			    get_var(Setf_Env, u_nth_subgoal, Nth_subgoal_Get157),
			    f_u_bd_no_var_bds(Bd_Get158, Var_bds_Ret),
			    get_var(Setf_Env,
				    u_xx_me_belief_path_xx,
				    Xx_me_belief_path_xx_Get161),
			    f_u_plan_instantiate(Nth_subgoal_Get157,
						 Var_bds_Ret,
						 Context_Param,
						 Top_level_goal_Param,
						 Xx_me_belief_path_xx_Get161,
						 [],
						 FORM1_Res),
			    FORM1_Res\==[],
			    TrueResult164=FORM1_Res
			->  true
			;   get_var(Setf_Env, u_nth_subgoal, Nth_subgoal_Get162),
			    TrueResult164=Nth_subgoal_Get162
			),
			set_var(Setf_Env, u_nth_subgoal, TrueResult164),
			_110784=TrueResult164
		    ;   _110784=[]
		    ),
		    cl_cdr(Ri_path_Param, C62_episode1_Param),
		    get_var(Setf_Env, u_bd, Bd_Get170),
		    get_var(Setf_Env, u_nth_subgoal, Nth_subgoal_Get168),
		    (   Ep_subgoals_Get80\==[]
		    ->  f_u_nth_elem(u_ep_subgoals, u_subgoalnum, TrueResult174),
			_111500=TrueResult174
		    ;   _111500=[]
		    ),
		    f_u_ri_path_c62_episode1(C62_episode1_Param,
					     Nth_subgoal_Get168,
					     Context_Param,
					     Bd_Get170,
					     _111500,
					     Top_level_goal_Param,
					     Bottom_goal_Param,
					     IFTEST165),
		    set_var(Setf_Env, u_temp, IFTEST165),
		    (   IFTEST165\==[]
		    ->  get_var(Setf_Env, u_episode, IFTEST177),
			(   IFTEST177\==[]
			->  get_var(Setf_Env, u_episode, Episode_Get180),
			    get_var(Setf_Env, u_temp, Temp_Get183),
			    cl_car(Temp_Get183, Car_Ret),
			    CAR=[Episode_Get180|Car_Ret],
			    TrueResult187=[CAR|Result_ep_Get182]
			;   cl_car(Temp_Get183, Car_Ret234),
			    TrueResult187=[Car_Ret234|Result_ep_Get182]
			)
		    ;   TrueResult187=[]
		    )
		)
	    ;   TrueResult187=[[]|Result_ep_Get182]
	    )
	).
:- set_opv(f_u_ri_path_c62_episode1, classof, claz_function),
   set_opv(u_ri_path_c62_episode1, compile_as, kw_function),
   set_opv(u_ri_path_c62_episode1, function, f_u_ri_path_c62_episode1),
   DefunResult=u_ri_path_c62_episode1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Since a SUCCEEDED-GOAL is reasserted upon success,",
				     5,
				     23409)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" taking the top-context of that goal will get us a good",
				     5,
				     23466)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" context for finding subgoals. I hope. Otherwise,",
				     5,
				     23527)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" we are going to have to pass in the last episode.",
				     5,
				     23582)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (ndbg-roman-nl *gate-dbg* seren-long \"ep-sgls = ~A, bd before = ~A\"",
				     1,
				     24019)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                        ep-subgoals bd)",
				     1,
				     24097)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (interest 'unify 'all)",
				     1,
				     24138)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (disinterest 'unify 'all)",
				     1,
				     24429)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (ndbg-roman-nl *gate-dbg* seren-long \"bd after = ~A\" bd)",
				     1,
				     24465)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end block", 12, 24542)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" end first clause of if", 7, 24813)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (ndbg-roman-nl *gate-dbg* seren-long \"pre bd ~A ob1 ~A ob2 ~A\"",
				     1,
				     24909)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                   bd (ob$get rule 'goal) (ob$get goal 'obj))",
				     1,
				     24979)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below was redundant. Moved down.",
				     1,
				     25042)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below line 'verifies' this goal.",
				     1,
				     25081)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (setq bd (ob$unify (ob$get rule 'goal) (ob$get goal 'obj) bd))",
				     1,
				     25120)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below modification enables variable instantiation",
				     1,
				     25191)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" from the goal unification. Iynwim.",
				     1,
				     25247)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Similar to a swatch of code in dd_rule (run-generic-plan).",
				     1,
				     25284)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (if (vars-in? (ob$get goal 'obj))",
				     1,
				     25345)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (setq goal (or (plan-instantiate goal (bd-no-var-bds bd)",
				     1,
				     25387)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                           context top-level-goal",
				     1,
				     25456)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                           *me-belief-path* nil)",
				     1,
				     25523)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                         goal)))",
				     1,
				     25589)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End above", 1, 25623)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below is a mini episodic-unify.",
				     12,
				     25793)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ran into problems with type compatibility when using",
				     17,
				     26028)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" *empty-me-bd* instead of bd above.",
				     17,
				     26099)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("was t", 64, 26436)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (progn", 1, 26504)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (setq bd source-bd)",
				     1,
				     26529)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (setq goal (or (plan-instantiate goal bd",
				     1,
				     26568)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                                  context top-level-goal",
				     1,
				     26628)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                                                                                 *me-belief-path* nil)",
				     1,
				     26702)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                goal)))",
				     1,
				     26775)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end if", 20, 26866)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end let, end if", 20, 26894)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end block", 10, 26921)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" end if", 9, 26941)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was *empty-me-bd* <------------- !",
				     37,
				     28217)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Below is new. (dle)", 18, 28408)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: reality and desirability are ignored here",
				     18,
				     28867)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 29560)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Rule induction", 1, 29562)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:22653 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 29579)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:29628 **********************/
:- lisp_compile_to_prolog(pkg_user, [cdr, concepts]).
:- get_var(TLEnv3, u_concepts, Concepts_Get),
   cl_cdr(Concepts_Get, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:29649 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ let,
			    
			    [ 
			      [ 'pers-attr',
				
				[ any,
				  
				  [ lambda,
				    [con],
				    
				    [ if,
				      
				      [ 'ty$instance?',
					con,
					[quote, 'personal-attribute']
				      ],
				      con,
				      []
				    ]
				  ],
				  concepts
				]
			      ],
			      
			      [ state,
				
				[ any,
				  
				  [ lambda,
				    [con],
				    
				    [ if,
				      
				      [ and,
					['ty$instance?', con, [quote, state]],
					
					[ not,
					  
					  [ 'ty$instance?',
					    con,
					    [quote, 'personal-attribute']
					  ]
					]
				      ],
				      con,
				      []
				    ]
				  ],
				  concepts
				]
			      ]
			    ],
			    
			    [ if,
			      [and, 'pers-attr', state],
			      ['induce-rule', state, [list, 'pers-attr'], context],
			      []
			    ]
			  ]).
:- f_u_any(
	   [ lambda,
	     [u_con],
	     
	     [ if,
	       [u_ty_c36_instance_c63, u_con, [quote, u_personal_attribute]],
	       u_con,
	       []
	     ]
	   ],
	   u_concepts,
	   Pers_attr_Init),
   f_u_any(
	   [ lambda,
	     [u_con],
	     
	     [ if,
	       
	       [ and,
		 [u_ty_c36_instance_c63, u_con, [quote, u_state]],
		 
		 [ not,
		   [u_ty_c36_instance_c63, u_con, [quote, u_personal_attribute]]
		 ]
	       ],
	       u_con,
	       []
	     ]
	   ],
	   u_concepts,
	   State_Init),
   LEnv=[[bv(u_pers_attr, Pers_attr_Init), bv(u_state, State_Init)]|TLEnv3],
   get_var(LEnv, u_pers_attr, IFTEST10),
   (   IFTEST10\==[]
   ->  get_var(LEnv, u_state, State_Get),
       IFTEST=State_Get
   ;   IFTEST=[]
   ),
   (   IFTEST\==[]
   ->  get_var(LEnv, u_pers_attr, Pers_attr_Get17),
       get_var(LEnv, u_state, State_Get16),
       _170714=[Pers_attr_Get17],
       get_var(LEnv, u_context, Context_Get),
       f_u_induce_rule(State_Get16, _170714, Context_Get, TrueResult19),
       _156352=TrueResult19
   ;   _156352=[]
   ),
   LetResult=_156352.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30269 **********************/
:- lisp_compile_to_prolog(pkg_user, []).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30272 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30273 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30276 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*next-irule-number*', 1]).
:- set_var(TLEnv3, setq, u_xx_next_irule_number_xx, 1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30306 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    '*induced-rule-name-genproc*',
			    [],
			    
			    [ 'string->symbol',
			      
			      [ 'string-append',
				'$STRING'("INDUCED-RULE."),
				
				[ prog1,
				  ['fixnum->string', '*next-irule-number*'],
				  ['increment-me', '*next-irule-number*']
				]
			      ]
			    ]
			  ]).

% annotating U::*INDUCED-RULE-NAME-GENPROC* 
wl: lambda_def(defun,
	      u_xx_induced_rule_name_genproc_xx,
	      f_u_xx_induced_rule_name_genproc_xx,
	      [],
	      
	      [ 
		[ u_string_c62_symbol,
		  
		  [ u_string_append,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('I'),
			       #\('N'),
			       #\('D'),
			       #\('U'),
			       #\('C'),
			       #\('E'),
			       #\('D'),
			       #\(-),
			       #\('R'),
			       #\('U'),
			       #\('L'),
			       #\('E'),
			       #\('.')
			     ]),
		    
		    [ prog1,
		      [u_fixnum_c62_string, u_xx_next_irule_number_xx],
		      [u_increment_me, u_xx_next_irule_number_xx]
		    ]
		  ]
		]
	      ]).


% annotating U::*INDUCED-RULE-NAME-GENPROC* 
wl: arglist_info(u_xx_induced_rule_name_genproc_xx,
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

% annotating U::*INDUCED-RULE-NAME-GENPROC* 
wl: init_args(exact_only, u_xx_induced_rule_name_genproc_xx).


% annotating U::*INDUCED-RULE-NAME-GENPROC* 
f_u_xx_induced_rule_name_genproc_xx(FnResult) :-
	Env=[],
	f_u_string_c62_symbol(
			      [ u_string_append,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\('N'),
					   #\('D'),
					   #\('U'),
					   #\('C'),
					   #\('E'),
					   #\('D'),
					   #\(-),
					   #\('R'),
					   #\('U'),
					   #\('L'),
					   #\('E'),
					   #\('.')
					 ]),
				
				[ prog1,
				  
				  [ u_fixnum_c62_string,
				    u_xx_next_irule_number_xx
				  ],
				  [u_increment_me, u_xx_next_irule_number_xx]
				]
			      ],
			      C62_symbol_Ret),
	C62_symbol_Ret=FnResult.
:- set_opv(f_u_xx_induced_rule_name_genproc_xx, classof, claz_function),
   set_opv(u_xx_induced_rule_name_genproc_xx, compile_as, kw_function),
   set_opv(u_xx_induced_rule_name_genproc_xx,
	   function,
	   f_u_xx_induced_rule_name_genproc_xx),
   DefunResult=u_xx_induced_rule_name_genproc_xx.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30306 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" No real need to assert an inference (or plan) in the context, since",
				     1,
				     30525)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30306 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" serendipity will use the rule directly in this case. (?)",
				     1,
				     30595)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30306 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 30654)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30306 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note: an inaccessible rule is created here? Should it be generic?",
				     1,
				     30656)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30306 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The fact of the matter is, if it isn't used in the serendipity, it",
				     1,
				     30724)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30306 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" will never be used in the future.",
				     1,
				     30793)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30828 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'induce-rule',
			    [goal, subgoals, context],
			    
			    [ let,
			      
			      [ 
				[ rule,
				  
				  [ 'plan->rule',
				    goal,
				    subgoals,
				    1.0,
				    '*induced-rule-name-genproc*'
				  ]
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				rule,
				'$STRING'("Induced rule ~A"),
				['ob->string', rule]
			      ],
			      ['run-serendipity', [list, [cons, rule, []]], []],
			      rule
			    ]
			  ]).

% annotating U::INDUCE-RULE 
wl: lambda_def(defun,
	      u_induce_rule,
	      f_u_induce_rule,
	      [u_goal, u_subgoals, u_context],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_rule,
		      
		      [ u_plan_c62_rule,
			u_goal,
			u_subgoals,
			1.0,
			u_xx_induced_rule_name_genproc_xx
		      ]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('I'),
			       #\(n),
			       #\(d),
			       #\(u),
			       #\(c),
			       #\(e),
			       #\(d),
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
		  [u_run_serendipity, [list, [cons, u_rule, []]], []],
		  u_rule
		]
	      ]).


% annotating U::INDUCE-RULE 
wl: arglist_info(u_induce_rule,
		[u_goal, u_subgoals, u_context],
		[Goal_Param, Subgoals_Param, Context_Param],
		arginfo{ all:[u_goal, u_subgoals, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal, u_subgoals, u_context],
			 opt:0,
			 req:[u_goal, u_subgoals, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INDUCE-RULE 
wl: init_args(exact_only, u_induce_rule).


% annotating U::INDUCE-RULE 
f_u_induce_rule(Goal_Param, Subgoals_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_subgoals, Subgoals_Param), bv(u_context, Context_Param)],
	get_var(Env,
		u_xx_induced_rule_name_genproc_xx,
		Xx_induced_rule_name_genproc_xx_Get),
	f_u_plan_c62_rule(Goal_Param,
			  Subgoals_Param,
			  1.0,
			  Xx_induced_rule_name_genproc_xx_Get,
			  Rule_Init),
	LEnv=[[bv(u_rule, Rule_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(d),
				       #\(u),
				       #\(c),
				       #\(e),
				       #\(d),
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
	get_var(LEnv, u_rule, Rule_Get),
	CAR=[Rule_Get],
	Run_serendipity_Param=[CAR],
	f_u_run_serendipity(Run_serendipity_Param, [], Run_serendipity_Ret),
	get_var(LEnv, u_rule, Rule_Get24),
	LetResult=Rule_Get24,
	LetResult=FnResult.
:- set_opv(f_u_induce_rule, classof, claz_function),
   set_opv(u_induce_rule, compile_as, kw_function),
   set_opv(u_induce_rule, function, f_u_induce_rule),
   DefunResult=u_induce_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30828 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 31089)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30828 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; teh best way to immagine the differnce between the 5 AGI models: Cycorp, Daydreamer, OpenCog, GeneticAlgrythems and ML, is thing about",
				     1,
				     31106)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30828 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; the best way to understand the 4 models on AI/AGI would be whatever the methods are used to teach each model a task like multiplication of base 10 numbers (such as how how to carry the digits while doing so)",
				     1,
				     31244)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_ri.cl:30828 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the best way to understand the 4 models on AI/AGI would be whatever the methods are used to teach each model a task like multiplication of base 10 numbers (such as how how to carry the digits while doing so)",
				     0,
				     31245)).
:- true.


% Total time: 14.885 seconds


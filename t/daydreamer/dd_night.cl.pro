
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_night" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:13:11 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" This file contains:", 1, 208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:229 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" DAYDREAMER*: A computer model of night dreaming and overdetermined daydreaming",
				     1,
				     230)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:310 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 311)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:312 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   9/9/88: First version written", 1, 313)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:346 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 12/17/88: Debugged, modified, and wrote English generation routines.",
				     1,
				     347)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:417 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 418)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:419 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     420)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:500 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     502)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:582 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Some utilities", 1, 583)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:599 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     600)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:680 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Turn off ob warnings", 1, 682)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:704 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [disinterest, [quote, 'ob-warn'], [quote, all]]).
:- f_u_disinterest(u_ob_warn, u_all, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:733 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*reality*', ['cx$create']]).
:- f_u_cx_c36_create(_Ignored),
   set_var(TLEnv3, u_xx_reality_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:763 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    pod,
			    [ob],
			    
			    [ if,
			      ['ob?', ob],
			      
			      [ progn,
				['ob$pr', ob, '*gate-dbg*', '*ob-print-options*'],
				[newline, '*gate-dbg*']
			      ],
			      [format, '*gate-dbg*', '$STRING'("~A~%"), ob]
			    ],
			    '*repl-wont-print*'
			  ]).

% annotating U::POD 
wl: lambda_def(defun,
	      u_pod,
	      f_u_pod,
	      [u_ob],
	      
	      [ 
		[ if,
		  [u_ob_c63, u_ob],
		  
		  [ progn,
		    
		    [ u_ob_c36_pr,
		      u_ob,
		      u_xx_gate_dbg_xx,
		      u_xx_ob_print_options_xx
		    ],
		    [u_newline, u_xx_gate_dbg_xx]
		  ],
		  
		  [ format,
		    u_xx_gate_dbg_xx,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(~), #\('A'), #\(~), #\('%')]),
		    u_ob
		  ]
		],
		u_xx_repl_wont_print_xx
	      ]).


% annotating U::POD 
wl: arglist_info(u_pod,
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

% annotating U::POD 
wl: init_args(exact_only, u_pod).


% annotating U::POD 
f_u_pod(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_ob_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    get_var(Env, u_xx_ob_print_options_xx, Xx_ob_print_options_xx_Get),
	    f_u_ob_c36_pr(Ob_Param,
			  Xx_gate_dbg_xx_Get,
			  Xx_ob_print_options_xx_Get,
			  C36_pr_Ret),
	    f_u_newline(u_xx_gate_dbg_xx, TrueResult),
	    _82648=TrueResult
	;   get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get17),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get17,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(~), #\('A'), #\(~), #\('%')]),
			Ob_Param
		      ],
		      ElseResult),
	    _82648=ElseResult
	),
	get_var(Env, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get),
	Xx_repl_wont_print_xx_Get=FnResult.
:- set_opv(f_u_pod, classof, claz_function),
   set_opv(u_pod, compile_as, kw_function),
   set_opv(u_pod, function, f_u_pod),
   DefunResult=u_pod.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:763 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     944)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:763 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Top-level functions for DAYDREAMER*",
				     1,
				     1025)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:763 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1063)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:1144 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'daydreamer-star', [], ['run-night-sample']]).

% annotating U::DAYDREAMER-STAR 
wl: lambda_def(defun,
	      u_daydreamer_star,
	      f_u_daydreamer_star,
	      [],
	      [[u_run_night_sample]]).


% annotating U::DAYDREAMER-STAR 
wl: arglist_info(u_daydreamer_star,
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

% annotating U::DAYDREAMER-STAR 
wl: init_args(exact_only, u_daydreamer_star).


% annotating U::DAYDREAMER-STAR 
f_u_daydreamer_star(FnResult) :-
	Env=[],
	f_u_run_night_sample(Night_sample_Ret),
	Night_sample_Ret=FnResult.
:- set_opv(f_u_daydreamer_star, classof, claz_function),
   set_opv(u_daydreamer_star, compile_as, kw_function),
   set_opv(u_daydreamer_star, function, f_u_daydreamer_star),
   DefunResult=u_daydreamer_star.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:1193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-night-sample',
			    [],
			    
			    [ 'run-night',
			      
			      [ list,
				'*test-reversal-goal*',
				'*test-revenge-goal*'
			      ]
			    ]
			  ]).

% annotating U::RUN-NIGHT-SAMPLE 
wl: lambda_def(defun,
	      u_run_night_sample,
	      f_u_run_night_sample,
	      [],
	      
	      [ 
		[ u_run_night,
		  [list, u_xx_test_reversal_goal_xx, u_xx_test_revenge_goal_xx]
		]
	      ]).


% annotating U::RUN-NIGHT-SAMPLE 
wl: arglist_info(u_run_night_sample,
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

% annotating U::RUN-NIGHT-SAMPLE 
wl: init_args(exact_only, u_run_night_sample).


% annotating U::RUN-NIGHT-SAMPLE 
f_u_run_night_sample(FnResult) :-
	Env=[],
	get_var(Env, u_xx_test_revenge_goal_xx, Xx_test_revenge_goal_xx_Get),
	get_var(Env, u_xx_test_reversal_goal_xx, Xx_test_reversal_goal_xx_Get),
	Run_night_Param=[Xx_test_reversal_goal_xx_Get, Xx_test_revenge_goal_xx_Get],
	f_u_run_night(Run_night_Param, Run_night_Ret),
	Run_night_Ret=FnResult.
:- set_opv(f_u_run_night_sample, classof, claz_function),
   set_opv(u_run_night_sample, compile_as, kw_function),
   set_opv(u_run_night_sample, function, f_u_run_night_sample),
   DefunResult=u_run_night_sample.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:1284 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'run-night',
			    ['dd-goals'],
			    
			    [ let,
			      
			      [ ['qplans-es', []],
				['all-rules', []],
				[metaphors, []],
				['metaphor-goal', []],
				['metaphor-qplans']
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				night,
				'$STRING'("----------------------------------------------------------")
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				night,
				'$STRING'("DAYDREAMER* version of 19990506")
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				night,
				'$STRING'("Generating plans for daydreaming goals...")
			      ],
			      
			      [ setq,
				'qplans-es',
				
				[ map,
				  [quote, list],
				  [lambda, [goal], ['qplan-generate', goal]],
				  'dd-goals'
				]
			      ],
			      
			      [ setq,
				'all-rules',
				
				[ 'map-app',
				  
				  [ lambda,
				    [qplans],
				    
				    [ 'map-app',
				      [lambda, [qplan], [car, qplan]],
				      qplans
				    ]
				  ],
				  'qplans-es'
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				night,
				'$STRING'("All rules = ~A"),
				'all-rules'
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				night,
				'$STRING'("Selecting metaphor script...")
			      ],
			      [setq, metaphors, ['select-metaphor', 'all-rules']],
			      
			      [ if,
				[not, ['null?', metaphors]],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    night,
				    '$STRING'("Selecting first metaphor from among: ~A"),
				    metaphors
				  ],
				  
				  [ setq,
				    'metaphor-goal',
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'ACTIVE-GOAL',
					  obj,
					  
					  [ '#COMMA',
					    
					    [ 'ob$instantiate-o',
					      
					      [ 'ob$get',
						[caar, metaphors],
						[quote, goal]
					      ],
					      '*empty-me-bd*'
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    night,
				    '$STRING'("Generating plans for metaphor...")
				  ],
				  
				  [ setq,
				    'metaphor-qplans',
				    ['qplan-generate', 'metaphor-goal']
				  ],
				  [setq, '*metaphor-qplans*', 'metaphor-qplans']
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  night,
				  '$STRING'("No metaphors found")
				]
			      ],
			      []
			    ]
			  ]).

% annotating U::RUN-NIGHT 
wl: lambda_def(defun,
	      u_run_night,
	      f_u_run_night,
	      [u_dd_goals],
	      
	      [ 
		[ let,
		  
		  [ [u_qplans_es, []],
		    [u_all_rules, []],
		    [u_metaphors, []],
		    [u_metaphor_goal, []],
		    [u_metaphor_qplans]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_night,
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
			       #\(-),
			       #\(-),
			       #\(-)
			     ])
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_night,
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
			       #\(*),
			       #\(' '),
			       #\(v),
			       #\(e),
			       #\(r),
			       #\(s),
			       #\(i),
			       #\(o),
			       #\(n),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\('1'),
			       #\('9'),
			       #\('9'),
			       #\('9'),
			       #\('0'),
			       #\('5'),
			       #\('0'),
			       #\('6')
			     ])
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_night,
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
			       #\(l),
			       #\(s),
			       #\('.'),
			       #\('.'),
			       #\('.')
			     ])
		  ],
		  
		  [ setq,
		    u_qplans_es,
		    
		    [ map,
		      [quote, list],
		      [lambda, [u_goal], [u_qplan_generate, u_goal]],
		      u_dd_goals
		    ]
		  ],
		  
		  [ setq,
		    u_all_rules,
		    
		    [ u_map_app,
		      
		      [ lambda,
			[u_qplans],
			[u_map_app, [lambda, [u_qplan], [car, u_qplan]], u_qplans]
		      ],
		      u_qplans_es
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_night,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('A'),
			       #\(l),
			       #\(l),
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
		    u_all_rules
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_night,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('S'),
			       #\(e),
			       #\(l),
			       #\(e),
			       #\(c),
			       #\(t),
			       #\(i),
			       #\(n),
			       #\(g),
			       #\(' '),
			       #\(m),
			       #\(e),
			       #\(t),
			       #\(a),
			       #\(p),
			       #\(h),
			       #\(o),
			       #\(r),
			       #\(' '),
			       #\(s),
			       #\(c),
			       #\(r),
			       #\(i),
			       #\(p),
			       #\(t),
			       #\('.'),
			       #\('.'),
			       #\('.')
			     ])
		  ],
		  [setq, u_metaphors, [u_select_metaphor, u_all_rules]],
		  
		  [ if,
		    [not, [u_null_c63, u_metaphors]],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_night,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('S'),
				   #\(e),
				   #\(l),
				   #\(e),
				   #\(c),
				   #\(t),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(f),
				   #\(i),
				   #\(r),
				   #\(s),
				   #\(t),
				   #\(' '),
				   #\(m),
				   #\(e),
				   #\(t),
				   #\(a),
				   #\(p),
				   #\(h),
				   #\(o),
				   #\(r),
				   #\(' '),
				   #\(f),
				   #\(r),
				   #\(o),
				   #\(m),
				   #\(' '),
				   #\(a),
				   #\(m),
				   #\(o),
				   #\(n),
				   #\(g),
				   #\(:),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_metaphors
		      ],
		      
		      [ setq,
			u_metaphor_goal,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_active_goal,
			      u_obj,
			      
			      [ '#COMMA',
				
				[ u_ob_c36_instantiate_o,
				  
				  [ u_ob_c36_get,
				    [caar, u_metaphors],
				    [quote, u_goal]
				  ],
				  u_xx_empty_me_bd_xx
				]
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_night,
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
				   #\(m),
				   #\(e),
				   #\(t),
				   #\(a),
				   #\(p),
				   #\(h),
				   #\(o),
				   #\(r),
				   #\('.'),
				   #\('.'),
				   #\('.')
				 ])
		      ],
		      
		      [ setq,
			u_metaphor_qplans,
			[u_qplan_generate, u_metaphor_goal]
		      ],
		      [setq, u_xx_metaphor_qplans_xx, u_metaphor_qplans]
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_night,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('N'),
				 #\(o),
				 #\(' '),
				 #\(m),
				 #\(e),
				 #\(t),
				 #\(a),
				 #\(p),
				 #\(h),
				 #\(o),
				 #\(r),
				 #\(s),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(u),
				 #\(n),
				 #\(d)
			       ])
		    ]
		  ],
		  []
		]
	      ]).


% annotating U::RUN-NIGHT 
wl: arglist_info(u_run_night,
		[u_dd_goals],
		[Dd_goals_Param],
		arginfo{ all:[u_dd_goals],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_dd_goals],
			 opt:0,
			 req:[u_dd_goals],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RUN-NIGHT 
wl: init_args(exact_only, u_run_night).


% annotating U::RUN-NIGHT 
f_u_run_night(Dd_goals_Param, FnResult) :-
	Env=[bv(u_dd_goals, Dd_goals_Param)],
	LEnv=[[bv(u_qplans_es, []), bv(u_all_rules, []), bv(u_metaphors, []), bv(u_metaphor_goal, []), bv([u_metaphor_qplans], [])]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
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
				       #\(-)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
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
				       #\(*),
				       #\(' '),
				       #\(v),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(' '),
				       #\('1'),
				       #\('9'),
				       #\('9'),
				       #\('9'),
				       #\('0'),
				       #\('5'),
				       #\('0'),
				       #\('6')
				     ])
			  ],
			  Roman_nl_Ret48),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
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
				       #\(l),
				       #\(s),
				       #\('.'),
				       #\('.'),
				       #\('.')
				     ])
			  ],
			  Roman_nl_Ret49),
	Lambda=closure([Env16|LEnv], LResult, [u_goal],  (get_var(Env16, u_goal, Goal_Get), f_u_qplan_generate(Goal_Get, LResult))),
	cl_map(list, Lambda, Dd_goals_Param, Qplans_es),
	set_var(LEnv, u_qplans_es, Qplans_es),
	Lambda29=closure([Env27|LEnv], LResult28, [u_qplans],  (Lambda24=closure([Env22|Env27], LResult23, [u_qplan],  (get_var(Env22, u_qplan, Qplan_Get), cl_car(Qplan_Get, LResult23))), get_var(Env27, u_qplans, Qplans_Get), f_u_map_app(Lambda24, Qplans_Get, LResult28))),
	get_var(LEnv, u_qplans_es, Qplans_es_Get),
	f_u_map_app(Lambda29, Qplans_es_Get, All_rules),
	set_var(LEnv, u_all_rules, All_rules),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(l),
				       #\(l),
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
			    u_all_rules
			  ],
			  Roman_nl_Ret50),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(e),
				       #\(l),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(m),
				       #\(e),
				       #\(t),
				       #\(a),
				       #\(p),
				       #\(h),
				       #\(o),
				       #\(r),
				       #\(' '),
				       #\(s),
				       #\(c),
				       #\(r),
				       #\(i),
				       #\(p),
				       #\(t),
				       #\('.'),
				       #\('.'),
				       #\('.')
				     ])
			  ],
			  Roman_nl_Ret51),
	get_var(LEnv, u_all_rules, All_rules_Get),
	f_u_select_metaphor(All_rules_Get, Metaphors),
	set_var(LEnv, u_metaphors, Metaphors),
	f_u_null_c63(u_metaphors, PredArgResult),
	(   PredArgResult==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_night,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('S'),
					   #\(e),
					   #\(l),
					   #\(e),
					   #\(c),
					   #\(t),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(' '),
					   #\(f),
					   #\(i),
					   #\(r),
					   #\(s),
					   #\(t),
					   #\(' '),
					   #\(m),
					   #\(e),
					   #\(t),
					   #\(a),
					   #\(p),
					   #\(h),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\(f),
					   #\(r),
					   #\(o),
					   #\(m),
					   #\(' '),
					   #\(a),
					   #\(m),
					   #\(o),
					   #\(n),
					   #\(g),
					   #\(:),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				u_metaphors
			      ],
			      Roman_nl_Ret52),
	    f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_active_goal,
				   u_obj,
				   
				   [ '#COMMA',
				     
				     [ u_ob_c36_instantiate_o,
				       
				       [ u_ob_c36_get,
					 [caar, u_metaphors],
					 [quote, u_goal]
				       ],
				       u_xx_empty_me_bd_xx
				     ]
				   ]
				 ]
			       ],
			       Metaphor_goal),
	    set_var(LEnv, u_metaphor_goal, Metaphor_goal),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_night,
			      
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
					   #\(m),
					   #\(e),
					   #\(t),
					   #\(a),
					   #\(p),
					   #\(h),
					   #\(o),
					   #\(r),
					   #\('.'),
					   #\('.'),
					   #\('.')
					 ])
			      ],
			      Roman_nl_Ret53),
	    get_var(LEnv, u_metaphor_goal, Metaphor_goal_Get),
	    f_u_qplan_generate(Metaphor_goal_Get, Metaphor_qplans),
	    set_var(LEnv, u_metaphor_qplans, Metaphor_qplans),
	    get_var(LEnv, u_metaphor_qplans, Metaphor_qplans_Get),
	    set_var(LEnv, u_xx_metaphor_qplans_xx, Metaphor_qplans_Get),
	    _90158=Metaphor_qplans_Get
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_night,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('N'),
					   #\(o),
					   #\(' '),
					   #\(m),
					   #\(e),
					   #\(t),
					   #\(a),
					   #\(p),
					   #\(h),
					   #\(o),
					   #\(r),
					   #\(s),
					   #\(' '),
					   #\(f),
					   #\(o),
					   #\(u),
					   #\(n),
					   #\(d)
					 ])
			      ],
			      ElseResult),
	    _90158=ElseResult
	),
	LetResult=[],
	LetResult=FnResult.
:- set_opv(f_u_run_night, classof, claz_function),
   set_opv(u_run_night, compile_as, kw_function),
   set_opv(u_run_night, function, f_u_run_night),
   DefunResult=u_run_night.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:1284 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Old test routines", 1, 2711)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:2730 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'night-test',
			    [],
			    
			    [ setq,
			      '*reversal-qplans*',
			      ['qplan-generate', '*test-reversal-goal*']
			    ],
			    
			    [ setq,
			      '*revenge-qplans*',
			      ['qplan-generate', '*test-revenge-goal*']
			    ],
			    
			    [ setq,
			      '*ranger-qplans*',
			      ['qplan-generate', '*test-ranger-goal*']
			    ],
			    ['night-pp']
			  ]).

% annotating U::NIGHT-TEST 
wl: lambda_def(defun,
	      u_night_test,
	      f_u_night_test,
	      [],
	      
	      [ 
		[ setq,
		  u_xx_reversal_qplans_xx,
		  [u_qplan_generate, u_xx_test_reversal_goal_xx]
		],
		
		[ setq,
		  u_xx_revenge_qplans_xx,
		  [u_qplan_generate, u_xx_test_revenge_goal_xx]
		],
		
		[ setq,
		  u_xx_ranger_qplans_xx,
		  [u_qplan_generate, u_xx_test_ranger_goal_xx]
		],
		[u_night_pp]
	      ]).


% annotating U::NIGHT-TEST 
wl: arglist_info(u_night_test,
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

% annotating U::NIGHT-TEST 
wl: init_args(exact_only, u_night_test).


% annotating U::NIGHT-TEST 
f_u_night_test(FnResult) :-
	Env=[],
	get_var(Env, u_xx_test_reversal_goal_xx, Xx_test_reversal_goal_xx_Get),
	f_u_qplan_generate(Xx_test_reversal_goal_xx_Get, Xx_reversal_qplans_xx),
	set_var(Env, u_xx_reversal_qplans_xx, Xx_reversal_qplans_xx),
	get_var(Env, u_xx_test_revenge_goal_xx, Xx_test_revenge_goal_xx_Get),
	f_u_qplan_generate(Xx_test_revenge_goal_xx_Get, Xx_revenge_qplans_xx),
	set_var(Env, u_xx_revenge_qplans_xx, Xx_revenge_qplans_xx),
	get_var(Env, u_xx_test_ranger_goal_xx, Xx_test_ranger_goal_xx_Get),
	f_u_qplan_generate(Xx_test_ranger_goal_xx_Get, Xx_ranger_qplans_xx),
	set_var(Env, u_xx_ranger_qplans_xx, Xx_ranger_qplans_xx),
	f_u_night_pp(Night_pp_Ret),
	Night_pp_Ret=FnResult.
:- set_opv(f_u_night_test, classof, claz_function),
   set_opv(u_night_test, compile_as, kw_function),
   set_opv(u_night_test, function, f_u_night_test),
   DefunResult=u_night_test.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:2957 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'pretty-print', [obj, st], [pprint, obj, st]]).

% annotating U::PRETTY-PRINT 
wl: lambda_def(defun,
	      u_pretty_print,
	      f_u_pretty_print,
	      [u_obj, u_st],
	      [[pprint, u_obj, u_st]]).


% annotating U::PRETTY-PRINT 
wl: arglist_info(u_pretty_print,
		[u_obj, u_st],
		[Obj_Param, St_Param],
		arginfo{ all:[u_obj, u_st],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obj, u_st],
			 opt:0,
			 req:[u_obj, u_st],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRETTY-PRINT 
wl: init_args(exact_only, u_pretty_print).


% annotating U::PRETTY-PRINT 
f_u_pretty_print(Obj_Param, St_Param, FnResult) :-
	cl_pprint(Obj_Param, St_Param, Pprint_Ret),
	Pprint_Ret=FnResult.
:- set_opv(f_u_pretty_print, classof, claz_function),
   set_opv(u_pretty_print, compile_as, kw_function),
   set_opv(u_pretty_print, function, f_u_pretty_print),
   DefunResult=u_pretty_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'night-pp',
			    [],
			    ['ndbg-roman-nl', '*gate-dbg*', night, '$STRING'("")],
			    ['pretty-print', '*reversal-qplans*', '*gate-dbg*'],
			    ['ndbg-roman-nl', '*gate-dbg*', night, '$STRING'("")],
			    ['pretty-print', '*revenge-qplans*', '*gate-dbg*'],
			    ['ndbg-roman-nl', '*gate-dbg*', night, '$STRING'("")],
			    ['pretty-print', '*ranger-qplans*', '*gate-dbg*'],
			    ['ndbg-roman-nl', '*gate-dbg*', night, '$STRING'("")]
			  ]).

% annotating U::NIGHT-PP 
wl: lambda_def(defun,
	      u_night_pp,
	      f_u_night_pp,
	      [],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_night,
		  '$ARRAY'([*], claz_base_character, [])
		],
		[u_pretty_print, u_xx_reversal_qplans_xx, u_xx_gate_dbg_xx],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_night,
		  '$ARRAY'([*], claz_base_character, [])
		],
		[u_pretty_print, u_xx_revenge_qplans_xx, u_xx_gate_dbg_xx],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_night,
		  '$ARRAY'([*], claz_base_character, [])
		],
		[u_pretty_print, u_xx_ranger_qplans_xx, u_xx_gate_dbg_xx],
		
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_night,
		  '$ARRAY'([*], claz_base_character, [])
		]
	      ]).


% annotating U::NIGHT-PP 
wl: arglist_info(u_night_pp,
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

% annotating U::NIGHT-PP 
wl: init_args(exact_only, u_night_pp).


% annotating U::NIGHT-PP 
f_u_night_pp(FnResult) :-
	Env=[],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  ['$ARRAY'([*], claz_base_character, [])],
			  Roman_nl_Ret),
	get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	get_var(Env, u_xx_reversal_qplans_xx, Xx_reversal_qplans_xx_Get),
	f_u_pretty_print(Xx_reversal_qplans_xx_Get,
			 Xx_gate_dbg_xx_Get,
			 Pretty_print_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  ['$ARRAY'([*], claz_base_character, [])],
			  Roman_nl_Ret20),
	get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get13),
	get_var(Env, u_xx_revenge_qplans_xx, Xx_revenge_qplans_xx_Get),
	f_u_pretty_print(Xx_revenge_qplans_xx_Get,
			 Xx_gate_dbg_xx_Get13,
			 Pretty_print_Ret21),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  ['$ARRAY'([*], claz_base_character, [])],
			  Roman_nl_Ret22),
	get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get15),
	get_var(Env, u_xx_ranger_qplans_xx, Xx_ranger_qplans_xx_Get),
	f_u_pretty_print(Xx_ranger_qplans_xx_Get,
			 Xx_gate_dbg_xx_Get15,
			 Pretty_print_Ret23),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  ['$ARRAY'([*], claz_base_character, [])],
			  Roman_nl_Ret24),
	Roman_nl_Ret24=FnResult.
:- set_opv(f_u_night_pp, classof, claz_function),
   set_opv(u_night_pp, compile_as, kw_function),
   set_opv(u_night_pp, function, f_u_night_pp),
   DefunResult=u_night_pp.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     3314)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Quick planning", 1, 3395)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     3412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3494)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Possible mods to qplan:", 1, 3496)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3522)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Optional unification", 1, 3524)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Optional instantiation", 1, 3547)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3572)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3575)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Returns:", 1, 3577)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (qplan1 qplan2 qplan3 ...)", 1, 3588)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" where a qplan =", 1, 3617)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ((rule1 rule2 rule3 ...) -- list of all rules used in qplan",
				     1,
				     3635)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("  qqplan)", 1, 3697)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" where a qqplan =", 1, 3708)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (rule qqplan1 qqplan2 ... qqplann)",
				     1,
				     3727)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" where rule has n subgoals", 1, 3764)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" A qqplan can be NIL if there are no plans",
				     1,
				     3792)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3004 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3836)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3837 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplan-generate',
			    [goal],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      night,
			      '$STRING'("Running quickplanner on:")
			    ],
			    [pod, goal],
			    [gn, goal],
			    
			    [ yloop,
			      [initial, [result, []], [plans, []]],
			      
			      [ yfor,
				'top-rule',
				in,
				['top-rules', ['ob$get', goal, [quote, obj]]]
			      ],
			      
			      [ ydo,
				
				[ if,
				  
				  [ setq,
				    plans,
				    
				    [ 'qplan-generate1',
				      'top-rule',
				      [list, 'top-rule']
				    ]
				  ],
				  [setq, result, ['append!', result, plans]]
				]
			      ],
			      
			      [ yresult,
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    night,
				    '$STRING'("Generated plans =")
				  ],
				  ['qplans-print', result],
				  
				  [ 'qplans-gen',
				    result,
				    ['ob$get', goal, [quote, obj]],
				    []
				  ],
				  
				  [ 'qplans-gen',
				    result,
				    ['ob$get', goal, [quote, obj]],
				    t
				  ],
				  result
				]
			      ]
			    ]
			  ]).

% annotating U::QPLAN-GENERATE 
wl: lambda_def(defun,
	      u_qplan_generate,
	      f_u_qplan_generate,
	      [u_goal],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_night,
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
			     #\(q),
			     #\(u),
			     #\(i),
			     #\(c),
			     #\(k),
			     #\(p),
			     #\(l),
			     #\(a),
			     #\(n),
			     #\(n),
			     #\(e),
			     #\(r),
			     #\(' '),
			     #\(o),
			     #\(n),
			     #\(:)
			   ])
		],
		[u_pod, u_goal],
		[u_gn, u_goal],
		
		[ u_yloop,
		  [u_initial, [u_result, []], [u_plans, []]],
		  
		  [ u_yfor,
		    u_top_rule,
		    u_in,
		    [u_top_rules, [u_ob_c36_get, u_goal, [quote, u_obj]]]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ setq,
			u_plans,
			[u_qplan_generate1, u_top_rule, [list, u_top_rule]]
		      ],
		      [setq, u_result, [u_append_c33, u_result, u_plans]]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_night,
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
				   #\(d),
				   #\(' '),
				   #\(p),
				   #\(l),
				   #\(a),
				   #\(n),
				   #\(s),
				   #\(' '),
				   #\(=)
				 ])
		      ],
		      [u_qplans_print, u_result],
		      
		      [ u_qplans_gen,
			u_result,
			[u_ob_c36_get, u_goal, [quote, u_obj]],
			[]
		      ],
		      
		      [ u_qplans_gen,
			u_result,
			[u_ob_c36_get, u_goal, [quote, u_obj]],
			t
		      ],
		      u_result
		    ]
		  ]
		]
	      ]).


% annotating U::QPLAN-GENERATE 
wl: arglist_info(u_qplan_generate,
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

% annotating U::QPLAN-GENERATE 
wl: init_args(exact_only, u_qplan_generate).


% annotating U::QPLAN-GENERATE 
f_u_qplan_generate(Goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
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
				       #\(q),
				       #\(u),
				       #\(i),
				       #\(c),
				       #\(k),
				       #\(p),
				       #\(l),
				       #\(a),
				       #\(n),
				       #\(n),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(o),
				       #\(n),
				       #\(:)
				     ])
			  ],
			  Roman_nl_Ret),
	f_u_pod(Goal_Param, Pod_Ret),
	f_u_gn(Goal_Param, Gn_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_plans, []]],
		    
		    [ u_yfor,
		      u_top_rule,
		      u_in,
		      [u_top_rules, [u_ob_c36_get, u_goal, [quote, u_obj]]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ setq,
			  u_plans,
			  [u_qplan_generate1, u_top_rule, [list, u_top_rule]]
			],
			[setq, u_result, [u_append_c33, u_result, u_plans]]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ progn,
			
			[ u_ndbg_roman_nl,
			  u_xx_gate_dbg_xx,
			  u_night,
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
				     #\(d),
				     #\(' '),
				     #\(p),
				     #\(l),
				     #\(a),
				     #\(n),
				     #\(s),
				     #\(' '),
				     #\(=)
				   ])
			],
			[u_qplans_print, u_result],
			
			[ u_qplans_gen,
			  u_result,
			  [u_ob_c36_get, u_goal, [quote, u_obj]],
			  []
			],
			
			[ u_qplans_gen,
			  u_result,
			  [u_ob_c36_get, u_goal, [quote, u_obj]],
			  t
			],
			u_result
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_qplan_generate, classof, claz_function),
   set_opv(u_qplan_generate, compile_as, kw_function),
   set_opv(u_qplan_generate, function, f_u_qplan_generate),
   DefunResult=u_qplan_generate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3837 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4458)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3837 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Returns:", 1, 4460)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3837 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" NIL (if rule is NIL) -or-", 1, 4471)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3837 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (qplan1 qplan2 qplan3 ...)", 1, 4499)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:3837 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4528)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplan-generate1',
			    [rule, 'rules-in-path'],
			    
			    [ cond,
			      [['nil?', rule], []],
			      
			      [ else,
				
				[ yloop,
				  [initial, [result, []], ['qplans-es', []]],
				  
				  [ yfor,
				    rules,
				    in,
				    [subrules, rule, 'rules-in-path']
				  ],
				  
				  [ ydo,
				    
				    [ setq,
				      'qplans-es',
				      
				      [ map,
					[quote, list],
					
					[ lambda,
					  [r],
					  
					  [ 'qplan-generate1',
					    r,
					    [cons, r, 'rules-in-path']
					  ]
					],
					rules
				      ]
				    ],
				    
				    [ if,
				      
				      [ and,
					['null?', [cdr, 'qplans-es']],
					['null?', [car, 'qplans-es']]
				      ],
				      [],
				      
				      [ setq,
					'qplans-es',
					
					[ 'cross-product',
					  ['embed-nils', 'qplans-es']
					]
				      ]
				    ],
				    
				    [ yloop,
				      [initial, ['rules-used', []], [qqplan, []]],
				      [yfor, qplans, in, 'qplans-es'],
				      
				      [ ydo,
					
					[ setq,
					  'rules-used',
					  
					  [ cons,
					    rule,
					    
					    [ 'map-app',
					      
					      [ lambda,
						[qplan],
						
						[ if,
						  ['nil?', qplan],
						  [],
						  
						  [ 'qplan-get-rules-used',
						    qplan
						  ]
						]
					      ],
					      qplans
					    ]
					  ]
					],
					
					[ setq,
					  qqplan,
					  
					  [ cons,
					    rule,
					    
					    [ map,
					      [quote, list],
					      
					      [ lambda,
						[qplan],
						
						[ if,
						  ['nil?', qplan],
						  [],
						  ['qplan-get-qqplan', qplan]
						]
					      ],
					      qplans
					    ]
					  ]
					],
					
					[ setq,
					  result,
					  
					  [ cons,
					    [list, 'rules-used', qqplan],
					    result
					  ]
					]
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ]
			    ]
			  ]).

% annotating U::QPLAN-GENERATE1 
wl: lambda_def(defun,
	      u_qplan_generate1,
	      f_u_qplan_generate1,
	      [u_rule, u_rules_in_path],
	      
	      [ 
		[ cond,
		  [[u_nil_c63, u_rule], []],
		  
		  [ u_else,
		    
		    [ u_yloop,
		      [u_initial, [u_result, []], [u_qplans_es, []]],
		      
		      [ u_yfor,
			u_rules,
			u_in,
			[u_subrules, u_rule, u_rules_in_path]
		      ],
		      
		      [ u_ydo,
			
			[ setq,
			  u_qplans_es,
			  
			  [ map,
			    [quote, list],
			    
			    [ lambda,
			      [u_r],
			      
			      [ u_qplan_generate1,
				u_r,
				[cons, u_r, u_rules_in_path]
			      ]
			    ],
			    u_rules
			  ]
			],
			
			[ if,
			  
			  [ and,
			    [u_null_c63, [cdr, u_qplans_es]],
			    [u_null_c63, [car, u_qplans_es]]
			  ],
			  [],
			  
			  [ setq,
			    u_qplans_es,
			    [u_cross_product, [u_embed_nils, u_qplans_es]]
			  ]
			],
			
			[ u_yloop,
			  [u_initial, [u_rules_used, []], [u_qqplan, []]],
			  [u_yfor, u_qplans, u_in, u_qplans_es],
			  
			  [ u_ydo,
			    
			    [ setq,
			      u_rules_used,
			      
			      [ cons,
				u_rule,
				
				[ u_map_app,
				  
				  [ lambda,
				    [u_qplan],
				    
				    [ if,
				      [u_nil_c63, u_qplan],
				      [],
				      [u_qplan_get_rules_used, u_qplan]
				    ]
				  ],
				  u_qplans
				]
			      ]
			    ],
			    
			    [ setq,
			      u_qqplan,
			      
			      [ cons,
				u_rule,
				
				[ map,
				  [quote, list],
				  
				  [ lambda,
				    [u_qplan],
				    
				    [ if,
				      [u_nil_c63, u_qplan],
				      [],
				      [u_qplan_get_qqplan, u_qplan]
				    ]
				  ],
				  u_qplans
				]
			      ]
			    ],
			    
			    [ setq,
			      u_result,
			      [cons, [list, u_rules_used, u_qqplan], u_result]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ]
		]
	      ]).


% annotating U::QPLAN-GENERATE1 
wl: arglist_info(u_qplan_generate1,
		[u_rule, u_rules_in_path],
		[Rule_Param, Rules_in_path_Param],
		arginfo{ all:[u_rule, u_rules_in_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_rules_in_path],
			 opt:0,
			 req:[u_rule, u_rules_in_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLAN-GENERATE1 
wl: init_args(exact_only, u_qplan_generate1).


% annotating U::QPLAN-GENERATE1 
f_u_qplan_generate1(Rule_Param, Rules_in_path_Param, ElseResult21) :-
	Env=[bv(u_rule, Rule_Param), bv(u_rules_in_path, Rules_in_path_Param)],
	f_u_nil_c63(u_rule, IFTEST),
	(   IFTEST\==[]
	->  ElseResult21=[]
	;   get_var(Env, u_else, IFTEST16),
	    (   IFTEST16\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [u_result, []], [u_qplans_es, []]],
			    
			    [ u_yfor,
			      u_rules,
			      u_in,
			      [u_subrules, u_rule, u_rules_in_path]
			    ],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_qplans_es,
				
				[ map,
				  [quote, list],
				  
				  [ lambda,
				    [u_r],
				    
				    [ u_qplan_generate1,
				      u_r,
				      [cons, u_r, u_rules_in_path]
				    ]
				  ],
				  u_rules
				]
			      ],
			      
			      [ if,
				
				[ and,
				  [u_null_c63, [cdr, u_qplans_es]],
				  [u_null_c63, [car, u_qplans_es]]
				],
				[],
				
				[ setq,
				  u_qplans_es,
				  [u_cross_product, [u_embed_nils, u_qplans_es]]
				]
			      ],
			      
			      [ u_yloop,
				[u_initial, [u_rules_used, []], [u_qqplan, []]],
				[u_yfor, u_qplans, u_in, u_qplans_es],
				
				[ u_ydo,
				  
				  [ setq,
				    u_rules_used,
				    
				    [ cons,
				      u_rule,
				      
				      [ u_map_app,
					
					[ lambda,
					  [u_qplan],
					  
					  [ if,
					    [u_nil_c63, u_qplan],
					    [],
					    [u_qplan_get_rules_used, u_qplan]
					  ]
					],
					u_qplans
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_qqplan,
				    
				    [ cons,
				      u_rule,
				      
				      [ map,
					[quote, list],
					
					[ lambda,
					  [u_qplan],
					  
					  [ if,
					    [u_nil_c63, u_qplan],
					    [],
					    [u_qplan_get_qqplan, u_qplan]
					  ]
					],
					u_qplans
				      ]
				    ]
				  ],
				  
				  [ setq,
				    u_result,
				    
				    [ cons,
				      [list, u_rules_used, u_qqplan],
				      u_result
				    ]
				  ]
				]
			      ]
			    ],
			    [u_yresult, u_result]
			  ],
			  TrueResult),
		ElseResult21=TrueResult
	    ;   ElseResult21=[]
	    )
	).
:- set_opv(f_u_qplan_generate1, classof, claz_function),
   set_opv(u_qplan_generate1, compile_as, kw_function),
   set_opv(u_qplan_generate1, function, f_u_qplan_generate1),
   DefunResult=u_qplan_generate1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"qplan-generate1 for ~A\" rule)",
				     3,
				     4576)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" rules = (subrule1 subrule2 ... subrulen)",
				     7,
				     4801)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Generate qplans for each subrule",
				     7,
				     4850)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"rules = \")",
				     7,
				     4891)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (pretty-print rules *gate-dbg*)",
				     7,
				     4943)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"\")",
				     7,
				     4983)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"qplans-es =\")",
				     7,
				     5149)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (pretty-print qplans-es *gate-dbg*)",
				     7,
				     5204)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"\")",
				     7,
				     5248)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" qplans-es = ((qplan1.1 qplan1.2) (qplan2.1))",
				     7,
				     5292)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" -or- qplans-es = (nil (qplan2.1))",
				     7,
				     5345)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Take the cross product", 7, 5387)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" qplans-es = ((qplan1.1 qplan2.1) (qplan1.2 qplan2.1))",
				     7,
				     5578)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" -or- qplans-es = ((nil qplan2.1))",
				     7,
				     5640)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"(crossed) qplans-es =\")",
				     7,
				     5682)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (pretty-print qplans-es *gate-dbg*)",
				     7,
				     5747)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"\")",
				     7,
				     5791)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" qplans-es = ((qplan1.1 qplan2.1) (qplan1.1 qplan2.1))",
				     7,
				     5835)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" qplans = (qplan1.1 qplan2.1)", 9, 6005)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Convert the qplans for subrules into a single qplan for rule",
				     9,
				     6044)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"qplans =\")",
				     9,
				     6115)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (pretty-print qplans *gate-dbg*)",
				     9,
				     6169)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"\")",
				     9,
				     6212)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"rules-used = ~A\" rules-used)",
				     9,
				     6851)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"qqplan =\")",
				     9,
				     6923)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (pretty-print qqplan *gate-dbg*)",
				     9,
				     6977)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"\")",
				     9,
				     7020)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"result =\")",
				     9,
				     7127)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (pretty-print result *gate-dbg*)",
				     9,
				     7181)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:4529 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"\")",
				     9,
				     7224)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7300 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'embed-nils',
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, elem, in, lst],
			      
			      [ ydo,
				
				[ if,
				  [not, ['nil?', elem]],
				  [setq, result, [append, result, [list, elem]]],
				  [setq, result, [append, result, [quote, [[[]]]]]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::EMBED-NILS 
wl: lambda_def(defun,
	      u_embed_nils,
	      f_u_embed_nils,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_elem, u_in, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      [not, [u_nil_c63, u_elem]],
		      [setq, u_result, [append, u_result, [list, u_elem]]],
		      [setq, u_result, [append, u_result, [quote, [[[]]]]]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::EMBED-NILS 
wl: arglist_info(u_embed_nils,
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

% annotating U::EMBED-NILS 
wl: init_args(exact_only, u_embed_nils).


% annotating U::EMBED-NILS 
f_u_embed_nils(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_elem, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[not, [u_nil_c63, u_elem]],
			[setq, u_result, [append, u_result, [list, u_elem]]],
			[setq, u_result, [append, u_result, [quote, [[[]]]]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_embed_nils, classof, claz_function),
   set_opv(u_embed_nils, compile_as, kw_function),
   set_opv(u_embed_nils, function, f_u_embed_nils),
   DefunResult=u_embed_nils.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7550 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplan-get-rules-used',
			    [qplan],
			    [if, qplan, [car, qplan], []]
			  ]).

% annotating U::QPLAN-GET-RULES-USED 
wl: lambda_def(defun,
	      u_qplan_get_rules_used,
	      f_u_qplan_get_rules_used,
	      [u_qplan],
	      [[if, u_qplan, [car, u_qplan], []]]).


% annotating U::QPLAN-GET-RULES-USED 
wl: arglist_info(u_qplan_get_rules_used,
		[u_qplan],
		[Qplan_Param],
		arginfo{ all:[u_qplan],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplan],
			 opt:0,
			 req:[u_qplan],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLAN-GET-RULES-USED 
wl: init_args(exact_only, u_qplan_get_rules_used).


% annotating U::QPLAN-GET-RULES-USED 
f_u_qplan_get_rules_used(Qplan_Param, FnResult) :-
	(   Qplan_Param\==[]
	->  cl_car(Qplan_Param, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_qplan_get_rules_used, classof, claz_function),
   set_opv(u_qplan_get_rules_used, compile_as, kw_function),
   set_opv(u_qplan_get_rules_used, function, f_u_qplan_get_rules_used),
   DefunResult=u_qplan_get_rules_used.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplan-get-qqplan',
			    [qplan],
			    [if, qplan, [cadr, qplan], []]
			  ]).

% annotating U::QPLAN-GET-QQPLAN 
wl: lambda_def(defun,
	      u_qplan_get_qqplan,
	      f_u_qplan_get_qqplan,
	      [u_qplan],
	      [[if, u_qplan, [cadr, u_qplan], []]]).


% annotating U::QPLAN-GET-QQPLAN 
wl: arglist_info(u_qplan_get_qqplan,
		[u_qplan],
		[Qplan_Param],
		arginfo{ all:[u_qplan],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplan],
			 opt:0,
			 req:[u_qplan],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLAN-GET-QQPLAN 
wl: init_args(exact_only, u_qplan_get_qqplan).


% annotating U::QPLAN-GET-QQPLAN 
f_u_qplan_get_qqplan(Qplan_Param, FnResult) :-
	(   Qplan_Param\==[]
	->  cl_cadr(Qplan_Param, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_qplan_get_qqplan, classof, claz_function),
   set_opv(u_qplan_get_qqplan, compile_as, kw_function),
   set_opv(u_qplan_get_qqplan, function, f_u_qplan_get_qqplan),
   DefunResult=u_qplan_get_qqplan.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7706)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Returns:", 1, 7708)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ((subrule1 subrule2 ... subrulen)",
				     1,
				     7719)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (subrule1 subrule2 ... subrulen)",
				     1,
				     7755)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("  ...)", 1, 7791)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" if rule has n subgoals", 1, 7799)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7629 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7824)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7825 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    subrules,
			    [rule, 'omit-rules'],
			    
			    [ yloop,
			      
			      [ initial,
				[subrule, []],
				[subgoalnum, []],
				
				[ result,
				  
				  [ 'list-of-n',
				    [],
				    
				    [ 'ob$get',
				      rule,
				      [quote, 'number-of-subgoals']
				    ]
				  ]
				]
			      ],
			      
			      [ yfor,
				'chain-num',
				in,
				['ob$gets', rule, [quote, 'backward-chain-nums']]
			      ],
			      
			      [ ydo,
				[setq, subrule, [car, 'chain-num']],
				[setq, subgoalnum, [cadr, 'chain-num']],
				
				[ if,
				  
				  [ and,
				    ['plan?', subrule],
				    [not, ['memq?', subrule, 'omit-rules']]
				  ],
				  
				  [ setf,
				    ['nth-elem', result, subgoalnum],
				    
				    [ cons,
				      subrule,
				      ['nth-elem', result, subgoalnum]
				    ]
				  ]
				]
			      ],
			      
			      [ yresult,
				
				[ yloop,
				  [initial, [i, 0]],
				  
				  [ ywhile,
				    
				    [ (<),
				      i,
				      
				      [ 'ob$get',
					rule,
					[quote, 'number-of-subgoals']
				      ]
				    ]
				  ],
				  
				  [ ydo,
				    
				    [ if,
				      ['nil?', ['nth-elem', result, i]],
				      
				      [ setf,
					['nth-elem', result, i],
					[quote, [[]]]
				      ],
				      []
				    ],
				    [setq, i, [+, 1, i]]
				  ]
				],
				[setq, result, ['cross-product', result]],
				result
			      ]
			    ]
			  ]).

% annotating U::SUBRULES 
wl: lambda_def(defun,
	      u_subrules,
	      f_u_subrules,
	      [u_rule, u_omit_rules],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_subrule, []],
		    [u_subgoalnum, []],
		    
		    [ u_result,
		      
		      [ u_list_of_n,
			[],
			[u_ob_c36_get, u_rule, [quote, u_number_of_subgoals]]
		      ]
		    ]
		  ],
		  
		  [ u_yfor,
		    u_chain_num,
		    u_in,
		    [u_ob_c36_gets, u_rule, [quote, u_backward_chain_nums]]
		  ],
		  
		  [ u_ydo,
		    [setq, u_subrule, [car, u_chain_num]],
		    [setq, u_subgoalnum, [cadr, u_chain_num]],
		    
		    [ if,
		      
		      [ and,
			[u_plan_c63, u_subrule],
			[not, [u_memq_c63, u_subrule, u_omit_rules]]
		      ],
		      
		      [ setf,
			[u_nth_elem, u_result, u_subgoalnum],
			[cons, u_subrule, [u_nth_elem, u_result, u_subgoalnum]]
		      ]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ u_yloop,
		      [u_initial, [u_i, 0]],
		      
		      [ u_ywhile,
			
			[ (<),
			  u_i,
			  [u_ob_c36_get, u_rule, [quote, u_number_of_subgoals]]
			]
		      ],
		      
		      [ u_ydo,
			
			[ if,
			  [u_nil_c63, [u_nth_elem, u_result, u_i]],
			  [setf, [u_nth_elem, u_result, u_i], [quote, [[]]]],
			  []
			],
			[setq, u_i, [+, 1, u_i]]
		      ]
		    ],
		    [setq, u_result, [u_cross_product, u_result]],
		    u_result
		  ]
		]
	      ]).


% annotating U::SUBRULES 
wl: arglist_info(u_subrules,
		[u_rule, u_omit_rules],
		[Rule_Param, Omit_rules_Param],
		arginfo{ all:[u_rule, u_omit_rules],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_omit_rules],
			 opt:0,
			 req:[u_rule, u_omit_rules],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBRULES 
wl: init_args(exact_only, u_subrules).


% annotating U::SUBRULES 
f_u_subrules(Rule_Param, Omit_rules_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_omit_rules, Omit_rules_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_subrule, []],
		      [u_subgoalnum, []],
		      
		      [ u_result,
			
			[ u_list_of_n,
			  [],
			  [u_ob_c36_get, u_rule, [quote, u_number_of_subgoals]]
			]
		      ]
		    ],
		    
		    [ u_yfor,
		      u_chain_num,
		      u_in,
		      [u_ob_c36_gets, u_rule, [quote, u_backward_chain_nums]]
		    ],
		    
		    [ u_ydo,
		      [setq, u_subrule, [car, u_chain_num]],
		      [setq, u_subgoalnum, [cadr, u_chain_num]],
		      
		      [ if,
			
			[ and,
			  [u_plan_c63, u_subrule],
			  [not, [u_memq_c63, u_subrule, u_omit_rules]]
			],
			
			[ setf,
			  [u_nth_elem, u_result, u_subgoalnum],
			  [cons, u_subrule, [u_nth_elem, u_result, u_subgoalnum]]
			]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ u_yloop,
			[u_initial, [u_i, 0]],
			
			[ u_ywhile,
			  
			  [ (<),
			    u_i,
			    [u_ob_c36_get, u_rule, [quote, u_number_of_subgoals]]
			  ]
			],
			
			[ u_ydo,
			  
			  [ if,
			    [u_nil_c63, [u_nth_elem, u_result, u_i]],
			    [setf, [u_nth_elem, u_result, u_i], [quote, [[]]]],
			    []
			  ],
			  [setq, u_i, [+, 1, u_i]]
			]
		      ],
		      [setq, u_result, [u_cross_product, u_result]],
		      u_result
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_subrules, classof, claz_function),
   set_opv(u_subrules, compile_as, kw_function),
   set_opv(u_subrules, function, f_u_subrules),
   DefunResult=u_subrules.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7825 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"Getting subrules for ~A\" rule)",
				     3,
				     7862)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7825 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For each subgoal having no rules, insert a single nil",
				     5,
				     8387)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7825 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Return the cross product", 5, 8669)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:7825 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-roman-nl *gate-dbg* night \"Returning ~A\" result)",
				     5,
				     8741)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'list-of-n',
			    [elem, n],
			    
			    [ yloop,
			      [initial, [result, []], [i, 0]],
			      [ywhile, [<, i, n]],
			      
			      [ ydo,
				[setq, result, [cons, elem, result]],
				[setq, i, [+, 1, i]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::LIST-OF-N 
wl: lambda_def(defun,
	      u_list_of_n,
	      f_u_list_of_n,
	      [u_elem, n],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_i, 0]],
		  [u_ywhile, [<, u_i, n]],
		  
		  [ u_ydo,
		    [setq, u_result, [cons, u_elem, u_result]],
		    [setq, u_i, [+, 1, u_i]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::LIST-OF-N 
wl: arglist_info(u_list_of_n,
		[u_elem, n],
		[Elem_Param, N_Param],
		arginfo{ all:[u_elem, n],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_elem, n],
			 opt:0,
			 req:[u_elem, n],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::LIST-OF-N 
wl: init_args(exact_only, u_list_of_n).


% annotating U::LIST-OF-N 
f_u_list_of_n(Elem_Param, N_Param, FnResult) :-
	Env=[bv(u_elem, Elem_Param), bv(n, N_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_i, 0]],
		    [u_ywhile, [<, u_i, n]],
		    
		    [ u_ydo,
		      [setq, u_result, [cons, u_elem, u_result]],
		      [setq, u_i, [+, 1, u_i]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_list_of_n, classof, claz_function),
   set_opv(u_list_of_n, compile_as, kw_function),
   set_opv(u_list_of_n, function, f_u_list_of_n),
   DefunResult=u_list_of_n.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8989)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (cross-product '((a b) (c d))) => ((a c) (a d) (b c) (b d))",
				     1,
				     8991)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (cross-product '((a b) ()) => ()",
				     1,
				     9053)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (cross-product '((a b) (nil)) => ((a nil) (b nil))",
				     1,
				     9088)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (cross-product '((nil) (nil)) => ((nil nil))",
				     1,
				     9141)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Preserves order of elements, but not order of lists",
				     1,
				     9188)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:8812 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9242)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:9243 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cross-product',
			    [lst],
			    
			    [ cond,
			      [['nil?', lst], []],
			      
			      [ ['nil?', [cdr, lst]],
				
				[ map,
				  [quote, list],
				  [lambda, [elem], [list, elem]],
				  [car, lst]
				]
			      ],
			      
			      [ else,
				
				[ yloop,
				  [initial, [result, []]],
				  
				  [ yfor,
				    'rest-cross',
				    in,
				    ['cross-product', [cdr, lst]]
				  ],
				  
				  [ ydo,
				    
				    [ yloop,
				      [yfor, elem, in, [car, lst]],
				      
				      [ ydo,
					
					[ setq,
					  result,
					  
					  [ cons,
					    [cons, elem, 'rest-cross'],
					    result
					  ]
					]
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ]
			    ]
			  ]).

% annotating U::CROSS-PRODUCT 
wl: lambda_def(defun,
	      u_cross_product,
	      f_u_cross_product,
	      [u_lst],
	      
	      [ 
		[ cond,
		  [[u_nil_c63, u_lst], []],
		  
		  [ [u_nil_c63, [cdr, u_lst]],
		    
		    [ map,
		      [quote, list],
		      [lambda, [u_elem], [list, u_elem]],
		      [car, u_lst]
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ u_yloop,
		      [u_initial, [u_result, []]],
		      [u_yfor, u_rest_cross, u_in, [u_cross_product, [cdr, u_lst]]],
		      
		      [ u_ydo,
			
			[ u_yloop,
			  [u_yfor, u_elem, u_in, [car, u_lst]],
			  
			  [ u_ydo,
			    
			    [ setq,
			      u_result,
			      [cons, [cons, u_elem, u_rest_cross], u_result]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ]
		]
	      ]).


% annotating U::CROSS-PRODUCT 
wl: arglist_info(u_cross_product,
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

% annotating U::CROSS-PRODUCT 
wl: init_args(exact_only, u_cross_product).


% annotating U::CROSS-PRODUCT 
f_u_cross_product(Lst_Param, ElseResult28) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_nil_c63(u_lst, IFTEST),
	(   IFTEST\==[]
	->  ElseResult28=[]
	;   f_u_nil_c63([cdr, u_lst], IFTEST14),
	    (   IFTEST14\==[]
	    ->  Lambda=closure([Env17|Env], LResult, [u_elem],  (get_var(Env17, u_elem, Elem_Get), LResult=[Elem_Get])),
		cl_car(Lst_Param, Car_Ret),
		cl_map(list, Lambda, Car_Ret, TrueResult27),
		ElseResult28=TrueResult27
	    ;   get_var(Env, u_else, IFTEST22),
		(   IFTEST22\==[]
		->  f_u_yloop(
			      [ [u_initial, [u_result, []]],
				
				[ u_yfor,
				  u_rest_cross,
				  u_in,
				  [u_cross_product, [cdr, u_lst]]
				],
				
				[ u_ydo,
				  
				  [ u_yloop,
				    [u_yfor, u_elem, u_in, [car, u_lst]],
				    
				    [ u_ydo,
				      
				      [ setq,
					u_result,
					
					[ cons,
					  [cons, u_elem, u_rest_cross],
					  u_result
					]
				      ]
				    ]
				  ]
				],
				[u_yresult, u_result]
			      ],
			      TrueResult),
		    ElseResult28=TrueResult
		;   ElseResult28=[]
		)
	    )
	).
:- set_opv(f_u_cross_product, classof, claz_function),
   set_opv(u_cross_product, compile_as, kw_function),
   set_opv(u_cross_product, function, f_u_cross_product),
   DefunResult=u_cross_product.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:9243 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     9632)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:9243 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Qplan printing", 1, 9713)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:9243 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     9730)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:9811 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplans-es-print',
			    ['qplans-es'],
			    
			    [ yloop,
			      [yfor, qplans, in, 'qplans-es'],
			      [ydo, ['qplans-print', qplans]]
			    ]
			  ]).

% annotating U::QPLANS-ES-PRINT 
wl: lambda_def(defun,
	      u_qplans_es_print,
	      f_u_qplans_es_print,
	      [u_qplans_es],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_qplans, u_in, u_qplans_es],
		  [u_ydo, [u_qplans_print, u_qplans]]
		]
	      ]).


% annotating U::QPLANS-ES-PRINT 
wl: arglist_info(u_qplans_es_print,
		[u_qplans_es],
		[Qplans_es_Param],
		arginfo{ all:[u_qplans_es],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplans_es],
			 opt:0,
			 req:[u_qplans_es],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLANS-ES-PRINT 
wl: init_args(exact_only, u_qplans_es_print).


% annotating U::QPLANS-ES-PRINT 
f_u_qplans_es_print(Qplans_es_Param, FnResult) :-
	Env=[bv(u_qplans_es, Qplans_es_Param)],
	f_u_yloop(
		  [ [u_yfor, u_qplans, u_in, u_qplans_es],
		    [u_ydo, [u_qplans_print, u_qplans]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_qplans_es_print, classof, claz_function),
   set_opv(u_qplans_es_print, compile_as, kw_function),
   set_opv(u_qplans_es_print, function, f_u_qplans_es_print),
   DefunResult=u_qplans_es_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:9919 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplans-print',
			    [qplans],
			    
			    [ yloop,
			      [yfor, qplan, in, qplans],
			      [ydo, ['qplan-print', qplan]]
			    ]
			  ]).

% annotating U::QPLANS-PRINT 
wl: lambda_def(defun,
	      u_qplans_print,
	      f_u_qplans_print,
	      [u_qplans],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_qplan, u_in, u_qplans],
		  [u_ydo, [u_qplan_print, u_qplan]]
		]
	      ]).


% annotating U::QPLANS-PRINT 
wl: arglist_info(u_qplans_print,
		[u_qplans],
		[Qplans_Param],
		arginfo{ all:[u_qplans],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplans],
			 opt:0,
			 req:[u_qplans],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLANS-PRINT 
wl: init_args(exact_only, u_qplans_print).


% annotating U::QPLANS-PRINT 
f_u_qplans_print(Qplans_Param, FnResult) :-
	Env=[bv(u_qplans, Qplans_Param)],
	f_u_yloop(
		  [ [u_yfor, u_qplan, u_in, u_qplans],
		    [u_ydo, [u_qplan_print, u_qplan]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_qplans_print, classof, claz_function),
   set_opv(u_qplans_print, compile_as, kw_function),
   set_opv(u_qplans_print, function, f_u_qplans_print),
   DefunResult=u_qplans_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10015 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplan-print',
			    [qplan],
			    
			    [ let,
			      [[rules, [car, qplan]], [qqplan, [cadr, qplan]]],
			      ['qqplan-print', qqplan, 0]
			    ]
			  ]).

% annotating U::QPLAN-PRINT 
wl: lambda_def(defun,
	      u_qplan_print,
	      f_u_qplan_print,
	      [u_qplan],
	      
	      [ 
		[ let,
		  [[u_rules, [car, u_qplan]], [u_qqplan, [cadr, u_qplan]]],
		  [u_qqplan_print, u_qqplan, 0]
		]
	      ]).


% annotating U::QPLAN-PRINT 
wl: arglist_info(u_qplan_print,
		[u_qplan],
		[Qplan_Param],
		arginfo{ all:[u_qplan],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplan],
			 opt:0,
			 req:[u_qplan],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLAN-PRINT 
wl: init_args(exact_only, u_qplan_print).


% annotating U::QPLAN-PRINT 
f_u_qplan_print(Qplan_Param, FnResult) :-
	Env=[bv(u_qplan, Qplan_Param)],
	cl_car(Qplan_Param, Rules_Init),
	cl_cadr(Qplan_Param, Qqplan_Init),
	LEnv=[[bv(u_rules, Rules_Init), bv(u_qqplan, Qqplan_Init)]|Env],
	get_var(LEnv, u_qqplan, Qqplan_Get),
	f_u_qqplan_print(Qqplan_Get, 0, Qqplan_print_Ret),
	LetResult=Qqplan_print_Ret,
	LetResult=FnResult.
:- set_opv(f_u_qplan_print, classof, claz_function),
   set_opv(u_qplan_print, compile_as, kw_function),
   set_opv(u_qplan_print, function, f_u_qplan_print),
   DefunResult=u_qplan_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qqplan-print',
			    [qqplan, indent],
			    
			    [ cond,
			      
			      [ ['nil?', qqplan],
				['print-spaces', '*gate-dbg*', indent],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  night,
				  '$STRING'("~A"),
				  [quote, 'LEAF']
				]
			      ],
			      
			      [ else,
				['print-spaces', '*gate-dbg*', indent],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  night,
				  '$STRING'("~A"),
				  [car, qqplan]
				],
				
				[ yloop,
				  [yfor, subqqplan, in, [cdr, qqplan]],
				  
				  [ ydo,
				    ['qqplan-print', subqqplan, [+, 1, indent]]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::QQPLAN-PRINT 
wl: lambda_def(defun,
	      u_qqplan_print,
	      f_u_qqplan_print,
	      [u_qqplan, u_indent],
	      
	      [ 
		[ cond,
		  
		  [ [u_nil_c63, u_qqplan],
		    [u_print_spaces, u_xx_gate_dbg_xx, u_indent],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_night,
		      '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		      [quote, u_leaf]
		    ]
		  ],
		  
		  [ u_else,
		    [u_print_spaces, u_xx_gate_dbg_xx, u_indent],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_night,
		      '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		      [car, u_qqplan]
		    ],
		    
		    [ u_yloop,
		      [u_yfor, u_subqqplan, u_in, [cdr, u_qqplan]],
		      [u_ydo, [u_qqplan_print, u_subqqplan, [+, 1, u_indent]]]
		    ]
		  ]
		]
	      ]).


% annotating U::QQPLAN-PRINT 
wl: arglist_info(u_qqplan_print,
		[u_qqplan, u_indent],
		[Qqplan_Param, Indent_Param],
		arginfo{ all:[u_qqplan, u_indent],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qqplan, u_indent],
			 opt:0,
			 req:[u_qqplan, u_indent],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QQPLAN-PRINT 
wl: init_args(exact_only, u_qqplan_print).


% annotating U::QQPLAN-PRINT 
f_u_qqplan_print(Qqplan_Param, Indent_Param, ElseResult26) :-
	Env=[bv(u_qqplan, Qqplan_Param), bv(u_indent, Indent_Param)],
	f_u_nil_c63(u_qqplan, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    f_u_print_spaces(Xx_gate_dbg_xx_Get, Indent_Param, Print_spaces_Ret),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_night,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 [#\(~), #\('A')]),
				[quote, u_leaf]
			      ],
			      TrueResult25),
	    ElseResult26=TrueResult25
	;   get_var(Env, u_else, IFTEST18),
	    (   IFTEST18\==[]
	    ->  get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get21),
		f_u_print_spaces(Xx_gate_dbg_xx_Get21,
				 Indent_Param,
				 Print_spaces_Ret30),
		f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				  u_night,
				  
				  [ '$ARRAY'([*],
					     claz_base_character,
					     [#\(~), #\('A')]),
				    [car, u_qqplan]
				  ],
				  Roman_nl_Ret),
		f_u_yloop(
			  [ [u_yfor, u_subqqplan, u_in, [cdr, u_qqplan]],
			    
			    [ u_ydo,
			      [u_qqplan_print, u_subqqplan, [+, 1, u_indent]]
			    ]
			  ],
			  TrueResult),
		ElseResult26=TrueResult
	    ;   ElseResult26=[]
	    )
	).
:- set_opv(f_u_qqplan_print, classof, claz_function),
   set_opv(u_qqplan_print, compile_as, kw_function),
   set_opv(u_qqplan_print, function, f_u_qqplan_print),
   DefunResult=u_qqplan_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     10488)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Qplan generation:", 1, 10569)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        Generate the plan in English.",
				     1,
				     10589)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       Also, instantiate it.", 1, 10628)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Later on, we would use the full-blown DAYDREAMER planner to instantiate",
				     1,
				     10658)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("       a plan.", 1, 10738)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     10754)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10835 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplans-es-gen',
			    ['qplans-es', topgoal, english],
			    
			    [ yloop,
			      [yfor, qplans, in, 'qplans-es'],
			      [ydo, ['qplans-gen', qplans, topgoal, english]]
			    ]
			  ]).

% annotating U::QPLANS-ES-GEN 
wl: lambda_def(defun,
	      u_qplans_es_gen,
	      f_u_qplans_es_gen,
	      [u_qplans_es, u_topgoal, u_english],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_qplans, u_in, u_qplans_es],
		  [u_ydo, [u_qplans_gen, u_qplans, u_topgoal, u_english]]
		]
	      ]).


% annotating U::QPLANS-ES-GEN 
wl: arglist_info(u_qplans_es_gen,
		[u_qplans_es, u_topgoal, u_english],
		[Qplans_es_Param, Topgoal_Param, English_Param],
		arginfo{ all:[u_qplans_es, u_topgoal, u_english],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplans_es, u_topgoal, u_english],
			 opt:0,
			 req:[u_qplans_es, u_topgoal, u_english],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLANS-ES-GEN 
wl: init_args(exact_only, u_qplans_es_gen).


% annotating U::QPLANS-ES-GEN 
f_u_qplans_es_gen(Qplans_es_Param, Topgoal_Param, English_Param, FnResult) :-
	Env=[bv(u_qplans_es, Qplans_es_Param), bv(u_topgoal, Topgoal_Param), bv(u_english, English_Param)],
	f_u_yloop(
		  [ [u_yfor, u_qplans, u_in, u_qplans_es],
		    [u_ydo, [u_qplans_gen, u_qplans, u_topgoal, u_english]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_qplans_es_gen, classof, claz_function),
   set_opv(u_qplans_es_gen, compile_as, kw_function),
   set_opv(u_qplans_es_gen, function, f_u_qplans_es_gen),
   DefunResult=u_qplans_es_gen.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:10971 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplans-gen',
			    [qplans, topgoal, english],
			    
			    [ yloop,
			      [yfor, qplan, in, qplans],
			      [ydo, ['qplan-gen', qplan, topgoal, english]]
			    ]
			  ]).

% annotating U::QPLANS-GEN 
wl: lambda_def(defun,
	      u_qplans_gen,
	      f_u_qplans_gen,
	      [u_qplans, u_topgoal, u_english],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_qplan, u_in, u_qplans],
		  [u_ydo, [u_qplan_gen, u_qplan, u_topgoal, u_english]]
		]
	      ]).


% annotating U::QPLANS-GEN 
wl: arglist_info(u_qplans_gen,
		[u_qplans, u_topgoal, u_english],
		[Qplans_Param, Topgoal_Param, English_Param],
		arginfo{ all:[u_qplans, u_topgoal, u_english],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplans, u_topgoal, u_english],
			 opt:0,
			 req:[u_qplans, u_topgoal, u_english],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLANS-GEN 
wl: init_args(exact_only, u_qplans_gen).


% annotating U::QPLANS-GEN 
f_u_qplans_gen(Qplans_Param, Topgoal_Param, English_Param, FnResult) :-
	Env=[bv(u_qplans, Qplans_Param), bv(u_topgoal, Topgoal_Param), bv(u_english, English_Param)],
	f_u_yloop(
		  [ [u_yfor, u_qplan, u_in, u_qplans],
		    [u_ydo, [u_qplan_gen, u_qplan, u_topgoal, u_english]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_qplans_gen, classof, claz_function),
   set_opv(u_qplans_gen, compile_as, kw_function),
   set_opv(u_qplans_gen, function, f_u_qplans_gen),
   DefunResult=u_qplans_gen.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:11095 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplan-gen',
			    [qplan, topgoal, english],
			    
			    [ let,
			      [[rules, [car, qplan]], [qqplan, [cadr, qplan]]],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				night,
				'$STRING'("----")
			      ],
			      ['qqplan-gen', qqplan, 0, topgoal, english],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				night,
				'$STRING'("----")
			      ]
			    ]
			  ]).

% annotating U::QPLAN-GEN 
wl: lambda_def(defun,
	      u_qplan_gen,
	      f_u_qplan_gen,
	      [u_qplan, u_topgoal, u_english],
	      
	      [ 
		[ let,
		  [[u_rules, [car, u_qplan]], [u_qqplan, [cadr, u_qplan]]],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_night,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(-), #\(-), #\(-), #\(-)])
		  ],
		  [u_qqplan_gen, u_qqplan, 0, u_topgoal, u_english],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_night,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(-), #\(-), #\(-), #\(-)])
		  ]
		]
	      ]).


% annotating U::QPLAN-GEN 
wl: arglist_info(u_qplan_gen,
		[u_qplan, u_topgoal, u_english],
		[Qplan_Param, Topgoal_Param, English_Param],
		arginfo{ all:[u_qplan, u_topgoal, u_english],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qplan, u_topgoal, u_english],
			 opt:0,
			 req:[u_qplan, u_topgoal, u_english],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLAN-GEN 
wl: init_args(exact_only, u_qplan_gen).


% annotating U::QPLAN-GEN 
f_u_qplan_gen(Qplan_Param, Topgoal_Param, English_Param, FnResult) :-
	Env=[bv(u_qplan, Qplan_Param), bv(u_topgoal, Topgoal_Param), bv(u_english, English_Param)],
	cl_car(Qplan_Param, Rules_Init),
	cl_cadr(Qplan_Param, Qqplan_Init),
	LEnv=[[bv(u_rules, Rules_Init), bv(u_qqplan, Qqplan_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     [#\(-), #\(-), #\(-), #\(-)])
			  ],
			  Roman_nl_Ret),
	get_var(LEnv, u_qqplan, Qqplan_Get),
	f_u_qqplan_gen(Qqplan_Get,
		       0,
		       Topgoal_Param,
		       English_Param,
		       Qqplan_gen_Ret),
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_night,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     [#\(-), #\(-), #\(-), #\(-)])
			  ],
			  Roman_nl_Ret30),
	LetResult=Roman_nl_Ret30,
	LetResult=FnResult.
:- set_opv(f_u_qplan_gen, classof, claz_function),
   set_opv(u_qplan_gen, compile_as, kw_function),
   set_opv(u_qplan_gen, function, f_u_qplan_gen),
   DefunResult=u_qplan_gen.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:11337 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qqplan-gen',
			    [qqplan, indent, goal, english],
			    
			    [ cond,
			      [['nil?', qqplan]],
			      
			      [ else,
				
				[ let,
				  
				  [ 
				    [ subgoals,
				      ['rule-embedded-subgoals', [car, qqplan]]
				    ],
				    [bd, []]
				  ],
				  
				  [ if,
				    goal,
				    
				    [ setq,
				      bd,
				      
				      [ 'ob$unify',
					['ob$get', [car, qqplan], [quote, goal]],
					goal,
					'*empty-bd*'
				      ]
				    ],
				    [setq, bd, '*empty-bd*']
				  ],
				  
				  [ if,
				    ['nil?', bd],
				    [],
				    
				    [ progn,
				      [setq, qqplan, [cdr, qqplan]],
				      
				      [ yloop,
					[initial, [subgoal, []], [subqqplan, []]],
					
					[ ywhile,
					  
					  [ or,
					    [not, ['null?', subgoals]],
					    [not, ['null?', qqplan]]
					  ]
					],
					
					[ ydo,
					  [setq, subgoal, []],
					  
					  [ if,
					    [not, ['null?', subgoals]],
					    
					    [ progn,
					      [setq, subgoal, [car, subgoals]],
					      
					      [ setq,
						subgoal,
						
						[ 'ob$instantiate-o',
						  subgoal,
						  bd
						]
					      ],
					      
					      [ 'qplan-gen-subgoal',
						subgoal,
						indent,
						english
					      ],
					      [setq, subgoals, [cdr, subgoals]]
					    ]
					  ],
					  
					  [ if,
					    [not, ['null?', qqplan]],
					    
					    [ progn,
					      [setq, subqqplan, [car, qqplan]],
					      
					      [ 'qqplan-gen',
						subqqplan,
						[+, 1, indent],
						subgoal,
						english
					      ],
					      [setq, qqplan, [cdr, qqplan]]
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

% annotating U::QQPLAN-GEN 
wl: lambda_def(defun,
	      u_qqplan_gen,
	      f_u_qqplan_gen,
	      [u_qqplan, u_indent, u_goal, u_english],
	      
	      [ 
		[ cond,
		  [[u_nil_c63, u_qqplan]],
		  
		  [ u_else,
		    
		    [ let,
		      
		      [ [u_subgoals, [u_rule_embedded_subgoals, [car, u_qqplan]]],
			[u_bd, []]
		      ],
		      
		      [ if,
			u_goal,
			
			[ setq,
			  u_bd,
			  
			  [ u_ob_c36_unify,
			    [u_ob_c36_get, [car, u_qqplan], [quote, u_goal]],
			    u_goal,
			    u_xx_empty_bd_xx
			  ]
			],
			[setq, u_bd, u_xx_empty_bd_xx]
		      ],
		      
		      [ if,
			[u_nil_c63, u_bd],
			[],
			
			[ progn,
			  [setq, u_qqplan, [cdr, u_qqplan]],
			  
			  [ u_yloop,
			    [u_initial, [u_subgoal, []], [u_subqqplan, []]],
			    
			    [ u_ywhile,
			      
			      [ or,
				[not, [u_null_c63, u_subgoals]],
				[not, [u_null_c63, u_qqplan]]
			      ]
			    ],
			    
			    [ u_ydo,
			      [setq, u_subgoal, []],
			      
			      [ if,
				[not, [u_null_c63, u_subgoals]],
				
				[ progn,
				  [setq, u_subgoal, [car, u_subgoals]],
				  
				  [ setq,
				    u_subgoal,
				    [u_ob_c36_instantiate_o, u_subgoal, u_bd]
				  ],
				  
				  [ u_qplan_gen_subgoal,
				    u_subgoal,
				    u_indent,
				    u_english
				  ],
				  [setq, u_subgoals, [cdr, u_subgoals]]
				]
			      ],
			      
			      [ if,
				[not, [u_null_c63, u_qqplan]],
				
				[ progn,
				  [setq, u_subqqplan, [car, u_qqplan]],
				  
				  [ u_qqplan_gen,
				    u_subqqplan,
				    [+, 1, u_indent],
				    u_subgoal,
				    u_english
				  ],
				  [setq, u_qqplan, [cdr, u_qqplan]]
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


% annotating U::QQPLAN-GEN 
wl: arglist_info(u_qqplan_gen,
		[u_qqplan, u_indent, u_goal, u_english],
		[Qqplan_Param, Indent_Param, Goal_Param, English_Param],
		arginfo{ all:[u_qqplan, u_indent, u_goal, u_english],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_qqplan, u_indent, u_goal, u_english],
			 opt:0,
			 req:[u_qqplan, u_indent, u_goal, u_english],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QQPLAN-GEN 
wl: init_args(exact_only, u_qqplan_gen).


% annotating U::QQPLAN-GEN 
f_u_qqplan_gen(Qqplan_Param, Indent_Param, Goal_Param, English_Param, ElseResult40) :-
	Env=[bv(u_qqplan, Qqplan_Param), bv(u_indent, Indent_Param), bv(u_goal, Goal_Param), bv(u_english, English_Param)],
	f_u_nil_c63(u_qqplan, IFTEST),
	(   IFTEST\==[]
	->  ElseResult40=[]
	;   get_var(Env, u_else, IFTEST20),
	    (   IFTEST20\==[]
	    ->  get_var(Env, u_qqplan, Qqplan_Get),
		cl_car(Qqplan_Get, Embedded_subgoals_Param),
		f_u_rule_embedded_subgoals(Embedded_subgoals_Param,
					   Subgoals_Init),
		LEnv=[[bv(u_subgoals, Subgoals_Init), bv(u_bd, [])]|Env],
		(   Goal_Param\==[]
		->  f_u_ob_c36_unify(
				     [ u_ob_c36_get,
				       [car, u_qqplan],
				       [quote, u_goal]
				     ],
				     u_goal,
				     u_xx_empty_bd_xx,
				     TrueResult),
		    set_var(LEnv, u_bd, TrueResult),
		    _88890=TrueResult
		;   get_var(LEnv, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get),
		    set_var(LEnv, u_bd, Xx_empty_bd_xx_Get),
		    _88890=Xx_empty_bd_xx_Get
		),
		f_u_nil_c63(u_bd, IFTEST34),
		(   IFTEST34\==[]
		->  ElseResult40=[]
		;   get_var(LEnv, u_qqplan, Qqplan_Get36),
		    cl_cdr(Qqplan_Get36, Qqplan),
		    set_var(LEnv, u_qqplan, Qqplan),
		    f_u_yloop(
			      [ [u_initial, [u_subgoal, []], [u_subqqplan, []]],
				
				[ u_ywhile,
				  
				  [ or,
				    [not, [u_null_c63, u_subgoals]],
				    [not, [u_null_c63, u_qqplan]]
				  ]
				],
				
				[ u_ydo,
				  [setq, u_subgoal, []],
				  
				  [ if,
				    [not, [u_null_c63, u_subgoals]],
				    
				    [ progn,
				      [setq, u_subgoal, [car, u_subgoals]],
				      
				      [ setq,
					u_subgoal,
					
					[ u_ob_c36_instantiate_o,
					  u_subgoal,
					  u_bd
					]
				      ],
				      
				      [ u_qplan_gen_subgoal,
					u_subgoal,
					u_indent,
					u_english
				      ],
				      [setq, u_subgoals, [cdr, u_subgoals]]
				    ]
				  ],
				  
				  [ if,
				    [not, [u_null_c63, u_qqplan]],
				    
				    [ progn,
				      [setq, u_subqqplan, [car, u_qqplan]],
				      
				      [ u_qqplan_gen,
					u_subqqplan,
					[+, 1, u_indent],
					u_subgoal,
					u_english
				      ],
				      [setq, u_qqplan, [cdr, u_qqplan]]
				    ]
				  ]
				]
			      ],
			      ElseResult37),
		    ElseResult40=ElseResult37
		)
	    ;   ElseResult40=[]
	    )
	).
:- set_opv(f_u_qqplan_gen, classof, claz_function),
   set_opv(u_qqplan_gen, compile_as, kw_function),
   set_opv(u_qqplan_gen, function, f_u_qqplan_gen),
   DefunResult=u_qqplan_gen.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:11337 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(print-spaces *gate-dbg* indent)",
				     5,
				     11415)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:11337 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(ndbg-roman-nl *gate-dbg* night \"~A\" 'LEAF)",
				     5,
				     11453)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:11337 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(ndbg-roman-nl *gate-dbg* night \"(Does not unify.)\")",
				     14,
				     11776)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'qplan-gen-subgoal',
			    [subgoal, indent, english],
			    ['print-spaces', '*gate-dbg*', indent],
			    [if, english, [gn, subgoal], [pod, subgoal]]
			  ]).

% annotating U::QPLAN-GEN-SUBGOAL 
wl: lambda_def(defun,
	      u_qplan_gen_subgoal,
	      f_u_qplan_gen_subgoal,
	      [u_subgoal, u_indent, u_english],
	      
	      [ [u_print_spaces, u_xx_gate_dbg_xx, u_indent],
		[if, u_english, [u_gn, u_subgoal], [u_pod, u_subgoal]]
	      ]).


% annotating U::QPLAN-GEN-SUBGOAL 
wl: arglist_info(u_qplan_gen_subgoal,
		[u_subgoal, u_indent, u_english],
		[Subgoal_Param, Indent_Param, English_Param],
		arginfo{ all:[u_subgoal, u_indent, u_english],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subgoal, u_indent, u_english],
			 opt:0,
			 req:[u_subgoal, u_indent, u_english],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::QPLAN-GEN-SUBGOAL 
wl: init_args(exact_only, u_qplan_gen_subgoal).


% annotating U::QPLAN-GEN-SUBGOAL 
f_u_qplan_gen_subgoal(Subgoal_Param, Indent_Param, English_Param, FnResult) :-
	Env=[bv(u_subgoal, Subgoal_Param), bv(u_indent, Indent_Param), bv(u_english, English_Param)],
	get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	f_u_print_spaces(Xx_gate_dbg_xx_Get, Indent_Param, Print_spaces_Ret),
	(   English_Param\==[]
	->  f_u_gn(Subgoal_Param, TrueResult),
	    FnResult=TrueResult
	;   f_u_pod(Subgoal_Param, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_qplan_gen_subgoal, classof, claz_function),
   set_opv(u_qplan_gen_subgoal, compile_as, kw_function),
   set_opv(u_qplan_gen_subgoal, function, f_u_qplan_gen_subgoal),
   DefunResult=u_qplan_gen_subgoal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     12838)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Metaphor selection", 1, 12919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     12940)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13022)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns ((rule1 occurrences1) (rule2 occurrences2) ...)",
				     1,
				     13024)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" where (eq? (ob$get rulei 'script) t)",
				     1,
				     13082)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Order of list is rules with greater occurrences to less",
				     1,
				     13121)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" occurrences.", 1, 13179)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If occurrences = (length rules) then we have found a",
				     1,
				     13194)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" bona fide intersection.", 1, 13249)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:12688 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13275)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:13276 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'select-metaphor',
			    [rules],
			    
			    [ 'uniquify-count-occurrences-and-sort',
			      
			      [ 'map-app',
				[lambda, [rule], ['find-scripts-above', rule]],
				rules
			      ]
			    ]
			  ]).

% annotating U::SELECT-METAPHOR 
wl: lambda_def(defun,
	      u_select_metaphor,
	      f_u_select_metaphor,
	      [u_rules],
	      
	      [ 
		[ u_uniquify_count_occurrences_and_sort,
		  
		  [ u_map_app,
		    [lambda, [u_rule], [u_find_scripts_above, u_rule]],
		    u_rules
		  ]
		]
	      ]).


% annotating U::SELECT-METAPHOR 
wl: arglist_info(u_select_metaphor,
		[u_rules],
		[Rules_Param],
		arginfo{ all:[u_rules],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rules],
			 opt:0,
			 req:[u_rules],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SELECT-METAPHOR 
wl: init_args(exact_only, u_select_metaphor).


% annotating U::SELECT-METAPHOR 
f_u_select_metaphor(Rules_Param, FnResult) :-
	Env=[bv(u_rules, Rules_Param)],
	Lambda=closure([Env13|Env], LResult, [u_rule],  (get_var(Env13, u_rule, Rule_Get), f_u_find_scripts_above(Rule_Get, LResult))),
	f_u_map_app(Lambda, Rules_Param, And_sort_Param),
	f_u_uniquify_count_occurrences_and_sort(And_sort_Param, And_sort_Ret),
	And_sort_Ret=FnResult.
:- set_opv(f_u_select_metaphor, classof, claz_function),
   set_opv(u_select_metaphor, compile_as, kw_function),
   set_opv(u_select_metaphor, function, f_u_select_metaphor),
   DefunResult=u_select_metaphor.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:13276 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (A A B B B C) =>", 1, 13411)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:13276 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" ((B 3) (A 2) (C 1))", 1, 13430)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:13451 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'uniquify-count-occurrences-and-sort',
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []], [entry, []]],
			      [yfor, elem, in, lst],
			      
			      [ ydo,
				
				[ if,
				  
				  [ setq,
				    entry,
				    
				    [ mem,
				      [lambda, [x, y], ['eq?', x, [car, y]]],
				      elem,
				      result
				    ]
				  ],
				  
				  [ progn,
				    [setq, entry, [car, entry]],
				    [setf, [cadr, entry], [+, 1, [cadr, entry]]]
				  ],
				  [setq, result, [cons, [list, elem, 1], result]]
				]
			      ],
			      
			      [ yresult,
				
				[ sort,
				  result,
				  [lambda, [a, b], [>, [cadr, a], [cadr, b]]]
				]
			      ]
			    ]
			  ]).

% annotating U::UNIQUIFY-COUNT-OCCURRENCES-AND-SORT 
wl: lambda_def(defun,
	      u_uniquify_count_occurrences_and_sort,
	      f_u_uniquify_count_occurrences_and_sort,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_entry, []]],
		  [u_yfor, u_elem, u_in, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ setq,
			u_entry,
			
			[ u_mem,
			  [lambda, [u_x, u_y], [u_eq_c63, u_x, [car, u_y]]],
			  u_elem,
			  u_result
			]
		      ],
		      
		      [ progn,
			[setq, u_entry, [car, u_entry]],
			[setf, [cadr, u_entry], [+, 1, [cadr, u_entry]]]
		      ],
		      [setq, u_result, [cons, [list, u_elem, 1], u_result]]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ sort,
		      u_result,
		      [lambda, [u_a, u_b], [>, [cadr, u_a], [cadr, u_b]]]
		    ]
		  ]
		]
	      ]).


% annotating U::UNIQUIFY-COUNT-OCCURRENCES-AND-SORT 
wl: arglist_info(u_uniquify_count_occurrences_and_sort,
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

% annotating U::UNIQUIFY-COUNT-OCCURRENCES-AND-SORT 
wl: init_args(exact_only, u_uniquify_count_occurrences_and_sort).


% annotating U::UNIQUIFY-COUNT-OCCURRENCES-AND-SORT 
f_u_uniquify_count_occurrences_and_sort(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_entry, []]],
		    [u_yfor, u_elem, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ setq,
			  u_entry,
			  
			  [ u_mem,
			    [lambda, [u_x, u_y], [u_eq_c63, u_x, [car, u_y]]],
			    u_elem,
			    u_result
			  ]
			],
			
			[ progn,
			  [setq, u_entry, [car, u_entry]],
			  [setf, [cadr, u_entry], [+, 1, [cadr, u_entry]]]
			],
			[setq, u_result, [cons, [list, u_elem, 1], u_result]]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ sort,
			u_result,
			[lambda, [u_a, u_b], [>, [cadr, u_a], [cadr, u_b]]]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_uniquify_count_occurrences_and_sort, classof, claz_function),
   set_opv(u_uniquify_count_occurrences_and_sort, compile_as, kw_function),
   set_opv(u_uniquify_count_occurrences_and_sort,
	   function,
	   f_u_uniquify_count_occurrences_and_sort),
   DefunResult=u_uniquify_count_occurrences_and_sort.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:13884 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'find-scripts-above',
			    [rule],
			    ['find-scripts-above1', rule, [list, rule]]
			  ]).

% annotating U::FIND-SCRIPTS-ABOVE 
wl: lambda_def(defun,
	      u_find_scripts_above,
	      f_u_find_scripts_above,
	      [u_rule],
	      [[u_find_scripts_above1, u_rule, [list, u_rule]]]).


% annotating U::FIND-SCRIPTS-ABOVE 
wl: arglist_info(u_find_scripts_above,
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

% annotating U::FIND-SCRIPTS-ABOVE 
wl: init_args(exact_only, u_find_scripts_above).


% annotating U::FIND-SCRIPTS-ABOVE 
f_u_find_scripts_above(Rule_Param, FnResult) :-
	_86298=[Rule_Param],
	f_u_find_scripts_above1(Rule_Param, _86298, Scripts_above1_Ret),
	Scripts_above1_Ret=FnResult.
:- set_opv(f_u_find_scripts_above, classof, claz_function),
   set_opv(u_find_scripts_above, compile_as, kw_function),
   set_opv(u_find_scripts_above, function, f_u_find_scripts_above),
   DefunResult=u_find_scripts_above.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:13960 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'find-scripts-above1',
			    [rule, 'rules-in-path'],
			    
			    [ yloop,
			      [initial, [result, []]],
			      
			      [ yfor,
				superrule,
				in,
				['ob$gets', rule, [quote, 'forward-chain']]
			      ],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    ['plan?', superrule],
				    [not, ['memq?', superrule, 'rules-in-path']]
				  ],
				  
				  [ if,
				    ['ob$get', superrule, [quote, script]],
				    [setq, result, [cons, superrule, result]],
				    
				    [ setq,
				      result,
				      
				      [ append,
					result,
					
					[ 'find-scripts-above1',
					  superrule,
					  [cons, superrule, 'rules-in-path']
					]
				      ]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::FIND-SCRIPTS-ABOVE1 
wl: lambda_def(defun,
	      u_find_scripts_above1,
	      f_u_find_scripts_above1,
	      [u_rule, u_rules_in_path],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  
		  [ u_yfor,
		    u_superrule,
		    u_in,
		    [u_ob_c36_gets, u_rule, [quote, u_forward_chain]]
		  ],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			[u_plan_c63, u_superrule],
			[not, [u_memq_c63, u_superrule, u_rules_in_path]]
		      ],
		      
		      [ if,
			[u_ob_c36_get, u_superrule, [quote, u_script]],
			[setq, u_result, [cons, u_superrule, u_result]],
			
			[ setq,
			  u_result,
			  
			  [ append,
			    u_result,
			    
			    [ u_find_scripts_above1,
			      u_superrule,
			      [cons, u_superrule, u_rules_in_path]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::FIND-SCRIPTS-ABOVE1 
wl: arglist_info(u_find_scripts_above1,
		[u_rule, u_rules_in_path],
		[Rule_Param, Rules_in_path_Param],
		arginfo{ all:[u_rule, u_rules_in_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_rules_in_path],
			 opt:0,
			 req:[u_rule, u_rules_in_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FIND-SCRIPTS-ABOVE1 
wl: init_args(exact_only, u_find_scripts_above1).


% annotating U::FIND-SCRIPTS-ABOVE1 
f_u_find_scripts_above1(Rule_Param, Rules_in_path_Param, FnResult) :-
	Env=[bv(u_rule, Rule_Param), bv(u_rules_in_path, Rules_in_path_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    
		    [ u_yfor,
		      u_superrule,
		      u_in,
		      [u_ob_c36_gets, u_rule, [quote, u_forward_chain]]
		    ],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_plan_c63, u_superrule],
			  [not, [u_memq_c63, u_superrule, u_rules_in_path]]
			],
			
			[ if,
			  [u_ob_c36_get, u_superrule, [quote, u_script]],
			  [setq, u_result, [cons, u_superrule, u_result]],
			  
			  [ setq,
			    u_result,
			    
			    [ append,
			      u_result,
			      
			      [ u_find_scripts_above1,
				u_superrule,
				[cons, u_superrule, u_rules_in_path]
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
:- set_opv(f_u_find_scripts_above1, classof, claz_function),
   set_opv(u_find_scripts_above1, compile_as, kw_function),
   set_opv(u_find_scripts_above1, function, f_u_find_scripts_above1),
   DefunResult=u_find_scripts_above1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_night.cl:13960 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 14548)).
:- true.


% Total time: 4.866 seconds


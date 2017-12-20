
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_gen" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:12:33 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 10/9/84:  Original generator written",
				     1,
				     208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:246 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  2/8/86:  Started adding new generation code",
				     1,
				     247)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:293 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/24/86:  Took out flavor calls", 1, 294)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:327 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 11/7/86:  Started adding some new entries",
				     1,
				     328)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:371 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 372)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:373 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     374)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:454 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*references*', []]).
:- set_var(TLEnv3, setq, u_xx_references_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:480 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    gn,
			    [con],
			    
			    [ generate1,
			      con,
			      '*global-switches*',
			      '*reality*',
			      '*me-belief-path*'
			    ]
			  ]).

% annotating U::GN 
wl: lambda_def(defun,
	      u_gn,
	      f_u_gn,
	      [u_con],
	      
	      [ 
		[ u_generate1,
		  u_con,
		  u_xx_global_switches_xx,
		  u_xx_reality_xx,
		  u_xx_me_belief_path_xx
		]
	      ]).


% annotating U::GN 
wl: arglist_info(u_gn,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GN 
wl: init_args(exact_only, u_gn).


% annotating U::GN 
f_u_gn(Con_Param, FnResult) :-
	Env=[bv(u_con, Con_Param)],
	get_var(Env, u_xx_global_switches_xx, Xx_global_switches_xx_Get),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_generate1(Con_Param,
		      Xx_global_switches_xx_Get,
		      Xx_reality_xx_Get,
		      Xx_me_belief_path_xx_Get,
		      Generate1_Ret),
	Generate1_Ret=FnResult.
:- set_opv(f_u_gn, classof, claz_function),
   set_opv(u_gn, compile_as, kw_function),
   set_opv(u_gn, function, f_u_gn),
   DefunResult=u_gn.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:559 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    gns,
			    [con, sw],
			    [generate1, con, sw, '*reality*', '*me-belief-path*']
			  ]).

% annotating U::GNS 
wl: lambda_def(defun,
	      u_gns,
	      f_u_gns,
	      [u_con, u_sw],
	      
	      [ 
		[ u_generate1,
		  u_con,
		  u_sw,
		  u_xx_reality_xx,
		  u_xx_me_belief_path_xx
		]
	      ]).


% annotating U::GNS 
wl: arglist_info(u_gns,
		[u_con, u_sw],
		[Con_Param, Sw_Param],
		arginfo{ all:[u_con, u_sw],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, u_sw],
			 opt:0,
			 req:[u_con, u_sw],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GNS 
wl: init_args(exact_only, u_gns).


% annotating U::GNS 
f_u_gns(Con_Param, Sw_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(u_sw, Sw_Param)],
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_generate1(Con_Param,
		      Sw_Param,
		      Xx_reality_xx_Get,
		      Xx_me_belief_path_xx_Get,
		      Generate1_Ret),
	Generate1_Ret=FnResult.
:- set_opv(f_u_gns, classof, claz_function),
   set_opv(u_gns, compile_as, kw_function),
   set_opv(u_gns, function, f_u_gns),
   DefunResult=u_gns.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:559 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This function should be called only from assert.",
				     1,
				     628)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:678 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    generate,
			    [con, context, switches],
			    
			    [ if,
			      
			      [ and,
				
				[ 'null?',
				  
				  [ 'switches-lookup',
				    [quote, 'no-gen'],
				    switches
				  ]
				],
				['null?', ['no-top-gen?', con, context]]
			      ],
			      
			      [ progn,
				['gs-reset-sentence', '*gen-stream*'],
				
				[ 'gs-string-write',
				  '*gen-stream*',
				  '$STRING'("==================================================")
				],
				['gs-newline', '*gen-stream*'],
				
				[ generate1,
				  con,
				  switches,
				  context,
				  '*me-belief-path*'
				],
				
				[ 'gs-string-write',
				  '*gen-stream*',
				  '$STRING'("==================================================")
				],
				['gs-newline', '*gen-stream*']
			      ]
			    ]
			  ]).

% annotating U::GENERATE 
wl: lambda_def(defun,
	      u_generate,
	      f_u_generate,
	      [u_con, u_context, u_switches],
	      
	      [ 
		[ if,
		  
		  [ and,
		    
		    [ u_null_c63,
		      [u_switches_lookup, [quote, u_no_gen], u_switches]
		    ],
		    [u_null_c63, [u_no_top_gen_c63, u_con, u_context]]
		  ],
		  
		  [ progn,
		    [u_gs_reset_sentence, u_xx_gen_stream_xx],
		    
		    [ u_gs_string_write,
		      u_xx_gen_stream_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=)
			       ])
		    ],
		    [u_gs_newline, u_xx_gen_stream_xx],
		    
		    [ u_generate1,
		      u_con,
		      u_switches,
		      u_context,
		      u_xx_me_belief_path_xx
		    ],
		    
		    [ u_gs_string_write,
		      u_xx_gen_stream_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=),
				 #\(=)
			       ])
		    ],
		    [u_gs_newline, u_xx_gen_stream_xx]
		  ]
		]
	      ]).


% annotating U::GENERATE 
wl: arglist_info(u_generate,
		[u_con, u_context, u_switches],
		[Con_Param, Context_Param, Switches_Param],
		arginfo{ all:[u_con, u_context, u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, u_context, u_switches],
			 opt:0,
			 req:[u_con, u_context, u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GENERATE 
wl: init_args(exact_only, u_generate).


% annotating U::GENERATE 
f_u_generate(Con_Param, Context_Param, Switches_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(u_context, Context_Param), bv(u_switches, Switches_Param)],
	f_u_null_c63([u_switches_lookup, [quote, u_no_gen], u_switches], IFTEST18),
	(   IFTEST18\==[]
	->  f_u_null_c63([u_no_top_gen_c63, u_con, u_context], TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get),
	    f_u_gs_reset_sentence(Xx_gen_stream_xx_Get, Reset_sentence_Ret),
	    get_var(Env, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get22),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get22,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=)
					 ]),
				String_write_Ret),
	    get_var(Env, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get23),
	    f_u_gs_newline(Xx_gen_stream_xx_Get23, Gs_newline_Ret),
	    get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	    f_u_generate1(Con_Param,
			  Switches_Param,
			  Context_Param,
			  Xx_me_belief_path_xx_Get,
			  Generate1_Ret),
	    get_var(Env, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get28),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get28,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=),
					   #\(=)
					 ]),
				String_write_Ret37),
	    get_var(Env, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get29),
	    f_u_gs_newline(Xx_gen_stream_xx_Get29, TrueResult30),
	    FnResult=TrueResult30
	;   FnResult=[]
	).
:- set_opv(f_u_generate, classof, claz_function),
   set_opv(u_generate, compile_as, kw_function),
   set_opv(u_generate, function, f_u_generate),
   DefunResult=u_generate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:678 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (setq *references* nil)", 1, 828)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:678 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" just for good measure.", 40, 898)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1240 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*no-top-gen-lst*',
			    
			    [ list,
			      '^rprox',
			      '^causal-link',
			      '^other',
			      '^ordering',
			      '^altern',
			      '^at'
			    ]
			  ]).
:- get_var(TLEnv3, u_c94_causal_link, C94_causal_link_Get),
   get_var(TLEnv3, u_c94_ordering, C94_ordering_Get),
   ( get_var(TLEnv3, u_c94_altern, C94_altern_Get),
     get_var(TLEnv3, u_c94_rprox, C94_rprox_Get)
   ),
   ( get_var(TLEnv3, u_c94_at, C94_at_Get),
     get_var(TLEnv3, u_c94_other, C94_other_Get)
   ),
   _Ignored=[C94_rprox_Get, C94_causal_link_Get, C94_other_Get, C94_ordering_Get, C94_altern_Get, C94_at_Get],
   set_var(TLEnv3, u_xx_no_top_gen_lst_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'no-top-gen?',
			    [con, context],
			    
			    [ and,
			      ['ob?', con],
			      [not, ['ob$get', con, [quote, 'input-state?']]],
			      
			      [ or,
				['instance-of-any?', con, '*no-top-gen-lst*'],
				['action-goal-success?', con],
				['believe-action?', con],
				['believe-introduction?', con],
				['believe-rprox?', con],
				['believe-link?', con],
				['know-location?', con],
				['very-small-emotion?', con],
				['rtrue-subgoal?', con]
			      ]
			    ]
			  ]).

% annotating U::NO-TOP-GEN? 
wl: lambda_def(defun,
	      u_no_top_gen_c63,
	      f_u_no_top_gen_c63,
	      [u_con, u_context],
	      
	      [ 
		[ and,
		  [u_ob_c63, u_con],
		  [not, [u_ob_c36_get, u_con, [quote, u_input_state_c63]]],
		  
		  [ or,
		    [u_instance_of_any_c63, u_con, u_xx_no_top_gen_lst_xx],
		    [u_action_goal_success_c63, u_con],
		    [u_believe_action_c63, u_con],
		    [u_believe_introduction_c63, u_con],
		    [u_believe_rprox_c63, u_con],
		    [u_believe_link_c63, u_con],
		    [u_know_location_c63, u_con],
		    [u_very_small_emotion_c63, u_con],
		    [u_rtrue_subgoal_c63, u_con]
		  ]
		]
	      ]).


% annotating U::NO-TOP-GEN? 
wl: arglist_info(u_no_top_gen_c63,
		[u_con, u_context],
		[Con_Param, Context_Param],
		arginfo{ all:[u_con, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, u_context],
			 opt:0,
			 req:[u_con, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NO-TOP-GEN? 
wl: init_args(exact_only, u_no_top_gen_c63).


% annotating U::NO-TOP-GEN? 
f_u_no_top_gen_c63(Con_Param, Context_Param, TrueResult39) :-
	Env=[bv(u_con, Con_Param), bv(u_context, Context_Param)],
	f_u_ob_c63(u_con, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Con_Param, u_input_state_c63, PredArgResult),
	    (   PredArgResult==[]
	    ->  (   get_var(Env,
			    u_xx_no_top_gen_lst_xx,
			    Xx_no_top_gen_lst_xx_Get),
		    f_u_instance_of_any_c63(Con_Param,
					    Xx_no_top_gen_lst_xx_Get,
					    FORM1_Res37),
		    FORM1_Res37\==[],
		    TrueResult39=FORM1_Res37
		->  true
		;   f_u_action_goal_success_c63(Con_Param, FORM1_Res36),
		    FORM1_Res36\==[],
		    TrueResult39=FORM1_Res36
		->  true
		;   f_u_believe_action_c63(Con_Param, FORM1_Res35),
		    FORM1_Res35\==[],
		    TrueResult39=FORM1_Res35
		->  true
		;   f_u_believe_introduction_c63(Con_Param, FORM1_Res34),
		    FORM1_Res34\==[],
		    TrueResult39=FORM1_Res34
		->  true
		;   f_u_believe_rprox_c63(Con_Param, FORM1_Res33),
		    FORM1_Res33\==[],
		    TrueResult39=FORM1_Res33
		->  true
		;   f_u_believe_link_c63(Con_Param, FORM1_Res32),
		    FORM1_Res32\==[],
		    TrueResult39=FORM1_Res32
		->  true
		;   f_u_know_location_c63(Con_Param, FORM1_Res31),
		    FORM1_Res31\==[],
		    TrueResult39=FORM1_Res31
		->  true
		;   f_u_very_small_emotion_c63(Con_Param, FORM1_Res),
		    FORM1_Res\==[],
		    TrueResult39=FORM1_Res
		->  true
		;   f_u_rtrue_subgoal_c63(Con_Param, Subgoal_c63_Ret),
		    TrueResult39=Subgoal_c63_Ret
		)
	    ;   TrueResult39=[]
	    )
	;   TrueResult39=[]
	).
:- set_opv(f_u_no_top_gen_c63, classof, claz_function),
   set_opv(u_no_top_gen_c63, compile_as, kw_function),
   set_opv(u_no_top_gen_c63, function, f_u_no_top_gen_c63),
   DefunResult=u_no_top_gen_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1724 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'very-small-emotion?',
			    [ob],
			    
			    [ and,
			      ['ty$instance?', ob, [quote, emotion]],
			      ['fl<', [strength, ob], 0.1]
			    ]
			  ]).

% annotating U::VERY-SMALL-EMOTION? 
wl: lambda_def(defun,
	      u_very_small_emotion_c63,
	      f_u_very_small_emotion_c63,
	      [u_ob],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_ob, [quote, u_emotion]],
		  [u_fl_c60, [u_strength, u_ob], 0.1]
		]
	      ]).


% annotating U::VERY-SMALL-EMOTION? 
wl: arglist_info(u_very_small_emotion_c63,
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

% annotating U::VERY-SMALL-EMOTION? 
wl: init_args(exact_only, u_very_small_emotion_c63).


% annotating U::VERY-SMALL-EMOTION? 
f_u_very_small_emotion_c63(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_ty_c36_instance_c63(Ob_Param, u_emotion, IFTEST),
	(   IFTEST\==[]
	->  f_u_fl_c60([u_strength, u_ob], 0.1, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_very_small_emotion_c63, classof, claz_function),
   set_opv(u_very_small_emotion_c63, compile_as, kw_function),
   set_opv(u_very_small_emotion_c63, function, f_u_very_small_emotion_c63),
   DefunResult=u_very_small_emotion_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1824 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'instance-of-any?',
			    [ob, lst],
			    ['any?', [lambda, [x], ['ty$instance-of?', ob, x]], lst]
			  ]).

% annotating U::INSTANCE-OF-ANY? 
wl: lambda_def(defun,
	      u_instance_of_any_c63,
	      f_u_instance_of_any_c63,
	      [u_ob, u_lst],
	      
	      [ 
		[ u_any_c63,
		  [lambda, [u_x], [u_ty_c36_instance_of_c63, u_ob, u_x]],
		  u_lst
		]
	      ]).


% annotating U::INSTANCE-OF-ANY? 
wl: arglist_info(u_instance_of_any_c63,
		[u_ob, u_lst],
		[Ob_Param, Lst_Param],
		arginfo{ all:[u_ob, u_lst],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_lst],
			 opt:0,
			 req:[u_ob, u_lst],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INSTANCE-OF-ANY? 
wl: init_args(exact_only, u_instance_of_any_c63).


% annotating U::INSTANCE-OF-ANY? 
f_u_instance_of_any_c63(Ob_Param, Lst_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_lst, Lst_Param)],
	f_u_any_c63([lambda, [u_x], [u_ty_c36_instance_of_c63, u_ob, u_x]],
		    u_lst,
		    Lst),
	Lst=FnResult.
:- set_opv(f_u_instance_of_any_c63, classof, claz_function),
   set_opv(u_instance_of_any_c63, compile_as, kw_function),
   set_opv(u_instance_of_any_c63, function, f_u_instance_of_any_c63),
   DefunResult=u_instance_of_any_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:1908 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'believe-action?',
			    [con],
			    
			    [ and,
			      ['ty$instance?', con, [quote, 'BELIEVE']],
			      ['ob?', ['ob$get', con, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', con, [quote, obj]],
				[quote, 'ACTION']
			      ]
			    ]
			  ]).

% annotating U::BELIEVE-ACTION? 
wl: lambda_def(defun,
	      u_believe_action_c63,
	      f_u_believe_action_c63,
	      [u_con],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_con, [quote, u_believe]],
		  [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_con, [quote, u_obj]],
		    [quote, u_action]
		  ]
		]
	      ]).


% annotating U::BELIEVE-ACTION? 
wl: arglist_info(u_believe_action_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BELIEVE-ACTION? 
wl: init_args(exact_only, u_believe_action_c63).


% annotating U::BELIEVE-ACTION? 
f_u_believe_action_c63(Con_Param, TrueResult19) :-
	Env=[bv(u_con, Con_Param)],
	f_u_ty_c36_instance_c63(Con_Param, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_action, TrueResult),
		TrueResult19=TrueResult
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_believe_action_c63, classof, claz_function),
   set_opv(u_believe_action_c63, compile_as, kw_function),
   set_opv(u_believe_action_c63, function, f_u_believe_action_c63),
   DefunResult=u_believe_action_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2054 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'action-goal-success?',
			    [con],
			    
			    [ and,
			      ['ty$instance?', con, [quote, 'SUCCEEDED-GOAL']],
			      ['ob?', ['ob$get', con, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', con, [quote, obj]],
				[quote, 'ACTION']
			      ]
			    ]
			  ]).

% annotating U::ACTION-GOAL-SUCCESS? 
wl: lambda_def(defun,
	      u_action_goal_success_c63,
	      f_u_action_goal_success_c63,
	      [u_con],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_con, [quote, u_succeeded_goal]],
		  [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_con, [quote, u_obj]],
		    [quote, u_action]
		  ]
		]
	      ]).


% annotating U::ACTION-GOAL-SUCCESS? 
wl: arglist_info(u_action_goal_success_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ACTION-GOAL-SUCCESS? 
wl: init_args(exact_only, u_action_goal_success_c63).


% annotating U::ACTION-GOAL-SUCCESS? 
f_u_action_goal_success_c63(Con_Param, TrueResult19) :-
	Env=[bv(u_con, Con_Param)],
	f_u_ty_c36_instance_c63(Con_Param, u_succeeded_goal, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_action, TrueResult),
		TrueResult19=TrueResult
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_action_goal_success_c63, classof, claz_function),
   set_opv(u_action_goal_success_c63, compile_as, kw_function),
   set_opv(u_action_goal_success_c63, function, f_u_action_goal_success_c63),
   DefunResult=u_action_goal_success_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2212 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'believe-introduction?',
			    [con],
			    
			    [ and,
			      ['ty$instance?', con, [quote, 'BELIEVE']],
			      ['ob?', ['ob$get', con, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', con, [quote, obj]],
				[quote, 'INTRODUCTION']
			      ]
			    ]
			  ]).

% annotating U::BELIEVE-INTRODUCTION? 
wl: lambda_def(defun,
	      u_believe_introduction_c63,
	      f_u_believe_introduction_c63,
	      [u_con],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_con, [quote, u_believe]],
		  [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_con, [quote, u_obj]],
		    [quote, u_introduction]
		  ]
		]
	      ]).


% annotating U::BELIEVE-INTRODUCTION? 
wl: arglist_info(u_believe_introduction_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BELIEVE-INTRODUCTION? 
wl: init_args(exact_only, u_believe_introduction_c63).


% annotating U::BELIEVE-INTRODUCTION? 
f_u_believe_introduction_c63(Con_Param, TrueResult19) :-
	Env=[bv(u_con, Con_Param)],
	f_u_ty_c36_instance_c63(Con_Param, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_introduction, TrueResult),
		TrueResult19=TrueResult
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_believe_introduction_c63, classof, claz_function),
   set_opv(u_believe_introduction_c63, compile_as, kw_function),
   set_opv(u_believe_introduction_c63, function, f_u_believe_introduction_c63),
   DefunResult=u_believe_introduction_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2370 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'believe-link?',
			    [con],
			    
			    [ and,
			      ['ty$instance?', con, [quote, 'BELIEVE']],
			      ['ob?', ['ob$get', con, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', con, [quote, obj]],
				[quote, 'CAUSAL-LINK']
			      ]
			    ]
			  ]).

% annotating U::BELIEVE-LINK? 
wl: lambda_def(defun,
	      u_believe_link_c63,
	      f_u_believe_link_c63,
	      [u_con],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_con, [quote, u_believe]],
		  [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_con, [quote, u_obj]],
		    [quote, u_causal_link]
		  ]
		]
	      ]).


% annotating U::BELIEVE-LINK? 
wl: arglist_info(u_believe_link_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BELIEVE-LINK? 
wl: init_args(exact_only, u_believe_link_c63).


% annotating U::BELIEVE-LINK? 
f_u_believe_link_c63(Con_Param, TrueResult19) :-
	Env=[bv(u_con, Con_Param)],
	f_u_ty_c36_instance_c63(Con_Param, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_causal_link, TrueResult),
		TrueResult19=TrueResult
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_believe_link_c63, classof, claz_function),
   set_opv(u_believe_link_c63, compile_as, kw_function),
   set_opv(u_believe_link_c63, function, f_u_believe_link_c63),
   DefunResult=u_believe_link_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2519 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'believe-rprox?',
			    [con],
			    
			    [ and,
			      ['ty$instance?', con, [quote, 'BELIEVE']],
			      ['ob?', ['ob$get', con, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', con, [quote, obj]],
				[quote, 'RPROX']
			      ]
			    ]
			  ]).

% annotating U::BELIEVE-RPROX? 
wl: lambda_def(defun,
	      u_believe_rprox_c63,
	      f_u_believe_rprox_c63,
	      [u_con],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_con, [quote, u_believe]],
		  [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_con, [quote, u_obj]],
		    [quote, u_rprox]
		  ]
		]
	      ]).


% annotating U::BELIEVE-RPROX? 
wl: arglist_info(u_believe_rprox_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BELIEVE-RPROX? 
wl: init_args(exact_only, u_believe_rprox_c63).


% annotating U::BELIEVE-RPROX? 
f_u_believe_rprox_c63(Con_Param, TrueResult19) :-
	Env=[bv(u_con, Con_Param)],
	f_u_ty_c36_instance_c63(Con_Param, u_believe, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_rprox, TrueResult),
		TrueResult19=TrueResult
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_believe_rprox_c63, classof, claz_function),
   set_opv(u_believe_rprox_c63, compile_as, kw_function),
   set_opv(u_believe_rprox_c63, function, f_u_believe_rprox_c63),
   DefunResult=u_believe_rprox_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2663 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'know-location?',
			    [con],
			    
			    [ and,
			      ['ty$instance?', con, [quote, 'KNOW']],
			      ['ob?', ['ob$get', con, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', con, [quote, obj]],
				[quote, 'LOCATION']
			      ]
			    ]
			  ]).

% annotating U::KNOW-LOCATION? 
wl: lambda_def(defun,
	      u_know_location_c63,
	      f_u_know_location_c63,
	      [u_con],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_con, [quote, u_know]],
		  [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_con, [quote, u_obj]],
		    [quote, u_location]
		  ]
		]
	      ]).


% annotating U::KNOW-LOCATION? 
wl: arglist_info(u_know_location_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::KNOW-LOCATION? 
wl: init_args(exact_only, u_know_location_c63).


% annotating U::KNOW-LOCATION? 
f_u_know_location_c63(Con_Param, TrueResult19) :-
	Env=[bv(u_con, Con_Param)],
	f_u_ty_c36_instance_c63(Con_Param, u_know, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
	    (   IFTEST15\==[]
	    ->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_location, TrueResult),
		TrueResult19=TrueResult
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_know_location_c63, classof, claz_function),
   set_opv(u_know_location_c63, compile_as, kw_function),
   set_opv(u_know_location_c63, function, f_u_know_location_c63),
   DefunResult=u_know_location_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:2807 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rtrue-subgoal?',
			    [con],
			    
			    [ or,
			      
			      [ and,
				['ty$instance?', con, [quote, 'ACTIVE-GOAL']],
				['ob?', ['ob$get', con, [quote, obj]]],
				
				[ 'ty$instance?',
				  ['ob$get', con, [quote, obj]],
				  [quote, 'RTRUE']
				]
			      ],
			      
			      [ and,
				['ty$instance?', con, [quote, 'BELIEVE']],
				['ob?', ['ob$get', con, [quote, obj]]],
				
				[ 'ty$instance?',
				  ['ob$get', con, [quote, obj]],
				  [quote, 'ACTIVE-GOAL']
				],
				['ob?', ['ob$pget', con, [quote, [obj, obj]]]],
				
				[ 'ty$instance?',
				  ['ob$pget', con, [quote, [obj, obj]]],
				  [quote, 'RTRUE']
				]
			      ]
			    ]
			  ]).

% annotating U::RTRUE-SUBGOAL? 
wl: lambda_def(defun,
	      u_rtrue_subgoal_c63,
	      f_u_rtrue_subgoal_c63,
	      [u_con],
	      
	      [ 
		[ or,
		  
		  [ and,
		    [u_ty_c36_instance_c63, u_con, [quote, u_active_goal]],
		    [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		    
		    [ u_ty_c36_instance_c63,
		      [u_ob_c36_get, u_con, [quote, u_obj]],
		      [quote, u_rtrue]
		    ]
		  ],
		  
		  [ and,
		    [u_ty_c36_instance_c63, u_con, [quote, u_believe]],
		    [u_ob_c63, [u_ob_c36_get, u_con, [quote, u_obj]]],
		    
		    [ u_ty_c36_instance_c63,
		      [u_ob_c36_get, u_con, [quote, u_obj]],
		      [quote, u_active_goal]
		    ],
		    [u_ob_c63, [u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]]],
		    
		    [ u_ty_c36_instance_c63,
		      [u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]],
		      [quote, u_rtrue]
		    ]
		  ]
		]
	      ]).


% annotating U::RTRUE-SUBGOAL? 
wl: arglist_info(u_rtrue_subgoal_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RTRUE-SUBGOAL? 
wl: init_args(exact_only, u_rtrue_subgoal_c63).


% annotating U::RTRUE-SUBGOAL? 
f_u_rtrue_subgoal_c63(Con_Param, TrueResult32) :-
	Env=[bv(u_con, Con_Param)],
	(   f_u_ty_c36_instance_c63(Con_Param, u_active_goal, IFTEST),
	    (   IFTEST\==[]
	    ->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST15),
		(   IFTEST15\==[]
		->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		    f_u_ty_c36_instance_c63(Obj, u_rtrue, TrueResult),
		    TrueResult19=TrueResult
		;   TrueResult19=[]
		)
	    ;   TrueResult19=[]
	    ),
	    TrueResult19\==[],
	    TrueResult32=TrueResult19
	->  true
	;   f_u_ty_c36_instance_c63(Con_Param, u_believe, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_ob_c63([u_ob_c36_get, u_con, [quote, u_obj]], IFTEST23),
		(   IFTEST23\==[]
		->  f_u_ob_c36_get(Con_Param, u_obj, Obj39),
		    f_u_ty_c36_instance_c63(Obj39, u_active_goal, IFTEST25),
		    (   IFTEST25\==[]
		    ->  f_u_ob_c63([u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]],
				   IFTEST28),
			(   IFTEST28\==[]
			->  f_u_ob_c36_pget(Con_Param,
					    [u_obj, u_obj],
					    Instance_c63_Param),
			    f_u_ty_c36_instance_c63(Instance_c63_Param,
						    u_rtrue,
						    TrueResult31),
			    TrueResult32=TrueResult31
			;   TrueResult32=[]
			)
		    ;   TrueResult32=[]
		    )
		;   TrueResult32=[]
		)
	    ;   TrueResult32=[]
	    )
	).
:- set_opv(f_u_rtrue_subgoal_c63, classof, claz_function),
   set_opv(u_rtrue_subgoal_c63, compile_as, kw_function),
   set_opv(u_rtrue_subgoal_c63, function, f_u_rtrue_subgoal_c63),
   DefunResult=u_rtrue_subgoal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'discourse-reset',
			    [],
			    [setq, '*references*', []]
			  ]).

% annotating U::DISCOURSE-RESET 
wl: lambda_def(defun,
	      u_discourse_reset,
	      f_u_discourse_reset,
	      [],
	      [[setq, u_xx_references_xx, []]]).


% annotating U::DISCOURSE-RESET 
wl: arglist_info(u_discourse_reset,
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

% annotating U::DISCOURSE-RESET 
wl: init_args(exact_only, u_discourse_reset).


% annotating U::DISCOURSE-RESET 
f_u_discourse_reset(FnResult) :-
	Env=[],
	set_var(Env, setq, u_xx_references_xx, []),
	[]=FnResult.
:- set_opv(f_u_discourse_reset, classof, claz_function),
   set_opv(u_discourse_reset, compile_as, kw_function),
   set_opv(u_discourse_reset, function, f_u_discourse_reset),
   DefunResult=u_discourse_reset.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3242)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Imagined future assumption === past-subjunctive",
				     1,
				     3244)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" \"What if I went to the store?\"",
				     1,
				     3294)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Imagined future === conditional",
				     1,
				     3327)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" \"I would go to the store\"", 1, 3361)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Imagined past assumption === past-perfect",
				     1,
				     3389)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" \"What if I had gone to the store?\"",
				     1,
				     3433)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Imagined past === conditional-present-perfect",
				     1,
				     3470)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" \"I would have gone to the store\"",
				     1,
				     3518)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3553)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below doesn't work (?) because obs can change (or are they copied",
				     1,
				     3556)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" for context's sake?)", 1, 3628)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3651)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(setq *gen-history* nil)", 1, 3653)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3679)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("(defun gen-history ()", 1, 3681)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (setq *references* nil)", 1, 3704)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (yloop (yfor item in (reverse *gen-history*))",
				     1,
				     3731)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (ydo (generate1 (car item)",
				     1,
				     3780)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                       (cadr item)",
				     1,
				     3816)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                       (caddr item)",
				     1,
				     3852)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                       (cadddr item)))))",
				     1,
				     3889)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Will add paragraph to English-only trace.",
				     1,
				     3932)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:3975 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-new-paragraph',
			    [],
			    [setq, '*references*', []],
			    []
			  ]).

% annotating U::GEN-NEW-PARAGRAPH 
wl: lambda_def(defun,
	      u_gen_new_paragraph,
	      f_u_gen_new_paragraph,
	      [],
	      [[setq, u_xx_references_xx, []], []]).


% annotating U::GEN-NEW-PARAGRAPH 
wl: arglist_info(u_gen_new_paragraph,
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

% annotating U::GEN-NEW-PARAGRAPH 
wl: init_args(exact_only, u_gen_new_paragraph).


% annotating U::GEN-NEW-PARAGRAPH 
f_u_gen_new_paragraph(FnResult) :-
	Env=[],
	set_var(Env, setq, u_xx_references_xx, []),
	[]=FnResult.
:- set_opv(f_u_gen_new_paragraph, classof, claz_function),
   set_opv(u_gen_new_paragraph, compile_as, kw_function),
   set_opv(u_gen_new_paragraph, function, f_u_gen_new_paragraph),
   DefunResult=u_gen_new_paragraph.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4037 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*possibly-realism*', 0.3]).
:- set_var(TLEnv3, setq, u_xx_possibly_realism_xx, 0.3).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4037 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("   Perf   Dd", 1, 4069)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4037 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   ------------------+", 1, 4083)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4037 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   Maybe    Say      | Subgoal relaxation",
				     1,
				     4107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4037 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   Possibly Possibly | Plausible planning",
				     1,
				     4150)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    generate1,
			    [con, switches, context, bp],
			    
			    [ let,
			      [['gened?', []]],
			      
			      [ if,
				'*typeset?*',
				
				[ format,
				  '*gate-dbg*',
				  '$STRING'("\\vspace{2 mm}~%")
				]
			      ],
			      ['ndbg-large-bold-font', '*gate-dbg*', rule],
			      ['gs-reset-sentence', '*gen-stream*'],
			      ['gs-reset-line', '*gen-stream*'],
			      
			      [ if,
				
				[ and,
				  [<, [strength, con], '*possibly-realism*'],
				  [not, ['ty$instance?', con, [quote, surprise]]],
				  
				  [ not,
				    
				    [ 'ty$instance?',
				      con,
				      [quote, 'overall-emotion']
				    ]
				  ],
				  
				  [ not,
				    
				    [ 'switches-lookup',
				      [quote, 'tongue-in-cheek'],
				      switches
				    ]
				  ]
				],
				
				[ 'gs-string-write',
				  '*gen-stream*',
				  '$STRING'(" possibly")
				]
			      ],
			      
			      [ cond,
				
				[ 
				  [ 'switches-lookup',
				    [quote, 'what-if'],
				    switches
				  ],
				  
				  [ 'gs-string-write',
				    '*gen-stream*',
				    '$STRING'(" what")
				  ],
				  
				  [ 'gs-string-write',
				    '*gen-stream*',
				    '$STRING'(" if")
				  ],
				  
				  [ if,
				    
				    [ setq,
				      'gened?',
				      
				      [ gen,
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ]
				    ],
				    
				    [ progn,
				      
				      [ 'justify-if-desired',
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ],
				      ['gs-end-question', '*gen-stream*']
				    ]
				  ]
				],
				
				[ 
				  [ 'switches-lookup',
				    [quote, 'tongue-in-cheek'],
				    switches
				  ],
				  
				  [ 'gs-string-write',
				    '*gen-stream*',
				    '$STRING'(" anyway,")
				  ],
				  
				  [ if,
				    
				    [ setq,
				      'gened?',
				      
				      [ gen,
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ]
				    ],
				    
				    [ progn,
				      
				      [ 'justify-if-desired',
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ],
				      ['gs-end-sentence', '*gen-stream*']
				    ]
				  ]
				],
				
				[ 
				  [ 'switches-lookup',
				    [quote, relaxation],
				    switches
				  ],
				  
				  [ if,
				    ['performance-mode?'],
				    
				    [ 'gs-string-write',
				      '*gen-stream*',
				      '$STRING'(" maybe")
				    ],
				    
				    [ 'gs-string-write',
				      '*gen-stream*',
				      '$STRING'(" say")
				    ]
				  ],
				  
				  [ if,
				    
				    [ setq,
				      'gened?',
				      
				      [ gen,
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ]
				    ],
				    
				    [ progn,
				      
				      [ 'justify-if-desired',
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ],
				      ['gs-end-sentence', '*gen-stream*']
				    ]
				  ]
				],
				
				[ 
				  [ 'switches-lookup',
				    [quote, backtrack],
				    switches
				  ],
				  
				  [ 'gs-string-write',
				    '*gen-stream*',
				    '$STRING'(" no")
				  ],
				  [setq, 'gened?', t],
				  ['gs-end-sentence', '*gen-stream*']
				],
				
				[ else,
				  
				  [ if,
				    
				    [ setq,
				      'gened?',
				      
				      [ gen,
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ]
				    ],
				    
				    [ progn,
				      
				      [ 'justify-if-desired',
					con,
					'*gen-stream*',
					switches,
					context,
					bp
				      ],
				      
				      [ if,
					['ty$instance?', con, [quote, surprise]],
					['gs-end-exclam', '*gen-stream*'],
					['gs-end-sentence', '*gen-stream*']
				      ]
				    ]
				  ]
				]
			      ],
			      ['ndbg-end-font', '*gate-dbg*', rule],
			      
			      [ if,
				'gened?',
				
				[ progn,
				  
				  [ if,
				    '*typeset?*',
				    
				    [ format,
				      '*gate-dbg*',
				      '$STRING'("\\vspace{2 mm}~%")
				    ]
				  ],
				  
				  [ if,
				    
				    [ 'null?',
				      
				      [ 'ob$get',
					context,
					[quote, 'first-gened-concept']
				      ]
				    ],
				    
				    [ 'ob$set',
				      context,
				      [quote, 'first-gened-concept'],
				      con
				    ]
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  [],
				  [not, ['ty$instance?', con, [quote, surprise]]],
				  
				  [ not,
				    
				    [ 'ty$instance?',
				      con,
				      [quote, 'overall-emotion']
				    ]
				  ]
				],
				['gen-overall-emot-state']
			      ]
			    ]
			  ]).

% annotating U::GENERATE1 
wl: lambda_def(defun,
	      u_generate1,
	      f_u_generate1,
	      [u_con, u_switches, u_context, u_bp],
	      
	      [ 
		[ let,
		  [[u_gened_c63, []]],
		  
		  [ if,
		    u_xx_typeset_c63_xx,
		    
		    [ format,
		      u_xx_gate_dbg_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(\),
				 #\(v),
				 #\(s),
				 #\(p),
				 #\(a),
				 #\(c),
				 #\(e),
				 #\('{'),
				 #\('2'),
				 #\(' '),
				 #\(m),
				 #\(m),
				 #\('}'),
				 #\(~),
				 #\('%')
			       ])
		    ]
		  ],
		  [u_ndbg_large_bold_font, u_xx_gate_dbg_xx, u_rule],
		  [u_gs_reset_sentence, u_xx_gen_stream_xx],
		  [u_gs_reset_line, u_xx_gen_stream_xx],
		  
		  [ if,
		    
		    [ and,
		      [<, [u_strength, u_con], u_xx_possibly_realism_xx],
		      [not, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]]],
		      
		      [ not,
			
			[ u_ty_c36_instance_c63,
			  u_con,
			  [quote, u_overall_emotion]
			]
		      ],
		      
		      [ not,
			
			[ u_switches_lookup,
			  [quote, u_tongue_in_cheek],
			  u_switches
			]
		      ]
		    ],
		    
		    [ u_gs_string_write,
		      u_xx_gen_stream_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(p),
				 #\(o),
				 #\(s),
				 #\(s),
				 #\(i),
				 #\(b),
				 #\(l),
				 #\(y)
			       ])
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_switches_lookup, [quote, u_what_if], u_switches],
		      
		      [ u_gs_string_write,
			u_xx_gen_stream_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(w), #\(h), #\(a), #\(t)])
		      ],
		      
		      [ u_gs_string_write,
			u_xx_gen_stream_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(i), #\(f)])
		      ],
		      
		      [ if,
			
			[ setq,
			  u_gened_c63,
			  
			  [ u_gen,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ progn,
			  
			  [ u_justify_if_desired,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  [u_gs_end_question, u_xx_gen_stream_xx]
			]
		      ]
		    ],
		    
		    [ [u_switches_lookup, [quote, u_tongue_in_cheek], u_switches],
		      
		      [ u_gs_string_write,
			u_xx_gen_stream_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(n),
				   #\(y),
				   #\(w),
				   #\(a),
				   #\(y),
				   #\(',')
				 ])
		      ],
		      
		      [ if,
			
			[ setq,
			  u_gened_c63,
			  
			  [ u_gen,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ progn,
			  
			  [ u_justify_if_desired,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  [u_gs_end_sentence, u_xx_gen_stream_xx]
			]
		      ]
		    ],
		    
		    [ [u_switches_lookup, [quote, u_relaxation], u_switches],
		      
		      [ if,
			[u_performance_mode_c63],
			
			[ u_gs_string_write,
			  u_xx_gen_stream_xx,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(' '), #\(m), #\(a), #\(y), #\(b), #\(e)])
			],
			
			[ u_gs_string_write,
			  u_xx_gen_stream_xx,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(' '), #\(s), #\(a), #\(y)])
			]
		      ],
		      
		      [ if,
			
			[ setq,
			  u_gened_c63,
			  
			  [ u_gen,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ progn,
			  
			  [ u_justify_if_desired,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  [u_gs_end_sentence, u_xx_gen_stream_xx]
			]
		      ]
		    ],
		    
		    [ [u_switches_lookup, [quote, u_backtrack], u_switches],
		      
		      [ u_gs_string_write,
			u_xx_gen_stream_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(n), #\(o)])
		      ],
		      [setq, u_gened_c63, t],
		      [u_gs_end_sentence, u_xx_gen_stream_xx]
		    ],
		    
		    [ u_else,
		      
		      [ if,
			
			[ setq,
			  u_gened_c63,
			  
			  [ u_gen,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ progn,
			  
			  [ u_justify_if_desired,
			    u_con,
			    u_xx_gen_stream_xx,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_con, [quote, u_surprise]],
			    [u_gs_end_exclam, u_xx_gen_stream_xx],
			    [u_gs_end_sentence, u_xx_gen_stream_xx]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_ndbg_end_font, u_xx_gate_dbg_xx, u_rule],
		  
		  [ if,
		    u_gened_c63,
		    
		    [ progn,
		      
		      [ if,
			u_xx_typeset_c63_xx,
			
			[ format,
			  u_xx_gate_dbg_xx,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(\),
				     #\(v),
				     #\(s),
				     #\(p),
				     #\(a),
				     #\(c),
				     #\(e),
				     #\('{'),
				     #\('2'),
				     #\(' '),
				     #\(m),
				     #\(m),
				     #\('}'),
				     #\(~),
				     #\('%')
				   ])
			]
		      ],
		      
		      [ if,
			
			[ u_null_c63,
			  
			  [ u_ob_c36_get,
			    u_context,
			    [quote, u_first_gened_concept]
			  ]
			],
			
			[ u_ob_c36_set,
			  u_context,
			  [quote, u_first_gened_concept],
			  u_con
			]
		      ]
		    ]
		  ],
		  
		  [ if,
		    
		    [ and,
		      [],
		      [not, [u_ty_c36_instance_c63, u_con, [quote, u_surprise]]],
		      
		      [ not,
			
			[ u_ty_c36_instance_c63,
			  u_con,
			  [quote, u_overall_emotion]
			]
		      ]
		    ],
		    [u_gen_overall_emot_state]
		  ]
		]
	      ]).


% annotating U::GENERATE1 
wl: arglist_info(u_generate1,
		[u_con, u_switches, u_context, u_bp],
		[Con_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_con, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_con, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GENERATE1 
wl: init_args(exact_only, u_generate1).


% annotating U::GENERATE1 
f_u_generate1(Con_Param, Switches_Param, Context_Param, Bp_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param)],
	LEnv=[[bv(u_gened_c63, [])]|Env],
	get_var(LEnv, u_xx_typeset_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(\),
				   #\(v),
				   #\(s),
				   #\(p),
				   #\(a),
				   #\(c),
				   #\(e),
				   #\('{'),
				   #\('2'),
				   #\(' '),
				   #\(m),
				   #\(m),
				   #\('}'),
				   #\(~),
				   #\('%')
				 ])
		      ],
		      TrueResult),
	    _68372=TrueResult
	;   _68372=[]
	),
	f_u_ndbg_large_bold_font(u_xx_gate_dbg_xx, u_rule, Rule),
	get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get),
	f_u_gs_reset_sentence(Xx_gen_stream_xx_Get, Reset_sentence_Ret),
	get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get27),
	f_u_gs_reset_line(Xx_gen_stream_xx_Get27, Reset_line_Ret),
	f_u_strength(u_con, PredArg1Result),
	get_var(LEnv, u_xx_possibly_realism_xx, Xx_possibly_realism_xx_Get),
	(   PredArg1Result<Xx_possibly_realism_xx_Get
	->  f_u_ty_c36_instance_c63(Con_Param, u_surprise, PredArgResult),
	    (   PredArgResult==[]
	    ->  f_u_ty_c36_instance_c63(Con_Param,
					u_overall_emotion,
					PredArgResult42),
		(   PredArgResult42==[]
		->  f_u_switches_lookup(u_tongue_in_cheek,
					Switches_Param,
					Not_Param),
		    cl_not(Not_Param, TrueResult44),
		    IFTEST28=TrueResult44
		;   IFTEST28=[]
		)
	    ;   IFTEST28=[]
	    )
	;   IFTEST28=[]
	),
	(   IFTEST28\==[]
	->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get47),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get47,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(p),
					   #\(o),
					   #\(s),
					   #\(s),
					   #\(i),
					   #\(b),
					   #\(l),
					   #\(y)
					 ]),
				TrueResult48),
	    _68654=TrueResult48
	;   _68654=[]
	),
	f_u_switches_lookup(u_what_if, Switches_Param, IFTEST49),
	(   IFTEST49\==[]
	->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get52),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get52,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(w), #\(h), #\(a), #\(t)]),
				String_write_Ret),
	    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get53),
	    f_u_gs_string_write(Xx_gen_stream_xx_Get53,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(i), #\(f)]),
				String_write_Ret182),
	    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get57),
	    f_u_gen(Con_Param,
		    Xx_gen_stream_xx_Get57,
		    Switches_Param,
		    Context_Param,
		    Bp_Param,
		    IFTEST54),
	    set_var(LEnv, u_gened_c63, IFTEST54),
	    (   IFTEST54\==[]
	    ->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get62),
		f_u_justify_if_desired(Con_Param,
				       Xx_gen_stream_xx_Get62,
				       Switches_Param,
				       Context_Param,
				       Bp_Param,
				       If_desired_Ret),
		get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get66),
		f_u_gs_end_question(Xx_gen_stream_xx_Get66, TrueResult67),
		TrueResult136=TrueResult67
	    ;   TrueResult136=[]
	    )
	;   f_u_switches_lookup(u_tongue_in_cheek, Switches_Param, IFTEST68),
	    (   IFTEST68\==[]
	    ->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get71),
		f_u_gs_string_write(Xx_gen_stream_xx_Get71,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(' '),
					       #\(a),
					       #\(n),
					       #\(y),
					       #\(w),
					       #\(a),
					       #\(y),
					       #\(',')
					     ]),
				    String_write_Ret184),
		get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get75),
		f_u_gen(Con_Param,
			Xx_gen_stream_xx_Get75,
			Switches_Param,
			Context_Param,
			Bp_Param,
			IFTEST72),
		set_var(LEnv, u_gened_c63, IFTEST72),
		(   IFTEST72\==[]
		->  get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get80),
		    f_u_justify_if_desired(Con_Param,
					   Xx_gen_stream_xx_Get80,
					   Switches_Param,
					   Context_Param,
					   Bp_Param,
					   If_desired_Ret185),
		    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get84),
		    f_u_gs_end_sentence(Xx_gen_stream_xx_Get84, TrueResult85),
		    TrueResult136=TrueResult85
		;   TrueResult136=[]
		)
	    ;   f_u_switches_lookup(u_relaxation, Switches_Param, IFTEST86),
		(   IFTEST86\==[]
		->  f_u_performance_mode_c63(IFTEST89),
		    (   IFTEST89\==[]
		    ->  get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get91),
			f_u_gs_string_write(Xx_gen_stream_xx_Get91,
					    '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\(' '),
						       #\(m),
						       #\(a),
						       #\(y),
						       #\(b),
						       #\(e)
						     ]),
					    TrueResult93),
			_70338=TrueResult93
		    ;   get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get92),
			f_u_gs_string_write(Xx_gen_stream_xx_Get92,
					    '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\(' '),
						       #\(s),
						       #\(a),
						       #\(y)
						     ]),
					    ElseResult),
			_70338=ElseResult
		    ),
		    get_var(LEnv, u_xx_gen_stream_xx, Xx_gen_stream_xx_Get98),
		    f_u_gen(Con_Param,
			    Xx_gen_stream_xx_Get98,
			    Switches_Param,
			    Context_Param,
			    Bp_Param,
			    IFTEST95),
		    set_var(LEnv, u_gened_c63, IFTEST95),
		    (   IFTEST95\==[]
		    ->  get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get103),
			f_u_justify_if_desired(Con_Param,
					       Xx_gen_stream_xx_Get103,
					       Switches_Param,
					       Context_Param,
					       Bp_Param,
					       If_desired_Ret186),
			get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get107),
			f_u_gs_end_sentence(Xx_gen_stream_xx_Get107,
					    TrueResult108),
			TrueResult136=TrueResult108
		    ;   TrueResult136=[]
		    )
		;   f_u_switches_lookup(u_backtrack, Switches_Param, IFTEST109),
		    (   IFTEST109\==[]
		    ->  get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get112),
			f_u_gs_string_write(Xx_gen_stream_xx_Get112,
					    '$ARRAY'([*],
						     claz_base_character,
						     [#\(' '), #\(n), #\(o)]),
					    String_write_Ret187),
			set_var(LEnv, setq, u_gened_c63, t),
			get_var(LEnv,
				u_xx_gen_stream_xx,
				Xx_gen_stream_xx_Get113),
			f_u_gs_end_sentence(Xx_gen_stream_xx_Get113,
					    TrueResult139),
			TrueResult136=TrueResult139
		    ;   get_var(LEnv, u_else, IFTEST114),
			(   IFTEST114\==[]
			->  get_var(LEnv,
				    u_xx_gen_stream_xx,
				    Xx_gen_stream_xx_Get120),
			    f_u_gen(Con_Param,
				    Xx_gen_stream_xx_Get120,
				    Switches_Param,
				    Context_Param,
				    Bp_Param,
				    IFTEST117),
			    set_var(LEnv, u_gened_c63, IFTEST117),
			    (   IFTEST117\==[]
			    ->  get_var(LEnv,
					u_xx_gen_stream_xx,
					Xx_gen_stream_xx_Get125),
				f_u_justify_if_desired(Con_Param,
						       Xx_gen_stream_xx_Get125,
						       Switches_Param,
						       Context_Param,
						       Bp_Param,
						       If_desired_Ret188),
				f_u_ty_c36_instance_c63(Con_Param,
							u_surprise,
							IFTEST129),
				(   IFTEST129\==[]
				->  get_var(LEnv,
					    u_xx_gen_stream_xx,
					    Xx_gen_stream_xx_Get132),
				    f_u_gs_end_exclam(Xx_gen_stream_xx_Get132,
						      TrueResult134),
				    TrueResult136=TrueResult134
				;   get_var(LEnv,
					    u_xx_gen_stream_xx,
					    Xx_gen_stream_xx_Get133),
				    f_u_gs_end_sentence(Xx_gen_stream_xx_Get133,
							ElseResult135),
				    TrueResult136=ElseResult135
				)
			    ;   TrueResult136=[]
			    )
			;   TrueResult136=[]
			)
		    )
		)
	    )
	),
	f_u_ndbg_end_font(u_xx_gate_dbg_xx, u_rule, Rule176),
	get_var(LEnv, u_gened_c63, IFTEST147),
	(   IFTEST147\==[]
	->  get_var(LEnv, u_xx_typeset_c63_xx, IFTEST150),
	    (   IFTEST150\==[]
	    ->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get153),
		cl_format(
			  [ Xx_gate_dbg_xx_Get153,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(\),
				       #\(v),
				       #\(s),
				       #\(p),
				       #\(a),
				       #\(c),
				       #\(e),
				       #\('{'),
				       #\('2'),
				       #\(' '),
				       #\(m),
				       #\(m),
				       #\('}'),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  TrueResult154),
		_72252=TrueResult154
	    ;   _72252=[]
	    ),
	    f_u_null_c63(
			 [ u_ob_c36_get,
			   u_context,
			   [quote, u_first_gened_concept]
			 ],
			 IFTEST155),
	    (   IFTEST155\==[]
	    ->  f_u_ob_c36_set(Context_Param,
			       u_first_gened_concept,
			       Con_Param,
			       TrueResult159),
		TrueResult160=TrueResult159
	    ;   TrueResult160=[]
	    )
	;   TrueResult160=[]
	),
	(   []\==[]
	->  f_u_ty_c36_instance_c63(Con_Param, u_surprise, PredArgResult168),
	    (   PredArgResult168==[]
	    ->  f_u_ty_c36_instance_c63(Con_Param,
					u_overall_emotion,
					Overall_emotion),
		cl_not(Overall_emotion, TrueResult170),
		IFTEST161=TrueResult170
	    ;   IFTEST161=[]
	    )
	;   IFTEST161=[]
	),
	(   IFTEST161\==[]
	->  f_u_gen_overall_emot_state(TrueResult172),
	    FnResult=TrueResult172
	;   FnResult=[]
	).
:- set_opv(f_u_generate1, classof, claz_function),
   set_opv(u_generate1, compile_as, kw_function),
   set_opv(u_generate1, function, f_u_generate1),
   DefunResult=u_generate1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   (setq *gen-history* (cons (list con switches context bp) *gen-history*))",
				     1,
				     4259)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("     (gs-end-sentence *gen-stream*)",
				     1,
				     5866)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("     (gs-string-write *gen-stream* \" instead of\")",
				     1,
				     5903)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("     (setq gened? (gen con *gen-stream* switches context bp))",
				     1,
				     5954)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("     (gs-string-write *gen-stream* \", how about this\")",
				     1,
				     6017)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("     ; A justification will never be used here.",
				     1,
				     6073)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:4193 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ty$instance? con 'emotion) No more overall gen",
				     18,
				     6732)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:6922 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'justify-if-desired',
			    [con, stream, switches, context, bp],
			    
			    [ if,
			      ['switches-lookup', [quote, 'justify?'], switches],
			      [justify, con, stream, switches, context, bp]
			    ]
			  ]).

% annotating U::JUSTIFY-IF-DESIRED 
wl: lambda_def(defun,
	      u_justify_if_desired,
	      f_u_justify_if_desired,
	      [u_con, stream, u_switches, u_context, u_bp],
	      
	      [ 
		[ if,
		  [u_switches_lookup, [quote, u_justify_c63], u_switches],
		  [u_justify, u_con, stream, u_switches, u_context, u_bp]
		]
	      ]).


% annotating U::JUSTIFY-IF-DESIRED 
wl: arglist_info(u_justify_if_desired,
		[u_con, stream, u_switches, u_context, u_bp],
		[Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_con, stream, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, stream, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_con, stream, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::JUSTIFY-IF-DESIRED 
wl: init_args(exact_only, u_justify_if_desired).


% annotating U::JUSTIFY-IF-DESIRED 
f_u_justify_if_desired(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, FnResult) :-
	f_u_switches_lookup(u_justify_c63, Switches_Param, IFTEST),
	(   IFTEST\==[]
	->  f_u_justify(Con_Param,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_justify_if_desired, classof, claz_function),
   set_opv(u_justify_if_desired, compile_as, kw_function),
   set_opv(u_justify_if_desired, function, f_u_justify_if_desired),
   DefunResult=u_justify_if_desired.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7074 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    justify,
			    [con, stream, switches, context, bp],
			    
			    [ let,
			      
			      [ 
				[ causes,
				  
				  [ prune,
				    ['get-leaf-causes', con, context],
				    [lambda, [x], ['ob$ty', x]]
				  ]
				]
			      ],
			      
			      [ if,
				[and, causes, ['neq?', [car, causes], con]],
				
				[ progn,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" because")
				  ],
				  
				  [ 'generate-list',
				    causes,
				    stream,
				    switches,
				    context,
				    bp
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::JUSTIFY 
wl: lambda_def(defun,
	      u_justify,
	      f_u_justify,
	      [u_con, stream, u_switches, u_context, u_bp],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_causes,
		      
		      [ u_prune,
			[u_get_leaf_causes, u_con, u_context],
			[lambda, [u_x], [u_ob_c36_ty, u_x]]
		      ]
		    ]
		  ],
		  
		  [ if,
		    [and, u_causes, [u_neq_c63, [car, u_causes], u_con]],
		    
		    [ progn,
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(b),
				   #\(e),
				   #\(c),
				   #\(a),
				   #\(u),
				   #\(s),
				   #\(e)
				 ])
		      ],
		      
		      [ u_generate_list,
			u_causes,
			stream,
			u_switches,
			u_context,
			u_bp
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::JUSTIFY 
wl: arglist_info(u_justify,
		[u_con, stream, u_switches, u_context, u_bp],
		[Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_con, stream, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, stream, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_con, stream, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::JUSTIFY 
wl: init_args(exact_only, u_justify).


% annotating U::JUSTIFY 
f_u_justify(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param)],
	f_u_get_leaf_causes(Con_Param, Context_Param, Prune_Param),
	Lambda=closure([ClosureEnvironment|Env], LResult, [u_x], f_u_ob_c36_ty(u_x, LResult)),
	f_u_prune(Prune_Param, Lambda, Causes_Init),
	LEnv=[[bv(u_causes, Causes_Init)]|Env],
	get_var(LEnv, u_causes, IFTEST30),
	(   IFTEST30\==[]
	->  f_u_neq_c63([car, u_causes], u_con, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_gs_string_write(Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(b),
					   #\(e),
					   #\(c),
					   #\(a),
					   #\(u),
					   #\(s),
					   #\(e)
					 ]),
				String_write_Ret),
	    get_var(LEnv, u_causes, Causes_Get36),
	    f_u_generate_list(Causes_Get36,
			      Stream_Param,
			      Switches_Param,
			      Context_Param,
			      Bp_Param,
			      TrueResult41),
	    FnResult=TrueResult41
	;   FnResult=[]
	).
:- set_opv(f_u_justify, classof, claz_function),
   set_opv(u_justify, compile_as, kw_function),
   set_opv(u_justify, function, f_u_justify),
   DefunResult=u_justify.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7412 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'generate-list',
			    [lst, stream, switches, context, bp],
			    
			    [ yloop,
			      [ywhile, lst],
			      
			      [ ydo,
				[gen, [car, lst], stream, switches, context, bp],
				[setq, lst, [cdr, lst]],
				
				[ if,
				  lst,
				  ['gs-string-write', stream, '$STRING'(" and")]
				]
			      ]
			    ]
			  ]).

% annotating U::GENERATE-LIST 
wl: lambda_def(defun,
	      u_generate_list,
	      f_u_generate_list,
	      [u_lst, stream, u_switches, u_context, u_bp],
	      
	      [ 
		[ u_yloop,
		  [u_ywhile, u_lst],
		  
		  [ u_ydo,
		    [u_gen, [car, u_lst], stream, u_switches, u_context, u_bp],
		    [setq, u_lst, [cdr, u_lst]],
		    
		    [ if,
		      u_lst,
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(a), #\(n), #\(d)])
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GENERATE-LIST 
wl: arglist_info(u_generate_list,
		[u_lst, stream, u_switches, u_context, u_bp],
		[Lst_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_lst, stream, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst, stream, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_lst, stream, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GENERATE-LIST 
wl: init_args(exact_only, u_generate_list).


% annotating U::GENERATE-LIST 
f_u_generate_list(Lst_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param)],
	f_u_yloop(
		  [ [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      [u_gen, [car, u_lst], stream, u_switches, u_context, u_bp],
		      [setq, u_lst, [cdr, u_lst]],
		      
		      [ if,
			u_lst,
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(' '), #\(a), #\(n), #\(d)])
			]
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_generate_list, classof, claz_function),
   set_opv(u_generate_list, compile_as, kw_function),
   set_opv(u_generate_list, function, f_u_generate_list),
   DefunResult=u_generate_list.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7412 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(use-font *dd-output* (load-font *dd-output* \"/sys/dm/fonts/std.19l\"))",
				     1,
				     7623)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7695 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'name-is?',
			    [ob, name],
			    [and, ['ob?', ob], ['eq?', name, ['ob$name', ob]]]
			  ]).

% annotating U::NAME-IS? 
wl: lambda_def(defun,
	      u_name_is_c63,
	      f_u_name_is_c63,
	      [u_ob, sys_name],
	      [[and, [u_ob_c63, u_ob], [u_eq_c63, sys_name, [u_ob_c36_name, u_ob]]]]).


% annotating U::NAME-IS? 
wl: arglist_info(u_name_is_c63,
		[u_ob, sys_name],
		[Ob_Param, Name_Param],
		arginfo{ all:[u_ob, sys_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, sys_name],
			 opt:0,
			 req:[u_ob, sys_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NAME-IS? 
wl: init_args(exact_only, u_name_is_c63).


% annotating U::NAME-IS? 
f_u_name_is_c63(Ob_Param, Name_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(sys_name, Name_Param)],
	f_u_ob_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(sys_name, [u_ob_c36_name, u_ob], TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_name_is_c63, classof, claz_function),
   set_opv(u_name_is_c63, compile_as, kw_function),
   set_opv(u_name_is_c63, function, f_u_name_is_c63),
   DefunResult=u_name_is_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-gen-proc',
			    [type],
			    
			    [ or,
			      ['ob$get', type, [quote, gen]],
			      
			      [ and,
				['ob$gets', type, [quote, isa]],
				['get-gen-proc', ['ob$get', type, [quote, isa]]]
			      ]
			    ]
			  ]).

% annotating U::GET-GEN-PROC 
wl: lambda_def(defun,
	      u_get_gen_proc,
	      f_u_get_gen_proc,
	      [type],
	      
	      [ 
		[ or,
		  [u_ob_c36_get, type, [quote, u_gen]],
		  
		  [ and,
		    [u_ob_c36_gets, type, [quote, u_isa]],
		    [u_get_gen_proc, [u_ob_c36_get, type, [quote, u_isa]]]
		  ]
		]
	      ]).


% annotating U::GET-GEN-PROC 
wl: arglist_info(u_get_gen_proc,
		[type],
		[Type_Param],
		arginfo{ all:[type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[type],
			 opt:0,
			 req:[type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-GEN-PROC 
wl: init_args(exact_only, u_get_gen_proc).


% annotating U::GET-GEN-PROC 
f_u_get_gen_proc(Type_Param, FnResult) :-
	(   f_u_ob_c36_get(Type_Param, u_gen, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_ob_c36_gets(Type_Param, u_isa, IFTEST),
	    (   IFTEST\==[]
	    ->  f_u_ob_c36_get(Type_Param, u_isa, Isa),
		f_u_get_gen_proc(Isa, TrueResult),
		FnResult=TrueResult
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_get_gen_proc, classof, claz_function),
   set_opv(u_get_gen_proc, compile_as, kw_function),
   set_opv(u_get_gen_proc, function, f_u_get_gen_proc),
   DefunResult=u_get_gen_proc.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    gen,
			    [con, stream, switches, context, bp],
			    
			    [ prog1,
			      
			      [ cond,
				
				[ ['var?', con],
				  
				  [ 'gen-variable',
				    con,
				    stream,
				    switches,
				    context,
				    bp
				  ]
				],
				
				[ ['ob?', con],
				  
				  [ let,
				    
				    [ 
				      [ 'gen-proc',
					['get-gen-proc', ['ob$ty', con]]
				      ]
				    ],
				    
				    [ cond,
				      [['eq?', 'gen-proc', [quote, 'no-gen']], []],
				      
				      [ 'gen-proc',
					
					[ funcall,
					  'gen-proc',
					  con,
					  stream,
					  switches,
					  context,
					  bp
					]
				      ],
				      [else, ['gen-unknown', con, stream]]
				    ]
				  ]
				],
				
				[ [and, ['pair?', con], ['null?', [cdr, con]]],
				  [gen, [car, con], stream, switches, context, bp]
				],
				
				[ ['pair?', con],
				  
				  [ if,
				    ['memq?', '*me-ob*', con],
				    
				    [ progn,
				      
				      [ yloop,
					[initial, [others, ['non-mes', con]]],
					[ywhile, others],
					
					[ ydo,
					  
					  [ gen,
					    [car, others],
					    stream,
					    switches,
					    context,
					    bp
					  ],
					  [setq, others, [cdr, others]]
					]
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" and")
				      ],
				      
				      [ gen,
					'*me-ob*',
					stream,
					switches,
					context,
					bp
				      ]
				    ],
				    
				    [ yloop,
				      [ywhile, con],
				      
				      [ ydo,
					
					[ gen,
					  [car, con],
					  stream,
					  switches,
					  context,
					  bp
					],
					[setq, con, [cdr, con]],
					
					[ if,
					  [and, con, ['null?', [cdr, con]]],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" and")
					  ]
					]
				      ]
				    ]
				  ],
				  con
				],
				[else, ['gen-unknown', con, stream]]
			      ],
			      
			      [ if,
				
				[ and,
				  ['ob?', con],
				  ['ty$instance?', con, [quote, object]],
				  [not, ['exemplar?', con]],
				  ['neq?', con, '*me-ob*'],
				  [not, ['memq?', con, '*references*']]
				],
				
				[ setq,
				  '*references*',
				  
				  [ cons,
				    con,
				    ['del-old-ref', '*references*', con]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GEN 
wl: lambda_def(defun,
	      u_gen,
	      f_u_gen,
	      [u_con, stream, u_switches, u_context, u_bp],
	      
	      [ 
		[ prog1,
		  
		  [ cond,
		    
		    [ [u_var_c63, u_con],
		      [u_gen_variable, u_con, stream, u_switches, u_context, u_bp]
		    ],
		    
		    [ [u_ob_c63, u_con],
		      
		      [ let,
			[[u_gen_proc, [u_get_gen_proc, [u_ob_c36_ty, u_con]]]],
			
			[ cond,
			  [[u_eq_c63, u_gen_proc, [quote, u_no_gen]], []],
			  
			  [ u_gen_proc,
			    
			    [ funcall,
			      u_gen_proc,
			      u_con,
			      stream,
			      u_switches,
			      u_context,
			      u_bp
			    ]
			  ],
			  [u_else, [u_gen_unknown, u_con, stream]]
			]
		      ]
		    ],
		    
		    [ [and, [u_pair_c63, u_con], [u_null_c63, [cdr, u_con]]],
		      [u_gen, [car, u_con], stream, u_switches, u_context, u_bp]
		    ],
		    
		    [ [u_pair_c63, u_con],
		      
		      [ if,
			[u_memq_c63, u_xx_me_ob_xx, u_con],
			
			[ progn,
			  
			  [ u_yloop,
			    [u_initial, [u_others, [u_non_mes, u_con]]],
			    [u_ywhile, u_others],
			    
			    [ u_ydo,
			      
			      [ u_gen,
				[car, u_others],
				stream,
				u_switches,
				u_context,
				u_bp
			      ],
			      [setq, u_others, [cdr, u_others]]
			    ]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(a), #\(n), #\(d)])
			  ],
			  
			  [ u_gen,
			    u_xx_me_ob_xx,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ u_yloop,
			  [u_ywhile, u_con],
			  
			  [ u_ydo,
			    
			    [ u_gen,
			      [car, u_con],
			      stream,
			      u_switches,
			      u_context,
			      u_bp
			    ],
			    [setq, u_con, [cdr, u_con]],
			    
			    [ if,
			      [and, u_con, [u_null_c63, [cdr, u_con]]],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(a), #\(n), #\(d)])
			      ]
			    ]
			  ]
			]
		      ],
		      u_con
		    ],
		    [u_else, [u_gen_unknown, u_con, stream]]
		  ],
		  
		  [ if,
		    
		    [ and,
		      [u_ob_c63, u_con],
		      [u_ty_c36_instance_c63, u_con, [quote, u_object]],
		      [not, [u_exemplar_c63, u_con]],
		      [u_neq_c63, u_con, u_xx_me_ob_xx],
		      [not, [u_memq_c63, u_con, u_xx_references_xx]]
		    ],
		    
		    [ setq,
		      u_xx_references_xx,
		      [cons, u_con, [u_del_old_ref, u_xx_references_xx, u_con]]
		    ]
		  ]
		]
	      ]).


% annotating U::GEN 
wl: arglist_info(u_gen,
		[u_con, stream, u_switches, u_context, u_bp],
		[Con_Get79, Stream_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_con, stream, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, stream, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_con, stream, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN 
wl: init_args(exact_only, u_gen).


% annotating U::GEN 
f_u_gen(Con_Get79, Stream_Param, Switches_Param, Context_Param, Bp_Param, Con_Get79) :-
	Env=[bv(u_con, Con_Get79), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param)],
	f_u_var_c63(u_con, IFTEST),
	(   IFTEST\==[]
	->  f_u_gen_variable(Con_Get79,
			     Stream_Param,
			     Switches_Param,
			     Context_Param,
			     Bp_Param,
			     TrueResult89),
	    Con_Get79=TrueResult89
	;   f_u_ob_c63(u_con, IFTEST27),
	    (   IFTEST27\==[]
	    ->  f_u_ob_c36_ty(u_con, Gen_proc_Param),
		f_u_get_gen_proc(Gen_proc_Param, Gen_proc_Init),
		LEnv=[[bv(u_gen_proc, Gen_proc_Init)]|Env],
		f_u_eq_c63(u_gen_proc, [quote, u_no_gen], IFTEST32),
		(   IFTEST32\==[]
		->  Con_Get79=[]
		;   get_var(LEnv, u_gen_proc, IFTEST34),
		    (   IFTEST34\==[]
		    ->  f_u_gen_proc(Con_Get79,
				     Stream_Param,
				     Switches_Param,
				     Context_Param,
				     Bp_Param,
				     TrueResult50),
			Con_Get79=TrueResult50
		    ;   get_var(LEnv, u_else, IFTEST43),
			(   IFTEST43\==[]
			->  f_u_gen_unknown(Con_Get79, Stream_Param, TrueResult),
			    Con_Get79=TrueResult
			;   Con_Get79=[]
			)
		    )
		)
	    ;   f_u_pair_c63(u_con, IFTEST55),
		(   IFTEST55\==[]
		->  f_u_null_c63([cdr, u_con], TrueResult57),
		    IFTEST53=TrueResult57
		;   IFTEST53=[]
		),
		(   IFTEST53\==[]
		->  cl_car(Con_Get79, Gen_Param),
		    f_u_gen(Gen_Param,
			    Stream_Param,
			    Switches_Param,
			    Context_Param,
			    Bp_Param,
			    TrueResult85),
		    Con_Get79=TrueResult85
		;   f_u_pair_c63(u_con, IFTEST63),
		    (   IFTEST63\==[]
		    ->  f_u_memq_c63(u_xx_me_ob_xx, u_con, IFTEST65),
			(   IFTEST65\==[]
			->  f_u_yloop(
				      [ 
					[ u_initial,
					  [u_others, [u_non_mes, u_con]]
					],
					[u_ywhile, u_others],
					
					[ u_ydo,
					  
					  [ u_gen,
					    [car, u_others],
					    stream,
					    u_switches,
					    u_context,
					    u_bp
					  ],
					  [setq, u_others, [cdr, u_others]]
					]
				      ],
				      Yloop_Ret),
			    f_u_gs_string_write(Stream_Param,
						'$ARRAY'([*],
							 claz_base_character,
							 
							 [ #\(' '),
							   #\(a),
							   #\(n),
							   #\(d)
							 ]),
						String_write_Ret),
			    get_var(Env, u_xx_me_ob_xx, Xx_me_ob_xx_Get),
			    f_u_gen(Xx_me_ob_xx_Get,
				    Stream_Param,
				    Switches_Param,
				    Context_Param,
				    Bp_Param,
				    TrueResult73),
			    _67750=TrueResult73
			;   f_u_yloop(
				      [ [u_ywhile, u_con],
					
					[ u_ydo,
					  
					  [ u_gen,
					    [car, u_con],
					    stream,
					    u_switches,
					    u_context,
					    u_bp
					  ],
					  [setq, u_con, [cdr, u_con]],
					  
					  [ if,
					    
					    [ and,
					      u_con,
					      [u_null_c63, [cdr, u_con]]
					    ],
					    
					    [ u_gs_string_write,
					      stream,
					      '$ARRAY'([*],
						       claz_base_character,
						       
						       [ #\(' '),
							 #\(a),
							 #\(n),
							 #\(d)
						       ])
					    ]
					  ]
					]
				      ],
				      ElseResult74),
			    _67750=ElseResult74
			)
		    ;   get_var(Env, u_else, IFTEST76),
			(   IFTEST76\==[]
			->  f_u_gen_unknown(Con_Get79,
					    Stream_Param,
					    TrueResult81),
			    Con_Get79=TrueResult81
			;   Con_Get79=[]
			)
		    )
		)
	    )
	),
	f_u_ob_c63(u_con, IFTEST93),
	(   IFTEST93\==[]
	->  f_u_ty_c36_instance_c63(Con_Get79, u_object, IFTEST95),
	    (   IFTEST95\==[]
	    ->  f_u_exemplar_c63(Con_Get79, PredArgResult),
		(   PredArgResult==[]
		->  f_u_neq_c63(u_con, u_xx_me_ob_xx, IFTEST102),
		    (   IFTEST102\==[]
		    ->  f_u_memq_c63(u_con,
				     u_xx_references_xx,
				     Xx_references_xx),
			cl_not(Xx_references_xx, TrueResult104),
			IFTEST91=TrueResult104
		    ;   IFTEST91=[]
		    )
		;   IFTEST91=[]
		)
	    ;   IFTEST91=[]
	    )
	;   IFTEST91=[]
	),
	(   IFTEST91\==[]
	->  get_var(Env, u_xx_references_xx, Xx_references_xx_Get),
	    f_u_del_old_ref(Xx_references_xx_Get, Con_Get79, Old_ref_Ret),
	    TrueResult111=[Con_Get79|Old_ref_Ret],
	    set_var(Env, u_xx_references_xx, TrueResult111),
	    _68562=TrueResult111
	;   _68562=[]
	).
:- set_opv(f_u_gen, classof, claz_function),
   set_opv(u_gen, compile_as, kw_function),
   set_opv(u_gen, function, f_u_gen),
   DefunResult=u_gen.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" here we wanted to flush up to top level",
				     37,
				     8134)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For now, the below prevents \"I want to be",
				     12,
				     9108)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" going out with this person.\" Originally,",
				     12,
				     9163)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:7896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" this was used for action mutation generation.",
				     12,
				     9217)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:9491 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'del-type',
			    [lst, typ],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, item, in, lst],
			      
			      [ ydo,
				
				[ if,
				  ['neq?', ['ob$ty', item], typ],
				  [setq, result, [cons, item, result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::DEL-TYPE 
wl: lambda_def(defun,
	      u_del_type,
	      f_u_del_type,
	      [u_lst, u_typ],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, item, u_in, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_neq_c63, [u_ob_c36_ty, item], u_typ],
		      [setq, u_result, [cons, item, u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::DEL-TYPE 
wl: arglist_info(u_del_type,
		[u_lst, u_typ],
		[Lst_Param, Typ_Param],
		arginfo{ all:[u_lst, u_typ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst, u_typ],
			 opt:0,
			 req:[u_lst, u_typ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DEL-TYPE 
wl: init_args(exact_only, u_del_type).


% annotating U::DEL-TYPE 
f_u_del_type(Lst_Param, Typ_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param), bv(u_typ, Typ_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, item, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_neq_c63, [u_ob_c36_ty, item], u_typ],
			[setq, u_result, [cons, item, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_del_type, classof, claz_function),
   set_opv(u_del_type, compile_as, kw_function),
   set_opv(u_del_type, function, f_u_del_type),
   DefunResult=u_del_type.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:9696 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'del-old-ref',
			    [lst, con],
			    
			    [ let,
			      
			      [ 
				[ typ,
				  
				  [ cond,
				    
				    [ 
				      [ 'ty$instance?',
					con,
					[quote, 'male-person']
				      ],
				      '*male-person-ob*'
				    ],
				    
				    [ 
				      [ 'ty$instance?',
					con,
					[quote, 'female-person']
				      ],
				      '*female-person-ob*'
				    ],
				    [else, []]
				  ]
				]
			      ],
			      
			      [ if,
				typ,
				
				[ yloop,
				  [initial, [result, []]],
				  [yfor, item, in, lst],
				  
				  [ ydo,
				    
				    [ if,
				      [not, ['ty$instance-of?', item, typ]],
				      [setq, result, [cons, item, result]]
				    ]
				  ],
				  [yresult, result]
				],
				['del-type', lst, ['ob$ty', con]]
			      ]
			    ]
			  ]).

% annotating U::DEL-OLD-REF 
wl: lambda_def(defun,
	      u_del_old_ref,
	      f_u_del_old_ref,
	      [u_lst, u_con],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_typ,
		      
		      [ cond,
			
			[ [u_ty_c36_instance_c63, u_con, [quote, u_male_person]],
			  u_xx_male_person_ob_xx
			],
			
			[ 
			  [ u_ty_c36_instance_c63,
			    u_con,
			    [quote, u_female_person]
			  ],
			  u_xx_female_person_ob_xx
			],
			[u_else, []]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_typ,
		    
		    [ u_yloop,
		      [u_initial, [u_result, []]],
		      [u_yfor, item, u_in, u_lst],
		      
		      [ u_ydo,
			
			[ if,
			  [not, [u_ty_c36_instance_of_c63, item, u_typ]],
			  [setq, u_result, [cons, item, u_result]]
			]
		      ],
		      [u_yresult, u_result]
		    ],
		    [u_del_type, u_lst, [u_ob_c36_ty, u_con]]
		  ]
		]
	      ]).


% annotating U::DEL-OLD-REF 
wl: arglist_info(u_del_old_ref,
		[u_lst, u_con],
		[Lst_Param, Con_Param],
		arginfo{ all:[u_lst, u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst, u_con],
			 opt:0,
			 req:[u_lst, u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DEL-OLD-REF 
wl: init_args(exact_only, u_del_old_ref).


% annotating U::DEL-OLD-REF 
f_u_del_old_ref(Lst_Param, Con_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param), bv(u_con, Con_Param)],
	f_u_ty_c36_instance_c63(Con_Param, u_male_person, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_male_person_ob_xx, Xx_male_person_ob_xx_Get),
	    ElseResult29=Xx_male_person_ob_xx_Get
	;   f_u_ty_c36_instance_c63(Con_Param, u_female_person, IFTEST20),
	    (   IFTEST20\==[]
	    ->  get_var(Env,
			u_xx_female_person_ob_xx,
			Xx_female_person_ob_xx_Get),
		ElseResult29=Xx_female_person_ob_xx_Get
	    ;   get_var(Env, u_else, IFTEST24),
		(   IFTEST24\==[]
		->  ElseResult29=[]
		;   ElseResult29=[]
		)
	    )
	),
	LEnv=[[bv(u_typ, ElseResult29)]|Env],
	get_var(LEnv, u_typ, IFTEST33),
	(   IFTEST33\==[]
	->  f_u_yloop(
		      [ [u_initial, [u_result, []]],
			[u_yfor, item, u_in, u_lst],
			
			[ u_ydo,
			  
			  [ if,
			    [not, [u_ty_c36_instance_of_c63, item, u_typ]],
			    [setq, u_result, [cons, item, u_result]]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      TrueResult38),
	    FnResult=TrueResult38
	;   f_u_ob_c36_ty(u_con, C36_ty_Ret),
	    f_u_del_type(Lst_Param, C36_ty_Ret, ElseResult39),
	    FnResult=ElseResult39
	).
:- set_opv(f_u_del_old_ref, classof, claz_function),
   set_opv(u_del_old_ref, compile_as, kw_function),
   set_opv(u_del_old_ref, function, f_u_del_old_ref),
   DefunResult=u_del_old_ref.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10210 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-unknown',
			    [con, stream],
			    ['gs-string-write', stream, '$STRING'(" something")],
			    
			    [ if,
			      [not, '*typeset?*'],
			      
			      [ format,
				stream,
				'$STRING'(" (~A)"),
				[if, ['ob?', con], ['ob$name', con], con]
			      ]
			    ],
			    t
			  ]).

% annotating U::GEN-UNKNOWN 
wl: lambda_def(defun,
	      u_gen_unknown,
	      f_u_gen_unknown,
	      [u_con, stream],
	      
	      [ 
		[ u_gs_string_write,
		  stream,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(' '),
			     #\(s),
			     #\(o),
			     #\(m),
			     #\(e),
			     #\(t),
			     #\(h),
			     #\(i),
			     #\(n),
			     #\(g)
			   ])
		],
		
		[ if,
		  [not, u_xx_typeset_c63_xx],
		  
		  [ format,
		    stream,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(' '), #\('('), #\(~), #\('A'), #\(')')]),
		    [if, [u_ob_c63, u_con], [u_ob_c36_name, u_con], u_con]
		  ]
		],
		t
	      ]).


% annotating U::GEN-UNKNOWN 
wl: arglist_info(u_gen_unknown,
		[u_con, stream],
		[Con_Param, Stream_Param],
		arginfo{ all:[u_con, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, stream],
			 opt:0,
			 req:[u_con, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-UNKNOWN 
wl: init_args(exact_only, u_gen_unknown).


% annotating U::GEN-UNKNOWN 
f_u_gen_unknown(Con_Param, Stream_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param)],
	f_u_gs_string_write(Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(s),
				       #\(o),
				       #\(m),
				       #\(e),
				       #\(t),
				       #\(h),
				       #\(i),
				       #\(n),
				       #\(g)
				     ]),
			    String_write_Ret),
	get_var(Env, u_xx_typeset_c63_xx, Xx_typeset_c63_xx_Get),
	(   Xx_typeset_c63_xx_Get==[]
	->  f_u_ob_c63(u_con, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_ob_c36_name(Con_Param, TrueResult),
		CAR=TrueResult
	    ;   CAR=Con_Param
	    ),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\('('), #\(~), #\('A'), #\(')')]),
			CAR
		      ],
		      TrueResult26),
	    _62276=TrueResult26
	;   _62276=[]
	),
	t=FnResult.
:- set_opv(f_u_gen_unknown, classof, claz_function),
   set_opv(u_gen_unknown, compile_as, kw_function),
   set_opv(u_gen_unknown, function, f_u_gen_unknown),
   DefunResult=u_gen_unknown.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10441 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-variable',
			    [var, stream, switches, context, bp],
			    
			    [ let,
			      [[typ, ['variable-type', var]]],
			      
			      [ if,
				['ob?', typ],
				
				[ gen,
				  ['ob$get', typ, [quote, exemplar]],
				  stream,
				  switches,
				  context,
				  bp
				],
				['gen-unknown', var, stream]
			      ],
			      t
			    ]
			  ]).

% annotating U::GEN-VARIABLE 
wl: lambda_def(defun,
	      u_gen_variable,
	      f_u_gen_variable,
	      [u_var, stream, u_switches, u_context, u_bp],
	      
	      [ 
		[ let,
		  [[u_typ, [u_variable_type, u_var]]],
		  
		  [ if,
		    [u_ob_c63, u_typ],
		    
		    [ u_gen,
		      [u_ob_c36_get, u_typ, [quote, u_exemplar]],
		      stream,
		      u_switches,
		      u_context,
		      u_bp
		    ],
		    [u_gen_unknown, u_var, stream]
		  ],
		  t
		]
	      ]).


% annotating U::GEN-VARIABLE 
wl: arglist_info(u_gen_variable,
		[u_var, stream, u_switches, u_context, u_bp],
		[Var_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_var, stream, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var, stream, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_var, stream, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-VARIABLE 
wl: init_args(exact_only, u_gen_variable).


% annotating U::GEN-VARIABLE 
f_u_gen_variable(Var_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, FnResult) :-
	Env=[bv(u_var, Var_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param)],
	f_u_variable_type(u_var, Typ_Init),
	LEnv=[[bv(u_typ, Typ_Init)]|Env],
	f_u_ob_c63(u_typ, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_typ, Typ_Get),
	    f_u_ob_c36_get(Typ_Get, u_exemplar, Exemplar),
	    f_u_gen(Exemplar,
		    Stream_Param,
		    Switches_Param,
		    Context_Param,
		    Bp_Param,
		    TrueResult),
	    _62708=TrueResult
	;   f_u_gen_unknown(Var_Param, Stream_Param, ElseResult),
	    _62708=ElseResult
	),
	LetResult=t,
	LetResult=FnResult.
:- set_opv(f_u_gen_variable, classof, claz_function),
   set_opv(u_gen_variable, compile_as, kw_function),
   set_opv(u_gen_variable, function, f_u_gen_variable),
   DefunResult=u_gen_variable.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'exemplar?',
			    [ob],
			    
			    [ and,
			      ['ob$get', ob, [quote, type]],
			      
			      [ 'eq?',
				ob,
				
				[ 'ob$get',
				  ['ob$get', ob, [quote, type]],
				  [quote, exemplar]
				]
			      ]
			    ]
			  ]).

% annotating U::EXEMPLAR? 
wl: lambda_def(defun,
	      u_exemplar_c63,
	      f_u_exemplar_c63,
	      [u_ob],
	      
	      [ 
		[ and,
		  [u_ob_c36_get, u_ob, [quote, type]],
		  
		  [ u_eq_c63,
		    u_ob,
		    
		    [ u_ob_c36_get,
		      [u_ob_c36_get, u_ob, [quote, type]],
		      [quote, u_exemplar]
		    ]
		  ]
		]
	      ]).


% annotating U::EXEMPLAR? 
wl: arglist_info(u_exemplar_c63,
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

% annotating U::EXEMPLAR? 
wl: init_args(exact_only, u_exemplar_c63).


% annotating U::EXEMPLAR? 
f_u_exemplar_c63(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_ob_c36_get(Ob_Param, type, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_ob,
		       
		       [ u_ob_c36_get,
			 [u_ob_c36_get, u_ob, [quote, type]],
			 [quote, u_exemplar]
		       ],
		       TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_exemplar_c63, classof, claz_function),
   set_opv(u_exemplar_c63, compile_as, kw_function),
   set_opv(u_exemplar_c63, function, f_u_exemplar_c63),
   DefunResult=u_exemplar_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Todo:", 1, 10770)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Allow flagging of subgoals not to be generated.",
				     1,
				     10778)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Add lookup for locations", 1, 10830)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Return subject in gen to enable NP-deletion below in 'by' phrases.",
				     1,
				     10859)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Diff btwn gening, say RPROX infs and RPROXs otherwise, say nested",
				     1,
				     10930)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:10666 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("   in ep.", 1, 11000)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:11010 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'EPISODE',
			    [],
			    
			    [ let,
			      [[goal, ['ob$get', con, [quote, goal]]]],
			      
			      [ 'gen-subgoals',
				goal,
				stream,
				switches,
				context,
				bp,
				t
			      ],
			      '*me-ob*'
			    ]
			  ]).
:- f_u_define_gen(u_episode,
		  [],
		  
		  [ 
		    [ let,
		      [[u_goal, [u_ob_c36_get, u_con, [quote, u_goal]]]],
		      
		      [ u_gen_subgoals,
			u_goal,
			stream,
			u_switches,
			u_context,
			u_bp,
			t
		      ],
		      u_xx_me_ob_xx
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:11139 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'genable-subgoals',
			    [goal, context, 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ links,
				  
				  [ 'get-links-relative',
				    goal,
				    '*intends-ob*',
				    context,
				    'belief-path'
				  ]
				],
				[rule, []],
				['plan-no-gen', []]
			      ],
			      
			      [ if,
				links,
				
				[ progn,
				  
				  [ setq,
				    rule,
				    ['ob$get', [car, links], [quote, rule]]
				  ],
				  
				  [ setq,
				    'plan-no-gen',
				    ['ob$get', rule, [quote, 'plan-no-gen']]
				  ],
				  
				  [ yloop,
				    [initial, [result, []], ['gen?', t]],
				    [yfor, link, in, links],
				    
				    [ ydo,
				      
				      [ if,
					'plan-no-gen',
					
					[ progn,
					  
					  [ setq,
					    'gen?',
					    [not, [car, 'plan-no-gen']]
					  ],
					  
					  [ setq,
					    'plan-no-gen',
					    [cdr, 'plan-no-gen']
					  ]
					]
				      ],
				      
				      [ if,
					'gen?',
					
					[ setq,
					  result,
					  
					  [ 'append!',
					    result,
					    
					    [ list,
					      
					      [ 'ob$get',
						link,
						[quote, 'linked-to']
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
			    ]
			  ]).

% annotating U::GENABLE-SUBGOALS 
wl: lambda_def(defun,
	      u_genable_subgoals,
	      f_u_genable_subgoals,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_links,
		      
		      [ u_get_links_relative,
			u_goal,
			u_xx_intends_ob_xx,
			u_context,
			u_belief_path
		      ]
		    ],
		    [u_rule, []],
		    [u_plan_no_gen, []]
		  ],
		  
		  [ if,
		    u_links,
		    
		    [ progn,
		      
		      [ setq,
			u_rule,
			[u_ob_c36_get, [car, u_links], [quote, u_rule]]
		      ],
		      
		      [ setq,
			u_plan_no_gen,
			[u_ob_c36_get, u_rule, [quote, u_plan_no_gen]]
		      ],
		      
		      [ u_yloop,
			[u_initial, [u_result, []], [u_gen_c63, t]],
			[u_yfor, u_link, u_in, u_links],
			
			[ u_ydo,
			  
			  [ if,
			    u_plan_no_gen,
			    
			    [ progn,
			      [setq, u_gen_c63, [not, [car, u_plan_no_gen]]],
			      [setq, u_plan_no_gen, [cdr, u_plan_no_gen]]
			    ]
			  ],
			  
			  [ if,
			    u_gen_c63,
			    
			    [ setq,
			      u_result,
			      
			      [ u_append_c33,
				u_result,
				
				[ list,
				  [u_ob_c36_get, u_link, [quote, u_linked_to]]
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
		]
	      ]).


% annotating U::GENABLE-SUBGOALS 
wl: arglist_info(u_genable_subgoals,
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

% annotating U::GENABLE-SUBGOALS 
wl: init_args(exact_only, u_genable_subgoals).


% annotating U::GENABLE-SUBGOALS 
f_u_genable_subgoals(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_get_links_relative(Goal_Param,
			       Xx_intends_ob_xx_Get,
			       Context_Param,
			       Belief_path_Param,
			       Links_Init),
	LEnv=[[bv(u_links, Links_Init), bv(u_rule, []), bv(u_plan_no_gen, [])]|Env],
	get_var(LEnv, u_links, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_links, Links_Get27),
	    cl_car(Links_Get27, C36_get_Param),
	    f_u_ob_c36_get(C36_get_Param, u_rule, Rule),
	    set_var(LEnv, u_rule, Rule),
	    get_var(LEnv, u_rule, Rule_Get),
	    f_u_ob_c36_get(Rule_Get, u_plan_no_gen, Plan_no_gen),
	    set_var(LEnv, u_plan_no_gen, Plan_no_gen),
	    f_u_yloop(
		      [ [u_initial, [u_result, []], [u_gen_c63, t]],
			[u_yfor, u_link, u_in, u_links],
			
			[ u_ydo,
			  
			  [ if,
			    u_plan_no_gen,
			    
			    [ progn,
			      [setq, u_gen_c63, [not, [car, u_plan_no_gen]]],
			      [setq, u_plan_no_gen, [cdr, u_plan_no_gen]]
			    ]
			  ],
			  
			  [ if,
			    u_gen_c63,
			    
			    [ setq,
			      u_result,
			      
			      [ u_append_c33,
				u_result,
				
				[ list,
				  [u_ob_c36_get, u_link, [quote, u_linked_to]]
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
:- set_opv(f_u_genable_subgoals, classof, claz_function),
   set_opv(u_genable_subgoals, compile_as, kw_function),
   set_opv(u_genable_subgoals, function, f_u_genable_subgoals),
   DefunResult=u_genable_subgoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:11951 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-subgoals',
			    [con, stream, switches, context, bp, 'top?'],
			    
			    [ let,
			      
			      [ [subgoals, ['genable-subgoals', con, context, bp]],
				[subj, []]
			      ],
			      
			      [ if,
				subgoals,
				
				[ progn,
				  
				  [ if,
				    ['null?', 'top?'],
				    ['gs-end-sentence', stream],
				    
				    [ progn,
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" I")
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" remember the time")
				      ]
				    ]
				  ],
				  
				  [ setq,
				    subj,
				    
				    [ gen,
				      ['ob$get', con, [quote, obj]],
				      stream,
				      [cons, [quote, [tense, past]], switches],
				      context,
				      bp
				    ]
				  ],
				  ['gs-string-write', stream, '$STRING'(" by")],
				  
				  [ yloop,
				    [initial, [rest, subgoals]],
				    [ywhile, rest],
				    
				    [ ydo,
				      
				      [ gen,
					['ob$get', [car, rest], [quote, obj]],
					stream,
					
					[ if,
					  subj,
					  
					  [ cons,
					    [list, [quote, 's-bar'], subj],
					    
					    [ cons,
					      [quote, [tense, gerund]],
					      switches
					    ]
					  ],
					  
					  [ cons,
					    [quote, [tense, gerund]],
					    switches
					  ]
					],
					context,
					bp
				      ],
				      [setq, rest, [cdr, rest]],
				      
				      [ if,
					rest,
					
					[ if,
					  ['null?', [cdr, rest]],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(", and by")
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(", by")
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ yloop,
				    [yfor, subgoal, in, subgoals],
				    
				    [ ydo,
				      
				      [ 'gen-subgoals',
					subgoal,
					stream,
					switches,
					context,
					bp,
					[]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GEN-SUBGOALS 
wl: lambda_def(defun,
	      u_gen_subgoals,
	      f_u_gen_subgoals,
	      [u_con, stream, u_switches, u_context, u_bp, u_top_c63],
	      
	      [ 
		[ let,
		  
		  [ [u_subgoals, [u_genable_subgoals, u_con, u_context, u_bp]],
		    [u_subj, []]
		  ],
		  
		  [ if,
		    u_subgoals,
		    
		    [ progn,
		      
		      [ if,
			[u_null_c63, u_top_c63],
			[u_gs_end_sentence, stream],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\('I')])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(r),
				       #\(e),
				       #\(m),
				       #\(e),
				       #\(m),
				       #\(b),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(' '),
				       #\(t),
				       #\(i),
				       #\(m),
				       #\(e)
				     ])
			  ]
			]
		      ],
		      
		      [ setq,
			u_subj,
			
			[ u_gen,
			  [u_ob_c36_get, u_con, [quote, u_obj]],
			  stream,
			  [cons, [quote, [u_tense, u_past]], u_switches],
			  u_context,
			  u_bp
			]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(b), #\(y)])
		      ],
		      
		      [ u_yloop,
			[u_initial, [rest, u_subgoals]],
			[u_ywhile, rest],
			
			[ u_ydo,
			  
			  [ u_gen,
			    [u_ob_c36_get, [car, rest], [quote, u_obj]],
			    stream,
			    
			    [ if,
			      u_subj,
			      
			      [ cons,
				[list, [quote, u_s_bar], u_subj],
				[cons, [quote, [u_tense, u_gerund]], u_switches]
			      ],
			      [cons, [quote, [u_tense, u_gerund]], u_switches]
			    ],
			    u_context,
			    u_bp
			  ],
			  [setq, rest, [cdr, rest]],
			  
			  [ if,
			    rest,
			    
			    [ if,
			      [u_null_c63, [cdr, rest]],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(','),
					   #\(' '),
					   #\(a),
					   #\(n),
					   #\(d),
					   #\(' '),
					   #\(b),
					   #\(y)
					 ])
			      ],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(','), #\(' '), #\(b), #\(y)])
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_subgoal, u_in, u_subgoals],
			
			[ u_ydo,
			  
			  [ u_gen_subgoals,
			    u_subgoal,
			    stream,
			    u_switches,
			    u_context,
			    u_bp,
			    []
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GEN-SUBGOALS 
wl: arglist_info(u_gen_subgoals,
		[u_con, stream, u_switches, u_context, u_bp, u_top_c63],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Top_c63_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_top_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_top_c63
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_top_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-SUBGOALS 
wl: init_args(exact_only, u_gen_subgoals).


% annotating U::GEN-SUBGOALS 
f_u_gen_subgoals(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Top_c63_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param), bv(u_top_c63, Top_c63_Param)],
	f_u_genable_subgoals(Con_Param, Context_Param, Bp_Param, Subgoals_Init),
	LEnv=[[bv(u_subgoals, Subgoals_Init), bv(u_subj, [])]|Env],
	get_var(LEnv, u_subgoals, IFTEST),
	(   IFTEST\==[]
	->  f_u_null_c63(u_top_c63, IFTEST32),
	    (   IFTEST32\==[]
	    ->  f_u_gs_end_sentence(Stream_Param, TrueResult),
		_67166=TrueResult
	    ;   f_u_gs_string_write(Stream_Param,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\('I')]),
				    String_write_Ret),
		f_u_gs_string_write(Stream_Param,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(' '),
					       #\(r),
					       #\(e),
					       #\(m),
					       #\(e),
					       #\(m),
					       #\(b),
					       #\(e),
					       #\(r),
					       #\(' '),
					       #\(t),
					       #\(h),
					       #\(e),
					       #\(' '),
					       #\(t),
					       #\(i),
					       #\(m),
					       #\(e)
					     ]),
				    ElseResult),
		_67166=ElseResult
	    ),
	    f_u_ob_c36_get(Con_Param, u_obj, Obj),
	    _67504=[[u_tense, u_past]|Switches_Param],
	    f_u_gen(Obj, Stream_Param, _67504, Context_Param, Bp_Param, Subj),
	    set_var(LEnv, u_subj, Subj),
	    f_u_gs_string_write(Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(b), #\(y)]),
				String_write_Ret51),
	    f_u_yloop(
		      [ [u_initial, [rest, u_subgoals]],
			[u_ywhile, rest],
			
			[ u_ydo,
			  
			  [ u_gen,
			    [u_ob_c36_get, [car, rest], [quote, u_obj]],
			    stream,
			    
			    [ if,
			      u_subj,
			      
			      [ cons,
				[list, [quote, u_s_bar], u_subj],
				[cons, [quote, [u_tense, u_gerund]], u_switches]
			      ],
			      [cons, [quote, [u_tense, u_gerund]], u_switches]
			    ],
			    u_context,
			    u_bp
			  ],
			  [setq, rest, [cdr, rest]],
			  
			  [ if,
			    rest,
			    
			    [ if,
			      [u_null_c63, [cdr, rest]],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(','),
					   #\(' '),
					   #\(a),
					   #\(n),
					   #\(d),
					   #\(' '),
					   #\(b),
					   #\(y)
					 ])
			      ],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(','), #\(' '), #\(b), #\(y)])
			      ]
			    ]
			  ]
			]
		      ],
		      Yloop_Ret),
	    f_u_yloop(
		      [ [u_yfor, u_subgoal, u_in, u_subgoals],
			
			[ u_ydo,
			  
			  [ u_gen_subgoals,
			    u_subgoal,
			    stream,
			    u_switches,
			    u_context,
			    u_bp,
			    []
			  ]
			]
		      ],
		      TrueResult45),
	    FnResult=TrueResult45
	;   FnResult=[]
	).
:- set_opv(f_u_gen_subgoals, classof, claz_function),
   set_opv(u_gen_subgoals, compile_as, kw_function),
   set_opv(u_gen_subgoals, function, f_u_gen_subgoals),
   DefunResult=u_gen_subgoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:13196 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'RPROX',
			    [],
			    
			    [ let,
			      
			      [ 
				[ subject,
				  
				  [ 'people-or-orgs',
				    ['ob$gets', con, [quote, actor]]
				  ]
				]
			      ],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, be],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      ['gs-string-write', stream, '$STRING'(" in")],
			      
			      [ gen,
				['ob$gets', con, [quote, location]],
				stream,
				switches,
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_rprox,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ 
			[ u_subject,
			  
			  [ u_people_or_orgs,
			    [u_ob_c36_gets, u_con, [quote, u_actor]]
			  ]
			]
		      ],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_be],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(i), #\(n)])
		      ],
		      
		      [ u_gen,
			[u_ob_c36_gets, u_con, [quote, u_location]],
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:13508 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    polities,
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [ywhile, lst],
			      
			      [ ydo,
				
				[ if,
				  ['ty$instance?', [car, lst], [quote, polity]],
				  [setq, result, [cons, [car, lst], result]]
				],
				[setq, lst, [cdr, lst]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::POLITIES 
wl: lambda_def(defun,
	      u_polities,
	      f_u_polities,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_ywhile, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_ty_c36_instance_c63, [car, u_lst], [quote, u_polity]],
		      [setq, u_result, [cons, [car, u_lst], u_result]]
		    ],
		    [setq, u_lst, [cdr, u_lst]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::POLITIES 
wl: arglist_info(u_polities,
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

% annotating U::POLITIES 
wl: init_args(exact_only, u_polities).


% annotating U::POLITIES 
f_u_polities(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ty_c36_instance_c63, [car, u_lst], [quote, u_polity]],
			[setq, u_result, [cons, [car, u_lst], u_result]]
		      ],
		      [setq, u_lst, [cdr, u_lst]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_polities, classof, claz_function),
   set_opv(u_polities, compile_as, kw_function),
   set_opv(u_polities, function, f_u_polities),
   DefunResult=u_polities.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:13750 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*neg-ob*',
			    ['ob$add-name', ['ob$create-empty'], [quote, neg]]
			  ]).
:- f_u_ob_c36_create_empty(Add_name_Param),
   f_u_ob_c36_add_name(Add_name_Param, u_neg, _Ignored),
   set_var(TLEnv3, u_xx_neg_ob_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:13804 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'neg?',
			    [con],
			    ['eq?', '*neg-ob*', ['ob$get', con, [quote, mode]]]
			  ]).

% annotating U::NEG? 
wl: lambda_def(defun,
	      u_neg_c63,
	      f_u_neg_c63,
	      [u_con],
	      [[u_eq_c63, u_xx_neg_ob_xx, [u_ob_c36_get, u_con, [quote, u_mode]]]]).


% annotating U::NEG? 
wl: arglist_info(u_neg_c63,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NEG? 
wl: init_args(exact_only, u_neg_c63).


% annotating U::NEG? 
f_u_neg_c63(Con_Param, FnResult) :-
	Env=[bv(u_con, Con_Param)],
	f_u_eq_c63(u_xx_neg_ob_xx,
		   [u_ob_c36_get, u_con, [quote, u_mode]],
		   Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_neg_c63, classof, claz_function),
   set_opv(u_neg_c63, compile_as, kw_function),
   set_opv(u_neg_c63, function, f_u_neg_c63),
   DefunResult=u_neg_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:13860 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-negative',
			    [con],
			    
			    [ let,
			      [[newcon, ['ob$copy', con]]],
			      ['ob$add', newcon, [quote, mode], '*neg-ob*'],
			      newcon
			    ]
			  ]).

% annotating U::MAKE-NEGATIVE 
wl: lambda_def(defun,
	      u_make_negative,
	      f_u_make_negative,
	      [u_con],
	      
	      [ 
		[ let,
		  [[u_newcon, [u_ob_c36_copy, u_con]]],
		  [u_ob_c36_add, u_newcon, [quote, u_mode], u_xx_neg_ob_xx],
		  u_newcon
		]
	      ]).


% annotating U::MAKE-NEGATIVE 
wl: arglist_info(u_make_negative,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-NEGATIVE 
wl: init_args(exact_only, u_make_negative).


% annotating U::MAKE-NEGATIVE 
f_u_make_negative(Con_Param, FnResult) :-
	Env=[bv(u_con, Con_Param)],
	f_u_ob_c36_copy(Con_Param, Newcon_Init),
	LEnv=[[bv(u_newcon, Newcon_Init)]|Env],
	get_var(LEnv, u_newcon, Newcon_Get),
	get_var(LEnv, u_xx_neg_ob_xx, Xx_neg_ob_xx_Get),
	f_u_ob_c36_add(Newcon_Get, u_mode, Xx_neg_ob_xx_Get, C36_add_Ret),
	get_var(LEnv, u_newcon, Newcon_Get19),
	LetResult=Newcon_Get19,
	LetResult=FnResult.
:- set_opv(f_u_make_negative, classof, claz_function),
   set_opv(u_make_negative, compile_as, kw_function),
   set_opv(u_make_negative, function, f_u_make_negative),
   DefunResult=u_make_negative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:13968 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'NOT',
			    [con, stream, switches],
			    
			    [ let,
			      
			      [ 
				[ newcon,
				  ['make-negative', ['ob$get', con, [quote, obj]]]
				],
				[result, []]
			      ],
			      
			      [ setq,
				result,
				[gen, newcon, stream, switches, context, bp]
			      ],
			      result
			    ]
			  ]).
:- f_u_define_gen(not,
		  [u_con, stream, u_switches],
		  
		  [ 
		    [ let,
		      
		      [ 
			[ u_newcon,
			  [u_make_negative, [u_ob_c36_get, u_con, [quote, u_obj]]]
			],
			[u_result, []]
		      ],
		      
		      [ setq,
			u_result,
			[u_gen, u_newcon, stream, u_switches, u_context, u_bp]
		      ],
		      u_result
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:14157 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'lovers-goal?',
			    [goal],
			    
			    [ 'ty$instance?',
			      ['ob$get', goal, [quote, obj]],
			      [quote, 'LOVERS']
			    ]
			  ]).

% annotating U::LOVERS-GOAL? 
wl: lambda_def(defun,
	      u_lovers_goal_c63,
	      f_u_lovers_goal_c63,
	      [u_goal],
	      
	      [ 
		[ u_ty_c36_instance_c63,
		  [u_ob_c36_get, u_goal, [quote, u_obj]],
		  [quote, u_lovers]
		]
	      ]).


% annotating U::LOVERS-GOAL? 
wl: arglist_info(u_lovers_goal_c63,
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

% annotating U::LOVERS-GOAL? 
wl: init_args(exact_only, u_lovers_goal_c63).


% annotating U::LOVERS-GOAL? 
f_u_lovers_goal_c63(Goal_Param, FnResult) :-
	f_u_ob_c36_get(Goal_Param, u_obj, Obj),
	f_u_ty_c36_instance_c63(Obj, u_lovers, Lovers),
	Lovers=FnResult.
:- set_opv(f_u_lovers_goal_c63, classof, claz_function),
   set_opv(u_lovers_goal_c63, compile_as, kw_function),
   set_opv(u_lovers_goal_c63, function, f_u_lovers_goal_c63),
   DefunResult=u_lovers_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:14230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'friends-goal?',
			    [goal],
			    
			    [ 'ty$instance?',
			      ['ob$get', goal, [quote, obj]],
			      [quote, 'FRIENDS']
			    ]
			  ]).

% annotating U::FRIENDS-GOAL? 
wl: lambda_def(defun,
	      u_friends_goal_c63,
	      f_u_friends_goal_c63,
	      [u_goal],
	      
	      [ 
		[ u_ty_c36_instance_c63,
		  [u_ob_c36_get, u_goal, [quote, u_obj]],
		  [quote, u_friends]
		]
	      ]).


% annotating U::FRIENDS-GOAL? 
wl: arglist_info(u_friends_goal_c63,
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

% annotating U::FRIENDS-GOAL? 
wl: init_args(exact_only, u_friends_goal_c63).


% annotating U::FRIENDS-GOAL? 
f_u_friends_goal_c63(Goal_Param, FnResult) :-
	f_u_ob_c36_get(Goal_Param, u_obj, Obj),
	f_u_ty_c36_instance_c63(Obj, u_friends, Friends),
	Friends=FnResult.
:- set_opv(f_u_friends_goal_c63, classof, claz_function),
   set_opv(u_friends_goal_c63, compile_as, kw_function),
   set_opv(u_friends_goal_c63, function, f_u_friends_goal_c63),
   DefunResult=u_friends_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:14305 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-car',
			    [x],
			    
			    [ if,
			      ['pair?', x],
			      
			      [ if,
				['ty$instance?', [car, x], [quote, goal]],
				[car, x],
				[]
			      ],
			      []
			    ]
			  ]).

% annotating U::GOAL-CAR 
wl: lambda_def(defun,
	      u_goal_car,
	      f_u_goal_car,
	      [u_x],
	      
	      [ 
		[ if,
		  [u_pair_c63, u_x],
		  
		  [ if,
		    [u_ty_c36_instance_c63, [car, u_x], [quote, u_goal]],
		    [car, u_x],
		    []
		  ],
		  []
		]
	      ]).


% annotating U::GOAL-CAR 
wl: arglist_info(u_goal_car,
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

% annotating U::GOAL-CAR 
wl: init_args(exact_only, u_goal_car).


% annotating U::GOAL-CAR 
f_u_goal_car(X_Param, TrueResult19) :-
	Env=[bv(u_x, X_Param)],
	f_u_pair_c63(u_x, IFTEST),
	(   IFTEST\==[]
	->  cl_car(X_Param, Instance_c63_Param),
	    f_u_ty_c36_instance_c63(Instance_c63_Param, u_goal, IFTEST14),
	    (   IFTEST14\==[]
	    ->  cl_car(X_Param, TrueResult),
		TrueResult19=TrueResult
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_goal_car, classof, claz_function),
   set_opv(u_goal_car, compile_as, kw_function),
   set_opv(u_goal_car, function, f_u_goal_car),
   DefunResult=u_goal_car.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:14426 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-gratitude-anger',
			    
			    [ con,
			      stream,
			      switches,
			      context,
			      bp,
			      subject,
			      scale,
			      toward
			    ],
			    
			    [ if,
			      subject,
			      
			      [ progn,
				
				[ 'gen-subject',
				  subject,
				  stream,
				  switches,
				  context,
				  bp
				],
				
				[ 'gen-verb',
				  [quote, feel],
				  subject,
				  stream,
				  switches,
				  ['neg?', con]
				]
			      ]
			    ],
			    ['gen-scale', scale, stream],
			    
			    [ if,
			      ['ty$instance?', con, [quote, 'NEG-EMOTION']],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" angry at")
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" grateful to")
			      ]
			    ],
			    [gen, toward, stream, switches, context, bp]
			  ]).

% annotating U::GEN-GRATITUDE-ANGER 
wl: lambda_def(defun,
	      u_gen_gratitude_anger,
	      f_u_gen_gratitude_anger,
	      
	      [ u_con,
		stream,
		u_switches,
		u_context,
		u_bp,
		u_subject,
		u_scale,
		u_toward
	      ],
	      
	      [ 
		[ if,
		  u_subject,
		  
		  [ progn,
		    [u_gen_subject, u_subject, stream, u_switches, u_context, u_bp],
		    
		    [ u_gen_verb,
		      [quote, u_feel],
		      u_subject,
		      stream,
		      u_switches,
		      [u_neg_c63, u_con]
		    ]
		  ]
		],
		[u_gen_scale, u_scale, stream],
		
		[ if,
		  [u_ty_c36_instance_c63, u_con, [quote, u_neg_emotion]],
		  
		  [ u_gs_string_write,
		    stream,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(' '),
			       #\(a),
			       #\(n),
			       #\(g),
			       #\(r),
			       #\(y),
			       #\(' '),
			       #\(a),
			       #\(t)
			     ])
		  ],
		  
		  [ u_gs_string_write,
		    stream,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\(' '),
			       #\(g),
			       #\(r),
			       #\(a),
			       #\(t),
			       #\(e),
			       #\(f),
			       #\(u),
			       #\(l),
			       #\(' '),
			       #\(t),
			       #\(o)
			     ])
		  ]
		],
		[u_gen, u_toward, stream, u_switches, u_context, u_bp]
	      ]).


% annotating U::GEN-GRATITUDE-ANGER 
wl: arglist_info(u_gen_gratitude_anger,
		
		[ u_con,
		  stream,
		  u_switches,
		  u_context,
		  u_bp,
		  u_subject,
		  u_scale,
		  u_toward
		],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Subject_Param,
		  Scale_Param,
		  Toward_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_scale,
			       u_toward
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_subject,
				 u_scale,
				 u_toward
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_scale,
			       u_toward
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-GRATITUDE-ANGER 
wl: init_args(exact_only, u_gen_gratitude_anger).


% annotating U::GEN-GRATITUDE-ANGER 
f_u_gen_gratitude_anger(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Subject_Param, Scale_Param, Toward_Param, FnResult) :-
	(   Subject_Param\==[]
	->  f_u_gen_subject(Subject_Param,
			    Stream_Param,
			    Switches_Param,
			    Context_Param,
			    Bp_Param,
			    Gen_subject_Ret),
	    f_u_neg_c63(Con_Param, Neg_c63_Ret),
	    f_u_gen_verb(u_feel,
			 Subject_Param,
			 Stream_Param,
			 Switches_Param,
			 Neg_c63_Ret,
			 TrueResult),
	    _65818=TrueResult
	;   _65818=[]
	),
	f_u_gen_scale(Scale_Param, Stream_Param, Gen_scale_Ret),
	f_u_ty_c36_instance_c63(Con_Param, u_neg_emotion, IFTEST41),
	(   IFTEST41\==[]
	->  f_u_gs_string_write(Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(a),
					   #\(n),
					   #\(g),
					   #\(r),
					   #\(y),
					   #\(' '),
					   #\(a),
					   #\(t)
					 ]),
				TrueResult46),
	    _66188=TrueResult46
	;   f_u_gs_string_write(Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(g),
					   #\(r),
					   #\(a),
					   #\(t),
					   #\(e),
					   #\(f),
					   #\(u),
					   #\(l),
					   #\(' '),
					   #\(t),
					   #\(o)
					 ]),
				ElseResult),
	    _66188=ElseResult
	),
	f_u_gen(Toward_Param,
		Stream_Param,
		Switches_Param,
		Context_Param,
		Bp_Param,
		Gen_Ret),
	Gen_Ret=FnResult.
:- set_opv(f_u_gen_gratitude_anger, classof, claz_function),
   set_opv(u_gen_gratitude_anger, compile_as, kw_function),
   set_opv(u_gen_gratitude_anger, function, f_u_gen_gratitude_anger),
   DefunResult=u_gen_gratitude_anger.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:14879 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-pos-neg-emot-string',
			    
			    [ con,
			      stream,
			      switches,
			      context,
			      bp,
			      subject,
			      scale,
			      'pos-string',
			      'neg-string'
			    ],
			    
			    [ if,
			      subject,
			      
			      [ progn,
				
				[ 'gen-subject',
				  subject,
				  stream,
				  switches,
				  context,
				  bp
				],
				
				[ 'gen-verb',
				  [quote, feel],
				  subject,
				  stream,
				  switches,
				  ['neg?', con]
				]
			      ]
			    ],
			    ['gen-scale', scale, stream],
			    
			    [ if,
			      ['ty$instance?', con, [quote, 'NEG-EMOTION']],
			      ['gs-string-write', stream, 'neg-string'],
			      ['gs-string-write', stream, 'pos-string']
			    ]
			  ]).

% annotating U::GEN-POS-NEG-EMOT-STRING 
wl: lambda_def(defun,
	      u_gen_pos_neg_emot_string,
	      f_u_gen_pos_neg_emot_string,
	      
	      [ u_con,
		stream,
		u_switches,
		u_context,
		u_bp,
		u_subject,
		u_scale,
		u_pos_string,
		u_neg_string
	      ],
	      
	      [ 
		[ if,
		  u_subject,
		  
		  [ progn,
		    [u_gen_subject, u_subject, stream, u_switches, u_context, u_bp],
		    
		    [ u_gen_verb,
		      [quote, u_feel],
		      u_subject,
		      stream,
		      u_switches,
		      [u_neg_c63, u_con]
		    ]
		  ]
		],
		[u_gen_scale, u_scale, stream],
		
		[ if,
		  [u_ty_c36_instance_c63, u_con, [quote, u_neg_emotion]],
		  [u_gs_string_write, stream, u_neg_string],
		  [u_gs_string_write, stream, u_pos_string]
		]
	      ]).


% annotating U::GEN-POS-NEG-EMOT-STRING 
wl: arglist_info(u_gen_pos_neg_emot_string,
		
		[ u_con,
		  stream,
		  u_switches,
		  u_context,
		  u_bp,
		  u_subject,
		  u_scale,
		  u_pos_string,
		  u_neg_string
		],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Subject_Param,
		  Scale_Param,
		  Pos_string_Param,
		  Neg_string_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_scale,
			       u_pos_string,
			       u_neg_string
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_subject,
				 u_scale,
				 u_pos_string,
				 u_neg_string
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_scale,
			       u_pos_string,
			       u_neg_string
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-POS-NEG-EMOT-STRING 
wl: init_args(exact_only, u_gen_pos_neg_emot_string).


% annotating U::GEN-POS-NEG-EMOT-STRING 
f_u_gen_pos_neg_emot_string(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Subject_Param, Scale_Param, Pos_string_Param, Neg_string_Param, FnResult) :-
	(   Subject_Param\==[]
	->  f_u_gen_subject(Subject_Param,
			    Stream_Param,
			    Switches_Param,
			    Context_Param,
			    Bp_Param,
			    Gen_subject_Ret),
	    f_u_neg_c63(Con_Param, Neg_c63_Ret),
	    f_u_gen_verb(u_feel,
			 Subject_Param,
			 Stream_Param,
			 Switches_Param,
			 Neg_c63_Ret,
			 TrueResult),
	    _65670=TrueResult
	;   _65670=[]
	),
	f_u_gen_scale(Scale_Param, Stream_Param, Gen_scale_Ret),
	f_u_ty_c36_instance_c63(Con_Param, u_neg_emotion, IFTEST43),
	(   IFTEST43\==[]
	->  f_u_gs_string_write(Stream_Param, Neg_string_Param, TrueResult50),
	    FnResult=TrueResult50
	;   f_u_gs_string_write(Stream_Param, Pos_string_Param, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_gen_pos_neg_emot_string, classof, claz_function),
   set_opv(u_gen_pos_neg_emot_string, compile_as, kw_function),
   set_opv(u_gen_pos_neg_emot_string, function, f_u_gen_pos_neg_emot_string),
   DefunResult=u_gen_pos_neg_emot_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:15308 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-emot-string',
			    
			    [ con,
			      stream,
			      switches,
			      context,
			      bp,
			      subject,
			      scale,
			      'emot-string'
			    ],
			    
			    [ if,
			      subject,
			      
			      [ progn,
				
				[ 'gen-subject',
				  subject,
				  stream,
				  switches,
				  context,
				  bp
				],
				
				[ 'gen-verb',
				  [quote, feel],
				  subject,
				  stream,
				  switches,
				  ['neg?', con]
				]
			      ]
			    ],
			    ['gen-scale', scale, stream],
			    ['gs-string-write', stream, 'emot-string']
			  ]).

% annotating U::GEN-EMOT-STRING 
wl: lambda_def(defun,
	      u_gen_emot_string,
	      f_u_gen_emot_string,
	      
	      [ u_con,
		stream,
		u_switches,
		u_context,
		u_bp,
		u_subject,
		u_scale,
		u_emot_string
	      ],
	      
	      [ 
		[ if,
		  u_subject,
		  
		  [ progn,
		    [u_gen_subject, u_subject, stream, u_switches, u_context, u_bp],
		    
		    [ u_gen_verb,
		      [quote, u_feel],
		      u_subject,
		      stream,
		      u_switches,
		      [u_neg_c63, u_con]
		    ]
		  ]
		],
		[u_gen_scale, u_scale, stream],
		[u_gs_string_write, stream, u_emot_string]
	      ]).


% annotating U::GEN-EMOT-STRING 
wl: arglist_info(u_gen_emot_string,
		
		[ u_con,
		  stream,
		  u_switches,
		  u_context,
		  u_bp,
		  u_subject,
		  u_scale,
		  u_emot_string
		],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Subject_Param,
		  Scale_Param,
		  Emot_string_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_scale,
			       u_emot_string
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_subject,
				 u_scale,
				 u_emot_string
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_scale,
			       u_emot_string
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-EMOT-STRING 
wl: init_args(exact_only, u_gen_emot_string).


% annotating U::GEN-EMOT-STRING 
f_u_gen_emot_string(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Subject_Param, Scale_Param, Emot_string_Param, FnResult) :-
	(   Subject_Param\==[]
	->  f_u_gen_subject(Subject_Param,
			    Stream_Param,
			    Switches_Param,
			    Context_Param,
			    Bp_Param,
			    Gen_subject_Ret),
	    f_u_neg_c63(Con_Param, Neg_c63_Ret),
	    f_u_gen_verb(u_feel,
			 Subject_Param,
			 Stream_Param,
			 Switches_Param,
			 Neg_c63_Ret,
			 TrueResult),
	    _65396=TrueResult
	;   _65396=[]
	),
	f_u_gen_scale(Scale_Param, Stream_Param, Gen_scale_Ret),
	f_u_gs_string_write(Stream_Param, Emot_string_Param, String_write_Ret),
	String_write_Ret=FnResult.
:- set_opv(f_u_gen_emot_string, classof, claz_function),
   set_opv(u_gen_emot_string, compile_as, kw_function),
   set_opv(u_gen_emot_string, function, f_u_gen_emot_string),
   DefunResult=u_gen_emot_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:15628 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*overall-emotional-state*', 0.0]).
:- set_var(TLEnv3, setq, u_xx_overall_emotional_state_xx, 0.0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:15665 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*as-well-as?*', []]).
:- set_var(TLEnv3, setq, u_xx_as_well_as_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:15692 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'EMOTION',
			    [],
			    
			    [ let,
			      
			      [ 
				[ subject,
				  
				  [ 'generate-emotion',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    t,
				    t
				  ]
				],
				['neg-total', 0.0],
				['pos-total', 0.0]
			      ],
			      
			      [ yloop,
				[yfor, emot, in, '*emotions*'],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'ty$instance?',
				      emot,
				      [quote, 'neg-emotion']
				    ],
				    
				    [ setq,
				      'neg-total',
				      ['fl+', 'neg-total', [strength, emot]]
				    ],
				    
				    [ setq,
				      'pos-total',
				      ['fl+', 'pos-total', [strength, emot]]
				    ]
				  ]
				]
			      ],
			      
			      [ setq,
				'*overall-emotional-state*',
				['fl-', 'pos-total', 'neg-total']
			      ],
			      
			      [ if,
				'*as-well-as?*',
				
				[ yloop,
				  
				  [ initial,
				    [emot, []],
				    ['first?', t],
				    
				    [ elems,
				      
				      [ prune,
					'*emotions*',
					
					[ lambda,
					  [x],
					  
					  [ and,
					    ['neq?', x, con],
					    
					    [ 'fl>',
					      [strength, x],
					      '*genable-emot-thresh*'
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  [ywhile, elems],
				  
				  [ ydo,
				    [setq, emot, [car, elems]],
				    [setq, elems, [cdr, elems]],
				    
				    [ if,
				      'first?',
				      
				      [ progn,
					[setq, 'first?', []],
					
					[ 'gs-string-write',
					  stream,
					  '$STRING'(" as well as")
					]
				      ]
				    ],
				    
				    [ 'generate-emotion',
				      emot,
				      stream,
				      switches,
				      '*reality*',
				      bp,
				      [],
				      t
				    ],
				    
				    [ if,
				      elems,
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(",")
				      ]
				    ],
				    
				    [ if,
				      [and, elems, ['null?', [cdr, elems]]],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" and")
				      ]
				    ]
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_emotion,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ 
			[ u_subject,
			  
			  [ u_generate_emotion,
			    u_con,
			    stream,
			    u_switches,
			    u_context,
			    u_bp,
			    t,
			    t
			  ]
			],
			[u_neg_total, 0.0],
			[u_pos_total, 0.0]
		      ],
		      
		      [ u_yloop,
			[u_yfor, u_emot, u_in, u_xx_emotions_xx],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ u_ty_c36_instance_c63,
			      u_emot,
			      [quote, u_neg_emotion]
			    ],
			    
			    [ setq,
			      u_neg_total,
			      [u_fl_c43, u_neg_total, [u_strength, u_emot]]
			    ],
			    
			    [ setq,
			      u_pos_total,
			      [u_fl_c43, u_pos_total, [u_strength, u_emot]]
			    ]
			  ]
			]
		      ],
		      
		      [ setq,
			u_xx_overall_emotional_state_xx,
			[u_flc45, u_pos_total, u_neg_total]
		      ],
		      
		      [ if,
			u_xx_as_well_as_c63_xx,
			
			[ u_yloop,
			  
			  [ u_initial,
			    [u_emot, []],
			    [u_first_c63, t],
			    
			    [ u_elems,
			      
			      [ u_prune,
				u_xx_emotions_xx,
				
				[ lambda,
				  [u_x],
				  
				  [ and,
				    [u_neq_c63, u_x, u_con],
				    
				    [ u_fl_c62,
				      [u_strength, u_x],
				      u_xx_genable_emot_thresh_xx
				    ]
				  ]
				]
			      ]
			    ]
			  ],
			  [u_ywhile, u_elems],
			  
			  [ u_ydo,
			    [setq, u_emot, [car, u_elems]],
			    [setq, u_elems, [cdr, u_elems]],
			    
			    [ if,
			      u_first_c63,
			      
			      [ progn,
				[setq, u_first_c63, []],
				
				[ u_gs_string_write,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(' '),
					     #\(a),
					     #\(s),
					     #\(' '),
					     #\(w),
					     #\(e),
					     #\(l),
					     #\(l),
					     #\(' '),
					     #\(a),
					     #\(s)
					   ])
				]
			      ]
			    ],
			    
			    [ u_generate_emotion,
			      u_emot,
			      stream,
			      u_switches,
			      u_xx_reality_xx,
			      u_bp,
			      [],
			      t
			    ],
			    
			    [ if,
			      u_elems,
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*], claz_base_character, [#\(',')])
			      ]
			    ],
			    
			    [ if,
			      [and, u_elems, [u_null_c63, [cdr, u_elems]]],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(a), #\(n), #\(d)])
			      ]
			    ]
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:16961 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*overall-con*',
			    ['ob$fcreate', [quote, ['OVERALL-EMOTION']]]
			  ]).
:- f_u_ob_c36_fcreate([quote, [u_overall_emotion]], _Ignored),
   set_var(TLEnv3, u_xx_overall_con_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:17015 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-overall-emot-state',
			    [],
			    
			    [ 'ob$set',
			      '*overall-con*',
			      [quote, scale],
			      '*overall-emotional-state*'
			    ],
			    
			    [ generate1,
			      '*overall-con*',
			      '*global-switches*',
			      '*reality*',
			      '*me-belief-path*'
			    ]
			  ]).

% annotating U::GEN-OVERALL-EMOT-STATE 
wl: lambda_def(defun,
	      u_gen_overall_emot_state,
	      f_u_gen_overall_emot_state,
	      [],
	      
	      [ 
		[ u_ob_c36_set,
		  u_xx_overall_con_xx,
		  [quote, u_scale],
		  u_xx_overall_emotional_state_xx
		],
		
		[ u_generate1,
		  u_xx_overall_con_xx,
		  u_xx_global_switches_xx,
		  u_xx_reality_xx,
		  u_xx_me_belief_path_xx
		]
	      ]).


% annotating U::GEN-OVERALL-EMOT-STATE 
wl: arglist_info(u_gen_overall_emot_state,
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

% annotating U::GEN-OVERALL-EMOT-STATE 
wl: init_args(exact_only, u_gen_overall_emot_state).


% annotating U::GEN-OVERALL-EMOT-STATE 
f_u_gen_overall_emot_state(FnResult) :-
	Env=[],
	get_var(Env, u_xx_overall_con_xx, Xx_overall_con_xx_Get),
	get_var(Env,
		u_xx_overall_emotional_state_xx,
		Xx_overall_emotional_state_xx_Get),
	f_u_ob_c36_set(Xx_overall_con_xx_Get,
		       u_scale,
		       Xx_overall_emotional_state_xx_Get,
		       C36_set_Ret),
	get_var(Env, u_xx_global_switches_xx, Xx_global_switches_xx_Get),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	get_var(Env, u_xx_overall_con_xx, Xx_overall_con_xx_Get12),
	get_var(Env, u_xx_reality_xx, Xx_reality_xx_Get),
	f_u_generate1(Xx_overall_con_xx_Get12,
		      Xx_global_switches_xx_Get,
		      Xx_reality_xx_Get,
		      Xx_me_belief_path_xx_Get,
		      Generate1_Ret),
	Generate1_Ret=FnResult.
:- set_opv(f_u_gen_overall_emot_state, classof, claz_function),
   set_opv(u_gen_overall_emot_state, compile_as, kw_function),
   set_opv(u_gen_overall_emot_state, function, f_u_gen_overall_emot_state),
   DefunResult=u_gen_overall_emot_state.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:17181 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'OVERALL-EMOTION',
			    [],
			    ['gs-string-write', stream, '$STRING'(" overall,")],
			    
			    [ let,
			      [[scale, ['ob$get', con, [quote, scale]]]],
			      ['gs-string-write', stream, '$STRING'(" I feel")],
			      
			      [ cond,
				
				[ ['fl>', scale, 2.0],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" ecstatic")
				  ]
				],
				
				[ ['fl>', scale, 1.0],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" happy")
				  ]
				],
				
				[ ['fl>', scale, 0.25],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" fine")
				  ]
				],
				
				[ ['fl>', scale, -1.0],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" ill at ease")
				  ]
				],
				
				[ ['fl>', scale, -2.0],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" upset")
				  ]
				],
				
				[ else,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" miserable")
				  ]
				]
			      ],
			      '*me-ob*'
			    ]
			  ]).
:- f_u_define_gen(u_overall_emotion,
		  [],
		  
		  [ 
		    [ u_gs_string_write,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(o),
				 #\(v),
				 #\(e),
				 #\(r),
				 #\(a),
				 #\(l),
				 #\(l),
				 #\(',')
			       ])
		    ],
		    
		    [ let,
		      [[u_scale, [u_ob_c36_get, u_con, [quote, u_scale]]]],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\('I'),
				   #\(' '),
				   #\(f),
				   #\(e),
				   #\(e),
				   #\(l)
				 ])
		      ],
		      
		      [ cond,
			
			[ [u_fl_c62, u_scale, 2.0],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(e),
				       #\(c),
				       #\(s),
				       #\(t),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(c)
				     ])
			  ]
			],
			
			[ [u_fl_c62, u_scale, 1.0],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(h), #\(a), #\(p), #\(p), #\(y)])
			  ]
			],
			
			[ [u_fl_c62, u_scale, 0.25],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(f), #\(i), #\(n), #\(e)])
			  ]
			],
			
			[ [u_fl_c62, u_scale, -1.0],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(i),
				       #\(l),
				       #\(l),
				       #\(' '),
				       #\(a),
				       #\(t),
				       #\(' '),
				       #\(e),
				       #\(a),
				       #\(s),
				       #\(e)
				     ])
			  ]
			],
			
			[ [u_fl_c62, u_scale, -2.0],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(u), #\(p), #\(s), #\(e), #\(t)])
			  ]
			],
			
			[ u_else,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(m),
				       #\(i),
				       #\(s),
				       #\(e),
				       #\(r),
				       #\(a),
				       #\(b),
				       #\(l),
				       #\(e)
				     ])
			  ]
			]
		      ],
		      u_xx_me_ob_xx
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'generate-emotion',
			    
			    [ con,
			      stream,
			      switches,
			      context,
			      bp,
			      'with-subject?',
			      'verbose?'
			    ],
			    
			    [ let,
			      
			      [ [scale, ['ob$get', con, [quote, strength]]],
				[subject, [if, 'with-subject?', [car, bp], []]],
				[toward, ['ob$get', con, [quote, to]]],
				
				[ 'to-goal',
				  
				  [ 'goal-car',
				    
				    [ 'ol-get',
				      con,
				      '*dependency-ob*',
				      [quote, forward],
				      context
				    ]
				  ]
				],
				
				[ 'from-goal',
				  
				  [ 'goal-car',
				    
				    [ 'ol-get',
				      con,
				      '*dependency-ob*',
				      [quote, backward],
				      context
				    ]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ ['ty$instance?', con, [quote, surprise]],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" What do you know")
				  ]
				],
				
				[ ['ob$get', con, [quote, 'altern?']],
				  
				  [ 'gen-pos-neg-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" relieved"),
				    '$STRING'(" regretful")
				  ],
				  
				  [ 'gen-emot-verbosity',
				    'from-goal',
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    '$STRING'(" about"),
				    'verbose?'
				  ]
				],
				
				[ [and, 'to-goal', ['null?', 'from-goal']],
				  
				  [ 'gen-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" interested")
				  ],
				  
				  [ 'gen-emot-verbosity',
				    'to-goal',
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    '$STRING'(" in"),
				    'verbose?'
				  ]
				],
				
				[ 
				  [ and,
				    'to-goal',
				    'from-goal',
				    ['imagined-outcome?', 'from-goal']
				  ],
				  
				  [ 'gen-pos-neg-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" hopeful"),
				    '$STRING'(" worried")
				  ],
				  
				  [ 'gen-emot-verbosity',
				    'from-goal',
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    '$STRING'(" about"),
				    'verbose?'
				  ]
				],
				
				[ 
				  [ and,
				    toward,
				    'from-goal',
				    ['social-esteem-goal?', 'from-goal']
				  ],
				  
				  [ 'gen-pos-neg-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" proud"),
				    '$STRING'(" humiliated")
				  ]
				],
				
				[ toward,
				  
				  [ 'gen-gratitude-anger',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    toward
				  ]
				],
				
				[ 
				  [ and,
				    'from-goal',
				    ['social-esteem-goal?', 'from-goal']
				  ],
				  
				  [ 'gen-pos-neg-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" poised"),
				    '$STRING'(" embarrassed")
				  ]
				],
				
				[ 
				  [ and,
				    'from-goal',
				    ['self-esteem-goal?', 'from-goal']
				  ],
				  
				  [ 'gen-pos-neg-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" proud"),
				    '$STRING'(" ashamed")
				  ]
				],
				
				[ 
				  [ and,
				    'from-goal',
				    ['ty$instance?', con, [quote, 'neg-emotion']],
				    ['lovers-goal?', 'from-goal'],
				    
				    [ 'existing-relationship1?',
				      ['ob$get', 'from-goal', [quote, obj]],
				      
				      [ or,
					
					[ 'ob$get',
					  'from-goal',
					  [quote, 'activation-context']
					],
					['cx$parent', context]
				      ]
				    ]
				  ],
				  
				  [ 'gen-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" broken hearted")
				  ]
				],
				
				[ 
				  [ and,
				    'from-goal',
				    ['ty$instance?', con, [quote, 'neg-emotion']],
				    
				    [ or,
				      ['lovers-goal?', 'from-goal'],
				      ['friends-goal?', 'from-goal']
				    ]
				  ],
				  
				  [ 'gen-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" rejected")
				  ]
				],
				
				[ 
				  [ and,
				    'from-goal',
				    
				    [ 'ty$instance?',
				      ['ob$get', 'from-goal', [quote, obj]],
				      [quote, entertainment]
				    ],
				    ['ty$instance?', con, [quote, 'pos-emotion']]
				  ],
				  
				  [ 'gen-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" amused")
				  ]
				],
				
				[ 
				  [ and,
				    'from-goal',
				    
				    [ 'ty$instance?',
				      ['ob$get', 'from-goal', [quote, obj]],
				      [quote, food]
				    ]
				  ],
				  
				  [ 'gen-pos-neg-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" satiated"),
				    '$STRING'(" starved")
				  ]
				],
				
				[ else,
				  
				  [ 'gen-pos-neg-emot-string',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    scale,
				    '$STRING'(" pleased"),
				    '$STRING'(" displeased")
				  ],
				  
				  [ 'gen-emot-verbosity',
				    'from-goal',
				    stream,
				    switches,
				    context,
				    bp,
				    subject,
				    '$STRING'(" about"),
				    'verbose?'
				  ]
				]
			      ],
			      subject
			    ]
			  ]).

% annotating U::GENERATE-EMOTION 
wl: lambda_def(defun,
	      u_generate_emotion,
	      f_u_generate_emotion,
	      
	      [ u_con,
		stream,
		u_switches,
		u_context,
		u_bp,
		u_with_subject_c63,
		u_verbose_c63
	      ],
	      
	      [ 
		[ let,
		  
		  [ [u_scale, [u_ob_c36_get, u_con, [quote, u_strength]]],
		    [u_subject, [if, u_with_subject_c63, [car, u_bp], []]],
		    [u_toward, [u_ob_c36_get, u_con, [quote, u_to]]],
		    
		    [ u_to_goal,
		      
		      [ u_goal_car,
			
			[ u_ol_get,
			  u_con,
			  u_xx_dependency_ob_xx,
			  [quote, u_forward],
			  u_context
			]
		      ]
		    ],
		    
		    [ u_from_goal,
		      
		      [ u_goal_car,
			
			[ u_ol_get,
			  u_con,
			  u_xx_dependency_ob_xx,
			  [quote, u_backward],
			  u_context
			]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [u_ty_c36_instance_c63, u_con, [quote, u_surprise]],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\('W'),
				   #\(h),
				   #\(a),
				   #\(t),
				   #\(' '),
				   #\(d),
				   #\(o),
				   #\(' '),
				   #\(y),
				   #\(o),
				   #\(u),
				   #\(' '),
				   #\(k),
				   #\(n),
				   #\(o),
				   #\(w)
				 ])
		      ]
		    ],
		    
		    [ [u_ob_c36_get, u_con, [quote, u_altern_c63]],
		      
		      [ u_gen_pos_neg_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(r),
				   #\(e),
				   #\(l),
				   #\(i),
				   #\(e),
				   #\(v),
				   #\(e),
				   #\(d)
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(r),
				   #\(e),
				   #\(g),
				   #\(r),
				   #\(e),
				   #\(t),
				   #\(f),
				   #\(u),
				   #\(l)
				 ])
		      ],
		      
		      [ u_gen_emot_verbosity,
			u_from_goal,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(a), #\(b), #\(o), #\(u), #\(t)]),
			u_verbose_c63
		      ]
		    ],
		    
		    [ [and, u_to_goal, [u_null_c63, u_from_goal]],
		      
		      [ u_gen_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(i),
				   #\(n),
				   #\(t),
				   #\(e),
				   #\(r),
				   #\(e),
				   #\(s),
				   #\(t),
				   #\(e),
				   #\(d)
				 ])
		      ],
		      
		      [ u_gen_emot_verbosity,
			u_to_goal,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(i), #\(n)]),
			u_verbose_c63
		      ]
		    ],
		    
		    [ 
		      [ and,
			u_to_goal,
			u_from_goal,
			[u_imagined_outcome_c63, u_from_goal]
		      ],
		      
		      [ u_gen_pos_neg_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(h),
				   #\(o),
				   #\(p),
				   #\(e),
				   #\(f),
				   #\(u),
				   #\(l)
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(w),
				   #\(o),
				   #\(r),
				   #\(r),
				   #\(i),
				   #\(e),
				   #\(d)
				 ])
		      ],
		      
		      [ u_gen_emot_verbosity,
			u_from_goal,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(a), #\(b), #\(o), #\(u), #\(t)]),
			u_verbose_c63
		      ]
		    ],
		    
		    [ 
		      [ and,
			u_toward,
			u_from_goal,
			[u_social_esteem_goal_c63, u_from_goal]
		      ],
		      
		      [ u_gen_pos_neg_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(p), #\(r), #\(o), #\(u), #\(d)]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(h),
				   #\(u),
				   #\(m),
				   #\(i),
				   #\(l),
				   #\(i),
				   #\(a),
				   #\(t),
				   #\(e),
				   #\(d)
				 ])
		      ]
		    ],
		    
		    [ u_toward,
		      
		      [ u_gen_gratitude_anger,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			u_toward
		      ]
		    ],
		    
		    [ [and, u_from_goal, [u_social_esteem_goal_c63, u_from_goal]],
		      
		      [ u_gen_pos_neg_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(p), #\(o), #\(i), #\(s), #\(e), #\(d)]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(e),
				   #\(m),
				   #\(b),
				   #\(a),
				   #\(r),
				   #\(r),
				   #\(a),
				   #\(s),
				   #\(s),
				   #\(e),
				   #\(d)
				 ])
		      ]
		    ],
		    
		    [ [and, u_from_goal, [u_self_esteem_goal_c63, u_from_goal]],
		      
		      [ u_gen_pos_neg_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(p), #\(r), #\(o), #\(u), #\(d)]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(s),
				   #\(h),
				   #\(a),
				   #\(m),
				   #\(e),
				   #\(d)
				 ])
		      ]
		    ],
		    
		    [ 
		      [ and,
			u_from_goal,
			[u_ty_c36_instance_c63, u_con, [quote, u_neg_emotion]],
			[u_lovers_goal_c63, u_from_goal],
			
			[ u_existing_relationship1_c63,
			  [u_ob_c36_get, u_from_goal, [quote, u_obj]],
			  
			  [ or,
			    
			    [ u_ob_c36_get,
			      u_from_goal,
			      [quote, u_activation_context]
			    ],
			    [u_cx_c36_parent, u_context]
			  ]
			]
		      ],
		      
		      [ u_gen_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(b),
				   #\(r),
				   #\(o),
				   #\(k),
				   #\(e),
				   #\(n),
				   #\(' '),
				   #\(h),
				   #\(e),
				   #\(a),
				   #\(r),
				   #\(t),
				   #\(e),
				   #\(d)
				 ])
		      ]
		    ],
		    
		    [ 
		      [ and,
			u_from_goal,
			[u_ty_c36_instance_c63, u_con, [quote, u_neg_emotion]],
			
			[ or,
			  [u_lovers_goal_c63, u_from_goal],
			  [u_friends_goal_c63, u_from_goal]
			]
		      ],
		      
		      [ u_gen_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(r),
				   #\(e),
				   #\(j),
				   #\(e),
				   #\(c),
				   #\(t),
				   #\(e),
				   #\(d)
				 ])
		      ]
		    ],
		    
		    [ 
		      [ and,
			u_from_goal,
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_from_goal, [quote, u_obj]],
			  [quote, u_entertainment]
			],
			[u_ty_c36_instance_c63, u_con, [quote, u_pos_emotion]]
		      ],
		      
		      [ u_gen_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(a), #\(m), #\(u), #\(s), #\(e), #\(d)])
		      ]
		    ],
		    
		    [ 
		      [ and,
			u_from_goal,
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_from_goal, [quote, u_obj]],
			  [quote, u_food]
			]
		      ],
		      
		      [ u_gen_pos_neg_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(s),
				   #\(a),
				   #\(t),
				   #\(i),
				   #\(a),
				   #\(t),
				   #\(e),
				   #\(d)
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(s),
				   #\(t),
				   #\(a),
				   #\(r),
				   #\(v),
				   #\(e),
				   #\(d)
				 ])
		      ]
		    ],
		    
		    [ u_else,
		      
		      [ u_gen_pos_neg_emot_string,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			u_scale,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(p),
				   #\(l),
				   #\(e),
				   #\(a),
				   #\(s),
				   #\(e),
				   #\(d)
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(d),
				   #\(i),
				   #\(s),
				   #\(p),
				   #\(l),
				   #\(e),
				   #\(a),
				   #\(s),
				   #\(e),
				   #\(d)
				 ])
		      ],
		      
		      [ u_gen_emot_verbosity,
			u_from_goal,
			stream,
			u_switches,
			u_context,
			u_bp,
			u_subject,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(a), #\(b), #\(o), #\(u), #\(t)]),
			u_verbose_c63
		      ]
		    ]
		  ],
		  u_subject
		]
	      ]).


% annotating U::GENERATE-EMOTION 
wl: arglist_info(u_generate_emotion,
		
		[ u_con,
		  stream,
		  u_switches,
		  u_context,
		  u_bp,
		  u_with_subject_c63,
		  u_verbose_c63
		],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  With_subject_c63_Param,
		  Verbose_c63_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_with_subject_c63,
			       u_verbose_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_with_subject_c63,
				 u_verbose_c63
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_with_subject_c63,
			       u_verbose_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GENERATE-EMOTION 
wl: init_args(exact_only, u_generate_emotion).


% annotating U::GENERATE-EMOTION 
f_u_generate_emotion(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, With_subject_c63_Param, Verbose_c63_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param), bv(u_with_subject_c63, With_subject_c63_Param), bv(u_verbose_c63, Verbose_c63_Param)],
	f_u_ob_c36_get(Con_Param, u_strength, Scale_Init),
	(   With_subject_c63_Param\==[]
	->  cl_car(Bp_Param, TrueResult),
	    Subject_Init=TrueResult
	;   Subject_Init=[]
	),
	f_u_ob_c36_get(Con_Param, u_to, Toward_Init),
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	f_u_ol_get(Con_Param,
		   Xx_dependency_ob_xx_Get,
		   u_forward,
		   Context_Param,
		   Goal_car_Param),
	f_u_goal_car(Goal_car_Param, To_goal_Init),
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get37),
	f_u_ol_get(Con_Param,
		   Xx_dependency_ob_xx_Get37,
		   u_backward,
		   Context_Param,
		   Goal_car_Param295),
	f_u_goal_car(Goal_car_Param295, From_goal_Init),
	LEnv=[[bv(u_scale, Scale_Init), bv(u_subject, Subject_Init), bv(u_toward, Toward_Init), bv(u_to_goal, To_goal_Init), bv(u_from_goal, From_goal_Init)]|Env],
	f_u_ty_c36_instance_c63(Con_Param, u_surprise, IFTEST44),
	(   IFTEST44\==[]
	->  f_u_gs_string_write(Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\('W'),
					   #\(h),
					   #\(a),
					   #\(t),
					   #\(' '),
					   #\(d),
					   #\(o),
					   #\(' '),
					   #\(y),
					   #\(o),
					   #\(u),
					   #\(' '),
					   #\(k),
					   #\(n),
					   #\(o),
					   #\(w)
					 ]),
				TrueResult286),
	    ElseResult265=TrueResult286
	;   f_u_ob_c36_get(Con_Param, u_altern_c63, IFTEST49),
	    (   IFTEST49\==[]
	    ->  get_var(LEnv, u_scale, Scale_Get),
		get_var(LEnv, u_subject, Subject_Get),
		f_u_gen_pos_neg_emot_string(Con_Param,
					    Stream_Param,
					    Switches_Param,
					    Context_Param,
					    Bp_Param,
					    Subject_Get,
					    Scale_Get,
					    '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\(' '),
						       #\(r),
						       #\(e),
						       #\(l),
						       #\(i),
						       #\(e),
						       #\(v),
						       #\(e),
						       #\(d)
						     ]),
					    '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\(' '),
						       #\(r),
						       #\(e),
						       #\(g),
						       #\(r),
						       #\(e),
						       #\(t),
						       #\(f),
						       #\(u),
						       #\(l)
						     ]),
					    Emot_string_Ret),
		get_var(LEnv, u_from_goal, From_goal_Get),
		get_var(LEnv, u_subject, Subject_Get64),
		f_u_gen_emot_verbosity(From_goal_Get,
				       Stream_Param,
				       Switches_Param,
				       Context_Param,
				       Bp_Param,
				       Subject_Get64,
				       '$ARRAY'([*],
						claz_base_character,
						
						[ #\(' '),
						  #\(a),
						  #\(b),
						  #\(o),
						  #\(u),
						  #\(t)
						]),
				       Verbose_c63_Param,
				       TrueResult284),
		ElseResult265=TrueResult284
	    ;   get_var(LEnv, u_to_goal, IFTEST68),
		(   IFTEST68\==[]
		->  f_u_null_c63(u_from_goal, TrueResult71),
		    IFTEST66=TrueResult71
		;   IFTEST66=[]
		),
		(   IFTEST66\==[]
		->  get_var(LEnv, u_scale, Scale_Get78),
		    get_var(LEnv, u_subject, Subject_Get77),
		    f_u_gen_emot_string(Con_Param,
					Stream_Param,
					Switches_Param,
					Context_Param,
					Bp_Param,
					Subject_Get77,
					Scale_Get78,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(' '),
						   #\(i),
						   #\(n),
						   #\(t),
						   #\(e),
						   #\(r),
						   #\(e),
						   #\(s),
						   #\(t),
						   #\(e),
						   #\(d)
						 ]),
					Emot_string_Ret297),
		    get_var(LEnv, u_subject, Subject_Get84),
		    get_var(LEnv, u_to_goal, To_goal_Get79),
		    f_u_gen_emot_verbosity(To_goal_Get79,
					   Stream_Param,
					   Switches_Param,
					   Context_Param,
					   Bp_Param,
					   Subject_Get84,
					   '$ARRAY'([*],
						    claz_base_character,
						    [#\(' '), #\(i), #\(n)]),
					   Verbose_c63_Param,
					   TrueResult282),
		    ElseResult265=TrueResult282
		;   get_var(LEnv, u_to_goal, IFTEST88),
		    (   IFTEST88\==[]
		    ->  get_var(LEnv, u_from_goal, IFTEST91),
			(   IFTEST91\==[]
			->  get_var(LEnv, u_from_goal, From_goal_Get94),
			    f_u_imagined_outcome_c63(From_goal_Get94,
						     TrueResult95),
			    IFTEST86=TrueResult95
			;   IFTEST86=[]
			)
		    ;   IFTEST86=[]
		    ),
		    (   IFTEST86\==[]
		    ->  get_var(LEnv, u_scale, Scale_Get103),
			get_var(LEnv, u_subject, Subject_Get102),
			f_u_gen_pos_neg_emot_string(Con_Param,
						    Stream_Param,
						    Switches_Param,
						    Context_Param,
						    Bp_Param,
						    Subject_Get102,
						    Scale_Get103,
						    '$ARRAY'([*],
							     claz_base_character,
							     
							     [ #\(' '),
							       #\(h),
							       #\(o),
							       #\(p),
							       #\(e),
							       #\(f),
							       #\(u),
							       #\(l)
							     ]),
						    '$ARRAY'([*],
							     claz_base_character,
							     
							     [ #\(' '),
							       #\(w),
							       #\(o),
							       #\(r),
							       #\(r),
							       #\(i),
							       #\(e),
							       #\(d)
							     ]),
						    Emot_string_Ret298),
			get_var(LEnv, u_from_goal, From_goal_Get104),
			get_var(LEnv, u_subject, Subject_Get109),
			f_u_gen_emot_verbosity(From_goal_Get104,
					       Stream_Param,
					       Switches_Param,
					       Context_Param,
					       Bp_Param,
					       Subject_Get109,
					       '$ARRAY'([*],
							claz_base_character,
							
							[ #\(' '),
							  #\(a),
							  #\(b),
							  #\(o),
							  #\(u),
							  #\(t)
							]),
					       Verbose_c63_Param,
					       TrueResult280),
			ElseResult265=TrueResult280
		    ;   get_var(LEnv, u_toward, IFTEST113),
			(   IFTEST113\==[]
			->  get_var(LEnv, u_from_goal, IFTEST116),
			    (   IFTEST116\==[]
			    ->  get_var(LEnv, u_from_goal, From_goal_Get119),
				f_u_social_esteem_goal_c63(From_goal_Get119,
							   TrueResult120),
				IFTEST111=TrueResult120
			    ;   IFTEST111=[]
			    )
			;   IFTEST111=[]
			),
			(   IFTEST111\==[]
			->  get_var(LEnv, u_scale, Scale_Get128),
			    get_var(LEnv, u_subject, Subject_Get127),
			    f_u_gen_pos_neg_emot_string(Con_Param,
							Stream_Param,
							Switches_Param,
							Context_Param,
							Bp_Param,
							Subject_Get127,
							Scale_Get128,
							'$ARRAY'([*],
								 claz_base_character,
								 
								 [ #\(' '),
								   #\(p),
								   #\(r),
								   #\(o),
								   #\(u),
								   #\(d)
								 ]),
							'$ARRAY'([*],
								 claz_base_character,
								 
								 [ #\(' '),
								   #\(h),
								   #\(u),
								   #\(m),
								   #\(i),
								   #\(l),
								   #\(i),
								   #\(a),
								   #\(t),
								   #\(e),
								   #\(d)
								 ]),
							TrueResult278),
			    ElseResult265=TrueResult278
			;   get_var(LEnv, u_toward, IFTEST129),
			    (   IFTEST129\==[]
			    ->  get_var(LEnv, u_scale, Scale_Get138),
				get_var(LEnv, u_subject, Subject_Get137),
				get_var(LEnv, u_toward, Toward_Get139),
				f_u_gen_gratitude_anger(Con_Param,
							Stream_Param,
							Switches_Param,
							Context_Param,
							Bp_Param,
							Subject_Get137,
							Scale_Get138,
							Toward_Get139,
							TrueResult276),
				ElseResult265=TrueResult276
			    ;   get_var(LEnv, u_from_goal, IFTEST142),
				(   IFTEST142\==[]
				->  get_var(LEnv, u_from_goal, From_goal_Get145),
				    f_u_social_esteem_goal_c63(From_goal_Get145,
							       TrueResult146),
				    IFTEST140=TrueResult146
				;   IFTEST140=[]
				),
				(   IFTEST140\==[]
				->  get_var(LEnv, u_scale, Scale_Get153),
				    get_var(LEnv, u_subject, Subject_Get152),
				    f_u_gen_pos_neg_emot_string(Con_Param,
								Stream_Param,
								Switches_Param,
								Context_Param,
								Bp_Param,
								Subject_Get152,
								Scale_Get153,
								'$ARRAY'([*],
									 claz_base_character,
									 
									 [ #\(' '),
									   #\(p),
									   #\(o),
									   #\(i),
									   #\(s),
									   #\(e),
									   #\(d)
									 ]),
								'$ARRAY'([*],
									 claz_base_character,
									 
									 [ #\(' '),
									   #\(e),
									   #\(m),
									   #\(b),
									   #\(a),
									   #\(r),
									   #\(r),
									   #\(a),
									   #\(s),
									   #\(s),
									   #\(e),
									   #\(d)
									 ]),
								TrueResult274),
				    ElseResult265=TrueResult274
				;   get_var(LEnv, u_from_goal, IFTEST156),
				    (   IFTEST156\==[]
				    ->  get_var(LEnv,
						u_from_goal,
						From_goal_Get159),
					f_u_self_esteem_goal_c63(From_goal_Get159,
								 TrueResult160),
					IFTEST154=TrueResult160
				    ;   IFTEST154=[]
				    ),
				    (   IFTEST154\==[]
				    ->  get_var(LEnv, u_scale, Scale_Get167),
					get_var(LEnv, u_subject, Subject_Get166),
					f_u_gen_pos_neg_emot_string(Con_Param,
								    Stream_Param,
								    Switches_Param,
								    Context_Param,
								    Bp_Param,
								    Subject_Get166,
								    Scale_Get167,
								    '$ARRAY'([*],
									     claz_base_character,
									     
									     [ #\(' '),
									       #\(p),
									       #\(r),
									       #\(o),
									       #\(u),
									       #\(d)
									     ]),
								    '$ARRAY'([*],
									     claz_base_character,
									     
									     [ #\(' '),
									       #\(a),
									       #\(s),
									       #\(h),
									       #\(a),
									       #\(m),
									       #\(e),
									       #\(d)
									     ]),
								    TrueResult272),
					ElseResult265=TrueResult272
				    ;   get_var(LEnv, u_from_goal, IFTEST170),
					(   IFTEST170\==[]
					->  f_u_ty_c36_instance_c63(Con_Param,
								    u_neg_emotion,
								    IFTEST173),
					    (   IFTEST173\==[]
					    ->  get_var(LEnv,
							u_from_goal,
							From_goal_Get178),
						f_u_lovers_goal_c63(From_goal_Get178,
								    IFTEST176),
						(   IFTEST176\==[]
						->  get_var(LEnv,
							    u_from_goal,
							    From_goal_Get179),
						    f_u_ob_c36_get(From_goal_Get179,
								   u_obj,
								   Obj),
						    (   get_var(LEnv,
								u_from_goal,
								From_goal_Get180),
							f_u_ob_c36_get(From_goal_Get180,
								       u_activation_context,
								       FORM1_Res),
							FORM1_Res\==[],
							_81378=FORM1_Res
						    ->  true
						    ;   f_u_cx_c36_parent(Context_Param,
									  C36_parent_Ret),
							_81378=C36_parent_Ret
						    ),
						    f_u_existing_relationship1_c63(Obj,
										   _81378,
										   TrueResult183),
						    IFTEST168=TrueResult183
						;   IFTEST168=[]
						)
					    ;   IFTEST168=[]
					    )
					;   IFTEST168=[]
					),
					(   IFTEST168\==[]
					->  get_var(LEnv, u_scale, Scale_Get192),
					    get_var(LEnv,
						    u_subject,
						    Subject_Get191),
					    f_u_gen_emot_string(Con_Param,
								Stream_Param,
								Switches_Param,
								Context_Param,
								Bp_Param,
								Subject_Get191,
								Scale_Get192,
								'$ARRAY'([*],
									 claz_base_character,
									 
									 [ #\(' '),
									   #\(b),
									   #\(r),
									   #\(o),
									   #\(k),
									   #\(e),
									   #\(n),
									   #\(' '),
									   #\(h),
									   #\(e),
									   #\(a),
									   #\(r),
									   #\(t),
									   #\(e),
									   #\(d)
									 ]),
								TrueResult270),
					    ElseResult265=TrueResult270
					;   get_var(LEnv,
						    u_from_goal,
						    IFTEST195),
					    (   IFTEST195\==[]
					    ->  f_u_ty_c36_instance_c63(Con_Param,
									u_neg_emotion,
									IFTEST198),
						(   IFTEST198\==[]
						->  (   get_var(LEnv,
								u_from_goal,
								From_goal_Get201),
							f_u_lovers_goal_c63(From_goal_Get201,
									    FORM1_Res203),
							FORM1_Res203\==[],
							IFTEST193=FORM1_Res203
						    ->  true
						    ;   get_var(LEnv,
								u_from_goal,
								From_goal_Get202),
							f_u_friends_goal_c63(From_goal_Get202,
									     Goal_c63_Ret),
							IFTEST193=Goal_c63_Ret
						    )
						;   IFTEST193=[]
						)
					    ;   IFTEST193=[]
					    ),
					    (   IFTEST193\==[]
					    ->  get_var(LEnv,
							u_scale,
							Scale_Get212),
						get_var(LEnv,
							u_subject,
							Subject_Get211),
						f_u_gen_emot_string(Con_Param,
								    Stream_Param,
								    Switches_Param,
								    Context_Param,
								    Bp_Param,
								    Subject_Get211,
								    Scale_Get212,
								    '$ARRAY'([*],
									     claz_base_character,
									     
									     [ #\(' '),
									       #\(r),
									       #\(e),
									       #\(j),
									       #\(e),
									       #\(c),
									       #\(t),
									       #\(e),
									       #\(d)
									     ]),
								    TrueResult268),
						ElseResult265=TrueResult268
					    ;   get_var(LEnv,
							u_from_goal,
							IFTEST215),
						(   IFTEST215\==[]
						->  get_var(LEnv,
							    u_from_goal,
							    From_goal_Get220),
						    f_u_ob_c36_get(From_goal_Get220,
								   u_obj,
								   Obj292),
						    f_u_ty_c36_instance_c63(Obj292,
									    u_entertainment,
									    IFTEST218),
						    (   IFTEST218\==[]
						    ->  f_u_ty_c36_instance_c63(Con_Param,
										u_pos_emotion,
										TrueResult222),
							IFTEST213=TrueResult222
						    ;   IFTEST213=[]
						    )
						;   IFTEST213=[]
						),
						(   IFTEST213\==[]
						->  get_var(LEnv,
							    u_scale,
							    Scale_Get230),
						    get_var(LEnv,
							    u_subject,
							    Subject_Get229),
						    f_u_gen_emot_string(Con_Param,
									Stream_Param,
									Switches_Param,
									Context_Param,
									Bp_Param,
									Subject_Get229,
									Scale_Get230,
									'$ARRAY'([*],
										 claz_base_character,
										 
										 [ #\(' '),
										   #\(a),
										   #\(m),
										   #\(u),
										   #\(s),
										   #\(e),
										   #\(d)
										 ]),
									TrueResult266),
						    ElseResult265=TrueResult266
						;   get_var(LEnv,
							    u_from_goal,
							    IFTEST233),
						    (   IFTEST233\==[]
						    ->  get_var(LEnv,
								u_from_goal,
								From_goal_Get236),
							f_u_ob_c36_get(From_goal_Get236,
								       u_obj,
								       Obj293),
							f_u_ty_c36_instance_c63(Obj293,
										u_food,
										TrueResult237),
							IFTEST231=TrueResult237
						    ;   IFTEST231=[]
						    ),
						    (   IFTEST231\==[]
						    ->  get_var(LEnv,
								u_scale,
								Scale_Get244),
							get_var(LEnv,
								u_subject,
								Subject_Get243),
							f_u_gen_pos_neg_emot_string(Con_Param,
										    Stream_Param,
										    Switches_Param,
										    Context_Param,
										    Bp_Param,
										    Subject_Get243,
										    Scale_Get244,
										    '$ARRAY'([*],
											     claz_base_character,
											     
											     [ #\(' '),
											       #\(s),
											       #\(a),
											       #\(t),
											       #\(i),
											       #\(a),
											       #\(t),
											       #\(e),
											       #\(d)
											     ]),
										    '$ARRAY'([*],
											     claz_base_character,
											     
											     [ #\(' '),
											       #\(s),
											       #\(t),
											       #\(a),
											       #\(r),
											       #\(v),
											       #\(e),
											       #\(d)
											     ]),
										    TrueResult264),
							ElseResult265=TrueResult264
						    ;   get_var(LEnv,
								u_else,
								IFTEST245),
							(   IFTEST245\==[]
							->  get_var(LEnv,
								    u_scale,
								    Scale_Get254),
							    get_var(LEnv,
								    u_subject,
								    Subject_Get253),
							    f_u_gen_pos_neg_emot_string(Con_Param,
											Stream_Param,
											Switches_Param,
											Context_Param,
											Bp_Param,
											Subject_Get253,
											Scale_Get254,
											'$ARRAY'([*],
												 claz_base_character,
												 
												 [ #\(' '),
												   #\(p),
												   #\(l),
												   #\(e),
												   #\(a),
												   #\(s),
												   #\(e),
												   #\(d)
												 ]),
											'$ARRAY'([*],
												 claz_base_character,
												 
												 [ #\(' '),
												   #\(d),
												   #\(i),
												   #\(s),
												   #\(p),
												   #\(l),
												   #\(e),
												   #\(a),
												   #\(s),
												   #\(e),
												   #\(d)
												 ]),
											Emot_string_Ret301),
							    get_var(LEnv,
								    u_from_goal,
								    From_goal_Get255),
							    get_var(LEnv,
								    u_subject,
								    Subject_Get260),
							    f_u_gen_emot_verbosity(From_goal_Get255,
										   Stream_Param,
										   Switches_Param,
										   Context_Param,
										   Bp_Param,
										   Subject_Get260,
										   '$ARRAY'([*],
											    claz_base_character,
											    
											    [ #\(' '),
											      #\(a),
											      #\(b),
											      #\(o),
											      #\(u),
											      #\(t)
											    ]),
										   Verbose_c63_Param,
										   TrueResult262),
							    ElseResult265=TrueResult262
							;   ElseResult265=[]
							)
						    )
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
	    )
	),
	get_var(LEnv, u_subject, Subject_Get288),
	LetResult=Subject_Get288,
	LetResult=FnResult.
:- set_opv(f_u_generate_emotion, classof, claz_function),
   set_opv(u_generate_emotion, compile_as, kw_function),
   set_opv(u_generate_emotion, function, f_u_generate_emotion),
   DefunResult=u_generate_emotion.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("     (gen-emot-string con stream switches context bp subject",
				     1,
				     18232)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      (fl+ *less-than-norm* .1)",
				     1,
				     18294)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:17781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                      \" surprised\")",
				     1,
				     18343)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21080 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'imagined-outcome?',
			    [goal],
			    
			    [ 'dd-goal?',
			      ['ob$get', goal, [quote, 'top-level-goal']]
			    ]
			  ]).

% annotating U::IMAGINED-OUTCOME? 
wl: lambda_def(defun,
	      u_imagined_outcome_c63,
	      f_u_imagined_outcome_c63,
	      [u_goal],
	      [[u_dd_goal_c63, [u_ob_c36_get, u_goal, [quote, u_top_level_goal]]]]).


% annotating U::IMAGINED-OUTCOME? 
wl: arglist_info(u_imagined_outcome_c63,
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

% annotating U::IMAGINED-OUTCOME? 
wl: init_args(exact_only, u_imagined_outcome_c63).


% annotating U::IMAGINED-OUTCOME? 
f_u_imagined_outcome_c63(Goal_Param, FnResult) :-
	f_u_ob_c36_get(Goal_Param, u_top_level_goal, Top_level_goal),
	f_u_dd_goal_c63(Top_level_goal, Goal_c63_Ret),
	Goal_c63_Ret=FnResult.
:- set_opv(f_u_imagined_outcome_c63, classof, claz_function),
   set_opv(u_imagined_outcome_c63, compile_as, kw_function),
   set_opv(u_imagined_outcome_c63, function, f_u_imagined_outcome_c63),
   DefunResult=u_imagined_outcome_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21157 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-emot-verbosity',
			    
			    [ goal,
			      stream,
			      switches,
			      context,
			      bp,
			      subject,
			      prep,
			      'verbose?'
			    ],
			    
			    [ if,
			      [and, 'verbose?', goal],
			      
			      [ progn,
				['gs-string-write', stream, prep],
				
				[ gen,
				  
				  [ if,
				    
				    [ or,
				      
				      [ 'ty$instance?',
					goal,
					[quote, 'succeeded-goal']
				      ],
				      
				      [ 'ty$instance?',
					goal,
					[quote, 'active-goal']
				      ]
				    ],
				    ['ob$get', goal, [quote, obj]],
				    goal
				  ],
				  stream,
				  
				  [ cons,
				    [list, [quote, 's-bar'], subject],
				    [cons, [quote, [tense, gerund]], switches]
				  ],
				  context,
				  bp
				]
			      ]
			    ]
			  ]).

% annotating U::GEN-EMOT-VERBOSITY 
wl: lambda_def(defun,
	      u_gen_emot_verbosity,
	      f_u_gen_emot_verbosity,
	      
	      [ u_goal,
		stream,
		u_switches,
		u_context,
		u_bp,
		u_subject,
		u_prep,
		u_verbose_c63
	      ],
	      
	      [ 
		[ if,
		  [and, u_verbose_c63, u_goal],
		  
		  [ progn,
		    [u_gs_string_write, stream, u_prep],
		    
		    [ u_gen,
		      
		      [ if,
			
			[ or,
			  
			  [ u_ty_c36_instance_c63,
			    u_goal,
			    [quote, u_succeeded_goal]
			  ],
			  [u_ty_c36_instance_c63, u_goal, [quote, u_active_goal]]
			],
			[u_ob_c36_get, u_goal, [quote, u_obj]],
			u_goal
		      ],
		      stream,
		      
		      [ cons,
			[list, [quote, u_s_bar], u_subject],
			[cons, [quote, [u_tense, u_gerund]], u_switches]
		      ],
		      u_context,
		      u_bp
		    ]
		  ]
		]
	      ]).


% annotating U::GEN-EMOT-VERBOSITY 
wl: arglist_info(u_gen_emot_verbosity,
		
		[ u_goal,
		  stream,
		  u_switches,
		  u_context,
		  u_bp,
		  u_subject,
		  u_prep,
		  u_verbose_c63
		],
		
		[ Goal_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Subject_Param,
		  Prep_Param,
		  Verbose_c63_Param
		],
		arginfo{ all:
			     [ u_goal,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_prep,
			       u_verbose_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_goal,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_subject,
				 u_prep,
				 u_verbose_c63
			       ],
			 opt:0,
			 req:
			     [ u_goal,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_subject,
			       u_prep,
			       u_verbose_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-EMOT-VERBOSITY 
wl: init_args(exact_only, u_gen_emot_verbosity).


% annotating U::GEN-EMOT-VERBOSITY 
f_u_gen_emot_verbosity(Goal_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Subject_Param, Prep_Param, Verbose_c63_Param, FnResult) :-
	(   Verbose_c63_Param\==[]
	->  IFTEST=Goal_Param
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_gs_string_write(Stream_Param, Prep_Param, String_write_Ret),
	    (   f_u_ty_c36_instance_c63(Goal_Param, u_succeeded_goal, FORM1_Res),
		FORM1_Res\==[],
		IFTEST35=FORM1_Res
	    ->  true
	    ;   f_u_ty_c36_instance_c63(Goal_Param, u_active_goal, Active_goal),
		IFTEST35=Active_goal
	    ),
	    (   IFTEST35\==[]
	    ->  f_u_ob_c36_get(Goal_Param, u_obj, TrueResult42),
		Gen_Param=TrueResult42
	    ;   Gen_Param=Goal_Param
	    ),
	    CAR=[u_s_bar, Subject_Param],
	    CDR=[[u_tense, u_gerund]|Switches_Param],
	    _67572=[CAR|CDR],
	    f_u_gen(Gen_Param,
		    Stream_Param,
		    _67572,
		    Context_Param,
		    Bp_Param,
		    TrueResult49),
	    FnResult=TrueResult49
	;   FnResult=[]
	).
:- set_opv(f_u_gen_emot_verbosity, classof, claz_function),
   set_opv(u_gen_emot_verbosity, compile_as, kw_function),
   set_opv(u_gen_emot_verbosity, function, f_u_gen_emot_verbosity),
   DefunResult=u_gen_emot_verbosity.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21648 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*less-than-norm*', 0.3]).
:- set_var(TLEnv3, setq, u_xx_less_than_norm_xx, 0.3).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21676 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*greater-than-norm*', 0.7]).
:- set_var(TLEnv3, setq, u_xx_greater_than_norm_xx, 0.7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21707 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-scale',
			    [scale, stream],
			    
			    [ if,
			      scale,
			      
			      [ cond,
				
				[ [<, scale, '*less-than-norm*'],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" a bit")
				  ]
				],
				
				[ [>, scale, '*greater-than-norm*'],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" really")
				  ]
				]
			      ],
			      []
			    ]
			  ]).

% annotating U::GEN-SCALE 
wl: lambda_def(defun,
	      u_gen_scale,
	      f_u_gen_scale,
	      [u_scale, stream],
	      
	      [ 
		[ if,
		  u_scale,
		  
		  [ cond,
		    
		    [ [<, u_scale, u_xx_less_than_norm_xx],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(a), #\(' '), #\(b), #\(i), #\(t)])
		      ]
		    ],
		    
		    [ [>, u_scale, u_xx_greater_than_norm_xx],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(r), #\(e), #\(a), #\(l), #\(l), #\(y)])
		      ]
		    ]
		  ],
		  []
		]
	      ]).


% annotating U::GEN-SCALE 
wl: arglist_info(u_gen_scale,
		[u_scale, stream],
		[Scale_Param, Stream_Param],
		arginfo{ all:[u_scale, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_scale, stream],
			 opt:0,
			 req:[u_scale, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-SCALE 
wl: init_args(exact_only, u_gen_scale).


% annotating U::GEN-SCALE 
f_u_gen_scale(Scale_Param, Stream_Param, ElseResult34) :-
	Env=[bv(u_scale, Scale_Param), bv(stream, Stream_Param)],
	(   Scale_Param\==[]
	->  get_var(Env, u_xx_less_than_norm_xx, Xx_less_than_norm_xx_Get),
	    (   Scale_Param<Xx_less_than_norm_xx_Get
	    ->  f_u_gs_string_write(Stream_Param,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(' '),
					       #\(a),
					       #\(' '),
					       #\(b),
					       #\(i),
					       #\(t)
					     ]),
				    TrueResult33),
		ElseResult34=TrueResult33
	    ;   get_var(Env,
			u_xx_greater_than_norm_xx,
			Xx_greater_than_norm_xx_Get),
		(   Scale_Param>Xx_greater_than_norm_xx_Get
		->  f_u_gs_string_write(Stream_Param,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(' '),
						   #\(r),
						   #\(e),
						   #\(a),
						   #\(l),
						   #\(l),
						   #\(y)
						 ]),
					TrueResult),
		    ElseResult34=TrueResult
		;   ElseResult34=[]
		)
	    )
	;   ElseResult34=[]
	).
:- set_opv(f_u_gen_scale, classof, claz_function),
   set_opv(u_gen_scale, compile_as, kw_function),
   set_opv(u_gen_scale, function, f_u_gen_scale),
   DefunResult=u_gen_scale.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21707 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" also slightly", 40, 21830)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21707 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" also very", 42, 21921)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:21945 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'WAIT',
			    [],
			    
			    [ let,
			      [[subject, [car, bp]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, wait],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_wait,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [car, u_bp]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_wait],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:22127 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'POS-ATTITUDE',
			    [],
			    
			    [ let,
			      
			      [ [subject, [car, bp]],
				[obj, ['ob$get', con, [quote, obj]]]
			      ],
			      
			      [ if,
				[],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, think],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" highly of")
				  ],
				  [gen, obj, stream, switches, context, bp],
				  subject
				],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, like],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  [gen, obj, stream, switches, context, bp],
				  subject
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_pos_attitude,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [car, u_bp]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]
		      ],
		      
		      [ if,
			[],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_think],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(h),
				       #\(i),
				       #\(g),
				       #\(h),
				       #\(l),
				       #\(y),
				       #\(' '),
				       #\(o),
				       #\(f)
				     ])
			  ],
			  [u_gen, u_obj, stream, u_switches, u_context, u_bp],
			  u_subject
			],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_like],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  [u_gen, u_obj, stream, u_switches, u_context, u_bp],
			  u_subject
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:22127 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ty$instance? obj 'person)", 13, 22229)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:22127 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 964.4 (Roget's Fourth Edition)",
				     10,
				     22282)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:22127 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This is too strong. Maybe \" appreciates\"",
				     10,
				     22444)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:22799 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'NEG-ATTITUDE',
			    [],
			    
			    [ let,
			      
			      [ [subject, [car, bp]],
				[obj, ['ob$get', con, [quote, obj]]]
			      ],
			      
			      [ if,
				['ty$instance?', obj, [quote, person]],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, think],
				    subject,
				    stream,
				    switches,
				    [not, ['neg?', con]]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" much of")
				  ],
				  [gen, obj, stream, switches, context, bp],
				  subject
				],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, dislike],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  [gen, obj, stream, switches, context, bp],
				  subject
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_neg_attitude,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [car, u_bp]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]
		      ],
		      
		      [ if,
			[u_ty_c36_instance_c63, u_obj, [quote, u_person]],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_think],
			    u_subject,
			    stream,
			    u_switches,
			    [not, [u_neg_c63, u_con]]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(m),
				       #\(u),
				       #\(c),
				       #\(h),
				       #\(' '),
				       #\(o),
				       #\(f)
				     ])
			  ],
			  [u_gen, u_obj, stream, u_switches, u_context, u_bp],
			  u_subject
			],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_dislike],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  [u_gen, u_obj, stream, u_switches, u_context, u_bp],
			  u_subject
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:23378 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ROMANTIC-INTEREST',
			    [],
			    
			    [ let,
			      [[subject, [car, bp]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, be],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" interested in")
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				switches,
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_romantic_interest,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [car, u_bp]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_be],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(i),
				   #\(n),
				   #\(t),
				   #\(e),
				   #\(r),
				   #\(e),
				   #\(s),
				   #\(t),
				   #\(e),
				   #\(d),
				   #\(' '),
				   #\(i),
				   #\(n)
				 ])
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:23663 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-regular',
			    [con, stream, switches, context, bp, verb, str],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				verb,
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      [if, str, ['gs-string-write', stream, str]],
			      subject
			    ]
			  ]).

% annotating U::GEN-REGULAR 
wl: lambda_def(defun,
	      u_gen_regular,
	      f_u_gen_regular,
	      [u_con, stream, u_switches, u_context, u_bp, u_verb, u_str],
	      
	      [ 
		[ let,
		  [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		  [u_gen_subject, u_subject, stream, u_switches, u_context, u_bp],
		  
		  [ u_gen_verb,
		    u_verb,
		    u_subject,
		    stream,
		    u_switches,
		    [u_neg_c63, u_con]
		  ],
		  [if, u_str, [u_gs_string_write, stream, u_str]],
		  u_subject
		]
	      ]).


% annotating U::GEN-REGULAR 
wl: arglist_info(u_gen_regular,
		[u_con, stream, u_switches, u_context, u_bp, u_verb, u_str],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Verb_Param,
		  Str_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_str
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_verb,
				 u_str
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_str
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-REGULAR 
wl: init_args(exact_only, u_gen_regular).


% annotating U::GEN-REGULAR 
f_u_gen_regular(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Verb_Param, Str_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param), bv(u_verb, Verb_Param), bv(u_str, Str_Param)],
	f_u_ob_c36_gets(Con_Param, u_actor, Subject_Init),
	LEnv=[[bv(u_subject, Subject_Init)]|Env],
	get_var(LEnv, u_subject, Subject_Get),
	f_u_gen_subject(Subject_Get,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			Gen_subject_Ret),
	get_var(LEnv, u_subject, Subject_Get35),
	f_u_neg_c63(Con_Param, Neg_c63_Ret),
	f_u_gen_verb(Verb_Param,
		     Subject_Get35,
		     Stream_Param,
		     Switches_Param,
		     Neg_c63_Ret,
		     Gen_verb_Ret),
	(   Str_Param\==[]
	->  f_u_gs_string_write(Stream_Param, Str_Param, TrueResult),
	    _67508=TrueResult
	;   _67508=[]
	),
	get_var(LEnv, u_subject, Subject_Get45),
	LetResult=Subject_Get45,
	LetResult=FnResult.
:- set_opv(f_u_gen_regular, classof, claz_function),
   set_opv(u_gen_regular, compile_as, kw_function),
   set_opv(u_gen_regular, function, f_u_gen_regular),
   DefunResult=u_gen_regular.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:23929 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'RICH',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" rich")
			    ]
			  ]).
:- f_u_define_gen(u_rich,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(r), #\(i), #\(c), #\(h)])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:24011 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-regular-plus',
			    [con, stream, switches, context, bp, verb, str, plus],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				verb,
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ if,
				plus,
				
				[ if,
				  
				  [ and,
				    str,
				    ['is-particle?', str],
				    ['null?', [cdr, subject]],
				    ['memq?', [car, subject], '*references*']
				  ],
				  
				  [ progn,
				    [gen, plus, stream, switches, context, bp],
				    ['gs-string-write', stream, str]
				  ],
				  
				  [ progn,
				    [if, str, ['gs-string-write', stream, str]],
				    [gen, plus, stream, switches, context, bp]
				  ]
				]
			      ],
			      subject
			    ]
			  ]).

% annotating U::GEN-REGULAR-PLUS 
wl: lambda_def(defun,
	      u_gen_regular_plus,
	      f_u_gen_regular_plus,
	      [u_con, stream, u_switches, u_context, u_bp, u_verb, u_str, u_plus],
	      
	      [ 
		[ let,
		  [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		  [u_gen_subject, u_subject, stream, u_switches, u_context, u_bp],
		  
		  [ u_gen_verb,
		    u_verb,
		    u_subject,
		    stream,
		    u_switches,
		    [u_neg_c63, u_con]
		  ],
		  
		  [ if,
		    u_plus,
		    
		    [ if,
		      
		      [ and,
			u_str,
			[u_is_particle_c63, u_str],
			[u_null_c63, [cdr, u_subject]],
			[u_memq_c63, [car, u_subject], u_xx_references_xx]
		      ],
		      
		      [ progn,
			[u_gen, u_plus, stream, u_switches, u_context, u_bp],
			[u_gs_string_write, stream, u_str]
		      ],
		      
		      [ progn,
			[if, u_str, [u_gs_string_write, stream, u_str]],
			[u_gen, u_plus, stream, u_switches, u_context, u_bp]
		      ]
		    ]
		  ],
		  u_subject
		]
	      ]).


% annotating U::GEN-REGULAR-PLUS 
wl: arglist_info(u_gen_regular_plus,
		[u_con, stream, u_switches, u_context, u_bp, u_verb, u_str, u_plus],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Verb_Param,
		  Str_Param,
		  Plus_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_str,
			       u_plus
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_verb,
				 u_str,
				 u_plus
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_str,
			       u_plus
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-REGULAR-PLUS 
wl: init_args(exact_only, u_gen_regular_plus).


% annotating U::GEN-REGULAR-PLUS 
f_u_gen_regular_plus(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Verb_Param, Str_Param, Plus_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param), bv(u_verb, Verb_Param), bv(u_str, Str_Param), bv(u_plus, Plus_Param)],
	f_u_ob_c36_gets(Con_Param, u_actor, Subject_Init),
	LEnv=[[bv(u_subject, Subject_Init)]|Env],
	get_var(LEnv, u_subject, Subject_Get),
	f_u_gen_subject(Subject_Get,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			Gen_subject_Ret),
	get_var(LEnv, u_subject, Subject_Get37),
	f_u_neg_c63(Con_Param, Neg_c63_Ret),
	f_u_gen_verb(Verb_Param,
		     Subject_Get37,
		     Stream_Param,
		     Switches_Param,
		     Neg_c63_Ret,
		     Gen_verb_Ret),
	(   Plus_Param\==[]
	->  (   Str_Param\==[]
	    ->  f_u_is_particle_c63(Str_Param, IFTEST49),
		(   IFTEST49\==[]
		->  f_u_null_c63([cdr, u_subject], IFTEST52),
		    (   IFTEST52\==[]
		    ->  f_u_memq_c63([car, u_subject],
				     u_xx_references_xx,
				     TrueResult),
			IFTEST44=TrueResult
		    ;   IFTEST44=[]
		    )
		;   IFTEST44=[]
		)
	    ;   IFTEST44=[]
	    ),
	    (   IFTEST44\==[]
	    ->  f_u_gen(Plus_Param,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			Gen_Ret),
		f_u_gs_string_write(Stream_Param, Str_Param, TrueResult75),
		TrueResult77=TrueResult75
	    ;   (   Str_Param\==[]
		->  f_u_gs_string_write(Stream_Param, Str_Param, TrueResult69),
		    _69418=TrueResult69
		;   _69418=[]
		),
		f_u_gen(Plus_Param,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			ElseResult),
		TrueResult77=ElseResult
	    )
	;   TrueResult77=[]
	),
	get_var(LEnv, u_subject, Subject_Get78),
	LetResult=Subject_Get78,
	LetResult=FnResult.
:- set_opv(f_u_gen_regular_plus, classof, claz_function),
   set_opv(u_gen_regular_plus, compile_as, kw_function),
   set_opv(u_gen_regular_plus, function, f_u_gen_regular_plus),
   DefunResult=u_gen_regular_plus.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:24644 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'is-particle?',
			    [str],
			    
			    [ or,
			      ['string-equal?', str, '$STRING'(" up")],
			      ['string-equal?', str, '$STRING'(" on")]
			    ]
			  ]).

% annotating U::IS-PARTICLE? 
wl: lambda_def(defun,
	      u_is_particle_c63,
	      f_u_is_particle_c63,
	      [u_str],
	      
	      [ 
		[ or,
		  
		  [ u_string_equal_c63,
		    u_str,
		    '$ARRAY'([*], claz_base_character, [#\(' '), #\(u), #\(p)])
		  ],
		  
		  [ u_string_equal_c63,
		    u_str,
		    '$ARRAY'([*], claz_base_character, [#\(' '), #\(o), #\(n)])
		  ]
		]
	      ]).


% annotating U::IS-PARTICLE? 
wl: arglist_info(u_is_particle_c63,
		[u_str],
		[Str_Param],
		arginfo{ all:[u_str],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_str],
			 opt:0,
			 req:[u_str],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::IS-PARTICLE? 
wl: init_args(exact_only, u_is_particle_c63).


% annotating U::IS-PARTICLE? 
f_u_is_particle_c63(Str_Param, FnResult) :-
	Env=[bv(u_str, Str_Param)],
	(   f_u_string_equal_c63(u_str,
				 '$ARRAY'([*],
					  claz_base_character,
					  [#\(' '), #\(u), #\(p)]),
				 FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_string_equal_c63(u_str,
				 '$ARRAY'([*],
					  claz_base_character,
					  [#\(' '), #\(o), #\(n)]),
				 Equal_c63_Ret),
	    FnResult=Equal_c63_Ret
	).
:- set_opv(f_u_is_particle_c63, classof, claz_function),
   set_opv(u_is_particle_c63, compile_as, kw_function),
   set_opv(u_is_particle_c63, function, f_u_is_particle_c63),
   DefunResult=u_is_particle_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:24737 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'POSS',
			    [],
			    
			    [ if,
			      
			      [ 'switches-lookup',
				[quote, 'active-goal?'],
				switches
			      ],
			      
			      [ 'gen-regular-plus',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, get],
				[],
				['ob$get', con, [quote, obj]]
			      ],
			      
			      [ 'gen-regular-plus',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, have],
				[],
				['ob$get', con, [quote, obj]]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_poss,
		  [],
		  
		  [ 
		    [ if,
		      [u_switches_lookup, [quote, u_active_goal_c63], u_switches],
		      
		      [ u_gen_regular_plus,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, get],
			[],
			[u_ob_c36_get, u_con, [quote, u_obj]]
		      ],
		      
		      [ u_gen_regular_plus,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, u_have],
			[],
			[u_ob_c36_get, u_con, [quote, u_obj]]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25023 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'WEARING',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, wear],
			      [],
			      ['ob$get', con, [quote, obj]]
			    ]
			  ]).
:- f_u_define_gen(u_wearing,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_wear],
		      [],
		      [u_ob_c36_get, u_con, [quote, u_obj]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25149 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'STAR',
			    [],
			    
			    [ if,
			      
			      [ and,
				['ob$get', con, [quote, level]],
				
				[ 'ty$instance?',
				  ['ob$get', con, [quote, level]],
				  [quote, 'greater-than']
				]
			      ],
			      
			      [ 'gen-regular',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, be],
				'$STRING'(" a star even more famous than he is")
			      ],
			      
			      [ 'gen-regular',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, be],
				'$STRING'(" a movie star")
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_star,
		  [],
		  
		  [ 
		    [ if,
		      
		      [ and,
			[u_ob_c36_get, u_con, [quote, u_level]],
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_con, [quote, u_level]],
			  [quote, u_greater_than]
			]
		      ],
		      
		      [ u_gen_regular,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, u_be],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(' '),
				   #\(s),
				   #\(t),
				   #\(a),
				   #\(r),
				   #\(' '),
				   #\(e),
				   #\(v),
				   #\(e),
				   #\(n),
				   #\(' '),
				   #\(m),
				   #\(o),
				   #\(r),
				   #\(e),
				   #\(' '),
				   #\(f),
				   #\(a),
				   #\(m),
				   #\(o),
				   #\(u),
				   #\(s),
				   #\(' '),
				   #\(t),
				   #\(h),
				   #\(a),
				   #\(n),
				   #\(' '),
				   #\(h),
				   #\(e),
				   #\(' '),
				   #\(i),
				   #\(s)
				 ])
		      ],
		      
		      [ u_gen_regular,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, u_be],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(' '),
				   #\(m),
				   #\(o),
				   #\(v),
				   #\(i),
				   #\(e),
				   #\(' '),
				   #\(s),
				   #\(t),
				   #\(a),
				   #\(r)
				 ])
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25149 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Look in context for who it is that this level is greater",
				     7,
				     25269)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25149 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" than.", 7, 25340)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25149 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 231.45 and 644.13 (Roget's Fourth Edition)",
				     1,
				     25534)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25578 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'WELL-DRESSED',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" dressed to kill")
			    ]
			  ]).
:- f_u_define_gen(u_well_dressed,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(d),
				 #\(r),
				 #\(e),
				 #\(s),
				 #\(s),
				 #\(e),
				 #\(d),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' '),
				 #\(k),
				 #\(i),
				 #\(l),
				 #\(l)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25679 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'EXECUTIVE',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" a powerful executive")
			    ]
			  ]).
:- f_u_define_gen(u_executive,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(' '),
				 #\(p),
				 #\(o),
				 #\(w),
				 #\(e),
				 #\(r),
				 #\(f),
				 #\(u),
				 #\(l),
				 #\(' '),
				 #\(e),
				 #\(x),
				 #\(e),
				 #\(c),
				 #\(u),
				 #\(t),
				 #\(i),
				 #\(v),
				 #\(e)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25782 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'UNDER-DOORWAY',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" under a doorway")
			    ]
			  ]).
:- f_u_define_gen(u_under_doorway,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(u),
				 #\(n),
				 #\(d),
				 #\(e),
				 #\(r),
				 #\(' '),
				 #\(a),
				 #\(' '),
				 #\(d),
				 #\(o),
				 #\(o),
				 #\(r),
				 #\(w),
				 #\(a),
				 #\(y)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25884 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'HURT',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" injured")
			    ]
			  ]).
:- f_u_define_gen(u_hurt,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(i),
				 #\(n),
				 #\(j),
				 #\(u),
				 #\(r),
				 #\(e),
				 #\(d)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:25969 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'INSURED',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" insured")
			    ]
			  ]).
:- f_u_define_gen(u_insured,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(i),
				 #\(n),
				 #\(s),
				 #\(u),
				 #\(r),
				 #\(e),
				 #\(d)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26057 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'FASHIONABLE-CLOTHES',
			    [],
			    
			    [ 'gs-string-write',
			      stream,
			      '$STRING'(" my cute outfit")
			    ],
			    t
			  ]).
:- f_u_define_gen(u_fashionable_clothes,
		  [],
		  
		  [ 
		    [ u_gs_string_write,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(m),
				 #\(y),
				 #\(' '),
				 #\(c),
				 #\(u),
				 #\(t),
				 #\(e),
				 #\(' '),
				 #\(o),
				 #\(u),
				 #\(t),
				 #\(f),
				 #\(i),
				 #\(t)
			       ])
		    ],
		    t
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'FRIDAY-NIGHT',
			    [],
			    
			    [ 'gs-string-write',
			      stream,
			      '$STRING'(" it is friday night")
			    ],
			    t
			  ]).
:- f_u_define_gen(u_friday_night,
		  [],
		  
		  [ 
		    [ u_gs_string_write,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(i),
				 #\(t),
				 #\(' '),
				 #\(i),
				 #\(s),
				 #\(' '),
				 #\(f),
				 #\(r),
				 #\(i),
				 #\(d),
				 #\(a),
				 #\(y),
				 #\(' '),
				 #\(n),
				 #\(i),
				 #\(g),
				 #\(h),
				 #\(t)
			       ])
		    ],
		    t
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26224 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'TIME-OF-DAY',
			    [],
			    ['gs-string-write', stream, '$STRING'(" the time")],
			    t
			  ]).
:- f_u_define_gen(u_time_of_day,
		  [],
		  
		  [ 
		    [ u_gs_string_write,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(t),
				 #\(h),
				 #\(e),
				 #\(' '),
				 #\(t),
				 #\(i),
				 #\(m),
				 #\(e)
			       ])
		    ],
		    t
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26295 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ATTRACTIVE',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" cute")
			    ]
			  ]).
:- f_u_define_gen(u_attractive,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(c), #\(u), #\(t), #\(e)])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26383 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-WALK-ON-PATROL',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, walk],
			      '$STRING'(" on patrol")
			    ]
			  ]).
:- f_u_define_gen(u_m_walk_on_patrol,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_walk],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(o),
				 #\(n),
				 #\(' '),
				 #\(p),
				 #\(a),
				 #\(t),
				 #\(r),
				 #\(o),
				 #\(l)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26484 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-RETURN-TO-STATION',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, return],
			      '$STRING'(" to the station")
			    ]
			  ]).
:- f_u_define_gen(u_m_return_to_station,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, return],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(t),
				 #\(o),
				 #\(' '),
				 #\(t),
				 #\(h),
				 #\(e),
				 #\(' '),
				 #\(s),
				 #\(t),
				 #\(a),
				 #\(t),
				 #\(i),
				 #\(o),
				 #\(n)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26595 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ACADEMIC-SUCCESS',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" an academic success")
			    ]
			  ]).
:- f_u_define_gen(u_academic_success,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(n),
				 #\(' '),
				 #\(a),
				 #\(c),
				 #\(a),
				 #\(d),
				 #\(e),
				 #\(m),
				 #\(i),
				 #\(c),
				 #\(' '),
				 #\(s),
				 #\(u),
				 #\(c),
				 #\(c),
				 #\(e),
				 #\(s),
				 #\(s)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26704 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-COURSE',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, take],
			      '$STRING'(" the course")
			    ]
			  ]).
:- f_u_define_gen(u_m_course,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_take],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(t),
				 #\(h),
				 #\(e),
				 #\(' '),
				 #\(c),
				 #\(o),
				 #\(u),
				 #\(r),
				 #\(s),
				 #\(e)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26798 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-RANGER',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" a ranger")
			    ]
			  ]).
:- f_u_define_gen(u_m_ranger,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(' '),
				 #\(r),
				 #\(a),
				 #\(n),
				 #\(g),
				 #\(e),
				 #\(r)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26888 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ENTERTAINMENT',
			    [],
			    
			    [ 'gen-need-obj',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" entertained")
			    ]
			  ]).
:- f_u_define_gen(u_entertainment,
		  [],
		  
		  [ 
		    [ u_gen_need_obj,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(e),
				 #\(n),
				 #\(t),
				 #\(e),
				 #\(r),
				 #\(t),
				 #\(a),
				 #\(i),
				 #\(n),
				 #\(e),
				 #\(d)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:26987 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'MONEY',
			    [],
			    
			    [ 'gen-need-obj',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, have],
			      '$STRING'(" enough money")
			    ]
			  ]).
:- f_u_define_gen(u_money,
		  [],
		  
		  [ 
		    [ u_gen_need_obj,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_have],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(e),
				 #\(n),
				 #\(o),
				 #\(u),
				 #\(g),
				 #\(h),
				 #\(' '),
				 #\(m),
				 #\(o),
				 #\(n),
				 #\(e),
				 #\(y)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:27081 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-need-obj',
			    [con, stream, switches, context, bp, verb, str],
			    
			    [ let,
			      [[subject, [car, bp]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				verb,
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      ['gs-string-write', stream, str],
			      subject
			    ]
			  ]).

% annotating U::GEN-NEED-OBJ 
wl: lambda_def(defun,
	      u_gen_need_obj,
	      f_u_gen_need_obj,
	      [u_con, stream, u_switches, u_context, u_bp, u_verb, u_str],
	      
	      [ 
		[ let,
		  [[u_subject, [car, u_bp]]],
		  [u_gen_subject, u_subject, stream, u_switches, u_context, u_bp],
		  
		  [ u_gen_verb,
		    u_verb,
		    u_subject,
		    stream,
		    u_switches,
		    [u_neg_c63, u_con]
		  ],
		  [u_gs_string_write, stream, u_str],
		  u_subject
		]
	      ]).


% annotating U::GEN-NEED-OBJ 
wl: arglist_info(u_gen_need_obj,
		[u_con, stream, u_switches, u_context, u_bp, u_verb, u_str],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Verb_Param,
		  Str_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_str
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_verb,
				 u_str
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_str
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-NEED-OBJ 
wl: init_args(exact_only, u_gen_need_obj).


% annotating U::GEN-NEED-OBJ 
f_u_gen_need_obj(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Verb_Param, Str_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param), bv(u_verb, Verb_Param), bv(u_str, Str_Param)],
	cl_car(Bp_Param, Subject_Init),
	LEnv=[[bv(u_subject, Subject_Init)]|Env],
	get_var(LEnv, u_subject, Subject_Get),
	f_u_gen_subject(Subject_Get,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			Gen_subject_Ret),
	get_var(LEnv, u_subject, Subject_Get35),
	f_u_neg_c63(Con_Param, Neg_c63_Ret),
	f_u_gen_verb(Verb_Param,
		     Subject_Get35,
		     Stream_Param,
		     Switches_Param,
		     Neg_c63_Ret,
		     Gen_verb_Ret),
	f_u_gs_string_write(Stream_Param, Str_Param, String_write_Ret),
	get_var(LEnv, u_subject, Subject_Get41),
	LetResult=Subject_Get41,
	LetResult=FnResult.
:- set_opv(f_u_gen_need_obj, classof, claz_function),
   set_opv(u_gen_need_obj, compile_as, kw_function),
   set_opv(u_gen_need_obj, function, f_u_gen_need_obj),
   DefunResult=u_gen_need_obj.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:27327 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'POSSESSIONS',
			    [],
			    
			    [ let,
			      [[subject, [car, bp]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, keep],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				subject,
				stream,
				[cons, [quote, [case, possessive]], switches],
				context,
				bp
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" belongings")
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_possessions,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [car, u_bp]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_keep],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			u_subject,
			stream,
			[cons, [quote, [case, u_possessive]], u_switches],
			u_context,
			u_bp
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(b),
				   #\(e),
				   #\(l),
				   #\(o),
				   #\(n),
				   #\(g),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(s)
				 ])
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:27621 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*gen-thats*', []]).
:- set_var(TLEnv3, setq, u_xx_gen_thats_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:27645 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'BELIEVE',
			    [],
			    
			    [ let,
			      
			      [ [obj, ['ob$get', con, [quote, obj]]],
				[subject, ['ob$get', con, [quote, actor]]]
			      ],
			      
			      [ if,
				
				[ or,
				  
				  [ and,
				    
				    [ 'ty$instance?',
				      obj,
				      [quote, 'MENTAL-STATE']
				    ],
				    [not, ['ty$instance?', obj, [quote, 'KNOW']]],
				    
				    [ not,
				      ['ty$instance?', obj, [quote, 'BELIEVE']]
				    ]
				  ],
				  ['ty$instance?', obj, [quote, 'NOT']]
				],
				
				[ progn,
				  
				  [ if,
				    ['neg?', con],
				    
				    [ gen,
				      ['make-negative', obj],
				      stream,
				      switches,
				      context,
				      [cons, subject, bp]
				    ],
				    
				    [ gen,
				      obj,
				      stream,
				      switches,
				      context,
				      [cons, subject, bp]
				    ]
				  ],
				  
				  [ if,
				    ['ty$instance?', obj, [quote, attitude]],
				    [justify, con, stream, switches, context, bp]
				  ]
				],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ if,
				    
				    [ 'ty$instance?',
				      ['ob$get', con, [quote, obj]],
				      [quote, attractive]
				    ],
				    
				    [ 'gen-verb',
				      [quote, think],
				      subject,
				      stream,
				      switches,
				      ['neg?', con]
				    ],
				    
				    [ 'gen-verb',
				      [quote, believe],
				      subject,
				      stream,
				      switches,
				      ['neg?', con]
				    ]
				  ],
				  
				  [ if,
				    '*gen-thats*',
				    
				    [ 'gs-string-write',
				      stream,
				      '$STRING'(" that")
				    ]
				  ],
				  
				  [ gen,
				    ['ob$get', con, [quote, obj]],
				    stream,
				    ['simplify-tense', switches],
				    context,
				    [cdr, bp]
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_believe,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]],
			[u_subject, [u_ob_c36_get, u_con, [quote, u_actor]]]
		      ],
		      
		      [ if,
			
			[ or,
			  
			  [ and,
			    
			    [ u_ty_c36_instance_c63,
			      u_obj,
			      [quote, u_mental_state]
			    ],
			    [not, [u_ty_c36_instance_c63, u_obj, [quote, u_know]]],
			    
			    [ not,
			      [u_ty_c36_instance_c63, u_obj, [quote, u_believe]]
			    ]
			  ],
			  [u_ty_c36_instance_c63, u_obj, [quote, not]]
			],
			
			[ progn,
			  
			  [ if,
			    [u_neg_c63, u_con],
			    
			    [ u_gen,
			      [u_make_negative, u_obj],
			      stream,
			      u_switches,
			      u_context,
			      [cons, u_subject, u_bp]
			    ],
			    
			    [ u_gen,
			      u_obj,
			      stream,
			      u_switches,
			      u_context,
			      [cons, u_subject, u_bp]
			    ]
			  ],
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_attitude]],
			    [u_justify, u_con, stream, u_switches, u_context, u_bp]
			  ]
			],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ if,
			    
			    [ u_ty_c36_instance_c63,
			      [u_ob_c36_get, u_con, [quote, u_obj]],
			      [quote, u_attractive]
			    ],
			    
			    [ u_gen_verb,
			      [quote, u_think],
			      u_subject,
			      stream,
			      u_switches,
			      [u_neg_c63, u_con]
			    ],
			    
			    [ u_gen_verb,
			      [quote, u_believe],
			      u_subject,
			      stream,
			      u_switches,
			      [u_neg_c63, u_con]
			    ]
			  ],
			  
			  [ if,
			    u_xx_gen_thats_xx,
			    
			    [ u_gs_string_write,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(' '), #\(t), #\(h), #\(a), #\(t)])
			    ]
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_obj]],
			    stream,
			    [u_simplify_tense, u_switches],
			    u_context,
			    [cdr, u_bp]
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:27645 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" this is not currently used.", 38, 27928)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:27645 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Must propagate negation.", 25, 28007)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:28735 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'MOVIE',
			    [],
			    ['gs-string-write', stream, '$STRING'(" a movie")],
			    t
			  ]).
:- f_u_define_gen(u_movie,
		  [],
		  
		  [ 
		    [ u_gs_string_write,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(' '),
				 #\(m),
				 #\(o),
				 #\(v),
				 #\(i),
				 #\(e)
			       ])
		    ],
		    t
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:28801 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'LOVERS',
			    [],
			    
			    [ 'gen-split-subject',
			      con,
			      stream,
			      [progressivize, switches],
			      context,
			      bp,
			      [quote, go],
			      '$STRING'(" out with")
			    ]
			  ]).
:- f_u_define_gen(u_lovers,
		  [],
		  
		  [ 
		    [ u_gen_split_subject,
		      u_con,
		      stream,
		      [u_progressivize, u_switches],
		      u_context,
		      u_bp,
		      [quote, go],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(o),
				 #\(u),
				 #\(t),
				 #\(' '),
				 #\(w),
				 #\(i),
				 #\(t),
				 #\(h)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:28932 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ACQUAINTED',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" acquainted")
			    ]
			  ]).
:- f_u_define_gen(u_acquainted,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(c),
				 #\(q),
				 #\(u),
				 #\(a),
				 #\(i),
				 #\(n),
				 #\(t),
				 #\(e),
				 #\(d)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:29026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-PHONE',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, call],
			      [],
			      ['ob$get', con, [quote, to]]
			    ]
			  ]).
:- f_u_define_gen(u_m_phone,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_call],
		      [],
		      [u_ob_c36_get, u_con, [quote, u_to]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:29151 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-PUTON',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, put],
			      '$STRING'(" on"),
			      ['ob$get', con, [quote, obj]]
			    ]
			  ]).
:- f_u_define_gen(u_m_puton,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, sys_put],
		      '$ARRAY'([*], claz_base_character, [#\(' '), #\(o), #\(n)]),
		      [u_ob_c36_get, u_con, [quote, u_obj]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:29278 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-LOGIN',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, log],
			      '$STRING'(" into"),
			      ['ob$get', con, [quote, obj]]
			    ]
			  ]).
:- f_u_define_gen(u_m_login,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, log],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(i), #\(n), #\(t), #\(o)]),
		      [u_ob_c36_get, u_con, [quote, u_obj]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:29407 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-MOVIE',
			    [],
			    
			    [ if,
			      
			      [ 'switches-lookup',
				[quote, 'active-goal?'],
				switches
			      ],
			      
			      [ 'gen-regular',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, go],
				'$STRING'(" see a movie")
			      ],
			      
			      [ 'gen-regular',
				con,
				stream,
				
				[ cons,
				  [quote, [tense, 'present-perfect']],
				  switches
				],
				context,
				bp,
				[quote, go],
				'$STRING'(" to see a movie")
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_m_movie,
		  [],
		  
		  [ 
		    [ if,
		      [u_switches_lookup, [quote, u_active_goal_c63], u_switches],
		      
		      [ u_gen_regular,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, go],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(s),
				   #\(e),
				   #\(e),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(m),
				   #\(o),
				   #\(v),
				   #\(i),
				   #\(e)
				 ])
		      ],
		      
		      [ u_gen_regular,
			u_con,
			stream,
			[cons, [quote, [u_tense, u_present_perfect]], u_switches],
			u_context,
			u_bp,
			[quote, go],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(t),
				   #\(o),
				   #\(' '),
				   #\(s),
				   #\(e),
				   #\(e),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(m),
				   #\(o),
				   #\(v),
				   #\(i),
				   #\(e)
				 ])
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:29675 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'MTRANS-ACCEPTABLE',
			    [],
			    
			    [ if,
			      
			      [ 'switches-lookup',
				[quote, 'active-goal?'],
				switches
			      ],
			      
			      [ 'gen-split-subject',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, break],
				'$STRING'(" the ice with")
			      ],
			      
			      [ 'gen-split-subject',
				con,
				stream,
				
				[ cons,
				  [quote, [tense, 'present-perfect']],
				  switches
				],
				context,
				bp,
				[quote, break],
				'$STRING'(" the ice with")
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_mtrans_acceptable,
		  [],
		  
		  [ 
		    [ if,
		      [u_switches_lookup, [quote, u_active_goal_c63], u_switches],
		      
		      [ u_gen_split_subject,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, break],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(t),
				   #\(h),
				   #\(e),
				   #\(' '),
				   #\(i),
				   #\(c),
				   #\(e),
				   #\(' '),
				   #\(w),
				   #\(i),
				   #\(t),
				   #\(h)
				 ])
		      ],
		      
		      [ u_gen_split_subject,
			u_con,
			stream,
			[cons, [quote, [u_tense, u_present_perfect]], u_switches],
			u_context,
			u_bp,
			[quote, break],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(t),
				   #\(h),
				   #\(e),
				   #\(' '),
				   #\(i),
				   #\(c),
				   #\(e),
				   #\(' '),
				   #\(w),
				   #\(i),
				   #\(t),
				   #\(h)
				 ])
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:29976 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-AGREE',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, agree],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], [quote, 'no-subject']],
				  [cons, [quote, [tense, infinitive]], switches]
				],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_m_agree,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_agree],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], [quote, u_no_subject]],
			  [cons, [quote, [u_tense, u_infinitive]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:30307 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-RESTAURANT',
			    [],
			    
			    [ let,
			      
			      [ 
				[ subject,
				  
				  [ 'gen-split-subject',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    [quote, have],
				    '$STRING'(" dinner with")
				  ]
				]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" at a restaurant")
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_m_restaurant,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ 
			[ u_subject,
			  
			  [ u_gen_split_subject,
			    u_con,
			    stream,
			    u_switches,
			    u_context,
			    u_bp,
			    [quote, u_have],
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(d),
				       #\(i),
				       #\(n),
				       #\(n),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h)
				     ])
			  ]
			]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(t),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(r),
				   #\(e),
				   #\(s),
				   #\(t),
				   #\(a),
				   #\(u),
				   #\(r),
				   #\(a),
				   #\(n),
				   #\(t)
				 ])
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:30532 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-CONVERSATION',
			    [],
			    
			    [ 'gen-split-subject',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, have],
			      '$STRING'(" a conversation with")
			    ]
			  ]).
:- f_u_define_gen(u_m_conversation,
		  [],
		  
		  [ 
		    [ u_gen_split_subject,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_have],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(' '),
				 #\(c),
				 #\(o),
				 #\(n),
				 #\(v),
				 #\(e),
				 #\(r),
				 #\(s),
				 #\(a),
				 #\(t),
				 #\(i),
				 #\(o),
				 #\(n),
				 #\(' '),
				 #\(w),
				 #\(i),
				 #\(t),
				 #\(h)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:30668 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-DATE',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, go],
			      '$STRING'(" out on a date")
			    ]
			  ]).
:- f_u_define_gen(u_m_date,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, go],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(o),
				 #\(u),
				 #\(t),
				 #\(' '),
				 #\(o),
				 #\(n),
				 #\(' '),
				 #\(a),
				 #\(' '),
				 #\(d),
				 #\(a),
				 #\(t),
				 #\(e)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:30761 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-STUDY',
			    [],
			    
			    [ if,
			      
			      [ 'ty$instance?',
				['ob$get', con, [quote, obj]],
				[quote, actor]
			      ],
			      
			      [ 'gen-regular',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, study],
				'$STRING'(" to be an actor")
			      ],
			      
			      [ 'gen-regular-plus',
				con,
				stream,
				switches,
				context,
				bp,
				[quote, study],
				'$STRING'(" to be"),
				['ob$get', con, [quote, obj]]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_m_study,
		  [],
		  
		  [ 
		    [ if,
		      
		      [ u_ty_c36_instance_c63,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			[quote, u_actor]
		      ],
		      
		      [ u_gen_regular,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, u_study],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(t),
				   #\(o),
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
				   #\(o),
				   #\(r)
				 ])
		      ],
		      
		      [ u_gen_regular_plus,
			u_con,
			stream,
			u_switches,
			u_context,
			u_bp,
			[quote, u_study],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(t), #\(o), #\(' '), #\(b), #\(e)]),
			[u_ob_c36_get, u_con, [quote, u_obj]]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31024 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-BEAT-UP',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, beat],
			      '$STRING'(" up"),
			      ['ob$get', con, [quote, obj]]
			    ]
			  ]).
:- f_u_define_gen(u_m_beat_up,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_beat],
		      '$ARRAY'([*], claz_base_character, [#\(' '), #\(u), #\(p)]),
		      [u_ob_c36_get, u_con, [quote, u_obj]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31154 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'SELLS',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, sell],
			      [],
			      ['ob$get', con, [quote, obj]]
			    ]
			  ]).
:- f_u_define_gen(u_sells,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_sell],
		      [],
		      [u_ob_c36_get, u_con, [quote, u_obj]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31154 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 554.6,8 (Roget's Fourth Edition)",
				     1,
				     31279)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31154 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 13.4 (Roget's Fourth Edition) \"each other\"",
				     1,
				     31314)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31358 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ENABLE-FUTURE-VPROX',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" able to contact each other")
			    ]
			  ]).
:- f_u_define_gen(u_enable_future_vprox,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(b),
				 #\(l),
				 #\(e),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' '),
				 #\(c),
				 #\(o),
				 #\(n),
				 #\(t),
				 #\(a),
				 #\(c),
				 #\(t),
				 #\(' '),
				 #\(e),
				 #\(a),
				 #\(c),
				 #\(h),
				 #\(' '),
				 #\(o),
				 #\(t),
				 #\(h),
				 #\(e),
				 #\(r)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31358 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" also one another", 48, 31492)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31511 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-KISS',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, kiss],
			      []
			    ]
			  ]).
:- f_u_define_gen(u_m_kiss,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_kiss],
		      []
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31593 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-BREAK-UP',
			    [],
			    
			    [ let,
			      
			      [ 
				[ subject,
				  
				  [ 'gen-regular',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    [quote, break],
				    '$STRING'(" up with")
				  ]
				]
			      ],
			      
			      [ if,
				
				[ 'ty$instance?',
				  [car, subject],
				  [quote, 'male-person']
				],
				
				[ 'gs-string-write',
				  stream,
				  '$STRING'(" his girlfriend")
				],
				
				[ 'gs-string-write',
				  stream,
				  '$STRING'(" her boyfriend")
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_m_break_up,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ 
			[ u_subject,
			  
			  [ u_gen_regular,
			    u_con,
			    stream,
			    u_switches,
			    u_context,
			    u_bp,
			    [quote, break],
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(u),
				       #\(p),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h)
				     ])
			  ]
			]
		      ],
		      
		      [ if,
			
			[ u_ty_c36_instance_c63,
			  [car, u_subject],
			  [quote, u_male_person]
			],
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(' '),
				     #\(h),
				     #\(i),
				     #\(s),
				     #\(' '),
				     #\(g),
				     #\(i),
				     #\(r),
				     #\(l),
				     #\(f),
				     #\(r),
				     #\(i),
				     #\(e),
				     #\(n),
				     #\(d)
				   ])
			],
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(' '),
				     #\(h),
				     #\(e),
				     #\(r),
				     #\(' '),
				     #\(b),
				     #\(o),
				     #\(y),
				     #\(f),
				     #\(r),
				     #\(i),
				     #\(e),
				     #\(n),
				     #\(d)
				   ])
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:31890 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'DATING-SERVICE-MEMBER',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" a member of the dating service")
			    ]
			  ]).
:- f_u_define_gen(u_dating_service_member,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(a),
				 #\(' '),
				 #\(m),
				 #\(e),
				 #\(m),
				 #\(b),
				 #\(e),
				 #\(r),
				 #\(' '),
				 #\(o),
				 #\(f),
				 #\(' '),
				 #\(t),
				 #\(h),
				 #\(e),
				 #\(' '),
				 #\(d),
				 #\(a),
				 #\(t),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\(' '),
				 #\(s),
				 #\(e),
				 #\(r),
				 #\(v),
				 #\(i),
				 #\(c),
				 #\(e)
			       ])
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32030 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-WORK',
			    [],
			    
			    [ 'gen-regular',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, work],
			      []
			    ]
			  ]).
:- f_u_define_gen(u_m_work,
		  [],
		  
		  [ 
		    [ u_gen_regular,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_work],
		      []
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32030 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Maybe add past subjunctive and conditional here.",
				     1,
				     32113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'LEADTO',
			    [],
			    
			    [ let,
			      
			      [ [ante, ['ob$get', con, [quote, ante]]],
				[conseq, ['ob$get', con, [quote, conseq]]],
				[subject, []]
			      ],
			      
			      [ setq,
				subject,
				
				[ gen,
				  ante,
				  stream,
				  
				  [ cons,
				    [quote, ['possessive-subj', t]],
				    [cons, [quote, [tense, gerund]], switches]
				  ],
				  context,
				  bp
				]
			      ],
			      
			      [ 'gen-verb',
				[quote, lead],
				ante,
				stream,
				switches,
				['neg?', con]
			      ],
			      ['gs-string-write', stream, '$STRING'(" to")],
			      
			      [ gen,
				conseq,
				stream,
				[cons, [quote, [tense, gerund]], switches],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_leadto,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_ante, [u_ob_c36_get, u_con, [quote, u_ante]]],
			[u_conseq, [u_ob_c36_get, u_con, [quote, u_conseq]]],
			[u_subject, []]
		      ],
		      
		      [ setq,
			u_subject,
			
			[ u_gen,
			  u_ante,
			  stream,
			  
			  [ cons,
			    [quote, [u_possessive_subj, t]],
			    [cons, [quote, [u_tense, u_gerund]], u_switches]
			  ],
			  u_context,
			  u_bp
			]
		      ],
		      
		      [ u_gen_verb,
			[quote, u_lead],
			u_ante,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(t), #\(o)])
		      ],
		      
		      [ u_gen,
			u_conseq,
			stream,
			[cons, [quote, [u_tense, u_gerund]], u_switches],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun gen-pu-mixed-blessing (con stream switches)",
				     1,
				     32639)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gen (ob$get con 'plus)", 1, 32691)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("       stream", 1, 32718)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (cons (list 's-bar (ob$gets con 'char1))",
				     1,
				     32733)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (cons '(tense gerund) switches)))",
				     1,
				     32782)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gs-string-write stream \" leads to\")",
				     1,
				     32830)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gen (ob$get con 'minus) stream",
				     1,
				     32870)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (cons (list 's-bar (ob$gets con 'char1))",
				     1,
				     32905)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (cons '(tense gerund) switches))))",
				     1,
				     32954)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun gen-pu-denied-request (con stream switches)",
				     1,
				     33004)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gen (ob$get con 'action) stream switches))",
				     1,
				     33056)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun gen-pu-retaliation (con stream switches)",
				     1,
				     33104)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gen-subject (ob$gets con 'char1) stream switches)",
				     1,
				     33153)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gen-verb 'get (list (ob$gets con 'char1))",
				     1,
				     33207)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 stream switches (neg? con))",
				     1,
				     33253)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gs-string-write stream \" back at\")",
				     1,
				     33299)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gen (ob$get con 'char2) stream switches)",
				     1,
				     33338)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gs-string-write stream \" by\")",
				     1,
				     33383)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gen (ob$get con 'action) stream",
				     1,
				     33417)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (cons (list 's-bar (ob$gets con 'char1))",
				     1,
				     33453)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:32163 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (cons '(tense gerund) switches))))",
				     1,
				     33502)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:33551 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'RATIONALIZATION',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, rationalize],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], subject],
				  [cons, [quote, [tense, gerund]], switches]
				],
				context,
				bp
			      ],
			      
			      [ if,
				['ob$get', con, [quote, reason]],
				
				[ progn,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" by the fact that")
				  ],
				  
				  [ gen,
				    ['ob$get', con, [quote, reason]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_rationalization,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, rationalize],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], u_subject],
			  [cons, [quote, [u_tense, u_gerund]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      
		      [ if,
			[u_ob_c36_get, u_con, [quote, u_reason]],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(b),
				       #\(y),
				       #\(' '),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(' '),
				       #\(f),
				       #\(a),
				       #\(c),
				       #\(t),
				       #\(' '),
				       #\(t),
				       #\(h),
				       #\(a),
				       #\(t)
				     ])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_reason]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:34034 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'RECOVERY',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, recover],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      ['gs-string-write', stream, '$STRING'(" from")],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], subject],
				  [cons, [quote, [tense, gerund]], switches]
				],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_recovery,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_recover],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(f), #\(r), #\(o), #\(m)])
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], u_subject],
			  [cons, [quote, [u_tense, u_gerund]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:34384 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'REPERCUSSIONS',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, consider],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], subject],
				  [cons, [quote, [tense, gerund]], switches]
				],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_repercussions,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_consider],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], u_subject],
			  [cons, [quote, [u_tense, u_gerund]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:34704 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'HYPOTHESIZE',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, imagine],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, state]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], subject],
				  [cons, [quote, [tense, gerund]], switches]
				],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_hypothesize,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_imagine],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_state]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], u_subject],
			  [cons, [quote, [u_tense, u_gerund]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:35023 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'LIST',
			    [],
			    
			    [ gen,
			      ['ob$get', con, [quote, first]],
			      stream,
			      switches,
			      context,
			      bp
			    ],
			    
			    [ if,
			      
			      [ not,
				
				[ 'ty$instance?',
				  ['ob$get', con, [quote, rest]],
				  [quote, list]
				]
			      ],
			      ['gs-string-write', stream, '$STRING'(", and")],
			      ['gs-string-write', stream, '$STRING'(",")]
			    ],
			    
			    [ gen,
			      ['ob$get', con, [quote, rest]],
			      stream,
			      switches,
			      context,
			      bp
			    ],
			    t
			  ]).
:- f_u_define_gen(list,
		  [],
		  
		  [ 
		    [ u_gen,
		      [u_ob_c36_get, u_con, [quote, first]],
		      stream,
		      u_switches,
		      u_context,
		      u_bp
		    ],
		    
		    [ if,
		      
		      [ not,
			
			[ u_ty_c36_instance_c63,
			  [u_ob_c36_get, u_con, [quote, rest]],
			  [quote, list]
			]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(','), #\(' '), #\(a), #\(n), #\(d)])
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*], claz_base_character, [#\(',')])
		      ]
		    ],
		    
		    [ u_gen,
		      [u_ob_c36_get, u_con, [quote, rest]],
		      stream,
		      u_switches,
		      u_context,
		      u_bp
		    ],
		    t
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:35286 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'REVERSAL',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, reverse],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], subject],
				  [cons, [quote, [tense, gerund]], switches]
				],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_reversal,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, reverse],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], u_subject],
			  [cons, [quote, [u_tense, u_gerund]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:35600 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'UNDO-CAUSES',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, undo],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], subject],
				  [cons, [quote, [tense, gerund]], switches]
				],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_undo_causes,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_undo],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], u_subject],
			  [cons, [quote, [u_tense, u_gerund]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:35914 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ROVING',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, think],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" about something else")
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_roving,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_think],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(b),
				   #\(o),
				   #\(u),
				   #\(t),
				   #\(' '),
				   #\(s),
				   #\(o),
				   #\(m),
				   #\(e),
				   #\(t),
				   #\(h),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(e),
				   #\(l),
				   #\(s),
				   #\(e)
				 ])
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'REVENGE',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, get],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" even with")
			      ],
			      
			      [ if,
				['ob$gets', con, [quote, to]],
				
				[ gen,
				  ['ob$gets', con, [quote, to]],
				  stream,
				  switches,
				  context,
				  bp
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_revenge,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, get],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(e),
				   #\(v),
				   #\(e),
				   #\(n),
				   #\(' '),
				   #\(w),
				   #\(i),
				   #\(t),
				   #\(h)
				 ])
		      ],
		      
		      [ if,
			[u_ob_c36_gets, u_con, [quote, u_to]],
			
			[ u_gen,
			  [u_ob_c36_gets, u_con, [quote, u_to]],
			  stream,
			  u_switches,
			  u_context,
			  u_bp
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 956.4 (Roget's Fourth Edition)",
				     4,
				     36209)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   Could use get-action-causes to generate the below.",
				     1,
				     36473)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   (gs-string-write stream \" for\")",
				     1,
				     36528)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   (gen (ob$get con 'obj)", 1, 36564)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        stream", 1, 36591)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (cons (list 's-bar subject) (cons '(tense gerund) switches))",
				     1,
				     36607)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36146 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        context bp)", 1, 36677)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36711 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ADVANTAGE',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, be],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" in a position of advantage over")
			      ],
			      
			      [ if,
				['ob$gets', con, [quote, obj]],
				
				[ gen,
				  ['ob$gets', con, [quote, obj]],
				  stream,
				  switches,
				  context,
				  bp
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_advantage,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_be],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(i),
				   #\(n),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(p),
				   #\(o),
				   #\(s),
				   #\(i),
				   #\(t),
				   #\(i),
				   #\(o),
				   #\(n),
				   #\(' '),
				   #\(o),
				   #\(f),
				   #\(' '),
				   #\(a),
				   #\(d),
				   #\(v),
				   #\(a),
				   #\(n),
				   #\(t),
				   #\(a),
				   #\(g),
				   #\(e),
				   #\(' '),
				   #\(o),
				   #\(v),
				   #\(e),
				   #\(r)
				 ])
		      ],
		      
		      [ if,
			[u_ob_c36_gets, u_con, [quote, u_obj]],
			
			[ u_gen,
			  [u_ob_c36_gets, u_con, [quote, u_obj]],
			  stream,
			  u_switches,
			  u_context,
			  u_bp
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:36711 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 956.4 (Roget's Fourth Edition)",
				     4,
				     36776)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:37076 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'GROUP',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ if,
				['ob$gets', con, [quote, nonmembers]],
				
				[ progn,
				  
				  [ 'gen-verb',
				    [quote, exclude],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ gen,
				    ['ob$gets', con, [quote, nonmembers]],
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" from the group")
				  ]
				],
				
				[ progn,
				  
				  [ 'gen-verb',
				    [quote, be],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" in a group")
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_group,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ if,
			[u_ob_c36_gets, u_con, [quote, u_nonmembers]],
			
			[ progn,
			  
			  [ u_gen_verb,
			    [quote, u_exclude],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_gets, u_con, [quote, u_nonmembers]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(f),
				       #\(r),
				       #\(o),
				       #\(m),
				       #\(' '),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(' '),
				       #\(g),
				       #\(r),
				       #\(o),
				       #\(u),
				       #\(p)
				     ])
			  ]
			],
			
			[ progn,
			  
			  [ u_gen_verb,
			    [quote, u_be],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(a),
				       #\(' '),
				       #\(g),
				       #\(r),
				       #\(o),
				       #\(u),
				       #\(p)
				     ])
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:37549 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'WELL-PREPARED',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, be],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" well prepared")
			      ],
			      
			      [ if,
				
				[ and,
				  ['ob$gets', con, [quote, to]],
				  [not, ['var?', ['ob$get', con, [quote, to]]]]
				],
				
				[ progn,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" in front of")
				  ],
				  
				  [ gen,
				    ['ob$gets', con, [quote, to]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				],
				
				[ progn,
				  ['gs-string-write', stream, '$STRING'(" for")],
				  
				  [ gen,
				    ['ob$get', con, [quote, obj]],
				    stream,
				    
				    [ cons,
				      [list, [quote, 's-bar'], subject],
				      [cons, [quote, [tense, gerund]], switches]
				    ],
				    context,
				    bp
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_well_prepared,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_be],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(w),
				   #\(e),
				   #\(l),
				   #\(l),
				   #\(' '),
				   #\(p),
				   #\(r),
				   #\(e),
				   #\(p),
				   #\(a),
				   #\(r),
				   #\(e),
				   #\(d)
				 ])
		      ],
		      
		      [ if,
			
			[ and,
			  [u_ob_c36_gets, u_con, [quote, u_to]],
			  [not, [u_var_c63, [u_ob_c36_get, u_con, [quote, u_to]]]]
			],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(f),
				       #\(r),
				       #\(o),
				       #\(n),
				       #\(t),
				       #\(' '),
				       #\(o),
				       #\(f)
				     ])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_gets, u_con, [quote, u_to]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(f), #\(o), #\(r)])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_obj]],
			    stream,
			    
			    [ cons,
			      [list, [quote, u_s_bar], u_subject],
			      [cons, [quote, [u_tense, u_gerund]], u_switches]
			    ],
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:37549 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 956.4 (Roget's Fourth Edition)",
				     4,
				     37618)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:38219 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'WELL-PREPARED-FOR',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, be],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" well prepared")
			      ],
			      
			      [ if,
				
				[ and,
				  ['ob$gets', con, [quote, to]],
				  [not, ['var?', ['ob$get', con, [quote, to]]]]
				],
				
				[ progn,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" in front of")
				  ],
				  
				  [ gen,
				    ['ob$gets', con, [quote, to]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				],
				
				[ progn,
				  ['gs-string-write', stream, '$STRING'(" for")],
				  
				  [ gen,
				    ['ob$get', con, [quote, obj]],
				    stream,
				    
				    [ cons,
				      [list, [quote, 's-bar'], subject],
				      [cons, [quote, [tense, gerund]], switches]
				    ],
				    context,
				    bp
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_well_prepared_for,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_be],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(w),
				   #\(e),
				   #\(l),
				   #\(l),
				   #\(' '),
				   #\(p),
				   #\(r),
				   #\(e),
				   #\(p),
				   #\(a),
				   #\(r),
				   #\(e),
				   #\(d)
				 ])
		      ],
		      
		      [ if,
			
			[ and,
			  [u_ob_c36_gets, u_con, [quote, u_to]],
			  [not, [u_var_c63, [u_ob_c36_get, u_con, [quote, u_to]]]]
			],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(f),
				       #\(r),
				       #\(o),
				       #\(n),
				       #\(t),
				       #\(' '),
				       #\(o),
				       #\(f)
				     ])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_gets, u_con, [quote, u_to]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(f), #\(o), #\(r)])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_obj]],
			    stream,
			    
			    [ cons,
			      [list, [quote, u_s_bar], u_subject],
			      [cons, [quote, [u_tense, u_gerund]], u_switches]
			    ],
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:38219 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 956.4 (Roget's Fourth Edition)",
				     4,
				     38292)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:38893 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-GET-READY',
			    [],
			    
			    [ let,
			      [[subject, [list, [car, bp]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, get],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" ready for")
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				
				[ cons,
				  [list, [quote, 's-bar'], subject],
				  [cons, [quote, [tense, gerund]], switches]
				],
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_m_get_ready,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [list, [car, u_bp]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, get],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(r),
				   #\(e),
				   #\(a),
				   #\(d),
				   #\(y),
				   #\(' '),
				   #\(f),
				   #\(o),
				   #\(r)
				 ])
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			
			[ cons,
			  [list, [quote, u_s_bar], u_subject],
			  [cons, [quote, [u_tense, u_gerund]], u_switches]
			],
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:38893 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 956.4 (Roget's Fourth Edition)",
				     4,
				     38960)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:39283 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'PERSON',
			    [],
			    
			    [ cond,
			      
			      [ ['eq?', con, '*me-ob*'],
				
				[ case,
				  ['switches-lookup', [quote, case], switches],
				  
				  [ [nominative],
				    ['gs-string-write', stream, '$STRING'(" I")]
				  ],
				  
				  [ [possessive],
				    
				    [ 'gs-string-write',
				      stream,
				      '$STRING'(" my")
				    ]
				  ],
				  
				  [ [reflexive],
				    
				    [ 'gs-string-write',
				      stream,
				      '$STRING'(" myself")
				    ]
				  ],
				  
				  [ otherwise,
				    
				    [ 'gs-string-write',
				      stream,
				      '$STRING'(" me")
				    ]
				  ]
				]
			      ],
			      
			      [ ['memq?', con, '*references*'],
				
				[ if,
				  ['exemplar?', con],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" this person")
				  ],
				  
				  [ progn,
				    
				    [ case,
				      
				      [ 'switches-lookup',
					[quote, case],
					switches
				      ],
				      
				      [ [nominative],
					
					[ if,
					  
					  [ 'ty$instance?',
					    con,
					    [quote, 'male-person']
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" he")
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" she")
					  ]
					]
				      ],
				      
				      [ [possessive],
					
					[ if,
					  
					  [ 'ty$instance?',
					    con,
					    [quote, 'male-person']
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" his")
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" her")
					  ]
					]
				      ],
				      
				      [ [reflexive],
					
					[ if,
					  
					  [ 'ty$instance?',
					    con,
					    [quote, 'male-person']
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" himself")
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" herself")
					  ]
					]
				      ],
				      
				      [ otherwise,
					
					[ if,
					  
					  [ 'ty$instance?',
					    con,
					    [quote, 'male-person']
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" him")
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" her")
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ else,
				
				[ let,
				  
				  [ 
				    [ name,
				      
				      [ or,
					
					[ and,
					  ['ob$get', con, [quote, 'first-name']],
					  ['ob$get', con, [quote, 'last-name']],
					  
					  [ 'null?',
					    ['pos-ipt-with?', con, context]
					  ],
					  
					  [ 'string-append',
					    
					    [ 'ob$get',
					      con,
					      [quote, 'first-name']
					    ],
					    '$STRING'(" "),
					    ['ob$get', con, [quote, 'last-name']]
					  ]
					],
					['ob$get', con, [quote, 'first-name']],
					['ob$get', con, [quote, 'last-name']],
					'$STRING'("someone")
				      ]
				    ]
				  ],
				  
				  [ case,
				    ['switches-lookup', [quote, case], switches],
				    
				    [ [possessive],
				      
				      [ 'gs-string-write',
					stream,
					
					[ 'string-append',
					  '$STRING'(" "),
					  name,
					  '$STRING'("'s")
					]
				      ]
				    ],
				    
				    [ otherwise,
				      
				      [ 'gs-string-write',
					stream,
					['string-append', '$STRING'(" "), name]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    t
			  ]).
:- f_u_define_gen(u_person,
		  [],
		  
		  [ 
		    [ cond,
		      
		      [ [u_eq_c63, u_con, u_xx_me_ob_xx],
			
			[ case,
			  [u_switches_lookup, [quote, case], u_switches],
			  
			  [ [u_nominative],
			    
			    [ u_gs_string_write,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(' '), #\('I')])
			    ]
			  ],
			  
			  [ [u_possessive],
			    
			    [ u_gs_string_write,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(' '), #\(m), #\(y)])
			    ]
			  ],
			  
			  [ [u_reflexive],
			    
			    [ u_gs_string_write,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(' '),
					 #\(m),
					 #\(y),
					 #\(s),
					 #\(e),
					 #\(l),
					 #\(f)
				       ])
			    ]
			  ],
			  
			  [ otherwise,
			    
			    [ u_gs_string_write,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(' '), #\(m), #\(e)])
			    ]
			  ]
			]
		      ],
		      
		      [ [u_memq_c63, u_con, u_xx_references_xx],
			
			[ if,
			  [u_exemplar_c63, u_con],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(t),
				       #\(h),
				       #\(i),
				       #\(s),
				       #\(' '),
				       #\(p),
				       #\(e),
				       #\(r),
				       #\(s),
				       #\(o),
				       #\(n)
				     ])
			  ],
			  
			  [ progn,
			    
			    [ case,
			      [u_switches_lookup, [quote, case], u_switches],
			      
			      [ [u_nominative],
				
				[ if,
				  
				  [ u_ty_c36_instance_c63,
				    u_con,
				    [quote, u_male_person]
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(h), #\(e)])
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(s), #\(h), #\(e)])
				  ]
				]
			      ],
			      
			      [ [u_possessive],
				
				[ if,
				  
				  [ u_ty_c36_instance_c63,
				    u_con,
				    [quote, u_male_person]
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(h), #\(i), #\(s)])
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(h), #\(e), #\(r)])
				  ]
				]
			      ],
			      
			      [ [u_reflexive],
				
				[ if,
				  
				  [ u_ty_c36_instance_c63,
				    u_con,
				    [quote, u_male_person]
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(' '),
					       #\(h),
					       #\(i),
					       #\(m),
					       #\(s),
					       #\(e),
					       #\(l),
					       #\(f)
					     ])
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(' '),
					       #\(h),
					       #\(e),
					       #\(r),
					       #\(s),
					       #\(e),
					       #\(l),
					       #\(f)
					     ])
				  ]
				]
			      ],
			      
			      [ otherwise,
				
				[ if,
				  
				  [ u_ty_c36_instance_c63,
				    u_con,
				    [quote, u_male_person]
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(h), #\(i), #\(m)])
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(h), #\(e), #\(r)])
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ u_else,
			
			[ let,
			  
			  [ 
			    [ sys_name,
			      
			      [ or,
				
				[ and,
				  [u_ob_c36_get, u_con, [quote, u_first_name]],
				  [u_ob_c36_get, u_con, [quote, u_last_name]],
				  
				  [ u_null_c63,
				    [u_pos_ipt_with_c63, u_con, u_context]
				  ],
				  
				  [ u_string_append,
				    [u_ob_c36_get, u_con, [quote, u_first_name]],
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' ')]),
				    [u_ob_c36_get, u_con, [quote, u_last_name]]
				  ]
				],
				[u_ob_c36_get, u_con, [quote, u_first_name]],
				[u_ob_c36_get, u_con, [quote, u_last_name]],
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(s),
					   #\(o),
					   #\(m),
					   #\(e),
					   #\(o),
					   #\(n),
					   #\(e)
					 ])
			      ]
			    ]
			  ],
			  
			  [ case,
			    [u_switches_lookup, [quote, case], u_switches],
			    
			    [ [u_possessive],
			      
			      [ u_gs_string_write,
				stream,
				
				[ u_string_append,
				  '$ARRAY'([*], claz_base_character, [#\(' ')]),
				  sys_name,
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\('\''), #\(s)])
				]
			      ]
			    ],
			    
			    [ otherwise,
			      
			      [ u_gs_string_write,
				stream,
				
				[ u_string_append,
				  '$ARRAY'([*], claz_base_character, [#\(' ')]),
				  sys_name
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    t
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:39283 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Use first names for people with whom you have a positive ipt",
				     1,
				     40577)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:39283 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (real or imagined) in the current context.",
				     1,
				     40640)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41253 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'pos-ipt-with?',
			    [pers, context],
			    
			    [ 'cx$retrieve',
			      context,
			      
			      [ 'ob$fcreate',
				
				[ '#BQ',
				  
				  [ 'UAND',
				    obj,
				    Pos_Relationship,
				    obj,
				    
				    [ 'NOTYPE',
				      actor,
				      'Me',
				      actor,
				      ['#COMMA', pers]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::POS-IPT-WITH? 
wl: lambda_def(defun,
	      u_pos_ipt_with_c63,
	      f_u_pos_ipt_with_c63,
	      [u_pers, u_context],
	      
	      [ 
		[ u_cx_c36_retrieve,
		  u_context,
		  
		  [ u_ob_c36_fcreate,
		    
		    [ '#BQ',
		      
		      [ u_uand,
			u_obj,
			'$VAR'(u_pos_relationship),
			u_obj,
			[u_notype, u_actor, u_me, u_actor, ['#COMMA', u_pers]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::POS-IPT-WITH? 
wl: arglist_info(u_pos_ipt_with_c63,
		[u_pers, u_context],
		[Pers_Param, Context_Param],
		arginfo{ all:[u_pers, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_pers, u_context],
			 opt:0,
			 req:[u_pers, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::POS-IPT-WITH? 
wl: init_args(exact_only, u_pos_ipt_with_c63).


% annotating U::POS-IPT-WITH? 
f_u_pos_ipt_with_c63(Pers_Param, Context_Param, FnResult) :-
	Env=[bv(u_pers, Pers_Param), bv(u_context, Context_Param)],
	f_u_ob_c36_fcreate(
			   [ '#BQ',
			     
			     [ u_uand,
			       u_obj,
			       '$VAR'(u_pos_relationship),
			       u_obj,
			       
			       [ u_notype,
				 u_actor,
				 u_me,
				 u_actor,
				 ['#COMMA', u_pers]
			       ]
			     ]
			   ],
			   C36_fcreate_Ret),
	f_u_cx_c36_retrieve(Context_Param, C36_fcreate_Ret, C36_retrieve_Ret),
	C36_retrieve_Ret=FnResult.
:- set_opv(f_u_pos_ipt_with_c63, classof, claz_function),
   set_opv(u_pos_ipt_with_c63, compile_as, kw_function),
   set_opv(u_pos_ipt_with_c63, function, f_u_pos_ipt_with_c63),
   DefunResult=u_pos_ipt_with_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41253 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(define-gen POLITY nil", 1, 41489)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41253 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (gs-string-write stream", 1, 41513)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41253 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   (string-append \" \"", 1, 41540)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41253 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (ob$get con 'name))))",
				     1,
				     41563)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41604 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ORGANIZATION',
			    [],
			    
			    [ let,
			      [[name, ['ob$get', con, [quote, name]]]],
			      
			      [ if,
				name,
				
				[ 'gs-string-write',
				  stream,
				  ['string-append', '$STRING'(" "), name]
				],
				
				[ 'gs-string-write',
				  stream,
				  '$STRING'(" some company")
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_organization,
		  [],
		  
		  [ 
		    [ let,
		      [[sys_name, [u_ob_c36_get, u_con, [quote, sys_name]]]],
		      
		      [ if,
			sys_name,
			
			[ u_gs_string_write,
			  stream,
			  
			  [ u_string_append,
			    '$ARRAY'([*], claz_base_character, [#\(' ')]),
			    sys_name
			  ]
			],
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(' '),
				     #\(s),
				     #\(o),
				     #\(m),
				     #\(e),
				     #\(' '),
				     #\(c),
				     #\(o),
				     #\(m),
				     #\(p),
				     #\(a),
				     #\(n),
				     #\(y)
				   ])
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41820 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'CITY',
			    [],
			    
			    [ let,
			      [[name, ['ob$get', con, [quote, name]]]],
			      
			      [ if,
				name,
				
				[ 'gs-string-write',
				  stream,
				  ['string-append', '$STRING'(" "), name]
				],
				
				[ 'gs-string-write',
				  stream,
				  '$STRING'(" some city")
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_city,
		  [],
		  
		  [ 
		    [ let,
		      [[sys_name, [u_ob_c36_get, u_con, [quote, sys_name]]]],
		      
		      [ if,
			sys_name,
			
			[ u_gs_string_write,
			  stream,
			  
			  [ u_string_append,
			    '$ARRAY'([*], claz_base_character, [#\(' ')]),
			    sys_name
			  ]
			],
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(' '),
				     #\(s),
				     #\(o),
				     #\(m),
				     #\(e),
				     #\(' '),
				     #\(c),
				     #\(i),
				     #\(t),
				     #\(y)
				   ])
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:41820 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: We might add pronouns for objects of various types too.",
				     1,
				     42026)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:42089 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'OBJECT',
			    [],
			    
			    [ let,
			      [[name, ['ob$get', con, [quote, name]]]],
			      
			      [ if,
				name,
				
				[ 'gs-string-write',
				  stream,
				  ['string-append', '$STRING'(" "), name]
				],
				
				[ progn,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" the ")
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    
				    [ 'string-downcase!',
				      
				      [ 'symbol->string',
					['ob$name', ['ob$ty', con]]
				      ]
				    ]
				  ]
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_object,
		  [],
		  
		  [ 
		    [ let,
		      [[sys_name, [u_ob_c36_get, u_con, [quote, sys_name]]]],
		      
		      [ if,
			sys_name,
			
			[ u_gs_string_write,
			  stream,
			  
			  [ u_string_append,
			    '$ARRAY'([*], claz_base_character, [#\(' ')]),
			    sys_name
			  ]
			],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(t), #\(h), #\(e), #\(' ')])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    
			    [ u_string_downcase_c33,
			      
			      [ u_symbol_c62_string,
				[u_ob_c36_name, [u_ob_c36_ty, u_con]]
			      ]
			    ]
			  ]
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:42089 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Default generator for an action; tries to come up with something reasonable.",
				     1,
				     42450)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:42528 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ACTION',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				['ob$name', ['ob$ty', con]],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ if,
				['ob$gets', con, [quote, obj]],
				
				[ gen,
				  ['ob$gets', con, [quote, obj]],
				  stream,
				  switches,
				  context,
				  bp
				]
			      ],
			      
			      [ if,
				['ob$get', con, [quote, from]],
				
				[ progn,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" from")
				  ],
				  
				  [ gen,
				    ['ob$get', con, [quote, from]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				]
			      ],
			      
			      [ if,
				['ob$get', con, [quote, to]],
				
				[ progn,
				  ['gs-string-write', stream, '$STRING'(" to")],
				  
				  [ gen,
				    ['ob$get', con, [quote, to]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_action,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[u_ob_c36_name, [u_ob_c36_ty, u_con]],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ if,
			[u_ob_c36_gets, u_con, [quote, u_obj]],
			
			[ u_gen,
			  [u_ob_c36_gets, u_con, [quote, u_obj]],
			  stream,
			  u_switches,
			  u_context,
			  u_bp
			]
		      ],
		      
		      [ if,
			[u_ob_c36_get, u_con, [quote, u_from]],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(f), #\(r), #\(o), #\(m)])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_from]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      
		      [ if,
			[u_ob_c36_get, u_con, [quote, u_to]],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(t), #\(o)])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_to]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:42528 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Default generator for a state; tries to come up with something reasonable.",
				     1,
				     43164)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:43240 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'STATE',
			    [],
			    
			    [ let,
			      
			      [ [subject, ['ob$gets', con, [quote, actor]]],
				[object, ['ob$gets', con, [quote, obj]]]
			      ],
			      
			      [ cond,
				
				[ [and, subject, object],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" the ")
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    
				    [ 'string-downcase!',
				      
				      [ 'symbol->string',
					['ob$name', ['ob$ty', con]]
				      ]
				    ]
				  ],
				  ['gs-string-write', stream, '$STRING'(" of")],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, be],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  [gen, object, stream, switches, context, bp],
				  subject
				],
				
				[ subject,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, be],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  ['gs-string-write', stream, '$STRING'(" ")],
				  
				  [ 'gs-string-write',
				    stream,
				    
				    [ 'string-downcase!',
				      
				      [ 'symbol->string',
					['ob$name', ['ob$ty', con]]
				      ]
				    ]
				  ],
				  subject
				],
				
				[ else,
				  ['gs-string-write', stream, '$STRING'(" ")],
				  
				  [ 'gs-string-write',
				    stream,
				    
				    [ 'string-downcase!',
				      
				      [ 'symbol->string',
					['ob$name', ['ob$ty', con]]
				      ]
				    ]
				  ],
				  t
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_state,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]],
			[u_object, [u_ob_c36_gets, u_con, [quote, u_obj]]]
		      ],
		      
		      [ cond,
			
			[ [and, u_subject, u_object],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(t), #\(h), #\(e), #\(' ')])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    
			    [ u_string_downcase_c33,
			      
			      [ u_symbol_c62_string,
				[u_ob_c36_name, [u_ob_c36_ty, u_con]]
			      ]
			    ]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(o), #\(f)])
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_be],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  [u_gen, u_object, stream, u_switches, u_context, u_bp],
			  u_subject
			],
			
			[ u_subject,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_be],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*], claz_base_character, [#\(' ')])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    
			    [ u_string_downcase_c33,
			      
			      [ u_symbol_c62_string,
				[u_ob_c36_name, [u_ob_c36_ty, u_con]]
			      ]
			    ]
			  ],
			  u_subject
			],
			
			[ u_else,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*], claz_base_character, [#\(' ')])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    
			    [ u_string_downcase_c33,
			      
			      [ u_symbol_c62_string,
				[u_ob_c36_name, [u_ob_c36_ty, u_con]]
			      ]
			    ]
			  ],
			  t
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:43240 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun plural-form (str)", 1, 44329)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:43240 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (string-append str (if (vowel? (last-char str)) \"es\" \"s\")))",
				     1,
				     44355)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:44418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-subject',
			    [con, stream, switches, context, bp],
			    
			    [ let,
			      
			      [ 
				[ 'previous-subject',
				  ['switches-lookup', [quote, 's-bar'], switches]
				]
			      ],
			      
			      [ if,
				'previous-subject',
				
				[ progn,
				  
				  [ if,
				    
				    [ and,
				      
				      [ 'neq?',
					'previous-subject',
					[quote, 'no-subject']
				      ],
				      
				      [ or,
					
					[ 'memq?',
					  
					  [ 'switches-lookup',
					    [quote, tense],
					    switches
					  ],
					  [quote, [present, past, future]]
					],
					
					[ 'null?',
					  
					  [ 'same-subject?',
					    con,
					    'previous-subject'
					  ]
					]
				      ]
				    ],
				    
				    [ 'gen-objective-subject',
				      con,
				      stream,
				      switches,
				      context,
				      bp
				    ]
				  ]
				],
				
				[ progn,
				  
				  [ cond,
				    
				    [ 
				      [ 'eq?',
					
					[ 'switches-lookup',
					  [quote, case],
					  switches
					],
					[quote, reflexive]
				      ],
				      [gen, con, stream, switches, context, bp]
				    ],
				    
				    [ 
				      [ or,
					['infinitive-tense?', switches],
					['gerund-tense?', switches]
				      ],
				      
				      [ 'gen-objective-subject',
					con,
					stream,
					switches,
					context,
					bp
				      ]
				    ],
				    
				    [ else,
				      
				      [ gen,
					con,
					stream,
					
					[ cons,
					  [quote, [case, nominative]],
					  switches
					],
					context,
					bp
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GEN-SUBJECT 
wl: lambda_def(defun,
	      u_gen_subject,
	      f_u_gen_subject,
	      [u_con, stream, u_switches, u_context, u_bp],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_previous_subject,
		      [u_switches_lookup, [quote, u_s_bar], u_switches]
		    ]
		  ],
		  
		  [ if,
		    u_previous_subject,
		    
		    [ progn,
		      
		      [ if,
			
			[ and,
			  [u_neq_c63, u_previous_subject, [quote, u_no_subject]],
			  
			  [ or,
			    
			    [ u_memq_c63,
			      [u_switches_lookup, [quote, u_tense], u_switches],
			      [quote, [u_present, u_past, u_future]]
			    ],
			    
			    [ u_null_c63,
			      [u_same_subject_c63, u_con, u_previous_subject]
			    ]
			  ]
			],
			
			[ u_gen_objective_subject,
			  u_con,
			  stream,
			  u_switches,
			  u_context,
			  u_bp
			]
		      ]
		    ],
		    
		    [ progn,
		      
		      [ cond,
			
			[ 
			  [ u_eq_c63,
			    [u_switches_lookup, [quote, case], u_switches],
			    [quote, u_reflexive]
			  ],
			  [u_gen, u_con, stream, u_switches, u_context, u_bp]
			],
			
			[ 
			  [ or,
			    [u_infinitive_tense_c63, u_switches],
			    [u_gerund_tense_c63, u_switches]
			  ],
			  
			  [ u_gen_objective_subject,
			    u_con,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ u_else,
			  
			  [ u_gen,
			    u_con,
			    stream,
			    [cons, [quote, [case, u_nominative]], u_switches],
			    u_context,
			    u_bp
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GEN-SUBJECT 
wl: arglist_info(u_gen_subject,
		[u_con, stream, u_switches, u_context, u_bp],
		[Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_con, stream, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, stream, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_con, stream, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-SUBJECT 
wl: init_args(exact_only, u_gen_subject).


% annotating U::GEN-SUBJECT 
f_u_gen_subject(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, ElseResult68) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param)],
	f_u_switches_lookup(u_s_bar, Switches_Param, Previous_subject_Init),
	LEnv=[[bv(u_previous_subject, Previous_subject_Init)]|Env],
	get_var(LEnv, u_previous_subject, IFTEST),
	(   IFTEST\==[]
	->  f_u_neq_c63(u_previous_subject, [quote, u_no_subject], IFTEST30),
	    (   IFTEST30\==[]
	    ->  (   f_u_memq_c63(
				 [ u_switches_lookup,
				   [quote, u_tense],
				   u_switches
				 ],
				 [quote, [u_present, u_past, u_future]],
				 FORM1_Res),
		    FORM1_Res\==[],
		    IFTEST28=FORM1_Res
		->  true
		;   f_u_null_c63([u_same_subject_c63, u_con, u_previous_subject],
				 Null_c63_Ret),
		    IFTEST28=Null_c63_Ret
		)
	    ;   IFTEST28=[]
	    ),
	    (   IFTEST28\==[]
	    ->  f_u_gen_objective_subject(Con_Param,
					  Stream_Param,
					  Switches_Param,
					  Context_Param,
					  Bp_Param,
					  TrueResult39),
		ElseResult68=TrueResult39
	    ;   ElseResult68=[]
	    )
	;   f_u_eq_c63([u_switches_lookup, [quote, case], u_switches],
		       [quote, u_reflexive],
		       IFTEST40),
	    (   IFTEST40\==[]
	    ->  f_u_gen(Con_Param,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			TrueResult69),
		ElseResult68=TrueResult69
	    ;   (   f_u_infinitive_tense_c63(Switches_Param, FORM1_Res51),
		    FORM1_Res51\==[],
		    IFTEST47=FORM1_Res51
		->  true
		;   f_u_gerund_tense_c63(Switches_Param, Tense_c63_Ret),
		    IFTEST47=Tense_c63_Ret
		),
		(   IFTEST47\==[]
		->  f_u_gen_objective_subject(Con_Param,
					      Stream_Param,
					      Switches_Param,
					      Context_Param,
					      Bp_Param,
					      TrueResult67),
		    ElseResult68=TrueResult67
		;   get_var(LEnv, u_else, IFTEST57),
		    (   IFTEST57\==[]
		    ->  _74780=[[case, u_nominative]|Switches_Param],
			f_u_gen(Con_Param,
				Stream_Param,
				_74780,
				Context_Param,
				Bp_Param,
				TrueResult65),
			ElseResult68=TrueResult65
		    ;   ElseResult68=[]
		    )
		)
	    )
	).
:- set_opv(f_u_gen_subject, classof, claz_function),
   set_opv(u_gen_subject, compile_as, kw_function),
   set_opv(u_gen_subject, function, f_u_gen_subject),
   DefunResult=u_gen_subject.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:44418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Only do NP-deletion for infinitive/gerund tenses...",
				     23,
				     44765)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:44418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" not quite complete, but it doesn't matter because the",
				     23,
				     44841)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:44418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" verbs for which this matters, i.e., know that, etc.,",
				     23,
				     44919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:44418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" only call this with tense present..",
				     23,
				     44996)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:45561 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-objective-subject',
			    [con, stream, switches, context, bp],
			    
			    [ if,
			      
			      [ 'switches-lookup',
				[quote, 'possessive-subj'],
				switches
			      ],
			      
			      [ gen,
				con,
				stream,
				[cons, [quote, [case, possessive]], switches],
				context,
				bp
			      ],
			      
			      [ gen,
				con,
				stream,
				[cons, [quote, [case, objective]], switches],
				context,
				bp
			      ]
			    ]
			  ]).

% annotating U::GEN-OBJECTIVE-SUBJECT 
wl: lambda_def(defun,
	      u_gen_objective_subject,
	      f_u_gen_objective_subject,
	      [u_con, stream, u_switches, u_context, u_bp],
	      
	      [ 
		[ if,
		  [u_switches_lookup, [quote, u_possessive_subj], u_switches],
		  
		  [ u_gen,
		    u_con,
		    stream,
		    [cons, [quote, [case, u_possessive]], u_switches],
		    u_context,
		    u_bp
		  ],
		  
		  [ u_gen,
		    u_con,
		    stream,
		    [cons, [quote, [case, u_objective]], u_switches],
		    u_context,
		    u_bp
		  ]
		]
	      ]).


% annotating U::GEN-OBJECTIVE-SUBJECT 
wl: arglist_info(u_gen_objective_subject,
		[u_con, stream, u_switches, u_context, u_bp],
		[Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param],
		arginfo{ all:[u_con, stream, u_switches, u_context, u_bp],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, stream, u_switches, u_context, u_bp],
			 opt:0,
			 req:[u_con, stream, u_switches, u_context, u_bp],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-OBJECTIVE-SUBJECT 
wl: init_args(exact_only, u_gen_objective_subject).


% annotating U::GEN-OBJECTIVE-SUBJECT 
f_u_gen_objective_subject(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, FnResult) :-
	f_u_switches_lookup(u_possessive_subj, Switches_Param, IFTEST),
	(   IFTEST\==[]
	->  _72064=[[case, u_possessive]|Switches_Param],
	    f_u_gen(Con_Param,
		    Stream_Param,
		    _72064,
		    Context_Param,
		    Bp_Param,
		    TrueResult),
	    FnResult=TrueResult
	;   _72224=[[case, u_objective]|Switches_Param],
	    f_u_gen(Con_Param,
		    Stream_Param,
		    _72224,
		    Context_Param,
		    Bp_Param,
		    ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_gen_objective_subject, classof, claz_function),
   set_opv(u_gen_objective_subject, compile_as, kw_function),
   set_opv(u_gen_objective_subject, function, f_u_gen_objective_subject),
   DefunResult=u_gen_objective_subject.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:45813 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'same-subject?',
			    [x, y],
			    
			    [ cond,
			      [[and, ['pair?', x], ['pair?', y]], ['alikeq?', x, y]],
			      [['pair?', x], ['alikeq?', x, [list, y]]],
			      [['pair?', y], ['alikeq?', y, [list, x]]],
			      [else, ['eq?', x, y]]
			    ]
			  ]).

% annotating U::SAME-SUBJECT? 
wl: lambda_def(defun,
	      u_same_subject_c63,
	      f_u_same_subject_c63,
	      [u_x, u_y],
	      
	      [ 
		[ cond,
		  
		  [ [and, [u_pair_c63, u_x], [u_pair_c63, u_y]],
		    [u_alikeq_c63, u_x, u_y]
		  ],
		  [[u_pair_c63, u_x], [u_alikeq_c63, u_x, [list, u_y]]],
		  [[u_pair_c63, u_y], [u_alikeq_c63, u_y, [list, u_x]]],
		  [u_else, [u_eq_c63, u_x, u_y]]
		]
	      ]).


% annotating U::SAME-SUBJECT? 
wl: arglist_info(u_same_subject_c63,
		[u_x, u_y],
		[X_Param, Y_Param],
		arginfo{ all:[u_x, u_y],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x, u_y],
			 opt:0,
			 req:[u_x, u_y],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SAME-SUBJECT? 
wl: init_args(exact_only, u_same_subject_c63).


% annotating U::SAME-SUBJECT? 
f_u_same_subject_c63(X_Param, Y_Param, ElseResult29) :-
	Env=[bv(u_x, X_Param), bv(u_y, Y_Param)],
	f_u_pair_c63(u_x, IFTEST16),
	(   IFTEST16\==[]
	->  f_u_pair_c63(u_y, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_alikeq_c63(u_x, u_y, TrueResult32),
	    ElseResult29=TrueResult32
	;   f_u_pair_c63(u_x, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_u_alikeq_c63(u_x, [list, u_y], TrueResult30),
		ElseResult29=TrueResult30
	    ;   f_u_pair_c63(u_y, IFTEST21),
		(   IFTEST21\==[]
		->  f_u_alikeq_c63(u_y, [list, u_x], TrueResult28),
		    ElseResult29=TrueResult28
		;   get_var(Env, u_else, IFTEST23),
		    (   IFTEST23\==[]
		    ->  f_u_eq_c63(u_x, u_y, TrueResult26),
			ElseResult29=TrueResult26
		    ;   ElseResult29=[]
		    )
		)
	    )
	).
:- set_opv(f_u_same_subject_c63, classof, claz_function),
   set_opv(u_same_subject_c63, compile_as, kw_function),
   set_opv(u_same_subject_c63, function, f_u_same_subject_c63),
   DefunResult=u_same_subject_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:46000 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'FAILED-GOAL',
			    [],
			    
			    [ let,
			      
			      [ [subject, [car, bp]],
				[obj, ['ob$get', con, [quote, obj]]]
			      ],
			      
			      [ cond,
				
				[ ['null?', obj],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" a failure")
				  ],
				  t
				],
				
				[ 
				  [ and,
				    ['ty$instance?', obj, [quote, lovers]],
				    
				    [ 'existing-relationship1?',
				      obj,
				      
				      [ or,
					
					[ 'ob$get',
					  con,
					  [quote, 'activation-context']
					],
					['cx$parent', context]
				      ]
				    ]
				  ],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, lose],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ gen,
				    
				    [ 'non-persons',
				      ['ob$gets', obj, [quote, actor]],
				      subject
				    ],
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  subject
				],
				
				[ 
				  [ and,
				    ['ty$instance?', obj, [quote, employment]],
				    
				    [ 'existing-relationship1?',
				      obj,
				      
				      [ or,
					
					[ 'ob$get',
					  con,
					  [quote, 'activation-context']
					],
					['cx$parent', context]
				      ]
				    ]
				  ],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, lose],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ gen,
				    subject,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  ['gs-string-write', stream, '$STRING'(" job")],
				  subject
				],
				
				[ else,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, fail],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  ['gs-string-write', stream, '$STRING'(" at")],
				  
				  [ gen,
				    obj,
				    stream,
				    
				    [ cons,
				      [list, [quote, 's-bar'], subject],
				      [cons, [quote, [tense, gerund]], switches]
				    ],
				    context,
				    bp
				  ],
				  subject
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_failed_goal,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [car, u_bp]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]
		      ],
		      
		      [ cond,
			
			[ [u_null_c63, u_obj],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
				       #\(' '),
				       #\(f),
				       #\(a),
				       #\(i),
				       #\(l),
				       #\(u),
				       #\(r),
				       #\(e)
				     ])
			  ],
			  t
			],
			
			[ 
			  [ and,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_lovers]],
			    
			    [ u_existing_relationship1_c63,
			      u_obj,
			      
			      [ or,
				
				[ u_ob_c36_get,
				  u_con,
				  [quote, u_activation_context]
				],
				[u_cx_c36_parent, u_context]
			      ]
			    ]
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_lose],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gen,
			    
			    [ u_non_persons,
			      [u_ob_c36_gets, u_obj, [quote, u_actor]],
			      u_subject
			    ],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  u_subject
			],
			
			[ 
			  [ and,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_employment]],
			    
			    [ u_existing_relationship1_c63,
			      u_obj,
			      
			      [ or,
				
				[ u_ob_c36_get,
				  u_con,
				  [quote, u_activation_context]
				],
				[u_cx_c36_parent, u_context]
			      ]
			    ]
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_lose],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gen,
			    u_subject,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(j), #\(o), #\(b)])
			  ],
			  u_subject
			],
			
			[ u_else,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_fail],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(a), #\(t)])
			  ],
			  
			  [ u_gen,
			    u_obj,
			    stream,
			    
			    [ cons,
			      [list, [quote, u_s_bar], u_subject],
			      [cons, [quote, [u_tense, u_gerund]], u_switches]
			    ],
			    u_context,
			    u_bp
			  ],
			  u_subject
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:46000 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Change to \"I successfully went to the store\"",
				     1,
				     47320)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:47372 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'SUCCEEDED-GOAL',
			    [],
			    
			    [ let,
			      
			      [ [subject, [car, bp]],
				[obj, ['ob$get', con, [quote, obj]]]
			      ],
			      
			      [ if,
				obj,
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, succeed],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  ['gs-string-write', stream, '$STRING'(" at")],
				  
				  [ gen,
				    obj,
				    stream,
				    
				    [ cons,
				      [list, [quote, 's-bar'], subject],
				      [cons, [quote, [tense, gerund]], switches]
				    ],
				    context,
				    bp
				  ],
				  subject
				],
				
				[ progn,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" a success")
				  ],
				  t
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_succeeded_goal,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [car, u_bp]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]
		      ],
		      
		      [ if,
			u_obj,
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_succeed],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(a), #\(t)])
			  ],
			  
			  [ u_gen,
			    u_obj,
			    stream,
			    
			    [ cons,
			      [list, [quote, u_s_bar], u_subject],
			      [cons, [quote, [u_tense, u_gerund]], u_switches]
			    ],
			    u_context,
			    u_bp
			  ],
			  u_subject
			],
			
			[ progn,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
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
			  t
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:47888 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'PRESERVATION',
			    [],
			    
			    [ gen,
			      ['ob$get', con, [quote, obj]],
			      stream,
			      
			      [ cons,
				[quote, ['continue?', t]],
				
				[ cons,
				  [list, [quote, 's-bar'], [car, bp]],
				  [cons, [quote, [tense, infinitive]], switches]
				]
			      ],
			      context,
			      bp
			    ]
			  ]).
:- f_u_define_gen(u_preservation,
		  [],
		  
		  [ 
		    [ u_gen,
		      [u_ob_c36_get, u_con, [quote, u_obj]],
		      stream,
		      
		      [ cons,
			[quote, [u_continue_c63, t]],
			
			[ cons,
			  [list, [quote, u_s_bar], [car, u_bp]],
			  [cons, [quote, [u_tense, u_infinitive]], u_switches]
			]
		      ],
		      u_context,
		      u_bp
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:48104 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ACTIVE-GOAL',
			    [],
			    
			    [ let,
			      
			      [ [subject, [car, bp]],
				[obj, ['ob$get', con, [quote, obj]]],
				['top?', ['top-goal?', con, context, bp]]
			      ],
			      
			      [ cond,
				
				[ 
				  [ and,
				    ['ty$instance?', obj, [quote, employment]],
				    ['ty$instance?', subject, [quote, actor]]
				  ],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, need],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" work")
				  ]
				],
				
				[ ['ty$instance?', obj, [quote, employment]],
				  
				  [ let,
				    
				    [ 
				      [ pers,
					
					[ 'persons-in',
					  ['ob$gets', obj, [quote, actor]]
					]
				      ],
				      
				      [ org,
					
					[ 'non-persons-in',
					  ['ob$gets', obj, [quote, actor]]
					]
				      ]
				    ],
				    
				    [ if,
				      ['null?', org],
				      
				      [ progn,
					
					[ 'gen-subject',
					  pers,
					  stream,
					  switches,
					  context,
					  bp
					],
					
					[ if,
					  'top?',
					  
					  [ 'gen-desire-verb',
					    subject,
					    stream,
					    switches,
					    con
					  ],
					  
					  [ 'gen-verb',
					    [quote, need],
					    subject,
					    stream,
					    switches,
					    ['neg?', con]
					  ]
					],
					
					[ 'gs-string-write',
					  stream,
					  '$STRING'(" a job")
					]
				      ],
				      
				      [ progn,
					
					[ 'gen-subject',
					  pers,
					  stream,
					  switches,
					  context,
					  bp
					],
					
					[ if,
					  'top?',
					  
					  [ 'gen-desire-verb',
					    subject,
					    stream,
					    switches,
					    con
					  ],
					  
					  [ 'gen-verb',
					    [quote, need],
					    subject,
					    stream,
					    switches,
					    ['neg?', con]
					  ]
					],
					
					[ 'gs-string-write',
					  stream,
					  '$STRING'(" to get a job with")
					],
					[gen, org, stream, switches, context, bp]
				      ]
				    ]
				  ]
				],
				
				[ 'top?',
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-desire-verb',
				    subject,
				    stream,
				    switches,
				    con
				  ],
				  
				  [ gen,
				    ['ob$get', con, [quote, obj]],
				    stream,
				    
				    [ cons,
				      
				      [ list,
					[quote, 'continue?'],
					['ty$instance?', con, [quote, 'p-goal']]
				      ],
				      
				      [ cons,
					[quote, ['active-goal?', t]],
					
					[ cons,
					  [list, [quote, 's-bar'], subject],
					  
					  [ cons,
					    [quote, [tense, infinitive]],
					    switches
					  ]
					]
				      ]
				    ],
				    context,
				    bp
				  ]
				],
				
				[ else,
				  
				  [ gen,
				    ['ob$get', con, [quote, obj]],
				    stream,
				    
				    [ cons,
				      [quote, ['active-goal?', t]],
				      ['have-to-ize', switches]
				    ],
				    context,
				    bp
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_active_goal,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [car, u_bp]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]],
			[u_top_c63, [u_top_goal_c63, u_con, u_context, u_bp]]
		      ],
		      
		      [ cond,
			
			[ 
			  [ and,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_employment]],
			    [u_ty_c36_instance_c63, u_subject, [quote, u_actor]]
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_need],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(w), #\(o), #\(r), #\(k)])
			  ]
			],
			
			[ [u_ty_c36_instance_c63, u_obj, [quote, u_employment]],
			  
			  [ let,
			    
			    [ 
			      [ u_pers,
				
				[ u_persons_in,
				  [u_ob_c36_gets, u_obj, [quote, u_actor]]
				]
			      ],
			      
			      [ u_org,
				
				[ u_non_persons_in,
				  [u_ob_c36_gets, u_obj, [quote, u_actor]]
				]
			      ]
			    ],
			    
			    [ if,
			      [u_null_c63, u_org],
			      
			      [ progn,
				
				[ u_gen_subject,
				  u_pers,
				  stream,
				  u_switches,
				  u_context,
				  u_bp
				],
				
				[ if,
				  u_top_c63,
				  
				  [ u_gen_desire_verb,
				    u_subject,
				    stream,
				    u_switches,
				    u_con
				  ],
				  
				  [ u_gen_verb,
				    [quote, u_need],
				    u_subject,
				    stream,
				    u_switches,
				    [u_neg_c63, u_con]
				  ]
				],
				
				[ u_gs_string_write,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(' '),
					     #\(a),
					     #\(' '),
					     #\(j),
					     #\(o),
					     #\(b)
					   ])
				]
			      ],
			      
			      [ progn,
				
				[ u_gen_subject,
				  u_pers,
				  stream,
				  u_switches,
				  u_context,
				  u_bp
				],
				
				[ if,
				  u_top_c63,
				  
				  [ u_gen_desire_verb,
				    u_subject,
				    stream,
				    u_switches,
				    u_con
				  ],
				  
				  [ u_gen_verb,
				    [quote, u_need],
				    u_subject,
				    stream,
				    u_switches,
				    [u_neg_c63, u_con]
				  ]
				],
				
				[ u_gs_string_write,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\(' '),
					     #\(t),
					     #\(o),
					     #\(' '),
					     #\(g),
					     #\(e),
					     #\(t),
					     #\(' '),
					     #\(a),
					     #\(' '),
					     #\(j),
					     #\(o),
					     #\(b),
					     #\(' '),
					     #\(w),
					     #\(i),
					     #\(t),
					     #\(h)
					   ])
				],
				[u_gen, u_org, stream, u_switches, u_context, u_bp]
			      ]
			    ]
			  ]
			],
			
			[ u_top_c63,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_desire_verb,
			    u_subject,
			    stream,
			    u_switches,
			    u_con
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_obj]],
			    stream,
			    
			    [ cons,
			      
			      [ list,
				[quote, u_continue_c63],
				[u_ty_c36_instance_c63, u_con, [quote, u_p_goal]]
			      ],
			      
			      [ cons,
				[quote, [u_active_goal_c63, t]],
				
				[ cons,
				  [list, [quote, u_s_bar], u_subject],
				  
				  [ cons,
				    [quote, [u_tense, u_infinitive]],
				    u_switches
				  ]
				]
			      ]
			    ],
			    u_context,
			    u_bp
			  ]
			],
			
			[ u_else,
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_obj]],
			    stream,
			    
			    [ cons,
			      [quote, [u_active_goal_c63, t]],
			      [u_have_to_ize, u_switches]
			    ],
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:48104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("subgoal", 13, 49703)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:49862 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-desire-verb',
			    [subject, stream, switches, con],
			    
			    [ if,
			      ['switches-lookup', [quote, mtrans], switches],
			      
			      [ progn,
				
				[ 'gen-verb',
				  [quote, would],
				  subject,
				  stream,
				  switches,
				  ['neg?', con]
				],
				['gs-string-write', stream, '$STRING'(" like")]
			      ],
			      
			      [ 'gen-verb',
				[quote, want],
				subject,
				stream,
				switches,
				['neg?', con]
			      ]
			    ]
			  ]).

% annotating U::GEN-DESIRE-VERB 
wl: lambda_def(defun,
	      u_gen_desire_verb,
	      f_u_gen_desire_verb,
	      [u_subject, stream, u_switches, u_con],
	      
	      [ 
		[ if,
		  [u_switches_lookup, [quote, u_mtrans], u_switches],
		  
		  [ progn,
		    
		    [ u_gen_verb,
		      [quote, u_would],
		      u_subject,
		      stream,
		      u_switches,
		      [u_neg_c63, u_con]
		    ],
		    
		    [ u_gs_string_write,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(l), #\(i), #\(k), #\(e)])
		    ]
		  ],
		  
		  [ u_gen_verb,
		    [quote, u_want],
		    u_subject,
		    stream,
		    u_switches,
		    [u_neg_c63, u_con]
		  ]
		]
	      ]).


% annotating U::GEN-DESIRE-VERB 
wl: arglist_info(u_gen_desire_verb,
		[u_subject, stream, u_switches, u_con],
		[Subject_Param, Stream_Param, Switches_Param, Con_Param],
		arginfo{ all:[u_subject, stream, u_switches, u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subject, stream, u_switches, u_con],
			 opt:0,
			 req:[u_subject, stream, u_switches, u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-DESIRE-VERB 
wl: init_args(exact_only, u_gen_desire_verb).


% annotating U::GEN-DESIRE-VERB 
f_u_gen_desire_verb(Subject_Param, Stream_Param, Switches_Param, Con_Param, FnResult) :-
	f_u_switches_lookup(u_mtrans, Switches_Param, IFTEST),
	(   IFTEST\==[]
	->  f_u_neg_c63(Con_Param, Neg_c63_Ret),
	    f_u_gen_verb(u_would,
			 Subject_Param,
			 Stream_Param,
			 Switches_Param,
			 Neg_c63_Ret,
			 Gen_verb_Ret),
	    f_u_gs_string_write(Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(l), #\(i), #\(k), #\(e)]),
				TrueResult),
	    FnResult=TrueResult
	;   f_u_neg_c63(Con_Param, Neg_c63_Ret36),
	    f_u_gen_verb(u_want,
			 Subject_Param,
			 Stream_Param,
			 Switches_Param,
			 Neg_c63_Ret36,
			 ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_gen_desire_verb, classof, claz_function),
   set_opv(u_gen_desire_verb, compile_as, kw_function),
   set_opv(u_gen_desire_verb, function, f_u_gen_desire_verb),
   DefunResult=u_gen_desire_verb.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:50131 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ACTIVE-P-GOAL',
			    [],
			    
			    [ let,
			      
			      [ [subject, [car, bp]],
				[obj, ['ob$get', con, [quote, obj]]]
			      ],
			      
			      [ cond,
				
				[ 
				  [ and,
				    ['ty$instance?', obj, [quote, lovers]],
				    
				    [ 'memq?',
				      '*me-ob*',
				      ['ob$gets', obj, [quote, actor]]
				    ]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" our relationship")
				  ],
				  
				  [ 'gen-verb',
				    [quote, be],
				    [],
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" in trouble")
				  ]
				],
				[else, []]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_active_p_goal,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [car, u_bp]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]
		      ],
		      
		      [ cond,
			
			[ 
			  [ and,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_lovers]],
			    
			    [ u_memq_c63,
			      u_xx_me_ob_xx,
			      [u_ob_c36_gets, u_obj, [quote, u_actor]]
			    ]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(o),
				       #\(u),
				       #\(r),
				       #\(' '),
				       #\(r),
				       #\(e),
				       #\(l),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n),
				       #\(s),
				       #\(h),
				       #\(i),
				       #\(p)
				     ])
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_be],
			    [],
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(t),
				       #\(r),
				       #\(o),
				       #\(u),
				       #\(b),
				       #\(l),
				       #\(e)
				     ])
			  ]
			],
			[u_else, []]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:50499 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'KNOW',
			    [],
			    
			    [ let,
			      
			      [ [subject, ['ob$gets', con, [quote, actor]]],
				[obj, ['ob$get', con, [quote, obj]]]
			      ],
			      
			      [ if,
				
				[ 'any?',
				  
				  [ lambda,
				    [x],
				    
				    [ and,
				      [not, ['var?', x]],
				      [not, ['ty$instance?', x, [quote, person]]]
				    ]
				  ],
				  subject
				],
				
				[ progn,
				  
				  [ 'gen-subject',
				    obj,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, be],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  ['gs-string-write', stream, '$STRING'(" in")],
				  [gen, subject, stream, switches, context, bp],
				  obj
				],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, know],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ if,
				    obj,
				    
				    [ cond,
				      
				      [ 
					[ or,
					  
					  [ 'ty$instance?',
					    obj,
					    [quote, 'OBJECT']
					  ],
					  
					  [ 'ty$instance?',
					    obj,
					    [quote, 'LOCATION']
					  ],
					  
					  [ 'ty$instance?',
					    obj,
					    [quote, 'TIME-OF-DAY']
					  ],
					  ['ty$instance?', obj, [quote, 'VPROX']],
					  ['ty$instance?', obj, [quote, 'TELNO']]
					],
					
					[ gen,
					  obj,
					  stream,
					  ['make-indicative', switches],
					  context,
					  bp
					]
				      ],
				      
				      [ else,
					
					[ if,
					  '*gen-thats*',
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" that")
					  ]
					],
					
					[ gen,
					  obj,
					  stream,
					  ['make-indicative', switches],
					  context,
					  bp
					]
				      ]
				    ]
				  ],
				  subject
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_know,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]
		      ],
		      
		      [ if,
			
			[ u_any_c63,
			  
			  [ lambda,
			    [u_x],
			    
			    [ and,
			      [not, [u_var_c63, u_x]],
			      
			      [ not,
				[u_ty_c36_instance_c63, u_x, [quote, u_person]]
			      ]
			    ]
			  ],
			  u_subject
			],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_obj,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_be],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(i), #\(n)])
			  ],
			  [u_gen, u_subject, stream, u_switches, u_context, u_bp],
			  u_obj
			],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_know],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ if,
			    u_obj,
			    
			    [ cond,
			      
			      [ 
				[ or,
				  
				  [ u_ty_c36_instance_c63,
				    u_obj,
				    [quote, u_object]
				  ],
				  
				  [ u_ty_c36_instance_c63,
				    u_obj,
				    [quote, u_location]
				  ],
				  
				  [ u_ty_c36_instance_c63,
				    u_obj,
				    [quote, u_time_of_day]
				  ],
				  
				  [ u_ty_c36_instance_c63,
				    u_obj,
				    [quote, u_vprox]
				  ],
				  
				  [ u_ty_c36_instance_c63,
				    u_obj,
				    [quote, u_telno]
				  ]
				],
				
				[ u_gen,
				  u_obj,
				  stream,
				  [u_make_indicative, u_switches],
				  u_context,
				  u_bp
				]
			      ],
			      
			      [ u_else,
				
				[ if,
				  u_xx_gen_thats_xx,
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(t), #\(h), #\(a), #\(t)])
				  ]
				],
				
				[ u_gen,
				  u_obj,
				  stream,
				  [u_make_indicative, u_switches],
				  u_context,
				  u_bp
				]
			      ]
			    ]
			  ],
			  u_subject
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:51642 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-indicative',
			    [switches],
			    
			    [ append,
			      [quote, [[tense, present], ['s-bar', []]]],
			      switches
			    ]
			  ]).

% annotating U::MAKE-INDICATIVE 
wl: lambda_def(defun,
	      u_make_indicative,
	      f_u_make_indicative,
	      [u_switches],
	      [[append, [quote, [[u_tense, u_present], [u_s_bar, []]]], u_switches]]).


% annotating U::MAKE-INDICATIVE 
wl: arglist_info(u_make_indicative,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-INDICATIVE 
wl: init_args(exact_only, u_make_indicative).


% annotating U::MAKE-INDICATIVE 
f_u_make_indicative(Switches_Param, FnResult) :-
	cl_append([[u_tense, u_present], [u_s_bar, []]],
		  Switches_Param,
		  Append_Ret),
	Append_Ret=FnResult.
:- set_opv(f_u_make_indicative, classof, claz_function),
   set_opv(u_make_indicative, compile_as, kw_function),
   set_opv(u_make_indicative, function, f_u_make_indicative),
   DefunResult=u_make_indicative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:51728 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'TELNO',
			    [],
			    
			    [ let,
			      
			      [ [subject, ['ob$gets', con, [quote, actor]]],
				[number, ['ob$gets', con, [quote, obj]]]
			      ],
			      
			      [ if,
				['string?', number],
				
				[ progn,
				  
				  [ gen,
				    subject,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" telephone number is")
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    ['string-append', '$STRING'(" "), number]
				  ]
				],
				
				[ progn,
				  
				  [ gen,
				    subject,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" telephone number")
				  ]
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_telno,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]],
			[number, [u_ob_c36_gets, u_con, [quote, u_obj]]]
		      ],
		      
		      [ if,
			[u_string_c63, number],
			
			[ progn,
			  
			  [ u_gen,
			    u_subject,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(t),
				       #\(e),
				       #\(l),
				       #\(e),
				       #\(p),
				       #\(h),
				       #\(o),
				       #\(n),
				       #\(e),
				       #\(' '),
				       #\(n),
				       #\(u),
				       #\(m),
				       #\(b),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(i),
				       #\(s)
				     ])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    
			    [ u_string_append,
			      '$ARRAY'([*], claz_base_character, [#\(' ')]),
			      number
			    ]
			  ]
			],
			
			[ progn,
			  
			  [ u_gen,
			    u_subject,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(t),
				       #\(e),
				       #\(l),
				       #\(e),
				       #\(p),
				       #\(h),
				       #\(o),
				       #\(n),
				       #\(e),
				       #\(' '),
				       #\(n),
				       #\(u),
				       #\(m),
				       #\(b),
				       #\(e),
				       #\(r)
				     ])
			  ]
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:52217 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ADDRESS',
			    [],
			    
			    [ let,
			      
			      [ [subject, ['ob$gets', con, [quote, actor]]],
				[number, ['ob$gets', con, [quote, obj]]]
			      ],
			      
			      [ if,
				['string?', number],
				
				[ progn,
				  
				  [ gen,
				    subject,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" address is")
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    ['string-append', '$STRING'(" "), number]
				  ]
				],
				
				[ progn,
				  
				  [ gen,
				    subject,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" address")
				  ]
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(ext_address,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]],
			[number, [u_ob_c36_gets, u_con, [quote, u_obj]]]
		      ],
		      
		      [ if,
			[u_string_c63, number],
			
			[ progn,
			  
			  [ u_gen,
			    u_subject,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
				       #\(d),
				       #\(d),
				       #\(r),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(' '),
				       #\(i),
				       #\(s)
				     ])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    
			    [ u_string_append,
			      '$ARRAY'([*], claz_base_character, [#\(' ')]),
			      number
			    ]
			  ]
			],
			
			[ progn,
			  
			  [ u_gen,
			    u_subject,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
				       #\(d),
				       #\(d),
				       #\(r),
				       #\(e),
				       #\(s),
				       #\(s)
				     ])
			  ]
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:52690 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'non-var-ob?',
			    [x],
			    [and, ['ob?', x], [not, ['var?', x]]]
			  ]).

% annotating U::NON-VAR-OB? 
wl: lambda_def(defun,
	      u_non_var_ob_c63,
	      f_u_non_var_ob_c63,
	      [u_x],
	      [[and, [u_ob_c63, u_x], [not, [u_var_c63, u_x]]]]).


% annotating U::NON-VAR-OB? 
wl: arglist_info(u_non_var_ob_c63,
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

% annotating U::NON-VAR-OB? 
wl: init_args(exact_only, u_non_var_ob_c63).


% annotating U::NON-VAR-OB? 
f_u_non_var_ob_c63(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_ob_c63(u_x, IFTEST),
	(   IFTEST\==[]
	->  f_u_var_c63(u_x, Not_Param),
	    cl_not(Not_Param, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_non_var_ob_c63, classof, claz_function),
   set_opv(u_non_var_ob_c63, compile_as, kw_function),
   set_opv(u_non_var_ob_c63, function, f_u_non_var_ob_c63),
   DefunResult=u_non_var_ob_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:52753 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ALUMNI-DIR',
			    [],
			    
			    [ let,
			      
			      [ [college, ['ob$pget', con, [quote, [obj, obj]]]],
				[person, ['ob$pget', con, [quote, [obj, actor]]]]
			      ],
			      
			      [ cond,
				
				[ 
				  [ and,
				    ['non-var-ob?', college],
				    ['non-var-ob?', person]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" the Alumni Directory for")
				  ],
				  
				  [ gen,
				    person,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" college")
				  ],
				  
				  [ gen,
				    college,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ]
				],
				
				[ ['non-var-ob?', college],
				  ['gs-string-write', stream, '$STRING'(" the")],
				  
				  [ gen,
				    college,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" Alumni Directory")
				  ]
				],
				
				[ ['non-var-ob?', person],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" the Alumni Directory for")
				  ],
				  
				  [ gen,
				    person,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" college")
				  ]
				],
				
				[ else,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" an Alumni Directory")
				  ]
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_alumni_dir,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ 
			[ u_college,
			  [u_ob_c36_pget, u_con, [quote, [u_obj, u_obj]]]
			],
			
			[ u_person,
			  [u_ob_c36_pget, u_con, [quote, [u_obj, u_actor]]]
			]
		      ],
		      
		      [ cond,
			
			[ 
			  [ and,
			    [u_non_var_ob_c63, u_college],
			    [u_non_var_ob_c63, u_person]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(' '),
				       #\('A'),
				       #\(l),
				       #\(u),
				       #\(m),
				       #\(n),
				       #\(i),
				       #\(' '),
				       #\('D'),
				       #\(i),
				       #\(r),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(o),
				       #\(r),
				       #\(y),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r)
				     ])
			  ],
			  
			  [ u_gen,
			    u_person,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(c),
				       #\(o),
				       #\(l),
				       #\(l),
				       #\(e),
				       #\(g),
				       #\(e)
				     ])
			  ],
			  
			  [ u_gen,
			    u_college,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ]
			],
			
			[ [u_non_var_ob_c63, u_college],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(t), #\(h), #\(e)])
			  ],
			  
			  [ u_gen,
			    u_college,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\('A'),
				       #\(l),
				       #\(u),
				       #\(m),
				       #\(n),
				       #\(i),
				       #\(' '),
				       #\('D'),
				       #\(i),
				       #\(r),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(o),
				       #\(r),
				       #\(y)
				     ])
			  ]
			],
			
			[ [u_non_var_ob_c63, u_person],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(t),
				       #\(h),
				       #\(e),
				       #\(' '),
				       #\('A'),
				       #\(l),
				       #\(u),
				       #\(m),
				       #\(n),
				       #\(i),
				       #\(' '),
				       #\('D'),
				       #\(i),
				       #\(r),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(o),
				       #\(r),
				       #\(y),
				       #\(' '),
				       #\(f),
				       #\(o),
				       #\(r)
				     ])
			  ],
			  
			  [ u_gen,
			    u_person,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(c),
				       #\(o),
				       #\(l),
				       #\(l),
				       #\(e),
				       #\(g),
				       #\(e)
				     ])
			  ]
			],
			
			[ u_else,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
				       #\(n),
				       #\(' '),
				       #\('A'),
				       #\(l),
				       #\(u),
				       #\(m),
				       #\(n),
				       #\(i),
				       #\(' '),
				       #\('D'),
				       #\(i),
				       #\(r),
				       #\(e),
				       #\(c),
				       #\(t),
				       #\(o),
				       #\(r),
				       #\(y)
				     ])
			  ]
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:53699 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'COLLEGE',
			    [],
			    
			    [ let,
			      
			      [ [college, ['ob$pget', con, [quote, obj]]],
				[subject, ['ob$pget', con, [quote, actor]]]
			      ],
			      
			      [ cond,
				
				[ ['non-var-ob?', college],
				  
				  [ 'gen-regular-plus',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    [quote, go],
				    '$STRING'(" to school at"),
				    college
				  ]
				],
				
				[ ['non-var-ob?', person],
				  
				  [ gen,
				    person,
				    stream,
				    [cons, [quote, [case, possessive]], switches],
				    context,
				    bp
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" college")
				  ],
				  t
				],
				
				[ else,
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" someone's college")
				  ],
				  t
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_college,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_college, [u_ob_c36_pget, u_con, [quote, u_obj]]],
			[u_subject, [u_ob_c36_pget, u_con, [quote, u_actor]]]
		      ],
		      
		      [ cond,
			
			[ [u_non_var_ob_c63, u_college],
			  
			  [ u_gen_regular_plus,
			    u_con,
			    stream,
			    u_switches,
			    u_context,
			    u_bp,
			    [quote, go],
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(t),
				       #\(o),
				       #\(' '),
				       #\(s),
				       #\(c),
				       #\(h),
				       #\(o),
				       #\(o),
				       #\(l),
				       #\(' '),
				       #\(a),
				       #\(t)
				     ]),
			    u_college
			  ]
			],
			
			[ [u_non_var_ob_c63, u_person],
			  
			  [ u_gen,
			    u_person,
			    stream,
			    [cons, [quote, [case, u_possessive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(c),
				       #\(o),
				       #\(l),
				       #\(l),
				       #\(e),
				       #\(g),
				       #\(e)
				     ])
			  ],
			  t
			],
			
			[ u_else,
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(s),
				       #\(o),
				       #\(m),
				       #\(e),
				       #\(o),
				       #\(n),
				       #\(e),
				       #\('\''),
				       #\(s),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(l),
				       #\(l),
				       #\(e),
				       #\(g),
				       #\(e)
				     ])
			  ],
			  t
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:54206 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'CLOTHES',
			    [],
			    
			    [ let,
			      [[obj, ['ob$get', con, [quote, obj]]]],
			      
			      [ gen,
				obj,
				stream,
				[cons, [quote, [case, possessive]], switches],
				context,
				bp
			      ],
			      ['gs-string-write', stream, '$STRING'(" clothes")],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_clothes,
		  [],
		  
		  [ 
		    [ let,
		      [[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]],
		      
		      [ u_gen,
			u_obj,
			stream,
			[cons, [quote, [case, u_possessive]], u_switches],
			u_context,
			u_bp
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(c),
				   #\(l),
				   #\(o),
				   #\(t),
				   #\(h),
				   #\(e),
				   #\(s)
				 ])
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:54376 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'MOVIES',
			    [],
			    
			    [ let,
			      [[obj, ['ob$get', con, [quote, obj]]]],
			      
			      [ gen,
				obj,
				stream,
				[cons, [quote, [case, possessive]], switches],
				context,
				bp
			      ],
			      ['gs-string-write', stream, '$STRING'(" movies")],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_movies,
		  [],
		  
		  [ 
		    [ let,
		      [[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]]],
		      
		      [ u_gen,
			u_obj,
			stream,
			[cons, [quote, [case, u_possessive]], u_switches],
			u_context,
			u_bp
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(m), #\(o), #\(v), #\(i), #\(e), #\(s)])
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:54544 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'PTRANS',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, go],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ if,
				['ob$get', con, [quote, to]],
				
				[ progn,
				  
				  [ if,
				    
				    [ not,
				      ['to-less?', ['ob$get', con, [quote, to]]]
				    ],
				    
				    [ 'gs-string-write',
				      stream,
				      '$STRING'(" to")
				    ]
				  ],
				  
				  [ gen,
				    ['ob$get', con, [quote, to]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_ptrans,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, go],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ if,
			[u_ob_c36_get, u_con, [quote, u_to]],
			
			[ progn,
			  
			  [ if,
			    
			    [ not,
			      
			      [ u_to_less_c63,
				[u_ob_c36_get, u_con, [quote, u_to]]
			      ]
			    ],
			    
			    [ u_gs_string_write,
			      stream,
			      '$ARRAY'([*],
				       claz_base_character,
				       [#\(' '), #\(t), #\(o)])
			    ]
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_to]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:54930 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'ATRANS',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ if,
				
				[ and,
				  ['ob$get', con, [quote, obj]],
				  
				  [ 'ty$instance?',
				    ['ob$get', con, [quote, obj]],
				    [quote, cash]
				  ]
				],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, pay],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ gen,
				    ['ob$get', con, [quote, to]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				],
				
				[ progn,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, give],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ gen,
				    ['ob$get', con, [quote, obj]],
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  ['gs-string-write', stream, '$STRING'(" to")],
				  
				  [ gen,
				    ['ob$get', con, [quote, to]],
				    stream,
				    switches,
				    context,
				    bp
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_atrans,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		      
		      [ if,
			
			[ and,
			  [u_ob_c36_get, u_con, [quote, u_obj]],
			  
			  [ u_ty_c36_instance_c63,
			    [u_ob_c36_get, u_con, [quote, u_obj]],
			    [quote, u_cash]
			  ]
			],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_pay],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_to]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			],
			
			[ progn,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_give],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_obj]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(t), #\(o)])
			  ],
			  
			  [ u_gen,
			    [u_ob_c36_get, u_con, [quote, u_to]],
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:55617 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'EARTHQUAKE',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, obj]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, has],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" an earthquake")
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_earthquake,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_obj]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_has],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(n),
				   #\(' '),
				   #\(e),
				   #\(a),
				   #\(r),
				   #\(t),
				   #\(h),
				   #\(q),
				   #\(u),
				   #\(a),
				   #\(k),
				   #\(e)
				 ])
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:55855 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'EARTHQUAKE-ONSET',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, obj]]]],
			      
			      [ 'gs-string-write',
				stream,
				'$STRING'(" an earthquake")
			      ],
			      
			      [ 'gen-verb',
				[quote, begin],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      ['gs-string-write', stream, '$STRING'(" in")],
			      [gen, subject, stream, switches, context, bp],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_earthquake_onset,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_obj]]]],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(' '),
				   #\(a),
				   #\(n),
				   #\(' '),
				   #\(e),
				   #\(a),
				   #\(r),
				   #\(t),
				   #\(h),
				   #\(q),
				   #\(u),
				   #\(a),
				   #\(k),
				   #\(e)
				 ])
		      ],
		      
		      [ u_gen_verb,
			[quote, u_begin],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(i), #\(n)])
		      ],
		      [u_gen, u_subject, stream, u_switches, u_context, u_bp],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:56129 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'M-PURCHASE',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ 'gen-subject',
				subject,
				stream,
				switches,
				context,
				bp
			      ],
			      
			      [ 'gen-verb',
				[quote, buy],
				subject,
				stream,
				switches,
				['neg?', con]
			      ],
			      
			      [ gen,
				['ob$get', con, [quote, obj]],
				stream,
				switches,
				context,
				bp
			      ],
			      ['gs-string-write', stream, '$STRING'(" from")],
			      
			      [ gen,
				['ob$get', con, [quote, from]],
				stream,
				switches,
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_m_purchase,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			[quote, u_buy],
			u_subject,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_obj]],
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(f), #\(r), #\(o), #\(m)])
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_from]],
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:56473 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'to-less?',
			    [to],
			    
			    [ or,
			      ['eq?', to, ['ob$name->ob', [quote, 'Outside']]],
			      ['eq?', to, ['ob$name->ob', [quote, 'Home']]]
			    ]
			  ]).

% annotating U::TO-LESS? 
wl: lambda_def(defun,
	      u_to_less_c63,
	      f_u_to_less_c63,
	      [u_to],
	      
	      [ 
		[ or,
		  [u_eq_c63, u_to, [u_ob_c36_name_c62_ob, [quote, u_outside]]],
		  [u_eq_c63, u_to, [u_ob_c36_name_c62_ob, [quote, u_home]]]
		]
	      ]).


% annotating U::TO-LESS? 
wl: arglist_info(u_to_less_c63,
		[u_to],
		[To_Param],
		arginfo{ all:[u_to],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_to],
			 opt:0,
			 req:[u_to],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TO-LESS? 
wl: init_args(exact_only, u_to_less_c63).


% annotating U::TO-LESS? 
f_u_to_less_c63(To_Param, FnResult) :-
	Env=[bv(u_to, To_Param)],
	(   f_u_eq_c63(u_to,
		       [u_ob_c36_name_c62_ob, [quote, u_outside]],
		       FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_eq_c63(u_to, [u_ob_c36_name_c62_ob, [quote, u_home]], Eq_c63_Ret),
	    FnResult=Eq_c63_Ret
	).
:- set_opv(f_u_to_less_c63, classof, claz_function),
   set_opv(u_to_less_c63, compile_as, kw_function),
   set_opv(u_to_less_c63, function, f_u_to_less_c63),
   DefunResult=u_to_less_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:56570 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'PTRANS1',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, go],
			      '$STRING'(" to"),
			      ['ob$get', con, [quote, to]]
			    ]
			  ]).
:- f_u_define_gen(u_ptrans1,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, go],
		      '$ARRAY'([*], claz_base_character, [#\(' '), #\(t), #\(o)]),
		      [u_ob_c36_get, u_con, [quote, u_to]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:56695 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'EMPLOYMENT',
			    [],
			    
			    [ let,
			      
			      [ 
				[ subject,
				  
				  [ 'gen-split-subject',
				    con,
				    stream,
				    switches,
				    context,
				    bp,
				    [quote, have],
				    '$STRING'(" a job with")
				  ]
				]
			      ],
			      ['gs-string-write', stream, '$STRING'(" at")],
			      
			      [ gen,
				['ob$get', con, [quote, organization]],
				stream,
				switches,
				context,
				bp
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_employment,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ 
			[ u_subject,
			  
			  [ u_gen_split_subject,
			    u_con,
			    stream,
			    u_switches,
			    u_context,
			    u_bp,
			    [quote, u_have],
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
				       #\(' '),
				       #\(j),
				       #\(o),
				       #\(b),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h)
				     ])
			  ]
			]
		      ],
		      
		      [ u_gs_string_write,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(a), #\(t)])
		      ],
		      
		      [ u_gen,
			[u_ob_c36_get, u_con, [quote, u_organization]],
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:56971 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    people,
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [ywhile, lst],
			      
			      [ ydo,
				
				[ if,
				  ['ty$instance?', [car, lst], [quote, person]],
				  [setq, result, [cons, [car, lst], result]]
				],
				[setq, lst, [cdr, lst]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::PEOPLE 
wl: lambda_def(defun,
	      u_people,
	      f_u_people,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_ywhile, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_ty_c36_instance_c63, [car, u_lst], [quote, u_person]],
		      [setq, u_result, [cons, [car, u_lst], u_result]]
		    ],
		    [setq, u_lst, [cdr, u_lst]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::PEOPLE 
wl: arglist_info(u_people,
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

% annotating U::PEOPLE 
wl: init_args(exact_only, u_people).


% annotating U::PEOPLE 
f_u_people(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ty_c36_instance_c63, [car, u_lst], [quote, u_person]],
			[setq, u_result, [cons, [car, u_lst], u_result]]
		      ],
		      [setq, u_lst, [cdr, u_lst]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_people, classof, claz_function),
   set_opv(u_people, compile_as, kw_function),
   set_opv(u_people, function, f_u_people),
   DefunResult=u_people.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57211 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'people-or-orgs',
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [ywhile, lst],
			      
			      [ ydo,
				
				[ if,
				  
				  [ or,
				    ['ty$instance?', [car, lst], [quote, person]],
				    
				    [ 'ty$instance?',
				      [car, lst],
				      [quote, organization]
				    ]
				  ],
				  [setq, result, [cons, [car, lst], result]]
				],
				[setq, lst, [cdr, lst]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::PEOPLE-OR-ORGS 
wl: lambda_def(defun,
	      u_people_or_orgs,
	      f_u_people_or_orgs,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_ywhile, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ or,
			[u_ty_c36_instance_c63, [car, u_lst], [quote, u_person]],
			
			[ u_ty_c36_instance_c63,
			  [car, u_lst],
			  [quote, u_organization]
			]
		      ],
		      [setq, u_result, [cons, [car, u_lst], u_result]]
		    ],
		    [setq, u_lst, [cdr, u_lst]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::PEOPLE-OR-ORGS 
wl: arglist_info(u_people_or_orgs,
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

% annotating U::PEOPLE-OR-ORGS 
wl: init_args(exact_only, u_people_or_orgs).


% annotating U::PEOPLE-OR-ORGS 
f_u_people_or_orgs(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ or,
			  [u_ty_c36_instance_c63, [car, u_lst], [quote, u_person]],
			  
			  [ u_ty_c36_instance_c63,
			    [car, u_lst],
			    [quote, u_organization]
			  ]
			],
			[setq, u_result, [cons, [car, u_lst], u_result]]
		      ],
		      [setq, u_lst, [cdr, u_lst]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_people_or_orgs, classof, claz_function),
   set_opv(u_people_or_orgs, compile_as, kw_function),
   set_opv(u_people_or_orgs, function, f_u_people_or_orgs),
   DefunResult=u_people_or_orgs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57523 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'AT',
			    [],
			    
			    [ 'gen-regular-plus',
			      con,
			      stream,
			      switches,
			      context,
			      bp,
			      [quote, be],
			      '$STRING'(" at"),
			      ['ob$get', con, [quote, obj]]
			    ]
			  ]).
:- f_u_define_gen(u_at,
		  [],
		  
		  [ 
		    [ u_gen_regular_plus,
		      u_con,
		      stream,
		      u_switches,
		      u_context,
		      u_bp,
		      [quote, u_be],
		      '$ARRAY'([*], claz_base_character, [#\(' '), #\(a), #\(t)]),
		      [u_ob_c36_get, u_con, [quote, u_obj]]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57644 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'LOCATION',
			    [],
			    
			    [ let,
			      [[name, ['ob$get', con, [quote, name]]]],
			      
			      [ if,
				name,
				
				[ 'gs-string-write',
				  stream,
				  ['string-append', '$STRING'(" "), name]
				],
				
				[ 'gs-string-write',
				  stream,
				  '$STRING'(" someplace")
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_location,
		  [],
		  
		  [ 
		    [ let,
		      [[sys_name, [u_ob_c36_get, u_con, [quote, sys_name]]]],
		      
		      [ if,
			sys_name,
			
			[ u_gs_string_write,
			  stream,
			  
			  [ u_string_append,
			    '$ARRAY'([*], claz_base_character, [#\(' ')]),
			    sys_name
			  ]
			],
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(' '),
				     #\(s),
				     #\(o),
				     #\(m),
				     #\(e),
				     #\(p),
				     #\(l),
				     #\(a),
				     #\(c),
				     #\(e)
				   ])
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'RANGER-STATION',
			    [],
			    
			    [ let,
			      [[name, ['ob$get', con, [quote, name]]]],
			      
			      [ if,
				name,
				
				[ 'gs-string-write',
				  stream,
				  ['string-append', '$STRING'(" "), name]
				],
				
				[ 'gs-string-write',
				  stream,
				  '$STRING'(" the ranger station")
				]
			      ],
			      t
			    ]
			  ]).
:- f_u_define_gen(u_ranger_station,
		  [],
		  
		  [ 
		    [ let,
		      [[sys_name, [u_ob_c36_get, u_con, [quote, sys_name]]]],
		      
		      [ if,
			sys_name,
			
			[ u_gs_string_write,
			  stream,
			  
			  [ u_string_append,
			    '$ARRAY'([*], claz_base_character, [#\(' ')]),
			    sys_name
			  ]
			],
			
			[ u_gs_string_write,
			  stream,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(' '),
				     #\(t),
				     #\(h),
				     #\(e),
				     #\(' '),
				     #\(r),
				     #\(a),
				     #\(n),
				     #\(g),
				     #\(e),
				     #\(r),
				     #\(' '),
				     #\(s),
				     #\(t),
				     #\(a),
				     #\(t),
				     #\(i),
				     #\(o),
				     #\(n)
				   ])
			]
		      ],
		      t
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("(define-gen PROX nil", 1, 58056)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (let ((subject (ob$gets con 'actor)))",
				     1,
				     58078)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    (if (> (length subject) 1)", 1, 58119)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        (progn", 1, 58151)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (gen-subject subject stream switches context bp)",
				     1,
				     58167)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (gen-verb 'be subject stream switches (neg? con))",
				     1,
				     58226)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (gs-string-write stream \" near each other\"))",
				     1,
				     58286)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        (progn", 1, 58341)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (gen subject stream (cons '(case possessive) switches context bp))",
				     1,
				     58357)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:57853 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("         (gs-string-write stream \" location\")))))",
				     1,
				     58434)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:58485 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'VPROX',
			    [],
			    
			    [ let,
			      [[subject, ['ob$gets', con, [quote, actor]]]],
			      
			      [ cond,
				
				[ 
				  [ 'any?',
				    
				    [ lambda,
				      [x],
				      [not, ['ty$instance?', x, [quote, person]]]
				    ],
				    subject
				  ],
				  
				  [ let,
				    [[subj, []], ['virtual-me', [car, bp]]],
				    
				    [ if,
				      ['memq?', 'virtual-me', subject],
				      
				      [ progn,
					[setq, subj, [list, 'virtual-me']],
					
					[ 'gen-subject',
					  subj,
					  stream,
					  switches,
					  context,
					  bp
					],
					
					[ 'gen-verb',
					  [quote, be],
					  subj,
					  stream,
					  switches,
					  ['neg?', con]
					],
					
					[ 'gs-string-write',
					  stream,
					  '$STRING'(" near")
					],
					
					[ gen,
					  ['non-persons', subject, 'virtual-me'],
					  stream,
					  switches,
					  context,
					  bp
					]
				      ],
				      
				      [ progn,
					[setq, subj, subject],
					
					[ 'gen-subject',
					  subject,
					  stream,
					  switches,
					  context,
					  bp
					],
					
					[ 'gen-verb',
					  [quote, be],
					  subject,
					  stream,
					  switches,
					  ['neg?', con]
					],
					
					[ 'gs-string-write',
					  stream,
					  '$STRING'(" near")
					],
					
					[ if,
					  [=, [length, subject], 1],
					  
					  [ if,
					    ['neg?', con],
					    
					    [ 'gs-string-write',
					      stream,
					      '$STRING'(" anything")
					    ],
					    
					    [ 'gs-string-write',
					      stream,
					      '$STRING'(" something")
					    ]
					  ]
					]
				      ]
				    ],
				    subj
				  ]
				],
				
				[ [>, [length, subject], 1],
				  
				  [ if,
				    
				    [ 'switches-lookup',
				      [quote, 'active-goal?'],
				      switches
				    ],
				    
				    [ 'gen-split-subject',
				      con,
				      stream,
				      switches,
				      context,
				      bp,
				      [quote, get],
				      '$STRING'(" in touch with")
				    ],
				    
				    [ 'gen-split-subject',
				      con,
				      stream,
				      switches,
				      context,
				      bp,
				      [quote, be],
				      '$STRING'(" in touch with")
				    ]
				  ]
				],
				
				[ else,
				  
				  [ gen,
				    subject,
				    stream,
				    
				    [ cons,
				      [quote, [case, possessive]],
				      switches,
				      context,
				      bp
				    ]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" location")
				  ],
				  t
				]
			      ]
			    ]
			  ]).
:- f_u_define_gen(u_vprox,
		  [],
		  
		  [ 
		    [ let,
		      [[u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]]],
		      
		      [ cond,
			
			[ 
			  [ u_any_c63,
			    
			    [ lambda,
			      [u_x],
			      
			      [ not,
				[u_ty_c36_instance_c63, u_x, [quote, u_person]]
			      ]
			    ],
			    u_subject
			  ],
			  
			  [ let,
			    [[u_subj, []], [u_virtual_me, [car, u_bp]]],
			    
			    [ if,
			      [u_memq_c63, u_virtual_me, u_subject],
			      
			      [ progn,
				[setq, u_subj, [list, u_virtual_me]],
				
				[ u_gen_subject,
				  u_subj,
				  stream,
				  u_switches,
				  u_context,
				  u_bp
				],
				
				[ u_gen_verb,
				  [quote, u_be],
				  u_subj,
				  stream,
				  u_switches,
				  [u_neg_c63, u_con]
				],
				
				[ u_gs_string_write,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\(' '), #\(n), #\(e), #\(a), #\(r)])
				],
				
				[ u_gen,
				  [u_non_persons, u_subject, u_virtual_me],
				  stream,
				  u_switches,
				  u_context,
				  u_bp
				]
			      ],
			      
			      [ progn,
				[setq, u_subj, u_subject],
				
				[ u_gen_subject,
				  u_subject,
				  stream,
				  u_switches,
				  u_context,
				  u_bp
				],
				
				[ u_gen_verb,
				  [quote, u_be],
				  u_subject,
				  stream,
				  u_switches,
				  [u_neg_c63, u_con]
				],
				
				[ u_gs_string_write,
				  stream,
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\(' '), #\(n), #\(e), #\(a), #\(r)])
				],
				
				[ if,
				  [=, [length, u_subject], 1],
				  
				  [ if,
				    [u_neg_c63, u_con],
				    
				    [ u_gs_string_write,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       
					       [ #\(' '),
						 #\(a),
						 #\(n),
						 #\(y),
						 #\(t),
						 #\(h),
						 #\(i),
						 #\(n),
						 #\(g)
					       ])
				    ],
				    
				    [ u_gs_string_write,
				      stream,
				      '$ARRAY'([*],
					       claz_base_character,
					       
					       [ #\(' '),
						 #\(s),
						 #\(o),
						 #\(m),
						 #\(e),
						 #\(t),
						 #\(h),
						 #\(i),
						 #\(n),
						 #\(g)
					       ])
				    ]
				  ]
				]
			      ]
			    ],
			    u_subj
			  ]
			],
			
			[ [>, [length, u_subject], 1],
			  
			  [ if,
			    
			    [ u_switches_lookup,
			      [quote, u_active_goal_c63],
			      u_switches
			    ],
			    
			    [ u_gen_split_subject,
			      u_con,
			      stream,
			      u_switches,
			      u_context,
			      u_bp,
			      [quote, get],
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(' '),
					 #\(i),
					 #\(n),
					 #\(' '),
					 #\(t),
					 #\(o),
					 #\(u),
					 #\(c),
					 #\(h),
					 #\(' '),
					 #\(w),
					 #\(i),
					 #\(t),
					 #\(h)
				       ])
			    ],
			    
			    [ u_gen_split_subject,
			      u_con,
			      stream,
			      u_switches,
			      u_context,
			      u_bp,
			      [quote, u_be],
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\(' '),
					 #\(i),
					 #\(n),
					 #\(' '),
					 #\(t),
					 #\(o),
					 #\(u),
					 #\(c),
					 #\(h),
					 #\(' '),
					 #\(w),
					 #\(i),
					 #\(t),
					 #\(h)
				       ])
			    ]
			  ]
			],
			
			[ u_else,
			  
			  [ u_gen,
			    u_subject,
			    stream,
			    
			    [ cons,
			      [quote, [case, u_possessive]],
			      u_switches,
			      u_context,
			      u_bp
			    ]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(l),
				       #\(o),
				       #\(c),
				       #\(a),
				       #\(t),
				       #\(i),
				       #\(o),
				       #\(n)
				     ])
			  ],
			  t
			]
		      ]
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:60085 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-split-subject',
			    
			    [ con,
			      stream,
			      switches,
			      context,
			      bp,
			      verb,
			      'prep-phrase'
			    ],
			    
			    [ let,
			      
			      [ [subject, ['ob$gets', con, [quote, actor]]],
				[subj, []],
				['virtual-me', [car, bp]],
				['non-virtual-mes', []]
			      ],
			      
			      [ if,
				['memq?', 'virtual-me', subject],
				
				[ progn,
				  [setq, subj, [list, 'virtual-me']],
				  
				  [ 'gen-subject',
				    subj,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    verb,
				    subj,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  ['gs-string-write', stream, 'prep-phrase'],
				  
				  [ setq,
				    'non-virtual-mes',
				    ['non-persons', subject, 'virtual-me']
				  ],
				  
				  [ if,
				    'non-virtual-mes',
				    
				    [ gen,
				      'non-virtual-mes',
				      stream,
				      switches,
				      context,
				      bp
				    ],
				    
				    [ if,
				      ['neg?', con],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" anyone")
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" someone")
				      ]
				    ]
				  ]
				],
				
				[ progn,
				  [setq, subj, subject],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    verb,
				    subj,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  ['gs-string-write', stream, 'prep-phrase'],
				  
				  [ if,
				    [=, [length, subject], 1],
				    
				    [ if,
				      ['neg?', con],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" anyone")
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" someone")
				      ]
				    ]
				  ]
				]
			      ],
			      subj
			    ]
			  ]).

% annotating U::GEN-SPLIT-SUBJECT 
wl: lambda_def(defun,
	      u_gen_split_subject,
	      f_u_gen_split_subject,
	      [u_con, stream, u_switches, u_context, u_bp, u_verb, u_prep_phrase],
	      
	      [ 
		[ let,
		  
		  [ [u_subject, [u_ob_c36_gets, u_con, [quote, u_actor]]],
		    [u_subj, []],
		    [u_virtual_me, [car, u_bp]],
		    [u_non_virtual_mes, []]
		  ],
		  
		  [ if,
		    [u_memq_c63, u_virtual_me, u_subject],
		    
		    [ progn,
		      [setq, u_subj, [list, u_virtual_me]],
		      [u_gen_subject, u_subj, stream, u_switches, u_context, u_bp],
		      
		      [ u_gen_verb,
			u_verb,
			u_subj,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      [u_gs_string_write, stream, u_prep_phrase],
		      
		      [ setq,
			u_non_virtual_mes,
			[u_non_persons, u_subject, u_virtual_me]
		      ],
		      
		      [ if,
			u_non_virtual_mes,
			
			[ u_gen,
			  u_non_virtual_mes,
			  stream,
			  u_switches,
			  u_context,
			  u_bp
			],
			
			[ if,
			  [u_neg_c63, u_con],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
				       #\(n),
				       #\(y),
				       #\(o),
				       #\(n),
				       #\(e)
				     ])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(s),
				       #\(o),
				       #\(m),
				       #\(e),
				       #\(o),
				       #\(n),
				       #\(e)
				     ])
			  ]
			]
		      ]
		    ],
		    
		    [ progn,
		      [setq, u_subj, u_subject],
		      
		      [ u_gen_subject,
			u_subject,
			stream,
			u_switches,
			u_context,
			u_bp
		      ],
		      
		      [ u_gen_verb,
			u_verb,
			u_subj,
			stream,
			u_switches,
			[u_neg_c63, u_con]
		      ],
		      [u_gs_string_write, stream, u_prep_phrase],
		      
		      [ if,
			[=, [length, u_subject], 1],
			
			[ if,
			  [u_neg_c63, u_con],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(a),
				       #\(n),
				       #\(y),
				       #\(o),
				       #\(n),
				       #\(e)
				     ])
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(s),
				       #\(o),
				       #\(m),
				       #\(e),
				       #\(o),
				       #\(n),
				       #\(e)
				     ])
			  ]
			]
		      ]
		    ]
		  ],
		  u_subj
		]
	      ]).


% annotating U::GEN-SPLIT-SUBJECT 
wl: arglist_info(u_gen_split_subject,
		[u_con, stream, u_switches, u_context, u_bp, u_verb, u_prep_phrase],
		
		[ Con_Param,
		  Stream_Param,
		  Switches_Param,
		  Context_Param,
		  Bp_Param,
		  Verb_Param,
		  Prep_phrase_Param
		],
		arginfo{ all:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_prep_phrase
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_con,
				 stream,
				 u_switches,
				 u_context,
				 u_bp,
				 u_verb,
				 u_prep_phrase
			       ],
			 opt:0,
			 req:
			     [ u_con,
			       stream,
			       u_switches,
			       u_context,
			       u_bp,
			       u_verb,
			       u_prep_phrase
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-SPLIT-SUBJECT 
wl: init_args(exact_only, u_gen_split_subject).


% annotating U::GEN-SPLIT-SUBJECT 
f_u_gen_split_subject(Con_Param, Stream_Param, Switches_Param, Context_Param, Bp_Param, Verb_Param, Prep_phrase_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_context, Context_Param), bv(u_bp, Bp_Param), bv(u_verb, Verb_Param), bv(u_prep_phrase, Prep_phrase_Param)],
	f_u_ob_c36_gets(Con_Param, u_actor, Subject_Init),
	cl_car(Bp_Param, Virtual_me_Init),
	LEnv=[[bv(u_subject, Subject_Init), bv(u_subj, []), bv(u_virtual_me, Virtual_me_Init), bv(u_non_virtual_mes, [])]|Env],
	f_u_memq_c63(u_virtual_me, u_subject, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_virtual_me, Virtual_me_Get),
	    Subj=[Virtual_me_Get],
	    set_var(LEnv, u_subj, Subj),
	    get_var(LEnv, u_subj, Subj_Get40),
	    f_u_gen_subject(Subj_Get40,
			    Stream_Param,
			    Switches_Param,
			    Context_Param,
			    Bp_Param,
			    Gen_subject_Ret),
	    f_u_neg_c63(Con_Param, Neg_c63_Ret),
	    f_u_gen_verb(Verb_Param,
			 Subj_Get40,
			 Stream_Param,
			 Switches_Param,
			 Neg_c63_Ret,
			 Gen_verb_Ret),
	    f_u_gs_string_write(Stream_Param,
				Prep_phrase_Param,
				String_write_Ret),
	    get_var(LEnv, u_subject, Subject_Get),
	    get_var(LEnv, u_virtual_me, Virtual_me_Get47),
	    f_u_non_persons(Subject_Get, Virtual_me_Get47, Non_virtual_mes),
	    set_var(LEnv, u_non_virtual_mes, Non_virtual_mes),
	    get_var(LEnv, u_non_virtual_mes, IFTEST48),
	    (   IFTEST48\==[]
	    ->  f_u_gen(IFTEST48,
			Stream_Param,
			Switches_Param,
			Context_Param,
			Bp_Param,
			TrueResult63),
		ElseResult64=TrueResult63
	    ;   f_u_neg_c63(Con_Param, IFTEST56),
		(   IFTEST56\==[]
		->  f_u_gs_string_write(Stream_Param,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(' '),
						   #\(a),
						   #\(n),
						   #\(y),
						   #\(o),
						   #\(n),
						   #\(e)
						 ]),
					TrueResult),
		    ElseResult64=TrueResult
		;   f_u_gs_string_write(Stream_Param,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(' '),
						   #\(s),
						   #\(o),
						   #\(m),
						   #\(e),
						   #\(o),
						   #\(n),
						   #\(e)
						 ]),
					ElseResult),
		    ElseResult64=ElseResult
		)
	    )
	;   get_var(LEnv, u_subject, Subject_Get65),
	    set_var(LEnv, u_subj, Subject_Get65),
	    get_var(LEnv, u_subject, Subject_Get66),
	    f_u_gen_subject(Subject_Get66,
			    Stream_Param,
			    Switches_Param,
			    Context_Param,
			    Bp_Param,
			    Gen_subject_Ret101),
	    get_var(LEnv, u_subj, Subj_Get72),
	    f_u_neg_c63(Con_Param, Neg_c63_Ret102),
	    f_u_gen_verb(Verb_Param,
			 Subj_Get72,
			 Stream_Param,
			 Switches_Param,
			 Neg_c63_Ret102,
			 Gen_verb_Ret103),
	    f_u_gs_string_write(Stream_Param,
				Prep_phrase_Param,
				String_write_Ret104),
	    get_var(LEnv, u_subject, Subject_Get79),
	    cl_length(Subject_Get79, PredArg1Result),
	    (   PredArg1Result=:=1
	    ->  f_u_neg_c63(Con_Param, IFTEST82),
		(   IFTEST82\==[]
		->  f_u_gs_string_write(Stream_Param,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(' '),
						   #\(a),
						   #\(n),
						   #\(y),
						   #\(o),
						   #\(n),
						   #\(e)
						 ]),
					TrueResult87),
		    ElseResult64=TrueResult87
		;   f_u_gs_string_write(Stream_Param,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(' '),
						   #\(s),
						   #\(o),
						   #\(m),
						   #\(e),
						   #\(o),
						   #\(n),
						   #\(e)
						 ]),
					ElseResult88),
		    ElseResult64=ElseResult88
		)
	    ;   ElseResult64=[]
	    )
	),
	get_var(LEnv, u_subj, Subj_Get92),
	LetResult=Subj_Get92,
	LetResult=FnResult.
:- set_opv(f_u_gen_split_subject, classof, claz_function),
   set_opv(u_gen_split_subject, compile_as, kw_function),
   set_opv(u_gen_split_subject, function, f_u_gen_split_subject),
   DefunResult=u_gen_split_subject.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:61207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'define-gen',
			    'MTRANS',
			    [],
			    
			    [ let,
			      
			      [ [subject, ['ob$get', con, [quote, actor]]],
				[from, ['ob$get', con, [quote, from]]],
				[to, ['ob$get', con, [quote, to]]],
				[obj, ['ob$get', con, [quote, obj]]],
				
				[ bd,
				  
				  [ 'ob$unify',
				    '*pos-rel-mtrans*',
				    con,
				    '*empty-bd*'
				  ]
				],
				[rel, []]
			      ],
			      
			      [ cond,
				
				[ bd,
				  
				  [ setq,
				    rel,
				    
				    [ 'bd-lookup',
				      [quote, 'pos-relationship'],
				      bd
				    ]
				  ],
				  
				  [ cond,
				    
				    [ ['ty$instance?', rel, [quote, lovers]],
				      
				      [ 'gen-subject',
					['bd-lookup', [quote, person1], bd],
					stream,
					switches,
					context,
					bp
				      ],
				      
				      [ 'gen-verb',
					[quote, ask],
					subject,
					stream,
					switches,
					['neg?', con]
				      ],
				      
				      [ gen,
					['bd-lookup', [quote, person2], bd],
					stream,
					switches,
					context,
					bp
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" out")
				      ]
				    ],
				    
				    [ ['ty$instance?', rel, [quote, employment]],
				      
				      [ 'gen-subject',
					['bd-lookup', [quote, person1], bd],
					stream,
					switches,
					context,
					bp
				      ],
				      
				      [ 'gen-verb',
					[quote, offer],
					subject,
					stream,
					switches,
					['neg?', con]
				      ],
				      
				      [ gen,
					['bd-lookup', [quote, person2], bd],
					stream,
					switches,
					context,
					bp
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" a job")
				      ]
				    ]
				  ]
				],
				
				[ 
				  [ setq,
				    bd,
				    
				    [ 'ob$unify',
				      '*neg-rel-mtrans*',
				      con,
				      '*empty-bd*'
				    ]
				  ],
				  
				  [ setq,
				    rel,
				    
				    [ 'bd-lookup',
				      [quote, 'pos-relationship'],
				      bd
				    ]
				  ],
				  
				  [ cond,
				    
				    [ ['null?', rel],
				      
				      [ 'gen-subject',
					['bd-lookup', [quote, person1], bd],
					stream,
					switches,
					context,
					bp
				      ],
				      
				      [ 'gen-verb',
					[quote, decline],
					subject,
					stream,
					switches,
					['neg?', con]
				      ]
				    ],
				    
				    [ ['ty$instance?', rel, [quote, lovers]],
				      
				      [ if,
					
					[ 'existing-relationship?',
					  rel,
					  con,
					  context
					],
					
					[ progn,
					  
					  [ 'gen-subject',
					    ['bd-lookup', [quote, person1], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ],
					  
					  [ 'gen-verb',
					    [quote, dump],
					    subject,
					    stream,
					    switches,
					    ['neg?', con]
					  ],
					  
					  [ gen,
					    ['bd-lookup', [quote, person2], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ]
					],
					
					[ progn,
					  
					  [ 'gen-subject',
					    ['bd-lookup', [quote, person1], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ],
					  
					  [ 'gen-verb',
					    [quote, turn],
					    subject,
					    stream,
					    switches,
					    ['neg?', con]
					  ],
					  
					  [ gen,
					    ['bd-lookup', [quote, person2], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" down")
					  ]
					]
				      ]
				    ],
				    
				    [ ['ty$instance?', rel, [quote, employment]],
				      
				      [ if,
					
					[ 'existing-relationship?',
					  rel,
					  con,
					  context
					],
					
					[ progn,
					  
					  [ 'gen-subject',
					    ['bd-lookup', [quote, person1], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ],
					  
					  [ 'gen-verb',
					    [quote, fire],
					    subject,
					    stream,
					    switches,
					    ['neg?', con]
					  ],
					  
					  [ gen,
					    ['bd-lookup', [quote, person2], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ]
					],
					
					[ progn,
					  
					  [ 'gen-subject',
					    ['bd-lookup', [quote, person1], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ],
					  
					  [ 'gen-verb',
					    [quote, pass],
					    subject,
					    stream,
					    switches,
					    ['neg?', con]
					  ],
					  
					  [ gen,
					    ['bd-lookup', [quote, person2], bd],
					    stream,
					    switches,
					    context,
					    bp
					  ],
					  
					  [ 'gs-string-write',
					    stream,
					    '$STRING'(" up")
					  ]
					]
				      ]
				    ]
				  ]
				],
				
				[ ['ty$instance?', obj, [quote, introduction]],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, introduce],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ gen,
				    subject,
				    stream,
				    [cons, [quote, [case, reflexive]], switches],
				    context,
				    bp
				  ],
				  ['gs-string-write', stream, '$STRING'(" to")],
				  [gen, to, stream, switches, context, bp]
				],
				
				[ ['ty$instance?', obj, [quote, movie]],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, watch],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  [gen, obj, stream, switches, context, bp],
				  
				  [ if,
				    [and, from, [not, ['var?', from]]],
				    
				    [ progn,
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" at")
				      ],
				      [gen, from, stream, switches, context, bp]
				    ]
				  ]
				],
				
				[ 
				  [ or,
				    ['eq?', subject, from],
				    
				    [ and,
				      ['var?', subject],
				      ['var?', from],
				      
				      [ 'eq?',
					['variable-name', subject],
					['variable-name', from]
				      ]
				    ]
				  ],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, tell],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  [gen, to, stream, switches, context, bp],
				  
				  [ 'gen-that',
				    obj,
				    stream,
				    [cons, [quote, [mtrans, t]], switches],
				    context,
				    bp
				  ]
				],
				
				[ 
				  [ and,
				    ['eq?', subject, to],
				    [not, ['ty$instance?', from, [quote, person]]],
				    ['ty$instance?', from, [quote, object]]
				  ],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ if,
				    ['ty$instance?', obj, [quote, telno]],
				    
				    [ progn,
				      
				      [ 'gen-verb',
					[quote, look],
					subject,
					stream,
					switches,
					['neg?', con]
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" up")
				      ]
				    ],
				    
				    [ 'gen-verb',
				      [quote, read],
				      subject,
				      stream,
				      switches,
				      ['neg?', con]
				    ]
				  ],
				  ['gen-that', obj, stream, switches, context, bp],
				  ['gs-string-write', stream, '$STRING'(" in")],
				  [gen, from, stream, switches, context, bp]
				],
				
				[ 
				  [ and,
				    ['neq?', subject, to],
				    [not, ['ty$instance?', from, [quote, person]]],
				    ['ty$instance?', from, [quote, object]]
				  ],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ if,
				    ['ty$instance?', obj, [quote, telno]],
				    should,
				    be,
				    'knowable?',
				    or,
				    'personal-attribute?',
				    
				    [ progn,
				      
				      [ 'gen-verb',
					[quote, look],
					subject,
					stream,
					switches,
					['neg?', con]
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" up")
				      ]
				    ],
				    
				    [ 'gen-verb',
				      [quote, read],
				      subject,
				      stream,
				      switches,
				      ['neg?', con]
				    ]
				  ],
				  ['gen-that', obj, stream, switches, context, bp],
				  ['gs-string-write', stream, '$STRING'(" in")],
				  [gen, from, stream, switches, context, bp]
				],
				
				[ 
				  [ and,
				    ['neq?', subject, to],
				    [not, ['ty$instance?', from, [quote, person]]],
				    ['ty$instance?', from, [quote, object]]
				  ],
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ if,
				    ['ty$instance?', obj, [quote, telno]],
				    
				    [ progn,
				      
				      [ 'gen-verb',
					[quote, look],
					subject,
					stream,
					switches,
					['neg?', con]
				      ],
				      
				      [ 'gs-string-write',
					stream,
					'$STRING'(" up")
				      ]
				    ],
				    
				    [ 'gen-verb',
				      [quote, read],
				      subject,
				      stream,
				      switches,
				      ['neg?', con]
				    ]
				  ],
				  ['gen-that', obj, stream, switches, context, bp],
				  ['gs-string-write', stream, '$STRING'(" in")],
				  [gen, from, stream, switches, context, bp],
				  ['gs-string-write', stream, '$STRING'(" and")],
				  
				  [ 'gen-verb',
				    [quote, tell],
				    subject,
				    stream,
				    
				    [ cons,
				      [quote, ['to-less-infinitive', t]],
				      switches
				    ],
				    ['neg?', con]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" it to")
				  ],
				  [gen, to, stream, switches, context, bp]
				],
				
				[ else,
				  
				  [ 'gen-subject',
				    subject,
				    stream,
				    switches,
				    context,
				    bp
				  ],
				  
				  [ 'gen-verb',
				    [quote, say],
				    subject,
				    stream,
				    switches,
				    ['neg?', con]
				  ],
				  
				  [ 'gs-string-write',
				    stream,
				    '$STRING'(" something")
				  ]
				]
			      ],
			      subject
			    ]
			  ]).
:- f_u_define_gen(u_mtrans,
		  [],
		  
		  [ 
		    [ let,
		      
		      [ [u_subject, [u_ob_c36_get, u_con, [quote, u_actor]]],
			[u_from, [u_ob_c36_get, u_con, [quote, u_from]]],
			[u_to, [u_ob_c36_get, u_con, [quote, u_to]]],
			[u_obj, [u_ob_c36_get, u_con, [quote, u_obj]]],
			
			[ u_bd,
			  
			  [ u_ob_c36_unify,
			    u_xx_pos_rel_mtrans_xx,
			    u_con,
			    u_xx_empty_bd_xx
			  ]
			],
			[u_rel, []]
		      ],
		      
		      [ cond,
			
			[ u_bd,
			  
			  [ setq,
			    u_rel,
			    [u_bd_lookup, [quote, u_pos_relationship], u_bd]
			  ],
			  
			  [ cond,
			    
			    [ [u_ty_c36_instance_c63, u_rel, [quote, u_lovers]],
			      
			      [ u_gen_subject,
				[u_bd_lookup, [quote, u_person1], u_bd],
				stream,
				u_switches,
				u_context,
				u_bp
			      ],
			      
			      [ u_gen_verb,
				[quote, u_ask],
				u_subject,
				stream,
				u_switches,
				[u_neg_c63, u_con]
			      ],
			      
			      [ u_gen,
				[u_bd_lookup, [quote, u_person2], u_bd],
				stream,
				u_switches,
				u_context,
				u_bp
			      ],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(o), #\(u), #\(t)])
			      ]
			    ],
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_rel,
				[quote, u_employment]
			      ],
			      
			      [ u_gen_subject,
				[u_bd_lookup, [quote, u_person1], u_bd],
				stream,
				u_switches,
				u_context,
				u_bp
			      ],
			      
			      [ u_gen_verb,
				[quote, u_offer],
				u_subject,
				stream,
				u_switches,
				[u_neg_c63, u_con]
			      ],
			      
			      [ u_gen,
				[u_bd_lookup, [quote, u_person2], u_bd],
				stream,
				u_switches,
				u_context,
				u_bp
			      ],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(' '),
					   #\(a),
					   #\(' '),
					   #\(j),
					   #\(o),
					   #\(b)
					 ])
			      ]
			    ]
			  ]
			],
			
			[ 
			  [ setq,
			    u_bd,
			    
			    [ u_ob_c36_unify,
			      u_xx_neg_rel_mtrans_xx,
			      u_con,
			      u_xx_empty_bd_xx
			    ]
			  ],
			  
			  [ setq,
			    u_rel,
			    [u_bd_lookup, [quote, u_pos_relationship], u_bd]
			  ],
			  
			  [ cond,
			    
			    [ [u_null_c63, u_rel],
			      
			      [ u_gen_subject,
				[u_bd_lookup, [quote, u_person1], u_bd],
				stream,
				u_switches,
				u_context,
				u_bp
			      ],
			      
			      [ u_gen_verb,
				[quote, u_decline],
				u_subject,
				stream,
				u_switches,
				[u_neg_c63, u_con]
			      ]
			    ],
			    
			    [ [u_ty_c36_instance_c63, u_rel, [quote, u_lovers]],
			      
			      [ if,
				
				[ u_existing_relationship_c63,
				  u_rel,
				  u_con,
				  u_context
				],
				
				[ progn,
				  
				  [ u_gen_subject,
				    [u_bd_lookup, [quote, u_person1], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ],
				  
				  [ u_gen_verb,
				    [quote, u_dump],
				    u_subject,
				    stream,
				    u_switches,
				    [u_neg_c63, u_con]
				  ],
				  
				  [ u_gen,
				    [u_bd_lookup, [quote, u_person2], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ]
				],
				
				[ progn,
				  
				  [ u_gen_subject,
				    [u_bd_lookup, [quote, u_person1], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ],
				  
				  [ u_gen_verb,
				    [quote, u_turn],
				    u_subject,
				    stream,
				    u_switches,
				    [u_neg_c63, u_con]
				  ],
				  
				  [ u_gen,
				    [u_bd_lookup, [quote, u_person2], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(d), #\(o), #\(w), #\(n)])
				  ]
				]
			      ]
			    ],
			    
			    [ 
			      [ u_ty_c36_instance_c63,
				u_rel,
				[quote, u_employment]
			      ],
			      
			      [ if,
				
				[ u_existing_relationship_c63,
				  u_rel,
				  u_con,
				  u_context
				],
				
				[ progn,
				  
				  [ u_gen_subject,
				    [u_bd_lookup, [quote, u_person1], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ],
				  
				  [ u_gen_verb,
				    [quote, u_fire],
				    u_subject,
				    stream,
				    u_switches,
				    [u_neg_c63, u_con]
				  ],
				  
				  [ u_gen,
				    [u_bd_lookup, [quote, u_person2], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ]
				],
				
				[ progn,
				  
				  [ u_gen_subject,
				    [u_bd_lookup, [quote, u_person1], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ],
				  
				  [ u_gen_verb,
				    [quote, u_pass],
				    u_subject,
				    stream,
				    u_switches,
				    [u_neg_c63, u_con]
				  ],
				  
				  [ u_gen,
				    [u_bd_lookup, [quote, u_person2], u_bd],
				    stream,
				    u_switches,
				    u_context,
				    u_bp
				  ],
				  
				  [ u_gs_string_write,
				    stream,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' '), #\(u), #\(p)])
				  ]
				]
			      ]
			    ]
			  ]
			],
			
			[ [u_ty_c36_instance_c63, u_obj, [quote, u_introduction]],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_introduce],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gen,
			    u_subject,
			    stream,
			    [cons, [quote, [case, u_reflexive]], u_switches],
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(t), #\(o)])
			  ],
			  [u_gen, u_to, stream, u_switches, u_context, u_bp]
			],
			
			[ [u_ty_c36_instance_c63, u_obj, [quote, u_movie]],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_watch],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  [u_gen, u_obj, stream, u_switches, u_context, u_bp],
			  
			  [ if,
			    [and, u_from, [not, [u_var_c63, u_from]]],
			    
			    [ progn,
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(a), #\(t)])
			      ],
			      [u_gen, u_from, stream, u_switches, u_context, u_bp]
			    ]
			  ]
			],
			
			[ 
			  [ or,
			    [u_eq_c63, u_subject, u_from],
			    
			    [ and,
			      [u_var_c63, u_subject],
			      [u_var_c63, u_from],
			      
			      [ u_eq_c63,
				[u_variable_name, u_subject],
				[u_variable_name, u_from]
			      ]
			    ]
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_tell],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  [u_gen, u_to, stream, u_switches, u_context, u_bp],
			  
			  [ u_gen_that,
			    u_obj,
			    stream,
			    [cons, [quote, [u_mtrans, t]], u_switches],
			    u_context,
			    u_bp
			  ]
			],
			
			[ 
			  [ and,
			    [u_eq_c63, u_subject, u_to],
			    
			    [ not,
			      [u_ty_c36_instance_c63, u_from, [quote, u_person]]
			    ],
			    [u_ty_c36_instance_c63, u_from, [quote, u_object]]
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_telno]],
			    
			    [ progn,
			      
			      [ u_gen_verb,
				[quote, u_look],
				u_subject,
				stream,
				u_switches,
				[u_neg_c63, u_con]
			      ],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(u), #\(p)])
			      ]
			    ],
			    
			    [ u_gen_verb,
			      [quote, read],
			      u_subject,
			      stream,
			      u_switches,
			      [u_neg_c63, u_con]
			    ]
			  ],
			  [u_gen_that, u_obj, stream, u_switches, u_context, u_bp],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(i), #\(n)])
			  ],
			  [u_gen, u_from, stream, u_switches, u_context, u_bp]
			],
			
			[ 
			  [ and,
			    [u_neq_c63, u_subject, u_to],
			    
			    [ not,
			      [u_ty_c36_instance_c63, u_from, [quote, u_person]]
			    ],
			    [u_ty_c36_instance_c63, u_from, [quote, u_object]]
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_telno]],
			    u_should,
			    u_be,
			    u_knowable_c63,
			    or,
			    u_personal_attribute_c63,
			    
			    [ progn,
			      
			      [ u_gen_verb,
				[quote, u_look],
				u_subject,
				stream,
				u_switches,
				[u_neg_c63, u_con]
			      ],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(u), #\(p)])
			      ]
			    ],
			    
			    [ u_gen_verb,
			      [quote, read],
			      u_subject,
			      stream,
			      u_switches,
			      [u_neg_c63, u_con]
			    ]
			  ],
			  [u_gen_that, u_obj, stream, u_switches, u_context, u_bp],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(i), #\(n)])
			  ],
			  [u_gen, u_from, stream, u_switches, u_context, u_bp]
			],
			
			[ 
			  [ and,
			    [u_neq_c63, u_subject, u_to],
			    
			    [ not,
			      [u_ty_c36_instance_c63, u_from, [quote, u_person]]
			    ],
			    [u_ty_c36_instance_c63, u_from, [quote, u_object]]
			  ],
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_obj, [quote, u_telno]],
			    
			    [ progn,
			      
			      [ u_gen_verb,
				[quote, u_look],
				u_subject,
				stream,
				u_switches,
				[u_neg_c63, u_con]
			      ],
			      
			      [ u_gs_string_write,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(u), #\(p)])
			      ]
			    ],
			    
			    [ u_gen_verb,
			      [quote, read],
			      u_subject,
			      stream,
			      u_switches,
			      [u_neg_c63, u_con]
			    ]
			  ],
			  [u_gen_that, u_obj, stream, u_switches, u_context, u_bp],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(i), #\(n)])
			  ],
			  [u_gen, u_from, stream, u_switches, u_context, u_bp],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(a), #\(n), #\(d)])
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_tell],
			    u_subject,
			    stream,
			    [cons, [quote, [u_to_less_infinitive, t]], u_switches],
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(i), #\(t), #\(' '), #\(t), #\(o)])
			  ],
			  [u_gen, u_to, stream, u_switches, u_context, u_bp]
			],
			
			[ u_else,
			  
			  [ u_gen_subject,
			    u_subject,
			    stream,
			    u_switches,
			    u_context,
			    u_bp
			  ],
			  
			  [ u_gen_verb,
			    [quote, u_say],
			    u_subject,
			    stream,
			    u_switches,
			    [u_neg_c63, u_con]
			  ],
			  
			  [ u_gs_string_write,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(' '),
				       #\(s),
				       #\(o),
				       #\(m),
				       #\(e),
				       #\(t),
				       #\(h),
				       #\(i),
				       #\(n),
				       #\(g)
				     ])
			  ]
			]
		      ],
		      u_subject
		    ]
		  ],
		  _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:61207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" should be knowable? or personal-attribute?",
				     36,
				     64742)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:61207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Did relationship exist at time of MTRANS?",
				     1,
				     66004)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:66608 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ if,
			    
			    [ and,
			      '*gen-thats*',
			      [not, ['ty$instance?', obj, [quote, telno]]]
			    ],
			    ['gs-string-write', stream, '$STRING'(" that")]
			  ]).
:- get_var(TLEnv3, u_xx_gen_thats_xx, IFTEST6),
   (   IFTEST6\==[]
   ->  get_var(TLEnv3, u_obj, Obj_Get),
       f_u_ty_c36_instance_c63(Obj_Get, u_telno, Telno),
       cl_not(Telno, TrueResult),
       IFTEST=TrueResult
   ;   IFTEST=[]
   ),
   (   IFTEST\==[]
   ->  get_var(TLEnv3, stream, Stream_Get),
       f_u_gs_string_write(Stream_Get,
			   '$ARRAY'([*],
				    claz_base_character,
				    [#\(' '), #\(t), #\(h), #\(a), #\(t)]),
			   TrueResult13),
       _Ignored=TrueResult13
   ;   _Ignored=[]
   ).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:66702 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ gen,
			    obj,
			    stream,
			    ['make-indicative', switches],
			    context,
			    bp
			  ]).
:- get_var(TLEnv3, stream, Stream_Get),
   get_var(TLEnv3, u_obj, Obj_Get),
   get_var(TLEnv3, u_switches, Switches_Get),
   f_u_make_indicative(Switches_Get, Make_indicative_Ret),
   get_var(TLEnv3, u_bp, Bp_Get),
   get_var(TLEnv3, u_context, Context_Get),
   f_u_gen(Obj_Get, Stream_Get, Make_indicative_Ret, Context_Get, Bp_Get, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:66756 **********************/
:- lisp_compile_to_prolog(pkg_user, ')').
:- get_var(TLEnv3, ')', C41_Get).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:66759 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-verb',
			    [verb, subject, stream, switches, 'negative?'],
			    
			    [ cond,
			      
			      [ 
				[ 'switches-lookup',
				  [quote, 'continue?'],
				  switches
				],
				
				[ 'gen-verb1',
				  [quote, continue],
				  subject,
				  stream,
				  switches,
				  'negative?'
				],
				
				[ 'gen-verb1',
				  verb,
				  subject,
				  stream,
				  [infinitivize, switches],
				  []
				]
			      ],
			      
			      [ else,
				
				[ 'gen-verb1',
				  verb,
				  subject,
				  stream,
				  switches,
				  'negative?'
				]
			      ]
			    ]
			  ]).

% annotating U::GEN-VERB 
wl: lambda_def(defun,
	      u_gen_verb,
	      f_u_gen_verb,
	      [u_verb, u_subject, stream, u_switches, u_negative_c63],
	      
	      [ 
		[ cond,
		  
		  [ [u_switches_lookup, [quote, u_continue_c63], u_switches],
		    
		    [ u_gen_verb1,
		      [quote, continue],
		      u_subject,
		      stream,
		      u_switches,
		      u_negative_c63
		    ],
		    
		    [ u_gen_verb1,
		      u_verb,
		      u_subject,
		      stream,
		      [u_infinitivize, u_switches],
		      []
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ u_gen_verb1,
		      u_verb,
		      u_subject,
		      stream,
		      u_switches,
		      u_negative_c63
		    ]
		  ]
		]
	      ]).


% annotating U::GEN-VERB 
wl: arglist_info(u_gen_verb,
		[u_verb, u_subject, stream, u_switches, u_negative_c63],
		
		[ Verb_Param,
		  Subject_Param,
		  Stream_Param,
		  Switches_Param,
		  Negative_c63_Param
		],
		arginfo{ all:
			     [ u_verb,
			       u_subject,
			       stream,
			       u_switches,
			       u_negative_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_verb,
				 u_subject,
				 stream,
				 u_switches,
				 u_negative_c63
			       ],
			 opt:0,
			 req:
			     [ u_verb,
			       u_subject,
			       stream,
			       u_switches,
			       u_negative_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-VERB 
wl: init_args(exact_only, u_gen_verb).


% annotating U::GEN-VERB 
f_u_gen_verb(Verb_Param, Subject_Param, Stream_Param, Switches_Param, Negative_c63_Param, ElseResult42) :-
	Env=[bv(u_verb, Verb_Param), bv(u_subject, Subject_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_negative_c63, Negative_c63_Param)],
	f_u_switches_lookup(u_continue_c63, Switches_Param, IFTEST),
	(   IFTEST\==[]
	->  f_u_gen_verb1(continue,
			  Subject_Param,
			  Stream_Param,
			  Switches_Param,
			  Negative_c63_Param,
			  Gen_verb1_Ret),
	    f_u_infinitivize(Switches_Param, Infinitivize_Ret),
	    f_u_gen_verb1(Verb_Param,
			  Subject_Param,
			  Stream_Param,
			  Infinitivize_Ret,
			  [],
			  TrueResult41),
	    ElseResult42=TrueResult41
	;   get_var(Env, u_else, IFTEST31),
	    (   IFTEST31\==[]
	    ->  f_u_gen_verb1(Verb_Param,
			      Subject_Param,
			      Stream_Param,
			      Switches_Param,
			      Negative_c63_Param,
			      TrueResult),
		ElseResult42=TrueResult
	    ;   ElseResult42=[]
	    )
	).
:- set_opv(f_u_gen_verb, classof, claz_function),
   set_opv(u_gen_verb, compile_as, kw_function),
   set_opv(u_gen_verb, function, f_u_gen_verb),
   DefunResult=u_gen_verb.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:67055 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gen-verb1',
			    [verb, subject, stream, switches, 'negative?'],
			    
			    [ 'gs-string-write',
			      stream,
			      
			      [ 'string-append',
				'$STRING'(" "),
				
				[ 'get-verb-form',
				  verb,
				  
				  [ or,
				    
				    [ 'switches-lookup',
				      [quote, number],
				      switches
				    ],
				    [cadr, ['get-number', subject]]
				  ],
				  ['switches-lookup', [quote, tense], switches],
				  'negative?',
				  switches
				]
			      ]
			    ]
			  ]).

% annotating U::GEN-VERB1 
wl: lambda_def(defun,
	      u_gen_verb1,
	      f_u_gen_verb1,
	      [u_verb, u_subject, stream, u_switches, u_negative_c63],
	      
	      [ 
		[ u_gs_string_write,
		  stream,
		  
		  [ u_string_append,
		    '$ARRAY'([*], claz_base_character, [#\(' ')]),
		    
		    [ u_get_verb_form,
		      u_verb,
		      
		      [ or,
			[u_switches_lookup, [quote, number], u_switches],
			[cadr, [u_get_number, u_subject]]
		      ],
		      [u_switches_lookup, [quote, u_tense], u_switches],
		      u_negative_c63,
		      u_switches
		    ]
		  ]
		]
	      ]).


% annotating U::GEN-VERB1 
wl: arglist_info(u_gen_verb1,
		[u_verb, u_subject, stream, u_switches, u_negative_c63],
		
		[ Verb_Param,
		  Subject_Param,
		  Stream_Param,
		  Switches_Param,
		  Negative_c63_Param
		],
		arginfo{ all:
			     [ u_verb,
			       u_subject,
			       stream,
			       u_switches,
			       u_negative_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_verb,
				 u_subject,
				 stream,
				 u_switches,
				 u_negative_c63
			       ],
			 opt:0,
			 req:
			     [ u_verb,
			       u_subject,
			       stream,
			       u_switches,
			       u_negative_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-VERB1 
wl: init_args(exact_only, u_gen_verb1).


% annotating U::GEN-VERB1 
f_u_gen_verb1(Verb_Param, Subject_Param, Stream_Param, Switches_Param, Negative_c63_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param), bv(u_subject, Subject_Param), bv(stream, Stream_Param), bv(u_switches, Switches_Param), bv(u_negative_c63, Negative_c63_Param)],
	f_u_string_append(
			  [ '$ARRAY'([*], claz_base_character, [#\(' ')]),
			    
			    [ u_get_verb_form,
			      u_verb,
			      
			      [ or,
				[u_switches_lookup, [quote, number], u_switches],
				[cadr, [u_get_number, u_subject]]
			      ],
			      [u_switches_lookup, [quote, u_tense], u_switches],
			      u_negative_c63,
			      u_switches
			    ]
			  ],
			  String_append_Ret),
	f_u_gs_string_write(Stream_Param, String_append_Ret, String_write_Ret),
	String_write_Ret=FnResult.
:- set_opv(f_u_gen_verb1, classof, claz_function),
   set_opv(u_gen_verb1, compile_as, kw_function),
   set_opv(u_gen_verb1, function, f_u_gen_verb1),
   DefunResult=u_gen_verb1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:67515 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'switches-lookup',
			    [item, switches],
			    [cadr, [assq, item, switches]]
			  ]).

% annotating U::SWITCHES-LOOKUP 
wl: lambda_def(defun,
	      u_switches_lookup,
	      f_u_switches_lookup,
	      [item, u_switches],
	      [[cadr, [ext_assq, item, u_switches]]]).


% annotating U::SWITCHES-LOOKUP 
wl: arglist_info(u_switches_lookup,
		[item, u_switches],
		[Item_Param, Switches_Param],
		arginfo{ all:[item, u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[item, u_switches],
			 opt:0,
			 req:[item, u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SWITCHES-LOOKUP 
wl: init_args(exact_only, u_switches_lookup).


% annotating U::SWITCHES-LOOKUP 
f_u_switches_lookup(Item_Param, Switches_Param, FnResult) :-
	Env=[bv(item, Item_Param), bv(u_switches, Switches_Param)],
	f_ext_assq(item, u_switches, Switches),
	cl_cadr(Switches, Cadr_Ret),
	Cadr_Ret=FnResult.
:- set_opv(f_u_switches_lookup, classof, claz_function),
   set_opv(u_switches_lookup, compile_as, kw_function),
   set_opv(u_switches_lookup, function, f_u_switches_lookup),
   DefunResult=u_switches_lookup.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:67586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-number',
			    [con],
			    
			    [ if,
			      ['pair?', con],
			      
			      [ if,
				[=, [length, con], 1],
				['get-number', [car, con]],
				[quote, [number, plural]]
			      ],
			      
			      [ if,
				['eq?', con, '*me-ob*'],
				[quote, [number, 'first-person-singular']],
				[quote, [number, 'third-person-singular']]
			      ]
			    ]
			  ]).

% annotating U::GET-NUMBER 
wl: lambda_def(defun,
	      u_get_number,
	      f_u_get_number,
	      [u_con],
	      
	      [ 
		[ if,
		  [u_pair_c63, u_con],
		  
		  [ if,
		    [=, [length, u_con], 1],
		    [u_get_number, [car, u_con]],
		    [quote, [number, u_plural]]
		  ],
		  
		  [ if,
		    [u_eq_c63, u_con, u_xx_me_ob_xx],
		    [quote, [number, u_first_person_singular]],
		    [quote, [number, u_third_person_singular]]
		  ]
		]
	      ]).


% annotating U::GET-NUMBER 
wl: arglist_info(u_get_number,
		[u_con],
		[Con_Param],
		arginfo{ all:[u_con],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con],
			 opt:0,
			 req:[u_con],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-NUMBER 
wl: init_args(exact_only, u_get_number).


% annotating U::GET-NUMBER 
f_u_get_number(Con_Param, TrueResult22) :-
	Env=[bv(u_con, Con_Param)],
	f_u_pair_c63(u_con, IFTEST),
	(   IFTEST\==[]
	->  cl_length(Con_Param, PredArg1Result),
	    (   PredArg1Result=:=1
	    ->  cl_car(Con_Param, Get_number_Param),
		f_u_get_number(Get_number_Param, TrueResult),
		TrueResult22=TrueResult
	    ;   TrueResult22=[number, u_plural]
	    )
	;   f_u_eq_c63(u_con, u_xx_me_ob_xx, IFTEST20),
	    (   IFTEST20\==[]
	    ->  TrueResult22=[number, u_first_person_singular]
	    ;   TrueResult22=[number, u_third_person_singular]
	    )
	).
:- set_opv(f_u_get_number, classof, claz_function),
   set_opv(u_get_number, compile_as, kw_function),
   set_opv(u_get_number, function, f_u_get_number),
   DefunResult=u_get_number.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:67586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     67834)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:67586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                          VERB MORPHOLOGY",
				     1,
				     67915)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:67586 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     67958)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:68039 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-verb-form',
			    [verb, number, tense, 'negative?', switches],
			    
			    [ if,
			      'negative?',
			      
			      [ 'get-negative-verb-form',
				verb,
				number,
				tense,
				switches
			      ],
			      
			      [ 'get-positive-verb-form',
				verb,
				number,
				tense,
				switches
			      ]
			    ]
			  ]).

% annotating U::GET-VERB-FORM 
wl: lambda_def(defun,
	      u_get_verb_form,
	      f_u_get_verb_form,
	      [u_verb, number, u_tense, u_negative_c63, u_switches],
	      
	      [ 
		[ if,
		  u_negative_c63,
		  [u_get_negative_verb_form, u_verb, number, u_tense, u_switches],
		  [u_get_positive_verb_form, u_verb, number, u_tense, u_switches]
		]
	      ]).


% annotating U::GET-VERB-FORM 
wl: arglist_info(u_get_verb_form,
		[u_verb, number, u_tense, u_negative_c63, u_switches],
		
		[ Verb_Param,
		  Number_Param,
		  Tense_Param,
		  Negative_c63_Param,
		  Switches_Param
		],
		arginfo{ all:[u_verb, number, u_tense, u_negative_c63, u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_verb,
				 number,
				 u_tense,
				 u_negative_c63,
				 u_switches
			       ],
			 opt:0,
			 req:[u_verb, number, u_tense, u_negative_c63, u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-VERB-FORM 
wl: init_args(exact_only, u_get_verb_form).


% annotating U::GET-VERB-FORM 
f_u_get_verb_form(Verb_Param, Number_Param, Tense_Param, Negative_c63_Param, Switches_Param, FnResult) :-
	Env=[bv(number, Number_Param), bv(u_tense, Tense_Param), bv(u_negative_c63, Negative_c63_Param), bv(u_switches, Switches_Param)],
	(   Negative_c63_Param\==[]
	->  f_u_get_negative_verb_form(Verb_Param,
				       number,
				       Tense_Param,
				       Switches_Param,
				       TrueResult),
	    FnResult=TrueResult
	;   f_u_get_positive_verb_form(Verb_Param,
				       number,
				       Tense_Param,
				       Switches_Param,
				       ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_get_verb_form, classof, claz_function),
   set_opv(u_get_verb_form, compile_as, kw_function),
   set_opv(u_get_verb_form, function, f_u_get_verb_form),
   DefunResult=u_get_verb_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:68234 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    progressivize,
			    [switches],
			    
			    [ let,
			      
			      [ 
				[ old,
				  ['switches-lookup', [quote, tense], switches]
				]
			      ],
			      
			      [ if,
				old,
				
				[ cond,
				  
				  [ ['eq?', old, [quote, gerund]],
				    [cons, [quote, [tense, gerund]], switches]
				  ],
				  
				  [ ['eq?', old, [quote, infinitive]],
				    
				    [ cons,
				      [quote, [tense, 'infinitive-progressive']],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, past]],
				    
				    [ cons,
				      [quote, [tense, 'past-progressive']],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, 'past-subjunctive']],
				    
				    [ cons,
				      
				      [ quote,
					[tense, 'past-subjunctive-progressive']
				      ],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, future]],
				    
				    [ cons,
				      [quote, [tense, 'future-progressive']],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, 'present-have-to']],
				    
				    [ cons,
				      [quote, [tense, 'have-to-progressive']],
				      switches
				    ]
				  ],
				  
				  [ 
				    [ 'eq?',
				      old,
				      
				      [ quote,
					'conditional-have-to-present-perfect'
				      ]
				    ],
				    
				    [ cons,
				      
				      [ quote,
					
					[ tense,
					  'conditional-have-to-present-perfect'
					]
				      ],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, 'have-to-gerund']],
				    
				    [ cons,
				      [quote, [tense, 'have-to-gerund']],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, 'conditional-have-to']],
				    
				    [ cons,
				      [quote, [tense, 'conditional-have-to']],
				      switches
				    ]
				  ],
				  
				  [ else,
				    
				    [ cons,
				      [quote, [tense, 'present-progressive']],
				      switches
				    ]
				  ]
				],
				
				[ cons,
				  [quote, [tense, 'present-progressive']],
				  switches
				]
			      ]
			    ]
			  ]).

% annotating U::PROGRESSIVIZE 
wl: lambda_def(defun,
	      u_progressivize,
	      f_u_progressivize,
	      [u_switches],
	      
	      [ 
		[ let,
		  [[u_old, [u_switches_lookup, [quote, u_tense], u_switches]]],
		  
		  [ if,
		    u_old,
		    
		    [ cond,
		      
		      [ [u_eq_c63, u_old, [quote, u_gerund]],
			[cons, [quote, [u_tense, u_gerund]], u_switches]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_infinitive]],
			
			[ cons,
			  [quote, [u_tense, u_infinitive_progressive]],
			  u_switches
			]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_past]],
			[cons, [quote, [u_tense, u_past_progressive]], u_switches]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_past_subjunctive]],
			
			[ cons,
			  [quote, [u_tense, u_past_subjunctive_progressive]],
			  u_switches
			]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_future]],
			
			[ cons,
			  [quote, [u_tense, u_future_progressive]],
			  u_switches
			]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_present_have_to]],
			
			[ cons,
			  [quote, [u_tense, u_have_to_progressive]],
			  u_switches
			]
		      ],
		      
		      [ 
			[ u_eq_c63,
			  u_old,
			  [quote, u_conditional_have_to_present_perfect]
			],
			
			[ cons,
			  
			  [ quote,
			    [u_tense, u_conditional_have_to_present_perfect]
			  ],
			  u_switches
			]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_have_to_gerund]],
			[cons, [quote, [u_tense, u_have_to_gerund]], u_switches]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_conditional_have_to]],
			
			[ cons,
			  [quote, [u_tense, u_conditional_have_to]],
			  u_switches
			]
		      ],
		      
		      [ u_else,
			
			[ cons,
			  [quote, [u_tense, u_present_progressive]],
			  u_switches
			]
		      ]
		    ],
		    [cons, [quote, [u_tense, u_present_progressive]], u_switches]
		  ]
		]
	      ]).


% annotating U::PROGRESSIVIZE 
wl: arglist_info(u_progressivize,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PROGRESSIVIZE 
wl: init_args(exact_only, u_progressivize).


% annotating U::PROGRESSIVIZE 
f_u_progressivize(Switches_Param, ElseResult54) :-
	Env=[bv(u_switches, Switches_Param)],
	f_u_switches_lookup(u_tense, Switches_Param, Old_Init),
	LEnv=[[bv(u_old, Old_Init)]|Env],
	get_var(LEnv, u_old, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_old, [quote, u_gerund], IFTEST20),
	    (   IFTEST20\==[]
	    ->  ElseResult54=[[u_tense, u_gerund]|Switches_Param]
	    ;   f_u_eq_c63(u_old, [quote, u_infinitive], IFTEST23),
		(   IFTEST23\==[]
		->  ElseResult54=[[u_tense, u_infinitive_progressive]|Switches_Param]
		;   f_u_eq_c63(u_old, [quote, u_past], IFTEST26),
		    (   IFTEST26\==[]
		    ->  ElseResult54=[[u_tense, u_past_progressive]|Switches_Param]
		    ;   f_u_eq_c63(u_old, [quote, u_past_subjunctive], IFTEST29),
			(   IFTEST29\==[]
			->  ElseResult54=[[u_tense, u_past_subjunctive_progressive]|Switches_Param]
			;   f_u_eq_c63(u_old, [quote, u_future], IFTEST32),
			    (   IFTEST32\==[]
			    ->  ElseResult54=[[u_tense, u_future_progressive]|Switches_Param]
			    ;   f_u_eq_c63(u_old,
					   [quote, u_present_have_to],
					   IFTEST35),
				(   IFTEST35\==[]
				->  ElseResult54=[[u_tense, u_have_to_progressive]|Switches_Param]
				;   f_u_eq_c63(u_old,
					       
					       [ quote,
						 u_conditional_have_to_present_perfect
					       ],
					       IFTEST38),
				    (   IFTEST38\==[]
				    ->  ElseResult54=[[u_tense, u_conditional_have_to_present_perfect]|Switches_Param]
				    ;   f_u_eq_c63(u_old,
						   [quote, u_have_to_gerund],
						   IFTEST41),
					(   IFTEST41\==[]
					->  ElseResult54=[[u_tense, u_have_to_gerund]|Switches_Param]
					;   f_u_eq_c63(u_old,
						       
						       [ quote,
							 u_conditional_have_to
						       ],
						       IFTEST44),
					    (   IFTEST44\==[]
					    ->  ElseResult54=[[u_tense, u_conditional_have_to]|Switches_Param]
					    ;   get_var(LEnv, u_else, IFTEST47),
						(   IFTEST47\==[]
						->  ElseResult54=[[u_tense, u_present_progressive]|Switches_Param]
						;   ElseResult54=[]
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
	    )
	;   ElseResult54=[[u_tense, u_present_progressive]|Switches_Param]
	).
:- set_opv(f_u_progressivize, classof, claz_function),
   set_opv(u_progressivize, compile_as, kw_function),
   set_opv(u_progressivize, function, f_u_progressivize),
   DefunResult=u_progressivize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:68234 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" could go to gerund-progressive, but this is too much!!",
				     1,
				     68413)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:69367 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'have-to-ize',
			    [switches],
			    
			    [ let,
			      
			      [ 
				[ old,
				  ['switches-lookup', [quote, tense], switches]
				]
			      ],
			      
			      [ if,
				old,
				
				[ cond,
				  
				  [ 
				    [ 'eq?',
				      old,
				      [quote, 'conditional-present-perfect']
				    ],
				    
				    [ cons,
				      
				      [ quote,
					
					[ tense,
					  'conditional-have-to-present-perfect'
					]
				      ],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, gerund]],
				    
				    [ cons,
				      [quote, [tense, 'have-to-gerund']],
				      switches
				    ]
				  ],
				  
				  [ ['eq?', old, [quote, conditional]],
				    
				    [ cons,
				      [quote, [tense, 'conditional-have-to']],
				      switches
				    ]
				  ],
				  
				  [ else,
				    
				    [ cons,
				      [quote, [tense, 'present-have-to']],
				      switches
				    ]
				  ]
				],
				
				[ cons,
				  [quote, [tense, 'present-have-to']],
				  switches
				]
			      ]
			    ]
			  ]).

% annotating U::HAVE-TO-IZE 
wl: lambda_def(defun,
	      u_have_to_ize,
	      f_u_have_to_ize,
	      [u_switches],
	      
	      [ 
		[ let,
		  [[u_old, [u_switches_lookup, [quote, u_tense], u_switches]]],
		  
		  [ if,
		    u_old,
		    
		    [ cond,
		      
		      [ [u_eq_c63, u_old, [quote, u_conditional_present_perfect]],
			
			[ cons,
			  
			  [ quote,
			    [u_tense, u_conditional_have_to_present_perfect]
			  ],
			  u_switches
			]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_gerund]],
			[cons, [quote, [u_tense, u_have_to_gerund]], u_switches]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_conditional]],
			
			[ cons,
			  [quote, [u_tense, u_conditional_have_to]],
			  u_switches
			]
		      ],
		      
		      [ u_else,
			[cons, [quote, [u_tense, u_present_have_to]], u_switches]
		      ]
		    ],
		    [cons, [quote, [u_tense, u_present_have_to]], u_switches]
		  ]
		]
	      ]).


% annotating U::HAVE-TO-IZE 
wl: arglist_info(u_have_to_ize,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::HAVE-TO-IZE 
wl: init_args(exact_only, u_have_to_ize).


% annotating U::HAVE-TO-IZE 
f_u_have_to_ize(Switches_Param, ElseResult36) :-
	Env=[bv(u_switches, Switches_Param)],
	f_u_switches_lookup(u_tense, Switches_Param, Old_Init),
	LEnv=[[bv(u_old, Old_Init)]|Env],
	get_var(LEnv, u_old, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_old, [quote, u_conditional_present_perfect], IFTEST20),
	    (   IFTEST20\==[]
	    ->  ElseResult36=[[u_tense, u_conditional_have_to_present_perfect]|Switches_Param]
	    ;   f_u_eq_c63(u_old, [quote, u_gerund], IFTEST23),
		(   IFTEST23\==[]
		->  ElseResult36=[[u_tense, u_have_to_gerund]|Switches_Param]
		;   f_u_eq_c63(u_old, [quote, u_conditional], IFTEST26),
		    (   IFTEST26\==[]
		    ->  ElseResult36=[[u_tense, u_conditional_have_to]|Switches_Param]
		    ;   get_var(LEnv, u_else, IFTEST29),
			(   IFTEST29\==[]
			->  ElseResult36=[[u_tense, u_present_have_to]|Switches_Param]
			;   ElseResult36=[]
			)
		    )
		)
	    )
	;   ElseResult36=[[u_tense, u_present_have_to]|Switches_Param]
	).
:- set_opv(f_u_have_to_ize, classof, claz_function),
   set_opv(u_have_to_ize, compile_as, kw_function),
   set_opv(u_have_to_ize, function, f_u_have_to_ize),
   DefunResult=u_have_to_ize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:69872 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'simplify-tense',
			    [switches],
			    
			    [ cons,
			      [list, [quote, 's-bar'], []],
			      
			      [ let,
				
				[ 
				  [ old,
				    ['switches-lookup', [quote, tense], switches]
				  ]
				],
				
				[ if,
				  old,
				  
				  [ cons,
				    
				    [ list,
				      [quote, tense],
				      
				      [ cond,
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'future-progressive']
					  ],
					  [quote, future]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'present-progressive']
					  ],
					  [quote, present]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'past-progressive']
					  ],
					  [quote, past]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    
					    [ quote,
					      'past-subjunctive-progressive'
					    ]
					  ],
					  [quote, past]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'present-perfect']
					  ],
					  [quote, past]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    
					    [ quote,
					      'conditional-present-perfect'
					    ]
					  ],
					  [quote, past]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    
					    [ quote,
					      'conditional-have-to-present-perfect'
					    ]
					  ],
					  [quote, past]
					],
					
					[ ['eq?', old, [quote, 'past-perfect']],
					  [quote, past]
					],
					
					[ ['eq?', old, [quote, future]],
					  [quote, future]
					],
					
					[ ['eq?', old, [quote, conditional]],
					  [quote, present]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'conditional-have-to']
					  ],
					  [quote, present]
					],
					
					[ ['eq?', old, [quote, past]],
					  [quote, past]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'past-subjunctive']
					  ],
					  [quote, past]
					],
					
					[ ['eq?', old, [quote, gerund]],
					  [quote, present]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'gerund-progressive']
					  ],
					  [quote, present]
					],
					
					[ ['eq?', old, [quote, 'have-to-gerund']],
					  [quote, present]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'infinitive-progressive']
					  ],
					  [quote, present]
					],
					
					[ ['eq?', old, [quote, infinitive]],
					  [quote, present]
					],
					
					[ ['eq?', old, [quote, 'past-gerund']],
					  [quote, past]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'past-infinitive']
					  ],
					  [quote, past]
					],
					
					[ 
					  [ 'eq?',
					    old,
					    [quote, 'present-have-to']
					  ],
					  [quote, present]
					],
					[else, [quote, present]]
				      ]
				    ],
				    switches
				  ],
				  switches
				]
			      ]
			    ]
			  ]).

% annotating U::SIMPLIFY-TENSE 
wl: lambda_def(defun,
	      u_simplify_tense,
	      f_u_simplify_tense,
	      [u_switches],
	      
	      [ 
		[ cons,
		  [list, [quote, u_s_bar], []],
		  
		  [ let,
		    [[u_old, [u_switches_lookup, [quote, u_tense], u_switches]]],
		    
		    [ if,
		      u_old,
		      
		      [ cons,
			
			[ list,
			  [quote, u_tense],
			  
			  [ cond,
			    
			    [ [u_eq_c63, u_old, [quote, u_future_progressive]],
			      [quote, u_future]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_present_progressive]],
			      [quote, u_present]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_past_progressive]],
			      [quote, u_past]
			    ],
			    
			    [ 
			      [ u_eq_c63,
				u_old,
				[quote, u_past_subjunctive_progressive]
			      ],
			      [quote, u_past]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_present_perfect]],
			      [quote, u_past]
			    ],
			    
			    [ 
			      [ u_eq_c63,
				u_old,
				[quote, u_conditional_present_perfect]
			      ],
			      [quote, u_past]
			    ],
			    
			    [ 
			      [ u_eq_c63,
				u_old,
				[quote, u_conditional_have_to_present_perfect]
			      ],
			      [quote, u_past]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_past_perfect]],
			      [quote, u_past]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_future]],
			      [quote, u_future]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_conditional]],
			      [quote, u_present]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_conditional_have_to]],
			      [quote, u_present]
			    ],
			    [[u_eq_c63, u_old, [quote, u_past]], [quote, u_past]],
			    
			    [ [u_eq_c63, u_old, [quote, u_past_subjunctive]],
			      [quote, u_past]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_gerund]],
			      [quote, u_present]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_gerund_progressive]],
			      [quote, u_present]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_have_to_gerund]],
			      [quote, u_present]
			    ],
			    
			    [ 
			      [ u_eq_c63,
				u_old,
				[quote, u_infinitive_progressive]
			      ],
			      [quote, u_present]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_infinitive]],
			      [quote, u_present]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_past_gerund]],
			      [quote, u_past]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_past_infinitive]],
			      [quote, u_past]
			    ],
			    
			    [ [u_eq_c63, u_old, [quote, u_present_have_to]],
			      [quote, u_present]
			    ],
			    [u_else, [quote, u_present]]
			  ]
			],
			u_switches
		      ],
		      u_switches
		    ]
		  ]
		]
	      ]).


% annotating U::SIMPLIFY-TENSE 
wl: arglist_info(u_simplify_tense,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SIMPLIFY-TENSE 
wl: init_args(exact_only, u_simplify_tense).


% annotating U::SIMPLIFY-TENSE 
f_u_simplify_tense(Switches_Param, FnResult) :-
	Env=[bv(u_switches, Switches_Param)],
	CAR94=[u_s_bar, []],
	f_u_switches_lookup(u_tense, Switches_Param, Old_Init),
	LEnv=[[bv(u_old, Old_Init)]|Env],
	get_var(LEnv, u_old, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_old, [quote, u_future_progressive], IFTEST20),
	    (   IFTEST20\==[]
	    ->  ElseResult66=u_future
	    ;   f_u_eq_c63(u_old, [quote, u_present_progressive], IFTEST22),
		(   IFTEST22\==[]
		->  ElseResult66=u_present
		;   f_u_eq_c63(u_old, [quote, u_past_progressive], IFTEST24),
		    (   IFTEST24\==[]
		    ->  ElseResult66=u_past
		    ;   f_u_eq_c63(u_old,
				   [quote, u_past_subjunctive_progressive],
				   IFTEST26),
			(   IFTEST26\==[]
			->  ElseResult66=u_past
			;   f_u_eq_c63(u_old,
				       [quote, u_present_perfect],
				       IFTEST28),
			    (   IFTEST28\==[]
			    ->  ElseResult66=u_past
			    ;   f_u_eq_c63(u_old,
					   
					   [ quote,
					     u_conditional_present_perfect
					   ],
					   IFTEST30),
				(   IFTEST30\==[]
				->  ElseResult66=u_past
				;   f_u_eq_c63(u_old,
					       
					       [ quote,
						 u_conditional_have_to_present_perfect
					       ],
					       IFTEST32),
				    (   IFTEST32\==[]
				    ->  ElseResult66=u_past
				    ;   f_u_eq_c63(u_old,
						   [quote, u_past_perfect],
						   IFTEST34),
					(   IFTEST34\==[]
					->  ElseResult66=u_past
					;   f_u_eq_c63(u_old,
						       [quote, u_future],
						       IFTEST36),
					    (   IFTEST36\==[]
					    ->  ElseResult66=u_future
					    ;   f_u_eq_c63(u_old,
							   
							   [ quote,
							     u_conditional
							   ],
							   IFTEST38),
						(   IFTEST38\==[]
						->  ElseResult66=u_present
						;   f_u_eq_c63(u_old,
							       
							       [ quote,
								 u_conditional_have_to
							       ],
							       IFTEST40),
						    (   IFTEST40\==[]
						    ->  ElseResult66=u_present
						    ;   f_u_eq_c63(u_old,
								   [quote, u_past],
								   IFTEST42),
							(   IFTEST42\==[]
							->  ElseResult66=u_past
							;   f_u_eq_c63(u_old,
								       
								       [ quote,
									 u_past_subjunctive
								       ],
								       IFTEST44),
							    (   IFTEST44\==[]
							    ->  ElseResult66=u_past
							    ;   f_u_eq_c63(u_old,
									   [quote, u_gerund],
									   IFTEST46),
								(   IFTEST46\==[]
								->  ElseResult66=u_present
								;   f_u_eq_c63(u_old,
									       
									       [ quote,
										 u_gerund_progressive
									       ],
									       IFTEST48),
								    (   IFTEST48\==[]
								    ->  ElseResult66=u_present
								    ;   f_u_eq_c63(u_old,
										   
										   [ quote,
										     u_have_to_gerund
										   ],
										   IFTEST50),
									(   IFTEST50\==[]
									->  ElseResult66=u_present
									;   f_u_eq_c63(u_old,
										       
										       [ quote,
											 u_infinitive_progressive
										       ],
										       IFTEST52),
									    (   IFTEST52\==[]
									    ->  ElseResult66=u_present
									    ;   f_u_eq_c63(u_old,
											   
											   [ quote,
											     u_infinitive
											   ],
											   IFTEST54),
										(   IFTEST54\==[]
										->  ElseResult66=u_present
										;   f_u_eq_c63(u_old,
											       
											       [ quote,
												 u_past_gerund
											       ],
											       IFTEST56),
										    (   IFTEST56\==[]
										    ->  ElseResult66=u_past
										    ;   f_u_eq_c63(u_old,
												   
												   [ quote,
												     u_past_infinitive
												   ],
												   IFTEST58),
											(   IFTEST58\==[]
											->  ElseResult66=u_past
											;   f_u_eq_c63(u_old,
												       
												       [ quote,
													 u_present_have_to
												       ],
												       IFTEST60),
											    (   IFTEST60\==[]
											    ->  ElseResult66=u_present
											    ;   get_var(LEnv,
													u_else,
													IFTEST62),
												(   IFTEST62\==[]
												->  ElseResult66=u_present
												;   ElseResult66=[]
												)
											    )
											)
										    )
										)
									    )
									)
								    )
								)
							    )
							)
						    )
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
	    ),
	    CAR=[u_tense, ElseResult66],
	    _81432=[CAR|Switches_Param]
	;   _81432=Switches_Param
	),
	LetResult=_81432,
	_81424=[CAR94|LetResult],
	_81424=FnResult.
:- set_opv(f_u_simplify_tense, classof, claz_function),
   set_opv(u_simplify_tense, compile_as, kw_function),
   set_opv(u_simplify_tense, function, f_u_simplify_tense),
   DefunResult=u_simplify_tense.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:71132 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'tense-time',
			    [switches],
			    
			    [ let,
			      
			      [ 
				[ old,
				  ['switches-lookup', [quote, tense], switches]
				]
			      ],
			      
			      [ if,
				old,
				
				[ cond,
				  [['eq?', old, [quote, past]], [quote, past]],
				  
				  [ ['eq?', old, [quote, 'future-progressive']],
				    [quote, future]
				  ],
				  
				  [ ['eq?', old, [quote, 'present-progressive']],
				    [quote, present]
				  ],
				  
				  [ ['eq?', old, [quote, 'past-progressive']],
				    [quote, past]
				  ],
				  
				  [ 
				    [ 'eq?',
				      old,
				      [quote, 'past-subjunctive-progressive']
				    ],
				    [quote, past]
				  ],
				  
				  [ ['eq?', old, [quote, 'present-perfect']],
				    [quote, past]
				  ],
				  
				  [ 
				    [ 'eq?',
				      old,
				      [quote, 'conditional-present-perfect']
				    ],
				    [quote, past]
				  ],
				  
				  [ 
				    [ 'eq?',
				      old,
				      
				      [ quote,
					'conditional-have-to-present-perfect'
				      ]
				    ],
				    [quote, past]
				  ],
				  
				  [ ['eq?', old, [quote, 'past-perfect']],
				    [quote, past]
				  ],
				  [['eq?', old, [quote, future]], [quote, future]],
				  
				  [ ['eq?', old, [quote, conditional]],
				    [quote, present]
				  ],
				  
				  [ ['eq?', old, [quote, 'conditional-have-to']],
				    [quote, present]
				  ],
				  
				  [ ['eq?', old, [quote, 'past-subjunctive']],
				    [quote, past]
				  ],
				  [['eq?', old, [quote, gerund]], [quote, present]],
				  
				  [ ['eq?', old, [quote, 'gerund-progressive']],
				    [quote, present]
				  ],
				  
				  [ ['eq?', old, [quote, 'have-to-gerund']],
				    [quote, present]
				  ],
				  
				  [ 
				    [ 'eq?',
				      old,
				      [quote, 'infinitive-progressive']
				    ],
				    [quote, present]
				  ],
				  
				  [ ['eq?', old, [quote, infinitive]],
				    [quote, present]
				  ],
				  
				  [ ['eq?', old, [quote, 'past-gerund']],
				    [quote, past]
				  ],
				  
				  [ ['eq?', old, [quote, 'past-infinitive']],
				    [quote, past]
				  ],
				  
				  [ ['eq?', old, [quote, 'present-have-to']],
				    [quote, present]
				  ],
				  [else, [quote, present]]
				],
				[quote, present]
			      ]
			    ]
			  ]).

% annotating U::TENSE-TIME 
wl: lambda_def(defun,
	      u_tense_time,
	      f_u_tense_time,
	      [u_switches],
	      
	      [ 
		[ let,
		  [[u_old, [u_switches_lookup, [quote, u_tense], u_switches]]],
		  
		  [ if,
		    u_old,
		    
		    [ cond,
		      [[u_eq_c63, u_old, [quote, u_past]], [quote, u_past]],
		      
		      [ [u_eq_c63, u_old, [quote, u_future_progressive]],
			[quote, u_future]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_present_progressive]],
			[quote, u_present]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_past_progressive]],
			[quote, u_past]
		      ],
		      
		      [ 
			[ u_eq_c63,
			  u_old,
			  [quote, u_past_subjunctive_progressive]
			],
			[quote, u_past]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_present_perfect]],
			[quote, u_past]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_conditional_present_perfect]],
			[quote, u_past]
		      ],
		      
		      [ 
			[ u_eq_c63,
			  u_old,
			  [quote, u_conditional_have_to_present_perfect]
			],
			[quote, u_past]
		      ],
		      [[u_eq_c63, u_old, [quote, u_past_perfect]], [quote, u_past]],
		      [[u_eq_c63, u_old, [quote, u_future]], [quote, u_future]],
		      
		      [ [u_eq_c63, u_old, [quote, u_conditional]],
			[quote, u_present]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_conditional_have_to]],
			[quote, u_present]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_past_subjunctive]],
			[quote, u_past]
		      ],
		      [[u_eq_c63, u_old, [quote, u_gerund]], [quote, u_present]],
		      
		      [ [u_eq_c63, u_old, [quote, u_gerund_progressive]],
			[quote, u_present]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_have_to_gerund]],
			[quote, u_present]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_infinitive_progressive]],
			[quote, u_present]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_infinitive]],
			[quote, u_present]
		      ],
		      [[u_eq_c63, u_old, [quote, u_past_gerund]], [quote, u_past]],
		      
		      [ [u_eq_c63, u_old, [quote, u_past_infinitive]],
			[quote, u_past]
		      ],
		      
		      [ [u_eq_c63, u_old, [quote, u_present_have_to]],
			[quote, u_present]
		      ],
		      [u_else, [quote, u_present]]
		    ],
		    [quote, u_present]
		  ]
		]
	      ]).


% annotating U::TENSE-TIME 
wl: arglist_info(u_tense_time,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TENSE-TIME 
wl: init_args(exact_only, u_tense_time).


% annotating U::TENSE-TIME 
f_u_tense_time(Switches_Param, ElseResult66) :-
	Env=[bv(u_switches, Switches_Param)],
	f_u_switches_lookup(u_tense, Switches_Param, Old_Init),
	LEnv=[[bv(u_old, Old_Init)]|Env],
	get_var(LEnv, u_old, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_old, [quote, u_past], IFTEST20),
	    (   IFTEST20\==[]
	    ->  ElseResult66=u_past
	    ;   f_u_eq_c63(u_old, [quote, u_future_progressive], IFTEST22),
		(   IFTEST22\==[]
		->  ElseResult66=u_future
		;   f_u_eq_c63(u_old, [quote, u_present_progressive], IFTEST24),
		    (   IFTEST24\==[]
		    ->  ElseResult66=u_present
		    ;   f_u_eq_c63(u_old, [quote, u_past_progressive], IFTEST26),
			(   IFTEST26\==[]
			->  ElseResult66=u_past
			;   f_u_eq_c63(u_old,
				       [quote, u_past_subjunctive_progressive],
				       IFTEST28),
			    (   IFTEST28\==[]
			    ->  ElseResult66=u_past
			    ;   f_u_eq_c63(u_old,
					   [quote, u_present_perfect],
					   IFTEST30),
				(   IFTEST30\==[]
				->  ElseResult66=u_past
				;   f_u_eq_c63(u_old,
					       
					       [ quote,
						 u_conditional_present_perfect
					       ],
					       IFTEST32),
				    (   IFTEST32\==[]
				    ->  ElseResult66=u_past
				    ;   f_u_eq_c63(u_old,
						   
						   [ quote,
						     u_conditional_have_to_present_perfect
						   ],
						   IFTEST34),
					(   IFTEST34\==[]
					->  ElseResult66=u_past
					;   f_u_eq_c63(u_old,
						       [quote, u_past_perfect],
						       IFTEST36),
					    (   IFTEST36\==[]
					    ->  ElseResult66=u_past
					    ;   f_u_eq_c63(u_old,
							   [quote, u_future],
							   IFTEST38),
						(   IFTEST38\==[]
						->  ElseResult66=u_future
						;   f_u_eq_c63(u_old,
							       
							       [ quote,
								 u_conditional
							       ],
							       IFTEST40),
						    (   IFTEST40\==[]
						    ->  ElseResult66=u_present
						    ;   f_u_eq_c63(u_old,
								   
								   [ quote,
								     u_conditional_have_to
								   ],
								   IFTEST42),
							(   IFTEST42\==[]
							->  ElseResult66=u_present
							;   f_u_eq_c63(u_old,
								       
								       [ quote,
									 u_past_subjunctive
								       ],
								       IFTEST44),
							    (   IFTEST44\==[]
							    ->  ElseResult66=u_past
							    ;   f_u_eq_c63(u_old,
									   [quote, u_gerund],
									   IFTEST46),
								(   IFTEST46\==[]
								->  ElseResult66=u_present
								;   f_u_eq_c63(u_old,
									       
									       [ quote,
										 u_gerund_progressive
									       ],
									       IFTEST48),
								    (   IFTEST48\==[]
								    ->  ElseResult66=u_present
								    ;   f_u_eq_c63(u_old,
										   
										   [ quote,
										     u_have_to_gerund
										   ],
										   IFTEST50),
									(   IFTEST50\==[]
									->  ElseResult66=u_present
									;   f_u_eq_c63(u_old,
										       
										       [ quote,
											 u_infinitive_progressive
										       ],
										       IFTEST52),
									    (   IFTEST52\==[]
									    ->  ElseResult66=u_present
									    ;   f_u_eq_c63(u_old,
											   
											   [ quote,
											     u_infinitive
											   ],
											   IFTEST54),
										(   IFTEST54\==[]
										->  ElseResult66=u_present
										;   f_u_eq_c63(u_old,
											       
											       [ quote,
												 u_past_gerund
											       ],
											       IFTEST56),
										    (   IFTEST56\==[]
										    ->  ElseResult66=u_past
										    ;   f_u_eq_c63(u_old,
												   
												   [ quote,
												     u_past_infinitive
												   ],
												   IFTEST58),
											(   IFTEST58\==[]
											->  ElseResult66=u_past
											;   f_u_eq_c63(u_old,
												       
												       [ quote,
													 u_present_have_to
												       ],
												       IFTEST60),
											    (   IFTEST60\==[]
											    ->  ElseResult66=u_present
											    ;   get_var(LEnv,
													u_else,
													IFTEST62),
												(   IFTEST62\==[]
												->  ElseResult66=u_present
												;   ElseResult66=[]
												)
											    )
											)
										    )
										)
									    )
									)
								    )
								)
							    )
							)
						    )
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
	    )
	;   ElseResult66=u_present
	).
:- set_opv(f_u_tense_time, classof, claz_function),
   set_opv(u_tense_time, compile_as, kw_function),
   set_opv(u_tense_time, function, f_u_tense_time),
   DefunResult=u_tense_time.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:72224 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    infinitivize,
			    [switches],
			    
			    [ let,
			      
			      [ 
				[ old,
				  ['switches-lookup', [quote, tense], switches]
				]
			      ],
			      
			      [ if,
				old,
				
				[ cond,
				  
				  [ ['eq?', old, [quote, past]],
				    
				    [ cons,
				      [quote, [tense, 'past-infinitive']],
				      switches
				    ]
				  ],
				  
				  [ else,
				    [cons, [quote, [tense, infinitive]], switches]
				  ]
				],
				[cons, [quote, [tense, infinitive]], switches]
			      ]
			    ]
			  ]).

% annotating U::INFINITIVIZE 
wl: lambda_def(defun,
	      u_infinitivize,
	      f_u_infinitivize,
	      [u_switches],
	      
	      [ 
		[ let,
		  [[u_old, [u_switches_lookup, [quote, u_tense], u_switches]]],
		  
		  [ if,
		    u_old,
		    
		    [ cond,
		      
		      [ [u_eq_c63, u_old, [quote, u_past]],
			[cons, [quote, [u_tense, u_past_infinitive]], u_switches]
		      ],
		      
		      [ u_else,
			[cons, [quote, [u_tense, u_infinitive]], u_switches]
		      ]
		    ],
		    [cons, [quote, [u_tense, u_infinitive]], u_switches]
		  ]
		]
	      ]).


% annotating U::INFINITIVIZE 
wl: arglist_info(u_infinitivize,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFINITIVIZE 
wl: init_args(exact_only, u_infinitivize).


% annotating U::INFINITIVIZE 
f_u_infinitivize(Switches_Param, ElseResult30) :-
	Env=[bv(u_switches, Switches_Param)],
	f_u_switches_lookup(u_tense, Switches_Param, Old_Init),
	LEnv=[[bv(u_old, Old_Init)]|Env],
	get_var(LEnv, u_old, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_old, [quote, u_past], IFTEST20),
	    (   IFTEST20\==[]
	    ->  ElseResult30=[[u_tense, u_past_infinitive]|Switches_Param]
	    ;   get_var(LEnv, u_else, IFTEST23),
		(   IFTEST23\==[]
		->  ElseResult30=[[u_tense, u_infinitive]|Switches_Param]
		;   ElseResult30=[]
		)
	    )
	;   ElseResult30=[[u_tense, u_infinitive]|Switches_Param]
	).
:- set_opv(f_u_infinitivize, classof, claz_function),
   set_opv(u_infinitivize, compile_as, kw_function),
   set_opv(u_infinitivize, function, f_u_infinitivize),
   DefunResult=u_infinitivize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:72509 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    gerundize,
			    [switches],
			    
			    [ let,
			      
			      [ 
				[ old,
				  ['switches-lookup', [quote, tense], switches]
				]
			      ],
			      
			      [ if,
				old,
				
				[ cond,
				  
				  [ ['eq?', old, [quote, past]],
				    
				    [ cons,
				      [quote, [tense, 'past-gerund']],
				      switches
				    ]
				  ],
				  
				  [ else,
				    [cons, [quote, [tense, gerund]], switches]
				  ]
				],
				[cons, [quote, [tense, gerund]], switches]
			      ]
			    ]
			  ]).

% annotating U::GERUNDIZE 
wl: lambda_def(defun,
	      u_gerundize,
	      f_u_gerundize,
	      [u_switches],
	      
	      [ 
		[ let,
		  [[u_old, [u_switches_lookup, [quote, u_tense], u_switches]]],
		  
		  [ if,
		    u_old,
		    
		    [ cond,
		      
		      [ [u_eq_c63, u_old, [quote, u_past]],
			[cons, [quote, [u_tense, u_past_gerund]], u_switches]
		      ],
		      [u_else, [cons, [quote, [u_tense, u_gerund]], u_switches]]
		    ],
		    [cons, [quote, [u_tense, u_gerund]], u_switches]
		  ]
		]
	      ]).


% annotating U::GERUNDIZE 
wl: arglist_info(u_gerundize,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GERUNDIZE 
wl: init_args(exact_only, u_gerundize).


% annotating U::GERUNDIZE 
f_u_gerundize(Switches_Param, ElseResult30) :-
	Env=[bv(u_switches, Switches_Param)],
	f_u_switches_lookup(u_tense, Switches_Param, Old_Init),
	LEnv=[[bv(u_old, Old_Init)]|Env],
	get_var(LEnv, u_old, IFTEST),
	(   IFTEST\==[]
	->  f_u_eq_c63(u_old, [quote, u_past], IFTEST20),
	    (   IFTEST20\==[]
	    ->  ElseResult30=[[u_tense, u_past_gerund]|Switches_Param]
	    ;   get_var(LEnv, u_else, IFTEST23),
		(   IFTEST23\==[]
		->  ElseResult30=[[u_tense, u_gerund]|Switches_Param]
		;   ElseResult30=[]
		)
	    )
	;   ElseResult30=[[u_tense, u_gerund]|Switches_Param]
	).
:- set_opv(f_u_gerundize, classof, claz_function),
   set_opv(u_gerundize, compile_as, kw_function),
   set_opv(u_gerundize, function, f_u_gerundize),
   DefunResult=u_gerundize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:72509 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          (cons '(tense conditional-have-to-present-perfect) switches))",
				     1,
				     72780)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:72509 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          (cons '(tense conditional-have-to) switches))",
				     1,
				     72853)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:72509 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (cons '(tense present-have-to) switches))))",
				     1,
				     72910)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:72963 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'infinitive-tense?',
			    [switches],
			    
			    [ let,
			      
			      [ 
				[ found,
				  ['switches-lookup', [quote, tense], switches]
				]
			      ],
			      
			      [ if,
				found,
				
				[ or,
				  ['eq?', found, [quote, infinitive]],
				  ['eq?', found, [quote, 'past-infinitive']],
				  
				  [ 'eq?',
				    found,
				    [quote, 'infinitive-progressive']
				  ]
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::INFINITIVE-TENSE? 
wl: lambda_def(defun,
	      u_infinitive_tense_c63,
	      f_u_infinitive_tense_c63,
	      [u_switches],
	      
	      [ 
		[ let,
		  [[u_found, [u_switches_lookup, [quote, u_tense], u_switches]]],
		  
		  [ if,
		    u_found,
		    
		    [ or,
		      [u_eq_c63, u_found, [quote, u_infinitive]],
		      [u_eq_c63, u_found, [quote, u_past_infinitive]],
		      [u_eq_c63, u_found, [quote, u_infinitive_progressive]]
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::INFINITIVE-TENSE? 
wl: arglist_info(u_infinitive_tense_c63,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFINITIVE-TENSE? 
wl: init_args(exact_only, u_infinitive_tense_c63).


% annotating U::INFINITIVE-TENSE? 
f_u_infinitive_tense_c63(Switches_Param, FnResult) :-
	Env=[bv(u_switches, Switches_Param)],
	f_u_switches_lookup(u_tense, Switches_Param, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  (   f_u_eq_c63(u_found, [quote, u_infinitive], FORM1_Res21),
		FORM1_Res21\==[],
		FnResult=FORM1_Res21
	    ->  true
	    ;   f_u_eq_c63(u_found, [quote, u_past_infinitive], FORM1_Res),
		FORM1_Res\==[],
		FnResult=FORM1_Res
	    ->  true
	    ;   f_u_eq_c63(u_found,
			   [quote, u_infinitive_progressive],
			   Eq_c63_Ret),
		FnResult=Eq_c63_Ret
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_infinitive_tense_c63, classof, claz_function),
   set_opv(u_infinitive_tense_c63, compile_as, kw_function),
   set_opv(u_infinitive_tense_c63, function, f_u_infinitive_tense_c63),
   DefunResult=u_infinitive_tense_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:73206 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gerund-tense?',
			    [switches],
			    
			    [ let,
			      
			      [ 
				[ found,
				  ['switches-lookup', [quote, tense], switches]
				]
			      ],
			      
			      [ if,
				found,
				
				[ or,
				  ['eq?', found, [quote, gerund]],
				  ['eq?', found, [quote, 'have-to-gerund']],
				  ['eq?', found, [quote, 'gerund-progressive']],
				  ['eq?', found, [quote, 'past-gerund']]
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::GERUND-TENSE? 
wl: lambda_def(defun,
	      u_gerund_tense_c63,
	      f_u_gerund_tense_c63,
	      [u_switches],
	      
	      [ 
		[ let,
		  [[u_found, [u_switches_lookup, [quote, u_tense], u_switches]]],
		  
		  [ if,
		    u_found,
		    
		    [ or,
		      [u_eq_c63, u_found, [quote, u_gerund]],
		      [u_eq_c63, u_found, [quote, u_have_to_gerund]],
		      [u_eq_c63, u_found, [quote, u_gerund_progressive]],
		      [u_eq_c63, u_found, [quote, u_past_gerund]]
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::GERUND-TENSE? 
wl: arglist_info(u_gerund_tense_c63,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GERUND-TENSE? 
wl: init_args(exact_only, u_gerund_tense_c63).


% annotating U::GERUND-TENSE? 
f_u_gerund_tense_c63(Switches_Param, FnResult) :-
	Env=[bv(u_switches, Switches_Param)],
	f_u_switches_lookup(u_tense, Switches_Param, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  (   f_u_eq_c63(u_found, [quote, u_gerund], FORM1_Res22),
		FORM1_Res22\==[],
		FnResult=FORM1_Res22
	    ->  true
	    ;   f_u_eq_c63(u_found, [quote, u_have_to_gerund], FORM1_Res21),
		FORM1_Res21\==[],
		FnResult=FORM1_Res21
	    ->  true
	    ;   f_u_eq_c63(u_found, [quote, u_gerund_progressive], FORM1_Res),
		FORM1_Res\==[],
		FnResult=FORM1_Res
	    ->  true
	    ;   f_u_eq_c63(u_found, [quote, u_past_gerund], Eq_c63_Ret),
		FnResult=Eq_c63_Ret
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_gerund_tense_c63, classof, claz_function),
   set_opv(u_gerund_tense_c63, compile_as, kw_function),
   set_opv(u_gerund_tense_c63, function, f_u_gerund_tense_c63),
   DefunResult=u_gerund_tense_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:73473 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inf-to',
			    [switches],
			    
			    [ if,
			      
			      [ 'switches-lookup',
				[quote, 'to-less-infinitive'],
				switches
			      ],
			      '$STRING'(""),
			      '$STRING'("to ")
			    ]
			  ]).

% annotating U::INF-TO 
wl: lambda_def(defun,
	      u_inf_to,
	      f_u_inf_to,
	      [u_switches],
	      
	      [ 
		[ if,
		  [u_switches_lookup, [quote, u_to_less_infinitive], u_switches],
		  '$ARRAY'([*], claz_base_character, []),
		  '$ARRAY'([*], claz_base_character, [#\(t), #\(o), #\(' ')])
		]
	      ]).


% annotating U::INF-TO 
wl: arglist_info(u_inf_to,
		[u_switches],
		[Switches_Param],
		arginfo{ all:[u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_switches],
			 opt:0,
			 req:[u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INF-TO 
wl: init_args(exact_only, u_inf_to).


% annotating U::INF-TO 
f_u_inf_to(Switches_Param, FnResult) :-
	f_u_switches_lookup(u_to_less_infinitive, Switches_Param, IFTEST),
	(   IFTEST\==[]
	->  FnResult='$ARRAY'([*], claz_base_character, [])
	;   FnResult='$ARRAY'([*], claz_base_character, [#\(t), #\(o), #\(' ')])
	).
:- set_opv(f_u_inf_to, classof, claz_function),
   set_opv(u_inf_to, compile_as, kw_function),
   set_opv(u_inf_to, function, f_u_inf_to),
   DefunResult=u_inf_to.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:73575 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-positive-verb-form',
			    [verb, number, tense, switches],
			    
			    [ cond,
			      
			      [ ['eq?', tense, [quote, 'future-progressive']],
				
				[ 'string-append',
				  '$STRING'("will "),
				  ['present-form', [quote, be], number],
				  '$STRING'(" "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'present-progressive']],
				
				[ 'string-append',
				  ['present-form', [quote, be], number],
				  '$STRING'(" "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-progressive']],
				
				[ 'string-append',
				  ['past-form', [quote, be], number],
				  '$STRING'(" "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ 
				[ 'eq?',
				  tense,
				  [quote, 'past-subjunctive-progressive']
				],
				
				[ 'string-append',
				  ['past-subjunctive-form', [quote, be], number],
				  '$STRING'(" "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'present-perfect']],
				
				[ 'string-append',
				  ['present-form', [quote, have], number],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ 
				[ 'eq?',
				  tense,
				  [quote, 'conditional-present-perfect']
				],
				
				[ 'string-append',
				  '$STRING'("would "),
				  ['infinitive-form', [quote, have]],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ 
				[ 'eq?',
				  tense,
				  
				  [ quote,
				    'conditional-have-to-present-perfect'
				  ]
				],
				
				[ 'string-append',
				  '$STRING'("would have to "),
				  ['infinitive-form', [quote, have]],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-perfect']],
				
				[ 'string-append',
				  '$STRING'("had "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, future]],
				
				[ 'string-append',
				  '$STRING'("will "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, conditional]],
				
				[ 'string-append',
				  '$STRING'("would "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'conditional-have-to']],
				
				[ 'string-append',
				  '$STRING'("would have to "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, past]],
				['past-form', verb, number]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-subjunctive']],
				['past-subjunctive-form', verb, number]
			      ],
			      [['eq?', tense, [quote, gerund]], ['ing-form', verb]],
			      
			      [ ['eq?', tense, [quote, 'gerund-progressive']],
				
				[ 'string-append',
				  ['ing-form', [quote, be]],
				  '$STRING'(" "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'have-to-gerund']],
				
				[ 'string-append',
				  ['ing-form', [quote, have]],
				  '$STRING'(" to "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'have-to-progressive']],
				
				[ 'string-append',
				  ['present-form', [quote, have], number],
				  '$STRING'(" to be "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'infinitive-progressive']],
				
				[ 'string-append',
				  ['inf-to', switches],
				  '$STRING'("be "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, infinitive]],
				
				[ 'string-append',
				  ['inf-to', switches],
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-gerund']],
				
				[ 'string-append',
				  ['ing-form', [quote, have]],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-infinitive']],
				
				[ 'string-append',
				  ['inf-to', switches],
				  ['infinitive-form', [quote, have]],
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'present-have-to']],
				
				[ 'string-append',
				  ['present-form', [quote, have], number],
				  '$STRING'(" to "),
				  ['infinitive-form', verb]
				]
			      ],
			      [else, ['present-form', verb, number]]
			    ]
			  ]).

% annotating U::GET-POSITIVE-VERB-FORM 
wl: lambda_def(defun,
	      u_get_positive_verb_form,
	      f_u_get_positive_verb_form,
	      [u_verb, number, u_tense, u_switches],
	      
	      [ 
		[ cond,
		  
		  [ [u_eq_c63, u_tense, [quote, u_future_progressive]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(w), #\(i), #\(l), #\(l), #\(' ')]),
		      [u_present_form, [quote, u_be], number],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_present_progressive]],
		    
		    [ u_string_append,
		      [u_present_form, [quote, u_be], number],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_progressive]],
		    
		    [ u_string_append,
		      [u_past_form, [quote, u_be], number],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_subjunctive_progressive]],
		    
		    [ u_string_append,
		      [u_past_subjunctive_form, [quote, u_be], number],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_present_perfect]],
		    
		    [ u_string_append,
		      [u_present_form, [quote, u_have], number],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_conditional_present_perfect]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(w), #\(o), #\(u), #\(l), #\(d), #\(' ')]),
		      [u_infinitive_form, [quote, u_have]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ 
		    [ u_eq_c63,
		      u_tense,
		      [quote, u_conditional_have_to_present_perfect]
		    ],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(o),
				 #\(u),
				 #\(l),
				 #\(d),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(v),
				 #\(e),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' ')
			       ]),
		      [u_infinitive_form, [quote, u_have]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_perfect]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(h), #\(a), #\(d), #\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_future]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(w), #\(i), #\(l), #\(l), #\(' ')]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_conditional]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(w), #\(o), #\(u), #\(l), #\(d), #\(' ')]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_conditional_have_to]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(o),
				 #\(u),
				 #\(l),
				 #\(d),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(v),
				 #\(e),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' ')
			       ]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past]],
		    [u_past_form, u_verb, number]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_subjunctive]],
		    [u_past_subjunctive_form, u_verb, number]
		  ],
		  [[u_eq_c63, u_tense, [quote, u_gerund]], [u_ing_form, u_verb]],
		  
		  [ [u_eq_c63, u_tense, [quote, u_gerund_progressive]],
		    
		    [ u_string_append,
		      [u_ing_form, [quote, u_be]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_have_to_gerund]],
		    
		    [ u_string_append,
		      [u_ing_form, [quote, u_have]],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(t), #\(o), #\(' ')]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_have_to_progressive]],
		    
		    [ u_string_append,
		      [u_present_form, [quote, u_have], number],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(' '),
				 #\(t),
				 #\(o),
				 #\(' '),
				 #\(b),
				 #\(e),
				 #\(' ')
			       ]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_infinitive_progressive]],
		    
		    [ u_string_append,
		      [u_inf_to, u_switches],
		      '$ARRAY'([*], claz_base_character, [#\(b), #\(e), #\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_infinitive]],
		    
		    [ u_string_append,
		      [u_inf_to, u_switches],
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_gerund]],
		    
		    [ u_string_append,
		      [u_ing_form, [quote, u_have]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_infinitive]],
		    
		    [ u_string_append,
		      [u_inf_to, u_switches],
		      [u_infinitive_form, [quote, u_have]],
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_present_have_to]],
		    
		    [ u_string_append,
		      [u_present_form, [quote, u_have], number],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(t), #\(o), #\(' ')]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  [u_else, [u_present_form, u_verb, number]]
		]
	      ]).


% annotating U::GET-POSITIVE-VERB-FORM 
wl: arglist_info(u_get_positive_verb_form,
		[u_verb, number, u_tense, u_switches],
		[Verb_Param, Number_Param, Tense_Param, Switches_Param],
		arginfo{ all:[u_verb, number, u_tense, u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb, number, u_tense, u_switches],
			 opt:0,
			 req:[u_verb, number, u_tense, u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-POSITIVE-VERB-FORM 
wl: init_args(exact_only, u_get_positive_verb_form).


% annotating U::GET-POSITIVE-VERB-FORM 
f_u_get_positive_verb_form(Verb_Param, Number_Param, Tense_Param, Switches_Param, ElseResult72) :-
	Env=[bv(u_verb, Verb_Param), bv(number, Number_Param), bv(u_tense, Tense_Param), bv(u_switches, Switches_Param)],
	f_u_eq_c63(u_tense, [quote, u_future_progressive], IFTEST),
	(   IFTEST\==[]
	->  f_u_string_append(
			      [ '$ARRAY'([*],
					 claz_base_character,
					 [#\(w), #\(i), #\(l), #\(l), #\(' ')]),
				[u_present_form, [quote, u_be], number],
				'$ARRAY'([*], claz_base_character, [#\(' ')]),
				[u_ing_form, u_verb]
			      ],
			      TrueResult113),
	    ElseResult72=TrueResult113
	;   f_u_eq_c63(u_tense, [quote, u_present_progressive], IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_string_append(
				  [ [u_present_form, [quote, u_be], number],
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' ')]),
				    [u_ing_form, u_verb]
				  ],
				  TrueResult111),
		ElseResult72=TrueResult111
	    ;   f_u_eq_c63(u_tense, [quote, u_past_progressive], IFTEST22),
		(   IFTEST22\==[]
		->  f_u_string_append(
				      [ [u_past_form, [quote, u_be], number],
					'$ARRAY'([*],
						 claz_base_character,
						 [#\(' ')]),
					[u_ing_form, u_verb]
				      ],
				      TrueResult109),
		    ElseResult72=TrueResult109
		;   f_u_eq_c63(u_tense,
			       [quote, u_past_subjunctive_progressive],
			       IFTEST24),
		    (   IFTEST24\==[]
		    ->  f_u_string_append(
					  [ 
					    [ u_past_subjunctive_form,
					      [quote, u_be],
					      number
					    ],
					    '$ARRAY'([*],
						     claz_base_character,
						     [#\(' ')]),
					    [u_ing_form, u_verb]
					  ],
					  TrueResult107),
			ElseResult72=TrueResult107
		    ;   f_u_eq_c63(u_tense, [quote, u_present_perfect], IFTEST26),
			(   IFTEST26\==[]
			->  f_u_string_append(
					      [ 
						[ u_present_form,
						  [quote, u_have],
						  number
						],
						'$ARRAY'([*],
							 claz_base_character,
							 [#\(' ')]),
						[u_en_form, u_verb]
					      ],
					      TrueResult105),
			    ElseResult72=TrueResult105
			;   f_u_eq_c63(u_tense,
				       [quote, u_conditional_present_perfect],
				       IFTEST28),
			    (   IFTEST28\==[]
			    ->  f_u_string_append(
						  [ '$ARRAY'([*],
							     claz_base_character,
							     
							     [ #\(w),
							       #\(o),
							       #\(u),
							       #\(l),
							       #\(d),
							       #\(' ')
							     ]),
						    
						    [ u_infinitive_form,
						      [quote, u_have]
						    ],
						    '$ARRAY'([*],
							     claz_base_character,
							     [#\(' ')]),
						    [u_en_form, u_verb]
						  ],
						  TrueResult103),
				ElseResult72=TrueResult103
			    ;   f_u_eq_c63(u_tense,
					   
					   [ quote,
					     u_conditional_have_to_present_perfect
					   ],
					   IFTEST30),
				(   IFTEST30\==[]
				->  f_u_string_append(
						      [ '$ARRAY'([*],
								 claz_base_character,
								 
								 [ #\(w),
								   #\(o),
								   #\(u),
								   #\(l),
								   #\(d),
								   #\(' '),
								   #\(h),
								   #\(a),
								   #\(v),
								   #\(e),
								   #\(' '),
								   #\(t),
								   #\(o),
								   #\(' ')
								 ]),
							
							[ u_infinitive_form,
							  [quote, u_have]
							],
							'$ARRAY'([*],
								 claz_base_character,
								 [#\(' ')]),
							[u_en_form, u_verb]
						      ],
						      TrueResult101),
				    ElseResult72=TrueResult101
				;   f_u_eq_c63(u_tense,
					       [quote, u_past_perfect],
					       IFTEST32),
				    (   IFTEST32\==[]
				    ->  f_u_string_append(
							  [ '$ARRAY'([*],
								     claz_base_character,
								     
								     [ #\(h),
								       #\(a),
								       #\(d),
								       #\(' ')
								     ]),
							    [u_en_form, u_verb]
							  ],
							  TrueResult99),
					ElseResult72=TrueResult99
				    ;   f_u_eq_c63(u_tense,
						   [quote, u_future],
						   IFTEST34),
					(   IFTEST34\==[]
					->  f_u_string_append(
							      [ '$ARRAY'([*],
									 claz_base_character,
									 
									 [ #\(w),
									   #\(i),
									   #\(l),
									   #\(l),
									   #\(' ')
									 ]),
								
								[ u_infinitive_form,
								  u_verb
								]
							      ],
							      TrueResult97),
					    ElseResult72=TrueResult97
					;   f_u_eq_c63(u_tense,
						       [quote, u_conditional],
						       IFTEST36),
					    (   IFTEST36\==[]
					    ->  f_u_string_append(
								  [ '$ARRAY'([*],
									     claz_base_character,
									     
									     [ #\(w),
									       #\(o),
									       #\(u),
									       #\(l),
									       #\(d),
									       #\(' ')
									     ]),
								    
								    [ u_infinitive_form,
								      u_verb
								    ]
								  ],
								  TrueResult95),
						ElseResult72=TrueResult95
					    ;   f_u_eq_c63(u_tense,
							   
							   [ quote,
							     u_conditional_have_to
							   ],
							   IFTEST38),
						(   IFTEST38\==[]
						->  f_u_string_append(
								      [ '$ARRAY'([*],
										 claz_base_character,
										 
										 [ #\(w),
										   #\(o),
										   #\(u),
										   #\(l),
										   #\(d),
										   #\(' '),
										   #\(h),
										   #\(a),
										   #\(v),
										   #\(e),
										   #\(' '),
										   #\(t),
										   #\(o),
										   #\(' ')
										 ]),
									
									[ u_infinitive_form,
									  u_verb
									]
								      ],
								      TrueResult93),
						    ElseResult72=TrueResult93
						;   f_u_eq_c63(u_tense,
							       [quote, u_past],
							       IFTEST40),
						    (   IFTEST40\==[]
						    ->  f_u_past_form(Verb_Param,
								      number,
								      TrueResult91),
							ElseResult72=TrueResult91
						    ;   f_u_eq_c63(u_tense,
								   
								   [ quote,
								     u_past_subjunctive
								   ],
								   IFTEST43),
							(   IFTEST43\==[]
							->  f_u_past_subjunctive_form(Verb_Param,
										      number,
										      TrueResult89),
							    ElseResult72=TrueResult89
							;   f_u_eq_c63(u_tense,
								       [quote, u_gerund],
								       IFTEST46),
							    (   IFTEST46\==[]
							    ->  f_u_ing_form(Verb_Param,
									     TrueResult87),
								ElseResult72=TrueResult87
							    ;   f_u_eq_c63(u_tense,
									   
									   [ quote,
									     u_gerund_progressive
									   ],
									   IFTEST49),
								(   IFTEST49\==[]
								->  f_u_string_append(
										      [ 
											[ u_ing_form,
											  [quote, u_be]
											],
											'$ARRAY'([*],
												 claz_base_character,
												 [#\(' ')]),
											[u_ing_form, u_verb]
										      ],
										      TrueResult85),
								    ElseResult72=TrueResult85
								;   f_u_eq_c63(u_tense,
									       
									       [ quote,
										 u_have_to_gerund
									       ],
									       IFTEST51),
								    (   IFTEST51\==[]
								    ->  f_u_string_append(
											  [ 
											    [ u_ing_form,
											      [quote, u_have]
											    ],
											    '$ARRAY'([*],
												     claz_base_character,
												     
												     [ #\(' '),
												       #\(t),
												       #\(o),
												       #\(' ')
												     ]),
											    
											    [ u_infinitive_form,
											      u_verb
											    ]
											  ],
											  TrueResult83),
									ElseResult72=TrueResult83
								    ;   f_u_eq_c63(u_tense,
										   
										   [ quote,
										     u_have_to_progressive
										   ],
										   IFTEST53),
									(   IFTEST53\==[]
									->  f_u_string_append(
											      [ 
												[ u_present_form,
												  [quote, u_have],
												  number
												],
												'$ARRAY'([*],
													 claz_base_character,
													 
													 [ #\(' '),
													   #\(t),
													   #\(o),
													   #\(' '),
													   #\(b),
													   #\(e),
													   #\(' ')
													 ]),
												[u_ing_form, u_verb]
											      ],
											      TrueResult81),
									    ElseResult72=TrueResult81
									;   f_u_eq_c63(u_tense,
										       
										       [ quote,
											 u_infinitive_progressive
										       ],
										       IFTEST55),
									    (   IFTEST55\==[]
									    ->  f_u_string_append(
												  [ 
												    [ u_inf_to,
												      u_switches
												    ],
												    '$ARRAY'([*],
													     claz_base_character,
													     
													     [ #\(b),
													       #\(e),
													       #\(' ')
													     ]),
												    [u_ing_form, u_verb]
												  ],
												  TrueResult79),
										ElseResult72=TrueResult79
									    ;   f_u_eq_c63(u_tense,
											   
											   [ quote,
											     u_infinitive
											   ],
											   IFTEST57),
										(   IFTEST57\==[]
										->  f_u_string_append(
												      [ 
													[ u_inf_to,
													  u_switches
													],
													
													[ u_infinitive_form,
													  u_verb
													]
												      ],
												      TrueResult77),
										    ElseResult72=TrueResult77
										;   f_u_eq_c63(u_tense,
											       
											       [ quote,
												 u_past_gerund
											       ],
											       IFTEST59),
										    (   IFTEST59\==[]
										    ->  f_u_string_append(
													  [ 
													    [ u_ing_form,
													      [quote, u_have]
													    ],
													    '$ARRAY'([*],
														     claz_base_character,
														     [#\(' ')]),
													    [u_en_form, u_verb]
													  ],
													  TrueResult75),
											ElseResult72=TrueResult75
										    ;   f_u_eq_c63(u_tense,
												   
												   [ quote,
												     u_past_infinitive
												   ],
												   IFTEST61),
											(   IFTEST61\==[]
											->  f_u_string_append(
													      [ 
														[ u_inf_to,
														  u_switches
														],
														
														[ u_infinitive_form,
														  [quote, u_have]
														],
														[u_en_form, u_verb]
													      ],
													      TrueResult73),
											    ElseResult72=TrueResult73
											;   f_u_eq_c63(u_tense,
												       
												       [ quote,
													 u_present_have_to
												       ],
												       IFTEST63),
											    (   IFTEST63\==[]
											    ->  f_u_string_append(
														  [ 
														    [ u_present_form,
														      [quote, u_have],
														      number
														    ],
														    '$ARRAY'([*],
															     claz_base_character,
															     
															     [ #\(' '),
															       #\(t),
															       #\(o),
															       #\(' ')
															     ]),
														    
														    [ u_infinitive_form,
														      u_verb
														    ]
														  ],
														  TrueResult71),
												ElseResult72=TrueResult71
											    ;   get_var(Env,
													u_else,
													IFTEST65),
												(   IFTEST65\==[]
												->  f_u_present_form(Verb_Param,
														     number,
														     TrueResult),
												    ElseResult72=TrueResult
												;   ElseResult72=[]
												)
											    )
											)
										    )
										)
									    )
									)
								    )
								)
							    )
							)
						    )
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_get_positive_verb_form, classof, claz_function),
   set_opv(u_get_positive_verb_form, compile_as, kw_function),
   set_opv(u_get_positive_verb_form, function, f_u_get_positive_verb_form),
   DefunResult=u_get_positive_verb_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:73575 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" default is present", 9, 75762)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:75818 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'modal-aux?',
			    [verb],
			    ['eq?', verb, [quote, would]]
			  ]).

% annotating U::MODAL-AUX? 
wl: lambda_def(defun,
	      u_modal_aux_c63,
	      f_u_modal_aux_c63,
	      [u_verb],
	      [[u_eq_c63, u_verb, [quote, u_would]]]).


% annotating U::MODAL-AUX? 
wl: arglist_info(u_modal_aux_c63,
		[u_verb],
		[Verb_Param],
		arginfo{ all:[u_verb],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb],
			 opt:0,
			 req:[u_verb],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MODAL-AUX? 
wl: init_args(exact_only, u_modal_aux_c63).


% annotating U::MODAL-AUX? 
f_u_modal_aux_c63(Verb_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param)],
	f_u_eq_c63(u_verb, [quote, u_would], Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_modal_aux_c63, classof, claz_function),
   set_opv(u_modal_aux_c63, compile_as, kw_function),
   set_opv(u_modal_aux_c63, function, f_u_modal_aux_c63),
   DefunResult=u_modal_aux_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:75865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-negative-verb-form',
			    [verb, number, tense, switches],
			    
			    [ cond,
			      
			      [ ['modal-aux?', verb],
				
				[ 'string-append',
				  ['present-form', verb, number],
				  '$STRING'(" not")
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'future-progressive']],
				
				[ 'string-append',
				  '$STRING'("will not "),
				  ['present-form', [quote, be], number],
				  '$STRING'(" "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'present-progressive']],
				
				[ 'string-append',
				  ['present-form', [quote, be], number],
				  '$STRING'(" not "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-progressive']],
				
				[ 'string-append',
				  ['past-form', [quote, be], number],
				  '$STRING'(" not "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ 
				[ 'eq?',
				  tense,
				  [quote, 'past-subjunctive-progressive']
				],
				
				[ 'string-append',
				  ['past-subjunctive-form', [quote, be], number],
				  '$STRING'(" not "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'present-perfect']],
				
				[ 'string-append',
				  ['present-form', [quote, have], number],
				  '$STRING'(" not "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ 
				[ 'eq?',
				  tense,
				  [quote, 'conditional-present-perfect']
				],
				
				[ 'string-append',
				  '$STRING'("would not "),
				  ['infinitive-form', [quote, have]],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ 
				[ 'eq?',
				  tense,
				  
				  [ quote,
				    'conditional-have-to-present-perfect'
				  ]
				],
				
				[ 'string-append',
				  '$STRING'("would not have to "),
				  ['infinitive-form', [quote, have]],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-perfect']],
				
				[ 'string-append',
				  '$STRING'("had not "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, future]],
				
				[ 'string-append',
				  '$STRING'("will not "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, conditional]],
				
				[ 'string-append',
				  '$STRING'("would not "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'conditional-have-to']],
				
				[ 'string-append',
				  '$STRING'("would not have to "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, past]],
				
				[ if,
				  ['eq?', verb, [quote, be]],
				  
				  [ 'string-append',
				    ['past-form', verb, number],
				    '$STRING'(" not")
				  ],
				  
				  [ 'string-append',
				    ['past-form', [quote, do], number],
				    '$STRING'(" not "),
				    ['infinitive-form', verb]
				  ]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-subjunctive']],
				
				[ if,
				  ['eq?', verb, [quote, be]],
				  
				  [ 'string-append',
				    ['past-subjunctive-form', verb, number],
				    '$STRING'(" not")
				  ],
				  
				  [ 'string-append',
				    
				    [ 'past-subjunctive-form',
				      [quote, do],
				      number
				    ],
				    '$STRING'(" not "),
				    ['infinitive-form', verb]
				  ]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, gerund]],
				
				[ 'string-append',
				  '$STRING'("not "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'gerund-progressive']],
				
				[ 'string-append',
				  '$STRING'("not "),
				  ['ing-form', [quote, be]],
				  '$STRING'(" "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'have-to-gerund']],
				
				[ 'string-append',
				  '$STRING'("not "),
				  ['ing-form', [quote, have]],
				  '$STRING'(" to "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'have-to-progressive']],
				
				[ 'string-append',
				  '$STRING'("cannot be "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'infinitive-progressive']],
				
				[ 'string-append',
				  '$STRING'("not "),
				  ['inf-to', switches],
				  '$STRING'("be "),
				  ['ing-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, infinitive]],
				
				[ 'string-append',
				  '$STRING'("not "),
				  ['inf-to', switches],
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-gerund']],
				
				[ 'string-append',
				  '$STRING'("not "),
				  ['ing-form', [quote, have]],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'past-infinitive']],
				
				[ 'string-append',
				  '$STRING'("not "),
				  ['inf-to', switches],
				  ['infinitive-form', [quote, have]],
				  '$STRING'(" "),
				  ['en-form', verb]
				]
			      ],
			      
			      [ ['eq?', tense, [quote, 'present-have-to']],
				
				[ 'string-append',
				  ['present-form', [quote, do], number],
				  '$STRING'("not have to "),
				  ['infinitive-form', verb]
				]
			      ],
			      
			      [ else,
				
				[ if,
				  ['eq?', verb, [quote, be]],
				  
				  [ 'string-append',
				    ['present-form', verb, number],
				    '$STRING'(" not")
				  ],
				  
				  [ 'string-append',
				    ['present-form', [quote, do], number],
				    '$STRING'(" not "),
				    ['infinitive-form', verb]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GET-NEGATIVE-VERB-FORM 
wl: lambda_def(defun,
	      u_get_negative_verb_form,
	      f_u_get_negative_verb_form,
	      [u_verb, number, u_tense, u_switches],
	      
	      [ 
		[ cond,
		  
		  [ [u_modal_aux_c63, u_verb],
		    
		    [ u_string_append,
		      [u_present_form, u_verb, number],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(n), #\(o), #\(t)])
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_future_progressive]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(i),
				 #\(l),
				 #\(l),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' ')
			       ]),
		      [u_present_form, [quote, u_be], number],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_present_progressive]],
		    
		    [ u_string_append,
		      [u_present_form, [quote, u_be], number],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(n), #\(o), #\(t), #\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_progressive]],
		    
		    [ u_string_append,
		      [u_past_form, [quote, u_be], number],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(n), #\(o), #\(t), #\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_subjunctive_progressive]],
		    
		    [ u_string_append,
		      [u_past_subjunctive_form, [quote, u_be], number],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(n), #\(o), #\(t), #\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_present_perfect]],
		    
		    [ u_string_append,
		      [u_present_form, [quote, u_have], number],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(n), #\(o), #\(t), #\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_conditional_present_perfect]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(o),
				 #\(u),
				 #\(l),
				 #\(d),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' ')
			       ]),
		      [u_infinitive_form, [quote, u_have]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ 
		    [ u_eq_c63,
		      u_tense,
		      [quote, u_conditional_have_to_present_perfect]
		    ],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(o),
				 #\(u),
				 #\(l),
				 #\(d),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(v),
				 #\(e),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' ')
			       ]),
		      [u_infinitive_form, [quote, u_have]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_perfect]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(h),
				 #\(a),
				 #\(d),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' ')
			       ]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_future]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(i),
				 #\(l),
				 #\(l),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' ')
			       ]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_conditional]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(o),
				 #\(u),
				 #\(l),
				 #\(d),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' ')
			       ]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_conditional_have_to]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(w),
				 #\(o),
				 #\(u),
				 #\(l),
				 #\(d),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(v),
				 #\(e),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' ')
			       ]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past]],
		    
		    [ if,
		      [u_eq_c63, u_verb, [quote, u_be]],
		      
		      [ u_string_append,
			[u_past_form, u_verb, number],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(n), #\(o), #\(t)])
		      ],
		      
		      [ u_string_append,
			[u_past_form, [quote, do], number],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(n), #\(o), #\(t), #\(' ')]),
			[u_infinitive_form, u_verb]
		      ]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_subjunctive]],
		    
		    [ if,
		      [u_eq_c63, u_verb, [quote, u_be]],
		      
		      [ u_string_append,
			[u_past_subjunctive_form, u_verb, number],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(n), #\(o), #\(t)])
		      ],
		      
		      [ u_string_append,
			[u_past_subjunctive_form, [quote, do], number],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(n), #\(o), #\(t), #\(' ')]),
			[u_infinitive_form, u_verb]
		      ]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_gerund]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(n), #\(o), #\(t), #\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_gerund_progressive]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(n), #\(o), #\(t), #\(' ')]),
		      [u_ing_form, [quote, u_be]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_have_to_gerund]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(n), #\(o), #\(t), #\(' ')]),
		      [u_ing_form, [quote, u_have]],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(' '), #\(t), #\(o), #\(' ')]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_have_to_progressive]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(c),
				 #\(a),
				 #\(n),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(b),
				 #\(e),
				 #\(' ')
			       ]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_infinitive_progressive]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(n), #\(o), #\(t), #\(' ')]),
		      [u_inf_to, u_switches],
		      '$ARRAY'([*], claz_base_character, [#\(b), #\(e), #\(' ')]),
		      [u_ing_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_infinitive]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(n), #\(o), #\(t), #\(' ')]),
		      [u_inf_to, u_switches],
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_gerund]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(n), #\(o), #\(t), #\(' ')]),
		      [u_ing_form, [quote, u_have]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_past_infinitive]],
		    
		    [ u_string_append,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(n), #\(o), #\(t), #\(' ')]),
		      [u_inf_to, u_switches],
		      [u_infinitive_form, [quote, u_have]],
		      '$ARRAY'([*], claz_base_character, [#\(' ')]),
		      [u_en_form, u_verb]
		    ]
		  ],
		  
		  [ [u_eq_c63, u_tense, [quote, u_present_have_to]],
		    
		    [ u_string_append,
		      [u_present_form, [quote, do], number],
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(v),
				 #\(e),
				 #\(' '),
				 #\(t),
				 #\(o),
				 #\(' ')
			       ]),
		      [u_infinitive_form, u_verb]
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ if,
		      [u_eq_c63, u_verb, [quote, u_be]],
		      
		      [ u_string_append,
			[u_present_form, u_verb, number],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(n), #\(o), #\(t)])
		      ],
		      
		      [ u_string_append,
			[u_present_form, [quote, do], number],
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(n), #\(o), #\(t), #\(' ')]),
			[u_infinitive_form, u_verb]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GET-NEGATIVE-VERB-FORM 
wl: arglist_info(u_get_negative_verb_form,
		[u_verb, number, u_tense, u_switches],
		[Verb_Param, Number_Param, Tense_Param, Switches_Param],
		arginfo{ all:[u_verb, number, u_tense, u_switches],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb, number, u_tense, u_switches],
			 opt:0,
			 req:[u_verb, number, u_tense, u_switches],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-NEGATIVE-VERB-FORM 
wl: init_args(exact_only, u_get_negative_verb_form).


% annotating U::GET-NEGATIVE-VERB-FORM 
f_u_get_negative_verb_form(Verb_Param, Number_Param, Tense_Param, Switches_Param, TrueResult80) :-
	Env=[bv(u_verb, Verb_Param), bv(number, Number_Param), bv(u_tense, Tense_Param), bv(u_switches, Switches_Param)],
	f_u_modal_aux_c63(Verb_Param, IFTEST),
	(   IFTEST\==[]
	->  f_u_string_append(
			      [ [u_present_form, u_verb, number],
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(n), #\(o), #\(t)])
			      ],
			      TrueResult126),
	    TrueResult80=TrueResult126
	;   f_u_eq_c63(u_tense, [quote, u_future_progressive], IFTEST21),
	    (   IFTEST21\==[]
	    ->  f_u_string_append(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\(w),
					       #\(i),
					       #\(l),
					       #\(l),
					       #\(' '),
					       #\(n),
					       #\(o),
					       #\(t),
					       #\(' ')
					     ]),
				    [u_present_form, [quote, u_be], number],
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(' ')]),
				    [u_ing_form, u_verb]
				  ],
				  TrueResult124),
		TrueResult80=TrueResult124
	    ;   f_u_eq_c63(u_tense, [quote, u_present_progressive], IFTEST23),
		(   IFTEST23\==[]
		->  f_u_string_append(
				      [ [u_present_form, [quote, u_be], number],
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(' '),
						   #\(n),
						   #\(o),
						   #\(t),
						   #\(' ')
						 ]),
					[u_ing_form, u_verb]
				      ],
				      TrueResult122),
		    TrueResult80=TrueResult122
		;   f_u_eq_c63(u_tense, [quote, u_past_progressive], IFTEST25),
		    (   IFTEST25\==[]
		    ->  f_u_string_append(
					  [ [u_past_form, [quote, u_be], number],
					    '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\(' '),
						       #\(n),
						       #\(o),
						       #\(t),
						       #\(' ')
						     ]),
					    [u_ing_form, u_verb]
					  ],
					  TrueResult120),
			TrueResult80=TrueResult120
		    ;   f_u_eq_c63(u_tense,
				   [quote, u_past_subjunctive_progressive],
				   IFTEST27),
			(   IFTEST27\==[]
			->  f_u_string_append(
					      [ 
						[ u_past_subjunctive_form,
						  [quote, u_be],
						  number
						],
						'$ARRAY'([*],
							 claz_base_character,
							 
							 [ #\(' '),
							   #\(n),
							   #\(o),
							   #\(t),
							   #\(' ')
							 ]),
						[u_ing_form, u_verb]
					      ],
					      TrueResult118),
			    TrueResult80=TrueResult118
			;   f_u_eq_c63(u_tense,
				       [quote, u_present_perfect],
				       IFTEST29),
			    (   IFTEST29\==[]
			    ->  f_u_string_append(
						  [ 
						    [ u_present_form,
						      [quote, u_have],
						      number
						    ],
						    '$ARRAY'([*],
							     claz_base_character,
							     
							     [ #\(' '),
							       #\(n),
							       #\(o),
							       #\(t),
							       #\(' ')
							     ]),
						    [u_en_form, u_verb]
						  ],
						  TrueResult116),
				TrueResult80=TrueResult116
			    ;   f_u_eq_c63(u_tense,
					   
					   [ quote,
					     u_conditional_present_perfect
					   ],
					   IFTEST31),
				(   IFTEST31\==[]
				->  f_u_string_append(
						      [ '$ARRAY'([*],
								 claz_base_character,
								 
								 [ #\(w),
								   #\(o),
								   #\(u),
								   #\(l),
								   #\(d),
								   #\(' '),
								   #\(n),
								   #\(o),
								   #\(t),
								   #\(' ')
								 ]),
							
							[ u_infinitive_form,
							  [quote, u_have]
							],
							'$ARRAY'([*],
								 claz_base_character,
								 [#\(' ')]),
							[u_en_form, u_verb]
						      ],
						      TrueResult114),
				    TrueResult80=TrueResult114
				;   f_u_eq_c63(u_tense,
					       
					       [ quote,
						 u_conditional_have_to_present_perfect
					       ],
					       IFTEST33),
				    (   IFTEST33\==[]
				    ->  f_u_string_append(
							  [ '$ARRAY'([*],
								     claz_base_character,
								     
								     [ #\(w),
								       #\(o),
								       #\(u),
								       #\(l),
								       #\(d),
								       #\(' '),
								       #\(n),
								       #\(o),
								       #\(t),
								       #\(' '),
								       #\(h),
								       #\(a),
								       #\(v),
								       #\(e),
								       #\(' '),
								       #\(t),
								       #\(o),
								       #\(' ')
								     ]),
							    
							    [ u_infinitive_form,
							      [quote, u_have]
							    ],
							    '$ARRAY'([*],
								     claz_base_character,
								     [#\(' ')]),
							    [u_en_form, u_verb]
							  ],
							  TrueResult112),
					TrueResult80=TrueResult112
				    ;   f_u_eq_c63(u_tense,
						   [quote, u_past_perfect],
						   IFTEST35),
					(   IFTEST35\==[]
					->  f_u_string_append(
							      [ '$ARRAY'([*],
									 claz_base_character,
									 
									 [ #\(h),
									   #\(a),
									   #\(d),
									   #\(' '),
									   #\(n),
									   #\(o),
									   #\(t),
									   #\(' ')
									 ]),
								[u_en_form, u_verb]
							      ],
							      TrueResult110),
					    TrueResult80=TrueResult110
					;   f_u_eq_c63(u_tense,
						       [quote, u_future],
						       IFTEST37),
					    (   IFTEST37\==[]
					    ->  f_u_string_append(
								  [ '$ARRAY'([*],
									     claz_base_character,
									     
									     [ #\(w),
									       #\(i),
									       #\(l),
									       #\(l),
									       #\(' '),
									       #\(n),
									       #\(o),
									       #\(t),
									       #\(' ')
									     ]),
								    
								    [ u_infinitive_form,
								      u_verb
								    ]
								  ],
								  TrueResult108),
						TrueResult80=TrueResult108
					    ;   f_u_eq_c63(u_tense,
							   
							   [ quote,
							     u_conditional
							   ],
							   IFTEST39),
						(   IFTEST39\==[]
						->  f_u_string_append(
								      [ '$ARRAY'([*],
										 claz_base_character,
										 
										 [ #\(w),
										   #\(o),
										   #\(u),
										   #\(l),
										   #\(d),
										   #\(' '),
										   #\(n),
										   #\(o),
										   #\(t),
										   #\(' ')
										 ]),
									
									[ u_infinitive_form,
									  u_verb
									]
								      ],
								      TrueResult106),
						    TrueResult80=TrueResult106
						;   f_u_eq_c63(u_tense,
							       
							       [ quote,
								 u_conditional_have_to
							       ],
							       IFTEST41),
						    (   IFTEST41\==[]
						    ->  f_u_string_append(
									  [ '$ARRAY'([*],
										     claz_base_character,
										     
										     [ #\(w),
										       #\(o),
										       #\(u),
										       #\(l),
										       #\(d),
										       #\(' '),
										       #\(n),
										       #\(o),
										       #\(t),
										       #\(' '),
										       #\(h),
										       #\(a),
										       #\(v),
										       #\(e),
										       #\(' '),
										       #\(t),
										       #\(o),
										       #\(' ')
										     ]),
									    
									    [ u_infinitive_form,
									      u_verb
									    ]
									  ],
									  TrueResult104),
							TrueResult80=TrueResult104
						    ;   f_u_eq_c63(u_tense,
								   [quote, u_past],
								   IFTEST43),
							(   IFTEST43\==[]
							->  f_u_eq_c63(u_verb,
								       [quote, u_be],
								       IFTEST45),
							    (   IFTEST45\==[]
							    ->  f_u_string_append(
										  [ 
										    [ u_past_form,
										      u_verb,
										      number
										    ],
										    '$ARRAY'([*],
											     claz_base_character,
											     
											     [ #\(' '),
											       #\(n),
											       #\(o),
											       #\(t)
											     ])
										  ],
										  TrueResult),
								TrueResult80=TrueResult
							    ;   f_u_string_append(
										  [ 
										    [ u_past_form,
										      [quote, do],
										      number
										    ],
										    '$ARRAY'([*],
											     claz_base_character,
											     
											     [ #\(' '),
											       #\(n),
											       #\(o),
											       #\(t),
											       #\(' ')
											     ]),
										    
										    [ u_infinitive_form,
										      u_verb
										    ]
										  ],
										  ElseResult),
								TrueResult80=ElseResult
							    )
							;   f_u_eq_c63(u_tense,
								       
								       [ quote,
									 u_past_subjunctive
								       ],
								       IFTEST49),
							    (   IFTEST49\==[]
							    ->  f_u_eq_c63(u_verb,
									   [quote, u_be],
									   IFTEST51),
								(   IFTEST51\==[]
								->  f_u_string_append(
										      [ 
											[ u_past_subjunctive_form,
											  u_verb,
											  number
											],
											'$ARRAY'([*],
												 claz_base_character,
												 
												 [ #\(' '),
												   #\(n),
												   #\(o),
												   #\(t)
												 ])
										      ],
										      TrueResult53),
								    TrueResult80=TrueResult53
								;   f_u_string_append(
										      [ 
											[ u_past_subjunctive_form,
											  [quote, do],
											  number
											],
											'$ARRAY'([*],
												 claz_base_character,
												 
												 [ #\(' '),
												   #\(n),
												   #\(o),
												   #\(t),
												   #\(' ')
												 ]),
											
											[ u_infinitive_form,
											  u_verb
											]
										      ],
										      ElseResult54),
								    TrueResult80=ElseResult54
								)
							    ;   f_u_eq_c63(u_tense,
									   [quote, u_gerund],
									   IFTEST55),
								(   IFTEST55\==[]
								->  f_u_string_append(
										      [ '$ARRAY'([*],
												 claz_base_character,
												 
												 [ #\(n),
												   #\(o),
												   #\(t),
												   #\(' ')
												 ]),
											[u_ing_form, u_verb]
										      ],
										      TrueResult98),
								    TrueResult80=TrueResult98
								;   f_u_eq_c63(u_tense,
									       
									       [ quote,
										 u_gerund_progressive
									       ],
									       IFTEST57),
								    (   IFTEST57\==[]
								    ->  f_u_string_append(
											  [ '$ARRAY'([*],
												     claz_base_character,
												     
												     [ #\(n),
												       #\(o),
												       #\(t),
												       #\(' ')
												     ]),
											    
											    [ u_ing_form,
											      [quote, u_be]
											    ],
											    '$ARRAY'([*],
												     claz_base_character,
												     [#\(' ')]),
											    [u_ing_form, u_verb]
											  ],
											  TrueResult96),
									TrueResult80=TrueResult96
								    ;   f_u_eq_c63(u_tense,
										   
										   [ quote,
										     u_have_to_gerund
										   ],
										   IFTEST59),
									(   IFTEST59\==[]
									->  f_u_string_append(
											      [ '$ARRAY'([*],
													 claz_base_character,
													 
													 [ #\(n),
													   #\(o),
													   #\(t),
													   #\(' ')
													 ]),
												
												[ u_ing_form,
												  [quote, u_have]
												],
												'$ARRAY'([*],
													 claz_base_character,
													 
													 [ #\(' '),
													   #\(t),
													   #\(o),
													   #\(' ')
													 ]),
												
												[ u_infinitive_form,
												  u_verb
												]
											      ],
											      TrueResult94),
									    TrueResult80=TrueResult94
									;   f_u_eq_c63(u_tense,
										       
										       [ quote,
											 u_have_to_progressive
										       ],
										       IFTEST61),
									    (   IFTEST61\==[]
									    ->  f_u_string_append(
												  [ '$ARRAY'([*],
													     claz_base_character,
													     
													     [ #\(c),
													       #\(a),
													       #\(n),
													       #\(n),
													       #\(o),
													       #\(t),
													       #\(' '),
													       #\(b),
													       #\(e),
													       #\(' ')
													     ]),
												    [u_ing_form, u_verb]
												  ],
												  TrueResult92),
										TrueResult80=TrueResult92
									    ;   f_u_eq_c63(u_tense,
											   
											   [ quote,
											     u_infinitive_progressive
											   ],
											   IFTEST63),
										(   IFTEST63\==[]
										->  f_u_string_append(
												      [ '$ARRAY'([*],
														 claz_base_character,
														 
														 [ #\(n),
														   #\(o),
														   #\(t),
														   #\(' ')
														 ]),
													
													[ u_inf_to,
													  u_switches
													],
													'$ARRAY'([*],
														 claz_base_character,
														 
														 [ #\(b),
														   #\(e),
														   #\(' ')
														 ]),
													[u_ing_form, u_verb]
												      ],
												      TrueResult90),
										    TrueResult80=TrueResult90
										;   f_u_eq_c63(u_tense,
											       
											       [ quote,
												 u_infinitive
											       ],
											       IFTEST65),
										    (   IFTEST65\==[]
										    ->  f_u_string_append(
													  [ '$ARRAY'([*],
														     claz_base_character,
														     
														     [ #\(n),
														       #\(o),
														       #\(t),
														       #\(' ')
														     ]),
													    
													    [ u_inf_to,
													      u_switches
													    ],
													    
													    [ u_infinitive_form,
													      u_verb
													    ]
													  ],
													  TrueResult88),
											TrueResult80=TrueResult88
										    ;   f_u_eq_c63(u_tense,
												   
												   [ quote,
												     u_past_gerund
												   ],
												   IFTEST67),
											(   IFTEST67\==[]
											->  f_u_string_append(
													      [ '$ARRAY'([*],
															 claz_base_character,
															 
															 [ #\(n),
															   #\(o),
															   #\(t),
															   #\(' ')
															 ]),
														
														[ u_ing_form,
														  [quote, u_have]
														],
														'$ARRAY'([*],
															 claz_base_character,
															 [#\(' ')]),
														[u_en_form, u_verb]
													      ],
													      TrueResult86),
											    TrueResult80=TrueResult86
											;   f_u_eq_c63(u_tense,
												       
												       [ quote,
													 u_past_infinitive
												       ],
												       IFTEST69),
											    (   IFTEST69\==[]
											    ->  f_u_string_append(
														  [ '$ARRAY'([*],
															     claz_base_character,
															     
															     [ #\(n),
															       #\(o),
															       #\(t),
															       #\(' ')
															     ]),
														    
														    [ u_inf_to,
														      u_switches
														    ],
														    
														    [ u_infinitive_form,
														      [quote, u_have]
														    ],
														    '$ARRAY'([*],
															     claz_base_character,
															     [#\(' ')]),
														    [u_en_form, u_verb]
														  ],
														  TrueResult84),
												TrueResult80=TrueResult84
											    ;   f_u_eq_c63(u_tense,
													   
													   [ quote,
													     u_present_have_to
													   ],
													   IFTEST71),
												(   IFTEST71\==[]
												->  f_u_string_append(
														      [ 
															[ u_present_form,
															  [quote, do],
															  number
															],
															'$ARRAY'([*],
																 claz_base_character,
																 
																 [ #\(n),
																   #\(o),
																   #\(t),
																   #\(' '),
																   #\(h),
																   #\(a),
																   #\(v),
																   #\(e),
																   #\(' '),
																   #\(t),
																   #\(o),
																   #\(' ')
																 ]),
															
															[ u_infinitive_form,
															  u_verb
															]
														      ],
														      TrueResult82),
												    TrueResult80=TrueResult82
												;   get_var(Env,
													    u_else,
													    IFTEST73),
												    (   IFTEST73\==[]
												    ->  f_u_eq_c63(u_verb,
														   [quote, u_be],
														   IFTEST76),
													(   IFTEST76\==[]
													->  f_u_string_append(
															      [ 
																[ u_present_form,
																  u_verb,
																  number
																],
																'$ARRAY'([*],
																	 claz_base_character,
																	 
																	 [ #\(' '),
																	   #\(n),
																	   #\(o),
																	   #\(t)
																	 ])
															      ],
															      TrueResult78),
													    TrueResult80=TrueResult78
													;   f_u_string_append(
															      [ 
																[ u_present_form,
																  [quote, do],
																  number
																],
																'$ARRAY'([*],
																	 claz_base_character,
																	 
																	 [ #\(' '),
																	   #\(n),
																	   #\(o),
																	   #\(t),
																	   #\(' ')
																	 ]),
																
																[ u_infinitive_form,
																  u_verb
																]
															      ],
															      ElseResult79),
													    TrueResult80=ElseResult79
													)
												    ;   TrueResult80=[]
												    )
												)
											    )
											)
										    )
										)
									    )
									)
								    )
								)
							    )
							)
						    )
						)
					    )
					)
				    )
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_get_negative_verb_form, classof, claz_function),
   set_opv(u_get_negative_verb_form, compile_as, kw_function),
   set_opv(u_get_negative_verb_form, function, f_u_get_negative_verb_form),
   DefunResult=u_get_negative_verb_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:75865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Old version of above.", 1, 78058)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:75865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   ((eq? tense 'have-to-progressive)",
				     1,
				     78082)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:75865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("    (string-append (present-form 'do number) \" not have to be \"",
				     1,
				     78120)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:75865 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                   (ing-form verb)))",
				     1,
				     78185)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:75865 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" default is present", 10, 78775)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:79008 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'infinitive-form',
			    [verb],
			    ['string-downcase!', ['symbol->string', verb]]
			  ]).

% annotating U::INFINITIVE-FORM 
wl: lambda_def(defun,
	      u_infinitive_form,
	      f_u_infinitive_form,
	      [u_verb],
	      [[u_string_downcase_c33, [u_symbol_c62_string, u_verb]]]).


% annotating U::INFINITIVE-FORM 
wl: arglist_info(u_infinitive_form,
		[u_verb],
		[Verb_Param],
		arginfo{ all:[u_verb],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb],
			 opt:0,
			 req:[u_verb],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INFINITIVE-FORM 
wl: init_args(exact_only, u_infinitive_form).


% annotating U::INFINITIVE-FORM 
f_u_infinitive_form(Verb_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param)],
	f_u_string_downcase_c33([u_symbol_c62_string, u_verb], Downcase_c33_Ret),
	Downcase_c33_Ret=FnResult.
:- set_opv(f_u_infinitive_form, classof, claz_function),
   set_opv(u_infinitive_form, compile_as, kw_function),
   set_opv(u_infinitive_form, function, f_u_infinitive_form),
   DefunResult=u_infinitive_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:79083 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ progn,
			    
			    [ setq,
			      '*present-forms*',
			      
			      [ quote,
				
				[ 
				  [ be,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("am")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("is")
				      ],
				      [plural, '$STRING'("are")]
				    ]
				  ],
				  
				  [ do,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("do")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("does")
				      ],
				      [plural, '$STRING'("do")]
				    ]
				  ],
				  
				  [ go,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("go")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("goes")
				      ],
				      [plural, '$STRING'("go")]
				    ]
				  ],
				  
				  [ have,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("have")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("has")
				      ],
				      [plural, '$STRING'("have")]
				    ]
				  ],
				  
				  [ would,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("would")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("would")
				      ],
				      [plural, '$STRING'("would")]
				    ]
				  ]
				]
			      ]
			    ],
			    []
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_present_forms_xx,
	   
	   [ 
	     [ u_be,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(a), #\(m)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(i), #\(s)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(a), #\(r), #\(e)])
		 ]
	       ]
	     ],
	     
	     [ do,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(d), #\(o)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(d), #\(o), #\(e), #\(s)])
		 ],
		 [u_plural, '$ARRAY'([*], claz_base_character, [#\(d), #\(o)])]
	       ]
	     ],
	     
	     [ go,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(g), #\(o)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(g), #\(o), #\(e), #\(s)])
		 ],
		 [u_plural, '$ARRAY'([*], claz_base_character, [#\(g), #\(o)])]
	       ]
	     ],
	     
	     [ u_have,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(v), #\(e)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(s)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(v), #\(e)])
		 ]
	       ]
	     ],
	     
	     [ u_would,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(w), #\(o), #\(u), #\(l), #\(d)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(w), #\(o), #\(u), #\(l), #\(d)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(w), #\(o), #\(u), #\(l), #\(d)])
		 ]
	       ]
	     ]
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:79953 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'present-form',
			    [verb, number],
			    
			    [ let,
			      [[found1, [assq, verb, '*present-forms*']]],
			      
			      [ if,
				found1,
				
				[ let,
				  [[found2, [assq, number, [cadr, found1]]]],
				  
				  [ if,
				    found2,
				    [cadr, found2],
				    ['derive-present-form', verb, number]
				  ]
				],
				['derive-present-form', verb, number]
			      ]
			    ]
			  ]).

% annotating U::PRESENT-FORM 
wl: lambda_def(defun,
	      u_present_form,
	      f_u_present_form,
	      [u_verb, number],
	      
	      [ 
		[ let,
		  [[u_found1, [ext_assq, u_verb, u_xx_present_forms_xx]]],
		  
		  [ if,
		    u_found1,
		    
		    [ let,
		      [[u_found2, [ext_assq, number, [cadr, u_found1]]]],
		      
		      [ if,
			u_found2,
			[cadr, u_found2],
			[u_derive_present_form, u_verb, number]
		      ]
		    ],
		    [u_derive_present_form, u_verb, number]
		  ]
		]
	      ]).


% annotating U::PRESENT-FORM 
wl: arglist_info(u_present_form,
		[u_verb, number],
		[Verb_Param, Number_Param],
		arginfo{ all:[u_verb, number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb, number],
			 opt:0,
			 req:[u_verb, number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRESENT-FORM 
wl: init_args(exact_only, u_present_form).


% annotating U::PRESENT-FORM 
f_u_present_form(Verb_Param, Number_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param), bv(number, Number_Param)],
	f_ext_assq(u_verb, u_xx_present_forms_xx, Found1_Init),
	LEnv=[[bv(u_found1, Found1_Init)]|Env],
	get_var(LEnv, u_found1, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(number, [cadr, u_found1], Found2_Init),
	    Env=[[bv(u_found2, Found2_Init)]|LEnv],
	    get_var(Env, u_found2, IFTEST24),
	    (   IFTEST24\==[]
	    ->  get_var(Env, u_found2, Found2_Get28),
		cl_cadr(Found2_Get28, TrueResult),
		FnResult=TrueResult
	    ;   f_u_derive_present_form(Verb_Param, number, ElseResult),
		FnResult=ElseResult
	    )
	;   f_u_derive_present_form(Verb_Param, number, ElseResult35),
	    FnResult=ElseResult35
	).
:- set_opv(f_u_present_form, classof, claz_function),
   set_opv(u_present_form, compile_as, kw_function),
   set_opv(u_present_form, function, f_u_present_form),
   DefunResult=u_present_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:80232 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'derive-present-form',
			    [verb, number],
			    
			    [ let,
			      [[str, ['infinitive-form', verb]]],
			      
			      [ if,
				['eq?', number, [quote, 'third-person-singular']],
				
				[ 'string-append',
				  str,
				  
				  [ if,
				    
				    [ 'string-equal?',
				      ['last-char', str],
				      '$STRING'("s")
				    ],
				    '$STRING'("es"),
				    '$STRING'("s")
				  ]
				],
				str
			      ]
			    ]
			  ]).

% annotating U::DERIVE-PRESENT-FORM 
wl: lambda_def(defun,
	      u_derive_present_form,
	      f_u_derive_present_form,
	      [u_verb, number],
	      
	      [ 
		[ let,
		  [[u_str, [u_infinitive_form, u_verb]]],
		  
		  [ if,
		    [u_eq_c63, number, [quote, u_third_person_singular]],
		    
		    [ u_string_append,
		      u_str,
		      
		      [ if,
			
			[ u_string_equal_c63,
			  [u_last_char, u_str],
			  '$ARRAY'([*], claz_base_character, [#\(s)])
			],
			'$ARRAY'([*], claz_base_character, [#\(e), #\(s)]),
			'$ARRAY'([*], claz_base_character, [#\(s)])
		      ]
		    ],
		    u_str
		  ]
		]
	      ]).


% annotating U::DERIVE-PRESENT-FORM 
wl: arglist_info(u_derive_present_form,
		[u_verb, number],
		[Verb_Param, Number_Param],
		arginfo{ all:[u_verb, number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb, number],
			 opt:0,
			 req:[u_verb, number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DERIVE-PRESENT-FORM 
wl: init_args(exact_only, u_derive_present_form).


% annotating U::DERIVE-PRESENT-FORM 
f_u_derive_present_form(Verb_Param, Number_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param), bv(number, Number_Param)],
	f_u_infinitive_form(Verb_Param, Str_Init),
	LEnv=[[bv(u_str, Str_Init)]|Env],
	f_u_eq_c63(number, [quote, u_third_person_singular], IFTEST),
	(   IFTEST\==[]
	->  f_u_string_append(
			      [ u_str,
				
				[ if,
				  
				  [ u_string_equal_c63,
				    [u_last_char, u_str],
				    '$ARRAY'([*], claz_base_character, [#\(s)])
				  ],
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\(e), #\(s)]),
				  '$ARRAY'([*], claz_base_character, [#\(s)])
				]
			      ],
			      TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_str, Str_Get),
	    FnResult=Str_Get
	).
:- set_opv(f_u_derive_present_form, classof, claz_function),
   set_opv(u_derive_present_form, compile_as, kw_function),
   set_opv(u_derive_present_form, function, f_u_derive_present_form),
   DefunResult=u_derive_present_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:80232 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (string-append str (if (vowel? (last-char str)) \"es\" \"s\"))",
				     1,
				     80356)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:80517 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ progn,
			    
			    [ setq,
			      '*past-forms*',
			      
			      [ quote,
				
				[ 
				  [ be,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("was")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("was")
				      ],
				      [plural, '$STRING'("were")]
				    ]
				  ],
				  
				  [ have,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("had")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("had")
				      ],
				      [plural, '$STRING'("had")]
				    ]
				  ],
				  
				  [ go,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("went")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("went")
				      ],
				      [plural, '$STRING'("went")]
				    ]
				  ],
				  
				  [ know,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("knew")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("knew")
				      ],
				      [plural, '$STRING'("knew")]
				    ]
				  ],
				  
				  [ read,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("read")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("read")
				      ],
				      [plural, '$STRING'("read")]
				    ]
				  ],
				  
				  [ tell,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("told")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("told")
				      ],
				      [plural, '$STRING'("told")]
				    ]
				  ],
				  
				  [ feel,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("felt")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("felt")
				      ],
				      [plural, '$STRING'("felt")]
				    ]
				  ],
				  
				  [ think,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("thought")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("thought")
				      ],
				      [plural, '$STRING'("thought")]
				    ]
				  ],
				  
				  [ lead,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("led")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("led")
				      ],
				      [plural, '$STRING'("led")]
				    ]
				  ],
				  
				  [ would,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("would")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("would")
				      ],
				      [plural, '$STRING'("would")]
				    ]
				  ],
				  
				  [ ydo,
				    
				    [ 
				      [ 'first-person-singular',
					'$STRING'("did")
				      ],
				      
				      [ 'third-person-singular',
					'$STRING'("did")
				      ],
				      [plural, '$STRING'("did")]
				    ]
				  ]
				]
			      ]
			    ],
			    []
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_past_forms_xx,
	   
	   [ 
	     [ u_be,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(w), #\(a), #\(s)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(w), #\(a), #\(s)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(w), #\(e), #\(r), #\(e)])
		 ]
	       ]
	     ],
	     
	     [ u_have,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(d)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(d)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(d)])
		 ]
	       ]
	     ],
	     
	     [ go,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(w), #\(e), #\(n), #\(t)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(w), #\(e), #\(n), #\(t)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(w), #\(e), #\(n), #\(t)])
		 ]
	       ]
	     ],
	     
	     [ u_know,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(k), #\(n), #\(e), #\(w)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(k), #\(n), #\(e), #\(w)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(k), #\(n), #\(e), #\(w)])
		 ]
	       ]
	     ],
	     
	     [ read,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(r), #\(e), #\(a), #\(d)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(r), #\(e), #\(a), #\(d)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(r), #\(e), #\(a), #\(d)])
		 ]
	       ]
	     ],
	     
	     [ u_tell,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(t), #\(o), #\(l), #\(d)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(t), #\(o), #\(l), #\(d)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(t), #\(o), #\(l), #\(d)])
		 ]
	       ]
	     ],
	     
	     [ u_feel,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(f), #\(e), #\(l), #\(t)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(f), #\(e), #\(l), #\(t)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(f), #\(e), #\(l), #\(t)])
		 ]
	       ]
	     ],
	     
	     [ u_think,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(t), #\(h), #\(o), #\(u), #\(g), #\(h), #\(t)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(t), #\(h), #\(o), #\(u), #\(g), #\(h), #\(t)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(t), #\(h), #\(o), #\(u), #\(g), #\(h), #\(t)])
		 ]
	       ]
	     ],
	     
	     [ u_lead,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(l), #\(e), #\(d)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(l), #\(e), #\(d)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(l), #\(e), #\(d)])
		 ]
	       ]
	     ],
	     
	     [ u_would,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(w), #\(o), #\(u), #\(l), #\(d)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(w), #\(o), #\(u), #\(l), #\(d)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*],
			    claz_base_character,
			    [#\(w), #\(o), #\(u), #\(l), #\(d)])
		 ]
	       ]
	     ],
	     
	     [ u_ydo,
	       
	       [ 
		 [ u_first_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(d), #\(i), #\(d)])
		 ],
		 
		 [ u_third_person_singular,
		   '$ARRAY'([*], claz_base_character, [#\(d), #\(i), #\(d)])
		 ],
		 
		 [ u_plural,
		   '$ARRAY'([*], claz_base_character, [#\(d), #\(i), #\(d)])
		 ]
	       ]
	     ]
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:82358 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'past-form',
			    [verb, number],
			    
			    [ let,
			      [[found1, [assq, verb, '*past-forms*']]],
			      
			      [ if,
				found1,
				
				[ let,
				  [[found2, [assq, number, [cadr, found1]]]],
				  
				  [ if,
				    found2,
				    [cadr, found2],
				    ['derive-past-form', verb, number]
				  ]
				],
				['derive-past-form', verb, number]
			      ]
			    ]
			  ]).

% annotating U::PAST-FORM 
wl: lambda_def(defun,
	      u_past_form,
	      f_u_past_form,
	      [u_verb, number],
	      
	      [ 
		[ let,
		  [[u_found1, [ext_assq, u_verb, u_xx_past_forms_xx]]],
		  
		  [ if,
		    u_found1,
		    
		    [ let,
		      [[u_found2, [ext_assq, number, [cadr, u_found1]]]],
		      
		      [ if,
			u_found2,
			[cadr, u_found2],
			[u_derive_past_form, u_verb, number]
		      ]
		    ],
		    [u_derive_past_form, u_verb, number]
		  ]
		]
	      ]).


% annotating U::PAST-FORM 
wl: arglist_info(u_past_form,
		[u_verb, number],
		[Verb_Param, Number_Param],
		arginfo{ all:[u_verb, number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb, number],
			 opt:0,
			 req:[u_verb, number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PAST-FORM 
wl: init_args(exact_only, u_past_form).


% annotating U::PAST-FORM 
f_u_past_form(Verb_Param, Number_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param), bv(number, Number_Param)],
	f_ext_assq(u_verb, u_xx_past_forms_xx, Found1_Init),
	LEnv=[[bv(u_found1, Found1_Init)]|Env],
	get_var(LEnv, u_found1, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq(number, [cadr, u_found1], Found2_Init),
	    Env=[[bv(u_found2, Found2_Init)]|LEnv],
	    get_var(Env, u_found2, IFTEST24),
	    (   IFTEST24\==[]
	    ->  get_var(Env, u_found2, Found2_Get28),
		cl_cadr(Found2_Get28, TrueResult),
		FnResult=TrueResult
	    ;   f_u_derive_past_form(Verb_Param, number, ElseResult),
		FnResult=ElseResult
	    )
	;   f_u_derive_past_form(Verb_Param, number, ElseResult35),
	    FnResult=ElseResult35
	).
:- set_opv(f_u_past_form, classof, claz_function),
   set_opv(u_past_form, compile_as, kw_function),
   set_opv(u_past_form, function, f_u_past_form),
   DefunResult=u_past_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:82625 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'past-subjunctive-form',
			    [verb, number],
			    ['past-form', verb, [quote, plural]]
			  ]).

% annotating U::PAST-SUBJUNCTIVE-FORM 
wl: lambda_def(defun,
	      u_past_subjunctive_form,
	      f_u_past_subjunctive_form,
	      [u_verb, number],
	      [[u_past_form, u_verb, [quote, u_plural]]]).


% annotating U::PAST-SUBJUNCTIVE-FORM 
wl: arglist_info(u_past_subjunctive_form,
		[u_verb, number],
		[Verb_Param, Number_Param],
		arginfo{ all:[u_verb, number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb, number],
			 opt:0,
			 req:[u_verb, number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PAST-SUBJUNCTIVE-FORM 
wl: init_args(exact_only, u_past_subjunctive_form).


% annotating U::PAST-SUBJUNCTIVE-FORM 
f_u_past_subjunctive_form(Verb_Param, Number_Param, FnResult) :-
	Env=[bv(number, Number_Param)],
	f_u_past_form(Verb_Param, u_plural, Plural),
	Plural=FnResult.
:- set_opv(f_u_past_subjunctive_form, classof, claz_function),
   set_opv(u_past_subjunctive_form, compile_as, kw_function),
   set_opv(u_past_subjunctive_form, function, f_u_past_subjunctive_form),
   DefunResult=u_past_subjunctive_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:82697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'derive-past-form',
			    [verb, number],
			    
			    [ let,
			      [[str, ['infinitive-form', verb]]],
			      
			      [ 'string-append',
				str,
				
				[ if,
				  
				  [ 'string-equal?',
				    ['last-char', str],
				    '$STRING'("e")
				  ],
				  '$STRING'("d"),
				  '$STRING'("ed")
				]
			      ]
			    ]
			  ]).

% annotating U::DERIVE-PAST-FORM 
wl: lambda_def(defun,
	      u_derive_past_form,
	      f_u_derive_past_form,
	      [u_verb, number],
	      
	      [ 
		[ let,
		  [[u_str, [u_infinitive_form, u_verb]]],
		  
		  [ u_string_append,
		    u_str,
		    
		    [ if,
		      
		      [ u_string_equal_c63,
			[u_last_char, u_str],
			'$ARRAY'([*], claz_base_character, [#\(e)])
		      ],
		      '$ARRAY'([*], claz_base_character, [#\(d)]),
		      '$ARRAY'([*], claz_base_character, [#\(e), #\(d)])
		    ]
		  ]
		]
	      ]).


% annotating U::DERIVE-PAST-FORM 
wl: arglist_info(u_derive_past_form,
		[u_verb, number],
		[Verb_Param, Number_Param],
		arginfo{ all:[u_verb, number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb, number],
			 opt:0,
			 req:[u_verb, number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DERIVE-PAST-FORM 
wl: init_args(exact_only, u_derive_past_form).


% annotating U::DERIVE-PAST-FORM 
f_u_derive_past_form(Verb_Param, Number_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param), bv(number, Number_Param)],
	f_u_infinitive_form(Verb_Param, Str_Init),
	LEnv=[[bv(u_str, Str_Init)]|Env],
	f_u_string_append(
			  [ u_str,
			    
			    [ if,
			      
			      [ u_string_equal_c63,
				[u_last_char, u_str],
				'$ARRAY'([*], claz_base_character, [#\(e)])
			      ],
			      '$ARRAY'([*], claz_base_character, [#\(d)]),
			      '$ARRAY'([*], claz_base_character, [#\(e), #\(d)])
			    ]
			  ],
			  String_append_Ret),
	LetResult=String_append_Ret,
	LetResult=FnResult.
:- set_opv(f_u_derive_past_form, classof, claz_function),
   set_opv(u_derive_past_form, compile_as, kw_function),
   set_opv(u_derive_past_form, function, f_u_derive_past_form),
   DefunResult=u_derive_past_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:82850 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ progn,
			    
			    [ setq,
			      '*en-forms*',
			      
			      [ quote,
				
				[ [be, '$STRING'("been")],
				  [have, '$STRING'("had")],
				  [go, '$STRING'("gone")],
				  [tell, '$STRING'("told")],
				  [know, '$STRING'("known")],
				  [think, '$STRING'("thought")],
				  [agree, '$STRING'("agreed")],
				  [watch, '$STRING'("watched")],
				  [introduce, '$STRING'("introduced")],
				  [put, '$STRING'("put")],
				  [continue, '$STRING'("continued")],
				  [succeed, '$STRING'("succeeded")],
				  [fail, '$STRING'("failed")],
				  [break, '$STRING'("broken")],
				  [reverse, '$STRING'("reversed")],
				  [want, '$STRING'("wanted")]
				]
			      ]
			    ],
			    []
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_en_forms_xx,
	   
	   [ 
	     [ u_be,
	       '$ARRAY'([*], claz_base_character, [#\(b), #\(e), #\(e), #\(n)])
	     ],
	     [u_have, '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(d)])],
	     [go, '$ARRAY'([*], claz_base_character, [#\(g), #\(o), #\(n), #\(e)])],
	     
	     [ u_tell,
	       '$ARRAY'([*], claz_base_character, [#\(t), #\(o), #\(l), #\(d)])
	     ],
	     
	     [ u_know,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(k), #\(n), #\(o), #\(w), #\(n)])
	     ],
	     
	     [ u_think,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(t), #\(h), #\(o), #\(u), #\(g), #\(h), #\(t)])
	     ],
	     
	     [ u_agree,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(a), #\(g), #\(r), #\(e), #\(e), #\(d)])
	     ],
	     
	     [ u_watch,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(w), #\(a), #\(t), #\(c), #\(h), #\(e), #\(d)])
	     ],
	     
	     [ u_introduce,
	       '$ARRAY'([*],
			claz_base_character,
			
			[ #\(i),
			  #\(n),
			  #\(t),
			  #\(r),
			  #\(o),
			  #\(d),
			  #\(u),
			  #\(c),
			  #\(e),
			  #\(d)
			])
	     ],
	     [sys_put, '$ARRAY'([*], claz_base_character, [#\(p), #\(u), #\(t)])],
	     
	     [ continue,
	       '$ARRAY'([*],
			claz_base_character,
			
			[ #\(c),
			  #\(o),
			  #\(n),
			  #\(t),
			  #\(i),
			  #\(n),
			  #\(u),
			  #\(e),
			  #\(d)
			])
	     ],
	     
	     [ u_succeed,
	       '$ARRAY'([*],
			claz_base_character,
			
			[ #\(s),
			  #\(u),
			  #\(c),
			  #\(c),
			  #\(e),
			  #\(e),
			  #\(d),
			  #\(e),
			  #\(d)
			])
	     ],
	     
	     [ u_fail,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(f), #\(a), #\(i), #\(l), #\(e), #\(d)])
	     ],
	     
	     [ break,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(b), #\(r), #\(o), #\(k), #\(e), #\(n)])
	     ],
	     
	     [ reverse,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(r), #\(e), #\(v), #\(e), #\(r), #\(s), #\(e), #\(d)])
	     ],
	     
	     [ u_want,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(w), #\(a), #\(n), #\(t), #\(e), #\(d)])
	     ]
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:83297 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'en-form',
			    [verb],
			    
			    [ let,
			      [[found, [assq, verb, '*en-forms*']]],
			      
			      [ if,
				found,
				[cadr, found],
				
				[ let,
				  [[str, ['infinitive-form', verb]]],
				  
				  [ 'string-append',
				    str,
				    
				    [ if,
				      
				      [ 'string-equal?',
					['last-char', str],
					'$STRING'("e")
				      ],
				      '$STRING'("n"),
				      '$STRING'("en")
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::EN-FORM 
wl: lambda_def(defun,
	      u_en_form,
	      f_u_en_form,
	      [u_verb],
	      
	      [ 
		[ let,
		  [[u_found, [ext_assq, u_verb, u_xx_en_forms_xx]]],
		  
		  [ if,
		    u_found,
		    [cadr, u_found],
		    
		    [ let,
		      [[u_str, [u_infinitive_form, u_verb]]],
		      
		      [ u_string_append,
			u_str,
			
			[ if,
			  
			  [ u_string_equal_c63,
			    [u_last_char, u_str],
			    '$ARRAY'([*], claz_base_character, [#\(e)])
			  ],
			  '$ARRAY'([*], claz_base_character, [#\(n)]),
			  '$ARRAY'([*], claz_base_character, [#\(e), #\(n)])
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::EN-FORM 
wl: arglist_info(u_en_form,
		[u_verb],
		[Verb_Param],
		arginfo{ all:[u_verb],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb],
			 opt:0,
			 req:[u_verb],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EN-FORM 
wl: init_args(exact_only, u_en_form).


% annotating U::EN-FORM 
f_u_en_form(Verb_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param)],
	f_ext_assq(u_verb, u_xx_en_forms_xx, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get19),
	    cl_cadr(Found_Get19, TrueResult),
	    FnResult=TrueResult
	;   f_u_infinitive_form(Verb_Param, Str_Init),
	    LEnv20=[[bv(u_str, Str_Init)]|LEnv],
	    f_u_string_append(
			      [ u_str,
				
				[ if,
				  
				  [ u_string_equal_c63,
				    [u_last_char, u_str],
				    '$ARRAY'([*], claz_base_character, [#\(e)])
				  ],
				  '$ARRAY'([*], claz_base_character, [#\(n)]),
				  '$ARRAY'([*],
					   claz_base_character,
					   [#\(e), #\(n)])
				]
			      ],
			      String_append_Ret),
	    FnResult=String_append_Ret
	).
:- set_opv(f_u_en_form, classof, claz_function),
   set_opv(u_en_form, compile_as, kw_function),
   set_opv(u_en_form, function, f_u_en_form),
   DefunResult=u_en_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:83512 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ progn,
			    
			    [ setq,
			      '*ing-forms*',
			      
			      [ quote,
				
				[ [agree, '$STRING'("agreeing")],
				  [get, '$STRING'("getting")]
				]
			      ]
			    ],
			    []
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_ing_forms_xx,
	   
	   [ 
	     [ u_agree,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(a), #\(g), #\(r), #\(e), #\(e), #\(i), #\(n), #\(g)])
	     ],
	     
	     [ get,
	       '$ARRAY'([*],
			claz_base_character,
			[#\(g), #\(e), #\(t), #\(t), #\(i), #\(n), #\(g)])
	     ]
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:83627 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ing-form',
			    [verb],
			    
			    [ let,
			      [[found, [assq, verb, '*ing-forms*']]],
			      
			      [ if,
				found,
				[cadr, found],
				
				[ let,
				  [[str, ['infinitive-form', verb]]],
				  
				  [ if,
				    
				    [ and,
				      
				      [ 'string-equal?',
					['last-char', str],
					'$STRING'("e")
				      ],
				      
				      [ not,
					['string-equal?', str, '$STRING'("be")]
				      ]
				    ],
				    
				    [ 'string-append',
				      
				      [ substring,
					str,
					0,
					['-1+', ['string-length', str]]
				      ],
				      '$STRING'("ing")
				    ],
				    ['string-append', str, '$STRING'("ing")]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::ING-FORM 
wl: lambda_def(defun,
	      u_ing_form,
	      f_u_ing_form,
	      [u_verb],
	      
	      [ 
		[ let,
		  [[u_found, [ext_assq, u_verb, u_xx_ing_forms_xx]]],
		  
		  [ if,
		    u_found,
		    [cadr, u_found],
		    
		    [ let,
		      [[u_str, [u_infinitive_form, u_verb]]],
		      
		      [ if,
			
			[ and,
			  
			  [ u_string_equal_c63,
			    [u_last_char, u_str],
			    '$ARRAY'([*], claz_base_character, [#\(e)])
			  ],
			  
			  [ not,
			    
			    [ u_string_equal_c63,
			      u_str,
			      '$ARRAY'([*], claz_base_character, [#\(b), #\(e)])
			    ]
			  ]
			],
			
			[ u_string_append,
			  
			  [ ext_substring,
			    u_str,
			    0,
			    ['-1+', [u_string_length, u_str]]
			  ],
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(i), #\(n), #\(g)])
			],
			
			[ u_string_append,
			  u_str,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(i), #\(n), #\(g)])
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::ING-FORM 
wl: arglist_info(u_ing_form,
		[u_verb],
		[Verb_Param],
		arginfo{ all:[u_verb],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_verb],
			 opt:0,
			 req:[u_verb],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ING-FORM 
wl: init_args(exact_only, u_ing_form).


% annotating U::ING-FORM 
f_u_ing_form(Verb_Param, FnResult) :-
	Env=[bv(u_verb, Verb_Param)],
	f_ext_assq(u_verb, u_xx_ing_forms_xx, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get19),
	    cl_cadr(Found_Get19, TrueResult32),
	    FnResult=TrueResult32
	;   f_u_infinitive_form(Verb_Param, Str_Init),
	    LEnv20=[[bv(u_str, Str_Init)]|LEnv],
	    f_u_string_equal_c63([u_last_char, u_str],
				 '$ARRAY'([*], claz_base_character, [#\(e)]),
				 IFTEST26),
	    (   IFTEST26\==[]
	    ->  f_u_string_equal_c63(u_str,
				     '$ARRAY'([*],
					      claz_base_character,
					      [#\(b), #\(e)]),
				     Not_Param),
		cl_not(Not_Param, TrueResult),
		IFTEST24=TrueResult
	    ;   IFTEST24=[]
	    ),
	    (   IFTEST24\==[]
	    ->  f_u_string_append(
				  [ 
				    [ ext_substring,
				      u_str,
				      0,
				      ['-1+', [u_string_length, u_str]]
				    ],
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(i), #\(n), #\(g)])
				  ],
				  TrueResult29),
		FnResult=TrueResult29
	    ;   f_u_string_append(
				  [ u_str,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\(i), #\(n), #\(g)])
				  ],
				  ElseResult),
		FnResult=ElseResult
	    )
	).
:- set_opv(f_u_ing_form, classof, claz_function),
   set_opv(u_ing_form, compile_as, kw_function),
   set_opv(u_ing_form, function, f_u_ing_form),
   DefunResult=u_ing_form.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:83983 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'vowel?',
			    [str],
			    
			    [ or,
			      ['string-equal?', str, '$STRING'("a")],
			      ['string-equal?', str, '$STRING'("e")],
			      ['string-equal?', str, '$STRING'("i")],
			      ['string-equal?', str, '$STRING'("o")],
			      ['string-equal?', str, '$STRING'("u")]
			    ]
			  ]).

% annotating U::VOWEL? 
wl: lambda_def(defun,
	      u_vowel_c63,
	      f_u_vowel_c63,
	      [u_str],
	      
	      [ 
		[ or,
		  
		  [ u_string_equal_c63,
		    u_str,
		    '$ARRAY'([*], claz_base_character, [#\(a)])
		  ],
		  
		  [ u_string_equal_c63,
		    u_str,
		    '$ARRAY'([*], claz_base_character, [#\(e)])
		  ],
		  
		  [ u_string_equal_c63,
		    u_str,
		    '$ARRAY'([*], claz_base_character, [#\(i)])
		  ],
		  
		  [ u_string_equal_c63,
		    u_str,
		    '$ARRAY'([*], claz_base_character, [#\(o)])
		  ],
		  
		  [ u_string_equal_c63,
		    u_str,
		    '$ARRAY'([*], claz_base_character, [#\(u)])
		  ]
		]
	      ]).


% annotating U::VOWEL? 
wl: arglist_info(u_vowel_c63,
		[u_str],
		[Str_Param],
		arginfo{ all:[u_str],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_str],
			 opt:0,
			 req:[u_str],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VOWEL? 
wl: init_args(exact_only, u_vowel_c63).


% annotating U::VOWEL? 
f_u_vowel_c63(Str_Param, FnResult) :-
	Env=[bv(u_str, Str_Param)],
	(   f_u_string_equal_c63(u_str,
				 '$ARRAY'([*], claz_base_character, [#\(a)]),
				 FORM1_Res15),
	    FORM1_Res15\==[],
	    FnResult=FORM1_Res15
	->  true
	;   f_u_string_equal_c63(u_str,
				 '$ARRAY'([*], claz_base_character, [#\(e)]),
				 FORM1_Res14),
	    FORM1_Res14\==[],
	    FnResult=FORM1_Res14
	->  true
	;   f_u_string_equal_c63(u_str,
				 '$ARRAY'([*], claz_base_character, [#\(i)]),
				 FORM1_Res13),
	    FORM1_Res13\==[],
	    FnResult=FORM1_Res13
	->  true
	;   f_u_string_equal_c63(u_str,
				 '$ARRAY'([*], claz_base_character, [#\(o)]),
				 FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_string_equal_c63(u_str,
				 '$ARRAY'([*], claz_base_character, [#\(u)]),
				 Equal_c63_Ret),
	    FnResult=Equal_c63_Ret
	).
:- set_opv(f_u_vowel_c63, classof, claz_function),
   set_opv(u_vowel_c63, compile_as, kw_function),
   set_opv(u_vowel_c63, function, f_u_vowel_c63),
   DefunResult=u_vowel_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84156 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'last-char',
			    [string],
			    [nthchdr, string, [-, ['string-length', string], 1]]
			  ]).

% annotating U::LAST-CHAR 
wl: lambda_def(defun,
	      u_last_char,
	      f_u_last_char,
	      [string],
	      [[u_nthchdr, string, [-, [u_string_length, string], 1]]]).


% annotating U::LAST-CHAR 
wl: arglist_info(u_last_char,
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

% annotating U::LAST-CHAR 
wl: init_args(exact_only, u_last_char).


% annotating U::LAST-CHAR 
f_u_last_char(String_Param, FnResult) :-
	Env=[bv(string, String_Param)],
	f_u_nthchdr(string, [-, [u_string_length, string], 1], Nthchdr_Ret),
	Nthchdr_Ret=FnResult.
:- set_opv(f_u_last_char, classof, claz_function),
   set_opv(u_last_char, compile_as, kw_function),
   set_opv(u_last_char, function, f_u_last_char),
   DefunResult=u_last_char.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84233 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'first-char',
			    [string],
			    ['string-slice', string, 0, 1]
			  ]).

% annotating U::FIRST-CHAR 
wl: lambda_def(defun,
	      u_first_char,
	      f_u_first_char,
	      [string],
	      [[u_string_slice, string, 0, 1]]).


% annotating U::FIRST-CHAR 
wl: arglist_info(u_first_char,
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

% annotating U::FIRST-CHAR 
wl: init_args(exact_only, u_first_char).


% annotating U::FIRST-CHAR 
f_u_first_char(String_Param, FnResult) :-
	Env=[bv(string, String_Param)],
	f_u_string_slice(string, 0, 1, String_slice_Ret),
	String_slice_Ret=FnResult.
:- set_opv(f_u_first_char, classof, claz_function),
   set_opv(u_first_char, compile_as, kw_function),
   set_opv(u_first_char, function, f_u_first_char),
   DefunResult=u_first_char.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84233 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     84292)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84373 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defclass,
			    'gen-stream',
			    [],
			    
			    [ 
			      [ 'hpos-value',
				':initform',
				0,
				':accessor',
				'hpos-value'
			      ],
			      
			      [ 'line-length-value',
				':initform',
				[if, '*typeset?*', 100, 50],
				':accessor',
				'line-length-value'
			      ],
			      
			      [ 'start-sentence?',
				':initform',
				t,
				':accessor',
				'start-sentence?'
			      ],
			      
			      [ stream,
				':initarg',
				':stream',
				':accessor',
				'gen-stream-stream'
			      ]
			    ]
			  ]).
:- cl_defclass(
	       [ u_gen_stream,
		 [],
		 
		 [ [u_hpos_value, kw_initform, 0, kw_accessor, u_hpos_value],
		   
		   [ u_line_length_value,
		     kw_initform,
		     [if, u_xx_typeset_c63_xx, 100, 50],
		     kw_accessor,
		     u_line_length_value
		   ],
		   
		   [ u_start_sentence_c63,
		     kw_initform,
		     t,
		     kw_accessor,
		     u_start_sentence_c63
		   ],
		   
		   [ stream,
		     kw_initarg,
		     kw_stream,
		     kw_accessor,
		     u_gen_stream_stream
		   ]
		 ]
	       ],
	       _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84671 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-gen-stream',
			    [stream],
			    
			    [ 'make-instance',
			      [quote, 'gen-stream'],
			      ':stream',
			      stream
			    ]
			  ]).

% annotating U::MAKE-GEN-STREAM 
wl: lambda_def(defun,
	      u_make_gen_stream,
	      f_u_make_gen_stream,
	      [stream],
	      [[make_instance, [quote, u_gen_stream], kw_stream, stream]]).


% annotating U::MAKE-GEN-STREAM 
wl: arglist_info(u_make_gen_stream,
		[stream],
		[Stream_Param],
		arginfo{ all:[stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[stream],
			 opt:0,
			 req:[stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-GEN-STREAM 
wl: init_args(exact_only, u_make_gen_stream).


% annotating U::MAKE-GEN-STREAM 
f_u_make_gen_stream(Stream_Param, FnResult) :-
	cl_make_instance([u_gen_stream, kw_stream, Stream_Param],
			 Make_instance_Ret),
	Make_instance_Ret=FnResult.
:- set_opv(f_u_make_gen_stream, classof, claz_function),
   set_opv(u_make_gen_stream, compile_as, kw_function),
   set_opv(u_make_gen_stream, function, f_u_make_gen_stream),
   DefunResult=u_make_gen_stream.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84750 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmethod,
			    'gs-newline',
			    [[self, 'gen-stream']],
			    [newline, ['gen-stream-stream', self]],
			    [setf, ['hpos-value', self], 0]
			  ]).
:- get_var(Env, u_gs_newline, Gs_newline_Get),
   cl_eval([[u_self, u_gen_stream]], Eval_Ret),
   f_u_newline([u_gen_stream_stream, u_self], Newline_Ret),
   get_var(Env, u_self, Self_Get),
   set_place(Env, setf, [u_hpos_value, Self_Get], [0], Setf_R),
   cl_defmethod(Gs_newline_Get, Eval_Ret, [Newline_Ret, Setf_R], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84858 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmethod,
			    'gs-reset-line',
			    [[self, 'gen-stream']],
			    [setf, ['hpos-value', self], 0]
			  ]).
:- get_var(Env, u_gs_reset_line, Gs_reset_line_Get),
   cl_eval([[u_self, u_gen_stream]], Eval_Ret),
   get_var(Env, u_self, Self_Get),
   set_place(Env, setf, [u_hpos_value, Self_Get], [0], Setf_R),
   cl_defmethod(Gs_reset_line_Get, Eval_Ret, [Setf_R], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:84933 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmethod,
			    'gs-reset-sentence',
			    [[self, 'gen-stream']],
			    [setf, ['start-sentence?', self], t]
			  ]).
:- get_var(Env, u_gs_reset_sentence, Gs_reset_sentence_Get),
   cl_eval([[u_self, u_gen_stream]], Eval_Ret),
   get_var(Env, u_self, Self_Get),
   set_place(Env, setf, [u_start_sentence_c63, Self_Get], [t], Setf_R),
   cl_defmethod(Gs_reset_sentence_Get, Eval_Ret, [Setf_R], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:85017 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmethod,
			    'gs-string-write',
			    [[self, 'gen-stream'], text],
			    
			    [ if,
			      ['start-sentence?', self],
			      
			      [ progn,
				[setq, text, [concatenate, [quote, string], text]],
				
				[ if,
				  ['capitalize-first!', text],
				  [setf, ['start-sentence?', self], []]
				]
			      ]
			    ],
			    
			    [ if,
			      
			      [ (>),
				[+, ['hpos-value', self], ['string-length', text]],
				['line-length-value', self]
			      ],
			      ['gs-newline', self]
			    ],
			    ['string-write', ['gen-stream-stream', self], text],
			    
			    [ setf,
			      ['hpos-value', self],
			      [+, ['hpos-value', self], ['string-length', text]]
			    ]
			  ]).
:- get_var(Env, u_gs_string_write, Gs_string_write_Get),
   cl_eval([[u_self, u_gen_stream], u_text], Eval_Ret),
   get_var(Env, u_self, Self_Get),
   f_u_start_sentence_c63(Self_Get, IFTEST),
   (   IFTEST\==[]
   ->  get_var(Env, u_text, Text_Get12),
       cl_concatenate(string, Text_Get12, Text),
       set_var(Env, u_text, Text),
       f_u_capitalize_first_c33(Text_Get12, IFTEST10),
       (   IFTEST10\==[]
       ->  get_var(Env, u_self, Self_Get15),
	   set_place(Env, setf, [u_start_sentence_c63, Self_Get15], [[]], Setf_R),
	   TrueResult17=Setf_R
       ;   TrueResult17=[]
       ),
       CAR38=TrueResult17
   ;   CAR38=[]
   ),
   get_var(Env, u_self, Self_Get19),
   f_u_hpos_value(Self_Get19, Hpos_value_Ret),
   f_u_string_length(u_text, String_length_Ret),
   +(Hpos_value_Ret, String_length_Ret, PredArg1Result),
   get_var(Env, u_self, Self_Get20),
   f_u_line_length_value(Self_Get20, PredArg2Result),
   (   PredArg1Result>PredArg2Result
   ->  get_var(Env, u_self, Self_Get24),
       f_u_gs_newline(Self_Get24, TrueResult25),
       CAR37=TrueResult25
   ;   CAR37=[]
   ),
   f_u_string_write([u_gen_stream_stream, u_self], u_text, Text30),
   get_var(Env, u_self, Self_Get26),
   f_u_hpos_value(Self_Get26, Hpos_value_Ret34),
   f_u_string_length(u_text, String_length_Ret35),
   +(Hpos_value_Ret34, String_length_Ret35, CAR),
   get_var(Env, u_self, Self_Get28),
   set_place(Env, setf, [u_hpos_value, Self_Get28], [CAR], Setf_R27),
   cl_defmethod(Gs_string_write_Get,
		Eval_Ret,
		[CAR38, CAR37, Text30, Setf_R27],
		_Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:85472 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmethod,
			    'gs-end-sentence',
			    [[self, 'gen-stream']],
			    ['gs-string-write', self, '$STRING'(".")],
			    [setf, ['start-sentence?', self], t]
			  ]).
:- get_var(Env, u_gs_end_sentence, Gs_end_sentence_Get),
   cl_eval([[u_self, u_gen_stream]], Eval_Ret),
   get_var(Env, u_self, Self_Get),
   f_u_gs_string_write(Self_Get,
		       '$ARRAY'([*], claz_base_character, [#\('.')]),
		       String_write_Ret),
   get_var(Env, u_self, Self_Get9),
   set_place(Env, setf, [u_start_sentence_c63, Self_Get9], [t], Setf_R),
   cl_defmethod(Gs_end_sentence_Get,
		Eval_Ret,
		[String_write_Ret, Setf_R],
		_Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:85582 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmethod,
			    'gs-end-question',
			    [[self, 'gen-stream']],
			    ['gs-string-write', self, '$STRING'("?")],
			    [setf, ['start-sentence?', self], t]
			  ]).
:- get_var(Env, u_gs_end_question, Gs_end_question_Get),
   cl_eval([[u_self, u_gen_stream]], Eval_Ret),
   get_var(Env, u_self, Self_Get),
   f_u_gs_string_write(Self_Get,
		       '$ARRAY'([*], claz_base_character, [#\(?)]),
		       String_write_Ret),
   get_var(Env, u_self, Self_Get9),
   set_place(Env, setf, [u_start_sentence_c63, Self_Get9], [t], Setf_R),
   cl_defmethod(Gs_end_question_Get,
		Eval_Ret,
		[String_write_Ret, Setf_R],
		_Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:85692 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmethod,
			    'gs-end-exclam',
			    [[self, 'gen-stream']],
			    ['gs-string-write', self, '$STRING'("!")],
			    [setf, ['start-sentence?', self], t]
			  ]).
:- get_var(Env, u_gs_end_exclam, Gs_end_exclam_Get),
   cl_eval([[u_self, u_gen_stream]], Eval_Ret),
   get_var(Env, u_self, Self_Get),
   f_u_gs_string_write(Self_Get,
		       '$ARRAY'([*], claz_base_character, [#\(!)]),
		       String_write_Ret),
   get_var(Env, u_self, Self_Get9),
   set_place(Env, setf, [u_start_sentence_c63, Self_Get9], [t], Setf_R),
   cl_defmethod(Gs_end_exclam_Get, Eval_Ret, [String_write_Ret, Setf_R], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:85800 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'capitalize-first!',
			    [str],
			    
			    [ yloop,
			      
			      [ initial,
				[i, 0],
				[result, []],
				[len, ['string-length', str]]
			      ],
			      [yuntil, result],
			      [ywhile, [<, i, len]],
			      
			      [ ydo,
				
				[ if,
				  ['char/=', [char, str, i], #\(' ')],
				  
				  [ progn,
				    
				    [ setf,
				      [char, str, i],
				      ['char-upcase', [char, str, i]]
				    ],
				    [setq, result, t]
				  ],
				  ['increment-me', i]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::CAPITALIZE-FIRST! 
wl: lambda_def(defun,
	      u_capitalize_first_c33,
	      f_u_capitalize_first_c33,
	      [u_str],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_i, 0],
		    [u_result, []],
		    [u_len, [u_string_length, u_str]]
		  ],
		  [u_yuntil, u_result],
		  [u_ywhile, [<, u_i, u_len]],
		  
		  [ u_ydo,
		    
		    [ if,
		      [char_c47_c61, [char, u_str, u_i], #\(' ')],
		      
		      [ progn,
			[setf, [char, u_str, u_i], [char_upcase, [char, u_str, u_i]]],
			[setq, u_result, t]
		      ],
		      [u_increment_me, u_i]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::CAPITALIZE-FIRST! 
wl: arglist_info(u_capitalize_first_c33,
		[u_str],
		[Str_Param],
		arginfo{ all:[u_str],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_str],
			 opt:0,
			 req:[u_str],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CAPITALIZE-FIRST! 
wl: init_args(exact_only, u_capitalize_first_c33).


% annotating U::CAPITALIZE-FIRST! 
f_u_capitalize_first_c33(Str_Param, FnResult) :-
	Env=[bv(u_str, Str_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_i, 0],
		      [u_result, []],
		      [u_len, [u_string_length, u_str]]
		    ],
		    [u_yuntil, u_result],
		    [u_ywhile, [<, u_i, u_len]],
		    
		    [ u_ydo,
		      
		      [ if,
			[char_c47_c61, [char, u_str, u_i], #\(' ')],
			
			[ progn,
			  
			  [ setf,
			    [char, u_str, u_i],
			    [char_upcase, [char, u_str, u_i]]
			  ],
			  [setq, u_result, t]
			],
			[u_increment_me, u_i]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_capitalize_first_c33, classof, claz_function),
   set_opv(u_capitalize_first_c33, compile_as, kw_function),
   set_opv(u_capitalize_first_c33, function, f_u_capitalize_first_c33),
   DefunResult=u_capitalize_first_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_gen.cl:85800 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 86226)).
:- true.


% Total time: 30.117 seconds


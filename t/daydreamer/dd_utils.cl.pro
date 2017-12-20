
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_utils" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:13:36 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 7/19/86: Added rule chaining macros",
				     1,
				     208)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:245 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 9/24/86: Removed flavors", 1, 246)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:272 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 273)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:274 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     275)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:355 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    htlg,
			    [],
			    
			    [ 'ob$set',
			      [car, '*top-level-goals*'],
			      [quote, status],
			      [quote, halted]
			    ]
			  ]).

% annotating U::HTLG 
wl: lambda_def(defun,
	      u_htlg,
	      f_u_htlg,
	      [],
	      
	      [ 
		[ u_ob_c36_set,
		  [car, u_xx_top_level_goals_xx],
		  [quote, u_status],
		  [quote, u_halted]
		]
	      ]).


% annotating U::HTLG 
wl: arglist_info(u_htlg,
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

% annotating U::HTLG 
wl: init_args(exact_only, u_htlg).


% annotating U::HTLG 
f_u_htlg(FnResult) :-
	Env=[],
	get_var(Env, u_xx_top_level_goals_xx, Xx_top_level_goals_xx_Get),
	cl_car(Xx_top_level_goals_xx_Get, C36_set_Param),
	f_u_ob_c36_set(C36_set_Param, u_status, u_halted, Halted),
	Halted=FnResult.
:- set_opv(f_u_htlg, classof, claz_function),
   set_opv(u_htlg, compile_as, kw_function),
   set_opv(u_htlg, function, f_u_htlg),
   DefunResult=u_htlg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:424 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    hlt,
			    [gl],
			    ['ob$set', gl, [quote, status], [quote, halted]]
			  ]).

% annotating U::HLT 
wl: lambda_def(defun,
	      u_hlt,
	      f_u_hlt,
	      [u_gl],
	      [[u_ob_c36_set, u_gl, [quote, u_status], [quote, u_halted]]]).


% annotating U::HLT 
wl: arglist_info(u_hlt,
		[u_gl],
		[Gl_Param],
		arginfo{ all:[u_gl],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_gl],
			 opt:0,
			 req:[u_gl],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::HLT 
wl: init_args(exact_only, u_hlt).


% annotating U::HLT 
f_u_hlt(Gl_Param, FnResult) :-
	f_u_ob_c36_set(Gl_Param, u_status, u_halted, Halted),
	Halted=FnResult.
:- set_opv(f_u_hlt, classof, claz_function),
   set_opv(u_hlt, compile_as, kw_function),
   set_opv(u_hlt, function, f_u_hlt),
   DefunResult=u_hlt.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:424 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 473)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:424 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This isn't right for non-fully-loaded obs",
				     1,
				     475)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:424 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 519)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:520 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*default-strength*', 1.0]).
:- set_var(TLEnv3, setq, u_xx_default_strength_xx, 1.0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:520 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 552)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:520 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Inverse slot declarations", 1, 554)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:520 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 582)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:583 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ob$decl-inverses',
			    [quote, causes],
			    [quote, 'caused-by']
			  ]).
:- f_u_ob_c36_decl_inverses(u_causes, u_caused_by, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:621 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ob$decl-inverses',
			    [quote, 'seq-next'],
			    [quote, 'seq-next-of']
			  ]).
:- f_u_ob_c36_decl_inverses(u_seq_next, u_seq_next_of, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:663 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ob$decl-inverses',
			    [quote, 'g-situ'],
			    [quote, 'g-situ-of']
			  ]).
:- f_u_ob_c36_decl_inverses(u_g_situ, u_g_situ_of, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:663 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 703)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:663 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Type definitions", 1, 705)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:663 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 724)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:725 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ty$create',
			    [quote, 'ORDERING'],
			    [],
			    [quote, [prop, [strength], []]]
			  ]).
:- f_u_ty_c36_create(u_ordering, [], [u_prop, [u_strength], []], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:773 **********************/
:- lisp_compile_to_prolog(pkg_user, ['ty$create', [quote, 'ALTERN'], [], []]).
:- f_u_ty_c36_create(u_altern, [], [], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:801 **********************/
:- lisp_compile_to_prolog(pkg_user, ['ty$create', [quote, 'MUTATION'], [], []]).
:- f_u_ty_c36_create(u_mutation, [], [], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:831 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ty$create',
			    [quote, 'OTHER'],
			    [],
			    [quote, [prop, [actor], []]]
			  ]).
:- f_u_ty_c36_create(u_other, [], [u_prop, [u_actor], []], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:874 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$print-sprout-trace',
			    [self],
			    
			    [ if,
			      ['ob$get', self, [quote, 'sprout-trace']],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Broadcasting delayed debugs.")
				],
				
				[ format,
				  '*gate-dbg*',
				  '$STRING'("~A"),
				  [car, ['ob$get', self, [quote, 'sprout-trace']]]
				],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("End of delayed broadcast.")
				]
			      ]
			    ]
			  ]).

% annotating U::CX$PRINT-SPROUT-TRACE 
wl: lambda_def(defun,
	      u_cx_c36_print_sprout_trace,
	      f_u_cx_c36_print_sprout_trace,
	      [u_self],
	      
	      [ 
		[ if,
		  [u_ob_c36_get, u_self, [quote, u_sprout_trace]],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('B'),
				 #\(r),
				 #\(o),
				 #\(a),
				 #\(d),
				 #\(c),
				 #\(a),
				 #\(s),
				 #\(t),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\(' '),
				 #\(d),
				 #\(e),
				 #\(l),
				 #\(a),
				 #\(y),
				 #\(e),
				 #\(d),
				 #\(' '),
				 #\(d),
				 #\(e),
				 #\(b),
				 #\(u),
				 #\(g),
				 #\(s),
				 #\('.')
			       ])
		    ],
		    
		    [ format,
		      u_xx_gate_dbg_xx,
		      '$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
		      [car, [u_ob_c36_get, u_self, [quote, u_sprout_trace]]]
		    ],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('E'),
				 #\(n),
				 #\(d),
				 #\(' '),
				 #\(o),
				 #\(f),
				 #\(' '),
				 #\(d),
				 #\(e),
				 #\(l),
				 #\(a),
				 #\(y),
				 #\(e),
				 #\(d),
				 #\(' '),
				 #\(b),
				 #\(r),
				 #\(o),
				 #\(a),
				 #\(d),
				 #\(c),
				 #\(a),
				 #\(s),
				 #\(t),
				 #\('.')
			       ])
		    ]
		  ]
		]
	      ]).


% annotating U::CX$PRINT-SPROUT-TRACE 
wl: arglist_info(u_cx_c36_print_sprout_trace,
		[u_self],
		[Self_Param],
		arginfo{ all:[u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self],
			 opt:0,
			 req:[u_self],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$PRINT-SPROUT-TRACE 
wl: init_args(exact_only, u_cx_c36_print_sprout_trace).


% annotating U::CX$PRINT-SPROUT-TRACE 
f_u_cx_c36_print_sprout_trace(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_ob_c36_get(Self_Param, u_sprout_trace, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('B'),
					   #\(r),
					   #\(o),
					   #\(a),
					   #\(d),
					   #\(c),
					   #\(a),
					   #\(s),
					   #\(t),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\(' '),
					   #\(d),
					   #\(e),
					   #\(l),
					   #\(a),
					   #\(y),
					   #\(e),
					   #\(d),
					   #\(' '),
					   #\(d),
					   #\(e),
					   #\(b),
					   #\(u),
					   #\(g),
					   #\(s),
					   #\('.')
					 ])
			      ],
			      Roman_nl_Ret),
	    get_var(Env, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
	    f_u_ob_c36_get(Self_Param, u_sprout_trace, Sprout_trace),
	    cl_car(Sprout_trace, Car_Ret),
	    cl_format(
		      [ Xx_gate_dbg_xx_Get,
			'$ARRAY'([*], claz_base_character, [#\(~), #\('A')]),
			Car_Ret
		      ],
		      Format_Ret),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('E'),
					   #\(n),
					   #\(d),
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(' '),
					   #\(d),
					   #\(e),
					   #\(l),
					   #\(a),
					   #\(y),
					   #\(e),
					   #\(d),
					   #\(' '),
					   #\(b),
					   #\(r),
					   #\(o),
					   #\(a),
					   #\(d),
					   #\(c),
					   #\(a),
					   #\(s),
					   #\(t),
					   #\('.')
					 ])
			      ],
			      TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_cx_c36_print_sprout_trace, classof, claz_function),
   set_opv(u_cx_c36_print_sprout_trace, compile_as, kw_function),
   set_opv(u_cx_c36_print_sprout_trace, function, f_u_cx_c36_print_sprout_trace),
   DefunResult=u_cx_c36_print_sprout_trace.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:1164 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*global-switches*', []]).
:- set_var(TLEnv3, setq, u_xx_global_switches_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:1164 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1195)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:1164 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Utility ob link functions", 1, 1197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:1164 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1225)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:1227 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'has-link-relative?',
			    [ob, direction, type, context, 'belief-path'],
			    
			    [ let,
			      [[links, ['ob$gets', ob, direction]]],
			      
			      [ 'any?',
				
				[ lambda,
				  [x],
				  
				  [ and,
				    
				    [ 'cx$true-relative',
				      context,
				      x,
				      'belief-path'
				    ],
				    ['ty$instance-of?', x, type]
				  ]
				],
				links
			      ]
			    ]
			  ]).

% annotating U::HAS-LINK-RELATIVE? 
wl: lambda_def(defun,
	      u_has_link_relative_c63,
	      f_u_has_link_relative_c63,
	      [u_ob, u_direction, type, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  [[u_links, [u_ob_c36_gets, u_ob, u_direction]]],
		  
		  [ u_any_c63,
		    
		    [ lambda,
		      [u_x],
		      
		      [ and,
			[u_cx_c36_true_relative, u_context, u_x, u_belief_path],
			[u_ty_c36_instance_of_c63, u_x, type]
		      ]
		    ],
		    u_links
		  ]
		]
	      ]).


% annotating U::HAS-LINK-RELATIVE? 
wl: arglist_info(u_has_link_relative_c63,
		[u_ob, u_direction, type, u_context, u_belief_path],
		
		[ Ob_Param,
		  Direction_Param,
		  Type_Param,
		  Context_Param,
		  Belief_path_Param
		],
		arginfo{ all:[u_ob, u_direction, type, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_direction, type, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_direction, type, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::HAS-LINK-RELATIVE? 
wl: init_args(exact_only, u_has_link_relative_c63).


% annotating U::HAS-LINK-RELATIVE? 
f_u_has_link_relative_c63(Ob_Param, Direction_Param, Type_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_direction, Direction_Param), bv(type, Type_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ob_c36_gets(Ob_Param, Direction_Param, Links_Init),
	LEnv=[[bv(u_links, Links_Init)]|Env],
	f_u_any_c63(
		    [ lambda,
		      [u_x],
		      
		      [ and,
			[u_cx_c36_true_relative, u_context, u_x, u_belief_path],
			[u_ty_c36_instance_of_c63, u_x, type]
		      ]
		    ],
		    u_links,
		    Links),
	LetResult=Links,
	LetResult=FnResult.
:- set_opv(f_u_has_link_relative_c63, classof, claz_function),
   set_opv(u_has_link_relative_c63, compile_as, kw_function),
   set_opv(u_has_link_relative_c63, function, f_u_has_link_relative_c63),
   DefunResult=u_has_link_relative_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:1465 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-links-relative',
			    [ob, 'link-type', context, 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ links,
				  ['ob$gets', ob, [quote, 'linked-from-of']]
				]
			      ],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, link, in, links],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ 'cx$true-relative',
					context,
					link,
					'belief-path'
				      ],
				      ['ty$instance-of?', link, 'link-type']
				    ],
				    
				    [ setq,
				      result,
				      ['append!', result, [list, link]]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::GET-LINKS-RELATIVE 
wl: lambda_def(defun,
	      u_get_links_relative,
	      f_u_get_links_relative,
	      [u_ob, u_link_type, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_from_of]]]],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::GET-LINKS-RELATIVE 
wl: arglist_info(u_get_links_relative,
		[u_ob, u_link_type, u_context, u_belief_path],
		[Ob_Param, Link_type_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_link_type, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_link_type, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_link_type, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-LINKS-RELATIVE 
wl: init_args(exact_only, u_get_links_relative).


% annotating U::GET-LINKS-RELATIVE 
f_u_get_links_relative(Ob_Param, Link_type_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ob_c36_gets(Ob_Param, u_linked_from_of, Links_Init),
	LEnv=[[bv(u_links, Links_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_get_links_relative, classof, claz_function),
   set_opv(u_get_links_relative, compile_as, kw_function),
   set_opv(u_get_links_relative, function, f_u_get_links_relative),
   DefunResult=u_get_links_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:1856 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-links-from-relative',
			    [ob, 'link-type', context, 'belief-path'],
			    
			    [ let,
			      [[links, ['ob$gets', ob, [quote, 'linked-to-of']]]],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, link, in, links],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ 'cx$true-relative',
					context,
					link,
					'belief-path'
				      ],
				      ['ty$instance-of?', link, 'link-type']
				    ],
				    
				    [ setq,
				      result,
				      ['append!', result, [list, link]]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::GET-LINKS-FROM-RELATIVE 
wl: lambda_def(defun,
	      u_get_links_from_relative,
	      f_u_get_links_from_relative,
	      [u_ob, u_link_type, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  [[u_links, [u_ob_c36_gets, u_ob, [quote, u_linked_to_of]]]],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::GET-LINKS-FROM-RELATIVE 
wl: arglist_info(u_get_links_from_relative,
		[u_ob, u_link_type, u_context, u_belief_path],
		[Ob_Param, Link_type_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_link_type, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_link_type, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_link_type, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-LINKS-FROM-RELATIVE 
wl: init_args(exact_only, u_get_links_from_relative).


% annotating U::GET-LINKS-FROM-RELATIVE 
f_u_get_links_from_relative(Ob_Param, Link_type_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ob_c36_gets(Ob_Param, u_linked_to_of, Links_Init),
	LEnv=[[bv(u_links, Links_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ],
			  [u_ty_c36_instance_of_c63, u_link, u_link_type]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_link]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_get_links_from_relative, classof, claz_function),
   set_opv(u_get_links_from_relative, compile_as, kw_function),
   set_opv(u_get_links_from_relative, function, f_u_get_links_from_relative),
   DefunResult=u_get_links_from_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:2250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ol-get-relative',
			    [ob, 'link-type', dir, context, 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ links,
				  
				  [ 'ob$gets',
				    ob,
				    
				    [ if,
				      ['eq?', dir, [quote, backward]],
				      [quote, 'linked-to-of'],
				      [quote, 'linked-from-of']
				    ]
				  ]
				],
				
				[ 'other-dir',
				  
				  [ if,
				    ['eq?', dir, [quote, backward]],
				    [quote, 'linked-from'],
				    [quote, 'linked-to']
				  ]
				]
			      ],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, link, in, links],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      ['ty$instance-of?', link, 'link-type'],
				      
				      [ 'cx$true-relative',
					context,
					link,
					'belief-path'
				      ]
				    ],
				    
				    [ setq,
				      result,
				      
				      [ append,
					['ob$gets', link, 'other-dir'],
					result
				      ]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::OL-GET-RELATIVE 
wl: lambda_def(defun,
	      u_ol_get_relative,
	      f_u_ol_get_relative,
	      [u_ob, u_link_type, ext_dir, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_links,
		      
		      [ u_ob_c36_gets,
			u_ob,
			
			[ if,
			  [u_eq_c63, ext_dir, [quote, u_backward]],
			  [quote, u_linked_to_of],
			  [quote, u_linked_from_of]
			]
		      ]
		    ],
		    
		    [ u_other_dir,
		      
		      [ if,
			[u_eq_c63, ext_dir, [quote, u_backward]],
			[quote, u_linked_from],
			[quote, u_linked_to]
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ty_c36_instance_of_c63, u_link, u_link_type],
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			],
			
			[ setq,
			  u_result,
			  [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::OL-GET-RELATIVE 
wl: arglist_info(u_ol_get_relative,
		[u_ob, u_link_type, ext_dir, u_context, u_belief_path],
		
		[ Ob_Param,
		  Link_type_Param,
		  Ext_dir_Param,
		  Context_Param,
		  Belief_path_Param
		],
		arginfo{ all:
			     [ u_ob,
			       u_link_type,
			       ext_dir,
			       u_context,
			       u_belief_path
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob,
				 u_link_type,
				 ext_dir,
				 u_context,
				 u_belief_path
			       ],
			 opt:0,
			 req:
			     [ u_ob,
			       u_link_type,
			       ext_dir,
			       u_context,
			       u_belief_path
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OL-GET-RELATIVE 
wl: init_args(exact_only, u_ol_get_relative).


% annotating U::OL-GET-RELATIVE 
f_u_ol_get_relative(Ob_Param, Link_type_Param, Ext_dir_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(ext_dir, Ext_dir_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST),
	(   IFTEST\==[]
	->  _97050=u_linked_to_of
	;   _97050=u_linked_from_of
	),
	f_u_ob_c36_gets(Ob_Param, _97050, Links_Init),
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST25),
	(   IFTEST25\==[]
	->  Other_dir_Init=u_linked_from
	;   Other_dir_Init=u_linked_to
	),
	LEnv=[[bv(u_links, Links_Init), bv(u_other_dir, Other_dir_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ty_c36_instance_of_c63, u_link, u_link_type],
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			],
			
			[ setq,
			  u_result,
			  [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_ol_get_relative, classof, claz_function),
   set_opv(u_ol_get_relative, compile_as, kw_function),
   set_opv(u_ol_get_relative, function, f_u_ol_get_relative),
   DefunResult=u_ol_get_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:2880 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ol-get-relative-rule',
			    [ob, 'link-type', dir, context, 'belief-path', rule],
			    
			    [ let,
			      
			      [ 
				[ links,
				  
				  [ 'ob$gets',
				    ob,
				    
				    [ if,
				      ['eq?', dir, [quote, backward]],
				      [quote, 'linked-to-of'],
				      [quote, 'linked-from-of']
				    ]
				  ]
				],
				
				[ 'other-dir',
				  
				  [ if,
				    ['eq?', dir, [quote, backward]],
				    [quote, 'linked-from'],
				    [quote, 'linked-to']
				  ]
				]
			      ],
			      
			      [ yloop,
				[initial, [result, []]],
				[yfor, link, in, links],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      ['ty$instance-of?', link, 'link-type'],
				      
				      [ 'eq?',
					rule,
					['ob$get', link, [quote, rule]]
				      ],
				      
				      [ 'cx$true-relative',
					context,
					link,
					'belief-path'
				      ]
				    ],
				    
				    [ setq,
				      result,
				      
				      [ append,
					['ob$gets', link, 'other-dir'],
					result
				      ]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::OL-GET-RELATIVE-RULE 
wl: lambda_def(defun,
	      u_ol_get_relative_rule,
	      f_u_ol_get_relative_rule,
	      [u_ob, u_link_type, ext_dir, u_context, u_belief_path, u_rule],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_links,
		      
		      [ u_ob_c36_gets,
			u_ob,
			
			[ if,
			  [u_eq_c63, ext_dir, [quote, u_backward]],
			  [quote, u_linked_to_of],
			  [quote, u_linked_from_of]
			]
		      ]
		    ],
		    
		    [ u_other_dir,
		      
		      [ if,
			[u_eq_c63, ext_dir, [quote, u_backward]],
			[quote, u_linked_from],
			[quote, u_linked_to]
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ty_c36_instance_of_c63, u_link, u_link_type],
			  
			  [ u_eq_c63,
			    u_rule,
			    [u_ob_c36_get, u_link, [quote, u_rule]]
			  ],
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			],
			
			[ setq,
			  u_result,
			  [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::OL-GET-RELATIVE-RULE 
wl: arglist_info(u_ol_get_relative_rule,
		[u_ob, u_link_type, ext_dir, u_context, u_belief_path, u_rule],
		
		[ Ob_Param,
		  Link_type_Param,
		  Ext_dir_Param,
		  Context_Param,
		  Belief_path_Param,
		  Rule_Param
		],
		arginfo{ all:
			     [ u_ob,
			       u_link_type,
			       ext_dir,
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
				 u_link_type,
				 ext_dir,
				 u_context,
				 u_belief_path,
				 u_rule
			       ],
			 opt:0,
			 req:
			     [ u_ob,
			       u_link_type,
			       ext_dir,
			       u_context,
			       u_belief_path,
			       u_rule
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OL-GET-RELATIVE-RULE 
wl: init_args(exact_only, u_ol_get_relative_rule).


% annotating U::OL-GET-RELATIVE-RULE 
f_u_ol_get_relative_rule(Ob_Param, Link_type_Param, Ext_dir_Param, Context_Param, Belief_path_Param, Rule_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(ext_dir, Ext_dir_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param), bv(u_rule, Rule_Param)],
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST),
	(   IFTEST\==[]
	->  _97478=u_linked_to_of
	;   _97478=u_linked_from_of
	),
	f_u_ob_c36_gets(Ob_Param, _97478, Links_Init),
	f_u_eq_c63(ext_dir, [quote, u_backward], IFTEST27),
	(   IFTEST27\==[]
	->  Other_dir_Init=u_linked_from
	;   Other_dir_Init=u_linked_to
	),
	LEnv=[[bv(u_links, Links_Init), bv(u_other_dir, Other_dir_Init)]|Env],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_link, u_in, u_links],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ty_c36_instance_of_c63, u_link, u_link_type],
			  
			  [ u_eq_c63,
			    u_rule,
			    [u_ob_c36_get, u_link, [quote, u_rule]]
			  ],
			  
			  [ u_cx_c36_true_relative,
			    u_context,
			    u_link,
			    u_belief_path
			  ]
			],
			
			[ setq,
			  u_result,
			  [append, [u_ob_c36_gets, u_link, u_other_dir], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_ol_get_relative_rule, classof, claz_function),
   set_opv(u_ol_get_relative_rule, compile_as, kw_function),
   set_opv(u_ol_get_relative_rule, function, f_u_ol_get_relative_rule),
   DefunResult=u_ol_get_relative_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:3572 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ol-set-relative',
			    
			    [ 'from-ob',
			      'link-type',
			      'to-ob',
			      context,
			      'belief-path'
			    ],
			    
			    [ let,
			      
			      [ 
				[ link,
				  
				  [ 'ob$fcreate',
				    
				    [ '#BQ',
				      
				      [ 'NOTYPE',
					'linked-from',
					['#COMMA', 'from-ob'],
					'linked-to',
					['#COMMA', 'to-ob']
				      ]
				    ]
				  ]
				]
			      ],
			      ['ob$set', link, [quote, type], 'link-type'],
			      
			      [ 'cx$assert-relative',
				context,
				link,
				'belief-path'
			      ]
			    ]
			  ]).

% annotating U::OL-SET-RELATIVE 
wl: lambda_def(defun,
	      u_ol_set_relative,
	      f_u_ol_set_relative,
	      [u_from_ob, u_link_type, u_to_ob, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_link,
		      
		      [ u_ob_c36_fcreate,
			
			[ '#BQ',
			  
			  [ u_notype,
			    u_linked_from,
			    ['#COMMA', u_from_ob],
			    u_linked_to,
			    ['#COMMA', u_to_ob]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_ob_c36_set, u_link, [quote, type], u_link_type],
		  [u_cx_c36_assert_relative, u_context, u_link, u_belief_path]
		]
	      ]).


% annotating U::OL-SET-RELATIVE 
wl: arglist_info(u_ol_set_relative,
		[u_from_ob, u_link_type, u_to_ob, u_context, u_belief_path],
		
		[ From_ob_Param,
		  Link_type_Param,
		  To_ob_Param,
		  Context_Param,
		  Belief_path_Param
		],
		arginfo{ all:
			     [ u_from_ob,
			       u_link_type,
			       u_to_ob,
			       u_context,
			       u_belief_path
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_from_ob,
				 u_link_type,
				 u_to_ob,
				 u_context,
				 u_belief_path
			       ],
			 opt:0,
			 req:
			     [ u_from_ob,
			       u_link_type,
			       u_to_ob,
			       u_context,
			       u_belief_path
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OL-SET-RELATIVE 
wl: init_args(exact_only, u_ol_set_relative).


% annotating U::OL-SET-RELATIVE 
f_u_ol_set_relative(From_ob_Param, Link_type_Param, To_ob_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_from_ob, From_ob_Param), bv(u_link_type, Link_type_Param), bv(u_to_ob, To_ob_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ob_c36_fcreate(
			   [ '#BQ',
			     
			     [ u_notype,
			       u_linked_from,
			       ['#COMMA', u_from_ob],
			       u_linked_to,
			       ['#COMMA', u_to_ob]
			     ]
			   ],
			   Link_Init),
	LEnv=[[bv(u_link, Link_Init)]|Env],
	get_var(LEnv, u_link, Link_Get),
	f_u_ob_c36_set(Link_Get, type, Link_type_Param, C36_set_Ret),
	get_var(LEnv, u_link, Link_Get27),
	f_u_cx_c36_assert_relative(Context_Param,
				   Link_Get27,
				   Belief_path_Param,
				   Assert_relative_Ret),
	LetResult=Assert_relative_Ret,
	LetResult=FnResult.
:- set_opv(f_u_ol_set_relative, classof, claz_function),
   set_opv(u_ol_set_relative, compile_as, kw_function),
   set_opv(u_ol_set_relative, function, f_u_ol_set_relative),
   DefunResult=u_ol_set_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:3572 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" see replace-linked-ob", 1, 3812)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:3572 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" It is not clear to me why we have to not copy and then later restore",
				     1,
				     3836)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:3572 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" these slots (except for those that are permanent ignore). Can't",
				     1,
				     3907)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:3572 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" we just not omit them in the first place?",
				     1,
				     3973)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4016 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*preserve-link-slots*',
			    
			    [ quote,
			      
			      [ 'analogical-episode',
				'activation-context',
				'inference-rule',
				strength,
				'termination-context',
				'top-level-goal',
				'preservation-subgoal?'
			      ]
			    ]
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_preserve_link_slots_xx,
	   
	   [ u_analogical_episode,
	     u_activation_context,
	     u_inference_rule,
	     u_strength,
	     u_termination_context,
	     u_top_level_goal,
	     u_preservation_subgoal_c63
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4016 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" plan-rule inference-rule", 1, 4365)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4016 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" These are contained in obj slot anyway?",
				     1,
				     4392)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4016 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Also plan-subgoalnum", 1, 4434)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*link-slots*',
			    
			    [ append,
			      
			      [ quote,
				
				[ 'linked-from-of',
				  'linked-to-of',
				  'seq-next',
				  'seq-next-of',
				  'active-goal',
				  'top-context',
				  episode
				]
			      ],
			      '*preserve-link-slots*'
			    ]
			  ]).
:- get_var(TLEnv3, u_xx_preserve_link_slots_xx, Xx_preserve_link_slots_xx_Get),
   cl_append(
	     [ u_linked_from_of,
	       u_linked_to_of,
	       u_seq_next,
	       u_seq_next_of,
	       u_active_goal,
	       u_top_context,
	       u_episode
	     ],
	     Xx_preserve_link_slots_xx_Get,
	     _Ignored),
   set_var(TLEnv3, u_xx_link_slots_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun ob-copy-basic (ob)", 1, 4645)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (copy-ob-omit ob *link-slots*))",
				     1,
				     4672)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Weight, offset, decay: offset optional (default 0.0),",
				     1,
				     4708)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" weight optional (default = percentage of number",
				     1,
				     4764)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" of numeric dependencies).", 1, 4814)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4842)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; DEPENDENCY ROUTINES", 1, 4844)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4868)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" froms: ((from-ob from-ptn-ob) ...)",
				     1,
				     4870)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4907)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4457 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" is bd right?", 1, 4910)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4924 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-dependency',
			    
			    [ froms,
			      to,
			      rule,
			      context,
			      'belief-path',
			      plausibility,
			      bd
			    ],
			    
			    [ 'let*',
			      
			      [ [num, [length, froms]],
				
				[ weight,
				  ['fl/', plausibility, ['fixnum->flonum', num]]
				]
			      ],
			      
			      [ yloop,
				[yfor, from, in, froms],
				
				[ ydo,
				  
				  [ if,
				    ['ty$instance?', [car, from], [quote, rnot]],
				    
				    [ progn,
				      
				      [ setf,
					[car, from],
					['ob$instantiate', [cadr, from], bd]
				      ],
				      
				      [ 'ob$set',
					[car, from],
					[quote, type],
					'*not-ob*'
				      ],
				      
				      [ 'no-gen',
					
					[ 'cx$assert-relative',
					  context,
					  [car, from],
					  'belief-path'
					]
				      ]
				    ]
				  ],
				  
				  [ 'cx$assert-relative',
				    context,
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'DEPENDENCY',
					  'linked-from',
					  ['#COMMA', [car, from]],
					  'linked-to',
					  ['#COMMA', to],
					  weight,
					  
					  [ '#COMMA',
					    
					    [ 'with-default',
					      
					      [ if,
						['ob?', [cadr, from]],
						
						[ 'ob$get',
						  [cadr, from],
						  [quote, weight]
						],
						[]
					      ],
					      weight
					    ]
					  ],
					  offset,
					  
					  [ '#COMMA',
					    
					    [ 'with-default',
					      
					      [ if,
						['ob?', [cadr, from]],
						
						[ 'ob$get',
						  [cadr, from],
						  [quote, offset]
						],
						[]
					      ],
					      0.0
					    ]
					  ],
					  decay,
					  
					  [ '#COMMA',
					    
					    [ 'with-default',
					      
					      [ if,
						['ob?', [cadr, from]],
						
						[ 'ob$get',
						  [cadr, from],
						  [quote, decay]
						],
						[]
					      ],
					      0.0
					    ]
					  ],
					  rule,
					  ['#COMMA', rule]
					]
				      ]
				    ],
				    'belief-path'
				  ]
				]
			      ],
			      
			      [ 'recalculate-strength',
				to,
				context,
				'belief-path'
			      ]
			    ]
			  ]).

% annotating U::MAKE-DEPENDENCY 
wl: lambda_def(defun,
	      u_make_dependency,
	      f_u_make_dependency,
	      
	      [ u_froms,
		u_to,
		u_rule,
		u_context,
		u_belief_path,
		u_plausibility,
		u_bd
	      ],
	      
	      [ 
		[ let_xx,
		  
		  [ [u_num, [length, u_froms]],
		    
		    [ u_weight,
		      [u_fl_c47, u_plausibility, [u_fixnum_c62_flonum, u_num]]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_yfor, u_from, u_in, u_froms],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ty_c36_instance_c63, [car, u_from], [quote, u_rnot]],
			
			[ progn,
			  
			  [ setf,
			    [car, u_from],
			    [u_ob_c36_instantiate, [cadr, u_from], u_bd]
			  ],
			  
			  [ u_ob_c36_set,
			    [car, u_from],
			    [quote, type],
			    u_xx_not_ob_xx
			  ],
			  
			  [ u_no_gen,
			    
			    [ u_cx_c36_assert_relative,
			      u_context,
			      [car, u_from],
			      u_belief_path
			    ]
			  ]
			]
		      ],
		      
		      [ u_cx_c36_assert_relative,
			u_context,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_dependency,
			      u_linked_from,
			      ['#COMMA', [car, u_from]],
			      u_linked_to,
			      ['#COMMA', u_to],
			      u_weight,
			      
			      [ '#COMMA',
				
				[ u_with_default,
				  
				  [ if,
				    [u_ob_c63, [cadr, u_from]],
				    
				    [ u_ob_c36_get,
				      [cadr, u_from],
				      [quote, u_weight]
				    ],
				    []
				  ],
				  u_weight
				]
			      ],
			      u_offset,
			      
			      [ '#COMMA',
				
				[ u_with_default,
				  
				  [ if,
				    [u_ob_c63, [cadr, u_from]],
				    
				    [ u_ob_c36_get,
				      [cadr, u_from],
				      [quote, u_offset]
				    ],
				    []
				  ],
				  0.0
				]
			      ],
			      u_decay,
			      
			      [ '#COMMA',
				
				[ u_with_default,
				  
				  [ if,
				    [u_ob_c63, [cadr, u_from]],
				    
				    [ u_ob_c36_get,
				      [cadr, u_from],
				      [quote, u_decay]
				    ],
				    []
				  ],
				  0.0
				]
			      ],
			      u_rule,
			      ['#COMMA', u_rule]
			    ]
			  ]
			],
			u_belief_path
		      ]
		    ]
		  ],
		  [u_recalculate_strength, u_to, u_context, u_belief_path]
		]
	      ]).


% annotating U::MAKE-DEPENDENCY 
wl: arglist_info(u_make_dependency,
		
		[ u_froms,
		  u_to,
		  u_rule,
		  u_context,
		  u_belief_path,
		  u_plausibility,
		  u_bd
		],
		
		[ Froms_Param,
		  To_Param,
		  Rule_Param,
		  Context_Param,
		  Belief_path_Param,
		  Plausibility_Param,
		  Bd_Param
		],
		arginfo{ all:
			     [ u_froms,
			       u_to,
			       u_rule,
			       u_context,
			       u_belief_path,
			       u_plausibility,
			       u_bd
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_froms,
				 u_to,
				 u_rule,
				 u_context,
				 u_belief_path,
				 u_plausibility,
				 u_bd
			       ],
			 opt:0,
			 req:
			     [ u_froms,
			       u_to,
			       u_rule,
			       u_context,
			       u_belief_path,
			       u_plausibility,
			       u_bd
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-DEPENDENCY 
wl: init_args(exact_only, u_make_dependency).


% annotating U::MAKE-DEPENDENCY 
f_u_make_dependency(Froms_Param, To_Param, Rule_Param, Context_Param, Belief_path_Param, Plausibility_Param, Bd_Param, FnResult) :-
	Env=[bv(u_froms, Froms_Param), bv(u_to, To_Param), bv(u_rule, Rule_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param), bv(u_plausibility, Plausibility_Param), bv(u_bd, Bd_Param)],
	cl_length(Froms_Param, Num_Init),
	LEnv=[[bv(u_num, Num_Init)]|Env],
	f_u_fl_c47(u_plausibility, [u_fixnum_c62_flonum, u_num], Weight_Init),
	Env=[[bv(u_weight, Weight_Init)]|LEnv],
	f_u_yloop(
		  [ [u_yfor, u_from, u_in, u_froms],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_ty_c36_instance_c63, [car, u_from], [quote, u_rnot]],
			
			[ progn,
			  
			  [ setf,
			    [car, u_from],
			    [u_ob_c36_instantiate, [cadr, u_from], u_bd]
			  ],
			  
			  [ u_ob_c36_set,
			    [car, u_from],
			    [quote, type],
			    u_xx_not_ob_xx
			  ],
			  
			  [ u_no_gen,
			    
			    [ u_cx_c36_assert_relative,
			      u_context,
			      [car, u_from],
			      u_belief_path
			    ]
			  ]
			]
		      ],
		      
		      [ u_cx_c36_assert_relative,
			u_context,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_dependency,
			      u_linked_from,
			      ['#COMMA', [car, u_from]],
			      u_linked_to,
			      ['#COMMA', u_to],
			      u_weight,
			      
			      [ '#COMMA',
				
				[ u_with_default,
				  
				  [ if,
				    [u_ob_c63, [cadr, u_from]],
				    
				    [ u_ob_c36_get,
				      [cadr, u_from],
				      [quote, u_weight]
				    ],
				    []
				  ],
				  u_weight
				]
			      ],
			      u_offset,
			      
			      [ '#COMMA',
				
				[ u_with_default,
				  
				  [ if,
				    [u_ob_c63, [cadr, u_from]],
				    
				    [ u_ob_c36_get,
				      [cadr, u_from],
				      [quote, u_offset]
				    ],
				    []
				  ],
				  0.0
				]
			      ],
			      u_decay,
			      
			      [ '#COMMA',
				
				[ u_with_default,
				  
				  [ if,
				    [u_ob_c63, [cadr, u_from]],
				    
				    [ u_ob_c36_get,
				      [cadr, u_from],
				      [quote, u_decay]
				    ],
				    []
				  ],
				  0.0
				]
			      ],
			      u_rule,
			      ['#COMMA', u_rule]
			    ]
			  ]
			],
			u_belief_path
		      ]
		    ]
		  ],
		  Yloop_Ret),
	f_u_recalculate_strength(To_Param,
				 Context_Param,
				 Belief_path_Param,
				 Recalculate_strength_Ret),
	LetResult=Recalculate_strength_Ret,
	LetResult=FnResult.
:- set_opv(f_u_make_dependency, classof, claz_function),
   set_opv(u_make_dependency, compile_as, kw_function),
   set_opv(u_make_dependency, function, f_u_make_dependency),
   DefunResult=u_make_dependency.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4924 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" weight assumes either all or none of the ptns",
				     9,
				     5097)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4924 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" have all or none of the params", 9, 5153)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4924 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Old code", 1, 5233)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4924 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (if (and (ty$instance? (car from) 'not)",
				     1,
				     5244)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4924 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               (not (cx$true-relative context (car from) belief-path)))",
				     1,
				     5291)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:4924 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("          (cx$assert-relative context (car from) belief-path))",
				     1,
				     5364)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:6757 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'add-depend',
			    [context, from, to, weight, offset, decay, rule],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Add dependency from ~A to ~A in ~A"),
			      from,
			      to,
			      context
			    ],
			    
			    [ let,
			      
			      [ 
				[ dep,
				  
				  [ if,
				    rule,
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'DEPENDENCY',
					  'linked-from',
					  ['#COMMA', from],
					  'linked-to',
					  ['#COMMA', to],
					  weight,
					  ['#COMMA', weight],
					  offset,
					  ['#COMMA', offset],
					  decay,
					  ['#COMMA', decay],
					  rule,
					  ['#COMMA', rule]
					]
				      ]
				    ],
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'DEPENDENCY',
					  'linked-from',
					  ['#COMMA', from],
					  'linked-to',
					  ['#COMMA', to],
					  weight,
					  ['#COMMA', weight],
					  offset,
					  ['#COMMA', offset],
					  decay,
					  ['#COMMA', decay]
					]
				      ]
				    ]
				  ]
				]
			      ],
			      ['cx$assert', context, dep],
			      
			      [ 'recalculate-strength',
				to,
				context,
				['->belief-path', '*me-ob*']
			      ]
			    ]
			  ]).

% annotating U::ADD-DEPEND 
wl: lambda_def(defun,
	      u_add_depend,
	      f_u_add_depend,
	      [u_context, u_from, u_to, u_weight, u_offset, u_decay, u_rule],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('A'),
			     #\(d),
			     #\(d),
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
			     #\(y),
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
			     #\('A'),
			     #\(' '),
			     #\(i),
			     #\(n),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_from,
		  u_to,
		  u_context
		],
		
		[ let,
		  
		  [ 
		    [ u_dep,
		      
		      [ if,
			u_rule,
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_dependency,
			      u_linked_from,
			      ['#COMMA', u_from],
			      u_linked_to,
			      ['#COMMA', u_to],
			      u_weight,
			      ['#COMMA', u_weight],
			      u_offset,
			      ['#COMMA', u_offset],
			      u_decay,
			      ['#COMMA', u_decay],
			      u_rule,
			      ['#COMMA', u_rule]
			    ]
			  ]
			],
			
			[ u_ob_c36_fcreate,
			  
			  [ '#BQ',
			    
			    [ u_dependency,
			      u_linked_from,
			      ['#COMMA', u_from],
			      u_linked_to,
			      ['#COMMA', u_to],
			      u_weight,
			      ['#COMMA', u_weight],
			      u_offset,
			      ['#COMMA', u_offset],
			      u_decay,
			      ['#COMMA', u_decay]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_cx_c36_assert, u_context, u_dep],
		  
		  [ u_recalculate_strength,
		    u_to,
		    u_context,
		    [u_c62_belief_path, u_xx_me_ob_xx]
		  ]
		]
	      ]).


% annotating U::ADD-DEPEND 
wl: arglist_info(u_add_depend,
		[u_context, u_from, u_to, u_weight, u_offset, u_decay, u_rule],
		
		[ Context_Param,
		  From_Param,
		  To_Param,
		  Weight_Param,
		  Offset_Param,
		  Decay_Param,
		  Rule_Param
		],
		arginfo{ all:
			     [ u_context,
			       u_from,
			       u_to,
			       u_weight,
			       u_offset,
			       u_decay,
			       u_rule
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_context,
				 u_from,
				 u_to,
				 u_weight,
				 u_offset,
				 u_decay,
				 u_rule
			       ],
			 opt:0,
			 req:
			     [ u_context,
			       u_from,
			       u_to,
			       u_weight,
			       u_offset,
			       u_decay,
			       u_rule
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ADD-DEPEND 
wl: init_args(exact_only, u_add_depend).


% annotating U::ADD-DEPEND 
f_u_add_depend(Context_Param, From_Param, To_Param, Weight_Param, Offset_Param, Decay_Param, Rule_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_from, From_Param), bv(u_to, To_Param), bv(u_weight, Weight_Param), bv(u_offset, Offset_Param), bv(u_decay, Decay_Param), bv(u_rule, Rule_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('A'),
				       #\(d),
				       #\(d),
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
				       #\(y),
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
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_from,
			    u_to,
			    u_context
			  ],
			  Roman_nl_Ret),
	(   Rule_Param\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_dependency,
				   u_linked_from,
				   ['#COMMA', u_from],
				   u_linked_to,
				   ['#COMMA', u_to],
				   u_weight,
				   ['#COMMA', u_weight],
				   u_offset,
				   ['#COMMA', u_offset],
				   u_decay,
				   ['#COMMA', u_decay],
				   u_rule,
				   ['#COMMA', u_rule]
				 ]
			       ],
			       TrueResult),
	    Dep_Init=TrueResult
	;   f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_dependency,
				   u_linked_from,
				   ['#COMMA', u_from],
				   u_linked_to,
				   ['#COMMA', u_to],
				   u_weight,
				   ['#COMMA', u_weight],
				   u_offset,
				   ['#COMMA', u_offset],
				   u_decay,
				   ['#COMMA', u_decay]
				 ]
			       ],
			       ElseResult),
	    Dep_Init=ElseResult
	),
	LEnv=[[bv(u_dep, Dep_Init)]|Env],
	get_var(LEnv, u_dep, Dep_Get),
	f_u_cx_c36_assert(Context_Param, Dep_Get, C36_assert_Ret),
	get_var(LEnv, u_xx_me_ob_xx, Xx_me_ob_xx_Get),
	f_u_c62_belief_path(Xx_me_ob_xx_Get, Belief_path_Ret),
	f_u_recalculate_strength(To_Param,
				 Context_Param,
				 Belief_path_Ret,
				 Recalculate_strength_Ret),
	LetResult=Recalculate_strength_Ret,
	LetResult=FnResult.
:- set_opv(f_u_add_depend, classof, claz_function),
   set_opv(u_add_depend, compile_as, kw_function),
   set_opv(u_add_depend, function, f_u_add_depend),
   DefunResult=u_add_depend.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:6757 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Sets the strength of an ob, maintaining dependency consistency.",
				     1,
				     7493)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:7558 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'hyper-set-strength',
			    [ob, value, context],
			    
			    [ let,
			      [['old-strength', [strength, ob]]],
			      
			      [ 'modify-strength',
				context,
				ob,
				['fl-', value, 'old-strength'],
				1.0
			      ]
			    ]
			  ]).

% annotating U::HYPER-SET-STRENGTH 
wl: lambda_def(defun,
	      u_hyper_set_strength,
	      f_u_hyper_set_strength,
	      [u_ob, u_value, u_context],
	      
	      [ 
		[ let,
		  [[u_old_strength, [u_strength, u_ob]]],
		  
		  [ u_modify_strength,
		    u_context,
		    u_ob,
		    [u_flc45, u_value, u_old_strength],
		    1.0
		  ]
		]
	      ]).


% annotating U::HYPER-SET-STRENGTH 
wl: arglist_info(u_hyper_set_strength,
		[u_ob, u_value, u_context],
		[Ob_Param, Value_Param, Context_Param],
		arginfo{ all:[u_ob, u_value, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_value, u_context],
			 opt:0,
			 req:[u_ob, u_value, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::HYPER-SET-STRENGTH 
wl: init_args(exact_only, u_hyper_set_strength).


% annotating U::HYPER-SET-STRENGTH 
f_u_hyper_set_strength(Ob_Param, Value_Param, Context_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_value, Value_Param), bv(u_context, Context_Param)],
	f_u_strength(u_ob, Old_strength_Init),
	LEnv=[[bv(u_old_strength, Old_strength_Init)]|Env],
	f_u_flc45(u_value, u_old_strength, Old_strength),
	f_u_modify_strength(Context_Param,
			    Ob_Param,
			    Old_strength,
			    1.0,
			    Modify_strength_Ret),
	LetResult=Modify_strength_Ret,
	LetResult=FnResult.
:- set_opv(f_u_hyper_set_strength, classof, claz_function),
   set_opv(u_hyper_set_strength, compile_as, kw_function),
   set_opv(u_hyper_set_strength, function, f_u_hyper_set_strength),
   DefunResult=u_hyper_set_strength.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:7558 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Works on dangling or dependent objects.",
				     1,
				     7710)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:7751 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'modify-strength',
			    [context, ob, delt, weight],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      depend,
			      '$STRING'("Modify strength for ~A in ~A by ~A ~A"),
			      ob,
			      context,
			      delt,
			      weight
			    ],
			    
			    [ if,
			      
			      [ 'get-dependencies',
				ob,
				context,
				'*me-belief-path*'
			      ],
			      
			      [ let,
				
				[ 
				  [ delta,
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					['NOTYPE', strength, ['#COMMA', delt]]
				      ]
				    ]
				  ]
				],
				
				[ 'add-depend',
				  context,
				  delta,
				  ob,
				  weight,
				  0.0,
				  0.0,
				  []
				]
			      ],
			      
			      [ progn,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  depend,
				  '$STRING'("Dangling.")
				],
				
				[ 'set-strength',
				  ob,
				  ['fl+', ['fl*', weight, delt], [strength, ob]]
				],
				
				[ 'recalculate-strength',
				  ob,
				  context,
				  '*me-belief-path*'
				]
			      ]
			    ]
			  ]).

% annotating U::MODIFY-STRENGTH 
wl: lambda_def(defun,
	      u_modify_strength,
	      f_u_modify_strength,
	      [u_context, u_ob, u_delt, u_weight],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_depend,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('M'),
			     #\(o),
			     #\(d),
			     #\(i),
			     #\(f),
			     #\(y),
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
			     #\(y),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_ob,
		  u_context,
		  u_delt,
		  u_weight
		],
		
		[ if,
		  [u_get_dependencies, u_ob, u_context, u_xx_me_belief_path_xx],
		  
		  [ let,
		    
		    [ 
		      [ u_delta,
			
			[ u_ob_c36_fcreate,
			  ['#BQ', [u_notype, u_strength, ['#COMMA', u_delt]]]
			]
		      ]
		    ],
		    [u_add_depend, u_context, u_delta, u_ob, u_weight, 0.0, 0.0, []]
		  ],
		  
		  [ progn,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_depend,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('D'),
				 #\(a),
				 #\(n),
				 #\(g),
				 #\(l),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\('.')
			       ])
		    ],
		    
		    [ u_set_strength,
		      u_ob,
		      [u_fl_c43, [u_fl_xx, u_weight, u_delt], [u_strength, u_ob]]
		    ],
		    
		    [ u_recalculate_strength,
		      u_ob,
		      u_context,
		      u_xx_me_belief_path_xx
		    ]
		  ]
		]
	      ]).


% annotating U::MODIFY-STRENGTH 
wl: arglist_info(u_modify_strength,
		[u_context, u_ob, u_delt, u_weight],
		[Context_Param, Ob_Param, Delt_Param, Weight_Param],
		arginfo{ all:[u_context, u_ob, u_delt, u_weight],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_ob, u_delt, u_weight],
			 opt:0,
			 req:[u_context, u_ob, u_delt, u_weight],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MODIFY-STRENGTH 
wl: init_args(exact_only, u_modify_strength).


% annotating U::MODIFY-STRENGTH 
f_u_modify_strength(Context_Param, Ob_Param, Delt_Param, Weight_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_ob, Ob_Param), bv(u_delt, Delt_Param), bv(u_weight, Weight_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_depend,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('M'),
				       #\(o),
				       #\(d),
				       #\(i),
				       #\(f),
				       #\(y),
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
				       #\(y),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ob,
			    u_context,
			    u_delt,
			    u_weight
			  ],
			  Roman_nl_Ret),
	get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get),
	f_u_get_dependencies(Ob_Param,
			     Context_Param,
			     Xx_me_belief_path_xx_Get,
			     IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_fcreate(['#BQ', [u_notype, u_strength, ['#COMMA', u_delt]]],
			       Delta_Init),
	    LEnv=[[bv(u_delta, Delta_Init)]|Env],
	    get_var(LEnv, u_delta, Delta_Get),
	    f_u_add_depend(Context_Param,
			   Delta_Get,
			   Ob_Param,
			   Weight_Param,
			   0.0,
			   0.0,
			   [],
			   Add_depend_Ret),
	    FnResult=Add_depend_Ret
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_depend,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('D'),
					   #\(a),
					   #\(n),
					   #\(g),
					   #\(l),
					   #\(i),
					   #\(n),
					   #\(g),
					   #\('.')
					 ])
			      ],
			      Roman_nl_Ret40),
	    f_u_set_strength(u_ob,
			     
			     [ u_fl_c43,
			       [u_fl_xx, u_weight, u_delt],
			       [u_strength, u_ob]
			     ],
			     Set_strength_Ret),
	    get_var(Env, u_xx_me_belief_path_xx, Xx_me_belief_path_xx_Get33),
	    f_u_recalculate_strength(Ob_Param,
				     Context_Param,
				     Xx_me_belief_path_xx_Get33,
				     ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_modify_strength, classof, claz_function),
   set_opv(u_modify_strength, compile_as, kw_function),
   set_opv(u_modify_strength, function, f_u_modify_strength),
   DefunResult=u_modify_strength.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:7751 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: the below could probably be made faster by coding it",
				     1,
				     8280)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:7751 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" analogously to ol-get-relative. Depends on whether typical number of",
				     1,
				     8341)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:7751 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" dependencies in a context is greater than typical number of",
				     1,
				     8412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:7751 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 'linked-to-of connections.", 1, 8474)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:8502 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-dependencies',
			    [ob, context, 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ ptn,
				  
				  [ 'ob$fcreate',
				    
				    [ '#BQ',
				      ['DEPENDENCY', 'linked-to', ['#COMMA', ob]]
				    ]
				  ]
				]
			      ],
			      
			      [ 'retrieve-bd->ob',
				
				[ 'cx$retrieve-relative',
				  context,
				  ptn,
				  'belief-path'
				]
			      ]
			    ]
			  ]).

% annotating U::GET-DEPENDENCIES 
wl: lambda_def(defun,
	      u_get_dependencies,
	      f_u_get_dependencies,
	      [u_ob, u_context, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_ptn,
		      
		      [ u_ob_c36_fcreate,
			['#BQ', [u_dependency, u_linked_to, ['#COMMA', u_ob]]]
		      ]
		    ]
		  ],
		  
		  [ u_retrieve_bd_c62_ob,
		    [u_cx_c36_retrieve_relative, u_context, u_ptn, u_belief_path]
		  ]
		]
	      ]).


% annotating U::GET-DEPENDENCIES 
wl: arglist_info(u_get_dependencies,
		[u_ob, u_context, u_belief_path],
		[Ob_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-DEPENDENCIES 
wl: init_args(exact_only, u_get_dependencies).


% annotating U::GET-DEPENDENCIES 
f_u_get_dependencies(Ob_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ob_c36_fcreate(['#BQ', [u_dependency, u_linked_to, ['#COMMA', u_ob]]],
			   Ptn_Init),
	LEnv=[[bv(u_ptn, Ptn_Init)]|Env],
	f_u_retrieve_bd_c62_ob(
			       [ u_cx_c36_retrieve_relative,
				 u_context,
				 u_ptn,
				 u_belief_path
			       ],
			       C62_ob_Ret),
	LetResult=C62_ob_Ret,
	LetResult=FnResult.
:- set_opv(f_u_get_dependencies, classof, claz_function),
   set_opv(u_get_dependencies, compile_as, kw_function),
   set_opv(u_get_dependencies, function, f_u_get_dependencies),
   DefunResult=u_get_dependencies.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:8678 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-dependees',
			    [ob, context, 'belief-path'],
			    
			    [ 'ol-get-relative',
			      ob,
			      '*dependency-ob*',
			      [quote, forward],
			      context,
			      'belief-path'
			    ]
			  ]).

% annotating U::GET-DEPENDEES 
wl: lambda_def(defun,
	      u_get_dependees,
	      f_u_get_dependees,
	      [u_ob, u_context, u_belief_path],
	      
	      [ 
		[ u_ol_get_relative,
		  u_ob,
		  u_xx_dependency_ob_xx,
		  [quote, u_forward],
		  u_context,
		  u_belief_path
		]
	      ]).


% annotating U::GET-DEPENDEES 
wl: arglist_info(u_get_dependees,
		[u_ob, u_context, u_belief_path],
		[Ob_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-DEPENDEES 
wl: init_args(exact_only, u_get_dependees).


% annotating U::GET-DEPENDEES 
f_u_get_dependees(Ob_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	f_u_ol_get_relative(Ob_Param,
			    Xx_dependency_ob_xx_Get,
			    u_forward,
			    Context_Param,
			    Belief_path_Param,
			    Get_relative_Ret),
	Get_relative_Ret=FnResult.
:- set_opv(f_u_get_dependees, classof, claz_function),
   set_opv(u_get_dependees, compile_as, kw_function),
   set_opv(u_get_dependees, function, f_u_get_dependees),
   DefunResult=u_get_dependees.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:8794 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'divert-strength',
			    ['from-ob', 'to-ob', factor],
			    
			    [ let,
			      
			      [ 
				[ diversion,
				  ['fl*', factor, [strength, 'from-ob']]
				]
			      ],
			      
			      [ 'set-strength',
				'from-ob',
				['fl-', [strength, 'from-ob'], diversion]
			      ],
			      
			      [ 'set-strength',
				'to-ob',
				['fl+', [strength, 'to-ob'], diversion]
			      ],
			      diversion
			    ]
			  ]).

% annotating U::DIVERT-STRENGTH 
wl: lambda_def(defun,
	      u_divert_strength,
	      f_u_divert_strength,
	      [u_from_ob, u_to_ob, u_factor],
	      
	      [ 
		[ let,
		  [[u_diversion, [u_fl_xx, u_factor, [u_strength, u_from_ob]]]],
		  
		  [ u_set_strength,
		    u_from_ob,
		    [u_flc45, [u_strength, u_from_ob], u_diversion]
		  ],
		  
		  [ u_set_strength,
		    u_to_ob,
		    [u_fl_c43, [u_strength, u_to_ob], u_diversion]
		  ],
		  u_diversion
		]
	      ]).


% annotating U::DIVERT-STRENGTH 
wl: arglist_info(u_divert_strength,
		[u_from_ob, u_to_ob, u_factor],
		[From_ob_Param, To_ob_Param, Factor_Param],
		arginfo{ all:[u_from_ob, u_to_ob, u_factor],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_from_ob, u_to_ob, u_factor],
			 opt:0,
			 req:[u_from_ob, u_to_ob, u_factor],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DIVERT-STRENGTH 
wl: init_args(exact_only, u_divert_strength).


% annotating U::DIVERT-STRENGTH 
f_u_divert_strength(From_ob_Param, To_ob_Param, Factor_Param, FnResult) :-
	Env=[bv(u_from_ob, From_ob_Param), bv(u_to_ob, To_ob_Param), bv(u_factor, Factor_Param)],
	f_u_fl_xx(u_factor, [u_strength, u_from_ob], Diversion_Init),
	LEnv=[[bv(u_diversion, Diversion_Init)]|Env],
	f_u_set_strength(u_from_ob,
			 [u_flc45, [u_strength, u_from_ob], u_diversion],
			 Set_strength_Ret),
	f_u_set_strength(u_to_ob,
			 [u_fl_c43, [u_strength, u_to_ob], u_diversion],
			 Set_strength_Ret24),
	get_var(LEnv, u_diversion, Diversion_Get),
	LetResult=Diversion_Get,
	LetResult=FnResult.
:- set_opv(f_u_divert_strength, classof, claz_function),
   set_opv(u_divert_strength, compile_as, kw_function),
   set_opv(u_divert_strength, function, f_u_divert_strength),
   DefunResult=u_divert_strength.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:9034 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'recalculate-strength',
			    [ob, context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      depend,
			      '$STRING'("Recalculate strength for ~A in ~A"),
			      ob,
			      context
			    ],
			    
			    [ let,
			      
			      [ 
				[ deps,
				  
				  [ 'get-dependencies',
				    ob,
				    context,
				    'belief-path'
				  ]
				]
			      ],
			      
			      [ if,
				deps,
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    depend,
				    '$STRING'("Initial strength = ~A"),
				    [strength, ob]
				  ],
				  
				  [ yloop,
				    
				    [ initial,
				      ['new-strength', 0.0],
				      [offset, []],
				      [weight, []]
				    ],
				    [yfor, dependency, in, deps],
				    
				    [ ydo,
				      
				      [ setq,
					offset,
					['ob$get', dependency, [quote, offset]]
				      ],
				      
				      [ setq,
					weight,
					['ob$get', dependency, [quote, weight]]
				      ],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					depend,
					'$STRING'("Dependency ~A with offset ~A weight ~A"),
					dependency,
					offset,
					weight
				      ],
				      
				      [ if,
					[or, ['null?', offset], ['null?', weight]],
					
					[ progn,
					  
					  [ error,
					    '$STRING'("weight = ~A offset = ~A ob = ~A for ~A!!!~%(ret) to proceed"),
					    weight,
					    offset,
					    ob,
					    dependency
					  ],
					  [setq, offset, 0.0],
					  [setq, weight, 1.0]
					]
				      ],
				      
				      [ setq,
					'new-strength',
					
					[ 'fl+',
					  ['fl+', 'new-strength', offset],
					  
					  [ 'fl*',
					    
					    [ strength,
					      
					      [ 'ob$get',
						dependency,
						[quote, 'linked-from']
					      ]
					    ],
					    weight
					  ]
					]
				      ],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					depend,
					'$STRING'("Sum so far is ~A"),
					'new-strength'
				      ]
				    ],
				    
				    [ yresult,
				      ['set-strength', ob, 'new-strength'],
				      
				      [ 'ndbg-roman-nl',
					'*gate-dbg*',
					depend,
					'$STRING'("New strength = ~A"),
					'new-strength'
				      ],
				      
				      [ if,
					['ty$instance?', ob, [quote, emotion]],
					['normalize-emotion', ob, context]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    
			    [ yloop,
			      
			      [ yfor,
				dependee,
				in,
				['get-dependees', ob, context, 'belief-path']
			      ],
			      
			      [ ydo,
				
				[ 'recalculate-strength',
				  dependee,
				  context,
				  'belief-path'
				]
			      ]
			    ]
			  ]).

% annotating U::RECALCULATE-STRENGTH 
wl: lambda_def(defun,
	      u_recalculate_strength,
	      f_u_recalculate_strength,
	      [u_ob, u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_depend,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('R'),
			     #\(e),
			     #\(c),
			     #\(a),
			     #\(l),
			     #\(c),
			     #\(u),
			     #\(l),
			     #\(a),
			     #\(t),
			     #\(e),
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
		  [[u_deps, [u_get_dependencies, u_ob, u_context, u_belief_path]]],
		  
		  [ if,
		    u_deps,
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_depend,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('I'),
				   #\(n),
				   #\(i),
				   #\(t),
				   #\(i),
				   #\(a),
				   #\(l),
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
				   #\(=),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			[u_strength, u_ob]
		      ],
		      
		      [ u_yloop,
			
			[ u_initial,
			  [u_new_strength, 0.0],
			  [u_offset, []],
			  [u_weight, []]
			],
			[u_yfor, u_dependency, u_in, u_deps],
			
			[ u_ydo,
			  
			  [ setq,
			    u_offset,
			    [u_ob_c36_get, u_dependency, [quote, u_offset]]
			  ],
			  
			  [ setq,
			    u_weight,
			    [u_ob_c36_get, u_dependency, [quote, u_weight]]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_depend,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('D'),
				       #\(e),
				       #\(p),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(y),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(f),
				       #\(s),
				       #\(e),
				       #\(t),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(w),
				       #\(e),
				       #\(i),
				       #\(g),
				       #\(h),
				       #\(t),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_dependency,
			    u_offset,
			    u_weight
			  ],
			  
			  [ if,
			    [or, [u_null_c63, u_offset], [u_null_c63, u_weight]],
			    
			    [ progn,
			      
			      [ error,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(w),
					   #\(e),
					   #\(i),
					   #\(g),
					   #\(h),
					   #\(t),
					   #\(' '),
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(f),
					   #\(s),
					   #\(e),
					   #\(t),
					   #\(' '),
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(o),
					   #\(b),
					   #\(' '),
					   #\(=),
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
					   #\(!),
					   #\(!),
					   #\(!),
					   #\(~),
					   #\('%'),
					   #\('('),
					   #\(r),
					   #\(e),
					   #\(t),
					   #\(')'),
					   #\(' '),
					   #\(t),
					   #\(o),
					   #\(' '),
					   #\(p),
					   #\(r),
					   #\(o),
					   #\(c),
					   #\(e),
					   #\(e),
					   #\(d)
					 ]),
				u_weight,
				u_offset,
				u_ob,
				u_dependency
			      ],
			      [setq, u_offset, 0.0],
			      [setq, u_weight, 1.0]
			    ]
			  ],
			  
			  [ setq,
			    u_new_strength,
			    
			    [ u_fl_c43,
			      [u_fl_c43, u_new_strength, u_offset],
			      
			      [ u_fl_xx,
				
				[ u_strength,
				  
				  [ u_ob_c36_get,
				    u_dependency,
				    [quote, u_linked_from]
				  ]
				],
				u_weight
			      ]
			    ]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_depend,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(u),
				       #\(m),
				       #\(' '),
				       #\(s),
				       #\(o),
				       #\(' '),
				       #\(f),
				       #\(a),
				       #\(r),
				       #\(' '),
				       #\(i),
				       #\(s),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_new_strength
			  ]
			],
			
			[ u_yresult,
			  [u_set_strength, u_ob, u_new_strength],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_depend,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('N'),
				       #\(e),
				       #\(w),
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
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_new_strength
			  ],
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_ob, [quote, u_emotion]],
			    [u_normalize_emotion, u_ob, u_context]
			  ]
			]
		      ]
		    ]
		  ]
		],
		
		[ u_yloop,
		  
		  [ u_yfor,
		    u_dependee,
		    u_in,
		    [u_get_dependees, u_ob, u_context, u_belief_path]
		  ],
		  
		  [ u_ydo,
		    
		    [ u_recalculate_strength,
		      u_dependee,
		      u_context,
		      u_belief_path
		    ]
		  ]
		]
	      ]).


% annotating U::RECALCULATE-STRENGTH 
wl: arglist_info(u_recalculate_strength,
		[u_ob, u_context, u_belief_path],
		[Ob_Param, Context_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context, u_belief_path],
			 opt:0,
			 req:[u_ob, u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RECALCULATE-STRENGTH 
wl: init_args(exact_only, u_recalculate_strength).


% annotating U::RECALCULATE-STRENGTH 
f_u_recalculate_strength(Ob_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_depend,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('R'),
				       #\(e),
				       #\(c),
				       #\(a),
				       #\(l),
				       #\(c),
				       #\(u),
				       #\(l),
				       #\(a),
				       #\(t),
				       #\(e),
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
	f_u_get_dependencies(Ob_Param,
			     Context_Param,
			     Belief_path_Param,
			     Deps_Init),
	LEnv=[[bv(u_deps, Deps_Init)]|Env],
	get_var(LEnv, u_deps, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_depend,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(n),
					   #\(i),
					   #\(t),
					   #\(i),
					   #\(a),
					   #\(l),
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
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A')
					 ]),
				[u_strength, u_ob]
			      ],
			      Roman_nl_Ret30),
	    f_u_yloop(
		      [ 
			[ u_initial,
			  [u_new_strength, 0.0],
			  [u_offset, []],
			  [u_weight, []]
			],
			[u_yfor, u_dependency, u_in, u_deps],
			
			[ u_ydo,
			  
			  [ setq,
			    u_offset,
			    [u_ob_c36_get, u_dependency, [quote, u_offset]]
			  ],
			  
			  [ setq,
			    u_weight,
			    [u_ob_c36_get, u_dependency, [quote, u_weight]]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_depend,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('D'),
				       #\(e),
				       #\(p),
				       #\(e),
				       #\(n),
				       #\(d),
				       #\(e),
				       #\(n),
				       #\(c),
				       #\(y),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(w),
				       #\(i),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(f),
				       #\(s),
				       #\(e),
				       #\(t),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(w),
				       #\(e),
				       #\(i),
				       #\(g),
				       #\(h),
				       #\(t),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_dependency,
			    u_offset,
			    u_weight
			  ],
			  
			  [ if,
			    [or, [u_null_c63, u_offset], [u_null_c63, u_weight]],
			    
			    [ progn,
			      
			      [ error,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(w),
					   #\(e),
					   #\(i),
					   #\(g),
					   #\(h),
					   #\(t),
					   #\(' '),
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(f),
					   #\(s),
					   #\(e),
					   #\(t),
					   #\(' '),
					   #\(=),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(o),
					   #\(b),
					   #\(' '),
					   #\(=),
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
					   #\(!),
					   #\(!),
					   #\(!),
					   #\(~),
					   #\('%'),
					   #\('('),
					   #\(r),
					   #\(e),
					   #\(t),
					   #\(')'),
					   #\(' '),
					   #\(t),
					   #\(o),
					   #\(' '),
					   #\(p),
					   #\(r),
					   #\(o),
					   #\(c),
					   #\(e),
					   #\(e),
					   #\(d)
					 ]),
				u_weight,
				u_offset,
				u_ob,
				u_dependency
			      ],
			      [setq, u_offset, 0.0],
			      [setq, u_weight, 1.0]
			    ]
			  ],
			  
			  [ setq,
			    u_new_strength,
			    
			    [ u_fl_c43,
			      [u_fl_c43, u_new_strength, u_offset],
			      
			      [ u_fl_xx,
				
				[ u_strength,
				  
				  [ u_ob_c36_get,
				    u_dependency,
				    [quote, u_linked_from]
				  ]
				],
				u_weight
			      ]
			    ]
			  ],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_depend,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(u),
				       #\(m),
				       #\(' '),
				       #\(s),
				       #\(o),
				       #\(' '),
				       #\(f),
				       #\(a),
				       #\(r),
				       #\(' '),
				       #\(i),
				       #\(s),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_new_strength
			  ]
			],
			
			[ u_yresult,
			  [u_set_strength, u_ob, u_new_strength],
			  
			  [ u_ndbg_roman_nl,
			    u_xx_gate_dbg_xx,
			    u_depend,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('N'),
				       #\(e),
				       #\(w),
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
				       #\(=),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_new_strength
			  ],
			  
			  [ if,
			    [u_ty_c36_instance_c63, u_ob, [quote, u_emotion]],
			    [u_normalize_emotion, u_ob, u_context]
			  ]
			]
		      ],
		      TrueResult),
	    _102738=TrueResult
	;   _102738=[]
	),
	LetResult=_102738,
	f_u_yloop(
		  [ 
		    [ u_yfor,
		      u_dependee,
		      u_in,
		      [u_get_dependees, u_ob, u_context, u_belief_path]
		    ],
		    
		    [ u_ydo,
		      
		      [ u_recalculate_strength,
			u_dependee,
			u_context,
			u_belief_path
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_recalculate_strength, classof, claz_function),
   set_opv(u_recalculate_strength, compile_as, kw_function),
   set_opv(u_recalculate_strength, function, f_u_recalculate_strength),
   DefunResult=u_recalculate_strength.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:10501 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'normalize-emotion',
			    [emot, context],
			    
			    [ if,
			      ['fl<', [strength, emot], 0.0],
			      
			      [ progn,
				['set-strength', emot, [-, [strength, emot]]],
				
				[ if,
				  ['ty$instance?', emot, [quote, 'neg-emotion']],
				  
				  [ 'no-gen',
				    
				    [ 'ob$set-type',
				      emot,
				      '*pos-emotion-ob*',
				      context
				    ]
				  ],
				  
				  [ 'no-gen',
				    
				    [ 'ob$set-type',
				      emot,
				      '*neg-emotion-ob*',
				      context
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::NORMALIZE-EMOTION 
wl: lambda_def(defun,
	      u_normalize_emotion,
	      f_u_normalize_emotion,
	      [u_emot, u_context],
	      
	      [ 
		[ if,
		  [u_fl_c60, [u_strength, u_emot], 0.0],
		  
		  [ progn,
		    [u_set_strength, u_emot, [-, [u_strength, u_emot]]],
		    
		    [ if,
		      [u_ty_c36_instance_c63, u_emot, [quote, u_neg_emotion]],
		      
		      [ u_no_gen,
			
			[ u_ob_c36_set_type,
			  u_emot,
			  u_xx_pos_emotion_ob_xx,
			  u_context
			]
		      ],
		      
		      [ u_no_gen,
			
			[ u_ob_c36_set_type,
			  u_emot,
			  u_xx_neg_emotion_ob_xx,
			  u_context
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::NORMALIZE-EMOTION 
wl: arglist_info(u_normalize_emotion,
		[u_emot, u_context],
		[Emot_Param, Context_Param],
		arginfo{ all:[u_emot, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_emot, u_context],
			 opt:0,
			 req:[u_emot, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NORMALIZE-EMOTION 
wl: init_args(exact_only, u_normalize_emotion).


% annotating U::NORMALIZE-EMOTION 
f_u_normalize_emotion(Emot_Param, Context_Param, TrueResult21) :-
	Env=[bv(u_emot, Emot_Param), bv(u_context, Context_Param)],
	f_u_fl_c60([u_strength, u_emot], 0.0, IFTEST),
	(   IFTEST\==[]
	->  f_u_set_strength(u_emot, [-, [u_strength, u_emot]], Set_strength_Ret),
	    f_u_ty_c36_instance_c63(Emot_Param, u_neg_emotion, IFTEST16),
	    (   IFTEST16\==[]
	    ->  f_u_no_gen(
			   [ 
			     [ u_ob_c36_set_type,
			       u_emot,
			       u_xx_pos_emotion_ob_xx,
			       u_context
			     ]
			   ],
			   TrueResult),
		TrueResult21=TrueResult
	    ;   f_u_no_gen(
			   [ 
			     [ u_ob_c36_set_type,
			       u_emot,
			       u_xx_neg_emotion_ob_xx,
			       u_context
			     ]
			   ],
			   ElseResult),
		TrueResult21=ElseResult
	    )
	;   TrueResult21=[]
	).
:- set_opv(f_u_normalize_emotion, classof, claz_function),
   set_opv(u_normalize_emotion, compile_as, kw_function),
   set_opv(u_normalize_emotion, function, f_u_normalize_emotion),
   DefunResult=u_normalize_emotion.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:10501 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" We need to retract and reassert because obs in contexts with",
				     1,
				     10811)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:10501 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" hashing are restricted not to change type.",
				     1,
				     10874)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:10501 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (see above comments in dd_rule for similar operations as below--",
				     1,
				     10919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:10501 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  which do not yet use below... For one, they need to use belief",
				     1,
				     10986)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:10501 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("  paths...)", 1, 11052)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:10501 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: Also, what if more than one context are involved?!!!",
				     1,
				     11065)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:11125 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$set-type',
			    [ob, 'new-type', context],
			    ['cx$retract', context, ob],
			    ['ob$set', ob, [quote, type], 'new-type'],
			    ['cx$assert', context, ob]
			  ]).

% annotating U::OB$SET-TYPE 
wl: lambda_def(defun,
	      u_ob_c36_set_type,
	      f_u_ob_c36_set_type,
	      [u_ob, u_new_type, u_context],
	      
	      [ [u_cx_c36_retract, u_context, u_ob],
		[u_ob_c36_set, u_ob, [quote, type], u_new_type],
		[u_cx_c36_assert, u_context, u_ob]
	      ]).


% annotating U::OB$SET-TYPE 
wl: arglist_info(u_ob_c36_set_type,
		[u_ob, u_new_type, u_context],
		[Ob_Param, New_type_Param, Context_Param],
		arginfo{ all:[u_ob, u_new_type, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_new_type, u_context],
			 opt:0,
			 req:[u_ob, u_new_type, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$SET-TYPE 
wl: init_args(exact_only, u_ob_c36_set_type).


% annotating U::OB$SET-TYPE 
f_u_ob_c36_set_type(Ob_Param, New_type_Param, Context_Param, FnResult) :-
	f_u_cx_c36_retract(Context_Param, Ob_Param, C36_retract_Ret),
	f_u_ob_c36_set(Ob_Param, type, New_type_Param, C36_set_Ret),
	f_u_cx_c36_assert(Context_Param, Ob_Param, C36_assert_Ret),
	C36_assert_Ret=FnResult.
:- set_opv(f_u_ob_c36_set_type, classof, claz_function),
   set_opv(u_ob_c36_set_type, compile_as, kw_function),
   set_opv(u_ob_c36_set_type, function, f_u_ob_c36_set_type),
   DefunResult=u_ob_c36_set_type.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:11248 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-other-causes',
			    [fact, context],
			    
			    [ 'ol-path',
			      fact,
			      [],
			      '*dependency-ob*',
			      [quote, backward],
			      context,
			      
			      [ lambda,
				[dummy, ob],
				
				[ if,
				  
				  [ and,
				    ['ty$instance?', ob, [quote, action]],
				    
				    [ not,
				      
				      [ 'memq?',
					'*me-ob*',
					['ob$gets', ob, [quote, actor]]
				      ]
				    ]
				  ],
				  '*empty-bd*',
				  []
				]
			      ],
			      '*empty-bd*'
			    ]
			  ]).

% annotating U::GET-OTHER-CAUSES 
wl: lambda_def(defun,
	      u_get_other_causes,
	      f_u_get_other_causes,
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
		    
		    [ if,
		      
		      [ and,
			[u_ty_c36_instance_c63, u_ob, [quote, u_action]],
			
			[ not,
			  
			  [ u_memq_c63,
			    u_xx_me_ob_xx,
			    [u_ob_c36_gets, u_ob, [quote, u_actor]]
			  ]
			]
		      ],
		      u_xx_empty_bd_xx,
		      []
		    ]
		  ],
		  u_xx_empty_bd_xx
		]
	      ]).


% annotating U::GET-OTHER-CAUSES 
wl: arglist_info(u_get_other_causes,
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

% annotating U::GET-OTHER-CAUSES 
wl: init_args(exact_only, u_get_other_causes).


% annotating U::GET-OTHER-CAUSES 
f_u_get_other_causes(Fact_Param, Context_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	Lambda=closure([Env|Env], LResult, [u_dummy, u_ob],  (get_var(Env, u_ob, Ob_Get), f_u_ty_c36_instance_c63(Ob_Get, u_action, IFTEST19), (IFTEST19\==[]->f_u_memq_c63(u_xx_me_ob_xx, [u_ob_c36_gets, u_ob, [quote, u_actor]], Not_Param), cl_not(Not_Param, TrueResult), IFTEST=TrueResult;IFTEST=[]), (IFTEST\==[]->get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get), LResult=Xx_empty_bd_xx_Get;LResult=[]))),
	get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get30),
	f_u_ol_path(Fact_Param,
		    [],
		    Xx_dependency_ob_xx_Get,
		    u_backward,
		    Context_Param,
		    Lambda,
		    Xx_empty_bd_xx_Get30,
		    Ol_path_Ret),
	Ol_path_Ret=FnResult.
:- set_opv(f_u_get_other_causes, classof, claz_function),
   set_opv(u_get_other_causes, compile_as, kw_function),
   set_opv(u_get_other_causes, function, f_u_get_other_causes),
   DefunResult=u_get_other_causes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:11624 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'tlg->string',
			    [tlg],
			    
			    [ 'string-append',
			      ['ob->string', tlg],
			      '$STRING'(": "),
			      
			      [ cond,
				
				[ ['dd-goal?', tlg],
				  
				  [ 'type->string',
				    ['ob$ty', ['ob$get', tlg, [quote, obj]]]
				  ]
				],
				
				[ ['not-hurt-goal?', tlg],
				  
				  [ if,
				    '*typeset?*',
				    '$STRING'("{\\bf{}NOT-HURT}"),
				    '$STRING'("NOT-HURT")
				  ]
				],
				
				[ ['social-esteem-goal?', tlg],
				  
				  [ if,
				    '*typeset?*',
				    '$STRING'("{\\bf{}SOCIAL-ESTEEM}"),
				    '$STRING'("SOCIAL-ESTEEM")
				  ]
				],
				
				[ ['self-esteem-goal?', tlg],
				  
				  [ if,
				    '*typeset?*',
				    '$STRING'("{\\bf{}SELF-ESTEEM}"),
				    '$STRING'("SELF-ESTEEM")
				  ]
				],
				
				[ else,
				  
				  [ 'type->string',
				    ['ob$ty', ['ob$get', tlg, [quote, obj]]]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::TLG->STRING 
wl: lambda_def(defun,
	      u_tlg_c62_string,
	      f_u_tlg_c62_string,
	      [u_tlg],
	      
	      [ 
		[ u_string_append,
		  [u_ob_c62_string, u_tlg],
		  '$ARRAY'([*], claz_base_character, [#\(:), #\(' ')]),
		  
		  [ cond,
		    
		    [ [u_dd_goal_c63, u_tlg],
		      
		      [ u_type_c62_string,
			[u_ob_c36_ty, [u_ob_c36_get, u_tlg, [quote, u_obj]]]
		      ]
		    ],
		    
		    [ [u_not_hurt_goal_c63, u_tlg],
		      
		      [ if,
			u_xx_typeset_c63_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('{'),
				   #\(\),
				   #\(b),
				   #\(f),
				   #\('{'),
				   #\('}'),
				   #\('N'),
				   #\('O'),
				   #\('T'),
				   #\(-),
				   #\('H'),
				   #\('U'),
				   #\('R'),
				   #\('T'),
				   #\('}')
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('N'),
				   #\('O'),
				   #\('T'),
				   #\(-),
				   #\('H'),
				   #\('U'),
				   #\('R'),
				   #\('T')
				 ])
		      ]
		    ],
		    
		    [ [u_social_esteem_goal_c63, u_tlg],
		      
		      [ if,
			u_xx_typeset_c63_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('{'),
				   #\(\),
				   #\(b),
				   #\(f),
				   #\('{'),
				   #\('}'),
				   #\('S'),
				   #\('O'),
				   #\('C'),
				   #\('I'),
				   #\('A'),
				   #\('L'),
				   #\(-),
				   #\('E'),
				   #\('S'),
				   #\('T'),
				   #\('E'),
				   #\('E'),
				   #\('M'),
				   #\('}')
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('S'),
				   #\('O'),
				   #\('C'),
				   #\('I'),
				   #\('A'),
				   #\('L'),
				   #\(-),
				   #\('E'),
				   #\('S'),
				   #\('T'),
				   #\('E'),
				   #\('E'),
				   #\('M')
				 ])
		      ]
		    ],
		    
		    [ [u_self_esteem_goal_c63, u_tlg],
		      
		      [ if,
			u_xx_typeset_c63_xx,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('{'),
				   #\(\),
				   #\(b),
				   #\(f),
				   #\('{'),
				   #\('}'),
				   #\('S'),
				   #\('E'),
				   #\('L'),
				   #\('F'),
				   #\(-),
				   #\('E'),
				   #\('S'),
				   #\('T'),
				   #\('E'),
				   #\('E'),
				   #\('M'),
				   #\('}')
				 ]),
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('S'),
				   #\('E'),
				   #\('L'),
				   #\('F'),
				   #\(-),
				   #\('E'),
				   #\('S'),
				   #\('T'),
				   #\('E'),
				   #\('E'),
				   #\('M')
				 ])
		      ]
		    ],
		    
		    [ u_else,
		      
		      [ u_type_c62_string,
			[u_ob_c36_ty, [u_ob_c36_get, u_tlg, [quote, u_obj]]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::TLG->STRING 
wl: arglist_info(u_tlg_c62_string,
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

% annotating U::TLG->STRING 
wl: init_args(exact_only, u_tlg_c62_string).


% annotating U::TLG->STRING 
f_u_tlg_c62_string(Tlg_Param, FnResult) :-
	Env=[bv(u_tlg, Tlg_Param)],
	f_u_string_append(
			  [ [u_ob_c62_string, u_tlg],
			    '$ARRAY'([*], claz_base_character, [#\(:), #\(' ')]),
			    
			    [ cond,
			      
			      [ [u_dd_goal_c63, u_tlg],
				
				[ u_type_c62_string,
				  
				  [ u_ob_c36_ty,
				    [u_ob_c36_get, u_tlg, [quote, u_obj]]
				  ]
				]
			      ],
			      
			      [ [u_not_hurt_goal_c63, u_tlg],
				
				[ if,
				  u_xx_typeset_c63_xx,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('{'),
					     #\(\),
					     #\(b),
					     #\(f),
					     #\('{'),
					     #\('}'),
					     #\('N'),
					     #\('O'),
					     #\('T'),
					     #\(-),
					     #\('H'),
					     #\('U'),
					     #\('R'),
					     #\('T'),
					     #\('}')
					   ]),
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('N'),
					     #\('O'),
					     #\('T'),
					     #\(-),
					     #\('H'),
					     #\('U'),
					     #\('R'),
					     #\('T')
					   ])
				]
			      ],
			      
			      [ [u_social_esteem_goal_c63, u_tlg],
				
				[ if,
				  u_xx_typeset_c63_xx,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('{'),
					     #\(\),
					     #\(b),
					     #\(f),
					     #\('{'),
					     #\('}'),
					     #\('S'),
					     #\('O'),
					     #\('C'),
					     #\('I'),
					     #\('A'),
					     #\('L'),
					     #\(-),
					     #\('E'),
					     #\('S'),
					     #\('T'),
					     #\('E'),
					     #\('E'),
					     #\('M'),
					     #\('}')
					   ]),
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('S'),
					     #\('O'),
					     #\('C'),
					     #\('I'),
					     #\('A'),
					     #\('L'),
					     #\(-),
					     #\('E'),
					     #\('S'),
					     #\('T'),
					     #\('E'),
					     #\('E'),
					     #\('M')
					   ])
				]
			      ],
			      
			      [ [u_self_esteem_goal_c63, u_tlg],
				
				[ if,
				  u_xx_typeset_c63_xx,
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('{'),
					     #\(\),
					     #\(b),
					     #\(f),
					     #\('{'),
					     #\('}'),
					     #\('S'),
					     #\('E'),
					     #\('L'),
					     #\('F'),
					     #\(-),
					     #\('E'),
					     #\('S'),
					     #\('T'),
					     #\('E'),
					     #\('E'),
					     #\('M'),
					     #\('}')
					   ]),
				  '$ARRAY'([*],
					   claz_base_character,
					   
					   [ #\('S'),
					     #\('E'),
					     #\('L'),
					     #\('F'),
					     #\(-),
					     #\('E'),
					     #\('S'),
					     #\('T'),
					     #\('E'),
					     #\('E'),
					     #\('M')
					   ])
				]
			      ],
			      
			      [ u_else,
				
				[ u_type_c62_string,
				  
				  [ u_ob_c36_ty,
				    [u_ob_c36_get, u_tlg, [quote, u_obj]]
				  ]
				]
			      ]
			    ]
			  ],
			  String_append_Ret),
	String_append_Ret=FnResult.
:- set_opv(f_u_tlg_c62_string, classof, claz_function),
   set_opv(u_tlg_c62_string, compile_as, kw_function),
   set_opv(u_tlg_c62_string, function, f_u_tlg_c62_string),
   DefunResult=u_tlg_c62_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:12070 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'dd-goal?',
			    [goal],
			    
			    [ and,
			      ['ob?', ['ob$get', goal, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', goal, [quote, obj]],
				[quote, 'dd-goal-obj']
			      ]
			    ]
			  ]).

% annotating U::DD-GOAL? 
wl: lambda_def(defun,
	      u_dd_goal_c63,
	      f_u_dd_goal_c63,
	      [u_goal],
	      
	      [ 
		[ and,
		  [u_ob_c63, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    [quote, u_dd_goal_obj]
		  ]
		]
	      ]).


% annotating U::DD-GOAL? 
wl: arglist_info(u_dd_goal_c63,
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

% annotating U::DD-GOAL? 
wl: init_args(exact_only, u_dd_goal_c63).


% annotating U::DD-GOAL? 
f_u_dd_goal_c63(Goal_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ob_c63([u_ob_c36_get, u_goal, [quote, u_obj]], IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Goal_Param, u_obj, Obj),
	    f_u_ty_c36_instance_c63(Obj, u_dd_goal_obj, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_dd_goal_c63, classof, claz_function),
   set_opv(u_dd_goal_c63, compile_as, kw_function),
   set_opv(u_dd_goal_c63, function, f_u_dd_goal_c63),
   DefunResult=u_dd_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:12182 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'social-esteem-goal?',
			    [goal],
			    
			    [ and,
			      ['ob?', goal],
			      ['ob?', ['ob$get', goal, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', goal, [quote, obj]],
				[quote, 'BELIEVE']
			      ],
			      
			      [ 'neq?',
				'*me-ob*',
				['ob$get', goal, [quote, [obj, actor]]]
			      ],
			      ['ob?', ['ob$pget', goal, [quote, [obj, obj]]]],
			      
			      [ 'ty$instance?',
				['ob$pget', goal, [quote, [obj, obj]]],
				[quote, 'POS-ATTITUDE']
			      ],
			      
			      [ 'eq?',
				'*me-ob*',
				['ob$pget', goal, [quote, [obj, obj, obj]]]
			      ]
			    ]
			  ]).

% annotating U::SOCIAL-ESTEEM-GOAL? 
wl: lambda_def(defun,
	      u_social_esteem_goal_c63,
	      f_u_social_esteem_goal_c63,
	      [u_goal],
	      
	      [ 
		[ and,
		  [u_ob_c63, u_goal],
		  [u_ob_c63, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    [quote, u_believe]
		  ],
		  
		  [ u_neq_c63,
		    u_xx_me_ob_xx,
		    [u_ob_c36_get, u_goal, [quote, [u_obj, u_actor]]]
		  ],
		  [u_ob_c63, [u_ob_c36_pget, u_goal, [quote, [u_obj, u_obj]]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_pget, u_goal, [quote, [u_obj, u_obj]]],
		    [quote, u_pos_attitude]
		  ],
		  
		  [ u_eq_c63,
		    u_xx_me_ob_xx,
		    [u_ob_c36_pget, u_goal, [quote, [u_obj, u_obj, u_obj]]]
		  ]
		]
	      ]).


% annotating U::SOCIAL-ESTEEM-GOAL? 
wl: arglist_info(u_social_esteem_goal_c63,
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

% annotating U::SOCIAL-ESTEEM-GOAL? 
wl: init_args(exact_only, u_social_esteem_goal_c63).


% annotating U::SOCIAL-ESTEEM-GOAL? 
f_u_social_esteem_goal_c63(Goal_Param, TrueResult27) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ob_c63(u_goal, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_goal, [quote, u_obj]], IFTEST14),
	    (   IFTEST14\==[]
	    ->  f_u_ob_c36_get(Goal_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_believe, IFTEST16),
		(   IFTEST16\==[]
		->  f_u_neq_c63(u_xx_me_ob_xx,
				[u_ob_c36_get, u_goal, [quote, [u_obj, u_actor]]],
				IFTEST19),
		    (   IFTEST19\==[]
		    ->  f_u_ob_c63(
				   [ u_ob_c36_pget,
				     u_goal,
				     [quote, [u_obj, u_obj]]
				   ],
				   IFTEST21),
			(   IFTEST21\==[]
			->  f_u_ob_c36_pget(Goal_Param,
					    [u_obj, u_obj],
					    Instance_c63_Param),
			    f_u_ty_c36_instance_c63(Instance_c63_Param,
						    u_pos_attitude,
						    IFTEST23),
			    (   IFTEST23\==[]
			    ->  f_u_eq_c63(u_xx_me_ob_xx,
					   
					   [ u_ob_c36_pget,
					     u_goal,
					     [quote, [u_obj, u_obj, u_obj]]
					   ],
					   TrueResult),
				TrueResult27=TrueResult
			    ;   TrueResult27=[]
			    )
			;   TrueResult27=[]
			)
		    ;   TrueResult27=[]
		    )
		;   TrueResult27=[]
		)
	    ;   TrueResult27=[]
	    )
	;   TrueResult27=[]
	).
:- set_opv(f_u_social_esteem_goal_c63, classof, claz_function),
   set_opv(u_social_esteem_goal_c63, compile_as, kw_function),
   set_opv(u_social_esteem_goal_c63, function, f_u_social_esteem_goal_c63),
   DefunResult=u_social_esteem_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:12520 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'self-esteem-goal?',
			    [goal],
			    
			    [ and,
			      ['ob?', goal],
			      ['ob?', ['ob$get', goal, [quote, obj]]],
			      
			      [ 'ty$instance?',
				['ob$get', goal, [quote, obj]],
				[quote, 'POS-ATTITUDE']
			      ],
			      
			      [ 'eq?',
				'*me-ob*',
				['ob$pget', goal, [quote, [obj, obj]]]
			      ]
			    ]
			  ]).

% annotating U::SELF-ESTEEM-GOAL? 
wl: lambda_def(defun,
	      u_self_esteem_goal_c63,
	      f_u_self_esteem_goal_c63,
	      [u_goal],
	      
	      [ 
		[ and,
		  [u_ob_c63, u_goal],
		  [u_ob_c63, [u_ob_c36_get, u_goal, [quote, u_obj]]],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    [quote, u_pos_attitude]
		  ],
		  
		  [ u_eq_c63,
		    u_xx_me_ob_xx,
		    [u_ob_c36_pget, u_goal, [quote, [u_obj, u_obj]]]
		  ]
		]
	      ]).


% annotating U::SELF-ESTEEM-GOAL? 
wl: arglist_info(u_self_esteem_goal_c63,
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

% annotating U::SELF-ESTEEM-GOAL? 
wl: init_args(exact_only, u_self_esteem_goal_c63).


% annotating U::SELF-ESTEEM-GOAL? 
f_u_self_esteem_goal_c63(Goal_Param, TrueResult20) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ob_c63(u_goal, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c63([u_ob_c36_get, u_goal, [quote, u_obj]], IFTEST14),
	    (   IFTEST14\==[]
	    ->  f_u_ob_c36_get(Goal_Param, u_obj, Obj),
		f_u_ty_c36_instance_c63(Obj, u_pos_attitude, IFTEST16),
		(   IFTEST16\==[]
		->  f_u_eq_c63(u_xx_me_ob_xx,
			       [u_ob_c36_pget, u_goal, [quote, [u_obj, u_obj]]],
			       TrueResult),
		    TrueResult20=TrueResult
		;   TrueResult20=[]
		)
	    ;   TrueResult20=[]
	    )
	;   TrueResult20=[]
	).
:- set_opv(f_u_self_esteem_goal_c63, classof, claz_function),
   set_opv(u_self_esteem_goal_c63, compile_as, kw_function),
   set_opv(u_self_esteem_goal_c63, function, f_u_self_esteem_goal_c63),
   DefunResult=u_self_esteem_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:12707 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'personal-goal?',
			    [goal],
			    
			    [ let,
			      [[obj, ['ob$get', goal, [quote, obj]]]],
			      
			      [ and,
				['ob?', obj],
				
				[ cond,
				  [['memq?', obj, '*new-personal-goals*'], t],
				  
				  [ 
				    [ or,
				      ['ty$instance?', obj, [quote, employment]],
				      ['ty$instance?', obj, [quote, lovers]]
				    ],
				    
				    [ 'memq?',
				      '*me-ob*',
				      ['ob$gets', obj, [quote, actor]]
				    ]
				  ],
				  
				  [ else,
				    
				    [ or,
				      ['social-esteem-goal?', goal],
				      ['not-hurt-goal?', goal],
				      
				      [ 'ty$instance?',
					obj,
					[quote, 'personal-goal-obj']
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::PERSONAL-GOAL? 
wl: lambda_def(defun,
	      u_personal_goal_c63,
	      f_u_personal_goal_c63,
	      [u_goal],
	      
	      [ 
		[ let,
		  [[u_obj, [u_ob_c36_get, u_goal, [quote, u_obj]]]],
		  
		  [ and,
		    [u_ob_c63, u_obj],
		    
		    [ cond,
		      [[u_memq_c63, u_obj, u_xx_new_personal_goals_xx], t],
		      
		      [ 
			[ or,
			  [u_ty_c36_instance_c63, u_obj, [quote, u_employment]],
			  [u_ty_c36_instance_c63, u_obj, [quote, u_lovers]]
			],
			
			[ u_memq_c63,
			  u_xx_me_ob_xx,
			  [u_ob_c36_gets, u_obj, [quote, u_actor]]
			]
		      ],
		      
		      [ u_else,
			
			[ or,
			  [u_social_esteem_goal_c63, u_goal],
			  [u_not_hurt_goal_c63, u_goal],
			  
			  [ u_ty_c36_instance_c63,
			    u_obj,
			    [quote, u_personal_goal_obj]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::PERSONAL-GOAL? 
wl: arglist_info(u_personal_goal_c63,
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

% annotating U::PERSONAL-GOAL? 
wl: init_args(exact_only, u_personal_goal_c63).


% annotating U::PERSONAL-GOAL? 
f_u_personal_goal_c63(Goal_Param, ElseResult37) :-
	Env=[bv(u_goal, Goal_Param)],
	f_u_ob_c36_get(Goal_Param, u_obj, Obj_Init),
	LEnv=[[bv(u_obj, Obj_Init)]|Env],
	f_u_ob_c63(u_obj, IFTEST),
	(   IFTEST\==[]
	->  f_u_memq_c63(u_obj, u_xx_new_personal_goals_xx, IFTEST18),
	    (   IFTEST18\==[]
	    ->  ElseResult37=t
	    ;   (   get_var(LEnv, u_obj, Obj_Get),
		    f_u_ty_c36_instance_c63(Obj_Get, u_employment, FORM1_Res),
		    FORM1_Res\==[],
		    IFTEST20=FORM1_Res
		->  true
		;   get_var(LEnv, u_obj, Obj_Get24),
		    f_u_ty_c36_instance_c63(Obj_Get24, u_lovers, Lovers),
		    IFTEST20=Lovers
		),
		(   IFTEST20\==[]
		->  f_u_memq_c63(u_xx_me_ob_xx,
				 [u_ob_c36_gets, u_obj, [quote, u_actor]],
				 TrueResult36),
		    ElseResult37=TrueResult36
		;   get_var(LEnv, u_else, IFTEST26),
		    (   IFTEST26\==[]
		    ->  (   f_u_social_esteem_goal_c63(Goal_Param, FORM1_Res33),
			    FORM1_Res33\==[],
			    ElseResult37=FORM1_Res33
			->  true
			;   f_u_not_hurt_goal_c63(Goal_Param, FORM1_Res32),
			    FORM1_Res32\==[],
			    ElseResult37=FORM1_Res32
			->  true
			;   get_var(LEnv, u_obj, Obj_Get31),
			    f_u_ty_c36_instance_c63(Obj_Get31,
						    u_personal_goal_obj,
						    Personal_goal_obj),
			    ElseResult37=Personal_goal_obj
			)
		    ;   ElseResult37=[]
		    )
		)
	    )
	;   ElseResult37=[]
	).
:- set_opv(f_u_personal_goal_c63, classof, claz_function),
   set_opv(u_personal_goal_c63, compile_as, kw_function),
   set_opv(u_personal_goal_c63, function, f_u_personal_goal_c63),
   DefunResult=u_personal_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13108 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'not-hurt-goal?',
			    [goal],
			    
			    [ and,
			      
			      [ 'ty$instance?',
				['ob$get', goal, [quote, obj]],
				[quote, not]
			      ],
			      
			      [ 'ty$instance?',
				['ob$pget', goal, [quote, [obj, obj]]],
				[quote, hurt]
			      ]
			    ]
			  ]).

% annotating U::NOT-HURT-GOAL? 
wl: lambda_def(defun,
	      u_not_hurt_goal_c63,
	      f_u_not_hurt_goal_c63,
	      [u_goal],
	      
	      [ 
		[ and,
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_goal, [quote, u_obj]],
		    [quote, not]
		  ],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_pget, u_goal, [quote, [u_obj, u_obj]]],
		    [quote, u_hurt]
		  ]
		]
	      ]).


% annotating U::NOT-HURT-GOAL? 
wl: arglist_info(u_not_hurt_goal_c63,
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

% annotating U::NOT-HURT-GOAL? 
wl: init_args(exact_only, u_not_hurt_goal_c63).


% annotating U::NOT-HURT-GOAL? 
f_u_not_hurt_goal_c63(Goal_Param, FnResult) :-
	f_u_ob_c36_get(Goal_Param, u_obj, Obj),
	f_u_ty_c36_instance_c63(Obj, not, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_pget(Goal_Param, [u_obj, u_obj], Instance_c63_Param),
	    f_u_ty_c36_instance_c63(Instance_c63_Param, u_hurt, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_not_hurt_goal_c63, classof, claz_function),
   set_opv(u_not_hurt_goal_c63, compile_as, kw_function),
   set_opv(u_not_hurt_goal_c63, function, f_u_not_hurt_goal_c63),
   DefunResult=u_not_hurt_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13108 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13241)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13108 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Enter concepts from the external world for performance mode and",
				     1,
				     13243)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13108 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" environmental object input.", 1, 13309)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13108 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13339)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13108 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Assert and add to *entered-concepts* (if not object)",
				     1,
				     13341)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13108 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13396)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13397 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'enter-concepts',
			    [context, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      rule,
			      '$STRING'("Enter concepts in ~A"),
			      context
			    ],
			    
			    [ yloop,
			      
			      [ initial,
				[line, []],
				[concept, []],
				[concepts, []],
				[done, []]
			      ],
			      [yuntil, done],
			      
			      [ ydo,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  rule,
				  '$STRING'("Parser> ")
				],
				[setq, line, ['read-input']],
				
				[ if,
				  ['string-empty?', line],
				  [setq, done, t],
				  
				  [ progn,
				    [setq, concept, [parse, line]],
				    
				    [ if,
				      concept,
				      
				      [ progn,
					
					[ 'ndbg-roman-nl',
					  '*gate-dbg*',
					  rule,
					  '$STRING'("Input received")
					],
					
					[ setq,
					  concepts,
					  [append, concept, concepts]
					],
					
					[ yloop,
					  [yfor, con, in, concept],
					  
					  [ ydo,
					    
					    [ if,
					      
					      [ not,
						
						[ 'ty$instance?',
						  con,
						  [quote, object]
						]
					      ],
					      
					      [ progn,
						
						[ 'ob$set',
						  con,
						  [quote, 'input-state?'],
						  t
						],
						
						[ 'cx$hyper-assert-relative',
						  context,
						  con,
						  'belief-path'
						],
						
						[ setq,
						  '*entered-concepts*',
						  
						  [ 'append!',
						    '*entered-concepts*',
						    [list, con]
						  ]
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
			      [yresult, concepts]
			    ]
			  ]).

% annotating U::ENTER-CONCEPTS 
wl: lambda_def(defun,
	      u_enter_concepts,
	      f_u_enter_concepts,
	      [u_context, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_rule,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('E'),
			     #\(n),
			     #\(t),
			     #\(e),
			     #\(r),
			     #\(' '),
			     #\(c),
			     #\(o),
			     #\(n),
			     #\(c),
			     #\(e),
			     #\(p),
			     #\(t),
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
		  
		  [ u_initial,
		    [u_line, []],
		    [u_concept, []],
		    [u_concepts, []],
		    [u_done, []]
		  ],
		  [u_yuntil, u_done],
		  
		  [ u_ydo,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_rule,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('P'),
				 #\(a),
				 #\(r),
				 #\(s),
				 #\(e),
				 #\(r),
				 #\(>),
				 #\(' ')
			       ])
		    ],
		    [setq, u_line, [u_read_input]],
		    
		    [ if,
		      [u_string_empty_c63, u_line],
		      [setq, u_done, t],
		      
		      [ progn,
			[setq, u_concept, [u_parse, u_line]],
			
			[ if,
			  u_concept,
			  
			  [ progn,
			    
			    [ u_ndbg_roman_nl,
			      u_xx_gate_dbg_xx,
			      u_rule,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('I'),
					 #\(n),
					 #\(p),
					 #\(u),
					 #\(t),
					 #\(' '),
					 #\(r),
					 #\(e),
					 #\(c),
					 #\(e),
					 #\(i),
					 #\(v),
					 #\(e),
					 #\(d)
				       ])
			    ],
			    [setq, u_concepts, [append, u_concept, u_concepts]],
			    
			    [ u_yloop,
			      [u_yfor, u_con, u_in, u_concept],
			      
			      [ u_ydo,
				
				[ if,
				  
				  [ not,
				    
				    [ u_ty_c36_instance_c63,
				      u_con,
				      [quote, u_object]
				    ]
				  ],
				  
				  [ progn,
				    
				    [ u_ob_c36_set,
				      u_con,
				      [quote, u_input_state_c63],
				      t
				    ],
				    
				    [ u_cx_c36_hyper_assert_relative,
				      u_context,
				      u_con,
				      u_belief_path
				    ],
				    
				    [ setq,
				      u_xx_entered_concepts_xx,
				      
				      [ u_append_c33,
					u_xx_entered_concepts_xx,
					[list, u_con]
				      ]
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
		  [u_yresult, u_concepts]
		]
	      ]).


% annotating U::ENTER-CONCEPTS 
wl: arglist_info(u_enter_concepts,
		[u_context, u_belief_path],
		[Context_Param, Belief_path_Param],
		arginfo{ all:[u_context, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_belief_path],
			 opt:0,
			 req:[u_context, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ENTER-CONCEPTS 
wl: init_args(exact_only, u_enter_concepts).


% annotating U::ENTER-CONCEPTS 
f_u_enter_concepts(Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('E'),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(' '),
				       #\(c),
				       #\(o),
				       #\(n),
				       #\(c),
				       #\(e),
				       #\(p),
				       #\(t),
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
		  [ 
		    [ u_initial,
		      [u_line, []],
		      [u_concept, []],
		      [u_concepts, []],
		      [u_done, []]
		    ],
		    [u_yuntil, u_done],
		    
		    [ u_ydo,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('P'),
				   #\(a),
				   #\(r),
				   #\(s),
				   #\(e),
				   #\(r),
				   #\(>),
				   #\(' ')
				 ])
		      ],
		      [setq, u_line, [u_read_input]],
		      
		      [ if,
			[u_string_empty_c63, u_line],
			[setq, u_done, t],
			
			[ progn,
			  [setq, u_concept, [u_parse, u_line]],
			  
			  [ if,
			    u_concept,
			    
			    [ progn,
			      
			      [ u_ndbg_roman_nl,
				u_xx_gate_dbg_xx,
				u_rule,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(n),
					   #\(p),
					   #\(u),
					   #\(t),
					   #\(' '),
					   #\(r),
					   #\(e),
					   #\(c),
					   #\(e),
					   #\(i),
					   #\(v),
					   #\(e),
					   #\(d)
					 ])
			      ],
			      [setq, u_concepts, [append, u_concept, u_concepts]],
			      
			      [ u_yloop,
				[u_yfor, u_con, u_in, u_concept],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ not,
				      
				      [ u_ty_c36_instance_c63,
					u_con,
					[quote, u_object]
				      ]
				    ],
				    
				    [ progn,
				      
				      [ u_ob_c36_set,
					u_con,
					[quote, u_input_state_c63],
					t
				      ],
				      
				      [ u_cx_c36_hyper_assert_relative,
					u_context,
					u_con,
					u_belief_path
				      ],
				      
				      [ setq,
					u_xx_entered_concepts_xx,
					
					[ u_append_c33,
					  u_xx_entered_concepts_xx,
					  [list, u_con]
					]
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
		    [u_yresult, u_concepts]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_enter_concepts, classof, claz_function),
   set_opv(u_enter_concepts, compile_as, kw_function),
   set_opv(u_enter_concepts, function, f_u_enter_concepts),
   DefunResult=u_enter_concepts.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:13397 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              (generate1 concept *global-switches* context belief-path)",
				     1,
				     13888)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:14440 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*empty-string*', '$STRING'("")]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_empty_string_xx,
	   '$ARRAY'([*], claz_base_character, [])).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:14466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'read-input',
			    [],
			    
			    [ let,
			      [[line, ['read-line', '*gate-input*', []]]],
			      
			      [ cond,
				
				[ [null, line],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("[stdin] ")
				  ],
				  [close, '*gate-input*'],
				  [setq, '*gate-input*', ['standard-input']],
				  ['read-input']
				],
				[['string-empty?', line], ['read-input']],
				
				[ ['string-equal?', line, '$STRING'("quit")],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Breaking DAYDREAMER...")
				  ],
				  [breakpoint],
				  ['read-input']
				],
				
				[ 
				  [ not,
				    ['string-equal?', line, '$STRING'("end")]
				  ],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Input: ~A"),
				    line
				  ],
				  line
				],
				
				[ else,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("End of parser input")
				  ],
				  '*empty-string*'
				]
			      ]
			    ]
			  ]).

% annotating U::READ-INPUT 
wl: lambda_def(defun,
	      u_read_input,
	      f_u_read_input,
	      [],
	      
	      [ 
		[ let,
		  [[u_line, [read_line, u_xx_gate_input_xx, []]]],
		  
		  [ cond,
		    
		    [ [null, u_line],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('['),
				   #\(s),
				   #\(t),
				   #\(d),
				   #\(i),
				   #\(n),
				   #\(']'),
				   #\(' ')
				 ])
		      ],
		      [close, u_xx_gate_input_xx],
		      [setq, u_xx_gate_input_xx, [u_standard_input]],
		      [u_read_input]
		    ],
		    [[u_string_empty_c63, u_line], [u_read_input]],
		    
		    [ 
		      [ u_string_equal_c63,
			u_line,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(q), #\(u), #\(i), #\(t)])
		      ],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('B'),
				   #\(r),
				   #\(e),
				   #\(a),
				   #\(k),
				   #\(i),
				   #\(n),
				   #\(g),
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
				   #\('R'),
				   #\('.'),
				   #\('.'),
				   #\('.')
				 ])
		      ],
		      [u_breakpoint],
		      [u_read_input]
		    ],
		    
		    [ 
		      [ not,
			
			[ u_string_equal_c63,
			  u_line,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\(e), #\(n), #\(d)])
			]
		      ],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('I'),
				   #\(n),
				   #\(p),
				   #\(u),
				   #\(t),
				   #\(:),
				   #\(' '),
				   #\(~),
				   #\('A')
				 ]),
			u_line
		      ],
		      u_line
		    ],
		    
		    [ u_else,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('E'),
				   #\(n),
				   #\(d),
				   #\(' '),
				   #\(o),
				   #\(f),
				   #\(' '),
				   #\(p),
				   #\(a),
				   #\(r),
				   #\(s),
				   #\(e),
				   #\(r),
				   #\(' '),
				   #\(i),
				   #\(n),
				   #\(p),
				   #\(u),
				   #\(t)
				 ])
		      ],
		      u_xx_empty_string_xx
		    ]
		  ]
		]
	      ]).


% annotating U::READ-INPUT 
wl: arglist_info(u_read_input,
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

% annotating U::READ-INPUT 
wl: init_args(exact_only, u_read_input).


% annotating U::READ-INPUT 
f_u_read_input(ElseResult34) :-
	Env=[],
	get_var(Env, u_xx_gate_input_xx, Xx_gate_input_xx_Get18),
	cl_read_line(Xx_gate_input_xx_Get18, [], Line_Init),
	LEnv=[[bv(u_line, Line_Init)]|Env],
	get_var(LEnv, u_line, IFTEST),
	(   IFTEST==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('['),
					   #\(s),
					   #\(t),
					   #\(d),
					   #\(i),
					   #\(n),
					   #\(']'),
					   #\(' ')
					 ])
			      ],
			      Roman_nl_Ret),
	    cl_close(Xx_gate_input_xx_Get18, Close_Ret),
	    f_u_standard_input(Xx_gate_input_xx),
	    set_var(LEnv, u_xx_gate_input_xx, Xx_gate_input_xx),
	    f_u_read_input(TrueResult39),
	    ElseResult34=TrueResult39
	;   f_u_string_empty_c63(u_line, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_u_read_input(TrueResult37),
		ElseResult34=TrueResult37
	    ;   f_u_string_equal_c63(u_line,
				     '$ARRAY'([*],
					      claz_base_character,
					      [#\(q), #\(u), #\(i), #\(t)]),
				     IFTEST21),
		(   IFTEST21\==[]
		->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
				      u_rule,
				      
				      [ '$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\('B'),
						   #\(r),
						   #\(e),
						   #\(a),
						   #\(k),
						   #\(i),
						   #\(n),
						   #\(g),
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
						   #\('R'),
						   #\('.'),
						   #\('.'),
						   #\('.')
						 ])
				      ],
				      Roman_nl_Ret46),
		    f_u_breakpoint(Breakpoint_Ret),
		    f_u_read_input(TrueResult35),
		    ElseResult34=TrueResult35
		;   f_u_string_equal_c63(u_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  [#\(e), #\(n), #\(d)]),
					 PredArgResult),
		    (   PredArgResult==[]
		    ->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
					  u_rule,
					  
					  [ '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\('I'),
						       #\(n),
						       #\(p),
						       #\(u),
						       #\(t),
						       #\(:),
						       #\(' '),
						       #\(~),
						       #\('A')
						     ]),
					    u_line
					  ],
					  Roman_nl_Ret48),
			get_var(LEnv, u_line, Line_Get26),
			ElseResult34=Line_Get26
		    ;   get_var(LEnv, u_else, IFTEST27),
			(   IFTEST27\==[]
			->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
					      u_rule,
					      
					      [ '$ARRAY'([*],
							 claz_base_character,
							 
							 [ #\('E'),
							   #\(n),
							   #\(d),
							   #\(' '),
							   #\(o),
							   #\(f),
							   #\(' '),
							   #\(p),
							   #\(a),
							   #\(r),
							   #\(s),
							   #\(e),
							   #\(r),
							   #\(' '),
							   #\(i),
							   #\(n),
							   #\(p),
							   #\(u),
							   #\(t)
							 ])
					      ],
					      Roman_nl_Ret49),
			    get_var(LEnv,
				    u_xx_empty_string_xx,
				    Xx_empty_string_xx_Get),
			    ElseResult34=Xx_empty_string_xx_Get
			;   ElseResult34=[]
			)
		    )
		)
	    )
	).
:- set_opv(f_u_read_input, classof, claz_function),
   set_opv(u_read_input, compile_as, kw_function),
   set_opv(u_read_input, function, f_u_read_input),
   DefunResult=u_read_input.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:15085 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*phrases*', []]).
:- set_var(TLEnv3, setq, u_xx_phrases_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:15107 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    parse,
			    [str],
			    
			    [ let,
			      
			      [ 
				[ found,
				  
				  [ assoc,
				    ['string-downcase', str],
				    '*phrases*',
				    ':test',
				    [quote, 'string-equal']
				  ]
				]
			      ],
			      
			      [ if,
				found,
				
				[ map,
				  [quote, list],
				  [lambda, [x], ['ob$copy', x]],
				  [cdr, found]
				],
				
				[ progn,
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Cannot parse input (ignored)")
				  ],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Input = '~A'"),
				    str
				  ],
				  []
				]
			      ]
			    ]
			  ]).

% annotating U::PARSE 
wl: lambda_def(defun,
	      u_parse,
	      f_u_parse,
	      [u_str],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_found,
		      
		      [ assoc,
			[string_downcase, u_str],
			u_xx_phrases_xx,
			kw_test,
			[quote, string_equal]
		      ]
		    ]
		  ],
		  
		  [ if,
		    u_found,
		    
		    [ map,
		      [quote, list],
		      [lambda, [u_x], [u_ob_c36_copy, u_x]],
		      [cdr, u_found]
		    ],
		    
		    [ progn,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('C'),
				   #\(a),
				   #\(n),
				   #\(n),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(p),
				   #\(a),
				   #\(r),
				   #\(s),
				   #\(e),
				   #\(' '),
				   #\(i),
				   #\(n),
				   #\(p),
				   #\(u),
				   #\(t),
				   #\(' '),
				   #\('('),
				   #\(i),
				   #\(g),
				   #\(n),
				   #\(o),
				   #\(r),
				   #\(e),
				   #\(d),
				   #\(')')
				 ])
		      ],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('I'),
				   #\(n),
				   #\(p),
				   #\(u),
				   #\(t),
				   #\(' '),
				   #\(=),
				   #\(' '),
				   #\('\''),
				   #\(~),
				   #\('A'),
				   #\('\'')
				 ]),
			u_str
		      ],
		      []
		    ]
		  ]
		]
	      ]).


% annotating U::PARSE 
wl: arglist_info(u_parse,
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

% annotating U::PARSE 
wl: init_args(exact_only, u_parse).


% annotating U::PARSE 
f_u_parse(Str_Param, FnResult) :-
	Env=[bv(u_str, Str_Param)],
	cl_string_downcase(Str_Param, Assoc_Param),
	get_var(Env, u_xx_phrases_xx, Xx_phrases_xx_Get),
	cl_assoc(Assoc_Param,
		 Xx_phrases_xx_Get,
		 [kw_test, string_equal],
		 Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  Lambda=closure([Env22|LEnv], LResult, [u_x],  (get_var(Env22, u_x, X_Get), f_u_ob_c36_copy(X_Get, LResult))),
	    get_var(LEnv, u_found, Found_Get26),
	    cl_cdr(Found_Get26, Cdr_Ret),
	    cl_map(list, Lambda, Cdr_Ret, TrueResult),
	    FnResult=TrueResult
	;   f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('C'),
					   #\(a),
					   #\(n),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(p),
					   #\(a),
					   #\(r),
					   #\(s),
					   #\(e),
					   #\(' '),
					   #\(i),
					   #\(n),
					   #\(p),
					   #\(u),
					   #\(t),
					   #\(' '),
					   #\('('),
					   #\(i),
					   #\(g),
					   #\(n),
					   #\(o),
					   #\(r),
					   #\(e),
					   #\(d),
					   #\(')')
					 ])
			      ],
			      Roman_nl_Ret),
	    f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_rule,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('I'),
					   #\(n),
					   #\(p),
					   #\(u),
					   #\(t),
					   #\(' '),
					   #\(=),
					   #\(' '),
					   #\('\''),
					   #\(~),
					   #\('A'),
					   #\('\'')
					 ]),
				u_str
			      ],
			      Roman_nl_Ret33),
	    FnResult=[]
	).
:- set_opv(f_u_parse, classof, claz_function),
   set_opv(u_parse, compile_as, kw_function),
   set_opv(u_parse, function, f_u_parse),
   DefunResult=u_parse.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:15474 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bd-print',
			    [bd, stream],
			    
			    [ if,
			      bd,
			      
			      [ yloop,
				[initial, [temp, []], [already, []]],
				[yfor, bdp, in, [cdr, bd]],
				
				[ ydo,
				  
				  [ if,
				    [not, ['memq?', [car, bdp], already]],
				    
				    [ progn,
				      [setq, already, [cons, [car, bdp], already]],
				      [setq, temp, ['symbol->string', [car, bdp]]],
				      ['string-downcase!', [chdr, temp]],
				      ['begin-slanted-font', stream],
				      [format, stream, '$STRING'("?~A"), temp],
				      ['end-font', stream],
				      ['begin-regular-font', stream],
				      
				      [ format,
					stream,
					'$STRING'(" = ~A"),
					[cadr, bdp]
				      ],
				      ['end-font', stream],
				      ['do-newline', stream]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::BD-PRINT 
wl: lambda_def(defun,
	      u_bd_print,
	      f_u_bd_print,
	      [u_bd, stream],
	      
	      [ 
		[ if,
		  u_bd,
		  
		  [ u_yloop,
		    [u_initial, [u_temp, []], [u_already, []]],
		    [u_yfor, u_bdp, u_in, [cdr, u_bd]],
		    
		    [ u_ydo,
		      
		      [ if,
			[not, [u_memq_c63, [car, u_bdp], u_already]],
			
			[ progn,
			  [setq, u_already, [cons, [car, u_bdp], u_already]],
			  [setq, u_temp, [u_symbol_c62_string, [car, u_bdp]]],
			  [u_string_downcase_c33, [u_chdr, u_temp]],
			  [u_begin_slanted_font, stream],
			  
			  [ format,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(?), #\(~), #\('A')]),
			    u_temp
			  ],
			  [u_end_font, stream],
			  [u_begin_regular_font, stream],
			  
			  [ format,
			    stream,
			    '$ARRAY'([*],
				     claz_base_character,
				     [#\(' '), #\(=), #\(' '), #\(~), #\('A')]),
			    [cadr, u_bdp]
			  ],
			  [u_end_font, stream],
			  [u_do_newline, stream]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::BD-PRINT 
wl: arglist_info(u_bd_print,
		[u_bd, stream],
		[Bd_Param, Stream_Param],
		arginfo{ all:[u_bd, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd, stream],
			 opt:0,
			 req:[u_bd, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-PRINT 
wl: init_args(exact_only, u_bd_print).


% annotating U::BD-PRINT 
f_u_bd_print(Bd_Param, Stream_Param, FnResult) :-
	Env=[bv(u_bd, Bd_Param), bv(stream, Stream_Param)],
	(   Bd_Param\==[]
	->  f_u_yloop(
		      [ [u_initial, [u_temp, []], [u_already, []]],
			[u_yfor, u_bdp, u_in, [cdr, u_bd]],
			
			[ u_ydo,
			  
			  [ if,
			    [not, [u_memq_c63, [car, u_bdp], u_already]],
			    
			    [ progn,
			      [setq, u_already, [cons, [car, u_bdp], u_already]],
			      [setq, u_temp, [u_symbol_c62_string, [car, u_bdp]]],
			      [u_string_downcase_c33, [u_chdr, u_temp]],
			      [u_begin_slanted_font, stream],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(?), #\(~), #\('A')]),
				u_temp
			      ],
			      [u_end_font, stream],
			      [u_begin_regular_font, stream],
			      
			      [ format,
				stream,
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(' '), #\(=), #\(' '), #\(~), #\('A')]),
				[cadr, u_bdp]
			      ],
			      [u_end_font, stream],
			      [u_do_newline, stream]
			    ]
			  ]
			]
		      ],
		      TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_bd_print, classof, claz_function),
   set_opv(u_bd_print, compile_as, kw_function),
   set_opv(u_bd_print, function, f_u_bd_print),
   DefunResult=u_bd_print.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16196 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*ordering-ptn*',
			    ['ob$fcreate', [quote, ['ORDERING']]]
			  ]).
:- f_u_ob_c36_fcreate([quote, [u_ordering]], _Ignored),
   set_var(TLEnv3, u_xx_ordering_ptn_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16244 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    ordering,
			    [context],
			    
			    [ let,
			      
			      [ 
				[ found,
				  ['cx$retrieve', context, '*ordering-ptn*']
				]
			      ],
			      
			      [ if,
				found,
				['ob$get', [car, [car, found]], [quote, value]],
				
				[ error,
				  '$STRING'("Ordering not found in ~A"),
				  context
				]
			      ]
			    ]
			  ]).

% annotating U::ORDERING 
wl: lambda_def(defun,
	      u_ordering,
	      f_u_ordering,
	      [u_context],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_found,
		      [u_cx_c36_retrieve, u_context, u_xx_ordering_ptn_xx]
		    ]
		  ],
		  
		  [ if,
		    u_found,
		    [u_ob_c36_get, [car, [car, u_found]], [quote, u_value]],
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('O'),
				 #\(r),
				 #\(d),
				 #\(e),
				 #\(r),
				 #\(i),
				 #\(n),
				 #\(g),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(u),
				 #\(n),
				 #\(d),
				 #\(' '),
				 #\(i),
				 #\(n),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_context
		    ]
		  ]
		]
	      ]).


% annotating U::ORDERING 
wl: arglist_info(u_ordering,
		[u_context],
		[Context_Param],
		arginfo{ all:[u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context],
			 opt:0,
			 req:[u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ORDERING 
wl: init_args(exact_only, u_ordering).


% annotating U::ORDERING 
f_u_ordering(Context_Param, FnResult) :-
	Env=[bv(u_context, Context_Param)],
	get_var(Env, u_xx_ordering_ptn_xx, Xx_ordering_ptn_xx_Get),
	f_u_cx_c36_retrieve(Context_Param, Xx_ordering_ptn_xx_Get, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get21),
	    cl_car(Found_Get21, Car_Param),
	    cl_car(Car_Param, C36_get_Param),
	    f_u_ob_c36_get(C36_get_Param, u_value, TrueResult),
	    FnResult=TrueResult
	;   cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('O'),
				  #\(r),
				  #\(d),
				  #\(e),
				  #\(r),
				  #\(i),
				  #\(n),
				  #\(g),
				  #\(' '),
				  #\(n),
				  #\(o),
				  #\(t),
				  #\(' '),
				  #\(f),
				  #\(o),
				  #\(u),
				  #\(n),
				  #\(d),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(~),
				  #\('A')
				]),
		       Context_Param
		     ],
		     ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ordering, classof, claz_function),
   set_opv(u_ordering, compile_as, kw_function),
   set_opv(u_ordering, function, f_u_ordering),
   DefunResult=u_ordering.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16435 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*altern-ptn*',
			    ['ob$fcreate', [quote, ['ALTERN']]]
			  ]).
:- f_u_ob_c36_fcreate([quote, [u_altern]], _Ignored),
   set_var(TLEnv3, u_xx_altern_ptn_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16479 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'altern?',
			    [context],
			    ['cx$retrieve', context, '*altern-ptn*']
			  ]).

% annotating U::ALTERN? 
wl: lambda_def(defun,
	      u_altern_c63,
	      f_u_altern_c63,
	      [u_context],
	      [[u_cx_c36_retrieve, u_context, u_xx_altern_ptn_xx]]).


% annotating U::ALTERN? 
wl: arglist_info(u_altern_c63,
		[u_context],
		[Context_Param],
		arginfo{ all:[u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context],
			 opt:0,
			 req:[u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ALTERN? 
wl: init_args(exact_only, u_altern_c63).


% annotating U::ALTERN? 
f_u_altern_c63(Context_Param, FnResult) :-
	Env=[bv(u_context, Context_Param)],
	get_var(Env, u_xx_altern_ptn_xx, Xx_altern_ptn_xx_Get),
	f_u_cx_c36_retrieve(Context_Param,
			    Xx_altern_ptn_xx_Get,
			    C36_retrieve_Ret),
	C36_retrieve_Ret=FnResult.
:- set_opv(f_u_altern_c63, classof, claz_function),
   set_opv(u_altern_c63, compile_as, kw_function),
   set_opv(u_altern_c63, function, f_u_altern_c63),
   DefunResult=u_altern_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16543 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'set-altern',
			    [context],
			    ['cx$assert', context, '*altern-ptn*']
			  ]).

% annotating U::SET-ALTERN 
wl: lambda_def(defun,
	      u_set_altern,
	      f_u_set_altern,
	      [u_context],
	      [[u_cx_c36_assert, u_context, u_xx_altern_ptn_xx]]).


% annotating U::SET-ALTERN 
wl: arglist_info(u_set_altern,
		[u_context],
		[Context_Param],
		arginfo{ all:[u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context],
			 opt:0,
			 req:[u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-ALTERN 
wl: init_args(exact_only, u_set_altern).


% annotating U::SET-ALTERN 
f_u_set_altern(Context_Param, FnResult) :-
	Env=[bv(u_context, Context_Param)],
	get_var(Env, u_xx_altern_ptn_xx, Xx_altern_ptn_xx_Get),
	f_u_cx_c36_assert(Context_Param, Xx_altern_ptn_xx_Get, C36_assert_Ret),
	C36_assert_Ret=FnResult.
:- set_opv(f_u_set_altern, classof, claz_function),
   set_opv(u_set_altern, compile_as, kw_function),
   set_opv(u_set_altern, function, f_u_set_altern),
   DefunResult=u_set_altern.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16608 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'set-ordering',
			    [context, value],
			    
			    [ let,
			      
			      [ 
				[ found,
				  ['cx$retrieve', context, '*ordering-ptn*']
				]
			      ],
			      
			      [ if,
				found,
				['cx$retract', context, [car, [car, found]]]
			      ],
			      
			      [ 'cx$assert',
				context,
				
				[ 'ob$fcreate',
				  ['#BQ', ['ORDERING', value, ['#COMMA', value]]]
				]
			      ]
			    ]
			  ]).

% annotating U::SET-ORDERING 
wl: lambda_def(defun,
	      u_set_ordering,
	      f_u_set_ordering,
	      [u_context, u_value],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_found,
		      [u_cx_c36_retrieve, u_context, u_xx_ordering_ptn_xx]
		    ]
		  ],
		  
		  [ if,
		    u_found,
		    [u_cx_c36_retract, u_context, [car, [car, u_found]]]
		  ],
		  
		  [ u_cx_c36_assert,
		    u_context,
		    
		    [ u_ob_c36_fcreate,
		      ['#BQ', [u_ordering, u_value, ['#COMMA', u_value]]]
		    ]
		  ]
		]
	      ]).


% annotating U::SET-ORDERING 
wl: arglist_info(u_set_ordering,
		[u_context, u_value],
		[Context_Param, Value_Param],
		arginfo{ all:[u_context, u_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context, u_value],
			 opt:0,
			 req:[u_context, u_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SET-ORDERING 
wl: init_args(exact_only, u_set_ordering).


% annotating U::SET-ORDERING 
f_u_set_ordering(Context_Param, Value_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_value, Value_Param)],
	get_var(Env, u_xx_ordering_ptn_xx, Xx_ordering_ptn_xx_Get),
	f_u_cx_c36_retrieve(Context_Param, Xx_ordering_ptn_xx_Get, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get24),
	    cl_car(Found_Get24, Car_Param),
	    cl_car(Car_Param, Car_Ret),
	    f_u_cx_c36_retract(Context_Param, Car_Ret, TrueResult),
	    _99058=TrueResult
	;   _99058=[]
	),
	f_u_ob_c36_fcreate(['#BQ', [u_ordering, u_value, ['#COMMA', u_value]]],
			   C36_fcreate_Ret),
	f_u_cx_c36_assert(Context_Param, C36_fcreate_Ret, C36_assert_Ret),
	LetResult=C36_assert_Ret,
	LetResult=FnResult.
:- set_opv(f_u_set_ordering, classof, claz_function),
   set_opv(u_set_ordering, compile_as, kw_function),
   set_opv(u_set_ordering, function, f_u_set_ordering),
   DefunResult=u_set_ordering.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16825 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*other-ptn*',
			    ['ob$fcreate', [quote, ['OTHER']]]
			  ]).
:- f_u_ob_c36_fcreate([quote, [u_other]], _Ignored),
   set_var(TLEnv3, u_xx_other_ptn_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16825 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Also want to allow nested planning stuff: the below only",
				     1,
				     16868)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:16927 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    others,
			    [context],
			    
			    [ append,
			      
			      [ map,
				[quote, list],
				[lambda, [x], ['ob$get', [car, x], [quote, actor]]],
				['cx$retrieve', context, '*other-ptn*']
			      ],
			      [list, '*me-ob*']
			    ]
			  ]).

% annotating U::OTHERS 
wl: lambda_def(defun,
	      u_others,
	      f_u_others,
	      [u_context],
	      
	      [ 
		[ append,
		  
		  [ map,
		    [quote, list],
		    [lambda, [u_x], [u_ob_c36_get, [car, u_x], [quote, u_actor]]],
		    [u_cx_c36_retrieve, u_context, u_xx_other_ptn_xx]
		  ],
		  [list, u_xx_me_ob_xx]
		]
	      ]).


% annotating U::OTHERS 
wl: arglist_info(u_others,
		[u_context],
		[Context_Param],
		arginfo{ all:[u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_context],
			 opt:0,
			 req:[u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OTHERS 
wl: init_args(exact_only, u_others).


% annotating U::OTHERS 
f_u_others(Context_Param, FnResult) :-
	Env=[bv(u_context, Context_Param)],
	Lambda=closure([Env13|Env], LResult, [u_x],  (get_var(Env13, u_x, X_Get), cl_car(X_Get, C36_get_Param), f_u_ob_c36_get(C36_get_Param, u_actor, LResult))),
	get_var(Env, u_xx_other_ptn_xx, Xx_other_ptn_xx_Get),
	f_u_cx_c36_retrieve(Context_Param,
			    Xx_other_ptn_xx_Get,
			    C36_retrieve_Ret),
	cl_map(list, Lambda, C36_retrieve_Ret, Append_Param),
	get_var(Env, u_xx_me_ob_xx, Xx_me_ob_xx_Get),
	_99140=[Xx_me_ob_xx_Get],
	cl_append(Append_Param, _99140, Append_Ret),
	Append_Ret=FnResult.
:- set_opv(f_u_others, classof, claz_function),
   set_opv(u_others, compile_as, kw_function),
   set_opv(u_others, function, f_u_others),
   DefunResult=u_others.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    '->belief-path',
			    [x],
			    
			    [ if,
			      ['eq?', x, '*me-ob*'],
			      [list, '*me-ob*'],
			      [list, x, '*me-ob*']
			    ]
			  ]).

% annotating U::->BELIEF-PATH 
wl: lambda_def(defun,
	      u_c62_belief_path,
	      f_u_c62_belief_path,
	      [u_x],
	      
	      [ 
		[ if,
		  [u_eq_c63, u_x, u_xx_me_ob_xx],
		  [list, u_xx_me_ob_xx],
		  [list, u_x, u_xx_me_ob_xx]
		]
	      ]).


% annotating U::->BELIEF-PATH 
wl: arglist_info(u_c62_belief_path,
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

% annotating U::->BELIEF-PATH 
wl: init_args(exact_only, u_c62_belief_path).


% annotating U::->BELIEF-PATH 
f_u_c62_belief_path(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_eq_c63(u_x, u_xx_me_ob_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_xx_me_ob_xx, Xx_me_ob_xx_Get),
	    FnResult=[Xx_me_ob_xx_Get]
	;   get_var(Env, u_xx_me_ob_xx, Xx_me_ob_xx_Get16),
	    FnResult=[X_Param, Xx_me_ob_xx_Get16]
	).
:- set_opv(f_u_c62_belief_path, classof, claz_function),
   set_opv(u_c62_belief_path, compile_as, kw_function),
   set_opv(u_c62_belief_path, function, f_u_c62_belief_path),
   DefunResult=u_c62_belief_path.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" belief path       absolute                          relative",
				     1,
				     17179)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" -----------       --------                          --------",
				     1,
				     17242)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (*me-ob*)             ?x                                ?x",
				     1,
				     17305)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (*debra-ob* *me-ob*)      (Believe Debra ?x)                ?x",
				     1,
				     17366)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (*me-ob* *debra-ob* *me-ob*)  (Believe Debra (Believe Me ?x))   ?x",
				     1,
				     17431)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17500)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" invariant: (tlast belief-path) = *me-ob*",
				     1,
				     17502)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17545)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" All things are asserted and retrieved in the database in absolute form.",
				     1,
				     17547)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Planning and inferencing is done relatively.",
				     1,
				     17621)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17082 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 17668)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17669 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'relative->absolute',
			    [con, 'belief-path'],
			    
			    [ if,
			      ['null?', 'belief-path'],
			      
			      [ error,
				'$STRING'("Bogus belief path ~A"),
				'belief-path'
			      ]
			    ],
			    
			    [ if,
			      ['null?', [cdr, 'belief-path']],
			      con,
			      
			      [ 'ob$fcreate',
				
				[ '#BQ',
				  
				  [ 'BELIEVE',
				    actor,
				    ['#COMMA', [car, 'belief-path']],
				    obj,
				    
				    [ '#COMMA',
				      
				      [ 'relative->absolute',
					con,
					[cdr, 'belief-path']
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::RELATIVE->ABSOLUTE 
wl: lambda_def(defun,
	      u_relative_c62_absolute,
	      f_u_relative_c62_absolute,
	      [u_con, u_belief_path],
	      
	      [ 
		[ if,
		  [u_null_c63, u_belief_path],
		  
		  [ error,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('B'),
			       #\(o),
			       #\(g),
			       #\(u),
			       #\(s),
			       #\(' '),
			       #\(b),
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
			       #\(~),
			       #\('A')
			     ]),
		    u_belief_path
		  ]
		],
		
		[ if,
		  [u_null_c63, [cdr, u_belief_path]],
		  u_con,
		  
		  [ u_ob_c36_fcreate,
		    
		    [ '#BQ',
		      
		      [ u_believe,
			u_actor,
			['#COMMA', [car, u_belief_path]],
			u_obj,
			
			[ '#COMMA',
			  [u_relative_c62_absolute, u_con, [cdr, u_belief_path]]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::RELATIVE->ABSOLUTE 
wl: arglist_info(u_relative_c62_absolute,
		[u_con, u_belief_path],
		[Con_Param, Belief_path_Param],
		arginfo{ all:[u_con, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, u_belief_path],
			 opt:0,
			 req:[u_con, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RELATIVE->ABSOLUTE 
wl: init_args(exact_only, u_relative_c62_absolute).


% annotating U::RELATIVE->ABSOLUTE 
f_u_relative_c62_absolute(Con_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_null_c63(u_belief_path, IFTEST),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('B'),
				  #\(o),
				  #\(g),
				  #\(u),
				  #\(s),
				  #\(' '),
				  #\(b),
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
				  #\(~),
				  #\('A')
				]),
		       Belief_path_Param
		     ],
		     TrueResult),
	    _99544=TrueResult
	;   _99544=[]
	),
	f_u_null_c63([cdr, u_belief_path], IFTEST18),
	(   IFTEST18\==[]
	->  FnResult=Con_Param
	;   f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_believe,
				   u_actor,
				   ['#COMMA', [car, u_belief_path]],
				   u_obj,
				   
				   [ '#COMMA',
				     
				     [ u_relative_c62_absolute,
				       u_con,
				       [cdr, u_belief_path]
				     ]
				   ]
				 ]
			       ],
			       ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_relative_c62_absolute, classof, claz_function),
   set_opv(u_relative_c62_absolute, compile_as, kw_function),
   set_opv(u_relative_c62_absolute, function, f_u_relative_c62_absolute),
   DefunResult=u_relative_c62_absolute.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:17669 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns nil if no relative equivalent.",
				     1,
				     17976)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:18016 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'absolute->relative',
			    [con, 'belief-path'],
			    
			    [ if,
			      ['null?', [cdr, 'belief-path']],
			      con,
			      
			      [ if,
				
				[ and,
				  ['ty$instance?', con, [quote, believe]],
				  
				  [ 'eq?',
				    [car, 'belief-path'],
				    ['ob$get', con, [quote, actor]]
				  ]
				],
				
				[ 'absolute->relative',
				  ['ob$get', con, [quote, obj]],
				  [cdr, 'belief-path']
				],
				[]
			      ]
			    ]
			  ]).

% annotating U::ABSOLUTE->RELATIVE 
wl: lambda_def(defun,
	      u_absolute_c62_relative,
	      f_u_absolute_c62_relative,
	      [u_con, u_belief_path],
	      
	      [ 
		[ if,
		  [u_null_c63, [cdr, u_belief_path]],
		  u_con,
		  
		  [ if,
		    
		    [ and,
		      [u_ty_c36_instance_c63, u_con, [quote, u_believe]],
		      
		      [ u_eq_c63,
			[car, u_belief_path],
			[u_ob_c36_get, u_con, [quote, u_actor]]
		      ]
		    ],
		    
		    [ u_absolute_c62_relative,
		      [u_ob_c36_get, u_con, [quote, u_obj]],
		      [cdr, u_belief_path]
		    ],
		    []
		  ]
		]
	      ]).


% annotating U::ABSOLUTE->RELATIVE 
wl: arglist_info(u_absolute_c62_relative,
		[u_con, u_belief_path],
		[Con_Param, Belief_path_Param],
		arginfo{ all:[u_con, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_con, u_belief_path],
			 opt:0,
			 req:[u_con, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ABSOLUTE->RELATIVE 
wl: init_args(exact_only, u_absolute_c62_relative).


% annotating U::ABSOLUTE->RELATIVE 
f_u_absolute_c62_relative(Con_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_con, Con_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_null_c63([cdr, u_belief_path], IFTEST),
	(   IFTEST\==[]
	->  FnResult=Con_Param
	;   f_u_ty_c36_instance_c63(Con_Param, u_believe, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_u_eq_c63([car, u_belief_path],
			   [u_ob_c36_get, u_con, [quote, u_actor]],
			   TrueResult),
		IFTEST17=TrueResult
	    ;   IFTEST17=[]
	    ),
	    (   IFTEST17\==[]
	    ->  f_u_ob_c36_get(Con_Param, u_obj, Obj),
		cl_cdr(Belief_path_Param, Cdr_Ret),
		f_u_absolute_c62_relative(Obj, Cdr_Ret, TrueResult25),
		FnResult=TrueResult25
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_absolute_c62_relative, classof, claz_function),
   set_opv(u_absolute_c62_relative, compile_as, kw_function),
   set_opv(u_absolute_c62_relative, function, f_u_absolute_c62_relative),
   DefunResult=u_absolute_c62_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:18290 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-relative',
			    [self, ob, 'belief-path'],
			    
			    [ 'absolute-bd-list->relative',
			      
			      [ 'cx$retrieve',
				self,
				['relative->absolute', ob, 'belief-path']
			      ],
			      'belief-path'
			    ]
			  ]).

% annotating U::CX$RETRIEVE-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_relative,
	      f_u_cx_c36_retrieve_relative,
	      [u_self, u_ob, u_belief_path],
	      
	      [ 
		[ u_absolute_bd_list_c62_relative,
		  
		  [ u_cx_c36_retrieve,
		    u_self,
		    [u_relative_c62_absolute, u_ob, u_belief_path]
		  ],
		  u_belief_path
		]
	      ]).


% annotating U::CX$RETRIEVE-RELATIVE 
wl: arglist_info(u_cx_c36_retrieve_relative,
		[u_self, u_ob, u_belief_path],
		[Self_Param, Ob_Param, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE-RELATIVE 
wl: init_args(exact_only, u_cx_c36_retrieve_relative).


% annotating U::CX$RETRIEVE-RELATIVE 
f_u_cx_c36_retrieve_relative(Self_Param, Ob_Param, Belief_path_Param, FnResult) :-
	f_u_relative_c62_absolute(Ob_Param, Belief_path_Param, C62_absolute_Ret),
	f_u_cx_c36_retrieve(Self_Param, C62_absolute_Ret, C62_relative_Param),
	f_u_absolute_bd_list_c62_relative(C62_relative_Param,
					  Belief_path_Param,
					  C62_relative_Ret),
	C62_relative_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_relative, classof, claz_function),
   set_opv(u_cx_c36_retrieve_relative, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_relative, function, f_u_cx_c36_retrieve_relative),
   DefunResult=u_cx_c36_retrieve_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:18446 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-bd-relative-ignore',
			    [self, ob, bd, 'belief-path', ignore],
			    
			    [ 'absolute-bd-list->relative',
			      
			      [ 'cx$retrieve-bd-ignore',
				self,
				['relative->absolute', ob, 'belief-path'],
				bd,
				ignore
			      ],
			      'belief-path'
			    ]
			  ]).

% annotating U::CX$RETRIEVE-BD-RELATIVE-IGNORE 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_bd_relative_ignore,
	      f_u_cx_c36_retrieve_bd_relative_ignore,
	      [u_self, u_ob, u_bd, u_belief_path, ignore],
	      
	      [ 
		[ u_absolute_bd_list_c62_relative,
		  
		  [ u_cx_c36_retrieve_bd_ignore,
		    u_self,
		    [u_relative_c62_absolute, u_ob, u_belief_path],
		    u_bd,
		    ignore
		  ],
		  u_belief_path
		]
	      ]).


% annotating U::CX$RETRIEVE-BD-RELATIVE-IGNORE 
wl: arglist_info(u_cx_c36_retrieve_bd_relative_ignore,
		[u_self, u_ob, u_bd, u_belief_path, ignore],
		[Self_Param, Ob_Param, Bd_Param, Belief_path_Param, Ignore_Param],
		arginfo{ all:[u_self, u_ob, u_bd, u_belief_path, ignore],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_bd, u_belief_path, ignore],
			 opt:0,
			 req:[u_self, u_ob, u_bd, u_belief_path, ignore],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE-BD-RELATIVE-IGNORE 
wl: init_args(exact_only, u_cx_c36_retrieve_bd_relative_ignore).


% annotating U::CX$RETRIEVE-BD-RELATIVE-IGNORE 
f_u_cx_c36_retrieve_bd_relative_ignore(Self_Param, Ob_Param, Bd_Param, Belief_path_Param, Ignore_Param, FnResult) :-
	Env=[bv(ignore, Ignore_Param)],
	f_u_relative_c62_absolute(Ob_Param, Belief_path_Param, C62_absolute_Ret),
	f_u_cx_c36_retrieve_bd_ignore(Self_Param,
				      C62_absolute_Ret,
				      Bd_Param,
				      ignore,
				      Ignore),
	f_u_absolute_bd_list_c62_relative(Ignore,
					  Belief_path_Param,
					  C62_relative_Ret),
	C62_relative_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_bd_relative_ignore, classof, claz_function),
   set_opv(u_cx_c36_retrieve_bd_relative_ignore, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_bd_relative_ignore,
	   function,
	   f_u_cx_c36_retrieve_bd_relative_ignore),
   DefunResult=u_cx_c36_retrieve_bd_relative_ignore.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:18668 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-bd-ignore',
			    [self, pattern, bd, ignore],
			    
			    [ yloop,
			      [initial, [result, []], [bindings, []]],
			      
			      [ yfor,
				ob,
				in,
				
				[ if,
				  '*hashing?*',
				  ['cx$retrieve-hash', self, pattern],
				  ['ob$get', self, [quote, 'all-obs']]
				]
			      ],
			      
			      [ ydo,
				
				[ setq,
				  bindings,
				  ['ob$unify-cx1', pattern, ob, bd, ignore, self]
				],
				
				[ if,
				  bindings,
				  
				  [ setq,
				    result,
				    [cons, [cons, ob, [cdr, bindings]], result]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::CX$RETRIEVE-BD-IGNORE 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_bd_ignore,
	      f_u_cx_c36_retrieve_bd_ignore,
	      [u_self, u_pattern, u_bd, ignore],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [bindings, []]],
		  
		  [ u_yfor,
		    u_ob,
		    u_in,
		    
		    [ if,
		      u_xx_hashing_c63_xx,
		      [u_cx_c36_retrieve_hash, u_self, u_pattern],
		      [u_ob_c36_get, u_self, [quote, u_all_obs]]
		    ]
		  ],
		  
		  [ u_ydo,
		    
		    [ setq,
		      bindings,
		      [u_ob_c36_unify_cx1, u_pattern, u_ob, u_bd, ignore, u_self]
		    ],
		    
		    [ if,
		      bindings,
		      
		      [ setq,
			u_result,
			[cons, [cons, u_ob, [cdr, bindings]], u_result]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::CX$RETRIEVE-BD-IGNORE 
wl: arglist_info(u_cx_c36_retrieve_bd_ignore,
		[u_self, u_pattern, u_bd, ignore],
		[Self_Param, Pattern_Param, Bd_Param, Ignore_Param],
		arginfo{ all:[u_self, u_pattern, u_bd, ignore],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_pattern, u_bd, ignore],
			 opt:0,
			 req:[u_self, u_pattern, u_bd, ignore],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE-BD-IGNORE 
wl: init_args(exact_only, u_cx_c36_retrieve_bd_ignore).


% annotating U::CX$RETRIEVE-BD-IGNORE 
f_u_cx_c36_retrieve_bd_ignore(Self_Param, Pattern_Param, Bd_Param, Ignore_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_pattern, Pattern_Param), bv(u_bd, Bd_Param), bv(ignore, Ignore_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [bindings, []]],
		    
		    [ u_yfor,
		      u_ob,
		      u_in,
		      
		      [ if,
			u_xx_hashing_c63_xx,
			[u_cx_c36_retrieve_hash, u_self, u_pattern],
			[u_ob_c36_get, u_self, [quote, u_all_obs]]
		      ]
		    ],
		    
		    [ u_ydo,
		      
		      [ setq,
			bindings,
			[u_ob_c36_unify_cx1, u_pattern, u_ob, u_bd, ignore, u_self]
		      ],
		      
		      [ if,
			bindings,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_ob, [cdr, bindings]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_bd_ignore, classof, claz_function),
   set_opv(u_cx_c36_retrieve_bd_ignore, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_bd_ignore, function, f_u_cx_c36_retrieve_bd_ignore),
   DefunResult=u_cx_c36_retrieve_bd_ignore.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19160 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retrieve-bd-relative',
			    [self, ob, bd, 'belief-path'],
			    
			    [ 'absolute-bd-list->relative',
			      
			      [ 'cx$retrieve-bd',
				self,
				['relative->absolute', ob, 'belief-path'],
				bd
			      ],
			      'belief-path'
			    ]
			  ]).

% annotating U::CX$RETRIEVE-BD-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_retrieve_bd_relative,
	      f_u_cx_c36_retrieve_bd_relative,
	      [u_self, u_ob, u_bd, u_belief_path],
	      
	      [ 
		[ u_absolute_bd_list_c62_relative,
		  
		  [ u_cx_c36_retrieve_bd,
		    u_self,
		    [u_relative_c62_absolute, u_ob, u_belief_path],
		    u_bd
		  ],
		  u_belief_path
		]
	      ]).


% annotating U::CX$RETRIEVE-BD-RELATIVE 
wl: arglist_info(u_cx_c36_retrieve_bd_relative,
		[u_self, u_ob, u_bd, u_belief_path],
		[Self_Param, Ob_Param, Bd_Param, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_bd, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_bd, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_bd, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRIEVE-BD-RELATIVE 
wl: init_args(exact_only, u_cx_c36_retrieve_bd_relative).


% annotating U::CX$RETRIEVE-BD-RELATIVE 
f_u_cx_c36_retrieve_bd_relative(Self_Param, Ob_Param, Bd_Param, Belief_path_Param, FnResult) :-
	f_u_relative_c62_absolute(Ob_Param, Belief_path_Param, C62_absolute_Ret),
	f_u_cx_c36_retrieve_bd(Self_Param,
			       C62_absolute_Ret,
			       Bd_Param,
			       C62_relative_Param),
	f_u_absolute_bd_list_c62_relative(C62_relative_Param,
					  Belief_path_Param,
					  C62_relative_Ret),
	C62_relative_Ret=FnResult.
:- set_opv(f_u_cx_c36_retrieve_bd_relative, classof, claz_function),
   set_opv(u_cx_c36_retrieve_bd_relative, compile_as, kw_function),
   set_opv(u_cx_c36_retrieve_bd_relative,
	   function,
	   f_u_cx_c36_retrieve_bd_relative),
   DefunResult=u_cx_c36_retrieve_bd_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$true-relative',
			    [self, ob, 'belief-path'],
			    
			    [ if,
			      ['me-belief-path?', 'belief-path'],
			      ['cx$true?', self, ob],
			      
			      [ 'absolute-bd-list->relative',
				
				[ 'cx$retrieve',
				  self,
				  ['relative->absolute', ob, 'belief-path']
				],
				'belief-path'
			      ]
			    ]
			  ]).

% annotating U::CX$TRUE-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_true_relative,
	      f_u_cx_c36_true_relative,
	      [u_self, u_ob, u_belief_path],
	      
	      [ 
		[ if,
		  [u_me_belief_path_c63, u_belief_path],
		  [u_cx_c36_true_c63, u_self, u_ob],
		  
		  [ u_absolute_bd_list_c62_relative,
		    
		    [ u_cx_c36_retrieve,
		      u_self,
		      [u_relative_c62_absolute, u_ob, u_belief_path]
		    ],
		    u_belief_path
		  ]
		]
	      ]).


% annotating U::CX$TRUE-RELATIVE 
wl: arglist_info(u_cx_c36_true_relative,
		[u_self, u_ob, u_belief_path],
		[Self_Param, Ob_Param, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$TRUE-RELATIVE 
wl: init_args(exact_only, u_cx_c36_true_relative).


% annotating U::CX$TRUE-RELATIVE 
f_u_cx_c36_true_relative(Self_Param, Ob_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_belief_path, Belief_path_Param)],
	f_u_me_belief_path_c63(u_belief_path, IFTEST),
	(   IFTEST\==[]
	->  f_u_cx_c36_true_c63(Self_Param, Ob_Param, TrueResult),
	    FnResult=TrueResult
	;   f_u_relative_c62_absolute(Ob_Param,
				      Belief_path_Param,
				      C62_absolute_Ret),
	    f_u_cx_c36_retrieve(Self_Param,
				C62_absolute_Ret,
				C62_relative_Param),
	    f_u_absolute_bd_list_c62_relative(C62_relative_Param,
					      Belief_path_Param,
					      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_true_relative, classof, claz_function),
   set_opv(u_cx_c36_true_relative, compile_as, kw_function),
   set_opv(u_cx_c36_true_relative, function, f_u_cx_c36_true_relative),
   DefunResult=u_cx_c36_true_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Or keep mapping so we don't have to do a retrieve each time.",
				     1,
				     19555)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Mapping is set up upon assert and destroyed upon retract.",
				     1,
				     19618)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (But this doesn't work in the face of multiple contexts. Thus",
				     1,
				     19678)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" gc/maintenance problems make this infeasible.)",
				     1,
				     19742)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Cannot use (cx$retract self) here as an optimization for",
				     1,
				     19791)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" me-belief-path case, because we want to delete things that",
				     1,
				     19850)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19328 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" unify with the ob.", 1, 19911)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:19931 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$retract-relative',
			    [self, ob, 'belief-path'],
			    
			    [ let,
			      
			      [ 
				[ found,
				  ['cx$intern-relative', self, ob, 'belief-path']
				]
			      ],
			      
			      [ 'ndbg-roman-nl',
				'*gate-dbg*',
				'rule-xtra',
				'$STRING'("Interned ~A as ~A"),
				ob,
				found
			      ],
			      
			      [ if,
				['null?', found],
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  context,
				  '$STRING'("Retract-relative: Cannot intern ~A in ~A"),
				  ob,
				  self
				],
				['cx$retract', self, found]
			      ]
			    ]
			  ]).

% annotating U::CX$RETRACT-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_retract_relative,
	      f_u_cx_c36_retract_relative,
	      [u_self, u_ob, u_belief_path],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_found,
		      [u_cx_c36_intern_relative, u_self, u_ob, u_belief_path]
		    ]
		  ],
		  
		  [ u_ndbg_roman_nl,
		    u_xx_gate_dbg_xx,
		    u_rule_xtra,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('I'),
			       #\(n),
			       #\(t),
			       #\(e),
			       #\(r),
			       #\(n),
			       #\(e),
			       #\(d),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(a),
			       #\(s),
			       #\(' '),
			       #\(~),
			       #\('A')
			     ]),
		    u_ob,
		    u_found
		  ],
		  
		  [ if,
		    [u_null_c63, u_found],
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_context,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('R'),
				 #\(e),
				 #\(t),
				 #\(r),
				 #\(a),
				 #\(c),
				 #\(t),
				 #\(-),
				 #\(r),
				 #\(e),
				 #\(l),
				 #\(a),
				 #\(t),
				 #\(i),
				 #\(v),
				 #\(e),
				 #\(:),
				 #\(' '),
				 #\('C'),
				 #\(a),
				 #\(n),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(i),
				 #\(n),
				 #\(t),
				 #\(e),
				 #\(r),
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
		      u_self
		    ],
		    [u_cx_c36_retract, u_self, u_found]
		  ]
		]
	      ]).


% annotating U::CX$RETRACT-RELATIVE 
wl: arglist_info(u_cx_c36_retract_relative,
		[u_self, u_ob, u_belief_path],
		[Self_Param, Ob_Param, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$RETRACT-RELATIVE 
wl: init_args(exact_only, u_cx_c36_retract_relative).


% annotating U::CX$RETRACT-RELATIVE 
f_u_cx_c36_retract_relative(Self_Param, Ob_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_cx_c36_intern_relative(Self_Param,
				   Ob_Param,
				   Belief_path_Param,
				   Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_rule_xtra,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(n),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(a),
				       #\(s),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_ob,
			    u_found
			  ],
			  Roman_nl_Ret),
	f_u_null_c63(u_found, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_context,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('R'),
					   #\(e),
					   #\(t),
					   #\(r),
					   #\(a),
					   #\(c),
					   #\(t),
					   #\(-),
					   #\(r),
					   #\(e),
					   #\(l),
					   #\(a),
					   #\(t),
					   #\(i),
					   #\(v),
					   #\(e),
					   #\(:),
					   #\(' '),
					   #\('C'),
					   #\(a),
					   #\(n),
					   #\(n),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(i),
					   #\(n),
					   #\(t),
					   #\(e),
					   #\(r),
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
				u_self
			      ],
			      TrueResult),
	    FnResult=TrueResult
	;   get_var(LEnv, u_found, Found_Get),
	    f_u_cx_c36_retract(Self_Param, Found_Get, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_cx_c36_retract_relative, classof, claz_function),
   set_opv(u_cx_c36_retract_relative, compile_as, kw_function),
   set_opv(u_cx_c36_retract_relative, function, f_u_cx_c36_retract_relative),
   DefunResult=u_cx_c36_retract_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:20308 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$intern-relative',
			    [self, ob, 'belief-path'],
			    
			    [ cond,
			      [['me-belief-path?', 'belief-path'], ob],
			      
			      [ else,
				
				[ any,
				  
				  [ lambda,
				    ['belief-bd'],
				    
				    [ if,
				      
				      [ 'eq?',
					
					[ 'absolute->relative',
					  [car, 'belief-bd'],
					  'belief-path'
					],
					ob
				      ],
				      [car, 'belief-bd'],
				      []
				    ]
				  ],
				  
				  [ 'cx$retrieve',
				    self,
				    
				    [ 'relative->absolute',
				      '*any-pattern*',
				      'belief-path'
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::CX$INTERN-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_intern_relative,
	      f_u_cx_c36_intern_relative,
	      [u_self, u_ob, u_belief_path],
	      
	      [ 
		[ cond,
		  [[u_me_belief_path_c63, u_belief_path], u_ob],
		  
		  [ u_else,
		    
		    [ u_any,
		      
		      [ lambda,
			[u_belief_bd],
			
			[ if,
			  
			  [ u_eq_c63,
			    
			    [ u_absolute_c62_relative,
			      [car, u_belief_bd],
			      u_belief_path
			    ],
			    u_ob
			  ],
			  [car, u_belief_bd],
			  []
			]
		      ],
		      
		      [ u_cx_c36_retrieve,
			u_self,
			
			[ u_relative_c62_absolute,
			  u_xx_any_pattern_xx,
			  u_belief_path
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::CX$INTERN-RELATIVE 
wl: arglist_info(u_cx_c36_intern_relative,
		[u_self, u_ob, u_belief_path],
		[Self_Param, Ob_Get, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$INTERN-RELATIVE 
wl: init_args(exact_only, u_cx_c36_intern_relative).


% annotating U::CX$INTERN-RELATIVE 
f_u_cx_c36_intern_relative(Self_Param, Ob_Get, Belief_path_Param, ElseResult25) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Get), bv(u_belief_path, Belief_path_Param)],
	f_u_me_belief_path_c63(u_belief_path, IFTEST),
	(   IFTEST\==[]
	->  ElseResult25=Ob_Get
	;   get_var(Env, u_else, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_u_any(
			[ lambda,
			  [u_belief_bd],
			  
			  [ if,
			    
			    [ u_eq_c63,
			      
			      [ u_absolute_c62_relative,
				[car, u_belief_bd],
				u_belief_path
			      ],
			      u_ob
			    ],
			    [car, u_belief_bd],
			    []
			  ]
			],
			
			[ u_cx_c36_retrieve,
			  u_self,
			  
			  [ u_relative_c62_absolute,
			    u_xx_any_pattern_xx,
			    u_belief_path
			  ]
			],
			TrueResult),
		ElseResult25=TrueResult
	    ;   ElseResult25=[]
	    )
	).
:- set_opv(f_u_cx_c36_intern_relative, classof, claz_function),
   set_opv(u_cx_c36_intern_relative, compile_as, kw_function),
   set_opv(u_cx_c36_intern_relative, function, f_u_cx_c36_intern_relative),
   DefunResult=u_cx_c36_intern_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:20663 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$hyper-retract-relative',
			    [self, ob, 'belief-path'],
			    
			    [ walkcdr,
			      
			      [ lambda,
				[bp],
				
				[ 'cx$retract',
				  self,
				  ['relative->absolute', ob, bp]
				]
			      ],
			      'belief-path'
			    ]
			  ]).

% annotating U::CX$HYPER-RETRACT-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_hyper_retract_relative,
	      f_u_cx_c36_hyper_retract_relative,
	      [u_self, u_ob, u_belief_path],
	      
	      [ 
		[ u_walkcdr,
		  
		  [ lambda,
		    [u_bp],
		    
		    [ u_cx_c36_retract,
		      u_self,
		      [u_relative_c62_absolute, u_ob, u_bp]
		    ]
		  ],
		  u_belief_path
		]
	      ]).


% annotating U::CX$HYPER-RETRACT-RELATIVE 
wl: arglist_info(u_cx_c36_hyper_retract_relative,
		[u_self, u_ob, u_belief_path],
		[Self_Param, Ob_Param, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$HYPER-RETRACT-RELATIVE 
wl: init_args(exact_only, u_cx_c36_hyper_retract_relative).


% annotating U::CX$HYPER-RETRACT-RELATIVE 
f_u_cx_c36_hyper_retract_relative(Self_Param, Ob_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param), bv(u_belief_path, Belief_path_Param)],
	Lambda=closure([Env|Env], LResult, [u_bp],  (get_var(Env, u_bp, Bp_Get), f_u_relative_c62_absolute(Ob_Param, Bp_Get, C62_absolute_Ret), f_u_cx_c36_retract(Self_Param, C62_absolute_Ret, LResult))),
	f_u_walkcdr(Lambda, Belief_path_Param, Walkcdr_Ret),
	Walkcdr_Ret=FnResult.
:- set_opv(f_u_cx_c36_hyper_retract_relative, classof, claz_function),
   set_opv(u_cx_c36_hyper_retract_relative, compile_as, kw_function),
   set_opv(u_cx_c36_hyper_retract_relative,
	   function,
	   f_u_cx_c36_hyper_retract_relative),
   DefunResult=u_cx_c36_hyper_retract_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:20826 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'absolute-bd-list->relative',
			    ['bd-list', 'belief-path'],
			    
			    [ yloop,
			      [initial, [relative, []], [result, []]],
			      [yfor, bd, in, 'bd-list'],
			      
			      [ ydo,
				
				[ setq,
				  relative,
				  
				  [ 'absolute->relative',
				    [car, bd],
				    'belief-path'
				  ]
				],
				
				[ if,
				  relative,
				  
				  [ setq,
				    result,
				    [cons, [cons, relative, [cdr, bd]], result]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::ABSOLUTE-BD-LIST->RELATIVE 
wl: lambda_def(defun,
	      u_absolute_bd_list_c62_relative,
	      f_u_absolute_bd_list_c62_relative,
	      [u_bd_list, u_belief_path],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_relative, []], [u_result, []]],
		  [u_yfor, u_bd, u_in, u_bd_list],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_relative,
		      [u_absolute_c62_relative, [car, u_bd], u_belief_path]
		    ],
		    
		    [ if,
		      u_relative,
		      
		      [ setq,
			u_result,
			[cons, [cons, u_relative, [cdr, u_bd]], u_result]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::ABSOLUTE-BD-LIST->RELATIVE 
wl: arglist_info(u_absolute_bd_list_c62_relative,
		[u_bd_list, u_belief_path],
		[Bd_list_Param, Belief_path_Param],
		arginfo{ all:[u_bd_list, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd_list, u_belief_path],
			 opt:0,
			 req:[u_bd_list, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ABSOLUTE-BD-LIST->RELATIVE 
wl: init_args(exact_only, u_absolute_bd_list_c62_relative).


% annotating U::ABSOLUTE-BD-LIST->RELATIVE 
f_u_absolute_bd_list_c62_relative(Bd_list_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_bd_list, Bd_list_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_yloop(
		  [ [u_initial, [u_relative, []], [u_result, []]],
		    [u_yfor, u_bd, u_in, u_bd_list],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_relative,
			[u_absolute_c62_relative, [car, u_bd], u_belief_path]
		      ],
		      
		      [ if,
			u_relative,
			
			[ setq,
			  u_result,
			  [cons, [cons, u_relative, [cdr, u_bd]], u_result]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_absolute_bd_list_c62_relative, classof, claz_function),
   set_opv(u_absolute_bd_list_c62_relative, compile_as, kw_function),
   set_opv(u_absolute_bd_list_c62_relative,
	   function,
	   f_u_absolute_bd_list_c62_relative),
   DefunResult=u_absolute_bd_list_c62_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:20826 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" actually, relative should always be true?",
				     11,
				     21153)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:21224 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$assert-relative',
			    [self, ob, 'belief-path'],
			    
			    [ 'cx$assert',
			      self,
			      ['relative->absolute', ob, 'belief-path']
			    ]
			  ]).

% annotating U::CX$ASSERT-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_assert_relative,
	      f_u_cx_c36_assert_relative,
	      [u_self, u_ob, u_belief_path],
	      
	      [ 
		[ u_cx_c36_assert,
		  u_self,
		  [u_relative_c62_absolute, u_ob, u_belief_path]
		]
	      ]).


% annotating U::CX$ASSERT-RELATIVE 
wl: arglist_info(u_cx_c36_assert_relative,
		[u_self, u_ob, u_belief_path],
		[Self_Param, Ob_Param, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$ASSERT-RELATIVE 
wl: init_args(exact_only, u_cx_c36_assert_relative).


% annotating U::CX$ASSERT-RELATIVE 
f_u_cx_c36_assert_relative(Self_Param, Ob_Param, Belief_path_Param, FnResult) :-
	f_u_relative_c62_absolute(Ob_Param, Belief_path_Param, C62_absolute_Ret),
	f_u_cx_c36_assert(Self_Param, C62_absolute_Ret, C36_assert_Ret),
	C36_assert_Ret=FnResult.
:- set_opv(f_u_cx_c36_assert_relative, classof, claz_function),
   set_opv(u_cx_c36_assert_relative, compile_as, kw_function),
   set_opv(u_cx_c36_assert_relative, function, f_u_cx_c36_assert_relative),
   DefunResult=u_cx_c36_assert_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:21329 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'cx$hyper-assert-relative',
			    [self, ob, 'belief-path'],
			    
			    [ walkcdr,
			      
			      [ lambda,
				[bp],
				
				[ 'cx$assert',
				  self,
				  ['relative->absolute', ob, bp]
				]
			      ],
			      'belief-path'
			    ]
			  ]).

% annotating U::CX$HYPER-ASSERT-RELATIVE 
wl: lambda_def(defun,
	      u_cx_c36_hyper_assert_relative,
	      f_u_cx_c36_hyper_assert_relative,
	      [u_self, u_ob, u_belief_path],
	      
	      [ 
		[ u_walkcdr,
		  
		  [ lambda,
		    [u_bp],
		    
		    [ u_cx_c36_assert,
		      u_self,
		      [u_relative_c62_absolute, u_ob, u_bp]
		    ]
		  ],
		  u_belief_path
		]
	      ]).


% annotating U::CX$HYPER-ASSERT-RELATIVE 
wl: arglist_info(u_cx_c36_hyper_assert_relative,
		[u_self, u_ob, u_belief_path],
		[Self_Param, Ob_Param, Belief_path_Param],
		arginfo{ all:[u_self, u_ob, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_ob, u_belief_path],
			 opt:0,
			 req:[u_self, u_ob, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CX$HYPER-ASSERT-RELATIVE 
wl: init_args(exact_only, u_cx_c36_hyper_assert_relative).


% annotating U::CX$HYPER-ASSERT-RELATIVE 
f_u_cx_c36_hyper_assert_relative(Self_Param, Ob_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_ob, Ob_Param), bv(u_belief_path, Belief_path_Param)],
	Lambda=closure([Env|Env], LResult, [u_bp],  (get_var(Env, u_bp, Bp_Get), f_u_relative_c62_absolute(Ob_Param, Bp_Get, C62_absolute_Ret), f_u_cx_c36_assert(Self_Param, C62_absolute_Ret, LResult))),
	f_u_walkcdr(Lambda, Belief_path_Param, Walkcdr_Ret),
	Walkcdr_Ret=FnResult.
:- set_opv(f_u_cx_c36_hyper_assert_relative, classof, claz_function),
   set_opv(u_cx_c36_hyper_assert_relative, compile_as, kw_function),
   set_opv(u_cx_c36_hyper_assert_relative,
	   function,
	   f_u_cx_c36_hyper_assert_relative),
   DefunResult=u_cx_c36_hyper_assert_relative.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:21490 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'imaginary?',
			    ['top-level-goal'],
			    
			    [ 'eq?',
			      [quote, imaginary],
			      
			      [ 'ob$get',
				'top-level-goal',
				[quote, 'planning-type']
			      ]
			    ]
			  ]).

% annotating U::IMAGINARY? 
wl: lambda_def(defun,
	      u_imaginary_c63,
	      f_u_imaginary_c63,
	      [u_top_level_goal],
	      
	      [ 
		[ u_eq_c63,
		  [quote, u_imaginary],
		  [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		]
	      ]).


% annotating U::IMAGINARY? 
wl: arglist_info(u_imaginary_c63,
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

% annotating U::IMAGINARY? 
wl: init_args(exact_only, u_imaginary_c63).


% annotating U::IMAGINARY? 
f_u_imaginary_c63(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_imaginary_c63, classof, claz_function),
   set_opv(u_imaginary_c63, compile_as, kw_function),
   set_opv(u_imaginary_c63, function, f_u_imaginary_c63),
   DefunResult=u_imaginary_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:21585 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'real?',
			    ['top-level-goal'],
			    
			    [ 'eq?',
			      [quote, real],
			      
			      [ 'ob$get',
				'top-level-goal',
				[quote, 'planning-type']
			      ]
			    ]
			  ]).

% annotating U::REAL? 
wl: lambda_def(defun,
	      u_real_c63,
	      f_u_real_c63,
	      [u_top_level_goal],
	      
	      [ 
		[ u_eq_c63,
		  [quote, real],
		  [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		]
	      ]).


% annotating U::REAL? 
wl: arglist_info(u_real_c63,
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

% annotating U::REAL? 
wl: init_args(exact_only, u_real_c63).


% annotating U::REAL? 
f_u_real_c63(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_eq_c63([quote, real],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   Eq_c63_Ret),
	Eq_c63_Ret=FnResult.
:- set_opv(f_u_real_c63, classof, claz_function),
   set_opv(u_real_c63, compile_as, kw_function),
   set_opv(u_real_c63, function, f_u_real_c63),
   DefunResult=u_real_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:21670 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'imaginary-realistic?',
			    ['top-level-goal'],
			    
			    [ and,
			      
			      [ 'eq?',
				[quote, imaginary],
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'planning-type']
				]
			      ],
			      
			      [ 'ty$instance?',
				['ob$get', 'top-level-goal', [quote, obj]],
				[quote, 'realistic-goal-obj']
			      ]
			    ]
			  ]).

% annotating U::IMAGINARY-REALISTIC? 
wl: lambda_def(defun,
	      u_imaginary_realistic_c63,
	      f_u_imaginary_realistic_c63,
	      [u_top_level_goal],
	      
	      [ 
		[ and,
		  
		  [ u_eq_c63,
		    [quote, u_imaginary],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		  ],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
		    [quote, u_realistic_goal_obj]
		  ]
		]
	      ]).


% annotating U::IMAGINARY-REALISTIC? 
wl: arglist_info(u_imaginary_realistic_c63,
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

% annotating U::IMAGINARY-REALISTIC? 
wl: init_args(exact_only, u_imaginary_realistic_c63).


% annotating U::IMAGINARY-REALISTIC? 
f_u_imaginary_realistic_c63(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Top_level_goal_Param, u_obj, Obj),
	    f_u_ty_c36_instance_c63(Obj, u_realistic_goal_obj, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_imaginary_realistic_c63, classof, claz_function),
   set_opv(u_imaginary_realistic_c63, compile_as, kw_function),
   set_opv(u_imaginary_realistic_c63, function, f_u_imaginary_realistic_c63),
   DefunResult=u_imaginary_realistic_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:21873 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'imaginary-fanciful?',
			    ['top-level-goal'],
			    
			    [ and,
			      
			      [ 'eq?',
				[quote, imaginary],
				
				[ 'ob$get',
				  'top-level-goal',
				  [quote, 'planning-type']
				]
			      ],
			      
			      [ 'ty$instance?',
				['ob$get', 'top-level-goal', [quote, obj]],
				[quote, 'fanciful-goal-obj']
			      ]
			    ]
			  ]).

% annotating U::IMAGINARY-FANCIFUL? 
wl: lambda_def(defun,
	      u_imaginary_fanciful_c63,
	      f_u_imaginary_fanciful_c63,
	      [u_top_level_goal],
	      
	      [ 
		[ and,
		  
		  [ u_eq_c63,
		    [quote, u_imaginary],
		    [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]]
		  ],
		  
		  [ u_ty_c36_instance_c63,
		    [u_ob_c36_get, u_top_level_goal, [quote, u_obj]],
		    [quote, u_fanciful_goal_obj]
		  ]
		]
	      ]).


% annotating U::IMAGINARY-FANCIFUL? 
wl: arglist_info(u_imaginary_fanciful_c63,
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

% annotating U::IMAGINARY-FANCIFUL? 
wl: init_args(exact_only, u_imaginary_fanciful_c63).


% annotating U::IMAGINARY-FANCIFUL? 
f_u_imaginary_fanciful_c63(Top_level_goal_Param, FnResult) :-
	Env=[bv(u_top_level_goal, Top_level_goal_Param)],
	f_u_eq_c63([quote, u_imaginary],
		   [u_ob_c36_get, u_top_level_goal, [quote, u_planning_type]],
		   IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Top_level_goal_Param, u_obj, Obj),
	    f_u_ty_c36_instance_c63(Obj, u_fanciful_goal_obj, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_imaginary_fanciful_c63, classof, claz_function),
   set_opv(u_imaginary_fanciful_c63, compile_as, kw_function),
   set_opv(u_imaginary_fanciful_c63, function, f_u_imaginary_fanciful_c63),
   DefunResult=u_imaginary_fanciful_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:22074 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'realistic?',
			    ['top-level-goal'],
			    
			    [ or,
			      ['real?', 'top-level-goal'],
			      ['imaginary-realistic?', 'top-level-goal']
			    ]
			  ]).

% annotating U::REALISTIC? 
wl: lambda_def(defun,
	      u_realistic_c63,
	      f_u_realistic_c63,
	      [u_top_level_goal],
	      
	      [ 
		[ or,
		  [u_real_c63, u_top_level_goal],
		  [u_imaginary_realistic_c63, u_top_level_goal]
		]
	      ]).


% annotating U::REALISTIC? 
wl: arglist_info(u_realistic_c63,
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

% annotating U::REALISTIC? 
wl: init_args(exact_only, u_realistic_c63).


% annotating U::REALISTIC? 
f_u_realistic_c63(Top_level_goal_Param, FnResult) :-
	(   f_u_real_c63(Top_level_goal_Param, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_imaginary_realistic_c63(Top_level_goal_Param,
					Realistic_c63_Ret),
	    FnResult=Realistic_c63_Ret
	).
:- set_opv(f_u_realistic_c63, classof, claz_function),
   set_opv(u_realistic_c63, compile_as, kw_function),
   set_opv(u_realistic_c63, function, f_u_realistic_c63),
   DefunResult=u_realistic_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:22185 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'top-goal?',
			    [goal, context, 'belief-path'],
			    
			    [ 'null?',
			      
			      [ 'get-links-from-relative',
				goal,
				'*intends-ob*',
				context,
				'belief-path'
			      ]
			    ]
			  ]).

% annotating U::TOP-GOAL? 
wl: lambda_def(defun,
	      u_top_goal_c63,
	      f_u_top_goal_c63,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_null_c63,
		  
		  [ u_get_links_from_relative,
		    u_goal,
		    u_xx_intends_ob_xx,
		    u_context,
		    u_belief_path
		  ]
		]
	      ]).


% annotating U::TOP-GOAL? 
wl: arglist_info(u_top_goal_c63,
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

% annotating U::TOP-GOAL? 
wl: init_args(exact_only, u_top_goal_c63).


% annotating U::TOP-GOAL? 
f_u_top_goal_c63(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_null_c63(
		     [ u_get_links_from_relative,
		       u_goal,
		       u_xx_intends_ob_xx,
		       u_context,
		       u_belief_path
		     ],
		     Null_c63_Ret),
	Null_c63_Ret=FnResult.
:- set_opv(f_u_top_goal_c63, classof, claz_function),
   set_opv(u_top_goal_c63, compile_as, kw_function),
   set_opv(u_top_goal_c63, function, f_u_top_goal_c63),
   DefunResult=u_top_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:22305 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-supergoal',
			    [goal, context],
			    
			    [ car,
			      
			      [ 'ol-get',
				goal,
				'*intends-ob*',
				[quote, backward],
				context
			      ]
			    ]
			  ]).

% annotating U::GOAL-SUPERGOAL 
wl: lambda_def(defun,
	      u_goal_supergoal,
	      f_u_goal_supergoal,
	      [u_goal, u_context],
	      
	      [ 
		[ car,
		  
		  [ u_ol_get,
		    u_goal,
		    u_xx_intends_ob_xx,
		    [quote, u_backward],
		    u_context
		  ]
		]
	      ]).


% annotating U::GOAL-SUPERGOAL 
wl: arglist_info(u_goal_supergoal,
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

% annotating U::GOAL-SUPERGOAL 
wl: init_args(exact_only, u_goal_supergoal).


% annotating U::GOAL-SUPERGOAL 
f_u_goal_supergoal(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_ol_get(Goal_Param,
		   Xx_intends_ob_xx_Get,
		   u_backward,
		   Context_Param,
		   Car_Param),
	cl_car(Car_Param, Car_Ret),
	Car_Ret=FnResult.
:- set_opv(f_u_goal_supergoal, classof, claz_function),
   set_opv(u_goal_supergoal, compile_as, kw_function),
   set_opv(u_goal_supergoal, function, f_u_goal_supergoal),
   DefunResult=u_goal_supergoal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:22397 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-supergoals',
			    [goal, context, 'belief-path'],
			    
			    [ if,
			      ['null?', goal],
			      [],
			      
			      [ cons,
				goal,
				
				[ 'goal-supergoals',
				  
				  [ car,
				    
				    [ 'ol-get-relative',
				      goal,
				      '*intends-ob*',
				      [quote, backward],
				      context,
				      'belief-path'
				    ]
				  ],
				  context,
				  'belief-path'
				]
			      ]
			    ]
			  ]).

% annotating U::GOAL-SUPERGOALS 
wl: lambda_def(defun,
	      u_goal_supergoals,
	      f_u_goal_supergoals,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ if,
		  [u_null_c63, u_goal],
		  [],
		  
		  [ cons,
		    u_goal,
		    
		    [ u_goal_supergoals,
		      
		      [ car,
			
			[ u_ol_get_relative,
			  u_goal,
			  u_xx_intends_ob_xx,
			  [quote, u_backward],
			  u_context,
			  u_belief_path
			]
		      ],
		      u_context,
		      u_belief_path
		    ]
		  ]
		]
	      ]).


% annotating U::GOAL-SUPERGOALS 
wl: arglist_info(u_goal_supergoals,
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

% annotating U::GOAL-SUPERGOALS 
wl: init_args(exact_only, u_goal_supergoals).


% annotating U::GOAL-SUPERGOALS 
f_u_goal_supergoals(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_null_c63(u_goal, IFTEST),
	(   IFTEST\==[]
	->  FnResult=[]
	;   get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	    f_u_ol_get_relative(Goal_Param,
				Xx_intends_ob_xx_Get,
				u_backward,
				Context_Param,
				Belief_path_Param,
				Car_Param),
	    cl_car(Car_Param, Goal_supergoals_Param),
	    f_u_goal_supergoals(Goal_supergoals_Param,
				Context_Param,
				Belief_path_Param,
				Goal_supergoals_Ret),
	    FnResult=[Goal_Param|Goal_supergoals_Ret]
	).
:- set_opv(f_u_goal_supergoals, classof, claz_function),
   set_opv(u_goal_supergoals, compile_as, kw_function),
   set_opv(u_goal_supergoals, function, f_u_goal_supergoals),
   DefunResult=u_goal_supergoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:22397 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Returns subgoals in order asserted. (That is, in regular forward order.)",
				     1,
				     22680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:22754 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-subgoals',
			    [goal, context, 'belief-path'],
			    
			    [ sort,
			      
			      [ 'ol-get-relative',
				goal,
				'*intends-ob*',
				[quote, forward],
				context,
				'belief-path'
			      ],
			      
			      [ lambda,
				[subgoal1, subgoal2],
				
				[ (<),
				  
				  [ 'ob$pget',
				    subgoal1,
				    [quote, [obj, 'plan-subgoalnum']]
				  ],
				  
				  [ 'ob$pget',
				    subgoal2,
				    [quote, [obj, 'plan-subgoalnum']]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::GOAL-SUBGOALS 
wl: lambda_def(defun,
	      u_goal_subgoals,
	      f_u_goal_subgoals,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ sort,
		  
		  [ u_ol_get_relative,
		    u_goal,
		    u_xx_intends_ob_xx,
		    [quote, u_forward],
		    u_context,
		    u_belief_path
		  ],
		  
		  [ lambda,
		    [u_subgoal1, u_subgoal2],
		    
		    [ (<),
		      
		      [ u_ob_c36_pget,
			u_subgoal1,
			[quote, [u_obj, u_plan_subgoalnum]]
		      ],
		      
		      [ u_ob_c36_pget,
			u_subgoal2,
			[quote, [u_obj, u_plan_subgoalnum]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::GOAL-SUBGOALS 
wl: arglist_info(u_goal_subgoals,
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

% annotating U::GOAL-SUBGOALS 
wl: init_args(exact_only, u_goal_subgoals).


% annotating U::GOAL-SUBGOALS 
f_u_goal_subgoals(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_ol_get_relative(Goal_Param,
			    Xx_intends_ob_xx_Get,
			    u_forward,
			    Context_Param,
			    Belief_path_Param,
			    Sort_Param),
	Lambda=closure([Env|Env], LResult, [u_subgoal1, u_subgoal2],  (get_var(Env, u_subgoal1, Subgoal1_Get), f_u_ob_c36_pget(Subgoal1_Get, [u_obj, u_plan_subgoalnum], C36_pget_Ret), get_var(Env, u_subgoal2, Subgoal2_Get), f_u_ob_c36_pget(Subgoal2_Get, [u_obj, u_plan_subgoalnum], C36_pget_Ret31), <(C36_pget_Ret, C36_pget_Ret31, LResult))),
	cl_sort(Sort_Param, Lambda, Sort_Ret),
	Sort_Ret=FnResult.
:- set_opv(f_u_goal_subgoals, classof, claz_function),
   set_opv(u_goal_subgoals, compile_as, kw_function),
   set_opv(u_goal_subgoals, function, f_u_goal_subgoals),
   DefunResult=u_goal_subgoals.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23038 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-subgoals-uo',
			    [goal, context, 'belief-path'],
			    
			    [ 'ol-get-relative',
			      goal,
			      '*intends-ob*',
			      [quote, forward],
			      context,
			      'belief-path'
			    ]
			  ]).

% annotating U::GOAL-SUBGOALS-UO 
wl: lambda_def(defun,
	      u_goal_subgoals_uo,
	      f_u_goal_subgoals_uo,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ol_get_relative,
		  u_goal,
		  u_xx_intends_ob_xx,
		  [quote, u_forward],
		  u_context,
		  u_belief_path
		]
	      ]).


% annotating U::GOAL-SUBGOALS-UO 
wl: arglist_info(u_goal_subgoals_uo,
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

% annotating U::GOAL-SUBGOALS-UO 
wl: init_args(exact_only, u_goal_subgoals_uo).


% annotating U::GOAL-SUBGOALS-UO 
f_u_goal_subgoals_uo(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_ol_get_relative(Goal_Param,
			    Xx_intends_ob_xx_Get,
			    u_forward,
			    Context_Param,
			    Belief_path_Param,
			    Get_relative_Ret),
	Get_relative_Ret=FnResult.
:- set_opv(f_u_goal_subgoals_uo, classof, claz_function),
   set_opv(u_goal_subgoals_uo, compile_as, kw_function),
   set_opv(u_goal_subgoals_uo, function, f_u_goal_subgoals_uo),
   DefunResult=u_goal_subgoals_uo.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23158 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-subgoals-rule',
			    [goal, context, 'belief-path'],
			    
			    [ 'ob$get',
			      
			      [ car,
				
				[ 'get-links-relative',
				  goal,
				  '*intends-ob*',
				  context,
				  'belief-path'
				]
			      ],
			      [quote, rule]
			    ]
			  ]).

% annotating U::GOAL-SUBGOALS-RULE 
wl: lambda_def(defun,
	      u_goal_subgoals_rule,
	      f_u_goal_subgoals_rule,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_ob_c36_get,
		  
		  [ car,
		    
		    [ u_get_links_relative,
		      u_goal,
		      u_xx_intends_ob_xx,
		      u_context,
		      u_belief_path
		    ]
		  ],
		  [quote, u_rule]
		]
	      ]).


% annotating U::GOAL-SUBGOALS-RULE 
wl: arglist_info(u_goal_subgoals_rule,
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

% annotating U::GOAL-SUBGOALS-RULE 
wl: init_args(exact_only, u_goal_subgoals_rule).


% annotating U::GOAL-SUBGOALS-RULE 
f_u_goal_subgoals_rule(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_get_links_relative(Goal_Param,
			       Xx_intends_ob_xx_Get,
			       Context_Param,
			       Belief_path_Param,
			       Car_Param),
	cl_car(Car_Param, C36_get_Param),
	f_u_ob_c36_get(C36_get_Param, u_rule, Rule),
	Rule=FnResult.
:- set_opv(f_u_goal_subgoals_rule, classof, claz_function),
   set_opv(u_goal_subgoals_rule, compile_as, kw_function),
   set_opv(u_goal_subgoals_rule, function, f_u_goal_subgoals_rule),
   DefunResult=u_goal_subgoals_rule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23305 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-subgoals-seq?',
			    [goal, context],
			    
			    [ 'ob$get',
			      [car, ['get-links', goal, '*intends-ob*', context]],
			      [quote, 'seq?']
			    ]
			  ]).

% annotating U::GOAL-SUBGOALS-SEQ? 
wl: lambda_def(defun,
	      u_goal_subgoals_seq_c63,
	      f_u_goal_subgoals_seq_c63,
	      [u_goal, u_context],
	      
	      [ 
		[ u_ob_c36_get,
		  [car, [u_get_links, u_goal, u_xx_intends_ob_xx, u_context]],
		  [quote, u_seq_c63]
		]
	      ]).


% annotating U::GOAL-SUBGOALS-SEQ? 
wl: arglist_info(u_goal_subgoals_seq_c63,
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

% annotating U::GOAL-SUBGOALS-SEQ? 
wl: init_args(exact_only, u_goal_subgoals_seq_c63).


% annotating U::GOAL-SUBGOALS-SEQ? 
f_u_goal_subgoals_seq_c63(Goal_Param, Context_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_get_links(Goal_Param, Xx_intends_ob_xx_Get, Context_Param, Car_Param),
	cl_car(Car_Param, C36_get_Param),
	f_u_ob_c36_get(C36_get_Param, u_seq_c63, Seq_c63),
	Seq_c63=FnResult.
:- set_opv(f_u_goal_subgoals_seq_c63, classof, claz_function),
   set_opv(u_goal_subgoals_seq_c63, compile_as, kw_function),
   set_opv(u_goal_subgoals_seq_c63, function, f_u_goal_subgoals_seq_c63),
   DefunResult=u_goal_subgoals_seq_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23419 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'goal-intends-links-uo',
			    [goal, context, 'belief-path'],
			    
			    [ 'get-links-relative',
			      goal,
			      '*intends-ob*',
			      context,
			      'belief-path'
			    ]
			  ]).

% annotating U::GOAL-INTENDS-LINKS-UO 
wl: lambda_def(defun,
	      u_goal_intends_links_uo,
	      f_u_goal_intends_links_uo,
	      [u_goal, u_context, u_belief_path],
	      
	      [ 
		[ u_get_links_relative,
		  u_goal,
		  u_xx_intends_ob_xx,
		  u_context,
		  u_belief_path
		]
	      ]).


% annotating U::GOAL-INTENDS-LINKS-UO 
wl: arglist_info(u_goal_intends_links_uo,
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

% annotating U::GOAL-INTENDS-LINKS-UO 
wl: init_args(exact_only, u_goal_intends_links_uo).


% annotating U::GOAL-INTENDS-LINKS-UO 
f_u_goal_intends_links_uo(Goal_Param, Context_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_goal, Goal_Param), bv(u_context, Context_Param), bv(u_belief_path, Belief_path_Param)],
	get_var(Env, u_xx_intends_ob_xx, Xx_intends_ob_xx_Get),
	f_u_get_links_relative(Goal_Param,
			       Xx_intends_ob_xx_Get,
			       Context_Param,
			       Belief_path_Param,
			       Links_relative_Ret),
	Links_relative_Ret=FnResult.
:- set_opv(f_u_goal_intends_links_uo, classof, claz_function),
   set_opv(u_goal_intends_links_uo, compile_as, kw_function),
   set_opv(u_goal_intends_links_uo, function, f_u_goal_intends_links_uo),
   DefunResult=u_goal_intends_links_uo.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23419 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun goal-subgoals (goal context belief-path)",
				     1,
				     23539)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23419 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (map 'list (lambda (x) (ob$get x 'linked-to))",
				     1,
				     23588)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23419 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("       (get-links-relative goal *intends-ob* context belief-path)))",
				     1,
				     23637)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23706 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'rule-subgoal-objs',
			    [rule],
			    
			    [ let,
			      
			      [ 
				[ 'rule-subgoal',
				  ['ob$get', rule, [quote, subgoal]]
				]
			      ],
			      
			      [ cond,
				
				[ 
				  [ or,
				    
				    [ 'ty$instance?',
				      'rule-subgoal',
				      [quote, rand]
				    ],
				    
				    [ 'ty$instance?',
				      'rule-subgoal',
				      [quote, rseq]
				    ]
				  ],
				  ['ob$gets', 'rule-subgoal', [quote, obj]]
				],
				
				[ ['ty$instance?', 'rule-subgoal', [quote, ror]],
				  
				  [ error,
				    '$STRING'("rule-subgoal-objs: ROR not allowed")
				  ],
				  [list, 'rule-subgoal']
				],
				[else, [list, 'rule-subgoal']]
			      ]
			    ]
			  ]).

% annotating U::RULE-SUBGOAL-OBJS 
wl: lambda_def(defun,
	      u_rule_subgoal_objs,
	      f_u_rule_subgoal_objs,
	      [u_rule],
	      
	      [ 
		[ let,
		  [[u_rule_subgoal, [u_ob_c36_get, u_rule, [quote, u_subgoal]]]],
		  
		  [ cond,
		    
		    [ 
		      [ or,
			[u_ty_c36_instance_c63, u_rule_subgoal, [quote, u_rand]],
			[u_ty_c36_instance_c63, u_rule_subgoal, [quote, u_rseq]]
		      ],
		      [u_ob_c36_gets, u_rule_subgoal, [quote, u_obj]]
		    ],
		    
		    [ [u_ty_c36_instance_c63, u_rule_subgoal, [quote, u_ror]],
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(r),
				   #\(u),
				   #\(l),
				   #\(e),
				   #\(-),
				   #\(s),
				   #\(u),
				   #\(b),
				   #\(g),
				   #\(o),
				   #\(a),
				   #\(l),
				   #\(-),
				   #\(o),
				   #\(b),
				   #\(j),
				   #\(s),
				   #\(:),
				   #\(' '),
				   #\('R'),
				   #\('O'),
				   #\('R'),
				   #\(' '),
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
				 ])
		      ],
		      [list, u_rule_subgoal]
		    ],
		    [u_else, [list, u_rule_subgoal]]
		  ]
		]
	      ]).


% annotating U::RULE-SUBGOAL-OBJS 
wl: arglist_info(u_rule_subgoal_objs,
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

% annotating U::RULE-SUBGOAL-OBJS 
wl: init_args(exact_only, u_rule_subgoal_objs).


% annotating U::RULE-SUBGOAL-OBJS 
f_u_rule_subgoal_objs(Rule_Param, ElseResult34) :-
	Env=[bv(u_rule, Rule_Param)],
	f_u_ob_c36_get(Rule_Param, u_subgoal, Rule_subgoal_Init),
	LEnv=[[bv(u_rule_subgoal, Rule_subgoal_Init)]|Env],
	(   get_var(LEnv, u_rule_subgoal, Rule_subgoal_Get),
	    f_u_ty_c36_instance_c63(Rule_subgoal_Get, u_rand, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   get_var(LEnv, u_rule_subgoal, Rule_subgoal_Get20),
	    f_u_ty_c36_instance_c63(Rule_subgoal_Get20, u_rseq, Rseq),
	    IFTEST=Rseq
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_rule_subgoal, Rule_subgoal_Get22),
	    f_u_ob_c36_gets(Rule_subgoal_Get22, u_obj, TrueResult35),
	    ElseResult34=TrueResult35
	;   get_var(LEnv, u_rule_subgoal, Rule_subgoal_Get25),
	    f_u_ty_c36_instance_c63(Rule_subgoal_Get25, u_ror, IFTEST23),
	    (   IFTEST23\==[]
	    ->  cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\(r),
				      #\(u),
				      #\(l),
				      #\(e),
				      #\(-),
				      #\(s),
				      #\(u),
				      #\(b),
				      #\(g),
				      #\(o),
				      #\(a),
				      #\(l),
				      #\(-),
				      #\(o),
				      #\(b),
				      #\(j),
				      #\(s),
				      #\(:),
				      #\(' '),
				      #\('R'),
				      #\('O'),
				      #\('R'),
				      #\(' '),
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
				    ])
			 ],
			 Error_Ret),
		get_var(LEnv, u_rule_subgoal, Rule_subgoal_Get26),
		ElseResult34=[Rule_subgoal_Get26]
	    ;   get_var(LEnv, u_else, IFTEST27),
		(   IFTEST27\==[]
		->  get_var(LEnv, u_rule_subgoal, Rule_subgoal_Get30),
		    ElseResult34=[Rule_subgoal_Get30]
		;   ElseResult34=[]
		)
	    )
	).
:- set_opv(f_u_rule_subgoal_objs, classof, claz_function),
   set_opv(u_rule_subgoal_objs, compile_as, kw_function),
   set_opv(u_rule_subgoal_objs, function, f_u_rule_subgoal_objs),
   DefunResult=u_rule_subgoal_objs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23706 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Set the below always to return T if you always want analogical",
				     1,
				     24097)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:23706 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" filling in of details.", 1, 24162)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24186 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'analogy-instantiatible?',
			    [sval],
			    
			    [ or,
			      ['ty$instance?', sval, [quote, building]],
			      ['ty$instance?', sval, [quote, location]],
			      ['ty$instance?', sval, [quote, organization]]
			    ]
			  ]).

% annotating U::ANALOGY-INSTANTIATIBLE? 
wl: lambda_def(defun,
	      u_analogy_instantiatible_c63,
	      f_u_analogy_instantiatible_c63,
	      [u_sval],
	      
	      [ 
		[ or,
		  [u_ty_c36_instance_c63, u_sval, [quote, u_building]],
		  [u_ty_c36_instance_c63, u_sval, [quote, u_location]],
		  [u_ty_c36_instance_c63, u_sval, [quote, u_organization]]
		]
	      ]).


% annotating U::ANALOGY-INSTANTIATIBLE? 
wl: arglist_info(u_analogy_instantiatible_c63,
		[u_sval],
		[Sval_Param],
		arginfo{ all:[u_sval],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_sval],
			 opt:0,
			 req:[u_sval],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ANALOGY-INSTANTIATIBLE? 
wl: init_args(exact_only, u_analogy_instantiatible_c63).


% annotating U::ANALOGY-INSTANTIATIBLE? 
f_u_analogy_instantiatible_c63(Sval_Param, FnResult) :-
	(   f_u_ty_c36_instance_c63(Sval_Param, u_building, FORM1_Res16),
	    FORM1_Res16\==[],
	    FnResult=FORM1_Res16
	->  true
	;   f_u_ty_c36_instance_c63(Sval_Param, u_location, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_ty_c36_instance_c63(Sval_Param, u_organization, Organization),
	    FnResult=Organization
	).
:- set_opv(f_u_analogy_instantiatible_c63, classof, claz_function),
   set_opv(u_analogy_instantiatible_c63, compile_as, kw_function),
   set_opv(u_analogy_instantiatible_c63,
	   function,
	   f_u_analogy_instantiatible_c63),
   DefunResult=u_analogy_instantiatible_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'analogy-instantiatible1?',
			    [sval, 'seren-ep'],
			    
			    [ or,
			      ['ty$instance?', sval, [quote, building]],
			      ['ty$instance?', sval, [quote, location]],
			      ['ty$instance?', sval, [quote, organization]],
			      
			      [ and,
				
				[ or,
				  ['ty$instance?', sval, [quote, person]],
				  ['ty$instance?', sval, [quote, 'phys-obj']]
				],
				'seren-ep'
			      ]
			    ]
			  ]).

% annotating U::ANALOGY-INSTANTIATIBLE1? 
wl: lambda_def(defun,
	      u_analogy_instantiatible1_c63,
	      f_u_analogy_instantiatible1_c63,
	      [u_sval, u_seren_ep],
	      
	      [ 
		[ or,
		  [u_ty_c36_instance_c63, u_sval, [quote, u_building]],
		  [u_ty_c36_instance_c63, u_sval, [quote, u_location]],
		  [u_ty_c36_instance_c63, u_sval, [quote, u_organization]],
		  
		  [ and,
		    
		    [ or,
		      [u_ty_c36_instance_c63, u_sval, [quote, u_person]],
		      [u_ty_c36_instance_c63, u_sval, [quote, u_phys_obj]]
		    ],
		    u_seren_ep
		  ]
		]
	      ]).


% annotating U::ANALOGY-INSTANTIATIBLE1? 
wl: arglist_info(u_analogy_instantiatible1_c63,
		[u_sval, u_seren_ep],
		[Sval_Param, Seren_ep_Param],
		arginfo{ all:[u_sval, u_seren_ep],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_sval, u_seren_ep],
			 opt:0,
			 req:[u_sval, u_seren_ep],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ANALOGY-INSTANTIATIBLE1? 
wl: init_args(exact_only, u_analogy_instantiatible1_c63).


% annotating U::ANALOGY-INSTANTIATIBLE1? 
f_u_analogy_instantiatible1_c63(Sval_Param, Seren_ep_Param, FnResult) :-
	(   f_u_ty_c36_instance_c63(Sval_Param, u_building, FORM1_Res26),
	    FORM1_Res26\==[],
	    FnResult=FORM1_Res26
	->  true
	;   f_u_ty_c36_instance_c63(Sval_Param, u_location, FORM1_Res25),
	    FORM1_Res25\==[],
	    FnResult=FORM1_Res25
	->  true
	;   f_u_ty_c36_instance_c63(Sval_Param, u_organization, FORM1_Res24),
	    FORM1_Res24\==[],
	    FnResult=FORM1_Res24
	->  true
	;   (   f_u_ty_c36_instance_c63(Sval_Param, u_person, FORM1_Res),
		FORM1_Res\==[],
		IFTEST=FORM1_Res
	    ->  true
	    ;   f_u_ty_c36_instance_c63(Sval_Param, u_phys_obj, Phys_obj),
		IFTEST=Phys_obj
	    ),
	    (   IFTEST\==[]
	    ->  FnResult=Seren_ep_Param
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_analogy_instantiatible1_c63, classof, claz_function),
   set_opv(u_analogy_instantiatible1_c63, compile_as, kw_function),
   set_opv(u_analogy_instantiatible1_c63,
	   function,
	   f_u_analogy_instantiatible1_c63),
   DefunResult=u_analogy_instantiatible1_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Note this alg isn't exactly the one given in notes p. 43/7.",
				     1,
				     24613)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" plan-instantiate should be optimized not to perform an instantiation",
				     1,
				     24675)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" and replace if there are no variables inside; even better--if there are",
				     1,
				     24746)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" not any variables given values in bd inside. The former was done, but",
				     1,
				     24820)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" not the latter.", 1, 24892)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 24910)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - Modified the below to batch to a single plan-instantiate.",
				     1,
				     24912)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" - vars-ok? is currently used from rip.",
				     1,
				     24974)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:24339 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 25015)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:25016 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bd-special-append',
			    
			    [ 'target-bd',
			      'source-bd',
			      'target-goal',
			      'target-context',
			      'belief-path',
			      'top-level-goal',
			      'vars-ok?',
			      'ok-proc'
			    ],
			    
			    [ let,
			      
			      [ [val, []],
				[bd2, ['bd-append', 'target-bd', 'source-bd']],
				['accum-bds', '*empty-bd*'],
				[sval, []]
			      ],
			      
			      [ 'bd-walk',
				
				[ lambda,
				  [var],
				  [setq, val, ['bd-hyper-lookup', var, bd2]],
				  
				  [ if,
				    ['var?', val],
				    
				    [ progn,
				      
				      [ setq,
					sval,
					
					[ 'bd-lookup',
					  ['variable-name', val],
					  'source-bd'
					]
				      ],
				      
				      [ if,
					
					[ and,
					  sval,
					  [apply, 'ok-proc', [list, sval]],
					  [or, 'vars-ok?', [not, ['var?', sval]]]
					],
					
					[ progn,
					  
					  [ setq,
					    'accum-bds',
					    ['bd-bind', var, sval, 'accum-bds']
					  ],
					  
					  [ setq,
					    'target-bd',
					    
					    [ 'bd-bind',
					      ['variable-name', val],
					      sval,
					      'target-bd'
					    ]
					  ]
					]
				      ]
				    ]
				  ]
				],
				'target-bd'
			      ],
			      
			      [ if,
				['neq?', 'accum-bds', '*empty-bd*'],
				
				[ progn,
				  
				  [ setq,
				    'target-goal',
				    
				    [ 'plan-instantiate',
				      'target-goal',
				      'accum-bds',
				      'target-context',
				      'top-level-goal',
				      'belief-path',
				      []
				    ]
				  ],
				  
				  [ if,
				    ['ob?', 'target-goal'],
				    
				    [ cons,
				      'target-goal',
				      
				      [ cdr,
					
					[ 'bd-append-ai',
					  'target-bd',
					  'source-bd'
					]
				      ]
				    ],
				    ['bd-append-ai', 'target-bd', 'source-bd']
				  ]
				],
				['bd-append-ai', 'target-bd', 'source-bd']
			      ]
			    ]
			  ]).

% annotating U::BD-SPECIAL-APPEND 
wl: lambda_def(defun,
	      u_bd_special_append,
	      f_u_bd_special_append,
	      
	      [ u_target_bd,
		u_source_bd,
		u_target_goal,
		u_target_context,
		u_belief_path,
		u_top_level_goal,
		u_vars_ok_c63,
		u_ok_proc
	      ],
	      
	      [ 
		[ let,
		  
		  [ [u_val, []],
		    [u_bd2, [u_bd_append, u_target_bd, u_source_bd]],
		    [u_accum_bds, u_xx_empty_bd_xx],
		    [u_sval, []]
		  ],
		  
		  [ u_bd_walk,
		    
		    [ lambda,
		      [u_var],
		      [setq, u_val, [u_bd_hyper_lookup, u_var, u_bd2]],
		      
		      [ if,
			[u_var_c63, u_val],
			
			[ progn,
			  
			  [ setq,
			    u_sval,
			    [u_bd_lookup, [u_variable_name, u_val], u_source_bd]
			  ],
			  
			  [ if,
			    
			    [ and,
			      u_sval,
			      [apply, u_ok_proc, [list, u_sval]],
			      [or, u_vars_ok_c63, [not, [u_var_c63, u_sval]]]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_accum_bds,
				[u_bd_bind, u_var, u_sval, u_accum_bds]
			      ],
			      
			      [ setq,
				u_target_bd,
				
				[ u_bd_bind,
				  [u_variable_name, u_val],
				  u_sval,
				  u_target_bd
				]
			      ]
			    ]
			  ]
			]
		      ]
		    ],
		    u_target_bd
		  ],
		  
		  [ if,
		    [u_neq_c63, u_accum_bds, u_xx_empty_bd_xx],
		    
		    [ progn,
		      
		      [ setq,
			u_target_goal,
			
			[ u_plan_instantiate,
			  u_target_goal,
			  u_accum_bds,
			  u_target_context,
			  u_top_level_goal,
			  u_belief_path,
			  []
			]
		      ],
		      
		      [ if,
			[u_ob_c63, u_target_goal],
			
			[ cons,
			  u_target_goal,
			  [cdr, [u_bd_append_ai, u_target_bd, u_source_bd]]
			],
			[u_bd_append_ai, u_target_bd, u_source_bd]
		      ]
		    ],
		    [u_bd_append_ai, u_target_bd, u_source_bd]
		  ]
		]
	      ]).


% annotating U::BD-SPECIAL-APPEND 
wl: arglist_info(u_bd_special_append,
		
		[ u_target_bd,
		  u_source_bd,
		  u_target_goal,
		  u_target_context,
		  u_belief_path,
		  u_top_level_goal,
		  u_vars_ok_c63,
		  u_ok_proc
		],
		
		[ Target_bd_Param,
		  Source_bd_Param,
		  Target_goal_Param,
		  Target_context_Param,
		  Belief_path_Param,
		  Top_level_goal_Param,
		  Vars_ok_c63_Param,
		  Ok_proc_Param
		],
		arginfo{ all:
			     [ u_target_bd,
			       u_source_bd,
			       u_target_goal,
			       u_target_context,
			       u_belief_path,
			       u_top_level_goal,
			       u_vars_ok_c63,
			       u_ok_proc
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_target_bd,
				 u_source_bd,
				 u_target_goal,
				 u_target_context,
				 u_belief_path,
				 u_top_level_goal,
				 u_vars_ok_c63,
				 u_ok_proc
			       ],
			 opt:0,
			 req:
			     [ u_target_bd,
			       u_source_bd,
			       u_target_goal,
			       u_target_context,
			       u_belief_path,
			       u_top_level_goal,
			       u_vars_ok_c63,
			       u_ok_proc
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-SPECIAL-APPEND 
wl: init_args(exact_only, u_bd_special_append).


% annotating U::BD-SPECIAL-APPEND 
f_u_bd_special_append(Target_bd_Param, Source_bd_Param, Target_goal_Param, Target_context_Param, Belief_path_Param, Top_level_goal_Param, Vars_ok_c63_Param, Ok_proc_Param, TrueResult67) :-
	Env=[bv(u_target_bd, Target_bd_Param), bv(u_source_bd, Source_bd_Param), bv(u_target_goal, Target_goal_Param), bv(u_target_context, Target_context_Param), bv(u_belief_path, Belief_path_Param), bv(u_top_level_goal, Top_level_goal_Param), bv(u_vars_ok_c63, Vars_ok_c63_Param), bv(u_ok_proc, Ok_proc_Param)],
	f_u_bd_append(u_target_bd, u_source_bd, Bd2_Init),
	get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get),
	LEnv=[[bv(u_val, []), bv(u_bd2, Bd2_Init), bv(u_accum_bds, Xx_empty_bd_xx_Get), bv(u_sval, [])]|Env],
	Lambda=closure([Env|LEnv], LResult, [u_var],  (f_u_bd_hyper_lookup(u_var, u_bd2, Bd2), set_var(Env, u_val, Bd2), f_u_var_c63(u_val, IFTEST), (IFTEST\==[]->f_u_bd_lookup([u_variable_name, u_val], u_source_bd, Source_bd), set_var(Env, u_sval, Source_bd), get_var(Env, u_sval, IFTEST37), (IFTEST37\==[]->get_var(Env, list, List_Get), f_u_ok_proc(List_Get, IFTEST37, IFTEST40), (IFTEST40\==[]->(Vars_ok_c63_Param\==[], TrueResult=Vars_ok_c63_Param->true;f_u_var_c63(u_sval, Not_Param), cl_not(Not_Param, Not_Ret), TrueResult=Not_Ret), TrueResult47=TrueResult;TrueResult47=[]), IFTEST35=TrueResult47;IFTEST35=[]), (IFTEST35\==[]->f_u_bd_bind(u_var, u_sval, u_accum_bds, Accum_bds), set_var(Env, u_accum_bds, Accum_bds), f_u_bd_bind([u_variable_name, u_val], u_sval, u_target_bd, TrueResult48), set_var(Env, u_target_bd, TrueResult48), TrueResult49=TrueResult48;TrueResult49=[]), LResult=TrueResult49;LResult=[]))),
	get_var(LEnv, u_target_bd, Target_bd_Get),
	f_u_bd_walk(Lambda, Target_bd_Get, Bd_walk_Ret),
	f_u_neq_c63(u_accum_bds, u_xx_empty_bd_xx, IFTEST55),
	(   IFTEST55\==[]
	->  get_var(LEnv, u_accum_bds, Accum_bds_Get),
	    get_var(LEnv, u_target_goal, Target_goal_Get),
	    f_u_plan_instantiate(Target_goal_Get,
				 Accum_bds_Get,
				 Target_context_Param,
				 Top_level_goal_Param,
				 Belief_path_Param,
				 [],
				 Target_goal),
	    set_var(LEnv, u_target_goal, Target_goal),
	    f_u_ob_c63(u_target_goal, IFTEST62),
	    (   IFTEST62\==[]
	    ->  get_var(LEnv, u_target_goal, Target_goal_Get64),
		f_u_bd_append_ai(u_target_bd, u_source_bd, Source_bd75),
		cl_cdr(Source_bd75, Cdr_Ret),
		TrueResult67=[Target_goal_Get64|Cdr_Ret]
	    ;   f_u_bd_append_ai(u_target_bd, u_source_bd, ElseResult),
		TrueResult67=ElseResult
	    )
	;   f_u_bd_append_ai(u_target_bd, u_source_bd, ElseResult68),
	    TrueResult67=ElseResult68
	).
:- set_opv(f_u_bd_special_append, classof, claz_function),
   set_opv(u_bd_special_append, compile_as, kw_function),
   set_opv(u_bd_special_append, function, f_u_bd_special_append),
   DefunResult=u_bd_special_append.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:25016 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: don't add var if it came from the rule only.",
				     20,
				     25652)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:25016 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was                   (setq target-bd (bd-unbind var target-bd))",
				     1,
				     25770)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:26443 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bd-walk',
			    [proc, bd],
			    
			    [ map,
			      [quote, list],
			      [lambda, [pair], [funcall, proc, [car, pair]]],
			      [cdr, bd]
			    ]
			  ]).

% annotating U::BD-WALK 
wl: lambda_def(defun,
	      u_bd_walk,
	      f_u_bd_walk,
	      [u_proc, u_bd],
	      
	      [ 
		[ map,
		  [quote, list],
		  [lambda, [u_pair], [funcall, u_proc, [car, u_pair]]],
		  [cdr, u_bd]
		]
	      ]).


% annotating U::BD-WALK 
wl: arglist_info(u_bd_walk,
		[u_proc, u_bd],
		[Proc_Param, Bd_Param],
		arginfo{ all:[u_proc, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_proc, u_bd],
			 opt:0,
			 req:[u_proc, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-WALK 
wl: init_args(exact_only, u_bd_walk).


% annotating U::BD-WALK 
f_u_bd_walk(Proc_Param, Bd_Param, FnResult) :-
	Env=[bv(u_proc, Proc_Param), bv(u_bd, Bd_Param)],
	Lambda=closure([Env15|Env], LResult, [u_pair],  (get_var(Env15, u_pair, Pair_Get), cl_car(Pair_Get, Proc_Param22), f_u_proc(Proc_Param22, LResult))),
	cl_cdr(Bd_Param, Cdr_Ret),
	cl_map(list, Lambda, Cdr_Ret, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_bd_walk, classof, claz_function),
   set_opv(u_bd_walk, compile_as, kw_function),
   set_opv(u_bd_walk, function, f_u_bd_walk),
   DefunResult=u_bd_walk.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:26548 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bd-unbind',
			    [var, bd],
			    
			    [ cons,
			      [car, bd],
			      
			      [ del,
				[lambda, [x, y], ['eq?', [car, y], x]],
				var,
				[cdr, bd]
			      ]
			    ]
			  ]).

% annotating U::BD-UNBIND 
wl: lambda_def(defun,
	      u_bd_unbind,
	      f_u_bd_unbind,
	      [u_var, u_bd],
	      
	      [ 
		[ cons,
		  [car, u_bd],
		  
		  [ u_del,
		    [lambda, [u_x, u_y], [u_eq_c63, [car, u_y], u_x]],
		    u_var,
		    [cdr, u_bd]
		  ]
		]
	      ]).


% annotating U::BD-UNBIND 
wl: arglist_info(u_bd_unbind,
		[u_var, u_bd],
		[Var_Param, Bd_Param],
		arginfo{ all:[u_var, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var, u_bd],
			 opt:0,
			 req:[u_var, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-UNBIND 
wl: init_args(exact_only, u_bd_unbind).


% annotating U::BD-UNBIND 
f_u_bd_unbind(Var_Param, Bd_Param, FnResult) :-
	Env=[bv(u_var, Var_Param), bv(u_bd, Bd_Param)],
	cl_car(Bd_Param, Car_Ret),
	Lambda=closure([ClosureEnvironment|Env], LResult, [u_x, u_y], f_u_eq_c63([car, u_y], u_x, LResult)),
	cl_cdr(Bd_Param, Cdr_Ret),
	f_u_del(Lambda, Var_Param, Cdr_Ret, Del_Ret),
	_101950=[Car_Ret|Del_Ret],
	_101950=FnResult.
:- set_opv(f_u_bd_unbind, classof, claz_function),
   set_opv(u_bd_unbind, compile_as, kw_function),
   set_opv(u_bd_unbind, function, f_u_bd_unbind),
   DefunResult=u_bd_unbind.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:26665 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-seq',
			    [subgoals],
			    
			    [ yloop,
			      [ywhile, [cdr, subgoals]],
			      
			      [ ydo,
				
				[ 'ob$add',
				  [car, subgoals],
				  [quote, 'seq-next'],
				  [cadr, subgoals]
				],
				[setq, subgoals, [cdr, subgoals]]
			      ]
			    ]
			  ]).

% annotating U::MAKE-SEQ 
wl: lambda_def(defun,
	      u_make_seq,
	      f_u_make_seq,
	      [u_subgoals],
	      
	      [ 
		[ u_yloop,
		  [u_ywhile, [cdr, u_subgoals]],
		  
		  [ u_ydo,
		    
		    [ u_ob_c36_add,
		      [car, u_subgoals],
		      [quote, u_seq_next],
		      [cadr, u_subgoals]
		    ],
		    [setq, u_subgoals, [cdr, u_subgoals]]
		  ]
		]
	      ]).


% annotating U::MAKE-SEQ 
wl: arglist_info(u_make_seq,
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

% annotating U::MAKE-SEQ 
wl: init_args(exact_only, u_make_seq).


% annotating U::MAKE-SEQ 
f_u_make_seq(Subgoals_Param, FnResult) :-
	Env=[bv(u_subgoals, Subgoals_Param)],
	f_u_yloop(
		  [ [u_ywhile, [cdr, u_subgoals]],
		    
		    [ u_ydo,
		      
		      [ u_ob_c36_add,
			[car, u_subgoals],
			[quote, u_seq_next],
			[cadr, u_subgoals]
		      ],
		      [setq, u_subgoals, [cdr, u_subgoals]]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_make_seq, classof, claz_function),
   set_opv(u_make_seq, compile_as, kw_function),
   set_opv(u_make_seq, function, f_u_make_seq),
   DefunResult=u_make_seq.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:26828 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'clear-seq',
			    [subgoals],
			    
			    [ yloop,
			      [yfor, subgoal, in, subgoals],
			      [ydo, ['ob$removes', subgoal, [quote, 'seq-next']]]
			    ]
			  ]).

% annotating U::CLEAR-SEQ 
wl: lambda_def(defun,
	      u_clear_seq,
	      f_u_clear_seq,
	      [u_subgoals],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_subgoal, u_in, u_subgoals],
		  [u_ydo, [u_ob_c36_removes, u_subgoal, [quote, u_seq_next]]]
		]
	      ]).


% annotating U::CLEAR-SEQ 
wl: arglist_info(u_clear_seq,
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

% annotating U::CLEAR-SEQ 
wl: init_args(exact_only, u_clear_seq).


% annotating U::CLEAR-SEQ 
f_u_clear_seq(Subgoals_Param, FnResult) :-
	Env=[bv(u_subgoals, Subgoals_Param)],
	f_u_yloop(
		  [ [u_yfor, u_subgoal, u_in, u_subgoals],
		    [u_ydo, [u_ob_c36_removes, u_subgoal, [quote, u_seq_next]]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_clear_seq, classof, claz_function),
   set_opv(u_clear_seq, compile_as, kw_function),
   set_opv(u_clear_seq, function, f_u_clear_seq),
   DefunResult=u_clear_seq.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:26938 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'var-value',
			    [var],
			    
			    [ 'bd-lookup',
			      ['variable-name', var],
			      '*ob-bindings*'
			    ]
			  ]).

% annotating U::VAR-VALUE 
wl: lambda_def(defun,
	      u_var_value,
	      f_u_var_value,
	      [u_var],
	      [[u_bd_lookup, [u_variable_name, u_var], u_xx_ob_bindings_xx]]).


% annotating U::VAR-VALUE 
wl: arglist_info(u_var_value,
		[u_var],
		[Var_Param],
		arginfo{ all:[u_var],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var],
			 opt:0,
			 req:[u_var],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VAR-VALUE 
wl: init_args(exact_only, u_var_value).


% annotating U::VAR-VALUE 
f_u_var_value(Var_Param, FnResult) :-
	Env=[bv(u_var, Var_Param)],
	f_u_bd_lookup([u_variable_name, u_var],
		      u_xx_ob_bindings_xx,
		      Xx_ob_bindings_xx),
	Xx_ob_bindings_xx=FnResult.
:- set_opv(f_u_var_value, classof, claz_function),
   set_opv(u_var_value, compile_as, kw_function),
   set_opv(u_var_value, function, f_u_var_value),
   DefunResult=u_var_value.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ob-bindings*', []]).
:- set_var(TLEnv3, setq, u_xx_ob_bindings_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 27038)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Result of show:", 1, 27040)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ((bd (from-ob from-ptn-ob)", 1, 27058)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (from-ob from-ptn-ob) ...)",
				     1,
				     27087)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("  ...)", 1, 27121)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 27129)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 27132)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Doesn't do multi-level dependencies, but basically we never need them:",
				     1,
				     27134)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (RAND (RAND a b) c) can be unfolded to (RAND a b c);",
				     1,
				     27207)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (RAND (ROR a b) c) actually isn't a multi-level dependency",
				     1,
				     27262)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (RAND (RNOT (RAND a b)) c) isn't so well-defined",
				     1,
				     27323)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" etc..", 1, 27374)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27011 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 27382)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27383 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    show,
			    [ob, context, bd, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      show,
			      '$STRING'("Show ~A in ~A"),
			      ob,
			      context
			    ],
			    
			    [ let,
			      [[result, [show1, ob, context, bd, 'belief-path']]],
			      
			      [ if,
				result,
				
				[ 'ndbg-roman-nl',
				  '*gate-dbg*',
				  show,
				  '$STRING'("~A shown in ~A"),
				  ob,
				  context
				]
			      ],
			      result
			    ]
			  ]).

% annotating U::SHOW 
wl: lambda_def(defun,
	      u_show,
	      f_u_show,
	      [u_ob, u_context, u_bd, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_show,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('S'),
			     #\(h),
			     #\(o),
			     #\(w),
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
		  [[u_result, [u_show1, u_ob, u_context, u_bd, u_belief_path]]],
		  
		  [ if,
		    u_result,
		    
		    [ u_ndbg_roman_nl,
		      u_xx_gate_dbg_xx,
		      u_show,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('A'),
				 #\(' '),
				 #\(s),
				 #\(h),
				 #\(o),
				 #\(w),
				 #\(n),
				 #\(' '),
				 #\(i),
				 #\(n),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_ob,
		      u_context
		    ]
		  ],
		  u_result
		]
	      ]).


% annotating U::SHOW 
wl: arglist_info(u_show,
		[u_ob, u_context, u_bd, u_belief_path],
		[Ob_Param, Context_Param, Bd_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_context, u_bd, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context, u_bd, u_belief_path],
			 opt:0,
			 req:[u_ob, u_context, u_bd, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SHOW 
wl: init_args(exact_only, u_show).


% annotating U::SHOW 
f_u_show(Ob_Param, Context_Param, Bd_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_context, Context_Param), bv(u_bd, Bd_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_show,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('S'),
				       #\(h),
				       #\(o),
				       #\(w),
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
	f_u_show1(Ob_Param,
		  Context_Param,
		  Bd_Param,
		  Belief_path_Param,
		  Result_Init),
	LEnv=[[bv(u_result, Result_Init)]|Env],
	get_var(LEnv, u_result, IFTEST),
	(   IFTEST\==[]
	->  f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			      u_show,
			      
			      [ '$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(~),
					   #\('A'),
					   #\(' '),
					   #\(s),
					   #\(h),
					   #\(o),
					   #\(w),
					   #\(n),
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
			      TrueResult),
	    _103280=TrueResult
	;   _103280=[]
	),
	get_var(LEnv, u_result, Result_Get30),
	LetResult=Result_Get30,
	LetResult=FnResult.
:- set_opv(f_u_show, classof, claz_function),
   set_opv(u_show, compile_as, kw_function),
   set_opv(u_show, function, f_u_show),
   DefunResult=u_show.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27634 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'ty$create',
			    [quote, 'NOT'],
			    [quote, []],
			    [quote, [prop, [obj], []]]
			  ]).
:- f_u_ty_c36_create(not, [], [u_prop, [u_obj], []], _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27672 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*temp-not-ob*',
			    ['ob$fcreate', [quote, ['NOT']]]
			  ]).
:- f_u_ob_c36_fcreate([quote, [not]], _Ignored),
   set_var(TLEnv3, u_xx_temp_not_ob_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27672 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" So far, only used in minimization.",
				     1,
				     27715)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27751 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'negation-of',
			    [fact],
			    
			    [ if,
			      ['ty$instance?', fact, [quote, not]],
			      ['ob$get', fact, [quote, obj]],
			      
			      [ 'ob$fcreate',
				['#BQ', ['NOT', obj, ['#COMMA', fact]]]
			      ]
			    ]
			  ]).

% annotating U::NEGATION-OF 
wl: lambda_def(defun,
	      u_negation_of,
	      f_u_negation_of,
	      [u_fact],
	      
	      [ 
		[ if,
		  [u_ty_c36_instance_c63, u_fact, [quote, not]],
		  [u_ob_c36_get, u_fact, [quote, u_obj]],
		  [u_ob_c36_fcreate, ['#BQ', [not, u_obj, ['#COMMA', u_fact]]]]
		]
	      ]).


% annotating U::NEGATION-OF 
wl: arglist_info(u_negation_of,
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

% annotating U::NEGATION-OF 
wl: init_args(exact_only, u_negation_of).


% annotating U::NEGATION-OF 
f_u_negation_of(Fact_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param)],
	f_u_ty_c36_instance_c63(Fact_Param, not, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_get(Fact_Param, u_obj, TrueResult),
	    FnResult=TrueResult
	;   f_u_ob_c36_fcreate(['#BQ', [not, u_obj, ['#COMMA', u_fact]]],
			       ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_negation_of, classof, claz_function),
   set_opv(u_negation_of, compile_as, kw_function),
   set_opv(u_negation_of, function, f_u_negation_of),
   DefunResult=u_negation_of.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27872 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setq, '*realistic-show-strength-thresh*', 0.4]).
:- set_var(TLEnv3, setq, u_xx_realistic_show_strength_thresh_xx, 0.4).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    show1,
			    [ob, context, bd, 'belief-path'],
			    
			    [ cond,
			      [['ty$instance?', ob, [quote, rfalse]], []],
			      
			      [ ['ty$instance?', ob, [quote, rtrue]],
				[list, [list, bd, [list, ob, ob]]]
			      ],
			      
			      [ 
				[ or,
				  ['ty$instance?', ob, [quote, rand]],
				  ['ty$instance?', ob, [quote, rseq]]
				],
				
				[ 'show-all',
				  ['ob$gets', ob, [quote, obj]],
				  context,
				  bd,
				  'belief-path'
				]
			      ],
			      
			      [ ['ty$instance?', ob, [quote, rnot]],
				
				[ 'ob$set',
				  '*temp-not-ob*',
				  [quote, obj],
				  ['ob$get', ob, [quote, obj]]
				],
				
				[ or,
				  
				  [ show1,
				    '*temp-not-ob*',
				    context,
				    bd,
				    'belief-path'
				  ],
				  
				  [ if,
				    
				    [ not,
				      
				      [ show,
					['ob$get', ob, [quote, obj]],
					context,
					bd,
					'belief-path'
				      ]
				    ],
				    [list, [list, bd, [list, ob, ob]]],
				    []
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', ob, [quote, ror]],
				
				[ yloop,
				  [initial, ['shown?', []], [result, []]],
				  [yfor, elem, in, ['ob$gets', ob, [quote, obj]]],
				  [yuntil, 'shown?'],
				  
				  [ ydo,
				    
				    [ setq,
				      'shown?',
				      [show, elem, context, bd, 'belief-path']
				    ],
				    
				    [ if,
				      'shown?',
				      
				      [ setq,
					result,
					['append!', result, 'shown?']
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ],
			      
			      [ else,
				
				[ yloop,
				  [initial, [result, []]],
				  
				  [ yfor,
				    retrieved,
				    in,
				    
				    [ 'cx$retrieve-bd-relative-ignore',
				      context,
				      ob,
				      bd,
				      'belief-path',
				      
				      [ quote,
					['linked-from-of', 'linked-to-of']
				      ]
				    ]
				  ],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ or,
					['null?', '*top-level-goal*'],
					
					[ not,
					  ['realistic?', '*top-level-goal*']
					],
					
					[ 'ty$instance?',
					  [car, retrieved],
					  [quote, need]
					],
					
					[ 'ty$instance?',
					  [car, retrieved],
					  [quote, emotion]
					],
					
					[ 'fl>',
					  [strength, [car, retrieved]],
					  '*realistic-show-strength-thresh*'
					]
				      ],
				      
				      [ setq,
					result,
					
					[ cons,
					  
					  [ list,
					    retrieved,
					    [list, [car, retrieved], ob]
					  ],
					  result
					]
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ]
			    ]
			  ]).

% annotating U::SHOW1 
wl: lambda_def(defun,
	      u_show1,
	      f_u_show1,
	      [u_ob, u_context, u_bd, u_belief_path],
	      
	      [ 
		[ cond,
		  [[u_ty_c36_instance_c63, u_ob, [quote, u_rfalse]], []],
		  
		  [ [u_ty_c36_instance_c63, u_ob, [quote, u_rtrue]],
		    [list, [list, u_bd, [list, u_ob, u_ob]]]
		  ],
		  
		  [ 
		    [ or,
		      [u_ty_c36_instance_c63, u_ob, [quote, u_rand]],
		      [u_ty_c36_instance_c63, u_ob, [quote, u_rseq]]
		    ],
		    
		    [ u_show_all,
		      [u_ob_c36_gets, u_ob, [quote, u_obj]],
		      u_context,
		      u_bd,
		      u_belief_path
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob, [quote, u_rnot]],
		    
		    [ u_ob_c36_set,
		      u_xx_temp_not_ob_xx,
		      [quote, u_obj],
		      [u_ob_c36_get, u_ob, [quote, u_obj]]
		    ],
		    
		    [ or,
		      
		      [ u_show1,
			u_xx_temp_not_ob_xx,
			u_context,
			u_bd,
			u_belief_path
		      ],
		      
		      [ if,
			
			[ not,
			  
			  [ u_show,
			    [u_ob_c36_get, u_ob, [quote, u_obj]],
			    u_context,
			    u_bd,
			    u_belief_path
			  ]
			],
			[list, [list, u_bd, [list, u_ob, u_ob]]],
			[]
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob, [quote, u_ror]],
		    
		    [ u_yloop,
		      [u_initial, [u_shown_c63, []], [u_result, []]],
		      [u_yfor, u_elem, u_in, [u_ob_c36_gets, u_ob, [quote, u_obj]]],
		      [u_yuntil, u_shown_c63],
		      
		      [ u_ydo,
			
			[ setq,
			  u_shown_c63,
			  [u_show, u_elem, u_context, u_bd, u_belief_path]
			],
			
			[ if,
			  u_shown_c63,
			  [setq, u_result, [u_append_c33, u_result, u_shown_c63]]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ u_yloop,
		      [u_initial, [u_result, []]],
		      
		      [ u_yfor,
			u_retrieved,
			u_in,
			
			[ u_cx_c36_retrieve_bd_relative_ignore,
			  u_context,
			  u_ob,
			  u_bd,
			  u_belief_path,
			  [quote, [u_linked_from_of, u_linked_to_of]]
			]
		      ],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ or,
			    [u_null_c63, u_xx_top_level_goal_xx],
			    [not, [u_realistic_c63, u_xx_top_level_goal_xx]],
			    
			    [ u_ty_c36_instance_c63,
			      [car, u_retrieved],
			      [quote, u_need]
			    ],
			    
			    [ u_ty_c36_instance_c63,
			      [car, u_retrieved],
			      [quote, u_emotion]
			    ],
			    
			    [ u_fl_c62,
			      [u_strength, [car, u_retrieved]],
			      u_xx_realistic_show_strength_thresh_xx
			    ]
			  ],
			  
			  [ setq,
			    u_result,
			    
			    [ cons,
			      [list, u_retrieved, [list, [car, u_retrieved], u_ob]],
			      u_result
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ]
		]
	      ]).


% annotating U::SHOW1 
wl: arglist_info(u_show1,
		[u_ob, u_context, u_bd, u_belief_path],
		[Ob_Param, Context_Param, Bd_Param, Belief_path_Param],
		arginfo{ all:[u_ob, u_context, u_bd, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_context, u_bd, u_belief_path],
			 opt:0,
			 req:[u_ob, u_context, u_bd, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SHOW1 
wl: init_args(exact_only, u_show1).


% annotating U::SHOW1 
f_u_show1(Ob_Param, Context_Param, Bd_Param, Belief_path_Param, ElseResult66) :-
	Env=[bv(u_ob, Ob_Param), bv(u_context, Context_Param), bv(u_bd, Bd_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ty_c36_instance_c63(Ob_Param, u_rfalse, IFTEST),
	(   IFTEST\==[]
	->  ElseResult66=[]
	;   f_u_ty_c36_instance_c63(Ob_Param, u_rtrue, IFTEST21),
	    (   IFTEST21\==[]
	    ->  CAR=[Ob_Param, Ob_Param],
		CAR81=[Bd_Param, CAR],
		ElseResult66=[CAR81]
	    ;   (   f_u_ty_c36_instance_c63(Ob_Param, u_rand, FORM1_Res),
		    FORM1_Res\==[],
		    IFTEST27=FORM1_Res
		->  true
		;   f_u_ty_c36_instance_c63(Ob_Param, u_rseq, Rseq),
		    IFTEST27=Rseq
		),
		(   IFTEST27\==[]
		->  f_u_ob_c36_gets(Ob_Param, u_obj, Obj),
		    f_u_show_all(Obj,
				 Context_Param,
				 Bd_Param,
				 Belief_path_Param,
				 TrueResult69),
		    ElseResult66=TrueResult69
		;   f_u_ty_c36_instance_c63(Ob_Param, u_rnot, IFTEST36),
		    (   IFTEST36\==[]
		    ->  get_var(Env, u_xx_temp_not_ob_xx, Xx_temp_not_ob_xx_Get),
			f_u_ob_c36_get(Ob_Param, u_obj, Obj78),
			f_u_ob_c36_set(Xx_temp_not_ob_xx_Get,
				       u_obj,
				       Obj78,
				       C36_set_Ret),
			(   get_var(Env,
				    u_xx_temp_not_ob_xx,
				    Xx_temp_not_ob_xx_Get41),
			    f_u_show1(Xx_temp_not_ob_xx_Get41,
				      Context_Param,
				      Bd_Param,
				      Belief_path_Param,
				      FORM1_Res56),
			    FORM1_Res56\==[],
			    ElseResult66=FORM1_Res56
			->  true
			;   f_u_ob_c36_get(Ob_Param, u_obj, Obj79),
			    f_u_show(Obj79,
				     Context_Param,
				     Bd_Param,
				     Belief_path_Param,
				     PredArgResult),
			    (   PredArgResult==[]
			    ->  CAR83=[Ob_Param, Ob_Param],
				CAR84=[Bd_Param, CAR83],
				ElseResult66=[CAR84]
			    ;   ElseResult66=[]
			    )
			)
		    ;   f_u_ty_c36_instance_c63(Ob_Param, u_ror, IFTEST57),
			(   IFTEST57\==[]
			->  f_u_yloop(
				      [ 
					[ u_initial,
					  [u_shown_c63, []],
					  [u_result, []]
					],
					
					[ u_yfor,
					  u_elem,
					  u_in,
					  [u_ob_c36_gets, u_ob, [quote, u_obj]]
					],
					[u_yuntil, u_shown_c63],
					
					[ u_ydo,
					  
					  [ setq,
					    u_shown_c63,
					    
					    [ u_show,
					      u_elem,
					      u_context,
					      u_bd,
					      u_belief_path
					    ]
					  ],
					  
					  [ if,
					    u_shown_c63,
					    
					    [ setq,
					      u_result,
					      
					      [ u_append_c33,
						u_result,
						u_shown_c63
					      ]
					    ]
					  ]
					],
					[u_yresult, u_result]
				      ],
				      TrueResult65),
			    ElseResult66=TrueResult65
			;   get_var(Env, u_else, IFTEST60),
			    (   IFTEST60\==[]
			    ->  f_u_yloop(
					  [ [u_initial, [u_result, []]],
					    
					    [ u_yfor,
					      u_retrieved,
					      u_in,
					      
					      [ u_cx_c36_retrieve_bd_relative_ignore,
						u_context,
						u_ob,
						u_bd,
						u_belief_path,
						
						[ quote,
						  
						  [ u_linked_from_of,
						    u_linked_to_of
						  ]
						]
					      ]
					    ],
					    
					    [ u_ydo,
					      
					      [ if,
						
						[ or,
						  
						  [ u_null_c63,
						    u_xx_top_level_goal_xx
						  ],
						  
						  [ not,
						    
						    [ u_realistic_c63,
						      u_xx_top_level_goal_xx
						    ]
						  ],
						  
						  [ u_ty_c36_instance_c63,
						    [car, u_retrieved],
						    [quote, u_need]
						  ],
						  
						  [ u_ty_c36_instance_c63,
						    [car, u_retrieved],
						    [quote, u_emotion]
						  ],
						  
						  [ u_fl_c62,
						    
						    [ u_strength,
						      [car, u_retrieved]
						    ],
						    u_xx_realistic_show_strength_thresh_xx
						  ]
						],
						
						[ setq,
						  u_result,
						  
						  [ cons,
						    
						    [ list,
						      u_retrieved,
						      
						      [ list,
							[car, u_retrieved],
							u_ob
						      ]
						    ],
						    u_result
						  ]
						]
					      ]
					    ],
					    [u_yresult, u_result]
					  ],
					  TrueResult63),
				ElseResult66=TrueResult63
			    ;   ElseResult66=[]
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_show1, classof, claz_function),
   set_opv(u_show1, compile_as, kw_function),
   set_opv(u_show1, function, f_u_show1),
   DefunResult=u_show1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" First look for asserted NOTs.", 5, 28283)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: do we get into recursion problems here?",
				     5,
				     28319)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Then look for absense of assertion.",
				     9,
				     28428)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" In the latter case, make-dependency will instantiate and assert it",
				     1,
				     28592)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (let ((instan (ob$instantiate ob bd)))",
				     1,
				     28661)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (ob$set instan 'type *not-ob*)",
				     1,
				     28709)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             ; make-dependency will assert it.",
				     1,
				     28754)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("             (list (list bd (list instan ob))))",
				     1,
				     28802)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Why was it exactly that the below was needed?",
				     16,
				     29739)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" For what example?", 16, 29802)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:27917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ((bd (from-ob weight offset decay) (from-ob weight offset decay) ...) ...)",
				     1,
				     30065)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:30142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'show-all',
			    [obs, context, bd, 'belief-path'],
			    
			    [ 'ndbg-roman-nl',
			      '*gate-dbg*',
			      show,
			      '$STRING'("~&Show all ~A"),
			      obs
			    ],
			    
			    [ let,
			      
			      [ 
				[ 'show-list',
				  [show, [car, obs], context, bd, 'belief-path']
				]
			      ],
			      
			      [ if,
				['null?', [cdr, obs]],
				'show-list',
				
				[ yloop,
				  [yfor, show1, in, 'show-list'],
				  [initial, [result, []]],
				  
				  [ ydo,
				    
				    [ yloop,
				      
				      [ yfor,
					show2,
					in,
					
					[ 'show-all',
					  [cdr, obs],
					  context,
					  [car, show1],
					  'belief-path'
					]
				      ],
				      
				      [ ydo,
					
					[ setq,
					  result,
					  
					  [ 'append!',
					    result,
					    
					    [ list,
					      
					      [ cons,
						[car, show2],
						
						[ append,
						  [cdr, show1],
						  [cdr, show2]
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
			      ]
			    ]
			  ]).

% annotating U::SHOW-ALL 
wl: lambda_def(defun,
	      u_show_all,
	      f_u_show_all,
	      [u_obs, u_context, u_bd, u_belief_path],
	      
	      [ 
		[ u_ndbg_roman_nl,
		  u_xx_gate_dbg_xx,
		  u_show,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(~),
			     #\(&),
			     #\('S'),
			     #\(h),
			     #\(o),
			     #\(w),
			     #\(' '),
			     #\(a),
			     #\(l),
			     #\(l),
			     #\(' '),
			     #\(~),
			     #\('A')
			   ]),
		  u_obs
		],
		
		[ let,
		  
		  [ 
		    [ u_show_list,
		      [u_show, [car, u_obs], u_context, u_bd, u_belief_path]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, [cdr, u_obs]],
		    u_show_list,
		    
		    [ u_yloop,
		      [u_yfor, u_show1, u_in, u_show_list],
		      [u_initial, [u_result, []]],
		      
		      [ u_ydo,
			
			[ u_yloop,
			  
			  [ u_yfor,
			    u_show2,
			    u_in,
			    
			    [ u_show_all,
			      [cdr, u_obs],
			      u_context,
			      [car, u_show1],
			      u_belief_path
			    ]
			  ],
			  
			  [ u_ydo,
			    
			    [ setq,
			      u_result,
			      
			      [ u_append_c33,
				u_result,
				
				[ list,
				  
				  [ cons,
				    [car, u_show2],
				    [append, [cdr, u_show1], [cdr, u_show2]]
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ]
		]
	      ]).


% annotating U::SHOW-ALL 
wl: arglist_info(u_show_all,
		[u_obs, u_context, u_bd, u_belief_path],
		[Obs_Param, Context_Param, Bd_Param, Belief_path_Param],
		arginfo{ all:[u_obs, u_context, u_bd, u_belief_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obs, u_context, u_bd, u_belief_path],
			 opt:0,
			 req:[u_obs, u_context, u_bd, u_belief_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SHOW-ALL 
wl: init_args(exact_only, u_show_all).


% annotating U::SHOW-ALL 
f_u_show_all(Obs_Param, Context_Param, Bd_Param, Belief_path_Param, FnResult) :-
	Env=[bv(u_obs, Obs_Param), bv(u_context, Context_Param), bv(u_bd, Bd_Param), bv(u_belief_path, Belief_path_Param)],
	f_u_ndbg_roman_nl(u_xx_gate_dbg_xx,
			  u_show,
			  
			  [ '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\(&),
				       #\('S'),
				       #\(h),
				       #\(o),
				       #\(w),
				       #\(' '),
				       #\(a),
				       #\(l),
				       #\(l),
				       #\(' '),
				       #\(~),
				       #\('A')
				     ]),
			    u_obs
			  ],
			  Roman_nl_Ret),
	cl_car(Obs_Param, Show_Param),
	f_u_show(Show_Param,
		 Context_Param,
		 Bd_Param,
		 Belief_path_Param,
		 Show_list_Init),
	LEnv=[[bv(u_show_list, Show_list_Init)]|Env],
	f_u_null_c63([cdr, u_obs], IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_show_list, Show_list_Get),
	    FnResult=Show_list_Get
	;   f_u_yloop(
		      [ [u_yfor, u_show1, u_in, u_show_list],
			[u_initial, [u_result, []]],
			
			[ u_ydo,
			  
			  [ u_yloop,
			    
			    [ u_yfor,
			      u_show2,
			      u_in,
			      
			      [ u_show_all,
				[cdr, u_obs],
				u_context,
				[car, u_show1],
				u_belief_path
			      ]
			    ],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_result,
				
				[ u_append_c33,
				  u_result,
				  
				  [ list,
				    
				    [ cons,
				      [car, u_show2],
				      [append, [cdr, u_show1], [cdr, u_show2]]
				    ]
				  ]
				]
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_show_all, classof, claz_function),
   set_opv(u_show_all, compile_as, kw_function),
   set_opv(u_show_all, function, f_u_show_all),
   DefunResult=u_show_all.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:30142 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: should most likely be relative to belief path, but this is gnarly.",
				     1,
				     30934)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31009 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'top-level-goal?',
			    [x],
			    
			    [ and,
			      ['ty$instance?', x, [quote, goal]],
			      
			      [ 'null?',
				
				[ 'ol-get',
				  x,
				  '*intends-ob*',
				  [quote, backward],
				  '*unify-context*'
				]
			      ]
			    ]
			  ]).

% annotating U::TOP-LEVEL-GOAL? 
wl: lambda_def(defun,
	      u_top_level_goal_c63,
	      f_u_top_level_goal_c63,
	      [u_x],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_x, [quote, u_goal]],
		  
		  [ u_null_c63,
		    
		    [ u_ol_get,
		      u_x,
		      u_xx_intends_ob_xx,
		      [quote, u_backward],
		      u_xx_unify_context_xx
		    ]
		  ]
		]
	      ]).


% annotating U::TOP-LEVEL-GOAL? 
wl: arglist_info(u_top_level_goal_c63,
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

% annotating U::TOP-LEVEL-GOAL? 
wl: init_args(exact_only, u_top_level_goal_c63).


% annotating U::TOP-LEVEL-GOAL? 
f_u_top_level_goal_c63(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_ty_c36_instance_c63(X_Param, u_goal, IFTEST),
	(   IFTEST\==[]
	->  f_u_null_c63(
			 [ u_ol_get,
			   u_x,
			   u_xx_intends_ob_xx,
			   [quote, u_backward],
			   u_xx_unify_context_xx
			 ],
			 TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_top_level_goal_c63, classof, claz_function),
   set_opv(u_top_level_goal_c63, compile_as, kw_function),
   set_opv(u_top_level_goal_c63, function, f_u_top_level_goal_c63),
   DefunResult=u_top_level_goal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'tlg?',
			    [x, cx],
			    
			    [ and,
			      ['ty$instance?', x, [quote, goal]],
			      
			      [ 'null?',
				
				[ 'ol-get',
				  x,
				  '*intends-ob*',
				  [quote, backward],
				  cx
				]
			      ]
			    ]
			  ]).

% annotating U::TLG? 
wl: lambda_def(defun,
	      u_tlg_c63,
	      f_u_tlg_c63,
	      [u_x, u_cx],
	      
	      [ 
		[ and,
		  [u_ty_c36_instance_c63, u_x, [quote, u_goal]],
		  
		  [ u_null_c63,
		    [u_ol_get, u_x, u_xx_intends_ob_xx, [quote, u_backward], u_cx]
		  ]
		]
	      ]).


% annotating U::TLG? 
wl: arglist_info(u_tlg_c63,
		[u_x, u_cx],
		[X_Param, Cx_Param],
		arginfo{ all:[u_x, u_cx],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x, u_cx],
			 opt:0,
			 req:[u_x, u_cx],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TLG? 
wl: init_args(exact_only, u_tlg_c63).


% annotating U::TLG? 
f_u_tlg_c63(X_Param, Cx_Param, FnResult) :-
	Env=[bv(u_x, X_Param), bv(u_cx, Cx_Param)],
	f_u_ty_c36_instance_c63(X_Param, u_goal, IFTEST),
	(   IFTEST\==[]
	->  f_u_null_c63(
			 [ u_ol_get,
			   u_x,
			   u_xx_intends_ob_xx,
			   [quote, u_backward],
			   u_cx
			 ],
			 TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_tlg_c63, classof, claz_function),
   set_opv(u_tlg_c63, compile_as, kw_function),
   set_opv(u_tlg_c63, function, f_u_tlg_c63),
   DefunResult=u_tlg_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31239 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    prune,
			    [lst, predicate],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, i, in, lst],
			      
			      [ ydo,
				
				[ if,
				  [funcall, predicate, i],
				  [setq, result, [cons, i, result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::PRUNE 
wl: lambda_def(defun,
	      u_prune,
	      f_u_prune,
	      [u_lst, predicate],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_i, u_in, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      [funcall, predicate, u_i],
		      [setq, u_result, [cons, u_i, u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::PRUNE 
wl: arglist_info(u_prune,
		[u_lst, predicate],
		[Lst_Param, Predicate_Param],
		arginfo{ all:[u_lst, predicate],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst, predicate],
			 opt:0,
			 req:[u_lst, predicate],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRUNE 
wl: init_args(exact_only, u_prune).


% annotating U::PRUNE 
f_u_prune(Lst_Param, Predicate_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param), bv(predicate, Predicate_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_i, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[funcall, predicate, u_i],
			[setq, u_result, [cons, u_i, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_prune, classof, claz_function),
   set_opv(u_prune, compile_as, kw_function),
   set_opv(u_prune, function, f_u_prune),
   DefunResult=u_prune.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-leaf-causes',
			    [fact, context],
			    
			    [ 'get-leafs',
			      fact,
			      '*dependency-ob*',
			      [quote, backward],
			      context
			    ]
			  ]).

% annotating U::GET-LEAF-CAUSES 
wl: lambda_def(defun,
	      u_get_leaf_causes,
	      f_u_get_leaf_causes,
	      [u_fact, u_context],
	      
	      [ 
		[ u_get_leafs,
		  u_fact,
		  u_xx_dependency_ob_xx,
		  [quote, u_backward],
		  u_context
		]
	      ]).


% annotating U::GET-LEAF-CAUSES 
wl: arglist_info(u_get_leaf_causes,
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

% annotating U::GET-LEAF-CAUSES 
wl: init_args(exact_only, u_get_leaf_causes).


% annotating U::GET-LEAF-CAUSES 
f_u_get_leaf_causes(Fact_Param, Context_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	f_u_get_leafs(Fact_Param,
		      Xx_dependency_ob_xx_Get,
		      u_backward,
		      Context_Param,
		      Get_leafs_Ret),
	Get_leafs_Ret=FnResult.
:- set_opv(f_u_get_leaf_causes, classof, claz_function),
   set_opv(u_get_leaf_causes, compile_as, kw_function),
   set_opv(u_get_leaf_causes, function, f_u_get_leaf_causes),
   DefunResult=u_get_leaf_causes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Altern impl:", 1, 31533)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" BUT MUST RETURN *empty-bd* AS BELOW.",
				     1,
				     31548)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun get-leaf-causes (fact context)",
				     1,
				     31587)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (ol-path fact nil *dependency-ob* 'backward",
				     1,
				     31626)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           context (lambda (dummy ob) (leaf-fact? ob context)) nil))",
				     1,
				     31673)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 31743)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun leaf-fact? (ob context)", 1, 31745)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (null? (ol-get ob *dependency-ob* 'backward context)))",
				     1,
				     31777)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:31835 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-result-emotions',
			    [fact, context],
			    
			    [ 'ol-path',
			      fact,
			      [],
			      '*dependency-ob*',
			      [quote, forward],
			      context,
			      
			      [ lambda,
				[dummy, ob],
				
				[ if,
				  ['ty$instance?', ob, [quote, emotion]],
				  '*empty-bd*',
				  []
				]
			      ],
			      []
			    ]
			  ]).

% annotating U::GET-RESULT-EMOTIONS 
wl: lambda_def(defun,
	      u_get_result_emotions,
	      f_u_get_result_emotions,
	      [u_fact, u_context],
	      
	      [ 
		[ u_ol_path,
		  u_fact,
		  [],
		  u_xx_dependency_ob_xx,
		  [quote, u_forward],
		  u_context,
		  
		  [ lambda,
		    [u_dummy, u_ob],
		    
		    [ if,
		      [u_ty_c36_instance_c63, u_ob, [quote, u_emotion]],
		      u_xx_empty_bd_xx,
		      []
		    ]
		  ],
		  []
		]
	      ]).


% annotating U::GET-RESULT-EMOTIONS 
wl: arglist_info(u_get_result_emotions,
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

% annotating U::GET-RESULT-EMOTIONS 
wl: init_args(exact_only, u_get_result_emotions).


% annotating U::GET-RESULT-EMOTIONS 
f_u_get_result_emotions(Fact_Param, Context_Param, FnResult) :-
	Env=[bv(u_fact, Fact_Param), bv(u_context, Context_Param)],
	get_var(Env, u_xx_dependency_ob_xx, Xx_dependency_ob_xx_Get),
	Lambda=closure([Env|Env], LResult, [u_dummy, u_ob],  (get_var(Env, u_ob, Ob_Get), f_u_ty_c36_instance_c63(Ob_Get, u_emotion, IFTEST), (IFTEST\==[]->get_var(Env, u_xx_empty_bd_xx, Xx_empty_bd_xx_Get), LResult=Xx_empty_bd_xx_Get;LResult=[]))),
	f_u_ol_path(Fact_Param,
		    [],
		    Xx_dependency_ob_xx_Get,
		    u_forward,
		    Context_Param,
		    Lambda,
		    [],
		    Ol_path_Ret),
	Ol_path_Ret=FnResult.
:- set_opv(f_u_get_result_emotions, classof, claz_function),
   set_opv(u_get_result_emotions, compile_as, kw_function),
   set_opv(u_get_result_emotions, function, f_u_get_result_emotions),
   DefunResult=u_get_result_emotions.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:32115 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'get-leafs',
			    [ob, 'link-type', dir, context],
			    
			    [ let,
			      [[obs, ['ol-get', ob, 'link-type', dir, context]]],
			      
			      [ if,
				['null?', obs],
				[list, ob],
				
				[ yloop,
				  [initial, [result, []]],
				  [yfor, w, in, obs],
				  
				  [ ydo,
				    
				    [ setq,
				      result,
				      
				      [ append,
					
					[ 'get-leafs',
					  w,
					  'link-type',
					  dir,
					  context
					],
					result
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ]
			    ]
			  ]).

% annotating U::GET-LEAFS 
wl: lambda_def(defun,
	      u_get_leafs,
	      f_u_get_leafs,
	      [u_ob, u_link_type, ext_dir, u_context],
	      
	      [ 
		[ let,
		  [[u_obs, [u_ol_get, u_ob, u_link_type, ext_dir, u_context]]],
		  
		  [ if,
		    [u_null_c63, u_obs],
		    [list, u_ob],
		    
		    [ u_yloop,
		      [u_initial, [u_result, []]],
		      [u_yfor, u_w, u_in, u_obs],
		      
		      [ u_ydo,
			
			[ setq,
			  u_result,
			  
			  [ append,
			    [u_get_leafs, u_w, u_link_type, ext_dir, u_context],
			    u_result
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ]
		]
	      ]).


% annotating U::GET-LEAFS 
wl: arglist_info(u_get_leafs,
		[u_ob, u_link_type, ext_dir, u_context],
		[Ob_Param, Link_type_Param, Ext_dir_Param, Context_Param],
		arginfo{ all:[u_ob, u_link_type, ext_dir, u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_link_type, ext_dir, u_context],
			 opt:0,
			 req:[u_ob, u_link_type, ext_dir, u_context],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GET-LEAFS 
wl: init_args(exact_only, u_get_leafs).


% annotating U::GET-LEAFS 
f_u_get_leafs(Ob_Param, Link_type_Param, Ext_dir_Param, Context_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_link_type, Link_type_Param), bv(ext_dir, Ext_dir_Param), bv(u_context, Context_Param)],
	f_u_ol_get(Ob_Param,
		   Link_type_Param,
		   Ext_dir_Param,
		   Context_Param,
		   Obs_Init),
	LEnv=[[bv(u_obs, Obs_Init)]|Env],
	f_u_null_c63(u_obs, IFTEST),
	(   IFTEST\==[]
	->  FnResult=[Ob_Param]
	;   f_u_yloop(
		      [ [u_initial, [u_result, []]],
			[u_yfor, u_w, u_in, u_obs],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ append,
			      [u_get_leafs, u_w, u_link_type, ext_dir, u_context],
			      u_result
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_get_leafs, classof, claz_function),
   set_opv(u_get_leafs, compile_as, kw_function),
   set_opv(u_get_leafs, function, f_u_get_leafs),
   DefunResult=u_get_leafs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:32495 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*objects-in-omits*',
			    [cons, [quote, 'plan-rule'], '*link-slots*']
			  ]).
:- get_var(TLEnv3, u_xx_link_slots_xx, Xx_link_slots_xx_Get),
   _Ignored=[u_plan_rule|Xx_link_slots_xx_Get],
   set_var(TLEnv3, u_xx_objects_in_omits_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:32553 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'objects-in',
			    [ob],
			    
			    [ 'items-in',
			      ob,
			      [quote, 'varize-object?'],
			      '*objects-in-omits*'
			    ]
			  ]).

% annotating U::OBJECTS-IN 
wl: lambda_def(defun,
	      u_objects_in,
	      f_u_objects_in,
	      [u_ob],
	      
	      [ 
		[ u_items_in,
		  u_ob,
		  [quote, u_varize_object_c63],
		  u_xx_objects_in_omits_xx
		]
	      ]).


% annotating U::OBJECTS-IN 
wl: arglist_info(u_objects_in,
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

% annotating U::OBJECTS-IN 
wl: init_args(exact_only, u_objects_in).


% annotating U::OBJECTS-IN 
f_u_objects_in(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	get_var(Env, u_xx_objects_in_omits_xx, Xx_objects_in_omits_xx_Get),
	f_u_items_in(Ob_Param,
		     u_varize_object_c63,
		     Xx_objects_in_omits_xx_Get,
		     Items_in_Ret),
	Items_in_Ret=FnResult.
:- set_opv(f_u_objects_in, classof, claz_function),
   set_opv(u_objects_in, compile_as, kw_function),
   set_opv(u_objects_in, function, f_u_objects_in),
   DefunResult=u_objects_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:32629 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*visited-item-obs*', []]).
:- set_var(TLEnv3, setq, u_xx_visited_item_obs_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:32660 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'items-in',
			    [ob, pred, 'omit-slots'],
			    [setq, '*visited-item-obs*', []],
			    ['items-in1', ob, pred, 'omit-slots']
			  ]).

% annotating U::ITEMS-IN 
wl: lambda_def(defun,
	      u_items_in,
	      f_u_items_in,
	      [u_ob, u_pred, u_omit_slots],
	      
	      [ [setq, u_xx_visited_item_obs_xx, []],
		[u_items_in1, u_ob, u_pred, u_omit_slots]
	      ]).


% annotating U::ITEMS-IN 
wl: arglist_info(u_items_in,
		[u_ob, u_pred, u_omit_slots],
		[Ob_Param, Pred_Param, Omit_slots_Param],
		arginfo{ all:[u_ob, u_pred, u_omit_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_pred, u_omit_slots],
			 opt:0,
			 req:[u_ob, u_pred, u_omit_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ITEMS-IN 
wl: init_args(exact_only, u_items_in).


% annotating U::ITEMS-IN 
f_u_items_in(Ob_Param, Pred_Param, Omit_slots_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_pred, Pred_Param), bv(u_omit_slots, Omit_slots_Param)],
	set_var(Env, setq, u_xx_visited_item_obs_xx, []),
	f_u_items_in1(Ob_Param, Pred_Param, Omit_slots_Param, Items_in1_Ret),
	Items_in1_Ret=FnResult.
:- set_opv(f_u_items_in, classof, claz_function),
   set_opv(u_items_in, compile_as, kw_function),
   set_opv(u_items_in, function, f_u_items_in),
   DefunResult=u_items_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:32764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'items-in1',
			    [ob, pred, 'omit-slots'],
			    
			    [ if,
			      ['memq?', ob, '*visited-item-obs*'],
			      [],
			      
			      [ progn,
				
				[ setq,
				  '*visited-item-obs*',
				  [cons, ob, '*visited-item-obs*']
				],
				
				[ cond,
				  [[and, ['ob?', ob], ['ob$literal?', ob]], []],
				  
				  [ ['ob?', ob],
				    
				    [ yloop,
				      [initial, [result, []]],
				      [yfor, sv, in, ['ob$pairs', ob]],
				      
				      [ ydo,
					
					[ if,
					  
					  [ and,
					    
					    [ not,
					      
					      [ 'memq?',
						['slots-name', sv],
						'omit-slots'
					      ]
					    ],
					    [not, ['cx?', ['slots-value', sv]]]
					  ],
					  
					  [ if,
					    
					    [ and,
					      
					      [ apply,
						pred,
						[list, ['slots-value', sv]]
					      ],
					      
					      [ not,
						
						[ 'memq?',
						  ['slots-value', sv],
						  result
						]
					      ]
					    ],
					    
					    [ setq,
					      result,
					      [cons, ['slots-value', sv], result]
					    ],
					    
					    [ setq,
					      result,
					      
					      [ union,
						result,
						
						[ 'items-in1',
						  ['slots-value', sv],
						  pred,
						  'omit-slots'
						]
					      ]
					    ]
					  ]
					]
				      ],
				      [yresult, result]
				    ]
				  ],
				  [else, []]
				]
			      ]
			    ]
			  ]).

% annotating U::ITEMS-IN1 
wl: lambda_def(defun,
	      u_items_in1,
	      f_u_items_in1,
	      [u_ob, u_pred, u_omit_slots],
	      
	      [ 
		[ if,
		  [u_memq_c63, u_ob, u_xx_visited_item_obs_xx],
		  [],
		  
		  [ progn,
		    
		    [ setq,
		      u_xx_visited_item_obs_xx,
		      [cons, u_ob, u_xx_visited_item_obs_xx]
		    ],
		    
		    [ cond,
		      [[and, [u_ob_c63, u_ob], [u_ob_c36_literal_c63, u_ob]], []],
		      
		      [ [u_ob_c63, u_ob],
			
			[ u_yloop,
			  [u_initial, [u_result, []]],
			  [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
			  
			  [ u_ydo,
			    
			    [ if,
			      
			      [ and,
				
				[ not,
				  
				  [ u_memq_c63,
				    [u_slots_name, u_sv],
				    u_omit_slots
				  ]
				],
				[not, [u_cx_c63, [u_slots_value, u_sv]]]
			      ],
			      
			      [ if,
				
				[ and,
				  [apply, u_pred, [list, [u_slots_value, u_sv]]],
				  
				  [ not,
				    [u_memq_c63, [u_slots_value, u_sv], u_result]
				  ]
				],
				
				[ setq,
				  u_result,
				  [cons, [u_slots_value, u_sv], u_result]
				],
				
				[ setq,
				  u_result,
				  
				  [ union,
				    u_result,
				    
				    [ u_items_in1,
				      [u_slots_value, u_sv],
				      u_pred,
				      u_omit_slots
				    ]
				  ]
				]
			      ]
			    ]
			  ],
			  [u_yresult, u_result]
			]
		      ],
		      [u_else, []]
		    ]
		  ]
		]
	      ]).


% annotating U::ITEMS-IN1 
wl: arglist_info(u_items_in1,
		[u_ob, u_pred, u_omit_slots],
		[Ob_Param, Pred_Param, Omit_slots_Param],
		arginfo{ all:[u_ob, u_pred, u_omit_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_pred, u_omit_slots],
			 opt:0,
			 req:[u_ob, u_pred, u_omit_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ITEMS-IN1 
wl: init_args(exact_only, u_items_in1).


% annotating U::ITEMS-IN1 
f_u_items_in1(Ob_Param, Pred_Param, Omit_slots_Param, ElseResult33) :-
	Env=[bv(u_ob, Ob_Param), bv(u_pred, Pred_Param), bv(u_omit_slots, Omit_slots_Param)],
	f_u_memq_c63(u_ob, u_xx_visited_item_obs_xx, IFTEST),
	(   IFTEST\==[]
	->  ElseResult33=[]
	;   get_var(Env, u_xx_visited_item_obs_xx, Xx_visited_item_obs_xx_Get),
	    Xx_visited_item_obs_xx=[Ob_Param|Xx_visited_item_obs_xx_Get],
	    set_var(Env, u_xx_visited_item_obs_xx, Xx_visited_item_obs_xx),
	    f_u_ob_c63(u_ob, IFTEST22),
	    (   IFTEST22\==[]
	    ->  f_u_ob_c36_literal_c63(Ob_Param, TrueResult),
		IFTEST20=TrueResult
	    ;   IFTEST20=[]
	    ),
	    (   IFTEST20\==[]
	    ->  ElseResult33=[]
	    ;   f_u_ob_c63(u_ob, IFTEST26),
		(   IFTEST26\==[]
		->  f_u_yloop(
			      [ [u_initial, [u_result, []]],
				[u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ not,
					
					[ u_memq_c63,
					  [u_slots_name, u_sv],
					  u_omit_slots
					]
				      ],
				      [not, [u_cx_c63, [u_slots_value, u_sv]]]
				    ],
				    
				    [ if,
				      
				      [ and,
					
					[ apply,
					  u_pred,
					  [list, [u_slots_value, u_sv]]
					],
					
					[ not,
					  
					  [ u_memq_c63,
					    [u_slots_value, u_sv],
					    u_result
					  ]
					]
				      ],
				      
				      [ setq,
					u_result,
					[cons, [u_slots_value, u_sv], u_result]
				      ],
				      
				      [ setq,
					u_result,
					
					[ union,
					  u_result,
					  
					  [ u_items_in1,
					    [u_slots_value, u_sv],
					    u_pred,
					    u_omit_slots
					  ]
					]
				      ]
				    ]
				  ]
				],
				[u_yresult, u_result]
			      ],
			      TrueResult32),
		    ElseResult33=TrueResult32
		;   get_var(Env, u_else, IFTEST28),
		    (   IFTEST28\==[]
		    ->  ElseResult33=[]
		    ;   ElseResult33=[]
		    )
		)
	    )
	).
:- set_opv(f_u_items_in1, classof, claz_function),
   set_opv(u_items_in1, compile_as, kw_function),
   set_opv(u_items_in1, function, f_u_items_in1),
   DefunResult=u_items_in1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:33688 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'subset?',
			    [set1, set2],
			    ['every?', [lambda, [x], ['memq?', x, set2]], set1]
			  ]).

% annotating U::SUBSET? 
wl: lambda_def(defun,
	      u_subset_c63,
	      f_u_subset_c63,
	      [u_set1, u_set2],
	      [[u_every_c63, [lambda, [u_x], [u_memq_c63, u_x, u_set2]], u_set1]]).


% annotating U::SUBSET? 
wl: arglist_info(u_subset_c63,
		[u_set1, u_set2],
		[Set1_Param, Set2_Param],
		arginfo{ all:[u_set1, u_set2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_set1, u_set2],
			 opt:0,
			 req:[u_set1, u_set2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SUBSET? 
wl: init_args(exact_only, u_subset_c63).


% annotating U::SUBSET? 
f_u_subset_c63(Set1_Param, Set2_Param, FnResult) :-
	Env=[bv(u_set1, Set1_Param), bv(u_set2, Set2_Param)],
	f_u_every_c63([lambda, [u_x], [u_memq_c63, u_x, u_set2]], u_set1, Set1),
	Set1=FnResult.
:- set_opv(f_u_subset_c63, classof, claz_function),
   set_opv(u_subset_c63, compile_as, kw_function),
   set_opv(u_subset_c63, function, f_u_subset_c63),
   DefunResult=u_subset_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:33771 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'map-append',
			    [fn, lst, arg2],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, item, in, lst],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  [append, [funcall, fn, item, arg2], result]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::MAP-APPEND 
wl: lambda_def(defun,
	      u_map_append,
	      f_u_map_append,
	      [u_fn, u_lst, u_arg2],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, item, u_in, u_lst],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      [append, [funcall, u_fn, item, u_arg2], u_result]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::MAP-APPEND 
wl: arglist_info(u_map_append,
		[u_fn, u_lst, u_arg2],
		[Fn_Param, Lst_Param, Arg2_Param],
		arginfo{ all:[u_fn, u_lst, u_arg2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fn, u_lst, u_arg2],
			 opt:0,
			 req:[u_fn, u_lst, u_arg2],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAP-APPEND 
wl: init_args(exact_only, u_map_append).


% annotating U::MAP-APPEND 
f_u_map_append(Fn_Param, Lst_Param, Arg2_Param, FnResult) :-
	Env=[bv(u_fn, Fn_Param), bv(u_lst, Lst_Param), bv(u_arg2, Arg2_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, item, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			[append, [funcall, u_fn, item, u_arg2], u_result]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_map_append, classof, claz_function),
   set_opv(u_map_append, compile_as, kw_function),
   set_opv(u_map_append, function, f_u_map_append),
   DefunResult=u_map_append.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:33957 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'map-app',
			    [fn, lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, item, in, lst],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  [append, [funcall, fn, item], result]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::MAP-APP 
wl: lambda_def(defun,
	      u_map_app,
	      f_u_map_app,
	      [u_fn, u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, item, u_in, u_lst],
		  
		  [ u_ydo,
		    [setq, u_result, [append, [funcall, u_fn, item], u_result]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::MAP-APP 
wl: arglist_info(u_map_app,
		[u_fn, u_lst],
		[Fn_Param, Lst_Param],
		arginfo{ all:[u_fn, u_lst],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fn, u_lst],
			 opt:0,
			 req:[u_fn, u_lst],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAP-APP 
wl: init_args(exact_only, u_map_app).


% annotating U::MAP-APP 
f_u_map_app(Fn_Param, Lst_Param, FnResult) :-
	Env=[bv(u_fn, Fn_Param), bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, item, u_in, u_lst],
		    
		    [ u_ydo,
		      [setq, u_result, [append, [funcall, u_fn, item], u_result]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_map_app, classof, claz_function),
   set_opv(u_map_app, compile_as, kw_function),
   set_opv(u_map_app, function, f_u_map_app),
   DefunResult=u_map_app.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:34130 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    't-list-append!',
			    [old, new],
			    
			    [ if,
			      old,
			      
			      [ if,
				new,
				
				[ cons,
				  [quote, t],
				  ['append!', [cdr, old], [cdr, new]]
				],
				old
			      ],
			      new
			    ]
			  ]).

% annotating U::T-LIST-APPEND! 
wl: lambda_def(defun,
	      u_t_list_append_c33,
	      f_u_t_list_append_c33,
	      [u_old, u_new],
	      
	      [ 
		[ if,
		  u_old,
		  
		  [ if,
		    u_new,
		    [cons, [quote, t], [u_append_c33, [cdr, u_old], [cdr, u_new]]],
		    u_old
		  ],
		  u_new
		]
	      ]).


% annotating U::T-LIST-APPEND! 
wl: arglist_info(u_t_list_append_c33,
		[u_old, u_new],
		[Old_Param, New_Param],
		arginfo{ all:[u_old, u_new],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_old, u_new],
			 opt:0,
			 req:[u_old, u_new],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::T-LIST-APPEND! 
wl: init_args(exact_only, u_t_list_append_c33).


% annotating U::T-LIST-APPEND! 
f_u_t_list_append_c33(Old_Param, New_Param, TrueResult24) :-
	Env=[bv(u_old, Old_Param), bv(u_new, New_Param)],
	(   Old_Param\==[]
	->  (   New_Param\==[]
	    ->  f_u_append_c33([cdr, u_old], [cdr, u_new], Append_c33_Ret),
		TrueResult24=[t|Append_c33_Ret]
	    ;   TrueResult24=Old_Param
	    )
	;   TrueResult24=New_Param
	).
:- set_opv(f_u_t_list_append_c33, classof, claz_function),
   set_opv(u_t_list_append_c33, compile_as, kw_function),
   set_opv(u_t_list_append_c33, function, f_u_t_list_append_c33),
   DefunResult=u_t_list_append_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:34264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'me-in?', [lst], ['memq?', '*me-ob*', lst]]).

% annotating U::ME-IN? 
wl: lambda_def(defun,
	      u_me_in_c63,
	      f_u_me_in_c63,
	      [u_lst],
	      [[u_memq_c63, u_xx_me_ob_xx, u_lst]]).


% annotating U::ME-IN? 
wl: arglist_info(u_me_in_c63,
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

% annotating U::ME-IN? 
wl: init_args(exact_only, u_me_in_c63).


% annotating U::ME-IN? 
f_u_me_in_c63(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_memq_c63(u_xx_me_ob_xx, u_lst, Lst),
	Lst=FnResult.
:- set_opv(f_u_me_in_c63, classof, claz_function),
   set_opv(u_me_in_c63, compile_as, kw_function),
   set_opv(u_me_in_c63, function, f_u_me_in_c63),
   DefunResult=u_me_in_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:34308 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'non-mes',
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [ywhile, lst],
			      
			      [ ydo,
				
				[ if,
				  ['neq?', '*me-ob*', [car, lst]],
				  [setq, result, [cons, [car, lst], result]]
				],
				[setq, lst, [cdr, lst]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::NON-MES 
wl: lambda_def(defun,
	      u_non_mes,
	      f_u_non_mes,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_ywhile, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_neq_c63, u_xx_me_ob_xx, [car, u_lst]],
		      [setq, u_result, [cons, [car, u_lst], u_result]]
		    ],
		    [setq, u_lst, [cdr, u_lst]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::NON-MES 
wl: arglist_info(u_non_mes,
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

% annotating U::NON-MES 
wl: init_args(exact_only, u_non_mes).


% annotating U::NON-MES 
f_u_non_mes(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_neq_c63, u_xx_me_ob_xx, [car, u_lst]],
			[setq, u_result, [cons, [car, u_lst], u_result]]
		      ],
		      [setq, u_lst, [cdr, u_lst]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_non_mes, classof, claz_function),
   set_opv(u_non_mes, compile_as, kw_function),
   set_opv(u_non_mes, function, f_u_non_mes),
   DefunResult=u_non_mes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:34541 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'non-persons',
			    [lst, person],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [ywhile, lst],
			      
			      [ ydo,
				
				[ if,
				  ['neq?', person, [car, lst]],
				  [setq, result, [cons, [car, lst], result]]
				],
				[setq, lst, [cdr, lst]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::NON-PERSONS 
wl: lambda_def(defun,
	      u_non_persons,
	      f_u_non_persons,
	      [u_lst, u_person],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_ywhile, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_neq_c63, u_person, [car, u_lst]],
		      [setq, u_result, [cons, [car, u_lst], u_result]]
		    ],
		    [setq, u_lst, [cdr, u_lst]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::NON-PERSONS 
wl: arglist_info(u_non_persons,
		[u_lst, u_person],
		[Lst_Param, Person_Param],
		arginfo{ all:[u_lst, u_person],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_lst, u_person],
			 opt:0,
			 req:[u_lst, u_person],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NON-PERSONS 
wl: init_args(exact_only, u_non_persons).


% annotating U::NON-PERSONS 
f_u_non_persons(Lst_Param, Person_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param), bv(u_person, Person_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_neq_c63, u_person, [car, u_lst]],
			[setq, u_result, [cons, [car, u_lst], u_result]]
		      ],
		      [setq, u_lst, [cdr, u_lst]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_non_persons, classof, claz_function),
   set_opv(u_non_persons, compile_as, kw_function),
   set_opv(u_non_persons, function, f_u_non_persons),
   DefunResult=u_non_persons.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:34784 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'persons-in',
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, elem, in, lst],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    ['ob?', elem],
				    ['ty$instance?', elem, [quote, person]]
				  ],
				  [setq, result, [cons, elem, result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::PERSONS-IN 
wl: lambda_def(defun,
	      u_persons_in,
	      f_u_persons_in,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_elem, u_in, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			[u_ob_c63, u_elem],
			[u_ty_c36_instance_c63, u_elem, [quote, u_person]]
		      ],
		      [setq, u_result, [cons, u_elem, u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::PERSONS-IN 
wl: arglist_info(u_persons_in,
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

% annotating U::PERSONS-IN 
wl: init_args(exact_only, u_persons_in).


% annotating U::PERSONS-IN 
f_u_persons_in(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_elem, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ob_c63, u_elem],
			  [u_ty_c36_instance_c63, u_elem, [quote, u_person]]
			],
			[setq, u_result, [cons, u_elem, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_persons_in, classof, claz_function),
   set_opv(u_persons_in, compile_as, kw_function),
   set_opv(u_persons_in, function, f_u_persons_in),
   DefunResult=u_persons_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35008 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'non-persons-in',
			    [lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, elem, in, lst],
			      
			      [ ydo,
				
				[ if,
				  
				  [ and,
				    ['ob?', elem],
				    [not, ['ty$instance?', elem, [quote, person]]]
				  ],
				  [setq, result, [cons, elem, result]]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::NON-PERSONS-IN 
wl: lambda_def(defun,
	      u_non_persons_in,
	      f_u_non_persons_in,
	      [u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_elem, u_in, u_lst],
		  
		  [ u_ydo,
		    
		    [ if,
		      
		      [ and,
			[u_ob_c63, u_elem],
			[not, [u_ty_c36_instance_c63, u_elem, [quote, u_person]]]
		      ],
		      [setq, u_result, [cons, u_elem, u_result]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::NON-PERSONS-IN 
wl: arglist_info(u_non_persons_in,
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

% annotating U::NON-PERSONS-IN 
wl: init_args(exact_only, u_non_persons_in).


% annotating U::NON-PERSONS-IN 
f_u_non_persons_in(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_elem, u_in, u_lst],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_ob_c63, u_elem],
			  
			  [ not,
			    [u_ty_c36_instance_c63, u_elem, [quote, u_person]]
			  ]
			],
			[setq, u_result, [cons, u_elem, u_result]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_non_persons_in, classof, claz_function),
   set_opv(u_non_persons_in, compile_as, kw_function),
   set_opv(u_non_persons_in, function, f_u_non_persons_in),
   DefunResult=u_non_persons_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'do-interest',
			    [proc],
			    [funcall, proc, [quote, rule], [quote, all]],
			    [funcall, proc, [quote, task], [quote, all]],
			    [funcall, proc, [quote, 'ep-store'], [quote, all]],
			    [funcall, proc, [quote, remind], [quote, all]],
			    [funcall, proc, [quote, desire], [quote, all]],
			    [funcall, proc, [quote, simil], [quote, all]],
			    [funcall, proc, [quote, analogy], [quote, all]],
			    [funcall, proc, [quote, night], [quote, all]]
			  ]).

% annotating U::DO-INTEREST 
wl: lambda_def(defun,
	      u_do_interest,
	      f_u_do_interest,
	      [u_proc],
	      
	      [ [funcall, u_proc, [quote, u_rule], [quote, u_all]],
		[funcall, u_proc, [quote, u_task], [quote, u_all]],
		[funcall, u_proc, [quote, u_ep_store], [quote, u_all]],
		[funcall, u_proc, [quote, u_remind], [quote, u_all]],
		[funcall, u_proc, [quote, u_desire], [quote, u_all]],
		[funcall, u_proc, [quote, u_simil], [quote, u_all]],
		[funcall, u_proc, [quote, u_analogy], [quote, u_all]],
		[funcall, u_proc, [quote, u_night], [quote, u_all]]
	      ]).


% annotating U::DO-INTEREST 
wl: arglist_info(u_do_interest,
		[u_proc],
		[Proc_Param],
		arginfo{ all:[u_proc],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_proc],
			 opt:0,
			 req:[u_proc],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DO-INTEREST 
wl: init_args(exact_only, u_do_interest).


% annotating U::DO-INTEREST 
f_u_do_interest(Proc_Param, FnResult) :-
	Env=[bv(u_proc, Proc_Param)],
	f_u_proc(u_rule, u_all, All),
	f_u_proc(u_task, u_all, All15),
	f_u_proc(u_ep_store, u_all, All16),
	f_u_proc(u_remind, u_all, All17),
	f_u_proc(u_desire, u_all, All18),
	f_u_proc(u_simil, u_all, All19),
	f_u_proc(u_analogy, u_all, All20),
	f_u_proc(u_night, u_all, All21),
	All21=FnResult.
:- set_opv(f_u_do_interest, classof, claz_function),
   set_opv(u_do_interest, compile_as, kw_function),
   set_opv(u_do_interest, function, f_u_do_interest),
   DefunResult=u_do_interest.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'rule-long 'all) ; interleaved but not activated processes.",
				     1,
				     35319)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'rule-xtra 'all)",
				     1,
				     35396)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'seren-long 'all)",
				     1,
				     35635)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'ri 'all)", 1, 35670)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'chain 'all)", 1, 35697)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'inference 'all)",
				     1,
				     35727)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'depend 'all)", 1, 35761)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (funcall proc 'show 'all)", 1, 35792)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_utils.cl:35264 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 35824)).
:- true.


% Total time: 14.916 seconds


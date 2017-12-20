
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "dd_macros" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Sat Dec 16 22:35:31 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Daydreamer", 1, 84)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:96 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 3.5", 1, 97)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:110 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 111)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:112 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     113)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:182 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 183)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:205 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 206)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:207 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     208)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'define-initial-fact',
			    [subsets, spec],
			    
			    [ '#BQ',
			      
			      [ if,
				
				[ 'loadable-subsets?',
				  [quote, ['#COMMA', subsets]]
				],
				
				[ let,
				  
				  [ 
				    [ temp,
				      ['ob$create', [quote, ['#COMMA', spec]]]
				    ]
				  ],
				  
				  [ setq,
				    '*initial-facts*',
				    [cons, temp, '*initial-facts*']
				  ],
				  temp
				],
				[]
			      ]
			    ]
			  ]).
/* 
alphas=[u_spec, u_subsets, f_u_define_initial_fact].
type=ctx.
var_tracker(u_spec)=rw{name:u_spec, p:1, r:0, ret:0, u:0, vars:[Spec_Param], w:1}.
var_tracker(u_subsets)=rw{name:u_subsets, p:1, r:0, ret:0, u:0, vars:[Subsets_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_define_initial_fact,
		[u_subsets, u_spec],
		[Subsets_Param, Spec_Param],
		arginfo{ all:[u_subsets, u_spec],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_subsets, u_spec],
			 opt:0,
			 req:[u_subsets, u_spec],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_define_initial_fact).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_define_initial_fact,
	      f_u_define_initial_fact,
	      [u_subsets, u_spec],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    [u_loadable_subsets_c63, [quote, ['#COMMA', u_subsets]]],
		    
		    [ let,
		      [[u_temp, [u_ob_c36_create, [quote, ['#COMMA', u_spec]]]]],
		      
		      [ setq,
			u_xx_initial_facts_xx,
			[cons, u_temp, u_xx_initial_facts_xx]
		      ],
		      u_temp
		    ],
		    []
		  ]
		]
	      ]).


% asserting... u 
f_u_define_initial_fact(Subsets_Param, Spec_Param, FnResult) :-
	[if, [u_loadable_subsets_c63, [quote, Subsets_Param]], [let, [[u_temp, [u_ob_c36_create, [quote, Spec_Param]]]], [setq, u_xx_initial_facts_xx, [cons, u_temp, u_xx_initial_facts_xx]], u_temp], []]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_define_initial_fact, classof, claz_macro),
   set_opv(u_define_initial_fact, compile_as, kw_operator),
   set_opv(u_define_initial_fact, function, f_u_define_initial_fact).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 507)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Episode definition", 1, 509)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 530)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (define-episode <subsets> <indices: ([ob-spec | obname] ...)> <plan-thresh>",
				     1,
				     532)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 <reminding-thresh> (<goal> [<rulename> | nil] <recurse> ...))",
				     1,
				     610)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 690)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: A define-episode pretty-printer (full obs are too cumbersome)",
				     1,
				     692)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 762)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Plan-thresh and reminding-thresh must include the rule index.",
				     1,
				     764)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:288 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 828)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'define-episode',
			    
			    [ subsets,
			      indices,
			      'plan-thresh',
			      'reminding-thresh',
			      'episode-defn'
			    ],
			    
			    [ '#BQ',
			      
			      [ if,
				
				[ 'loadable-subsets?',
				  [quote, ['#COMMA', subsets]]
				],
				
				[ let,
				  
				  [ ['ep-context', ['cx$create']],
				    [ep, []],
				    
				    [ 'hidden?',
				      
				      [ not,
					['nil?', [quote, ['#COMMA', indices]]]
				      ]
				    ],
				    [temp, []]
				  ],
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Defining episode...")
				  ],
				  
				  [ setq,
				    ep,
				    
				    [ 'ob$get',
				      
				      [ 'episode-defn->stored-episode',
					[quote, ['#COMMA', 'episode-defn']],
					'ep-context',
					'hidden?'
				      ],
				      [quote, episode]
				    ]
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
					1
				      ],
				      
				      [ yloop,
					
					[ yfor,
					  index,
					  in,
					  [quote, ['#COMMA', indices]]
					],
					
					[ ydo,
					  
					  [ if,
					    ['symbol?', index],
					    [setq, temp, ['ob$name->ob', index]],
					    [setq, temp, ['ob$fcreate', index]]
					  ],
					  
					  [ if,
					    ['null?', temp],
					    
					    [ progn,
					      
					      [ error,
						'$STRING'("Trouble with defining ~A"),
						index
					      ],
					      
					      [ 'ndbg-roman-nl',
						'*gate-dbg*',
						rule,
						'$STRING'("Ignored.")
					      ]
					    ],
					    
					    [ progn,
					      ['epmem-store', ep, temp, t, t]
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ if,
				    ['#COMMA', 'plan-thresh'],
				    
				    [ 'ob$set',
				      ep,
				      [quote, 'plan-threshold'],
				      ['#COMMA', 'plan-thresh']
				    ]
				  ],
				  
				  [ if,
				    ['#COMMA', 'reminding-thresh'],
				    
				    [ 'ob$set',
				      ep,
				      [quote, 'reminding-threshold'],
				      ['#COMMA', 'reminding-thresh']
				    ]
				  ],
				  ep
				],
				[]
			      ]
			    ]
			  ]).
/* 
alphas=[u_episode_defn, u_reminding_thresh, u_plan_thresh, u_indices, u_subsets, f_u_define_episode].
type=ctx.
var_tracker(u_episode_defn)=rw{name:u_episode_defn, p:1, r:0, ret:0, u:0, vars:[Episode_defn_Param], w:1}.
var_tracker(u_indices)=rw{name:u_indices, p:1, r:0, ret:0, u:0, vars:[Indices_Param], w:1}.
var_tracker(u_plan_thresh)=rw{name:u_plan_thresh, p:1, r:0, ret:0, u:0, vars:[Plan_thresh_Param], w:1}.
var_tracker(u_reminding_thresh)=rw{name:u_reminding_thresh, p:1, r:0, ret:0, u:0, vars:[Reminding_thresh_Param], w:1}.
var_tracker(u_subsets)=rw{name:u_subsets, p:1, r:0, ret:0, u:0, vars:[Subsets_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_define_episode,
		
		[ u_subsets,
		  u_indices,
		  u_plan_thresh,
		  u_reminding_thresh,
		  u_episode_defn
		],
		
		[ Subsets_Param,
		  Indices_Param,
		  Plan_thresh_Param,
		  Reminding_thresh_Param,
		  Episode_defn_Param
		],
		arginfo{ all:
			     [ u_subsets,
			       u_indices,
			       u_plan_thresh,
			       u_reminding_thresh,
			       u_episode_defn
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_subsets,
				 u_indices,
				 u_plan_thresh,
				 u_reminding_thresh,
				 u_episode_defn
			       ],
			 opt:0,
			 req:
			     [ u_subsets,
			       u_indices,
			       u_plan_thresh,
			       u_reminding_thresh,
			       u_episode_defn
			     ],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_define_episode).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_define_episode,
	      f_u_define_episode,
	      
	      [ u_subsets,
		u_indices,
		u_plan_thresh,
		u_reminding_thresh,
		u_episode_defn
	      ],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    [u_loadable_subsets_c63, [quote, ['#COMMA', u_subsets]]],
		    
		    [ let,
		      
		      [ [u_ep_context, [u_cx_c36_create]],
			[u_ep, []],
			
			[ u_hidden_c63,
			  [not, [u_nil_c63, [quote, ['#COMMA', u_indices]]]]
			],
			[u_temp, []]
		      ],
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('D'),
				   #\(e),
				   #\(f),
				   #\(i),
				   #\(n),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(e),
				   #\(p),
				   #\(i),
				   #\(s),
				   #\(o),
				   #\(d),
				   #\(e),
				   #\('.'),
				   #\('.'),
				   #\('.')
				 ])
		      ],
		      
		      [ setq,
			u_ep,
			
			[ u_ob_c36_get,
			  
			  [ u_episode_defn_c62_stored_episode,
			    [quote, ['#COMMA', u_episode_defn]],
			    u_ep_context,
			    u_hidden_c63
			  ],
			  [quote, u_episode]
			]
		      ],
		      
		      [ if,
			u_hidden_c63,
			
			[ progn,
			  
			  [ u_ob_c36_set,
			    u_ep,
			    [quote, u_plan_threshold],
			    u_xx_infinite_thresh_xx
			  ],
			  [u_ob_c36_set, u_ep, [quote, u_reminding_threshold], 1],
			  
			  [ u_yloop,
			    [u_yfor, index, u_in, [quote, ['#COMMA', u_indices]]],
			    
			    [ u_ydo,
			      
			      [ if,
				[u_symbol_c63, index],
				[setq, u_temp, [u_ob_c36_name_c62_ob, index]],
				[setq, u_temp, [u_ob_c36_fcreate, index]]
			      ],
			      
			      [ if,
				[u_null_c63, u_temp],
				
				[ progn,
				  
				  [ error,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('T'),
					       #\(r),
					       #\(o),
					       #\(u),
					       #\(b),
					       #\(l),
					       #\(e),
					       #\(' '),
					       #\(w),
					       #\(i),
					       #\(t),
					       #\(h),
					       #\(' '),
					       #\(d),
					       #\(e),
					       #\(f),
					       #\(i),
					       #\(n),
					       #\(i),
					       #\(n),
					       #\(g),
					       #\(' '),
					       #\(~),
					       #\('A')
					     ]),
				    index
				  ],
				  
				  [ u_ndbg_roman_nl,
				    u_xx_gate_dbg_xx,
				    u_rule,
				    '$ARRAY'([*],
					     claz_base_character,
					     
					     [ #\('I'),
					       #\(g),
					       #\(n),
					       #\(o),
					       #\(r),
					       #\(e),
					       #\(d),
					       #\('.')
					     ])
				  ]
				],
				[progn, [u_epmem_store, u_ep, u_temp, t, t]]
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ if,
			['#COMMA', u_plan_thresh],
			
			[ u_ob_c36_set,
			  u_ep,
			  [quote, u_plan_threshold],
			  ['#COMMA', u_plan_thresh]
			]
		      ],
		      
		      [ if,
			['#COMMA', u_reminding_thresh],
			
			[ u_ob_c36_set,
			  u_ep,
			  [quote, u_reminding_threshold],
			  ['#COMMA', u_reminding_thresh]
			]
		      ],
		      u_ep
		    ],
		    []
		  ]
		]
	      ]).


% asserting... u 
f_u_define_episode(Subsets_Param, Indices_Param, Plan_thresh_Param, Reminding_thresh_Param, Episode_defn_Param, FnResult) :-
	[if, [u_loadable_subsets_c63, [quote, Subsets_Param]], [let, [[u_ep_context, [u_cx_c36_create]], [u_ep, []], [u_hidden_c63, [not, [u_nil_c63, [quote, Indices_Param]]]], [u_temp, []]], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, [#\('D'), #\(e), #\(f), #\(i), #\(n), #\(i), #\(n), #\(g), #\(' '), #\(e), #\(p), #\(i), #\(s), #\(o), #\(d), #\(e), #\('.'), #\('.'), #\('.')])], [setq, u_ep, [u_ob_c36_get, [u_episode_defn_c62_stored_episode, [quote, Episode_defn_Param], u_ep_context, u_hidden_c63], [quote, u_episode]]], [if, u_hidden_c63, [progn, [u_ob_c36_set, u_ep, [quote, u_plan_threshold], u_xx_infinite_thresh_xx], [u_ob_c36_set, u_ep, [quote, u_reminding_threshold], 1], [u_yloop, [u_yfor, index, u_in, [quote, Indices_Param]], [u_ydo, [if, [u_symbol_c63, index], [setq, u_temp, [u_ob_c36_name_c62_ob, index]], [setq, u_temp, [u_ob_c36_fcreate, index]]], [if, [u_null_c63, u_temp], [progn, [error, '$ARRAY'([*], claz_base_character, [#\('T'), #\(r), #\(o), #\(u), #\(b), #\(l), #\(e), #\(' '), #\(w), #\(i), #\(t), #\(h), #\(' '), #\(d), #\(e), #\(f), #\(i), #\(n), #\(i), #\(n), #\(g), #\(' '), #\(~), #\('A')]), index], [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, [#\('I'), #\(g), #\(n), #\(o), #\(r), #\(e), #\(d), #\('.')])]], [progn, [u_epmem_store, u_ep, u_temp, t, t]]]]]]], [if, Plan_thresh_Param, [u_ob_c36_set, u_ep, [quote, u_plan_threshold], Plan_thresh_Param]], [if, Reminding_thresh_Param, [u_ob_c36_set, u_ep, [quote, u_reminding_threshold], Reminding_thresh_Param]], u_ep], []]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_define_episode, classof, claz_macro),
   set_opv(u_define_episode, compile_as, kw_operator),
   set_opv(u_define_episode, function, f_u_define_episode).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:829 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If indices are present, only the top-level goal of episode",
				     14,
				     1087)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:829 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" is retrievable.", 14, 1161)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:2506 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'with-no-dbg',
			    ['&rest', rest],
			    
			    [ '#BQ',
			      
			      [ 'unwind-protect',
				
				[ progn,
				  ['do-interest', function(disinterest)],
				  ['#BQ-COMMA-ELIPSE', rest]
				],
				['do-interest', function(interest)]
			      ]
			    ]
			  ]).
/* 
alphas=[rest, f_u_with_no_dbg].
type=ctx.
var_tracker(rest)=rw{name:rest, p:0, r:0, ret:0, u:0, vars:[Rest_Param], w:0}.
 */

% asserting1... u 
wl: arglist_info(f_u_with_no_dbg,
		[c38_rest, rest],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[rest],
			 opt:0,
			 req:0,
			 rest:[rest],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(rest_only, f_u_with_no_dbg).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_with_no_dbg,
	      f_u_with_no_dbg,
	      [c38_rest, rest],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ unwind_protect,
		    
		    [ progn,
		      [u_do_interest, function(u_disinterest)],
		      ['#BQ-COMMA-ELIPSE', rest]
		    ],
		    [u_do_interest, function(u_interest)]
		  ]
		]
	      ]).


% asserting... u 
f_u_with_no_dbg(Whole, FnResult) :-
	Env=[bv(rest, Rest_Param)],
	append([], Rest_Param, Whole),
	get_var(Env, rest, Rest_Get),
	[unwind_protect, [progn, [u_do_interest, function(u_disinterest)]|Rest_Get], [u_do_interest, function(u_interest)]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_with_no_dbg, classof, claz_macro),
   set_opv(u_with_no_dbg, compile_as, kw_operator),
   set_opv(u_with_no_dbg, function, f_u_with_no_dbg).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:2649 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'self-type-ok?',
			    [rule, self],
			    
			    [ '#BQ',
			      
			      [ or,
				
				[ 'null?',
				  
				  [ 'ob$gets',
				    ['#COMMA', rule],
				    [quote, 'self-type']
				  ]
				],
				
				[ 'any?',
				  
				  [ lambda,
				    [x],
				    ['ty$instance-of?', ['#COMMA', self], x]
				  ],
				  
				  [ 'ob$gets',
				    ['#COMMA', rule],
				    [quote, 'self-type']
				  ]
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_self, u_rule, f_u_self_type_ok_c63].
type=ctx.
var_tracker(u_rule)=rw{name:u_rule, p:1, r:0, ret:0, u:0, vars:[Rule_Param], w:1}.
var_tracker(u_self)=rw{name:u_self, p:1, r:0, ret:0, u:0, vars:[Self_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_self_type_ok_c63,
		[u_rule, u_self],
		[Rule_Param, Self_Param],
		arginfo{ all:[u_rule, u_self],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_self],
			 opt:0,
			 req:[u_rule, u_self],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_self_type_ok_c63).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_self_type_ok_c63,
	      f_u_self_type_ok_c63,
	      [u_rule, u_self],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ or,
		    
		    [ u_null_c63,
		      [u_ob_c36_gets, ['#COMMA', u_rule], [quote, u_self_type]]
		    ],
		    
		    [ u_any_c63,
		      
		      [ lambda,
			[u_x],
			[u_ty_c36_instance_of_c63, ['#COMMA', u_self], u_x]
		      ],
		      [u_ob_c36_gets, ['#COMMA', u_rule], [quote, u_self_type]]
		    ]
		  ]
		]
	      ]).


% asserting... u 
f_u_self_type_ok_c63(Rule_Param, Self_Param, FnResult) :-
	[or, [u_null_c63, [u_ob_c36_gets, Rule_Param, [quote, u_self_type]]], [u_any_c63, [lambda, [u_x], [u_ty_c36_instance_of_c63, Self_Param, u_x]], [u_ob_c36_gets, Rule_Param, [quote, u_self_type]]]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_self_type_ok_c63, classof, claz_macro),
   set_opv(u_self_type_ok_c63, compile_as, kw_operator),
   set_opv(u_self_type_ok_c63, function, f_u_self_type_ok_c63).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:2823 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'define-rule',
			    [name, subsets, spec],
			    
			    [ if,
			      ['loadable-subsets?', subsets],
			      
			      [ let,
				
				[ [rule, ['ob$name->ob', name]],
				  [ruleob, ['ob$create', spec]]
				],
				
				[ if,
				  rule,
				  
				  [ progn,
				    ['rule-destroy-chaining', rule],
				    ['ob$remove-all', rule],
				    ['ob$concatenate!', rule, ruleob],
				    ['rule-create-chaining', rule],
				    
				    [ 'ndbg-roman',
				      '*gate-dbg*',
				      rule,
				      '$STRING'("~A redefined "),
				      name
				    ]
				  ],
				  
				  [ progn,
				    
				    [ if,
				      ['nil?', name],
				      
				      [ progn,
					[setq, rule, ['ob$create-empty']],
					['ob$concatenate!', rule, ruleob]
				      ],
				      
				      [ progn,
					[setq, rule, ['ob$create-empty']],
					['ob$add-name', rule, name],
					['ob$concatenate!', rule, ruleob]
				      ]
				    ],
				    ['ob$set', rule, [quote, 'accessible?'], t],
				    ['add-rule', rule]
				  ]
				],
				['check-rule', rule, ['ob$name', rule]],
				[list, [quote, quote], ['ob$name', rule]]
			      ],
			      [list, [quote, quote], [quote, 'rule-not-loaded']]
			    ]
			  ]).
/* 
alphas=[u_ruleob, u_rule, u_spec, u_subsets, sys_name, f_u_define_rule].
type=ctx.
var_tracker(sys_name)=rw{name:sys_name, p:1, r:6, ret:0, u:0, vars:[Name_Param, Name_Get, Name_Get218, Name_Get237, Name_Get284, Name_Get288, Name_Get320], w:1}.
var_tracker(u_else)=rw{name:u_else, p:0, r:1, ret:0, u:0, vars:[IFTEST244], w:0}.
var_tracker(u_rule)=rw{name:u_rule, p:0, r:13, ret:0, u:0, vars:[IFTEST52, Rule_Get62, Rule_Get67, Rule_Get72, Rule_Get79, Rule_Get302, Rule_Get318, Rule_Get325, Rule_Get336, Rule_Get343, Rule_Get352, Rule_Get355, Rule_Get362], w:2}.
var_tracker(u_ruleob)=rw{name:u_ruleob, p:0, r:3, ret:0, u:0, vars:[Ruleob_Get, Ruleob_Get304, Ruleob_Get327], w:0}.
var_tracker(u_spec)=rw{name:u_spec, p:1, r:1, ret:0, u:0, vars:[Spec_Param, Spec_Get], w:1}.
var_tracker(u_subsets)=rw{name:u_subsets, p:1, r:1, ret:0, u:0, vars:[Subsets_Param, Subsets_Get], w:1}.
var_tracker(u_x)=rw{name:u_x, p:0, r:2, ret:0, u:0, vars:[X_Get, X_Get184], w:0}.
var_tracker(u_xx_gate_dbg_xx)=rw{name:u_xx_gate_dbg_xx, p:0, r:5, ret:0, u:0, vars:[Xx_gate_dbg_xx_Get, Xx_gate_dbg_xx_Get208, IFTEST225, Xx_gate_dbg_xx_Get234, Xx_gate_dbg_xx_Get266], w:0}.
var_tracker(u_xx_ndbg_interests_xx)=rw{name:u_xx_ndbg_interests_xx, p:0, r:6, ret:0, u:0, vars:[Xx_ndbg_interests_xx_Get, Xx_ndbg_interests_xx_Get106, Xx_ndbg_interests_xx_Get133, Xx_ndbg_interests_xx_Get161, Xx_ndbg_interests_xx_Get171, Xx_ndbg_interests_xx_Get198], w:0}.
var_tracker(u_xx_ndbg_items_xx)=rw{name:u_xx_ndbg_items_xx, p:0, r:2, ret:0, u:0, vars:[Xx_ndbg_items_xx_Get, Xx_ndbg_items_xx_Get187], w:0}.
 */

% asserting1... u 
wl: arglist_info(f_u_define_rule,
		[sys_name, u_subsets, u_spec],
		[Name_Param, Subsets_Param, Spec_Param],
		arginfo{ all:[sys_name, u_subsets, u_spec],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_name, u_subsets, u_spec],
			 opt:0,
			 req:[sys_name, u_subsets, u_spec],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_define_rule).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_define_rule,
	      f_u_define_rule,
	      [sys_name, u_subsets, u_spec],
	      
	      [ progn,
		
		[ if,
		  [u_loadable_subsets_c63, u_subsets],
		  
		  [ let,
		    
		    [ [u_rule, [u_ob_c36_name_c62_ob, sys_name]],
		      [u_ruleob, [u_ob_c36_create, u_spec]]
		    ],
		    
		    [ if,
		      u_rule,
		      
		      [ progn,
			[u_rule_destroy_chaining, u_rule],
			[u_ob_c36_remove_all, u_rule],
			[u_ob_c36_concatenate_c33, u_rule, u_ruleob],
			[u_rule_create_chaining, u_rule],
			
			[ u_ndbg_roman,
			  u_xx_gate_dbg_xx,
			  u_rule,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(~),
				     #\('A'),
				     #\(' '),
				     #\(r),
				     #\(e),
				     #\(d),
				     #\(e),
				     #\(f),
				     #\(i),
				     #\(n),
				     #\(e),
				     #\(d),
				     #\(' ')
				   ]),
			  sys_name
			]
		      ],
		      
		      [ progn,
			
			[ if,
			  [u_nil_c63, sys_name],
			  
			  [ progn,
			    [setq, u_rule, [u_ob_c36_create_empty]],
			    [u_ob_c36_concatenate_c33, u_rule, u_ruleob]
			  ],
			  
			  [ progn,
			    [setq, u_rule, [u_ob_c36_create_empty]],
			    [u_ob_c36_add_name, u_rule, sys_name],
			    [u_ob_c36_concatenate_c33, u_rule, u_ruleob]
			  ]
			],
			[u_ob_c36_set, u_rule, [quote, u_accessible_c63], t],
			[u_add_rule, u_rule]
		      ]
		    ],
		    [u_check_rule, u_rule, [u_ob_c36_name, u_rule]],
		    [list, [quote, quote], [u_ob_c36_name, u_rule]]
		  ],
		  [list, [quote, quote], [quote, u_rule_not_loaded]]
		]
	      ]).


% asserting... u 
f_u_define_rule(Name_Param, Subsets_Param, Spec_Param, FnResult) :-
	Env=[bv(sys_name, Name_Param), bv(u_subsets, Subsets_Param), bv(u_spec, Spec_Param)],
	f_u_loadable_subsets_c63(Subsets_Param, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_name_c62_ob(Name_Param, Rule_Init),
	    f_u_ob_c36_readlist(Spec_Param, Ruleob_Init),
	    LEnv=[[bv(u_rule, Rule_Init), bv(u_ruleob, Ruleob_Init)]|Env],
	    get_var(LEnv, u_rule, IFTEST52),
	    (   IFTEST52\==[]
	    ->  get_var(LEnv, u_rule, Rule_Get62),
		f_u_rule_destroy_chaining(Rule_Get62, Destroy_chaining_Ret),
		get_var(LEnv, u_rule, Rule_Get67),
		f_u_ob_c36_remove_all(Rule_Get67, Remove_all_Ret),
		get_var(LEnv, u_rule, Rule_Get72),
		get_var(LEnv, u_ruleob, Ruleob_Get),
		f_u_ob_c36_concatenate_c33(Rule_Get72,
					   Ruleob_Get,
					   Concatenate_c33_Ret),
		get_var(LEnv, u_rule, Rule_Get79),
		f_u_rule_create_chaining(Rule_Get79, Create_chaining_Ret),
		get_var(LEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
		cl_assoc(u_rule, Xx_ndbg_interests_xx_Get, [], IFTEST90),
		(   IFTEST90\==[]
		->  (   get_var(LEnv,
				u_xx_ndbg_interests_xx,
				Xx_ndbg_interests_xx_Get106),
			cl_assoc(u_rule,
				 Xx_ndbg_interests_xx_Get106,
				 [],
				 Assoc_Ret),
			cl_member(u_all, Assoc_Ret, Or_nil_Param),
			f_u_t_or_nil(Or_nil_Param, FORM1_Res),
			FORM1_Res\==[],
			IFTEST85=FORM1_Res
		    ->  true
		    ;   Lambda=closure([Env|LEnv], LResult, [u_x],  (get_var(Env, u_x, X_Get), get_var(Env, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get), cl_member(X_Get, Xx_ndbg_items_xx_Get, Or_nil_Param380), f_u_t_or_nil(Or_nil_Param380, LResult))),
			get_var(LEnv,
				u_xx_ndbg_interests_xx,
				Xx_ndbg_interests_xx_Get133),
			cl_assoc(u_rule,
				 Xx_ndbg_interests_xx_Get133,
				 [],
				 Cdr_Param),
			cl_cdr(Cdr_Param, Cdr_Ret),
			cl_some(Lambda, Cdr_Ret, Or_nil_Param382),
			f_u_t_or_nil(Or_nil_Param382, Or_nil_Ret),
			IFTEST85=Or_nil_Ret
		    )
		;   IFTEST85=[]
		),
		(   IFTEST85\==[]
		->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get),
		    f_u_begin_roman_font(Xx_gate_dbg_xx_Get, Roman_font_Ret),
		    get_var(LEnv,
			    u_xx_ndbg_interests_xx,
			    Xx_ndbg_interests_xx_Get161),
		    cl_assoc(u_rule, Xx_ndbg_interests_xx_Get161, [], IFTEST155),
		    (   IFTEST155\==[]
		    ->  (   get_var(LEnv,
				    u_xx_ndbg_interests_xx,
				    Xx_ndbg_interests_xx_Get171),
			    cl_assoc(u_rule,
				     Xx_ndbg_interests_xx_Get171,
				     [],
				     Assoc_Ret395),
			    cl_member(u_all, Assoc_Ret395, Or_nil_Param383),
			    f_u_t_or_nil(Or_nil_Param383, FORM1_Res199),
			    FORM1_Res199\==[],
			    IFTEST150=FORM1_Res199
			->  true
			;   Lambda191=closure([Env|LEnv], LResult190, [u_x],  (get_var(Env, u_x, X_Get184), get_var(Env, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get187), cl_member(X_Get184, Xx_ndbg_items_xx_Get187, Or_nil_Param384), f_u_t_or_nil(Or_nil_Param384, LResult190))),
			    get_var(LEnv,
				    u_xx_ndbg_interests_xx,
				    Xx_ndbg_interests_xx_Get198),
			    cl_assoc(u_rule,
				     Xx_ndbg_interests_xx_Get198,
				     [],
				     Cdr_Param385),
			    cl_cdr(Cdr_Param385, Cdr_Ret396),
			    cl_some(Lambda191, Cdr_Ret396, Or_nil_Param386),
			    f_u_t_or_nil(Or_nil_Param386, Or_nil_Ret397),
			    IFTEST150=Or_nil_Ret397
			)
		    ;   IFTEST150=[]
		    ),
		    (   IFTEST150\==[]
		    ->  get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get208),
			cl_eql(Xx_gate_dbg_xx_Get208, t, IFTEST203),
			(   IFTEST203\==[]
			->  cl_eval([cons, [quote, u_standard_output], []],
				    Eval_Ret),
			    cl_format(
				      [ Eval_Ret,
					'$ARRAY'([*],
						 claz_base_character,
						 
						 [ #\(~),
						   #\('A'),
						   #\(' '),
						   #\(r),
						   #\(e),
						   #\(d),
						   #\(e),
						   #\(f),
						   #\(i),
						   #\(n),
						   #\(e),
						   #\(d),
						   #\(' ')
						 ]),
					Name_Param
				      ],
				      Format_Ret),
			    ElseResult256=t
			;   get_var(LEnv, u_xx_gate_dbg_xx, IFTEST225),
			    (   IFTEST225\==[]
			    ->  get_var(LEnv,
					u_xx_gate_dbg_xx,
					Xx_gate_dbg_xx_Get234),
				cl_format(
					  [ Xx_gate_dbg_xx_Get234,
					    '$ARRAY'([*],
						     claz_base_character,
						     
						     [ #\(~),
						       #\('A'),
						       #\(' '),
						       #\(r),
						       #\(e),
						       #\(d),
						       #\(e),
						       #\(f),
						       #\(i),
						       #\(n),
						       #\(e),
						       #\(d),
						       #\(' ')
						     ]),
					    Name_Param
					  ],
					  Format_Ret400),
				ElseResult256=t
			    ;   get_var(LEnv, u_else, IFTEST244),
				(   IFTEST244\==[]
				->  ElseResult256=[]
				;   ElseResult256=[]
				)
			    )
			)
		    ;   ElseResult256=[]
		    ),
		    get_var(LEnv, u_xx_gate_dbg_xx, Xx_gate_dbg_xx_Get266),
		    f_u_end_font(Xx_gate_dbg_xx_Get266, TrueResult270),
		    TrueResult346=TrueResult270
		;   TrueResult346=[]
		)
	    ;   (   cl_null(Name_Param, FORM1_Res290),
		    FORM1_Res290\==[],
		    IFTEST277=FORM1_Res290
		->  true
		;   cl_eql(Name_Param, [], Eql_Ret),
		    IFTEST277=Eql_Ret
		),
		(   IFTEST277\==[]
		->  f_u_ob_c36_create_named_empty([], Rule),
		    set_var(LEnv, u_rule, Rule),
		    get_var(LEnv, u_rule, Rule_Get302),
		    get_var(LEnv, u_ruleob, Ruleob_Get304),
		    f_u_ob_c36_concatenate_c33(Rule_Get302,
					       Ruleob_Get304,
					       TrueResult330),
		    _417139636=TrueResult330
		;   f_u_ob_c36_create_named_empty([], Rule377),
		    set_var(LEnv, u_rule, Rule377),
		    get_var(LEnv, u_rule, Rule_Get318),
		    f_u_ob_c36_add_name(Rule_Get318, Name_Param, Add_name_Ret),
		    get_var(LEnv, u_rule, Rule_Get325),
		    get_var(LEnv, u_ruleob, Ruleob_Get327),
		    f_u_ob_c36_concatenate_c33(Rule_Get325,
					       Ruleob_Get327,
					       ElseResult331),
		    _417139636=ElseResult331
		),
		get_var(LEnv, u_rule, Rule_Get336),
		f_u_ob_c36_set(Rule_Get336, u_accessible_c63, t, T),
		get_var(LEnv, u_rule, Rule_Get343),
		f_u_add_rule(Rule_Get343, ElseResult347),
		TrueResult346=ElseResult347
	    ),
	    get_var(LEnv, u_rule, Rule_Get352),
	    f_u_ob_c36_name(Rule_Get352, C36_name_Ret),
	    f_u_check_rule(Rule_Get352, C36_name_Ret, Check_rule_Ret),
	    get_var(LEnv, u_rule, Rule_Get362),
	    f_u_ob_c36_name(Rule_Get362, C36_name_Ret405),
	    MResult=[quote, C36_name_Ret405]
	;   MResult=[quote, u_rule_not_loaded]
	),
	cl_eval(MResult, FnResult).
:- set_opv(f_u_define_rule, classof, claz_macro),
   set_opv(u_define_rule, compile_as, kw_operator),
   set_opv(u_define_rule, function, f_u_define_rule).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:2823 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (if (not (memq? rule *rules*))",
				     1,
				     3011)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:2823 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("               (add-rule rule))", 1, 3054)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:3818 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'possible-unify?',
			    [ob1, ob2],
			    
			    [ '#BQ',
			      
			      [ or,
				['special?', ['#COMMA', ob1]],
				['special?', ['#COMMA', ob2]],
				['var?', ['#COMMA', ob1]],
				['var?', ['#COMMA', ob2]],
				
				[ 'eq?',
				  ['ob$ty', ['#COMMA', ob1]],
				  ['ob$ty', ['#COMMA', ob2]]
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_ob2, u_ob1, f_u_possible_unify_c63].
type=ctx.
var_tracker(u_ob1)=rw{name:u_ob1, p:1, r:0, ret:0, u:0, vars:[Ob1_Param], w:1}.
var_tracker(u_ob2)=rw{name:u_ob2, p:1, r:0, ret:0, u:0, vars:[Ob2_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_possible_unify_c63,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_possible_unify_c63).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_possible_unify_c63,
	      f_u_possible_unify_c63,
	      [u_ob1, u_ob2],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ or,
		    [u_special_c63, ['#COMMA', u_ob1]],
		    [u_special_c63, ['#COMMA', u_ob2]],
		    [u_var_c63, ['#COMMA', u_ob1]],
		    [u_var_c63, ['#COMMA', u_ob2]],
		    
		    [ u_eq_c63,
		      [u_ob_c36_ty, ['#COMMA', u_ob1]],
		      [u_ob_c36_ty, ['#COMMA', u_ob2]]
		    ]
		  ]
		]
	      ]).


% asserting... u 
f_u_possible_unify_c63(Ob1_Param, Ob2_Param, FnResult) :-
	[or, [u_special_c63, Ob1_Param], [u_special_c63, Ob2_Param], [u_var_c63, Ob1_Param], [u_var_c63, Ob2_Param], [u_eq_c63, [u_ob_c36_ty, Ob1_Param], [u_ob_c36_ty, Ob2_Param]]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_possible_unify_c63, classof, claz_macro),
   set_opv(u_possible_unify_c63, compile_as, kw_operator),
   set_opv(u_possible_unify_c63, function, f_u_possible_unify_c63).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:3980 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ri-pathelt-rule',
			    [x],
			    ['#BQ', [car, ['#COMMA', x]]]
			  ]).
/* 
alphas=[u_x, f_u_ri_pathelt_rule].
type=ctx.
var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[X_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_ri_pathelt_rule,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_ri_pathelt_rule).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_ri_pathelt_rule,
	      f_u_ri_pathelt_rule,
	      [u_x],
	      [progn, ['#BQ', [car, ['#COMMA', u_x]]]]).


% asserting... u 
f_u_ri_pathelt_rule(X_Param, FnResult) :-
	[car, X_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_ri_pathelt_rule, classof, claz_macro),
   set_opv(u_ri_pathelt_rule, compile_as, kw_operator),
   set_opv(u_ri_pathelt_rule, function, f_u_ri_pathelt_rule).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4024 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ri-pathelt-subgoalnum',
			    [x],
			    ['#BQ', [cadr, ['#COMMA', x]]]
			  ]).
/* 
alphas=[u_x, f_u_ri_pathelt_subgoalnum].
type=ctx.
var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[X_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_ri_pathelt_subgoalnum,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_ri_pathelt_subgoalnum).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_ri_pathelt_subgoalnum,
	      f_u_ri_pathelt_subgoalnum,
	      [u_x],
	      [progn, ['#BQ', [cadr, ['#COMMA', u_x]]]]).


% asserting... u 
f_u_ri_pathelt_subgoalnum(X_Param, FnResult) :-
	[cadr, X_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_ri_pathelt_subgoalnum, classof, claz_macro),
   set_opv(u_ri_pathelt_subgoalnum, compile_as, kw_operator),
   set_opv(u_ri_pathelt_subgoalnum, function, f_u_ri_pathelt_subgoalnum).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4075 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ri-pathelt-episodes',
			    [x],
			    ['#BQ', [cddr, ['#COMMA', x]]]
			  ]).
/* 
alphas=[u_x, f_u_ri_pathelt_episodes].
type=ctx.
var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[X_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_ri_pathelt_episodes,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_ri_pathelt_episodes).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_ri_pathelt_episodes,
	      f_u_ri_pathelt_episodes,
	      [u_x],
	      [progn, ['#BQ', [cddr, ['#COMMA', u_x]]]]).


% asserting... u 
f_u_ri_pathelt_episodes(X_Param, FnResult) :-
	[cddr, X_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_ri_pathelt_episodes, classof, claz_macro),
   set_opv(u_ri_pathelt_episodes, compile_as, kw_operator),
   set_opv(u_ri_pathelt_episodes, function, f_u_ri_pathelt_episodes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4124 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ri-pathelt-make',
			    [rule, subgoalnum, episodes],
			    
			    [ '#BQ',
			      
			      [ cons,
				['#COMMA', rule],
				
				[ cons,
				  ['#COMMA', subgoalnum],
				  ['#COMMA', episodes]
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_episodes, u_subgoalnum, u_rule, f_u_ri_pathelt_make].
type=ctx.
var_tracker(u_episodes)=rw{name:u_episodes, p:1, r:0, ret:0, u:0, vars:[Episodes_Param], w:1}.
var_tracker(u_rule)=rw{name:u_rule, p:1, r:0, ret:0, u:0, vars:[Rule_Param], w:1}.
var_tracker(u_subgoalnum)=rw{name:u_subgoalnum, p:1, r:0, ret:0, u:0, vars:[Subgoalnum_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_ri_pathelt_make,
		[u_rule, u_subgoalnum, u_episodes],
		[Rule_Param, Subgoalnum_Param, Episodes_Param],
		arginfo{ all:[u_rule, u_subgoalnum, u_episodes],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_subgoalnum, u_episodes],
			 opt:0,
			 req:[u_rule, u_subgoalnum, u_episodes],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_ri_pathelt_make).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_ri_pathelt_make,
	      f_u_ri_pathelt_make,
	      [u_rule, u_subgoalnum, u_episodes],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ cons,
		    ['#COMMA', u_rule],
		    [cons, ['#COMMA', u_subgoalnum], ['#COMMA', u_episodes]]
		  ]
		]
	      ]).


% asserting... u 
f_u_ri_pathelt_make(Rule_Param, Subgoalnum_Param, Episodes_Param, FnResult) :-
	[cons, Rule_Param, [cons, Subgoalnum_Param, Episodes_Param]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_ri_pathelt_make, classof, claz_macro),
   set_opv(u_ri_pathelt_make, compile_as, kw_operator),
   set_opv(u_ri_pathelt_make, function, f_u_ri_pathelt_make).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4224 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'chain-rule',
			    [x],
			    ['#BQ', [car, ['#COMMA', x]]]
			  ]).
/* 
alphas=[u_x, f_u_chain_rule].
type=ctx.
var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[X_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_chain_rule,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_chain_rule).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_chain_rule,
	      f_u_chain_rule,
	      [u_x],
	      [progn, ['#BQ', [car, ['#COMMA', u_x]]]]).


% asserting... u 
f_u_chain_rule(X_Param, FnResult) :-
	[car, X_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_chain_rule, classof, claz_macro),
   set_opv(u_chain_rule, compile_as, kw_operator),
   set_opv(u_chain_rule, function, f_u_chain_rule).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4263 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'chain-num',
			    [x],
			    ['#BQ', [cadr, ['#COMMA', x]]]
			  ]).
/* 
alphas=[u_x, f_u_chain_num].
type=ctx.
var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[X_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_chain_num,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_chain_num).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_chain_num,
	      f_u_chain_num,
	      [u_x],
	      [progn, ['#BQ', [cadr, ['#COMMA', u_x]]]]).


% asserting... u 
f_u_chain_num(X_Param, FnResult) :-
	[cadr, X_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_chain_num, classof, claz_macro),
   set_opv(u_chain_num, compile_as, kw_operator),
   set_opv(u_chain_num, function, f_u_chain_num).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4302 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'old-backward-chain-rules',
			    ['goal-obj'],
			    
			    [ '#BQ',
			      
			      [ if,
				
				[ 'ob$get',
				  ['#COMMA', 'goal-obj'],
				  [quote, 'plan-rule']
				],
				
				[ 'ob$gets',
				  
				  [ 'ob$get',
				    ['#COMMA', 'goal-obj'],
				    [quote, 'plan-rule']
				  ],
				  [quote, 'backward-chain']
				],
				'*rules*'
			      ]
			    ]
			  ]).
/* 
alphas=[u_goal_obj, f_u_old_backward_chain_rules].
type=ctx.
var_tracker(u_goal_obj)=rw{name:u_goal_obj, p:1, r:0, ret:0, u:0, vars:[Goal_obj_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_old_backward_chain_rules,
		[u_goal_obj],
		[Goal_obj_Param],
		arginfo{ all:[u_goal_obj],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal_obj],
			 opt:0,
			 req:[u_goal_obj],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_old_backward_chain_rules).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_old_backward_chain_rules,
	      f_u_old_backward_chain_rules,
	      [u_goal_obj],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    [u_ob_c36_get, ['#COMMA', u_goal_obj], [quote, u_plan_rule]],
		    
		    [ u_ob_c36_gets,
		      [u_ob_c36_get, ['#COMMA', u_goal_obj], [quote, u_plan_rule]],
		      [quote, u_backward_chain]
		    ],
		    u_xx_rules_xx
		  ]
		]
	      ]).


% asserting... u 
f_u_old_backward_chain_rules(Goal_obj_Param, FnResult) :-
	[if, [u_ob_c36_get, Goal_obj_Param, [quote, u_plan_rule]], [u_ob_c36_gets, [u_ob_c36_get, Goal_obj_Param, [quote, u_plan_rule]], [quote, u_backward_chain]], u_xx_rules_xx]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_old_backward_chain_rules, classof, claz_macro),
   set_opv(u_old_backward_chain_rules, compile_as, kw_operator),
   set_opv(u_old_backward_chain_rules, function, f_u_old_backward_chain_rules).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4466 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'backward-chain-rules',
			    ['goal-obj'],
			    
			    [ '#BQ',
			      
			      [ if,
				
				[ 'ob$get',
				  ['#COMMA', 'goal-obj'],
				  [quote, 'plan-rule']
				],
				
				[ yloop,
				  
				  [ initial,
				    [result, []],
				    
				    [ subgoalnum,
				      
				      [ 'ob$get',
					['#COMMA', 'goal-obj'],
					[quote, 'plan-subgoalnum']
				      ]
				    ]
				  ],
				  
				  [ yfor,
				    'chain-num',
				    in,
				    
				    [ 'ob$gets',
				      
				      [ 'ob$get',
					['#COMMA', 'goal-obj'],
					[quote, 'plan-rule']
				      ],
				      [quote, 'backward-chain-nums']
				    ]
				  ],
				  
				  [ ydo,
				    
				    [ if,
				      ['eq?', subgoalnum, [cadr, 'chain-num']],
				      
				      [ setq,
					result,
					[cons, [car, 'chain-num'], result]
				      ]
				    ]
				  ],
				  [yresult, result]
				],
				'*rules*'
			      ]
			    ]
			  ]).
/* 
alphas=[u_goal_obj, f_u_backward_chain_rules].
type=ctx.
var_tracker(u_goal_obj)=rw{name:u_goal_obj, p:1, r:0, ret:0, u:0, vars:[Goal_obj_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_backward_chain_rules,
		[u_goal_obj],
		[Goal_obj_Param],
		arginfo{ all:[u_goal_obj],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal_obj],
			 opt:0,
			 req:[u_goal_obj],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_backward_chain_rules).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_backward_chain_rules,
	      f_u_backward_chain_rules,
	      [u_goal_obj],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    [u_ob_c36_get, ['#COMMA', u_goal_obj], [quote, u_plan_rule]],
		    
		    [ u_yloop,
		      
		      [ u_initial,
			[u_result, []],
			
			[ u_subgoalnum,
			  
			  [ u_ob_c36_get,
			    ['#COMMA', u_goal_obj],
			    [quote, u_plan_subgoalnum]
			  ]
			]
		      ],
		      
		      [ u_yfor,
			u_chain_num,
			u_in,
			
			[ u_ob_c36_gets,
			  
			  [ u_ob_c36_get,
			    ['#COMMA', u_goal_obj],
			    [quote, u_plan_rule]
			  ],
			  [quote, u_backward_chain_nums]
			]
		      ],
		      
		      [ u_ydo,
			
			[ if,
			  [u_eq_c63, u_subgoalnum, [cadr, u_chain_num]],
			  [setq, u_result, [cons, [car, u_chain_num], u_result]]
			]
		      ],
		      [u_yresult, u_result]
		    ],
		    u_xx_rules_xx
		  ]
		]
	      ]).


% asserting... u 
f_u_backward_chain_rules(Goal_obj_Param, FnResult) :-
	[if, [u_ob_c36_get, Goal_obj_Param, [quote, u_plan_rule]], [u_yloop, [u_initial, [u_result, []], [u_subgoalnum, [u_ob_c36_get, Goal_obj_Param, [quote, u_plan_subgoalnum]]]], [u_yfor, u_chain_num, u_in, [u_ob_c36_gets, [u_ob_c36_get, Goal_obj_Param, [quote, u_plan_rule]], [quote, u_backward_chain_nums]]], [u_ydo, [if, [u_eq_c63, u_subgoalnum, [cadr, u_chain_num]], [setq, u_result, [cons, [car, u_chain_num], u_result]]]], [u_yresult, u_result]], u_xx_rules_xx]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_backward_chain_rules, classof, claz_macro),
   set_opv(u_backward_chain_rules, compile_as, kw_operator),
   set_opv(u_backward_chain_rules, function, f_u_backward_chain_rules).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:4967 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'forward-chain-rules',
			    ['goal-obj'],
			    
			    [ '#BQ',
			      
			      [ if,
				
				[ 'ob$get',
				  ['#COMMA', 'goal-obj'],
				  [quote, 'inference-rule']
				],
				
				[ 'ob$gets',
				  
				  [ 'ob$get',
				    ['#COMMA', 'goal-obj'],
				    [quote, 'inference-rule']
				  ],
				  [quote, 'forward-chain']
				],
				'*rules*'
			      ]
			    ]
			  ]).
/* 
alphas=[u_goal_obj, f_u_forward_chain_rules].
type=ctx.
var_tracker(u_goal_obj)=rw{name:u_goal_obj, p:1, r:0, ret:0, u:0, vars:[Goal_obj_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_forward_chain_rules,
		[u_goal_obj],
		[Goal_obj_Param],
		arginfo{ all:[u_goal_obj],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_goal_obj],
			 opt:0,
			 req:[u_goal_obj],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_forward_chain_rules).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_forward_chain_rules,
	      f_u_forward_chain_rules,
	      [u_goal_obj],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    
		    [ u_ob_c36_get,
		      ['#COMMA', u_goal_obj],
		      [quote, u_inference_rule]
		    ],
		    
		    [ u_ob_c36_gets,
		      
		      [ u_ob_c36_get,
			['#COMMA', u_goal_obj],
			[quote, u_inference_rule]
		      ],
		      [quote, u_forward_chain]
		    ],
		    u_xx_rules_xx
		  ]
		]
	      ]).


% asserting... u 
f_u_forward_chain_rules(Goal_obj_Param, FnResult) :-
	[if, [u_ob_c36_get, Goal_obj_Param, [quote, u_inference_rule]], [u_ob_c36_gets, [u_ob_c36_get, Goal_obj_Param, [quote, u_inference_rule]], [quote, u_forward_chain]], u_xx_rules_xx]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_forward_chain_rules, classof, claz_macro),
   set_opv(u_forward_chain_rules, compile_as, kw_operator),
   set_opv(u_forward_chain_rules, function, f_u_forward_chain_rules).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5135 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'define-gen',
			    [type, args|body],
			    
			    [ '#BQ',
			      
			      [ let,
				[[ty, ['ob$name->ob', [quote, ['#COMMA', type]]]]],
				
				[ if,
				  ['null?', ty],
				  
				  [ format,
				    t,
				    '$STRING'("define-gen: unknown type: ~A~%"),
				    [quote, ['#COMMA', type]]
				  ],
				  
				  [ 'ob$set',
				    ty,
				    [quote, gen],
				    
				    [ '#COMMA',
				      
				      [ '#BQ',
					
					[ lambda,
					  [con, stream, switches, context, bp],
					  ['#BQ-COMMA-ELIPSE', body]
					]
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_body, args, type, f_u_define_gen].
type=ctx.
var_tracker(args)=rw{name:args, p:1, r:0, ret:0, u:0, vars:[Args_Param], w:1}.
var_tracker(type)=rw{name:type, p:1, r:0, ret:0, u:0, vars:[Type_Param], w:1}.
var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[Body_Param], w:0}.
 */

% asserting1... u 
wl: arglist_info(f_u_define_gen,
		[type, args, '&rest', u_body],
		[Type_Param, Args_Param],
		arginfo{ all:[type, args],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[type, args, u_body],
			 opt:0,
			 req:[type, args],
			 rest:[u_body],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(2, f_u_define_gen).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_define_gen,
	      f_u_define_gen,
	      [type, args|u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    [[u_ty, [u_ob_c36_name_c62_ob, [quote, ['#COMMA', type]]]]],
		    
		    [ if,
		      [u_null_c63, u_ty],
		      
		      [ format,
			t,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(d),
				   #\(e),
				   #\(f),
				   #\(i),
				   #\(n),
				   #\(e),
				   #\(-),
				   #\(g),
				   #\(e),
				   #\(n),
				   #\(:),
				   #\(' '),
				   #\(u),
				   #\(n),
				   #\(k),
				   #\(n),
				   #\(o),
				   #\(w),
				   #\(n),
				   #\(' '),
				   #\(t),
				   #\(y),
				   #\(p),
				   #\(e),
				   #\(:),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(~),
				   #\('%')
				 ]),
			[quote, ['#COMMA', type]]
		      ],
		      
		      [ u_ob_c36_set,
			u_ty,
			[quote, u_gen],
			
			[ '#COMMA',
			  
			  [ '#BQ',
			    
			    [ lambda,
			      [u_con, stream, u_switches, u_context, u_bp],
			      ['#BQ-COMMA-ELIPSE', u_body]
			    ]
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% asserting... u 
f_u_define_gen(Type_Param, Args_Param, Body_Param, FnResult) :-
	Env=[bv(type, Type_Param), bv(args, Args_Param), bv(u_body, Body_Param)],
	get_var(Env, u_body, Body_Get),
	[let, [[u_ty, [u_ob_c36_name_c62_ob, [quote, Type_Param]]]], [if, [u_null_c63, u_ty], [format, t, '$ARRAY'([*], claz_base_character, [#\(d), #\(e), #\(f), #\(i), #\(n), #\(e), #\(-), #\(g), #\(e), #\(n), #\(:), #\(' '), #\(u), #\(n), #\(k), #\(n), #\(o), #\(w), #\(n), #\(' '), #\(t), #\(y), #\(p), #\(e), #\(:), #\(' '), #\(~), #\('A'), #\(~), #\('%')]), [quote, Type_Param]], [u_ob_c36_set, u_ty, [quote, u_gen], [lambda, [u_con, stream, u_switches, u_context, u_bp]|Body_Get]]]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_define_gen, classof, claz_macro),
   set_opv(u_define_gen, compile_as, kw_operator),
   set_opv(u_define_gen, function, f_u_define_gen).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5396 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'define-no-gen',
			    [type],
			    
			    [ '#BQ',
			      
			      [ 'ob$set',
				['ob$name->ob', [quote, ['#COMMA', type]]],
				[quote, gen],
				[quote, 'no-gen']
			      ]
			    ]
			  ]).
/* 
alphas=[type, f_u_define_no_gen].
type=ctx.
var_tracker(type)=rw{name:type, p:1, r:0, ret:0, u:0, vars:[Type_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_define_no_gen,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_define_no_gen).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_define_no_gen,
	      f_u_define_no_gen,
	      [type],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_ob_c36_set,
		    [u_ob_c36_name_c62_ob, [quote, ['#COMMA', type]]],
		    [quote, u_gen],
		    [quote, u_no_gen]
		  ]
		]
	      ]).


% asserting... u 
f_u_define_no_gen(Type_Param, FnResult) :-
	[u_ob_c36_set, [u_ob_c36_name_c62_ob, [quote, Type_Param]], [quote, u_gen], [quote, u_no_gen]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_define_no_gen, classof, claz_macro),
   set_opv(u_define_no_gen, compile_as, kw_operator),
   set_opv(u_define_no_gen, function, f_u_define_no_gen).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5499 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    strength,
			    [ob],
			    
			    [ '#BQ',
			      
			      [ let,
				
				[ 
				  [ found,
				    ['ob$get', ['#COMMA', ob], [quote, strength]]
				  ]
				],
				
				[ if,
				  ['flonum?', found],
				  found,
				  '*default-strength*'
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_ob, f_u_strength].
type=ctx.
var_tracker(u_ob)=rw{name:u_ob, p:1, r:0, ret:0, u:0, vars:[Ob_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_strength,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_strength).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_strength,
	      f_u_strength,
	      [u_ob],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    
		    [ 
		      [ u_found,
			[u_ob_c36_get, ['#COMMA', u_ob], [quote, u_strength]]
		      ]
		    ],
		    
		    [ if,
		      [u_flonum_c63, u_found],
		      u_found,
		      u_xx_default_strength_xx
		    ]
		  ]
		]
	      ]).


% asserting... u 
f_u_strength(Ob_Param, FnResult) :-
	[let, [[u_found, [u_ob_c36_get, Ob_Param, [quote, u_strength]]]], [if, [u_flonum_c63, u_found], u_found, u_xx_default_strength_xx]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_strength, classof, claz_macro),
   set_opv(u_strength, compile_as, kw_operator),
   set_opv(u_strength, function, f_u_strength).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5633 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'set-strength',
			    [ob, str],
			    
			    [ '#BQ',
			      
			      [ 'ob$set',
				['#COMMA', ob],
				[quote, strength],
				['#COMMA', str]
			      ]
			    ]
			  ]).
/* 
alphas=[u_str, u_ob, f_u_set_strength].
type=ctx.
var_tracker(u_ob)=rw{name:u_ob, p:1, r:0, ret:0, u:0, vars:[Ob_Param], w:1}.
var_tracker(u_str)=rw{name:u_str, p:1, r:0, ret:0, u:0, vars:[Str_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_set_strength,
		[u_ob, u_str],
		[Ob_Param, Str_Param],
		arginfo{ all:[u_ob, u_str],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_str],
			 opt:0,
			 req:[u_ob, u_str],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_set_strength).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_set_strength,
	      f_u_set_strength,
	      [u_ob, u_str],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_ob_c36_set,
		    ['#COMMA', u_ob],
		    [quote, u_strength],
		    ['#COMMA', u_str]
		  ]
		]
	      ]).


% asserting... u 
f_u_set_strength(Ob_Param, Str_Param, FnResult) :-
	[u_ob_c36_set, Ob_Param, [quote, u_strength], Str_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_set_strength, classof, claz_macro),
   set_opv(u_set_strength, compile_as, kw_operator),
   set_opv(u_set_strength, function, f_u_set_strength).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5633 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Doesn't affect linkages, so how does this offset really work",
				     1,
				     5699)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5633 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" in the long run!?", 1, 5762)).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'offset-strength',
			    [ob, offset],
			    
			    [ '#BQ',
			      
			      [ 'set-strength',
				['#COMMA', ob],
				
				[ 'fl+',
				  ['#COMMA', offset],
				  [strength, ['#COMMA', ob]]
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_offset, u_ob, f_u_offset_strength].
type=ctx.
var_tracker(u_ob)=rw{name:u_ob, p:1, r:0, ret:0, u:0, vars:[Ob_Param], w:1}.
var_tracker(u_offset)=rw{name:u_offset, p:1, r:0, ret:0, u:0, vars:[Offset_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_offset_strength,
		[u_ob, u_offset],
		[Ob_Param, Offset_Param],
		arginfo{ all:[u_ob, u_offset],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_offset],
			 opt:0,
			 req:[u_ob, u_offset],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_offset_strength).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_offset_strength,
	      f_u_offset_strength,
	      [u_ob, u_offset],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_set_strength,
		    ['#COMMA', u_ob],
		    
		    [ u_fl_c43,
		      ['#COMMA', u_offset],
		      [u_strength, ['#COMMA', u_ob]]
		    ]
		  ]
		]
	      ]).


% asserting... u 
f_u_offset_strength(Ob_Param, Offset_Param, FnResult) :-
	[u_set_strength, Ob_Param, [u_fl_c43, Offset_Param, [u_strength, Ob_Param]]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_offset_strength, classof, claz_macro),
   set_opv(u_offset_strength, compile_as, kw_operator),
   set_opv(u_offset_strength, function, f_u_offset_strength).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:5872 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'delay-dbgs',
			    [context|body],
			    
			    [ '#BQ',
			      
			      [ let,
				
				[ [string1, []],
				  [xxcontext, ['#COMMA', context]],
				  [temp, []]
				],
				
				[ if,
				  '*linearized?*',
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Debugging being delayed for broadcast at a later time.")
				  ]
				],
				
				[ setq,
				  string1,
				  
				  [ 'with-output-to-string',
				    [stream1],
				    
				    [ let,
				      
				      [ ['old-gate-dbg', '*gate-dbg*'],
					['old-gen-stream', '*gen-stream*']
				      ],
				      
				      [ 'unwind-protect',
					
					[ progn,
					  [setq, '*gate-dbg*', stream1],
					  
					  [ setq,
					    '*gen-stream*',
					    ['make-gen-stream', '*gate-dbg*']
					  ],
					  
					  [ setq,
					    temp,
					    [progn, ['#BQ-COMMA-ELIPSE', body]]
					  ]
					],
					[setq, '*gate-dbg*', 'old-gate-dbg'],
					[setq, '*gen-stream*', 'old-gen-stream']
				      ]
				    ]
				  ]
				],
				
				[ 'ob$set',
				  xxcontext,
				  [quote, 'sprout-trace'],
				  [list, string1]
				],
				
				[ if,
				  '*linearized?*',
				  
				  [ 'ndbg-roman-nl',
				    '*gate-dbg*',
				    rule,
				    '$STRING'("Debugging resumed.")
				  ]
				],
				
				[ if,
				  [not, '*linearized?*'],
				  ['cx$print-sprout-trace', xxcontext]
				],
				temp
			      ]
			    ]
			  ]).
/* 
alphas=[u_body, u_context, f_u_delay_dbgs].
type=ctx.
var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[Body_Param], w:0}.
var_tracker(u_context)=rw{name:u_context, p:1, r:0, ret:0, u:0, vars:[Context_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_delay_dbgs,
		[u_context, '&rest', u_body],
		[Context_Param],
		arginfo{ all:[u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_context, u_body],
			 opt:0,
			 req:[u_context],
			 rest:[u_body],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(1, f_u_delay_dbgs).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_delay_dbgs,
	      f_u_delay_dbgs,
	      [u_context|u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    
		    [ [u_string1, []],
		      [u_xxcontext, ['#COMMA', u_context]],
		      [u_temp, []]
		    ],
		    
		    [ if,
		      u_xx_linearized_c63_xx,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('D'),
				   #\(e),
				   #\(b),
				   #\(u),
				   #\(g),
				   #\(g),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(b),
				   #\(e),
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
				   #\(f),
				   #\(o),
				   #\(r),
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
				   #\(' '),
				   #\(a),
				   #\(t),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(l),
				   #\(a),
				   #\(t),
				   #\(e),
				   #\(r),
				   #\(' '),
				   #\(t),
				   #\(i),
				   #\(m),
				   #\(e),
				   #\('.')
				 ])
		      ]
		    ],
		    
		    [ setq,
		      u_string1,
		      
		      [ with_output_to_string,
			[u_stream1],
			
			[ let,
			  
			  [ [u_old_gate_dbg, u_xx_gate_dbg_xx],
			    [u_old_gen_stream, u_xx_gen_stream_xx]
			  ],
			  
			  [ unwind_protect,
			    
			    [ progn,
			      [setq, u_xx_gate_dbg_xx, u_stream1],
			      
			      [ setq,
				u_xx_gen_stream_xx,
				[u_make_gen_stream, u_xx_gate_dbg_xx]
			      ],
			      
			      [ setq,
				u_temp,
				[progn, ['#BQ-COMMA-ELIPSE', u_body]]
			      ]
			    ],
			    [setq, u_xx_gate_dbg_xx, u_old_gate_dbg],
			    [setq, u_xx_gen_stream_xx, u_old_gen_stream]
			  ]
			]
		      ]
		    ],
		    
		    [ u_ob_c36_set,
		      u_xxcontext,
		      [quote, u_sprout_trace],
		      [list, u_string1]
		    ],
		    
		    [ if,
		      u_xx_linearized_c63_xx,
		      
		      [ u_ndbg_roman_nl,
			u_xx_gate_dbg_xx,
			u_rule,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('D'),
				   #\(e),
				   #\(b),
				   #\(u),
				   #\(g),
				   #\(g),
				   #\(i),
				   #\(n),
				   #\(g),
				   #\(' '),
				   #\(r),
				   #\(e),
				   #\(s),
				   #\(u),
				   #\(m),
				   #\(e),
				   #\(d),
				   #\('.')
				 ])
		      ]
		    ],
		    
		    [ if,
		      [not, u_xx_linearized_c63_xx],
		      [u_cx_c36_print_sprout_trace, u_xxcontext]
		    ],
		    u_temp
		  ]
		]
	      ]).


% asserting... u 
f_u_delay_dbgs(Context_Param, Body_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_body, Body_Param)],
	get_var(Env, u_body, Body_Get),
	[let, [[u_string1, []], [u_xxcontext, Context_Param], [u_temp, []]], [if, u_xx_linearized_c63_xx, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, [#\('D'), #\(e), #\(b), #\(u), #\(g), #\(g), #\(i), #\(n), #\(g), #\(' '), #\(b), #\(e), #\(i), #\(n), #\(g), #\(' '), #\(d), #\(e), #\(l), #\(a), #\(y), #\(e), #\(d), #\(' '), #\(f), #\(o), #\(r), #\(' '), #\(b), #\(r), #\(o), #\(a), #\(d), #\(c), #\(a), #\(s), #\(t), #\(' '), #\(a), #\(t), #\(' '), #\(a), #\(' '), #\(l), #\(a), #\(t), #\(e), #\(r), #\(' '), #\(t), #\(i), #\(m), #\(e), #\('.')])]], [setq, u_string1, [with_output_to_string, [u_stream1], [let, [[u_old_gate_dbg, u_xx_gate_dbg_xx], [u_old_gen_stream, u_xx_gen_stream_xx]], [unwind_protect, [progn, [setq, u_xx_gate_dbg_xx, u_stream1], [setq, u_xx_gen_stream_xx, [u_make_gen_stream, u_xx_gate_dbg_xx]], [setq, u_temp, [progn|Body_Get]]], [setq, u_xx_gate_dbg_xx, u_old_gate_dbg], [setq, u_xx_gen_stream_xx, u_old_gen_stream]]]]], [u_ob_c36_set, u_xxcontext, [quote, u_sprout_trace], [list, u_string1]], [if, u_xx_linearized_c63_xx, [u_ndbg_roman_nl, u_xx_gate_dbg_xx, u_rule, '$ARRAY'([*], claz_base_character, [#\('D'), #\(e), #\(b), #\(u), #\(g), #\(g), #\(i), #\(n), #\(g), #\(' '), #\(r), #\(e), #\(s), #\(u), #\(m), #\(e), #\(d), #\('.')])]], [if, [not, u_xx_linearized_c63_xx], [u_cx_c36_print_sprout_trace, u_xxcontext]], u_temp]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_delay_dbgs, classof, claz_macro),
   set_opv(u_delay_dbgs, compile_as, kw_operator),
   set_opv(u_delay_dbgs, function, f_u_delay_dbgs).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:6717 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'no-gen',
			    ['&rest', body],
			    
			    [ '#BQ',
			      
			      [ let,
				[[old, '*global-switches*'], [temp, []]],
				
				[ setq,
				  '*global-switches*',
				  
				  [ cons,
				    [quote, ['no-gen', t]],
				    '*global-switches*'
				  ]
				],
				[setq, temp, [progn, ['#BQ-COMMA-ELIPSE', body]]],
				[setq, '*global-switches*', old],
				temp
			      ]
			    ]
			  ]).
/* 
alphas=[u_body, f_u_no_gen].
type=ctx.
var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[Body_Param], w:0}.
 */

% asserting1... u 
wl: arglist_info(f_u_no_gen,
		[c38_rest, u_body],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(rest_only, f_u_no_gen).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_no_gen,
	      f_u_no_gen,
	      [c38_rest, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    [[u_old, u_xx_global_switches_xx], [u_temp, []]],
		    
		    [ setq,
		      u_xx_global_switches_xx,
		      [cons, [quote, [u_no_gen, t]], u_xx_global_switches_xx]
		    ],
		    [setq, u_temp, [progn, ['#BQ-COMMA-ELIPSE', u_body]]],
		    [setq, u_xx_global_switches_xx, u_old],
		    u_temp
		  ]
		]
	      ]).


% asserting... u 
f_u_no_gen(Whole, FnResult) :-
	Env=[bv(u_body, Body_Param)],
	append([], Body_Param, Whole),
	get_var(Env, u_body, Body_Get),
	[let, [[u_old, u_xx_global_switches_xx], [u_temp, []]], [setq, u_xx_global_switches_xx, [cons, [quote, [u_no_gen, t]], u_xx_global_switches_xx]], [setq, u_temp, [progn|Body_Get]], [setq, u_xx_global_switches_xx, u_old], u_temp]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_no_gen, classof, claz_macro),
   set_opv(u_no_gen, compile_as, kw_operator),
   set_opv(u_no_gen, function, f_u_no_gen).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:6947 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'gen-future-assumption',
			    ['&rest', body],
			    
			    [ '#BQ',
			      
			      [ let,
				[[old, '*global-switches*'], [temp, []]],
				
				[ setq,
				  '*global-switches*',
				  
				  [ cons,
				    [quote, [tense, 'past-subjunctive']],
				    
				    [ cons,
				      [quote, ['what-if', t]],
				      '*global-switches*'
				    ]
				  ]
				],
				[setq, temp, [progn, ['#BQ-COMMA-ELIPSE', body]]],
				[setq, '*global-switches*', old],
				temp
			      ]
			    ]
			  ]).
/* 
alphas=[u_body, f_u_gen_future_assumption].
type=ctx.
var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[Body_Param], w:0}.
 */

% asserting1... u 
wl: arglist_info(f_u_gen_future_assumption,
		[c38_rest, u_body],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(rest_only, f_u_gen_future_assumption).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_gen_future_assumption,
	      f_u_gen_future_assumption,
	      [c38_rest, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    [[u_old, u_xx_global_switches_xx], [u_temp, []]],
		    
		    [ setq,
		      u_xx_global_switches_xx,
		      
		      [ cons,
			[quote, [u_tense, u_past_subjunctive]],
			[cons, [quote, [u_what_if, t]], u_xx_global_switches_xx]
		      ]
		    ],
		    [setq, u_temp, [progn, ['#BQ-COMMA-ELIPSE', u_body]]],
		    [setq, u_xx_global_switches_xx, u_old],
		    u_temp
		  ]
		]
	      ]).


% asserting... u 
f_u_gen_future_assumption(Whole, FnResult) :-
	Env=[bv(u_body, Body_Param)],
	append([], Body_Param, Whole),
	get_var(Env, u_body, Body_Get),
	[let, [[u_old, u_xx_global_switches_xx], [u_temp, []]], [setq, u_xx_global_switches_xx, [cons, [quote, [u_tense, u_past_subjunctive]], [cons, [quote, [u_what_if, t]], u_xx_global_switches_xx]]], [setq, u_temp, [progn|Body_Get]], [setq, u_xx_global_switches_xx, u_old], u_temp]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_gen_future_assumption, classof, claz_macro),
   set_opv(u_gen_future_assumption, compile_as, kw_operator),
   set_opv(u_gen_future_assumption, function, f_u_gen_future_assumption).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:7252 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'gen-past-assumption',
			    ['&rest', body],
			    
			    [ '#BQ',
			      
			      [ let,
				[[old, '*global-switches*'], [temp, []]],
				
				[ setq,
				  '*global-switches*',
				  
				  [ cons,
				    [quote, [tense, 'past-perfect']],
				    
				    [ cons,
				      [quote, ['what-if', t]],
				      '*global-switches*'
				    ]
				  ]
				],
				[setq, temp, [progn, ['#BQ-COMMA-ELIPSE', body]]],
				[setq, '*global-switches*', old],
				temp
			      ]
			    ]
			  ]).
/* 
alphas=[u_body, f_u_gen_past_assumption].
type=ctx.
var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[Body_Param], w:0}.
 */

% asserting1... u 
wl: arglist_info(f_u_gen_past_assumption,
		[c38_rest, u_body],
		[],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_body],
			 opt:0,
			 req:0,
			 rest:[u_body],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(rest_only, f_u_gen_past_assumption).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_gen_past_assumption,
	      f_u_gen_past_assumption,
	      [c38_rest, u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    [[u_old, u_xx_global_switches_xx], [u_temp, []]],
		    
		    [ setq,
		      u_xx_global_switches_xx,
		      
		      [ cons,
			[quote, [u_tense, u_past_perfect]],
			[cons, [quote, [u_what_if, t]], u_xx_global_switches_xx]
		      ]
		    ],
		    [setq, u_temp, [progn, ['#BQ-COMMA-ELIPSE', u_body]]],
		    [setq, u_xx_global_switches_xx, u_old],
		    u_temp
		  ]
		]
	      ]).


% asserting... u 
f_u_gen_past_assumption(Whole, FnResult) :-
	Env=[bv(u_body, Body_Param)],
	append([], Body_Param, Whole),
	get_var(Env, u_body, Body_Get),
	[let, [[u_old, u_xx_global_switches_xx], [u_temp, []]], [setq, u_xx_global_switches_xx, [cons, [quote, [u_tense, u_past_perfect]], [cons, [quote, [u_what_if, t]], u_xx_global_switches_xx]]], [setq, u_temp, [progn|Body_Get]], [setq, u_xx_global_switches_xx, u_old], u_temp]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_gen_past_assumption, classof, claz_macro),
   set_opv(u_gen_past_assumption, compile_as, kw_operator),
   set_opv(u_gen_past_assumption, function, f_u_gen_past_assumption).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:7551 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'gen-relaxation',
			    [context|body],
			    
			    [ '#BQ',
			      
			      [ let,
				[[old, '*global-switches*'], [temp, []]],
				
				[ setq,
				  '*global-switches*',
				  
				  [ cons,
				    
				    [ if,
				      ['altern?', ['#COMMA', context]],
				      [quote, [tense, 'past-subjunctive']],
				      [quote, [tense, present]]
				    ],
				    
				    [ cons,
				      [quote, [relaxation, t]],
				      '*global-switches*'
				    ]
				  ]
				],
				[setq, temp, [progn, ['#BQ-COMMA-ELIPSE', body]]],
				[setq, '*global-switches*', old],
				temp
			      ]
			    ]
			  ]).
/* 
alphas=[u_body, u_context, f_u_gen_relaxation].
type=ctx.
var_tracker(u_body)=rw{name:u_body, p:0, r:0, ret:0, u:0, vars:[Body_Param], w:0}.
var_tracker(u_context)=rw{name:u_context, p:1, r:0, ret:0, u:0, vars:[Context_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_gen_relaxation,
		[u_context, '&rest', u_body],
		[Context_Param],
		arginfo{ all:[u_context],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_context, u_body],
			 opt:0,
			 req:[u_context],
			 rest:[u_body],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(1, f_u_gen_relaxation).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_gen_relaxation,
	      f_u_gen_relaxation,
	      [u_context|u_body],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ let,
		    [[u_old, u_xx_global_switches_xx], [u_temp, []]],
		    
		    [ setq,
		      u_xx_global_switches_xx,
		      
		      [ cons,
			
			[ if,
			  [u_altern_c63, ['#COMMA', u_context]],
			  [quote, [u_tense, u_past_subjunctive]],
			  [quote, [u_tense, u_present]]
			],
			
			[ cons,
			  [quote, [u_relaxation, t]],
			  u_xx_global_switches_xx
			]
		      ]
		    ],
		    [setq, u_temp, [progn, ['#BQ-COMMA-ELIPSE', u_body]]],
		    [setq, u_xx_global_switches_xx, u_old],
		    u_temp
		  ]
		]
	      ]).


% asserting... u 
f_u_gen_relaxation(Context_Param, Body_Param, FnResult) :-
	Env=[bv(u_context, Context_Param), bv(u_body, Body_Param)],
	get_var(Env, u_body, Body_Get),
	[let, [[u_old, u_xx_global_switches_xx], [u_temp, []]], [setq, u_xx_global_switches_xx, [cons, [if, [u_altern_c63, Context_Param], [quote, [u_tense, u_past_subjunctive]], [quote, [u_tense, u_present]]], [cons, [quote, [u_relaxation, t]], u_xx_global_switches_xx]]], [setq, u_temp, [progn|Body_Get]], [setq, u_xx_global_switches_xx, u_old], u_temp]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_gen_relaxation, classof, claz_macro),
   set_opv(u_gen_relaxation, compile_as, kw_operator),
   set_opv(u_gen_relaxation, function, f_u_gen_relaxation).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:7937 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'me-belief-path?',
			    [x],
			    ['#BQ', ['null?', [cdr, ['#COMMA', x]]]]
			  ]).
/* 
alphas=[u_x, f_u_me_belief_path_c63].
type=ctx.
var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[X_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_me_belief_path_c63,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_me_belief_path_c63).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_me_belief_path_c63,
	      f_u_me_belief_path_c63,
	      [u_x],
	      [progn, ['#BQ', [u_null_c63, [cdr, ['#COMMA', u_x]]]]]).


% asserting... u 
f_u_me_belief_path_c63(X_Param, FnResult) :-
	[u_null_c63, [cdr, X_Param]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_me_belief_path_c63, classof, claz_macro),
   set_opv(u_me_belief_path_c63, compile_as, kw_operator),
   set_opv(u_me_belief_path_c63, function, f_u_me_belief_path_c63).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:7989 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'not-me-belief-path?',
			    [x],
			    ['#BQ', [cdr, ['#COMMA', x]]]
			  ]).
/* 
alphas=[u_x, f_u_not_me_belief_path_c63].
type=ctx.
var_tracker(u_x)=rw{name:u_x, p:1, r:0, ret:0, u:0, vars:[X_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_not_me_belief_path_c63,
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
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_not_me_belief_path_c63).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_not_me_belief_path_c63,
	      f_u_not_me_belief_path_c63,
	      [u_x],
	      [progn, ['#BQ', [cdr, ['#COMMA', u_x]]]]).


% asserting... u 
f_u_not_me_belief_path_c63(X_Param, FnResult) :-
	[cdr, X_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_not_me_belief_path_c63, classof, claz_macro),
   set_opv(u_not_me_belief_path_c63, compile_as, kw_operator),
   set_opv(u_not_me_belief_path_c63, function, f_u_not_me_belief_path_c63).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8037 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'define-phrase',
			    [pattern|concepts],
			    
			    [ '#BQ',
			      
			      [ progn,
				
				[ setq,
				  '*phrases*',
				  
				  [ cons,
				    
				    [ cons,
				      ['#COMMA', pattern],
				      
				      [ quote,
					
					[ '#COMMA',
					  
					  [ map,
					    [quote, list],
					    [lambda, [x], ['ob$create', x]],
					    concepts
					  ]
					]
				      ]
				    ],
				    '*phrases*'
				  ]
				],
				[]
			      ]
			    ]
			  ]).
/* 
alphas=[u_concepts, u_pattern, f_u_define_phrase].
type=ctx.
var_tracker(u_concepts)=rw{name:u_concepts, p:0, r:0, ret:0, u:0, vars:[Concepts_Param], w:0}.
var_tracker(u_pattern)=rw{name:u_pattern, p:1, r:0, ret:0, u:0, vars:[Pattern_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_define_phrase,
		[u_pattern, '&rest', u_concepts],
		[Pattern_Param],
		arginfo{ all:[u_pattern],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_pattern, u_concepts],
			 opt:0,
			 req:[u_pattern],
			 rest:[u_concepts],
			 whole:0
		       }).


% asserting1... u 
wl: init_args(1, f_u_define_phrase).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_define_phrase,
	      f_u_define_phrase,
	      [u_pattern|u_concepts],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ progn,
		    
		    [ setq,
		      u_xx_phrases_xx,
		      
		      [ cons,
			
			[ cons,
			  ['#COMMA', u_pattern],
			  
			  [ quote,
			    
			    [ '#COMMA',
			      
			      [ map,
				[quote, list],
				[lambda, [u_x], [u_ob_c36_create, u_x]],
				u_concepts
			      ]
			    ]
			  ]
			],
			u_xx_phrases_xx
		      ]
		    ],
		    []
		  ]
		]
	      ]).


% asserting... u 
f_u_define_phrase(Pattern_Param, Concepts_Param, FnResult) :-
	Env=[bv(u_pattern, Pattern_Param), bv(u_concepts, Concepts_Param)],
	Lambda=closure([Env|Env], LResult, [u_x],  (get_var(Env, u_x, X_Get), f_u_ob_c36_readlist(X_Get, LResult))),
	get_var(Env, u_concepts, Concepts_Get),
	cl_map(list, Lambda, Concepts_Get, Map_Ret),
	[progn, [setq, u_xx_phrases_xx, [cons, [cons, Pattern_Param, [quote, Map_Ret]], u_xx_phrases_xx]], []]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_define_phrase, classof, claz_macro),
   set_opv(u_define_phrase, compile_as, kw_operator),
   set_opv(u_define_phrase, function, f_u_define_phrase).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8265 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'candidate-create',
			    [rule, bd, episodes],
			    
			    [ '#BQ',
			      
			      [ cons,
				['#COMMA', rule],
				[cons, ['#COMMA', bd], ['#COMMA', episodes]]
			      ]
			    ]
			  ]).
/* 
alphas=[u_episodes, u_bd, u_rule, f_u_candidate_create].
type=ctx.
var_tracker(u_bd)=rw{name:u_bd, p:1, r:0, ret:0, u:0, vars:[Bd_Param], w:1}.
var_tracker(u_episodes)=rw{name:u_episodes, p:1, r:0, ret:0, u:0, vars:[Episodes_Param], w:1}.
var_tracker(u_rule)=rw{name:u_rule, p:1, r:0, ret:0, u:0, vars:[Rule_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_candidate_create,
		[u_rule, u_bd, u_episodes],
		[Rule_Param, Bd_Param, Episodes_Param],
		arginfo{ all:[u_rule, u_bd, u_episodes],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_rule, u_bd, u_episodes],
			 opt:0,
			 req:[u_rule, u_bd, u_episodes],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_candidate_create).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_candidate_create,
	      f_u_candidate_create,
	      [u_rule, u_bd, u_episodes],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ cons,
		    ['#COMMA', u_rule],
		    [cons, ['#COMMA', u_bd], ['#COMMA', u_episodes]]
		  ]
		]
	      ]).


% asserting... u 
f_u_candidate_create(Rule_Param, Bd_Param, Episodes_Param, FnResult) :-
	[cons, Rule_Param, [cons, Bd_Param, Episodes_Param]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_candidate_create, classof, claz_macro),
   set_opv(u_candidate_create, compile_as, kw_operator),
   set_opv(u_candidate_create, function, f_u_candidate_create).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8350 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'candidate-rule',
			    [candidate],
			    ['#BQ', [car, ['#COMMA', candidate]]]
			  ]).
/* 
alphas=[u_candidate, f_u_candidate_rule].
type=ctx.
var_tracker(u_candidate)=rw{name:u_candidate, p:1, r:0, ret:0, u:0, vars:[Candidate_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_candidate_rule,
		[u_candidate],
		[Candidate_Param],
		arginfo{ all:[u_candidate],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_candidate],
			 opt:0,
			 req:[u_candidate],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_candidate_rule).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_candidate_rule,
	      f_u_candidate_rule,
	      [u_candidate],
	      [progn, ['#BQ', [car, ['#COMMA', u_candidate]]]]).


% asserting... u 
f_u_candidate_rule(Candidate_Param, FnResult) :-
	[car, Candidate_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_candidate_rule, classof, claz_macro),
   set_opv(u_candidate_rule, compile_as, kw_operator),
   set_opv(u_candidate_rule, function, f_u_candidate_rule).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8409 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'candidate-bd',
			    [candidate],
			    ['#BQ', [cadr, ['#COMMA', candidate]]]
			  ]).
/* 
alphas=[u_candidate, f_u_candidate_bd].
type=ctx.
var_tracker(u_candidate)=rw{name:u_candidate, p:1, r:0, ret:0, u:0, vars:[Candidate_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_candidate_bd,
		[u_candidate],
		[Candidate_Param],
		arginfo{ all:[u_candidate],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_candidate],
			 opt:0,
			 req:[u_candidate],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_candidate_bd).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_candidate_bd,
	      f_u_candidate_bd,
	      [u_candidate],
	      [progn, ['#BQ', [cadr, ['#COMMA', u_candidate]]]]).


% asserting... u 
f_u_candidate_bd(Candidate_Param, FnResult) :-
	[cadr, Candidate_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_candidate_bd, classof, claz_macro),
   set_opv(u_candidate_bd, compile_as, kw_operator),
   set_opv(u_candidate_bd, function, f_u_candidate_bd).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8467 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'candidate-episodes',
			    [candidate],
			    ['#BQ', [cddr, ['#COMMA', candidate]]]
			  ]).
/* 
alphas=[u_candidate, f_u_candidate_episodes].
type=ctx.
var_tracker(u_candidate)=rw{name:u_candidate, p:1, r:0, ret:0, u:0, vars:[Candidate_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_candidate_episodes,
		[u_candidate],
		[Candidate_Param],
		arginfo{ all:[u_candidate],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_candidate],
			 opt:0,
			 req:[u_candidate],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_candidate_episodes).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_candidate_episodes,
	      f_u_candidate_episodes,
	      [u_candidate],
	      [progn, ['#BQ', [cddr, ['#COMMA', u_candidate]]]]).


% asserting... u 
f_u_candidate_episodes(Candidate_Param, FnResult) :-
	[cddr, Candidate_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_candidate_episodes, classof, claz_macro),
   set_opv(u_candidate_episodes, compile_as, kw_operator),
   set_opv(u_candidate_episodes, function, f_u_candidate_episodes).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8531 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    thresh,
			    [value, threshold],
			    
			    [ '#BQ',
			      
			      [ if,
				['fl<', ['#COMMA', value], ['#COMMA', threshold]],
				0.0,
				['#COMMA', value]
			      ]
			    ]
			  ]).
/* 
alphas=[u_threshold, u_value, f_u_thresh].
type=ctx.
var_tracker(u_threshold)=rw{name:u_threshold, p:1, r:0, ret:0, u:0, vars:[Threshold_Param], w:1}.
var_tracker(u_value)=rw{name:u_value, p:1, r:0, ret:0, u:0, vars:[Value_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_thresh,
		[u_value, u_threshold],
		[Value_Param, Threshold_Param],
		arginfo{ all:[u_value, u_threshold],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_value, u_threshold],
			 opt:0,
			 req:[u_value, u_threshold],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_thresh).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_thresh,
	      f_u_thresh,
	      [u_value, u_threshold],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ if,
		    [u_fl_c60, ['#COMMA', u_value], ['#COMMA', u_threshold]],
		    0.0,
		    ['#COMMA', u_value]
		  ]
		]
	      ]).


% asserting... u 
f_u_thresh(Value_Param, Threshold_Param, FnResult) :-
	[if, [u_fl_c60, Value_Param, Threshold_Param], 0.0, Value_Param]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_thresh, classof, claz_macro),
   set_opv(u_thresh, compile_as, kw_operator),
   set_opv(u_thresh, function, f_u_thresh).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8625 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'bd-append',
			    [bd1, bd2],
			    
			    [ '#BQ',
			      
			      [ cons,
				[quote, t],
				
				[ append,
				  [cdr, ['#COMMA', bd1]],
				  [cdr, ['#COMMA', bd2]]
				]
			      ]
			    ]
			  ]).
/* 
alphas=[u_bd2, u_bd1, f_u_bd_append].
type=ctx.
var_tracker(u_bd1)=rw{name:u_bd1, p:1, r:0, ret:0, u:0, vars:[Bd1_Param], w:1}.
var_tracker(u_bd2)=rw{name:u_bd2, p:1, r:0, ret:0, u:0, vars:[Bd2_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_bd_append,
		[u_bd1, u_bd2],
		[Bd1_Param, Bd2_Param],
		arginfo{ all:[u_bd1, u_bd2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd1, u_bd2],
			 opt:0,
			 req:[u_bd1, u_bd2],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_bd_append).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_bd_append,
	      f_u_bd_append,
	      [u_bd1, u_bd2],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ cons,
		    [quote, t],
		    [append, [cdr, ['#COMMA', u_bd1]], [cdr, ['#COMMA', u_bd2]]]
		  ]
		]
	      ]).


% asserting... u 
f_u_bd_append(Bd1_Param, Bd2_Param, FnResult) :-
	[cons, [quote, t], [append, [cdr, Bd1_Param], [cdr, Bd2_Param]]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_bd_append, classof, claz_macro),
   set_opv(u_bd_append, compile_as, kw_operator),
   set_opv(u_bd_append, function, f_u_bd_append).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8701 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'bd-append-ai',
			    [bd1, bd2],
			    
			    [ '#BQ',
			      
			      [ yloop,
				
				[ initial,
				  
				  [ result,
				    
				    [ cons,
				      [quote, t],
				      ['copy-list', [cdr, ['#COMMA', bd1]]]
				    ]
				  ]
				],
				[yfor, elem, in, [cdr, ['#COMMA', bd2]]],
				
				[ ydo,
				  
				  [ if,
				    
				    [ or,
				      ['var?', [cadr, elem]],
				      ['analogy-instantiatible?', [cadr, elem]]
				    ],
				    
				    [ setq,
				      result,
				      ['append!', result, [list, elem]]
				    ]
				  ]
				],
				[yresult, result]
			      ]
			    ]
			  ]).
/* 
alphas=[u_bd2, u_bd1, f_u_bd_append_ai].
type=ctx.
var_tracker(u_bd1)=rw{name:u_bd1, p:1, r:0, ret:0, u:0, vars:[Bd1_Param], w:1}.
var_tracker(u_bd2)=rw{name:u_bd2, p:1, r:0, ret:0, u:0, vars:[Bd2_Param], w:1}.
 */

% asserting1... u 
wl: arglist_info(f_u_bd_append_ai,
		[u_bd1, u_bd2],
		[Bd1_Param, Bd2_Param],
		arginfo{ all:[u_bd1, u_bd2],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd1, u_bd2],
			 opt:0,
			 req:[u_bd1, u_bd2],
			 rest:0,
			 whole:0
		       }).


% asserting1... u 
wl: init_args(exact_only, f_u_bd_append_ai).


% asserting1... u 
wl: lambda_def(defmacro,
	      u_bd_append_ai,
	      f_u_bd_append_ai,
	      [u_bd1, u_bd2],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_yloop,
		    
		    [ u_initial,
		      
		      [ u_result,
			[cons, [quote, t], [copy_list, [cdr, ['#COMMA', u_bd1]]]]
		      ]
		    ],
		    [u_yfor, u_elem, u_in, [cdr, ['#COMMA', u_bd2]]],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ or,
			  [u_var_c63, [cadr, u_elem]],
			  [u_analogy_instantiatible_c63, [cadr, u_elem]]
			],
			[setq, u_result, [u_append_c33, u_result, [list, u_elem]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% asserting... u 
f_u_bd_append_ai(Bd1_Param, Bd2_Param, FnResult) :-
	[u_yloop, [u_initial, [u_result, [cons, [quote, t], [copy_list, [cdr, Bd1_Param]]]]], [u_yfor, u_elem, u_in, [cdr, Bd2_Param]], [u_ydo, [if, [or, [u_var_c63, [cadr, u_elem]], [u_analogy_instantiatible_c63, [cadr, u_elem]]], [setq, u_result, [u_append_c33, u_result, [list, u_elem]]]]], [u_yresult, u_result]]=MResult,
	cl_eval(MResult, FnResult).
:- set_opv(f_u_bd_append_ai, classof, claz_macro),
   set_opv(u_bd_append_ai, compile_as, kw_operator),
   set_opv(u_bd_append_ai, function, f_u_bd_append_ai).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/dd_macros.cl:8701 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 9031)).


% Total time: 92.45 seconds



% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_instan2" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan2.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:09 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instan-special',
			    
			    [ template,
			      bindings,
			      depth,
			      'omit-slots',
			      'include-slots',
			      substit,
			      abstract,
			      'omit-proc'
			    ],
			    
			    [ cond,
			      
			      [ ['ty$instance?', template, [quote, uor]],
				
				[ 'ob$instantiate2',
				  ['ob$get', template, [quote, obj]],
				  bindings,
				  depth,
				  'omit-slots',
				  'include-slots',
				  substit,
				  abstract,
				  'omit-proc'
				]
			      ],
			      
			      [ ['ty$instance?', template, [quote, uand]],
				
				[ cond,
				  
				  [ 
				    [ any,
				      
				      [ lambda,
					[elem],
					[if, [not, ['ob?', elem]], elem, []]
				      ],
				      ['ob$gets', template, [quote, obj]]
				    ]
				  ],
				  
				  [ else,
				    
				    [ yloop,
				      [initial, [result, []], [found, []]],
				      
				      [ yfor,
					elem,
					in,
					['ob$gets', template, [quote, obj]]
				      ],
				      [yuntil, result],
				      
				      [ ydo,
					
					[ if,
					  
					  [ and,
					    ['var?', elem],
					    
					    [ setq,
					      found,
					      
					      [ 'bd-hyper-lookup',
						['variable-name', elem],
						bindings
					      ]
					    ]
					  ],
					  
					  [ cond,
					    
					    [ ['var?', found],
					      [setq, '*any-unbound?*', t],
					      [setq, result, found]
					    ],
					    
					    [ 
					      [ and,
						['ob?', found],
						['vars-in?', found]
					      ],
					      
					      [ setq,
						result,
						
						[ 'ob$instantiate2',
						  found,
						  bindings,
						  depth,
						  'omit-slots',
						  'include-slots',
						  substit,
						  abstract,
						  'omit-proc'
						]
					      ]
					    ],
					    
					    [ else,
					      
					      [ setq,
						result,
						
						[ 'ob$instantiate2',
						  found,
						  bindings,
						  depth,
						  'omit-slots',
						  'include-slots',
						  substit,
						  abstract,
						  'omit-proc'
						]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ yresult,
					
					[ if,
					  result,
					  result,
					  
					  [ if,
					    
					    [ 'any?',
					      
					      [ lambda,
						[elem],
						
						[ and,
						  ['ob?', elem],
						  [not, ['var?', elem]]
						]
					      ],
					      ['ob$gets', template, [quote, obj]]
					    ],
					    
					    [ let,
					      
					      [ 
						[ 'result-ob',
						  ['ob$create-empty']
						]
					      ],
					      
					      [ setq,
						'*instan-obs*',
						
						[ cons,
						  [cons, template, 'result-ob'],
						  '*instan-obs*'
						]
					      ],
					      
					      [ yloop,
						[initial, [result, []]],
						
						[ yfor,
						  elem,
						  in,
						  
						  [ 'ob$gets',
						    template,
						    [quote, obj]
						  ]
						],
						
						[ ydo,
						  
						  [ if,
						    
						    [ and,
						      ['ob?', elem],
						      [not, ['var?', elem]]
						    ],
						    
						    [ progn,
						      
						      [ setq,
							result,
							
							[ 'ob$instantiate2',
							  elem,
							  bindings,
							  depth,
							  'omit-slots',
							  'include-slots',
							  substit,
							  abstract,
							  'omit-proc'
							]
						      ],
						      
						      [ yloop,
							
							[ yfor,
							  pair,
							  in,
							  ['ob$pairs', result]
							],
							
							[ ydo,
							  
							  [ 'ob$add',
							    'result-ob',
							    ['slots-name', pair],
							    
							    [ 'slots-value',
							      pair
							    ]
							  ]
							]
						      ],
						      
						      [ if,
							['ob?', result],
							['ob$destroy', result],
							
							[ error,
							  '$STRING'("~A not ob to destroy"),
							  result
							]
						      ]
						    ]
						  ]
						]
					      ],
					      'result-ob'
					    ],
					    ['ob$copy', template]
					  ]
					]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', template, [quote, unot]],
				
				[ 'ob$fcreate',
				  
				  [ '#BQ',
				    
				    [ 'UNOT',
				      obj,
				      
				      [ '#COMMA',
					['ob$get', template, [quote, obj]]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', template, [quote, udist]],
				
				[ 'ob$fcreate',
				  
				  [ '#BQ',
				    
				    [ 'UDIST',
				      obj,
				      
				      [ '#COMMA',
					['ob$get', template, [quote, obj]]
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', template, [quote, uproc]],
				[quote, 'uproc-answer-true']
			      ],
			      
			      [ ['ty$instance?', template, [quote, uselect]],
				
				[ let,
				  
				  [ 
				    [ ob,
				      
				      [ 'ob$instantiate2',
					['ob$get', template, [quote, pattern]],
					bindings,
					depth,
					'omit-slots',
					'include-slots',
					substit,
					abstract,
					'omit-proc'
				      ]
				    ]
				  ],
				  
				  [ if,
				    ['ob?', ob],
				    
				    [ 'ob$get',
				      ob,
				      ['ob$get', template, [quote, slot]]
				    ],
				    ob
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', template, [quote, ucode]],
				
				[ let,
				  
				  [ ['old-ob-bindings', '*ob-bindings*'],
				    [result, []]
				  ],
				  [setq, '*ob-bindings*', bindings],
				  
				  [ setq,
				    result,
				    [eval, ['ob$get', template, [quote, proc]]]
				  ],
				  [setq, '*ob-bindings*', 'old-ob-bindings'],
				  result
				]
			      ],
			      
			      [ ['ty$instance?', template, [quote, 'ubind!']],
				
				[ let,
				  
				  [ 
				    [ result,
				      
				      [ 'ob$instantiate2',
					['ob$get', template, [quote, pattern]],
					bindings,
					depth,
					'omit-slots',
					'include-slots',
					substit,
					abstract,
					'omit-proc'
				      ]
				    ]
				  ],
				  
				  [ 'bd-bind!',
				    
				    [ 'variable-name',
				      ['ob$get', template, [quote, var]]
				    ],
				    result,
				    bindings
				  ],
				  result
				]
			      ],
			      
			      [ else,
				
				[ error,
				  '$STRING'("~A unknown special"),
				  templa,
				  isn,
				  [quote, t],
				  a,
				  'copy?',
				  'result-ob'
				],
				['ob$copy', template]
			      ]
			    ]
			  ]).

% annotating U::OB$INSTAN-SPECIAL 
wl: lambda_def(defun,
	      u_ob_c36_instan_special,
	      f_u_ob_c36_instan_special,
	      
	      [ u_template,
		bindings,
		u_depth,
		u_omit_slots,
		u_include_slots,
		u_substit,
		u_abstract,
		u_omit_proc
	      ],
	      
	      [ 
		[ cond,
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_uor]],
		    
		    [ u_ob_c36_instantiate2,
		      [u_ob_c36_get, u_template, [quote, u_obj]],
		      bindings,
		      u_depth,
		      u_omit_slots,
		      u_include_slots,
		      u_substit,
		      u_abstract,
		      u_omit_proc
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_uand]],
		    
		    [ cond,
		      
		      [ 
			[ u_any,
			  
			  [ lambda,
			    [u_elem],
			    [if, [not, [u_ob_c63, u_elem]], u_elem, []]
			  ],
			  [u_ob_c36_gets, u_template, [quote, u_obj]]
			]
		      ],
		      
		      [ u_else,
			
			[ u_yloop,
			  [u_initial, [u_result, []], [u_found, []]],
			  
			  [ u_yfor,
			    u_elem,
			    u_in,
			    [u_ob_c36_gets, u_template, [quote, u_obj]]
			  ],
			  [u_yuntil, u_result],
			  
			  [ u_ydo,
			    
			    [ if,
			      
			      [ and,
				[u_var_c63, u_elem],
				
				[ setq,
				  u_found,
				  
				  [ u_bd_hyper_lookup,
				    [u_variable_name, u_elem],
				    bindings
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ [u_var_c63, u_found],
				  [setq, u_xx_any_unbound_c63_xx, t],
				  [setq, u_result, u_found]
				],
				
				[ 
				  [ and,
				    [u_ob_c63, u_found],
				    [u_vars_in_c63, u_found]
				  ],
				  
				  [ setq,
				    u_result,
				    
				    [ u_ob_c36_instantiate2,
				      u_found,
				      bindings,
				      u_depth,
				      u_omit_slots,
				      u_include_slots,
				      u_substit,
				      u_abstract,
				      u_omit_proc
				    ]
				  ]
				],
				
				[ u_else,
				  
				  [ setq,
				    u_result,
				    
				    [ u_ob_c36_instantiate2,
				      u_found,
				      bindings,
				      u_depth,
				      u_omit_slots,
				      u_include_slots,
				      u_substit,
				      u_abstract,
				      u_omit_proc
				    ]
				  ]
				]
			      ]
			    ]
			  ],
			  
			  [ u_yresult,
			    
			    [ if,
			      u_result,
			      u_result,
			      
			      [ if,
				
				[ u_any_c63,
				  
				  [ lambda,
				    [u_elem],
				    
				    [ and,
				      [u_ob_c63, u_elem],
				      [not, [u_var_c63, u_elem]]
				    ]
				  ],
				  [u_ob_c36_gets, u_template, [quote, u_obj]]
				],
				
				[ let,
				  [[u_result_ob, [u_ob_c36_create_empty]]],
				  
				  [ setq,
				    u_xx_instan_obs_xx,
				    
				    [ cons,
				      [cons, u_template, u_result_ob],
				      u_xx_instan_obs_xx
				    ]
				  ],
				  
				  [ u_yloop,
				    [u_initial, [u_result, []]],
				    
				    [ u_yfor,
				      u_elem,
				      u_in,
				      [u_ob_c36_gets, u_template, [quote, u_obj]]
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ and,
					  [u_ob_c63, u_elem],
					  [not, [u_var_c63, u_elem]]
					],
					
					[ progn,
					  
					  [ setq,
					    u_result,
					    
					    [ u_ob_c36_instantiate2,
					      u_elem,
					      bindings,
					      u_depth,
					      u_omit_slots,
					      u_include_slots,
					      u_substit,
					      u_abstract,
					      u_omit_proc
					    ]
					  ],
					  
					  [ u_yloop,
					    
					    [ u_yfor,
					      u_pair,
					      u_in,
					      [u_ob_c36_pairs, u_result]
					    ],
					    
					    [ u_ydo,
					      
					      [ u_ob_c36_add,
						u_result_ob,
						[u_slots_name, u_pair],
						[u_slots_value, u_pair]
					      ]
					    ]
					  ],
					  
					  [ if,
					    [u_ob_c63, u_result],
					    [u_ob_c36_destroy, u_result],
					    
					    [ error,
					      '$ARRAY'([*],
						       claz_base_character,
						       
						       [ #\(~),
							 #\('A'),
							 #\(' '),
							 #\(n),
							 #\(o),
							 #\(t),
							 #\(' '),
							 #\(o),
							 #\(b),
							 #\(' '),
							 #\(t),
							 #\(o),
							 #\(' '),
							 #\(d),
							 #\(e),
							 #\(s),
							 #\(t),
							 #\(r),
							 #\(o),
							 #\(y)
						       ]),
					      u_result
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  u_result_ob
				],
				[u_ob_c36_copy, u_template]
			      ]
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_unot]],
		    
		    [ u_ob_c36_fcreate,
		      
		      [ '#BQ',
			
			[ u_unot,
			  u_obj,
			  ['#COMMA', [u_ob_c36_get, u_template, [quote, u_obj]]]
			]
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_udist]],
		    
		    [ u_ob_c36_fcreate,
		      
		      [ '#BQ',
			
			[ u_udist,
			  u_obj,
			  ['#COMMA', [u_ob_c36_get, u_template, [quote, u_obj]]]
			]
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_uproc]],
		    [quote, u_uproc_answer_true]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_uselect]],
		    
		    [ let,
		      
		      [ 
			[ u_ob,
			  
			  [ u_ob_c36_instantiate2,
			    [u_ob_c36_get, u_template, [quote, u_pattern]],
			    bindings,
			    u_depth,
			    u_omit_slots,
			    u_include_slots,
			    u_substit,
			    u_abstract,
			    u_omit_proc
			  ]
			]
		      ],
		      
		      [ if,
			[u_ob_c63, u_ob],
			
			[ u_ob_c36_get,
			  u_ob,
			  [u_ob_c36_get, u_template, [quote, u_slot]]
			],
			u_ob
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_ucode]],
		    
		    [ let,
		      [[u_old_ob_bindings, u_xx_ob_bindings_xx], [u_result, []]],
		      [setq, u_xx_ob_bindings_xx, bindings],
		      
		      [ setq,
			u_result,
			[eval, [u_ob_c36_get, u_template, [quote, u_proc]]]
		      ],
		      [setq, u_xx_ob_bindings_xx, u_old_ob_bindings],
		      u_result
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_template, [quote, u_ubind_c33]],
		    
		    [ let,
		      
		      [ 
			[ u_result,
			  
			  [ u_ob_c36_instantiate2,
			    [u_ob_c36_get, u_template, [quote, u_pattern]],
			    bindings,
			    u_depth,
			    u_omit_slots,
			    u_include_slots,
			    u_substit,
			    u_abstract,
			    u_omit_proc
			  ]
			]
		      ],
		      
		      [ u_bd_bind_c33,
			
			[ u_variable_name,
			  [u_ob_c36_get, u_template, [quote, u_var]]
			],
			u_result,
			bindings
		      ],
		      u_result
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\('A'),
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
				 #\(p),
				 #\(e),
				 #\(c),
				 #\(i),
				 #\(a),
				 #\(l)
			       ]),
		      u_templa,
		      u_isn,
		      [quote, t],
		      u_a,
		      u_copy_c63,
		      u_result_ob
		    ],
		    [u_ob_c36_copy, u_template]
		  ]
		]
	      ]).


% annotating U::OB$INSTAN-SPECIAL 
wl: arglist_info(u_ob_c36_instan_special,
		
		[ u_template,
		  bindings,
		  u_depth,
		  u_omit_slots,
		  u_include_slots,
		  u_substit,
		  u_abstract,
		  u_omit_proc
		],
		
		[ Template_Param,
		  Bindings_Param,
		  Depth_Param,
		  Omit_slots_Param,
		  Include_slots_Param,
		  Substit_Param,
		  Abstract_Param,
		  Omit_proc_Param
		],
		arginfo{ all:
			     [ u_template,
			       bindings,
			       u_depth,
			       u_omit_slots,
			       u_include_slots,
			       u_substit,
			       u_abstract,
			       u_omit_proc
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_template,
				 bindings,
				 u_depth,
				 u_omit_slots,
				 u_include_slots,
				 u_substit,
				 u_abstract,
				 u_omit_proc
			       ],
			 opt:0,
			 req:
			     [ u_template,
			       bindings,
			       u_depth,
			       u_omit_slots,
			       u_include_slots,
			       u_substit,
			       u_abstract,
			       u_omit_proc
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTAN-SPECIAL 
wl: init_args(exact_only, u_ob_c36_instan_special).


% annotating U::OB$INSTAN-SPECIAL 
f_u_ob_c36_instan_special(Template_Param, Bindings_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, Substit_Param, Abstract_Param, Omit_proc_Param, ElseResult39) :-
	Env=[bv(u_template, Template_Param), bv(bindings, Bindings_Param), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param), bv(u_include_slots, Include_slots_Param), bv(u_substit, Substit_Param), bv(u_abstract, Abstract_Param), bv(u_omit_proc, Omit_proc_Param)],
	f_u_ty_c36_instance_c63(Template_Param, u_uor, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_instantiate2([u_ob_c36_get, u_template, [quote, u_obj]],
				    bindings,
				    u_depth,
				    u_omit_slots,
				    u_include_slots,
				    u_substit,
				    u_abstract,
				    u_omit_proc,
				    TrueResult108),
	    ElseResult39=TrueResult108
	;   f_u_ty_c36_instance_c63(Template_Param, u_uand, IFTEST29),
	    (   IFTEST29\==[]
	    ->  f_u_any(
			[ lambda,
			  [u_elem],
			  [if, [not, [u_ob_c63, u_elem]], u_elem, []]
			],
			[u_ob_c36_gets, u_template, [quote, u_obj]],
			IFTEST32),
		(   IFTEST32\==[]
		->  ElseResult39=[]
		;   get_var(Env, u_else, IFTEST34),
		    (   IFTEST34\==[]
		    ->  f_u_yloop(
				  [ [u_initial, [u_result, []], [u_found, []]],
				    
				    [ u_yfor,
				      u_elem,
				      u_in,
				      [u_ob_c36_gets, u_template, [quote, u_obj]]
				    ],
				    [u_yuntil, u_result],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ and,
					  [u_var_c63, u_elem],
					  
					  [ setq,
					    u_found,
					    
					    [ u_bd_hyper_lookup,
					      [u_variable_name, u_elem],
					      bindings
					    ]
					  ]
					],
					
					[ cond,
					  
					  [ [u_var_c63, u_found],
					    [setq, u_xx_any_unbound_c63_xx, t],
					    [setq, u_result, u_found]
					  ],
					  
					  [ 
					    [ and,
					      [u_ob_c63, u_found],
					      [u_vars_in_c63, u_found]
					    ],
					    
					    [ setq,
					      u_result,
					      
					      [ u_ob_c36_instantiate2,
						u_found,
						bindings,
						u_depth,
						u_omit_slots,
						u_include_slots,
						u_substit,
						u_abstract,
						u_omit_proc
					      ]
					    ]
					  ],
					  
					  [ u_else,
					    
					    [ setq,
					      u_result,
					      
					      [ u_ob_c36_instantiate2,
						u_found,
						bindings,
						u_depth,
						u_omit_slots,
						u_include_slots,
						u_substit,
						u_abstract,
						u_omit_proc
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ u_yresult,
				      
				      [ if,
					u_result,
					u_result,
					
					[ if,
					  
					  [ u_any_c63,
					    
					    [ lambda,
					      [u_elem],
					      
					      [ and,
						[u_ob_c63, u_elem],
						[not, [u_var_c63, u_elem]]
					      ]
					    ],
					    
					    [ u_ob_c36_gets,
					      u_template,
					      [quote, u_obj]
					    ]
					  ],
					  
					  [ let,
					    
					    [ 
					      [ u_result_ob,
						[u_ob_c36_create_empty]
					      ]
					    ],
					    
					    [ setq,
					      u_xx_instan_obs_xx,
					      
					      [ cons,
						[cons, u_template, u_result_ob],
						u_xx_instan_obs_xx
					      ]
					    ],
					    
					    [ u_yloop,
					      [u_initial, [u_result, []]],
					      
					      [ u_yfor,
						u_elem,
						u_in,
						
						[ u_ob_c36_gets,
						  u_template,
						  [quote, u_obj]
						]
					      ],
					      
					      [ u_ydo,
						
						[ if,
						  
						  [ and,
						    [u_ob_c63, u_elem],
						    [not, [u_var_c63, u_elem]]
						  ],
						  
						  [ progn,
						    
						    [ setq,
						      u_result,
						      
						      [ u_ob_c36_instantiate2,
							u_elem,
							bindings,
							u_depth,
							u_omit_slots,
							u_include_slots,
							u_substit,
							u_abstract,
							u_omit_proc
						      ]
						    ],
						    
						    [ u_yloop,
						      
						      [ u_yfor,
							u_pair,
							u_in,
							
							[ u_ob_c36_pairs,
							  u_result
							]
						      ],
						      
						      [ u_ydo,
							
							[ u_ob_c36_add,
							  u_result_ob,
							  
							  [ u_slots_name,
							    u_pair
							  ],
							  
							  [ u_slots_value,
							    u_pair
							  ]
							]
						      ]
						    ],
						    
						    [ if,
						      [u_ob_c63, u_result],
						      
						      [ u_ob_c36_destroy,
							u_result
						      ],
						      
						      [ error,
							'$ARRAY'([*],
								 claz_base_character,
								 
								 [ #\(~),
								   #\('A'),
								   #\(' '),
								   #\(n),
								   #\(o),
								   #\(t),
								   #\(' '),
								   #\(o),
								   #\(b),
								   #\(' '),
								   #\(t),
								   #\(o),
								   #\(' '),
								   #\(d),
								   #\(e),
								   #\(s),
								   #\(t),
								   #\(r),
								   #\(o),
								   #\(y)
								 ]),
							u_result
						      ]
						    ]
						  ]
						]
					      ]
					    ],
					    u_result_ob
					  ],
					  [u_ob_c36_copy, u_template]
					]
				      ]
				    ]
				  ],
				  TrueResult),
			ElseResult39=TrueResult
		    ;   ElseResult39=[]
		    )
		)
	    ;   f_u_ty_c36_instance_c63(Template_Param, u_unot, IFTEST40),
		(   IFTEST40\==[]
		->  f_u_ob_c36_fcreate(
				       [ '#BQ',
					 
					 [ u_unot,
					   u_obj,
					   
					   [ '#COMMA',
					     
					     [ u_ob_c36_get,
					       u_template,
					       [quote, u_obj]
					     ]
					   ]
					 ]
				       ],
				       TrueResult104),
		    ElseResult39=TrueResult104
		;   f_u_ty_c36_instance_c63(Template_Param, u_udist, IFTEST43),
		    (   IFTEST43\==[]
		    ->  f_u_ob_c36_fcreate(
					   [ '#BQ',
					     
					     [ u_udist,
					       u_obj,
					       
					       [ '#COMMA',
						 
						 [ u_ob_c36_get,
						   u_template,
						   [quote, u_obj]
						 ]
					       ]
					     ]
					   ],
					   TrueResult102),
			ElseResult39=TrueResult102
		    ;   f_u_ty_c36_instance_c63(Template_Param,
						u_uproc,
						IFTEST46),
			(   IFTEST46\==[]
			->  ElseResult39=u_uproc_answer_true
			;   f_u_ty_c36_instance_c63(Template_Param,
						    u_uselect,
						    IFTEST49),
			    (   IFTEST49\==[]
			    ->  f_u_ob_c36_instantiate2(
							[ u_ob_c36_get,
							  u_template,
							  [quote, u_pattern]
							],
							bindings,
							u_depth,
							u_omit_slots,
							u_include_slots,
							u_substit,
							u_abstract,
							u_omit_proc,
							Ob_Init),
				LEnv=[[bv(u_ob, Ob_Init)]|Env],
				f_u_ob_c63(u_ob, IFTEST55),
				(   IFTEST55\==[]
				->  get_var(LEnv, u_ob, Ob_Get),
				    f_u_ob_c36_get(Template_Param, u_slot, Slot),
				    f_u_ob_c36_get(Ob_Get, Slot, TrueResult61),
				    ElseResult39=TrueResult61
				;   get_var(LEnv, u_ob, Ob_Get60),
				    ElseResult39=Ob_Get60
				)
			    ;   f_u_ty_c36_instance_c63(Template_Param,
							u_ucode,
							IFTEST63),
				(   IFTEST63\==[]
				->  get_var(Env,
					    u_xx_ob_bindings_xx,
					    Xx_ob_bindings_xx_Get),
				    Env=[[bv(u_old_ob_bindings, Xx_ob_bindings_xx_Get), bv(u_result, [])]|Env],
				    set_var(Env,
					    u_xx_ob_bindings_xx,
					    Bindings_Param),
				    cl_eval(
					    [ u_ob_c36_get,
					      u_template,
					      [quote, u_proc]
					    ],
					    Result113),
				    set_var(Env, u_result, Result113),
				    get_var(Env,
					    u_old_ob_bindings,
					    Old_ob_bindings_Get),
				    set_var(Env,
					    u_xx_ob_bindings_xx,
					    Old_ob_bindings_Get),
				    get_var(Env, u_result, Result_Get81),
				    ElseResult39=Result_Get81
				;   f_u_ty_c36_instance_c63(Template_Param,
							    u_ubind_c33,
							    IFTEST75),
				    (   IFTEST75\==[]
				    ->  f_u_ob_c36_instantiate2(
								[ u_ob_c36_get,
								  u_template,
								  [quote, u_pattern]
								],
								bindings,
								u_depth,
								u_omit_slots,
								u_include_slots,
								u_substit,
								u_abstract,
								u_omit_proc,
								Result_Init),
					Env=[[bv(u_result, Result_Init)]|Env],
					f_u_bd_bind_c33(
							[ u_variable_name,
							  
							  [ u_ob_c36_get,
							    u_template,
							    [quote, u_var]
							  ]
							],
							u_result,
							bindings,
							Bindings),
					ElseResult39=Result_Get81
				    ;   get_var(Env, u_else, IFTEST84),
					(   IFTEST84\==[]
					->  get_var(Env, u_a, A_Get),
					    get_var(Env,
						    u_copy_c63,
						    Copy_c63_Get),
					    get_var(Env, u_isn, Isn_Get),
					    get_var(Env,
						    u_result_ob,
						    Result_ob_Get),
					    get_var(Env, u_templa, Templa_Get),
					    cl_error(
						     [ '$ARRAY'([*],
								claz_base_character,
								
								[ #\(~),
								  #\('A'),
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
								  #\(p),
								  #\(e),
								  #\(c),
								  #\(i),
								  #\(a),
								  #\(l)
								]),
						       Templa_Get,
						       Isn_Get,
						       t,
						       A_Get,
						       Copy_c63_Get,
						       Result_ob_Get
						     ],
						     Error_Ret),
					    f_u_ob_c36_copy(Template_Param,
							    TrueResult93),
					    ElseResult39=TrueResult93
					;   ElseResult39=[]
					)
				    )
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_instan_special, classof, claz_function),
   set_opv(u_ob_c36_instan_special, compile_as, kw_function),
   set_opv(u_ob_c36_instan_special, function, f_u_ob_c36_instan_special),
   DefunResult=u_ob_c36_instan_special.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("              (ndbg *gate-dbg* ob-warn \"(?~A binding cycle)~%\"",
				     1,
				     775)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                    (variable-name found))",
				     1,
				     839)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: something about type here.",
				     27,
				     2418)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: should not always destroy? What if result",
				     25,
				     2755)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan2.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" isn't a copy?", 25, 2829)).
:- true.


% Total time: 1.321 seconds



% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "wam-cl-init-1" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
% Start time: Tue Dec 19 20:51:59 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; setf.lisp", 1, 1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:14 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 16)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:19 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Copyright (C) 2003-2006 Peter Graves",
				     1,
				     21)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:61 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";; $Id$", 1, 63)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:71 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 73)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:76 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; This program is free software; you can redistribute it and/or",
				     1,
				     78)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:143 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; modify it under the terms of the GNU General Public License",
				     1,
				     145)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:208 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; as published by the Free Software Foundation; either version 2",
				     1,
				     210)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:276 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; of the License, or (at your option) any later version.",
				     1,
				     278)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:336 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 338)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:341 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; This program is distributed in the hope that it will be useful,",
				     1,
				     343)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:410 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; but WITHOUT ANY WARRANTY; without even the implied warranty of",
				     1,
				     412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:478 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
				     1,
				     480)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:545 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; GNU General Public License for more details.",
				     1,
				     547)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:595 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 597)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:600 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; You should have received a copy of the GNU General Public License",
				     1,
				     602)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:671 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; along with this program; if not, write to the Free Software",
				     1,
				     673)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:736 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.",
				     1,
				     738)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:817 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";;", 1, 819)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:822 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; As a special exception, the copyright holders of this library give you",
				     1,
				     824)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:898 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; permission to link this library with independent modules to produce an",
				     1,
				     900)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:974 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; executable, regardless of the license terms of these independent",
				     1,
				     976)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1044 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; modules, and to copy and distribute the resulting executable under",
				     1,
				     1046)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1116 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; terms of your choice, provided that you also meet, for each linked",
				     1,
				     1118)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1188 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; independent module, the terms and conditions of the license of that",
				     1,
				     1190)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1261 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; module.  An independent module is a module which is not derived from",
				     1,
				     1263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1335 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; or based on this library.  If you modify this library, you may extend",
				     1,
				     1337)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1410 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; this exception to your version of the library, but you are not",
				     1,
				     1412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1478 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; obligated to do so.  If you do not wish to do so, delete this",
				     1,
				     1480)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1545 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";; exception statement from your version.",
				     1,
				     1547)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1589 **********************/
:- lisp_compile_to_prolog(pkg_user, ['in-package', '#:system']).
:- cl_in_package(system2, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1617 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    'get-setf-method-inverse',
			    [form, inverse, 'setf-function'],
			    
			    [ let,
			      [['new-var', [gensym]], [vars, []], [vals, []]],
			      
			      [ dolist,
				[x, [cdr, form]],
				[push, [gensym], vars],
				[push, x, vals]
			      ],
			      [setq, vals, [nreverse, vals]],
			      
			      [ values,
				vars,
				vals,
				[list, 'new-var'],
				
				[ if,
				  'setf-function',
				  
				  [ '#BQ',
				    
				    [ ['#BQ-COMMA-ELIPSE', inverse],
				      ['#COMMA', 'new-var'],
				      ['#BQ-COMMA-ELIPSE', vars]
				    ]
				  ],
				  
				  [ if,
				    [functionp, [car, inverse]],
				    
				    [ '#BQ',
				      
				      [ funcall,
					['#BQ-COMMA-ELIPSE', inverse],
					['#BQ-COMMA-ELIPSE', vars],
					['#COMMA', 'new-var']
				      ]
				    ],
				    
				    [ '#BQ',
				      
				      [ ['#BQ-COMMA-ELIPSE', inverse],
					['#BQ-COMMA-ELIPSE', vars],
					['#COMMA', 'new-var']
				      ]
				    ]
				  ]
				],
				
				[ '#BQ',
				  
				  [ ['#COMMA', [car, form]],
				    ['#BQ-COMMA-ELIPSE', vars]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating SYS::GET-SETF-METHOD-INVERSE 
wl: lambda_def(defun,
	      sys_get_setf_method_inverse,
	      f_sys_get_setf_method_inverse,
	      [sys_form, sys_inverse, sys_setf_function],
	      
	      [ 
		[ let,
		  [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]],
		  
		  [ dolist,
		    [sys_x, [cdr, sys_form]],
		    [push, [gensym], sys_vars],
		    [push, sys_x, sys_vals]
		  ],
		  [setq, sys_vals, [nreverse, sys_vals]],
		  
		  [ values,
		    sys_vars,
		    sys_vals,
		    [list, sys_new_var],
		    
		    [ if,
		      sys_setf_function,
		      
		      [ '#BQ',
			
			[ ['#BQ-COMMA-ELIPSE', sys_inverse],
			  ['#COMMA', sys_new_var],
			  ['#BQ-COMMA-ELIPSE', sys_vars]
			]
		      ],
		      
		      [ if,
			[functionp, [car, sys_inverse]],
			
			[ '#BQ',
			  
			  [ funcall,
			    ['#BQ-COMMA-ELIPSE', sys_inverse],
			    ['#BQ-COMMA-ELIPSE', sys_vars],
			    ['#COMMA', sys_new_var]
			  ]
			],
			
			[ '#BQ',
			  
			  [ ['#BQ-COMMA-ELIPSE', sys_inverse],
			    ['#BQ-COMMA-ELIPSE', sys_vars],
			    ['#COMMA', sys_new_var]
			  ]
			]
		      ]
		    ],
		    
		    [ '#BQ',
		      
		      [ ['#COMMA', [car, sys_form]],
			['#BQ-COMMA-ELIPSE', sys_vars]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating SYS::GET-SETF-METHOD-INVERSE 
wl: arglist_info(sys_get_setf_method_inverse,
		[sys_form, sys_inverse, sys_setf_function],
		[Form_Param, Inverse_Param, Setf_function_Param],
		arginfo{ all:[sys_form, sys_inverse, sys_setf_function],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_form, sys_inverse, sys_setf_function],
			 opt:0,
			 req:[sys_form, sys_inverse, sys_setf_function],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::GET-SETF-METHOD-INVERSE 
wl: init_args(exact_only, sys_get_setf_method_inverse).


% annotating SYS::GET-SETF-METHOD-INVERSE 
f_sys_get_setf_method_inverse(Form_Param, Inverse_Param, Setf_function_Param, FnResult) :-
	Env=[bv(sys_form, Form_Param), bv(sys_inverse, Inverse_Param), bv(sys_setf_function, Setf_function_Param)],
	cl_gensym(New_var_Init),
	LEnv=[[bv(sys_new_var, New_var_Init), bv(sys_vars, []), bv(sys_vals, [])]|Env],
	cl_cdr(Form_Param, List),
	BV=bv(sys_x, Ele),
	Env2=[BV|LEnv],
	forall(member(Ele, List),
	       ( nb_setarg(2, BV, Ele),
		 cl_push([gensym], sys_vars, Vars),
		 cl_push(sys_x, sys_vals, Vals)
	       )),
	get_var(LEnv, sys_vals, Vals_Get32),
	cl_nreverse(Vals_Get32, Vals60),
	set_var(LEnv, sys_vals, Vals60),
	get_var(LEnv, sys_new_var, New_var_Get),
	get_var(LEnv, sys_vars, Vars_Get),
	CAR=[New_var_Get],
	(   Setf_function_Param\==[]
	->  get_var(LEnv, sys_new_var, New_var_Get38),
	    get_var(LEnv, sys_vars, Vars_Get39),
	    bq_append(Inverse_Param, [New_var_Get38|Vars_Get39], TrueResult52),
	    ElseResult53=TrueResult52
	;   cl_car(Inverse_Param, PredArgResult),
	    (   is_functionp(PredArgResult)
	    ->  get_var(LEnv, sys_new_var, New_var_Get46),
		get_var(LEnv, sys_vars, Vars_Get45),
		bq_append(Vars_Get45, [New_var_Get46], Bq_append_Ret),
		bq_append([funcall|Inverse_Param], Bq_append_Ret, TrueResult),
		ElseResult53=TrueResult
	    ;   get_var(LEnv, sys_new_var, New_var_Get49),
		get_var(LEnv, sys_vars, Vars_Get48),
		bq_append(Vars_Get48, [New_var_Get49], Bq_append_Ret62),
		bq_append(Inverse_Param, Bq_append_Ret62, ElseResult),
		ElseResult53=ElseResult
	    )
	),
	cl_car(Form_Param, Car_Ret),
	get_var(LEnv, sys_vars, Vars_Get55),
	nb_setval('$mv_return',
		  [Vars_Get, Vals_Get32, CAR, ElseResult53, [Car_Ret|Vars_Get55]]),
	LetResult=Vars_Get,
	LetResult=FnResult.
:- set_opv(f_sys_get_setf_method_inverse, classof, claz_function),
   set_opv(sys_get_setf_method_inverse, compile_as, kw_function),
   set_opv(sys_get_setf_method_inverse, function, f_sys_get_setf_method_inverse),
   _Ignored7=sys_get_setf_method_inverse.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1617 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  '$COMMENT'(";; If a macro, expand one level and try again.  If not, go for the",
				     1,
				     2171)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:1617 **********************/
:- lisp_compile_to_prolog(pkg_sys, '$COMMENT'(";; SETF function.", 1, 2240)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:2259 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    'expand-or-get-setf-inverse',
			    [form, environment],
			    
			    [ 'multiple-value-bind',
			      [expansion, expanded],
			      ['macroexpand-1', form, environment],
			      
			      [ if,
				expanded,
				['get-setf-expansion', expansion, environment],
				
				[ 'get-setf-method-inverse',
				  form,
				  
				  [ '#BQ',
				    
				    [ funcall,
				      function([setf, ['#COMMA', [car, form]]])
				    ]
				  ],
				  t
				]
			      ]
			    ]
			  ]).

% annotating SYS::EXPAND-OR-GET-SETF-INVERSE 
wl: lambda_def(defun,
	      sys_expand_or_get_setf_inverse,
	      f_sys_expand_or_get_setf_inverse,
	      [sys_form, sys_environment],
	      
	      [ 
		[ multiple_value_bind,
		  [sys_expansion, sys_expanded],
		  [macroexpand_1, sys_form, sys_environment],
		  
		  [ if,
		    sys_expanded,
		    [get_setf_expansion, sys_expansion, sys_environment],
		    
		    [ sys_get_setf_method_inverse,
		      sys_form,
		      
		      [ '#BQ',
			[funcall, function([setf, ['#COMMA', [car, sys_form]]])]
		      ],
		      t
		    ]
		  ]
		]
	      ]).


% annotating SYS::EXPAND-OR-GET-SETF-INVERSE 
wl: arglist_info(sys_expand_or_get_setf_inverse,
		[sys_form, sys_environment],
		[Form_Param, Environment_Param],
		arginfo{ all:[sys_form, sys_environment],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_form, sys_environment],
			 opt:0,
			 req:[sys_form, sys_environment],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::EXPAND-OR-GET-SETF-INVERSE 
wl: init_args(exact_only, sys_expand_or_get_setf_inverse).


% annotating SYS::EXPAND-OR-GET-SETF-INVERSE 
f_sys_expand_or_get_setf_inverse(Form_Param, Environment_Param, FnResult) :-
	Env=[bv(sys_form, Form_Param), bv(sys_environment, Environment_Param)],
	LEnv=[[bv(sys_expansion, []), bv(sys_expanded, [])]|Env],
	cl_macroexpand_1([Form_Param, Environment_Param], Macroexpand_1_Ret),
	setq_from_values(LEnv, [sys_expansion, sys_expanded]),
	get_var(LEnv, sys_expanded, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, sys_expansion, Expansion_Get),
	    cl_get_setf_expansion(Expansion_Get,
				  [Environment_Param],
				  TrueResult),
	    FnResult=TrueResult
	;   f_sys_get_setf_method_inverse(Form_Param,
					  
					  [ funcall,
					    function(
						     [ setf,
						       
						       [ '#COMMA',
							 [car, sys_form]
						       ]
						     ])
					  ],
					  t,
					  ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_sys_expand_or_get_setf_inverse, classof, claz_function),
   set_opv(sys_expand_or_get_setf_inverse, compile_as, kw_function),
   set_opv(sys_expand_or_get_setf_inverse,
	   function,
	   f_sys_expand_or_get_setf_inverse),
   _Ignored7=sys_expand_or_get_setf_inverse.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:2581 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    'get-setf-expansion',
			    [form, '&optional', environment],
			    
			    [ let,
			      [temp],
			      
			      [ cond,
				
				[ [symbolp, form],
				  
				  [ 'multiple-value-bind',
				    [expansion, expanded],
				    ['macroexpand-1', form, environment],
				    
				    [ if,
				      expanded,
				      
				      [ 'get-setf-expansion',
					expansion,
					environment
				      ],
				      
				      [ let,
					[['new-var', [gensym]]],
					
					[ values,
					  [],
					  [],
					  [list, 'new-var'],
					  
					  [ '#BQ',
					    
					    [ setq,
					      ['#COMMA', form],
					      ['#COMMA', 'new-var']
					    ]
					  ],
					  form
					]
				      ]
				    ]
				  ]
				],
				
				[ 
				  [ setq,
				    temp,
				    [get, [car, form], [quote, 'setf-inverse']]
				  ],
				  
				  [ 'get-setf-method-inverse',
				    form,
				    ['#BQ', [['#COMMA', temp]]],
				    []
				  ]
				],
				
				[ 
				  [ setq,
				    temp,
				    [get, [car, form], [quote, 'setf-expander']]
				  ],
				  [funcall, temp, form, environment]
				],
				
				[ t,
				  
				  [ 'expand-or-get-setf-inverse',
				    form,
				    environment
				  ]
				]
			      ]
			    ]
			  ]).

% annotating CL:GET-SETF-EXPANSION 
wl: lambda_def(defun,
	      get_setf_expansion,
	      cl_get_setf_expansion,
	      [sys_form, c38_optional, sys_environment],
	      
	      [ 
		[ let,
		  [sys_temp],
		  
		  [ cond,
		    
		    [ [symbolp, sys_form],
		      
		      [ multiple_value_bind,
			[sys_expansion, sys_expanded],
			[macroexpand_1, sys_form, sys_environment],
			
			[ if,
			  sys_expanded,
			  [get_setf_expansion, sys_expansion, sys_environment],
			  
			  [ let,
			    [[sys_new_var, [gensym]]],
			    
			    [ values,
			      [],
			      [],
			      [list, sys_new_var],
			      
			      [ '#BQ',
				
				[ setq,
				  ['#COMMA', sys_form],
				  ['#COMMA', sys_new_var]
				]
			      ],
			      sys_form
			    ]
			  ]
			]
		      ]
		    ],
		    
		    [ 
		      [ setq,
			sys_temp,
			[get, [car, sys_form], [quote, sys_setf_inverse]]
		      ],
		      
		      [ sys_get_setf_method_inverse,
			sys_form,
			['#BQ', [['#COMMA', sys_temp]]],
			[]
		      ]
		    ],
		    
		    [ 
		      [ setq,
			sys_temp,
			[get, [car, sys_form], [quote, sys_setf_expander]]
		      ],
		      [funcall, sys_temp, sys_form, sys_environment]
		    ],
		    
		    [ t,
		      
		      [ sys_expand_or_get_setf_inverse,
			sys_form,
			sys_environment
		      ]
		    ]
		  ]
		]
	      ]).


% annotating CL:GET-SETF-EXPANSION 
wl: arglist_info(get_setf_expansion,
		[sys_form, c38_optional, sys_environment],
		[sys_form, sys_environment],
		arginfo{ all:[sys_form, sys_environment],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_form, sys_environment],
			 opt:[sys_environment],
			 req:[sys_form],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).


% annotating CL:GET-SETF-EXPANSION 
wl: init_args(1, get_setf_expansion).


% annotating CL:GET-SETF-EXPANSION 
cl_get_setf_expansion(Form_Param, RestNKeys, ElseResult65) :-
	Env=[bv(sys_form, Form_Param), bv(sys_environment, Environment_Param)],
	opt_var(Env, sys_environment, Environment_Param, true, [], 1, RestNKeys),
	LEnv=[[bv(sys_temp, [])]|Env],
	(   is_symbolp(Form_Param)
	->  Env=[[bv(sys_expansion, []), bv(sys_expanded, [])]|LEnv],
	    get_var(Env, sys_environment, Environment_Get),
	    cl_macroexpand_1([Form_Param, Environment_Get], Macroexpand_1_Ret),
	    setq_from_values(Env, [sys_expansion, sys_expanded]),
	    get_var(Env, sys_expanded, IFTEST33),
	    (   IFTEST33\==[]
	    ->  get_var(Env, sys_environment, Environment_Get37),
		get_var(Env, sys_expansion, Expansion_Get),
		cl_get_setf_expansion(Expansion_Get,
				      [Environment_Get37],
				      TrueResult),
		ElseResult65=TrueResult
	    ;   cl_gensym(New_var_Init),
		Env=[[bv(sys_new_var, New_var_Init)]|Env],
		get_var(Env, sys_new_var, New_var_Get),
		CAR=[New_var_Get],
		get_var(Env, sys_new_var, New_var_Get44),
		nb_setval('$mv_return',
			  
			  [ [],
			    [],
			    CAR,
			    [setq, Form_Param, New_var_Get44],
			    Form_Param
			  ]),
		ElseResult65=[]
	    )
	;   cl_car(Form_Param, Get_Param),
	    cl_get(Get_Param, sys_setf_inverse, [], IFTEST50),
	    set_var(LEnv, sys_temp, IFTEST50),
	    (   IFTEST50\==[]
	    ->  get_var(LEnv, sys_temp, Temp_Get),
		f_sys_get_setf_method_inverse(Form_Param,
					      [Temp_Get],
					      [],
					      TrueResult64),
		ElseResult65=TrueResult64
	    ;   cl_car(Form_Param, Get_Param71),
		cl_get(Get_Param71, sys_setf_expander, [], IFTEST55),
		set_var(LEnv, sys_temp, IFTEST55),
		(   IFTEST55\==[]
		->  get_var(LEnv, sys_environment, Environment_Get59),
		    f_sys_temp(Form_Param, Environment_Get59, TrueResult62),
		    ElseResult65=TrueResult62
		;   get_var(LEnv, sys_environment, Environment_Get61),
		    f_sys_expand_or_get_setf_inverse(Form_Param,
						     Environment_Get61,
						     ElseResult63),
		    ElseResult65=ElseResult63
		)
	    )
	).
:- set_opv(cl_get_setf_expansion, classof, claz_function),
   set_opv(get_setf_expansion, compile_as, kw_function),
   set_opv(get_setf_expansion, function, cl_get_setf_expansion),
   _Ignored7=get_setf_expansion.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:3317 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defmacro,
			    'abcl-setf',
			    ['&rest', args, '&environment', environment],
			    
			    [ let,
			      [[numargs, [length, args]]],
			      
			      [ cond,
				
				[ [=, numargs, 2],
				  
				  [ let,
				    
				    [ [place, [first, args]],
				      ['value-form', [second, args]]
				    ],
				    
				    [ if,
				      [atom, place],
				      
				      [ '#BQ',
					
					[ setq,
					  ['#COMMA', place],
					  ['#COMMA', 'value-form']
					]
				      ],
				      
				      [ progn,
					
					[ 'multiple-value-bind',
					  
					  [ dummies,
					    vals,
					    'store-vars',
					    setter,
					    getter
					  ],
					  
					  [ 'get-setf-expansion',
					    place,
					    environment
					  ],
					  
					  [ let,
					    
					    [ 
					      [ inverse,
						
						[ get,
						  [car, place],
						  [quote, 'setf-inverse']
						]
					      ]
					    ],
					    
					    [ if,
					      
					      [ and,
						inverse,
						[eq, inverse, [car, setter]]
					      ],
					      
					      [ if,
						[functionp, inverse],
						
						[ '#BQ',
						  
						  [ funcall,
						    ['#COMMA', inverse],
						    
						    [ '#BQ-COMMA-ELIPSE',
						      [cdr, place]
						    ],
						    ['#COMMA', 'value-form']
						  ]
						],
						
						[ '#BQ',
						  
						  [ ['#COMMA', inverse],
						    
						    [ '#BQ-COMMA-ELIPSE',
						      [cdr, place]
						    ],
						    ['#COMMA', 'value-form']
						  ]
						]
					      ],
					      
					      [ if,
						
						[ or,
						  [null, 'store-vars'],
						  [cdr, 'store-vars']
						],
						
						[ '#BQ',
						  
						  [ 'let*',
						    
						    [ 
						      [ '#BQ-COMMA-ELIPSE',
							
							[ mapcar,
							  function(list),
							  dummies,
							  vals
							]
						      ]
						    ],
						    
						    [ 'multiple-value-bind',
						      ['#COMMA', 'store-vars'],
						      ['#COMMA', 'value-form'],
						      ['#COMMA', setter]
						    ]
						  ]
						],
						
						[ '#BQ',
						  
						  [ 'let*',
						    
						    [ 
						      [ '#BQ-COMMA-ELIPSE',
							
							[ mapcar,
							  function(list),
							  dummies,
							  vals
							]
						      ],
						      
						      [ '#COMMA',
							
							[ list,
							  [car, 'store-vars'],
							  'value-form'
							]
						      ]
						    ],
						    ['#COMMA', setter]
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
				
				[ [oddp, numargs],
				  
				  [ error,
				    '$STRING'("Odd number of arguments to SETF.")
				  ]
				],
				
				[ t,
				  
				  [ do,
				    [[a, args, [cddr, a]], [l, []]],
				    
				    [ [null, a],
				      
				      [ '#BQ',
					
					[ progn,
					  ['#BQ-COMMA-ELIPSE', [nreverse, l]]
					]
				      ]
				    ],
				    
				    [ setq,
				      l,
				      
				      [ cons,
					[list, [quote, setf], [car, a], [cadr, a]],
					l
				      ]
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating SYS::ABCL-SETF 
wl: lambda_def(defmacro,
	      sys_abcl_setf,
	      f_sys_abcl_setf,
	      [c38_rest, args, c38_environment, sys_environment],
	      
	      [ progn,
		
		[ let,
		  [[sys_numargs, [length, args]]],
		  
		  [ cond,
		    
		    [ [=, sys_numargs, 2],
		      
		      [ let,
			
			[ [sys_place, [first, args]],
			  [sys_value_form, [second, args]]
			],
			
			[ if,
			  [atom, sys_place],
			  
			  [ '#BQ',
			    
			    [ setq,
			      ['#COMMA', sys_place],
			      ['#COMMA', sys_value_form]
			    ]
			  ],
			  
			  [ progn,
			    
			    [ multiple_value_bind,
			      
			      [ sys_dummies,
				sys_vals,
				sys_store_vars,
				sys_setter,
				sys_getter
			      ],
			      [get_setf_expansion, sys_place, sys_environment],
			      
			      [ let,
				
				[ 
				  [ sys_inverse,
				    
				    [ get,
				      [car, sys_place],
				      [quote, sys_setf_inverse]
				    ]
				  ]
				],
				
				[ if,
				  
				  [ and,
				    sys_inverse,
				    [eq, sys_inverse, [car, sys_setter]]
				  ],
				  
				  [ if,
				    [functionp, sys_inverse],
				    
				    [ '#BQ',
				      
				      [ funcall,
					['#COMMA', sys_inverse],
					['#BQ-COMMA-ELIPSE', [cdr, sys_place]],
					['#COMMA', sys_value_form]
				      ]
				    ],
				    
				    [ '#BQ',
				      
				      [ ['#COMMA', sys_inverse],
					['#BQ-COMMA-ELIPSE', [cdr, sys_place]],
					['#COMMA', sys_value_form]
				      ]
				    ]
				  ],
				  
				  [ if,
				    
				    [ or,
				      [null, sys_store_vars],
				      [cdr, sys_store_vars]
				    ],
				    
				    [ '#BQ',
				      
				      [ let_xx,
					
					[ 
					  [ '#BQ-COMMA-ELIPSE',
					    
					    [ mapcar,
					      function(list),
					      sys_dummies,
					      sys_vals
					    ]
					  ]
					],
					
					[ multiple_value_bind,
					  ['#COMMA', sys_store_vars],
					  ['#COMMA', sys_value_form],
					  ['#COMMA', sys_setter]
					]
				      ]
				    ],
				    
				    [ '#BQ',
				      
				      [ let_xx,
					
					[ 
					  [ '#BQ-COMMA-ELIPSE',
					    
					    [ mapcar,
					      function(list),
					      sys_dummies,
					      sys_vals
					    ]
					  ],
					  
					  [ '#COMMA',
					    
					    [ list,
					      [car, sys_store_vars],
					      sys_value_form
					    ]
					  ]
					],
					['#COMMA', sys_setter]
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
		    
		    [ [oddp, sys_numargs],
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('O'),
				   #\(d),
				   #\(d),
				   #\(' '),
				   #\(n),
				   #\(u),
				   #\(m),
				   #\(b),
				   #\(e),
				   #\(r),
				   #\(' '),
				   #\(o),
				   #\(f),
				   #\(' '),
				   #\(a),
				   #\(r),
				   #\(g),
				   #\(u),
				   #\(m),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(s),
				   #\(' '),
				   #\(t),
				   #\(o),
				   #\(' '),
				   #\('S'),
				   #\('E'),
				   #\('T'),
				   #\('F'),
				   #\('.')
				 ])
		      ]
		    ],
		    
		    [ t,
		      
		      [ do,
			[[sys_a, args, [cddr, sys_a]], [sys_l, []]],
			
			[ [null, sys_a],
			  
			  [ '#BQ',
			    [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]
			  ]
			],
			
			[ setq,
			  sys_l,
			  
			  [ cons,
			    [list, [quote, setf], [car, sys_a], [cadr, sys_a]],
			    sys_l
			  ]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating SYS::ABCL-SETF 
wl: arglist_info(sys_abcl_setf,
		[c38_rest, args, c38_environment, sys_environment],
		[args, sys_environment],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest, environment],
			 env:[sys_environment],
			 key:0,
			 names:[args, sys_environment],
			 opt:0,
			 req:0,
			 rest:[args],
			 sublists:0,
			 whole:0
		       }).


% annotating SYS::ABCL-SETF 
wl: init_args(0, sys_abcl_setf).


% annotating SYS::ABCL-SETF 
f_sys_abcl_setf(Args_Param, FnResult) :-
	TLEnv8=[bv(args, Args_Param), bv(sys_environment, Environment_Param)],
	get_env(TLEnv8, sys_environment, Environment_Param),
	catch(( ( get_var(TLEnv8, args, Args_Get),
		  cl_length(Args_Get, Numargs_Init),
		  LEnv=[[bv(sys_numargs, Numargs_Init)]|TLEnv8],
		  get_var(LEnv, sys_numargs, Numargs_Get),
		  (   Numargs_Get=:=2
		  ->  get_var(LEnv, args, Args_Get31),
		      cl_car(Args_Get31, Place_Init),
		      get_var(LEnv, args, Args_Get32),
		      cl_second(Args_Get32, Value_form_Init),
		      Env=[[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)]|LEnv],
		      get_var(Env, sys_place, Place_Get),
		      (   Place_Get\=[CAR|CDR]
		      ->  get_var(Env, sys_place, Place_Get40),
			  get_var(Env, sys_value_form, Value_form_Get),
			  TrueResult86=[setq, Place_Get40, Value_form_Get]
		      ;   Env=[[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_store_vars, []), bv(sys_setter, []), bv(sys_getter, [])]|Env],
			  get_var(Env, sys_environment, Environment_Get),
			  get_var(Env, sys_place, Place_Get44),
			  cl_get_setf_expansion(Place_Get44,
						[Environment_Get],
						Setf_expansion_Ret),
			  setq_from_values(Env,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_store_vars,
					     sys_setter,
					     sys_getter
					   ]),
			  get_var(Env, sys_place, Place_Get49),
			  cl_car(Place_Get49, Get_Param),
			  cl_get(Get_Param, sys_setf_inverse, [], Inverse_Init),
			  Env=[[bv(sys_inverse, Inverse_Init)]|Env],
			  get_var(Env, sys_inverse, IFTEST53),
			  (   IFTEST53\==[]
			  ->  get_var(Env, sys_inverse, Inverse_Get57),
			      get_var(Env, sys_setter, Setter_Get),
			      cl_car(Setter_Get, Car_Ret),
			      cl_eq(Inverse_Get57, Car_Ret, TrueResult),
			      IFTEST51=TrueResult
			  ;   IFTEST51=[]
			  ),
			  (   IFTEST51\==[]
			  ->  get_var(Env, sys_inverse, Inverse_Get61),
			      (   is_functionp(Inverse_Get61)
			      ->  get_var(Env, sys_inverse, Inverse_Get64),
				  get_var(Env, sys_place, Place_Get65),
				  cl_cdr(Place_Get65, Cdr_Ret),
				  get_var(Env, sys_value_form, Value_form_Get66),
				  bq_append([Inverse_Get64|Cdr_Ret],
					    [Value_form_Get66],
					    Bq_append_Ret),
				  TrueResult86=[funcall|Bq_append_Ret]
			      ;   get_var(Env, sys_inverse, Inverse_Get67),
				  get_var(Env, sys_place, Place_Get68),
				  cl_cdr(Place_Get68, Cdr_Ret151),
				  get_var(Env, sys_value_form, Value_form_Get69),
				  bq_append([Inverse_Get67|Cdr_Ret151],
					    [Value_form_Get69],
					    ElseResult),
				  TrueResult86=ElseResult
			      )
			  ;   (   get_var(Env, sys_store_vars, Store_vars_Get),
				  cl_null(Store_vars_Get, FORM1_Res),
				  FORM1_Res\==[],
				  IFTEST71=FORM1_Res
			      ->  true
			      ;   get_var(Env, sys_store_vars, Store_vars_Get74),
				  cl_cdr(Store_vars_Get74, Cdr_Ret152),
				  IFTEST71=Cdr_Ret152
			      ),
			      (   IFTEST71\==[]
			      ->  get_var(Env, sys_dummies, Dummies_Get),
				  get_var(Env, sys_vals, Vals_Get),
				  cl_mapcar(function(list),
					    [Dummies_Get, Vals_Get],
					    Mapcar_Ret),
				  get_var(Env, sys_setter, Setter_Get80),
				  get_var(Env, sys_store_vars, Store_vars_Get78),
				  get_var(Env, sys_value_form, Value_form_Get79),
				  TrueResult86=[let_xx, Mapcar_Ret, [multiple_value_bind, Store_vars_Get78, Value_form_Get79, Setter_Get80]]
			      ;   get_var(Env, sys_dummies, Dummies_Get81),
				  get_var(Env, sys_vals, Vals_Get82),
				  cl_mapcar(function(list),
					    [Dummies_Get81, Vals_Get82],
					    Bq_append_Param),
				  get_var(Env, sys_store_vars, Store_vars_Get83),
				  cl_car(Store_vars_Get83, Car_Ret154),
				  get_var(Env, sys_value_form, Value_form_Get84),
				  CAR156=[Car_Ret154, Value_form_Get84],
				  bq_append(Bq_append_Param,
					    [CAR156],
					    Bq_append_Ret155),
				  get_var(Env, sys_setter, Setter_Get85),
				  TrueResult86=[let_xx, Bq_append_Ret155, Setter_Get85]
			      )
			  )
		      )
		  ;   get_var(LEnv, sys_numargs, Numargs_Get93),
		      (   mth:is_oddp(Numargs_Get93)
		      ->  cl_error(
				   [ '$ARRAY'([*],
					      claz_base_character,
					      
					      [ #\('O'),
						#\(d),
						#\(d),
						#\(' '),
						#\(n),
						#\(u),
						#\(m),
						#\(b),
						#\(e),
						#\(r),
						#\(' '),
						#\(o),
						#\(f),
						#\(' '),
						#\(a),
						#\(r),
						#\(g),
						#\(u),
						#\(m),
						#\(e),
						#\(n),
						#\(t),
						#\(s),
						#\(' '),
						#\(t),
						#\(o),
						#\(' '),
						#\('S'),
						#\('E'),
						#\('T'),
						#\('F'),
						#\('.')
					      ])
				   ],
				   TrueResult136),
			  TrueResult86=TrueResult136
		      ;   get_var(LEnv, args, Args_Get98),
			  GoEnv=[[bv(sys_a, Args_Get98), bv(sys_l, [])]|LEnv],
			  catch(( call_addr_block(GoEnv,
						  (push_label(do_label_1), get_var(GoEnv, sys_a, IFTEST119), (IFTEST119==[]->cl_nreverse(L_Get112, Nreverse_Ret), throw(block_exit([], [progn|Nreverse_Ret])), _TBResult=ThrowResult124;cl_car(IFTEST101, Car_Ret158), cl_cadr(IFTEST101, Cadr_Ret), CAR160=[setf, Car_Ret158, Cadr_Ret], get_var(GoEnv, sys_l, L_Get129), L=[CAR160|L_Get129], set_var(GoEnv, sys_l, L), get_var(GoEnv, sys_a, A_Get130), cl_cddr(A_Get130, A), set_var(GoEnv, sys_a, A), goto(do_label_1, GoEnv), _TBResult=_GORES131)),
						  
						  [ addr(addr_tagbody_1_do_label_1,
							 do_label_1,
							 '$unused',
							 BlockExitEnv,
							 (get_var(BlockExitEnv, sys_a, IFTEST101), (IFTEST101==[]->get_var(BlockExitEnv, sys_l, L_Get112), cl_nreverse(L_Get112, Nreverse_Ret161), throw(block_exit([], [progn|Nreverse_Ret161])), _34074=ThrowResult;cl_car(IFTEST101, Car_Ret162), cl_cadr(IFTEST101, Cadr_Ret163), CAR164=[setf, Car_Ret162, Cadr_Ret163], Set_var_Ret=[CAR164|L_Get112], set_var(BlockExitEnv, sys_l, Set_var_Ret), cl_cddr(IFTEST101, Cddr_Ret), set_var(BlockExitEnv, sys_a, Cddr_Ret), goto(do_label_1, BlockExitEnv), _34074=_GORES)))
						  ]),
				  []=Block_exit_Ret
				),
				block_exit([], Block_exit_Ret),
				true),
			  TrueResult86=Block_exit_Ret
		      )
		  )
		),
		TrueResult86=MFResult
	      ),
	      block_exit(sys_abcl_setf, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(f_sys_abcl_setf, classof, claz_macro),
   set_opv(sys_abcl_setf, compile_as, kw_operator),
   set_opv(sys_abcl_setf, function, f_sys_abcl_setf),
   _Ignored7=sys_abcl_setf.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:3317 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  '$COMMENT'(";; Redefined in define-modify-macro.lisp.",
				     1,
				     4724)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4767 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defmacro,
			    incf,
			    [place, '&optional', [delta, 1]],
			    
			    [ '#BQ',
			      
			      [ setf,
				['#COMMA', place],
				[+, ['#COMMA', place], ['#COMMA', delta]]
			      ]
			    ]
			  ]).

% annotating CL:INCF 
wl: lambda_def(defmacro,
	      incf,
	      cl_incf,
	      [sys_place, c38_optional, [sys_delta, 1]],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ setf,
		    ['#COMMA', sys_place],
		    [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]
		  ]
		]
	      ]).


% annotating CL:INCF 
wl: arglist_info(incf,
		[sys_place, c38_optional, [sys_delta, 1]],
		[sys_place, sys_delta],
		arginfo{ all:[sys_place, sys_delta],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_place, sys_delta],
			 opt:[sys_delta],
			 req:[sys_place],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).


% annotating CL:INCF 
wl: init_args(1, incf).


% annotating CL:INCF 
cl_incf(Place_Param, RestNKeys, FnResult) :-
	TLEnv8=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
	opt_var(TLEnv8, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
	get_var(TLEnv8, sys_delta, Delta_Get),
	[setf, Place_Param, [+, Place_Param, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_incf, classof, claz_macro),
   set_opv(incf, compile_as, kw_operator),
   set_opv(incf, function, cl_incf),
   _Ignored7=incf.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4767 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  '$COMMENT'(";; Redefined in define-modify-macro.lisp.",
				     1,
				     4851)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4894 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defmacro,
			    decf,
			    [place, '&optional', [delta, 1]],
			    
			    [ '#BQ',
			      
			      [ setf,
				['#COMMA', place],
				[-, ['#COMMA', place], ['#COMMA', delta]]
			      ]
			    ]
			  ]).

% annotating CL:DECF 
wl: lambda_def(defmacro,
	      decf,
	      cl_decf,
	      [sys_place, c38_optional, [sys_delta, 1]],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ setf,
		    ['#COMMA', sys_place],
		    [-, ['#COMMA', sys_place], ['#COMMA', sys_delta]]
		  ]
		]
	      ]).


% annotating CL:DECF 
wl: arglist_info(decf,
		[sys_place, c38_optional, [sys_delta, 1]],
		[sys_place, sys_delta],
		arginfo{ all:[sys_place, sys_delta],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_place, sys_delta],
			 opt:[sys_delta],
			 req:[sys_place],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).


% annotating CL:DECF 
wl: init_args(1, decf).


% annotating CL:DECF 
cl_decf(Place_Param, RestNKeys, FnResult) :-
	TLEnv8=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
	opt_var(TLEnv8, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
	get_var(TLEnv8, sys_delta, Delta_Get),
	[setf, Place_Param, [-, Place_Param, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_decf, classof, claz_macro),
   set_opv(decf, compile_as, kw_operator),
   set_opv(decf, function, cl_decf),
   _Ignored7=decf.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4894 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  '$COMMENT'("; (defsetf subseq (sequence start &optional (end nil)) (v)",
				     1,
				     4978)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4894 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  '$COMMENT'(";   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)",
				     1,
				     5039)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:4894 **********************/
:- lisp_compile_to_prolog(pkg_sys, '$COMMENT'(";      ,v))", 1, 5102)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5115 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    '%set-subseq',
			    [sequence, start, '&rest', rest],
			    
			    [ let,
			      [[end, []], v],
			      
			      [ ecase,
				[length, rest],
				[1, [setq, v, [car, rest]]],
				[2, [setq, end, [car, rest], v, [cadr, rest]]]
			      ],
			      
			      [ progn,
				
				[ replace,
				  sequence,
				  v,
				  ':start1',
				  start,
				  ':end1',
				  end
				],
				v
			      ]
			    ]
			  ]).

% annotating SYS::%SET-SUBSEQ 
wl: lambda_def(defun,
	      sys_pf_set_subseq,
	      f_sys_pf_set_subseq,
	      [sequence, start, c38_rest, rest],
	      
	      [ 
		[ let,
		  [[end, []], sys_v],
		  
		  [ ecase,
		    [length, rest],
		    [1, [setq, sys_v, [car, rest]]],
		    [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]
		  ],
		  
		  [ progn,
		    [replace, sequence, sys_v, kw_start1, start, kw_end1, end],
		    sys_v
		  ]
		]
	      ]).


% annotating SYS::%SET-SUBSEQ 
wl: arglist_info(sys_pf_set_subseq,
		[sequence, start, c38_rest, rest],
		[sequence, start, rest],
		arginfo{ all:[sequence, start],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[sequence, start, rest],
			 opt:0,
			 req:[sequence, start],
			 rest:[rest],
			 sublists:0,
			 whole:0
		       }).


% annotating SYS::%SET-SUBSEQ 
wl: init_args(2, sys_pf_set_subseq).


% annotating SYS::%SET-SUBSEQ 
f_sys_pf_set_subseq(Sequence_Param, Start_Param, Rest_Param, FnResult) :-
	Env=[bv(sequence, Sequence_Param), bv(start, Start_Param), bv(rest, Rest_Param)],
	LEnv=[[bv(end, []), bv(sys_v, [])]|Env],
	get_var(LEnv, rest, Rest_Get),
	cl_length(Rest_Get, avar(PredArg1Result, att(preserved_var, t, []))),
	(   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), 1)
	->  get_var(LEnv, rest, Rest_Get29),
	    cl_car(Rest_Get29, TrueResult36),
	    set_var(LEnv, sys_v, TrueResult36),
	    ElseResult37=TrueResult36
	;   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), 2)
	->  get_var(LEnv, rest, Rest_Get32),
	    cl_car(Rest_Get32, End),
	    set_var(LEnv, end, End),
	    get_var(LEnv, rest, Rest_Get33),
	    cl_cadr(Rest_Get33, TrueResult),
	    set_var(LEnv, sys_v, TrueResult),
	    ElseResult37=TrueResult
	;   cl_type_error(avar(Type_error_Param, att(preserved_var, t, [])),
			  [member, 1, 2],
			  ElseResult),
	    ElseResult37=ElseResult
	),
	get_var(LEnv, end, End_Get),
	get_var(LEnv, sys_v, V_Get),
	cl_replace(Sequence_Param,
		   V_Get,
		   [kw_start1, Start_Param, kw_end1, End_Get],
		   Replace_Ret),
	get_var(LEnv, sys_v, V_Get42),
	LetResult=V_Get42,
	LetResult=FnResult.
:- set_opv(f_sys_pf_set_subseq, classof, claz_function),
   set_opv(sys_pf_set_subseq, compile_as, kw_function),
   set_opv(sys_pf_set_subseq, function, f_sys_pf_set_subseq),
   _Ignored7=sys_pf_set_subseq.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5398 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    '%define-setf-macro',
			    [name, expander, inverse, doc],
			    [declare, [ignore, doc]],
			    
			    [ when,
			      inverse,
			      [put, name, [quote, 'setf-inverse'], inverse]
			    ],
			    
			    [ when,
			      expander,
			      [put, name, [quote, 'setf-expander'], expander]
			    ],
			    name
			  ]).

% annotating SYS::%DEFINE-SETF-MACRO 
wl: lambda_def(defun,
	      sys_pf_define_setf_macro,
	      f_sys_pf_define_setf_type_macro,
	      [sys_name, sys_expander, sys_inverse, sys_doc],
	      
	      [ [declare, [ignore, sys_doc]],
		
		[ when,
		  sys_inverse,
		  [sys_put, sys_name, [quote, sys_setf_inverse], sys_inverse]
		],
		
		[ when,
		  sys_expander,
		  [sys_put, sys_name, [quote, sys_setf_expander], sys_expander]
		],
		sys_name
	      ]).


% annotating SYS::%DEFINE-SETF-MACRO 
wl: arglist_info(sys_pf_define_setf_macro,
		[sys_name, sys_expander, sys_inverse, sys_doc],
		[Name_Param, Expander_Param, Inverse_Param, Doc_Param],
		arginfo{ all:[sys_name, sys_expander, sys_inverse, sys_doc],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_name, sys_expander, sys_inverse, sys_doc],
			 opt:0,
			 req:[sys_name, sys_expander, sys_inverse, sys_doc],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%DEFINE-SETF-MACRO 
wl: init_args(exact_only, sys_pf_define_setf_macro).


% annotating SYS::%DEFINE-SETF-MACRO 
f_sys_pf_define_setf_type_macro(Name_Param, Expander_Param, Inverse_Param, Doc_Param, Name_Param) :-
	Env=[bv(sys_doc, Doc_Param)],
	cl_declare([ignore, sys_doc], Declare_Ret),
	(   Inverse_Param\==[]
	->  f_sys_put(Name_Param, sys_setf_inverse, Inverse_Param, TrueResult),
	    _27428=TrueResult
	;   _27428=[]
	),
	(   Expander_Param\==[]
	->  f_sys_put(Name_Param,
		      sys_setf_expander,
		      Expander_Param,
		      TrueResult34),
	    _27566=TrueResult34
	;   _27566=[]
	).
:- set_opv(f_sys_pf_define_setf_type_macro, classof, claz_function),
   set_opv(sys_pf_define_setf_macro, compile_as, kw_function),
   set_opv(sys_pf_define_setf_macro, function, f_sys_pf_define_setf_type_macro),
   _Ignored7=sys_pf_define_setf_macro.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5398 **********************/
:- lisp_compile_to_prolog(pkg_sys, '$COMMENT'(" FIXME", 26, 5479)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5613 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defmacro,
			    defsetf,
			    ['access-function', 'update-function'],
			    
			    [ '#BQ',
			      
			      [ 'eval-when',
				
				[ ':load-toplevel',
				  ':compile-toplevel',
				  ':execute'
				],
				
				[ put,
				  [quote, ['#COMMA', 'access-function']],
				  [quote, 'setf-inverse'],
				  [quote, ['#COMMA', 'update-function']]
				]
			      ]
			    ]
			  ]).

% annotating CL:DEFSETF 
wl: lambda_def(defmacro,
	      defsetf,
	      cl_defsetf,
	      [sys_access_function, sys_update_function],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ eval_when,
		    [kw_load_toplevel, kw_compile_toplevel, kw_execute],
		    
		    [ sys_put,
		      [quote, ['#COMMA', sys_access_function]],
		      [quote, sys_setf_inverse],
		      [quote, ['#COMMA', sys_update_function]]
		    ]
		  ]
		]
	      ]).


% annotating CL:DEFSETF 
wl: arglist_info(defsetf,
		[sys_access_function, sys_update_function],
		[Access_function_Param, Update_function_Param],
		arginfo{ all:[sys_access_function, sys_update_function],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_access_function, sys_update_function],
			 opt:0,
			 req:[sys_access_function, sys_update_function],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating CL:DEFSETF 
wl: init_args(exact_only, defsetf).


% annotating CL:DEFSETF 
cl_defsetf(Access_function_Param, Update_function_Param, FnResult) :-
	[eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, Access_function_Param], [quote, sys_setf_inverse], [quote, Update_function_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_defsetf, classof, claz_macro),
   set_opv(defsetf, compile_as, kw_operator),
   set_opv(defsetf, function, cl_defsetf),
   _Ignored7=defsetf.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5791 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caar', [x, v], ['set-car', [car, x], v]]).

% annotating SYS::%SET-CAAR 
wl: lambda_def(defun,
	      sys_pf_set_caar,
	      f_sys_pf_set_caar,
	      [sys_x, sys_v],
	      [[sys_set_car, [car, sys_x], sys_v]]).


% annotating SYS::%SET-CAAR 
wl: arglist_info(sys_pf_set_caar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CAAR 
wl: init_args(exact_only, sys_pf_set_caar).


% annotating SYS::%SET-CAAR 
f_sys_pf_set_caar(X_Param, V_Param, FnResult) :-
	cl_car(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caar, classof, claz_function),
   set_opv(sys_pf_set_caar, compile_as, kw_function),
   set_opv(sys_pf_set_caar, function, f_sys_pf_set_caar),
   _Ignored7=sys_pf_set_caar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5836 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cadr', [x, v], ['set-car', [cdr, x], v]]).

% annotating SYS::%SET-CADR 
wl: lambda_def(defun,
	      sys_pf_set_cadr,
	      f_sys_pf_set_cadr,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdr, sys_x], sys_v]]).


% annotating SYS::%SET-CADR 
wl: arglist_info(sys_pf_set_cadr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CADR 
wl: init_args(exact_only, sys_pf_set_cadr).


% annotating SYS::%SET-CADR 
f_sys_pf_set_cadr(X_Param, V_Param, FnResult) :-
	cl_cdr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadr, classof, claz_function),
   set_opv(sys_pf_set_cadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadr, function, f_sys_pf_set_cadr),
   _Ignored7=sys_pf_set_cadr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5881 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdar', [x, v], ['set-cdr', [car, x], v]]).

% annotating SYS::%SET-CDAR 
wl: lambda_def(defun,
	      sys_pf_set_cdar,
	      f_sys_pf_set_cdar,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [car, sys_x], sys_v]]).


% annotating SYS::%SET-CDAR 
wl: arglist_info(sys_pf_set_cdar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDAR 
wl: init_args(exact_only, sys_pf_set_cdar).


% annotating SYS::%SET-CDAR 
f_sys_pf_set_cdar(X_Param, V_Param, FnResult) :-
	cl_car(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdar, classof, claz_function),
   set_opv(sys_pf_set_cdar, compile_as, kw_function),
   set_opv(sys_pf_set_cdar, function, f_sys_pf_set_cdar),
   _Ignored7=sys_pf_set_cdar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5926 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cddr', [x, v], ['set-cdr', [cdr, x], v]]).

% annotating SYS::%SET-CDDR 
wl: lambda_def(defun,
	      sys_pf_set_cddr,
	      f_sys_pf_set_cddr,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cdr, sys_x], sys_v]]).


% annotating SYS::%SET-CDDR 
wl: arglist_info(sys_pf_set_cddr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDDR 
wl: init_args(exact_only, sys_pf_set_cddr).


% annotating SYS::%SET-CDDR 
f_sys_pf_set_cddr(X_Param, V_Param, FnResult) :-
	cl_cdr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddr, classof, claz_function),
   set_opv(sys_pf_set_cddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddr, function, f_sys_pf_set_cddr),
   _Ignored7=sys_pf_set_cddr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5971 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caaar', [x, v], ['set-car', [caar, x], v]]).

% annotating SYS::%SET-CAAAR 
wl: lambda_def(defun,
	      sys_pf_set_caaar,
	      f_sys_pf_set_caaar,
	      [sys_x, sys_v],
	      [[sys_set_car, [caar, sys_x], sys_v]]).


% annotating SYS::%SET-CAAAR 
wl: arglist_info(sys_pf_set_caaar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CAAAR 
wl: init_args(exact_only, sys_pf_set_caaar).


% annotating SYS::%SET-CAAAR 
f_sys_pf_set_caaar(X_Param, V_Param, FnResult) :-
	cl_caar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaar, classof, claz_function),
   set_opv(sys_pf_set_caaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaar, function, f_sys_pf_set_caaar),
   _Ignored7=sys_pf_set_caaar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6018 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cadar', [x, v], ['set-car', [cdar, x], v]]).

% annotating SYS::%SET-CADAR 
wl: lambda_def(defun,
	      sys_pf_set_cadar,
	      f_sys_pf_set_cadar,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdar, sys_x], sys_v]]).


% annotating SYS::%SET-CADAR 
wl: arglist_info(sys_pf_set_cadar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CADAR 
wl: init_args(exact_only, sys_pf_set_cadar).


% annotating SYS::%SET-CADAR 
f_sys_pf_set_cadar(X_Param, V_Param, FnResult) :-
	cl_cdar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadar, classof, claz_function),
   set_opv(sys_pf_set_cadar, compile_as, kw_function),
   set_opv(sys_pf_set_cadar, function, f_sys_pf_set_cadar),
   _Ignored7=sys_pf_set_cadar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6065 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdaar', [x, v], ['set-cdr', [caar, x], v]]).

% annotating SYS::%SET-CDAAR 
wl: lambda_def(defun,
	      sys_pf_set_cdaar,
	      f_sys_pf_set_cdaar,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [caar, sys_x], sys_v]]).


% annotating SYS::%SET-CDAAR 
wl: arglist_info(sys_pf_set_cdaar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDAAR 
wl: init_args(exact_only, sys_pf_set_cdaar).


% annotating SYS::%SET-CDAAR 
f_sys_pf_set_cdaar(X_Param, V_Param, FnResult) :-
	cl_caar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaar, classof, claz_function),
   set_opv(sys_pf_set_cdaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaar, function, f_sys_pf_set_cdaar),
   _Ignored7=sys_pf_set_cdaar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6112 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cddar', [x, v], ['set-cdr', [cdar, x], v]]).

% annotating SYS::%SET-CDDAR 
wl: lambda_def(defun,
	      sys_pf_set_cddar,
	      f_sys_pf_set_cddar,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cdar, sys_x], sys_v]]).


% annotating SYS::%SET-CDDAR 
wl: arglist_info(sys_pf_set_cddar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDDAR 
wl: init_args(exact_only, sys_pf_set_cddar).


% annotating SYS::%SET-CDDAR 
f_sys_pf_set_cddar(X_Param, V_Param, FnResult) :-
	cl_cdar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddar, classof, claz_function),
   set_opv(sys_pf_set_cddar, compile_as, kw_function),
   set_opv(sys_pf_set_cddar, function, f_sys_pf_set_cddar),
   _Ignored7=sys_pf_set_cddar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6159 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caadr', [x, v], ['set-car', [cadr, x], v]]).

% annotating SYS::%SET-CAADR 
wl: lambda_def(defun,
	      sys_pf_set_caadr,
	      f_sys_pf_set_caadr,
	      [sys_x, sys_v],
	      [[sys_set_car, [cadr, sys_x], sys_v]]).


% annotating SYS::%SET-CAADR 
wl: arglist_info(sys_pf_set_caadr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CAADR 
wl: init_args(exact_only, sys_pf_set_caadr).


% annotating SYS::%SET-CAADR 
f_sys_pf_set_caadr(X_Param, V_Param, FnResult) :-
	cl_cadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadr, classof, claz_function),
   set_opv(sys_pf_set_caadr, compile_as, kw_function),
   set_opv(sys_pf_set_caadr, function, f_sys_pf_set_caadr),
   _Ignored7=sys_pf_set_caadr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6206 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caddr', [x, v], ['set-car', [cddr, x], v]]).

% annotating SYS::%SET-CADDR 
wl: lambda_def(defun,
	      sys_pf_set_caddr,
	      f_sys_pf_set_caddr,
	      [sys_x, sys_v],
	      [[sys_set_car, [cddr, sys_x], sys_v]]).


% annotating SYS::%SET-CADDR 
wl: arglist_info(sys_pf_set_caddr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CADDR 
wl: init_args(exact_only, sys_pf_set_caddr).


% annotating SYS::%SET-CADDR 
f_sys_pf_set_caddr(X_Param, V_Param, FnResult) :-
	cl_cddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddr, classof, claz_function),
   set_opv(sys_pf_set_caddr, compile_as, kw_function),
   set_opv(sys_pf_set_caddr, function, f_sys_pf_set_caddr),
   _Ignored7=sys_pf_set_caddr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6253 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdadr', [x, v], ['set-cdr', [cadr, x], v]]).

% annotating SYS::%SET-CDADR 
wl: lambda_def(defun,
	      sys_pf_set_cdadr,
	      f_sys_pf_set_cdadr,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cadr, sys_x], sys_v]]).


% annotating SYS::%SET-CDADR 
wl: arglist_info(sys_pf_set_cdadr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDADR 
wl: init_args(exact_only, sys_pf_set_cdadr).


% annotating SYS::%SET-CDADR 
f_sys_pf_set_cdadr(X_Param, V_Param, FnResult) :-
	cl_cadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadr, classof, claz_function),
   set_opv(sys_pf_set_cdadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdadr, function, f_sys_pf_set_cdadr),
   _Ignored7=sys_pf_set_cdadr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6300 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdddr', [x, v], ['set-cdr', [cddr, x], v]]).

% annotating SYS::%SET-CDDDR 
wl: lambda_def(defun,
	      sys_pf_set_cdddr,
	      f_sys_pf_set_cdddr,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cddr, sys_x], sys_v]]).


% annotating SYS::%SET-CDDDR 
wl: arglist_info(sys_pf_set_cdddr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDDDR 
wl: init_args(exact_only, sys_pf_set_cdddr).


% annotating SYS::%SET-CDDDR 
f_sys_pf_set_cdddr(X_Param, V_Param, FnResult) :-
	cl_cddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddr, classof, claz_function),
   set_opv(sys_pf_set_cdddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdddr, function, f_sys_pf_set_cdddr),
   _Ignored7=sys_pf_set_cdddr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6347 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caaaar', [x, v], ['set-car', [caaar, x], v]]).

% annotating SYS::%SET-CAAAAR 
wl: lambda_def(defun,
	      sys_pf_set_caaaar,
	      f_sys_pf_set_caaaar,
	      [sys_x, sys_v],
	      [[sys_set_car, [caaar, sys_x], sys_v]]).


% annotating SYS::%SET-CAAAAR 
wl: arglist_info(sys_pf_set_caaaar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CAAAAR 
wl: init_args(exact_only, sys_pf_set_caaaar).


% annotating SYS::%SET-CAAAAR 
f_sys_pf_set_caaaar(X_Param, V_Param, FnResult) :-
	cl_caaar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaaar, classof, claz_function),
   set_opv(sys_pf_set_caaaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaaar, function, f_sys_pf_set_caaaar),
   _Ignored7=sys_pf_set_caaaar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6396 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cadaar', [x, v], ['set-car', [cdaar, x], v]]).

% annotating SYS::%SET-CADAAR 
wl: lambda_def(defun,
	      sys_pf_set_cadaar,
	      f_sys_pf_set_cadaar,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdaar, sys_x], sys_v]]).


% annotating SYS::%SET-CADAAR 
wl: arglist_info(sys_pf_set_cadaar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CADAAR 
wl: init_args(exact_only, sys_pf_set_cadaar).


% annotating SYS::%SET-CADAAR 
f_sys_pf_set_cadaar(X_Param, V_Param, FnResult) :-
	cl_cdaar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadaar, classof, claz_function),
   set_opv(sys_pf_set_cadaar, compile_as, kw_function),
   set_opv(sys_pf_set_cadaar, function, f_sys_pf_set_cadaar),
   _Ignored7=sys_pf_set_cadaar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6445 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdaaar', [x, v], ['set-cdr', [caaar, x], v]]).

% annotating SYS::%SET-CDAAAR 
wl: lambda_def(defun,
	      sys_pf_set_cdaaar,
	      f_sys_pf_set_cdaaar,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [caaar, sys_x], sys_v]]).


% annotating SYS::%SET-CDAAAR 
wl: arglist_info(sys_pf_set_cdaaar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDAAAR 
wl: init_args(exact_only, sys_pf_set_cdaaar).


% annotating SYS::%SET-CDAAAR 
f_sys_pf_set_cdaaar(X_Param, V_Param, FnResult) :-
	cl_caaar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaaar, classof, claz_function),
   set_opv(sys_pf_set_cdaaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaaar, function, f_sys_pf_set_cdaaar),
   _Ignored7=sys_pf_set_cdaaar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6494 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cddaar', [x, v], ['set-cdr', [cdaar, x], v]]).

% annotating SYS::%SET-CDDAAR 
wl: lambda_def(defun,
	      sys_pf_set_cddaar,
	      f_sys_pf_set_cddaar,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cdaar, sys_x], sys_v]]).


% annotating SYS::%SET-CDDAAR 
wl: arglist_info(sys_pf_set_cddaar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDDAAR 
wl: init_args(exact_only, sys_pf_set_cddaar).


% annotating SYS::%SET-CDDAAR 
f_sys_pf_set_cddaar(X_Param, V_Param, FnResult) :-
	cl_cdaar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddaar, classof, claz_function),
   set_opv(sys_pf_set_cddaar, compile_as, kw_function),
   set_opv(sys_pf_set_cddaar, function, f_sys_pf_set_cddaar),
   _Ignored7=sys_pf_set_cddaar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6543 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caadar', [x, v], ['set-car', [cadar, x], v]]).

% annotating SYS::%SET-CAADAR 
wl: lambda_def(defun,
	      sys_pf_set_caadar,
	      f_sys_pf_set_caadar,
	      [sys_x, sys_v],
	      [[sys_set_car, [cadar, sys_x], sys_v]]).


% annotating SYS::%SET-CAADAR 
wl: arglist_info(sys_pf_set_caadar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CAADAR 
wl: init_args(exact_only, sys_pf_set_caadar).


% annotating SYS::%SET-CAADAR 
f_sys_pf_set_caadar(X_Param, V_Param, FnResult) :-
	cl_cadar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadar, classof, claz_function),
   set_opv(sys_pf_set_caadar, compile_as, kw_function),
   set_opv(sys_pf_set_caadar, function, f_sys_pf_set_caadar),
   _Ignored7=sys_pf_set_caadar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6592 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caddar', [x, v], ['set-car', [cddar, x], v]]).

% annotating SYS::%SET-CADDAR 
wl: lambda_def(defun,
	      sys_pf_set_caddar,
	      f_sys_pf_set_caddar,
	      [sys_x, sys_v],
	      [[sys_set_car, [cddar, sys_x], sys_v]]).


% annotating SYS::%SET-CADDAR 
wl: arglist_info(sys_pf_set_caddar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CADDAR 
wl: init_args(exact_only, sys_pf_set_caddar).


% annotating SYS::%SET-CADDAR 
f_sys_pf_set_caddar(X_Param, V_Param, FnResult) :-
	cl_cddar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddar, classof, claz_function),
   set_opv(sys_pf_set_caddar, compile_as, kw_function),
   set_opv(sys_pf_set_caddar, function, f_sys_pf_set_caddar),
   _Ignored7=sys_pf_set_caddar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6641 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdadar', [x, v], ['set-cdr', [cadar, x], v]]).

% annotating SYS::%SET-CDADAR 
wl: lambda_def(defun,
	      sys_pf_set_cdadar,
	      f_sys_pf_set_cdadar,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cadar, sys_x], sys_v]]).


% annotating SYS::%SET-CDADAR 
wl: arglist_info(sys_pf_set_cdadar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDADAR 
wl: init_args(exact_only, sys_pf_set_cdadar).


% annotating SYS::%SET-CDADAR 
f_sys_pf_set_cdadar(X_Param, V_Param, FnResult) :-
	cl_cadar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadar, classof, claz_function),
   set_opv(sys_pf_set_cdadar, compile_as, kw_function),
   set_opv(sys_pf_set_cdadar, function, f_sys_pf_set_cdadar),
   _Ignored7=sys_pf_set_cdadar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6690 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdddar', [x, v], ['set-cdr', [cddar, x], v]]).

% annotating SYS::%SET-CDDDAR 
wl: lambda_def(defun,
	      sys_pf_set_cdddar,
	      f_sys_pf_set_cdddar,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cddar, sys_x], sys_v]]).


% annotating SYS::%SET-CDDDAR 
wl: arglist_info(sys_pf_set_cdddar,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDDDAR 
wl: init_args(exact_only, sys_pf_set_cdddar).


% annotating SYS::%SET-CDDDAR 
f_sys_pf_set_cdddar(X_Param, V_Param, FnResult) :-
	cl_cddar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddar, classof, claz_function),
   set_opv(sys_pf_set_cdddar, compile_as, kw_function),
   set_opv(sys_pf_set_cdddar, function, f_sys_pf_set_cdddar),
   _Ignored7=sys_pf_set_cdddar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6739 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caaadr', [x, v], ['set-car', [caadr, x], v]]).

% annotating SYS::%SET-CAAADR 
wl: lambda_def(defun,
	      sys_pf_set_caaadr,
	      f_sys_pf_set_caaadr,
	      [sys_x, sys_v],
	      [[sys_set_car, [caadr, sys_x], sys_v]]).


% annotating SYS::%SET-CAAADR 
wl: arglist_info(sys_pf_set_caaadr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CAAADR 
wl: init_args(exact_only, sys_pf_set_caaadr).


% annotating SYS::%SET-CAAADR 
f_sys_pf_set_caaadr(X_Param, V_Param, FnResult) :-
	cl_caadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaadr, classof, claz_function),
   set_opv(sys_pf_set_caaadr, compile_as, kw_function),
   set_opv(sys_pf_set_caaadr, function, f_sys_pf_set_caaadr),
   _Ignored7=sys_pf_set_caaadr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6788 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cadadr', [x, v], ['set-car', [cdadr, x], v]]).

% annotating SYS::%SET-CADADR 
wl: lambda_def(defun,
	      sys_pf_set_cadadr,
	      f_sys_pf_set_cadadr,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdadr, sys_x], sys_v]]).


% annotating SYS::%SET-CADADR 
wl: arglist_info(sys_pf_set_cadadr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CADADR 
wl: init_args(exact_only, sys_pf_set_cadadr).


% annotating SYS::%SET-CADADR 
f_sys_pf_set_cadadr(X_Param, V_Param, FnResult) :-
	cl_cdadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadadr, classof, claz_function),
   set_opv(sys_pf_set_cadadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadadr, function, f_sys_pf_set_cadadr),
   _Ignored7=sys_pf_set_cadadr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6837 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdaadr', [x, v], ['set-cdr', [caadr, x], v]]).

% annotating SYS::%SET-CDAADR 
wl: lambda_def(defun,
	      sys_pf_set_cdaadr,
	      f_sys_pf_set_cdaadr,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [caadr, sys_x], sys_v]]).


% annotating SYS::%SET-CDAADR 
wl: arglist_info(sys_pf_set_cdaadr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDAADR 
wl: init_args(exact_only, sys_pf_set_cdaadr).


% annotating SYS::%SET-CDAADR 
f_sys_pf_set_cdaadr(X_Param, V_Param, FnResult) :-
	cl_caadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaadr, classof, claz_function),
   set_opv(sys_pf_set_cdaadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaadr, function, f_sys_pf_set_cdaadr),
   _Ignored7=sys_pf_set_cdaadr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6886 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cddadr', [x, v], ['set-cdr', [cdadr, x], v]]).

% annotating SYS::%SET-CDDADR 
wl: lambda_def(defun,
	      sys_pf_set_cddadr,
	      f_sys_pf_set_cddadr,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cdadr, sys_x], sys_v]]).


% annotating SYS::%SET-CDDADR 
wl: arglist_info(sys_pf_set_cddadr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDDADR 
wl: init_args(exact_only, sys_pf_set_cddadr).


% annotating SYS::%SET-CDDADR 
f_sys_pf_set_cddadr(X_Param, V_Param, FnResult) :-
	cl_cdadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddadr, classof, claz_function),
   set_opv(sys_pf_set_cddadr, compile_as, kw_function),
   set_opv(sys_pf_set_cddadr, function, f_sys_pf_set_cddadr),
   _Ignored7=sys_pf_set_cddadr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6935 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-caaddr', [x, v], ['set-car', [caddr, x], v]]).

% annotating SYS::%SET-CAADDR 
wl: lambda_def(defun,
	      sys_pf_set_caaddr,
	      f_sys_pf_set_caaddr,
	      [sys_x, sys_v],
	      [[sys_set_car, [caddr, sys_x], sys_v]]).


% annotating SYS::%SET-CAADDR 
wl: arglist_info(sys_pf_set_caaddr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CAADDR 
wl: init_args(exact_only, sys_pf_set_caaddr).


% annotating SYS::%SET-CAADDR 
f_sys_pf_set_caaddr(X_Param, V_Param, FnResult) :-
	cl_caddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaddr, classof, claz_function),
   set_opv(sys_pf_set_caaddr, compile_as, kw_function),
   set_opv(sys_pf_set_caaddr, function, f_sys_pf_set_caaddr),
   _Ignored7=sys_pf_set_caaddr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:6984 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cadddr', [x, v], ['set-car', [cdddr, x], v]]).

% annotating SYS::%SET-CADDDR 
wl: lambda_def(defun,
	      sys_pf_set_cadddr,
	      f_sys_pf_set_cadddr,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdddr, sys_x], sys_v]]).


% annotating SYS::%SET-CADDDR 
wl: arglist_info(sys_pf_set_cadddr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CADDDR 
wl: init_args(exact_only, sys_pf_set_cadddr).


% annotating SYS::%SET-CADDDR 
f_sys_pf_set_cadddr(X_Param, V_Param, FnResult) :-
	cl_cdddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadddr, classof, claz_function),
   set_opv(sys_pf_set_cadddr, compile_as, kw_function),
   set_opv(sys_pf_set_cadddr, function, f_sys_pf_set_cadddr),
   _Ignored7=sys_pf_set_cadddr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7033 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cdaddr', [x, v], ['set-cdr', [caddr, x], v]]).

% annotating SYS::%SET-CDADDR 
wl: lambda_def(defun,
	      sys_pf_set_cdaddr,
	      f_sys_pf_set_cdaddr,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [caddr, sys_x], sys_v]]).


% annotating SYS::%SET-CDADDR 
wl: arglist_info(sys_pf_set_cdaddr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDADDR 
wl: init_args(exact_only, sys_pf_set_cdaddr).


% annotating SYS::%SET-CDADDR 
f_sys_pf_set_cdaddr(X_Param, V_Param, FnResult) :-
	cl_caddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaddr, classof, claz_function),
   set_opv(sys_pf_set_cdaddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaddr, function, f_sys_pf_set_cdaddr),
   _Ignored7=sys_pf_set_cdaddr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7082 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-cddddr', [x, v], ['set-cdr', [cdddr, x], v]]).

% annotating SYS::%SET-CDDDDR 
wl: lambda_def(defun,
	      sys_pf_set_cddddr,
	      f_sys_pf_set_cddddr,
	      [sys_x, sys_v],
	      [[sys_set_cdr, [cdddr, sys_x], sys_v]]).


% annotating SYS::%SET-CDDDDR 
wl: arglist_info(sys_pf_set_cddddr,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-CDDDDR 
wl: init_args(exact_only, sys_pf_set_cddddr).


% annotating SYS::%SET-CDDDDR 
f_sys_pf_set_cddddr(X_Param, V_Param, FnResult) :-
	cl_cdddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddddr, classof, claz_function),
   set_opv(sys_pf_set_cddddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddddr, function, f_sys_pf_set_cddddr),
   _Ignored7=sys_pf_set_cddddr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7133 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, car, 'set-car']).
:- cl_defsetf(car, sys_set_car, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7156 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdr, 'set-cdr']).
:- cl_defsetf(cdr, sys_set_cdr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7179 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caar, '%set-caar']).
:- cl_defsetf(caar, sys_pf_set_caar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7205 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadr, '%set-cadr']).
:- cl_defsetf(cadr, sys_pf_set_cadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7231 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdar, '%set-cdar']).
:- cl_defsetf(cdar, sys_pf_set_cdar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7257 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddr, '%set-cddr']).
:- cl_defsetf(cddr, sys_pf_set_cddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7283 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaar, '%set-caaar']).
:- cl_defsetf(caaar, sys_pf_set_caaar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7311 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadar, '%set-cadar']).
:- cl_defsetf(cadar, sys_pf_set_cadar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7339 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaar, '%set-cdaar']).
:- cl_defsetf(cdaar, sys_pf_set_cdaar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7367 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddar, '%set-cddar']).
:- cl_defsetf(cddar, sys_pf_set_cddar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7395 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caadr, '%set-caadr']).
:- cl_defsetf(caadr, sys_pf_set_caadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7423 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caddr, '%set-caddr']).
:- cl_defsetf(caddr, sys_pf_set_caddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7451 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdadr, '%set-cdadr']).
:- cl_defsetf(cdadr, sys_pf_set_cdadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7479 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdddr, '%set-cdddr']).
:- cl_defsetf(cdddr, sys_pf_set_cdddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7507 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaaar, '%set-caaaar']).
:- cl_defsetf(caaaar, sys_pf_set_caaaar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7537 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadaar, '%set-cadaar']).
:- cl_defsetf(cadaar, sys_pf_set_cadaar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7567 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaaar, '%set-cdaaar']).
:- cl_defsetf(cdaaar, sys_pf_set_cdaaar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7597 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddaar, '%set-cddaar']).
:- cl_defsetf(cddaar, sys_pf_set_cddaar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7627 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caadar, '%set-caadar']).
:- cl_defsetf(caadar, sys_pf_set_caadar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7657 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caddar, '%set-caddar']).
:- cl_defsetf(caddar, sys_pf_set_caddar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7687 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdadar, '%set-cdadar']).
:- cl_defsetf(cdadar, sys_pf_set_cdadar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7717 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdddar, '%set-cdddar']).
:- cl_defsetf(cdddar, sys_pf_set_cdddar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7747 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaadr, '%set-caaadr']).
:- cl_defsetf(caaadr, sys_pf_set_caaadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7777 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadadr, '%set-cadadr']).
:- cl_defsetf(cadadr, sys_pf_set_cadadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7807 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaadr, '%set-cdaadr']).
:- cl_defsetf(cdaadr, sys_pf_set_cdaadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7837 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddadr, '%set-cddadr']).
:- cl_defsetf(cddadr, sys_pf_set_cddadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7867 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaddr, '%set-caaddr']).
:- cl_defsetf(caaddr, sys_pf_set_caaddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7897 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadddr, '%set-cadddr']).
:- cl_defsetf(cadddr, sys_pf_set_cadddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7927 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaddr, '%set-cdaddr']).
:- cl_defsetf(cdaddr, sys_pf_set_cdaddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7957 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddddr, '%set-cddddr']).
:- cl_defsetf(cddddr, sys_pf_set_cddddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:7989 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, first, 'set-car']).
:- cl_defsetf(first, sys_set_car, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8014 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, second, '%set-cadr']).
:- cl_defsetf(second, sys_pf_set_cadr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8042 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, third, '%set-caddr']).
:- cl_defsetf(third, sys_pf_set_caddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8070 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, fourth, '%set-cadddr']).
:- cl_defsetf(fourth, sys_pf_set_cadddr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8100 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defun, '%set-fifth', [x, v], ['set-car', [cddddr, x], v]]).

% annotating SYS::%SET-FIFTH 
wl: lambda_def(defun,
	      sys_pf_set_fifth,
	      f_sys_pf_set_fifth,
	      [sys_x, sys_v],
	      [[sys_set_car, [cddddr, sys_x], sys_v]]).


% annotating SYS::%SET-FIFTH 
wl: arglist_info(sys_pf_set_fifth,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-FIFTH 
wl: init_args(exact_only, sys_pf_set_fifth).


% annotating SYS::%SET-FIFTH 
f_sys_pf_set_fifth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_fifth, classof, claz_function),
   set_opv(sys_pf_set_fifth, compile_as, kw_function),
   set_opv(sys_pf_set_fifth, function, f_sys_pf_set_fifth),
   _Ignored7=sys_pf_set_fifth.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8149 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, fifth, '%set-fifth']).
:- cl_defsetf(fifth, sys_pf_set_fifth, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8177 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    '%set-sixth',
			    [x, v],
			    ['set-car', [cdr, [cddddr, x]], v]
			  ]).

% annotating SYS::%SET-SIXTH 
wl: lambda_def(defun,
	      sys_pf_set_sixth,
	      f_sys_pf_set_sixth,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]).


% annotating SYS::%SET-SIXTH 
wl: arglist_info(sys_pf_set_sixth,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-SIXTH 
wl: init_args(exact_only, sys_pf_set_sixth).


% annotating SYS::%SET-SIXTH 
f_sys_pf_set_sixth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_sixth, classof, claz_function),
   set_opv(sys_pf_set_sixth, compile_as, kw_function),
   set_opv(sys_pf_set_sixth, function, f_sys_pf_set_sixth),
   _Ignored7=sys_pf_set_sixth.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8232 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, sixth, '%set-sixth']).
:- cl_defsetf(sixth, sys_pf_set_sixth, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8260 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    '%set-seventh',
			    [x, v],
			    ['set-car', [cddr, [cddddr, x]], v]
			  ]).

% annotating SYS::%SET-SEVENTH 
wl: lambda_def(defun,
	      sys_pf_set_seventh,
	      f_sys_pf_set_seventh,
	      [sys_x, sys_v],
	      [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]).


% annotating SYS::%SET-SEVENTH 
wl: arglist_info(sys_pf_set_seventh,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-SEVENTH 
wl: init_args(exact_only, sys_pf_set_seventh).


% annotating SYS::%SET-SEVENTH 
f_sys_pf_set_seventh(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddr_Param),
	cl_cddr(Cddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_seventh, classof, claz_function),
   set_opv(sys_pf_set_seventh, compile_as, kw_function),
   set_opv(sys_pf_set_seventh, function, f_sys_pf_set_seventh),
   _Ignored7=sys_pf_set_seventh.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8318 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, seventh, '%set-seventh']).
:- cl_defsetf(seventh, sys_pf_set_seventh, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8350 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    '%set-eighth',
			    [x, v],
			    ['set-car', [cdddr, [cddddr, x]], v]
			  ]).

% annotating SYS::%SET-EIGHTH 
wl: lambda_def(defun,
	      sys_pf_set_eighth,
	      f_sys_pf_set_eighth,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]).


% annotating SYS::%SET-EIGHTH 
wl: arglist_info(sys_pf_set_eighth,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-EIGHTH 
wl: init_args(exact_only, sys_pf_set_eighth).


% annotating SYS::%SET-EIGHTH 
f_sys_pf_set_eighth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cdddr_Param),
	cl_cdddr(Cdddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_eighth, classof, claz_function),
   set_opv(sys_pf_set_eighth, compile_as, kw_function),
   set_opv(sys_pf_set_eighth, function, f_sys_pf_set_eighth),
   _Ignored7=sys_pf_set_eighth.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8408 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, eighth, '%set-eighth']).
:- cl_defsetf(eighth, sys_pf_set_eighth, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8438 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    '%set-ninth',
			    [x, v],
			    ['set-car', [cddddr, [cddddr, x]], v]
			  ]).

% annotating SYS::%SET-NINTH 
wl: lambda_def(defun,
	      sys_pf_set_ninth,
	      f_sys_pf_set_ninth,
	      [sys_x, sys_v],
	      [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]).


% annotating SYS::%SET-NINTH 
wl: arglist_info(sys_pf_set_ninth,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-NINTH 
wl: init_args(exact_only, sys_pf_set_ninth).


% annotating SYS::%SET-NINTH 
f_sys_pf_set_ninth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_ninth, classof, claz_function),
   set_opv(sys_pf_set_ninth, compile_as, kw_function),
   set_opv(sys_pf_set_ninth, function, f_sys_pf_set_ninth),
   _Ignored7=sys_pf_set_ninth.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8496 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, ninth, '%set-ninth']).
:- cl_defsetf(ninth, sys_pf_set_ninth, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8524 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    '%set-tenth',
			    [x, v],
			    ['set-car', [cdr, [cddddr, [cddddr, x]]], v]
			  ]).

% annotating SYS::%SET-TENTH 
wl: lambda_def(defun,
	      sys_pf_set_tenth,
	      f_sys_pf_set_tenth,
	      [sys_x, sys_v],
	      [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]).


% annotating SYS::%SET-TENTH 
wl: arglist_info(sys_pf_set_tenth,
		[sys_x, sys_v],
		[X_Param, V_Param],
		arginfo{ all:[sys_x, sys_v],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_x, sys_v],
			 opt:0,
			 req:[sys_x, sys_v],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating SYS::%SET-TENTH 
wl: init_args(exact_only, sys_pf_set_tenth).


% annotating SYS::%SET-TENTH 
f_sys_pf_set_tenth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_tenth, classof, claz_function),
   set_opv(sys_pf_set_tenth, compile_as, kw_function),
   set_opv(sys_pf_set_tenth, function, f_sys_pf_set_tenth),
   _Ignored7=sys_pf_set_tenth.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8588 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, tenth, '%set-tenth']).
:- cl_defsetf(tenth, sys_pf_set_tenth, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8618 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, rest, 'set-cdr']).
:- cl_defsetf(rest, sys_set_cdr, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8618 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  '$COMMENT'(";Redefined in extensible-sequences-base.lisp",
				     1,
				     8643)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8689 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, elt, '%set-elt']).
:- cl_defsetf(elt, sys_pf_set_elt, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8713 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, nth, '%set-nth']).
:- cl_defsetf(nth, sys_pf_set_nth, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8737 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, svref, svset]).
:- cl_defsetf(svref, sys_svset, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8760 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defsetf, 'fill-pointer', '%set-fill-pointer']).
:- cl_defsetf(fill_pointer, sys_pf_set_fill_pointer, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8802 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, subseq, '%set-subseq']).
:- cl_defsetf(subseq, sys_pf_set_subseq, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8832 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, 'symbol-value', set]).
:- cl_defsetf(symbol_value, set, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8860 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defsetf, 'symbol-function', '%set-symbol-function']).
:- cl_defsetf(symbol_function, sys_pf_set_symbol_function, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8908 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defsetf, 'symbol-plist', '%set-symbol-plist']).
:- cl_defsetf(symbol_plist, sys_pf_set_symbol_plist, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8950 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, get, put]).
:- cl_defsetf(get, sys_put, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8969 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, gethash, puthash]).
:- cl_defsetf(gethash, sys_puthash, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8996 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, char, 'set-char']).
:- cl_defsetf(char, sys_set_char, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9021 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, schar, 'set-schar']).
:- cl_defsetf(schar, sys_set_schar, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9048 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defsetf,
			    'logical-pathname-translations',
			    '%set-logical-pathname-translations'
			  ]).
:- cl_defsetf(logical_pathname_translations,
	      sys_pf_set_logical_pathname_translations,
	      _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9124 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defsetf, 'readtable-case', '%set-readtable-case']).
:- cl_defsetf(readtable_case, sys_pf_set_readtable_case, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9172 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  [defsetf, 'function-info', '%set-function-info']).
:- cl_defsetf(sys_function_info, sys_pf_set_function_info, _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9218 **********************/
:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defsetf,
			    'stream-external-format',
			    '%set-stream-external-format'
			  ]).
:- cl_defsetf(stream_external_format,
	      sys_pf_set_stream_external_format,
	      _Ignored7).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9282 **********************/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, 'structure-ref', 'structure-set']).
:- cl_defsetf(sys_structure_ref, sys_structure_set, _Ignored7).


% Total time: 6.731 seconds


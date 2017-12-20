
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "sanity-test-0" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
% Start time: Tue Dec 19 19:56:55 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user, [-, 1, 0, 1]).
:- -(1, 0, _27628),
   -(_27628, 1, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" \n\n(- #-:allegro 1 #+:allegro die:a 0 1)\n\n\nFunction MACROEXPAND, MACROEXPAND-1\nSyntax:\n\nmacroexpand form &optional env  ;;  expansion, expanded-p\n\nmacroexpand-1 form &optional env  ;;  expansion, expanded-p\n\nArguments and Values:\n\nform---a form.\n\nenv---an environment object. The default is nil.\n\nexpansion---a form.\n\nexpanded-p---a generalized boolean.\n\nDescription:\n\nmacroexpand and macroexpand-1 expand macros.\n\nIf form is a macro form, then macroexpand-1 expands the macro form call once.\n\nmacroexpand repeatedly expands form until it is no longer a macro form. In effect, macroexpand calls macroexpand-1 repeatedly until the secondary value it returns is nil.\n\nIf form is a macro form, then the expansion is a macro expansion and expanded-p is true. Otherwise, the expansion is the given form and expanded-p is false.\n\nMacro expansion is carried out as follows. Once macroexpand-1 has determined that the form is a macro form, it obtains an appropriate expansion function for the macro or symbol macro. The value of *macroexpand-hook* is coerced to a function and then called as a function of three arguments: the expansion function, the form, and the env. The value returned from this call is taken to be the expansion of the form.\n\nIn addition to macro definitions in the global environment, any local macro definitions established within env by macrolet or symbol-macrolet are considered. If only form is supplied as an argument, then the environment is effectively null, and only global macro definitions as established by defmacro are considered. Macro definitions are shadowed by local function definitions.\n\nExamples:\n",
				     2,
				     40)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1675 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    alpha,
			    [x, y],
			    ['#BQ', [beta, ['#COMMA', x], ['#COMMA', y]]]
			  ]).

% annotating U::ALPHA 
wl: lambda_def(defmacro,
	      u_alpha,
	      f_u_alpha,
	      [u_x, u_y],
	      [progn, ['#BQ', [u_beta, ['#COMMA', u_x], ['#COMMA', u_y]]]]).


% annotating U::ALPHA 
wl: arglist_info(u_alpha,
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

% annotating U::ALPHA 
wl: init_args(exact_only, u_alpha).


% annotating U::ALPHA 
f_u_alpha(X_Param, Y_Param, FnResult) :-
	TLEnv=[bv(u_x, X_Param), bv(u_y, Y_Param)],
	get_var(TLEnv, u_x, X_Get),
	[u_beta, X_Get, Y_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_alpha, classof, claz_macro),
   set_opv(u_alpha, compile_as, kw_operator),
   set_opv(u_alpha, function, f_u_alpha),
   DefMacroResult=u_alpha.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1675 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   ALPHA", 39, 1714)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1725 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    beta,
			    [x, y],
			    ['#BQ', [gamma, ['#COMMA', x], ['#COMMA', y]]]
			  ]).

% annotating U::BETA 
wl: lambda_def(defmacro,
	      u_beta,
	      f_u_beta,
	      [u_x, u_y],
	      [progn, ['#BQ', [u_gamma, ['#COMMA', u_x], ['#COMMA', u_y]]]]).


% annotating U::BETA 
wl: arglist_info(u_beta,
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

% annotating U::BETA 
wl: init_args(exact_only, u_beta).


% annotating U::BETA 
f_u_beta(X_Param, Y_Param, FnResult) :-
	TLEnv=[bv(u_x, X_Param), bv(u_y, Y_Param)],
	get_var(TLEnv, u_x, X_Get),
	[u_gamma, X_Get, Y_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_beta, classof, claz_macro),
   set_opv(u_beta, compile_as, kw_operator),
   set_opv(u_beta, function, f_u_beta),
   DefMacroResult=u_beta.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1725 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   BETA", 39, 1764)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1774 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    delta,
			    [x, y],
			    ['#BQ', [gamma, ['#COMMA', x], ['#COMMA', y]]]
			  ]).

% annotating U::DELTA 
wl: lambda_def(defmacro,
	      u_delta,
	      f_u_delta,
	      [u_x, u_y],
	      [progn, ['#BQ', [u_gamma, ['#COMMA', u_x], ['#COMMA', u_y]]]]).


% annotating U::DELTA 
wl: arglist_info(u_delta,
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

% annotating U::DELTA 
wl: init_args(exact_only, u_delta).


% annotating U::DELTA 
f_u_delta(X_Param, Y_Param, FnResult) :-
	TLEnv=[bv(u_x, X_Param), bv(u_y, Y_Param)],
	get_var(TLEnv, u_x, X_Get),
	[u_gamma, X_Get, Y_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_delta, classof, claz_macro),
   set_opv(u_delta, compile_as, kw_operator),
   set_opv(u_delta, function, f_u_delta),
   DefMacroResult=u_delta.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1774 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   EPSILON", 40, 1814)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1827 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    expand,
			    [form, '&environment', env],
			    
			    [ 'multiple-value-bind',
			      [expansion, 'expanded-p'],
			      [macroexpand, form, env],
			      
			      [ '#BQ',
				
				[ values,
				  [quote, ['#COMMA', expansion]],
				  [quote, ['#COMMA', 'expanded-p']]
				]
			      ]
			    ]
			  ]).

% annotating U::EXPAND 
wl: lambda_def(defmacro,
	      u_expand,
	      f_u_expand,
	      [u_form, c38_environment, env],
	      
	      [ progn,
		
		[ multiple_value_bind,
		  [u_expansion, u_expanded_p],
		  [macroexpand, u_form, env],
		  
		  [ '#BQ',
		    
		    [ values,
		      [quote, ['#COMMA', u_expansion]],
		      [quote, ['#COMMA', u_expanded_p]]
		    ]
		  ]
		]
	      ]).


% annotating U::EXPAND 
wl: arglist_info(u_expand,
		[u_form, c38_environment, env],
		[u_form, env],
		arginfo{ all:[u_form],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[environment],
			 env:[env],
			 key:0,
			 names:[u_form, env],
			 opt:0,
			 req:[u_form],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).


% annotating U::EXPAND 
wl: init_args(1, u_expand).


% annotating U::EXPAND 
f_u_expand(Form_Param, RestNKeys, FnResult) :-
	TLEnv=[bv(u_form, Form_Param), bv(env, Env_Param)],
	get_env(TLEnv, env, Env_Param),
	LEnv=[[bv(u_expansion, []), bv(u_expanded_p, [])]|TLEnv],
	get_var(LEnv, env, Env_Get),
	cl_macroexpand([Form_Param, Env_Get], Macroexpand_Ret),
	setq_from_values(LEnv, [u_expansion, u_expanded_p]),
	get_var(LEnv, u_expanded_p, Expanded_p_Get),
	get_var(LEnv, u_expansion, Expansion_Get),
	LetResult=[values, [quote, Expansion_Get], [quote, Expanded_p_Get]],
	LetResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_expand, classof, claz_macro),
   set_opv(u_expand, compile_as, kw_operator),
   set_opv(u_expand, function, f_u_expand),
   DefMacroResult=u_expand.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:1827 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   EXPAND", 44, 1989)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2001 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'expand-1',
			    [form, '&environment', env],
			    
			    [ 'multiple-value-bind',
			      [expansion, 'expanded-p'],
			      ['macroexpand-1', form, env],
			      
			      [ '#BQ',
				
				[ values,
				  [quote, ['#COMMA', expansion]],
				  [quote, ['#COMMA', 'expanded-p']]
				]
			      ]
			    ]
			  ]).

% annotating U::EXPAND-1 
wl: lambda_def(defmacro,
	      u_expand_1,
	      f_u_expand_1,
	      [u_form, c38_environment, env],
	      
	      [ progn,
		
		[ multiple_value_bind,
		  [u_expansion, u_expanded_p],
		  [macroexpand_1, u_form, env],
		  
		  [ '#BQ',
		    
		    [ values,
		      [quote, ['#COMMA', u_expansion]],
		      [quote, ['#COMMA', u_expanded_p]]
		    ]
		  ]
		]
	      ]).


% annotating U::EXPAND-1 
wl: arglist_info(u_expand_1,
		[u_form, c38_environment, env],
		[u_form, env],
		arginfo{ all:[u_form],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[environment],
			 env:[env],
			 key:0,
			 names:[u_form, env],
			 opt:0,
			 req:[u_form],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).


% annotating U::EXPAND-1 
wl: init_args(1, u_expand_1).


% annotating U::EXPAND-1 
f_u_expand_1(Form_Param, RestNKeys, FnResult) :-
	TLEnv=[bv(u_form, Form_Param), bv(env, Env_Param)],
	get_env(TLEnv, env, Env_Param),
	LEnv=[[bv(u_expansion, []), bv(u_expanded_p, [])]|TLEnv],
	get_var(LEnv, env, Env_Get),
	cl_macroexpand_1([Form_Param, Env_Get], Macroexpand_1_Ret),
	setq_from_values(LEnv, [u_expansion, u_expanded_p]),
	get_var(LEnv, u_expanded_p, Expanded_p_Get),
	get_var(LEnv, u_expansion, Expansion_Get),
	LetResult=[values, [quote, Expansion_Get], [quote, Expanded_p_Get]],
	LetResult=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_expand_1, classof, claz_macro),
   set_opv(u_expand_1, compile_as, kw_operator),
   set_opv(u_expand_1, function, f_u_expand_1),
   DefMacroResult=u_expand_1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2001 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   EXPAND-1", 44, 2167)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2182 **********************/
:- lisp_compile_to_prolog(pkg_user, ['prolog-call', '$STRING'("lisp")]).
:- (   lisp
   ->  _Ignored=t
   ;   _Ignored=[]
   ).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ quote,
			    
			    [ list,
			      ['macroexpand-1', [quote, [alpha, a, b]]],
			      ['expand-1', [alpha, a, b]],
			      [macroexpand, [quote, [alpha, a, b]]],
			      [expand, [alpha, a, b]],
			      ['macroexpand-1', [quote, 'not-a-macro']],
			      ['expand-1', 'not-a-macro'],
			      [macroexpand, [quote, ['not-a-macro', a, b]]],
			      [expand, ['not-a-macro', a, b]],
			      
			      [ macrolet,
				
				[ 
				  [ alpha,
				    [x, y],
				    ['#BQ', [delta, ['#COMMA', x], ['#COMMA', y]]]
				  ]
				],
				['macroexpand-1', [quote, [alpha, a, b]]]
			      ],
			      
			      [ macrolet,
				
				[ 
				  [ alpha,
				    [x, y],
				    ['#BQ', [delta, ['#COMMA', x], ['#COMMA', y]]]
				  ]
				],
				['expand-1', [alpha, a, b]]
			      ],
			      
			      [ macrolet,
				
				[ 
				  [ alpha,
				    [x, y],
				    ['#BQ', [delta, ['#COMMA', x], ['#COMMA', y]]]
				  ]
				],
				[macroexpand, [quote, [alpha, a, b]]]
			      ],
			      
			      [ macrolet,
				
				[ 
				  [ alpha,
				    [x, y],
				    ['#BQ', [delta, ['#COMMA', x], ['#COMMA', y]]]
				  ]
				],
				[expand, [alpha, a, b]]
			      ],
			      
			      [ macrolet,
				
				[ 
				  [ beta,
				    [x, y],
				    
				    [ '#BQ',
				      [epsilon, ['#COMMA', x], ['#COMMA', y]]
				    ]
				  ]
				],
				[expand, [alpha, a, b]]
			      ],
			      
			      [ let,
				[[x, [list, 1, 2, 3]]],
				['symbol-macrolet', [[a, [first, x]]], [expand, a]]
			      ],
			      
			      [ let,
				[[x, [list, 1, 2, 3]]],
				
				[ 'symbol-macrolet',
				  [[a, [first, x]]],
				  [macroexpand, [quote, a]]
				]
			      ],
			      
			      [ 'symbol-macrolet',
				[[b, [alpha, x, y]]],
				['expand-1', b]
			      ],
			      ['symbol-macrolet', [[b, [alpha, x, y]]], [expand, b]],
			      
			      [ 'symbol-macrolet',
				[[b, [alpha, x, y]], [a, b]],
				['expand-1', a]
			      ],
			      
			      [ 'symbol-macrolet',
				[[b, [alpha, x, y]], [a, b]],
				[expand, a]
			      ],
			      
			      [ flet,
				[[beta, [x, y], [+, x, y]]],
				[expand, [alpha, a, b]]
			      ],
			      
			      [ macrolet,
				
				[ 
				  [ alpha,
				    [x, y],
				    ['#BQ', [delta, ['#COMMA', x], ['#COMMA', y]]]
				  ]
				],
				
				[ flet,
				  [[alpha, [x, y], [+, x, y]]],
				  [expand, [alpha, a, b]]
				]
			      ],
			      
			      [ let,
				[[x, [list, 1, 2, 3]]],
				
				[ 'symbol-macrolet',
				  [[a, [first, x]]],
				  [let, [[a, x]], [expand, a]]
				]
			      ]
			    ]
			  ]).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; Simple examples involving just the global environment",
				     1,
				     2213)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   (BETA A B), true", 32, 2301)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   (BETA A B), true", 26, 2348)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (GAMMA A B), true", 30, 2399)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (GAMMA A B), true", 24, 2445)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   NOT-A-MACRO, false", 32, 2499)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   NOT-A-MACRO, false", 26, 2548)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (NOT-A-MACRO A B), false", 36, 2607)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (NOT-A-MACRO A B), false", 30, 2666)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; Examples involving lexical environments",
				     1,
				     2697)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   (BETA A B), true", 35, 2816)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (DELTA A B), true", 29, 2908)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (GAMMA A B), true", 33, 3005)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (GAMMA A B), true", 27, 3096)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (EPSILON A B), true", 27, 3188)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   (FIRST X), true", 20, 3293)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   A, false", 26, 3400)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (ALPHA X Y), true", 19, 3468)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (GAMMA X Y), true", 17, 3543)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   B, true", 19, 3645)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (GAMMA X Y), true", 17, 3735)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("; Examples of shadowing behavior",
				     1,
				     3759)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   (BETA A B), true", 27, 3849)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";   (ALPHA A B), false", 30, 3975)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:2204 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(";   A, false", 23, 4100)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4119 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    foo,
			    [x, flag],
			    
			    [ macrolet,
			      
			      [ 
				[ fudge,
				  [z],
				  
				  [ '#BQ',
				    
				    [ if,
				      flag,
				      [*, ['#COMMA', z], ['#COMMA', z]],
				      ['#COMMA', z]
				    ]
				  ]
				]
			      ],
			      [+, x, [fudge, x], [fudge, [+, x, 1]]]
			    ]
			  ]).

% annotating U::FUDGE 
wl: lambda_def(defmacro,
	      u_fudge,
	      f_u_fudge2,
	      [u_z],
	      
	      [ progn,
		
		[ '#BQ',
		  [if, u_flag, [*, ['#COMMA', u_z], ['#COMMA', u_z]], ['#COMMA', u_z]]
		]
	      ]).


% annotating U::FUDGE 
wl: arglist_info(u_fudge,
		[u_z],
		[Z_Param],
		arginfo{ all:[u_z],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_z],
			 opt:0,
			 req:[u_z],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FUDGE 
wl: init_args(exact_only, u_fudge).


% annotating U::FUDGE 
f_u_fudge2(Z_Param, FnResult16) :-
	Env=[bv(u_z, Z_Param)],
	get_var(Env, u_z, Z_Get24),
	[if, u_flag, [*, Z_Get24, Z_Get24], Z_Get24]=MFResult,
	cl_eval(MFResult, FnResult16).

% annotating U::FOO 
wl: lambda_def(defun,
	      u_foo,
	      f_u_foo,
	      [u_x, u_flag],
	      
	      [ 
		[ macrolet,
		  
		  [ 
		    [ u_fudge,
		      [u_z],
		      
		      [ '#BQ',
			
			[ if,
			  u_flag,
			  [*, ['#COMMA', u_z], ['#COMMA', u_z]],
			  ['#COMMA', u_z]
			]
		      ]
		    ]
		  ],
		  [+, u_x, [u_fudge, u_x], [u_fudge, [+, u_x, 1]]]
		]
	      ]).


% annotating U::FOO 
wl: arglist_info(u_foo,
		[u_x, u_flag],
		[X_Param, Flag_Param],
		arginfo{ all:[u_x, u_flag],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_x, u_flag],
			 opt:0,
			 req:[u_x, u_flag],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FOO 
wl: init_args(exact_only, u_foo).


% annotating U::FOO 
f_u_foo(X_Param, Flag_Param, FnResult) :-
	Env=[bv(u_x, X_Param), bv(u_flag, Flag_Param)],
	get_var(Env, u_x, X_Get),
	f_u_fudge2(u_x, Fudge2_Ret),
	+(X_Get, Fudge2_Ret, _11664),
	f_u_fudge2([+, u_x, 1], Fudge2_Ret30),
	+(_11664, Fudge2_Ret30, _11380),
	_11380=FnResult.
:- set_opv(f_u_foo, classof, claz_function),
   set_opv(u_foo, compile_as, kw_function),
   set_opv(u_foo, function, f_u_foo),
   DefunResult=u_foo.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4119 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("The parameters x and flag are not accessible",
				     18,
				     4182)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4119 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" at this point; a reference to flag would be to",
				     18,
				     4245)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4119 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" the global variable of that name.",
				     18,
				     4311)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test-0.lisp:4119 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("The parameters x and flag are accessible here.",
				     5,
				     4395)).
:- true.


% Total time: 0.998 seconds



% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_unify" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:38 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 701)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This file contains the OB unifier",
				     1,
				     202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:237 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 238)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:239 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 10/13/84: Original version written",
				     1,
				     240)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:276 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/24/85: Upgraded to full unifier",
				     1,
				     277)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:313 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  6/30/85: Added *instance-of*, ob$compare",
				     1,
				     314)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:357 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   9/3/85: Added loop checking in unifier",
				     1,
				     358)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:400 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   1/6/86: Changed special forms to obs",
				     1,
				     401)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:441 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/24/86: Commented out compile-web-pattern, added relaxation to ob-unify-var",
				     1,
				     442)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:521 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/26/86: Added variable-value", 1, 522)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:554 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/24/86: Removed flavors", 1, 555)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:582 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/29/86: Updated to new unification algorithm",
				     1,
				     583)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:631 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  11/2/86: Added UDIST, changed ob$unify-var",
				     1,
				     632)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:677 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 678)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:679 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:760 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*unify-debugging?*', []]).
:- set_var(TLEnv3, setq, u_xx_unify_debugging_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:791 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*relax-unify-var*', []]).
:- set_var(TLEnv3, setq, u_xx_relax_unify_var_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:791 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 822)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:791 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Empty binding list", 1, 824)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:791 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 845)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:846 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*empty-bd*', [quote, [t]]]).
:- set_var(TLEnv3, setq, u_xx_empty_bd_xx, [t]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:870 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bd-and-empty-bd?',
			    [bd],
			    [if, bd, [if, ['null?', [cdr, bd]], bd, []], []]
			  ]).

% annotating U::BD-AND-EMPTY-BD? 
wl: lambda_def(defun,
	      u_bd_and_empty_bd_c63,
	      f_u_bd_and_empty_bd_c63,
	      [u_bd],
	      [[if, u_bd, [if, [u_null_c63, [cdr, u_bd]], u_bd, []], []]]).


% annotating U::BD-AND-EMPTY-BD? 
wl: arglist_info(u_bd_and_empty_bd_c63,
		[u_bd],
		[Bd_Param],
		arginfo{ all:[u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd],
			 opt:0,
			 req:[u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-AND-EMPTY-BD? 
wl: init_args(exact_only, u_bd_and_empty_bd_c63).


% annotating U::BD-AND-EMPTY-BD? 
f_u_bd_and_empty_bd_c63(Bd_Param, TrueResult19) :-
	Env=[bv(u_bd, Bd_Param)],
	(   Bd_Param\==[]
	->  f_u_null_c63([cdr, u_bd], IFTEST15),
	    (   IFTEST15\==[]
	    ->  TrueResult19=Bd_Param
	    ;   TrueResult19=[]
	    )
	;   TrueResult19=[]
	).
:- set_opv(f_u_bd_and_empty_bd_c63, classof, claz_function),
   set_opv(u_bd_and_empty_bd_c63, compile_as, kw_function),
   set_opv(u_bd_and_empty_bd_c63, function, f_u_bd_and_empty_bd_c63),
   DefunResult=u_bd_and_empty_bd_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:976 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'empty-bd?',
			    [bd],
			    [if, ['null?', [cdr, bd]], bd, []]
			  ]).

% annotating U::EMPTY-BD? 
wl: lambda_def(defun,
	      u_empty_bd_c63,
	      f_u_empty_bd_c63,
	      [u_bd],
	      [[if, [u_null_c63, [cdr, u_bd]], u_bd, []]]).


% annotating U::EMPTY-BD? 
wl: arglist_info(u_empty_bd_c63,
		[u_bd],
		[Bd_Param],
		arginfo{ all:[u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd],
			 opt:0,
			 req:[u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EMPTY-BD? 
wl: init_args(exact_only, u_empty_bd_c63).


% annotating U::EMPTY-BD? 
f_u_empty_bd_c63(Bd_Param, FnResult) :-
	Env=[bv(u_bd, Bd_Param)],
	f_u_null_c63([cdr, u_bd], IFTEST),
	(   IFTEST\==[]
	->  FnResult=Bd_Param
	;   FnResult=[]
	).
:- set_opv(f_u_empty_bd_c63, classof, claz_function),
   set_opv(u_empty_bd_c63, compile_as, kw_function),
   set_opv(u_empty_bd_c63, function, f_u_empty_bd_c63),
   DefunResult=u_empty_bd_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1043 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'non-empty-bd?', [bd], [if, [cdr, bd], bd, []]]).

% annotating U::NON-EMPTY-BD? 
wl: lambda_def(defun,
	      u_non_empty_bd_c63,
	      f_u_non_empty_bd_c63,
	      [u_bd],
	      [[if, [cdr, u_bd], u_bd, []]]).


% annotating U::NON-EMPTY-BD? 
wl: arglist_info(u_non_empty_bd_c63,
		[u_bd],
		[Bd_Param],
		arginfo{ all:[u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_bd],
			 opt:0,
			 req:[u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NON-EMPTY-BD? 
wl: init_args(exact_only, u_non_empty_bd_c63).


% annotating U::NON-EMPTY-BD? 
f_u_non_empty_bd_c63(Bd_Param, FnResult) :-
	cl_cdr(Bd_Param, IFTEST),
	(   IFTEST\==[]
	->  FnResult=Bd_Param
	;   FnResult=[]
	).
:- set_opv(f_u_non_empty_bd_c63, classof, claz_function),
   set_opv(u_non_empty_bd_c63, compile_as, kw_function),
   set_opv(u_non_empty_bd_c63, function, f_u_non_empty_bd_c63),
   DefunResult=u_non_empty_bd_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1043 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1095)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1043 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (bd-lookup var bindings):", 1, 1097)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1043 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Look up the value of a variable in a binding list returned by ob$unify.",
				     1,
				     1125)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1043 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1199)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1200 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, 'bd-create', [], [cons, t, []]]).

% annotating U::BD-CREATE 
wl: lambda_def(defun, u_bd_create, f_u_bd_create, [], [[cons, t, []]]).


% annotating U::BD-CREATE 
wl: arglist_info(u_bd_create,
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

% annotating U::BD-CREATE 
wl: init_args(exact_only, u_bd_create).


% annotating U::BD-CREATE 
f_u_bd_create(FnResult) :-
	Env=[],
	_362944=[t],
	_362944=FnResult.
:- set_opv(f_u_bd_create, classof, claz_function),
   set_opv(u_bd_create, compile_as, kw_function),
   set_opv(u_bd_create, function, f_u_bd_create),
   DefunResult=u_bd_create.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1235 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'bd-hyper-lookup1',
			    [var, bd, vars, 'first-level'],
			    
			    [ if,
			      ['memq?', var, vars],
			      'first-level',
			      
			      [ let,
				[[val, ['bd-lookup', var, bd]]],
				
				[ if,
				  ['var?', val],
				  
				  [ 'bd-hyper-lookup1',
				    ['variable-name', val],
				    bd,
				    [cons, var, vars],
				    [if, 'first-level', 'first-level', val]
				  ],
				  val
				]
			      ]
			    ]
			  ]).

% annotating U::BD-HYPER-LOOKUP1 
wl: lambda_def(defun,
	      u_bd_hyper_lookup1,
	      f_u_bd_hyper_lookup1,
	      [u_var, u_bd, u_vars, u_first_level],
	      
	      [ 
		[ if,
		  [u_memq_c63, u_var, u_vars],
		  u_first_level,
		  
		  [ let,
		    [[u_val, [u_bd_lookup, u_var, u_bd]]],
		    
		    [ if,
		      [u_var_c63, u_val],
		      
		      [ u_bd_hyper_lookup1,
			[u_variable_name, u_val],
			u_bd,
			[cons, u_var, u_vars],
			[if, u_first_level, u_first_level, u_val]
		      ],
		      u_val
		    ]
		  ]
		]
	      ]).


% annotating U::BD-HYPER-LOOKUP1 
wl: arglist_info(u_bd_hyper_lookup1,
		[u_var, u_bd, u_vars, u_first_level],
		[Var_Param, Bd_Param, Vars_Param, First_level_Param],
		arginfo{ all:[u_var, u_bd, u_vars, u_first_level],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_var, u_bd, u_vars, u_first_level],
			 opt:0,
			 req:[u_var, u_bd, u_vars, u_first_level],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::BD-HYPER-LOOKUP1 
wl: init_args(exact_only, u_bd_hyper_lookup1).


% annotating U::BD-HYPER-LOOKUP1 
f_u_bd_hyper_lookup1(Var_Param, Bd_Param, Vars_Param, First_level_Param, FnResult) :-
	Env=[bv(u_var, Var_Param), bv(u_bd, Bd_Param), bv(u_vars, Vars_Param), bv(u_first_level, First_level_Param)],
	f_u_memq_c63(u_var, u_vars, IFTEST),
	(   IFTEST\==[]
	->  FnResult=First_level_Param
	;   f_u_bd_lookup(u_var, u_bd, Val_Init),
	    LEnv=[[bv(u_val, Val_Init)]|Env],
	    f_u_var_c63(u_val, IFTEST24),
	    (   IFTEST24\==[]
	    ->  f_u_variable_name(u_val, Hyper_lookup1_Param),
		_139546=[Var_Param|Vars_Param],
		(   First_level_Param\==[]
		->  _139604=First_level_Param
		;   get_var(LEnv, u_val, Val_Get),
		    _139604=Val_Get
		),
		f_u_bd_hyper_lookup1(Hyper_lookup1_Param,
				     Bd_Param,
				     _139546,
				     _139604,
				     TrueResult38),
		FnResult=TrueResult38
	    ;   get_var(LEnv, u_val, Val_Get37),
		FnResult=Val_Get37
	    )
	).
:- set_opv(f_u_bd_hyper_lookup1, classof, claz_function),
   set_opv(u_bd_hyper_lookup1, compile_as, kw_function),
   set_opv(u_bd_hyper_lookup1, function, f_u_bd_hyper_lookup1),
   DefunResult=u_bd_hyper_lookup1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'variable-hyper-lookup',
			    [variable, bindings],
			    
			    [ 'bd-hyper-lookup',
			      ['variable-name', variable],
			      bindings
			    ]
			  ]).

% annotating U::VARIABLE-HYPER-LOOKUP 
wl: lambda_def(defun,
	      u_variable_hyper_lookup,
	      f_u_variable_hyper_lookup,
	      [variable, bindings],
	      [[u_bd_hyper_lookup, [u_variable_name, variable], bindings]]).


% annotating U::VARIABLE-HYPER-LOOKUP 
wl: arglist_info(u_variable_hyper_lookup,
		[variable, bindings],
		[Variable_Param, Bindings_Param],
		arginfo{ all:[variable, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[variable, bindings],
			 opt:0,
			 req:[variable, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VARIABLE-HYPER-LOOKUP 
wl: init_args(exact_only, u_variable_hyper_lookup).


% annotating U::VARIABLE-HYPER-LOOKUP 
f_u_variable_hyper_lookup(Variable_Param, Bindings_Param, FnResult) :-
	Env=[bv(variable, Variable_Param), bv(bindings, Bindings_Param)],
	f_u_bd_hyper_lookup([u_variable_name, variable], bindings, Bindings),
	Bindings=FnResult.
:- set_opv(f_u_variable_hyper_lookup, classof, claz_function),
   set_opv(u_variable_hyper_lookup, compile_as, kw_function),
   set_opv(u_variable_hyper_lookup, function, f_u_variable_hyper_lookup),
   DefunResult=u_variable_hyper_lookup.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun variable-hyper-lookup (variable bindings)",
				     1,
				     1725)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (let ((found (assq (variable-name variable)",
				     1,
				     1775)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                     (cdr bindings))))",
				     1,
				     1822)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("    (if found", 1, 1862)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (if (var? (cadr found))", 1, 1877)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (variable-hyper-lookup1 (cadr found) bindings",
				     1,
				     1910)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                    (list (variable-name variable)))",
				     1,
				     1969)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (cadr found))", 1, 2039)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        nil)))", 1, 2066)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun variable-hyper-lookup1 (variable bindings names)",
				     1,
				     2083)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (if (memq? (variable-name variable) names)",
				     1,
				     2140)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("      nil", 1, 2186)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("      (let ((found (assq (variable-name variable)",
				     1,
				     2197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                         (cdr bindings))))",
				     1,
				     2248)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        (if found", 1, 2292)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (if (var? (cadr found))",
				     1,
				     2311)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (variable-hyper-lookup1 (cadr found) bindings",
				     1,
				     2348)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                        (cons (variable-name variable) names))",
				     1,
				     2411)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                (cadr found))", 1, 2491)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            variable))))", 1, 2522)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defun bd-hyper-lookup1 (var bindings)",
				     1,
				     2549)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  (let ((found (assq var (cdr bindings))))",
				     1,
				     2589)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("    (if found", 1, 2633)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("        (if (var? (cadr found))", 1, 2648)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (bd-hyper-lookup (variable-name (cadr found)) bindings)",
				     1,
				     2681)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (cadr found))", 1, 2750)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("        var)))", 1, 2777)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2794)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Variables", 1, 2796)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2808)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Examples of macro translation:", 1, 2810)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ?Self --> (UVAR name 'self unifies-with PERSON)",
				     1,
				     2843)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ?Person1 --> (UVAR name 'person1 unifies-with PERSON))",
				     1,
				     2893)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ?Silly:Person --> (UVAR name 'silly unifies-with PERSON)",
				     1,
				     2950)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ?:Person --> (UVAR unifies-with PERSON)",
				     1,
				     3009)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" ?? --> (UVAR)", 1, 3051)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ?Notatype --> (UVAR name 'notatype)",
				     1,
				     3067)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:1619 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3107 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-var',
			    [name, type],
			    
			    [ cond,
			      
			      [ [and, name, type],
				
				[ 'ob$fcreate',
				  
				  [ '#BQ',
				    
				    [ 'UVAR',
				      name,
				      ['QUOTE', ['#COMMA', name]],
				      'unifies-with',
				      ['#COMMA', type]
				    ]
				  ]
				]
			      ],
			      
			      [ type,
				
				[ 'ob$fcreate',
				  
				  [ '#BQ',
				    ['UVAR', 'unifies-with', ['#COMMA', type]]
				  ]
				]
			      ],
			      
			      [ name,
				
				[ 'ob$fcreate',
				  
				  [ '#BQ',
				    ['UVAR', name, ['QUOTE', ['#COMMA', name]]]
				  ]
				]
			      ],
			      [else, ['ob$fcreate', [quote, ['UVAR']]]]
			    ]
			  ]).

% annotating U::MAKE-VAR 
wl: lambda_def(defun,
	      u_make_var,
	      f_u_make_var,
	      [sys_name, type],
	      
	      [ 
		[ cond,
		  
		  [ [and, sys_name, type],
		    
		    [ u_ob_c36_fcreate,
		      
		      [ '#BQ',
			
			[ u_uvar,
			  sys_name,
			  [quote, ['#COMMA', sys_name]],
			  u_unifies_with,
			  ['#COMMA', type]
			]
		      ]
		    ]
		  ],
		  
		  [ type,
		    
		    [ u_ob_c36_fcreate,
		      ['#BQ', [u_uvar, u_unifies_with, ['#COMMA', type]]]
		    ]
		  ],
		  
		  [ sys_name,
		    
		    [ u_ob_c36_fcreate,
		      ['#BQ', [u_uvar, sys_name, [quote, ['#COMMA', sys_name]]]]
		    ]
		  ],
		  [u_else, [u_ob_c36_fcreate, [quote, [u_uvar]]]]
		]
	      ]).


% annotating U::MAKE-VAR 
wl: arglist_info(u_make_var,
		[sys_name, type],
		[Name_Param, Type_Param],
		arginfo{ all:[sys_name, type],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_name, type],
			 opt:0,
			 req:[sys_name, type],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-VAR 
wl: init_args(exact_only, u_make_var).


% annotating U::MAKE-VAR 
f_u_make_var(Name_Param, Type_Param, ElseResult33) :-
	Env=[bv(sys_name, Name_Param), bv(type, Type_Param)],
	(   Name_Param\==[]
	->  IFTEST=Type_Param
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 
				 [ u_uvar,
				   sys_name,
				   [quote, ['#COMMA', sys_name]],
				   u_unifies_with,
				   ['#COMMA', type]
				 ]
			       ],
			       TrueResult36),
	    ElseResult33=TrueResult36
	;   Type_Param\==[]
	->  f_u_ob_c36_fcreate(['#BQ', [u_uvar, u_unifies_with, ['#COMMA', type]]],
			       TrueResult34),
	    ElseResult33=TrueResult34
	;   Name_Param\==[]
	->  f_u_ob_c36_fcreate(
			       [ '#BQ',
				 [u_uvar, sys_name, [quote, ['#COMMA', sys_name]]]
			       ],
			       TrueResult32),
	    ElseResult33=TrueResult32
	;   get_var(Env, u_else, IFTEST27),
	    (   IFTEST27\==[]
	    ->  f_u_ob_c36_fcreate([quote, [u_uvar]], TrueResult30),
		ElseResult33=TrueResult30
	    ;   ElseResult33=[]
	    )
	).
:- set_opv(f_u_make_var, classof, claz_function),
   set_opv(u_make_var, compile_as, kw_function),
   set_opv(u_make_var, function, f_u_make_var),
   DefunResult=u_make_var.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'variable-value',
			    [var, bd],
			    ['bd-lookup', ['variable-name', var], bd]
			  ]).

% annotating U::VARIABLE-VALUE 
wl: lambda_def(defun,
	      u_variable_value,
	      f_u_variable_value,
	      [u_var, u_bd],
	      [[u_bd_lookup, [u_variable_name, u_var], u_bd]]).


% annotating U::VARIABLE-VALUE 
wl: arglist_info(u_variable_value,
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

% annotating U::VARIABLE-VALUE 
wl: init_args(exact_only, u_variable_value).


% annotating U::VARIABLE-VALUE 
f_u_variable_value(Var_Param, Bd_Param, FnResult) :-
	Env=[bv(u_var, Var_Param), bv(u_bd, Bd_Param)],
	f_u_bd_lookup([u_variable_name, u_var], u_bd, Bd),
	Bd=FnResult.
:- set_opv(f_u_variable_value, classof, claz_function),
   set_opv(u_variable_value, compile_as, kw_function),
   set_opv(u_variable_value, function, f_u_variable_value),
   DefunResult=u_variable_value.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3520)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$unify ob1 ob2 bindings):", 1, 3522)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3553)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Unifier for obs", 1, 3555)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (Looping check code taken from the rhapsody matcher by Scott Turner).",
				     1,
				     3573)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3449 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3645)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3647 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*already-matched*', []]).
:- set_var(TLEnv3, setq, u_xx_already_matched_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3677 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*diff?*', []]).
:- set_var(TLEnv3, setq, u_xx_diff_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$unify-dbg',
			    [ob1, ob2, bindings, 'ignore-slots'],
			    ['ndbg-begin'],
			    
			    [ ndbg,
			      '*gate-dbg*',
			      unify,
			      '$STRING'("Call ob$unify: ~A ~A ~A ~A~%"),
			      ob1,
			      ob2,
			      bindings,
			      'ignore-slots'
			    ],
			    
			    [ let,
			      
			      [ 
				[ result,
				  
				  [ 'ob$unify0',
				    ob1,
				    ob2,
				    bindings,
				    'ignore-slots'
				  ]
				]
			      ],
			      
			      [ ndbg,
				'*gate-dbg*',
				unify,
				'$STRING'("Return from ob$unify: ~A~%"),
				result
			      ],
			      ['ndbg-end'],
			      result
			    ]
			  ]).

% annotating U::OB$UNIFY-DBG 
wl: lambda_def(defun,
	      u_ob_c36_unify_dbg,
	      f_u_ob_c36_unify_dbg,
	      [u_ob1, u_ob2, bindings, u_ignore_slots],
	      
	      [ [u_ndbg_begin],
		
		[ u_ndbg,
		  u_xx_gate_dbg_xx,
		  u_unify,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('C'),
			     #\(a),
			     #\(l),
			     #\(l),
			     #\(' '),
			     #\(o),
			     #\(b),
			     #\($),
			     #\(u),
			     #\(n),
			     #\(i),
			     #\(f),
			     #\(y),
			     #\(:),
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
			     #\(~),
			     #\('%')
			   ]),
		  u_ob1,
		  u_ob2,
		  bindings,
		  u_ignore_slots
		],
		
		[ let,
		  
		  [ 
		    [ u_result,
		      [u_ob_c36_unify0, u_ob1, u_ob2, bindings, u_ignore_slots]
		    ]
		  ],
		  
		  [ u_ndbg,
		    u_xx_gate_dbg_xx,
		    u_unify,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('R'),
			       #\(e),
			       #\(t),
			       #\(u),
			       #\(r),
			       #\(n),
			       #\(' '),
			       #\(f),
			       #\(r),
			       #\(o),
			       #\(m),
			       #\(' '),
			       #\(o),
			       #\(b),
			       #\($),
			       #\(u),
			       #\(n),
			       #\(i),
			       #\(f),
			       #\(y),
			       #\(:),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(~),
			       #\('%')
			     ]),
		    u_result
		  ],
		  [u_ndbg_end],
		  u_result
		]
	      ]).


% annotating U::OB$UNIFY-DBG 
wl: arglist_info(u_ob_c36_unify_dbg,
		[u_ob1, u_ob2, bindings, u_ignore_slots],
		[Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY-DBG 
wl: init_args(exact_only, u_ob_c36_unify_dbg).


% annotating U::OB$UNIFY-DBG 
f_u_ob_c36_unify_dbg(Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param, FnResult) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param), bv(bindings, Bindings_Param), bv(u_ignore_slots, Ignore_slots_Param)],
	f_u_ndbg_begin(Ndbg_begin_Ret),
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_unify,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    
			    [ #\('C'),
			      #\(a),
			      #\(l),
			      #\(l),
			      #\(' '),
			      #\(o),
			      #\(b),
			      #\($),
			      #\(u),
			      #\(n),
			      #\(i),
			      #\(f),
			      #\(y),
			      #\(:),
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
			      #\(~),
			      #\('%')
			    ]),
		   u_ob1,
		   u_ob2,
		   bindings,
		   u_ignore_slots
		 ],
		 Ndbg_Ret),
	f_u_ob_c36_unify0(Ob1_Param,
			  Ob2_Param,
			  Bindings_Param,
			  Ignore_slots_Param,
			  Result_Init),
	LEnv=[[bv(u_result, Result_Init)]|Env],
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_unify,
		 
		 [ '$ARRAY'([*],
			    claz_base_character,
			    
			    [ #\('R'),
			      #\(e),
			      #\(t),
			      #\(u),
			      #\(r),
			      #\(n),
			      #\(' '),
			      #\(f),
			      #\(r),
			      #\(o),
			      #\(m),
			      #\(' '),
			      #\(o),
			      #\(b),
			      #\($),
			      #\(u),
			      #\(n),
			      #\(i),
			      #\(f),
			      #\(y),
			      #\(:),
			      #\(' '),
			      #\(~),
			      #\('A'),
			      #\(~),
			      #\('%')
			    ]),
		   u_result
		 ],
		 Ndbg_Ret31),
	f_u_ndbg_end(Ndbg_end_Ret),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_ob_c36_unify_dbg, classof, claz_function),
   set_opv(u_ob_c36_unify_dbg, compile_as, kw_function),
   set_opv(u_ob_c36_unify_dbg, function, f_u_ob_c36_unify_dbg),
   DefunResult=u_ob_c36_unify_dbg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3697 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4013)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3697 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" List of slots which unification should always ignore.",
				     1,
				     4015)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:3697 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4071)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4072 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*permanent-ignore-slots*',
			    
			    [ quote,
			      
			      [ 'top-context',
				value,
				weight,
				offset,
				decay,
				'plan-rule',
				'plan-subgoalnum',
				'input-state?',
				'inference-rule',
				indexes
			      ]
			    ]
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_permanent_ignore_slots_xx,
	   
	   [ u_top_context,
	     u_value,
	     u_weight,
	     u_offset,
	     u_decay,
	     u_plan_rule,
	     u_plan_subgoalnum,
	     u_input_state_c63,
	     u_inference_rule,
	     u_indexes
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4072 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(";;; no no no linked-to-of linked-from-of",
				     1,
				     4215)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4430 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*unify-context*', []]).
:- set_var(TLEnv3, setq, u_xx_unify_context_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4430 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4459)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4430 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This could be made faster by doing types first. Actually, types",
				     1,
				     4461)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4430 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" are done first anyway because they are the first slot.",
				     1,
				     4527)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4430 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4584)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$unify0',
			    [ob1, ob2, bindings, 'ignore-slots'],
			    
			    [ if,
			      
			      [ 'memq?',
				ob2,
				['bd-lookup', ob1, '*already-matched*']
			      ],
			      bindings,
			      
			      [ progn,
				
				[ 'bd-bind!',
				  ob1,
				  
				  [ cons,
				    ob2,
				    ['bd-lookup', ob1, '*already-matched*']
				  ],
				  '*already-matched*'
				],
				
				[ let,
				  
				  [ 
				    [ result,
				      
				      [ cond,
					[['eq?', ob1, ob2], bindings],
					
					[ 
					  [ or,
					    ['special?', ob1],
					    ['special?', ob2]
					  ],
					  
					  [ if,
					    ['special-priority?', ob1, ob2],
					    
					    [ 'ob$unify-special',
					      ob1,
					      ob2,
					      bindings,
					      'ignore-slots',
					      []
					    ],
					    
					    [ 'ob$unify-special',
					      ob2,
					      ob1,
					      bindings,
					      'ignore-slots',
					      t
					    ]
					  ]
					],
					
					[ ['var?', ob1],
					  
					  [ 'ob$unify-var',
					    ob1,
					    ob2,
					    bindings,
					    'ignore-slots',
					    []
					  ]
					],
					
					[ ['var?', ob2],
					  
					  [ 'ob$unify-var',
					    ob2,
					    ob1,
					    bindings,
					    'ignore-slots',
					    t
					  ]
					],
					
					[ 
					  [ and,
					    ['ob?', ob1],
					    ['ob$literal?', ob1]
					  ],
					  []
					],
					
					[ 
					  [ and,
					    ['ob?', ob2],
					    ['ob$literal?', ob2]
					  ],
					  []
					],
					
					[ [and, ['ob?', ob1], ['ob?', ob2]],
					  
					  [ yloop,
					    
					    [ initial,
					      ['unified-slot-indices', []],
					      ['ob2-slots', ['ob$pairs', ob2]],
					      ['constant-slot-index', []],
					      ['last-constant-value', []],
					      ['new-bindings', []],
					      ['found?', []]
					    ],
					    [yfor, cur, in, ['ob$pairs', ob1]],
					    [ywhile, bindings],
					    
					    [ ydo,
					      
					      [ if,
						
						[ and,
						  
						  [ not,
						    
						    [ 'memq?',
						      [car, cur],
						      'ignore-slots'
						    ]
						  ],
						  
						  [ not,
						    
						    [ 'memq?',
						      [car, cur],
						      '*permanent-ignore-slots*'
						    ]
						  ]
						],
						
						[ progn,
						  
						  [ setq,
						    'constant-slot-index',
						    0
						  ],
						  [setq, 'new-bindings', []],
						  [setq, 'found?', []],
						  
						  [ setq,
						    'last-constant-value',
						    []
						  ],
						  
						  [ yloop,
						    
						    [ yfor,
						      'constant-slot-value',
						      in,
						      'ob2-slots'
						    ],
						    [yuntil, 'found?'],
						    
						    [ ydo,
						      
						      [ if,
							
							[ and,
							  
							  [ 'eq?',
							    [car, cur],
							    
							    [ 'slots-name',
							      'constant-slot-value'
							    ]
							  ],
							  
							  [ not,
							    
							    [ 'memq?',
							      'constant-slot-index',
							      'unified-slot-indices'
							    ]
							  ],
							  
							  [ setq,
							    'last-constant-value',
							    
							    [ 'slots-value',
							      'constant-slot-value'
							    ]
							  ],
							  
							  [ setq,
							    'new-bindings',
							    
							    [ if,
							      
							      [ 'eq?',
								[cadr, cur],
								
								[ 'slots-value',
								  'constant-slot-value'
								]
							      ],
							      bindings,
							      
							      [ 'ob$unify2',
								[cadr, cur],
								
								[ 'slots-value',
								  'constant-slot-value'
								],
								bindings,
								'ignore-slots'
							      ]
							    ]
							  ]
							],
							
							[ progn,
							  [setq, 'found?', t],
							  
							  [ setq,
							    'unified-slot-indices',
							    
							    [ cons,
							      'constant-slot-index',
							      'unified-slot-indices'
							    ]
							  ]
							]
						      ],
						      
						      [ 'increment-me',
							'constant-slot-index'
						      ]
						    ]
						  ],
						  
						  [ if,
						    'found?',
						    
						    [ setq,
						      bindings,
						      'new-bindings'
						    ],
						    
						    [ if,
						      '*diff?*',
						      
						      [ setq,
							bindings,
							
							[ 'bd-bind',
							  ['slots-name', cur],
							  
							  [ list,
							    [cadr, cur],
							    'last-constant-value'
							  ],
							  bindings
							]
						      ],
						      [setq, bindings, []]
						    ]
						  ]
						]
					      ]
					    ],
					    [yresult, bindings]
					  ]
					],
					[else, []]
				      ]
				    ]
				  ],
				  
				  [ if,
				    result,
				    result,
				    
				    [ progn,
				      
				      [ 'bd-bind!',
					ob1,
					
					[ 'delq!',
					  ob2,
					  
					  [ 'bd-lookup',
					    ob1,
					    '*already-matched*'
					  ]
					],
					'*already-matched*'
				      ],
				      
				      [ 'bd-bind!',
					ob2,
					
					[ 'delq!',
					  ob1,
					  
					  [ 'bd-lookup',
					    ob2,
					    '*already-matched*'
					  ]
					],
					'*already-matched*'
				      ],
				      []
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY0 
wl: lambda_def(defun,
	      u_ob_c36_unify0,
	      f_u_ob_c36_unify0,
	      [u_ob1, u_ob2, bindings, u_ignore_slots],
	      
	      [ 
		[ if,
		  
		  [ u_memq_c63,
		    u_ob2,
		    [u_bd_lookup, u_ob1, u_xx_already_matched_xx]
		  ],
		  bindings,
		  
		  [ progn,
		    
		    [ u_bd_bind_c33,
		      u_ob1,
		      [cons, u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx]],
		      u_xx_already_matched_xx
		    ],
		    
		    [ let,
		      
		      [ 
			[ u_result,
			  
			  [ cond,
			    [[u_eq_c63, u_ob1, u_ob2], bindings],
			    
			    [ [or, [u_special_c63, u_ob1], [u_special_c63, u_ob2]],
			      
			      [ if,
				[u_special_priority_c63, u_ob1, u_ob2],
				
				[ u_ob_c36_unify_special,
				  u_ob1,
				  u_ob2,
				  bindings,
				  u_ignore_slots,
				  []
				],
				
				[ u_ob_c36_unify_special,
				  u_ob2,
				  u_ob1,
				  bindings,
				  u_ignore_slots,
				  t
				]
			      ]
			    ],
			    
			    [ [u_var_c63, u_ob1],
			      
			      [ u_ob_c36_unify_var,
				u_ob1,
				u_ob2,
				bindings,
				u_ignore_slots,
				[]
			      ]
			    ],
			    
			    [ [u_var_c63, u_ob2],
			      
			      [ u_ob_c36_unify_var,
				u_ob2,
				u_ob1,
				bindings,
				u_ignore_slots,
				t
			      ]
			    ],
			    
			    [ 
			      [ and,
				[u_ob_c63, u_ob1],
				[u_ob_c36_literal_c63, u_ob1]
			      ],
			      []
			    ],
			    
			    [ 
			      [ and,
				[u_ob_c63, u_ob2],
				[u_ob_c36_literal_c63, u_ob2]
			      ],
			      []
			    ],
			    
			    [ [and, [u_ob_c63, u_ob1], [u_ob_c63, u_ob2]],
			      
			      [ u_yloop,
				
				[ u_initial,
				  [u_unified_slot_indices, []],
				  [u_ob2_slots, [u_ob_c36_pairs, u_ob2]],
				  [u_constant_slot_index, []],
				  [u_last_constant_value, []],
				  [u_new_bindings, []],
				  [u_found_c63, []]
				],
				[u_yfor, u_cur, u_in, [u_ob_c36_pairs, u_ob1]],
				[u_ywhile, bindings],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ not,
					
					[ u_memq_c63,
					  [car, u_cur],
					  u_ignore_slots
					]
				      ],
				      
				      [ not,
					
					[ u_memq_c63,
					  [car, u_cur],
					  u_xx_permanent_ignore_slots_xx
					]
				      ]
				    ],
				    
				    [ progn,
				      [setq, u_constant_slot_index, 0],
				      [setq, u_new_bindings, []],
				      [setq, u_found_c63, []],
				      [setq, u_last_constant_value, []],
				      
				      [ u_yloop,
					
					[ u_yfor,
					  u_constant_slot_value,
					  u_in,
					  u_ob2_slots
					],
					[u_yuntil, u_found_c63],
					
					[ u_ydo,
					  
					  [ if,
					    
					    [ and,
					      
					      [ u_eq_c63,
						[car, u_cur],
						
						[ u_slots_name,
						  u_constant_slot_value
						]
					      ],
					      
					      [ not,
						
						[ u_memq_c63,
						  u_constant_slot_index,
						  u_unified_slot_indices
						]
					      ],
					      
					      [ setq,
						u_last_constant_value,
						
						[ u_slots_value,
						  u_constant_slot_value
						]
					      ],
					      
					      [ setq,
						u_new_bindings,
						
						[ if,
						  
						  [ u_eq_c63,
						    [cadr, u_cur],
						    
						    [ u_slots_value,
						      u_constant_slot_value
						    ]
						  ],
						  bindings,
						  
						  [ u_ob_c36_unify2,
						    [cadr, u_cur],
						    
						    [ u_slots_value,
						      u_constant_slot_value
						    ],
						    bindings,
						    u_ignore_slots
						  ]
						]
					      ]
					    ],
					    
					    [ progn,
					      [setq, u_found_c63, t],
					      
					      [ setq,
						u_unified_slot_indices,
						
						[ cons,
						  u_constant_slot_index,
						  u_unified_slot_indices
						]
					      ]
					    ]
					  ],
					  
					  [ u_increment_me,
					    u_constant_slot_index
					  ]
					]
				      ],
				      
				      [ if,
					u_found_c63,
					[setq, bindings, u_new_bindings],
					
					[ if,
					  u_xx_diff_c63_xx,
					  
					  [ setq,
					    bindings,
					    
					    [ u_bd_bind,
					      [u_slots_name, u_cur],
					      
					      [ list,
						[cadr, u_cur],
						u_last_constant_value
					      ],
					      bindings
					    ]
					  ],
					  [setq, bindings, []]
					]
				      ]
				    ]
				  ]
				],
				[u_yresult, bindings]
			      ]
			    ],
			    [u_else, []]
			  ]
			]
		      ],
		      
		      [ if,
			u_result,
			u_result,
			
			[ progn,
			  
			  [ u_bd_bind_c33,
			    u_ob1,
			    
			    [ u_delq_c33,
			      u_ob2,
			      [u_bd_lookup, u_ob1, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx
			  ],
			  
			  [ u_bd_bind_c33,
			    u_ob2,
			    
			    [ u_delq_c33,
			      u_ob1,
			      [u_bd_lookup, u_ob2, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx
			  ],
			  []
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$UNIFY0 
wl: arglist_info(u_ob_c36_unify0,
		[u_ob1, u_ob2, bindings, u_ignore_slots],
		[Ob1_Param, Ob2_Param, Bindings_Get51, Ignore_slots_Param],
		arginfo{ all:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 opt:0,
			 req:[u_ob1, u_ob2, bindings, u_ignore_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY0 
wl: init_args(exact_only, u_ob_c36_unify0).


% annotating U::OB$UNIFY0 
f_u_ob_c36_unify0(Ob1_Param, Ob2_Param, Bindings_Get51, Ignore_slots_Param, FnResult) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param), bv(bindings, Bindings_Get51), bv(u_ignore_slots, Ignore_slots_Param)],
	f_u_memq_c63(u_ob2, [u_bd_lookup, u_ob1, u_xx_already_matched_xx], IFTEST),
	(   IFTEST\==[]
	->  FnResult=Bindings_Get51
	;   f_u_bd_bind_c33(u_ob1,
			    
			    [ cons,
			      u_ob2,
			      [u_bd_lookup, u_ob1, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx,
			    Xx_already_matched_xx),
	    f_u_eq_c63(u_ob1, u_ob2, IFTEST23),
	    (   IFTEST23\==[]
	    ->  ElseResult75=Bindings_Get51
	    ;   (   f_u_special_c63(u_ob1, FORM1_Res),
		    FORM1_Res\==[],
		    IFTEST26=FORM1_Res
		->  true
		;   f_u_special_c63(u_ob2, Special_c63_Ret),
		    IFTEST26=Special_c63_Ret
		),
		(   IFTEST26\==[]
		->  f_u_special_priority_c63(u_ob1, u_ob2, IFTEST29),
		    (   IFTEST29\==[]
		    ->  f_u_ob_c36_unify_special(Ob1_Param,
						 Ob2_Param,
						 Bindings_Get51,
						 Ignore_slots_Param,
						 [],
						 TrueResult),
			ElseResult75=TrueResult
		    ;   f_u_ob_c36_unify_special(Ob2_Param,
						 Ob1_Param,
						 Bindings_Get51,
						 Ignore_slots_Param,
						 t,
						 ElseResult),
			ElseResult75=ElseResult
		    )
		;   f_u_var_c63(u_ob1, IFTEST41),
		    (   IFTEST41\==[]
		    ->  f_u_ob_c36_unify_var(Ob1_Param,
					     Ob2_Param,
					     Bindings_Get51,
					     Ignore_slots_Param,
					     [],
					     TrueResult80),
			ElseResult75=TrueResult80
		    ;   f_u_var_c63(u_ob2, IFTEST47),
			(   IFTEST47\==[]
			->  f_u_ob_c36_unify_var(Ob2_Param,
						 Ob1_Param,
						 Bindings_Get51,
						 Ignore_slots_Param,
						 t,
						 TrueResult78),
			    ElseResult75=TrueResult78
			;   f_u_ob_c63(u_ob1, IFTEST55),
			    (   IFTEST55\==[]
			    ->  f_u_ob_c36_literal_c63(Ob1_Param, TrueResult58),
				IFTEST53=TrueResult58
			    ;   IFTEST53=[]
			    ),
			    (   IFTEST53\==[]
			    ->  ElseResult75=[]
			    ;   f_u_ob_c63(u_ob2, IFTEST61),
				(   IFTEST61\==[]
				->  f_u_ob_c36_literal_c63(Ob2_Param,
							   TrueResult64),
				    IFTEST59=TrueResult64
				;   IFTEST59=[]
				),
				(   IFTEST59\==[]
				->  ElseResult75=[]
				;   f_u_ob_c63(u_ob1, IFTEST67),
				    (   IFTEST67\==[]
				    ->  f_u_ob_c63(u_ob2, TrueResult69),
					IFTEST65=TrueResult69
				    ;   IFTEST65=[]
				    ),
				    (   IFTEST65\==[]
				    ->  f_u_yloop(
						  [ 
						    [ u_initial,
						      
						      [ u_unified_slot_indices,
							[]
						      ],
						      
						      [ u_ob2_slots,
							[u_ob_c36_pairs, u_ob2]
						      ],
						      
						      [ u_constant_slot_index,
							[]
						      ],
						      
						      [ u_last_constant_value,
							[]
						      ],
						      [u_new_bindings, []],
						      [u_found_c63, []]
						    ],
						    
						    [ u_yfor,
						      u_cur,
						      u_in,
						      [u_ob_c36_pairs, u_ob1]
						    ],
						    [u_ywhile, bindings],
						    
						    [ u_ydo,
						      
						      [ if,
							
							[ and,
							  
							  [ not,
							    
							    [ u_memq_c63,
							      [car, u_cur],
							      u_ignore_slots
							    ]
							  ],
							  
							  [ not,
							    
							    [ u_memq_c63,
							      [car, u_cur],
							      u_xx_permanent_ignore_slots_xx
							    ]
							  ]
							],
							
							[ progn,
							  
							  [ setq,
							    u_constant_slot_index,
							    0
							  ],
							  
							  [ setq,
							    u_new_bindings,
							    []
							  ],
							  
							  [ setq,
							    u_found_c63,
							    []
							  ],
							  
							  [ setq,
							    u_last_constant_value,
							    []
							  ],
							  
							  [ u_yloop,
							    
							    [ u_yfor,
							      u_constant_slot_value,
							      u_in,
							      u_ob2_slots
							    ],
							    
							    [ u_yuntil,
							      u_found_c63
							    ],
							    
							    [ u_ydo,
							      
							      [ if,
								
								[ and,
								  
								  [ u_eq_c63,
								    [car, u_cur],
								    
								    [ u_slots_name,
								      u_constant_slot_value
								    ]
								  ],
								  
								  [ not,
								    
								    [ u_memq_c63,
								      u_constant_slot_index,
								      u_unified_slot_indices
								    ]
								  ],
								  
								  [ setq,
								    u_last_constant_value,
								    
								    [ u_slots_value,
								      u_constant_slot_value
								    ]
								  ],
								  
								  [ setq,
								    u_new_bindings,
								    
								    [ if,
								      
								      [ u_eq_c63,
									[cadr, u_cur],
									
									[ u_slots_value,
									  u_constant_slot_value
									]
								      ],
								      bindings,
								      
								      [ u_ob_c36_unify2,
									[cadr, u_cur],
									
									[ u_slots_value,
									  u_constant_slot_value
									],
									bindings,
									u_ignore_slots
								      ]
								    ]
								  ]
								],
								
								[ progn,
								  
								  [ setq,
								    u_found_c63,
								    t
								  ],
								  
								  [ setq,
								    u_unified_slot_indices,
								    
								    [ cons,
								      u_constant_slot_index,
								      u_unified_slot_indices
								    ]
								  ]
								]
							      ],
							      
							      [ u_increment_me,
								u_constant_slot_index
							      ]
							    ]
							  ],
							  
							  [ if,
							    u_found_c63,
							    
							    [ setq,
							      bindings,
							      u_new_bindings
							    ],
							    
							    [ if,
							      u_xx_diff_c63_xx,
							      
							      [ setq,
								bindings,
								
								[ u_bd_bind,
								  
								  [ u_slots_name,
								    u_cur
								  ],
								  
								  [ list,
								    [cadr, u_cur],
								    u_last_constant_value
								  ],
								  bindings
								]
							      ],
							      [setq, bindings, []]
							    ]
							  ]
							]
						      ]
						    ],
						    [u_yresult, bindings]
						  ],
						  TrueResult74),
					ElseResult75=TrueResult74
				    ;   get_var(Env, u_else, IFTEST70),
					(   IFTEST70\==[]
					->  ElseResult75=[]
					;   ElseResult75=[]
					)
				    )
				)
			    )
			)
		    )
		)
	    ),
	    LEnv=[[bv(u_result, ElseResult75)]|Env],
	    get_var(LEnv, u_result, IFTEST87),
	    (   IFTEST87\==[]
	    ->  get_var(LEnv, u_result, Result_Get91),
		FnResult=Result_Get91
	    ;   f_u_bd_bind_c33(u_ob1,
				
				[ u_delq_c33,
				  u_ob2,
				  [u_bd_lookup, u_ob1, u_xx_already_matched_xx]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx98),
		f_u_bd_bind_c33(u_ob2,
				
				[ u_delq_c33,
				  u_ob1,
				  [u_bd_lookup, u_ob2, u_xx_already_matched_xx]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx99),
		FnResult=[]
	    )
	).
:- set_opv(f_u_ob_c36_unify0, classof, claz_function),
   set_opv(u_ob_c36_unify0, compile_as, kw_function),
   set_opv(u_ob_c36_unify0, function, f_u_ob_c36_unify0),
   DefunResult=u_ob_c36_unify0.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The below would introduce a semantics which does not conform",
				     1,
				     4834)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to unification asymmetry.", 1, 4897)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("       (bd-bind! ob2", 1, 4925)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 (cons ob1 (bd-lookup ob2 *already-matched*))",
				     1,
				     4947)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                 *already-matched*)",
				     1,
				     5010)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:4585 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" was reverse", 46, 5981)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$unify-special',
			    [ob1, ob2, bindings, 'ignore-slots', 'reverse?'],
			    
			    [ cond,
			      
			      [ ['ty$instance?', ob1, [quote, uand]],
				
				[ yloop,
				  [yfor, item, in, ['ob$gets', ob1, [quote, obj]]],
				  [ywhile, bindings],
				  
				  [ ydo,
				    
				    [ setq,
				      bindings,
				      
				      [ if,
					'reverse?',
					
					[ 'ob$unify2',
					  ob2,
					  item,
					  bindings,
					  'ignore-slots'
					],
					
					[ 'ob$unify2',
					  item,
					  ob2,
					  bindings,
					  'ignore-slots'
					]
				      ]
				    ]
				  ],
				  [yresult, bindings]
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, uor]],
				
				[ yloop,
				  [yfor, item, in, ['ob$gets', ob1, [quote, obj]]],
				  [initial, ['new-bindings', []]],
				  [yuntil, 'new-bindings'],
				  
				  [ ydo,
				    
				    [ setq,
				      'new-bindings',
				      
				      [ if,
					'reverse?',
					
					[ 'ob$unify2',
					  ob2,
					  item,
					  bindings,
					  'ignore-slots'
					],
					
					[ 'ob$unify2',
					  item,
					  ob2,
					  bindings,
					  'ignore-slots'
					]
				      ]
				    ]
				  ],
				  [yresult, 'new-bindings']
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, unot]],
				
				[ if,
				  
				  [ if,
				    'reverse?',
				    
				    [ 'ob$unify2',
				      ob2,
				      ['ob$get', ob1, [quote, obj]],
				      bindings,
				      'ignore-slots'
				    ],
				    
				    [ 'ob$unify2',
				      ['ob$get', ob1, [quote, obj]],
				      ob2,
				      bindings,
				      'ignore-slots'
				    ]
				  ],
				  [],
				  bindings
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, udist]],
				
				[ let,
				  
				  [ 
				    [ val1,
				      
				      [ if,
					
					[ not,
					  ['var?', ['ob$get', ob1, [quote, obj]]]
					],
					['ob$get', ob1, [quote, obj]],
					
					[ 'bd-hyper-lookup',
					  
					  [ 'variable-name',
					    ['ob$get', ob1, [quote, obj]]
					  ],
					  bindings
					]
				      ]
				    ],
				    
				    [ val2,
				      
				      [ if,
					[not, ['var?', ob2]],
					ob2,
					
					[ 'bd-hyper-lookup',
					  ['variable-name', ob2],
					  bindings
					]
				      ]
				    ]
				  ],
				  
				  [ if,
				    
				    [ and,
				      ['ob?', val1],
				      ['ob?', val2],
				      [not, ['var?', val1]],
				      [not, ['var?', val2]]
				    ],
				    [if, ['neq?', val1, val2], bindings, []],
				    bindings
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, uproc]],
				
				[ if,
				  ['eq?', ob2, [quote, 'uproc-answer-true']],
				  bindings,
				  
				  [ 'ob$unify-proc',
				    ob2,
				    ['ob$get', ob1, [quote, proc]],
				    bindings
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, 'uempty-slots']],
				
				[ if,
				  
				  [ 'every?',
				    
				    [ lambda,
				      ['slot-name'],
				      ['null?', ['ob$gets', ob2, 'slot-name']]
				    ],
				    ['ob$get', ob1, [quote, slots]]
				  ],
				  bindings,
				  []
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, 'uignore-slots']],
				
				[ if,
				  'reverse?',
				  
				  [ 'ob$unify2',
				    ob2,
				    ['ob$get', ob1, [quote, pattern]],
				    bindings,
				    
				    [ append,
				      'ignore-slots',
				      ['ob$get', ob1, [quote, slots]]
				    ]
				  ],
				  
				  [ 'ob$unify2',
				    ['ob$get', ob1, [quote, pattern]],
				    ob2,
				    bindings,
				    
				    [ append,
				      'ignore-slots',
				      ['ob$get', ob1, [quote, slots]]
				    ]
				  ]
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, upath]],
				
				[ 'ob$path',
				  ob2,
				  ['ob$get', ob1, [quote, pattern]],
				  ['ob$get', ob1, [quote, path]],
				  bindings
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, uolpath]],
				
				[ 'ol-path',
				  ob2,
				  ['ob$get', ob1, [quote, pattern]],
				  ['ob$get', ob1, [quote, link]],
				  ['ob$get', ob1, [quote, direction]],
				  '*unify-context*',
				  [],
				  bindings
				]
			      ],
			      
			      [ ['ty$instance?', ob1, [quote, ueval]],
				
				[ 'ob$eval',
				  ['ob$get', ob1, [quote, proc]],
				  bindings
				]
			      ],
			      [['ty$instance?', ob1, [quote, ucode]], bindings],
			      
			      [ else,
				
				[ error,
				  '$STRING'("ob$unify: unknown special!! ~A"),
				  ob1
				]
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY-SPECIAL 
wl: lambda_def(defun,
	      u_ob_c36_unify_special,
	      f_u_ob_c36_unify_special,
	      [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63],
	      
	      [ 
		[ cond,
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_uand]],
		    
		    [ u_yloop,
		      [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]],
		      [u_ywhile, bindings],
		      
		      [ u_ydo,
			
			[ setq,
			  bindings,
			  
			  [ if,
			    u_reverse_c63,
			    
			    [ u_ob_c36_unify2,
			      u_ob2,
			      item,
			      bindings,
			      u_ignore_slots
			    ],
			    
			    [ u_ob_c36_unify2,
			      item,
			      u_ob2,
			      bindings,
			      u_ignore_slots
			    ]
			  ]
			]
		      ],
		      [u_yresult, bindings]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_uor]],
		    
		    [ u_yloop,
		      [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]],
		      [u_initial, [u_new_bindings, []]],
		      [u_yuntil, u_new_bindings],
		      
		      [ u_ydo,
			
			[ setq,
			  u_new_bindings,
			  
			  [ if,
			    u_reverse_c63,
			    
			    [ u_ob_c36_unify2,
			      u_ob2,
			      item,
			      bindings,
			      u_ignore_slots
			    ],
			    
			    [ u_ob_c36_unify2,
			      item,
			      u_ob2,
			      bindings,
			      u_ignore_slots
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_new_bindings]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_unot]],
		    
		    [ if,
		      
		      [ if,
			u_reverse_c63,
			
			[ u_ob_c36_unify2,
			  u_ob2,
			  [u_ob_c36_get, u_ob1, [quote, u_obj]],
			  bindings,
			  u_ignore_slots
			],
			
			[ u_ob_c36_unify2,
			  [u_ob_c36_get, u_ob1, [quote, u_obj]],
			  u_ob2,
			  bindings,
			  u_ignore_slots
			]
		      ],
		      [],
		      bindings
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_udist]],
		    
		    [ let,
		      
		      [ 
			[ u_val1,
			  
			  [ if,
			    
			    [ not,
			      [u_var_c63, [u_ob_c36_get, u_ob1, [quote, u_obj]]]
			    ],
			    [u_ob_c36_get, u_ob1, [quote, u_obj]],
			    
			    [ u_bd_hyper_lookup,
			      
			      [ u_variable_name,
				[u_ob_c36_get, u_ob1, [quote, u_obj]]
			      ],
			      bindings
			    ]
			  ]
			],
			
			[ u_val2,
			  
			  [ if,
			    [not, [u_var_c63, u_ob2]],
			    u_ob2,
			    
			    [ u_bd_hyper_lookup,
			      [u_variable_name, u_ob2],
			      bindings
			    ]
			  ]
			]
		      ],
		      
		      [ if,
			
			[ and,
			  [u_ob_c63, u_val1],
			  [u_ob_c63, u_val2],
			  [not, [u_var_c63, u_val1]],
			  [not, [u_var_c63, u_val2]]
			],
			[if, [u_neq_c63, u_val1, u_val2], bindings, []],
			bindings
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_uproc]],
		    
		    [ if,
		      [u_eq_c63, u_ob2, [quote, u_uproc_answer_true]],
		      bindings,
		      
		      [ u_ob_c36_unify_proc,
			u_ob2,
			[u_ob_c36_get, u_ob1, [quote, u_proc]],
			bindings
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_uempty_slots]],
		    
		    [ if,
		      
		      [ u_every_c63,
			
			[ lambda,
			  [u_slot_name],
			  [u_null_c63, [u_ob_c36_gets, u_ob2, u_slot_name]]
			],
			[u_ob_c36_get, u_ob1, [quote, sys_slots]]
		      ],
		      bindings,
		      []
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_uignore_slots]],
		    
		    [ if,
		      u_reverse_c63,
		      
		      [ u_ob_c36_unify2,
			u_ob2,
			[u_ob_c36_get, u_ob1, [quote, u_pattern]],
			bindings,
			
			[ append,
			  u_ignore_slots,
			  [u_ob_c36_get, u_ob1, [quote, sys_slots]]
			]
		      ],
		      
		      [ u_ob_c36_unify2,
			[u_ob_c36_get, u_ob1, [quote, u_pattern]],
			u_ob2,
			bindings,
			
			[ append,
			  u_ignore_slots,
			  [u_ob_c36_get, u_ob1, [quote, sys_slots]]
			]
		      ]
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_upath]],
		    
		    [ u_ob_c36_path,
		      u_ob2,
		      [u_ob_c36_get, u_ob1, [quote, u_pattern]],
		      [u_ob_c36_get, u_ob1, [quote, u_path]],
		      bindings
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_uolpath]],
		    
		    [ u_ol_path,
		      u_ob2,
		      [u_ob_c36_get, u_ob1, [quote, u_pattern]],
		      [u_ob_c36_get, u_ob1, [quote, u_link]],
		      [u_ob_c36_get, u_ob1, [quote, u_direction]],
		      u_xx_unify_context_xx,
		      [],
		      bindings
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_ob1, [quote, u_ueval]],
		    
		    [ u_ob_c36_eval,
		      [u_ob_c36_get, u_ob1, [quote, u_proc]],
		      bindings
		    ]
		  ],
		  [[u_ty_c36_instance_c63, u_ob1, [quote, u_ucode]], bindings],
		  
		  [ u_else,
		    
		    [ error,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(o),
				 #\(b),
				 #\($),
				 #\(u),
				 #\(n),
				 #\(i),
				 #\(f),
				 #\(y),
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
				 #\(s),
				 #\(p),
				 #\(e),
				 #\(c),
				 #\(i),
				 #\(a),
				 #\(l),
				 #\(!),
				 #\(!),
				 #\(' '),
				 #\(~),
				 #\('A')
			       ]),
		      u_ob1
		    ]
		  ]
		]
	      ]).


% annotating U::OB$UNIFY-SPECIAL 
wl: arglist_info(u_ob_c36_unify_special,
		[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63],
		
		[ Ob1_Param,
		  Ob2_Param,
		  Bindings_Get127,
		  Ignore_slots_Param,
		  Reverse_c63_Param
		],
		arginfo{ all:
			     [ u_ob1,
			       u_ob2,
			       bindings,
			       u_ignore_slots,
			       u_reverse_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob1,
				 u_ob2,
				 bindings,
				 u_ignore_slots,
				 u_reverse_c63
			       ],
			 opt:0,
			 req:
			     [ u_ob1,
			       u_ob2,
			       bindings,
			       u_ignore_slots,
			       u_reverse_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY-SPECIAL 
wl: init_args(exact_only, u_ob_c36_unify_special).


% annotating U::OB$UNIFY-SPECIAL 
f_u_ob_c36_unify_special(Ob1_Param, Ob2_Param, Bindings_Get127, Ignore_slots_Param, Reverse_c63_Param, TrueResult75) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param), bv(bindings, Bindings_Get127), bv(u_ignore_slots, Ignore_slots_Param), bv(u_reverse_c63, Reverse_c63_Param)],
	f_u_ty_c36_instance_c63(Ob1_Param, u_uand, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_yfor, item, u_in, [u_ob_c36_gets, u_ob1, [quote, u_obj]]],
			[u_ywhile, bindings],
			
			[ u_ydo,
			  
			  [ setq,
			    bindings,
			    
			    [ if,
			      u_reverse_c63,
			      
			      [ u_ob_c36_unify2,
				u_ob2,
				item,
				bindings,
				u_ignore_slots
			      ],
			      
			      [ u_ob_c36_unify2,
				item,
				u_ob2,
				bindings,
				u_ignore_slots
			      ]
			    ]
			  ]
			],
			[u_yresult, bindings]
		      ],
		      TrueResult154),
	    TrueResult75=TrueResult154
	;   f_u_ty_c36_instance_c63(Ob1_Param, u_uor, IFTEST23),
	    (   IFTEST23\==[]
	    ->  f_u_yloop(
			  [ 
			    [ u_yfor,
			      item,
			      u_in,
			      [u_ob_c36_gets, u_ob1, [quote, u_obj]]
			    ],
			    [u_initial, [u_new_bindings, []]],
			    [u_yuntil, u_new_bindings],
			    
			    [ u_ydo,
			      
			      [ setq,
				u_new_bindings,
				
				[ if,
				  u_reverse_c63,
				  
				  [ u_ob_c36_unify2,
				    u_ob2,
				    item,
				    bindings,
				    u_ignore_slots
				  ],
				  
				  [ u_ob_c36_unify2,
				    item,
				    u_ob2,
				    bindings,
				    u_ignore_slots
				  ]
				]
			      ]
			    ],
			    [u_yresult, u_new_bindings]
			  ],
			  TrueResult152),
		TrueResult75=TrueResult152
	    ;   f_u_ty_c36_instance_c63(Ob1_Param, u_unot, IFTEST26),
		(   IFTEST26\==[]
		->  (   Reverse_c63_Param\==[]
		    ->  f_u_ob_c36_unify2(u_ob2,
					  [u_ob_c36_get, u_ob1, [quote, u_obj]],
					  bindings,
					  u_ignore_slots,
					  TrueResult),
			IFTEST29=TrueResult
		    ;   f_u_ob_c36_unify2([u_ob_c36_get, u_ob1, [quote, u_obj]],
					  u_ob2,
					  bindings,
					  u_ignore_slots,
					  ElseResult),
			IFTEST29=ElseResult
		    ),
		    (   IFTEST29\==[]
		    ->  TrueResult75=[]
		    ;   TrueResult75=Bindings_Get127
		    )
		;   f_u_ty_c36_instance_c63(Ob1_Param, u_udist, IFTEST38),
		    (   IFTEST38\==[]
		    ->  f_u_var_c63([u_ob_c36_get, u_ob1, [quote, u_obj]],
				    PredArgResult),
			(   PredArgResult==[]
			->  f_u_ob_c36_get(Ob1_Param, u_obj, TrueResult47),
			    Val1_Init=TrueResult47
			;   f_u_bd_hyper_lookup(
						[ u_variable_name,
						  
						  [ u_ob_c36_get,
						    u_ob1,
						    [quote, u_obj]
						  ]
						],
						bindings,
						ElseResult48),
			    Val1_Init=ElseResult48
			),
			f_u_var_c63(u_ob2, PredArgResult51),
			(   PredArgResult51==[]
			->  Val2_Init=Ob2_Param
			;   f_u_bd_hyper_lookup([u_variable_name, u_ob2],
						bindings,
						ElseResult54),
			    Val2_Init=ElseResult54
			),
			LEnv=[[bv(u_val1, Val1_Init), bv(u_val2, Val2_Init)]|Env],
			f_u_ob_c63(u_val1, IFTEST59),
			(   IFTEST59\==[]
			->  f_u_ob_c63(u_val2, IFTEST61),
			    (   IFTEST61\==[]
			    ->  f_u_var_c63(u_val1, PredArgResult65),
				(   PredArgResult65==[]
				->  f_u_var_c63(u_val2, Not_Param),
				    cl_not(Not_Param, TrueResult66),
				    IFTEST57=TrueResult66
				;   IFTEST57=[]
				)
			    ;   IFTEST57=[]
			    )
			;   IFTEST57=[]
			),
			(   IFTEST57\==[]
			->  f_u_neq_c63(u_val1, u_val2, IFTEST69),
			    (   IFTEST69\==[]
			    ->  TrueResult75=Bindings_Get127
			    ;   TrueResult75=[]
			    )
			;   TrueResult75=Bindings_Get127
			)
		    ;   f_u_ty_c36_instance_c63(Ob1_Param, u_uproc, IFTEST77),
			(   IFTEST77\==[]
			->  f_u_eq_c63(u_ob2,
				       [quote, u_uproc_answer_true],
				       IFTEST80),
			    (   IFTEST80\==[]
			    ->  TrueResult75=Bindings_Get127
			    ;   f_u_ob_c36_get(Ob1_Param, u_proc, Proc),
				f_u_ob_c36_unify_proc(Ob2_Param,
						      Proc,
						      Bindings_Get127,
						      ElseResult87),
				TrueResult75=ElseResult87
			    )
			;   f_u_ty_c36_instance_c63(Ob1_Param,
						    u_uempty_slots,
						    IFTEST88),
			    (   IFTEST88\==[]
			    ->  f_u_every_c63(
					      [ lambda,
						[u_slot_name],
						
						[ u_null_c63,
						  
						  [ u_ob_c36_gets,
						    u_ob2,
						    u_slot_name
						  ]
						]
					      ],
					      
					      [ u_ob_c36_get,
						u_ob1,
						[quote, sys_slots]
					      ],
					      IFTEST91),
				(   IFTEST91\==[]
				->  TrueResult75=Bindings_Get127
				;   TrueResult75=[]
				)
			    ;   f_u_ty_c36_instance_c63(Ob1_Param,
							u_uignore_slots,
							IFTEST95),
				(   IFTEST95\==[]
				->  (   Reverse_c63_Param\==[]
				    ->  f_u_ob_c36_unify2(u_ob2,
							  
							  [ u_ob_c36_get,
							    u_ob1,
							    [quote, u_pattern]
							  ],
							  bindings,
							  
							  [ append,
							    u_ignore_slots,
							    
							    [ u_ob_c36_get,
							      u_ob1,
							      [quote, sys_slots]
							    ]
							  ],
							  TrueResult101),
					TrueResult75=TrueResult101
				    ;   f_u_ob_c36_unify2(
							  [ u_ob_c36_get,
							    u_ob1,
							    [quote, u_pattern]
							  ],
							  u_ob2,
							  bindings,
							  
							  [ append,
							    u_ignore_slots,
							    
							    [ u_ob_c36_get,
							      u_ob1,
							      [quote, sys_slots]
							    ]
							  ],
							  ElseResult102),
					TrueResult75=ElseResult102
				    )
				;   f_u_ty_c36_instance_c63(Ob1_Param,
							    u_upath,
							    IFTEST103),
				    (   IFTEST103\==[]
				    ->  f_u_ob_c36_get(Ob1_Param,
						       u_pattern,
						       Pattern),
					f_u_ob_c36_get(Ob1_Param, u_path, Path),
					f_u_ob_c36_path(Ob2_Param,
							Pattern,
							Path,
							Bindings_Get127,
							TrueResult140),
					TrueResult75=TrueResult140
				    ;   f_u_ty_c36_instance_c63(Ob1_Param,
								u_uolpath,
								IFTEST110),
					(   IFTEST110\==[]
					->  f_u_ob_c36_get(Ob1_Param,
							   u_pattern,
							   Pattern161),
					    f_u_ob_c36_get(Ob1_Param,
							   u_link,
							   Link),
					    f_u_ob_c36_get(Ob1_Param,
							   u_direction,
							   Direction),
					    get_var(Env,
						    u_xx_unify_context_xx,
						    Xx_unify_context_xx_Get),
					    f_u_ol_path(Ob2_Param,
							Pattern161,
							Link,
							Direction,
							Xx_unify_context_xx_Get,
							[],
							Bindings_Get127,
							TrueResult138),
					    TrueResult75=TrueResult138
					;   f_u_ty_c36_instance_c63(Ob1_Param,
								    u_ueval,
								    IFTEST119),
					    (   IFTEST119\==[]
					    ->  f_u_ob_c36_get(Ob1_Param,
							       u_proc,
							       Proc164),
						f_u_ob_c36_eval(Proc164,
								Bindings_Get127,
								TrueResult136),
						TrueResult75=TrueResult136
					    ;   f_u_ty_c36_instance_c63(Ob1_Param,
									u_ucode,
									IFTEST124),
						(   IFTEST124\==[]
						->  TrueResult75=Bindings_Get127
						;   get_var(Env,
							    u_else,
							    IFTEST128),
						    (   IFTEST128\==[]
						    ->  cl_error(
								 [ '$ARRAY'([*],
									    claz_base_character,
									    
									    [ #\(o),
									      #\(b),
									      #\($),
									      #\(u),
									      #\(n),
									      #\(i),
									      #\(f),
									      #\(y),
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
									      #\(s),
									      #\(p),
									      #\(e),
									      #\(c),
									      #\(i),
									      #\(a),
									      #\(l),
									      #\(!),
									      #\(!),
									      #\(' '),
									      #\(~),
									      #\('A')
									    ]),
								   Ob1_Param
								 ],
								 TrueResult132),
							TrueResult75=TrueResult132
						    ;   TrueResult75=[]
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
:- set_opv(f_u_ob_c36_unify_special, classof, claz_function),
   set_opv(u_ob_c36_unify_special, compile_as, kw_function),
   set_opv(u_ob_c36_unify_special, function, f_u_ob_c36_unify_special),
   DefunResult=u_ob_c36_unify_special.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" for now", 15, 10753)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The (else t) above basically ignores prioritization of:",
				     1,
				     10821)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ty$instance? ,ob1 'uempty-slots)",
				     1,
				     10879)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ty$instance? ,ob1 'uignore-slots)",
				     1,
				     10915)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ty$instance? ,ob1 'upath)", 1, 10952)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:8120 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ty$instance? ,ob1 'uolpath)", 1, 10981)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11012 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$unify-proc',
			    [ob2, proc, bd],
			    [setq, ob2, ['ob$concretize', ob2, bd]],
			    
			    [ if,
			      ['concretized?', ob2],
			      [if, [funcall, proc, ob2], bd, []],
			      bd
			    ]
			  ]).

% annotating U::OB$UNIFY-PROC 
wl: lambda_def(defun,
	      u_ob_c36_unify_proc,
	      f_u_ob_c36_unify_proc,
	      [u_ob2, u_proc, u_bd],
	      
	      [ [setq, u_ob2, [u_ob_c36_concretize, u_ob2, u_bd]],
		
		[ if,
		  [u_concretized_c63, u_ob2],
		  [if, [funcall, u_proc, u_ob2], u_bd, []],
		  u_bd
		]
	      ]).


% annotating U::OB$UNIFY-PROC 
wl: arglist_info(u_ob_c36_unify_proc,
		[u_ob2, u_proc, u_bd],
		[Ob2_Param, Proc_Param, Bd_Param],
		arginfo{ all:[u_ob2, u_proc, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob2, u_proc, u_bd],
			 opt:0,
			 req:[u_ob2, u_proc, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY-PROC 
wl: init_args(exact_only, u_ob_c36_unify_proc).


% annotating U::OB$UNIFY-PROC 
f_u_ob_c36_unify_proc(Ob2_Param, Proc_Param, Bd_Param, TrueResult27) :-
	Env=[bv(u_ob2, Ob2_Param), bv(u_proc, Proc_Param), bv(u_bd, Bd_Param)],
	get_var(Env, u_ob2, Ob2_Get),
	f_u_ob_c36_concretize(Ob2_Get, Bd_Param, Ob2),
	set_var(Env, u_ob2, Ob2),
	get_var(Env, u_ob2, Ob2_Get20),
	f_u_concretized_c63(Ob2_Get20, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env, u_ob2, Ob2_Get23),
	    f_u_proc(Ob2_Get23, IFTEST21),
	    (   IFTEST21\==[]
	    ->  TrueResult27=Bd_Param
	    ;   TrueResult27=[]
	    )
	;   TrueResult27=Bd_Param
	).
:- set_opv(f_u_ob_c36_unify_proc, classof, claz_function),
   set_opv(u_ob_c36_unify_proc, compile_as, kw_function),
   set_opv(u_ob_c36_unify_proc, function, f_u_ob_c36_unify_proc),
   DefunResult=u_ob_c36_unify_proc.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11171 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$concretize',
			    [ob, bd],
			    
			    [ cond,
			      [['var?', ob], ['ob$concretize-var', ob, bd]],
			      
			      [ 
				[ and,
				  ['ob?', ob],
				  ['ty$instance?', ob, [quote, uand]]
				],
				['ob$concretize-and', ob, bd]
			      ],
			      [else, ob]
			    ]
			  ]).

% annotating U::OB$CONCRETIZE 
wl: lambda_def(defun,
	      u_ob_c36_concretize,
	      f_u_ob_c36_concretize,
	      [u_ob, u_bd],
	      
	      [ 
		[ cond,
		  [[u_var_c63, u_ob], [u_ob_c36_concretize_var, u_ob, u_bd]],
		  
		  [ 
		    [ and,
		      [u_ob_c63, u_ob],
		      [u_ty_c36_instance_c63, u_ob, [quote, u_uand]]
		    ],
		    [u_ob_c36_concretize_and, u_ob, u_bd]
		  ],
		  [u_else, u_ob]
		]
	      ]).


% annotating U::OB$CONCRETIZE 
wl: arglist_info(u_ob_c36_concretize,
		[u_ob, u_bd],
		[Ob_Get29, Bd_Param],
		arginfo{ all:[u_ob, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_bd],
			 opt:0,
			 req:[u_ob, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$CONCRETIZE 
wl: init_args(exact_only, u_ob_c36_concretize).


% annotating U::OB$CONCRETIZE 
f_u_ob_c36_concretize(Ob_Get29, Bd_Param, ElseResult33) :-
	Env=[bv(u_ob, Ob_Get29), bv(u_bd, Bd_Param)],
	f_u_var_c63(u_ob, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_concretize_var(Ob_Get29, Bd_Param, TrueResult34),
	    ElseResult33=TrueResult34
	;   f_u_ob_c63(u_ob, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_ty_c36_instance_c63(Ob_Get29, u_uand, TrueResult),
		IFTEST18=TrueResult
	    ;   IFTEST18=[]
	    ),
	    (   IFTEST18\==[]
	    ->  f_u_ob_c36_concretize_and(Ob_Get29, Bd_Param, TrueResult32),
		ElseResult33=TrueResult32
	    ;   get_var(Env, u_else, IFTEST26),
		(   IFTEST26\==[]
		->  ElseResult33=Ob_Get29
		;   ElseResult33=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_concretize, classof, claz_function),
   set_opv(u_ob_c36_concretize, compile_as, kw_function),
   set_opv(u_ob_c36_concretize, function, f_u_ob_c36_concretize),
   DefunResult=u_ob_c36_concretize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11345 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$concretize-and',
			    ['and-ptn', bd],
			    
			    [ yloop,
			      [yfor, item, in, ['ob$gets', 'and-ptn', [quote, obj]]],
			      [initial, [result, []]],
			      [yuntil, result],
			      
			      [ ydo,
				
				[ if,
				  ['var?', item],
				  [setq, result, ['ob$concretize-var', item, bd]]
				]
			      ],
			      
			      [ yresult,
				
				[ if,
				  ['null?', result],
				  
				  [ progn,
				    
				    [ format,
				      '*gate-output*',
				      '$STRING'("Warning: ob$concretize-and unsuccessful.~%")
				    ],
				    'and-ptn'
				  ],
				  result
				]
			      ]
			    ]
			  ]).

% annotating U::OB$CONCRETIZE-AND 
wl: lambda_def(defun,
	      u_ob_c36_concretize_and,
	      f_u_ob_c36_concretize_and,
	      [u_and_ptn, u_bd],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, item, u_in, [u_ob_c36_gets, u_and_ptn, [quote, u_obj]]],
		  [u_initial, [u_result, []]],
		  [u_yuntil, u_result],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_var_c63, item],
		      [setq, u_result, [u_ob_c36_concretize_var, item, u_bd]]
		    ]
		  ],
		  
		  [ u_yresult,
		    
		    [ if,
		      [u_null_c63, u_result],
		      
		      [ progn,
			
			[ format,
			  u_xx_gate_output_xx,
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
				     #\(o),
				     #\(b),
				     #\($),
				     #\(c),
				     #\(o),
				     #\(n),
				     #\(c),
				     #\(r),
				     #\(e),
				     #\(t),
				     #\(i),
				     #\(z),
				     #\(e),
				     #\(-),
				     #\(a),
				     #\(n),
				     #\(d),
				     #\(' '),
				     #\(u),
				     #\(n),
				     #\(s),
				     #\(u),
				     #\(c),
				     #\(c),
				     #\(e),
				     #\(s),
				     #\(s),
				     #\(f),
				     #\(u),
				     #\(l),
				     #\('.'),
				     #\(~),
				     #\('%')
				   ])
			],
			u_and_ptn
		      ],
		      u_result
		    ]
		  ]
		]
	      ]).


% annotating U::OB$CONCRETIZE-AND 
wl: arglist_info(u_ob_c36_concretize_and,
		[u_and_ptn, u_bd],
		[And_ptn_Param, Bd_Param],
		arginfo{ all:[u_and_ptn, u_bd],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_and_ptn, u_bd],
			 opt:0,
			 req:[u_and_ptn, u_bd],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$CONCRETIZE-AND 
wl: init_args(exact_only, u_ob_c36_concretize_and).


% annotating U::OB$CONCRETIZE-AND 
f_u_ob_c36_concretize_and(And_ptn_Param, Bd_Param, FnResult) :-
	Env=[bv(u_and_ptn, And_ptn_Param), bv(u_bd, Bd_Param)],
	f_u_yloop(
		  [ [u_yfor, item, u_in, [u_ob_c36_gets, u_and_ptn, [quote, u_obj]]],
		    [u_initial, [u_result, []]],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_var_c63, item],
			[setq, u_result, [u_ob_c36_concretize_var, item, u_bd]]
		      ]
		    ],
		    
		    [ u_yresult,
		      
		      [ if,
			[u_null_c63, u_result],
			
			[ progn,
			  
			  [ format,
			    u_xx_gate_output_xx,
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
				       #\(o),
				       #\(b),
				       #\($),
				       #\(c),
				       #\(o),
				       #\(n),
				       #\(c),
				       #\(r),
				       #\(e),
				       #\(t),
				       #\(i),
				       #\(z),
				       #\(e),
				       #\(-),
				       #\(a),
				       #\(n),
				       #\(d),
				       #\(' '),
				       #\(u),
				       #\(n),
				       #\(s),
				       #\(u),
				       #\(c),
				       #\(c),
				       #\(e),
				       #\(s),
				       #\(s),
				       #\(f),
				       #\(u),
				       #\(l),
				       #\('.'),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  u_and_ptn
			],
			u_result
		      ]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_concretize_and, classof, claz_function),
   set_opv(u_ob_c36_concretize_and, compile_as, kw_function),
   set_opv(u_ob_c36_concretize_and, function, f_u_ob_c36_concretize_and),
   DefunResult=u_ob_c36_concretize_and.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11833 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$concretize-var',
			    [var, bd],
			    
			    [ let,
			      [[found, ['bd-lookup', ['variable-name', var], bd]]],
			      [if, found, found, var]
			    ]
			  ]).

% annotating U::OB$CONCRETIZE-VAR 
wl: lambda_def(defun,
	      u_ob_c36_concretize_var,
	      f_u_ob_c36_concretize_var,
	      [u_var, u_bd],
	      
	      [ 
		[ let,
		  [[u_found, [u_bd_lookup, [u_variable_name, u_var], u_bd]]],
		  [if, u_found, u_found, u_var]
		]
	      ]).


% annotating U::OB$CONCRETIZE-VAR 
wl: arglist_info(u_ob_c36_concretize_var,
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

% annotating U::OB$CONCRETIZE-VAR 
wl: init_args(exact_only, u_ob_c36_concretize_var).


% annotating U::OB$CONCRETIZE-VAR 
f_u_ob_c36_concretize_var(Var_Param, Bd_Param, FnResult) :-
	Env=[bv(u_var, Var_Param), bv(u_bd, Bd_Param)],
	f_u_bd_lookup([u_variable_name, u_var], u_bd, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get21),
	    FnResult=Found_Get21
	;   FnResult=Var_Param
	).
:- set_opv(f_u_ob_c36_concretize_var, classof, claz_function),
   set_opv(u_ob_c36_concretize_var, compile_as, kw_function),
   set_opv(u_ob_c36_concretize_var, function, f_u_ob_c36_concretize_var),
   DefunResult=u_ob_c36_concretize_var.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'concretized?', [var], [not, ['var?', var]]]).

% annotating U::CONCRETIZED? 
wl: lambda_def(defun,
	      u_concretized_c63,
	      f_u_concretized_c63,
	      [u_var],
	      [[not, [u_var_c63, u_var]]]).


% annotating U::CONCRETIZED? 
wl: arglist_info(u_concretized_c63,
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

% annotating U::CONCRETIZED? 
wl: init_args(exact_only, u_concretized_c63).


% annotating U::CONCRETIZED? 
f_u_concretized_c63(Var_Param, FnResult) :-
	Env=[bv(u_var, Var_Param)],
	f_u_var_c63(u_var, Not_Param),
	cl_not(Not_Param, Not_Ret),
	Not_Ret=FnResult.
:- set_opv(f_u_concretized_c63, classof, claz_function),
   set_opv(u_concretized_c63, compile_as, kw_function),
   set_opv(u_concretized_c63, function, f_u_concretized_c63),
   DefunResult=u_concretized_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 11995)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Question mark atom should never get to here.",
				     1,
				     11997)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 12044)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This routine no longer checks if the types match right even if the variable",
				     1,
				     12046)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" is already bound. This used to be used to handle prebound typed ?Self, but",
				     1,
				     12124)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" now the self-type slot of rules serves this function.",
				     1,
				     12201)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:11947 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 12257)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$unify-var',
			    [ob1, ob2, bindings, 'ignore-slots', 'reverse?'],
			    
			    [ let,
			      
			      [ 
				[ val1,
				  ['bd-lookup', ['variable-name', ob1], bindings]
				],
				[val2, []]
			      ],
			      
			      [ if,
				[and, val1, [not, ['var?', val1]]],
				[setq, ob1, val1]
			      ],
			      
			      [ if,
				['var?', ob2],
				
				[ progn,
				  
				  [ setq,
				    val2,
				    
				    [ 'bd-lookup',
				      ['variable-name', ob2],
				      bindings
				    ]
				  ],
				  
				  [ if,
				    [and, val2, [not, ['var?', val2]]],
				    [setq, ob2, val2]
				  ]
				]
			      ],
			      
			      [ cond,
				
				[ [and, ['var?', ob1], ['var?', ob2]],
				  
				  [ if,
				    ['type-compatible-vars?', ob1, ob2],
				    
				    [ if,
				      '*diff?*',
				      bindings,
				      
				      [ 'bd-bind',
					['variable-name', ob2],
					ob1,
					
					[ 'bd-bind',
					  ['variable-name', ob1],
					  ob2,
					  bindings
					]
				      ]
				    ],
				    []
				  ]
				],
				
				[ ['var?', ob1],
				  
				  [ if,
				    
				    [ 'var-ty$instance?',
				      ob2,
				      ['variable-type', ob1]
				    ],
				    
				    [ if,
				      '*diff?*',
				      bindings,
				      
				      [ 'bd-bind',
					['variable-name', ob1],
					ob2,
					bindings
				      ]
				    ],
				    []
				  ]
				],
				
				[ ['var?', ob2],
				  
				  [ if,
				    
				    [ 'var-ty$instance?',
				      ob1,
				      ['variable-type', ob2]
				    ],
				    
				    [ if,
				      '*diff?*',
				      bindings,
				      
				      [ 'bd-bind',
					['variable-name', ob2],
					ob1,
					bindings
				      ]
				    ],
				    []
				  ]
				],
				
				[ else,
				  
				  [ if,
				    'reverse?',
				    
				    [ 'ob$unify2',
				      ob2,
				      ob1,
				      bindings,
				      'ignore-slots'
				    ],
				    
				    [ 'ob$unify2',
				      ob1,
				      ob2,
				      bindings,
				      'ignore-slots'
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$UNIFY-VAR 
wl: lambda_def(defun,
	      u_ob_c36_unify_var,
	      f_u_ob_c36_unify_var,
	      [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63],
	      
	      [ 
		[ let,
		  
		  [ [u_val1, [u_bd_lookup, [u_variable_name, u_ob1], bindings]],
		    [u_val2, []]
		  ],
		  
		  [ if,
		    [and, u_val1, [not, [u_var_c63, u_val1]]],
		    [setq, u_ob1, u_val1]
		  ],
		  
		  [ if,
		    [u_var_c63, u_ob2],
		    
		    [ progn,
		      
		      [ setq,
			u_val2,
			[u_bd_lookup, [u_variable_name, u_ob2], bindings]
		      ],
		      
		      [ if,
			[and, u_val2, [not, [u_var_c63, u_val2]]],
			[setq, u_ob2, u_val2]
		      ]
		    ]
		  ],
		  
		  [ cond,
		    
		    [ [and, [u_var_c63, u_ob1], [u_var_c63, u_ob2]],
		      
		      [ if,
			[u_type_compatible_vars_c63, u_ob1, u_ob2],
			
			[ if,
			  u_xx_diff_c63_xx,
			  bindings,
			  
			  [ u_bd_bind,
			    [u_variable_name, u_ob2],
			    u_ob1,
			    [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]
			  ]
			],
			[]
		      ]
		    ],
		    
		    [ [u_var_c63, u_ob1],
		      
		      [ if,
			
			[ u_var_ty_c36_instance_c63,
			  u_ob2,
			  [u_variable_type, u_ob1]
			],
			
			[ if,
			  u_xx_diff_c63_xx,
			  bindings,
			  [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]
			],
			[]
		      ]
		    ],
		    
		    [ [u_var_c63, u_ob2],
		      
		      [ if,
			
			[ u_var_ty_c36_instance_c63,
			  u_ob1,
			  [u_variable_type, u_ob2]
			],
			
			[ if,
			  u_xx_diff_c63_xx,
			  bindings,
			  [u_bd_bind, [u_variable_name, u_ob2], u_ob1, bindings]
			],
			[]
		      ]
		    ],
		    
		    [ u_else,
		      
		      [ if,
			u_reverse_c63,
			[u_ob_c36_unify2, u_ob2, u_ob1, bindings, u_ignore_slots],
			[u_ob_c36_unify2, u_ob1, u_ob2, bindings, u_ignore_slots]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$UNIFY-VAR 
wl: arglist_info(u_ob_c36_unify_var,
		[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63],
		
		[ Ob1_Param,
		  Ob2_Param,
		  Bindings_Param,
		  Ignore_slots_Param,
		  Reverse_c63_Param
		],
		arginfo{ all:
			     [ u_ob1,
			       u_ob2,
			       bindings,
			       u_ignore_slots,
			       u_reverse_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob1,
				 u_ob2,
				 bindings,
				 u_ignore_slots,
				 u_reverse_c63
			       ],
			 opt:0,
			 req:
			     [ u_ob1,
			       u_ob2,
			       bindings,
			       u_ignore_slots,
			       u_reverse_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$UNIFY-VAR 
wl: init_args(exact_only, u_ob_c36_unify_var).


% annotating U::OB$UNIFY-VAR 
f_u_ob_c36_unify_var(Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param, Reverse_c63_Param, TrueResult56) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param), bv(bindings, Bindings_Param), bv(u_ignore_slots, Ignore_slots_Param), bv(u_reverse_c63, Reverse_c63_Param)],
	f_u_bd_lookup([u_variable_name, u_ob1], bindings, Val1_Init),
	LEnv=[[bv(u_val1, Val1_Init), bv(u_val2, [])]|Env],
	get_var(LEnv, u_val1, IFTEST25),
	(   IFTEST25\==[]
	->  f_u_var_c63(u_val1, Not_Param),
	    cl_not(Not_Param, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_val1, Val1_Get30),
	    set_var(LEnv, u_ob1, Val1_Get30),
	    _144118=Val1_Get30
	;   _144118=[]
	),
	f_u_var_c63(u_ob2, IFTEST32),
	(   IFTEST32\==[]
	->  f_u_bd_lookup([u_variable_name, u_ob2], bindings, Bindings),
	    set_var(LEnv, u_val2, Bindings),
	    get_var(LEnv, u_val2, IFTEST36),
	    (   IFTEST36\==[]
	    ->  f_u_var_c63(u_val2, Not_Param99),
		cl_not(Not_Param99, TrueResult39),
		IFTEST34=TrueResult39
	    ;   IFTEST34=[]
	    ),
	    (   IFTEST34\==[]
	    ->  set_var(LEnv, u_ob2, IFTEST36),
		TrueResult42=IFTEST36
	    ;   TrueResult42=[]
	    )
	;   TrueResult42=[]
	),
	f_u_var_c63(u_ob1, IFTEST45),
	(   IFTEST45\==[]
	->  f_u_var_c63(u_ob2, TrueResult47),
	    IFTEST43=TrueResult47
	;   IFTEST43=[]
	),
	(   IFTEST43\==[]
	->  f_u_type_compatible_vars_c63(u_ob1, u_ob2, IFTEST48),
	    (   IFTEST48\==[]
	    ->  get_var(LEnv, u_xx_diff_c63_xx, IFTEST50),
		(   IFTEST50\==[]
		->  TrueResult56=Bindings_Param
		;   f_u_bd_bind([u_variable_name, u_ob2],
				u_ob1,
				
				[ u_bd_bind,
				  [u_variable_name, u_ob1],
				  u_ob2,
				  bindings
				],
				ElseResult),
		    TrueResult56=ElseResult
		)
	    ;   TrueResult56=[]
	    )
	;   f_u_var_c63(u_ob1, IFTEST57),
	    (   IFTEST57\==[]
	    ->  f_u_var_ty_c36_instance_c63(u_ob2,
					    [u_variable_type, u_ob1],
					    IFTEST59),
		(   IFTEST59\==[]
		->  get_var(LEnv, u_xx_diff_c63_xx, IFTEST61),
		    (   IFTEST61\==[]
		    ->  TrueResult56=Bindings_Param
		    ;   f_u_bd_bind([u_variable_name, u_ob1],
				    u_ob2,
				    bindings,
				    ElseResult66),
			TrueResult56=ElseResult66
		    )
		;   TrueResult56=[]
		)
	    ;   f_u_var_c63(u_ob2, IFTEST68),
		(   IFTEST68\==[]
		->  f_u_var_ty_c36_instance_c63(u_ob1,
						[u_variable_type, u_ob2],
						IFTEST70),
		    (   IFTEST70\==[]
		    ->  get_var(LEnv, u_xx_diff_c63_xx, IFTEST72),
			(   IFTEST72\==[]
			->  TrueResult56=Bindings_Param
			;   f_u_bd_bind([u_variable_name, u_ob2],
					u_ob1,
					bindings,
					ElseResult77),
			    TrueResult56=ElseResult77
			)
		    ;   TrueResult56=[]
		    )
		;   get_var(LEnv, u_else, IFTEST79),
		    (   IFTEST79\==[]
		    ->  (   Reverse_c63_Param\==[]
			->  f_u_ob_c36_unify2(u_ob2,
					      u_ob1,
					      bindings,
					      u_ignore_slots,
					      TrueResult85),
			    TrueResult56=TrueResult85
			;   f_u_ob_c36_unify2(u_ob1,
					      u_ob2,
					      bindings,
					      u_ignore_slots,
					      ElseResult86),
			    TrueResult56=ElseResult86
			)
		    ;   TrueResult56=[]
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_unify_var, classof, claz_function),
   set_opv(u_ob_c36_unify_var, compile_as, kw_function),
   set_opv(u_ob_c36_unify_var, function, f_u_ob_c36_unify_var),
   DefunResult=u_ob_c36_unify_var.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was  (if val1 (setq ob1 val1))",
				     1,
				     12397)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was       (if val2 (setq ob2 val2))",
				     1,
				     12683)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(and (not (var? ob1)) (not (var? ob2)))",
				     15,
				     13435)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13614)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This is the old incomprehensible version.",
				     1,
				     13616)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:12259 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13660)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:13661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$old-unify-var',
			    [ob1, ob2, bindings, 'ignore-slots', 'reverse?'],
			    
			    [ if,
			      '*relax-unify-var*',
			      
			      [ if,
				['null?', ['variable-name', ob1]],
				bindings,
				
				[ let,
				  
				  [ 
				    [ found,
				      
				      [ 'bd-lookup',
					['variable-name', ob1],
					bindings
				      ]
				    ]
				  ],
				  
				  [ if,
				    found,
				    
				    [ 'ob$unify2',
				      found,
				      ob2,
				      bindings,
				      'ignore-slots'
				    ],
				    
				    [ if,
				      ['var?', ob2],
				      
				      [ progn,
					
					[ setq,
					  found,
					  
					  [ 'bd-lookup',
					    ['variable-name', ob2],
					    bindings
					  ]
					],
					
					[ if,
					  found,
					  
					  [ 'ob$unify2',
					    ob1,
					    found,
					    bindings,
					    'ignore-slots'
					  ],
					  
					  [ 'bd-bind',
					    ['variable-name', ob1],
					    ob2,
					    bindings
					  ]
					]
				      ],
				      
				      [ 'bd-bind',
					['variable-name', ob1],
					ob2,
					bindings
				      ]
				    ]
				  ]
				]
			      ],
			      
			      [ progn,
				
				[ if,
				  
				  [ and,
				    ['variable-type', ob1],
				    [not, ['var?', ob2]],
				    
				    [ not,
				      
				      [ 'ty$instance-of?',
					ob2,
					['variable-type', ob1]
				      ]
				    ]
				  ],
				  [],
				  
				  [ if,
				    ['null?', ['variable-name', ob1]],
				    bindings,
				    
				    [ let,
				      
				      [ 
					[ found,
					  
					  [ 'bd-lookup',
					    ['variable-name', ob1],
					    bindings
					  ]
					]
				      ],
				      
				      [ if,
					found,
					
					[ 'ob$unify2',
					  found,
					  ob2,
					  bindings,
					  'ignore-slots'
					],
					
					[ if,
					  ['var?', ob2],
					  
					  [ progn,
					    
					    [ setq,
					      found,
					      
					      [ 'bd-lookup',
						['variable-name', ob2],
						bindings
					      ]
					    ],
					    
					    [ if,
					      found,
					      
					      [ 'ob$unify2',
						ob1,
						found,
						bindings,
						'ignore-slots'
					      ],
					      
					      [ if,
						
						[ 'type-compatible-vars?',
						  ob1,
						  ob2
						],
						
						[ 'bd-bind',
						  ['variable-name', ob1],
						  ob2,
						  bindings
						],
						[]
					      ]
					    ]
					  ],
					  
					  [ if,
					    ['variable-type', ob1],
					    
					    [ if,
					      
					      [ 'ty$instance-of?',
						ob2,
						['variable-type', ob1]
					      ],
					      
					      [ 'bd-bind',
						['variable-name', ob1],
						ob2,
						bindings
					      ],
					      []
					    ],
					    
					    [ 'bd-bind',
					      ['variable-name', ob1],
					      ob2,
					      bindings
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

% annotating U::OB$OLD-UNIFY-VAR 
wl: lambda_def(defun,
	      u_ob_c36_old_unify_var,
	      f_u_ob_c36_old_unify_var,
	      [u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63],
	      
	      [ 
		[ if,
		  u_xx_relax_unify_var_xx,
		  
		  [ if,
		    [u_null_c63, [u_variable_name, u_ob1]],
		    bindings,
		    
		    [ let,
		      
		      [ 
			[ u_found,
			  [u_bd_lookup, [u_variable_name, u_ob1], bindings]
			]
		      ],
		      
		      [ if,
			u_found,
			
			[ u_ob_c36_unify2,
			  u_found,
			  u_ob2,
			  bindings,
			  u_ignore_slots
			],
			
			[ if,
			  [u_var_c63, u_ob2],
			  
			  [ progn,
			    
			    [ setq,
			      u_found,
			      [u_bd_lookup, [u_variable_name, u_ob2], bindings]
			    ],
			    
			    [ if,
			      u_found,
			      
			      [ u_ob_c36_unify2,
				u_ob1,
				u_found,
				bindings,
				u_ignore_slots
			      ],
			      
			      [ u_bd_bind,
				[u_variable_name, u_ob1],
				u_ob2,
				bindings
			      ]
			    ]
			  ],
			  [u_bd_bind, [u_variable_name, u_ob1], u_ob2, bindings]
			]
		      ]
		    ]
		  ],
		  
		  [ progn,
		    
		    [ if,
		      
		      [ and,
			[u_variable_type, u_ob1],
			[not, [u_var_c63, u_ob2]],
			
			[ not,
			  
			  [ u_ty_c36_instance_of_c63,
			    u_ob2,
			    [u_variable_type, u_ob1]
			  ]
			]
		      ],
		      [],
		      
		      [ if,
			[u_null_c63, [u_variable_name, u_ob1]],
			bindings,
			
			[ let,
			  
			  [ 
			    [ u_found,
			      [u_bd_lookup, [u_variable_name, u_ob1], bindings]
			    ]
			  ],
			  
			  [ if,
			    u_found,
			    
			    [ u_ob_c36_unify2,
			      u_found,
			      u_ob2,
			      bindings,
			      u_ignore_slots
			    ],
			    
			    [ if,
			      [u_var_c63, u_ob2],
			      
			      [ progn,
				
				[ setq,
				  u_found,
				  
				  [ u_bd_lookup,
				    [u_variable_name, u_ob2],
				    bindings
				  ]
				],
				
				[ if,
				  u_found,
				  
				  [ u_ob_c36_unify2,
				    u_ob1,
				    u_found,
				    bindings,
				    u_ignore_slots
				  ],
				  
				  [ if,
				    [u_type_compatible_vars_c63, u_ob1, u_ob2],
				    
				    [ u_bd_bind,
				      [u_variable_name, u_ob1],
				      u_ob2,
				      bindings
				    ],
				    []
				  ]
				]
			      ],
			      
			      [ if,
				[u_variable_type, u_ob1],
				
				[ if,
				  
				  [ u_ty_c36_instance_of_c63,
				    u_ob2,
				    [u_variable_type, u_ob1]
				  ],
				  
				  [ u_bd_bind,
				    [u_variable_name, u_ob1],
				    u_ob2,
				    bindings
				  ],
				  []
				],
				
				[ u_bd_bind,
				  [u_variable_name, u_ob1],
				  u_ob2,
				  bindings
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


% annotating U::OB$OLD-UNIFY-VAR 
wl: arglist_info(u_ob_c36_old_unify_var,
		[u_ob1, u_ob2, bindings, u_ignore_slots, u_reverse_c63],
		
		[ Ob1_Param,
		  Ob2_Param,
		  Bindings_Param,
		  Ignore_slots_Param,
		  Reverse_c63_Param
		],
		arginfo{ all:
			     [ u_ob1,
			       u_ob2,
			       bindings,
			       u_ignore_slots,
			       u_reverse_c63
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob1,
				 u_ob2,
				 bindings,
				 u_ignore_slots,
				 u_reverse_c63
			       ],
			 opt:0,
			 req:
			     [ u_ob1,
			       u_ob2,
			       bindings,
			       u_ignore_slots,
			       u_reverse_c63
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$OLD-UNIFY-VAR 
wl: init_args(exact_only, u_ob_c36_old_unify_var).


% annotating U::OB$OLD-UNIFY-VAR 
f_u_ob_c36_old_unify_var(Ob1_Param, Ob2_Param, Bindings_Param, Ignore_slots_Param, Reverse_c63_Param, TrueResult40) :-
	Env=[bv(u_ob1, Ob1_Param), bv(u_ob2, Ob2_Param), bv(bindings, Bindings_Param), bv(u_ignore_slots, Ignore_slots_Param), bv(u_reverse_c63, Reverse_c63_Param)],
	get_var(Env, u_xx_relax_unify_var_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_null_c63([u_variable_name, u_ob1], IFTEST23),
	    (   IFTEST23\==[]
	    ->  TrueResult40=Bindings_Param
	    ;   f_u_bd_lookup([u_variable_name, u_ob1], bindings, Found_Init),
		LEnv=[[bv(u_found, Found_Init)]|Env],
		get_var(LEnv, u_found, IFTEST29),
		(   IFTEST29\==[]
		->  f_u_ob_c36_unify2(u_found,
				      u_ob2,
				      bindings,
				      u_ignore_slots,
				      TrueResult42),
		    TrueResult40=TrueResult42
		;   f_u_var_c63(u_ob2, IFTEST33),
		    (   IFTEST33\==[]
		    ->  f_u_bd_lookup([u_variable_name, u_ob2],
				      bindings,
				      Bindings),
			set_var(LEnv, u_found, Bindings),
			(   IFTEST29\==[]
			->  f_u_ob_c36_unify2(u_ob1,
					      u_found,
					      bindings,
					      u_ignore_slots,
					      TrueResult),
			    TrueResult40=TrueResult
			;   f_u_bd_bind([u_variable_name, u_ob1],
					u_ob2,
					bindings,
					ElseResult),
			    TrueResult40=ElseResult
			)
		    ;   f_u_bd_bind([u_variable_name, u_ob1],
				    u_ob2,
				    bindings,
				    ElseResult41),
			TrueResult40=ElseResult41
		    )
		)
	    )
	;   f_u_variable_type(u_ob1, IFTEST48),
	    (   IFTEST48\==[]
	    ->  f_u_var_c63(u_ob2, PredArgResult),
		(   PredArgResult==[]
		->  f_u_variable_type(u_ob1, Variable_type_Ret),
		    f_u_ty_c36_instance_of_c63(Ob2_Param,
					       Variable_type_Ret,
					       Not_Param),
		    cl_not(Not_Param, TrueResult54),
		    IFTEST46=TrueResult54
		;   IFTEST46=[]
		)
	    ;   IFTEST46=[]
	    ),
	    (   IFTEST46\==[]
	    ->  TrueResult40=[]
	    ;   f_u_null_c63([u_variable_name, u_ob1], IFTEST56),
		(   IFTEST56\==[]
		->  TrueResult40=Bindings_Param
		;   f_u_bd_lookup([u_variable_name, u_ob1],
				  bindings,
				  Found_Init61),
		    Env=[[bv(u_found, Found_Init61)]|Env],
		    get_var(Env, u_found, IFTEST62),
		    (   IFTEST62\==[]
		    ->  f_u_ob_c36_unify2(u_found,
					  u_ob2,
					  bindings,
					  u_ignore_slots,
					  TrueResult86),
			TrueResult40=TrueResult86
		    ;   f_u_var_c63(u_ob2, IFTEST66),
			(   IFTEST66\==[]
			->  f_u_bd_lookup([u_variable_name, u_ob2],
					  bindings,
					  Bindings97),
			    set_var(Env, u_found, Bindings97),
			    get_var(Env, u_found, IFTEST68),
			    (   IFTEST68\==[]
			    ->  f_u_ob_c36_unify2(u_ob1,
						  u_found,
						  bindings,
						  u_ignore_slots,
						  TrueResult74),
				TrueResult40=TrueResult74
			    ;   f_u_type_compatible_vars_c63(u_ob1,
							     u_ob2,
							     IFTEST71),
				(   IFTEST71\==[]
				->  f_u_bd_bind([u_variable_name, u_ob1],
						u_ob2,
						bindings,
						TrueResult73),
				    TrueResult40=TrueResult73
				;   TrueResult40=[]
				)
			    )
			;   f_u_variable_type(u_ob1, IFTEST76),
			    (   IFTEST76\==[]
			    ->  f_u_variable_type(u_ob1, Variable_type_Ret100),
				f_u_ty_c36_instance_of_c63(Ob2_Param,
							   Variable_type_Ret100,
							   IFTEST78),
				(   IFTEST78\==[]
				->  f_u_bd_bind([u_variable_name, u_ob1],
						u_ob2,
						bindings,
						TrueResult81),
				    TrueResult40=TrueResult81
				;   TrueResult40=[]
				)
			    ;   f_u_bd_bind([u_variable_name, u_ob1],
					    u_ob2,
					    bindings,
					    ElseResult83),
				TrueResult40=ElseResult83
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_old_unify_var, classof, claz_function),
   set_opv(u_ob_c36_old_unify_var, compile_as, kw_function),
   set_opv(u_ob_c36_old_unify_var, function, f_u_ob_c36_old_unify_var),
   DefunResult=u_ob_c36_old_unify_var.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:13661 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" ?", 73, 14210)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:13661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" but should do type compatibility check",
				     16,
				     14548)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:13661 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" ?", 69, 14955)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:13661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" should check type compatibility in above line,",
				     20,
				     15100)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:13661 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" but only if both variables are typed.",
				     20,
				     15168)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:15473 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*max-breadth*', 10]).
:- set_var(TLEnv3, setq, u_xx_max_breadth_xx, 10).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:15498 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$path',
			    ['from-constant', 'to-ptn', links, bindings],
			    
			    [ yloop,
			      
			      [ initial,
				[result, []],
				[count, 0],
				
				[ 'next-obs',
				  ['ob$get-many', 'from-constant', links]
				]
			      ],
			      
			      [ yuntil,
				
				[ or,
				  result,
				  
				  [ if,
				    [>, count, '*max-breadth*'],
				    
				    [ progn,
				      
				      [ ndbg,
					'*gate-dbg*',
					'ob-warn',
					'$STRING'("Exceeded max breadth in ob$path.~%")
				      ],
				      t
				    ],
				    []
				  ]
				]
			      ],
			      [ywhile, 'next-obs'],
			      
			      [ ydo,
				
				[ yloop,
				  [yfor, 'next-ob', in, 'next-obs'],
				  [yuntil, result],
				  
				  [ ydo,
				    
				    [ setq,
				      result,
				      ['ob$unify', 'to-ptn', 'next-ob', bindings]
				    ]
				  ]
				],
				
				[ if,
				  ['null?', result],
				  
				  [ setq,
				    'next-obs',
				    
				    [ 'walk-append',
				      [lambda, [ob], ['ob$get-many', ob, links]],
				      'next-obs'
				    ]
				  ]
				],
				['increment-me', count]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::OB$PATH 
wl: lambda_def(defun,
	      u_ob_c36_path,
	      f_u_ob_c36_path,
	      [u_from_constant, u_to_ptn, u_links, bindings],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    [u_result, []],
		    [count, 0],
		    [u_next_obs, [u_ob_c36_get_many, u_from_constant, u_links]]
		  ],
		  
		  [ u_yuntil,
		    
		    [ or,
		      u_result,
		      
		      [ if,
			[>, count, u_xx_max_breadth_xx],
			
			[ progn,
			  
			  [ u_ndbg,
			    u_xx_gate_dbg_xx,
			    u_ob_warn,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('E'),
				       #\(x),
				       #\(c),
				       #\(e),
				       #\(e),
				       #\(d),
				       #\(e),
				       #\(d),
				       #\(' '),
				       #\(m),
				       #\(a),
				       #\(x),
				       #\(' '),
				       #\(b),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(d),
				       #\(t),
				       #\(h),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(' '),
				       #\(o),
				       #\(b),
				       #\($),
				       #\(p),
				       #\(a),
				       #\(t),
				       #\(h),
				       #\('.'),
				       #\(~),
				       #\('%')
				     ])
			  ],
			  t
			],
			[]
		      ]
		    ]
		  ],
		  [u_ywhile, u_next_obs],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      [u_yfor, u_next_ob, u_in, u_next_obs],
		      [u_yuntil, u_result],
		      
		      [ u_ydo,
			
			[ setq,
			  u_result,
			  [u_ob_c36_unify, u_to_ptn, u_next_ob, bindings]
			]
		      ]
		    ],
		    
		    [ if,
		      [u_null_c63, u_result],
		      
		      [ setq,
			u_next_obs,
			
			[ u_walk_append,
			  [lambda, [u_ob], [u_ob_c36_get_many, u_ob, u_links]],
			  u_next_obs
			]
		      ]
		    ],
		    [u_increment_me, count]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::OB$PATH 
wl: arglist_info(u_ob_c36_path,
		[u_from_constant, u_to_ptn, u_links, bindings],
		[From_constant_Param, To_ptn_Param, Links_Param, Bindings_Param],
		arginfo{ all:[u_from_constant, u_to_ptn, u_links, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_from_constant, u_to_ptn, u_links, bindings],
			 opt:0,
			 req:[u_from_constant, u_to_ptn, u_links, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PATH 
wl: init_args(exact_only, u_ob_c36_path).


% annotating U::OB$PATH 
f_u_ob_c36_path(From_constant_Param, To_ptn_Param, Links_Param, Bindings_Param, FnResult) :-
	Env=[bv(u_from_constant, From_constant_Param), bv(u_to_ptn, To_ptn_Param), bv(u_links, Links_Param), bv(bindings, Bindings_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      [u_result, []],
		      [count, 0],
		      [u_next_obs, [u_ob_c36_get_many, u_from_constant, u_links]]
		    ],
		    
		    [ u_yuntil,
		      
		      [ or,
			u_result,
			
			[ if,
			  [>, count, u_xx_max_breadth_xx],
			  
			  [ progn,
			    
			    [ u_ndbg,
			      u_xx_gate_dbg_xx,
			      u_ob_warn,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('E'),
					 #\(x),
					 #\(c),
					 #\(e),
					 #\(e),
					 #\(d),
					 #\(e),
					 #\(d),
					 #\(' '),
					 #\(m),
					 #\(a),
					 #\(x),
					 #\(' '),
					 #\(b),
					 #\(r),
					 #\(e),
					 #\(a),
					 #\(d),
					 #\(t),
					 #\(h),
					 #\(' '),
					 #\(i),
					 #\(n),
					 #\(' '),
					 #\(o),
					 #\(b),
					 #\($),
					 #\(p),
					 #\(a),
					 #\(t),
					 #\(h),
					 #\('.'),
					 #\(~),
					 #\('%')
				       ])
			    ],
			    t
			  ],
			  []
			]
		      ]
		    ],
		    [u_ywhile, u_next_obs],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_yfor, u_next_ob, u_in, u_next_obs],
			[u_yuntil, u_result],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    [u_ob_c36_unify, u_to_ptn, u_next_ob, bindings]
			  ]
			]
		      ],
		      
		      [ if,
			[u_null_c63, u_result],
			
			[ setq,
			  u_next_obs,
			  
			  [ u_walk_append,
			    [lambda, [u_ob], [u_ob_c36_get_many, u_ob, u_links]],
			    u_next_obs
			  ]
			]
		      ],
		      [u_increment_me, count]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_path, classof, claz_function),
   set_opv(u_ob_c36_path, compile_as, kw_function),
   set_opv(u_ob_c36_path, function, f_u_ob_c36_path),
   DefunResult=u_ob_c36_path.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16333 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*uniquified-obs*', []]).
:- set_var(TLEnv3, setq, u_xx_uniquified_obs_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16333 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 16363)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The following function seems to be ineffectual. Maybe all references",
				     1,
				     16365)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16333 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to these obs are not being deleted?",
				     1,
				     16436)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16333 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 16474)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16475 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'gc-uniquified-obs',
			    [],
			    
			    [ yloop,
			      [yfor, ob, in, '*uniquified-obs*'],
			      [ydo, ['ob$destroy', ob]]
			    ],
			    [setq, '*uniquified-obs*', []]
			  ]).

% annotating U::GC-UNIQUIFIED-OBS 
wl: lambda_def(defun,
	      u_gc_uniquified_obs,
	      f_u_gc_uniquified_obs,
	      [],
	      
	      [ 
		[ u_yloop,
		  [u_yfor, u_ob, u_in, u_xx_uniquified_obs_xx],
		  [u_ydo, [u_ob_c36_destroy, u_ob]]
		],
		[setq, u_xx_uniquified_obs_xx, []]
	      ]).


% annotating U::GC-UNIQUIFIED-OBS 
wl: arglist_info(u_gc_uniquified_obs,
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

% annotating U::GC-UNIQUIFIED-OBS 
wl: init_args(exact_only, u_gc_uniquified_obs).


% annotating U::GC-UNIQUIFIED-OBS 
f_u_gc_uniquified_obs(FnResult) :-
	Env=[],
	f_u_yloop(
		  [ [u_yfor, u_ob, u_in, u_xx_uniquified_obs_xx],
		    [u_ydo, [u_ob_c36_destroy, u_ob]]
		  ],
		  Yloop_Ret),
	set_var(Env, setq, u_xx_uniquified_obs_xx, []),
	[]=FnResult.
:- set_opv(f_u_gc_uniquified_obs, classof, claz_function),
   set_opv(u_gc_uniquified_obs, compile_as, kw_function),
   set_opv(u_gc_uniquified_obs, function, f_u_gc_uniquified_obs),
   DefunResult=u_gc_uniquified_obs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16605 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$compare1',
			    [source, target, substit, 'ignore-slots', proc],
			    
			    [ if,
			      
			      [ 'memq?',
				target,
				['bd-lookup', source, '*already-matched*']
			      ],
			      substit,
			      
			      [ progn,
				
				[ 'bd-bind!',
				  source,
				  
				  [ cons,
				    target,
				    ['bd-lookup', source, '*already-matched*']
				  ],
				  '*already-matched*'
				],
				
				[ 'bd-bind!',
				  target,
				  
				  [ cons,
				    source,
				    ['bd-lookup', target, '*already-matched*']
				  ],
				  '*already-matched*'
				],
				
				[ let,
				  
				  [ 
				    [ result,
				      
				      [ cond,
					[['eq?', source, target], substit],
					
					[ 
					  [ and,
					    ['ob?', source],
					    
					    [ and,
					      ['ob$literal?', source],
					      [not, ['ty?', source]]
					    ]
					  ],
					  []
					],
					
					[ 
					  [ and,
					    ['ob?', target],
					    
					    [ and,
					      ['ob$literal?', target],
					      [not, ['ty?', target]]
					    ]
					  ],
					  []
					],
					
					[ 
					  [ 'eq?',
					    ['bd-lookup', source, substit],
					    target
					  ],
					  substit
					],
					
					[ 
					  [ and,
					    ['ob?', source],
					    [not, ['ty?', source]],
					    ['ob?', target],
					    [not, ['ty?', target]]
					  ],
					  
					  [ yloop,
					    
					    [ initial,
					      ['compared-slot-indices', []],
					      
					      [ 'target-slots',
						['ob$pairs', target]
					      ],
					      ['target-slot-index', []],
					      ['new-substit', []],
					      ['save-substit', substit],
					      ['found?', []],
					      ['proc-result', []]
					    ],
					    [yfor, cur, in, ['ob$pairs', source]],
					    [ywhile, substit],
					    
					    [ ydo,
					      
					      [ if,
						
						[ and,
						  
						  [ not,
						    
						    [ 'memq?',
						      [car, cur],
						      'ignore-slots'
						    ]
						  ],
						  
						  [ not,
						    
						    [ 'memq?',
						      [car, cur],
						      '*permanent-ignore-slots*'
						    ]
						  ]
						],
						
						[ progn,
						  [setq, 'target-slot-index', 0],
						  [setq, 'new-substit', []],
						  [setq, 'found?', []],
						  
						  [ yloop,
						    
						    [ yfor,
						      'target-slot-value',
						      in,
						      'target-slots'
						    ],
						    [yuntil, 'found?'],
						    
						    [ ydo,
						      
						      [ if,
							
							[ and,
							  
							  [ 'eq?',
							    [car, cur],
							    
							    [ 'slots-name',
							      'target-slot-value'
							    ]
							  ],
							  
							  [ not,
							    
							    [ 'memq?',
							      'target-slot-index',
							      'compared-slot-indices'
							    ]
							  ],
							  
							  [ setq,
							    'new-substit',
							    
							    [ if,
							      
							      [ 'eq?',
								[cadr, cur],
								
								[ 'slots-value',
								  'target-slot-value'
								]
							      ],
							      substit,
							      
							      [ 'ob$compare1',
								[cadr, cur],
								
								[ 'slots-value',
								  'target-slot-value'
								],
								substit,
								'ignore-slots',
								proc
							      ]
							    ]
							  ]
							],
							
							[ progn,
							  [setq, 'found?', t],
							  
							  [ setq,
							    'compared-slot-indices',
							    
							    [ cons,
							      'target-slot-index',
							      'compared-slot-indices'
							    ]
							  ]
							]
						      ],
						      
						      [ 'increment-me',
							'target-slot-index'
						      ]
						    ]
						  ],
						  
						  [ if,
						    'found?',
						    
						    [ setq,
						      substit,
						      'new-substit'
						    ],
						    [setq, substit, []]
						  ]
						]
					      ]
					    ],
					    
					    [ yresult,
					      
					      [ if,
						['null?', substit],
						
						[ if,
						  
						  [ setq,
						    'proc-result',
						    
						    [ funcall,
						      proc,
						      source,
						      target
						    ]
						  ],
						  
						  [ cons,
						    [quote, t],
						    
						    [ cons,
						      
						      [ list,
							source,
							target,
							'proc-result'
						      ],
						      [cdr, 'save-substit']
						    ]
						  ],
						  []
						],
						substit
					      ]
					    ]
					  ]
					],
					
					[ [and, ['ty?', source], ['ty?', target]],
					  
					  [ let,
					    
					    [ 
					      [ 'proc-result',
						[funcall, proc, source, target]
					      ]
					    ],
					    
					    [ if,
					      'proc-result',
					      
					      [ cons,
						[quote, t],
						
						[ cons,
						  
						  [ list,
						    source,
						    target,
						    'proc-result'
						  ],
						  [cdr, substit]
						]
					      ],
					      []
					    ]
					  ]
					],
					[else, []]
				      ]
				    ]
				  ],
				  
				  [ if,
				    result,
				    result,
				    
				    [ progn,
				      
				      [ 'bd-bind!',
					source,
					
					[ 'delq!',
					  target,
					  
					  [ 'bd-lookup',
					    source,
					    '*already-matched*'
					  ]
					],
					'*already-matched*'
				      ],
				      
				      [ 'bd-bind!',
					target,
					
					[ 'delq!',
					  source,
					  
					  [ 'bd-lookup',
					    target,
					    '*already-matched*'
					  ]
					],
					'*already-matched*'
				      ],
				      []
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$COMPARE1 
wl: lambda_def(defun,
	      u_ob_c36_compare1,
	      f_u_ob_c36_compare1,
	      [ext_source, u_target, u_substit, u_ignore_slots, u_proc],
	      
	      [ 
		[ if,
		  
		  [ u_memq_c63,
		    u_target,
		    [u_bd_lookup, ext_source, u_xx_already_matched_xx]
		  ],
		  u_substit,
		  
		  [ progn,
		    
		    [ u_bd_bind_c33,
		      ext_source,
		      
		      [ cons,
			u_target,
			[u_bd_lookup, ext_source, u_xx_already_matched_xx]
		      ],
		      u_xx_already_matched_xx
		    ],
		    
		    [ u_bd_bind_c33,
		      u_target,
		      
		      [ cons,
			ext_source,
			[u_bd_lookup, u_target, u_xx_already_matched_xx]
		      ],
		      u_xx_already_matched_xx
		    ],
		    
		    [ let,
		      
		      [ 
			[ u_result,
			  
			  [ cond,
			    [[u_eq_c63, ext_source, u_target], u_substit],
			    
			    [ 
			      [ and,
				[u_ob_c63, ext_source],
				
				[ and,
				  [u_ob_c36_literal_c63, ext_source],
				  [not, [u_ty_c63, ext_source]]
				]
			      ],
			      []
			    ],
			    
			    [ 
			      [ and,
				[u_ob_c63, u_target],
				
				[ and,
				  [u_ob_c36_literal_c63, u_target],
				  [not, [u_ty_c63, u_target]]
				]
			      ],
			      []
			    ],
			    
			    [ 
			      [ u_eq_c63,
				[u_bd_lookup, ext_source, u_substit],
				u_target
			      ],
			      u_substit
			    ],
			    
			    [ 
			      [ and,
				[u_ob_c63, ext_source],
				[not, [u_ty_c63, ext_source]],
				[u_ob_c63, u_target],
				[not, [u_ty_c63, u_target]]
			      ],
			      
			      [ u_yloop,
				
				[ u_initial,
				  [u_compared_slot_indices, []],
				  [u_target_slots, [u_ob_c36_pairs, u_target]],
				  [u_target_slot_index, []],
				  [u_new_substit, []],
				  [u_save_substit, u_substit],
				  [u_found_c63, []],
				  [u_proc_result, []]
				],
				
				[ u_yfor,
				  u_cur,
				  u_in,
				  [u_ob_c36_pairs, ext_source]
				],
				[u_ywhile, u_substit],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ not,
					
					[ u_memq_c63,
					  [car, u_cur],
					  u_ignore_slots
					]
				      ],
				      
				      [ not,
					
					[ u_memq_c63,
					  [car, u_cur],
					  u_xx_permanent_ignore_slots_xx
					]
				      ]
				    ],
				    
				    [ progn,
				      [setq, u_target_slot_index, 0],
				      [setq, u_new_substit, []],
				      [setq, u_found_c63, []],
				      
				      [ u_yloop,
					
					[ u_yfor,
					  u_target_slot_value,
					  u_in,
					  u_target_slots
					],
					[u_yuntil, u_found_c63],
					
					[ u_ydo,
					  
					  [ if,
					    
					    [ and,
					      
					      [ u_eq_c63,
						[car, u_cur],
						
						[ u_slots_name,
						  u_target_slot_value
						]
					      ],
					      
					      [ not,
						
						[ u_memq_c63,
						  u_target_slot_index,
						  u_compared_slot_indices
						]
					      ],
					      
					      [ setq,
						u_new_substit,
						
						[ if,
						  
						  [ u_eq_c63,
						    [cadr, u_cur],
						    
						    [ u_slots_value,
						      u_target_slot_value
						    ]
						  ],
						  u_substit,
						  
						  [ u_ob_c36_compare1,
						    [cadr, u_cur],
						    
						    [ u_slots_value,
						      u_target_slot_value
						    ],
						    u_substit,
						    u_ignore_slots,
						    u_proc
						  ]
						]
					      ]
					    ],
					    
					    [ progn,
					      [setq, u_found_c63, t],
					      
					      [ setq,
						u_compared_slot_indices,
						
						[ cons,
						  u_target_slot_index,
						  u_compared_slot_indices
						]
					      ]
					    ]
					  ],
					  [u_increment_me, u_target_slot_index]
					]
				      ],
				      
				      [ if,
					u_found_c63,
					[setq, u_substit, u_new_substit],
					[setq, u_substit, []]
				      ]
				    ]
				  ]
				],
				
				[ u_yresult,
				  
				  [ if,
				    [u_null_c63, u_substit],
				    
				    [ if,
				      
				      [ setq,
					u_proc_result,
					[funcall, u_proc, ext_source, u_target]
				      ],
				      
				      [ cons,
					[quote, t],
					
					[ cons,
					  
					  [ list,
					    ext_source,
					    u_target,
					    u_proc_result
					  ],
					  [cdr, u_save_substit]
					]
				      ],
				      []
				    ],
				    u_substit
				  ]
				]
			      ]
			    ],
			    
			    [ [and, [u_ty_c63, ext_source], [u_ty_c63, u_target]],
			      
			      [ let,
				
				[ 
				  [ u_proc_result,
				    [funcall, u_proc, ext_source, u_target]
				  ]
				],
				
				[ if,
				  u_proc_result,
				  
				  [ cons,
				    [quote, t],
				    
				    [ cons,
				      [list, ext_source, u_target, u_proc_result],
				      [cdr, u_substit]
				    ]
				  ],
				  []
				]
			      ]
			    ],
			    [u_else, []]
			  ]
			]
		      ],
		      
		      [ if,
			u_result,
			u_result,
			
			[ progn,
			  
			  [ u_bd_bind_c33,
			    ext_source,
			    
			    [ u_delq_c33,
			      u_target,
			      [u_bd_lookup, ext_source, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx
			  ],
			  
			  [ u_bd_bind_c33,
			    u_target,
			    
			    [ u_delq_c33,
			      ext_source,
			      [u_bd_lookup, u_target, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx
			  ],
			  []
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$COMPARE1 
wl: arglist_info(u_ob_c36_compare1,
		[ext_source, u_target, u_substit, u_ignore_slots, u_proc],
		
		[ Ext_source_Param,
		  Target_Param,
		  Substit_Get78,
		  Ignore_slots_Param,
		  Proc_Param
		],
		arginfo{ all:
			     [ ext_source,
			       u_target,
			       u_substit,
			       u_ignore_slots,
			       u_proc
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ ext_source,
				 u_target,
				 u_substit,
				 u_ignore_slots,
				 u_proc
			       ],
			 opt:0,
			 req:
			     [ ext_source,
			       u_target,
			       u_substit,
			       u_ignore_slots,
			       u_proc
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$COMPARE1 
wl: init_args(exact_only, u_ob_c36_compare1).


% annotating U::OB$COMPARE1 
f_u_ob_c36_compare1(Ext_source_Param, Target_Param, Substit_Get78, Ignore_slots_Param, Proc_Param, FnResult) :-
	Env=[bv(ext_source, Ext_source_Param), bv(u_target, Target_Param), bv(u_substit, Substit_Get78), bv(u_ignore_slots, Ignore_slots_Param), bv(u_proc, Proc_Param)],
	f_u_memq_c63(u_target,
		     [u_bd_lookup, ext_source, u_xx_already_matched_xx],
		     IFTEST),
	(   IFTEST\==[]
	->  FnResult=Substit_Get78
	;   f_u_bd_bind_c33(ext_source,
			    
			    [ cons,
			      u_target,
			      [u_bd_lookup, ext_source, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx,
			    Xx_already_matched_xx),
	    f_u_bd_bind_c33(u_target,
			    
			    [ cons,
			      ext_source,
			      [u_bd_lookup, u_target, u_xx_already_matched_xx]
			    ],
			    u_xx_already_matched_xx,
			    Xx_already_matched_xx106),
	    f_u_eq_c63(ext_source, u_target, IFTEST25),
	    (   IFTEST25\==[]
	    ->  ElseResult86=Substit_Get78
	    ;   f_u_ob_c63(ext_source, IFTEST30),
		(   IFTEST30\==[]
		->  f_u_ob_c36_literal_c63(Ext_source_Param, IFTEST32),
		    (   IFTEST32\==[]
		    ->  f_u_ty_c63(ext_source, Not_Param),
			cl_not(Not_Param, TrueResult),
			IFTEST28=TrueResult
		    ;   IFTEST28=[]
		    )
		;   IFTEST28=[]
		),
		(   IFTEST28\==[]
		->  ElseResult86=[]
		;   f_u_ob_c63(u_target, IFTEST39),
		    (   IFTEST39\==[]
		    ->  f_u_ob_c36_literal_c63(Target_Param, IFTEST41),
			(   IFTEST41\==[]
			->  f_u_ty_c63(u_target, Not_Param110),
			    cl_not(Not_Param110, TrueResult44),
			    IFTEST37=TrueResult44
			;   IFTEST37=[]
			)
		    ;   IFTEST37=[]
		    ),
		    (   IFTEST37\==[]
		    ->  ElseResult86=[]
		    ;   f_u_eq_c63([u_bd_lookup, ext_source, u_substit],
				   u_target,
				   IFTEST46),
			(   IFTEST46\==[]
			->  ElseResult86=Substit_Get78
			;   f_u_ob_c63(ext_source, IFTEST51),
			    (   IFTEST51\==[]
			    ->  f_u_ty_c63(ext_source, PredArgResult),
				(   PredArgResult==[]
				->  f_u_ob_c63(u_target, IFTEST56),
				    (   IFTEST56\==[]
				    ->  f_u_ty_c63(u_target, Not_Param111),
					cl_not(Not_Param111, TrueResult58),
					IFTEST49=TrueResult58
				    ;   IFTEST49=[]
				    )
				;   IFTEST49=[]
				)
			    ;   IFTEST49=[]
			    ),
			    (   IFTEST49\==[]
			    ->  f_u_yloop(
					  [ 
					    [ u_initial,
					      [u_compared_slot_indices, []],
					      
					      [ u_target_slots,
						[u_ob_c36_pairs, u_target]
					      ],
					      [u_target_slot_index, []],
					      [u_new_substit, []],
					      [u_save_substit, u_substit],
					      [u_found_c63, []],
					      [u_proc_result, []]
					    ],
					    
					    [ u_yfor,
					      u_cur,
					      u_in,
					      [u_ob_c36_pairs, ext_source]
					    ],
					    [u_ywhile, u_substit],
					    
					    [ u_ydo,
					      
					      [ if,
						
						[ and,
						  
						  [ not,
						    
						    [ u_memq_c63,
						      [car, u_cur],
						      u_ignore_slots
						    ]
						  ],
						  
						  [ not,
						    
						    [ u_memq_c63,
						      [car, u_cur],
						      u_xx_permanent_ignore_slots_xx
						    ]
						  ]
						],
						
						[ progn,
						  [setq, u_target_slot_index, 0],
						  [setq, u_new_substit, []],
						  [setq, u_found_c63, []],
						  
						  [ u_yloop,
						    
						    [ u_yfor,
						      u_target_slot_value,
						      u_in,
						      u_target_slots
						    ],
						    [u_yuntil, u_found_c63],
						    
						    [ u_ydo,
						      
						      [ if,
							
							[ and,
							  
							  [ u_eq_c63,
							    [car, u_cur],
							    
							    [ u_slots_name,
							      u_target_slot_value
							    ]
							  ],
							  
							  [ not,
							    
							    [ u_memq_c63,
							      u_target_slot_index,
							      u_compared_slot_indices
							    ]
							  ],
							  
							  [ setq,
							    u_new_substit,
							    
							    [ if,
							      
							      [ u_eq_c63,
								[cadr, u_cur],
								
								[ u_slots_value,
								  u_target_slot_value
								]
							      ],
							      u_substit,
							      
							      [ u_ob_c36_compare1,
								[cadr, u_cur],
								
								[ u_slots_value,
								  u_target_slot_value
								],
								u_substit,
								u_ignore_slots,
								u_proc
							      ]
							    ]
							  ]
							],
							
							[ progn,
							  [setq, u_found_c63, t],
							  
							  [ setq,
							    u_compared_slot_indices,
							    
							    [ cons,
							      u_target_slot_index,
							      u_compared_slot_indices
							    ]
							  ]
							]
						      ],
						      
						      [ u_increment_me,
							u_target_slot_index
						      ]
						    ]
						  ],
						  
						  [ if,
						    u_found_c63,
						    
						    [ setq,
						      u_substit,
						      u_new_substit
						    ],
						    [setq, u_substit, []]
						  ]
						]
					      ]
					    ],
					    
					    [ u_yresult,
					      
					      [ if,
						[u_null_c63, u_substit],
						
						[ if,
						  
						  [ setq,
						    u_proc_result,
						    
						    [ funcall,
						      u_proc,
						      ext_source,
						      u_target
						    ]
						  ],
						  
						  [ cons,
						    [quote, t],
						    
						    [ cons,
						      
						      [ list,
							ext_source,
							u_target,
							u_proc_result
						      ],
						      [cdr, u_save_substit]
						    ]
						  ],
						  []
						],
						u_substit
					      ]
					    ]
					  ],
					  TrueResult87),
				ElseResult86=TrueResult87
			    ;   f_u_ty_c63(ext_source, IFTEST63),
				(   IFTEST63\==[]
				->  f_u_ty_c63(u_target, TrueResult65),
				    IFTEST61=TrueResult65
				;   IFTEST61=[]
				),
				(   IFTEST61\==[]
				->  f_u_proc(Ext_source_Param,
					     Target_Param,
					     Proc_result_Init),
				    Env=[[bv(u_proc_result, Proc_result_Init)]|Env],
				    get_var(Env, u_proc_result, IFTEST71),
				    (   IFTEST71\==[]
				    ->  get_var(Env,
						u_proc_result,
						Proc_result_Get77),
					CAR=[Ext_source_Param, Target_Param, Proc_result_Get77],
					cl_cdr(Substit_Get78, Cdr_Ret),
					CDR=[CAR|Cdr_Ret],
					ElseResult86=[t|CDR]
				    ;   ElseResult86=[]
				    )
				;   get_var(Env, u_else, IFTEST81),
				    (   IFTEST81\==[]
				    ->  ElseResult86=[]
				    ;   ElseResult86=[]
				    )
				)
			    )
			)
		    )
		)
	    ),
	    LEnv=[[bv(u_result, ElseResult86)]|Env],
	    get_var(LEnv, u_result, IFTEST95),
	    (   IFTEST95\==[]
	    ->  get_var(LEnv, u_result, Result_Get99),
		FnResult=Result_Get99
	    ;   f_u_bd_bind_c33(ext_source,
				
				[ u_delq_c33,
				  u_target,
				  
				  [ u_bd_lookup,
				    ext_source,
				    u_xx_already_matched_xx
				  ]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx107),
		f_u_bd_bind_c33(u_target,
				
				[ u_delq_c33,
				  ext_source,
				  
				  [ u_bd_lookup,
				    u_target,
				    u_xx_already_matched_xx
				  ]
				],
				u_xx_already_matched_xx,
				Xx_already_matched_xx108),
		FnResult=[]
	    )
	).
:- set_opv(f_u_ob_c36_compare1, classof, claz_function),
   set_opv(u_ob_c36_compare1, compile_as, kw_function),
   set_opv(u_ob_c36_compare1, function, f_u_ob_c36_compare1),
   DefunResult=u_ob_c36_compare1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16605 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" was reverse", 49, 17838)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_unify.cl:16605 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 20502)).
:- true.


% Total time: 7.165 seconds


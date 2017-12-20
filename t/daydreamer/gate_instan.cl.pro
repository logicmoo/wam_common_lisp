
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_instan" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:05 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This file contains the instantiator for obs",
				     1,
				     202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:247 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 248)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:249 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 10/13/84: Original version written",
				     1,
				     250)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:286 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  6/30/85: Added *modify*, *expand*",
				     1,
				     287)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:323 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   1/6/86: Changed specials to obs",
				     1,
				     324)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:359 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/29/86: Added omit-proc", 1, 360)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:387 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/30/86: Added variables-in", 1, 388)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/24/86: Got rid of flavors", 1, 419)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:449 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/29/86: Updated to new instantiation algorithm with cycle preservation",
				     1,
				     450)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:524 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 525)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:526 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     527)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:607 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*found-obs*', []]).
:- set_var(TLEnv3, setq, u_xx_found_obs_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:631 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*instan-obs*', []]).
:- set_var(TLEnv3, setq, u_xx_instan_obs_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:656 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*any-unbound?*', []]).
:- set_var(TLEnv3, setq, u_xx_any_unbound_c63_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:683 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate',
			    [template, bindings],
			    ['ob$instantiate1', template, bindings, 100, [], []]
			  ]).

% annotating U::OB$INSTANTIATE 
wl: lambda_def(defun,
	      u_ob_c36_instantiate,
	      f_u_ob_c36_instantiate,
	      [u_template, bindings],
	      [[u_ob_c36_instantiate1, u_template, bindings, 100, [], []]]).


% annotating U::OB$INSTANTIATE 
wl: arglist_info(u_ob_c36_instantiate,
		[u_template, bindings],
		[Template_Param, Bindings_Param],
		arginfo{ all:[u_template, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_template, bindings],
			 opt:0,
			 req:[u_template, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTANTIATE 
wl: init_args(exact_only, u_ob_c36_instantiate).


% annotating U::OB$INSTANTIATE 
f_u_ob_c36_instantiate(Template_Param, Bindings_Param, FnResult) :-
	f_u_ob_c36_instantiate1(Template_Param,
				Bindings_Param,
				100,
				[],
				[],
				C36_instantiate1_Ret),
	C36_instantiate1_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate, classof, claz_function),
   set_opv(u_ob_c36_instantiate, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate, function, f_u_ob_c36_instantiate),
   DefunResult=u_ob_c36_instantiate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:777 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate1',
			    
			    [ template,
			      bindings,
			      depth,
			      'omit-slots',
			      'include-slots'
			    ],
			    [setq, '*instan-obs*', []],
			    [setq, '*any-unbound?*', []],
			    
			    [ 'ob$instantiate2',
			      template,
			      bindings,
			      depth,
			      'omit-slots',
			      'include-slots',
			      [],
			      [],
			      []
			    ]
			  ]).

% annotating U::OB$INSTANTIATE1 
wl: lambda_def(defun,
	      u_ob_c36_instantiate1,
	      f_u_ob_c36_instantiate1,
	      [u_template, bindings, u_depth, u_omit_slots, u_include_slots],
	      
	      [ [setq, u_xx_instan_obs_xx, []],
		[setq, u_xx_any_unbound_c63_xx, []],
		
		[ u_ob_c36_instantiate2,
		  u_template,
		  bindings,
		  u_depth,
		  u_omit_slots,
		  u_include_slots,
		  [],
		  [],
		  []
		]
	      ]).


% annotating U::OB$INSTANTIATE1 
wl: arglist_info(u_ob_c36_instantiate1,
		[u_template, bindings, u_depth, u_omit_slots, u_include_slots],
		
		[ Template_Param,
		  Bindings_Param,
		  Depth_Param,
		  Omit_slots_Param,
		  Include_slots_Param
		],
		arginfo{ all:
			     [ u_template,
			       bindings,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
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
				 u_include_slots
			       ],
			 opt:0,
			 req:
			     [ u_template,
			       bindings,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTANTIATE1 
wl: init_args(exact_only, u_ob_c36_instantiate1).


% annotating U::OB$INSTANTIATE1 
f_u_ob_c36_instantiate1(Template_Param, Bindings_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, FnResult) :-
	Env=[bv(u_template, Template_Param), bv(bindings, Bindings_Param), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param), bv(u_include_slots, Include_slots_Param)],
	set_var(Env, setq, u_xx_instan_obs_xx, []),
	set_var(Env, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_template,
				bindings,
				u_depth,
				u_omit_slots,
				u_include_slots,
				[],
				[],
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate1, classof, claz_function),
   set_opv(u_ob_c36_instantiate1, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate1, function, f_u_ob_c36_instantiate1),
   DefunResult=u_ob_c36_instantiate1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:777 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1009)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:777 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" substit: a binding list of pairs. Each pair has the thing (ob or otherwise)",
				     1,
				     1011)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:777 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to substitute and the thing to substitute it with.",
				     1,
				     1089)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:777 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1142)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1143 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$subst',
			    [ob, substit, depth, 'omit-slots', 'include-slots'],
			    [setq, '*instan-obs*', []],
			    [setq, '*any-unbound?*', []],
			    
			    [ 'ob$instantiate2',
			      ob,
			      '*empty-bd*',
			      depth,
			      'omit-slots',
			      'include-slots',
			      substit,
			      [],
			      []
			    ]
			  ]).

% annotating U::OB$SUBST 
wl: lambda_def(defun,
	      u_ob_c36_subst,
	      f_u_ob_c36_subst,
	      [u_ob, u_substit, u_depth, u_omit_slots, u_include_slots],
	      
	      [ [setq, u_xx_instan_obs_xx, []],
		[setq, u_xx_any_unbound_c63_xx, []],
		
		[ u_ob_c36_instantiate2,
		  u_ob,
		  u_xx_empty_bd_xx,
		  u_depth,
		  u_omit_slots,
		  u_include_slots,
		  u_substit,
		  [],
		  []
		]
	      ]).


% annotating U::OB$SUBST 
wl: arglist_info(u_ob_c36_subst,
		[u_ob, u_substit, u_depth, u_omit_slots, u_include_slots],
		
		[ Ob_Param,
		  Substit_Param,
		  Depth_Param,
		  Omit_slots_Param,
		  Include_slots_Param
		],
		arginfo{ all:
			     [ u_ob,
			       u_substit,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob,
				 u_substit,
				 u_depth,
				 u_omit_slots,
				 u_include_slots
			       ],
			 opt:0,
			 req:
			     [ u_ob,
			       u_substit,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$SUBST 
wl: init_args(exact_only, u_ob_c36_subst).


% annotating U::OB$SUBST 
f_u_ob_c36_subst(Ob_Param, Substit_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_substit, Substit_Param), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param), bv(u_include_slots, Include_slots_Param)],
	set_var(Env, setq, u_xx_instan_obs_xx, []),
	set_var(Env, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_xx_empty_bd_xx,
				u_depth,
				u_omit_slots,
				u_include_slots,
				u_substit,
				[],
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_subst, classof, claz_function),
   set_opv(u_ob_c36_subst, compile_as, kw_function),
   set_opv(u_ob_c36_subst, function, f_u_ob_c36_subst),
   DefunResult=u_ob_c36_subst.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1143 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1361)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1143 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" variabilize?: a predicate determining whether an ob should be abstracted",
				     1,
				     1363)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1143 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" and converted into a unique variable. Multiple occurences of the same ob",
				     1,
				     1438)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1143 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" will become the same variable.", 1, 1513)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1143 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1546)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1547 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$variabilize',
			    
			    [ ob,
			      'variabilize?',
			      depth,
			      'omit-slots',
			      'include-slots'
			    ],
			    [setq, '*instan-obs*', []],
			    [setq, '*any-unbound?*', []],
			    
			    [ 'ob$instantiate2',
			      ob,
			      '*empty-bd*',
			      depth,
			      'omit-slots',
			      'include-slots',
			      [quote, [t]],
			      'variabilize?',
			      []
			    ]
			  ]).

% annotating U::OB$VARIABILIZE 
wl: lambda_def(defun,
	      u_ob_c36_variabilize,
	      f_u_ob_c36_variabilize,
	      [u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots],
	      
	      [ [setq, u_xx_instan_obs_xx, []],
		[setq, u_xx_any_unbound_c63_xx, []],
		
		[ u_ob_c36_instantiate2,
		  u_ob,
		  u_xx_empty_bd_xx,
		  u_depth,
		  u_omit_slots,
		  u_include_slots,
		  [quote, [t]],
		  u_variabilize_c63,
		  []
		]
	      ]).


% annotating U::OB$VARIABILIZE 
wl: arglist_info(u_ob_c36_variabilize,
		[u_ob, u_variabilize_c63, u_depth, u_omit_slots, u_include_slots],
		
		[ Ob_Param,
		  Variabilize_c63_Param,
		  Depth_Param,
		  Omit_slots_Param,
		  Include_slots_Param
		],
		arginfo{ all:
			     [ u_ob,
			       u_variabilize_c63,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob,
				 u_variabilize_c63,
				 u_depth,
				 u_omit_slots,
				 u_include_slots
			       ],
			 opt:0,
			 req:
			     [ u_ob,
			       u_variabilize_c63,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$VARIABILIZE 
wl: init_args(exact_only, u_ob_c36_variabilize).


% annotating U::OB$VARIABILIZE 
f_u_ob_c36_variabilize(Ob_Param, Variabilize_c63_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_variabilize_c63, Variabilize_c63_Param), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param), bv(u_include_slots, Include_slots_Param)],
	set_var(Env, setq, u_xx_instan_obs_xx, []),
	set_var(Env, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_xx_empty_bd_xx,
				u_depth,
				u_omit_slots,
				u_include_slots,
				[quote, [t]],
				u_variabilize_c63,
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_variabilize, classof, claz_function),
   set_opv(u_ob_c36_variabilize, compile_as, kw_function),
   set_opv(u_ob_c36_variabilize, function, f_u_ob_c36_variabilize),
   DefunResult=u_ob_c36_variabilize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$varize',
			    [ob, 'variabilize?'],
			    [setq, '*instan-obs*', []],
			    [setq, '*any-unbound?*', []],
			    
			    [ 'ob$instantiate2',
			      ob,
			      '*empty-bd*',
			      100,
			      [],
			      [],
			      [quote, [t]],
			      'variabilize?',
			      []
			    ]
			  ]).

% annotating U::OB$VARIZE 
wl: lambda_def(defun,
	      u_ob_c36_varize,
	      f_u_ob_c36_varize,
	      [u_ob, u_variabilize_c63],
	      
	      [ [setq, u_xx_instan_obs_xx, []],
		[setq, u_xx_any_unbound_c63_xx, []],
		
		[ u_ob_c36_instantiate2,
		  u_ob,
		  u_xx_empty_bd_xx,
		  100,
		  [],
		  [],
		  [quote, [t]],
		  u_variabilize_c63,
		  []
		]
	      ]).


% annotating U::OB$VARIZE 
wl: arglist_info(u_ob_c36_varize,
		[u_ob, u_variabilize_c63],
		[Ob_Param, Variabilize_c63_Param],
		arginfo{ all:[u_ob, u_variabilize_c63],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_variabilize_c63],
			 opt:0,
			 req:[u_ob, u_variabilize_c63],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$VARIZE 
wl: init_args(exact_only, u_ob_c36_varize).


% annotating U::OB$VARIZE 
f_u_ob_c36_varize(Ob_Param, Variabilize_c63_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_variabilize_c63, Variabilize_c63_Param)],
	set_var(Env, setq, u_xx_instan_obs_xx, []),
	set_var(Env, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_xx_empty_bd_xx,
				100,
				[],
				[],
				[quote, [t]],
				u_variabilize_c63,
				[],
				C36_instantiate2_Ret),
	C36_instantiate2_Ret=FnResult.
:- set_opv(f_u_ob_c36_varize, classof, claz_function),
   set_opv(u_ob_c36_varize, compile_as, kw_function),
   set_opv(u_ob_c36_varize, function, f_u_ob_c36_varize),
   DefunResult=u_ob_c36_varize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1781 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1961)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" omit-proc: a predicate determining whether an ob should be returned",
				     1,
				     1963)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1781 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" as is, without instantiation.", 1, 2033)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:1781 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2065)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2066 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instan-omit',
			    
			    [ ob,
			      bd,
			      'omit-proc',
			      depth,
			      'omit-slots',
			      'include-slots'
			    ],
			    [setq, '*instan-obs*', []],
			    [setq, '*any-unbound?*', []],
			    
			    [ 'ob$instantiate2',
			      ob,
			      bd,
			      depth,
			      'omit-slots',
			      'include-slots',
			      [],
			      [],
			      'omit-proc'
			    ]
			  ]).

% annotating U::OB$INSTAN-OMIT 
wl: lambda_def(defun,
	      u_ob_c36_instan_omit,
	      f_u_ob_c36_instan_omit,
	      [u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots],
	      
	      [ [setq, u_xx_instan_obs_xx, []],
		[setq, u_xx_any_unbound_c63_xx, []],
		
		[ u_ob_c36_instantiate2,
		  u_ob,
		  u_bd,
		  u_depth,
		  u_omit_slots,
		  u_include_slots,
		  [],
		  [],
		  u_omit_proc
		]
	      ]).


% annotating U::OB$INSTAN-OMIT 
wl: arglist_info(u_ob_c36_instan_omit,
		[u_ob, u_bd, u_omit_proc, u_depth, u_omit_slots, u_include_slots],
		
		[ Ob_Param,
		  Bd_Param,
		  Omit_proc_Param,
		  Depth_Param,
		  Omit_slots_Param,
		  Include_slots_Param
		],
		arginfo{ all:
			     [ u_ob,
			       u_bd,
			       u_omit_proc,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_ob,
				 u_bd,
				 u_omit_proc,
				 u_depth,
				 u_omit_slots,
				 u_include_slots
			       ],
			 opt:0,
			 req:
			     [ u_ob,
			       u_bd,
			       u_omit_proc,
			       u_depth,
			       u_omit_slots,
			       u_include_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTAN-OMIT 
wl: init_args(exact_only, u_ob_c36_instan_omit).


% annotating U::OB$INSTAN-OMIT 
f_u_ob_c36_instan_omit(Ob_Param, Bd_Param, Omit_proc_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_bd, Bd_Param), bv(u_omit_proc, Omit_proc_Param), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param), bv(u_include_slots, Include_slots_Param)],
	set_var(Env, setq, u_xx_instan_obs_xx, []),
	set_var(Env, setq, u_xx_any_unbound_c63_xx, []),
	f_u_ob_c36_instantiate2(u_ob,
				u_bd,
				u_depth,
				u_omit_slots,
				u_include_slots,
				[],
				[],
				u_omit_proc,
				Omit_proc),
	Omit_proc=FnResult.
:- set_opv(f_u_ob_c36_instan_omit, classof, claz_function),
   set_opv(u_ob_c36_instan_omit, compile_as, kw_function),
   set_opv(u_ob_c36_instan_omit, function, f_u_ob_c36_instan_omit),
   DefunResult=u_ob_c36_instan_omit.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2268 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*instantiate-omit-obs*', []]).
:- set_var(TLEnv3, setq, u_xx_instantiate_omit_obs_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2303 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate-dbg',
			    
			    [ template,
			      bindings,
			      depth,
			      'omit-slots',
			      'include-slots',
			      substit,
			      abstract,
			      'omit-proc'
			    ],
			    ['ndbg-begin'],
			    
			    [ ndbg,
			      '*gate-dbg*',
			      instantiate,
			      '$STRING'("Call ob$instantiate3: ~A ~A~%"),
			      template,
			      bindings
			    ],
			    
			    [ let,
			      
			      [ 
				[ result,
				  
				  [ 'ob$instantiate3',
				    template,
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
			      
			      [ ndbg,
				'*gate-dbg*',
				instantiate,
				'$STRING'("Return from ob$instantiate3: ~A~%"),
				result
			      ],
			      ['ndbg-end'],
			      result
			    ]
			  ]).

% annotating U::OB$INSTANTIATE-DBG 
wl: lambda_def(defun,
	      u_ob_c36_instantiate_dbg,
	      f_u_ob_c36_instantiate_dbg,
	      
	      [ u_template,
		bindings,
		u_depth,
		u_omit_slots,
		u_include_slots,
		u_substit,
		u_abstract,
		u_omit_proc
	      ],
	      
	      [ [u_ndbg_begin],
		
		[ u_ndbg,
		  u_xx_gate_dbg_xx,
		  u_instantiate,
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
			     #\(i),
			     #\(n),
			     #\(s),
			     #\(t),
			     #\(a),
			     #\(n),
			     #\(t),
			     #\(i),
			     #\(a),
			     #\(t),
			     #\(e),
			     #\('3'),
			     #\(:),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(' '),
			     #\(~),
			     #\('A'),
			     #\(~),
			     #\('%')
			   ]),
		  u_template,
		  bindings
		],
		
		[ let,
		  
		  [ 
		    [ u_result,
		      
		      [ u_ob_c36_instantiate3,
			u_template,
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
		  
		  [ u_ndbg,
		    u_xx_gate_dbg_xx,
		    u_instantiate,
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
			       #\(i),
			       #\(n),
			       #\(s),
			       #\(t),
			       #\(a),
			       #\(n),
			       #\(t),
			       #\(i),
			       #\(a),
			       #\(t),
			       #\(e),
			       #\('3'),
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


% annotating U::OB$INSTANTIATE-DBG 
wl: arglist_info(u_ob_c36_instantiate_dbg,
		
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

% annotating U::OB$INSTANTIATE-DBG 
wl: init_args(exact_only, u_ob_c36_instantiate_dbg).


% annotating U::OB$INSTANTIATE-DBG 
f_u_ob_c36_instantiate_dbg(Template_Param, Bindings_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, Substit_Param, Abstract_Param, Omit_proc_Param, FnResult) :-
	Env=[bv(u_template, Template_Param), bv(bindings, Bindings_Param), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param), bv(u_include_slots, Include_slots_Param), bv(u_substit, Substit_Param), bv(u_abstract, Abstract_Param), bv(u_omit_proc, Omit_proc_Param)],
	f_u_ndbg_begin(Ndbg_begin_Ret),
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_instantiate,
		 
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
			      #\(i),
			      #\(n),
			      #\(s),
			      #\(t),
			      #\(a),
			      #\(n),
			      #\(t),
			      #\(i),
			      #\(a),
			      #\(t),
			      #\(e),
			      #\('3'),
			      #\(:),
			      #\(' '),
			      #\(~),
			      #\('A'),
			      #\(' '),
			      #\(~),
			      #\('A'),
			      #\(~),
			      #\('%')
			    ]),
		   u_template,
		   bindings
		 ],
		 Ndbg_Ret),
	f_u_ob_c36_instantiate3(Template_Param,
				Bindings_Param,
				Depth_Param,
				Omit_slots_Param,
				Include_slots_Param,
				Substit_Param,
				Abstract_Param,
				Omit_proc_Param,
				Result_Init),
	LEnv=[[bv(u_result, Result_Init)]|Env],
	f_u_ndbg(u_xx_gate_dbg_xx,
		 u_instantiate,
		 
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
			      #\(i),
			      #\(n),
			      #\(s),
			      #\(t),
			      #\(a),
			      #\(n),
			      #\(t),
			      #\(i),
			      #\(a),
			      #\(t),
			      #\(e),
			      #\('3'),
			      #\(:),
			      #\(' '),
			      #\(~),
			      #\('A'),
			      #\(~),
			      #\('%')
			    ]),
		   u_result
		 ],
		 Ndbg_Ret43),
	f_u_ndbg_end(Ndbg_end_Ret),
	get_var(LEnv, u_result, Result_Get),
	LetResult=Result_Get,
	LetResult=FnResult.
:- set_opv(f_u_ob_c36_instantiate_dbg, classof, claz_function),
   set_opv(u_ob_c36_instantiate_dbg, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate_dbg, function, f_u_ob_c36_instantiate_dbg),
   DefunResult=u_ob_c36_instantiate_dbg.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2303 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2882)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2303 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This should never be called from the top-level, at least without not",
				     1,
				     2884)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2303 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" first doing (setq *instan-obs* nil) and (setq *any-unbound?* nil).",
				     1,
				     2955)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:2303 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3024)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate3',
			    
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
			      
			      [ 
				[ let,
				  [[found, [assq, template, '*instan-obs*']]],
				  [if, found, [cdr, found], []]
				]
			      ],
			      [[and, depth, [<, depth, 0]], template],
			      
			      [ 
				[ and,
				  'omit-proc',
				  [funcall, 'omit-proc', template]
				],
				template
			      ],
			      [[not, ['ob?', template]], template],
			      
			      [ ['var?', template],
				
				[ let,
				  
				  [ 
				    [ found,
				      
				      [ 'bd-hyper-lookup',
					['variable-name', template],
					bindings
				      ]
				    ]
				  ],
				  
				  [ if,
				    found,
				    
				    [ cond,
				      
				      [ ['var?', found],
					[setq, '*any-unbound?*', t],
					found
				      ],
				      
				      [ [and, ['ob?', found], ['vars-in?', found]],
					
					[ 'ob$instantiate2',
					  found,
					  bindings,
					  [if, depth, ['-1+', depth], []],
					  'omit-slots',
					  'include-slots',
					  substit,
					  abstract,
					  'omit-proc'
					]
				      ],
				      [else, found]
				    ],
				    [progn, [setq, '*any-unbound?*', t], template]
				  ]
				]
			      ],
			      
			      [ ['special?', template],
				
				[ 'ob$instan-special',
				  template,
				  bindings,
				  [if, depth, ['-1+', depth], []],
				  'omit-slots',
				  'include-slots',
				  substit,
				  abstract,
				  'omit-proc'
				]
			      ],
			      
			      [ else,
				
				[ let,
				  [['result-ob', ['ob$create-empty']]],
				  
				  [ setq,
				    '*instan-obs*',
				    
				    [ cons,
				      [cons, template, 'result-ob'],
				      '*instan-obs*'
				    ]
				  ],
				  
				  [ yloop,
				    
				    [ initial,
				      [rest, ['ob$pairs', template]],
				      [substitution, []]
				    ],
				    [ywhile, rest],
				    
				    [ ydo,
				      
				      [ if,
					
					[ and,
					  
					  [ not,
					    
					    [ 'memq?',
					      ['slots-name', [car, rest]],
					      'omit-slots'
					    ]
					  ],
					  
					  [ not,
					    
					    [ 'memq?',
					      ['slots-name', [car, rest]],
					      '*permanent-ignore-slots*'
					    ]
					  ],
					  
					  [ not,
					    
					    [ 'null?',
					      ['slots-value', [car, rest]]
					    ]
					  ],
					  
					  [ if,
					    'include-slots',
					    
					    [ 'memq?',
					      ['slots-name', [car, rest]],
					      'include-slots'
					    ],
					    t
					  ]
					],
					
					[ progn,
					  
					  [ setq,
					    substitution,
					    
					    [ 'bd-lookup',
					      ['slots-value', [car, rest]],
					      substit
					    ]
					  ],
					  
					  [ 'ob$add',
					    'result-ob',
					    ['slots-name', [car, rest]],
					    
					    [ cond,
					      [substitution, substitution],
					      
					      [ 
						[ and,
						  abstract,
						  
						  [ funcall,
						    abstract,
						    ['slots-value', [car, rest]]
						  ]
						],
						
						[ let,
						  
						  [ 
						    [ uniqvar,
						      
						      [ 'make-var',
							
							[ 'gen-id',
							  '$STRING'("var")
							],
							
							[ 'ty$get-major-type',
							  
							  [ 'ob$ty',
							    
							    [ 'slots-value',
							      [car, rest]
							    ]
							  ]
							]
						      ]
						    ]
						  ],
						  
						  [ setq,
						    substit,
						    
						    [ 'bd-bind!',
						      
						      [ 'slots-value',
							[car, rest]
						      ],
						      uniqvar,
						      substit
						    ]
						  ],
						  uniqvar
						]
					      ],
					      
					      [ else,
						
						[ if,
						  
						  [ or,
						    
						    [ 'memq?',
						      
						      [ 'slots-value',
							[car, rest]
						      ],
						      '*instantiate-omit-obs*'
						    ],
						    
						    [ and,
						      
						      [ 'ob?',
							
							[ 'slots-value',
							  [car, rest]
							]
						      ],
						      
						      [ 'ob$literal?',
							
							[ 'slots-value',
							  [car, rest]
							]
						      ]
						    ]
						  ],
						  ['slots-value', [car, rest]],
						  
						  [ 'ob$instantiate2',
						    ['slots-value', [car, rest]],
						    bindings,
						    
						    [ if,
						      depth,
						      ['-1+', depth],
						      []
						    ],
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
					]
				      ],
				      [setq, rest, [cdr, rest]]
				    ],
				    [yresult, 'result-ob']
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$INSTANTIATE3 
wl: lambda_def(defun,
	      u_ob_c36_instantiate3,
	      f_u_ob_c36_instantiate3,
	      
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
		  
		  [ 
		    [ let,
		      [[u_found, [ext_assq, u_template, u_xx_instan_obs_xx]]],
		      [if, u_found, [cdr, u_found], []]
		    ]
		  ],
		  [[and, u_depth, [<, u_depth, 0]], u_template],
		  
		  [ [and, u_omit_proc, [funcall, u_omit_proc, u_template]],
		    u_template
		  ],
		  [[not, [u_ob_c63, u_template]], u_template],
		  
		  [ [u_var_c63, u_template],
		    
		    [ let,
		      
		      [ 
			[ u_found,
			  
			  [ u_bd_hyper_lookup,
			    [u_variable_name, u_template],
			    bindings
			  ]
			]
		      ],
		      
		      [ if,
			u_found,
			
			[ cond,
			  
			  [ [u_var_c63, u_found],
			    [setq, u_xx_any_unbound_c63_xx, t],
			    u_found
			  ],
			  
			  [ [and, [u_ob_c63, u_found], [u_vars_in_c63, u_found]],
			    
			    [ u_ob_c36_instantiate2,
			      u_found,
			      bindings,
			      [if, u_depth, ['-1+', u_depth], []],
			      u_omit_slots,
			      u_include_slots,
			      u_substit,
			      u_abstract,
			      u_omit_proc
			    ]
			  ],
			  [u_else, u_found]
			],
			[progn, [setq, u_xx_any_unbound_c63_xx, t], u_template]
		      ]
		    ]
		  ],
		  
		  [ [u_special_c63, u_template],
		    
		    [ u_ob_c36_instan_special,
		      u_template,
		      bindings,
		      [if, u_depth, ['-1+', u_depth], []],
		      u_omit_slots,
		      u_include_slots,
		      u_substit,
		      u_abstract,
		      u_omit_proc
		    ]
		  ],
		  
		  [ u_else,
		    
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
			
			[ u_initial,
			  [rest, [u_ob_c36_pairs, u_template]],
			  [u_substitution, []]
			],
			[u_ywhile, rest],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ and,
			      
			      [ not,
				
				[ u_memq_c63,
				  [u_slots_name, [car, rest]],
				  u_omit_slots
				]
			      ],
			      
			      [ not,
				
				[ u_memq_c63,
				  [u_slots_name, [car, rest]],
				  u_xx_permanent_ignore_slots_xx
				]
			      ],
			      [not, [u_null_c63, [u_slots_value, [car, rest]]]],
			      
			      [ if,
				u_include_slots,
				
				[ u_memq_c63,
				  [u_slots_name, [car, rest]],
				  u_include_slots
				],
				t
			      ]
			    ],
			    
			    [ progn,
			      
			      [ setq,
				u_substitution,
				
				[ u_bd_lookup,
				  [u_slots_value, [car, rest]],
				  u_substit
				]
			      ],
			      
			      [ u_ob_c36_add,
				u_result_ob,
				[u_slots_name, [car, rest]],
				
				[ cond,
				  [u_substitution, u_substitution],
				  
				  [ 
				    [ and,
				      u_abstract,
				      
				      [ funcall,
					u_abstract,
					[u_slots_value, [car, rest]]
				      ]
				    ],
				    
				    [ let,
				      
				      [ 
					[ u_uniqvar,
					  
					  [ u_make_var,
					    
					    [ u_gen_id,
					      '$ARRAY'([*],
						       claz_base_character,
						       [#\(v), #\(a), #\(r)])
					    ],
					    
					    [ u_ty_c36_get_major_type,
					      
					      [ u_ob_c36_ty,
						[u_slots_value, [car, rest]]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ setq,
					u_substit,
					
					[ u_bd_bind_c33,
					  [u_slots_value, [car, rest]],
					  u_uniqvar,
					  u_substit
					]
				      ],
				      u_uniqvar
				    ]
				  ],
				  
				  [ u_else,
				    
				    [ if,
				      
				      [ or,
					
					[ u_memq_c63,
					  [u_slots_value, [car, rest]],
					  u_xx_instantiate_omit_obs_xx
					],
					
					[ and,
					  
					  [ u_ob_c63,
					    [u_slots_value, [car, rest]]
					  ],
					  
					  [ u_ob_c36_literal_c63,
					    [u_slots_value, [car, rest]]
					  ]
					]
				      ],
				      [u_slots_value, [car, rest]],
				      
				      [ u_ob_c36_instantiate2,
					[u_slots_value, [car, rest]],
					bindings,
					[if, u_depth, ['-1+', u_depth], []],
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
			    ]
			  ],
			  [setq, rest, [cdr, rest]]
			],
			[u_yresult, u_result_ob]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$INSTANTIATE3 
wl: arglist_info(u_ob_c36_instantiate3,
		
		[ u_template,
		  bindings,
		  u_depth,
		  u_omit_slots,
		  u_include_slots,
		  u_substit,
		  u_abstract,
		  u_omit_proc
		],
		
		[ Template_Get110,
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

% annotating U::OB$INSTANTIATE3 
wl: init_args(exact_only, u_ob_c36_instantiate3).


% annotating U::OB$INSTANTIATE3 
f_u_ob_c36_instantiate3(Template_Get110, Bindings_Param, Depth_Param, Omit_slots_Param, Include_slots_Param, Substit_Param, Abstract_Param, Omit_proc_Param, ElseResult82) :-
	Env=[bv(u_template, Template_Get110), bv(bindings, Bindings_Param), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param), bv(u_include_slots, Include_slots_Param), bv(u_substit, Substit_Param), bv(u_abstract, Abstract_Param), bv(u_omit_proc, Omit_proc_Param)],
	f_ext_assq(u_template, u_xx_instan_obs_xx, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST31),
	(   IFTEST31\==[]
	->  get_var(LEnv, u_found, Found_Get35),
	    cl_cdr(Found_Get35, TrueResult),
	    _131542=TrueResult
	;   _131542=[]
	),
	IFTEST=_131542,
	(   IFTEST\==[]
	->  ElseResult82=[]
	;   (   Depth_Param\==[]
	    ->  <(Depth_Param, 0, TrueResult43),
		IFTEST37=TrueResult43
	    ;   IFTEST37=[]
	    ),
	    (   IFTEST37\==[]
	    ->  ElseResult82=Template_Get110
	    ;   (   Omit_proc_Param\==[]
		->  f_u_omit_proc(Template_Get110, TrueResult51),
		    IFTEST45=TrueResult51
		;   IFTEST45=[]
		),
		(   IFTEST45\==[]
		->  ElseResult82=Template_Get110
		;   f_u_ob_c63(u_template, PredArgResult),
		    (   PredArgResult==[]
		    ->  ElseResult82=Template_Get110
		    ;   f_u_var_c63(u_template, IFTEST57),
			(   IFTEST57\==[]
			->  f_u_bd_hyper_lookup([u_variable_name, u_template],
						bindings,
						Found_Init61),
			    Env=[[bv(u_found, Found_Init61)]|Env],
			    get_var(Env, u_found, IFTEST62),
			    (   IFTEST62\==[]
			    ->  f_u_var_c63(u_found, IFTEST66),
				(   IFTEST66\==[]
				->  set_var(Env,
					    setq,
					    u_xx_any_unbound_c63_xx,
					    t),
				    get_var(Env, u_found, Found_Get68),
				    ElseResult82=Found_Get68
				;   f_u_ob_c63(u_found, IFTEST71),
				    (   IFTEST71\==[]
				    ->  get_var(Env, u_found, Found_Get73),
					f_u_vars_in_c63(Found_Get73,
							TrueResult74),
					IFTEST69=TrueResult74
				    ;   IFTEST69=[]
				    ),
				    (   IFTEST69\==[]
				    ->  f_u_ob_c36_instantiate2(u_found,
								bindings,
								
								[ if,
								  u_depth,
								  ['-1+', u_depth],
								  []
								],
								u_omit_slots,
								u_include_slots,
								u_substit,
								u_abstract,
								u_omit_proc,
								TrueResult81),
					ElseResult82=TrueResult81
				    ;   get_var(Env, u_else, IFTEST75),
					(   IFTEST75\==[]
					->  get_var(Env, u_found, Found_Get78),
					    ElseResult82=Found_Get78
					;   ElseResult82=[]
					)
				    )
				)
			    ;   set_var(Env, setq, u_xx_any_unbound_c63_xx, t),
				ElseResult82=Template_Get110
			    )
			;   f_u_special_c63(u_template, IFTEST89),
			    (   IFTEST89\==[]
			    ->  (   Depth_Param\==[]
				->  f_u_1_c43(Depth_Param, TrueResult97),
				    _133164=TrueResult97
				;   _133164=[]
				),
				f_u_ob_c36_instan_special(Template_Get110,
							  Bindings_Param,
							  _133164,
							  Omit_slots_Param,
							  Include_slots_Param,
							  Substit_Param,
							  Abstract_Param,
							  Omit_proc_Param,
							  TrueResult116),
				ElseResult82=TrueResult116
			    ;   get_var(Env, u_else, IFTEST103),
				(   IFTEST103\==[]
				->  f_u_ob_c36_create_empty(Result_ob_Init),
				    Env=[[bv(u_result_ob, Result_ob_Init)]|Env],
				    get_var(Env, u_result_ob, Result_ob_Get),
				    CAR=[Template_Get110|Result_ob_Get],
				    get_var(Env,
					    u_xx_instan_obs_xx,
					    Xx_instan_obs_xx_Get),
				    Xx_instan_obs_xx=[CAR|Xx_instan_obs_xx_Get],
				    set_var(Env,
					    u_xx_instan_obs_xx,
					    Xx_instan_obs_xx),
				    f_u_yloop(
					      [ 
						[ u_initial,
						  
						  [ rest,
						    
						    [ u_ob_c36_pairs,
						      u_template
						    ]
						  ],
						  [u_substitution, []]
						],
						[u_ywhile, rest],
						
						[ u_ydo,
						  
						  [ if,
						    
						    [ and,
						      
						      [ not,
							
							[ u_memq_c63,
							  
							  [ u_slots_name,
							    [car, rest]
							  ],
							  u_omit_slots
							]
						      ],
						      
						      [ not,
							
							[ u_memq_c63,
							  
							  [ u_slots_name,
							    [car, rest]
							  ],
							  u_xx_permanent_ignore_slots_xx
							]
						      ],
						      
						      [ not,
							
							[ u_null_c63,
							  
							  [ u_slots_value,
							    [car, rest]
							  ]
							]
						      ],
						      
						      [ if,
							u_include_slots,
							
							[ u_memq_c63,
							  
							  [ u_slots_name,
							    [car, rest]
							  ],
							  u_include_slots
							],
							t
						      ]
						    ],
						    
						    [ progn,
						      
						      [ setq,
							u_substitution,
							
							[ u_bd_lookup,
							  
							  [ u_slots_value,
							    [car, rest]
							  ],
							  u_substit
							]
						      ],
						      
						      [ u_ob_c36_add,
							u_result_ob,
							
							[ u_slots_name,
							  [car, rest]
							],
							
							[ cond,
							  
							  [ u_substitution,
							    u_substitution
							  ],
							  
							  [ 
							    [ and,
							      u_abstract,
							      
							      [ funcall,
								u_abstract,
								
								[ u_slots_value,
								  [car, rest]
								]
							      ]
							    ],
							    
							    [ let,
							      
							      [ 
								[ u_uniqvar,
								  
								  [ u_make_var,
								    
								    [ u_gen_id,
								      '$ARRAY'([*],
									       claz_base_character,
									       [#\(v), #\(a), #\(r)])
								    ],
								    
								    [ u_ty_c36_get_major_type,
								      
								      [ u_ob_c36_ty,
									
									[ u_slots_value,
									  [car, rest]
									]
								      ]
								    ]
								  ]
								]
							      ],
							      
							      [ setq,
								u_substit,
								
								[ u_bd_bind_c33,
								  
								  [ u_slots_value,
								    [car, rest]
								  ],
								  u_uniqvar,
								  u_substit
								]
							      ],
							      u_uniqvar
							    ]
							  ],
							  
							  [ u_else,
							    
							    [ if,
							      
							      [ or,
								
								[ u_memq_c63,
								  
								  [ u_slots_value,
								    [car, rest]
								  ],
								  u_xx_instantiate_omit_obs_xx
								],
								
								[ and,
								  
								  [ u_ob_c63,
								    
								    [ u_slots_value,
								      [car, rest]
								    ]
								  ],
								  
								  [ u_ob_c36_literal_c63,
								    
								    [ u_slots_value,
								      [car, rest]
								    ]
								  ]
								]
							      ],
							      
							      [ u_slots_value,
								[car, rest]
							      ],
							      
							      [ u_ob_c36_instantiate2,
								
								[ u_slots_value,
								  [car, rest]
								],
								bindings,
								
								[ if,
								  u_depth,
								  ['-1+', u_depth],
								  []
								],
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
						    ]
						  ],
						  [setq, rest, [cdr, rest]]
						],
						[u_yresult, u_result_ob]
					      ],
					      Yloop_Ret),
				    ElseResult82=Yloop_Ret
				;   ElseResult82=[]
				)
			    )
			)
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_instantiate3, classof, claz_function),
   set_opv(u_ob_c36_instantiate3, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate3, function, f_u_ob_c36_instantiate3),
   DefunResult=u_ob_c36_instantiate3.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("            (ndbg *gate-dbg* ob-warn \"(?~A binding cycle)~%\"",
				     1,
				     3618)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                  (variable-name found))",
				     1,
				     3680)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("           (ndbg *gate-dbg* ob-warn \"(?~A unbound)~%\"",
				     1,
				     4048)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                      (variable-name template))",
				     1,
				     4103)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (ob? template)", 10, 4397)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" todo", 61, 4880)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6695)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6697)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" ob$instantiate!:", 1, 6699)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6718)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This version of instantiate does not copy anything.",
				     1,
				     6720)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" It simply replaces all bound variables with their values.",
				     1,
				     6774)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6834)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:3026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\nMoved to gate_instan2.cl\n\n(defun ob$instan-special \n (template bindings depth omit-slots include-slots substit abstract omit-proc) \n .. )\n",
				     2,
				     6551)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:6887 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate!',
			    [template, bindings],
			    ['ob$instantiate1!', template, bindings, []]
			  ]).

% annotating U::OB$INSTANTIATE! 
wl: lambda_def(defun,
	      u_ob_c36_instantiate_c33,
	      f_u_ob_c36_instantiate_c33,
	      [u_template, bindings],
	      [[u_ob_c36_instantiate1_c33, u_template, bindings, []]]).


% annotating U::OB$INSTANTIATE! 
wl: arglist_info(u_ob_c36_instantiate_c33,
		[u_template, bindings],
		[Template_Param, Bindings_Param],
		arginfo{ all:[u_template, bindings],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_template, bindings],
			 opt:0,
			 req:[u_template, bindings],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTANTIATE! 
wl: init_args(exact_only, u_ob_c36_instantiate_c33).


% annotating U::OB$INSTANTIATE! 
f_u_ob_c36_instantiate_c33(Template_Param, Bindings_Param, FnResult) :-
	f_u_ob_c36_instantiate1_c33(Template_Param,
				    Bindings_Param,
				    [],
				    Instantiate1_c33_Ret),
	Instantiate1_c33_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate_c33, classof, claz_function),
   set_opv(u_ob_c36_instantiate_c33, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate_c33, function, f_u_ob_c36_instantiate_c33),
   DefunResult=u_ob_c36_instantiate_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:6975 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate1!',
			    [template, bindings, depth],
			    ['ob$instantiate2!', template, bindings, depth, [], []]
			  ]).

% annotating U::OB$INSTANTIATE1! 
wl: lambda_def(defun,
	      u_ob_c36_instantiate1_c33,
	      f_u_ob_c36_instantiate1_c33,
	      [u_template, bindings, u_depth],
	      [[u_ob_c36_instantiate2_c33, u_template, bindings, u_depth, [], []]]).


% annotating U::OB$INSTANTIATE1! 
wl: arglist_info(u_ob_c36_instantiate1_c33,
		[u_template, bindings, u_depth],
		[Template_Param, Bindings_Param, Depth_Param],
		arginfo{ all:[u_template, bindings, u_depth],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_template, bindings, u_depth],
			 opt:0,
			 req:[u_template, bindings, u_depth],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTANTIATE1! 
wl: init_args(exact_only, u_ob_c36_instantiate1_c33).


% annotating U::OB$INSTANTIATE1! 
f_u_ob_c36_instantiate1_c33(Template_Param, Bindings_Param, Depth_Param, FnResult) :-
	f_u_ob_c36_instantiate2_c33(Template_Param,
				    Bindings_Param,
				    Depth_Param,
				    [],
				    [],
				    Instantiate2_c33_Ret),
	Instantiate2_c33_Ret=FnResult.
:- set_opv(f_u_ob_c36_instantiate1_c33, classof, claz_function),
   set_opv(u_ob_c36_instantiate1_c33, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate1_c33, function, f_u_ob_c36_instantiate1_c33),
   DefunResult=u_ob_c36_instantiate1_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$instantiate2!',
			    [template, bindings, depth, ob, 'slot-name'],
			    
			    [ cond,
			      
			      [ ['var?', template],
				
				[ let,
				  
				  [ 
				    [ found,
				      
				      [ assq,
					['variable-name', template],
					[cdr, bindings]
				      ]
				    ]
				  ],
				  
				  [ if,
				    found,
				    
				    [ progn,
				      ['ob$remove', ob, 'slot-name', template],
				      ['ob$add', ob, 'slot-name', [cadr, found]],
				      [cadr, found]
				    ],
				    
				    [ progn,
				      
				      [ ndbg,
					'*gate-dbg*',
					'ob-warn',
					'$STRING'("Warning: No binding for ~A in instantiate.~%"),
					template
				      ],
				      template
				    ]
				  ]
				]
			      ],
			      
			      [ ['ob?', template],
				
				[ yloop,
				  [initial, [rest, ['ob$pairs', template]]],
				  [ywhile, rest],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ and,
					['ob?', ['slots-value', [car, rest]]],
					
					[ 'ob$literal?',
					  ['slots-value', [car, rest]]
					]
				      ],
				      ['slots-value', [car, rest]],
				      
				      [ if,
					['number?', depth],
					
					[ if,
					  [>, depth, 1],
					  
					  [ 'ob$instantiate2!',
					    ['slots-value', [car, rest]],
					    bindings,
					    ['-1+', depth],
					    template,
					    ['slots-name', [car, rest]]
					  ],
					  ['slots-value', [car, rest]]
					],
					
					[ 'ob$instantiate2!',
					  ['slots-value', [car, rest]],
					  bindings,
					  [],
					  template,
					  ['slots-name', [car, rest]]
					]
				      ]
				    ],
				    [setq, rest, [cdr, rest]]
				  ],
				  [yresult, template]
				]
			      ],
			      [else, template]
			    ]
			  ]).

% annotating U::OB$INSTANTIATE2! 
wl: lambda_def(defun,
	      u_ob_c36_instantiate2_c33,
	      f_u_ob_c36_instantiate2_c33,
	      [u_template, bindings, u_depth, u_ob, u_slot_name],
	      
	      [ 
		[ cond,
		  
		  [ [u_var_c63, u_template],
		    
		    [ let,
		      
		      [ 
			[ u_found,
			  
			  [ ext_assq,
			    [u_variable_name, u_template],
			    [cdr, bindings]
			  ]
			]
		      ],
		      
		      [ if,
			u_found,
			
			[ progn,
			  [u_ob_c36_remove, u_ob, u_slot_name, u_template],
			  [u_ob_c36_add, u_ob, u_slot_name, [cadr, u_found]],
			  [cadr, u_found]
			],
			
			[ progn,
			  
			  [ u_ndbg,
			    u_xx_gate_dbg_xx,
			    u_ob_warn,
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
				       #\('N'),
				       #\(o),
				       #\(' '),
				       #\(b),
				       #\(i),
				       #\(n),
				       #\(d),
				       #\(i),
				       #\(n),
				       #\(g),
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
				       #\(i),
				       #\(n),
				       #\(s),
				       #\(t),
				       #\(a),
				       #\(n),
				       #\(t),
				       #\(i),
				       #\(a),
				       #\(t),
				       #\(e),
				       #\('.'),
				       #\(~),
				       #\('%')
				     ]),
			    u_template
			  ],
			  u_template
			]
		      ]
		    ]
		  ],
		  
		  [ [u_ob_c63, u_template],
		    
		    [ u_yloop,
		      [u_initial, [rest, [u_ob_c36_pairs, u_template]]],
		      [u_ywhile, rest],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ and,
			    [u_ob_c63, [u_slots_value, [car, rest]]],
			    [u_ob_c36_literal_c63, [u_slots_value, [car, rest]]]
			  ],
			  [u_slots_value, [car, rest]],
			  
			  [ if,
			    [u_number_c63, u_depth],
			    
			    [ if,
			      [>, u_depth, 1],
			      
			      [ u_ob_c36_instantiate2_c33,
				[u_slots_value, [car, rest]],
				bindings,
				['-1+', u_depth],
				u_template,
				[u_slots_name, [car, rest]]
			      ],
			      [u_slots_value, [car, rest]]
			    ],
			    
			    [ u_ob_c36_instantiate2_c33,
			      [u_slots_value, [car, rest]],
			      bindings,
			      [],
			      u_template,
			      [u_slots_name, [car, rest]]
			    ]
			  ]
			],
			[setq, rest, [cdr, rest]]
		      ],
		      [u_yresult, u_template]
		    ]
		  ],
		  [u_else, u_template]
		]
	      ]).


% annotating U::OB$INSTANTIATE2! 
wl: arglist_info(u_ob_c36_instantiate2_c33,
		[u_template, bindings, u_depth, u_ob, u_slot_name],
		
		[ Template_Get44,
		  Bindings_Param,
		  Depth_Param,
		  Ob_Param,
		  Slot_name_Param
		],
		arginfo{ all:[u_template, bindings, u_depth, u_ob, u_slot_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_template, bindings, u_depth, u_ob, u_slot_name],
			 opt:0,
			 req:[u_template, bindings, u_depth, u_ob, u_slot_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$INSTANTIATE2! 
wl: init_args(exact_only, u_ob_c36_instantiate2_c33).


% annotating U::OB$INSTANTIATE2! 
f_u_ob_c36_instantiate2_c33(Template_Get44, Bindings_Param, Depth_Param, Ob_Param, Slot_name_Param, ElseResult48) :-
	Env=[bv(u_template, Template_Get44), bv(bindings, Bindings_Param), bv(u_depth, Depth_Param), bv(u_ob, Ob_Param), bv(u_slot_name, Slot_name_Param)],
	f_u_var_c63(u_template, IFTEST),
	(   IFTEST\==[]
	->  f_ext_assq([u_variable_name, u_template], [cdr, bindings], Found_Init),
	    LEnv=[[bv(u_found, Found_Init)]|Env],
	    get_var(LEnv, u_found, IFTEST25),
	    (   IFTEST25\==[]
	    ->  f_u_ob_c36_remove(Ob_Param,
				  Slot_name_Param,
				  Template_Get44,
				  C36_remove_Ret),
		get_var(LEnv, u_found, Found_Get34),
		cl_cadr(Found_Get34, Cadr_Ret),
		f_u_ob_c36_add(Ob_Param, Slot_name_Param, Cadr_Ret, C36_add_Ret),
		get_var(LEnv, u_found, Found_Get35),
		cl_cadr(Found_Get35, TrueResult),
		ElseResult48=TrueResult
	    ;   f_u_ndbg(u_xx_gate_dbg_xx,
			 u_ob_warn,
			 
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
				      #\('N'),
				      #\(o),
				      #\(' '),
				      #\(b),
				      #\(i),
				      #\(n),
				      #\(d),
				      #\(i),
				      #\(n),
				      #\(g),
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
				      #\(i),
				      #\(n),
				      #\(s),
				      #\(t),
				      #\(a),
				      #\(n),
				      #\(t),
				      #\(i),
				      #\(a),
				      #\(t),
				      #\(e),
				      #\('.'),
				      #\(~),
				      #\('%')
				    ]),
			   u_template
			 ],
			 Ndbg_Ret),
		ElseResult48=Template_Get44
	    )
	;   f_u_ob_c63(u_template, IFTEST39),
	    (   IFTEST39\==[]
	    ->  f_u_yloop(
			  [ [u_initial, [rest, [u_ob_c36_pairs, u_template]]],
			    [u_ywhile, rest],
			    
			    [ u_ydo,
			      
			      [ if,
				
				[ and,
				  [u_ob_c63, [u_slots_value, [car, rest]]],
				  
				  [ u_ob_c36_literal_c63,
				    [u_slots_value, [car, rest]]
				  ]
				],
				[u_slots_value, [car, rest]],
				
				[ if,
				  [u_number_c63, u_depth],
				  
				  [ if,
				    [>, u_depth, 1],
				    
				    [ u_ob_c36_instantiate2_c33,
				      [u_slots_value, [car, rest]],
				      bindings,
				      ['-1+', u_depth],
				      u_template,
				      [u_slots_name, [car, rest]]
				    ],
				    [u_slots_value, [car, rest]]
				  ],
				  
				  [ u_ob_c36_instantiate2_c33,
				    [u_slots_value, [car, rest]],
				    bindings,
				    [],
				    u_template,
				    [u_slots_name, [car, rest]]
				  ]
				]
			      ],
			      [setq, rest, [cdr, rest]]
			    ],
			    [u_yresult, u_template]
			  ],
			  TrueResult47),
		ElseResult48=TrueResult47
	    ;   get_var(Env, u_else, IFTEST41),
		(   IFTEST41\==[]
		->  ElseResult48=Template_Get44
		;   ElseResult48=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_instantiate2_c33, classof, claz_function),
   set_opv(u_ob_c36_instantiate2_c33, compile_as, kw_function),
   set_opv(u_ob_c36_instantiate2_c33, function, f_u_ob_c36_instantiate2_c33),
   DefunResult=u_ob_c36_instantiate2_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8506)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copies an ob down to the given depth. Does NOT replace variables",
				     1,
				     8508)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" with their values the way ob-instantiate does.",
				     1,
				     8575)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8624)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (coding assistance from Sergio Alvarado)",
				     1,
				     8626)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:7080 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8669)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8671 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$copy',
			    [self],
			    [setq, '*found-obs*', []],
			    ['copy-ob1', self, 1, [quote, ['top-context']]]
			  ]).

% annotating U::OB$COPY 
wl: lambda_def(defun,
	      u_ob_c36_copy,
	      f_u_ob_c36_copy,
	      [u_self],
	      
	      [ [setq, u_xx_found_obs_xx, []],
		[u_copy_ob1, u_self, 1, [quote, [u_top_context]]]
	      ]).


% annotating U::OB$COPY 
wl: arglist_info(u_ob_c36_copy,
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

% annotating U::OB$COPY 
wl: init_args(exact_only, u_ob_c36_copy).


% annotating U::OB$COPY 
f_u_ob_c36_copy(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	set_var(Env, setq, u_xx_found_obs_xx, []),
	f_u_copy_ob1(Self_Param, 1, [u_top_context], Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_ob_c36_copy, classof, claz_function),
   set_opv(u_ob_c36_copy, compile_as, kw_function),
   set_opv(u_ob_c36_copy, function, f_u_ob_c36_copy),
   DefunResult=u_ob_c36_copy.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8755 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$copy-deep',
			    [self],
			    [setq, '*found-obs*', []],
			    ['copy-ob1', self, 1000, []]
			  ]).

% annotating U::OB$COPY-DEEP 
wl: lambda_def(defun,
	      u_ob_c36_copy_deep,
	      f_u_ob_c36_copy_deep,
	      [u_self],
	      [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_self, 1000, []]]).


% annotating U::OB$COPY-DEEP 
wl: arglist_info(u_ob_c36_copy_deep,
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

% annotating U::OB$COPY-DEEP 
wl: init_args(exact_only, u_ob_c36_copy_deep).


% annotating U::OB$COPY-DEEP 
f_u_ob_c36_copy_deep(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	set_var(Env, setq, u_xx_found_obs_xx, []),
	f_u_copy_ob1(Self_Param, 1000, [], Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_ob_c36_copy_deep, classof, claz_function),
   set_opv(u_ob_c36_copy_deep, compile_as, kw_function),
   set_opv(u_ob_c36_copy_deep, function, f_u_ob_c36_copy_deep),
   DefunResult=u_ob_c36_copy_deep.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8836 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'copy-ob',
			    [template],
			    [setq, '*found-obs*', []],
			    ['copy-ob1', template, 1, []]
			  ]).

% annotating U::COPY-OB 
wl: lambda_def(defun,
	      u_copy_ob,
	      f_u_copy_ob,
	      [u_template],
	      [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_template, 1, []]]).


% annotating U::COPY-OB 
wl: arglist_info(u_copy_ob,
		[u_template],
		[Template_Param],
		arginfo{ all:[u_template],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_template],
			 opt:0,
			 req:[u_template],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::COPY-OB 
wl: init_args(exact_only, u_copy_ob).


% annotating U::COPY-OB 
f_u_copy_ob(Template_Param, FnResult) :-
	Env=[bv(u_template, Template_Param)],
	set_var(Env, setq, u_xx_found_obs_xx, []),
	f_u_copy_ob1(Template_Param, 1, [], Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_copy_ob, classof, claz_function),
   set_opv(u_copy_ob, compile_as, kw_function),
   set_opv(u_copy_ob, function, f_u_copy_ob),
   DefunResult=u_copy_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8917 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$copy-omit',
			    [ob, slots],
			    [setq, '*found-obs*', []],
			    ['copy-ob1', ob, 1, slots]
			  ]).

% annotating U::OB$COPY-OMIT 
wl: lambda_def(defun,
	      u_ob_c36_copy_omit,
	      f_u_ob_c36_copy_omit,
	      [u_ob, sys_slots],
	      [[setq, u_xx_found_obs_xx, []], [u_copy_ob1, u_ob, 1, sys_slots]]).


% annotating U::OB$COPY-OMIT 
wl: arglist_info(u_ob_c36_copy_omit,
		[u_ob, sys_slots],
		[Ob_Param, Slots_Param],
		arginfo{ all:[u_ob, sys_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, sys_slots],
			 opt:0,
			 req:[u_ob, sys_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$COPY-OMIT 
wl: init_args(exact_only, u_ob_c36_copy_omit).


% annotating U::OB$COPY-OMIT 
f_u_ob_c36_copy_omit(Ob_Param, Slots_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(sys_slots, Slots_Param)],
	set_var(Env, setq, u_xx_found_obs_xx, []),
	f_u_copy_ob1(Ob_Param, 1, Slots_Param, Copy_ob1_Ret),
	Copy_ob1_Ret=FnResult.
:- set_opv(f_u_ob_c36_copy_omit, classof, claz_function),
   set_opv(u_ob_c36_copy_omit, compile_as, kw_function),
   set_opv(u_ob_c36_copy_omit, function, f_u_ob_c36_copy_omit),
   DefunResult=u_ob_c36_copy_omit.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:8999 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'copy-ob1',
			    [template, depth, 'omit-slots'],
			    
			    [ cond,
			      [['var?', template], template],
			      
			      [ ['ob?', template],
				
				[ cond,
				  
				  [ 
				    [ let,
				      [[found, [assq, template, '*found-obs*']]],
				      [if, found, [cadr, found], []]
				    ]
				  ],
				  
				  [ else,
				    
				    [ yloop,
				      [initial, ['new-ob', ['ob$create-empty']]],
				      [yfor, sv, in, ['ob$pairs', template]],
				      
				      [ ydo,
					
					[ if,
					  
					  [ not,
					    
					    [ 'memq?',
					      ['slots-name', sv],
					      'omit-slots'
					    ]
					  ],
					  
					  [ 'ob$add',
					    'new-ob',
					    ['slots-name', sv],
					    
					    [ if,
					      
					      [ and,
						['ob?', ['slots-value', sv]],
						
						[ 'ob$literal?',
						  ['slots-value', sv]
						]
					      ],
					      ['slots-value', sv],
					      
					      [ if,
						['number?', depth],
						
						[ if,
						  [>, depth, 1],
						  
						  [ 'copy-ob1',
						    ['slots-value', sv],
						    ['-1+', depth],
						    'omit-slots'
						  ],
						  ['slots-value', sv]
						],
						
						[ 'copy-ob1',
						  ['slots-value', sv],
						  [],
						  'omit-slots'
						]
					      ]
					    ]
					  ]
					]
				      ],
				      
				      [ yresult,
					
					[ progn,
					  
					  [ push,
					    [list, template, 'new-ob'],
					    '*found-obs*'
					  ],
					  'new-ob'
					]
				      ]
				    ]
				  ]
				]
			      ],
			      [else, template]
			    ]
			  ]).

% annotating U::COPY-OB1 
wl: lambda_def(defun,
	      u_copy_ob1,
	      f_u_copy_ob1,
	      [u_template, u_depth, u_omit_slots],
	      
	      [ 
		[ cond,
		  [[u_var_c63, u_template], u_template],
		  
		  [ [u_ob_c63, u_template],
		    
		    [ cond,
		      
		      [ 
			[ let,
			  [[u_found, [ext_assq, u_template, u_xx_found_obs_xx]]],
			  [if, u_found, [cadr, u_found], []]
			]
		      ],
		      
		      [ u_else,
			
			[ u_yloop,
			  [u_initial, [u_new_ob, [u_ob_c36_create_empty]]],
			  [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_template]],
			  
			  [ u_ydo,
			    
			    [ if,
			      
			      [ not,
				[u_memq_c63, [u_slots_name, u_sv], u_omit_slots]
			      ],
			      
			      [ u_ob_c36_add,
				u_new_ob,
				[u_slots_name, u_sv],
				
				[ if,
				  
				  [ and,
				    [u_ob_c63, [u_slots_value, u_sv]],
				    
				    [ u_ob_c36_literal_c63,
				      [u_slots_value, u_sv]
				    ]
				  ],
				  [u_slots_value, u_sv],
				  
				  [ if,
				    [u_number_c63, u_depth],
				    
				    [ if,
				      [>, u_depth, 1],
				      
				      [ u_copy_ob1,
					[u_slots_value, u_sv],
					['-1+', u_depth],
					u_omit_slots
				      ],
				      [u_slots_value, u_sv]
				    ],
				    
				    [ u_copy_ob1,
				      [u_slots_value, u_sv],
				      [],
				      u_omit_slots
				    ]
				  ]
				]
			      ]
			    ]
			  ],
			  
			  [ u_yresult,
			    
			    [ progn,
			      
			      [ push,
				[list, u_template, u_new_ob],
				u_xx_found_obs_xx
			      ],
			      u_new_ob
			    ]
			  ]
			]
		      ]
		    ]
		  ],
		  [u_else, u_template]
		]
	      ]).


% annotating U::COPY-OB1 
wl: arglist_info(u_copy_ob1,
		[u_template, u_depth, u_omit_slots],
		[Template_Get41, Depth_Param, Omit_slots_Param],
		arginfo{ all:[u_template, u_depth, u_omit_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_template, u_depth, u_omit_slots],
			 opt:0,
			 req:[u_template, u_depth, u_omit_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::COPY-OB1 
wl: init_args(exact_only, u_copy_ob1).


% annotating U::COPY-OB1 
f_u_copy_ob1(Template_Get41, Depth_Param, Omit_slots_Param, ElseResult37) :-
	Env=[bv(u_template, Template_Get41), bv(u_depth, Depth_Param), bv(u_omit_slots, Omit_slots_Param)],
	f_u_var_c63(u_template, IFTEST),
	(   IFTEST\==[]
	->  ElseResult37=Template_Get41
	;   f_u_ob_c63(u_template, IFTEST19),
	    (   IFTEST19\==[]
	    ->  f_ext_assq(u_template, u_xx_found_obs_xx, Found_Init),
		LEnv=[[bv(u_found, Found_Init)]|Env],
		get_var(LEnv, u_found, IFTEST26),
		(   IFTEST26\==[]
		->  get_var(LEnv, u_found, Found_Get30),
		    cl_cadr(Found_Get30, TrueResult),
		    _125622=TrueResult
		;   _125622=[]
		),
		IFTEST21=_125622,
		(   IFTEST21\==[]
		->  ElseResult37=[]
		;   get_var(Env, u_else, IFTEST32),
		    (   IFTEST32\==[]
		    ->  f_u_yloop(
				  [ 
				    [ u_initial,
				      [u_new_ob, [u_ob_c36_create_empty]]
				    ],
				    
				    [ u_yfor,
				      u_sv,
				      u_in,
				      [u_ob_c36_pairs, u_template]
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ not,
					  
					  [ u_memq_c63,
					    [u_slots_name, u_sv],
					    u_omit_slots
					  ]
					],
					
					[ u_ob_c36_add,
					  u_new_ob,
					  [u_slots_name, u_sv],
					  
					  [ if,
					    
					    [ and,
					      [u_ob_c63, [u_slots_value, u_sv]],
					      
					      [ u_ob_c36_literal_c63,
						[u_slots_value, u_sv]
					      ]
					    ],
					    [u_slots_value, u_sv],
					    
					    [ if,
					      [u_number_c63, u_depth],
					      
					      [ if,
						[>, u_depth, 1],
						
						[ u_copy_ob1,
						  [u_slots_value, u_sv],
						  ['-1+', u_depth],
						  u_omit_slots
						],
						[u_slots_value, u_sv]
					      ],
					      
					      [ u_copy_ob1,
						[u_slots_value, u_sv],
						[],
						u_omit_slots
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ u_yresult,
				      
				      [ progn,
					
					[ push,
					  [list, u_template, u_new_ob],
					  u_xx_found_obs_xx
					],
					u_new_ob
				      ]
				    ]
				  ],
				  TrueResult35),
			ElseResult37=TrueResult35
		    ;   ElseResult37=[]
		    )
		)
	    ;   get_var(Env, u_else, IFTEST38),
		(   IFTEST38\==[]
		->  ElseResult37=Template_Get41
		;   ElseResult37=[]
		)
	    )
	).
:- set_opv(f_u_copy_ob1, classof, claz_function),
   set_opv(u_copy_ob1, compile_as, kw_function),
   set_opv(u_copy_ob1, function, f_u_copy_ob1),
   DefunResult=u_copy_ob1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:10254 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'vars-in?',
			    [ob],
			    [setq, '*found-vars*', []],
			    ['vars-in1?', ob]
			  ]).

% annotating U::VARS-IN? 
wl: lambda_def(defun,
	      u_vars_in_c63,
	      f_u_vars_in_c63,
	      [u_ob],
	      [[setq, u_xx_found_vars_xx, []], [u_vars_in1_c63, u_ob]]).


% annotating U::VARS-IN? 
wl: arglist_info(u_vars_in_c63,
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

% annotating U::VARS-IN? 
wl: init_args(exact_only, u_vars_in_c63).


% annotating U::VARS-IN? 
f_u_vars_in_c63(Ob_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param)],
	set_var(Env, setq, u_xx_found_vars_xx, []),
	f_u_vars_in1_c63(Ob_Param, In1_c63_Ret),
	In1_c63_Ret=FnResult.
:- set_opv(f_u_vars_in_c63, classof, claz_function),
   set_opv(u_vars_in_c63, compile_as, kw_function),
   set_opv(u_vars_in_c63, function, f_u_vars_in_c63),
   DefunResult=u_vars_in_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:10320 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ setq,
			    '*vars-in-ignores*',
			    
			    [ quote,
			      
			      [ 'linked-to',
				'linked-from',
				'linked-to-of',
				'linked-from-of',
				'analogical-episode',
				'main-motiv',
				'termination-context',
				'failure-context'
			      ]
			    ]
			  ]).
:- set_var(TLEnv3,
	   setq,
	   u_xx_vars_in_ignores_xx,
	   
	   [ u_linked_to,
	     u_linked_from,
	     u_linked_to_of,
	     u_linked_from_of,
	     u_analogical_episode,
	     u_main_motiv,
	     u_termination_context,
	     u_failure_context
	   ]).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:10495 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'vars-in1?',
			    [ob],
			    
			    [ if,
			      ['memq?', ob, '*found-vars*'],
			      [],
			      
			      [ progn,
				[setq, '*found-vars*', [cons, ob, '*found-vars*']],
				
				[ cond,
				  [[and, ['ob?', ob], ['ob$literal?', ob]], []],
				  
				  [ ['ob?', ob],
				    
				    [ yloop,
				      [initial, [result, []]],
				      [yfor, sv, in, ['ob$pairs', ob]],
				      [ywhile, [not, result]],
				      
				      [ ydo,
					
					[ if,
					  
					  [ and,
					    [not, ['cx?', ['slots-value', sv]]],
					    
					    [ not,
					      
					      [ 'memq?',
						['slots-name', sv],
						'*vars-in-ignores*'
					      ]
					    ],
					    
					    [ not,
					      
					      [ 'memq?',
						['slots-name', sv],
						'*permanent-ignore-slots*'
					      ]
					    ]
					  ],
					  
					  [ if,
					    
					    [ and,
					      ['var?', ['slots-value', sv]],
					      
					      [ not,
						
						[ 'memq?',
						  ['slots-value', sv],
						  result
						]
					      ]
					    ],
					    [setq, result, t],
					    
					    [ setq,
					      result,
					      ['vars-in1?', ['slots-value', sv]]
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

% annotating U::VARS-IN1? 
wl: lambda_def(defun,
	      u_vars_in1_c63,
	      f_u_vars_in1_c63,
	      [u_ob],
	      
	      [ 
		[ if,
		  [u_memq_c63, u_ob, u_xx_found_vars_xx],
		  [],
		  
		  [ progn,
		    [setq, u_xx_found_vars_xx, [cons, u_ob, u_xx_found_vars_xx]],
		    
		    [ cond,
		      [[and, [u_ob_c63, u_ob], [u_ob_c36_literal_c63, u_ob]], []],
		      
		      [ [u_ob_c63, u_ob],
			
			[ u_yloop,
			  [u_initial, [u_result, []]],
			  [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
			  [u_ywhile, [not, u_result]],
			  
			  [ u_ydo,
			    
			    [ if,
			      
			      [ and,
				[not, [u_cx_c63, [u_slots_value, u_sv]]],
				
				[ not,
				  
				  [ u_memq_c63,
				    [u_slots_name, u_sv],
				    u_xx_vars_in_ignores_xx
				  ]
				],
				
				[ not,
				  
				  [ u_memq_c63,
				    [u_slots_name, u_sv],
				    u_xx_permanent_ignore_slots_xx
				  ]
				]
			      ],
			      
			      [ if,
				
				[ and,
				  [u_var_c63, [u_slots_value, u_sv]],
				  
				  [ not,
				    [u_memq_c63, [u_slots_value, u_sv], u_result]
				  ]
				],
				[setq, u_result, t],
				
				[ setq,
				  u_result,
				  [u_vars_in1_c63, [u_slots_value, u_sv]]
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


% annotating U::VARS-IN1? 
wl: arglist_info(u_vars_in1_c63,
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

% annotating U::VARS-IN1? 
wl: init_args(exact_only, u_vars_in1_c63).


% annotating U::VARS-IN1? 
f_u_vars_in1_c63(Ob_Param, ElseResult29) :-
	Env=[bv(u_ob, Ob_Param)],
	f_u_memq_c63(u_ob, u_xx_found_vars_xx, IFTEST),
	(   IFTEST\==[]
	->  ElseResult29=[]
	;   get_var(Env, u_xx_found_vars_xx, Xx_found_vars_xx_Get),
	    Xx_found_vars_xx=[Ob_Param|Xx_found_vars_xx_Get],
	    set_var(Env, u_xx_found_vars_xx, Xx_found_vars_xx),
	    f_u_ob_c63(u_ob, IFTEST18),
	    (   IFTEST18\==[]
	    ->  f_u_ob_c36_literal_c63(Ob_Param, TrueResult),
		IFTEST16=TrueResult
	    ;   IFTEST16=[]
	    ),
	    (   IFTEST16\==[]
	    ->  ElseResult29=[]
	    ;   f_u_ob_c63(u_ob, IFTEST22),
		(   IFTEST22\==[]
		->  f_u_yloop(
			      [ [u_initial, [u_result, []]],
				[u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
				[u_ywhile, [not, u_result]],
				
				[ u_ydo,
				  
				  [ if,
				    
				    [ and,
				      [not, [u_cx_c63, [u_slots_value, u_sv]]],
				      
				      [ not,
					
					[ u_memq_c63,
					  [u_slots_name, u_sv],
					  u_xx_vars_in_ignores_xx
					]
				      ],
				      
				      [ not,
					
					[ u_memq_c63,
					  [u_slots_name, u_sv],
					  u_xx_permanent_ignore_slots_xx
					]
				      ]
				    ],
				    
				    [ if,
				      
				      [ and,
					[u_var_c63, [u_slots_value, u_sv]],
					
					[ not,
					  
					  [ u_memq_c63,
					    [u_slots_value, u_sv],
					    u_result
					  ]
					]
				      ],
				      [setq, u_result, t],
				      
				      [ setq,
					u_result,
					[u_vars_in1_c63, [u_slots_value, u_sv]]
				      ]
				    ]
				  ]
				],
				[u_yresult, u_result]
			      ],
			      TrueResult28),
		    ElseResult29=TrueResult28
		;   get_var(Env, u_else, IFTEST24),
		    (   IFTEST24\==[]
		    ->  ElseResult29=[]
		    ;   ElseResult29=[]
		    )
		)
	    )
	).
:- set_opv(f_u_vars_in1_c63, classof, claz_function),
   set_opv(u_vars_in1_c63, compile_as, kw_function),
   set_opv(u_vars_in1_c63, function, f_u_vars_in1_c63),
   DefunResult=u_vars_in1_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:11414 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*found-vars*', []]).
:- set_var(TLEnv3, setq, u_xx_found_vars_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:11439 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'variables-in',
			    [ob, 'omit-slots'],
			    [setq, '*found-vars*', []],
			    ['variables-in1', ob, 'omit-slots']
			  ]).

% annotating U::VARIABLES-IN 
wl: lambda_def(defun,
	      u_variables_in,
	      f_u_variables_in,
	      [u_ob, u_omit_slots],
	      
	      [ [setq, u_xx_found_vars_xx, []],
		[u_variables_in1, u_ob, u_omit_slots]
	      ]).


% annotating U::VARIABLES-IN 
wl: arglist_info(u_variables_in,
		[u_ob, u_omit_slots],
		[Ob_Param, Omit_slots_Param],
		arginfo{ all:[u_ob, u_omit_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_omit_slots],
			 opt:0,
			 req:[u_ob, u_omit_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VARIABLES-IN 
wl: init_args(exact_only, u_variables_in).


% annotating U::VARIABLES-IN 
f_u_variables_in(Ob_Param, Omit_slots_Param, FnResult) :-
	Env=[bv(u_ob, Ob_Param), bv(u_omit_slots, Omit_slots_Param)],
	set_var(Env, setq, u_xx_found_vars_xx, []),
	f_u_variables_in1(Ob_Param, Omit_slots_Param, Variables_in1_Ret),
	Variables_in1_Ret=FnResult.
:- set_opv(f_u_variables_in, classof, claz_function),
   set_opv(u_variables_in, compile_as, kw_function),
   set_opv(u_variables_in, function, f_u_variables_in),
   DefunResult=u_variables_in.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:11535 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'variables-in1',
			    [ob, 'omit-slots'],
			    
			    [ if,
			      ['memq?', ob, '*found-vars*'],
			      [],
			      
			      [ progn,
				[setq, '*found-vars*', [cons, ob, '*found-vars*']],
				
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
					      ['var?', ['slots-value', sv]],
					      
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
						
						[ 'variables-in1',
						  ['slots-value', sv],
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

% annotating U::VARIABLES-IN1 
wl: lambda_def(defun,
	      u_variables_in1,
	      f_u_variables_in1,
	      [u_ob, u_omit_slots],
	      
	      [ 
		[ if,
		  [u_memq_c63, u_ob, u_xx_found_vars_xx],
		  [],
		  
		  [ progn,
		    [setq, u_xx_found_vars_xx, [cons, u_ob, u_xx_found_vars_xx]],
		    
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
				  [u_var_c63, [u_slots_value, u_sv]],
				  
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
				    
				    [ u_variables_in1,
				      [u_slots_value, u_sv],
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


% annotating U::VARIABLES-IN1 
wl: arglist_info(u_variables_in1,
		[u_ob, u_omit_slots],
		[Ob_Param, Omit_slots_Param],
		arginfo{ all:[u_ob, u_omit_slots],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_omit_slots],
			 opt:0,
			 req:[u_ob, u_omit_slots],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::VARIABLES-IN1 
wl: init_args(exact_only, u_variables_in1).


% annotating U::VARIABLES-IN1 
f_u_variables_in1(Ob_Param, Omit_slots_Param, ElseResult31) :-
	Env=[bv(u_ob, Ob_Param), bv(u_omit_slots, Omit_slots_Param)],
	f_u_memq_c63(u_ob, u_xx_found_vars_xx, IFTEST),
	(   IFTEST\==[]
	->  ElseResult31=[]
	;   get_var(Env, u_xx_found_vars_xx, Xx_found_vars_xx_Get),
	    Xx_found_vars_xx=[Ob_Param|Xx_found_vars_xx_Get],
	    set_var(Env, u_xx_found_vars_xx, Xx_found_vars_xx),
	    f_u_ob_c63(u_ob, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_ob_c36_literal_c63(Ob_Param, TrueResult),
		IFTEST18=TrueResult
	    ;   IFTEST18=[]
	    ),
	    (   IFTEST18\==[]
	    ->  ElseResult31=[]
	    ;   f_u_ob_c63(u_ob, IFTEST24),
		(   IFTEST24\==[]
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
					[u_var_c63, [u_slots_value, u_sv]],
					
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
					  
					  [ u_variables_in1,
					    [u_slots_value, u_sv],
					    u_omit_slots
					  ]
					]
				      ]
				    ]
				  ]
				],
				[u_yresult, u_result]
			      ],
			      TrueResult30),
		    ElseResult31=TrueResult30
		;   get_var(Env, u_else, IFTEST26),
		    (   IFTEST26\==[]
		    ->  ElseResult31=[]
		    ;   ElseResult31=[]
		    )
		)
	    )
	).
:- set_opv(f_u_variables_in1, classof, claz_function),
   set_opv(u_variables_in1, compile_as, kw_function),
   set_opv(u_variables_in1, function, f_u_variables_in1),
   DefunResult=u_variables_in1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_instan.cl:11535 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 12348)).
:- true.


% Total time: 4.269 seconds


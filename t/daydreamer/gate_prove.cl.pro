
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_prove" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:18 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This file contains the OB theorem prover",
				     1,
				     202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:244 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 245)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:246 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  2/23/87: First version written", 1, 247)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:280 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 281)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:282 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     283)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:363 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*prules*', []]).
:- set_var(TLEnv3, setq, u_xx_prules_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:385 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$add-prule',
			    [prule],
			    [setq, '*prules*', [cons, prule, '*prules*']]
			  ]).

% annotating U::OB$ADD-PRULE 
wl: lambda_def(defun,
	      u_ob_c36_add_prule,
	      f_u_ob_c36_add_prule,
	      [u_prule],
	      [[setq, u_xx_prules_xx, [cons, u_prule, u_xx_prules_xx]]]).


% annotating U::OB$ADD-PRULE 
wl: arglist_info(u_ob_c36_add_prule,
		[u_prule],
		[Prule_Param],
		arginfo{ all:[u_prule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_prule],
			 opt:0,
			 req:[u_prule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$ADD-PRULE 
wl: init_args(exact_only, u_ob_c36_add_prule).


% annotating U::OB$ADD-PRULE 
f_u_ob_c36_add_prule(Prule_Param, FnResult) :-
	Env=[bv(u_prule, Prule_Param)],
	get_var(Env, u_xx_prules_xx, Xx_prules_xx_Get),
	Xx_prules_xx=[Prule_Param|Xx_prules_xx_Get],
	set_var(Env, u_xx_prules_xx, Xx_prules_xx),
	Xx_prules_xx=FnResult.
:- set_opv(f_u_ob_c36_add_prule, classof, claz_function),
   set_opv(u_ob_c36_add_prule, compile_as, kw_function),
   set_opv(u_ob_c36_add_prule, function, f_u_ob_c36_add_prule),
   DefunResult=u_ob_c36_add_prule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$remove-prule',
			    [prule],
			    [setq, '*prules*', ['delq!', prule, '*prules*']]
			  ]).

% annotating U::OB$REMOVE-PRULE 
wl: lambda_def(defun,
	      u_ob_c36_remove_prule,
	      f_u_ob_c36_remove_prule,
	      [u_prule],
	      [[setq, u_xx_prules_xx, [u_delq_c33, u_prule, u_xx_prules_xx]]]).


% annotating U::OB$REMOVE-PRULE 
wl: arglist_info(u_ob_c36_remove_prule,
		[u_prule],
		[Prule_Param],
		arginfo{ all:[u_prule],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_prule],
			 opt:0,
			 req:[u_prule],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$REMOVE-PRULE 
wl: init_args(exact_only, u_ob_c36_remove_prule).


% annotating U::OB$REMOVE-PRULE 
f_u_ob_c36_remove_prule(Prule_Param, FnResult) :-
	Env=[bv(u_prule, Prule_Param)],
	f_u_delq_c33(u_prule, u_xx_prules_xx, Xx_prules_xx),
	set_var(Env, u_xx_prules_xx, Xx_prules_xx),
	Xx_prules_xx=FnResult.
:- set_opv(f_u_ob_c36_remove_prule, classof, claz_function),
   set_opv(u_ob_c36_remove_prule, compile_as, kw_function),
   set_opv(u_ob_c36_remove_prule, function, f_u_ob_c36_remove_prule),
   DefunResult=u_ob_c36_remove_prule.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     530)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 611)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" ob$prove:", 1, 613)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 625)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" pattern - concept, possibly containing variables (i.e., a query), to prove",
				     1,
				     627)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" bd - binding list with respect to which the proof is to be performed",
				     1,
				     704)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" max-number - the maximum number of solutions that are to be generated",
				     1,
				     775)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 847)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" ob$prove1:", 1, 849)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 862)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" pfacts - context containing the 'facts' which may be used in the proof",
				     1,
				     864)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" prules - list of obs which are the 'prules' which may be used in the proof",
				     1,
				     937)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ignore-slots - list of slots to ignore",
				     1,
				     1014)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1055)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Sample rules demonstrating the use of ROR, RAND, and RNOT:",
				     1,
				     1057)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1118)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$fcreate '(PRULE subgoal (ROR obj (PTRANS actor ?Person to ?Location)",
				     1,
				     1120)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                                  obj (LIVES-IN actor ?Person loc ?Location))",
				     1,
				     1195)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("                     goal (PROX actor ?Person loc ?Location)))",
				     1,
				     1274)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1338)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" A solution is an augmented binding list. Since web-prove can generate",
				     1,
				     1340)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" several solutions, the result is a list of augmented binding lists. Thus,",
				     1,
				     1412)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ob$prove returns either:", 1, 1488)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1515)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 1) NIL if con cannot be proved", 1, 1517)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 2) list of augmented binding lists if con can be proved",
				     1,
				     1550)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1608)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Still to do:", 1, 1610)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Add negation", 1, 1625)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1640)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1642)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:1723 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*proof-failures*', []]).
:- set_var(TLEnv3, setq, u_xx_proof_failures_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:1752 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$prove',
			    [pattern, bd, 'max-number'],
			    
			    [ 'ob$prove1',
			      pattern,
			      bd,
			      'max-number',
			      '*prules*',
			      '*pfacts*',
			      []
			    ]
			  ]).

% annotating U::OB$PROVE 
wl: lambda_def(defun,
	      u_ob_c36_prove,
	      f_u_ob_c36_prove,
	      [u_pattern, u_bd, u_max_number],
	      
	      [ 
		[ u_ob_c36_prove1,
		  u_pattern,
		  u_bd,
		  u_max_number,
		  u_xx_prules_xx,
		  u_xx_pfacts_xx,
		  []
		]
	      ]).


% annotating U::OB$PROVE 
wl: arglist_info(u_ob_c36_prove,
		[u_pattern, u_bd, u_max_number],
		[Pattern_Param, Bd_Param, Max_number_Param],
		arginfo{ all:[u_pattern, u_bd, u_max_number],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_pattern, u_bd, u_max_number],
			 opt:0,
			 req:[u_pattern, u_bd, u_max_number],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PROVE 
wl: init_args(exact_only, u_ob_c36_prove).


% annotating U::OB$PROVE 
f_u_ob_c36_prove(Pattern_Param, Bd_Param, Max_number_Param, FnResult) :-
	Env=[bv(u_pattern, Pattern_Param), bv(u_bd, Bd_Param), bv(u_max_number, Max_number_Param)],
	get_var(Env, u_xx_pfacts_xx, Xx_pfacts_xx_Get),
	get_var(Env, u_xx_prules_xx, Xx_prules_xx_Get),
	f_u_ob_c36_prove1(Pattern_Param,
			  Bd_Param,
			  Max_number_Param,
			  Xx_prules_xx_Get,
			  Xx_pfacts_xx_Get,
			  [],
			  C36_prove1_Ret),
	C36_prove1_Ret=FnResult.
:- set_opv(f_u_ob_c36_prove, classof, claz_function),
   set_opv(u_ob_c36_prove, compile_as, kw_function),
   set_opv(u_ob_c36_prove, function, f_u_ob_c36_prove),
   DefunResult=u_ob_c36_prove.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:1852 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$prove1',
			    
			    [ pattern,
			      bd,
			      'max-number',
			      prules,
			      pfacts,
			      'ignore-slots'
			    ],
			    [setq, '*proof-failures*', []],
			    
			    [ 'ob$prove2',
			      pattern,
			      bd,
			      'max-number',
			      prules,
			      pfacts,
			      'ignore-slots'
			    ]
			  ]).

% annotating U::OB$PROVE1 
wl: lambda_def(defun,
	      u_ob_c36_prove1,
	      f_u_ob_c36_prove1,
	      [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots],
	      
	      [ [setq, u_xx_proof_failures_xx, []],
		
		[ u_ob_c36_prove2,
		  u_pattern,
		  u_bd,
		  u_max_number,
		  u_prules,
		  u_pfacts,
		  u_ignore_slots
		]
	      ]).


% annotating U::OB$PROVE1 
wl: arglist_info(u_ob_c36_prove1,
		[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots],
		
		[ Pattern_Param,
		  Bd_Param,
		  Max_number_Param,
		  Prules_Param,
		  Pfacts_Param,
		  Ignore_slots_Param
		],
		arginfo{ all:
			     [ u_pattern,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_pattern,
				 u_bd,
				 u_max_number,
				 u_prules,
				 u_pfacts,
				 u_ignore_slots
			       ],
			 opt:0,
			 req:
			     [ u_pattern,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PROVE1 
wl: init_args(exact_only, u_ob_c36_prove1).


% annotating U::OB$PROVE1 
f_u_ob_c36_prove1(Pattern_Param, Bd_Param, Max_number_Param, Prules_Param, Pfacts_Param, Ignore_slots_Param, FnResult) :-
	Env=[bv(u_pattern, Pattern_Param), bv(u_bd, Bd_Param), bv(u_max_number, Max_number_Param), bv(u_prules, Prules_Param), bv(u_pfacts, Pfacts_Param), bv(u_ignore_slots, Ignore_slots_Param)],
	set_var(Env, setq, u_xx_proof_failures_xx, []),
	f_u_ob_c36_prove2(Pattern_Param,
			  Bd_Param,
			  Max_number_Param,
			  Prules_Param,
			  Pfacts_Param,
			  Ignore_slots_Param,
			  C36_prove2_Ret),
	C36_prove2_Ret=FnResult.
:- set_opv(f_u_ob_c36_prove1, classof, claz_function),
   set_opv(u_ob_c36_prove1, compile_as, kw_function),
   set_opv(u_ob_c36_prove1, function, f_u_ob_c36_prove1),
   DefunResult=u_ob_c36_prove1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:2015 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$prove2',
			    
			    [ pattern,
			      bd,
			      'max-number',
			      prules,
			      pfacts,
			      'ignore-slots'
			    ],
			    
			    [ cond,
			      
			      [ ['ty$instance?', pattern, [quote, rand]],
				
				[ 'ob$prove-all',
				  ['ob$gets', pattern, [quote, obj]],
				  bd,
				  'max-number',
				  prules,
				  pfacts,
				  'ignore-slots'
				]
			      ],
			      
			      [ ['ty$instance?', pattern, [quote, ror]],
				
				[ 'ob$prove-any',
				  ['ob$gets', pattern, [quote, obj]],
				  bd,
				  'max-number',
				  prules,
				  pfacts,
				  'ignore-slots'
				]
			      ],
			      
			      [ ['ty$instance?', pattern, [quote, rnot]],
				
				[ if,
				  
				  [ 'ob$prove2',
				    ['ob$get', pattern, [quote, obj]],
				    bd,
				    'max-number',
				    prules,
				    pfacts,
				    'ignore-slots'
				  ],
				  [],
				  bd
				]
			      ],
			      
			      [ else,
				
				[ yloop,
				  
				  [ initial,
				    
				    [ result,
				      
				      [ map,
					[quote, list],
					[lambda, [elem], [cons, [], [cdr, elem]]],
					['cx$retrieve-bd', pfacts, pattern, bd]
				      ]
				    ],
				    ['new-bd', []],
				    [result1, []]
				  ],
				  [yfor, prule, in, prules],
				  [ywhile, [<, [length, result], 'max-number']],
				  
				  [ ydo,
				    
				    [ if,
				      
				      [ setq,
					'new-bd',
					
					[ 'ob$unify1',
					  ['ob$get', prule, [quote, goal]],
					  pattern,
					  bd,
					  'ignore-slots'
					]
				      ],
				      
				      [ progn,
					
					[ if,
					  
					  [ setq,
					    result1,
					    
					    [ 'ob$prove2',
					      
					      [ 'ob$instantiate',
						
						[ 'ob$get',
						  prule,
						  [quote, subgoal]
						],
						'new-bd'
					      ],
					      bd,
					      'max-number',
					      prules,
					      pfacts,
					      'ignore-slots'
					    ]
					  ],
					  
					  [ setq,
					    result,
					    
					    [ 'append!',
					      result,
					      
					      [ map,
						[quote, list],
						
						[ lambda,
						  [elem],
						  [cons, t, [cdr, elem]]
						],
						result1
					      ]
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ yresult,
				    
				    [ if,
				      
				      [ and,
					['null?', result],
					
					[ not,
					  ['memq?', pattern, '*proof-failures*']
					]
				      ],
				      
				      [ setq,
					'*proof-failures*',
					
					[ cons,
					  [list, pattern, bd],
					  '*proof-failures*'
					]
				      ]
				    ],
				    result
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$PROVE2 
wl: lambda_def(defun,
	      u_ob_c36_prove2,
	      f_u_ob_c36_prove2,
	      [u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots],
	      
	      [ 
		[ cond,
		  
		  [ [u_ty_c36_instance_c63, u_pattern, [quote, u_rand]],
		    
		    [ u_ob_c36_prove_all,
		      [u_ob_c36_gets, u_pattern, [quote, u_obj]],
		      u_bd,
		      u_max_number,
		      u_prules,
		      u_pfacts,
		      u_ignore_slots
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_pattern, [quote, u_ror]],
		    
		    [ u_ob_c36_prove_any,
		      [u_ob_c36_gets, u_pattern, [quote, u_obj]],
		      u_bd,
		      u_max_number,
		      u_prules,
		      u_pfacts,
		      u_ignore_slots
		    ]
		  ],
		  
		  [ [u_ty_c36_instance_c63, u_pattern, [quote, u_rnot]],
		    
		    [ if,
		      
		      [ u_ob_c36_prove2,
			[u_ob_c36_get, u_pattern, [quote, u_obj]],
			u_bd,
			u_max_number,
			u_prules,
			u_pfacts,
			u_ignore_slots
		      ],
		      [],
		      u_bd
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ u_yloop,
		      
		      [ u_initial,
			
			[ u_result,
			  
			  [ map,
			    [quote, list],
			    [lambda, [u_elem], [cons, [], [cdr, u_elem]]],
			    [u_cx_c36_retrieve_bd, u_pfacts, u_pattern, u_bd]
			  ]
			],
			[u_new_bd, []],
			[u_result1, []]
		      ],
		      [u_yfor, u_prule, u_in, u_prules],
		      [u_ywhile, [<, [length, u_result], u_max_number]],
		      
		      [ u_ydo,
			
			[ if,
			  
			  [ setq,
			    u_new_bd,
			    
			    [ u_ob_c36_unify1,
			      [u_ob_c36_get, u_prule, [quote, u_goal]],
			      u_pattern,
			      u_bd,
			      u_ignore_slots
			    ]
			  ],
			  
			  [ progn,
			    
			    [ if,
			      
			      [ setq,
				u_result1,
				
				[ u_ob_c36_prove2,
				  
				  [ u_ob_c36_instantiate,
				    [u_ob_c36_get, u_prule, [quote, u_subgoal]],
				    u_new_bd
				  ],
				  u_bd,
				  u_max_number,
				  u_prules,
				  u_pfacts,
				  u_ignore_slots
				]
			      ],
			      
			      [ setq,
				u_result,
				
				[ u_append_c33,
				  u_result,
				  
				  [ map,
				    [quote, list],
				    [lambda, [u_elem], [cons, t, [cdr, u_elem]]],
				    u_result1
				  ]
				]
			      ]
			    ]
			  ]
			]
		      ],
		      
		      [ u_yresult,
			
			[ if,
			  
			  [ and,
			    [u_null_c63, u_result],
			    
			    [ not,
			      [u_memq_c63, u_pattern, u_xx_proof_failures_xx]
			    ]
			  ],
			  
			  [ setq,
			    u_xx_proof_failures_xx,
			    
			    [ cons,
			      [list, u_pattern, u_bd],
			      u_xx_proof_failures_xx
			    ]
			  ]
			],
			u_result
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$PROVE2 
wl: arglist_info(u_ob_c36_prove2,
		[u_pattern, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots],
		
		[ Pattern_Param,
		  Bd_Param,
		  Max_number_Param,
		  Prules_Param,
		  Pfacts_Param,
		  Ignore_slots_Param
		],
		arginfo{ all:
			     [ u_pattern,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_pattern,
				 u_bd,
				 u_max_number,
				 u_prules,
				 u_pfacts,
				 u_ignore_slots
			       ],
			 opt:0,
			 req:
			     [ u_pattern,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PROVE2 
wl: init_args(exact_only, u_ob_c36_prove2).


% annotating U::OB$PROVE2 
f_u_ob_c36_prove2(Pattern_Param, Bd_Param, Max_number_Param, Prules_Param, Pfacts_Param, Ignore_slots_Param, TrueResult58) :-
	Env=[bv(u_pattern, Pattern_Param), bv(u_bd, Bd_Param), bv(u_max_number, Max_number_Param), bv(u_prules, Prules_Param), bv(u_pfacts, Pfacts_Param), bv(u_ignore_slots, Ignore_slots_Param)],
	f_u_ty_c36_instance_c63(Pattern_Param, u_rand, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_gets(Pattern_Param, u_obj, Obj),
	    f_u_ob_c36_prove_all(Obj,
				 Bd_Param,
				 Max_number_Param,
				 Prules_Param,
				 Pfacts_Param,
				 Ignore_slots_Param,
				 TrueResult62),
	    TrueResult58=TrueResult62
	;   f_u_ty_c36_instance_c63(Pattern_Param, u_ror, IFTEST31),
	    (   IFTEST31\==[]
	    ->  f_u_ob_c36_gets(Pattern_Param, u_obj, Obj67),
		f_u_ob_c36_prove_any(Obj67,
				     Bd_Param,
				     Max_number_Param,
				     Prules_Param,
				     Pfacts_Param,
				     Ignore_slots_Param,
				     TrueResult60),
		TrueResult58=TrueResult60
	    ;   f_u_ty_c36_instance_c63(Pattern_Param, u_rnot, IFTEST40),
		(   IFTEST40\==[]
		->  f_u_ob_c36_get(Pattern_Param, u_obj, Obj68),
		    f_u_ob_c36_prove2(Obj68,
				      Bd_Param,
				      Max_number_Param,
				      Prules_Param,
				      Pfacts_Param,
				      Ignore_slots_Param,
				      IFTEST43),
		    (   IFTEST43\==[]
		    ->  TrueResult58=[]
		    ;   TrueResult58=Bd_Param
		    )
		;   get_var(Env, u_else, IFTEST53),
		    (   IFTEST53\==[]
		    ->  f_u_yloop(
				  [ 
				    [ u_initial,
				      
				      [ u_result,
					
					[ map,
					  [quote, list],
					  
					  [ lambda,
					    [u_elem],
					    [cons, [], [cdr, u_elem]]
					  ],
					  
					  [ u_cx_c36_retrieve_bd,
					    u_pfacts,
					    u_pattern,
					    u_bd
					  ]
					]
				      ],
				      [u_new_bd, []],
				      [u_result1, []]
				    ],
				    [u_yfor, u_prule, u_in, u_prules],
				    
				    [ u_ywhile,
				      [<, [length, u_result], u_max_number]
				    ],
				    
				    [ u_ydo,
				      
				      [ if,
					
					[ setq,
					  u_new_bd,
					  
					  [ u_ob_c36_unify1,
					    
					    [ u_ob_c36_get,
					      u_prule,
					      [quote, u_goal]
					    ],
					    u_pattern,
					    u_bd,
					    u_ignore_slots
					  ]
					],
					
					[ progn,
					  
					  [ if,
					    
					    [ setq,
					      u_result1,
					      
					      [ u_ob_c36_prove2,
						
						[ u_ob_c36_instantiate,
						  
						  [ u_ob_c36_get,
						    u_prule,
						    [quote, u_subgoal]
						  ],
						  u_new_bd
						],
						u_bd,
						u_max_number,
						u_prules,
						u_pfacts,
						u_ignore_slots
					      ]
					    ],
					    
					    [ setq,
					      u_result,
					      
					      [ u_append_c33,
						u_result,
						
						[ map,
						  [quote, list],
						  
						  [ lambda,
						    [u_elem],
						    [cons, t, [cdr, u_elem]]
						  ],
						  u_result1
						]
					      ]
					    ]
					  ]
					]
				      ]
				    ],
				    
				    [ u_yresult,
				      
				      [ if,
					
					[ and,
					  [u_null_c63, u_result],
					  
					  [ not,
					    
					    [ u_memq_c63,
					      u_pattern,
					      u_xx_proof_failures_xx
					    ]
					  ]
					],
					
					[ setq,
					  u_xx_proof_failures_xx,
					  
					  [ cons,
					    [list, u_pattern, u_bd],
					    u_xx_proof_failures_xx
					  ]
					]
				      ],
				      u_result
				    ]
				  ],
				  TrueResult),
			TrueResult58=TrueResult
		    ;   TrueResult58=[]
		    )
		)
	    )
	).
:- set_opv(f_u_ob_c36_prove2, classof, claz_function),
   set_opv(u_ob_c36_prove2, compile_as, kw_function),
   set_opv(u_ob_c36_prove2, function, f_u_ob_c36_prove2),
   DefunResult=u_ob_c36_prove2.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:2015 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(cons prule (car elem))", 48, 3342)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:3657 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$prove-all',
			    
			    [ 'prove-obs',
			      bd,
			      'max-number',
			      prules,
			      pfacts,
			      'ignore-slots'
			    ],
			    
			    [ let,
			      
			      [ 
				[ 'bd-list',
				  
				  [ 'ob$prove2',
				    [car, 'prove-obs'],
				    bd,
				    'max-number',
				    prules,
				    pfacts,
				    'ignore-slots'
				  ]
				]
			      ],
			      
			      [ if,
				['null?', [cdr, 'prove-obs']],
				'bd-list',
				
				[ yloop,
				  [yfor, bd, in, 'bd-list'],
				  [initial, [result, []]],
				  
				  [ ydo,
				    
				    [ setq,
				      result,
				      
				      [ 'append!',
					result,
					
					[ 'ob$prove-all',
					  [cdr, 'prove-obs'],
					  bd,
					  'max-number',
					  prules,
					  pfacts,
					  'ignore-slots'
					]
				      ]
				    ]
				  ],
				  [yresult, result]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$PROVE-ALL 
wl: lambda_def(defun,
	      u_ob_c36_prove_all,
	      f_u_ob_c36_prove_all,
	      [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_bd_list,
		      
		      [ u_ob_c36_prove2,
			[car, u_prove_obs],
			u_bd,
			u_max_number,
			u_prules,
			u_pfacts,
			u_ignore_slots
		      ]
		    ]
		  ],
		  
		  [ if,
		    [u_null_c63, [cdr, u_prove_obs]],
		    u_bd_list,
		    
		    [ u_yloop,
		      [u_yfor, u_bd, u_in, u_bd_list],
		      [u_initial, [u_result, []]],
		      
		      [ u_ydo,
			
			[ setq,
			  u_result,
			  
			  [ u_append_c33,
			    u_result,
			    
			    [ u_ob_c36_prove_all,
			      [cdr, u_prove_obs],
			      u_bd,
			      u_max_number,
			      u_prules,
			      u_pfacts,
			      u_ignore_slots
			    ]
			  ]
			]
		      ],
		      [u_yresult, u_result]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$PROVE-ALL 
wl: arglist_info(u_ob_c36_prove_all,
		
		[ u_prove_obs,
		  u_bd,
		  u_max_number,
		  u_prules,
		  u_pfacts,
		  u_ignore_slots
		],
		
		[ Prove_obs_Param,
		  Bd_Param,
		  Max_number_Param,
		  Prules_Param,
		  Pfacts_Param,
		  Ignore_slots_Param
		],
		arginfo{ all:
			     [ u_prove_obs,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_prove_obs,
				 u_bd,
				 u_max_number,
				 u_prules,
				 u_pfacts,
				 u_ignore_slots
			       ],
			 opt:0,
			 req:
			     [ u_prove_obs,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PROVE-ALL 
wl: init_args(exact_only, u_ob_c36_prove_all).


% annotating U::OB$PROVE-ALL 
f_u_ob_c36_prove_all(Prove_obs_Param, Bd_Param, Max_number_Param, Prules_Param, Pfacts_Param, Ignore_slots_Param, FnResult) :-
	Env=[bv(u_prove_obs, Prove_obs_Param), bv(u_bd, Bd_Param), bv(u_max_number, Max_number_Param), bv(u_prules, Prules_Param), bv(u_pfacts, Pfacts_Param), bv(u_ignore_slots, Ignore_slots_Param)],
	cl_car(Prove_obs_Param, C36_prove2_Param),
	f_u_ob_c36_prove2(C36_prove2_Param,
			  Bd_Param,
			  Max_number_Param,
			  Prules_Param,
			  Pfacts_Param,
			  Ignore_slots_Param,
			  Bd_list_Init),
	LEnv=[[bv(u_bd_list, Bd_list_Init)]|Env],
	f_u_null_c63([cdr, u_prove_obs], IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_bd_list, Bd_list_Get),
	    FnResult=Bd_list_Get
	;   f_u_yloop(
		      [ [u_yfor, u_bd, u_in, u_bd_list],
			[u_initial, [u_result, []]],
			
			[ u_ydo,
			  
			  [ setq,
			    u_result,
			    
			    [ u_append_c33,
			      u_result,
			      
			      [ u_ob_c36_prove_all,
				[cdr, u_prove_obs],
				u_bd,
				u_max_number,
				u_prules,
				u_pfacts,
				u_ignore_slots
			      ]
			    ]
			  ]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ob_c36_prove_all, classof, claz_function),
   set_opv(u_ob_c36_prove_all, compile_as, kw_function),
   set_opv(u_ob_c36_prove_all, function, f_u_ob_c36_prove_all),
   DefunResult=u_ob_c36_prove_all.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:4259 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$prove-any',
			    
			    [ 'prove-obs',
			      bd,
			      'max-number',
			      prules,
			      pfacts,
			      'ignore-slots'
			    ],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, elem, in, 'prove-obs'],
			      [yuntil, result],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  
				  [ 'ob$prove2',
				    elem,
				    bd,
				    'max-number',
				    prules,
				    pfacts,
				    'ignore-slots'
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::OB$PROVE-ANY 
wl: lambda_def(defun,
	      u_ob_c36_prove_any,
	      f_u_ob_c36_prove_any,
	      [u_prove_obs, u_bd, u_max_number, u_prules, u_pfacts, u_ignore_slots],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_elem, u_in, u_prove_obs],
		  [u_yuntil, u_result],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      
		      [ u_ob_c36_prove2,
			u_elem,
			u_bd,
			u_max_number,
			u_prules,
			u_pfacts,
			u_ignore_slots
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::OB$PROVE-ANY 
wl: arglist_info(u_ob_c36_prove_any,
		
		[ u_prove_obs,
		  u_bd,
		  u_max_number,
		  u_prules,
		  u_pfacts,
		  u_ignore_slots
		],
		
		[ Prove_obs_Param,
		  Bd_Param,
		  Max_number_Param,
		  Prules_Param,
		  Pfacts_Param,
		  Ignore_slots_Param
		],
		arginfo{ all:
			     [ u_prove_obs,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:
			       [ u_prove_obs,
				 u_bd,
				 u_max_number,
				 u_prules,
				 u_pfacts,
				 u_ignore_slots
			       ],
			 opt:0,
			 req:
			     [ u_prove_obs,
			       u_bd,
			       u_max_number,
			       u_prules,
			       u_pfacts,
			       u_ignore_slots
			     ],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PROVE-ANY 
wl: init_args(exact_only, u_ob_c36_prove_any).


% annotating U::OB$PROVE-ANY 
f_u_ob_c36_prove_any(Prove_obs_Param, Bd_Param, Max_number_Param, Prules_Param, Pfacts_Param, Ignore_slots_Param, FnResult) :-
	Env=[bv(u_prove_obs, Prove_obs_Param), bv(u_bd, Bd_Param), bv(u_max_number, Max_number_Param), bv(u_prules, Prules_Param), bv(u_pfacts, Pfacts_Param), bv(u_ignore_slots, Ignore_slots_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_elem, u_in, u_prove_obs],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			
			[ u_ob_c36_prove2,
			  u_elem,
			  u_bd,
			  u_max_number,
			  u_prules,
			  u_pfacts,
			  u_ignore_slots
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_prove_any, classof, claz_function),
   set_opv(u_ob_c36_prove_any, compile_as, kw_function),
   set_opv(u_ob_c36_prove_any, function, f_u_ob_c36_prove_any),
   DefunResult=u_ob_c36_prove_any.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_prove.cl:4259 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 4523)).
:- true.


% Total time: 1.364 seconds


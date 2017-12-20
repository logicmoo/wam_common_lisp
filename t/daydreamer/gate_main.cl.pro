
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_main" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:11 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" This file contains:", 1, 202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:223 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" OB slot-filler objects", 1, 224)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:248 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 249)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 10/13/84: Original version written",
				     1,
				     251)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:287 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/24/86: Added path functions, got rid of weblists",
				     1,
				     288)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:341 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  1/28/86: Changed add to use append-end instead of cons",
				     1,
				     342)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:399 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/25/86: Converted to be independent of flavors",
				     1,
				     400)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:450 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 11/02/86: Added add-unique-name", 1, 451)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:484 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 485)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:486 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     487)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:567 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, 'is-var?', [x], ['var?', x]]).

% annotating U::IS-VAR? 
wl: lambda_def(defun, u_is_var_c63, f_u_is_var_c63, [u_x], [[u_var_c63, u_x]]).


% annotating U::IS-VAR? 
wl: arglist_info(u_is_var_c63,
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

% annotating U::IS-VAR? 
wl: init_args(exact_only, u_is_var_c63).


% annotating U::IS-VAR? 
f_u_is_var_c63(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_var_c63(u_x, Var_c63_Ret),
	Var_c63_Ret=FnResult.
:- set_opv(f_u_is_var_c63, classof, claz_function),
   set_opv(u_is_var_c63, compile_as, kw_function),
   set_opv(u_is_var_c63, function, f_u_is_var_c63),
   DefunResult=u_is_var_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:567 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Global list of obs", 1, 599)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:619 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*obs*', []]).
:- set_var(TLEnv3, setq, u_xx_obs_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:619 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Global list of obnames", 1, 638)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:662 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*obnames*', []]).
:- set_var(TLEnv3, setq, u_xx_obnames_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:662 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 685)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:662 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" OBR", 1, 687)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:662 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 693)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:695 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'print-ob',
			    [ob, stream, depth],
			    [declare, [ignore, depth]],
			    ['ob$print-self', ob, stream]
			  ]).

% annotating U::PRINT-OB 
wl: lambda_def(defun,
	      u_print_ob,
	      f_u_print_ob,
	      [u_ob, stream, u_depth],
	      [[declare, [ignore, u_depth]], [u_ob_c36_print_self, u_ob, stream]]).


% annotating U::PRINT-OB 
wl: arglist_info(u_print_ob,
		[u_ob, stream, u_depth],
		[Ob_Param, Stream_Param, Depth_Param],
		arginfo{ all:[u_ob, stream, u_depth],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, stream, u_depth],
			 opt:0,
			 req:[u_ob, stream, u_depth],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRINT-OB 
wl: init_args(exact_only, u_print_ob).


% annotating U::PRINT-OB 
f_u_print_ob(Ob_Param, Stream_Param, Depth_Param, FnResult) :-
	Env=[bv(u_depth, Depth_Param)],
	cl_declare([ignore, u_depth], Declare_Ret),
	f_u_ob_c36_print_self(Ob_Param, Stream_Param, Print_self_Ret),
	Print_self_Ret=FnResult.
:- set_opv(f_u_print_ob, classof, claz_function),
   set_opv(u_print_ob, compile_as, kw_function),
   set_opv(u_print_ob, function, f_u_print_ob),
   DefunResult=u_print_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:784 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defstruct,
			    [obr, [':print-function', 'print-ob']],
			    '$STRING'("OB representation structure"),
			    [obnames, []],
			    [slots, []],
			    [literal, []]
			  ]).
:- cl_defstruct(
		[ [u_obr, [kw_print_function, u_print_ob]],
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\('O'),
			     #\('B'),
			     #\(' '),
			     #\(r),
			     #\(e),
			     #\(p),
			     #\(r),
			     #\(e),
			     #\(s),
			     #\(e),
			     #\(n),
			     #\(t),
			     #\(a),
			     #\(t),
			     #\(i),
			     #\(o),
			     #\(n),
			     #\(' '),
			     #\(s),
			     #\(t),
			     #\(r),
			     #\(u),
			     #\(c),
			     #\(t),
			     #\(u),
			     #\(r),
			     #\(e)
			   ]),
		  [u_obnames, []],
		  [sys_slots, []],
		  [u_literal, []]
		],
		_Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:784 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" list of symbols which may be used to name the ob",
				     24,
				     888)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:784 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" list of (slot-name slot-value)", 24, 962)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:784 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" whether the ob is a literal ob",
				     24,
				     1018)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1053 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$print-self',
			    [self, stream],
			    
			    [ cond,
			      
			      [ ['ty?', self],
				
				[ format,
				  stream,
				  '$STRING'("#{~A}"),
				  [car, ['obr-obnames', self]]
				]
			      ],
			      
			      [ ['var?', self],
				
				[ format,
				  stream,
				  '$STRING'("#{~A: ?~A:~A}"),
				  [car, ['obr-obnames', self]],
				  ['variable-name', self],
				  
				  [ if,
				    ['variable-type', self],
				    
				    [ car,
				      ['obr-obnames', ['variable-type', self]]
				    ],
				    []
				  ]
				]
			      ],
			      
			      [ else,
				
				[ format,
				  stream,
				  '$STRING'("#{~A: "),
				  [car, ['obr-obnames', self]]
				],
				['ob$sprint', self, stream],
				[format, stream, '$STRING'("}")]
			      ]
			    ]
			  ]).

% annotating U::OB$PRINT-SELF 
wl: lambda_def(defun,
	      u_ob_c36_print_self,
	      f_u_ob_c36_print_self,
	      [u_self, stream],
	      
	      [ 
		[ cond,
		  
		  [ [u_ty_c63, u_self],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(#), #\('{'), #\(~), #\('A'), #\('}')]),
		      [car, [u_obr_obnames, u_self]]
		    ]
		  ],
		  
		  [ [u_var_c63, u_self],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(#),
				 #\('{'),
				 #\(~),
				 #\('A'),
				 #\(:),
				 #\(' '),
				 #\(?),
				 #\(~),
				 #\('A'),
				 #\(:),
				 #\(~),
				 #\('A'),
				 #\('}')
			       ]),
		      [car, [u_obr_obnames, u_self]],
		      [u_variable_name, u_self],
		      
		      [ if,
			[u_variable_type, u_self],
			[car, [u_obr_obnames, [u_variable_type, u_self]]],
			[]
		      ]
		    ]
		  ],
		  
		  [ u_else,
		    
		    [ format,
		      stream,
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(#), #\('{'), #\(~), #\('A'), #\(:), #\(' ')]),
		      [car, [u_obr_obnames, u_self]]
		    ],
		    [u_ob_c36_sprint, u_self, stream],
		    
		    [ format,
		      stream,
		      '$ARRAY'([*], claz_base_character, [#\('}')])
		    ]
		  ]
		]
	      ]).


% annotating U::OB$PRINT-SELF 
wl: arglist_info(u_ob_c36_print_self,
		[u_self, stream],
		[Self_Param, Stream_Param],
		arginfo{ all:[u_self, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, stream],
			 opt:0,
			 req:[u_self, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PRINT-SELF 
wl: init_args(exact_only, u_ob_c36_print_self).


% annotating U::OB$PRINT-SELF 
f_u_ob_c36_print_self(Self_Param, Stream_Param, ElseResult36) :-
	Env=[bv(u_self, Self_Param), bv(stream, Stream_Param)],
	f_u_ty_c63(u_self, IFTEST),
	(   IFTEST\==[]
	->  f_u_obr_obnames(Self_Param, Car_Param),
	    cl_car(Car_Param, Car_Ret),
	    cl_format(
		      [ Stream_Param,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(#), #\('{'), #\(~), #\('A'), #\('}')]),
			Car_Ret
		      ],
		      TrueResult37),
	    ElseResult36=TrueResult37
	;   f_u_var_c63(u_self, IFTEST18),
	    (   IFTEST18\==[]
	    ->  f_u_obr_obnames(Self_Param, Car_Param42),
		cl_car(Car_Param42, Car_Ret47),
		f_u_variable_name(u_self, Variable_name_Ret),
		f_u_variable_type(u_self, IFTEST22),
		(   IFTEST22\==[]
		->  f_u_variable_type(u_self, Obr_obnames_Param),
		    f_u_obr_obnames(Obr_obnames_Param, Car_Param44),
		    cl_car(Car_Param44, TrueResult),
		    CAR=TrueResult
		;   CAR=[]
		),
		cl_format(
			  [ Stream_Param,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(#),
				       #\('{'),
				       #\(~),
				       #\('A'),
				       #\(:),
				       #\(' '),
				       #\(?),
				       #\(~),
				       #\('A'),
				       #\(:),
				       #\(~),
				       #\('A'),
				       #\('}')
				     ]),
			    Car_Ret47,
			    Variable_name_Ret,
			    CAR
			  ],
			  TrueResult35),
		ElseResult36=TrueResult35
	    ;   get_var(Env, u_else, IFTEST25),
		(   IFTEST25\==[]
		->  f_u_obr_obnames(Self_Param, Car_Param45),
		    cl_car(Car_Param45, Car_Ret50),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(#),
					   #\('{'),
					   #\(~),
					   #\('A'),
					   #\(:),
					   #\(' ')
					 ]),
				Car_Ret50
			      ],
			      Format_Ret),
		    f_u_ob_c36_sprint(Self_Param, Stream_Param, C36_sprint_Ret),
		    cl_format(
			      [ Stream_Param,
				'$ARRAY'([*], claz_base_character, [#\('}')])
			      ],
			      TrueResult33),
		    ElseResult36=TrueResult33
		;   ElseResult36=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_print_self, classof, claz_function),
   set_opv(u_ob_c36_print_self, compile_as, kw_function),
   set_opv(u_ob_c36_print_self, function, f_u_ob_c36_print_self),
   DefunResult=u_ob_c36_print_self.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1578 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*hidden-default*', t]).
:- set_var(TLEnv3, setq, u_xx_hidden_default_xx, t).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1605 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*next-ob-number*', 1]).
:- set_var(TLEnv3, setq, u_xx_next_ob_number_xx, 1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1605 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1633)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1605 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ob$create-named-empty: create an empty ob with the specified name",
				     1,
				     1635)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1605 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1703)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1704 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$create-named-empty',
			    [name],
			    
			    [ let,
			      [[self, ['make-obr']]],
			      [setq, '*obs*', [cons, self, '*obs*']],
			      
			      [ if,
				name,
				['ob$add-name', self, name],
				
				[ 'ob$add-unique-name',
				  self,
				  
				  [ 'string->symbol',
				    
				    [ 'string-append',
				      '$STRING'("OB."),
				      
				      [ prog1,
					['fixnum->string', '*next-ob-number*'],
					['increment-me', '*next-ob-number*']
				      ]
				    ]
				  ]
				]
			      ],
			      self
			    ]
			  ]).

% annotating U::OB$CREATE-NAMED-EMPTY 
wl: lambda_def(defun,
	      u_ob_c36_create_named_empty,
	      f_u_ob_c36_create_named_empty,
	      [sys_name],
	      
	      [ 
		[ let,
		  [[u_self, [u_make_obr]]],
		  [setq, u_xx_obs_xx, [cons, u_self, u_xx_obs_xx]],
		  
		  [ if,
		    sys_name,
		    [u_ob_c36_add_name, u_self, sys_name],
		    
		    [ u_ob_c36_add_unique_name,
		      u_self,
		      
		      [ u_string_c62_symbol,
			
			[ u_string_append,
			  '$ARRAY'([*],
				   claz_base_character,
				   [#\('O'), #\('B'), #\('.')]),
			  
			  [ prog1,
			    [u_fixnum_c62_string, u_xx_next_ob_number_xx],
			    [u_increment_me, u_xx_next_ob_number_xx]
			  ]
			]
		      ]
		    ]
		  ],
		  u_self
		]
	      ]).


% annotating U::OB$CREATE-NAMED-EMPTY 
wl: arglist_info(u_ob_c36_create_named_empty,
		[sys_name],
		[Name_Param],
		arginfo{ all:[sys_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[sys_name],
			 opt:0,
			 req:[sys_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$CREATE-NAMED-EMPTY 
wl: init_args(exact_only, u_ob_c36_create_named_empty).


% annotating U::OB$CREATE-NAMED-EMPTY 
f_u_ob_c36_create_named_empty(Name_Param, FnResult) :-
	Env=[bv(sys_name, Name_Param)],
	f_u_make_obr(Self_Init),
	LEnv=[[bv(u_self, Self_Init)]|Env],
	get_var(LEnv, u_self, Self_Get),
	get_var(LEnv, u_xx_obs_xx, Xx_obs_xx_Get),
	Xx_obs_xx=[Self_Get|Xx_obs_xx_Get],
	set_var(LEnv, u_xx_obs_xx, Xx_obs_xx),
	(   Name_Param\==[]
	->  get_var(LEnv, u_self, Self_Get21),
	    f_u_ob_c36_add_name(Self_Get21, Name_Param, TrueResult),
	    _125472=TrueResult
	;   get_var(LEnv, u_self, Self_Get23),
	    f_u_string_c62_symbol(
				  [ u_string_append,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\('O'), #\('B'), #\('.')]),
				    
				    [ prog1,
				      
				      [ u_fixnum_c62_string,
					u_xx_next_ob_number_xx
				      ],
				      [u_increment_me, u_xx_next_ob_number_xx]
				    ]
				  ],
				  C62_symbol_Ret),
	    f_u_ob_c36_add_unique_name(Self_Get23, C62_symbol_Ret, ElseResult),
	    _125472=ElseResult
	),
	get_var(LEnv, u_self, Self_Get26),
	LetResult=Self_Get26,
	LetResult=FnResult.
:- set_opv(f_u_ob_c36_create_named_empty, classof, claz_function),
   set_opv(u_ob_c36_create_named_empty, compile_as, kw_function),
   set_opv(u_ob_c36_create_named_empty, function, f_u_ob_c36_create_named_empty),
   DefunResult=u_ob_c36_create_named_empty.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2145 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$destroy',
			    [self],
			    
			    [ if,
			      ['obr-node', self],
			      
			      [ error,
				'$STRING'("Sure enough, node of ~A isn't nil!"),
				self
			      ]
			    ],
			    ['ob$remove-all', self],
			    
			    [ yloop,
			      [yfor, obname, in, ['obr-obnames', self]],
			      [ydo, ['ob$remove-name', self, obname]]
			    ],
			    [setq, '*obs*', ['delq!', self, '*obs*']]
			  ]).

% annotating U::OB$DESTROY 
wl: lambda_def(defun,
	      u_ob_c36_destroy,
	      f_u_ob_c36_destroy,
	      [u_self],
	      
	      [ 
		[ if,
		  [u_obr_node, u_self],
		  
		  [ error,
		    '$ARRAY'([*],
			     claz_base_character,
			     
			     [ #\('S'),
			       #\(u),
			       #\(r),
			       #\(e),
			       #\(' '),
			       #\(e),
			       #\(n),
			       #\(o),
			       #\(u),
			       #\(g),
			       #\(h),
			       #\(','),
			       #\(' '),
			       #\(n),
			       #\(o),
			       #\(d),
			       #\(e),
			       #\(' '),
			       #\(o),
			       #\(f),
			       #\(' '),
			       #\(~),
			       #\('A'),
			       #\(' '),
			       #\(i),
			       #\(s),
			       #\(n),
			       #\('\''),
			       #\(t),
			       #\(' '),
			       #\(n),
			       #\(i),
			       #\(l),
			       #\(!)
			     ]),
		    u_self
		  ]
		],
		[u_ob_c36_remove_all, u_self],
		
		[ u_yloop,
		  [u_yfor, u_obname, u_in, [u_obr_obnames, u_self]],
		  [u_ydo, [u_ob_c36_remove_name, u_self, u_obname]]
		],
		[setq, u_xx_obs_xx, [u_delq_c33, u_self, u_xx_obs_xx]]
	      ]).


% annotating U::OB$DESTROY 
wl: arglist_info(u_ob_c36_destroy,
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

% annotating U::OB$DESTROY 
wl: init_args(exact_only, u_ob_c36_destroy).


% annotating U::OB$DESTROY 
f_u_ob_c36_destroy(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_obr_node(Self_Param, IFTEST),
	(   IFTEST\==[]
	->  cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				
				[ #\('S'),
				  #\(u),
				  #\(r),
				  #\(e),
				  #\(' '),
				  #\(e),
				  #\(n),
				  #\(o),
				  #\(u),
				  #\(g),
				  #\(h),
				  #\(','),
				  #\(' '),
				  #\(n),
				  #\(o),
				  #\(d),
				  #\(e),
				  #\(' '),
				  #\(o),
				  #\(f),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(i),
				  #\(s),
				  #\(n),
				  #\('\''),
				  #\(t),
				  #\(' '),
				  #\(n),
				  #\(i),
				  #\(l),
				  #\(!)
				]),
		       Self_Param
		     ],
		     TrueResult),
	    _125630=TrueResult
	;   _125630=[]
	),
	f_u_ob_c36_remove_all(Self_Param, Remove_all_Ret),
	f_u_yloop(
		  [ [u_yfor, u_obname, u_in, [u_obr_obnames, u_self]],
		    [u_ydo, [u_ob_c36_remove_name, u_self, u_obname]]
		  ],
		  Yloop_Ret),
	f_u_delq_c33(u_self, u_xx_obs_xx, Xx_obs_xx),
	set_var(Env, u_xx_obs_xx, Xx_obs_xx),
	Xx_obs_xx=FnResult.
:- set_opv(f_u_ob_c36_destroy, classof, claz_function),
   set_opv(u_ob_c36_destroy, compile_as, kw_function),
   set_opv(u_ob_c36_destroy, function, f_u_ob_c36_destroy),
   DefunResult=u_ob_c36_destroy.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2145 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2398)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2145 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Inverse slots", 1, 2400)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2145 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2416)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2418 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*inverse-slot-list*', []]).
:- set_var(TLEnv3, setq, u_xx_inverse_slot_list_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2418 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2451)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" A primary slot is any that does not have an inverse or one that",
				     1,
				     2453)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2418 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" was explicitly defined as a primary slot in a primary/secondary",
				     1,
				     2519)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2418 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" declaration.", 1, 2585)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2418 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2600)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2601 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'primary-slot?',
			    ['slot-name'],
			    
			    [ or,
			      
			      [ not,
				[not, [assq, 'slot-name', '*inverse-slot-list*']]
			      ],
			      
			      [ yloop,
				
				[ initial,
				  [rest, '*inverse-slot-list*'],
				  [result, []]
				],
				[ywhile, rest],
				[yuntil, result],
				
				[ ydo,
				  
				  [ if,
				    ['eq?', 'slot-name', [cadr, [car, rest]]],
				    [setq, result, [car, [car, rest]]]
				  ],
				  [setq, rest, [cdr, rest]]
				],
				[yresult, ['null?', result]]
			      ]
			    ]
			  ]).

% annotating U::PRIMARY-SLOT? 
wl: lambda_def(defun,
	      u_primary_slot_c63,
	      f_u_primary_slot_c63,
	      [u_slot_name],
	      
	      [ 
		[ or,
		  [not, [not, [ext_assq, u_slot_name, u_xx_inverse_slot_list_xx]]],
		  
		  [ u_yloop,
		    [u_initial, [rest, u_xx_inverse_slot_list_xx], [u_result, []]],
		    [u_ywhile, rest],
		    [u_yuntil, u_result],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_slot_name, [cadr, [car, rest]]],
			[setq, u_result, [car, [car, rest]]]
		      ],
		      [setq, rest, [cdr, rest]]
		    ],
		    [u_yresult, [u_null_c63, u_result]]
		  ]
		]
	      ]).


% annotating U::PRIMARY-SLOT? 
wl: arglist_info(u_primary_slot_c63,
		[u_slot_name],
		[Slot_name_Param],
		arginfo{ all:[u_slot_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_slot_name],
			 opt:0,
			 req:[u_slot_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PRIMARY-SLOT? 
wl: init_args(exact_only, u_primary_slot_c63).


% annotating U::PRIMARY-SLOT? 
f_u_primary_slot_c63(Slot_name_Param, FnResult) :-
	Env=[bv(u_slot_name, Slot_name_Param)],
	(   f_ext_assq(u_slot_name,
		       u_xx_inverse_slot_list_xx,
		       Xx_inverse_slot_list_xx),
	    cl_not(Xx_inverse_slot_list_xx, Not_Param),
	    cl_not(Not_Param, FORM1_Res),
	    FORM1_Res\==[],
	    FnResult=FORM1_Res
	->  true
	;   f_u_yloop(
		      [ 
			[ u_initial,
			  [rest, u_xx_inverse_slot_list_xx],
			  [u_result, []]
			],
			[u_ywhile, rest],
			[u_yuntil, u_result],
			
			[ u_ydo,
			  
			  [ if,
			    [u_eq_c63, u_slot_name, [cadr, [car, rest]]],
			    [setq, u_result, [car, [car, rest]]]
			  ],
			  [setq, rest, [cdr, rest]]
			],
			[u_yresult, [u_null_c63, u_result]]
		      ],
		      Yloop_Ret),
	    FnResult=Yloop_Ret
	).
:- set_opv(f_u_primary_slot_c63, classof, claz_function),
   set_opv(u_primary_slot_c63, compile_as, kw_function),
   set_opv(u_primary_slot_c63, function, f_u_primary_slot_c63),
   DefunResult=u_primary_slot_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3015 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inverse-slot',
			    ['slot-name'],
			    
			    [ let,
			      
			      [ 
				[ found,
				  [assq, 'slot-name', '*inverse-slot-list*']
				]
			      ],
			      
			      [ if,
				found,
				[cadr, found],
				
				[ yloop,
				  
				  [ initial,
				    [rest, '*inverse-slot-list*'],
				    [result, []]
				  ],
				  [ywhile, rest],
				  [yuntil, result],
				  
				  [ ydo,
				    
				    [ if,
				      ['eq?', 'slot-name', [cadr, [car, rest]]],
				      [setq, result, [car, [car, rest]]]
				    ],
				    [setq, rest, [cdr, rest]]
				  ],
				  [yresult, result]
				]
			      ]
			    ]
			  ]).

% annotating U::INVERSE-SLOT 
wl: lambda_def(defun,
	      u_inverse_slot,
	      f_u_inverse_slot,
	      [u_slot_name],
	      
	      [ 
		[ let,
		  [[u_found, [ext_assq, u_slot_name, u_xx_inverse_slot_list_xx]]],
		  
		  [ if,
		    u_found,
		    [cadr, u_found],
		    
		    [ u_yloop,
		      
		      [ u_initial,
			[rest, u_xx_inverse_slot_list_xx],
			[u_result, []]
		      ],
		      [u_ywhile, rest],
		      [u_yuntil, u_result],
		      
		      [ u_ydo,
			
			[ if,
			  [u_eq_c63, u_slot_name, [cadr, [car, rest]]],
			  [setq, u_result, [car, [car, rest]]]
			],
			[setq, rest, [cdr, rest]]
		      ],
		      [u_yresult, u_result]
		    ]
		  ]
		]
	      ]).


% annotating U::INVERSE-SLOT 
wl: arglist_info(u_inverse_slot,
		[u_slot_name],
		[Slot_name_Param],
		arginfo{ all:[u_slot_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_slot_name],
			 opt:0,
			 req:[u_slot_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INVERSE-SLOT 
wl: init_args(exact_only, u_inverse_slot).


% annotating U::INVERSE-SLOT 
f_u_inverse_slot(Slot_name_Param, FnResult) :-
	Env=[bv(u_slot_name, Slot_name_Param)],
	f_ext_assq(u_slot_name, u_xx_inverse_slot_list_xx, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get19),
	    cl_cadr(Found_Get19, TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ 
			[ u_initial,
			  [rest, u_xx_inverse_slot_list_xx],
			  [u_result, []]
			],
			[u_ywhile, rest],
			[u_yuntil, u_result],
			
			[ u_ydo,
			  
			  [ if,
			    [u_eq_c63, u_slot_name, [cadr, [car, rest]]],
			    [setq, u_result, [car, [car, rest]]]
			  ],
			  [setq, rest, [cdr, rest]]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_inverse_slot, classof, claz_function),
   set_opv(u_inverse_slot, compile_as, kw_function),
   set_opv(u_inverse_slot, function, f_u_inverse_slot),
   DefunResult=u_inverse_slot.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3471 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$decl-has-inverse',
			    ['primary-slot-name'],
			    
			    [ 'ob$decl-inverses',
			      'primary-slot-name',
			      
			      [ 'string->symbol',
				
				[ 'string-append',
				  ['symbol->string', 'primary-slot-name'],
				  '$STRING'("-OF")
				]
			      ]
			    ]
			  ]).

% annotating U::OB$DECL-HAS-INVERSE 
wl: lambda_def(defun,
	      u_ob_c36_decl_has_inverse,
	      f_u_ob_c36_decl_has_inverse,
	      [u_primary_slot_name],
	      
	      [ 
		[ u_ob_c36_decl_inverses,
		  u_primary_slot_name,
		  
		  [ u_string_c62_symbol,
		    
		    [ u_string_append,
		      [u_symbol_c62_string, u_primary_slot_name],
		      '$ARRAY'([*],
			       claz_base_character,
			       [#\(-), #\('O'), #\('F')])
		    ]
		  ]
		]
	      ]).


% annotating U::OB$DECL-HAS-INVERSE 
wl: arglist_info(u_ob_c36_decl_has_inverse,
		[u_primary_slot_name],
		[Primary_slot_name_Param],
		arginfo{ all:[u_primary_slot_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_primary_slot_name],
			 opt:0,
			 req:[u_primary_slot_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$DECL-HAS-INVERSE 
wl: init_args(exact_only, u_ob_c36_decl_has_inverse).


% annotating U::OB$DECL-HAS-INVERSE 
f_u_ob_c36_decl_has_inverse(Primary_slot_name_Param, FnResult) :-
	Env=[bv(u_primary_slot_name, Primary_slot_name_Param)],
	f_u_string_c62_symbol(
			      [ u_string_append,
				[u_symbol_c62_string, u_primary_slot_name],
				'$ARRAY'([*],
					 claz_base_character,
					 [#\(-), #\('O'), #\('F')])
			      ],
			      C62_symbol_Ret),
	f_u_ob_c36_decl_inverses(Primary_slot_name_Param,
				 C62_symbol_Ret,
				 Decl_inverses_Ret),
	Decl_inverses_Ret=FnResult.
:- set_opv(f_u_ob_c36_decl_has_inverse, classof, claz_function),
   set_opv(u_ob_c36_decl_has_inverse, compile_as, kw_function),
   set_opv(u_ob_c36_decl_has_inverse, function, f_u_ob_c36_decl_has_inverse),
   DefunResult=u_ob_c36_decl_has_inverse.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'used-as-primary?',
			    [primary],
			    
			    [ and,
			      ['primary-slot?', primary],
			      
			      [ 'any?',
				
				[ lambda,
				  [ob],
				  
				  [ 'any?',
				    
				    [ lambda,
				      [slot],
				      ['eq?', primary, ['slots-name', slot]]
				    ],
				    ['ob$pairs', ob]
				  ]
				],
				'*obs*'
			      ]
			    ]
			  ]).

% annotating U::USED-AS-PRIMARY? 
wl: lambda_def(defun,
	      u_used_as_primary_c63,
	      f_u_used_as_primary_c63,
	      [u_primary],
	      
	      [ 
		[ and,
		  [u_primary_slot_c63, u_primary],
		  
		  [ u_any_c63,
		    
		    [ lambda,
		      [u_ob],
		      
		      [ u_any_c63,
			
			[ lambda,
			  [u_slot],
			  [u_eq_c63, u_primary, [u_slots_name, u_slot]]
			],
			[u_ob_c36_pairs, u_ob]
		      ]
		    ],
		    u_xx_obs_xx
		  ]
		]
	      ]).


% annotating U::USED-AS-PRIMARY? 
wl: arglist_info(u_used_as_primary_c63,
		[u_primary],
		[Primary_Param],
		arginfo{ all:[u_primary],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_primary],
			 opt:0,
			 req:[u_primary],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::USED-AS-PRIMARY? 
wl: init_args(exact_only, u_used_as_primary_c63).


% annotating U::USED-AS-PRIMARY? 
f_u_used_as_primary_c63(Primary_Param, FnResult) :-
	Env=[bv(u_primary, Primary_Param)],
	f_u_primary_slot_c63(Primary_Param, IFTEST),
	(   IFTEST\==[]
	->  f_u_any_c63(
			[ lambda,
			  [u_ob],
			  
			  [ u_any_c63,
			    
			    [ lambda,
			      [u_slot],
			      [u_eq_c63, u_primary, [u_slots_name, u_slot]]
			    ],
			    [u_ob_c36_pairs, u_ob]
			  ]
			],
			u_xx_obs_xx,
			TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_used_as_primary_c63, classof, claz_function),
   set_opv(u_used_as_primary_c63, compile_as, kw_function),
   set_opv(u_used_as_primary_c63, function, f_u_used_as_primary_c63),
   DefunResult=u_used_as_primary_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 3961)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$decl-inverses primary-slot-name secondary-slot-name):",
				     1,
				     3963)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4023)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Declare a primary-slot/secondary-slot pair. Simply warns if the",
				     1,
				     4025)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" declaration has already been performed.",
				     1,
				     4091)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 4133)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:4134 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$decl-inverses',
			    [primary, secondary],
			    
			    [ if,
			      [not, ['used-as-primary?', secondary]],
			      
			      [ if,
				
				[ and,
				  ['primary-slot?', primary],
				  ['eq?', ['inverse-slot', primary], secondary]
				],
				
				[ ndbg,
				  '*gate-dbg*',
				  'ob-warn',
				  '$STRING'("Warning: Duplicate primary/secondary declaration ~A ~A~%"),
				  primary,
				  secondary
				],
				
				[ cond,
				  
				  [ ['inverse-slot', primary],
				    
				    [ error,
				      '$STRING'("~A already has an inverse of ~A."),
				      primary,
				      ['inverse-slot', primary]
				    ]
				  ],
				  
				  [ ['inverse-slot', secondary],
				    
				    [ error,
				      '$STRING'("~A already has an inverse of ~A."),
				      primary,
				      ['inverse-slot', secondary]
				    ]
				  ],
				  
				  [ else,
				    
				    [ setq,
				      '*inverse-slot-list*',
				      
				      [ cons,
					[list, primary, secondary],
					'*inverse-slot-list*'
				      ]
				    ],
				    t
				  ]
				]
			      ],
			      
			      [ progn,
				
				[ format,
				  '*gate-output*',
				  '$STRING'("~&~A has already been used as a primary slot name.~%"),
				  secondary
				],
				
				[ format,
				  '*gate-output*',
				  '$STRING'("Declaration not performed.~%")
				]
			      ]
			    ]
			  ]).

% annotating U::OB$DECL-INVERSES 
wl: lambda_def(defun,
	      u_ob_c36_decl_inverses,
	      f_u_ob_c36_decl_inverses,
	      [u_primary, u_secondary],
	      
	      [ 
		[ if,
		  [not, [u_used_as_primary_c63, u_secondary]],
		  
		  [ if,
		    
		    [ and,
		      [u_primary_slot_c63, u_primary],
		      [u_eq_c63, [u_inverse_slot, u_primary], u_secondary]
		    ],
		    
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
				 #\('D'),
				 #\(u),
				 #\(p),
				 #\(l),
				 #\(i),
				 #\(c),
				 #\(a),
				 #\(t),
				 #\(e),
				 #\(' '),
				 #\(p),
				 #\(r),
				 #\(i),
				 #\(m),
				 #\(a),
				 #\(r),
				 #\(y),
				 #\(/),
				 #\(s),
				 #\(e),
				 #\(c),
				 #\(o),
				 #\(n),
				 #\(d),
				 #\(a),
				 #\(r),
				 #\(y),
				 #\(' '),
				 #\(d),
				 #\(e),
				 #\(c),
				 #\(l),
				 #\(a),
				 #\(r),
				 #\(a),
				 #\(t),
				 #\(i),
				 #\(o),
				 #\(n),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(~),
				 #\('%')
			       ]),
		      u_primary,
		      u_secondary
		    ],
		    
		    [ cond,
		      
		      [ [u_inverse_slot, u_primary],
			
			[ error,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(~),
				     #\('A'),
				     #\(' '),
				     #\(a),
				     #\(l),
				     #\(r),
				     #\(e),
				     #\(a),
				     #\(d),
				     #\(y),
				     #\(' '),
				     #\(h),
				     #\(a),
				     #\(s),
				     #\(' '),
				     #\(a),
				     #\(n),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(v),
				     #\(e),
				     #\(r),
				     #\(s),
				     #\(e),
				     #\(' '),
				     #\(o),
				     #\(f),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\('.')
				   ]),
			  u_primary,
			  [u_inverse_slot, u_primary]
			]
		      ],
		      
		      [ [u_inverse_slot, u_secondary],
			
			[ error,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(~),
				     #\('A'),
				     #\(' '),
				     #\(a),
				     #\(l),
				     #\(r),
				     #\(e),
				     #\(a),
				     #\(d),
				     #\(y),
				     #\(' '),
				     #\(h),
				     #\(a),
				     #\(s),
				     #\(' '),
				     #\(a),
				     #\(n),
				     #\(' '),
				     #\(i),
				     #\(n),
				     #\(v),
				     #\(e),
				     #\(r),
				     #\(s),
				     #\(e),
				     #\(' '),
				     #\(o),
				     #\(f),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\('.')
				   ]),
			  u_primary,
			  [u_inverse_slot, u_secondary]
			]
		      ],
		      
		      [ u_else,
			
			[ setq,
			  u_xx_inverse_slot_list_xx,
			  
			  [ cons,
			    [list, u_primary, u_secondary],
			    u_xx_inverse_slot_list_xx
			  ]
			],
			t
		      ]
		    ]
		  ],
		  
		  [ progn,
		    
		    [ format,
		      u_xx_gate_output_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\(~),
				 #\(&),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(h),
				 #\(a),
				 #\(s),
				 #\(' '),
				 #\(a),
				 #\(l),
				 #\(r),
				 #\(e),
				 #\(a),
				 #\(d),
				 #\(y),
				 #\(' '),
				 #\(b),
				 #\(e),
				 #\(e),
				 #\(n),
				 #\(' '),
				 #\(u),
				 #\(s),
				 #\(e),
				 #\(d),
				 #\(' '),
				 #\(a),
				 #\(s),
				 #\(' '),
				 #\(a),
				 #\(' '),
				 #\(p),
				 #\(r),
				 #\(i),
				 #\(m),
				 #\(a),
				 #\(r),
				 #\(y),
				 #\(' '),
				 #\(s),
				 #\(l),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(n),
				 #\(a),
				 #\(m),
				 #\(e),
				 #\('.'),
				 #\(~),
				 #\('%')
			       ]),
		      u_secondary
		    ],
		    
		    [ format,
		      u_xx_gate_output_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('D'),
				 #\(e),
				 #\(c),
				 #\(l),
				 #\(a),
				 #\(r),
				 #\(a),
				 #\(t),
				 #\(i),
				 #\(o),
				 #\(n),
				 #\(' '),
				 #\(n),
				 #\(o),
				 #\(t),
				 #\(' '),
				 #\(p),
				 #\(e),
				 #\(r),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(m),
				 #\(e),
				 #\(d),
				 #\('.'),
				 #\(~),
				 #\('%')
			       ])
		    ]
		  ]
		]
	      ]).


% annotating U::OB$DECL-INVERSES 
wl: arglist_info(u_ob_c36_decl_inverses,
		[u_primary, u_secondary],
		[Primary_Param, Secondary_Param],
		arginfo{ all:[u_primary, u_secondary],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_primary, u_secondary],
			 opt:0,
			 req:[u_primary, u_secondary],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$DECL-INVERSES 
wl: init_args(exact_only, u_ob_c36_decl_inverses).


% annotating U::OB$DECL-INVERSES 
f_u_ob_c36_decl_inverses(Primary_Param, Secondary_Param, ElseResult42) :-
	Env=[bv(u_primary, Primary_Param), bv(u_secondary, Secondary_Param)],
	f_u_used_as_primary_c63(Secondary_Param, PredArgResult),
	(   PredArgResult==[]
	->  f_u_primary_slot_c63(Primary_Param, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_eq_c63([u_inverse_slot, u_primary], u_secondary, TrueResult),
		IFTEST18=TrueResult
	    ;   IFTEST18=[]
	    ),
	    (   IFTEST18\==[]
	    ->  f_u_ndbg(u_xx_gate_dbg_xx,
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
				      #\('D'),
				      #\(u),
				      #\(p),
				      #\(l),
				      #\(i),
				      #\(c),
				      #\(a),
				      #\(t),
				      #\(e),
				      #\(' '),
				      #\(p),
				      #\(r),
				      #\(i),
				      #\(m),
				      #\(a),
				      #\(r),
				      #\(y),
				      #\(/),
				      #\(s),
				      #\(e),
				      #\(c),
				      #\(o),
				      #\(n),
				      #\(d),
				      #\(a),
				      #\(r),
				      #\(y),
				      #\(' '),
				      #\(d),
				      #\(e),
				      #\(c),
				      #\(l),
				      #\(a),
				      #\(r),
				      #\(a),
				      #\(t),
				      #\(i),
				      #\(o),
				      #\(n),
				      #\(' '),
				      #\(~),
				      #\('A'),
				      #\(' '),
				      #\(~),
				      #\('A'),
				      #\(~),
				      #\('%')
				    ]),
			   u_primary,
			   u_secondary
			 ],
			 TrueResult45),
		ElseResult42=TrueResult45
	    ;   f_u_inverse_slot(Primary_Param, IFTEST24),
		(   IFTEST24\==[]
		->  f_u_inverse_slot(Primary_Param, Inverse_slot_Ret),
		    cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					
					[ #\(~),
					  #\('A'),
					  #\(' '),
					  #\(a),
					  #\(l),
					  #\(r),
					  #\(e),
					  #\(a),
					  #\(d),
					  #\(y),
					  #\(' '),
					  #\(h),
					  #\(a),
					  #\(s),
					  #\(' '),
					  #\(a),
					  #\(n),
					  #\(' '),
					  #\(i),
					  #\(n),
					  #\(v),
					  #\(e),
					  #\(r),
					  #\(s),
					  #\(e),
					  #\(' '),
					  #\(o),
					  #\(f),
					  #\(' '),
					  #\(~),
					  #\('A'),
					  #\('.')
					]),
			       Primary_Param,
			       Inverse_slot_Ret
			     ],
			     TrueResult43),
		    ElseResult42=TrueResult43
		;   f_u_inverse_slot(Secondary_Param, IFTEST29),
		    (   IFTEST29\==[]
		    ->  f_u_inverse_slot(Secondary_Param, Inverse_slot_Ret56),
			cl_error(
				 [ '$ARRAY'([*],
					    claz_base_character,
					    
					    [ #\(~),
					      #\('A'),
					      #\(' '),
					      #\(a),
					      #\(l),
					      #\(r),
					      #\(e),
					      #\(a),
					      #\(d),
					      #\(y),
					      #\(' '),
					      #\(h),
					      #\(a),
					      #\(s),
					      #\(' '),
					      #\(a),
					      #\(n),
					      #\(' '),
					      #\(i),
					      #\(n),
					      #\(v),
					      #\(e),
					      #\(r),
					      #\(s),
					      #\(e),
					      #\(' '),
					      #\(o),
					      #\(f),
					      #\(' '),
					      #\(~),
					      #\('A'),
					      #\('.')
					    ]),
				   Primary_Param,
				   Inverse_slot_Ret56
				 ],
				 TrueResult41),
			ElseResult42=TrueResult41
		    ;   get_var(Env, u_else, IFTEST34),
			(   IFTEST34\==[]
			->  CAR=[Primary_Param, Secondary_Param],
			    get_var(Env,
				    u_xx_inverse_slot_list_xx,
				    Xx_inverse_slot_list_xx_Get),
			    Xx_inverse_slot_list_xx=[CAR|Xx_inverse_slot_list_xx_Get],
			    set_var(Env,
				    u_xx_inverse_slot_list_xx,
				    Xx_inverse_slot_list_xx),
			    ElseResult42=t
			;   ElseResult42=[]
			)
		    )
		)
	    )
	;   get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	    cl_format(
		      [ Xx_gate_output_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\(~),
				   #\(&),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(h),
				   #\(a),
				   #\(s),
				   #\(' '),
				   #\(a),
				   #\(l),
				   #\(r),
				   #\(e),
				   #\(a),
				   #\(d),
				   #\(y),
				   #\(' '),
				   #\(b),
				   #\(e),
				   #\(e),
				   #\(n),
				   #\(' '),
				   #\(u),
				   #\(s),
				   #\(e),
				   #\(d),
				   #\(' '),
				   #\(a),
				   #\(s),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(p),
				   #\(r),
				   #\(i),
				   #\(m),
				   #\(a),
				   #\(r),
				   #\(y),
				   #\(' '),
				   #\(s),
				   #\(l),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(n),
				   #\(a),
				   #\(m),
				   #\(e),
				   #\('.'),
				   #\(~),
				   #\('%')
				 ]),
			Secondary_Param
		      ],
		      Format_Ret),
	    get_var(Env, u_xx_gate_output_xx, Xx_gate_output_xx_Get49),
	    cl_format(
		      [ Xx_gate_output_xx_Get49,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('D'),
				   #\(e),
				   #\(c),
				   #\(l),
				   #\(a),
				   #\(r),
				   #\(a),
				   #\(t),
				   #\(i),
				   #\(o),
				   #\(n),
				   #\(' '),
				   #\(n),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(p),
				   #\(e),
				   #\(r),
				   #\(f),
				   #\(o),
				   #\(r),
				   #\(m),
				   #\(e),
				   #\(d),
				   #\('.'),
				   #\(~),
				   #\('%')
				 ])
		      ],
		      ElseResult51),
	    ElseResult42=ElseResult51
	).
:- set_opv(f_u_ob_c36_decl_inverses, classof, claz_function),
   set_opv(u_ob_c36_decl_inverses, compile_as, kw_function),
   set_opv(u_ob_c36_decl_inverses, function, f_u_ob_c36_decl_inverses),
   DefunResult=u_ob_c36_decl_inverses.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5095 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'decl-primary-secondaries',
			    [lst],
			    
			    [ map,
			      [quote, list],
			      
			      [ lambda,
				[pair],
				['ob$decl-inverses', [car, pair], [cadr, pair]]
			      ],
			      lst
			    ]
			  ]).

% annotating U::DECL-PRIMARY-SECONDARIES 
wl: lambda_def(defun,
	      u_decl_primary_secondaries,
	      f_u_decl_primary_secondaries,
	      [u_lst],
	      
	      [ 
		[ map,
		  [quote, list],
		  
		  [ lambda,
		    [u_pair],
		    [u_ob_c36_decl_inverses, [car, u_pair], [cadr, u_pair]]
		  ],
		  u_lst
		]
	      ]).


% annotating U::DECL-PRIMARY-SECONDARIES 
wl: arglist_info(u_decl_primary_secondaries,
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

% annotating U::DECL-PRIMARY-SECONDARIES 
wl: init_args(exact_only, u_decl_primary_secondaries).


% annotating U::DECL-PRIMARY-SECONDARIES 
f_u_decl_primary_secondaries(Lst_Param, FnResult) :-
	Env=[bv(u_lst, Lst_Param)],
	Lambda=closure([Env|Env], LResult, [u_pair],  (get_var(Env, u_pair, Pair_Get), cl_car(Pair_Get, Decl_inverses_Param), get_var(Env, u_pair, Pair_Get14), cl_cadr(Pair_Get14, Cadr_Ret), f_u_ob_c36_decl_inverses(Decl_inverses_Param, Cadr_Ret, LResult))),
	cl_map(list, Lambda, Lst_Param, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_decl_primary_secondaries, classof, claz_function),
   set_opv(u_decl_primary_secondaries, compile_as, kw_function),
   set_opv(u_decl_primary_secondaries, function, f_u_decl_primary_secondaries),
   DefunResult=u_decl_primary_secondaries.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5095 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5215)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5095 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This function is not perfect. If one desires duplicate pairs, this",
				     1,
				     5217)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5095 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" will not create them.", 1, 5286)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5095 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5310)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'enforce-inverses',
			    [],
			    
			    [ map,
			      [quote, list],
			      
			      [ lambda,
				[ob],
				
				[ if,
				  t,
				  
				  [ map,
				    [quote, list],
				    
				    [ lambda,
				      [slot],
				      
				      [ let,
					
					[ 
					  [ inv,
					    
					    [ 'inverse-slot',
					      ['slots-name', slot]
					    ]
					  ]
					],
					
					[ if,
					  
					  [ and,
					    inv,
					    ['ob?', ['slots-value', slot]],
					    
					    [ 'null?',
					      
					      [ 'memq?',
						ob,
						
						[ 'ob$gets',
						  ['slots-value', slot],
						  inv
						]
					      ]
					    ]
					  ],
					  
					  [ 'ob$basic-add',
					    ['slots-value', slot],
					    inv,
					    ob
					  ]
					]
				      ]
				    ],
				    ['ob$pairs', ob]
				  ]
				]
			      ],
			      '*obs*'
			    ]
			  ]).

% annotating U::ENFORCE-INVERSES 
wl: lambda_def(defun,
	      u_enforce_inverses,
	      f_u_enforce_inverses,
	      [],
	      
	      [ 
		[ map,
		  [quote, list],
		  
		  [ lambda,
		    [u_ob],
		    
		    [ if,
		      t,
		      
		      [ map,
			[quote, list],
			
			[ lambda,
			  [u_slot],
			  
			  [ let,
			    [[u_inv, [u_inverse_slot, [u_slots_name, u_slot]]]],
			    
			    [ if,
			      
			      [ and,
				u_inv,
				[u_ob_c63, [u_slots_value, u_slot]],
				
				[ u_null_c63,
				  
				  [ u_memq_c63,
				    u_ob,
				    
				    [ u_ob_c36_gets,
				      [u_slots_value, u_slot],
				      u_inv
				    ]
				  ]
				]
			      ],
			      
			      [ u_ob_c36_basic_add,
				[u_slots_value, u_slot],
				u_inv,
				u_ob
			      ]
			    ]
			  ]
			],
			[u_ob_c36_pairs, u_ob]
		      ]
		    ]
		  ],
		  u_xx_obs_xx
		]
	      ]).


% annotating U::ENFORCE-INVERSES 
wl: arglist_info(u_enforce_inverses,
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

% annotating U::ENFORCE-INVERSES 
wl: init_args(exact_only, u_enforce_inverses).


% annotating U::ENFORCE-INVERSES 
f_u_enforce_inverses(FnResult) :-
	Env=[],
	Lambda35=closure([Env32|Env], LResult34, [u_ob],  (t\==[]->Lambda=closure([ClosureEnvironment|Env32], LetResult, [u_slot],  (f_u_slots_name(u_slot, Inverse_slot_Param), f_u_inverse_slot(Inverse_slot_Param, Inv_Init), LEnv=[[bv(u_inv, Inv_Init)]|ClosureEnvironment], get_var(LEnv, u_inv, IFTEST17), (IFTEST17\==[]->f_u_ob_c63([u_slots_value, u_slot], IFTEST21), (IFTEST21\==[]->f_u_null_c63([u_memq_c63, u_ob, [u_ob_c36_gets, [u_slots_value, u_slot], u_inv]], TrueResult), TrueResult24=TrueResult;TrueResult24=[]), IFTEST15=TrueResult24;IFTEST15=[]), (IFTEST15\==[]->f_u_slots_value(u_slot, Basic_add_Param), get_var(LEnv, u_inv, Inv_Get25), get_var(LEnv, u_ob, Ob_Get), f_u_ob_c36_basic_add(Basic_add_Param, Inv_Get25, Ob_Get, TrueResult27), _126714=TrueResult27;_126714=[]), LetResult=_126714)), get_var(Env32, u_ob, Ob_Get31), f_u_ob_c36_pairs(Ob_Get31, C36_pairs_Ret), cl_map(list, Lambda, C36_pairs_Ret, TrueResult33), LResult34=TrueResult33;LResult34=[])),
	get_var(Env, u_xx_obs_xx, Xx_obs_xx_Get),
	cl_map(list, Lambda35, Xx_obs_xx_Get, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_enforce_inverses, classof, claz_function),
   set_opv(u_enforce_inverses, compile_as, kw_function),
   set_opv(u_enforce_inverses, function, f_u_enforce_inverses),
   DefunResult=u_enforce_inverses.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5781)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$add-name ob obname):", 1, 5783)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5810)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Associate another obname with the ob. This new obname may be",
				     1,
				     5812)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" used to refer to the ob, as may any obnames previously defined.",
				     1,
				     5875)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 5941)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Todo: a separate *obnames* for non \"OB.\" names would speed things up.",
				     1,
				     5943)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Alternatively, use hash tables.",
				     1,
				     6015)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 6049)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:6050 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$add-name',
			    [self, obname],
			    
			    [ if,
			      [not, ['memq?', obname, ['obr-obnames', self]]],
			      
			      [ progn,
				
				[ if,
				  ['ob?', obname],
				  
				  [ progn,
				    [setq, obname, ['ob$name', obname]],
				    
				    [ ndbg,
				      '*gate-dbg*',
				      'ob-warn',
				      '$STRING'("Warning: Probable obname redefinition.~%")
				    ]
				  ]
				],
				
				[ if,
				  [not, ['symbol?', obname]],
				  
				  [ setq,
				    obname,
				    
				    [ error,
				      '$STRING'("ob$add-name: ~A not symbol"),
				      obname
				    ]
				  ]
				],
				
				[ yloop,
				  [ywhile, [assq, obname, '*obnames*']],
				  
				  [ ydo,
				    
				    [ let,
				      
				      [ 
					[ 'new-obname',
					  
					  [ 'string->symbol',
					    
					    [ 'string-append',
					      ['symbol->string', obname],
					      '$STRING'("X")
					    ]
					  ]
					]
				      ],
				      
				      [ ndbg,
					'*gate-dbg*',
					'ob-warn',
					'$STRING'("Warning: Obname ~A already used--using ~A instead.~%"),
					obname,
					'new-obname'
				      ],
				      [setq, obname, 'new-obname']
				    ]
				  ]
				],
				
				[ setq,
				  '*obnames*',
				  [cons, [list, obname, self], '*obnames*']
				],
				
				[ 'set-obr-obnames',
				  self,
				  [cons, obname, ['obr-obnames', self]]
				],
				obname
			      ],
			      
			      [ progn,
				
				[ ndbg,
				  '*gate-dbg*',
				  'ob-warn',
				  '$STRING'("Warning: Obname ~A already in effect for specified ob.~%"),
				  obname
				],
				obname
			      ]
			    ]
			  ]).

% annotating U::OB$ADD-NAME 
wl: lambda_def(defun,
	      u_ob_c36_add_name,
	      f_u_ob_c36_add_name,
	      [u_self, u_obname],
	      
	      [ 
		[ if,
		  [not, [u_memq_c63, u_obname, [u_obr_obnames, u_self]]],
		  
		  [ progn,
		    
		    [ if,
		      [u_ob_c63, u_obname],
		      
		      [ progn,
			[setq, u_obname, [u_ob_c36_name, u_obname]],
			
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
				     #\('P'),
				     #\(r),
				     #\(o),
				     #\(b),
				     #\(a),
				     #\(b),
				     #\(l),
				     #\(e),
				     #\(' '),
				     #\(o),
				     #\(b),
				     #\(n),
				     #\(a),
				     #\(m),
				     #\(e),
				     #\(' '),
				     #\(r),
				     #\(e),
				     #\(d),
				     #\(e),
				     #\(f),
				     #\(i),
				     #\(n),
				     #\(i),
				     #\(t),
				     #\(i),
				     #\(o),
				     #\(n),
				     #\('.'),
				     #\(~),
				     #\('%')
				   ])
			]
		      ]
		    ],
		    
		    [ if,
		      [not, [u_symbol_c63, u_obname]],
		      
		      [ setq,
			u_obname,
			
			[ error,
			  '$ARRAY'([*],
				   claz_base_character,
				   
				   [ #\(o),
				     #\(b),
				     #\($),
				     #\(a),
				     #\(d),
				     #\(d),
				     #\(-),
				     #\(n),
				     #\(a),
				     #\(m),
				     #\(e),
				     #\(:),
				     #\(' '),
				     #\(~),
				     #\('A'),
				     #\(' '),
				     #\(n),
				     #\(o),
				     #\(t),
				     #\(' '),
				     #\(s),
				     #\(y),
				     #\(m),
				     #\(b),
				     #\(o),
				     #\(l)
				   ]),
			  u_obname
			]
		      ]
		    ],
		    
		    [ u_yloop,
		      [u_ywhile, [ext_assq, u_obname, u_xx_obnames_xx]],
		      
		      [ u_ydo,
			
			[ let,
			  
			  [ 
			    [ u_new_obname,
			      
			      [ u_string_c62_symbol,
				
				[ u_string_append,
				  [u_symbol_c62_string, u_obname],
				  '$ARRAY'([*], claz_base_character, [#\('X')])
				]
			      ]
			    ]
			  ],
			  
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
				       #\('O'),
				       #\(b),
				       #\(n),
				       #\(a),
				       #\(m),
				       #\(e),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(a),
				       #\(l),
				       #\(r),
				       #\(e),
				       #\(a),
				       #\(d),
				       #\(y),
				       #\(' '),
				       #\(u),
				       #\(s),
				       #\(e),
				       #\(d),
				       #\(-),
				       #\(-),
				       #\(u),
				       #\(s),
				       #\(i),
				       #\(n),
				       #\(g),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(s),
				       #\(t),
				       #\(e),
				       #\(a),
				       #\(d),
				       #\('.'),
				       #\(~),
				       #\('%')
				     ]),
			    u_obname,
			    u_new_obname
			  ],
			  [setq, u_obname, u_new_obname]
			]
		      ]
		    ],
		    
		    [ setq,
		      u_xx_obnames_xx,
		      [cons, [list, u_obname, u_self], u_xx_obnames_xx]
		    ],
		    
		    [ u_set_obr_obnames,
		      u_self,
		      [cons, u_obname, [u_obr_obnames, u_self]]
		    ],
		    u_obname
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
				 #\('O'),
				 #\(b),
				 #\(n),
				 #\(a),
				 #\(m),
				 #\(e),
				 #\(' '),
				 #\(~),
				 #\('A'),
				 #\(' '),
				 #\(a),
				 #\(l),
				 #\(r),
				 #\(e),
				 #\(a),
				 #\(d),
				 #\(y),
				 #\(' '),
				 #\(i),
				 #\(n),
				 #\(' '),
				 #\(e),
				 #\(f),
				 #\(f),
				 #\(e),
				 #\(c),
				 #\(t),
				 #\(' '),
				 #\(f),
				 #\(o),
				 #\(r),
				 #\(' '),
				 #\(s),
				 #\(p),
				 #\(e),
				 #\(c),
				 #\(i),
				 #\(f),
				 #\(i),
				 #\(e),
				 #\(d),
				 #\(' '),
				 #\(o),
				 #\(b),
				 #\('.'),
				 #\(~),
				 #\('%')
			       ]),
		      u_obname
		    ],
		    u_obname
		  ]
		]
	      ]).


% annotating U::OB$ADD-NAME 
wl: arglist_info(u_ob_c36_add_name,
		[u_self, u_obname],
		[Self_Param, Obname_Param],
		arginfo{ all:[u_self, u_obname],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_obname],
			 opt:0,
			 req:[u_self, u_obname],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$ADD-NAME 
wl: init_args(exact_only, u_ob_c36_add_name).


% annotating U::OB$ADD-NAME 
f_u_ob_c36_add_name(Self_Param, Obname_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_obname, Obname_Param)],
	f_u_memq_c63(u_obname, [u_obr_obnames, u_self], PredArgResult),
	(   PredArgResult==[]
	->  f_u_ob_c63(u_obname, IFTEST17),
	    (   IFTEST17\==[]
	    ->  get_var(Env, u_obname, Obname_Get),
		f_u_ob_c36_name(Obname_Get, Obname),
		set_var(Env, u_obname, Obname),
		f_u_ndbg(u_xx_gate_dbg_xx,
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
				      #\('P'),
				      #\(r),
				      #\(o),
				      #\(b),
				      #\(a),
				      #\(b),
				      #\(l),
				      #\(e),
				      #\(' '),
				      #\(o),
				      #\(b),
				      #\(n),
				      #\(a),
				      #\(m),
				      #\(e),
				      #\(' '),
				      #\(r),
				      #\(e),
				      #\(d),
				      #\(e),
				      #\(f),
				      #\(i),
				      #\(n),
				      #\(i),
				      #\(t),
				      #\(i),
				      #\(o),
				      #\(n),
				      #\('.'),
				      #\(~),
				      #\('%')
				    ])
			 ],
			 TrueResult),
		_130030=TrueResult
	    ;   _130030=[]
	    ),
	    f_u_symbol_c63(u_obname, PredArgResult23),
	    (   PredArgResult23==[]
	    ->  get_var(Env, u_obname, Obname_Get24),
		cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\(o),
				      #\(b),
				      #\($),
				      #\(a),
				      #\(d),
				      #\(d),
				      #\(-),
				      #\(n),
				      #\(a),
				      #\(m),
				      #\(e),
				      #\(:),
				      #\(' '),
				      #\(~),
				      #\('A'),
				      #\(' '),
				      #\(n),
				      #\(o),
				      #\(t),
				      #\(' '),
				      #\(s),
				      #\(y),
				      #\(m),
				      #\(b),
				      #\(o),
				      #\(l)
				    ]),
			   Obname_Get24
			 ],
			 TrueResult25),
		set_var(Env, u_obname, TrueResult25),
		_130108=TrueResult25
	    ;   _130108=[]
	    ),
	    f_u_yloop(
		      [ [u_ywhile, [ext_assq, u_obname, u_xx_obnames_xx]],
			
			[ u_ydo,
			  
			  [ let,
			    
			    [ 
			      [ u_new_obname,
				
				[ u_string_c62_symbol,
				  
				  [ u_string_append,
				    [u_symbol_c62_string, u_obname],
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\('X')])
				  ]
				]
			      ]
			    ],
			    
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
					 #\('O'),
					 #\(b),
					 #\(n),
					 #\(a),
					 #\(m),
					 #\(e),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(a),
					 #\(l),
					 #\(r),
					 #\(e),
					 #\(a),
					 #\(d),
					 #\(y),
					 #\(' '),
					 #\(u),
					 #\(s),
					 #\(e),
					 #\(d),
					 #\(-),
					 #\(-),
					 #\(u),
					 #\(s),
					 #\(i),
					 #\(n),
					 #\(g),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(i),
					 #\(n),
					 #\(s),
					 #\(t),
					 #\(e),
					 #\(a),
					 #\(d),
					 #\('.'),
					 #\(~),
					 #\('%')
				       ]),
			      u_obname,
			      u_new_obname
			    ],
			    [setq, u_obname, u_new_obname]
			  ]
			]
		      ],
		      Yloop_Ret),
	    get_var(Env, u_obname, Obname_Get26),
	    CAR=[Obname_Get26, Self_Param],
	    get_var(Env, u_xx_obnames_xx, Xx_obnames_xx_Get),
	    Xx_obnames_xx=[CAR|Xx_obnames_xx_Get],
	    set_var(Env, u_xx_obnames_xx, Xx_obnames_xx),
	    f_u_set_obr_obnames(u_self,
				[cons, u_obname, [u_obr_obnames, u_self]],
				Obr_obnames_Ret),
	    get_var(Env, u_obname, Obname_Get29),
	    FnResult=Obname_Get29
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
				  #\('O'),
				  #\(b),
				  #\(n),
				  #\(a),
				  #\(m),
				  #\(e),
				  #\(' '),
				  #\(~),
				  #\('A'),
				  #\(' '),
				  #\(a),
				  #\(l),
				  #\(r),
				  #\(e),
				  #\(a),
				  #\(d),
				  #\(y),
				  #\(' '),
				  #\(i),
				  #\(n),
				  #\(' '),
				  #\(e),
				  #\(f),
				  #\(f),
				  #\(e),
				  #\(c),
				  #\(t),
				  #\(' '),
				  #\(f),
				  #\(o),
				  #\(r),
				  #\(' '),
				  #\(s),
				  #\(p),
				  #\(e),
				  #\(c),
				  #\(i),
				  #\(f),
				  #\(i),
				  #\(e),
				  #\(d),
				  #\(' '),
				  #\(o),
				  #\(b),
				  #\('.'),
				  #\(~),
				  #\('%')
				]),
		       u_obname
		     ],
		     Ndbg_Ret),
	    get_var(Env, u_obname, Obname_Get30),
	    FnResult=Obname_Get30
	).
:- set_opv(f_u_ob_c36_add_name, classof, claz_function),
   set_opv(u_ob_c36_add_name, compile_as, kw_function),
   set_opv(u_ob_c36_add_name, function, f_u_ob_c36_add_name),
   DefunResult=u_ob_c36_add_name.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:6050 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This assumes obname is already determined to be unique. We assume that",
				     1,
				     7128)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:6050 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" we are able to generate unique \"OB.\" names above. This assumes the",
				     1,
				     7201)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:6050 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" user does not create such names also.",
				     1,
				     7270)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7309 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$add-unique-name',
			    [self, obname],
			    
			    [ setq,
			      '*obnames*',
			      [cons, [list, obname, self], '*obnames*']
			    ],
			    
			    [ 'set-obr-obnames',
			      self,
			      [cons, obname, ['obr-obnames', self]]
			    ],
			    obname
			  ]).

% annotating U::OB$ADD-UNIQUE-NAME 
wl: lambda_def(defun,
	      u_ob_c36_add_unique_name,
	      f_u_ob_c36_add_unique_name,
	      [u_self, u_obname],
	      
	      [ 
		[ setq,
		  u_xx_obnames_xx,
		  [cons, [list, u_obname, u_self], u_xx_obnames_xx]
		],
		
		[ u_set_obr_obnames,
		  u_self,
		  [cons, u_obname, [u_obr_obnames, u_self]]
		],
		u_obname
	      ]).


% annotating U::OB$ADD-UNIQUE-NAME 
wl: arglist_info(u_ob_c36_add_unique_name,
		[u_self, u_obname],
		[Self_Param, Obname_Param],
		arginfo{ all:[u_self, u_obname],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_obname],
			 opt:0,
			 req:[u_self, u_obname],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$ADD-UNIQUE-NAME 
wl: init_args(exact_only, u_ob_c36_add_unique_name).


% annotating U::OB$ADD-UNIQUE-NAME 
f_u_ob_c36_add_unique_name(Self_Param, Obname_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_obname, Obname_Param)],
	CAR=[Obname_Param, Self_Param],
	get_var(Env, u_xx_obnames_xx, Xx_obnames_xx_Get),
	Xx_obnames_xx=[CAR|Xx_obnames_xx_Get],
	set_var(Env, u_xx_obnames_xx, Xx_obnames_xx),
	f_u_set_obr_obnames(u_self,
			    [cons, u_obname, [u_obr_obnames, u_self]],
			    Obr_obnames_Ret),
	Obname_Param=FnResult.
:- set_opv(f_u_ob_c36_add_unique_name, classof, claz_function),
   set_opv(u_ob_c36_add_unique_name, compile_as, kw_function),
   set_opv(u_ob_c36_add_unique_name, function, f_u_ob_c36_add_unique_name),
   DefunResult=u_ob_c36_add_unique_name.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$remove-name',
			    [self, obname],
			    
			    [ 'set-obr-obnames',
			      self,
			      ['delq!', obname, ['obr-obnames', self]]
			    ],
			    
			    [ setq,
			      '*obnames*',
			      
			      [ 'del!',
				[lambda, [x, y], ['eq?', x, [car, y]]],
				obname,
				'*obnames*'
			      ]
			    ],
			    
			    [ if,
			      ['null?', ['obr-obnames', self]],
			      
			      [ 'ob$add-unique-name',
				self,
				
				[ 'string->symbol',
				  
				  [ 'string-append',
				    '$STRING'("OB."),
				    
				    [ prog1,
				      ['fixnum->string', '*next-ob-number*'],
				      ['increment-me', '*next-ob-number*']
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$REMOVE-NAME 
wl: lambda_def(defun,
	      u_ob_c36_remove_name,
	      f_u_ob_c36_remove_name,
	      [u_self, u_obname],
	      
	      [ 
		[ u_set_obr_obnames,
		  u_self,
		  [u_delq_c33, u_obname, [u_obr_obnames, u_self]]
		],
		
		[ setq,
		  u_xx_obnames_xx,
		  
		  [ u_del_c33,
		    [lambda, [u_x, u_y], [u_eq_c63, u_x, [car, u_y]]],
		    u_obname,
		    u_xx_obnames_xx
		  ]
		],
		
		[ if,
		  [u_null_c63, [u_obr_obnames, u_self]],
		  
		  [ u_ob_c36_add_unique_name,
		    u_self,
		    
		    [ u_string_c62_symbol,
		      
		      [ u_string_append,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\('O'), #\('B'), #\('.')]),
			
			[ prog1,
			  [u_fixnum_c62_string, u_xx_next_ob_number_xx],
			  [u_increment_me, u_xx_next_ob_number_xx]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$REMOVE-NAME 
wl: arglist_info(u_ob_c36_remove_name,
		[u_self, u_obname],
		[Self_Param, Obname_Param],
		arginfo{ all:[u_self, u_obname],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_obname],
			 opt:0,
			 req:[u_self, u_obname],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$REMOVE-NAME 
wl: init_args(exact_only, u_ob_c36_remove_name).


% annotating U::OB$REMOVE-NAME 
f_u_ob_c36_remove_name(Self_Param, Obname_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_obname, Obname_Param)],
	f_u_set_obr_obnames(u_self,
			    [u_delq_c33, u_obname, [u_obr_obnames, u_self]],
			    Obr_obnames_Ret),
	Lambda=closure([ClosureEnvironment|Env], LResult, [u_x, u_y], f_u_eq_c63(u_x, [car, u_y], LResult)),
	get_var(Env, u_xx_obnames_xx, Xx_obnames_xx_Get),
	f_u_del_c33(Lambda, Obname_Param, Xx_obnames_xx_Get, Xx_obnames_xx),
	set_var(Env, u_xx_obnames_xx, Xx_obnames_xx),
	f_u_null_c63([u_obr_obnames, u_self], IFTEST),
	(   IFTEST\==[]
	->  f_u_string_c62_symbol(
				  [ u_string_append,
				    '$ARRAY'([*],
					     claz_base_character,
					     [#\('O'), #\('B'), #\('.')]),
				    
				    [ prog1,
				      
				      [ u_fixnum_c62_string,
					u_xx_next_ob_number_xx
				      ],
				      [u_increment_me, u_xx_next_ob_number_xx]
				    ]
				  ],
				  C62_symbol_Ret),
	    f_u_ob_c36_add_unique_name(Self_Param, C62_symbol_Ret, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_ob_c36_remove_name, classof, claz_function),
   set_opv(u_ob_c36_remove_name, compile_as, kw_function),
   set_opv(u_ob_c36_remove_name, function, f_u_ob_c36_remove_name),
   DefunResult=u_ob_c36_remove_name.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7917)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$name->ob obname):", 1, 7919)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 7943)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Return the ob referred to by a obname. If there is no ob associated",
				     1,
				     7945)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" with the given obname, nil is returned.",
				     1,
				     8015)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8057)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8058 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$name->ob',
			    [obname],
			    
			    [ let,
			      [[found, [assq, obname, '*obnames*']]],
			      [if, found, [cadr, found], []]
			    ]
			  ]).

% annotating U::OB$NAME->OB 
wl: lambda_def(defun,
	      u_ob_c36_name_c62_ob,
	      f_u_ob_c36_name_c62_ob,
	      [u_obname],
	      
	      [ 
		[ let,
		  [[u_found, [ext_assq, u_obname, u_xx_obnames_xx]]],
		  [if, u_found, [cadr, u_found], []]
		]
	      ]).


% annotating U::OB$NAME->OB 
wl: arglist_info(u_ob_c36_name_c62_ob,
		[u_obname],
		[Obname_Param],
		arginfo{ all:[u_obname],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obname],
			 opt:0,
			 req:[u_obname],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$NAME->OB 
wl: init_args(exact_only, u_ob_c36_name_c62_ob).


% annotating U::OB$NAME->OB 
f_u_ob_c36_name_c62_ob(Obname_Param, FnResult) :-
	Env=[bv(u_obname, Obname_Param)],
	f_ext_assq(u_obname, u_xx_obnames_xx, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get19),
	    cl_cadr(Found_Get19, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_ob_c36_name_c62_ob, classof, claz_function),
   set_opv(u_ob_c36_name_c62_ob, compile_as, kw_function),
   set_opv(u_ob_c36_name_c62_ob, function, f_u_ob_c36_name_c62_ob),
   DefunResult=u_ob_c36_name_c62_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8058 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8163)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8058 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (ob$name ob):", 1, 8165)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8058 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8181)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8058 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Return a (actually, the most recently defined) obname for an ob.",
				     1,
				     8183)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8058 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8250)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8251 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'ob$name', [self], [car, ['obr-obnames', self]]]).

% annotating U::OB$NAME 
wl: lambda_def(defun,
	      u_ob_c36_name,
	      f_u_ob_c36_name,
	      [u_self],
	      [[car, [u_obr_obnames, u_self]]]).


% annotating U::OB$NAME 
wl: arglist_info(u_ob_c36_name,
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

% annotating U::OB$NAME 
wl: init_args(exact_only, u_ob_c36_name).


% annotating U::OB$NAME 
f_u_ob_c36_name(Self_Param, FnResult) :-
	f_u_obr_obnames(Self_Param, Car_Param),
	cl_car(Car_Param, Car_Ret),
	Car_Ret=FnResult.
:- set_opv(f_u_ob_c36_name, classof, claz_function),
   set_opv(u_ob_c36_name, compile_as, kw_function),
   set_opv(u_ob_c36_name, function, f_u_ob_c36_name),
   DefunResult=u_ob_c36_name.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8251 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8325)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8251 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" (ob$names ob):", 1, 8327)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8251 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8344)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8345 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'ob$names', [self], ['obr-obnames', self]]).

% annotating U::OB$NAMES 
wl: lambda_def(defun,
	      u_ob_c36_names,
	      f_u_ob_c36_names,
	      [u_self],
	      [[u_obr_obnames, u_self]]).


% annotating U::OB$NAMES 
wl: arglist_info(u_ob_c36_names,
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

% annotating U::OB$NAMES 
wl: init_args(exact_only, u_ob_c36_names).


% annotating U::OB$NAMES 
f_u_ob_c36_names(Self_Param, FnResult) :-
	f_u_obr_obnames(Self_Param, Obr_obnames_Ret),
	Obr_obnames_Ret=FnResult.
:- set_opv(f_u_ob_c36_names, classof, claz_function),
   set_opv(u_ob_c36_names, compile_as, kw_function),
   set_opv(u_ob_c36_names, function, f_u_ob_c36_names),
   DefunResult=u_ob_c36_names.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8345 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8392)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8345 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" The automatic setting of inverses can be disabled. Currently,",
				     1,
				     8394)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8345 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" inverse setting is turned off only during load of a ob dump.",
				     1,
				     8458)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8345 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8521)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8523 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*inverse-setting?*', t]).
:- set_var(TLEnv3, setq, u_xx_inverse_setting_c63_xx, t).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8552 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inverse-setting-on',
			    [],
			    
			    [ let,
			      [[previous, '*inverse-setting?*']],
			      [setq, '*inverse-setting?*', t],
			      previous
			    ]
			  ]).

% annotating U::INVERSE-SETTING-ON 
wl: lambda_def(defun,
	      u_inverse_setting_on,
	      f_u_inverse_setting_on,
	      [],
	      
	      [ 
		[ let,
		  [[u_previous, u_xx_inverse_setting_c63_xx]],
		  [setq, u_xx_inverse_setting_c63_xx, t],
		  u_previous
		]
	      ]).


% annotating U::INVERSE-SETTING-ON 
wl: arglist_info(u_inverse_setting_on,
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

% annotating U::INVERSE-SETTING-ON 
wl: init_args(exact_only, u_inverse_setting_on).


% annotating U::INVERSE-SETTING-ON 
f_u_inverse_setting_on(FnResult) :-
	Env=[],
	get_var(Env, u_xx_inverse_setting_c63_xx, Xx_inverse_setting_c63_xx_Get),
	LEnv=[[bv(u_previous, Xx_inverse_setting_c63_xx_Get)]|Env],
	set_var(LEnv, setq, u_xx_inverse_setting_c63_xx, t),
	get_var(LEnv, u_previous, Previous_Get),
	LetResult=Previous_Get,
	LetResult=FnResult.
:- set_opv(f_u_inverse_setting_on, classof, claz_function),
   set_opv(u_inverse_setting_on, compile_as, kw_function),
   set_opv(u_inverse_setting_on, function, f_u_inverse_setting_on),
   DefunResult=u_inverse_setting_on.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8668 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'inverse-setting-off',
			    [],
			    
			    [ let,
			      [[previous, '*inverse-setting?*']],
			      [setq, '*inverse-setting?*', []],
			      previous
			    ]
			  ]).

% annotating U::INVERSE-SETTING-OFF 
wl: lambda_def(defun,
	      u_inverse_setting_off,
	      f_u_inverse_setting_off,
	      [],
	      
	      [ 
		[ let,
		  [[u_previous, u_xx_inverse_setting_c63_xx]],
		  [setq, u_xx_inverse_setting_c63_xx, []],
		  u_previous
		]
	      ]).


% annotating U::INVERSE-SETTING-OFF 
wl: arglist_info(u_inverse_setting_off,
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

% annotating U::INVERSE-SETTING-OFF 
wl: init_args(exact_only, u_inverse_setting_off).


% annotating U::INVERSE-SETTING-OFF 
f_u_inverse_setting_off(FnResult) :-
	Env=[],
	get_var(Env, u_xx_inverse_setting_c63_xx, Xx_inverse_setting_c63_xx_Get),
	LEnv=[[bv(u_previous, Xx_inverse_setting_c63_xx_Get)]|Env],
	set_var(LEnv, setq, u_xx_inverse_setting_c63_xx, []),
	get_var(LEnv, u_previous, Previous_Get),
	LetResult=Previous_Get,
	LetResult=FnResult.
:- set_opv(f_u_inverse_setting_off, classof, claz_function),
   set_opv(u_inverse_setting_off, compile_as, kw_function),
   set_opv(u_inverse_setting_off, function, f_u_inverse_setting_off),
   DefunResult=u_inverse_setting_off.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'restore-inverse-setting',
			    [val],
			    [setq, '*inverse-setting?*', val]
			  ]).

% annotating U::RESTORE-INVERSE-SETTING 
wl: lambda_def(defun,
	      u_restore_inverse_setting,
	      f_u_restore_inverse_setting,
	      [u_val],
	      [[setq, u_xx_inverse_setting_c63_xx, u_val]]).


% annotating U::RESTORE-INVERSE-SETTING 
wl: arglist_info(u_restore_inverse_setting,
		[u_val],
		[Val_Param],
		arginfo{ all:[u_val],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_val],
			 opt:0,
			 req:[u_val],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RESTORE-INVERSE-SETTING 
wl: init_args(exact_only, u_restore_inverse_setting).


% annotating U::RESTORE-INVERSE-SETTING 
f_u_restore_inverse_setting(Val_Param, FnResult) :-
	Env=[bv(u_val, Val_Param)],
	set_var(Env, u_xx_inverse_setting_c63_xx, Val_Param),
	Val_Param=FnResult.
:- set_opv(f_u_restore_inverse_setting, classof, claz_function),
   set_opv(u_restore_inverse_setting, compile_as, kw_function),
   set_opv(u_restore_inverse_setting, function, f_u_restore_inverse_setting),
   DefunResult=u_restore_inverse_setting.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8859)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$add ob slot-name slot-value):",
				     1,
				     8861)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8897)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Add a slot value to an ob. If slot-value is an ob, the inverse slot addition",
				     1,
				     8899)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" is performed.", 1, 8978)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 8994)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8996 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$add',
			    [self, 'slot-name', 'slot-value'],
			    ['enforce-ob', self, '$STRING'("ob$add")],
			    ['ob$add1', self, 'slot-name', 'slot-value'],
			    'slot-value'
			  ]).

% annotating U::OB$ADD 
wl: lambda_def(defun,
	      u_ob_c36_add,
	      f_u_ob_c36_add,
	      [u_self, u_slot_name, slot_value],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(o), #\(b), #\($), #\(a), #\(d), #\(d)])
		],
		[u_ob_c36_add1, u_self, u_slot_name, slot_value],
		slot_value
	      ]).


% annotating U::OB$ADD 
wl: arglist_info(u_ob_c36_add,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$ADD 
wl: init_args(exact_only, u_ob_c36_add).


% annotating U::OB$ADD 
f_u_ob_c36_add(Self_Param, Slot_name_Param, Slot_value_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				[#\(o), #\(b), #\($), #\(a), #\(d), #\(d)]),
		       Enforce_ob_Ret),
	f_u_ob_c36_add1(Self_Param,
			Slot_name_Param,
			Slot_value_Param,
			C36_add1_Ret),
	Slot_value_Param=FnResult.
:- set_opv(f_u_ob_c36_add, classof, claz_function),
   set_opv(u_ob_c36_add, compile_as, kw_function),
   set_opv(u_ob_c36_add, function, f_u_ob_c36_add),
   DefunResult=u_ob_c36_add.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8996 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9121)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8996 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$padd ob slot-path slot-value):",
				     1,
				     9123)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8996 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9160)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8996 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Allows a path to be used.", 1, 9162)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8996 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9190)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9192 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$padd',
			    [self, 'slot-path', 'slot-value'],
			    ['enforce-ob', self, '$STRING'("ob$padd")],
			    
			    [ if,
			      ['pair?', 'slot-path'],
			      
			      [ 'ob$add1',
				['path->ob', self, 'slot-path'],
				['path->slot-name', 'slot-path'],
				'slot-value'
			      ],
			      ['ob$add1', self, 'slot-path', 'slot-value']
			    ],
			    'slot-value'
			  ]).

% annotating U::OB$PADD 
wl: lambda_def(defun,
	      u_ob_c36_padd,
	      f_u_ob_c36_padd,
	      [u_self, u_slot_path, slot_value],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(o), #\(b), #\($), #\(p), #\(a), #\(d), #\(d)])
		],
		
		[ if,
		  [u_pair_c63, u_slot_path],
		  
		  [ u_ob_c36_add1,
		    [u_path_c62_ob, u_self, u_slot_path],
		    [u_path_c62_slot_name, u_slot_path],
		    slot_value
		  ],
		  [u_ob_c36_add1, u_self, u_slot_path, slot_value]
		],
		slot_value
	      ]).


% annotating U::OB$PADD 
wl: arglist_info(u_ob_c36_padd,
		[u_self, u_slot_path, slot_value],
		[Self_Param, Slot_path_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_path, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_path, slot_value],
			 opt:0,
			 req:[u_self, u_slot_path, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PADD 
wl: init_args(exact_only, u_ob_c36_padd).


% annotating U::OB$PADD 
f_u_ob_c36_padd(Self_Param, Slot_path_Param, Slot_value_Param, Slot_value_Param) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_path, Slot_path_Param), bv(slot_value, Slot_value_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				[#\(o), #\(b), #\($), #\(p), #\(a), #\(d), #\(d)]),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  f_u_path_c62_ob(Self_Param, Slot_path_Param, C36_add1_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    f_u_ob_c36_add1(C36_add1_Param,
			    Slot_name_Ret,
			    Slot_value_Param,
			    TrueResult),
	    _127172=TrueResult
	;   f_u_ob_c36_add1(Self_Param,
			    Slot_path_Param,
			    Slot_value_Param,
			    ElseResult),
	    _127172=ElseResult
	).
:- set_opv(f_u_ob_c36_padd, classof, claz_function),
   set_opv(u_ob_c36_padd, compile_as, kw_function),
   set_opv(u_ob_c36_padd, function, f_u_ob_c36_padd),
   DefunResult=u_ob_c36_padd.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'path->ob',
			    [ob, path],
			    
			    [ if,
			      [cdr, path],
			      ['path->ob', ['ob$get', ob, [car, path]], [cdr, path]],
			      ob
			    ]
			  ]).

% annotating U::PATH->OB 
wl: lambda_def(defun,
	      u_path_c62_ob,
	      f_u_path_c62_ob,
	      [u_ob, u_path],
	      
	      [ 
		[ if,
		  [cdr, u_path],
		  
		  [ u_path_c62_ob,
		    [u_ob_c36_get, u_ob, [car, u_path]],
		    [cdr, u_path]
		  ],
		  u_ob
		]
	      ]).


% annotating U::PATH->OB 
wl: arglist_info(u_path_c62_ob,
		[u_ob, u_path],
		[Ob_Param, Path_Param],
		arginfo{ all:[u_ob, u_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_ob, u_path],
			 opt:0,
			 req:[u_ob, u_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PATH->OB 
wl: init_args(exact_only, u_path_c62_ob).


% annotating U::PATH->OB 
f_u_path_c62_ob(Ob_Param, Path_Param, FnResult) :-
	cl_cdr(Path_Param, IFTEST),
	(   IFTEST\==[]
	->  cl_car(Path_Param, Car_Ret),
	    f_u_ob_c36_get(Ob_Param, Car_Ret, C62_ob_Param),
	    cl_cdr(Path_Param, Cdr_Ret),
	    f_u_path_c62_ob(C62_ob_Param, Cdr_Ret, TrueResult),
	    FnResult=TrueResult
	;   FnResult=Ob_Param
	).
:- set_opv(f_u_path_c62_ob, classof, claz_function),
   set_opv(u_path_c62_ob, compile_as, kw_function),
   set_opv(u_path_c62_ob, function, f_u_path_c62_ob),
   DefunResult=u_path_c62_ob.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9600)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$remove ob slot-name slot-value):",
				     1,
				     9602)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9641)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Remove the specified value from the specified slot. If slot-value is a",
				     1,
				     9643)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ob, the inverse slot removal is performed.",
				     1,
				     9716)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 9761)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9763 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$remove',
			    [self, 'slot-name', 'slot-value'],
			    ['enforce-ob', self, '$STRING'("ob$remove")],
			    ['ob$remove1', self, 'slot-name', 'slot-value'],
			    'slot-value'
			  ]).

% annotating U::OB$REMOVE 
wl: lambda_def(defun,
	      u_ob_c36_remove,
	      f_u_ob_c36_remove,
	      [u_self, u_slot_name, slot_value],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(o),
			     #\(b),
			     #\($),
			     #\(r),
			     #\(e),
			     #\(m),
			     #\(o),
			     #\(v),
			     #\(e)
			   ])
		],
		[u_ob_c36_remove1, u_self, u_slot_name, slot_value],
		slot_value
	      ]).


% annotating U::OB$REMOVE 
wl: arglist_info(u_ob_c36_remove,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$REMOVE 
wl: init_args(exact_only, u_ob_c36_remove).


% annotating U::OB$REMOVE 
f_u_ob_c36_remove(Self_Param, Slot_name_Param, Slot_value_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				
				[ #\(o),
				  #\(b),
				  #\($),
				  #\(r),
				  #\(e),
				  #\(m),
				  #\(o),
				  #\(v),
				  #\(e)
				]),
		       Enforce_ob_Ret),
	f_u_ob_c36_remove1(Self_Param,
			   Slot_name_Param,
			   Slot_value_Param,
			   C36_remove1_Ret),
	Slot_value_Param=FnResult.
:- set_opv(f_u_ob_c36_remove, classof, claz_function),
   set_opv(u_ob_c36_remove, compile_as, kw_function),
   set_opv(u_ob_c36_remove, function, f_u_ob_c36_remove),
   DefunResult=u_ob_c36_remove.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$premove',
			    [self, 'slot-path', 'slot-value'],
			    ['enforce-ob', self, '$STRING'("ob$premove")],
			    
			    [ if,
			      ['pair?', 'slot-path'],
			      
			      [ 'ob$remove1',
				['path->ob', self, 'slot-path'],
				['path->slot-name', 'slot-path']
			      ],
			      ['ob$add1', self, 'slot-path', 'slot-value']
			    ],
			    'slot-value'
			  ]).

% annotating U::OB$PREMOVE 
wl: lambda_def(defun,
	      u_ob_c36_premove,
	      f_u_ob_c36_premove,
	      [u_self, u_slot_path, slot_value],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(o),
			     #\(b),
			     #\($),
			     #\(p),
			     #\(r),
			     #\(e),
			     #\(m),
			     #\(o),
			     #\(v),
			     #\(e)
			   ])
		],
		
		[ if,
		  [u_pair_c63, u_slot_path],
		  
		  [ u_ob_c36_remove1,
		    [u_path_c62_ob, u_self, u_slot_path],
		    [u_path_c62_slot_name, u_slot_path]
		  ],
		  [u_ob_c36_add1, u_self, u_slot_path, slot_value]
		],
		slot_value
	      ]).


% annotating U::OB$PREMOVE 
wl: arglist_info(u_ob_c36_premove,
		[u_self, u_slot_path, slot_value],
		[Self_Param, Slot_path_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_path, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_path, slot_value],
			 opt:0,
			 req:[u_self, u_slot_path, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PREMOVE 
wl: init_args(exact_only, u_ob_c36_premove).


% annotating U::OB$PREMOVE 
f_u_ob_c36_premove(Self_Param, Slot_path_Param, Slot_value_Param, Slot_value_Param) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_path, Slot_path_Param), bv(slot_value, Slot_value_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				
				[ #\(o),
				  #\(b),
				  #\($),
				  #\(p),
				  #\(r),
				  #\(e),
				  #\(m),
				  #\(o),
				  #\(v),
				  #\(e)
				]),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  f_u_path_c62_ob(Self_Param, Slot_path_Param, C36_remove1_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    f_u_ob_c36_remove1(C36_remove1_Param, Slot_name_Ret, TrueResult),
	    _127384=TrueResult
	;   f_u_ob_c36_add1(Self_Param,
			    Slot_path_Param,
			    Slot_value_Param,
			    ElseResult),
	    _127384=ElseResult
	).
:- set_opv(f_u_ob_c36_premove, classof, claz_function),
   set_opv(u_ob_c36_premove, compile_as, kw_function),
   set_opv(u_ob_c36_premove, function, f_u_ob_c36_premove),
   DefunResult=u_ob_c36_premove.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9896 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10167)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$gets ob slot-name):", 1, 10169)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9896 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10195)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9896 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Return all values of a slot.", 1, 10197)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9896 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10228)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$gets',
			    [self, 'slot-name'],
			    ['enforce-ob', self, '$STRING'("ob$gets")],
			    
			    [ if,
			      ['eq?', 'slot-name', [quote, obname]],
			      ['obr-obnames', self],
			      
			      [ yloop,
				
				[ initial,
				  [result, []],
				  [rest, ['obr-slots', self]]
				],
				[ywhile, rest],
				
				[ ydo,
				  
				  [ if,
				    
				    [ 'eq?',
				      'slot-name',
				      ['slots-name', [car, rest]]
				    ],
				    
				    [ setq,
				      result,
				      
				      [ 'append!',
					result,
					[list, ['slots-value', [car, rest]]]
				      ]
				    ]
				  ],
				  [setq, rest, [cdr, rest]]
				],
				[yresult, result]
			      ]
			    ]
			  ]).

% annotating U::OB$GETS 
wl: lambda_def(defun,
	      u_ob_c36_gets,
	      f_u_ob_c36_gets,
	      [u_self, u_slot_name],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(o), #\(b), #\($), #\(g), #\(e), #\(t), #\(s)])
		],
		
		[ if,
		  [u_eq_c63, u_slot_name, [quote, u_obname]],
		  [u_obr_obnames, u_self],
		  
		  [ u_yloop,
		    [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]],
		    [u_ywhile, rest],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]],
			
			[ setq,
			  u_result,
			  
			  [ u_append_c33,
			    u_result,
			    [list, [u_slots_value, [car, rest]]]
			  ]
			]
		      ],
		      [setq, rest, [cdr, rest]]
		    ],
		    [u_yresult, u_result]
		  ]
		]
	      ]).


% annotating U::OB$GETS 
wl: arglist_info(u_ob_c36_gets,
		[u_self, u_slot_name],
		[Self_Param, Slot_name_Param],
		arginfo{ all:[u_self, u_slot_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name],
			 opt:0,
			 req:[u_self, u_slot_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$GETS 
wl: init_args(exact_only, u_ob_c36_gets).


% annotating U::OB$GETS 
f_u_ob_c36_gets(Self_Param, Slot_name_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				[#\(o), #\(b), #\($), #\(g), #\(e), #\(t), #\(s)]),
		       Enforce_ob_Ret),
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  f_u_obr_obnames(Self_Param, TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]],
			[u_ywhile, rest],
			
			[ u_ydo,
			  
			  [ if,
			    [u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]],
			    
			    [ setq,
			      u_result,
			      
			      [ u_append_c33,
				u_result,
				[list, [u_slots_value, [car, rest]]]
			      ]
			    ]
			  ],
			  [setq, rest, [cdr, rest]]
			],
			[u_yresult, u_result]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ob_c36_gets, classof, claz_function),
   set_opv(u_ob_c36_gets, compile_as, kw_function),
   set_opv(u_ob_c36_gets, function, f_u_ob_c36_gets),
   DefunResult=u_ob_c36_gets.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10230 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10688)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$get-many ob slot-names):", 1, 10690)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10230 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10721)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10230 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Return values of several slots.",
				     1,
				     10723)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10230 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 10757)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10759 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$get-many',
			    [self, 'slot-names'],
			    ['enforce-ob', self, '$STRING'("ob$get-many")],
			    
			    [ yloop,
			      [initial, [result, []], [rest, ['obr-slots', self]]],
			      [ywhile, rest],
			      
			      [ ydo,
				
				[ if,
				  
				  [ 'memq?',
				    ['slots-name', [car, rest]],
				    'slot-names'
				  ],
				  
				  [ setq,
				    result,
				    
				    [ 'append!',
				      result,
				      [list, ['slots-value', [car, rest]]]
				    ]
				  ]
				],
				[setq, rest, [cdr, rest]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::OB$GET-MANY 
wl: lambda_def(defun,
	      u_ob_c36_get_many,
	      f_u_ob_c36_get_many,
	      [u_self, u_slot_names],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   
			   [ #\(o),
			     #\(b),
			     #\($),
			     #\(g),
			     #\(e),
			     #\(t),
			     #\(-),
			     #\(m),
			     #\(a),
			     #\(n),
			     #\(y)
			   ])
		],
		
		[ u_yloop,
		  [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]],
		  [u_ywhile, rest],
		  
		  [ u_ydo,
		    
		    [ if,
		      [u_memq_c63, [u_slots_name, [car, rest]], u_slot_names],
		      
		      [ setq,
			u_result,
			
			[ u_append_c33,
			  u_result,
			  [list, [u_slots_value, [car, rest]]]
			]
		      ]
		    ],
		    [setq, rest, [cdr, rest]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::OB$GET-MANY 
wl: arglist_info(u_ob_c36_get_many,
		[u_self, u_slot_names],
		[Self_Param, Slot_names_Param],
		arginfo{ all:[u_self, u_slot_names],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_names],
			 opt:0,
			 req:[u_self, u_slot_names],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$GET-MANY 
wl: init_args(exact_only, u_ob_c36_get_many).


% annotating U::OB$GET-MANY 
f_u_ob_c36_get_many(Self_Param, Slot_names_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_names, Slot_names_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				
				[ #\(o),
				  #\(b),
				  #\($),
				  #\(g),
				  #\(e),
				  #\(t),
				  #\(-),
				  #\(m),
				  #\(a),
				  #\(n),
				  #\(y)
				]),
		       Enforce_ob_Ret),
	f_u_yloop(
		  [ [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]],
		    [u_ywhile, rest],
		    
		    [ u_ydo,
		      
		      [ if,
			[u_memq_c63, [u_slots_name, [car, rest]], u_slot_names],
			
			[ setq,
			  u_result,
			  
			  [ u_append_c33,
			    u_result,
			    [list, [u_slots_value, [car, rest]]]
			  ]
			]
		      ],
		      [setq, rest, [cdr, rest]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_get_many, classof, claz_function),
   set_opv(u_ob_c36_get_many, compile_as, kw_function),
   set_opv(u_ob_c36_get_many, function, f_u_ob_c36_get_many),
   DefunResult=u_ob_c36_get_many.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11131 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$concatenate',
			    ['&rest', obs],
			    
			    [ yloop,
			      [initial, [result, ['ob$create-empty']]],
			      [yfor, ob, in, obs],
			      
			      [ ydo,
				
				[ yloop,
				  [yfor, sv, in, ['ob$pairs', ob]],
				  
				  [ ydo,
				    
				    [ 'ob$add1',
				      result,
				      ['slots-name', sv],
				      ['slots-value', sv]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::OB$CONCATENATE 
wl: lambda_def(defun,
	      u_ob_c36_concatenate,
	      f_u_ob_c36_concatenate,
	      [c38_rest, u_obs],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, [u_ob_c36_create_empty]]],
		  [u_yfor, u_ob, u_in, u_obs],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
		      
		      [ u_ydo,
			
			[ u_ob_c36_add1,
			  u_result,
			  [u_slots_name, u_sv],
			  [u_slots_value, u_sv]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::OB$CONCATENATE 
wl: arglist_info(u_ob_c36_concatenate,
		[c38_rest, u_obs],
		[u_obs],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_obs],
			 opt:0,
			 req:0,
			 rest:[u_obs],
			 sublists:0,
			 whole:0
		       }).


% annotating U::OB$CONCATENATE 
wl: init_args(0, u_ob_c36_concatenate).


% annotating U::OB$CONCATENATE 
f_u_ob_c36_concatenate(Obs_Param, FnResult) :-
	Env=[bv(u_obs, Obs_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, [u_ob_c36_create_empty]]],
		    [u_yfor, u_ob, u_in, u_obs],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
			
			[ u_ydo,
			  
			  [ u_ob_c36_add1,
			    u_result,
			    [u_slots_name, u_sv],
			    [u_slots_value, u_sv]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_concatenate, classof, claz_function),
   set_opv(u_ob_c36_concatenate, compile_as, kw_function),
   set_opv(u_ob_c36_concatenate, function, f_u_ob_c36_concatenate),
   DefunResult=u_ob_c36_concatenate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11377 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$concatenate!',
			    ['&rest', obs],
			    
			    [ yloop,
			      [initial, [result, [car, obs]]],
			      [yfor, ob, in, [cdr, obs]],
			      
			      [ ydo,
				
				[ yloop,
				  [yfor, sv, in, ['ob$pairs', ob]],
				  
				  [ ydo,
				    
				    [ 'ob$add1',
				      result,
				      ['slots-name', sv],
				      ['slots-value', sv]
				    ]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::OB$CONCATENATE! 
wl: lambda_def(defun,
	      u_ob_c36_concatenate_c33,
	      f_u_ob_c36_concatenate_c33,
	      [c38_rest, u_obs],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, [car, u_obs]]],
		  [u_yfor, u_ob, u_in, [cdr, u_obs]],
		  
		  [ u_ydo,
		    
		    [ u_yloop,
		      [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
		      
		      [ u_ydo,
			
			[ u_ob_c36_add1,
			  u_result,
			  [u_slots_name, u_sv],
			  [u_slots_value, u_sv]
			]
		      ]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::OB$CONCATENATE! 
wl: arglist_info(u_ob_c36_concatenate_c33,
		[c38_rest, u_obs],
		[u_obs],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[u_obs],
			 opt:0,
			 req:0,
			 rest:[u_obs],
			 sublists:0,
			 whole:0
		       }).


% annotating U::OB$CONCATENATE! 
wl: init_args(0, u_ob_c36_concatenate_c33).


% annotating U::OB$CONCATENATE! 
f_u_ob_c36_concatenate_c33(Obs_Param, FnResult) :-
	Env=[bv(u_obs, Obs_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, [car, u_obs]]],
		    [u_yfor, u_ob, u_in, [cdr, u_obs]],
		    
		    [ u_ydo,
		      
		      [ u_yloop,
			[u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]],
			
			[ u_ydo,
			  
			  [ u_ob_c36_add1,
			    u_result,
			    [u_slots_name, u_sv],
			    [u_slots_value, u_sv]
			  ]
			]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_concatenate_c33, classof, claz_function),
   set_opv(u_ob_c36_concatenate_c33, compile_as, kw_function),
   set_opv(u_ob_c36_concatenate_c33, function, f_u_ob_c36_concatenate_c33),
   DefunResult=u_ob_c36_concatenate_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11377 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 11623)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11377 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" ob$pairs: get all the slot-name slot-value pairs of an ob",
				     1,
				     11625)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11377 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 11685)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11686 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'ob$pairs', [self], ['obr-slots', self]]).

% annotating U::OB$PAIRS 
wl: lambda_def(defun,
	      u_ob_c36_pairs,
	      f_u_ob_c36_pairs,
	      [u_self],
	      [[u_obr_slots, u_self]]).


% annotating U::OB$PAIRS 
wl: arglist_info(u_ob_c36_pairs,
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

% annotating U::OB$PAIRS 
wl: init_args(exact_only, u_ob_c36_pairs).


% annotating U::OB$PAIRS 
f_u_ob_c36_pairs(Self_Param, FnResult) :-
	f_u_obr_slots(Self_Param, Obr_slots_Ret),
	Obr_slots_Ret=FnResult.
:- set_opv(f_u_ob_c36_pairs, classof, claz_function),
   set_opv(u_ob_c36_pairs, compile_as, kw_function),
   set_opv(u_ob_c36_pairs, function, f_u_ob_c36_pairs),
   DefunResult=u_ob_c36_pairs.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11728 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$slot-names',
			    [self],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [yfor, pair, in, ['ob$pairs', self]],
			      
			      [ ydo,
				
				[ if,
				  [not, ['memq?', [car, pair], result]],
				  
				  [ setq,
				    result,
				    [append, result, [list, [car, pair]]]
				  ]
				]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::OB$SLOT-NAMES 
wl: lambda_def(defun,
	      u_ob_c36_slot_names,
	      f_u_ob_c36_slot_names,
	      [u_self],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_yfor, u_pair, u_in, [u_ob_c36_pairs, u_self]],
		  
		  [ u_ydo,
		    
		    [ if,
		      [not, [u_memq_c63, [car, u_pair], u_result]],
		      [setq, u_result, [append, u_result, [list, [car, u_pair]]]]
		    ]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::OB$SLOT-NAMES 
wl: arglist_info(u_ob_c36_slot_names,
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

% annotating U::OB$SLOT-NAMES 
wl: init_args(exact_only, u_ob_c36_slot_names).


% annotating U::OB$SLOT-NAMES 
f_u_ob_c36_slot_names(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_yfor, u_pair, u_in, [u_ob_c36_pairs, u_self]],
		    
		    [ u_ydo,
		      
		      [ if,
			[not, [u_memq_c63, [car, u_pair], u_result]],
			[setq, u_result, [append, u_result, [list, [car, u_pair]]]]
		      ]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ob_c36_slot_names, classof, claz_function),
   set_opv(u_ob_c36_slot_names, compile_as, kw_function),
   set_opv(u_ob_c36_slot_names, function, f_u_ob_c36_slot_names),
   DefunResult=u_ob_c36_slot_names.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11954 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-into-obname',
			    [obj],
			    [if, ['ob?', obj], ['ob$name', obj], obj]
			  ]).

% annotating U::MAKE-INTO-OBNAME 
wl: lambda_def(defun,
	      u_make_into_obname,
	      f_u_make_into_obname,
	      [u_obj],
	      [[if, [u_ob_c63, u_obj], [u_ob_c36_name, u_obj], u_obj]]).


% annotating U::MAKE-INTO-OBNAME 
wl: arglist_info(u_make_into_obname,
		[u_obj],
		[Obj_Param],
		arginfo{ all:[u_obj],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_obj],
			 opt:0,
			 req:[u_obj],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAKE-INTO-OBNAME 
wl: init_args(exact_only, u_make_into_obname).


% annotating U::MAKE-INTO-OBNAME 
f_u_make_into_obname(Obj_Param, FnResult) :-
	Env=[bv(u_obj, Obj_Param)],
	f_u_ob_c63(u_obj, IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_name(Obj_Param, TrueResult),
	    FnResult=TrueResult
	;   FnResult=Obj_Param
	).
:- set_opv(f_u_make_into_obname, classof, claz_function),
   set_opv(u_make_into_obname, compile_as, kw_function),
   set_opv(u_make_into_obname, function, f_u_make_into_obname),
   DefunResult=u_make_into_obname.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12019 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$basic-add',
			    [self, 'slot-name', 'slot-value'],
			    
			    [ if,
			      ['eq?', 'slot-name', [quote, obname]],
			      ['ob$add-name', self, 'slot-value'],
			      
			      [ 'set-obr-slots',
				self,
				
				[ 'append!',
				  ['obr-slots', self],
				  [list, [list, 'slot-name', 'slot-value']]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$BASIC-ADD 
wl: lambda_def(defun,
	      u_ob_c36_basic_add,
	      f_u_ob_c36_basic_add,
	      [u_self, u_slot_name, slot_value],
	      
	      [ 
		[ if,
		  [u_eq_c63, u_slot_name, [quote, u_obname]],
		  [u_ob_c36_add_name, u_self, slot_value],
		  
		  [ u_set_obr_slots,
		    u_self,
		    
		    [ u_append_c33,
		      [u_obr_slots, u_self],
		      [list, [list, u_slot_name, slot_value]]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$BASIC-ADD 
wl: arglist_info(u_ob_c36_basic_add,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$BASIC-ADD 
wl: init_args(exact_only, u_ob_c36_basic_add).


% annotating U::OB$BASIC-ADD 
f_u_ob_c36_basic_add(Self_Param, Slot_name_Param, Slot_value_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Param)],
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_add_name(Self_Param, Slot_value_Param, TrueResult),
	    FnResult=TrueResult
	;   f_u_set_obr_slots(u_self,
			      
			      [ u_append_c33,
				[u_obr_slots, u_self],
				[list, [list, u_slot_name, slot_value]]
			      ],
			      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ob_c36_basic_add, classof, claz_function),
   set_opv(u_ob_c36_basic_add, compile_as, kw_function),
   set_opv(u_ob_c36_basic_add, function, f_u_ob_c36_basic_add),
   DefunResult=u_ob_c36_basic_add.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12260 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'ob$literal?', [self], ['obr-literal', self]]).

% annotating U::OB$LITERAL? 
wl: lambda_def(defun,
	      u_ob_c36_literal_c63,
	      f_u_ob_c36_literal_c63,
	      [u_self],
	      [[u_obr_literal, u_self]]).


% annotating U::OB$LITERAL? 
wl: arglist_info(u_ob_c36_literal_c63,
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

% annotating U::OB$LITERAL? 
wl: init_args(exact_only, u_ob_c36_literal_c63).


% annotating U::OB$LITERAL? 
f_u_ob_c36_literal_c63(Self_Param, FnResult) :-
	f_u_obr_literal(Self_Param, Obr_literal_Ret),
	Obr_literal_Ret=FnResult.
:- set_opv(f_u_ob_c36_literal_c63, classof, claz_function),
   set_opv(u_ob_c36_literal_c63, compile_as, kw_function),
   set_opv(u_ob_c36_literal_c63, function, f_u_ob_c36_literal_c63),
   DefunResult=u_ob_c36_literal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12307 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$set-literal',
			    [self, val],
			    ['set-obr-literal', self, val]
			  ]).

% annotating U::OB$SET-LITERAL 
wl: lambda_def(defun,
	      u_ob_c36_set_literal,
	      f_u_ob_c36_set_literal,
	      [u_self, u_val],
	      [[u_set_obr_literal, u_self, u_val]]).


% annotating U::OB$SET-LITERAL 
wl: arglist_info(u_ob_c36_set_literal,
		[u_self, u_val],
		[Self_Param, Val_Param],
		arginfo{ all:[u_self, u_val],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_val],
			 opt:0,
			 req:[u_self, u_val],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$SET-LITERAL 
wl: init_args(exact_only, u_ob_c36_set_literal).


% annotating U::OB$SET-LITERAL 
f_u_ob_c36_set_literal(Self_Param, Val_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_val, Val_Param)],
	f_u_set_obr_literal(u_self, u_val, Val),
	Val=FnResult.
:- set_opv(f_u_ob_c36_set_literal, classof, claz_function),
   set_opv(u_ob_c36_set_literal, compile_as, kw_function),
   set_opv(u_ob_c36_set_literal, function, f_u_ob_c36_set_literal),
   DefunResult=u_ob_c36_set_literal.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$add1',
			    [self, 'slot-name', 'slot-value'],
			    ['ob$basic-add', self, 'slot-name', 'slot-value'],
			    
			    [ if,
			      '*inverse-setting?*',
			      
			      [ let,
				[[inv, ['inverse-slot', 'slot-name']]],
				
				[ if,
				  [and, inv, ['ob?', 'slot-value']],
				  ['ob$basic-add', 'slot-value', inv, self]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$ADD1 
wl: lambda_def(defun,
	      u_ob_c36_add1,
	      f_u_ob_c36_add1,
	      [u_self, u_slot_name, slot_value],
	      
	      [ [u_ob_c36_basic_add, u_self, u_slot_name, slot_value],
		
		[ if,
		  u_xx_inverse_setting_c63_xx,
		  
		  [ let,
		    [[u_inv, [u_inverse_slot, u_slot_name]]],
		    
		    [ if,
		      [and, u_inv, [u_ob_c63, slot_value]],
		      [u_ob_c36_basic_add, slot_value, u_inv, u_self]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$ADD1 
wl: arglist_info(u_ob_c36_add1,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$ADD1 
wl: init_args(exact_only, u_ob_c36_add1).


% annotating U::OB$ADD1 
f_u_ob_c36_add1(Self_Param, Slot_name_Param, Slot_value_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Param)],
	f_u_ob_c36_basic_add(Self_Param,
			     Slot_name_Param,
			     Slot_value_Param,
			     Basic_add_Ret),
	get_var(Env, u_xx_inverse_setting_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_inverse_slot(Slot_name_Param, Inv_Init),
	    LEnv=[[bv(u_inv, Inv_Init)]|Env],
	    get_var(LEnv, u_inv, IFTEST28),
	    (   IFTEST28\==[]
	    ->  f_u_ob_c63(slot_value, TrueResult),
		IFTEST26=TrueResult
	    ;   IFTEST26=[]
	    ),
	    (   IFTEST26\==[]
	    ->  get_var(LEnv, u_inv, Inv_Get34),
		f_u_ob_c36_basic_add(Slot_value_Param,
				     Inv_Get34,
				     Self_Param,
				     TrueResult36),
		FnResult=TrueResult36
	    ;   FnResult=[]
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_ob_c36_add1, classof, claz_function),
   set_opv(u_ob_c36_add1, compile_as, kw_function),
   set_opv(u_ob_c36_add1, function, f_u_ob_c36_add1),
   DefunResult=u_ob_c36_add1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If slot-name has an inverse and slot-value is a ob,",
				     10,
				     12569)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" perform inverse setting", 10, 12632)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 12707)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$get ob slot-name):", 1, 12709)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 12734)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Return a unique value of a slot. If there is more than one value for the",
				     1,
				     12736)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" slot, an arbitrary one is returned.",
				     1,
				     12811)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 12849)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12851 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$get',
			    [self, 'slot-name'],
			    ['enforce-ob', self, '$STRING'("ob$get")],
			    
			    [ if,
			      ['eq?', 'slot-name', [quote, obname]],
			      [car, ['obr-obnames', self]],
			      
			      [ let,
				
				[ 
				  [ found,
				    [assq, 'slot-name', ['obr-slots', self]]
				  ]
				],
				[if, found, ['slots-value', found], []]
			      ]
			    ]
			  ]).

% annotating U::OB$GET 
wl: lambda_def(defun,
	      u_ob_c36_get,
	      f_u_ob_c36_get,
	      [u_self, u_slot_name],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(o), #\(b), #\($), #\(g), #\(e), #\(t)])
		],
		
		[ if,
		  [u_eq_c63, u_slot_name, [quote, u_obname]],
		  [car, [u_obr_obnames, u_self]],
		  
		  [ let,
		    [[u_found, [ext_assq, u_slot_name, [u_obr_slots, u_self]]]],
		    [if, u_found, [u_slots_value, u_found], []]
		  ]
		]
	      ]).


% annotating U::OB$GET 
wl: arglist_info(u_ob_c36_get,
		[u_self, u_slot_name],
		[Self_Param, Slot_name_Param],
		arginfo{ all:[u_self, u_slot_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name],
			 opt:0,
			 req:[u_self, u_slot_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$GET 
wl: init_args(exact_only, u_ob_c36_get).


% annotating U::OB$GET 
f_u_ob_c36_get(Self_Param, Slot_name_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				[#\(o), #\(b), #\($), #\(g), #\(e), #\(t)]),
		       Enforce_ob_Ret),
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  f_u_obr_obnames(Self_Param, Car_Param),
	    cl_car(Car_Param, TrueResult25),
	    FnResult=TrueResult25
	;   f_ext_assq(u_slot_name, [u_obr_slots, u_self], Found_Init),
	    LEnv=[[bv(u_found, Found_Init)]|Env],
	    get_var(LEnv, u_found, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_slots_value(u_found, TrueResult),
		FnResult=TrueResult
	    ;   FnResult=[]
	    )
	).
:- set_opv(f_u_ob_c36_get, classof, claz_function),
   set_opv(u_ob_c36_get, compile_as, kw_function),
   set_opv(u_ob_c36_get, function, f_u_ob_c36_get),
   DefunResult=u_ob_c36_get.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$pget',
			    [self, 'slot-path'],
			    ['enforce-ob', self, '$STRING'("ob$pget")],
			    
			    [ if,
			      ['pair?', 'slot-path'],
			      
			      [ 'ob$get',
				['path->ob', self, 'slot-path'],
				['path->slot-name', 'slot-path']
			      ],
			      
			      [ if,
				['eq?', 'slot-path', [quote, obname]],
				[car, ['obr-obnames', self]],
				
				[ let,
				  
				  [ 
				    [ found,
				      [assq, 'slot-path', ['obr-slots', self]]
				    ]
				  ],
				  [if, found, ['slots-value', found], []]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$PGET 
wl: lambda_def(defun,
	      u_ob_c36_pget,
	      f_u_ob_c36_pget,
	      [u_self, u_slot_path],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(o), #\(b), #\($), #\(p), #\(g), #\(e), #\(t)])
		],
		
		[ if,
		  [u_pair_c63, u_slot_path],
		  
		  [ u_ob_c36_get,
		    [u_path_c62_ob, u_self, u_slot_path],
		    [u_path_c62_slot_name, u_slot_path]
		  ],
		  
		  [ if,
		    [u_eq_c63, u_slot_path, [quote, u_obname]],
		    [car, [u_obr_obnames, u_self]],
		    
		    [ let,
		      [[u_found, [ext_assq, u_slot_path, [u_obr_slots, u_self]]]],
		      [if, u_found, [u_slots_value, u_found], []]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$PGET 
wl: arglist_info(u_ob_c36_pget,
		[u_self, u_slot_path],
		[Self_Param, Slot_path_Param],
		arginfo{ all:[u_self, u_slot_path],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_path],
			 opt:0,
			 req:[u_self, u_slot_path],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PGET 
wl: init_args(exact_only, u_ob_c36_pget).


% annotating U::OB$PGET 
f_u_ob_c36_pget(Self_Param, Slot_path_Param, ElseResult32) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_path, Slot_path_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				[#\(o), #\(b), #\($), #\(p), #\(g), #\(e), #\(t)]),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  f_u_path_c62_ob(Self_Param, Slot_path_Param, C36_get_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    f_u_ob_c36_get(C36_get_Param, Slot_name_Ret, TrueResult31),
	    ElseResult32=TrueResult31
	;   f_u_eq_c63(u_slot_path, [quote, u_obname], IFTEST18),
	    (   IFTEST18\==[]
	    ->  f_u_obr_obnames(Self_Param, Car_Param),
		cl_car(Car_Param, TrueResult29),
		ElseResult32=TrueResult29
	    ;   f_ext_assq(u_slot_path, [u_obr_slots, u_self], Found_Init),
		LEnv=[[bv(u_found, Found_Init)]|Env],
		get_var(LEnv, u_found, IFTEST24),
		(   IFTEST24\==[]
		->  f_u_slots_value(u_found, TrueResult),
		    ElseResult32=TrueResult
		;   ElseResult32=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_pget, classof, claz_function),
   set_opv(u_ob_c36_pget, compile_as, kw_function),
   set_opv(u_ob_c36_pget, function, f_u_ob_c36_pget),
   DefunResult=u_ob_c36_pget.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13411)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ob$set ob slot-name slot-value):",
				     1,
				     13413)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13449)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If it is desired to restrict slot values to a unique entry, this",
				     1,
				     13451)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" method removes all slot values from a slot, then sets the unique entry.",
				     1,
				     13518)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Inverses are affected similarly.",
				     1,
				     13592)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 13627)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13629 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$set',
			    [self, 'slot-name', 'slot-value'],
			    ['enforce-ob', self, '$STRING'("ob$set")],
			    
			    [ if,
			      ['eq?', 'slot-name', [quote, obname]],
			      
			      [ progn,
				['ob$add-name', self, 'slot-value'],
				'slot-value'
			      ],
			      
			      [ yloop,
				
				[ initial,
				  [values, ['ob$gets', self, 'slot-name']]
				],
				[ywhile, values],
				
				[ ydo,
				  ['ob$remove1', self, 'slot-name', [car, values]],
				  [setq, values, [cdr, values]]
				],
				
				[ yresult,
				  ['ob$add1', self, 'slot-name', 'slot-value'],
				  'slot-value'
				]
			      ]
			    ]
			  ]).

% annotating U::OB$SET 
wl: lambda_def(defun,
	      u_ob_c36_set,
	      f_u_ob_c36_set,
	      [u_self, u_slot_name, slot_value],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(o), #\(b), #\($), #\(s), #\(e), #\(t)])
		],
		
		[ if,
		  [u_eq_c63, u_slot_name, [quote, u_obname]],
		  [progn, [u_ob_c36_add_name, u_self, slot_value], slot_value],
		  
		  [ u_yloop,
		    [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_name]]],
		    [u_ywhile, values],
		    
		    [ u_ydo,
		      [u_ob_c36_remove1, u_self, u_slot_name, [car, values]],
		      [setq, values, [cdr, values]]
		    ],
		    
		    [ u_yresult,
		      [u_ob_c36_add1, u_self, u_slot_name, slot_value],
		      slot_value
		    ]
		  ]
		]
	      ]).


% annotating U::OB$SET 
wl: arglist_info(u_ob_c36_set,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Get20],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$SET 
wl: init_args(exact_only, u_ob_c36_set).


% annotating U::OB$SET 
f_u_ob_c36_set(Self_Param, Slot_name_Param, Slot_value_Get20, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Get20)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				[#\(o), #\(b), #\($), #\(s), #\(e), #\(t)]),
		       Enforce_ob_Ret),
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_add_name(Self_Param, Slot_value_Get20, Add_name_Ret),
	    FnResult=Slot_value_Get20
	;   f_u_yloop(
		      [ 
			[ u_initial,
			  [values, [u_ob_c36_gets, u_self, u_slot_name]]
			],
			[u_ywhile, values],
			
			[ u_ydo,
			  [u_ob_c36_remove1, u_self, u_slot_name, [car, values]],
			  [setq, values, [cdr, values]]
			],
			
			[ u_yresult,
			  [u_ob_c36_add1, u_self, u_slot_name, slot_value],
			  slot_value
			]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ob_c36_set, classof, claz_function),
   set_opv(u_ob_c36_set, compile_as, kw_function),
   set_opv(u_ob_c36_set, function, f_u_ob_c36_set),
   DefunResult=u_ob_c36_set.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$pset',
			    [self, 'slot-path', 'slot-value'],
			    ['enforce-ob', self, '$STRING'("ob$pset")],
			    
			    [ if,
			      ['pair?', 'slot-path'],
			      
			      [ 'ob$set',
				['path->ob', self, 'slot-path'],
				['path->slot-name', 'slot-path'],
				'slot-value'
			      ],
			      
			      [ yloop,
				
				[ initial,
				  [values, ['ob$gets', self, 'slot-path']]
				],
				[ywhile, values],
				
				[ ydo,
				  ['ob$remove1', self, 'slot-path', [car, values]],
				  [setq, values, [cdr, values]]
				],
				
				[ yresult,
				  ['ob$add1', self, 'slot-path', 'slot-value']
				]
			      ]
			    ],
			    'slot-value'
			  ]).

% annotating U::OB$PSET 
wl: lambda_def(defun,
	      u_ob_c36_pset,
	      f_u_ob_c36_pset,
	      [u_self, u_slot_path, slot_value],
	      
	      [ 
		[ u_enforce_ob,
		  u_self,
		  '$ARRAY'([*],
			   claz_base_character,
			   [#\(o), #\(b), #\($), #\(p), #\(s), #\(e), #\(t)])
		],
		
		[ if,
		  [u_pair_c63, u_slot_path],
		  
		  [ u_ob_c36_set,
		    [u_path_c62_ob, u_self, u_slot_path],
		    [u_path_c62_slot_name, u_slot_path],
		    slot_value
		  ],
		  
		  [ u_yloop,
		    [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_path]]],
		    [u_ywhile, values],
		    
		    [ u_ydo,
		      [u_ob_c36_remove1, u_self, u_slot_path, [car, values]],
		      [setq, values, [cdr, values]]
		    ],
		    [u_yresult, [u_ob_c36_add1, u_self, u_slot_path, slot_value]]
		  ]
		],
		slot_value
	      ]).


% annotating U::OB$PSET 
wl: arglist_info(u_ob_c36_pset,
		[u_self, u_slot_path, slot_value],
		[Self_Param, Slot_path_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_path, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_path, slot_value],
			 opt:0,
			 req:[u_self, u_slot_path, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$PSET 
wl: init_args(exact_only, u_ob_c36_pset).


% annotating U::OB$PSET 
f_u_ob_c36_pset(Self_Param, Slot_path_Param, Slot_value_Param, Slot_value_Param) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_path, Slot_path_Param), bv(slot_value, Slot_value_Param)],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*],
				claz_base_character,
				[#\(o), #\(b), #\($), #\(p), #\(s), #\(e), #\(t)]),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  f_u_path_c62_ob(Self_Param, Slot_path_Param, C36_set_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    f_u_ob_c36_set(C36_set_Param,
			   Slot_name_Ret,
			   Slot_value_Param,
			   TrueResult),
	    _128946=TrueResult
	;   f_u_yloop(
		      [ 
			[ u_initial,
			  [values, [u_ob_c36_gets, u_self, u_slot_path]]
			],
			[u_ywhile, values],
			
			[ u_ydo,
			  [u_ob_c36_remove1, u_self, u_slot_path, [car, values]],
			  [setq, values, [cdr, values]]
			],
			
			[ u_yresult,
			  [u_ob_c36_add1, u_self, u_slot_path, slot_value]
			]
		      ],
		      ElseResult),
	    _128946=ElseResult
	).
:- set_opv(f_u_ob_c36_pset, classof, claz_function),
   set_opv(u_ob_c36_pset, compile_as, kw_function),
   set_opv(u_ob_c36_pset, function, f_u_ob_c36_pset),
   DefunResult=u_ob_c36_pset.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14068 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" This function is never used?", 1, 14546)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14576 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$set1',
			    [self, 'slot-name', 'slot-value'],
			    
			    [ yloop,
			      [initial, [values, ['ob$gets', self, 'slot-name']]],
			      [ywhile, values],
			      
			      [ ydo,
				['ob$remove1', self, 'slot-name', [car, values]],
				[setq, values, [cdr, values]]
			      ],
			      
			      [ yresult,
				['ob$add1', self, 'slot-name', 'slot-value']
			      ]
			    ],
			    'slot-value'
			  ]).

% annotating U::OB$SET1 
wl: lambda_def(defun,
	      u_ob_c36_set1,
	      f_u_ob_c36_set1,
	      [u_self, u_slot_name, slot_value],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_name]]],
		  [u_ywhile, values],
		  
		  [ u_ydo,
		    [u_ob_c36_remove1, u_self, u_slot_name, [car, values]],
		    [setq, values, [cdr, values]]
		  ],
		  [u_yresult, [u_ob_c36_add1, u_self, u_slot_name, slot_value]]
		],
		slot_value
	      ]).


% annotating U::OB$SET1 
wl: arglist_info(u_ob_c36_set1,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$SET1 
wl: init_args(exact_only, u_ob_c36_set1).


% annotating U::OB$SET1 
f_u_ob_c36_set1(Self_Param, Slot_name_Param, Slot_value_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Param)],
	f_u_yloop(
		  [ [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_name]]],
		    [u_ywhile, values],
		    
		    [ u_ydo,
		      [u_ob_c36_remove1, u_self, u_slot_name, [car, values]],
		      [setq, values, [cdr, values]]
		    ],
		    [u_yresult, [u_ob_c36_add1, u_self, u_slot_name, slot_value]]
		  ],
		  Yloop_Ret),
	Slot_value_Param=FnResult.
:- set_opv(f_u_ob_c36_set1, classof, claz_function),
   set_opv(u_ob_c36_set1, compile_as, kw_function),
   set_opv(u_ob_c36_set1, function, f_u_ob_c36_set1),
   DefunResult=u_ob_c36_set1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14860 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$removes',
			    [self, 'slot-name'],
			    ['ob$set', self, 'slot-name', []],
			    self
			  ]).

% annotating U::OB$REMOVES 
wl: lambda_def(defun,
	      u_ob_c36_removes,
	      f_u_ob_c36_removes,
	      [u_self, u_slot_name],
	      [[u_ob_c36_set, u_self, u_slot_name, []], u_self]).


% annotating U::OB$REMOVES 
wl: arglist_info(u_ob_c36_removes,
		[u_self, u_slot_name],
		[Self_Param, Slot_name_Param],
		arginfo{ all:[u_self, u_slot_name],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name],
			 opt:0,
			 req:[u_self, u_slot_name],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$REMOVES 
wl: init_args(exact_only, u_ob_c36_removes).


% annotating U::OB$REMOVES 
f_u_ob_c36_removes(Self_Param, Slot_name_Param, FnResult) :-
	f_u_ob_c36_set(Self_Param, Slot_name_Param, [], C36_set_Ret),
	Self_Param=FnResult.
:- set_opv(f_u_ob_c36_removes, classof, claz_function),
   set_opv(u_ob_c36_removes, compile_as, kw_function),
   set_opv(u_ob_c36_removes, function, f_u_ob_c36_removes),
   DefunResult=u_ob_c36_removes.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14860 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("\n(defun ob$removes (self slot-name)\n  (map 'list\n   (lambda (slot)\n    (if (eq? (slots-name slot) slot-name)\n        (ob$remove1 self (slots-name slot)\n                   (slots-value slot))))\n   (obr-slots self))\n  self)\n",
				     2,
				     14931)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15157 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$remove-all',
			    [self],
			    
			    [ map,
			      [quote, list],
			      
			      [ lambda,
				[slot],
				
				[ 'ob$remove1',
				  self,
				  ['slots-name', slot],
				  ['slots-value', slot]
				]
			      ],
			      ['obr-slots', self]
			    ],
			    self
			  ]).

% annotating U::OB$REMOVE-ALL 
wl: lambda_def(defun,
	      u_ob_c36_remove_all,
	      f_u_ob_c36_remove_all,
	      [u_self],
	      
	      [ 
		[ map,
		  [quote, list],
		  
		  [ lambda,
		    [u_slot],
		    
		    [ u_ob_c36_remove1,
		      u_self,
		      [u_slots_name, u_slot],
		      [u_slots_value, u_slot]
		    ]
		  ],
		  [u_obr_slots, u_self]
		],
		u_self
	      ]).


% annotating U::OB$REMOVE-ALL 
wl: arglist_info(u_ob_c36_remove_all,
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

% annotating U::OB$REMOVE-ALL 
wl: init_args(exact_only, u_ob_c36_remove_all).


% annotating U::OB$REMOVE-ALL 
f_u_ob_c36_remove_all(Self_Param, FnResult) :-
	Env=[bv(u_self, Self_Param)],
	Lambda=closure([Env13|Env], LResult, [u_slot],  (f_u_slots_name(u_slot, Slots_name_Ret), f_u_slots_value(u_slot, Slots_value_Ret), f_u_ob_c36_remove1(Self_Param, Slots_name_Ret, Slots_value_Ret, LResult))),
	f_u_obr_slots(Self_Param, Obr_slots_Ret),
	cl_map(list, Lambda, Obr_slots_Ret, Map_Ret),
	Self_Param=FnResult.
:- set_opv(f_u_ob_c36_remove_all, classof, claz_function),
   set_opv(u_ob_c36_remove_all, compile_as, kw_function),
   set_opv(u_ob_c36_remove_all, function, f_u_ob_c36_remove_all),
   DefunResult=u_ob_c36_remove_all.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15321 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$remove1',
			    [self, 'slot-name', 'slot-value'],
			    ['ob$basic-remove', self, 'slot-name', 'slot-value'],
			    
			    [ if,
			      '*inverse-setting?*',
			      
			      [ let,
				[[inv, ['inverse-slot', 'slot-name']]],
				
				[ if,
				  [and, inv, ['ob?', 'slot-value']],
				  ['ob$basic-remove', 'slot-value', inv, self]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$REMOVE1 
wl: lambda_def(defun,
	      u_ob_c36_remove1,
	      f_u_ob_c36_remove1,
	      [u_self, u_slot_name, slot_value],
	      
	      [ [u_ob_c36_basic_remove, u_self, u_slot_name, slot_value],
		
		[ if,
		  u_xx_inverse_setting_c63_xx,
		  
		  [ let,
		    [[u_inv, [u_inverse_slot, u_slot_name]]],
		    
		    [ if,
		      [and, u_inv, [u_ob_c63, slot_value]],
		      [u_ob_c36_basic_remove, slot_value, u_inv, u_self]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$REMOVE1 
wl: arglist_info(u_ob_c36_remove1,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$REMOVE1 
wl: init_args(exact_only, u_ob_c36_remove1).


% annotating U::OB$REMOVE1 
f_u_ob_c36_remove1(Self_Param, Slot_name_Param, Slot_value_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Param)],
	f_u_ob_c36_basic_remove(Self_Param,
				Slot_name_Param,
				Slot_value_Param,
				Basic_remove_Ret),
	get_var(Env, u_xx_inverse_setting_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_inverse_slot(Slot_name_Param, Inv_Init),
	    LEnv=[[bv(u_inv, Inv_Init)]|Env],
	    get_var(LEnv, u_inv, IFTEST28),
	    (   IFTEST28\==[]
	    ->  f_u_ob_c63(slot_value, TrueResult),
		IFTEST26=TrueResult
	    ;   IFTEST26=[]
	    ),
	    (   IFTEST26\==[]
	    ->  get_var(LEnv, u_inv, Inv_Get34),
		f_u_ob_c36_basic_remove(Slot_value_Param,
					Inv_Get34,
					Self_Param,
					TrueResult36),
		FnResult=TrueResult36
	    ;   FnResult=[]
	    )
	;   FnResult=[]
	).
:- set_opv(f_u_ob_c36_remove1, classof, claz_function),
   set_opv(u_ob_c36_remove1, compile_as, kw_function),
   set_opv(u_ob_c36_remove1, function, f_u_ob_c36_remove1),
   DefunResult=u_ob_c36_remove1.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15321 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" If slot-name has an inverse and slot-value is an ob,",
				     9,
				     15523)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15321 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" perform inverse removal.", 9, 15586)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15663 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ob$basic-remove',
			    [self, 'slot-name', 'slot-value'],
			    
			    [ if,
			      ['eq?', 'slot-name', [quote, obname]],
			      ['ob$remove-name', self, 'slot-value'],
			      
			      [ yloop,
				[initial, [rest, ['obr-slots', self]], [found, []]],
				[ywhile, rest],
				[yuntil, found],
				
				[ ydo,
				  
				  [ if,
				    
				    [ and,
				      
				      [ 'eq?',
					'slot-name',
					['slots-name', [car, rest]]
				      ],
				      
				      [ 'eq?',
					'slot-value',
					['slots-value', [car, rest]]
				      ]
				    ],
				    
				    [ progn,
				      [setq, found, t],
				      
				      [ 'set-obr-slots',
					self,
					
					[ 'delq!',
					  [car, rest],
					  ['obr-slots', self]
					]
				      ]
				    ]
				  ],
				  [setq, rest, [cdr, rest]]
				],
				
				[ yresult,
				  
				  [ if,
				    ['null?', found],
				    
				    [ progn,
				      
				      [ error,
					'$STRING'("~A slot of ~A has no ~A value."),
					'slot-name',
					self,
					'slot-value'
				      ],
				      []
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::OB$BASIC-REMOVE 
wl: lambda_def(defun,
	      u_ob_c36_basic_remove,
	      f_u_ob_c36_basic_remove,
	      [u_self, u_slot_name, slot_value],
	      
	      [ 
		[ if,
		  [u_eq_c63, u_slot_name, [quote, u_obname]],
		  [u_ob_c36_remove_name, u_self, slot_value],
		  
		  [ u_yloop,
		    [u_initial, [rest, [u_obr_slots, u_self]], [u_found, []]],
		    [u_ywhile, rest],
		    [u_yuntil, u_found],
		    
		    [ u_ydo,
		      
		      [ if,
			
			[ and,
			  [u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]],
			  [u_eq_c63, slot_value, [u_slots_value, [car, rest]]]
			],
			
			[ progn,
			  [setq, u_found, t],
			  
			  [ u_set_obr_slots,
			    u_self,
			    [u_delq_c33, [car, rest], [u_obr_slots, u_self]]
			  ]
			]
		      ],
		      [setq, rest, [cdr, rest]]
		    ],
		    
		    [ u_yresult,
		      
		      [ if,
			[u_null_c63, u_found],
			
			[ progn,
			  
			  [ error,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\(~),
				       #\('A'),
				       #\(' '),
				       #\(s),
				       #\(l),
				       #\(o),
				       #\(t),
				       #\(' '),
				       #\(o),
				       #\(f),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(h),
				       #\(a),
				       #\(s),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(v),
				       #\(a),
				       #\(l),
				       #\(u),
				       #\(e),
				       #\('.')
				     ]),
			    u_slot_name,
			    u_self,
			    slot_value
			  ],
			  []
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::OB$BASIC-REMOVE 
wl: arglist_info(u_ob_c36_basic_remove,
		[u_self, u_slot_name, slot_value],
		[Self_Param, Slot_name_Param, Slot_value_Param],
		arginfo{ all:[u_self, u_slot_name, slot_value],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_self, u_slot_name, slot_value],
			 opt:0,
			 req:[u_self, u_slot_name, slot_value],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::OB$BASIC-REMOVE 
wl: init_args(exact_only, u_ob_c36_basic_remove).


% annotating U::OB$BASIC-REMOVE 
f_u_ob_c36_basic_remove(Self_Param, Slot_name_Param, Slot_value_Param, FnResult) :-
	Env=[bv(u_self, Self_Param), bv(u_slot_name, Slot_name_Param), bv(slot_value, Slot_value_Param)],
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  f_u_ob_c36_remove_name(Self_Param, Slot_value_Param, TrueResult),
	    FnResult=TrueResult
	;   f_u_yloop(
		      [ [u_initial, [rest, [u_obr_slots, u_self]], [u_found, []]],
			[u_ywhile, rest],
			[u_yuntil, u_found],
			
			[ u_ydo,
			  
			  [ if,
			    
			    [ and,
			      [u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]],
			      [u_eq_c63, slot_value, [u_slots_value, [car, rest]]]
			    ],
			    
			    [ progn,
			      [setq, u_found, t],
			      
			      [ u_set_obr_slots,
				u_self,
				[u_delq_c33, [car, rest], [u_obr_slots, u_self]]
			      ]
			    ]
			  ],
			  [setq, rest, [cdr, rest]]
			],
			
			[ u_yresult,
			  
			  [ if,
			    [u_null_c63, u_found],
			    
			    [ progn,
			      
			      [ error,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\(~),
					   #\('A'),
					   #\(' '),
					   #\(s),
					   #\(l),
					   #\(o),
					   #\(t),
					   #\(' '),
					   #\(o),
					   #\(f),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(h),
					   #\(a),
					   #\(s),
					   #\(' '),
					   #\(n),
					   #\(o),
					   #\(' '),
					   #\(~),
					   #\('A'),
					   #\(' '),
					   #\(v),
					   #\(a),
					   #\(l),
					   #\(u),
					   #\(e),
					   #\('.')
					 ]),
				u_slot_name,
				u_self,
				slot_value
			      ],
			      []
			    ]
			  ]
			]
		      ],
		      ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_ob_c36_basic_remove, classof, claz_function),
   set_opv(u_ob_c36_basic_remove, compile_as, kw_function),
   set_opv(u_ob_c36_basic_remove, function, f_u_ob_c36_basic_remove),
   DefunResult=u_ob_c36_basic_remove.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15663 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 16383)).
:- true.


% Total time: 7.025 seconds


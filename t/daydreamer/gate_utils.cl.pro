
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "gate_utils" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:15:34 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" 10/13/84: A few utility functions and other initialization",
				     1,
				     202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:262 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   1/6/86: Added new variable syntax",
				     1,
				     263)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:300 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("  9/27/86: Removed flavors", 1, 301)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:328 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 329)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:330 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     331)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:411 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'make-typed-var',
			    [name],
			    
			    [ cond,
			      
			      [ ['eq?', name, '*question-mark-atom*'],
				['make-var', [], []]
			      ],
			      
			      [ ['eq?', name, [quote, self]],
				['make-var', name, '*person-ob*']
			      ],
			      
			      [ ['eq?', name, [quote, other]],
				['make-var', name, '*person-ob*']
			      ],
			      
			      [ else,
				
				[ 'let*',
				  
				  [ [str, ['symbol->string', name]],
				    [len, ['string-length', str]],
				    ['last-char', [nthchar, str, [-, len, 1]]]
				  ],
				  
				  [ if,
				    ['digit?', 'last-char', 10],
				    
				    [ 'make-var',
				      name,
				      
				      [ 'ob$name->ob',
					
					[ 'string->symbol',
					  ['string-slice', str, 0, [-, len, 1]]
					]
				      ]
				    ],
				    ['make-var', name, ['ob$name->ob', name]]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::MAKE-TYPED-VAR 
wl: lambda_def(defun,
	      u_make_typed_var,
	      f_u_make_typed_var,
	      [sys_name],
	      
	      [ 
		[ cond,
		  
		  [ [u_eq_c63, sys_name, u_xx_question_mark_atom_xx],
		    [u_make_var, [], []]
		  ],
		  
		  [ [u_eq_c63, sys_name, [quote, u_self]],
		    [u_make_var, sys_name, u_xx_person_ob_xx]
		  ],
		  
		  [ [u_eq_c63, sys_name, [quote, u_other]],
		    [u_make_var, sys_name, u_xx_person_ob_xx]
		  ],
		  
		  [ u_else,
		    
		    [ let_xx,
		      
		      [ [u_str, [u_symbol_c62_string, sys_name]],
			[u_len, [u_string_length, u_str]],
			[u_last_char, [u_nthchar, u_str, [-, u_len, 1]]]
		      ],
		      
		      [ if,
			[u_digit_c63, u_last_char, 10],
			
			[ u_make_var,
			  sys_name,
			  
			  [ u_ob_c36_name_c62_ob,
			    
			    [ u_string_c62_symbol,
			      [u_string_slice, u_str, 0, [-, u_len, 1]]
			    ]
			  ]
			],
			[u_make_var, sys_name, [u_ob_c36_name_c62_ob, sys_name]]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::MAKE-TYPED-VAR 
wl: arglist_info(u_make_typed_var,
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

% annotating U::MAKE-TYPED-VAR 
wl: init_args(exact_only, u_make_typed_var).


% annotating U::MAKE-TYPED-VAR 
f_u_make_typed_var(Name_Param, ElseResult47) :-
	Env=[bv(sys_name, Name_Param)],
	f_u_eq_c63(sys_name, u_xx_question_mark_atom_xx, IFTEST),
	(   IFTEST\==[]
	->  f_u_make_var([], [], TrueResult50),
	    ElseResult47=TrueResult50
	;   f_u_eq_c63(sys_name, [quote, u_self], IFTEST14),
	    (   IFTEST14\==[]
	    ->  get_var(Env, u_xx_person_ob_xx, Xx_person_ob_xx_Get),
		f_u_make_var(Name_Param, Xx_person_ob_xx_Get, TrueResult48),
		ElseResult47=TrueResult48
	    ;   f_u_eq_c63(sys_name, [quote, u_other], IFTEST18),
		(   IFTEST18\==[]
		->  get_var(Env, u_xx_person_ob_xx, Xx_person_ob_xx_Get21),
		    f_u_make_var(Name_Param,
				 Xx_person_ob_xx_Get21,
				 TrueResult46),
		    ElseResult47=TrueResult46
		;   get_var(Env, u_else, IFTEST22),
		    (   IFTEST22\==[]
		    ->  f_u_symbol_c62_string(sys_name, Str_Init),
			LEnv=[[bv(u_str, Str_Init)]|Env],
			f_u_string_length(u_str, Len_Init),
			LEnv28=[[bv(u_len, Len_Init)]|LEnv],
			f_u_nthchar(u_str, [-, u_len, 1], Last_char_Init),
			Env=[[bv(u_last_char, Last_char_Init)]|LEnv28],
			f_u_digit_c63(u_last_char, 10, IFTEST34),
			(   IFTEST34\==[]
			->  f_u_string_c62_symbol(
						  [ u_string_slice,
						    u_str,
						    0,
						    [-, u_len, 1]
						  ],
						  C62_ob_Param),
			    f_u_ob_c36_name_c62_ob(C62_ob_Param, C62_ob_Ret),
			    f_u_make_var(Name_Param, C62_ob_Ret, TrueResult),
			    ElseResult47=TrueResult
			;   f_u_ob_c36_name_c62_ob(Name_Param, C62_ob_Ret56),
			    f_u_make_var(Name_Param, C62_ob_Ret56, ElseResult),
			    ElseResult47=ElseResult
			)
		    ;   ElseResult47=[]
		    )
		)
	    )
	).
:- set_opv(f_u_make_typed_var, classof, claz_function),
   set_opv(u_make_typed_var, compile_as, kw_function),
   set_opv(u_make_typed_var, function, f_u_make_typed_var),
   DefunResult=u_make_typed_var.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1030 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*bell-char*', ['ascii->char', 7]]).
:- f_u_ascii_c62_char(7, _Ignored),
   set_var(TLEnv3, u_xx_bell_char_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1065 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*esc-char*', ['ascii->char', 27]]).
:- f_u_ascii_c62_char(27, _Ignored),
   set_var(TLEnv3, u_xx_esc_char_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1100 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*del-char*', ['ascii->char', 127]]).
:- f_u_ascii_c62_char(127, _Ignored),
   set_var(TLEnv3, u_xx_del_char_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1136 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*cr-char*', ['ascii->char', 13]]).
:- f_u_ascii_c62_char(13, _Ignored),
   set_var(TLEnv3, u_xx_cr_char_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1170 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*cntrl-z-char*', ['ascii->char', 26]]).
:- f_u_ascii_c62_char(26, _Ignored),
   set_var(TLEnv3, u_xx_cntrl_z_char_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1209 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [setq, '*cntrl-rb-char*', ['ascii->char', 29]]).
:- f_u_ascii_c62_char(29, _Ignored),
   set_var(TLEnv3, u_xx_cntrl_rb_char_xx, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'string-truncate',
			    [str, len],
			    
			    [ 'string-slice',
			      str,
			      0,
			      [min, ['string-length', str], len]
			    ]
			  ]).

% annotating U::STRING-TRUNCATE 
wl: lambda_def(defun,
	      u_string_truncate,
	      f_u_string_truncate,
	      [u_str, u_len],
	      [[u_string_slice, u_str, 0, [min, [u_string_length, u_str], u_len]]]).


% annotating U::STRING-TRUNCATE 
wl: arglist_info(u_string_truncate,
		[u_str, u_len],
		[Str_Param, Len_Param],
		arginfo{ all:[u_str, u_len],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_str, u_len],
			 opt:0,
			 req:[u_str, u_len],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-TRUNCATE 
wl: init_args(exact_only, u_string_truncate).


% annotating U::STRING-TRUNCATE 
f_u_string_truncate(Str_Param, Len_Param, FnResult) :-
	Env=[bv(u_str, Str_Param), bv(u_len, Len_Param)],
	f_u_string_slice(u_str,
			 0,
			 [min, [u_string_length, u_str], u_len],
			 String_slice_Ret),
	String_slice_Ret=FnResult.
:- set_opv(f_u_string_truncate, classof, claz_function),
   set_opv(u_string_truncate, compile_as, kw_function),
   set_opv(u_string_truncate, function, f_u_string_truncate),
   DefunResult=u_string_truncate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1339)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" NDBG: New Debugging Mechanism", 1, 1341)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1373)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For use in the program:", 1, 1375)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1401)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-begin) - Start a new indentation level",
				     1,
				     1403)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-add-item rule) - Add item to list of current items",
				     1,
				     1450)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg *dbg-stream* keyname \"Message~%\") - Print a debugging message",
				     1,
				     1509)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-remove-item rule) - Remove item to list of current items",
				     1,
				     1579)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-end) - End indentation level",
				     1,
				     1644)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1681)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" For use at debugging time:", 1, 1683)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 1712)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (interest 'keyname . items) - Show debugging info for keyname",
				     1,
				     1714)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("   when any item is present in current items, or if an item",
				     1,
				     1778)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("   is 'all, always", 1, 1839)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (disinterest 'keyname . items) - Stop debugging info for keyname",
				     1,
				     1859)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("   and items", 1, 1926)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (interests) - Show current interests",
				     1,
				     1940)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" (ndbg-reset) - Reset indenting level back to zero",
				     1,
				     1979)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:1250 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 2031)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2033 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ndbg-interests*', []]).
:- set_var(TLEnv3, setq, u_xx_ndbg_interests_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2061 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ndbg-level*', 0]).
:- set_var(TLEnv3, setq, u_xx_ndbg_level_xx, 0).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2083 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ndbg-items*', []]).
:- set_var(TLEnv3, setq, u_xx_ndbg_items_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2107 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ndbg-indentation*', 1]).
:- set_var(TLEnv3, setq, u_xx_ndbg_indentation_xx, 1).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2135 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*ndbg-max-indentation*', 50]).
:- set_var(TLEnv3, setq, u_xx_ndbg_max_indentation_xx, 50).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2169 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ndbg-add-item',
			    [item],
			    [setq, '*ndbg-items*', [cons, item, '*ndbg-items*']]
			  ]).

% annotating U::NDBG-ADD-ITEM 
wl: lambda_def(defun,
	      u_ndbg_add_item,
	      f_u_ndbg_add_item,
	      [item],
	      [[setq, u_xx_ndbg_items_xx, [cons, item, u_xx_ndbg_items_xx]]]).


% annotating U::NDBG-ADD-ITEM 
wl: arglist_info(u_ndbg_add_item,
		[item],
		[Item_Param],
		arginfo{ all:[item],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[item],
			 opt:0,
			 req:[item],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-ADD-ITEM 
wl: init_args(exact_only, u_ndbg_add_item).


% annotating U::NDBG-ADD-ITEM 
f_u_ndbg_add_item(Item_Param, FnResult) :-
	Env=[bv(item, Item_Param)],
	get_var(Env, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get),
	Xx_ndbg_items_xx=[Item_Param|Xx_ndbg_items_xx_Get],
	set_var(Env, u_xx_ndbg_items_xx, Xx_ndbg_items_xx),
	Xx_ndbg_items_xx=FnResult.
:- set_opv(f_u_ndbg_add_item, classof, claz_function),
   set_opv(u_ndbg_add_item, compile_as, kw_function),
   set_opv(u_ndbg_add_item, function, f_u_ndbg_add_item),
   DefunResult=u_ndbg_add_item.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2245 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ndbg-remove-item',
			    [item],
			    
			    [ setq,
			      '*ndbg-items*',
			      ['delq!', item, '*ndbg-items*']
			    ]
			  ]).

% annotating U::NDBG-REMOVE-ITEM 
wl: lambda_def(defun,
	      u_ndbg_remove_item,
	      f_u_ndbg_remove_item,
	      [item],
	      [[setq, u_xx_ndbg_items_xx, [u_delq_c33, item, u_xx_ndbg_items_xx]]]).


% annotating U::NDBG-REMOVE-ITEM 
wl: arglist_info(u_ndbg_remove_item,
		[item],
		[Item_Param],
		arginfo{ all:[item],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[item],
			 opt:0,
			 req:[item],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NDBG-REMOVE-ITEM 
wl: init_args(exact_only, u_ndbg_remove_item).


% annotating U::NDBG-REMOVE-ITEM 
f_u_ndbg_remove_item(Item_Param, FnResult) :-
	Env=[bv(item, Item_Param)],
	f_u_delq_c33(item, u_xx_ndbg_items_xx, Xx_ndbg_items_xx),
	set_var(Env, u_xx_ndbg_items_xx, Xx_ndbg_items_xx),
	Xx_ndbg_items_xx=FnResult.
:- set_opv(f_u_ndbg_remove_item, classof, claz_function),
   set_opv(u_ndbg_remove_item, compile_as, kw_function),
   set_opv(u_ndbg_remove_item, function, f_u_ndbg_remove_item),
   DefunResult=u_ndbg_remove_item.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2325 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ndbg-indentation',
			    [stream],
			    
			    [ yloop,
			      
			      [ initial,
				
				[ cnt,
				  
				  [ min,
				    [*, '*ndbg-level*', '*ndbg-indentation*'],
				    '*ndbg-max-indentation*'
				  ]
				]
			      ],
			      [ywhile, [>, cnt, 0]],
			      
			      [ ydo,
				[format, stream, '$STRING'(" ")],
				[setq, cnt, [-, cnt, 1]]
			      ]
			    ]
			  ]).

% annotating U::NDBG-INDENTATION 
wl: lambda_def(defun,
	      u_ndbg_indentation,
	      f_u_ndbg_indentation,
	      [stream],
	      
	      [ 
		[ u_yloop,
		  
		  [ u_initial,
		    
		    [ u_cnt,
		      
		      [ min,
			[*, u_xx_ndbg_level_xx, u_xx_ndbg_indentation_xx],
			u_xx_ndbg_max_indentation_xx
		      ]
		    ]
		  ],
		  [u_ywhile, [>, u_cnt, 0]],
		  
		  [ u_ydo,
		    
		    [ format,
		      stream,
		      '$ARRAY'([*], claz_base_character, [#\(' ')])
		    ],
		    [setq, u_cnt, [-, u_cnt, 1]]
		  ]
		]
	      ]).


% annotating U::NDBG-INDENTATION 
wl: arglist_info(u_ndbg_indentation,
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

% annotating U::NDBG-INDENTATION 
wl: init_args(exact_only, u_ndbg_indentation).


% annotating U::NDBG-INDENTATION 
f_u_ndbg_indentation(Stream_Param, FnResult) :-
	Env=[bv(stream, Stream_Param)],
	f_u_yloop(
		  [ 
		    [ u_initial,
		      
		      [ u_cnt,
			
			[ min,
			  [*, u_xx_ndbg_level_xx, u_xx_ndbg_indentation_xx],
			  u_xx_ndbg_max_indentation_xx
			]
		      ]
		    ],
		    [u_ywhile, [>, u_cnt, 0]],
		    
		    [ u_ydo,
		      
		      [ format,
			stream,
			'$ARRAY'([*], claz_base_character, [#\(' ')])
		      ],
		      [setq, u_cnt, [-, u_cnt, 1]]
		    ]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_ndbg_indentation, classof, claz_function),
   set_opv(u_ndbg_indentation, compile_as, kw_function),
   set_opv(u_ndbg_indentation, function, f_u_ndbg_indentation),
   DefunResult=u_ndbg_indentation.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2577 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ndbg-begin',
			    [],
			    [setq, '*ndbg-level*', [+, '*ndbg-level*', 1]]
			  ]).

% annotating U::NDBG-BEGIN 
wl: lambda_def(defun,
	      u_ndbg_begin,
	      f_u_ndbg_begin,
	      [],
	      [[setq, u_xx_ndbg_level_xx, [+, u_xx_ndbg_level_xx, 1]]]).


% annotating U::NDBG-BEGIN 
wl: arglist_info(u_ndbg_begin,
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

% annotating U::NDBG-BEGIN 
wl: init_args(exact_only, u_ndbg_begin).


% annotating U::NDBG-BEGIN 
f_u_ndbg_begin(FnResult) :-
	Env=[],
	get_var(Env, u_xx_ndbg_level_xx, Xx_ndbg_level_xx_Get),
	+(Xx_ndbg_level_xx_Get, 1, Xx_ndbg_level_xx),
	set_var(Env, u_xx_ndbg_level_xx, Xx_ndbg_level_xx),
	Xx_ndbg_level_xx=FnResult.
:- set_opv(f_u_ndbg_begin, classof, claz_function),
   set_opv(u_ndbg_begin, compile_as, kw_function),
   set_opv(u_ndbg_begin, function, f_u_ndbg_begin),
   DefunResult=u_ndbg_begin.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2641 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'ndbg-end',
			    [],
			    [setq, '*ndbg-level*', [-, '*ndbg-level*', 1]]
			  ]).

% annotating U::NDBG-END 
wl: lambda_def(defun,
	      u_ndbg_end,
	      f_u_ndbg_end,
	      [],
	      [[setq, u_xx_ndbg_level_xx, [-, u_xx_ndbg_level_xx, 1]]]).


% annotating U::NDBG-END 
wl: arglist_info(u_ndbg_end,
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

% annotating U::NDBG-END 
wl: init_args(exact_only, u_ndbg_end).


% annotating U::NDBG-END 
f_u_ndbg_end(FnResult) :-
	Env=[],
	get_var(Env, u_xx_ndbg_level_xx, Xx_ndbg_level_xx_Get),
	-(Xx_ndbg_level_xx_Get, 1, Xx_ndbg_level_xx),
	set_var(Env, u_xx_ndbg_level_xx, Xx_ndbg_level_xx),
	Xx_ndbg_level_xx=FnResult.
:- set_opv(f_u_ndbg_end, classof, claz_function),
   set_opv(u_ndbg_end, compile_as, kw_function),
   set_opv(u_ndbg_end, function, f_u_ndbg_end),
   DefunResult=u_ndbg_end.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2703 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'ndbg-reset', [], [setq, '*ndbg-level*', 0]]).

% annotating U::NDBG-RESET 
wl: lambda_def(defun,
	      u_ndbg_reset,
	      f_u_ndbg_reset,
	      [],
	      [[setq, u_xx_ndbg_level_xx, 0]]).


% annotating U::NDBG-RESET 
wl: arglist_info(u_ndbg_reset,
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

% annotating U::NDBG-RESET 
wl: init_args(exact_only, u_ndbg_reset).


% annotating U::NDBG-RESET 
f_u_ndbg_reset(FnResult) :-
	Env=[],
	set_var(Env, setq, u_xx_ndbg_level_xx, 0),
	0=FnResult.
:- set_opv(f_u_ndbg_reset, classof, claz_function),
   set_opv(u_ndbg_reset, compile_as, kw_function),
   set_opv(u_ndbg_reset, function, f_u_ndbg_reset),
   DefunResult=u_ndbg_reset.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2703 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Use (interest 'unify ^rule) and (interest 'show ^rule)",
				     1,
				     2749)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2703 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" to get full debugging info for a rule. (And use disinterest",
				     1,
				     2806)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2703 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" to turn off).", 1, 2868)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:2884 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    interest,
			    [key, '&rest', items],
			    
			    [ let,
			      [[found, [assq, key, '*ndbg-interests*']]],
			      
			      [ if,
				found,
				
				[ yloop,
				  [yfor, item, in, items],
				  
				  [ ydo,
				    
				    [ if,
				      ['memq?', item, [cdr, found]],
				      
				      [ format,
					'*gate-output*',
					'$STRING'("Item ~A key ~A already an interest~%"),
					item,
					key
				      ],
				      
				      [ setf,
					[cdr, found],
					[cons, item, [cdr, found]]
				      ]
				    ]
				  ]
				],
				
				[ setq,
				  '*ndbg-interests*',
				  [cons, [cons, key, items], '*ndbg-interests*']
				]
			      ],
			      [interests]
			    ]
			  ]).

% annotating U::INTEREST 
wl: lambda_def(defun,
	      u_interest,
	      f_u_interest,
	      [key, c38_rest, u_items],
	      
	      [ 
		[ let,
		  [[u_found, [ext_assq, key, u_xx_ndbg_interests_xx]]],
		  
		  [ if,
		    u_found,
		    
		    [ u_yloop,
		      [u_yfor, item, u_in, u_items],
		      
		      [ u_ydo,
			
			[ if,
			  [u_memq_c63, item, [cdr, u_found]],
			  
			  [ format,
			    u_xx_gate_output_xx,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(t),
				       #\(e),
				       #\(m),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(k),
				       #\(e),
				       #\(y),
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
				       #\(a),
				       #\(n),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(s),
				       #\(t),
				       #\(~),
				       #\('%')
				     ]),
			    item,
			    key
			  ],
			  [setf, [cdr, u_found], [cons, item, [cdr, u_found]]]
			]
		      ]
		    ],
		    
		    [ setq,
		      u_xx_ndbg_interests_xx,
		      [cons, [cons, key, u_items], u_xx_ndbg_interests_xx]
		    ]
		  ],
		  [u_interests]
		]
	      ]).


% annotating U::INTEREST 
wl: arglist_info(u_interest,
		[key, c38_rest, u_items],
		[key, u_items],
		arginfo{ all:[key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[key, u_items],
			 opt:0,
			 req:[key],
			 rest:[u_items],
			 sublists:0,
			 whole:0
		       }).


% annotating U::INTEREST 
wl: init_args(1, u_interest).


% annotating U::INTEREST 
f_u_interest(Key_Param, Items_Param, FnResult) :-
	Env=[bv(key, Key_Param), bv(u_items, Items_Param)],
	f_ext_assq(key, u_xx_ndbg_interests_xx, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_yfor, item, u_in, u_items],
			
			[ u_ydo,
			  
			  [ if,
			    [u_memq_c63, item, [cdr, u_found]],
			    
			    [ format,
			      u_xx_gate_output_xx,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('I'),
					 #\(t),
					 #\(e),
					 #\(m),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(k),
					 #\(e),
					 #\(y),
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
					 #\(a),
					 #\(n),
					 #\(' '),
					 #\(i),
					 #\(n),
					 #\(t),
					 #\(e),
					 #\(r),
					 #\(e),
					 #\(s),
					 #\(t),
					 #\(~),
					 #\('%')
				       ]),
			      item,
			      key
			    ],
			    [setf, [cdr, u_found], [cons, item, [cdr, u_found]]]
			  ]
			]
		      ],
		      TrueResult),
	    _137986=TrueResult
	;   get_var(LEnv, u_items, Items_Get),
	    CAR=[Key_Param|Items_Get],
	    get_var(LEnv, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
	    ElseResult=[CAR|Xx_ndbg_interests_xx_Get],
	    set_var(LEnv, u_xx_ndbg_interests_xx, ElseResult),
	    _137986=ElseResult
	),
	f_u_interests(Interests_Ret),
	LetResult=Interests_Ret,
	LetResult=FnResult.
:- set_opv(f_u_interest, classof, claz_function),
   set_opv(u_interest, compile_as, kw_function),
   set_opv(u_interest, function, f_u_interest),
   DefunResult=u_interest.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3379 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    disinterest,
			    [key, '&rest', items],
			    
			    [ let,
			      [[found, [assq, key, '*ndbg-interests*']]],
			      
			      [ if,
				found,
				
				[ yloop,
				  [yfor, item, in, items],
				  
				  [ ydo,
				    
				    [ if,
				      [not, ['memq?', item, [cdr, found]]],
				      
				      [ format,
					'*gate-output*',
					'$STRING'("Item ~A key ~A not an interest~%"),
					item,
					key
				      ],
				      
				      [ setf,
					[cdr, found],
					['delq!', item, [cdr, found]]
				      ]
				    ]
				  ]
				],
				
				[ format,
				  '*gate-output*',
				  '$STRING'("Key ~A not found at all~%"),
				  key
				]
			      ],
			      [interests]
			    ]
			  ]).

% annotating U::DISINTEREST 
wl: lambda_def(defun,
	      u_disinterest,
	      f_u_disinterest,
	      [key, c38_rest, u_items],
	      
	      [ 
		[ let,
		  [[u_found, [ext_assq, key, u_xx_ndbg_interests_xx]]],
		  
		  [ if,
		    u_found,
		    
		    [ u_yloop,
		      [u_yfor, item, u_in, u_items],
		      
		      [ u_ydo,
			
			[ if,
			  [not, [u_memq_c63, item, [cdr, u_found]]],
			  
			  [ format,
			    u_xx_gate_output_xx,
			    '$ARRAY'([*],
				     claz_base_character,
				     
				     [ #\('I'),
				       #\(t),
				       #\(e),
				       #\(m),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(k),
				       #\(e),
				       #\(y),
				       #\(' '),
				       #\(~),
				       #\('A'),
				       #\(' '),
				       #\(n),
				       #\(o),
				       #\(t),
				       #\(' '),
				       #\(a),
				       #\(n),
				       #\(' '),
				       #\(i),
				       #\(n),
				       #\(t),
				       #\(e),
				       #\(r),
				       #\(e),
				       #\(s),
				       #\(t),
				       #\(~),
				       #\('%')
				     ]),
			    item,
			    key
			  ],
			  [setf, [cdr, u_found], [u_delq_c33, item, [cdr, u_found]]]
			]
		      ]
		    ],
		    
		    [ format,
		      u_xx_gate_output_xx,
		      '$ARRAY'([*],
			       claz_base_character,
			       
			       [ #\('K'),
				 #\(e),
				 #\(y),
				 #\(' '),
				 #\(~),
				 #\('A'),
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
				 #\(a),
				 #\(t),
				 #\(' '),
				 #\(a),
				 #\(l),
				 #\(l),
				 #\(~),
				 #\('%')
			       ]),
		      key
		    ]
		  ],
		  [u_interests]
		]
	      ]).


% annotating U::DISINTEREST 
wl: arglist_info(u_disinterest,
		[key, c38_rest, u_items],
		[key, u_items],
		arginfo{ all:[key],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[key, u_items],
			 opt:0,
			 req:[key],
			 rest:[u_items],
			 sublists:0,
			 whole:0
		       }).


% annotating U::DISINTEREST 
wl: init_args(1, u_disinterest).


% annotating U::DISINTEREST 
f_u_disinterest(Key_Param, Items_Param, FnResult) :-
	Env=[bv(key, Key_Param), bv(u_items, Items_Param)],
	f_ext_assq(key, u_xx_ndbg_interests_xx, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  f_u_yloop(
		      [ [u_yfor, item, u_in, u_items],
			
			[ u_ydo,
			  
			  [ if,
			    [not, [u_memq_c63, item, [cdr, u_found]]],
			    
			    [ format,
			      u_xx_gate_output_xx,
			      '$ARRAY'([*],
				       claz_base_character,
				       
				       [ #\('I'),
					 #\(t),
					 #\(e),
					 #\(m),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(k),
					 #\(e),
					 #\(y),
					 #\(' '),
					 #\(~),
					 #\('A'),
					 #\(' '),
					 #\(n),
					 #\(o),
					 #\(t),
					 #\(' '),
					 #\(a),
					 #\(n),
					 #\(' '),
					 #\(i),
					 #\(n),
					 #\(t),
					 #\(e),
					 #\(r),
					 #\(e),
					 #\(s),
					 #\(t),
					 #\(~),
					 #\('%')
				       ]),
			      item,
			      key
			    ],
			    
			    [ setf,
			      [cdr, u_found],
			      [u_delq_c33, item, [cdr, u_found]]
			    ]
			  ]
			]
		      ],
		      TrueResult),
	    _138206=TrueResult
	;   get_var(LEnv, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	    cl_format(
		      [ Xx_gate_output_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('K'),
				   #\(e),
				   #\(y),
				   #\(' '),
				   #\(~),
				   #\('A'),
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
				   #\(a),
				   #\(t),
				   #\(' '),
				   #\(a),
				   #\(l),
				   #\(l),
				   #\(~),
				   #\('%')
				 ]),
			Key_Param
		      ],
		      ElseResult),
	    _138206=ElseResult
	),
	f_u_interests(Interests_Ret),
	LetResult=Interests_Ret,
	LetResult=FnResult.
:- set_opv(f_u_disinterest, classof, claz_function),
   set_opv(u_disinterest, compile_as, kw_function),
   set_opv(u_disinterest, function, f_u_disinterest),
   DefunResult=u_disinterest.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3875 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, interests, [], '*ndbg-interests*']).

% annotating U::INTERESTS 
wl: lambda_def(defun, u_interests, f_u_interests, [], [u_xx_ndbg_interests_xx]).


% annotating U::INTERESTS 
wl: arglist_info(u_interests,
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

% annotating U::INTERESTS 
wl: init_args(exact_only, u_interests).


% annotating U::INTERESTS 
f_u_interests(FnResult) :-
	Env=[],
	get_var(Env, u_xx_ndbg_interests_xx, Xx_ndbg_interests_xx_Get),
	Xx_ndbg_interests_xx_Get=FnResult.
:- set_opv(f_u_interests, classof, claz_function),
   set_opv(u_interests, compile_as, kw_function),
   set_opv(u_interests, function, f_u_interests),
   DefunResult=u_interests.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3913 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, items, [], '*ndbg-items*']).

% annotating U::ITEMS 
wl: lambda_def(defun, u_items, f_u_items, [], [u_xx_ndbg_items_xx]).


% annotating U::ITEMS 
wl: arglist_info(u_items,
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

% annotating U::ITEMS 
wl: init_args(exact_only, u_items).


% annotating U::ITEMS 
f_u_items(FnResult) :-
	Env=[],
	get_var(Env, u_xx_ndbg_items_xx, Xx_ndbg_items_xx_Get),
	Xx_ndbg_items_xx_Get=FnResult.
:- set_opv(f_u_items, classof, claz_function),
   set_opv(u_items, compile_as, kw_function),
   set_opv(u_items, function, f_u_items),
   DefunResult=u_items.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:3944 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'write-comments',
			    [comments, stream],
			    
			    [ let,
			      
			      [ 
				[ 'max-length',
				  
				  [ (+),
				    2,
				    
				    [ apply,
				      [quote, max],
				      
				      [ map,
					[quote, list],
					'string-length',
					comments
				      ]
				    ]
				  ]
				]
			      ],
			      ['write-dashes-stream', 'max-length', stream],
			      
			      [ yloop,
				[yfor, comment1, in, comments],
				[ydo, [dbg, stream, '$STRING'(" ~A~%"), comment1]]
			      ],
			      ['write-dashes-stream', 'max-length', stream]
			    ]
			  ]).

% annotating U::WRITE-COMMENTS 
wl: lambda_def(defun,
	      u_write_comments,
	      f_u_write_comments,
	      [u_comments, stream],
	      
	      [ 
		[ let,
		  
		  [ 
		    [ u_max_length,
		      
		      [ (+),
			2,
			
			[ apply,
			  [quote, max],
			  [map, [quote, list], u_string_length, u_comments]
			]
		      ]
		    ]
		  ],
		  [u_write_dashes_stream, u_max_length, stream],
		  
		  [ u_yloop,
		    [u_yfor, u_comment1, u_in, u_comments],
		    
		    [ u_ydo,
		      
		      [ u_dbg,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(~), #\('A'), #\(~), #\('%')]),
			u_comment1
		      ]
		    ]
		  ],
		  [u_write_dashes_stream, u_max_length, stream]
		]
	      ]).


% annotating U::WRITE-COMMENTS 
wl: arglist_info(u_write_comments,
		[u_comments, stream],
		[Comments_Param, Stream_Param],
		arginfo{ all:[u_comments, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_comments, stream],
			 opt:0,
			 req:[u_comments, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::WRITE-COMMENTS 
wl: init_args(exact_only, u_write_comments).


% annotating U::WRITE-COMMENTS 
f_u_write_comments(Comments_Param, Stream_Param, FnResult) :-
	Env=[bv(u_comments, Comments_Param), bv(stream, Stream_Param)],
	get_var(Env, map, Map_Get),
	get_var(Env, u_string_length, String_length_Get),
	cl_max(Map_Get, list, String_length_Get, Comments_Param, Max_Ret),
	+(2, Max_Ret, Max_length_Init),
	LEnv=[[bv(u_max_length, Max_length_Init)]|Env],
	get_var(LEnv, u_max_length, Max_length_Get),
	f_u_write_dashes_stream(Max_length_Get, Stream_Param, Dashes_stream_Ret),
	f_u_yloop(
		  [ [u_yfor, u_comment1, u_in, u_comments],
		    
		    [ u_ydo,
		      
		      [ u_dbg,
			stream,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(' '), #\(~), #\('A'), #\(~), #\('%')]),
			u_comment1
		      ]
		    ]
		  ],
		  Yloop_Ret),
	get_var(LEnv, u_max_length, Max_length_Get23),
	f_u_write_dashes_stream(Max_length_Get23,
				Stream_Param,
				Dashes_stream_Ret30),
	LetResult=Dashes_stream_Ret30,
	LetResult=FnResult.
:- set_opv(f_u_write_comments, classof, claz_function),
   set_opv(u_write_comments, compile_as, kw_function),
   set_opv(u_write_comments, function, f_u_write_comments),
   DefunResult=u_write_comments.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:4265 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'write-dashes-stream',
			    [number, stream],
			    
			    [ yloop,
			      [initial, [count, 1]],
			      [ywhile, [<=, count, number]],
			      
			      [ ydo,
				[format, stream, '$STRING'("-")],
				['increment-me', count]
			      ]
			    ],
			    [newline, stream]
			  ]).

% annotating U::WRITE-DASHES-STREAM 
wl: lambda_def(defun,
	      u_write_dashes_stream,
	      f_u_write_dashes_stream,
	      [number, stream],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [count, 1]],
		  [u_ywhile, [<=, count, number]],
		  
		  [ u_ydo,
		    [format, stream, '$ARRAY'([*], claz_base_character, [#\(-)])],
		    [u_increment_me, count]
		  ]
		],
		[u_newline, stream]
	      ]).


% annotating U::WRITE-DASHES-STREAM 
wl: arglist_info(u_write_dashes_stream,
		[number, stream],
		[Number_Param, Stream_Param],
		arginfo{ all:[number, stream],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[number, stream],
			 opt:0,
			 req:[number, stream],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::WRITE-DASHES-STREAM 
wl: init_args(exact_only, u_write_dashes_stream).


% annotating U::WRITE-DASHES-STREAM 
f_u_write_dashes_stream(Number_Param, Stream_Param, FnResult) :-
	Env=[bv(number, Number_Param), bv(stream, Stream_Param)],
	f_u_yloop(
		  [ [u_initial, [count, 1]],
		    [u_ywhile, [<=, count, number]],
		    
		    [ u_ydo,
		      
		      [ format,
			stream,
			'$ARRAY'([*], claz_base_character, [#\(-)])
		      ],
		      [u_increment_me, count]
		    ]
		  ],
		  Yloop_Ret),
	f_u_newline(stream, Newline_Ret),
	Newline_Ret=FnResult.
:- set_opv(f_u_write_dashes_stream, classof, claz_function),
   set_opv(u_write_dashes_stream, compile_as, kw_function),
   set_opv(u_write_dashes_stream, function, f_u_write_dashes_stream),
   DefunResult=u_write_dashes_stream.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:4461 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'new-filename',
			    [atm],
			    
			    [ 'let*',
			      
			      [ 
				[ name,
				  
				  [ 'string-downcase!',
				    ['symbol->string', ['gen-id', atm]]
				  ]
				],
				
				[ filename,
				  ['string-append', '$STRING'("tmp."), name]
				]
			      ],
			      
			      [ yloop,
				[ywhile, ['file-exists?', filename]],
				
				[ ydo,
				  [dbg, '*gate-warn-dbg*', '$STRING'("-")],
				  
				  [ setq,
				    filename,
				    ['string-append', filename, '$STRING'("a")]
				  ]
				],
				[yresult, filename]
			      ]
			    ]
			  ]).

% annotating U::NEW-FILENAME 
wl: lambda_def(defun,
	      u_new_filename,
	      f_u_new_filename,
	      [u_atm],
	      
	      [ 
		[ let_xx,
		  
		  [ 
		    [ sys_name,
		      
		      [ u_string_downcase_c33,
			[u_symbol_c62_string, [u_gen_id, u_atm]]
		      ]
		    ],
		    
		    [ u_filename,
		      
		      [ u_string_append,
			'$ARRAY'([*],
				 claz_base_character,
				 [#\(t), #\(m), #\(p), #\('.')]),
			sys_name
		      ]
		    ]
		  ],
		  
		  [ u_yloop,
		    [u_ywhile, [u_file_exists_c63, u_filename]],
		    
		    [ u_ydo,
		      
		      [ u_dbg,
			u_xx_gate_warn_dbg_xx,
			'$ARRAY'([*], claz_base_character, [#\(-)])
		      ],
		      
		      [ setq,
			u_filename,
			
			[ u_string_append,
			  u_filename,
			  '$ARRAY'([*], claz_base_character, [#\(a)])
			]
		      ]
		    ],
		    [u_yresult, u_filename]
		  ]
		]
	      ]).


% annotating U::NEW-FILENAME 
wl: arglist_info(u_new_filename,
		[u_atm],
		[Atm_Param],
		arginfo{ all:[u_atm],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_atm],
			 opt:0,
			 req:[u_atm],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NEW-FILENAME 
wl: init_args(exact_only, u_new_filename).


% annotating U::NEW-FILENAME 
f_u_new_filename(Atm_Param, FnResult) :-
	Env=[bv(u_atm, Atm_Param)],
	f_u_string_downcase_c33([u_symbol_c62_string, [u_gen_id, u_atm]],
				Name_Init),
	LEnv=[[bv(sys_name, Name_Init)]|Env],
	f_u_string_append(
			  [ '$ARRAY'([*],
				     claz_base_character,
				     [#\(t), #\(m), #\(p), #\('.')]),
			    sys_name
			  ],
			  Filename_Init),
	LEnv15=[[bv(u_filename, Filename_Init)]|LEnv],
	f_u_yloop(
		  [ [u_ywhile, [u_file_exists_c63, u_filename]],
		    
		    [ u_ydo,
		      
		      [ u_dbg,
			u_xx_gate_warn_dbg_xx,
			'$ARRAY'([*], claz_base_character, [#\(-)])
		      ],
		      
		      [ setq,
			u_filename,
			
			[ u_string_append,
			  u_filename,
			  '$ARRAY'([*], claz_base_character, [#\(a)])
			]
		      ]
		    ],
		    [u_yresult, u_filename]
		  ],
		  Yloop_Ret),
	LetResult=Yloop_Ret,
	LetResult=FnResult.
:- set_opv(f_u_new_filename, classof, claz_function),
   set_opv(u_new_filename, compile_as, kw_function),
   set_opv(u_new_filename, function, f_u_new_filename),
   DefunResult=u_new_filename.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:4764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'set-macro-character',
			    #\(?),
			    
			    [ lambda,
			      [stream, ch],
			      
			      [ let,
				
				[ ['read-in', [read, stream, t, [], t]],
				  ['colon-pos', []],
				  [str, []]
				],
				[setq, str, ['symbol->string', 'read-in']],
				
				[ cond,
				  
				  [ 
				    [ setq,
				      'colon-pos',
				      ['string-posq', #\(+), str]
				    ],
				    
				    [ 'ob$fcreate',
				      
				      [ '#BQ',
					
					[ 'UAND',
					  obj,
					  
					  [ 'UPROC',
					    proc,
					    
					    [ 'QUOTE',
					      
					      [ '#COMMA',
						
						[ 'string->symbol',
						  
						  [ 'string-append',
						    
						    [ nthchdr,
						      str,
						      ['1+', 'colon-pos']
						    ],
						    '$STRING'("?")
						  ]
						]
					      ]
					    ]
					  ],
					  obj,
					  
					  [ '#COMMA',
					    
					    [ 'make-typed-var',
					      
					      [ 'string->symbol',
						[substring, str, 0, 'colon-pos']
					      ]
					    ]
					  ]
					]
				      ]
				    ]
				  ],
				  
				  [ 
				    [ setq,
				      'colon-pos',
				      ['string-posq', #\(:), str]
				    ],
				    
				    [ if,
				      [=, 'colon-pos', 0],
				      
				      [ 'make-var',
					[],
					
					[ 'ob$name->ob',
					  
					  [ 'string->symbol',
					    [nthchdr, str, ['1+', 'colon-pos']]
					  ]
					]
				      ],
				      
				      [ 'make-var',
					
					[ 'string->symbol',
					  [substring, str, 0, 'colon-pos']
					],
					
					[ 'ob$name->ob',
					  
					  [ 'string->symbol',
					    [nthchdr, str, ['1+', 'colon-pos']]
					  ]
					]
				      ]
				    ]
				  ],
				  [else, ['make-typed-var', 'read-in']]
				]
			      ]
			    ],
			    t
			  ]).
:- Lambda=closure([Env|TLEnv3], LetResult, [stream, u_ch],  (get_var(Env, stream, Stream_Get), cl_read(Stream_Get, t, [], t, Read_in_Init), LEnv=[[bv(u_read_in, Read_in_Init), bv(u_colon_pos, []), bv(u_str, [])]|Env], f_u_symbol_c62_string(u_read_in, Str), set_var(LEnv, u_str, Str), f_u_string_posq(#\(+), u_str, IFTEST), set_var(LEnv, u_colon_pos, IFTEST), (IFTEST\==[]->f_u_ob_c36_fcreate(['#BQ', [u_uand, u_obj, [u_uproc, u_proc, [quote, ['#COMMA', [u_string_c62_symbol, [u_string_append, [u_nthchdr, u_str, ['1+', u_colon_pos]], '$ARRAY'([*], claz_base_character, [#\(?)])]]]]], u_obj, ['#COMMA', [u_make_typed_var, [u_string_c62_symbol, [ext_substring, u_str, 0, u_colon_pos]]]]]], TrueResult28), _139756=TrueResult28;f_u_string_posq(#\(:), u_str, IFTEST12), set_var(LEnv, u_colon_pos, IFTEST12), (IFTEST12\==[]->get_var(LEnv, u_colon_pos, Colon_pos_Get), (Colon_pos_Get=:=0->f_u_string_c62_symbol([u_nthchdr, u_str, ['1+', u_colon_pos]], C62_ob_Param), f_u_ob_c36_name_c62_ob(C62_ob_Param, C62_ob_Ret), f_u_make_var([], C62_ob_Ret, TrueResult), TrueResult26=TrueResult;f_u_string_c62_symbol([ext_substring, u_str, 0, u_colon_pos], Make_var_Param), f_u_string_c62_symbol([u_nthchdr, u_str, ['1+', u_colon_pos]], C62_ob_Param35), f_u_ob_c36_name_c62_ob(C62_ob_Param35, C62_ob_Ret38), f_u_make_var(Make_var_Param, C62_ob_Ret38, ElseResult), TrueResult26=ElseResult), ElseResult29=TrueResult26;get_var(LEnv, u_else, IFTEST20), (IFTEST20\==[]->get_var(LEnv, u_read_in, Read_in_Get), f_u_make_typed_var(Read_in_Get, TrueResult24), ElseResult27=TrueResult24;ElseResult25=[], ElseResult27=ElseResult25), ElseResult29=ElseResult27), _139756=ElseResult29), LetResult=_139756)),
   cl_set_macro_character(#\(?), Lambda, t, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:4764 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" e.g. for ?:person", 50, 5502)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:5752 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'set-macro-character',
			    #\(^),
			    
			    [ lambda,
			      [stream, ch],
			      
			      [ let,
				[[name, [read, stream, t, [], t]], [ob, []]],
				[setq, ob, ['ob$name->ob', name]],
				
				[ if,
				  ob,
				  [list, [quote, quote], ob],
				  
				  [ progn,
				    
				    [ format,
				      t,
				      '$STRING'("No such ob ^~A~%"),
				      name
				    ],
				    [list, [quote, quote], '*repl-wont-print*']
				  ]
				]
			      ]
			    ],
			    t
			  ]).
:- Lambda=closure([Env|TLEnv3], LetResult, [stream, u_ch],  (get_var(Env, stream, Stream_Get), cl_read(Stream_Get, t, [], t, Name_Init), LEnv=[[bv(sys_name, Name_Init), bv(u_ob, [])]|Env], get_var(LEnv, sys_name, Name_Get), f_u_ob_c36_name_c62_ob(Name_Get, Ob), set_var(LEnv, u_ob, Ob), get_var(LEnv, u_ob, IFTEST), (IFTEST\==[]->TrueResult=[quote, IFTEST], _137984=TrueResult;get_var(LEnv, sys_name, Name_Get15), cl_format([t, '$ARRAY'([*], claz_base_character, [#\('N'), #\(o), #\(' '), #\(s), #\(u), #\(c), #\(h), #\(' '), #\(o), #\(b), #\(' '), #\(^), #\(~), #\('A'), #\(~), #\('%')]), Name_Get15], Format_Ret), get_var(LEnv, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get), ElseResult=[quote, Xx_repl_wont_print_xx_Get], _137984=ElseResult), LetResult=_137984)),
   cl_set_macro_character(#\(^), Lambda, t, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:6044 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ 'set-macro-character',
			    #\(!),
			    
			    [ lambda,
			      [stream, ch],
			      
			      [ let,
				[[name, [read, stream, t, [], t]], [ob, []]],
				[setq, ob, ['ob$name->ob', name]],
				
				[ if,
				  ob,
				  
				  [ progn,
				    [po, ob],
				    [list, [quote, quote], '*repl-wont-print*']
				  ],
				  
				  [ progn,
				    
				    [ format,
				      t,
				      '$STRING'("No such ob ^~A~%"),
				      name
				    ],
				    [list, [quote, quote], '*repl-wont-print*']
				  ]
				]
			      ]
			    ],
			    t
			  ]).
:- Lambda=closure([Env|TLEnv3], LetResult, [stream, u_ch],  (get_var(Env, stream, Stream_Get), cl_read(Stream_Get, t, [], t, Name_Init), LEnv=[[bv(sys_name, Name_Init), bv(u_ob, [])]|Env], get_var(LEnv, sys_name, Name_Get), f_u_ob_c36_name_c62_ob(Name_Get, Ob), set_var(LEnv, u_ob, Ob), get_var(LEnv, u_ob, IFTEST), (IFTEST\==[]->f_u_po(IFTEST, Po_Ret), get_var(LEnv, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get), TrueResult=[quote, Xx_repl_wont_print_xx_Get], _138102=TrueResult;get_var(LEnv, sys_name, Name_Get16), cl_format([t, '$ARRAY'([*], claz_base_character, [#\('N'), #\(o), #\(' '), #\(s), #\(u), #\(c), #\(h), #\(' '), #\(o), #\(b), #\(' '), #\(^), #\(~), #\('A'), #\(~), #\('%')]), Name_Get16], Format_Ret), get_var(LEnv, u_xx_repl_wont_print_xx, Xx_repl_wont_print_xx_Get17), ElseResult=[quote, Xx_repl_wont_print_xx_Get17], _138102=ElseResult), LetResult=_138102)),
   cl_set_macro_character(#\(!), Lambda, t, _Ignored).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:6526 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    interrogate,
			    [string],
			    [format, ['standard-output'], string],
			    
			    [ let,
			      [[response, [read, ['standard-input']]]],
			      ['read-line', ['standard-input']],
			      
			      [ cond,
				
				[ 
				  [ or,
				    ['eq?', [quote, y], response],
				    ['eq?', [quote, yes], response]
				  ],
				  t
				],
				
				[ 
				  [ or,
				    ['eq?', [quote, n], response],
				    ['eq?', [quote, no], response]
				  ],
				  []
				],
				
				[ else,
				  
				  [ format,
				    ['standard-output'],
				    '$STRING'("Please type 'y' or 'n' as a response.~%")
				  ],
				  [interrogate, string]
				]
			      ]
			    ]
			  ]).

% annotating U::INTERROGATE 
wl: lambda_def(defun,
	      u_interrogate,
	      f_u_interrogate,
	      [string],
	      
	      [ [format, [u_standard_output], string],
		
		[ let,
		  [[u_response, [read, [u_standard_input]]]],
		  [read_line, [u_standard_input]],
		  
		  [ cond,
		    
		    [ 
		      [ or,
			[u_eq_c63, [quote, u_y], u_response],
			[u_eq_c63, [quote, u_yes], u_response]
		      ],
		      t
		    ],
		    
		    [ 
		      [ or,
			[u_eq_c63, [quote, n], u_response],
			[u_eq_c63, [quote, u_no], u_response]
		      ],
		      []
		    ],
		    
		    [ u_else,
		      
		      [ format,
			[u_standard_output],
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('P'),
				   #\(l),
				   #\(e),
				   #\(a),
				   #\(s),
				   #\(e),
				   #\(' '),
				   #\(t),
				   #\(y),
				   #\(p),
				   #\(e),
				   #\(' '),
				   #\('\''),
				   #\(y),
				   #\('\''),
				   #\(' '),
				   #\(o),
				   #\(r),
				   #\(' '),
				   #\('\''),
				   #\(n),
				   #\('\''),
				   #\(' '),
				   #\(a),
				   #\(s),
				   #\(' '),
				   #\(a),
				   #\(' '),
				   #\(r),
				   #\(e),
				   #\(s),
				   #\(p),
				   #\(o),
				   #\(n),
				   #\(s),
				   #\(e),
				   #\('.'),
				   #\(~),
				   #\('%')
				 ])
		      ],
		      [u_interrogate, string]
		    ]
		  ]
		]
	      ]).


% annotating U::INTERROGATE 
wl: arglist_info(u_interrogate,
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

% annotating U::INTERROGATE 
wl: init_args(exact_only, u_interrogate).


% annotating U::INTERROGATE 
f_u_interrogate(String_Param, ElseResult27) :-
	Env=[bv(string, String_Param)],
	f_u_standard_output(Standard_output_Ret),
	cl_format([Standard_output_Ret, string], Format_Ret),
	f_u_standard_input(Read_Param),
	cl_read(Read_Param, Response_Init),
	LEnv=[[bv(u_response, Response_Init)]|Env],
	f_u_standard_input(Read_line_Param),
	cl_read_line(Read_line_Param, Read_line_Ret),
	(   f_u_eq_c63([quote, u_y], u_response, FORM1_Res),
	    FORM1_Res\==[],
	    IFTEST=FORM1_Res
	->  true
	;   f_u_eq_c63([quote, u_yes], u_response, Response),
	    IFTEST=Response
	),
	(   IFTEST\==[]
	->  ElseResult27=t
	;   (   f_u_eq_c63([quote, n], u_response, FORM1_Res20),
		FORM1_Res20\==[],
		IFTEST18=FORM1_Res20
	    ->  true
	    ;   f_u_eq_c63([quote, u_no], u_response, Response32),
		IFTEST18=Response32
	    ),
	    (   IFTEST18\==[]
	    ->  ElseResult27=[]
	    ;   get_var(LEnv, u_else, IFTEST21),
		(   IFTEST21\==[]
		->  f_u_standard_output(Standard_output_Ret38),
		    cl_format(
			      [ Standard_output_Ret38,
				'$ARRAY'([*],
					 claz_base_character,
					 
					 [ #\('P'),
					   #\(l),
					   #\(e),
					   #\(a),
					   #\(s),
					   #\(e),
					   #\(' '),
					   #\(t),
					   #\(y),
					   #\(p),
					   #\(e),
					   #\(' '),
					   #\('\''),
					   #\(y),
					   #\('\''),
					   #\(' '),
					   #\(o),
					   #\(r),
					   #\(' '),
					   #\('\''),
					   #\(n),
					   #\('\''),
					   #\(' '),
					   #\(a),
					   #\(s),
					   #\(' '),
					   #\(a),
					   #\(' '),
					   #\(r),
					   #\(e),
					   #\(s),
					   #\(p),
					   #\(o),
					   #\(n),
					   #\(s),
					   #\(e),
					   #\('.'),
					   #\(~),
					   #\('%')
					 ])
			      ],
			      Format_Ret39),
		    f_u_interrogate(string, TrueResult),
		    ElseResult27=TrueResult
		;   ElseResult27=[]
		)
	    )
	).
:- set_opv(f_u_interrogate, classof, claz_function),
   set_opv(u_interrogate, compile_as, kw_function),
   set_opv(u_interrogate, function, f_u_interrogate),
   DefunResult=u_interrogate.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:6987 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'arg-value',
			    ['arg-name', 'init-plist', default],
			    
			    [ let,
			      [[found, [assq, 'arg-name', 'init-plist']]],
			      
			      [ if,
				[and, found, ['neq?', [cadr, found], [quote, none]]],
				[cadr, found],
				
				[ if,
				  ['eq?', default, [quote, required]],
				  
				  [ error,
				    '$STRING'("Required make-instance argument ~A not supplied"),
				    'arg-name'
				  ],
				  default
				]
			      ]
			    ]
			  ]).

% annotating U::ARG-VALUE 
wl: lambda_def(defun,
	      u_arg_value,
	      f_u_arg_value,
	      [u_arg_name, u_init_plist, u_default],
	      
	      [ 
		[ let,
		  [[u_found, [ext_assq, u_arg_name, u_init_plist]]],
		  
		  [ if,
		    [and, u_found, [u_neq_c63, [cadr, u_found], [quote, u_none]]],
		    [cadr, u_found],
		    
		    [ if,
		      [u_eq_c63, u_default, [quote, u_required]],
		      
		      [ error,
			'$ARRAY'([*],
				 claz_base_character,
				 
				 [ #\('R'),
				   #\(e),
				   #\(q),
				   #\(u),
				   #\(i),
				   #\(r),
				   #\(e),
				   #\(d),
				   #\(' '),
				   #\(m),
				   #\(a),
				   #\(k),
				   #\(e),
				   #\(-),
				   #\(i),
				   #\(n),
				   #\(s),
				   #\(t),
				   #\(a),
				   #\(n),
				   #\(c),
				   #\(e),
				   #\(' '),
				   #\(a),
				   #\(r),
				   #\(g),
				   #\(u),
				   #\(m),
				   #\(e),
				   #\(n),
				   #\(t),
				   #\(' '),
				   #\(~),
				   #\('A'),
				   #\(' '),
				   #\(n),
				   #\(o),
				   #\(t),
				   #\(' '),
				   #\(s),
				   #\(u),
				   #\(p),
				   #\(p),
				   #\(l),
				   #\(i),
				   #\(e),
				   #\(d)
				 ]),
			u_arg_name
		      ],
		      u_default
		    ]
		  ]
		]
	      ]).


% annotating U::ARG-VALUE 
wl: arglist_info(u_arg_value,
		[u_arg_name, u_init_plist, u_default],
		[Arg_name_Param, Init_plist_Param, Default_Param],
		arginfo{ all:[u_arg_name, u_init_plist, u_default],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_arg_name, u_init_plist, u_default],
			 opt:0,
			 req:[u_arg_name, u_init_plist, u_default],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ARG-VALUE 
wl: init_args(exact_only, u_arg_value).


% annotating U::ARG-VALUE 
f_u_arg_value(Arg_name_Param, Init_plist_Param, Default_Param, ElseResult34) :-
	Env=[bv(u_arg_name, Arg_name_Param), bv(u_init_plist, Init_plist_Param), bv(u_default, Default_Param)],
	f_ext_assq(u_arg_name, u_init_plist, Found_Init),
	LEnv=[[bv(u_found, Found_Init)]|Env],
	get_var(LEnv, u_found, IFTEST21),
	(   IFTEST21\==[]
	->  f_u_neq_c63([cadr, u_found], [quote, u_none], TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get26),
	    cl_cadr(Found_Get26, TrueResult33),
	    ElseResult34=TrueResult33
	;   f_u_eq_c63(u_default, [quote, u_required], IFTEST27),
	    (   IFTEST27\==[]
	    ->  cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    
				    [ #\('R'),
				      #\(e),
				      #\(q),
				      #\(u),
				      #\(i),
				      #\(r),
				      #\(e),
				      #\(d),
				      #\(' '),
				      #\(m),
				      #\(a),
				      #\(k),
				      #\(e),
				      #\(-),
				      #\(i),
				      #\(n),
				      #\(s),
				      #\(t),
				      #\(a),
				      #\(n),
				      #\(c),
				      #\(e),
				      #\(' '),
				      #\(a),
				      #\(r),
				      #\(g),
				      #\(u),
				      #\(m),
				      #\(e),
				      #\(n),
				      #\(t),
				      #\(' '),
				      #\(~),
				      #\('A'),
				      #\(' '),
				      #\(n),
				      #\(o),
				      #\(t),
				      #\(' '),
				      #\(s),
				      #\(u),
				      #\(p),
				      #\(p),
				      #\(l),
				      #\(i),
				      #\(e),
				      #\(d)
				    ]),
			   Arg_name_Param
			 ],
			 TrueResult31),
		ElseResult34=TrueResult31
	    ;   ElseResult34=Default_Param
	    )
	).
:- set_opv(f_u_arg_value, classof, claz_function),
   set_opv(u_arg_value, compile_as, kw_function),
   set_opv(u_arg_value, function, f_u_arg_value),
   DefunResult=u_arg_value.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7341 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'walk-append',
			    [proc, lst],
			    
			    [ yloop,
			      [initial, [result, []]],
			      [ywhile, lst],
			      
			      [ ydo,
				
				[ setq,
				  result,
				  ['append!', result, [funcall, proc, [car, lst]]]
				],
				[setq, lst, [cdr, lst]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::WALK-APPEND 
wl: lambda_def(defun,
	      u_walk_append,
	      f_u_walk_append,
	      [u_proc, u_lst],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []]],
		  [u_ywhile, u_lst],
		  
		  [ u_ydo,
		    
		    [ setq,
		      u_result,
		      [u_append_c33, u_result, [funcall, u_proc, [car, u_lst]]]
		    ],
		    [setq, u_lst, [cdr, u_lst]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::WALK-APPEND 
wl: arglist_info(u_walk_append,
		[u_proc, u_lst],
		[Proc_Param, Lst_Param],
		arginfo{ all:[u_proc, u_lst],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_proc, u_lst],
			 opt:0,
			 req:[u_proc, u_lst],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::WALK-APPEND 
wl: init_args(exact_only, u_walk_append).


% annotating U::WALK-APPEND 
f_u_walk_append(Proc_Param, Lst_Param, FnResult) :-
	Env=[bv(u_proc, Proc_Param), bv(u_lst, Lst_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []]],
		    [u_ywhile, u_lst],
		    
		    [ u_ydo,
		      
		      [ setq,
			u_result,
			[u_append_c33, u_result, [funcall, u_proc, [car, u_lst]]]
		      ],
		      [setq, u_lst, [cdr, u_lst]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_walk_append, classof, claz_function),
   set_opv(u_walk_append, compile_as, kw_function),
   set_opv(u_walk_append, function, f_u_walk_append),
   DefunResult=u_walk_append.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7555 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'with-default',
			    [val, default],
			    [if, val, val, default]
			  ]).

% annotating U::WITH-DEFAULT 
wl: lambda_def(defun,
	      u_with_default,
	      f_u_with_default,
	      [u_val, u_default],
	      [[if, u_val, u_val, u_default]]).


% annotating U::WITH-DEFAULT 
wl: arglist_info(u_with_default,
		[u_val, u_default],
		[Val_Param, Default_Param],
		arginfo{ all:[u_val, u_default],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_val, u_default],
			 opt:0,
			 req:[u_val, u_default],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::WITH-DEFAULT 
wl: init_args(exact_only, u_with_default).


% annotating U::WITH-DEFAULT 
f_u_with_default(Val_Param, Default_Param, FnResult) :-
	(   Val_Param\==[]
	->  FnResult=Val_Param
	;   FnResult=Default_Param
	).
:- set_opv(f_u_with_default, classof, claz_function),
   set_opv(u_with_default, compile_as, kw_function),
   set_opv(u_with_default, function, f_u_with_default),
   DefunResult=u_with_default.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7615 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'random-integer',
			    [from, to],
			    
			    [ cond,
			      [[=, to, from], to],
			      [[<, to, from], ['random-integer', to, from]],
			      [else, [+, from, [random, ['1+', [-, to, from]]]]]
			    ]
			  ]).

% annotating U::RANDOM-INTEGER 
wl: lambda_def(defun,
	      u_random_integer,
	      f_u_random_integer,
	      [u_from, u_to],
	      
	      [ 
		[ cond,
		  [[=, u_to, u_from], u_to],
		  [[<, u_to, u_from], [u_random_integer, u_to, u_from]],
		  [u_else, [+, u_from, [random, ['1+', [-, u_to, u_from]]]]]
		]
	      ]).


% annotating U::RANDOM-INTEGER 
wl: arglist_info(u_random_integer,
		[u_from, u_to],
		[From_Param, To_Get33],
		arginfo{ all:[u_from, u_to],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_from, u_to],
			 opt:0,
			 req:[u_from, u_to],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RANDOM-INTEGER 
wl: init_args(exact_only, u_random_integer).


% annotating U::RANDOM-INTEGER 
f_u_random_integer(From_Param, To_Get33, ElseResult38) :-
	Env=[bv(u_from, From_Param), bv(u_to, To_Get33)],
	(   To_Get33=:=From_Param
	->  ElseResult38=To_Get33
	;   To_Get33<From_Param
	->  f_u_random_integer(To_Get33, From_Param, TrueResult37),
	    ElseResult38=TrueResult37
	;   get_var(Env, u_else, IFTEST29),
	    (   IFTEST29\==[]
	    ->  -(To_Get33, From_Param, _139146),
		'1+'(_139146, Random_Param),
		cl_random(Random_Param, Random_Ret),
		+(From_Param, Random_Ret, TrueResult),
		ElseResult38=TrueResult
	    ;   ElseResult38=[]
	    )
	).
:- set_opv(f_u_random_integer, classof, claz_function),
   set_opv(u_random_integer, compile_as, kw_function),
   set_opv(u_random_integer, function, f_u_random_integer),
   DefunResult=u_random_integer.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7772 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'random-element',
			    [x],
			    
			    [ 'nth-elem',
			      x,
			      ['random-integer', 0, ['-1+', [length, x]]]
			    ]
			  ]).

% annotating U::RANDOM-ELEMENT 
wl: lambda_def(defun,
	      u_random_element,
	      f_u_random_element,
	      [u_x],
	      [[u_nth_elem, u_x, [u_random_integer, 0, ['-1+', [length, u_x]]]]]).


% annotating U::RANDOM-ELEMENT 
wl: arglist_info(u_random_element,
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

% annotating U::RANDOM-ELEMENT 
wl: init_args(exact_only, u_random_element).


% annotating U::RANDOM-ELEMENT 
f_u_random_element(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_nth_elem(u_x,
		     [u_random_integer, 0, ['-1+', [length, u_x]]],
		     Nth_elem_Ret),
	Nth_elem_Ret=FnResult.
:- set_opv(f_u_random_element, classof, claz_function),
   set_opv(u_random_element, compile_as, kw_function),
   set_opv(u_random_element, function, f_u_random_element),
   DefunResult=u_random_element.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:7851 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    randomize,
			    [x],
			    
			    [ yloop,
			      [initial, [result, []], [elem, []]],
			      [ywhile, x],
			      
			      [ ydo,
				[setq, elem, ['random-element', x]],
				[setq, x, [delq, elem, x]],
				[setq, result, [cons, elem, result]]
			      ],
			      [yresult, result]
			    ]
			  ]).

% annotating U::RANDOMIZE 
wl: lambda_def(defun,
	      u_randomize,
	      f_u_randomize,
	      [u_x],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [u_result, []], [u_elem, []]],
		  [u_ywhile, u_x],
		  
		  [ u_ydo,
		    [setq, u_elem, [u_random_element, u_x]],
		    [setq, u_x, [u_delq, u_elem, u_x]],
		    [setq, u_result, [cons, u_elem, u_result]]
		  ],
		  [u_yresult, u_result]
		]
	      ]).


% annotating U::RANDOMIZE 
wl: arglist_info(u_randomize,
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

% annotating U::RANDOMIZE 
wl: init_args(exact_only, u_randomize).


% annotating U::RANDOMIZE 
f_u_randomize(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_yloop(
		  [ [u_initial, [u_result, []], [u_elem, []]],
		    [u_ywhile, u_x],
		    
		    [ u_ydo,
		      [setq, u_elem, [u_random_element, u_x]],
		      [setq, u_x, [u_delq, u_elem, u_x]],
		      [setq, u_result, [cons, u_elem, u_result]]
		    ],
		    [u_yresult, u_result]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_randomize, classof, claz_function),
   set_opv(u_randomize, compile_as, kw_function),
   set_opv(u_randomize, function, f_u_randomize),
   DefunResult=u_randomize.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:8104 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'force-flonum',
			    [x],
			    [if, ['flonum?', x], x, ['fixnum->flonum', x]]
			  ]).

% annotating U::FORCE-FLONUM 
wl: lambda_def(defun,
	      u_force_flonum,
	      f_u_force_flonum,
	      [u_x],
	      [[if, [u_flonum_c63, u_x], u_x, [u_fixnum_c62_flonum, u_x]]]).


% annotating U::FORCE-FLONUM 
wl: arglist_info(u_force_flonum,
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

% annotating U::FORCE-FLONUM 
wl: init_args(exact_only, u_force_flonum).


% annotating U::FORCE-FLONUM 
f_u_force_flonum(X_Param, FnResult) :-
	Env=[bv(u_x, X_Param)],
	f_u_flonum_c63(u_x, IFTEST),
	(   IFTEST\==[]
	->  FnResult=X_Param
	;   f_u_fixnum_c62_flonum(u_x, ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_u_force_flonum, classof, claz_function),
   set_opv(u_force_flonum, compile_as, kw_function),
   set_opv(u_force_flonum, function, f_u_force_flonum),
   DefunResult=u_force_flonum.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:8170 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    'random-real',
			    [from, to],
			    [setq, '*large-integer*', ['random-integer', 4, 20]],
			    
			    [ cond,
			      [[=, to, from], to],
			      [[<, to, from], ['random-real', to, from]],
			      
			      [ else,
				
				[ (+),
				  from,
				  
				  [ (*),
				    [-, to, from],
				    
				    [ (/),
				      
				      [ 'force-flonum',
					['random-integer', 0, '*large-integer*']
				      ],
				      ['force-flonum', '*large-integer*']
				    ]
				  ]
				]
			      ]
			    ]
			  ]).

% annotating U::RANDOM-REAL 
wl: lambda_def(defun,
	      u_random_real,
	      f_u_random_real,
	      [u_from, u_to],
	      
	      [ [setq, u_xx_large_integer_xx, [u_random_integer, 4, 20]],
		
		[ cond,
		  [[=, u_to, u_from], u_to],
		  [[<, u_to, u_from], [u_random_real, u_to, u_from]],
		  
		  [ u_else,
		    
		    [ (+),
		      u_from,
		      
		      [ (*),
			[-, u_to, u_from],
			
			[ (/),
			  
			  [ u_force_flonum,
			    [u_random_integer, 0, u_xx_large_integer_xx]
			  ],
			  [u_force_flonum, u_xx_large_integer_xx]
			]
		      ]
		    ]
		  ]
		]
	      ]).


% annotating U::RANDOM-REAL 
wl: arglist_info(u_random_real,
		[u_from, u_to],
		[From_Param, To_Get33],
		arginfo{ all:[u_from, u_to],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_from, u_to],
			 opt:0,
			 req:[u_from, u_to],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::RANDOM-REAL 
wl: init_args(exact_only, u_random_real).


% annotating U::RANDOM-REAL 
f_u_random_real(From_Param, To_Get33, ElseResult40) :-
	Env=[bv(u_from, From_Param), bv(u_to, To_Get33)],
	f_u_random_integer(4, 20, Xx_large_integer_xx),
	set_var(Env, u_xx_large_integer_xx, Xx_large_integer_xx),
	(   To_Get33=:=From_Param
	->  ElseResult40=To_Get33
	;   To_Get33<From_Param
	->  f_u_random_real(To_Get33, From_Param, TrueResult39),
	    ElseResult40=TrueResult39
	;   get_var(Env, u_else, IFTEST29),
	    (   IFTEST29\==[]
	    ->  -(To_Get33, From_Param, _139540),
		get_var(Env, u_xx_large_integer_xx, Xx_large_integer_xx_Get36),
		f_u_random_integer(0,
				   Xx_large_integer_xx_Get36,
				   Force_flonum_Param),
		f_u_force_flonum(Force_flonum_Param, Force_flonum_Ret),
		f_u_force_flonum(Xx_large_integer_xx_Get36, Force_flonum_Ret48),
		/(Force_flonum_Ret, Force_flonum_Ret48, _139652),
		*(_139540, _139652, _139538),
		+(From_Param, _139538, TrueResult),
		ElseResult40=TrueResult
	    ;   ElseResult40=[]
	    )
	).
:- set_opv(f_u_random_real, classof, claz_function),
   set_opv(u_random_real, compile_as, kw_function),
   set_opv(u_random_real, function, f_u_random_real),
   DefunResult=u_random_real.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_utils.cl:8170 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 8498)).
:- true.


% Total time: 3.794 seconds


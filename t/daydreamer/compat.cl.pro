
% WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
% File: "compat" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl)
% PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/
% Start time: Tue Dec 19 21:14:37 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:0 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("*******************************************************************************",
				     1,
				     1)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:81 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 82)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:83 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" GATE", 1, 84)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:90 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" Version 2.3", 1, 91)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:104 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 105)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:106 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.",
				     1,
				     107)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:176 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" All Rights Reserved.", 1, 177)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:199 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 200)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:201 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" This file contains:", 1, 202)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:223 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'(" Compatibility functions for T2.8/T3 (Scheme) code running under Common Lisp",
				     1,
				     224)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:301 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 302)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:303 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" 19990429: begun", 1, 304)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:321 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" 19990503: more work", 1, 322)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:343 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("", 1, 344)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:345 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, else, t]).
:- set_var(TLEnv3, setq, u_else, t).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:360 **********************/
:- lisp_compile_to_prolog(pkg_user, [setq, '*repl-wont-print*', []]).
:- set_var(TLEnv3, setq, u_xx_repl_wont_print_xx, []).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:389 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, 't-or-nil', [a], [if, a, [quote, t], []]]).

% annotating U::T-OR-NIL 
wl: lambda_def(defun, u_t_or_nil, f_u_t_or_nil, [u_a], [[if, u_a, [quote, t], []]]).


% annotating U::T-OR-NIL 
wl: arglist_info(u_t_or_nil,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::T-OR-NIL 
wl: init_args(exact_only, u_t_or_nil).


% annotating U::T-OR-NIL 
f_u_t_or_nil(A_Param, FnResult) :-
	(   A_Param\==[]
	->  FnResult=t
	;   FnResult=[]
	).
:- set_opv(f_u_t_or_nil, classof, claz_function),
   set_opv(u_t_or_nil, compile_as, kw_function),
   set_opv(u_t_or_nil, function, f_u_t_or_nil),
   DefunResult=u_t_or_nil.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:424 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string->symbol',
			    [a],
			    ['#BQ', [intern, ['#COMMA', a]]]
			  ]).

% annotating U::STRING->SYMBOL 
wl: lambda_def(defmacro,
	      u_string_c62_symbol,
	      f_u_string_c62_symbol,
	      [u_a],
	      [progn, ['#BQ', [intern, ['#COMMA', u_a]]]]).


% annotating U::STRING->SYMBOL 
wl: arglist_info(u_string_c62_symbol,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING->SYMBOL 
wl: init_args(exact_only, u_string_c62_symbol).


% annotating U::STRING->SYMBOL 
f_u_string_c62_symbol(A_Param, FnResult) :-
	[intern, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_c62_symbol, classof, claz_macro),
   set_opv(u_string_c62_symbol, compile_as, kw_operator),
   set_opv(u_string_c62_symbol, function, f_u_string_c62_symbol),
   DefMacroResult=u_string_c62_symbol.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:467 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'symbol->string',
			    [a],
			    ['#BQ', ['symbol-name', ['#COMMA', a]]]
			  ]).

% annotating U::SYMBOL->STRING 
wl: lambda_def(defmacro,
	      u_symbol_c62_string,
	      f_u_symbol_c62_string,
	      [u_a],
	      [progn, ['#BQ', [symbol_name, ['#COMMA', u_a]]]]).


% annotating U::SYMBOL->STRING 
wl: arglist_info(u_symbol_c62_string,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::SYMBOL->STRING 
wl: init_args(exact_only, u_symbol_c62_string).


% annotating U::SYMBOL->STRING 
f_u_symbol_c62_string(A_Param, FnResult) :-
	[symbol_name, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_symbol_c62_string, classof, claz_macro),
   set_opv(u_symbol_c62_string, compile_as, kw_operator),
   set_opv(u_symbol_c62_string, function, f_u_symbol_c62_string),
   DefMacroResult=u_symbol_c62_string.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:515 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-length',
			    [a],
			    ['#BQ', [length, ['#COMMA', a]]]
			  ]).

% annotating U::STRING-LENGTH 
wl: lambda_def(defmacro,
	      u_string_length,
	      f_u_string_length,
	      [u_a],
	      [progn, ['#BQ', [length, ['#COMMA', u_a]]]]).


% annotating U::STRING-LENGTH 
wl: arglist_info(u_string_length,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-LENGTH 
wl: init_args(exact_only, u_string_length).


% annotating U::STRING-LENGTH 
f_u_string_length(A_Param, FnResult) :-
	[length, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_length, classof, claz_macro),
   set_opv(u_string_length, compile_as, kw_operator),
   set_opv(u_string_length, function, f_u_string_length),
   DefMacroResult=u_string_length.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:557 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-empty?',
			    [a],
			    ['#BQ', [=, [length, ['#COMMA', a]], 0]]
			  ]).

% annotating U::STRING-EMPTY? 
wl: lambda_def(defmacro,
	      u_string_empty_c63,
	      f_u_string_empty_c63,
	      [u_a],
	      [progn, ['#BQ', [=, [length, ['#COMMA', u_a]], 0]]]).


% annotating U::STRING-EMPTY? 
wl: arglist_info(u_string_empty_c63,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-EMPTY? 
wl: init_args(exact_only, u_string_empty_c63).


% annotating U::STRING-EMPTY? 
f_u_string_empty_c63(A_Param, FnResult) :-
	[=, [length, A_Param], 0]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_empty_c63, classof, claz_macro),
   set_opv(u_string_empty_c63, compile_as, kw_operator),
   set_opv(u_string_empty_c63, function, f_u_string_empty_c63),
   DefMacroResult=u_string_empty_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:605 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-slice',
			    [a, b, c],
			    
			    [ '#BQ',
			      [subseq, ['#COMMA', a], ['#COMMA', b], ['#COMMA', c]]
			    ]
			  ]).

% annotating U::STRING-SLICE 
wl: lambda_def(defmacro,
	      u_string_slice,
	      f_u_string_slice,
	      [u_a, u_b, u_c],
	      
	      [ progn,
		['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], ['#COMMA', u_c]]]
	      ]).


% annotating U::STRING-SLICE 
wl: arglist_info(u_string_slice,
		[u_a, u_b, u_c],
		[A_Param, B_Param, C_Param],
		arginfo{ all:[u_a, u_b, u_c],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b, u_c],
			 opt:0,
			 req:[u_a, u_b, u_c],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-SLICE 
wl: init_args(exact_only, u_string_slice).


% annotating U::STRING-SLICE 
f_u_string_slice(A_Param, B_Param, C_Param, FnResult) :-
	[subseq, A_Param, B_Param, C_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_slice, classof, claz_macro),
   set_opv(u_string_slice, compile_as, kw_operator),
   set_opv(u_string_slice, function, f_u_string_slice),
   DefMacroResult=u_string_slice.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:656 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    substring,
			    [a, b, c],
			    
			    [ '#BQ',
			      [subseq, ['#COMMA', a], ['#COMMA', b], ['#COMMA', c]]
			    ]
			  ]).

% annotating EXT:SUBSTRING 
wl: lambda_def(defmacro,
	      ext_substring,
	      f_ext_substring,
	      [u_a, u_b, u_c],
	      
	      [ progn,
		['#BQ', [subseq, ['#COMMA', u_a], ['#COMMA', u_b], ['#COMMA', u_c]]]
	      ]).


% annotating EXT:SUBSTRING 
wl: arglist_info(ext_substring,
		[u_a, u_b, u_c],
		[A_Param, B_Param, C_Param],
		arginfo{ all:[u_a, u_b, u_c],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b, u_c],
			 opt:0,
			 req:[u_a, u_b, u_c],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating EXT:SUBSTRING 
wl: init_args(exact_only, ext_substring).


% annotating EXT:SUBSTRING 
f_ext_substring(A_Param, B_Param, C_Param, FnResult) :-
	[subseq, A_Param, B_Param, C_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_substring, classof, claz_macro),
   set_opv(ext_substring, compile_as, kw_operator),
   set_opv(ext_substring, function, f_ext_substring),
   DefMacroResult=ext_substring.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:704 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-nthtail',
			    [a, b],
			    
			    [ '#BQ',
			      
			      [ subseq,
				['#COMMA', a],
				['#COMMA', b],
				[length, ['#COMMA', a]]
			      ]
			    ]
			  ]).

% annotating U::STRING-NTHTAIL 
wl: lambda_def(defmacro,
	      u_string_nthtail,
	      f_u_string_nthtail,
	      [u_a, u_b],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ subseq,
		    ['#COMMA', u_a],
		    ['#COMMA', u_b],
		    [length, ['#COMMA', u_a]]
		  ]
		]
	      ]).


% annotating U::STRING-NTHTAIL 
wl: arglist_info(u_string_nthtail,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-NTHTAIL 
wl: init_args(exact_only, u_string_nthtail).


% annotating U::STRING-NTHTAIL 
f_u_string_nthtail(A_Param, B_Param, FnResult) :-
	[subseq, A_Param, B_Param, [length, A_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_nthtail, classof, claz_macro),
   set_opv(u_string_nthtail, compile_as, kw_operator),
   set_opv(u_string_nthtail, function, f_u_string_nthtail),
   DefMacroResult=u_string_nthtail.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:764 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    nthchdr,
			    [a, b],
			    
			    [ '#BQ',
			      
			      [ subseq,
				['#COMMA', a],
				['#COMMA', b],
				[length, ['#COMMA', a]]
			      ]
			    ]
			  ]).

% annotating U::NTHCHDR 
wl: lambda_def(defmacro,
	      u_nthchdr,
	      f_u_nthchdr,
	      [u_a, u_b],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ subseq,
		    ['#COMMA', u_a],
		    ['#COMMA', u_b],
		    [length, ['#COMMA', u_a]]
		  ]
		]
	      ]).


% annotating U::NTHCHDR 
wl: arglist_info(u_nthchdr,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NTHCHDR 
wl: init_args(exact_only, u_nthchdr).


% annotating U::NTHCHDR 
f_u_nthchdr(A_Param, B_Param, FnResult) :-
	[subseq, A_Param, B_Param, [length, A_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nthchdr, classof, claz_macro),
   set_opv(u_nthchdr, compile_as, kw_operator),
   set_opv(u_nthchdr, function, f_u_nthchdr),
   DefMacroResult=u_nthchdr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:817 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-downcase!',
			    [a],
			    ['#BQ', ['string-downcase', ['#COMMA', a]]]
			  ]).

% annotating U::STRING-DOWNCASE! 
wl: lambda_def(defmacro,
	      u_string_downcase_c33,
	      f_u_string_downcase_c33,
	      [u_a],
	      [progn, ['#BQ', [string_downcase, ['#COMMA', u_a]]]]).


% annotating U::STRING-DOWNCASE! 
wl: arglist_info(u_string_downcase_c33,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-DOWNCASE! 
wl: init_args(exact_only, u_string_downcase_c33).


% annotating U::STRING-DOWNCASE! 
f_u_string_downcase_c33(A_Param, FnResult) :-
	[string_downcase, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_downcase_c33, classof, claz_macro),
   set_opv(u_string_downcase_c33, compile_as, kw_operator),
   set_opv(u_string_downcase_c33, function, f_u_string_downcase_c33),
   DefMacroResult=u_string_downcase_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:871 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-write',
			    [a, b],
			    ['#BQ', ['write-string', ['#COMMA', b], ['#COMMA', a]]]
			  ]).

% annotating U::STRING-WRITE 
wl: lambda_def(defmacro,
	      u_string_write,
	      f_u_string_write,
	      [u_a, u_b],
	      [progn, ['#BQ', [write_string, ['#COMMA', u_b], ['#COMMA', u_a]]]]).


% annotating U::STRING-WRITE 
wl: arglist_info(u_string_write,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-WRITE 
wl: init_args(exact_only, u_string_write).


% annotating U::STRING-WRITE 
f_u_string_write(A_Param, B_Param, FnResult) :-
	[write_string, B_Param, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_write, classof, claz_macro),
   set_opv(u_string_write, compile_as, kw_operator),
   set_opv(u_string_write, function, f_u_string_write),
   DefMacroResult=u_string_write.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:923 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'map-string!',
			    [a, b],
			    
			    [ '#BQ',
			      [map, ['#BQ', string], ['#COMMA', a], ['#COMMA', b]]
			    ]
			  ]).

% annotating U::MAP-STRING! 
wl: lambda_def(defmacro,
	      u_map_string_c33,
	      f_u_map_string_c33,
	      [u_a, u_b],
	      
	      [ progn,
		['#BQ', [map, ['#BQ', string], ['#COMMA', u_a], ['#COMMA', u_b]]]
	      ]).


% annotating U::MAP-STRING! 
wl: arglist_info(u_map_string_c33,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MAP-STRING! 
wl: init_args(exact_only, u_map_string_c33).


% annotating U::MAP-STRING! 
f_u_map_string_c33(A_Param, B_Param, FnResult) :-
	[map, [quote, string], A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_map_string_c33, classof, claz_macro),
   set_opv(u_map_string_c33, compile_as, kw_operator),
   set_opv(u_map_string_c33, function, f_u_map_string_c33),
   DefMacroResult=u_map_string_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:973 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-equal?',
			    [a, b],
			    ['#BQ', ['string-equal', ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::STRING-EQUAL? 
wl: lambda_def(defmacro,
	      u_string_equal_c63,
	      f_u_string_equal_c63,
	      [u_a, u_b],
	      [progn, ['#BQ', [string_equal, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::STRING-EQUAL? 
wl: arglist_info(u_string_equal_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-EQUAL? 
wl: init_args(exact_only, u_string_equal_c63).


% annotating U::STRING-EQUAL? 
f_u_string_equal_c63(A_Param, B_Param, FnResult) :-
	[string_equal, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_equal_c63, classof, claz_macro),
   set_opv(u_string_equal_c63, compile_as, kw_operator),
   set_opv(u_string_equal_c63, function, f_u_string_equal_c63),
   DefMacroResult=u_string_equal_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1026 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    chdr,
			    [a],
			    
			    [ '#BQ',
			      [subseq, ['#COMMA', a], 1, [length, ['#COMMA', a]]]
			    ]
			  ]).

% annotating U::CHDR 
wl: lambda_def(defmacro,
	      u_chdr,
	      f_u_chdr,
	      [u_a],
	      
	      [ progn,
		['#BQ', [subseq, ['#COMMA', u_a], 1, [length, ['#COMMA', u_a]]]]
	      ]).


% annotating U::CHDR 
wl: arglist_info(u_chdr,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::CHDR 
wl: init_args(exact_only, u_chdr).


% annotating U::CHDR 
f_u_chdr(A_Param, FnResult) :-
	[subseq, A_Param, 1, [length, A_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_chdr, classof, claz_macro),
   set_opv(u_chdr, compile_as, kw_operator),
   set_opv(u_chdr, function, f_u_chdr),
   DefMacroResult=u_chdr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1073 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    nthchar,
			    [a, b],
			    ['#BQ', [elt, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::NTHCHAR 
wl: lambda_def(defmacro,
	      u_nthchar,
	      f_u_nthchar,
	      [u_a, u_b],
	      [progn, ['#BQ', [elt, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::NTHCHAR 
wl: arglist_info(u_nthchar,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NTHCHAR 
wl: init_args(exact_only, u_nthchar).


% annotating U::NTHCHAR 
f_u_nthchar(A_Param, B_Param, FnResult) :-
	[elt, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nthchar, classof, claz_macro),
   set_opv(u_nthchar, compile_as, kw_operator),
   set_opv(u_nthchar, function, f_u_nthchar),
   DefMacroResult=u_nthchar.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1111 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'digit?',
			    [a, b],
			    ['#BQ', ['digit-char-p', ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::DIGIT? 
wl: lambda_def(defmacro,
	      u_digit_c63,
	      f_u_digit_c63,
	      [u_a, u_b],
	      [progn, ['#BQ', [digit_char_p, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::DIGIT? 
wl: arglist_info(u_digit_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DIGIT? 
wl: init_args(exact_only, u_digit_c63).


% annotating U::DIGIT? 
f_u_digit_c63(A_Param, B_Param, FnResult) :-
	[digit_char_p, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_digit_c63, classof, claz_macro),
   set_opv(u_digit_c63, compile_as, kw_operator),
   set_opv(u_digit_c63, function, f_u_digit_c63),
   DefMacroResult=u_digit_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1157 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-append',
			    ['&rest', args],
			    
			    [ '#BQ',
			      
			      [ concatenate,
				[quote, string],
				['#BQ-COMMA-ELIPSE', args]
			      ]
			    ]
			  ]).

% annotating U::STRING-APPEND 
wl: lambda_def(defmacro,
	      u_string_append,
	      f_u_string_append,
	      [c38_rest, args],
	      
	      [ progn,
		['#BQ', [concatenate, [quote, string], ['#BQ-COMMA-ELIPSE', args]]]
	      ]).


% annotating U::STRING-APPEND 
wl: arglist_info(u_string_append,
		[c38_rest, args],
		[args],
		arginfo{ all:0,
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:[rest],
			 env:0,
			 key:0,
			 names:[args],
			 opt:0,
			 req:0,
			 rest:[args],
			 sublists:0,
			 whole:0
		       }).


% annotating U::STRING-APPEND 
wl: init_args(rest_only, u_string_append).


% annotating U::STRING-APPEND 
f_u_string_append(Whole, FnResult) :-
	append([], Args_Param, Whole),
	TLEnv3=[bv(args, Args_Param)],
	get_var(TLEnv3, args, Args_Get),
	[concatenate, [quote, string]|Args_Get]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_append, classof, claz_macro),
   set_opv(u_string_append, compile_as, kw_operator),
   set_opv(u_string_append, function, f_u_string_append),
   DefMacroResult=u_string_append.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1225 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'any?',
			    [a, b],
			    
			    [ '#BQ',
			      ['t-or-nil', [some, ['#COMMA', a], ['#COMMA', b]]]
			    ]
			  ]).

% annotating U::ANY? 
wl: lambda_def(defmacro,
	      u_any_c63,
	      f_u_any_c63,
	      [u_a, u_b],
	      
	      [ progn,
		['#BQ', [u_t_or_nil, [some, ['#COMMA', u_a], ['#COMMA', u_b]]]]
	      ]).


% annotating U::ANY? 
wl: arglist_info(u_any_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ANY? 
wl: init_args(exact_only, u_any_c63).


% annotating U::ANY? 
f_u_any_c63(A_Param, B_Param, FnResult) :-
	[u_t_or_nil, [some, A_Param, B_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_any_c63, classof, claz_macro),
   set_opv(u_any_c63, compile_as, kw_operator),
   set_opv(u_any_c63, function, f_u_any_c63),
   DefMacroResult=u_any_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1272 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    any,
			    [a, b],
			    ['#BQ', [some, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::ANY 
wl: lambda_def(defmacro,
	      u_any,
	      f_u_any,
	      [u_a, u_b],
	      [progn, ['#BQ', [some, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::ANY 
wl: arglist_info(u_any,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ANY 
wl: init_args(exact_only, u_any).


% annotating U::ANY 
f_u_any(A_Param, B_Param, FnResult) :-
	[some, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_any, classof, claz_macro),
   set_opv(u_any, compile_as, kw_operator),
   set_opv(u_any, function, f_u_any),
   DefMacroResult=u_any.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1307 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defmacro, 'null?', [a], ['#BQ', [null, ['#COMMA', a]]]]).

% annotating U::NULL? 
wl: lambda_def(defmacro,
	      u_null_c63,
	      f_u_null_c63,
	      [u_a],
	      [progn, ['#BQ', [null, ['#COMMA', u_a]]]]).


% annotating U::NULL? 
wl: arglist_info(u_null_c63,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NULL? 
wl: init_args(exact_only, u_null_c63).


% annotating U::NULL? 
f_u_null_c63(A_Param, FnResult) :-
	[null, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_null_c63, classof, claz_macro),
   set_opv(u_null_c63, compile_as, kw_operator),
   set_opv(u_null_c63, function, f_u_null_c63),
   DefMacroResult=u_null_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1339 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'eq?',
			    [a, b],
			    ['#BQ', [eql, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::EQ? 
wl: lambda_def(defmacro,
	      u_eq_c63,
	      f_u_eq_c63,
	      [u_a, u_b],
	      [progn, ['#BQ', [eql, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::EQ? 
wl: arglist_info(u_eq_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EQ? 
wl: init_args(exact_only, u_eq_c63).


% annotating U::EQ? 
f_u_eq_c63(A_Param, B_Param, FnResult) :-
	[eql, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_eq_c63, classof, claz_macro),
   set_opv(u_eq_c63, compile_as, kw_operator),
   set_opv(u_eq_c63, function, f_u_eq_c63),
   DefMacroResult=u_eq_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1373 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'alikeq?',
			    [a, b],
			    ['#BQ', [equalp, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::ALIKEQ? 
wl: lambda_def(defmacro,
	      u_alikeq_c63,
	      f_u_alikeq_c63,
	      [u_a, u_b],
	      [progn, ['#BQ', [equalp, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::ALIKEQ? 
wl: arglist_info(u_alikeq_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::ALIKEQ? 
wl: init_args(exact_only, u_alikeq_c63).


% annotating U::ALIKEQ? 
f_u_alikeq_c63(A_Param, B_Param, FnResult) :-
	[equalp, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_alikeq_c63, classof, claz_macro),
   set_opv(u_alikeq_c63, compile_as, kw_operator),
   set_opv(u_alikeq_c63, function, f_u_alikeq_c63),
   DefMacroResult=u_alikeq_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1414 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'neq?',
			    [a, b],
			    ['#BQ', [not, [eql, ['#COMMA', a], ['#COMMA', b]]]]
			  ]).

% annotating U::NEQ? 
wl: lambda_def(defmacro,
	      u_neq_c63,
	      f_u_neq_c63,
	      [u_a, u_b],
	      [progn, ['#BQ', [not, [eql, ['#COMMA', u_a], ['#COMMA', u_b]]]]]).


% annotating U::NEQ? 
wl: arglist_info(u_neq_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NEQ? 
wl: init_args(exact_only, u_neq_c63).


% annotating U::NEQ? 
f_u_neq_c63(A_Param, B_Param, FnResult) :-
	[not, [eql, A_Param, B_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_neq_c63, classof, claz_macro),
   set_opv(u_neq_c63, compile_as, kw_operator),
   set_opv(u_neq_c63, function, f_u_neq_c63),
   DefMacroResult=u_neq_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1455 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'memq?',
			    [a, b],
			    
			    [ '#BQ',
			      ['t-or-nil', [member, ['#COMMA', a], ['#COMMA', b]]]
			    ]
			  ]).

% annotating U::MEMQ? 
wl: lambda_def(defmacro,
	      u_memq_c63,
	      f_u_memq_c63,
	      [u_a, u_b],
	      
	      [ progn,
		['#BQ', [u_t_or_nil, [member, ['#COMMA', u_a], ['#COMMA', u_b]]]]
	      ]).


% annotating U::MEMQ? 
wl: arglist_info(u_memq_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MEMQ? 
wl: init_args(exact_only, u_memq_c63).


% annotating U::MEMQ? 
f_u_memq_c63(A_Param, B_Param, FnResult) :-
	[u_t_or_nil, [member, A_Param, B_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_memq_c63, classof, claz_macro),
   set_opv(u_memq_c63, compile_as, kw_operator),
   set_opv(u_memq_c63, function, f_u_memq_c63),
   DefMacroResult=u_memq_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1455 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #-abcl ", 1, 1507)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1517 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    memq,
			    [a, b],
			    ['#BQ', [member, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating EXT:MEMQ 
wl: lambda_def(defmacro,
	      ext_memq,
	      f_ext_memq,
	      [u_a, u_b],
	      [progn, ['#BQ', [member, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating EXT:MEMQ 
wl: arglist_info(ext_memq,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating EXT:MEMQ 
wl: init_args(exact_only, ext_memq).


% annotating EXT:MEMQ 
f_ext_memq(A_Param, B_Param, FnResult) :-
	[member, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_memq, classof, claz_macro),
   set_opv(ext_memq, compile_as, kw_operator),
   set_opv(ext_memq, function, f_ext_memq),
   DefMacroResult=ext_memq.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1555 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'gen-id',
			    [symbol],
			    ['#BQ', [gensym, ['#COMMA', symbol]]]
			  ]).

% annotating U::GEN-ID 
wl: lambda_def(defmacro,
	      u_gen_id,
	      f_u_gen_id,
	      [symbol],
	      [progn, ['#BQ', [gensym, ['#COMMA', symbol]]]]).


% annotating U::GEN-ID 
wl: arglist_info(u_gen_id,
		[symbol],
		[Symbol_Param],
		arginfo{ all:[symbol],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[symbol],
			 opt:0,
			 req:[symbol],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::GEN-ID 
wl: init_args(exact_only, u_gen_id).


% annotating U::GEN-ID 
f_u_gen_id(Symbol_Param, FnResult) :-
	[gensym, Symbol_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_gen_id, classof, claz_macro),
   set_opv(u_gen_id, compile_as, kw_operator),
   set_opv(u_gen_id, function, f_u_gen_id),
   DefMacroResult=u_gen_id.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1600 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    (div),
			    [a, b],
			    ['#BQ', [/, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::DIV 
wl: lambda_def(defmacro,
	      u_div,
	      f_u_div,
	      [u_a, u_b],
	      [progn, ['#BQ', [/, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::DIV 
wl: arglist_info(u_div,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DIV 
wl: init_args(exact_only, u_div).


% annotating U::DIV 
f_u_div(A_Param, B_Param, FnResult) :-
	[/, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_div, classof, claz_macro),
   set_opv(u_div, compile_as, kw_operator),
   set_opv(u_div, function, f_u_div),
   DefMacroResult=u_div.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1632 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'procedure?',
			    [x],
			    ['#BQ', [functionp, ['#COMMA', x]]]
			  ]).

% annotating U::PROCEDURE? 
wl: lambda_def(defmacro,
	      u_procedure_c63,
	      f_u_procedure_c63,
	      [u_x],
	      [progn, ['#BQ', [functionp, ['#COMMA', u_x]]]]).


% annotating U::PROCEDURE? 
wl: arglist_info(u_procedure_c63,
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

% annotating U::PROCEDURE? 
wl: init_args(exact_only, u_procedure_c63).


% annotating U::PROCEDURE? 
f_u_procedure_c63(X_Param, FnResult) :-
	[functionp, X_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_procedure_c63, classof, claz_macro),
   set_opv(u_procedure_c63, compile_as, kw_operator),
   set_opv(u_procedure_c63, function, f_u_procedure_c63),
   DefMacroResult=u_procedure_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1674 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'number?',
			    [x],
			    ['#BQ', [numberp, ['#COMMA', x]]]
			  ]).

% annotating U::NUMBER? 
wl: lambda_def(defmacro,
	      u_number_c63,
	      f_u_number_c63,
	      [u_x],
	      [progn, ['#BQ', [numberp, ['#COMMA', u_x]]]]).


% annotating U::NUMBER? 
wl: arglist_info(u_number_c63,
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

% annotating U::NUMBER? 
wl: init_args(exact_only, u_number_c63).


% annotating U::NUMBER? 
f_u_number_c63(X_Param, FnResult) :-
	[numberp, X_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_number_c63, classof, claz_macro),
   set_opv(u_number_c63, compile_as, kw_operator),
   set_opv(u_number_c63, function, f_u_number_c63),
   DefMacroResult=u_number_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1711 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'flonum?',
			    [x],
			    ['#BQ', [floatp, ['#COMMA', x]]]
			  ]).

% annotating U::FLONUM? 
wl: lambda_def(defmacro,
	      u_flonum_c63,
	      f_u_flonum_c63,
	      [u_x],
	      [progn, ['#BQ', [floatp, ['#COMMA', u_x]]]]).


% annotating U::FLONUM? 
wl: arglist_info(u_flonum_c63,
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

% annotating U::FLONUM? 
wl: init_args(exact_only, u_flonum_c63).


% annotating U::FLONUM? 
f_u_flonum_c63(X_Param, FnResult) :-
	[floatp, X_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_flonum_c63, classof, claz_macro),
   set_opv(u_flonum_c63, compile_as, kw_operator),
   set_opv(u_flonum_c63, function, f_u_flonum_c63),
   DefMacroResult=u_flonum_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1747 **********************/
:- lisp_compile_to_prolog(pkg_user, [defmacro, 'fixnum->flonum', [x], x]).

% annotating U::FIXNUM->FLONUM 
wl: lambda_def(defmacro,
	      u_fixnum_c62_flonum,
	      f_u_fixnum_c62_flonum,
	      [u_x],
	      [progn, u_x]).


% annotating U::FIXNUM->FLONUM 
wl: arglist_info(u_fixnum_c62_flonum,
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

% annotating U::FIXNUM->FLONUM 
wl: init_args(exact_only, u_fixnum_c62_flonum).


% annotating U::FIXNUM->FLONUM 
f_u_fixnum_c62_flonum(X_Param, FnResult) :-
	X_Param=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fixnum_c62_flonum, classof, claz_macro),
   set_opv(u_fixnum_c62_flonum, compile_as, kw_operator),
   set_opv(u_fixnum_c62_flonum, function, f_u_fixnum_c62_flonum),
   DefMacroResult=u_fixnum_c62_flonum.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1779 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'symbol?',
			    [x],
			    ['#BQ', [symbolp, ['#COMMA', x]]]
			  ]).

% annotating U::SYMBOL? 
wl: lambda_def(defmacro,
	      u_symbol_c63,
	      f_u_symbol_c63,
	      [u_x],
	      [progn, ['#BQ', [symbolp, ['#COMMA', u_x]]]]).


% annotating U::SYMBOL? 
wl: arglist_info(u_symbol_c63,
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

% annotating U::SYMBOL? 
wl: init_args(exact_only, u_symbol_c63).


% annotating U::SYMBOL? 
f_u_symbol_c63(X_Param, FnResult) :-
	[symbolp, X_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_symbol_c63, classof, claz_macro),
   set_opv(u_symbol_c63, compile_as, kw_operator),
   set_opv(u_symbol_c63, function, f_u_symbol_c63),
   DefMacroResult=u_symbol_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1816 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defmacro, 'pair?', [a], ['#BQ', [consp, ['#COMMA', a]]]]).

% annotating U::PAIR? 
wl: lambda_def(defmacro,
	      u_pair_c63,
	      f_u_pair_c63,
	      [u_a],
	      [progn, ['#BQ', [consp, ['#COMMA', u_a]]]]).


% annotating U::PAIR? 
wl: arglist_info(u_pair_c63,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::PAIR? 
wl: init_args(exact_only, u_pair_c63).


% annotating U::PAIR? 
f_u_pair_c63(A_Param, FnResult) :-
	[consp, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_pair_c63, classof, claz_macro),
   set_opv(u_pair_c63, compile_as, kw_operator),
   set_opv(u_pair_c63, function, f_u_pair_c63),
   DefMacroResult=u_pair_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1849 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string?',
			    [a],
			    ['#BQ', [stringp, ['#COMMA', a]]]
			  ]).

% annotating U::STRING? 
wl: lambda_def(defmacro,
	      u_string_c63,
	      f_u_string_c63,
	      [u_a],
	      [progn, ['#BQ', [stringp, ['#COMMA', u_a]]]]).


% annotating U::STRING? 
wl: arglist_info(u_string_c63,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING? 
wl: init_args(exact_only, u_string_c63).


% annotating U::STRING? 
f_u_string_c63(A_Param, FnResult) :-
	[stringp, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_c63, classof, claz_macro),
   set_opv(u_string_c63, compile_as, kw_operator),
   set_opv(u_string_c63, function, f_u_string_c63),
   DefMacroResult=u_string_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1886 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'uppercase?',
			    [x],
			    ['#BQ', ['upper-case-p', ['#COMMA', x]]]
			  ]).

% annotating U::UPPERCASE? 
wl: lambda_def(defmacro,
	      u_uppercase_c63,
	      f_u_uppercase_c63,
	      [u_x],
	      [progn, ['#BQ', [upper_case_p, ['#COMMA', u_x]]]]).


% annotating U::UPPERCASE? 
wl: arglist_info(u_uppercase_c63,
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

% annotating U::UPPERCASE? 
wl: init_args(exact_only, u_uppercase_c63).


% annotating U::UPPERCASE? 
f_u_uppercase_c63(X_Param, FnResult) :-
	[upper_case_p, X_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_uppercase_c63, classof, claz_macro),
   set_opv(u_uppercase_c63, compile_as, kw_operator),
   set_opv(u_uppercase_c63, function, f_u_uppercase_c63),
   DefMacroResult=u_uppercase_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1931 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'delq!',
			    [a, b],
			    ['#BQ', [delete, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::DELQ! 
wl: lambda_def(defmacro,
	      u_delq_c33,
	      f_u_delq_c33,
	      [u_a, u_b],
	      [progn, ['#BQ', [delete, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::DELQ! 
wl: arglist_info(u_delq_c33,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::DELQ! 
wl: init_args(exact_only, u_delq_c33).


% annotating U::DELQ! 
f_u_delq_c33(A_Param, B_Param, FnResult) :-
	[delete, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_delq_c33, classof, claz_macro),
   set_opv(u_delq_c33, compile_as, kw_operator),
   set_opv(u_delq_c33, function, f_u_delq_c33),
   DefMacroResult=u_delq_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:1971 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, listify, [a], [if, [listp, a], a, [list, a]]]).

% annotating U::LISTIFY 
wl: lambda_def(defun,
	      u_listify,
	      f_u_listify,
	      [u_a],
	      [[if, [listp, u_a], u_a, [list, u_a]]]).


% annotating U::LISTIFY 
wl: arglist_info(u_listify,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::LISTIFY 
wl: init_args(exact_only, u_listify).


% annotating U::LISTIFY 
f_u_listify(A_Param, FnResult) :-
	(   is_listp(A_Param)
	->  FnResult=A_Param
	;   FnResult=[A_Param]
	).
:- set_opv(f_u_listify, classof, claz_function),
   set_opv(u_listify, compile_as, kw_function),
   set_opv(u_listify, function, f_u_listify),
   DefunResult=u_listify.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2017 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'append!',
			    [a, b],
			    
			    [ '#BQ',
			      
			      [ nconc,
				[listify, ['#COMMA', a]],
				[listify, ['#COMMA', b]]
			      ]
			    ]
			  ]).

% annotating U::APPEND! 
wl: lambda_def(defmacro,
	      u_append_c33,
	      f_u_append_c33,
	      [u_a, u_b],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ nconc,
		    [u_listify, ['#COMMA', u_a]],
		    [u_listify, ['#COMMA', u_b]]
		  ]
		]
	      ]).


% annotating U::APPEND! 
wl: arglist_info(u_append_c33,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::APPEND! 
wl: init_args(exact_only, u_append_c33).


% annotating U::APPEND! 
f_u_append_c33(A_Param, B_Param, FnResult) :-
	[nconc, [u_listify, A_Param], [u_listify, B_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_append_c33, classof, claz_macro),
   set_opv(u_append_c33, compile_as, kw_operator),
   set_opv(u_append_c33, function, f_u_append_c33),
   DefMacroResult=u_append_c33.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2077 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'ascii->char',
			    [x],
			    ['#BQ', ['code-char', ['#COMMA', x]]]
			  ]).

% annotating U::ASCII->CHAR 
wl: lambda_def(defmacro,
	      u_ascii_c62_char,
	      f_u_ascii_c62_char,
	      [u_x],
	      [progn, ['#BQ', [code_char, ['#COMMA', u_x]]]]).


% annotating U::ASCII->CHAR 
wl: arglist_info(u_ascii_c62_char,
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

% annotating U::ASCII->CHAR 
wl: init_args(exact_only, u_ascii_c62_char).


% annotating U::ASCII->CHAR 
f_u_ascii_c62_char(X_Param, FnResult) :-
	[code_char, X_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_ascii_c62_char, classof, claz_macro),
   set_opv(u_ascii_c62_char, compile_as, kw_operator),
   set_opv(u_ascii_c62_char, function, f_u_ascii_c62_char),
   DefMacroResult=u_ascii_c62_char.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2077 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'("; #-abcl ", 1, 2122)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2133 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    assq,
			    [a, b],
			    ['#BQ', [assoc, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating EXT:ASSQ 
wl: lambda_def(defmacro,
	      ext_assq,
	      f_ext_assq,
	      [u_a, u_b],
	      [progn, ['#BQ', [assoc, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating EXT:ASSQ 
wl: arglist_info(ext_assq,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating EXT:ASSQ 
wl: init_args(exact_only, ext_assq).


% annotating EXT:ASSQ 
f_ext_assq(A_Param, B_Param, FnResult) :-
	[assoc, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_ext_assq, classof, claz_macro),
   set_opv(ext_assq, compile_as, kw_operator),
   set_opv(ext_assq, function, f_ext_assq),
   DefMacroResult=ext_assq.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2171 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'increment-me',
			    [a],
			    ['#BQ', [setq, ['#COMMA', a], [+, ['#COMMA', a], 1]]]
			  ]).

% annotating U::INCREMENT-ME 
wl: lambda_def(defmacro,
	      u_increment_me,
	      f_u_increment_me,
	      [u_a],
	      [progn, ['#BQ', [setq, ['#COMMA', u_a], [+, ['#COMMA', u_a], 1]]]]).


% annotating U::INCREMENT-ME 
wl: arglist_info(u_increment_me,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::INCREMENT-ME 
wl: init_args(exact_only, u_increment_me).


% annotating U::INCREMENT-ME 
f_u_increment_me(A_Param, FnResult) :-
	[setq, A_Param, [+, A_Param, 1]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_increment_me, classof, claz_macro),
   set_opv(u_increment_me, compile_as, kw_operator),
   set_opv(u_increment_me, function, f_u_increment_me),
   DefMacroResult=u_increment_me.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2219 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'string-posq',
			    [a, b],
			    ['#BQ', [position, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::STRING-POSQ 
wl: lambda_def(defmacro,
	      u_string_posq,
	      f_u_string_posq,
	      [u_a, u_b],
	      [progn, ['#BQ', [position, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::STRING-POSQ 
wl: arglist_info(u_string_posq,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::STRING-POSQ 
wl: init_args(exact_only, u_string_posq).


% annotating U::STRING-POSQ 
f_u_string_posq(A_Param, B_Param, FnResult) :-
	[position, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_string_posq, classof, claz_macro),
   set_opv(u_string_posq, compile_as, kw_operator),
   set_opv(u_string_posq, function, f_u_string_posq),
   DefMacroResult=u_string_posq.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2266 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'nth-elem',
			    [a, b],
			    ['#BQ', [nth, ['#COMMA', b], ['#COMMA', a]]]
			  ]).

% annotating U::NTH-ELEM 
wl: lambda_def(defmacro,
	      u_nth_elem,
	      f_u_nth_elem,
	      [u_a, u_b],
	      [progn, ['#BQ', [nth, ['#COMMA', u_b], ['#COMMA', u_a]]]]).


% annotating U::NTH-ELEM 
wl: arglist_info(u_nth_elem,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NTH-ELEM 
wl: init_args(exact_only, u_nth_elem).


% annotating U::NTH-ELEM 
f_u_nth_elem(A_Param, B_Param, FnResult) :-
	[nth, B_Param, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_nth_elem, classof, claz_macro),
   set_opv(u_nth_elem, compile_as, kw_operator),
   set_opv(u_nth_elem, function, f_u_nth_elem),
   DefMacroResult=u_nth_elem.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2305 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defmacro, newline, [a], ['#BQ', [terpri, ['#COMMA', a]]]]).

% annotating U::NEWLINE 
wl: lambda_def(defmacro,
	      u_newline,
	      f_u_newline,
	      [u_a],
	      [progn, ['#BQ', [terpri, ['#COMMA', u_a]]]]).


% annotating U::NEWLINE 
wl: arglist_info(u_newline,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::NEWLINE 
wl: init_args(exact_only, u_newline).


% annotating U::NEWLINE 
f_u_newline(A_Param, FnResult) :-
	[terpri, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_newline, classof, claz_macro),
   set_opv(u_newline, compile_as, kw_operator),
   set_opv(u_newline, function, f_u_newline),
   DefMacroResult=u_newline.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2305 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  '$COMMENT'("(defmacro -1+ (a) `(+ -1 ,a))", 1, 2342)).
:- true.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2372 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl+',
			    [a, b],
			    ['#BQ', [+, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL+ 
wl: lambda_def(defmacro,
	      u_fl_c43,
	      f_u_fl_c43,
	      [u_a, u_b],
	      [progn, ['#BQ', [+, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL+ 
wl: arglist_info(u_fl_c43,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL+ 
wl: init_args(exact_only, u_fl_c43).


% annotating U::FL+ 
f_u_fl_c43(A_Param, B_Param, FnResult) :-
	[+, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c43, classof, claz_macro),
   set_opv(u_fl_c43, compile_as, kw_operator),
   set_opv(u_fl_c43, function, f_u_fl_c43),
   DefMacroResult=u_fl_c43.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2404 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl-',
			    [a, b],
			    ['#BQ', [-, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL- 
wl: lambda_def(defmacro,
	      u_flc45,
	      f_u_flc45,
	      [u_a, u_b],
	      [progn, ['#BQ', [-, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL- 
wl: arglist_info(u_flc45,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL- 
wl: init_args(exact_only, u_flc45).


% annotating U::FL- 
f_u_flc45(A_Param, B_Param, FnResult) :-
	[-, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_flc45, classof, claz_macro),
   set_opv(u_flc45, compile_as, kw_operator),
   set_opv(u_flc45, function, f_u_flc45),
   DefMacroResult=u_flc45.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2436 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl*',
			    [a, b],
			    ['#BQ', [*, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL* 
wl: lambda_def(defmacro,
	      u_fl_xx,
	      f_u_fl_xx,
	      [u_a, u_b],
	      [progn, ['#BQ', [*, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL* 
wl: arglist_info(u_fl_xx,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL* 
wl: init_args(exact_only, u_fl_xx).


% annotating U::FL* 
f_u_fl_xx(A_Param, B_Param, FnResult) :-
	[*, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_xx, classof, claz_macro),
   set_opv(u_fl_xx, compile_as, kw_operator),
   set_opv(u_fl_xx, function, f_u_fl_xx),
   DefMacroResult=u_fl_xx.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2468 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl/',
			    [a, b],
			    ['#BQ', [/, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL/ 
wl: lambda_def(defmacro,
	      u_fl_c47,
	      f_u_fl_c47,
	      [u_a, u_b],
	      [progn, ['#BQ', [/, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL/ 
wl: arglist_info(u_fl_c47,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL/ 
wl: init_args(exact_only, u_fl_c47).


% annotating U::FL/ 
f_u_fl_c47(A_Param, B_Param, FnResult) :-
	[/, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c47, classof, claz_macro),
   set_opv(u_fl_c47, compile_as, kw_operator),
   set_opv(u_fl_c47, function, f_u_fl_c47),
   DefMacroResult=u_fl_c47.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2500 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl<',
			    [a, b],
			    ['#BQ', [<, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL< 
wl: lambda_def(defmacro,
	      u_fl_c60,
	      f_u_fl_c60,
	      [u_a, u_b],
	      [progn, ['#BQ', [<, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL< 
wl: arglist_info(u_fl_c60,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL< 
wl: init_args(exact_only, u_fl_c60).


% annotating U::FL< 
f_u_fl_c60(A_Param, B_Param, FnResult) :-
	[<, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c60, classof, claz_macro),
   set_opv(u_fl_c60, compile_as, kw_operator),
   set_opv(u_fl_c60, function, f_u_fl_c60),
   DefMacroResult=u_fl_c60.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2532 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl>',
			    [a, b],
			    ['#BQ', [>, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL> 
wl: lambda_def(defmacro,
	      u_fl_c62,
	      f_u_fl_c62,
	      [u_a, u_b],
	      [progn, ['#BQ', [>, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL> 
wl: arglist_info(u_fl_c62,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL> 
wl: init_args(exact_only, u_fl_c62).


% annotating U::FL> 
f_u_fl_c62(A_Param, B_Param, FnResult) :-
	[>, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c62, classof, claz_macro),
   set_opv(u_fl_c62, compile_as, kw_operator),
   set_opv(u_fl_c62, function, f_u_fl_c62),
   DefMacroResult=u_fl_c62.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2564 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl>=',
			    [a, b],
			    ['#BQ', [>=, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL>= 
wl: lambda_def(defmacro,
	      u_fl_c62_c61,
	      f_u_fl_c62_c61,
	      [u_a, u_b],
	      [progn, ['#BQ', [>=, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL>= 
wl: arglist_info(u_fl_c62_c61,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL>= 
wl: init_args(exact_only, u_fl_c62_c61).


% annotating U::FL>= 
f_u_fl_c62_c61(A_Param, B_Param, FnResult) :-
	[>=, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c62_c61, classof, claz_macro),
   set_opv(u_fl_c62_c61, compile_as, kw_operator),
   set_opv(u_fl_c62_c61, function, f_u_fl_c62_c61),
   DefMacroResult=u_fl_c62_c61.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2598 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl<=',
			    [a, b],
			    ['#BQ', [<=, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL<= 
wl: lambda_def(defmacro,
	      u_fl_c60_c61,
	      f_u_fl_c60_c61,
	      [u_a, u_b],
	      [progn, ['#BQ', [<=, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL<= 
wl: arglist_info(u_fl_c60_c61,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL<= 
wl: init_args(exact_only, u_fl_c60_c61).


% annotating U::FL<= 
f_u_fl_c60_c61(A_Param, B_Param, FnResult) :-
	[<=, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c60_c61, classof, claz_macro),
   set_opv(u_fl_c60_c61, compile_as, kw_operator),
   set_opv(u_fl_c60_c61, function, f_u_fl_c60_c61),
   DefMacroResult=u_fl_c60_c61.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2632 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'fl=',
			    [a, b],
			    ['#BQ', [>, ['#COMMA', a], ['#COMMA', b]]]
			  ]).

% annotating U::FL= 
wl: lambda_def(defmacro,
	      u_fl_c61,
	      f_u_fl_c61,
	      [u_a, u_b],
	      [progn, ['#BQ', [>, ['#COMMA', u_a], ['#COMMA', u_b]]]]).


% annotating U::FL= 
wl: arglist_info(u_fl_c61,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FL= 
wl: init_args(exact_only, u_fl_c61).


% annotating U::FL= 
f_u_fl_c61(A_Param, B_Param, FnResult) :-
	[>, A_Param, B_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_fl_c61, classof, claz_macro),
   set_opv(u_fl_c61, compile_as, kw_operator),
   set_opv(u_fl_c61, function, f_u_fl_c61),
   DefMacroResult=u_fl_c61.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2664 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'file-exists?',
			    [a],
			    ['#BQ', ['probe-file', ['#COMMA', a]]]
			  ]).

% annotating U::FILE-EXISTS? 
wl: lambda_def(defmacro,
	      u_file_exists_c63,
	      f_u_file_exists_c63,
	      [u_a],
	      [progn, ['#BQ', [probe_file, ['#COMMA', u_a]]]]).


% annotating U::FILE-EXISTS? 
wl: arglist_info(u_file_exists_c63,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::FILE-EXISTS? 
wl: init_args(exact_only, u_file_exists_c63).


% annotating U::FILE-EXISTS? 
f_u_file_exists_c63(A_Param, FnResult) :-
	[probe_file, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_file_exists_c63, classof, claz_macro),
   set_opv(u_file_exists_c63, compile_as, kw_operator),
   set_opv(u_file_exists_c63, function, f_u_file_exists_c63),
   DefMacroResult=u_file_exists_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2709 **********************/
:- lisp_compile_to_prolog(pkg_user, [defmacro, comment, [a], []]).

% annotating U::COMMENT 
wl: lambda_def(defmacro, u_comment, f_u_comment, [u_a], [progn, []]).


% annotating U::COMMENT 
wl: arglist_info(u_comment,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::COMMENT 
wl: init_args(exact_only, u_comment).


% annotating U::COMMENT 
f_u_comment(A_Param, FnResult) :-
	TLEnv3=[bv(u_a, A_Param)],
	[]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_comment, classof, claz_macro),
   set_opv(u_comment, compile_as, kw_operator),
   set_opv(u_comment, function, f_u_comment),
   DefMacroResult=u_comment.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2736 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'mem?',
			    [a, b, c],
			    
			    [ '#BQ',
			      
			      [ 't-or-nil',
				
				[ member,
				  ['#COMMA', b],
				  ['#COMMA', c],
				  ':test',
				  ['#COMMA', a]
				]
			      ]
			    ]
			  ]).

% annotating U::MEM? 
wl: lambda_def(defmacro,
	      u_mem_c63,
	      f_u_mem_c63,
	      [u_a, u_b, u_c],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ u_t_or_nil,
		    
		    [ member,
		      ['#COMMA', u_b],
		      ['#COMMA', u_c],
		      kw_test,
		      ['#COMMA', u_a]
		    ]
		  ]
		]
	      ]).


% annotating U::MEM? 
wl: arglist_info(u_mem_c63,
		[u_a, u_b, u_c],
		[A_Param, B_Param, C_Param],
		arginfo{ all:[u_a, u_b, u_c],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b, u_c],
			 opt:0,
			 req:[u_a, u_b, u_c],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MEM? 
wl: init_args(exact_only, u_mem_c63).


% annotating U::MEM? 
f_u_mem_c63(A_Param, B_Param, C_Param, FnResult) :-
	[u_t_or_nil, [member, B_Param, C_Param, kw_test, A_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem_c63, classof, claz_macro),
   set_opv(u_mem_c63, compile_as, kw_operator),
   set_opv(u_mem_c63, function, f_u_mem_c63),
   DefMacroResult=u_mem_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2796 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    mem,
			    [a, b, c],
			    
			    [ '#BQ',
			      
			      [ member,
				['#COMMA', b],
				['#COMMA', c],
				':test',
				['#COMMA', a]
			      ]
			    ]
			  ]).

% annotating U::MEM 
wl: lambda_def(defmacro,
	      u_mem,
	      f_u_mem,
	      [u_a, u_b, u_c],
	      
	      [ progn,
		
		[ '#BQ',
		  
		  [ member,
		    ['#COMMA', u_b],
		    ['#COMMA', u_c],
		    kw_test,
		    ['#COMMA', u_a]
		  ]
		]
	      ]).


% annotating U::MEM 
wl: arglist_info(u_mem,
		[u_a, u_b, u_c],
		[A_Param, B_Param, C_Param],
		arginfo{ all:[u_a, u_b, u_c],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b, u_c],
			 opt:0,
			 req:[u_a, u_b, u_c],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::MEM 
wl: init_args(exact_only, u_mem).


% annotating U::MEM 
f_u_mem(A_Param, B_Param, C_Param, FnResult) :-
	[member, B_Param, C_Param, kw_test, A_Param]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_mem, classof, claz_macro),
   set_opv(u_mem, compile_as, kw_operator),
   set_opv(u_mem, function, f_u_mem),
   DefMacroResult=u_mem.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2844 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    'every?',
			    [a, b],
			    
			    [ '#BQ',
			      ['t-or-nil', [every, ['#COMMA', a], ['#COMMA', b]]]
			    ]
			  ]).

% annotating U::EVERY? 
wl: lambda_def(defmacro,
	      u_every_c63,
	      f_u_every_c63,
	      [u_a, u_b],
	      
	      [ progn,
		['#BQ', [u_t_or_nil, [every, ['#COMMA', u_a], ['#COMMA', u_b]]]]
	      ]).


% annotating U::EVERY? 
wl: arglist_info(u_every_c63,
		[u_a, u_b],
		[A_Param, B_Param],
		arginfo{ all:[u_a, u_b],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a, u_b],
			 opt:0,
			 req:[u_a, u_b],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::EVERY? 
wl: init_args(exact_only, u_every_c63).


% annotating U::EVERY? 
f_u_every_c63(A_Param, B_Param, FnResult) :-
	[u_t_or_nil, [every, A_Param, B_Param]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_every_c63, classof, claz_macro),
   set_opv(u_every_c63, compile_as, kw_operator),
   set_opv(u_every_c63, function, f_u_every_c63),
   DefMacroResult=u_every_c63.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2894 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defmacro,
			    tlast,
			    [a],
			    ['#BQ', [car, [last, ['#COMMA', a], 1]]]
			  ]).

% annotating U::TLAST 
wl: lambda_def(defmacro,
	      u_tlast,
	      f_u_tlast,
	      [u_a],
	      [progn, ['#BQ', [car, [last, ['#COMMA', u_a], 1]]]]).


% annotating U::TLAST 
wl: arglist_info(u_tlast,
		[u_a],
		[A_Param],
		arginfo{ all:[u_a],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_a],
			 opt:0,
			 req:[u_a],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::TLAST 
wl: init_args(exact_only, u_tlast).


% annotating U::TLAST 
f_u_tlast(A_Param, FnResult) :-
	[car, [last, A_Param, 1]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_tlast, classof, claz_macro),
   set_opv(u_tlast, compile_as, kw_operator),
   set_opv(u_tlast, function, f_u_tlast),
   DefMacroResult=u_tlast.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2934 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'standard-input', [], '*standard-input*']).

% annotating U::STANDARD-INPUT 
wl: lambda_def(defun,
	      u_standard_input,
	      f_u_standard_input,
	      [],
	      [xx_standard_input_xx]).


% annotating U::STANDARD-INPUT 
wl: arglist_info(u_standard_input,
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

% annotating U::STANDARD-INPUT 
wl: init_args(exact_only, u_standard_input).


% annotating U::STANDARD-INPUT 
f_u_standard_input(FnResult) :-
	Env=[],
	get_var(Env, xx_standard_input_xx, Xx_standard_input_xx_Get),
	Xx_standard_input_xx_Get=FnResult.
:- set_opv(f_u_standard_input, classof, claz_function),
   set_opv(u_standard_input, compile_as, kw_function),
   set_opv(u_standard_input, function, f_u_standard_input),
   DefunResult=u_standard_input.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:2977 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  [defun, 'standard-output', [], '*standard-output*']).

% annotating U::STANDARD-OUTPUT 
wl: lambda_def(defun,
	      u_standard_output,
	      f_u_standard_output,
	      [],
	      [xx_standard_output_xx]).


% annotating U::STANDARD-OUTPUT 
wl: arglist_info(u_standard_output,
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

% annotating U::STANDARD-OUTPUT 
wl: init_args(exact_only, u_standard_output).


% annotating U::STANDARD-OUTPUT 
f_u_standard_output(FnResult) :-
	Env=[],
	get_var(Env, xx_standard_output_xx, Xx_standard_output_xx_Get),
	Xx_standard_output_xx_Get=FnResult.
:- set_opv(f_u_standard_output, classof, claz_function),
   set_opv(u_standard_output, compile_as, kw_function),
   set_opv(u_standard_output, function, f_u_standard_output),
   DefunResult=u_standard_output.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:3022 **********************/
:- lisp_compile_to_prolog(pkg_user, [defun, 'string-head', [x], [char, x, 0]]).

% annotating U::STRING-HEAD 
wl: lambda_def(defun, u_string_head, f_u_string_head, [u_x], [[char, u_x, 0]]).


% annotating U::STRING-HEAD 
wl: arglist_info(u_string_head,
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

% annotating U::STRING-HEAD 
wl: init_args(exact_only, u_string_head).


% annotating U::STRING-HEAD 
f_u_string_head(X_Param, FnResult) :-
	cl_char(X_Param, 0, Char_Ret),
	Char_Ret=FnResult.
:- set_opv(f_u_string_head, classof, claz_function),
   set_opv(u_string_head, compile_as, kw_function),
   set_opv(u_string_head, function, f_u_string_head),
   DefunResult=u_string_head.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:3057 **********************/
:- lisp_compile_to_prolog(pkg_user,
			  
			  [ defun,
			    walkcdr,
			    [fn, x],
			    
			    [ yloop,
			      [initial, [rest, x]],
			      [ywhile, rest],
			      
			      [ ydo,
				[apply, fn, [list, rest]],
				[setq, rest, [cdr, rest]]
			      ]
			    ]
			  ]).

% annotating U::WALKCDR 
wl: lambda_def(defun,
	      u_walkcdr,
	      f_u_walkcdr,
	      [u_fn, u_x],
	      
	      [ 
		[ u_yloop,
		  [u_initial, [rest, u_x]],
		  [u_ywhile, rest],
		  [u_ydo, [apply, u_fn, [list, rest]], [setq, rest, [cdr, rest]]]
		]
	      ]).


% annotating U::WALKCDR 
wl: arglist_info(u_walkcdr,
		[u_fn, u_x],
		[Fn_Param, X_Param],
		arginfo{ all:[u_fn, u_x],
			 allow_other_keys:0,
			 aux:0,
			 body:0,
			 complex:0,
			 env:0,
			 key:0,
			 names:[u_fn, u_x],
			 opt:0,
			 req:[u_fn, u_x],
			 rest:0,
			 sublists:0,
			 whole:0
		       }).

:-  !.

% annotating U::WALKCDR 
wl: init_args(exact_only, u_walkcdr).


% annotating U::WALKCDR 
f_u_walkcdr(Fn_Param, X_Param, FnResult) :-
	Env=[bv(u_fn, Fn_Param), bv(u_x, X_Param)],
	f_u_yloop(
		  [ [u_initial, [rest, u_x]],
		    [u_ywhile, rest],
		    [u_ydo, [apply, u_fn, [list, rest]], [setq, rest, [cdr, rest]]]
		  ],
		  Yloop_Ret),
	Yloop_Ret=FnResult.
:- set_opv(f_u_walkcdr, classof, claz_function),
   set_opv(u_walkcdr, compile_as, kw_function),
   set_opv(u_walkcdr, function, f_u_walkcdr),
   DefunResult=u_walkcdr.

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/compat.cl:3057 **********************/
:- lisp_compile_to_prolog(pkg_user, '$COMMENT'(" End of file.", 1, 3209)).
:- true.


% Total time: 4.001 seconds


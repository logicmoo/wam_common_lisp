#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "hello.lisp" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/hello.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Thu Dec 21 06:25:52 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

:- lisp_compile_to_prolog(pkg_sys,
			  
			  [ defun,
			    main,
			    [],
			    [print, [cons, '$STRING'("hello"), '*ARGS*']]
			  ]).

% annotating `SYS::MAIN` 
wl: lambda_def(defun,
	      sys_main,
	      f_sys_main,
	      [],
	      
	      [ 
		[ print,
		  
		  [ cons,
		    '$ARRAY'([*],
			     claz_base_character,
			     [#\(h), #\(e), #\(l), #\(l), #\(o)]),
		    ext_xx_args_xx
		  ]
		]
	      ]).


% annotating `SYS::MAIN` 
wl: arglist_info(sys_main,
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

/*
:-  !.
*/

% annotating `SYS::MAIN` 
wl: init_args(exact_only, sys_main).


% annotating `SYS::MAIN` 
f_sys_main(FnResult) :-
	Env=[],
	get_var(Env, ext_xx_args_xx, Ext_xx_args_xx_Get),
	Print_Param=['$ARRAY'([*], claz_base_character, [#\(h), #\(e), #\(l), #\(l), #\(o)])|Ext_xx_args_xx_Get],
	cl_print(Print_Param, Print_Ret),
	Print_Ret=FnResult.
/*
:- set_opv(f_sys_main, classof, claz_function),
   set_opv(sys_main, compile_as, kw_function),
   set_opv(sys_main, function, f_sys_main),
   DefunResult=sys_main.
*/
:- lisp_compile_to_prolog(pkg_sys, [main]).
:- f_sys_main(_Ignored).


%; Total compilation time: 0.098 seconds


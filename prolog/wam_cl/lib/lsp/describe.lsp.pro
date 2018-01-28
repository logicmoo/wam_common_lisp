#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "lib/lsp/describe" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sun Jan 28 04:50:23 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
*/
/*
;;;  Copyright (c) 1990, Giuseppe Attardi.
*/
/*
;;;
*/
/*
;;;    This program is free software; you can redistribute it and/or
*/
/*
;;;    modify it under the terms of the GNU Library General Public
*/
/*
;;;    License as published by the Free Software Foundation; either
*/
/*
;;;    version 2 of the License, or (at your option) any later version.
*/
/*
;;;
*/
/*
;;;    See file '../Copyright' for full details.
*/
/*
;;                           DESCRIBE and INSPECT
*/
/*
(in-package "COMMON-LISP")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:494 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','#:lisp'])
/*
% macroexpand:-[in_package,lisp2].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"LISP")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "LISP"),
				_Ignored),
	   _Ignored).
/*
#+AKCL (import 'sys::arglist 'lisp)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:517 **********************/
:-lisp_compile_to_prolog(pkg_cl,'$COMMENT'([flag_removed,[+,':AKCL'],[import,[quote,'sys::arglist'],[quote,lisp]]]))
/*
(export '(arglist describe inspect))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:554 **********************/
:-lisp_compile_to_prolog(pkg_cl,[export,[quote,[arglist,describe,inspect]]])
:- f_export([sys_arglist, describe, inspect], _Ignored).
/*
(export '(documentation variable function structure type setf))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:591 **********************/
:-lisp_compile_to_prolog(pkg_cl,[export,[quote,[documentation,variable,function,structure,type,setf]]])
:- f_export([documentation, variable, function, structure, type, setf], _Ignored).
/*
(in-package "SYSTEM")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:656 **********************/
:-lisp_compile_to_prolog(pkg_cl,['in-package','#:system'])
/*
% macroexpand:-[in_package,system4].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
(proclaim '(optimize (safety 2) (space 3)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:679 **********************/
:-lisp_compile_to_prolog(pkg_sys,[proclaim,[quote,[optimize,[safety,2],[space,3]]]])
:- sf_proclaim(Sf_proclaim_Param,
	       [quote, [optimize, [safety, 2], [space, 3]]],
	       _Ignored).
/*
(defvar *inspect-level* 0)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:724 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*inspect-level*',0])
:- set_var(AEnv, sys_xx_inspect_level_xx, 0).
/*
(defvar *inspect-history* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:751 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*inspect-history*',[]])
:- set_var(AEnv, sys_xx_inspect_history_xx, []).
/*
(defvar *inspect-mode* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:782 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*inspect-mode*',[]])
:- set_var(AEnv, sys_xx_inspect_mode_xx, []).
/*
(defvar *old-print-level* nil)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:811 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*old-print-level*',[]])
:- set_var(AEnv, sys_xx_old_print_level_xx, []).
/*
(defvar *old-print-length* nil)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:842 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*old-print-length*',[]])
:- set_var(AEnv, sys_xx_old_print_length_xx, []).
/*
(defun inspect-read-line ()
  (do ((char (read-char *query-io*) (read-char *query-io*)))
      ((or (char= char #\Newline) (char= char #\Return)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:876 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-read-line',[],[do,[[char,['read-char','*query-io*'],['read-char','*query-io*']]],[[or,['char=',char,#\(10)],['char=',char,#\(13)]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_read_line,
					       kw_function,
					       f_sys_inspect_read_line)).
*/
wl:lambda_def(defun, sys_inspect_read_line, f_sys_inspect_read_line, [], [[do, [[char, [read_char, xx_query_io_xx], [read_char, xx_query_io_xx]]], [[or, [char_c61, char|"\n"], [char_c61, char|"\r"]]]]]).
wl:arglist_info(sys_inspect_read_line, f_sys_inspect_read_line, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_read_line).

/*

### Compiled Function: `SYS::INSPECT-READ-LINE` 
*/
f_sys_inspect_read_line(FnResult) :-
	GEnv=[],
	catch(( ( get_var(GEnv, xx_query_io_xx, Xx_query_io_xx_Get),
		  f_read_char(Xx_query_io_xx_Get, Char_Init),
		  AEnv=[bv(char, Char_Init)|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_27), (get_var(AEnv, char, Char_Get27), f_char_c61(Char_Get27, #\(10), FORM1_Res29), FORM1_Res29\==[], IFTEST25=FORM1_Res29->true;get_var(AEnv, char, Char_Get28), f_char_c61(Char_Get28, #\(13), Char_c61_Ret), IFTEST25=Char_c61_Ret), (IFTEST25\==[]->throw(block_exit([], [])), _TBResult=ThrowResult31;get_var(AEnv, xx_query_io_xx, Xx_query_io_xx_Get34), f_read_char(Xx_query_io_xx_Get34, Char), set_var(AEnv, char, Char), goto(do_label_27, AEnv), _TBResult=_GORES35)),
					  
					  [ addr(addr_tagbody_27_do_label_27,
						 do_label_27,
						 '$unused',
						 AEnv,
						 ((get_var(AEnv, char, Char_c61_Param), f_char_c61(Char_c61_Param, #\(10), Char_c61_Ret45), Char_c61_Ret45\==[], IFTEST=Char_c61_Ret45->true;get_var(AEnv, char, Char_Get13), f_char_c61(Char_Get13, #\(13), Char_c61_Ret46), IFTEST=Char_c61_Ret46), (IFTEST\==[]->throw(block_exit([], [])), _4772=ThrowResult;get_var(AEnv, xx_query_io_xx, Xx_query_io_xx_Get19), f_read_char(Xx_query_io_xx_Get19, Read_char_Ret), set_var(AEnv, char, Read_char_Ret), goto(do_label_27, AEnv), _4772=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_inspect_read_line, FnResult),
	      true).
:- set_opv(sys_inspect_read_line, symbol_function, f_sys_inspect_read_line),
   DefunResult=sys_inspect_read_line.
/*
:- side_effect(assert_lsp(sys_inspect_read_line,
			  lambda_def(defun,
				     sys_inspect_read_line,
				     f_sys_inspect_read_line,
				     [],
				     
				     [ 
				       [ do,
					 
					 [ 
					   [ char,
					     [read_char, xx_query_io_xx],
					     [read_char, xx_query_io_xx]
					   ]
					 ],
					 
					 [ 
					   [ or,
					     [char_c61, char|"\n"],
					     [char_c61, char|"\r"]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_read_line,
			  arglist_info(sys_inspect_read_line,
				       f_sys_inspect_read_line,
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_read_line,
			  init_args(x, f_sys_inspect_read_line))).
*/
/*
(defun select-P (object)
  (let ((*print-pretty* t) (*print-level* nil) (*print-length* nil))
       (prin1 object)
       (terpri)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:1026 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-P',[object],[let,[['*print-pretty*',t],['*print-level*',[]],['*print-length*',[]]],[prin1,object],[terpri]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_p,
					       kw_function,
					       f_sys_select_p)).
*/
wl:lambda_def(defun, sys_select_p, f_sys_select_p, [object], [[let, [[xx_print_pretty_xx, t], [xx_print_level_xx, []], [xx_print_length_xx, []]], [prin1, object], [terpri]]]).
wl:arglist_info(sys_select_p, f_sys_select_p, [object], arginfo{all:[object], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[object], opt:0, req:[object], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_p).

/*

### Compiled Function: `SYS::SELECT-P` 
*/
f_sys_select_p(Object_In, FnResult) :-
	_3844=[bv(object, Object_In)],
	catch(( ( LEnv=_3844,
		  maplist(save_special,
			  
			  [ sv(xx_print_pretty_xx, t, symbol_value, Symbol_value),
			    sv(xx_print_level_xx,
			       [],
			       symbol_value,
			       Symbol_value12),
			    sv(xx_print_length_xx,
			       [],
			       symbol_value,
			       Symbol_value13)
			  ]),
		  get_var(LEnv, object, Object_Get),
		  f_prin1(Object_Get, Prin1_Ret),
		  f_terpri(LetResult),
		  maplist(restore_special,
			  
			  [ sv(xx_print_pretty_xx, t, symbol_value, Symbol_value),
			    sv(xx_print_level_xx,
			       [],
			       symbol_value,
			       Symbol_value12),
			    sv(xx_print_length_xx,
			       [],
			       symbol_value,
			       Symbol_value13)
			  ])
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_select_p, FnResult),
	      true).
:- set_opv(sys_select_p, symbol_function, f_sys_select_p),
   DefunResult=sys_select_p.
/*
:- side_effect(assert_lsp(sys_select_p,
			  lambda_def(defun,
				     sys_select_p,
				     f_sys_select_p,
				     [object],
				     
				     [ 
				       [ let,
					 
					 [ [xx_print_pretty_xx, t],
					   [xx_print_level_xx, []],
					   [xx_print_length_xx, []]
					 ],
					 [prin1, object],
					 [terpri]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_p,
			  arglist_info(sys_select_p,
				       f_sys_select_p,
				       [object],
				       arginfo{ all:[object],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[object],
						opt:0,
						req:[object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_p, init_args(x, f_sys_select_p))).
*/
/*
(defun select-E ()
  (dolist (x (multiple-value-list
	       (multiple-value-prog1
		 (eval (read-preserving-whitespace *query-io*))
		 (inspect-read-line))))
	  (write x
		 :level *old-print-level*
		 :length *old-print-length*)
	  (terpri)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:1161 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-E',[],[dolist,[x,['multiple-value-list',['multiple-value-prog1',[eval,['read-preserving-whitespace','*query-io*']],['inspect-read-line']]]],[write,x,':level','*old-print-level*',':length','*old-print-length*'],[terpri]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_e,
					       kw_function,
					       f_sys_select_e)).
*/
wl:lambda_def(defun, sys_select_e, f_sys_select_e, [], [[dolist, [x, [multiple_value_list, [multiple_value_prog1, [eval, [read_preserving_whitespace, xx_query_io_xx]], [sys_inspect_read_line]]]], [write, x, kw_level, sys_xx_old_print_level_xx, kw_length, sys_xx_old_print_length_xx], [terpri]]]).
wl:arglist_info(sys_select_e, f_sys_select_e, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_e).

/*

### Compiled Function: `SYS::SELECT-E` 
*/
f_sys_select_e(FnResult) :-
	Value_prog1_Param=[],
	catch(( ( sf_multiple_value_prog1(Value_prog1_Param,
					  
					  [ eval,
					    
					    [ read_preserving_whitespace,
					      xx_query_io_xx
					    ]
					  ],
					  [sys_inspect_read_line],
					  IgnoredRet),
		  nb_current('$mv_return', MV_RETURN),
		  BV=bv(x, Ele),
		  Env2=[BV|Value_prog1_Param],
		  forall(member(Ele, MV_RETURN),
			 ( nb_setarg(2, BV, Ele),
			   get_var(Env2,
				   sys_xx_old_print_level_xx,
				   Xx_old_print_level_xx_Get),
			   ( get_var(Env2,
				     sys_xx_old_print_length_xx,
				     Xx_old_print_length_xx_Get),
			     get_var(Env2, x, X_Get)
			   ),
			   f_write(X_Get,
				   kw_level,
				   Xx_old_print_level_xx_Get,
				   kw_length,
				   Xx_old_print_length_xx_Get,
				   Write_Ret),
			   f_terpri(Terpri_Ret)
			 ))
		),
		Terpri_Ret=FnResult
	      ),
	      block_exit(sys_select_e, FnResult),
	      true).
:- set_opv(sys_select_e, symbol_function, f_sys_select_e),
   DefunResult=sys_select_e.
/*
:- side_effect(assert_lsp(sys_select_e,
			  lambda_def(defun,
				     sys_select_e,
				     f_sys_select_e,
				     [],
				     
				     [ 
				       [ dolist,
					 
					 [ x,
					   
					   [ multiple_value_list,
					     
					     [ multiple_value_prog1,
					       
					       [ eval,
						 
						 [ read_preserving_whitespace,
						   xx_query_io_xx
						 ]
					       ],
					       [sys_inspect_read_line]
					     ]
					   ]
					 ],
					 
					 [ write,
					   x,
					   kw_level,
					   sys_xx_old_print_level_xx,
					   kw_length,
					   sys_xx_old_print_length_xx
					 ],
					 [terpri]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_e,
			  arglist_info(sys_select_e,
				       f_sys_select_e,
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_e, init_args(x, f_sys_select_e))).
*/
/*
(defun select-U ()
  (prog1
    (eval (read-preserving-whitespace *query-io*))
    (inspect-read-line)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:1406 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-U',[],[prog1,[eval,['read-preserving-whitespace','*query-io*']],['inspect-read-line']]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_u,
					       kw_function,
					       f_sys_select_u)).
*/
wl:lambda_def(defun, sys_select_u, f_sys_select_u, [], [[prog1, [eval, [read_preserving_whitespace, xx_query_io_xx]], [sys_inspect_read_line]]]).
wl:arglist_info(sys_select_u, f_sys_select_u, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_u).

/*

### Compiled Function: `SYS::SELECT-U` 
*/
f_sys_select_u(FnResult) :-
	GEnv=[],
	catch(( ( get_var(GEnv, xx_query_io_xx, Xx_query_io_xx_Get),
		  f_read_preserving_whitespace(Xx_query_io_xx_Get,
					       Preserving_whitespace_Ret),
		  f_sys_env_eval(GEnv, Preserving_whitespace_Ret, Env_eval_Ret),
		  f_sys_inspect_read_line(Read_line_Ret)
		),
		Env_eval_Ret=FnResult
	      ),
	      block_exit(sys_select_u, FnResult),
	      true).
:- set_opv(sys_select_u, symbol_function, f_sys_select_u),
   DefunResult=sys_select_u.
/*
:- side_effect(assert_lsp(sys_select_u,
			  lambda_def(defun,
				     sys_select_u,
				     f_sys_select_u,
				     [],
				     
				     [ 
				       [ prog1,
					 
					 [ eval,
					   
					   [ read_preserving_whitespace,
					     xx_query_io_xx
					   ]
					 ],
					 [sys_inspect_read_line]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_u,
			  arglist_info(sys_select_u,
				       f_sys_select_u,
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_u, init_args(x, f_sys_select_u))).
*/
/*
(defun select-? ()
  (terpri)
  (format t
	  "Inspect commands:"(defun select-? ()\n  (terpri)\n  (format t\n\t  \"Inspect commands:~%~\n                n (or N or Newline):    inspects the field (recursively).~%~\n                s (or S):               skips the field.~%~\n                p (or P):               pretty-prints the field.~%~\n                a (or A):               aborts the inspection ~\n                                        of the rest of the fields.~%~\n                u (or U) form:          updates the field ~\n                                        with the value of the form.~%~\n                e (or E) form:          evaluates and prints the form.~%~\n                q (or Q):               quits the inspection.~%~\n                ?:                      prints this.~%~%\"))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:1512 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-?',[],[terpri],[format,t,'$STRING'("Inspect commands:~%~\n                n (or N or Newline):    inspects the field (recursively).~%~\n                s (or S):               skips the field.~%~\n                p (or P):               pretty-prints the field.~%~\n                a (or A):               aborts the inspection ~\n                                        of the rest of the fields.~%~\n                u (or U) form:          updates the field ~\n                                        with the value of the form.~%~\n                e (or E) form:          evaluates and prints the form.~%~\n                q (or Q):               quits the inspection.~%~\n                ?:                      prints this.~%~%")]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_c63,
					       kw_function,
					       f_sys_select_c63)).
*/
wl:lambda_def(defun, sys_select_c63, f_sys_select_c63, [], [[terpri], [format, t, '$ARRAY'([*], claz_base_character, "Inspect commands:~%~\n                n (or N or Newline):    inspects the field (recursively).~%~\n                s (or S):               skips the field.~%~\n                p (or P):               pretty-prints the field.~%~\n                a (or A):               aborts the inspection ~\n                                        of the rest of the fields.~%~\n                u (or U) form:          updates the field ~\n                                        with the value of the form.~%~\n                e (or E) form:          evaluates and prints the form.~%~\n                q (or Q):               quits the inspection.~%~\n                ?:                      prints this.~%~%")]]).
wl:arglist_info(sys_select_c63, f_sys_select_c63, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_c63).

/*

### Compiled Function: `SYS::SELECT-?` 
*/
f_sys_select_c63(FnResult) :-
	_10770=[],
	catch(( ( f_terpri(Terpri_Ret),
		  f_format(
			   [ t,
			     '$ARRAY'([*],
				      claz_base_character,
				      "Inspect commands:~%~\n                n (or N or Newline):    inspects the field (recursively).~%~\n                s (or S):               skips the field.~%~\n                p (or P):               pretty-prints the field.~%~\n                a (or A):               aborts the inspection ~\n                                        of the rest of the fields.~%~\n                u (or U) form:          updates the field ~\n                                        with the value of the form.~%~\n                e (or E) form:          evaluates and prints the form.~%~\n                q (or Q):               quits the inspection.~%~\n                ?:                      prints this.~%~%")
			   ],
			   Format_Ret)
		),
		Format_Ret=FnResult
	      ),
	      block_exit(sys_select_c63, FnResult),
	      true).
:- set_opv(sys_select_c63, symbol_function, f_sys_select_c63),
   DefunResult=sys_select_c63.
/*
:- side_effect(assert_lsp(sys_select_c63,
			  lambda_def(defun,
				     sys_select_c63,
				     f_sys_select_c63,
				     [],
				     
				     [ [terpri],
				       
				       [ format,
					 t,
					 '$ARRAY'([*],
						  claz_base_character,
						  "Inspect commands:~%~\n                n (or N or Newline):    inspects the field (recursively).~%~\n                s (or S):               skips the field.~%~\n                p (or P):               pretty-prints the field.~%~\n                a (or A):               aborts the inspection ~\n                                        of the rest of the fields.~%~\n                u (or U) form:          updates the field ~\n                                        with the value of the form.~%~\n                e (or E) form:          evaluates and prints the form.~%~\n                q (or Q):               quits the inspection.~%~\n                ?:                      prints this.~%~%")
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_c63,
			  arglist_info(sys_select_c63,
				       f_sys_select_c63,
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_c63, init_args(x, f_sys_select_c63))).
*/
/*
(defun read-inspect-command (label object allow-recursive)
  (unless *inspect-mode*
    (inspect-indent-1)
    (if allow-recursive
        (progn (princ label) (inspect-object object))
        (format t label object))
    (return-from read-inspect-command nil))
  (loop
    (inspect-indent-1)
    (if allow-recursive
        (progn (princ label)
               (inspect-indent)
               (prin1 object))
        (format t label object))
    (write-char #\Space)
    (force-output)
    (case (do ((char (read-char *query-io*) (read-char *query-io*)))
              ((and (char/= char #\Space) (char/= #\Tab)) char))
      ((#\Newline #\Return)
       (when allow-recursive (inspect-object object))
       (return nil))
      ((#\n #\N)
       (inspect-read-line)
       (when allow-recursive (inspect-object object))
       (return nil))
      ((#\s #\S)
       (inspect-read-line)
       (return nil))
      ((#\p #\P)
       (inspect-read-line)
       (select-P object))
      ((#\a #\A)
       (inspect-read-line)
       (throw 'ABORT-INSPECT nil))
      ((#\u #\U)
       (return (values t (select-U))))
      ((#\e #\E)
       (select-E))
      ((#\q #\Q)
       (inspect-read-line)
       (throw 'QUIT-INSPECT nil))
      ((#\?)
	(inspect-read-line)
	(select-?))
      (t
        (inspect-read-line))
      )))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:2249 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'read-inspect-command',[label,object,'allow-recursive'],[unless,'*inspect-mode*',['inspect-indent-1'],[if,'allow-recursive',[progn,[princ,label],['inspect-object',object]],[format,t,label,object]],['return-from','read-inspect-command',[]]],[loop,['inspect-indent-1'],[if,'allow-recursive',[progn,[princ,label],['inspect-indent'],[prin1,object]],[format,t,label,object]],['write-char',#\(' ')],['force-output'],[case,[do,[[char,['read-char','*query-io*'],['read-char','*query-io*']]],[[and,['char/=',char,#\(' ')],['char/=',#\(9)]],char]],[[#\(10),#\(13)],[when,'allow-recursive',['inspect-object',object]],[return,[]]],[[#\(n),#\('N')],['inspect-read-line'],[when,'allow-recursive',['inspect-object',object]],[return,[]]],[[#\(s),#\('S')],['inspect-read-line'],[return,[]]],[[#\(p),#\('P')],['inspect-read-line'],['select-P',object]],[[#\(a),#\('A')],['inspect-read-line'],[throw,[quote,'ABORT-INSPECT'],[]]],[[#\(u),#\('U')],[return,[values,t,['select-U']]]],[[#\(e),#\('E')],['select-E']],[[#\(q),#\('Q')],['inspect-read-line'],[throw,[quote,'QUIT-INSPECT'],[]]],[[#\(?)],['inspect-read-line'],['select-?']],[t,['inspect-read-line']]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_read_inspect_command,
					       kw_function,
					       f_sys_read_inspect_command)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_indent_1,
					       kw_function,
					       f_sys_inspect_indent_1)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_object,
					       kw_function,
					       f_sys_inspect_object)).
*/
wl:lambda_def(defun, sys_read_inspect_command, f_sys_read_inspect_command, [sys_label, object, sys_allow_recursive], [[unless, sys_xx_inspect_mode_xx, [sys_inspect_indent_1], [if, sys_allow_recursive, [progn, [princ, sys_label], [sys_inspect_object, object]], [format, t, sys_label, object]], [return_from, sys_read_inspect_command, []]], [loop, [sys_inspect_indent_1], [if, sys_allow_recursive, [progn, [princ, sys_label], [sys_inspect_indent], [prin1, object]], [format, t, sys_label, object]], [write_char|" "], [force_output], [case, [do, [[char, [read_char, xx_query_io_xx], [read_char, xx_query_io_xx]]], [[and, [char_c47_c61, char|" "], [char_c47_c61|"\t"]], char]], ["\n\r", [when, sys_allow_recursive, [sys_inspect_object, object]], [return, []]], ["nN", [sys_inspect_read_line], [when, sys_allow_recursive, [sys_inspect_object, object]], [return, []]], ["sS", [sys_inspect_read_line], [return, []]], ["pP", [sys_inspect_read_line], [sys_select_p, object]], ["aA", [sys_inspect_read_line], [throw, [quote, sys_abort_inspect], []]], ["uU", [return, [values, t, [sys_select_u]]]], ["eE", [sys_select_e]], ["qQ", [sys_inspect_read_line], [throw, [quote, sys_quit_inspect], []]], ["?", [sys_inspect_read_line], [sys_select_c63]], [t, [sys_inspect_read_line]]]]]).
wl:arglist_info(sys_read_inspect_command, f_sys_read_inspect_command, [sys_label, object, sys_allow_recursive], arginfo{all:[sys_label, object, sys_allow_recursive], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_label, object, sys_allow_recursive], opt:0, req:[sys_label, object, sys_allow_recursive], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_read_inspect_command).

/*

### Compiled Function: `SYS::READ-INSPECT-COMMAND` 
*/
f_sys_read_inspect_command(Label_In, Object_In, Allow_recursive_In, FnResult) :-
	BlockExitEnv=[bv(sys_label, Label_In), bv(object, Object_In), bv(sys_allow_recursive, Allow_recursive_In)],
	catch(( ( get_var(BlockExitEnv, sys_xx_inspect_mode_xx, IFTEST),
		  (   IFTEST\==[]
		  ->  _8360=[]
		  ;   f_sys_inspect_indent_1(Indent_1_Ret),
		      get_var(BlockExitEnv, sys_allow_recursive, IFTEST10),
		      (   IFTEST10\==[]
		      ->  get_var(BlockExitEnv, sys_label, Label_Get),
			  f_princ(Label_Get, Princ_Ret),
			  get_var(BlockExitEnv, object, Object_Get),
			  f_sys_inspect_object(Object_Get, TrueResult),
			  _8420=TrueResult
		      ;   get_var(BlockExitEnv, object, Object_Get16),
			  get_var(BlockExitEnv, sys_label, Label_Get15),
			  f_format([t, Label_Get15, Object_Get16], ElseResult),
			  _8420=ElseResult
		      ),
		      throw(block_exit(sys_read_inspect_command, [])),
		      _8360=ThrowResult
		  ),
		  sf_loop(BlockExitEnv,
			  [sys_inspect_indent_1],
			  
			  [ if,
			    sys_allow_recursive,
			    
			    [ progn,
			      [princ, sys_label],
			      [sys_inspect_indent],
			      [prin1, object]
			    ],
			    [format, t, sys_label, object]
			  ],
			  [write_char|" "],
			  [force_output],
			  
			  [ case,
			    
			    [ do,
			      
			      [ 
				[ char,
				  [read_char, xx_query_io_xx],
				  [read_char, xx_query_io_xx]
				]
			      ],
			      
			      [ 
				[ and,
				  [char_c47_c61, char|" "],
				  [char_c47_c61|"\t"]
				],
				char
			      ]
			    ],
			    
			    [ "\n\r",
			      
			      [ when,
				sys_allow_recursive,
				[sys_inspect_object, object]
			      ],
			      [return, []]
			    ],
			    
			    [ "nN",
			      [sys_inspect_read_line],
			      
			      [ when,
				sys_allow_recursive,
				[sys_inspect_object, object]
			      ],
			      [return, []]
			    ],
			    ["sS", [sys_inspect_read_line], [return, []]],
			    
			    [ "pP",
			      [sys_inspect_read_line],
			      [sys_select_p, object]
			    ],
			    
			    [ "aA",
			      [sys_inspect_read_line],
			      [throw, [quote, sys_abort_inspect], []]
			    ],
			    ["uU", [return, [values, t, [sys_select_u]]]],
			    ["eE", [sys_select_e]],
			    
			    [ "qQ",
			      [sys_inspect_read_line],
			      [throw, [quote, sys_quit_inspect], []]
			    ],
			    ["?", [sys_inspect_read_line], [sys_select_c63]],
			    [t, [sys_inspect_read_line]]
			  ],
			  Sf_loop_Ret)
		),
		Sf_loop_Ret=FnResult
	      ),
	      block_exit(sys_read_inspect_command, FnResult),
	      true).
:- set_opv(sys_read_inspect_command,
	   symbol_function,
	   f_sys_read_inspect_command),
   DefunResult=sys_read_inspect_command.
/*
:- side_effect(assert_lsp(sys_read_inspect_command,
			  lambda_def(defun,
				     sys_read_inspect_command,
				     f_sys_read_inspect_command,
				     [sys_label, object, sys_allow_recursive],
				     
				     [ 
				       [ unless,
					 sys_xx_inspect_mode_xx,
					 [sys_inspect_indent_1],
					 
					 [ if,
					   sys_allow_recursive,
					   
					   [ progn,
					     [princ, sys_label],
					     [sys_inspect_object, object]
					   ],
					   [format, t, sys_label, object]
					 ],
					 
					 [ return_from,
					   sys_read_inspect_command,
					   []
					 ]
				       ],
				       
				       [ loop,
					 [sys_inspect_indent_1],
					 
					 [ if,
					   sys_allow_recursive,
					   
					   [ progn,
					     [princ, sys_label],
					     [sys_inspect_indent],
					     [prin1, object]
					   ],
					   [format, t, sys_label, object]
					 ],
					 [write_char|" "],
					 [force_output],
					 
					 [ case,
					   
					   [ do,
					     
					     [ 
					       [ char,
						 [read_char, xx_query_io_xx],
						 [read_char, xx_query_io_xx]
					       ]
					     ],
					     
					     [ 
					       [ and,
						 [char_c47_c61, char|" "],
						 [char_c47_c61|"\t"]
					       ],
					       char
					     ]
					   ],
					   
					   [ "\n\r",
					     
					     [ when,
					       sys_allow_recursive,
					       [sys_inspect_object, object]
					     ],
					     [return, []]
					   ],
					   
					   [ "nN",
					     [sys_inspect_read_line],
					     
					     [ when,
					       sys_allow_recursive,
					       [sys_inspect_object, object]
					     ],
					     [return, []]
					   ],
					   
					   [ "sS",
					     [sys_inspect_read_line],
					     [return, []]
					   ],
					   
					   [ "pP",
					     [sys_inspect_read_line],
					     [sys_select_p, object]
					   ],
					   
					   [ "aA",
					     [sys_inspect_read_line],
					     
					     [ throw,
					       [quote, sys_abort_inspect],
					       []
					     ]
					   ],
					   
					   [ "uU",
					     
					     [ return,
					       [values, t, [sys_select_u]]
					     ]
					   ],
					   ["eE", [sys_select_e]],
					   
					   [ "qQ",
					     [sys_inspect_read_line],
					     
					     [ throw,
					       [quote, sys_quit_inspect],
					       []
					     ]
					   ],
					   
					   [ "?",
					     [sys_inspect_read_line],
					     [sys_select_c63]
					   ],
					   [t, [sys_inspect_read_line]]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_read_inspect_command,
			  arglist_info(sys_read_inspect_command,
				       f_sys_read_inspect_command,
				       [sys_label, object, sys_allow_recursive],
				       arginfo{ all:
						    [ sys_label,
						      object,
						      sys_allow_recursive
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_label,
							object,
							sys_allow_recursive
						      ],
						opt:0,
						req:
						    [ sys_label,
						      object,
						      sys_allow_recursive
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_read_inspect_command,
			  init_args(x, f_sys_read_inspect_command))).
*/
/*
(defmacro inspect-recursively (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
            (read-inspect-command ,label ,object t)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object t)
             (princ "Not updated.")
             (terpri))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:3571 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'inspect-recursively',[label,object,'&optional',place],[if,place,['#BQ',['multiple-value-bind',['update-flag','new-value'],['read-inspect-command',['#COMMA',label],['#COMMA',object],t],[when,'update-flag',[setf,['#COMMA',place],'new-value']]]],['#BQ',[when,['read-inspect-command',['#COMMA',label],['#COMMA',object],t],[princ,'$STRING'("Not updated.")],[terpri]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_recursively,
					       kw_macro,
					       mf_sys_inspect_recursively)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_recursively,
					       kw_special,
					       sf_sys_inspect_recursively)).
*/
wl:lambda_def(defmacro, sys_inspect_recursively, mf_sys_inspect_recursively, [sys_label, object, c38_optional, sys_place], [[if, sys_place, ['#BQ', [multiple_value_bind, [sys_update_flag, sys_new_value], [sys_read_inspect_command, ['#COMMA', sys_label], ['#COMMA', object], t], [when, sys_update_flag, [setf, ['#COMMA', sys_place], sys_new_value]]]], ['#BQ', [when, [sys_read_inspect_command, ['#COMMA', sys_label], ['#COMMA', object], t], [princ, '$ARRAY'([*], claz_base_character, "Not updated.")], [terpri]]]]]).
wl:arglist_info(sys_inspect_recursively, mf_sys_inspect_recursively, [sys_label, object, c38_optional, sys_place], arginfo{all:[sys_label, object, sys_place], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_label, object, sys_place], opt:[sys_place], req:[sys_label, object], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_sys_inspect_recursively).

/*

### Compiled Macro Operator: `SYS::INSPECT-RECURSIVELY` 
*/
sf_sys_inspect_recursively(MacroEnv, Label_In, Object_In, RestNKeys, FResult) :-
	mf_sys_inspect_recursively(
				   [ sys_inspect_recursively,
				     Label_In,
				     Object_In
				   | RestNKeys
				   ],
				   MacroEnv,
				   MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `SYS::INSPECT-RECURSIVELY` 
*/
mf_sys_inspect_recursively([sys_inspect_recursively, Label_In, Object_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_label, Label_In), bv(object, Object_In), bv(sys_place, Place_In)],
	opt_var(MacroEnv, sys_place, Place_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_place, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, object, Object_Get),
		      get_var(GEnv, sys_label, Label_Get),
		      get_var(GEnv, sys_place, Place_Get13),
		      _6948=[multiple_value_bind, [sys_update_flag, sys_new_value], [sys_read_inspect_command, Label_Get, Object_Get, t], [when, sys_update_flag, [setf, Place_Get13, sys_new_value]]]
		  ;   get_var(GEnv, object, Object_Get15),
		      get_var(GEnv, sys_label, Label_Get14),
		      _6948=[when, [sys_read_inspect_command, Label_Get14, Object_Get15, t], [princ, '$ARRAY'([*], claz_base_character, "Not updated.")], [terpri]]
		  )
		),
		_6948=MFResult
	      ),
	      block_exit(sys_inspect_recursively, MFResult),
	      true).
:- set_opv(mf_sys_inspect_recursively, type_of, sys_macro),
   set_opv(sys_inspect_recursively, symbol_function, mf_sys_inspect_recursively),
   DefMacroResult=sys_inspect_recursively.
/*
:- side_effect(assert_lsp(sys_inspect_recursively,
			  lambda_def(defmacro,
				     sys_inspect_recursively,
				     mf_sys_inspect_recursively,
				     [sys_label, object, c38_optional, sys_place],
				     
				     [ 
				       [ if,
					 sys_place,
					 
					 [ '#BQ',
					   
					   [ multiple_value_bind,
					     [sys_update_flag, sys_new_value],
					     
					     [ sys_read_inspect_command,
					       ['#COMMA', sys_label],
					       ['#COMMA', object],
					       t
					     ],
					     
					     [ when,
					       sys_update_flag,
					       
					       [ setf,
						 ['#COMMA', sys_place],
						 sys_new_value
					       ]
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ when,
					     
					     [ sys_read_inspect_command,
					       ['#COMMA', sys_label],
					       ['#COMMA', object],
					       t
					     ],
					     
					     [ princ,
					       '$ARRAY'([*],
							claz_base_character,
							"Not updated.")
					     ],
					     [terpri]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_recursively,
			  arglist_info(sys_inspect_recursively,
				       mf_sys_inspect_recursively,
				       
				       [ sys_label,
					 object,
					 c38_optional,
					 sys_place
				       ],
				       arginfo{ all:
						    [ sys_label,
						      object,
						      sys_place
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_label,
							object,
							sys_place
						      ],
						opt:[sys_place],
						req:[sys_label, object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_recursively,
			  init_args(2, mf_sys_inspect_recursively))).
*/
/*
(defmacro inspect-print (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
           (read-inspect-command ,label ,object nil)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object nil)
             (princ "Not updated.")
             (terpri))))
          
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:3916 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'inspect-print',[label,object,'&optional',place],[if,place,['#BQ',['multiple-value-bind',['update-flag','new-value'],['read-inspect-command',['#COMMA',label],['#COMMA',object],[]],[when,'update-flag',[setf,['#COMMA',place],'new-value']]]],['#BQ',[when,['read-inspect-command',['#COMMA',label],['#COMMA',object],[]],[princ,'$STRING'("Not updated.")],[terpri]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_print,
					       kw_macro,
					       mf_sys_inspect_print)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_print,
					       kw_special,
					       sf_sys_inspect_print)).
*/
wl:lambda_def(defmacro, sys_inspect_print, mf_sys_inspect_print, [sys_label, object, c38_optional, sys_place], [[if, sys_place, ['#BQ', [multiple_value_bind, [sys_update_flag, sys_new_value], [sys_read_inspect_command, ['#COMMA', sys_label], ['#COMMA', object], []], [when, sys_update_flag, [setf, ['#COMMA', sys_place], sys_new_value]]]], ['#BQ', [when, [sys_read_inspect_command, ['#COMMA', sys_label], ['#COMMA', object], []], [princ, '$ARRAY'([*], claz_base_character, "Not updated.")], [terpri]]]]]).
wl:arglist_info(sys_inspect_print, mf_sys_inspect_print, [sys_label, object, c38_optional, sys_place], arginfo{all:[sys_label, object, sys_place], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_label, object, sys_place], opt:[sys_place], req:[sys_label, object], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_sys_inspect_print).

/*

### Compiled Macro Operator: `SYS::INSPECT-PRINT` 
*/
sf_sys_inspect_print(MacroEnv, Label_In, Object_In, RestNKeys, FResult) :-
	mf_sys_inspect_print([sys_inspect_print, Label_In, Object_In|RestNKeys],
			     MacroEnv,
			     MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `SYS::INSPECT-PRINT` 
*/
mf_sys_inspect_print([sys_inspect_print, Label_In, Object_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_label, Label_In), bv(object, Object_In), bv(sys_place, Place_In)],
	opt_var(MacroEnv, sys_place, Place_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_place, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, object, Object_Get),
		      get_var(GEnv, sys_label, Label_Get),
		      get_var(GEnv, sys_place, Place_Get13),
		      _6950=[multiple_value_bind, [sys_update_flag, sys_new_value], [sys_read_inspect_command, Label_Get, Object_Get, []], [when, sys_update_flag, [setf, Place_Get13, sys_new_value]]]
		  ;   get_var(GEnv, object, Object_Get15),
		      get_var(GEnv, sys_label, Label_Get14),
		      _6950=[when, [sys_read_inspect_command, Label_Get14, Object_Get15, []], [princ, '$ARRAY'([*], claz_base_character, "Not updated.")], [terpri]]
		  )
		),
		_6950=MFResult
	      ),
	      block_exit(sys_inspect_print, MFResult),
	      true).
:- set_opv(mf_sys_inspect_print, type_of, sys_macro),
   set_opv(sys_inspect_print, symbol_function, mf_sys_inspect_print),
   DefMacroResult=sys_inspect_print.
/*
:- side_effect(assert_lsp(sys_inspect_print,
			  lambda_def(defmacro,
				     sys_inspect_print,
				     mf_sys_inspect_print,
				     [sys_label, object, c38_optional, sys_place],
				     
				     [ 
				       [ if,
					 sys_place,
					 
					 [ '#BQ',
					   
					   [ multiple_value_bind,
					     [sys_update_flag, sys_new_value],
					     
					     [ sys_read_inspect_command,
					       ['#COMMA', sys_label],
					       ['#COMMA', object],
					       []
					     ],
					     
					     [ when,
					       sys_update_flag,
					       
					       [ setf,
						 ['#COMMA', sys_place],
						 sys_new_value
					       ]
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ when,
					     
					     [ sys_read_inspect_command,
					       ['#COMMA', sys_label],
					       ['#COMMA', object],
					       []
					     ],
					     
					     [ princ,
					       '$ARRAY'([*],
							claz_base_character,
							"Not updated.")
					     ],
					     [terpri]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_print,
			  arglist_info(sys_inspect_print,
				       mf_sys_inspect_print,
				       
				       [ sys_label,
					 object,
					 c38_optional,
					 sys_place
				       ],
				       arginfo{ all:
						    [ sys_label,
						      object,
						      sys_place
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_label,
							object,
							sys_place
						      ],
						opt:[sys_place],
						req:[sys_label, object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_print, init_args(2, mf_sys_inspect_print))).
*/
/*
(defun inspect-indent ()
  (fresh-line)
  (format t ""(defun inspect-indent ()\n  (fresh-line)\n  (format t \"~V@T\"\n          (* 4 (if (< *inspect-level* 8) *inspect-level* 8))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:4268 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-indent',[],['fresh-line'],[format,t,'$STRING'("~V@T"),[*,4,[if,[<,'*inspect-level*',8],'*inspect-level*',8]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_indent,
					       kw_function,
					       f_sys_inspect_indent)).
*/
wl:lambda_def(defun, sys_inspect_indent, f_sys_inspect_indent, [], [[fresh_line], [format, t, '$ARRAY'([*], claz_base_character, "~V@T"), [*, 4, [if, [<, sys_xx_inspect_level_xx, 8], sys_xx_inspect_level_xx, 8]]]]).
wl:arglist_info(sys_inspect_indent, f_sys_inspect_indent, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_indent).

/*

### Compiled Function: `SYS::INSPECT-INDENT` 
*/
f_sys_inspect_indent(FnResult) :-
	GEnv=[],
	catch(( ( f_fresh_line(Fresh_line_Ret),
		  get_var(GEnv,
			  sys_xx_inspect_level_xx,
			  Xx_inspect_level_xx_Get),
		  (   Xx_inspect_level_xx_Get<8
		  ->  get_var(GEnv,
			      sys_xx_inspect_level_xx,
			      Xx_inspect_level_xx_Get8),
		      _3938=Xx_inspect_level_xx_Get8
		  ;   _3938=8
		  ),
		  'f_*'(4, _3938, CAR),
		  f_format([t, '$ARRAY'([*], claz_base_character, "~V@T"), CAR],
			   Format_Ret)
		),
		Format_Ret=FnResult
	      ),
	      block_exit(sys_inspect_indent, FnResult),
	      true).
:- set_opv(sys_inspect_indent, symbol_function, f_sys_inspect_indent),
   DefunResult=sys_inspect_indent.
/*
:- side_effect(assert_lsp(sys_inspect_indent,
			  lambda_def(defun,
				     sys_inspect_indent,
				     f_sys_inspect_indent,
				     [],
				     
				     [ [fresh_line],
				       
				       [ format,
					 t,
					 '$ARRAY'([*],
						  claz_base_character,
						  "~V@T"),
					 
					 [ (*),
					   4,
					   
					   [ if,
					     [<, sys_xx_inspect_level_xx, 8],
					     sys_xx_inspect_level_xx,
					     8
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_indent,
			  arglist_info(sys_inspect_indent,
				       f_sys_inspect_indent,
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_indent,
			  init_args(x, f_sys_inspect_indent))).
*/
/*
(defun inspect-indent-1 ()
  (fresh-line)
  (format t ""(defun inspect-indent-1 ()\n  (fresh-line)\n  (format t \"~V@T\"\n          (- (* 4 (if (< *inspect-level* 8) *inspect-level* 8)) 3)))\n\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:4391 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-indent-1',[],['fresh-line'],[format,t,'$STRING'("~V@T"),[-,[*,4,[if,[<,'*inspect-level*',8],'*inspect-level*',8]],3]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_indent_1,
					       kw_function,
					       f_sys_inspect_indent_1)).
*/
wl:lambda_def(defun, sys_inspect_indent_1, f_sys_inspect_indent_1, [], [[fresh_line], [format, t, '$ARRAY'([*], claz_base_character, "~V@T"), [-, [*, 4, [if, [<, sys_xx_inspect_level_xx, 8], sys_xx_inspect_level_xx, 8]], 3]]]).
wl:arglist_info(sys_inspect_indent_1, f_sys_inspect_indent_1, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_indent_1).

/*

### Compiled Function: `SYS::INSPECT-INDENT-1` 
*/
f_sys_inspect_indent_1(FnResult) :-
	GEnv=[],
	catch(( ( f_fresh_line(Fresh_line_Ret),
		  get_var(GEnv,
			  sys_xx_inspect_level_xx,
			  Xx_inspect_level_xx_Get),
		  (   Xx_inspect_level_xx_Get<8
		  ->  get_var(GEnv,
			      sys_xx_inspect_level_xx,
			      Xx_inspect_level_xx_Get8),
		      _4014=Xx_inspect_level_xx_Get8
		  ;   _4014=8
		  ),
		  'f_*'(4, _4014, _4012),
		  'f_-'(_4012, 3, CAR),
		  f_format([t, '$ARRAY'([*], claz_base_character, "~V@T"), CAR],
			   Format_Ret)
		),
		Format_Ret=FnResult
	      ),
	      block_exit(sys_inspect_indent_1, FnResult),
	      true).
:- set_opv(sys_inspect_indent_1, symbol_function, f_sys_inspect_indent_1),
   DefunResult=sys_inspect_indent_1.
/*
:- side_effect(assert_lsp(sys_inspect_indent_1,
			  lambda_def(defun,
				     sys_inspect_indent_1,
				     f_sys_inspect_indent_1,
				     [],
				     
				     [ [fresh_line],
				       
				       [ format,
					 t,
					 '$ARRAY'([*],
						  claz_base_character,
						  "~V@T"),
					 
					 [ (-),
					   
					   [ (*),
					     4,
					     
					     [ if,
					       [<, sys_xx_inspect_level_xx, 8],
					       sys_xx_inspect_level_xx,
					       8
					     ]
					   ],
					   3
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_indent_1,
			  arglist_info(sys_inspect_indent_1,
				       f_sys_inspect_indent_1,
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_indent_1,
			  init_args(x, f_sys_inspect_indent_1))).
*/
/*
(defun inspect-symbol (symbol)
  (let ((p (symbol-package symbol)))
    (cond ((null p)
           (format t ""(defun inspect-symbol (symbol)\n  (let ((p (symbol-package symbol)))\n    (cond ((null p)\n           (format t \"~:@(~S~) - uninterned symbol\" symbol))\n          ((eq p (find-package \"KEYWORD\"))\n           (format t \"~:@(~S~) - keyword\" symbol))\n          (t\n           (format t \"~:@(~S~) - ~:[internal~;external~] symbol in ~A package\"\n                   symbol\n                   (multiple-value-bind (b f)\n                                        (find-symbol (symbol-name symbol) p)\n                     (declare (ignore b))\n                     (eq f :external))\n                   (package-name p)))))\n\n  (when (boundp symbol)\n        (if *inspect-mode*\n            (inspect-recursively \"value:\"\n                                 (symbol-value symbol)\n                                 (symbol-value symbol))\n            (inspect-print \"value:~%   ~S\"\n                           (symbol-value symbol)\n                           (symbol-value symbol))))\n\n  (do ((pl (symbol-plist symbol) (cddr pl)))\n      ((endp pl))\n    (unless (and (symbolp (car pl))\n                 (or (eq (symbol-package (car pl)) (find-package 'SYSTEM))\n                     (eq (symbol-package (car pl)) (find-package 'COMPILER))))\n      (if *inspect-mode*\n          (inspect-recursively (format nil \"property ~S:\" (car pl))\n                               (cadr pl)\n                               (get symbol (car pl)))\n          (inspect-print (format nil \"property ~:@(~S~):~%   ~~S\" (car pl))\n                         (cadr pl)\n                         (get symbol (car pl))))))\n  \n  (when (print-doc symbol t)\n        (format t \"~&-----------------------------------------------------------------------------~%\"))\n  )\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:4523 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-symbol',[symbol],[let,[[p,['symbol-package',symbol]]],[cond,[[null,p],[format,t,'$STRING'("~:@(~S~) - uninterned symbol"),symbol]],[[eq,p,['find-package','$STRING'("KEYWORD")]],[format,t,'$STRING'("~:@(~S~) - keyword"),symbol]],[t,[format,t,'$STRING'("~:@(~S~) - ~:[internal~;external~] symbol in ~A package"),symbol,['multiple-value-bind',[b,f],['find-symbol',['symbol-name',symbol],p],[declare,[ignore,b]],[eq,f,':external']],['package-name',p]]]]],[when,[boundp,symbol],[if,'*inspect-mode*',['inspect-recursively','$STRING'("value:"),['symbol-value',symbol],['symbol-value',symbol]],['inspect-print','$STRING'("value:~%   ~S"),['symbol-value',symbol],['symbol-value',symbol]]]],[do,[[pl,['symbol-plist',symbol],[cddr,pl]]],[[endp,pl]],[unless,[and,[symbolp,[car,pl]],[or,[eq,['symbol-package',[car,pl]],['find-package',[quote,'SYSTEM']]],[eq,['symbol-package',[car,pl]],['find-package',[quote,'COMPILER']]]]],[if,'*inspect-mode*',['inspect-recursively',[format,[],'$STRING'("property ~S:"),[car,pl]],[cadr,pl],[get,symbol,[car,pl]]],['inspect-print',[format,[],'$STRING'("property ~:@(~S~):~%   ~~S"),[car,pl]],[cadr,pl],[get,symbol,[car,pl]]]]]],[when,['print-doc',symbol,t],[format,t,'$STRING'("~&-----------------------------------------------------------------------------~%")]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_symbol,
					       kw_function,
					       f_sys_inspect_symbol)).
*/
/*
% macroexpand:-[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"value:"),[symbol_value,symbol],[symbol_value,symbol]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"value:"),[symbol_value,symbol],t],[when,sys_update_flag,[setf,[symbol_value,symbol],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv41, [], CDR, Compile_each_Ret), append([symbol|CDR], [CAR50, CAR], Append_Ret), setf_inverse_op(symbol_value, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv41, [], CDR, Compile_each_Ret), append([symbol|CDR], [CAR50, CAR], Append_Ret), setf_inverse_op(symbol_value, Inverse_op_Ret)))).
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"value:~%   ~S"),[symbol_value,symbol],[symbol_value,symbol]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"value:~%   ~S"),[symbol_value,symbol],[]],[when,sys_update_flag,[setf,[symbol_value,symbol],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv53, [], CDR, Compile_each_Ret), append([symbol|CDR], [CAR62, CAR], Append_Ret), setf_inverse_op(symbol_value, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv53, [], CDR, Compile_each_Ret), append([symbol|CDR], [CAR62, CAR], Append_Ret), setf_inverse_op(symbol_value, Inverse_op_Ret)))).
*/
/*
% macroexpand:-[sys_inspect_recursively,[format,[],'$ARRAY'([*],claz_base_character,"property ~S:"),[car,sys_pl]],[cadr,sys_pl],[get,symbol,[car,sys_pl]]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"property ~S:"),[car,sys_pl]],[cadr,sys_pl],t],[when,sys_update_flag,[setf,[get,symbol,[car,sys_pl]],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv94, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR104, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv94, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR104, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
% macroexpand:-[sys_inspect_print,[format,[],'$ARRAY'([*],claz_base_character,"property ~:@(~S~):~%   ~~S"),[car,sys_pl]],[cadr,sys_pl],[get,symbol,[car,sys_pl]]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"property ~:@(~S~):~%   ~~S"),[car,sys_pl]],[cadr,sys_pl],[]],[when,sys_update_flag,[setf,[get,symbol,[car,sys_pl]],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv108, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR118, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv108, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR118, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
% macroexpand:-[sys_inspect_recursively,[format,[],'$ARRAY'([*],claz_base_character,"property ~S:"),[car,sys_pl]],[cadr,sys_pl],[get,symbol,[car,sys_pl]]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"property ~S:"),[car,sys_pl]],[cadr,sys_pl],t],[when,sys_update_flag,[setf,[get,symbol,[car,sys_pl]],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv152, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR162, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv152, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR162, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
% macroexpand:-[sys_inspect_print,[format,[],'$ARRAY'([*],claz_base_character,"property ~:@(~S~):~%   ~~S"),[car,sys_pl]],[cadr,sys_pl],[get,symbol,[car,sys_pl]]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"property ~:@(~S~):~%   ~~S"),[car,sys_pl]],[cadr,sys_pl],[]],[when,sys_update_flag,[setf,[get,symbol,[car,sys_pl]],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv166, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR176, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv166, [[car, sys_pl], []], CDR, Compile_each_Ret), append([symbol|CDR], [CAR176, CAR], Append_Ret), setf_inverse_op(get, Inverse_op_Ret)))).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_print_doc,
					       kw_function,
					       f_sys_print_doc)).
*/
wl:lambda_def(defun, sys_inspect_symbol, f_sys_inspect_symbol, [symbol], [[let, [[sys_p, [symbol_package, symbol]]], [cond, [[null, sys_p], [format, t, '$ARRAY'([*], claz_base_character, "~:@(~S~) - uninterned symbol"), symbol]], [[eq, sys_p, [find_package, '$ARRAY'([*], claz_base_character, "KEYWORD")]], [format, t, '$ARRAY'([*], claz_base_character, "~:@(~S~) - keyword"), symbol]], [t, [format, t, '$ARRAY'([*], claz_base_character, "~:@(~S~) - ~:[internal~;external~] symbol in ~A package"), symbol, [multiple_value_bind, [b, sys_f], [find_symbol, [symbol_name, symbol], sys_p], [declare, [ignore, b]], [eq, sys_f, kw_external]], [package_name, sys_p]]]]], [when, [boundp, symbol], [if, sys_xx_inspect_mode_xx, [sys_inspect_recursively, '$ARRAY'([*], claz_base_character, "value:"), [symbol_value, symbol], [symbol_value, symbol]], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "value:~%   ~S"), [symbol_value, symbol], [symbol_value, symbol]]]], [do, [[sys_pl, [symbol_plist, symbol], [cddr, sys_pl]]], [[endp, sys_pl]], [unless, [and, [symbolp, [car, sys_pl]], [or, [eq, [symbol_package, [car, sys_pl]], [find_package, [quote, sys_system]]], [eq, [symbol_package, [car, sys_pl]], [find_package, [quote, sys_compiler]]]]], [if, sys_xx_inspect_mode_xx, [sys_inspect_recursively, [format, [], '$ARRAY'([*], claz_base_character, "property ~S:"), [car, sys_pl]], [cadr, sys_pl], [get, symbol, [car, sys_pl]]], [sys_inspect_print, [format, [], '$ARRAY'([*], claz_base_character, "property ~:@(~S~):~%   ~~S"), [car, sys_pl]], [cadr, sys_pl], [get, symbol, [car, sys_pl]]]]]], [when, [sys_print_doc, symbol, t], [format, t, '$ARRAY'([*], claz_base_character, "~&-----------------------------------------------------------------------------~%")]]]).
wl:arglist_info(sys_inspect_symbol, f_sys_inspect_symbol, [symbol], arginfo{all:[symbol], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[symbol], opt:0, req:[symbol], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_symbol).

/*

### Compiled Function: `SYS::INSPECT-SYMBOL` 
*/
f_sys_inspect_symbol(Symbol_In, FnResult) :-
	GEnv=[bv(symbol, Symbol_In)],
	catch(( ( get_var(GEnv, symbol, Symbol_Get),
		  f_symbol_package(Symbol_Get, P_Init),
		  LEnv=[bv(sys_p, P_Init)|GEnv],
		  get_var(LEnv, sys_p, IFTEST),
		  (   IFTEST==[]
		  ->  get_var(LEnv, symbol, Symbol_Get13),
		      f_format(
			       [ t,
				 '$ARRAY'([*],
					  claz_base_character,
					  "~:@(~S~) - uninterned symbol"),
				 Symbol_Get13
			       ],
			       TrueResult30),
		      LetResult=TrueResult30
		  ;   get_var(LEnv, sys_p, P_Get15),
		      f_find_package('$ARRAY'([*],
					      claz_base_character,
					      "KEYWORD"),
				     PredArg2Result),
		      (   is_eq(P_Get15, PredArg2Result)
		      ->  get_var(LEnv, symbol, Symbol_Get19),
			  f_format(
				   [ t,
				     '$ARRAY'([*],
					      claz_base_character,
					      "~:@(~S~) - keyword"),
				     Symbol_Get19
				   ],
				   TrueResult),
			  ElseResult31=TrueResult
		      ;   get_var(LEnv, symbol, Symbol_Get20),
			  LEnv23=[bv(b, []), bv(sys_f, [])|LEnv],
			  get_var(LEnv23, symbol, Symbol_Get24),
			  f_symbol_name(Symbol_Get24, Find_symbol_Param),
			  get_var(LEnv23, sys_p, P_Get25),
			  f_find_symbol(Find_symbol_Param,
					P_Get25,
					Find_symbol_Ret),
			  setq_from_values(LEnv23, [b, sys_f]),
			  sf_declare(LEnv23, [ignore, b], Sf_declare_Ret),
			  get_var(LEnv23, sys_f, Get),
			  f_eq(Get, kw_external, LetResult22),
			  get_var(LEnv, sys_p, P_Get27),
			  f_package_name(P_Get27, Package_name_Ret),
			  f_format(
				   [ t,
				     '$ARRAY'([*],
					      claz_base_character,
					      "~:@(~S~) - ~:[internal~;external~] symbol in ~A package"),
				     Symbol_Get20,
				     LetResult22,
				     Package_name_Ret
				   ],
				   ElseResult),
			  ElseResult31=ElseResult
		      ),
		      LetResult=ElseResult31
		  ),
		  get_var(GEnv, symbol, Symbol_Get33),
		  (   symbol:is_boundp(Symbol_Get33)
		  ->  get_var(GEnv, sys_xx_inspect_mode_xx, IFTEST36),
		      (   IFTEST36\==[]
		      ->  LEnv41=[bv(sys_update_flag, []), bv(sys_new_value, [])|GEnv],
			  get_var(LEnv41, symbol, Symbol_Get42),
			  f_symbol_value(Symbol_Get42, Symbol_value_Ret),
			  f_sys_read_inspect_command('$ARRAY'([*],
							      claz_base_character,
							      "value:"),
						     Symbol_value_Ret,
						     t,
						     T),
			  setq_from_values(LEnv41,
					   [sys_update_flag, sys_new_value]),
			  get_var(LEnv41, sys_update_flag, IFTEST43),
			  (   IFTEST43\==[]
			  ->  get_var(LEnv41, symbol, Symbol_Get49),
			      get_var(LEnv41, sys_new_value, New_value_Get),
			      set_place(LEnv41,
					setf,
					[symbol_value, Symbol_Get49],
					[New_value_Get],
					Setf_R),
			      LetResult40=Setf_R
			  ;   LetResult40=[]
			  ),
			  TrueResult65=LetResult40
		      ;   LEnv53=[bv(sys_update_flag, []), bv(sys_new_value, [])|GEnv],
			  get_var(LEnv53, symbol, Symbol_Get54),
			  f_symbol_value(Symbol_Get54, Symbol_value_Ret214),
			  f_sys_read_inspect_command('$ARRAY'([*],
							      claz_base_character,
							      "value:~%   ~S"),
						     Symbol_value_Ret214,
						     [],
						     Inspect_command_Ret),
			  setq_from_values(LEnv53,
					   [sys_update_flag, sys_new_value]),
			  get_var(LEnv53, sys_update_flag, IFTEST55),
			  (   IFTEST55\==[]
			  ->  get_var(LEnv53, symbol, Symbol_Get61),
			      get_var(LEnv53, sys_new_value, New_value_Get58),
			      set_place(LEnv53,
					setf,
					[symbol_value, Symbol_Get61],
					[New_value_Get58],
					Setf_R59),
			      LetResult52=Setf_R59
			  ;   LetResult52=[]
			  ),
			  TrueResult65=LetResult52
		      ),
		      _11738=TrueResult65
		  ;   _11738=[]
		  ),
		  get_var(GEnv, symbol, Symbol_Get69),
		  f_symbol_plist(Symbol_Get69, Pl_Init),
		  AEnv=[bv(sys_pl, Pl_Init)|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_28), get_var(AEnv, sys_pl, Pl_Get131), (s3q:is_endp(Pl_Get131)->throw(block_exit([], [])), _TBResult=ThrowResult135;get_var(AEnv, sys_pl, Pl_Get140), f_car(Pl_Get140, PredArgResult142), (is_symbolp(PredArgResult142)->(get_var(AEnv, sys_pl, Pl_Get143), f_car(Pl_Get143, Symbol_package_Param), f_symbol_package(Symbol_package_Param, Eq_Param), f_find_package(sys_system, Find_package_Ret), f_eq(Eq_Param, Find_package_Ret, FORM1_Res145), FORM1_Res145\==[], TrueResult146=FORM1_Res145->true;get_var(AEnv, sys_pl, Pl_Get144), f_car(Pl_Get144, Symbol_package_Param200), f_symbol_package(Symbol_package_Param200, Eq_Param201), f_find_package(sys_compiler, Find_package_Ret217), f_eq(Eq_Param201, Find_package_Ret217, Eq_Ret), TrueResult146=Eq_Ret), IFTEST137=TrueResult146;IFTEST137=[]), (IFTEST137\==[]->_14150=[];get_var(AEnv, sys_xx_inspect_mode_xx, IFTEST147), (IFTEST147\==[]->LEnv152=[bv(sys_update_flag, []), bv(sys_new_value, [])|AEnv], get_var(LEnv152, sys_pl, Pl_Get153), f_car(Pl_Get153, Car_Ret), f_format([[], '$ARRAY'([*], claz_base_character, "property ~S:"), Car_Ret], Inspect_command_Param), get_var(LEnv152, sys_pl, Pl_Get154), f_cadr(Pl_Get154, Cadr_Ret), f_sys_read_inspect_command(Inspect_command_Param, Cadr_Ret, t, T195), setq_from_values(LEnv152, [sys_update_flag, sys_new_value]), get_var(LEnv152, sys_update_flag, IFTEST155), (IFTEST155\==[]->get_var(LEnv152, symbol, Symbol_Get161), get_var(LEnv152, sys_new_value, New_value_Get158), get_var(LEnv152, sys_pl, Pl_Get162), f_car(Pl_Get162, Car_Ret221), set_place(LEnv152, setf, [get, Symbol_Get161, Car_Ret221], [New_value_Get158], Setf_R159), LetResult151=Setf_R159;LetResult151=[]), ElseResult180=LetResult151;LEnv166=[bv(sys_update_flag, []), bv(sys_new_value, [])|AEnv], get_var(LEnv166, sys_pl, Pl_Get167), f_car(Pl_Get167, Car_Ret222), f_format([[], '$ARRAY'([*], claz_base_character, "property ~:@(~S~):~%   ~~S"), Car_Ret222], Inspect_command_Param203), get_var(LEnv166, sys_pl, Pl_Get168), f_cadr(Pl_Get168, Cadr_Ret223), f_sys_read_inspect_command(Inspect_command_Param203, Cadr_Ret223, [], Inspect_command_Ret224), setq_from_values(LEnv166, [sys_update_flag, sys_new_value]), get_var(LEnv166, sys_update_flag, IFTEST169), (IFTEST169\==[]->get_var(LEnv166, symbol, Symbol_Get175), get_var(LEnv166, sys_new_value, New_value_Get172), get_var(LEnv166, sys_pl, Pl_Get176), f_car(Pl_Get176, Car_Ret225), set_place(LEnv166, setf, [get, Symbol_Get175, Car_Ret225], [New_value_Get172], Setf_R173), LetResult165=Setf_R173;LetResult165=[]), ElseResult180=LetResult165), _14150=ElseResult180), get_var(AEnv, sys_pl, Pl_Get182), f_cddr(Pl_Get182, Pl), set_var(AEnv, sys_pl, Pl), goto(do_label_28, AEnv), _TBResult=_GORES183)),
					  
					  [ addr(addr_tagbody_28_do_label_28,
						 do_label_28,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_pl, Pl_Get), (s3q:is_endp(Pl_Get)->throw(block_exit([], [])), _15408=ThrowResult;get_var(AEnv, sys_pl, Pl_Get82), f_car(Pl_Get82, PredArgResult84), (is_symbolp(PredArgResult84)->(get_var(AEnv, sys_pl, Pl_Get85), f_car(Pl_Get85, Symbol_package_Param204), f_symbol_package(Symbol_package_Param204, Eq_Param205), f_find_package(sys_system, Find_package_Ret226), f_eq(Eq_Param205, Find_package_Ret226, Eq_Ret227), Eq_Ret227\==[], TrueResult88=Eq_Ret227->true;get_var(AEnv, sys_pl, Pl_Get86), f_car(Pl_Get86, Symbol_package_Param206), f_symbol_package(Symbol_package_Param206, Eq_Param207), f_find_package(sys_compiler, Find_package_Ret228), f_eq(Eq_Param207, Find_package_Ret228, Eq_Ret229), TrueResult88=Eq_Ret229), IFTEST79=TrueResult88;IFTEST79=[]), (IFTEST79\==[]->_15524=[];get_var(AEnv, sys_xx_inspect_mode_xx, IFTEST89), (IFTEST89\==[]->LEnv94=[bv(sys_update_flag, []), bv(sys_new_value, [])|AEnv], get_var(LEnv94, sys_pl, Pl_Get95), f_car(Pl_Get95, Car_Ret230), f_format([[], '$ARRAY'([*], claz_base_character, "property ~S:"), Car_Ret230], Inspect_command_Param208), get_var(LEnv94, sys_pl, Pl_Get96), f_cadr(Pl_Get96, Cadr_Ret231), f_sys_read_inspect_command(Inspect_command_Param208, Cadr_Ret231, t, Inspect_command_Ret232), setq_from_values(LEnv94, [sys_update_flag, sys_new_value]), get_var(LEnv94, sys_update_flag, IFTEST97), (IFTEST97\==[]->get_var(LEnv94, symbol, Symbol_Get103), get_var(LEnv94, sys_new_value, New_value_Get100), get_var(LEnv94, sys_pl, Pl_Get104), f_car(Pl_Get104, Car_Ret233), set_place(LEnv94, setf, [get, Symbol_Get103, Car_Ret233], [New_value_Get100], Setf_R101), LetResult93=Setf_R101;LetResult93=[]), ElseResult122=LetResult93;LEnv108=[bv(sys_update_flag, []), bv(sys_new_value, [])|AEnv], get_var(LEnv108, sys_pl, Pl_Get109), f_car(Pl_Get109, Car_Ret234), f_format([[], '$ARRAY'([*], claz_base_character, "property ~:@(~S~):~%   ~~S"), Car_Ret234], Inspect_command_Param209), get_var(LEnv108, sys_pl, Pl_Get110), f_cadr(Pl_Get110, Cadr_Ret235), f_sys_read_inspect_command(Inspect_command_Param209, Cadr_Ret235, [], Inspect_command_Ret236), setq_from_values(LEnv108, [sys_update_flag, sys_new_value]), get_var(LEnv108, sys_update_flag, IFTEST111), (IFTEST111\==[]->get_var(LEnv108, symbol, Symbol_Get117), get_var(LEnv108, sys_new_value, New_value_Get114), get_var(LEnv108, sys_pl, Pl_Get118), f_car(Pl_Get118, Car_Ret237), set_place(LEnv108, setf, [get, Symbol_Get117, Car_Ret237], [New_value_Get114], Setf_R115), LetResult107=Setf_R115;LetResult107=[]), ElseResult122=LetResult107), _15524=ElseResult122), get_var(AEnv, sys_pl, Pl_Get124), f_cddr(Pl_Get124, Cddr_Ret), set_var(AEnv, sys_pl, Cddr_Ret), goto(do_label_28, AEnv), _15408=_GORES)))
					  ]),
			  []=LetResult67
			),
			block_exit([], LetResult67),
			true),
		  get_var(GEnv, symbol, Symbol_Get189),
		  f_sys_print_doc(Symbol_Get189, t, IFTEST187),
		  (   IFTEST187\==[]
		  ->  f_format(
			       [ t,
				 '$ARRAY'([*],
					  claz_base_character,
					  "~&-----------------------------------------------------------------------------~%")
			       ],
			       TrueResult190),
		      _11148=TrueResult190
		  ;   _11148=[]
		  )
		),
		_11148=FnResult
	      ),
	      block_exit(sys_inspect_symbol, FnResult),
	      true).
:- set_opv(sys_inspect_symbol, symbol_function, f_sys_inspect_symbol),
   DefunResult=sys_inspect_symbol.
/*
:- side_effect(assert_lsp(sys_inspect_symbol,
			  lambda_def(defun,
				     sys_inspect_symbol,
				     f_sys_inspect_symbol,
				     [symbol],
				     
				     [ 
				       [ let,
					 [[sys_p, [symbol_package, symbol]]],
					 
					 [ cond,
					   
					   [ [null, sys_p],
					     
					     [ format,
					       t,
					       '$ARRAY'([*],
							claz_base_character,
							"~:@(~S~) - uninterned symbol"),
					       symbol
					     ]
					   ],
					   
					   [ 
					     [ eq,
					       sys_p,
					       
					       [ find_package,
						 '$ARRAY'([*],
							  claz_base_character,
							  "KEYWORD")
					       ]
					     ],
					     
					     [ format,
					       t,
					       '$ARRAY'([*],
							claz_base_character,
							"~:@(~S~) - keyword"),
					       symbol
					     ]
					   ],
					   
					   [ t,
					     
					     [ format,
					       t,
					       '$ARRAY'([*],
							claz_base_character,
							"~:@(~S~) - ~:[internal~;external~] symbol in ~A package"),
					       symbol,
					       
					       [ multiple_value_bind,
						 [b, sys_f],
						 
						 [ find_symbol,
						   [symbol_name, symbol],
						   sys_p
						 ],
						 [declare, [ignore, b]],
						 [eq, sys_f, kw_external]
					       ],
					       [package_name, sys_p]
					     ]
					   ]
					 ]
				       ],
				       
				       [ when,
					 [boundp, symbol],
					 
					 [ if,
					   sys_xx_inspect_mode_xx,
					   
					   [ sys_inspect_recursively,
					     '$ARRAY'([*],
						      claz_base_character,
						      "value:"),
					     [symbol_value, symbol],
					     [symbol_value, symbol]
					   ],
					   
					   [ sys_inspect_print,
					     '$ARRAY'([*],
						      claz_base_character,
						      "value:~%   ~S"),
					     [symbol_value, symbol],
					     [symbol_value, symbol]
					   ]
					 ]
				       ],
				       
				       [ do,
					 
					 [ 
					   [ sys_pl,
					     [symbol_plist, symbol],
					     [cddr, sys_pl]
					   ]
					 ],
					 [[endp, sys_pl]],
					 
					 [ unless,
					   
					   [ and,
					     [symbolp, [car, sys_pl]],
					     
					     [ or,
					       
					       [ eq,
						 [symbol_package, [car, sys_pl]],
						 
						 [ find_package,
						   [quote, sys_system]
						 ]
					       ],
					       
					       [ eq,
						 [symbol_package, [car, sys_pl]],
						 
						 [ find_package,
						   [quote, sys_compiler]
						 ]
					       ]
					     ]
					   ],
					   
					   [ if,
					     sys_xx_inspect_mode_xx,
					     
					     [ sys_inspect_recursively,
					       
					       [ format,
						 [],
						 '$ARRAY'([*],
							  claz_base_character,
							  "property ~S:"),
						 [car, sys_pl]
					       ],
					       [cadr, sys_pl],
					       [get, symbol, [car, sys_pl]]
					     ],
					     
					     [ sys_inspect_print,
					       
					       [ format,
						 [],
						 '$ARRAY'([*],
							  claz_base_character,
							  "property ~:@(~S~):~%   ~~S"),
						 [car, sys_pl]
					       ],
					       [cadr, sys_pl],
					       [get, symbol, [car, sys_pl]]
					     ]
					   ]
					 ]
				       ],
				       
				       [ when,
					 [sys_print_doc, symbol, t],
					 
					 [ format,
					   t,
					   '$ARRAY'([*],
						    claz_base_character,
						    "~&-----------------------------------------------------------------------------~%")
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_symbol,
			  arglist_info(sys_inspect_symbol,
				       f_sys_inspect_symbol,
				       [symbol],
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_symbol,
			  init_args(x, f_sys_inspect_symbol))).
*/
/*
(defun inspect-package (package)
  (format t ""(defun inspect-package (package)\n  (format t \"~S - package\" package)\n  (when (package-nicknames package)\n        (inspect-print \"nicknames:  ~S\" (package-nicknames package)))\n  (when (package-use-list package)\n        (inspect-print \"use list:  ~S\" (package-use-list package)))\n  (when  (package-used-by-list package)\n         (inspect-print \"used-by list:  ~S\" (package-used-by-list package)))\n  (when (package-shadowing-symbols package)\n        (inspect-print \"shadowing symbols:  ~S\"\n                       (package-shadowing-symbols package))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:6222 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-package',[package],[format,t,'$STRING'("~S - package"),package],[when,['package-nicknames',package],['inspect-print','$STRING'("nicknames:  ~S"),['package-nicknames',package]]],[when,['package-use-list',package],['inspect-print','$STRING'("use list:  ~S"),['package-use-list',package]]],[when,['package-used-by-list',package],['inspect-print','$STRING'("used-by list:  ~S"),['package-used-by-list',package]]],[when,['package-shadowing-symbols',package],['inspect-print','$STRING'("shadowing symbols:  ~S"),['package-shadowing-symbols',package]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_package,
					       kw_function,
					       f_sys_inspect_package)).
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"nicknames:  ~S"),[package_nicknames,package]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"nicknames:  ~S"),[package_nicknames,package],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"use list:  ~S"),[package_use_list,package]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"use list:  ~S"),[package_use_list,package],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"used-by list:  ~S"),[package_used_by_list,package]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"used-by list:  ~S"),[package_used_by_list,package],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"shadowing symbols:  ~S"),[package_shadowing_symbols,package]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"shadowing symbols:  ~S"),[package_shadowing_symbols,package],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
wl:lambda_def(defun, sys_inspect_package, f_sys_inspect_package, [package], [[format, t, '$ARRAY'([*], claz_base_character, "~S - package"), package], [when, [package_nicknames, package], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "nicknames:  ~S"), [package_nicknames, package]]], [when, [package_use_list, package], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "use list:  ~S"), [package_use_list, package]]], [when, [package_used_by_list, package], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "used-by list:  ~S"), [package_used_by_list, package]]], [when, [package_shadowing_symbols, package], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "shadowing symbols:  ~S"), [package_shadowing_symbols, package]]]]).
wl:arglist_info(sys_inspect_package, f_sys_inspect_package, [package], arginfo{all:[package], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[package], opt:0, req:[package], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_package).

/*

### Compiled Function: `SYS::INSPECT-PACKAGE` 
*/
f_sys_inspect_package(Package_In, FnResult) :-
	GEnv=[bv(package, Package_In)],
	catch(( ( get_var(GEnv, package, Package_Get),
		  f_format(
			   [ t,
			     '$ARRAY'([*], claz_base_character, "~S - package"),
			     Package_Get
			   ],
			   Format_Ret),
		  get_var(GEnv, package, Package_Get8),
		  f_package_nicknames(Package_Get8, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, package, Package_Get11),
		      f_package_nicknames(Package_Get11, Package_nicknames_Ret),
		      f_sys_read_inspect_command('$ARRAY'([*],
							  claz_base_character,
							  "nicknames:  ~S"),
						 Package_nicknames_Ret,
						 [],
						 IFTEST9),
		      (   IFTEST9\==[]
		      ->  f_princ('$ARRAY'([*],
					   claz_base_character,
					   "Not updated."),
				  Princ_Ret),
			  f_terpri(TrueResult),
			  TrueResult13=TrueResult
		      ;   TrueResult13=[]
		      ),
		      _5746=TrueResult13
		  ;   _5746=[]
		  ),
		  get_var(GEnv, package, Package_Get16),
		  f_package_use_list(Package_Get16, IFTEST14),
		  (   IFTEST14\==[]
		  ->  get_var(GEnv, package, Package_Get19),
		      f_package_use_list(Package_Get19, Use_list_Ret),
		      f_sys_read_inspect_command('$ARRAY'([*],
							  claz_base_character,
							  "use list:  ~S"),
						 Use_list_Ret,
						 [],
						 IFTEST17),
		      (   IFTEST17\==[]
		      ->  f_princ('$ARRAY'([*],
					   claz_base_character,
					   "Not updated."),
				  Princ_Ret45),
			  f_terpri(TrueResult20),
			  TrueResult21=TrueResult20
		      ;   TrueResult21=[]
		      ),
		      _5942=TrueResult21
		  ;   _5942=[]
		  ),
		  get_var(GEnv, package, Package_Get24),
		  f_package_used_by_list(Package_Get24, IFTEST22),
		  (   IFTEST22\==[]
		  ->  get_var(GEnv, package, Package_Get27),
		      f_package_used_by_list(Package_Get27, By_list_Ret),
		      f_sys_read_inspect_command('$ARRAY'([*],
							  claz_base_character,
							  "used-by list:  ~S"),
						 By_list_Ret,
						 [],
						 IFTEST25),
		      (   IFTEST25\==[]
		      ->  f_princ('$ARRAY'([*],
					   claz_base_character,
					   "Not updated."),
				  Princ_Ret47),
			  f_terpri(TrueResult28),
			  TrueResult29=TrueResult28
		      ;   TrueResult29=[]
		      ),
		      _6154=TrueResult29
		  ;   _6154=[]
		  ),
		  get_var(GEnv, package, Package_Get32),
		  f_package_shadowing_symbols(Package_Get32, IFTEST30),
		  (   IFTEST30\==[]
		  ->  get_var(GEnv, package, Package_Get35),
		      f_package_shadowing_symbols(Package_Get35,
						  Shadowing_symbols_Ret),
		      f_sys_read_inspect_command('$ARRAY'([*],
							  claz_base_character,
							  "shadowing symbols:  ~S"),
						 Shadowing_symbols_Ret,
						 [],
						 IFTEST33),
		      (   IFTEST33\==[]
		      ->  f_princ('$ARRAY'([*],
					   claz_base_character,
					   "Not updated."),
				  Princ_Ret49),
			  f_terpri(TrueResult36),
			  TrueResult37=TrueResult36
		      ;   TrueResult37=[]
		      ),
		      _5724=TrueResult37
		  ;   _5724=[]
		  )
		),
		_5724=FnResult
	      ),
	      block_exit(sys_inspect_package, FnResult),
	      true).
:- set_opv(sys_inspect_package, symbol_function, f_sys_inspect_package),
   DefunResult=sys_inspect_package.
/*
:- side_effect(assert_lsp(sys_inspect_package,
			  lambda_def(defun,
				     sys_inspect_package,
				     f_sys_inspect_package,
				     [package],
				     
				     [ 
				       [ format,
					 t,
					 '$ARRAY'([*],
						  claz_base_character,
						  "~S - package"),
					 package
				       ],
				       
				       [ when,
					 [package_nicknames, package],
					 
					 [ sys_inspect_print,
					   '$ARRAY'([*],
						    claz_base_character,
						    "nicknames:  ~S"),
					   [package_nicknames, package]
					 ]
				       ],
				       
				       [ when,
					 [package_use_list, package],
					 
					 [ sys_inspect_print,
					   '$ARRAY'([*],
						    claz_base_character,
						    "use list:  ~S"),
					   [package_use_list, package]
					 ]
				       ],
				       
				       [ when,
					 [package_used_by_list, package],
					 
					 [ sys_inspect_print,
					   '$ARRAY'([*],
						    claz_base_character,
						    "used-by list:  ~S"),
					   [package_used_by_list, package]
					 ]
				       ],
				       
				       [ when,
					 [package_shadowing_symbols, package],
					 
					 [ sys_inspect_print,
					   '$ARRAY'([*],
						    claz_base_character,
						    "shadowing symbols:  ~S"),
					   [package_shadowing_symbols, package]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_package,
			  arglist_info(sys_inspect_package,
				       f_sys_inspect_package,
				       [package],
				       arginfo{ all:[package],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[package],
						opt:0,
						req:[package],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_package,
			  init_args(x, f_sys_inspect_package))).
*/
/*
(defun inspect-character (character)
  (format t
          (cond ((standard-char-p character) ""(defun inspect-character (character)\n  (format t\n          (cond ((standard-char-p character) \"~S - standard character\")\n                ((string-char-p character) \"~S - string character\")\n                (t \"~S - character\"))\n          character)\n  (inspect-print \"code:  #x~X\" (char-code character))\n  (inspect-print \"bits:  ~D\" (char-bits character))\n  (inspect-print \"font:  ~D\" (char-font character)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:6772 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-character',[character],[format,t,[cond,[['standard-char-p',character],'$STRING'("~S - standard character")],[['string-char-p',character],'$STRING'("~S - string character")],[t,'$STRING'("~S - character")]],character],['inspect-print','$STRING'("code:  #x~X"),['char-code',character]],['inspect-print','$STRING'("bits:  ~D"),['char-bits',character]],['inspect-print','$STRING'("font:  ~D"),['char-font',character]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_character,
					       kw_function,
					       f_sys_inspect_character)).
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"code:  #x~X"),[char_code,character]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"code:  #x~X"),[char_code,character],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"bits:  ~D"),[sys_char_bits,character]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"bits:  ~D"),[sys_char_bits,character],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"font:  ~D"),[sys_char_font,character]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"font:  ~D"),[sys_char_font,character],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
wl:lambda_def(defun, sys_inspect_character, f_sys_inspect_character, [character], [[format, t, [cond, [[standard_char_p, character], '$ARRAY'([*], claz_base_character, "~S - standard character")], [[sys_string_char_p, character], '$ARRAY'([*], claz_base_character, "~S - string character")], [t, '$ARRAY'([*], claz_base_character, "~S - character")]], character], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "code:  #x~X"), [char_code, character]], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "bits:  ~D"), [sys_char_bits, character]], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "font:  ~D"), [sys_char_font, character]]]).
wl:arglist_info(sys_inspect_character, f_sys_inspect_character, [character], arginfo{all:[character], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[character], opt:0, req:[character], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_character).

/*

### Compiled Function: `SYS::INSPECT-CHARACTER` 
*/
f_sys_inspect_character(Character_In, FnResult) :-
	GEnv=[bv(character, Character_In)],
	catch(( ( get_var(GEnv, character, Character_Get),
		  f_standard_char_p(Character_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  CAR='$ARRAY'([*], claz_base_character, "~S - standard character")
		  ;   get_var(GEnv, character, Character_Get10),
		      f_sys_string_char_p(Character_Get10, IFTEST8),
		      (   IFTEST8\==[]
		      ->  ElseResult='$ARRAY'([*], claz_base_character, "~S - string character")
		      ;   ElseResult='$ARRAY'([*], claz_base_character, "~S - character")
		      ),
		      CAR=ElseResult
		  ),
		  get_var(GEnv, character, Character_Get12),
		  f_format([t, CAR, Character_Get12], Format_Ret),
		  get_var(GEnv, character, Character_Get15),
		  f_char_code(Character_Get15, Char_code_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "code:  #x~X"),
					     Char_code_Ret,
					     [],
					     IFTEST13),
		  (   IFTEST13\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret),
		      f_terpri(TrueResult),
		      _5704=TrueResult
		  ;   _5704=[]
		  ),
		  get_var(GEnv, character, Character_Get19),
		  f_sys_char_bits(Character_Get19, Char_bits_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "bits:  ~D"),
					     Char_bits_Ret,
					     [],
					     IFTEST17),
		  (   IFTEST17\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret33),
		      f_terpri(TrueResult20),
		      _5804=TrueResult20
		  ;   _5804=[]
		  ),
		  get_var(GEnv, character, Character_Get23),
		  f_sys_char_font(Character_Get23, Char_font_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "font:  ~D"),
					     Char_font_Ret,
					     [],
					     IFTEST21),
		  (   IFTEST21\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret35),
		      f_terpri(TrueResult24),
		      _5528=TrueResult24
		  ;   _5528=[]
		  )
		),
		_5528=FnResult
	      ),
	      block_exit(sys_inspect_character, FnResult),
	      true).
:- set_opv(sys_inspect_character, symbol_function, f_sys_inspect_character),
   DefunResult=sys_inspect_character.
/*
:- side_effect(assert_lsp(sys_inspect_character,
			  lambda_def(defun,
				     sys_inspect_character,
				     f_sys_inspect_character,
				     [character],
				     
				     [ 
				       [ format,
					 t,
					 
					 [ cond,
					   
					   [ [standard_char_p, character],
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - standard character")
					   ],
					   
					   [ [sys_string_char_p, character],
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - string character")
					   ],
					   
					   [ t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - character")
					   ]
					 ],
					 character
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "code:  #x~X"),
					 [char_code, character]
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "bits:  ~D"),
					 [sys_char_bits, character]
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "font:  ~D"),
					 [sys_char_font, character]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_character,
			  arglist_info(sys_inspect_character,
				       f_sys_inspect_character,
				       [character],
				       arginfo{ all:[character],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[character],
						opt:0,
						req:[character],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_character,
			  init_args(x, f_sys_inspect_character))).
*/
/*
(defun inspect-number (number)
  (case (type-of number)
    (FIXNUM (format t ""(defun inspect-number (number)\n  (case (type-of number)\n    (FIXNUM (format t \"~S - fixnum (32 bits)\" number))\n    (BIGNUM (format t \"~S - bignum\" number))\n    (RATIO\n     (format t \"~S - ratio\" number)\n     (inspect-recursively \"numerator:\" (numerator number))\n     (inspect-recursively \"denominator:\" (denominator number)))\n    (COMPLEX\n     (format t \"~S - complex\" number)\n     (inspect-recursively \"real part:\" (realpart number))\n     (inspect-recursively \"imaginary part:\" (imagpart number)))\n    ((SHORT-FLOAT SINGLE-FLOAT)\n     (format t \"~S - short-float\" number)\n     (multiple-value-bind (signif expon sign)\n          (integer-decode-float number)\n       (declare (ignore sign))\n       (inspect-print \"exponent:  ~D\" expon)\n       (inspect-print \"mantissa:  ~D\" signif)))\n    ((LONG-FLOAT DOUBLE-FLOAT)\n     (format t \"~S - long-float\" number)\n     (multiple-value-bind (signif expon sign)\n          (integer-decode-float number)\n       (declare (ignore sign))\n       (inspect-print \"exponent:  ~D\" expon)\n       (inspect-print \"mantissa:  ~D\" signif)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:7180 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-number',[number],[case,['type-of',number],['FIXNUM',[format,t,'$STRING'("~S - fixnum (32 bits)"),number]],['BIGNUM',[format,t,'$STRING'("~S - bignum"),number]],['RATIO',[format,t,'$STRING'("~S - ratio"),number],['inspect-recursively','$STRING'("numerator:"),[numerator,number]],['inspect-recursively','$STRING'("denominator:"),[denominator,number]]],['COMPLEX',[format,t,'$STRING'("~S - complex"),number],['inspect-recursively','$STRING'("real part:"),[realpart,number]],['inspect-recursively','$STRING'("imaginary part:"),[imagpart,number]]],[['SHORT-FLOAT','SINGLE-FLOAT'],[format,t,'$STRING'("~S - short-float"),number],['multiple-value-bind',[signif,expon,sign],['integer-decode-float',number],[declare,[ignore,sign]],['inspect-print','$STRING'("exponent:  ~D"),expon],['inspect-print','$STRING'("mantissa:  ~D"),signif]]],[['LONG-FLOAT','DOUBLE-FLOAT'],[format,t,'$STRING'("~S - long-float"),number],['multiple-value-bind',[signif,expon,sign],['integer-decode-float',number],[declare,[ignore,sign]],['inspect-print','$STRING'("exponent:  ~D"),expon],['inspect-print','$STRING'("mantissa:  ~D"),signif]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_number,
					       kw_function,
					       f_sys_inspect_number)).
*/
/*
% case:-[[fixnum,[format,t,'$ARRAY'([*],claz_base_character,"~S - fixnum (32 bits)"),number]],[bignum,[format,t,'$ARRAY'([*],claz_base_character,"~S - bignum"),number]],[ratio,[format,t,'$ARRAY'([*],claz_base_character,"~S - ratio"),number],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"numerator:"),[numerator,number]],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"denominator:"),[denominator,number]]],[complex,[format,t,'$ARRAY'([*],claz_base_character,"~S - complex"),number],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"real part:"),[realpart,number]],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"imaginary part:"),[imagpart,number]]],[[short_float,single_float],[format,t,'$ARRAY'([*],claz_base_character,"~S - short-float"),number],[multiple_value_bind,[sys_signif,sys_expon,sys_sign],[integer_decode_float,number],[declare,[ignore,sys_sign]],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif]]],[[long_float,double_float],[format,t,'$ARRAY'([*],claz_base_character,"~S - long-float"),number],[multiple_value_bind,[sys_signif,sys_expon,sys_sign],[integer_decode_float,number],[declare,[ignore,sys_sign]],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif]]]].
*/
/*
% conds:-[[[eq,_29042,[quote,fixnum]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"~S - fixnum (32 bits)"),number]]],[[eq,_29042,[quote,bignum]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"~S - bignum"),number]]],[[eq,_29042,[quote,ratio]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"~S - ratio"),number],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"numerator:"),[numerator,number]],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"denominator:"),[denominator,number]]]],[[eq,_29042,[quote,complex]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"~S - complex"),number],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"real part:"),[realpart,number]],[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"imaginary part:"),[imagpart,number]]]],[[sys_memq,_29042,[quote,[short_float,single_float]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"~S - short-float"),number],[multiple_value_bind,[sys_signif,sys_expon,sys_sign],[integer_decode_float,number],[declare,[ignore,sys_sign]],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif]]]],[[sys_memq,_29042,[quote,[long_float,double_float]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"~S - long-float"),number],[multiple_value_bind,[sys_signif,sys_expon,sys_sign],[integer_decode_float,number],[declare,[ignore,sys_sign]],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon],[sys_inspect_print,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif]]]]].
*/
/*
% macroexpand:-[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"numerator:"),[numerator,number]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"numerator:"),[numerator,number],t],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"denominator:"),[denominator,number]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"denominator:"),[denominator,number],t],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"real part:"),[realpart,number]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"real part:"),[realpart,number],t],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"imaginary part:"),[imagpart,number]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"imaginary part:"),[imagpart,number],t],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon,[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif,[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"exponent:  ~D"),sys_expon,[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"mantissa:  ~D"),sys_signif,[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
wl:lambda_def(defun, sys_inspect_number, f_sys_inspect_number, [number], [[case, [type_of, number], [fixnum, [format, t, '$ARRAY'([*], claz_base_character, "~S - fixnum (32 bits)"), number]], [bignum, [format, t, '$ARRAY'([*], claz_base_character, "~S - bignum"), number]], [ratio, [format, t, '$ARRAY'([*], claz_base_character, "~S - ratio"), number], [sys_inspect_recursively, '$ARRAY'([*], claz_base_character, "numerator:"), [numerator, number]], [sys_inspect_recursively, '$ARRAY'([*], claz_base_character, "denominator:"), [denominator, number]]], [complex, [format, t, '$ARRAY'([*], claz_base_character, "~S - complex"), number], [sys_inspect_recursively, '$ARRAY'([*], claz_base_character, "real part:"), [realpart, number]], [sys_inspect_recursively, '$ARRAY'([*], claz_base_character, "imaginary part:"), [imagpart, number]]], [[short_float, single_float], [format, t, '$ARRAY'([*], claz_base_character, "~S - short-float"), number], [multiple_value_bind, [sys_signif, sys_expon, sys_sign], [integer_decode_float, number], [declare, [ignore, sys_sign]], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "exponent:  ~D"), sys_expon], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "mantissa:  ~D"), sys_signif]]], [[long_float, double_float], [format, t, '$ARRAY'([*], claz_base_character, "~S - long-float"), number], [multiple_value_bind, [sys_signif, sys_expon, sys_sign], [integer_decode_float, number], [declare, [ignore, sys_sign]], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "exponent:  ~D"), sys_expon], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "mantissa:  ~D"), sys_signif]]]]]).
wl:arglist_info(sys_inspect_number, f_sys_inspect_number, [number], arginfo{all:[number], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[number], opt:0, req:[number], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_number).

/*

### Compiled Function: `SYS::INSPECT-NUMBER` 
*/
f_sys_inspect_number(Number_In, FnResult) :-
	GEnv=[bv(number, Number_In)],
	catch(( ( get_var(GEnv, number, Number_Get),
		  f_type_of(Number_Get, Key),
		  (   is_eq(Key, fixnum)
		  ->  get_var(GEnv, number, Number_Get10),
		      f_format(
			       [ t,
				 '$ARRAY'([*],
					  claz_base_character,
					  "~S - fixnum (32 bits)"),
				 Number_Get10
			       ],
			       TrueResult76),
		      _8902=TrueResult76
		  ;   (   is_eq(Key, bignum)
		      ->  get_var(GEnv, number, Number_Get13),
			  f_format(
				   [ t,
				     '$ARRAY'([*],
					      claz_base_character,
					      "~S - bignum"),
				     Number_Get13
				   ],
				   TrueResult74),
			  ElseResult77=TrueResult74
		      ;   (   is_eq(Key, ratio)
			  ->  get_var(GEnv, number, Number_Get16),
			      f_format(
				       [ t,
					 '$ARRAY'([*],
						  claz_base_character,
						  "~S - ratio"),
					 Number_Get16
				       ],
				       Format_Ret),
			      get_var(GEnv, number, Number_Get19),
			      f_numerator(Number_Get19, Numerator_Ret),
			      f_sys_read_inspect_command('$ARRAY'([*],
								  claz_base_character,
								  "numerator:"),
							 Numerator_Ret,
							 t,
							 IFTEST17),
			      (   IFTEST17\==[]
			      ->  f_princ('$ARRAY'([*],
						   claz_base_character,
						   "Not updated."),
					  Princ_Ret),
				  f_terpri(TrueResult),
				  _9190=TrueResult
			      ;   _9190=[]
			      ),
			      get_var(GEnv, number, Number_Get23),
			      f_denominator(Number_Get23, Denominator_Ret),
			      f_sys_read_inspect_command('$ARRAY'([*],
								  claz_base_character,
								  "denominator:"),
							 Denominator_Ret,
							 t,
							 IFTEST21),
			      (   IFTEST21\==[]
			      ->  f_princ('$ARRAY'([*],
						   claz_base_character,
						   "Not updated."),
					  Princ_Ret85),
				  f_terpri(TrueResult24),
				  TrueResult72=TrueResult24
			      ;   TrueResult72=[]
			      ),
			      ElseResult75=TrueResult72
			  ;   (   is_eq(Key, complex)
			      ->  get_var(GEnv, number, Number_Get27),
				  f_format(
					   [ t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - complex"),
					     Number_Get27
					   ],
					   Format_Ret86),
				  get_var(GEnv, number, Number_Get30),
				  f_realpart(Number_Get30, Realpart_Ret),
				  f_sys_read_inspect_command('$ARRAY'([*],
								      claz_base_character,
								      "real part:"),
							     Realpart_Ret,
							     t,
							     IFTEST28),
				  (   IFTEST28\==[]
				  ->  f_princ('$ARRAY'([*],
						       claz_base_character,
						       "Not updated."),
					      Princ_Ret88),
				      f_terpri(TrueResult31),
				      _9474=TrueResult31
				  ;   _9474=[]
				  ),
				  get_var(GEnv, number, Number_Get34),
				  f_imagpart(Number_Get34, Imagpart_Ret),
				  f_sys_read_inspect_command('$ARRAY'([*],
								      claz_base_character,
								      "imaginary part:"),
							     Imagpart_Ret,
							     t,
							     IFTEST32),
				  (   IFTEST32\==[]
				  ->  f_princ('$ARRAY'([*],
						       claz_base_character,
						       "Not updated."),
					      Princ_Ret90),
				      f_terpri(TrueResult35),
				      TrueResult70=TrueResult35
				  ;   TrueResult70=[]
				  ),
				  ElseResult73=TrueResult70
			      ;   f_sys_memq(Key,
					     [short_float, single_float],
					     IFTEST36),
				  (   IFTEST36\==[]
				  ->  get_var(GEnv, number, Number_Get38),
				      f_format(
					       [ t,
						 '$ARRAY'([*],
							  claz_base_character,
							  "~S - short-float"),
						 Number_Get38
					       ],
					       Format_Ret91),
				      LEnv=[bv(sys_signif, []), bv(sys_expon, []), bv(sys_sign, [])|GEnv],
				      get_var(LEnv, number, Number_Get42),
				      f_integer_decode_float(Number_Get42,
							     Decode_float_Ret),
				      setq_from_values(LEnv,
						       
						       [ sys_signif,
							 sys_expon,
							 sys_sign
						       ]),
				      sf_declare(LEnv,
						 [ignore, sys_sign],
						 Sf_declare_Ret),
				      get_var(LEnv, sys_expon, Expon_Get),
				      f_sys_read_inspect_command('$ARRAY'([*],
									  claz_base_character,
									  "exponent:  ~D"),
								 Expon_Get,
								 [],
								 IFTEST43),
				      (   IFTEST43\==[]
				      ->  f_princ('$ARRAY'([*],
							   claz_base_character,
							   "Not updated."),
						  Princ_Ret94),
					  f_terpri(TrueResult46),
					  _9854=TrueResult46
				      ;   _9854=[]
				      ),
				      get_var(LEnv, sys_signif, Signif_Get),
				      f_sys_read_inspect_command('$ARRAY'([*],
									  claz_base_character,
									  "mantissa:  ~D"),
								 Signif_Get,
								 [],
								 IFTEST47),
				      (   IFTEST47\==[]
				      ->  f_princ('$ARRAY'([*],
							   claz_base_character,
							   "Not updated."),
						  Princ_Ret95),
					  f_terpri(TrueResult50),
					  LetResult=TrueResult50
				      ;   LetResult=[]
				      ),
				      ElseResult71=LetResult
				  ;   f_sys_memq(Key,
						 [long_float, double_float],
						 IFTEST51),
				      (   IFTEST51\==[]
				      ->  get_var(GEnv, number, Number_Get53),
					  f_format(
						   [ t,
						     '$ARRAY'([*],
							      claz_base_character,
							      "~S - long-float"),
						     Number_Get53
						   ],
						   Format_Ret96),
					  LEnv56=[bv(sys_signif, []), bv(sys_expon, []), bv(sys_sign, [])|GEnv],
					  get_var(LEnv56, number, Number_Get57),
					  f_integer_decode_float(Number_Get57,
								 Decode_float_Ret97),
					  setq_from_values(LEnv56,
							   
							   [ sys_signif,
							     sys_expon,
							     sys_sign
							   ]),
					  sf_declare(LEnv56,
						     [ignore, sys_sign],
						     Sf_declare_Ret98),
					  get_var(LEnv56,
						  sys_expon,
						  Expon_Get60),
					  f_sys_read_inspect_command('$ARRAY'([*],
									      claz_base_character,
									      "exponent:  ~D"),
								     Expon_Get60,
								     [],
								     IFTEST58),
					  (   IFTEST58\==[]
					  ->  f_princ('$ARRAY'([*],
							       claz_base_character,
							       "Not updated."),
						      Princ_Ret99),
					      f_terpri(TrueResult61),
					      _10238=TrueResult61
					  ;   _10238=[]
					  ),
					  get_var(LEnv56,
						  sys_signif,
						  Signif_Get64),
					  f_sys_read_inspect_command('$ARRAY'([*],
									      claz_base_character,
									      "mantissa:  ~D"),
								     Signif_Get64,
								     [],
								     IFTEST62),
					  (   IFTEST62\==[]
					  ->  f_princ('$ARRAY'([*],
							       claz_base_character,
							       "Not updated."),
						      Princ_Ret100),
					      f_terpri(TrueResult65),
					      LetResult55=TrueResult65
					  ;   LetResult55=[]
					  ),
					  ElseResult69=LetResult55
				      ;   ElseResult=[],
					  ElseResult69=ElseResult
				      ),
				      ElseResult71=ElseResult69
				  ),
				  ElseResult73=ElseResult71
			      ),
			      ElseResult75=ElseResult73
			  ),
			  ElseResult77=ElseResult75
		      ),
		      _8902=ElseResult77
		  )
		),
		_8902=FnResult
	      ),
	      block_exit(sys_inspect_number, FnResult),
	      true).
:- set_opv(sys_inspect_number, symbol_function, f_sys_inspect_number),
   DefunResult=sys_inspect_number.
/*
:- side_effect(assert_lsp(sys_inspect_number,
			  lambda_def(defun,
				     sys_inspect_number,
				     f_sys_inspect_number,
				     [number],
				     
				     [ 
				       [ case,
					 [type_of, number],
					 
					 [ fixnum,
					   
					   [ format,
					     t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - fixnum (32 bits)"),
					     number
					   ]
					 ],
					 
					 [ bignum,
					   
					   [ format,
					     t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - bignum"),
					     number
					   ]
					 ],
					 
					 [ ratio,
					   
					   [ format,
					     t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - ratio"),
					     number
					   ],
					   
					   [ sys_inspect_recursively,
					     '$ARRAY'([*],
						      claz_base_character,
						      "numerator:"),
					     [numerator, number]
					   ],
					   
					   [ sys_inspect_recursively,
					     '$ARRAY'([*],
						      claz_base_character,
						      "denominator:"),
					     [denominator, number]
					   ]
					 ],
					 
					 [ complex,
					   
					   [ format,
					     t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - complex"),
					     number
					   ],
					   
					   [ sys_inspect_recursively,
					     '$ARRAY'([*],
						      claz_base_character,
						      "real part:"),
					     [realpart, number]
					   ],
					   
					   [ sys_inspect_recursively,
					     '$ARRAY'([*],
						      claz_base_character,
						      "imaginary part:"),
					     [imagpart, number]
					   ]
					 ],
					 
					 [ [short_float, single_float],
					   
					   [ format,
					     t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - short-float"),
					     number
					   ],
					   
					   [ multiple_value_bind,
					     [sys_signif, sys_expon, sys_sign],
					     [integer_decode_float, number],
					     [declare, [ignore, sys_sign]],
					     
					     [ sys_inspect_print,
					       '$ARRAY'([*],
							claz_base_character,
							"exponent:  ~D"),
					       sys_expon
					     ],
					     
					     [ sys_inspect_print,
					       '$ARRAY'([*],
							claz_base_character,
							"mantissa:  ~D"),
					       sys_signif
					     ]
					   ]
					 ],
					 
					 [ [long_float, double_float],
					   
					   [ format,
					     t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - long-float"),
					     number
					   ],
					   
					   [ multiple_value_bind,
					     [sys_signif, sys_expon, sys_sign],
					     [integer_decode_float, number],
					     [declare, [ignore, sys_sign]],
					     
					     [ sys_inspect_print,
					       '$ARRAY'([*],
							claz_base_character,
							"exponent:  ~D"),
					       sys_expon
					     ],
					     
					     [ sys_inspect_print,
					       '$ARRAY'([*],
							claz_base_character,
							"mantissa:  ~D"),
					       sys_signif
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_number,
			  arglist_info(sys_inspect_number,
				       f_sys_inspect_number,
				       [number],
				       arginfo{ all:[number],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[number],
						opt:0,
						req:[number],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_number,
			  init_args(x, f_sys_inspect_number))).
*/
/*
(defun inspect-cons (cons)
  (format t
          (case
	      #-LOCATIVE (car cons)
	      #+LOCATIVE
	      (let ((acar (car cons)))
		(cond ((locativep acar)
		       (dereference acar))
		      ((sl-boundp acar) acar)
		      (t nil)))
            ((LAMBDA LAMBDA-BLOCK LAMBDA-CLOSURE LAMBDA-BLOCK-CLOSURE)
             ""(defun inspect-cons (cons)\n  (format t\n          (case\n\t      #-LOCATIVE (car cons)\n\t      #+LOCATIVE\n\t      (let ((acar (car cons)))\n\t\t(cond ((locativep acar)\n\t\t       (dereference acar))\n\t\t      ((sl-boundp acar) acar)\n\t\t      (t nil)))\n            ((LAMBDA LAMBDA-BLOCK LAMBDA-CLOSURE LAMBDA-BLOCK-CLOSURE)\n             \"~S - function\")\n            (QUOTE \"~S - constant\")\n            (t \"~S - cons\"))\n          cons)\n  (when *inspect-mode*\n        (do ((i 0 (1+ i))\n             (l cons (cdr l)))\n            ((atom l)\n             (inspect-recursively (format nil \"nthcdr ~D:\" i)\n                                  l (cdr (nthcdr (1- i) cons))))\n          (inspect-recursively (format nil \"nth ~D:\" i)\n                               (car l) (nth i cons)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:8248 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-cons',[cons],[format,t,[case,[car,cons],[['LAMBDA','LAMBDA-BLOCK','LAMBDA-CLOSURE','LAMBDA-BLOCK-CLOSURE'],'$STRING'("~S - function")],['QUOTE','$STRING'("~S - constant")],[t,'$STRING'("~S - cons")]],cons],[when,'*inspect-mode*',[do,[[i,0,['1+',i]],[l,cons,[cdr,l]]],[[atom,l],['inspect-recursively',[format,[],'$STRING'("nthcdr ~D:"),i],l,[cdr,[nthcdr,['1-',i],cons]]]],['inspect-recursively',[format,[],'$STRING'("nth ~D:"),i],[car,l],[nth,i,cons]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_cons,
					       kw_function,
					       f_sys_inspect_cons)).
*/
/*
% case:-[[[lambda,sys_lambda_block,sys_lambda_closure,sys_lambda_block_closure],'$ARRAY'([*],claz_base_character,"~S - function")],[quote,'$ARRAY'([*],claz_base_character,"~S - constant")],[t,'$ARRAY'([*],claz_base_character,"~S - cons")]].
*/
/*
% conds:-[[[sys_memq,_24334,[quote,[lambda,sys_lambda_block,sys_lambda_closure,sys_lambda_block_closure]]],[progn,'$ARRAY'([*],claz_base_character,"~S - function")]],[[eq,_24334,[quote,quote]],[progn,'$ARRAY'([*],claz_base_character,"~S - constant")]],[t,[progn,'$ARRAY'([*],claz_base_character,"~S - cons")]]].
*/
/*
% macroexpand:-[sys_inspect_recursively,[format,[],'$ARRAY'([*],claz_base_character,"nthcdr ~D:"),sys_i],sys_l,[cdr,[nthcdr,['1-',sys_i],cons]]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"nthcdr ~D:"),sys_i],sys_l,t],[when,sys_update_flag,[setf,[cdr,[nthcdr,['1-',sys_i],cons]],sys_new_value]]].
*/
/*
:-side_effect((compile_each([name='GLOBAL',environ=env_1],_18188,[],[],true),append([[nthcdr,['1-',sys_i],cons]],[_53334,_27408],[[nthcdr,['1-',sys_i],cons],_53334,_27408]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([name='GLOBAL',environ=env_1],_18166,[],[],true),append([[nthcdr,['1-',sys_i],cons]],[_48566,_22640],[[nthcdr,['1-',sys_i],cons],_48566,_22640]),setf_inverse_op(cdr,rplacd))).
*/
/*
% macroexpand:-[sys_inspect_recursively,[format,[],'$ARRAY'([*],claz_base_character,"nth ~D:"),sys_i],[car,sys_l],[nth,sys_i,cons]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"nth ~D:"),sys_i],[car,sys_l],t],[when,sys_update_flag,[setf,[nth,sys_i,cons],sys_new_value]]].
*/
/*
:-failure(show_call_trace((compile_each([name='GLOBAL',environ=env_1],_19124,[cons],_32410,_31870),append([sys_i|_32410],[_51452,_51280],_32426),setf_inverse_op(nth,_31858)))).
*/
/*
:-failure(show_call_trace((compile_each([name='GLOBAL',environ=env_1],_19098,[cons],_20898,_20842),append([sys_i|_20898],[_25406,_25234],_20914),setf_inverse_op(nth,_20830)))).
*/
/*
% macroexpand:-[sys_inspect_recursively,[format,[],'$ARRAY'([*],claz_base_character,"nthcdr ~D:"),sys_i],sys_l,[cdr,[nthcdr,['1-',sys_i],cons]]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"nthcdr ~D:"),sys_i],sys_l,t],[when,sys_update_flag,[setf,[cdr,[nthcdr,['1-',sys_i],cons]],sys_new_value]]].
*/
/*
:-side_effect((compile_each([name='GLOBAL',environ=env_1],_20482,[],[],true),append([[nthcdr,['1-',sys_i],cons]],[_55774,_55602],[[nthcdr,['1-',sys_i],cons],_55774,_55602]),setf_inverse_op(cdr,rplacd))).
*/
/*
:-side_effect((compile_each([name='GLOBAL',environ=env_1],_20474,[],[],true),append([[nthcdr,['1-',sys_i],cons]],[_50942,_50770],[[nthcdr,['1-',sys_i],cons],_50942,_50770]),setf_inverse_op(cdr,rplacd))).
*/
/*
% macroexpand:-[sys_inspect_recursively,[format,[],'$ARRAY'([*],claz_base_character,"nth ~D:"),sys_i],[car,sys_l],[nth,sys_i,cons]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,[format,[],'$ARRAY'([*],claz_base_character,"nth ~D:"),sys_i],[car,sys_l],t],[when,sys_update_flag,[setf,[nth,sys_i,cons],sys_new_value]]].
*/
/*
:-failure(show_call_trace((compile_each([name='GLOBAL',environ=env_1],_21526,[cons],_34812,_34272),append([sys_i|_34812],[_53854,_53682],_34828),setf_inverse_op(nth,_34260)))).
*/
/*
:-failure(show_call_trace((compile_each([name='GLOBAL',environ=env_1],_21492,[cons],_23292,_23236),append([sys_i|_23292],[_27800,_27628],_23308),setf_inverse_op(nth,_23224)))).
*/
wl:lambda_def(defun, sys_inspect_cons, f_sys_inspect_cons, [cons], [[format, t, [case, [car, cons], [[lambda, sys_lambda_block, sys_lambda_closure, sys_lambda_block_closure], '$ARRAY'([*], claz_base_character, "~S - function")], [quote, '$ARRAY'([*], claz_base_character, "~S - constant")], [t, '$ARRAY'([*], claz_base_character, "~S - cons")]], cons], [when, sys_xx_inspect_mode_xx, [do, [[sys_i, 0, ['1+', sys_i]], [sys_l, cons, [cdr, sys_l]]], [[atom, sys_l], [sys_inspect_recursively, [format, [], '$ARRAY'([*], claz_base_character, "nthcdr ~D:"), sys_i], sys_l, [cdr, [nthcdr, ['1-', sys_i], cons]]]], [sys_inspect_recursively, [format, [], '$ARRAY'([*], claz_base_character, "nth ~D:"), sys_i], [car, sys_l], [nth, sys_i, cons]]]]]).
wl:arglist_info(sys_inspect_cons, f_sys_inspect_cons, [cons], arginfo{all:[cons], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[cons], opt:0, req:[cons], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_cons).

/*

### Compiled Function: `SYS::INSPECT-CONS` 
*/
f_sys_inspect_cons(Cons_In, FnResult) :-
	GEnv=[bv(cons, Cons_In)],
	catch(( ( get_var(GEnv, cons, Cons_Get),
		  f_car(Cons_Get, Key),
		  f_sys_memq(Key,
			     
			     [ lambda,
			       sys_lambda_block,
			       sys_lambda_closure,
			       sys_lambda_block_closure
			     ],
			     IFTEST),
		  (   IFTEST\==[]
		  ->  CAR123='$ARRAY'([*], claz_base_character, "~S - function")
		  ;   (   is_eq(Key, quote)
		      ->  ElseResult='$ARRAY'([*], claz_base_character, "~S - constant")
		      ;   ElseResult='$ARRAY'([*], claz_base_character, "~S - cons")
		      ),
		      CAR123=ElseResult
		  ),
		  get_var(GEnv, cons, Cons_Get13),
		  f_format([t, CAR123, Cons_Get13], Format_Ret),
		  get_var(GEnv, sys_xx_inspect_mode_xx, IFTEST14),
		  (   IFTEST14\==[]
		  ->  get_var(GEnv, cons, Cons_Get20),
		      BlockExitEnv=[bv(sys_i, 0), bv(sys_l, Cons_Get20)|GEnv],
		      catch(( call_addr_block(BlockExitEnv,
					      (push_label(do_label_29), get_var(BlockExitEnv, sys_l, L_Get66), (L_Get66\=[CAR124|CDR]->LEnv73=[bv(sys_update_flag, []), bv(sys_new_value, [])|BlockExitEnv], get_var(LEnv73, sys_i, I_Get74), f_format([[], '$ARRAY'([*], claz_base_character, "nthcdr ~D:"), I_Get74], Inspect_command_Param), get_var(LEnv73, sys_l, L_Get75), f_sys_read_inspect_command(Inspect_command_Param, L_Get75, t, T), setq_from_values(LEnv73, [sys_update_flag, sys_new_value]), get_var(LEnv73, sys_update_flag, IFTEST76), (IFTEST76\==[]->get_var(LEnv73, sys_i, I_Get81), 'f_1-'(I_Get81, Nthcdr_Param), get_var(LEnv73, cons, Cons_Get82), f_nthcdr(Nthcdr_Param, Cons_Get82, Rplacd_Param), get_var(LEnv73, sys_new_value, New_value_Get83), f_rplacd(Rplacd_Param, New_value_Get83, TrueResult84), RetResult69=TrueResult84;RetResult69=[]), throw(block_exit([], RetResult69)), _TBResult=ThrowResult70;LEnv88=[bv(sys_update_flag, []), bv(sys_new_value, [])|BlockExitEnv], get_var(LEnv88, sys_i, I_Get89), f_format([[], '$ARRAY'([*], claz_base_character, "nth ~D:"), I_Get89], Inspect_command_Param117), get_var(LEnv88, sys_l, L_Get90), f_car(L_Get90, Car_Ret), f_sys_read_inspect_command(Inspect_command_Param117, Car_Ret, t, T111), setq_from_values(LEnv88, [sys_update_flag, sys_new_value]), get_var(LEnv88, sys_update_flag, IFTEST91), (IFTEST91\==[]->get_var(LEnv88, cons, Cons_Get98), get_var(LEnv88, sys_i, I_Get97), get_var(LEnv88, sys_new_value, New_value_Get94), set_place(LEnv88, setf, [nth, I_Get97, Cons_Get98], [New_value_Get94], Setf_R95), LetResult87=Setf_R95;LetResult87=[]), get_var(BlockExitEnv, sys_i, I_Get100), 'f_1+'(I_Get100, I), get_var(BlockExitEnv, sys_l, L_Get101), f_cdr(L_Get101, L), set_var(BlockExitEnv, sys_i, I), set_var(BlockExitEnv, sys_l, L), goto(do_label_29, BlockExitEnv), _TBResult=_GORES102)),
					      
					      [ addr(addr_tagbody_29_do_label_29,
						     do_label_29,
						     '$unused',
						     BlockExitEnv,
						     (get_var(BlockExitEnv, sys_l, L_Get), (L_Get\=[CAR127|CDR128]->LEnv31=[bv(sys_update_flag, []), bv(sys_new_value, [])|BlockExitEnv], get_var(LEnv31, sys_i, Get_var_Ret), f_format([[], '$ARRAY'([*], claz_base_character, "nthcdr ~D:"), Get_var_Ret], Inspect_command_Param118), get_var(LEnv31, sys_l, L_Get33), f_sys_read_inspect_command(Inspect_command_Param118, L_Get33, t, Inspect_command_Ret), setq_from_values(LEnv31, [sys_update_flag, sys_new_value]), get_var(LEnv31, sys_update_flag, IFTEST34), (IFTEST34\==[]->get_var(LEnv31, sys_i, I_Get39), 'f_1-'(I_Get39, Nthcdr_Param119), get_var(LEnv31, cons, Cons_Get40), f_nthcdr(Nthcdr_Param119, Cons_Get40, Rplacd_Param120), get_var(LEnv31, sys_new_value, Get_var_Ret131), f_rplacd(Rplacd_Param120, Get_var_Ret131, Rplacd_Ret), LetResult30=Rplacd_Ret;LetResult30=[]), throw(block_exit([], LetResult30)), _8964=ThrowResult;LEnv46=[bv(sys_update_flag, []), bv(sys_new_value, [])|BlockExitEnv], get_var(LEnv46, sys_i, I_Get47), f_format([[], '$ARRAY'([*], claz_base_character, "nth ~D:"), I_Get47], Inspect_command_Param121), get_var(LEnv46, sys_l, L_Get48), f_car(L_Get48, Car_Ret133), f_sys_read_inspect_command(Inspect_command_Param121, Car_Ret133, t, Inspect_command_Ret134), setq_from_values(LEnv46, [sys_update_flag, sys_new_value]), get_var(LEnv46, sys_update_flag, IFTEST49), (IFTEST49\==[]->get_var(LEnv46, cons, Cons_Get56), get_var(LEnv46, sys_i, I_Get55), get_var(LEnv46, sys_new_value, New_value_Get52), set_place(LEnv46, setf, [nth, I_Get55, Cons_Get56], [New_value_Get52], Setf_R), LetResult45=Setf_R;LetResult45=[]), get_var(BlockExitEnv, sys_i, I_Get58), 'f_1+'(I_Get58, Set_var_Ret), get_var(BlockExitEnv, sys_l, L_Get59), f_cdr(L_Get59, Cdr_Ret), set_var(BlockExitEnv, sys_i, Set_var_Ret), set_var(BlockExitEnv, sys_l, Cdr_Ret), goto(do_label_29, BlockExitEnv), _8964=_GORES)))
					      ]),
			      []=LetResult
			    ),
			    block_exit([], LetResult),
			    true),
		      _6462=LetResult
		  ;   _6462=[]
		  )
		),
		_6462=FnResult
	      ),
	      block_exit(sys_inspect_cons, FnResult),
	      true).
:- set_opv(sys_inspect_cons, symbol_function, f_sys_inspect_cons),
   DefunResult=sys_inspect_cons.
/*
:- side_effect(assert_lsp(sys_inspect_cons,
			  lambda_def(defun,
				     sys_inspect_cons,
				     f_sys_inspect_cons,
				     [cons],
				     
				     [ 
				       [ format,
					 t,
					 
					 [ case,
					   [car, cons],
					   
					   [ 
					     [ lambda,
					       sys_lambda_block,
					       sys_lambda_closure,
					       sys_lambda_block_closure
					     ],
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - function")
					   ],
					   
					   [ quote,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - constant")
					   ],
					   
					   [ t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - cons")
					   ]
					 ],
					 cons
				       ],
				       
				       [ when,
					 sys_xx_inspect_mode_xx,
					 
					 [ do,
					   
					   [ [sys_i, 0, ['1+', sys_i]],
					     [sys_l, cons, [cdr, sys_l]]
					   ],
					   
					   [ [atom, sys_l],
					     
					     [ sys_inspect_recursively,
					       
					       [ format,
						 [],
						 '$ARRAY'([*],
							  claz_base_character,
							  "nthcdr ~D:"),
						 sys_i
					       ],
					       sys_l,
					       
					       [ cdr,
						 [nthcdr, ['1-', sys_i], cons]
					       ]
					     ]
					   ],
					   
					   [ sys_inspect_recursively,
					     
					     [ format,
					       [],
					       '$ARRAY'([*],
							claz_base_character,
							"nth ~D:"),
					       sys_i
					     ],
					     [car, sys_l],
					     [nth, sys_i, cons]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_cons,
			  arglist_info(sys_inspect_cons,
				       f_sys_inspect_cons,
				       [cons],
				       arginfo{ all:[cons],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[cons],
						opt:0,
						req:[cons],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_cons, init_args(x, f_sys_inspect_cons))).
*/
/*
(defun inspect-string (string)
  (format t (if (simple-string-p string) ""(defun inspect-string (string)\n  (format t (if (simple-string-p string) \"~S - simple string\" \"~S - string\")\n          string)\n  (inspect-print  \"dimension:  ~D\"(array-dimension string 0))\n  (when (array-has-fill-pointer-p string)\n        (inspect-print \"fill pointer:  ~D\"\n                       (fill-pointer string)\n                       (fill-pointer string)))\n  (when *inspect-mode*\n        (dotimes (i (array-dimension string 0))\n                 (inspect-recursively (format nil \"aref ~D:\" i)\n                                      (char string i)\n                                      (char string i)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:9011 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-string',[string],[format,t,[if,['simple-string-p',string],'$STRING'("~S - simple string"),'$STRING'("~S - string")],string],['inspect-print','$STRING'("dimension:  ~D"),['array-dimension',string,0]],[when,['array-has-fill-pointer-p',string],['inspect-print','$STRING'("fill pointer:  ~D"),['fill-pointer',string],['fill-pointer',string]]],[when,'*inspect-mode*',[dotimes,[i,['array-dimension',string,0]],['inspect-recursively',[format,[],'$STRING'("aref ~D:"),i],[char,string,i],[char,string,i]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_string,
					       kw_function,
					       f_sys_inspect_string)).
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"dimension:  ~D"),[array_dimension,string,0]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"dimension:  ~D"),[array_dimension,string,0],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"fill pointer:  ~D"),[fill_pointer,string],[fill_pointer,string]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"fill pointer:  ~D"),[fill_pointer,string],[]],[when,sys_update_flag,[setf,[fill_pointer,string],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv, [], CDR, Compile_each_Ret), append([string|CDR], [CAR27, CAR], Append_Ret), setf_inverse_op(fill_pointer, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv, [], CDR, Compile_each_Ret), append([string|CDR], [CAR27, CAR], Append_Ret), setf_inverse_op(fill_pointer, Inverse_op_Ret)))).
*/
wl:lambda_def(defun, sys_inspect_string, f_sys_inspect_string, [string], [[format, t, [if, [simple_string_p, string], '$ARRAY'([*], claz_base_character, "~S - simple string"), '$ARRAY'([*], claz_base_character, "~S - string")], string], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "dimension:  ~D"), [array_dimension, string, 0]], [when, [array_has_fill_pointer_p, string], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "fill pointer:  ~D"), [fill_pointer, string], [fill_pointer, string]]], [when, sys_xx_inspect_mode_xx, [dotimes, [sys_i, [array_dimension, string, 0]], [sys_inspect_recursively, [format, [], '$ARRAY'([*], claz_base_character, "aref ~D:"), sys_i], [char, string, sys_i], [char, string, sys_i]]]]]).
wl:arglist_info(sys_inspect_string, f_sys_inspect_string, [string], arginfo{all:[string], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[string], opt:0, req:[string], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_string).

/*

### Compiled Function: `SYS::INSPECT-STRING` 
*/
f_sys_inspect_string(String_In, FnResult) :-
	GEnv=[bv(string, String_In)],
	catch(( ( get_var(GEnv, string, String_Get),
		  f_simple_string_p(String_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  CAR='$ARRAY'([*], claz_base_character, "~S - simple string")
		  ;   CAR='$ARRAY'([*], claz_base_character, "~S - string")
		  ),
		  get_var(GEnv, string, String_Get8),
		  f_format([t, CAR, String_Get8], Format_Ret),
		  get_var(GEnv, string, String_Get11),
		  f_array_dimension(String_Get11, 0, Array_dimension_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "dimension:  ~D"),
					     Array_dimension_Ret,
					     [],
					     IFTEST9),
		  (   IFTEST9\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret),
		      f_terpri(TrueResult),
		      _5948=TrueResult
		  ;   _5948=[]
		  ),
		  get_var(GEnv, string, String_Get15),
		  f_array_has_fill_pointer_p(String_Get15, IFTEST13),
		  (   IFTEST13\==[]
		  ->  LEnv=[bv(sys_update_flag, []), bv(sys_new_value, [])|GEnv],
		      get_var(LEnv, string, String_Get19),
		      f_fill_pointer(String_Get19, Fill_pointer_Ret),
		      f_sys_read_inspect_command('$ARRAY'([*],
							  claz_base_character,
							  "fill pointer:  ~D"),
						 Fill_pointer_Ret,
						 [],
						 Inspect_command_Ret),
		      setq_from_values(LEnv, [sys_update_flag, sys_new_value]),
		      get_var(LEnv, sys_update_flag, IFTEST20),
		      (   IFTEST20\==[]
		      ->  get_var(LEnv, string, String_Get26),
			  get_var(LEnv, sys_new_value, New_value_Get),
			  set_place(LEnv,
				    setf,
				    [fill_pointer, String_Get26],
				    [New_value_Get],
				    Setf_R),
			  LetResult=Setf_R
		      ;   LetResult=[]
		      ),
		      _6048=LetResult
		  ;   _6048=[]
		  ),
		  get_var(GEnv, sys_xx_inspect_mode_xx, IFTEST29),
		  (   IFTEST29\==[]
		  ->  sf_dotimes(GEnv,
				 [sys_i, [array_dimension, string, 0]],
				 
				 [ sys_inspect_recursively,
				   
				   [ format,
				     [],
				     '$ARRAY'([*],
					      claz_base_character,
					      "aref ~D:"),
				     sys_i
				   ],
				   [char, string, sys_i],
				   [char, string, sys_i]
				 ],
				 TrueResult32),
		      _5856=TrueResult32
		  ;   _5856=[]
		  )
		),
		_5856=FnResult
	      ),
	      block_exit(sys_inspect_string, FnResult),
	      true).
:- set_opv(sys_inspect_string, symbol_function, f_sys_inspect_string),
   DefunResult=sys_inspect_string.
/*
:- side_effect(assert_lsp(sys_inspect_string,
			  lambda_def(defun,
				     sys_inspect_string,
				     f_sys_inspect_string,
				     [string],
				     
				     [ 
				       [ format,
					 t,
					 
					 [ if,
					   [simple_string_p, string],
					   '$ARRAY'([*],
						    claz_base_character,
						    "~S - simple string"),
					   '$ARRAY'([*],
						    claz_base_character,
						    "~S - string")
					 ],
					 string
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "dimension:  ~D"),
					 [array_dimension, string, 0]
				       ],
				       
				       [ when,
					 [array_has_fill_pointer_p, string],
					 
					 [ sys_inspect_print,
					   '$ARRAY'([*],
						    claz_base_character,
						    "fill pointer:  ~D"),
					   [fill_pointer, string],
					   [fill_pointer, string]
					 ]
				       ],
				       
				       [ when,
					 sys_xx_inspect_mode_xx,
					 
					 [ dotimes,
					   [sys_i, [array_dimension, string, 0]],
					   
					   [ sys_inspect_recursively,
					     
					     [ format,
					       [],
					       '$ARRAY'([*],
							claz_base_character,
							"aref ~D:"),
					       sys_i
					     ],
					     [char, string, sys_i],
					     [char, string, sys_i]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_string,
			  arglist_info(sys_inspect_string,
				       f_sys_inspect_string,
				       [string],
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_string,
			  init_args(x, f_sys_inspect_string))).
*/
/*
(defun inspect-vector (vector)
  (format t (if (simple-vector-p vector) ""(defun inspect-vector (vector)\n  (format t (if (simple-vector-p vector) \"~S - simple vector\" \"~S - vector\")\n          vector)\n  (inspect-print  \"dimension:  ~D\" (array-dimension vector 0))\n  (when (array-has-fill-pointer-p vector)\n        (inspect-print \"fill pointer:  ~D\"\n                       (fill-pointer vector)\n                       (fill-pointer vector)))\n  (when *inspect-mode*\n        (dotimes (i (array-dimension vector 0))\n                 (inspect-recursively (format nil \"aref ~D:\" i)\n                                      (aref vector i)\n                                      (aref vector i)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:9624 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-vector',[vector],[format,t,[if,['simple-vector-p',vector],'$STRING'("~S - simple vector"),'$STRING'("~S - vector")],vector],['inspect-print','$STRING'("dimension:  ~D"),['array-dimension',vector,0]],[when,['array-has-fill-pointer-p',vector],['inspect-print','$STRING'("fill pointer:  ~D"),['fill-pointer',vector],['fill-pointer',vector]]],[when,'*inspect-mode*',[dotimes,[i,['array-dimension',vector,0]],['inspect-recursively',[format,[],'$STRING'("aref ~D:"),i],[aref,vector,i],[aref,vector,i]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_vector,
					       kw_function,
					       f_sys_inspect_vector)).
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"dimension:  ~D"),[array_dimension,vector,0]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"dimension:  ~D"),[array_dimension,vector,0],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"fill pointer:  ~D"),[fill_pointer,vector],[fill_pointer,vector]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"fill pointer:  ~D"),[fill_pointer,vector],[]],[when,sys_update_flag,[setf,[fill_pointer,vector],sys_new_value]]].
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv, [], CDR, Compile_each_Ret), append([vector|CDR], [CAR27, CAR], Append_Ret), setf_inverse_op(fill_pointer, Inverse_op_Ret)))).
*/
/*
:- failure(show_call_trace((compile_each([name='GLOBAL', environ=env_1], LEnv, [], CDR, Compile_each_Ret), append([vector|CDR], [CAR27, CAR], Append_Ret), setf_inverse_op(fill_pointer, Inverse_op_Ret)))).
*/
wl:lambda_def(defun, sys_inspect_vector, f_sys_inspect_vector, [vector], [[format, t, [if, [simple_vector_p, vector], '$ARRAY'([*], claz_base_character, "~S - simple vector"), '$ARRAY'([*], claz_base_character, "~S - vector")], vector], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "dimension:  ~D"), [array_dimension, vector, 0]], [when, [array_has_fill_pointer_p, vector], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "fill pointer:  ~D"), [fill_pointer, vector], [fill_pointer, vector]]], [when, sys_xx_inspect_mode_xx, [dotimes, [sys_i, [array_dimension, vector, 0]], [sys_inspect_recursively, [format, [], '$ARRAY'([*], claz_base_character, "aref ~D:"), sys_i], [aref, vector, sys_i], [aref, vector, sys_i]]]]]).
wl:arglist_info(sys_inspect_vector, f_sys_inspect_vector, [vector], arginfo{all:[vector], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[vector], opt:0, req:[vector], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_vector).

/*

### Compiled Function: `SYS::INSPECT-VECTOR` 
*/
f_sys_inspect_vector(Vector_In, FnResult) :-
	GEnv=[bv(vector, Vector_In)],
	catch(( ( get_var(GEnv, vector, Vector_Get),
		  f_simple_vector_p(Vector_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  CAR='$ARRAY'([*], claz_base_character, "~S - simple vector")
		  ;   CAR='$ARRAY'([*], claz_base_character, "~S - vector")
		  ),
		  get_var(GEnv, vector, Vector_Get8),
		  f_format([t, CAR, Vector_Get8], Format_Ret),
		  get_var(GEnv, vector, Vector_Get11),
		  f_array_dimension(Vector_Get11, 0, Array_dimension_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "dimension:  ~D"),
					     Array_dimension_Ret,
					     [],
					     IFTEST9),
		  (   IFTEST9\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret),
		      f_terpri(TrueResult),
		      _5948=TrueResult
		  ;   _5948=[]
		  ),
		  get_var(GEnv, vector, Vector_Get15),
		  f_array_has_fill_pointer_p(Vector_Get15, IFTEST13),
		  (   IFTEST13\==[]
		  ->  LEnv=[bv(sys_update_flag, []), bv(sys_new_value, [])|GEnv],
		      get_var(LEnv, vector, Vector_Get19),
		      f_fill_pointer(Vector_Get19, Fill_pointer_Ret),
		      f_sys_read_inspect_command('$ARRAY'([*],
							  claz_base_character,
							  "fill pointer:  ~D"),
						 Fill_pointer_Ret,
						 [],
						 Inspect_command_Ret),
		      setq_from_values(LEnv, [sys_update_flag, sys_new_value]),
		      get_var(LEnv, sys_update_flag, IFTEST20),
		      (   IFTEST20\==[]
		      ->  get_var(LEnv, sys_new_value, New_value_Get),
			  get_var(LEnv, vector, Vector_Get26),
			  set_place(LEnv,
				    setf,
				    [fill_pointer, Vector_Get26],
				    [New_value_Get],
				    Setf_R),
			  LetResult=Setf_R
		      ;   LetResult=[]
		      ),
		      _6048=LetResult
		  ;   _6048=[]
		  ),
		  get_var(GEnv, sys_xx_inspect_mode_xx, IFTEST29),
		  (   IFTEST29\==[]
		  ->  sf_dotimes(GEnv,
				 [sys_i, [array_dimension, vector, 0]],
				 
				 [ sys_inspect_recursively,
				   
				   [ format,
				     [],
				     '$ARRAY'([*],
					      claz_base_character,
					      "aref ~D:"),
				     sys_i
				   ],
				   [aref, vector, sys_i],
				   [aref, vector, sys_i]
				 ],
				 TrueResult32),
		      _5856=TrueResult32
		  ;   _5856=[]
		  )
		),
		_5856=FnResult
	      ),
	      block_exit(sys_inspect_vector, FnResult),
	      true).
:- set_opv(sys_inspect_vector, symbol_function, f_sys_inspect_vector),
   DefunResult=sys_inspect_vector.
/*
:- side_effect(assert_lsp(sys_inspect_vector,
			  lambda_def(defun,
				     sys_inspect_vector,
				     f_sys_inspect_vector,
				     [vector],
				     
				     [ 
				       [ format,
					 t,
					 
					 [ if,
					   [simple_vector_p, vector],
					   '$ARRAY'([*],
						    claz_base_character,
						    "~S - simple vector"),
					   '$ARRAY'([*],
						    claz_base_character,
						    "~S - vector")
					 ],
					 vector
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "dimension:  ~D"),
					 [array_dimension, vector, 0]
				       ],
				       
				       [ when,
					 [array_has_fill_pointer_p, vector],
					 
					 [ sys_inspect_print,
					   '$ARRAY'([*],
						    claz_base_character,
						    "fill pointer:  ~D"),
					   [fill_pointer, vector],
					   [fill_pointer, vector]
					 ]
				       ],
				       
				       [ when,
					 sys_xx_inspect_mode_xx,
					 
					 [ dotimes,
					   [sys_i, [array_dimension, vector, 0]],
					   
					   [ sys_inspect_recursively,
					     
					     [ format,
					       [],
					       '$ARRAY'([*],
							claz_base_character,
							"aref ~D:"),
					       sys_i
					     ],
					     [aref, vector, sys_i],
					     [aref, vector, sys_i]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_vector,
			  arglist_info(sys_inspect_vector,
				       f_sys_inspect_vector,
				       [vector],
				       arginfo{ all:[vector],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[vector],
						opt:0,
						req:[vector],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_vector,
			  init_args(x, f_sys_inspect_vector))).
*/
/*
(defun inspect-array (array)
  (format t (if (adjustable-array-p array)
                ""(defun inspect-array (array)\n  (format t (if (adjustable-array-p array)\n                \"~S - adjustable aray\"\n                \"~S - array\")\n          array)\n  (inspect-print \"rank:  ~D\" (array-rank array))\n  (inspect-print \"dimensions:  ~D\" (array-dimensions array))\n  (inspect-print \"total size:  ~D\" (array-total-size array)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:10238 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-array',[array],[format,t,[if,['adjustable-array-p',array],'$STRING'("~S - adjustable aray"),'$STRING'("~S - array")],array],['inspect-print','$STRING'("rank:  ~D"),['array-rank',array]],['inspect-print','$STRING'("dimensions:  ~D"),['array-dimensions',array]],['inspect-print','$STRING'("total size:  ~D"),['array-total-size',array]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_array,
					       kw_function,
					       f_sys_inspect_array)).
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"rank:  ~D"),[array_rank,array]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"rank:  ~D"),[array_rank,array],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"dimensions:  ~D"),[array_dimensions,array]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"dimensions:  ~D"),[array_dimensions,array],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
/*
% macroexpand:-[sys_inspect_print,'$ARRAY'([*],claz_base_character,"total size:  ~D"),[array_total_size,array]].
*/
/*
% into:-[when,[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"total size:  ~D"),[array_total_size,array],[]],[princ,'$ARRAY'([*],claz_base_character,"Not updated.")],[terpri]].
*/
wl:lambda_def(defun, sys_inspect_array, f_sys_inspect_array, [array], [[format, t, [if, [adjustable_array_p, array], '$ARRAY'([*], claz_base_character, "~S - adjustable aray"), '$ARRAY'([*], claz_base_character, "~S - array")], array], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "rank:  ~D"), [array_rank, array]], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "dimensions:  ~D"), [array_dimensions, array]], [sys_inspect_print, '$ARRAY'([*], claz_base_character, "total size:  ~D"), [array_total_size, array]]]).
wl:arglist_info(sys_inspect_array, f_sys_inspect_array, [array], arginfo{all:[array], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[array], opt:0, req:[array], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_array).

/*

### Compiled Function: `SYS::INSPECT-ARRAY` 
*/
f_sys_inspect_array(Array_In, FnResult) :-
	GEnv=[bv(array, Array_In)],
	catch(( ( get_var(GEnv, array, Array_Get),
		  (   get_opv(Array_Get, adjustable, t)
		  ->  CAR='$ARRAY'([*], claz_base_character, "~S - adjustable aray")
		  ;   CAR='$ARRAY'([*], claz_base_character, "~S - array")
		  ),
		  get_var(GEnv, array, Array_Get9),
		  f_format([t, CAR, Array_Get9], Format_Ret),
		  get_var(GEnv, array, Array_Get12),
		  f_array_rank(Array_Get12, Array_rank_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "rank:  ~D"),
					     Array_rank_Ret,
					     [],
					     IFTEST10),
		  (   IFTEST10\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret),
		      f_terpri(TrueResult),
		      _5220=TrueResult
		  ;   _5220=[]
		  ),
		  get_var(GEnv, array, Array_Get16),
		  f_array_dimensions(Array_Get16, Array_dimensions_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "dimensions:  ~D"),
					     Array_dimensions_Ret,
					     [],
					     IFTEST14),
		  (   IFTEST14\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret30),
		      f_terpri(TrueResult17),
		      _5320=TrueResult17
		  ;   _5320=[]
		  ),
		  get_var(GEnv, array, Array_Get20),
		  f_array_total_size(Array_Get20, Total_size_Ret),
		  f_sys_read_inspect_command('$ARRAY'([*],
						      claz_base_character,
						      "total size:  ~D"),
					     Total_size_Ret,
					     [],
					     IFTEST18),
		  (   IFTEST18\==[]
		  ->  f_princ('$ARRAY'([*], claz_base_character, "Not updated."),
			      Princ_Ret32),
		      f_terpri(TrueResult21),
		      _5102=TrueResult21
		  ;   _5102=[]
		  )
		),
		_5102=FnResult
	      ),
	      block_exit(sys_inspect_array, FnResult),
	      true).
:- set_opv(sys_inspect_array, symbol_function, f_sys_inspect_array),
   DefunResult=sys_inspect_array.
/*
:- side_effect(assert_lsp(sys_inspect_array,
			  lambda_def(defun,
				     sys_inspect_array,
				     f_sys_inspect_array,
				     [array],
				     
				     [ 
				       [ format,
					 t,
					 
					 [ if,
					   [adjustable_array_p, array],
					   '$ARRAY'([*],
						    claz_base_character,
						    "~S - adjustable aray"),
					   '$ARRAY'([*],
						    claz_base_character,
						    "~S - array")
					 ],
					 array
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "rank:  ~D"),
					 [array_rank, array]
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "dimensions:  ~D"),
					 [array_dimensions, array]
				       ],
				       
				       [ sys_inspect_print,
					 '$ARRAY'([*],
						  claz_base_character,
						  "total size:  ~D"),
					 [array_total_size, array]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_array,
			  arglist_info(sys_inspect_array,
				       f_sys_inspect_array,
				       [array],
				       arginfo{ all:[array],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[array],
						opt:0,
						req:[array],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_array, init_args(x, f_sys_inspect_array))).
*/
/*
(defun select-ht-N (hashtable)
  (incf *inspect-level*)
  (maphash #'(lambda (key val)
	       (inspect-indent-1)
	       (format t "key  : "(defun select-ht-N (hashtable)\n  (incf *inspect-level*)\n  (maphash #'(lambda (key val)\n\t       (inspect-indent-1)\n\t       (format t \"key  : ~S\" key)\n\t       (inspect-recursively \"value:\" val (gethash key hashtable)))\n\t   hashtable)\n  (decf *inspect-level*))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:10569 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-ht-N',[hashtable],[incf,'*inspect-level*'],[maphash,function([lambda,[key,val],['inspect-indent-1'],[format,t,'$STRING'("key  : ~S"),key],['inspect-recursively','$STRING'("value:"),val,[gethash,key,hashtable]]]),hashtable],[decf,'*inspect-level*']])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_ht_n,
					       kw_function,
					       f_sys_select_ht_n)).
*/
/*
% macroexpand:-[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"value:"),sys_val,[gethash,key,sys_hashtable]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"value:"),sys_val,t],[when,sys_update_flag,[setf,[gethash,key,sys_hashtable],sys_new_value]]].
*/
/*
:- side_effect((compile_each([name='GLOBAL', environ=env_1], LEnv, [sys_hashtable], [sys_hashtable], true), append([key, sys_hashtable], [CAR14, CAR], [key, sys_hashtable, CAR14, CAR]), setf_inverse_op(gethash, sys_puthash))).
*/
/*
:- side_effect((compile_each([name='GLOBAL', environ=env_1], LEnv, [sys_hashtable], [sys_hashtable], true), append([key, sys_hashtable], [CAR14, CAR], [key, sys_hashtable, CAR14, CAR]), setf_inverse_op(gethash, sys_puthash))).
*/
wl:lambda_def(defun, sys_select_ht_n, f_sys_select_ht_n, [sys_hashtable], [[incf, sys_xx_inspect_level_xx], [maphash, function([lambda, [key, sys_val], [sys_inspect_indent_1], [format, t, '$ARRAY'([*], claz_base_character, "key  : ~S"), key], [sys_inspect_recursively, '$ARRAY'([*], claz_base_character, "value:"), sys_val, [gethash, key, sys_hashtable]]]), sys_hashtable], [decf, sys_xx_inspect_level_xx]]).
wl:arglist_info(sys_select_ht_n, f_sys_select_ht_n, [sys_hashtable], arginfo{all:[sys_hashtable], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_hashtable], opt:0, req:[sys_hashtable], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_ht_n).

/*

### Compiled Function: `SYS::SELECT-HT-N` 
*/
f_sys_select_ht_n(Hashtable_In, FnResult) :-
	Decf_Env=[bv(sys_hashtable, Hashtable_In)],
	catch(( ( place_op(Decf_Env,
			   incf,
			   sys_xx_inspect_level_xx,
			   symbol_value,
			   [],
			   Place_op_Ret),
		  get_var(Decf_Env, sys_hashtable, Hashtable_Get23),
		  f_maphash(closure(kw_function,
				    [ClosureEnvironment|Decf_Env],
				    Whole,
				    LetResult,
				    [key, sys_val],
				    (f_sys_inspect_indent_1(Indent_1_Ret), get_var(ClosureEnvironment, key, Key_Get), f_format([t, '$ARRAY'([*], claz_base_character, "key  : ~S"), Key_Get], Format_Ret), LEnv=[bv(sys_update_flag, []), bv(sys_new_value, [])|ClosureEnvironment], get_var(LEnv, sys_val, Val_Get), f_sys_read_inspect_command('$ARRAY'([*], claz_base_character, "value:"), Val_Get, t, T), setq_from_values(LEnv, [sys_update_flag, sys_new_value]), get_var(LEnv, sys_update_flag, IFTEST), (IFTEST\==[]->get_var(LEnv, key, Key_Get15), get_var(LEnv, sys_hashtable, Hashtable_Get), get_var(LEnv, sys_new_value, New_value_Get), f_sys_puthash(Key_Get15, Hashtable_Get, New_value_Get, TrueResult), LetResult=TrueResult;LetResult=[])),
				    
				    [ lambda,
				      [key, sys_val],
				      [sys_inspect_indent_1],
				      
				      [ format,
					t,
					'$ARRAY'([*],
						 claz_base_character,
						 "key  : ~S"),
					key
				      ],
				      
				      [ sys_inspect_recursively,
					'$ARRAY'([*],
						 claz_base_character,
						 "value:"),
					sys_val,
					[gethash, key, sys_hashtable]
				      ]
				    ]),
			    Hashtable_Get23,
			    Maphash_Ret),
		  set_place(Decf_Env,
			    decf,
			    [value, sys_xx_inspect_level_xx],
			    [],
			    Decf_R)
		),
		Decf_R=FnResult
	      ),
	      block_exit(sys_select_ht_n, FnResult),
	      true).
:- set_opv(sys_select_ht_n, symbol_function, f_sys_select_ht_n),
   DefunResult=sys_select_ht_n.
/*
:- side_effect(assert_lsp(sys_select_ht_n,
			  lambda_def(defun,
				     sys_select_ht_n,
				     f_sys_select_ht_n,
				     [sys_hashtable],
				     
				     [ [incf, sys_xx_inspect_level_xx],
				       
				       [ maphash,
					 function(
						  [ lambda,
						    [key, sys_val],
						    [sys_inspect_indent_1],
						    
						    [ format,
						      t,
						      '$ARRAY'([*],
							       claz_base_character,
							       "key  : ~S"),
						      key
						    ],
						    
						    [ sys_inspect_recursively,
						      '$ARRAY'([*],
							       claz_base_character,
							       "value:"),
						      sys_val,
						      
						      [ gethash,
							key,
							sys_hashtable
						      ]
						    ]
						  ]),
					 sys_hashtable
				       ],
				       [decf, sys_xx_inspect_level_xx]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_n,
			  arglist_info(sys_select_ht_n,
				       f_sys_select_ht_n,
				       [sys_hashtable],
				       arginfo{ all:[sys_hashtable],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_hashtable],
						opt:0,
						req:[sys_hashtable],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_n, init_args(x, f_sys_select_ht_n))).
*/
/*
(defun select-ht-L (hashtable)
  (terpri)
  (format t "The keys of the hash table are:"(defun select-ht-L (hashtable)\n  (terpri)\n  (format t \"The keys of the hash table are:~%\")\n  (maphash #'(lambda (key val)\n\t       (declare (ignore val))\n\t       (format t \"  ~S~%\" key))\n\t   hashtable)\n  (terpri))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:10828 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-ht-L',[hashtable],[terpri],[format,t,'$STRING'("The keys of the hash table are:~%")],[maphash,function([lambda,[key,val],[declare,[ignore,val]],[format,t,'$STRING'("  ~S~%"),key]]),hashtable],[terpri]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_ht_l,
					       kw_function,
					       f_sys_select_ht_l)).
*/
wl:lambda_def(defun, sys_select_ht_l, f_sys_select_ht_l, [sys_hashtable], [[terpri], [format, t, '$ARRAY'([*], claz_base_character, "The keys of the hash table are:~%")], [maphash, function([lambda, [key, sys_val], [declare, [ignore, sys_val]], [format, t, '$ARRAY'([*], claz_base_character, "  ~S~%"), key]]), sys_hashtable], [terpri]]).
wl:arglist_info(sys_select_ht_l, f_sys_select_ht_l, [sys_hashtable], arginfo{all:[sys_hashtable], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_hashtable], opt:0, req:[sys_hashtable], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_ht_l).

/*

### Compiled Function: `SYS::SELECT-HT-L` 
*/
f_sys_select_ht_l(Hashtable_In, FnResult) :-
	GEnv=[bv(sys_hashtable, Hashtable_In)],
	catch(( ( f_terpri(Terpri_Ret),
		  f_format(
			   [ t,
			     '$ARRAY'([*],
				      claz_base_character,
				      "The keys of the hash table are:~%")
			   ],
			   Format_Ret),
		  get_var(GEnv, sys_hashtable, Hashtable_Get),
		  f_maphash(closure(kw_function,
				    [ClosureEnvironment|GEnv],
				    Whole,
				    LResult,
				    [key, sys_val],
				    (sf_declare(ClosureEnvironment, [ignore, sys_val], Sf_declare_Ret), get_var(ClosureEnvironment, key, Key_Get), f_format([t, '$ARRAY'([*], claz_base_character, "  ~S~%"), Key_Get], LResult)),
				    
				    [ lambda,
				      [key, sys_val],
				      [declare, [ignore, sys_val]],
				      
				      [ format,
					t,
					'$ARRAY'([*],
						 claz_base_character,
						 "  ~S~%"),
					key
				      ]
				    ]),
			    Hashtable_Get,
			    Maphash_Ret),
		  f_terpri(Terpri_Ret18)
		),
		Terpri_Ret18=FnResult
	      ),
	      block_exit(sys_select_ht_l, FnResult),
	      true).
:- set_opv(sys_select_ht_l, symbol_function, f_sys_select_ht_l),
   DefunResult=sys_select_ht_l.
/*
:- side_effect(assert_lsp(sys_select_ht_l,
			  lambda_def(defun,
				     sys_select_ht_l,
				     f_sys_select_ht_l,
				     [sys_hashtable],
				     
				     [ [terpri],
				       
				       [ format,
					 t,
					 '$ARRAY'([*],
						  claz_base_character,
						  "The keys of the hash table are:~%")
				       ],
				       
				       [ maphash,
					 function(
						  [ lambda,
						    [key, sys_val],
						    [declare, [ignore, sys_val]],
						    
						    [ format,
						      t,
						      '$ARRAY'([*],
							       claz_base_character,
							       "  ~S~%"),
						      key
						    ]
						  ]),
					 sys_hashtable
				       ],
				       [terpri]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_l,
			  arglist_info(sys_select_ht_l,
				       f_sys_select_ht_l,
				       [sys_hashtable],
				       arginfo{ all:[sys_hashtable],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_hashtable],
						opt:0,
						req:[sys_hashtable],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_l, init_args(x, f_sys_select_ht_l))).
*/
/*
(defun select-ht-J (hashtable)
  (let* ((key (prog1
		(read-preserving-whitespace *query-io*)
		(inspect-read-line)))
	 (val (gethash key hashtable)))
        (if val
	    (progn
	      (incf *inspect-level*)
	      (inspect-indent-1)
	      (format t "key  : "(defun select-ht-J (hashtable)\n  (let* ((key (prog1\n\t\t(read-preserving-whitespace *query-io*)\n\t\t(inspect-read-line)))\n\t (val (gethash key hashtable)))\n        (if val\n\t    (progn\n\t      (incf *inspect-level*)\n\t      (inspect-indent-1)\n\t      (format t \"key  : ~S\" key)\n\t      (inspect-recursively \"value:\" val (gethash key hashtable))\n\t      (decf *inspect-level*))\n\t    (progn\n\t      (terpri)\n\t      (format t \"The key ~S is not present or the value associated is NIL.\" key)\n\t      (terpri)\n\t      (terpri)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:11042 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-ht-J',[hashtable],['let*',[[key,[prog1,['read-preserving-whitespace','*query-io*'],['inspect-read-line']]],[val,[gethash,key,hashtable]]],[if,val,[progn,[incf,'*inspect-level*'],['inspect-indent-1'],[format,t,'$STRING'("key  : ~S"),key],['inspect-recursively','$STRING'("value:"),val,[gethash,key,hashtable]],[decf,'*inspect-level*']],[progn,[terpri],[format,t,'$STRING'("The key ~S is not present or the value associated is NIL."),key],[terpri],[terpri]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_ht_j,
					       kw_function,
					       f_sys_select_ht_j)).
*/
/*
% macroexpand:-[sys_inspect_recursively,'$ARRAY'([*],claz_base_character,"value:"),sys_val,[gethash,key,sys_hashtable]].
*/
/*
% into:-[multiple_value_bind,[sys_update_flag,sys_new_value],[sys_read_inspect_command,'$ARRAY'([*],claz_base_character,"value:"),sys_val,t],[when,sys_update_flag,[setf,[gethash,key,sys_hashtable],sys_new_value]]].
*/
/*
:- side_effect((compile_each([name='GLOBAL', environ=env_1], LEnv22, [sys_hashtable], [sys_hashtable], true), append([key, sys_hashtable], [CAR28, CAR], [key, sys_hashtable, CAR28, CAR]), setf_inverse_op(gethash, sys_puthash))).
*/
/*
:- side_effect((compile_each([name='GLOBAL', environ=env_1], LEnv22, [sys_hashtable], [sys_hashtable], true), append([key, sys_hashtable], [CAR28, CAR], [key, sys_hashtable, CAR28, CAR]), setf_inverse_op(gethash, sys_puthash))).
*/
wl:lambda_def(defun, sys_select_ht_j, f_sys_select_ht_j, [sys_hashtable], [[let_xx, [[key, [prog1, [read_preserving_whitespace, xx_query_io_xx], [sys_inspect_read_line]]], [sys_val, [gethash, key, sys_hashtable]]], [if, sys_val, [progn, [incf, sys_xx_inspect_level_xx], [sys_inspect_indent_1], [format, t, '$ARRAY'([*], claz_base_character, "key  : ~S"), key], [sys_inspect_recursively, '$ARRAY'([*], claz_base_character, "value:"), sys_val, [gethash, key, sys_hashtable]], [decf, sys_xx_inspect_level_xx]], [progn, [terpri], [format, t, '$ARRAY'([*], claz_base_character, "The key ~S is not present or the value associated is NIL."), key], [terpri], [terpri]]]]]).
wl:arglist_info(sys_select_ht_j, f_sys_select_ht_j, [sys_hashtable], arginfo{all:[sys_hashtable], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_hashtable], opt:0, req:[sys_hashtable], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_ht_j).

/*

### Compiled Function: `SYS::SELECT-HT-J` 
*/
f_sys_select_ht_j(Hashtable_In, FnResult) :-
	GEnv=[bv(sys_hashtable, Hashtable_In)],
	catch(( ( get_var(GEnv, xx_query_io_xx, Xx_query_io_xx_Get),
		  f_read_preserving_whitespace(Xx_query_io_xx_Get, Key_Init),
		  f_sys_inspect_read_line(Read_line_Ret),
		  LEnv=[bv(key, Key_Init)|GEnv],
		  get_var(LEnv, key, Key_Get),
		  get_var(LEnv, sys_hashtable, Hashtable_Get),
		  f_gethash(Key_Get, Hashtable_Get, Val_Init),
		  LEnv12=[bv(sys_val, Val_Init)|LEnv],
		  get_var(LEnv12, sys_val, IFTEST),
		  (   IFTEST\==[]
		  ->  place_op(LEnv12,
			       incf,
			       sys_xx_inspect_level_xx,
			       symbol_value,
			       [],
			       Place_op_Ret),
		      f_sys_inspect_indent_1(Indent_1_Ret),
		      get_var(LEnv12, key, Key_Get19),
		      f_format(
			       [ t,
				 '$ARRAY'([*], claz_base_character, "key  : ~S"),
				 Key_Get19
			       ],
			       Format_Ret),
		      LEnv22=[bv(sys_update_flag, []), bv(sys_new_value, [])|LEnv12],
		      get_var(LEnv22, sys_val, Val_Get23),
		      f_sys_read_inspect_command('$ARRAY'([*],
							  claz_base_character,
							  "value:"),
						 Val_Get23,
						 t,
						 T),
		      setq_from_values(LEnv22, [sys_update_flag, sys_new_value]),
		      get_var(LEnv22, sys_update_flag, IFTEST24),
		      (   IFTEST24\==[]
		      ->  get_var(LEnv22, key, Key_Get29),
			  get_var(LEnv22, sys_hashtable, Hashtable_Get31),
			  get_var(LEnv22, sys_new_value, New_value_Get),
			  f_sys_puthash(Key_Get29,
					Hashtable_Get31,
					New_value_Get,
					TrueResult),
			  LetResult21=TrueResult
		      ;   LetResult21=[]
		      ),
		      set_place(LEnv12,
				decf,
				[value, sys_xx_inspect_level_xx],
				[],
				Decf_R),
		      LetResult11=Decf_R
		  ;   f_terpri(Terpri_Ret),
		      get_var(LEnv12, key, Key_Get35),
		      f_format(
			       [ t,
				 '$ARRAY'([*],
					  claz_base_character,
					  "The key ~S is not present or the value associated is NIL."),
				 Key_Get35
			       ],
			       Format_Ret47),
		      f_terpri(Terpri_Ret48),
		      f_terpri(ElseResult),
		      LetResult11=ElseResult
		  )
		),
		LetResult11=FnResult
	      ),
	      block_exit(sys_select_ht_j, FnResult),
	      true).
:- set_opv(sys_select_ht_j, symbol_function, f_sys_select_ht_j),
   DefunResult=sys_select_ht_j.
/*
:- side_effect(assert_lsp(sys_select_ht_j,
			  lambda_def(defun,
				     sys_select_ht_j,
				     f_sys_select_ht_j,
				     [sys_hashtable],
				     
				     [ 
				       [ let_xx,
					 
					 [ 
					   [ key,
					     
					     [ prog1,
					       
					       [ read_preserving_whitespace,
						 xx_query_io_xx
					       ],
					       [sys_inspect_read_line]
					     ]
					   ],
					   
					   [ sys_val,
					     [gethash, key, sys_hashtable]
					   ]
					 ],
					 
					 [ if,
					   sys_val,
					   
					   [ progn,
					     [incf, sys_xx_inspect_level_xx],
					     [sys_inspect_indent_1],
					     
					     [ format,
					       t,
					       '$ARRAY'([*],
							claz_base_character,
							"key  : ~S"),
					       key
					     ],
					     
					     [ sys_inspect_recursively,
					       '$ARRAY'([*],
							claz_base_character,
							"value:"),
					       sys_val,
					       [gethash, key, sys_hashtable]
					     ],
					     [decf, sys_xx_inspect_level_xx]
					   ],
					   
					   [ progn,
					     [terpri],
					     
					     [ format,
					       t,
					       '$ARRAY'([*],
							claz_base_character,
							"The key ~S is not present or the value associated is NIL."),
					       key
					     ],
					     [terpri],
					     [terpri]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_j,
			  arglist_info(sys_select_ht_j,
				       f_sys_select_ht_j,
				       [sys_hashtable],
				       arginfo{ all:[sys_hashtable],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_hashtable],
						opt:0,
						req:[sys_hashtable],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_j, init_args(x, f_sys_select_ht_j))).
*/
/*
(defun select-ht-? ()
  (terpri)
  (format t
	  "Inspect commands for hash tables:"(defun select-ht-? ()\n  (terpri)\n  (format t\n\t  \"Inspect commands for hash tables:~%~\nn (or N or Newline):  inspects the keys/values of the hashtable (recursively).~%~\ns (or S):             skips the field.~%~\np (or P):             pretty-prints the field.~%~\na (or A):             aborts the inspection of the rest of the fields.~%~\ne (or E) form:        evaluates and prints the form.~%~\nl (or L):             show the keys of the hash table.~%~\nj (or J) key:         inspect the value associated to the key requested.~%~\nq (or Q):             quits the inspection.~%~\n?:                    prints this.~%~%\"\n\t  ))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:11555 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'select-ht-?',[],[terpri],[format,t,'$STRING'("Inspect commands for hash tables:~%~\nn (or N or Newline):  inspects the keys/values of the hashtable (recursively).~%~\ns (or S):             skips the field.~%~\np (or P):             pretty-prints the field.~%~\na (or A):             aborts the inspection of the rest of the fields.~%~\ne (or E) form:        evaluates and prints the form.~%~\nl (or L):             show the keys of the hash table.~%~\nj (or J) key:         inspect the value associated to the key requested.~%~\nq (or Q):             quits the inspection.~%~\n?:                    prints this.~%~%")]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_select_ht_c63,
					       kw_function,
					       f_sys_select_ht_c63)).
*/
wl:lambda_def(defun, sys_select_ht_c63, f_sys_select_ht_c63, [], [[terpri], [format, t, '$ARRAY'([*], claz_base_character, "Inspect commands for hash tables:~%~\nn (or N or Newline):  inspects the keys/values of the hashtable (recursively).~%~\ns (or S):             skips the field.~%~\np (or P):             pretty-prints the field.~%~\na (or A):             aborts the inspection of the rest of the fields.~%~\ne (or E) form:        evaluates and prints the form.~%~\nl (or L):             show the keys of the hash table.~%~\nj (or J) key:         inspect the value associated to the key requested.~%~\nq (or Q):             quits the inspection.~%~\n?:                    prints this.~%~%")]]).
wl:arglist_info(sys_select_ht_c63, f_sys_select_ht_c63, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_select_ht_c63).

/*

### Compiled Function: `SYS::SELECT-HT-?` 
*/
f_sys_select_ht_c63(FnResult) :-
	_9450=[],
	catch(( ( f_terpri(Terpri_Ret),
		  f_format(
			   [ t,
			     '$ARRAY'([*],
				      claz_base_character,
				      "Inspect commands for hash tables:~%~\nn (or N or Newline):  inspects the keys/values of the hashtable (recursively).~%~\ns (or S):             skips the field.~%~\np (or P):             pretty-prints the field.~%~\na (or A):             aborts the inspection of the rest of the fields.~%~\ne (or E) form:        evaluates and prints the form.~%~\nl (or L):             show the keys of the hash table.~%~\nj (or J) key:         inspect the value associated to the key requested.~%~\nq (or Q):             quits the inspection.~%~\n?:                    prints this.~%~%")
			   ],
			   Format_Ret)
		),
		Format_Ret=FnResult
	      ),
	      block_exit(sys_select_ht_c63, FnResult),
	      true).
:- set_opv(sys_select_ht_c63, symbol_function, f_sys_select_ht_c63),
   DefunResult=sys_select_ht_c63.
/*
:- side_effect(assert_lsp(sys_select_ht_c63,
			  lambda_def(defun,
				     sys_select_ht_c63,
				     f_sys_select_ht_c63,
				     [],
				     
				     [ [terpri],
				       
				       [ format,
					 t,
					 '$ARRAY'([*],
						  claz_base_character,
						  "Inspect commands for hash tables:~%~\nn (or N or Newline):  inspects the keys/values of the hashtable (recursively).~%~\ns (or S):             skips the field.~%~\np (or P):             pretty-prints the field.~%~\na (or A):             aborts the inspection of the rest of the fields.~%~\ne (or E) form:        evaluates and prints the form.~%~\nl (or L):             show the keys of the hash table.~%~\nj (or J) key:         inspect the value associated to the key requested.~%~\nq (or Q):             quits the inspection.~%~\n?:                    prints this.~%~%")
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_c63,
			  arglist_info(sys_select_ht_c63,
				       f_sys_select_ht_c63,
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
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_select_ht_c63, init_args(x, f_sys_select_ht_c63))).
*/
/*
(defun inspect-hashtable (hashtable)
  (if *inspect-mode*
      (progn
	(decf *inspect-level*)
        (loop
          (format t ""(defun inspect-hashtable (hashtable)\n  (if *inspect-mode*\n      (progn\n\t(decf *inspect-level*)\n        (loop\n          (format t \"~S - hash table: \" hashtable)\n\t  (force-output)\n          (case (do ((char (read-char *query-io*) (read-char *query-io*)))\n\t            ((and (char/= char #\\Space) (char/= #\\Tab)) char))\n\t        ((#\\Newline #\\Return)\n\t\t (select-ht-N hashtable)\n\t\t (return nil))\n\t        ((#\\n #\\N)\n\t         (inspect-read-line)\n\t\t (select-ht-N hashtable)\n\t\t (return nil))\n\t        ((#\\s #\\S)\n\t         (inspect-read-line)\n\t         (return nil))\n\t\t((#\\p #\\P)\n\t\t (inspect-read-line)\n\t\t (select-P hashtable))\n\t\t((#\\a #\\A)\n\t\t (inspect-read-line)\n\t\t (throw 'ABORT-INSPECT nil))\n\t\t((#\\e #\\E)\n\t\t (select-E))\n\t\t((#\\q #\\Q)\n\t\t (inspect-read-line)\n\t\t (throw 'QUIT-INSPECT nil))\n\t\t((#\\l #\\L)\n\t\t (inspect-read-line)\n\t\t (select-ht-L hashtable))\n\t\t((#\\j #\\J)\n\t\t (select-ht-J hashtable))\n\t\t((#\\?)\n\t\t (inspect-read-line)\n\t\t (select-ht-?)))\n          (inspect-indent)))\n      (progn\n\t(format t \"~S - hash table: \" hashtable)\n\t(maphash #'(lambda (key val)\n\t\t     (inspect-indent-1)\n\t\t     (format t \"key  : ~S\" key)\n\t\t     (inspect-indent-1)\n\t\t     (format t \"value:\")\n\t\t     (inspect-object val))\n\t         hashtable))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:12173 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-hashtable',[hashtable],[if,'*inspect-mode*',[progn,[decf,'*inspect-level*'],[loop,[format,t,'$STRING'("~S - hash table: "),hashtable],['force-output'],[case,[do,[[char,['read-char','*query-io*'],['read-char','*query-io*']]],[[and,['char/=',char,#\(' ')],['char/=',#\(9)]],char]],[[#\(10),#\(13)],['select-ht-N',hashtable],[return,[]]],[[#\(n),#\('N')],['inspect-read-line'],['select-ht-N',hashtable],[return,[]]],[[#\(s),#\('S')],['inspect-read-line'],[return,[]]],[[#\(p),#\('P')],['inspect-read-line'],['select-P',hashtable]],[[#\(a),#\('A')],['inspect-read-line'],[throw,[quote,'ABORT-INSPECT'],[]]],[[#\(e),#\('E')],['select-E']],[[#\(q),#\('Q')],['inspect-read-line'],[throw,[quote,'QUIT-INSPECT'],[]]],[[#\(l),#\('L')],['inspect-read-line'],['select-ht-L',hashtable]],[[#\(j),#\('J')],['select-ht-J',hashtable]],[[#\(?)],['inspect-read-line'],['select-ht-?']]],['inspect-indent']]],[progn,[format,t,'$STRING'("~S - hash table: "),hashtable],[maphash,function([lambda,[key,val],['inspect-indent-1'],[format,t,'$STRING'("key  : ~S"),key],['inspect-indent-1'],[format,t,'$STRING'("value:")],['inspect-object',val]]),hashtable]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_hashtable,
					       kw_function,
					       f_sys_inspect_hashtable)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_object,
					       kw_function,
					       f_sys_inspect_object)).
*/
wl:lambda_def(defun, sys_inspect_hashtable, f_sys_inspect_hashtable, [sys_hashtable], [[if, sys_xx_inspect_mode_xx, [progn, [decf, sys_xx_inspect_level_xx], [loop, [format, t, '$ARRAY'([*], claz_base_character, "~S - hash table: "), sys_hashtable], [force_output], [case, [do, [[char, [read_char, xx_query_io_xx], [read_char, xx_query_io_xx]]], [[and, [char_c47_c61, char|" "], [char_c47_c61|"\t"]], char]], ["\n\r", [sys_select_ht_n, sys_hashtable], [return, []]], ["nN", [sys_inspect_read_line], [sys_select_ht_n, sys_hashtable], [return, []]], ["sS", [sys_inspect_read_line], [return, []]], ["pP", [sys_inspect_read_line], [sys_select_p, sys_hashtable]], ["aA", [sys_inspect_read_line], [throw, [quote, sys_abort_inspect], []]], ["eE", [sys_select_e]], ["qQ", [sys_inspect_read_line], [throw, [quote, sys_quit_inspect], []]], ["lL", [sys_inspect_read_line], [sys_select_ht_l, sys_hashtable]], ["jJ", [sys_select_ht_j, sys_hashtable]], ["?", [sys_inspect_read_line], [sys_select_ht_c63]]], [sys_inspect_indent]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "~S - hash table: "), sys_hashtable], [maphash, function([lambda, [key, sys_val], [sys_inspect_indent_1], [format, t, '$ARRAY'([*], claz_base_character, "key  : ~S"), key], [sys_inspect_indent_1], [format, t, '$ARRAY'([*], claz_base_character, "value:")], [sys_inspect_object, sys_val]]), sys_hashtable]]]]).
wl:arglist_info(sys_inspect_hashtable, f_sys_inspect_hashtable, [sys_hashtable], arginfo{all:[sys_hashtable], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_hashtable], opt:0, req:[sys_hashtable], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_inspect_hashtable).

/*

### Compiled Function: `SYS::INSPECT-HASHTABLE` 
*/
f_sys_inspect_hashtable(Hashtable_In, FnResult) :-
	Decf_Env=[bv(sys_hashtable, Hashtable_In)],
	catch(( ( get_var(Decf_Env, sys_xx_inspect_mode_xx, IFTEST),
		  (   IFTEST\==[]
		  ->  set_place(Decf_Env,
				decf,
				[value, sys_xx_inspect_level_xx],
				[],
				Decf_R),
		      sf_loop(Decf_Env,
			      
			      [ format,
				t,
				'$ARRAY'([*],
					 claz_base_character,
					 "~S - hash table: "),
				sys_hashtable
			      ],
			      [force_output],
			      
			      [ case,
				
				[ do,
				  
				  [ 
				    [ char,
				      [read_char, xx_query_io_xx],
				      [read_char, xx_query_io_xx]
				    ]
				  ],
				  
				  [ 
				    [ and,
				      [char_c47_c61, char|" "],
				      [char_c47_c61|"\t"]
				    ],
				    char
				  ]
				],
				
				[ "\n\r",
				  [sys_select_ht_n, sys_hashtable],
				  [return, []]
				],
				
				[ "nN",
				  [sys_inspect_read_line],
				  [sys_select_ht_n, sys_hashtable],
				  [return, []]
				],
				["sS", [sys_inspect_read_line], [return, []]],
				
				[ "pP",
				  [sys_inspect_read_line],
				  [sys_select_p, sys_hashtable]
				],
				
				[ "aA",
				  [sys_inspect_read_line],
				  [throw, [quote, sys_abort_inspect], []]
				],
				["eE", [sys_select_e]],
				
				[ "qQ",
				  [sys_inspect_read_line],
				  [throw, [quote, sys_quit_inspect], []]
				],
				
				[ "lL",
				  [sys_inspect_read_line],
				  [sys_select_ht_l, sys_hashtable]
				],
				["jJ", [sys_select_ht_j, sys_hashtable]],
				
				[ "?",
				  [sys_inspect_read_line],
				  [sys_select_ht_c63]
				]
			      ],
			      [sys_inspect_indent],
			      TrueResult),
		      _9034=TrueResult
		  ;   get_var(Decf_Env, sys_hashtable, Hashtable_Get),
		      f_format(
			       [ t,
				 '$ARRAY'([*],
					  claz_base_character,
					  "~S - hash table: "),
				 Hashtable_Get
			       ],
			       Format_Ret),
		      get_var(Decf_Env, sys_hashtable, Hashtable_Get17),
		      f_maphash(closure(kw_function,
					[ClosureEnvironment|Decf_Env],
					Whole,
					LResult,
					[key, sys_val],
					(f_sys_inspect_indent_1(Indent_1_Ret), get_var(ClosureEnvironment, key, Key_Get), f_format([t, '$ARRAY'([*], claz_base_character, "key  : ~S"), Key_Get], Format_Ret24), f_sys_inspect_indent_1(Indent_1_Ret25), f_format([t, '$ARRAY'([*], claz_base_character, "value:")], Format_Ret26), get_var(ClosureEnvironment, sys_val, Val_Get), f_sys_inspect_object(Val_Get, LResult)),
					
					[ lambda,
					  [key, sys_val],
					  [sys_inspect_indent_1],
					  
					  [ format,
					    t,
					    '$ARRAY'([*],
						     claz_base_character,
						     "key  : ~S"),
					    key
					  ],
					  [sys_inspect_indent_1],
					  
					  [ format,
					    t,
					    '$ARRAY'([*],
						     claz_base_character,
						     "value:")
					  ],
					  [sys_inspect_object, sys_val]
					]),
				Hashtable_Get17,
				ElseResult),
		      _9034=ElseResult
		  )
		),
		_9034=FnResult
	      ),
	      block_exit(sys_inspect_hashtable, FnResult),
	      true).
:- set_opv(sys_inspect_hashtable, symbol_function, f_sys_inspect_hashtable),
   DefunResult=sys_inspect_hashtable.
/*
:- side_effect(assert_lsp(sys_inspect_hashtable,
			  lambda_def(defun,
				     sys_inspect_hashtable,
				     f_sys_inspect_hashtable,
				     [sys_hashtable],
				     
				     [ 
				       [ if,
					 sys_xx_inspect_mode_xx,
					 
					 [ progn,
					   [decf, sys_xx_inspect_level_xx],
					   
					   [ loop,
					     
					     [ format,
					       t,
					       '$ARRAY'([*],
							claz_base_character,
							"~S - hash table: "),
					       sys_hashtable
					     ],
					     [force_output],
					     
					     [ case,
					       
					       [ do,
						 
						 [ 
						   [ char,
						     
						     [ read_char,
						       xx_query_io_xx
						     ],
						     
						     [ read_char,
						       xx_query_io_xx
						     ]
						   ]
						 ],
						 
						 [ 
						   [ and,
						     [char_c47_c61, char|" "],
						     [char_c47_c61|"\t"]
						   ],
						   char
						 ]
					       ],
					       
					       [ "\n\r",
						 
						 [ sys_select_ht_n,
						   sys_hashtable
						 ],
						 [return, []]
					       ],
					       
					       [ "nN",
						 [sys_inspect_read_line],
						 
						 [ sys_select_ht_n,
						   sys_hashtable
						 ],
						 [return, []]
					       ],
					       
					       [ "sS",
						 [sys_inspect_read_line],
						 [return, []]
					       ],
					       
					       [ "pP",
						 [sys_inspect_read_line],
						 [sys_select_p, sys_hashtable]
					       ],
					       
					       [ "aA",
						 [sys_inspect_read_line],
						 
						 [ throw,
						   [quote, sys_abort_inspect],
						   []
						 ]
					       ],
					       ["eE", [sys_select_e]],
					       
					       [ "qQ",
						 [sys_inspect_read_line],
						 
						 [ throw,
						   [quote, sys_quit_inspect],
						   []
						 ]
					       ],
					       
					       [ "lL",
						 [sys_inspect_read_line],
						 
						 [ sys_select_ht_l,
						   sys_hashtable
						 ]
					       ],
					       
					       [ "jJ",
						 
						 [ sys_select_ht_j,
						   sys_hashtable
						 ]
					       ],
					       
					       [ "?",
						 [sys_inspect_read_line],
						 [sys_select_ht_c63]
					       ]
					     ],
					     [sys_inspect_indent]
					   ]
					 ],
					 
					 [ progn,
					   
					   [ format,
					     t,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S - hash table: "),
					     sys_hashtable
					   ],
					   
					   [ maphash,
					     function(
						      [ lambda,
							[key, sys_val],
							[sys_inspect_indent_1],
							
							[ format,
							  t,
							  '$ARRAY'([*],
								   claz_base_character,
								   "key  : ~S"),
							  key
							],
							[sys_inspect_indent_1],
							
							[ format,
							  t,
							  '$ARRAY'([*],
								   claz_base_character,
								   "value:")
							],
							
							[ sys_inspect_object,
							  sys_val
							]
						      ]),
					     sys_hashtable
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_hashtable,
			  arglist_info(sys_inspect_hashtable,
				       f_sys_inspect_hashtable,
				       [sys_hashtable],
				       arginfo{ all:[sys_hashtable],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_hashtable],
						opt:0,
						req:[sys_hashtable],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_hashtable,
			  init_args(x, f_sys_inspect_hashtable))).
*/
/*
#+CLOS
(defun inspect-instance (instance)
  (if *inspect-mode*
      (clos::inspect-obj instance)
      (clos::describe-object instance)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:13392 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':CLOS'],[defun,'inspect-instance',[instance],[if,'*inspect-mode*',['clos::inspect-obj',instance],['clos::describe-object',instance]]]]))
/*
(defun inspect-object (object &aux (*inspect-level* *inspect-level*))
  (inspect-indent)
  (when (and (not *inspect-mode*)
             (or (> *inspect-level* 5)
                 (member object *inspect-history*)))
        (prin1 object)
        (return-from inspect-object))
  (incf *inspect-level*)
  (push object *inspect-history*)
  (catch 'ABORT-INSPECT
         (cond
	       #+LOCATIVE
               ((not (sys:sl-boundp object)) nil)
	       ((symbolp object) (inspect-symbol object))
               ((packagep object) (inspect-package object))
               ((characterp object) (inspect-character object))
               ((numberp object) (inspect-number object))
               ((consp object) (inspect-cons object))
               ((stringp object) (inspect-string object))
               ((vectorp object) (inspect-vector object))
               ((arrayp object) (inspect-array object))
               ((hash-table-p object) (inspect-hashtable object))
	       #+clos
	       ((sys:instancep object) (inspect-instance object))
	       #+LOCATIVE
	       ((sys:locativep object) (inspect-locative object))
               (t (format t ""(defun inspect-object (object &aux (*inspect-level* *inspect-level*))\n  (inspect-indent)\n  (when (and (not *inspect-mode*)\n             (or (> *inspect-level* 5)\n                 (member object *inspect-history*)))\n        (prin1 object)\n        (return-from inspect-object))\n  (incf *inspect-level*)\n  (push object *inspect-history*)\n  (catch 'ABORT-INSPECT\n         (cond\n\t       #+LOCATIVE\n               ((not (sys:sl-boundp object)) nil)\n\t       ((symbolp object) (inspect-symbol object))\n               ((packagep object) (inspect-package object))\n               ((characterp object) (inspect-character object))\n               ((numberp object) (inspect-number object))\n               ((consp object) (inspect-cons object))\n               ((stringp object) (inspect-string object))\n               ((vectorp object) (inspect-vector object))\n               ((arrayp object) (inspect-array object))\n               ((hash-table-p object) (inspect-hashtable object))\n\t       #+clos\n\t       ((sys:instancep object) (inspect-instance object))\n\t       #+LOCATIVE\n\t       ((sys:locativep object) (inspect-locative object))\n               (t (format t \"~S - ~S\" object (type-of object))))))\n\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:13532 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'inspect-object',[object,'&aux',['*inspect-level*','*inspect-level*']],['inspect-indent'],[when,[and,[not,'*inspect-mode*'],[or,[>,'*inspect-level*',5],[member,object,'*inspect-history*']]],[prin1,object],['return-from','inspect-object']],[incf,'*inspect-level*'],[push,object,'*inspect-history*'],[catch,[quote,'ABORT-INSPECT'],[cond,[[symbolp,object],['inspect-symbol',object]],[[packagep,object],['inspect-package',object]],[[characterp,object],['inspect-character',object]],[[numberp,object],['inspect-number',object]],[[consp,object],['inspect-cons',object]],[[stringp,object],['inspect-string',object]],[[vectorp,object],['inspect-vector',object]],[[arrayp,object],['inspect-array',object]],[['hash-table-p',object],['inspect-hashtable',object]],[t,[format,t,'$STRING'("~S - ~S"),object,['type-of',object]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_inspect_object,
					       kw_function,
					       f_sys_inspect_object)).
*/
wl:lambda_def(defun, sys_inspect_object, f_sys_inspect_object, [object, c38_aux, [sys_xx_inspect_level_xx, sys_xx_inspect_level_xx]], [[sys_inspect_indent], [when, [and, [not, sys_xx_inspect_mode_xx], [or, [>, sys_xx_inspect_level_xx, 5], [member, object, sys_xx_inspect_history_xx]]], [prin1, object], [return_from, sys_inspect_object]], [incf, sys_xx_inspect_level_xx], [push, object, sys_xx_inspect_history_xx], [catch, [quote, sys_abort_inspect], [cond, [[symbolp, object], [sys_inspect_symbol, object]], [[packagep, object], [sys_inspect_package, object]], [[characterp, object], [sys_inspect_character, object]], [[numberp, object], [sys_inspect_number, object]], [[consp, object], [sys_inspect_cons, object]], [[stringp, object], [sys_inspect_string, object]], [[vectorp, object], [sys_inspect_vector, object]], [[arrayp, object], [sys_inspect_array, object]], [[hash_table_p, object], [sys_inspect_hashtable, object]], [t, [format, t, '$ARRAY'([*], claz_base_character, "~S - ~S"), object, [type_of, object]]]]]]).
wl:arglist_info(sys_inspect_object, f_sys_inspect_object, [object, c38_aux, [sys_xx_inspect_level_xx, sys_xx_inspect_level_xx]], arginfo{all:[object], allow_other_keys:0, aux:[sys_xx_inspect_level_xx], body:0, complex:0, env:0, key:0, names:[object, sys_xx_inspect_level_xx], opt:0, req:[object], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_sys_inspect_object).

/*

### Compiled Function: `SYS::INSPECT-OBJECT` 
*/
f_sys_inspect_object(Object_In, RestNKeys, FnResult) :-
	BlockExitEnv=[bv(object, Object_In), bv(sys_xx_inspect_level_xx, Xx_inspect_level_xx_In)],
	aux_var(Env,
		sys_xx_inspect_level_xx,
		Xx_inspect_level_xx_In,
		get_var(Get_var_Param,
			sys_xx_inspect_level_xx,
			Xx_inspect_level_xx_Get),
		Xx_inspect_level_xx_Get),
	catch(( ( f_sys_inspect_indent(Inspect_indent_Ret),
		  get_var(BlockExitEnv,
			  sys_xx_inspect_mode_xx,
			  Xx_inspect_mode_xx_Get),
		  (   Xx_inspect_mode_xx_Get==[]
		  ->  (   get_var(BlockExitEnv,
				  sys_xx_inspect_level_xx,
				  Xx_inspect_level_xx_Get13),
			  'f_>'(Xx_inspect_level_xx_Get13, 5, FORM1_Res),
			  FORM1_Res\==[],
			  TrueResult=FORM1_Res
		      ->  true
		      ;   get_var(BlockExitEnv, object, Object_Get),
			  get_var(BlockExitEnv,
				  sys_xx_inspect_history_xx,
				  Xx_inspect_history_xx_Get),
			  f_member(Object_Get,
				   Xx_inspect_history_xx_Get,
				   [],
				   Member_Ret),
			  TrueResult=Member_Ret
		      ),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(BlockExitEnv, object, Object_Get18),
		      f_prin1(Object_Get18, Prin1_Ret),
		      set_var(BlockExitEnv, block_ret_sys_inspect_object, []),
		      always(block_exit_sys_inspect_object, BlockExitEnv)
		  ;   _7024=[]
		  ),
		  place_op(BlockExitEnv,
			   incf,
			   sys_xx_inspect_level_xx,
			   symbol_value,
			   [],
			   Place_op_Ret),
		  sf_push(BlockExitEnv,
			  object,
			  sys_xx_inspect_history_xx,
			  Xx_inspect_history_xx),
		  sf_catch(BlockExitEnv,
			   [quote, sys_abort_inspect],
			   
			   [ cond,
			     [[symbolp, object], [sys_inspect_symbol, object]],
			     [[packagep, object], [sys_inspect_package, object]],
			     
			     [ [characterp, object],
			       [sys_inspect_character, object]
			     ],
			     [[numberp, object], [sys_inspect_number, object]],
			     [[consp, object], [sys_inspect_cons, object]],
			     [[stringp, object], [sys_inspect_string, object]],
			     [[vectorp, object], [sys_inspect_vector, object]],
			     [[arrayp, object], [sys_inspect_array, object]],
			     
			     [ [hash_table_p, object],
			       [sys_inspect_hashtable, object]
			     ],
			     
			     [ t,
			       
			       [ format,
				 t,
				 '$ARRAY'([*], claz_base_character, "~S - ~S"),
				 object,
				 [type_of, object]
			       ]
			     ]
			   ],
			   Sf_catch_Ret)
		),
		Sf_catch_Ret=FnResult
	      ),
	      block_exit(sys_inspect_object, FnResult),
	      true).
:- set_opv(sys_inspect_object, symbol_function, f_sys_inspect_object),
   DefunResult=sys_inspect_object.
/*
:- side_effect(assert_lsp(sys_inspect_object,
			  lambda_def(defun,
				     sys_inspect_object,
				     f_sys_inspect_object,
				     
				     [ object,
				       c38_aux,
				       
				       [ sys_xx_inspect_level_xx,
					 sys_xx_inspect_level_xx
				       ]
				     ],
				     
				     [ [sys_inspect_indent],
				       
				       [ when,
					 
					 [ and,
					   [not, sys_xx_inspect_mode_xx],
					   
					   [ or,
					     [>, sys_xx_inspect_level_xx, 5],
					     
					     [ member,
					       object,
					       sys_xx_inspect_history_xx
					     ]
					   ]
					 ],
					 [prin1, object],
					 [return_from, sys_inspect_object]
				       ],
				       [incf, sys_xx_inspect_level_xx],
				       [push, object, sys_xx_inspect_history_xx],
				       
				       [ catch,
					 [quote, sys_abort_inspect],
					 
					 [ cond,
					   
					   [ [symbolp, object],
					     [sys_inspect_symbol, object]
					   ],
					   
					   [ [packagep, object],
					     [sys_inspect_package, object]
					   ],
					   
					   [ [characterp, object],
					     [sys_inspect_character, object]
					   ],
					   
					   [ [numberp, object],
					     [sys_inspect_number, object]
					   ],
					   
					   [ [consp, object],
					     [sys_inspect_cons, object]
					   ],
					   
					   [ [stringp, object],
					     [sys_inspect_string, object]
					   ],
					   
					   [ [vectorp, object],
					     [sys_inspect_vector, object]
					   ],
					   
					   [ [arrayp, object],
					     [sys_inspect_array, object]
					   ],
					   
					   [ [hash_table_p, object],
					     [sys_inspect_hashtable, object]
					   ],
					   
					   [ t,
					     
					     [ format,
					       t,
					       '$ARRAY'([*],
							claz_base_character,
							"~S - ~S"),
					       object,
					       [type_of, object]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_object,
			  arglist_info(sys_inspect_object,
				       f_sys_inspect_object,
				       
				       [ object,
					 c38_aux,
					 
					 [ sys_xx_inspect_level_xx,
					   sys_xx_inspect_level_xx
					 ]
				       ],
				       arginfo{ all:[object],
						allow_other_keys:0,
						aux:[sys_xx_inspect_level_xx],
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ object,
							sys_xx_inspect_level_xx
						      ],
						opt:0,
						req:[object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_inspect_object,
			  init_args(1, f_sys_inspect_object))).
*/
/*
(defun describe (object &aux (*inspect-mode* nil)
                             (*inspect-level* 0)
                             (*inspect-history* nil)
                             (*print-level* nil)
                             (*print-length* nil))
  "The lisp function DESCRIBE."
  (terpri)
  (catch 'QUIT-INSPECT (inspect-object object))
  (terpri)
  (values))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:14721 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,describe,[object,'&aux',['*inspect-mode*',[]],['*inspect-level*',0],['*inspect-history*',[]],['*print-level*',[]],['*print-length*',[]]],'$STRING'("The lisp function DESCRIBE."),[terpri],[catch,[quote,'QUIT-INSPECT'],['inspect-object',object]],[terpri],[values]])
doc: doc_string(describe, _3846, function, "The lisp function DESCRIBE.").

wl:lambda_def(defun, describe, f_describe, [object, c38_aux, [sys_xx_inspect_mode_xx, []], [sys_xx_inspect_level_xx, 0], [sys_xx_inspect_history_xx, []], [xx_print_level_xx, []], [xx_print_length_xx, []]], [[terpri], [catch, [quote, sys_quit_inspect], [sys_inspect_object, object]], [terpri], [values]]).
wl:arglist_info(describe, f_describe, [object, c38_aux, [sys_xx_inspect_mode_xx, []], [sys_xx_inspect_level_xx, 0], [sys_xx_inspect_history_xx, []], [xx_print_level_xx, []], [xx_print_length_xx, []]], arginfo{all:[object], allow_other_keys:0, aux:[sys_xx_inspect_mode_xx, sys_xx_inspect_level_xx, sys_xx_inspect_history_xx, xx_print_level_xx, xx_print_length_xx], body:0, complex:0, env:0, key:0, names:[object, sys_xx_inspect_mode_xx, sys_xx_inspect_level_xx, sys_xx_inspect_history_xx, xx_print_level_xx, xx_print_length_xx], opt:0, req:[object], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_describe).

/*

### Compiled Function: `CL:DESCRIBE` 
*/
f_describe(Object_In, RestNKeys, FnResult) :-
	Sf_catch_Param=[bv(object, Object_In), bv(sys_xx_inspect_mode_xx, Xx_inspect_mode_xx_In), bv(sys_xx_inspect_level_xx, Xx_inspect_level_xx_In), bv(sys_xx_inspect_history_xx, Xx_inspect_history_xx_In), bv(xx_print_level_xx, Xx_print_level_xx_In), bv(xx_print_length_xx, Xx_print_length_xx_In)],
	aux_var(Env, sys_xx_inspect_mode_xx, Xx_inspect_mode_xx_In, true, []),
	aux_var(Env, sys_xx_inspect_level_xx, Xx_inspect_level_xx_In, true, 0),
	aux_var(Env,
		sys_xx_inspect_history_xx,
		Xx_inspect_history_xx_In,
		true,
		[]),
	aux_var(Env, xx_print_level_xx, Xx_print_level_xx_In, true, []),
	aux_var(Env, xx_print_length_xx, Xx_print_length_xx_In, true, []),
	catch(( ( f_terpri(Terpri_Ret),
		  sf_catch(Sf_catch_Param,
			   [quote, sys_quit_inspect],
			   [sys_inspect_object, object],
			   Sf_catch_Ret),
		  f_terpri(Terpri_Ret15),
		  nb_setval('$mv_return', [])
		),
		[]=FnResult
	      ),
	      block_exit(describe, FnResult),
	      true).
:- set_opv(describe, symbol_function, f_describe),
   DefunResult=describe.
/*
:- side_effect(assert_lsp(describe,
			  doc_string(describe,
				     _3846,
				     function,
				     "The lisp function DESCRIBE."))).
*/
/*
:- side_effect(assert_lsp(describe,
			  lambda_def(defun,
				     describe,
				     f_describe,
				     
				     [ object,
				       c38_aux,
				       [sys_xx_inspect_mode_xx, []],
				       [sys_xx_inspect_level_xx, 0],
				       [sys_xx_inspect_history_xx, []],
				       [xx_print_level_xx, []],
				       [xx_print_length_xx, []]
				     ],
				     
				     [ [terpri],
				       
				       [ catch,
					 [quote, sys_quit_inspect],
					 [sys_inspect_object, object]
				       ],
				       [terpri],
				       [values]
				     ]))).
*/
/*
:- side_effect(assert_lsp(describe,
			  arglist_info(describe,
				       f_describe,
				       
				       [ object,
					 c38_aux,
					 [sys_xx_inspect_mode_xx, []],
					 [sys_xx_inspect_level_xx, 0],
					 [sys_xx_inspect_history_xx, []],
					 [xx_print_level_xx, []],
					 [xx_print_length_xx, []]
				       ],
				       arginfo{ all:[object],
						allow_other_keys:0,
						aux:
						    [ sys_xx_inspect_mode_xx,
						      sys_xx_inspect_level_xx,
						      sys_xx_inspect_history_xx,
						      xx_print_level_xx,
						      xx_print_length_xx
						    ],
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ object,
							sys_xx_inspect_mode_xx,
							sys_xx_inspect_level_xx,
							sys_xx_inspect_history_xx,
							xx_print_level_xx,
							xx_print_length_xx
						      ],
						opt:0,
						req:[object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(describe, init_args(1, f_describe))).
*/
/*
(defun inspect (object &aux (*inspect-mode* t)
                            (*inspect-level* 0)
                            (*inspect-history* nil)
                            (*old-print-level* *print-level*)
                            (*old-print-length* *print-length*)
                            (*print-level* 3)
                            (*print-length* 3))
  "The lisp function INSPECT."
  (read-line)
  (princ "Type ? and a newline for help.")
  (terpri)
  (catch 'QUIT-INSPECT (inspect-object object))
  (terpri)
  (values))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/describe.lsp:15088 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,inspect,[object,'&aux',['*inspect-mode*',t],['*inspect-level*',0],['*inspect-history*',[]],['*old-print-level*','*print-level*'],['*old-print-length*','*print-length*'],['*print-level*',3],['*print-length*',3]],'$STRING'("The lisp function INSPECT."),['read-line'],[princ,'$STRING'("Type ? and a newline for help.")],[terpri],[catch,[quote,'QUIT-INSPECT'],['inspect-object',object]],[terpri],[values]])
doc: doc_string(inspect, _4040, function, "The lisp function INSPECT.").

wl:lambda_def(defun, inspect, f_inspect, [object, c38_aux, [sys_xx_inspect_mode_xx, t], [sys_xx_inspect_level_xx, 0], [sys_xx_inspect_history_xx, []], [sys_xx_old_print_level_xx, xx_print_level_xx], [sys_xx_old_print_length_xx, xx_print_length_xx], [xx_print_level_xx, 3], [xx_print_length_xx, 3]], [[read_line], [princ, '$ARRAY'([*], claz_base_character, "Type ? and a newline for help.")], [terpri], [catch, [quote, sys_quit_inspect], [sys_inspect_object, object]], [terpri], [values]]).
wl:arglist_info(inspect, f_inspect, [object, c38_aux, [sys_xx_inspect_mode_xx, t], [sys_xx_inspect_level_xx, 0], [sys_xx_inspect_history_xx, []], [sys_xx_old_print_level_xx, xx_print_level_xx], [sys_xx_old_print_length_xx, xx_print_length_xx], [xx_print_level_xx, 3], [xx_print_length_xx, 3]], arginfo{all:[object], allow_other_keys:0, aux:[sys_xx_inspect_mode_xx, sys_xx_inspect_level_xx, sys_xx_inspect_history_xx, sys_xx_old_print_level_xx, sys_xx_old_print_length_xx, xx_print_level_xx, xx_print_length_xx], body:0, complex:0, env:0, key:0, names:[object, sys_xx_inspect_mode_xx, sys_xx_inspect_level_xx, sys_xx_inspect_history_xx, sys_xx_old_print_level_xx, sys_xx_old_print_length_xx, xx_print_level_xx, xx_print_length_xx], opt:0, req:[object], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_inspect).

/*

### Compiled Function: `CL:INSPECT` 
*/
f_inspect(Object_In, RestNKeys, FnResult) :-
	Sf_catch_Param=[bv(object, Object_In), bv(sys_xx_inspect_mode_xx, Xx_inspect_mode_xx_In), bv(sys_xx_inspect_level_xx, Xx_inspect_level_xx_In), bv(sys_xx_inspect_history_xx, Xx_inspect_history_xx_In), bv(sys_xx_old_print_level_xx, Xx_old_print_level_xx_In), bv(sys_xx_old_print_length_xx, Xx_old_print_length_xx_In), bv(xx_print_level_xx, Xx_print_level_xx_In), bv(xx_print_length_xx, Xx_print_length_xx_In)],
	aux_var(Env, sys_xx_inspect_mode_xx, Xx_inspect_mode_xx_In, true, t),
	aux_var(Env, sys_xx_inspect_level_xx, Xx_inspect_level_xx_In, true, 0),
	aux_var(Env,
		sys_xx_inspect_history_xx,
		Xx_inspect_history_xx_In,
		true,
		[]),
	aux_var(Env,
		sys_xx_old_print_level_xx,
		Xx_old_print_level_xx_In,
		get_var(Get_var_Param, xx_print_level_xx, Xx_print_level_xx_Get),
		Xx_print_level_xx_Get),
	aux_var(Env,
		sys_xx_old_print_length_xx,
		Xx_old_print_length_xx_In,
		get_var(Get_var_Param17,
			xx_print_length_xx,
			Xx_print_length_xx_Get),
		Xx_print_length_xx_Get),
	aux_var(Env, xx_print_level_xx, Xx_print_level_xx_In, true, 3),
	aux_var(Env, xx_print_length_xx, Xx_print_length_xx_In, true, 3),
	catch(( ( f_read_line(Read_line_Ret),
		  f_princ('$ARRAY'([*],
				   claz_base_character,
				   "Type ? and a newline for help."),
			  Princ_Ret),
		  f_terpri(Terpri_Ret),
		  sf_catch(Sf_catch_Param,
			   [quote, sys_quit_inspect],
			   [sys_inspect_object, object],
			   Sf_catch_Ret),
		  f_terpri(Terpri_Ret23),
		  nb_setval('$mv_return', [])
		),
		[]=FnResult
	      ),
	      block_exit(inspect, FnResult),
	      true).
:- set_opv(inspect, symbol_function, f_inspect),
   DefunResult=inspect.
/*
:- side_effect(assert_lsp(inspect,
			  doc_string(inspect,
				     _4040,
				     function,
				     "The lisp function INSPECT."))).
*/
/*
:- side_effect(assert_lsp(inspect,
			  lambda_def(defun,
				     inspect,
				     f_inspect,
				     
				     [ object,
				       c38_aux,
				       [sys_xx_inspect_mode_xx, t],
				       [sys_xx_inspect_level_xx, 0],
				       [sys_xx_inspect_history_xx, []],
				       
				       [ sys_xx_old_print_level_xx,
					 xx_print_level_xx
				       ],
				       
				       [ sys_xx_old_print_length_xx,
					 xx_print_length_xx
				       ],
				       [xx_print_level_xx, 3],
				       [xx_print_length_xx, 3]
				     ],
				     
				     [ [read_line],
				       
				       [ princ,
					 '$ARRAY'([*],
						  claz_base_character,
						  "Type ? and a newline for help.")
				       ],
				       [terpri],
				       
				       [ catch,
					 [quote, sys_quit_inspect],
					 [sys_inspect_object, object]
				       ],
				       [terpri],
				       [values]
				     ]))).
*/
/*
:- side_effect(assert_lsp(inspect,
			  arglist_info(inspect,
				       f_inspect,
				       
				       [ object,
					 c38_aux,
					 [sys_xx_inspect_mode_xx, t],
					 [sys_xx_inspect_level_xx, 0],
					 [sys_xx_inspect_history_xx, []],
					 
					 [ sys_xx_old_print_level_xx,
					   xx_print_level_xx
					 ],
					 
					 [ sys_xx_old_print_length_xx,
					   xx_print_length_xx
					 ],
					 [xx_print_level_xx, 3],
					 [xx_print_length_xx, 3]
				       ],
				       arginfo{ all:[object],
						allow_other_keys:0,
						aux:
						    [ sys_xx_inspect_mode_xx,
						      sys_xx_inspect_level_xx,
						      sys_xx_inspect_history_xx,
						      sys_xx_old_print_level_xx,
						      sys_xx_old_print_length_xx,
						      xx_print_level_xx,
						      xx_print_length_xx
						    ],
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ object,
							sys_xx_inspect_mode_xx,
							sys_xx_inspect_level_xx,
							sys_xx_inspect_history_xx,
							sys_xx_old_print_level_xx,
							sys_xx_old_print_length_xx,
							xx_print_level_xx,
							xx_print_length_xx
						      ],
						opt:0,
						req:[object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(inspect, init_args(1, f_inspe*/
)).

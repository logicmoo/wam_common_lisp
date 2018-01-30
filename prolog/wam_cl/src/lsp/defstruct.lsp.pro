#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "src/lsp/defstruct.lsp" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/lsp/defstruct.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Mon Jan 29 13:20:45 2018

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
;;;        The structure routines.
*/
/*
(in-package "COMMON-LISP")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/lsp/defstruct.lsp:479 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','$STRING'("COMMON-LISP")])
/*
% macroexpand:-[in_package,'$ARRAY'([*],claz_base_character,"COMMON-LISP")].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"COMMON-LISP")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*],
					 claz_base_character,
					 "COMMON-LISP"),
				_Ignored),
	   _Ignored).
/*
(export 'defstruct)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/lsp/defstruct.lsp:508 **********************/
:-lisp_compile_to_prolog(pkg_cl,[export,[quote,defstruct]])
:- f_export(defstruct, [], _Ignored).
/*
(in-package "SYSTEM")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/lsp/defstruct.lsp:529 **********************/
:-lisp_compile_to_prolog(pkg_cl,['in-package','$STRING'("SYSTEM")])
/*
% macroexpand:-[in_package,'$ARRAY'([*],claz_base_character,"SYSTEM")].
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/lsp/defstruct.lsp:552 **********************/
:-lisp_compile_to_prolog(pkg_sys,[proclaim,[quote,[optimize,[safety,2],[space,3]]]])
:- sf_proclaim(Sf_proclaim_Param,
	       [quote, [optimize, [safety, 2], [space, 3]]],
	       _Ignored).
/*
(defun make-access-function (name conc-name type named slot-descr)
  (declare (ignore named))
  (let* ((slot-name (nth 0 slot-descr))
	 ;; (default-init (nth 1 slot-descr))
	 ;; (slot-type (nth 2 slot-descr))
	 (read-only (nth 3 slot-descr))
	 (offset (nth 4 slot-descr))
	 (access-function (intern (string-concatenate (string conc-name)
							 (string slot-name)))))
    (cond ((null type)
           ;; If TYPE is NIL,
           ;;  the slot is at the offset in the structure-body.
	   (fset access-function #'(lambda (x)
				     (sys:structure-ref x name offset))))
          ((or (eq type 'VECTOR)
               (and (consp type)
                    (eq (car type) 'VECTOR)))
	   ;; If TYPE is VECTOR or (VECTOR ... ), ELT is used.
           (fset access-function
		 #'(lambda (x) (elt x offset))))
          ((eq type 'LIST)
           ;; If TYPE is LIST, NTH is used.
	   (fset access-function
		 #'(lambda (x) (sys:list-nth offset x))))
          (t (error ""(defun make-access-function (name conc-name type named slot-descr)\n  (declare (ignore named))\n  (let* ((slot-name (nth 0 slot-descr))\n\t ;; (default-init (nth 1 slot-descr))\n\t ;; (slot-type (nth 2 slot-descr))\n\t (read-only (nth 3 slot-descr))\n\t (offset (nth 4 slot-descr))\n\t (access-function (intern (string-concatenate (string conc-name)\n\t\t\t\t\t\t\t (string slot-name)))))\n    (cond ((null type)\n           ;; If TYPE is NIL,\n           ;;  the slot is at the offset in the structure-body.\n\t   (fset access-function #'(lambda (x)\n\t\t\t\t     (sys:structure-ref x name offset))))\n          ((or (eq type 'VECTOR)\n               (and (consp type)\n                    (eq (car type) 'VECTOR)))\n\t   ;; If TYPE is VECTOR or (VECTOR ... ), ELT is used.\n           (fset access-function\n\t\t #'(lambda (x) (elt x offset))))\n          ((eq type 'LIST)\n           ;; If TYPE is LIST, NTH is used.\n\t   (fset access-function\n\t\t #'(lambda (x) (sys:list-nth offset x))))\n          (t (error \"~S is an illegal structure type.\" type)))\n    (if read-only\n\t(progn\n\t  (rem-sysprop access-function 'SETF-UPDATE-FN)\n\t  (rem-sysprop access-function 'SETF-LAMBDA)\n\t  (rem-sysprop access-function 'SETF-DOCUMENTATION))\n\t(progn\n\t  ;; The following is used by the compiler to expand inline   ;; the accessor\n\t  (put-sysprop-r access-function (cons (or type name) offset)\n\t\t      'STRUCTURE-ACCESS)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/src/lsp/defstruct.lsp:598 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'make-access-function',[name,'conc-name',type,named,'slot-descr'],[declare,[ignore,named]],['let*',[['slot-name',[nth,0,'slot-descr']],['read-only',[nth,3,'slot-descr']],[offset,[nth,4,'slot-descr']],['access-function',[intern,['string-concatenate',[string,'conc-name'],[string,'slot-name']]]]],[cond,[[null,type],[fset,'access-function',function([lambda,[x],['sys:structure-ref',x,name,offset]])]],[[or,[eq,type,[quote,'VECTOR']],[and,[consp,type],[eq,[car,type],[quote,'VECTOR']]]],[fset,'access-function',function([lambda,[x],[elt,x,offset]])]],[[eq,type,[quote,'LIST']],[fset,'access-function',function([lambda,[x],['sys:list-nth',offset,x]])]],[t,[error,'$STRING'("~S is an illegal structure type."),type]]],[if,'read-only',[progn,['rem-sysprop','access-function',[quote,'SETF-UPDATE-FN']],['rem-sysprop','access-function',[quote,'SETF-LAMBDA']],['rem-sysprop','access-function',[quote,'SETF-DOCUMENTATION']]],[progn,['put-sysprop-r','access-function',[cons,[or,type,name],offset],[quote,'STRUCTURE-ACCESS']]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_make_access_function,
					       kw_function,
					       f_sys_make_access_function)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_string_concatenate,
					       kw_function,
					       f_sys_string_concatenate)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_list_nth,
					       kw_function,
					       f_sys_list_nth)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_rem_sysprop,
					       kw_function,
					       f_sys_rem_sysprop)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_rem_sysprop,
					       kw_function,
					       f_sys_rem_sysprop)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_rem_sysprop,
					       kw_function,
					       f_sys_rem_sysprop)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_put_sysprop_r,
					       kw_function,
					       f_sys_put_sysprop_r)).
*/
wl:lambda_def(defun, sys_make_access_function, f_sys_make_access_function, [sys_name, sys_conc_name, type, sys_named, sys_slot_descr], [[declare, [ignore, sys_named]], [let_xx, [[sys_slot_name, [nth, 0, sys_slot_descr]], [sys_read_only, [nth, 3, sys_slot_descr]], [sys_offset, [nth, 4, sys_slot_descr]], [sys_access_function, [intern, [sys_string_concatenate, [string, sys_conc_name], [string, sys_slot_name]]]]], [cond, [[null, type], [sys_fset, sys_access_function, function([lambda, [sys_x], [sys_structure_ref, sys_x, sys_name, sys_offset]])]], [[or, [eq, type, [quote, vector]], [and, [consp, type], [eq, [car, type], [quote, vector]]]], [sys_fset, sys_access_function, function([lambda, [sys_x], [elt, sys_x, sys_offset]])]], [[eq, type, [quote, list]], [sys_fset, sys_access_function, function([lambda, [sys_x], [sys_list_nth, sys_offset, sys_x]])]], [t, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure type."), type]]], [if, sys_read_only, [progn, [sys_rem_sysprop, sys_access_function, [quote, sys_setf_update_fn]], [sys_rem_sysprop, sys_access_function, [quote, sys_setf_lambda]], [sys_rem_sysprop, sys_access_function, [quote, sys_setf_documentation]]], [progn, [sys_put_sysprop_r, sys_access_function, [cons, [or, type, sys_name], sys_offset], [quote, sys_structure_access]]]]]]).
wl:arglist_info(sys_make_access_function, f_sys_make_access_function, [sys_name, sys_conc_name, type, sys_named, sys_slot_descr], arginfo{all:[sys_name, sys_conc_name, type, sys_named, sys_slot_descr], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_conc_name, type, sys_named, sys_slot_descr], opt:0, req:[sys_name, sys_conc_name, type, sys_named, sys_slot_descr], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_make_access_function).

/*

### Compiled Function: `SYS::MAKE-ACCESS-FUNCTION` 
*/
f_sys_make_access_function(Name_In, Conc_name_In, Type_In, Named_In, Slot_descr_In, FnResult) :-
	GEnv=[bv(sys_name, Name_In), bv(sys_conc_name, Conc_name_In), bv(type, Type_In), bv(sys_named, Named_In), bv(sys_slot_descr, Slot_descr_In)],
	catch(( ( sf_declare(GEnv, [ignore, sys_named], Sf_declare_Ret),
		  get_var(GEnv, sys_slot_descr, Slot_descr_Get),
		  f_nth(0, Slot_descr_Get, Slot_name_Init),
		  LEnv=[bv(sys_slot_name, Slot_name_Init)|GEnv],
		  get_var(LEnv, sys_slot_descr, Slot_descr_Get17),
		  f_nth(3, Slot_descr_Get17, Read_only_Init),
		  LEnv16=[bv(sys_read_only, Read_only_Init)|LEnv],
		  get_var(LEnv16, sys_slot_descr, Slot_descr_Get22),
		  f_nth(4, Slot_descr_Get22, Offset_Init),
		  LEnv21=[bv(sys_offset, Offset_Init)|LEnv16],
		  get_var(LEnv21, sys_conc_name, Conc_name_Get),
		  f_string(Conc_name_Get, String_concatenate_Param),
		  get_var(LEnv21, sys_slot_name, Slot_name_Get),
		  f_string(Slot_name_Get, String_Ret),
		  f_sys_string_concatenate(String_concatenate_Param,
					   String_Ret,
					   Intern_Param),
		  f_intern(Intern_Param, Access_function_Init),
		  LEnv26=[bv(sys_access_function, Access_function_Init)|LEnv21],
		  get_var(LEnv26, type, IFTEST),
		  (   IFTEST==[]
		  ->  get_var(LEnv26, sys_access_function, Access_function_Get),
		      f_sys_fset(Access_function_Get,
				 closure(kw_function,
					 [ClosureEnvironment|LEnv26],
					 Whole,
					 LResult,
					 [sys_x],
					 (get_var(ClosureEnvironment, sys_name, Name_Get), get_var(ClosureEnvironment, sys_offset, Offset_Get), get_var(ClosureEnvironment, sys_x, X_Get), f_sys_structure_ref(X_Get, Name_Get, Offset_Get, LResult)),
					 
					 [ lambda,
					   [sys_x],
					   
					   [ sys_structure_ref,
					     sys_x,
					     sys_name,
					     sys_offset
					   ]
					 ]),
				 TrueResult74),
		      _7404=TrueResult74
		  ;   (   get_var(LEnv26, type, Type_Get43),
			  f_eq(Type_Get43, vector, FORM1_Res),
			  FORM1_Res\==[],
			  IFTEST41=FORM1_Res
		      ->  true
		      ;   get_var(LEnv26, type, Type_Get45),
			  (   c0nz:is_consp(Type_Get45)
			  ->  get_var(LEnv26, type, Type_Get48),
			      f_car(Type_Get48, Eq_Param),
			      f_eq(Eq_Param, vector, TrueResult),
			      _7742=TrueResult
			  ;   _7742=[]
			  ),
			  IFTEST41=_7742
		      ),
		      (   IFTEST41\==[]
		      ->  get_var(LEnv26,
				  sys_access_function,
				  Access_function_Get51),
			  f_sys_fset(Access_function_Get51,
				     closure(kw_function,
					     [ClosureEnvironment56|LEnv26],
					     Whole57,
					     LResult54,
					     [sys_x],
					     (get_var(ClosureEnvironment56, sys_offset, Offset_Get53), get_var(ClosureEnvironment56, sys_x, X_Get52), f_elt(X_Get52, Offset_Get53, LResult54)),
					     
					     [ lambda,
					       [sys_x],
					       [elt, sys_x, sys_offset]
					     ]),
				     TrueResult72),
			  ElseResult75=TrueResult72
		      ;   get_var(LEnv26, type, Type_Get59),
			  (   is_eq(Type_Get59, list)
			  ->  get_var(LEnv26,
				      sys_access_function,
				      Access_function_Get62),
			      f_sys_fset(Access_function_Get62,
					 closure(kw_function,
						 [ClosureEnvironment67|LEnv26],
						 Whole68,
						 LResult65,
						 [sys_x],
						 (get_var(ClosureEnvironment67, sys_offset, Offset_Get63), get_var(ClosureEnvironment67, sys_x, X_Get64), f_sys_list_nth(Offset_Get63, X_Get64, LResult65)),
						 
						 [ lambda,
						   [sys_x],
						   
						   [ sys_list_nth,
						     sys_offset,
						     sys_x
						   ]
						 ]),
					 TrueResult70),
			      ElseResult73=TrueResult70
			  ;   get_var(LEnv26, type, Type_Get69),
			      f_error(
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "~S is an illegal structure type."),
					Type_Get69
				      ],
				      ElseResult),
			      ElseResult73=ElseResult
			  ),
			  ElseResult75=ElseResult73
		      ),
		      _7404=ElseResult75
		  ),
		  get_var(LEnv26, sys_read_only, IFTEST76),
		  (   IFTEST76\==[]
		  ->  get_var(LEnv26,
			      sys_access_function,
			      Access_function_Get79),
		      f_sys_rem_sysprop(Access_function_Get79,
					sys_setf_update_fn,
					Setf_update_fn),
		      get_var(LEnv26,
			      sys_access_function,
			      Access_function_Get80),
		      f_sys_rem_sysprop(Access_function_Get80,
					sys_setf_lambda,
					Setf_lambda),
		      get_var(LEnv26,
			      sys_access_function,
			      Access_function_Get81),
		      f_sys_rem_sysprop(Access_function_Get81,
					sys_setf_documentation,
					TrueResult87),
		      LetResult15=TrueResult87
		  ;   get_var(LEnv26,
			      sys_access_function,
			      Access_function_Get82),
		      (   get_var(LEnv26, type, Type_Get83),
			  Type_Get83\==[],
			  CAR=Type_Get83
		      ->  true
		      ;   get_var(LEnv26, sys_name, Name_Get84),
			  CAR=Name_Get84
		      ),
		      get_var(LEnv26, sys_offset, Offset_Get86),
		      _8830=[CAR|Offset_Get86],
		      f_sys_put_sysprop_r(Access_function_Get82,
					  _8830,
					  sys_structure_access,
					  ElseResult88),
		      LetResult15=ElseResult88
		  )
		),
		LetResult15=FnResult
	      ),
	      block_exit(sys_make_access_function, FnResult),
	      true).
:- set_opv(sys_make_access_function,
	   symbol_function,
	   f_sys_make_access_function),
   DefunResult=sys_make_access_function.
/*
:- side_effect(assert_lsp(sys_make_access_function,
			  lambda_def(defun,
				     sys_make_access_function,
				     f_sys_make_access_function,
				     
				     [ sys_name,
				       sys_conc_name,
				       type,
				       sys_named,
				       sys_slot_descr
				     ],
				     
				     [ [declare, [ignore, sys_named]],
				       
				       [ let_xx,
					 
					 [ 
					   [ sys_slot_name,
					     [nth, 0, sys_slot_descr]
					   ],
					   
					   [ sys_read_only,
					     [nth, 3, sys_slot_descr]
					   ],
					   [sys_offset, [nth, 4, sys_slot_descr]],
					   
					   [ sys_access_function,
					     
					     [ intern,
					       
					       [ sys_string_concatenate,
						 [string, sys_conc_name],
						 [string, sys_slot_name]
					       ]
					     ]
					   ]
					 ],
					 
					 [ cond,
					   
					   [ [null, type],
					     
					     [ sys_fset,
					       sys_access_function,
					       function(
							[ lambda,
							  [sys_x],
							  
							  [ sys_structure_ref,
							    sys_x,
							    sys_name,
							    sys_offset
							  ]
							])
					     ]
					   ],
					   
					   [ 
					     [ or,
					       [eq, type, [quote, vector]],
					       
					       [ and,
						 [consp, type],
						 
						 [ eq,
						   [car, type],
						   [quote, vector]
						 ]
					       ]
					     ],
					     
					     [ sys_fset,
					       sys_access_function,
					       function(
							[ lambda,
							  [sys_x],
							  
							  [ elt,
							    sys_x,
							    sys_offset
							  ]
							])
					     ]
					   ],
					   
					   [ [eq, type, [quote, list]],
					     
					     [ sys_fset,
					       sys_access_function,
					       function(
							[ lambda,
							  [sys_x],
							  
							  [ sys_list_nth,
							    sys_offset,
							    sys_x
							  ]
							])
					     ]
					   ],
					   
					   [ t,
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"~S is an illegal structure type."),
					       type
					     ]
					   ]
					 ],
					 
					 [ if,
					   sys_read_only,
					   
					   [ progn,
					     
					     [ sys_rem_sysprop,
					       sys_access_function,
					       [quote, sys_setf_update_fn]
					     ],
					     
					     [ sys_rem_sysprop,
					       sys_access_function,
					       [quote, sys_setf_lambda]
					     ],
					     
					     [ sys_rem_sysprop,
					       sys_access_function,
					       [quote, sys_setf_documentation]
					     ]
					   ],
					   
					   [ progn,
					     
					     [ sys_put_sysprop_r,
					       sys_access_function,
					       
					       [ cons,
						 [or, type, sys_name],
						 sys_offset
					       ],
					       [quote, sys_structure_access]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_make_access_function,
			  arglist_info(sys_make_access_function,
				       f_sys_make_access_function,
				       
				       [ sys_name,
					 sys_conc_name,
					 type,
					 sys_named,
					 sys_slot_descr
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_conc_name,
						      type,
						      sys_named,
						      sys_slot_descr
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_conc_name,
							type,
							sys_named,
							sys_slot_descr
						      ],
						opt:0,
						req:
						    [ sys_name,
						      sys_conc_name,
						      type,
						      sys_named,
						      sys_slot_descr
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_make_access_function,
			  init_args(x, f_sys_make_access_function))).
*/
/*
; (default-init (nth 1 slot-descr))
*/
/*
; (slot-type (nth 2 slot-descr))
*/
/*
; If TYPE is NIL,
*/
/*
;  the slot is at the offset in the structure-body.
*/
/*
; If TYPE is VECTOR or (VECTOR ... ), ELT is used.
*/
/*
; If TYPE is LIST, NTH is used.
*/
/*
; The following is used by the compiler to expand inline   ;; the accessor
*/

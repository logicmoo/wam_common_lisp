#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/logicmoo/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "lib/lsp/defstruct" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sun Jan 28 04:24:00 2018

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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:479 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','#:lisp'])
/*
% macroexpand:-[in_package,lisp1].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"LISP")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "LISP"),
				_Ignored),
	   _Ignored).
/*
(export 'defstruct)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:501 **********************/
:-lisp_compile_to_prolog(pkg_cl,[export,[quote,defstruct]])
:- f_export(defstruct, _Ignored).
/*
(in-package "SYSTEM")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:522 **********************/
:-lisp_compile_to_prolog(pkg_cl,['in-package','#:system'])
/*
% macroexpand:-[in_package,system1].
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

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:545 **********************/
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
          (t (error ""(defun make-access-function (name conc-name type named slot-descr)\n  (declare (ignore named))\n  (let* ((slot-name (nth 0 slot-descr))\n\t ;; (default-init (nth 1 slot-descr))\n\t ;; (slot-type (nth 2 slot-descr))\n\t (read-only (nth 3 slot-descr))\n\t (offset (nth 4 slot-descr))\n\t (access-function (intern (string-concatenate (string conc-name)\n\t\t\t\t\t\t\t (string slot-name)))))\n    (cond ((null type)\n           ;; If TYPE is NIL,\n           ;;  the slot is at the offset in the structure-body.\n\t   (fset access-function #'(lambda (x)\n\t\t\t\t     (sys:structure-ref x name offset))))\n          ((or (eq type 'VECTOR)\n               (and (consp type)\n                    (eq (car type) 'VECTOR)))\n\t   ;; If TYPE is VECTOR or (VECTOR ... ), ELT is used.\n           (fset access-function\n\t\t #'(lambda (x) (elt x offset))))\n          ((eq type 'LIST)\n           ;; If TYPE is LIST, NTH is used.\n\t   (fset access-function\n\t\t #'(lambda (x) (sys:list-nth offset x))))\n          (t (error \"~S is an illegal structure type.\" type)))\n    (if read-only\n\t(progn\n\t  (rem-sysprop access-function 'SETF-UPDATE-FN)\n\t  (rem-sysprop access-function 'SETF-LAMBDA)\n\t  (rem-sysprop access-function 'SETF-DOCUMENTATION))\n\t(progn\n\t  ;; The following is used by the compiler to expand inline   ;; the accessor\n\t  (put-sysprop-r access-function (cons (or type name) offset)\n\t\t      'STRUCTURE-ACCESS)))))\n  \t\t     \n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:591 **********************/
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
		      _8224=TrueResult74
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
			      _8562=TrueResult
			  ;   _8562=[]
			  ),
			  IFTEST41=_8562
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
		      _8224=ElseResult75
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
		      _9650=[CAR|Offset_Get86],
		      f_sys_put_sysprop_r(Access_function_Get82,
					  _9650,
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
/*
(defun illegal-boa ()
  (error "An illegal BOA constructor."))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:1970 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'illegal-boa',[],[error,'$STRING'("An illegal BOA constructor.")]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
wl:lambda_def(defun, sys_illegal_boa, f_sys_illegal_boa, [], [[error, '$ARRAY'([*], claz_base_character, "An illegal BOA constructor.")]]).
wl:arglist_info(sys_illegal_boa, f_sys_illegal_boa, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_illegal_boa).

/*

### Compiled Function: `SYS::ILLEGAL-BOA` 
*/
f_sys_illegal_boa(FnResult) :-
	_3454=[],
	catch(( f_error(
			[ '$ARRAY'([*],
				   claz_base_character,
				   "An illegal BOA constructor.")
			],
			Error_Ret),
		Error_Ret=FnResult
	      ),
	      block_exit(sys_illegal_boa, FnResult),
	      true).
:- set_opv(sys_illegal_boa, symbol_function, f_sys_illegal_boa),
   DefunResult=sys_illegal_boa.
/*
:- side_effect(assert_lsp(sys_illegal_boa,
			  lambda_def(defun,
				     sys_illegal_boa,
				     f_sys_illegal_boa,
				     [],
				     
				     [ 
				       [ error,
					 '$ARRAY'([*],
						  claz_base_character,
						  "An illegal BOA constructor.")
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_illegal_boa,
			  arglist_info(sys_illegal_boa,
				       f_sys_illegal_boa,
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
:- side_effect(assert_lsp(sys_illegal_boa, init_args(x, f_sys_illegal_boa))).
*/
/*
(defun make-predicate (name type named name-offset)
  (cond ((null type)
	 #'(lambda (x)
	     (structure-subtype-p x name)))
        ((or (eq type 'VECTOR)
             (and (consp type) (eq (car type) 'VECTOR)))
         ;; The name is at the NAME-OFFSET in the vector.
         (unless named (error "The structure should be named."))
	 #'(lambda (x)
	     (and (vectorp x)
		  (> (length x) name-offset)
		  ;; AKCL has (aref (the (vector t) x).)
		  ;; which fails with strings
		  (eq (elt x name-offset) name))))
        ((eq type 'LIST)
         ;; The name is at the NAME-OFFSET in the list.
         (unless named (error "The structure should be named."))
         (if (= name-offset 0)
	     #'(lambda (x)
		 (and (consp x) (eq (car x) name)))
	     #'(lambda (x)
		 (do ((i name-offset (1- i))
		      (y x (cdr y)))
		     ((= i 0) (and (consp y) (eq (car y) name)))
		   (declare (fixnum i))
		   (unless (consp y) (return nil))))))
        ((error ""(defun make-predicate (name type named name-offset)\n  (cond ((null type)\n\t #'(lambda (x)\n\t     (structure-subtype-p x name)))\n        ((or (eq type 'VECTOR)\n             (and (consp type) (eq (car type) 'VECTOR)))\n         ;; The name is at the NAME-OFFSET in the vector.\n         (unless named (error \"The structure should be named.\"))\n\t #'(lambda (x)\n\t     (and (vectorp x)\n\t\t  (> (length x) name-offset)\n\t\t  ;; AKCL has (aref (the (vector t) x).)\n\t\t  ;; which fails with strings\n\t\t  (eq (elt x name-offset) name))))\n        ((eq type 'LIST)\n         ;; The name is at the NAME-OFFSET in the list.\n         (unless named (error \"The structure should be named.\"))\n         (if (= name-offset 0)\n\t     #'(lambda (x)\n\t\t (and (consp x) (eq (car x) name)))\n\t     #'(lambda (x)\n\t\t (do ((i name-offset (1- i))\n\t\t      (y x (cdr y)))\n\t\t     ((= i 0) (and (consp y) (eq (car y) name)))\n\t\t   (declare (fixnum i))\n\t\t   (unless (consp y) (return nil))))))\n        ((error \"~S is an illegal structure type.\"))))\n\n\n;;; PARSE-SLOT-DESCRIPTION parses the given slot-description\n;;;  and returns a list of the form:\n;;;        (slot-name default-init slot-type read-only offset)\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:2035 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'make-predicate',[name,type,named,'name-offset'],[cond,[[null,type],function([lambda,[x],['structure-subtype-p',x,name]])],[[or,[eq,type,[quote,'VECTOR']],[and,[consp,type],[eq,[car,type],[quote,'VECTOR']]]],[unless,named,[error,'$STRING'("The structure should be named.")]],function([lambda,[x],[and,[vectorp,x],[>,[length,x],'name-offset'],[eq,[elt,x,'name-offset'],name]]])],[[eq,type,[quote,'LIST']],[unless,named,[error,'$STRING'("The structure should be named.")]],[if,[=,'name-offset',0],function([lambda,[x],[and,[consp,x],[eq,[car,x],name]]]),function([lambda,[x],[do,[[i,'name-offset',['1-',i]],[y,x,[cdr,y]]],[[=,i,0],[and,[consp,y],[eq,[car,y],name]]],[declare,[fixnum,i]],[unless,[consp,y],[return,[]]]]])]],[[error,'$STRING'("~S is an illegal structure type.")]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_make_predicate,
					       kw_function,
					       f_sys_make_predicate)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_structure_subtype_p,
					       kw_function,
					       f_sys_structure_subtype_p)).
*/
wl:lambda_def(defun, sys_make_predicate, f_sys_make_predicate, [sys_name, type, sys_named, sys_name_offset], [[cond, [[null, type], function([lambda, [sys_x], [sys_structure_subtype_p, sys_x, sys_name]])], [[or, [eq, type, [quote, vector]], [and, [consp, type], [eq, [car, type], [quote, vector]]]], [unless, sys_named, [error, '$ARRAY'([*], claz_base_character, "The structure should be named.")]], function([lambda, [sys_x], [and, [vectorp, sys_x], [>, [length, sys_x], sys_name_offset], [eq, [elt, sys_x, sys_name_offset], sys_name]]])], [[eq, type, [quote, list]], [unless, sys_named, [error, '$ARRAY'([*], claz_base_character, "The structure should be named.")]], [if, [=, sys_name_offset, 0], function([lambda, [sys_x], [and, [consp, sys_x], [eq, [car, sys_x], sys_name]]]), function([lambda, [sys_x], [do, [[sys_i, sys_name_offset, ['1-', sys_i]], [sys_y, sys_x, [cdr, sys_y]]], [[=, sys_i, 0], [and, [consp, sys_y], [eq, [car, sys_y], sys_name]]], [declare, [fixnum, sys_i]], [unless, [consp, sys_y], [return, []]]]])]], [[error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure type.")]]]]).
wl:arglist_info(sys_make_predicate, f_sys_make_predicate, [sys_name, type, sys_named, sys_name_offset], arginfo{all:[sys_name, type, sys_named, sys_name_offset], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, type, sys_named, sys_name_offset], opt:0, req:[sys_name, type, sys_named, sys_name_offset], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_make_predicate).

/*

### Compiled Function: `SYS::MAKE-PREDICATE` 
*/
f_sys_make_predicate(Name_In, Type_In, Named_In, Name_offset_In, FnResult) :-
	GEnv=[bv(sys_name, Name_In), bv(type, Type_In), bv(sys_named, Named_In), bv(sys_name_offset, Name_offset_In)],
	catch(( ( get_var(GEnv, type, IFTEST),
		  (   IFTEST==[]
		  ->  _8760=closure(kw_function, [ClosureEnvironment|GEnv], Whole, LResult, [sys_x],  (get_var(ClosureEnvironment, sys_name, Name_Get), get_var(ClosureEnvironment, sys_x, X_Get), f_sys_structure_subtype_p(X_Get, Name_Get, LResult)), [lambda, [sys_x], [sys_structure_subtype_p, sys_x, sys_name]])
		  ;   (   get_var(GEnv, type, Type_Get19),
			  f_eq(Type_Get19, vector, FORM1_Res),
			  FORM1_Res\==[],
			  IFTEST17=FORM1_Res
		      ->  true
		      ;   get_var(GEnv, type, Type_Get21),
			  (   c0nz:is_consp(Type_Get21)
			  ->  get_var(GEnv, type, Type_Get24),
			      f_car(Type_Get24, Eq_Param),
			      f_eq(Eq_Param, vector, TrueResult),
			      _9048=TrueResult
			  ;   _9048=[]
			  ),
			  IFTEST17=_9048
		      ),
		      (   IFTEST17\==[]
		      ->  get_var(GEnv, sys_named, IFTEST27),
			  (   IFTEST27\==[]
			  ->  _9190=[]
			  ;   f_error(
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "The structure should be named.")
				      ],
				      ElseResult),
			      _9190=ElseResult
			  ),
			  ElseResult146=closure(kw_function, [ClosureEnvironment48|GEnv], Whole49, LResult46, [sys_x],  (get_var(ClosureEnvironment48, sys_x, X_Get32), (aray:is_vectorp(X_Get32)->get_var(ClosureEnvironment48, sys_x, X_Get36), f_length(X_Get36, PredArg1Result), get_var(ClosureEnvironment48, sys_name_offset, Name_offset_Get), (PredArg1Result>Name_offset_Get->get_var(ClosureEnvironment48, sys_name_offset, Name_offset_Get42), get_var(ClosureEnvironment48, sys_x, X_Get41), f_elt(X_Get41, Name_offset_Get42, Eq_Param153), get_var(ClosureEnvironment48, sys_name, Name_Get43), f_eq(Eq_Param153, Name_Get43, TrueResult44), TrueResult45=TrueResult44;TrueResult45=[]), LResult46=TrueResult45;LResult46=[])), [lambda, [sys_x], [and, [vectorp, sys_x], [>, [length, sys_x], sys_name_offset], [eq, [elt, sys_x, sys_name_offset], sys_name]]])
		      ;   get_var(GEnv, type, Type_Get51),
			  (   is_eq(Type_Get51, list)
			  ->  get_var(GEnv, sys_named, IFTEST54),
			      (   IFTEST54\==[]
			      ->  _10032=[]
			      ;   f_error(
					  [ '$ARRAY'([*],
						     claz_base_character,
						     "The structure should be named.")
					  ],
					  ElseResult57),
				  _10032=ElseResult57
			      ),
			      get_var(GEnv, sys_name_offset, Name_offset_Get59),
			      (   Name_offset_Get59=:=0
			      ->  TrueResult143=closure(kw_function, [ClosureEnvironment71|GEnv], Whole72, LResult69, [sys_x],  (get_var(ClosureEnvironment71, sys_x, X_Get63), (c0nz:is_consp(X_Get63)->get_var(ClosureEnvironment71, sys_x, X_Get66), f_car(X_Get66, Eq_Param154), get_var(ClosureEnvironment71, sys_name, Name_Get67), f_eq(Eq_Param154, Name_Get67, TrueResult68), LResult69=TrueResult68;LResult69=[])), [lambda, [sys_x], [and, [consp, sys_x], [eq, [car, sys_x], sys_name]]])
			      ;   TrueResult143=closure(kw_function, [ClosureEnvironment138|GEnv], Whole139, LetResult, [sys_x],  (get_var(ClosureEnvironment138, sys_name_offset, Name_offset_Get76), get_var(ClosureEnvironment138, sys_x, X_Get77), BlockExitEnv=[bv(sys_i, Name_offset_Get76), bv(sys_y, X_Get77)|ClosureEnvironment138], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_1), get_var(BlockExitEnv, sys_i, I_Get110), (I_Get110=:=0->get_var(BlockExitEnv, sys_y, Y_Get116), (c0nz:is_consp(Y_Get116)->get_var(BlockExitEnv, sys_y, Y_Get119), f_car(Y_Get119, Eq_Param155), get_var(BlockExitEnv, sys_name, Name_Get120), f_eq(Eq_Param155, Name_Get120, TrueResult121), RetResult113=TrueResult121;RetResult113=[]), throw(block_exit([], RetResult113)), _TBResult=ThrowResult114;sf_declare(BlockExitEnv, [fixnum, sys_i], Sf_declare_Ret), get_var(BlockExitEnv, sys_y, Y_Get124), (c0nz:is_consp(Y_Get124)->_11730=[];throw(block_exit([], [])), _11730=ThrowResult128), get_var(BlockExitEnv, sys_i, I_Get130), 'f_1-'(I_Get130, I), get_var(BlockExitEnv, sys_y, Y_Get131), f_cdr(Y_Get131, Y), set_var(BlockExitEnv, sys_i, I), set_var(BlockExitEnv, sys_y, Y), goto(do_label_1, BlockExitEnv), _TBResult=_GORES132)), [addr(addr_tagbody_1_do_label_1, do_label_1, '$unused', BlockExitEnv,  (get_var(BlockExitEnv, sys_i, I_Get), (I_Get=:=0->get_var(BlockExitEnv, sys_y, Y_Get), (c0nz:is_consp(Y_Get)->get_var(BlockExitEnv, sys_y, Y_Get91), f_car(Y_Get91, Eq_Param156), get_var(BlockExitEnv, sys_name, Name_Get92), f_eq(Eq_Param156, Name_Get92, TrueResult93), Block_exit_Ret=TrueResult93;Block_exit_Ret=[]), throw(block_exit([], Block_exit_Ret)), _12106=ThrowResult;sf_declare(BlockExitEnv, [fixnum, sys_i], Sf_declare_Ret159), get_var(BlockExitEnv, sys_y, Y_Get96), (c0nz:is_consp(Y_Get96)->_12138=[];throw(block_exit([], [])), _12138=ThrowResult100), get_var(BlockExitEnv, sys_i, I_Get102), 'f_1-'(I_Get102, Set_var_Ret), get_var(BlockExitEnv, sys_y, Y_Get103), f_cdr(Y_Get103, Cdr_Ret), set_var(BlockExitEnv, sys_i, Set_var_Ret), set_var(BlockExitEnv, sys_y, Cdr_Ret), goto(do_label_1, BlockExitEnv), _12106=_GORES)))]), []=LetResult), block_exit([], LetResult), true)), [lambda, [sys_x], [do, [[sys_i, sys_name_offset, ['1-', sys_i]], [sys_y, sys_x, [cdr, sys_y]]], [[=, sys_i, 0], [and, [consp, sys_y], [eq, [car, sys_y], sys_name]]], [declare, [fixnum, sys_i]], [unless, [consp, sys_y], [return, []]]]])
			      ),
			      ElseResult145=TrueResult143
			  ;   f_error(
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "~S is an illegal structure type.")
				      ],
				      IFTEST140),
			      (   IFTEST140\==[]
			      ->  ElseResult144=[]
			      ;   ElseResult142=[],
				  ElseResult144=ElseResult142
			      ),
			      ElseResult145=ElseResult144
			  ),
			  ElseResult146=ElseResult145
		      ),
		      _8760=ElseResult146
		  )
		),
		_8760=FnResult
	      ),
	      block_exit(sys_make_predicate, FnResult),
	      true).
:- set_opv(sys_make_predicate, symbol_function, f_sys_make_predicate),
   DefunResult=sys_make_predicate.
/*
:- side_effect(assert_lsp(sys_make_predicate,
			  lambda_def(defun,
				     sys_make_predicate,
				     f_sys_make_predicate,
				     [sys_name, type, sys_named, sys_name_offset],
				     
				     [ 
				       [ cond,
					 
					 [ [null, type],
					   function(
						    [ lambda,
						      [sys_x],
						      
						      [ sys_structure_subtype_p,
							sys_x,
							sys_name
						      ]
						    ])
					 ],
					 
					 [ 
					   [ or,
					     [eq, type, [quote, vector]],
					     
					     [ and,
					       [consp, type],
					       [eq, [car, type], [quote, vector]]
					     ]
					   ],
					   
					   [ unless,
					     sys_named,
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"The structure should be named.")
					     ]
					   ],
					   function(
						    [ lambda,
						      [sys_x],
						      
						      [ and,
							[vectorp, sys_x],
							
							[ (>),
							  [length, sys_x],
							  sys_name_offset
							],
							
							[ eq,
							  
							  [ elt,
							    sys_x,
							    sys_name_offset
							  ],
							  sys_name
							]
						      ]
						    ])
					 ],
					 
					 [ [eq, type, [quote, list]],
					   
					   [ unless,
					     sys_named,
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"The structure should be named.")
					     ]
					   ],
					   
					   [ if,
					     [=, sys_name_offset, 0],
					     function(
						      [ lambda,
							[sys_x],
							
							[ and,
							  [consp, sys_x],
							  
							  [ eq,
							    [car, sys_x],
							    sys_name
							  ]
							]
						      ]),
					     function(
						      [ lambda,
							[sys_x],
							
							[ do,
							  
							  [ 
							    [ sys_i,
							      sys_name_offset,
							      ['1-', sys_i]
							    ],
							    
							    [ sys_y,
							      sys_x,
							      [cdr, sys_y]
							    ]
							  ],
							  
							  [ [=, sys_i, 0],
							    
							    [ and,
							      [consp, sys_y],
							      
							      [ eq,
								[car, sys_y],
								sys_name
							      ]
							    ]
							  ],
							  
							  [ declare,
							    [fixnum, sys_i]
							  ],
							  
							  [ unless,
							    [consp, sys_y],
							    [return, []]
							  ]
							]
						      ])
					   ]
					 ],
					 
					 [ 
					   [ error,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S is an illegal structure type.")
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_make_predicate,
			  arglist_info(sys_make_predicate,
				       f_sys_make_predicate,
				       
				       [ sys_name,
					 type,
					 sys_named,
					 sys_name_offset
				       ],
				       arginfo{ all:
						    [ sys_name,
						      type,
						      sys_named,
						      sys_name_offset
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							type,
							sys_named,
							sys_name_offset
						      ],
						opt:0,
						req:
						    [ sys_name,
						      type,
						      sys_named,
						      sys_name_offset
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_make_predicate,
			  init_args(x, f_sys_make_predicate))).
*/
/*
; The name is at the NAME-OFFSET in the vector.
*/
/*
; AKCL has (aref (the (vector t) x).)
*/
/*
; which fails with strings
*/
/*
; The name is at the NAME-OFFSET in the list.
*/
/*
;; PARSE-SLOT-DESCRIPTION parses the given slot-description
*/
/*
;;  and returns a list of the form:
*/
/*
;;        (slot-name default-init slot-type read-only offset)
*/
/*
(defun parse-slot-description (slot-description offset)
  (declare (si::c-local))
  (let* (slot-name default-init slot-type read-only)
    (cond ((atom slot-description)
           (setq slot-name slot-description))
          ((endp (cdr slot-description))
           (setq slot-name (car slot-description)))
          (t
           (setq slot-name (car slot-description))
           (setq default-init (cadr slot-description))
           (do ((os (cddr slot-description) (cddr os)) (o) (v))
               ((endp os))
             (setq o (car os))
             (when (endp (cdr os))
                   (error ""(defun parse-slot-description (slot-description offset)\n  (declare (si::c-local))\n  (let* (slot-name default-init slot-type read-only)\n    (cond ((atom slot-description)\n           (setq slot-name slot-description))\n          ((endp (cdr slot-description))\n           (setq slot-name (car slot-description)))\n          (t\n           (setq slot-name (car slot-description))\n           (setq default-init (cadr slot-description))\n           (do ((os (cddr slot-description) (cddr os)) (o) (v))\n               ((endp os))\n             (setq o (car os))\n             (when (endp (cdr os))\n                   (error \"~S is an illegal structure slot option.\"\n                          os))\n             (setq v (cadr os))\n             (case o\n               (:TYPE (setq slot-type v))\n               (:READ-ONLY (setq read-only v))\n               (t\n                (error \"~S is an illegal structure slot option.\"\n                         os))))))\n    (list slot-name default-init slot-type read-only offset)))\n\n\n;;; OVERWRITE-SLOT-DESCRIPTIONS overwrites the old slot-descriptions\n;;;  with the new descriptions which are specified in the\n;;;  :include defstruct option.\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:3200 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'parse-slot-description',['slot-description',offset],[declare,['si::c-local']],['let*',['slot-name','default-init','slot-type','read-only'],[cond,[[atom,'slot-description'],[setq,'slot-name','slot-description']],[[endp,[cdr,'slot-description']],[setq,'slot-name',[car,'slot-description']]],[t,[setq,'slot-name',[car,'slot-description']],[setq,'default-init',[cadr,'slot-description']],[do,[[os,[cddr,'slot-description'],[cddr,os]],[o],[v]],[[endp,os]],[setq,o,[car,os]],[when,[endp,[cdr,os]],[error,'$STRING'("~S is an illegal structure slot option."),os]],[setq,v,[cadr,os]],[case,o,[':TYPE',[setq,'slot-type',v]],[':READ-ONLY',[setq,'read-only',v]],[t,[error,'$STRING'("~S is an illegal structure slot option."),os]]]]]],[list,'slot-name','default-init','slot-type','read-only',offset]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_parse_slot_description,
					       kw_function,
					       f_sys_parse_slot_description)).
*/
/*
% case:-[[kw_type,[setq,sys_slot_type,sys_v]],[kw_read_only,[setq,sys_read_only,sys_v]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal structure slot option."),sys_os]]].
*/
/*
% conds:-[[[eq,_29578,[quote,kw_type]],[progn,[setq,sys_slot_type,sys_v]]],[[eq,_29578,[quote,kw_read_only]],[progn,[setq,sys_read_only,sys_v]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal structure slot option."),sys_os]]]].
*/
/*
% case:-[[kw_type,[setq,sys_slot_type,sys_v]],[kw_read_only,[setq,sys_read_only,sys_v]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal structure slot option."),sys_os]]].
*/
/*
% conds:-[[[eq,_32110,[quote,kw_type]],[progn,[setq,sys_slot_type,sys_v]]],[[eq,_32110,[quote,kw_read_only]],[progn,[setq,sys_read_only,sys_v]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal structure slot option."),sys_os]]]].
*/
wl:lambda_def(defun, sys_parse_slot_description, f_sys_parse_slot_description, [sys_slot_description, sys_offset], [[declare, [sys_c_local]], [let_xx, [sys_slot_name, sys_default_init, sys_slot_type, sys_read_only], [cond, [[atom, sys_slot_description], [setq, sys_slot_name, sys_slot_description]], [[endp, [cdr, sys_slot_description]], [setq, sys_slot_name, [car, sys_slot_description]]], [t, [setq, sys_slot_name, [car, sys_slot_description]], [setq, sys_default_init, [cadr, sys_slot_description]], [do, [[sys_os, [cddr, sys_slot_description], [cddr, sys_os]], [sys_o], [sys_v]], [[endp, sys_os]], [setq, sys_o, [car, sys_os]], [when, [endp, [cdr, sys_os]], [error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure slot option."), sys_os]], [setq, sys_v, [cadr, sys_os]], [case, sys_o, [kw_type, [setq, sys_slot_type, sys_v]], [kw_read_only, [setq, sys_read_only, sys_v]], [t, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure slot option."), sys_os]]]]]], [list, sys_slot_name, sys_default_init, sys_slot_type, sys_read_only, sys_offset]]]).
wl:arglist_info(sys_parse_slot_description, f_sys_parse_slot_description, [sys_slot_description, sys_offset], arginfo{all:[sys_slot_description, sys_offset], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_slot_description, sys_offset], opt:0, req:[sys_slot_description, sys_offset], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_parse_slot_description).

/*

### Compiled Function: `SYS::PARSE-SLOT-DESCRIPTION` 
*/
f_sys_parse_slot_description(Slot_description_In, Offset_In, FnResult) :-
	Sf_declare_Param=[bv(sys_slot_description, Slot_description_In), bv(sys_offset, Offset_In)],
	catch(( ( sf_declare(Sf_declare_Param, [sys_c_local], Sf_declare_Ret),
		  LEnv=[bv(sys_slot_name, [])|Sf_declare_Param],
		  LEnv11=[bv(sys_default_init, [])|LEnv],
		  LEnv14=[bv(sys_slot_type, [])|LEnv11],
		  LEnv17=[bv(sys_read_only, [])|LEnv14],
		  get_var(LEnv17, sys_slot_description, Slot_description_Get),
		  (   Slot_description_Get\=[CAR|CDR]
		  ->  get_var(LEnv17,
			      sys_slot_description,
			      Slot_description_Get23),
		      set_var(LEnv17, sys_slot_name, Slot_description_Get23),
		      _7472=Slot_description_Get23
		  ;   get_var(LEnv17,
			      sys_slot_description,
			      Slot_description_Get25),
		      f_cdr(Slot_description_Get25, PredArgResult27),
		      (   s3q:is_endp(PredArgResult27)
		      ->  get_var(LEnv17,
				  sys_slot_description,
				  Slot_description_Get28),
			  f_car(Slot_description_Get28, TrueResult108),
			  set_var(LEnv17, sys_slot_name, TrueResult108),
			  ElseResult111=TrueResult108
		      ;   get_var(LEnv17,
				  sys_slot_description,
				  Slot_description_Get29),
			  f_car(Slot_description_Get29, Slot_name),
			  set_var(LEnv17, sys_slot_name, Slot_name),
			  get_var(LEnv17,
				  sys_slot_description,
				  Slot_description_Get30),
			  f_cadr(Slot_description_Get30, Default_init),
			  set_var(LEnv17, sys_default_init, Default_init),
			  get_var(LEnv17,
				  sys_slot_description,
				  Slot_description_Get34),
			  f_cddr(Slot_description_Get34, Os_Init),
			  AEnv=[bv(sys_os, Os_Init), bv([sys_o], []), bv([sys_v], [])|LEnv17],
			  catch(( call_addr_block(AEnv,
						  (push_label(do_label_2), get_var(AEnv, sys_os, Os_Get74), (s3q:is_endp(Os_Get74)->throw(block_exit([], [])), _TBResult=ThrowResult78;get_var(AEnv, sys_os, Os_Get81), f_car(Os_Get81, O), set_var(AEnv, sys_o, O), get_var(AEnv, sys_os, Os_Get83), f_cdr(Os_Get83, PredArgResult85), (s3q:is_endp(PredArgResult85)->get_var(AEnv, sys_os, Os_Get86), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal structure slot option."), Os_Get86], TrueResult87), _8910=TrueResult87;_8910=[]), get_var(AEnv, sys_os, Os_Get88), f_cadr(Os_Get88, V), set_var(AEnv, sys_v, V), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_type)->get_var(AEnv, sys_v, V_Get94), set_var(AEnv, sys_slot_type, V_Get94), _8866=V_Get94;(is_eq(Key, kw_read_only)->get_var(AEnv, sys_v, V_Get97), set_var(AEnv, sys_read_only, V_Get97), ElseResult102=V_Get97;get_var(AEnv, sys_os, Os_Get98), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal structure slot option."), Os_Get98], ElseResult100), ElseResult102=ElseResult100), _8866=ElseResult102), get_var(AEnv, sys_os, Os_Get103), f_cddr(Os_Get103, Os), set_var(AEnv, sys_os, Os), goto(do_label_2, AEnv), _TBResult=_GORES104)),
						  
						  [ addr(addr_tagbody_2_do_label_2,
							 do_label_2,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_os, Os_Get), (s3q:is_endp(Os_Get)->throw(block_exit([], [])), _9528=ThrowResult;get_var(AEnv, sys_os, Os_Get45), f_car(Os_Get45, Car_Ret), set_var(AEnv, sys_o, Car_Ret), get_var(AEnv, sys_os, Os_Get47), f_cdr(Os_Get47, PredArgResult49), (s3q:is_endp(PredArgResult49)->get_var(AEnv, sys_os, Os_Get50), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal structure slot option."), Os_Get50], Error_Ret), _9604=Error_Ret;_9604=[]), get_var(AEnv, sys_os, Os_Get52), f_cadr(Os_Get52, Cadr_Ret), set_var(AEnv, sys_v, Cadr_Ret), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_type)->get_var(AEnv, sys_v, V_Get), set_var(AEnv, sys_slot_type, V_Get), _9650=V_Get;(is_eq(Key, kw_read_only)->get_var(AEnv, sys_v, V_Get61), set_var(AEnv, sys_read_only, V_Get61), ElseResult66=V_Get61;get_var(AEnv, sys_os, Os_Get62), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal structure slot option."), Os_Get62], Error_Ret131), ElseResult66=Error_Ret131), _9650=ElseResult66), get_var(AEnv, sys_os, Os_Get67), f_cddr(Os_Get67, Cddr_Ret), set_var(AEnv, sys_os, Cddr_Ret), goto(do_label_2, AEnv), _9528=_GORES)))
						  ]),
				  []=LetResult32
				),
				block_exit([], LetResult32),
				true),
			  ElseResult111=LetResult32
		      ),
		      _7472=ElseResult111
		  ),
		  get_var(LEnv17, sys_default_init, Default_init_Get),
		  get_var(LEnv17, sys_offset, Offset_Get),
		  get_var(LEnv17, sys_read_only, Read_only_Get),
		  get_var(LEnv17, sys_slot_name, Slot_name_Get),
		  get_var(LEnv17, sys_slot_type, Slot_type_Get),
		  LetResult10=[Slot_name_Get, Default_init_Get, Slot_type_Get, Read_only_Get, Offset_Get]
		),
		LetResult10=FnResult
	      ),
	      block_exit(sys_parse_slot_description, FnResult),
	      true).
:- set_opv(sys_parse_slot_description,
	   symbol_function,
	   f_sys_parse_slot_description),
   DefunResult=sys_parse_slot_description.
/*
:- side_effect(assert_lsp(sys_parse_slot_description,
			  lambda_def(defun,
				     sys_parse_slot_description,
				     f_sys_parse_slot_description,
				     [sys_slot_description, sys_offset],
				     
				     [ [declare, [sys_c_local]],
				       
				       [ let_xx,
					 
					 [ sys_slot_name,
					   sys_default_init,
					   sys_slot_type,
					   sys_read_only
					 ],
					 
					 [ cond,
					   
					   [ [atom, sys_slot_description],
					     
					     [ setq,
					       sys_slot_name,
					       sys_slot_description
					     ]
					   ],
					   
					   [ [endp, [cdr, sys_slot_description]],
					     
					     [ setq,
					       sys_slot_name,
					       [car, sys_slot_description]
					     ]
					   ],
					   
					   [ t,
					     
					     [ setq,
					       sys_slot_name,
					       [car, sys_slot_description]
					     ],
					     
					     [ setq,
					       sys_default_init,
					       [cadr, sys_slot_description]
					     ],
					     
					     [ do,
					       
					       [ 
						 [ sys_os,
						   [cddr, sys_slot_description],
						   [cddr, sys_os]
						 ],
						 [sys_o],
						 [sys_v]
					       ],
					       [[endp, sys_os]],
					       [setq, sys_o, [car, sys_os]],
					       
					       [ when,
						 [endp, [cdr, sys_os]],
						 
						 [ error,
						   '$ARRAY'([*],
							    claz_base_character,
							    "~S is an illegal structure slot option."),
						   sys_os
						 ]
					       ],
					       [setq, sys_v, [cadr, sys_os]],
					       
					       [ case,
						 sys_o,
						 
						 [ kw_type,
						   [setq, sys_slot_type, sys_v]
						 ],
						 
						 [ kw_read_only,
						   [setq, sys_read_only, sys_v]
						 ],
						 
						 [ t,
						   
						   [ error,
						     '$ARRAY'([*],
							      claz_base_character,
							      "~S is an illegal structure slot option."),
						     sys_os
						   ]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ list,
					   sys_slot_name,
					   sys_default_init,
					   sys_slot_type,
					   sys_read_only,
					   sys_offset
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_parse_slot_description,
			  arglist_info(sys_parse_slot_description,
				       f_sys_parse_slot_description,
				       [sys_slot_description, sys_offset],
				       arginfo{ all:
						    [ sys_slot_description,
						      sys_offset
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_slot_description,
							sys_offset
						      ],
						opt:0,
						req:
						    [ sys_slot_description,
						      sys_offset
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_parse_slot_description,
			  init_args(x, f_sys_parse_slot_description))).
*/
/*
;; OVERWRITE-SLOT-DESCRIPTIONS overwrites the old slot-descriptions
*/
/*
;;  with the new descriptions which are specified in the
*/
/*
;;  :include defstruct option.
*/
/*
(defun overwrite-slot-descriptions (news olds)
  (declare (si::c-local))
  (when olds
      (let ((sds (member (caar olds) news :key #'car)))
        (cond (sds
               (when (and (null (cadddr (car sds)))
                          (cadddr (car olds)))
                     ;; If read-only is true in the old
                     ;;  and false in the new, signal an error.
                     (error ""(defun overwrite-slot-descriptions (news olds)\n  (declare (si::c-local))\n  (when olds\n      (let ((sds (member (caar olds) news :key #'car)))\n        (cond (sds\n               (when (and (null (cadddr (car sds)))\n                          (cadddr (car olds)))\n                     ;; If read-only is true in the old\n                     ;;  and false in the new, signal an error.\n                     (error \"~S is an illegal include slot-description.\"\n                            sds))\n               (cons (list (caar sds)\n                           (cadar sds)\n                           (caddar sds)\n                           (cadddr (car sds))\n                           ;; The offset if from the old.\n                           (car (cddddr (car olds))))\n                     (overwrite-slot-descriptions news (cdr olds))))\n              (t\n               (cons (car olds)\n                     (overwrite-slot-descriptions news (cdr olds))))))))\n\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:4368 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'overwrite-slot-descriptions',[news,olds],[declare,['si::c-local']],[when,olds,[let,[[sds,[member,[caar,olds],news,':key',function(car)]]],[cond,[sds,[when,[and,[null,[cadddr,[car,sds]]],[cadddr,[car,olds]]],[error,'$STRING'("~S is an illegal include slot-description."),sds]],[cons,[list,[caar,sds],[cadar,sds],[caddar,sds],[cadddr,[car,sds]],[car,[cddddr,[car,olds]]]],['overwrite-slot-descriptions',news,[cdr,olds]]]],[t,[cons,[car,olds],['overwrite-slot-descriptions',news,[cdr,olds]]]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_overwrite_slot_descriptions,
					       kw_function,
					       f_sys_overwrite_slot_descriptions)).
*/
wl:lambda_def(defun, sys_overwrite_slot_descriptions, f_sys_overwrite_slot_descriptions, [sys_news, sys_olds], [[declare, [sys_c_local]], [when, sys_olds, [let, [[sys_sds, [member, [caar, sys_olds], sys_news, kw_key, function(car)]]], [cond, [sys_sds, [when, [and, [null, [cadddr, [car, sys_sds]]], [cadddr, [car, sys_olds]]], [error, '$ARRAY'([*], claz_base_character, "~S is an illegal include slot-description."), sys_sds]], [cons, [list, [caar, sys_sds], [cadar, sys_sds], [caddar, sys_sds], [cadddr, [car, sys_sds]], [car, [cddddr, [car, sys_olds]]]], [sys_overwrite_slot_descriptions, sys_news, [cdr, sys_olds]]]], [t, [cons, [car, sys_olds], [sys_overwrite_slot_descriptions, sys_news, [cdr, sys_olds]]]]]]]]).
wl:arglist_info(sys_overwrite_slot_descriptions, f_sys_overwrite_slot_descriptions, [sys_news, sys_olds], arginfo{all:[sys_news, sys_olds], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_news, sys_olds], opt:0, req:[sys_news, sys_olds], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_overwrite_slot_descriptions).

/*

### Compiled Function: `SYS::OVERWRITE-SLOT-DESCRIPTIONS` 
*/
f_sys_overwrite_slot_descriptions(News_In, Olds_In, FnResult) :-
	GEnv=[bv(sys_news, News_In), bv(sys_olds, Olds_In)],
	catch(( ( sf_declare(GEnv, [sys_c_local], Sf_declare_Ret),
		  get_var(GEnv, sys_olds, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_olds, Olds_Get12),
		      f_caar(Olds_Get12, Member_Param),
		      get_var(GEnv, sys_news, News_Get),
		      f_member(Member_Param, News_Get, [kw_key, f_car], Sds_Init),
		      LEnv=[bv(sys_sds, Sds_Init)|GEnv],
		      get_var(LEnv, sys_sds, IFTEST15),
		      (   IFTEST15\==[]
		      ->  get_var(LEnv, sys_sds, Sds_Get22),
			  f_car(Sds_Get22, Cadddr_Param),
			  f_cadddr(Cadddr_Param, IFTEST20),
			  (   IFTEST20==[]
			  ->  get_var(LEnv, sys_olds, Olds_Get23),
			      f_car(Olds_Get23, Cadddr_Param45),
			      f_cadddr(Cadddr_Param45, TrueResult),
			      IFTEST18=TrueResult
			  ;   IFTEST18=[]
			  ),
			  (   IFTEST18\==[]
			  ->  get_var(LEnv, sys_sds, Sds_Get25),
			      f_error(
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "~S is an illegal include slot-description."),
					Sds_Get25
				      ],
				      TrueResult26),
			      _6260=TrueResult26
			  ;   _6260=[]
			  ),
			  get_var(LEnv, sys_sds, Sds_Get27),
			  f_caar(Sds_Get27, Caar_Ret),
			  get_var(LEnv, sys_sds, Sds_Get28),
			  f_cadar(Sds_Get28, Cadar_Ret),
			  get_var(LEnv, sys_sds, Sds_Get29),
			  f_caddar(Sds_Get29, Caddar_Ret),
			  get_var(LEnv, sys_sds, Sds_Get30),
			  f_car(Sds_Get30, Cadddr_Param46),
			  f_cadddr(Cadddr_Param46, Cadddr_Ret),
			  get_var(LEnv, sys_olds, Olds_Get31),
			  f_car(Olds_Get31, Cddddr_Param),
			  f_cddddr(Cddddr_Param, Car_Param),
			  f_car(Car_Param, Car_Ret),
			  CAR=[Caar_Ret, Cadar_Ret, Caddar_Ret, Cadddr_Ret, Car_Ret],
			  get_var(LEnv, sys_news, News_Get32),
			  get_var(LEnv, sys_olds, Olds_Get33),
			  f_cdr(Olds_Get33, Cdr_Ret),
			  f_sys_overwrite_slot_descriptions(News_Get32,
							    Cdr_Ret,
							    Slot_descriptions_Ret),
			  TrueResult37=[CAR|Slot_descriptions_Ret],
			  LetResult=TrueResult37
		      ;   get_var(LEnv, sys_olds, Olds_Get34),
			  f_car(Olds_Get34, Car_Ret58),
			  get_var(LEnv, sys_news, News_Get35),
			  get_var(LEnv, sys_olds, Olds_Get36),
			  f_cdr(Olds_Get36, Cdr_Ret59),
			  f_sys_overwrite_slot_descriptions(News_Get35,
							    Cdr_Ret59,
							    Slot_descriptions_Ret60),
			  ElseResult=[Car_Ret58|Slot_descriptions_Ret60],
			  LetResult=ElseResult
		      ),
		      _6036=LetResult
		  ;   _6036=[]
		  )
		),
		_6036=FnResult
	      ),
	      block_exit(sys_overwrite_slot_descriptions, FnResult),
	      true).
:- set_opv(sys_overwrite_slot_descriptions,
	   symbol_function,
	   f_sys_overwrite_slot_descriptions),
   DefunResult=sys_overwrite_slot_descriptions.
/*
:- side_effect(assert_lsp(sys_overwrite_slot_descriptions,
			  lambda_def(defun,
				     sys_overwrite_slot_descriptions,
				     f_sys_overwrite_slot_descriptions,
				     [sys_news, sys_olds],
				     
				     [ [declare, [sys_c_local]],
				       
				       [ when,
					 sys_olds,
					 
					 [ let,
					   
					   [ 
					     [ sys_sds,
					       
					       [ member,
						 [caar, sys_olds],
						 sys_news,
						 kw_key,
						 function(car)
					       ]
					     ]
					   ],
					   
					   [ cond,
					     
					     [ sys_sds,
					       
					       [ when,
						 
						 [ and,
						   
						   [ null,
						     [cadddr, [car, sys_sds]]
						   ],
						   [cadddr, [car, sys_olds]]
						 ],
						 
						 [ error,
						   '$ARRAY'([*],
							    claz_base_character,
							    "~S is an illegal include slot-description."),
						   sys_sds
						 ]
					       ],
					       
					       [ cons,
						 
						 [ list,
						   [caar, sys_sds],
						   [cadar, sys_sds],
						   [caddar, sys_sds],
						   [cadddr, [car, sys_sds]],
						   
						   [ car,
						     [cddddr, [car, sys_olds]]
						   ]
						 ],
						 
						 [ sys_overwrite_slot_descriptions,
						   sys_news,
						   [cdr, sys_olds]
						 ]
					       ]
					     ],
					     
					     [ t,
					       
					       [ cons,
						 [car, sys_olds],
						 
						 [ sys_overwrite_slot_descriptions,
						   sys_news,
						   [cdr, sys_olds]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_overwrite_slot_descriptions,
			  arglist_info(sys_overwrite_slot_descriptions,
				       f_sys_overwrite_slot_descriptions,
				       [sys_news, sys_olds],
				       arginfo{ all:[sys_news, sys_olds],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_news, sys_olds],
						opt:0,
						req:[sys_news, sys_olds],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_overwrite_slot_descriptions,
			  init_args(x, f_sys_overwrite_slot_descriptions))).
*/
/*
; If read-only is true in the old
*/
/*
;  and false in the new, signal an error.
*/
/*
; The offset if from the old.
*/
/*
(defun define-structure (name conc-name type named slots slot-descriptions
			      copier include print-function constructors
			      offset documentation)
  (put-sysprop name 'DEFSTRUCT-FORM `(defstruct ,name ,@slots))
  (put-sysprop name 'IS-A-STRUCTURE t)
  (put-sysprop name 'STRUCTURE-SLOT-DESCRIPTIONS slot-descriptions)
  (put-sysprop name 'STRUCTURE-INCLUDE include)
  (put-sysprop name 'STRUCTURE-PRINT-FUNCTION print-function)
  (put-sysprop name 'STRUCTURE-TYPE type)
  (put-sysprop name 'STRUCTURE-NAMED named)
  (put-sysprop name 'STRUCTURE-OFFSET offset)
  (put-sysprop name 'STRUCTURE-CONSTRUCTORS constructors)
  #+clos
  (when *keep-documentation*
    (sys:set-documentation name 'STRUCTURE documentation))
  (and (consp type) (eq (car type) 'VECTOR)
       (setq type 'VECTOR))
  (dolist (x slot-descriptions)
    (and x (car x)
	 (funcall #'make-access-function name conc-name type named x)))
  (when copier
    (fset copier
	  (ecase type
	    ((NIL) #'sys::copy-structure)
	    (LIST #'copy-list)
	    (VECTOR #'copy-seq))))
  )


;; Set the dispatch macro.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:5323 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'define-structure',[name,'conc-name',type,named,slots,'slot-descriptions',copier,include,'print-function',constructors,offset,documentation],['put-sysprop',name,[quote,'DEFSTRUCT-FORM'],['#BQ',[defstruct,['#COMMA',name],['#BQ-COMMA-ELIPSE',slots]]]],['put-sysprop',name,[quote,'IS-A-STRUCTURE'],t],['put-sysprop',name,[quote,'STRUCTURE-SLOT-DESCRIPTIONS'],'slot-descriptions'],['put-sysprop',name,[quote,'STRUCTURE-INCLUDE'],include],['put-sysprop',name,[quote,'STRUCTURE-PRINT-FUNCTION'],'print-function'],['put-sysprop',name,[quote,'STRUCTURE-TYPE'],type],['put-sysprop',name,[quote,'STRUCTURE-NAMED'],named],['put-sysprop',name,[quote,'STRUCTURE-OFFSET'],offset],['put-sysprop',name,[quote,'STRUCTURE-CONSTRUCTORS'],constructors],[and,[consp,type],[eq,[car,type],[quote,'VECTOR']],[setq,type,[quote,'VECTOR']]],[dolist,[x,'slot-descriptions'],[and,x,[car,x],[funcall,function('make-access-function'),name,'conc-name',type,named,x]]],[when,copier,[fset,copier,[ecase,type,[[[]],function('sys::copy-structure')],['LIST',function('copy-list')],['VECTOR',function('copy-seq')]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_define_structure,
					       kw_function,
					       f_sys_define_structure)).
*/
/*
% ecase:-[[[[]],function(copy_structure)],[list,function(copy_list)],[vector,function(copy_seq)]].
*/
/*
% conds:-[[[eq,_21118,[quote,[]]],[progn,function(copy_structure)]],[[eq,_21118,[quote,list]],[progn,function(copy_list)]],[[eq,_21118,[quote,vector]],[progn,function(copy_seq)]],[t,[type_error,_21374,[quote,[member,[],list,vector]]]]].
*/
/*
:-side_effect(generate_function_or_macro_name([name='GLOBAL',environ=env_1],type_error,kw_function,f_type_error)).
*/
wl:lambda_def(defun, sys_define_structure, f_sys_define_structure, [sys_name, sys_conc_name, type, sys_named, sys_slots, sys_slot_descriptions, sys_copier, sys_include, sys_print_function, sys_constructors, sys_offset, documentation], [[sys_put_sysprop, sys_name, [quote, sys_defstruct_form], ['#BQ', [defstruct, ['#COMMA', sys_name], ['#BQ-COMMA-ELIPSE', sys_slots]]]], [sys_put_sysprop, sys_name, [quote, sys_is_a_structure], t], [sys_put_sysprop, sys_name, [quote, sys_structure_slot_descriptions], sys_slot_descriptions], [sys_put_sysprop, sys_name, [quote, sys_structure_include], sys_include], [sys_put_sysprop, sys_name, [quote, sys_structure_print_function], sys_print_function], [sys_put_sysprop, sys_name, [quote, sys_structure_type], type], [sys_put_sysprop, sys_name, [quote, sys_structure_named], sys_named], [sys_put_sysprop, sys_name, [quote, sys_structure_offset], sys_offset], [sys_put_sysprop, sys_name, [quote, sys_structure_constructors], sys_constructors], [and, [consp, type], [eq, [car, type], [quote, vector]], [setq, type, [quote, vector]]], [dolist, [sys_x, sys_slot_descriptions], [and, sys_x, [car, sys_x], [funcall, function(sys_make_access_function), sys_name, sys_conc_name, type, sys_named, sys_x]]], [when, sys_copier, [sys_fset, sys_copier, [ecase, type, [[[]], function(copy_structure)], [list, function(copy_list)], [vector, function(copy_seq)]]]]]).
wl:arglist_info(sys_define_structure, f_sys_define_structure, [sys_name, sys_conc_name, type, sys_named, sys_slots, sys_slot_descriptions, sys_copier, sys_include, sys_print_function, sys_constructors, sys_offset, documentation], arginfo{all:[sys_name, sys_conc_name, type, sys_named, sys_slots, sys_slot_descriptions, sys_copier, sys_include, sys_print_function, sys_constructors, sys_offset, documentation], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_conc_name, type, sys_named, sys_slots, sys_slot_descriptions, sys_copier, sys_include, sys_print_function, sys_constructors, sys_offset, documentation], opt:0, req:[sys_name, sys_conc_name, type, sys_named, sys_slots, sys_slot_descriptions, sys_copier, sys_include, sys_print_function, sys_constructors, sys_offset, documentation], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_define_structure).

/*

### Compiled Function: `SYS::DEFINE-STRUCTURE` 
*/
f_sys_define_structure(Name_In, Conc_name_In, Type_In, Named_In, Slots_In, Slot_descriptions_In, Copier_In, Include_In, Print_function_In, Constructors_In, Offset_In, Documentation_In, FnResult) :-
	AEnv=[bv(sys_name, Name_In), bv(sys_conc_name, Conc_name_In), bv(type, Type_In), bv(sys_named, Named_In), bv(sys_slots, Slots_In), bv(sys_slot_descriptions, Slot_descriptions_In), bv(sys_copier, Copier_In), bv(sys_include, Include_In), bv(sys_print_function, Print_function_In), bv(sys_constructors, Constructors_In), bv(sys_offset, Offset_In), bv(documentation, Documentation_In)],
	catch(( ( get_var(AEnv, sys_name, Name_Get17),
		  get_var(AEnv, sys_slots, Slots_Get),
		  f_sys_put_sysprop(Name_Get17,
				    sys_defstruct_form,
				    [defstruct, Name_Get17|Slots_Get],
				    [],
				    Put_sysprop_Ret),
		  get_var(AEnv, sys_name, Name_Get19),
		  f_sys_put_sysprop(Name_Get19,
				    sys_is_a_structure,
				    t,
				    [],
				    Put_sysprop_Ret82),
		  get_var(AEnv, sys_name, Name_Get20),
		  get_var(AEnv, sys_slot_descriptions, Slot_descriptions_Get),
		  f_sys_put_sysprop(Name_Get20,
				    sys_structure_slot_descriptions,
				    Slot_descriptions_Get,
				    [],
				    Put_sysprop_Ret83),
		  get_var(AEnv, sys_include, Include_Get),
		  get_var(AEnv, sys_name, Name_Get22),
		  f_sys_put_sysprop(Name_Get22,
				    sys_structure_include,
				    Include_Get,
				    [],
				    Put_sysprop_Ret84),
		  get_var(AEnv, sys_name, Name_Get24),
		  get_var(AEnv, sys_print_function, Print_function_Get),
		  f_sys_put_sysprop(Name_Get24,
				    sys_structure_print_function,
				    Print_function_Get,
				    [],
				    Put_sysprop_Ret85),
		  get_var(AEnv, sys_name, Name_Get26),
		  get_var(AEnv, type, Type_Get),
		  f_sys_put_sysprop(Name_Get26,
				    sys_structure_type,
				    Type_Get,
				    [],
				    Put_sysprop_Ret86),
		  get_var(AEnv, sys_name, Name_Get28),
		  get_var(AEnv, sys_named, Named_Get),
		  f_sys_put_sysprop(Name_Get28,
				    sys_structure_named,
				    Named_Get,
				    [],
				    Put_sysprop_Ret87),
		  get_var(AEnv, sys_name, Name_Get30),
		  get_var(AEnv, sys_offset, Offset_Get),
		  f_sys_put_sysprop(Name_Get30,
				    sys_structure_offset,
				    Offset_Get,
				    [],
				    Put_sysprop_Ret88),
		  get_var(AEnv, sys_constructors, Constructors_Get),
		  get_var(AEnv, sys_name, Name_Get32),
		  f_sys_put_sysprop(Name_Get32,
				    sys_structure_constructors,
				    Constructors_Get,
				    [],
				    Put_sysprop_Ret89),
		  get_var(AEnv, type, Type_Get35),
		  (   c0nz:is_consp(Type_Get35)
		  ->  get_var(AEnv, type, Type_Get39),
		      f_car(Type_Get39, PredArg1Result),
		      (   is_eq(PredArg1Result, vector)
		      ->  set_var(AEnv, type, vector),
			  TrueResult=vector
		      ;   TrueResult=[]
		      ),
		      _7586=TrueResult
		  ;   _7586=[]
		  ),
		  get_var(AEnv, sys_slot_descriptions, Slot_descriptions_Get44),
		  BV=bv(sys_x, Ele),
		  Env2=[BV|AEnv],
		  forall(member(Ele, Slot_descriptions_Get44),
			 ( nb_setarg(2, BV, Ele),
			   get_var(Env2, sys_x, IFTEST45),
			   (   IFTEST45\==[]
			   ->  get_var(Env2, sys_x, X_Get50),
			       f_car(X_Get50, IFTEST48),
			       (   IFTEST48\==[]
			       ->  get_var(Env2, sys_conc_name, Conc_name_Get),
				   get_var(Env2, sys_name, Name_Get51),
				   get_var(Env2, sys_named, Named_Get54),
				   ( get_var(Env2, sys_x, X_Get55),
				     get_var(Env2, type, Type_Get53)
				   ),
				   f_sys_make_access_function(Name_Get51,
							      Conc_name_Get,
							      Type_Get53,
							      Named_Get54,
							      X_Get55,
							      TrueResult56),
				   TrueResult57=TrueResult56
			       ;   TrueResult57=[]
			       ),
			       _7780=TrueResult57
			   ;   _7780=[]
			   )
			 )),
		  get_var(AEnv, sys_copier, IFTEST62),
		  (   IFTEST62\==[]
		  ->  get_var(AEnv, sys_copier, Copier_Get65),
		      get_var(AEnv, type, Key),
		      (   is_eq(Key, [])
		      ->  _8262=f_copy_structure
		      ;   (   is_eq(Key, list)
			  ->  ElseResult77=f_copy_list
			  ;   (   is_eq(Key, vector)
			      ->  ElseResult76=f_copy_seq
			      ;   f_type_error(Type_Get66,
					       [member, [], list, vector],
					       ElseResult),
				  ElseResult76=ElseResult
			      ),
			      ElseResult77=ElseResult76
			  ),
			  _8262=ElseResult77
		      ),
		      f_sys_fset(Copier_Get65, _8262, TrueResult78),
		      _7170=TrueResult78
		  ;   _7170=[]
		  )
		),
		_7170=FnResult
	      ),
	      block_exit(sys_define_structure, FnResult),
	      true).
:- set_opv(sys_define_structure, symbol_function, f_sys_define_structure),
   DefunResult=sys_define_structure.
/*
:- side_effect(assert_lsp(sys_define_structure,
			  lambda_def(defun,
				     sys_define_structure,
				     f_sys_define_structure,
				     
				     [ sys_name,
				       sys_conc_name,
				       type,
				       sys_named,
				       sys_slots,
				       sys_slot_descriptions,
				       sys_copier,
				       sys_include,
				       sys_print_function,
				       sys_constructors,
				       sys_offset,
				       documentation
				     ],
				     
				     [ 
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_defstruct_form],
					 
					 [ '#BQ',
					   
					   [ defstruct,
					     ['#COMMA', sys_name],
					     ['#BQ-COMMA-ELIPSE', sys_slots]
					   ]
					 ]
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_is_a_structure],
					 t
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 
					 [ quote,
					   sys_structure_slot_descriptions
					 ],
					 sys_slot_descriptions
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_structure_include],
					 sys_include
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_structure_print_function],
					 sys_print_function
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_structure_type],
					 type
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_structure_named],
					 sys_named
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_structure_offset],
					 sys_offset
				       ],
				       
				       [ sys_put_sysprop,
					 sys_name,
					 [quote, sys_structure_constructors],
					 sys_constructors
				       ],
				       
				       [ and,
					 [consp, type],
					 [eq, [car, type], [quote, vector]],
					 [setq, type, [quote, vector]]
				       ],
				       
				       [ dolist,
					 [sys_x, sys_slot_descriptions],
					 
					 [ and,
					   sys_x,
					   [car, sys_x],
					   
					   [ funcall,
					     function(sys_make_access_function),
					     sys_name,
					     sys_conc_name,
					     type,
					     sys_named,
					     sys_x
					   ]
					 ]
				       ],
				       
				       [ when,
					 sys_copier,
					 
					 [ sys_fset,
					   sys_copier,
					   
					   [ ecase,
					     type,
					     [[[]], function(copy_structure)],
					     [list, function(copy_list)],
					     [vector, function(copy_seq)]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_define_structure,
			  arglist_info(sys_define_structure,
				       f_sys_define_structure,
				       
				       [ sys_name,
					 sys_conc_name,
					 type,
					 sys_named,
					 sys_slots,
					 sys_slot_descriptions,
					 sys_copier,
					 sys_include,
					 sys_print_function,
					 sys_constructors,
					 sys_offset,
					 documentation
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_conc_name,
						      type,
						      sys_named,
						      sys_slots,
						      sys_slot_descriptions,
						      sys_copier,
						      sys_include,
						      sys_print_function,
						      sys_constructors,
						      sys_offset,
						      documentation
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
							sys_slots,
							sys_slot_descriptions,
							sys_copier,
							sys_include,
							sys_print_function,
							sys_constructors,
							sys_offset,
							documentation
						      ],
						opt:0,
						req:
						    [ sys_name,
						      sys_conc_name,
						      type,
						      sys_named,
						      sys_slots,
						      sys_slot_descriptions,
						      sys_copier,
						      sys_include,
						      sys_print_function,
						      sys_constructors,
						      sys_offset,
						      documentation
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_define_structure,
			  init_args(x, f_sys_define_structure))).
*/
/*
; Set the dispatch macro.
*/
/*
(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:6404 **********************/
:-lisp_compile_to_prolog(pkg_sys,['set-dispatch-macro-character',#\(#),#\(s),[quote,'sharp-s-reader']])
:- f_set_dispatch_macro_character(#\(#), #\(s), sys_sharp_s_reader, _Ignored).
/*
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)


;; Examples from Common Lisp Reference Manual.

#|
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct.lsp:6459 **********************/
:-lisp_compile_to_prolog(pkg_sys,['set-dispatch-macro-character',#\(#),#\('S'),[quote,'sharp-s-reader']])
:- f_set_dispatch_macro_character(#\(#), #\('S'), sys_sharp_s_reader, _Ignored).
/*
; Examples from Common Lisp Reference Manual.
*/
/*

(defstruct ship
  x-position
  y-position
  x-velocity
  y-velocity
  mass)

(defstruct person name age sex)

(defstruct (astronaut (:include person (age 45))
                      (:conc-name astro-))
  helmet-size
  (favorite-beverage 'tang))

(defstruct (foo (:constructor create-foo (a
                                          &optional b (c 'sea)
                                          &rest d
                                          &aux e (f 'eff))))
  a (b 'bee) c d e f)

(defstruct (binop (:type list) :named (:initial-offset 2))
  (operator '?)
  operand-1
  operand-2)

(defstruct (annotated-binop (:type list)
                            (:initial-offset 3)
                            (:include binop))
  commutative
  associative
  identity)
*/


%; Total compilation time: 6.615 seconds


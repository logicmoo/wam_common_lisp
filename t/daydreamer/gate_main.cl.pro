#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "gate_main" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/
%; Start time: Sat Dec 23 22:14:41 2017

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
*******************************************************************************
*/
/*
*/
/*
 GATE
*/
/*
 Version 2.3
*/
/*
*/
/*
 Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
*/
/*
 All Rights Reserved.
*/
/*
*/
/*
 This file contains:
*/
/*
 OB slot-filler objects
*/
/*
*/
/*
 10/13/84: Original version written
*/
/*
  1/24/86: Added path functions, got rid of weblists
*/
/*
  1/28/86: Changed add to use append-end instead of cons
*/
/*
  9/25/86: Converted to be independent of flavors
*/
/*
 11/02/86: Added add-unique-name
*/
/*
*/
/*
*******************************************************************************
*/
/*
(defun is-var? (x) (var? x))

; Global list of obs
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:567 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'is-var?',[x],['var?',x]])
wl:lambda_def(defun, u_is_var_c63, f_u_is_var_c63, [u_x], [[u_var_c63, u_x]]).
wl:arglist_info(u_is_var_c63, f_u_is_var_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_is_var_c63).

/*

### Compiled:  `U::IS-VAR?` 
*/
f_u_is_var_c63(X, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_x, X)|Env],
	f_u_var_c63(u_x, Var_c63_Ret),
	Var_c63_Ret=FnResult.
:- set_opv(f_u_is_var_c63, classof, claz_function),
   set_opv(u_is_var_c63, compile_as, kw_function),
   set_opv(u_is_var_c63, function, f_u_is_var_c63),
   _Ignored4=u_is_var_c63.
/*
:- side_effect(assert_lsp(u_is_var_c63,
			  wl:lambda_def(defun, u_is_var_c63, f_u_is_var_c63, [u_x], [[u_var_c63, u_x]]))).
*/
/*
:- side_effect(assert_lsp(u_is_var_c63,
			  wl:arglist_info(u_is_var_c63, f_u_is_var_c63, [u_x], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_is_var_c63,
			  wl:init_args(exact_only, f_u_is_var_c63))).
*/
/*
 Global list of obs
*/
/*
(setq *obs* nil)

; Global list of obnames
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:619 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*obs*',[]])
:- set_var(AEnv, setq, u_xx_obs_xx, []).
/*
 Global list of obnames
*/
/*
(setq *obnames* nil)

;
; OBR
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:662 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*obnames*',[]])
:- set_var(AEnv, setq, u_xx_obnames_xx, []).
/*
*/
/*
 OBR
*/
/*
*/
/*
(defun print-ob (ob stream depth)
 (declare (ignore depth))
 (ob$print-self ob stream))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:695 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'print-ob',[ob,stream,depth],[declare,[ignore,depth]],['ob$print-self',ob,stream]])
wl:lambda_def(defun, u_print_ob, f_u_print_ob, [u_ob, stream, u_depth], [[declare, [ignore, u_depth]], [u_ob_c36_print_self, u_ob, stream]]).
wl:arglist_info(u_print_ob, f_u_print_ob, [u_ob, stream, u_depth], arginfo{all:[u_ob, stream, u_depth], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, stream, u_depth], opt:0, req:[u_ob, stream, u_depth], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_print_ob).

/*

### Compiled:  `U::PRINT-OB` 
*/
f_u_print_ob(Ob, Stream, Depth, FnResult) :-
	nop(global_env(Env)),
	Env11=[bv(u_ob, Ob), bv(stream, Stream), bv(u_depth, Depth)|Env],
	cl_declare([ignore, u_depth], Declare_Ret),
	get_var(Env11, stream, Stream_Get),
	get_var(Env11, u_ob, Ob_Get),
	f_u_ob_c36_print_self(Ob_Get, Stream_Get, Print_self_Ret),
	Print_self_Ret=FnResult.
:- set_opv(f_u_print_ob, classof, claz_function),
   set_opv(u_print_ob, compile_as, kw_function),
   set_opv(u_print_ob, function, f_u_print_ob),
   _Ignored4=u_print_ob.
/*
:- side_effect(assert_lsp(u_print_ob,
			  wl:lambda_def(defun, u_print_ob, f_u_print_ob, [u_ob, stream, u_depth], [[declare, [ignore, u_depth]], [u_ob_c36_print_self, u_ob, stream]]))).
*/
/*
:- side_effect(assert_lsp(u_print_ob,
			  wl:arglist_info(u_print_ob, f_u_print_ob, [u_ob, stream, u_depth], arginfo{all:[u_ob, stream, u_depth], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, stream, u_depth], opt:0, req:[u_ob, stream, u_depth], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_print_ob, wl:init_args(exact_only, f_u_print_ob))).
*/
/*
(defstruct (obr (:print-function print-ob))
      "OB representation structure"
      (obnames nil)    ; list of symbols which may be used to name the ob
      (slots nil)      ; list of (slot-name slot-value)
      (literal nil)    ; whether the ob is a literal ob
)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:784 **********************/
:-lisp_compile_to_prolog(pkg_user,[defstruct,[obr,[':print-function','print-ob']],'$STRING'("OB representation structure"),[obnames,[]],[slots,[]],[literal,[]]])
:- cl_defstruct(
		[ [u_obr, [kw_print_function, u_print_ob]],
		  '$ARRAY'([*],
			   claz_base_character,
			   "OB representation structure"),
		  [u_obnames, []],
		  [sys_slots, []],
		  [u_literal, []]
		],
		_Ignored4).
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_alternate_metaclass, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 1, zlot_structure_object_alternate_metaclass))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_conc_name, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 2, zlot_structure_object_conc_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_constructors, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 3, zlot_structure_object_constructors))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_copier_name, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 4, zlot_structure_object_copier_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_doc, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 5, zlot_structure_object_doc))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_element_type, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 6, zlot_structure_object_element_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_include, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 7, zlot_structure_object_include))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 8, zlot_structure_object_inherited_accessor_alist))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_length, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 9, zlot_structure_object_length))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_name, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 10, zlot_structure_object_name))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_named, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 11, zlot_structure_object_named))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_null_lexenv_p, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 12, zlot_structure_object_null_lexenv_p))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_offset, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 13, zlot_structure_object_offset))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_predicate, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 14, zlot_structure_object_predicate))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_print_option, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 15, zlot_structure_object_print_option))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_printer_fname, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 16, zlot_structure_object_printer_fname))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_pure, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 17, zlot_structure_object_pure))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_slots, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 18, zlot_structure_object_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_structure_class, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 19, zlot_structure_object_structure_class))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_type, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 20, zlot_structure_object_type))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, symbolname, u_obr))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, type, u_obr))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, print_function, u_print_ob))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, slot, u_obnames, zlot_obr_obnames))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, keyword, kw_obnames, zlot_obr_obnames))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, ordinal, 1, zlot_obr_obnames))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, initform, [], zlot_obr_obnames))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, slot, sys_slots, zlot_obr_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, keyword, kw_slots, zlot_obr_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, ordinal, 2, zlot_obr_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, initform, [], zlot_obr_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, slot, u_literal, zlot_obr_literal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, keyword, kw_literal, zlot_obr_literal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, ordinal, 3, zlot_obr_literal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, initform, [], zlot_obr_literal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, conc_name, "OBR-"))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, setter_fn, u_setf_obr_obnames, zlot_obr_obnames))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, accessor, u_obr_obnames, zlot_obr_obnames))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, setter_fn, u_setf_obr_slots, zlot_obr_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, accessor, u_obr_slots, zlot_obr_slots))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, setter_fn, u_setf_obr_literal, zlot_obr_literal))).
*/
/*
:- side_effect(assertz_new(soops:struct_opv(claz_u_obr, accessor, u_obr_literal, zlot_obr_literal))).
*/
/*
 list of symbols which may be used to name the ob
*/
/*
 list of (slot-name slot-value)
*/
/*
 whether the ob is a literal ob
*/
/*
(defun ob$print-self (self stream)
  (cond
   ((ty? self)
    (format stream "#{"(defun ob$print-self (self stream)\n  (cond\n   ((ty? self)\n    (format stream \"#{~A}\" (car (obr-obnames self))))\n   ((var? self) (format stream \"#{~A: ?~A:~A}\"\n                        (car (obr-obnames self))\n                        (variable-name self)\n                        (if (variable-type self)\n                            (car (obr-obnames (variable-type self)))\n                            nil)))\n   (else\n    (format stream \"#{~A: \" (car (obr-obnames self)))\n    (ob$sprint self stream)\n    (format stream \"}\"))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1053 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$print-self',[self,stream],[cond,[['ty?',self],[format,stream,'$STRING'("#{~A}"),[car,['obr-obnames',self]]]],[['var?',self],[format,stream,'$STRING'("#{~A: ?~A:~A}"),[car,['obr-obnames',self]],['variable-name',self],[if,['variable-type',self],[car,['obr-obnames',['variable-type',self]]],[]]]],[else,[format,stream,'$STRING'("#{~A: "),[car,['obr-obnames',self]]],['ob$sprint',self,stream],[format,stream,'$STRING'("}")]]]])
wl:lambda_def(defun, u_ob_c36_print_self, f_u_ob_c36_print_self, [u_self, stream], [[cond, [[u_ty_c63, u_self], [format, stream, '$ARRAY'([*], claz_base_character, "#{~A}"), [car, [u_obr_obnames, u_self]]]], [[u_var_c63, u_self], [format, stream, '$ARRAY'([*], claz_base_character, "#{~A: ?~A:~A}"), [car, [u_obr_obnames, u_self]], [u_variable_name, u_self], [if, [u_variable_type, u_self], [car, [u_obr_obnames, [u_variable_type, u_self]]], []]]], [u_else, [format, stream, '$ARRAY'([*], claz_base_character, "#{~A: "), [car, [u_obr_obnames, u_self]]], [u_ob_c36_sprint, u_self, stream], [format, stream, '$ARRAY'([*], claz_base_character, "}")]]]]).
wl:arglist_info(u_ob_c36_print_self, f_u_ob_c36_print_self, [u_self, stream], arginfo{all:[u_self, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, stream], opt:0, req:[u_self, stream], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_print_self).

/*

### Compiled:  `U::OB$PRINT-SELF` 
*/
f_u_ob_c36_print_self(Self, Stream, ElseResult29) :-
	nop(global_env(Env)),
	Env34=[bv(u_self, Self), bv(stream, Stream)|Env],
	f_u_ty_c63(u_self, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env34, stream, Stream_Get),
	    get_var(Env34, u_self, Self_Get),
	    f_u_obr_obnames(Self_Get, Car_Param),
	    cl_car(Car_Param, Car_Ret),
	    cl_format(
		      [ Stream_Get,
			'$ARRAY'([*], claz_base_character, "#{~A}"),
			Car_Ret
		      ],
		      TrueResult30),
	    ElseResult29=TrueResult30
	;   f_u_var_c63(u_self, IFTEST11),
	    (   IFTEST11\==[]
	    ->  get_var(Env34, stream, Stream_Get13),
		get_var(Env34, u_self, Self_Get14),
		f_u_obr_obnames(Self_Get14, Car_Param38),
		cl_car(Car_Param38, Car_Ret43),
		f_u_variable_name(u_self, Variable_name_Ret),
		f_u_variable_type(u_self, IFTEST15),
		(   IFTEST15\==[]
		->  f_u_variable_type(u_self, Obr_obnames_Param),
		    f_u_obr_obnames(Obr_obnames_Param, Car_Param40),
		    cl_car(Car_Param40, TrueResult),
		    CAR=TrueResult
		;   CAR=[]
		),
		cl_format(
			  [ Stream_Get13,
			    '$ARRAY'([*], claz_base_character, "#{~A: ?~A:~A}"),
			    Car_Ret43,
			    Variable_name_Ret,
			    CAR
			  ],
			  TrueResult28),
		ElseResult29=TrueResult28
	    ;   get_var(Env34, u_else, IFTEST18),
		(   IFTEST18\==[]
		->  get_var(Env34, stream, Stream_Get21),
		    get_var(Env34, u_self, Self_Get22),
		    f_u_obr_obnames(Self_Get22, Car_Param41),
		    cl_car(Car_Param41, Car_Ret46),
		    cl_format(
			      [ Stream_Get21,
				'$ARRAY'([*], claz_base_character, "#{~A: "),
				Car_Ret46
			      ],
			      Format_Ret),
		    get_var(Env34, stream, Stream_Get24),
		    get_var(Env34, u_self, Self_Get23),
		    f_u_ob_c36_sprint(Self_Get23, Stream_Get24, C36_sprint_Ret),
		    get_var(Env34, stream, Stream_Get25),
		    cl_format(
			      [ Stream_Get25,
				'$ARRAY'([*], claz_base_character, "}")
			      ],
			      TrueResult26),
		    ElseResult29=TrueResult26
		;   ElseResult29=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_print_self, classof, claz_function),
   set_opv(u_ob_c36_print_self, compile_as, kw_function),
   set_opv(u_ob_c36_print_self, function, f_u_ob_c36_print_self),
   _Ignored4=u_ob_c36_print_self.
/*
:- side_effect(assert_lsp(u_ob_c36_print_self,
			  wl:lambda_def(defun, u_ob_c36_print_self, f_u_ob_c36_print_self, [u_self, stream], [[cond, [[u_ty_c63, u_self], [format, stream, '$ARRAY'([*], claz_base_character, "#{~A}"), [car, [u_obr_obnames, u_self]]]], [[u_var_c63, u_self], [format, stream, '$ARRAY'([*], claz_base_character, "#{~A: ?~A:~A}"), [car, [u_obr_obnames, u_self]], [u_variable_name, u_self], [if, [u_variable_type, u_self], [car, [u_obr_obnames, [u_variable_type, u_self]]], []]]], [u_else, [format, stream, '$ARRAY'([*], claz_base_character, "#{~A: "), [car, [u_obr_obnames, u_self]]], [u_ob_c36_sprint, u_self, stream], [format, stream, '$ARRAY'([*], claz_base_character, "}")]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_print_self,
			  wl:arglist_info(u_ob_c36_print_self, f_u_ob_c36_print_self, [u_self, stream], arginfo{all:[u_self, stream], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, stream], opt:0, req:[u_self, stream], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_print_self,
			  wl:init_args(exact_only, f_u_ob_c36_print_self))).
*/
/*
(setq *hidden-default* t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1578 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*hidden-default*',t])
:- set_var(AEnv, setq, u_xx_hidden_default_xx, t).
/*
(setq *next-ob-number* 1)

;
; ob$create-named-empty: create an empty ob with the specified name
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1605 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*next-ob-number*',1])
:- set_var(AEnv, setq, u_xx_next_ob_number_xx, 1).
/*
*/
/*
 ob$create-named-empty: create an empty ob with the specified name
*/
/*
*/
/*
(defun ob$create-named-empty (name)
  (let ((self (make-obr)))
       (setq *obs* (cons self *obs*))
       (if name
           (ob$add-name self name)
           (ob$add-unique-name
            self
            (string->symbol
             (string-append "OB."
                            (prog1
                             (fixnum->string *next-ob-number*)
                             (increment-me *next-ob-number*))))))
       self))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:1704 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$create-named-empty',[name],[let,[[self,['make-obr']]],[setq,'*obs*',[cons,self,'*obs*']],[if,name,['ob$add-name',self,name],['ob$add-unique-name',self,['string->symbol',['string-append','$STRING'("OB."),[prog1,['fixnum->string','*next-ob-number*'],['increment-me','*next-ob-number*']]]]]],self]])
wl:lambda_def(defun, u_ob_c36_create_named_empty, f_u_ob_c36_create_named_empty, [sys_name], [[let, [[u_self, [u_make_obr]]], [setq, u_xx_obs_xx, [cons, u_self, u_xx_obs_xx]], [if, sys_name, [u_ob_c36_add_name, u_self, sys_name], [u_ob_c36_add_unique_name, u_self, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "OB."), [prog1, [u_fixnum_c62_string, u_xx_next_ob_number_xx], [u_increment_me, u_xx_next_ob_number_xx]]]]]], u_self]]).
wl:arglist_info(u_ob_c36_create_named_empty, f_u_ob_c36_create_named_empty, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_create_named_empty).

/*

### Compiled:  `U::OB$CREATE-NAMED-EMPTY` 
*/
f_u_ob_c36_create_named_empty(Name, FnResult) :-
	nop(global_env(Env)),
	Env25=[bv(sys_name, Name)|Env],
	f_u_make_obr([], Self_Init),
	LEnv=[bv(u_self, Self_Init)|Env25],
	get_var(LEnv, u_self, Self_Get),
	get_var(LEnv, u_xx_obs_xx, Xx_obs_xx_Get),
	Xx_obs_xx=[Self_Get|Xx_obs_xx_Get],
	set_var(LEnv, u_xx_obs_xx, Xx_obs_xx),
	get_var(LEnv, sys_name, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, sys_name, Name_Get18),
	    get_var(LEnv, u_self, Self_Get17),
	    f_u_ob_c36_add_name(Self_Get17, Name_Get18, TrueResult),
	    _150958120=TrueResult
	;   get_var(LEnv, u_self, Self_Get19),
	    f_u_string_c62_symbol(
				  [ u_string_append,
				    '$ARRAY'([*], claz_base_character, "OB."),
				    
				    [ prog1,
				      
				      [ u_fixnum_c62_string,
					u_xx_next_ob_number_xx
				      ],
				      [u_increment_me, u_xx_next_ob_number_xx]
				    ]
				  ],
				  C62_symbol_Ret),
	    f_u_ob_c36_add_unique_name(Self_Get19, C62_symbol_Ret, ElseResult),
	    _150958120=ElseResult
	),
	get_var(LEnv, u_self, Self_Get22),
	Self_Get22=FnResult.
:- set_opv(f_u_ob_c36_create_named_empty, classof, claz_function),
   set_opv(u_ob_c36_create_named_empty, compile_as, kw_function),
   set_opv(u_ob_c36_create_named_empty, function, f_u_ob_c36_create_named_empty),
   _Ignored4=u_ob_c36_create_named_empty.
/*
:- side_effect(assert_lsp(u_ob_c36_create_named_empty,
			  wl:lambda_def(defun, u_ob_c36_create_named_empty, f_u_ob_c36_create_named_empty, [sys_name], [[let, [[u_self, [u_make_obr]]], [setq, u_xx_obs_xx, [cons, u_self, u_xx_obs_xx]], [if, sys_name, [u_ob_c36_add_name, u_self, sys_name], [u_ob_c36_add_unique_name, u_self, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "OB."), [prog1, [u_fixnum_c62_string, u_xx_next_ob_number_xx], [u_increment_me, u_xx_next_ob_number_xx]]]]]], u_self]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_create_named_empty,
			  wl:arglist_info(u_ob_c36_create_named_empty, f_u_ob_c36_create_named_empty, [sys_name], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_create_named_empty,
			  wl:init_args(exact_only, f_u_ob_c36_create_named_empty))).
*/
/*
(defun ob$destroy (self)
  (if (obr-node self)
      (error "Sure enough, node of "(defun ob$destroy (self)\n  (if (obr-node self)\n      (error \"Sure enough, node of ~A isn't nil!\" self))\n  (ob$remove-all self)\n  (yloop (yfor obname in (obr-obnames self))\n        (ydo (ob$remove-name self obname)))\n  (setq *obs* (delq! self *obs*)))\n\n;\n; Inverse slots\n;\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2145 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$destroy',[self],[if,['obr-node',self],[error,'$STRING'("Sure enough, node of ~A isn't nil!"),self]],['ob$remove-all',self],[yloop,[yfor,obname,in,['obr-obnames',self]],[ydo,['ob$remove-name',self,obname]]],[setq,'*obs*',['delq!',self,'*obs*']]])
wl:lambda_def(defun, u_ob_c36_destroy, f_u_ob_c36_destroy, [u_self], [[if, [u_obr_node, u_self], [error, '$ARRAY'([*], claz_base_character, "Sure enough, node of ~A isn't nil!"), u_self]], [u_ob_c36_remove_all, u_self], [u_yloop, [u_yfor, u_obname, u_in, [u_obr_obnames, u_self]], [u_ydo, [u_ob_c36_remove_name, u_self, u_obname]]], [setq, u_xx_obs_xx, [u_delq_c33, u_self, u_xx_obs_xx]]]).
wl:arglist_info(u_ob_c36_destroy, f_u_ob_c36_destroy, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_destroy).

/*

### Compiled:  `U::OB$DESTROY` 
*/
f_u_ob_c36_destroy(Self, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_self, Self)|Env],
	get_var(AEnv, u_self, Self_Get),
	f_u_obr_node(Self_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(AEnv, u_self, Self_Get10),
	    cl_error(
		     [ '$ARRAY'([*],
				claz_base_character,
				"Sure enough, node of ~A isn't nil!"),
		       Self_Get10
		     ],
		     TrueResult),
	    _152155690=TrueResult
	;   _152155690=[]
	),
	get_var(AEnv, u_self, Self_Get12),
	f_u_ob_c36_remove_all(Self_Get12, Remove_all_Ret),
	f_u_yloop(
		  [ [u_yfor, u_obname, u_in, [u_obr_obnames, u_self]],
		    [u_ydo, [u_ob_c36_remove_name, u_self, u_obname]]
		  ],
		  Yloop_Ret),
	f_u_delq_c33(u_self, u_xx_obs_xx, Xx_obs_xx),
	set_var(AEnv, u_xx_obs_xx, Xx_obs_xx),
	Xx_obs_xx=FnResult.
:- set_opv(f_u_ob_c36_destroy, classof, claz_function),
   set_opv(u_ob_c36_destroy, compile_as, kw_function),
   set_opv(u_ob_c36_destroy, function, f_u_ob_c36_destroy),
   _Ignored4=u_ob_c36_destroy.
/*
:- side_effect(assert_lsp(u_ob_c36_destroy,
			  wl:lambda_def(defun, u_ob_c36_destroy, f_u_ob_c36_destroy, [u_self], [[if, [u_obr_node, u_self], [error, '$ARRAY'([*], claz_base_character, "Sure enough, node of ~A isn't nil!"), u_self]], [u_ob_c36_remove_all, u_self], [u_yloop, [u_yfor, u_obname, u_in, [u_obr_obnames, u_self]], [u_ydo, [u_ob_c36_remove_name, u_self, u_obname]]], [setq, u_xx_obs_xx, [u_delq_c33, u_self, u_xx_obs_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_destroy,
			  wl:arglist_info(u_ob_c36_destroy, f_u_ob_c36_destroy, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_destroy,
			  wl:init_args(exact_only, f_u_ob_c36_destroy))).
*/
/*
*/
/*
 Inverse slots
*/
/*
*/
/*
(setq *inverse-slot-list* nil)

;
; A primary slot is any that does not have an inverse or one that
; was explicitly defined as a primary slot in a primary/secondary
; declaration.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2418 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*inverse-slot-list*',[]])
:- set_var(AEnv, setq, u_xx_inverse_slot_list_xx, []).
/*
*/
/*
 A primary slot is any that does not have an inverse or one that
*/
/*
 was explicitly defined as a primary slot in a primary/secondary
*/
/*
 declaration.
*/
/*
*/
/*
(defun primary-slot? (slot-name)
  (or (not (not (assq slot-name *inverse-slot-list*)))
      (yloop (initial (rest *inverse-slot-list*)
                     (result nil))
            (ywhile rest)
            (yuntil result)
            (ydo (if (eq? slot-name (cadr (car rest)))
                    (setq result (car (car rest))))
                (setq rest (cdr rest)))
            (yresult (null? result)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:2601 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'primary-slot?',['slot-name'],[or,[not,[not,[assq,'slot-name','*inverse-slot-list*']]],[yloop,[initial,[rest,'*inverse-slot-list*'],[result,[]]],[ywhile,rest],[yuntil,result],[ydo,[if,['eq?','slot-name',[cadr,[car,rest]]],[setq,result,[car,[car,rest]]]],[setq,rest,[cdr,rest]]],[yresult,['null?',result]]]]])
wl:lambda_def(defun, u_primary_slot_c63, f_u_primary_slot_c63, [u_slot_name], [[or, [not, [not, [ext_assq, u_slot_name, u_xx_inverse_slot_list_xx]]], [u_yloop, [u_initial, [rest, u_xx_inverse_slot_list_xx], [u_result, []]], [u_ywhile, rest], [u_yuntil, u_result], [u_ydo, [if, [u_eq_c63, u_slot_name, [cadr, [car, rest]]], [setq, u_result, [car, [car, rest]]]], [setq, rest, [cdr, rest]]], [u_yresult, [u_null_c63, u_result]]]]]).
wl:arglist_info(u_primary_slot_c63, f_u_primary_slot_c63, [u_slot_name], arginfo{all:[u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_slot_name], opt:0, req:[u_slot_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_primary_slot_c63).

/*

### Compiled:  `U::PRIMARY-SLOT?` 
*/
f_u_primary_slot_c63(Slot_name, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_slot_name, Slot_name)|Env],
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
   _Ignored4=u_primary_slot_c63.
/*
:- side_effect(assert_lsp(u_primary_slot_c63,
			  wl:lambda_def(defun, u_primary_slot_c63, f_u_primary_slot_c63, [u_slot_name], [[or, [not, [not, [ext_assq, u_slot_name, u_xx_inverse_slot_list_xx]]], [u_yloop, [u_initial, [rest, u_xx_inverse_slot_list_xx], [u_result, []]], [u_ywhile, rest], [u_yuntil, u_result], [u_ydo, [if, [u_eq_c63, u_slot_name, [cadr, [car, rest]]], [setq, u_result, [car, [car, rest]]]], [setq, rest, [cdr, rest]]], [u_yresult, [u_null_c63, u_result]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_primary_slot_c63,
			  wl:arglist_info(u_primary_slot_c63, f_u_primary_slot_c63, [u_slot_name], arginfo{all:[u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_slot_name], opt:0, req:[u_slot_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_primary_slot_c63,
			  wl:init_args(exact_only, f_u_primary_slot_c63))).
*/
/*
(defun inverse-slot (slot-name)
  (let ((found (assq slot-name *inverse-slot-list*)))
    (if found
        (cadr found)
        (yloop (initial (rest *inverse-slot-list*)
                       (result nil))
              (ywhile rest)
              (yuntil result)
              (ydo (if (eq? slot-name (cadr (car rest)))
                      (setq result (car (car rest))))
                  (setq rest (cdr rest)))
              (yresult result)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3015 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'inverse-slot',['slot-name'],[let,[[found,[assq,'slot-name','*inverse-slot-list*']]],[if,found,[cadr,found],[yloop,[initial,[rest,'*inverse-slot-list*'],[result,[]]],[ywhile,rest],[yuntil,result],[ydo,[if,['eq?','slot-name',[cadr,[car,rest]]],[setq,result,[car,[car,rest]]]],[setq,rest,[cdr,rest]]],[yresult,result]]]]])
wl:lambda_def(defun, u_inverse_slot, f_u_inverse_slot, [u_slot_name], [[let, [[u_found, [ext_assq, u_slot_name, u_xx_inverse_slot_list_xx]]], [if, u_found, [cadr, u_found], [u_yloop, [u_initial, [rest, u_xx_inverse_slot_list_xx], [u_result, []]], [u_ywhile, rest], [u_yuntil, u_result], [u_ydo, [if, [u_eq_c63, u_slot_name, [cadr, [car, rest]]], [setq, u_result, [car, [car, rest]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result]]]]]).
wl:arglist_info(u_inverse_slot, f_u_inverse_slot, [u_slot_name], arginfo{all:[u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_slot_name], opt:0, req:[u_slot_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_inverse_slot).

/*

### Compiled:  `U::INVERSE-SLOT` 
*/
f_u_inverse_slot(Slot_name, FnResult) :-
	nop(global_env(Env)),
	Env19=[bv(u_slot_name, Slot_name)|Env],
	f_ext_assq(u_slot_name, u_xx_inverse_slot_list_xx, Found_Init),
	LEnv=[bv(u_found, Found_Init)|Env19],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get14),
	    cl_cadr(Found_Get14, TrueResult),
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
   _Ignored4=u_inverse_slot.
/*
:- side_effect(assert_lsp(u_inverse_slot,
			  wl:lambda_def(defun, u_inverse_slot, f_u_inverse_slot, [u_slot_name], [[let, [[u_found, [ext_assq, u_slot_name, u_xx_inverse_slot_list_xx]]], [if, u_found, [cadr, u_found], [u_yloop, [u_initial, [rest, u_xx_inverse_slot_list_xx], [u_result, []]], [u_ywhile, rest], [u_yuntil, u_result], [u_ydo, [if, [u_eq_c63, u_slot_name, [cadr, [car, rest]]], [setq, u_result, [car, [car, rest]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_inverse_slot,
			  wl:arglist_info(u_inverse_slot, f_u_inverse_slot, [u_slot_name], arginfo{all:[u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_slot_name], opt:0, req:[u_slot_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_inverse_slot,
			  wl:init_args(exact_only, f_u_inverse_slot))).
*/
/*
(defun ob$decl-has-inverse (primary-slot-name)
  (ob$decl-inverses primary-slot-name
                          (string->symbol
                           (string-append
                            (symbol->string primary-slot-name)
                            "-OF"))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3471 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$decl-has-inverse',['primary-slot-name'],['ob$decl-inverses','primary-slot-name',['string->symbol',['string-append',['symbol->string','primary-slot-name'],'$STRING'("-OF")]]]])
wl:lambda_def(defun, u_ob_c36_decl_has_inverse, f_u_ob_c36_decl_has_inverse, [u_primary_slot_name], [[u_ob_c36_decl_inverses, u_primary_slot_name, [u_string_c62_symbol, [u_string_append, [u_symbol_c62_string, u_primary_slot_name], '$ARRAY'([*], claz_base_character, "-OF")]]]]).
wl:arglist_info(u_ob_c36_decl_has_inverse, f_u_ob_c36_decl_has_inverse, [u_primary_slot_name], arginfo{all:[u_primary_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_primary_slot_name], opt:0, req:[u_primary_slot_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_decl_has_inverse).

/*

### Compiled:  `U::OB$DECL-HAS-INVERSE` 
*/
f_u_ob_c36_decl_has_inverse(Primary_slot_name, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_primary_slot_name, Primary_slot_name)|Env],
	get_var(Env10, u_primary_slot_name, Primary_slot_name_Get),
	f_u_string_c62_symbol(
			      [ u_string_append,
				[u_symbol_c62_string, u_primary_slot_name],
				'$ARRAY'([*], claz_base_character, "-OF")
			      ],
			      C62_symbol_Ret),
	f_u_ob_c36_decl_inverses(Primary_slot_name_Get,
				 C62_symbol_Ret,
				 Decl_inverses_Ret),
	Decl_inverses_Ret=FnResult.
:- set_opv(f_u_ob_c36_decl_has_inverse, classof, claz_function),
   set_opv(u_ob_c36_decl_has_inverse, compile_as, kw_function),
   set_opv(u_ob_c36_decl_has_inverse, function, f_u_ob_c36_decl_has_inverse),
   _Ignored4=u_ob_c36_decl_has_inverse.
/*
:- side_effect(assert_lsp(u_ob_c36_decl_has_inverse,
			  wl:lambda_def(defun, u_ob_c36_decl_has_inverse, f_u_ob_c36_decl_has_inverse, [u_primary_slot_name], [[u_ob_c36_decl_inverses, u_primary_slot_name, [u_string_c62_symbol, [u_string_append, [u_symbol_c62_string, u_primary_slot_name], '$ARRAY'([*], claz_base_character, "-OF")]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_decl_has_inverse,
			  wl:arglist_info(u_ob_c36_decl_has_inverse, f_u_ob_c36_decl_has_inverse, [u_primary_slot_name], arginfo{all:[u_primary_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_primary_slot_name], opt:0, req:[u_primary_slot_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_decl_has_inverse,
			  wl:init_args(exact_only, f_u_ob_c36_decl_has_inverse))).
*/
/*
(defun used-as-primary? (primary)
  (and (primary-slot? primary)
       (any? (lambda (ob)
              (any? (lambda (slot) (eq? primary (slots-name slot)))
                    (ob$pairs ob)))
             *obs*)))

;
; (ob$decl-inverses primary-slot-name secondary-slot-name):
;
; Declare a primary-slot/secondary-slot pair. Simply warns if the
; declaration has already been performed.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:3742 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'used-as-primary?',[primary],[and,['primary-slot?',primary],['any?',[lambda,[ob],['any?',[lambda,[slot],['eq?',primary,['slots-name',slot]]],['ob$pairs',ob]]],'*obs*']]])
wl:lambda_def(defun, u_used_as_primary_c63, f_u_used_as_primary_c63, [u_primary], [[and, [u_primary_slot_c63, u_primary], [u_any_c63, [lambda, [u_ob], [u_any_c63, [lambda, [u_slot], [u_eq_c63, u_primary, [u_slots_name, u_slot]]], [u_ob_c36_pairs, u_ob]]], u_xx_obs_xx]]]).
wl:arglist_info(u_used_as_primary_c63, f_u_used_as_primary_c63, [u_primary], arginfo{all:[u_primary], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_primary], opt:0, req:[u_primary], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_used_as_primary_c63).

/*

### Compiled:  `U::USED-AS-PRIMARY?` 
*/
f_u_used_as_primary_c63(Primary, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(u_primary, Primary)|Env],
	get_var(Env13, u_primary, Primary_Get),
	f_u_primary_slot_c63(Primary_Get, IFTEST),
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
   _Ignored4=u_used_as_primary_c63.
/*
:- side_effect(assert_lsp(u_used_as_primary_c63,
			  wl:lambda_def(defun, u_used_as_primary_c63, f_u_used_as_primary_c63, [u_primary], [[and, [u_primary_slot_c63, u_primary], [u_any_c63, [lambda, [u_ob], [u_any_c63, [lambda, [u_slot], [u_eq_c63, u_primary, [u_slots_name, u_slot]]], [u_ob_c36_pairs, u_ob]]], u_xx_obs_xx]]]))).
*/
/*
:- side_effect(assert_lsp(u_used_as_primary_c63,
			  wl:arglist_info(u_used_as_primary_c63, f_u_used_as_primary_c63, [u_primary], arginfo{all:[u_primary], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_primary], opt:0, req:[u_primary], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_used_as_primary_c63,
			  wl:init_args(exact_only, f_u_used_as_primary_c63))).
*/
/*
*/
/*
 (ob$decl-inverses primary-slot-name secondary-slot-name):
*/
/*
*/
/*
 Declare a primary-slot/secondary-slot pair. Simply warns if the
*/
/*
 declaration has already been performed.
*/
/*
*/
/*
(defun ob$decl-inverses (primary secondary)
 (if (not (used-as-primary? secondary))
  (if (and (primary-slot? primary)
           (eq? (inverse-slot primary) secondary))
      (ndbg *gate-dbg* ob-warn
       "Warning: Duplicate primary/secondary declaration "(defun ob$decl-inverses (primary secondary)\n (if (not (used-as-primary? secondary))\n  (if (and (primary-slot? primary)\n           (eq? (inverse-slot primary) secondary))\n      (ndbg *gate-dbg* ob-warn\n       \"Warning: Duplicate primary/secondary declaration ~A ~A~%\"\n       primary secondary)\n      (cond\n       ((inverse-slot primary)\n        (error \"~A already has an inverse of ~A.\" primary\n                                                  (inverse-slot primary)))\n       ((inverse-slot secondary)\n        (error \"~A already has an inverse of ~A.\" primary\n                                                  (inverse-slot secondary)))\n       (else (setq *inverse-slot-list* (cons (list primary secondary)\n                                            *inverse-slot-list*)) t)))\n  (progn\n   (format *gate-output*\n           \"~&~A has already been used as a primary slot name.~%\"\n           secondary)\n   (format *gate-output* \"Declaration not performed.~%\"))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:4134 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$decl-inverses',[primary,secondary],[if,[not,['used-as-primary?',secondary]],[if,[and,['primary-slot?',primary],['eq?',['inverse-slot',primary],secondary]],[ndbg,'*gate-dbg*','ob-warn','$STRING'("Warning: Duplicate primary/secondary declaration ~A ~A~%"),primary,secondary],[cond,[['inverse-slot',primary],[error,'$STRING'("~A already has an inverse of ~A."),primary,['inverse-slot',primary]]],[['inverse-slot',secondary],[error,'$STRING'("~A already has an inverse of ~A."),primary,['inverse-slot',secondary]]],[else,[setq,'*inverse-slot-list*',[cons,[list,primary,secondary],'*inverse-slot-list*']],t]]],[progn,[format,'*gate-output*','$STRING'("~&~A has already been used as a primary slot name.~%"),secondary],[format,'*gate-output*','$STRING'("Declaration not performed.~%")]]]])
wl:lambda_def(defun, u_ob_c36_decl_inverses, f_u_ob_c36_decl_inverses, [u_primary, u_secondary], [[if, [not, [u_used_as_primary_c63, u_secondary]], [if, [and, [u_primary_slot_c63, u_primary], [u_eq_c63, [u_inverse_slot, u_primary], u_secondary]], [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Duplicate primary/secondary declaration ~A ~A~%"), u_primary, u_secondary], [cond, [[u_inverse_slot, u_primary], [error, '$ARRAY'([*], claz_base_character, "~A already has an inverse of ~A."), u_primary, [u_inverse_slot, u_primary]]], [[u_inverse_slot, u_secondary], [error, '$ARRAY'([*], claz_base_character, "~A already has an inverse of ~A."), u_primary, [u_inverse_slot, u_secondary]]], [u_else, [setq, u_xx_inverse_slot_list_xx, [cons, [list, u_primary, u_secondary], u_xx_inverse_slot_list_xx]], t]]], [progn, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&~A has already been used as a primary slot name.~%"), u_secondary], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Declaration not performed.~%")]]]]).
wl:arglist_info(u_ob_c36_decl_inverses, f_u_ob_c36_decl_inverses, [u_primary, u_secondary], arginfo{all:[u_primary, u_secondary], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_primary, u_secondary], opt:0, req:[u_primary, u_secondary], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_decl_inverses).

/*

### Compiled:  `U::OB$DECL-INVERSES` 
*/
f_u_ob_c36_decl_inverses(Primary, Secondary, ElseResult36) :-
	nop(global_env(Env)),
	AEnv=[bv(u_primary, Primary), bv(u_secondary, Secondary)|Env],
	get_var(AEnv, u_secondary, Secondary_Get),
	f_u_used_as_primary_c63(Secondary_Get, PredArgResult),
	(   PredArgResult==[]
	->  get_var(AEnv, u_primary, Primary_Get),
	    f_u_primary_slot_c63(Primary_Get, IFTEST13),
	    (   IFTEST13\==[]
	    ->  f_u_eq_c63([u_inverse_slot, u_primary], u_secondary, TrueResult),
		IFTEST11=TrueResult
	    ;   IFTEST11=[]
	    ),
	    (   IFTEST11\==[]
	    ->  f_u_ndbg(u_xx_gate_dbg_xx,
			 u_ob_warn,
			 
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "Warning: Duplicate primary/secondary declaration ~A ~A~%"),
			   u_primary,
			   u_secondary
			 ],
			 TrueResult39),
		ElseResult36=TrueResult39
	    ;   get_var(AEnv, u_primary, Primary_Get19),
		f_u_inverse_slot(Primary_Get19, IFTEST17),
		(   IFTEST17\==[]
		->  get_var(AEnv, u_primary, Primary_Get20),
		    f_u_inverse_slot(Primary_Get20, Inverse_slot_Ret),
		    cl_error(
			     [ '$ARRAY'([*],
					claz_base_character,
					"~A already has an inverse of ~A."),
			       Primary_Get20,
			       Inverse_slot_Ret
			     ],
			     TrueResult37),
		    ElseResult36=TrueResult37
		;   get_var(AEnv, u_secondary, Secondary_Get24),
		    f_u_inverse_slot(Secondary_Get24, IFTEST22),
		    (   IFTEST22\==[]
		    ->  get_var(AEnv, u_primary, Primary_Get25),
			get_var(AEnv, u_secondary, Secondary_Get26),
			f_u_inverse_slot(Secondary_Get26, Inverse_slot_Ret52),
			cl_error(
				 [ '$ARRAY'([*],
					    claz_base_character,
					    "~A already has an inverse of ~A."),
				   Primary_Get25,
				   Inverse_slot_Ret52
				 ],
				 TrueResult35),
			ElseResult36=TrueResult35
		    ;   get_var(AEnv, u_else, IFTEST27),
			(   IFTEST27\==[]
			->  get_var(AEnv, u_primary, Primary_Get31),
			    get_var(AEnv, u_secondary, Secondary_Get32),
			    CAR=[Primary_Get31, Secondary_Get32],
			    get_var(AEnv,
				    u_xx_inverse_slot_list_xx,
				    Xx_inverse_slot_list_xx_Get),
			    Xx_inverse_slot_list_xx=[CAR|Xx_inverse_slot_list_xx_Get],
			    set_var(AEnv,
				    u_xx_inverse_slot_list_xx,
				    Xx_inverse_slot_list_xx),
			    ElseResult36=t
			;   ElseResult36=[]
			)
		    )
		)
	    )
	;   get_var(AEnv, u_secondary, Secondary_Get42),
	    get_var(AEnv, u_xx_gate_output_xx, Xx_gate_output_xx_Get),
	    cl_format(
		      [ Xx_gate_output_xx_Get,
			'$ARRAY'([*],
				 claz_base_character,
				 "~&~A has already been used as a primary slot name.~%"),
			Secondary_Get42
		      ],
		      Format_Ret),
	    get_var(AEnv, u_xx_gate_output_xx, Xx_gate_output_xx_Get43),
	    cl_format(
		      [ Xx_gate_output_xx_Get43,
			'$ARRAY'([*],
				 claz_base_character,
				 "Declaration not performed.~%")
		      ],
		      ElseResult45),
	    ElseResult36=ElseResult45
	).
:- set_opv(f_u_ob_c36_decl_inverses, classof, claz_function),
   set_opv(u_ob_c36_decl_inverses, compile_as, kw_function),
   set_opv(u_ob_c36_decl_inverses, function, f_u_ob_c36_decl_inverses),
   _Ignored4=u_ob_c36_decl_inverses.
/*
:- side_effect(assert_lsp(u_ob_c36_decl_inverses,
			  wl:lambda_def(defun, u_ob_c36_decl_inverses, f_u_ob_c36_decl_inverses, [u_primary, u_secondary], [[if, [not, [u_used_as_primary_c63, u_secondary]], [if, [and, [u_primary_slot_c63, u_primary], [u_eq_c63, [u_inverse_slot, u_primary], u_secondary]], [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Duplicate primary/secondary declaration ~A ~A~%"), u_primary, u_secondary], [cond, [[u_inverse_slot, u_primary], [error, '$ARRAY'([*], claz_base_character, "~A already has an inverse of ~A."), u_primary, [u_inverse_slot, u_primary]]], [[u_inverse_slot, u_secondary], [error, '$ARRAY'([*], claz_base_character, "~A already has an inverse of ~A."), u_primary, [u_inverse_slot, u_secondary]]], [u_else, [setq, u_xx_inverse_slot_list_xx, [cons, [list, u_primary, u_secondary], u_xx_inverse_slot_list_xx]], t]]], [progn, [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "~&~A has already been used as a primary slot name.~%"), u_secondary], [format, u_xx_gate_output_xx, '$ARRAY'([*], claz_base_character, "Declaration not performed.~%")]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_decl_inverses,
			  wl:arglist_info(u_ob_c36_decl_inverses, f_u_ob_c36_decl_inverses, [u_primary, u_secondary], arginfo{all:[u_primary, u_secondary], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_primary, u_secondary], opt:0, req:[u_primary, u_secondary], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_decl_inverses,
			  wl:init_args(exact_only, f_u_ob_c36_decl_inverses))).
*/
/*
(defun decl-primary-secondaries (lst)
 (map 'list
  (lambda (pair) (ob$decl-inverses (car pair) (cadr pair)))
  lst))

;
; This function is not perfect. If one desires duplicate pairs, this
; will not create them.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5095 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'decl-primary-secondaries',[lst],[map,[quote,list],[lambda,[pair],['ob$decl-inverses',[car,pair],[cadr,pair]]],lst]])
wl:lambda_def(defun, u_decl_primary_secondaries, f_u_decl_primary_secondaries, [u_lst], [[map, [quote, list], [lambda, [u_pair], [u_ob_c36_decl_inverses, [car, u_pair], [cadr, u_pair]]], u_lst]]).
wl:arglist_info(u_decl_primary_secondaries, f_u_decl_primary_secondaries, [u_lst], arginfo{all:[u_lst], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst], opt:0, req:[u_lst], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_decl_primary_secondaries).

/*

### Compiled:  `U::DECL-PRIMARY-SECONDARIES` 
*/
f_u_decl_primary_secondaries(Lst, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_lst, Lst)|Env],
	Lambda=closure([ClosureEnvironment|Env15], LResult, [u_pair],  (get_var(ClosureEnvironment, u_pair, Pair_Get), cl_car(Pair_Get, Decl_inverses_Param), get_var(ClosureEnvironment, u_pair, Pair_Get8), cl_cadr(Pair_Get8, Cadr_Ret), f_u_ob_c36_decl_inverses(Decl_inverses_Param, Cadr_Ret, LResult))),
	get_var(Env15, u_lst, Lst_Get),
	cl_map(list, Lambda, Lst_Get, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_decl_primary_secondaries, classof, claz_function),
   set_opv(u_decl_primary_secondaries, compile_as, kw_function),
   set_opv(u_decl_primary_secondaries, function, f_u_decl_primary_secondaries),
   _Ignored4=u_decl_primary_secondaries.
/*
:- side_effect(assert_lsp(u_decl_primary_secondaries,
			  wl:lambda_def(defun, u_decl_primary_secondaries, f_u_decl_primary_secondaries, [u_lst], [[map, [quote, list], [lambda, [u_pair], [u_ob_c36_decl_inverses, [car, u_pair], [cadr, u_pair]]], u_lst]]))).
*/
/*
:- side_effect(assert_lsp(u_decl_primary_secondaries,
			  wl:arglist_info(u_decl_primary_secondaries, f_u_decl_primary_secondaries, [u_lst], arginfo{all:[u_lst], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_lst], opt:0, req:[u_lst], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_decl_primary_secondaries,
			  wl:init_args(exact_only, f_u_decl_primary_secondaries))).
*/
/*
*/
/*
 This function is not perfect. If one desires duplicate pairs, this
*/
/*
 will not create them.
*/
/*
*/
/*
(defun enforce-inverses ()
 (map 'list
  (lambda (ob)
    (if t
        (map 'list
         (lambda (slot)
           (let ((inv (inverse-slot (slots-name slot))))
             (if (and inv
                      (ob? (slots-value slot))
                      (null? (memq? ob (ob$gets
                                         (slots-value slot) inv))))
                 (ob$basic-add
                  (slots-value slot) inv ob))))
         (ob$pairs ob))))
  *obs*))

;
; (ob$add-name ob obname):
;
; Associate another obname with the ob. This new obname may be
; used to refer to the ob, as may any obnames previously defined.
;
; Todo: a separate *obnames* for non "OB." names would speed things up.
; Alternatively, use hash tables.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:5311 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'enforce-inverses',[],[map,[quote,list],[lambda,[ob],[if,t,[map,[quote,list],[lambda,[slot],[let,[[inv,['inverse-slot',['slots-name',slot]]]],[if,[and,inv,['ob?',['slots-value',slot]],['null?',['memq?',ob,['ob$gets',['slots-value',slot],inv]]]],['ob$basic-add',['slots-value',slot],inv,ob]]]],['ob$pairs',ob]]]],'*obs*']])
wl:lambda_def(defun, u_enforce_inverses, f_u_enforce_inverses, [], [[map, [quote, list], [lambda, [u_ob], [if, t, [map, [quote, list], [lambda, [u_slot], [let, [[u_inv, [u_inverse_slot, [u_slots_name, u_slot]]]], [if, [and, u_inv, [u_ob_c63, [u_slots_value, u_slot]], [u_null_c63, [u_memq_c63, u_ob, [u_ob_c36_gets, [u_slots_value, u_slot], u_inv]]]], [u_ob_c36_basic_add, [u_slots_value, u_slot], u_inv, u_ob]]]], [u_ob_c36_pairs, u_ob]]]], u_xx_obs_xx]]).
wl:arglist_info(u_enforce_inverses, f_u_enforce_inverses, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_enforce_inverses).

/*

### Compiled:  `U::ENFORCE-INVERSES` 
*/
f_u_enforce_inverses(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	Lambda31=closure([ClosureEnvironment32|GEnv], LResult30, [u_ob],  (t\==[]->Lambda=closure([ClosureEnvironment|ClosureEnvironment32], LetResult, [u_slot],  (f_u_slots_name(u_slot, Inverse_slot_Param), f_u_inverse_slot(Inverse_slot_Param, Inv_Init), LEnv=[bv(u_inv, Inv_Init)|ClosureEnvironment], get_var(LEnv, u_inv, IFTEST15), (IFTEST15\==[]->f_u_ob_c63([u_slots_value, u_slot], IFTEST18), (IFTEST18\==[]->f_u_null_c63([u_memq_c63, u_ob, [u_ob_c36_gets, [u_slots_value, u_slot], u_inv]], TrueResult), TrueResult21=TrueResult;TrueResult21=[]), IFTEST13=TrueResult21;IFTEST13=[]), (IFTEST13\==[]->f_u_slots_value(u_slot, Basic_add_Param), get_var(LEnv, u_inv, Inv_Get22), get_var(LEnv, u_ob, Ob_Get), f_u_ob_c36_basic_add(Basic_add_Param, Inv_Get22, Ob_Get, TrueResult24), LetResult=TrueResult24;LetResult=[]))), get_var(ClosureEnvironment32, u_ob, Ob_Get28), f_u_ob_c36_pairs(Ob_Get28, C36_pairs_Ret), cl_map(list, Lambda, C36_pairs_Ret, TrueResult29), LResult30=TrueResult29;LResult30=[])),
	get_var(GEnv, u_xx_obs_xx, Xx_obs_xx_Get),
	cl_map(list, Lambda31, Xx_obs_xx_Get, Map_Ret),
	Map_Ret=FnResult.
:- set_opv(f_u_enforce_inverses, classof, claz_function),
   set_opv(u_enforce_inverses, compile_as, kw_function),
   set_opv(u_enforce_inverses, function, f_u_enforce_inverses),
   _Ignored4=u_enforce_inverses.
/*
:- side_effect(assert_lsp(u_enforce_inverses,
			  wl:lambda_def(defun, u_enforce_inverses, f_u_enforce_inverses, [], [[map, [quote, list], [lambda, [u_ob], [if, t, [map, [quote, list], [lambda, [u_slot], [let, [[u_inv, [u_inverse_slot, [u_slots_name, u_slot]]]], [if, [and, u_inv, [u_ob_c63, [u_slots_value, u_slot]], [u_null_c63, [u_memq_c63, u_ob, [u_ob_c36_gets, [u_slots_value, u_slot], u_inv]]]], [u_ob_c36_basic_add, [u_slots_value, u_slot], u_inv, u_ob]]]], [u_ob_c36_pairs, u_ob]]]], u_xx_obs_xx]]))).
*/
/*
:- side_effect(assert_lsp(u_enforce_inverses,
			  wl:arglist_info(u_enforce_inverses, f_u_enforce_inverses, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_enforce_inverses,
			  wl:init_args(exact_only, f_u_enforce_inverses))).
*/
/*
*/
/*
 (ob$add-name ob obname):
*/
/*
*/
/*
 Associate another obname with the ob. This new obname may be
*/
/*
 used to refer to the ob, as may any obnames previously defined.
*/
/*
*/
/*
 Todo: a separate *obnames* for non "OB." names would speed things up.
*/
/*
 Alternatively, use hash tables.
*/
/*
*/
/*
(defun ob$add-name (self obname)
  (if (not (memq? obname (obr-obnames self)))
      (progn
       (if (ob? obname)
           (progn
            (setq obname (ob$name obname))
            (ndbg *gate-dbg* ob-warn
                  "Warning: Probable obname redefinition."(defun ob$add-name (self obname)\n  (if (not (memq? obname (obr-obnames self)))\n      (progn\n       (if (ob? obname)\n           (progn\n            (setq obname (ob$name obname))\n            (ndbg *gate-dbg* ob-warn\n                  \"Warning: Probable obname redefinition.~%\")))\n       (if (not (symbol? obname))\n           (setq obname (error \"ob$add-name: ~A not symbol\" obname)))\n       (yloop\n        (ywhile (assq obname *obnames*))\n        (ydo\n         (let ((new-obname\n                (string->symbol\n                 (string-append\n                  (symbol->string obname) \"X\"))))\n              (ndbg *gate-dbg* ob-warn\n                   \"Warning: Obname ~A already used--using ~A instead.~%\"\n                   obname new-obname)\n              (setq obname new-obname))))\n       (setq *obnames* (cons (list obname self) *obnames*))\n       (set-obr-obnames self (cons obname (obr-obnames self)))\n       obname)\n      (progn\n      (ndbg *gate-dbg* ob-warn\n             \"Warning: Obname ~A already in effect for specified ob.~%\"\n             obname)\n       obname)))\n\n; This assumes obname is already determined to be unique. We assume that\n; we are able to generate unique \"OB.\" names above. This assumes the\n; user does not create such names also.\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:6050 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$add-name',[self,obname],[if,[not,['memq?',obname,['obr-obnames',self]]],[progn,[if,['ob?',obname],[progn,[setq,obname,['ob$name',obname]],[ndbg,'*gate-dbg*','ob-warn','$STRING'("Warning: Probable obname redefinition.~%")]]],[if,[not,['symbol?',obname]],[setq,obname,[error,'$STRING'("ob$add-name: ~A not symbol"),obname]]],[yloop,[ywhile,[assq,obname,'*obnames*']],[ydo,[let,[['new-obname',['string->symbol',['string-append',['symbol->string',obname],'$STRING'("X")]]]],[ndbg,'*gate-dbg*','ob-warn','$STRING'("Warning: Obname ~A already used--using ~A instead.~%"),obname,'new-obname'],[setq,obname,'new-obname']]]],[setq,'*obnames*',[cons,[list,obname,self],'*obnames*']],['set-obr-obnames',self,[cons,obname,['obr-obnames',self]]],obname],[progn,[ndbg,'*gate-dbg*','ob-warn','$STRING'("Warning: Obname ~A already in effect for specified ob.~%"),obname],obname]]])
wl:lambda_def(defun, u_ob_c36_add_name, f_u_ob_c36_add_name, [u_self, u_obname], [[if, [not, [u_memq_c63, u_obname, [u_obr_obnames, u_self]]], [progn, [if, [u_ob_c63, u_obname], [progn, [setq, u_obname, [u_ob_c36_name, u_obname]], [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Probable obname redefinition.~%")]]], [if, [not, [u_symbol_c63, u_obname]], [setq, u_obname, [error, '$ARRAY'([*], claz_base_character, "ob$add-name: ~A not symbol"), u_obname]]], [u_yloop, [u_ywhile, [ext_assq, u_obname, u_xx_obnames_xx]], [u_ydo, [let, [[u_new_obname, [u_string_c62_symbol, [u_string_append, [u_symbol_c62_string, u_obname], '$ARRAY'([*], claz_base_character, "X")]]]], [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Obname ~A already used--using ~A instead.~%"), u_obname, u_new_obname], [setq, u_obname, u_new_obname]]]], [setq, u_xx_obnames_xx, [cons, [list, u_obname, u_self], u_xx_obnames_xx]], [u_set_obr_obnames, u_self, [cons, u_obname, [u_obr_obnames, u_self]]], u_obname], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Obname ~A already in effect for specified ob.~%"), u_obname], u_obname]]]).
wl:arglist_info(u_ob_c36_add_name, f_u_ob_c36_add_name, [u_self, u_obname], arginfo{all:[u_self, u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_obname], opt:0, req:[u_self, u_obname], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_add_name).

/*

### Compiled:  `U::OB$ADD-NAME` 
*/
f_u_ob_c36_add_name(Self, Obname, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_self, Self), bv(u_obname, Obname)|Env],
	f_u_memq_c63(u_obname, [u_obr_obnames, u_self], PredArgResult),
	(   PredArgResult==[]
	->  f_u_ob_c63(u_obname, IFTEST10),
	    (   IFTEST10\==[]
	    ->  get_var(AEnv, u_obname, Obname_Get),
		f_u_ob_c36_name(Obname_Get, Obname31),
		set_var(AEnv, u_obname, Obname31),
		f_u_ndbg(u_xx_gate_dbg_xx,
			 u_ob_warn,
			 
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "Warning: Probable obname redefinition.~%")
			 ],
			 TrueResult),
		_163471870=TrueResult
	    ;   _163471870=[]
	    ),
	    f_u_symbol_c63(u_obname, PredArgResult17),
	    (   PredArgResult17==[]
	    ->  get_var(AEnv, u_obname, Obname_Get18),
		cl_error(
			 [ '$ARRAY'([*],
				    claz_base_character,
				    "ob$add-name: ~A not symbol"),
			   Obname_Get18
			 ],
			 TrueResult19),
		set_var(AEnv, u_obname, TrueResult19),
		_163535102=TrueResult19
	    ;   _163535102=[]
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
				    '$ARRAY'([*], claz_base_character, "X")
				  ]
				]
			      ]
			    ],
			    
			    [ u_ndbg,
			      u_xx_gate_dbg_xx,
			      u_ob_warn,
			      '$ARRAY'([*],
				       claz_base_character,
				       "Warning: Obname ~A already used--using ~A instead.~%"),
			      u_obname,
			      u_new_obname
			    ],
			    [setq, u_obname, u_new_obname]
			  ]
			]
		      ],
		      Yloop_Ret),
	    get_var(AEnv, u_obname, Obname_Get20),
	    get_var(AEnv, u_self, Self_Get),
	    CAR=[Obname_Get20, Self_Get],
	    get_var(AEnv, u_xx_obnames_xx, Xx_obnames_xx_Get),
	    Xx_obnames_xx=[CAR|Xx_obnames_xx_Get],
	    set_var(AEnv, u_xx_obnames_xx, Xx_obnames_xx),
	    f_u_set_obr_obnames(u_self,
				[cons, u_obname, [u_obr_obnames, u_self]],
				Obr_obnames_Ret),
	    get_var(AEnv, u_obname, Obname_Get23),
	    FnResult=Obname_Get23
	;   f_u_ndbg(u_xx_gate_dbg_xx,
		     u_ob_warn,
		     
		     [ '$ARRAY'([*],
				claz_base_character,
				"Warning: Obname ~A already in effect for specified ob.~%"),
		       u_obname
		     ],
		     Ndbg_Ret),
	    get_var(AEnv, u_obname, Obname_Get24),
	    FnResult=Obname_Get24
	).
:- set_opv(f_u_ob_c36_add_name, classof, claz_function),
   set_opv(u_ob_c36_add_name, compile_as, kw_function),
   set_opv(u_ob_c36_add_name, function, f_u_ob_c36_add_name),
   _Ignored4=u_ob_c36_add_name.
/*
:- side_effect(assert_lsp(u_ob_c36_add_name,
			  wl:lambda_def(defun, u_ob_c36_add_name, f_u_ob_c36_add_name, [u_self, u_obname], [[if, [not, [u_memq_c63, u_obname, [u_obr_obnames, u_self]]], [progn, [if, [u_ob_c63, u_obname], [progn, [setq, u_obname, [u_ob_c36_name, u_obname]], [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Probable obname redefinition.~%")]]], [if, [not, [u_symbol_c63, u_obname]], [setq, u_obname, [error, '$ARRAY'([*], claz_base_character, "ob$add-name: ~A not symbol"), u_obname]]], [u_yloop, [u_ywhile, [ext_assq, u_obname, u_xx_obnames_xx]], [u_ydo, [let, [[u_new_obname, [u_string_c62_symbol, [u_string_append, [u_symbol_c62_string, u_obname], '$ARRAY'([*], claz_base_character, "X")]]]], [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Obname ~A already used--using ~A instead.~%"), u_obname, u_new_obname], [setq, u_obname, u_new_obname]]]], [setq, u_xx_obnames_xx, [cons, [list, u_obname, u_self], u_xx_obnames_xx]], [u_set_obr_obnames, u_self, [cons, u_obname, [u_obr_obnames, u_self]]], u_obname], [progn, [u_ndbg, u_xx_gate_dbg_xx, u_ob_warn, '$ARRAY'([*], claz_base_character, "Warning: Obname ~A already in effect for specified ob.~%"), u_obname], u_obname]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add_name,
			  wl:arglist_info(u_ob_c36_add_name, f_u_ob_c36_add_name, [u_self, u_obname], arginfo{all:[u_self, u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_obname], opt:0, req:[u_self, u_obname], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add_name,
			  wl:init_args(exact_only, f_u_ob_c36_add_name))).
*/
/*
 This assumes obname is already determined to be unique. We assume that
*/
/*
 we are able to generate unique "OB." names above. This assumes the
*/
/*
 user does not create such names also.
*/
/*
(defun ob$add-unique-name (self obname)
  (setq *obnames* (cons (list obname self) *obnames*))
  (set-obr-obnames self (cons obname (obr-obnames self)))
  obname)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7309 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$add-unique-name',[self,obname],[setq,'*obnames*',[cons,[list,obname,self],'*obnames*']],['set-obr-obnames',self,[cons,obname,['obr-obnames',self]]],obname])
wl:lambda_def(defun, u_ob_c36_add_unique_name, f_u_ob_c36_add_unique_name, [u_self, u_obname], [[setq, u_xx_obnames_xx, [cons, [list, u_obname, u_self], u_xx_obnames_xx]], [u_set_obr_obnames, u_self, [cons, u_obname, [u_obr_obnames, u_self]]], u_obname]).
wl:arglist_info(u_ob_c36_add_unique_name, f_u_ob_c36_add_unique_name, [u_self, u_obname], arginfo{all:[u_self, u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_obname], opt:0, req:[u_self, u_obname], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_add_unique_name).

/*

### Compiled:  `U::OB$ADD-UNIQUE-NAME` 
*/
f_u_ob_c36_add_unique_name(Self, Obname, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_self, Self), bv(u_obname, Obname)|Env],
	get_var(AEnv, u_obname, Obname_Get),
	get_var(AEnv, u_self, Self_Get),
	CAR=[Obname_Get, Self_Get],
	get_var(AEnv, u_xx_obnames_xx, Xx_obnames_xx_Get),
	Xx_obnames_xx=[CAR|Xx_obnames_xx_Get],
	set_var(AEnv, u_xx_obnames_xx, Xx_obnames_xx),
	f_u_set_obr_obnames(u_self,
			    [cons, u_obname, [u_obr_obnames, u_self]],
			    Obr_obnames_Ret),
	get_var(AEnv, u_obname, Obname_Get11),
	Obname_Get11=FnResult.
:- set_opv(f_u_ob_c36_add_unique_name, classof, claz_function),
   set_opv(u_ob_c36_add_unique_name, compile_as, kw_function),
   set_opv(u_ob_c36_add_unique_name, function, f_u_ob_c36_add_unique_name),
   _Ignored4=u_ob_c36_add_unique_name.
/*
:- side_effect(assert_lsp(u_ob_c36_add_unique_name,
			  wl:lambda_def(defun, u_ob_c36_add_unique_name, f_u_ob_c36_add_unique_name, [u_self, u_obname], [[setq, u_xx_obnames_xx, [cons, [list, u_obname, u_self], u_xx_obnames_xx]], [u_set_obr_obnames, u_self, [cons, u_obname, [u_obr_obnames, u_self]]], u_obname]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add_unique_name,
			  wl:arglist_info(u_ob_c36_add_unique_name, f_u_ob_c36_add_unique_name, [u_self, u_obname], arginfo{all:[u_self, u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_obname], opt:0, req:[u_self, u_obname], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add_unique_name,
			  wl:init_args(exact_only, f_u_ob_c36_add_unique_name))).
*/
/*
(defun ob$remove-name (self obname)
  (set-obr-obnames self (delq! obname (obr-obnames self)))
  (setq *obnames* (del! (lambda (x y) (eq? x (car y))) obname *obnames*))
  (if (null? (obr-obnames self))
      (ob$add-unique-name
       self
       (string->symbol
        (string-append "OB."
                       (prog1
                        (fixnum->string *next-ob-number*)
                        (increment-me *next-ob-number*)))))))

;
; (ob$name->ob obname):
;
; Return the ob referred to by a obname. If there is no ob associated
; with the given obname, nil is returned.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:7473 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$remove-name',[self,obname],['set-obr-obnames',self,['delq!',obname,['obr-obnames',self]]],[setq,'*obnames*',['del!',[lambda,[x,y],['eq?',x,[car,y]]],obname,'*obnames*']],[if,['null?',['obr-obnames',self]],['ob$add-unique-name',self,['string->symbol',['string-append','$STRING'("OB."),[prog1,['fixnum->string','*next-ob-number*'],['increment-me','*next-ob-number*']]]]]]])
wl:lambda_def(defun, u_ob_c36_remove_name, f_u_ob_c36_remove_name, [u_self, u_obname], [[u_set_obr_obnames, u_self, [u_delq_c33, u_obname, [u_obr_obnames, u_self]]], [setq, u_xx_obnames_xx, [u_del_c33, [lambda, [u_x, u_y], [u_eq_c63, u_x, [car, u_y]]], u_obname, u_xx_obnames_xx]], [if, [u_null_c63, [u_obr_obnames, u_self]], [u_ob_c36_add_unique_name, u_self, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "OB."), [prog1, [u_fixnum_c62_string, u_xx_next_ob_number_xx], [u_increment_me, u_xx_next_ob_number_xx]]]]]]]).
wl:arglist_info(u_ob_c36_remove_name, f_u_ob_c36_remove_name, [u_self, u_obname], arginfo{all:[u_self, u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_obname], opt:0, req:[u_self, u_obname], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_remove_name).

/*

### Compiled:  `U::OB$REMOVE-NAME` 
*/
f_u_ob_c36_remove_name(Self, Obname, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_self, Self), bv(u_obname, Obname)|Env],
	f_u_set_obr_obnames(u_self,
			    [u_delq_c33, u_obname, [u_obr_obnames, u_self]],
			    Obr_obnames_Ret),
	Lambda=closure([ClosureEnvironment|AEnv], LResult, [u_x, u_y], f_u_eq_c63(u_x, [car, u_y], LResult)),
	get_var(AEnv, u_obname, Obname_Get),
	get_var(AEnv, u_xx_obnames_xx, Xx_obnames_xx_Get),
	f_u_del_c33(Lambda, Obname_Get, Xx_obnames_xx_Get, Xx_obnames_xx),
	set_var(AEnv, u_xx_obnames_xx, Xx_obnames_xx),
	f_u_null_c63([u_obr_obnames, u_self], IFTEST),
	(   IFTEST\==[]
	->  get_var(AEnv, u_self, Self_Get),
	    f_u_string_c62_symbol(
				  [ u_string_append,
				    '$ARRAY'([*], claz_base_character, "OB."),
				    
				    [ prog1,
				      
				      [ u_fixnum_c62_string,
					u_xx_next_ob_number_xx
				      ],
				      [u_increment_me, u_xx_next_ob_number_xx]
				    ]
				  ],
				  C62_symbol_Ret),
	    f_u_ob_c36_add_unique_name(Self_Get, C62_symbol_Ret, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_ob_c36_remove_name, classof, claz_function),
   set_opv(u_ob_c36_remove_name, compile_as, kw_function),
   set_opv(u_ob_c36_remove_name, function, f_u_ob_c36_remove_name),
   _Ignored4=u_ob_c36_remove_name.
/*
:- side_effect(assert_lsp(u_ob_c36_remove_name,
			  wl:lambda_def(defun, u_ob_c36_remove_name, f_u_ob_c36_remove_name, [u_self, u_obname], [[u_set_obr_obnames, u_self, [u_delq_c33, u_obname, [u_obr_obnames, u_self]]], [setq, u_xx_obnames_xx, [u_del_c33, [lambda, [u_x, u_y], [u_eq_c63, u_x, [car, u_y]]], u_obname, u_xx_obnames_xx]], [if, [u_null_c63, [u_obr_obnames, u_self]], [u_ob_c36_add_unique_name, u_self, [u_string_c62_symbol, [u_string_append, '$ARRAY'([*], claz_base_character, "OB."), [prog1, [u_fixnum_c62_string, u_xx_next_ob_number_xx], [u_increment_me, u_xx_next_ob_number_xx]]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove_name,
			  wl:arglist_info(u_ob_c36_remove_name, f_u_ob_c36_remove_name, [u_self, u_obname], arginfo{all:[u_self, u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_obname], opt:0, req:[u_self, u_obname], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove_name,
			  wl:init_args(exact_only, f_u_ob_c36_remove_name))).
*/
/*
*/
/*
 (ob$name->ob obname):
*/
/*
*/
/*
 Return the ob referred to by a obname. If there is no ob associated
*/
/*
 with the given obname, nil is returned.
*/
/*
*/
/*
(defun ob$name->ob (obname)
  (let ((found (assq obname *obnames*)))
    (if found (cadr found) nil)))

;
; (ob$name ob):
;
; Return a (actually, the most recently defined) obname for an ob.
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8058 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$name->ob',[obname],[let,[[found,[assq,obname,'*obnames*']]],[if,found,[cadr,found],[]]]])
wl:lambda_def(defun, u_ob_c36_name_c62_ob, f_u_ob_c36_name_c62_ob, [u_obname], [[let, [[u_found, [ext_assq, u_obname, u_xx_obnames_xx]]], [if, u_found, [cadr, u_found], []]]]).
wl:arglist_info(u_ob_c36_name_c62_ob, f_u_ob_c36_name_c62_ob, [u_obname], arginfo{all:[u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obname], opt:0, req:[u_obname], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_name_c62_ob).

/*

### Compiled:  `U::OB$NAME->OB` 
*/
f_u_ob_c36_name_c62_ob(Obname, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_obname, Obname)|Env],
	f_ext_assq(u_obname, u_xx_obnames_xx, Found_Init),
	LEnv=[bv(u_found, Found_Init)|Env18],
	get_var(LEnv, u_found, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, u_found, Found_Get14),
	    cl_cadr(Found_Get14, TrueResult),
	    FnResult=TrueResult
	;   FnResult=[]
	).
:- set_opv(f_u_ob_c36_name_c62_ob, classof, claz_function),
   set_opv(u_ob_c36_name_c62_ob, compile_as, kw_function),
   set_opv(u_ob_c36_name_c62_ob, function, f_u_ob_c36_name_c62_ob),
   _Ignored4=u_ob_c36_name_c62_ob.
/*
:- side_effect(assert_lsp(u_ob_c36_name_c62_ob,
			  wl:lambda_def(defun, u_ob_c36_name_c62_ob, f_u_ob_c36_name_c62_ob, [u_obname], [[let, [[u_found, [ext_assq, u_obname, u_xx_obnames_xx]]], [if, u_found, [cadr, u_found], []]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_name_c62_ob,
			  wl:arglist_info(u_ob_c36_name_c62_ob, f_u_ob_c36_name_c62_ob, [u_obname], arginfo{all:[u_obname], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obname], opt:0, req:[u_obname], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_name_c62_ob,
			  wl:init_args(exact_only, f_u_ob_c36_name_c62_ob))).
*/
/*
*/
/*
 (ob$name ob):
*/
/*
*/
/*
 Return a (actually, the most recently defined) obname for an ob.
*/
/*
*/
/*
(defun ob$name (self)
  (car (obr-obnames self)))                      

;
; (ob$names ob):
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8251 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$name',[self],[car,['obr-obnames',self]]])
wl:lambda_def(defun, u_ob_c36_name, f_u_ob_c36_name, [u_self], [[car, [u_obr_obnames, u_self]]]).
wl:arglist_info(u_ob_c36_name, f_u_ob_c36_name, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_name).

/*

### Compiled:  `U::OB$NAME` 
*/
f_u_ob_c36_name(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_obr_obnames(Self_Get, Car_Param),
	cl_car(Car_Param, Car_Ret),
	Car_Ret=FnResult.
:- set_opv(f_u_ob_c36_name, classof, claz_function),
   set_opv(u_ob_c36_name, compile_as, kw_function),
   set_opv(u_ob_c36_name, function, f_u_ob_c36_name),
   _Ignored4=u_ob_c36_name.
/*
:- side_effect(assert_lsp(u_ob_c36_name,
			  wl:lambda_def(defun, u_ob_c36_name, f_u_ob_c36_name, [u_self], [[car, [u_obr_obnames, u_self]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_name,
			  wl:arglist_info(u_ob_c36_name, f_u_ob_c36_name, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_name,
			  wl:init_args(exact_only, f_u_ob_c36_name))).
*/
/*
*/
/*
 (ob$names ob):
*/
/*
*/
/*
(defun ob$names (self)
  (obr-obnames self))

;
; The automatic setting of inverses can be disabled. Currently,
; inverse setting is turned off only during load of a ob dump.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8345 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$names',[self],['obr-obnames',self]])
wl:lambda_def(defun, u_ob_c36_names, f_u_ob_c36_names, [u_self], [[u_obr_obnames, u_self]]).
wl:arglist_info(u_ob_c36_names, f_u_ob_c36_names, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_names).

/*

### Compiled:  `U::OB$NAMES` 
*/
f_u_ob_c36_names(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_obr_obnames(Self_Get, Obr_obnames_Ret),
	Obr_obnames_Ret=FnResult.
:- set_opv(f_u_ob_c36_names, classof, claz_function),
   set_opv(u_ob_c36_names, compile_as, kw_function),
   set_opv(u_ob_c36_names, function, f_u_ob_c36_names),
   _Ignored4=u_ob_c36_names.
/*
:- side_effect(assert_lsp(u_ob_c36_names,
			  wl:lambda_def(defun, u_ob_c36_names, f_u_ob_c36_names, [u_self], [[u_obr_obnames, u_self]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_names,
			  wl:arglist_info(u_ob_c36_names, f_u_ob_c36_names, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_names,
			  wl:init_args(exact_only, f_u_ob_c36_names))).
*/
/*
*/
/*
 The automatic setting of inverses can be disabled. Currently,
*/
/*
 inverse setting is turned off only during load of a ob dump.
*/
/*
*/
/*
(setq *inverse-setting?* t)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8523 **********************/
:-lisp_compile_to_prolog(pkg_user,[setq,'*inverse-setting?*',t])
:- set_var(AEnv, setq, u_xx_inverse_setting_c63_xx, t).
/*
(defun inverse-setting-on ()
  (let ((previous *inverse-setting?*))
    (setq *inverse-setting?* t)
    previous))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8552 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'inverse-setting-on',[],[let,[[previous,'*inverse-setting?*']],[setq,'*inverse-setting?*',t],previous]])
wl:lambda_def(defun, u_inverse_setting_on, f_u_inverse_setting_on, [], [[let, [[u_previous, u_xx_inverse_setting_c63_xx]], [setq, u_xx_inverse_setting_c63_xx, t], u_previous]]).
wl:arglist_info(u_inverse_setting_on, f_u_inverse_setting_on, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_inverse_setting_on).

/*

### Compiled:  `U::INVERSE-SETTING-ON` 
*/
f_u_inverse_setting_on(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv,
		u_xx_inverse_setting_c63_xx,
		Xx_inverse_setting_c63_xx_Get),
	LEnv=[bv(u_previous, Xx_inverse_setting_c63_xx_Get)|GEnv],
	set_var(LEnv, setq, u_xx_inverse_setting_c63_xx, t),
	get_var(LEnv, u_previous, Previous_Get),
	Previous_Get=FnResult.
:- set_opv(f_u_inverse_setting_on, classof, claz_function),
   set_opv(u_inverse_setting_on, compile_as, kw_function),
   set_opv(u_inverse_setting_on, function, f_u_inverse_setting_on),
   _Ignored4=u_inverse_setting_on.
/*
:- side_effect(assert_lsp(u_inverse_setting_on,
			  wl:lambda_def(defun, u_inverse_setting_on, f_u_inverse_setting_on, [], [[let, [[u_previous, u_xx_inverse_setting_c63_xx]], [setq, u_xx_inverse_setting_c63_xx, t], u_previous]]))).
*/
/*
:- side_effect(assert_lsp(u_inverse_setting_on,
			  wl:arglist_info(u_inverse_setting_on, f_u_inverse_setting_on, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_inverse_setting_on,
			  wl:init_args(exact_only, f_u_inverse_setting_on))).
*/
/*
(defun inverse-setting-off ()
  (let ((previous *inverse-setting?*))
    (setq *inverse-setting?* nil)
    previous))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8668 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'inverse-setting-off',[],[let,[[previous,'*inverse-setting?*']],[setq,'*inverse-setting?*',[]],previous]])
wl:lambda_def(defun, u_inverse_setting_off, f_u_inverse_setting_off, [], [[let, [[u_previous, u_xx_inverse_setting_c63_xx]], [setq, u_xx_inverse_setting_c63_xx, []], u_previous]]).
wl:arglist_info(u_inverse_setting_off, f_u_inverse_setting_off, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_inverse_setting_off).

/*

### Compiled:  `U::INVERSE-SETTING-OFF` 
*/
f_u_inverse_setting_off(FnResult) :-
	nop(global_env(Env)),
	GEnv=Env,
	get_var(GEnv,
		u_xx_inverse_setting_c63_xx,
		Xx_inverse_setting_c63_xx_Get),
	LEnv=[bv(u_previous, Xx_inverse_setting_c63_xx_Get)|GEnv],
	set_var(LEnv, setq, u_xx_inverse_setting_c63_xx, []),
	get_var(LEnv, u_previous, Previous_Get),
	Previous_Get=FnResult.
:- set_opv(f_u_inverse_setting_off, classof, claz_function),
   set_opv(u_inverse_setting_off, compile_as, kw_function),
   set_opv(u_inverse_setting_off, function, f_u_inverse_setting_off),
   _Ignored4=u_inverse_setting_off.
/*
:- side_effect(assert_lsp(u_inverse_setting_off,
			  wl:lambda_def(defun, u_inverse_setting_off, f_u_inverse_setting_off, [], [[let, [[u_previous, u_xx_inverse_setting_c63_xx]], [setq, u_xx_inverse_setting_c63_xx, []], u_previous]]))).
*/
/*
:- side_effect(assert_lsp(u_inverse_setting_off,
			  wl:arglist_info(u_inverse_setting_off, f_u_inverse_setting_off, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_inverse_setting_off,
			  wl:init_args(exact_only, f_u_inverse_setting_off))).
*/
/*
(defun restore-inverse-setting (val)
  (setq *inverse-setting?* val))

;
; (ob$add ob slot-name slot-value):
;
; Add a slot value to an ob. If slot-value is an ob, the inverse slot addition
; is performed.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8787 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'restore-inverse-setting',[val],[setq,'*inverse-setting?*',val]])
wl:lambda_def(defun, u_restore_inverse_setting, f_u_restore_inverse_setting, [u_val], [[setq, u_xx_inverse_setting_c63_xx, u_val]]).
wl:arglist_info(u_restore_inverse_setting, f_u_restore_inverse_setting, [u_val], arginfo{all:[u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_val], opt:0, req:[u_val], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_restore_inverse_setting).

/*

### Compiled:  `U::RESTORE-INVERSE-SETTING` 
*/
f_u_restore_inverse_setting(Val, FnResult) :-
	nop(global_env(Env)),
	AEnv=[bv(u_val, Val)|Env],
	get_var(AEnv, u_val, Val_Get),
	set_var(AEnv, u_xx_inverse_setting_c63_xx, Val_Get),
	Val_Get=FnResult.
:- set_opv(f_u_restore_inverse_setting, classof, claz_function),
   set_opv(u_restore_inverse_setting, compile_as, kw_function),
   set_opv(u_restore_inverse_setting, function, f_u_restore_inverse_setting),
   _Ignored4=u_restore_inverse_setting.
/*
:- side_effect(assert_lsp(u_restore_inverse_setting,
			  wl:lambda_def(defun, u_restore_inverse_setting, f_u_restore_inverse_setting, [u_val], [[setq, u_xx_inverse_setting_c63_xx, u_val]]))).
*/
/*
:- side_effect(assert_lsp(u_restore_inverse_setting,
			  wl:arglist_info(u_restore_inverse_setting, f_u_restore_inverse_setting, [u_val], arginfo{all:[u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_val], opt:0, req:[u_val], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_restore_inverse_setting,
			  wl:init_args(exact_only, f_u_restore_inverse_setting))).
*/
/*
*/
/*
 (ob$add ob slot-name slot-value):
*/
/*
*/
/*
 Add a slot value to an ob. If slot-value is an ob, the inverse slot addition
*/
/*
 is performed.
*/
/*
*/
/*
(defun ob$add (self slot-name slot-value)
  (enforce-ob self "ob$add")
  (ob$add1 self slot-name slot-value)
  slot-value)

;
; (ob$padd ob slot-path slot-value):
;
; Allows a path to be used.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:8996 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$add',[self,'slot-name','slot-value'],['enforce-ob',self,'$STRING'("ob$add")],['ob$add1',self,'slot-name','slot-value'],'slot-value'])
wl:lambda_def(defun, u_ob_c36_add, f_u_ob_c36_add, [u_self, u_slot_name, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$add")], [u_ob_c36_add1, u_self, u_slot_name, slot_value], slot_value]).
wl:arglist_info(u_ob_c36_add, f_u_ob_c36_add, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_add).

/*

### Compiled:  `U::OB$ADD` 
*/
f_u_ob_c36_add(Self, Slot_name, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$add"),
		       Enforce_ob_Ret),
	get_var(Env13, slot_value, Slot_value_Get),
	get_var(Env13, u_self, Self_Get),
	get_var(Env13, u_slot_name, Slot_name_Get),
	f_u_ob_c36_add1(Self_Get, Slot_name_Get, Slot_value_Get, C36_add1_Ret),
	get_var(Env13, slot_value, Slot_value_Get10),
	Slot_value_Get10=FnResult.
:- set_opv(f_u_ob_c36_add, classof, claz_function),
   set_opv(u_ob_c36_add, compile_as, kw_function),
   set_opv(u_ob_c36_add, function, f_u_ob_c36_add),
   _Ignored4=u_ob_c36_add.
/*
:- side_effect(assert_lsp(u_ob_c36_add,
			  wl:lambda_def(defun, u_ob_c36_add, f_u_ob_c36_add, [u_self, u_slot_name, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$add")], [u_ob_c36_add1, u_self, u_slot_name, slot_value], slot_value]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add,
			  wl:arglist_info(u_ob_c36_add, f_u_ob_c36_add, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add,
			  wl:init_args(exact_only, f_u_ob_c36_add))).
*/
/*
*/
/*
 (ob$padd ob slot-path slot-value):
*/
/*
*/
/*
 Allows a path to be used.
*/
/*
*/
/*
(defun ob$padd (self slot-path slot-value)
  (enforce-ob self "ob$padd")
  (if (pair? slot-path)
      (ob$add1
                (path->ob self slot-path)
                (path->slot-name slot-path)
                slot-value)
      (ob$add1 self slot-path slot-value))
  slot-value)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9192 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$padd',[self,'slot-path','slot-value'],['enforce-ob',self,'$STRING'("ob$padd")],[if,['pair?','slot-path'],['ob$add1',['path->ob',self,'slot-path'],['path->slot-name','slot-path'],'slot-value'],['ob$add1',self,'slot-path','slot-value']],'slot-value'])
wl:lambda_def(defun, u_ob_c36_padd, f_u_ob_c36_padd, [u_self, u_slot_path, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$padd")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_add1, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path], slot_value], [u_ob_c36_add1, u_self, u_slot_path, slot_value]], slot_value]).
wl:arglist_info(u_ob_c36_padd, f_u_ob_c36_padd, [u_self, u_slot_path, slot_value], arginfo{all:[u_self, u_slot_path, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path, slot_value], opt:0, req:[u_self, u_slot_path, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_padd).

/*

### Compiled:  `U::OB$PADD` 
*/
f_u_ob_c36_padd(Self, Slot_path, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env20=[bv(u_self, Self), bv(u_slot_path, Slot_path), bv(slot_value, Slot_value)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$padd"),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env20, u_self, Self_Get),
	    get_var(Env20, u_slot_path, Slot_path_Get),
	    f_u_path_c62_ob(Self_Get, Slot_path_Get, C36_add1_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    get_var(Env20, slot_value, Slot_value_Get),
	    f_u_ob_c36_add1(C36_add1_Param,
			    Slot_name_Ret,
			    Slot_value_Get,
			    TrueResult),
	    _173283348=TrueResult
	;   get_var(Env20, slot_value, Slot_value_Get14),
	    get_var(Env20, u_self, Self_Get12),
	    get_var(Env20, u_slot_path, Slot_path_Get13),
	    f_u_ob_c36_add1(Self_Get12,
			    Slot_path_Get13,
			    Slot_value_Get14,
			    ElseResult),
	    _173283348=ElseResult
	),
	get_var(Env20, slot_value, Slot_value_Get17),
	Slot_value_Get17=FnResult.
:- set_opv(f_u_ob_c36_padd, classof, claz_function),
   set_opv(u_ob_c36_padd, compile_as, kw_function),
   set_opv(u_ob_c36_padd, function, f_u_ob_c36_padd),
   _Ignored4=u_ob_c36_padd.
/*
:- side_effect(assert_lsp(u_ob_c36_padd,
			  wl:lambda_def(defun, u_ob_c36_padd, f_u_ob_c36_padd, [u_self, u_slot_path, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$padd")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_add1, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path], slot_value], [u_ob_c36_add1, u_self, u_slot_path, slot_value]], slot_value]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_padd,
			  wl:arglist_info(u_ob_c36_padd, f_u_ob_c36_padd, [u_self, u_slot_path, slot_value], arginfo{all:[u_self, u_slot_path, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path, slot_value], opt:0, req:[u_self, u_slot_path, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_padd,
			  wl:init_args(exact_only, f_u_ob_c36_padd))).
*/
/*
(defun path->ob (ob path)
  (if (cdr path)
      (path->ob (ob$get ob (car path))
                 (cdr path))
      ob))

;
; (ob$remove ob slot-name slot-value):
;
; Remove the specified value from the specified slot. If slot-value is a
; ob, the inverse slot removal is performed.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9476 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'path->ob',[ob,path],[if,[cdr,path],['path->ob',['ob$get',ob,[car,path]],[cdr,path]],ob]])
wl:lambda_def(defun, u_path_c62_ob, f_u_path_c62_ob, [u_ob, u_path], [[if, [cdr, u_path], [u_path_c62_ob, [u_ob_c36_get, u_ob, [car, u_path]], [cdr, u_path]], u_ob]]).
wl:arglist_info(u_path_c62_ob, f_u_path_c62_ob, [u_ob, u_path], arginfo{all:[u_ob, u_path], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_path], opt:0, req:[u_ob, u_path], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_path_c62_ob).

/*

### Compiled:  `U::PATH->OB` 
*/
f_u_path_c62_ob(Ob, Path, FnResult) :-
	nop(global_env(Env)),
	Env18=[bv(u_ob, Ob), bv(u_path, Path)|Env],
	get_var(Env18, u_path, Path_Get),
	cl_cdr(Path_Get, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env18, u_ob, Ob_Get),
	    get_var(Env18, u_path, Path_Get11),
	    cl_car(Path_Get11, Car_Ret),
	    f_u_ob_c36_get(Ob_Get, Car_Ret, C62_ob_Param),
	    get_var(Env18, u_path, Path_Get12),
	    cl_cdr(Path_Get12, Cdr_Ret),
	    f_u_path_c62_ob(C62_ob_Param, Cdr_Ret, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env18, u_ob, Ob_Get13),
	    FnResult=Ob_Get13
	).
:- set_opv(f_u_path_c62_ob, classof, claz_function),
   set_opv(u_path_c62_ob, compile_as, kw_function),
   set_opv(u_path_c62_ob, function, f_u_path_c62_ob),
   _Ignored4=u_path_c62_ob.
/*
:- side_effect(assert_lsp(u_path_c62_ob,
			  wl:lambda_def(defun, u_path_c62_ob, f_u_path_c62_ob, [u_ob, u_path], [[if, [cdr, u_path], [u_path_c62_ob, [u_ob_c36_get, u_ob, [car, u_path]], [cdr, u_path]], u_ob]]))).
*/
/*
:- side_effect(assert_lsp(u_path_c62_ob,
			  wl:arglist_info(u_path_c62_ob, f_u_path_c62_ob, [u_ob, u_path], arginfo{all:[u_ob, u_path], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_ob, u_path], opt:0, req:[u_ob, u_path], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_path_c62_ob,
			  wl:init_args(exact_only, f_u_path_c62_ob))).
*/
/*
*/
/*
 (ob$remove ob slot-name slot-value):
*/
/*
*/
/*
 Remove the specified value from the specified slot. If slot-value is a
*/
/*
 ob, the inverse slot removal is performed.
*/
/*
*/
/*
(defun ob$remove (self slot-name slot-value)
  (enforce-ob self "ob$remove")
  (ob$remove1 self slot-name slot-value)
  slot-value)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9763 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$remove',[self,'slot-name','slot-value'],['enforce-ob',self,'$STRING'("ob$remove")],['ob$remove1',self,'slot-name','slot-value'],'slot-value'])
wl:lambda_def(defun, u_ob_c36_remove, f_u_ob_c36_remove, [u_self, u_slot_name, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$remove")], [u_ob_c36_remove1, u_self, u_slot_name, slot_value], slot_value]).
wl:arglist_info(u_ob_c36_remove, f_u_ob_c36_remove, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_remove).

/*

### Compiled:  `U::OB$REMOVE` 
*/
f_u_ob_c36_remove(Self, Slot_name, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env13=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$remove"),
		       Enforce_ob_Ret),
	get_var(Env13, slot_value, Slot_value_Get),
	get_var(Env13, u_self, Self_Get),
	get_var(Env13, u_slot_name, Slot_name_Get),
	f_u_ob_c36_remove1(Self_Get,
			   Slot_name_Get,
			   Slot_value_Get,
			   C36_remove1_Ret),
	get_var(Env13, slot_value, Slot_value_Get10),
	Slot_value_Get10=FnResult.
:- set_opv(f_u_ob_c36_remove, classof, claz_function),
   set_opv(u_ob_c36_remove, compile_as, kw_function),
   set_opv(u_ob_c36_remove, function, f_u_ob_c36_remove),
   _Ignored4=u_ob_c36_remove.
/*
:- side_effect(assert_lsp(u_ob_c36_remove,
			  wl:lambda_def(defun, u_ob_c36_remove, f_u_ob_c36_remove, [u_self, u_slot_name, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$remove")], [u_ob_c36_remove1, u_self, u_slot_name, slot_value], slot_value]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove,
			  wl:arglist_info(u_ob_c36_remove, f_u_ob_c36_remove, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove,
			  wl:init_args(exact_only, f_u_ob_c36_remove))).
*/
/*
(defun ob$premove (self slot-path slot-value)
  (enforce-ob self "ob$premove")
  (if (pair? slot-path)
      (ob$remove1
                  (path->ob self slot-path)
                  (path->slot-name slot-path))
      (ob$add1 self slot-path slot-value))
  slot-value)

;
; (ob$gets ob slot-name):
;
; Return all values of a slot.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:9896 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$premove',[self,'slot-path','slot-value'],['enforce-ob',self,'$STRING'("ob$premove")],[if,['pair?','slot-path'],['ob$remove1',['path->ob',self,'slot-path'],['path->slot-name','slot-path']],['ob$add1',self,'slot-path','slot-value']],'slot-value'])
wl:lambda_def(defun, u_ob_c36_premove, f_u_ob_c36_premove, [u_self, u_slot_path, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$premove")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_remove1, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path]], [u_ob_c36_add1, u_self, u_slot_path, slot_value]], slot_value]).
wl:arglist_info(u_ob_c36_premove, f_u_ob_c36_premove, [u_self, u_slot_path, slot_value], arginfo{all:[u_self, u_slot_path, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path, slot_value], opt:0, req:[u_self, u_slot_path, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_premove).

/*

### Compiled:  `U::OB$PREMOVE` 
*/
f_u_ob_c36_premove(Self, Slot_path, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env19=[bv(u_self, Self), bv(u_slot_path, Slot_path), bv(slot_value, Slot_value)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$premove"),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env19, u_self, Self_Get),
	    get_var(Env19, u_slot_path, Slot_path_Get),
	    f_u_path_c62_ob(Self_Get, Slot_path_Get, C36_remove1_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    f_u_ob_c36_remove1(C36_remove1_Param, Slot_name_Ret, TrueResult),
	    _176041278=TrueResult
	;   get_var(Env19, slot_value, Slot_value_Get),
	    get_var(Env19, u_self, Self_Get11),
	    get_var(Env19, u_slot_path, Slot_path_Get12),
	    f_u_ob_c36_add1(Self_Get11,
			    Slot_path_Get12,
			    Slot_value_Get,
			    ElseResult),
	    _176041278=ElseResult
	),
	get_var(Env19, slot_value, Slot_value_Get16),
	Slot_value_Get16=FnResult.
:- set_opv(f_u_ob_c36_premove, classof, claz_function),
   set_opv(u_ob_c36_premove, compile_as, kw_function),
   set_opv(u_ob_c36_premove, function, f_u_ob_c36_premove),
   _Ignored4=u_ob_c36_premove.
/*
:- side_effect(assert_lsp(u_ob_c36_premove,
			  wl:lambda_def(defun, u_ob_c36_premove, f_u_ob_c36_premove, [u_self, u_slot_path, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$premove")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_remove1, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path]], [u_ob_c36_add1, u_self, u_slot_path, slot_value]], slot_value]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_premove,
			  wl:arglist_info(u_ob_c36_premove, f_u_ob_c36_premove, [u_self, u_slot_path, slot_value], arginfo{all:[u_self, u_slot_path, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path, slot_value], opt:0, req:[u_self, u_slot_path, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_premove,
			  wl:init_args(exact_only, f_u_ob_c36_premove))).
*/
/*
*/
/*
 (ob$gets ob slot-name):
*/
/*
*/
/*
 Return all values of a slot.
*/
/*
*/
/*
(defun ob$gets (self slot-name)
  (enforce-ob self "ob$gets")
 (if (eq? slot-name 'obname)
     (obr-obnames self)
     (yloop (initial (result nil)
                    (rest (obr-slots self)))
           (ywhile rest)
           (ydo (if (eq? slot-name (slots-name (car rest)))
                   (setq result
                        (append! result (list (slots-value (car rest))))))
               (setq rest (cdr rest)))
           (yresult result))))

;
; (ob$get-many ob slot-names):
;
; Return values of several slots.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10230 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$gets',[self,'slot-name'],['enforce-ob',self,'$STRING'("ob$gets")],[if,['eq?','slot-name',[quote,obname]],['obr-obnames',self],[yloop,[initial,[result,[]],[rest,['obr-slots',self]]],[ywhile,rest],[ydo,[if,['eq?','slot-name',['slots-name',[car,rest]]],[setq,result,['append!',result,[list,['slots-value',[car,rest]]]]]],[setq,rest,[cdr,rest]]],[yresult,result]]]])
wl:lambda_def(defun, u_ob_c36_gets, f_u_ob_c36_gets, [u_self, u_slot_name], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$gets")], [if, [u_eq_c63, u_slot_name, [quote, u_obname]], [u_obr_obnames, u_self], [u_yloop, [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]], [u_ywhile, rest], [u_ydo, [if, [u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]], [setq, u_result, [u_append_c33, u_result, [list, [u_slots_value, [car, rest]]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result]]]]).
wl:arglist_info(u_ob_c36_gets, f_u_ob_c36_gets, [u_self, u_slot_name], arginfo{all:[u_self, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name], opt:0, req:[u_self, u_slot_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_gets).

/*

### Compiled:  `U::OB$GETS` 
*/
f_u_ob_c36_gets(Self, Slot_name, FnResult) :-
	nop(global_env(Env)),
	Env14=[bv(u_self, Self), bv(u_slot_name, Slot_name)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$gets"),
		       Enforce_ob_Ret),
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env14, u_self, Self_Get),
	    f_u_obr_obnames(Self_Get, TrueResult),
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
   _Ignored4=u_ob_c36_gets.
/*
:- side_effect(assert_lsp(u_ob_c36_gets,
			  wl:lambda_def(defun, u_ob_c36_gets, f_u_ob_c36_gets, [u_self, u_slot_name], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$gets")], [if, [u_eq_c63, u_slot_name, [quote, u_obname]], [u_obr_obnames, u_self], [u_yloop, [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]], [u_ywhile, rest], [u_ydo, [if, [u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]], [setq, u_result, [u_append_c33, u_result, [list, [u_slots_value, [car, rest]]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_gets,
			  wl:arglist_info(u_ob_c36_gets, f_u_ob_c36_gets, [u_self, u_slot_name], arginfo{all:[u_self, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name], opt:0, req:[u_self, u_slot_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_gets,
			  wl:init_args(exact_only, f_u_ob_c36_gets))).
*/
/*
*/
/*
 (ob$get-many ob slot-names):
*/
/*
*/
/*
 Return values of several slots.
*/
/*
*/
/*
(defun ob$get-many (self slot-names)
  (enforce-ob self "ob$get-many")
  (yloop (initial (result nil)
                 (rest (obr-slots self)))
        (ywhile rest)
        (ydo (if (memq? (slots-name (car rest)) slot-names)
                (setq result (append! result (list (slots-value (car rest))))))
            (setq rest (cdr rest)))
        (yresult result)))  

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:10759 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$get-many',[self,'slot-names'],['enforce-ob',self,'$STRING'("ob$get-many")],[yloop,[initial,[result,[]],[rest,['obr-slots',self]]],[ywhile,rest],[ydo,[if,['memq?',['slots-name',[car,rest]],'slot-names'],[setq,result,['append!',result,[list,['slots-value',[car,rest]]]]]],[setq,rest,[cdr,rest]]],[yresult,result]]])
wl:lambda_def(defun, u_ob_c36_get_many, f_u_ob_c36_get_many, [u_self, u_slot_names], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$get-many")], [u_yloop, [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]], [u_ywhile, rest], [u_ydo, [if, [u_memq_c63, [u_slots_name, [car, rest]], u_slot_names], [setq, u_result, [u_append_c33, u_result, [list, [u_slots_value, [car, rest]]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result]]]).
wl:arglist_info(u_ob_c36_get_many, f_u_ob_c36_get_many, [u_self, u_slot_names], arginfo{all:[u_self, u_slot_names], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_names], opt:0, req:[u_self, u_slot_names], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_get_many).

/*

### Compiled:  `U::OB$GET-MANY` 
*/
f_u_ob_c36_get_many(Self, Slot_names, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(u_slot_names, Slot_names)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$get-many"),
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
   _Ignored4=u_ob_c36_get_many.
/*
:- side_effect(assert_lsp(u_ob_c36_get_many,
			  wl:lambda_def(defun, u_ob_c36_get_many, f_u_ob_c36_get_many, [u_self, u_slot_names], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$get-many")], [u_yloop, [u_initial, [u_result, []], [rest, [u_obr_slots, u_self]]], [u_ywhile, rest], [u_ydo, [if, [u_memq_c63, [u_slots_name, [car, rest]], u_slot_names], [setq, u_result, [u_append_c33, u_result, [list, [u_slots_value, [car, rest]]]]]], [setq, rest, [cdr, rest]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_get_many,
			  wl:arglist_info(u_ob_c36_get_many, f_u_ob_c36_get_many, [u_self, u_slot_names], arginfo{all:[u_self, u_slot_names], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_names], opt:0, req:[u_self, u_slot_names], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_get_many,
			  wl:init_args(exact_only, f_u_ob_c36_get_many))).
*/
/*
(defun ob$concatenate (&rest obs)
  (yloop
    (initial (result (ob$create-empty)))
    (yfor ob in obs)
    (ydo
     (yloop
      (yfor sv in (ob$pairs ob))
      (ydo (ob$add1 result (slots-name sv) (slots-value sv)))))
   (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11131 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$concatenate',['&rest',obs],[yloop,[initial,[result,['ob$create-empty']]],[yfor,ob,in,obs],[ydo,[yloop,[yfor,sv,in,['ob$pairs',ob]],[ydo,['ob$add1',result,['slots-name',sv],['slots-value',sv]]]]],[yresult,result]]])
wl:lambda_def(defun, u_ob_c36_concatenate, f_u_ob_c36_concatenate, [c38_rest, u_obs], [[u_yloop, [u_initial, [u_result, [u_ob_c36_create_empty]]], [u_yfor, u_ob, u_in, u_obs], [u_ydo, [u_yloop, [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ydo, [u_ob_c36_add1, u_result, [u_slots_name, u_sv], [u_slots_value, u_sv]]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_ob_c36_concatenate, f_u_ob_c36_concatenate, [c38_rest, u_obs], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_obs], opt:0, req:0, rest:[u_obs], sublists:0, whole:0}).
wl: init_args(0, f_u_ob_c36_concatenate).

/*

### Compiled:  `U::OB$CONCATENATE` 
*/
f_u_ob_c36_concatenate(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	_179365584=[[[bv(u_obs, RestNKeys)]|Env]|Env],
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
   _Ignored4=u_ob_c36_concatenate.
/*
:- side_effect(assert_lsp(u_ob_c36_concatenate,
			  wl:lambda_def(defun, u_ob_c36_concatenate, f_u_ob_c36_concatenate, [c38_rest, u_obs], [[u_yloop, [u_initial, [u_result, [u_ob_c36_create_empty]]], [u_yfor, u_ob, u_in, u_obs], [u_ydo, [u_yloop, [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ydo, [u_ob_c36_add1, u_result, [u_slots_name, u_sv], [u_slots_value, u_sv]]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concatenate,
			  wl:arglist_info(u_ob_c36_concatenate, f_u_ob_c36_concatenate, [c38_rest, u_obs], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_obs], opt:0, req:0, rest:[u_obs], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concatenate,
			  wl:init_args(0, f_u_ob_c36_concatenate))).
*/
/*
(defun ob$concatenate! (&rest obs)
  (yloop
    (initial (result (car obs)))
    (yfor ob in (cdr obs))
    (ydo
     (yloop
      (yfor sv in (ob$pairs ob))
      (ydo (ob$add1 result (slots-name sv) (slots-value sv)))))
   (yresult result)))

;
; ob$pairs: get all the slot-name slot-value pairs of an ob
;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11377 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$concatenate!',['&rest',obs],[yloop,[initial,[result,[car,obs]]],[yfor,ob,in,[cdr,obs]],[ydo,[yloop,[yfor,sv,in,['ob$pairs',ob]],[ydo,['ob$add1',result,['slots-name',sv],['slots-value',sv]]]]],[yresult,result]]])
wl:lambda_def(defun, u_ob_c36_concatenate_c33, f_u_ob_c36_concatenate_c33, [c38_rest, u_obs], [[u_yloop, [u_initial, [u_result, [car, u_obs]]], [u_yfor, u_ob, u_in, [cdr, u_obs]], [u_ydo, [u_yloop, [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ydo, [u_ob_c36_add1, u_result, [u_slots_name, u_sv], [u_slots_value, u_sv]]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_ob_c36_concatenate_c33, f_u_ob_c36_concatenate_c33, [c38_rest, u_obs], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_obs], opt:0, req:0, rest:[u_obs], sublists:0, whole:0}).
wl: init_args(0, f_u_ob_c36_concatenate_c33).

/*

### Compiled:  `U::OB$CONCATENATE!` 
*/
f_u_ob_c36_concatenate_c33(RestNKeys, FnResult) :-
	nop(global_env(Env)),
	_180147026=[[[bv(u_obs, RestNKeys)]|Env]|Env],
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
   _Ignored4=u_ob_c36_concatenate_c33.
/*
:- side_effect(assert_lsp(u_ob_c36_concatenate_c33,
			  wl:lambda_def(defun, u_ob_c36_concatenate_c33, f_u_ob_c36_concatenate_c33, [c38_rest, u_obs], [[u_yloop, [u_initial, [u_result, [car, u_obs]]], [u_yfor, u_ob, u_in, [cdr, u_obs]], [u_ydo, [u_yloop, [u_yfor, u_sv, u_in, [u_ob_c36_pairs, u_ob]], [u_ydo, [u_ob_c36_add1, u_result, [u_slots_name, u_sv], [u_slots_value, u_sv]]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concatenate_c33,
			  wl:arglist_info(u_ob_c36_concatenate_c33, f_u_ob_c36_concatenate_c33, [c38_rest, u_obs], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[u_obs], opt:0, req:0, rest:[u_obs], sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_concatenate_c33,
			  wl:init_args(0, f_u_ob_c36_concatenate_c33))).
*/
/*
*/
/*
 ob$pairs: get all the slot-name slot-value pairs of an ob
*/
/*
*/
/*
(defun ob$pairs (self) (obr-slots self))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11686 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$pairs',[self],['obr-slots',self]])
wl:lambda_def(defun, u_ob_c36_pairs, f_u_ob_c36_pairs, [u_self], [[u_obr_slots, u_self]]).
wl:arglist_info(u_ob_c36_pairs, f_u_ob_c36_pairs, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_pairs).

/*

### Compiled:  `U::OB$PAIRS` 
*/
f_u_ob_c36_pairs(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_obr_slots(Self_Get, Obr_slots_Ret),
	Obr_slots_Ret=FnResult.
:- set_opv(f_u_ob_c36_pairs, classof, claz_function),
   set_opv(u_ob_c36_pairs, compile_as, kw_function),
   set_opv(u_ob_c36_pairs, function, f_u_ob_c36_pairs),
   _Ignored4=u_ob_c36_pairs.
/*
:- side_effect(assert_lsp(u_ob_c36_pairs,
			  wl:lambda_def(defun, u_ob_c36_pairs, f_u_ob_c36_pairs, [u_self], [[u_obr_slots, u_self]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_pairs,
			  wl:arglist_info(u_ob_c36_pairs, f_u_ob_c36_pairs, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_pairs,
			  wl:init_args(exact_only, f_u_ob_c36_pairs))).
*/
/*
(defun ob$slot-names (self)
  (yloop
   (initial (result nil))
   (yfor pair in (ob$pairs self))
   (ydo
    (if (not (memq? (car pair) result))
        (setq result (append result (list (car pair))))))
   (yresult result)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11728 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$slot-names',[self],[yloop,[initial,[result,[]]],[yfor,pair,in,['ob$pairs',self]],[ydo,[if,[not,['memq?',[car,pair],result]],[setq,result,[append,result,[list,[car,pair]]]]]],[yresult,result]]])
wl:lambda_def(defun, u_ob_c36_slot_names, f_u_ob_c36_slot_names, [u_self], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, u_pair, u_in, [u_ob_c36_pairs, u_self]], [u_ydo, [if, [not, [u_memq_c63, [car, u_pair], u_result]], [setq, u_result, [append, u_result, [list, [car, u_pair]]]]]], [u_yresult, u_result]]]).
wl:arglist_info(u_ob_c36_slot_names, f_u_ob_c36_slot_names, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_slot_names).

/*

### Compiled:  `U::OB$SLOT-NAMES` 
*/
f_u_ob_c36_slot_names(Self, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self)|Env],
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
   _Ignored4=u_ob_c36_slot_names.
/*
:- side_effect(assert_lsp(u_ob_c36_slot_names,
			  wl:lambda_def(defun, u_ob_c36_slot_names, f_u_ob_c36_slot_names, [u_self], [[u_yloop, [u_initial, [u_result, []]], [u_yfor, u_pair, u_in, [u_ob_c36_pairs, u_self]], [u_ydo, [if, [not, [u_memq_c63, [car, u_pair], u_result]], [setq, u_result, [append, u_result, [list, [car, u_pair]]]]]], [u_yresult, u_result]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_slot_names,
			  wl:arglist_info(u_ob_c36_slot_names, f_u_ob_c36_slot_names, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_slot_names,
			  wl:init_args(exact_only, f_u_ob_c36_slot_names))).
*/
/*
(defun make-into-obname (obj) (if (ob? obj) (ob$name obj) obj))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:11954 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'make-into-obname',[obj],[if,['ob?',obj],['ob$name',obj],obj]])
wl:lambda_def(defun, u_make_into_obname, f_u_make_into_obname, [u_obj], [[if, [u_ob_c63, u_obj], [u_ob_c36_name, u_obj], u_obj]]).
wl:arglist_info(u_make_into_obname, f_u_make_into_obname, [u_obj], arginfo{all:[u_obj], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obj], opt:0, req:[u_obj], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_make_into_obname).

/*

### Compiled:  `U::MAKE-INTO-OBNAME` 
*/
f_u_make_into_obname(Obj, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_obj, Obj)|Env],
	f_u_ob_c63(u_obj, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env15, u_obj, Obj_Get),
	    f_u_ob_c36_name(Obj_Get, TrueResult),
	    FnResult=TrueResult
	;   get_var(Env15, u_obj, Obj_Get10),
	    FnResult=Obj_Get10
	).
:- set_opv(f_u_make_into_obname, classof, claz_function),
   set_opv(u_make_into_obname, compile_as, kw_function),
   set_opv(u_make_into_obname, function, f_u_make_into_obname),
   _Ignored4=u_make_into_obname.
/*
:- side_effect(assert_lsp(u_make_into_obname,
			  wl:lambda_def(defun, u_make_into_obname, f_u_make_into_obname, [u_obj], [[if, [u_ob_c63, u_obj], [u_ob_c36_name, u_obj], u_obj]]))).
*/
/*
:- side_effect(assert_lsp(u_make_into_obname,
			  wl:arglist_info(u_make_into_obname, f_u_make_into_obname, [u_obj], arginfo{all:[u_obj], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_obj], opt:0, req:[u_obj], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_make_into_obname,
			  wl:init_args(exact_only, f_u_make_into_obname))).
*/
/*
(defun ob$basic-add (self slot-name slot-value)
  (if (eq? slot-name 'obname)
      (ob$add-name self slot-value)
      (set-obr-slots self (append! (obr-slots self)
                                   (list (list slot-name slot-value))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12019 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$basic-add',[self,'slot-name','slot-value'],[if,['eq?','slot-name',[quote,obname]],['ob$add-name',self,'slot-value'],['set-obr-slots',self,['append!',['obr-slots',self],[list,[list,'slot-name','slot-value']]]]]])
wl:lambda_def(defun, u_ob_c36_basic_add, f_u_ob_c36_basic_add, [u_self, u_slot_name, slot_value], [[if, [u_eq_c63, u_slot_name, [quote, u_obname]], [u_ob_c36_add_name, u_self, slot_value], [u_set_obr_slots, u_self, [u_append_c33, [u_obr_slots, u_self], [list, [list, u_slot_name, slot_value]]]]]]).
wl:arglist_info(u_ob_c36_basic_add, f_u_ob_c36_basic_add, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_basic_add).

/*

### Compiled:  `U::OB$BASIC-ADD` 
*/
f_u_ob_c36_basic_add(Self, Slot_name, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env15, slot_value, Slot_value_Get),
	    get_var(Env15, u_self, Self_Get),
	    f_u_ob_c36_add_name(Self_Get, Slot_value_Get, TrueResult),
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
   _Ignored4=u_ob_c36_basic_add.
/*
:- side_effect(assert_lsp(u_ob_c36_basic_add,
			  wl:lambda_def(defun, u_ob_c36_basic_add, f_u_ob_c36_basic_add, [u_self, u_slot_name, slot_value], [[if, [u_eq_c63, u_slot_name, [quote, u_obname]], [u_ob_c36_add_name, u_self, slot_value], [u_set_obr_slots, u_self, [u_append_c33, [u_obr_slots, u_self], [list, [list, u_slot_name, slot_value]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_basic_add,
			  wl:arglist_info(u_ob_c36_basic_add, f_u_ob_c36_basic_add, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_basic_add,
			  wl:init_args(exact_only, f_u_ob_c36_basic_add))).
*/
/*
(defun ob$literal? (self) (obr-literal self))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12260 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$literal?',[self],['obr-literal',self]])
wl:lambda_def(defun, u_ob_c36_literal_c63, f_u_ob_c36_literal_c63, [u_self], [[u_obr_literal, u_self]]).
wl:arglist_info(u_ob_c36_literal_c63, f_u_ob_c36_literal_c63, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_literal_c63).

/*

### Compiled:  `U::OB$LITERAL?` 
*/
f_u_ob_c36_literal_c63(Self, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self)|Env],
	get_var(Env10, u_self, Self_Get),
	f_u_obr_literal(Self_Get, Obr_literal_Ret),
	Obr_literal_Ret=FnResult.
:- set_opv(f_u_ob_c36_literal_c63, classof, claz_function),
   set_opv(u_ob_c36_literal_c63, compile_as, kw_function),
   set_opv(u_ob_c36_literal_c63, function, f_u_ob_c36_literal_c63),
   _Ignored4=u_ob_c36_literal_c63.
/*
:- side_effect(assert_lsp(u_ob_c36_literal_c63,
			  wl:lambda_def(defun, u_ob_c36_literal_c63, f_u_ob_c36_literal_c63, [u_self], [[u_obr_literal, u_self]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_literal_c63,
			  wl:arglist_info(u_ob_c36_literal_c63, f_u_ob_c36_literal_c63, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_literal_c63,
			  wl:init_args(exact_only, f_u_ob_c36_literal_c63))).
*/
/*
(defun ob$set-literal (self val)
  (set-obr-literal self val))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12307 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$set-literal',[self,val],['set-obr-literal',self,val]])
wl:lambda_def(defun, u_ob_c36_set_literal, f_u_ob_c36_set_literal, [u_self, u_val], [[u_set_obr_literal, u_self, u_val]]).
wl:arglist_info(u_ob_c36_set_literal, f_u_ob_c36_set_literal, [u_self, u_val], arginfo{all:[u_self, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_val], opt:0, req:[u_self, u_val], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_set_literal).

/*

### Compiled:  `U::OB$SET-LITERAL` 
*/
f_u_ob_c36_set_literal(Self, Val, FnResult) :-
	nop(global_env(Env)),
	Env9=[bv(u_self, Self), bv(u_val, Val)|Env],
	f_u_set_obr_literal(u_self, u_val, Val12),
	Val12=FnResult.
:- set_opv(f_u_ob_c36_set_literal, classof, claz_function),
   set_opv(u_ob_c36_set_literal, compile_as, kw_function),
   set_opv(u_ob_c36_set_literal, function, f_u_ob_c36_set_literal),
   _Ignored4=u_ob_c36_set_literal.
/*
:- side_effect(assert_lsp(u_ob_c36_set_literal,
			  wl:lambda_def(defun, u_ob_c36_set_literal, f_u_ob_c36_set_literal, [u_self, u_val], [[u_set_obr_literal, u_self, u_val]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_set_literal,
			  wl:arglist_info(u_ob_c36_set_literal, f_u_ob_c36_set_literal, [u_self, u_val], arginfo{all:[u_self, u_val], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_val], opt:0, req:[u_self, u_val], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_set_literal,
			  wl:init_args(exact_only, f_u_ob_c36_set_literal))).
*/
/*
(defun ob$add1 (self slot-name slot-value)
  (ob$basic-add self slot-name slot-value)
  (if *inverse-setting?*
   (let ((inv (inverse-slot slot-name)))
     (if (and inv (ob? slot-value))
         ; If slot-name has an inverse and slot-value is a ob,
         ; perform inverse setting
         (ob$basic-add slot-value inv self)))))

;
; (ob$get ob slot-name):
;
; Return a unique value of a slot. If there is more than one value for the
; slot, an arbitrary one is returned.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12371 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$add1',[self,'slot-name','slot-value'],['ob$basic-add',self,'slot-name','slot-value'],[if,'*inverse-setting?*',[let,[[inv,['inverse-slot','slot-name']]],[if,[and,inv,['ob?','slot-value']],['ob$basic-add','slot-value',inv,self]]]]])
wl:lambda_def(defun, u_ob_c36_add1, f_u_ob_c36_add1, [u_self, u_slot_name, slot_value], [[u_ob_c36_basic_add, u_self, u_slot_name, slot_value], [if, u_xx_inverse_setting_c63_xx, [let, [[u_inv, [u_inverse_slot, u_slot_name]]], [if, [and, u_inv, [u_ob_c63, slot_value]], [u_ob_c36_basic_add, slot_value, u_inv, u_self]]]]]).
wl:arglist_info(u_ob_c36_add1, f_u_ob_c36_add1, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_add1).

/*

### Compiled:  `U::OB$ADD1` 
*/
f_u_ob_c36_add1(Self, Slot_name, Slot_value, LetResult) :-
	nop(global_env(Env)),
	Env31=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
	get_var(Env31, slot_value, Slot_value_Get),
	get_var(Env31, u_self, Self_Get),
	get_var(Env31, u_slot_name, Slot_name_Get),
	f_u_ob_c36_basic_add(Self_Get,
			     Slot_name_Get,
			     Slot_value_Get,
			     Basic_add_Ret),
	get_var(Env31, u_xx_inverse_setting_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env31, u_slot_name, Slot_name_Get16),
	    f_u_inverse_slot(Slot_name_Get16, Inv_Init),
	    LEnv=[bv(u_inv, Inv_Init)|Env31],
	    get_var(LEnv, u_inv, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_ob_c63(slot_value, TrueResult),
		IFTEST18=TrueResult
	    ;   IFTEST18=[]
	    ),
	    (   IFTEST18\==[]
	    ->  get_var(LEnv, slot_value, Slot_value_Get24),
		get_var(LEnv, u_inv, Inv_Get25),
		get_var(LEnv, u_self, Self_Get26),
		f_u_ob_c36_basic_add(Slot_value_Get24,
				     Inv_Get25,
				     Self_Get26,
				     TrueResult27),
		LetResult=TrueResult27
	    ;   LetResult=[]
	    )
	;   LetResult=[]
	).
:- set_opv(f_u_ob_c36_add1, classof, claz_function),
   set_opv(u_ob_c36_add1, compile_as, kw_function),
   set_opv(u_ob_c36_add1, function, f_u_ob_c36_add1),
   _Ignored4=u_ob_c36_add1.
/*
:- side_effect(assert_lsp(u_ob_c36_add1,
			  wl:lambda_def(defun, u_ob_c36_add1, f_u_ob_c36_add1, [u_self, u_slot_name, slot_value], [[u_ob_c36_basic_add, u_self, u_slot_name, slot_value], [if, u_xx_inverse_setting_c63_xx, [let, [[u_inv, [u_inverse_slot, u_slot_name]]], [if, [and, u_inv, [u_ob_c63, slot_value]], [u_ob_c36_basic_add, slot_value, u_inv, u_self]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add1,
			  wl:arglist_info(u_ob_c36_add1, f_u_ob_c36_add1, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_add1,
			  wl:init_args(exact_only, f_u_ob_c36_add1))).
*/
/*
 If slot-name has an inverse and slot-value is a ob,
*/
/*
 perform inverse setting
*/
/*
*/
/*
 (ob$get ob slot-name):
*/
/*
*/
/*
 Return a unique value of a slot. If there is more than one value for the
*/
/*
 slot, an arbitrary one is returned.
*/
/*
*/
/*
(defun ob$get (self slot-name)
 (enforce-ob self "ob$get")
 (if (eq? slot-name 'obname)
  (car (obr-obnames self))
  (let ((found (assq slot-name (obr-slots self))))
    (if found (slots-value found) nil))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:12851 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$get',[self,'slot-name'],['enforce-ob',self,'$STRING'("ob$get")],[if,['eq?','slot-name',[quote,obname]],[car,['obr-obnames',self]],[let,[[found,[assq,'slot-name',['obr-slots',self]]]],[if,found,['slots-value',found],[]]]]])
wl:lambda_def(defun, u_ob_c36_get, f_u_ob_c36_get, [u_self, u_slot_name], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$get")], [if, [u_eq_c63, u_slot_name, [quote, u_obname]], [car, [u_obr_obnames, u_self]], [let, [[u_found, [ext_assq, u_slot_name, [u_obr_slots, u_self]]]], [if, u_found, [u_slots_value, u_found], []]]]]).
wl:arglist_info(u_ob_c36_get, f_u_ob_c36_get, [u_self, u_slot_name], arginfo{all:[u_self, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name], opt:0, req:[u_self, u_slot_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_get).

/*

### Compiled:  `U::OB$GET` 
*/
f_u_ob_c36_get(Self, Slot_name, LetResult) :-
	nop(global_env(Env)),
	Env22=[bv(u_self, Self), bv(u_slot_name, Slot_name)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$get"),
		       Enforce_ob_Ret),
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env22, u_self, Self_Get),
	    f_u_obr_obnames(Self_Get, Car_Param),
	    cl_car(Car_Param, TrueResult18),
	    LetResult=TrueResult18
	;   f_ext_assq(u_slot_name, [u_obr_slots, u_self], Found_Init),
	    LEnv=[bv(u_found, Found_Init)|Env22],
	    get_var(LEnv, u_found, IFTEST14),
	    (   IFTEST14\==[]
	    ->  f_u_slots_value(u_found, TrueResult),
		LetResult=TrueResult
	    ;   LetResult=[]
	    )
	).
:- set_opv(f_u_ob_c36_get, classof, claz_function),
   set_opv(u_ob_c36_get, compile_as, kw_function),
   set_opv(u_ob_c36_get, function, f_u_ob_c36_get),
   _Ignored4=u_ob_c36_get.
/*
:- side_effect(assert_lsp(u_ob_c36_get,
			  wl:lambda_def(defun, u_ob_c36_get, f_u_ob_c36_get, [u_self, u_slot_name], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$get")], [if, [u_eq_c63, u_slot_name, [quote, u_obname]], [car, [u_obr_obnames, u_self]], [let, [[u_found, [ext_assq, u_slot_name, [u_obr_slots, u_self]]]], [if, u_found, [u_slots_value, u_found], []]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_get,
			  wl:arglist_info(u_ob_c36_get, f_u_ob_c36_get, [u_self, u_slot_name], arginfo{all:[u_self, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name], opt:0, req:[u_self, u_slot_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_get,
			  wl:init_args(exact_only, f_u_ob_c36_get))).
*/
/*
(defun ob$pget (self slot-path)
  (enforce-ob self "ob$pget")
  (if (pair? slot-path)
      (ob$get (path->ob self slot-path)
            (path->slot-name slot-path))
      (if (eq? slot-path 'obname)
          (car (obr-obnames self))
          (let ((found (assq slot-path (obr-slots self))))
               (if found (slots-value found) nil)))))

;
; (ob$set ob slot-name slot-value):
;
; If it is desired to restrict slot values to a unique entry, this
; method removes all slot values from a slot, then sets the unique entry.
; Inverses are affected similarly.
;

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13060 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$pget',[self,'slot-path'],['enforce-ob',self,'$STRING'("ob$pget")],[if,['pair?','slot-path'],['ob$get',['path->ob',self,'slot-path'],['path->slot-name','slot-path']],[if,['eq?','slot-path',[quote,obname]],[car,['obr-obnames',self]],[let,[[found,[assq,'slot-path',['obr-slots',self]]]],[if,found,['slots-value',found],[]]]]]])
wl:lambda_def(defun, u_ob_c36_pget, f_u_ob_c36_pget, [u_self, u_slot_path], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$pget")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_get, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path]], [if, [u_eq_c63, u_slot_path, [quote, u_obname]], [car, [u_obr_obnames, u_self]], [let, [[u_found, [ext_assq, u_slot_path, [u_obr_slots, u_self]]]], [if, u_found, [u_slots_value, u_found], []]]]]]).
wl:arglist_info(u_ob_c36_pget, f_u_ob_c36_pget, [u_self, u_slot_path], arginfo{all:[u_self, u_slot_path], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path], opt:0, req:[u_self, u_slot_path], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_pget).

/*

### Compiled:  `U::OB$PGET` 
*/
f_u_ob_c36_pget(Self, Slot_path, LetResult) :-
	nop(global_env(Env)),
	Env28=[bv(u_self, Self), bv(u_slot_path, Slot_path)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$pget"),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env28, u_self, Self_Get),
	    get_var(Env28, u_slot_path, Slot_path_Get),
	    f_u_path_c62_ob(Self_Get, Slot_path_Get, C36_get_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    f_u_ob_c36_get(C36_get_Param, Slot_name_Ret, TrueResult24),
	    LetResult=TrueResult24
	;   f_u_eq_c63(u_slot_path, [quote, u_obname], IFTEST11),
	    (   IFTEST11\==[]
	    ->  get_var(Env28, u_self, Self_Get13),
		f_u_obr_obnames(Self_Get13, Car_Param),
		cl_car(Car_Param, TrueResult22),
		LetResult=TrueResult22
	    ;   f_ext_assq(u_slot_path, [u_obr_slots, u_self], Found_Init),
		LEnv=[bv(u_found, Found_Init)|Env28],
		get_var(LEnv, u_found, IFTEST18),
		(   IFTEST18\==[]
		->  f_u_slots_value(u_found, TrueResult),
		    LetResult=TrueResult
		;   LetResult=[]
		)
	    )
	).
:- set_opv(f_u_ob_c36_pget, classof, claz_function),
   set_opv(u_ob_c36_pget, compile_as, kw_function),
   set_opv(u_ob_c36_pget, function, f_u_ob_c36_pget),
   _Ignored4=u_ob_c36_pget.
/*
:- side_effect(assert_lsp(u_ob_c36_pget,
			  wl:lambda_def(defun, u_ob_c36_pget, f_u_ob_c36_pget, [u_self, u_slot_path], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$pget")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_get, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path]], [if, [u_eq_c63, u_slot_path, [quote, u_obname]], [car, [u_obr_obnames, u_self]], [let, [[u_found, [ext_assq, u_slot_path, [u_obr_slots, u_self]]]], [if, u_found, [u_slots_value, u_found], []]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_pget,
			  wl:arglist_info(u_ob_c36_pget, f_u_ob_c36_pget, [u_self, u_slot_path], arginfo{all:[u_self, u_slot_path], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path], opt:0, req:[u_self, u_slot_path], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_pget,
			  wl:init_args(exact_only, f_u_ob_c36_pget))).
*/
/*
*/
/*
 (ob$set ob slot-name slot-value):
*/
/*
*/
/*
 If it is desired to restrict slot values to a unique entry, this
*/
/*
 method removes all slot values from a slot, then sets the unique entry.
*/
/*
 Inverses are affected similarly.
*/
/*
*/
/*
(defun ob$set (self slot-name slot-value)
  (enforce-ob self "ob$set")
  (if (eq? slot-name 'obname)
      (progn 
       (ob$add-name self slot-value)
       slot-value)
      (yloop
       (initial (values (ob$gets self slot-name)))
       (ywhile values)
       (ydo
        (ob$remove1 self slot-name (car values))
        (setq values (cdr values)))
       (yresult
        (ob$add1 self slot-name slot-value)
        slot-value))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:13629 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$set',[self,'slot-name','slot-value'],['enforce-ob',self,'$STRING'("ob$set")],[if,['eq?','slot-name',[quote,obname]],[progn,['ob$add-name',self,'slot-value'],'slot-value'],[yloop,[initial,[values,['ob$gets',self,'slot-name']]],[ywhile,values],[ydo,['ob$remove1',self,'slot-name',[car,values]],[setq,values,[cdr,values]]],[yresult,['ob$add1',self,'slot-name','slot-value'],'slot-value']]]])
wl:lambda_def(defun, u_ob_c36_set, f_u_ob_c36_set, [u_self, u_slot_name, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$set")], [if, [u_eq_c63, u_slot_name, [quote, u_obname]], [progn, [u_ob_c36_add_name, u_self, slot_value], slot_value], [u_yloop, [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_name]]], [u_ywhile, values], [u_ydo, [u_ob_c36_remove1, u_self, u_slot_name, [car, values]], [setq, values, [cdr, values]]], [u_yresult, [u_ob_c36_add1, u_self, u_slot_name, slot_value], slot_value]]]]).
wl:arglist_info(u_ob_c36_set, f_u_ob_c36_set, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_set).

/*

### Compiled:  `U::OB$SET` 
*/
f_u_ob_c36_set(Self, Slot_name, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env16=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$set"),
		       Enforce_ob_Ret),
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env16, slot_value, Slot_value_Get),
	    get_var(Env16, u_self, Self_Get),
	    f_u_ob_c36_add_name(Self_Get, Slot_value_Get, Add_name_Ret),
	    get_var(Env16, slot_value, Slot_value_Get11),
	    FnResult=Slot_value_Get11
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
   _Ignored4=u_ob_c36_set.
/*
:- side_effect(assert_lsp(u_ob_c36_set,
			  wl:lambda_def(defun, u_ob_c36_set, f_u_ob_c36_set, [u_self, u_slot_name, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$set")], [if, [u_eq_c63, u_slot_name, [quote, u_obname]], [progn, [u_ob_c36_add_name, u_self, slot_value], slot_value], [u_yloop, [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_name]]], [u_ywhile, values], [u_ydo, [u_ob_c36_remove1, u_self, u_slot_name, [car, values]], [setq, values, [cdr, values]]], [u_yresult, [u_ob_c36_add1, u_self, u_slot_name, slot_value], slot_value]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_set,
			  wl:arglist_info(u_ob_c36_set, f_u_ob_c36_set, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_set,
			  wl:init_args(exact_only, f_u_ob_c36_set))).
*/
/*
(defun ob$pset (self slot-path slot-value)
  (enforce-ob self "ob$pset")
  (if (pair? slot-path)
      (ob$set (path->ob self slot-path)
            (path->slot-name slot-path)
            slot-value)
      (yloop (initial (values (ob$gets self slot-path)))
            (ywhile values)
            (ydo (ob$remove1 self slot-path (car values))
                 (setq values (cdr values)))
            (yresult
             (ob$add1 self slot-path slot-value))))
  slot-value)

; This function is never used?
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14068 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$pset',[self,'slot-path','slot-value'],['enforce-ob',self,'$STRING'("ob$pset")],[if,['pair?','slot-path'],['ob$set',['path->ob',self,'slot-path'],['path->slot-name','slot-path'],'slot-value'],[yloop,[initial,[values,['ob$gets',self,'slot-path']]],[ywhile,values],[ydo,['ob$remove1',self,'slot-path',[car,values]],[setq,values,[cdr,values]]],[yresult,['ob$add1',self,'slot-path','slot-value']]]],'slot-value'])
wl:lambda_def(defun, u_ob_c36_pset, f_u_ob_c36_pset, [u_self, u_slot_path, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$pset")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_set, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path], slot_value], [u_yloop, [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_path]]], [u_ywhile, values], [u_ydo, [u_ob_c36_remove1, u_self, u_slot_path, [car, values]], [setq, values, [cdr, values]]], [u_yresult, [u_ob_c36_add1, u_self, u_slot_path, slot_value]]]], slot_value]).
wl:arglist_info(u_ob_c36_pset, f_u_ob_c36_pset, [u_self, u_slot_path, slot_value], arginfo{all:[u_self, u_slot_path, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path, slot_value], opt:0, req:[u_self, u_slot_path, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_pset).

/*

### Compiled:  `U::OB$PSET` 
*/
f_u_ob_c36_pset(Self, Slot_path, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env17=[bv(u_self, Self), bv(u_slot_path, Slot_path), bv(slot_value, Slot_value)|Env],
	f_u_enforce_ob(u_self,
		       '$ARRAY'([*], claz_base_character, "ob$pset"),
		       Enforce_ob_Ret),
	f_u_pair_c63(u_slot_path, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env17, u_self, Self_Get),
	    get_var(Env17, u_slot_path, Slot_path_Get),
	    f_u_path_c62_ob(Self_Get, Slot_path_Get, C36_set_Param),
	    f_u_path_c62_slot_name(u_slot_path, Slot_name_Ret),
	    get_var(Env17, slot_value, Slot_value_Get),
	    f_u_ob_c36_set(C36_set_Param,
			   Slot_name_Ret,
			   Slot_value_Get,
			   TrueResult),
	    _189707612=TrueResult
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
	    _189707612=ElseResult
	),
	get_var(Env17, slot_value, Slot_value_Get14),
	Slot_value_Get14=FnResult.
:- set_opv(f_u_ob_c36_pset, classof, claz_function),
   set_opv(u_ob_c36_pset, compile_as, kw_function),
   set_opv(u_ob_c36_pset, function, f_u_ob_c36_pset),
   _Ignored4=u_ob_c36_pset.
/*
:- side_effect(assert_lsp(u_ob_c36_pset,
			  wl:lambda_def(defun, u_ob_c36_pset, f_u_ob_c36_pset, [u_self, u_slot_path, slot_value], [[u_enforce_ob, u_self, '$ARRAY'([*], claz_base_character, "ob$pset")], [if, [u_pair_c63, u_slot_path], [u_ob_c36_set, [u_path_c62_ob, u_self, u_slot_path], [u_path_c62_slot_name, u_slot_path], slot_value], [u_yloop, [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_path]]], [u_ywhile, values], [u_ydo, [u_ob_c36_remove1, u_self, u_slot_path, [car, values]], [setq, values, [cdr, values]]], [u_yresult, [u_ob_c36_add1, u_self, u_slot_path, slot_value]]]], slot_value]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_pset,
			  wl:arglist_info(u_ob_c36_pset, f_u_ob_c36_pset, [u_self, u_slot_path, slot_value], arginfo{all:[u_self, u_slot_path, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_path, slot_value], opt:0, req:[u_self, u_slot_path, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_pset,
			  wl:init_args(exact_only, f_u_ob_c36_pset))).
*/
/*
 This function is never used?
*/
/*
(defun ob$set1 (self slot-name slot-value)
  (yloop (initial (values (ob$gets self slot-name)))
        (ywhile values)
        (ydo (ob$remove1 self slot-name (car values))
            (setq values (cdr values)))
        (yresult (ob$add1 self slot-name slot-value)))
  slot-value)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14576 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$set1',[self,'slot-name','slot-value'],[yloop,[initial,[values,['ob$gets',self,'slot-name']]],[ywhile,values],[ydo,['ob$remove1',self,'slot-name',[car,values]],[setq,values,[cdr,values]]],[yresult,['ob$add1',self,'slot-name','slot-value']]],'slot-value'])
wl:lambda_def(defun, u_ob_c36_set1, f_u_ob_c36_set1, [u_self, u_slot_name, slot_value], [[u_yloop, [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_name]]], [u_ywhile, values], [u_ydo, [u_ob_c36_remove1, u_self, u_slot_name, [car, values]], [setq, values, [cdr, values]]], [u_yresult, [u_ob_c36_add1, u_self, u_slot_name, slot_value]]], slot_value]).
wl:arglist_info(u_ob_c36_set1, f_u_ob_c36_set1, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_set1).

/*

### Compiled:  `U::OB$SET1` 
*/
f_u_ob_c36_set1(Self, Slot_name, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env10=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
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
	get_var(Env10, slot_value, Slot_value_Get),
	Slot_value_Get=FnResult.
:- set_opv(f_u_ob_c36_set1, classof, claz_function),
   set_opv(u_ob_c36_set1, compile_as, kw_function),
   set_opv(u_ob_c36_set1, function, f_u_ob_c36_set1),
   _Ignored4=u_ob_c36_set1.
/*
:- side_effect(assert_lsp(u_ob_c36_set1,
			  wl:lambda_def(defun, u_ob_c36_set1, f_u_ob_c36_set1, [u_self, u_slot_name, slot_value], [[u_yloop, [u_initial, [values, [u_ob_c36_gets, u_self, u_slot_name]]], [u_ywhile, values], [u_ydo, [u_ob_c36_remove1, u_self, u_slot_name, [car, values]], [setq, values, [cdr, values]]], [u_yresult, [u_ob_c36_add1, u_self, u_slot_name, slot_value]]], slot_value]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_set1,
			  wl:arglist_info(u_ob_c36_set1, f_u_ob_c36_set1, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_set1,
			  wl:init_args(exact_only, f_u_ob_c36_set1))).
*/
/*
(defun ob$removes (self slot-name) (ob$set self slot-name nil) self)
#|
(defun ob$removes (self slot-name)
  (map 'list
   (lambda (slot)
    (if (eq? (slots-name slot) slot-name)
        (ob$remove1 self (slots-name slot)
                   (slots-value slot))))
   (obr-slots self))
  self)
|#

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:14860 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$removes',[self,'slot-name'],['ob$set',self,'slot-name',[]],self])
wl:lambda_def(defun, u_ob_c36_removes, f_u_ob_c36_removes, [u_self, u_slot_name], [[u_ob_c36_set, u_self, u_slot_name, []], u_self]).
wl:arglist_info(u_ob_c36_removes, f_u_ob_c36_removes, [u_self, u_slot_name], arginfo{all:[u_self, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name], opt:0, req:[u_self, u_slot_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_removes).

/*

### Compiled:  `U::OB$REMOVES` 
*/
f_u_ob_c36_removes(Self, Slot_name, FnResult) :-
	nop(global_env(Env)),
	Env12=[bv(u_self, Self), bv(u_slot_name, Slot_name)|Env],
	get_var(Env12, u_self, Self_Get),
	get_var(Env12, u_slot_name, Slot_name_Get),
	f_u_ob_c36_set(Self_Get, Slot_name_Get, [], C36_set_Ret),
	get_var(Env12, u_self, Self_Get9),
	Self_Get9=FnResult.
:- set_opv(f_u_ob_c36_removes, classof, claz_function),
   set_opv(u_ob_c36_removes, compile_as, kw_function),
   set_opv(u_ob_c36_removes, function, f_u_ob_c36_removes),
   _Ignored4=u_ob_c36_removes.
/*
:- side_effect(assert_lsp(u_ob_c36_removes,
			  wl:lambda_def(defun, u_ob_c36_removes, f_u_ob_c36_removes, [u_self, u_slot_name], [[u_ob_c36_set, u_self, u_slot_name, []], u_self]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_removes,
			  wl:arglist_info(u_ob_c36_removes, f_u_ob_c36_removes, [u_self, u_slot_name], arginfo{all:[u_self, u_slot_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name], opt:0, req:[u_self, u_slot_name], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_removes,
			  wl:init_args(exact_only, f_u_ob_c36_removes))).
*/
/*

(defun ob$removes (self slot-name)
  (map 'list
   (lambda (slot)
    (if (eq? (slots-name slot) slot-name)
        (ob$remove1 self (slots-name slot)
                   (slots-value slot))))
   (obr-slots self))
  self)
*/
/*
(defun ob$remove-all (self)
  (map 'list
   (lambda (slot)
    (ob$remove1 self (slots-name slot)
               (slots-value slot)))
   (obr-slots self))
  self)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15157 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$remove-all',[self],[map,[quote,list],[lambda,[slot],['ob$remove1',self,['slots-name',slot],['slots-value',slot]]],['obr-slots',self]],self])
wl:lambda_def(defun, u_ob_c36_remove_all, f_u_ob_c36_remove_all, [u_self], [[map, [quote, list], [lambda, [u_slot], [u_ob_c36_remove1, u_self, [u_slots_name, u_slot], [u_slots_value, u_slot]]], [u_obr_slots, u_self]], u_self]).
wl:arglist_info(u_ob_c36_remove_all, f_u_ob_c36_remove_all, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_remove_all).

/*

### Compiled:  `U::OB$REMOVE-ALL` 
*/
f_u_ob_c36_remove_all(Self, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_self, Self)|Env],
	Lambda=closure([ClosureEnvironment|Env15], LResult, [u_slot],  (get_var(ClosureEnvironment, u_self, Self_Get), f_u_slots_name(u_slot, Slots_name_Ret), f_u_slots_value(u_slot, Slots_value_Ret), f_u_ob_c36_remove1(Self_Get, Slots_name_Ret, Slots_value_Ret, LResult))),
	get_var(Env15, u_self, Self_Get11),
	f_u_obr_slots(Self_Get11, Obr_slots_Ret),
	cl_map(list, Lambda, Obr_slots_Ret, Map_Ret),
	get_var(Env15, u_self, Self_Get12),
	Self_Get12=FnResult.
:- set_opv(f_u_ob_c36_remove_all, classof, claz_function),
   set_opv(u_ob_c36_remove_all, compile_as, kw_function),
   set_opv(u_ob_c36_remove_all, function, f_u_ob_c36_remove_all),
   _Ignored4=u_ob_c36_remove_all.
/*
:- side_effect(assert_lsp(u_ob_c36_remove_all,
			  wl:lambda_def(defun, u_ob_c36_remove_all, f_u_ob_c36_remove_all, [u_self], [[map, [quote, list], [lambda, [u_slot], [u_ob_c36_remove1, u_self, [u_slots_name, u_slot], [u_slots_value, u_slot]]], [u_obr_slots, u_self]], u_self]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove_all,
			  wl:arglist_info(u_ob_c36_remove_all, f_u_ob_c36_remove_all, [u_self], arginfo{all:[u_self], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self], opt:0, req:[u_self], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove_all,
			  wl:init_args(exact_only, f_u_ob_c36_remove_all))).
*/
/*
(defun ob$remove1 (self slot-name slot-value)
  (ob$basic-remove self slot-name slot-value)
  (if *inverse-setting?*
   (let ((inv (inverse-slot slot-name)))
    (if (and inv (ob? slot-value))
        ; If slot-name has an inverse and slot-value is an ob,
        ; perform inverse removal.
        (ob$basic-remove slot-value inv self)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15321 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$remove1',[self,'slot-name','slot-value'],['ob$basic-remove',self,'slot-name','slot-value'],[if,'*inverse-setting?*',[let,[[inv,['inverse-slot','slot-name']]],[if,[and,inv,['ob?','slot-value']],['ob$basic-remove','slot-value',inv,self]]]]])
wl:lambda_def(defun, u_ob_c36_remove1, f_u_ob_c36_remove1, [u_self, u_slot_name, slot_value], [[u_ob_c36_basic_remove, u_self, u_slot_name, slot_value], [if, u_xx_inverse_setting_c63_xx, [let, [[u_inv, [u_inverse_slot, u_slot_name]]], [if, [and, u_inv, [u_ob_c63, slot_value]], [u_ob_c36_basic_remove, slot_value, u_inv, u_self]]]]]).
wl:arglist_info(u_ob_c36_remove1, f_u_ob_c36_remove1, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_remove1).

/*

### Compiled:  `U::OB$REMOVE1` 
*/
f_u_ob_c36_remove1(Self, Slot_name, Slot_value, LetResult) :-
	nop(global_env(Env)),
	Env31=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
	get_var(Env31, slot_value, Slot_value_Get),
	get_var(Env31, u_self, Self_Get),
	get_var(Env31, u_slot_name, Slot_name_Get),
	f_u_ob_c36_basic_remove(Self_Get,
				Slot_name_Get,
				Slot_value_Get,
				Basic_remove_Ret),
	get_var(Env31, u_xx_inverse_setting_c63_xx, IFTEST),
	(   IFTEST\==[]
	->  get_var(Env31, u_slot_name, Slot_name_Get16),
	    f_u_inverse_slot(Slot_name_Get16, Inv_Init),
	    LEnv=[bv(u_inv, Inv_Init)|Env31],
	    get_var(LEnv, u_inv, IFTEST20),
	    (   IFTEST20\==[]
	    ->  f_u_ob_c63(slot_value, TrueResult),
		IFTEST18=TrueResult
	    ;   IFTEST18=[]
	    ),
	    (   IFTEST18\==[]
	    ->  get_var(LEnv, slot_value, Slot_value_Get24),
		get_var(LEnv, u_inv, Inv_Get25),
		get_var(LEnv, u_self, Self_Get26),
		f_u_ob_c36_basic_remove(Slot_value_Get24,
					Inv_Get25,
					Self_Get26,
					TrueResult27),
		LetResult=TrueResult27
	    ;   LetResult=[]
	    )
	;   LetResult=[]
	).
:- set_opv(f_u_ob_c36_remove1, classof, claz_function),
   set_opv(u_ob_c36_remove1, compile_as, kw_function),
   set_opv(u_ob_c36_remove1, function, f_u_ob_c36_remove1),
   _Ignored4=u_ob_c36_remove1.
/*
:- side_effect(assert_lsp(u_ob_c36_remove1,
			  wl:lambda_def(defun, u_ob_c36_remove1, f_u_ob_c36_remove1, [u_self, u_slot_name, slot_value], [[u_ob_c36_basic_remove, u_self, u_slot_name, slot_value], [if, u_xx_inverse_setting_c63_xx, [let, [[u_inv, [u_inverse_slot, u_slot_name]]], [if, [and, u_inv, [u_ob_c63, slot_value]], [u_ob_c36_basic_remove, slot_value, u_inv, u_self]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove1,
			  wl:arglist_info(u_ob_c36_remove1, f_u_ob_c36_remove1, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_remove1,
			  wl:init_args(exact_only, f_u_ob_c36_remove1))).
*/
/*
 If slot-name has an inverse and slot-value is an ob,
*/
/*
 perform inverse removal.
*/
/*
(defun ob$basic-remove (self slot-name slot-value)
 (if (eq? slot-name 'obname)
  (ob$remove-name self slot-value)
  (yloop (initial (rest (obr-slots self)) (found nil))
        (ywhile rest)
        (yuntil found)
        (ydo (if (and (eq? slot-name (slots-name (car rest)))
                      (eq? slot-value (slots-value (car rest))))
                (progn
                 (setq found t)
                 (set-obr-slots self (delq! (car rest) (obr-slots self)))))
            (setq rest (cdr rest)))
        (yresult (if (null? found)
                    (progn
                     (error ""(defun ob$basic-remove (self slot-name slot-value)\n (if (eq? slot-name 'obname)\n  (ob$remove-name self slot-value)\n  (yloop (initial (rest (obr-slots self)) (found nil))\n        (ywhile rest)\n        (yuntil found)\n        (ydo (if (and (eq? slot-name (slots-name (car rest)))\n                      (eq? slot-value (slots-value (car rest))))\n                (progn\n                 (setq found t)\n                 (set-obr-slots self (delq! (car rest) (obr-slots self)))))\n            (setq rest (cdr rest)))\n        (yresult (if (null? found)\n                    (progn\n                     (error \"~A slot of ~A has no ~A value.\"\n                            slot-name self slot-value)\n                     nil))))))\n\n; ".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/gate_main.cl:15663 **********************/
:-lisp_compile_to_prolog(pkg_user,[defun,'ob$basic-remove',[self,'slot-name','slot-value'],[if,['eq?','slot-name',[quote,obname]],['ob$remove-name',self,'slot-value'],[yloop,[initial,[rest,['obr-slots',self]],[found,[]]],[ywhile,rest],[yuntil,found],[ydo,[if,[and,['eq?','slot-name',['slots-name',[car,rest]]],['eq?','slot-value',['slots-value',[car,rest]]]],[progn,[setq,found,t],['set-obr-slots',self,['delq!',[car,rest],['obr-slots',self]]]]],[setq,rest,[cdr,rest]]],[yresult,[if,['null?',found],[progn,[error,'$STRING'("~A slot of ~A has no ~A value."),'slot-name',self,'slot-value'],[]]]]]]])
wl:lambda_def(defun, u_ob_c36_basic_remove, f_u_ob_c36_basic_remove, [u_self, u_slot_name, slot_value], [[if, [u_eq_c63, u_slot_name, [quote, u_obname]], [u_ob_c36_remove_name, u_self, slot_value], [u_yloop, [u_initial, [rest, [u_obr_slots, u_self]], [u_found, []]], [u_ywhile, rest], [u_yuntil, u_found], [u_ydo, [if, [and, [u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]], [u_eq_c63, slot_value, [u_slots_value, [car, rest]]]], [progn, [setq, u_found, t], [u_set_obr_slots, u_self, [u_delq_c33, [car, rest], [u_obr_slots, u_self]]]]], [setq, rest, [cdr, rest]]], [u_yresult, [if, [u_null_c63, u_found], [progn, [error, '$ARRAY'([*], claz_base_character, "~A slot of ~A has no ~A value."), u_slot_name, u_self, slot_value], []]]]]]]).
wl:arglist_info(u_ob_c36_basic_remove, f_u_ob_c36_basic_remove, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, f_u_ob_c36_basic_remove).

/*

### Compiled:  `U::OB$BASIC-REMOVE` 
*/
f_u_ob_c36_basic_remove(Self, Slot_name, Slot_value, FnResult) :-
	nop(global_env(Env)),
	Env15=[bv(u_self, Self), bv(u_slot_name, Slot_name), bv(slot_value, Slot_value)|Env],
	f_u_eq_c63(u_slot_name, [quote, u_obname], IFTEST),
	(   IFTEST\==[]
	->  get_var(Env15, slot_value, Slot_value_Get),
	    get_var(Env15, u_self, Self_Get),
	    f_u_ob_c36_remove_name(Self_Get, Slot_value_Get, TrueResult),
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
					 "~A slot of ~A has no ~A value."),
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
   _Ignored4=u_ob_c36_basic_remove.
/*
:- side_effect(assert_lsp(u_ob_c36_basic_remove,
			  wl:lambda_def(defun, u_ob_c36_basic_remove, f_u_ob_c36_basic_remove, [u_self, u_slot_name, slot_value], [[if, [u_eq_c63, u_slot_name, [quote, u_obname]], [u_ob_c36_remove_name, u_self, slot_value], [u_yloop, [u_initial, [rest, [u_obr_slots, u_self]], [u_found, []]], [u_ywhile, rest], [u_yuntil, u_found], [u_ydo, [if, [and, [u_eq_c63, u_slot_name, [u_slots_name, [car, rest]]], [u_eq_c63, slot_value, [u_slots_value, [car, rest]]]], [progn, [setq, u_found, t], [u_set_obr_slots, u_self, [u_delq_c33, [car, rest], [u_obr_slots, u_self]]]]], [setq, rest, [cdr, rest]]], [u_yresult, [if, [u_null_c63, u_found], [progn, [error, '$ARRAY'([*], claz_base_character, "~A slot of ~A has no ~A value."), u_slot_name, u_self, slot_value], []]]]]]]))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_basic_remove,
			  wl:arglist_info(u_ob_c36_basic_remove, f_u_ob_c36_basic_remove, [u_self, u_slot_name, slot_value], arginfo{all:[u_self, u_slot_name, slot_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_self, u_slot_name, slot_value], opt:0, req:[u_self, u_slot_name, slot_value], rest:0, sublists:0, whole:0}))).
*/
/*
:- side_effect(assert_lsp(u_ob_c36_basic_remove,
			  wl:init_args(exact_only, f_u_ob_c36_basic_remove))).
*/
/*
 End of file.
*/


%; Total compilation time: 10.227 seconds

